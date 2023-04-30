// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "ssabuilder.h"

enum ApKind : uint8_t
{
    OAK_INVALID,
    OAK_EQUAL,
    OAK_NOT_EQUAL,
    OAK_RANGE,
    OAK_BOUNDS_CHK
};

enum ApOp1Kind : uint8_t
{
    O1K_INVALID,
    O1K_LCLVAR,
    O1K_BOUND_OPER_BND,
    O1K_BOUND_LOOP_BND,
    O1K_INSTANCE_OF,
    O1K_VALUE_NUMBER
};

enum ApOp2Kind : uint8_t
{
    O2K_INVALID,
    O2K_CONST_INT,
#ifndef TARGET_64BIT
    O2K_CONST_LONG,
#endif
    O2K_CONST_DOUBLE,
    O2K_RANGE,
    O2K_VALUE_NUMBER
};

struct AssertionDsc
{
    struct IntCon
    {
        ssize_t      value;
        GenTreeFlags flags;

        bool operator==(const IntCon& other) const
        {
            return (value == other.value) && (flags == other.flags);
        }

        bool operator!=(const IntCon& other) const
        {
            return (value != other.value) || (flags != other.flags);
        }
    };

#ifndef TARGET_64BIT
    struct LngCon
    {
        int64_t value;

        bool operator==(const LngCon& other) const
        {
            return value == other.value;
        }

        bool operator!=(const LngCon& other) const
        {
            return value != other.value;
        }
    };
#endif

    struct DblCon
    {
        double value;

        bool operator==(const DblCon& other) const
        {
            return jitstd::bit_cast<uint64_t>(value) == jitstd::bit_cast<uint64_t>(other.value);
        }

        bool operator!=(const DblCon& other) const
        {
            return jitstd::bit_cast<uint64_t>(value) != jitstd::bit_cast<uint64_t>(other.value);
        }
    };

    struct Range
    {
        ssize_t min;
        ssize_t max;

        bool operator==(const Range& other) const
        {
            return (min == other.min) && (max == other.max);
        }

        bool operator!=(const Range& other) const
        {
            return (min != other.min) || (max != other.max);
        }
    };

    struct Op1
    {
        ApOp1Kind kind;
        ValueNum  vn;
        unsigned  lclNum;
    };

    struct Op2
    {
        ApOp2Kind kind;
        ValueNum  vn;
        union {
            IntCon intCon;
#ifndef TARGET_64BIT
            LngCon lngCon;
#endif
            DblCon dblCon;
            Range  range;
        };
    };

    ApKind kind;
    Op1    op1;
    Op2    op2;

    bool HasSameOp1(const AssertionDsc& that) const
    {
        return (op1.kind == that.op1.kind) && (op1.vn == that.op1.vn);
    }

    bool HasSameOp2(const AssertionDsc& that) const
    {
        if (op2.kind != that.op2.kind)
        {
            return false;
        }

        switch (op2.kind)
        {
            case O2K_CONST_INT:
                return op2.intCon == that.op2.intCon;
#ifndef TARGET_64BIT
            case O2K_CONST_LONG:
                return op2.lngCon == that.op2.lngCon;
#endif
            case O2K_CONST_DOUBLE:
                return op2.dblCon == that.op2.dblCon;
            case O2K_VALUE_NUMBER:
                return op2.vn == that.op2.vn;
            case O2K_RANGE:
                // Ranges are handled separately.
                return false;
            default:
                assert(op2.kind == O2K_INVALID);
                return false;
        }
    }

    bool operator==(const AssertionDsc& that) const
    {
        return (kind == that.kind) && HasSameOp1(that) && HasSameOp2(that);
    }
};

static const AssertionDsc::Range& GetSmallTypeRange(var_types type)
{
    static const AssertionDsc::Range ranges[]{
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

AssertionIndex GetAssertionIndex(unsigned index)
{
    return static_cast<AssertionIndex>(index + 1);
}

class AssertionProp
{
    typedef JitHashTable<ValueNum, JitSmallPrimitiveKeyFuncs<ValueNum>, ASSERT_TP> ValueNumToAssertsMap;

    SsaOptimizer&  ssa;
    Compiler*      compiler;
    ValueNumStore* vnStore;

    AssertionDsc*         assertionTable;
    AssertionIndex        assertionTableSize;
    AssertionIndex        assertionCount;
    BitVecTraits          sizeTraits;
    BitVecTraits          countTraits;
    uint16_t*             invertedAssertions;
    ValueNumToAssertsMap* vnAssertionMap;
    bool                  stmtMorphPending;
#ifdef DEBUG
    bool        verbose;
    GenTree*    currentNode  = nullptr;
    BasicBlock* currentBlock = nullptr;
#endif

public:
    AssertionProp(SsaOptimizer& ssa)
        : ssa(ssa)
        , compiler(ssa.GetCompiler())
        , vnStore(ssa.GetVNStore())
        , sizeTraits(0, compiler)
        , countTraits(0, compiler)
#ifdef DEBUG
        , verbose(compiler->verbose)
#endif
    {
    }

    void Run()
    {
#ifdef DEBUG
        DBEXEC(verbose, compiler->fgDispBasicBlocks(true);)
        compiler->fgDebugCheckLinks();
#endif

        Init();
        GenerateAssertions();

        if (assertionCount != 0)
        {
            ComputeAvailability();
            PropagateAssertions();
        }
        else
        {
            for (BasicBlock* block : compiler->Blocks())
            {
                block->bbAssertionIn          = nullptr;
                block->bbAssertionOut         = nullptr;
                block->bbAssertionOutJumpDest = nullptr;
            }
        }

        ssa.SetAssertionTable(assertionTable, assertionCount);

#ifdef DEBUG
        compiler->fgDebugCheckBBlist();
        compiler->fgDebugCheckLinks();
#endif
    }

private:
    void Init()
    {
        // Use a function countFunc to determine a proper maximum assertion count for the
        // method being compiled. The function is linear to the IL size for small and
        // moderate methods. For large methods, considering throughput impact, we track no
        // more than 64 assertions.
        // Note this tracks at most only 256 assertions.

        static const AssertionIndex countFunc[]{64, 128, 256, 64};

        const unsigned upperBound = _countof(countFunc) - 1;
        const unsigned codeSize   = compiler->info.compILCodeSize / 512;

        CompAllocator allocator = compiler->getAllocator(CMK_AssertionProp);

        assertionTableSize = countFunc[min(upperBound, codeSize)];
        assertionTable     = new (allocator) AssertionDsc[assertionTableSize];
        invertedAssertions = new (allocator) uint16_t[assertionTableSize]();
        vnAssertionMap     = new (allocator) ValueNumToAssertsMap(allocator);
        sizeTraits         = BitVecTraits(assertionTableSize, compiler);
        assertionCount     = 0;
    }

    const AssertionDsc& GetAssertion(AssertionIndex index) const
    {
        assert((1 <= index) && (index <= assertionCount));

        const AssertionDsc& assertion = assertionTable[index - 1];
        INDEBUG(DebugCheckAssertion(assertion));
        return assertion;
    }

    AssertionIndex GenerateBoundsChkAssertion(GenTreeBoundsChk* boundsChk)
    {
        ValueNum indexVN  = vnStore->VNNormalValue(boundsChk->GetIndex()->GetConservativeVN());
        ValueNum lengthVN = vnStore->VNNormalValue(boundsChk->GetLength()->GetConservativeVN());

        if ((indexVN == NoVN) || (lengthVN == NoVN))
        {
            return NO_ASSERTION_INDEX;
        }

        if (vnStore->IsVNInt32Constant(indexVN))
        {
            ssize_t indexVal = vnStore->ConstantValue<int32_t>(indexVN);

            return indexVal == INT32_MAX ? NO_ASSERTION_INDEX : AddRangeAssertion(lengthVN, indexVal + 1, INT32_MAX);
        }

        if (vnStore->IsVNInt32Constant(lengthVN))
        {
            ssize_t lengthVal = vnStore->ConstantValue<int32_t>(lengthVN);

            return lengthVal == 0 ? NO_ASSERTION_INDEX : AddRangeAssertion(indexVN, 0, lengthVal - 1);
        }

        return AddBoundsChkAssertion(indexVN, lengthVN);
    }

    AssertionIndex CreateNotNullAssertion(GenTree* addr)
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

        if (compiler->fgIsBigOffset(offset))
        {
            return NO_ASSERTION_INDEX;
        }

        if (!addr->OperIs(GT_LCL_VAR, GT_LCL_USE))
        {
            return NO_ASSERTION_INDEX;
        }

        unsigned   lclNum = addr->IsLclUse() ? addr->AsLclUse()->GetDef()->GetLclNum() : addr->AsLclVar()->GetLclNum();
        LclVarDsc* lcl    = compiler->lvaGetDesc(lclNum);

        AssertionDsc assertion;

        if (lcl->TypeIs(TYP_REF))
        {
            // TODO: only copy assertions rely on valid SSA number so we could generate more assertions here
            if (!addr->IsLclUse())
            {
                return NO_ASSERTION_INDEX;
            }

            assert(!lcl->IsAddressExposed());

            assertion.op1.kind   = O1K_LCLVAR;
            assertion.op1.vn     = addr->GetConservativeVN();
            assertion.op1.lclNum = lclNum;
        }
        else if (lcl->TypeIs(TYP_BYREF))
        {
            ValueNum  vn = vnStore->VNNormalValue(addr->GetConservativeVN());
            VNFuncApp funcApp;

            while (vnStore->GetVNFunc(vn, &funcApp) && funcApp.Is(GT_ADD) && (vnStore->TypeOfVN(vn) == TYP_BYREF))
            {
                if (vnStore->IsVNConstant(funcApp[1]) && varTypeIsIntegral(vnStore->TypeOfVN(funcApp[1])))
                {
                    offset += vnStore->CoercedConstantValue<ssize_t>(funcApp[1]);
                    vn = funcApp[0];
                }
                else if (vnStore->IsVNConstant(funcApp[0]) && varTypeIsIntegral(vnStore->TypeOfVN(funcApp[0])))
                {
                    offset += vnStore->CoercedConstantValue<ssize_t>(funcApp[0]);
                    vn = funcApp[1];
                }
                else
                {
                    break;
                }
            }

            if (compiler->fgIsBigOffset(offset))
            {
                return NO_ASSERTION_INDEX;
            }

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

        return AddAssertion(assertion);
    }

    AssertionIndex CreateRangeAssertion(GenTreeCast* cast)
    {
        GenTree* value = cast->GetOp(0);

        if (value->GetConservativeVN() == NoVN)
        {
            return NO_ASSERTION_INDEX;
        }

        if (!varTypeIsIntegral(value->GetType()))
        {
            return NO_ASSERTION_INDEX;
        }

        var_types toType = cast->GetCastType();

        if (!varTypeIsSmall(toType))
        {
            return NO_ASSERTION_INDEX;
        }

        // TODO-MIKE-Review: Restricting the cast operands to SSA_USE is largely superfluous,
        // all it does is preventing less useful assertions from being created, that would
        // just wast space in the assertion table.
        if (!value->IsLclUse())
        {
            return NO_ASSERTION_INDEX;
        }

        LclVarDsc* lcl = compiler->lvaGetDesc(value->AsLclUse()->GetDef()->GetLclNum());

        assert(!lcl->IsAddressExposed());

        if (lcl->IsPromotedField() && lcl->lvNormalizeOnLoad())
        {
            return NO_ASSERTION_INDEX;
        }

        const auto& range = GetSmallTypeRange(toType);

        return AddRangeAssertion(value->GetConservativeVN(), range.min, range.max);
    }

    // TODO: only copy assertions rely on valid SSA number so we could generate more assertions here
    AssertionIndex CreateEqualityAssertion(GenTreeLclUse* op1, GenTree* op2, ApKind kind)
    {
        assert((op1 != nullptr) && (op2 != nullptr));
        assert((kind == OAK_EQUAL) || (kind == OAK_NOT_EQUAL));

        LclVarDsc* lcl = compiler->lvaGetDesc(op1->GetDef()->GetLclNum());

        assert(!lcl->IsAddressExposed());

        op2 = op2->SkipComma();

        AssertionDsc assertion;

        switch (op2->GetOper())
        {
            case GT_CNS_INT:
#ifdef TARGET_ARM
                if (!emitter::emitIns_valid_imm_for_mov(op2->AsIntCon()->GetInt32Value()))
                {
                    return NO_ASSERTION_INDEX;
                }
#endif

                if (lcl->TypeIs(TYP_LONG) && !op2->TypeIs(TYP_LONG))
                {
                    return NO_ASSERTION_INDEX;
                }

                assertion.op1.kind         = O1K_LCLVAR;
                assertion.op1.lclNum       = op1->GetDef()->GetLclNum();
                assertion.op2.kind         = O2K_CONST_INT;
                assertion.op2.intCon.value = op2->AsIntCon()->GetValue(lcl->GetType());
                assertion.op2.intCon.flags = op2->AsIntCon()->GetHandleKind();
                break;

#ifndef TARGET_64BIT
            case GT_CNS_LNG:
                assertion.op1.kind         = O1K_LCLVAR;
                assertion.op1.lclNum       = op1->GetDef()->GetLclNum();
                assertion.op2.kind         = O2K_CONST_LONG;
                assertion.op2.lngCon.value = op2->AsLngCon()->GetValue();
                break;
#endif

            case GT_CNS_DBL:
                // TODO-MIKE-Cleanup: This doesn't really belong here. An "x == NaN" assertion is
                // fine in itself, the problem is that we can't really substitute x with NaN in
                // subsequent code because NaN has multiple values. And then "x == NaN" is always
                // false so we shouldn't even see this case, except that VN doesn't handle this.
                if (_isnan(op2->AsDblCon()->GetValue()))
                {
                    return NO_ASSERTION_INDEX;
                }

                assertion.op1.kind         = O1K_LCLVAR;
                assertion.op1.lclNum       = op1->GetDef()->GetLclNum();
                assertion.op2.kind         = O2K_CONST_DOUBLE;
                assertion.op2.dblCon.value = op2->AsDblCon()->GetValue();
                break;

            case GT_LCL_VAR:
            {
                if (op1->GetDef()->GetLclNum() == op2->AsLclVar()->GetLclNum())
                {
                    return NO_ASSERTION_INDEX;
                }

                LclVarDsc* valLcl = compiler->lvaGetDesc(op2->AsLclVar());

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

                assertion.op1.kind = O1K_VALUE_NUMBER;
                assertion.op2.kind = O2K_VALUE_NUMBER;
                break;
            }

            case GT_LCL_USE:
            {
                if (op1->GetDef()->GetLclNum() == op2->AsLclUse()->GetDef()->GetLclNum())
                {
                    return NO_ASSERTION_INDEX;
                }

                LclVarDsc* valLcl = compiler->lvaGetDesc(op2->AsLclUse()->GetDef()->GetLclNum());

                assert(!valLcl->IsAddressExposed());

                if (lcl->GetType() != valLcl->GetType())
                {
                    return NO_ASSERTION_INDEX;
                }

                if (valLcl->lvNormalizeOnLoad() && !lcl->lvNormalizeOnLoad())
                {
                    return NO_ASSERTION_INDEX;
                }

                assertion.op1.kind = O1K_VALUE_NUMBER;
                assertion.op2.kind = O2K_VALUE_NUMBER;
                break;
            }

            default:
                return NO_ASSERTION_INDEX;
        }

        assertion.kind   = kind;
        assertion.op1.vn = op1->GetConservativeVN();
        assertion.op2.vn = op2->GetConservativeVN();

        if ((assertion.op1.vn == NoVN) || (assertion.op2.vn == NoVN))
        {
            return NO_ASSERTION_INDEX;
        }

        return AddEqualityAssertions(assertion);
    }

    AssertionIndex AddAssertion(const AssertionDsc& assertion)
    {
        assert((assertion.kind == OAK_EQUAL) || (assertion.kind == OAK_NOT_EQUAL));
        INDEBUG(DebugCheckAssertion(assertion));

        for (AssertionIndex index = assertionCount; index >= 1; index--)
        {
            if (GetAssertion(index) == assertion)
            {
                return index;
            }
        }

        if (assertionCount >= assertionTableSize)
        {
            return NO_ASSERTION_INDEX;
        }

        assertionTable[assertionCount++] = assertion;

        DBEXEC(verbose, TraceAssertion("generates", assertionTable[assertionCount - 1]);)

        if (assertion.kind == OAK_NOT_EQUAL)
        {
            AddVNAssertion(assertion.op1.vn, assertionCount);
        }

        return assertionCount;
    }

    AssertionIndex AddEqualityAssertions(AssertionDsc& assertion)
    {
        assert((assertion.kind == OAK_EQUAL) || (assertion.kind == OAK_NOT_EQUAL));

        AssertionIndex index = AddAssertion(assertion);

        if (index != NO_ASSERTION_INDEX)
        {
            assertion.kind               = assertion.kind == OAK_EQUAL ? OAK_NOT_EQUAL : OAK_EQUAL;
            AssertionIndex invertedIndex = AddAssertion(assertion);

            if (invertedIndex != NO_ASSERTION_INDEX)
            {
                invertedAssertions[index - 1]         = static_cast<uint16_t>(invertedIndex);
                invertedAssertions[invertedIndex - 1] = static_cast<uint16_t>(index);
            }
        }

        return index;
    }

    AssertionIndex AddRangeAssertion(ValueNum vn, ssize_t min, ssize_t max)
    {
        assert(vn != NoVN);
        assert((min >= INT32_MIN) && (max <= INT32_MAX) && (min <= max));

        for (AssertionIndex index = assertionCount; index >= 1; index--)
        {
            const AssertionDsc& existing = GetAssertion(index);

            if ((existing.kind == OAK_RANGE) && (existing.op1.vn == vn) && (existing.op2.range.min == min) &&
                (existing.op2.range.max == max))
            {
                return index;
            }
        }

        if (assertionCount >= assertionTableSize)
        {
            return NO_ASSERTION_INDEX;
        }

        AssertionIndex index     = ++assertionCount;
        AssertionDsc&  assertion = assertionTable[index - 1];

        assertion.kind      = OAK_RANGE;
        assertion.op1.kind  = O1K_VALUE_NUMBER;
        assertion.op1.vn    = vn;
        assertion.op2.kind  = O2K_RANGE;
        assertion.op2.vn    = NoVN;
        assertion.op2.range = {min, max};

        DBEXEC(verbose, TraceAssertion("generates", assertionTable[index - 1]);)

        AddVNAssertion(vn, index);

        return index;
    }

    AssertionIndex AddRangeAssertions(genTreeOps oper, ValueNum vn, ValueNum limitVN)
    {
        assert((GT_LT <= oper) && (oper <= GT_GT));
        assert(vnStore->IsVNInt32Constant(limitVN));

        ssize_t limit = vnStore->ConstantValue<int32_t>(limitVN);
        ssize_t min   = INT32_MIN;
        ssize_t max   = INT32_MAX;

        switch (oper)
        {
            case GT_LT:
                if (limit == INT32_MIN)
                {
                    return NO_ASSERTION_INDEX;
                }
                max = limit - 1;
                break;
            case GT_LE:
                if (limit == INT32_MAX)
                {
                    return NO_ASSERTION_INDEX;
                }
                max = limit;
                break;
            case GT_GE:
                if (limit == INT32_MIN)
                {
                    return NO_ASSERTION_INDEX;
                }
                min = limit;
                break;
            default:
                assert(oper == GT_GT);
                if (limit == INT32_MAX)
                {
                    return NO_ASSERTION_INDEX;
                }
                min = limit + 1;
                break;
        }

        AssertionIndex index = AddRangeAssertion(vn, min, max);

        if (index == NO_ASSERTION_INDEX)
        {
            return NO_ASSERTION_INDEX;
        }

        if (max == INT32_MAX)
        {
            assert(min != INT32_MIN);

            max = min - 1;
            min = INT32_MIN;
        }
        else
        {
            assert(min == INT32_MIN);

            min = max + 1;
            max = INT32_MAX;
        }

        AssertionIndex invertedIndex = AddRangeAssertion(vn, min, max);

        if (invertedIndex != NO_ASSERTION_INDEX)
        {
            invertedAssertions[index - 1]         = static_cast<uint16_t>(invertedIndex);
            invertedAssertions[invertedIndex - 1] = static_cast<uint16_t>(index);
        }

        return index;
    }

    AssertionIndex AddBoundsChkAssertion(ValueNum indexVN, ValueNum lengthVN)
    {
        assert(varActualTypeIsIntOrI(vnStore->TypeOfVN(indexVN)));
        assert(varActualTypeIsIntOrI(vnStore->TypeOfVN(lengthVN)));

        for (AssertionIndex index = assertionCount; index >= 1; index--)
        {
            const AssertionDsc& existing = GetAssertion(index);

            if ((existing.kind == OAK_BOUNDS_CHK) && (existing.op1.vn == indexVN) && (existing.op2.vn == lengthVN))
            {
                return index;
            }
        }

        if (assertionCount >= assertionTableSize)
        {
            return NO_ASSERTION_INDEX;
        }

        AssertionIndex index     = ++assertionCount;
        AssertionDsc&  assertion = assertionTable[index - 1];

        assertion.kind     = OAK_BOUNDS_CHK;
        assertion.op1.kind = O1K_VALUE_NUMBER;
        assertion.op1.vn   = indexVN;
        assertion.op2.kind = O2K_VALUE_NUMBER;
        assertion.op2.vn   = lengthVN;

        DBEXEC(verbose, TraceAssertion("generates", assertionTable[index - 1]);)

        return index;
    }

    void AddVNAssertion(ValueNum vn, AssertionIndex index)
    {
        ASSERT_TP* set = vnAssertionMap->Emplace(vn);

        if (*set == BitVecOps::UninitVal())
        {
            *set = BitVecOps::MakeSingleton(&sizeTraits, index - 1);
        }
        else
        {
            BitVecOps::AddElemD(&sizeTraits, *set, index - 1);
        }
    }

    const ASSERT_TP GetVNAssertions(ValueNum vn) const
    {
        ASSERT_TP* set = vnAssertionMap->LookupPointer(vn);

        return set == nullptr ? BitVecOps::UninitVal() : *set;
    }

    AssertionIndex GetInvertedAssertion(AssertionIndex index) const
    {
        assert((index != NO_ASSERTION_INDEX) && (index <= assertionCount));

        return invertedAssertions[index - 1];
    }

    AssertionInfo GenerateJTrueBoundAssertions(GenTreeOp* relop)
    {
        assert(relop->OperIsCompare());

        GenTree* op1 = relop->GetOp(0);
        GenTree* op2 = relop->GetOp(1);

        ValueNum relopVN = vnStore->VNNormalValue(relop->GetConservativeVN());
        ValueNum boundVN;

        ApKind kind;

        if (relop->OperIs(GT_EQ, GT_NE) && op2->IsIntegralConst(0))
        {
            kind    = relop->OperIs(GT_EQ) ? OAK_EQUAL : OAK_NOT_EQUAL;
            boundVN = vnStore->VNNormalValue(op1->GetConservativeVN());
        }
        else
        {
            kind    = OAK_NOT_EQUAL;
            boundVN = relopVN;
        }

        VNFuncApp funcApp;

        if (vnStore->GetVNFunc(boundVN, &funcApp) && ValueNumStore::IsVNCompareCheckedBoundRelop(funcApp))
        {
            ApOp1Kind boundKind = O1K_INVALID;

            // "(i +/- c1) LT|LE|GE|GT (j +/- c2)" where either i or j is used as length by an ARR_BOUNDS_CHK
            if (vnStore->IsVNCompareCheckedBoundArith(funcApp))
            {
                boundKind = O1K_BOUND_OPER_BND;
            }
            // "i LT|LE|GE|GT j" where either i or j is used as length by an ARR_BOUNDS_CHK
            else if (vnStore->IsVNCompareCheckedBound(funcApp))
            {
                boundKind = O1K_BOUND_LOOP_BND;
            }
            // "i LT|LE|GE|GT j" where either i or j is constant
            else
            {
                genTreeOps oper    = static_cast<genTreeOps>(funcApp.m_func);
                ValueNum   vn      = funcApp[0];
                ValueNum   limitVN = funcApp[1];

                if (vnStore->IsVNInt32Constant(vn))
                {
                    std::swap(vn, limitVN);
                    oper = GenTree::SwapRelop(oper);
                }
                else if (!vnStore->IsVNInt32Constant(limitVN))
                {
                    return NO_ASSERTION_INDEX;
                }

                if (kind == OAK_EQUAL)
                {
                    oper = GenTree::ReverseRelop(oper);
                }

                return AddRangeAssertions(oper, vn, limitVN);
            }

            AssertionDsc dsc;

            dsc.kind             = kind;
            dsc.op1.kind         = boundKind;
            dsc.op1.vn           = boundVN;
            dsc.op2.kind         = O2K_CONST_INT;
            dsc.op2.vn           = vnStore->VNForIntCon(0);
            dsc.op2.intCon.value = 0;
            dsc.op2.intCon.flags = GTF_EMPTY;

            return AddEqualityAssertions(dsc);
        }

        return GenerateJTrueUnsignedBoundAssertions(relopVN);
    }

    AssertionInfo GenerateJTrueUnsignedBoundAssertions(ValueNum vn)
    {
        VNFuncApp funcApp;

        if (!vnStore->GetVNFunc(vn, &funcApp))
        {
            return NO_ASSERTION_INDEX;
        }

        // Conditions like "(uint)i < (uint)length" generate BoundsChk or Range assertions.

        if ((funcApp.m_func == VNF_GT_UN) || (funcApp.m_func == VNF_LE_UN))
        {
            funcApp.m_func = funcApp.m_func == VNF_GT_UN ? VNF_LT_UN : VNF_GE_UN;
            std::swap(funcApp.m_args[0], funcApp.m_args[1]);
        }
        else if ((funcApp.m_func != VNF_LT_UN) && (funcApp.m_func != VNF_GE_UN))
        {
            return NO_ASSERTION_INDEX;
        }

        AssertionIndex index;
        bool           isTrue = false;

        if (vnStore->IsVNCheckedBound(funcApp[1]))
        {
            // "(uint)i < (uint)len" or "(uint)i >= (uint)len" => BoundsChk(i, len)
            if (!vnStore->IsVNInt32Constant(funcApp[0]))
            {
                index = AddBoundsChkAssertion(funcApp[0], funcApp[1]);
            }
            else
            {
                // "C < (uint)len" or "C >= (uint)len" => len IN [C+1..INT32_MAX]
                ssize_t constVal = vnStore->ConstantValue<int32_t>(funcApp[0]);

                if ((constVal < 0) || (constVal == INT32_MAX))
                {
                    return NO_ASSERTION_INDEX;
                }

                index = AddRangeAssertion(funcApp[1], constVal + 1, INT32_MAX);
            }

            isTrue = funcApp.m_func == VNF_LT_UN;
        }
        else if (vnStore->IsVNCheckedBound(funcApp[0]) && vnStore->IsVNInt32Constant(funcApp[1]))
        {
            // "(uint)len < C" or "(uint)len >= C" => len IN [C..INT32_MAX]
            ssize_t constVal = vnStore->ConstantValue<int32_t>(funcApp[1]);

            if (constVal <= 0)
            {
                return NO_ASSERTION_INDEX;
            }

            index  = AddRangeAssertion(funcApp[0], constVal, INT32_MAX);
            isTrue = funcApp.m_func == VNF_GE_UN;
        }
        else
        {
            return NO_ASSERTION_INDEX;
        }

        return isTrue ? index : AssertionInfo::ForNextEdge(index);
    }

    GenTree* GetCseValue(GenTree* node)
    {
        if (!node->OperIs(GT_COMMA))
        {
            return node;
        }

        GenTree* op1 = node->AsOp()->GetOp(0);
        GenTree* op2 = node->AsOp()->GetOp(1);

        if (op2->OperIs(GT_LCL_VAR) && op1->OperIs(GT_STORE_LCL_VAR))
        {
            if (op1->AsLclVar()->GetLclNum() == op2->AsLclVar()->GetLclNum())
            {
                return op1->AsLclVar()->GetOp(0);
            }
        }
        else if (op2->OperIs(GT_LCL_USE) && op1->OperIs(GT_LCL_DEF))
        {
            if (op2->AsLclUse()->GetDef() == op1->AsLclDef())
            {
                return op1->AsLclDef()->GetValue();
            }
        }

        return node;
    }

    AssertionInfo GenerateJTrueEqualityAssertions(GenTreeOp* relop)
    {
        assert(relop->OperIs(GT_EQ, GT_NE));

        ApKind assertionKind = relop->OperIs(GT_EQ) ? OAK_EQUAL : OAK_NOT_EQUAL;

        // Look through any CSEs so we see the actual trees providing values, if possible.
        // This is important for exact type assertions, which need to see the GT_IND.
        GenTree* op1 = GetCseValue(relop->GetOp(0));
        GenTree* op2 = GetCseValue(relop->GetOp(1));

        // TODO-MIKE-Review: This is probably bogus, old code tried to swap operands so
        // that LCL_VAR comes first. But it didn't check if the local is in SSA form and
        // then CreateEqualityAssertion rejected it.
        if (op1->OperIs(GT_LCL_VAR) && op2->OperIs(GT_LCL_USE))
        {
            return NO_ASSERTION_INDEX;
        }

        if (!op1->OperIs(GT_LCL_USE) && op2->OperIs(GT_LCL_USE))
        {
            std::swap(op1, op2);
        }

        if (op1->OperIs(GT_LCL_USE) && op2->OperIs(GT_CNS_INT, GT_CNS_LNG, GT_CNS_DBL, GT_LCL_VAR, GT_LCL_USE))
        {
            return CreateEqualityAssertion(op1->AsLclUse(), op2, assertionKind);
        }

        // TODO-MIKE-Review: This is probably bogus, in old code CreateEqualityAssertion handled
        // this case and rejected AX locals, even though those can be handled by the code below.
        if (op1->OperIs(GT_LCL_VAR) && op2->OperIs(GT_CNS_INT, GT_CNS_LNG, GT_CNS_DBL))
        {
            return NO_ASSERTION_INDEX;
        }

        ValueNum op1VN = vnStore->VNNormalValue(op1->GetConservativeVN());
        ValueNum op2VN = vnStore->VNNormalValue(op2->GetConservativeVN());

        if (vnStore->IsVNCheckedBound(op1VN) && vnStore->IsVNInt32Constant(op2VN))
        {
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

                AssertionIndex index = AddAssertion(dsc);

                if (relop->OperIs(GT_NE) != (dsc.kind == OAK_NOT_EQUAL))
                {
                    return AssertionInfo::ForNextEdge(index);
                }

                return AssertionInfo(index);
            }
        }

        if (compiler->opts.IsReadyToRun())
        {
            return GenerateJTrueReadyToRunTypeAssertions(op1, op2, assertionKind);
        }
        else
        {
            return GenerateJTrueTypeAssertions(op1, op2, assertionKind);
        }
    }

    bool IsUnaliasedLocal(GenTree* node)
    {
        return node->OperIs(GT_LCL_USE) ||
               (node->OperIs(GT_LCL_VAR) && !compiler->lvaGetDesc(node->AsLclVar())->IsAddressExposed());
    }

    AssertionIndex GenerateJTrueReadyToRunTypeAssertions(GenTree* op1, GenTree* op2, ApKind assertionKind)
    {
        assert(compiler->opts.IsReadyToRun());

        if (!op1->OperIs(GT_IND) || !op2->OperIs(GT_IND))
        {
            return NO_ASSERTION_INDEX;
        }

        GenTree* addr = op1->AsIndir()->GetAddr();

        if (!addr->TypeIs(TYP_REF) || !IsUnaliasedLocal(addr))
        {
            return NO_ASSERTION_INDEX;
        }

        if ((op1->GetConservativeVN() == NoVN) || (op2->GetConservativeVN() == NoVN))
        {
            return NO_ASSERTION_INDEX;
        }

        AssertionDsc assertion;

        assertion.kind     = assertionKind;
        assertion.op1.kind = O1K_VALUE_NUMBER;
        assertion.op1.vn   = vnStore->VNNormalValue(op1->GetConservativeVN());
        assertion.op2.kind = O2K_VALUE_NUMBER;
        assertion.op2.vn   = vnStore->VNNormalValue(op2->GetConservativeVN());

        return AddEqualityAssertions(assertion);
    }

    AssertionIndex GenerateJTrueTypeAssertions(GenTree* op1, GenTree* op2, ApKind assertionKind)
    {
        assert((assertionKind == OAK_EQUAL) || (assertionKind == OAK_NOT_EQUAL));
        assert(!compiler->opts.IsReadyToRun());

        if (op1->OperIs(GT_IND) || op2->OperIs(GT_IND))
        {
            if (!op1->OperIs(GT_IND))
            {
                std::swap(op1, op2);
            }

            GenTree* addr = op1->AsIndir()->GetAddr();

            if (!addr->TypeIs(TYP_REF) || !IsUnaliasedLocal(addr) || (addr->GetConservativeVN() == NoVN))
            {
                return NO_ASSERTION_INDEX;
            }

            ValueNum objMTVN = vnStore->VNNormalValue(op1->GetConservativeVN());
            ValueNum mtVN    = vnStore->VNNormalValue(op2->GetConservativeVN());

            if ((objMTVN == NoVN) || (mtVN == NoVN))
            {
                return NO_ASSERTION_INDEX;
            }

            AssertionDsc assertion;

            assertion.kind     = assertionKind;
            assertion.op1.kind = O1K_VALUE_NUMBER;
            assertion.op1.vn   = objMTVN;
            assertion.op2.kind = O2K_VALUE_NUMBER;
            assertion.op2.vn   = mtVN;

            return AddEqualityAssertions(assertion);
        }

        if (op1->IsCall() || op2->IsCall())
        {
            if (!op1->IsCall())
            {
                std::swap(op1, op2);
            }

            GenTreeCall* call = op1->AsCall();

            if (!call->IsHelperCall() || !call->TypeIs(TYP_REF) || !op2->IsIntegralConst(0))
            {
                return NO_ASSERTION_INDEX;
            }

            CorInfoHelpFunc helper = Compiler::eeGetHelperNum(call->GetMethodHandle());

            // Note CORINFO_HELP_READYTORUN_ISINSTANCEOF does not have the same argument pattern.
            // In particular, it is not possible to deduce what class is being tested from its args.
            // Also note The CASTCLASS helpers won't appear in predicates as they throw on failure.
            // So the helper list here is smaller than the one in PropagateCall.

            if ((helper != CORINFO_HELP_ISINSTANCEOFINTERFACE) && (helper != CORINFO_HELP_ISINSTANCEOFARRAY) &&
                (helper != CORINFO_HELP_ISINSTANCEOFCLASS) && (helper != CORINFO_HELP_ISINSTANCEOFANY))
            {
                return NO_ASSERTION_INDEX;
            }

            GenTree* objectArg = call->GetArgNodeByArgNum(1);
            GenTree* mtArg     = call->GetArgNodeByArgNum(0);

            assert(objectArg->TypeIs(TYP_REF));
            assert(mtArg->TypeIs(TYP_I_IMPL));

            if (!IsUnaliasedLocal(objectArg) || (objectArg->GetConservativeVN() == NoVN))
            {
                return NO_ASSERTION_INDEX;
            }

            ValueNum objVN = objectArg->GetConservativeVN();
            ValueNum mtVN  = vnStore->VNNormalValue(mtArg->GetConservativeVN());

            if ((objVN == NoVN) || (mtVN == NoVN))
            {
                return NO_ASSERTION_INDEX;
            }

            assert(vnStore->TypeOfVN(objVN) == TYP_REF);
            assert(vnStore->TypeOfVN(mtVN) == TYP_I_IMPL);

            AssertionDsc assertion;

            assertion.kind     = (assertionKind == OAK_EQUAL) ? OAK_NOT_EQUAL : OAK_EQUAL;
            assertion.op1.kind = O1K_INSTANCE_OF;
            assertion.op1.vn   = objVN;
            assertion.op2.kind = O2K_VALUE_NUMBER;
            assertion.op2.vn   = mtVN;

            return AddEqualityAssertions(assertion);
        }

        return NO_ASSERTION_INDEX;
    }

    AssertionInfo GenerateJTrueAssertions(GenTreeOp* relop)
    {
        AssertionInfo info = GenerateJTrueBoundAssertions(relop);

        if (info.HasAssertion())
        {
            return info;
        }

        // TODO-CQ: add other relop operands. Disabled for now to measure perf
        // and not occupy assertion table slots. We'll add them when used.
        if (relop->OperIs(GT_EQ, GT_NE))
        {
            return GenerateJTrueEqualityAssertions(relop);
        }

        return NO_ASSERTION_INDEX;
    }

    void GenerateNodeAssertions(GenTree* node)
    {
        node->ClearAssertionInfo();

        INDEBUG(currentNode = node);

        AssertionInfo assertionInfo;

        switch (node->GetOper())
        {
            case GT_BOUNDS_CHECK:
                assertionInfo = GenerateBoundsChkAssertion(node->AsBoundsChk());
                break;

            case GT_BLK:
            case GT_OBJ:
                assert(node->AsBlk()->GetLayout()->GetSize() != 0);
                FALLTHROUGH;
            case GT_IND:
            case GT_NULLCHECK:
                assertionInfo = CreateNotNullAssertion(node->AsIndir()->GetAddr());
                break;
            case GT_ARR_LENGTH:
                assertionInfo = CreateNotNullAssertion(node->AsArrLen()->GetArray());
                break;
            case GT_ARR_ELEM:
                assertionInfo = CreateNotNullAssertion(node->AsArrElem()->GetArray());
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
                        assertionInfo = CreateNotNullAssertion(call->GetThisArg());
                    }
                }
                break;

            case GT_CAST:
                // We create a range assertion for a CAST's operand, not for the CAST itself.
                // This assertion isn't known to be true at this time (and thus its index is
                // not recorded in any node) but it can later be implied to be true by other
                // assertions and then we can remove the cast (e.g. a const assertion x = 42
                // implies CAST<UBYTE>(x) can be reduced to x).
                // TODO-MIKE-Review: Why don't we just check for the relevant const assertion
                // when we propagate to CAST?!? Given the diffs this seems to be doing more
                // harm than good - there are very few cases where this helps and instead
                // there are some cases where it just wastes space in the assertion table and
                // prevents other useful assertions from being created.
                CreateRangeAssertion(node->AsCast());
                break;

            case GT_JTRUE:
                if (node->AsUnOp()->GetOp(0)->OperIsCompare())
                {
                    assertionInfo = GenerateJTrueAssertions(node->AsUnOp()->GetOp(0)->AsOp());
                }
                break;

            default:
                break;
        }

        if (assertionInfo.HasAssertion())
        {
            node->SetAssertionInfo(assertionInfo);
        }
    }

    GenTree* PropagateLclVarConst(const AssertionDsc& assertion, GenTreeLclVar* lclVar, Statement* stmt)
    {
#ifdef DEBUG
        LclVarDsc* lcl = compiler->lvaGetDesc(lclVar);

        assert(!lcl->IsAddressExposed() && !lcl->lvIsCSE);
        assert(lclVar->GetType() == lcl->GetType());
        assert(!varTypeIsStruct(lclVar->GetType()));

        DBEXEC(verbose, TraceAssertion("propagating", assertion);)
#endif

        const auto& val     = assertion.op2;
        GenTree*    conNode = nullptr;

        switch (val.kind)
        {
            case O2K_CONST_DOUBLE:
                // "x == 0.0" implies both "x == 0.0" and "x == -0.0" so we can't substitute x with 0.0.
                if (val.dblCon.value == 0.0)
                {
                    return nullptr;
                }

                conNode = lclVar->ChangeToDblCon(val.dblCon.value);
                break;

#ifndef TARGET_64BIT
            case O2K_CONST_LONG:
                if (!lclVar->TypeIs(TYP_LONG))
                {
                    return nullptr;
                }

                conNode = lclVar->ChangeToLngCon(val.lngCon.value);
                break;
#endif

            default:
                assert(val.kind == O2K_CONST_INT);

                if ((val.intCon.flags & GTF_ICON_HDL_MASK) == 0)
                {
                    conNode = lclVar->ChangeToIntCon(varActualType(lclVar->GetType()), val.intCon.value);
                }
                else if (compiler->opts.compReloc)
                {
                    return nullptr;
                }
                else
                {
                    conNode = lclVar->ChangeToIntCon(TYP_I_IMPL, val.intCon.value);
                    conNode->AsIntCon()->SetHandleKind(val.intCon.flags & GTF_ICON_HDL_MASK);
                }
                break;
        }

        assert(vnStore->IsVNConstant(val.vn));

        conNode->gtVNPair.SetBoth(val.vn);

        return UpdateTree(conNode, lclVar, stmt);
    }

    GenTree* PropagateSsaUseConst(const AssertionDsc& assertion, GenTreeLclUse* use, Statement* stmt)
    {
#ifdef DEBUG
        LclVarDsc* lcl = compiler->lvaGetDesc(use->GetDef()->GetLclNum());

        assert(!lcl->IsAddressExposed() && !lcl->lvIsCSE);
        assert(use->GetType() == lcl->GetType());
        assert(!varTypeIsStruct(use->GetType()));

        DBEXEC(verbose, TraceAssertion("propagating", assertion);)
#endif

        const auto& val = assertion.op2;
        GenTree*    conNode;

        switch (val.kind)
        {
            case O2K_CONST_DOUBLE:
                // "x == 0.0" implies both "x == 0.0" and "x == -0.0" so we can't substitute x with 0.0.
                if (val.dblCon.value == 0.0)
                {
                    return nullptr;
                }

                use->GetDef()->RemoveUse(use);
                conNode = use->ChangeToDblCon(val.dblCon.value);
                break;

#ifndef TARGET_64BIT
            case O2K_CONST_LONG:
                if (!use->TypeIs(TYP_LONG))
                {
                    return nullptr;
                }

                use->GetDef()->RemoveUse(use);
                conNode = use->ChangeToLngCon(val.lngCon.value);
                break;
#endif

            default:
                assert(val.kind == O2K_CONST_INT);

                if ((val.intCon.flags & GTF_ICON_HDL_MASK) == 0)
                {
                    use->GetDef()->RemoveUse(use);
                    conNode = use->ChangeToIntCon(varActualType(use->GetType()), val.intCon.value);
                }
                else if (compiler->opts.compReloc)
                {
                    return nullptr;
                }
                else
                {
                    use->GetDef()->RemoveUse(use);
                    conNode = use->ChangeToIntCon(TYP_I_IMPL, val.intCon.value);
                    conNode->AsIntCon()->SetHandleKind(val.intCon.flags & GTF_ICON_HDL_MASK);
                }
                break;
        }

        assert(vnStore->IsVNConstant(val.vn));

        conNode->gtVNPair.SetBoth(val.vn);

        return UpdateTree(conNode, use, stmt);
    }

    const AssertionDsc* FindConstAssertion(const ASSERT_TP assertions, ValueNum vn, unsigned lclNum)
    {
        for (BitVecOps::Enumerator en(&countTraits, assertions); en.MoveNext();)
        {
            const AssertionDsc& assertion = GetAssertion(GetAssertionIndex(en.Current()));

            if ((assertion.kind != OAK_EQUAL) || (assertion.op1.kind != O1K_LCLVAR))
            {
                continue;
            }

            if (assertion.op1.vn != vn)
            {
                continue;
            }

            if (assertion.op1.lclNum != lclNum)
            {
                continue;
            }

            if ((assertion.op2.kind != O2K_CONST_INT) &&
#ifndef TARGET_64BIT
                (assertion.op2.kind != O2K_CONST_LONG) &&
#endif
                (assertion.op2.kind != O2K_CONST_DOUBLE))
            {
                continue;
            }

            return &assertion;
        }

        return nullptr;
    }

    GenTree* PropagateLclVarUse(const ASSERT_TP assertions, GenTreeLclVar* lclVar, Statement* stmt)
    {
        assert(lclVar->OperIs(GT_LCL_VAR) && ((lclVar->gtFlags & GTF_VAR_DEF) == 0));

        LclVarDsc* lcl = compiler->lvaGetDesc(lclVar);

        // TODO-MIKE-Review: It's not clear why propagation is blocked for AX and CSE temps.
        // For AX it should be perfectly fine to do it, we're using VN after all. And we'd
        // be replacing a memory load with a constant so the trade offs are pretty clear.
        //
        // CSE temps have the same trade offs as other locals - const propagation is usually
        // good because it can result is more constant folding and breaks dependency chains.
        // But it can also be bad - if there's no further constant folding then we'll end
        // up loading a constant into a register that already contains that constant and
        // that will just increase code size, especially on x86/64.
        // But is there anything specific to CSE temps that warrants blocking const prop?
        if (lcl->IsAddressExposed() || lcl->lvIsCSE)
        {
            return nullptr;
        }

        // TODO-MIKE-Review: This likely blocks const propagation to small int locals for no reason.
        if (lclVar->GetType() != lcl->GetType())
        {
            return nullptr;
        }

        // There are no struct/vector equality relops.
        if (varTypeIsStruct(lclVar->GetType()))
        {
            return nullptr;
        }

        const AssertionDsc* assertion =
            FindConstAssertion(assertions, lclVar->GetConservativeVN(), lclVar->GetLclNum());

        if (assertion == nullptr)
        {
            return nullptr;
        }

        return PropagateLclVarConst(*assertion, lclVar, stmt);
    }

    GenTree* PropagateSsaUse(const ASSERT_TP assertions, GenTreeLclUse* use, Statement* stmt)
    {
        unsigned   lclNum = use->GetDef()->GetLclNum();
        LclVarDsc* lcl    = compiler->lvaGetDesc(lclNum);

        assert(!lcl->IsAddressExposed());

        // TODO-MIKE-Review: It's not clear why propagation is blocked for CSE temps.

        if (lcl->lvIsCSE)
        {
            return nullptr;
        }

        // TODO-MIKE-Review: This likely blocks const propagation to small int locals for no reason.
        if (use->GetType() != lcl->GetType())
        {
            return nullptr;
        }

        // There are no struct/vector equality relops.
        if (varTypeIsStruct(use->GetType()))
        {
            return nullptr;
        }

        const AssertionDsc* assertion = FindConstAssertion(assertions, use->GetConservativeVN(), lclNum);

        if (assertion == nullptr)
        {
            return nullptr;
        }

        return PropagateSsaUseConst(*assertion, use, stmt);
    }

    const AssertionDsc* FindEqualityAssertion(const ASSERT_TP assertions, GenTree* op1, GenTree* op2)
    {
        ValueNum vn1 = vnStore->VNNormalValue(op1->GetConservativeVN());
        ValueNum vn2 = vnStore->VNNormalValue(op2->GetConservativeVN());

        for (BitVecOps::Enumerator en(&countTraits, assertions); en.MoveNext();)
        {
            const AssertionDsc& assertion = GetAssertion(GetAssertionIndex(en.Current()));

            if (((assertion.kind == OAK_EQUAL) || (assertion.kind == OAK_NOT_EQUAL)) && (assertion.op1.vn == vn1) &&
                (assertion.op2.vn == vn2))
            {
                return &assertion;
            }
        }

        return nullptr;
    }

    const AssertionDsc* FindZeroEqualityAssertion(const ASSERT_TP assertions, ValueNum vn, var_types type)
    {
        ValueNum vn1 = vnStore->VNNormalValue(vn);
        ValueNum vn2 = vnStore->VNZeroForType(type);

        for (BitVecOps::Enumerator en(&countTraits, assertions); en.MoveNext();)
        {
            const AssertionDsc& assertion = GetAssertion(GetAssertionIndex(en.Current()));

            if ((assertion.kind != OAK_EQUAL) && (assertion.kind != OAK_NOT_EQUAL))
            {
                continue;
            }

            if ((assertion.op1.vn == vn1) && (assertion.op2.vn == vn2))
            {
                return &assertion;
            }
        }

        return nullptr;
    }

    AssertionInfo FindRelopRangeAssertion(const ASSERT_TP assertions, genTreeOps oper, ValueNum vn, ValueNum limitVN)
    {
        assert((GT_LT <= oper) && (oper <= GT_GT));
        assert(vnStore->IsVNInt32Constant(limitVN));

        ssize_t limit = vnStore->ConstantValue<int32_t>(limitVN);
        ssize_t min   = INT32_MIN;
        ssize_t max   = INT32_MAX;

        switch (oper)
        {
            case GT_LT:
                if (limit == INT32_MIN)
                {
                    return NO_ASSERTION_INDEX;
                }
                max = limit - 1;
                break;
            case GT_LE:
                if (limit == INT32_MAX)
                {
                    return NO_ASSERTION_INDEX;
                }
                max = limit;
                break;
            case GT_GE:
                if (limit == INT32_MIN)
                {
                    return NO_ASSERTION_INDEX;
                }
                min = limit;
                break;
            default:
                assert(oper == GT_GT);
                if (limit == INT32_MAX)
                {
                    return NO_ASSERTION_INDEX;
                }
                min = limit + 1;
                break;
        }

        for (BitVecOps::Enumerator en(&countTraits, assertions); en.MoveNext();)
        {
            const AssertionIndex index     = GetAssertionIndex(en.Current());
            const AssertionDsc&  assertion = GetAssertion(index);

            if ((assertion.kind != OAK_RANGE) || (assertion.op1.vn != vn))
            {
                continue;
            }

            const auto range = assertion.op2.range;

            if ((range.min >= min) && (range.max <= max))
            {
                return index;
            }

            if ((range.min > max) || (range.max < min))
            {
                return AssertionInfo::ForNextEdge(index);
            }
        }

        return NO_ASSERTION_INDEX;
    }

    GenTree* PropagateRelop(const ASSERT_TP assertions, GenTreeOp* relop, Statement* stmt)
    {
        assert(relop->OperIsCompare());

        if ((relop->gtFlags & GTF_SIDE_EFFECT) != 0)
        {
            return nullptr;
        }

        // TODO-MIKE-CQ: This is inconsistent with GenerateJTrueBoundAssertions, where we
        // skip "x EQ/NE 0" when x is LT|LE|GE|GT, this has caused a couple of regressions
        // when converting constant bound assertions to ranges. We should probably always
        // skip "x EQ/NE 0" when x is any relop, but then that looks like an issue caused
        // by the lack of forward substitution.

        const AssertionDsc* assertion =
            FindZeroEqualityAssertion(assertions, relop->GetConservativeVN(), relop->GetType());
        bool isTrue;

        if (assertion != nullptr)
        {
            DBEXEC(verbose, TraceAssertion("propagating", *assertion);)

            isTrue = assertion->kind == OAK_NOT_EQUAL;
        }
        else if (relop->OperIs(GT_EQ, GT_NE))
        {
            GenTree* op1 = relop->GetOp(0);
            GenTree* op2 = relop->GetOp(1);

            if (!op1->OperIs(GT_LCL_VAR, GT_LCL_USE, GT_IND))
            {
                return nullptr;
            }

            assertion = FindEqualityAssertion(assertions, op1, op2);

            if (assertion == nullptr)
            {
                return nullptr;
            }

            DBEXEC(verbose, TraceAssertion("propagating", *assertion);)

            isTrue = (assertion->kind == OAK_EQUAL) == relop->OperIs(GT_EQ);
        }
        else if (!relop->IsUnsigned() && varActualTypeIsInt(relop->GetOp(0)))
        {
            genTreeOps oper    = relop->GetOper();
            ValueNum   vn      = vnStore->VNNormalValue(relop->GetOp(0)->GetConservativeVN());
            ValueNum   limitVN = vnStore->VNNormalValue(relop->GetOp(1)->GetConservativeVN());

            if (vnStore->IsVNInt32Constant(vn))
            {
                std::swap(vn, limitVN);
                oper = GenTree::SwapRelop(oper);
            }
            else if (!vnStore->IsVNInt32Constant(limitVN))
            {
                return nullptr;
            }

            AssertionInfo info = FindRelopRangeAssertion(assertions, oper, vn, limitVN);

            if (!info.HasAssertion())
            {
                return nullptr;
            }

            DBEXEC(verbose, TraceAssertion("propagating", GetAssertion(info.GetAssertionIndex()));)

            isTrue = !info.IsNextEdgeAssertion();
        }
        else
        {
            return nullptr;
        }

        relop->ChangeToIntCon(isTrue);
        relop->gtVNPair.SetBoth(vnStore->VNForIntCon(isTrue));
        return UpdateTree(relop, relop, stmt);
    }

    const AssertionDsc* FindCastRangeAssertion(const ASSERT_TP assertions, ValueNum vn, ssize_t min, ssize_t max)
    {
        ssize_t intersectionMin = INT32_MIN;
        ssize_t intersectionMax = INT32_MAX;

        for (BitVecOps::Enumerator en(&countTraits, assertions); en.MoveNext();)
        {
            const AssertionDsc& assertion = GetAssertion(GetAssertionIndex(en.Current()));

            if ((assertion.kind != OAK_RANGE) || (assertion.op1.vn != vn))
            {
                continue;
            }

            intersectionMin = max(intersectionMin, assertion.op2.range.min);
            intersectionMax = min(intersectionMax, assertion.op2.range.max);

            if ((min <= intersectionMin) && (intersectionMax <= max))
            {
                return &assertion;
            }
        }

        return nullptr;
    }

    GenTree* PropagateCast(const ASSERT_TP assertions, GenTreeCast* cast, Statement* stmt)
    {
        GenTree*  op1      = cast->GetOp(0);
        var_types fromType = op1->GetType();
        var_types toType   = cast->GetCastType();

        if (!varTypeIsIntegral(toType) || !varTypeIsIntegral(fromType))
        {
            return nullptr;
        }

        GenTree* actualOp1 = op1->SkipComma();
        ValueNum vn;

        if (actualOp1->OperIs(GT_LCL_VAR, GT_LCL_USE))
        {
            LclVarDsc* lcl =
                compiler->lvaGetDesc(actualOp1->OperIs(GT_LCL_VAR) ? actualOp1->AsLclVar()->GetLclNum()
                                                                   : actualOp1->AsLclUse()->GetDef()->GetLclNum());

            // TODO-MIKE-Review: Usually we can't eliminate load "normalization" casts.
            // They're usually present on every LCL_VAR use so we'll never get assertions
            // about the LCL_VAR value itself (e.g. usually we have "if ((byte)b < 42)",
            // not "if (b < 42)"). xunit assemblies have a few cases where these casts do
            // get eliminated but it turns out that this skews register allocation in such
            // a way that the codegen end up being worse.
            // Besides, the way load/store "normalization" is implemented is just asking
            // for trouble so it's best to ignore these casts for now.
            if (lcl->lvNormalizeOnLoad())
            {
                return nullptr;
            }

            vn = actualOp1->GetConservativeVN();
        }
        else
        {
            vn = vnStore->VNNormalValue(actualOp1->GetConservativeVN());
        }

        ssize_t min;
        ssize_t max;

        if (varTypeIsSmall(toType))
        {
            min = cast->IsUnsigned() ? 0 : GetSmallTypeRange(toType).min;
            max = GetSmallTypeRange(toType).max;
        }
        else
        {
            // For all other cases keep it simple - the 0..INT32_MAX range allows us to
            // remove overflow checks from LONG/INT casts and also change sign extension
            // to zero extension. We'll miss some cases, such as 0..UINT32_MAX needed for
            // LONG -> UINT casts but these should be pretty rare.
            min = 0;
            max = INT32_MAX;
        }

        const AssertionDsc* assertion = FindCastRangeAssertion(assertions, vn, min, max);

        if (assertion == nullptr)
        {
            return nullptr;
        }

        DBEXEC(verbose, TraceAssertion("propagating", *assertion);)

        fromType = varActualType(fromType);
        toType   = varActualType(toType);

        if (fromType != toType)
        {
            // LONG/INT cannot be removed but we can remove overflow checking.
            // We can also change sign extending casts to zero extending casts, on X86/64 they
            // are preferable due to having smaller encoding and sometimes better performance.
            cast->SetCastType(toType);
            cast->gtFlags &= ~(GTF_OVERFLOW | GTF_UNSIGNED);

#ifdef TARGET_AMD64
            // TODO-MIKE-CQ: For now do this only on x64. It's also useful on 32 bit
            // targets but it sometimes interferes with LMUL helper call elimination.
            // On ARM64 it seems to be useless and it interferes with smull generation.

            if (toType == TYP_LONG)
            {
                cast->gtFlags |= GTF_UNSIGNED;
            }
#endif

            op1 = cast;
        }

        return UpdateTree(op1, cast, stmt);
    }

    const AssertionDsc* FindPositiveIntAssertion(const ASSERT_TP assertions, ValueNum vn)
    {
        for (BitVecOps::Enumerator en(&countTraits, assertions); en.MoveNext();)
        {
            const AssertionDsc& assertion = GetAssertion(GetAssertionIndex(en.Current()));

            if ((assertion.op1.vn == vn) && (((assertion.kind == OAK_RANGE) && (assertion.op2.range.min >= 0)) ||
                                             (assertion.kind == OAK_BOUNDS_CHK)))
            {
                return &assertion;
            }
        }

        return nullptr;
    }

    GenTree* PropagateSignedDivision(const ASSERT_TP assertions, GenTreeOp* div, Statement* stmt)
    {
        assert(div->OperIs(GT_DIV, GT_MOD) && div->TypeIs(TYP_INT));

        ValueNum divisorVN = vnStore->VNNormalValue(div->GetOp(1)->GetConservativeVN());

        if (!vnStore->IsVNInt32Constant(divisorVN))
        {
            return nullptr;
        }

        ssize_t divisor = vnStore->ConstantValue<int32_t>(divisorVN);

        if (divisor < 2)
        {
            return nullptr;
        }

        ValueNum dividendVN = vnStore->VNNormalValue(div->GetOp(0)->GetConservativeVN());

        const AssertionDsc* assertion = FindPositiveIntAssertion(assertions, dividendVN);

        if (assertion == nullptr)
        {
            return nullptr;
        }

        DBEXEC(verbose, TraceAssertion("propagating", *assertion);)

        div->SetOper(div->OperIs(GT_DIV) ? GT_UDIV : GT_UMOD, GenTree::PRESERVE_VN);
        div->CheckDivideByConstOptimized(compiler);

        return nullptr;
    }

    GenTree* PropagateComma(GenTreeOp* comma, Statement* stmt)
    {
        // Remove the bounds check as part of the COMMA node since we need the user to remove nodes.
        if (comma->GetOp(0)->OperIs(GT_BOUNDS_CHECK) && ((comma->GetOp(0)->gtFlags & GTF_BOUND_VALID) != 0))
        {
            compiler->optRemoveRangeCheck(comma->GetOp(0)->AsBoundsChk(), comma, stmt);
            return UpdateTree(comma, comma, stmt);
        }

        return nullptr;
    }

    GenTree* PropagateIndir(const ASSERT_TP assertions, GenTreeIndir* indir, Statement* stmt)
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

        if (!addr->OperIs(GT_LCL_VAR, GT_LCL_USE))
        {
            return nullptr;
        }

        const AssertionDsc* assertion = FindNotNullAssertion(assertions, addr->GetConservativeVN());

        if (assertion == nullptr)
        {
            return nullptr;
        }

        DBEXEC(verbose, TraceAssertion("propagating", *assertion);)

        indir->gtFlags &= ~GTF_EXCEPT;
        indir->gtFlags |= GTF_IND_NONFAULTING;
        // Set this flag to prevent reordering
        indir->gtFlags |= GTF_ORDER_SIDEEFF;

        return UpdateTree(indir, indir, stmt);
    }

    const AssertionDsc* FindNotNullAssertion(const ASSERT_TP assertions, ValueNum vn)
    {
        vn = vnStore->VNNormalValue(vn);

        ValueNum baseVN = vn;

        for (VNFuncApp funcApp; vnStore->GetVNFunc(baseVN, &funcApp) && funcApp.Is(GT_ADD);)
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

        for (BitVecOps::Enumerator en(&countTraits, assertions); en.MoveNext();)
        {
            const AssertionDsc& assertion = GetAssertion(GetAssertionIndex(en.Current()));

            if (assertion.kind != OAK_NOT_EQUAL)
            {
                continue;
            }

            if (assertion.op2.vn != ValueNumStore::VNForNull())
            {
                continue;
            }

            if ((assertion.op1.vn != vn) && (assertion.op1.vn != baseVN))
            {
                continue;
            }

            return &assertion;
        }

        return nullptr;
    }

    GenTree* PropagateCallNotNull(const ASSERT_TP assertions, GenTreeCall* call)
    {
        if ((call->gtFlags & GTF_CALL_NULLCHECK) == 0)
        {
            return nullptr;
        }

        GenTree* thisArg = call->GetThisArg();
        noway_assert(thisArg != nullptr);

        if (!thisArg->OperIs(GT_LCL_VAR, GT_LCL_USE))
        {
            return nullptr;
        }

        const AssertionDsc* assertion = FindNotNullAssertion(assertions, thisArg->GetConservativeVN());

        if (assertion == nullptr)
        {
            return nullptr;
        }

        DBEXEC(verbose, TraceAssertion("propagating", *assertion);)

        call->gtFlags &= ~GTF_CALL_NULLCHECK;
        call->gtFlags &= ~GTF_EXCEPT;
        noway_assert(call->gtFlags & GTF_SIDE_EFFECT);
        return call;
    }

    const AssertionDsc* FindInstanceOfAssertion(const ASSERT_TP assertions, ValueNum objVN, ValueNum mtVN)
    {
        ValueNum objMTVN = vnStore->HasFunc(TYP_I_IMPL, VNF_ObjMT, objVN);

        for (BitVecOps::Enumerator en(&countTraits, assertions); en.MoveNext();)
        {
            const AssertionDsc& assertion = GetAssertion(GetAssertionIndex(en.Current()));

            if ((assertion.kind == OAK_EQUAL) && (assertion.op2.vn == mtVN) &&
                (((assertion.op1.kind == O1K_INSTANCE_OF) && (assertion.op1.vn == objVN)) ||
                 ((assertion.op1.kind == O1K_VALUE_NUMBER) && (assertion.op1.vn == objMTVN))))
            {
                return &assertion;
            }
        }

        return nullptr;
    }

    GenTree* PropagateCall(const ASSERT_TP assertions, GenTreeCall* call, Statement* stmt)
    {
        if (PropagateCallNotNull(assertions, call))
        {
            return UpdateTree(call, call, stmt);
        }

        if (compiler->opts.IsReadyToRun() || !call->IsHelperCall())
        {
            return nullptr;
        }

        CorInfoHelpFunc helper = Compiler::eeGetHelperNum(call->GetMethodHandle());

        if ((helper != CORINFO_HELP_ISINSTANCEOFINTERFACE) && (helper != CORINFO_HELP_ISINSTANCEOFARRAY) &&
            (helper != CORINFO_HELP_ISINSTANCEOFCLASS) && (helper != CORINFO_HELP_ISINSTANCEOFANY) &&
            (helper != CORINFO_HELP_CHKCASTINTERFACE) && (helper != CORINFO_HELP_CHKCASTARRAY) &&
            (helper != CORINFO_HELP_CHKCASTCLASS) && (helper != CORINFO_HELP_CHKCASTANY) &&
            (helper != CORINFO_HELP_CHKCASTCLASS_SPECIAL))
        {
            return nullptr;
        }

        GenTree* objectArg = call->GetArgNodeByArgNum(1);

        if (!objectArg->OperIs(GT_LCL_VAR, GT_LCL_USE))
        {
            return nullptr;
        }

        ValueNum objectVN      = objectArg->GetConservativeVN();
        ValueNum methodTableVN = vnStore->VNNormalValue(call->GetArgNodeByArgNum(0)->GetConservativeVN());

        const AssertionDsc* assertion = FindInstanceOfAssertion(assertions, objectVN, methodTableVN);

        if (assertion == nullptr)
        {
            return nullptr;
        }

        DBEXEC(verbose, TraceAssertion("propagating", *assertion);)

        GenTree* sideEffects = compiler->gtExtractSideEffList(call, GTF_SIDE_EFFECT, true);

        if (sideEffects != nullptr)
        {
            objectArg = compiler->gtNewCommaNode(sideEffects, objectArg);
            compiler->fgSetTreeSeq(objectArg);
        }

        return UpdateTree(objectArg, call, stmt);
    }

    GenTree* PropagateBoundsChk(const ASSERT_TP assertions, GenTreeBoundsChk* boundsChk, Statement* stmt)
    {
#ifdef FEATURE_ENABLE_NO_RANGE_CHECKS
        if (JitConfig.JitNoRangeChks())
        {
            boundsChk->gtFlags |= GTF_BOUND_VALID;
            return nullptr;
        }
#endif

        ValueNum indexVN      = vnStore->VNNormalValue(boundsChk->GetIndex()->GetConservativeVN());
        ValueNum lengthVN     = vnStore->VNNormalValue(boundsChk->GetLength()->GetConservativeVN());
        ssize_t  indexVal     = vnStore->IsVNInt32Constant(indexVN) ? vnStore->ConstantValue<int>(indexVN) : -1;
        ssize_t  lengthVal    = vnStore->IsVNInt32Constant(lengthVN) ? vnStore->ConstantValue<int>(lengthVN) : -1;
        ssize_t  indexMinVal  = INT32_MIN;
        ssize_t  indexMaxVal  = INT32_MAX;
        ValueNum indexMaxVN   = NoVN;
        ssize_t  lengthMinVal = 0;

        bool isRedundant = false;
        INDEBUG(const char* comment = nullptr);

        if ((indexVal >= 0) && (lengthVal >= 0))
        {
            isRedundant = indexVal < lengthVal;
            INDEBUG(comment = isRedundant ? "a[K1] with a.Length == K2 && K1 < K2" : "");
        }

        for (BitVecOps::Enumerator en(&countTraits, assertions); en.MoveNext() && !isRedundant;)
        {
            const AssertionDsc& assertion = GetAssertion(GetAssertionIndex(en.Current()));
            const auto          kind      = assertion.kind;
            const auto&         op1       = assertion.op1;
            const auto&         op2       = assertion.op2;

            if (kind == OAK_BOUNDS_CHK)
            {
                if (op2.vn == lengthVN)
                {
                    if (op1.vn == indexVN)
                    {
                        isRedundant = true;
                        INDEBUG(comment = "a[i] followed by a[i]");
                    }
                    else if (indexVal == 0)
                    {
                        isRedundant = true;
                        INDEBUG(comment = "a[*] followed by a[0]");
                    }
                }
                else if (op1.vn == indexVN)
                {
                    if (indexMinVal < 0)
                    {
                        indexMinVal = 0;
                    }
                }
            }
            else if ((op1.vn == lengthVN) && (op2.kind == O2K_CONST_INT))
            {
                if (kind == OAK_NOT_EQUAL)
                {
                    if ((indexVal == 0) && (op2.intCon.value == 0))
                    {
                        isRedundant = true;
                        INDEBUG(comment = "a[0] with a.Length != 0");
                    }
                }
                else if (kind == OAK_EQUAL)
                {
                    if ((indexVal >= 0) && (indexVal < op2.intCon.value))
                    {
                        isRedundant = true;
                        INDEBUG(comment = "a[K1] with a.Length == K2 && K1 < K2");
                    }
                }
            }
            else if (kind == OAK_RANGE)
            {
                if (op1.vn == lengthVN)
                {
                    // There may be multiple ranges for the same VN e.g. if (i > -10) { if (i > 5) { ... } }.
                    lengthMinVal = Max(lengthMinVal, op2.range.min);

                    if (indexVal >= 0)
                    {
                        isRedundant = indexVal < lengthMinVal;
                        INDEBUG(comment = "a[K1] with a.Length > K2 && K1 <= K2");
                    }
                }
                else if (op1.vn == indexVN)
                {
                    indexMinVal = Max(indexMinVal, op2.range.min);
                    indexMaxVal = Min(indexMaxVal, op2.range.max);

                    if (lengthVal > 0)
                    {
                        isRedundant = (0 <= indexMinVal) && (indexMaxVal < lengthVal);
                        INDEBUG(comment = "a[i] with a.Length == K && 0 <= i && i < K");
                    }
                }
            }
            else if (op1.kind == O1K_BOUND_LOOP_BND)
            {
                assert((op2.kind == O2K_CONST_INT) && (op2.intCon.value == 0));

                VNFuncApp funcApp;
                vnStore->GetVNFunc(op1.vn, &funcApp);
                assert(ValueNumStore::IsVNCompareCheckedBoundRelop(funcApp));
                genTreeOps oper = static_cast<genTreeOps>(funcApp.m_func);

                if (funcApp[0] == lengthVN)
                {
                    std::swap(funcApp.m_args[0], funcApp.m_args[1]);
                    oper = GenTree::SwapRelop(oper);
                }
                else if (funcApp[1] != lengthVN)
                {
                    continue;
                }

                if (kind == OAK_EQUAL)
                {
                    oper = GenTree::ReverseRelop(oper);
                }

                // TODO-MIKE-Cleanup: We get "len > C" as O1K_BOUND_LOOP_BND but it really should
                // be a range assertion. Problem is, RangeCheck depends on O1K_BOUND_LOOP_BND and
                // it is a pile of crap that needs serious work.

                if (vnStore->IsVNInt32Constant(funcApp[0]))
                {
                    ssize_t constVal = vnStore->ConstantValue<int>(funcApp[0]);

                    if ((oper == GT_LT) && (constVal != INT32_MAX))
                    {
                        constVal++;
                    }
                    else if (oper != GT_LE)
                    {
                        continue;
                    }

                    lengthMinVal = Max(lengthMinVal, constVal);

                    if ((0 <= indexVal) && (indexVal < lengthMinVal))
                    {
                        isRedundant = true;
                        INDEBUG(comment = "a[K1] with a.Length >= K2 && K1 < K2");
                    }

                    continue;
                }

                if ((funcApp[0] == indexVN) && (oper == GT_LT))
                {
                    indexMaxVN = lengthVN;
                }
            }

            // Extend this to remove additional redundant bounds checks:
            // i.e.  a[i+1] followed by a[i]  by using the VN(i+1) >= VN(i)
            //       a[i]   followed by a[j]  when j is known to be >= i
            //       a[i]   followed by a[5]  when i is known to be >= 5

            if (isRedundant)
            {
                DBEXEC(verbose, TraceAssertion("propagating", assertion, comment);)
                break;
            }
        }

        if ((indexMinVal >= 0) && ((indexMaxVN == lengthVN) || (indexMaxVal < lengthMinVal)))
        {
            isRedundant = true;
        }

        if (isRedundant)
        {
            if (boundsChk == stmt->GetRootNode())
            {
                compiler->optRemoveRangeCheck(boundsChk, nullptr, stmt);
                return UpdateTree(stmt->GetRootNode(), boundsChk, stmt);
            }

            boundsChk->gtFlags |= GTF_BOUND_VALID;
        }

        return nullptr;
    }

    GenTree* UpdateTree(GenTree* newTree, GenTree* tree, Statement* stmt)
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
                Compiler::FindLinkData use = compiler->gtFindLink(stmt, tree);
                noway_assert((use.useEdge != nullptr) && (use.user != nullptr));
                use.user->ReplaceOperand(use.useEdge, newTree);
            }

            // We need to ensure that the gtNext field is set as it is used to traverse
            // the statement. Later we'll re-morph and sequence the statement, so that
            // gtPrev gets updated as well.
            newTree->gtNext = tree->gtNext;

            DEBUG_DESTROY_NODE(tree);
        }

        stmtMorphPending = true;

        return newTree;
    }

    GenTree* PropagateNode(const ASSERT_TP assertions, GenTree* node, Statement* stmt, BasicBlock* block)
    {
        INDEBUG(currentNode = node);

        switch (node->GetOper())
        {
            case GT_LCL_VAR:
                if ((node->gtFlags & (GTF_VAR_DEF | GTF_DONT_CSE)) != 0)
                {
                    return nullptr;
                }
                return PropagateLclVarUse(assertions, node->AsLclVar(), stmt);
            case GT_LCL_USE:
                if ((node->gtFlags & GTF_DONT_CSE) != 0)
                {
                    return nullptr;
                }
                return PropagateSsaUse(assertions, node->AsLclUse(), stmt);
            case GT_OBJ:
            case GT_BLK:
            case GT_IND:
            case GT_NULLCHECK:
                return PropagateIndir(assertions, node->AsIndir(), stmt);
            case GT_BOUNDS_CHECK:
                return PropagateBoundsChk(assertions, node->AsBoundsChk(), stmt);
            case GT_COMMA:
                return PropagateComma(node->AsOp(), stmt);
            case GT_CAST:
                return PropagateCast(assertions, node->AsCast(), stmt);
            case GT_CALL:
                return PropagateCall(assertions, node->AsCall(), stmt);
            case GT_EQ:
            case GT_NE:
            case GT_LT:
            case GT_LE:
            case GT_GT:
            case GT_GE:
                return PropagateRelop(assertions, node->AsOp(), stmt);
            case GT_JTRUE:
                return PropagateJTrue(block, node->AsUnOp());
            case GT_DIV:
            case GT_MOD:
                return node->TypeIs(TYP_INT) ? PropagateSignedDivision(assertions, node->AsOp(), stmt) : nullptr;
            default:
                return nullptr;
        }
    }

    void AddImpliedAssertions(AssertionIndex index, ASSERT_TP& assertions)
    {
        const AssertionDsc& assertion = GetAssertion(index);

        if (((assertion.kind == OAK_EQUAL) && (assertion.op1.kind == O1K_LCLVAR) &&
             (assertion.op2.kind == O2K_CONST_INT)) ||
            (assertion.kind == OAK_RANGE) || (assertion.kind == OAK_BOUNDS_CHK))
        {
            AddRangeImpliedAssertions(assertion, assertions);
            return;
        }

        if ((assertion.kind == OAK_EQUAL) && (assertion.op1.kind == O1K_INSTANCE_OF))
        {
            AddTypeImpliedNotNullAssertions(assertion, assertions);
            return;
        }

        // TODO-MIKE-CQ: Try restoring equality implied assertions (e.g a == b && a == 2 implies b == 2).
        // Old code did it poorly and had practically no effect - removing it resulted in one regression
        // in System.Security.AccessControl CommonAcl:RemoveQualifiedAces.
    }

    void AddTypeImpliedNotNullAssertions(const AssertionDsc& typeAssertion, ASSERT_TP& assertions)
    {
        assert((typeAssertion.kind == OAK_EQUAL) && (typeAssertion.op1.kind == O1K_INSTANCE_OF));

        const ASSERT_TP vnAssertions = GetVNAssertions(typeAssertion.op1.vn);

        if (vnAssertions == BitVecOps::UninitVal())
        {
            return;
        }

        for (BitVecOps::Enumerator en(&sizeTraits, vnAssertions); en.MoveNext();)
        {
            const AssertionDsc& notNullAssertion = GetAssertion(GetAssertionIndex(en.Current()));
            assert(notNullAssertion.op1.vn == typeAssertion.op1.vn);

            if (&notNullAssertion == &typeAssertion)
            {
                continue;
            }

            if ((notNullAssertion.kind == OAK_NOT_EQUAL) &&
                ((notNullAssertion.op1.kind == O1K_LCLVAR) || (notNullAssertion.op1.kind == O1K_VALUE_NUMBER)) &&
                (notNullAssertion.op2.kind == O2K_CONST_INT))
            {
                assert(notNullAssertion.op2.intCon.value == 0);

                if (BitVecOps::TryAddElemD(&countTraits, assertions, en.Current()))
                {
                    JITDUMP("Assertion A%02d implies A%02d\n", &typeAssertion - assertionTable,
                            &notNullAssertion - assertionTable);
                }

                // There is at most one not null assertion that is implied by a type assertion.
                break;
            }
        }
    }

    void AddRangeImpliedAssertions(const AssertionDsc& rangeAssertion, ASSERT_TP& result)
    {
        assert(((rangeAssertion.kind == OAK_EQUAL) && (rangeAssertion.op2.kind == O2K_CONST_INT)) ||
               (rangeAssertion.kind == OAK_RANGE) || (rangeAssertion.kind == OAK_BOUNDS_CHK));

        // TODO-MIKE-Throughput: It should be possible to eliminate the repeated linear
        // searches needed for assertion implication by chaining implied assertions into
        // linked list (probably this could be done when creating assertions, when we
        // already need a linear search for duplicates).
        // x EQ 42 implies x IN [41..44] implies x IN [0..100] etc.

        const ASSERT_TP vnAssertions = GetVNAssertions(rangeAssertion.op1.vn);

        if (vnAssertions == BitVecOps::UninitVal())
        {
            return;
        }

        ssize_t min;
        ssize_t max;

        if (rangeAssertion.kind == OAK_RANGE)
        {
            min = rangeAssertion.op2.range.min;
            max = rangeAssertion.op2.range.max;
        }
        else if (rangeAssertion.kind == OAK_BOUNDS_CHK)
        {
            min = 0;
            max = INT32_MAX;
        }
        else
        {
            min = rangeAssertion.op2.intCon.value;
            max = min;
        }

        for (BitVecOps::Enumerator en(&sizeTraits, vnAssertions); en.MoveNext();)
        {
            const AssertionDsc& impliedAssertion = GetAssertion(GetAssertionIndex(en.Current()));
            assert(impliedAssertion.op1.vn == rangeAssertion.op1.vn);

            if (&impliedAssertion == &rangeAssertion)
            {
                continue;
            }

            bool isImplied = false;

            if (impliedAssertion.kind == OAK_RANGE)
            {
                if ((impliedAssertion.op2.range.min <= min) && (max <= impliedAssertion.op2.range.max))
                {
                    isImplied = true;
                }
            }
            else if ((impliedAssertion.kind == OAK_NOT_EQUAL) && (impliedAssertion.op2.kind == O2K_CONST_INT))
            {
                if ((impliedAssertion.op2.intCon.value < min) || (max < impliedAssertion.op2.intCon.value))
                {
                    isImplied = true;
                }
            }

            if (isImplied && BitVecOps::TryAddElemD(&countTraits, result, en.Current()))
            {
                JITDUMP("Assertion A%02d implies assertion A%02d\n", &rangeAssertion - assertionTable,
                        &impliedAssertion - assertionTable);
            }
        }
    }

    struct AssertionGen
    {
        ASSERT_TP next;
        ASSERT_TP jump;
    };

    class DataFlowCallback
    {
        AssertionProp&      ap;
        BitVecTraits*       apTraits;
        const AssertionGen* assertionGen;
        ASSERT_TP           preMergeOut;
        ASSERT_TP           preMergeJumpDestOut;

    public:
        DataFlowCallback(AssertionProp& ap, const AssertionGen* assertionGen)
            : ap(ap)
            , apTraits(&ap.countTraits)
            , assertionGen(assertionGen)
            , preMergeOut(BitVecOps::UninitVal())
            , preMergeJumpDestOut(BitVecOps::UninitVal())
        {
        }

        // At the start of the merge function of the dataflow equations, initialize premerge state (to detect change.)
        void StartMerge(BasicBlock* block)
        {
#ifdef DEBUG
            if (VerboseDataflow())
            {
                printf("StartMerge: " FMT_BB " ", block->bbNum);
                ap.DumpAssertionIndices("in -> ", block->bbAssertionIn, "\n");
            }
#endif

            BitVecOps::Assign(apTraits, preMergeOut, block->bbAssertionOut);
            BitVecOps::Assign(apTraits, preMergeJumpDestOut, block->bbAssertionOutJumpDest);
        }

        // During merge, perform the actual merging of the predecessor's (since this is a forward analysis) dataflow
        // flags.
        void Merge(BasicBlock* block, BasicBlock* predBlock, unsigned dupCount)
        {
            ASSERT_TP assertionOut;

            if ((predBlock->bbJumpKind == BBJ_COND) && (predBlock->bbJumpDest == block))
            {
                assertionOut = predBlock->bbAssertionOutJumpDest;

                if (dupCount > 1)
                {
                    // Scenario where next block and conditional block, both point to the same block.
                    // In such case, intersect the assertions present on both the out edges of predBlock.
                    assert(predBlock->bbNext == block);
                    BitVecOps::IntersectionD(apTraits, assertionOut, predBlock->bbAssertionOut);

#ifdef DEBUG
                    if (VerboseDataflow())
                    {
                        printf("Merge     : Duplicate flow, " FMT_BB " ", block->bbNum);
                        ap.DumpAssertionIndices("in -> ", block->bbAssertionIn, "; ");
                        printf("pred " FMT_BB " ", predBlock->bbNum);
                        ap.DumpAssertionIndices("out1 -> ", predBlock->bbAssertionOutJumpDest, "; ");
                        ap.DumpAssertionIndices("out2 -> ", predBlock->bbAssertionOut, "\n");
                    }
#endif
                }
            }
            else
            {
                assertionOut = predBlock->bbAssertionOut;
            }

#ifdef DEBUG
            if (VerboseDataflow())
            {
                printf("Merge     : " FMT_BB " ", block->bbNum);
                ap.DumpAssertionIndices("in -> ", block->bbAssertionIn, "; ");
                printf("pred " FMT_BB " ", predBlock->bbNum);
                ap.DumpAssertionIndices("out -> ", assertionOut, "\n");
            }
#endif

            BitVecOps::IntersectionD(apTraits, block->bbAssertionIn, assertionOut);
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
                ap.DumpAssertionIndices("in -> ", block->bbAssertionIn, "; ");
                printf("firstTryBlock " FMT_BB " ", firstTryBlock->bbNum);
                ap.DumpAssertionIndices("in -> ", firstTryBlock->bbAssertionIn, "; ");
                printf("lastTryBlock " FMT_BB " ", lastTryBlock->bbNum);
                ap.DumpAssertionIndices("out -> ", lastTryBlock->bbAssertionOut, "\n");
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
                ap.DumpAssertionIndices("in -> ", block->bbAssertionIn, "\n\n");
            }
#endif

            const AssertionGen& gen = assertionGen[block->bbNum];
            BitVecOps::DataFlowD(apTraits, block->bbAssertionOut, gen.next, block->bbAssertionIn);
            BitVecOps::DataFlowD(apTraits, block->bbAssertionOutJumpDest, gen.jump, block->bbAssertionIn);

            bool changed = !BitVecOps::Equal(apTraits, preMergeOut, block->bbAssertionOut) ||
                           !BitVecOps::Equal(apTraits, preMergeJumpDestOut, block->bbAssertionOutJumpDest);

#ifdef DEBUG
            if (VerboseDataflow())
            {
                if (changed)
                {
                    printf("Changed   : " FMT_BB " ", block->bbNum);
                    ap.DumpAssertionIndices("before out -> ", preMergeOut, "; ");
                    ap.DumpAssertionIndices("after out -> ", block->bbAssertionOut, ";\n        ");
                    ap.DumpAssertionIndices("jumpDest before out -> ", preMergeJumpDestOut, "; ");
                    ap.DumpAssertionIndices("jumpDest after out -> ", block->bbAssertionOutJumpDest, ";\n\n");
                }
                else
                {
                    printf("Unchanged : " FMT_BB " ", block->bbNum);
                    ap.DumpAssertionIndices("out -> ", block->bbAssertionOut, "; ");
                    ap.DumpAssertionIndices("jumpDest out -> ", block->bbAssertionOutJumpDest, "\n\n");
                }
            }
#endif

            return changed;
        }

        // Can be enabled to get detailed debug output about dataflow for assertions.
        bool VerboseDataflow()
        {
#if 0
        return ap.verbose;
#endif
            return false;
        }
    };

    const AssertionGen* ComputeBlockAssertionGen()
    {
        AssertionGen* generated = compiler->fgAllocateTypeForEachBlk<AssertionGen>(CMK_AssertionProp);

        for (BasicBlock* const block : compiler->Blocks())
        {
            ASSERT_TP assertions = BitVecOps::MakeEmpty(&countTraits);
            GenTree*  jtrue      = nullptr;

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
                        AddImpliedAssertions(info.GetAssertionIndex(), assertions);
                        BitVecOps::AddElemD(&countTraits, assertions, info.GetAssertionIndex() - 1);
                    }
                }
            }

            ASSERT_TP jumpDestAssertions;

            if (jtrue == nullptr)
            {
                jumpDestAssertions = BitVecOps::MakeEmpty(&countTraits);
            }
            else
            {
                jumpDestAssertions = BitVecOps::MakeCopy(&countTraits, assertions);

                if (jtrue->GeneratesAssertion())
                {
                    AssertionInfo  info = jtrue->GetAssertionInfo();
                    AssertionIndex nextIndex;
                    AssertionIndex jumpIndex;

                    if (info.IsNextEdgeAssertion())
                    {
                        nextIndex = info.GetAssertionIndex();
                        jumpIndex = GetInvertedAssertion(nextIndex);
                    }
                    else
                    {
                        jumpIndex = info.GetAssertionIndex();
                        nextIndex = GetInvertedAssertion(jumpIndex);
                    }

                    if (nextIndex != NO_ASSERTION_INDEX)
                    {
                        AddImpliedAssertions(nextIndex, assertions);
                        BitVecOps::AddElemD(&countTraits, assertions, nextIndex - 1);
                    }

                    if (jumpIndex != NO_ASSERTION_INDEX)
                    {
                        AddImpliedAssertions(jumpIndex, jumpDestAssertions);
                        BitVecOps::AddElemD(&countTraits, jumpDestAssertions, jumpIndex - 1);
                    }
                }
            }

            generated[block->bbNum].next = assertions;
            generated[block->bbNum].jump = jumpDestAssertions;

#ifdef DEBUG
            if (verbose)
            {
                if (block == compiler->fgFirstBB)
                {
                    printf("\n");
                }

                printf(FMT_BB " gen = ", block->bbNum);
                DumpAssertionIndices("", assertions, "");

                if (jtrue != nullptr)
                {
                    printf(", branch to " FMT_BB " gen = ", block->bbJumpDest->bbNum);
                    DumpAssertionIndices("", jumpDestAssertions, "");
                }

                printf("\n");

                if (block == compiler->fgLastBB)
                {
                    printf("\n");
                }
            }
#endif
        }

        return generated;
    }

    void InitAssertionDataflowSets()
    {
        for (BasicBlock* const block : compiler->Blocks())
        {
            block->bbAssertionIn          = BitVecOps::MakeFull(&countTraits);
            block->bbAssertionOut         = BitVecOps::MakeFull(&countTraits);
            block->bbAssertionOutJumpDest = BitVecOps::MakeFull(&countTraits);
        }

        BitVecOps::ClearD(&countTraits, compiler->fgFirstBB->bbAssertionIn);
    }

    GenTree* ExtractConstantSideEffects(GenTree* tree)
    {
        assert(vnStore->IsVNConstant(vnStore->VNNormalValue(tree->GetConservativeVN())));

        if ((tree->gtFlags & GTF_SIDE_EFFECT) == 0)
        {
            return nullptr;
        }

        // Do a sanity check to ensure persistent side effects aren't discarded and
        // tell gtExtractSideEffList to ignore the root of the tree.
        assert(!compiler->gtNodeHasSideEffects(tree, GTF_PERSISTENT_SIDE_EFFECTS));

        // Exception side effects may be ignored because the root is known to be a constant
        // (e.g. VN may evaluate a DIV/MOD node to a constant and the node may still
        // have GTF_EXCEPT set, even if it does not actually throw any exceptions).
        bool ignoreRoot = true;

        GenTree* sideEffects = compiler->gtExtractSideEffList(tree, GTF_SIDE_EFFECT, ignoreRoot);
        JITDUMPTREE(sideEffects, "Extracted side effects from constant tree [%06u]:\n", tree->GetID());
        return sideEffects;
    }

    GenTree* PropagateJTrue(BasicBlock* block, GenTreeUnOp* jtrue)
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

        GenTree* sideEffects = ExtractConstantSideEffects(relop);

        // Transform the relop into EQ|NE(0, 0)
        ValueNum vnZero = vnStore->VNForIntCon(0);
        GenTree* op1    = compiler->gtNewIconNode(0);
        op1->SetVNs(ValueNumPair(vnZero, vnZero));
        relop->AsOp()->SetOp(0, op1);
        GenTree* op2 = compiler->gtNewIconNode(0);
        op2->SetVNs(ValueNumPair(vnZero, vnZero));
        relop->AsOp()->SetOp(1, op2);
        relop->SetOper(vnStore->CoercedConstantValue<int64_t>(relopVN) != 0 ? GT_EQ : GT_NE);
        ValueNum vnLib = vnStore->VNNormalValue(relop->GetLiberalVN());
        relop->SetVNs(ValueNumPair(vnLib, relopVN));

        while (sideEffects != nullptr)
        {
            Statement* newStmt;

            if (sideEffects->OperIs(GT_COMMA))
            {
                newStmt     = compiler->fgNewStmtNearEnd(block, sideEffects->AsOp()->GetOp(0));
                sideEffects = sideEffects->AsOp()->GetOp(1);
            }
            else
            {
                newStmt     = compiler->fgNewStmtNearEnd(block, sideEffects);
                sideEffects = nullptr;
            }

            // fgMorphBlockStmt could potentially affect stmts after the current one,
            // for example when it decides to fgRemoveRestOfBlock.

            // TODO-MIKE-Review: Do we really need to remorph? Seems like simply
            // fgSetStmtSeq should suffice here. Also, this morphs trees before
            // doing constant propagation so we may morph again if they contains
            // constants.

            compiler->fgMorphBlockStmt(block, newStmt DEBUGARG(__FUNCTION__));
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
                if (!addr->OperIs(GT_LCL_ADDR))
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

                if ((user != nullptr) &&
                    ((user->OperIs(GT_ASG) && (user->AsOp()->GetOp(1) == tree)) || user->IsInsert()) &&
                    ((tree->gtFlags & GTF_SIDE_EFFECT) == 0) &&
                    (m_vnStore->VNNormalValue(tree->GetConservativeVN()) == m_vnStore->VNForZeroMap()))
                {
                    if (user->OperIs(GT_ASG))
                    {
                        user->AsOp()->SetOp(1, m_compiler->gtNewIconNode(0));
                    }
                    else if (user->AsInsert()->GetStructValue() == tree)
                    {
                        user->AsInsert()->SetStructValue(m_compiler->gtNewIconNode(0));
                    }
                    else
                    {
                        assert(user->AsInsert()->GetFieldValue() == tree);
                        user->AsInsert()->SetFieldValue(m_compiler->gtNewIconNode(0));
                    }

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
                    if (m_compiler->lvaGetDesc(tree->AsLclVar())->lvIsCSE)
                    {
                        return Compiler::WALK_CONTINUE;
                    }

                    break;

                case GT_LCL_USE:
                    // Don't undo constant CSEs.
                    if (m_compiler->lvaGetDesc(tree->AsLclUse()->GetDef()->GetLclNum())->lvIsCSE)
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
                case GT_FADD:
                case GT_FSUB:
                case GT_FMUL:
                case GT_FDIV:
                case GT_FNEG:
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

                // TODO-MIKE-CQ: This doesn't handle some helper calls that can be evaluated to
                // constants, like CORINFO_HELP_FLTREM and CORINFO_HELP_DBLREM.

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

                if (m_vnStore->GetVNFunc(tree->gtVNPair.GetConservative(), &lclAddr) &&
                    (lclAddr.m_func == VNF_LclAddr) && ChangeToLocalAddress(tree, lclAddr))
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

            return m_compiler->gtExtractSideEffList(tree, GTF_SIDE_EFFECT, /* ignoreRoot */ true);
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

            if ((offset > UINT16_MAX) || (offset >= m_compiler->lvaGetDesc(lclNum)->GetTypeSize()))
            {
                return false;
            }

            if (GenTreeLclUse* use = node->IsLclUse())
            {
                use->GetDef()->RemoveUse(use);
            }

            if ((offset == 0) && (fieldSeq == nullptr))
            {
                node->ChangeToLclAddr(TYP_I_IMPL, lclNum);
            }
            else
            {
                node->ChangeToLclAddr(TYP_I_IMPL, lclNum, static_cast<unsigned>(offset),
                                      fieldSeq == nullptr ? FieldSeqNode::NotAField() : fieldSeq);
            }

            m_stmtMorphPending = true;

            return true;
        }
    };

    void GenerateAssertions()
    {
        for (BasicBlock* const block : compiler->Blocks())
        {
            INDEBUG(currentBlock = block);

            for (Statement* stmt = block->GetFirstStatement(); stmt != nullptr; stmt = stmt->GetNextStmt())
            {
                VNConstPropVisitor visitor(compiler);
                stmt = visitor.VisitStmt(block, stmt);

                if (stmt == nullptr)
                {
                    break;
                }

                for (GenTree* const node : stmt->Nodes())
                {
                    GenerateNodeAssertions(node);
                }
            }
        }

        INDEBUG(currentBlock = nullptr);
    }

    void ComputeAvailability()
    {
        countTraits = BitVecTraits(assertionCount, compiler);

        const AssertionGen* assertionGen = ComputeBlockAssertionGen();
        InitAssertionDataflowSets();
        ForwardDataFlow(DataFlowCallback(*this, assertionGen), compiler);

#ifdef DEBUG
        if (verbose)
        {
            for (BasicBlock* const block : compiler->Blocks())
            {
                printf(FMT_BB ":\n in = ", block->bbNum);

                DumpAssertionIndices("", block->bbAssertionIn, "\n");

                if (block->bbJumpKind != BBJ_COND)
                {
                    DumpAssertionIndices(" out = ", block->bbAssertionOut, "\n");
                }
                else
                {
                    printf(" out(" FMT_BB ") = ", block->bbNext->bbNum);
                    DumpAssertionIndices("", block->bbAssertionOut, "\n");
                    printf(" out(" FMT_BB ") = ", block->bbJumpDest->bbNum);
                    DumpAssertionIndices("", block->bbAssertionOutJumpDest, "\n");
                }
            }
            printf("\n");
        }
#endif // DEBUG
    }

    void PropagateAssertions()
    {
        ASSERT_TP assertions = BitVecOps::MakeEmpty(&countTraits);

        for (BasicBlock* const block : compiler->Blocks())
        {
            BitVecOps::Assign(&countTraits, assertions, block->bbAssertionIn);

            // TODO-Review: EH successor/predecessor iteration seems broken.
            // SELF_HOST_TESTS_ARM\jit\Directed\ExcepFilters\fault\fault.exe
            if (block->bbCatchTyp == BBCT_FAULT)
            {
                continue;
            }

            compiler->fgRemoveRestOfBlock = false;
            INDEBUG(currentBlock = block);

            for (Statement* stmt = block->FirstNonPhiDef(); stmt != nullptr;)
            {
                if (compiler->fgRemoveRestOfBlock)
                {
                    compiler->fgRemoveStmt(block, stmt);
                    stmt = stmt->GetNextStmt();

                    continue;
                }

                // Preserve the prev link before the propagation and morph, to check if propagation
                // removes the current stmt.
                Statement* prevStmt = (stmt == block->firstStmt()) ? nullptr : stmt->GetPrevStmt();

                stmtMorphPending = false;

                for (GenTree* node = stmt->GetNodeList(); node != nullptr; node = node->gtNext)
                {
                    GenTree* newNode = PropagateNode(assertions, node, stmt, block);

                    if (newNode != nullptr)
                    {
                        assert(stmtMorphPending);
                        node = newNode;
                    }

                    if (node->GeneratesAssertion())
                    {
                        AssertionInfo info = node->GetAssertionInfo();
                        AddImpliedAssertions(info.GetAssertionIndex(), assertions);
                        BitVecOps::AddElemD(&countTraits, assertions, info.GetAssertionIndex() - 1);
                    }
                }

                if (stmtMorphPending)
                {
                    JITDUMP("\nMorphing statement " FMT_STMT "\n", stmt->GetID())
                    compiler->fgMorphBlockStmt(block, stmt DEBUGARG(__FUNCTION__));
                }

                // Check if propagation removed statements starting from current stmt.
                // If so, advance to the next good statement.
                Statement* nextStmt = (prevStmt == nullptr) ? block->firstStmt() : prevStmt->GetNextStmt();
                stmt                = (stmt == nextStmt) ? stmt->GetNextStmt() : nextStmt;
            }
        }

        INDEBUG(currentBlock = nullptr);
    }

#ifdef DEBUG
    void DebugCheckAssertion(const AssertionDsc& assertion) const
    {
        const auto  kind = assertion.kind;
        const auto& op1  = assertion.op1;
        const auto& op2  = assertion.op2;

        if (kind == OAK_BOUNDS_CHK)
        {
            assert((op1.kind == O1K_VALUE_NUMBER) && varTypeIsIntegral(vnStore->TypeOfVN(op1.vn)));
            assert((op2.kind == O2K_VALUE_NUMBER) && varTypeIsIntegral(vnStore->TypeOfVN(op2.vn)));

            return;
        }

        if (kind == OAK_RANGE)
        {
            assert((op1.kind == O1K_VALUE_NUMBER) && varTypeIsIntegral(vnStore->TypeOfVN(op1.vn)));
            assert((op2.kind == O2K_RANGE) && (op2.vn == NoVN));

            return;
        }

        assert((kind == OAK_EQUAL) || (kind == OAK_NOT_EQUAL));

        if ((op1.kind == O1K_BOUND_LOOP_BND) || (op1.kind == O1K_BOUND_OPER_BND))
        {
            assert(varTypeIsIntegral(vnStore->TypeOfVN(op1.vn)));
            assert((op2.kind == O2K_CONST_INT) && (op2.intCon.value == 0) && (op2.vn == vnStore->VNForIntCon(0)));

            return;
        }

        if (op1.kind == O1K_INSTANCE_OF)
        {
            assert(vnStore->TypeOfVN(op1.vn) == TYP_REF);
            assert(op2.kind == O2K_VALUE_NUMBER);
            assert(vnStore->TypeOfVN(op2.vn) == TYP_I_IMPL);

            return;
        }

        assert(vnStore->TypeOfVN(op1.vn) != TYP_UNDEF);
        assert(vnStore->TypeOfVN(op2.vn) != TYP_UNDEF);

        if (op1.kind == O1K_LCLVAR)
        {
            assert(!compiler->lvaGetDesc(op1.lclNum)->IsAddressExposed());
#ifdef TARGET_64BIT
            assert((op2.kind == O2K_CONST_INT) || (op2.kind == O2K_CONST_DOUBLE));
#else
            assert((op2.kind == O2K_CONST_INT) || (op2.kind == O2K_CONST_LONG) || (op2.kind == O2K_CONST_DOUBLE));
#endif
            return;
        }

        assert((op1.kind == O1K_VALUE_NUMBER) && ((op2.kind == O2K_VALUE_NUMBER) || (op2.kind == O2K_CONST_INT)));
    }

    void TraceAssertion(const char* message, const AssertionDsc& assertion, const char* comment = nullptr)
    {
        printf(FMT_BB " [%06u] %s %s ", currentBlock->bbNum, currentNode->GetID(),
               GenTree::OpName(currentNode->GetOper()), message);

        DumpAssertion(assertion);

        if (comment != nullptr)
        {
            printf(" ; %s\n", comment);
        }
        else
        {
            printf("\n");
        }
    }

    void DumpAssertion(const AssertionDsc& assertion) const
    {
        ssa.DumpAssertion(assertion, static_cast<unsigned>(&assertion - assertionTable));
    }

    void DumpAssertionIndices(const char* header, ASSERT_TP assertions, const char* footer) const
    {
        ssa.DumpAssertionIndices(header, assertions, footer);
    }
#endif // DEBUG
};

void SsaOptimizer::DoAssertionProp()
{
    AssertionProp ap(*this);
    ap.Run();
}

#ifdef DEBUG

void SsaOptimizer::DumpAssertion(const AssertionDsc& assertion, unsigned index)
{
    const auto  kind = assertion.kind;
    const auto& op1  = assertion.op1;
    const auto& op2  = assertion.op2;

    if (kind == OAK_BOUNDS_CHK)
    {
        printf("BoundsChk assertion A%02u: " FMT_VN " LT_UN " FMT_VN, index, op1.vn, op2.vn);
        return;
    }

    if (kind == OAK_RANGE)
    {
        printf("Range assertion A%02u: " FMT_VN " IN [%d..%d]", index, op1.vn, op2.range.min, op2.range.max);
        return;
    }

    if (op1.kind == O1K_INSTANCE_OF)
    {
        printf("Type assertion A%02u: MT(" FMT_VN ") %s " FMT_VN, index, op1.vn, (kind == OAK_EQUAL) ? "IS" : "IS NOT",
               op2.vn);
        return;
    }

    if ((op1.kind == O1K_BOUND_OPER_BND) || (op1.kind == O1K_BOUND_LOOP_BND))
    {
        printf("Bounds assertion A%02u: " FMT_VN " %s", index, op1.vn, kind == OAK_EQUAL ? "NOT(" : "");

        VNFuncApp funcApp;
        vnStore->GetVNFunc(op1.vn, &funcApp);

        ValueNumStore::CompareCheckedBoundArithInfo cmpInfo;

        if (op1.kind == O1K_BOUND_OPER_BND)
        {
            vnStore->GetCompareCheckedBoundArithInfo(funcApp, &cmpInfo);
        }
        else
        {
            vnStore->GetCompareCheckedBound(funcApp, &cmpInfo);
        }

        cmpInfo.Dump();
        printf("%s", kind == OAK_EQUAL ? ")" : "");

        return;
    }

    printf("Equality assertion A%02u: " FMT_VN, index, op1.vn);

    if (op1.kind == O1K_LCLVAR)
    {
        printf(" (V%02u)", op1.lclNum);
    }

    printf(" %s " FMT_VN, kind == OAK_EQUAL ? "EQ" : "NE", op2.vn);

    switch (op2.kind)
    {
        case O2K_CONST_INT:
            if (op2.intCon.flags != GTF_EMPTY)
            {
                printf(" (0x%p %s)", dspPtr(op2.intCon.value), dmpGetHandleKindName(op2.intCon.flags));
            }
            else
            {
                printf(" (IntCon %Id)", op2.intCon.value);
            }
            break;
#ifndef TARGET_64BIT
        case O2K_CONST_LONG:
            printf(" (LngCon 0x%016llx)", op2.lngCon.value);
            break;
#endif
        case O2K_CONST_DOUBLE:
            printf(" (DblCon %#.17g)", op2.dblCon.value);
            break;
        default:
            break;
    }
}

void SsaOptimizer::DumpAssertionIndices(const char* header, ASSERT_TP assertions, const char* footer)
{
    if (!compiler->verbose)
    {
        return;
    }

    if (header != nullptr)
    {
        printf("%s", header);
    }

    printf("{");
    const char* separator = "";

    BitVecTraits apTraits(assertionCount, compiler);
    for (BitVecOps::Enumerator en(&apTraits, assertions); en.MoveNext();)
    {
        printf("%sA%02u", separator, en.Current());
        separator = ", ";
    }

    printf("}");

    if (footer != nullptr)
    {
        printf("%s", footer);
    }
}

#endif // DEBUG

bool BoundsAssertion::IsBoundsAssertion() const
{
    return (assertion.kind == OAK_EQUAL) || (assertion.kind == OAK_NOT_EQUAL) || (assertion.kind == OAK_RANGE);
}

bool BoundsAssertion::IsEqual() const
{
    return assertion.kind == OAK_EQUAL;
}

bool BoundsAssertion::IsCompareCheckedBoundArith() const
{
    return assertion.op1.kind == O1K_BOUND_OPER_BND;
}

bool BoundsAssertion::IsCompareCheckedBound() const
{
    return assertion.op1.kind == O1K_BOUND_LOOP_BND;
}

bool BoundsAssertion::IsRange() const
{
    return assertion.kind == OAK_RANGE;
}

bool BoundsAssertion::IsConstant() const
{
    return assertion.op2.kind == O2K_CONST_INT;
}

ValueNum BoundsAssertion::GetVN() const
{
    return assertion.op1.vn;
}

ValueNum BoundsAssertion::GetConstantVN() const
{
    return assertion.op2.vn;
}

int BoundsAssertion::GetRangeMin() const
{
    assert(assertion.kind == OAK_RANGE);
    return static_cast<int>(assertion.op2.range.min);
}

int BoundsAssertion::GetRangeMax() const
{
    assert(assertion.kind == OAK_RANGE);
    return static_cast<int>(assertion.op2.range.max);
}

BoundsAssertion SsaOptimizer::GetBoundsAssertion(unsigned bitIndex) const
{
    assert(bitIndex < assertionCount);

    return assertionTable[bitIndex];
}

#ifdef DEBUG
const AssertionDsc& BoundsAssertion::GetAssertion() const
{
    return assertion;
}

void SsaOptimizer::DumpBoundsAssertion(BoundsAssertion assertion)
{
    DumpAssertion(assertion.GetAssertion(), static_cast<unsigned>(&assertion.GetAssertion() - assertionTable));
    printf("\n");
}
#endif
