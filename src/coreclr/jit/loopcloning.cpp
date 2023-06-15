// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

// Loop cloning is an optimization which duplicates a loop to create two versions.
// One copy is optimized by hoisting out various dynamic checks, such as array bounds
// checks that can't be statically eliminated. The checks are dynamically run. If
// they fail, the original copy of the loop is executed. If they pass, the
// optimized copy of the loop is executed, knowing that the bounds checks are
// dynamically unnecessary.
//
// The optimization can reduce the amount of code executed within a loop body.
//
// For example:
//
//     public static int f(int[] a, int l)
//     {
//         int sum = 0;
//         for (int i = 0; i < l; i++)
//         {
//             sum += a[i];     // This array bounds check must be executed in the loop
//         }
//     }
//
// This can be transformed to (in pseudo-code):
//
//     public static int f(int[] a, int l)
//     {
//         int sum = 0;
//         if (a != null && l <= a.Length)
//         {
//             for (int i = 0; i < l; i++)
//             {
//                 sum += a[i]; // no bounds check needed
//             }
//         }
//         else
//         {
//             for (int i = 0; i < l; i++)
//             {
//                 // bounds check needed. We need to do the normal computation (esp., side effects) before the
//                 // exception occurs.
//                 sum += a[i];
//             }
//         }
//     }
//
// One generalization of this is "loop unswitching".
//
// Because code is duplicated, this is a code size expanding optimization, and
// therefore we need to be careful to avoid duplicating too much code unnecessarily.
//
// Also, there is a risk that we can duplicate the loops and later, downstream
// phases optimize away the bounds checks even on the un-optimized copy of the loop.
//
// Loop cloning is implemented with the following steps:
//
// 1. Loop detection logic, which is existing logic in the JIT that records
//    loop information with loop flags.
//
// 2. Identify loop optimization candidates. This is done by optObtainLoopCloningOpts.
//    The loop context variable is updated with all the necessary information (for example:
//    block, stmt, tree information) to do the optimization later.
//         a) This involves checking if the loop is well-formed with respect to
//         the optimization being performed.
//         b) In array bounds check case, reconstructing the morphed GT_INDEX_ADDR
//         nodes back to their array representation.
//             i) The array index is stored in the "context" variable with
//             additional block, tree, stmt info.
//
// 3. Once the optimization candidates are identified, we derive cloning conditions.
//    For example: to clone a simple "for (i=0; i<n; ++i) { a[i] }" loop, we need the
//    following conditions:
//           (a != null) && (n >= 0) && (n <= a.length) && (stride > 0)
//    Note that "&&" implies a short-circuiting operator. This requires each condition
//    to be in its own block with its own comparison and branch instruction. This can
//    be optimized if there are no dependent conditions in a block by using a bitwise
//    AND instead of a short-circuit AND. The (a != null) condition needs to occur before
//    "a.length" is checked. But otherwise, the last three conditions can be computed in
//    the same block, as:
//           (a != null) && ((n >= 0) & (n <= a.length) & (stride > 0))
//    Since we're optimizing for the expected fast path case, where all the conditions
//    are true, we expect all the conditions to be executed most of the time. Thus, it
//    is advantageous to make as many as possible non-short-circuiting to reduce the
//    number of compare/branch/blocks needed.
//
//    In the above case, stride == 1, so we statically know stride > 0.
//
//    If we had "for (i=0; i<=n; ++i) { a[i] }", we would need:
//           (a != null) && (n >= 0) && (a.length >= 1) && (n <= a.length - 1) && (stride > 0)
//    This is more complicated. The loop is equivalent (except for possible overflow) to:
//           for (i=0; i<n+1; ++i) { a[i] }"
//    (`n+1` due to the `++i` stride). We'd have to worry about overflow doing this conversion, though.
//
//    REVIEW: why do we need the (n >= 0) condition? We do need to know
//    "array index var initialization value >= array lower bound (0)".
//
//           a) Conditions that need to be in their own blocks to enable short-circuit are called block
//           conditions or deref-conditions.
//              i) For a doubly nested loop on i, j, we would then have conditions like
//                  (a != null) && (i < a.len) && (a[i] != null) && (j < a[i].len)
//              all short-circuiting creating blocks.
//
//              Advantage:
//                 All conditions are checked before we enter the fast path. So fast
//                 path gets as fast as it can be.
//
//              Disadvantage:
//                 Creation of blocks.
//
//              Heuristic:
//                 Therefore we will not clone if we exceed creating 4 blocks.
//                 Note: this means we never clone more than 2-dimension a[i][j] expressions
//                 (see optComputeDerefConditions()).
//                 REVIEW: make this heuristic defined by a COMPlus variable, for easier
//                 experimentation, and make it more dynamic and based on potential benefit?
//
//           b) The other conditions called cloning conditions are transformed into LcCondition
//           structs which are then optimized.
//              i) Optimization of conditions involves removing redundant condition checks.
//              ii) If some conditions evaluate to true statically, then they are removed.
//              iii) If any condition evaluates to false statically, then loop cloning is
//              aborted for that loop.
//
// 4. Then the block splitting occurs and loop cloning conditions are transformed into
//    GenTree and added to the loop cloning choice block (the block that determines which
//    copy of the loop is executed).
//
// Preconditions
//
// 1. Loop detection has completed and the loop table is populated.
//
// 2. The loops that will be considered are the ones with the LPFLG_ITER flag:
//    "for (i = icon or lclVar; test_condition(); i++)"
//
// Limitations
//
// 1. For array based optimizations the loop choice condition is checked
//    before the loop body. This implies that the loop initializer statement
//    has not executed at the time of the check. So any loop cloning condition
//    involving the initial value of the loop counter cannot be condition checked
//    as it hasn't been assigned yet at the time of condition checking. Therefore
//    the initial value has to be statically known. This can be fixed with further
//    effort.
//
// 2. Loops containing nested exception handling regions are not cloned. (Cloning them
//    would require creating new exception handling regions for the cloned loop, which
//    is "hard".) There are a few other EH-related edge conditions that also cause us to
//    reject cloning.
//
// 3. If the loop contains RETURN blocks, and cloning those would push us over the maximum
//    number of allowed RETURN blocks in the function (either due to GC info encoding limitations
//    or otherwise), we reject cloning.
//
// 4. Loop increment must be `i += 1`
//
// 5. Loop test must be `i < x` where `x` is a constant, a variable, or `a.Length` for array `a`
//
// (There is some implementation support for decrementing loops, but it is incomplete.
// There is some implementation support for `i <= x` conditions, but it is incomplete
// (Compiler::optDeriveLoopCloningConditions() only handles GT_LT conditions))
//
// 6. Loop must have been converted to a do-while form.
//
// 7. There are a few other loop well-formedness conditions.
//
// 8. Multi-dimensional (non-jagged) loop index checking is only partially implemented.
//
// 9. Constant initializations and constant limits must be non-negative (REVIEW: why? The
//    implementation does use `unsigned` to represent them.)
//
// 10. The cloned loop (the slow path) is not added to the loop table, meaning certain
//    downstream optimization passes do not see them. See
//    https://github.com/dotnet/runtime/issues/43713.
//
// Assumptions
//
// 1. The assumption is that the optimization candidates collected during the
//    identification phase will be the ones that will be optimized. In other words,
//    the loop that is present originally will be the fast path. The cloned
//    path will be the slow path and will be unoptimized. This allows us to
//    collect additional information at the same time as identifying the optimization
//    candidates. This later helps us to perform the optimizations during actual cloning.
//
// 2. All loop cloning choice conditions will automatically be "AND"-ed. These are bitwise AND operations.
//
// 3. Perform short circuit AND for (array != null) side effect check
//   before hoisting (limit <= a.length) check.

// Represents an array access and associated bounds checks.
// Array access is required to have the array and indices in local variables.
// This struct is constructed using a GT_INDEX_ADDR node that is broken into
// its sub trees.
struct ArrIndex
{
    JitVector<unsigned>   indLcls;              // The indices local nums
    JitVector<GenTreeOp*> bndsChks;             // The bounds check COMMA nodes along each dimension.
    unsigned              arrLcl = BAD_VAR_NUM; // The array base local num
    unsigned              rank   = 0;           // Rank of the array

    ArrIndex(CompAllocator alloc) : indLcls(alloc), bndsChks(alloc)
    {
    }

#ifdef DEBUG
    void Print(unsigned dim = UINT_MAX)
    {
        printf("V%02u", arrLcl);

        for (unsigned i = 0; i < Min(rank, dim); ++i)
        {
            printf("[V%02u]", indLcls[i]);
        }
    }

    void PrintBoundsCheckNodes(unsigned dim = UINT_MAX)
    {
        for (unsigned i = 0; i < Min(rank, dim); ++i)
        {
            printf("[%06u]", bndsChks[i]->GetID());
        }
    }
#endif
};

struct LcOptInfo
{
    enum Kind
    {
        JaggedArray
    };

    const Kind kind;

    LcOptInfo(Kind kind) : kind(kind)
    {
    }
};

// Optimization info for a jagged array.
struct LcJaggedArrayOptInfo : public LcOptInfo
{
    unsigned dim; // "dim" represents up to what level of the rank this optimization applies to.
    // For example, a[i][j][k] could be the jagged array but if "dim" is 2,
    // then this node is treated as though it were a[i][j]
    ArrIndex   arrIndex;
    Statement* stmt;

    LcJaggedArrayOptInfo(const ArrIndex& arrIndex, unsigned dim, Statement* stmt)
        : LcOptInfo(JaggedArray), dim(dim), arrIndex(arrIndex), stmt(stmt)
    {
    }
};

// Symbolic representation of a.length, or a[i][j].length or a[i,j].length and so on.
// OperType decides whether "arrLength" is invoked on the array or if it is just an array.
struct LcArray
{
    enum Kind : uint8_t
    {
        Invalid,
        Jagged
    };

    enum Oper : uint8_t
    {
        None,
        ArrLen,
    };

    Kind kind;
    Oper oper;
    int  dim; // "dim" = which index to invoke arrLen on, if -1 invoke on the whole array
    // Example 1: a[0][1][2] and dim =  2 implies a[0][1].length
    // Example 2: a[0][1][2] and dim = -1 implies a[0][1][2].length
    ArrIndex* arrIndex;

    LcArray() : kind(Invalid), dim(-1)
    {
    }

    LcArray(Kind kind, ArrIndex* arrIndex, int dim, Oper oper) : kind(kind), oper(oper), dim(dim), arrIndex(arrIndex)
    {
    }

    LcArray(Kind kind, ArrIndex* arrIndex, Oper oper) : kind(kind), oper(oper), dim(-1), arrIndex(arrIndex)
    {
    }

    int GetDimRank() const
    {
        return dim < 0 ? static_cast<int>(arrIndex->rank) : dim;
    }

    bool operator==(const LcArray& that) const
    {
        assert(kind != Invalid && that.kind != Invalid);

        if (kind != that.kind || oper != that.oper || arrIndex->arrLcl != that.arrIndex->arrLcl)
        {
            return false;
        }

        int rank = GetDimRank();

        if (rank != that.GetDimRank())
        {
            return false;
        }

        for (int i = 0; i < rank; ++i)
        {
            if (arrIndex->indLcls[i] != that.arrIndex->indLcls[i])
            {
                return false;
            }
        }

        return true;
    }

    GenTree* ToGenTree(Compiler* comp);

#ifdef DEBUG
    void Print()
    {
        arrIndex->Print(dim);

        if (oper == ArrLen)
        {
            printf(".Length");
        }
    }
#endif
};

// Symbolic representation of either a constant like 1 or 2, or a variable like V02 or V03,
// or an "LcArray", or the null constant.
struct LcIdent
{
    enum Kind
    {
        Invalid,
        Const,
        Lcl,
        ArrLen,
        Null,
    };

    Kind     kind;
    LcArray  arrLen;   // kind == ArrLen
    unsigned constant; // kind == Const || kind == Lcl

    LcIdent() : kind(Invalid)
    {
    }

    explicit LcIdent(Kind kind) : kind(kind)
    {
    }

    LcIdent(unsigned constant, Kind kind) : kind(kind), constant(constant)
    {
    }

    explicit LcIdent(const LcArray& arrLen) : kind(ArrLen), arrLen(arrLen)
    {
    }

    bool operator==(const LcIdent& that) const
    {
        switch (kind)
        {
            case Const:
            case Lcl:
                return (kind == that.kind) && (constant == that.constant);
            case ArrLen:
                return (kind == that.kind) && (arrLen == that.arrLen);
            case Null:
                return (kind == that.kind);
            default:
                unreached();
        }
    }

    GenTree* ToGenTree(Compiler* comp);

#ifdef DEBUG
    void Print()
    {
        switch (kind)
        {
            case Const:
                printf("%u", constant);
                break;
            case Lcl:
                printf("V%02u", constant);
                break;
            case ArrLen:
                arrLen.Print();
                break;
            case Null:
                printf("null");
                break;
            default:
                printf("INVALID");
                break;
        }
    }
#endif
};

// Symbolic representation of an expr that involves an "LcIdent"
struct LcExpr
{
    LcIdent ident;

    explicit LcExpr(const LcIdent& ident) : ident(ident)
    {
    }

    bool operator==(const LcExpr& that) const
    {
        return ident == that.ident;
    }

    GenTree* ToGenTree(Compiler* comp);

#ifdef DEBUG
    void Print()
    {
        ident.Print();
    }
#endif
};

// Symbolic representation of a conditional operation involving two "LcExpr":
// LcExpr < LcExpr, for example: i > 0, i < a.length
struct LcCondition
{
    LcExpr     op1;
    LcExpr     op2;
    genTreeOps oper;

    LcCondition(genTreeOps oper, const LcExpr& op1, const LcExpr& op2) : op1(op1), op2(op2), oper(oper)
    {
    }

    bool Evaluate(bool* result);
    bool operator==(const LcCondition& cond) const;

    GenTree* ToGenTree(Compiler* comp);

#ifdef DEBUG
    void Print()
    {
        op1.Print();
        printf(" %s ", GenTree::OpName(oper));
        op2.Print();
    }
#endif
};

// A deref tree of an array expression.
// a[i][j][k], b[i] and a[i][y][k] are the occurrences in the loop, then, the tree would be:
//     a => {
//         i => {
//             j => {
//                 k => {}
//             },
//             y => {
//                 k => {}
//             },
//         }
//     },
//     b => {
//         i => {}
//     }
//
struct LcDeref
{
    const LcArray        array;
    JitVector<LcDeref*>* children = nullptr;
    const unsigned       level;

    LcDeref(const LcArray& array, unsigned level) : array(array), level(level)
    {
    }

    unsigned Lcl() const
    {
        return level == 0 ? array.arrIndex->arrLcl : array.arrIndex->indLcls[level - 1];
    }

    static LcDeref* Find(JitVector<LcDeref*>& children, unsigned lcl);

    void DeriveLevelConditions(JitVector<JitVector<LcCondition>*>& conds);

#ifdef DEBUG
    void Print(unsigned indent = 0)
    {
        unsigned tab = 4 * indent;
        printf("%*sV%02u, level %u => {", tab, "", Lcl(), level);

        if (children != nullptr)
        {
            for (unsigned i = 0; i < children->Size(); ++i)
            {
                if (i > 0)
                {
                    printf(",");
                }
                printf("\n");

                (*children)[i]->Print(indent + 1);
            }
        }

        printf("\n%*s}", tab, "");
    }
#endif
};

using LoopDsc = Compiler::LoopDsc;

struct LoopCloneContext
{
    Compiler* const compiler;
    LoopDsc* const  loopTable;
    unsigned const  loopCount;
    INDEBUG(bool const verbose;)
    CompAllocator alloc;
    // The array of optimization opportunities found in each loop. (loop x optimization-opportunities)
    jitstd::vector<JitVector<LcOptInfo*>*> optInfo;
    // The array of conditions that influence which path to take for each loop. (loop x cloning-conditions)
    jitstd::vector<JitVector<LcCondition>*> conditions;
    // The array of block levels of conditions for each loop. (loop x level x conditions)
    jitstd::vector<JitVector<JitVector<LcCondition>*>*> blockConditions;

    LoopCloneContext(Compiler* compiler)
        : compiler(compiler)
        , loopTable(compiler->optLoopTable)
        , loopCount(compiler->optLoopCount)
#ifdef DEBUG
        , verbose(compiler->verbose)
#endif
        , alloc(compiler->getAllocator(CMK_LoopClone))
        , optInfo(alloc)
        , conditions(alloc)
        , blockConditions(alloc)
    {
        optInfo.resize(loopCount, nullptr);
        conditions.resize(loopCount, nullptr);
        blockConditions.resize(loopCount, nullptr);
    }

    void CondToStmtInBlock(JitVector<LcCondition>& conds, BasicBlock* block, bool reverse);
    void EvaluateConditions(unsigned loopNum, bool* pAllTrue, bool* pAnyFalse);
    void OptimizeConditions(JitVector<LcCondition>& conds);
    void OptimizeConditions(unsigned loopNum);
    void OptimizeBlockConditions(unsigned loopNum);
    bool IsLoopClonable(unsigned loopNum);
    void IdentifyLoopOptInfo(unsigned loopNum);
    Compiler::fgWalkResult CanOptimizeByLoopCloning(GenTree* tree, LoopCloneVisitorInfo& info);
    bool ExtractArrIndex(GenTree* tree, ArrIndex* result, unsigned tempArrayLclNum);
    bool ReconstructArrIndex(GenTree* tree, ArrIndex* result, unsigned lhsNum);
    bool ArrLenLimit(const LoopDsc& loop, ArrIndex* index);
    void PerformStaticOptimizations(unsigned loopNum);
    bool ComputeDerefConditions(const ArrayStack<LcArray>& derefs, unsigned loopNum);
    bool DeriveLoopCloningConditions(unsigned loopNum);
    BasicBlock* InsertLoopChoiceConditions(unsigned loopNum, BasicBlock* head, BasicBlock* slow);
    void CloneLoop(unsigned loopNum);
    bool Run();

    void AddArrayIndex(unsigned loopNum, ArrIndex& index, int dim, Statement* stmt)
    {
        if (optInfo[loopNum] == nullptr)
        {
            optInfo[loopNum] = new (alloc) JitVector<LcOptInfo*>(alloc);
        }

        optInfo[loopNum]->Add(new (alloc) LcJaggedArrayOptInfo(index, dim, stmt));
    }

    JitVector<LcOptInfo*>* GetLoopOptInfo(unsigned loopNum)
    {
        return optInfo[loopNum];
    }

    void CancelLoopOptInfo(unsigned loopNum)
    {
        JITDUMP("Cancelling loop cloning for loop " FMT_LP "\n", loopNum);

        optInfo[loopNum]    = nullptr;
        conditions[loopNum] = nullptr;
    }

    JitVector<LcCondition>* EnsureConditions(unsigned loopNum)
    {
        if (conditions[loopNum] == nullptr)
        {
            conditions[loopNum] = new (alloc) JitVector<LcCondition>(alloc);
        }

        return conditions[loopNum];
    }

    bool HasBlockConditions(unsigned loopNum) const
    {
        JitVector<JitVector<LcCondition>*>* levelCond = blockConditions[loopNum];

        if (levelCond == nullptr)
        {
            return false;
        }

        for (unsigned i = 0; i < levelCond->Size(); ++i)
        {
            if ((*levelCond)[i]->Size() > 0)
            {
                return true;
            }
        }

        return false;
    }

#ifdef DEBUG
    void PrintBlockConditions(unsigned loopNum)
    {
        printf("Block conditions:\n");

        JitVector<JitVector<LcCondition>*>* blockConds = blockConditions[loopNum];

        if (blockConds == nullptr || blockConds->Size() == 0)
        {
            printf("No block conditions\n");
            return;
        }

        for (unsigned i = 0; i < blockConds->Size(); ++i)
        {
            PrintBlockLevelConditions(i, *(*blockConds)[i]);
        }
    }

    void PrintBlockLevelConditions(unsigned level, JitVector<LcCondition>& levelCond)
    {
        printf("%u = ", level);

        for (unsigned j = 0; j < levelCond.Size(); ++j)
        {
            if (j != 0)
            {
                printf(" & ");
            }

            levelCond[j].Print();
        }

        printf("\n");
    }

    void PrintConditions(unsigned loopNum);
#endif
};

GenTree* LcArray::ToGenTree(Compiler* comp)
{
    assert(kind == Jagged);

    // Create a a[i][j][k].length type node.
    GenTree* arr  = comp->gtNewLclvNode(arrIndex->arrLcl, comp->lvaGetDesc(arrIndex->arrLcl)->GetType());
    int      rank = GetDimRank();

    for (int i = 0; i < rank; ++i)
    {
        unsigned indexLclNum = arrIndex->indLcls[i];

        GenTreeIndexAddr* addr =
            comp->gtNewArrayIndexAddr(arr, comp->gtNewLclvNode(indexLclNum, comp->lvaGetDesc(indexLclNum)->GetType()),
                                      TYP_REF);
        // Clear the range check flag and mark the index as non-faulting: we guarantee that all necessary range
        // checking has already been done by the time this array index expression is invoked.
        arr->gtFlags &= ~(GTF_INX_RNGCHK | GTF_EXCEPT);
        arr = comp->gtNewIndexIndir(TYP_REF, addr);
        arr->gtFlags &= ~GTF_EXCEPT;
        arr->gtFlags |= GTF_IND_NONFAULTING;
        arr->AsIndir()->SetAddr(comp->fgMorphIndexAddr(addr));
    }

    if (oper == ArrLen)
    {
        GenTree* arrLen = comp->gtNewArrLen(arr, OFFSETOF__CORINFO_Array__length);

        // We already guaranteed (by a sequence of preceding checks) that the array length operator will not
        // throw an exception because we null checked the base array.
        // So, we should be able to do the following:
        //     arrLen->gtFlags &= ~GTF_EXCEPT;
        //     arrLen->gtFlags |= GTF_IND_NONFAULTING;
        // However, we then end up with a mix of non-faulting array length operators as well as normal faulting
        // array length operators in the slow-path of the cloned loops. CSE doesn't keep these separate, so bails
        // out on creating CSEs on this very useful type of CSE, leading to CQ losses in the cloned loop fast path.
        // TODO-CQ: fix this.
        return arrLen;
    }

    assert(oper == None);

    return arr;
}

GenTree* LcIdent::ToGenTree(Compiler* comp)
{
    switch (kind)
    {
        case Const:
            assert(constant <= INT32_MAX);
            return comp->gtNewIconNode(constant);
        case Lcl:
            return comp->gtNewLclvNode(constant, comp->lvaGetDesc(constant)->GetType());
        case ArrLen:
            return arrLen.ToGenTree(comp);
        case Null:
            return comp->gtNewIconNode(0, TYP_REF);
        default:
            unreached();
    }
}

GenTree* LcExpr::ToGenTree(Compiler* comp)
{
    return ident.ToGenTree(comp);
}

GenTree* LcCondition::ToGenTree(Compiler* comp)
{
    GenTree* op1Tree = op1.ToGenTree(comp);
    GenTree* op2Tree = op2.ToGenTree(comp);

    assert(varTypeSize(varActualType(op1Tree->GetType())) == varTypeSize(varActualType(op2Tree->GetType())));

    return comp->gtNewOperNode(oper, TYP_INT, op1Tree, op2Tree);
}

bool LcCondition::Evaluate(bool* result)
{
    switch (oper)
    {
        case GT_EQ:
        case GT_GE:
        case GT_LE:
            if (op1 == op2)
            {
                *result = true;
                return true;
            }
            return false;

        case GT_GT:
        case GT_LT:
        case GT_NE:
            if (op1 == op2)
            {
                *result = false;
                return true;
            }
            return false;

        default:
            return false;
    }
}

bool LcCondition::operator==(const LcCondition& cond) const
{
    if (oper == cond.oper && op1 == cond.op1 && op2 == cond.op2)
    {
        return true;
    }

    if ((oper == GT_LT || oper == GT_LE || oper == GT_GT || oper == GT_GE) &&
        GenTree::ReverseRelop(oper) == cond.oper && op1 == cond.op2 && op2 == cond.op1)
    {
        return true;
    }

    return false;
}

// Evaluate the loop cloning conditions statically, if they can be evaluated.
//
// For example, a condition like "V02 >= V02" statically evaluates to true. Caller should detect such
// conditions and remove them from the "conditions" array.
//
// Similarly, conditions like "V02 > V02" will evaluate to "false". In this case caller has to abort
// loop cloning optimization for the loop. Note that the assumption for conditions is that they will
// all be "AND"ed, so statically we know we will never take the fast path.
//
// Sometimes we simply can't say statically whether "V02 > V01.length" is true or false.
// In that case, `*pAllTrue` will be false because this condition doesn't evaluate to "true" and
// `*pAnyFalse` could be false if no other condition statically evaluates to "false".
//
// If `*pAnyFalse` is true, we set that and return, and `*pAllTrue` is not accurate, since the loop cloning
// needs to be aborted.
//
void LoopCloneContext::EvaluateConditions(unsigned loopNum, bool* pAllTrue, bool* pAnyFalse)
{
    JitVector<LcCondition>& conds = *conditions[loopNum];

    JITDUMP("Evaluating %u loop cloning conditions for loop " FMT_LP "\n", conds.Size(), loopNum);
    assert(conds.Size() > 0);

    bool allTrue  = true;
    bool anyFalse = false;

    for (unsigned i = 0; i < conds.Size(); ++i)
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("Considering condition %u: (", i);
            conds[i].Print();
        }
#endif

        bool res = false;

        if (conds[i].Evaluate(&res))
        {
            JITDUMP(") evaluates to %s\n", dspBool(res));

            if (!res)
            {
                anyFalse = true;

                // Since this will force us to abort loop cloning, there is no need compute an accurate `allTrue`,
                // so we can break out of the loop now.
                // REVIEW: it appears we never hit this condition in any test.
                break;
            }
        }
        else
        {
            JITDUMP("), could not be evaluated\n");
            allTrue = false;
        }
    }

    JITDUMP("Evaluation result allTrue = %s, anyFalse = %s\n", dspBool(allTrue), dspBool(anyFalse));
    *pAllTrue  = allTrue;
    *pAnyFalse = anyFalse;
}

// Evaluate the loop cloning conditions statically, if they can be evaluated then optimize
// the "conditions" array accordingly.
//
// For example, a condition like "V02 >= V02" statically evaluates to true. Remove such conditions
// from the "conditions" array.
//
// Similarly, conditions like "V02 > V02" will evaluate to "false". In this case abort loop cloning
// optimization for the loop.
//
void LoopCloneContext::OptimizeConditions(JitVector<LcCondition>& conds)
{
    for (unsigned i = 0; i < conds.Size(); ++i)
    {
        bool result = false;

        if (conds[i].Evaluate(&result))
        {
            if (!result)
            {
                CancelLoopOptInfo(i);
                break;
            }

            conds.Remove(i);
            --i;
            continue;
        }

        for (unsigned j = i + 1; j < conds.Size(); ++j)
        {
            if (conds[i] == conds[j])
            {
                conds.Remove(j);
                --j;
            }
        }
    }
}

void LoopCloneContext::OptimizeBlockConditions(unsigned loopNum)
{
    if (!HasBlockConditions(loopNum))
    {
        return;
    }

    JitVector<JitVector<LcCondition>*>& levelCond = *blockConditions[loopNum];

    for (unsigned i = 0; i < levelCond.Size(); ++i)
    {
        OptimizeConditions(*levelCond[i]);
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("After optimizing block-level cloning conditions\n\t");
        PrintConditions(loopNum);
        printf("\n");
    }
#endif
}

void LoopCloneContext::OptimizeConditions(unsigned loopNum)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("Before optimizing cloning conditions\n\t");
        PrintConditions(loopNum);
        printf("\n");
    }
#endif

    OptimizeConditions(*conditions[loopNum]);

#ifdef DEBUG
    if (verbose)
    {
        printf("After optimizing cloning conditions\n\t");
        PrintConditions(loopNum);
        printf("\n");
    }
#endif
}

#ifdef DEBUG
void LoopCloneContext::PrintConditions(unsigned loopNum)
{
    JitVector<LcCondition>* loopConditions = conditions[loopNum];

    if (loopConditions == nullptr)
    {
        printf("NO conditions");
        return;
    }

    if (loopConditions->Size() == 0)
    {
        printf("Conditions were optimized away! Will always take cloned path.");
        return;
    }

    for (unsigned i = 0; i < loopConditions->Size(); ++i)
    {
        if (i != 0)
        {
            printf(" & ");
        }

        (*loopConditions)[i].Print();
    }
}
#endif

void LoopCloneContext::CondToStmtInBlock(JitVector<LcCondition>& conds, BasicBlock* block, bool reverse)
{
    noway_assert(conds.Size() > 0);

    GenTree* cond = conds[0].ToGenTree(compiler);

    for (unsigned i = 1; i < conds.Size(); ++i)
    {
        cond = compiler->gtNewOperNode(GT_AND, TYP_INT, cond, conds[i].ToGenTree(compiler));
    }

    cond = compiler->gtNewOperNode(reverse ? GT_NE : GT_EQ, TYP_INT, cond, compiler->gtNewIconNode(0));

    GenTree*   jtrue = compiler->gtNewOperNode(GT_JTRUE, TYP_VOID, cond);
    Statement* stmt  = compiler->fgNewStmtFromTree(jtrue);

    compiler->fgInsertStmtAtEnd(block, stmt);

    JITDUMPTREE(jtrue, "Loop cloning condition tree before morphing:\n");
    JITDUMP("\n");

    compiler->fgMorphBlockStmt(block, stmt DEBUGARG("Loop cloning condition"));
}

void LcDeref::DeriveLevelConditions(JitVector<JitVector<LcCondition>*>& conds)
{
    if (level == 0)
    {
        // For level 0, just push (a != null).
        conds[level]->Emplace(GT_NE, LcExpr(LcIdent(Lcl(), LcIdent::Lcl)), LcExpr(LcIdent(LcIdent::Null)));
    }
    else
    {
        // Adjust for level 0 having just 1 condition and push condition (i < a.len).
        LcArray arrLen = array;
        arrLen.oper    = LcArray::ArrLen;
        arrLen.dim     = level - 1;

        conds[level * 2 - 1]->Emplace(GT_LT, LcExpr(LcIdent(Lcl(), LcIdent::Lcl)), LcExpr(LcIdent(arrLen)));

        // Push condition (a[i] != null)
        LcArray arrTmp = array;
        arrTmp.dim     = level;

        conds[level * 2]->Emplace(GT_NE, LcExpr(LcIdent(arrTmp)), LcExpr(LcIdent(LcIdent::Null)));
    }

    if (children != nullptr)
    {
        for (unsigned i = 0; i < children->Size(); ++i)
        {
            (*children)[i]->DeriveLevelConditions(conds);
        }
    }
}

LcDeref* LcDeref::Find(JitVector<LcDeref*>& children, unsigned lcl)
{
    for (unsigned i = 0; i < children.Size(); ++i)
    {
        if (children[i]->Lcl() == lcl)
        {
            return children[i];
        }
    }

    return nullptr;
}

bool LoopCloneContext::ArrLenLimit(const LoopDsc& loop, ArrIndex* index)
{
    INDEBUG(loop.VerifyIterator());
    assert(loop.lpFlags & LPFLG_ARRLEN_LIMIT);

    GenTree* array = loop.lpLimit()->AsArrLen()->GetArray();

    if (array->OperIs(GT_LCL_VAR))
    {
        index->arrLcl = array->AsLclVar()->GetLclNum();
        index->rank   = 0;

        return true;
    }

    if (array->OperIs(GT_COMMA))
    {
        return ReconstructArrIndex(array, index, BAD_VAR_NUM);
    }

    return false;
}

// Inspect the loop cloning optimization candidates and populate the conditions necessary
// for each optimization candidate. Checks if the loop stride is "> 0" if the loop
// condition is "less than". If the initializer is "var" init then adds condition
// "var >= 0", and if the loop is var limit then, "var >= 0" and "var <= a.len"
// are added to "context". These conditions are checked in the pre-header block
// and the cloning choice is made.
//
// Callers should assume AND operation is used i.e., if all conditions are true,
// then take the fast path.
//
bool LoopCloneContext::DeriveLoopCloningConditions(unsigned loopNum)
{
    JITDUMP("------------------------------------------------------------\n");
    JITDUMP("Deriving cloning conditions for " FMT_LP "\n", loopNum);

    const LoopDsc&         loop     = loopTable[loopNum];
    JitVector<LcOptInfo*>* optInfos = GetLoopOptInfo(loopNum);

    if (loop.lpTestOper() != GT_LT)
    {
        return false;
    }

    // Stride conditions
    if (loop.lpIterConst() <= 0)
    {
        JITDUMP("> Stride %d is invalid\n", loop.lpIterConst());
        return false;
    }

    // Init conditions
    if (loop.lpFlags & LPFLG_CONST_INIT)
    {
        // Only allowing non-negative const init at this time.
        // REVIEW: why?

        if (loop.lpConstInit < 0)
        {
            JITDUMP("> Init %d is invalid\n", loop.lpConstInit);

            return false;
        }
    }
    else if (loop.lpFlags & LPFLG_VAR_INIT)
    {
        // initVar >= 0
        EnsureConditions(loopNum)->Emplace(GT_GE, LcExpr(LcIdent(loop.lpVarInit, LcIdent::Lcl)),
                                           LcExpr(LcIdent(0, LcIdent::Const)));
    }
    else
    {
        JITDUMP("> Not variable init\n");
        return false;
    }

    // Limit Conditions
    LcIdent             ident;
    ArrayStack<LcArray> derefs(alloc);

    if (loop.lpFlags & LPFLG_CONST_LIMIT)
    {
        int limit = loop.lpConstLimit();

        if (limit < 0)
        {
            JITDUMP("> limit %d is invalid\n", limit);
            return false;
        }

        ident = LcIdent(static_cast<unsigned>(limit), LcIdent::Const);
    }
    else if (loop.lpFlags & LPFLG_VAR_LIMIT)
    {
        ident = LcIdent(loop.lpVarLimit(), LcIdent::Lcl);

        EnsureConditions(loopNum)->Emplace(GT_GE, LcExpr(ident), LcExpr(LcIdent(0, LcIdent::Const)));
    }
    else if (loop.lpFlags & LPFLG_ARRLEN_LIMIT)
    {
        ArrIndex* index = new (alloc) ArrIndex(alloc);

        if (!ArrLenLimit(loop, index))
        {
            JITDUMP("> ArrLen not matching");
            return false;
        }

        ident = LcIdent(LcArray(LcArray::Jagged, index, LcArray::ArrLen));

        // Ensure that this array must be dereference-able, before executing the actual condition.
        derefs.Emplace(LcArray::Jagged, index, LcArray::None);
    }
    else
    {
        JITDUMP("> Undetected limit\n");

        return false;
    }

    for (unsigned i = 0; i < optInfos->Size(); ++i)
    {
        LcOptInfo* optInfo = (*optInfos)[i];
        assert(optInfo->kind == LcOptInfo::JaggedArray);
        LcJaggedArrayOptInfo* arrIndexInfo = static_cast<LcJaggedArrayOptInfo*>(optInfo);

        EnsureConditions(loopNum)->Emplace(GT_LE, LcExpr(ident),
                                           LcExpr(LcIdent(LcArray(LcArray::Jagged, &arrIndexInfo->arrIndex,
                                                                  arrIndexInfo->dim, LcArray::ArrLen))));

        derefs.Emplace(LcArray::Jagged, &arrIndexInfo->arrIndex, arrIndexInfo->dim, LcArray::None);
    }

    JITDUMP("Conditions: ");
    DBEXEC(verbose, PrintConditions(loopNum));
    JITDUMP("\n");

    return (derefs.Size() == 0) || ComputeDerefConditions(derefs, loopNum);
}

// To be able to check for the loop cloning condition that (limitVar <= a.len)
// we should first be able to dereference "a". i.e., "a" is non-null.
//
// Example:
//
// for (i in 0..n)
//   for (j in 0..n)
//     for (k in 0..n)      // Inner most loop is being cloned. Cloning needs to check if
//                          // (n <= a[i][j].len) and other safer conditions to take the fast path
//       a[i][j][k] = 0
//
// Now, we want to deref a[i][j] to invoke length operator on it to perform the cloning fast path check.
// This involves deref of (a), (a[i]), (a[i][j]), therefore, the following should first
// be true to do the deref.
//
// (a != null) && (i < a.len) && (a[i] != null) && (j < a[i].len) && (a[i][j] != null) --> condition set (1)
//
// Note the short circuiting AND. Implication: these conditions should be performed in separate
// blocks each of which will branch to slow path if the condition evaluates to false.
//
// Now, imagine a situation where, in the inner loop above, in addition to "a[i][j][k] = 0" we
// also have:
//    a[x][y][k] = 20
// where x and y are parameters, then our conditions will have to include:
//    (x < a.len) &&
//    (y < a[x].len)
// in addition to the above conditions (1) to get rid of bounds check on index 'k'
//
// But these conditions can be checked together with conditions
// (i < a.len) without a need for a separate block. In summary, the conditions will be:
//
// (a != null) &&
// ((i < a.len) & (x < a.len)) &&      <-- Note the bitwise AND here.
// (a[i] != null & a[x] != null) &&    <-- Note the bitwise AND here.
// (j < a[i].len & y < a[x].len) &&    <-- Note the bitwise AND here.
// (a[i][j] != null & a[x][y] != null) <-- Note the bitwise AND here.
//
// This naturally yields a tree style pattern, where the nodes of the tree are
// the array and indices respectively.
//
// Example:
//     a => {
//         i => {
//             j => {
//                 k => {}
//             }
//         },
//         x => {
//             y => {
//                 k => {}
//             }
//         }
//     }
//
//     Notice that the variables in the same levels can have their conditions combined in the
//     same block with a bitwise AND. Whereas, the conditions in consecutive levels will be
//     combined with a short-circuiting AND (i.e., different basic blocks).
//
// Operation:
//   Construct a tree of array indices and the array which will generate the optimal
//   conditions for loop cloning.
//
//   a[i][j][k], b[i] and a[i][y][k] are the occurrences in the loop. Then, the tree should be:
//
//   a => {
//       i => {
//           j => {
//               k => {}
//           },
//           y => {
//               k => {}
//           },
//       }
//   },
//   b => {
//       i => {}
//   }
//
//   In this method, we will construct such a tree by descending depth first into the array
//   index operation and forming a tree structure as we encounter the array or the index variables.
//
//   This tree structure will then be used to generate conditions like below:
//   (a != null) & (b != null) &&       // from the first level of the tree.
//
//   (i < a.len) & (i < b.len) &&       // from the second level of the tree. Levels can be combined.
//   (a[i] != null) & (b[i] != null) && // from the second level of the tree.
//
//   (j < a[i].len) & (y < a[i].len) &&       // from the third level.
//   (a[i][j] != null) & (a[i][y] != null) && // from the third level.
//
//   and so on.
//
bool LoopCloneContext::ComputeDerefConditions(const ArrayStack<LcArray>& derefs, unsigned loopNum)
{
    assert(!derefs.Empty());
    assert(blockConditions[loopNum] == nullptr);

    JitVector<LcDeref*> nodes(alloc);
    int                 maxRank = -1;

    // For each array in the dereference list, construct a tree,
    // where the nodes are array and index variables and an edge 'u-v'
    // exists if a node 'v' indexes node 'u' directly as in u[v] or an edge
    // 'u-v-w' transitively if u[v][w] occurs.
    for (unsigned i = 0; i < derefs.Size(); ++i)
    {
        LcArray& array = derefs.GetRef(i);

        // First populate the array base variable.
        LcDeref* node = LcDeref::Find(nodes, array.arrIndex->arrLcl);

        if (node == nullptr)
        {
            node = new (alloc) LcDeref(array, /*level*/ 0);
            nodes.Add(node);
        }

        // For each dimension (level) for the array, populate the tree with the variable
        // from that dimension.
        unsigned rank = static_cast<unsigned>(array.GetDimRank());

        for (unsigned i = 0; i < rank; ++i)
        {
            LcDeref* tmp = nullptr;

            if (node->children == nullptr)
            {
                node->children = new (alloc) JitVector<LcDeref*>(alloc);
            }
            else
            {
                tmp = LcDeref::Find(*node->children, array.arrIndex->indLcls[i]);
            }

            if (tmp == nullptr)
            {
                tmp = new (alloc) LcDeref(array, node->level + 1);
                node->children->Add(tmp);
            }

            // Descend one level down.
            node = tmp;
        }

        // Keep the maxRank of all array dereferences.
        maxRank = Max(static_cast<int>(rank), maxRank);
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("Deref condition tree:\n");

        for (unsigned i = 0; i < nodes.Size(); ++i)
        {
            nodes[i]->Print();
            printf("\n");
        }
    }
#endif

    if (maxRank == -1)
    {
        JITDUMP("> maxRank undefined\n");
        return false;
    }

    // First level will always yield the null-check, since it is made of the array base variables.
    // All other levels (dimensions) will yield two conditions ex: (i < a.length && a[i] != null)
    const unsigned condBlocks = 1 + static_cast<unsigned>(maxRank) * 2;

    // Heuristic to not create too many blocks. Defining as 3 allows, effectively, loop cloning on
    // doubly-nested loops.
    // REVIEW: make this based on a COMPlus configuration, at least for debug?
    const unsigned maxAllowedCondBlocks = 3;

    if (condBlocks > maxAllowedCondBlocks)
    {
        JITDUMP("> Too many condition blocks (%u > %u)\n", condBlocks, maxAllowedCondBlocks);
        return false;
    }

    // Derive conditions into an 'array of level x array of conditions' i.e., levelCond[levels][conds]

    JitVector<JitVector<LcCondition>*>* levelCond = new (alloc) JitVector<JitVector<LcCondition>*>(alloc);
    levelCond->Reserve(condBlocks);

    for (unsigned i = 0; i < condBlocks; ++i)
    {
        levelCond->Add(new (alloc) JitVector<LcCondition>(alloc));
    }

    blockConditions[loopNum] = levelCond;

    for (unsigned i = 0; i < nodes.Size(); ++i)
    {
        nodes[i]->DeriveLevelConditions(*levelCond);
    }

    DBEXEC(verbose, PrintBlockConditions(loopNum));

    return true;
}

// Perform the optimizations on the fast path i.e., the path in which the
// optimization candidates were collected at the time of identifying them.
// The candidates store all the information necessary (the tree/stmt/block
// they are from) to perform the optimization.
//
// The unoptimized path is either already cloned when this method is called or
// there is no unoptimized path (got eliminated statically.) So this method
// performs the optimizations assuming that the path in which the candidates
// were collected is the fast path in which the optimizations will be performed.
//
void LoopCloneContext::PerformStaticOptimizations(unsigned loopNum)
{
    JitVector<LcOptInfo*>* optInfos = GetLoopOptInfo(loopNum);

    for (unsigned i = 0; i < optInfos->Size(); ++i)
    {
        LcOptInfo* optInfo = (*optInfos)[i];
        assert(optInfo->kind == LcOptInfo::JaggedArray);
        LcJaggedArrayOptInfo* arrIndexInfo = static_cast<LcJaggedArrayOptInfo*>(optInfo);

        // Remove all bounds checks for this array up to (and including) `arrIndexInfo->dim`. So, if that is 1,
        // Remove rank 0 and 1 bounds checks.

        for (unsigned dim = 0; dim <= arrIndexInfo->dim; dim++)
        {
            GenTreeOp* comma = arrIndexInfo->arrIndex.bndsChks[dim];

#ifdef DEBUG
            if (verbose)
            {
                printf("Remove bounds check [%06u]", comma->GetOp(0)->GetID());
                printf(" for " FMT_STMT ", dim% d, ", arrIndexInfo->stmt->GetID(), dim);
                arrIndexInfo->arrIndex.Print();
                printf(", bounds check nodes: ");
                arrIndexInfo->arrIndex.PrintBoundsCheckNodes();
                printf("\n");
            }
#endif

            if (GenTreeBoundsChk* boundsChk = comma->GetOp(0)->IsBoundsChk())
            {
                // This COMMA node will only represent a bounds check if we've haven't already removed this
                // bounds check in some other nesting cloned loop. For example, consider:
                //   for (i = 0; i < x; i++)
                //      for (j = 0; j < y; j++)
                //         a[i][j] = i + j;
                // If the outer loop is cloned first, it will remove the a[i] bounds check from the optimized
                // path. Later, when the inner loop is cloned, we want to remove the a[i][j] bounds check. If
                // we clone the inner loop, we know that the a[i] bounds check isn't required because we'll add
                // it to the loop cloning conditions. On the other hand, we can clone a loop where we get rid of
                // the nested bounds check but nobody has gotten rid of the outer bounds check. As before, we
                // know the outer bounds check is not needed because it's been added to the cloning conditions,
                // so we can get rid of the bounds check here.
                compiler->optRemoveRangeCheck(boundsChk, comma, arrIndexInfo->stmt);

                // TODO-MIKE-Cleanup: Turns out that CSE is dumb and it will make CSE def out of
                // COMMA(NOP, x) and a CSE use out x, because they have the same value numbers.
                // Can we just make CSE ignore COMMA(NOP, x)? Or remove it altogether somewhere
                // along the way?
                comma->gtFlags |= GTF_DONT_CSE;
            }
            else
            {
                JITDUMP("  Bounds check already removed\n");

                // If the bounds check node isn't there, it better have been converted to a GT_NOP.
                assert(comma->GetOp(0)->OperIs(GT_NOP));
            }
        }
    }
}

bool LoopCloneContext::IsLoopClonable(unsigned loopNum)
{
    const LoopDsc& loop = loopTable[loopNum];

    if (loop.lpIterTree == nullptr)
    {
        JITDUMP("Rejecting loop. No iterator.\n");
        return false;
    }

    if (loop.lpFlags & LPFLG_REMOVED)
    {
        JITDUMP("Rejecting loop. It is marked LPFLG_REMOVED.\n");
        return false;
    }

    // Make sure the loop doesn't have any embedded exception handling.
    // Walk the loop blocks from lexically first to lexically last (all blocks in this region must be
    // part of the loop), looking for a `try` begin block. Note that a loop must entirely contain any
    // EH region, or be itself entirely contained within an EH region. Thus, looking just for a `try`
    // begin is sufficient; there is no need to look for other EH constructs, such as a `catch` begin.
    //
    // TODO: this limitation could be removed if we do the work to insert new EH regions in the exception table,
    // for the cloned loop (and its embedded EH regions).
    //
    // Also, count the number of return blocks within the loop for future use.
    unsigned loopRetCount = 0;

    for (BasicBlock* const blk : loop.LoopBlocks())
    {
        if (blk->bbJumpKind == BBJ_RETURN)
        {
            loopRetCount++;
        }

        if (compiler->bbIsTryBeg(blk))
        {
            JITDUMP("Rejecting loop. It has a `try` begin.\n");
            return false;
        }
    }

    // Is the entry block a handler or filter start?  If so, then if we cloned, we could create a jump
    // into the middle of a handler (to go to the cloned copy.)  Reject.
    if (compiler->bbIsHandlerBeg(loop.lpEntry))
    {
        JITDUMP("Rejecting loop. Entry block is a handler start.\n");
        return false;
    }

    if (!BasicBlock::sameEHRegion(loop.lpHead, loop.lpEntry))
    {
        JITDUMP("Rejecting loop. Head and entry blocks are in different EH regions.\n");
        return false;
    }

    // Is the first block after the last block of the loop a handler or filter start?
    // Usually, we create a dummy block after the orginal loop, to skip over the loop clone
    // and go to where the original loop did.  That raises problems when we don't actually go to
    // that block; this is one of those cases.  This could be fixed fairly easily; for example,
    // we could add a dummy nop block after the (cloned) loop bottom, in the same handler scope as the
    // loop.  This is just a corner to cut to get this working faster.
    BasicBlock* bbAfterLoop = loop.lpBottom->bbNext;
    if (bbAfterLoop != nullptr && compiler->bbIsHandlerBeg(bbAfterLoop))
    {
        JITDUMP("Rejecting loop. Next block after bottom is a handler start.\n");
        return false;
    }

// We've previously made a decision whether to have separate return epilogs, or branch to one.
// There's a GCInfo limitation in the x86 case, so that there can be no more than SET_EPILOGCNT_MAX separate
// epilogs. Other architectures have a limit of 4 here for "historical reasons", but this should be revisited
// (or return blocks should not be considered part of the loop, rendering this issue moot).
#ifdef JIT32_GCENCODER
    constexpr unsigned epilogLimit = SET_EPILOGCNT_MAX;
#else
    constexpr unsigned epilogLimit = 4;
#endif

    if (compiler->fgReturnCount + loopRetCount > epilogLimit)
    {
        JITDUMP("Rejecting loop. It has %u returns; if added to previously existing %u returns, it would exceed the "
                "limit of %u.\n",
                loopRetCount, compiler->fgReturnCount, epilogLimit);
        return false;
    }

    unsigned ivLclNum = loop.lpIterVar();
    if (compiler->lvaGetDesc(ivLclNum)->IsAddressExposed())
    {
        JITDUMP("Rejecting loop. Rejected V%02u as iter var because is address-exposed.\n", ivLclNum);
        return false;
    }

    BasicBlock* head = loop.lpHead;
    BasicBlock* end  = loop.lpBottom;
    BasicBlock* beg  = head->bbNext;

    if (end->bbJumpKind != BBJ_COND)
    {
        JITDUMP("Rejecting loop. Couldn't find termination test.\n");
        return false;
    }

    if (end->bbJumpDest != beg)
    {
        JITDUMP("Rejecting loop. Branch at loop 'end' not looping to 'begin'.\n");
        return false;
    }

    // TODO-CQ: CLONE: Mark increasing or decreasing loops.
    if ((loop.lpIterOper() != GT_ADD) || (loop.lpIterConst() != 1))
    {
        JITDUMP("Rejecting loop. Loop iteration operator not matching.\n");
        return false;
    }

    if ((loop.lpFlags & (LPFLG_CONST_LIMIT | LPFLG_VAR_LIMIT | LPFLG_ARRLEN_LIMIT)) == 0)
    {
        JITDUMP("Rejecting loop. Loop limit is neither constant, variable or array length.\n");
        return false;
    }

    if (!((GenTree::StaticOperIs(loop.lpTestOper(), GT_LT, GT_LE) && (loop.lpIterOper() == GT_ADD)) ||
          (GenTree::StaticOperIs(loop.lpTestOper(), GT_GT, GT_GE) && (loop.lpIterOper() == GT_SUB))))
    {
        JITDUMP("Rejecting loop. Loop test (%s) doesn't agree with the direction (%s) of the loop.\n",
                GenTree::OpName(loop.lpTestOper()), GenTree::OpName(loop.lpIterOper()));
        return false;
    }

    if (!loop.lpTestTree->OperIsCompare() || ((loop.lpTestTree->gtFlags & GTF_RELOP_ZTT) == 0))
    {
        JITDUMP("Rejecting loop. Loop inversion NOT present, loop test [%06u] may not protect entry from head.\n",
                loop.lpTestTree->GetID());
        return false;
    }

    INDEBUG(GenTree* op1 = loop.lpIterator());
    assert(op1->OperIs(GT_LCL_VAR) && (op1->AsLclVar()->GetLclNum() == ivLclNum));

    // Otherwise, we're going to add those return blocks.
    // TODO-MIKE-Review: Why the crap does this update fgReturnCount before we actually clone the loop?
    compiler->fgReturnCount += loopRetCount;

    return true;
}

// Insert the loop conditions for a loop between loop head and entry
// Returns the last conditional block inserted.
//
// Note below that the cond0 is inverted in head, i.e., if true jump to cond1. This is because
// condn cannot jtrue to loop head h2. It has to be from a direct pred block.
//
// cond0 (in h)  -?> cond1
// slowHead      --> e2 (slowHead) always
// !cond1        -?> slowHead
// !cond2        -?> slowHead
// ...
// !condn        -?> slowHead
// h2/entry (fast)
//
// Insert condition 0 in 'h' and create other condition blocks and insert conditions in them.
// On entry, block 'h' is a conditional block, but its bbJumpDest hasn't yet been set.
//
BasicBlock* LoopCloneContext::InsertLoopChoiceConditions(unsigned loopNum, BasicBlock* head, BasicBlock* slowHead)
{
    JITDUMP("Inserting loop " FMT_LP " loop choice conditions\n", loopNum);
    assert(HasBlockConditions(loopNum));
    assert(head->bbJumpKind == BBJ_COND);

    JitVector<JitVector<LcCondition>*>& loopBlockConditions = *blockConditions[loopNum];
    BasicBlock*                         condBlock           = head;

    for (unsigned i = 0; i < loopBlockConditions.Size(); ++i)
    {
        JITDUMP("Adding loop " FMT_LP " level %u block conditions to " FMT_BB "\n    ", loopNum, i, condBlock->bbNum);

        const bool              isHeaderBlock   = condBlock == head;
        JitVector<LcCondition>& levelConditions = *loopBlockConditions[i];

        DBEXEC(verbose, PrintBlockLevelConditions(i, levelConditions));
        CondToStmtInBlock(levelConditions, condBlock, /*reverse*/ isHeaderBlock);

        BasicBlock* tmp = compiler->fgNewBBafter(BBJ_COND, isHeaderBlock ? slowHead : condBlock, /*extendRegion*/ true);
        tmp->inheritWeight(head);
        tmp->bbNatLoopNum = head->bbNatLoopNum;

        condBlock->bbJumpDest = isHeaderBlock ? tmp : slowHead;

        JITDUMP("Adding " FMT_BB " -> " FMT_BB "\n", condBlock->bbNum, condBlock->bbJumpDest->bbNum);
        compiler->fgAddRefPred(condBlock->bbJumpDest, condBlock);

        if (!isHeaderBlock)
        {
            JITDUMP("Adding " FMT_BB " -> " FMT_BB "\n", condBlock->bbNum, tmp->bbNum);
            compiler->fgAddRefPred(tmp, condBlock);
        }

        condBlock = tmp;
    }

    // Finally insert cloning conditions after all deref conditions have been inserted.

    JITDUMP("Adding loop " FMT_LP " cloning conditions to " FMT_BB "\n", loopNum, condBlock->bbNum);
    JITDUMP("    ");
    DBEXEC(verbose, PrintConditions(loopNum));
    JITDUMP("\n");

    CondToStmtInBlock(*conditions[loopNum], condBlock, /*reverse*/ false);

    return condBlock;
}

// Ensure that loop "loopNum" has a unique head block.
// If the existing entry has non-loop predecessors other than the head entry,
// create a new, empty block that goes (only) to the entry, and redirects the
// preds of the entry to this new block. Sets the weight of the newly created
// block to "ambientWeight".
//
// NOTE: this is currently dead code, because it is only called by loop cloning,
// and loop cloning only works with single-entry loops where the immediately
// preceding head block is the only predecessor of the loop entry.
//
void Compiler::optEnsureUniqueHead(unsigned loopNum, BasicBlock::weight_t ambientWeight)
{
    const LoopDsc& loop = optLoopTable[loopNum];

    BasicBlock* h = loop.lpHead;
    BasicBlock* t = loop.lpTop;
    BasicBlock* e = loop.lpEntry;
    BasicBlock* b = loop.lpBottom;

    // If "h" dominates the entry block, then it is the unique header.
    if (fgDominate(h, e))
    {
        return;
    }

    // Otherwise, create a new empty header block, make it the pred of the entry block,
    // and redirect the preds of the entry block to go to this.

    BasicBlock* beforeTop = t->bbPrev;
    assert(!beforeTop->bbFallsThrough() || (beforeTop->bbNext == e));

    // Make sure that the new block is in the same region as the loop.
    // (We will only create loops that are entirely within a region.)
    BasicBlock* h2 = fgNewBBafter(BBJ_NONE, beforeTop, /*extendRegion*/ true);
    assert(beforeTop->bbNext == h2);

    // This is in the containing loop.
    h2->bbNatLoopNum = loop.lpParent;
    h2->bbWeight     = h2->isRunRarely() ? BB_ZERO_WEIGHT : ambientWeight;

    if (h2->bbNext != e)
    {
        h2->bbJumpKind = BBJ_ALWAYS;
        h2->bbJumpDest = e;
    }
    BlockSetOps::Assign(this, h2->bbReach, e->bbReach);

    fgAddRefPred(e, h2);

    // Redirect paths from preds of "e" to go to "h2" instead of "e".
    BlockToBlockMap* blockMap = new (getAllocator(CMK_LoopClone)) BlockToBlockMap(getAllocator(CMK_LoopClone));
    blockMap->Set(e, h2);

    for (BasicBlock* const predBlock : e->PredBlocks())
    {
        // Skip if predBlock is in the loop.
        if (t->bbNum <= predBlock->bbNum && predBlock->bbNum <= b->bbNum)
        {
            continue;
        }

        optRedirectBlock(predBlock, blockMap);

        fgAddRefPred(h2, predBlock);
        fgRemoveRefPred(e, predBlock);
    }

    optUpdateLoopHead(loopNum, h, h2);
}

void LoopCloneContext::CloneLoop(unsigned loopNum)
{
    assert(loopNum < loopCount);

    LoopDsc& loop = loopTable[loopNum];

    JITDUMP("\nCloning loop " FMT_LP ": [head: " FMT_BB ", first: " FMT_BB ", top: " FMT_BB ", entry: " FMT_BB
            ", bottom: " FMT_BB ", child: " FMT_LP "].\n",
            loopNum, loop.lpHead->bbNum, loop.lpFirst->bbNum, loop.lpTop->bbNum, loop.lpEntry->bbNum,
            loop.lpBottom->bbNum, loop.lpChild);

    // Determine the depth of the loop, so we can properly weight blocks added (outside the cloned loop blocks).
    unsigned             depth         = compiler->optLoopDepth(loopNum);
    BasicBlock::weight_t ambientWeight = 1;
    for (unsigned j = 0; j < depth; j++)
    {
        BasicBlock::weight_t lastWeight = ambientWeight;
        ambientWeight *= BB_LOOP_WEIGHT_SCALE;
        assert(ambientWeight > lastWeight);
    }

    // If we're in a non-natural loop, the ambient weight might be higher than we computed above.
    // Be safe by taking the max with the head block's weight.
    ambientWeight = max(ambientWeight, loop.lpHead->bbWeight);

    // We assume that the fast path will run 99% of the time, and thus should get 99% of the block weights.
    // The slow path will, correspondingly, get only 1% of the block weights. It could be argued that we should
    // mark the slow path as "run rarely", since it really shouldn't execute (given the currently optimized loop
    // conditions) except under exceptional circumstances.
    const BasicBlock::weight_t fastPathWeightScaleFactor = 0.99f;
    const BasicBlock::weight_t slowPathWeightScaleFactor = 1.0f - fastPathWeightScaleFactor;

    // This is the containing loop, if any -- to label any blocks we create that are outside
    // the loop being cloned.
    unsigned char ambientLoop = loop.lpParent;

    // First, make sure that the loop has a unique header block, creating an empty one if necessary.
    compiler->optEnsureUniqueHead(loopNum, ambientWeight);

    // We're going to transform this loop:
    //
    // H --> E    (or, H conditionally branches around the loop and has fall-through to F == T == E)
    // F
    // T
    // E
    // B ?-> T
    // X
    //
    // to this pair of loops:
    //
    // H ?-> E2
    // H2--> E    (Optional; if E == T == F, let H fall through to F/T/E)
    // F
    // T
    // E
    // B  ?-> T
    // X2--> X
    // F2
    // T2
    // E2
    // B2 ?-> T2
    // X

    BasicBlock* h = loop.lpHead;
    if (h->bbJumpKind != BBJ_NONE && h->bbJumpKind != BBJ_ALWAYS)
    {
        // Make a new block to be the unique entry to the loop.
        JITDUMP("Create new unique single-successor entry to loop\n");
        assert((h->bbJumpKind == BBJ_COND) && (h->bbNext == loop.lpEntry));
        BasicBlock* newH = compiler->fgNewBBafter(BBJ_NONE, h, /*extendRegion*/ true);
        JITDUMP("Adding " FMT_BB " after " FMT_BB "\n", newH->bbNum, h->bbNum);
        newH->bbWeight = newH->isRunRarely() ? BB_ZERO_WEIGHT : ambientWeight;
        BlockSetOps::Assign(compiler, newH->bbReach, h->bbReach);
        // This is in the scope of a surrounding loop, if one exists -- the parent of the loop we're cloning.
        newH->bbNatLoopNum = ambientLoop;
        compiler->optUpdateLoopHead(loopNum, h, newH);

        compiler->fgAddRefPred(newH, h); // Add h->newH pred edge
        JITDUMP("Adding " FMT_BB " -> " FMT_BB "\n", h->bbNum, newH->bbNum);
        compiler->fgReplacePred(newH->bbNext, h, newH); // Replace pred in COND fall-through block.
        JITDUMP("Replace " FMT_BB " -> " FMT_BB " with " FMT_BB " -> " FMT_BB "\n", h->bbNum, newH->bbNext->bbNum,
                newH->bbNum, newH->bbNext->bbNum);

        h = newH;
    }
    assert(h == loop.lpHead);

    // Make X2 after B, if necessary.  (Not necessary if B is a BBJ_ALWAYS.)
    // "newPred" will be the predecessor of the blocks of the cloned loop.
    BasicBlock* b       = loop.lpBottom;
    BasicBlock* newPred = b;
    if (b->bbJumpKind != BBJ_ALWAYS)
    {
        assert(b->bbJumpKind == BBJ_COND);

        BasicBlock* x = b->bbNext;
        if (x != nullptr)
        {
            JITDUMP("Create branch around cloned loop\n");
            BasicBlock* x2 = compiler->fgNewBBafter(BBJ_ALWAYS, b, /*extendRegion*/ true);
            JITDUMP("Adding " FMT_BB " after " FMT_BB "\n", x2->bbNum, b->bbNum);
            x2->bbWeight = x2->isRunRarely() ? BB_ZERO_WEIGHT : ambientWeight;

            // This is in the scope of a surrounding loop, if one exists -- the parent of the loop we're cloning.
            x2->bbNatLoopNum = ambientLoop;

            x2->bbJumpDest = x;
            BlockSetOps::Assign(compiler, x2->bbReach, h->bbReach);

            compiler->fgAddRefPred(x2, b); // Add b->x2 pred edge
            JITDUMP("Adding " FMT_BB " -> " FMT_BB "\n", b->bbNum, x2->bbNum);
            compiler->fgReplacePred(x, b, x2); // The pred of x is now x2, not the fall-through of COND b.
            JITDUMP("Replace " FMT_BB " -> " FMT_BB " with " FMT_BB " -> " FMT_BB "\n", b->bbNum, x->bbNum, x2->bbNum,
                    x->bbNum);

            newPred = x2;
        }
    }

    // Now we'll make "h2", after "h" to go to "e" -- unless the loop is a do-while,
    // so that "h" already falls through to "e" (e == t == f).
    // It might look like this code is unreachable, since "h" must be a BBJ_ALWAYS, but
    // later we will change "h" to a BBJ_COND along with a set of loop conditions.
    // TODO: it still might be unreachable, since cloning currently is restricted to "do-while" loop forms.
    BasicBlock* h2 = nullptr;
    if (h->bbNext != loop.lpEntry)
    {
        assert(h->bbJumpKind == BBJ_ALWAYS);
        JITDUMP("Create branch to entry of optimized loop\n");
        BasicBlock* h2 = compiler->fgNewBBafter(BBJ_ALWAYS, h, /*extendRegion*/ true);
        JITDUMP("Adding " FMT_BB " after " FMT_BB "\n", h2->bbNum, h->bbNum);
        h2->bbWeight = h2->isRunRarely() ? BB_ZERO_WEIGHT : ambientWeight;

        // This is in the scope of a surrounding loop, if one exists -- the parent of the loop we're cloning.
        h2->bbNatLoopNum = ambientLoop;

        h2->bbJumpDest = loop.lpEntry;

        compiler->fgAddRefPred(h2, h); // Add h->h2 pred edge
        JITDUMP("Adding " FMT_BB " -> " FMT_BB "\n", h->bbNum, h2->bbNum);
        compiler->fgReplacePred(loop.lpEntry, h, h2);
        JITDUMP("Replace " FMT_BB " -> " FMT_BB " with " FMT_BB " -> " FMT_BB "\n", h->bbNum, loop.lpEntry->bbNum,
                h2->bbNum, loop.lpEntry->bbNum);

        compiler->optUpdateLoopHead(loopNum, h, h2);

        // NOTE: 'h' is no longer the loop head; 'h2' is!
    }

    // Now we'll clone the blocks of the loop body. These cloned blocks will be the slow path.
    BasicBlock* newFirst = nullptr;

    BlockToBlockMap* blockMap = new (alloc) BlockToBlockMap(alloc);
    for (BasicBlock* const blk : loop.LoopBlocks())
    {
        BasicBlock* newBlk = compiler->fgNewBBafter(blk->bbJumpKind, newPred, /*extendRegion*/ true);
        JITDUMP("Adding " FMT_BB " (copy of " FMT_BB ") after " FMT_BB "\n", newBlk->bbNum, blk->bbNum, newPred->bbNum);

        // Call CloneBlockState to make a copy of the block's statements (and attributes), and assert that it
        // has a return value indicating success, because optCanOptimizeByLoopCloningVisitor has already
        // checked them to guarantee they are clonable.
        bool cloneOk = BasicBlock::CloneBlockState(compiler, newBlk, blk);
        noway_assert(cloneOk);

        // We're going to create the preds below, which will set the bbRefs properly,
        // so clear out the cloned bbRefs field.
        newBlk->bbRefs = 0;

        newBlk->scaleBBWeight(slowPathWeightScaleFactor);
        blk->scaleBBWeight(fastPathWeightScaleFactor);

// TODO: scale the pred edges of `blk`?

#if FEATURE_LOOP_ALIGN
        // If the original loop is aligned, do not align the cloned loop because cloned loop will be executed in
        // rare scenario. Additionally, having to align cloned loop will force us to disable some VEX prefix encoding
        // and adding compensation for over-estimated instructions.
        if (blk->isLoopAlign())
        {
            newBlk->bbFlags &= ~BBF_LOOP_ALIGN;
            JITDUMP("Removing LOOP_ALIGN flag from cloned loop in " FMT_BB "\n", newBlk->bbNum);
        }
#endif

        // TODO-Cleanup: The above clones the bbNatLoopNum, which is incorrect.  Eventually, we should probably insert
        // the cloned loop in the loop table.  For now, however, we'll just make these blocks be part of the surrounding
        // loop, if one exists -- the parent of the loop we're cloning.
        newBlk->bbNatLoopNum = loop.lpParent;

        if (newFirst == nullptr)
        {
            newFirst = newBlk;
        }
        newPred = newBlk;
        blockMap->Set(blk, newBlk);
    }

    // Perform the static optimizations on the fast path.
    PerformStaticOptimizations(loopNum);

    // Now go through the new blocks, remapping their jump targets within the loop
    // and updating the preds lists.
    for (BasicBlock* const blk : loop.LoopBlocks())
    {
        BasicBlock* newblk = nullptr;
        bool        b      = blockMap->Lookup(blk, &newblk);
        assert(b && newblk != nullptr);

        assert(blk->bbJumpKind == newblk->bbJumpKind);

        // First copy the jump destination(s) from "blk".
        compiler->optCopyBlkDest(blk, newblk);

        // Now redirect the new block according to "blockMap".
        compiler->optRedirectBlock(newblk, blockMap);

        // Add predecessor edges for the new successors, as well as the fall-through paths.
        switch (newblk->bbJumpKind)
        {
            case BBJ_NONE:
                compiler->fgAddRefPred(newblk->bbNext, newblk);
                break;

            case BBJ_ALWAYS:
            case BBJ_CALLFINALLY:
                compiler->fgAddRefPred(newblk->bbJumpDest, newblk);
                break;

            case BBJ_COND:
                compiler->fgAddRefPred(newblk->bbNext, newblk);
                compiler->fgAddRefPred(newblk->bbJumpDest, newblk);
                break;

            case BBJ_SWITCH:
                for (BasicBlock* const switchDest : newblk->SwitchTargets())
                {
                    compiler->fgAddRefPred(switchDest, newblk);
                }
                break;

            default:
                break;
        }
    }

#ifdef DEBUG
    // Display the preds for the new blocks, after all the new blocks have been redirected.
    JITDUMP("Preds after loop copy:\n");
    for (BasicBlock* const blk : loop.LoopBlocks())
    {
        BasicBlock* newblk = nullptr;
        bool        b      = blockMap->Lookup(blk, &newblk);
        assert(b && newblk != nullptr);
        JITDUMP(FMT_BB ":", newblk->bbNum);
        for (BasicBlock* const predBlock : newblk->PredBlocks())
        {
            JITDUMP(" " FMT_BB, predBlock->bbNum);
        }
        JITDUMP("\n");
    }
#endif // DEBUG

    // We will create the following structure
    //
    // cond0 (in h)  -?> cond1
    // slow          --> e2 (slow) always
    // !cond1        -?> slow
    // !cond2        -?> slow
    // ...
    // !condn        -?> slow
    // h2/entry (fast)
    //
    // We should always have block conditions, at the minimum, the array should be deref-able
    assert(HasBlockConditions(loopNum));

    if (h->bbJumpKind == BBJ_NONE)
    {
        assert((h->bbNext == h2) || (h->bbNext == loop.lpEntry));
    }
    else
    {
        assert(h->bbJumpKind == BBJ_ALWAYS);
        assert(h->bbJumpDest == loop.lpEntry);
    }

    // If all the conditions are true, go to E2.
    BasicBlock* e2      = nullptr;
    bool        foundIt = blockMap->Lookup(loop.lpEntry, &e2);

    // We're going to replace the fall-through path from "h".
    if (h->bbJumpKind == BBJ_NONE)
    {
        compiler->fgRemoveRefPred(h->bbNext, h);
    }

    // Create a unique header for the slow path.
    JITDUMP("Create unique head block for slow path loop\n");
    BasicBlock* slowHead = compiler->fgNewBBafter(BBJ_ALWAYS, h, /*extendRegion*/ true);
    JITDUMP("Adding " FMT_BB " after " FMT_BB "\n", slowHead->bbNum, h->bbNum);
    slowHead->bbWeight = h->isRunRarely() ? BB_ZERO_WEIGHT : ambientWeight;
    slowHead->scaleBBWeight(slowPathWeightScaleFactor);
    slowHead->bbNatLoopNum = ambientLoop;
    slowHead->bbJumpDest   = e2;

    compiler->fgAddRefPred(slowHead, h);
    JITDUMP("Adding " FMT_BB " -> " FMT_BB "\n", h->bbNum, slowHead->bbNum);

    // This is the only predecessor to the copied loop, and it hasn't been added yet.
    compiler->fgAddRefPred(slowHead->bbJumpDest, slowHead);
    JITDUMP("Adding " FMT_BB " -> " FMT_BB "\n", slowHead->bbNum, slowHead->bbJumpDest->bbNum);

    // "h" is now going to be a COND block
    h->bbJumpKind = BBJ_COND;

    BasicBlock* condLast = InsertLoopChoiceConditions(loopNum, h, slowHead);
    condLast->bbJumpDest = slowHead;

    JITDUMP("Adding " FMT_BB " -> " FMT_BB "\n", condLast->bbNum, condLast->bbJumpDest->bbNum);
    compiler->fgAddRefPred(condLast->bbJumpDest, condLast);

    // Add the fall-through path pred.
    assert(condLast->bbJumpKind == BBJ_COND);
    JITDUMP("Adding " FMT_BB " -> " FMT_BB "\n", condLast->bbNum, condLast->bbNext->bbNum);
    compiler->fgAddRefPred(condLast->bbNext, condLast);

    // If h2 is present it is already the head or replace 'h' by 'condLast'.
    if (h2 == nullptr)
    {
        compiler->optUpdateLoopHead(loopNum, loop.lpHead, condLast);
    }
    assert(foundIt && e2 != nullptr);

    // Don't unroll loops that we've cloned -- the unroller expects any loop it should unroll to
    // initialize the loop counter immediately before entering the loop, but we've left a shared
    // initialization of the loop counter up above the test that determines which version of the
    // loop to take.
    loop.lpFlags |= LPFLG_DONT_UNROLL;
}

enum CallInterf
{
    CALLINT_NONE,       // no interference                               (most helpers)
    CALLINT_REF_INDIRS, // kills GC ref indirections                     (SETFIELD OBJ)
    CALLINT_SCL_INDIRS, // kills non GC ref indirections                 (SETFIELD non-OBJ)
    CALLINT_ALL_INDIRS, // kills both GC ref and non GC ref indirections (SETFIELD STRUCT)
    CALLINT_ALL,        // kills everything                              (normal method call)
};

enum varRefKinds
{
    VR_NONE    = 0x00,
    VR_IND_REF = 0x01, // an object reference
    VR_IND_SCL = 0x02, // a non-object reference
    VR_GLB_VAR = 0x04, // a global (clsVar)
};

struct LoopCloneVisitorInfo
{
    LoopCloneContext& context;
    const LoopDsc&    loop;
    const unsigned    loopNum;
    Statement*        stmt           = nullptr;
    BasicBlock*       block          = nullptr;
    bool              hasLoopSummary = false;
    ALLVARSET_TP      lpAsgVars; // set of vars assigned within the loop (all vars, not just tracked)
    CallInterf        lpAsgCall; // "callInterf" for calls in the loop
    varRefKinds       lpAsgInds; // set of inds modified within the loop

    LoopCloneVisitorInfo(LoopCloneContext& context, const LoopDsc& loop, unsigned loopNum)
        : context(context), loop(loop), loopNum(loopNum)
    {
    }

    void AddArrayIndex(GenTree* tree, ArrIndex& index);
    bool IsLclLoopInvariant(unsigned lclNum);
    bool IsLclAssignedInLoop(unsigned lclNum);
    bool IsTrackedLclAssignedInLoop(ALLVARSET_VALARG_TP vars);
    void IsLclAssignedVisitor(GenTree* node);
};

bool LoopCloneVisitorInfo::IsLclLoopInvariant(unsigned lclNum)
{
    return !context.compiler->lvaGetDesc(lclNum)->IsAddressExposed() && !IsLclAssignedInLoop(lclNum);
}

bool LoopCloneVisitorInfo::IsLclAssignedInLoop(unsigned lclNum)
{
    if (lclNum < lclMAX_ALLSET_TRACKED)
    {
        return IsTrackedLclAssignedInLoop(AllVarSetOps::MakeSingleton(context.compiler, lclNum)) != 0;
    }

    return context.compiler->optIsVarAssigned(loop.lpHead->bbNext, loop.lpBottom, nullptr, lclNum);
}

bool LoopCloneVisitorInfo::IsTrackedLclAssignedInLoop(ALLVARSET_VALARG_TP vars)
{
    if (!hasLoopSummary)
    {
        lpAsgVars = AllVarSetOps::MakeEmpty(context.compiler);
        lpAsgInds = VR_NONE;
        lpAsgCall = CALLINT_NONE;

        for (BasicBlock* block : loop.LoopBlocks())
        {
            for (Statement* stmt : block->Statements())
            {
                context.compiler->fgWalkTreePre(stmt->GetRootNodePointer(),
                                                [](GenTree** use, Compiler::fgWalkData* data) {
                                                    static_cast<LoopCloneVisitorInfo*>(data->pCallbackData)
                                                        ->IsLclAssignedVisitor(*use);
                                                    return Compiler::WALK_CONTINUE;
                                                },
                                                this);
            }
        }

        hasLoopSummary = true;
    }

    if (!AllVarSetOps::IsEmptyIntersection(context.compiler, lpAsgVars, vars))
    {
        return true;
    }

    switch (lpAsgCall)
    {
        case CALLINT_ALL:
            return lpAsgInds != VR_NONE;
        case CALLINT_REF_INDIRS:
            return (lpAsgInds & VR_IND_REF) != 0;
        case CALLINT_SCL_INDIRS:
            return (lpAsgInds & VR_IND_SCL) != 0;
        case CALLINT_ALL_INDIRS:
            return (lpAsgInds & (VR_IND_REF | VR_IND_SCL)) != 0;
        case CALLINT_NONE:
            return false;
        default:
            unreached();
    }
}

static CallInterf optCallInterf(GenTreeCall* call)
{
    // if not a helper, kills everything
    if (call->gtCallType != CT_HELPER)
    {
        return CALLINT_ALL;
    }

    // setfield and array address store kill all indirections
    switch (Compiler::eeGetHelperNum(call->GetMethodHandle()))
    {
        case CORINFO_HELP_ASSIGN_REF:         // Not strictly needed as we don't make a GT_CALL with this
        case CORINFO_HELP_CHECKED_ASSIGN_REF: // Not strictly needed as we don't make a GT_CALL with this
        case CORINFO_HELP_ASSIGN_BYREF:       // Not strictly needed as we don't make a GT_CALL with this
        case CORINFO_HELP_SETFIELDOBJ:
        case CORINFO_HELP_ARRADDR_ST:
            return CALLINT_REF_INDIRS;

        case CORINFO_HELP_SETFIELDFLOAT:
        case CORINFO_HELP_SETFIELDDOUBLE:
        case CORINFO_HELP_SETFIELD8:
        case CORINFO_HELP_SETFIELD16:
        case CORINFO_HELP_SETFIELD32:
        case CORINFO_HELP_SETFIELD64:
            return CALLINT_SCL_INDIRS;

        case CORINFO_HELP_ASSIGN_STRUCT: // Not strictly needed as we don't use this
        case CORINFO_HELP_MEMSET:        // Not strictly needed as we don't make a GT_CALL with this
        case CORINFO_HELP_MEMCPY:        // Not strictly needed as we don't make a GT_CALL with this
        case CORINFO_HELP_SETFIELDSTRUCT:
            return CALLINT_ALL_INDIRS;

        default:
            return CALLINT_NONE;
    }
}

void LoopCloneVisitorInfo::IsLclAssignedVisitor(GenTree* tree)
{
    if (tree->OperIs(GT_ASG))
    {
        GenTree* dest = tree->AsOp()->GetOp(0);

        if (dest->OperIs(GT_LCL_VAR))
        {
            unsigned lclNum = dest->AsLclVar()->GetLclNum();

            if (lclNum < lclMAX_ALLSET_TRACKED)
            {
                AllVarSetOps::AddElemD(context.compiler, lpAsgVars, lclNum);
            }
        }
        else if (dest->OperIs(GT_LCL_FLD))
        {
            // TODO-MIKE-Cleanup: This stuff is bonkers. It should simply be treated like LCL_VAR above.
            // Also, the LCL_VAR code is missing the promoted local case...

            if (!tree->TypeIs(TYP_STRUCT))
            {
                varRefKinds refs = varTypeIsGC(tree->GetType()) ? VR_IND_REF : VR_IND_SCL;
                lpAsgInds        = static_cast<varRefKinds>(lpAsgInds | refs);
            }
        }
        else if (dest->OperIs(GT_IND))
        {
            if (dest->AsIndir()->GetAddr()->OperIs(GT_CLS_VAR_ADDR))
            {
                lpAsgInds = static_cast<varRefKinds>(lpAsgInds | VR_GLB_VAR);
            }
            else
            {
                varRefKinds refs = varTypeIsGC(tree->GetType()) ? VR_IND_REF : VR_IND_SCL;
                lpAsgInds        = static_cast<varRefKinds>(lpAsgInds | refs);
            }
        }
    }
    else if (tree->OperIs(GT_CALL))
    {
        lpAsgCall = optCallInterf(tree->AsCall());
    }
}

bool LoopCloneContext::ExtractArrIndex(GenTree* tree, ArrIndex* result, unsigned tempArrayLclNum)
{
    if (!tree->OperIs(GT_COMMA))
    {
        return false;
    }

    GenTreeBoundsChk* boundsCheck = tree->AsOp()->GetOp(0)->IsBoundsChk();

    if (boundsCheck == nullptr)
    {
        return false;
    }

    GenTreeArrLen* length = boundsCheck->GetLength()->IsArrLen();
    GenTree*       index  = boundsCheck->GetIndex();

    if (length == nullptr)
    {
        return false;
    }

    if (!index->OperIs(GT_LCL_VAR) || !length->GetArray()->OperIs(GT_LCL_VAR))
    {
        return false;
    }

    unsigned arrayLclNum = length->GetArray()->AsLclVar()->GetLclNum();
    unsigned indexLclNum = index->AsLclVar()->GetLclNum();

    if (tempArrayLclNum == BAD_VAR_NUM)
    {
        result->arrLcl = arrayLclNum;
    }
    else if (arrayLclNum != tempArrayLclNum)
    {
        return false;
    }

    result->indLcls.Add(indexLclNum);
    result->bndsChks.Add(tree->AsOp());
    result->rank++;

    return true;
}

bool LoopCloneContext::ReconstructArrIndex(GenTree* tree, ArrIndex* result, unsigned lhsNum)
{
    if (ExtractArrIndex(tree, result, lhsNum))
    {
        return true;
    }

    if (tree->OperIs(GT_COMMA))
    {
        GenTree* asg = tree->AsOp()->GetOp(0);

        if (!asg->OperIs(GT_ASG))
        {
            return false;
        }

        GenTree* dst = asg->AsOp()->GetOp(0);
        GenTree* src = asg->AsOp()->GetOp(1);

        return dst->OperIs(GT_LCL_VAR) && ReconstructArrIndex(src, result, lhsNum) &&
               ExtractArrIndex(tree->AsOp()->GetOp(1), result, dst->AsLclVar()->GetLclNum());
    }

    return false;
}

Compiler::fgWalkResult LoopCloneContext::CanOptimizeByLoopCloning(GenTree* tree, LoopCloneVisitorInfo& info)
{
    ArrIndex arrIndex(alloc);

    if (ReconstructArrIndex(tree, &arrIndex, BAD_VAR_NUM))
    {
        info.AddArrayIndex(tree, arrIndex);
    }

    return Compiler::WALK_CONTINUE;
}

void LoopCloneVisitorInfo::AddArrayIndex(GenTree* tree, ArrIndex& arrIndex)
{
    assert(tree->OperIs(GT_COMMA));

#ifdef DEBUG
    if (context.verbose)
    {
        printf("Found ArrIndex at " FMT_BB " " FMT_STMT " tree [%06u] which is equivalent to: ", block->bbNum,
               stmt->GetID(), tree->GetID());
        arrIndex.Print();
        printf(", bounds check nodes: ");
        arrIndex.PrintBoundsCheckNodes();
        printf("\n");
    }
#endif

    if (!IsLclLoopInvariant(arrIndex.arrLcl))
    {
        JITDUMP("V%02u is not loop invariant\n", arrIndex.arrLcl);
        return;
    }

    for (unsigned dim = 0; dim < arrIndex.rank; ++dim)
    {
        if (arrIndex.indLcls[dim] != loop.lpIterVar())
        {
            JITDUMP("Induction V%02u is not used as index on dim %u\n", loop.lpIterVar(), dim);
            continue;
        }

        for (unsigned dim2 = 0; dim2 < dim; ++dim2)
        {
            if (IsLclAssignedInLoop(arrIndex.indLcls[dim2]))
            {
                JITDUMP("V%02u is assigned in loop\n", arrIndex.indLcls[dim2]);
                return;
            }
        }

#ifdef DEBUG
        if (context.verbose)
        {
            printf("Loop " FMT_LP " can be cloned for ArrIndex ", loopNum);
            arrIndex.Print();
            printf(" on dim %u\n", dim);
        }
#endif

        context.AddArrayIndex(loopNum, arrIndex, dim, stmt);
    }
}

void LoopCloneContext::IdentifyLoopOptInfo(unsigned loopNum)
{
    JITDUMP("Checking loop for optimization candidates\n");

    const LoopDsc&       loop = loopTable[loopNum];
    LoopCloneVisitorInfo info(*this, loop, loopNum);

    for (BasicBlock* block : loop.LoopBlocks())
    {
        info.block = block;

        for (Statement* stmt : block->Statements())
        {
            info.stmt = stmt;

            compiler->fgWalkTreePre(stmt->GetRootNodePointer(),
                                    [](GenTree** use, Compiler::fgWalkData* data) {
                                        LoopCloneVisitorInfo* info =
                                            static_cast<LoopCloneVisitorInfo*>(data->pCallbackData);
                                        return info->context.CanOptimizeByLoopCloning(*use, *info);
                                    },
                                    &info);
        }
    }
}

bool LoopCloneContext::Run()
{
    bool hasClonableLoops = false;

    for (unsigned i = 0; i < loopCount; i++)
    {
        JITDUMP("Considering loop " FMT_LP " to clone for optimizations.\n", i);

        if (IsLoopClonable(i))
        {
            IdentifyLoopOptInfo(i);
            hasClonableLoops = true;
        }

        JITDUMP("------------------------------------------------------------\n");
    }

    JITDUMP("\n");

    if (!hasClonableLoops)
    {
        JITDUMP("No clonable loops\n");
        return false;
    }

    unsigned staticallyOptimizedLoops = 0;

    for (unsigned i = 0; i < loopCount; ++i)
    {
        JitVector<LcOptInfo*>* optInfos = GetLoopOptInfo(i);

        if (optInfos == nullptr)
        {
            continue;
        }

        if (!DeriveLoopCloningConditions(i))
        {
            JITDUMP("Conditions could not be obtained\n");
            CancelLoopOptInfo(i);
            continue;
        }

        bool allTrue  = false;
        bool anyFalse = false;
        EvaluateConditions(i, &allTrue, &anyFalse);

        if (anyFalse)
        {
            CancelLoopOptInfo(i);
            continue;
        }

        if (allTrue)
        {
            PerformStaticOptimizations(i);
            staticallyOptimizedLoops++;
            CancelLoopOptInfo(i);
            continue;
        }
    }

    unsigned clonedLoopCount = 0;

    for (unsigned i = 0; i < loopCount; ++i)
    {
        if (GetLoopOptInfo(i) != nullptr)
        {
            OptimizeConditions(i);
            OptimizeBlockConditions(i);
            CloneLoop(i);
            clonedLoopCount++;
        }
    }

    if (clonedLoopCount > 0)
    {
        JITDUMP("Recompute reachability and dominators after loop cloning\n");
        compiler->fgUpdateChangedFlowGraph(/*computePreds*/ false);
        compiler->fgComputeDoms();
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("Loops cloned: %u\n", clonedLoopCount);
        printf("Loops statically optimized: %u\n", staticallyOptimizedLoops);
    }

    compiler->fgDebugCheckLoopTable();
#endif

    compiler->optLoopsCloned = clonedLoopCount;
    return staticallyOptimizedLoops + clonedLoopCount != 0;
}

PhaseStatus Compiler::phCloneLoops()
{
    if (optLoopCount == 0)
    {
        JITDUMP("No loops to clone\n");
        return PhaseStatus::MODIFIED_NOTHING;
    }

#ifdef DEBUG
    if (JitConfig.JitCloneLoops() == 0)
    {
        JITDUMP("Loop cloning disabled\n");
        return PhaseStatus::MODIFIED_NOTHING;
    }
#endif

    LoopCloneContext context(this);
    return context.Run() ? PhaseStatus::MODIFIED_EVERYTHING : PhaseStatus::MODIFIED_NOTHING;
}
