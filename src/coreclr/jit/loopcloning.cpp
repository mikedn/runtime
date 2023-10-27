// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

struct ArrIndex
{
    unsigned   arrayLclNum = BAD_VAR_NUM;
    unsigned   indexLclNum = BAD_VAR_NUM;
    GenTreeOp* boundsCheck = nullptr;

#ifdef DEBUG
    void Print() const
    {
        printf("V%02u[V%02u]", arrayLclNum, indexLclNum);
    }

    void PrintBoundsCheckNodes() const
    {
        printf("[%06u]", boundsCheck->GetID());
    }
#endif
};

// TODO-MIKE-Consider: Old code attempted to optimize "jagged" arrays but it was beyond broken.
// Optimizing a[i][j] via loop cloning requires inserting "limit < a[i].Length" checks before
// the loop and in turn that requires a[i] loop memory invariance checks. Old code only did
// some nonsensical memory store checks, which it then ignored anyway. And even if a[i] does
// not change within the loop, it still could be changed by a different thread, so the only way
// to do this optimization in a thread safe manner is to actually hoist a[i] out of the loop.
// And having an a[i].Length IV limit will probably add more gas to the fire, as that too will
// need to be hoisted.
// On top of this, the old code didn't account for loop nests and happily cloned a[i][j] just
// because i is loop invariant, not because i is the IV of the parent loop. That's not wrong
// but then if we bother to deal with an invariant a[i] why not also deal with an invariant
// class object field? We more or less need to do the same interference checks and hoisting.
// Ideally loop cloning would run after loop hoisting, but that's not quite the same thing
// as loop cloning doing its own hoisting because the current loop hoisting won't hoist loop
// code that's not guaranteed to execute, while loop cloning doesn't care about any control
// flow within the loop.
// Anyway, there were only 2-3 cases of a[i][j] in the entire FX diff so it's far from obvious
// that dealing with it is worthwhile.
struct LcOptInfo
{
    enum Kind
    {
        SZArray
    };

    const Kind kind;

    LcOptInfo(Kind kind) : kind(kind)
    {
    }
};

struct LcSZArrayOptInfo : public LcOptInfo
{
    ArrIndex   arrIndex;
    Statement* stmt;

    LcSZArrayOptInfo(const ArrIndex& arrIndex, Statement* stmt) : LcOptInfo(SZArray), arrIndex(arrIndex), stmt(stmt)
    {
    }
};

// Symbolic representation of a.Length.
// Oper decides whether "ARR_LENGTH" is invoked on the array or if it is just an array.
struct LcArray
{
    enum Kind : uint8_t
    {
        Invalid,
        SZArray
    };

    enum Oper : uint8_t
    {
        None,
        ArrLen,
    };

    Kind     kind;
    Oper     oper;
    unsigned lclNum;

    LcArray() : kind(Invalid)
    {
    }

    LcArray(Kind kind, unsigned lclNum, Oper oper) : kind(kind), oper(oper), lclNum(lclNum)
    {
    }

    bool operator==(const LcArray& that) const
    {
        assert((kind != Invalid) && (that.kind != Invalid));

        return (kind == that.kind) && (oper == that.oper) && (lclNum == that.lclNum);
    }

    GenTree* ToGenTree(Compiler* comp) const;

#ifdef DEBUG
    void Print() const
    {
        printf("V%02u", lclNum);

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

    GenTree* ToGenTree(Compiler* comp) const;

#ifdef DEBUG
    void Print() const
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

    GenTree* ToGenTree(Compiler* comp) const;

#ifdef DEBUG
    void Print() const
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
        assert(GenTree::OperIsCompare(oper));
    }

    bool Evaluate(bool* result) const;
    bool operator==(const LcCondition& cond) const;

    GenTree* ToGenTree(Compiler* comp) const;

#ifdef DEBUG
    void Print() const
    {
        printf("%s ", GenTree::OpName(oper));
        op1.Print();
        printf(", ");
        op2.Print();
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
    BitVecTraits  modifiedLocalsTraits;
    BitVec        modifiedLocals = nullptr;
    // The array of optimization opportunities found in each loop. (loop x optimization-opportunities)
    jitstd::vector<JitVector<LcOptInfo*>*> optInfo;
    // The array of conditions that influence which path to take for each loop. (loop x cloning-conditions)
    jitstd::vector<JitVector<LcCondition>*> conditions;
    // The array of block levels of conditions for each loop. (loop x level x conditions)
    jitstd::vector<JitVector<LcCondition>*> blockConditions;

    LoopCloneContext(Compiler* compiler)
        : compiler(compiler)
        , loopTable(compiler->optLoopTable)
        , loopCount(compiler->optLoopCount)
#ifdef DEBUG
        , verbose(compiler->verbose)
#endif
        , alloc(compiler->getAllocator(CMK_LoopClone))
        , modifiedLocalsTraits(compiler->lvaCount, compiler)
        , optInfo(alloc)
        , conditions(alloc)
        , blockConditions(alloc)
    {
        optInfo.resize(loopCount, nullptr);
        conditions.resize(loopCount, nullptr);
        blockConditions.resize(loopCount, nullptr);
    }

    void CondToStmtInBlock(JitVector<LcCondition>& conds, BasicBlock* block, bool reverse) const;
    void EvaluateConditions(unsigned loopNum, bool* pAllTrue, bool* pAnyFalse);
    void OptimizeConditions(JitVector<LcCondition>& conds);
    void OptimizeConditions(unsigned loopNum);
    void OptimizeBlockConditions(unsigned loopNum);
    bool IsLoopClonable(unsigned loopNum) const;
    void IdentifyLoopOptInfo(unsigned loopNum);
    void PerformStaticOptimizations(unsigned loopNum);
    bool ComputeDerefConditions(const ArrayStack<unsigned>& derefs, unsigned loopNum);
    bool DeriveLoopCloningConditions(unsigned loopNum);
    BasicBlock* InsertLoopChoiceConditions(unsigned loopNum, BasicBlock* head, BasicBlock* slow);
    void CloneLoop(unsigned loopNum);
    bool Run();

    void AddArrayIndex(unsigned loopNum, const ArrIndex& index, Statement* stmt)
    {
        if (optInfo[loopNum] == nullptr)
        {
            optInfo[loopNum] = new (alloc) JitVector<LcOptInfo*>(alloc);
        }

        optInfo[loopNum]->Add(new (alloc) LcSZArrayOptInfo(index, stmt));
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

#ifdef DEBUG
    bool HasBlockConditions(unsigned loopNum) const
    {
        JitVector<LcCondition>* loopBlockConditions = blockConditions[loopNum];

        return (loopBlockConditions != nullptr) && !loopBlockConditions->Empty();
    }

    void PrintBlockConditions(unsigned loopNum)
    {
        printf("Block conditions:\n");

        if (!HasBlockConditions(loopNum))
        {
            printf("No block conditions\n");
            return;
        }

        PrintBlockConditions(*blockConditions[loopNum]);
    }

    void PrintBlockConditions(const JitVector<LcCondition>& levelCond)
    {
        for (unsigned i = 0; i < levelCond.Size(); ++i)
        {
            if (i != 0)
            {
                printf(" & ");
            }

            levelCond[i].Print();
        }

        printf("\n");
    }

    void PrintConditions(unsigned loopNum) const;
#endif
};

GenTree* LcArray::ToGenTree(Compiler* comp) const
{
    assert(kind == SZArray);

    assert(comp->lvaGetDesc(lclNum)->TypeIs(TYP_REF));
    GenTree* array = comp->gtNewLclvNode(lclNum, TYP_REF);

    if (oper == ArrLen)
    {
        GenTree* arrLen = comp->gtNewArrLen(array, OFFSETOF__CORINFO_Array__length);

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

    return array;
}

GenTree* LcIdent::ToGenTree(Compiler* comp) const
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

GenTree* LcExpr::ToGenTree(Compiler* comp) const
{
    return ident.ToGenTree(comp);
}

GenTree* LcCondition::ToGenTree(Compiler* comp) const
{
    GenTree* op1Tree = op1.ToGenTree(comp);
    GenTree* op2Tree = op2.ToGenTree(comp);

    assert(varTypeSize(varActualType(op1Tree->GetType())) == varTypeSize(varActualType(op2Tree->GetType())));

    return comp->gtNewOperNode(oper, TYP_INT, op1Tree, op2Tree);
}

bool LcCondition::Evaluate(bool* result) const
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

    for (const LcCondition& cond : conds)
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("Considering condition: (");
            cond.Print();
        }
#endif

        bool res = false;

        if (cond.Evaluate(&res))
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
    OptimizeConditions(*blockConditions[loopNum]);

#ifdef DEBUG
    if (verbose)
    {
        printf("After optimizing block-level cloning conditions:\n");
        PrintBlockConditions(loopNum);
        printf("\n");
    }
#endif
}

void LoopCloneContext::OptimizeConditions(unsigned loopNum)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("Before optimizing cloning conditions:\n");
        PrintConditions(loopNum);
        printf("\n");
    }
#endif

    OptimizeConditions(*conditions[loopNum]);

#ifdef DEBUG
    if (verbose)
    {
        printf("After optimizing cloning conditions:\n");
        PrintConditions(loopNum);
        printf("\n");
    }
#endif
}

#ifdef DEBUG
void LoopCloneContext::PrintConditions(unsigned loopNum) const
{
    JitVector<LcCondition>* loopConditions = conditions[loopNum];

    if (loopConditions == nullptr)
    {
        printf("NO conditions");
        return;
    }

    if (loopConditions->Empty())
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

void LoopCloneContext::CondToStmtInBlock(JitVector<LcCondition>& conds, BasicBlock* block, bool reverse) const
{
    noway_assert(!conds.Empty());

    DBEXEC(verbose, conds);
    JITDUMP("\n");

    GenTree* cond = conds[0].ToGenTree(compiler);

    for (unsigned i = 1; i < conds.Size(); ++i)
    {
        cond = compiler->gtNewOperNode(GT_AND, TYP_INT, cond, conds[i].ToGenTree(compiler));
    }

    cond = compiler->gtNewOperNode(reverse ? GT_NE : GT_EQ, TYP_INT, cond, compiler->gtNewIconNode(0));

    GenTree*   jtrue = compiler->gtNewOperNode(GT_JTRUE, TYP_VOID, cond);
    Statement* stmt  = compiler->gtNewStmt(jtrue);

    compiler->fgInsertStmtAtEnd(block, stmt);

    JITDUMPTREE(jtrue, "Loop cloning condition tree before morphing:\n");
    JITDUMP("\n");

    bool removedStmt = compiler->fgMorphBlockStmt(block, stmt DEBUGARG("Loop cloning condition"));

    if (!removedStmt)
    {
        // TODO-MIKE-Review: This shouldn't be needed as we haven't run phSetBlockOrder yet.
        // But removing it causes a few diffs, it's likely that something depends on GTF_REVERSE_OPS
        // or that running multiple ordering passes results in a different order.
        compiler->gtSetOrder(stmt->GetRootNode());
    }
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

    const LoopDsc& loop = loopTable[loopNum];

    assert(loop.lpTestOper() == GT_LT);
    assert((loop.lpFlags & (LPFLG_CONST_INIT | LPFLG_VAR_INIT)) != 0);
    assert((loop.lpFlags & (LPFLG_CONST_LIMIT | LPFLG_VAR_LIMIT | LPFLG_ARRLEN_LIMIT)) != 0);
    assert(((loop.lpFlags & LPFLG_CONST_INIT) == 0) || (loop.lpIterConst() > 0));

    if ((loop.lpFlags & LPFLG_VAR_INIT) != 0)
    {
        EnsureConditions(loopNum)->Emplace(GT_GE, LcExpr(LcIdent(loop.lpVarInit, LcIdent::Lcl)),
                                           LcExpr(LcIdent(0, LcIdent::Const)));
    }

    LcIdent              limit;
    ArrayStack<unsigned> derefs(alloc);

    if ((loop.lpFlags & LPFLG_CONST_LIMIT) != 0)
    {
        assert(loop.lpConstLimit() > 0);

        limit = LcIdent(static_cast<unsigned>(loop.lpConstLimit()), LcIdent::Const);
    }
    else if ((loop.lpFlags & LPFLG_VAR_LIMIT) != 0)
    {
        limit = LcIdent(loop.lpVarLimit(), LcIdent::Lcl);

        EnsureConditions(loopNum)->Emplace(GT_GE, LcExpr(limit), LcExpr(LcIdent(0, LcIdent::Const)));
    }
    else
    {
        assert((loop.lpFlags & LPFLG_ARRLEN_LIMIT) != 0);

        GenTree* array = loop.lpLimit()->AsArrLen()->GetArray();

        if (!array->OperIs(GT_LCL_VAR))
        {
            JITDUMP("> ArrLen not matching");
            return false;
        }

        // TODO-MIKE-Review: Why do we check for null "a" in a "a.Length" limit if GTF_RELOP_ZTT is required?
        derefs.Emplace(array->AsLclVar()->GetLclNum());

        limit = LcIdent(LcArray(LcArray::SZArray, array->AsLclVar()->GetLclNum(), LcArray::ArrLen));
    }

    for (LcOptInfo* optInfo : *GetLoopOptInfo(loopNum))
    {
        assert(optInfo->kind == LcOptInfo::SZArray);
        LcSZArrayOptInfo* arrIndexInfo = static_cast<LcSZArrayOptInfo*>(optInfo);

        derefs.Emplace(arrIndexInfo->arrIndex.arrayLclNum);
        EnsureConditions(loopNum)->Emplace(GT_LE, LcExpr(limit),
                                           LcExpr(LcIdent(LcArray(LcArray::SZArray, arrIndexInfo->arrIndex.arrayLclNum,
                                                                  LcArray::ArrLen))));
    }

    JITDUMP("Conditions: ");
    DBEXEC(verbose, PrintConditions(loopNum));
    JITDUMP("\n");

    return derefs.Empty() || ComputeDerefConditions(derefs, loopNum);
}

bool LoopCloneContext::ComputeDerefConditions(const ArrayStack<unsigned>& arrays, unsigned loopNum)
{
    assert(!arrays.Empty());
    assert(blockConditions[loopNum] == nullptr);

    JitVector<LcCondition>* conditions = new (alloc) JitVector<LcCondition>(alloc);
    ArrayStack<unsigned>    uniqueArrays(alloc);

    for (unsigned arrayLclNum : arrays)
    {
        bool alreadyAdded = false;

        for (unsigned lclNum : uniqueArrays)
        {
            if (lclNum == arrayLclNum)
            {
                alreadyAdded = true;
                break;
            }
        }

        if (!alreadyAdded)
        {
            conditions->Emplace(GT_NE, LcExpr(LcIdent(arrayLclNum, LcIdent::Lcl)), LcExpr(LcIdent(LcIdent::Null)));
        }
    }

    blockConditions[loopNum] = conditions;

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

    for (LcOptInfo* optInfo : *optInfos)
    {
        assert(optInfo->kind == LcOptInfo::SZArray);
        LcSZArrayOptInfo* arrIndexInfo = static_cast<LcSZArrayOptInfo*>(optInfo);
        GenTreeOp*        comma        = arrIndexInfo->arrIndex.boundsCheck;

#ifdef DEBUG
        if (verbose)
        {
            printf("Remove bounds check [%06u]", comma->GetOp(0)->GetID());
            printf(" from " FMT_STMT ", ", arrIndexInfo->stmt->GetID());
            arrIndexInfo->arrIndex.Print();
            printf("\n");
        }
#endif

        // This COMMA node will only represent a bounds check if we've haven't already
        // removed this bounds check in some other nesting cloned loop.
        if (GenTreeBoundsChk* boundsChk = comma->GetOp(0)->IsBoundsChk())
        {
            compiler->optRemoveRangeCheck(boundsChk, comma, arrIndexInfo->stmt);
        }
        else
        {
            JITDUMP("Bounds check already removed\n");

            // If the bounds check node isn't there, it better have been converted to a NOP.
            assert(comma->GetOp(0)->OperIs(GT_NOP));
        }
    }
}

bool LoopCloneContext::IsLoopClonable(unsigned loopNum) const
{
    const LoopDsc& loop = loopTable[loopNum];

    if (loop.lpFlags & LPFLG_REMOVED)
    {
        JITDUMP("Rejecting loop. It is marked LPFLG_REMOVED.\n");
        return false;
    }

    if (loop.lpIterTree == nullptr)
    {
        JITDUMP("Rejecting loop. No iterator.\n");
        return false;
    }

    assert(!compiler->lvaGetDesc(loop.lpIterVar())->IsAddressExposed());

    if ((loop.lpFlags & (LPFLG_CONST_INIT | LPFLG_VAR_INIT)) == 0)
    {
        JITDUMP("Rejecting loop. IV init is neither constant or local.\n");
        return false;
    }

    if ((loop.lpFlags & (LPFLG_CONST_LIMIT | LPFLG_VAR_LIMIT | LPFLG_ARRLEN_LIMIT)) == 0)
    {
        JITDUMP("Rejecting loop. IV limit is neither constant, variable or array length.\n");
        return false;
    }

    if (((loop.lpFlags & LPFLG_CONST_INIT) != 0) && (loop.lpConstInit < 0))
    {
        JITDUMP("Rejecting loop. IV init value %d is invalid.\n", loop.lpConstInit);
        return false;
    }

    if ((loop.lpFlags & LPFLG_CONST_LIMIT) != 0)
    {
        if (loop.lpConstLimit() <= 0)
        {
            JITDUMP("Rejecting loop. IV limit value %d is invalid.\n", loop.lpConstLimit());
            return false;
        }
    }
    else if ((loop.lpFlags & LPFLG_VAR_LIMIT) != 0)
    {
        if (compiler->lvaGetDesc(loop.lpVarLimit())->IsAddressExposed())
        {
            JITDUMP("Rejecting loop. IV limit V%02u is address exposed.\n", loop.lpVarLimit());
            return false;
        }
    }

    if (loop.lpTestOper() != GT_LT)
    {
        JITDUMP("Rejecting loop. Unsupported test oper.\n");
        return false;
    }

    // TODO-CQ: Handle decreasing IVs. Also, step can be greater than 1 for arrays because
    // the maximum array length is less than INT_MAX.
    if ((loop.lpIterOper() != GT_ADD) || (loop.lpIterConst() != 1))
    {
        JITDUMP("Rejecting loop. Loop iteration operator not matching.\n");
        return false;
    }

    if (!loop.lpTestTree->OperIsCompare() || ((loop.lpTestTree->gtFlags & GTF_RELOP_ZTT) == 0))
    {
        JITDUMP("Rejecting loop. Loop inversion NOT present, loop test [%06u] may not protect entry from head.\n",
                loop.lpTestTree->GetID());
        return false;
    }

    BasicBlock* const head = loop.lpHead;
    BasicBlock* const end  = loop.lpBottom;
    BasicBlock* const beg  = head->bbNext;

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

    JITDUMP("Adding loop " FMT_LP " block conditions to " FMT_BB "\n    ", loopNum, head->bbNum);
    CondToStmtInBlock(*blockConditions[loopNum], head, /*reverse*/ true);

    BasicBlock* condBlock = compiler->fgNewBBafter(BBJ_COND, slowHead, /*extendRegion*/ true);
    JITDUMP("Adding loop " FMT_LP " cloning conditions to " FMT_BB "\n", loopNum, condBlock->bbNum);
    condBlock->inheritWeight(head);
    condBlock->bbNatLoopNum = head->bbNatLoopNum;
    head->bbJumpDest        = condBlock;
    JITDUMP("Adding " FMT_BB " -> " FMT_BB "\n", head->bbNum, condBlock->bbNum);
    compiler->fgAddRefPred(condBlock, head);

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

struct LoopCloneVisitorInfo
{
    LoopCloneContext& context;
    const LoopDsc&    loop;
    const unsigned    loopNum;
    Statement*        stmt           = nullptr;
    BasicBlock*       block          = nullptr;
    bool              hasLoopSummary = false;

    LoopCloneVisitorInfo(LoopCloneContext& context, const LoopDsc& loop, unsigned loopNum)
        : context(context), loop(loop), loopNum(loopNum)
    {
    }

    void ArrayIndexVisitor(GenTreeOp* comma);
    bool ExtractArrayIndex(GenTreeOp* comma, ArrIndex* result) const;
    void AddArrayIndex(const ArrIndex& index);
    bool IsLclLoopInvariant(unsigned lclNum);
    bool IsLclAssignedInLoop(unsigned lclNum);
    void SummarizeLocalStores(unsigned lclNum);
    void SummarizeLocalStoresVisitor(GenTreeLclVarCommon* store);
};

bool LoopCloneVisitorInfo::IsLclLoopInvariant(unsigned lclNum)
{
    return !context.compiler->lvaGetDesc(lclNum)->IsAddressExposed() && !IsLclAssignedInLoop(lclNum);
}

bool LoopCloneVisitorInfo::IsLclAssignedInLoop(unsigned lclNum)
{
    // We currently don't add any locals during loop cloning but in case it
    // happens just be conservative and treat any new locals as modified.
    if (lclNum >= BitVecTraits::GetSize(&context.modifiedLocalsTraits))
    {
        return true;
    }

    if (!hasLoopSummary)
    {
        SummarizeLocalStores(lclNum);
        hasLoopSummary = true;
    }

    return BitVecOps::IsMember(context.modifiedLocalsTraits, context.modifiedLocals, lclNum);
}

void LoopCloneVisitorInfo::SummarizeLocalStores(unsigned lclNum)
{
    assert(!hasLoopSummary);

    if (context.modifiedLocals == nullptr)
    {
        context.modifiedLocals = BitVecOps::MakeEmpty(context.modifiedLocalsTraits);
    }
    else
    {
        BitVecOps::ClearD(context.modifiedLocalsTraits, context.modifiedLocals);
    }

    for (BasicBlock* block : loop.LoopBlocks())
    {
        for (Statement* stmt : block->Statements())
        {
            context.compiler->fgWalkTreePre(stmt->GetRootNodePointer(),
                                            [](GenTree** use, Compiler::fgWalkData* data) {
                                                LoopCloneVisitorInfo* info =
                                                    static_cast<LoopCloneVisitorInfo*>(data->pCallbackData);
                                                GenTree* node = *use;

                                                if (node->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
                                                {
                                                    info->SummarizeLocalStoresVisitor(node->AsLclVarCommon());
                                                }

                                                return Compiler::WALK_CONTINUE;
                                            },
                                            this);
        }
    }
}

void LoopCloneVisitorInfo::SummarizeLocalStoresVisitor(GenTreeLclVarCommon* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    unsigned lclNum = store->AsLclVarCommon()->GetLclNum();

    // We currently don't add any locals during loop cloning but in case it
    // happens just be conservative and treat any new locals as modified.
    if (lclNum < BitVecTraits::GetSize(&context.modifiedLocalsTraits))
    {
        BitVecOps::AddElemD(context.modifiedLocalsTraits, context.modifiedLocals, lclNum);
    }

    // Assigning a promoted local modifies all its fields. This is somewhat conservative,
    // a LCL_FLD could modify only some of the fields. But that's rare, most of the time
    // LCL_FLDs are used only to load from promoted locals (mostly for ABI related stuff).
    //
    // Note that the opposite can also be true - we may have an assignment to a promoted
    // field, which implies that the parent struct is modified as well. But we only care
    // about scalar locals in loop cloning (INT indices and REF arrays) so we can ignore
    // this case.

    LclVarDsc* lcl = context.compiler->lvaGetDesc(lclNum);

    if (lcl->IsPromoted())
    {
        for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); i++)
        {
            unsigned fieldLclNum = lcl->GetPromotedFieldLclNum(i);

            if (fieldLclNum < BitVecTraits::GetSize(&context.modifiedLocalsTraits))
            {
                BitVecOps::AddElemD(context.modifiedLocalsTraits, context.modifiedLocals, fieldLclNum);
            }
        }
    }
}

bool LoopCloneVisitorInfo::ExtractArrayIndex(GenTreeOp* comma, ArrIndex* result) const
{
    assert(comma->OperIs(GT_COMMA));

    GenTreeBoundsChk* boundsCheck = comma->GetOp(0)->IsBoundsChk();

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

    result->arrayLclNum = length->GetArray()->AsLclVar()->GetLclNum();
    result->indexLclNum = index->AsLclVar()->GetLclNum();
    result->boundsCheck = comma;

    return true;
}

void LoopCloneVisitorInfo::AddArrayIndex(const ArrIndex& arrIndex)
{
    assert(arrIndex.boundsCheck->OperIs(GT_COMMA));

#ifdef DEBUG
    if (context.verbose)
    {
        printf("Found ArrIndex at " FMT_BB " " FMT_STMT " [%06u] which is equivalent to: ", block->bbNum, stmt->GetID(),
               arrIndex.boundsCheck->GetID());
        arrIndex.Print();
        printf(", bounds check node: ");
        arrIndex.PrintBoundsCheckNodes();
        printf("\n");
    }
#endif

    if (!IsLclLoopInvariant(arrIndex.arrayLclNum))
    {
        JITDUMP("Array V%02u is not loop invariant\n", arrIndex.arrayLclNum);
        return;
    }

    if (arrIndex.indexLclNum != loop.lpIterVar())
    {
        JITDUMP("Index V%02u is not loop IV\n", loop.lpIterVar());
        return;
    }

#ifdef DEBUG
    if (context.verbose)
    {
        printf("Loop " FMT_LP " can be cloned for ArrIndex ", loopNum);
        arrIndex.Print();
    }
#endif

    context.AddArrayIndex(loopNum, arrIndex, stmt);
}

void LoopCloneVisitorInfo::ArrayIndexVisitor(GenTreeOp* comma)
{
    ArrIndex arrIndex;

    if (ExtractArrayIndex(comma, &arrIndex))
    {
        AddArrayIndex(arrIndex);
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
                                        GenTree* comma = *use;

                                        if (comma->OperIs(GT_COMMA))
                                        {
                                            info->ArrayIndexVisitor(comma->AsOp());
                                        }

                                        return Compiler::WALK_CONTINUE;
                                    },
                                    &info);
        }
    }
}

bool LoopCloneContext::Run()
{
    for (unsigned i = 0; i < loopCount; i++)
    {
        JITDUMP("Considering loop " FMT_LP " to clone for optimizations.\n", i);

        if (IsLoopClonable(i))
        {
            IdentifyLoopOptInfo(i);
        }

        JITDUMP("------------------------------------------------------------\n");
    }

    JITDUMP("\n");

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
