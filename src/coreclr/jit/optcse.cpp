// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "jitstd/algorithm.h"

bool Compiler::cseIsCandidate(GenTree* node)
{
    if ((node->gtFlags & (GTF_ASG | GTF_DONT_CSE)) != 0)
    {
        return false;
    }

    if (((compCodeOpt() == SMALL_CODE) ? node->GetCostSz() : node->GetCostEx()) < MIN_CSE_COST)
    {
        return false;
    }

    if (node->TypeIs(TYP_VOID))
    {
        return false;
    }

    if (node->TypeIs(TYP_STRUCT) && !node->IsCall())
    {
        // Don't attempt to CSE expressions having non-enregistrable types (STRUCT),
        // unless they're calls (some helper calls can be CSEed and do return structs
        // - CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE).

        // TODO-MIKE-CQ: It may be useful to CSE a small STRUCT load (OBJ), doing so may
        // may shorten the live range of the address and thus decrease register pressure.
        // But it gets complicated if the code also contains loads from struct's fields.
        // It does not make sense to CSE those independently from the struct load itself
        // so we'd need to use the CSE temp created for the struct load. And for that to
        // work well we'd need to promote that temp, otherwise it would be in memory and
        // CSEed field loads would also be done from memory.
        return false;
    }

    switch (node->GetOper())
    {
        case GT_CALL:
            // TODO-MIKE-Review: How could 2 allocator helper calls get the same VN
            // so CSEing would be a possibility to begin with?!?
            // Maybe it happens with CORINFO_HELP_STRCNS but then that doesn't sound
            // like "allocation"...

            // Don't mark calls to allocation helpers as CSE candidates.
            // Marking them as CSE candidates usually blocks CSEs rather than enables them.
            // A typical case is:
            // [1] GT_IND(x) = GT_CALL ALLOC_HELPER
            // ...
            // [2] y = GT_IND(x)
            // ...
            // [3] z = GT_IND(x)
            // If we mark CALL ALLOC_HELPER as a CSE candidate, we later discover
            // that it can't be a CSE def because GT_INDs in [2] and [3] can cause
            // more exceptions (NullRef) so we abandon this CSE.
            // If we don't mark CALL ALLOC_HELPER as a CSE candidate, we are able
            // to use GT_IND(x) in [2] as a CSE def.
            if (node->IsHelperCall() &&
                s_helperCallProperties.IsAllocator(eeGetHelperNum(node->AsCall()->GetMethodHandle())))
            {
                return false;
            }

            // If we have a simple helper call with no other persistent side-effects
            // then we allow this tree to be a CSE candidate.
            return !gtTreeHasSideEffects(node, GTF_PERSISTENT_SIDE_EFFECTS | GTF_IS_IN_CSE);

        case GT_IND:
            // TODO-MIKE-Review: This comment doesn't make a lot of sense, it should
            // be possible to CSE both IND and ARR_ELEM...

            // We try to CSE GT_ARR_ELEM nodes instead of GT_IND(GT_ARR_ELEM).
            // Doing the first allows CSE to also kick in for code like
            // "GT_IND(GT_ARR_ELEM) = GT_IND(GT_ARR_ELEM) + xyz", whereas doing
            // the second would not allow it
            return !node->AsIndir()->GetAddr()->OperIs(GT_ARR_ELEM);

        case GT_ADD:
        case GT_LSH:
        case GT_MUL:
        case GT_COMMA:
            return (node->gtFlags & GTF_ADDRMODE_NO_CSE) == 0;

        case GT_LCL_VAR:
            // TODO-MIKE-Review: Huh? What volatile LCL_VAR?!?
            // In general it doesn't make sense to CSE a LCL_VAR. Though CSEing
            // a DNER local may be useful...
            // P.S. Probably this was referring to the lvVolatileHint crap...
            return false; // Can't CSE a volatile LCL_VAR

        case GT_FNEG:
        case GT_FADD:
        case GT_FSUB:
        case GT_FMUL:
        case GT_FDIV:
        case GT_ARR_ELEM:
        case GT_ARR_LENGTH:
        case GT_LCL_FLD:
        case GT_NEG:
        case GT_NOT:
        case GT_BSWAP:
        case GT_BSWAP16:
        case GT_CAST:
        case GT_BITCAST:
        case GT_SUB:
        case GT_DIV:
        case GT_MOD:
        case GT_UDIV:
        case GT_UMOD:
        case GT_OR:
        case GT_AND:
        case GT_XOR:
        case GT_RSH:
        case GT_RSZ:
        case GT_ROL:
        case GT_ROR:
        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GE:
        case GT_GT:
        case GT_INTRINSIC:
        case GT_OBJ:
#ifdef TARGET_64BIT
        case GT_CNS_LNG:
#endif
        case GT_CNS_INT:
        case GT_CNS_DBL:
            // TODO-MIKE-CQ: Might want to add CLS_VAR_ADDR to this, especially on ARM.
            return true;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            switch (HWIntrinsicInfo::lookupCategory(node->AsHWIntrinsic()->GetIntrinsic()))
            {
#ifdef TARGET_XARCH
                case HW_Category_SimpleSIMD:
                case HW_Category_IMM:
                case HW_Category_SIMDScalar:
#elif defined(TARGET_ARM64)
                case HW_Category_SIMD:
                case HW_Category_SIMDByIndexedElement:
                case HW_Category_ShiftLeftByImmediate:
                case HW_Category_ShiftRightByImmediate:
#endif
                case HW_Category_Scalar:
                case HW_Category_Helper:
                    return true;

                case HW_Category_MemoryLoad:
                case HW_Category_MemoryStore:
                case HW_Category_Special:
                default:
                    return false;
            }
#endif // FEATURE_HW_INTRINSICS

        default:
            return false;
    }
}

// We're using gtSetEvalOrder during CSE to restore the linear order of a statement after
// performing CSE. gtSetEvalOrder may attempt to swap the order of evaluation of subtrees
// and that's not possible if we have CSE uses in a subtree that depend on CSE defs in
// the other subtree.
// TODO-MIKE-Review: See if we can update linear order after CSE is done to avoid this.
bool Compiler::cseCanSwapOrder(GenTree* tree1, GenTree* tree2)
{
    assert(csePhase);

    struct CseDefUse
    {
        BitVecTraits traits;
        BitVec       def;
        BitVec       use;

        CseDefUse(Compiler* compiler)
            : traits(compiler->cseCandidateCount, compiler)
            , def(BitVecOps::MakeEmpty(&traits))
            , use(BitVecOps::MakeEmpty(&traits))
        {
        }
    };

    CseDefUse defUse1(this);
    CseDefUse defUse2(this);

    auto visitor = [](GenTree** use, Compiler::fgWalkData* walkData) {
        GenTree*   node = *use;
        CseDefUse* data = static_cast<CseDefUse*>(walkData->pCallbackData);

        if (IsCseIndex(node->gtCSEnum))
        {
            unsigned index = GetCseZeroIndex(node->gtCSEnum);

            if (IsCseDef(node->gtCSEnum))
            {
                BitVecOps::AddElemD(&data->traits, data->def, index);
            }
            else
            {
                BitVecOps::AddElemD(&data->traits, data->use, index);
            }
        }

        return Compiler::WALK_CONTINUE;
    };

    fgWalkTreePre(&tree1, visitor, &defUse1);
    fgWalkTreePre(&tree2, visitor, &defUse2);

    return BitVecOps::IsEmptyIntersection(&defUse1.traits, defUse1.def, defUse2.use) &&
           BitVecOps::IsEmptyIntersection(&defUse1.traits, defUse2.def, defUse1.use);
}

struct CseOccurence
{
    CseOccurence* next = nullptr;
    GenTree*      expr;
    Statement*    stmt;
    BasicBlock*   block;

    CseOccurence(GenTree* expr, Statement* stmt, BasicBlock* block) : expr(expr), stmt(stmt), block(block)
    {
    }
};

struct CseDesc
{
    CseDesc* nextInBucket;
    ValueNum hashVN;

    unsigned index; // 1..descCount

    bool isSharedConst;
    bool isLiveAcrossCall;

    uint16_t defCount;
    uint16_t useCount;

    BasicBlock::weight_t defWeight; // weighted def count
    BasicBlock::weight_t useWeight; // weighted use count (excluding the implicit uses at defs)

    CseOccurence  firstOccurence;
    CseOccurence* lastOccurence;

    ClassLayout* layout;

    ValueNum defExcSetPromise; // The exception set that is now required for all defs of this CSE.
    // This will be set to NoVN if we decide to abandon this CSE
    ValueNum defExcSetCurrent; // The set of exceptions we currently can use for CSE uses.
    ValueNum defConservNormVN; // if all def occurrences share the same conservative normal value
    // number, this will reflect it; otherwise, NoVN.
    // not used for shared const CSE's

    CseDesc(ValueNum hashVN, GenTree* expr, Statement* stmt, BasicBlock* block)
        : nextInBucket(nullptr)
        , hashVN(hashVN)
        , index(0)
        , isSharedConst(false)
        , isLiveAcrossCall(false)
        , defCount(0)
        , useCount(0)
        , defWeight(0)
        , useWeight(0)
        , firstOccurence(expr, stmt, block)
        , lastOccurence(&firstOccurence)
        , layout(nullptr)
        , defExcSetPromise(ValueNumStore::VNForEmptyExcSet())
        , defExcSetCurrent(ValueNumStore::VNForNull())
        , defConservNormVN(ValueNumStore::VNForNull())
    {
    }
};

CseDesc* Compiler::cseGetDesc(unsigned index)
{
    noway_assert((0 < index) && (index <= cseCandidateCount));
    noway_assert(cseTable[index - 1]);

    return cseTable[index - 1];
}

// When performing CSE we need to
// - update the CSE use count and weight of other CSE uses that are
//   present in the subtree
// - preserve any CSE defs that are present in the subtree
// This is done as part of side effect extraction (gtExtractSideEffList),
// thus making CSE defs appear as if they're side effects.
bool Compiler::cseUnmarkNode(GenTree* node)
{
    assert(csePhase);

    if (!IsCseIndex(node->gtCSEnum))
    {
        return true;
    }

    noway_assert(cseBlockWeight <= BB_MAX_WEIGHT);

    if (IsCseDef(node->gtCSEnum))
    {
        return false;
    }

    unsigned index = GetCseIndex(node->gtCSEnum);
    node->gtCSEnum = NoCse;

    CseDesc* desc = cseGetDesc(index);

    noway_assert(desc->useCount > 0);

    if (desc->useCount > 0)
    {
        desc->useCount -= 1;

        if (desc->useWeight < cseBlockWeight)
        {
            desc->useWeight = 0;
        }
        else
        {
            desc->useWeight -= cseBlockWeight;
        }

        JITDUMP("Updated CSE #%02u use count at [%06u]: %3d\n", index, node->GetID(), desc->useCount);
    }

    return true;
}

class Cse
{
    // The following is the upper limit on how many expressions we'll keep track
    // of for the CSE analysis.
    static constexpr unsigned MAX_CSE_CNT = 64;

    static constexpr size_t HashSizeInitial  = MAX_CSE_CNT * 2;
    static constexpr size_t HashGrowthFactor = 2;
    static constexpr size_t HashBucketSize   = 4;

    Compiler*      compiler;
    CompAllocator  allocator;
    ValueNumStore* vnStore;

    // The current size of hashtable
    size_t hashBucketCount = HashSizeInitial;
    // Number of entries in hashtable
    size_t hashCount = 0;
    // Number of entries before resize
    size_t    hashResizeCount = HashSizeInitial * HashBucketSize;
    CseDesc** hashBuckets;
    CseDesc** descTable;
    unsigned  descCount;

    typedef JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, GenTree*> NodeToNodeMap;

    // Maps bound nodes to ancestor compares that should be re-numbered
    // with the bound to improve range check elimination.
    NodeToNodeMap checkedBoundMap;

    // BitVec trait information for computing CSE availability using the DataFlowCallback algorithm.
    // Two bits are allocated per CSE candidate to compute CSE availability
    // plus an extra bit to handle the initial unvisited case.
    // (See DataFlowCallback::EndMerge for an explanation of why this is necessary.)
    //
    // The two bits per CSE candidate have the following meanings:
    //     11 - The CSE is available, and is also available when considering calls as killing availability.
    //     10 - The CSE is available, but is not available when considering calls as killing availability.
    //     00 - The CSE is not available
    //     01 - An illegal combination
    //
    BitVecTraits dataFlowTraits;

    BitVec callKillsMask; // Computed once - A mask that is used to kill available CSEs at callsites

    bool enableConstCse       = false;
    bool enableSharedConstCse = false;
    bool foundCandidates      = false;

    Compiler::codeOptimize codeOptKind;

    // Record the weight of the last "for sure" callee saved local
    BasicBlock::weight_t aggressiveWeight = 0;
    BasicBlock::weight_t moderateWeight   = 0;
    // count of the number of predicted enregistered variables
    unsigned enregCount = 0;
    bool     largeFrame = false;
    bool     hugeFrame  = false;
#ifdef DEBUG
    CLRRandom m_cseRNG;
    unsigned  m_bias;
#endif

public:
    Cse(Compiler* compiler)
        : compiler(compiler)
        , allocator(compiler->getAllocator(CMK_CSE))
        , vnStore(compiler->vnStore)
        , hashBuckets(new (allocator) CseDesc*[hashBucketCount]())
        , descTable(nullptr)
        , descCount(0)
        , checkedBoundMap(allocator)
        , dataFlowTraits(0, compiler)
        , codeOptKind(compiler->compCodeOpt())
    {
        compiler->cseTable          = nullptr;
        compiler->cseCandidateCount = 0;

        INDEBUG(compiler->cseFirstLclNum = compiler->lvaCount);
    }

    void Run()
    {
        compiler->csePhase = true;

        INDEBUG(EnsureClearCseNum());

        Configure();
        Locate();

        if (foundCandidates)
        {
            BuildCseTable();
            InitDataFlow();
            DataFlow();
            Availablity();

            JITDUMP("\n************ Trees at start of Heuristic()\n");
            DBEXEC(compiler->verbose, compiler->fgDumpTrees(compiler->fgFirstBB, nullptr));

            EstimateFrameSize();
            EstimateCutOffWeights();
            ConsiderCandidates();
        }

        compiler->csePhase = false;
    }

    void Configure()
    {
        enum ConstCseConfig
        {
            EnableArm64          = 0,
            DisableAll           = 1,
            EnableArm64NoSharing = 2,
            EnableAll            = 3,
            EnableAllNoSharing   = 4
        };

        ConstCseConfig constCse = static_cast<ConstCseConfig>(JitConfig.JitConstCSE());

#ifdef TARGET_ARM64
        enableConstCse       = (constCse != DisableAll);
        enableSharedConstCse = (constCse != EnableArm64NoSharing) && (constCse != EnableAllNoSharing);
#else
        enableConstCse              = (constCse == EnableAll) || (constCse == EnableAllNoSharing);
        enableSharedConstCse        = (constCse == EnableAll);
#endif
    }

#ifdef DEBUG
    void EnsureClearCseNum()
    {
        for (BasicBlock* block : compiler->Blocks())
        {
            for (Statement* stmt : block->Statements())
            {
                for (GenTree* node : stmt->Nodes())
                {
                    assert(node->gtCSEnum == NoCse);
                }
            }
        }
    }
#endif

    void BuildCseTable()
    {
        if (descCount == 0)
        {
            return;
        }

        CseDesc** table = new (allocator) CseDesc*[descCount]();

        for (size_t i = 0; i != hashBucketCount; i++)
        {
            for (CseDesc* desc = hashBuckets[i]; desc != nullptr; desc = desc->nextInBucket)
            {
                if (desc->index != 0)
                {
                    noway_assert(desc->index <= descCount);

                    if (table[desc->index - 1] == nullptr)
                    {
                        table[desc->index - 1] = desc;
                    }
                }
            }
        }

#ifdef DEBUG
        for (unsigned i = 0; i < descCount; i++)
        {
            noway_assert(table[i] != nullptr);
        }
#endif

        descTable = table;
    }

#ifdef DEBUG
    static const char* genES2str(BitVecTraits* traits, const BitVec set)
    {
        const int    bufSize = 65; // Supports a BitVec of up to 256 bits
        static char  num1[bufSize];
        static char  num2[bufSize];
        static char* nump = num1;

        assert(bufSize > roundUp(BitVecTraits::GetSize(traits), (unsigned)sizeof(char)) / 8);

        char* temp = nump;
        nump       = (nump == num1) ? num2 : num1;
        sprintf_s(temp, bufSize, "%s", BitVecOps::ToString(traits, set));

        return temp;
    }

    void DumpDataFlowSet(const BitVec set, bool includeBits = true)
    {
        if (includeBits)
        {
            printf("%s ", genES2str(&dataFlowTraits, set));
        }

        const char* prefix = "";
        for (unsigned i = 1; i <= descCount; i++)
        {
            unsigned availBit          = GetAvailBitIndex(i);
            unsigned availCrossCallBit = GetAvailCrossCallBitIndex(i);

            if (BitVecOps::IsMember(&dataFlowTraits, set, availBit))
            {
                const bool isAvailCrossCall = BitVecOps::IsMember(&dataFlowTraits, set, availCrossCallBit);

                printf("%s" FMT_CSE "%s", prefix, i, isAvailCrossCall ? ".c" : "");
                prefix = ", ";
            }
        }
    }
#endif // DEBUG

    static CseIndex ToCseIndex(unsigned index)
    {
        assert(index < INT8_MAX);
        return static_cast<CseIndex>(index);
    }

    static CseIndex ToCseDefIndex(CseIndex index)
    {
        return static_cast<CseIndex>(-index);
    }

    static ssize_t GetSharedConstValue(ssize_t value)
    {
#if defined(TARGET_XARCH)
        constexpr int SharedLowBits = 16;
#elif defined(TARGET_ARMARCH)
        constexpr int SharedLowBits = 12;
#else
#error Unsupported or unset target architecture
#endif

        return value & ~((static_cast<ssize_t>(1) << SharedLowBits) - 1);
    }

    static unsigned KeyVNToBucketIndex(ValueNum hashVN, size_t bucketCount)
    {
        unsigned hash = hashVN;
        hash *= static_cast<unsigned>(bucketCount + 1);
        hash >>= 7;
        return hash % bucketCount;
    }

    // Returns the CSE index to use for this tree, or zero if this expression is not currently a CSE.
    //
    // We build a hash table that contains all of the expressions that are presented to this method.
    // Whenever we see a duplicate expression we have a CSE candidate. If it is the first time seeing
    // the duplicate we allocate a new CSE index. If we have already allocated a CSE index we return
    // that index. There currently is a limit on the number of CSEs that we can have of MAX_CSE_CNT.
    unsigned Index(GenTree* expr, Statement* stmt, BasicBlock* block)
    {
        // We use the normal value number because we want the CSE candidate to
        // represent all expressions that produce the same normal value number.
        // We will handle the case where we have different exception sets when
        // promoting the candidates.
        //
        // We do this because a IND will usually have a NullPtrExc entry in its
        // exc set, but we may have cleared the GTF_EXCEPT flag and if so, it
        // won't have an NullPtrExc, or we may have assigned the value of an IND
        // into a LCL_VAR and then read it back later.
        //
        // When we are promoting the CSE candidates we ensure that any CSE uses
        // that we promote have an exc set that is the same as the CSE defs or
        // have an empty set. And that all of the CSE defs produced the required
        // set of exceptions for the CSE uses.

        // We use either exprVN or exprValueVN as the hash key.
        //
        // The only exception to using the normal value is for COMMA nodes.
        // Here we check to see if we have a COMMA with a different value number
        // than the one from its op2. For this case we want to create two different
        // CSE candidates. This allows us to CSE the COMMA separately from its value.

        ValueNum exprVN        = expr->GetLiberalVN();
        ValueNum exprValueVN   = vnStore->VNNormalValue(exprVN);
        ValueNum hashVN        = exprValueVN;
        bool     isSharedConst = false;

        if (expr->OperIs(GT_COMMA))
        {
            GenTree* op2   = expr->AsOp()->GetOp(1);
            ValueNum op2VN = op2->GetLiberalVN();

            // If the value number for op2 and expr are different, then some new
            // exceptions were produced by op1. For that case we will NOT use the
            // normal value. This allows us to CSE commas with an op1 that is
            // an ARR_BOUNDS_CHECK.
            if (op2VN != exprVN)
            {
                hashVN = exprVN;
            }

            // If we didn't do the above we would have op1 as the CSE def
            // and the parent comma as the CSE use (but with a different exc set)
            // This would prevent us from making any CSE with the comma
            assert(exprValueVN == vnStore->VNNormalValue(op2VN));
        }
        else if (enableSharedConstCse && expr->IsIntCon() && !expr->AsIntCon()->ImmedValNeedsReloc(compiler))
        {
            assert(vnStore->IsVNConstant(exprValueVN));

            // Create a VN that for a constant that has only the upper bits of the
            // real constant by shifting out some of the low bits, (12 or 16 bits).
            ssize_t sharedConstVal = GetSharedConstValue(expr->AsIntCon()->GetValue());

            hashVN = varTypeSize(expr->GetType()) == 4 ? vnStore->VNForIntCon(static_cast<int32_t>(sharedConstVal))
                                                       : vnStore->VNForLongCon(sharedConstVal);
            isSharedConst = true;
        }

        unsigned bucketIndex = KeyVNToBucketIndex(hashVN, hashBucketCount);
        CseDesc* found       = nullptr;

        for (CseDesc* desc = hashBuckets[bucketIndex]; desc != nullptr; desc = desc->nextInBucket)
        {
            if (desc->hashVN != hashVN)
            {
                continue;
            }

            if (expr->OperIs(GT_CNS_INT) && (expr->GetType() != desc->firstOccurence.expr->GetType()))
            {
                continue;
            }

            found = desc;
            break;
        }

        if (found != nullptr)
        {
            // TODO-MIKE-Fix: Currently shared const code recognizes only constant nodes,
            // not VN constants. Don't mix them for now as it's a rather case anyway.
            if (found->isSharedConst != isSharedConst)
            {
                return 0;
            }

            foundCandidates = true;

            if (varTypeIsSIMD(expr->GetType()) && (found->layout == nullptr))
            {
                // If we haven't yet obtained the SIMD layout try again, maybe we get lucky.
                // Mostly for the sake of consistency. Otherwise it doesn't really matter.
                // If we decide to CSE this expression we'll try to get an approximate layout.
                // The SIMD base type and the kind of vector that's associated with this SIMD
                // expression is ultimately irrelevant, we just need a layout that has the
                // same SIMD type as the expression. It also doesn't matter if 2 equivalent
                // expression somehow have different layouts, as long as the layout SIMD type
                // is the same.

                found->layout = compiler->typGetStructLayout(expr);
            }

            CseOccurence* occurrence   = new (allocator) CseOccurence(expr, stmt, block);
            found->lastOccurence->next = occurrence;
            found->lastOccurence       = occurrence;

            if (found->index != 0)
            {
                expr->gtCSEnum = ToCseIndex(found->index);

                return found->index;
            }
        }

        if (descCount == MAX_CSE_CNT)
        {
            JITDUMPTREE(expr, "Exceeded MAX_CSE_CNT, not using expression:\n");

            return 0;
        }

        if (found != nullptr)
        {
            noway_assert(found->firstOccurence.expr->gtCSEnum == NoCse);

            unsigned index = ++descCount;

            found->index                         = index;
            found->firstOccurence.expr->gtCSEnum = ToCseIndex(index);
            expr->gtCSEnum                       = static_cast<CseIndex>(index);

#ifdef DEBUG
            if (compiler->verbose)
            {
                printf("\nCSE candidate #%02u, key=", index);
                compiler->vnPrint(static_cast<ValueNum>(hashVN), 0);
                printf(" in " FMT_BB ", [cost=%2u, size=%2u]: \n", block->bbNum, expr->GetCostEx(), expr->GetCostSz());

                compiler->gtDispTree(expr);
            }
#endif // DEBUG

            return index;
        }

        if (hashCount == hashResizeCount)
        {
            ResizeHashTable();
        }

        CseDesc* desc       = new (allocator) CseDesc(hashVN, expr, stmt, block);
        desc->isSharedConst = isSharedConst;

        if (varTypeIsStruct(expr->GetType()))
        {
            desc->layout = compiler->typGetStructLayout(expr);
            assert((desc->layout != nullptr) || varTypeIsSIMD(expr->GetType()));
        }

        desc->nextInBucket       = hashBuckets[bucketIndex];
        hashBuckets[bucketIndex] = desc;
        hashCount++;

        return 0;
    }

    void ResizeHashTable()
    {
        size_t    newBucketCount = hashBucketCount * HashGrowthFactor;
        CseDesc** newBuckets     = new (allocator) CseDesc*[newBucketCount]();

        for (size_t i = 0; i < hashBucketCount; i++)
        {
            for (CseDesc *d = hashBuckets[i], *next; d != nullptr; d = next)
            {
                next = d->nextInBucket;

                size_t newBucketIndex      = KeyVNToBucketIndex(d->hashVN, newBucketCount);
                d->nextInBucket            = newBuckets[newBucketIndex];
                newBuckets[newBucketIndex] = d;
            }
        }

        hashBuckets     = newBuckets;
        hashBucketCount = newBucketCount;
        hashResizeCount = hashResizeCount * HashGrowthFactor;
    }

    void Locate()
    {
        for (BasicBlock* block : compiler->Blocks())
        {
            for (Statement* stmt : block->NonPhiStatements())
            {
                bool isReturnStmt              = stmt->GetRootNode()->OperIs(GT_RETURN);
                bool stmtHasArrLengthCandidate = false;

                for (GenTree* node : stmt->Nodes())
                {
                    if (node->OperIsCompare() && stmtHasArrLengthCandidate)
                    {
                        UpdateCheckedBoundMap(node->AsOp());
                    }

                    if (node->IsIntegralConst() && !enableConstCse)
                    {
                        continue;
                    }

                    // Don't allow non-SIMD struct CSEs under a return; we don't fully
                    // re-morph these if we introduce a CSE assignment, and so may create
                    // IR that lower is not yet prepared to handle.
                    // TODO-MIKE-Cleanup: Remove this crap.
                    if (isReturnStmt && varTypeIsStruct(node->GetType()) && !varTypeIsSIMD(node->GetType()))
                    {
                        continue;
                    }

                    if (!compiler->cseIsCandidate(node))
                    {
                        continue;
                    }

                    if (ValueNumStore::isReservedVN(node->GetLiberalVN()))
                    {
                        continue;
                    }

                    // We want to CSE simple constant leaf nodes, but we don't want to CSE non-leaf
                    // trees that compute CSE constant values. Instead we let assertion prop phase
                    // handle them.
                    // Here, unlike the rest of CSE, we use the conservative value number rather
                    // than the liberal one, since the conservative one is what assertion prop will
                    // use and the point is to avoid optimizing cases that it will handle.

                    if (!node->OperIsLeaf() && vnStore->IsVNConstant(vnStore->VNNormalValue(node->GetConservativeVN())))
                    {
                        continue;
                    }

                    unsigned index = Index(node, stmt, block);

                    if ((index != 0) && node->OperIs(GT_ARR_LENGTH))
                    {
                        stmtHasArrLengthCandidate = true;
                    }
                }
            }
        }
    }

    // Check if this compare is a tractable function of a checked bound that is
    // a CSE candidate, and insert an entry in the optCseCheckedBoundMap if so.
    // This facilitates subsequently updating the compare's value number if
    // the bound gets CSEd.
    void UpdateCheckedBoundMap(GenTreeOp* compare)
    {
        assert(compare->OperIsCompare());

        ValueNum  compareVN = compare->gtVNPair.GetConservative();
        VNFuncApp cmpVNFuncApp;

        if (!vnStore->GetVNFunc(compareVN, &cmpVNFuncApp) || (cmpVNFuncApp.m_func != GetVNFuncForNode(compare)))
        {
            // Value numbering inferred this compare as something other
            // than its own operator; leave its value number alone.
            return;
        }

        if (!ValueNumStore::IsVNCompareCheckedBoundRelop(cmpVNFuncApp))
        {
            return;
        }

        // Now look for a checked bound feeding the compare
        ValueNumStore::CompareCheckedBoundArithInfo info;

        GenTree* boundParent = nullptr;

        if (vnStore->IsVNCompareCheckedBound(cmpVNFuncApp))
        {
            // Simple compare of an bound against something else.

            vnStore->GetCompareCheckedBound(cmpVNFuncApp, &info);
            boundParent = compare;
        }
        else if (vnStore->IsVNCompareCheckedBoundArith(cmpVNFuncApp))
        {
            // Compare of a bound +/- some offset to something else.

            GenTree* op1 = compare->gtGetOp1();
            GenTree* op2 = compare->gtGetOp2();

            vnStore->GetCompareCheckedBoundArithInfo(cmpVNFuncApp, &info);
            if (GetVNFuncForNode(op1) == (VNFunc)info.arrOper)
            {
                // The arithmetic node is the bound's parent.
                boundParent = op1;
            }
            else if (GetVNFuncForNode(op2) == (VNFunc)info.arrOper)
            {
                // The arithmetic node is the bound's parent.
                boundParent = op2;
            }
        }

        if (boundParent != nullptr)
        {
            GenTree* bound = nullptr;

            // Find which child of boundParent is the bound.  Abort if neither
            // conservative value number matches the one from the compare VN.

            GenTree* child1 = boundParent->gtGetOp1();
            if ((info.vnBound == child1->gtVNPair.GetConservative()) && IsCseIndex(child1->gtCSEnum))
            {
                bound = child1;
            }
            else
            {
                GenTree* child2 = boundParent->gtGetOp2();
                if ((info.vnBound == child2->gtVNPair.GetConservative()) && IsCseIndex(child2->gtCSEnum))
                {
                    bound = child2;
                }
            }

            if (bound != nullptr)
            {
                // Found a checked bound feeding a compare that is a tractable function of it;
                // record this in the map so we can update the compare VN if the bound
                // node gets CSEd.

                checkedBoundMap.Set(bound, compare);
            }
        }
    }

    CseDesc* GetDesc(unsigned index) const
    {
        noway_assert((0 < index) && (index <= descCount));
        noway_assert(descTable[index - 1]);

        return descTable[index - 1];
    }

    // Return the bit used by CSE dataflow sets (bbCseGen, etc.) for the availability bit for a CSE.
    static unsigned GetAvailBitIndex(unsigned cseNum)
    {
        assert((cseNum > 0) && (cseNum <= MAX_CSE_CNT));

        return (cseNum - 1) * 2;
    }

    // Return the bit used by CSE dataflow sets (bbCseGen, etc.) for the availability bit
    // for a CSE considering calls as killing availability bit (see description above).
    static unsigned GetAvailCrossCallBitIndex(unsigned cseNum)
    {
        return GetAvailBitIndex(cseNum) + 1;
    }

    void InitDataFlow()
    {
        // Two bits are allocated per CSE candidate to compute CSE availability
        // plus an extra bit to handle the initial unvisited case.
        // (See DataFlowCallback::EndMerge for an explaination of why this is necessary)
        //
        // The two bits per CSE candidate have the following meanings:
        //     11 - The CSE is available, and is also available when considering calls as killing availability.
        //     10 - The CSE is available, but is not available when considering calls as killing availability.
        //     00 - The CSE is not available
        //     01 - An illegal combination

        dataFlowTraits = BitVecTraits((descCount * 2) + 1, compiler);

        callKillsMask = BitVecOps::MakeEmpty(&dataFlowTraits);

        for (unsigned cseNum = 1; cseNum <= descCount; cseNum++)
        {
            BitVecOps::AddElemD(&dataFlowTraits, callKillsMask, GetAvailBitIndex(cseNum));
        }

        for (BasicBlock* block : compiler->Blocks())
        {
            bool initToZero = false;

            if (block == compiler->fgFirstBB)
            {
                initToZero = true;
            }
            // TODO-CQ: Add CSE for handler blocks.
            else if (compiler->bbIsHandlerBeg(block))
            {
                initToZero = true;
            }

            if (initToZero)
            {
                block->bbCseIn = BitVecOps::MakeEmpty(&dataFlowTraits);
            }
            else
            {
                block->bbCseIn = BitVecOps::MakeFull(&dataFlowTraits);
            }

            block->bbCseOut = BitVecOps::MakeFull(&dataFlowTraits);
            block->bbCseGen = BitVecOps::MakeEmpty(&dataFlowTraits);
        }

        for (unsigned i = 0; i < descCount; i++)
        {
            CseDesc* desc              = descTable[i];
            unsigned availBit          = GetAvailBitIndex(desc->index);
            unsigned availCrossCallBit = GetAvailCrossCallBitIndex(desc->index);

            for (CseOccurence* occurence = &desc->firstOccurence; occurence != nullptr; occurence = occurence->next)
            {
                BitVecOps::AddElemD(&dataFlowTraits, occurence->block->bbCseGen, availBit);

                if ((occurence->block->bbFlags & BBF_HAS_CALL) == 0)
                {
                    BitVecOps::AddElemD(&dataFlowTraits, occurence->block->bbCseGen, availCrossCallBit);
                }
            }
        }

        // If a block contains a call and generates CSEs, we may need to update the
        // bbCseGen set as we may generate some CSEs after the last call in the block.

        for (BasicBlock* block : compiler->Blocks())
        {
            if ((block->bbFlags & BBF_HAS_CALL) == 0)
            {
                continue;
            }

            if (BitVecOps::IsEmpty(&dataFlowTraits, block->bbCseGen))
            {
                continue;
            }

            for (Statement* stmt = block->lastStmt(); stmt != nullptr; stmt = stmt->GetPrevStmt())
            {
                GenTree* node;

                for (node = stmt->GetRootNode(); node != nullptr; node = node->gtPrev)
                {
                    if (IsCseIndex(node->gtCSEnum))
                    {
                        unsigned availCrossCallBit = GetAvailCrossCallBitIndex(GetCseIndex(node->gtCSEnum));

                        BitVecOps::AddElemD(&dataFlowTraits, block->bbCseGen, availCrossCallBit);
                    }

                    if (node->IsCall())
                    {
                        break;
                    }
                }

                if ((stmt == block->firstStmt()) || (node != nullptr))
                {
                    break;
                }
            }
        }

#ifdef DEBUG
        if (compiler->verbose)
        {
            bool headerPrinted = false;

            for (BasicBlock* block : compiler->Blocks())
            {
                if (!BitVecOps::IsEmpty(&dataFlowTraits, block->bbCseGen))
                {
                    if (!headerPrinted)
                    {
                        printf("\nBlocks that generate CSE def/uses\n");
                        headerPrinted = true;
                    }

                    printf(FMT_BB " cseGen = ", block->bbNum);
                    DumpDataFlowSet(block->bbCseGen);
                    printf("\n");
                }
            }
        }

        compiler->fgDebugCheckLinks();
#endif // DEBUG
    }

    class DataFlowCallback
    {
        BitVecTraits traits;
        BitVec const callKillsMask;
        BitVec       cseInWithCallsKill;
        BitVec       preMergeOut;
        INDEBUG(bool const verbose;)

    public:
        DataFlowCallback(Compiler* compiler, Cse& cse)
            : traits(cse.dataFlowTraits)
            , callKillsMask(cse.callKillsMask)
            , cseInWithCallsKill(BitVecOps::UninitVal())
            , preMergeOut(BitVecOps::UninitVal())
#ifdef DEBUG
            , verbose(compiler->verbose)
#endif
        {
        }

        // At the start of the merge function of the dataflow equations, initialize premerge state (to detect changes.)
        void StartMerge(BasicBlock* block)
        {
            // Record the initial value of block->bbCseOut in m_preMergeOut.
            // It is used in EndMerge() to control the termination of the DataFlow algorithm.
            // Note that the first time we visit a block, the value of bbCseOut is MakeFull()
            BitVecOps::Assign(&traits, preMergeOut, block->bbCseOut);

#if 0
            JITDUMP("StartMerge " FMT_BB "\n", block->bbNum);
            JITDUMP("  :: cseOut    = %s\n", genES2str(&traits, block->bbCseOut));
#endif
        }

        // Perform the merging of each of the predecessor's liveness values (since this is a forward analysis)
        void Merge(BasicBlock* block, BasicBlock* predBlock, unsigned dupCount)
        {
#if 0
            JITDUMP("Merge " FMT_BB " and " FMT_BB "\n", block->bbNum, predBlock->bbNum);
            JITDUMP("  :: cseIn     = %s\n", genES2str(&traits, block->bbCseIn));
            JITDUMP("  :: cseOut    = %s\n", genES2str(&traits, block->bbCseOut));
#endif

            BitVecOps::IntersectionD(&traits, block->bbCseIn, predBlock->bbCseOut);

#if 0
            JITDUMP("  => cseIn     = %s\n", genES2str(&traits, block->bbCseIn));
#endif
        }

        // Merge CSE values into the first exception handler/filter block.
        // We can jump to the handler from any instruction in the try region.
        // It means we can propagate only CSE that are valid for the whole try region.
        void MergeHandler(BasicBlock* block, BasicBlock* firstTryBlock, BasicBlock* lastTryBlock)
        {
            // TODO-CQ: Add CSE for handler blocks.
        }

        // At the end of the merge store results of the dataflow equations, in a postmerge state.
        // We also handle the case where calls conditionally kill CSE availabilty.
        bool EndMerge(BasicBlock* block)
        {
            // We can skip the calls kill step when our block doesn't have a callsite
            // or we don't have any available CSEs in our bbCseIn
            if (((block->bbFlags & BBF_HAS_CALL) == 0) || BitVecOps::IsEmpty(&traits, block->bbCseIn))
            {
                // No callsite in 'block' or 'block->bbCseIn was empty, so we can use bbCseIn directly
                BitVecOps::DataFlowD(&traits, block->bbCseOut, block->bbCseGen, block->bbCseIn);
            }
            else
            {
                BitVecOps::Assign(&traits, cseInWithCallsKill, block->bbCseIn);
                BitVecOps::IntersectionD(&traits, cseInWithCallsKill, callKillsMask);
                BitVecOps::DataFlowD(&traits, block->bbCseOut, block->bbCseGen, cseInWithCallsKill);
            }

            // This is why we need to allocate an extra bit in our BitVecs.
            // We always need to visit our successor blocks once, thus we require that that the first time
            // that we visit a block we have a bit set in preMergeOut that won't be set when we compute
            // the new value of bbCseOut.
            bool notDone = !BitVecOps::Equal(&traits, block->bbCseOut, preMergeOut);

#if 0
#ifdef DEBUG
        if (verbose)
        {
            printf("EndMerge " FMT_BB "\n", block->bbNum);
            printf("  :: cseIn     = %s\n", genES2str(&traits, block->bbCseIn));

            if (((block->bbFlags & BBF_HAS_CALL) != 0) && !BitVecOps::IsEmpty(&traits, block->bbCseIn))
            {
                printf("  -- cseKill   = %s\n", genES2str(&traits, m_comp->cseCallKillsMask));
            }

            printf("  :: cseGen    = %s\n", genES2str(&traits, block->bbCseGen));
            printf("  => cseOut    = %s\n", genES2str(&traits, block->bbCseOut));
            printf("  != preMerge  = %s, => %s\n", genES2str(&traits, m_preMergeOut), notDone ? "true" : "false");
        }
#endif // DEBUG
#endif // 0

            return notDone;
        }
    };

    // Perform a DataFlow forward analysis using the block CSE bitsets:
    //   Inputs:
    //     bbCseGen  - Exact CSEs that are always generated within the block
    //     bbCseIn   - Maximal estimate of CSEs that are/could be available at input to the block
    //     bbCseOut  - Maximal estimate of CSEs that are/could be available at exit to the block
    //
    //   Outputs:
    //     bbCseIn   - Computed CSEs that are available at input to the block
    //     bbCseOut  - Computed CSEs that are available at exit to the block
    void DataFlow()
    {
        JITDUMP("\nPerforming DataFlow for ValnumCSE's\n");

        ForwardDataFlow(DataFlowCallback(compiler, *this), compiler);

#ifdef DEBUG
        if (compiler->verbose)
        {
            printf("\nAfter performing DataFlow for ValnumCSE's\n");

            for (BasicBlock* const block : compiler->Blocks())
            {
                printf(FMT_BB " in gen out\n", block->bbNum);
                DumpDataFlowSet(block->bbCseIn);
                printf("\n");
                DumpDataFlowSet(block->bbCseGen);
                printf("\n");
                DumpDataFlowSet(block->bbCseOut);
                printf("\n");
            }

            printf("\n");
        }
#endif // DEBUG
    }

    // Using the information computed by DataFlowCallback determine for each
    // CSE whether the CSE is a definition (if the CSE was not available)
    // or if the CSE is a use (if the CSE was previously made available).
    // The implementation iterates over all blocks setting 'available_cses'
    // to the CSEs that are available at input to the block.
    // When a CSE expression is encountered it is classified as either
    // as a definition (if the CSE is not in the 'available_cses' set) or
    // as a use (if the CSE is in the 'available_cses' set).  If the CSE
    // is a definition then it is added to the 'available_cses' set.
    //
    // This algorithm uncovers the defs and uses gradually and as it does
    // so it also builds the exception set that all defs make: 'defExcSetCurrent'
    // and the exception set that the uses we have seen depend upon: 'defExcSetPromise'.
    //
    // Typically expressions with the same normal ValueNum generate exactly the
    // same exception sets. There are two way that we can get different exception
    // sets with the same Normal value number.
    //
    // 1. We used an arithmetic identiity:
    //    e.g. (p.a + q.b) * 0   :: The normal value for the expression is zero
    //                              and we have NullPtrExc(p) and NullPtrExc(q)
    //    e.g. (p.a - p.a)       :: The normal value for the expression is zero
    //                              and we have NullPtrExc(p)
    // 2. We stored an expression into a LclVar or into Memory and read it later
    //    e.g. t = p.a;
    //         e1 = (t + q.b)    :: e1 has one NullPtrExc and e2 has two.
    //         e2 = (p.a + q.b)     but both compute the same normal value
    //    e.g. m.a = p.a;
    //         e1 = (m.a + q.b)  :: e1 and e2 have different exception sets.
    //         e2 = (p.a + q.b)     but both compute the same normal value
    //
    void Availablity()
    {
        JITDUMP("Labeling the CSEs with Use/Def information\n");

        BitVec available_cses = BitVecOps::MakeEmpty(&dataFlowTraits);

        for (BasicBlock* const block : compiler->Blocks())
        {
            BitVecOps::Assign(&dataFlowTraits, available_cses, block->bbCseIn);
            BasicBlock::weight_t blockWeight = block->getBBWeight(compiler);

            for (Statement* const stmt : block->NonPhiStatements())
            {
                for (GenTree* const tree : stmt->TreeList())
                {
                    bool isUse = false;
                    bool isDef = false;

                    if (IsCseIndex(tree->gtCSEnum))
                    {
                        unsigned CSEnum               = GetCseIndex(tree->gtCSEnum);
                        unsigned cseAvailBit          = GetAvailBitIndex(CSEnum);
                        unsigned cseAvailCrossCallBit = GetAvailCrossCallBitIndex(CSEnum);
                        CseDesc* desc                 = GetDesc(CSEnum);

                        isUse = BitVecOps::IsMember(&dataFlowTraits, available_cses, cseAvailBit);
                        isDef = !isUse; // If is isn't a CSE use, it is a CSE def

                        // Is this a "use", that we haven't yet marked as live across a call and it
                        // is not available when we have calls that kill CSE's (cseAvailCrossCallBit)
                        // if the above is true then we will mark this the CSE as live across a call
                        bool madeLiveAcrossCall = false;
                        if (isUse && !desc->isLiveAcrossCall &&
                            !BitVecOps::IsMember(&dataFlowTraits, available_cses, cseAvailCrossCallBit))
                        {
                            desc->isLiveAcrossCall = true;
                            madeLiveAcrossCall     = true;
                        }

#ifdef DEBUG
                        // If this is a CSE def (i.e. the CSE is not available here, since it is being defined),
                        // then the call-kill bit should also be zero since it is also not available across a call.
                        if (isDef)
                        {
                            assert(!BitVecOps::IsMember(&dataFlowTraits, available_cses, cseAvailCrossCallBit));
                        }

                        JITDUMP(FMT_BB " [%06u] %s of " FMT_CSE " [weight=%s]%s\n", block->bbNum, tree->GetID(),
                                isUse ? "use" : "def", CSEnum, refCntWtd2str(blockWeight),
                                madeLiveAcrossCall ? " *** Now Live Across Call ***" : "");
#endif // DEBUG

                        // Have we decided to abandon work on this CSE?
                        if (desc->defExcSetPromise == ValueNumStore::NoVN)
                        {
                            // This candidate had defs with differing liberal exc set VNs
                            // We have abandoned CSE promotion for this candidate
                            tree->gtCSEnum = NoCse;

                            JITDUMP(" Abandoned - CSE candidate has defs with different exception sets!\n");
                            continue;
                        }

                        // Record the exception set for tree's liberal value number
                        ValueNum theLiberalExcSet = vnStore->VNExceptionSet(tree->gtVNPair.GetLiberal());

                        if (isDef)
                        {
                            if (desc->defExcSetCurrent == vnStore->VNForNull())
                            {
                                // This is the first time visited, so record this defs exception set.
                                desc->defExcSetCurrent = theLiberalExcSet;
                            }

                            // Have we seen a CSE use and made a promise of an exception set?
                            if (desc->defExcSetPromise != vnStore->VNForEmptyExcSet())
                            {
                                // The exeception set held in desc->defExcSetPromise must be a subset of
                                // theLiberalExcSet.
                                if (vnStore->VNExcIsSubset(theLiberalExcSet, desc->defExcSetPromise))
                                {
                                    // This new def still satisfies any promise made to all the CSE uses that we have
                                    // encountered.

                                    // no update is needed when these are the same VN
                                    if (desc->defExcSetCurrent != theLiberalExcSet)
                                    {
                                        // We will change the value of desc->defExcSetCurrent to be the intersection of
                                        // these two sets. This is the set of exceptions that all CSE defs have (that we
                                        // have visited so far).
                                        ValueNum intersectionExcSet =
                                            vnStore->VNExcSetIntersection(desc->defExcSetCurrent, theLiberalExcSet);
#ifdef DEBUG
                                        if (compiler->verbose)
                                        {
                                            VNFuncApp excSeq;

                                            vnStore->GetVNFunc(desc->defExcSetCurrent, &excSeq);
                                            printf(">>> defExcSetCurrent is ");
                                            vnStore->vnDumpExcSeq(compiler, &excSeq, true);
                                            printf("\n");

                                            vnStore->GetVNFunc(theLiberalExcSet, &excSeq);
                                            printf(">>> theLiberalExcSet is ");
                                            vnStore->vnDumpExcSeq(compiler, &excSeq, true);
                                            printf("\n");

                                            if (intersectionExcSet == vnStore->VNForEmptyExcSet())
                                            {
                                                printf(">>> the intersectionExcSet is the EmptyExcSet\n");
                                            }
                                            else
                                            {
                                                vnStore->GetVNFunc(intersectionExcSet, &excSeq);
                                                printf(">>> the intersectionExcSet is ");
                                                vnStore->vnDumpExcSeq(compiler, &excSeq, true);
                                                printf("\n");
                                            }
                                        }
#endif // DEBUG

                                        // Change the defExcSetCurrent to be a subset of its prior value
                                        assert(vnStore->VNExcIsSubset(desc->defExcSetCurrent, intersectionExcSet));
                                        desc->defExcSetCurrent = intersectionExcSet;
                                    }
                                }
                                else // This CSE def doesn't satisfy one of the exceptions already promised to a CSE
                                     // use.
                                {
                                    // So, we will abandon all CSE promotions for this candidate
                                    // We use the marker value of NoVN to indicate that we should abandon this CSE
                                    // candidate.
                                    desc->defExcSetPromise = ValueNumStore::NoVN;
                                    tree->gtCSEnum         = NoCse;

                                    JITDUMP(" Abandon - CSE candidate has defs with exception sets that do not satisfy "
                                            "some CSE use\n");
                                    continue;
                                }
                            }

                            // For shared const CSE we don't set/use the defConservNormVN.
                            if (!desc->isSharedConst)
                            {
                                // Record or update the value of desc->defConservNormVN.
                                ValueNum theConservNormVN = vnStore->VNConservativeNormalValue(tree->gtVNPair);

                                if (desc->defConservNormVN == vnStore->VNForNull())
                                {
                                    // This is the first def that we have visited, set defConservNormVN
                                    desc->defConservNormVN = theConservNormVN;
                                }
                                else
                                {
                                    // Check to see if all defs have the same conservative normal VN
                                    if (theConservNormVN != desc->defConservNormVN)
                                    {
                                        // This candidate has defs with differing conservative normal VNs, mark it with
                                        // NoVN
                                        desc->defConservNormVN = ValueNumStore::NoVN;
                                    }
                                }
                            }

                            // If we get here we have accepted this node as a valid CSE def

                            desc->defCount += 1;
                            desc->defWeight += blockWeight;

                            // Mark the node as a CSE definition

                            tree->gtCSEnum = ToCseDefIndex(tree->gtCSEnum);

                            // This CSE becomes available after this def
                            BitVecOps::AddElemD(&dataFlowTraits, available_cses, cseAvailBit);
                            BitVecOps::AddElemD(&dataFlowTraits, available_cses, cseAvailCrossCallBit);
                        }
                        else // We are visiting a CSE use
                        {
                            assert(isUse);

                            // If the CSE use has no requirements for an exception set then we don't have to do anything
                            // here
                            if (theLiberalExcSet != vnStore->VNForEmptyExcSet())
                            {
                                // Are we visiting a use first, before visiting any defs of this CSE?
                                // This is an atypical case that can occur with a bottom tested loop.
                                //
                                // Is defExcSetCurrent still set to the uninit marker value of VNForNull() ?
                                if (desc->defExcSetCurrent == vnStore->VNForNull())
                                {
                                    // Update defExcSetPromise, this is our required exception set for all CSE defs
                                    // that we encounter later.
                                    //
                                    // We could see multiple uses before a def, so we require the Union of all exception
                                    // sets.
                                    desc->defExcSetPromise =
                                        vnStore->VNExcSetUnion(desc->defExcSetPromise, theLiberalExcSet);
                                }
                                else // we have already seen a def for this CSE and defExcSetCurrent is setup
                                {
                                    if (vnStore->VNExcIsSubset(desc->defExcSetCurrent, theLiberalExcSet))
                                    {
                                        // The current set of exceptions produced by all CSE defs have (that we have
                                        // visited so far) meets our requirement
                                        desc->defExcSetPromise =
                                            vnStore->VNExcSetUnion(desc->defExcSetPromise, theLiberalExcSet);
                                    }
                                }

                                // At this point defExcSetPromise contains all of the exception items that we can
                                // promise here.
                                if (!vnStore->VNExcIsSubset(desc->defExcSetPromise, theLiberalExcSet))
                                {
                                    // We can't safely make this into a CSE use, because this
                                    // CSE use has an exception set item that is not promised
                                    // by all of our CSE defs.
                                    //
                                    // We will omit this CSE use from the graph and proceed,
                                    // the other uses and defs can still participate in the CSE optimization.

                                    // So this can't be a CSE use
                                    tree->gtCSEnum = NoCse;

                                    JITDUMP(" NO_CSE - This use has an exception set item that isn't contained in the "
                                            "defs!\n");

                                    continue;
                                }
                            }

                            // When we get here we have accepted this node as a valid CSE use

                            desc->useCount += 1;
                            desc->useWeight += blockWeight;
                        }
                    }

                    // In order to determine if a CSE is live across a call, we model availablity using two bits and
                    // kill all of the cseAvailCrossCallBit for each CSE whenever we see a GT_CALL (unless the call
                    // generates a CSE).
                    if (tree->OperGet() == GT_CALL)
                    {
                        // Check for the common case of an already empty available_cses set
                        // and thus nothing needs to be killed
                        if (!(BitVecOps::IsEmpty(&dataFlowTraits, available_cses)))
                        {
                            if (isUse)
                            {
                                // For a CSE Use we will assume that the CSE logic will replace it with a CSE LclVar and
                                // not make the call so kill nothing
                            }
                            else
                            {
                                // partially kill any cse's that are currently alive (using the cseCallKillsMask set)
                                BitVecOps::IntersectionD(&dataFlowTraits, available_cses, callKillsMask);

                                if (isDef)
                                {
                                    // We can have a GT_CALL that produces a CSE,
                                    // (i.e. HELPER.CORINFO_HELP_GETSHARED_*STATIC_BASE or
                                    // CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE)
                                    //
                                    // The CSE becomes available after the call, so set the cseAvailCrossCallBit bit in
                                    // available_cses

                                    unsigned CSEnum               = GetCseIndex(tree->gtCSEnum);
                                    unsigned cseAvailCrossCallBit = GetAvailCrossCallBitIndex(CSEnum);

                                    BitVecOps::AddElemD(&dataFlowTraits, available_cses, cseAvailCrossCallBit);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    void EstimateFrameSize()
    {
        unsigned frameSize        = 0;
        unsigned regAvailEstimate = (CNT_CALLEE_ENREG * 3) + (CNT_CALLEE_TRASH * 2) + 1;

        for (unsigned lclNum = 0; lclNum < compiler->lvaCount; lclNum++)
        {
            LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

            // Locals with no references don't use any local stack frame slots
            if (lcl->GetRefCount() == 0)
            {
                continue;
            }

            // Incoming stack arguments don't use any local stack frame slots
            if (lcl->IsParam() && !lcl->IsRegParam())
            {
                continue;
            }

#if FEATURE_FIXED_OUT_ARGS
            // Skip the OutgoingArgArea in computing frame size, since its size is not yet
            // known and it doesn't affect local offsets from the frame pointer (though it
            // may affect them from the stack pointer).
            // TODO-MIKE-Cleanup: This is likely pointless, lvaOutgoingArgSpaceVar should
            // have ref count 0 at this point so we skip it anyway above.
            noway_assert(compiler->lvaOutgoingArgSpaceVar != BAD_VAR_NUM);

            if (lclNum == compiler->lvaOutgoingArgSpaceVar)
            {
                continue;
            }
#endif // FEATURE_FIXED_OUT_ARGS

            // true when it is likely that this local will have a stack home.
            // TODO-MIKE-Review: Should we check lvTracked too?
            bool onStack = lcl->lvDoNotEnregister || lcl->TypeIs(TYP_BLK) || (regAvailEstimate == 0);

#ifdef TARGET_X86
            // Treat floating point and 64 bit integers as always on the stack
            // TODO-MIKE-Review: What the crap does FP has to do with X86?
            // And LONG locals can get promoted and thus enregistered, this is nonsense.
            if (varTypeIsFloating(lcl->GetType()) || varTypeIsLong(lcl->GetType()))
            {
                onStack = true;
            }
#endif

            if (!onStack)
            {
                // For the purposes of estimating the frameSize we will consider this local
                // as being enregistered. Now we reduce the remaining regAvailEstimate by an
                // appropriate amount.

                if (lcl->GetRefCount() <= 2)
                {
                    regAvailEstimate -= 1;
                }
                else if (regAvailEstimate >= 2)
                {
                    regAvailEstimate -= 2;
                }
                else
                {
                    regAvailEstimate = 0;
                }

                continue;
            }

            frameSize += compiler->lvaLclSize(lclNum);

#ifdef TARGET_XARCH
            if (frameSize > 128)
            {
                // On XARCH stack frame displacements can either use a 1-byte or a 4-byte displacement.
                // With a large franme we will need to use some 4-byte displacements.
                largeFrame = true;
                break;
            }
#elif defined(TARGET_ARM)
            if (frameSize > 1024)
            {
                // We might need to use large displacements when loading or storing to CSE
                // locals that are not enregistered. On ARM32 this means using rsGetRsvdReg()
                // to hold the large displacement.
                largeFrame = true;

                if (frameSize > 65536)
                {
                    hugeFrame = true;
                    break;
                }
            }
#elif defined(TARGET_ARM64)
            if (frameSize > 4096)
            {
                // Thus we might need to use large displacements when loading or storing
                // to CSE local that are not enregistered.
                // On ARM64 this means using rsGetRsvdReg() to hold the large displacement.
                largeFrame = true;
                break;
            }
#endif
        }

        JITDUMP("Framesize estimate is %u (%s)\n", frameSize, hugeFrame ? "huge" : (largeFrame ? "large" : "small"));
    }

    void EstimateCutOffWeights()
    {
        // Set the cut off values to use for deciding when we want to use aggressive or
        // moderate weight.
        //
        // The value of aggressiveWeight and moderateWeight start off as zero and when
        // enregCount reaches a certain value we assign the current local weight to
        // aggressiveWeight or moderateWeight.
        //
        // On Windows x64 this yeilds aggressiveEnregNum == 12 and moderateEnregNum == 38
        // thus we will typically set the cutoff values for
        //   aggressiveWeight based upon the weight of the 13th tracked local
        //   moderateWeight based upon the weight of the 39th tracked local
        // For other architecture and platforms these values dynamically change
        // based upon the number of callee saved and callee scratch registers.
        const unsigned aggressiveEnregNum = CNT_CALLEE_ENREG * 3 / 2;
        const unsigned moderateEnregNum   = (CNT_CALLEE_ENREG * 3) + (CNT_CALLEE_TRASH * 2);

        // Iterate over the sorted list of tracked local variables these are the register candidates
        // for LSRA. We normally vist locals in order of their weighted ref counts and our heuristic
        // assumes that the highest weight local swill be enregistered and that the lowest weight
        // locals are likely be allocated in the stack frame.
        // The value of enregCount is incremented when we visit a local that can be enregistered.
        for (unsigned trackedIndex = 0; trackedIndex < compiler->lvaTrackedCount; trackedIndex++)
        {
            LclVarDsc* lcl = compiler->lvaGetDescByTrackedIndex(trackedIndex);

            if (lcl->GetRefCount() == 0)
            {
                continue;
            }

            if (lcl->lvDoNotEnregister || lcl->TypeIs(TYP_BLK))
            {
                continue;
            }

            // The enregCount only tracks the uses of integer registers.
            // We could track floating point register usage seperately but it isn't worth
            // the additional complexity as floating point CSEs are rare and we typically
            // have plenty of floating point register available.
            if (!varTypeIsFloating(lcl->GetType()))
            {
                enregCount++;

#ifndef TARGET_64BIT
                if (lcl->TypeIs(TYP_LONG))
                {
                    enregCount++;
                }
#endif
            }

            if ((aggressiveWeight == 0) && (enregCount > aggressiveEnregNum))
            {
                if (codeOptKind == Compiler::SMALL_CODE)
                {
                    aggressiveWeight = static_cast<BasicBlock::weight_t>(lcl->GetRefCount());
                }
                else
                {
                    aggressiveWeight = lcl->GetRefWeight();
                }

                aggressiveWeight += BB_UNITY_WEIGHT;
            }

            if ((moderateWeight == 0) && (enregCount > moderateEnregNum))
            {
                if (codeOptKind == Compiler::SMALL_CODE)
                {
                    moderateWeight = static_cast<BasicBlock::weight_t>(lcl->GetRefCount());
                }
                else
                {
                    moderateWeight = lcl->GetRefWeight();
                }

                moderateWeight += (BB_UNITY_WEIGHT / 2);
            }
        }

        aggressiveWeight = max(BB_UNITY_WEIGHT * 2, aggressiveWeight);
        moderateWeight   = max(BB_UNITY_WEIGHT, moderateWeight);

        JITDUMP("Aggressive CSE Promotion cutoff is %f\n", aggressiveWeight);
        JITDUMP("Moderate CSE Promotion cutoff is %f\n", moderateWeight);
        JITDUMP("EnregCount is %u\n", enregCount);
    }

    struct CostCompareSpeed
    {
        bool operator()(const CseDesc* dsc1, const CseDesc* dsc2)
        {
            GenTree* exp1 = dsc1->firstOccurence.expr;
            GenTree* exp2 = dsc2->firstOccurence.expr;

            auto expCost1 = exp1->GetCostEx();
            auto expCost2 = exp2->GetCostEx();

            if (expCost2 != expCost1)
            {
                return expCost2 < expCost1;
            }

            if (dsc2->useWeight != dsc1->useWeight)
            {
                return dsc2->useWeight < dsc1->useWeight;
            }

            if (dsc1->defWeight != dsc2->defWeight)
            {
                return dsc1->defWeight < dsc2->defWeight;
            }

            // Ensure a stable sort order
            return dsc1->index < dsc2->index;
        }
    };

    struct CostCompareSize
    {
        bool operator()(const CseDesc* dsc1, const CseDesc* dsc2)
        {
            GenTree* exp1 = dsc1->firstOccurence.expr;
            GenTree* exp2 = dsc2->firstOccurence.expr;

            auto expCost1 = exp1->GetCostSz();
            auto expCost2 = exp2->GetCostSz();

            if (expCost2 != expCost1)
            {
                return expCost2 < expCost1;
            }

            if (dsc2->useCount != dsc1->useCount)
            {
                return dsc2->useCount < dsc1->useCount;
            }

            if (dsc1->defCount != dsc2->defCount)
            {
                return dsc1->defCount < dsc2->defCount;
            }

            // Ensure a stable sort order
            return dsc1->index < dsc2->index;
        }
    };

    CseDesc** SortCandidates()
    {
        CseDesc** sorted = new (allocator) CseDesc*[descCount];
        memcpy(sorted, descTable, descCount * sizeof(*sorted));

        if (codeOptKind == Compiler::SMALL_CODE)
        {
            jitstd::sort(sorted, sorted + descCount, CostCompareSize());
        }
        else
        {
            jitstd::sort(sorted, sorted + descCount, CostCompareSpeed());
        }

        DBEXEC(compiler->verbose, DumpSortedCandidates(sorted));

        return sorted;
    }

#ifdef DEBUG
    void DumpSortedCandidates(CseDesc** sorted)
    {
        printf("\nSorted CSE candidates:\n");

        for (unsigned cnt = 0; cnt < descCount; cnt++)
        {
            CseDesc* dsc  = sorted[cnt];
            GenTree* expr = dsc->firstOccurence.expr;

            BasicBlock::weight_t def;
            BasicBlock::weight_t use;
            unsigned             cost;

            if (codeOptKind == Compiler::SMALL_CODE)
            {
                def  = dsc->defCount;
                use  = dsc->useCount;
                cost = dsc->firstOccurence.expr->GetCostSz();
            }
            else
            {
                def  = dsc->defWeight;
                use  = dsc->useWeight;
                cost = dsc->firstOccurence.expr->GetCostEx();
            }

            printf(FMT_CSE " {" FMT_VN ", " FMT_VN "} useCount %d def %3f use %3f cost %u%s", dsc->index, dsc->hashVN,
                   dsc->defExcSetPromise, dsc->useCount, def, use, cost, dsc->isLiveAcrossCall ? " call" : "");

            if (dsc->isSharedConst)
            {
                printf(" sharedConst %p", dspPtr(vnStore->CoercedConstantValue<size_t>(dsc->hashVN)));
            }

            printf("\n        ");

            compiler->gtDispTree(expr, nullptr, nullptr, true);
        }
        printf("\n");
    }
#endif // DEBUG

    struct Candidate
    {
        CseDesc* const desc;
        GenTree* const expr;
        unsigned const index;
        unsigned const size;
        unsigned const cost;
        float const    defWeight;
        float const    useWeight;
        bool const     isLiveAcrossCall;

        // We believe that the CSE is very valuable in terms of weight,
        // such that it would always be enregistered by the register allocator.
        bool isAggressive;
        // We believe that the CSE is moderately valuable in terms of weight,
        // such that it is more likely than not to be enregistered by the register allocator
        bool isModerate;
        // It's neither aggressive nor moderate.
        // Such candidates typically are expensive to compute and thus are
        // always profitable to promote even when they aren't enregistered.
        bool isConservative;
        // Candidate is only being promoted because of a Stress mode.
        bool isStress;

        Candidate(Compiler::codeOptimize codeOptKind, CseDesc* desc)
            : desc(desc)
            , expr(desc->firstOccurence.expr)
            , index(desc->index)
            , size(desc->firstOccurence.expr->GetCostSz())
            , cost(codeOptKind == Compiler::SMALL_CODE ? size : desc->firstOccurence.expr->GetCostEx())
            , defWeight(codeOptKind == Compiler::SMALL_CODE ? desc->defCount : desc->defWeight)
            , useWeight(codeOptKind == Compiler::SMALL_CODE ? desc->useCount : desc->useWeight)
            , isLiveAcrossCall(desc->isLiveAcrossCall)
            , isAggressive(false)
            , isModerate(false)
            , isConservative(false)
            , isStress(false)
        {
        }
    };

#ifdef DEBUG
    // Stress mode to shuffle the decision to CSE or not using environment
    // variable COMPlus_JitStressBiasedCSE (= 0 to 100%). When the bias value
    // is not specified but COMPlus_JitStress is ON, generate a random bias.
    //
    // Return Value:
    //   0 -- This method is indifferent about this CSE (no bias specified and no stress)
    //   1 -- This CSE must be performed to maintain specified/generated bias.
    //  -1 -- This CSE mustn't be performed to maintain specified/generated bias.
    //
    // Operation:
    //   A debug stress only method that returns "1" with probability (P)
    //   defined by:
    //
    //       P = (COMPlus_JitStressBiasedCSE / 100) (or)
    //       P = (random(100) / 100) when COMPlus_JitStress is specified and
    //                               COMPlus_JitStressBiasedCSE is unspecified.
    //
    //   When specified, the bias is reinterpreted as a decimal number between 0
    //   to 100.
    //   When bias is not specified, a bias is randomly generated if COMPlus_JitStress
    //   is non-zero.
    //
    //   Callers are supposed to call this method for each CSE promotion decision
    //   and ignore the call if return value is 0 and honor the 1 with a CSE and
    //   -1 with a no-CSE to maintain the specified/generated bias.
    //
    int ConfigBiasedCse()
    {
        // Seed the PRNG, if never done before.
        if (!m_cseRNG.IsInitialized())
        {
            m_cseRNG.Init(compiler->info.compMethodHash());
            m_bias = m_cseRNG.Next(100);
        }

        // Obtain the bias value and reinterpret as decimal.
        unsigned bias = ReinterpretHexAsDecimal(JitConfig.JitStressBiasedCSE());

        // Invalid value, check if JitStress is ON.
        if (bias > 100)
        {
            if (!compiler->compStressCompile(Compiler::STRESS_MAKE_CSE, MAX_STRESS_WEIGHT))
            {
                // JitStress is OFF for CSE, nothing to do.
                return 0;
            }
            bias = m_bias;
            JITDUMP("JitStressBiasedCSE is OFF, but JitStress is ON: generated bias=%d.\n", bias);
        }

        // Generate a number between (0, 99) and if the generated
        // number is smaller than bias, then perform CSE.
        unsigned gen = m_cseRNG.Next(100);
        int      ret = (gen < bias) ? 1 : -1;

        if (ret < 0)
        {
            JITDUMP("No CSE because gen=%d >= bias=%d\n", gen, bias);
        }
        else
        {
            JITDUMP("Promoting CSE because gen=%d < bias=%d\n", gen, bias);
        }

        // Indicate whether to perform CSE or not.
        return ret;
    }

    // A Debug only method that allows you to control whether the CSE logic is enabled for
    // a particular CSE in a method
    //
    // If this method returns false then the CSE should be performed.
    // If the method returns true then the CSE should be skipped.
    //
    bool ConfigDisableCse2()
    {
        static unsigned totalCSEcount = 0;

        unsigned jitNoCSE2 = JitConfig.JitNoCSE2();

        totalCSEcount++;

        if (jitNoCSE2 > 0)
        {
            if ((jitNoCSE2 & 0xF000000) == 0xF000000)
            {
                unsigned totalCSEMask = totalCSEcount & 0xFFF;
                unsigned bitsZero     = (jitNoCSE2 >> 12) & 0xFFF;
                unsigned bitsOne      = (jitNoCSE2 >> 0) & 0xFFF;

                if (((totalCSEMask & bitsOne) == bitsOne) && ((~totalCSEMask & bitsZero) == bitsZero))
                {
                    JITDUMP(" Disabled by jitNoCSE2 Ones/Zeros mask\n");
                    return true;
                }
            }
            else if ((jitNoCSE2 & 0xF000000) == 0xE000000)
            {
                unsigned totalCSEMask = totalCSEcount & 0xFFF;
                unsigned disableMask  = jitNoCSE2 & 0xFFF;

                disableMask >>= (totalCSEMask % 12);

                if (disableMask & 1)
                {
                    JITDUMP(" Disabled by jitNoCSE2 rotating disable mask\n");
                    return true;
                }
            }
            else if (jitNoCSE2 <= totalCSEcount)
            {
                JITDUMP(" Disabled by jitNoCSE2 > totalCSEcount\n");
                return true;
            }
        }
        return false;
    }
#endif // DEBUG

    bool PromotionCheck(Candidate& candidate)
    {
        if (varTypeIsSIMD(candidate.expr->GetType()) && (candidate.desc->layout == nullptr))
        {
            // If we haven't found an exact layout yet then use any layout that happens
            // to have the same SIMD type as the expression. Even so, it's possible to
            // not have a layout, usually due to SIMD typed nodes generated internally
            // by the JIT, in such cases there's no struct handle and the layout table
            // isn't populated. In such a case we don't have any option but to reject
            // this CSE.

            // TODO-MIKE-Cleanup: Do we really need this? There are other places that
            // create SIMD temps without layout so why bother at all here?

            ClassLayout* layout = compiler->typGetVectorLayout(candidate.expr);

            if (layout == nullptr)
            {
                return false;
            }

            candidate.desc->layout = layout;
        }

#ifdef DEBUG
        if (ConfigBiasedCse() > 0)
        {
            candidate.isStress = true;
            return true;
        }

        if (ConfigDisableCse2())
        {
            return false;
        }
#endif

        // Our calculation is based on the following cost estimate formula
        //
        // Existing costs are:
        //
        // (def + use) * cost
        //
        // If we introduce a CSE temp are each definition and
        // replace the use with a CSE temp then our cost is:
        //
        // (def * (cost + cse-def-cost)) + (use * cse-use-cost)
        //
        // We must estimate the values to use for cse-def-cost and cse-use-cost
        //
        // If we are able to enregister the CSE then the cse-use-cost is one
        // and cse-def-cost is either zero or one.  Zero in the case where
        // we needed to evaluate the def into a register and we can use that
        // register as the CSE temp as well.
        //
        // If we are unable to enregister the CSE then the cse-use-cost is IND_COST
        // and the cse-def-cost is also IND_COST.
        //
        // If we want to be conservative we use IND_COST as the the value
        // for both cse-def-cost and cse-use-cost and then we never introduce
        // a CSE that could pessimize the execution time of the method.
        //
        // If we want to be more moderate we use (IND_COST_EX + 1) / 2 as the
        // values for both cse-def-cost and cse-use-cost.
        //
        // If we want to be aggressive we use 1 as the values for both
        // cse-def-cost and cse-use-cost.
        //
        // If we believe that the CSE very valuable in terms of weight ref counts
        // such that it would always be enregistered by the register allocator we choose
        // the aggressive use def costs.
        //
        // If we believe that the CSE is somewhat valuable in terms of weighted ref counts
        // such that it could be likely be enregistered by the register allocator we choose
        // the moderate use def costs.
        //
        // otherwise we choose the conservative use def costs.

        // The estimated weight of the temp local we create for this CSE.
        // The def also implies a use so we consider it twice.
        float lclWeight = (candidate.defWeight * 2) + candidate.useWeight;

        unsigned defCost;
        unsigned useCost;

        if (codeOptKind == Compiler::SMALL_CODE)
        {
            // When optimizing for small code we set the costs based upon
            // the code size and we use ref counts instead of weights.

            if (lclWeight >= aggressiveWeight)
            {
                JITDUMP("Aggressive CSE Promotion (%f >= %f)\n", lclWeight, aggressiveWeight);

                candidate.isAggressive = true;

                // We expect that the candidate will be enregistered so we have minimum costs.
                defCost = 1;
                useCost = 1;

                if (candidate.isLiveAcrossCall)
                {
                    if (largeFrame)
                    {
                        defCost++;
                        useCost++;
                    }

                    if (hugeFrame)
                    {
                        defCost++;
                        useCost++;
                    }
                }
            }
            else
            {
                candidate.isConservative = true;

                if (largeFrame)
                {
                    JITDUMP("Codesize CSE Promotion (%s frame)\n", hugeFrame ? "huge" : "large");

#ifdef TARGET_XARCH
                    defCost = 6; // mov [EBP-0x00001FC], reg
                    useCost = 5; //     [EBP-0x00001FC]
#else
                    if (hugeFrame)
                    {
                        defCost = 10 + 2; // movw/movt r10 and str reg,[sp+r10]
                        useCost = 10 + 2;
                    }
                    else
                    {
                        defCost = 6 + 2; // movw r10 and str reg,[sp+r10]
                        useCost = 6 + 2;
                    }
#endif
                }
                else
                {
                    JITDUMP("Codesize CSE Promotion (small frame)\n");

#ifdef TARGET_XARCH
                    defCost = 3; // mov [EBP-1C], reg
                    useCost = 2; //     [EBP-1C]
#else
                    defCost = 2; // str reg, [sp+0x9c]
                    useCost = 2; // ldr reg, [sp+0x9c]
#endif
                }
            }

#ifdef TARGET_AMD64
            if (varTypeIsFloating(candidate.expr->GetType()))
            {
                // Floating point loads/store have larger encodings.
                // TODO-MIKE-Review: This should also apply to vector types (varTypeUsesFloatReg).
                defCost += 2;
                useCost += 1;
            }
#endif
        }
        else
        {
            // When optimizing for blended/fast code we set costs based upon the
            // execution costs of the code and we use weights instead of ref counts.

            if (lclWeight >= aggressiveWeight)
            {
                candidate.isAggressive = true;

                JITDUMP("Aggressive CSE Promotion (%f >= %f)\n", lclWeight, aggressiveWeight);

                // We expect that the candidate will be enregistered so we have minimum costs.
                defCost = 1;
                useCost = 1;
            }
            else if (lclWeight >= moderateWeight)
            {
                candidate.isModerate = true;

                if (!candidate.isLiveAcrossCall)
                {
                    JITDUMP("Moderate CSE Promotion (never live at call) (%f >= %f)\n", lclWeight, moderateWeight);

                    defCost = 2;
                    useCost = 1;
                }
                else
                {
                    JITDUMP("Moderate CSE Promotion (live across a call) (%f >= %f)\n", lclWeight, moderateWeight);

                    defCost = 2;

                    if (enregCount < (CNT_CALLEE_ENREG * 3 / 2))
                    {
                        useCost = 1;
                    }
                    else
                    {
                        useCost = 2;
                    }
                }
            }
            else
            {
                candidate.isConservative = true;

                if (!candidate.isLiveAcrossCall)
                {
                    JITDUMP("Conservative CSE Promotion (not enregisterable) (%f < %f)\n", lclWeight, moderateWeight);

                    defCost = 2;
                    useCost = 2;
                }
                else
                {
                    JITDUMP("Conservative CSE Promotion (%f < %f)\n", lclWeight, moderateWeight);

                    defCost = 2;
                    useCost = 3;
                }

                // If we have maxed out lvaTrackedCount then this CSE may end up as an untracked variable
                if (compiler->lvaTrackedCount == static_cast<unsigned>(JitConfig.JitMaxLocalsToTrack()))
                {
                    defCost += 1;
                    useCost += 1;
                }
            }
        }

        unsigned extraYesCost = 0;

        // If this CSE is live across a call then we may need to spill an additional caller save register.
        if (candidate.isLiveAcrossCall)
        {
            if (candidate.expr->IsDblCon() && (CNT_CALLEE_SAVED_FLOAT == 0) && (candidate.desc->useWeight <= 4))
            {
                // Floating point constants are expected to be contained, so unless there are
                // more than 4 uses we better not to CSE them, especially on platforms without
                // callee-saved registers for values living across calls
                return false;
            }

            // If we don't have a lot of variables to enregister or we have a floating point type
            // then we will likely need to spill an additional caller save register.
            if ((enregCount < (CNT_CALLEE_ENREG * 3 / 2)) || varTypeIsFloating(candidate.expr->GetType()))
            {
                // Extra cost in case we have to spill/restore a caller saved register
                extraYesCost = BB_UNITY_WEIGHT_UNSIGNED;

                if (lclWeight < moderateWeight)
                {
                    extraYesCost *= 2;
                }
            }

#ifdef FEATURE_SIMD
            // Vector types may cause a vector register to be spilled/restored in the prolog and epilog.
            if (varTypeIsSIMD(candidate.expr->GetType()))
            {
                // We don't have complete information about when these extra spilled/restore will be needed.
                // Instead we are conservative and assume that each vector CSE that is live across a call
                // will cause an additional spill/restore in the prolog and epilog.
                int spillVectorRegInProlog = 1;

                if (candidate.expr->TypeIs(TYP_SIMD32))
                {
                    // Additionally, for a SIMD32 CSE candidate we assume that a second spill/restore is
                    // needed, to hold the upper half of the SIMD32 register that isn't preserved across
                    // the call.
                    spillVectorRegInProlog++;

                    // We also increase the CSE use cost here to because we may have to generate instructions
                    // to move the upper half of the SIMD32 before and after a call.
                    useCost += 2;
                }

                extraYesCost = (BB_UNITY_WEIGHT_UNSIGNED * spillVectorRegInProlog) * 3;
            }
#endif // FEATURE_SIMD
        }

        unsigned extraNoCost = 0;

        // Estimate the cost from lost code size reduction if we do not perform the CSE.
        if (candidate.size > useCost)
        {
            extraNoCost = (candidate.size - useCost) * candidate.desc->useCount * 2;
        }

        // Cost estimate when we decide not to make a CSE.
        float noCseCost = candidate.useWeight * candidate.cost + extraNoCost;
        // Cost estimate when we decide to make a CSE.
        float yesCseCost = (candidate.defWeight * defCost) + (candidate.useWeight * useCost) + extraYesCost;

#ifdef DEBUG
        if (compiler->verbose)
        {
            printf("lclWeight=%f, aggressiveWeight=%f, moderateWeight=%f\n", lclWeight, aggressiveWeight,
                   moderateWeight);
            printf("defCnt=%f, useCnt=%f, cost=%d, size=%d%s\n", candidate.defWeight, candidate.useWeight,
                   candidate.cost, candidate.size, candidate.isLiveAcrossCall ? ", LiveAcrossCall" : "");
            printf("defCost=%d, useCost=%d, extraNoCost=%d, extraYesCost=%d\n", defCost, useCost, extraNoCost,
                   extraYesCost);
            printf("CSE cost savings check (%f >= %f) %s\n", noCseCost, yesCseCost,
                   noCseCost >= yesCseCost ? "passes" : "fails");
        }
#endif

        if (yesCseCost <= noCseCost)
        {
            return true;
        }

#ifdef DEBUG
        if (noCseCost > 0)
        {
            int percentage = static_cast<int>((noCseCost * 100) / yesCseCost);

            if (compiler->compStressCompile(Compiler::STRESS_MAKE_CSE, percentage))
            {
                return true;
            }
        }
#endif

        return false;
    }

    static bool IsCompatibleType(var_types lclType, var_types exprType)
    {
        return (lclType == exprType) || ((lclType == TYP_BYREF) && (exprType == TYP_I_IMPL)) ||
               ((lclType == TYP_I_IMPL) && (exprType == TYP_BYREF));
    }

    // PerformCSE() takes a successful candidate and performs  the appropriate replacements:
    //
    // It will replace all of the CSE defs with assignments to a new "cse0" LclVar
    // and will replace all of the CSE uses with reads of the "cse0" LclVar
    //
    // It will also put cse0 into SSA if there is just one def.
    void PerformCSE(const Candidate& candidate)
    {
        JITDUMP("\nPromoting CSE:\n");

        BasicBlock::weight_t cseRefCnt = (candidate.defWeight * 2) + candidate.useWeight;

        if (candidate.isLiveAcrossCall != 0)
        {
            // As we introduce new LclVars for these CSE we slightly
            // increase the cutoffs for aggressive and moderate CSE's
            BasicBlock::weight_t incr = BB_UNITY_WEIGHT;

            if (cseRefCnt > aggressiveWeight)
            {
                aggressiveWeight += incr;
            }

            if (cseRefCnt > moderateWeight)
            {
                moderateWeight += (incr / 2);
            }
        }

#ifdef DEBUG
        const char* grabTempMessage = "CSE - unknown";

        if (candidate.isAggressive)
        {
            grabTempMessage = "CSE - aggressive";
        }
        else if (candidate.isModerate)
        {
            grabTempMessage = "CSE - moderate";
        }
        else if (candidate.isConservative)
        {
            grabTempMessage = "CSE - conservative";
        }
        else if (candidate.isStress)
        {
            grabTempMessage = "CSE - stress mode";
        }
#endif // DEBUG

        unsigned   cseLclVarNum = compiler->lvaGrabTemp(false DEBUGARG(grabTempMessage));
        var_types  cseLclVarTyp = varActualType(candidate.expr->GetType());
        LclVarDsc* cseLcl       = compiler->lvaGetDesc(cseLclVarNum);

        if (varTypeIsStruct(cseLclVarTyp))
        {
            compiler->lvaSetStruct(cseLclVarNum, candidate.desc->layout, false);
            assert(cseLcl->GetType() == cseLclVarTyp);
        }
        else
        {
            cseLcl->SetType(cseLclVarTyp);
        }

        cseLcl->lvIsCSE = true;

        INDEBUG(compiler->cseCount++);

        // Walk all references to this CSE, adding an assignment
        // to the CSE temp to all defs and changing all refs to
        // a simple use of the CSE temp.
        // Later we will unmark any nested CSE's for the CSE uses.
        CseDesc* dsc = candidate.desc;

        // If there's just a single def for the CSE, we'll put this
        // CSE into SSA form on the fly. We won't need any PHIs.
        unsigned cseSsaNum = SsaConfig::RESERVED_SSA_NUM;

        if (dsc->defCount == 1)
        {
            JITDUMP(FMT_CSE " is single-def, so associated CSE temp V%02u will be in SSA\n", dsc->index, cseLclVarNum);
            cseLcl->lvInSsa = true;

            cseSsaNum = cseLcl->lvPerSsaData.AllocSsaNum(compiler->getAllocator(CMK_SSA));
        }

        bool     setRefCnt      = true;
        bool     isSharedConst  = dsc->isSharedConst;
        bool     isSameConst    = true;
        bool     baseConstIsDef = false;
        ssize_t  baseConstVal   = 0;
        ValueNum baseConstVN    = NoVN;

        for (CseOccurence* lst = &dsc->firstOccurence; lst != nullptr; lst = lst->next)
        {
            GenTree* expr = lst->expr;

            if (!IsCseIndex(expr->gtCSEnum))
            {
                continue;
            }

            bool isDef = IsCseDef(expr->gtCSEnum);

            if (isSharedConst)
            {
                ValueNum exprConstVN    = expr->GetLiberalVN();
                ssize_t  exprConstValue = expr->AsIntCon()->GetValue();

                if (baseConstVN == ValueNumStore::NoVN)
                {
                    baseConstVN = exprConstVN;

                    if (isSharedConst)
                    {
                        baseConstVal   = exprConstValue;
                        baseConstIsDef = isDef;
                    }
                }
                else if (exprConstVN != baseConstVN)
                {
                    isSameConst = false;

                    ssize_t delta = exprConstValue - baseConstVal;

                    // The ARM addressing modes allow for a subtraction of up to 255 so we
                    // will allow the delta to be up to -255 before replacing a CSE def.
                    // This will minimize the number of extra subtract instructions.
                    if ((baseConstIsDef && (delta < -255)) || (!baseConstIsDef && (delta < 0)))
                    {
                        baseConstVN    = exprConstVN;
                        baseConstVal   = exprConstValue;
                        baseConstIsDef = isDef;
                    }
                }
            }

            BasicBlock*          blk       = lst->block;
            BasicBlock::weight_t curWeight = blk->getBBWeight(compiler);

            if (setRefCnt)
            {
                cseLcl->SetRefCount(1);
                cseLcl->SetRefWeight(curWeight);
                setRefCnt = false;
            }
            else
            {
                compiler->lvaAddRef(cseLcl, curWeight);
            }

            // A CSE Def references the LclVar twice
            if (isDef)
            {
                compiler->lvaAddRef(cseLcl, curWeight);
            }
        }

        if (isSharedConst)
        {
            JITDUMP("Using 0x%p (" FMT_VN ") as the base value for shared const CSE.\n", dspPtr(baseConstVal),
                    baseConstVN);
        }

        for (CseOccurence* lst = &dsc->firstOccurence; lst != nullptr; lst = lst->next)
        {
            GenTree*    exp  = lst->expr;
            Statement*  stmt = lst->stmt;
            BasicBlock* blk  = lst->block;

            // We may have cleared this CSE in optValuenumCSE_Availablity
            // due to different exception sets.
            // Ignore this node if the gtCSEnum value has been cleared.
            if (!IsCseIndex(exp->gtCSEnum))
            {
                continue;
            }

            assert(exp->gtOper != GT_COUNT);

            compiler->cseBlockWeight = blk->getBBWeight(compiler);

            // Figure out the actual type of the value
            var_types expTyp = genActualType(exp->TypeGet());

            // The cseLclVarType must be a compatible with expTyp
            noway_assert(IsCompatibleType(cseLclVarTyp, expTyp) || (baseConstVN != vnStore->VNForNull()));

            // This will contain the replacement tree for exp
            GenTree*      cse = nullptr;
            bool          isDef;
            GenTree*      effectiveExp = exp->SkipComma();
            FieldSeqNode* fieldSeq     = compiler->GetZeroOffsetFieldSeq(effectiveExp);

            if (IsCseUse(exp->gtCSEnum))
            {
                isDef = false;

                JITDUMP("\nWorking on the replacement of the " FMT_CSE " use at [%06u] in " FMT_BB "\n", exp->gtCSEnum,
                        exp->GetID(), blk->bbNum);

                // We will replace the CSE ref with a new tree
                // this is typically just a simple use of the new CSE LclVar
                // Create a reference to the CSE temp.

                GenTree* cseLclVar = compiler->gtNewLclvNode(cseLclVarNum, cseLclVarTyp);

                // Assign the ssa num for the lclvar use. Note it may be the reserved num.
                cseLclVar->AsLclVarCommon()->SetSsaNum(cseSsaNum);

                cse = cseLclVar;

                if (isSharedConst)
                {
                    ssize_t exprConsVal = exp->AsIntCon()->GetValue();
                    ssize_t delta       = exprConsVal - baseConstVal;

                    if (delta != 0)
                    {
                        cseLclVar->SetVNP(ValueNumPair(baseConstVN));

                        GenTree* deltaNode = compiler->gtNewIconNode(delta, cseLclVarTyp);
                        compiler->fgValueNumberTreeConst(deltaNode);
                        cse = compiler->gtNewOperNode(GT_ADD, cseLclVarTyp, cseLclVar, deltaNode);
                        cse->SetDoNotCSE();
                    }
                }

                // assign the proper ValueNumber, A CSE use discards any exceptions
                cse->gtVNPair = vnStore->VNPNormalPair(exp->gtVNPair);

                // shared const CSE has the correct value number assigned
                // and both liberal and conservative are identical
                // and they do not use theConservativeVN
                if (!isSharedConst)
                {
                    ValueNum theConservativeVN = candidate.desc->defConservNormVN;

                    if (theConservativeVN != ValueNumStore::NoVN)
                    {
                        // All defs of this CSE share the same normal conservative VN, and we are rewriting this
                        // use to fetch the same value with no reload, so we can safely propagate that
                        // conservative VN to this use.  This can help range check elimination later on.
                        cse->gtVNPair.SetConservative(theConservativeVN);

                        // If the old VN was flagged as a checked bound, propagate that to the new VN
                        // to make sure assertion prop will pay attention to this VN.
                        ValueNum oldVN = exp->gtVNPair.GetConservative();
                        if (!vnStore->IsVNConstant(theConservativeVN) && vnStore->IsVNCheckedBound(oldVN))
                        {
                            vnStore->SetVNIsCheckedBound(theConservativeVN);
                        }

                        GenTree* cmp;
                        if (checkedBoundMap.Lookup(exp, &cmp))
                        {
                            // Propagate the new value number to this compare node as well, since
                            // subsequent range check elimination will try to correlate it with
                            // the other appearances that are getting CSEd.

                            ValueNum oldCmpVN = cmp->gtVNPair.GetConservative();
                            ValueNum newCmpArgVN;

                            VNFuncApp oldFuncApp;

                            if (vnStore->GetVNFunc(oldCmpVN, &oldFuncApp) &&
                                ValueNumStore::IsVNCompareCheckedBoundRelop(oldFuncApp))
                            {
                                ValueNumStore::CompareCheckedBoundArithInfo info;

                                if (vnStore->IsVNCompareCheckedBound(oldFuncApp))
                                {
                                    // Comparison is against the bound directly.

                                    newCmpArgVN = theConservativeVN;
                                    vnStore->GetCompareCheckedBound(oldFuncApp, &info);
                                }
                                else
                                {
                                    // Comparison is against the bound +/- some offset.

                                    vnStore->GetCompareCheckedBoundArithInfo(oldFuncApp, &info);
                                    newCmpArgVN =
                                        vnStore->VNForFunc(vnStore->TypeOfVN(info.arrOp), (VNFunc)info.arrOper,
                                                           info.arrOp, theConservativeVN);
                                }

                                ValueNum newCmpVN = vnStore->VNForFunc(vnStore->TypeOfVN(oldCmpVN),
                                                                       (VNFunc)info.cmpOper, info.cmpOp, newCmpArgVN);
                                cmp->gtVNPair.SetConservative(newCmpVN);
                            }
                        }
                    }
                }

                // Now we need to unmark any nested CSE's uses that are found in 'exp'
                // As well we extract any nested CSE defs that are found in 'exp' and
                // these are appended to the sideEffList

                // Afterwards the set of nodes in the 'sideEffectList' are preserved and
                // all other nodes are removed.
                exp->gtCSEnum = NoCse;

                GenTree* sideEffList = nullptr;
                compiler->gtExtractSideEffList(exp, &sideEffList, GTF_PERSISTENT_SIDE_EFFECTS | GTF_IS_IN_CSE);

                if (sideEffList != nullptr)
                {
                    JITDUMPTREE(sideEffList,
                                "\nThis CSE use has side effects and/or nested CSE defs. The sideEffectList:\n");

                    GenTree*     cseVal         = cse;
                    GenTree*     curSideEff     = sideEffList;
                    ValueNumPair exceptions_vnp = ValueNumStore::VNPForEmptyExcSet();

                    while ((curSideEff->OperGet() == GT_COMMA) || (curSideEff->OperGet() == GT_ASG))
                    {
                        GenTree* op1 = curSideEff->AsOp()->gtOp1;
                        GenTree* op2 = curSideEff->AsOp()->gtOp2;

                        ValueNumPair op1vnp;
                        ValueNumPair op1Xvnp = ValueNumStore::VNPForEmptyExcSet();
                        vnStore->VNPUnpackExc(op1->gtVNPair, &op1vnp, &op1Xvnp);

                        exceptions_vnp = vnStore->VNPExcSetUnion(exceptions_vnp, op1Xvnp);
                        curSideEff     = op2;
                    }

                    // We may have inserted a narrowing cast during a previous remorph
                    // and it will not have a value number.
                    if ((curSideEff->OperGet() == GT_CAST) && !curSideEff->gtVNPair.BothDefined())
                    {
                        // The inserted cast will have no exceptional effects
                        assert(curSideEff->gtOverflow() == false);
                        // Process the exception effects from the cast's operand.
                        curSideEff = curSideEff->AsOp()->gtOp1;
                    }

                    ValueNumPair op2vnp;
                    ValueNumPair op2Xvnp = ValueNumStore::VNPForEmptyExcSet();
                    vnStore->VNPUnpackExc(curSideEff->gtVNPair, &op2vnp, &op2Xvnp);
                    exceptions_vnp = vnStore->VNPExcSetUnion(exceptions_vnp, op2Xvnp);

                    op2Xvnp = ValueNumStore::VNPForEmptyExcSet();
                    vnStore->VNPUnpackExc(cseVal->gtVNPair, &op2vnp, &op2Xvnp);
                    exceptions_vnp = vnStore->VNPExcSetUnion(exceptions_vnp, op2Xvnp);

                    // Create a comma node with the sideEffList as op1
                    cse           = compiler->gtNewCommaNode(sideEffList, cseVal, expTyp);
                    cse->gtVNPair = vnStore->VNPWithExc(op2vnp, exceptions_vnp);
                }
            }
            else
            {
                isDef = true;

                JITDUMP("\n" FMT_CSE " def at [%06u] replaced in " FMT_BB " with def of V%02u\n",
                        GetCseIndex(exp->gtCSEnum), exp->GetID(), blk->bbNum, cseLclVarNum);

                exp->gtCSEnum = NoCse;

                GenTree* val = exp;

                if (isSharedConst)
                {
                    ssize_t exprConstVal = exp->AsIntCon()->GetValue();
                    ssize_t delta        = exprConstVal - baseConstVal;

                    if (delta != 0)
                    {
                        val = compiler->gtNewIconNode(baseConstVal, cseLclVarTyp);
                        val->SetVNP(ValueNumPair(baseConstVN));
                    }
                }

                assert((cseLclVarTyp != TYP_STRUCT) ||
                       (val->IsCall() && (val->AsCall()->GetRetDesc()->GetRegCount() == 1)));

                GenTreeLclVar* dstLclNode = compiler->gtNewLclvNode(cseLclVarNum, cseLclVarTyp);
                dstLclNode->SetVNP(val->gtVNPair);
                dstLclNode->SetSsaNum(cseSsaNum);

                GenTreeOp* asg = compiler->gtNewAssignNode(dstLclNode, val);
                asg->gtVNPair.SetBoth(ValueNumStore::VNForVoid());

                if (cseSsaNum != SsaConfig::RESERVED_SSA_NUM)
                {
                    LclSsaVarDsc* ssaVarDsc = compiler->lvaTable[cseLclVarNum].GetPerSsaData(cseSsaNum);

                    // These should not have been set yet, since this is the first and
                    // only def for this CSE.
                    assert(ssaVarDsc->GetBlock() == nullptr);
                    assert(ssaVarDsc->GetAssignment() == nullptr);

                    ssaVarDsc->m_vnPair = val->gtVNPair;
                    ssaVarDsc->SetBlock(blk);
                    ssaVarDsc->SetAssignment(asg->AsOp());
                }

                GenTreeLclVar* cseLclVar = compiler->gtNewLclvNode(cseLclVarNum, cseLclVarTyp);
                cseLclVar->SetSsaNum(cseSsaNum);

                GenTree* cseUse = cseLclVar;

                if (isSharedConst)
                {
                    ssize_t exprConstVal = exp->AsIntCon()->GetValue();
                    ssize_t delta        = exprConstVal - baseConstVal;

                    if (delta != 0)
                    {
                        cseLclVar->SetVNP(ValueNumPair(baseConstVN));

                        GenTree* deltaNode = compiler->gtNewIconNode(delta, cseLclVarTyp);
                        compiler->fgValueNumberTreeConst(deltaNode);
                        cseUse = compiler->gtNewOperNode(GT_ADD, cseLclVarTyp, cseLclVar, deltaNode);
                        cseUse->SetDoNotCSE();
                    }
                }

                cseUse->gtVNPair = val->gtVNPair;

                cse = compiler->gtNewCommaNode(asg, cseUse, expTyp);
                // COMMA's VN is the same the original expression VN because assignment does not add any exceptions.
                cse->gtVNPair = cseUse->gtVNPair;
            }

            Compiler::FindLinkData linkData = compiler->gtFindLink(stmt, exp);
            GenTree**              link     = linkData.useEdge;

#ifdef DEBUG
            if (link == nullptr)
            {
                printf("\ngtFindLink failed: stm=");
                Compiler::printStmtID(stmt);
                printf(", expr=");
                Compiler::printTreeID(exp);
                printf("\n");
                printf("stm =");
                compiler->gtDispStmt(stmt);
                printf("\n");
                printf("expr =");
                compiler->gtDispTree(exp);
                printf("\n");
            }
#endif // DEBUG

            noway_assert(link);

            *link = cse;

            if (fieldSeq != nullptr)
            {
                compiler->AddZeroOffsetFieldSeq(cse, fieldSeq);
            }

            assert(compiler->fgRemoveRestOfBlock == false);

            if (linkData.user != nullptr)
            {
                compiler->gtUpdateTreeAncestorsSideEffects(linkData.user);
            }

            compiler->gtSetStmtInfo(stmt);
            compiler->fgSetStmtSeq(stmt);
        }
    }

    void ConsiderCandidates()
    {
        // cseCanSwapOrder and cseUnmarkNode need the CSE table.
        compiler->cseTable          = descTable;
        compiler->cseCandidateCount = descCount;

        CseDesc** sorted = SortCandidates();

        for (unsigned i = 0; i < descCount; i++)
        {
            CseDesc* desc = sorted[i];

            if ((desc->defCount == 0) || (desc->useCount == 0))
            {
                // If we reach this point, then the CSE def was incorrectly marked or the
                // block with this use is unreachable.
                // The problem is if there is sub-graph that is not reachable from the
                // entry point, the CSE flags propagated, would be incorrect for it.
                JITDUMP("Skipped " FMT_CSE " because def/use count is 0\n", desc->index);
                continue;
            }

            if (desc->defExcSetPromise == ValueNumStore::NoVN)
            {
                JITDUMP("Abandoned " FMT_CSE " because we had defs with different Exc sets\n", desc->index);
                continue;
            }

            Candidate candidate(codeOptKind, desc);

            if (candidate.useWeight == 0)
            {
                JITDUMP("Skipped " FMT_CSE " because use count is 0\n", desc->index);
                continue;
            }

#ifdef DEBUG
            JITDUMP(FMT_CSE " {" FMT_VN ", " FMT_VN "} def %3f use %3f cost %u%s", desc->index, desc->hashVN,
                    desc->defExcSetPromise, candidate.defWeight, candidate.useWeight, candidate.cost,
                    candidate.isLiveAcrossCall ? " call" : "");

            if (desc->isSharedConst)
            {
                JITDUMP(" sharedConst %p", dspPtr(vnStore->CoercedConstantValue<size_t>(desc->hashVN)));
            }

            JITDUMPTREE(candidate.expr, "\nCSE Expression:\n");
#endif

            if (!PromotionCheck(candidate))
            {
                JITDUMP("Did Not promote this CSE\n");
                continue;
            }

            PerformCSE(candidate);
        }
    }
};

void Compiler::cseMain()
{
    assert(ssaForm && (vnStore != nullptr));

    Cse cse(this);
    cse.Run();
}
