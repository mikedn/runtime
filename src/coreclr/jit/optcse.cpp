// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                              OptCSE                                       XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#include "jitstd/algorithm.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

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
#if CSE_CONSTS
#ifdef TARGET_64BIT
        case GT_CNS_LNG:
#endif
        case GT_CNS_INT:
        case GT_CNS_DBL:
        case GT_CNS_STR:
#endif // CSE_CONSTS
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

// The following is the upper limit on how many expressions we'll keep track
// of for the CSE analysis.
constexpr unsigned MAX_CSE_CNT = 64;

//-----------------------------------------------------------------------------------------------------------------
// getCSEnum2bit: Return the normalized index to use in the EXPSET_TP for the CSE with the given CSE index.
// Each GenTree has a `gtCSEnum` field. Zero is reserved to mean this node is not a CSE, positive values indicate
// CSE uses, and negative values indicate CSE defs. The caller must pass a non-zero positive value, as from
// GET_CSE_INDEX().
//
static unsigned genCSEnum2bit(unsigned CSEnum)
{
    assert((CSEnum > 0) && (CSEnum <= MAX_CSE_CNT));
    return CSEnum - 1;
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
            unsigned cseBit = genCSEnum2bit(GetCseIndex(node->gtCSEnum));

            if (IsCseDef(node->gtCSEnum))
            {
                BitVecOps::AddElemD(&data->traits, data->def, cseBit);
            }
            else
            {
                BitVecOps::AddElemD(&data->traits, data->use, cseBit);
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
    GenTree*      tree;
    Statement*    stmt;
    BasicBlock*   block;

    CseOccurence(GenTree* tree, Statement* stmt, BasicBlock* block) : tree(tree), stmt(stmt), block(block)
    {
    }
};

struct CseDesc
{
    CseDesc* nextInBucket;
    size_t   hashKey;

    ssize_t  constDefValue; // When we CSE similar constants, this is the value that we use as the def
    ValueNum constDefVN;    // When we CSE similar constants, this is the VN that we use for the LclVar assignment

    unsigned index; // 1..cseCandidateCount

    bool isSharedConst;
    bool isLiveAcrossCall;

    uint16_t defCount;
    uint16_t useCount;

    BasicBlock::weight_t defWeight; // weighted def count
    BasicBlock::weight_t useWeight; // weighted use count (excluding the implicit uses at defs)

    GenTree*    tree;  // treenode containing the 1st occurrence
    Statement*  stmt;  // stmt containing the 1st occurrence
    BasicBlock* block; // block containing the 1st occurrence

    CseOccurence* treeList; // list of matching tree nodes: head
    CseOccurence* treeLast; // list of matching tree nodes: tail

    ClassLayout* layout;

    ValueNum defExcSetPromise; // The exception set that is now required for all defs of this CSE.
    // This will be set to NoVN if we decide to abandon this CSE
    ValueNum defExcSetCurrent; // The set of exceptions we currently can use for CSE uses.
    ValueNum defConservNormVN; // if all def occurrences share the same conservative normal value
    // number, this will reflect it; otherwise, NoVN.
    // not used for shared const CSE's

    CseDesc(size_t hashKey, GenTree* tree, Statement* stmt, BasicBlock* block)
        : nextInBucket(nullptr)
        , hashKey(hashKey)
        , constDefValue(0)
        , constDefVN(ValueNumStore::VNForNull())
        , index(0)
        , isSharedConst(false)
        , isLiveAcrossCall(false)
        , defCount(0)
        , useCount(0)
        , defWeight(0)
        , useWeight(0)
        , tree(tree)
        , stmt(stmt)
        , block(block)
        , treeList(nullptr)
        , treeLast(nullptr)
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

constexpr CseIndex ToCseIndex(unsigned index)
{
    assert(index < INT8_MAX);
    return static_cast<CseIndex>(index);
}

constexpr CseIndex ToCseDefIndex(CseIndex index)
{
    return static_cast<CseIndex>(-index);
}

constexpr size_t s_optCSEhashSizeInitial  = MAX_CSE_CNT * 2;
constexpr size_t s_optCSEhashGrowthFactor = 2;
constexpr size_t s_optCSEhashBucketSize   = 4;

//-----------------------------------------------------------------------------------------------------------------
// getCSEAvailBit: Return the bit used by CSE dataflow sets (bbCseGen, etc.) for the availability bit for a CSE.
//
static unsigned getCSEAvailBit(unsigned CSEnum)
{
    return genCSEnum2bit(CSEnum) * 2;
}

//-----------------------------------------------------------------------------------------------------------------
// getCSEAvailCrossCallBit: Return the bit used by CSE dataflow sets (bbCseGen, etc.) for the availability bit
// for a CSE considering calls as killing availability bit (see description above).
//
static unsigned getCSEAvailCrossCallBit(unsigned CSEnum)
{
    return getCSEAvailBit(CSEnum) + 1;
}

static bool Is_Shared_Const_CSE(size_t key)
{
    return ((key & TARGET_SIGN_BIT) != 0);
}

// returns the encoded key
static size_t Encode_Shared_Const_CSE_Value(size_t key)
{
    return TARGET_SIGN_BIT | (key >> CSE_CONST_SHARED_LOW_BITS);
}

#ifdef DEBUG
// returns the orginal key
static size_t Decode_Shared_Const_CSE_Value(size_t enckey)
{
    assert(Is_Shared_Const_CSE(enckey));
    return (enckey & ~TARGET_SIGN_BIT) << CSE_CONST_SHARED_LOW_BITS;
}
#endif

class Cse
{
    friend class CseDataFlow;
    friend class CseHeuristic;

    Compiler*      compiler;
    ValueNumStore* vnStore;

    // The current size of hashtable
    size_t hashSize = s_optCSEhashSizeInitial;
    // Number of entries in hashtable
    size_t hashCount = 0;
    // Number of entries before resize
    size_t    hashMaxCountBeforeResize = s_optCSEhashSizeInitial * s_optCSEhashBucketSize;
    CseDesc** hashBuckets;

    typedef JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, GenTree*> NodeToNodeMap;

    // Maps bound nodes to ancestor compares that should be re-numbered
    // with the bound to improve range check elimination.
    NodeToNodeMap checkedBoundMap;

    // BitVec trait information for computing CSE availability using the CseDataFlow algorithm.
    // Two bits are allocated per CSE candidate to compute CSE availability
    // plus an extra bit to handle the initial unvisited case.
    // (See CseDataFlow::EndMerge for an explanation of why this is necessary.)
    //
    // The two bits per CSE candidate have the following meanings:
    //     11 - The CSE is available, and is also available when considering calls as killing availability.
    //     10 - The CSE is available, but is not available when considering calls as killing availability.
    //     00 - The CSE is not available
    //     01 - An illegal combination
    //
    BitVecTraits dataFlowTraits;

    EXPSET_TP callKillsMask; // Computed once - A mask that is used to kill available CSEs at callsites

    bool doCSE = false; // True when we have found a duplicate CSE tree

public:
    Cse(Compiler* compiler)
        : compiler(compiler)
        , vnStore(compiler->vnStore)
        , hashBuckets(new (compiler, CMK_CSE) CseDesc*[s_optCSEhashSizeInitial]())
        , checkedBoundMap(compiler->getAllocator(CMK_CSE))
        , dataFlowTraits(0, compiler)
    {
        compiler->cseTable          = nullptr;
        compiler->cseCandidateCount = 0;
        compiler->cseFirstLclNum    = compiler->lvaCount;
    }

    void Run()
    {
        compiler->csePhase = true;

        INDEBUG(EnsureClearCseNum());

        if (Locate())
        {
            InitDataFlow();
            DataFlow();
            Availablity();
            Heuristic();
        }

        compiler->csePhase = false;
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

    unsigned Index(GenTree* tree, Statement* stmt);
    bool Locate();
    void InitDataFlow();
    void DataFlow();
    void Availablity();
    void Heuristic();
    void BuildCseTable();
    void UpdateCheckedBoundMap(GenTree* compare);
    void DumpDataFlowSet(EXPSET_VALARG_TP set, bool includeBits = true);
};

void Cse::BuildCseTable()
{
    unsigned candidateCount = compiler->cseCandidateCount;

    if (candidateCount == 0)
    {
        return;
    }

    CseDesc** table = new (compiler, CMK_CSE) CseDesc*[candidateCount]();

    for (size_t i = 0; i != hashSize; i++)
    {
        for (CseDesc* desc = hashBuckets[i]; desc != nullptr; desc = desc->nextInBucket)
        {
            if (desc->index != 0)
            {
                noway_assert(desc->index <= candidateCount);

                if (table[desc->index - 1] == nullptr)
                {
                    table[desc->index - 1] = desc;
                }
            }
        }
    }

#ifdef DEBUG
    for (unsigned i = 0; i < candidateCount; i++)
    {
        noway_assert(table[i] != nullptr);
    }
#endif

    compiler->cseTable = table;
}

/*****************************************************************************
 *
 *  Compare function passed to jitstd::sort() by CseHeuristic::SortCandidates
 *  when (CodeOptKind() != Compiler::SMALL_CODE)
 */

struct CseCostCompareSpeed
{
    bool operator()(const CseDesc* dsc1, const CseDesc* dsc2)
    {
        GenTree* exp1 = dsc1->tree;
        GenTree* exp2 = dsc2->tree;

        auto expCost1 = exp1->GetCostEx();
        auto expCost2 = exp2->GetCostEx();

        if (expCost2 != expCost1)
        {
            return expCost2 < expCost1;
        }

        // Sort the higher Use Counts toward the top
        if (dsc2->useWeight != dsc1->useWeight)
        {
            return dsc2->useWeight < dsc1->useWeight;
        }

        // With the same use count, Sort the lower Def Counts toward the top
        if (dsc1->defWeight != dsc2->defWeight)
        {
            return dsc1->defWeight < dsc2->defWeight;
        }

        // In order to ensure that we have a stable sort, we break ties using the index
        return dsc1->index < dsc2->index;
    }
};

/*****************************************************************************
 *
 *  Compare function passed to jitstd::sort() by CseHeuristic::SortCandidates
 *  when (CodeOptKind() == Compiler::SMALL_CODE)
 */

struct CseCostCompareSize
{
    bool operator()(const CseDesc* dsc1, const CseDesc* dsc2)
    {
        GenTree* exp1 = dsc1->tree;
        GenTree* exp2 = dsc2->tree;

        auto expCost1 = exp1->GetCostSz();
        auto expCost2 = exp2->GetCostSz();

        if (expCost2 != expCost1)
        {
            return expCost2 < expCost1;
        }

        // Sort the higher Use Counts toward the top
        if (dsc2->useCount != dsc1->useCount)
        {
            return dsc2->useCount < dsc1->useCount;
        }

        // With the same use count, Sort the lower Def Counts toward the top
        if (dsc1->defCount != dsc2->defCount)
        {
            return dsc1->defCount < dsc2->defCount;
        }

        // In order to ensure that we have a stable sort, we break ties using the index
        return dsc1->index < dsc2->index;
    }
};

unsigned optCSEKeyToHashIndex(size_t key, size_t optCSEhashSize)
{
    unsigned hash;

    hash = (unsigned)key;
#ifdef TARGET_64BIT
    hash ^= (unsigned)(key >> 32);
#endif
    hash *= (unsigned)(optCSEhashSize + 1);
    hash >>= 7;

    return hash % optCSEhashSize;
}

//---------------------------------------------------------------------------
// Index:
//               - Returns the CSE index to use for this tree,
//                 or zero if this expression is not currently a CSE.
//
// Arguments:
//    tree       - The current candidate CSE expression
//    stmt       - The current statement that contains tree
//
//
// Notes:   We build a hash table that contains all of the expressions that
//          are presented to this method.  Whenever we see a duplicate expression
//          we have a CSE candidate.  If it is the first time seeing the duplicate
//          we allocate a new CSE index. If we have already allocated a CSE index
//          we return that index.  There currently is a limit on the number of CSEs
//          that we can have of MAX_CSE_CNT (64)
//
unsigned Cse::Index(GenTree* tree, Statement* stmt)
{
    size_t   key;
    unsigned hval;
    CseDesc* hashDsc;
    bool     enableSharedConstCSE = false;
    bool     isSharedConst        = false;
    int      configValue          = JitConfig.JitConstCSE();

#if defined(TARGET_ARM64)
    // ARM64 - allow to combine with nearby offsets, when config is not 2 or 4
    if ((configValue != CONST_CSE_ENABLE_ARM64_NO_SHARING) && (configValue != CONST_CSE_ENABLE_ALL_NO_SHARING))
    {
        enableSharedConstCSE = true;
    }
#endif // TARGET_ARM64

    // All Platforms - also allow to combine with nearby offsets, when config is 3
    if (configValue == CONST_CSE_ENABLE_ALL)
    {
        enableSharedConstCSE = true;
    }

    // We use the liberal Value numbers when building the set of CSE
    ValueNum vnLib     = tree->GetVN(VNK_Liberal);
    ValueNum vnLibNorm = vnStore->VNNormalValue(vnLib);

    // We use the normal value number because we want the CSE candidate to
    // represent all expressions that produce the same normal value number.
    // We will handle the case where we have different exception sets when
    // promoting the candidates.
    //
    // We do this because a GT_IND will usually have a NullPtrExc entry in its
    // exc set, but we may have cleared the GTF_EXCEPT flag and if so, it won't
    // have an NullPtrExc, or we may have assigned the value of an GT_IND
    // into a LCL_VAR and then read it back later.
    //
    // When we are promoting the CSE candidates we ensure that any CSE
    // uses that we promote have an exc set that is the same as the CSE defs
    // or have an empty set.  And that all of the CSE defs produced the required
    // set of exceptions for the CSE uses.
    //

    // We assign either vnLib or vnLibNorm as the hash key
    //
    // The only exception to using the normal value is for the GT_COMMA nodes.
    // Here we check to see if we have a GT_COMMA with a different value number
    // than the one from its op2.  For this case we want to create two different
    // CSE candidates. This allows us to CSE the GT_COMMA separately from its value.
    //
    if (tree->OperGet() == GT_COMMA)
    {
        // op2 is the value produced by a GT_COMMA
        GenTree* op2      = tree->AsOp()->gtOp2;
        ValueNum vnOp2Lib = op2->GetVN(VNK_Liberal);

        // If the value number for op2 and tree are different, then some new
        // exceptions were produced by op1. For that case we will NOT use the
        // normal value. This allows us to CSE commas with an op1 that is
        // an ARR_BOUNDS_CHECK.
        //
        if (vnOp2Lib != vnLib)
        {
            key = vnLib; // include the exc set in the hash key
        }
        else
        {
            key = vnLibNorm;
        }

        // If we didn't do the above we would have op1 as the CSE def
        // and the parent comma as the CSE use (but with a different exc set)
        // This would prevent us from making any CSE with the comma
        //
        assert(vnLibNorm == vnStore->VNNormalValue(vnOp2Lib));
    }
    else if (enableSharedConstCSE && tree->IsIntegralConst())
    {
        assert(vnStore->IsVNConstant(vnLibNorm));

        // We don't share small offset constants when they require a reloc
        //
        if (tree->IsLngCon() || !tree->AsIntCon()->ImmedValNeedsReloc(compiler))
        {
            // Here we make constants that have the same upper bits use the same key
            //
            // We create a key that encodes just the upper bits of the constant by
            // shifting out some of the low bits, (12 or 16 bits)
            //
            // This is the only case where the hash key is not a ValueNumber
            //
            size_t constVal = vnStore->CoercedConstantValue<size_t>(vnLibNorm);
            key             = Encode_Shared_Const_CSE_Value(constVal);
            isSharedConst   = true;
        }
        else
        {
            // Use the vnLibNorm value as the key
            key = vnLibNorm;
        }
    }
    else // Not a GT_COMMA or a GT_CNS_INT
    {
        key = vnLibNorm;
    }

    // Make sure that the result of Is_Shared_Const_CSE(key) matches isSharedConst.
    // Note that when isSharedConst is true then we require that the TARGET_SIGN_BIT is set in the key
    // and otherwise we require that we never create a ValueNumber with the TARGET_SIGN_BIT set.
    //
    assert(isSharedConst == Is_Shared_Const_CSE(key));

    // Compute the hash value for the expression

    hval = optCSEKeyToHashIndex(key, hashSize);

    /* Look for a matching index in the hash table */

    bool newCSE = false;

    for (hashDsc = hashBuckets[hval]; hashDsc; hashDsc = hashDsc->nextInBucket)
    {
        if (hashDsc->hashKey == key)
        {
            // Check for mismatched types on GT_CNS_INT nodes
            if (tree->OperIs(GT_CNS_INT) && (tree->GetType() != hashDsc->tree->GetType()))
            {
                continue;
            }

            if (hashDsc->treeList == nullptr)
            {
                // Start the occurrence list now that we found a second occurrence.

                CseOccurence* occurrence =
                    new (compiler, CMK_CSE) CseOccurence(hashDsc->tree, hashDsc->stmt, hashDsc->block);

                hashDsc->treeList      = occurrence;
                hashDsc->treeLast      = occurrence;
                hashDsc->isSharedConst = isSharedConst;
            }

            if (varTypeIsSIMD(tree->GetType()) && (hashDsc->layout == nullptr))
            {
                // If we haven't yet obtained the SIMD layout try again, maybe we get lucky.
                // Mostly for the sake of consistency. Otherwise it doesn't really matter.
                // If we decide to CSE this expression we'll try to get an approximate layout.
                // The SIMD base type and the kind of vector that's associated with this SIMD
                // expression is ultimately irrelevant, we just need a layout that has the
                // same SIMD type as the expression. It also doesn't matter if 2 equivalent
                // expression somehow have different layouts, as long as the layout SIMD type
                // is the same.

                hashDsc->layout = compiler->typGetStructLayout(tree);
            }

            CseOccurence* occurrence = new (compiler, CMK_CSE) CseOccurence(tree, stmt, compiler->compCurBB);

            hashDsc->treeLast->next = occurrence;
            hashDsc->treeLast       = occurrence;

            doCSE = true; // Found a duplicate CSE tree

            if (hashDsc->index == 0)
            {
                newCSE = true;
                break;
            }

            tree->gtCSEnum = ToCseIndex(hashDsc->index);

            return hashDsc->index;
        }
    }

    if (!newCSE)
    {
        // Not found, create a new entry (unless we have too many already)

        if (compiler->cseCandidateCount < MAX_CSE_CNT)
        {
            if (hashCount == hashMaxCountBeforeResize)
            {
                size_t    newOptCSEhashSize = hashSize * s_optCSEhashGrowthFactor;
                CseDesc** newOptCSEhash     = new (compiler, CMK_CSE) CseDesc*[newOptCSEhashSize]();

                // Iterate through each existing entry, moving to the new table
                CseDesc** ptr;
                CseDesc*  dsc;
                size_t    cnt;
                for (cnt = hashSize, ptr = hashBuckets; cnt; cnt--, ptr++)
                {
                    for (dsc = *ptr; dsc;)
                    {
                        CseDesc* nextDsc = dsc->nextInBucket;

                        size_t newHval = optCSEKeyToHashIndex(dsc->hashKey, newOptCSEhashSize);

                        // Move CseDesc to bucket in enlarged table
                        dsc->nextInBucket      = newOptCSEhash[newHval];
                        newOptCSEhash[newHval] = dsc;

                        dsc = nextDsc;
                    }
                }

                hashBuckets              = newOptCSEhash;
                hashSize                 = newOptCSEhashSize;
                hashMaxCountBeforeResize = hashMaxCountBeforeResize * s_optCSEhashGrowthFactor;
            }

            ++hashCount;

            hashDsc = new (compiler, CMK_CSE) CseDesc(key, tree, stmt, compiler->compCurBB);

            if (varTypeIsStruct(tree->GetType()))
            {
                hashDsc->layout = compiler->typGetStructLayout(tree);
                assert((hashDsc->layout != nullptr) || varTypeIsSIMD(tree->GetType()));
            }

            // Append the entry to the hash bucket
            hashDsc->nextInBucket = hashBuckets[hval];
            hashBuckets[hval]     = hashDsc;
        }
        return 0;
    }
    else // newCSE is true
    {
        /* We get here only after finding a matching CSE */

        /* Create a new CSE (unless we have the maximum already) */

        if (compiler->cseCandidateCount == MAX_CSE_CNT)
        {
#ifdef DEBUG
            if (compiler->verbose)
            {
                printf("Exceeded the MAX_CSE_CNT, not using tree:\n");
                compiler->gtDispTree(tree);
            }
#endif // DEBUG
            return 0;
        }

        C_ASSERT((signed char)MAX_CSE_CNT == MAX_CSE_CNT);

        unsigned CSEindex = ++compiler->cseCandidateCount;

        /* Record the new CSE index in the hashDsc */
        hashDsc->index = CSEindex;

        /* Update the gtCSEnum field in the original tree */
        noway_assert(hashDsc->treeList->tree->gtCSEnum == NoCse);

        hashDsc->treeList->tree->gtCSEnum = ToCseIndex(CSEindex);

        tree->gtCSEnum = static_cast<CseIndex>(CSEindex);

#ifdef DEBUG
        if (compiler->verbose)
        {
            printf("\nCSE candidate #%02u, key=", CSEindex);
            if (!Is_Shared_Const_CSE(key))
            {
                compiler->vnPrint((unsigned)key, 0);
            }
            else
            {
                size_t kVal = Decode_Shared_Const_CSE_Value(key);
                printf("K_%p", dspPtr(kVal));
            }

            printf(" in " FMT_BB ", [cost=%2u, size=%2u]: \n", compiler->compCurBB->bbNum, tree->GetCostEx(),
                   tree->GetCostSz());
            compiler->gtDispTree(tree);
        }
#endif // DEBUG

        return CSEindex;
    }
}

//------------------------------------------------------------------------
// Locate: Locate CSE candidates and assign them indices.
//
// Returns:
//    true if there are any CSE candidates, false otherwise
//
bool Cse::Locate()
{
    bool enableConstCSE = true;

    int configValue = JitConfig.JitConstCSE();

    // all platforms - disable CSE of constant values when config is 1
    if (configValue == CONST_CSE_DISABLE_ALL)
    {
        enableConstCSE = false;
    }

#if !defined(TARGET_ARM64)
    // non-ARM64 platforms - disable by default
    //
    enableConstCSE = false;

    // Check for the two enable cases for all platforms
    //
    if ((configValue == CONST_CSE_ENABLE_ALL) || (configValue == CONST_CSE_ENABLE_ALL_NO_SHARING))
    {
        enableConstCSE = true;
    }
#endif

    for (BasicBlock* const block : compiler->Blocks())
    {
        /* Make the block publicly available */

        compiler->compCurBB = block;

        /* Ensure that the BBF_MARKED flag is clear */
        /* Everyone who uses this flag are required to clear afterwards */
        noway_assert((block->bbFlags & BBF_MARKED) == 0);

        /* Walk the statement trees in this basic block */
        for (Statement* const stmt : block->NonPhiStatements())
        {
            const bool isReturn = stmt->GetRootNode()->OperIs(GT_RETURN);

            /* We walk the tree in the forwards direction (bottom up) */
            bool stmtHasArrLenCandidate = false;
            for (GenTree* const tree : stmt->TreeList())
            {
                if (tree->OperIsCompare() && stmtHasArrLenCandidate)
                {
                    // Check if this compare is a function of (one of) the checked
                    // bound candidate(s); we may want to update its value number.
                    // if the array length gets CSEd
                    UpdateCheckedBoundMap(tree);
                }

                // Don't allow CSE of constants if it is disabled
                //
                if (tree->IsIntegralConst())
                {
                    if (!enableConstCSE)
                    {
                        continue;
                    }
                }

                // Don't allow non-SIMD struct CSEs under a return; we don't fully
                // re-morph these if we introduce a CSE assignment, and so may create
                // IR that lower is not yet prepared to handle.
                //
                if (isReturn && varTypeIsStruct(tree->gtType) && !varTypeIsSIMD(tree->gtType))
                {
                    continue;
                }

                if (!compiler->cseIsCandidate(tree))
                {
                    continue;
                }

                if (ValueNumStore::isReservedVN(tree->GetVN(VNK_Liberal)))
                {
                    continue;
                }

                // We want to CSE simple constant leaf nodes, but we don't want to
                // CSE non-leaf trees that compute CSE constant values.
                // Instead we let the Value Number based Assertion Prop phase handle them.
                //
                // Here, unlike the rest of optCSE, we use the conservative value number
                // rather than the liberal one, since the conservative one
                // is what the Value Number based Assertion Prop will use
                // and the point is to avoid optimizing cases that it will
                // handle.
                //
                if (!tree->OperIsLeaf() && vnStore->IsVNConstant(vnStore->VNConservativeNormalValue(tree->gtVNPair)))
                {
                    continue;
                }

                /* Assign an index to this expression */

                unsigned CSEindex = Index(tree, stmt);

                if (CSEindex != 0)
                {
                    noway_assert(((unsigned)tree->gtCSEnum) == CSEindex);

                    if (tree->OperIs(GT_ARR_LENGTH))
                    {
                        stmtHasArrLenCandidate = true;
                    }
                }
            }
        }
    }

    /* We're done if there were no interesting expressions */

    if (!doCSE)
    {
        return false;
    }

    BuildCseTable();

    return true;
}

//------------------------------------------------------------------------
// UpdateCheckedBoundMap: Check if this compare is a tractable function of
//                     a checked bound that is a CSE candidate, and insert
//                     an entry in the optCseCheckedBoundMap if so.  This facilitates
//                     subsequently updating the compare's value number if
//                     the bound gets CSEd.
//
// Arguments:
//    compare - The compare node to check
//
void Cse::UpdateCheckedBoundMap(GenTree* compare)
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

/*****************************************************************************
 *
 *  Compute each blocks bbCseGen
 *  This is the bitset that represents the CSEs that are generated within the block
 *  Also initialize bbCseIn, bbCseOut and bbCseGen sets for all blocks
 */
void Cse::InitDataFlow()
{
    // BitVec trait information for computing CSE availability using the CseDataFlow algorithm.
    // Two bits are allocated per CSE candidate to compute CSE availability
    // plus an extra bit to handle the initial unvisited case.
    // (See CseDataFlow::EndMerge for an explaination of why this is necessary)
    //
    // The two bits per CSE candidate have the following meanings:
    //     11 - The CSE is available, and is also available when considering calls as killing availability.
    //     10 - The CSE is available, but is not available when considering calls as killing availability.
    //     00 - The CSE is not available
    //     01 - An illegal combination
    //
    const unsigned bitCount = (compiler->cseCandidateCount * 2) + 1;

    // Init traits and cseCallKillsMask bitvectors.
    dataFlowTraits = BitVecTraits(bitCount, compiler);
    callKillsMask  = BitVecOps::MakeEmpty(&dataFlowTraits);
    for (unsigned inx = 1; inx <= compiler->cseCandidateCount; inx++)
    {
        unsigned cseAvailBit = getCSEAvailBit(inx);

        // a one preserves availability and a zero kills the availability
        // we generate this kind of bit pattern:  101010101010
        //
        BitVecOps::AddElemD(&dataFlowTraits, callKillsMask, cseAvailBit);
    }

    for (BasicBlock* const block : compiler->Blocks())
    {
        /* Initialize the blocks's bbCseIn set */

        bool init_to_zero = false;

        if (block == compiler->fgFirstBB)
        {
            /* Clear bbCseIn for the entry block */
            init_to_zero = true;
        }
#if !CSE_INTO_HANDLERS
        else
        {
            if (compiler->bbIsHandlerBeg(block))
            {
                /* Clear everything on entry to filters or handlers */
                init_to_zero = true;
            }
        }
#endif
        if (init_to_zero)
        {
            /* Initialize to {ZERO} prior to dataflow */
            block->bbCseIn = BitVecOps::MakeEmpty(&dataFlowTraits);
        }
        else
        {
            /* Initialize to {ALL} prior to dataflow */
            block->bbCseIn = BitVecOps::MakeFull(&dataFlowTraits);
        }

        block->bbCseOut = BitVecOps::MakeFull(&dataFlowTraits);

        /* Initialize to {ZERO} prior to locating the CSE candidates */
        block->bbCseGen = BitVecOps::MakeEmpty(&dataFlowTraits);
    }

    // We walk the set of CSE candidates and set the bit corresponding to the CSEindex
    // in the block's bbCseGen bitset
    //
    for (unsigned inx = 0; inx < compiler->cseCandidateCount; inx++)
    {
        CseDesc*      dsc      = compiler->cseTable[inx];
        unsigned      CSEindex = dsc->index;
        CseOccurence* lst      = dsc->treeList;
        noway_assert(lst);

        while (lst != nullptr)
        {
            BasicBlock* block                = lst->block;
            unsigned    cseAvailBit          = getCSEAvailBit(CSEindex);
            unsigned    cseAvailCrossCallBit = getCSEAvailCrossCallBit(CSEindex);

            // This CSE is generated in 'block', we always set the cseAvailBit
            // If this block does not contain a call, we also set cseAvailCrossCallBit
            //
            // If we have a call in this block then in the loop below we walk the trees
            // backwards to find any CSEs that are generated after the last call in the block.
            //
            BitVecOps::AddElemD(&dataFlowTraits, block->bbCseGen, cseAvailBit);
            if ((block->bbFlags & BBF_HAS_CALL) == 0)
            {
                BitVecOps::AddElemD(&dataFlowTraits, block->bbCseGen, cseAvailCrossCallBit);
            }
            lst = lst->next;
        }
    }

    for (BasicBlock* const block : compiler->Blocks())
    {
        // If the block doesn't contains a call then skip it...
        //
        if ((block->bbFlags & BBF_HAS_CALL) == 0)
        {
            continue;
        }

        // We only need to examine blocks that generate CSEs
        //
        if (BitVecOps::IsEmpty(&dataFlowTraits, block->bbCseGen))
        {
            continue;
        }

        // If the block contains a call and generates CSEs, we may need to update
        // the bbCseGen set as we may generate some CSEs after the last call in the block.
        //
        // We walk the statements in this basic block starting at the end and walking backwards,
        // until we reach the first call
        //
        Statement* stmt      = block->lastStmt();
        bool       foundCall = false;
        while (!foundCall)
        {
            // Also walk the tree in the backwards direction (bottom up)
            // looking for CSE's and updating block->bbCseGen
            // When we reach a call node, we can exit the for loop
            //
            for (GenTree* tree = stmt->GetRootNode(); tree != nullptr; tree = tree->gtPrev)
            {
                if (IsCseIndex(tree->gtCSEnum))
                {
                    unsigned CSEnum               = GetCseIndex(tree->gtCSEnum);
                    unsigned cseAvailCrossCallBit = getCSEAvailCrossCallBit(CSEnum);
                    BitVecOps::AddElemD(&dataFlowTraits, block->bbCseGen, cseAvailCrossCallBit);
                }
                if (tree->OperGet() == GT_CALL)
                {
                    // Any cse's that we haven't placed in the block->bbCseGen set
                    // aren't currently alive (using cseAvailCrossCallBit)
                    //
                    foundCall = true;
                    break;
                }
            }
            // The JIT can sometimes remove the only call in the block
            if (stmt == block->firstStmt())
            {
                break;
            }
            stmt = stmt->GetPrevStmt();
        }
    }

#ifdef DEBUG
    // Dump out the bbCseGen information that we just created
    //
    if (compiler->verbose)
    {
        bool headerPrinted = false;
        for (BasicBlock* const block : compiler->Blocks())
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

/*****************************************************************************
 *
 * CSE Dataflow, so that all helper methods for dataflow are in a single place
 *
 */
class CseDataFlow
{
    Compiler* m_comp;
    Cse&      cse;
    EXPSET_TP m_preMergeOut;

public:
    CseDataFlow(Compiler* pCompiler, Cse& cse) : m_comp(pCompiler), cse(cse), m_preMergeOut(BitVecOps::UninitVal())
    {
    }

    // At the start of the merge function of the dataflow equations, initialize premerge state (to detect changes.)
    void StartMerge(BasicBlock* block)
    {
        // Record the initial value of block->bbCseOut in m_preMergeOut.
        // It is used in EndMerge() to control the termination of the DataFlow algorithm.
        // Note that the first time we visit a block, the value of bbCseOut is MakeFull()
        //
        BitVecOps::Assign(&cse.dataFlowTraits, m_preMergeOut, block->bbCseOut);

#if 0
#ifdef DEBUG
        if (m_comp->verbose)
        {
            printf("StartMerge " FMT_BB "\n", block->bbNum);
            printf("  :: cseOut    = %s\n", genES2str(&cse.dataFlowTraits, block->bbCseOut));
        }
#endif // DEBUG
#endif // 0
    }

    // Merge: perform the merging of each of the predecessor's liveness values (since this is a forward analysis)
    void Merge(BasicBlock* block, BasicBlock* predBlock, unsigned dupCount)
    {
#if 0
#ifdef DEBUG
        if (m_comp->verbose)
        {
            printf("Merge " FMT_BB " and " FMT_BB "\n", block->bbNum, predBlock->bbNum);
            printf("  :: cseIn     = %s\n", genES2str(&cse.dataFlowTraits, block->bbCseIn));
            printf("  :: cseOut    = %s\n", genES2str(&cse.dataFlowTraits, block->bbCseOut));
        }
#endif // DEBUG
#endif // 0

        BitVecOps::IntersectionD(&cse.dataFlowTraits, block->bbCseIn, predBlock->bbCseOut);

#if 0
#ifdef DEBUG
        if (m_comp->verbose)
        {
            printf("  => cseIn     = %s\n", genES2str(&cse.dataFlowTraits, block->bbCseIn));
        }
#endif // DEBUG
#endif // 0
    }

    //------------------------------------------------------------------------
    // MergeHandler: Merge CSE values into the first exception handler/filter block.
    //
    // Arguments:
    //   block         - the block that is the start of a handler or filter;
    //   firstTryBlock - the first block of the try for "block" handler;
    //   lastTryBlock  - the last block of the try for "block" handler;.
    //
    // Notes:
    //   We can jump to the handler from any instruction in the try region.
    //   It means we can propagate only CSE that are valid for the whole try region.
    void MergeHandler(BasicBlock* block, BasicBlock* firstTryBlock, BasicBlock* lastTryBlock)
    {
        // TODO CQ: add CSE for handler blocks, CSE_INTO_HANDLERS should be defined.
    }

    // At the end of the merge store results of the dataflow equations, in a postmerge state.
    // We also handle the case where calls conditionally kill CSE availabilty.
    //
    bool EndMerge(BasicBlock* block)
    {
        // We can skip the calls kill step when our block doesn't have a callsite
        // or we don't have any available CSEs in our bbCseIn
        //
        if (((block->bbFlags & BBF_HAS_CALL) == 0) || BitVecOps::IsEmpty(&cse.dataFlowTraits, block->bbCseIn))
        {
            // No callsite in 'block' or 'block->bbCseIn was empty, so we can use bbCseIn directly
            //
            BitVecOps::DataFlowD(&cse.dataFlowTraits, block->bbCseOut, block->bbCseGen, block->bbCseIn);
        }
        else
        {
            // We will create a temporary BitVec to pass to DataFlowD()
            //
            EXPSET_TP cseIn_withCallsKill = BitVecOps::UninitVal();

            // cseIn_withCallsKill is set to (bbCseIn AND cseCallKillsMask)
            //
            BitVecOps::Assign(&cse.dataFlowTraits, cseIn_withCallsKill, block->bbCseIn);
            BitVecOps::IntersectionD(&cse.dataFlowTraits, cseIn_withCallsKill, cse.callKillsMask);

            // Call DataFlowD with the modified BitVec: (bbCseIn AND cseCallKillsMask)
            //
            BitVecOps::DataFlowD(&cse.dataFlowTraits, block->bbCseOut, block->bbCseGen, cseIn_withCallsKill);
        }

        // The bool 'notDone' is our terminating condition.
        // If it is 'true' then the initial value of m_preMergeOut was different than the final value that
        // we computed for bbCseOut.  When it is true we will visit every the successor of 'block'
        //
        // This is also why we need to allocate an extra bit in our cseLivenessTrair BitVecs.
        // We always need to visit our successor blocks once, thus we require that that the first time
        // that we visit a block we have a bit set in m_preMergeOut that won't be set when we compute
        // the new value of bbCseOut.
        //
        bool notDone = !BitVecOps::Equal(&cse.dataFlowTraits, block->bbCseOut, m_preMergeOut);

#if 0
#ifdef DEBUG
        if (m_comp->verbose)
        {
            printf("EndMerge " FMT_BB "\n", block->bbNum);
            printf("  :: cseIn     = %s\n", genES2str(&cse.dataFlowTraits, block->bbCseIn));
            if (((block->bbFlags & BBF_HAS_CALL) != 0) &&
                !BitVecOps::IsEmpty(&cse.dataFlowTraits, block->bbCseIn))
            {
                printf("  -- cseKill   = %s\n", genES2str(&cse.dataFlowTraits, m_comp->cseCallKillsMask));
            }
            printf("  :: cseGen    = %s\n", genES2str(&cse.dataFlowTraits, block->bbCseGen));
            printf("  => cseOut    = %s\n", genES2str(&cse.dataFlowTraits, block->bbCseOut));
            printf("  != preMerge  = %s, => %s\n", genES2str(&cse.dataFlowTraits, m_preMergeOut),
                   notDone ? "true" : "false");
        }
#endif // DEBUG
#endif // 0

        return notDone;
    }
};

/*****************************************************************************
 *
 *  Perform a DataFlow forward analysis using the block CSE bitsets:
 *    Inputs:
 *      bbCseGen  - Exact CSEs that are always generated within the block
 *      bbCseIn   - Maximal estimate of CSEs that are/could be available at input to the block
 *      bbCseOut  - Maximal estimate of CSEs that are/could be available at exit to the block
 *
 *    Outputs:
 *      bbCseIn   - Computed CSEs that are available at input to the block
 *      bbCseOut  - Computed CSEs that are available at exit to the block
 */

void Cse::DataFlow()
{
#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("\nPerforming DataFlow for ValnumCSE's\n");
    }
#endif // DEBUG

    ForwardDataFlow(CseDataFlow(compiler, *this), compiler);

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

//---------------------------------------------------------------------------
// Availablity:
//
//     Using the information computed by CseDataFlow determine for each
//     CSE whether the CSE is a definition (if the CSE was not available)
//     or if the CSE is a use (if the CSE was previously made available).
//     The implementation iterates over all blocks setting 'available_cses'
//     to the CSEs that are available at input to the block.
//     When a CSE expression is encountered it is classified as either
//     as a definition (if the CSE is not in the 'available_cses' set) or
//     as a use (if the CSE is in the 'available_cses' set).  If the CSE
//     is a definition then it is added to the 'available_cses' set.
//
//     This algorithm uncovers the defs and uses gradually and as it does
//     so it also builds the exception set that all defs make: 'defExcSetCurrent'
//     and the exception set that the uses we have seen depend upon: 'defExcSetPromise'.
//
//     Typically expressions with the same normal ValueNum generate exactly the
//     same exception sets. There are two way that we can get different exception
//     sets with the same Normal value number.
//
//     1. We used an arithmetic identiity:
//        e.g. (p.a + q.b) * 0   :: The normal value for the expression is zero
//                                  and we have NullPtrExc(p) and NullPtrExc(q)
//        e.g. (p.a - p.a)       :: The normal value for the expression is zero
//                                  and we have NullPtrExc(p)
//     2. We stored an expression into a LclVar or into Memory and read it later
//        e.g. t = p.a;
//             e1 = (t + q.b)    :: e1 has one NullPtrExc and e2 has two.
//             e2 = (p.a + q.b)     but both compute the same normal value
//        e.g. m.a = p.a;
//             e1 = (m.a + q.b)  :: e1 and e2 have different exception sets.
//             e2 = (p.a + q.b)     but both compute the same normal value
//
void Cse::Availablity()
{
#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("Labeling the CSEs with Use/Def information\n");
    }
#endif
    EXPSET_TP available_cses = BitVecOps::MakeEmpty(&dataFlowTraits);

    for (BasicBlock* const block : compiler->Blocks())
    {
        // Make the block publicly available

        compiler->compCurBB = block;

        // Retrieve the available CSE's at the start of this block

        BitVecOps::Assign(&dataFlowTraits, available_cses, block->bbCseIn);

        // Walk the statement trees in this basic block

        for (Statement* const stmt : block->NonPhiStatements())
        {
            // We walk the tree in the forwards direction (bottom up)

            for (GenTree* const tree : stmt->TreeList())
            {
                bool isUse = false;
                bool isDef = false;

                if (IsCseIndex(tree->gtCSEnum))
                {
                    unsigned             CSEnum               = GetCseIndex(tree->gtCSEnum);
                    unsigned             cseAvailBit          = getCSEAvailBit(CSEnum);
                    unsigned             cseAvailCrossCallBit = getCSEAvailCrossCallBit(CSEnum);
                    CseDesc*             desc                 = compiler->cseGetDesc(CSEnum);
                    BasicBlock::weight_t stmw                 = block->getBBWeight(compiler);

                    isUse = BitVecOps::IsMember(&dataFlowTraits, available_cses, cseAvailBit);
                    isDef = !isUse; // If is isn't a CSE use, it is a CSE def

                    // Is this a "use", that we haven't yet marked as live across a call
                    // and it is not available when we have calls that kill CSE's (cseAvailCrossCallBit)
                    // if the above is true then we will mark this the CSE as live across a call
                    //
                    bool madeLiveAcrossCall = false;
                    if (isUse && !desc->isLiveAcrossCall &&
                        !BitVecOps::IsMember(&dataFlowTraits, available_cses, cseAvailCrossCallBit))
                    {
                        desc->isLiveAcrossCall = true;
                        madeLiveAcrossCall     = true;
                    }

#ifdef DEBUG
                    // If this is a CSE def (i.e. the CSE is not available here, since it is being defined), then the
                    // call-kill bit
                    // should also be zero since it is also not available across a call.
                    //
                    if (isDef)
                    {
                        assert(!BitVecOps::IsMember(&dataFlowTraits, available_cses, cseAvailCrossCallBit));
                    }

                    if (compiler->verbose)
                    {
                        printf(FMT_BB " ", block->bbNum);
                        Compiler::printTreeID(tree);

                        printf(" %s of " FMT_CSE " [weight=%s]%s\n", isUse ? "Use" : "Def", CSEnum, refCntWtd2str(stmw),
                               madeLiveAcrossCall ? " *** Now Live Across Call ***" : "");
                    }
#endif // DEBUG

                    // Have we decided to abandon work on this CSE?
                    if (desc->defExcSetPromise == ValueNumStore::NoVN)
                    {
                        // This candidate had defs with differing liberal exc set VNs
                        // We have abandoned CSE promotion for this candidate

                        // Clear the CSE flag
                        tree->gtCSEnum = NoCse;

                        JITDUMP(" Abandoned - CSE candidate has defs with different exception sets!\n");
                        continue;
                    }

                    // Record the exception set for tree's liberal value number
                    //
                    ValueNum theLiberalExcSet = vnStore->VNExceptionSet(tree->gtVNPair.GetLiberal());

                    // Is this a CSE use or a def?

                    if (isDef)
                    {
                        // This is a CSE def

                        // Is defExcSetCurrent still set to the uninit marker value of VNForNull() ?
                        if (desc->defExcSetCurrent == vnStore->VNForNull())
                        {
                            // This is the first time visited, so record this defs exception set
                            desc->defExcSetCurrent = theLiberalExcSet;
                        }

                        // Have we seen a CSE use and made a promise of an exception set?
                        //
                        if (desc->defExcSetPromise != vnStore->VNForEmptyExcSet())
                        {
                            // The exeception set held in desc->defExcSetPromise must be a subset of theLiberalExcSet
                            //
                            if (vnStore->VNExcIsSubset(theLiberalExcSet, desc->defExcSetPromise))
                            {
                                // This new def still satisfies any promise made to all the CSE uses that we have
                                // encountered
                                //

                                // no update is needed when these are the same VN
                                if (desc->defExcSetCurrent != theLiberalExcSet)
                                {
                                    // We will change the value of desc->defExcSetCurrent to be the intersection of
                                    // these two sets.
                                    // This is the set of exceptions that all CSE defs have (that we have visited so
                                    // far)
                                    //
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
                                    //
                                    assert(vnStore->VNExcIsSubset(desc->defExcSetCurrent, intersectionExcSet));
                                    desc->defExcSetCurrent = intersectionExcSet;
                                }
                            }
                            else // This CSE def doesn't satisfy one of the exceptions already promised to a CSE use
                            {
                                // So, we will abandon all CSE promotions for this candidate
                                //
                                // We use the marker value of NoVN to indicate that we
                                // should abandon this CSE candidate
                                //
                                desc->defExcSetPromise = ValueNumStore::NoVN;
                                tree->gtCSEnum         = NoCse;

                                JITDUMP(" Abandon - CSE candidate has defs with exception sets that do not satisfy "
                                        "some CSE use\n");
                                continue;
                            }
                        }

                        // For shared const CSE we don't set/use the defConservNormVN
                        //
                        if (!Is_Shared_Const_CSE(desc->hashKey))
                        {
                            // Record or update the value of desc->defConservNormVN
                            //
                            ValueNum theConservNormVN = vnStore->VNConservativeNormalValue(tree->gtVNPair);

                            // Is defConservNormVN still set to the uninit marker value of VNForNull() ?
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
                                    // This candidate has defs with differing conservative normal VNs, mark it with NoVN
                                    desc->defConservNormVN = ValueNumStore::NoVN; // record the marker for differing VNs
                                }
                            }
                        }

                        // If we get here we have accepted this node as a valid CSE def

                        desc->defCount += 1;
                        desc->defWeight += stmw;

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
                        //
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
                                // sets
                                //
                                desc->defExcSetPromise =
                                    vnStore->VNExcSetUnion(desc->defExcSetPromise, theLiberalExcSet);
                            }
                            else // we have already seen a def for this CSE and defExcSetCurrent is setup
                            {
                                if (vnStore->VNExcIsSubset(desc->defExcSetCurrent, theLiberalExcSet))
                                {
                                    // The current set of exceptions produced by all CSE defs have (that we have
                                    // visited so far) meets our requirement
                                    //
                                    // Add any exception items to the defExcSetPromise set
                                    //
                                    desc->defExcSetPromise =
                                        vnStore->VNExcSetUnion(desc->defExcSetPromise, theLiberalExcSet);
                                }
                            }

                            // At this point defExcSetPromise contains all of the exception items that we can promise
                            // here.
                            //
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
                        desc->useWeight += stmw;
                    }
                }

                // In order to determine if a CSE is live across a call, we model availablity using two bits and
                // kill all of the cseAvailCrossCallBit for each CSE whenever we see a GT_CALL (unless the call
                // generates a CSE).
                //
                if (tree->OperGet() == GT_CALL)
                {
                    // Check for the common case of an already empty available_cses set
                    // and thus nothing needs to be killed
                    //
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
                            //
                            BitVecOps::IntersectionD(&dataFlowTraits, available_cses, callKillsMask);

                            if (isDef)
                            {
                                // We can have a GT_CALL that produces a CSE,
                                // (i.e. HELPER.CORINFO_HELP_GETSHARED_*STATIC_BASE or
                                // CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE)
                                //
                                // The CSE becomes available after the call, so set the cseAvailCrossCallBit bit in
                                // available_cses
                                //
                                unsigned CSEnum               = GetCseIndex(tree->gtCSEnum);
                                unsigned cseAvailCrossCallBit = getCSEAvailCrossCallBit(CSEnum);

                                BitVecOps::AddElemD(&dataFlowTraits, available_cses, cseAvailCrossCallBit);
                            }
                        }
                    }
                }
            }
        }
    }
}

//  The following class handles the CSE heuristics
//  we use a complex set of heuristic rules
//  to determine if it is likely to be profitable to perform this CSE
//
class CseHeuristic
{
    Compiler* m_pCompiler;
    Cse&      m_cse;
    unsigned  m_addCSEcount;

    BasicBlock::weight_t   aggressiveRefCnt;
    BasicBlock::weight_t   moderateRefCnt;
    unsigned               enregCount; // count of the number of predicted enregistered variables
    bool                   largeFrame;
    bool                   hugeFrame;
    Compiler::codeOptimize codeOptKind;
    CseDesc**              sortTab;
    size_t                 sortSiz;
#ifdef DEBUG
    CLRRandom m_cseRNG;
    unsigned  m_bias;
#endif

public:
    CseHeuristic(Compiler* pCompiler, Cse& cse) : m_pCompiler(pCompiler), m_cse(cse)
    {
        codeOptKind = m_pCompiler->compCodeOpt();
    }

    Compiler::codeOptimize CodeOptKind()
    {
        return codeOptKind;
    }

    // Perform the Initialization step for our CSE Heuristics
    // determine the various cut off values to use for
    // the aggressive, moderate and conservative CSE promotions
    // count the number of enregisterable variables
    // determine if the method has a large or huge stack frame.
    //
    void Initialize()
    {
        m_addCSEcount = 0; /* Count of the number of LclVars for CSEs that we added */

        // Record the weighted ref count of the last "for sure" callee saved LclVar
        aggressiveRefCnt = 0;
        moderateRefCnt   = 0;
        enregCount       = 0;
        largeFrame       = false;
        hugeFrame        = false;
        sortTab          = nullptr;
        sortSiz          = 0;

        unsigned   frameSize        = 0;
        unsigned   regAvailEstimate = ((CNT_CALLEE_ENREG * 3) + (CNT_CALLEE_TRASH * 2) + 1);
        unsigned   lclNum;
        LclVarDsc* varDsc;

        for (lclNum = 0, varDsc = m_pCompiler->lvaTable; lclNum < m_pCompiler->lvaCount; lclNum++, varDsc++)
        {
            // Locals with no references don't use any local stack frame slots
            if (varDsc->lvRefCnt() == 0)
            {
                continue;
            }

            // Incoming stack arguments don't use any local stack frame slots
            if (varDsc->lvIsParam && !varDsc->lvIsRegArg)
            {
                continue;
            }

#if FEATURE_FIXED_OUT_ARGS
            // Skip the OutgoingArgArea in computing frame size, since
            // its size is not yet known and it doesn't affect local
            // offsets from the frame pointer (though it may affect
            // them from the stack pointer).
            noway_assert(m_pCompiler->lvaOutgoingArgSpaceVar != BAD_VAR_NUM);
            if (lclNum == m_pCompiler->lvaOutgoingArgSpaceVar)
            {
                continue;
            }
#endif // FEATURE_FIXED_OUT_ARGS

            bool onStack = (regAvailEstimate == 0); // true when it is likely that this LclVar will have a stack home

            // Some LclVars always have stack homes
            if ((varDsc->lvDoNotEnregister) || (varDsc->lvType == TYP_BLK))
            {
                onStack = true;
            }

#ifdef TARGET_X86
            // Treat floating point and 64 bit integers as always on the stack
            if (varTypeIsFloating(varDsc->TypeGet()) || varTypeIsLong(varDsc->TypeGet()))
            {
                onStack = true;
            }
#endif

            if (onStack)
            {
                frameSize += m_pCompiler->lvaLclSize(lclNum);
            }
            else
            {
                // For the purposes of estimating the frameSize we
                // will consider this LclVar as being enregistered.
                // Now we reduce the remaining regAvailEstimate by
                // an appropriate amount.
                //
                if (varDsc->lvRefCnt() <= 2)
                {
                    // a single use single def LclVar only uses 1
                    regAvailEstimate -= 1;
                }
                else
                {
                    // a LclVar with multiple uses and defs uses 2
                    if (regAvailEstimate >= 2)
                    {
                        regAvailEstimate -= 2;
                    }
                    else
                    {
                        // Don't try to subtract when regAvailEstimate is 1
                        regAvailEstimate = 0;
                    }
                }
            }
#ifdef TARGET_XARCH
            if (frameSize > 0x080)
            {
                // We likely have a large stack frame.
                //
                // On XARCH stack frame displacements can either use a 1-byte or a 4-byte displacement
                // with a large franme we will need to use some 4-byte displacements.
                //
                largeFrame = true;
                break; // early out,  we don't need to keep increasing frameSize
            }
#elif defined(TARGET_ARM)
            if (frameSize > 0x0400)
            {
                // We likely have a large stack frame.
                //
                // Thus we might need to use large displacements when loading or storing
                // to CSE LclVars that are not enregistered
                // On ARM32 this means using rsGetRsvdReg() to hold the large displacement
                largeFrame = true;
            }
            if (frameSize > 0x10000)
            {
                hugeFrame = true;
                break; // early out,  we don't need to keep increasing frameSize
            }
#elif defined(TARGET_ARM64)
            if (frameSize > 0x1000)
            {
                // We likely have a large stack frame.
                //
                // Thus we might need to use large displacements when loading or storing
                // to CSE LclVars that are not enregistered
                // On ARM64 this means using rsGetRsvdReg() to hold the large displacement
                //
                largeFrame = true;
                break; // early out,  we don't need to keep increasing frameSize
            }
#endif
        }

        // Iterate over the sorted list of tracked local variables
        // these are the register candidates for LSRA
        // We normally vist the LclVar in order of their weighted ref counts
        // and our hueristic assumes that the highest weighted ref count
        // LclVars will be enregistered and that the lowest weighted ref count
        // are likely be allocated in the stack frame.
        // The value of enregCount is incremented when we visit a LclVar
        // that can be enregistered.
        //
        for (unsigned trackedIndex = 0; trackedIndex < m_pCompiler->lvaTrackedCount; trackedIndex++)
        {
            LclVarDsc* varDsc = m_pCompiler->lvaGetDescByTrackedIndex(trackedIndex);
            var_types  varTyp = varDsc->TypeGet();

            // Locals with no references aren't enregistered
            if (varDsc->lvRefCnt() == 0)
            {
                continue;
            }

            // Some LclVars always have stack homes
            if ((varDsc->lvDoNotEnregister) || (varDsc->lvType == TYP_BLK))
            {
                continue;
            }

            // The enregCount only tracks the uses of integer registers
            //
            // We could track floating point register usage seperately
            // but it isn't worth the additional complexity as floating point CSEs
            // are rare and we typically have plenty of floating point register available.
            //
            if (!varTypeIsFloating(varTyp))
            {
                enregCount++; // The primitive types, including TYP_SIMD types use one register

#ifndef TARGET_64BIT
                if (varTyp == TYP_LONG)
                {
                    enregCount++; // on 32-bit targets longs use two registers
                }
#endif
            }

            // Set the cut off values to use for deciding when we want to use aggressive, moderate or conservative
            //
            // The value of aggressiveRefCnt and moderateRefCnt start off as zero and
            // when enregCount reached a certain value we assign the current LclVar
            // (weighted) ref count to aggressiveRefCnt or moderateRefCnt.
            //
            const unsigned aggressiveEnregNum = (CNT_CALLEE_ENREG * 3 / 2);
            const unsigned moderateEnregNum   = ((CNT_CALLEE_ENREG * 3) + (CNT_CALLEE_TRASH * 2));
            //
            // On Windows x64 this yeilds:
            // aggressiveEnregNum == 12 and moderateEnregNum == 38
            // Thus we will typically set the cutoff values for
            //   aggressiveRefCnt based upon the weight of T13 (the 13th tracked LclVar)
            //   moderateRefCnt based upon the weight of T39 (the 39th tracked LclVar)
            //
            // For other architecture and platforms these values dynamically change
            // based upon the number of callee saved and callee scratch registers.
            //
            if ((aggressiveRefCnt == 0) && (enregCount > aggressiveEnregNum))
            {
                if (CodeOptKind() == Compiler::SMALL_CODE)
                {
                    aggressiveRefCnt = static_cast<BasicBlock::weight_t>(varDsc->GetRefCount());
                }
                else
                {
                    aggressiveRefCnt = varDsc->lvRefCntWtd();
                }
                aggressiveRefCnt += BB_UNITY_WEIGHT;
            }
            if ((moderateRefCnt == 0) && (enregCount > ((CNT_CALLEE_ENREG * 3) + (CNT_CALLEE_TRASH * 2))))
            {
                if (CodeOptKind() == Compiler::SMALL_CODE)
                {
                    moderateRefCnt = static_cast<BasicBlock::weight_t>(varDsc->GetRefCount());
                }
                else
                {
                    moderateRefCnt = varDsc->lvRefCntWtd();
                }
                moderateRefCnt += (BB_UNITY_WEIGHT / 2);
            }
        }

        // The minumum value that we want to use for aggressiveRefCnt is BB_UNITY_WEIGHT * 2
        // so increase it when we are below that value
        //
        aggressiveRefCnt = max(BB_UNITY_WEIGHT * 2, aggressiveRefCnt);

        // The minumum value that we want to use for moderateRefCnt is BB_UNITY_WEIGHT
        // so increase it when we are below that value
        //
        moderateRefCnt = max(BB_UNITY_WEIGHT, moderateRefCnt);

#ifdef DEBUG
        if (m_pCompiler->verbose)
        {
            printf("\n");
            printf("Aggressive CSE Promotion cutoff is %f\n", aggressiveRefCnt);
            printf("Moderate CSE Promotion cutoff is %f\n", moderateRefCnt);
            printf("enregCount is %u\n", enregCount);
            printf("Framesize estimate is 0x%04X\n", frameSize);
            printf("We have a %s frame\n", hugeFrame ? "huge" : (largeFrame ? "large" : "small"));
        }
#endif
    }

    void SortCandidates()
    {
        /* Create an expression table sorted by decreasing cost */
        sortTab = new (m_pCompiler, CMK_CSE) CseDesc*[m_pCompiler->cseCandidateCount];

        sortSiz = m_pCompiler->cseCandidateCount * sizeof(*sortTab);
        memcpy(sortTab, m_pCompiler->cseTable, sortSiz);

        if (CodeOptKind() == Compiler::SMALL_CODE)
        {
            jitstd::sort(sortTab, sortTab + m_pCompiler->cseCandidateCount, CseCostCompareSize());
        }
        else
        {
            jitstd::sort(sortTab, sortTab + m_pCompiler->cseCandidateCount, CseCostCompareSpeed());
        }

#ifdef DEBUG
        if (m_pCompiler->verbose)
        {
            printf("\nSorted CSE candidates:\n");
            /* Print out the CSE candidates */
            for (unsigned cnt = 0; cnt < m_pCompiler->cseCandidateCount; cnt++)
            {
                CseDesc* dsc  = sortTab[cnt];
                GenTree* expr = dsc->tree;

                BasicBlock::weight_t def;
                BasicBlock::weight_t use;
                unsigned             cost;

                if (CodeOptKind() == Compiler::SMALL_CODE)
                {
                    def  = dsc->defCount; // def count
                    use  = dsc->useCount; // use count (excluding the implicit uses at defs)
                    cost = dsc->tree->GetCostSz();
                }
                else
                {
                    def  = dsc->defWeight; // weighted def count
                    use  = dsc->useWeight; // weighted use count (excluding the implicit uses at defs)
                    cost = dsc->tree->GetCostEx();
                }

                if (!Is_Shared_Const_CSE(dsc->hashKey))
                {
                    printf(FMT_CSE ", {$%-3x, $%-3x} useCnt=%d: [def=%3f, use=%3f, cost=%3u%s]\n        :: ",
                           dsc->index, dsc->hashKey, dsc->defExcSetPromise, dsc->useCount, def, use, cost,
                           dsc->isLiveAcrossCall ? ", call" : "      ");
                }
                else
                {
                    size_t kVal = Decode_Shared_Const_CSE_Value(dsc->hashKey);
                    printf(FMT_CSE ", {K_%p} useCnt=%d: [def=%3f, use=%3f, cost=%3u%s]\n        :: ", dsc->index,
                           dspPtr(kVal), dsc->useCount, def, use, cost, dsc->isLiveAcrossCall ? ", call" : "      ");
                }

                m_pCompiler->gtDispTree(expr, nullptr, nullptr, true);
            }
            printf("\n");
        }
#endif // DEBUG
    }

    //  The following class nested within CseHeuristic encapsulates the information
    //  about the current CSE candidate that is under consideration
    //
    //  TODO-Cleanup: This is still very much based upon the old Lexical CSE implementation
    //  and needs to be reworked for the Value Number based implementation
    //
    class Candidate
    {
        CseHeuristic* m_context;
        CseDesc*      m_CseDsc;

        unsigned             m_cseIndex;
        BasicBlock::weight_t m_defCount;
        BasicBlock::weight_t m_useCount;
        unsigned             m_Cost;
        unsigned             m_Size;

        // When this Candidate is successfully promoted to a CSE we record
        // the following information about what category was used when promoting it.
        //
        //  We will set m_Aggressive:
        //    When we believe that the CSE very valuable in terms of weighted ref counts,
        //    such that it would always be enregistered by the register allocator.
        //
        //  We will set m_Moderate:
        //    When we believe that the CSE is moderately valuable in terms of weighted ref counts,
        //    such that it is more likely than not to be enregistered by the register allocator
        //
        //  We will set m_Conservative:
        //    When we didn't set m_Aggressive or  m_Moderate.
        //    Such candidates typically are expensive to compute and thus are
        //    always profitable to promote even when they aren't enregistered.
        //
        //  We will set  m_StressCSE:
        //    When the candidate is only being promoted because of a Stress mode.
        //
        bool m_Aggressive;
        bool m_Moderate;
        bool m_Conservative;
        bool m_StressCSE;

    public:
        Candidate(CseHeuristic* context, CseDesc* cseDsc)
            : m_context(context)
            , m_CseDsc(cseDsc)
            , m_cseIndex(m_CseDsc->index)
            , m_defCount(0)
            , m_useCount(0)
            , m_Cost(0)
            , m_Size(0)
            , m_Aggressive(false)
            , m_Moderate(false)
            , m_Conservative(false)
            , m_StressCSE(false)
        {
        }

        CseDesc* CseDsc()
        {
            return m_CseDsc;
        }
        unsigned CseIndex()
        {
            return m_cseIndex;
        }
        BasicBlock::weight_t DefCount()
        {
            return m_defCount;
        }
        BasicBlock::weight_t UseCount()
        {
            return m_useCount;
        }
        // TODO-CQ: With ValNum CSE's the Expr and its cost can vary.
        GenTree* Expr()
        {
            return m_CseDsc->tree;
        }
        unsigned Cost()
        {
            return m_Cost;
        }
        unsigned Size()
        {
            return m_Size;
        }

        bool IsSharedConst()
        {
            return m_CseDsc->isSharedConst;
        }

        bool LiveAcrossCall()
        {
            return m_CseDsc->isLiveAcrossCall;
        }

        void SetAggressive()
        {
            m_Aggressive = true;
        }

        bool IsAggressive()
        {
            return m_Aggressive;
        }

        void SetModerate()
        {
            m_Moderate = true;
        }

        bool IsModerate()
        {
            return m_Moderate;
        }

        void SetConservative()
        {
            m_Conservative = true;
        }

        bool IsConservative()
        {
            return m_Conservative;
        }

        void SetStressCSE()
        {
            m_StressCSE = true;
        }

        bool IsStressCSE()
        {
            return m_StressCSE;
        }

        void InitializeCounts()
        {
            m_Size = Expr()->GetCostSz(); // always the GetCostSz()
            if (m_context->CodeOptKind() == Compiler::SMALL_CODE)
            {
                m_Cost     = m_Size;             // the estimated code size
                m_defCount = m_CseDsc->defCount; // def count
                m_useCount = m_CseDsc->useCount; // use count (excluding the implicit uses at defs)
            }
            else
            {
                m_Cost     = Expr()->GetCostEx(); // the estimated execution cost
                m_defCount = m_CseDsc->defWeight; // weighted def count
                m_useCount = m_CseDsc->useWeight; // weighted use count (excluding the implicit uses at defs)
            }
        }
    };

#ifdef DEBUG
    //------------------------------------------------------------------------
    // ConfigBiasedCse:
    //     Stress mode to shuffle the decision to CSE or not using environment
    //     variable COMPlus_JitStressBiasedCSE (= 0 to 100%). When the bias value
    //     is not specified but COMPlus_JitStress is ON, generate a random bias.
    //
    // Return Value:
    //      0 -- This method is indifferent about this CSE (no bias specified and no stress)
    //      1 -- This CSE must be performed to maintain specified/generated bias.
    //     -1 -- This CSE mustn't be performed to maintain specified/generated bias.
    //
    // Operation:
    //     A debug stress only method that returns "1" with probability (P)
    //     defined by:
    //
    //         P = (COMPlus_JitStressBiasedCSE / 100) (or)
    //         P = (random(100) / 100) when COMPlus_JitStress is specified and
    //                                 COMPlus_JitStressBiasedCSE is unspecified.
    //
    //     When specified, the bias is reinterpreted as a decimal number between 0
    //     to 100.
    //     When bias is not specified, a bias is randomly generated if COMPlus_JitStress
    //     is non-zero.
    //
    //     Callers are supposed to call this method for each CSE promotion decision
    //     and ignore the call if return value is 0 and honor the 1 with a CSE and
    //     -1 with a no-CSE to maintain the specified/generated bias.
    //
    int ConfigBiasedCse()
    {
        // Seed the PRNG, if never done before.
        if (!m_cseRNG.IsInitialized())
        {
            m_cseRNG.Init(m_pCompiler->info.compMethodHash());
            m_bias = m_cseRNG.Next(100);
        }

        // Obtain the bias value and reinterpret as decimal.
        unsigned bias = ReinterpretHexAsDecimal(JitConfig.JitStressBiasedCSE());

        // Invalid value, check if JitStress is ON.
        if (bias > 100)
        {
            if (!m_pCompiler->compStressCompile(Compiler::STRESS_MAKE_CSE, MAX_STRESS_WEIGHT))
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

        if (m_pCompiler->verbose)
        {
            if (ret < 0)
            {
                printf("No CSE because gen=%d >= bias=%d\n", gen, bias);
            }
            else
            {
                printf("Promoting CSE because gen=%d < bias=%d\n", gen, bias);
            }
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
                    if (m_pCompiler->verbose)
                    {
                        printf(" Disabled by jitNoCSE2 Ones/Zeros mask\n");
                    }
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
                    if (m_pCompiler->verbose)
                    {
                        printf(" Disabled by jitNoCSE2 rotating disable mask\n");
                    }
                    return true;
                }
            }
            else if (jitNoCSE2 <= totalCSEcount)
            {
                if (m_pCompiler->verbose)
                {
                    printf(" Disabled by jitNoCSE2 > totalCSEcount\n");
                }
                return true;
            }
        }
        return false;
    }
#endif // DEBUG

    // Given a CSE candidate decide whether it passes or fails the profitability heuristic
    // return true if we believe that it is profitable to promote this candidate to a CSE
    //
    bool PromotionCheck(Candidate* candidate)
    {
        var_types cseLclVarTyp = varActualType(candidate->Expr()->GetType());

        if (varTypeIsSIMD(cseLclVarTyp) && (candidate->CseDsc()->layout == nullptr))
        {
            // If we haven't found an exact layout yet then use any layout that happens
            // to have the same SIMD type as the expression. Even so, it's possible to
            // not have a layout, usually due to SIMD typed nodes generated internally
            // by the JIT, in such cases there's no struct handle and the layout table
            // isn't populated. In such a case we don't have any option but to reject
            // this CSE.

            // TODO-MIKE-Cleanup: Do we really need this? There are other places that
            // create SIMD temps without layout so why bother at all here?

            ClassLayout* layout = m_pCompiler->typGetVectorLayout(candidate->Expr());

            if (layout == nullptr)
            {
                return false;
            }

            candidate->CseDsc()->layout = layout;
        }

        bool result = false;

#ifdef DEBUG
        int stressResult = ConfigBiasedCse();
        if (stressResult != 0)
        {
            // Stress is enabled. Check whether to perform CSE or not.
            if (stressResult > 0)
            {
                candidate->SetStressCSE();
                return true;
            }
        }

        if (ConfigDisableCse2())
        {
            return false; // skip this CSE
        }
#endif

        /*
            Our calculation is based on the following cost estimate formula

            Existing costs are:

            (def + use) * cost

            If we introduce a CSE temp are each definition and
            replace the use with a CSE temp then our cost is:

            (def * (cost + cse-def-cost)) + (use * cse-use-cost)

            We must estimate the values to use for cse-def-cost and cse-use-cost

            If we are able to enregister the CSE then the cse-use-cost is one
            and cse-def-cost is either zero or one.  Zero in the case where
            we needed to evaluate the def into a register and we can use that
            register as the CSE temp as well.

            If we are unable to enregister the CSE then the cse-use-cost is IND_COST
            and the cse-def-cost is also IND_COST.

            If we want to be conservative we use IND_COST as the the value
            for both cse-def-cost and cse-use-cost and then we never introduce
            a CSE that could pessimize the execution time of the method.

            If we want to be more moderate we use (IND_COST_EX + 1) / 2 as the
            values for both cse-def-cost and cse-use-cost.

            If we want to be aggressive we use 1 as the values for both
            cse-def-cost and cse-use-cost.

            If we believe that the CSE very valuable in terms of weighted ref counts
            such that it would always be enregistered by the register allocator we choose
            the aggressive use def costs.

            If we believe that the CSE is somewhat valuable in terms of weighted ref counts
            such that it could be likely be enregistered by the register allocator we choose
            the moderate use def costs.

            otherwise we choose the conservative use def costs.

        */

        unsigned cse_def_cost;
        unsigned cse_use_cost;

        BasicBlock::weight_t no_cse_cost    = 0;
        BasicBlock::weight_t yes_cse_cost   = 0;
        unsigned             extra_yes_cost = 0;
        unsigned             extra_no_cost  = 0;

        // The 'cseRefCnt' is the RefCnt that we will have if we promote this CSE into a new LclVar
        // Each CSE Def will contain two Refs and each CSE Use will have one Ref of this new LclVar
        BasicBlock::weight_t cseRefCnt = (candidate->DefCount() * 2) + candidate->UseCount();

        bool     canEnregister = true;
        unsigned slotCount     = 1;

        if (CodeOptKind() == Compiler::SMALL_CODE)
        {
            // Note that when optimizing for SMALL_CODE we set the cse_def_cost/cse_use_cost based
            // upon the code size and we use unweighted ref counts instead of weighted ref counts.
            // Also note that optimizing for SMALL_CODE is rare, we typically only optimize this way
            // for class constructors, because we know that they will only run once.
            //
            if (cseRefCnt >= aggressiveRefCnt)
            {
                // Record that we are choosing to use the aggressive promotion rules
                //
                candidate->SetAggressive();
#ifdef DEBUG
                if (m_pCompiler->verbose)
                {
                    printf("Aggressive CSE Promotion (%f >= %f)\n", cseRefCnt, aggressiveRefCnt);
                }
#endif
                // With aggressive promotion we expect that the candidate will be enregistered
                // so we set the use and def costs to their miniumum values
                //
                cse_def_cost = 1;
                cse_use_cost = 1;

                // Check if this candidate is likely to live on the stack
                //
                if (candidate->LiveAcrossCall() || !canEnregister)
                {
                    // Increase the costs when we have a large or huge frame
                    //
                    if (largeFrame)
                    {
                        cse_def_cost++;
                        cse_use_cost++;
                    }
                    if (hugeFrame)
                    {
                        cse_def_cost++;
                        cse_use_cost++;
                    }
                }
            }
            else // not aggressiveRefCnt
            {
                // Record that we are choosing to use the conservative promotion rules
                //
                candidate->SetConservative();
                if (largeFrame)
                {
#ifdef DEBUG
                    if (m_pCompiler->verbose)
                    {
                        printf("Codesize CSE Promotion (%s frame)\n", hugeFrame ? "huge" : "large");
                    }
#endif
#ifdef TARGET_XARCH
                    /* The following formula is good choice when optimizing CSE for SMALL_CODE */
                    cse_def_cost = 6; // mov [EBP-0x00001FC],reg
                    cse_use_cost = 5; //     [EBP-0x00001FC]
#else                                 // TARGET_ARM
                    if (hugeFrame)
                    {
                        cse_def_cost = 10 + 2; // movw/movt r10 and str reg,[sp+r10]
                        cse_use_cost = 10 + 2;
                    }
                    else
                    {
                        cse_def_cost = 6 + 2; // movw r10 and str reg,[sp+r10]
                        cse_use_cost = 6 + 2;
                    }
#endif
                }
                else // small frame
                {
#ifdef DEBUG
                    if (m_pCompiler->verbose)
                    {
                        printf("Codesize CSE Promotion (small frame)\n");
                    }
#endif
#ifdef TARGET_XARCH
                    /* The following formula is good choice when optimizing CSE for SMALL_CODE */
                    cse_def_cost = 3; // mov [EBP-1C],reg
                    cse_use_cost = 2; //     [EBP-1C]

#else // TARGET_ARM

                    cse_def_cost = 2; // str reg,[sp+0x9c]
                    cse_use_cost = 2; // ldr reg,[sp+0x9c]
#endif
                }
            }
#ifdef TARGET_AMD64
            if (varTypeIsFloating(candidate->Expr()->TypeGet()))
            {
                // floating point loads/store encode larger
                cse_def_cost += 2;
                cse_use_cost += 1;
            }
#endif // TARGET_AMD64
        }
        else // not SMALL_CODE ...
        {
            // Note that when optimizing for BLENDED_CODE or FAST_CODE we set cse_def_cost/cse_use_cost
            // based upon the execution costs of the code and we use weighted ref counts.
            //
            if ((cseRefCnt >= aggressiveRefCnt) && canEnregister)
            {
                // Record that we are choosing to use the aggressive promotion rules
                //
                candidate->SetAggressive();
#ifdef DEBUG
                if (m_pCompiler->verbose)
                {
                    printf("Aggressive CSE Promotion (%f >= %f)\n", cseRefCnt, aggressiveRefCnt);
                }
#endif
                // With aggressive promotion we expect that the candidate will be enregistered
                // so we set the use and def costs to their miniumum values
                //
                cse_def_cost = 1;
                cse_use_cost = 1;
            }
            else if (cseRefCnt >= moderateRefCnt)
            {
                // Record that we are choosing to use the moderate promotion rules
                //
                candidate->SetModerate();
                if (!candidate->LiveAcrossCall() && canEnregister)
                {
#ifdef DEBUG
                    if (m_pCompiler->verbose)
                    {
                        printf("Moderate CSE Promotion (CSE never live at call) (%f >= %f)\n", cseRefCnt,
                               moderateRefCnt);
                    }
#endif
                    cse_def_cost = 2;
                    cse_use_cost = 1;
                }
                else // candidate is live across call or not enregisterable.
                {
#ifdef DEBUG
                    if (m_pCompiler->verbose)
                    {
                        printf("Moderate CSE Promotion (%s) (%f >= %f)\n",
                               candidate->LiveAcrossCall() ? "CSE is live across a call" : "not enregisterable",
                               cseRefCnt, moderateRefCnt);
                    }
#endif
                    cse_def_cost = 2;
                    if (canEnregister)
                    {
                        if (enregCount < (CNT_CALLEE_ENREG * 3 / 2))
                        {
                            cse_use_cost = 1;
                        }
                        else
                        {
                            cse_use_cost = 2;
                        }
                    }
                    else
                    {
                        cse_use_cost = 3;
                    }
                }
            }
            else // Conservative CSE promotion
            {
                // Record that we are choosing to use the conservative promotion rules
                //
                candidate->SetConservative();
                if (!candidate->LiveAcrossCall() && canEnregister)
                {
#ifdef DEBUG
                    if (m_pCompiler->verbose)
                    {
                        printf("Conservative CSE Promotion (%s) (%f < %f)\n",
                               candidate->LiveAcrossCall() ? "CSE is live across a call" : "not enregisterable",
                               cseRefCnt, moderateRefCnt);
                    }
#endif
                    cse_def_cost = 2;
                    cse_use_cost = 2;
                }
                else // candidate is live across call
                {
#ifdef DEBUG
                    if (m_pCompiler->verbose)
                    {
                        printf("Conservative CSE Promotion (%f < %f)\n", cseRefCnt, moderateRefCnt);
                    }
#endif
                    cse_def_cost = 2;
                    cse_use_cost = 3;
                }

                // If we have maxed out lvaTrackedCount then this CSE may end up as an untracked variable
                if (m_pCompiler->lvaTrackedCount == (unsigned)JitConfig.JitMaxLocalsToTrack())
                {
                    cse_def_cost += 1;
                    cse_use_cost += 1;
                }
            }
        }

        if (slotCount > 1)
        {
            cse_def_cost *= slotCount;
            cse_use_cost *= slotCount;
        }

        // If this CSE is live across a call then we may need to spill an additional caller save register
        //
        if (candidate->LiveAcrossCall())
        {
            if (candidate->Expr()->IsCnsFltOrDbl() && (CNT_CALLEE_SAVED_FLOAT == 0) &&
                (candidate->CseDsc()->useWeight <= 4))
            {
                // Floating point constants are expected to be contained, so unless there are more than 4 uses
                // we better not to CSE them, especially on platforms without callee-saved registers
                // for values living across calls
                return false;
            }

            // If we don't have a lot of variables to enregister or we have a floating point type
            // then we will likely need to spill an additional caller save register.
            //
            if ((enregCount < (CNT_CALLEE_ENREG * 3 / 2)) || varTypeIsFloating(candidate->Expr()))
            {
                // Extra cost in case we have to spill/restore a caller saved register
                extra_yes_cost = BB_UNITY_WEIGHT_UNSIGNED;

                if (cseRefCnt < moderateRefCnt) // If Conservative CSE promotion
                {
                    extra_yes_cost *= 2; // full cost if we are being Conservative
                }
            }

#ifdef FEATURE_SIMD
            // SIMD types may cause a SIMD register to be spilled/restored in the prolog and epilog.
            //
            if (varTypeIsSIMD(candidate->Expr()->TypeGet()))
            {
                // We don't have complete information about when these extra spilled/restore will be needed.
                // Instead we are conservative and assume that each SIMD CSE that is live across a call
                // will cause an additional spill/restore in the prolog and epilog.
                //
                int spillSimdRegInProlog = 1;

                // If we have a SIMD32 that is live across a call we have even higher spill costs
                //
                if (candidate->Expr()->TypeGet() == TYP_SIMD32)
                {
                    // Additionally for a simd32 CSE candidate we assume that and second spilled/restore will be needed.
                    // (to hold the upper half of the simd32 register that isn't preserved across the call)
                    //
                    spillSimdRegInProlog++;

                    // We also increase the CSE use cost here to because we may have to generate instructions
                    // to move the upper half of the simd32 before and after a call.
                    //
                    cse_use_cost += 2;
                }

                extra_yes_cost = (BB_UNITY_WEIGHT_UNSIGNED * spillSimdRegInProlog) * 3;
            }
#endif // FEATURE_SIMD
        }

        // estimate the cost from lost codesize reduction if we do not perform the CSE
        if (candidate->Size() > cse_use_cost)
        {
            CseDesc* dsc = candidate->CseDsc(); // We need to retrieve the actual use count, not the
                                                // weighted count
            extra_no_cost = candidate->Size() - cse_use_cost;
            extra_no_cost = extra_no_cost * dsc->useCount * 2;
        }

        /* no_cse_cost  is the cost estimate when we decide not to make a CSE */
        /* yes_cse_cost is the cost estimate when we decide to make a CSE     */

        no_cse_cost  = candidate->UseCount() * candidate->Cost();
        yes_cse_cost = (candidate->DefCount() * cse_def_cost) + (candidate->UseCount() * cse_use_cost);

        no_cse_cost += extra_no_cost;
        yes_cse_cost += extra_yes_cost;

#ifdef DEBUG
        if (m_pCompiler->verbose)
        {
            printf("cseRefCnt=%f, aggressiveRefCnt=%f, moderateRefCnt=%f\n", cseRefCnt, aggressiveRefCnt,
                   moderateRefCnt);
            printf("defCnt=%f, useCnt=%f, cost=%d, size=%d%s\n", candidate->DefCount(), candidate->UseCount(),
                   candidate->Cost(), candidate->Size(), candidate->LiveAcrossCall() ? ", LiveAcrossCall" : "");
            printf("def_cost=%d, use_cost=%d, extra_no_cost=%d, extra_yes_cost=%d\n", cse_def_cost, cse_use_cost,
                   extra_no_cost, extra_yes_cost);

            printf("CSE cost savings check (%f >= %f) %s\n", no_cse_cost, yes_cse_cost,
                   (no_cse_cost >= yes_cse_cost) ? "passes" : "fails");
        }
#endif // DEBUG

        // Should we make this candidate into a CSE?
        // Is the yes cost less than the no cost
        //
        if (yes_cse_cost <= no_cse_cost)
        {
            result = true; // Yes make this a CSE
        }
        else
        {
            /* In stress mode we will make some extra CSEs */
            if (no_cse_cost > 0)
            {
                int percentage = (int)((no_cse_cost * 100) / yes_cse_cost);

                if (m_pCompiler->compStressCompile(Compiler::STRESS_MAKE_CSE, percentage))
                {
                    result = true; // Yes make this a CSE
                }
            }
        }

        return result;
    }

    // IsCompatibleType() takes two var_types and returns true if they
    // are compatible types for CSE substitution
    //
    bool IsCompatibleType(var_types cseLclVarTyp, var_types expTyp)
    {
        // Exact type match is the expected case
        if (cseLclVarTyp == expTyp)
        {
            return true;
        }

        // We also allow TYP_BYREF and TYP_I_IMPL as compatible types
        //
        if ((cseLclVarTyp == TYP_BYREF) && (expTyp == TYP_I_IMPL))
        {
            return true;
        }
        if ((cseLclVarTyp == TYP_I_IMPL) && (expTyp == TYP_BYREF))
        {
            return true;
        }

        // Otherwise we have incompatible types
        return false;
    }

    // PerformCSE() takes a successful candidate and performs  the appropriate replacements:
    //
    // It will replace all of the CSE defs with assignments to a new "cse0" LclVar
    // and will replace all of the CSE uses with reads of the "cse0" LclVar
    //
    // It will also put cse0 into SSA if there is just one def.
    void PerformCSE(Candidate* successfulCandidate)
    {
        BasicBlock::weight_t cseRefCnt = (successfulCandidate->DefCount() * 2) + successfulCandidate->UseCount();

        if (successfulCandidate->LiveAcrossCall() != 0)
        {
            // As we introduce new LclVars for these CSE we slightly
            // increase the cutoffs for aggressive and moderate CSE's
            //
            BasicBlock::weight_t incr = BB_UNITY_WEIGHT;

            if (cseRefCnt > aggressiveRefCnt)
            {
                aggressiveRefCnt += incr;
            }

            if (cseRefCnt > moderateRefCnt)
            {
                moderateRefCnt += (incr / 2);
            }
        }

#ifdef DEBUG
        // Setup the message arg for lvaGrabTemp()
        //
        const char* grabTempMessage = "CSE - unknown";

        if (successfulCandidate->IsAggressive())
        {
            grabTempMessage = "CSE - aggressive";
        }
        else if (successfulCandidate->IsModerate())
        {
            grabTempMessage = "CSE - moderate";
        }
        else if (successfulCandidate->IsConservative())
        {
            grabTempMessage = "CSE - conservative";
        }
        else if (successfulCandidate->IsStressCSE())
        {
            grabTempMessage = "CSE - stress mode";
        }
#endif // DEBUG

        // we will create a  long lifetime temp for the new CSE LclVar
        unsigned   cseLclVarNum = m_pCompiler->lvaGrabTemp(false DEBUGARG(grabTempMessage));
        var_types  cseLclVarTyp = varActualType(successfulCandidate->Expr()->GetType());
        LclVarDsc* cseLcl       = m_pCompiler->lvaGetDesc(cseLclVarNum);

        if (varTypeIsStruct(cseLclVarTyp))
        {
            m_pCompiler->lvaSetStruct(cseLclVarNum, successfulCandidate->CseDsc()->layout, false);
            assert(cseLcl->GetType() == cseLclVarTyp);
        }
        else
        {
            cseLcl->SetType(cseLclVarTyp);
        }

        cseLcl->lvIsCSE = true;

        // Record that we created a new LclVar for use as a CSE temp
        m_addCSEcount++;
        m_pCompiler->cseCount++;

        //  Walk all references to this CSE, adding an assignment
        //  to the CSE temp to all defs and changing all refs to
        //  a simple use of the CSE temp.
        //
        //  Later we will unmark any nested CSE's for the CSE uses.
        //
        CseDesc* dsc = successfulCandidate->CseDsc();

        // If there's just a single def for the CSE, we'll put this
        // CSE into SSA form on the fly. We won't need any PHIs.
        unsigned cseSsaNum = SsaConfig::RESERVED_SSA_NUM;

        if (dsc->defCount == 1)
        {
            JITDUMP(FMT_CSE " is single-def, so associated CSE temp V%02u will be in SSA\n", dsc->index, cseLclVarNum);
            cseLcl->lvInSsa = true;

            cseSsaNum = cseLcl->lvPerSsaData.AllocSsaNum(m_pCompiler->getAllocator(CMK_SSA));
        }

        // Verify that all of the ValueNumbers in this list are correct as
        // Morph will change them when it performs a mutating operation.
        //
        bool          setRefCnt      = true;
        bool          allSame        = true;
        bool          isSharedConst  = successfulCandidate->IsSharedConst();
        ValueNum      bestVN         = ValueNumStore::NoVN;
        bool          bestIsDef      = false;
        ssize_t       bestConstValue = 0;
        CseOccurence* lst            = dsc->treeList;

        while (lst != nullptr)
        {
            // Ignore this node if the gtCSEnum value has been cleared
            if (IsCseIndex(lst->tree->gtCSEnum))
            {
                // We used the liberal Value numbers when building the set of CSE
                ValueNum currVN = m_pCompiler->vnStore->VNLiberalNormalValue(lst->tree->gtVNPair);
                assert(currVN != ValueNumStore::NoVN);
                ssize_t curConstValue = isSharedConst ? m_pCompiler->vnStore->CoercedConstantValue<ssize_t>(currVN) : 0;

                GenTree* exp   = lst->tree;
                bool     isDef = IsCseDef(exp->gtCSEnum);

                if (bestVN == ValueNumStore::NoVN)
                {
                    // first entry
                    // set bestVN
                    bestVN = currVN;

                    if (isSharedConst)
                    {
                        // set bestConstValue and bestIsDef
                        bestConstValue = curConstValue;
                        bestIsDef      = isDef;
                    }
                }
                else if (currVN != bestVN)
                {
                    assert(isSharedConst); // Must be true when we have differing VNs

                    // subsequent entry
                    // clear allSame and check for a lower constant
                    allSame = false;

                    ssize_t diff = curConstValue - bestConstValue;

                    // The ARM addressing modes allow for a subtraction of up to 255
                    // so we will allow the diff to be up to -255 before replacing a CSE def
                    // This will minimize the number of extra subtract instructions.
                    //
                    if ((bestIsDef && (diff < -255)) || (!bestIsDef && (diff < 0)))
                    {
                        // set new bestVN, bestConstValue and bestIsDef
                        bestVN         = currVN;
                        bestConstValue = curConstValue;
                        bestIsDef      = isDef;
                    }
                }

                BasicBlock*          blk       = lst->block;
                BasicBlock::weight_t curWeight = blk->getBBWeight(m_pCompiler);

                if (setRefCnt)
                {
                    cseLcl->SetRefCount(1);
                    cseLcl->SetRefWeight(curWeight);
                    setRefCnt = false;
                }
                else
                {
                    m_pCompiler->lvaAddRef(cseLcl, curWeight);
                }

                // A CSE Def references the LclVar twice
                if (isDef)
                {
                    m_pCompiler->lvaAddRef(cseLcl, curWeight);
                }
            }
            lst = lst->next;
        }

        dsc->constDefValue = bestConstValue;
        dsc->constDefVN    = bestVN;

#ifdef DEBUG
        if (m_pCompiler->verbose)
        {
            if (!allSame)
            {
                if (isSharedConst)
                {
                    printf("\nWe have shared Const CSE's and selected " FMT_VN " with a value of 0x%p as the base.\n",
                           dsc->constDefVN, dspPtr(dsc->constDefValue));
                }
                else // !isSharedConst
                {
                    lst                = dsc->treeList;
                    GenTree* firstTree = lst->tree;
                    printf("In %s, CSE (oper = %s, type = %s) has differing VNs: ", m_pCompiler->info.compFullName,
                           GenTree::OpName(firstTree->OperGet()), varTypeName(firstTree->TypeGet()));
                    while (lst != nullptr)
                    {
                        if (IsCseIndex(lst->tree->gtCSEnum))
                        {
                            ValueNum currVN = m_pCompiler->vnStore->VNLiberalNormalValue(lst->tree->gtVNPair);
                            printf("[%06d](%s " FMT_VN ") ", m_pCompiler->dspTreeID(lst->tree),
                                   IsCseUse(lst->tree->gtCSEnum) ? "use" : "def", currVN);
                        }
                        lst = lst->next;
                    }
                    printf("\n");
                }
            }
        }
#endif // DEBUG

        // Setup 'lst' to point at the start of this candidate list
        lst = dsc->treeList;
        noway_assert(lst);

        do
        {
            /* Process the next node in the list */
            GenTree*    exp  = lst->tree;
            Statement*  stmt = lst->stmt;
            BasicBlock* blk  = lst->block;

            /* Advance to the next node in the list */
            lst = lst->next;

            // We may have cleared this CSE in optValuenumCSE_Availablity
            // due to different exception sets.
            //
            // Ignore this node if the gtCSEnum value has been cleared
            if (!IsCseIndex(exp->gtCSEnum))
            {
                continue;
            }

            // Assert if we used DEBUG_DESTROY_NODE on this CSE exp
            assert(exp->gtOper != GT_COUNT);

            /* Make sure we update the weighted ref count correctly */
            m_pCompiler->cseBlockWeight = blk->getBBWeight(m_pCompiler);

            /* Figure out the actual type of the value */
            var_types expTyp = genActualType(exp->TypeGet());

            // The cseLclVarType must be a compatible with expTyp
            //
            ValueNumStore* vnStore = m_pCompiler->vnStore;
            noway_assert(IsCompatibleType(cseLclVarTyp, expTyp) || (dsc->constDefVN != vnStore->VNForNull()));

            // This will contain the replacement tree for exp
            // It will either be the CSE def or CSE ref
            //
            GenTree*      cse = nullptr;
            bool          isDef;
            GenTree*      effectiveExp = exp->SkipComma();
            FieldSeqNode* fieldSeq     = m_pCompiler->GetZeroOffsetFieldSeq(effectiveExp);

            if (IsCseUse(exp->gtCSEnum))
            {
                /* This is a use of the CSE */
                isDef = false;
#ifdef DEBUG
                if (m_pCompiler->verbose)
                {
                    printf("\nWorking on the replacement of the " FMT_CSE " use at ", exp->gtCSEnum);
                    Compiler::printTreeID(exp);
                    printf(" in " FMT_BB "\n", blk->bbNum);
                }
#endif // DEBUG

                // We will replace the CSE ref with a new tree
                // this is typically just a simple use of the new CSE LclVar
                //

                // Create a reference to the CSE temp
                GenTree* cseLclVar = m_pCompiler->gtNewLclvNode(cseLclVarNum, cseLclVarTyp);
                cseLclVar->gtVNPair.SetBoth(dsc->constDefVN);

                // Assign the ssa num for the lclvar use. Note it may be the reserved num.
                cseLclVar->AsLclVarCommon()->SetSsaNum(cseSsaNum);

                cse = cseLclVar;
                if (isSharedConst)
                {
                    ValueNum currVN   = m_pCompiler->vnStore->VNLiberalNormalValue(exp->gtVNPair);
                    ssize_t  curValue = m_pCompiler->vnStore->CoercedConstantValue<ssize_t>(currVN);
                    ssize_t  delta    = curValue - dsc->constDefValue;
                    if (delta != 0)
                    {
                        GenTree* deltaNode = m_pCompiler->gtNewIconNode(delta, cseLclVarTyp);
                        cse                = m_pCompiler->gtNewOperNode(GT_ADD, cseLclVarTyp, cseLclVar, deltaNode);
                        cse->SetDoNotCSE();
                    }
                }

                // assign the proper ValueNumber, A CSE use discards any exceptions
                cse->gtVNPair = vnStore->VNPNormalPair(exp->gtVNPair);

                // shared const CSE has the correct value number assigned
                // and both liberal and conservative are identical
                // and they do not use theConservativeVN
                //
                if (!isSharedConst)
                {
                    ValueNum theConservativeVN = successfulCandidate->CseDsc()->defConservNormVN;

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
                        if (m_cse.checkedBoundMap.Lookup(exp, &cmp))
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
                //
                exp->gtCSEnum = NoCse; // clear the gtCSEnum field

                GenTree* sideEffList = nullptr;
                m_pCompiler->gtExtractSideEffList(exp, &sideEffList, GTF_PERSISTENT_SIDE_EFFECTS | GTF_IS_IN_CSE);

                // If we have any side effects or extracted CSE defs then we need to create a GT_COMMA tree instead
                //
                if (sideEffList != nullptr)
                {
#ifdef DEBUG
                    if (m_pCompiler->verbose)
                    {
                        printf("\nThis CSE use has side effects and/or nested CSE defs. The sideEffectList:\n");
                        m_pCompiler->gtDispTree(sideEffList);
                        printf("\n");
                    }
#endif

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
                    cse           = m_pCompiler->gtNewCommaNode(sideEffList, cseVal, expTyp);
                    cse->gtVNPair = vnStore->VNPWithExc(op2vnp, exceptions_vnp);
                }
            }
            else
            {
                /* This is a def of the CSE */
                isDef = true;
#ifdef DEBUG
                if (m_pCompiler->verbose)
                {
                    printf("\n" FMT_CSE " def at ", GetCseIndex(exp->gtCSEnum));
                    Compiler::printTreeID(exp);
                    printf(" replaced in " FMT_BB " with def of V%02u\n", blk->bbNum, cseLclVarNum);
                }
#endif // DEBUG

                exp->gtCSEnum = NoCse; // clear the gtCSEnum field

                GenTree* val = exp;
                if (isSharedConst)
                {
                    ValueNum currVN   = m_pCompiler->vnStore->VNLiberalNormalValue(exp->gtVNPair);
                    ssize_t  curValue = m_pCompiler->vnStore->CoercedConstantValue<ssize_t>(currVN);
                    ssize_t  delta    = curValue - dsc->constDefValue;
                    if (delta != 0)
                    {
                        val = m_pCompiler->gtNewIconNode(dsc->constDefValue, cseLclVarTyp);
                        val->gtVNPair.SetBoth(dsc->constDefVN);
                    }
                }

                assert((cseLclVarTyp != TYP_STRUCT) ||
                       (val->IsCall() && (val->AsCall()->GetRetDesc()->GetRegCount() == 1)));

                GenTreeLclVar* dstLclNode = m_pCompiler->gtNewLclvNode(cseLclVarNum, cseLclVarTyp);
                dstLclNode->SetVNP(val->gtVNPair);
                dstLclNode->SetSsaNum(cseSsaNum);

                GenTreeOp* asg = m_pCompiler->gtNewAssignNode(dstLclNode, val);
                asg->gtVNPair.SetBoth(ValueNumStore::VNForVoid());

                if (cseSsaNum != SsaConfig::RESERVED_SSA_NUM)
                {
                    LclSsaVarDsc* ssaVarDsc = m_pCompiler->lvaTable[cseLclVarNum].GetPerSsaData(cseSsaNum);

                    // These should not have been set yet, since this is the first and
                    // only def for this CSE.
                    assert(ssaVarDsc->GetBlock() == nullptr);
                    assert(ssaVarDsc->GetAssignment() == nullptr);

                    ssaVarDsc->m_vnPair = val->gtVNPair;
                    ssaVarDsc->SetBlock(blk);
                    ssaVarDsc->SetAssignment(asg->AsOp());
                }

                GenTreeLclVar* cseLclVar = m_pCompiler->gtNewLclvNode(cseLclVarNum, cseLclVarTyp);
                cseLclVar->gtVNPair.SetBoth(dsc->constDefVN);
                cseLclVar->SetSsaNum(cseSsaNum);

                GenTree* cseUse = cseLclVar;
                if (isSharedConst)
                {
                    ValueNum currVN   = m_pCompiler->vnStore->VNLiberalNormalValue(exp->gtVNPair);
                    ssize_t  curValue = m_pCompiler->vnStore->CoercedConstantValue<ssize_t>(currVN);
                    ssize_t  delta    = curValue - dsc->constDefValue;
                    if (delta != 0)
                    {
                        GenTree* deltaNode = m_pCompiler->gtNewIconNode(delta, cseLclVarTyp);
                        cseUse             = m_pCompiler->gtNewOperNode(GT_ADD, cseLclVarTyp, cseLclVar, deltaNode);
                        cseUse->SetDoNotCSE();
                    }
                }
                cseUse->gtVNPair = val->gtVNPair;

                cse = m_pCompiler->gtNewCommaNode(asg, cseUse, expTyp);
                // COMMA's VN is the same the original expression VN because assignment does not add any exceptions.
                cse->gtVNPair = cseUse->gtVNPair;
            }

            // Walk the statement 'stmt' and find the pointer
            // in the tree is pointing to 'exp'
            //
            Compiler::FindLinkData linkData = m_pCompiler->gtFindLink(stmt, exp);
            GenTree**              link     = linkData.useEdge;

#ifdef DEBUG
            if (link == nullptr)
            {
                printf("\ngtFindLink failed: stm=");
                Compiler::printStmtID(stmt);
                printf(", exp=");
                Compiler::printTreeID(exp);
                printf("\n");
                printf("stm =");
                m_pCompiler->gtDispStmt(stmt);
                printf("\n");
                printf("exp =");
                m_pCompiler->gtDispTree(exp);
                printf("\n");
            }
#endif // DEBUG

            noway_assert(link);

            // Mutate this link, thus replacing the old exp with the new CSE representation
            //
            *link = cse;

            // If it has a zero-offset field seq, copy annotation.
            if (fieldSeq != nullptr)
            {
                m_pCompiler->AddZeroOffsetFieldSeq(cse, fieldSeq);
            }

            assert(m_pCompiler->fgRemoveRestOfBlock == false);

            if (linkData.user != nullptr)
            {
                m_pCompiler->gtUpdateTreeAncestorsSideEffects(linkData.user);
            }

            m_pCompiler->gtSetStmtInfo(stmt);
            m_pCompiler->fgSetStmtSeq(stmt);

        } while (lst != nullptr);
    }

    // Consider each of the CSE candidates and if the CSE passes
    // the PromotionCheck then transform the CSE by calling PerformCSE
    //
    void ConsiderCandidates()
    {
        /* Consider each CSE candidate, in order of decreasing cost */
        unsigned  cnt = m_pCompiler->cseCandidateCount;
        CseDesc** ptr = sortTab;
        for (; (cnt > 0); cnt--, ptr++)
        {
            CseDesc*  dsc = *ptr;
            Candidate candidate(this, dsc);

            if (dsc->defExcSetPromise == ValueNumStore::NoVN)
            {
                JITDUMP("Abandoned " FMT_CSE " because we had defs with different Exc sets\n", candidate.CseIndex());
                continue;
            }

            candidate.InitializeCounts();

            if (candidate.UseCount() == 0)
            {
                JITDUMP("Skipped " FMT_CSE " because use count is 0\n", candidate.CseIndex());
                continue;
            }

#ifdef DEBUG
            if (m_pCompiler->verbose)
            {
                if (!Is_Shared_Const_CSE(dsc->hashKey))
                {
                    printf("\nConsidering " FMT_CSE " {$%-3x, $%-3x} [def=%3f, use=%3f, cost=%3u%s]\n",
                           candidate.CseIndex(), dsc->hashKey, dsc->defExcSetPromise, candidate.DefCount(),
                           candidate.UseCount(), candidate.Cost(), dsc->isLiveAcrossCall ? ", call" : "      ");
                }
                else
                {
                    size_t kVal = Decode_Shared_Const_CSE_Value(dsc->hashKey);
                    printf("\nConsidering " FMT_CSE " {K_%p} [def=%3f, use=%3f, cost=%3u%s]\n", candidate.CseIndex(),
                           dspPtr(kVal), candidate.DefCount(), candidate.UseCount(), candidate.Cost(),
                           dsc->isLiveAcrossCall ? ", call" : "      ");
                }
                printf("CSE Expression : \n");
                m_pCompiler->gtDispTree(candidate.Expr());
                printf("\n");
            }
#endif // DEBUG

            if ((dsc->defCount <= 0) || (dsc->useCount == 0))
            {
                // If we reach this point, then the CSE def was incorrectly marked or the
                // block with this use is unreachable. So skip and go to the next CSE.
                // Without the "continue", we'd generate bad code in retail.
                // Commented out a noway_assert(false) here due to bug: 3290124.
                // The problem is if there is sub-graph that is not reachable from the
                // entry point, the CSE flags propagated, would be incorrect for it.
                continue;
            }

            bool doCSE = PromotionCheck(&candidate);

#ifdef DEBUG
            if (m_pCompiler->verbose)
            {
                if (doCSE)
                {
                    printf("\nPromoting CSE:\n");
                }
                else
                {
                    printf("Did Not promote this CSE\n");
                }
            }
#endif // DEBUG

            if (doCSE)
            {
                PerformCSE(&candidate);
            }
        }
    }

    // Perform the necessary cleanup after our CSE heuristics have run
    //
    void Cleanup()
    {
        // Nothing to do, currently.
    }
};

/*****************************************************************************
 *
 *  Routine for performing the Value Number based CSE using our heuristics
 */

void Cse::Heuristic()
{
#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("\n************ Trees at start of Heuristic()\n");
        compiler->fgDumpTrees(compiler->fgFirstBB, nullptr);
        printf("\n");
    }
#endif // DEBUG

    CseHeuristic heuristic(compiler, *this);

    heuristic.Initialize();
    heuristic.SortCandidates();
    heuristic.ConsiderCandidates();
    heuristic.Cleanup();
}

#ifdef DEBUG

//------------------------------------------------------------------------
// optPrintCSEDataFlowSet: Print out one of the CSE dataflow sets bbCseGen, bbCseIn, bbCseOut,
// interpreting the bits in a more useful way for the dump.
//
// Arguments:
//    set - One of the dataflow sets to display
//    includeBits    - Display the actual bits of the set as well
//
void Cse::DumpDataFlowSet(EXPSET_VALARG_TP set, bool includeBits)
{
    if (includeBits)
    {
        printf("%s ", genES2str(&dataFlowTraits, set));
    }

    bool first = true;
    for (unsigned cseIndex = 1; cseIndex <= compiler->cseCandidateCount; cseIndex++)
    {
        unsigned cseAvailBit          = getCSEAvailBit(cseIndex);
        unsigned cseAvailCrossCallBit = getCSEAvailCrossCallBit(cseIndex);

        if (BitVecOps::IsMember(&dataFlowTraits, set, cseAvailBit))
        {
            if (!first)
            {
                printf(", ");
            }
            const bool isAvailCrossCall = BitVecOps::IsMember(&dataFlowTraits, set, cseAvailCrossCallBit);
            printf(FMT_CSE "%s", cseIndex, isAvailCrossCall ? ".c" : "");
            first = false;
        }
    }
}

#endif // DEBUG

void Compiler::cseMain()
{
    assert(ssaForm && (vnStore != nullptr));

    Cse cse(this);
    cse.Run();
}
