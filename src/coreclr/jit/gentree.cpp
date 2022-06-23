// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                               GenTree                                     XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#include "hwintrinsic.h"
#include "simd.h"

#ifdef _MSC_VER
#pragma hdrstop
#endif

/*****************************************************************************/

const unsigned short GenTree::gtOperKindTable[] = {
#define GTNODE(en, st, cm, ok) (ok) + GTK_COMMUTE *cm,
#include "gtlist.h"
};

/*****************************************************************************
 *
 *  The types of different GenTree nodes
 */

#ifdef DEBUG

#define INDENT_SIZE 3

//--------------------------------------------
//
// IndentStack: This struct is used, along with its related enums and strings,
//    to control both the indendtation and the printing of arcs.
//
// Notes:
//    The mode of printing is set in the Constructor, using its 'compiler' argument.
//    Currently it only prints arcs when fgOrder == fgOrderLinear.
//    The type of arc to print is specified by the IndentInfo enum, and is controlled
//    by the caller of the Push() method.

enum IndentChars
{
    ICVertical,
    ICBottom,
    ICTop,
    ICMiddle,
    ICDash,
    ICTerminal,
    ICError,
    IndentCharCount
};

// clang-format off
// Sets of strings for different dumping options            vert             bot             top             mid             dash       embedded    terminal    error
static const char*  emptyIndents[IndentCharCount]   = {     " ",             " ",            " ",            " ",            " ",            "",        "?"  };
static const char*  asciiIndents[IndentCharCount]   = {     "|",            "\\",            "/",            "+",            "-",            "*",       "?"  };
static const char*  unicodeIndents[IndentCharCount] = { "\xe2\x94\x82", "\xe2\x94\x94", "\xe2\x94\x8c", "\xe2\x94\x9c", "\xe2\x94\x80", "\xe2\x96\x8c", "?"  };
// clang-format on

struct IndentStack
{
    ArrayStack<Compiler::IndentInfo> stack;
    const char**                     indents;

    // Constructor for IndentStack.  Uses 'compiler' to determine the mode of printing.
    IndentStack(Compiler* compiler) : stack(compiler->getAllocator(CMK_DebugOnly))
    {
        if (compiler->asciiTrees)
        {
            indents = asciiIndents;
        }
        else
        {
            indents = unicodeIndents;
        }
    }

    // Return the depth of the current indentation.
    unsigned Depth()
    {
        return stack.Size();
    }

    // Push a new indentation onto the stack, of the given type.
    void Push(Compiler::IndentInfo info)
    {
        stack.Push(info);
    }

    // Pop the most recent indentation type off the stack.
    Compiler::IndentInfo Pop()
    {
        return stack.Pop();
    }

    // Print the current indentation and arcs.
    void print()
    {
        unsigned indentCount = Depth();
        for (unsigned i = 0; i < indentCount; i++)
        {
            unsigned index = indentCount - 1 - i;
            switch (stack.Top(index))
            {
                case Compiler::IndentInfo::IINone:
                    printf("   ");
                    break;
                case Compiler::IndentInfo::IIArc:
                    if (index == 0)
                    {
                        printf("%s%s%s", indents[ICMiddle], indents[ICDash], indents[ICDash]);
                    }
                    else
                    {
                        printf("%s  ", indents[ICVertical]);
                    }
                    break;
                case Compiler::IndentInfo::IIArcBottom:
                    printf("%s%s%s", indents[ICBottom], indents[ICDash], indents[ICDash]);
                    break;
                case Compiler::IndentInfo::IIArcTop:
                    printf("%s%s%s", indents[ICTop], indents[ICDash], indents[ICDash]);
                    break;
                case Compiler::IndentInfo::IIError:
                    printf("%s%s%s", indents[ICError], indents[ICDash], indents[ICDash]);
                    break;
                default:
                    unreached();
            }
        }
        printf("%s", indents[ICTerminal]);
    }
};

//------------------------------------------------------------------------
// printIndent: This is a static method which simply invokes the 'print'
//    method on its 'indentStack' argument.
//
// Arguments:
//    indentStack - specifies the information for the indentation & arcs to be printed
//
// Notes:
//    This method exists to localize the checking for the case where indentStack is null.

static void printIndent(IndentStack* indentStack)
{
    if (indentStack == nullptr)
    {
        return;
    }
    indentStack->print();
}

#endif

#if defined(DEBUG) || NODEBASH_STATS || MEASURE_NODE_SIZE || COUNT_AST_OPERS || DUMP_FLOWGRAPHS

static const char* opNames[] = {
#define GTNODE(en, st, cm, ok) #en,
#include "gtlist.h"
};

const char* GenTree::OpName(genTreeOps op)
{
    assert((unsigned)op < _countof(opNames));

    return opNames[op];
}

#endif

#if MEASURE_NODE_SIZE

static const char* opStructNames[] = {
#define GTNODE(en, st, cm, ok) #st,
#include "gtlist.h"
};

const char* GenTree::OpStructName(genTreeOps op)
{
    assert((unsigned)op < _countof(opStructNames));

    return opStructNames[op];
}

#endif

//  We allocate tree nodes in 2 different sizes:
//  - TREE_NODE_SZ_SMALL for most nodes
//  - TREE_NODE_SZ_LARGE for the few nodes (such as calls) that have
//    more fields and take up a lot more space.

template <typename T>
constexpr uint8_t GetNodeAllocationSize(genTreeOps oper)
{
    static_assert(sizeof(T) <= TREE_NODE_SZ_LARGE, "Node struct is too large");

    return (sizeof(T) > TREE_NODE_SZ_SMALL) ||
                   // Ensure that these are always large since they often get transformed into calls.
                   (oper == GT_INTRINSIC) || (oper == GT_ALLOCOBJ)
#if USE_HELPERS_FOR_INT_DIV
                   || (oper == GT_DIV) || (oper == GT_UDIV) || (oper == GT_MOD) || (oper == GT_UMOD)
#endif
               ? TREE_NODE_SZ_LARGE
               : TREE_NODE_SZ_SMALL;
}

// GT_COUNT'th oper is overloaded as 'undefined oper', so allocate storage for GT_COUNT'th oper also
const uint8_t GenTree::s_gtNodeSizes[GT_COUNT + 1]{
#define GTNODE(en, st, cm, ok) GetNodeAllocationSize<st>(GT_##en),
#include "gtlist.h"
    GetNodeAllocationSize<GenTree>(GT_COUNT)};

#if NODEBASH_STATS || MEASURE_NODE_SIZE || COUNT_AST_OPERS

const uint8_t GenTree::s_gtTrueSizes[GT_COUNT + 1]{
#define GTNODE(en, st, cm, ok) sizeof(st),
#include "gtlist.h"
};

#endif // NODEBASH_STATS || MEASURE_NODE_SIZE || COUNT_AST_OPERS

#if COUNT_AST_OPERS
LONG GenTree::s_gtNodeCounts[GT_COUNT + 1] = {0};
#endif // COUNT_AST_OPERS

// clang-format off
static_assert_no_msg(sizeof(GenTree)             <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeUnOp)         <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeOp)           <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeVal)          <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeIntConCommon) <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreePhysReg)      <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeIntCon)       <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeLngCon)       <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeDblCon)       <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeStrCon)       <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeLclVarCommon) <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeLclVar)       <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeLclFld)       <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeCC)           <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeCast)         <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeBox)          <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeFieldAddr)    <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeFieldList)    <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeCmpXchg)      <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeFptrVal)      <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeQmark)        <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeIndexAddr)    <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeArrLen)       <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeBoundsChk)    <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeArrIndex)     <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeArrOffs)      <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeIndir)        <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeStoreInd)     <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeAddrMode)     <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeObj)          <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeBlk)          <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeDynBlk)       <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeRetExpr)      <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeILOffset)     <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeClsVar)       <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreePhiArg)       <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreeInstr)        <= TREE_NODE_SZ_SMALL);
static_assert_no_msg(sizeof(GenTreePutArgStk)    <= TREE_NODE_SZ_SMALL);
#ifdef FEATURE_HW_INTRINSICS
static_assert_no_msg(sizeof(GenTreeHWIntrinsic)  <= TREE_NODE_SZ_SMALL);
#endif
static_assert_no_msg(sizeof(GenTreeCall)         <= TREE_NODE_SZ_LARGE); // *** large node
static_assert_no_msg(sizeof(GenTreeIntrinsic)    <= TREE_NODE_SZ_LARGE); // *** large node
static_assert_no_msg(sizeof(GenTreeArrElem)      <= TREE_NODE_SZ_LARGE); // *** large node
static_assert_no_msg(sizeof(GenTreeAllocObj)     <= TREE_NODE_SZ_LARGE); // *** large node
#if FEATURE_ARG_SPLIT
static_assert_no_msg(sizeof(GenTreePutArgSplit)  <= TREE_NODE_SZ_LARGE); // *** large node
#endif
// clang-format on

size_t GenTree::GetNodeSize() const
{
    return GenTree::s_gtNodeSizes[gtOper];
}

//------------------------------------------------------------------------
// ReplaceWith: replace this with the src node. The source must be an isolated node
//              and cannot be used after the replacement.
//
// Arguments:
//    src  - source tree, that replaces this.
//    comp - the compiler instance to transfer annotations for arrays.
//
void GenTree::ReplaceWith(GenTree* src, Compiler* comp)
{
    // The source may be big only if the target is also a big node
    assert((gtDebugFlags & GTF_DEBUG_NODE_LARGE) || GenTree::s_gtNodeSizes[src->gtOper] == TREE_NODE_SZ_SMALL);

    // The check is effective only if nodes have been already threaded.
    assert((src->gtPrev == nullptr) && (src->gtNext == nullptr));

    RecordOperBashing(OperGet(), src->OperGet()); // nop unless NODEBASH_STATS is enabled

    GenTree* prev = gtPrev;
    GenTree* next = gtNext;
    // The VTable pointer is copied intentionally here
    memcpy((void*)this, (void*)src, src->GetNodeSize());
    this->gtPrev = prev;
    this->gtNext = next;

    INDEBUG(gtSeqNum = 0;)
    DEBUG_DESTROY_NODE(src);
}

/*****************************************************************************
 *
 *  When 'NODEBASH_STATS' is enabled in "jit.h" we record all instances of
 *  an existing GenTree node having its operator changed. This can be useful
 *  for two (related) things - to see what is being bashed (and what isn't),
 *  and to verify that the existing choices for what nodes are marked 'large'
 *  are reasonable (to minimize "wasted" space).
 *
 *  And yes, the hash function / logic is simplistic, but it is conflict-free
 *  and transparent for what we need.
 */

#if NODEBASH_STATS

#define BASH_HASH_SIZE 211

inline unsigned hashme(genTreeOps op1, genTreeOps op2)
{
    return ((op1 * 104729) ^ (op2 * 56569)) % BASH_HASH_SIZE;
}

struct BashHashDsc
{
    unsigned __int32 bhFullHash; // the hash value (unique for all old->new pairs)
    unsigned __int32 bhCount;    // the same old->new bashings seen so far
    unsigned __int8  bhOperOld;  // original gtOper
    unsigned __int8  bhOperNew;  // new      gtOper
};

static BashHashDsc BashHash[BASH_HASH_SIZE];

void GenTree::RecordOperBashing(genTreeOps operOld, genTreeOps operNew)
{
    unsigned     hash = hashme(operOld, operNew);
    BashHashDsc* desc = BashHash + hash;

    if (desc->bhFullHash != hash)
    {
        noway_assert(desc->bhCount == 0); // if this ever fires, need fix the hash fn
        desc->bhFullHash = hash;
    }

    desc->bhCount += 1;
    desc->bhOperOld = operOld;
    desc->bhOperNew = operNew;
}

void GenTree::ReportOperBashing(FILE* f)
{
    unsigned total = 0;

    fflush(f);

    fprintf(f, "\n");
    fprintf(f, "Bashed gtOper stats:\n");
    fprintf(f, "\n");
    fprintf(f, "    Old operator        New operator     #bytes old->new      Count\n");
    fprintf(f, "    ---------------------------------------------------------------\n");

    for (unsigned h = 0; h < BASH_HASH_SIZE; h++)
    {
        unsigned count = BashHash[h].bhCount;
        if (count == 0)
            continue;

        unsigned opOld = BashHash[h].bhOperOld;
        unsigned opNew = BashHash[h].bhOperNew;

        fprintf(f, "    GT_%-13s -> GT_%-13s [size: %3u->%3u] %c %7u\n", OpName((genTreeOps)opOld),
                OpName((genTreeOps)opNew), s_gtTrueSizes[opOld], s_gtTrueSizes[opNew],
                (s_gtTrueSizes[opOld] < s_gtTrueSizes[opNew]) ? 'X' : ' ', count);
        total += count;
    }
    fprintf(f, "\n");
    fprintf(f, "Total bashings: %u\n", total);
    fprintf(f, "\n");

    fflush(f);
}

#endif // NODEBASH_STATS

/*****************************************************************************/

#if MEASURE_NODE_SIZE

void GenTree::DumpNodeSizes(FILE* fp)
{
    // Dump the sizes of the various GenTree flavors

    fprintf(fp, "Small tree node size = %3u bytes\n", TREE_NODE_SZ_SMALL);
    fprintf(fp, "Large tree node size = %3u bytes\n", TREE_NODE_SZ_LARGE);
    fprintf(fp, "\n");

    // Verify that node sizes are set kosherly and dump sizes
    for (unsigned op = GT_NONE + 1; op < GT_COUNT; op++)
    {
        unsigned needSize = s_gtTrueSizes[op];
        unsigned nodeSize = s_gtNodeSizes[op];

        const char* structNm = OpStructName((genTreeOps)op);
        const char* operName = OpName((genTreeOps)op);

        bool repeated = false;

        // Have we seen this struct flavor before?
        for (unsigned mop = GT_NONE + 1; mop < op; mop++)
        {
            if (strcmp(structNm, OpStructName((genTreeOps)mop)) == 0)
            {
                repeated = true;
                break;
            }
        }

        // Don't repeat the same GenTree flavor unless we have an error
        if (!repeated || needSize > nodeSize)
        {
            unsigned sizeChar = '?';

            if (nodeSize == TREE_NODE_SZ_SMALL)
                sizeChar = 'S';
            else if (nodeSize == TREE_NODE_SZ_LARGE)
                sizeChar = 'L';

            fprintf(fp, "GT_%-16s ... %-19s = %3u bytes (%c)", operName, structNm, needSize, sizeChar);
            if (needSize > nodeSize)
            {
                fprintf(fp, " -- ERROR -- allocation is only %u bytes!", nodeSize);
            }
            else if (needSize <= TREE_NODE_SZ_SMALL && nodeSize == TREE_NODE_SZ_LARGE)
            {
                fprintf(fp, " ... could be small");
            }

            fprintf(fp, "\n");
        }
    }
}

#endif // MEASURE_NODE_SIZE

/*****************************************************************************
 *
 *  Walk all basic blocks and call the given function pointer for all tree
 *  nodes contained therein.
 */

void Compiler::fgWalkAllTreesPre(fgWalkPreFn* visitor, void* pCallBackData)
{
    for (BasicBlock* const block : Blocks())
    {
        for (Statement* const stmt : block->Statements())
        {
            fgWalkTreePre(stmt->GetRootNodePointer(), visitor, pCallBackData);
        }
    }
}

//------------------------------------------------------------------
// gtHasReg: Whether node beeen assigned a register by LSRA
//
// Arguments:
//    None
//
// Return Value:
//    Returns true if the node was assigned a register.
//
//    In case of multi-reg call nodes, it is considered
//    having a reg if regs are allocated for all its
//    return values.
//
//    In case of GT_COPY or GT_RELOAD of a multi-reg call,
//    GT_COPY/GT_RELOAD is considered having a reg if it
//    has a reg assigned to any of its positions.
//
bool GenTree::gtHasReg() const
{
    bool hasReg = false;

    if (IsMultiRegCall())
    {
        const GenTreeCall* call = AsCall();

        // A Multi-reg call node is said to have regs, if it has
        // reg assigned to each of its result registers.
        for (unsigned i = 0; i < call->GetRegCount(); ++i)
        {
            hasReg = (call->GetRegNum(i) != REG_NA);
            if (!hasReg)
            {
                break;
            }
        }
    }
    else if (IsCopyOrReloadOfMultiRegCall())
    {
        const GenTreeCopyOrReload* copyOrReload = AsCopyOrReload();
        const GenTreeCall*         call         = copyOrReload->gtGetOp1()->AsCall();

        // A Multi-reg copy or reload node is said to have regs,
        // if it has valid regs in any of the positions.
        for (unsigned i = 0; i < call->GetRegCount(); ++i)
        {
            hasReg = (copyOrReload->GetRegNum(i) != REG_NA);
            if (hasReg)
            {
                break;
            }
        }
    }
    else
    {
        hasReg = (GetRegNum() != REG_NA);
    }

    return hasReg;
}

#ifdef DEBUG
//-----------------------------------------------------------------------------
// GetRegisterDstCount: Get the number of registers defined by the node.
//
// Arguments:
//    None
//
// Return Value:
//    The number of registers that this node defines.
//
// Notes:
//    This should not be called on a contained node.
//    This does not look at the actual register assignments, if any, and so
//    is valid after Lowering.
//
int GenTree::GetRegisterDstCount(Compiler* compiler) const
{
    assert(!isContained());

    if (IsMultiRegNode())
    {
        return GetMultiRegCount(compiler);
    }

    return IsValue() ? 1 : 0;
}
#endif // DEBUG

//---------------------------------------------------------------
// gtGetRegMask: Get the reg mask of the node.
//
// Arguments:
//    None
//
// Return Value:
//    Reg Mask of GenTree node.
//
regMaskTP GenTree::gtGetRegMask() const
{
    regMaskTP resultMask;

    if (IsMultiRegCall())
    {
        resultMask = genRegMask(GetRegNum());
        resultMask |= AsCall()->GetOtherRegMask();
    }
    else if (IsCopyOrReloadOfMultiRegCall())
    {
        // A multi-reg copy or reload, will have valid regs for only those
        // positions that need to be copied or reloaded.  Hence we need
        // to consider only those registers for computing reg mask.

        const GenTreeCopyOrReload* copyOrReload = AsCopyOrReload();
        const GenTreeCall*         call         = copyOrReload->gtGetOp1()->AsCall();

        resultMask = RBM_NONE;
        for (unsigned i = 0; i < call->GetRegCount(); ++i)
        {
            regNumber reg = copyOrReload->GetRegNum(i);
            if (reg != REG_NA)
            {
                resultMask |= genRegMask(reg);
            }
        }
    }
#if FEATURE_ARG_SPLIT
    else if (OperIsPutArgSplit())
    {
        const GenTreePutArgSplit* splitArg = AsPutArgSplit();
        const unsigned            regCount = splitArg->GetRegCount();

        resultMask = RBM_NONE;
        for (unsigned i = 0; i < regCount; ++i)
        {
            regNumber reg = splitArg->GetRegNum(i);
            assert(reg != REG_NA);
            resultMask |= genRegMask(reg);
        }
    }
#endif // FEATURE_ARG_SPLIT
    else
    {
        resultMask = genRegMask(GetRegNum());
    }

    return resultMask;
}

void GenTreeFieldList::AddField(Compiler* compiler, GenTree* node, unsigned offset, var_types type)
{
    m_uses.AddUse(new (compiler, CMK_ASTNode) Use(node, offset, type));
    gtFlags |= node->gtFlags & GTF_ALL_EFFECT;
}

void GenTreeFieldList::AddFieldLIR(Compiler* compiler, GenTree* node, unsigned offset, var_types type)
{
    m_uses.AddUse(new (compiler, CMK_ASTNode) Use(node, offset, type));
}

void GenTreeFieldList::InsertField(Compiler* compiler, Use* insertAfter, GenTree* node, unsigned offset, var_types type)
{
    m_uses.InsertUse(insertAfter, new (compiler, CMK_ASTNode) Use(node, offset, type));
    gtFlags |= node->gtFlags & GTF_ALL_EFFECT;
}

void GenTreeFieldList::InsertFieldLIR(
    Compiler* compiler, Use* insertAfter, GenTree* node, unsigned offset, var_types type)
{
    m_uses.InsertUse(insertAfter, new (compiler, CMK_ASTNode) Use(node, offset, type));
}

//---------------------------------------------------------------
// GetOtherRegMask: Get the reg mask of gtOtherRegs of call node
//
// Arguments:
//    None
//
// Return Value:
//    Reg mask of gtOtherRegs of call node.
//
regMaskTP GenTreeCall::GetOtherRegMask() const
{
    regMaskTP resultMask = RBM_NONE;

#if FEATURE_MULTIREG_RET
    for (unsigned i = 1; i < MAX_MULTIREG_COUNT; ++i)
    {
        regNumber reg = GetRegNum(i);

        if (reg == REG_NA)
        {
            break;
        }

        resultMask |= genRegMask(reg);
    }
#endif

    return resultMask;
}

//-------------------------------------------------------------------------
// IsPure:
//    Returns true if this call is pure. For now, this uses the same
//    definition of "pure" that is that used by HelperCallProperties: a
//    pure call does not read or write any aliased (e.g. heap) memory or
//    have other global side effects (e.g. class constructors, finalizers),
//    but is allowed to throw an exception.
//
//    NOTE: this call currently only returns true if the call target is a
//    helper method that is known to be pure. No other analysis is
//    performed.
//
// Arguments:
//    Copiler - the compiler context.
//
// Returns:
//    True if the call is pure; false otherwise.
//
bool GenTreeCall::IsPure(Compiler* compiler) const
{
    return (gtCallType == CT_HELPER) &&
           compiler->s_helperCallProperties.IsPure(compiler->eeGetHelperNum(gtCallMethHnd));
}

//-------------------------------------------------------------------------
// HasSideEffects:
//    Returns true if this call has any side effects. All non-helpers are considered to have side-effects. Only helpers
//    that do not mutate the heap, do not run constructors, may not throw, and are either a) pure or b) non-finalizing
//    allocation functions are considered side-effect-free.
//
// Arguments:
//     compiler         - the compiler instance
//     ignoreExceptions - when `true`, ignores exception side effects
//     ignoreCctors     - when `true`, ignores class constructor side effects
//
// Return Value:
//      true if this call has any side-effects; false otherwise.
bool GenTreeCall::HasSideEffects(Compiler* compiler, bool ignoreExceptions, bool ignoreCctors) const
{
    // Generally all GT_CALL nodes are considered to have side-effects, but we may have extra information about helper
    // calls that can prove them side-effect-free.
    if (gtCallType != CT_HELPER)
    {
        return true;
    }

    CorInfoHelpFunc             helper           = compiler->eeGetHelperNum(gtCallMethHnd);
    const HelperCallProperties& helperProperties = Compiler::s_helperCallProperties;

    // We definitely care about the side effects if MutatesHeap is true
    if (helperProperties.MutatesHeap(helper))
    {
        return true;
    }

    // Unless we have been instructed to ignore cctors (CSE, for example, ignores cctors), consider them side effects.
    if (!ignoreCctors && helperProperties.MayRunCctor(helper))
    {
        return true;
    }

    // If we also care about exceptions then check if the helper can throw
    if (!ignoreExceptions && !helperProperties.NoThrow(helper))
    {
        return true;
    }

    // If this is not a Pure helper call or an allocator (that will not need to run a finalizer)
    // then this call has side effects.
    return !helperProperties.IsPure(helper) &&
           (!helperProperties.IsAllocator(helper) || ((gtCallMoreFlags & GTF_CALL_M_ALLOC_SIDE_EFFECTS) != 0));
}

//-------------------------------------------------------------------------
// HasNonStandardAddedArgs: Return true if the method has non-standard args added to the call
// argument list during argument morphing (fgMorphArgs), e.g., passed in R10 or R11 on AMD64.
// See also GetNonStandardAddedArgCount().
//
// Arguments:
//     compiler - the compiler instance
//
// Return Value:
//      true if there are any such args, false otherwise.
//
bool GenTreeCall::HasNonStandardAddedArgs(Compiler* compiler) const
{
    return GetNonStandardAddedArgCount(compiler) != 0;
}

//-------------------------------------------------------------------------
// GetNonStandardAddedArgCount: Get the count of non-standard arguments that have been added
// during call argument morphing (fgMorphArgs). Do not count non-standard args that are already
// counted in the argument list prior to morphing.
//
// This function is used to help map the caller and callee arguments during tail call setup.
//
// Arguments:
//     compiler - the compiler instance
//
// Return Value:
//      The count of args, as described.
//
// Notes:
//      It would be more general to have fgMorphArgs set a bit on the call node when such
//      args are added to a call, and a bit on each such arg, and then have this code loop
//      over the call args when the special call bit is set, counting the args with the special
//      arg bit. This seems pretty heavyweight, though. Instead, this logic needs to be kept
//      in sync with fgMorphArgs.
//
int GenTreeCall::GetNonStandardAddedArgCount(Compiler* compiler) const
{
    if (IsUnmanaged() && !compiler->opts.ShouldUsePInvokeHelpers())
    {
        // R11 = PInvoke cookie param
        return 1;
    }
    else if (IsVirtualStub())
    {
        // R11 = Virtual stub param
        return 1;
    }
    else if ((gtCallType == CT_INDIRECT) && (gtCallCookie != nullptr))
    {
        // R10 = PInvoke target param
        // R11 = PInvoke cookie param
        return 2;
    }
    return 0;
}

//-------------------------------------------------------------------------
// TreatAsHasRetBufArg:
//
// Return Value:
//     Returns true if we treat the call as if it has a retBuf argument
//     This method may actually have a retBuf argument
//     or it could be a JIT helper that we are still transforming during
//     the importer phase.
//
// Notes:
//     On ARM64 marking the method with the GTF_CALL_M_RETBUFFARG flag
//     will make HasRetBufArg() return true, but will also force the
//     use of register x8 to pass the RetBuf argument.
//
//     These Jit Helpers that we handle here by returning true aren't
//     actually defined to return a struct, so they don't expect their
//     RetBuf to be passed in x8, instead they  expect it in x0.
//
bool GenTreeCall::TreatAsHasRetBufArg() const
{
    if (HasRetBufArg())
    {
        return true;
    }

    if (!TypeIs(TYP_STRUCT) || !IsHelperCall())
    {
        return false;
    }

    switch (Compiler::eeGetHelperNum(gtCallMethHnd))
    {
        case CORINFO_HELP_UNBOX_NULLABLE:
            return true;
        case CORINFO_HELP_METHODDESC_TO_STUBRUNTIMEMETHOD:
        case CORINFO_HELP_FIELDDESC_TO_STUBRUNTIMEFIELD:
        case CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE:
        case CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE_MAYBENULL:
            return false;
        default:
            assert(!"Unexpected JIT helper in TreatAsHasRetBufArg");
            return false;
    }
}

//-------------------------------------------------------------------------
// IsHelperCall: Determine if this GT_CALL node is a specific helper call.
//
// Arguments:
//     compiler - the compiler instance so that we can call eeFindHelper
//
// Return Value:
//     Returns true if this GT_CALL node is a call to the specified helper.
//
bool GenTreeCall::IsHelperCall(Compiler* compiler, unsigned helper) const
{
    return IsHelperCall(compiler->eeFindHelper(helper));
}

//-------------------------------------------------------------------------
// AreArgsComplete: Determine if this GT_CALL node's arguments have been processed.
//
// Return Value:
//     Returns true if fgMorphArgs has processed the arguments.
//
bool GenTreeCall::AreArgsComplete() const
{
    if (fgArgInfo == nullptr)
    {
        return false;
    }
    if (fgArgInfo->AreArgsComplete())
    {
        assert((gtCallLateArgs != nullptr) || !fgArgInfo->HasRegArgs());
        return true;
    }

#if defined(FEATURE_FASTTAILCALL)
// If we have FEATURE_FASTTAILCALL, 'fgCanFastTailCall()' can call 'fgInitArgInfo()', and in that
// scenario it is valid to have 'fgArgInfo' be non-null when 'fgMorphArgs()' first queries this,
// when it hasn't yet morphed the arguments.
#else
    assert(gtCallArgs == nullptr);
#endif

    return false;
}

//--------------------------------------------------------------------------
// Equals: Check if 2 CALL nodes are equal.
//
// Arguments:
//    c1 - The first call node
//    c2 - The second call node
//
// Return Value:
//    true if the 2 CALL nodes have the same type and operands
//
bool GenTreeCall::Equals(GenTreeCall* c1, GenTreeCall* c2)
{
    assert(c1->OperGet() == c2->OperGet());

    if (c1->TypeGet() != c2->TypeGet())
    {
        return false;
    }

    if (c1->gtCallType != c2->gtCallType)
    {
        return false;
    }

    if (c1->gtCallType != CT_INDIRECT)
    {
        if (c1->gtCallMethHnd != c2->gtCallMethHnd)
        {
            return false;
        }

#ifdef FEATURE_READYTORUN_COMPILER
        if (c1->gtEntryPoint.addr != c2->gtEntryPoint.addr)
        {
            return false;
        }
#endif
    }
    else
    {
        if (!Compare(c1->gtCallAddr, c2->gtCallAddr))
        {
            return false;
        }
    }

    if ((c1->gtCallThisArg != nullptr) != (c2->gtCallThisArg != nullptr))
    {
        return false;
    }

    if ((c1->gtCallThisArg != nullptr) && !Compare(c1->gtCallThisArg->GetNode(), c2->gtCallThisArg->GetNode()))
    {
        return false;
    }

    GenTreeCall::UseIterator i1   = c1->Args().begin();
    GenTreeCall::UseIterator end1 = c1->Args().end();
    GenTreeCall::UseIterator i2   = c2->Args().begin();
    GenTreeCall::UseIterator end2 = c2->Args().end();

    for (; (i1 != end1) && (i2 != end2); ++i1, ++i2)
    {
        if (!Compare(i1->GetNode(), i2->GetNode()))
        {
            return false;
        }
    }

    if ((i1 != end1) || (i2 != end2))
    {
        return false;
    }

    i1   = c1->LateArgs().begin();
    end1 = c1->LateArgs().end();
    i2   = c2->LateArgs().begin();
    end2 = c2->LateArgs().end();

    for (; (i1 != end1) && (i2 != end2); ++i1, ++i2)
    {
        if (!Compare(i1->GetNode(), i2->GetNode()))
        {
            return false;
        }
    }

    if ((i1 != end1) || (i2 != end2))
    {
        return false;
    }

    if (!Compare(c1->gtControlExpr, c2->gtControlExpr))
    {
        return false;
    }

    return true;
}

//--------------------------------------------------------------------------
// ResetArgInfo: The argument info needs to be reset so it can be recomputed based on some change
// in conditions, such as changing the return type of a call due to giving up on doing a tailcall.
// If there is no fgArgInfo computed yet for this call, then there is nothing to reset.
//
void GenTreeCall::ResetArgInfo()
{
    if (fgArgInfo == nullptr)
    {
        return;
    }

    // We would like to just set `fgArgInfo = nullptr`. But `fgInitArgInfo()` not
    // only sets up fgArgInfo, it also adds non-standard args to the IR, and we need
    // to remove that extra IR so it doesn't get added again.
    //
    // NOTE: this doesn't handle all possible cases. There might be cases where we
    // should be removing non-standard arg IR but currently aren't.
    CLANG_FORMAT_COMMENT_ANCHOR;

#if !defined(TARGET_X86)
    if (IsVirtualStub())
    {
        JITDUMP("Removing VSD non-standard arg [%06u] to prepare for re-morphing call [%06u]\n",
                Compiler::dspTreeID(gtCallArgs->GetNode()), gtTreeID);
        gtCallArgs = gtCallArgs->GetNext();
    }
#endif // !defined(TARGET_X86)

    fgArgInfo = nullptr;
}

/*****************************************************************************
 *
 *  Returns non-zero if the two trees are identical.
 */

bool GenTree::Compare(GenTree* op1, GenTree* op2, bool swapOK)
{
    genTreeOps oper;
    unsigned   kind;

//  printf("tree1:\n"); gtDispTree(op1);
//  printf("tree2:\n"); gtDispTree(op2);

AGAIN:

    if (op1 == nullptr)
    {
        return (op2 == nullptr);
    }
    if (op2 == nullptr)
    {
        return false;
    }
    if (op1 == op2)
    {
        return true;
    }

    oper = op1->OperGet();

    /* The operators must be equal */

    if (oper != op2->gtOper)
    {
        return false;
    }

    /* The types must be equal */

    if (op1->gtType != op2->gtType)
    {
        return false;
    }

    /* Overflow must be equal */
    if (op1->gtOverflowEx() != op2->gtOverflowEx())
    {
        return false;
    }

    /* Sensible flags must be equal */
    if ((op1->gtFlags & (GTF_UNSIGNED)) != (op2->gtFlags & (GTF_UNSIGNED)))
    {
        return false;
    }

    /* Figure out what kind of nodes we're comparing */

    kind = op1->OperKind();

    /* Is this a constant node? */

    if (op1->OperIsConst())
    {
        switch (oper)
        {
            case GT_CNS_INT:
                if (op1->AsIntCon()->gtIconVal == op2->AsIntCon()->gtIconVal)
                {
                    return true;
                }
                break;

            case GT_CNS_STR:
                if ((op1->AsStrCon()->gtSconCPX == op2->AsStrCon()->gtSconCPX) &&
                    (op1->AsStrCon()->gtScpHnd == op2->AsStrCon()->gtScpHnd))
                {
                    return true;
                }
                break;

#if 0
            // TODO-CQ: Enable this in the future
        case GT_CNS_LNG:
            if  (op1->AsLngCon()->gtLconVal == op2->AsLngCon()->gtLconVal)
                return true;
            break;

        case GT_CNS_DBL:
            if  (op1->AsDblCon()->gtDconVal == op2->AsDblCon()->gtDconVal)
                return true;
            break;
#endif
            default:
                break;
        }

        return false;
    }

    /* Is this a leaf node? */

    if (kind & GTK_LEAF)
    {
        switch (oper)
        {
            case GT_LCL_VAR:
            case GT_LCL_VAR_ADDR:
                if (op1->AsLclVarCommon()->GetLclNum() != op2->AsLclVarCommon()->GetLclNum())
                {
                    break;
                }

                return true;

            case GT_LCL_FLD:
            case GT_LCL_FLD_ADDR:
                return GenTreeLclFld::Equals(op1->AsLclFld(), op2->AsLclFld());

            case GT_CLS_VAR_ADDR:
                return op1->AsClsVar()->GetFieldHandle() == op2->AsClsVar()->GetFieldHandle();

            case GT_LABEL:
            case GT_ARGPLACE:
                return true;

            default:
                break;
        }

        return false;
    }

    /* Is it a 'simple' unary/binary operator? */

    if (kind & GTK_UNOP)
    {
        if (IsExOp(kind))
        {
            // ExOp operators extend unary operator with extra, non-GenTree* members.  In many cases,
            // these should be included in the comparison.
            switch (oper)
            {
                case GT_ARR_LENGTH:
                    break;
                case GT_CAST:
                    if (op1->AsCast()->gtCastType != op2->AsCast()->gtCastType)
                    {
                        return false;
                    }
                    break;
                case GT_BLK:
                case GT_OBJ:
                    if (op1->AsBlk()->GetLayout() != op2->AsBlk()->GetLayout())
                    {
                        return false;
                    }
                    break;
                case GT_FIELD_ADDR:
                    // TODO-MIKE-Review: It's not clear if the field sequence should be checked.
                    // Old code did check the field handle but given the current (very few) uses
                    // of GenTree::Compare it seems that checking only the offset should suffice.
                    if ((op1->AsFieldAddr()->GetOffset() != op2->AsFieldAddr()->GetOffset()) ||
                        (op1->AsFieldAddr()->GetFieldSeq() != op2->AsFieldAddr()->GetFieldSeq()))
                    {
                        return false;
                    }
                    break;

                // For the ones below no extra argument matters for comparison.
                case GT_BOX:
                case GT_RUNTIMELOOKUP:
                    break;

                default:
                    assert(!"unexpected unary ExOp operator");
            }
        }

        return Compare(op1->AsUnOp()->gtOp1, op2->AsUnOp()->gtOp1);
    }

    if (kind & GTK_BINOP)
    {
        if (IsExOp(kind))
        {
            // ExOp operators extend unary operator with extra, non-GenTree* members.  In many cases,
            // these should be included in the hash code.
            switch (oper)
            {
                case GT_INTRINSIC:
                    if (op1->AsIntrinsic()->gtIntrinsicId != op2->AsIntrinsic()->gtIntrinsicId)
                    {
                        return false;
                    }
                    break;
                case GT_LEA:
                    if (op1->AsAddrMode()->GetScale() != op2->AsAddrMode()->GetScale())
                    {
                        return false;
                    }
                    if (op1->AsAddrMode()->GetOffset() != op2->AsAddrMode()->GetOffset())
                    {
                        return false;
                    }
                    break;
                case GT_INDEX_ADDR:
                    if (op1->AsIndexAddr()->GetElemSize() != op2->AsIndexAddr()->GetElemSize())
                    {
                        return false;
                    }
                    break;

                // For the ones below no extra argument matters for comparison.
                case GT_QMARK:
                    break;

                default:
                    assert(!"unexpected binary ExOp operator");
            }
        }

        if (op1->AsOp()->gtOp2 != nullptr)
        {
            if (!Compare(op1->AsOp()->gtOp1, op2->AsOp()->gtOp1, swapOK))
            {
                if (swapOK && OperIsCommutative(oper) &&
                    ((op1->AsOp()->gtOp1->gtFlags | op1->AsOp()->gtOp2->gtFlags | op2->AsOp()->gtOp1->gtFlags |
                      op2->AsOp()->gtOp2->gtFlags) &
                     GTF_ALL_EFFECT) == 0)
                {
                    if (Compare(op1->AsOp()->gtOp1, op2->AsOp()->gtOp2, swapOK))
                    {
                        op1 = op1->AsOp()->gtOp2;
                        op2 = op2->AsOp()->gtOp1;
                        goto AGAIN;
                    }
                }

                return false;
            }

            op1 = op1->AsOp()->gtOp2;
            op2 = op2->AsOp()->gtOp2;

            goto AGAIN;
        }
        else
        {

            op1 = op1->AsOp()->gtOp1;
            op2 = op2->AsOp()->gtOp1;

            if (!op1)
            {
                return (op2 == nullptr);
            }
            if (!op2)
            {
                return false;
            }

            goto AGAIN;
        }
    }

    /* See what kind of a special operator we have here */

    switch (oper)
    {
        case GT_CALL:
            return GenTreeCall::Equals(op1->AsCall(), op2->AsCall());

        case GT_ARR_ELEM:

            if (op1->AsArrElem()->gtArrRank != op2->AsArrElem()->gtArrRank)
            {
                return false;
            }

            // NOTE: gtArrElemSize may need to be handled

            unsigned dim;
            for (dim = 0; dim < op1->AsArrElem()->gtArrRank; dim++)
            {
                if (!Compare(op1->AsArrElem()->gtArrInds[dim], op2->AsArrElem()->gtArrInds[dim]))
                {
                    return false;
                }
            }

            op1 = op1->AsArrElem()->gtArrObj;
            op2 = op2->AsArrElem()->gtArrObj;
            goto AGAIN;

        case GT_PHI:
            return GenTreePhi::Equals(op1->AsPhi(), op2->AsPhi());

        case GT_FIELD_LIST:
            return GenTreeFieldList::Equals(op1->AsFieldList(), op2->AsFieldList());

#ifdef FEATURE_SIMD
        case GT_HWINTRINSIC:
            return GenTreeHWIntrinsic::Equals(op1->AsHWIntrinsic(), op2->AsHWIntrinsic());
#endif

        case GT_INSTR:
            return GenTreeInstr::Equals(op1->AsInstr(), op2->AsInstr());

        case GT_ARR_OFFSET:
            if ((op1->AsArrOffs()->gtCurrDim != op2->AsArrOffs()->gtCurrDim) ||
                (op1->AsArrOffs()->gtArrRank != op2->AsArrOffs()->gtArrRank))
            {
                return false;
            }
            FALLTHROUGH;
        case GT_CMPXCHG:
        case GT_COPY_BLK:
        case GT_INIT_BLK:
            return GenTreeTernaryOp::Equals(op1->AsTernaryOp(), op2->AsTernaryOp());

        case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
        case GT_HW_INTRINSIC_CHK:
#endif
            return GenTreeBoundsChk::Equals(op1->AsBoundsChk(), op2->AsBoundsChk());

        default:
            assert(!"unexpected operator");
    }

    return false;
}

#ifdef DEBUG

/*****************************************************************************
 *
 *  Helper used to compute hash values for trees.
 */

inline unsigned genTreeHashAdd(unsigned old, unsigned add)
{
    return (old + old / 2) ^ add;
}

inline unsigned genTreeHashAdd(unsigned old, void* add)
{
    return genTreeHashAdd(old, (unsigned)(size_t)add);
}

/*****************************************************************************
 *
 *  Given an arbitrary expression tree, compute a hash value for it.
 */

unsigned Compiler::gtHashValue(GenTree* tree)
{
    genTreeOps oper;
    unsigned   kind;

    unsigned hash = 0;

    GenTree* temp;

AGAIN:
    assert(tree);

    /* Figure out what kind of a node we have */

    oper = tree->OperGet();
    kind = tree->OperKind();

    /* Include the operator value in the hash */

    hash = genTreeHashAdd(hash, oper);

    /* Is this a constant or leaf node? */

    if (kind & GTK_LEAF)
    {
        size_t add;

        switch (oper)
        {
            UINT64 bits;
            case GT_LCL_VAR:
                add = tree->AsLclVar()->GetLclNum();
                break;
            case GT_LCL_FLD:
                hash = genTreeHashAdd(hash, tree->AsLclFld()->GetLclNum());
                hash = genTreeHashAdd(hash, tree->AsLclFld()->GetLayoutNum());
                add  = tree->AsLclFld()->GetLclOffs();
                break;

            case GT_CNS_INT:
                add = tree->AsIntCon()->gtIconVal;
                break;
            case GT_CNS_LNG:
                bits = (UINT64)tree->AsLngCon()->gtLconVal;
#ifdef HOST_64BIT
                add = bits;
#else // 32-bit host
                add = genTreeHashAdd(uhi32(bits), ulo32(bits));
#endif
                break;
            case GT_CNS_DBL:
                bits = *(UINT64*)(&tree->AsDblCon()->gtDconVal);
#ifdef HOST_64BIT
                add = bits;
#else // 32-bit host
                add = genTreeHashAdd(uhi32(bits), ulo32(bits));
#endif
                break;
            case GT_CNS_STR:
                add = tree->AsStrCon()->gtSconCPX;
                break;

            case GT_JMP:
                add = tree->AsVal()->gtVal1;
                break;

            default:
                add = 0;
                break;
        }

        // clang-format off
        // narrow 'add' into a 32-bit 'val'
        unsigned val;
#ifdef HOST_64BIT
        val = genTreeHashAdd(uhi32(add), ulo32(add));
#else // 32-bit host
        val = add;
#endif
        // clang-format on

        hash = genTreeHashAdd(hash, val);
        goto DONE;
    }

    /* Is it a 'simple' unary/binary operator? */

    GenTree* op1;

    if (kind & GTK_UNOP)
    {
        op1 = tree->AsOp()->gtOp1;
        /* Special case: no sub-operand at all */

        if (GenTree::IsExOp(kind))
        {
            // ExOp operators extend operators with extra, non-GenTree* members.  In many cases,
            // these should be included in the hash code.
            switch (oper)
            {
                case GT_ARR_LENGTH:
                    break;
                case GT_CAST:
                    hash ^= tree->AsCast()->gtCastType;
                    break;
                case GT_INDEX_ADDR:
                    hash += tree->AsIndexAddr()->GetElemSize();
                    break;
                case GT_ALLOCOBJ:
                    hash = genTreeHashAdd(hash, static_cast<unsigned>(
                                                    reinterpret_cast<uintptr_t>(tree->AsAllocObj()->gtAllocObjClsHnd)));
                    hash = genTreeHashAdd(hash, tree->AsAllocObj()->gtNewHelper);
                    break;
                case GT_RUNTIMELOOKUP:
                    hash = genTreeHashAdd(hash, static_cast<unsigned>(
                                                    reinterpret_cast<uintptr_t>(tree->AsRuntimeLookup()->gtHnd)));
                    break;
                case GT_BLK:
                case GT_OBJ:
                    hash =
                        genTreeHashAdd(hash,
                                       static_cast<unsigned>(reinterpret_cast<uintptr_t>(tree->AsBlk()->GetLayout())));
                    break;
                case GT_FIELD_ADDR:
                    hash = genTreeHashAdd(hash, gtHashValue(tree->AsFieldAddr()->GetAddr()));
                    // TODO-MIKE-Review: Shouldn't the field handle be included in the hash?
                    break;

                // For the ones below no extra argument matters for comparison.
                case GT_BOX:
                    break;

                default:
                    assert(!"unexpected unary ExOp operator");
            }
        }

        if (!op1)
        {
            goto DONE;
        }

        tree = op1;
        goto AGAIN;
    }

    if (kind & GTK_BINOP)
    {
        if (GenTree::IsExOp(kind))
        {
            // ExOp operators extend operators with extra, non-GenTree* members.  In many cases,
            // these should be included in the hash code.
            switch (oper)
            {
                case GT_INTRINSIC:
                    hash += tree->AsIntrinsic()->gtIntrinsicId;
                    break;
                case GT_LEA:
                    hash +=
                        static_cast<unsigned>(tree->AsAddrMode()->GetOffset() << 3) + tree->AsAddrMode()->GetScale();
                    break;

                case GT_STORE_BLK:
                case GT_STORE_OBJ:
                    hash ^= PtrToUlong(tree->AsBlk()->GetLayout());
                    break;

                // For the ones below no extra argument matters for comparison.
                case GT_ARR_INDEX:
                case GT_INDEX_ADDR:
                    break;

                default:
                    assert(!"unexpected binary ExOp operator");
            }
        }

        op1          = tree->AsOp()->gtOp1;
        GenTree* op2 = tree->AsOp()->gtOp2;

        /* Is there a second sub-operand? */

        if (!op2)
        {
            /* Special case: no sub-operands at all */

            if (!op1)
            {
                goto DONE;
            }

            /* This is a unary operator */

            tree = op1;
            goto AGAIN;
        }

        /* This is a binary operator */

        unsigned hsh1 = gtHashValue(op1);

        /* Add op1's hash to the running value and continue with op2 */

        hash = genTreeHashAdd(hash, hsh1);

        tree = op2;
        goto AGAIN;
    }

    /* See what kind of a special operator we have here */
    switch (tree->gtOper)
    {
        case GT_ARR_ELEM:

            hash = genTreeHashAdd(hash, gtHashValue(tree->AsArrElem()->gtArrObj));

            unsigned dim;
            for (dim = 0; dim < tree->AsArrElem()->gtArrRank; dim++)
            {
                hash = genTreeHashAdd(hash, gtHashValue(tree->AsArrElem()->gtArrInds[dim]));
            }

            break;

        case GT_CALL:
            if ((tree->AsCall()->gtCallThisArg != nullptr) && !tree->AsCall()->gtCallThisArg->GetNode()->OperIs(GT_NOP))
            {
                hash = genTreeHashAdd(hash, gtHashValue(tree->AsCall()->gtCallThisArg->GetNode()));
            }

            for (GenTreeCall::Use& use : tree->AsCall()->Args())
            {
                hash = genTreeHashAdd(hash, gtHashValue(use.GetNode()));
            }

            if (tree->AsCall()->gtCallType == CT_INDIRECT)
            {
                temp = tree->AsCall()->gtCallAddr;
                assert(temp);
                hash = genTreeHashAdd(hash, gtHashValue(temp));
            }
            else
            {
                hash = genTreeHashAdd(hash, tree->AsCall()->gtCallMethHnd);
            }

            for (GenTreeCall::Use& use : tree->AsCall()->LateArgs())
            {
                hash = genTreeHashAdd(hash, gtHashValue(use.GetNode()));
            }
            break;

        case GT_PHI:
            for (GenTreePhi::Use& use : tree->AsPhi()->Uses())
            {
                hash = genTreeHashAdd(hash, gtHashValue(use.GetNode()));
            }
            break;

        case GT_FIELD_LIST:
            for (GenTreeFieldList::Use& use : tree->AsFieldList()->Uses())
            {
                hash = genTreeHashAdd(hash, gtHashValue(use.GetNode()));
            }
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            hash += tree->AsHWIntrinsic()->GetIntrinsic();
            hash += tree->AsHWIntrinsic()->GetSimdBaseType();
            hash += tree->AsHWIntrinsic()->GetSimdSize();
            hash += tree->AsHWIntrinsic()->GetAuxiliaryType();
            for (GenTreeHWIntrinsic::Use& use : tree->AsHWIntrinsic()->Uses())
            {
                hash = genTreeHashAdd(hash, gtHashValue(use.GetNode()));
            }
            break;
#endif // FEATURE_HW_INTRINSICS

        case GT_INSTR:
            hash = genTreeHashAdd(hash, tree->AsInstr()->GetIns());
            hash = genTreeHashAdd(hash, tree->AsInstr()->GetSize());
#ifdef TARGET_ARMARCH
            hash = genTreeHashAdd(hash, tree->AsInstr()->GetOption());
#endif
            hash = genTreeHashAdd(hash, tree->AsInstr()->GetImmediate());
            for (GenTreeInstr::Use& use : tree->AsInstr()->Uses())
            {
                hash = genTreeHashAdd(hash, gtHashValue(use.GetNode()));
            }
            break;

        case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
        case GT_HW_INTRINSIC_CHK:
#endif
            hash = genTreeHashAdd(hash, gtHashValue(tree->AsBoundsChk()->GetIndex()));
            hash = genTreeHashAdd(hash, gtHashValue(tree->AsBoundsChk()->GetLength()));
            hash = genTreeHashAdd(hash, tree->AsBoundsChk()->GetThrowKind());
            break;

        case GT_ARR_OFFSET:
        case GT_CMPXCHG:
        case GT_COPY_BLK:
        case GT_INIT_BLK:
        case GT_QMARK:
            // TODO-MIKE-Review: The hash does not include non operand data from ARR_OFFSET
            for (unsigned i = 0; i < 3; i++)
            {
                hash = genTreeHashAdd(hash, gtHashValue(tree->AsTernaryOp()->GetOp(i)));
            }
            break;

        default:
            INDEBUG(gtDispTree(tree);)
            assert(!"unexpected operator");
            break;
    }

DONE:

    return hash;
}

#endif // DEBUG

/*****************************************************************************
 *
 *  Return a relational operator that is the reverse of the given one.
 */

/* static */
genTreeOps GenTree::ReverseRelop(genTreeOps relop)
{
    static const genTreeOps reverseOps[] = {
        GT_NE,      // GT_EQ
        GT_EQ,      // GT_NE
        GT_GE,      // GT_LT
        GT_GT,      // GT_LE
        GT_LT,      // GT_GE
        GT_LE,      // GT_GT
        GT_TEST_NE, // GT_TEST_EQ
        GT_TEST_EQ, // GT_TEST_NE
    };

    assert(reverseOps[GT_EQ - GT_EQ] == GT_NE);
    assert(reverseOps[GT_NE - GT_EQ] == GT_EQ);

    assert(reverseOps[GT_LT - GT_EQ] == GT_GE);
    assert(reverseOps[GT_LE - GT_EQ] == GT_GT);
    assert(reverseOps[GT_GE - GT_EQ] == GT_LT);
    assert(reverseOps[GT_GT - GT_EQ] == GT_LE);

    assert(reverseOps[GT_TEST_EQ - GT_EQ] == GT_TEST_NE);
    assert(reverseOps[GT_TEST_NE - GT_EQ] == GT_TEST_EQ);

    assert(OperIsCompare(relop));
    assert(relop >= GT_EQ && (unsigned)(relop - GT_EQ) < sizeof(reverseOps));

    return reverseOps[relop - GT_EQ];
}

/*****************************************************************************
 *
 *  Return a relational operator that will work for swapped operands.
 */

/* static */
genTreeOps GenTree::SwapRelop(genTreeOps relop)
{
    static const genTreeOps swapOps[] = {
        GT_EQ,      // GT_EQ
        GT_NE,      // GT_NE
        GT_GT,      // GT_LT
        GT_GE,      // GT_LE
        GT_LE,      // GT_GE
        GT_LT,      // GT_GT
        GT_TEST_EQ, // GT_TEST_EQ
        GT_TEST_NE, // GT_TEST_NE
    };

    assert(swapOps[GT_EQ - GT_EQ] == GT_EQ);
    assert(swapOps[GT_NE - GT_EQ] == GT_NE);

    assert(swapOps[GT_LT - GT_EQ] == GT_GT);
    assert(swapOps[GT_LE - GT_EQ] == GT_GE);
    assert(swapOps[GT_GE - GT_EQ] == GT_LE);
    assert(swapOps[GT_GT - GT_EQ] == GT_LT);

    assert(swapOps[GT_TEST_EQ - GT_EQ] == GT_TEST_EQ);
    assert(swapOps[GT_TEST_NE - GT_EQ] == GT_TEST_NE);

    assert(OperIsCompare(relop));
    assert(relop >= GT_EQ && (unsigned)(relop - GT_EQ) < sizeof(swapOps));

    return swapOps[relop - GT_EQ];
}

/*****************************************************************************
 *
 *  Reverse the meaning of the given test condition.
 */

GenTree* Compiler::gtReverseCond(GenTree* tree)
{
    if (tree->OperIsCompare())
    {
        tree->SetOper(GenTree::ReverseRelop(tree->OperGet()));

        // Flip the GTF_RELOP_NAN_UN bit
        //     a ord b   === (a != NaN && b != NaN)
        //     a unord b === (a == NaN || b == NaN)
        // => !(a ord b) === (a unord b)
        if (varTypeIsFloating(tree->AsOp()->gtOp1->TypeGet()))
        {
            tree->gtFlags ^= GTF_RELOP_NAN_UN;
        }
    }
    else if (tree->OperIs(GT_JCC, GT_SETCC))
    {
        GenTreeCC* cc   = tree->AsCC();
        cc->gtCondition = GenCondition::Reverse(cc->gtCondition);
    }
    else if (tree->OperIs(GT_JCMP))
    {
        // Flip the GTF_JCMP_EQ
        //
        // This causes switching
        //     cbz <=> cbnz
        //     tbz <=> tbnz
        tree->gtFlags ^= GTF_JCMP_EQ;
    }
    else
    {
        tree = gtNewOperNode(GT_NOT, TYP_INT, tree);
    }

    return tree;
}

unsigned Compiler::gtSetCallArgsOrder(const GenTreeCall::UseList& args, bool lateArgs, int* callCostEx, int* callCostSz)
{
    unsigned level  = 0;
    unsigned costEx = 0;
    unsigned costSz = 0;

    for (GenTreeCall::Use& use : args)
    {
        GenTree* argNode  = use.GetNode();
        unsigned argLevel = gtSetEvalOrder(argNode);

        if (argLevel > level)
        {
            level = argLevel;
        }

        if (argNode->GetCostEx() != 0)
        {
            costEx += argNode->GetCostEx();
            costEx += lateArgs ? 0 : IND_COST_EX;
        }

        if (argNode->GetCostSz() != 0)
        {
            costSz += argNode->GetCostSz();
#ifdef TARGET_XARCH
            if (lateArgs) // push is smaller than mov to reg
#endif
            {
                costSz += 1;
            }
        }
    }

    *callCostEx += costEx;
    *callCostSz += costSz;

    return level;
}

void Compiler::gtPrepareCost(GenTree* tree)
{
    gtSetEvalOrder(tree);
}

bool Compiler::gtIsLikelyRegVar(GenTree* tree)
{
    if (tree->gtOper != GT_LCL_VAR)
    {
        return false;
    }

    LclVarDsc* varDsc = lvaGetDesc(tree->AsLclVar());

    if (varDsc->lvDoNotEnregister)
    {
        return false;
    }

    // If this is an EH-live var, return false if it is a def,
    // as it will have to go to memory.
    if (varDsc->lvLiveInOutOfHndlr && ((tree->gtFlags & GTF_VAR_DEF) != 0))
    {
        return false;
    }

    // Be pessimistic if ref counts are not yet set up.
    //
    // Perhaps we should be optimistic though.
    // See notes in GitHub issue 18969.
    if (!lvaLocalVarRefCounted())
    {
        return false;
    }

    if (varDsc->lvRefCntWtd() < (BB_UNITY_WEIGHT * 3))
    {
        return false;
    }

#ifdef TARGET_X86
    if (varTypeUsesFloatReg(tree->TypeGet()))
        return false;
    if (varTypeIsLong(tree->TypeGet()))
        return false;
#endif

    return true;
}

//------------------------------------------------------------------------
// gtCanSwapOrder: Returns true iff the secondNode can be swapped with firstNode.
//
// Arguments:
//    firstNode  - An operand of a tree that can have GTF_REVERSE_OPS set.
//    secondNode - The other operand of the tree.
//
// Return Value:
//    Returns a boolean indicating whether it is safe to reverse the execution
//    order of the two trees, considering any exception, global effects, or
//    ordering constraints.
//
bool Compiler::gtCanSwapOrder(GenTree* firstNode, GenTree* secondNode)
{
    // Relative of order of global / side effects can't be swapped.

    bool canSwap = true;

    if (optValnumCSE_phase)
    {
        canSwap = optCSE_canSwap(firstNode, secondNode);
    }

    // We cannot swap in the presence of special side effects such as GT_CATCH_ARG.

    if (canSwap && (firstNode->gtFlags & GTF_ORDER_SIDEEFF))
    {
        canSwap = false;
    }

    // When strict side effect order is disabled we allow GTF_REVERSE_OPS to be set
    // when one or both sides contains a GTF_CALL or GTF_EXCEPT.
    // Currently only the C and C++ languages allow non strict side effect order.

    unsigned strictEffects = GTF_GLOB_EFFECT;

    if (canSwap && (firstNode->gtFlags & strictEffects))
    {
        // op1 has side efects that can't be reordered.
        // Check for some special cases where we still may be able to swap.

        if (secondNode->gtFlags & strictEffects)
        {
            // op2 has also has non reorderable side effects - can't swap.
            canSwap = false;
        }
        else
        {
            // No side effects in op2 - we can swap iff op1 has no way of modifying op2,
            // i.e. through byref assignments or calls or op2 is a constant.

            if (firstNode->gtFlags & strictEffects & GTF_PERSISTENT_SIDE_EFFECTS)
            {
                // We have to be conservative - can swap iff op2 is constant.
                if (!secondNode->OperIsConst())
                {
                    canSwap = false;
                }
            }
        }
    }
    return canSwap;
}

// Given an address expression, compute its costs and addressing mode
// opportunities, and mark addressing mode candidates as GTF_DONT_CSE.
// Returns true if it finds an addressing mode.
//
// TODO-Throughput - Consider actually instantiating these early, to avoid
// having to re-run the algorithm that looks for them (might also improve CQ).
//
bool Compiler::gtMarkAddrMode(GenTree* addr, int* indirCostEx, int* indirCostSz, var_types indirType)
{
    AddrMode am(addr);
    am.Extract(this);

    if (am.HasTooManyNodes())
    {
        return false;
    }

    // We can form a complex addressing mode, so mark each of the interior
    // nodes with GTF_ADDRMODE_NO_CSE and calculate a more accurate cost.

    if (am.base != nullptr)
    {
        *indirCostEx += am.base->GetCostEx();
        *indirCostSz += am.base->GetCostSz();
    }

    if (am.index != nullptr)
    {
        *indirCostEx += am.index->GetCostEx();
        *indirCostSz += am.index->GetCostSz();

#ifdef TARGET_ARMARCH
        assert(am.scale == 1);
#endif
    }

#ifdef TARGET_XARCH
    if (am.offset != 0)
    {
        *indirCostSz += FitsIn<int8_t>(am.offset) ? 1 : 4;
    }
#elif defined(TARGET_ARM)
    // TODO-MIKE-Review: It's not clear what this is trying to do. Removing
    // it improves code size a bit by reducing loop condition cloning.
    if (am.base->OperIs(GT_LCL_VAR))
    {
        *indirCostSz -= 1;
    }

    if (am.offset >= 128) // small offsets fits into a 16-bit instruction
    {
        if (am.offset < 4096) // medium offsets require a 32-bit instruction
        {
            if (!varTypeIsFloating(indirType))
            {
                *indirCostSz += 2;
            }
        }
        else
        {
            *indirCostEx += 2; // Very large offsets require movw/movt instructions
            *indirCostSz += 8;
        }
    }
#elif defined(TARGET_ARM64)
    if ((am.offset != 0) && (am.offset >= static_cast<int32_t>(4096 * varTypeSize(indirType))))
    {
        *indirCostEx += 1;
        *indirCostSz += 4;
    }
#else
#error "Unknown TARGET"
#endif

    for (unsigned i = 0; i < am.nodeCount; i++)
    {
        GenTree* node = am.nodes[i];
        assert(node->OperIs(GT_ADD, GT_LSH, GT_MUL, GT_COMMA, GT_CNS_INT));

        if (!node->OperIs(GT_CNS_INT))
        {
            node->gtFlags |= GTF_ADDRMODE_NO_CSE;
        }
    }

    return true;
}

/*****************************************************************************
 *
 *  Given a tree, figure out the order in which its sub-operands should be
 *  evaluated. If the second operand of a binary operator is more expensive
 *  than the first operand, then try to swap the operand trees. Updates the
 *  GTF_REVERSE_OPS bit if necessary in this case.
 *
 *  Returns the Sethi 'complexity' estimate for this tree (the higher
 *  the number, the higher is the tree's resources requirement).
 *
 *  This function sets:
 *      1. GetCostEx() to the execution complexity estimate
 *      2. GetCostSz() to the code size estimate
 *      3. Sometimes sets GTF_ADDRMODE_NO_CSE on nodes in the tree.
 *      4. DEBUG-only: clears GTF_DEBUG_NODE_MORPHED.
 */

#ifdef _PREFAST_
#pragma warning(push)
#pragma warning(disable : 21000) // Suppress PREFast warning about overly large function
#endif
unsigned Compiler::gtSetEvalOrder(GenTree* tree)
{
    assert(tree);

#ifdef DEBUG
    /* Clear the GTF_DEBUG_NODE_MORPHED flag as well */
    tree->gtDebugFlags &= ~GTF_DEBUG_NODE_MORPHED;
#endif

    /* Is this a FP value? */

    bool isflt = varTypeIsFloating(tree->TypeGet());

    /* Figure out what kind of a node we have */

    const genTreeOps oper = tree->OperGet();
    const unsigned   kind = tree->OperKind();

    /* Assume no fixed registers will be trashed */

    unsigned level;
    int      costEx;
    int      costSz;

#ifdef DEBUG
    costEx = -1;
    costSz = -1;
#endif

    /* Is this a constant or a leaf node? */

    if (kind & GTK_LEAF)
    {
        switch (oper)
        {
            // Note that CNS_STR does not normally reach gtSetEvalOrder, it's morphed to IND/CNS_INT/CALL.
            // Give it some costs, just in case gtSetEvalOrder is called before morphing.
            case GT_CNS_STR:
#if defined(TARGET_ARMARCH)
                costSz = 8;
                costEx = 2;
#elif defined(TARGET_AMD64)
                costSz = 10;
                costEx = 2;
#elif defined(TARGET_X86)
                costSz = 4;
                costEx = 1;
#else
#error Unknown TARGET
#endif
                level = 0;
                break;

#ifdef TARGET_ARM
            case GT_CNS_LNG:
            {
                GenTreeIntConCommon* con = tree->AsIntConCommon();

                INT64 lngVal = con->LngValue();
                INT32 loVal  = (INT32)(lngVal & 0xffffffff);
                INT32 hiVal  = (INT32)(lngVal >> 32);

                if (lngVal == 0)
                {
                    costSz = 1;
                    costEx = 1;
                }
                else
                {
                    // Minimum of one instruction to setup hiVal,
                    // and one instruction to setup loVal
                    costSz = 4 + 4;
                    costEx = 1 + 1;

                    if (!codeGen->validImmForInstr(INS_mov, (target_ssize_t)hiVal) &&
                        !codeGen->validImmForInstr(INS_mvn, (target_ssize_t)hiVal))
                    {
                        // Needs extra instruction: movw/movt
                        costSz += 4;
                        costEx += 1;
                    }

                    if (!codeGen->validImmForInstr(INS_mov, (target_ssize_t)loVal) &&
                        !codeGen->validImmForInstr(INS_mvn, (target_ssize_t)loVal))
                    {
                        // Needs extra instruction: movw/movt
                        costSz += 4;
                        costEx += 1;
                    }
                }
                level = 0;
                break;
            }

            case GT_CNS_INT:
            {
                // If the constant is a handle then it will need to have a relocation
                //  applied to it.
                // Any constant that requires a reloc must use the movw/movt sequence
                //
                GenTreeIntCon* con    = tree->AsIntCon();
                target_ssize_t conVal = (target_ssize_t)con->IconValue();

                if (con->ImmedValNeedsReloc(this))
                {
                    // Requires movw/movt
                    costSz = 8;
                    costEx = 2;
                }
                else if (codeGen->validImmForInstr(INS_add, conVal))
                {
                    // Typically included with parent oper
                    costSz = 2;
                    costEx = 1;
                }
                else if (codeGen->validImmForInstr(INS_mov, conVal) || codeGen->validImmForInstr(INS_mvn, conVal))
                {
                    // Uses mov or mvn
                    costSz = 4;
                    costEx = 1;
                }
                else
                {
                    // Needs movw/movt
                    costSz = 8;
                    costEx = 2;
                }
                level = 0;
                break;
            }

#elif defined TARGET_XARCH
#ifdef TARGET_X86
            case GT_CNS_LNG:
            {
                GenTreeLngCon* con    = tree->AsLngCon();
                int64_t        lngVal = con->LngValue();
                // TODO-MIKE-Review: This truncates to 32 bits only when running on a 32 bit host.
                ssize_t conVal    = (ssize_t)lngVal; // truncate to 32-bits
                bool    fitsInVal = (int64_t)conVal == lngVal;

                if (fitsInVal && GenTreeIntConCommon::FitsInI8(conVal))
                {
                    costSz = 1;
                    costEx = 1;
                }
                else
                {
                    costSz = 4;
                    costEx = 1;
                }

                costSz += fitsInVal ? 1 : 4;
                costEx += 1;
                level = 0;
                break;
            }
#endif // TARGET_X86
            case GT_CNS_INT:
            {
                GenTreeIntCon* con    = tree->AsIntCon();
                ssize_t        conVal = con->GetValue();

                if (con->ImmedValNeedsReloc(this))
                {
                    costSz = 4;
                    costEx = 1;
                }
                else if (GenTreeIntConCommon::FitsInI8(conVal))
                {
                    costSz = 1;
                    costEx = 1;
                }
#ifdef TARGET_AMD64
                else if (!GenTreeIntConCommon::FitsInI32(conVal))
                {
                    costSz = 10;
                    costEx = 2;
                }
#endif // TARGET_AMD64
                else
                {
                    costSz = 4;
                    costEx = 1;
                }

                level = 0;
                break;
            }

#elif defined(TARGET_ARM64)
            case GT_CNS_INT:
            {
                GenTreeIntCon* con  = tree->AsIntCon();
                ssize_t        imm  = con->GetValue();
                emitAttr       size = EA_SIZE(emitActualTypeSize(tree->GetType()));

                if (con->ImmedValNeedsReloc(this))
                {
                    costSz = 8;
                    costEx = 2;
                }
                else if (emitter::emitIns_valid_imm_for_add(imm, size))
                {
                    costSz = 2;
                    costEx = 1;
                }
                else if (emitter::emitIns_valid_imm_for_mov(imm, size))
                {
                    costSz = 4;
                    costEx = 1;
                }
                else
                {
                    // Arm64 allows any arbitrary 16-bit constant to be loaded into a register halfword
                    // There are three forms
                    //    movk which loads into any halfword preserving the remaining halfwords
                    //    movz which loads into any halfword zeroing the remaining halfwords
                    //    movn which loads into any halfword zeroing the remaining halfwords then bitwise inverting
                    //    the register
                    // In some cases it is preferable to use movn, because it has the side effect of filling the
                    // other halfwords
                    // with ones

                    // Determine whether movn or movz will require the fewest instructions to populate the immediate
                    bool preferMovz       = false;
                    bool preferMovn       = false;
                    int  instructionCount = 4;

                    for (int i = (size == EA_8BYTE) ? 48 : 16; i >= 0; i -= 16)
                    {
                        if (!preferMovn && (uint16_t(imm >> i) == 0x0000))
                        {
                            preferMovz = true; // by using a movk to start we can save one instruction
                            instructionCount--;
                        }
                        else if (!preferMovz && (uint16_t(imm >> i) == 0xffff))
                        {
                            preferMovn = true; // by using a movn to start we can save one instruction
                            instructionCount--;
                        }
                    }

                    costEx = instructionCount;
                    costSz = 4 * instructionCount;
                }
                level = 0;
                break;
            }
#else
#error "Unknown TARGET"
#endif

            case GT_CNS_DBL:
            {
                level = 0;
#if defined(TARGET_XARCH)
                /* We use fldz and fld1 to load 0.0 and 1.0, but all other  */
                /* floating point constants are loaded using an indirection */
                if ((*((__int64*)&(tree->AsDblCon()->gtDconVal)) == 0) ||
                    (*((__int64*)&(tree->AsDblCon()->gtDconVal)) == I64(0x3ff0000000000000)))
                {
                    costEx = 1;
                    costSz = 1;
                }
                else
                {
                    costEx = IND_COST_EX;
                    costSz = 4;
                }
#elif defined(TARGET_ARM)
                var_types targetType = tree->TypeGet();
                if (targetType == TYP_FLOAT)
                {
                    costEx = 1 + 2;
                    costSz = 2 + 4;
                }
                else
                {
                    assert(targetType == TYP_DOUBLE);
                    costEx = 1 + 4;
                    costSz = 2 + 8;
                }
#elif defined(TARGET_ARM64)
                if ((*((__int64*)&(tree->AsDblCon()->gtDconVal)) == 0) ||
                    emitter::emitIns_valid_imm_for_fmov(tree->AsDblCon()->gtDconVal))
                {
                    costEx = 1;
                    costSz = 1;
                }
                else
                {
                    costEx = IND_COST_EX;
                    costSz = 4;
                }
#else
#error "Unknown TARGET"
#endif
            }
            break;

            case GT_LCL_VAR:
                level = 1;
                if (gtIsLikelyRegVar(tree))
                {
                    costEx = 1;
                    costSz = 1;
                    /* Sign-extend and zero-extend are more expensive to load */
                    if (lvaTable[tree->AsLclVar()->GetLclNum()].lvNormalizeOnLoad())
                    {
                        costEx += 1;
                        costSz += 1;
                    }
                }
                else
                {
                    costEx = IND_COST_EX;
                    costSz = 2;
                    /* Sign-extend and zero-extend are more expensive to load */
                    if (varTypeIsSmall(tree->TypeGet()))
                    {
                        costEx += 1;
                        costSz += 1;
                    }
                }
#if defined(TARGET_AMD64)
                // increase costSz for floating point locals
                if (isflt)
                {
                    costSz += 1;
                    if (!gtIsLikelyRegVar(tree))
                    {
                        costSz += 1;
                    }
                }
#endif
                break;

            case GT_CLS_VAR_ADDR:
                level = 1;
#ifdef TARGET_ARM
                // We generate movw/movt
                costEx = 2;
                costSz = 4 + 4;
#else
                costEx     = 1;
                costSz     = 4;
#endif
                break;
            case GT_LCL_FLD:
                level  = 1;
                costEx = IND_COST_EX;
                costSz = 4;
                if (varTypeIsSmall(tree->TypeGet()))
                {
                    costEx += 1;
                    costSz += 1;
                }
                break;

            case GT_LCL_FLD_ADDR:
            case GT_LCL_VAR_ADDR:
                level  = 1;
                costEx = 3;
                costSz = 3;
                break;

            case GT_PHI_ARG:
            case GT_ARGPLACE:
                level  = 0;
                costEx = 0;
                costSz = 0;
                break;

            default:
                level  = 1;
                costEx = 1;
                costSz = 1;
                break;
        }
        goto DONE;
    }

    /* Is it a 'simple' unary/binary operator? */

    if (kind & GTK_SMPOP)
    {
        int      lvlb; // preference for op2
        unsigned lvl2; // scratch variable

        GenTree* op1 = tree->AsOp()->gtOp1;
        GenTree* op2 = tree->gtGetOp2IfPresent();

        costEx = 0;
        costSz = 0;

        if (tree->IsAddrMode())
        {
            if (op1 == nullptr)
            {
                op1 = op2;
                op2 = nullptr;
            }
        }

        /* Check for a nilary operator */

        if (op1 == nullptr)
        {
            assert(op2 == nullptr);

            level = 0;

            goto DONE;
        }

        /* Is this a unary operator? */

        if (op2 == nullptr)
        {
            /* Process the operand of the operator */

            /* Most Unary ops have costEx of 1 */
            costEx = 1;
            costSz = 1;

            level = gtSetEvalOrder(op1);

            GenTreeIntrinsic* intrinsic;

            /* Special handling for some operators */

            switch (oper)
            {
                case GT_JTRUE:
                    costEx = 2;
                    costSz = 2;
                    break;

                case GT_SWITCH:
                    costEx = 10;
                    costSz = 5;
                    break;

                case GT_CAST:
#if defined(TARGET_ARM)
                    costEx = 1;
                    costSz = 1;
                    if (isflt || varTypeIsFloating(op1->TypeGet()))
                    {
                        costEx = 3;
                        costSz = 4;
                    }
#elif defined(TARGET_ARM64)
                    costEx = 1;
                    costSz = 2;
                    if (isflt || varTypeIsFloating(op1->TypeGet()))
                    {
                        costEx = 2;
                        costSz = 4;
                    }
#elif defined(TARGET_XARCH)
                    costEx = 1;
                    costSz = 2;

                    if (isflt || varTypeIsFloating(op1->TypeGet()))
                    {
                        /* cast involving floats always go through memory */
                        costEx = IND_COST_EX * 2;
                        costSz = 6;
                    }
#else
#error "Unknown TARGET"
#endif

                    /* Overflow casts are a lot more expensive */
                    if (tree->gtOverflow())
                    {
                        costEx += 6;
                        costSz += 6;
                    }

                    break;

                case GT_NOP:
                    costEx = 0;
                    costSz = 0;
                    break;

                case GT_INTRINSIC:
                    intrinsic = tree->AsIntrinsic();
                    if (intrinsic->gtIntrinsicId == CORINFO_INTRINSIC_Illegal)
                    {
                        // named intrinsic
                        assert(intrinsic->gtIntrinsicName != NI_Illegal);

                        // GT_INTRINSIC intrinsics Sin, Cos, Sqrt, Abs ... have higher costs.
                        // TODO: tune these costs target specific as some of these are
                        // target intrinsics and would cost less to generate code.
                        switch (intrinsic->gtIntrinsicName)
                        {
                            default:
                                assert(!"missing case for gtIntrinsicName");
                                costEx = 12;
                                costSz = 12;
                                break;

                            case NI_System_Math_Abs:
                                costEx = 5;
                                costSz = 15;
                                break;

                            case NI_System_Math_Acos:
                            case NI_System_Math_Acosh:
                            case NI_System_Math_Asin:
                            case NI_System_Math_Asinh:
                            case NI_System_Math_Atan:
                            case NI_System_Math_Atanh:
                            case NI_System_Math_Atan2:
                            case NI_System_Math_Cbrt:
                            case NI_System_Math_Ceiling:
                            case NI_System_Math_Cos:
                            case NI_System_Math_Cosh:
                            case NI_System_Math_Exp:
                            case NI_System_Math_Floor:
                            case NI_System_Math_FMod:
                            case NI_System_Math_FusedMultiplyAdd:
                            case NI_System_Math_ILogB:
                            case NI_System_Math_Log:
                            case NI_System_Math_Log2:
                            case NI_System_Math_Log10:
                            case NI_System_Math_Pow:
                            case NI_System_Math_Round:
                            case NI_System_Math_Sin:
                            case NI_System_Math_Sinh:
                            case NI_System_Math_Sqrt:
                            case NI_System_Math_Tan:
                            case NI_System_Math_Tanh:
                            {
                                // Giving intrinsics a large fixed execution cost is because we'd like to CSE
                                // them, even if they are implemented by calls. This is different from modeling
                                // user calls since we never CSE user calls. We don't do this for target intrinsics
                                // however as they typically represent single instruction calls

                                if (IsIntrinsicImplementedByUserCall(intrinsic->gtIntrinsicName))
                                {
                                    costEx = 36;
                                    costSz = 4;
                                }
                                else
                                {
                                    costEx = 3;
                                    costSz = 4;
                                }
                                break;
                            }
                        }
                    }
                    else
                    {
                        // old style intrinsic (only Object_GetType is expected)
                        assert(intrinsic->gtIntrinsicName == NI_Illegal);
                        assert(intrinsic->gtIntrinsicId == CORINFO_INTRINSIC_Object_GetType);

                        // Giving intrinsics a large fixed execution cost is because we'd like to CSE
                        // them, even if they are implemented by calls. This is different from modeling
                        // user calls since we never CSE user calls.
                        costEx = 36;
                        costSz = 4;
                        break;
                    }
                    level++;
                    break;

                case GT_NOT:
                case GT_NEG:
                case GT_FNEG:
                    // We need to ensure that -x is evaluated before x or else
                    // we get burned while adjusting genFPstkLevel in x*-x where
                    // the rhs x is the last use of the enregistered x.
                    //
                    // Even in the integer case we want to prefer to
                    // evaluate the side without the GT_NEG node, all other things
                    // being equal.  Also a GT_NOT requires a scratch register

                    level++;
                    break;

                case GT_ARR_LENGTH:
                    level++;

                    /* Array Len should be the same as an indirections, which have a costEx of IND_COST_EX */
                    costEx = IND_COST_EX - 1;
                    costSz = 2;
                    break;

                case GT_MKREFANY:
                case GT_OBJ:
                    // We estimate the cost of a GT_OBJ or GT_MKREFANY to be two loads (GT_INDs)
                    costEx = 2 * IND_COST_EX;
                    costSz = 2 * 2;
                    break;

                case GT_BOX:
                    // We estimate the cost of a GT_BOX to be two stores (GT_INDs)
                    costEx = 2 * IND_COST_EX;
                    costSz = 2 * 2;
                    break;

                case GT_BLK:
                case GT_IND:

                    /* An indirection should always have a non-zero level.
                     * Only constant leaf nodes have level 0.
                     */

                    if (level == 0)
                    {
                        level = 1;
                    }

                    /* Indirections have a costEx of IND_COST_EX */
                    costEx = IND_COST_EX;
                    costSz = 2;

                    /* If we have to sign-extend or zero-extend, bump the cost */
                    if (varTypeIsSmall(tree->TypeGet()))
                    {
                        costEx += 1;
                        costSz += 1;
                    }

                    if (isflt)
                    {
                        if (tree->TypeGet() == TYP_DOUBLE)
                        {
                            costEx += 1;
                        }
#ifdef TARGET_ARM
                        costSz += 2;
#endif // TARGET_ARM
                    }

                    if (gtIsLikelyRegVar(op1))
                    {
                        // Indirection of an enregister LCL_VAR, don't increase costEx/costSz
                        goto DONE;
                    }
#ifdef TARGET_XARCH
                    else if (op1->IsCnsIntOrI())
                    {
                        costEx -= 1;
                    }
                    else if (op1->OperIs(GT_CLS_VAR_ADDR))
                    {
                        op1->SetDoNotCSE();
                        costEx -= 1;
                        costSz -= 2;
                    }
#endif
                    else
                    {
                        GenTree* addr = op1->SkipComma();

                        if (addr->OperIs(GT_ADD) && !addr->gtOverflow() &&
                            gtMarkAddrMode(addr, &costEx, &costSz, tree->TypeGet()))
                        {
                            while (op1 != addr)
                            {
                                // TODO-MIKE-CQ: Marking COMMAs with GTF_ADDRMODE_NO_CSE sometimes interferes with
                                // redundant range check elimination done via CSE.
                                // Normally CSE can't eliminate range checks because it uses liberal value numbers
                                // and that makes it senstive to race conditions in user code. However, if the
                                // entire array element address tree is CSEd, including the range check, then race
                                // conditions aren't an issue.
                                //
                                // So we have a choice between blocking CSE to allow address mode formation and
                                // allowing CSE in he hope that redundant range checks may get CSEd. We'll block
                                // CSE for now.
                                //
                                // CSE is anyway not guaranteed while address mode formation more or less is. And
                                // such address modes avoid slow 3 component LEAs.
                                // Also, this is a rather rare case, involving arrays of structs. Normally the COMMA
                                // is above the indir - COMMA(range check, IND(addr)) - and this avoids the whole
                                // issue. Sort of, as placing the COMMA like that probably makes it less likely for
                                // to be CSEd. But that's how it works in the typical case.
                                // With structs we may end up with IND(COMMA(range check, addr)), thanks in part to
                                // IND(COMMA(...)) morphing code not applying to OBJs as well.

                                op1->gtFlags |= GTF_ADDRMODE_NO_CSE;
                                op1 = op1->AsOp()->GetOp(1);
                            }

                            goto DONE;
                        }
                    }
                    break;

                default:
                    break;
            }
            costEx += op1->GetCostEx();
            costSz += op1->GetCostSz();
            goto DONE;
        }

        /* Binary operator - check for certain special cases */

        lvlb = 0;

        /* Default Binary ops have a cost of 1,1 */
        costEx = 1;
        costSz = 1;

#ifdef TARGET_ARM
        if (isflt)
        {
            costSz += 2;
        }
#endif
#ifndef TARGET_64BIT
        if (varTypeIsLong(op1->TypeGet()))
        {
            /* Operations on longs are more expensive */
            costEx += 3;
            costSz += 3;
        }
#endif
        switch (oper)
        {
            case GT_MOD:
            case GT_UMOD:

                /* Modulo by a power of 2 is easy */

                if (op2->IsCnsIntOrI())
                {
                    size_t ival = op2->AsIntConCommon()->IconValue();

                    if (ival > 0 && ival == genFindLowestBit(ival))
                    {
                        break;
                    }
                }

                FALLTHROUGH;

            case GT_DIV:
            case GT_UDIV:
                costEx += 19;
                costSz += 2;
                // Encourage the first operand to be evaluated (into EAX/EDX) first
                lvlb -= 3;
                break;

            case GT_MUL:
                costEx += 3;
                costSz += 2;

                if (tree->gtOverflow())
                {
                    /* Overflow check are more expensive */
                    costEx += 3;
                    costSz += 3;
                }

#ifdef TARGET_X86
                if ((tree->gtType == TYP_LONG) || tree->gtOverflow())
                {
                    /* We use imulEAX for TYP_LONG and overflow multiplications */
                    // Encourage the first operand to be evaluated (into EAX/EDX) first */
                    lvlb -= 4;

                    /* The 64-bit imul instruction costs more */
                    costEx += 4;
                }
#endif //  TARGET_X86
                break;

            case GT_ADD:
            case GT_SUB:
                if (tree->gtOverflow())
                {
                    costEx += 3;
                    costSz += 3;
                }
                break;

            case GT_FADD:
            case GT_FSUB:
            case GT_FMUL:
                costEx += 4;
                costSz += 3;
                break;

            case GT_FDIV:
                costEx += 35;
                costSz += 3;
                break;

            case GT_COMMA:
                gtSetEvalOrder(op1);
                level = gtSetEvalOrder(op2);

                // TODO-MIKE-Cleanup: ADDR costing code was bogus, it managed to add operand's costs
                // twice for ADDR(IND(x)) trees. Most ADDRs were removed during global morphing but
                // the ones computing array element addresses were not. So, in order to reduce diffs,
                // double op2's costs when the COMMA looks like an array element address.
                //
                // Also, address mode marking code managed to mark in ADDR(IND(x)) even though the
                // IND is fake and disappears before codegen, so there isn't really an address mode
                // to mark here. We'll mark it for now to reduce diffs, even if this increases the
                // chance of producing 3 component LEAs that may be slow on many older CPUs.

                if (op1->IsBoundsChk() && op2->TypeIs(TYP_BYREF) && op2->OperIs(GT_ADD) && !op2->gtOverflow())
                {
                    op2->SetCosts(op2->GetCostEx() * 2, op2->GetCostSz() * 2);

                    int indirCostEx = 0;
                    int indirCostSz = 0;
                    gtMarkAddrMode(op2, &indirCostEx, &indirCostSz, TYP_STRUCT);
                }

                costEx = op1->GetCostEx() + op2->GetCostEx();
                costSz = op1->GetCostSz() + op2->GetCostSz();

                goto DONE;

            case GT_ASG:
                /* Assignments need a bit of special handling */
                /* Process the target */
                level = gtSetEvalOrder(op1);

                if (gtIsLikelyRegVar(op1))
                {
                    assert(lvlb == 0);
                    lvl2 = gtSetEvalOrder(op2);

                    /* Assignment to an enregistered LCL_VAR */
                    costEx = op2->GetCostEx();
                    costSz = max(3, op2->GetCostSz()); // 3 is an estimate for a reg-reg assignment
                    goto DONE_OP1_AFTER_COST;
                }
                goto DONE_OP1;

            default:
                break;
        }

        /* Process the sub-operands */

        level = gtSetEvalOrder(op1);
        if (lvlb < 0)
        {
            level -= lvlb; // lvlb is negative, so this increases level
            lvlb = 0;
        }

    DONE_OP1:
        assert(lvlb >= 0);
        lvl2 = gtSetEvalOrder(op2) + lvlb;

        costEx += (op1->GetCostEx() + op2->GetCostEx());
        costSz += (op1->GetCostSz() + op2->GetCostSz());

    DONE_OP1_AFTER_COST:

        bool bReverseInAssignment = false;
        if (oper == GT_ASG && (!optValnumCSE_phase || optCSE_canSwap(op1, op2)))
        {
            GenTree* op1Val = op1;

            switch (op1Val->GetOper())
            {
                case GT_IND:
                case GT_BLK:
                case GT_OBJ:
                    // In an indirection, the destination address is evaluated prior to the source.
                    // If we have any side effects on the target indirection,
                    // we have to evaluate op1 first.
                    // However, if the LHS is a lclVar address, SSA relies on using evaluation order for its
                    // renaming, and therefore the RHS must be evaluated first.
                    // If we have an assignment involving a lclVar address, the LHS may be marked as having
                    // side-effects.
                    // However the side-effects won't require that we evaluate the LHS address first:
                    // - The GTF_GLOB_REF might have been conservatively set on a field of a local.
                    // - The local might be address-exposed, but that side-effect happens at the actual assignment (not
                    //   when its address is "evaluated") so it doesn't change the side effect to "evaluate" the address
                    //   after the RHS (note that in this case it won't be renamed by SSA anyway, but the reordering is
                    //   safe).
                    //
                    if (op1Val->AsIndir()->GetAddr()->IsLocalAddrExpr())
                    {
                        bReverseInAssignment = true;
                        tree->gtFlags |= GTF_REVERSE_OPS;
                        break;
                    }

                    if (op1Val->AsIndir()->GetAddr()->gtFlags & GTF_ALL_EFFECT)
                    {
                        break;
                    }

                    // In case op2 assigns to a local var that is used in op1Val, we have to evaluate op1Val first.
                    if (op2->gtFlags & GTF_ASG)
                    {
                        break;
                    }

                    // If op2 is simple then evaluate op1 first

                    if (op2->OperKind() & GTK_LEAF)
                    {
                        break;
                    }

                    // fall through and set GTF_REVERSE_OPS
                    FALLTHROUGH;

                case GT_LCL_VAR:
                case GT_LCL_FLD:

                    // We evaluate op2 before op1
                    bReverseInAssignment = true;
                    tree->gtFlags |= GTF_REVERSE_OPS;
                    break;

                default:
                    break;
            }
        }
        else if (GenTree::OperIsCompare(oper))
        {
            /* Float compares remove both operands from the FP stack */
            /* Also FP comparison uses EAX for flags */

            if (varTypeIsFloating(op1->TypeGet()))
            {
                level++;
                lvl2++;
            }
            if ((tree->gtFlags & GTF_RELOP_JMP_USED) == 0)
            {
                /* Using a setcc instruction is more expensive */
                costEx += 3;
            }
        }

        /* Check for other interesting cases */

        switch (oper)
        {
            case GT_LSH:
            case GT_RSH:
            case GT_RSZ:
            case GT_ROL:
            case GT_ROR:
                /* Variable sized shifts are more expensive and use REG_SHIFT */

                if (!op2->IsCnsIntOrI())
                {
                    costEx += 3;
#ifndef TARGET_64BIT
                    // Variable sized LONG shifts require the use of a helper call
                    //
                    if (tree->gtType == TYP_LONG)
                    {
                        level += 5;
                        lvl2 += 5;
                        costEx += 3 * IND_COST_EX;
                        costSz += 4;
                    }
#endif // !TARGET_64BIT
                }
                break;

            case GT_INTRINSIC:

                switch (tree->AsIntrinsic()->gtIntrinsicName)
                {
                    case NI_System_Math_Atan2:
                    case NI_System_Math_Pow:
                        // These math intrinsics are actually implemented by user calls.
                        // Increase the Sethi 'complexity' by two to reflect the argument
                        // register requirement.
                        level += 2;
                        break;
                    default:
                        assert(!"Unknown binary GT_INTRINSIC operator");
                        break;
                }

                break;

            default:
                break;
        }

        /* We need to evalutate constants later as many places in codegen
           can't handle op1 being a constant. This is normally naturally
           enforced as constants have the least level of 0. However,
           sometimes we end up with a tree like "cns1 < nop(cns2)". In
           such cases, both sides have a level of 0. So encourage constants
           to be evaluated last in such cases */

        if ((level == 0) && (level == lvl2) && op1->OperIsConst() &&
            (tree->OperIsCommutative() || tree->OperIsCompare()))
        {
            lvl2++;
        }

        /* We try to swap operands if the second one is more expensive */
        bool     tryToSwap;
        GenTree* opA;
        GenTree* opB;

        if (tree->gtFlags & GTF_REVERSE_OPS)
        {
            opA = op2;
            opB = op1;
        }
        else
        {
            opA = op1;
            opB = op2;
        }

        if (fgOrder == FGOrderLinear)
        {
            // Don't swap anything if we're in linear order; we're really just interested in the costs.
            tryToSwap = false;
        }
        else if (bReverseInAssignment)
        {
            // Assignments are special, we want the reverseops flags
            // so if possible it was set above.
            tryToSwap = false;
        }
        else if ((oper == GT_INTRINSIC) && IsIntrinsicImplementedByUserCall(tree->AsIntrinsic()->gtIntrinsicName))
        {
            // We do not swap operand execution order for intrinsics that are implemented by user calls
            // because of trickiness around ensuring the execution order does not change during rationalization.
            tryToSwap = false;
        }
        else
        {
            if (tree->gtFlags & GTF_REVERSE_OPS)
            {
                tryToSwap = (level > lvl2);
            }
            else
            {
                tryToSwap = (level < lvl2);
            }

            // Try to force extra swapping when in the stress mode:
            if (compStressCompile(STRESS_REVERSE_FLAG, 60) && ((tree->gtFlags & GTF_REVERSE_OPS) == 0) &&
                !op2->OperIsConst())
            {
                tryToSwap = true;
            }
        }

        if (tryToSwap)
        {
            bool canSwap = gtCanSwapOrder(opA, opB);

            if (canSwap)
            {
                /* Can we swap the order by commuting the operands? */

                switch (oper)
                {
                    case GT_EQ:
                    case GT_NE:
                    case GT_LT:
                    case GT_LE:
                    case GT_GE:
                    case GT_GT:
                        if (GenTree::SwapRelop(oper) != oper)
                        {
                            tree->SetOper(GenTree::SwapRelop(oper), GenTree::PRESERVE_VN);
                        }

                        FALLTHROUGH;

                    case GT_FADD:
                    case GT_FMUL:

                    case GT_ADD:
                    case GT_MUL:

                    case GT_OR:
                    case GT_XOR:
                    case GT_AND:

                        /* Swap the operands */

                        tree->AsOp()->gtOp1 = op2;
                        tree->AsOp()->gtOp2 = op1;
                        break;

                    case GT_MKREFANY:
                        break;

                    default:

                        /* Mark the operand's evaluation order to be swapped */
                        if (tree->gtFlags & GTF_REVERSE_OPS)
                        {
                            tree->gtFlags &= ~GTF_REVERSE_OPS;
                        }
                        else
                        {
                            tree->gtFlags |= GTF_REVERSE_OPS;
                        }

                        break;
                }
            }
        }

        /* Swap the level counts */
        if (tree->gtFlags & GTF_REVERSE_OPS)
        {
            unsigned tmpl;

            tmpl  = level;
            level = lvl2;
            lvl2  = tmpl;
        }

        /* Compute the sethi number for this binary operator */

        if (level < 1)
        {
            level = lvl2;
        }
        else if (level == lvl2)
        {
            level += 1;
        }

        goto DONE;
    }

    /* See what kind of a special operator we have here */

    switch (oper)
    {
        unsigned lvl2; // Scratch variable

        case GT_CALL:

            assert(tree->gtFlags & GTF_CALL);

            level  = 0;
            costEx = 5;
            costSz = 2;

            GenTreeCall* call;
            call = tree->AsCall();

            /* Evaluate the 'this' argument, if present */

            if (tree->AsCall()->gtCallThisArg != nullptr)
            {
                GenTree* thisVal = tree->AsCall()->gtCallThisArg->GetNode();

                lvl2 = gtSetEvalOrder(thisVal);
                if (level < lvl2)
                {
                    level = lvl2;
                }
                costEx += thisVal->GetCostEx();
                costSz += thisVal->GetCostSz() + 1;
            }

            /* Evaluate the arguments, right to left */

            if (call->gtCallArgs != nullptr)
            {
                const bool lateArgs = false;
                lvl2                = gtSetCallArgsOrder(call->Args(), lateArgs, &costEx, &costSz);
                if (level < lvl2)
                {
                    level = lvl2;
                }
            }

            /* Evaluate the temp register arguments list
             * This is a "hidden" list and its only purpose is to
             * extend the life of temps until we make the call */

            if (call->gtCallLateArgs != nullptr)
            {
                const bool lateArgs = true;
                lvl2                = gtSetCallArgsOrder(call->LateArgs(), lateArgs, &costEx, &costSz);
                if (level < lvl2)
                {
                    level = lvl2;
                }
            }

            if (call->gtCallType == CT_INDIRECT)
            {
                // pinvoke-calli cookie is a constant, or constant indirection
                assert(call->gtCallCookie == nullptr || call->gtCallCookie->gtOper == GT_CNS_INT ||
                       call->gtCallCookie->gtOper == GT_IND);

                GenTree* indirect = call->gtCallAddr;

                lvl2 = gtSetEvalOrder(indirect);
                if (level < lvl2)
                {
                    level = lvl2;
                }
                costEx += indirect->GetCostEx() + IND_COST_EX;
                costSz += indirect->GetCostSz();
            }
            else
            {
                if (call->IsVirtual())
                {
                    GenTree* controlExpr = call->gtControlExpr;
                    if (controlExpr != nullptr)
                    {
                        lvl2 = gtSetEvalOrder(controlExpr);
                        if (level < lvl2)
                        {
                            level = lvl2;
                        }
                        costEx += controlExpr->GetCostEx();
                        costSz += controlExpr->GetCostSz();
                    }
                }
#ifdef TARGET_ARM
                if (call->IsVirtualStub())
                {
                    // We generate movw/movt/ldr
                    costEx += (1 + IND_COST_EX);
                    costSz += 8;
                    if (call->gtCallMoreFlags & GTF_CALL_M_VIRTSTUB_REL_INDIRECT)
                    {
                        // Must use R12 for the ldr target -- REG_JUMP_THUNK_PARAM
                        costSz += 2;
                    }
                }
                else if (!opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT))
                {
                    costEx += 2;
                    costSz += 6;
                }
                costSz += 2;
#endif

#ifdef TARGET_XARCH
                costSz += 3;
#endif
            }

            level += 1;

            /* Virtual calls are a bit more expensive */
            if (call->IsVirtual())
            {
                costEx += 2 * IND_COST_EX;
                costSz += 2;
            }

            level += 5;
            costEx += 3 * IND_COST_EX;
            break;

        case GT_ARR_ELEM:
        {
            GenTreeArrElem* arrElem = tree->AsArrElem();

            level  = gtSetEvalOrder(arrElem->gtArrObj);
            costEx = arrElem->gtArrObj->GetCostEx();
            costSz = arrElem->gtArrObj->GetCostSz();

            for (unsigned dim = 0; dim < arrElem->gtArrRank; dim++)
            {
                lvl2 = gtSetEvalOrder(arrElem->gtArrInds[dim]);
                if (level < lvl2)
                {
                    level = lvl2;
                }
                costEx += arrElem->gtArrInds[dim]->GetCostEx();
                costSz += arrElem->gtArrInds[dim]->GetCostSz();
            }

            level += arrElem->gtArrRank;
            costEx += 2 + (arrElem->gtArrRank * (IND_COST_EX + 1));
            costSz += 2 + (arrElem->gtArrRank * 2);
        }
        break;

        case GT_PHI:
            for (GenTreePhi::Use& use : tree->AsPhi()->Uses())
            {
                lvl2 = gtSetEvalOrder(use.GetNode());
                // PHI args should always have cost 0 and level 0
                assert(lvl2 == 0);
                assert(use.GetNode()->GetCostEx() == 0);
                assert(use.GetNode()->GetCostSz() == 0);
            }
            // Give it a level of 2, just to be sure that it's greater than the LHS of
            // the parent assignment and the PHI gets evaluated first in linear order.
            // See also SsaBuilder::InsertPhi and SsaBuilder::AddPhiArg.
            level  = 2;
            costEx = 0;
            costSz = 0;
            break;

        case GT_FIELD_LIST:
            level  = 0;
            costEx = 0;
            costSz = 0;
            for (GenTreeFieldList::Use& use : tree->AsFieldList()->Uses())
            {
                unsigned opLevel = gtSetEvalOrder(use.GetNode());
                level            = max(level, opLevel);
                gtSetEvalOrder(use.GetNode());
                costEx += use.GetNode()->GetCostEx();
                costSz += use.GetNode()->GetCostSz();
            }
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
#ifdef TARGET_XARCH
            if (tree->AsHWIntrinsic()->IsUnary() && tree->AsHWIntrinsic()->OperIsMemoryLoadOrStore())
            {
                level  = gtSetEvalOrder(tree->AsHWIntrinsic()->GetOp(0));
                costEx = IND_COST_EX;
                costSz = 2;
                // See if we can form a complex addressing mode.

                GenTree* addr = tree->AsHWIntrinsic()->GetOp(0)->gtEffectiveVal();

                if (addr->OperIs(GT_ADD) && gtMarkAddrMode(addr, &costEx, &costSz, tree->TypeGet()))
                {
                    goto DONE;
                }
            }
#endif // TARGET_XARCH

            if (tree->AsHWIntrinsic()->IsBinary())
            {
                GenTree* op1 = tree->AsHWIntrinsic()->GetOp(0);
                GenTree* op2 = tree->AsHWIntrinsic()->GetOp(1);

                if (tree->IsReverseOp())
                {
                    std::swap(op1, op2);
                }

                level = gtSetEvalOrder(op1);
                lvl2  = gtSetEvalOrder(op2);

                if ((fgOrder == FGOrderTree) && (level < lvl2) && gtCanSwapOrder(op1, op2)
#ifndef TARGET_64BIT
                    // Create lowering depends on the evaluation ordering matching the operand order
                    // if there are more than 2 operands. Vector128.Create<long> starts with 2 operands
                    // but due to decomposition it ends up with 4 operands in lowering so we need to
                    // prevent reordering.
                    && ((tree->AsHWIntrinsic()->GetIntrinsic() != NI_Vector128_Create) ||
                        !varTypeIsLong(tree->AsHWIntrinsic()->GetSimdBaseType()))
#endif
                        )
                {
                    tree->gtFlags ^= GTF_REVERSE_OPS;
                    std::swap(level, lvl2);
                }

                if (level == 0)
                {
                    level = lvl2;
                }
                else if (level == lvl2)
                {
                    level += 1;
                }

                costEx = op1->GetCostEx() + op2->GetCostEx() + 1;
                costSz = op1->GetCostSz() + op2->GetCostSz() + 1;
            }
            else
            {
                level  = 0;
                costEx = 1;
                costSz = 1;

                for (GenTreeHWIntrinsic::Use& use : tree->AsHWIntrinsic()->Uses())
                {
                    level = max(level, gtSetEvalOrder(use.GetNode()));
                    costEx += use.GetNode()->GetCostEx();
                    costSz += use.GetNode()->GetCostSz();
                }

                level++;
            }
            break;
#endif // FEATURE_HW_INTRINSICS

        case GT_INSTR:
            level  = 0;
            costEx = 1;
            costSz = 1;

            for (GenTreeInstr::Use& use : tree->AsInstr()->Uses())
            {
                level = max(level, gtSetEvalOrder(use.GetNode()));
                costEx += use.GetNode()->GetCostEx();
                costSz += use.GetNode()->GetCostSz();
            }

            level++;
            break;

        case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
        case GT_HW_INTRINSIC_CHK:
#endif
            costEx = 4; // cmp reg,reg and jae throw (not taken)
            costSz = 7; // jump to cold section

            level = gtSetEvalOrder(tree->AsBoundsChk()->GetIndex());
            costEx += tree->AsBoundsChk()->GetIndex()->GetCostEx();
            costSz += tree->AsBoundsChk()->GetIndex()->GetCostSz();

            lvl2 = gtSetEvalOrder(tree->AsBoundsChk()->GetLength());
            if (level < lvl2)
            {
                level = lvl2;
            }
            costEx += tree->AsBoundsChk()->GetLength()->GetCostEx();
            costSz += tree->AsBoundsChk()->GetLength()->GetCostSz();
            break;

        case GT_ARR_OFFSET:
        case GT_CMPXCHG:
        case GT_COPY_BLK:
        case GT_INIT_BLK:
            level = 0;

            if (tree->OperIs(GT_CMPXCHG))
            {
                costEx = MAX_COST; // Seriously, what could be more expensive than lock cmpxchg?
                costSz = 5;        // size of lock cmpxchg [reg+C], reg
            }
            else
            {
                // TODO-MIKE-Cleanup: We should some set costs on ARR_OFFSET and COPY|INIT_BLK
                // for the sake of clarity. It's unlikely to matter otherwise.
                costEx = 0;
                costSz = 0;
            }

            for (unsigned i = 0; i < 3; i++)
            {
                level = Max(level, gtSetEvalOrder(tree->AsTernaryOp()->GetOp(i)));
            }

            for (unsigned i = 0; i < 3; i++)
            {
                costEx += tree->AsTernaryOp()->GetOp(i)->GetCostEx();
                costSz += tree->AsTernaryOp()->GetOp(i)->GetCostSz();
            }
            break;

        case GT_INDEX_ADDR:
            costEx = 6; // cmp reg,reg; jae throw; mov reg, [addrmode]  (not taken)
            costSz = 9; // jump to cold section

            level = gtSetEvalOrder(tree->AsIndexAddr()->GetIndex());
            costEx += tree->AsIndexAddr()->GetIndex()->GetCostEx();
            costSz += tree->AsIndexAddr()->GetIndex()->GetCostSz();

            lvl2 = gtSetEvalOrder(tree->AsIndexAddr()->GetArray());
            if (level < lvl2)
            {
                level = lvl2;
            }
            costEx += tree->AsIndexAddr()->GetArray()->GetCostEx();
            costSz += tree->AsIndexAddr()->GetArray()->GetCostSz();
            break;

        default:
            JITDUMP("unexpected operator in this tree:\n");
            DISPTREE(tree);

            NO_WAY("unexpected operator");
    }

DONE:

    // Some path through this function must have set the costs.
    assert(costEx != -1);
    assert(costSz != -1);

    tree->SetCosts(costEx, costSz);

    return level;
}
#ifdef _PREFAST_
#pragma warning(pop)
#endif

//------------------------------------------------------------------------
// FindUse: Find the use of a node within this node.
//
// Arguments:
//    def - The definition node for which to find the corresponding use.
//
// Return Value:
//    The use edge that corresponds to the definition node, if one exists.
//
GenTree** GenTree::FindUse(GenTree* def)
{
    assert(def != nullptr);

    GenTree** use = nullptr;

    VisitOperands([def, &use](GenTree*& op) {
        if (op == def)
        {
            use = &op;
            return VisitResult::Abort;
        }
        return VisitResult::Continue;
    });

    return use;
}

void GenTree::ReplaceOperand(GenTree** useEdge, GenTree* replacement)
{
    assert(useEdge != nullptr);
    assert(replacement != nullptr);
    assert(FindUse(*useEdge) == useEdge);

    *useEdge = replacement;
}

//------------------------------------------------------------------------
// FindUser: Find the user of a node, and optionally capture the use so
//    that it can be modified.
//
// Arguments:
//    useEdge - An optional pointer used to return the user's use edge
//              that points to this node.
//
// Return Value:
//    The user of this node, if one exists.
//
// Notes:
//    This requires that the execution order must be defined
//    (i.e. gtSetEvalOrder() has been called).
//
GenTree* GenTree::FindUser(GenTree*** useEdge)
{
    for (GenTree* user = gtNext; user != nullptr; user = user->gtNext)
    {
        GenTree** u = user->FindUse(this);
        if (u != nullptr)
        {
            if (useEdge != nullptr)
            {
                *useEdge = u;
            }
            return user;
        }
    }

    return nullptr;
}

GenTreeRetExpr::GenTreeRetExpr(var_types type, GenTreeCall* call)
    : GenTree(GT_RET_EXPR, type), m_call(call), m_retExpr(call), m_retBlockIRSummary(BBF_EMPTY)
{
    // GT_RET_EXPR node eventually might be bashed back to GT_CALL (when inlining is aborted for example).
    // Therefore it should carry the GTF_CALL flag so that all the rules about spilling can apply to it as well.
    // For example, impImportLeave or CEE_POP need to spill GT_RET_EXPR before empty the evaluation stack.
    gtFlags |= GTF_CALL;
}

//------------------------------------------------------------------------------
// OperRequiresAsgFlag : Check whether the operation requires GTF_ASG flag regardless
//                       of the children's flags.
//

bool GenTree::OperRequiresAsgFlag()
{
    if (OperIs(GT_ASG) ||
        OperIs(GT_XADD, GT_XORR, GT_XAND, GT_XCHG, GT_LOCKADD, GT_CMPXCHG, GT_MEMORYBARRIER, GT_COPY_BLK, GT_INIT_BLK))
    {
        return true;
    }
#ifdef FEATURE_HW_INTRINSICS
    if (gtOper == GT_HWINTRINSIC)
    {
        GenTreeHWIntrinsic* hwIntrinsicNode = this->AsHWIntrinsic();
        if (hwIntrinsicNode->OperIsMemoryStore())
        {
            // A MemoryStore operation is an assignment
            return true;
        }
    }
#endif // FEATURE_HW_INTRINSICS
    return false;
}

//------------------------------------------------------------------------------
// OperRequiresCallFlag : Check whether the operation requires GTF_CALL flag regardless
//                        of the children's flags.
//

bool GenTree::OperRequiresCallFlag(Compiler* comp)
{
    switch (gtOper)
    {
        case GT_CALL:
            return true;

        case GT_KEEPALIVE:
            return true;

        case GT_INTRINSIC:
            return comp->IsIntrinsicImplementedByUserCall(this->AsIntrinsic()->gtIntrinsicName);

#if FEATURE_FIXED_OUT_ARGS && !defined(TARGET_64BIT)
        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:

            // Variable shifts of a long end up being helper calls, so mark the tree as such in morph.
            // This is potentially too conservative, since they'll get treated as having side effects.
            // It is important to mark them as calls so if they are part of an argument list,
            // they will get sorted and processed properly (for example, it is important to handle
            // all nested calls before putting struct arguments in the argument registers). We
            // could mark the trees just before argument processing, but it would require a full
            // tree walk of the argument tree, so we just do it when morphing, instead, even though we'll
            // mark non-argument trees (that will still get converted to calls, anyway).
            return (this->TypeGet() == TYP_LONG) && (gtGetOp2()->OperGet() != GT_CNS_INT);
#endif // FEATURE_FIXED_OUT_ARGS && !TARGET_64BIT

        default:
            return false;
    }
}

//------------------------------------------------------------------------------
// OperIsImplicitIndir : Check whether the operation contains an implicit
//                       indirection.
// Arguments:
//    this      -  a GenTree node
//
// Return Value:
//    True if the given node contains an implicit indirection
//
// Note that for the GT_HWINTRINSIC node we have to examine the
// details of the node to determine its result.
//

bool GenTree::OperIsImplicitIndir() const
{
    switch (gtOper)
    {
        case GT_LOCKADD:
        case GT_XORR:
        case GT_XAND:
        case GT_XADD:
        case GT_XCHG:
        case GT_CMPXCHG:
        case GT_BLK:
        case GT_OBJ:
        case GT_STORE_BLK:
        case GT_STORE_OBJ:
        case GT_COPY_BLK:
        case GT_INIT_BLK:
        case GT_BOX:
        case GT_ARR_INDEX:
        case GT_ARR_ELEM:
        case GT_ARR_OFFSET:
            return true;
#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
        {
            return AsHWIntrinsic()->OperIsMemoryLoadOrStore();
        }
#endif // FEATURE_HW_INTRINSICS
        default:
            return false;
    }
}

//------------------------------------------------------------------------------
// OperMayThrow : Check whether the operation may throw.
//
//
// Arguments:
//    comp      -  Compiler instance
//
// Return Value:
//    True if the given operator may cause an exception

bool GenTree::OperMayThrow(Compiler* comp)
{
    GenTree* op;

    switch (gtOper)
    {
        case GT_MOD:
        case GT_DIV:
        case GT_UMOD:
        case GT_UDIV:

            /* Division with a non-zero, non-minus-one constant does not throw an exception */

            op = AsOp()->gtOp2;

            // For integers only division by 0 or by -1 can throw
            if (op->IsIntegralConst() && !op->IsIntegralConst(0) && !op->IsIntegralConst(-1))
            {
                return false;
            }
            return true;

        case GT_INTRINSIC:
            // If this is an intrinsic that represents the object.GetType(), it can throw an NullReferenceException.
            // Report it as may throw.
            // Note: Some of the rest of the existing intrinsics could potentially throw an exception (for example
            //       the array and string element access ones). They are handled differently than the GetType intrinsic
            //       and are not marked with GTF_EXCEPT. If these are revisited at some point to be marked as
            //       GTF_EXCEPT,
            //       the code below might need to be specialized to handle them properly.
            if ((this->gtFlags & GTF_EXCEPT) != 0)
            {
                return true;
            }

            break;

        case GT_CALL:

            CorInfoHelpFunc helper;
            helper = comp->eeGetHelperNum(this->AsCall()->gtCallMethHnd);
            return ((helper == CORINFO_HELP_UNDEF) || !comp->s_helperCallProperties.NoThrow(helper));

        case GT_IND:
        case GT_BLK:
        case GT_OBJ:
        case GT_STORE_BLK:
        case GT_NULLCHECK:
            return (((this->gtFlags & GTF_IND_NONFAULTING) == 0) && comp->fgAddrCouldBeNull(this->AsIndir()->Addr()));

        case GT_COPY_BLK:
        case GT_INIT_BLK:
            return !AsDynBlk()->GetSize()->IsIntegralConst(0);

        case GT_ARR_LENGTH:
            return ((gtFlags & GTF_IND_NONFAULTING) == 0) && comp->fgAddrCouldBeNull(AsArrLen()->GetArray());

        case GT_ARR_ELEM:
            return comp->fgAddrCouldBeNull(this->AsArrElem()->gtArrObj);

        case GT_FIELD_ADDR:
            return comp->fgAddrCouldBeNull(AsFieldAddr()->GetAddr());

        case GT_ARR_BOUNDS_CHECK:
        case GT_ARR_INDEX:
        case GT_ARR_OFFSET:
        case GT_LCLHEAP:
        case GT_CKFINITE:
            return true;

        case GT_INDEX_ADDR:
            return (gtFlags & GTF_INX_RNGCHK) != 0;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HW_INTRINSIC_CHK:
            return true;
        case GT_HWINTRINSIC:
            return AsHWIntrinsic()->OperIsMemoryLoadOrStore();
#endif
        default:
            break;
    }

    /* Overflow arithmetic operations also throw exceptions */

    if (gtOverflowEx())
    {
        return true;
    }

    return false;
}

unsigned GenTreeLclVar::GetMultiRegCount(Compiler* compiler) const
{
    assert(IsMultiReg());

    return compiler->lvaGetDesc(GetLclNum())->GetPromotedFieldCount();
}

var_types GenTreeLclVar::GetMultiRegType(Compiler* compiler, unsigned regIndex)
{
    assert(IsMultiReg());

    LclVarDsc* lcl      = compiler->lvaGetDesc(GetLclNum());
    LclVarDsc* fieldLcl = compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(regIndex));
    assert(!fieldLcl->TypeIs(TYP_STRUCT));
    return fieldLcl->GetType();
}

#if DEBUGGABLE_GENTREE
// static
GenTree::VtablePtr GenTree::s_vtablesForOpers[] = {nullptr};
GenTree::VtablePtr GenTree::s_vtableForOp       = nullptr;

GenTree::VtablePtr GenTree::GetVtableForOper(genTreeOps oper)
{
    noway_assert(oper < GT_COUNT);

    // First, check a cache.

    if (s_vtablesForOpers[oper] != nullptr)
    {
        return s_vtablesForOpers[oper];
    }

    // Otherwise, look up the correct vtable entry. Note that we want the most derived GenTree subtype
    // for an oper. E.g., GT_LCL_VAR is defined in GTSTRUCT_3 as GenTreeLclVar and in GTSTRUCT_N as
    // GenTreeLclVarCommon. We want the GenTreeLclVar vtable, since nothing should actually be
    // instantiated as a GenTreeLclVarCommon.

    VtablePtr res = nullptr;
    switch (oper)
    {

// clang-format off

#define GTSTRUCT_0(nm, tag)                             /*handle explicitly*/
#define GTSTRUCT_1(nm, tag)                             \
        case tag:                                       \
        {                                               \
            GenTree##nm gt;                             \
            res = *reinterpret_cast<VtablePtr*>(&gt);   \
        }                                               \
        break;
#define GTSTRUCT_2(nm, tag, tag2)                       \
        case tag:                                       \
        case tag2:                                      \
        {                                               \
            GenTree##nm gt;                             \
            res = *reinterpret_cast<VtablePtr*>(&gt);   \
        }                                               \
        break;
#define GTSTRUCT_3(nm, tag, tag2, tag3)                 \
        case tag:                                       \
        case tag2:                                      \
        case tag3:                                      \
        {                                               \
            GenTree##nm gt;                             \
            res = *reinterpret_cast<VtablePtr*>(&gt);   \
        }                                               \
        break;
#define GTSTRUCT_4(nm, tag, tag2, tag3, tag4)           \
        case tag:                                       \
        case tag2:                                      \
        case tag3:                                      \
        case tag4:                                      \
        {                                               \
            GenTree##nm gt;                             \
            res = *reinterpret_cast<VtablePtr*>(&gt);   \
        }                                               \
        break;
#define GTSTRUCT_N(nm, ...)                             /*handle explicitly*/
#define GTSTRUCT_2_SPECIAL(nm, tag, tag2)               /*handle explicitly*/
#define GTSTRUCT_3_SPECIAL(nm, tag, tag2, tag3)         /*handle explicitly*/
#include "gtstructs.h"

        // clang-format on

        // Handle the special cases.
        // The following opers are in GTSTRUCT_N but no other place (namely, no subtypes).

        case GT_STORE_BLK:
        case GT_BLK:
        {
            GenTreeBlk gt;
            res = *reinterpret_cast<VtablePtr*>(&gt);
        }
        break;

        case GT_IND:
        case GT_NULLCHECK:
        {
            GenTreeIndir gt;
            res = *reinterpret_cast<VtablePtr*>(&gt);
        }
        break;

        // We don't need to handle GTSTRUCT_N for LclVarCommon, since all those allowed opers are specified
        // in their proper subtype. Similarly for GenTreeIndir.

        default:
        {
            // Should be unary or binary op.
            if (s_vtableForOp == nullptr)
            {
                unsigned opKind = OperKind(oper);
                assert(!IsExOp(opKind));
                assert(OperIsSimple(oper) || OperIsLeaf(oper));
                // Need to provide non-null operands.
                GenTreeIntCon dummyOp(TYP_INT, 0);
                GenTreeOp     gt(oper, TYP_INT, &dummyOp, ((opKind & GTK_UNOP) ? nullptr : &dummyOp));
                s_vtableForOp = *reinterpret_cast<VtablePtr*>(&gt);
            }
            res = s_vtableForOp;
            break;
        }
    }
    s_vtablesForOpers[oper] = res;
    return res;
}

void GenTree::SetVtableForOper(genTreeOps oper)
{
    *reinterpret_cast<VtablePtr*>(this) = GetVtableForOper(oper);
}
#endif // DEBUGGABLE_GENTREE

GenTreeOp* Compiler::gtNewOperNode(genTreeOps oper, var_types type, GenTree* op1, GenTree* op2)
{
    assert(op1 != nullptr);
    assert(op2 != nullptr);

    // We should not be allocating nodes that extend GenTreeOp with this;
    // should call the appropriate constructor for the extended type.
    assert(!GenTree::IsExOp(GenTree::OperKind(oper)));

    return new (this, oper) GenTreeOp(oper, type, op1, op2);
}

GenTreeOp* Compiler::gtNewCommaNode(GenTree* op1, GenTree* op2, var_types type)
{
    assert(op1 != nullptr);
    assert(op2 != nullptr);

    if (type == TYP_UNDEF)
    {
        // ASG and NULLCHECK have non VOID types but they don't actually
        // produce a value. Don't propagate the type through COMMAs.
        type = op2->OperIs(GT_ASG, GT_NULLCHECK) ? TYP_VOID : op2->GetType();
    }

    assert(!op2->OperIs(GT_NULLCHECK, GT_ASG) || (type == TYP_VOID));

    return new (this, GT_COMMA) GenTreeOp(GT_COMMA, type, op1, op2);
}

GenTreeQmark* Compiler::gtNewQmarkNode(var_types type, GenTree* cond, GenTree* op1, GenTree* op2)
{
    assert(!compQmarkRationalized);

    compQmarkUsed = true;
    return new (this, GT_QMARK) GenTreeQmark(type, cond, op1, op2);
}

GenTreeIntCon* Compiler::gtNewIconNode(ssize_t value, var_types type)
{
    return new (this, GT_CNS_INT) GenTreeIntCon(type, value);
}

GenTreeIntCon* Compiler::gtNewIconNode(unsigned fieldOffset, FieldSeqNode* fieldSeq)
{
    return new (this, GT_CNS_INT) GenTreeIntCon(TYP_I_IMPL, static_cast<ssize_t>(fieldOffset),
                                                fieldSeq == nullptr ? FieldSeqStore::NotAField() : fieldSeq);
}

GenTreeIntCon* Compiler::gtNewIntConFieldOffset(target_size_t fieldOffset, FieldSeqNode* fieldSeq)
{
    return new (this, GT_CNS_INT) GenTreeIntCon(TYP_I_IMPL, static_cast<ssize_t>(fieldOffset),
                                                fieldSeq == nullptr ? FieldSeqStore::NotAField() : fieldSeq);
}

// return a new node representing the value in a physical register
GenTreePhysReg* Compiler::gtNewPhysRegNode(regNumber reg, var_types type)
{
    assert(genIsValidIntReg(reg) || (reg == REG_SPBASE));
    return new (this, GT_PHYSREG) GenTreePhysReg(reg, type);
}

GenTree* Compiler::gtNewJmpTableNode()
{
    return new (this, GT_JMPTABLE) GenTree(GT_JMPTABLE, TYP_I_IMPL);
}

/*****************************************************************************
 *
 *  Converts an annotated token into an icon flags (so that we will later be
 *  able to tell the type of the handle that will be embedded in the icon
 *  node)
 */

GenTreeFlags Compiler::gtTokenToIconFlags(unsigned token)
{
    GenTreeFlags flags = GTF_EMPTY;

    switch (TypeFromToken(token))
    {
        case mdtTypeRef:
        case mdtTypeDef:
        case mdtTypeSpec:
            flags = GTF_ICON_CLASS_HDL;
            break;

        case mdtMethodDef:
            flags = GTF_ICON_METHOD_HDL;
            break;

        case mdtFieldDef:
            flags = GTF_ICON_FIELD_HDL;
            break;

        default:
            flags = GTF_ICON_TOKEN_HDL;
            break;
    }

    return flags;
}

//-----------------------------------------------------------------------------------------
// gtNewIndOfIconHandleNode: Creates an indirection GenTree node of a constant handle
//
// Arguments:
//    indType     - The type returned by the indirection node
//    addr        - The constant address to read from
//    iconFlags   - The GTF_ICON flag value that specifies the kind of handle that we have
//    isInvariant - The indNode should also be marked as invariant
//
// Return Value:
//    Returns a GT_IND node representing value at the address provided by 'value'
//
// Notes:
//    The GT_IND node is marked as non-faulting
//    If the indType is GT_REF we also mark the indNode as GTF_GLOB_REF
//

GenTree* Compiler::gtNewIndOfIconHandleNode(var_types indType, size_t addr, GenTreeFlags iconFlags, bool isInvariant)
{
    GenTree* addrNode = gtNewIconHandleNode(addr, iconFlags);
    GenTree* indNode  = gtNewOperNode(GT_IND, indType, addrNode);

    // This indirection won't cause an exception.
    //
    indNode->gtFlags |= GTF_IND_NONFAULTING;

    // String Literal handles are indirections that return a TYP_REF, and
    // these are pointers into the GC heap.  We don't currently have any
    // TYP_BYREF pointers, but if we did they also must be pointers into the GC heap.
    //
    // Also every GTF_ICON_STATIC_HDL also must be a pointer into the GC heap
    // we will set GTF_GLOB_REF for these kinds of references.
    //
    if ((varTypeIsGC(indType)) || (iconFlags == GTF_ICON_STATIC_HDL))
    {
        // This indirection also points into the gloabal heap
        indNode->gtFlags |= GTF_GLOB_REF;
    }

    if (isInvariant)
    {
        assert(iconFlags != GTF_ICON_STATIC_HDL); // Pointer to a mutable class Static variable
        assert(iconFlags != GTF_ICON_BBC_PTR);    // Pointer to a mutable basic block count value
        assert(iconFlags != GTF_ICON_GLOBAL_PTR); // Pointer to mutable data from the VM state

        // This indirection also is invariant.
        indNode->gtFlags |= GTF_IND_INVARIANT;

        if (iconFlags == GTF_ICON_STR_HDL)
        {
            // String literals are never null
            indNode->gtFlags |= GTF_IND_NONNULL;
        }
    }
    return indNode;
}

/*****************************************************************************
 *
 *  Allocates a integer constant entry that represents a HANDLE to something.
 *  It may not be allowed to embed HANDLEs directly into the JITed code (for eg,
 *  as arguments to JIT helpers). Get a corresponding value that can be embedded.
 *  If the handle needs to be accessed via an indirection, pValue points to it.
 */

GenTree* Compiler::gtNewIconEmbHndNode(void* value, void* pValue, GenTreeFlags iconFlags, void* compileTimeHandle)
{
    GenTree* iconNode;
    GenTree* handleNode;

    if (value != nullptr)
    {
        // When 'value' is non-null, pValue is required to be null
        assert(pValue == nullptr);

        iconNode = gtNewIconHandleNode(reinterpret_cast<size_t>(value), iconFlags);

        // 'value' is the handle
        handleNode = iconNode;
    }
    else
    {
        // When 'value' is null, pValue is required to be non-null
        assert(pValue != nullptr);

        // 'pValue' is the address of a location that contains the handle
        iconNode   = gtNewIconHandleNode(reinterpret_cast<size_t>(pValue), iconFlags);
        handleNode = gtNewOperNode(GT_IND, TYP_I_IMPL, iconNode);
        handleNode->gtFlags |= GTF_IND_NONFAULTING | GTF_IND_INVARIANT;
    }

    iconNode->AsIntCon()->gtCompileTimeHandle = reinterpret_cast<size_t>(compileTimeHandle);

    return handleNode;
}

/*****************************************************************************/
GenTree* Compiler::gtNewStringLiteralNode(InfoAccessType iat, void* pValue)
{
    GenTree* tree = nullptr;

    switch (iat)
    {
        case IAT_VALUE:
            setMethodHasFrozenString();
            tree         = gtNewIconEmbHndNode(pValue, nullptr, GTF_ICON_STR_HDL, nullptr);
            tree->gtType = TYP_REF;
#ifdef DEBUG
            tree->AsIntCon()->gtTargetHandle = (size_t)pValue;
#endif
            break;

        case IAT_PVALUE: // The value needs to be accessed via an indirection
            // Create an indirection
            tree = gtNewIndOfIconHandleNode(TYP_REF, (size_t)pValue, GTF_ICON_STR_HDL, true);
#ifdef DEBUG
            tree->gtGetOp1()->AsIntCon()->gtTargetHandle = (size_t)pValue;
#endif
            break;

        case IAT_PPVALUE: // The value needs to be accessed via a double indirection
            // Create the first indirection
            tree = gtNewIndOfIconHandleNode(TYP_I_IMPL, (size_t)pValue, GTF_ICON_CONST_PTR, true);
#ifdef DEBUG
            tree->gtGetOp1()->AsIntCon()->gtTargetHandle = (size_t)pValue;
#endif

            // Create the second indirection
            tree = gtNewOperNode(GT_IND, TYP_REF, tree);
            // This indirection won't cause an exception.
            tree->gtFlags |= GTF_IND_NONFAULTING;
            // This indirection points into the gloabal heap (it is String Object)
            tree->gtFlags |= GTF_GLOB_REF;
            break;

        default:
            noway_assert(!"Unexpected InfoAccessType");
    }

    return tree;
}

//------------------------------------------------------------------------
// gtNewStringLiteralLength: create GenTreeIntCon node for the given string
//    literal to store its length.
//
// Arguments:
//    node  - string literal node.
//
// Return Value:
//    GenTreeIntCon node with string's length as a value or null.
//
GenTreeIntCon* Compiler::gtNewStringLiteralLength(GenTreeStrCon* node)
{
    int             length = -1;
    const char16_t* str    = info.compCompHnd->getStringLiteral(node->gtScpHnd, node->gtSconCPX, &length);
    if (length >= 0)
    {
        GenTreeIntCon* iconNode = gtNewIconNode(length);

        // str can be NULL for dynamic context
        if (str != nullptr)
        {
            JITDUMP("String '\"%ws\".Length' is '%d'\n", str, length)
        }
        else
        {
            JITDUMP("String 'CNS_STR.Length' is '%d'\n", length)
        }
        return iconNode;
    }
    return nullptr;
}

/*****************************************************************************/

GenTree* Compiler::gtNewLconNode(int64_t value)
{
#ifdef TARGET_64BIT
    GenTree* node = new (this, GT_CNS_INT) GenTreeIntCon(TYP_LONG, value);
#else
    GenTree* node = new (this, GT_CNS_LNG) GenTreeLngCon(value);
#endif

    return node;
}

GenTree* Compiler::gtNewDconNode(double value, var_types type)
{
    GenTree* node = new (this, GT_CNS_DBL) GenTreeDblCon(value, type);

    return node;
}

GenTree* Compiler::gtNewSconNode(int CPX, CORINFO_MODULE_HANDLE scpHandle)
{
    // 'GT_CNS_STR' nodes later get transformed into 'GT_CALL'
    assert(GenTree::s_gtNodeSizes[GT_CALL] > GenTree::s_gtNodeSizes[GT_CNS_STR]);
    GenTree* node = new (this, GT_CALL) GenTreeStrCon(CPX, scpHandle DEBUGARG(/*largeNode*/ true));
    return node;
}

GenTree* Compiler::gtNewZeroConNode(var_types type)
{
    switch (varActualType(type))
    {
        case TYP_INT:
        case TYP_BYREF:
        case TYP_REF:
            return gtNewIconNode(0, type);
        case TYP_LONG:
            return gtNewLconNode(0);
        case TYP_FLOAT:
        case TYP_DOUBLE:
            return gtNewDconNode(0.0, type);
        default:
            unreached();
    }
}

GenTree* Compiler::gtNewOneConNode(var_types type)
{
    switch (varActualType(type))
    {
        case TYP_INT:
            return gtNewIconNode(1);
        case TYP_LONG:
            return gtNewLconNode(1);
        case TYP_FLOAT:
        case TYP_DOUBLE:
            return gtNewDconNode(1.0, type);
        default:
            unreached();
    }
}

GenTreeLclVar* Compiler::gtNewStoreLclVar(unsigned lclNum, var_types type, GenTree* src)
{
    return new (this, GT_STORE_LCL_VAR) GenTreeLclVar(type, lclNum, src);
}

GenTreeLclFld* Compiler::gtNewStoreLclFld(var_types type, unsigned lclNum, unsigned lclOffs, GenTree* value)
{
    return new (this, GT_STORE_LCL_FLD) GenTreeLclFld(type, lclNum, lclOffs, value);
}

GenTreeCall* Compiler::gtNewIndCallNode(GenTree* addr, var_types type, GenTreeCall::Use* args, IL_OFFSETX ilOffset)
{
    return gtNewCallNode(CT_INDIRECT, (CORINFO_METHOD_HANDLE)addr, type, args, ilOffset);
}

GenTreeCall* Compiler::gtNewCallNode(
    gtCallTypes kind, CORINFO_METHOD_HANDLE callHnd, var_types type, GenTreeCall::Use* args, IL_OFFSETX ilOffset)
{
    GenTreeCall* node = new (this, GT_CALL) GenTreeCall(type, kind, args);
#ifdef UNIX_X86_ABI
    node->gtFlags |= GTF_CALL_POP_ARGS;
#endif // UNIX_X86_ABI
    node->gtCallMethHnd = callHnd;

    if (kind == CT_INDIRECT)
    {
        node->gtCallCookie = nullptr;
    }
    else
    {
        node->gtInlineCandidateInfo = nullptr;
    }

#ifdef FEATURE_READYTORUN_COMPILER
    node->gtEntryPoint.addr       = nullptr;
    node->gtEntryPoint.accessType = IAT_VALUE;
#endif

#if defined(DEBUG) || defined(INLINE_DATA)
    // These get updated after call node is built.
    node->gtInlineObservation = InlineObservation::CALLEE_UNUSED_INITIAL;
    node->gtRawILOffset       = BAD_IL_OFFSET;
#endif

    // Spec: Managed Retval sequence points needs to be generated while generating debug info for debuggable code.
    //
    // Implementation note: if not generating MRV info genCallSite2ILOffsetMap will be NULL and
    // codegen will pass BAD_IL_OFFSET as IL offset of a call node to emitter, which will cause emitter
    // not to emit IP mapping entry.
    if (opts.compDbgCode && opts.compDbgInfo)
    {
        // Managed Retval - IL offset of the call.  This offset is used to emit a
        // CALL_INSTRUCTION type sequence point while emitting corresponding native call.
        //
        // TODO-Cleanup:
        // a) (Opt) We need not store this offset if the method doesn't return a
        // value.  Rather it can be made BAD_IL_OFFSET to prevent a sequence
        // point being emitted.
        //
        // b) (Opt) Add new sequence points only if requested by debugger through
        // a new boundary type - ICorDebugInfo::BoundaryTypes
        if (genCallSite2ILOffsetMap == nullptr)
        {
            genCallSite2ILOffsetMap = new (getAllocator()) CallSiteILOffsetTable(getAllocator());
        }

        // Make sure that there are no duplicate entries for a given call node
        assert(!genCallSite2ILOffsetMap->Lookup(node));
        genCallSite2ILOffsetMap->Set(node, ilOffset);
    }

    // Initialize gtOtherRegs
    node->ClearOtherRegs();

#ifndef TARGET_64BIT
    if (varTypeIsLong(type))
    {
        node->GetRetDesc()->InitializeLong();
    }
#endif

    return node;
}

GenTreeLclVar* Compiler::gtNewLclvNode(unsigned lnum, var_types type DEBUGARG(IL_OFFSETX ILoffs))
{
#ifdef DEBUG
    // We need to ensure that all struct values are normalized.
    // It might be nice to assert this in general, but we have assignments of int to long.
    if (varTypeIsStruct(type))
    {
        LclVarDsc* lcl = lvaGetDesc(lnum);

        // Make an exception for implicit by-ref parameters during global morph, since
        // their lvType has been updated to byref but their appearances have not yet all
        // been rewritten and so may have struct type still.
        bool simd12ToSimd16Widening = false;
#if FEATURE_SIMD
        // We can additionally have a SIMD12 that was widened to a SIMD16, generally as part of lowering
        simd12ToSimd16Widening = (type == TYP_SIMD16) && lcl->TypeIs(TYP_SIMD12);
#endif
        assert((type == lcl->GetType()) || simd12ToSimd16Widening ||
               (lcl->IsImplicitByRefParam() && fgGlobalMorph && lcl->TypeIs(TYP_BYREF)));
    }
#endif

    return new (this, GT_LCL_VAR) GenTreeLclVar(GT_LCL_VAR, type, lnum DEBUGARG(ILoffs));
}

GenTreeLclVar* Compiler::gtNewLclVarLargeNode(unsigned lnum, var_types type DEBUGARG(IL_OFFSETX ILoffs))
{
#ifdef DEBUG
    LclVarDsc* lcl = lvaGetDesc(lnum);

    // We need to ensure that all struct values are normalized.
    // It might be nice to assert this in general, but we have assignments of int to long.
    if (varTypeIsStruct(type))
    {
        // Make an exception for implicit by-ref parameters during global morph, since
        // their lvType has been updated to byref but their appearances have not yet all
        // been rewritten and so may have struct type still.
        assert((type == lcl->GetType()) ||
               (lcl->IsImplicitByRefParam() && fgGlobalMorph && (lcl->GetType() == TYP_BYREF)));
    }
#endif

    // This local variable node may later get transformed into a large node
    assert(GenTree::s_gtNodeSizes[LargeOpOpcode()] > GenTree::s_gtNodeSizes[GT_LCL_VAR]);

    return new (this, LargeOpOpcode())
        GenTreeLclVar(GT_LCL_VAR, type, lnum DEBUGARG(ILoffs) DEBUGARG(/*largeNode*/ true));
}

GenTreeLclVar* Compiler::gtNewLclVarAddrNode(unsigned lclNum, var_types type)
{
    return new (this, GT_LCL_VAR_ADDR) GenTreeLclVar(GT_LCL_VAR_ADDR, type, lclNum);
}

GenTreeLclFld* Compiler::gtNewLclFldAddrNode(unsigned lclNum, unsigned lclOffs, FieldSeqNode* fieldSeq, var_types type)
{
    GenTreeLclFld* node = new (this, GT_LCL_FLD_ADDR) GenTreeLclFld(GT_LCL_FLD_ADDR, type, lclNum, lclOffs);
    node->SetFieldSeq(fieldSeq == nullptr ? FieldSeqStore::NotAField() : fieldSeq);
    return node;
}

GenTreeLclFld* Compiler::gtNewLclFldNode(unsigned lnum, var_types type, unsigned offset)
{
    return new (this, GT_LCL_FLD) GenTreeLclFld(GT_LCL_FLD, type, lnum, offset);
}

GenTreeRetExpr* Compiler::gtNewRetExpr(GenTreeCall* call, var_types type)
{
    return new (this, GT_RET_EXPR) GenTreeRetExpr(type, call);
}

GenTreeCall::Use* Compiler::gtPrependNewCallArg(GenTree* node, GenTreeCall::Use* args)
{
    return new (this, CMK_ASTNode) GenTreeCall::Use(node, args);
}

GenTreeCall::Use* Compiler::gtInsertNewCallArgAfter(GenTree* node, GenTreeCall::Use* after)
{
    after->SetNext(new (this, CMK_ASTNode) GenTreeCall::Use(node, after->GetNext()));
    return after->GetNext();
}

GenTreeCall::Use* Compiler::gtNewCallArgs(GenTree* node)
{
    return new (this, CMK_ASTNode) GenTreeCall::Use(node);
}

GenTreeCall::Use* Compiler::gtNewCallArgs(GenTree* node1, GenTree* node2)
{
    return new (this, CMK_ASTNode) GenTreeCall::Use(node1, gtNewCallArgs(node2));
}

GenTreeCall::Use* Compiler::gtNewCallArgs(GenTree* node1, GenTree* node2, GenTree* node3)
{
    return new (this, CMK_ASTNode) GenTreeCall::Use(node1, gtNewCallArgs(node2, node3));
}

GenTreeCall::Use* Compiler::gtNewCallArgs(GenTree* node1, GenTree* node2, GenTree* node3, GenTree* node4)
{
    return new (this, CMK_ASTNode) GenTreeCall::Use(node1, gtNewCallArgs(node2, node3, node4));
}

GenTree* GenTreeCall::GetThisArg() const
{
    assert(gtCallThisArg != nullptr);

    if (fgArgInfo == nullptr)
    {
        return gtCallThisArg->GetNode();
    }

    CallArgInfo* argInfo = GetArgInfoByArgNum(0);
    assert(argInfo->use == gtCallThisArg);
    return argInfo->GetNode();
}

CallArgInfo* GenTreeCall::GetArgInfoByArgNum(unsigned argNum) const
{
    noway_assert(fgArgInfo != nullptr);
    assert(argNum < fgArgInfo->GetArgCount());
    assert(fgArgInfo->GetArgInfo(argNum)->GetArgNum() == argNum);

    return fgArgInfo->GetArgInfo(argNum);
}

GenTree* GenTreeCall::GetArgNodeByArgNum(unsigned argNum) const
{
    return GetArgInfoByArgNum(argNum)->GetNode();
}

CallArgInfo* GenTreeCall::GetArgInfoByArgNode(GenTree* node) const
{
    noway_assert(fgArgInfo != nullptr);

    for (unsigned i = 0; i < fgArgInfo->GetArgCount(); i++)
    {
        CallArgInfo* argInfo = fgArgInfo->GetArgInfo(i);

        if (argInfo->GetNode() == node)
        {
            return argInfo;
        }

        if (argInfo->use->GetNode() == node)
        {
            return argInfo;
        }
    }

    unreached();
}

CallArgInfo* GenTreeCall::GetArgInfoByLateArgUse(Use* use) const
{
    noway_assert(fgArgInfo != nullptr);
    assert(use != nullptr);

    for (unsigned i = 0; i < fgArgInfo->GetArgCount(); i++)
    {
        CallArgInfo* argInfo = fgArgInfo->GetArgInfo(i);

        if (argInfo->HasLateUse() && (argInfo->GetLateUse() == use))
        {
            return argInfo;
        }
    }

    unreached();
}

GenTreeOp* Compiler::gtNewAssignNode(GenTree* dst, GenTree* src)
{
    assert(!src->TypeIs(TYP_VOID));

    dst->gtFlags |= GTF_DONT_CSE;

    gtAssignSetVarDef(dst);

    GenTreeOp* asg = gtNewOperNode(GT_ASG, dst->GetType(), dst, src);
    asg->gtFlags |= GTF_ASG;
    return asg;
}

void Compiler::gtAssignSetVarDef(GenTree* dst)
{
    if (dst->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        dst->gtFlags |= GTF_VAR_DEF;

        if (dst->IsPartialLclFld(this))
        {
            dst->gtFlags |= GTF_VAR_USEASG;
        }
    }
}

//------------------------------------------------------------------------
// gtNewObjNode: Creates a new Obj node.
//
// Arguments:
//    structHnd - The class handle of the struct type.
//    addr      - The address of the struct.
//
// Return Value:
//    Returns a node representing the struct value at the given address.
//
GenTreeObj* Compiler::gtNewObjNode(CORINFO_CLASS_HANDLE structHnd, GenTree* addr)
{
    return gtNewObjNode(typGetObjLayout(structHnd), addr);
}

GenTreeObj* Compiler::gtNewObjNode(ClassLayout* layout, GenTree* addr)
{
    return gtNewObjNode(typGetStructType(layout), layout, addr);
}

GenTreeObj* Compiler::gtNewObjNode(var_types type, ClassLayout* layout, GenTree* addr)
{
    assert(varTypeIsStruct(type));

    GenTreeObj* objNode = new (this, GT_OBJ) GenTreeObj(type, addr, layout);

    if (GenTreeLclVarCommon* lclNode = addr->IsLocalAddrExpr())
    {
        objNode->gtFlags |= GTF_IND_NONFAULTING;

        // An Obj is not a global reference, if it is known to be a local struct.
        if (((addr->gtFlags & GTF_GLOB_REF) == 0) && !lvaIsImplicitByRefLocal(lclNode->GetLclNum()))
        {
            objNode->gtFlags &= ~GTF_GLOB_REF;
        }
    }
    else if (GenTreeFieldAddr* fieldAddr = addr->IsFieldAddr())
    {
        FieldSeqNode* fieldSeq = fieldAddr->GetFieldSeq();

        if (fieldSeq->IsBoxedValueField() ||
            (fieldSeq->IsField() && info.compCompHnd->isFieldStatic(fieldSeq->GetFieldHandle())))
        {
            objNode->gtFlags |= GTF_IND_NONFAULTING;
        }
    }

    return objNode;
}

//----------------------------------------------------------------------------
// UsesDivideByConstOptimized:
//    returns true if rationalize will use the division by constant
//    optimization for this node.
//
// Arguments:
//    this - a GenTreeOp node
//    comp - the compiler instance
//
// Return Value:
//    Return true iff the node is a GT_DIV,GT_UDIV, GT_MOD or GT_UMOD with
//    an integer constant and we can perform the division operation using
//    a reciprocal multiply or a shift operation.
//
bool GenTreeOp::UsesDivideByConstOptimized(Compiler* comp)
{
    if (!comp->opts.OptimizationEnabled())
    {
        return false;
    }

    if (!OperIs(GT_DIV, GT_MOD, GT_UDIV, GT_UMOD))
    {
        return false;
    }
#if defined(TARGET_ARM64)
    if (OperIs(GT_MOD, GT_UMOD))
    {
        // MOD, UMOD not supported for ARM64
        return false;
    }
#endif // TARGET_ARM64

    bool     isSignedDivide = OperIs(GT_DIV, GT_MOD);
    GenTree* dividend       = GetOp(0)->SkipComma();
    GenTree* divisor        = GetOp(1)->SkipComma();

#if !defined(TARGET_64BIT)
    if (dividend->OperIs(GT_LONG))
    {
        return false;
    }
#endif

    if (dividend->IsCnsIntOrI())
    {
        // We shouldn't see a divmod with constant operands here but if we do then it's likely
        // because optimizations are disabled or it's a case that's supposed to throw an exception.
        // Don't optimize this.
        return false;
    }

    ssize_t divisorValue;
    if (divisor->IsCnsIntOrI())
    {
        divisorValue = static_cast<ssize_t>(divisor->AsIntCon()->IconValue());
    }
    else
    {
        ValueNum vn = divisor->gtVNPair.GetLiberal();
        if (comp->vnStore->IsVNConstant(vn))
        {
            divisorValue = comp->vnStore->CoercedConstantValue<ssize_t>(vn);
        }
        else
        {
            return false;
        }
    }

    const var_types divType = TypeGet();

    if (divisorValue == 0)
    {
        // x / 0 and x % 0 can't be optimized because they are required to throw an exception.
        return false;
    }
    else if (isSignedDivide)
    {
        if (divisorValue == -1)
        {
            // x / -1 can't be optimized because INT_MIN / -1 is required to throw an exception.
            return false;
        }
        else if (isPow2(divisorValue))
        {
            return true;
        }
    }
    else // unsigned divide
    {
        if (divType == TYP_INT)
        {
            // Clear up the upper 32 bits of the value, they may be set to 1 because constants
            // are treated as signed and stored in ssize_t which is 64 bit in size on 64 bit targets.
            divisorValue &= UINT32_MAX;
        }

        size_t unsignedDivisorValue = (size_t)divisorValue;
        if (isPow2(unsignedDivisorValue))
        {
            return true;
        }
    }

    const bool isDiv = OperIs(GT_DIV, GT_UDIV);

    if (isDiv)
    {
        if (isSignedDivide)
        {
            // If the divisor is the minimum representable integer value then the result is either 0 or 1
            if ((divType == TYP_INT && divisorValue == INT_MIN) || (divType == TYP_LONG && divisorValue == INT64_MIN))
            {
                return true;
            }
        }
        else
        {
            // If the divisor is greater or equal than 2^(N - 1) then the result is either 0 or 1
            if (((divType == TYP_INT) && ((UINT32)divisorValue > (UINT32_MAX / 2))) ||
                ((divType == TYP_LONG) && ((UINT64)divisorValue > (UINT64_MAX / 2))))
            {
                return true;
            }
        }
    }

// TODO-ARM-CQ: Currently there's no GT_MULHI for ARM32
#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
    if (!comp->opts.MinOpts() && ((divisorValue >= 3) || !isSignedDivide))
    {
        // All checks pass we can perform the division operation using a reciprocal multiply.
        return true;
    }
#endif

    return false;
}

//------------------------------------------------------------------------
// CheckDivideByConstOptimized:
//      Checks if we can use the division by constant optimization
//      on this node
//      and if so sets the flag GTF_DIV_BY_CNS_OPT and
//      set GTF_DONT_CSE on the constant node
//
// Arguments:
//    this       - a GenTreeOp node
//    comp       - the compiler instance
//
void GenTreeOp::CheckDivideByConstOptimized(Compiler* comp)
{
    if (UsesDivideByConstOptimized(comp))
    {
        gtFlags |= GTF_DIV_BY_CNS_OPT;

        // Now set DONT_CSE on the GT_CNS_INT divisor, note that
        // with ValueNumbering we can have a non GT_CNS_INT divisior
        GenTree* divisor = GetOp(1)->SkipComma();
        if (divisor->OperIs(GT_CNS_INT))
        {
            divisor->gtFlags |= GTF_DONT_CSE;
        }
    }
}

//------------------------------------------------------------------------
// gtInitStructCopyAsg: Initializes a struct copy assignment.
//
// Arguments:
//    asg - an assignment node that is to be initialized.
//
void Compiler::gtInitStructCopyAsg(GenTreeOp* asg)
{
    assert(asg->OperIs(GT_ASG));

    GenTree* dst = asg->GetOp(0);
    GenTree* src = asg->GetOp(1);

    assert(varTypeIsStruct(dst->GetType()));

    if (src->OperIs(GT_INIT_VAL, GT_CNS_INT))
    {
        return;
    }

    // In the case of a block copy, we want to avoid generating nodes where the source
    // and destination are the same because of two reasons, first, is useless, second
    // it introduces issues in liveness and also copying memory from an overlapping
    // memory location is undefined both as per the ECMA standard and also the memcpy
    // semantics specify that.
    //
    // NOTE: In this case we'll only detect the case for addr of a local and a local
    // itself, any other complex expressions won't be caught.
    //
    // TODO-Cleanup: though having this logic is goodness (i.e. avoids self-assignment
    // of struct vars very early), it was added because fgInterBlockLocalVarLiveness()
    // isn't handling self-assignment of struct variables correctly. This issue may not
    // surface if struct promotion is ON (which is the case on x86/arm). But still the
    // fundamental issue exists that needs to be addressed.

    unsigned srcLclNum = BAD_VAR_NUM;
    unsigned dstLclNum = BAD_VAR_NUM;

    if (src->OperIsIndir() && src->AsIndir()->GetAddr()->OperIs(GT_LCL_VAR_ADDR))
    {
        srcLclNum = src->AsIndir()->GetAddr()->AsLclVar()->GetLclNum();
    }
    else if (src->OperIs(GT_LCL_VAR))
    {
        srcLclNum = src->AsLclVar()->GetLclNum();
    }

    if (dst->OperIsIndir() && dst->AsIndir()->GetAddr()->OperIs(GT_LCL_VAR_ADDR))
    {
        dstLclNum = dst->AsIndir()->GetAddr()->AsLclVar()->GetLclNum();
    }
    else if (dst->OperIs(GT_LCL_VAR))
    {
        dstLclNum = dst->AsLclVar()->GetLclNum();
    }

    if (dstLclNum != BAD_VAR_NUM)
    {
        if (srcLclNum == dstLclNum)
        {
            // Make this a NOP
            // TODO-Cleanup: probably doesn't matter, but could do this earlier and avoid creating a GT_ASG
            asg->ChangeToNothingNode();
            return;
        }

#ifdef FEATURE_SIMD
        if (GenTreeHWIntrinsic* hwi = src->IsHWIntrinsic())
        {
            if (varTypeIsSIMD(lvaGetDesc(dstLclNum)->GetType()))
            {
                lvaRecordSimdIntrinsicDef(dstLclNum, hwi);
            }
        }
#endif
    }
}

//------------------------------------------------------------------------
// gtNewBitCastNode: Creates a new BitCast node.
//
// Arguments:
//    type   - The actual type of the argument
//    arg    - The argument node
//    argReg - The register that the argument will be passed in
//
// Return Value:
//    Returns the newly created BitCast node.
//
// Notes:
//    The node is generated as GenTreeMultiRegOp on RyuJIT/arm, as GenTreeOp on all the other archs.
//
GenTreeUnOp* Compiler::gtNewBitCastNode(var_types type, GenTree* arg)
{
    assert(arg != nullptr);
    assert(type != TYP_STRUCT);

    return gtNewOperNode(GT_BITCAST, type, arg)->AsUnOp();
}

//------------------------------------------------------------------------
// gtNewAllocObjNode: Helper to create an object allocation node.
//
// Arguments:
//    pResolvedToken   - Resolved token for the object being allocated
//    useParent     -    true iff the token represents a child of the object's class
//
// Return Value:
//    Returns GT_ALLOCOBJ node that will be later morphed into an
//    allocation helper call or local variable allocation on the stack.

GenTreeAllocObj* Compiler::gtNewAllocObjNode(CORINFO_RESOLVED_TOKEN* pResolvedToken, bool useParent)
{
    const bool      mustRestoreHandle     = true;
    bool* const     pRuntimeLookup        = nullptr;
    bool            usingReadyToRunHelper = false;
    CorInfoHelpFunc helper                = CORINFO_HELP_UNDEF;
    GenTree*        opHandle = impTokenToHandle(pResolvedToken, pRuntimeLookup, mustRestoreHandle, useParent);

#ifdef FEATURE_READYTORUN_COMPILER
    CORINFO_CONST_LOOKUP lookup = {};

    if (opts.IsReadyToRun())
    {
        helper                                        = CORINFO_HELP_READYTORUN_NEW;
        CORINFO_LOOKUP_KIND* const pGenericLookupKind = nullptr;
        usingReadyToRunHelper =
            info.compCompHnd->getReadyToRunHelper(pResolvedToken, pGenericLookupKind, helper, &lookup);
    }
#endif

    if (!usingReadyToRunHelper)
    {
        if (opHandle == nullptr)
        {
            // We must be backing out of an inline.
            assert(compDonotInline());
            return nullptr;
        }
    }

    bool            helperHasSideEffects;
    CorInfoHelpFunc helperTemp =
        info.compCompHnd->getNewHelper(pResolvedToken, info.compMethodHnd, &helperHasSideEffects);

    if (!usingReadyToRunHelper)
    {
        helper = helperTemp;
    }

    // TODO: ReadyToRun: When generic dictionary lookups are necessary, replace the lookup call
    // and the newfast call with a single call to a dynamic R2R cell that will:
    //      1) Load the context
    //      2) Perform the generic dictionary lookup and caching, and generate the appropriate stub
    //      3) Allocate and return the new object for boxing
    // Reason: performance (today, we'll always use the slow helper for the R2R generics case)

    GenTreeAllocObj* allocObj =
        gtNewAllocObjNode(helper, helperHasSideEffects, pResolvedToken->hClass, TYP_REF, opHandle);

#ifdef FEATURE_READYTORUN_COMPILER
    if (usingReadyToRunHelper)
    {
        allocObj->gtEntryPoint = lookup;
    }
#endif

    return allocObj;
}

/*****************************************************************************
 *
 *  Clones the given tree value and returns a copy of the given tree.
 *  If 'complexOK' is false, the cloning is only done provided the tree
 *     is not too complex (whatever that may mean);
 *  If 'complexOK' is true, we try slightly harder to clone the tree.
 *  In either case, NULL is returned if the tree cannot be cloned
 *
 *  Note that there is the function gtCloneExpr() which does a more
 *  complete job if you can't handle this function failing.
 */

GenTree* Compiler::gtClone(GenTree* tree, bool complexOK)
{
    GenTree* copy;

    switch (tree->gtOper)
    {
        case GT_CNS_INT:
            copy = new (this, GT_CNS_INT) GenTreeIntCon(tree->AsIntCon());
            break;

        case GT_CNS_LNG:
            copy = gtNewLconNode(tree->AsLngCon()->gtLconVal);
            break;

        case GT_LCL_VAR:
        case GT_LCL_VAR_ADDR:
            // Remember that the LclVar node has been cloned. The flag will be set
            // on 'copy' as well.
            tree->gtFlags |= GTF_VAR_CLONED;
            copy = new (this, tree->GetOper()) GenTreeLclVar(tree->AsLclVar());
            break;

        case GT_LCL_FLD:
        case GT_LCL_FLD_ADDR:
            // Remember that the LclVar node has been cloned. The flag will be set
            // on 'copy' as well.
            tree->gtFlags |= GTF_VAR_CLONED;
            copy = new (this, tree->GetOper()) GenTreeLclFld(tree->AsLclFld());
            break;

        case GT_CLS_VAR_ADDR:
            copy = new (this, GT_CLS_VAR_ADDR) GenTreeClsVar(tree->AsClsVar());
            break;

        default:
            if (!complexOK)
            {
                return nullptr;
            }

            // TODO-MIKE-Review: This should check for IND(FIELD_ADDR) to be 100% equivalent
            // to old FIELD code but then it looks like this is pointless. The address is
            // cloned with "complexOK = false" so it's basically limited to a local or const
            // node. In both cases the FIELD should have had GTF_GLOB_REF and most code that
            // calls gtClone with "complexOK = true" does so only after checking that "tree"
            // does not have side effects.
            if (GenTreeFieldAddr* field = tree->IsFieldAddr())
            {
                GenTree* addr = gtClone(field->GetAddr(), false);

                if (addr == nullptr)
                {
                    return nullptr;
                }

                copy = new (this, GT_FIELD_ADDR) GenTreeFieldAddr(field);
                copy->AsFieldAddr()->SetAddr(addr);
            }
            else if (tree->OperIs(GT_ADD, GT_SUB))
            {
                GenTree* op1 = tree->AsOp()->gtOp1;
                GenTree* op2 = tree->AsOp()->gtOp2;

                if (op1->OperIsLeaf() && op2->OperIsLeaf())
                {
                    op1 = gtClone(op1);
                    if (op1 == nullptr)
                    {
                        return nullptr;
                    }
                    op2 = gtClone(op2);
                    if (op2 == nullptr)
                    {
                        return nullptr;
                    }

                    copy = gtNewOperNode(tree->OperGet(), tree->TypeGet(), op1, op2);
                }
                else
                {
                    return nullptr;
                }
            }
            else
            {
                return nullptr;
            }

            break;
    }

    copy->gtFlags |= tree->gtFlags & ~GTF_NODE_MASK;
#if defined(DEBUG)
    copy->gtDebugFlags |= tree->gtDebugFlags & ~GTF_DEBUG_NODE_MASK;
#endif // defined(DEBUG)

    return copy;
}

//------------------------------------------------------------------------
// gtCloneExpr: Create a copy of `tree`, adding flags `addFlags`, mapping
//              local `varNum` to int constant `varVal` if it appears at
//              the root, and mapping uses of local `deepVarNum` to constant
//              `deepVarVal` if they occur beyond the root.
//
// Arguments:
//    tree - GenTree to create a copy of
//    addFlags - GTF_* flags to add to the copied tree nodes
//    varNum - lclNum to replace at the root, or ~0 for no root replacement
//    varVal - If replacing at root, replace local `varNum` with IntCns `varVal`
//    deepVarNum - lclNum to replace uses of beyond the root, or ~0 for no replacement
//    deepVarVal - If replacing beyond root, replace `deepVarNum` with IntCns `deepVarVal`
//
// Return Value:
//    A copy of the given tree with the replacements and added flags specified.
//
// Notes:
//    Top-level callers should generally call the overload that doesn't have
//    the explicit `deepVarNum` and `deepVarVal` parameters; those are used in
//    recursive invocations to avoid replacing defs.

GenTree* Compiler::gtCloneExpr(
    GenTree* tree, GenTreeFlags addFlags, unsigned varNum, int varVal, unsigned deepVarNum, int deepVarVal)
{
    if (tree == nullptr)
    {
        return nullptr;
    }

    /* Figure out what kind of a node we have */

    genTreeOps oper = tree->OperGet();
    unsigned   kind = tree->OperKind();
    GenTree*   copy;

    /* Is this a constant or leaf node? */

    if (kind & GTK_LEAF)
    {
        switch (oper)
        {
            case GT_CNS_INT:
                copy = new (this, GT_CNS_INT) GenTreeIntCon(tree->AsIntCon());
                goto DONE;

            case GT_CNS_LNG:
                copy = gtNewLconNode(tree->AsLngCon()->gtLconVal);
                goto DONE;

            case GT_CNS_DBL:
                copy         = gtNewDconNode(tree->AsDblCon()->gtDconVal);
                copy->gtType = tree->gtType; // keep the same type
                goto DONE;

            case GT_CNS_STR:
                copy = gtNewSconNode(tree->AsStrCon()->gtSconCPX, tree->AsStrCon()->gtScpHnd);
                goto DONE;

            case GT_LCL_VAR:
            case GT_LCL_VAR_ADDR:
                if (tree->AsLclVar()->GetLclNum() == varNum)
                {
                    copy = gtNewIconNode(varVal, tree->gtType);
                }
                else
                {
                    // Remember that the LclVar node has been cloned. The flag will
                    // be set on 'copy' as well.
                    tree->gtFlags |= GTF_VAR_CLONED;
                    copy = new (this, tree->GetOper()) GenTreeLclVar(tree->AsLclVar());
                }
                copy->gtFlags = tree->gtFlags;
                goto DONE;

            case GT_LCL_FLD:
            case GT_LCL_FLD_ADDR:
                if (tree->AsLclFld()->GetLclNum() == varNum)
                {
                    IMPL_LIMITATION("replacing GT_LCL_FLD with a constant");
                }
                else
                {
                    copy = new (this, tree->GetOper()) GenTreeLclFld(tree->AsLclFld());
                }
                goto DONE;

            case GT_CLS_VAR_ADDR:
                copy = new (this, GT_CLS_VAR_ADDR) GenTreeClsVar(tree->AsClsVar());
                goto DONE;

            case GT_RET_EXPR:
                // GT_RET_EXPR is unique node, that contains a link to a gtInlineCandidate node,
                // that is part of another statement. We cannot clone both here and cannot
                // create another GT_RET_EXPR that points to the same gtInlineCandidate.
                NO_WAY("Cloning of GT_RET_EXPR node not supported");
                goto DONE;

            case GT_MEMORYBARRIER:
                copy = new (this, GT_MEMORYBARRIER) GenTree(GT_MEMORYBARRIER, TYP_VOID);
                goto DONE;

            case GT_ARGPLACE:
                copy = new (this, GT_ARGPLACE) GenTree(GT_ARGPLACE, tree->GetType());
                goto DONE;

            case GT_FTN_ADDR:
                copy = new (this, oper) GenTreeFptrVal(tree->gtType, tree->AsFptrVal()->gtFptrMethod);

#ifdef FEATURE_READYTORUN_COMPILER
                copy->AsFptrVal()->gtEntryPoint = tree->AsFptrVal()->gtEntryPoint;
#endif
                goto DONE;

            case GT_CATCH_ARG:
            case GT_NO_OP:
            case GT_LABEL:
                copy = new (this, oper) GenTree(oper, tree->gtType);
                goto DONE;

#if !defined(FEATURE_EH_FUNCLETS)
            case GT_END_LFIN:
#endif // !FEATURE_EH_FUNCLETS
            case GT_JMP:
                copy = new (this, oper) GenTreeVal(oper, tree->gtType, tree->AsVal()->gtVal1);
                goto DONE;

            default:
                NO_WAY("Cloning of node not supported");
                goto DONE;
        }
    }

    if (kind & GTK_SMPOP)
    {
        switch (oper)
        {
            case GT_FMOD:
                // This is always converted to a helper call.
                copy = new (this, GT_CALL)
                    GenTreeOp(GT_FMOD, tree->GetType(), tree->AsOp()->GetOp(0), tree->AsOp()->GetOp(1));
                break;

            case GT_CAST:
                copy = new (this, GT_CAST) GenTreeCast(tree->GetType(), tree->AsCast()->GetOp(0), tree->IsUnsigned(),
                                                       tree->AsCast()->GetCastType());
                break;

            case GT_FIELD_ADDR:
                copy = new (this, GT_FIELD_ADDR) GenTreeFieldAddr(tree->AsFieldAddr());
                break;

            case GT_INDEX_ADDR:
                copy = new (this, GT_INDEX_ADDR) GenTreeIndexAddr(tree->AsIndexAddr());
                break;

            case GT_ALLOCOBJ:
            {
                GenTreeAllocObj* asAllocObj = tree->AsAllocObj();
                copy                        = new (this, GT_ALLOCOBJ)
                    GenTreeAllocObj(tree->TypeGet(), asAllocObj->gtNewHelper, asAllocObj->gtHelperHasSideEffects,
                                    asAllocObj->gtAllocObjClsHnd, asAllocObj->gtOp1);
            }
            break;

            case GT_RUNTIMELOOKUP:
            {
                GenTreeRuntimeLookup* asRuntimeLookup = tree->AsRuntimeLookup();

                copy = new (this, GT_RUNTIMELOOKUP)
                    GenTreeRuntimeLookup(asRuntimeLookup->gtHnd, asRuntimeLookup->gtHndType, asRuntimeLookup->gtOp1);
            }
            break;

            case GT_ARR_LENGTH:
                copy = new (this, GT_ARR_LENGTH) GenTreeArrLen(tree->AsArrLen());
                break;

            case GT_ARR_INDEX:
                copy = new (this, GT_ARR_INDEX)
                    GenTreeArrIndex(tree->TypeGet(),
                                    gtCloneExpr(tree->AsArrIndex()->ArrObj(), addFlags, deepVarNum, deepVarVal),
                                    gtCloneExpr(tree->AsArrIndex()->IndexExpr(), addFlags, deepVarNum, deepVarVal),
                                    tree->AsArrIndex()->gtCurrDim, tree->AsArrIndex()->gtArrRank,
                                    tree->AsArrIndex()->gtArrElemType);
                break;

            case GT_OBJ:
                copy = new (this, GT_OBJ) GenTreeObj(tree->AsObj());
                break;

            case GT_STORE_OBJ:
                copy = new (this, GT_STORE_OBJ) GenTreeObj(tree->AsObj());
                break;

            case GT_BLK:
                copy = new (this, GT_BLK) GenTreeBlk(tree->AsBlk());
                break;

            case GT_STORE_BLK:
                copy = new (this, GT_STORE_BLK) GenTreeBlk(tree->AsBlk());
                break;

            case GT_BOX:
                copy = new (this, GT_BOX)
                    GenTreeBox(tree->TypeGet(), tree->AsOp()->gtOp1, tree->AsBox()->gtAsgStmtWhenInlinedBoxValue,
                               tree->AsBox()->gtCopyStmtWhenInlinedBoxValue);
                break;

            case GT_INTRINSIC:
                copy = new (this, GT_INTRINSIC)
                    GenTreeIntrinsic(tree->TypeGet(), tree->AsOp()->gtOp1, tree->AsOp()->gtOp2,
                                     tree->AsIntrinsic()->gtIntrinsicId, tree->AsIntrinsic()->gtIntrinsicName,
                                     tree->AsIntrinsic()->gtMethodHandle);
#ifdef FEATURE_READYTORUN_COMPILER
                copy->AsIntrinsic()->gtEntryPoint = tree->AsIntrinsic()->gtEntryPoint;
#endif
                break;

            case GT_LEA:
            {
                GenTreeAddrMode* addrModeOp = tree->AsAddrMode();
                copy                        = new (this, GT_LEA)
                    GenTreeAddrMode(addrModeOp->TypeGet(), addrModeOp->GetBase(), addrModeOp->GetIndex(),
                                    addrModeOp->GetScale(), static_cast<unsigned>(addrModeOp->GetOffset()));
            }
            break;

            case GT_COPY:
            case GT_RELOAD:
                copy = new (this, oper) GenTreeCopyOrReload(oper, tree->GetType(), tree->AsUnOp()->GetOp(0));
                break;

#ifndef TARGET_64BIT
            case GT_MUL:
            case GT_DIV:
            case GT_MOD:
            case GT_UDIV:
            case GT_UMOD:
                if (tree->TypeIs(TYP_LONG))
                {
                    // LONG multiplication/division usually requires helper calls on 32 bit targets.
                    copy = new (this, GT_CALL) GenTreeOp(oper, tree->GetType(), tree->AsOp()->GetOp(0),
                                                         tree->AsOp()->GetOp(1) DEBUGARG(/*largeNode*/ true));
                    break;
                }
                FALLTHROUGH;
#endif
            default:
                assert(!GenTree::IsExOp(tree->OperKind()) && tree->OperIsSimple());
                // We're in the SimpleOp case, so it's always unary or binary.
                if (GenTree::OperIsUnary(tree->OperGet()))
                {
                    copy = gtNewOperNode(oper, tree->TypeGet(), tree->AsOp()->gtOp1);
                }
                else
                {
                    assert(GenTree::OperIsBinary(tree->OperGet()));
                    copy = gtNewOperNode(oper, tree->TypeGet(), tree->AsOp()->gtOp1, tree->AsOp()->gtOp2);
                }
                break;
        }

        // Some flags are conceptually part of the gtOper, and should be copied immediately.
        if (tree->gtOverflowEx())
        {
            copy->gtFlags |= GTF_OVERFLOW;
        }

        if (tree->AsOp()->gtOp1 != nullptr)
        {
            if (tree->gtOper == GT_ASG)
            {
                // Don't replace varNum if it appears as the LHS of an assign.
                copy->AsOp()->gtOp1 = gtCloneExpr(tree->AsOp()->gtOp1, addFlags, -1, 0, deepVarNum, deepVarVal);
            }
            else
            {
                copy->AsOp()->gtOp1 = gtCloneExpr(tree->AsOp()->gtOp1, addFlags, deepVarNum, deepVarVal);
            }
        }

        if (tree->gtGetOp2IfPresent() != nullptr)
        {
            copy->AsOp()->gtOp2 = gtCloneExpr(tree->AsOp()->gtOp2, addFlags, deepVarNum, deepVarVal);
        }

        /* Flags */
        addFlags |= tree->gtFlags;

        // GTF_NODE_MASK should not be propagated from 'tree' to 'copy'
        INDEBUG(addFlags &= ~GTF_NODE_MASK;)

        // Effects flags propagate upwards.
        if (copy->AsOp()->gtOp1 != nullptr)
        {
            copy->gtFlags |= (copy->AsOp()->gtOp1->gtFlags & GTF_ALL_EFFECT);
        }
        if (copy->gtGetOp2IfPresent() != nullptr)
        {
            copy->gtFlags |= (copy->gtGetOp2()->gtFlags & GTF_ALL_EFFECT);
        }

        goto DONE;
    }

    /* See what kind of a special operator we have here */

    switch (oper)
    {
        case GT_CALL:

            // We can't safely clone calls that have GT_RET_EXPRs via gtCloneExpr.
            // You must use gtCloneCandidateCall for these calls (and then do appropriate other fixup)
            if (tree->AsCall()->IsInlineCandidate() || tree->AsCall()->IsGuardedDevirtualizationCandidate())
            {
                NO_WAY("Cloning of calls with associated GT_RET_EXPR nodes is not supported");
            }

            copy = gtCloneExprCallHelper(tree->AsCall(), addFlags, deepVarNum, deepVarVal);
            break;

        case GT_ARR_ELEM:
        {
            GenTreeArrElem* arrElem = tree->AsArrElem();
            GenTree*        inds[GT_ARR_MAX_RANK];
            for (unsigned dim = 0; dim < arrElem->gtArrRank; dim++)
            {
                inds[dim] = gtCloneExpr(arrElem->gtArrInds[dim], addFlags, deepVarNum, deepVarVal);
            }
            copy = new (this, GT_ARR_ELEM)
                GenTreeArrElem(arrElem->TypeGet(), gtCloneExpr(arrElem->gtArrObj, addFlags, deepVarNum, deepVarVal),
                               arrElem->gtArrRank, arrElem->gtArrElemSize, arrElem->gtArrElemType, &inds[0]);
        }
        break;

        case GT_PHI:
        {
            copy                      = new (this, GT_PHI) GenTreePhi(tree->TypeGet());
            GenTreePhi::Use** prevUse = &copy->AsPhi()->gtUses;
            for (GenTreePhi::Use& use : tree->AsPhi()->Uses())
            {
                *prevUse = new (this, CMK_ASTNode)
                    GenTreePhi::Use(gtCloneExpr(use.GetNode(), addFlags, deepVarNum, deepVarVal), *prevUse);
                prevUse = &((*prevUse)->NextRef());
            }
        }
        break;

        case GT_FIELD_LIST:
            copy = new (this, GT_FIELD_LIST) GenTreeFieldList();
            for (GenTreeFieldList::Use& use : tree->AsFieldList()->Uses())
            {
                copy->AsFieldList()->AddField(this, gtCloneExpr(use.GetNode(), addFlags, deepVarNum, deepVarVal),
                                              use.GetOffset(), use.GetType());
            }
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            copy = new (this, GT_HWINTRINSIC) GenTreeHWIntrinsic(tree->AsHWIntrinsic(), getAllocator(CMK_ASTNode));
            for (unsigned i = 0; i < copy->AsHWIntrinsic()->GetNumOps(); i++)
            {
                copy->AsHWIntrinsic()->SetOp(i, gtCloneExpr(tree->AsHWIntrinsic()->GetOp(i), addFlags, deepVarNum,
                                                            deepVarVal));
            }
            break;
#endif

        case GT_INSTR:
            copy = new (this, GT_INSTR) GenTreeInstr(tree->AsInstr(), this);
            break;

        case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
        case GT_HW_INTRINSIC_CHK:
#endif
            copy = new (this, oper) GenTreeBoundsChk(tree->AsBoundsChk());
            copy->AsBoundsChk()->SetIndex(
                gtCloneExpr(tree->AsBoundsChk()->GetIndex(), addFlags, deepVarNum, deepVarVal));
            copy->AsBoundsChk()->SetLength(
                gtCloneExpr(tree->AsBoundsChk()->GetLength(), addFlags, deepVarNum, deepVarVal));
            break;

        case GT_QMARK:
            copy = new (this, GT_QMARK) GenTreeQmark(tree->AsQmark());
            goto CLONE_TERNARY;
        case GT_ARR_OFFSET:
            copy = new (this, GT_ARR_OFFSET) GenTreeArrOffs(tree->AsArrOffs());
            goto CLONE_TERNARY;
        case GT_CMPXCHG:
            copy = new (this, GT_CMPXCHG) GenTreeCmpXchg(tree->AsCmpXchg());
            goto CLONE_TERNARY;
        case GT_COPY_BLK:
        case GT_INIT_BLK:
            copy = new (this, oper) GenTreeDynBlk(tree->AsDynBlk());
        CLONE_TERNARY:
            for (unsigned i = 0; i < 3; i++)
            {
                copy->AsTernaryOp()->SetOp(i, gtCloneExpr(copy->AsTernaryOp()->GetOp(i), addFlags, deepVarNum,
                                                          deepVarVal));
            }
            break;

        default:
            INDEBUG(gtDispTree(tree);)
            NO_WAY("unexpected operator");
    }

DONE:

    // If it has a zero-offset field seq, copy annotation.
    if (tree->TypeGet() == TYP_BYREF)
    {
        CopyZeroOffsetFieldSeq(tree, copy);
    }

    copy->gtVNPair = tree->gtVNPair; // A cloned tree gets the orginal's Value number pair

    /* Compute the flags for the copied node. Note that we can do this only
       if we didnt gtFoldExpr(copy) */

    if (copy->gtOper == oper)
    {
        addFlags |= tree->gtFlags;

        // GTF_NODE_MASK should not be propagated from 'tree' to 'copy'
        INDEBUG(addFlags &= ~GTF_NODE_MASK;)

        copy->gtFlags |= addFlags;

        // Update side effect flags since they may be different from the source side effect flags.
        // For example, we may have replaced some locals with constants and made indirections non-throwing.
        gtUpdateNodeSideEffects(copy);
    }

    // Non-node debug flags should be propagated from 'tree' to 'copy'
    INDEBUG(copy->gtDebugFlags |= (tree->gtDebugFlags & ~GTF_DEBUG_NODE_MASK);)

    copy->CopyCosts(tree);

    // We don't expect to clone trees after register allocation.
    assert(!tree->HasRegs() || (tree->GetRegNum() == REG_NA));

    return copy;
}

//------------------------------------------------------------------------
// gtCloneExprCallHelper: clone a call tree
//
// Notes:
//    Do not invoke this method directly, instead call either gtCloneExpr
//    or gtCloneCandidateCall, as appropriate.
//
// Arguments:
//    tree - the call to clone
//    addFlags - GTF_* flags to add to the copied tree nodes
//    deepVarNum - lclNum to replace uses of beyond the root, or BAD_VAR_NUM for no replacement
//    deepVarVal - If replacing beyond root, replace `deepVarNum` with IntCns `deepVarVal`
//
// Returns:
//    Cloned copy of call and all subtrees.

GenTreeCall* Compiler::gtCloneExprCallHelper(GenTreeCall* tree,
                                             GenTreeFlags addFlags,
                                             unsigned     deepVarNum,
                                             int          deepVarVal)
{
    GenTreeCall* copy = new (this, GT_CALL) GenTreeCall(tree);

    if (tree->gtCallThisArg != nullptr)
    {
        copy->gtCallThisArg =
            gtNewCallArgs(gtCloneExpr(tree->gtCallThisArg->GetNode(), addFlags, deepVarNum, deepVarVal));
    }

    GenTreeCall::Use** argsTail = &copy->gtCallArgs;
    for (GenTreeCall::Use& use : tree->Args())
    {
        *argsTail = gtNewCallArgs(gtCloneExpr(use.GetNode(), addFlags, deepVarNum, deepVarVal));
        (*argsTail)->SetSigTypeNum(use.GetSigTypeNum());
        argsTail = &((*argsTail)->NextRef());
    }

    argsTail = &copy->gtCallLateArgs;
    for (GenTreeCall::Use& use : tree->LateArgs())
    {
        *argsTail = gtNewCallArgs(gtCloneExpr(use.GetNode(), addFlags, deepVarNum, deepVarVal));
        argsTail  = &((*argsTail)->NextRef());
    }

    // The call sig comes from the EE and doesn't change throughout the compilation process, meaning
    // we only really need one physical copy of it. Therefore a shallow pointer copy will suffice.
    // (Note that this still holds even if the tree we are cloning was created by an inlinee compiler,
    // because the inlinee still uses the inliner's memory allocator anyway.)
    INDEBUG(copy->callSig = tree->callSig;)

    // The tail call info does not change after it is allocated, so for the same reasons as above
    // a shallow copy suffices.
    copy->tailCallInfo = tree->tailCallInfo;

    copy->gtControlExpr = gtCloneExpr(tree->gtControlExpr, addFlags, deepVarNum, deepVarVal);

    /* Copy the union */
    if (tree->gtCallType == CT_INDIRECT)
    {
        copy->gtCallCookie =
            tree->gtCallCookie ? gtCloneExpr(tree->gtCallCookie, addFlags, deepVarNum, deepVarVal) : nullptr;
        copy->gtCallAddr = tree->gtCallAddr ? gtCloneExpr(tree->gtCallAddr, addFlags, deepVarNum, deepVarVal) : nullptr;
    }
    else if (tree->IsVirtualStub())
    {
        copy->gtCallMethHnd      = tree->gtCallMethHnd;
        copy->gtStubCallStubAddr = tree->gtStubCallStubAddr;
    }
    else
    {
        copy->gtCallMethHnd         = tree->gtCallMethHnd;
        copy->gtInlineCandidateInfo = nullptr;
    }

    if (tree->fgArgInfo != nullptr)
    {
        copy->fgArgInfo = new (this, CMK_Unknown) fgArgInfo(this, copy, tree);
    }

#ifdef FEATURE_READYTORUN_COMPILER
    copy->setEntryPoint(tree->gtEntryPoint);
#endif

#if defined(DEBUG) || defined(INLINE_DATA)
    copy->gtInlineObservation = tree->gtInlineObservation;
    copy->gtRawILOffset       = tree->AsCall()->gtRawILOffset;
#endif

    // We keep track of the number of no return calls, so if we've cloned
    // one of these, update the tracking.
    //
    if (tree->IsNoReturn())
    {
        assert(copy->IsNoReturn());
        setMethodHasNoReturnCalls();
    }

    return copy;
}

//------------------------------------------------------------------------
// gtCloneCandidateCall: clone a call that is an inline or guarded
//    devirtualization candidate (~ any call that can have a GT_RET_EXPR)
//
// Notes:
//    If the call really is a candidate, the caller must take additional steps
//    after cloning to re-establish candidate info and the relationship between
//    the candidate and any associated GT_RET_EXPR.
//
// Arguments:
//    call - the call to clone
//
// Returns:
//    Cloned copy of call and all subtrees.

GenTreeCall* Compiler::gtCloneCandidateCall(GenTreeCall* call)
{
    assert(call->IsInlineCandidate() || call->IsGuardedDevirtualizationCandidate());

    GenTreeCall* result = gtCloneExprCallHelper(call);

    // There is some common post-processing in gtCloneExpr that we reproduce
    // here, for the fields that make sense for candidate calls.
    result->gtFlags |= call->gtFlags;

#if defined(DEBUG)
    result->gtDebugFlags |= (call->gtDebugFlags & ~GTF_DEBUG_NODE_MASK);
#endif

    return result;
}

//------------------------------------------------------------------------
// gtUpdateSideEffects: Update the side effects of a tree and its ancestors
//
// Arguments:
//    stmt            - The tree's statement
//    tree            - Tree to update the side effects for
//
// Note: If tree's order hasn't been established, the method updates side effect
//       flags on all statement's nodes.

void Compiler::gtUpdateSideEffects(Statement* stmt, GenTree* tree)
{
    if (fgStmtListThreaded)
    {
        gtUpdateTreeAncestorsSideEffects(tree);
    }
    else
    {
        gtUpdateStmtSideEffects(stmt);
    }
}

//------------------------------------------------------------------------
// gtUpdateTreeAncestorsSideEffects: Update the side effects of a tree and its ancestors
//                                   when statement order has been established.
//
// Arguments:
//    tree            - Tree to update the side effects for
//
void Compiler::gtUpdateTreeAncestorsSideEffects(GenTree* tree)
{
    assert(fgStmtListThreaded);
    while (tree != nullptr)
    {
        gtUpdateNodeSideEffects(tree);
        tree = tree->FindUser();
    }
}

//------------------------------------------------------------------------
// gtUpdateStmtSideEffects: Update the side effects for statement tree nodes.
//
// Arguments:
//    stmt            - The statement to update side effects on
//
void Compiler::gtUpdateStmtSideEffects(Statement* stmt)
{
    fgWalkTree(stmt->GetRootNodePointer(), fgUpdateSideEffectsPre, fgUpdateSideEffectsPost);
}

//------------------------------------------------------------------------
// gtUpdateNodeOperSideEffects: Update the side effects based on the node operation.
//
// Arguments:
//    tree            - Tree to update the side effects on
//
// Notes:
//    This method currently only updates GTF_EXCEPT, GTF_ASG, and GTF_CALL flags.
//    The other side effect flags may remain unnecessarily (conservatively) set.
//    The caller of this method is expected to update the flags based on the children's flags.
//
void Compiler::gtUpdateNodeOperSideEffects(GenTree* tree)
{
    if (tree->OperMayThrow(this))
    {
        tree->gtFlags |= GTF_EXCEPT;
    }
    else
    {
        tree->gtFlags &= ~GTF_EXCEPT;
        if (tree->OperIsIndirOrArrLength())
        {
            tree->SetIndirExceptionFlags(this);
        }
    }

    if (tree->OperRequiresAsgFlag())
    {
        tree->gtFlags |= GTF_ASG;
    }
    else
    {
        tree->gtFlags &= ~GTF_ASG;
    }

    if (tree->OperRequiresCallFlag(this))
    {
        tree->gtFlags |= GTF_CALL;
    }
    else
    {
        tree->gtFlags &= ~GTF_CALL;
    }
}

//------------------------------------------------------------------------
// gtUpdateNodeOperSideEffectsPost: Update the side effects based on the node operation,
// in the post-order visit of a tree walk. It is expected that the pre-order visit cleared
// the bits, so the post-order visit only sets them. This is important for binary nodes
// where one child already may have set the GTF_EXCEPT bit. Note that `SetIndirExceptionFlags`
// looks at its child, which is why we need to do this in a bottom-up walk.
//
// Arguments:
//    tree            - Tree to update the side effects on
//
// Notes:
//    This method currently only updates GTF_ASG, GTF_CALL, and GTF_EXCEPT flags.
//    The other side effect flags may remain unnecessarily (conservatively) set.
//
void Compiler::gtUpdateNodeOperSideEffectsPost(GenTree* tree)
{
    if (tree->OperMayThrow(this))
    {
        tree->gtFlags |= GTF_EXCEPT;
    }

    if (tree->OperRequiresAsgFlag())
    {
        tree->gtFlags |= GTF_ASG;
    }

    if (tree->OperRequiresCallFlag(this))
    {
        tree->gtFlags |= GTF_CALL;
    }
}

//------------------------------------------------------------------------
// gtUpdateNodeSideEffects: Update the side effects based on the node operation and
//                          children's side efects.
//
// Arguments:
//    tree            - Tree to update the side effects on
//
// Notes:
//    This method currently only updates GTF_EXCEPT, GTF_ASG, and GTF_CALL flags.
//    The other side effect flags may remain unnecessarily (conservatively) set.
//
void Compiler::gtUpdateNodeSideEffects(GenTree* tree)
{
    gtUpdateNodeOperSideEffects(tree);
    tree->VisitOperands([tree](GenTree* op) {
        tree->gtFlags |= (op->gtFlags & GTF_ALL_EFFECT);
        return GenTree::VisitResult::Continue;
    });
}

//------------------------------------------------------------------------
// fgUpdateSideEffectsPre: Update the side effects based on the tree operation.
// The pre-visit walk clears GTF_ASG, GTF_CALL, and GTF_EXCEPT; the post-visit walk sets
// the bits as necessary.
//
// Arguments:
//    pTree            - Pointer to the tree to update the side effects
//    fgWalkPre        - Walk data
//
Compiler::fgWalkResult Compiler::fgUpdateSideEffectsPre(GenTree** pTree, fgWalkData* fgWalkPre)
{
    GenTree* tree = *pTree;
    tree->gtFlags &= ~(GTF_ASG | GTF_CALL | GTF_EXCEPT);

    return WALK_CONTINUE;
}

//------------------------------------------------------------------------
// fgUpdateSideEffectsPost: Update the side effects of the node and parent based on the tree's flags.
//
// Arguments:
//    pTree            - Pointer to the tree
//    fgWalkPost       - Walk data
//
// Notes:
//    The routine is used for updating the stale side effect flags for ancestor
//    nodes starting from treeParent up to the top-level stmt expr.
//
Compiler::fgWalkResult Compiler::fgUpdateSideEffectsPost(GenTree** pTree, fgWalkData* fgWalkPost)
{
    GenTree* tree = *pTree;

    // Update the node's side effects first.
    fgWalkPost->compiler->gtUpdateNodeOperSideEffectsPost(tree);

    // If this node is an indir or array length, and it doesn't have the GTF_EXCEPT bit set, we
    // set the GTF_IND_NONFAULTING bit. This needs to be done after all children, and this node, have
    // been processed.
    if (tree->OperIsIndirOrArrLength() && ((tree->gtFlags & GTF_EXCEPT) == 0))
    {
        tree->gtFlags |= GTF_IND_NONFAULTING;
    }

    // Then update the parent's side effects based on this node.
    GenTree* parent = fgWalkPost->parent;
    if (parent != nullptr)
    {
        parent->gtFlags |= (tree->gtFlags & GTF_ALL_EFFECT);
    }
    return WALK_CONTINUE;
}

/*****************************************************************************
 *
 *  Compares two trees and returns true when both trees are the same.
 *  Instead of fully comparing the two trees this method can just return false.
 *  Thus callers should not assume that the trees are different when false is returned.
 *  Only when true is returned can the caller perform code optimizations.
 *  The current implementation only compares a limited set of LEAF/CONST node
 *  and returns false for all othere trees.
 */
bool Compiler::gtCompareTree(GenTree* op1, GenTree* op2)
{
    /* Make sure that both trees are of the same GT node kind */
    if (op1->OperGet() != op2->OperGet())
    {
        return false;
    }

    /* Make sure that both trees are returning the same type */
    if (op1->gtType != op2->gtType)
    {
        return false;
    }

    /* Figure out what kind of a node we have */

    genTreeOps oper = op1->OperGet();
    unsigned   kind = op1->OperKind();

    /* Is this a constant or leaf node? */

    if (kind & GTK_LEAF)
    {
        switch (oper)
        {
            case GT_CNS_INT:
                if ((op1->AsIntCon()->gtIconVal == op2->AsIntCon()->gtIconVal) && GenTree::SameIconHandleFlag(op1, op2))
                {
                    return true;
                }
                break;

            case GT_CNS_LNG:
                if (op1->AsLngCon()->gtLconVal == op2->AsLngCon()->gtLconVal)
                {
                    return true;
                }
                break;

            case GT_CNS_STR:
                if (op1->AsStrCon()->gtSconCPX == op2->AsStrCon()->gtSconCPX)
                {
                    return true;
                }
                break;

            case GT_LCL_VAR:
                if (op1->AsLclVarCommon()->GetLclNum() == op2->AsLclVarCommon()->GetLclNum())
                {
                    return true;
                }
                break;

            case GT_CLS_VAR_ADDR:
                return op1->AsClsVar()->GetFieldHandle() == op2->AsClsVar()->GetFieldHandle();

            default:
                // we return false for these unhandled 'oper' kinds
                break;
        }
    }
    return false;
}

GenTreeUseEdgeIterator::GenTreeUseEdgeIterator()
    : m_advance(nullptr), m_node(nullptr), m_edge(nullptr), m_statePtr(nullptr), m_state(-1)
{
}

GenTreeUseEdgeIterator::GenTreeUseEdgeIterator(GenTree* node)
    : m_advance(nullptr), m_node(node), m_edge(nullptr), m_statePtr(nullptr), m_state(0)
{
    assert(m_node != nullptr);

    // NOTE: the switch statement below must be updated when introducing new nodes.

    switch (m_node->OperGet())
    {
        // Leaf nodes
        case GT_LCL_VAR:
        case GT_LCL_FLD:
        case GT_LCL_VAR_ADDR:
        case GT_LCL_FLD_ADDR:
        case GT_CATCH_ARG:
        case GT_LABEL:
        case GT_FTN_ADDR:
        case GT_RET_EXPR:
        case GT_CNS_INT:
        case GT_CNS_LNG:
        case GT_CNS_DBL:
        case GT_CNS_STR:
        case GT_MEMORYBARRIER:
        case GT_JMP:
        case GT_JCC:
        case GT_SETCC:
        case GT_NO_OP:
        case GT_START_NONGC:
        case GT_START_PREEMPTGC:
        case GT_PROF_HOOK:
#if !defined(FEATURE_EH_FUNCLETS)
        case GT_END_LFIN:
#endif // !FEATURE_EH_FUNCLETS
        case GT_PHI_ARG:
        case GT_JMPTABLE:
        case GT_CLS_VAR_ADDR:
        case GT_ARGPLACE:
        case GT_PHYSREG:
        case GT_EMITNOP:
        case GT_PINVOKE_PROLOG:
        case GT_PINVOKE_EPILOG:
        case GT_IL_OFFSET:
            m_state = -1;
            return;

        // Standard unary operators
        case GT_STORE_LCL_VAR:
        case GT_STORE_LCL_FLD:
        case GT_NOT:
        case GT_NEG:
        case GT_FNEG:
        case GT_COPY:
        case GT_RELOAD:
        case GT_ARR_LENGTH:
        case GT_CAST:
        case GT_BITCAST:
        case GT_CKFINITE:
        case GT_LCLHEAP:
        case GT_FIELD_ADDR:
        case GT_IND:
        case GT_OBJ:
        case GT_BLK:
        case GT_BOX:
        case GT_ALLOCOBJ:
        case GT_RUNTIMELOOKUP:
        case GT_INIT_VAL:
        case GT_JTRUE:
        case GT_SWITCH:
        case GT_NULLCHECK:
        case GT_PUTARG_REG:
        case GT_PUTARG_STK:
        case GT_BSWAP:
        case GT_BSWAP16:
        case GT_KEEPALIVE:
        case GT_INC_SATURATE:
#if FEATURE_ARG_SPLIT
        case GT_PUTARG_SPLIT:
#endif // FEATURE_ARG_SPLIT
        case GT_RETURNTRAP:
#ifdef FEATURE_SIMD
        case GT_SIMD_UPPER_SPILL:
        case GT_SIMD_UPPER_UNSPILL:
#endif
            m_edge = &m_node->AsUnOp()->gtOp1;
            assert(*m_edge != nullptr);
            m_advance = &GenTreeUseEdgeIterator::Terminate;
            return;

        // Unary operators with an optional operand
        case GT_NOP:
        case GT_RETURN:
        case GT_RETFILT:
            if (m_node->AsUnOp()->gtOp1 == nullptr)
            {
                assert(m_node->NullOp1Legal());
                m_state = -1;
            }
            else
            {
                m_edge    = &m_node->AsUnOp()->gtOp1;
                m_advance = &GenTreeUseEdgeIterator::Terminate;
            }
            return;

        // Special nodes
        case GT_FIELD_LIST:
            m_statePtr = m_node->AsFieldList()->Uses().GetHead();
            m_advance  = &GenTreeUseEdgeIterator::AdvanceFieldList;
            AdvanceFieldList();
            return;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            if (m_node->AsHWIntrinsic()->IsBinary() && m_node->IsReverseOp())
            {
                m_edge    = &m_node->AsHWIntrinsic()->GetUse(1).NodeRef();
                m_advance = &GenTreeUseEdgeIterator::AdvanceHWIntrinsicReverseOp;
            }
            else
            {
                m_statePtr = m_node->AsHWIntrinsic()->Uses().begin();
                m_advance  = &GenTreeUseEdgeIterator::AdvanceHWIntrinsic;
                AdvanceHWIntrinsic();
            }
            return;
#endif // FEATURE_HW_INTRINSICS

        case GT_INSTR:
            m_statePtr = m_node->AsInstr()->Uses().begin();
            m_advance  = &GenTreeUseEdgeIterator::AdvanceInstr;
            AdvanceInstr();
            return;

        case GT_PHI:
            m_statePtr = m_node->AsPhi()->gtUses;
            m_advance  = &GenTreeUseEdgeIterator::AdvancePhi;
            AdvancePhi();
            return;

        case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
        case GT_HW_INTRINSIC_CHK:
#endif
            m_edge = &m_node->AsBoundsChk()->gtIndex;
            assert(*m_edge != nullptr);
            m_advance = &GenTreeUseEdgeIterator::AdvanceBoundsChk;
            return;

        case GT_ARR_ELEM:
            m_edge = &m_node->AsArrElem()->gtArrObj;
            assert(*m_edge != nullptr);
            m_advance = &GenTreeUseEdgeIterator::AdvanceArrElem;
            return;

        case GT_ARR_OFFSET:
        case GT_CMPXCHG:
        case GT_COPY_BLK:
        case GT_INIT_BLK:
        case GT_QMARK:
            m_edge = &m_node->AsTernaryOp()->gtOp1;
            assert(*m_edge != nullptr);
            m_advance = &GenTreeUseEdgeIterator::AdvanceTernaryOp;
            return;

        case GT_CALL:
            AdvanceCall<CALL_INSTANCE>();
            return;

        case GT_LEA:
            if (m_node->AsAddrMode()->gtOp1 == nullptr)
            {
                m_edge    = &m_node->AsAddrMode()->gtOp2;
                m_advance = &GenTreeUseEdgeIterator::Terminate;
                return;
            }
            FALLTHROUGH;
        case GT_INTRINSIC:
            assert(m_node->AsOp()->gtOp1 != nullptr);
            if (m_node->AsOp()->gtOp2 == nullptr)
            {
                m_edge    = &m_node->AsOp()->gtOp1;
                m_advance = &GenTreeUseEdgeIterator::Terminate;
                return;
            }
            FALLTHROUGH;
        default:
            SetEntryStateForBinOp();
            return;
    }
}

void GenTreeUseEdgeIterator::AdvanceTernaryOp()
{
    switch (m_state)
    {
        case 0:
            m_edge  = &m_node->AsTernaryOp()->gtOp2;
            m_state = 1;
            break;
        case 1:
            m_edge    = &m_node->AsTernaryOp()->gtOp3;
            m_advance = &GenTreeUseEdgeIterator::Terminate;
            break;
        default:
            unreached();
    }

    assert(*m_edge != nullptr);
}

//------------------------------------------------------------------------
// GenTreeUseEdgeIterator::AdvanceBoundsChk: produces the next operand of a BoundsChk node and advances the state.
//
void GenTreeUseEdgeIterator::AdvanceBoundsChk()
{
    m_edge = &m_node->AsBoundsChk()->gtArrLen;
    assert(*m_edge != nullptr);
    m_advance = &GenTreeUseEdgeIterator::Terminate;
}

//------------------------------------------------------------------------
// GenTreeUseEdgeIterator::AdvanceArrElem: produces the next operand of a ArrElem node and advances the state.
//
// Because these nodes are variadic, this function uses `m_state` to index into the list of array indices.
//
void GenTreeUseEdgeIterator::AdvanceArrElem()
{
    if (m_state < m_node->AsArrElem()->gtArrRank)
    {
        m_edge = &m_node->AsArrElem()->gtArrInds[m_state];
        assert(*m_edge != nullptr);
        m_state++;
    }
    else
    {
        m_state = -1;
    }
}

//------------------------------------------------------------------------
// GenTreeUseEdgeIterator::AdvanceFieldList: produces the next operand of a FieldList node and advances the state.
//
void GenTreeUseEdgeIterator::AdvanceFieldList()
{
    assert(m_state == 0);

    if (m_statePtr == nullptr)
    {
        m_state = -1;
    }
    else
    {
        GenTreeFieldList::Use* currentUse = static_cast<GenTreeFieldList::Use*>(m_statePtr);
        m_edge                            = &currentUse->NodeRef();
        m_statePtr                        = currentUse->GetNext();
    }
}

#ifdef FEATURE_HW_INTRINSICS
//------------------------------------------------------------------------
// GenTreeUseEdgeIterator::AdvanceHWIntrinsic: produces the next operand of a HWINTRINSIC node and advances the state.
//
void GenTreeUseEdgeIterator::AdvanceHWIntrinsic()
{
    assert(m_state == 0);

    if (m_statePtr == m_node->AsHWIntrinsic()->Uses().end())
    {
        m_state = -1;
    }
    else
    {
        GenTreeHWIntrinsic::Use* currentUse = static_cast<GenTreeHWIntrinsic::Use*>(m_statePtr);
        m_edge                              = &currentUse->NodeRef();
        m_statePtr                          = currentUse + 1;
    }
}

void GenTreeUseEdgeIterator::AdvanceHWIntrinsicReverseOp()
{
    assert(m_state == 0);
    assert(m_edge == &m_node->AsHWIntrinsic()->GetUse(1).NodeRef());

    m_edge    = &m_node->AsHWIntrinsic()->GetUse(0).NodeRef();
    m_advance = &GenTreeUseEdgeIterator::Terminate;
}
#endif // FEATURE_HW_INTRINSICS

void GenTreeUseEdgeIterator::AdvanceInstr()
{
    assert(m_state == 0);

    if (m_statePtr == m_node->AsInstr()->Uses().end())
    {
        m_state = -1;
    }
    else
    {
        GenTreeInstr::Use* currentUse = static_cast<GenTreeInstr::Use*>(m_statePtr);
        m_edge                        = &currentUse->NodeRef();
        m_statePtr                    = currentUse + 1;
    }
}

//------------------------------------------------------------------------
// GenTreeUseEdgeIterator::AdvancePhi: produces the next operand of a Phi node and advances the state.
//
void GenTreeUseEdgeIterator::AdvancePhi()
{
    assert(m_state == 0);

    if (m_statePtr == nullptr)
    {
        m_state = -1;
    }
    else
    {
        GenTreePhi::Use* currentUse = static_cast<GenTreePhi::Use*>(m_statePtr);
        m_edge                      = &currentUse->NodeRef();
        m_statePtr                  = currentUse->GetNext();
    }
}

//------------------------------------------------------------------------
// GenTreeUseEdgeIterator::AdvanceBinOp: produces the next operand of a binary node and advances the state.
//
// This function must be instantiated s.t. `ReverseOperands` is `true` iff the node is marked with the
// `GTF_REVERSE_OPS` flag.
//
template <bool ReverseOperands>
void           GenTreeUseEdgeIterator::AdvanceBinOp()
{
    assert(ReverseOperands == ((m_node->gtFlags & GTF_REVERSE_OPS) != 0));

    m_edge = !ReverseOperands ? &m_node->AsOp()->gtOp2 : &m_node->AsOp()->gtOp1;
    assert(*m_edge != nullptr);
    m_advance = &GenTreeUseEdgeIterator::Terminate;
}

//------------------------------------------------------------------------
// GenTreeUseEdgeIterator::SetEntryStateForBinOp: produces the first operand of a binary node and chooses
//                                                the appropriate advance function.
//
void GenTreeUseEdgeIterator::SetEntryStateForBinOp()
{
    assert(m_node->OperIsBinary());

    if (m_node->IsReverseOp())
    {
        m_edge    = &m_node->AsOp()->gtOp2;
        m_advance = &GenTreeUseEdgeIterator::AdvanceBinOp<true>;
    }
    else
    {
        m_edge    = &m_node->AsOp()->gtOp1;
        m_advance = &GenTreeUseEdgeIterator::AdvanceBinOp<false>;
    }
}

//------------------------------------------------------------------------
// GenTreeUseEdgeIterator::AdvanceCall: produces the next operand of a call node and advances the state.
//
// This function is a bit tricky: in order to avoid doing unnecessary work, it is instantiated with the
// state number the iterator will be in when it is called. For example, `AdvanceCall<CALL_INSTANCE>`
// is the instantiation used when the iterator is at the `CALL_INSTANCE` state (i.e. the entry state).
// This sort of templating allows each state to avoid processing earlier states without unnecessary
// duplication of code.
//
// Note that this method expands the argument lists (`gtCallArgs` and `gtCallLateArgs`) into their
// component operands.
//
template <int state>
void          GenTreeUseEdgeIterator::AdvanceCall()
{
    GenTreeCall* const call = m_node->AsCall();

    switch (state)
    {
        case CALL_INSTANCE:
            m_statePtr = call->gtCallArgs;
            m_advance  = &GenTreeUseEdgeIterator::AdvanceCall<CALL_ARGS>;
            if (call->gtCallThisArg != nullptr)
            {
                m_edge = &call->gtCallThisArg->NodeRef();
                return;
            }
            FALLTHROUGH;

        case CALL_ARGS:
            if (m_statePtr != nullptr)
            {
                GenTreeCall::Use* use = static_cast<GenTreeCall::Use*>(m_statePtr);
                m_edge                = &use->NodeRef();
                m_statePtr            = use->GetNext();
                return;
            }
            m_statePtr = call->gtCallLateArgs;
            m_advance  = &GenTreeUseEdgeIterator::AdvanceCall<CALL_LATE_ARGS>;
            FALLTHROUGH;

        case CALL_LATE_ARGS:
            if (m_statePtr != nullptr)
            {
                GenTreeCall::Use* use = static_cast<GenTreeCall::Use*>(m_statePtr);
                m_edge                = &use->NodeRef();
                m_statePtr            = use->GetNext();
                return;
            }
            m_advance = &GenTreeUseEdgeIterator::AdvanceCall<CALL_CONTROL_EXPR>;
            FALLTHROUGH;

        case CALL_CONTROL_EXPR:
            if (call->gtControlExpr != nullptr)
            {
                if (call->gtCallType == CT_INDIRECT)
                {
                    m_advance = &GenTreeUseEdgeIterator::AdvanceCall<CALL_COOKIE>;
                }
                else
                {
                    m_advance = &GenTreeUseEdgeIterator::Terminate;
                }
                m_edge = &call->gtControlExpr;
                return;
            }
            else if (call->gtCallType != CT_INDIRECT)
            {
                m_state = -1;
                return;
            }
            FALLTHROUGH;

        case CALL_COOKIE:
            assert(call->gtCallType == CT_INDIRECT);

            m_advance = &GenTreeUseEdgeIterator::AdvanceCall<CALL_ADDRESS>;
            if (call->gtCallCookie != nullptr)
            {
                m_edge = &call->gtCallCookie;
                return;
            }
            FALLTHROUGH;

        case CALL_ADDRESS:
            assert(call->gtCallType == CT_INDIRECT);

            m_advance = &GenTreeUseEdgeIterator::Terminate;
            if (call->gtCallAddr != nullptr)
            {
                m_edge = &call->gtCallAddr;
            }
            return;

        default:
            unreached();
    }
}

//------------------------------------------------------------------------
// GenTreeUseEdgeIterator::Terminate: advances the iterator to the terminal state.
//
void GenTreeUseEdgeIterator::Terminate()
{
    m_state = -1;
}

//------------------------------------------------------------------------
// GenTreeUseEdgeIterator::operator++: advances the iterator to the next operand.
//
GenTreeUseEdgeIterator& GenTreeUseEdgeIterator::operator++()
{
    // If we've reached the terminal state, do nothing.
    if (m_state != -1)
    {
        (this->*m_advance)();
    }

    return *this;
}

GenTreeUseEdgeIterator GenTree::UseEdgesBegin()
{
    return GenTreeUseEdgeIterator(this);
}

GenTreeUseEdgeIterator GenTree::UseEdgesEnd()
{
    return GenTreeUseEdgeIterator();
}

IteratorPair<GenTreeUseEdgeIterator> GenTree::UseEdges()
{
    return MakeIteratorPair(UseEdgesBegin(), UseEdgesEnd());
}

GenTreeOperandIterator GenTree::OperandsBegin()
{
    return GenTreeOperandIterator(this);
}

GenTreeOperandIterator GenTree::OperandsEnd()
{
    return GenTreeOperandIterator();
}

IteratorPair<GenTreeOperandIterator> GenTree::Operands()
{
    return MakeIteratorPair(OperandsBegin(), OperandsEnd());
}

bool GenTree::Precedes(GenTree* other)
{
    assert(other != nullptr);

    for (GenTree* node = gtNext; node != nullptr; node = node->gtNext)
    {
        if (node == other)
        {
            return true;
        }
    }

    return false;
}

//------------------------------------------------------------------------------
// SetIndirExceptionFlags : Set GTF_EXCEPT and GTF_IND_NONFAULTING flags as appropriate
//                          on an indirection or an array length node.
//
// Arguments:
//    comp  - compiler instance
//
void GenTree::SetIndirExceptionFlags(Compiler* comp)
{
    assert(OperIsIndirOrArrLength());

    if (OperMayThrow(comp))
    {
        gtFlags |= GTF_EXCEPT;
        return;
    }

    GenTree* addr = nullptr;
    if (OperIsIndir())
    {
        addr = AsIndir()->GetAddr();
    }
    else
    {
        assert(gtOper == GT_ARR_LENGTH);
        addr = AsArrLen()->GetArray();
    }

    if ((addr->gtFlags & GTF_EXCEPT) != 0)
    {
        gtFlags |= GTF_EXCEPT;
    }
    else
    {
        gtFlags &= ~GTF_EXCEPT;
        gtFlags |= GTF_IND_NONFAULTING;
    }
}

#ifdef DEBUG

int Compiler::gtDispFlags(GenTreeFlags flags, GenTreeDebugFlags debugFlags)
{
    // clang-format off
    return printf("%c%c%c%c%c%c%c%c%c%c%c",
        (flags & GTF_ASG) ? 'A' : '-',
        (flags & GTF_CALL) ? 'C' : '-',
        (flags & GTF_EXCEPT) ? 'X' : '-',
        (flags & GTF_GLOB_REF) ? 'G' : '-',
        (flags & GTF_ORDER_SIDEEFF) ? 'O' : '-',
        (flags & GTF_SET_FLAGS) ? 'S' : (flags & GTF_USE_FLAGS) ? 'U' : '-',
        (flags & GTF_DONT_CSE) ? 'N' : (flags & GTF_MAKE_CSE) ? 'H' : '-',
        (flags & GTF_REVERSE_OPS) ? 'R' : '-',
        (flags & GTF_UNSIGNED) ? 'U' : (flags & GTF_BOOLEAN) ? 'B' : '-',
        (flags & GTF_CONTAINED) ? 'c' : '-',
        (debugFlags & GTF_DEBUG_NODE_MORPHED) ? '+' : '-');
    // clang-format on
}

/*****************************************************************************/

void Compiler::gtDispNodeName(GenTree* tree)
{
    /* print the node name */

    const char* name;

    assert(tree);
    if (tree->gtOper < GT_COUNT)
    {
        name = GenTree::OpName(tree->OperGet());
    }
    else
    {
        name = "<ERROR>";
    }
    char  buf[32];
    char* bufp = &buf[0];

    if (tree->gtOper == GT_CALL)
    {
        const char* callType = "CALL";
        const char* gtfType  = "";
        const char* ctType   = "";
        char        gtfTypeBuf[100];

        if (tree->AsCall()->gtCallType == CT_USER_FUNC)
        {
            if (tree->AsCall()->IsVirtual())
            {
                callType = "CALLV";
            }
        }
        else if (tree->AsCall()->gtCallType == CT_HELPER)
        {
            ctType = " help";
        }
        else if (tree->AsCall()->gtCallType == CT_INDIRECT)
        {
            ctType = " ind";
        }
        else
        {
            assert(!"Unknown gtCallType");
        }

        if (tree->gtFlags & GTF_CALL_NULLCHECK)
        {
            gtfType = " nullcheck";
        }
        if (tree->AsCall()->IsVirtualVtable())
        {
            gtfType = " vt-ind";
        }
        else if (tree->AsCall()->IsVirtualStub())
        {
            gtfType = " stub";
        }
#ifdef FEATURE_READYTORUN_COMPILER
        else if (tree->AsCall()->IsR2RRelativeIndir())
        {
            gtfType = " r2r_ind";
        }
#endif // FEATURE_READYTORUN_COMPILER
        else if (tree->gtFlags & GTF_CALL_UNMANAGED)
        {
            char* gtfTypeBufWalk = gtfTypeBuf;
            gtfTypeBufWalk += SimpleSprintf_s(gtfTypeBufWalk, gtfTypeBuf, sizeof(gtfTypeBuf), " unman");
            if (tree->gtFlags & GTF_CALL_POP_ARGS)
            {
                gtfTypeBufWalk += SimpleSprintf_s(gtfTypeBufWalk, gtfTypeBuf, sizeof(gtfTypeBuf), " popargs");
            }
            if (tree->AsCall()->gtCallMoreFlags & GTF_CALL_M_UNMGD_THISCALL)
            {
                gtfTypeBufWalk += SimpleSprintf_s(gtfTypeBufWalk, gtfTypeBuf, sizeof(gtfTypeBuf), " thiscall");
            }
            gtfType = gtfTypeBuf;
        }

        sprintf_s(bufp, sizeof(buf), "%s%s%s%c", callType, ctType, gtfType, 0);
    }
    else if (tree->gtOper == GT_ARR_ELEM)
    {
        bufp += SimpleSprintf_s(bufp, buf, sizeof(buf), "%s[", name);
        for (unsigned rank = tree->AsArrElem()->gtArrRank - 1; rank; rank--)
        {
            bufp += SimpleSprintf_s(bufp, buf, sizeof(buf), ",");
        }
        SimpleSprintf_s(bufp, buf, sizeof(buf), "]");
    }
    else if (tree->gtOper == GT_ARR_OFFSET || tree->gtOper == GT_ARR_INDEX)
    {
        bufp += SimpleSprintf_s(bufp, buf, sizeof(buf), "%s[", name);
        unsigned char currDim;
        unsigned char rank;
        if (tree->gtOper == GT_ARR_OFFSET)
        {
            currDim = tree->AsArrOffs()->gtCurrDim;
            rank    = tree->AsArrOffs()->gtArrRank;
        }
        else
        {
            currDim = tree->AsArrIndex()->gtCurrDim;
            rank    = tree->AsArrIndex()->gtArrRank;
        }

        for (unsigned char dim = 0; dim < rank; dim++)
        {
            // Use a defacto standard i,j,k for the dimensions.
            // Note that we only support up to rank 3 arrays with these nodes, so we won't run out of characters.
            char dimChar = '*';
            if (dim == currDim)
            {
                dimChar = 'i' + dim;
            }
            else if (dim > currDim)
            {
                dimChar = ' ';
            }

            bufp += SimpleSprintf_s(bufp, buf, sizeof(buf), "%c", dimChar);
            if (dim != rank - 1)
            {
                bufp += SimpleSprintf_s(bufp, buf, sizeof(buf), ",");
            }
        }
        SimpleSprintf_s(bufp, buf, sizeof(buf), "]");
    }
    else if (GenTreeAddrMode* lea = tree->IsAddrMode())
    {
        bufp += SimpleSprintf_s(bufp, buf, sizeof(buf), "%s(", name);
        if (lea->GetBase() != nullptr)
        {
            bufp += SimpleSprintf_s(bufp, buf, sizeof(buf), "b+");
        }
        if (lea->GetIndex() != nullptr)
        {
            bufp += SimpleSprintf_s(bufp, buf, sizeof(buf), "(i*%d)+", lea->GetScale());
        }
        bufp += SimpleSprintf_s(bufp, buf, sizeof(buf), "%d)", lea->GetOffset());
    }
    else if (tree->gtOper == GT_ARR_BOUNDS_CHECK)
    {
        switch (tree->AsBoundsChk()->GetThrowKind())
        {
            case SCK_RNGCHK_FAIL:
            {
                bufp += SimpleSprintf_s(bufp, buf, sizeof(buf), "%s_Rng", name);
                if (tree->AsBoundsChk()->GetThrowBlock() != nullptr)
                {
                    bufp += SimpleSprintf_s(bufp, buf, sizeof(buf), " -> " FMT_BB,
                                            tree->AsBoundsChk()->GetThrowBlock()->bbNum);
                }
                break;
            }
            case SCK_ARG_EXCPN:
                sprintf_s(bufp, sizeof(buf), "%s_Arg", name);
                break;
            case SCK_ARG_RNG_EXCPN:
                sprintf_s(bufp, sizeof(buf), "%s_ArgRng", name);
                break;
            default:
                unreached();
        }
    }
    else if (tree->gtOverflowEx())
    {
        sprintf_s(bufp, sizeof(buf), "%s_ovfl%c", name, 0);
    }
    else
    {
        sprintf_s(bufp, sizeof(buf), "%s%c", name, 0);
    }

    if (strlen(buf) < 10)
    {
        printf(" %-10s", buf);
    }
    else
    {
        printf(" %s", buf);
    }
}

void Compiler::gtDispZeroFieldSeq(GenTree* tree)
{
    if (FieldSeqNode* fieldSeq = GetZeroOffsetFieldSeq(tree))
    {
        printf(" ZeroFseq(");
        dmpFieldSeqFields(fieldSeq);
        printf(")");
    }
}

//------------------------------------------------------------------------
// gtDispVN: Utility function that prints a tree's ValueNumber: gtVNPair
//
void Compiler::gtDispVN(GenTree* tree)
{
    if (tree->gtVNPair.GetLiberal() != ValueNumStore::NoVN)
    {
        printf(" ");
        vnpPrint(tree->gtVNPair, 0);
    }
}

//------------------------------------------------------------------------
// gtDispCommonEndLine
//     Utility function that prints the following node information
//       1: The associated zero field sequence (if any)
//       2. The register assigned to this node (if any)
//       2. The value number assigned (if any)
//       3. A newline character
//
void Compiler::gtDispCommonEndLine(GenTree* tree)
{
    gtDispZeroFieldSeq(tree);

    if (!compRationalIRForm)
    {
        gtDispVN(tree);
    }
    else
    {
        dmpNodeOperands(tree);
    }

    printf("\n");
}

// Display the sequence, costs and flags portion of the node dump.
void Compiler::gtDispNodeHeader(GenTree* tree)
{
    printf("[%06u] ", tree->GetID());

    dmpNodeFlags(tree);

    if (tree->gtSeqNum != 0)
    {
        printf(" N%04u", tree->gtSeqNum);
    }
    else
    {
        printf("      ");
    }

    if (!compRationalIRForm)
    {
        if (tree->HasCosts())
        {
            printf(" (%3u,%3u)", tree->GetCostEx(), tree->GetCostSz());
        }
        else
        {
            printf("          ");
        }
    }

    if (optValnumCSE_phase)
    {
        if (IS_CSE_INDEX(tree->gtCSEnum))
        {
            printf(" " FMT_CSE " (%s)", GET_CSE_INDEX(tree->gtCSEnum), (IS_CSE_USE(tree->gtCSEnum) ? "use" : "def"));
        }
        else
        {
            printf("              ");
        }
    }
}

int Compiler::dmpNodeFlags(GenTree* tree)
{
    GenTreeFlags flags = tree->gtFlags;

    if (tree->OperIs(GT_ADD, GT_LSH, GT_MUL, GT_COMMA) && ((flags & GTF_ADDRMODE_NO_CSE) != 0))
    {
        // Force the GTF_ADDRMODE_NO_CSE flag to print out like GTF_DONT_CSE
        flags |= GTF_DONT_CSE;
    }

    if (!tree->OperIsBinary())
    {
        // the GTF_REVERSE flag only applies to binary operations
        flags &= ~GTF_REVERSE_OPS;
    }

    char operFlag = '-';

    switch (tree->GetOper())
    {
        case GT_IND:
        case GT_BLK:
        case GT_OBJ:
        case GT_STOREIND:
        case GT_STORE_BLK:
        case GT_STORE_OBJ:
            if (flags & GTF_IND_VOLATILE)
            {
                operFlag = 'V';
            }
            else if (flags & GTF_IND_UNALIGNED)
            {
                operFlag = 'U';
            }
            else if (flags & GTF_IND_TGT_HEAP)
            {
                operFlag = 'h';
            }
            else if (flags & GTF_IND_TGT_NOT_HEAP)
            {
                operFlag = 'l';
            }
            else if (flags & GTF_IND_INVARIANT)
            {
                operFlag = '#';
            }
            else if (flags & GTF_IND_NONFAULTING)
            {
                operFlag = 'n';
            }
            else if (flags & GTF_IND_ASG_LHS)
            {
                operFlag = 'D';
            }
            else if (flags & GTF_IND_NONNULL)
            {
                operFlag = '@';
            }
            break;

        case GT_COPY_BLK:
        case GT_INIT_BLK:
            if (tree->AsDynBlk()->IsVolatile())
            {
                operFlag = 'V';
            }
            else if (tree->AsDynBlk()->IsUnaligned())
            {
                operFlag = 'U';
            }
            break;

        case GT_CALL:
            if (tree->AsCall()->IsInlineCandidate())
            {
                if (tree->AsCall()->IsGuardedDevirtualizationCandidate())
                {
                    operFlag = '&';
                }
                else
                {
                    operFlag = 'I';
                }
            }
            else if (tree->AsCall()->IsGuardedDevirtualizationCandidate())
            {
                operFlag = 'G';
            }
            else if (tree->AsCall()->gtCallMoreFlags & GTF_CALL_M_RETBUFFARG)
            {
                operFlag = 'S';
            }
            else if (flags & GTF_CALL_HOISTABLE)
            {
                operFlag = 'H';
            }
            break;

        case GT_DIV:
        case GT_MOD:
        case GT_UDIV:
        case GT_UMOD:
            if (flags & GTF_DIV_BY_CNS_OPT)
            {
                operFlag = 'M';
            }
            break;

        case GT_STORE_LCL_VAR:
            if (tree->AsLclVar()->IsMultiReg())
            {
                operFlag = 'M';
                break;
            }
            FALLTHROUGH;
        case GT_LCL_VAR:
        case GT_LCL_FLD:
        case GT_STORE_LCL_FLD:
            if (flags & GTF_VAR_USEASG)
            {
                operFlag = 'U';
            }
            else if (flags & GTF_VAR_DEF)
            {
                operFlag = 'D';
            }
            else if (flags & GTF_VAR_CONTEXT)
            {
                operFlag = '!';
            }
            break;

        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GE:
        case GT_GT:
        case GT_TEST_EQ:
        case GT_TEST_NE:
            if (flags & GTF_RELOP_NAN_UN)
            {
                operFlag = 'N';
            }
            else if (flags & GTF_RELOP_JMP_USED)
            {
                operFlag = 'J';
            }
            break;

        case GT_JCMP:
            if (flags & GTF_JCMP_TST)
            {
                operFlag = (flags & GTF_JCMP_EQ) ? 'C' : 'T';
            }
            else
            {
                operFlag = (flags & GTF_JCMP_EQ) ? 'E' : 'N';
            }
            break;

        case GT_CNS_INT:
            if (tree->IsIconHandle())
            {
                operFlag = (flags & GTF_ICON_INITCLASS) ? 'I' : 'H';
            }
            break;

        default:
            break;
    }

    int length = printf("%c", operFlag);
    length += gtDispFlags(flags, tree->gtDebugFlags);
    return length;
}

//------------------------------------------------------------------------
// gtDispNode: Print a tree to jitstdout.
//
// Arguments:
//    tree - the tree to be printed
//    indentStack - the specification for the current level of indentation & arcs
//    msg         - a contextual method (i.e. from the parent) to print
//
// Return Value:
//    None.
//
// Notes:
//    'indentStack' may be null, in which case no indentation or arcs are printed
//    'msg' may be null
//
void Compiler::gtDispNode(GenTree* tree, IndentStack* indentStack, __in __in_z __in_opt const char* msg, bool isLIR)
{
    gtDispNodeHeader(tree);

    if (isLIR)
    {
        assert(msg == nullptr);
        dmpNodeRegs(tree);
    }
    else
    {
        printf(" %-*s", 13, msg == nullptr ? "" : msg);
        printIndent(indentStack);
    }

    gtDispNodeName(tree);

    if (!varTypeIsStruct(tree->GetType()))
    {
        ClassLayout* layout = nullptr;

        if (GenTreeFieldAddr* field = tree->IsFieldAddr())
        {
            layout = field->GetLayout(this);
        }
        else if (GenTreeIndexAddr* index = tree->IsIndexAddr())
        {
            layout = index->GetLayout(this);
        }

        if (layout != nullptr)
        {
            printf(" %s<%s>", varTypeName(tree->GetType()), layout->GetClassName());
        }
        else if (tree->IsILOffset())
        {
            printf(" ");
        }
        else
        {
            printf(" %-6s", varTypeName(tree->GetType()));
        }
    }
    else
    {
        ClassLayout* layout = nullptr;

        if (tree->OperIs(GT_BLK, GT_OBJ, GT_STORE_BLK, GT_STORE_OBJ))
        {
            layout = tree->AsBlk()->GetLayout();
        }
        else if (tree->OperIs(GT_LCL_VAR, GT_STORE_LCL_VAR))
        {
            LclVarDsc* varDsc = lvaGetDesc(tree->AsLclVar());

            if (varTypeIsStruct(varDsc->GetType()))
            {
                layout = varDsc->GetLayout();
            }
        }
        else if (GenTreeLclFld* lclFld = tree->IsLclFld())
        {
            layout = tree->AsLclFld()->GetLayout(this);
        }
        else if (GenTreeCall* call = tree->IsCall())
        {
            layout = call->GetRetLayout();
        }
        else if (GenTreeRetExpr* retExpr = tree->IsRetExpr())
        {
            layout = retExpr->GetCall()->GetRetLayout();
        }

        printf(" %s", varTypeName(tree->GetType()));

        if (layout != nullptr)
        {
            gtDispClassLayout(layout, tree->GetType());
        }
    }

    if (tree->gtOper == GT_RUNTIMELOOKUP)
    {
#ifdef TARGET_64BIT
        printf(" 0x%llx", dspPtr(tree->AsRuntimeLookup()->gtHnd));
#else
        printf(" 0x%x", dspPtr(tree->AsRuntimeLookup()->gtHnd));
#endif

        switch (tree->AsRuntimeLookup()->gtHndType)
        {
            case CORINFO_HANDLETYPE_CLASS:
                printf(" class");
                break;
            case CORINFO_HANDLETYPE_METHOD:
                printf(" method");
                break;
            case CORINFO_HANDLETYPE_FIELD:
                printf(" field");
                break;
            default:
                printf(" unknown");
                break;
        }
    }

    // for tracking down problems in reguse prediction or liveness tracking

    if (verbose && 0)
    {
        printf(" RR=");
        dspRegMask(tree->gtRsvdRegs);
        printf("\n");
    }
}

void Compiler::dmpNodeRegs(GenTree* node)
{
    char   message[256];
    char*  buffer     = message;
    size_t bufferSize = sizeof(message);

    const char* prefix = "";

    if (node->IsValue())
    {
        int size = sprintf_s(buffer, bufferSize, "%st%u", prefix, node->GetID());
        buffer += size;
        bufferSize -= size;
        prefix = ":";
    }

    if (!node->isContained() && !node->TypeIs(TYP_VOID) && node->gtHasReg())
    {
        unsigned count = 1;

#if FEATURE_MULTIREG_RET
        if (node->IsMultiRegNode())
        {
            count = node->GetMultiRegCount(this);
        }
#endif

        for (unsigned i = 0; i < count; i++)
        {
            int size = sprintf_s(buffer, bufferSize, "%s%s%s%s", prefix, getRegName(node->GetRegNum(i)),
                                 node->IsRegSpill(i) ? "$" : "", node->IsRegSpilled(i) ? "#" : "");
            buffer += size;
            bufferSize -= size;
            prefix = " ";
        }
    }

    strcpy_s(buffer, bufferSize, " =");
    printf(" %+*s", 6 + MAX_MULTIREG_COUNT * 6, message);
}

// We usually/commonly don't expect to print anything longer than this string,
#define LONGEST_COMMON_LCL_VAR_DISPLAY "V99 PInvokeFrame"
#define LONGEST_COMMON_LCL_VAR_DISPLAY_LENGTH (sizeof(LONGEST_COMMON_LCL_VAR_DISPLAY))
#define BUF_SIZE (LONGEST_COMMON_LCL_VAR_DISPLAY_LENGTH * 2)

void Compiler::gtGetLclVarNameInfo(unsigned lclNum, const char** ilKindOut, const char** ilNameOut, unsigned* ilNumOut)
{
    const char* ilKind = nullptr;
    const char* ilName = nullptr;

    unsigned ilNum = compMap2ILvarNum(lclNum);

    if (ilNum == (unsigned)ICorDebugInfo::RETBUF_ILNUM)
    {
        ilName = "RetBuf";
    }
    else if (ilNum == (unsigned)ICorDebugInfo::VARARGS_HND_ILNUM)
    {
        ilName = "VarArgHandle";
    }
    else if (ilNum == (unsigned)ICorDebugInfo::TYPECTXT_ILNUM)
    {
        ilName = "TypeCtx";
    }
    else if (ilNum == (unsigned)ICorDebugInfo::UNKNOWN_ILNUM)
    {
        if (lclNumIsTrueCSE(lclNum))
        {
            ilKind = "cse";
            ilNum  = lclNum - optCSEstart;
        }
        else if (lclNum >= optCSEstart)
        {
            // Currently any new LclVar's introduced after the CSE phase
            // are believed to be created by the "rationalizer" that is what is meant by the "rat" prefix.
            ilKind = "rat";
            ilNum  = lclNum - (optCSEstart + optCSEcount);
        }
        else
        {
            if (lclNum == lvaPInvokeFrameListVar)
            {
                ilName = "FrameList";
            }
            else if (lclNum == lvaInlinedPInvokeFrameVar)
            {
                ilName = "PInvokeFrame";
            }
            else if (lclNum == lvaReversePInvokeFrameVar)
            {
                ilName = "ReversePInvokeFrame";
            }
            else if (lclNum == lvaGSSecurityCookie)
            {
                ilName = "GsCookie";
            }
            else if (lclNum == lvaRetAddrVar)
            {
                ilName = "ReturnAddress";
            }
            else if (lclNum == genReturnLocal)
            {
                ilName = "MergedReturn";
            }
            else if (lclNum == lvaStubArgumentVar)
            {
                ilName = "StubParam";
            }
#if FEATURE_FIXED_OUT_ARGS
            else if (lclNum == lvaOutgoingArgSpaceVar)
            {
                ilName = "OutArgs";
            }
#endif // FEATURE_FIXED_OUT_ARGS
#if !defined(FEATURE_EH_FUNCLETS)
            else if (lclNum == lvaShadowSPslotsVar)
            {
                ilName = "EHSlots";
            }
#endif // !FEATURE_EH_FUNCLETS
#ifdef JIT32_GCENCODER
            else if (lclNum == lvaLocAllocSPvar)
            {
                ilName = "LocAllocSP";
            }
#endif // JIT32_GCENCODER
#if defined(FEATURE_EH_FUNCLETS)
            else if (lclNum == lvaPSPSym)
            {
                ilName = "PSPSym";
            }
#endif // FEATURE_EH_FUNCLETS
            else
            {
                ilKind = "tmp";
                if (compIsForInlining())
                {
                    ilNum = lclNum - impInlineInfo->InlinerCompiler->info.compLocalsCount;
                }
                else
                {
                    ilNum = lclNum - info.compLocalsCount;
                }
            }
        }
    }
    else if (lclNum < (compIsForInlining() ? impInlineInfo->InlinerCompiler->info.compArgsCount : info.compArgsCount))
    {
        if (ilNum == 0 && !info.compIsStatic)
        {
            ilName = "this";
        }
        else
        {
            ilKind = "arg";
        }
    }
    else
    {
        if (!lvaTable[lclNum].lvIsStructField)
        {
            ilKind = "loc";
        }
        if (compIsForInlining())
        {
            ilNum -= impInlineInfo->InlinerCompiler->info.compILargsCount;
        }
        else
        {
            ilNum -= info.compILargsCount;
        }
    }

    *ilKindOut = ilKind;
    *ilNameOut = ilName;
    *ilNumOut  = ilNum;
}

/*****************************************************************************/
int Compiler::gtGetLclVarName(unsigned lclNum, char* buf, unsigned buf_remaining)
{
    char*    bufp_next    = buf;
    unsigned charsPrinted = 0;
    int      sprintf_result;

    sprintf_result = sprintf_s(bufp_next, buf_remaining, "V%02u", lclNum);

    if (sprintf_result < 0)
    {
        return sprintf_result;
    }

    charsPrinted += sprintf_result;
    bufp_next += sprintf_result;
    buf_remaining -= sprintf_result;

    const char* ilKind = nullptr;
    const char* ilName = nullptr;
    unsigned    ilNum  = 0;

    gtGetLclVarNameInfo(lclNum, &ilKind, &ilName, &ilNum);

    if (ilName != nullptr)
    {
        sprintf_result = sprintf_s(bufp_next, buf_remaining, " %s", ilName);
        if (sprintf_result < 0)
        {
            return sprintf_result;
        }
        charsPrinted += sprintf_result;
        bufp_next += sprintf_result;
        buf_remaining -= sprintf_result;
    }
    else if (ilKind != nullptr)
    {
        sprintf_result = sprintf_s(bufp_next, buf_remaining, " %s%d", ilKind, ilNum);
        if (sprintf_result < 0)
        {
            return sprintf_result;
        }
        charsPrinted += sprintf_result;
        bufp_next += sprintf_result;
        buf_remaining -= sprintf_result;
    }

    assert(charsPrinted > 0);
    assert(buf_remaining > 0);

    return (int)charsPrinted;
}

int Compiler::dmpLclName(unsigned lclNum)
{
    const char* kind = nullptr;
    const char* name = nullptr;
    unsigned    num  = 0;

    gtGetLclVarNameInfo(lclNum, &kind, &name, &num);

    if (name != nullptr)
    {
        return printf("\"%s\"", name);
    }

    if ((kind != nullptr) && (strcmp(kind, "tmp") != 0))
    {
        return printf("\"%s%d\"", kind, num);
    }

    return 0;
}

/*****************************************************************************
 * Get the local var name, and create a copy of the string that can be used in debug output.
 */
char* Compiler::gtGetLclVarName(unsigned lclNum)
{
    char buf[BUF_SIZE];
    int  charsPrinted = gtGetLclVarName(lclNum, buf, _countof(buf));
    if (charsPrinted < 0)
    {
        return nullptr;
    }

    char* retBuf = new (this, CMK_DebugOnly) char[charsPrinted + 1];
    strcpy_s(retBuf, charsPrinted + 1, buf);
    return retBuf;
}

/*****************************************************************************/
void Compiler::gtDispLclVar(unsigned lclNum, bool padForBiggestDisp)
{
    char buf[BUF_SIZE];
    int  charsPrinted = gtGetLclVarName(lclNum, buf, _countof(buf));

    if (charsPrinted < 0)
    {
        return;
    }

    printf("%s", buf);

    if (padForBiggestDisp && (charsPrinted < (int)LONGEST_COMMON_LCL_VAR_DISPLAY_LENGTH))
    {
        printf("%*c", LONGEST_COMMON_LCL_VAR_DISPLAY_LENGTH - charsPrinted, ' ');
    }
}

//------------------------------------------------------------------------
// gtDispLclVarStructType: Print size and type information about a struct or lclBlk local variable.
//
// Arguments:
//   lclNum - The local var id.
//
void Compiler::gtDispLclVarStructType(unsigned lclNum)
{
    LclVarDsc* varDsc = lvaGetDesc(lclNum);
    var_types  type   = varDsc->TypeGet();
    if (type == TYP_STRUCT)
    {
        ClassLayout* layout = varDsc->GetLayout();
        assert(layout != nullptr);
        gtDispClassLayout(layout, type);
    }
#if FEATURE_FIXED_OUT_ARGS
    else if (lclNum == lvaOutgoingArgSpaceVar)
    {
        if (lvaOutgoingArgSpaceSize.HasFinalValue())
        {
            printf("<%u> ", lvaOutgoingArgSpaceSize.GetValue());
        }
        else
        {
            printf("<na> ");
        }
    }
#endif // FEATURE_FIXED_OUT_ARGS
}

//------------------------------------------------------------------------
// gtDispClassLayout: Print size and type information about a layout.
//
// Arguments:
//   layout - the layout;
//   type   - variable type, used to avoid printing size for SIMD nodes.
//
void Compiler::gtDispClassLayout(ClassLayout* layout, var_types type)
{
    assert(layout != nullptr);
    if (layout->IsBlockLayout())
    {
        printf("<%u>", layout->GetSize());
    }
    else if (varTypeIsSIMD(type))
    {
        printf("<%s>", layout->GetClassName());
    }
    else
    {
        printf("<%s, %u>", layout->GetClassName(), layout->GetSize());
    }
}

const char* dmpGetHandleKindName(GenTreeFlags flags)
{
    switch (flags & GTF_ICON_HDL_MASK)
    {
        case GTF_ICON_SCOPE_HDL:
            return "scope";
        case GTF_ICON_CLASS_HDL:
            return "class";
        case GTF_ICON_METHOD_HDL:
            return "method";
        case GTF_ICON_FIELD_HDL:
            return "field";
        case GTF_ICON_STATIC_HDL:
            return "static";
        case GTF_ICON_STR_HDL:
            return "string";
        case GTF_ICON_CONST_PTR:
            return "const ptr";
        case GTF_ICON_GLOBAL_PTR:
            return "global ptr";
        case GTF_ICON_VARG_HDL:
            return "vararg";
        case GTF_ICON_PINVKI_HDL:
            return "pinvoke";
            break;
        case GTF_ICON_TOKEN_HDL:
            return "token";
#ifdef WINDOWS_X86_ABI
        case GTF_ICON_TLS_HDL:
            return "tls";
#endif
        case GTF_ICON_FTN_ADDR:
            return "ftn";
        case GTF_ICON_CIDMID_HDL:
            return "cid/mid";
        case GTF_ICON_BBC_PTR:
            return "bbc";
        default:
            return "handle";
    }
}

/*****************************************************************************/
void Compiler::gtDispConst(GenTree* tree)
{
    switch (tree->gtOper)
    {
        case GT_CNS_INT:
            if (tree->IsIconHandle(GTF_ICON_STR_HDL))
            {
                const WCHAR* str = eeGetCPString(tree->AsIntCon()->GetValue());
                // If *str points to a '\0' then don't print the string's values
                if ((str != nullptr) && (*str != '\0'))
                {
                    printf(" 0x%X \"%S\"", dspPtr(tree->AsIntCon()->GetValue()), str);
                }
                else // We can't print the value of the string
                {
                    // Note that eeGetCPString isn't currently implemented on Linux/ARM
                    // and instead always returns nullptr
                    printf(" 0x%X [ICON_STR_HDL]", dspPtr(tree->AsIntCon()->GetValue()));
                }
            }
            else
            {
                ssize_t value = tree->AsIntCon()->GetValue();

                if (tree->IsIconHandle())
                {
                    value = dspPtr(value);
                }

                if (tree->TypeIs(TYP_REF) && (value == 0))
                {
                    printf(" null");
                }
                else if ((value > -1000) && (value < 1000))
                {
                    printf(" %ld", value);
                }
#ifdef TARGET_64BIT
                else if ((value & 0xFFFFFFFF00000000LL) != 0)
                {
                    if (value >= 0)
                    {
                        printf(" 0x%llx", value);
                    }
                    else
                    {
                        printf(" -0x%llx", -value);
                    }
                }
#endif
                else
                {
                    if (value >= 0)
                    {
                        printf(" 0x%X", value);
                    }
                    else
                    {
                        printf(" -0x%X", -value);
                    }
                }

                const char* prefix = " (";

                if (tree->IsIconHandle())
                {
                    printf("%s%s", prefix, dmpGetHandleKindName(tree->GetIconHandleFlag()));
                    prefix = ", ";
                }

                if ((tree->AsIntCon()->GetFieldSeq() != nullptr) &&
                    (tree->AsIntCon()->GetFieldSeq() != FieldSeqStore::NotAField()))
                {
                    printf(prefix);
                    dmpFieldSeqFields(tree->AsIntCon()->GetFieldSeq());
                    prefix = ", ";
                }

#ifdef FEATURE_SIMD
                if ((tree->gtFlags & GTF_ICON_SIMD_COUNT) != 0)
                {
                    printf("%svector-length", prefix);
                    prefix = ", ";
                }
#endif

                if ((tree->IsReuseRegVal()) != 0)
                {
                    printf("%sreuse-reg-val", prefix);
                    prefix = ", ";
                }

                if (prefix[0] == ',')
                {
                    printf(")");
                }
            }
            break;

        case GT_CNS_LNG:
            printf(" 0x%016I64x", tree->AsLngCon()->GetValue());
            break;
        case GT_CNS_DBL:
            printf(" %#.17g", tree->AsDblCon()->GetValue());
            break;
        case GT_CNS_STR:
            printf("<string constant>");
            break;
        default:
            assert(!"unexpected constant node");
    }
}

void Compiler::dmpFieldSeqFields(FieldSeqNode* fieldSeq)
{
    if (fieldSeq->IsField())
    {
        printf("%s::", eeGetSimpleClassName(info.compCompHnd->getFieldClass(fieldSeq->GetFieldHandle())));
    }
    else if (fieldSeq->IsBoxedValueField())
    {
        FieldSeqNode* next = fieldSeq->GetNext();

        if ((next != nullptr) && next->IsField())
        {
            printf("box<%s>::", eeGetSimpleClassName(info.compCompHnd->getFieldClass(next->GetFieldHandle())));
            fieldSeq = next;
        }
    }
    else if (fieldSeq->IsArrayElement())
    {
        FieldSeqNode* next = fieldSeq->GetNext();

        if ((next != nullptr) && next->IsField())
        {
            printf("%s[]::", eeGetSimpleClassName(info.compCompHnd->getFieldClass(next->GetFieldHandle())));
            fieldSeq = next;
        }
    }

    for (; fieldSeq != nullptr; fieldSeq = fieldSeq->GetNext())
    {
        if (fieldSeq == FieldSeqStore::NotAField())
        {
            printf("N/A");
        }
        else if (fieldSeq->IsBoxedValueField())
        {
            printf("#BoxedValue");
        }
        else if (fieldSeq->IsArrayElement())
        {
            printf("[*]");
        }
        else
        {
            printf("%s", eeGetFieldName(fieldSeq->GetFieldHandle()));
        }

        if (fieldSeq->GetNext() != nullptr)
        {
            printf(".");
        }
    }
}

//------------------------------------------------------------------------
// gtDispLeaf: Print a single leaf node to jitstdout.
//
// Arguments:
//    tree - the tree to be printed
//    indentStack - the specification for the current level of indentation & arcs
//
// Return Value:
//    None.
//
// Notes:
//    'indentStack' may be null, in which case no indentation or arcs are printed

void Compiler::gtDispLeaf(GenTree* tree, IndentStack* indentStack)
{
    if (tree->OperIsConst())
    {
        gtDispConst(tree);
        return;
    }

    bool isLclFld = false;

    switch (tree->gtOper)
    {
        case GT_LCL_FLD:
        case GT_LCL_FLD_ADDR:
        case GT_STORE_LCL_FLD:
        case GT_PHI_ARG:
        case GT_LCL_VAR:
        case GT_LCL_VAR_ADDR:
        case GT_STORE_LCL_VAR:
            dmpLclVarCommon(tree->AsLclVarCommon(), indentStack);
            break;

        case GT_JMP:
        {
            const char* methodName;
            const char* className;

            methodName = eeGetMethodName((CORINFO_METHOD_HANDLE)tree->AsVal()->gtVal1, &className);
            printf(" %s.%s\n", className, methodName);
        }
        break;

        case GT_CLS_VAR_ADDR:
            printf(" %#x", dspPtr(tree->AsClsVar()->GetFieldHandle()));

            if (tree->AsClsVar()->GetFieldSeq() != nullptr)
            {
                printf(" (");
                dmpFieldSeqFields(tree->AsClsVar()->GetFieldSeq());
                printf(")");
            }
            break;

        case GT_LABEL:
            break;

        case GT_FTN_ADDR:
        {
            const char* methodName;
            const char* className;

            methodName = eeGetMethodName((CORINFO_METHOD_HANDLE)tree->AsFptrVal()->gtFptrMethod, &className);
            printf(" %s.%s\n", className, methodName);
        }
        break;

#if !defined(FEATURE_EH_FUNCLETS)
        case GT_END_LFIN:
            printf(" endNstLvl=%d", tree->AsVal()->gtVal1);
            break;
#endif // !FEATURE_EH_FUNCLETS

        // Vanilla leaves. No qualifying information available. So do nothing

        case GT_NO_OP:
        case GT_START_NONGC:
        case GT_START_PREEMPTGC:
        case GT_PROF_HOOK:
        case GT_CATCH_ARG:
        case GT_MEMORYBARRIER:
        case GT_ARGPLACE:
        case GT_PINVOKE_PROLOG:
        case GT_JMPTABLE:
            break;

        case GT_RET_EXPR:
            printf(" (call " FMT_TREEID ")", tree->AsRetExpr()->GetCall()->GetID());
            break;

        case GT_PHYSREG:
            printf(" %s", getRegName(tree->AsPhysReg()->gtSrcReg));
            break;

        case GT_IL_OFFSET:
            if (tree->AsILOffset()->gtStmtILoffsx == BAD_IL_OFFSET)
            {
                printf("???");
            }
            else
            {
                printf("0x%x", jitGetILoffs(tree->AsILOffset()->gtStmtILoffsx));
            }
            break;

        case GT_JCC:
        case GT_SETCC:
            printf(" cond=%s", tree->AsCC()->gtCondition.Name());
            break;
        case GT_JCMP:
            printf(" cond=%s%s", (tree->gtFlags & GTF_JCMP_TST) ? "TEST_" : "",
                   (tree->gtFlags & GTF_JCMP_EQ) ? "EQ" : "NE");
            break;

        default:
            assert(!"don't know how to display tree leaf node");
    }
}

void Compiler::dmpLclVarCommon(GenTreeLclVarCommon* node, IndentStack* indentStack)
{
    const unsigned   lclNum = node->GetLclNum();
    const LclVarDsc* lcl    = lvaGetDesc(lclNum);

    printf(" V%02u", lclNum);

    if (GenTreeLclFld* lclFld = node->IsLclFld())
    {
        printf("@%u", lclFld->GetLclOffs());
    }

    printf(" ");

    int         nameLength = dmpLclName(lclNum);
    const char* prefix     = nameLength > 0 ? " (" : "(";

    if (lcl->lvAddrExposed)
    {
        printf("%sAX", prefix);
        prefix = ", ";
    }
    else if (lcl->lvDoNotEnregister)
    {
        printf("%sDNER", prefix);
        prefix = ", ";
    }

    if (node->HasSsaName())
    {
        printf("%s", prefix);

        if ((node->gtFlags & GTF_VAR_USEASG) != 0)
        {
            printf("u:%d, d:%d", node->GetSsaNum(), GetSsaDefNum(node));
        }
        else
        {
            printf("%s:%d", ((node->gtFlags & GTF_VAR_DEF) != 0) ? "d" : "u", node->GetSsaNum());
        }

        prefix = ", ";
    }

    if (lcl->IsPromoted())
    {
        printf("%s%s", prefix, lcl->IsIndependentPromoted() ? "P-INDEP" : "P-DEP");
        prefix = ", ";
    }
    else if (lcl->IsPromotedField())
    {
        printf("%s%s V%02u@%u", prefix, lcl->IsDependentPromotedField(this) ? "P-DEP" : "P-INDEP",
               lcl->GetPromotedFieldParentLclNum(), lcl->GetPromotedFieldOffset());

        if (lcl->GetPromotedFieldSeq() != nullptr)
        {
            printf(" ");
            dmpFieldSeqFields(lcl->GetPromotedFieldSeq());
        }

        prefix = ", ";
    }

    if (GenTreeLclFld* lclFld = node->IsLclFld())
    {
        if (lclFld->GetFieldSeq() != nullptr)
        {
            printf(prefix);
            dmpFieldSeqFields(lclFld->GetFieldSeq());
            prefix = ", ";
        }
    }

    if (fgLocalVarLivenessDone && node->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD) &&
        node->HasLastUse())
    {
        printf("%slast-use", prefix);
        prefix = ", ";
    }

    if (lcl->lvRegister)
    {
        printf("%s%%%s", prefix, getRegName(lcl->GetRegNum()));
        prefix = ", ";
    }

    if (prefix[0] == ',')
    {
        printf(")");
    }

    if (!lcl->IsPromoted())
    {
        return;
    }

    printf(" { ");

    for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
    {
        LclVarDsc* fieldLcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

        if (i != 0)
        {
            printf(", ");
        }

        printf("V%02u %s @%u", lcl->GetPromotedFieldLclNum(i), varTypeName(fieldLcl->GetType()),
               fieldLcl->GetPromotedFieldOffset());

        prefix = " (";

        if (fieldLcl->lvTracked && fgLocalVarLivenessDone && node->IsMultiRegLclVar() && node->AsLclVar()->IsLastUse(i))
        {
            printf("%slast-use", prefix);
            prefix = ", ";
        }

        if (fieldLcl->lvRegister)
        {
            printf("%s%%%s", prefix, getRegName(fieldLcl->GetRegNum()));
            prefix = ", ";
        }

        if (prefix[0] == ',')
        {
            printf(")");
        }
    }

    printf(" }");
}

const char* StructStoreKindName(StructStoreKind kind)
{
    switch (kind)
    {
        case StructStoreKind::Invalid:
            return "";
        case StructStoreKind::UnrollInit:
            return ("UnrollInit");
        case StructStoreKind::UnrollCopy:
            return ("UnrollCopy");
        case StructStoreKind::UnrollCopyWB:
            return "UnrollCopyWB";
#ifdef TARGET_XARCH
        case StructStoreKind::UnrollCopyWBRepMovs:
            return "UnrollCopyWBRepMovs";
        case StructStoreKind::RepStos:
            return "RepStos";
        case StructStoreKind::RepMovs:
            return "RepMovs";
#endif
#ifndef TARGET_X86
        case StructStoreKind::MemSet:
            return "MemSet";
        case StructStoreKind::MemCpy:
            return "MemCpy";
#endif
#if FEATURE_MULTIREG_RET
        case StructStoreKind::UnrollRegs:
            return "UnrollRegs";
#endif
#if defined(UNIX_AMD64_ABI) || defined(TARGET_ARM64)
        case StructStoreKind::UnrollRegsWB:
            return "UnrollRegsWB";
#endif
        default:
            return "???";
    }
}

//------------------------------------------------------------------------
// gtDispLeaf: Print a child node to jitstdout.
//
// Arguments:
//    tree - the tree to be printed
//    indentStack - the specification for the current level of indentation & arcs
//    arcType     - the type of arc to use for this child
//    msg         - a contextual method (i.e. from the parent) to print
//    topOnly     - a boolean indicating whether to print the children, or just the top node
//
// Return Value:
//    None.
//
// Notes:
//    'indentStack' may be null, in which case no indentation or arcs are printed
//    'msg' has a default value of null
//    'topOnly' is an optional argument that defaults to false

void Compiler::gtDispChild(GenTree*             child,
                           IndentStack*         indentStack,
                           IndentInfo           arcType,
                           __in_opt const char* msg,     /* = nullptr  */
                           bool                 topOnly) /* = false */
{
    indentStack->Push(arcType);
    gtDispTree(child, indentStack, msg, topOnly);
    indentStack->Pop();
}

void Compiler::gtDispTree(GenTree*     tree,
                          IndentStack* indentStack,                 /* = nullptr */
                          __in __in_z __in_opt const char* msg,     /* = nullptr  */
                          bool                             topOnly, /* = false */
                          bool                             isLIR)   /* = false */
{
    if (tree == nullptr)
    {
        printf(" [%08X] <NULL>\n", tree);
        printf(""); // null string means flush
        return;
    }

    if (indentStack == nullptr)
    {
        indentStack = new (this, CMK_DebugOnly) IndentStack(this);
    }

    if (IsUninitialized(tree))
    {
        /* Value used to initalize nodes */
        printf("Uninitialized tree node!\n");
        return;
    }

    if (tree->gtOper >= GT_COUNT)
    {
        gtDispNode(tree, indentStack, msg, isLIR);
        printf("Bogus operator!\n");
        return;
    }

    /* Is tree a leaf node? */

    if (tree->OperIsLeaf() || tree->OperIsLocalStore()) // local stores used to be leaves
    {
        gtDispNode(tree, indentStack, msg, isLIR);
        gtDispLeaf(tree, indentStack);
        gtDispCommonEndLine(tree);

        if (tree->OperIsLocalStore() && !topOnly)
        {
            gtDispChild(tree->AsOp()->gtOp1, indentStack, IINone);
        }
        return;
    }

    // Determine what kind of arc to propagate.
    IndentInfo myArc    = IINone;
    IndentInfo lowerArc = IINone;
    if (indentStack->Depth() > 0)
    {
        myArc = indentStack->Pop();
        switch (myArc)
        {
            case IIArcBottom:
                indentStack->Push(IIArc);
                lowerArc = IINone;
                break;
            case IIArc:
                indentStack->Push(IIArc);
                lowerArc = IIArc;
                break;
            case IIArcTop:
                indentStack->Push(IINone);
                lowerArc = IIArc;
                break;
            case IINone:
                indentStack->Push(IINone);
                lowerArc = IINone;
                break;
            default:
                unreached();
                break;
        }
    }

    if (myArc != IINone)
    {
        indentStack->Pop();
        indentStack->Push(myArc);
    }

    gtDispNode(tree, indentStack, msg, isLIR);

    if (indentStack->Depth() > 0)
    {
        (void)indentStack->Pop();
        indentStack->Push(lowerArc);
    }

    switch (tree->GetOper())
    {
        case GT_CAST:
        {
            var_types fromType = genActualType(tree->AsUnOp()->GetOp(0)->GetType());
            var_types toType   = tree->AsCast()->GetCastType();

            if (tree->IsUnsigned())
            {
                fromType = varTypeToUnsigned(fromType);
            }

            printf(" (%s to %s)", varTypeName(fromType), varTypeName(toType));
        }
        break;

        case GT_PUTARG_STK:
        {
            printf(" (@%u, %d slots", tree->AsPutArgStk()->GetSlotOffset(), tree->AsPutArgStk()->GetSlotCount());
#ifdef TARGET_XARCH
            const char* kindName;
            switch (tree->AsPutArgStk()->GetKind())
            {
                case GenTreePutArgStk::Kind::RepInstr:
                    kindName = "RepInst";
                    break;
                case GenTreePutArgStk::Kind::Unroll:
                    kindName = "Unroll";
                    break;
#ifdef TARGET_X86
                case GenTreePutArgStk::Kind::Push:
                    kindName = "Push";
                    break;
                case GenTreePutArgStk::Kind::PushAllSlots:
                    kindName = "PushAllSlots";
                    break;
#endif
                case GenTreePutArgStk::Kind::RepInstrXMM:
                    kindName = "RepInstrXMM";
                    break;
                case GenTreePutArgStk::Kind::GCUnroll:
                    kindName = "GCUnroll";
                    break;
                case GenTreePutArgStk::Kind::GCUnrollXMM:
                    kindName = "GCUnrollXMM";
                    break;
                default:
                    kindName = "???";
                    break;
            }
            printf(", %s)", kindName);
#else
            printf(")");
#endif
        }
        break;

        case GT_INTRINSIC:
        {
            GenTreeIntrinsic* intrinsic = tree->AsIntrinsic();

            if (intrinsic->gtIntrinsicId == CORINFO_INTRINSIC_Object_GetType)
            {
                assert(intrinsic->gtIntrinsicName == NI_Illegal);
                printf(" objGetType");
            }
            else
            {
                assert(intrinsic->gtIntrinsicId == CORINFO_INTRINSIC_Illegal);
                switch (intrinsic->gtIntrinsicName)
                {
                    case NI_System_Math_Abs:
                        printf(" abs");
                        break;
                    case NI_System_Math_Acos:
                        printf(" acos");
                        break;
                    case NI_System_Math_Acosh:
                        printf(" acosh");
                        break;
                    case NI_System_Math_Asin:
                        printf(" asin");
                        break;
                    case NI_System_Math_Asinh:
                        printf(" asinh");
                        break;
                    case NI_System_Math_Atan:
                        printf(" atan");
                        break;
                    case NI_System_Math_Atanh:
                        printf(" atanh");
                        break;
                    case NI_System_Math_Atan2:
                        printf(" atan2");
                        break;
                    case NI_System_Math_Cbrt:
                        printf(" cbrt");
                        break;
                    case NI_System_Math_Ceiling:
                        printf(" ceiling");
                        break;
                    case NI_System_Math_Cos:
                        printf(" cos");
                        break;
                    case NI_System_Math_Cosh:
                        printf(" cosh");
                        break;
                    case NI_System_Math_Exp:
                        printf(" exp");
                        break;
                    case NI_System_Math_Floor:
                        printf(" floor");
                        break;
                    case NI_System_Math_FMod:
                        printf(" fmod");
                        break;
                    case NI_System_Math_FusedMultiplyAdd:
                        printf(" fma");
                        break;
                    case NI_System_Math_ILogB:
                        printf(" ilogb");
                        break;
                    case NI_System_Math_Log:
                        printf(" log");
                        break;
                    case NI_System_Math_Log2:
                        printf(" log2");
                        break;
                    case NI_System_Math_Log10:
                        printf(" log10");
                        break;
                    case NI_System_Math_Pow:
                        printf(" pow");
                        break;
                    case NI_System_Math_Round:
                        printf(" round");
                        break;
                    case NI_System_Math_Sin:
                        printf(" sin");
                        break;
                    case NI_System_Math_Sinh:
                        printf(" sinh");
                        break;
                    case NI_System_Math_Sqrt:
                        printf(" sqrt");
                        break;
                    case NI_System_Math_Tan:
                        printf(" tan");
                        break;
                    case NI_System_Math_Tanh:
                        printf(" tanh");
                        break;

                    default:
                        unreached();
                }
            }
        }
        break;

        case GT_FIELD_LIST:
            printf(" { ");
            for (GenTreeFieldList::Use& use : tree->AsFieldList()->Uses())
            {
                printf("%s%s @%u", use.GetOffset() != 0 ? ", " : "", varTypeName(use.GetType()), use.GetOffset());
            }
            printf(" }");

            gtDispCommonEndLine(tree);

            if (!topOnly)
            {
                for (GenTreeFieldList::Use& use : tree->AsFieldList()->Uses())
                {
                    gtDispChild(use.GetNode(), indentStack, (use.GetNext() == nullptr) ? IIArcBottom : IIArc);
                }
            }
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            printf(" %s %s %u", GetHWIntrinsicIdName(tree->AsHWIntrinsic()->GetIntrinsic()),
                   tree->AsHWIntrinsic()->GetSimdBaseType() == TYP_UNDEF
                       ? ""
                       : varTypeName(tree->AsHWIntrinsic()->GetSimdBaseType()),
                   tree->AsHWIntrinsic()->GetSimdSize());

            if (tree->AsHWIntrinsic()->GetAuxiliaryType() != TYP_UNDEF)
            {
                printf(" %s", varTypeName(tree->AsHWIntrinsic()->GetAuxiliaryType()));
            }

            gtDispCommonEndLine(tree);

            if (!topOnly)
            {
                for (unsigned i = 0; i < tree->AsHWIntrinsic()->GetNumOps(); i++)
                {
                    gtDispChild(tree->AsHWIntrinsic()->GetOp(i), indentStack,
                                (i == tree->AsHWIntrinsic()->GetNumOps() - 1) ? IIArcBottom : IIArc);
                }
            }
            break;
#endif // FEATURE_HW_INTRINSICS

        case GT_INSTR:
            printf(" %s", insName(tree->AsInstr()->GetIns()));
#ifdef TARGET_ARMARCH
            if (tree->AsInstr()->GetOption() != 0)
            {
                printf(" %s", insOptsName(tree->AsInstr()->GetOption()));
            }
#endif
            printf(" #%u", tree->AsInstr()->GetImmediate());
            gtDispCommonEndLine(tree);

            if (!topOnly)
            {
                for (unsigned i = 0; i < tree->AsInstr()->GetNumOps(); i++)
                {
                    gtDispChild(tree->AsInstr()->GetOp(i), indentStack,
                                (i == tree->AsInstr()->GetNumOps() - 1) ? IIArcBottom : IIArc);
                }
            }
            break;

        case GT_PHI:
            gtDispCommonEndLine(tree);

            if (!topOnly)
            {
                for (GenTreePhi::Use& use : tree->AsPhi()->Uses())
                {
                    char block[32];
                    sprintf_s(block, sizeof(block), "pred " FMT_BB, use.GetNode()->AsPhiArg()->gtPredBB->bbNum);
                    gtDispChild(use.GetNode(), indentStack, (use.GetNext() == nullptr) ? IIArcBottom : IIArc, block);
                }
            }
            break;

        case GT_FIELD_ADDR:
            printf(" @%u ", tree->AsFieldAddr()->GetOffset());
            dmpFieldSeqFields(tree->AsFieldAddr()->GetFieldSeq());
            break;

        case GT_CALL:
        {
            GenTreeCall* call = tree->AsCall();

            if (call->gtCallType != CT_INDIRECT)
            {
                const char* methodName;
                const char* className;

                methodName = eeGetMethodName(call->gtCallMethHnd, &className);

                printf(" %s.%s", className, methodName);
            }

            if (((call->gtFlags & GTF_CALL_INLINE_CANDIDATE) != 0) && (call->gtInlineCandidateInfo != nullptr) &&
                (call->gtInlineCandidateInfo->exactContextHnd != nullptr))
            {
                printf(" (exactContextHnd=0x%p)", dspPtr(call->gtInlineCandidateInfo->exactContextHnd));
            }

            gtDispCommonEndLine(tree);

            if (!topOnly)
            {
                GenTree* lastChild = nullptr;
                call->VisitOperands([&lastChild](GenTree* op) {
                    lastChild = op;
                    return GenTree::VisitResult::Continue;
                });

                unsigned argNum = 0;

                if (call->gtCallThisArg != nullptr)
                {
                    char buf[256];

                    GenTree* argNode = call->gtCallThisArg->GetNode();
                    gtGetCallArgMsg(call, argNode, argNum, buf, sizeof(buf));
                    gtDispChild(argNode, indentStack, (argNode == lastChild) ? IIArcBottom : IIArc, buf, false);
                    argNum++;
                }

                for (GenTreeCall::Use& use : call->Args())
                {
                    char buf[256];

                    GenTree* argNode = use.GetNode();
                    gtGetCallArgMsg(call, argNode, argNum, buf, sizeof(buf));
                    gtDispChild(argNode, indentStack, (argNode == lastChild) ? IIArcBottom : IIArc, buf, false);
                    argNum++;
                }

                for (GenTreeCall::Use& use : call->LateArgs())
                {
                    char buf[256];

                    IndentInfo arcType = (use.GetNext() == nullptr) ? IIArcBottom : IIArc;
                    gtGetCallArgMsg(call, call->GetArgInfoByLateArgUse(&use), use.GetNode(), buf, sizeof(buf));
                    gtDispChild(use.GetNode(), indentStack, arcType, buf, false);
                }

                if (call->gtCallType == CT_INDIRECT)
                {
                    gtDispChild(call->gtCallAddr, indentStack, (call->gtCallAddr == lastChild) ? IIArcBottom : IIArc,
                                "calli tgt", false);
                }

                if (call->gtControlExpr != nullptr)
                {
                    gtDispChild(call->gtControlExpr, indentStack,
                                (call->gtControlExpr == lastChild) ? IIArcBottom : IIArc, "control expr", false);
                }
            }
        }
        break;

        case GT_ARR_ELEM:
            gtDispCommonEndLine(tree);

            if (!topOnly)
            {
                gtDispChild(tree->AsArrElem()->gtArrObj, indentStack, IIArc, nullptr, topOnly);

                unsigned dim;
                for (dim = 0; dim < tree->AsArrElem()->gtArrRank; dim++)
                {
                    IndentInfo arcType = ((dim + 1) == tree->AsArrElem()->gtArrRank) ? IIArcBottom : IIArc;
                    gtDispChild(tree->AsArrElem()->gtArrInds[dim], indentStack, arcType, nullptr, topOnly);
                }
            }
            break;

        case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
        case GT_HW_INTRINSIC_CHK:
#endif
            gtDispCommonEndLine(tree);

            if (!topOnly)
            {
                gtDispChild(tree->AsBoundsChk()->GetIndex(), indentStack, IIArc, nullptr, topOnly);
                gtDispChild(tree->AsBoundsChk()->GetLength(), indentStack, IIArcBottom, nullptr, topOnly);
            }
            break;

        case GT_STORE_OBJ:
        case GT_STORE_BLK:
            if (tree->AsBlk()->GetKind() != StructStoreKind::Invalid)
            {
                printf(" (%s)", StructStoreKindName(tree->AsBlk()->GetKind()));
            }

            gtDispCommonEndLine(tree);

            if (!topOnly)
            {
                gtDispChild(tree->AsBlk()->GetValue(), indentStack, IIArc);
                gtDispChild(tree->AsBlk()->GetAddr(), indentStack, IIArc);
            }
            return;

        case GT_ARR_OFFSET:
        case GT_CMPXCHG:
        case GT_COPY_BLK:
        case GT_INIT_BLK:
        case GT_QMARK:
            gtDispCommonEndLine(tree);

            if (!topOnly)
            {
                gtDispChild(tree->AsTernaryOp()->GetOp(0), indentStack, IIArc);
                gtDispChild(tree->AsTernaryOp()->GetOp(1), indentStack, IIArc);
                gtDispChild(tree->AsTernaryOp()->GetOp(2), indentStack, IIArcBottom);
            }
            break;

        default:
            if (!tree->OperIsSimple())
            {
                printf("<DON'T KNOW HOW TO DISPLAY THIS NODE> :");
                printf(""); // null string means flush
            }
            break;
    }

    if (tree->OperIsSimple())
    {
        gtDispCommonEndLine(tree);

        if (!topOnly)
        {
            if (tree->AsOp()->gtOp1 != nullptr)
            {
                gtDispChild(tree->AsOp()->gtOp1, indentStack,
                            (tree->gtGetOp2IfPresent() == nullptr) ? IIArcBottom : IIArc);
            }

            if (tree->gtGetOp2IfPresent())
            {
                gtDispChild(tree->AsOp()->gtOp2, indentStack, IIArcBottom);
            }
        }
    }
}

void Compiler::gtGetCallArgMsg(GenTreeCall* call, GenTree* arg, unsigned argNum, char* buf, unsigned bufLength)
{
    if (call->GetInfo() == nullptr)
    {
        if ((call->gtCallThisArg != nullptr) && (arg == call->gtCallThisArg->GetNode()))
        {
            sprintf_s(buf, bufLength, "this");
        }
        else if (call->HasRetBufArg() && call->TypeIs(TYP_VOID) && (arg == call->gtCallArgs->GetNode()))
        {
            sprintf_s(buf, bufLength, "retbuf");
        }
        else
        {
            sprintf_s(buf, bufLength, "arg%d", argNum);
        }

        return;
    }

    gtGetCallArgMsg(call, call->GetArgInfoByArgNum(argNum), arg, buf, bufLength);
}

void Compiler::gtGetCallArgMsg(GenTreeCall* call, CallArgInfo* argInfo, GenTree* arg, char* buf, unsigned bufLength)
{
    if (argInfo->use == call->gtCallThisArg)
    {
        int len = sprintf_s(buf, bufLength, "this");
        buf += len;
        bufLength -= len;
    }
    else if (call->HasRetBufArg() && call->TypeIs(TYP_VOID) && (argInfo->use == call->gtCallArgs))
    {
        int len = sprintf_s(buf, bufLength, "retbuf");
        buf += len;
        bufLength -= len;
    }
    else
    {
        int len = sprintf_s(buf, bufLength, "arg%d", argInfo->GetArgNum());
        buf += len;
        bufLength -= len;
    }

    if (argInfo->GetNode() != arg)
    {
        sprintf_s(buf, bufLength, " SETUP");
        return;
    }

    if (argInfo->GetRegCount() == 1)
    {
        int len = sprintf_s(buf, bufLength, " %s", getRegName(argInfo->GetRegNum()));
        buf += len;
        bufLength -= len;
    }
    else if (argInfo->GetRegCount() > 1)
    {
        int len = sprintf_s(buf, bufLength, " %s-%s", getRegName(argInfo->GetRegNum(0)),
                            getRegName(argInfo->GetRegNum(argInfo->GetRegCount() - 1)));
        buf += len;
        bufLength -= len;
    }

    if (argInfo->GetSlotCount() != 0)
    {
#if FEATURE_FIXED_OUT_ARGS
        sprintf_s(buf, bufLength, " out+%02x", argInfo->GetSlotNum() * REGSIZE_BYTES);
#else
        sprintf_s(buf, bufLength, " PUSH");
#endif
    }
}

// gtDispStmt: Print a statement to jitstdout.
//
// Arguments:
//    stmt - the statement to be printed;
//    msg  - an additional message to print before the statement.
//
void Compiler::gtDispStmt(Statement* stmt, const char* msg /* = nullptr */)
{
    if (opts.compDbgInfo)
    {
        if (msg != nullptr)
        {
            printf("%s ", msg);
        }
        printStmtID(stmt);
        IL_OFFSETX firstILOffsx = stmt->GetILOffsetX();
        printf(" (IL ");
        if (firstILOffsx == BAD_IL_OFFSET)
        {
            printf("  ???");
        }
        else
        {
            printf("0x%03X", jitGetILoffs(firstILOffsx));
        }
        printf("...");

        IL_OFFSET lastILOffs = stmt->GetLastILOffset();
        if (lastILOffs == BAD_IL_OFFSET)
        {
            printf("  ???");
        }
        else
        {
            printf("0x%03X", lastILOffs);
        }
        printf(")\n");
    }
    gtDispTree(stmt->GetRootNode());
}

//------------------------------------------------------------------------
// gtDispBlockStmts: dumps all statements inside `block`.
//
// Arguments:
//    block - the block to display statements for.
//
void Compiler::gtDispBlockStmts(BasicBlock* block)
{
    for (Statement* const stmt : block->Statements())
    {
        gtDispStmt(stmt);
        printf("\n");
    }
}

//------------------------------------------------------------------------
// Compiler::gtDispRange: dumps a range of LIR.
//
// Arguments:
//    range - the range of LIR to display.
//
void Compiler::gtDispRange(LIR::ReadOnlyRange const& range)
{
    for (GenTree* node : range)
    {
        gtDispLIRNode(node);
    }
}

//------------------------------------------------------------------------
// Compiler::gtDispTreeRange: dumps the LIR range that contains all of the
//                            nodes in the dataflow tree rooted at a given
//                            node.
//
// Arguments:
//    containingRange - the LIR range that contains the root node.
//    tree - the root of the dataflow tree.
//
void Compiler::gtDispTreeRange(LIR::Range& containingRange, GenTree* tree)
{
    bool unused;
    gtDispRange(containingRange.GetTreeRange(tree, &unused));
}

void Compiler::gtDispLIRNode(GenTree* node)
{
    if (GenTreeInstr* instr = node->IsInstr())
    {
        gtDispNodeHeader(instr);
        dmpNodeRegs(instr);

        printf(" %s %s ", insName(instr->GetIns()), varTypeName(node->GetType()));

        for (unsigned i = 0; i < instr->GetNumOps(); i++)
        {
            GenTree* op = instr->GetOp(i);

            printf("t%u.%s", op->gtTreeID, varTypeName(op->GetType()));

            if (op->GetRegNum() != REG_NA)
            {
                printf(" @%s", getRegName(op->GetRegNum()));
            }

            if (i != instr->GetNumOps() - 1)
            {
                printf(", ");
            }
        }

#ifdef TARGET_ARMARCH
        if (instr->GetOption() != INS_OPTS_NONE)
        {
            printf(", %s %u", insOptsName(instr->GetOption()), instr->GetImmediate());
        }
        else
#endif
            if (instr->GetImmediate() != 0)
        {
            switch (instr->GetIns())
            {
#ifdef TARGET_ARM64
                case INS_and:
                case INS_orr:
                case INS_eor:
                    printf(", 0x%x", DecodeBitmaskImm(instr->GetImmediate(), instr->GetSize()));
                    break;
#endif
                default:
                    printf(", %u", instr->GetImmediate());
                    break;
            }
        }

        printf("\n");
        return;
    }

    IndentStack indentStack(this);

    const bool topOnly = true;
    const bool isLIR   = true;
    gtDispTree(node, &indentStack, nullptr, topOnly, isLIR);
}

void Compiler::dmpNodeOperands(GenTree* node)
{
    auto displayOperand = [](GenTree* operand, const char* prefix, const char* message = nullptr) {
        printf("%s", prefix);

        if (message != nullptr)
        {
            printf("%s:", message);
        }

        printf("t%u %s", operand->GetID(), varTypeName(operand->GetType()));
    };

    char        message[256];
    const char* prefix = " USES(";

    for (GenTree* operand : node->Operands())
    {
        if (operand->OperIs(GT_ARGPLACE) || !operand->IsValue())
        {
            // Either of these situations may happen with calls.
            continue;
        }

        if (GenTreeCall* call = node->IsCall())
        {
            if (operand == call->gtCallAddr)
            {
                displayOperand(operand, prefix, "calli tgt");
            }
            else if (operand == call->gtControlExpr)
            {
                displayOperand(operand, prefix, "control expr");
            }
            else if (operand == call->gtCallCookie)
            {
                displayOperand(operand, prefix, "cookie");
            }
            else
            {
                CallArgInfo* argInfo = call->GetArgInfoByArgNode(operand);
                gtGetCallArgMsg(call, argInfo, operand, message, sizeof(message));
                displayOperand(operand, prefix, message);
            }
        }
        else
        {
            displayOperand(operand, prefix);
        }

        prefix = ", ";
    }

    if (prefix[0] == ',')
    {
        printf(")");
    }
}

/*****************************************************************************/
#endif // DEBUG

/*****************************************************************************
 *
 *  Check if the given node can be folded,
 *  and call the methods to perform the folding
 */

GenTree* Compiler::gtFoldExpr(GenTree* tree)
{
    unsigned kind = tree->OperKind();

    /* We must have a simple operation to fold */

    // If we're in CSE, it's not safe to perform tree
    // folding given that it can will potentially
    // change considered CSE candidates.
    if (optValnumCSE_phase)
    {
        return tree;
    }

    if (!(kind & GTK_SMPOP))
    {
        return tree;
    }

    GenTree* op1 = tree->AsOp()->gtOp1;

    /* Filter out non-foldable trees that can have constant children */

    assert(kind & (GTK_UNOP | GTK_BINOP));
    switch (tree->gtOper)
    {
        case GT_RETFILT:
        case GT_RETURN:
        case GT_IND:
            return tree;
        default:
            break;
    }

    /* try to fold the current node */

    if ((kind & GTK_UNOP) && op1)
    {
        if (op1->OperIsConst())
        {
            return gtFoldExprConst(tree);
        }
    }
    else if ((kind & GTK_BINOP) && op1 && tree->AsOp()->gtOp2 &&
             // Don't take out conditionals for debugging
             (opts.OptimizationEnabled() || !tree->OperIsCompare()))
    {
        GenTree* op2 = tree->AsOp()->gtOp2;

        // The atomic operations are exempted here because they are never computable statically;
        // one of their arguments is an address.
        if (op1->OperIsConst() && op2->OperIsConst() && !tree->OperIsAtomicOp())
        {
            /* both nodes are constants - fold the expression */
            return gtFoldExprConst(tree);
        }
        else if (op1->OperIsConst() || op2->OperIsConst())
        {
            /* at least one is a constant - see if we have a
             * special operator that can use only one constant
             * to fold - e.g. booleans */

            return gtFoldExprSpecial(tree);
        }
        else if (tree->OperIsCompare())
        {
            /* comparisons of two local variables can sometimes be folded */

            return gtFoldExprCompare(tree);
        }
    }

    /* Return the original node (folded/bashed or not) */

    return tree;
}

//------------------------------------------------------------------------
// gtFoldExprCall: see if a call is foldable
//
// Arguments:
//    call - call to examine
//
// Returns:
//    The original call if no folding happened.
//    An alternative tree if folding happens.
//
// Notes:
//    Checks for calls to Type.op_Equality, Type.op_Inequality, and
//    Enum.HasFlag, and if the call is to one of these,
//    attempts to optimize.

GenTree* Compiler::gtFoldExprCall(GenTreeCall* call)
{
    // Can only fold calls to special intrinsics.
    if ((call->gtCallMoreFlags & GTF_CALL_M_SPECIAL_INTRINSIC) == 0)
    {
        return call;
    }

    // Defer folding if not optimizing.
    if (opts.OptimizationDisabled())
    {
        return call;
    }

    // Check for a new-style jit intrinsic.
    const NamedIntrinsic ni = lookupNamedIntrinsic(call->gtCallMethHnd);

    switch (ni)
    {
        case NI_System_Enum_HasFlag:
        {
            GenTree* thisOp = call->gtCallThisArg->GetNode();
            GenTree* flagOp = call->gtCallArgs->GetNode();
            GenTree* result = gtOptimizeEnumHasFlag(thisOp, flagOp);

            if (result != nullptr)
            {
                return result;
            }
            break;
        }

        case NI_System_Type_op_Equality:
        case NI_System_Type_op_Inequality:
        {
            noway_assert(call->TypeGet() == TYP_INT);
            GenTree* op1 = call->gtCallArgs->GetNode();
            GenTree* op2 = call->gtCallArgs->GetNext()->GetNode();

            // If either operand is known to be a RuntimeType, this can be folded
            GenTree* result = gtFoldTypeEqualityCall(ni == NI_System_Type_op_Equality, op1, op2);
            if (result != nullptr)
            {
                return result;
            }
            break;
        }

        default:
            break;
    }

    return call;
}

//------------------------------------------------------------------------
// gtFoldTypeEqualityCall: see if a (potential) type equality call is foldable
//
// Arguments:
//    isEq -- is it == or != operator
//    op1  -- first argument to call
//    op2  -- second argument to call
//
// Returns:
//    nulltpr if no folding happened.
//    An alternative tree if folding happens.
//
// Notes:
//    If either operand is known to be a a RuntimeType, then the type
//    equality methods will simply check object identity and so we can
//    fold the call into a simple compare of the call's operands.

GenTree* Compiler::gtFoldTypeEqualityCall(bool isEq, GenTree* op1, GenTree* op2)
{
    if ((gtGetTypeProducerKind(op1) == TPK_Unknown) && (gtGetTypeProducerKind(op2) == TPK_Unknown))
    {
        return nullptr;
    }

    const genTreeOps simpleOp = isEq ? GT_EQ : GT_NE;

    JITDUMP("\nFolding call to Type:op_%s to a simple compare via %s\n", isEq ? "Equality" : "Inequality",
            GenTree::OpName(simpleOp));

    GenTree* compare = gtNewOperNode(simpleOp, TYP_INT, op1, op2);

    return compare;
}

/*****************************************************************************
 *
 *  Some comparisons can be folded:
 *
 *    locA        == locA
 *    classVarA   == classVarA
 *    locA + locB == locB + locA
 *
 */

GenTree* Compiler::gtFoldExprCompare(GenTree* tree)
{
    GenTree* op1 = tree->AsOp()->gtOp1;
    GenTree* op2 = tree->AsOp()->gtOp2;

    assert(tree->OperIsCompare());

    /* Filter out cases that cannot be folded here */

    /* Do not fold floats or doubles (e.g. NaN != Nan) */

    if (varTypeIsFloating(op1->TypeGet()))
    {
        return tree;
    }

    /* Currently we can only fold when the two subtrees exactly match */

    if ((tree->gtFlags & GTF_SIDE_EFFECT) || GenTree::Compare(op1, op2, true) == false)
    {
        return tree; /* return unfolded tree */
    }

    GenTree* cons;

    switch (tree->gtOper)
    {
        case GT_EQ:
        case GT_LE:
        case GT_GE:
            cons = gtNewIconNode(true); /* Folds to GT_CNS_INT(true) */
            break;

        case GT_NE:
        case GT_LT:
        case GT_GT:
            cons = gtNewIconNode(false); /* Folds to GT_CNS_INT(false) */
            break;

        default:
            assert(!"Unexpected relOp");
            return tree;
    }

    /* The node has beeen folded into 'cons' */

    JITDUMP("\nFolding comparison with identical operands:\n");
    DISPTREE(tree);

    if (fgGlobalMorph)
    {
        fgMorphTreeDone(cons);
    }
    else
    {
        cons->gtNext = tree->gtNext;
        cons->gtPrev = tree->gtPrev;
    }

    JITDUMP("Bashed to %s:\n", cons->AsIntConCommon()->IconValue() ? "true" : "false");
    DISPTREE(cons);

    return cons;
}

//------------------------------------------------------------------------
// gtCreateHandleCompare: generate a type handle comparison
//
// Arguments:
//    oper -- comparison operation (equal/not equal)
//    op1 -- first operand
//    op2 -- second operand
//    typeCheckInliningResult -- indicates how the comparison should happen
//
// Returns:
//    Type comparison tree
//

GenTree* Compiler::gtCreateHandleCompare(genTreeOps             oper,
                                         GenTree*               op1,
                                         GenTree*               op2,
                                         CorInfoInlineTypeCheck typeCheckInliningResult)
{
    // If we can compare pointers directly, just emit the binary operation
    if (typeCheckInliningResult == CORINFO_INLINE_TYPECHECK_PASS)
    {
        return gtNewOperNode(oper, TYP_INT, op1, op2);
    }

    assert(typeCheckInliningResult == CORINFO_INLINE_TYPECHECK_USE_HELPER);

    // Emit a call to a runtime helper
    GenTreeCall::Use* helperArgs = gtNewCallArgs(op1, op2);
    GenTree*          ret        = gtNewHelperCallNode(CORINFO_HELP_ARE_TYPES_EQUIVALENT, TYP_INT, helperArgs);
    if (oper == GT_EQ)
    {
        ret = gtNewOperNode(GT_NE, TYP_INT, ret, gtNewIconNode(0, TYP_INT));
    }
    else
    {
        assert(oper == GT_NE);
        ret = gtNewOperNode(GT_EQ, TYP_INT, ret, gtNewIconNode(0, TYP_INT));
    }

    return ret;
}

//------------------------------------------------------------------------
// gtFoldTypeCompare: see if a type comparison can be further simplified
//
// Arguments:
//    tree -- tree possibly comparing types
//
// Returns:
//    An alternative tree if folding happens.
//    Original tree otherwise.
//
// Notes:
//    Checks for
//        typeof(...) == obj.GetType()
//        typeof(...) == typeof(...)
//        obj1.GetType() == obj2.GetType()
//
//    And potentially optimizes away the need to obtain actual
//    RuntimeType objects to do the comparison.

GenTree* Compiler::gtFoldTypeCompare(GenTree* tree)
{
    // Only handle EQ and NE
    // (maybe relop vs null someday)
    const genTreeOps oper = tree->OperGet();
    if ((oper != GT_EQ) && (oper != GT_NE))
    {
        return tree;
    }

    // Screen for the right kinds of operands
    GenTree* const         op1     = tree->AsOp()->gtOp1;
    const TypeProducerKind op1Kind = gtGetTypeProducerKind(op1);
    if (op1Kind == TPK_Unknown)
    {
        return tree;
    }

    GenTree* const         op2     = tree->AsOp()->gtOp2;
    const TypeProducerKind op2Kind = gtGetTypeProducerKind(op2);
    if (op2Kind == TPK_Unknown)
    {
        return tree;
    }

    // If both types are created via handles, we can simply compare
    // handles instead of the types that they'd create.
    if ((op1Kind == TPK_Handle) && (op2Kind == TPK_Handle))
    {
        JITDUMP("Optimizing compare of types-from-handles to instead compare handles\n");
        GenTree*             op1ClassFromHandle = tree->AsOp()->gtOp1->AsCall()->gtCallArgs->GetNode();
        GenTree*             op2ClassFromHandle = tree->AsOp()->gtOp2->AsCall()->gtCallArgs->GetNode();
        CORINFO_CLASS_HANDLE cls1Hnd            = NO_CLASS_HANDLE;
        CORINFO_CLASS_HANDLE cls2Hnd            = NO_CLASS_HANDLE;

        // Try and find class handles from op1 and op2
        cls1Hnd = gtGetHelperArgClassHandle(op1ClassFromHandle);
        cls2Hnd = gtGetHelperArgClassHandle(op2ClassFromHandle);

        // If we have both class handles, try and resolve the type equality test completely.
        bool resolveFailed = false;

        if ((cls1Hnd != NO_CLASS_HANDLE) && (cls2Hnd != NO_CLASS_HANDLE))
        {
            JITDUMP("Asking runtime to compare %p (%s) and %p (%s) for equality\n", dspPtr(cls1Hnd),
                    info.compCompHnd->getClassName(cls1Hnd), dspPtr(cls2Hnd), info.compCompHnd->getClassName(cls2Hnd));
            TypeCompareState s = info.compCompHnd->compareTypesForEquality(cls1Hnd, cls2Hnd);

            if (s != TypeCompareState::May)
            {
                // Type comparison result is known.
                const bool typesAreEqual = (s == TypeCompareState::Must);
                const bool operatorIsEQ  = (oper == GT_EQ);
                const int  compareResult = operatorIsEQ ^ typesAreEqual ? 0 : 1;
                JITDUMP("Runtime reports comparison is known at jit time: %u\n", compareResult);
                GenTree* result = gtNewIconNode(compareResult);
                return result;
            }
            else
            {
                resolveFailed = true;
            }
        }

        if (resolveFailed)
        {
            JITDUMP("Runtime reports comparison is NOT known at jit time\n");
        }
        else
        {
            JITDUMP("Could not find handle for %s%s\n", (cls1Hnd == NO_CLASS_HANDLE) ? " cls1" : "",
                    (cls2Hnd == NO_CLASS_HANDLE) ? " cls2" : "");
        }

        // We can't answer the equality comparison definitively at jit
        // time, but can still simplify the comparison.
        //
        // Find out how we can compare the two handles.
        // NOTE: We're potentially passing NO_CLASS_HANDLE, but the runtime knows what to do with it here.
        CorInfoInlineTypeCheck inliningKind =
            info.compCompHnd->canInlineTypeCheck(cls1Hnd, CORINFO_INLINE_TYPECHECK_SOURCE_TOKEN);

        // If the first type needs helper, check the other type: it might be okay with a simple compare.
        if (inliningKind == CORINFO_INLINE_TYPECHECK_USE_HELPER)
        {
            inliningKind = info.compCompHnd->canInlineTypeCheck(cls2Hnd, CORINFO_INLINE_TYPECHECK_SOURCE_TOKEN);
        }

        assert(inliningKind == CORINFO_INLINE_TYPECHECK_PASS || inliningKind == CORINFO_INLINE_TYPECHECK_USE_HELPER);

        GenTree* compare = gtCreateHandleCompare(oper, op1ClassFromHandle, op2ClassFromHandle, inliningKind);

        // Drop any now-irrelvant flags
        compare->gtFlags |= tree->gtFlags & (GTF_RELOP_JMP_USED | GTF_DONT_CSE);

        return compare;
    }

    if ((op1Kind == TPK_GetType) && (op2Kind == TPK_GetType))
    {
        GenTree* arg1;

        if (op1->OperGet() == GT_INTRINSIC)
        {
            arg1 = op1->AsUnOp()->gtOp1;
        }
        else
        {
            arg1 = op1->AsCall()->gtCallThisArg->GetNode();
        }

        arg1 = gtNewMethodTableLookup(arg1);

        GenTree* arg2;

        if (op2->OperGet() == GT_INTRINSIC)
        {
            arg2 = op2->AsUnOp()->gtOp1;
        }
        else
        {
            arg2 = op2->AsCall()->gtCallThisArg->GetNode();
        }

        arg2 = gtNewMethodTableLookup(arg2);

        CorInfoInlineTypeCheck inliningKind =
            info.compCompHnd->canInlineTypeCheck(nullptr, CORINFO_INLINE_TYPECHECK_SOURCE_VTABLE);
        assert(inliningKind == CORINFO_INLINE_TYPECHECK_PASS || inliningKind == CORINFO_INLINE_TYPECHECK_USE_HELPER);

        GenTree* compare = gtCreateHandleCompare(oper, arg1, arg2, inliningKind);

        // Drop any now-irrelvant flags
        compare->gtFlags |= tree->gtFlags & (GTF_RELOP_JMP_USED | GTF_DONT_CSE);

        return compare;
    }

    // If one operand creates a type from a handle and the other operand is fetching the type from an object,
    // we can sometimes optimize the type compare into a simpler
    // method table comparison.
    //
    // TODO: if other operand is null...
    if (!(((op1Kind == TPK_GetType) && (op2Kind == TPK_Handle)) ||
          ((op1Kind == TPK_Handle) && (op2Kind == TPK_GetType))))
    {
        return tree;
    }

    GenTree* const opHandle = (op1Kind == TPK_Handle) ? op1 : op2;
    GenTree* const opOther  = (op1Kind == TPK_Handle) ? op2 : op1;

    // Tunnel through the handle operand to get at the class handle involved.
    GenTree* const       opHandleArgument = opHandle->AsCall()->gtCallArgs->GetNode();
    CORINFO_CLASS_HANDLE clsHnd           = gtGetHelperArgClassHandle(opHandleArgument);

    // If we couldn't find the class handle, give up.
    if (clsHnd == NO_CLASS_HANDLE)
    {
        return tree;
    }

    // Ask the VM if this type can be equality tested by a simple method
    // table comparison.
    CorInfoInlineTypeCheck typeCheckInliningResult =
        info.compCompHnd->canInlineTypeCheck(clsHnd, CORINFO_INLINE_TYPECHECK_SOURCE_VTABLE);
    if (typeCheckInliningResult == CORINFO_INLINE_TYPECHECK_NONE)
    {
        return tree;
    }

    // We're good to go.
    JITDUMP("Optimizing compare of obj.GetType()"
            " and type-from-handle to compare method table pointer\n");

    // opHandleArgument is the method table we're looking for.
    GenTree* const knownMT = opHandleArgument;

    // Fetch object method table from the object itself.
    GenTree* objOp = nullptr;

    // Note we may see intrinsified or regular calls to GetType
    if (opOther->OperGet() == GT_INTRINSIC)
    {
        objOp = opOther->AsUnOp()->gtOp1;
    }
    else
    {
        objOp = opOther->AsCall()->gtCallThisArg->GetNode();
    }

    bool                 pIsExact   = false;
    bool                 pIsNonNull = false;
    CORINFO_CLASS_HANDLE objCls     = gtGetClassHandle(objOp, &pIsExact, &pIsNonNull);

    // if both classes are "final" (e.g. System.String[]) we can replace the comparison
    // with `true/false` + null check.
    if ((objCls != NO_CLASS_HANDLE) && (pIsExact || impIsClassExact(objCls)))
    {
        TypeCompareState tcs = info.compCompHnd->compareTypesForEquality(objCls, clsHnd);
        if (tcs != TypeCompareState::May)
        {
            const bool operatorIsEQ  = oper == GT_EQ;
            const bool typesAreEqual = tcs == TypeCompareState::Must;
            GenTree*   compareResult = gtNewIconNode((operatorIsEQ ^ typesAreEqual) ? 0 : 1);

            if (!pIsNonNull)
            {
                // we still have to emit a null-check
                // obj.GetType == typeof() -> (nullcheck) true/false
                GenTree* nullcheck = gtNewNullCheck(objOp, compCurBB);
                return gtNewCommaNode(nullcheck, compareResult);
            }
            else if (objOp->GetSideEffects() != 0)
            {
                return gtNewCommaNode(objOp, compareResult);
            }
            else
            {
                return compareResult;
            }
        }
    }

    // Fetch the method table from the object
    GenTree* const objMT = gtNewMethodTableLookup(objOp);

    // Compare the two method tables
    GenTree* const compare = gtCreateHandleCompare(oper, objMT, knownMT, typeCheckInliningResult);

    // Drop any now irrelevant flags
    compare->gtFlags |= tree->gtFlags & (GTF_RELOP_JMP_USED | GTF_DONT_CSE);

    // And we're done
    return compare;
}

//------------------------------------------------------------------------
// gtGetHelperArgClassHandle: find the compile time class handle from
//   a helper call argument tree
//
// Arguments:
//    tree - tree that passes the handle to the helper
//
// Returns:
//    The compile time class handle if known.
//
CORINFO_CLASS_HANDLE Compiler::gtGetHelperArgClassHandle(GenTree* tree)
{
    CORINFO_CLASS_HANDLE result = NO_CLASS_HANDLE;

    // Walk through any wrapping nop.
    if ((tree->gtOper == GT_NOP) && (tree->gtType == TYP_I_IMPL))
    {
        tree = tree->AsOp()->gtOp1;
    }

    // The handle could be a literal constant
    if ((tree->OperGet() == GT_CNS_INT) && (tree->TypeGet() == TYP_I_IMPL))
    {
        assert(tree->IsIconHandle(GTF_ICON_CLASS_HDL));
        result = (CORINFO_CLASS_HANDLE)tree->AsIntCon()->gtCompileTimeHandle;
    }
    // Or the result of a runtime lookup
    else if (tree->OperGet() == GT_RUNTIMELOOKUP)
    {
        result = tree->AsRuntimeLookup()->GetClassHandle();
    }
    // Or something reached indirectly
    else if (tree->gtOper == GT_IND)
    {
        // The handle indirs we are looking for will be marked as non-faulting.
        // Certain others (eg from refanytype) may not be.
        if (tree->gtFlags & GTF_IND_NONFAULTING)
        {
            GenTree* handleTreeInternal = tree->AsOp()->gtOp1;

            if ((handleTreeInternal->OperGet() == GT_CNS_INT) && (handleTreeInternal->TypeGet() == TYP_I_IMPL))
            {
                // These handle constants should be class handles.
                assert(handleTreeInternal->IsIconHandle(GTF_ICON_CLASS_HDL));
                result = (CORINFO_CLASS_HANDLE)handleTreeInternal->AsIntCon()->gtCompileTimeHandle;
            }
        }
    }

    return result;
}

//------------------------------------------------------------------------
// gtFoldExprSpecial -- optimize binary ops with one constant operand
//
// Arguments:
//   tree - tree to optimize
//
// Return value:
//   Tree (possibly modified at root or below), or a new tree
//   Any new tree is fully morphed, if necessary.
//
GenTree* Compiler::gtFoldExprSpecial(GenTree* tree)
{
    if (GenTreeQmark* qmark = tree->IsQmark())
    {
        if (GenTreeIntCon* cond = qmark->GetCondition()->IsIntCon())
        {
            JITDUMPTREE(tree, "Folding QMARK \n");
            GenTree* result = cond->GetValue() != 0 ? qmark->GetThen() : qmark->GetElse();
            JITDUMPTREE(result, "to \n");
            return result;
        }

        return qmark;
    }

    GenTree*   op1  = tree->AsOp()->gtOp1;
    GenTree*   op2  = tree->AsOp()->gtOp2;
    genTreeOps oper = tree->OperGet();

    GenTree* op;
    GenTree* cons;
    ssize_t  val;

    assert(tree->OperKind() & GTK_BINOP);

    /* Filter out operators that cannot be folded here */
    if (oper == GT_CAST)
    {
        return tree;
    }

    /* We only consider TYP_INT for folding
     * Do not fold pointer arithmetic (e.g. addressing modes!) */

    if (oper != GT_QMARK && !varTypeIsIntOrI(tree->gtType))
    {
        return tree;
    }

    /* Find out which is the constant node */

    if (op1->IsCnsIntOrI())
    {
        op   = op2;
        cons = op1;
    }
    else if (op2->IsCnsIntOrI())
    {
        op   = op1;
        cons = op2;
    }
    else
    {
        return tree;
    }

    /* Get the constant value */

    val = cons->AsIntConCommon()->IconValue();

    // Transforms that would drop op cannot be performed if op has side effects
    bool opHasSideEffects = (op->gtFlags & GTF_SIDE_EFFECT) != 0;

    // Helper function that creates a new IntCon node and morphs it, if required
    auto NewMorphedIntConNode = [&](int value) -> GenTreeIntCon* {
        GenTreeIntCon* icon = gtNewIconNode(value);
        if (fgGlobalMorph)
        {
            fgMorphTreeDone(icon);
        }
        return icon;
    };

    // Here `op` is the non-constant operand, `cons` is the constant operand
    // and `val` is the constant value.

    switch (oper)
    {
        case GT_LE:
            if (tree->IsUnsigned() && (val == 0) && (op1 == cons) && !opHasSideEffects)
            {
                // unsigned (0 <= x) is always true
                op = NewMorphedIntConNode(1);
                goto DONE_FOLD;
            }
            break;

        case GT_GE:
            if (tree->IsUnsigned() && (val == 0) && (op2 == cons) && !opHasSideEffects)
            {
                // unsigned (x >= 0) is always true
                op = NewMorphedIntConNode(1);
                goto DONE_FOLD;
            }
            break;

        case GT_LT:
            if (tree->IsUnsigned() && (val == 0) && (op2 == cons) && !opHasSideEffects)
            {
                // unsigned (x < 0) is always false
                op = NewMorphedIntConNode(0);
                goto DONE_FOLD;
            }
            break;

        case GT_GT:
            if (tree->IsUnsigned() && (val == 0) && (op1 == cons) && !opHasSideEffects)
            {
                // unsigned (0 > x) is always false
                op = NewMorphedIntConNode(0);
                goto DONE_FOLD;
            }
            FALLTHROUGH;
        case GT_EQ:
        case GT_NE:

            // Optimize boxed value classes; these are always false.  This IL is
            // generated when a generic value is tested against null:
            //     <T> ... foo(T x) { ... if ((object)x == null) ...
            if ((val == 0) && op->IsBox())
            {
                JITDUMP("\nAttempting to optimize BOX(valueType) %s null [%06u]\n", GenTree::OpName(oper),
                        dspTreeID(tree));

                // We don't expect GT_GT with signed compares, and we
                // can't predict the result if we do see it, since the
                // boxed object addr could have its high bit set.
                if ((oper == GT_GT) && !tree->IsUnsigned())
                {
                    JITDUMP(" bailing; unexpected signed compare via GT_GT\n");
                }
                else
                {
                    // The tree under the box must be side effect free
                    // since we will drop it if we optimize.
                    assert(!gtTreeHasSideEffects(op->AsBox()->GetOp(0), GTF_SIDE_EFFECT));

                    // See if we can optimize away the box and related statements.
                    GenTree* boxSourceTree = gtTryRemoveBoxUpstreamEffects(op->AsBox());
                    bool     didOptimize   = (boxSourceTree != nullptr);

                    // If optimization succeeded, remove the box.
                    if (didOptimize)
                    {
                        // Set up the result of the compare.
                        int compareResult = 0;
                        if (oper == GT_GT)
                        {
                            // GT_GT(null, box) == false
                            // GT_GT(box, null) == true
                            compareResult = (op1 == op);
                        }
                        else if (oper == GT_EQ)
                        {
                            // GT_EQ(box, null) == false
                            // GT_EQ(null, box) == false
                            compareResult = 0;
                        }
                        else
                        {
                            assert(oper == GT_NE);
                            // GT_NE(box, null) == true
                            // GT_NE(null, box) == true
                            compareResult = 1;
                        }

                        JITDUMP("\nSuccess: replacing BOX(valueType) %s null with %d\n", GenTree::OpName(oper),
                                compareResult);

                        return NewMorphedIntConNode(compareResult);
                    }
                }
            }
            else
            {
                return gtFoldBoxNullable(tree);
            }

            break;

        case GT_ADD:
            if (val == 0)
            {
                goto DONE_FOLD;
            }
            break;

        case GT_MUL:
            if (val == 1)
            {
                goto DONE_FOLD;
            }
            else if (val == 0)
            {
                /* Multiply by zero - return the 'zero' node, but not if side effects */
                if (!opHasSideEffects)
                {
                    op = cons;
                    goto DONE_FOLD;
                }
            }
            break;

        case GT_DIV:
        case GT_UDIV:
            if ((op2 == cons) && (val == 1) && !op1->OperIsConst())
            {
                goto DONE_FOLD;
            }
            break;

        case GT_SUB:
            if ((op2 == cons) && (val == 0) && !op1->OperIsConst())
            {
                goto DONE_FOLD;
            }
            break;

        case GT_AND:
            if (val == 0)
            {
                /* AND with zero - return the 'zero' node, but not if side effects */

                if (!opHasSideEffects)
                {
                    op = cons;
                    goto DONE_FOLD;
                }
            }
            else
            {
                /* The GTF_BOOLEAN flag is set for nodes that are part
                 * of a boolean expression, thus all their children
                 * are known to evaluate to only 0 or 1 */

                if (tree->gtFlags & GTF_BOOLEAN)
                {

                    /* The constant value must be 1
                     * AND with 1 stays the same */
                    assert(val == 1);
                    goto DONE_FOLD;
                }
            }
            break;

        case GT_OR:
            if (val == 0)
            {
                goto DONE_FOLD;
            }
            else if (tree->gtFlags & GTF_BOOLEAN)
            {
                /* The constant value must be 1 - OR with 1 is 1 */

                assert(val == 1);

                /* OR with one - return the 'one' node, but not if side effects */

                if (!opHasSideEffects)
                {
                    op = cons;
                    goto DONE_FOLD;
                }
            }
            break;

        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:
        case GT_ROL:
        case GT_ROR:
            if (val == 0)
            {
                if (op2 == cons)
                {
                    goto DONE_FOLD;
                }
                else if (!opHasSideEffects)
                {
                    op = cons;
                    goto DONE_FOLD;
                }
            }
            break;

        default:
            break;
    }

    /* The node is not foldable */

    return tree;

DONE_FOLD:
    JITDUMPTREE(tree, "\nFolding binary operator with a constant operand:\n");
    JITDUMPTREE(op, "Transformed into:\n");

    return op;
}

//------------------------------------------------------------------------
// gtFoldBoxNullable -- optimize a boxed nullable feeding a compare to zero
//
// Arguments:
//   tree - binop tree to potentially optimize, must be
//          GT_GT, GT_EQ, or GT_NE
//
// Return value:
//   Tree (possibly modified below the root).
//
GenTree* Compiler::gtFoldBoxNullable(GenTree* tree)
{
    assert(tree->OperKind() & GTK_BINOP);
    assert(tree->OperIs(GT_GT, GT_EQ, GT_NE));

    genTreeOps const oper = tree->OperGet();

    if ((oper == GT_GT) && !tree->IsUnsigned())
    {
        return tree;
    }

    GenTree* const op1 = tree->AsOp()->gtOp1;
    GenTree* const op2 = tree->AsOp()->gtOp2;
    GenTree*       op;
    GenTree*       cons;

    if (op1->IsCnsIntOrI())
    {
        op   = op2;
        cons = op1;
    }
    else if (op2->IsCnsIntOrI())
    {
        op   = op1;
        cons = op2;
    }
    else
    {
        return tree;
    }

    ssize_t const val = cons->AsIntConCommon()->IconValue();

    if (val != 0)
    {
        return tree;
    }

    if (!op->IsCall())
    {
        return tree;
    }

    GenTreeCall* const call = op->AsCall();

    if (!call->IsHelperCall(this, CORINFO_HELP_BOX_NULLABLE))
    {
        return tree;
    }

    JITDUMP("\nAttempting to optimize BOX_NULLABLE(&x) %s null [%06u]\n", GenTree::OpName(oper), dspTreeID(tree));

    // Get the address of the struct being boxed
    GenTree* arg;

    if (call->GetInfo() == nullptr)
    {
        arg = call->gtCallArgs->GetNext()->GetNode();
    }
    else
    {
        arg = call->GetArgNodeByArgNum(1);
    }

    ClassLayout* nullableLayout = nullptr;

    if (GenTreeFieldAddr* fieldAddr = arg->IsFieldAddr())
    {
        nullableLayout = fieldAddr->GetLayout(this);
    }
    else if (GenTreeIndexAddr* indexAddr = arg->IsIndexAddr())
    {
        nullableLayout = indexAddr->GetLayout(this);
    }
    else if (arg->OperIs(GT_LCL_VAR_ADDR))
    {
        nullableLayout = lvaGetDesc(arg->AsLclVar())->GetLayout();
    }

    if (nullableLayout == nullptr)
    {
        return tree;
    }

    CORINFO_FIELD_HANDLE fieldHnd    = info.compCompHnd->getFieldInClass(nullableLayout->GetClassHandle(), 0);
    unsigned             fieldOffset = info.compCompHnd->getFieldOffset(fieldHnd);

    // Replace the box with an access of the nullable 'hasValue' field.
    JITDUMP("\nSuccess: replacing BOX_NULLABLE(&x) [%06u] with x.hasValue\n", dspTreeID(op));

    GenTree* newOp;

    if (arg->OperIs(GT_LCL_VAR_ADDR) && lvaAddressExposedLocalsMarked)
    {
        newOp = arg->ChangeToLclFld(TYP_BOOL, arg->AsLclVar()->GetLclNum(), fieldOffset,
                                    GetFieldSeqStore()->CreateSingleton(fieldHnd));
    }
    else
    {
        newOp = gtNewFieldIndir(TYP_BOOL, gtNewFieldAddr(arg, fieldHnd, fieldOffset));
    }

    if (op == op1)
    {
        tree->AsOp()->gtOp1 = newOp;
    }
    else
    {
        tree->AsOp()->gtOp2 = newOp;
    }

    tree->SetSideEffects(newOp->GetSideEffects());

    cons->gtType = TYP_INT;

    return tree;
}

//------------------------------------------------------------------------
// gtTryRemoveBoxUpstreamEffects: given an unused value type box,
//    try and remove the upstream allocation and unnecessary parts of
//    the copy.
//
// Arguments:
//    op  - the box node to optimize
//    options - controls whether and how trees are modified
//        (see notes)
//
// Return Value:
//    A tree representing the original value to box, if removal
//    is successful/possible (but see note). nullptr if removal fails.
//
// Notes:
//    Value typed box gets special treatment because it has associated
//    side effects that can be removed if the box result is not used.
//
//    By default (options == BR_REMOVE_AND_NARROW) this method will
//    try and remove unnecessary trees and will try and reduce remaning
//    operations to the minimal set, possibly narrowing the width of
//    loads from the box source if it is a struct.
//
//    To perform a trial removal, pass BR_DONT_REMOVE. This can be
//    useful to determine if this optimization should only be
//    performed if some other conditions hold true.
//
//    To remove but not alter the access to the box source, pass
//    BR_REMOVE_BUT_NOT_NARROW.
//
//    To remove and return the tree for the type handle used for
//    the boxed newobj, pass BR_REMOVE_BUT_NOT_NARROW_WANT_TYPE_HANDLE.
//    This can be useful when the only part of the box that is "live"
//    is its type.
//
//    If removal fails, is is possible that a subsequent pass may be
//    able to optimize.  Blocking side effects may now be minimized
//    (null or bounds checks might have been removed) or might be
//    better known (inline return placeholder updated with the actual
//    return expression). So the box is perhaps best left as is to
//    help trigger this re-examination.

GenTree* Compiler::gtTryRemoveBoxUpstreamEffects(GenTreeBox* box, BoxRemovalOptions options)
{
    Statement* asgStmt  = box->gtAsgStmtWhenInlinedBoxValue;
    Statement* copyStmt = box->gtCopyStmtWhenInlinedBoxValue;

    JITDUMP("gtTryRemoveBoxUpstreamEffects: %s to %s of BOX (valuetype)"
            " [%06u] (assign/newobj " FMT_STMT " copy " FMT_STMT "\n",
            (options == BR_DONT_REMOVE) ? "checking if it is possible" : "attempting",
            (options == BR_MAKE_LOCAL_COPY) ? "make local unboxed version" : "remove side effects", box->GetID(),
            asgStmt->GetID(), copyStmt->GetID());

    DISPSTMT(asgStmt);
    DISPSTMT(copyStmt);

    // If we don't recognize the form of the assign, bail.
    GenTree* asg = asgStmt->GetRootNode();
    if (asg->gtOper != GT_ASG)
    {
        JITDUMP(" bailing; unexpected assignment op %s\n", GenTree::OpName(asg->gtOper));
        return nullptr;
    }

    // If we're eventually going to return the type handle, remember it now.
    GenTree* boxTypeHandle = nullptr;
    if ((options == BR_REMOVE_AND_NARROW_WANT_TYPE_HANDLE) || (options == BR_DONT_REMOVE_WANT_TYPE_HANDLE))
    {
        GenTree*   asgSrc     = asg->AsOp()->gtOp2;
        genTreeOps asgSrcOper = asgSrc->OperGet();

        // Allocation may be via AllocObj or via helper call, depending
        // on when this is invoked and whether the jit is using AllocObj
        // for R2R allocations.
        if (asgSrcOper == GT_ALLOCOBJ)
        {
            GenTreeAllocObj* allocObj = asgSrc->AsAllocObj();
            boxTypeHandle             = allocObj->AsOp()->gtOp1;
        }
        else if (asgSrcOper == GT_CALL)
        {
            GenTreeCall*      newobjCall = asgSrc->AsCall();
            GenTreeCall::Use* newobjArgs = newobjCall->gtCallArgs;

            // In R2R expansions the handle may not be an explicit operand to the helper,
            // so we can't remove the box.
            if (newobjArgs == nullptr)
            {
                assert(newobjCall->IsHelperCall(this, CORINFO_HELP_READYTORUN_NEW));
                JITDUMP(" bailing; newobj via R2R helper\n");
                return nullptr;
            }

            boxTypeHandle = newobjArgs->GetNode();
        }
        else
        {
            unreached();
        }

        assert(boxTypeHandle != nullptr);
    }

    // If we don't recognize the form of the copy, bail.
    GenTree* copy = copyStmt->GetRootNode();
    if (copy->gtOper != GT_ASG)
    {
        // GT_RET_EXPR is a tolerable temporary failure.
        // The jit will revisit this optimization after
        // inlining is done.
        if (copy->gtOper == GT_RET_EXPR)
        {
            JITDUMP(" bailing; must wait for replacement of copy %s\n", GenTree::OpName(copy->gtOper));
        }
        else
        {
            // Anything else is a missed case we should
            // figure out how to handle.  One known case
            // is GT_COMMAs enclosing the GT_ASG we are
            // looking for.
            JITDUMP(" bailing; unexpected copy op %s\n", GenTree::OpName(copy->gtOper));
        }
        return nullptr;
    }

    // Handle case where we are optimizing the box into a local copy
    if (options == BR_MAKE_LOCAL_COPY)
    {
        // Drill into the box to get at the box temp local and the box type
        GenTreeLclVar* boxTemp = box->GetOp(0)->AsLclVar();
        assert(boxTemp->OperIs(GT_LCL_VAR));
        const unsigned boxTempLclNum = boxTemp->GetLclNum();
        LclVarDsc*     boxTempLclDsc = lvaGetDesc(boxTempLclNum);
        assert(boxTempLclDsc->TypeGet() == TYP_REF);
        assert(boxTempLclDsc->lvClassHnd != nullptr);

        // Verify that the copyDst has the expected shape
        // (blk|obj|ind (add (boxTempLclNum, ptr-size)))
        //
        // The shape here is constrained to the patterns we produce
        // over in impImportAndPushBox for the inlined box case.
        GenTree* copyDst = copy->AsOp()->gtOp1;

        if (!copyDst->OperIs(GT_BLK, GT_IND, GT_OBJ))
        {
            JITDUMP("Unexpected copy dest operator %s\n", GenTree::OpName(copyDst->gtOper));
            return nullptr;
        }

        GenTree* copyDstAddr = copyDst->AsOp()->gtOp1;
        if (copyDstAddr->OperGet() != GT_ADD)
        {
            JITDUMP("Unexpected copy dest address tree\n");
            return nullptr;
        }

        GenTree* copyDstAddrOp1 = copyDstAddr->AsOp()->gtOp1;
        if ((copyDstAddrOp1->OperGet() != GT_LCL_VAR) || (copyDstAddrOp1->AsLclVar()->GetLclNum() != boxTempLclNum))
        {
            JITDUMP("Unexpected copy dest address 1st addend\n");
            return nullptr;
        }

        GenTree* copyDstAddrOp2 = copyDstAddr->AsOp()->gtOp2;
        if (!copyDstAddrOp2->IsIntegralConst(TARGET_POINTER_SIZE))
        {
            JITDUMP("Unexpected copy dest address 2nd addend\n");
            return nullptr;
        }

        // Screening checks have all passed. Do the transformation.
        //

        // Remove the newobj and assigment to box temp
        JITDUMP("Bashing NEWOBJ [%06u] to NOP\n", dspTreeID(asg));
        asg->ChangeToNothingNode();

        if (varTypeIsStruct(copyDst->GetType()))
        {
            JITDUMP("Retyping box temp V%02u to struct %s\n", boxTempLclNum, eeGetClassName(boxTempLclDsc->lvClassHnd));
            lvaSetStruct(boxTempLclNum, boxTempLclDsc->lvClassHnd, /* checkUnsafeBuffer */ false);
        }
        else
        {
            assert(copyDst->GetType() == JITtype2varType(info.compCompHnd->asCorInfoType(boxTempLclDsc->lvClassHnd)));
            JITDUMP("Retyping box temp V%02u to primitive %s\n", boxTempLclNum, varTypeName(copyDst->GetType()));
            boxTempLclDsc->SetType(copyDst->GetType());
        }

        copyDst->ChangeOper(GT_LCL_VAR);
        copyDst->gtFlags |= GTF_VAR_DEF;
        copyDst->gtFlags &= ~GTF_ALL_EFFECT;
        copyDst->AsLclVar()->SetLclNum(boxTempLclNum);

        DISPSTMT(copyStmt);

        // Return the address of the now-struct typed box temp
        return gtNewLclVarAddrNode(boxTempLclNum, TYP_BYREF);
    }

    // If the copy is a struct copy, make sure we know how to isolate
    // any source side effects.
    GenTree* copySrc = copy->AsOp()->gtOp2;

    // If the copy source is from a pending inline, wait for it to resolve.
    if (copySrc->gtOper == GT_RET_EXPR)
    {
        JITDUMP(" bailing; must wait for replacement of copy source %s\n", GenTree::OpName(copySrc->gtOper));
        return nullptr;
    }

    bool hasSrcSideEffect = false;
    bool isStructCopy     = false;

    if (gtTreeHasSideEffects(copySrc, GTF_SIDE_EFFECT))
    {
        hasSrcSideEffect = true;

        if (varTypeIsStruct(copySrc->gtType))
        {
            isStructCopy = true;

            if (!copySrc->OperIs(GT_OBJ, GT_IND))
            {
                // We don't know how to handle other cases, yet.
                JITDUMP(" bailing; unexpected copy source struct op with side effect %s\n",
                        GenTree::OpName(copySrc->gtOper));
                return nullptr;
            }
        }
    }

    // If this was a trial removal, we're done.
    if (options == BR_DONT_REMOVE)
    {
        return copySrc;
    }

    if (options == BR_DONT_REMOVE_WANT_TYPE_HANDLE)
    {
        return boxTypeHandle;
    }

    // Otherwise, proceed with the optimization.
    //
    // Change the assignment expression to a NOP.
    JITDUMP("\nBashing NEWOBJ [%06u] to NOP\n", dspTreeID(asg));
    asg->ChangeToNothingNode();

    // Change the copy expression so it preserves key
    // source side effects.
    JITDUMP("\nBashing COPY [%06u]", dspTreeID(copy));

    if (!hasSrcSideEffect)
    {
        // If there were no copy source side effects just bash
        // the copy to a NOP.
        copy->ChangeToNothingNode();
        JITDUMP(" to NOP; no source side effects.\n");
    }
    else if (!isStructCopy)
    {
        // For scalar types, go ahead and produce the
        // value as the copy is fairly cheap and likely
        // the optimizer can trim things down to just the
        // minimal side effect parts.
        copyStmt->SetRootNode(copySrc);
        JITDUMP(" to scalar read via [%06u]\n", dspTreeID(copySrc));
    }
    else
    {
        // For struct types read the first byte of the
        // source struct; there's no need to read the
        // entire thing, and no place to put it.
        assert(copySrc->OperIs(GT_OBJ, GT_IND));
        copyStmt->SetRootNode(copySrc);

        if (options == BR_REMOVE_AND_NARROW || options == BR_REMOVE_AND_NARROW_WANT_TYPE_HANDLE)
        {
            JITDUMP(" to read first byte of struct via modified [%06u]\n", dspTreeID(copySrc));

            GenTree* addr = copySrc->AsIndir()->GetAddr();

            if (GenTreeFieldAddr* field = addr->IsFieldAddr())
            {
                while (GenTreeFieldAddr* nextField = field->GetAddr()->IsFieldAddr())
                {
                    field = nextField;
                }

                copySrc = field;
            }

            gtChangeOperToNullCheck(copySrc, compCurBB);
            copyStmt->SetRootNode(copySrc);
        }
        else
        {
            JITDUMP(" to read entire struct via modified [%06u]\n", dspTreeID(copySrc));
        }
    }

    if (fgStmtListThreaded)
    {
        fgSetStmtSeq(asgStmt);
        fgSetStmtSeq(copyStmt);
    }

    // Box effects were successfully optimized.

    if (options == BR_REMOVE_AND_NARROW_WANT_TYPE_HANDLE)
    {
        return boxTypeHandle;
    }
    else
    {
        return copySrc;
    }
}

//------------------------------------------------------------------------
// gtOptimizeEnumHasFlag: given the operands for a call to Enum.HasFlag,
//    try and optimize the call to a simple and/compare tree.
//
// Arguments:
//    thisOp  - first argument to the call
//    flagOp  - second argument to the call
//
// Return Value:
//    A new cmp/amd tree if successful. nullptr on failure.
//
// Notes:
//    If successful, may allocate new temps and modify connected
//    statements.

GenTree* Compiler::gtOptimizeEnumHasFlag(GenTree* thisOp, GenTree* flagOp)
{
    JITDUMP("Considering optimizing call to Enum.HasFlag....\n");

    // Operands must be boxes
    if (!thisOp->IsBox() || !flagOp->IsBox())
    {
        JITDUMP("bailing, need both inputs to be BOXes\n");
        return nullptr;
    }

    // Operands must have same type
    bool                 isExactThis   = false;
    bool                 isNonNullThis = false;
    CORINFO_CLASS_HANDLE thisHnd       = gtGetClassHandle(thisOp, &isExactThis, &isNonNullThis);

    if (thisHnd == nullptr)
    {
        JITDUMP("bailing, can't find type for 'this' operand\n");
        return nullptr;
    }

    // A boxed thisOp should have exact type and non-null instance
    assert(isExactThis);
    assert(isNonNullThis);

    bool                 isExactFlag   = false;
    bool                 isNonNullFlag = false;
    CORINFO_CLASS_HANDLE flagHnd       = gtGetClassHandle(flagOp, &isExactFlag, &isNonNullFlag);

    if (flagHnd == nullptr)
    {
        JITDUMP("bailing, can't find type for 'flag' operand\n");
        return nullptr;
    }

    // A boxed flagOp should have exact type and non-null instance
    assert(isExactFlag);
    assert(isNonNullFlag);

    if (flagHnd != thisHnd)
    {
        JITDUMP("bailing, operand types differ\n");
        return nullptr;
    }

    // If we have a shared type instance we can't safely check type
    // equality, so bail.
    DWORD classAttribs = info.compCompHnd->getClassAttribs(thisHnd);
    if (classAttribs & CORINFO_FLG_SHAREDINST)
    {
        JITDUMP("bailing, have shared instance type\n");
        return nullptr;
    }

    // Simulate removing the box for thisOP. We need to know that it can
    // be safely removed before we can optimize.
    GenTree* thisVal = gtTryRemoveBoxUpstreamEffects(thisOp->AsBox(), BR_DONT_REMOVE);
    if (thisVal == nullptr)
    {
        // Note we may fail here if the this operand comes from
        // a call. We should be able to retry this post-inlining.
        JITDUMP("bailing, can't undo box of 'this' operand\n");
        return nullptr;
    }

    // Do likewise with flagOp.
    GenTree* flagVal = gtTryRemoveBoxUpstreamEffects(flagOp->AsBox(), BR_DONT_REMOVE);
    if (flagVal == nullptr)
    {
        // Note we may fail here if the flag operand comes from
        // a call. We should be able to retry this post-inlining.
        JITDUMP("bailing, can't undo box of 'flag' operand\n");
        return nullptr;
    }

    // Only proceed when both box sources have the same actual type.
    // (this rules out long/int mismatches)
    if (genActualType(thisVal->TypeGet()) != genActualType(flagVal->TypeGet()))
    {
        JITDUMP("bailing, pre-boxed values have different types\n");
        return nullptr;
    }

    // Yes, both boxes can be cleaned up. Optimize.
    JITDUMP("Optimizing call to Enum.HasFlag\n");

    // Undo the boxing of the Ops and prepare to operate directly
    // on the pre-boxed values.
    thisVal = gtTryRemoveBoxUpstreamEffects(thisOp->AsBox(), BR_REMOVE_BUT_NOT_NARROW);
    flagVal = gtTryRemoveBoxUpstreamEffects(flagOp->AsBox(), BR_REMOVE_BUT_NOT_NARROW);

    // Our trial removals above should guarantee successful removals here.
    assert(thisVal != nullptr);
    assert(flagVal != nullptr);

    // Type to use for optimized check
    var_types type = varActualType(thisVal->GetType());
    assert(type == varActualType(flagVal->GetType()));

    // The thisVal and flagVal trees come from earlier statements.
    //
    // Unless they are invariant values, we need to evaluate them both
    // to temps at those points to safely transmit the values here.
    //
    // Also we need to use the flag twice, so we need two trees for it.
    GenTree* thisValOpt     = nullptr;
    GenTree* flagValOpt     = nullptr;
    GenTree* flagValOptCopy = nullptr;

    if (thisVal->IsIntegralConst())
    {
        thisValOpt = gtClone(thisVal);
        assert(thisValOpt != nullptr);
    }
    else
    {
        unsigned   thisTmp     = lvaNewTemp(type, true DEBUGARG("Enum:HasFlag this temp"));
        GenTree*   thisAsg     = gtNewAssignNode(gtNewLclvNode(thisTmp, type), thisVal);
        Statement* thisAsgStmt = thisOp->AsBox()->gtCopyStmtWhenInlinedBoxValue;
        thisAsgStmt->SetRootNode(thisAsg);
        thisValOpt = gtNewLclvNode(thisTmp, type);
    }

    if (flagVal->IsIntegralConst())
    {
        flagValOpt = gtClone(flagVal);
        assert(flagValOpt != nullptr);
        flagValOptCopy = gtClone(flagVal);
        assert(flagValOptCopy != nullptr);
    }
    else
    {
        unsigned   flagTmp     = lvaNewTemp(type, true DEBUGARG("Enum:HasFlag flag temp"));
        GenTree*   flagAsg     = gtNewAssignNode(gtNewLclvNode(flagTmp, type), flagVal);
        Statement* flagAsgStmt = flagOp->AsBox()->gtCopyStmtWhenInlinedBoxValue;
        flagAsgStmt->SetRootNode(flagAsg);
        flagValOpt     = gtNewLclvNode(flagTmp, type);
        flagValOptCopy = gtNewLclvNode(flagTmp, type);
    }

    // Turn the call into (thisValTmp & flagTmp) == flagTmp.
    GenTree* andTree = gtNewOperNode(GT_AND, type, thisValOpt, flagValOpt);
    GenTree* cmpTree = gtNewOperNode(GT_EQ, TYP_INT, andTree, flagValOptCopy);

    JITDUMP("Optimized call to Enum.HasFlag\n");

    return cmpTree;
}

/*****************************************************************************
 *
 *  Fold the given constant tree.
 */

#ifdef _PREFAST_
#pragma warning(push)
#pragma warning(disable : 21000) // Suppress PREFast warning about overly large function
#endif
GenTree* Compiler::gtFoldExprConst(GenTree* tree)
{
    SSIZE_T       i1, i2, itemp;
    INT64         lval1, lval2, ltemp;
    float         f1, f2;
    double        d1, d2;
    var_types     switchType;
    FieldSeqNode* fieldSeq = FieldSeqStore::NotAField(); // default unless we override it when folding

    assert(tree->OperIsUnary() || tree->OperIsBinary());

    GenTree* op1 = tree->gtGetOp1();
    GenTree* op2 = tree->gtGetOp2IfPresent();

    if (!opts.OptEnabled(CLFLG_CONSTANTFOLD))
    {
        return tree;
    }

    if (tree->OperIs(GT_NOP, GT_ALLOCOBJ, GT_RUNTIMELOOKUP))
    {
        return tree;
    }

#ifdef FEATURE_HW_INTRINSICS
    if (tree->OperIs(GT_HWINTRINSIC))
    {
        return tree;
    }
#endif

    if (tree->OperIsUnary())
    {
        assert(op1->OperIsConst());

        switch (op1->gtType)
        {
            case TYP_INT:

                // Fold constant INT unary operator.

                if (!op1->AsIntCon()->ImmedValCanBeFolded(this, tree->OperGet()))
                {
                    return tree;
                }

                i1 = (INT32)op1->AsIntCon()->IconValue();

                switch (tree->GetOper())
                {
                    case GT_NOT:
                        i1 = ~i1;
                        break;

                    case GT_NEG:
                        i1 = -i1;
                        break;

                    case GT_BSWAP:
                        i1 = ((i1 >> 24) & 0xFF) | ((i1 >> 8) & 0xFF00) | ((i1 << 8) & 0xFF0000) |
                             ((i1 << 24) & 0xFF000000);
                        break;

                    case GT_BSWAP16:
                        i1 = ((i1 >> 8) & 0xFF) | ((i1 << 8) & 0xFF00);
                        break;

                    case GT_BITCAST:
                        if (tree->TypeIs(TYP_FLOAT))
                        {
                            d1 = jitstd::bit_cast<float>(static_cast<int>(i1));
                            goto CNS_DOUBLE;
                        }
                        return tree;

                    case GT_CAST:
                        // assert (genActualType(tree->CastToType()) == tree->TypeGet());

                        if (tree->gtOverflow() &&
                            CheckedOps::CastFromIntOverflows((INT32)i1, tree->CastToType(), tree->IsUnsigned()))
                        {
                            goto INTEGRAL_OVF;
                        }

                        switch (tree->CastToType())
                        {
                            case TYP_BYTE:
                                i1 = INT32(INT8(i1));
                                goto CNS_INT;

                            case TYP_SHORT:
                                i1 = INT32(INT16(i1));
                                goto CNS_INT;

                            case TYP_USHORT:
                                i1 = INT32(UINT16(i1));
                                goto CNS_INT;

                            case TYP_BOOL:
                            case TYP_UBYTE:
                                i1 = INT32(UINT8(i1));
                                goto CNS_INT;

                            case TYP_UINT:
                            case TYP_INT:
                                goto CNS_INT;

                            case TYP_ULONG:
                                if (tree->IsUnsigned())
                                {
                                    lval1 = UINT64(UINT32(i1));
                                }
                                else
                                {
                                    lval1 = UINT64(INT32(i1));
                                }
                                goto CNS_LONG;

                            case TYP_LONG:
                                if (tree->IsUnsigned())
                                {
                                    lval1 = INT64(UINT32(i1));
                                }
                                else
                                {
                                    lval1 = INT64(INT32(i1));
                                }
                                goto CNS_LONG;

                            case TYP_FLOAT:
                                if (tree->IsUnsigned())
                                {
                                    f1 = forceCastToFloat(UINT32(i1));
                                }
                                else
                                {
                                    f1 = forceCastToFloat(INT32(i1));
                                }
                                d1 = f1;
                                goto CNS_DOUBLE;

                            case TYP_DOUBLE:
                                if (tree->IsUnsigned())
                                {
                                    d1 = (double)UINT32(i1);
                                }
                                else
                                {
                                    d1 = (double)INT32(i1);
                                }
                                goto CNS_DOUBLE;

                            default:
                                assert(!"Bad CastToType() in gtFoldExprConst() for a cast from int");
                                return tree;
                        }

                    default:
                        return tree;
                }

                goto CNS_INT;

            case TYP_LONG:

                // Fold constant LONG unary operator.

                if (!op1->AsIntConCommon()->ImmedValCanBeFolded(this, tree->OperGet()))
                {
                    return tree;
                }

                lval1 = op1->AsIntConCommon()->LngValue();

                switch (tree->OperGet())
                {
                    case GT_NOT:
                        lval1 = ~lval1;
                        break;

                    case GT_NEG:
                        lval1 = -lval1;
                        break;

                    case GT_BSWAP:
                        lval1 = ((lval1 >> 56) & 0xFF) | ((lval1 >> 40) & 0xFF00) | ((lval1 >> 24) & 0xFF0000) |
                                ((lval1 >> 8) & 0xFF000000) | ((lval1 << 8) & 0xFF00000000) |
                                ((lval1 << 24) & 0xFF0000000000) | ((lval1 << 40) & 0xFF000000000000) |
                                ((lval1 << 56) & 0xFF00000000000000);
                        break;

                    case GT_BITCAST:
                        if (tree->TypeIs(TYP_DOUBLE))
                        {
                            d1 = jitstd::bit_cast<double>(lval1);
                            goto CNS_DOUBLE;
                        }
                        return tree;

                    case GT_CAST:
                        assert(tree->TypeIs(genActualType(tree->CastToType())));

                        if (tree->gtOverflow() &&
                            CheckedOps::CastFromLongOverflows(lval1, tree->CastToType(), tree->IsUnsigned()))
                        {
                            goto INTEGRAL_OVF;
                        }

                        switch (tree->CastToType())
                        {
                            case TYP_BYTE:
                                i1 = INT32(INT8(lval1));
                                goto CNS_INT;

                            case TYP_SHORT:
                                i1 = INT32(INT16(lval1));
                                goto CNS_INT;

                            case TYP_USHORT:
                                i1 = INT32(UINT16(lval1));
                                goto CNS_INT;

                            case TYP_UBYTE:
                                i1 = INT32(UINT8(lval1));
                                goto CNS_INT;

                            case TYP_INT:
                                i1 = INT32(lval1);
                                goto CNS_INT;

                            case TYP_UINT:
                                i1 = UINT32(lval1);
                                goto CNS_INT;

                            case TYP_ULONG:
                            case TYP_LONG:
                                goto CNS_LONG;

                            case TYP_FLOAT:
                            case TYP_DOUBLE:
                                if (tree->IsUnsigned() && (lval1 < 0))
                                {
                                    d1 = FloatingPointUtils::convertUInt64ToDouble((unsigned __int64)lval1);
                                }
                                else
                                {
                                    d1 = (double)lval1;
                                }

                                if (tree->CastToType() == TYP_FLOAT)
                                {
                                    f1 = forceCastToFloat(d1); // truncate precision
                                    d1 = f1;
                                }
                                goto CNS_DOUBLE;
                            default:
                                assert(!"Bad CastToType() in gtFoldExprConst() for a cast from long");
                                return tree;
                        }

                    default:
                        return tree;
                }

                goto CNS_LONG;

            case TYP_FLOAT:
            case TYP_DOUBLE:
                assert(op1->OperIs(GT_CNS_DBL));

                // Fold constant DOUBLE unary operator.

                d1 = op1->AsDblCon()->gtDconVal;

                switch (tree->OperGet())
                {
                    case GT_FNEG:
                        d1 = -d1;
                        break;

                    case GT_BITCAST:
                        if (tree->TypeIs(TYP_INT) && op1->TypeIs(TYP_FLOAT))
                        {
                            i1 = jitstd::bit_cast<int>(static_cast<float>(d1));
                            goto CNS_INT;
                        }

                        if (tree->TypeIs(TYP_LONG) && op1->TypeIs(TYP_DOUBLE))
                        {
                            lval1 = jitstd::bit_cast<INT64>(d1);
                            goto CNS_LONG;
                        }

                        return tree;

                    case GT_CAST:
                        f1 = forceCastToFloat(d1);

                        if ((op1->TypeIs(TYP_DOUBLE) && CheckedOps::CastFromDoubleOverflows(d1, tree->CastToType())) ||
                            (op1->TypeIs(TYP_FLOAT) && CheckedOps::CastFromFloatOverflows(f1, tree->CastToType())))
                        {
                            // The conversion overflows. The ECMA spec says, in III 3.27, that
                            // "...if overflow occurs converting a floating point type to an integer, ...,
                            // the value returned is unspecified."  However, it would at least be
                            // desirable to have the same value returned for casting an overflowing
                            // constant to an int as would be obtained by passing that constant as
                            // a parameter and then casting that parameter to an int type.

                            // Don't fold overflowing converions, as the value returned by
                            // JIT's codegen doesn't always match with the C compiler's cast result.
                            // We want the behavior to be the same with or without folding.

                            return tree;
                        }

                        assert(tree->TypeIs(genActualType(tree->CastToType())));

                        switch (tree->CastToType())
                        {
                            case TYP_BYTE:
                                i1 = INT32(INT8(d1));
                                goto CNS_INT;

                            case TYP_SHORT:
                                i1 = INT32(INT16(d1));
                                goto CNS_INT;

                            case TYP_USHORT:
                                i1 = INT32(UINT16(d1));
                                goto CNS_INT;

                            case TYP_UBYTE:
                                i1 = INT32(UINT8(d1));
                                goto CNS_INT;

                            case TYP_INT:
                                i1 = INT32(d1);
                                goto CNS_INT;

                            case TYP_UINT:
                                i1 = forceCastToUInt32(d1);
                                goto CNS_INT;

                            case TYP_LONG:
                                lval1 = INT64(d1);
                                goto CNS_LONG;

                            case TYP_ULONG:
                                lval1 = FloatingPointUtils::convertDoubleToUInt64(d1);
                                goto CNS_LONG;

                            case TYP_FLOAT:
                                d1 = forceCastToFloat(d1);
                                goto CNS_DOUBLE;

                            case TYP_DOUBLE:
                                if (op1->TypeIs(TYP_FLOAT))
                                {
                                    d1 = forceCastToFloat(d1); // Truncate precision.
                                }
                                goto CNS_DOUBLE; // Redundant cast.

                            default:
                                assert(!"Bad CastToType() in gtFoldExprConst() for a cast from double/float");
                                break;
                        }
                        return tree;

                    default:
                        return tree;
                }
                goto CNS_DOUBLE;

            default:
                // Not a foldable typ - e.g. RET const.
                return tree;
        }
    }

    // We have a binary operator.

    assert(tree->OperIsBinary());
    assert(op2 != nullptr);
    assert(op1->OperIsConst());
    assert(op2->OperIsConst());

    if (tree->OperIs(GT_COMMA))
    {
        return op2;
    }

    switchType = op1->GetType();

    // Normally we will just switch on op1 types, but for the case where
    // only op2 is a GC type and op1 is not a GC type, we use the op2 type.
    // This makes us handle this as a case of folding for GC type.
    if (varTypeIsGC(op2->gtType) && !varTypeIsGC(op1->gtType))
    {
        switchType = op2->TypeGet();
    }

    switch (switchType)
    {
        // Fold constant REF of BYREF binary operator.
        // These can only be comparisons or null pointers.

        case TYP_REF:

            // String nodes are an RVA at this point.
            if (op1->OperIs(GT_CNS_STR) || op2->OperIs(GT_CNS_STR))
            {
                // Fold "ldstr" ==/!= null.
                if (op2->IsIntegralConst(0))
                {
                    if (tree->OperIs(GT_EQ))
                    {
                        i1 = 0;
                        goto FOLD_COND;
                    }
                    if (tree->OperIs(GT_NE) || (tree->OperIs(GT_GT) && tree->IsUnsigned()))
                    {
                        i1 = 1;
                        goto FOLD_COND;
                    }
                }
                return tree;
            }

            FALLTHROUGH;

        case TYP_BYREF:

            i1 = op1->AsIntConCommon()->IconValue();
            i2 = op2->AsIntConCommon()->IconValue();

            switch (tree->OperGet())
            {
                case GT_EQ:
                    i1 = (i1 == i2);
                    goto FOLD_COND;

                case GT_NE:
                    i1 = (i1 != i2);
                    goto FOLD_COND;

                case GT_ADD:
                    noway_assert(!tree->TypeIs(TYP_REF));
                    // We only fold a GT_ADD that involves a null reference.
                    if ((op1->TypeIs(TYP_REF) && (i1 == 0)) || (op2->TypeIs(TYP_REF) && (i2 == 0)))
                    {
                        JITDUMP("\nFolding operator with constant nodes into a constant:\n");
                        DISPTREE(tree);

                        // Fold into GT_IND of null byref.
                        tree->ChangeOperConst(GT_CNS_INT);
                        tree->SetType(TYP_BYREF);
                        tree->AsIntCon()->SetValue(0);

                        if (vnStore != nullptr)
                        {
                            fgValueNumberTreeConst(tree);
                        }

                        JITDUMP("\nFolded to null byref:\n");
                        DISPTREE(tree);

                        goto DONE;
                    }
                    break;

                default:
                    break;
            }

            return tree;

        // Fold constant INT binary operator.

        case TYP_INT:

            if (tree->OperIsCompare() && tree->TypeIs(TYP_BYTE))
            {
                tree->ChangeType(TYP_INT);
            }

            assert(tree->TypeIs(TYP_INT) || varTypeIsGC(tree->TypeGet()) || tree->OperIs(GT_MKREFANY));

            // No GC pointer types should be folded here...
            assert(!varTypeIsGC(op1->TypeGet()) && !varTypeIsGC(op2->TypeGet()));

            if (!op1->AsIntConCommon()->ImmedValCanBeFolded(this, tree->OperGet()))
            {
                return tree;
            }

            if (!op2->AsIntConCommon()->ImmedValCanBeFolded(this, tree->OperGet()))
            {
                return tree;
            }

            i1 = op1->AsIntConCommon()->IconValue();
            i2 = op2->AsIntConCommon()->IconValue();

            switch (tree->OperGet())
            {
                case GT_EQ:
                    i1 = (INT32(i1) == INT32(i2));
                    break;
                case GT_NE:
                    i1 = (INT32(i1) != INT32(i2));
                    break;

                case GT_LT:
                    if (tree->IsUnsigned())
                    {
                        i1 = (UINT32(i1) < UINT32(i2));
                    }
                    else
                    {
                        i1 = (INT32(i1) < INT32(i2));
                    }
                    break;

                case GT_LE:
                    if (tree->IsUnsigned())
                    {
                        i1 = (UINT32(i1) <= UINT32(i2));
                    }
                    else
                    {
                        i1 = (INT32(i1) <= INT32(i2));
                    }
                    break;

                case GT_GE:
                    if (tree->IsUnsigned())
                    {
                        i1 = (UINT32(i1) >= UINT32(i2));
                    }
                    else
                    {
                        i1 = (INT32(i1) >= INT32(i2));
                    }
                    break;

                case GT_GT:
                    if (tree->IsUnsigned())
                    {
                        i1 = (UINT32(i1) > UINT32(i2));
                    }
                    else
                    {
                        i1 = (INT32(i1) > INT32(i2));
                    }
                    break;

                case GT_ADD:
#ifndef TARGET_64BIT
                    fieldSeq = GetFieldSeqStore()->FoldAdd(op1->AsIntCon(), op2->AsIntCon());
#endif

                    itemp = i1 + i2;
                    if (tree->gtOverflow() && CheckedOps::AddOverflows(INT32(i1), INT32(i2), tree->IsUnsigned()))
                    {
                        goto INTEGRAL_OVF;
                    }
                    i1 = itemp;
                    break;
                case GT_SUB:
                    itemp = i1 - i2;
                    if (tree->gtOverflow() && CheckedOps::SubOverflows(INT32(i1), INT32(i2), tree->IsUnsigned()))
                    {
                        goto INTEGRAL_OVF;
                    }
                    i1 = itemp;
                    break;
                case GT_MUL:
                    itemp = i1 * i2;
                    if (tree->gtOverflow() && CheckedOps::MulOverflows(INT32(i1), INT32(i2), tree->IsUnsigned()))
                    {
                        goto INTEGRAL_OVF;
                    }

                    i1 = itemp;
                    break;

                case GT_OR:
                    i1 |= i2;
                    break;
                case GT_XOR:
                    i1 ^= i2;
                    break;
                case GT_AND:
                    i1 &= i2;
                    break;

                case GT_LSH:
                    i1 <<= (i2 & 0x1f);
                    break;
                case GT_RSH:
                    i1 >>= (i2 & 0x1f);
                    break;
                case GT_RSZ:
                    // logical shift -> make it unsigned to not propagate the sign bit.
                    i1 = UINT32(i1) >> (i2 & 0x1f);
                    break;
                case GT_ROL:
                    i1 = (i1 << (i2 & 0x1f)) | (UINT32(i1) >> ((32 - i2) & 0x1f));
                    break;
                case GT_ROR:
                    i1 = (i1 << ((32 - i2) & 0x1f)) | (UINT32(i1) >> (i2 & 0x1f));
                    break;

                // DIV and MOD can throw an exception - if the division is by 0
                // or there is overflow - when dividing MIN by -1.

                case GT_DIV:
                case GT_MOD:
                case GT_UDIV:
                case GT_UMOD:
                    if (INT32(i2) == 0)
                    {
                        // Division by zero.
                        // We have to evaluate this expression and throw an exception.
                        return tree;
                    }
                    else if ((INT32(i2) == -1) && (UINT32(i1) == 0x80000000))
                    {
                        // Overflow Division.
                        // We have to evaluate this expression and throw an exception.
                        return tree;
                    }

                    if (tree->OperIs(GT_DIV))
                    {
                        i1 = INT32(i1) / INT32(i2);
                    }
                    else if (tree->OperIs(GT_MOD))
                    {
                        i1 = INT32(i1) % INT32(i2);
                    }
                    else if (tree->OperIs(GT_UDIV))
                    {
                        i1 = UINT32(i1) / UINT32(i2);
                    }
                    else
                    {
                        assert(tree->OperIs(GT_UMOD));
                        i1 = UINT32(i1) % UINT32(i2);
                    }
                    break;

                default:
                    return tree;
            }

        // We get here after folding to a GT_CNS_INT type.
        // change the node to the new type / value and make sure the node sizes are OK.
        CNS_INT:
        FOLD_COND:

            JITDUMP("\nFolding operator with constant nodes into a constant:\n");
            DISPTREE(tree);

            // Also all conditional folding jumps here since the node hanging from
            // GT_JTRUE has to be a GT_CNS_INT - value 0 or 1.

            tree->ChangeOperConst(GT_CNS_INT);
            tree->SetType(TYP_INT);
            // Some operations are performed as 64 bit instead of 32 bit so the upper 32 bits
            // need to be discarded. Since constant values are stored as ssize_t and the node
            // has TYP_INT the result needs to be sign extended rather than zero extended.
            tree->AsIntCon()->SetValue(static_cast<int>(i1));
            tree->AsIntCon()->SetFieldSeq(fieldSeq);
            if (vnStore != nullptr)
            {
                fgValueNumberTreeConst(tree);
            }

            JITDUMP("Bashed to int constant:\n");
            DISPTREE(tree);

            goto DONE;

        // Fold constant LONG binary operator.

        case TYP_LONG:

            // No GC pointer types should be folded here...
            assert(!varTypeIsGC(op1->TypeGet()) && !varTypeIsGC(op2->TypeGet()));

            // op1 is known to be a TYP_LONG, op2 is normally a TYP_LONG, unless we have a shift operator in which case
            // it is a TYP_INT.
            assert(op2->TypeIs(TYP_LONG, TYP_INT));

            if (!op1->AsIntConCommon()->ImmedValCanBeFolded(this, tree->OperGet()))
            {
                return tree;
            }

            if (!op2->AsIntConCommon()->ImmedValCanBeFolded(this, tree->OperGet()))
            {
                return tree;
            }

            lval1 = op1->AsIntConCommon()->LngValue();

            // For the shift operators we can have a op2 that is a TYP_INT.
            // Thus we cannot just use LngValue(), as it will assert on 32 bit if op2 is not GT_CNS_LNG.
            lval2 = op2->AsIntConCommon()->IntegralValue();

            switch (tree->OperGet())
            {
                case GT_EQ:
                    i1 = (lval1 == lval2);
                    goto FOLD_COND;
                case GT_NE:
                    i1 = (lval1 != lval2);
                    goto FOLD_COND;

                case GT_LT:
                    if (tree->IsUnsigned())
                    {
                        i1 = (UINT64(lval1) < UINT64(lval2));
                    }
                    else
                    {
                        i1 = (lval1 < lval2);
                    }
                    goto FOLD_COND;

                case GT_LE:
                    if (tree->IsUnsigned())
                    {
                        i1 = (UINT64(lval1) <= UINT64(lval2));
                    }
                    else
                    {
                        i1 = (lval1 <= lval2);
                    }
                    goto FOLD_COND;

                case GT_GE:
                    if (tree->IsUnsigned())
                    {
                        i1 = (UINT64(lval1) >= UINT64(lval2));
                    }
                    else
                    {
                        i1 = (lval1 >= lval2);
                    }
                    goto FOLD_COND;

                case GT_GT:
                    if (tree->IsUnsigned())
                    {
                        i1 = (UINT64(lval1) > UINT64(lval2));
                    }
                    else
                    {
                        i1 = (lval1 > lval2);
                    }
                    goto FOLD_COND;

                case GT_ADD:
#ifdef TARGET_64BIT
                    fieldSeq = GetFieldSeqStore()->FoldAdd(op1->AsIntCon(), op2->AsIntCon());
#endif

                    ltemp = lval1 + lval2;
                    if (tree->gtOverflow() && CheckedOps::AddOverflows(lval1, lval2, tree->IsUnsigned()))
                    {
                        goto INTEGRAL_OVF;
                    }
                    lval1 = ltemp;
                    break;

                case GT_SUB:
                    ltemp = lval1 - lval2;
                    if (tree->gtOverflow() && CheckedOps::SubOverflows(lval1, lval2, tree->IsUnsigned()))
                    {
                        goto INTEGRAL_OVF;
                    }
                    lval1 = ltemp;
                    break;

                case GT_MUL:
                    ltemp = lval1 * lval2;
                    if (tree->gtOverflow() && CheckedOps::MulOverflows(lval1, lval2, tree->IsUnsigned()))
                    {
                        goto INTEGRAL_OVF;
                    }
                    lval1 = ltemp;
                    break;

                case GT_OR:
                    lval1 |= lval2;
                    break;
                case GT_XOR:
                    lval1 ^= lval2;
                    break;
                case GT_AND:
                    lval1 &= lval2;
                    break;

                case GT_LSH:
                    lval1 <<= (lval2 & 0x3f);
                    break;
                case GT_RSH:
                    lval1 >>= (lval2 & 0x3f);
                    break;
                case GT_RSZ:
                    // logical shift -> make it unsigned to not propagate the sign bit.
                    lval1 = UINT64(lval1) >> (lval2 & 0x3f);
                    break;
                case GT_ROL:
                    lval1 = (lval1 << (lval2 & 0x3f)) | (UINT64(lval1) >> ((64 - lval2) & 0x3f));
                    break;
                case GT_ROR:
                    lval1 = (lval1 << ((64 - lval2) & 0x3f)) | (UINT64(lval1) >> (lval2 & 0x3f));
                    break;

                // Both DIV and IDIV on x86 raise an exception for min_int (and min_long) / -1.  So we preserve
                // that behavior here.
                case GT_DIV:
                    if (lval2 == 0)
                    {
                        return tree;
                    }
                    if ((UINT64(lval1) == UINT64(0x8000000000000000)) && (lval2 == INT64(-1)))
                    {
                        return tree;
                    }

                    lval1 /= lval2;
                    break;

                case GT_MOD:
                    if (lval2 == 0)
                    {
                        return tree;
                    }
                    if ((UINT64(lval1) == UINT64(0x8000000000000000)) && (lval2 == INT64(-1)))
                    {
                        return tree;
                    }

                    lval1 %= lval2;
                    break;

                case GT_UDIV:
                    if (lval2 == 0)
                    {
                        return tree;
                    }
                    if ((UINT64(lval1) == UINT64(0x8000000000000000)) && (lval2 == INT64(-1)))
                    {
                        return tree;
                    }

                    lval1 = UINT64(lval1) / UINT64(lval2);
                    break;

                case GT_UMOD:
                    if (lval2 == 0)
                    {
                        return tree;
                    }
                    if ((UINT64(lval1) == UINT64(0x8000000000000000)) && (lval2 == INT64(-1)))
                    {
                        return tree;
                    }

                    lval1 = UINT64(lval1) % UINT64(lval2);
                    break;
                default:
                    return tree;
            }

        CNS_LONG:

#ifdef DEBUG
            if (verbose)
            {
                printf("\nFolding long operator with constant nodes into a constant:\n");
                gtDispTree(tree);
            }
#endif
            assert((GenTree::s_gtNodeSizes[GT_CNS_NATIVELONG] == TREE_NODE_SZ_SMALL) ||
                   (tree->gtDebugFlags & GTF_DEBUG_NODE_LARGE));

            tree->ChangeOperConst(GT_CNS_NATIVELONG);
            tree->AsIntConCommon()->SetLngValue(lval1);
#ifdef TARGET_64BIT
            tree->AsIntCon()->SetFieldSeq(fieldSeq);
#endif
            if (vnStore != nullptr)
            {
                fgValueNumberTreeConst(tree);
            }

            JITDUMP("Bashed to long constant:\n");
            DISPTREE(tree);

            goto DONE;

        // Fold constant FLOAT or DOUBLE binary operator

        case TYP_FLOAT:
        case TYP_DOUBLE:
            assert(!tree->gtOverflowEx());

            d1 = op1->AsDblCon()->GetValue();
            d2 = op2->AsDblCon()->GetValue();

            // Special case - check if we have NaN operands.
            // For comparisons if not an unordered operation always return 0.
            // For unordered operations (i.e. the GTF_RELOP_NAN_UN flag is set)
            // the result is always true - return 1.

            if (_isnan(d1) || _isnan(d2))
            {
                JITDUMP("Double operator(s) is NaN\n");

                if (tree->OperIsCompare())
                {
                    if (tree->gtFlags & GTF_RELOP_NAN_UN)
                    {
                        // Unordered comparison with NaN always succeeds.
                        i1 = 1;
                        goto FOLD_COND;
                    }
                    else
                    {
                        // Normal comparison with NaN always fails.
                        i1 = 0;
                        goto FOLD_COND;
                    }
                }
            }

            switch (tree->OperGet())
            {
                case GT_EQ:
                    i1 = (d1 == d2);
                    goto FOLD_COND;
                case GT_NE:
                    i1 = (d1 != d2);
                    goto FOLD_COND;

                case GT_LT:
                    i1 = (d1 < d2);
                    goto FOLD_COND;
                case GT_LE:
                    i1 = (d1 <= d2);
                    goto FOLD_COND;
                case GT_GE:
                    i1 = (d1 >= d2);
                    goto FOLD_COND;
                case GT_GT:
                    i1 = (d1 > d2);
                    goto FOLD_COND;

                // Floating point arithmetic should be done in declared
                // precision while doing constant folding. For this reason though TYP_FLOAT
                // constants are stored as double constants, while performing float arithmetic,
                // double constants should be converted to float.  Here is an example case
                // where performing arithmetic in double precision would lead to incorrect
                // results.
                //
                // Example:
                // float a = float.MaxValue;
                // float b = a*a;   This will produce +inf in single precision and 1.1579207543382391e+077 in double
                //                  precision.
                // flaot c = b/b;   This will produce NaN in single precision and 1 in double precision.
                case GT_FADD:
                    if (op1->TypeIs(TYP_FLOAT))
                    {
                        f1 = forceCastToFloat(d1);
                        f2 = forceCastToFloat(d2);
                        d1 = forceCastToFloat(f1 + f2);
                    }
                    else
                    {
                        d1 += d2;
                    }
                    break;

                case GT_FSUB:
                    if (op1->TypeIs(TYP_FLOAT))
                    {
                        f1 = forceCastToFloat(d1);
                        f2 = forceCastToFloat(d2);
                        d1 = forceCastToFloat(f1 - f2);
                    }
                    else
                    {
                        d1 -= d2;
                    }
                    break;

                case GT_FMUL:
                    if (op1->TypeIs(TYP_FLOAT))
                    {
                        f1 = forceCastToFloat(d1);
                        f2 = forceCastToFloat(d2);
                        d1 = forceCastToFloat(f1 * f2);
                    }
                    else
                    {
                        d1 *= d2;
                    }
                    break;

                case GT_FDIV:
                    // We do not fold division by zero, even for floating point.
                    // This is because the result will be platform-dependent for an expression like 0d / 0d.
                    if (d2 == 0)
                    {
                        return tree;
                    }
                    if (op1->TypeIs(TYP_FLOAT))
                    {
                        f1 = forceCastToFloat(d1);
                        f2 = forceCastToFloat(d2);
                        d1 = forceCastToFloat(f1 / f2);
                    }
                    else
                    {
                        d1 /= d2;
                    }
                    break;

                default:
                    return tree;
            }

        CNS_DOUBLE:

            JITDUMP("\nFolding fp operator with constant nodes into a fp constant:\n");
            DISPTREE(tree);

            assert((GenTree::s_gtNodeSizes[GT_CNS_DBL] == TREE_NODE_SZ_SMALL) ||
                   (tree->gtDebugFlags & GTF_DEBUG_NODE_LARGE));

            tree->ChangeOperConst(GT_CNS_DBL);
            tree->AsDblCon()->gtDconVal = d1;
            if (vnStore != nullptr)
            {
                fgValueNumberTreeConst(tree);
            }

            JITDUMP("Bashed to fp constant:\n");
            DISPTREE(tree);

            goto DONE;

        default:
            // Not a foldable type.
            return tree;
    }

DONE:

    // Make sure no side effect flags are set on this constant node.

    tree->gtFlags &= ~GTF_ALL_EFFECT;

    return tree;

INTEGRAL_OVF:

    // This operation is going to cause an overflow exception. Morph into
    // an overflow helper. Put a dummy constant value for code generation.
    //
    // We could remove all subsequent trees in the current basic block,
    // unless this node is a child of GT_COLON
    //
    // NOTE: Since the folded value is not constant we should not change the
    //       "tree" node - otherwise we confuse the logic that checks if the folding
    //       was successful - instead use one of the operands, e.g. op1.

    // Don't fold overflow operations if not global morph phase.
    // The reason for this is that this optimization is replacing a gentree node
    // with another new gentree node. Say a GT_CALL(arglist) has one 'arg'
    // involving overflow arithmetic.  During assertion prop, it is possible
    // that the 'arg' could be constant folded and the result could lead to an
    // overflow.  In such a case 'arg' will get replaced with GT_COMMA node
    // but fgMorphArgs() - see the logic around "if(lateArgsComputed)" - doesn't
    // update args table. For this reason this optimization is enabled only
    // for global morphing phase.
    //
    // TODO-CQ: Once fgMorphArgs() is fixed this restriction could be removed.

    if (!fgGlobalMorph)
    {
        assert(tree->gtOverflow());
        return tree;
    }

    var_types type = genActualType(tree->TypeGet());
    op1            = type == TYP_LONG ? gtNewLconNode(0) : gtNewIconNode(0);
    if (vnStore != nullptr)
    {
        op1->gtVNPair.SetBoth(vnStore->VNZeroForType(type));
    }

    JITDUMP("\nFolding binary operator with constant nodes into a comma throw:\n");
    DISPTREE(tree);

    // We will change the cast to a GT_COMMA and attach the exception helper as AsOp()->gtOp1.
    // The constant expression zero becomes op2.

    assert(tree->gtOverflow());
    assert(tree->OperIs(GT_ADD, GT_SUB, GT_CAST, GT_MUL));
    assert(op1 != nullptr);

    op2 = op1;
    op1 = gtNewHelperCallNode(CORINFO_HELP_OVERFLOW, TYP_VOID, gtNewCallArgs(gtNewIconNode(compCurBB->bbTryIndex)));

    // op1 is a call to the JIT helper that throws an Overflow exception.
    // Attach the ExcSet for VNF_OverflowExc(Void) to this call.

    if (vnStore != nullptr)
    {
        op1->gtVNPair = vnStore->VNPWithExc(ValueNumPair(ValueNumStore::VNForVoid(), ValueNumStore::VNForVoid()),
                                            vnStore->VNPExcSetSingleton(vnStore->VNPairForFunc(TYP_REF, VNF_OverflowExc,
                                                                                               vnStore->VNPForVoid())));
    }

    tree = gtNewCommaNode(op1, op2);

    return tree;
}
#ifdef _PREFAST_
#pragma warning(pop)
#endif

/*****************************************************************************
 *
 *  Return true if the given node (excluding children trees) contains side effects.
 *  Note that it does not recurse, and children need to be handled separately.
 *  It may return false even if the node has GTF_SIDE_EFFECT (because of its children).
 *
 *  Similar to OperMayThrow() (but handles GT_CALLs specially), but considers
 *  assignments too.
 */

bool Compiler::gtNodeHasSideEffects(GenTree* tree, unsigned flags)
{
    if (flags & GTF_ASG)
    {
        // TODO-Cleanup: This only checks for GT_ASG but according to OperRequiresAsgFlag there
        // are many more opers that are considered to have an assignment side effect: atomic ops
        // (GT_CMPXCHG & co.), GT_MEMORYBARRIER (not classified as an atomic op) and HW intrinsic
        // memory stores. Atomic ops have special handling in gtExtractSideEffList but the others
        // will simply be dropped is they are ever subject to an "extract side effects" operation.
        // It is possible that the reason no bugs have yet been observed in this area is that the
        // other nodes are likely to always be tree roots.
        if (tree->OperIs(GT_ASG))
        {
            return true;
        }
    }

    // Are there only GTF_CALL side effects remaining? (and no other side effect kinds)
    if (flags & GTF_CALL)
    {
        if (tree->OperGet() == GT_CALL)
        {
            GenTreeCall* const call             = tree->AsCall();
            const bool         ignoreExceptions = (flags & GTF_EXCEPT) == 0;
            const bool         ignoreCctors     = (flags & GTF_IS_IN_CSE) != 0; // We can CSE helpers that run cctors.
            if (!call->HasSideEffects(this, ignoreExceptions, ignoreCctors))
            {
                // If this call is otherwise side effect free, check its arguments.
                for (GenTreeCall::Use& use : call->Args())
                {
                    if (gtTreeHasSideEffects(use.GetNode(), flags))
                    {
                        return true;
                    }
                }
                // I'm a little worried that args that assign to temps that are late args will look like
                // side effects...but better to be conservative for now.
                for (GenTreeCall::Use& use : call->LateArgs())
                {
                    if (gtTreeHasSideEffects(use.GetNode(), flags))
                    {
                        return true;
                    }
                }

                // Otherwise:
                return false;
            }

            // Otherwise the GT_CALL is considered to have side-effects.
            return true;
        }
    }

    if (flags & GTF_EXCEPT)
    {
        if (tree->OperMayThrow(this))
        {
            return true;
        }
    }

    // Expressions declared as CSE by (e.g.) hoisting code are considered to have relevant side
    // effects (if we care about GTF_MAKE_CSE).
    if ((flags & GTF_MAKE_CSE) && (tree->gtFlags & GTF_MAKE_CSE))
    {
        return true;
    }

    return false;
}

/*****************************************************************************
 * Returns true if the expr tree has any side effects.
 */

bool Compiler::gtTreeHasSideEffects(GenTree* tree, unsigned flags /* = GTF_SIDE_EFFECT*/)
{
    // These are the side effect flags that we care about for this tree
    unsigned sideEffectFlags = tree->gtFlags & flags;

    // Does this tree have any Side-effect flags set that we care about?
    if (sideEffectFlags == 0)
    {
        // no it doesn't..
        return false;
    }

    if (sideEffectFlags == GTF_CALL)
    {
        if (tree->OperGet() == GT_CALL)
        {
            // Generally all trees that contain GT_CALL nodes are considered to have side-effects.
            //
            if (tree->AsCall()->gtCallType == CT_HELPER)
            {
                // If this node is a helper call we may not care about the side-effects.
                // Note that gtNodeHasSideEffects checks the side effects of the helper itself
                // as well as the side effects of its arguments.
                return gtNodeHasSideEffects(tree, flags);
            }
        }
        else if (tree->OperGet() == GT_INTRINSIC)
        {
            if (gtNodeHasSideEffects(tree, flags))
            {
                return true;
            }

            if (gtNodeHasSideEffects(tree->AsOp()->gtOp1, flags))
            {
                return true;
            }

            if ((tree->AsOp()->gtOp2 != nullptr) && gtNodeHasSideEffects(tree->AsOp()->gtOp2, flags))
            {
                return true;
            }

            return false;
        }
    }

    return true;
}

GenTree* Compiler::gtBuildCommaList(GenTree* list, GenTree* expr)
{
    // 'list' starts off as null,
    //        and when it is null we haven't started the list yet.
    //
    if (list != nullptr)
    {
        // Create a GT_COMMA that appends 'expr' in front of the remaining set of expressions in (*list)
        GenTree* result = gtNewCommaNode(expr, list, TYP_VOID);

        // Set the flags in the comma node
        result->gtFlags |= (list->gtFlags & GTF_ALL_EFFECT);
        result->gtFlags |= (expr->gtFlags & GTF_ALL_EFFECT);

        // 'list' and 'expr' should have valuenumbers defined for both or for neither one (unless we are remorphing,
        // in which case a prior transform involving either node may have discarded or otherwise invalidated the value
        // numbers).
        assert((list->gtVNPair.BothDefined() == expr->gtVNPair.BothDefined()) || !fgGlobalMorph);

        // Set the ValueNumber 'gtVNPair' for the new GT_COMMA node
        //
        if (list->gtVNPair.BothDefined() && expr->gtVNPair.BothDefined())
        {
            // The result of a GT_COMMA node is op2, the normal value number is op2vnp
            // But we also need to include the union of side effects from op1 and op2.
            // we compute this value into exceptions_vnp.
            ValueNumPair op1vnp;
            ValueNumPair op1Xvnp = ValueNumStore::VNPForEmptyExcSet();
            ValueNumPair op2vnp;
            ValueNumPair op2Xvnp = ValueNumStore::VNPForEmptyExcSet();

            vnStore->VNPUnpackExc(expr->gtVNPair, &op1vnp, &op1Xvnp);
            vnStore->VNPUnpackExc(list->gtVNPair, &op2vnp, &op2Xvnp);

            ValueNumPair exceptions_vnp = ValueNumStore::VNPForEmptyExcSet();

            exceptions_vnp = vnStore->VNPExcSetUnion(exceptions_vnp, op1Xvnp);
            exceptions_vnp = vnStore->VNPExcSetUnion(exceptions_vnp, op2Xvnp);

            result->gtVNPair = vnStore->VNPWithExc(op2vnp, exceptions_vnp);
        }

        return result;
    }
    else
    {
        // The 'expr' will start the list of expressions
        return expr;
    }
}

//------------------------------------------------------------------------
// gtExtractSideEffList: Extracts side effects from the given expression.
//
// Arguments:
//    expr       - the expression tree to extract side effects from
//    pList      - pointer to a (possibly null) GT_COMMA list that
//                 will contain the extracted side effects
//    flags      - side effect flags to be considered
//    ignoreRoot - ignore side effects on the expression root node
//
// Notes:
//    Side effects are prepended to the GT_COMMA list such that op1 of
//    each comma node holds the side effect tree and op2 points to the
//    next comma node. The original side effect execution order is preserved.
//
void Compiler::gtExtractSideEffList(GenTree*  expr,
                                    GenTree** pList,
                                    unsigned  flags /* = GTF_SIDE_EFFECT*/,
                                    bool      ignoreRoot /* = false */)
{
    class SideEffectExtractor final : public GenTreeVisitor<SideEffectExtractor>
    {
    public:
        const unsigned       m_flags;
        ArrayStack<GenTree*> m_sideEffects;

        enum
        {
            DoPreOrder        = true,
            UseExecutionOrder = true
        };

        SideEffectExtractor(Compiler* compiler, unsigned flags)
            : GenTreeVisitor(compiler), m_flags(flags), m_sideEffects(compiler->getAllocator(CMK_SideEffects))
        {
        }

        fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
        {
            GenTree* node = *use;

            bool treeHasSideEffects = m_compiler->gtTreeHasSideEffects(node, m_flags);

            if (treeHasSideEffects)
            {
                if (m_compiler->gtNodeHasSideEffects(node, m_flags))
                {
                    m_sideEffects.Push(node);
                    if (node->OperIs(GT_OBJ, GT_BLK))
                    {
                        JITDUMP("Replace an unused OBJ/BLK node [%06d] with a NULLCHECK\n", dspTreeID(node));
                        m_compiler->gtChangeOperToNullCheck(node, m_compiler->compCurBB);
                    }
                    return Compiler::WALK_SKIP_SUBTREES;
                }

                // TODO-Cleanup: These have GTF_ASG set but for some reason gtNodeHasSideEffects ignores
                // them. See the related gtNodeHasSideEffects comment as well.
                // Also, these nodes must always be preserved, no matter what side effect flags are passed
                // in. But then it should never be the case that gtExtractSideEffList gets called without
                // specifying GTF_ASG so there doesn't seem to be any reason to be inconsistent with
                // gtNodeHasSideEffects and make this check unconditionally.
                if (node->OperIsAtomicOp())
                {
                    m_sideEffects.Push(node);
                    return Compiler::WALK_SKIP_SUBTREES;
                }

                // Generally all GT_CALL nodes are considered to have side-effects.
                // So if we get here it must be a helper call that we decided it does
                // not have side effects that we needed to keep.
                assert(!node->OperIs(GT_CALL) || (node->AsCall()->gtCallType == CT_HELPER));
            }

            if ((m_flags & GTF_IS_IN_CSE) != 0)
            {
                // If we're doing CSE then we also need to unmark CSE nodes. This will fail for CSE defs,
                // those need to be extracted as if they're side effects.
                if (!UnmarkCSE(node))
                {
                    m_sideEffects.Push(node);
                    return Compiler::WALK_SKIP_SUBTREES;
                }

                // The existence of CSE defs and uses is not propagated up the tree like side
                // effects are. We need to continue visiting the tree as if it has side effects.
                treeHasSideEffects = true;
            }

            return treeHasSideEffects ? Compiler::WALK_CONTINUE : Compiler::WALK_SKIP_SUBTREES;
        }

    private:
        bool UnmarkCSE(GenTree* node)
        {
            assert(m_compiler->optValnumCSE_phase);

            if (m_compiler->optUnmarkCSE(node))
            {
                // The call to optUnmarkCSE(node) should have cleared any CSE info.
                assert(!IS_CSE_INDEX(node->gtCSEnum));
                return true;
            }
            else
            {
                assert(IS_CSE_DEF(node->gtCSEnum));
#ifdef DEBUG
                if (m_compiler->verbose)
                {
                    printf("Preserving the CSE def #%02d at ", GET_CSE_INDEX(node->gtCSEnum));
                    m_compiler->printTreeID(node);
                }
#endif
                return false;
            }
        }
    };

    SideEffectExtractor extractor(this, flags);

    if (ignoreRoot)
    {
        for (GenTree* op : expr->Operands())
        {
            extractor.WalkTree(&op, nullptr);
        }
    }
    else
    {
        extractor.WalkTree(&expr, nullptr);
    }

    GenTree* list = *pList;

    // The extractor returns side effects in execution order but gtBuildCommaList prepends
    // to the comma-based side effect list so we have to build the list in reverse order.
    // This is also why the list cannot be built while traversing the tree.
    // The number of side effects is usually small (<= 4), less than the ArrayStack's
    // built-in size, so memory allocation is avoided.
    while (!extractor.m_sideEffects.Empty())
    {
        list = gtBuildCommaList(list, extractor.m_sideEffects.Pop());
    }

    *pList = list;
}

/*****************************************************************************
 *
 *  For debugging only - displays a tree node list and makes sure all the
 *  links are correctly set.
 */

#ifdef DEBUG

void dispNodeList(GenTree* list, bool verbose)
{
    GenTree* last = nullptr;
    GenTree* next;

    if (!list)
    {
        return;
    }

    for (;;)
    {
        next = list->gtNext;

        if (verbose)
        {
            printf("%08X -> %08X -> %08X\n", last, list, next);
        }

        assert(!last || last->gtNext == list);

        assert(next == nullptr || next->gtPrev == list);

        if (!next)
        {
            break;
        }

        last = list;
        list = next;
    }
    printf(""); // null string means flush
}

#endif // DEBUG

/*****************************************************************************
 *
 *  Callback used by the tree walker to implement fgFindLink()
 */
static Compiler::fgWalkResult gtFindLinkCB(GenTree** pTree, Compiler::fgWalkData* cbData)
{
    Compiler::FindLinkData* data = (Compiler::FindLinkData*)cbData->pCallbackData;
    if (*pTree == data->nodeToFind)
    {
        data->useEdge = pTree;
        data->user    = cbData->parent;
        return Compiler::WALK_ABORT;
    }

    return Compiler::WALK_CONTINUE;
}

Compiler::FindLinkData Compiler::gtFindLink(Statement* stmt, GenTree* node)
{
    FindLinkData data = {node, nullptr, nullptr};

    fgWalkResult result = fgWalkTreePre(stmt->GetRootNodePointer(), gtFindLinkCB, &data);

    if (result == WALK_ABORT)
    {
        assert(data.nodeToFind == *data.useEdge);
        return data;
    }
    else
    {
        return {node, nullptr, nullptr};
    }
}

//------------------------------------------------------------------------
// gtHasCallOnStack:
//
// Arguments:
//    parentStack: a context (stack of parent nodes)
//
// Return Value:
//     returns true if any of the parent nodes are a GT_CALL
//
// Assumptions:
//    We have a stack of parent nodes. This generally requires that
//    we are performing a recursive tree walk using struct fgWalkData
//
//------------------------------------------------------------------------
/* static */ bool Compiler::gtHasCallOnStack(GenTreeStack* parentStack)
{
    for (unsigned i = 0; i < parentStack->Size(); i++)
    {
        GenTree* node = parentStack->Top(i);
        if (node->OperGet() == GT_CALL)
        {
            return true;
        }
    }
    return false;
}

//------------------------------------------------------------------------
// gtGetTypeProducerKind: determine if a tree produces a runtime type, and
//    if so, how.
//
// Arguments:
//    tree - tree to examine
//
// Return Value:
//    TypeProducerKind for the tree.
//
// Notes:
//    Checks to see if this tree returns a RuntimeType value, and if so,
//    how that value is determined.
//
//    Currently handles these cases
//    1) The result of Object::GetType
//    2) The result of typeof(...)
//    3) A null reference
//    4) Tree is otherwise known to have type RuntimeType
//
//    The null reference case is surprisingly common because operator
//    overloading turns the otherwise innocuous
//
//        Type t = ....;
//        if (t == null)
//
//    into a method call.

Compiler::TypeProducerKind Compiler::gtGetTypeProducerKind(GenTree* tree)
{
    if (tree->gtOper == GT_CALL)
    {
        if (tree->AsCall()->gtCallType == CT_HELPER)
        {
            if (gtIsTypeHandleToRuntimeTypeHelper(tree->AsCall()))
            {
                return TPK_Handle;
            }
        }
        else if (tree->AsCall()->gtCallMoreFlags & GTF_CALL_M_SPECIAL_INTRINSIC)
        {
            if (info.compCompHnd->getIntrinsicID(tree->AsCall()->gtCallMethHnd) == CORINFO_INTRINSIC_Object_GetType)
            {
                return TPK_GetType;
            }
        }
    }
    else if ((tree->gtOper == GT_INTRINSIC) && (tree->AsIntrinsic()->gtIntrinsicId == CORINFO_INTRINSIC_Object_GetType))
    {
        return TPK_GetType;
    }
    else if ((tree->gtOper == GT_CNS_INT) && (tree->AsIntCon()->gtIconVal == 0))
    {
        return TPK_Null;
    }
    else
    {
        bool                 isExact   = false;
        bool                 isNonNull = false;
        CORINFO_CLASS_HANDLE clsHnd    = gtGetClassHandle(tree, &isExact, &isNonNull);

        if (clsHnd != NO_CLASS_HANDLE && clsHnd == info.compCompHnd->getBuiltinClass(CLASSID_RUNTIME_TYPE))
        {
            return TPK_Other;
        }
    }
    return TPK_Unknown;
}

//------------------------------------------------------------------------
// gtIsTypeHandleToRuntimeTypeHelperCall -- see if tree is constructing
//    a RuntimeType from a handle
//
// Arguments:
//    tree - tree to examine
//
// Return Value:
//    True if so

bool Compiler::gtIsTypeHandleToRuntimeTypeHelper(GenTreeCall* call)
{
    return call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE) ||
           call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE_MAYBENULL);
}

//------------------------------------------------------------------------
// gtIsTypeHandleToRuntimeTypeHandleHelperCall -- see if tree is constructing
//    a RuntimeTypeHandle from a handle
//
// Arguments:
//    tree - tree to examine
//    pHelper - optional pointer to a variable that receives the type of the helper
//
// Return Value:
//    True if so

bool Compiler::gtIsTypeHandleToRuntimeTypeHandleHelper(GenTreeCall* call, CorInfoHelpFunc* pHelper)
{
    CorInfoHelpFunc helper = CORINFO_HELP_UNDEF;

    if (call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE))
    {
        helper = CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE;
    }
    else if (call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE_MAYBENULL))
    {
        helper = CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE_MAYBENULL;
    }

    if (pHelper != nullptr)
    {
        *pHelper = helper;
    }

    return helper != CORINFO_HELP_UNDEF;
}

/*****************************************************************************/

struct ComplexityStruct
{
    unsigned m_numNodes;
    unsigned m_nodeLimit;
    ComplexityStruct(unsigned nodeLimit) : m_numNodes(0), m_nodeLimit(nodeLimit)
    {
    }
};

static Compiler::fgWalkResult ComplexityExceedsWalker(GenTree** pTree, Compiler::fgWalkData* data)
{
    ComplexityStruct* pComplexity = (ComplexityStruct*)data->pCallbackData;
    if (++pComplexity->m_numNodes > pComplexity->m_nodeLimit)
    {
        return Compiler::WALK_ABORT;
    }
    else
    {
        return Compiler::WALK_CONTINUE;
    }
}

bool Compiler::gtComplexityExceeds(GenTree* tree, unsigned limit)
{
    ComplexityStruct complexity(limit);
    return fgWalkTreePre(&tree, &ComplexityExceedsWalker, &complexity) == WALK_ABORT;
}

bool GenTree::IsPhiNode()
{
    return OperIs(GT_PHI_ARG, GT_PHI) || IsPhiDefn();
}

bool GenTree::IsPhiDefn()
{
    return (OperIs(GT_ASG) && AsOp()->GetOp(1)->OperIs(GT_PHI)) ||
           (OperIs(GT_STORE_LCL_VAR) && AsLclVar()->GetOp(0)->OperIs(GT_PHI));
}

bool GenTree::IsPartialLclFld(Compiler* comp)
{
    if (gtOper != GT_LCL_FLD)
    {
        return false;
    }

    if (AsLclFld()->GetLclOffs() != 0)
    {
        return true;
    }

    unsigned lclSize = comp->lvaGetDesc(AsLclFld())->GetSize();
    unsigned lclFldSize;

    if (gtType == TYP_STRUCT)
    {
        lclFldSize = AsLclFld()->GetLayout(comp)->GetSize();
    }
    else
    {
        lclFldSize = varTypeSize(gtType);
    }

    return lclFldSize < lclSize;
}

GenTreeLclVarCommon* GenTree::IsLocalAddrExpr()
{
    GenTree* node = this;

    while (node->OperIs(GT_ADD))
    {
        GenTree* op1 = node->AsOp()->GetOp(0);
        GenTree* op2 = node->AsOp()->GetOp(1);

        if (op1->OperIs(GT_CNS_INT))
        {
            std::swap(op1, op2);
        }

        if (!op2->OperIs(GT_CNS_INT))
        {
            return nullptr;
        }

        node = op1;
    }

    return node->OperIs(GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR) ? node->AsLclVarCommon() : nullptr;
}

GenTreeLclVar* GenTree::IsImplicitByrefIndir(Compiler* compiler)
{
#if defined(TARGET_AMD64) || defined(TARGET_ARM64)
    if (OperIs(GT_OBJ, GT_IND) && AsIndir()->GetAddr()->OperIs(GT_LCL_VAR))
    {
        // TODO-MIKE-CQ: This does not recognize access to fields of an
        // implicit byref param so abiMorphImplicitByRefStructArg will
        // make an unnecessary copy and fgCallHasMustCopyByrefParameter
        // will block fast tail calls for calls like "CALL(param.b)" when
        // "b" is also a struct passed by implicit reference.

        GenTreeLclVar* lclVar = AsIndir()->GetAddr()->AsLclVar();

        if (compiler->lvaGetDesc(lclVar)->IsImplicitByRefParam())
        {
            return lclVar;
        }
    }
#endif

    return nullptr;
}

//------------------------------------------------------------------------
// IsLclVarUpdateTree: Determine whether this is an assignment tree of the
//                     form Vn = Vn 'oper' 'otherTree' where Vn is a lclVar
//
// Arguments:
//    pOtherTree - An "out" argument in which 'otherTree' will be returned.
//    pOper      - An "out" argument in which 'oper' will be returned.
//
// Return Value:
//    If the tree is of the above form, the lclNum of the variable being
//    updated is returned, and 'pOtherTree' and 'pOper' are set.
//    Otherwise, returns BAD_VAR_NUM.
//
// Notes:
//    'otherTree' can have any shape.
//     We avoid worrying about whether the op is commutative by only considering the
//     first operand of the rhs. It is expected that most trees of this form will
//     already have the lclVar on the lhs.
//     TODO-CQ: Evaluate whether there are missed opportunities due to this, or
//     whether gtSetEvalOrder will already have put the lclVar on the lhs in
//     the cases of interest.

unsigned GenTree::IsLclVarUpdateTree(GenTree** pOtherTree, genTreeOps* pOper)
{
    unsigned lclNum = BAD_VAR_NUM;
    if (OperIs(GT_ASG))
    {
        GenTree* lhs = AsOp()->gtOp1;
        GenTree* rhs = AsOp()->gtOp2;
        if ((lhs->OperGet() == GT_LCL_VAR) && rhs->OperIsBinary())
        {
            unsigned lhsLclNum = lhs->AsLclVarCommon()->GetLclNum();
            GenTree* rhsOp1    = rhs->AsOp()->gtOp1;
            GenTree* rhsOp2    = rhs->AsOp()->gtOp2;

            // Some operators, such as HWINTRINSIC, are currently declared as binary but
            // may not have two operands. We must check that both operands actually exist.
            if ((rhsOp1 != nullptr) && (rhsOp2 != nullptr) && (rhsOp1->OperGet() == GT_LCL_VAR) &&
                (rhsOp1->AsLclVarCommon()->GetLclNum() == lhsLclNum))
            {
                lclNum      = lhsLclNum;
                *pOtherTree = rhsOp2;
                *pOper      = rhs->OperGet();
            }
        }
    }
    return lclNum;
}

#ifdef DEBUG
//------------------------------------------------------------------------
// canBeContained: check whether this tree node may be a subcomponent of its parent for purposes
//                 of code generation.
//
// Return value: returns true if it is possible to contain this node and false otherwise.
bool GenTree::canBeContained() const
{
    assert(IsLIR());

    if (gtHasReg())
    {
        return false;
    }

    // It is not possible for nodes that do not produce values or that are not containable values
    // to be contained.
    if (((OperKind() & (GTK_NOVALUE | GTK_NOCONTAIN)) != 0) || (OperIsHWIntrinsic() && !isContainableHWIntrinsic()))
    {
        return false;
    }

    return true;
}
#endif // DEBUG

//------------------------------------------------------------------------
// isContained: check whether this tree node is a subcomponent of its parent for codegen purposes
//
// Return Value:
//    Returns true if there is no code generated explicitly for this node.
//    Essentially, it will be rolled into the code generation for the parent.
//
// Assumptions:
//    This method relies upon the value of the GTF_CONTAINED flag.
//    Therefore this method is only valid after Lowering.
//    Also note that register allocation or other subsequent phases may cause
//    nodes to become contained (or not) and therefore this property may change.
//
bool GenTree::isContained() const
{
    assert(IsLIR());

    if ((gtFlags & GTF_CONTAINED) == 0)
    {
        return false;
    }

    assert(canBeContained());
    assert(!IsUnusedValue());

    return true;
}

//------------------------------------------------------------------------
// GenTreeIntCon::ImmedValNeedsReloc: does this immediate value needs recording a relocation with the VM?
//
// Arguments:
//    comp - Compiler instance
//
// Return Value:
//    True if this immediate value requires us to record a relocation for it; false otherwise.

bool GenTreeIntCon::ImmedValNeedsReloc(Compiler* comp)
{
    return comp->opts.compReloc && IsIconHandle();
}

//------------------------------------------------------------------------
// ImmedValCanBeFolded: can this immediate value be folded for op?
//
// Arguments:
//    comp - Compiler instance
//    op - Tree operator
//
// Return Value:
//    True if this immediate value can be folded for op; false otherwise.

bool GenTreeIntConCommon::ImmedValCanBeFolded(Compiler* comp, genTreeOps op)
{
    // In general, immediate values that need relocations can't be folded.
    // There are cases where we do want to allow folding of handle comparisons
    // (e.g., typeof(T) == typeof(int)).
    return IsLngCon() || !IsIntCon()->ImmedValNeedsReloc(comp) || (op == GT_EQ) || (op == GT_NE);
}

#ifdef TARGET_AMD64
// Returns true if this absolute address fits within the base of an addr mode.
// On Amd64 this effectively means, whether an absolute indirect address can
// be encoded as 32-bit offset relative to IP or zero.
bool GenTreeIntCon::FitsInAddrBase(Compiler* comp)
{
#ifdef DEBUG
    // Early out if PC-rel encoding of absolute addr is disabled.
    if (!comp->opts.compEnablePCRelAddr)
    {
        return false;
    }
#endif

    if (comp->opts.compReloc)
    {
        // During Ngen JIT is always asked to generate relocatable code.
        // Hence JIT will try to encode only icon handles as pc-relative offsets.
        return IsIconHandle() && (IMAGE_REL_BASED_REL32 == comp->eeGetRelocTypeHint((void*)IconValue()));
    }
    else
    {
        // During Jitting, we are allowed to generate non-relocatable code.
        // On Amd64 we can encode an absolute indirect addr as an offset relative to zero or RIP.
        // An absolute indir addr that can fit within 32-bits can ben encoded as an offset relative
        // to zero. All other absolute indir addr could be attempted to be encoded as RIP relative
        // based on reloc hint provided by VM.  RIP relative encoding is preferred over relative
        // to zero, because the former is one byte smaller than the latter.  For this reason
        // we check for reloc hint first and then whether addr fits in 32-bits next.
        //
        // VM starts off with an initial state to allow both data and code address to be encoded as
        // pc-relative offsets.  Hence JIT will attempt to encode all absolute addresses as pc-relative
        // offsets.  It is possible while jitting a method, an address could not be encoded as a
        // pc-relative offset.  In that case VM will note the overflow and will trigger re-jitting
        // of the method with reloc hints turned off for all future methods. Second time around
        // jitting will succeed since JIT will not attempt to encode data addresses as pc-relative
        // offsets.  Note that JIT will always attempt to relocate code addresses (.e.g call addr).
        // After an overflow, VM will assume any relocation recorded is for a code address and will
        // emit jump thunk if it cannot be encoded as pc-relative offset.
        return (IMAGE_REL_BASED_REL32 == comp->eeGetRelocTypeHint((void*)IconValue())) || FitsInI32();
    }
}

// Returns true if this icon value is encoded as addr needs recording a relocation with VM
bool GenTreeIntCon::AddrNeedsReloc(Compiler* comp)
{
    if (comp->opts.compReloc)
    {
        // During Ngen JIT is always asked to generate relocatable code.
        // Hence JIT will try to encode only icon handles as pc-relative offsets.
        return IsIconHandle() && (IMAGE_REL_BASED_REL32 == comp->eeGetRelocTypeHint((void*)IconValue()));
    }
    else
    {
        return IMAGE_REL_BASED_REL32 == comp->eeGetRelocTypeHint((void*)IconValue());
    }
}

#elif defined(TARGET_X86)
// Returns true if this absolute address fits within the base of an addr mode.
// On x86 all addresses are 4-bytes and can be directly encoded in an addr mode.
bool GenTreeIntCon::FitsInAddrBase(Compiler* comp)
{
#ifdef DEBUG
    // Early out if PC-rel encoding of absolute addr is disabled.
    if (!comp->opts.compEnablePCRelAddr)
    {
        return false;
    }
#endif

    return true;
}

// Returns true if this icon value is encoded as addr needs recording a relocation with VM
bool GenTreeIntCon::AddrNeedsReloc(Compiler* comp)
{
    // If generating relocatable code, icons should be reported for recording relocatons.
    return comp->opts.compReloc && IsIconHandle();
}
#endif // TARGET_X86

ClassLayout* GenTreeLclFld::GetLayout(Compiler* compiler) const
{
    return (m_layoutNum == 0) ? nullptr : compiler->typGetLayoutByNum(m_layoutNum);
}

void GenTreeLclFld::SetLayout(ClassLayout* layout, Compiler* compiler)
{
    SetLayoutNum(layout == nullptr ? 0 : compiler->typGetLayoutNum(layout));
}

ClassLayout* GenTreeFieldAddr::GetLayout(Compiler* compiler) const
{
    return (m_layoutNum == 0) ? nullptr : compiler->typGetLayoutByNum(m_layoutNum);
}

void GenTreeFieldAddr::SetLayout(ClassLayout* layout, Compiler* compiler)
{
    SetLayoutNum(layout == nullptr ? 0 : compiler->typGetLayoutNum(layout));
}

ClassLayout* GenTreeIndexAddr::GetLayout(Compiler* compiler) const
{
    return !compiler->typIsLayoutNum(m_elemTypeNum) ? nullptr : compiler->typGetLayoutByNum(m_elemTypeNum);
}

//------------------------------------------------------------------------
// gtGetClassHandle: find class handle for a ref type
//
// Arguments:
//    tree -- tree to find handle for
//    pIsExact   [out] -- whether handle is exact type
//    pIsNonNull [out] -- whether tree value is known not to be null
//
// Return Value:
//    nullptr if class handle is unknown,
//        otherwise the class handle.
//    *pIsExact set true if tree type is known to be exactly the handle type,
//        otherwise actual type may be a subtype.
//    *pIsNonNull set true if tree value is known not to be null,
//        otherwise a null value is possible.

CORINFO_CLASS_HANDLE Compiler::gtGetClassHandle(GenTree* tree, bool* pIsExact, bool* pIsNonNull)
{
    *pIsNonNull = false;
    *pIsExact   = false;

    if (!tree->TypeIs(TYP_REF))
    {
        return NO_CLASS_HANDLE;
    }

    CORINFO_CLASS_HANDLE objClass = nullptr;
    GenTree*             obj      = tree->gtEffectiveVal();

    switch (obj->GetOper())
    {
        case GT_LCL_VAR:
        {
            LclVarDsc* lcl = lvaGetDesc(obj->AsLclVar());

            objClass  = lcl->lvClassHnd;
            *pIsExact = lcl->lvClassIsExact;
            break;
        }

        case GT_RET_EXPR:
            objClass = gtGetClassHandle(tree->AsRetExpr()->GetRetExpr(), pIsExact, pIsNonNull);
            break;

        case GT_CALL:
        {
            GenTreeCall* call = tree->AsCall();
            if (call->gtCallMoreFlags & GTF_CALL_M_SPECIAL_INTRINSIC)
            {
                NamedIntrinsic ni = lookupNamedIntrinsic(call->gtCallMethHnd);
                if ((ni == NI_System_Array_Clone) || (ni == NI_System_Object_MemberwiseClone))
                {
                    objClass = gtGetClassHandle(call->gtCallThisArg->GetNode(), pIsExact, pIsNonNull);
                    break;
                }

                CORINFO_CLASS_HANDLE specialObjClass = impGetSpecialIntrinsicExactReturnType(call->gtCallMethHnd);
                if (specialObjClass != nullptr)
                {
                    objClass    = specialObjClass;
                    *pIsExact   = true;
                    *pIsNonNull = true;
                    break;
                }
            }
            if (call->IsInlineCandidate())
            {
                // For inline candidates, we've already cached the return
                // type class handle in the inline info.
                InlineCandidateInfo* inlInfo = call->gtInlineCandidateInfo;
                assert(inlInfo != nullptr);

                // Grab it as our first cut at a return type.
                assert(inlInfo->methInfo.args.retType == CORINFO_TYPE_CLASS);
                objClass = inlInfo->methInfo.args.retTypeClass;

                // If the method is shared, the above may not capture
                // the most precise return type information (that is,
                // it may represent a shared return type and as such,
                // have instances of __Canon). See if we can use the
                // context to get at something more definite.
                //
                // For now, we do this here on demand rather than when
                // processing the call, but we could/should apply
                // similar sharpening to the argument and local types
                // of the inlinee.
                const unsigned retClassFlags = info.compCompHnd->getClassAttribs(objClass);
                if (retClassFlags & CORINFO_FLG_SHAREDINST)
                {
                    CORINFO_CONTEXT_HANDLE context = inlInfo->exactContextHnd;

                    if (context != nullptr)
                    {
                        CORINFO_CLASS_HANDLE exactClass = eeGetClassFromContext(context);

                        // Grab the signature in this context.
                        CORINFO_SIG_INFO sig;
                        eeGetMethodSig(call->gtCallMethHnd, &sig, exactClass);
                        assert(sig.retType == CORINFO_TYPE_CLASS);
                        objClass = sig.retTypeClass;
                    }
                }
            }
            else if (call->gtCallType == CT_USER_FUNC)
            {
                // For user calls, we can fetch the approximate return
                // type info from the method handle. Unfortunately
                // we've lost the exact context, so this is the best
                // we can do for now.
                CORINFO_METHOD_HANDLE method     = call->gtCallMethHnd;
                CORINFO_CLASS_HANDLE  exactClass = nullptr;
                CORINFO_SIG_INFO      sig;
                eeGetMethodSig(method, &sig, exactClass);
                if (sig.retType == CORINFO_TYPE_VOID)
                {
                    // This is a constructor call.
                    const unsigned methodFlags = info.compCompHnd->getMethodAttribs(method);
                    assert((methodFlags & CORINFO_FLG_CONSTRUCTOR) != 0);
                    objClass    = info.compCompHnd->getMethodClass(method);
                    *pIsExact   = true;
                    *pIsNonNull = true;
                }
                else
                {
                    assert(sig.retType == CORINFO_TYPE_CLASS);
                    objClass = sig.retTypeClass;
                }
            }
            else if (call->gtCallType == CT_HELPER)
            {
                objClass = gtGetHelperCallClassHandle(call, pIsExact, pIsNonNull);
            }

            break;
        }

        case GT_INTRINSIC:
        {
            GenTreeIntrinsic* intrinsic = obj->AsIntrinsic();

            if (intrinsic->gtIntrinsicId == CORINFO_INTRINSIC_Object_GetType)
            {
                CORINFO_CLASS_HANDLE runtimeType = info.compCompHnd->getBuiltinClass(CLASSID_RUNTIME_TYPE);
                assert(runtimeType != NO_CLASS_HANDLE);

                objClass    = runtimeType;
                *pIsExact   = false;
                *pIsNonNull = true;
            }

            break;
        }

        case GT_CNS_STR:
        {
            // For literal strings, we know the class and that the
            // value is not null.
            objClass    = impGetStringClass();
            *pIsExact   = true;
            *pIsNonNull = true;
            break;
        }

        case GT_IND:
        {
            GenTree*             addr = obj->AsIndir()->GetAddr();
            GenTreeLclVarCommon* lcl  = addr->IsLocalAddrExpr();

            if ((lcl != nullptr) && (addr->OperGet() != GT_ADD))
            {
                // indir(addr(lcl)) --> lcl
                //
                // This comes up during constrained callvirt on ref types.

                const unsigned objLcl = lcl->GetLclNum();
                objClass              = lvaTable[objLcl].lvClassHnd;
                *pIsExact             = lvaTable[objLcl].lvClassIsExact;
            }
            else if (addr->OperGet() == GT_ARR_ELEM)
            {
                // indir(arr_elem(...)) -> array element type

                GenTree* array = addr->AsArrElem()->gtArrObj;

                objClass    = gtGetArrayElementClassHandle(array);
                *pIsExact   = false;
                *pIsNonNull = false;
            }
            else if (addr->OperGet() == GT_ADD)
            {
                // This could be a static field access.
                //
                // See if op1 is a static field base helper call
                // and if so, op2 will have the field info.
                GenTree* op1 = addr->AsOp()->gtOp1;
                GenTree* op2 = addr->AsOp()->gtOp2;

                const bool op1IsStaticFieldBase = gtIsStaticGCBaseHelperCall(op1);

                if (op1IsStaticFieldBase && op2->IsIntCon())
                {
                    FieldSeqNode* fieldSeq = op2->AsIntCon()->GetFieldSeq();

                    if ((fieldSeq != nullptr) && !fieldSeq->IsArrayElement())
                    {
                        fieldSeq = fieldSeq->GetTail();

                        // No benefit to calling gtGetFieldClassHandle here, as
                        // the exact field being accessed can vary.
                        CORINFO_FIELD_HANDLE fieldHnd     = fieldSeq->GetFieldHandle();
                        CORINFO_CLASS_HANDLE fieldClass   = nullptr;
                        CorInfoType          fieldCorType = info.compCompHnd->getFieldType(fieldHnd, &fieldClass);

                        assert(fieldCorType == CORINFO_TYPE_CLASS);
                        objClass = fieldClass;
                    }
                }
            }
            else if (GenTreeIntCon* icon = addr->IsIntCon())
            {
                FieldSeqNode* fieldSeq = icon->GetFieldSeq();

                if ((fieldSeq != nullptr) && fieldSeq->IsField())
                {
                    CORINFO_FIELD_HANDLE fieldHandle = fieldSeq->GetFieldHandle();
                    assert(info.compCompHnd->isFieldStatic(fieldHandle));
                    objClass = gtGetFieldClassHandle(fieldHandle, pIsExact, pIsNonNull);
                }
            }
            else if (GenTreeClsVar* clsVarAddr = addr->IsClsVar())
            {
                CORINFO_FIELD_HANDLE fieldHandle = clsVarAddr->GetFieldHandle();
                assert(info.compCompHnd->isFieldStatic(fieldHandle));
                objClass = gtGetFieldClassHandle(fieldHandle, pIsExact, pIsNonNull);
            }
            else if (GenTreeFieldAddr* field = addr->IsFieldAddr())
            {
                FieldSeqNode* fieldSeq = field->GetFieldSeq()->GetTail();

                if (fieldSeq->IsField())
                {
                    objClass = gtGetFieldClassHandle(fieldSeq->GetFieldHandle(), pIsExact, pIsNonNull);
                }
            }
            else if (GenTreeIndexAddr* index = addr->IsIndexAddr())
            {
                objClass    = gtGetArrayElementClassHandle(index->GetArray());
                *pIsExact   = false;
                *pIsNonNull = false;
            }

            break;
        }

        case GT_BOX:
        {
            // Box should just wrap a local var reference which has
            // the type we're looking for. Also box only represents a
            // non-nullable value type so result cannot be null.
            LclVarDsc* boxTempLcl = lvaGetDesc(obj->AsBox()->GetOp(0)->AsLclVar());
            objClass              = boxTempLcl->lvClassHnd;
            *pIsExact             = boxTempLcl->lvClassIsExact;
            *pIsNonNull           = true;
            break;
        }

        default:
        {
            break;
        }
    }

    return objClass;
}

//------------------------------------------------------------------------
// gtGetHelperCallClassHandle: find class handle for return value of a
//   helper call
//
// Arguments:
//    call - helper call to examine
//    pIsExact - [OUT] true if type is known exactly
//    pIsNonNull - [OUT] true if return value is not null
//
// Return Value:
//    nullptr if helper call result is not a ref class, or the class handle
//    is unknown, otherwise the class handle.

CORINFO_CLASS_HANDLE Compiler::gtGetHelperCallClassHandle(GenTreeCall* call, bool* pIsExact, bool* pIsNonNull)
{
    assert(call->gtCallType == CT_HELPER);

    *pIsNonNull                    = false;
    *pIsExact                      = false;
    CORINFO_CLASS_HANDLE  objClass = nullptr;
    const CorInfoHelpFunc helper   = eeGetHelperNum(call->gtCallMethHnd);

    switch (helper)
    {
        case CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE:
        case CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE_MAYBENULL:
        {
            // Note for some runtimes these helpers return exact types.
            //
            // But in those cases the types are also sealed, so there's no
            // need to claim exactness here.
            const bool           helperResultNonNull = (helper == CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE);
            CORINFO_CLASS_HANDLE runtimeType         = info.compCompHnd->getBuiltinClass(CLASSID_RUNTIME_TYPE);

            assert(runtimeType != NO_CLASS_HANDLE);

            objClass    = runtimeType;
            *pIsNonNull = helperResultNonNull;
            break;
        }

        case CORINFO_HELP_CHKCASTCLASS:
        case CORINFO_HELP_CHKCASTANY:
        case CORINFO_HELP_CHKCASTARRAY:
        case CORINFO_HELP_CHKCASTINTERFACE:
        case CORINFO_HELP_CHKCASTCLASS_SPECIAL:
        case CORINFO_HELP_ISINSTANCEOFINTERFACE:
        case CORINFO_HELP_ISINSTANCEOFARRAY:
        case CORINFO_HELP_ISINSTANCEOFCLASS:
        case CORINFO_HELP_ISINSTANCEOFANY:
        {
            // Fetch the class handle from the helper call arglist
            GenTreeCall::Use*    args    = call->gtCallArgs;
            GenTree*             typeArg = args->GetNode();
            CORINFO_CLASS_HANDLE castHnd = gtGetHelperArgClassHandle(typeArg);

            // We generally assume the type being cast to is the best type
            // for the result, unless it is an interface type.
            //
            // TODO-CQ: when we have default interface methods then
            // this might not be the best assumption. We could also
            // explore calling something like mergeClasses to identify
            // the more specific class. A similar issue arises when
            // typing the temp in impCastClassOrIsInstToTree, when we
            // expand the cast inline.
            if (castHnd != nullptr)
            {
                DWORD attrs = info.compCompHnd->getClassAttribs(castHnd);

                if ((attrs & CORINFO_FLG_INTERFACE) != 0)
                {
                    castHnd = nullptr;
                }
            }

            // If we don't have a good estimate for the type we can use the
            // type from the value being cast instead.
            if (castHnd == nullptr)
            {
                GenTree* valueArg = args->GetNext()->GetNode();
                castHnd           = gtGetClassHandle(valueArg, pIsExact, pIsNonNull);
            }

            // We don't know at jit time if the cast will succeed or fail, but if it
            // fails at runtime then an exception is thrown for cast helpers, or the
            // result is set null for instance helpers.
            //
            // So it safe to claim the result has the cast type.
            // Note we don't know for sure that it is exactly this type.
            if (castHnd != nullptr)
            {
                objClass = castHnd;
            }

            break;
        }

        case CORINFO_HELP_NEWARR_1_DIRECT:
        case CORINFO_HELP_NEWARR_1_OBJ:
        case CORINFO_HELP_NEWARR_1_VC:
        case CORINFO_HELP_NEWARR_1_ALIGN8:
        case CORINFO_HELP_READYTORUN_NEWARR_1:
        {
            CORINFO_CLASS_HANDLE arrayHnd = (CORINFO_CLASS_HANDLE)call->compileTimeHelperArgumentHandle;

            if (arrayHnd != NO_CLASS_HANDLE)
            {
                objClass    = arrayHnd;
                *pIsExact   = true;
                *pIsNonNull = true;
            }
            break;
        }

        default:
            break;
    }

    return objClass;
}

//------------------------------------------------------------------------
// gtGetArrayElementClassHandle: find class handle for elements of an array
// of ref types
//
// Arguments:
//    array -- array to find handle for
//
// Return Value:
//    nullptr if element class handle is unknown, otherwise the class handle.

CORINFO_CLASS_HANDLE Compiler::gtGetArrayElementClassHandle(GenTree* array)
{
    bool                 isArrayExact   = false;
    bool                 isArrayNonNull = false;
    CORINFO_CLASS_HANDLE arrayClassHnd  = gtGetClassHandle(array, &isArrayExact, &isArrayNonNull);

    if (arrayClassHnd != nullptr)
    {
        // We know the class of the reference
        DWORD attribs = info.compCompHnd->getClassAttribs(arrayClassHnd);

        if ((attribs & CORINFO_FLG_ARRAY) != 0)
        {
            // We know for sure it is an array
            CORINFO_CLASS_HANDLE elemClassHnd  = nullptr;
            CorInfoType          arrayElemType = info.compCompHnd->getChildType(arrayClassHnd, &elemClassHnd);

            if (arrayElemType == CORINFO_TYPE_CLASS)
            {
                // We know it is an array of ref types
                return elemClassHnd;
            }
        }
    }

    return nullptr;
}

//------------------------------------------------------------------------
// gtGetFieldClassHandle: find class handle for a field
//
// Arguments:
//    fieldHnd - field handle for field in question
//    pIsExact - [OUT] true if type is known exactly
//    pIsNonNull - [OUT] true if field value is not null
//
// Return Value:
//    nullptr if helper call result is not a ref class, or the class handle
//    is unknown, otherwise the class handle.
//
//    May examine runtime state of static field instances.

CORINFO_CLASS_HANDLE Compiler::gtGetFieldClassHandle(CORINFO_FIELD_HANDLE fieldHnd, bool* pIsExact, bool* pIsNonNull)
{
    CORINFO_CLASS_HANDLE fieldClass   = nullptr;
    CorInfoType          fieldCorType = info.compCompHnd->getFieldType(fieldHnd, &fieldClass);

    if (fieldCorType == CORINFO_TYPE_CLASS)
    {
        // Optionally, look at the actual type of the field's value
        bool queryForCurrentClass = true;
        INDEBUG(queryForCurrentClass = (JitConfig.JitQueryCurrentStaticFieldClass() > 0););

        if (queryForCurrentClass)
        {

#if DEBUG
            const char* fieldClassName = nullptr;
            const char* fieldName      = eeGetFieldName(fieldHnd, &fieldClassName);
            JITDUMP("Querying runtime about current class of field %s.%s (declared as %s)\n", fieldClassName, fieldName,
                    eeGetClassName(fieldClass));
#endif // DEBUG

            // Is this a fully initialized init-only static field?
            //
            // Note we're not asking for speculative results here, yet.
            CORINFO_CLASS_HANDLE currentClass = info.compCompHnd->getStaticFieldCurrentClass(fieldHnd);

            if (currentClass != NO_CLASS_HANDLE)
            {
                // Yes! We know the class exactly and can rely on this to always be true.
                fieldClass  = currentClass;
                *pIsExact   = true;
                *pIsNonNull = true;
                JITDUMP("Runtime reports field is init-only and initialized and has class %s\n",
                        eeGetClassName(fieldClass));
            }
            else
            {
                JITDUMP("Field's current class not available\n");
            }
        }
    }

    return fieldClass;
}

//------------------------------------------------------------------------
// gtIsGCStaticBaseHelperCall: true if tree is fetching the gc static base
//    for a subsequent static field access
//
// Arguments:
//    tree - tree to consider
//
// Return Value:
//    true if the tree is a suitable helper call
//
// Notes:
//    Excludes R2R helpers as they specify the target field in a way
//    that is opaque to the jit.

bool Compiler::gtIsStaticGCBaseHelperCall(GenTree* tree)
{
    if (tree->OperGet() != GT_CALL)
    {
        return false;
    }

    GenTreeCall* call = tree->AsCall();

    if (call->gtCallType != CT_HELPER)
    {
        return false;
    }

    const CorInfoHelpFunc helper = eeGetHelperNum(call->gtCallMethHnd);

    switch (helper)
    {
        // We are looking for a REF type so only need to check for the GC base helpers
        case CORINFO_HELP_GETGENERICS_GCSTATIC_BASE:
        case CORINFO_HELP_GETSHARED_GCSTATIC_BASE:
        case CORINFO_HELP_GETSHARED_GCSTATIC_BASE_NOCTOR:
        case CORINFO_HELP_GETSHARED_GCSTATIC_BASE_DYNAMICCLASS:
        case CORINFO_HELP_GETGENERICS_GCTHREADSTATIC_BASE:
        case CORINFO_HELP_GETSHARED_GCTHREADSTATIC_BASE:
        case CORINFO_HELP_GETSHARED_GCTHREADSTATIC_BASE_NOCTOR:
        case CORINFO_HELP_GETSHARED_GCTHREADSTATIC_BASE_DYNAMICCLASS:
            return true;
        default:
            break;
    }

    return false;
}

FieldSeqNode FieldSeqNode::s_notAField(nullptr);

int FieldSeqNode::BoxedValuePseudoFieldStruct;

const CORINFO_FIELD_HANDLE FieldSeqNode::BoxedValuePseudoFieldHandle =
    reinterpret_cast<CORINFO_FIELD_HANDLE>(&FieldSeqNode::BoxedValuePseudoFieldStruct);

FieldSeqStore::FieldSeqStore(Compiler* compiler)
    : m_compiler(compiler)
    , m_alloc(compiler->getAllocator(CMK_FieldSeqStore))
    , m_canonMap(new (m_alloc) FieldSeqNodeCanonMap(m_alloc))
{
}

FieldSeqNode* FieldSeqStore::CreateSingleton(CORINFO_FIELD_HANDLE fieldHnd)
{
    FieldSeqNode* seq = nullptr;

    if (!m_canonMap->Lookup(fieldHnd, &seq))
    {
        seq = new (m_alloc) FieldSeqNode(fieldHnd);
        m_canonMap->Set(fieldHnd, seq);
    }

    return seq;
}

FieldSeqNode* FieldSeqStore::GetArrayElement(unsigned elementTypeNum, uint8_t dataOffs)
{
    assert(elementTypeNum < (1u << 23));

    uintptr_t fieldHandle = elementTypeNum;
    fieldHandle <<= 8;
    fieldHandle |= dataOffs;
    fieldHandle <<= 1;
    fieldHandle |= 1;

    return CreateSingleton(reinterpret_cast<CORINFO_FIELD_HANDLE>(fieldHandle));
}

FieldSeqNode* FieldSeqStore::Append(FieldSeqNode* a, FieldSeqNode* b)
{
    if (a == nullptr)
    {
        return b;
    }

    if (a == NotAField())
    {
        return NotAField();
    }

    if (b == nullptr)
    {
        return a;
    }

    if (b == NotAField())
    {
        return NotAField();
    }

    FieldSeqNode* seq[FieldSeqNode::MaxLength];
    unsigned      len = 0;

    for (FieldSeqNode* p = a; p != nullptr; p = p->GetNext())
    {
        seq[len++] = p;

        if (len == _countof(seq))
        {
            return NotAField();
        }

        // TODO-MIKE-Cleanup: Recursive single field struct promotion implementation is messy,
        // at least in connection to implicit byref param demotion. We may start with an access
        // to field X of a param and recursive promotion changes it to X.Y, there's no X promoted
        // field. Demotion would need to restore the original X.Y field sequence but it doesn't
        // have a good way to know it was X.Y and not just X. In some case we may guess the field
        // sequence based on the type of an indir (if there's one at all) but it doesn't seem
        // very reliable. If we demote to X.Y instead of X we can later have issues when promoting
        // struct copies - if there was an X = X assignment we need to make it X.Y = X.Y and now
        // the field sequence coming from demotion is already X.Y and appending another Y is not
        // correct.
        //
        // So for now tolerate appending field sequences with suffix/prefix overlapping:
        //   - x.y.z + y.z = x.y.z
        //   - x.y.z + z.w = x.y.z.w
        //   - x.y.z + y = invalid (not a field)
        //
        // This isn't ideal - such cases may also arise from invalid IL and/or incorrect handling
        // of reinterpretation in user code. In those case it would be better to treat all cases
        // as invalid.

        if (p->m_fieldHnd == b->m_fieldHnd)
        {
            do
            {
                p = p->GetNext();
                b = b->GetNext();
            } while ((p != nullptr) && (b != nullptr) && p->m_fieldHnd == b->m_fieldHnd);

            // b is somewhere inside a, that's likely due to bad IL.
            noway_assert(p == nullptr);

            // b is a suffix of a, just return a.
            if (b == nullptr)
            {
                return a;
            }

            // a prefix of b is a suffix of a, we still need to append the rest of b.
            break;
        }
    }

    seq[len] = b;

    // Skip prefix nodes that already exist.
    for (FieldSeqNode* p; (len != 0) && m_canonMap->Lookup({seq[len - 1], seq[len]}, &p); len--)
    {
        seq[len - 1] = p;
    }

    // Create new prefix nodes.
    for (; len != 0; len--)
    {
        FieldSeqNode* p = new (m_alloc) FieldSeqNode(seq[len - 1], seq[len]);
        m_canonMap->Set(*p, p);
        INDEBUG(DebugCheck(p);)
        seq[len - 1] = p;
    }

    return seq[0];
}

FieldSeqNode* FieldSeqStore::Append(FieldSeqNode* a, CORINFO_FIELD_HANDLE b)
{
    return Append(a, CreateSingleton(b));
}

FieldSeqNode* FieldSeqStore::FoldAdd(const GenTreeIntCon* i1, const GenTreeIntCon* i2)
{
    assert(i1->TypeIs(TYP_I_IMPL) && i2->TypeIs(TYP_I_IMPL));

    FieldSeqNode* f1 = i1->GetFieldSeq();
    FieldSeqNode* f2 = i2->GetFieldSeq();

    assert(f1 != nullptr);
    assert(f2 != nullptr);

    if (f2->IsArrayElement())
    {
        // ArrayElement is always first in the field sequence so we don't need to worry
        // about correct field sequence ordering.

        std::swap(f1, f2);
        std::swap(i1, i2);
    }

    if (f1->IsArrayElement())
    {
        unsigned elemTypeNum  = f1->GetArrayElementTypeNum();
        unsigned elemTypeSize = m_compiler->typIsLayoutNum(elemTypeNum)
                                    ? m_compiler->typGetLayoutByNum(elemTypeNum)->GetSize()
                                    : varTypeSize(static_cast<var_types>(elemTypeNum));

        target_size_t offset1 = static_cast<target_ssize_t>(i1->GetValue());
        target_size_t offset2 = static_cast<target_ssize_t>(i2->GetValue());

        if (f2->IsField() && (offset2 < elemTypeSize))
        {
            return Append(f1, f2);
        }

        // If the second offset is a multiple of the element size it means that we had a
        // constant index and the offset expression was folded to a constant.

        if ((f2 == FieldSeqStore::NotAField()) && ((offset2 % elemTypeSize) == 0))
        {
            return f1;
        }

        // TODO-MIKE-CQ: Can we make a ArrayElement + NotAField field sequence so we don't discard
        // array information? If f2 is NotAField (because, for example, the array element type is a
        // struct with overlapping fields - System.Runtime.Caching has some examples) that doesn't
        // mean that the store may alias an array having a different element type. Or instead of
        // an ArrayElement + NotAField field sequence keep only ArrayElement and have VN check the
        // offset - if it's not a multiple of the array element type size then it means that some
        // part of the array element is accessed and since there's no Field present in the field
        // sequence that means we're dealing with a "not a field" case. In this version we don't
        // need to deal with the pre-existing "NotAField + anything = NotAField" rule.

        return FieldSeqStore::NotAField();
    }

    return Append(f1, f2);
}

#ifdef DEBUG
void FieldSeqStore::DebugCheck(FieldSeqNode* f)
{
    FieldSeqNode* a = f;
    FieldSeqNode* b = f->m_next;

    assert(!b->IsArrayElement());

    if (a->IsBoxedValueField())
    {
        // We don't know the class of a boxed value field so we can't check
        // if the appended field is valid. At least check that we're not
        // trying to append yet another boxed value field.
        assert(!b->IsBoxedValueField());

        return;
    }

    ICorJitInfo* vm = m_compiler->info.compCompHnd;

    if (b->IsBoxedValueField())
    {
        // Boxed value fields are only used together with static fields.
        assert(!a->IsBoxedValueField());
        assert(!a->IsArrayElement());
        assert(vm->isFieldStatic(a->m_fieldHnd));

        return;
    }

    if (a->IsArrayElement())
    {
        assert(b->IsField());

        return;
    }

    CORINFO_CLASS_HANDLE fieldClass;
    CorInfoType          t = vm->getFieldType(a->m_fieldHnd, &fieldClass);

    // It really should be a value class but we may also get a primitive type due
    // to the normed type mess. It's also possible to end up attempting to access
    // the value field of a primtive struct (Int32, Double etc.) via the primitive
    // type itself, e.g. by calling GetHashCode on a Nullable<double> local.
    // Double.GetHashCode is inlined and due to inliner arg substitution we end up
    // with something like:
    //   FIELD_ADDR Double.m_value (FIELD_ADDR Nullable.value)
    //
    // TODO-MIKE-Cleanup: Probably the importer should detect and discard the
    // redundant FIELD_ADDR node.
    // At least in the Double.GetHashCode case this doesn't seem to have any ill
    // side effects - either the Nullable<Double> local is promoted and then the
    // whole thing is replaced with a LCL_VAR or it is not promoted and then we
    // get a LCL_FLD with NotAField field sequence because GetHashCode actually
    // reinterprets the double value as long.

    if (t != CORINFO_TYPE_VALUECLASS)
    {
        return;
    }

    // In theory the below check should be just
    //     assert(vm->getFieldClass(b->m_fieldHnd) != fieldClass)
    // but this doesn't work because sometimes one class is A<Cannon> while the other is A<SomeRefType>...

    unsigned fieldCount = vm->getClassNumInstanceFields(fieldClass);
    unsigned fieldIndex = 0;

    for (fieldIndex = 0; fieldIndex < fieldCount; fieldIndex++)
    {
        if (vm->getFieldInClass(fieldClass, fieldIndex) == b->m_fieldHnd)
        {
            break;
        }
    }

    if (fieldIndex >= fieldCount)
    {
        if (m_compiler->verbose)
        {
            printf("%s.%s - field %s not found in class %s\n", m_compiler->eeGetFieldName(a->m_fieldHnd),
                   m_compiler->eeGetFieldName(b->m_fieldHnd), m_compiler->eeGetFieldName(b->m_fieldHnd),
                   m_compiler->eeGetClassName(fieldClass));
        }

        // TODO-MIKE-Fix: This assert started failing after the last merge from main. Field sequence
        // validity should be properly checked at import time instead of having such debug checks later.
        // assert(!"Field not found in class");
    }
}
#endif // DEBUG

#ifdef FEATURE_HW_INTRINSICS
bool GenTree::isCommutativeHWIntrinsic() const
{
    assert(gtOper == GT_HWINTRINSIC);

#ifdef TARGET_XARCH
    return HWIntrinsicInfo::IsCommutative(AsHWIntrinsic()->GetIntrinsic());
#else
    return false;
#endif // TARGET_XARCH
}

bool GenTree::isContainableHWIntrinsic() const
{
    assert(gtOper == GT_HWINTRINSIC);

    switch (AsHWIntrinsic()->GetIntrinsic())
    {
#ifdef TARGET_XARCH
        case NI_SSE_LoadAlignedVector128:
        case NI_SSE_LoadScalarVector128:
        case NI_SSE_LoadVector128:
        case NI_SSE2_LoadAlignedVector128:
        case NI_SSE2_LoadScalarVector128:
        case NI_SSE2_LoadVector128:
        case NI_AVX_LoadAlignedVector256:
        case NI_AVX_LoadVector256:
        case NI_AVX_ExtractVector128:
        case NI_AVX2_ExtractVector128:
        case NI_Vector256_get_Zero:
#endif
#ifdef TARGET_ARM64
        case NI_Vector64_get_Zero:
#endif
        case NI_Vector128_get_Zero:
            return true;

        default:
            return false;
    }
}

bool GenTree::isRMWHWIntrinsic(Compiler* comp)
{
    assert(gtOper == GT_HWINTRINSIC);
    assert(comp != nullptr);

#if defined(TARGET_XARCH)
    if (!comp->canUseVexEncoding())
    {
        return HWIntrinsicInfo::HasRMWSemantics(AsHWIntrinsic()->GetIntrinsic());
    }

    switch (AsHWIntrinsic()->GetIntrinsic())
    {
        // TODO-XArch-Cleanup: Move this switch block to be table driven.

        case NI_SSE42_Crc32:
        case NI_SSE42_X64_Crc32:
        case NI_FMA_MultiplyAdd:
        case NI_FMA_MultiplyAddNegated:
        case NI_FMA_MultiplyAddNegatedScalar:
        case NI_FMA_MultiplyAddScalar:
        case NI_FMA_MultiplyAddSubtract:
        case NI_FMA_MultiplySubtract:
        case NI_FMA_MultiplySubtractAdd:
        case NI_FMA_MultiplySubtractNegated:
        case NI_FMA_MultiplySubtractNegatedScalar:
        case NI_FMA_MultiplySubtractScalar:
        {
            return true;
        }

        default:
        {
            return false;
        }
    }
#elif defined(TARGET_ARM64)
    return HWIntrinsicInfo::HasRMWSemantics(AsHWIntrinsic()->GetIntrinsic());
#else
    return false;
#endif
}

GenTreeHWIntrinsic* Compiler::gtNewZeroSimdHWIntrinsicNode(ClassLayout* layout)
{
    return new (this, GT_HWINTRINSIC)
        GenTreeHWIntrinsic(layout->GetSIMDType(), GetZeroSimdHWIntrinsic(layout->GetSIMDType()),
                           layout->GetElementType(), layout->GetSize());
}

GenTreeHWIntrinsic* Compiler::gtNewZeroSimdHWIntrinsicNode(var_types type, var_types baseType)
{
    return new (this, GT_HWINTRINSIC)
        GenTreeHWIntrinsic(type, GetZeroSimdHWIntrinsic(type), baseType, varTypeSize(type));
}

GenTreeHWIntrinsic* Compiler::gtNewSimdHWIntrinsicNode(var_types      type,
                                                       NamedIntrinsic hwIntrinsicID,
                                                       var_types      baseType,
                                                       unsigned       size)
{
    return new (this, GT_HWINTRINSIC) GenTreeHWIntrinsic(type, hwIntrinsicID, baseType, size);
}

GenTreeHWIntrinsic* Compiler::gtNewSimdHWIntrinsicNode(
    var_types type, NamedIntrinsic hwIntrinsicID, var_types baseType, unsigned size, GenTree* op1)
{
    lvaRecordSimdIntrinsicUse(op1);
    return new (this, GT_HWINTRINSIC) GenTreeHWIntrinsic(type, hwIntrinsicID, baseType, size, op1);
}

GenTreeHWIntrinsic* Compiler::gtNewSimdHWIntrinsicNode(
    var_types type, NamedIntrinsic hwIntrinsicID, var_types baseType, unsigned size, GenTree* op1, GenTree* op2)
{
    lvaRecordSimdIntrinsicUse(op1);
    lvaRecordSimdIntrinsicUse(op2);
    return new (this, GT_HWINTRINSIC) GenTreeHWIntrinsic(type, hwIntrinsicID, baseType, size, op1, op2);
}

GenTreeHWIntrinsic* Compiler::gtNewSimdHWIntrinsicNode(var_types      type,
                                                       NamedIntrinsic hwIntrinsicID,
                                                       var_types      baseType,
                                                       unsigned       size,
                                                       GenTree*       op1,
                                                       GenTree*       op2,
                                                       GenTree*       op3)
{
    lvaRecordSimdIntrinsicUse(op1);
    lvaRecordSimdIntrinsicUse(op2);
    lvaRecordSimdIntrinsicUse(op3);
    return new (this, GT_HWINTRINSIC) GenTreeHWIntrinsic(type, hwIntrinsicID, baseType, size, op1, op2, op3);
}

GenTreeHWIntrinsic* Compiler::gtNewSimdHWIntrinsicNode(var_types      type,
                                                       NamedIntrinsic hwIntrinsicID,
                                                       var_types      baseType,
                                                       unsigned       size,
                                                       GenTree*       op1,
                                                       GenTree*       op2,
                                                       GenTree*       op3,
                                                       GenTree*       op4)
{
    GenTreeHWIntrinsic* node = new (this, GT_HWINTRINSIC) GenTreeHWIntrinsic(type, hwIntrinsicID, baseType, size);
    node->SetNumOps(4, getAllocator(CMK_ASTNode));
    node->SetOp(0, op1);
    node->SetOp(1, op2);
    node->SetOp(2, op3);
    node->SetOp(3, op4);
    for (GenTreeHWIntrinsic::Use& use : node->Uses())
    {
        node->gtFlags |= use.GetNode()->gtFlags & GTF_ALL_EFFECT;
        lvaRecordSimdIntrinsicUse(use.GetNode());
    }
    return node;
}

GenTreeHWIntrinsic* Compiler::gtNewSimdHWIntrinsicNode(var_types      type,
                                                       NamedIntrinsic hwIntrinsicID,
                                                       var_types      baseType,
                                                       unsigned       size,
                                                       GenTree*       op1,
                                                       GenTree*       op2,
                                                       GenTree*       op3,
                                                       GenTree*       op4,
                                                       GenTree*       op5)
{
    GenTreeHWIntrinsic* node = new (this, GT_HWINTRINSIC) GenTreeHWIntrinsic(type, hwIntrinsicID, baseType, size);
    node->SetNumOps(5, getAllocator(CMK_ASTNode));
    node->SetOp(0, op1);
    node->SetOp(1, op2);
    node->SetOp(2, op3);
    node->SetOp(3, op4);
    node->SetOp(4, op5);
    for (GenTreeHWIntrinsic::Use& use : node->Uses())
    {
        node->gtFlags |= use.GetNode()->gtFlags & GTF_ALL_EFFECT;
        lvaRecordSimdIntrinsicUse(use.GetNode());
    }
    return node;
}

GenTreeHWIntrinsic* Compiler::gtNewSimdHWIntrinsicNode(
    var_types type, NamedIntrinsic hwIntrinsicID, var_types baseType, unsigned size, unsigned numOps, GenTree** ops)
{
    GenTreeHWIntrinsic* node = new (this, GT_HWINTRINSIC) GenTreeHWIntrinsic(type, hwIntrinsicID, baseType, size);
    node->SetNumOps(numOps, getAllocator(CMK_ASTNode));
    for (unsigned i = 0; i < numOps; i++)
    {
        node->SetOp(i, ops[i]);
        node->gtFlags |= ops[i]->gtFlags & GTF_ALL_EFFECT;
        lvaRecordSimdIntrinsicUse(ops[i]);
    }
    return node;
}

GenTreeHWIntrinsic* Compiler::NewExtractVectorElement(var_types vecType,
                                                      var_types eltType,
                                                      GenTree*  vec,
                                                      unsigned  index)
{
    return gtNewSimdGetElementNode(vecType, eltType, vec, gtNewIconNode(index));
}

GenTreeHWIntrinsic* Compiler::gtNewSimdGetElementNode(var_types simdType,
                                                      var_types elementType,
                                                      GenTree*  value,
                                                      GenTree*  index)
{
    assert(varTypeIsSIMD(simdType));
    assert(varTypeIsArithmetic(elementType));
    assert(varActualType(index->GetType()) == TYP_INT);

    NamedIntrinsic intrinsic = NI_Vector128_GetElement;
    unsigned       size;

#ifdef TARGET_XARCH
    if (simdType == TYP_SIMD32)
    {
        intrinsic = NI_Vector256_GetElement;
        size      = 32;
    }
    else
    {
        size = 16;
    }
#elif defined(TARGET_ARM64)
    if (simdType == TYP_SIMD8)
    {
        intrinsic = NI_Vector64_GetElement;
        size      = 8;
    }
    else
    {
        size = 16;
    }
#else
#error Unsupported platform
#endif

    return gtNewSimdHWIntrinsicNode(varTypeNodeType(elementType), intrinsic, elementType, size, value, index);
}

GenTreeHWIntrinsic* Compiler::gtNewSimdWithElementNode(
    var_types type, var_types eltType, GenTree* vec, GenTreeIntCon* idx, GenTree* elt)
{
    assert(varTypeIsSIMD(type));
    assert(varTypeIsArithmetic(eltType));
    assert(idx->GetUInt32Value() < varTypeSize(type) / varTypeSize(eltType));

    NamedIntrinsic intrinsic;
    unsigned       simdSize;

#if defined(TARGET_XARCH)
    assert(varTypeIsFloating(eltType) || varTypeIsShort(eltType) || compIsaSupportedDebugOnly(InstructionSet_SSE41));
    assert((eltType == TYP_FLOAT) || (varTypeSize(type) >= 16));

    intrinsic = type == TYP_SIMD32 ? NI_Vector256_WithElement : NI_Vector128_WithElement;
    simdSize  = type == TYP_SIMD32 ? 32 : 16;
#elif defined(TARGET_ARM64)
    if ((type == TYP_SIMD8) && (varTypeSize(eltType) == 8))
    {
        return gtNewSimdHWIntrinsicNode(TYP_SIMD8, NI_Vector64_Create, eltType, 8, elt);
    }

    intrinsic = NI_AdvSimd_Insert;
    simdSize  = type == TYP_SIMD8 ? 8 : 16;
#else
#error Unsupported platform
#endif // !TARGET_XARCH && !TARGET_ARM64

    return gtNewSimdHWIntrinsicNode(type, intrinsic, eltType, simdSize, vec, idx, elt);
}

GenTreeHWIntrinsic* Compiler::gtNewScalarHWIntrinsicNode(var_types type, NamedIntrinsic hwIntrinsicID, GenTree* op1)
{
    return new (this, GT_HWINTRINSIC) GenTreeHWIntrinsic(type, hwIntrinsicID, TYP_UNDEF, 0, op1);
}

GenTreeHWIntrinsic* Compiler::gtNewScalarHWIntrinsicNode(var_types      type,
                                                         NamedIntrinsic hwIntrinsicID,
                                                         GenTree*       op1,
                                                         GenTree*       op2)
{
    return new (this, GT_HWINTRINSIC) GenTreeHWIntrinsic(type, hwIntrinsicID, TYP_UNDEF, 0, op1, op2);
}

GenTreeHWIntrinsic* Compiler::gtNewScalarHWIntrinsicNode(
    var_types type, NamedIntrinsic hwIntrinsicID, GenTree* op1, GenTree* op2, GenTree* op3)
{
    return new (this, GT_HWINTRINSIC) GenTreeHWIntrinsic(type, hwIntrinsicID, TYP_UNDEF, 0, op1, op2, op3);
}

// Returns true for the HW Intrinsic instructions that have MemoryLoad semantics, false otherwise
bool GenTreeHWIntrinsic::OperIsMemoryLoad() const
{
#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
    HWIntrinsicCategory category = HWIntrinsicInfo::lookupCategory(m_intrinsic);
    if (category == HW_Category_MemoryLoad)
    {
        return true;
    }
#ifdef TARGET_XARCH
    else if (HWIntrinsicInfo::MaybeMemoryLoad(m_intrinsic))
    {
        // Some intrinsics (without HW_Category_MemoryLoad) also have MemoryLoad semantics

        if (category == HW_Category_SIMDScalar)
        {
            // Avx2.BroadcastScalarToVector128/256 have vector and pointer overloads both, e.g.,
            // Vector128<byte> BroadcastScalarToVector128(Vector128<byte> value)
            // Vector128<byte> BroadcastScalarToVector128(byte* source)
            // So, we need to check the argument's type is memory-reference or Vector128
            assert(IsUnary());
            return (m_intrinsic == NI_AVX2_BroadcastScalarToVector128 ||
                    m_intrinsic == NI_AVX2_BroadcastScalarToVector256) &&
                   GetOp(0)->TypeGet() != TYP_SIMD16;
        }
        else if (category == HW_Category_IMM)
        {
            // Do we have less than 3 operands?
            if (GetNumOps() < 3)
            {
                return false;
            }
            else if (HWIntrinsicInfo::isAVX2GatherIntrinsic(m_intrinsic))
            {
                return true;
            }
        }
    }
#endif // TARGET_XARCH
#endif // TARGET_XARCH || TARGET_ARM64
    return false;
}

// Returns true for the HW Intrinsic instructions that have MemoryStore semantics, false otherwise
bool GenTreeHWIntrinsic::OperIsMemoryStore() const
{
#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
    HWIntrinsicCategory category = HWIntrinsicInfo::lookupCategory(m_intrinsic);
    if (category == HW_Category_MemoryStore)
    {
        return true;
    }
#ifdef TARGET_XARCH
    else if (HWIntrinsicInfo::MaybeMemoryStore(m_intrinsic) &&
             (category == HW_Category_IMM || category == HW_Category_Scalar))
    {
        // Some intrinsics (without HW_Category_MemoryStore) also have MemoryStore semantics

        // Bmi2/Bmi2.X64.MultiplyNoFlags may return the lower half result by a out argument
        // unsafe ulong MultiplyNoFlags(ulong left, ulong right, ulong* low)
        //
        // So, the 3-argument form is MemoryStore
        if (IsTernary())
        {
            switch (m_intrinsic)
            {
                case NI_BMI2_MultiplyNoFlags:
                case NI_BMI2_X64_MultiplyNoFlags:
                    return true;
                default:
                    return false;
            }
        }
    }
#endif // TARGET_XARCH
#endif // TARGET_XARCH || TARGET_ARM64
    return false;
}

// Returns true for the HW Intrinsic instructions that have MemoryLoad or MemoryStore semantics, false otherwise
bool GenTreeHWIntrinsic::OperIsMemoryLoadOrStore() const
{
#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
    return OperIsMemoryLoad() || OperIsMemoryStore();
#else
    return false;
#endif
}

#endif // FEATURE_HW_INTRINSICS

#if FEATURE_MULTIREG_RET
void ReturnTypeDesc::InitializeStruct(Compiler* comp, ClassLayout* retLayout)
{
    if (retLayout->IsHfa())
    {
        m_regCount = retLayout->GetHfaElementCount();
        assert((m_regCount >= 2) && (m_regCount <= _countof(m_regType)));

        var_types regType = retLayout->GetHfaElementType();

        for (unsigned i = 0; i < m_regCount; ++i)
        {
            m_regType[i] = regType;
        }

        comp->compFloatingPointUsed |= true;

        return;
    }

#ifdef UNIX_AMD64_ABI
    assert(retLayout->GetSysVAmd64AbiRegCount() == 2);

    m_regCount = 2;

    for (int i = 0; i < 2; i++)
    {
        m_regType[i] = varActualType(retLayout->GetSysVAmd64AbiRegType(i));
    }
#elif defined(TARGET_ARM64) || defined(TARGET_X86)
    assert(retLayout->GetSlotCount() == 2);

    m_regCount = 2;

    for (unsigned i = 0; i < 2; i++)
    {
        m_regType[i] = retLayout->GetGCPtrType(i);
    }
#else
    unreached();
#endif
}
#endif //  FEATURE_MULTIREG_RET

void ReturnTypeDesc::InitializePrimitive(var_types regType)
{
    if (regType == TYP_VOID)
    {
        m_regCount = 0;
    }
    else if (varTypeIsLong(regType))
    {
        InitializeLong();
    }
    else
    {
        assert(varTypeIsGC(regType) || varTypeIsArithmetic(regType) || varTypeIsSIMD(regType));

        m_regCount   = 1;
        m_regType[0] = regType;
    }
}

void ReturnTypeDesc::InitializeLong()
{
#ifdef TARGET_64BIT
    m_regCount   = 1;
    m_regType[0] = TYP_LONG;
#else
    assert(MAX_RET_REG_COUNT >= 2);

    m_regCount   = 2;
    m_regType[0] = TYP_INT;
    m_regType[1] = TYP_INT;
#endif
}

regNumber ReturnTypeDesc::GetRegNum(unsigned i) const
{
    assert(i < m_regCount);

#if defined(TARGET_X86)
    assert(!varTypeUsesFloatReg(m_regType[i]));
    return i == 0 ? REG_EAX : REG_EDX;
#elif defined(WINDOWS_AMD64_ABI)
    return varTypeUsesFloatReg(m_regType[0]) ? REG_XMM0 : REG_RAX;
#elif defined(UNIX_AMD64_ABI)
    if (i == 0)
    {
        return varTypeUsesFloatReg(m_regType[0]) ? REG_XMM0 : REG_RAX;
    }

    if (varTypeUsesFloatReg(m_regType[1]))
    {
        return varTypeUsesFloatReg(m_regType[0]) ? REG_XMM1 : REG_XMM0;
    }
    else
    {
        return varTypeUsesFloatReg(m_regType[0]) ? REG_RAX : REG_RDX;
    }
#elif defined(TARGET_ARM)
    regNumber firstReg = REG_R0;

    if (varTypeUsesFloatReg(m_regType[0]))
    {
        firstReg = REG_F0;

        if (m_regType[0] == TYP_DOUBLE)
        {
            // A DOUBLE reg uses 2 consecutive FLOAT registers.
            i *= 2;
        }
    }

    return static_cast<regNumber>(firstReg + i);
#elif defined(TARGET_ARM64)
    regNumber firstReg = varTypeUsesFloatReg(m_regType[0]) ? REG_V0 : REG_R0;

    return static_cast<regNumber>(firstReg + i);
#else
    return REG_NA;
#endif
}

//------------------------------------------------------------------------
// The following functions manage the gtRsvdRegs set of temporary registers
// created by LSRA during code generation.

//------------------------------------------------------------------------
// AvailableTempRegCount: return the number of available temporary registers in the (optional) given set
// (typically, RBM_ALLINT or RBM_ALLFLOAT).
//
// Arguments:
//    mask - (optional) Check for available temporary registers only in this set.
//
// Return Value:
//    Count of available temporary registers in given set.
//
unsigned GenTree::AvailableTempRegCount(regMaskTP mask /* = (regMaskTP)-1 */) const
{
    return genCountBits(gtRsvdRegs & mask);
}

//------------------------------------------------------------------------
// GetSingleTempReg: There is expected to be exactly one available temporary register
// in the given mask in the gtRsvdRegs set. Get that register. No future calls to get
// a temporary register are expected. Removes the register from the set, but only in
// DEBUG to avoid doing unnecessary work in non-DEBUG builds.
//
// Arguments:
//    mask - (optional) Get an available temporary register only in this set.
//
// Return Value:
//    Available temporary register in given mask.
//
regNumber GenTree::GetSingleTempReg(regMaskTP mask /* = (regMaskTP)-1 */)
{
    regMaskTP availableSet = gtRsvdRegs & mask;
    assert(genCountBits(availableSet) == 1);
    regNumber tempReg = genRegNumFromMask(availableSet);
    INDEBUG(gtRsvdRegs &= ~availableSet;) // Remove the register from the set, so it can't be used again.
    return tempReg;
}

//------------------------------------------------------------------------
// ExtractTempReg: Find the lowest number temporary register from the gtRsvdRegs set
// that is also in the optional given mask (typically, RBM_ALLINT or RBM_ALLFLOAT),
// and return it. Remove this register from the temporary register set, so it won't
// be returned again.
//
// Arguments:
//    mask - (optional) Extract an available temporary register only in this set.
//
// Return Value:
//    Available temporary register in given mask.
//
regNumber GenTree::ExtractTempReg(regMaskTP mask /* = (regMaskTP)-1 */)
{
    regMaskTP availableSet = gtRsvdRegs & mask;
    assert(genCountBits(availableSet) >= 1);
    regMaskTP tempRegMask = genFindLowestBit(availableSet);
    gtRsvdRegs &= ~tempRegMask;
    return genRegNumFromMask(tempRegMask);
}

bool GenTree::HasTempReg(regNumber reg) const
{
    return (gtRsvdRegs & genRegMask(reg)) != 0;
}

//------------------------------------------------------------------------
// GetLclOffs: if `this` is a field or a field address it returns offset
// of the field inside the struct, for not a field it returns 0.
//
// Return Value:
//    The offset value.
//
uint16_t GenTreeLclVarCommon::GetLclOffs() const
{
    if (const GenTreeLclFld* lclFld = IsLclFld())
    {
        return lclFld->GetLclOffs();
    }

    return 0;
}

#ifdef TARGET_ARM
//------------------------------------------------------------------------
// IsOffsetMisaligned: check if the field needs a special handling on arm.
//
// Return Value:
//    true if it is a float field with a misaligned offset, false otherwise.
//
bool GenTreeLclFld::IsOffsetMisaligned() const
{
    if (varTypeIsFloating(gtType))
    {
        return ((m_lclOffs % emitTypeSize(TYP_FLOAT)) != 0);
    }
    return false;
}
#endif // TARGET_ARM

bool Compiler::gtIsSmallIntCastNeeded(GenTree* tree, var_types toType)
{
    assert(varTypeIsSmall(toType));

    if (tree->OperIsCompare())
    {
        // Relops have type INT but they always produce 0/1 values so a
        // cast is not required, no matter what the destination type is.

        return false;
    }

    if (GenTreeIntCon* con = tree->IsIntCon())
    {
        return !varTypeSmallIntCanRepresentValue(toType, con->GetValue());
    }

    var_types fromType = tree->GetType();

    assert(varTypeIsIntegral(fromType));

    if (GenTreeCast* cast = tree->IsCast())
    {
        // Casts to small int types have type INT, use the cast type instead.

        fromType = cast->GetCastType();
    }
    else if (GenTreeCall* call = tree->IsCall())
    {
        // Calls have "actual" type, use the signature type instead.
        // Note that this works only in the managed ABI (and only when we're not compiling
        // for R2R), native ABIs expect the caller to widen the return value. impImportCall
        // should have already the necessary casts so we don't need to check again here but
        // this means that this function can only be used when we need to add new casts
        // (e.g. fgMorphNormalizeLclVarStore) and not to remove existing casts.

        fromType = call->GetRetSigType();
    }
    else if ((fromType == TYP_INT) && tree->OperIs(GT_LCL_VAR))
    {
        // LCL_VARs associated with small int locals may have type INT,
        // we need to check the type of the local variable.

        fromType = lvaGetDesc(tree->AsLclVar())->GetType();
    }

    if (toType == fromType)
    {
        return false;
    }

    if (varTypeSize(fromType) > varTypeSize(toType))
    {
        return true;
    }

    if (varTypeSize(fromType) < varTypeSize(toType))
    {
        return !varTypeIsUnsigned(fromType) && varTypeIsUnsigned(toType);
    }

    return varTypeIsUnsigned(fromType) != varTypeIsUnsigned(toType);
}
