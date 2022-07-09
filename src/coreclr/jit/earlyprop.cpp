// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "ssabuilder.h"

// This phase performs an SSA-based value propagation optimization that currently only
// applies to array lengths and explicit null checks.
//
// For array length propagation, a demand-driven SSA-based backwards tracking of constant
// array lengths is performed at each array length reference site which is in form of an
// ARR_LENGTH node. When a ARR_LENGTH node is seen, the array ref pointer is tracked.
// The tracking is along SSA use-def chain and stops at the original array allocation site
// where we can grab the array length. The ARR_LENGTH node will then be rewritten to a
// CNS_INT node if the array length is constant.
//
// Null check folding tries to find NULLCHECK nodes followed by indirections that would
// throw the same NullReferenceException, making NULLCHECKs redundant.
//
//   - NULLCHECK(addr + offset1)
//     any code that does not interfere with the NULLCHECK thrown exception
//     IND(addr)
//
//   - ASG(x, COMMA(NULLCHECK(y), ADD(y, offset1)))
//     any code that does not interfere with the NULLCHECK thrown exception
//     IND(ADD(x, offset2))
//
// We can eliminate a NULLCHECK only if there are no interfering side effects between
// the NULLCHECK node and the indir. For example, we can't eliminate the NULLCHECK if
// there is an assignment to a memory location between it and the indir. Because of
// this NULLCHECKs are eliminated only when the indirections are in the same block,
// to avoid potentially costly intereference checks across blocks.

class EarlyProp
{
    static const int SsaChaseLimit = 5;

    typedef JitHashTable<unsigned, JitSmallPrimitiveKeyFuncs<unsigned>, GenTreeIndir*> LocalNumberToNullCheckTreeMap;

    Compiler*                     compiler;
    BasicBlock*                   currentBlock;
    Statement*                    currentStatement;
    LocalNumberToNullCheckTreeMap nullCheckMap;

public:
    EarlyProp(Compiler* compiler) : compiler(compiler), nullCheckMap(compiler->getAllocator(CMK_EarlyProp))
    {
    }

    void Run()
    {
        if (DoEarlyPropForFunc())
        {
            DoEarlyProp();
        }
    }

private:
    bool DoEarlyPropForFunc()
    {
        bool hasNewArray    = (compiler->optMethodFlags & OMF_HAS_NEWARRAY) != 0;
        bool hasArrayLength = (compiler->optMethodFlags & OMF_HAS_ARRAYREF) != 0;
        bool hasNullCheck   = (compiler->optMethodFlags & OMF_HAS_NULLCHECK) != 0;

        return (hasArrayLength && hasNewArray) || hasNullCheck;
    }

    bool DoEarlyPropForBlock(BasicBlock* block)
    {
        bool hasArrayLength = (block->bbFlags & BBF_HAS_IDX_LEN) != 0;
        bool hasNullCheck   = (block->bbFlags & BBF_HAS_NULLCHECK) != 0;

        return hasArrayLength || hasNullCheck;
    }

    void DoEarlyProp()
    {
        for (BasicBlock* const block : compiler->Blocks())
        {
            if (!DoEarlyPropForBlock(block))
            {
                continue;
            }

            nullCheckMap.RemoveAll();
            currentBlock = block;

            for (Statement *stmt = block->firstStmt(), *next; stmt != nullptr; stmt = next)
            {
                currentStatement  = stmt;
                next              = stmt->GetNextStmt();
                bool stmtModified = false;

                for (GenTree* node = stmt->GetNodeList(); node != nullptr; node = node->gtNext)
                {
                    if (!node->OperIsIndirOrArrLength())
                    {
                        continue;
                    }

                    if (GenTreeArrLen* arrLen = node->IsArrLen())
                    {
                        GenTree* newNode = PropagateArrayLength(arrLen);

                        if (newNode != nullptr)
                        {
                            compiler->gtUpdateTreeAncestorsSideEffects(newNode);
                            stmtModified = true;
                            continue;
                        }
                    }

                    if ((currentBlock->bbFlags & BBF_HAS_NULLCHECK) != 0)
                    {
                        FoldNullCheck(node);
                    }
                }

                if (stmtModified)
                {
                    compiler->gtSetStmtInfo(stmt);
                    compiler->fgSetStmtSeq(stmt);
                }
            }
        }

#ifdef DEBUG
        if (compiler->verbose)
        {
            JITDUMP("\nAfter DoEarlyProp:\n");
            compiler->fgDispBasicBlocks(/*dumpTrees*/ true);
        }
#endif
    }

    GenTree* PropagateArrayLength(GenTreeArrLen* arrLen)
    {
        GenTree* array = arrLen->GetArray();

        if (!array->OperIs(GT_LCL_VAR) || !compiler->lvaInSsa(array->AsLclVar()->GetLclNum()))
        {
            return nullptr;
        }

        GenTree* length = GetArrayLength(array->AsLclVar());

        if ((length == nullptr) || !length->IsIntCon())
        {
            return nullptr;
        }

        return PropagateConstArrayLength(arrLen, length->AsIntCon());
    }

    GenTree* PropagateConstArrayLength(GenTreeArrLen* arrLen, GenTreeIntCon* constLen)
    {
        ssize_t lengthValue = constLen->GetValue();

        if ((lengthValue < 0) || (lengthValue > INT32_MAX))
        {
            // Don't propagate array lengths that are beyond the maximum value of a ARR_LENGTH node
            // or negative. CORINFO_HELP_NEWARR_1_OBJ helper call allows a LONG as the array length
            // argument, but the type of ARR_LENGTH is always INT.
            return nullptr;
        }

        // When replacing ARR_LENGTH nodes with constants we can end up with ARR_BOUNDS_CHECK nodes
        // that have constant operands and thus can be trivially proved to be useless. It's better
        // to remove these range checks here, otherwise they'll reach assertion prop (creating
        // useless (c1 < c2)-like assertions) and reach RangeCheck where they are finally removed.
        // Common patterns like new int[] { x, y, z } benefit from this.

        if ((arrLen->gtNext != nullptr) && arrLen->gtNext->OperIs(GT_ARR_BOUNDS_CHECK))
        {
            GenTreeBoundsChk* check = arrLen->gtNext->AsBoundsChk();

            if ((check->GetLength() == arrLen) && check->GetIndex()->IsIntCon())
            {
                ssize_t indexValue = check->GetIndex()->AsIntCon()->GetValue();

                if ((indexValue >= 0) && (indexValue < lengthValue))
                {
                    GenTree* comma = check->FindUser();

                    check->ChangeToNothingNode();

                    if ((comma != nullptr) && comma->OperIs(GT_COMMA) && (comma->AsOp()->GetOp(0) == check))
                    {
                        GenTree** use;
                        GenTree*  user = comma->FindUser(&use);

                        if (user != nullptr)
                        {
                            *use = comma->AsOp()->GetOp(1);

                            // COMMA and ARR_LEN remain in the statment until we finish traversing the statement,
                            // remove all side effects so they don't interfere with null check folding.
                            comma->ChangeToNothingNode();
                            arrLen->ChangeToNothingNode();

                            return user;
                        }
                    }

                    return check;
                }
            }
        }

        JITDUMPTREE(currentStatement->GetRootNode(), "PropagateConstArrayLength rewriting\n");

        assert(arrLen->TypeIs(TYP_INT));
        arrLen->ChangeToIntCon(TYP_INT, lengthValue);
        arrLen->gtFlags = constLen->gtFlags;

        // Propagating a constant may create an opportunity to use a division by constant optimization
        if ((arrLen->gtNext != nullptr) && arrLen->gtNext->OperIs(GT_DIV, GT_MOD, GT_UDIV, GT_UMOD))
        {
            // We need to mark the parent divide/mod operation when this occurs
            arrLen->gtNext->AsOp()->CheckDivideByConstOptimized(compiler);
        }

        JITDUMPTREE(currentStatement->GetRootNode(), "to\n");

        return arrLen;
    }

    GenTree* GetArrayLength(GenTreeLclVar* lclVar)
    {
        INDEBUG(BasicBlock* defBlock = nullptr;)
        GenTree* value = GetSsaValue(lclVar DEBUGARG(&defBlock));
        return value->IsHelperCall() ? GetArrayLengthFromNewHelperCall(value->AsCall() DEBUGARG(defBlock)) : nullptr;
    }

    GenTree* GetArrayLengthFromNewHelperCall(GenTreeCall* call DEBUGARG(BasicBlock* block))
    {
        assert(call->IsHelperCall());

        GenTree* arrayLength = nullptr;

        switch (Compiler::eeGetHelperNum(call->GetMethodHandle()))
        {
            case CORINFO_HELP_NEWARR_1_DIRECT:
            case CORINFO_HELP_NEWARR_1_OBJ:
            case CORINFO_HELP_NEWARR_1_VC:
            case CORINFO_HELP_NEWARR_1_ALIGN8:
                arrayLength = call->GetArgNodeByArgNum(1);
                break;
            case CORINFO_HELP_READYTORUN_NEWARR_1:
                // On arm when compiling on certain platforms for ready to run, a handle will be
                // inserted before the length. To handle this case, we will grab the last argument
                // as that's always the length. See fgInitArgInfo for where the handle is inserted.
                arrayLength = call->GetArgNodeByArgNum(call->GetInfo()->GetArgCount() - 1);
                break;
            default:
                break;
        }

        return arrayLength;
    }

    GenTree* GetSsaValue(GenTreeLclVar* use DEBUGARG(BasicBlock** defBlock))
    {
        for (unsigned i = 0; (i < SsaChaseLimit) && (use->GetSsaNum() != SsaConfig::RESERVED_SSA_NUM); i++)
        {
            LclSsaVarDsc* ssaDef = compiler->lvaGetSsaDesc(use);
            GenTreeOp*    asg    = ssaDef->GetAssignment();

            INDEBUG(*defBlock = ssaDef->GetBlock());

            if (asg == nullptr)
            {
                // Parameters have no definition assignments.
                assert(use->GetSsaNum() == SsaConfig::FIRST_SSA_NUM);

                break;
            }

            assert(asg->OperIs(GT_ASG));

            GenTree* value = asg->GetOp(1);

            if (!value->OperIs(GT_LCL_VAR))
            {
                return value;
            }

            use = value->AsLclVar();
        }

        return use;
    }

    void FoldNullCheck(GenTree* indir)
    {
        GenTree* addr = indir->IsArrLen() ? indir->AsArrLen()->GetArray() : indir->AsIndir()->GetAddr();

        GenTreeIndir* nullCheck     = FindNullCheckToFold(addr);
        GenTree*      nullCheckUser = nullptr;
        Statement*    nullCheckStmt = nullptr;

        if ((nullCheck != nullptr) && IsNullCheckFoldingLegal(nullCheck, indir, &nullCheckUser, &nullCheckStmt))
        {
            JITDUMPTREE(nullCheck, "FoldNullCheck marking a NULLCHECK for removal\n");

            nullCheckMap.Remove(nullCheck->GetAddr()->AsLclVar()->GetLclNum());

            nullCheck->ChangeToNothingNode();

            if (nullCheckUser == nullptr)
            {
                assert(nullCheck == nullCheckStmt->GetRootNode());
            }
            else
            {
                // TODO-MIKE-Review: Check if this is really needed. After the null check is removed
                // the remaining subtree either has other side effects that will prevent reordering,
                // or it has no other side effects and then reordering isn't an issue. Someone likely
                // confused this with another situation that does require GTF_ORDER_SIDEEFF - an indir
                // dominated by another indir may be made "non faulting" and then we do need to prevent
                // the "non faulting" indir to be reordered in front of the dominating faulting indir.
                nullCheck->gtFlags |= GTF_ORDER_SIDEEFF;

                compiler->gtUpdateTreeAncestorsSideEffects(nullCheck);

                if (nullCheckUser->OperIs(GT_COMMA) && (nullCheckUser->AsOp()->GetOp(0) == nullCheck))
                {
                    GenTree** use;
                    GenTree*  commaUser = nullCheckUser->FindUser(&use);

                    if (commaUser != nullptr)
                    {
                        *use = nullCheckUser->AsOp()->GetOp(1);
                    }
                }
            }

            compiler->gtSetStmtInfo(nullCheckStmt);
            compiler->fgSetStmtSeq(nullCheckStmt);
        }

        if (indir->OperIs(GT_NULLCHECK) && indir->AsIndir()->GetAddr()->OperIs(GT_LCL_VAR))
        {
            nullCheckMap.Set(indir->AsIndir()->GetAddr()->AsLclVar()->GetLclNum(), indir->AsIndir(),
                             LocalNumberToNullCheckTreeMap::SetKind::Overwrite);
        }
    }

    GenTreeIndir* FindNullCheckToFold(GenTree* addr)
    {
        assert(varTypeIsI(addr->GetType()));

        ssize_t offset = 0;

        if (addr->OperIs(GT_ADD) && addr->AsOp()->GetOp(1)->IsIntCon())
        {
            offset += addr->AsOp()->GetOp(1)->AsIntCon()->GetValue();
            addr = addr->AsOp()->GetOp(0);
        }

        if (!addr->OperIs(GT_LCL_VAR))
        {
            return nullptr;
        }

        GenTreeLclVar* addrLclVar = addr->AsLclVar();

        if (addrLclVar->GetSsaNum() == SsaConfig::RESERVED_SSA_NUM)
        {
            return nullptr;
        }

        GenTreeIndir* nullCheck = nullptr;

        if (nullCheckMap.Lookup(addrLclVar->GetLclNum(), &nullCheck))
        {
            if (nullCheck->GetAddr()->AsLclVar()->GetSsaNum() != addrLclVar->GetSsaNum())
            {
                nullCheck = nullptr;
            }
        }

        if (nullCheck == nullptr)
        {
            LclSsaVarDsc* ssaDef = compiler->lvaGetSsaDesc(addrLclVar);

            // We can only check for NULLCHECK exception interference if the
            // NULLCHECK is in the same block as the indir.
            if (currentBlock != ssaDef->GetBlock())
            {
                return nullptr;
            }

            GenTree* ssaDefValue = ssaDef->GetAssignment()->GetOp(1);

            if (!ssaDefValue->OperIs(GT_COMMA))
            {
                return nullptr;
            }

            GenTree* commaOp1 = ssaDefValue->AsOp()->GetOp(0)->SkipComma();
            GenTree* commaOp2 = ssaDefValue->AsOp()->GetOp(1);

            if (!commaOp1->OperIs(GT_NULLCHECK))
            {
                return nullptr;
            }

            nullCheck = commaOp1->AsIndir();

            GenTree* nullCheckAddr = nullCheck->GetAddr();

            if (!nullCheckAddr->OperIs(GT_LCL_VAR) || !commaOp2->OperIs(GT_ADD))
            {
                return nullptr;
            }

            GenTreeOp* add = commaOp2->AsOp();

            if (!add->GetOp(0)->OperIs(GT_LCL_VAR) ||
                (add->GetOp(0)->AsLclVar()->GetLclNum() != nullCheckAddr->AsLclVar()->GetLclNum()))
            {
                return nullptr;
            }

            if (!add->GetOp(1)->IsIntCon())
            {
                return nullptr;
            }

            offset += add->GetOp(1)->AsIntCon()->GetValue();
        }

        return compiler->fgIsBigOffset(offset) ? nullptr : nullCheck;
    }

    bool IsNullCheckFoldingLegal(GenTree* nullCheck, GenTree* indir, GenTree** nullCheckUser, Statement** nullCheckStmt)
    {
        assert(nullCheck->OperIs(GT_NULLCHECK));
        assert(indir->OperIsIndirOrArrLength());

        // Usually we can ignore assignments to any locals that are not address exposed,
        // but in try regions we also need to ignore assignemtns to locals that are live
        // in exception handlers.
        bool isInsideTry = currentBlock->hasTryIndex();

        const unsigned maxNodesWalked = 50;
        unsigned       nodesWalked    = 0;

        Statement* indirStatement         = currentStatement;
        GenTree*   nullCheckStatementRoot = nullCheck;

        // TODO-MIKE-Review: Could we simply remove null checks from the map as we traverse
        // the block and entirely avoid this intereference check?

        for (GenTree* node = nullCheck->gtNext; node != nullptr; node = node->gtNext)
        {
            if (node == indir)
            {
                *nullCheckStmt = indirStatement;

                // We may have reached the indir before NULLCHECK's user.
                if (*nullCheckUser == nullptr)
                {
                    *nullCheckUser = nullCheck->FindUser();
                }

                return true;
            }

            if ((*nullCheckUser == nullptr) && node->HasUse(nullCheck))
            {
                *nullCheckUser = node;
            }

            if ((nodesWalked++ > maxNodesWalked) || !CanMoveNullCheckPastNode(node, isInsideTry))
            {
                return false;
            }

            nullCheckStatementRoot = node;
        }

        for (GenTree* node = indir->gtPrev; node != nullptr; node = node->gtPrev)
        {
            if ((nodesWalked++ > maxNodesWalked) || !CanMoveNullCheckPastNode(node, isInsideTry))
            {
                return false;
            }
        }

        Statement* stmt = indirStatement->GetPrevStmt();

        for (; stmt->GetRootNode() != nullCheckStatementRoot; stmt = stmt->GetPrevStmt())
        {
            if ((nodesWalked++ > maxNodesWalked) || !CanMoveNullCheckPastStmt(stmt, isInsideTry))
            {
                return false;
            }
        }

        *nullCheckStmt = stmt;

        return true;
    }

    bool CanMoveNullCheckPastNode(GenTree* node, bool isInsideTry)
    {
        if (((node->gtFlags & GTF_CALL) != 0) && node->OperRequiresCallFlag(compiler))
        {
            return false;
        }

        // TODO-MIKE-Review: Perhaps we can move a null check past another null check,
        // they should always throw NullReferenceException.
        if (((node->gtFlags & GTF_EXCEPT) != 0) && node->OperMayThrow(compiler))
        {
            return false;
        }

        if ((node->gtFlags & GTF_ASG) == 0)
        {
            return true;
        }

        if (node->OperIs(GT_ASG))
        {
            GenTree* dst = node->AsOp()->GetOp(0);

            if (isInsideTry)
            {
                // Inside try we allow only assignments to locals not live in handlers.
                // TODO-MIKE-Review: This should probably check AX too.
                return dst->OperIs(GT_LCL_VAR) && !compiler->lvaGetDesc(dst->AsLclVar())->lvEHLive;
            }
            else
            {
                // We disallow only assignments to global memory.
                return (dst->gtFlags & GTF_GLOB_REF) == 0;
            }
        }

        return !isInsideTry && (!node->OperRequiresAsgFlag() || ((node->gtFlags & GTF_GLOB_REF) == 0));
    }

    bool CanMoveNullCheckPastStmt(Statement* stmt, bool isInsideTry)
    {
        GenTree* node = stmt->GetRootNode();

        if ((node->gtFlags & (GTF_CALL | GTF_EXCEPT)) != 0)
        {
            return false;
        }

        if ((node->gtFlags & GTF_ASG) == 0)
        {
            return true;
        }

        if (node->OperIs(GT_ASG))
        {
            if ((node->AsOp()->GetOp(1)->gtFlags & GTF_ASG) != 0)
            {
                return false;
            }

            GenTree* dst = node->AsOp()->GetOp(0);

            if (isInsideTry)
            {
                // Inside try we allow only assignments to locals not live in handlers.
                // TODO-MIKE-Review: This should probably check AX too.
                return dst->OperIs(GT_LCL_VAR) && !compiler->lvaGetDesc(dst->AsLclVar())->lvEHLive;
            }
            else
            {
                // We disallow only assignments to global memory.
                return (dst->gtFlags & GTF_GLOB_REF) == 0;
            }
        }

        return !isInsideTry && ((node->gtFlags & GTF_GLOB_REF) == 0);
    }
};

void Compiler::optEarlyProp()
{
    assert(ssaForm);

    EarlyProp prop(this);
    prop.Run();
}
