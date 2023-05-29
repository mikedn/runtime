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
//   - SSA_DEF(x, COMMA(NULLCHECK(y), ADD(y, offset1)))
//     any code that does not interfere with the NULLCHECK thrown exception
//     IND(ADD(SSA_USE(x), offset2))
//
// We can eliminate a NULLCHECK only if there are no interfering side effects between
// the NULLCHECK node and the indir. For example, we can't eliminate the NULLCHECK if
// there is an assignment to a memory location between it and the indir. Because of
// this NULLCHECKs are eliminated only when the indirections are in the same block,
// to avoid potentially costly interference checks across blocks.

class EarlyProp
{
    static const int SsaChaseLimit = 5;

    typedef JitHashTable<GenTreeLclDef*, JitPtrKeyFuncs<GenTreeLclDef>, GenTreeIndir*> DefNullCheckMap;

    Compiler*       compiler;
    BasicBlock*     currentBlock;
    Statement*      currentStatement;
    DefNullCheckMap nullCheckMap;
    bool            madeChanges = false;

public:
    EarlyProp(Compiler* compiler) : compiler(compiler), nullCheckMap(compiler->getAllocator(CMK_EarlyProp))
    {
    }

    bool Run()
    {
        if (DoEarlyPropForFunc())
        {
            DoEarlyProp();
        }

        return madeChanges;
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
                    madeChanges = true;
                }
            }
        }
    }

    GenTree* PropagateArrayLength(GenTreeArrLen* arrLen)
    {
        GenTreeLclUse* array = arrLen->GetArray()->IsLclUse();

        if (array == nullptr)
        {
            return nullptr;
        }

        GenTree* length = GetArrayLength(array);

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

        if ((arrLen->gtNext != nullptr) && arrLen->gtNext->IsBoundsChk())
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

    GenTree* GetArrayLength(GenTreeLclUse* use)
    {
        GenTree* value = GetSsaValue(use);
        return value->IsHelperCall() ? GetArrayLengthFromNewHelperCall(value->AsCall()) : nullptr;
    }

    GenTree* GetArrayLengthFromNewHelperCall(GenTreeCall* call)
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

    static GenTree* GetSsaValue(GenTreeLclUse* use)
    {
        for (unsigned i = 0; i < SsaChaseLimit; i++)
        {
            GenTree* value = use->GetDef()->GetValue();

            if (!value->IsLclUse())
            {
                return value;
            }

            use = value->AsLclUse();
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

            if (GenTreeLclUse* use = nullCheck->GetAddr()->IsLclUse())
            {
                nullCheckMap.Remove(use->GetDef());
            }

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
            madeChanges = true;
        }

        if (indir->OperIs(GT_NULLCHECK) && indir->AsIndir()->GetAddr()->IsLclUse())
        {
            nullCheckMap.Set(indir->AsIndir()->GetAddr()->AsLclUse()->GetDef(), indir->AsIndir(),
                             DefNullCheckMap::SetKind::Overwrite);
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

        GenTreeLclUse* addrUse = addr->IsLclUse();

        if (addrUse == nullptr)
        {
            return nullptr;
        }

        GenTreeLclDef* addrDef   = addrUse->GetDef();
        GenTreeIndir*  nullCheck = nullptr;

        if (!nullCheckMap.Lookup(addrDef, &nullCheck))
        {
            // We can only check for NULLCHECK exception interference if the
            // NULLCHECK is in the same block as the indir.
            if (currentBlock != addrDef->GetBlock())
            {
                return nullptr;
            }

            GenTree* addrValue = addrDef->GetValue();

            if (!addrValue->OperIs(GT_COMMA))
            {
                return nullptr;
            }

            GenTree* commaOp1 = addrValue->AsOp()->GetOp(0)->SkipComma();
            GenTree* commaOp2 = addrValue->AsOp()->GetOp(1);

            if (!commaOp1->OperIs(GT_NULLCHECK) || !commaOp2->OperIs(GT_ADD))
            {
                return nullptr;
            }

            GenTreeOp*     add       = commaOp2->AsOp();
            GenTreeIntCon* addOffset = add->GetOp(1)->IsIntCon();

            if (addOffset == nullptr)
            {
                return nullptr;
            }

            nullCheck = commaOp1->AsIndir();

            GenTree* nullCheckAddr = nullCheck->GetAddr();
            GenTree* addBase       = add->GetOp(0);

            if (nullCheckAddr->OperIs(GT_LCL_VAR))
            {
                if (!addBase->OperIs(GT_LCL_VAR) ||
                    (addBase->AsLclVar()->GetLclNum() != nullCheckAddr->AsLclVar()->GetLclNum()))
                {
                    return nullptr;
                }
            }
            else if (GenTreeLclUse* use = nullCheckAddr->IsLclUse())
            {
                if (!addBase->IsLclUse() || (addBase->AsLclUse()->GetDef() != use->GetDef()))
                {
                    return nullptr;
                }
            }
            else
            {
                return nullptr;
            }

            offset += offset + addOffset->GetValue();
        }

        return compiler->fgIsBigOffset(offset) ? nullptr : nullCheck;
    }

    bool IsNullCheckFoldingLegal(GenTree* nullCheck, GenTree* indir, GenTree** nullCheckUser, Statement** nullCheckStmt)
    {
        assert(nullCheck->OperIs(GT_NULLCHECK));
        assert(indir->OperIsIndirOrArrLength());

        // Usually we can ignore assignments to any locals that are not address exposed,
        // but in try regions we also need to ignore assignments to locals that are live
        // in exception handlers.
        bool isInsideTry = currentBlock->hasTryIndex();

        const unsigned maxNodesWalked = 50;
        unsigned       nodesWalked    = 0;

        Statement* indirStatement         = currentStatement;
        GenTree*   nullCheckStatementRoot = nullCheck;

        // TODO-MIKE-Review: Could we simply remove null checks from the map as we traverse
        // the block and entirely avoid this interference check?

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

            if ((nodesWalked++ > maxNodesWalked) ||
                (node->HasAnySideEffect(GTF_SIDE_EFFECT) && !CanMoveNullCheckPastNode(node, isInsideTry)))
            {
                JITDUMP("Cannot move NULLCHECK [%06u] past node [%06u].\n", nullCheck->GetID(), node->GetID());
                return false;
            }

            nullCheckStatementRoot = node;
        }

        for (GenTree* node = indir->gtPrev; node != nullptr; node = node->gtPrev)
        {
            if ((nodesWalked++ > maxNodesWalked) ||
                (node->HasAnySideEffect(GTF_SIDE_EFFECT) && !CanMoveNullCheckPastNode(node, isInsideTry)))
            {
                JITDUMP("Cannot move NULLCHECK [%06u] past node [%06u].\n", nullCheck->GetID(), node->GetID());
                return false;
            }
        }

        Statement* stmt = indirStatement->GetPrevStmt();

        for (; stmt->GetRootNode() != nullCheckStatementRoot; stmt = stmt->GetPrevStmt())
        {
            GenTree* root = stmt->GetRootNode();

            if ((nodesWalked++ > maxNodesWalked) ||
                (root->HasAnySideEffect(GTF_SIDE_EFFECT) && !CanMoveNullCheckPastTree(root, isInsideTry)))
            {
                JITDUMP("Cannot move NULLCHECK [%06u] past tree [%06u].\n", nullCheck->GetID(), root->GetID());
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

        if ((node->gtFlags & GTF_ASG) != 0)
        {
            if (node->OperIs(GT_ASG))
            {
                GenTree* dst = node->AsOp()->GetOp(0);

                if (dst->OperIs(GT_LCL_VAR, GT_LCL_FLD))
                {
                    return CanMoveNullCheckPastLclStore(dst->AsLclVarCommon(), isInsideTry);
                }

                return false;
            }

            if (GenTreeLclDef* def = node->IsLclDef())
            {
                return CanMoveNullCheckPastLclDef(def, isInsideTry);
            }

            return !node->OperRequiresAsgFlag();
        }

        return true;
    }

    bool CanMoveNullCheckPastTree(GenTree* node, bool isInsideTry)
    {
        if ((node->gtFlags & (GTF_CALL | GTF_EXCEPT)) != 0)
        {
            return false;
        }

        assert((node->gtFlags & GTF_ASG) != 0);

        if (node->OperIs(GT_ASG))
        {
            if ((node->AsOp()->GetOp(1)->gtFlags & GTF_ASG) != 0)
            {
                return false;
            }

            GenTree* dst = node->AsOp()->GetOp(0);

            if (dst->OperIs(GT_LCL_VAR, GT_LCL_FLD))
            {
                return CanMoveNullCheckPastLclStore(dst->AsLclVarCommon(), isInsideTry);
            }

            return false;
        }

        if (GenTreeLclDef* def = node->IsLclDef())
        {
            if ((def->GetValue()->gtFlags & GTF_ASG) != 0)
            {
                return false;
            }

            return CanMoveNullCheckPastLclDef(def, isInsideTry);
        }

        return false;
    }

    bool CanMoveNullCheckPastLclStore(GenTreeLclVarCommon* store, bool isInsideTry)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(store);

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
        if (lcl->lvIsImplicitByRefArgTemp)
        {
            // Implicit-by-ref arg temps are address exposed but they're only
            // "used" by a subsequent call so we can completely ignore them.
            return true;
        }
#endif

        if (lcl->IsAddressExposed())
        {
            return false;
        }

        return !isInsideTry || !lcl->lvHasEHUses;
    }

    bool CanMoveNullCheckPastLclDef(GenTreeLclDef* def, bool isInsideTry)
    {
        return !isInsideTry || !compiler->lvaGetDesc(def->GetLclNum())->lvHasEHUses;
    }
};

PhaseStatus SsaOptimizer::DoEarlyProp()
{
    EarlyProp prop(compiler);
    return prop.Run() ? PhaseStatus::MODIFIED_EVERYTHING : PhaseStatus::MODIFIED_NOTHING;
}
