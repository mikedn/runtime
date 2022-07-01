// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//
//                                    Early Value Propagation
//
// This phase performs an SSA-based value propagation optimization that currently only applies to array
// lengths and explicit null checks. An SSA-based backwards tracking of local variables
// is performed at each point of interest, e.g., an array length reference site, a method table reference site, or
// an indirection.
// The tracking continues until an interesting value is encountered. The value is then used to rewrite
// the source site or the value.
//
///////////////////////////////////////////////////////////////////////////////////////

#include "jitpch.h"
#include "ssabuilder.h"

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

    //------------------------------------------------------------------------------------------
    // DoEarlyProp: The entry point of the early value propagation.
    //
    // Notes:
    //    This phase performs an SSA-based value propagation, including
    //      1. Array length propagation.
    //      2. Runtime type handle propagation.
    //      3. Null check folding.
    //
    //    For array length propagation, a demand-driven SSA-based backwards tracking of constant
    //    array lengths is performed at each array length reference site which is in form of a
    //    GT_ARR_LENGTH node. When a GT_ARR_LENGTH node is seen, the array ref pointer which is
    //    the only child node of the GT_ARR_LENGTH is tracked. This is only done for array ref
    //    pointers that have valid SSA forms.The tracking is along SSA use-def chain and stops
    //    at the original array allocation site where we can grab the array length. The
    //    GT_ARR_LENGTH node will then be rewritten to a GT_CNS_INT node if the array length is
    //    constant.
    //
    //    Similarly, the same algorithm also applies to rewriting a method table (also known as
    //    vtable) reference site which is in form of GT_INDIR node. The base pointer, which is
    //    an object reference pointer, is treated in the same way as an array reference pointer.
    //
    //    Null check folding tries to find GT_INDIR(obj + const) that GT_NULLCHECK(obj) can be folded into
    //    and removed. Currently, the algorithm only matches GT_INDIR and GT_NULLCHECK in the same basic block.

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

            for (Statement* stmt = block->firstStmt(); stmt != nullptr;)
            {
                // Preserve the next link before the propagation and morph.
                Statement* next = stmt->GetNextStmt();

                currentStatement = stmt;

                // Walk the stmt tree in linear order to rewrite any array length reference with a
                // constant array length.
                bool isRewritten = false;
                for (GenTree* tree = stmt->GetTreeList(); tree != nullptr; tree = tree->gtNext)
                {
                    if (!tree->OperIsIndirOrArrLength())
                    {
                        continue;
                    }

                    GenTree* rewrittenTree = PropagateNode(tree);

                    if (rewrittenTree != nullptr)
                    {
                        compiler->gtUpdateTreeAncestorsSideEffects(rewrittenTree);
                        isRewritten = true;
                        tree        = rewrittenTree;
                    }
                }

                // Update the evaluation order and the statement info if the stmt has been rewritten.
                if (isRewritten)
                {
                    compiler->gtSetStmtInfo(stmt);
                    compiler->fgSetStmtSeq(stmt);
                }

                stmt = next;
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

    GenTree* PropagateNode(GenTree* node)
    {
        assert(node->OperIsIndirOrArrLength());

        if (GenTreeArrLen* arrLen = node->IsArrLen())
        {
            GenTree* newNode = PropagateArrayLength(arrLen);

            if (newNode != nullptr)
            {
                return newNode;
            }
        }

        if ((currentBlock->bbFlags & BBF_HAS_NULLCHECK) != 0)
        {
            FoldNullCheck(node);
        }

        return nullptr;
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
        ssize_t constVal = constLen->GetValue();

        if ((constVal < 0) || (constVal > INT32_MAX))
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
                ssize_t checkConstVal = check->gtIndex->AsIntCon()->GetValue();

                if ((checkConstVal >= 0) && (checkConstVal < constVal))
                {
                    GenTree* comma = check->FindUser();

                    // We should never see cases other than these in the IR,
                    // as the check node does not produce a value.
                    assert(((comma != nullptr) && comma->OperIs(GT_COMMA) &&
                            (comma->AsOp()->GetOp(0) == check || comma->TypeIs(TYP_VOID))) ||
                           (check == currentStatement->GetRootNode()));

                    // Still, we guard here so that release builds do not try to optimize trees we don't understand.
                    if (((comma != nullptr) && comma->OperIs(GT_COMMA) && (comma->AsOp()->GetOp(0) == check)) ||
                        (check == currentStatement->GetRootNode()))
                    {
                        // Both `tree` and `check` have been removed from the statement.
                        // 'tree' was replaced with 'nop' or side effect list under 'comma'.
                        // optRemoveRangeCheck returns this modified tree.
                        return compiler->optRemoveRangeCheck(check, comma, currentStatement);
                    }
                }
            }
        }

        JITDUMPTREE(currentStatement->GetRootNode(), "PropagateConstArrayLength rewriting\n");

        GenTree* constLenClone = compiler->gtCloneExpr(constLen);

        if (constLenClone->GetType() != arrLen->GetType())
        {
            assert(constLenClone->TypeIs(TYP_LONG));
            assert(arrLen->TypeIs(TYP_INT));

            constLenClone->SetType(arrLen->GetType());
        }

        // constLenClone has small tree node size, it is safe to use ReplaceWith here.
        arrLen->ReplaceWith(constLenClone, compiler);

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

    // If NULLCHECK node is post-dominated by an indirection node on the same local and the nodes between
    // the NULLCHECK and the indirection don't have unsafe side effects, the NULLCHECK can be removed.
    // The indir will cause a NullReferenceException if and only if GT_NULLCHECK will cause the same
    // NullReferenceException.
    void FoldNullCheck(GenTree* indir)
    {
        GenTree* addr = indir->IsArrLen() ? indir->AsArrLen()->GetArray() : indir->AsIndir()->GetAddr();

        GenTreeIndir* nullCheck     = FindNullCheckToFold(addr);
        GenTree*      nullCheckUser = nullptr;
        Statement*    nullCheckStmt = nullptr;

        if ((nullCheck != nullptr) && IsNullCheckFoldingLegal(nullCheck, indir, &nullCheckUser, &nullCheckStmt))
        {
            JITDUMPTREE(nullCheck, "FoldNullCheck marking a NULLCHECK for removal\n");

            nullCheck->gtFlags |= GTF_IND_NONFAULTING;
            nullCheck->gtFlags &= ~(GTF_EXCEPT | GTF_DONT_CSE);

            // TODO-MIKE-Review: This is dubios, the NULLCHECK node will be removed
            // and then GTF_ORDER_SIDEEFF probably needs to be set on its ancestores.
            nullCheck->gtFlags |= GTF_ORDER_SIDEEFF;

            // TODO-MIKE-Cleanup: This mostly deals with spurious GTF_DONT_CSE added
            // by fgMorphFieldAddr to reduce diffs.
            if (nullCheckUser != nullptr)
            {
                nullCheckUser->gtFlags &= ~GTF_DONT_CSE;
            }

            nullCheckMap.Remove(nullCheck->GetAddr()->AsLclVar()->GetLclNum());

            compiler->fgMorphBlockStmt(currentBlock, nullCheckStmt DEBUGARG("FoldNullCheck"));
        }

        if (indir->OperIs(GT_NULLCHECK) && indir->AsIndir()->GetAddr()->OperIs(GT_LCL_VAR))
        {
            nullCheckMap.Set(indir->AsIndir()->GetAddr()->AsLclVar()->GetLclNum(), indir->AsIndir(),
                             LocalNumberToNullCheckTreeMap::SetKind::Overwrite);
        }
    }

    // Try to find a NULLCHECK node that is post-dominated in the same basic block by
    // an indir that will produce the same exception, making the NULLCHECK redundant.
    //   - NULLCHECK(addr + offset1)
    //     any code that does not interfere with the NULLCHECK thrown exception
    //     IND(addr)
    //
    //   - ASG(x, COMMA(NULLCHECK(y), ADD(y, offset1)))
    //     any code that does not interfere with the NULLCHECK thrown exception
    //     IND(ADD(x, offset2))
    //
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

    // We can eliminate a NULLCHECK only if there are no interfering side effects between
    // the NULLCHECK node and the indir. For example, we can't eliminate the NULLCHECK if
    // there is an assignment to a memory location between it and the indir.
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

    //----------------------------------------------------------------
    // CanMoveNullCheckPastTree: Check if a nullcheck node that is before `tree`
    //                              in execution order may be folded into an indirection node that
    //                              is after `tree` is execution order.
    //
    // Arguments:
    //    tree                  - The tree to check.
    //    isInsideTry           - True if tree is inside try, false otherwise.
    //
    // Return Value:
    //    True if nullcheck may be folded into a node that is after tree in execution order,
    //    false otherwise.

    bool CanMoveNullCheckPastNode(GenTree* node, bool isInsideTry)
    {
        if (((node->gtFlags & GTF_CALL) != 0) && node->OperRequiresCallFlag(compiler))
        {
            return false;
        }

        if (((node->gtFlags & GTF_EXCEPT) != 0) && node->OperMayThrow(compiler))
        {
            return false;
        }

        if ((node->gtFlags & GTF_ASG) == 0)
        {
            return true;
        }

        if (node->OperGet() == GT_ASG)
        {
            GenTree* lhs = node->gtGetOp1();
            GenTree* rhs = node->gtGetOp2();

            if (isInsideTry)
            {
                // Inside try we allow only assignments to locals not live in handlers.
                return lhs->OperIs(GT_LCL_VAR) && !compiler->lvaGetDesc(lhs->AsLclVar())->lvEHLive;
            }
            else
            {
                // We disallow only assignments to global memory.
                return (lhs->gtFlags & GTF_GLOB_REF) == 0;
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

        if (node->OperGet() == GT_ASG)
        {
            GenTree* lhs = node->gtGetOp1();
            GenTree* rhs = node->gtGetOp2();

            if ((rhs->gtFlags & GTF_ASG) != 0)
            {
                return false;
            }

            if (isInsideTry)
            {
                // Inside try we allow only assignments to locals not live in handlers.
                return lhs->OperIs(GT_LCL_VAR) && !compiler->lvaGetDesc(lhs->AsLclVar())->lvEHLive;
            }
            else
            {
                // We disallow only assignments to global memory.
                return (lhs->gtFlags & GTF_GLOB_REF) == 0;
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
