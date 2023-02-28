// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

class Rationalizer
{
    Compiler*   comp;
    BasicBlock* m_block;
    Statement*  m_statement;

public:
    Rationalizer(Compiler* comp) : comp(comp)
    {
        INDEBUG(comp->compNumStatementLinksTraversed = 0;)
    }

    void Run();

private:
    inline LIR::Range& BlockRange() const
    {
        return LIR::AsRange(m_block);
    }

    void RewriteNodeAsCall(GenTree**             use,
                           ArrayStack<GenTree*>& parents,
                           CORINFO_METHOD_HANDLE callHnd,
#ifdef FEATURE_READYTORUN_COMPILER
                           CORINFO_CONST_LOOKUP entryPoint,
#endif
                           GenTreeCall::Use* args);

    void RewriteIntrinsicAsUserCall(GenTree** use, ArrayStack<GenTree*>& parents);
    void RewriteAssignment(LIR::Use& use);
    void RewriteLocalAssignment(GenTreeOp* assignment, GenTreeLclVarCommon* location);

    Compiler::fgWalkResult RewriteNode(GenTree** useEdge, GenTree* user);
};

void Rationalizer::RewriteNodeAsCall(GenTree**             use,
                                     ArrayStack<GenTree*>& parents,
                                     CORINFO_METHOD_HANDLE callHnd,
#ifdef FEATURE_READYTORUN_COMPILER
                                     CORINFO_CONST_LOOKUP entryPoint,
#endif
                                     GenTreeCall::Use* args)
{
    GenTree* const tree           = *use;
    GenTree* const treeFirstNode  = comp->fgGetFirstNode(tree);
    GenTree* const insertionPoint = treeFirstNode->gtPrev;

    BlockRange().Remove(treeFirstNode, tree);

    GenTreeCall* call = comp->gtNewUserCallNode(callHnd, tree->GetType(), args);

#if DEBUG
    CORINFO_SIG_INFO sig;
    comp->eeGetMethodSig(callHnd, &sig);
    assert(JITtype2varType(sig.retType) == tree->gtType);
#endif

#ifdef FEATURE_READYTORUN_COMPILER
    call->setEntryPoint(entryPoint);
#endif

    comp->fgMorphBlock = m_block;
    comp->fgMorphArgs(call);

    // Replace "tree" with "call"
    if (parents.Size() > 1)
    {
        parents.Top(1)->ReplaceOperand(use, call);
    }
    else
    {
        // If there's no parent, the tree being replaced is the root of the
        // statement (and no special handling is necessary).
        *use = call;
    }

    comp->gtSetEvalOrder(call);
    BlockRange().InsertAfter(insertionPoint, LIR::Range(comp->fgSetTreeSeq(call), call));

    // Propagate flags of "call" to its parents.
    // 0 is current node, so start at 1
    for (unsigned i = 1; i < parents.Size(); i++)
    {
        parents.Top(i)->gtFlags |= (call->gtFlags & GTF_ALL_EFFECT) | GTF_CALL;
    }

    // Since "tree" is replaced with "call", pop "tree" node (i.e the current node)
    // and replace it with "call" on parent stack.
    assert(parents.Top() == tree);
    (void)parents.Pop();
    parents.Push(call);
}

// Rewrite an intrinsic operator as a GT_CALL to the original method.
// Some intrinsics, such as operation Sqrt, are rewritten back to calls, and some are not.
// The ones that are not being rewritten here must be handled in Codegen.
// Conceptually, the lower is the right place to do the rewrite.
// Keeping it in rationalization is mainly for throughput issue.
void Rationalizer::RewriteIntrinsicAsUserCall(GenTree** use, ArrayStack<GenTree*>& parents)
{
    GenTreeIntrinsic* intrinsic = (*use)->AsIntrinsic();

    GenTreeCall::Use* args;
    if (intrinsic->AsOp()->gtOp2 == nullptr)
    {
        args = comp->gtNewCallArgs(intrinsic->gtGetOp1());
    }
    else
    {
        args = comp->gtNewCallArgs(intrinsic->gtGetOp1(), intrinsic->gtGetOp2());
    }

    RewriteNodeAsCall(use, parents, intrinsic->gtMethodHandle,
#ifdef FEATURE_READYTORUN_COMPILER
                      intrinsic->gtEntryPoint,
#endif
                      args);
}

void Rationalizer::RewriteLocalAssignment(GenTreeOp* assignment, GenTreeLclVarCommon* location)
{
    assert(assignment->OperIs(GT_ASG));
    assert(location->OperIs(GT_LCL_VAR, GT_LCL_FLD));

    GenTree* value = assignment->GetOp(1);

    assignment->ChangeOper(location->OperIs(GT_LCL_VAR) ? GT_STORE_LCL_VAR : GT_STORE_LCL_FLD);

    GenTreeLclVarCommon* store = assignment->AsLclVarCommon();
    store->SetType(location->GetType());
    store->SetOp(0, value);
    store->SetLclNum(location->GetLclNum());
    store->SetSsaNum(location->GetSsaNum());

    if (store->OperIs(GT_STORE_LCL_FLD))
    {
        store->AsLclFld()->SetLclOffs(location->AsLclFld()->GetLclOffs());
        store->AsLclFld()->SetFieldSeq(location->AsLclFld()->GetFieldSeq());
        store->AsLclFld()->SetLayoutNum(location->AsLclFld()->GetLayoutNum());
        store->gtFlags |= location->gtFlags & GTF_VAR_USEASG;
    }

    store->gtFlags |= GTF_VAR_DEF;
    store->gtFlags &= ~GTF_EXCEPT;

    // We don't use SSA in LIR but being able to still display SSA use/defs might be useful.
    INDEBUG(comp->MoveSsaDefNum(location, store));
}

void Rationalizer::RewriteAssignment(LIR::Use& use)
{
    assert(use.IsInitialized());

    GenTreeOp* assignment = use.Def()->AsOp();
    assert(assignment->OperIs(GT_ASG));
    assert((assignment->gtFlags & GTF_ASG) != 0);

    GenTree* location = assignment->GetOp(0);
    GenTree* value    = assignment->GetOp(1);

    switch (location->GetOper())
    {
        case GT_LCL_VAR:
        case GT_LCL_FLD:
            RewriteLocalAssignment(assignment, location->AsLclVarCommon());
            break;

        case GT_IND:
            assignment->ChangeOper(GT_STOREIND);
            assignment->SetType(location->GetType());
            assignment->AsIndir()->SetAddr(location->AsIndir()->GetAddr());
            assignment->AsIndir()->SetValue(value);
            assignment->gtFlags |= location->gtFlags & GTF_IND_FLAGS;
            break;

        case GT_BLK:
        case GT_OBJ:
            assignment->ChangeOper(location->OperIs(GT_BLK) ? GT_STORE_BLK : GT_STORE_OBJ);
            assignment->SetType(location->GetType());
            assignment->AsBlk()->SetLayout(location->AsBlk()->GetLayout());
            assignment->AsBlk()->SetKind(StructStoreKind::Invalid);

            assignment->AsIndir()->SetAddr(location->AsIndir()->GetAddr());
            assignment->AsIndir()->SetValue(value);
            assignment->gtFlags |= location->gtFlags & GTF_IND_FLAGS;
            break;

        default:
            unreached();
    }

    BlockRange().Remove(location);
}

Compiler::fgWalkResult Rationalizer::RewriteNode(GenTree** useEdge, GenTree* user)
{
    assert(useEdge != nullptr);

    GenTree* node = *useEdge;
    assert(node != nullptr);

    // Now clear the REVERSE_OPS flag on the current node.
    node->gtFlags &= ~GTF_REVERSE_OPS;

    LIR::Use use;
    if (user == nullptr)
    {
        use = LIR::Use::GetDummyUse(BlockRange(), *useEdge);
    }
    else
    {
        use = LIR::Use(BlockRange(), useEdge, user);
    }

    assert(node == use.Def());
    switch (node->OperGet())
    {
        case GT_ASG:
            RewriteAssignment(use);
            break;

        case GT_BOX:
            // GT_BOX at this level just passes through so get rid of it
            use.ReplaceWith(comp, node->gtGetOp1());
            BlockRange().Remove(node);
            break;

        case GT_ARR_LENGTH:
        {
            GenTree* array  = node->AsArrLen()->GetArray();
            unsigned offset = node->AsArrLen()->GetLenOffs();
            GenTree* addr;

            if (array->IsIntegralConst(0))
            {
                // If the array is NULL, then we should get a NULL reference
                // exception when computing its length.  We need to maintain
                // an invariant where there is no sum of two constants node,
                // so let's simply return an indirection of NULL. Also change
                // the address to I_IMPL, there's no reason to keep the REF.

                addr = array;
                addr->SetType(TYP_I_IMPL);
            }
            else
            {
                GenTree* intCon = comp->gtNewIconNode(offset, TYP_I_IMPL);
                addr            = comp->gtNewOperNode(GT_ADD, TYP_BYREF, array, intCon);

                BlockRange().InsertAfter(array, intCon, addr);
            }

            node->ChangeOper(GT_IND);
            node->AsIndir()->SetAddr(addr);
            goto IND;
        }

        case GT_OBJ:
            if (varTypeIsSIMD(node->GetType()))
            {
                node->SetOper(GT_IND);
            }
            FALLTHROUGH;
        case GT_IND:
        case GT_BLK:
        IND:
            // Remove side effects that may have been inherited from address.
            node->gtFlags &= ~GTF_ASG;

            if ((node->gtFlags & GTF_IND_NONFAULTING) != 0)
            {
                node->gtFlags &= ~GTF_EXCEPT;
            }
            break;

        case GT_NOP:
            // fgMorph sometimes inserts NOP nodes between defs and uses supposedly
            // 'to prevent constant folding'. In this case, remove the NOP.
            if (GenTree* value = node->gtGetOp1())
            {
                if (!use.IsDummyUse())
                {
                    use.ReplaceWith(comp, value);
                }
                else
                {
                    value->SetUnusedValue();
                }

                BlockRange().Remove(node);

                return Compiler::WALK_CONTINUE;
            }
            break;

        case GT_COMMA:
        {
            GenTree*           op1         = node->gtGetOp1();
            bool               isClosed    = false;
            unsigned           sideEffects = 0;
            LIR::ReadOnlyRange lhsRange    = BlockRange().GetTreeRange(op1, &isClosed, &sideEffects);

            if ((sideEffects & GTF_ALL_EFFECT) == 0)
            {
                // The LHS has no side effects. Remove it.
                // None of the transforms performed herein violate tree order, so isClosed
                // should always be true.
                assert(isClosed);

                BlockRange().Delete(comp, m_block, std::move(lhsRange));
            }
            else if (op1->IsValue())
            {
                op1->SetUnusedValue();
            }

            BlockRange().Remove(node);

            GenTree* value = node->gtGetOp2();
            if (!use.IsDummyUse())
            {
                use.ReplaceWith(comp, value);
            }
            else
            {
                // This is a top-level comma. If the RHS has no side effects we can remove
                // it as well.
                bool               isClosed    = false;
                unsigned           sideEffects = 0;
                LIR::ReadOnlyRange rhsRange    = BlockRange().GetTreeRange(value, &isClosed, &sideEffects);

                if ((sideEffects & GTF_ALL_EFFECT) == 0)
                {
                    // None of the transforms performed herein violate tree order, so isClosed
                    // should always be true.
                    assert(isClosed);

                    BlockRange().Delete(comp, m_block, std::move(rhsRange));
                }
                else if (value->IsValue())
                {
                    value->SetUnusedValue();
                }
            }

            return Compiler::WALK_CONTINUE;
        }
        break;

        case GT_ARGPLACE:
            // Remove argplace and list nodes from the execution order.
            //
            // TODO: remove phi args and phi nodes as well?
            BlockRange().Remove(node);
            break;

        case GT_INTRINSIC:
            // Non-target intrinsics should have already been rewritten back into user calls.
            assert(comp->IsTargetIntrinsic(node->AsIntrinsic()->GetIntrinsic()));
            break;

        case GT_ADD:
        case GT_SUB:
        case GT_MUL:
        case GT_CAST:
            // Remove side effects that may have been inherited from operands.
            if (!node->gtOverflow())
            {
                node->SetSideEffects(GTF_EMPTY);
                break;
            }
            FALLTHROUGH;
        case GT_DIV:
        case GT_UDIV:
        case GT_MOD:
        case GT_UMOD:
            node->SetSideEffects(node->GetSideEffects() & GTF_EXCEPT);
            break;

        case GT_AND:
        case GT_OR:
        case GT_XOR:
        case GT_NOT:
        case GT_NEG:
        case GT_BITCAST:
        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:
        case GT_ROL:
        case GT_ROR:
        case GT_BSWAP:
        case GT_BSWAP16:
        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GT:
        case GT_GE:
        case GT_FADD:
        case GT_FSUB:
        case GT_FMUL:
        case GT_FDIV:
        case GT_FNEG:
            node->SetSideEffects(GTF_EMPTY);
            break;

        default:
            // These nodes should not be present before rationalization.
            assert(!node->OperIs(GT_CMP, GT_SETCC, GT_JCC, GT_JCMP, GT_LOCKADD, GT_INSTR));
            break;
    }

    // Do some extra processing on top-level nodes to remove unused local reads.
    if (node->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        if (use.IsDummyUse())
        {
            BlockRange().Remove(node);
            return Compiler::WALK_CONTINUE;
        }

        // Local reads are side-effect-free; clear any flags leftover from frontend transformations.
        node->SetSideEffects(GTF_EMPTY);

#ifndef TARGET_64BIT
        if (node->TypeIs(TYP_LONG) ||
            // We may end up with INT LCL_VAR nodes for LONG locals, we should
            // treat them as LONG in case we want to promote the LONG local.
            (node->TypeIs(TYP_INT) && node->OperIs(GT_LCL_VAR) && comp->lvaGetDesc(node->AsLclVar())->TypeIs(TYP_LONG)))
        {
            comp->compLongUsed = true;
        }
#endif
    }
    else
    {
        if (!node->OperIsStore())
        {
            // Clear the GTF_ASG flag for all nodes but stores
            node->gtFlags &= ~GTF_ASG;
        }

        if (!node->IsCall())
        {
            // Clear the GTF_CALL flag for all nodes but calls
            node->gtFlags &= ~GTF_CALL;
        }

        if (node->IsValue() && use.IsDummyUse())
        {
            node->SetUnusedValue();
        }

#ifndef TARGET_64BIT
        if (node->TypeIs(TYP_LONG))
        {
            comp->compLongUsed = true;
        }
#endif
    }

    return Compiler::WALK_CONTINUE;
}

void Rationalizer::Run()
{
    class RationalizeVisitor final : public GenTreeVisitor<RationalizeVisitor>
    {
        Rationalizer& m_rationalizer;

    public:
        enum
        {
            ComputeStack      = true,
            DoPreOrder        = true,
            DoPostOrder       = true,
            UseExecutionOrder = true,
        };

        RationalizeVisitor(Rationalizer& rationalizer)
            : GenTreeVisitor<RationalizeVisitor>(rationalizer.comp), m_rationalizer(rationalizer)
        {
        }

        // Rewrite intrinsics that are not supported by the target back into user calls.
        // This needs to be done before the transition to LIR because it relies on the use
        // of fgMorphArgs, which is designed to operate on HIR. Once this is done for a
        // particular statement, link that statement's nodes into the current basic block.
        fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
        {
            GenTree* const node = *use;
            if (node->IsIntrinsic() &&
                m_rationalizer.comp->IsIntrinsicImplementedByUserCall(node->AsIntrinsic()->GetIntrinsic()))
            {
                m_rationalizer.RewriteIntrinsicAsUserCall(use, this->m_ancestors);
            }

            return Compiler::WALK_CONTINUE;
        }

        // Rewrite HIR nodes into LIR nodes.
        fgWalkResult PostOrderVisit(GenTree** use, GenTree* user)
        {
            return m_rationalizer.RewriteNode(use, user);
        }
    };

    comp->fgOrder = Compiler::FGOrderLinear;

    RationalizeVisitor visitor(*this);
    for (BasicBlock* const block : comp->Blocks())
    {
        m_block = block;

        block->MakeLIR(nullptr, nullptr);

        // Establish the first and last nodes for the block. This is necessary in order for the LIR
        // utilities that hang off the BasicBlock type to work correctly.
        Statement* firstStatement = block->firstStmt();
        if (firstStatement == nullptr)
        {
            // No statements in this block; skip it.
            continue;
        }

        IL_OFFSETX currentILOffset = BAD_IL_OFFSET;

        for (Statement* const statement : block->Statements())
        {
            assert(statement->GetNodeList() != nullptr);
            assert(statement->GetNodeList()->gtPrev == nullptr);
            assert(statement->GetRootNode() != nullptr);
            assert(statement->GetRootNode()->gtNext == nullptr);

            if (statement->IsPhiDefnStmt())
            {
                continue;
            }

            IL_OFFSETX stmtILOffset = statement->GetILOffsetX();

            if ((stmtILOffset != BAD_IL_OFFSET) && (stmtILOffset != currentILOffset))
            {
                BlockRange().InsertAtEnd(new (comp, GT_IL_OFFSET) GenTreeILOffset(stmtILOffset));
                currentILOffset = stmtILOffset;
            }

            BlockRange().InsertAtEnd(LIR::Range(statement->GetNodeList(), statement->GetRootNode()));

            m_block = block;
            visitor.WalkTree(statement->GetRootNodePointer(), nullptr);
        }

        block->bbStmtList = nullptr;

        assert(BlockRange().CheckLIR(comp, true));
    }

    comp->compRationalIRForm = true;
}

PhaseStatus Compiler::phRationalize()
{
#ifdef DEBUG
    fgDebugCheckLinks(compStressCompile(Compiler::STRESS_REMORPH_TREES, 50));

    for (BasicBlock* block = fgFirstBB; block != nullptr; block = block->bbNext)
    {
        for (Statement* statement : block->Statements())
        {
            fgDebugCheckNodeLinks(block, statement);
        }
    }
#endif

    Rationalizer rationalizer(this);
    rationalizer.Run();

#ifdef DEBUG
    fgDebugCheckBBlist();
    fgDebugCheckLinks();
#endif

    return PhaseStatus::MODIFIED_EVERYTHING;
}
