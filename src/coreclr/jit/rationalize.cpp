// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

// return op that is the store equivalent of the given load opcode
genTreeOps storeForm(genTreeOps loadForm)
{
    switch (loadForm)
    {
        case GT_LCL_VAR:
            return GT_STORE_LCL_VAR;
        case GT_LCL_FLD:
            return GT_STORE_LCL_FLD;
        default:
            noway_assert(!"not a data load opcode\n");
            unreached();
    }
}

// return op that is the addr equivalent of the given load opcode
genTreeOps addrForm(genTreeOps loadForm)
{
    switch (loadForm)
    {
        case GT_LCL_VAR:
            return GT_LCL_VAR_ADDR;
        case GT_LCL_FLD:
            return GT_LCL_FLD_ADDR;
        default:
            noway_assert(!"not a data load opcode\n");
            unreached();
    }
}

// copy the flags determined by mask from src to dst
void copyFlags(GenTree* dst, GenTree* src, GenTreeFlags mask)
{
    dst->gtFlags &= ~mask;
    dst->gtFlags |= (src->gtFlags & mask);
}

// RewriteIndir: Rewrite an indirection and clear the flags that should not be set after rationalize.
//
// Arguments:
//    use - A use of an indirection node.
//
void Rationalizer::RewriteIndir(LIR::Use& use)
{
    GenTreeIndir* indir = use.Def()->AsIndir();
    assert(indir->OperIs(GT_IND, GT_BLK, GT_OBJ));

    // Clear the `GTF_IND_ASG_LHS` flag, which overlaps with `GTF_IND_REQ_ADDR_IN_REG`.
    indir->gtFlags &= ~GTF_IND_ASG_LHS;

    if (indir->OperIs(GT_OBJ))
    {
        assert(varTypeIsStruct(indir->GetType()));

        if (varTypeIsSIMD(indir->GetType()))
        {
            indir->SetOper(GT_IND);
        }
    }
}

// RewriteNodeAsCall : Replace the given tree node by a GT_CALL.
//
// Arguments:
//    ppTree      - A pointer-to-a-pointer for the tree node
//    fgWalkData  - A pointer to tree walk data providing the context
//    callHnd     - The method handle of the call to be generated
//    entryPoint  - The method entrypoint of the call to be generated
//    args        - The argument list of the call to be generated
//
// Return Value:
//    None.
//

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

    // Create the call node
    GenTreeCall* call = comp->gtNewCallNode(CT_USER_FUNC, callHnd, tree->gtType, args);

#if DEBUG
    CORINFO_SIG_INFO sig;
    comp->eeGetMethodSig(callHnd, &sig);
    assert(JITtype2varType(sig.retType) == tree->gtType);
#endif // DEBUG

#ifdef FEATURE_READYTORUN_COMPILER
    call->AsCall()->setEntryPoint(entryPoint);
#endif

    call = comp->fgMorphArgs(call);

    // Replace "tree" with "call"
    if (parents.Height() > 1)
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
    for (int i = 1; i < parents.Height(); i++)
    {
        parents.Top(i)->gtFlags |= (call->gtFlags & GTF_ALL_EFFECT) | GTF_CALL;
    }

    // Since "tree" is replaced with "call", pop "tree" node (i.e the current node)
    // and replace it with "call" on parent stack.
    assert(parents.Top() == tree);
    (void)parents.Pop();
    parents.Push(call);
}

// RewriteIntrinsicAsUserCall : Rewrite an intrinsic operator as a GT_CALL to the original method.
//
// Arguments:
//    ppTree      - A pointer-to-a-pointer for the intrinsic node
//    fgWalkData  - A pointer to tree walk data providing the context
//
// Return Value:
//    None.
//
// Some intrinsics, such as operation Sqrt, are rewritten back to calls, and some are not.
// The ones that are not being rewritten here must be handled in Codegen.
// Conceptually, the lower is the right place to do the rewrite. Keeping it in rationalization is
// mainly for throughput issue.

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

#ifdef DEBUG

void Rationalizer::ValidateStatement(Statement* stmt, BasicBlock* block)
{
    DBEXEC(TRUE, JitTls::GetCompiler()->fgDebugCheckNodeLinks(block, stmt));
}

// sanity checks that apply to all kinds of IR
void Rationalizer::SanityCheck()
{
    // TODO: assert(!IsLIR());
    BasicBlock* block;
    foreach_block(comp, block)
    {
        for (Statement* statement : block->Statements())
        {
            ValidateStatement(statement, block);

            for (GenTree* tree = statement->GetTreeList(); tree; tree = tree->gtNext)
            {
                // QMARK nodes should have been removed before this phase.
                assert(!tree->OperIs(GT_QMARK));

                if (tree->OperGet() == GT_ASG)
                {
                    if (tree->gtGetOp1()->OperGet() == GT_LCL_VAR)
                    {
                        assert(tree->gtGetOp1()->gtFlags & GTF_VAR_DEF);
                    }
                    else if (tree->gtGetOp2()->OperGet() == GT_LCL_VAR)
                    {
                        assert(!(tree->gtGetOp2()->gtFlags & GTF_VAR_DEF));
                    }
                }
            }
        }
    }
}

void Rationalizer::SanityCheckRational()
{
    // TODO-Cleanup : check that the tree is rational here
    // then do normal checks
    SanityCheck();
}

#endif // DEBUG

static void RewriteAssignmentIntoStoreLclCore(GenTreeOp* assignment,
                                              GenTree*   location,
                                              GenTree*   value,
                                              genTreeOps locationOp)
{
    assert(assignment != nullptr);
    assert(assignment->OperGet() == GT_ASG);
    assert(location != nullptr);
    assert(value != nullptr);

    genTreeOps storeOp = storeForm(locationOp);

    JITDUMP("rewriting asg(%s, X) to %s(X)\n", GenTree::OpName(locationOp), GenTree::OpName(storeOp));

    assignment->ChangeOper(storeOp);
    GenTreeLclVarCommon* store = assignment->AsLclVarCommon();

    GenTreeLclVarCommon* var = location->AsLclVarCommon();
    store->SetLclNum(var->GetLclNum());
    store->SetSsaNum(var->GetSsaNum());

    if (locationOp == GT_LCL_FLD)
    {
        store->AsLclFld()->SetLclOffs(var->AsLclFld()->GetLclOffs());
        store->AsLclFld()->SetFieldSeq(var->AsLclFld()->GetFieldSeq());
        store->AsLclFld()->SetLayoutNum(var->AsLclFld()->GetLayoutNum());
    }

    copyFlags(store, var, (GTF_LIVENESS_MASK | GTF_VAR_MULTIREG));
    store->gtFlags &= ~GTF_REVERSE_OPS;

    store->gtType = var->TypeGet();
    store->gtOp1  = value;

    DISPNODE(store);
    JITDUMP("\n");
}

void Rationalizer::RewriteAssignment(LIR::Use& use)
{
    assert(use.IsInitialized());

    GenTreeOp* assignment = use.Def()->AsOp();
    assert(assignment->OperGet() == GT_ASG);

    GenTree* location = assignment->gtGetOp1();
    GenTree* value    = assignment->gtGetOp2();

    switch (location->GetOper())
    {
        case GT_LCL_VAR:
        case GT_LCL_FLD:
        case GT_PHI_ARG:
            RewriteAssignmentIntoStoreLclCore(assignment, location, value, location->GetOper());
            BlockRange().Remove(location);
            break;

        case GT_IND:
        {
            GenTreeStoreInd* store =
                new (comp, GT_STOREIND) GenTreeStoreInd(location->TypeGet(), location->gtGetOp1(), value);

            copyFlags(store, assignment, GTF_ALL_EFFECT);
            copyFlags(store, location, GTF_IND_FLAGS);

            // TODO: JIT dump

            // Remove the GT_IND node and replace the assignment node with the store
            BlockRange().Remove(location);
            BlockRange().InsertBefore(assignment, store);
            use.ReplaceWith(comp, store);
            BlockRange().Remove(assignment);
        }
        break;

        case GT_CLS_VAR:
        {
            location->SetOper(GT_CLS_VAR_ADDR);
            location->gtType = TYP_BYREF;

            assignment->SetOper(GT_STOREIND);
            assignment->AsStoreInd()->SetRMWStatusDefault();

            // TODO: JIT dump
        }
        break;

        case GT_BLK:
        case GT_OBJ:
        case GT_DYN_BLK:
        {
            assert(varTypeIsStruct(location));
            GenTreeBlk* storeBlk = location->AsBlk();
            genTreeOps  storeOper;
            switch (location->gtOper)
            {
                case GT_BLK:
                    storeOper = GT_STORE_BLK;
                    break;
                case GT_OBJ:
                    storeOper = GT_STORE_OBJ;
                    break;
                case GT_DYN_BLK:
                    storeOper                             = GT_STORE_DYN_BLK;
                    storeBlk->AsDynBlk()->gtEvalSizeFirst = false;
                    break;
                default:
                    unreached();
            }
            JITDUMP("Rewriting GT_ASG(%s(X), Y) to %s(X,Y):\n", GenTree::OpName(location->gtOper),
                    GenTree::OpName(storeOper));
            storeBlk->SetOperRaw(storeOper);
            storeBlk->gtFlags &= ~GTF_DONT_CSE;
            storeBlk->gtFlags |= (assignment->gtFlags & (GTF_ALL_EFFECT | GTF_DONT_CSE));
            storeBlk->AsBlk()->Data() = value;

            // Remove the block node from its current position and replace the assignment node with it
            // (now in its store form).
            BlockRange().Remove(storeBlk);
            BlockRange().InsertBefore(assignment, storeBlk);
            use.ReplaceWith(comp, storeBlk);
            BlockRange().Remove(assignment);
            DISPTREERANGE(BlockRange(), use.Def());
            JITDUMP("\n");
        }
        break;

        default:
            unreached();
            break;
    }
}

void Rationalizer::RewriteAddress(LIR::Use& use)
{
    assert(use.IsInitialized());

    GenTreeUnOp* address = use.Def()->AsUnOp();
    assert(address->OperGet() == GT_ADDR);

    GenTree*   location   = address->gtGetOp1();
    genTreeOps locationOp = location->OperGet();

    assert(!location->OperIsHWIntrinsic());

    if (location->IsLocal())
    {
// We are changing the child from GT_LCL_VAR TO GT_LCL_VAR_ADDR.
// Therefore gtType of the child needs to be changed to a TYP_BYREF
#ifdef DEBUG
        if (locationOp == GT_LCL_VAR)
        {
            JITDUMP("Rewriting GT_ADDR(GT_LCL_VAR) to GT_LCL_VAR_ADDR:\n");
        }
        else
        {
            assert(locationOp == GT_LCL_FLD);
            JITDUMP("Rewriting GT_ADDR(GT_LCL_FLD) to GT_LCL_FLD_ADDR:\n");
        }
#endif // DEBUG

        location->SetOper(addrForm(locationOp));
        location->gtType = TYP_BYREF;
        copyFlags(location, address, GTF_ALL_EFFECT);

        use.ReplaceWith(comp, location);
        BlockRange().Remove(address);
    }
    else if (locationOp == GT_CLS_VAR)
    {
        location->SetOper(GT_CLS_VAR_ADDR);
        location->gtType = TYP_BYREF;
        copyFlags(location, address, GTF_ALL_EFFECT);

        use.ReplaceWith(comp, location);
        BlockRange().Remove(address);

        JITDUMP("Rewriting GT_ADDR(GT_CLS_VAR) to GT_CLS_VAR_ADDR:\n");
    }
    else if (location->OperIsIndir())
    {
        use.ReplaceWith(comp, location->gtGetOp1());
        BlockRange().Remove(location);
        BlockRange().Remove(address);

        JITDUMP("Rewriting GT_ADDR(GT_IND(X)) to X:\n");
    }

    DISPTREERANGE(BlockRange(), use.Def());
    JITDUMP("\n");
}

Compiler::fgWalkResult Rationalizer::RewriteNode(GenTree** useEdge, Compiler::GenTreeStack& parentStack)
{
    assert(useEdge != nullptr);

    GenTree* node = *useEdge;
    assert(node != nullptr);

#ifdef DEBUG
    const bool isLateArg = (node->gtFlags & GTF_LATE_ARG) != 0;
#endif

    // Now clear the REVERSE_OPS flag on the current node.
    node->gtFlags &= ~GTF_REVERSE_OPS;

    LIR::Use use;
    if (parentStack.Height() < 2)
    {
        use = LIR::Use::GetDummyUse(BlockRange(), *useEdge);
    }
    else
    {
        use = LIR::Use(BlockRange(), useEdge, parentStack.Top(1));
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

        case GT_ADDR:
            RewriteAddress(use);
            break;

        case GT_IND:
        case GT_BLK:
        case GT_OBJ:
            RewriteIndir(use);
            break;

        case GT_NOP:
            // fgMorph sometimes inserts NOP nodes between defs and uses
            // supposedly 'to prevent constant folding'. In this case, remove the
            // NOP.
            if (node->gtGetOp1() != nullptr)
            {
                use.ReplaceWith(comp, node->gtGetOp1());
                BlockRange().Remove(node);
                node = node->gtGetOp1();
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

            GenTree* replacement = node->gtGetOp2();
            if (!use.IsDummyUse())
            {
                use.ReplaceWith(comp, replacement);
                node = replacement;
            }
            else
            {
                // This is a top-level comma. If the RHS has no side effects we can remove
                // it as well.
                bool               isClosed    = false;
                unsigned           sideEffects = 0;
                LIR::ReadOnlyRange rhsRange    = BlockRange().GetTreeRange(replacement, &isClosed, &sideEffects);

                if ((sideEffects & GTF_ALL_EFFECT) == 0)
                {
                    // None of the transforms performed herein violate tree order, so isClosed
                    // should always be true.
                    assert(isClosed);

                    BlockRange().Delete(comp, m_block, std::move(rhsRange));
                }
                else
                {
                    node = replacement;
                }
            }
        }
        break;

        case GT_ARGPLACE:
            // Remove argplace and list nodes from the execution order.
            //
            // TODO: remove phi args and phi nodes as well?
            BlockRange().Remove(node);
            break;

#if defined(TARGET_XARCH) || defined(TARGET_ARM)
        case GT_CLS_VAR:
        {
            // Class vars that are the target of an assignment will get rewritten into
            // GT_STOREIND(GT_CLS_VAR_ADDR, val) by RewriteAssignment. This check is
            // not strictly necessary--the GT_IND(GT_CLS_VAR_ADDR) pattern that would
            // otherwise be generated would also be picked up by RewriteAssignment--but
            // skipping the rewrite here saves an allocation and a bit of extra work.
            const bool isLHSOfAssignment = (use.User()->OperGet() == GT_ASG) && (use.User()->gtGetOp1() == node);
            if (!isLHSOfAssignment)
            {
                GenTree* ind = comp->gtNewOperNode(GT_IND, node->TypeGet(), node);

                node->SetOper(GT_CLS_VAR_ADDR);
                node->gtType = TYP_BYREF;

                BlockRange().InsertAfter(node, ind);
                use.ReplaceWith(comp, ind);

                // TODO: JIT dump
            }
        }
        break;
#endif // TARGET_XARCH

        case GT_INTRINSIC:
            // Non-target intrinsics should have already been rewritten back into user calls.
            assert(comp->IsTargetIntrinsic(node->AsIntrinsic()->gtIntrinsicName));
            break;

        default:
            // These nodes should not be present in HIR.
            assert(!node->OperIs(GT_CMP, GT_SETCC, GT_JCC, GT_JCMP, GT_LOCKADD, GT_INSTR));
            break;
    }

    // Do some extra processing on top-level nodes to remove unused local reads.
    if (node->OperIsLocalRead())
    {
        if (use.IsDummyUse())
        {
            BlockRange().Remove(node);
        }
        else
        {
            // Local reads are side-effect-free; clear any flags leftover from frontend transformations.
            node->gtFlags &= ~GTF_ALL_EFFECT;
        }
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

        if (node->TypeGet() == TYP_LONG)
        {
            comp->compLongUsed = true;
        }
    }

    assert(isLateArg == ((use.Def()->gtFlags & GTF_LATE_ARG) != 0));

    return Compiler::WALK_CONTINUE;
}

//------------------------------------------------------------------------
// DoPhase: Run the rationalize over the method IR.
//
// Returns:
//    PhaseStatus indicating, what, if anything, was modified
//
PhaseStatus Rationalizer::DoPhase()
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
            if (node->OperGet() == GT_INTRINSIC &&
                m_rationalizer.comp->IsIntrinsicImplementedByUserCall(node->AsIntrinsic()->gtIntrinsicName))
            {
                m_rationalizer.RewriteIntrinsicAsUserCall(use, this->m_ancestors);
            }

            return Compiler::WALK_CONTINUE;
        }

        // Rewrite HIR nodes into LIR nodes.
        fgWalkResult PostOrderVisit(GenTree** use, GenTree* user)
        {
            return m_rationalizer.RewriteNode(use, this->m_ancestors);
        }
    };

    DBEXEC(TRUE, SanityCheck());

    comp->compCurBB = nullptr;
    comp->fgOrder   = Compiler::FGOrderLinear;

    RationalizeVisitor visitor(*this);
    for (BasicBlock* block = comp->fgFirstBB; block != nullptr; block = block->bbNext)
    {
        comp->compCurBB = block;
        m_block         = block;

        block->MakeLIR(nullptr, nullptr);

        // Establish the first and last nodes for the block. This is necessary in order for the LIR
        // utilities that hang off the BasicBlock type to work correctly.
        Statement* firstStatement = block->firstStmt();
        if (firstStatement == nullptr)
        {
            // No statements in this block; skip it.
            continue;
        }

        for (Statement* statement : StatementList(firstStatement))
        {
            assert(statement->GetTreeList() != nullptr);
            assert(statement->GetTreeList()->gtPrev == nullptr);
            assert(statement->GetRootNode() != nullptr);
            assert(statement->GetRootNode()->gtNext == nullptr);

            BlockRange().InsertAtEnd(LIR::Range(statement->GetTreeList(), statement->GetRootNode()));

            // If this statement has correct offset information, change it into an IL offset
            // node and insert it into the LIR.
            if (statement->GetILOffsetX() != BAD_IL_OFFSET)
            {
                assert(!statement->IsPhiDefnStmt());
                GenTreeILOffset* ilOffset = new (comp, GT_IL_OFFSET)
                    GenTreeILOffset(statement->GetILOffsetX() DEBUGARG(statement->GetLastILOffset()));
                BlockRange().InsertBefore(statement->GetTreeList(), ilOffset);
            }

            m_block = block;
            visitor.WalkTree(statement->GetRootNodePointer(), nullptr);
        }

        block->bbStmtList = nullptr;

        assert(BlockRange().CheckLIR(comp, true));
    }

    comp->compRationalIRForm = true;

    return PhaseStatus::MODIFIED_EVERYTHING;
}
