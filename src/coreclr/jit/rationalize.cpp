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
        INDEBUG(comp->fgStmtLinksTraversed = 0;)
    }

    void Run();

private:
    inline LIR::Range& BlockRange() const
    {
        return LIR::AsRange(m_block);
    }

    void RewriteNodeAsCall(GenTree**             use,
                           CORINFO_METHOD_HANDLE callHnd,
#ifdef FEATURE_READYTORUN_COMPILER
                           CORINFO_CONST_LOOKUP entryPoint,
#endif
                           GenTreeCall::Use* args);

    void RewriteIntrinsicAsUserCall(GenTree** use);
    GenTreeWalkResult RewriteNode(GenTree** useEdge, GenTree* user);
};

void Rationalizer::RewriteNodeAsCall(GenTree**             use,
                                     CORINFO_METHOD_HANDLE callHnd,
#ifdef FEATURE_READYTORUN_COMPILER
                                     CORINFO_CONST_LOOKUP entryPoint,
#endif
                                     GenTreeCall::Use* args)
{
    GenTree* const tree           = *use;
    GenTree* const treeFirstNode  = comp->gtGetFirstNode(tree);
    GenTree* const insertionPoint = treeFirstNode->gtPrev;

    assert(tree->HasAnySideEffect(GTF_CALL));

#if DEBUG
    CORINFO_SIG_INFO sig;
    comp->eeGetMethodSig(callHnd, &sig);
    assert(CorTypeToVarType(sig.retType) == tree->GetType());
#endif

    BlockRange().Remove(treeFirstNode, tree);

    GenTreeCall* call = comp->gtNewUserCallNode(callHnd, tree->GetType(), args);
#ifdef FEATURE_READYTORUN_COMPILER
    call->SetR2REntryPoint(entryPoint);
#endif
    comp->fgMorphBlock = m_block;
    comp->fgInitArgInfo(call);
    comp->fgSetupArgs(call);

    *use = call;

    BlockRange().InsertAfter(insertionPoint, LIR::Range(comp->gtSetTreeSeq(call), call));
}

// Rewrite an intrinsic operator as a GT_CALL to the original method.
// Some intrinsics, such as operation Sqrt, are rewritten back to calls, and some are not.
// The ones that are not being rewritten here must be handled in Codegen.
// Conceptually, the lower is the right place to do the rewrite.
// Keeping it in rationalization is mainly for throughput issue.
void Rationalizer::RewriteIntrinsicAsUserCall(GenTree** use)
{
    GenTreeIntrinsic* intrinsic = (*use)->AsIntrinsic();
    GenTreeCall::Use* args;

    if (intrinsic->AsOp()->gtOp2 == nullptr)
    {
        args = comp->gtNewCallArgs(intrinsic->GetOp(0));
    }
    else
    {
        args = comp->gtNewCallArgs(intrinsic->GetOp(0), intrinsic->GetOp(1));
    }

    RewriteNodeAsCall(use, intrinsic->GetMethodHandle(),
#ifdef FEATURE_READYTORUN_COMPILER
                      intrinsic->GetR2REntryPoint(),
#endif
                      args);
}

GenTreeWalkResult Rationalizer::RewriteNode(GenTree** useEdge, GenTree* user)
{
    assert(useEdge != nullptr);

    GenTree* node = *useEdge;
    assert(node != nullptr);

    if (node->TypeIs(TYP_BOOL))
    {
        node->SetType(TYP_UBYTE);
    }

    node->SetReverseOps(false);

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
    switch (node->GetOper())
    {
        case GT_BOX:
            // GT_BOX at this level just passes through so get rid of it
            use.SetDef(node->AsBox()->GetOp(0));
            BlockRange().Unlink(node);
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

            node->ChangeOper(GT_IND_LOAD);
            node->AsIndir()->SetAddr(addr);
            goto IND;
        }

        case GT_IND_LOAD_OBJ:
            if (varTypeIsSIMD(node->GetType()))
            {
                node->SetOper(GT_IND_LOAD);
            }
            FALLTHROUGH;
        case GT_IND_LOAD:
        case GT_IND_LOAD_BLK:
        IND:
            // Remove side effects that may have been inherited from address.
            node->RemoveSideEffects(GTF_ASG);

            if ((node->gtFlags & GTF_IND_NONFAULTING) != 0)
            {
                node->RemoveSideEffects(GTF_EXCEPT);
            }
            break;

#ifndef TARGET_ARM64
        case GT_CLS_VAR_ADDR:
            assert(!comp->opts.compReloc);
            {
                INDEBUG(FieldSeqNode* fieldSeq = node->AsClsVar()->GetFieldSeq());

                GenTreeIntCon* intCon = node->ChangeToIntCon(node->AsClsVar()->GetFieldAddr(), HandleKind::Static);
#ifdef DEBUG
                intCon->SetDumpHandle(fieldSeq->GetFieldHandle());
                intCon->SetFieldSeq(fieldSeq);
#endif
            }
            break;
#endif // TARGET_ARM64

        case GT_NOP:
            // fgMorph sometimes inserts NOP nodes between defs and uses supposedly
            // 'to prevent constant folding'. In this case, remove the NOP.
            if (GenTree* value = node->gtGetOp1())
            {
                if (!use.IsDummyUse())
                {
                    use.SetDef(value);
                }
                else
                {
                    value->SetUnusedValue();
                }

                BlockRange().Unlink(node);

                return GenTreeWalkResult::Continue;
            }
            break;

        case GT_COMMA:
        {
            GenTree* sideEffects = node->AsOp()->GetOp(0);

            if (!sideEffects->HasAnySideEffect(GTF_SIDE_EFFECT))
            {
                BlockRange().RemoveDeadTree(sideEffects);
            }
            else if (sideEffects->IsValue())
            {
                sideEffects->SetUnusedValue();
            }

            BlockRange().Unlink(node);

            GenTree* value = node->AsOp()->GetOp(1);

            if (!use.IsDummyUse())
            {
                use.SetDef(node->AsOp()->GetOp(1));
            }
            else if (!value->HasAnySideEffect(GTF_SIDE_EFFECT))
            {
                BlockRange().RemoveDeadTree(value);
            }
            else if (value->IsValue())
            {
                value->SetUnusedValue();
            }

            return GenTreeWalkResult::Continue;
        }

        case GT_CALL:
        {
            GenTreeCall* call = node->AsCall();
            CallInfo*    info = call->GetInfo();

            for (unsigned i = 0, argCount = info->GetArgCount(); i < argCount; i++)
            {
                CallArgInfo* argInfo = info->GetArgInfo(i);

                if (GenTreeCall::Use* lateUse = argInfo->GetLateUse())
                {
                    argInfo->use->NodeRef() = nullptr;
                    argInfo->RemoveLateUse();
                }
            }

            GenTreeCall::Use** prevUseLink = &call->m_uses;

            for (GenTreeCall::Use& use : call->Uses())
            {
                if (use.NodeRef() == nullptr)
                {
                    *prevUseLink = use.GetNext();
                }
                else
                {
                    prevUseLink = &use.NextRef();
                }
            }

            break;
        }

        case GT_INTRINSIC:
            // Non-target intrinsics should have already been rewritten back into user calls.
            assert(!node->AsIntrinsic()->IsUserCall());
            break;

        case GT_DIV:
        case GT_UDIV:
        case GT_MOD:
        case GT_UMOD:
            node->RemoveSideEffects(GTF_ALL_EFFECT & ~GTF_EXCEPT);
            break;

        case GT_ADD:
        case GT_SUB:
        case GT_MUL:
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
        case GT_FTRUNC:
        case GT_FXT:
        case GT_SXT:
        case GT_UXT:
        case GT_STOF:
        case GT_UTOF:
        case GT_FTOS:
        case GT_FTOU:
        case GT_TRUNC:
        case GT_CONV:
        case GT_RETURN:
        case GT_JTRUE:
        case GT_SWITCH:
            node->SetSideEffects(GTF_NONE);
            break;

        case GT_OVF_U:
        case GT_OVF_TRUNC:
        case GT_OVF_STRUNC:
        case GT_OVF_UTRUNC:
        case GT_OVF_SCONV:
        case GT_OVF_UCONV:
        case GT_OVF_FTOS:
        case GT_OVF_FTOU:
        case GT_OVF_SADD:
        case GT_OVF_UADD:
        case GT_OVF_SSUB:
        case GT_OVF_USUB:
        case GT_OVF_SMUL:
        case GT_OVF_UMUL:
            node->SetSideEffects(GTF_EXCEPT);
            break;

        default:
            // These nodes should not be present before rationalization.
            assert(!node->OperIs(GT_CMP, GT_SETCC, GT_JCC, GT_LOCKADD, GT_INSTR));
#ifdef TARGET_ARM64
            assert(!node->OperIs(GT_JCMP));
#endif
            break;
    }

    // Do some extra processing on top-level nodes to remove unused local reads.
    if (node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
    {
        if (use.IsDummyUse())
        {
            BlockRange().Unlink(node);
            return GenTreeWalkResult::Continue;
        }

        // Local reads are side-effect-free; clear any flags leftover from frontend transformations.
        node->SetSideEffects(GTF_NONE);

#ifndef TARGET_64BIT
        if (node->TypeIs(TYP_LONG) ||
            // We may end up with INT LCL_VAR nodes for LONG locals, we should
            // treat them as LONG in case we want to promote the LONG local.
            (node->TypeIs(TYP_INT) && node->OperIs(GT_LCL_LOAD) && node->AsLclLoad()->GetLcl()->TypeIs(TYP_LONG)))
        {
            comp->compLongUsed = true;
        }
#endif
    }
    else
    {
        // TODO-MIKE-Review: Is this missing HWINTRINSIC stores?
        if (!node->OperIsStore())
        {
            // Clear the GTF_ASG flag for all nodes but stores
            node->RemoveSideEffects(GTF_ASG);
        }
        else if (node->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD))
        {
            // Local stores may have inherited GTF_EXCEPT from the value tree.
            node->RemoveSideEffects(GTF_EXCEPT);
        }

        if (!node->IsCall())
        {
            node->RemoveSideEffects(GTF_CALL);
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

    return GenTreeWalkResult::Continue;
}

void Rationalizer::Run()
{
    class RationalizeVisitor final : public GenTreeVisitor<RationalizeVisitor>
    {
        Rationalizer& m_rationalizer;

    public:
        enum
        {
            DoPreOrder        = true,
            DoPostOrder       = true,
            UseExecutionOrder = true,
        };

        RationalizeVisitor(Rationalizer& rationalizer) : m_rationalizer(rationalizer)
        {
        }

        // Rewrite intrinsics that are not supported by the target back into user calls.
        // This needs to be done before the transition to LIR because it relies on the use
        // of fgSetupArgs, which is designed to operate on HIR. Once this is done for a
        // particular statement, link that statement's nodes into the current basic block.
        GenTreeWalkResult PreOrderVisit(GenTree** use, GenTree* user)
        {
            GenTree* const node = *use;

            if (GenTreeIntrinsic* intrinsic = node->IsIntrinsic())
            {
                if (intrinsic->IsUserCall())
                {
                    m_rationalizer.RewriteIntrinsicAsUserCall(use);
                }
            }

            return GenTreeWalkResult::Continue;
        }

        // Rewrite HIR nodes into LIR nodes.
        GenTreeWalkResult PostOrderVisit(GenTree** use, GenTree* user)
        {
            return m_rationalizer.RewriteNode(use, user);
        }
    };

    INDEBUG(comp->fgLinearOrder = true);

    RationalizeVisitor visitor(*this);
    for (BasicBlock* const block : comp->Blocks())
    {
        m_block = block;

        block->MakeLIR();

        IL_OFFSETX currentILOffset = BAD_IL_OFFSET;

        for (Statement* const statement : block->Statements())
        {
            assert(statement->GetNodeList() != nullptr);
            assert(statement->GetNodeList()->gtPrev == nullptr);
            assert(statement->GetRootNode() != nullptr);
            assert(statement->GetRootNode()->gtNext == nullptr);
            assert(!statement->GetRootNode()->IsPhiDef());

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
