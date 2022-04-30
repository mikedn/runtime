// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "ssabuilder.h"

// VN based copy propagation
//
// This stage performs value numbering based copy propagation. Since copy propagation
// is about data flow, we cannot find them in assertion prop phase. In assertion prop
// we can identify copies, like so: if (a == b) else, i.e., control flow assertions.
//
// To identify data flow copies, we'll follow a similar approach to SSA renaming.
// We would walk each path in the graph keeping track of every live definition. Thus
// when we see a variable that shares the VN with a live definition, we'd replace this
// variable with the variable in the live definition, if suitable.
//
// Due to limited SSA support this suffers from various constraints and issues:
//   - We build pruned SSA and lack any real SSA repair facility thus care needs
//     to be taken to not introduce new uses of a local in places where PHIs have
//     not been added due to the local being dead.
//   - The backend does not support the transformed SSA form so again, we can't
//     introduce new uses anywhere we might need them, that would require proper
//     SSA destruction which is too missing from the JIT.
//   - These restrictions cause another problem - given A = B we'd simply replace
//     all uses of A with B but given the restrictions we might not be able to do
//     that. This means that A = B won't actually be removed and worse, we may end
//     up extending the live range of B, which may be concurrent with that of A
//     and increase register pressure.

class CopyPropDomTreeVisitor : public DomTreeVisitor<CopyPropDomTreeVisitor>
{
    class LivenessUpdater
    {
        Compiler* compiler;
        VARSET_TP liveSet;
        INDEBUG(VARSET_TP scratchSet;)

    public:
        LivenessUpdater::LivenessUpdater(Compiler* compiler)
            : compiler(compiler)
            , liveSet(VarSetOps::MakeEmpty(compiler))
#ifdef DEBUG
            , scratchSet(VarSetOps::MakeEmpty(compiler))
#endif
        {
        }

        void BeginBlock(BasicBlock* block)
        {
            VarSetOps::Assign(compiler, liveSet, block->bbLiveIn);
        }

        VARSET_VALARG_TP GetLiveSet() const
        {
            return liveSet;
        }

        void UpdateDef(GenTreeLclVarCommon* lclNode)
        {
            assert(lclNode->OperIs(GT_LCL_VAR, GT_LCL_FLD) && ((lclNode->gtFlags & GTF_VAR_DEF) != 0));

            LclVarDsc* lcl = compiler->lvaGetDesc(lclNode);

            assert(lcl->IsInSsa());

            bool isBorn  = (lclNode->gtFlags & GTF_VAR_USEASG) == 0;
            bool isDying = (lclNode->gtFlags & GTF_VAR_DEATH) != 0;

            if (isBorn || isDying)
            {
                DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet, liveSet));

                if (isDying)
                {
                    VarSetOps::RemoveElemD(compiler, liveSet, lcl->GetLivenessBitIndex());
                }
                else
                {
                    VarSetOps::AddElemD(compiler, liveSet, lcl->GetLivenessBitIndex());
                }

                DBEXEC(compiler->verbose, compiler->dmpVarSetDiff("Live vars: ", scratchSet, liveSet);)
            }
        }

        void UpdateUse(GenTreeLclVarCommon* lclNode)
        {
            assert(lclNode->OperIs(GT_LCL_VAR, GT_LCL_FLD) && ((lclNode->gtFlags & GTF_VAR_DEF) == 0));

            LclVarDsc* lcl = compiler->lvaGetDesc(lclNode);

            assert(lcl->IsInSsa());

            if ((lclNode->gtFlags & GTF_VAR_DEATH) != 0)
            {
                DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet, liveSet));
                VarSetOps::RemoveElemD(compiler, liveSet, lcl->GetLivenessBitIndex());
                DBEXEC(compiler->verbose, compiler->dmpVarSetDiff("Live vars: ", scratchSet, liveSet);)
            }
        }
    };

    using SsaStack     = SsaRenameState::Stack;
    using SsaStackNode = SsaRenameState::StackNode;

    typedef JitHashTable<unsigned, JitSmallPrimitiveKeyFuncs<unsigned>, SsaStack> LclSsaStackMap;

    LclSsaStackMap  lclSsaStackMap;
    LivenessUpdater liveness;
    SsaStack*       stackListTail = nullptr;
    SsaStack        freeStack;

    template <class... Args>
    SsaStackNode* AllocStackNode(Args&&... args)
    {
        SsaStackNode* stack = freeStack.Top();

        if (stack != nullptr)
        {
            freeStack.Pop();
        }
        else
        {
            stack = m_compiler->getAllocator(CMK_CopyProp).allocate<SsaStackNode>(1);
        }

        return new (stack) SsaStackNode(std::forward<Args>(args)...);
    }

public:
    CopyPropDomTreeVisitor(Compiler* compiler)
        : DomTreeVisitor(compiler, compiler->fgSsaDomTree)
        , lclSsaStackMap(compiler->getAllocator(CMK_CopyProp))
        , liveness(compiler)
    {
    }

    void PushSsaDef(SsaStack* stack, BasicBlock* block, unsigned ssaNum)
    {
        SsaStackNode* top = stack->Top();

        if ((top == nullptr) || (top->m_block != block))
        {
            stack->Push(AllocStackNode(stackListTail, block, ssaNum));
            // Append the stack to the stack list. The stack list allows PopBlockSsaDefs
            // to easily find stacks that need popping.
            stackListTail = stack;
        }
        else
        {
            // If we already have a stack node for this block then simply update
            // update the SSA number, the previous one is no longer needed.
            top->m_ssaNum = ssaNum;
        }
    }

#ifdef DEBUG
    void DumpLiveSsaDefs()
    {
        printf("{ ");
        const char* prefix = "";
        for (const auto& pair : lclSsaStackMap)
        {
            unsigned lclNum = pair.key;
            unsigned ssaNum = pair.value.Top()->m_ssaNum;
            printf("%sV%02u:%u " FMT_VN, prefix, lclNum, ssaNum,
                   m_compiler->lvaGetDesc(lclNum)->GetPerSsaData(ssaNum)->GetConservativeVN());
            prefix = ", ";
        }
        printf(" }\n");
    }
#endif

    void PopBlockSsaDefs(BasicBlock* block)
    {
        while ((stackListTail != nullptr) && (stackListTail->Top()->m_block == block))
        {
            SsaStackNode* top = stackListTail->Pop();
            stackListTail     = top->m_listPrev;
            freeStack.Push(top);
        }
    }

    void Begin()
    {
        for (unsigned lclNum = 0; lclNum < m_compiler->lvaCount; lclNum++)
        {
            LclVarDsc* lcl = m_compiler->lvaGetDesc(lclNum);

            if (lcl->HasImplicitSsaDef())
            {
                PushSsaDef(lclSsaStackMap.Emplace(lclNum), m_compiler->fgFirstBB, SsaConfig::FIRST_SSA_NUM);
            }
        }
    }

    void PreOrderVisit(BasicBlock* block)
    {
#ifdef DEBUG
        if (m_compiler->verbose)
        {
            printf(FMT_BB " entry SSA defs: ", block->bbNum);
            DumpLiveSsaDefs();
        }
#endif

        liveness.BeginBlock(block);

        for (Statement* const stmt : block->Statements())
        {
            for (GenTree* const node : stmt->Nodes())
            {
                if (!node->OperIs(GT_LCL_VAR, GT_LCL_FLD))
                {
                    continue;
                }

                GenTreeLclVarCommon* lclNode = node->AsLclVarCommon();
                unsigned             lclNum  = lclNode->GetLclNum();
                LclVarDsc*           lcl     = m_compiler->lvaGetDesc(lclNum);

                if (!lcl->IsInSsa())
                {
                    continue;
                }

                if ((lclNode->gtFlags & GTF_VAR_DEF) != 0)
                {
                    liveness.UpdateDef(lclNode);
                    PushSsaDef(lclSsaStackMap.Emplace(lclNum), block, m_compiler->GetSsaDefNum(lclNode));

                    continue;
                }

                liveness.UpdateUse(lclNode);

                if (lclNode->OperIs(GT_LCL_VAR))
                {
                    // TODO-Review: EH successor/predecessor iteration seems broken.
                    if ((block->bbCatchTyp != BBCT_FINALLY) && (block->bbCatchTyp != BBCT_FAULT))
                    {
                        CopyProp(lclNode->AsLclVar());
                    }
                }
            }
        }
    }

    int GetCopyPropScore(LclVarDsc* lclVarDsc, LclVarDsc* copyVarDsc)
    {
        int score = 1;

        if (lclVarDsc->lvVolatileHint)
        {
            score += 4;
        }

        if (copyVarDsc->lvVolatileHint)
        {
            score -= 4;
        }

#ifdef TARGET_X86
        // For doubles we also prefer to change parameters into non-parameter local variables
        if (lclVarDsc->lvType == TYP_DOUBLE)
        {
            if (lclVarDsc->lvIsParam)
            {
                score += 2;
            }

            if (copyVarDsc->lvIsParam)
            {
                score -= 2;
            }
        }
#endif

        return score;
    }

    void CopyProp(GenTreeLclVar* use)
    {
        assert(use->OperIs(GT_LCL_VAR) && ((use->gtFlags & GTF_VAR_DEF) == 0));

        if (use->GetConservativeVN() == ValueNumStore::NoVN)
        {
            return;
        }

        unsigned   lclNum = use->GetLclNum();
        LclVarDsc* lcl    = m_compiler->lvaGetDesc(lclNum);

        for (const auto& pair : lclSsaStackMap)
        {
            unsigned newLclNum = pair.key;

            // Nothing to do if same.
            if (lclNum == newLclNum)
            {
                continue;
            }

            LclVarDsc* newLcl = m_compiler->lvaGetDesc(newLclNum);

            if (varActualType(newLcl->GetType()) != varActualType(lcl->GetType()))
            {
                continue;
            }

            if (!lcl->lvDoNotEnregister && newLcl->lvDoNotEnregister)
            {
                continue;
            }

            if ((m_compiler->gsShadowVarInfo != nullptr) && newLcl->IsParam() &&
                (m_compiler->gsShadowVarInfo[newLclNum].shadowLclNum == lclNum))
            {
                continue;
            }

            unsigned newSsaNum = pair.value.Top()->m_ssaNum;

            // The use must produce the same value number if we substitute the def.
            if (m_compiler->vnLocalLoad(use, newLcl, newSsaNum).GetConservative() != use->GetConservativeVN())
            {
                continue;
            }

            if (GetCopyPropScore(lcl, newLcl) <= 0)
            {
                continue;
            }

            // The new local should be live to use it, we might need to add a PHI that got pruned
            // during initial SSA construction and we don't have the means to do that here. Also,
            // we don't know if all uses of the old local will disappear the extended live range
            // of the new local may very well interfere with the live range of the old local and
            // increase register pressure.
            //
            // Make an exception if the new local is `this` (the real one, not a copy). There are
            // no stores to `this` so there aren't any PHIs, pruned or not, and we can add a new
            // use anywhere in the method. It's rare for developer written code to contain `this`
            // copies but the C# compiler seem to introduce such copies for `lock (this)`.
            //
            // TODO-MIKE-Review: The special casing of `this` is inherited from old code that was
            // actually broken. But the corrected code doesn't have much to do with `this`, it's
            // likely fine to do this for any local with a single SSA def. But of course, this
            // may again turn out to not be so great for CQ due to live range extension issues.
            // Maybe it could work well for parameters and other locals that have the single SSA
            // def at the start of the first block?
            //
            // TODO-MIKE-Cleanup: Well, `this` should really have only one def. Except that loop
            // tail call morphing is messy and introduces unnecessary defs...

            if ((newLclNum == m_compiler->info.compThisArg) && (newLcl->lvPerSsaData.GetCount() == 1))
            {
                assert(newLcl->HasImplicitSsaDef());
            }
            else if (!VarSetOps::IsMember(m_compiler, liveness.GetLiveSet(), newLcl->GetLivenessBitIndex()))
            {
                continue;
            }

            JITDUMP("[%06u] replacing V%02u:%u by V%02u:%u\n", use->GetID(), use->GetLclNum(), use->GetSsaNum(),
                    newLclNum, newSsaNum);

            use->SetLclNum(newLclNum);
            use->SetSsaNum(newSsaNum);

            break;
        }
    }

    void PostOrderVisit(BasicBlock* block)
    {
        PopBlockSsaDefs(block);

        for (auto iter = lclSsaStackMap.begin(); iter != lclSsaStackMap.end();)
        {
            if (iter.GetValue().Top() == nullptr)
            {
                unsigned lclNum = iter.GetKey();
                ++iter;
                lclSsaStackMap.Remove(lclNum);
            }
            else
            {
                ++iter;
            }
        }
    }
};

void Compiler::optVnCopyProp()
{
    assert(ssaForm && (vnStore != nullptr));

    CopyPropDomTreeVisitor visitor(this);
    visitor.WalkTree();
}
