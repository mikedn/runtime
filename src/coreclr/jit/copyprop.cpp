// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "ssabuilder.h"
#include "ssarenamestate.h"
#include "valuenum.h"

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
//   - `this` can be special cased: usually there are no stores to it and we can
//     consider it to be always live as far as copy propagation is concerned. If
//     we never replace its uses with a copy we're more or less guaranteed that we
//     will instead replace all uses of its copies and remove the copies so live
//     range extension is unlikely to be an issue for `this`. And since it has a
//     single SSA definition it is also not subject to SSA pruning issues.
//     Note that he importer avoids `this` stores but some JIT transforms may
//     introduce `this` stores (e.g. the tail call to loop transform) so we do
//     need to check if `this` is actually always live.
//     It's rare for developer written code to contain `this` copies but the C#
//     compiler seems to introduce such copies for `lock (this)`.
//
// TODO-MIKE-Review: The special casing of `this` is inherited from old code that was
// actually broken. But the corrected code doesn't have much to do with `this`, it's
// likely fine to do this for any local with a single SSA def. But of course, this
// may again turn out to not be so great for CQ due to live range extension issues.
// Maybe it could work well for parameters and other locals that have the single SSA
// def at the start of the first block?

class CopyPropDomTreeVisitor : public DomTreeVisitor<CopyPropDomTreeVisitor>
{
    using LclSsaStackMap = JitHashTable<unsigned, JitSmallPrimitiveKeyFuncs<unsigned>, SsaDefStack>;

    SsaOptimizer&  ssa;
    LclSsaStackMap lclSsaStackMap;
    SsaDefStack*   stackListTail = nullptr;
    SsaDefStack    freeStack{nullptr};
    LclVarDsc*     thisParamLcl  = nullptr;
    unsigned       copyPropCount = 0;

    template <class... Args>
    SsaDefStackNode* AllocStackNode(Args&&... args)
    {
        SsaDefStackNode* stack = freeStack.Top();

        if (stack != nullptr)
        {
            freeStack.Pop();
        }
        else
        {
            stack = m_compiler->getAllocator(CMK_CopyProp).allocate<SsaDefStackNode>(1);
        }

        return new (stack) SsaDefStackNode(std::forward<Args>(args)...);
    }

    bool IsThisParam(LclVarDsc* lcl) const
    {
        return thisParamLcl == lcl;
    }

    void PushSsaDef(LclVarDsc* lcl, BasicBlock* block, GenTreeLclDef* def)
    {
        SsaDefStack*     stack = lclSsaStackMap.Emplace(lcl->GetLclNum(), nullptr);
        SsaDefStackNode* top   = stack->Top();

        if ((top == nullptr) || (top->block != block))
        {
            stack->Push(AllocStackNode(stackListTail, block, def));
            // Append the stack to the stack list. The stack list allows PopBlockSsaDefs
            // to easily find stacks that need popping.
            stackListTail = stack;
        }
        else
        {
            // If we already have a stack node for this block then simply update
            // update the def, the previous one is no longer needed.
            top->SetDef(def);
        }
    }

#ifdef DEBUG
    void DumpLiveSsaDefs() const
    {
        printf("{ ");
        const char* prefix = "";
        for (const auto& pair : lclSsaStackMap)
        {
            if (GenTreeLclDef* def = pair.value.Top()->lclDef)
            {
                printf("%sV%02u [%06u] " FMT_VN, prefix, pair.key, def->GetID(), def->GetConservativeVN());
                prefix = ", ";
            }
        }
        printf(" }\n");
    }
#endif

    void PopBlockSsaDefs(BasicBlock* block)
    {
        while ((stackListTail != nullptr) && (stackListTail->Top()->block == block))
        {
            SsaDefStackNode* top = stackListTail->Pop();
            stackListTail        = top->listPrev;
            freeStack.Push(top);
        }
    }

public:
    CopyPropDomTreeVisitor(SsaOptimizer& ssa)
        : DomTreeVisitor(ssa.GetCompiler(), ssa.GetDomTree())
        , ssa(ssa)
        , lclSsaStackMap(ssa.GetCompiler()->getAllocator(CMK_CopyProp))
    {
    }

    unsigned GetCopyPropCount() const
    {
        return copyPropCount;
    }

    void Begin()
    {
        for (GenTreeLclDef* def = ssa.GetInitLclDefs(); def != nullptr; def = static_cast<GenTreeLclDef*>(def->gtNext))
        {
            LclVarDsc* lcl = def->GetLcl();

            if (lcl->GetLclNum() == m_compiler->info.GetThisParamLclNum())
            {
                thisParamLcl = m_compiler->lvaGetDesc(m_compiler->info.GetThisParamLclNum());
            }

            PushSsaDef(lcl, m_compiler->fgFirstBB, def);
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

        for (Statement* stmt : block->Statements())
        {
            GenTree* root = stmt->GetRootNode();

            if (!root->IsPhiDef())
            {
                break;
            }

            PushSsaDef(root->AsLclDef()->GetLcl(), block, root->AsLclDef());
        }

        for (Statement* stmt : block->NonPhiStatements())
        {
            for (GenTree* node : stmt->Nodes())
            {
                if (GenTreeLclDef* def = node->IsLclDef())
                {
                    PushSsaDef(def->GetLcl(), block, def);
                }
                else if (GenTreeLclUse* use = node->IsLclUse())
                {
                    LclVarDsc* lcl = use->GetDef()->GetLcl();

                    // `this` param normally has no explicit definitions so we prefer to keep using it.
                    // The importer introduces a temp if the IL attempts to modify the `this` param so
                    // we can get defs only if the JIT itself introduces new defs, which is a rare case
                    // (tail call to loop conversion does this). This is only a heuristic so we simply
                    // assume that there are no defs.
                    if (IsThisParam(lcl))
                    {
                        continue;
                    }

                    if ((node->gtFlags & GTF_VAR_DEATH) != 0)
                    {
                        // We push a "fake" def for VAR_DEATH, to prevent live range extension.
                        // For STRUCT locals live range extension isn't an issue, as they're
                        // currently not enregistered nor is any stack packing done.

                        if (!use->TypeIs(TYP_STRUCT))
                        {
                            PushSsaDef(lcl, block, nullptr);
                        }
                    }

                    // TODO-MIKE-SSA: Ignore EXTRACT/INSERTs for now, old code ignored LCL_FLDs.
                    if ((node->gtNext != nullptr) &&
                        ((node->gtNext->IsExtract() && (node->gtNext->AsExtract()->GetStructValue() == node)) ||
                         (node->gtNext->IsInsert() && (node->gtNext->AsInsert()->GetStructValue() == node))))
                    {
                        continue;
                    }

                    // TODO-Review: EH successor/predecessor iteration seems broken.
                    if ((block->bbCatchTyp != BBCT_FINALLY) && (block->bbCatchTyp != BBCT_FAULT))
                    {
                        CopyProp(block, use);
                    }
                }
            }
        }
    }

    void CopyProp(BasicBlock* block, GenTreeLclUse* use)
    {
        if (use->GetConservativeVN() == NoVN)
        {
            return;
        }

        ValueNum   defVN  = use->GetDef()->GetConservativeVN();
        LclVarDsc* lcl    = use->GetDef()->GetLcl();
        unsigned   lclNum = lcl->GetLclNum();

        // TODO-MIKE-Review: The whole "search the SSA defs for a def with matching VN" approach
        // is rather shoddy, partly due to it being just a linear search (and funnily enough, in
        // a hashtable) and partly due it stopping at the first match. Ideally it would look for
        // all matches and pick the best. But what exactly is "best" in this case? Something that
        // avoids live range extension issues and ping pong copy issues. But it doesn't look like
        // that can be done with readily available information and in a cheap manner.
        // Oh well, this approach to copy propagation seems to be doomed.

        for (const auto& pair : lclSsaStackMap)
        {
            LclVarDsc*     newLcl = m_compiler->lvaGetDesc(pair.key);
            GenTreeLclDef* newDef = pair.value.Top()->lclDef;

            if ((lcl == newLcl) || (newDef == nullptr))
            {
                continue;
            }

            if (varActualType(newLcl->GetType()) != varActualType(lcl->GetType()))
            {
                continue;
            }

            if (newDef->GetConservativeVN() != defVN)
            {
                continue;
            }

            if (!lcl->lvDoNotEnregister && newLcl->lvDoNotEnregister)
            {
                continue;
            }

            if (!lcl->lvHasEHRefs && newLcl->lvHasEHRefs)
            {
                continue;
            }

#ifdef TARGET_X86
            // TODO-MIKE-CQ: This avoids replacing a DOUBLE local with a parameter because such parameters
            // aren't 8 byte aligned on x86. It's not clear how useful is this. In terms of code size this
            // turns out to make things worse and the cost of unaligned access is usually small on modern
            // CPUs. And the overall approach is dubious anyway since such parameters can be enregistered.
            // If spilling is needed then it would make more sense to just spill into a local slot instead
            // of the parameter's home. See also local copy assertion propagation and optAddCopies.
            if (lcl->TypeIs(TYP_DOUBLE) && !lcl->IsParam() && newLcl->IsParam())
            {
                continue;
            }
#endif

            if ((m_compiler->gsLclShadowMap != nullptr) && newLcl->IsParam() &&
                (m_compiler->gsLclShadowMap[newLcl->GetLclNum()] == lclNum))
            {
                continue;
            }

            if (!IsThisParam(newLcl) && (pair.value.Top()->block != block) &&
                !VarSetOps::IsMember(m_compiler, block->bbLiveIn, newLcl->GetLivenessBitIndex()))
            {
                continue;
            }

            // TODO-MIKE-Fix: VN ZeroMap is untyped so we risk replacing a use of a local
            // having type A with a use of a local having type B. Usually this does not
            // matter (the end result is a 0 initialized struct value) but in the case of
            // INSERT we can end swapping the struct and field operands.
            if (lcl->TypeIs(TYP_STRUCT) && (lcl->GetLayout()->GetSize() != newLcl->GetLayout()->GetSize()))
            {
                continue;
            }

            JITDUMP("[%06u] replacing V%02u def [%06u] by V%02u def [%06u]\n", use->GetID(),
                    use->GetDef()->GetLcl()->GetLclNum(), use->GetDef()->GetID(), newLcl->GetLclNum(), newDef->GetID());

            use->GetDef()->RemoveUse(use);
            newDef->AddUse(use);
            copyPropCount++;

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

PhaseStatus SsaOptimizer::DoCopyProp()
{
    CopyPropDomTreeVisitor visitor(*this);
    visitor.WalkTree();
    return visitor.GetCopyPropCount() != 0 ? PhaseStatus::MODIFIED_EVERYTHING : PhaseStatus::MODIFIED_NOTHING;
}
