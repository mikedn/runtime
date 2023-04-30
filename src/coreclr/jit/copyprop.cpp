// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "ssarenamestate.h"

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
    using SsaStack     = SsaRenameState::Stack;
    using SsaStackNode = SsaRenameState::StackNode;

    typedef JitHashTable<unsigned, JitSmallPrimitiveKeyFuncs<unsigned>, SsaStack> LclSsaStackMap;

    LclSsaStackMap lclSsaStackMap;
    SsaStack*      stackListTail = nullptr;
    SsaStack       freeStack;
    unsigned       thisParamLclNum = BAD_VAR_NUM;

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

    bool IsAlwaysLiveThisParam(unsigned lclNum) const
    {
        return lclNum == thisParamLclNum;
    }

    void PushSsaDef(SsaStack* stack, BasicBlock* block, GenTreeLclDef* def)
    {
        SsaStackNode* top = stack->Top();

        if ((top == nullptr) || (top->m_block != block))
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
            top->m_def = def;
        }
    }

#ifdef DEBUG
    void DumpLiveSsaDefs()
    {
        printf("{ ");
        const char* prefix = "";
        for (const auto& pair : lclSsaStackMap)
        {
            if (GenTreeLclDef* def = pair.value.Top()->m_def)
            {
                printf("%sV%02u#%u " FMT_VN, prefix, pair.key, def->GetSsaNum(), def->GetConservativeVN());
                prefix = ", ";
            }
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

public:
    CopyPropDomTreeVisitor(Compiler* compiler)
        : DomTreeVisitor(compiler, compiler->fgSsaDomTree), lclSsaStackMap(compiler->getAllocator(CMK_CopyProp))
    {
    }

    void Begin()
    {
        for (GenTreeLclDef* def = m_compiler->m_initSsaDefs; def != nullptr;
             def                = static_cast<GenTreeLclDef*>(def->gtNext))
        {
            unsigned lclNum = def->GetLclNum();

            if ((lclNum == m_compiler->info.compThisArg) && m_compiler->lvaGetDesc(lclNum)->HasSingleSsaDef())
            {
                thisParamLclNum = m_compiler->info.compThisArg;
            }

            PushSsaDef(lclSsaStackMap.Emplace(lclNum), m_compiler->fgFirstBB, def);
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

            unsigned lclNum = root->AsLclDef()->GetLclNum();

            if (IsAlwaysLiveThisParam(lclNum))
            {
                continue;
            }

            PushSsaDef(lclSsaStackMap.Emplace(lclNum), block, root->AsLclDef());
        }

        for (Statement* stmt : block->NonPhiStatements())
        {
            for (GenTree* node : stmt->Nodes())
            {
                if (GenTreeLclDef* def = node->IsLclDef())
                {
                    unsigned lclNum = def->GetLclNum();

                    if (IsAlwaysLiveThisParam(lclNum))
                    {
                        continue;
                    }

                    PushSsaDef(lclSsaStackMap.Emplace(lclNum), block, def);
                }
                else if (GenTreeLclUse* use = node->IsLclUse())
                {
                    unsigned lclNum = use->GetDef()->GetLclNum();

                    if (IsAlwaysLiveThisParam(lclNum))
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
                            PushSsaDef(lclSsaStackMap.Emplace(lclNum), block, nullptr);
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
        if (use->GetConservativeVN() == ValueNumStore::NoVN)
        {
            return;
        }

        ValueNum   defVN  = use->GetDef()->GetConservativeVN();
        unsigned   lclNum = use->GetDef()->GetLclNum();
        LclVarDsc* lcl    = m_compiler->lvaGetDesc(lclNum);

        // TODO-MIKE-Review: The whole "search the SSA defs for a def with matching VN" approach
        // is rather shoddy, partly due to it being just a linear search (and funnily enough, in
        // a hashtable) and partly due it stopping at the first match. Ideally it would look for
        // all matches and pick the best. But what exactly is "best" in this case? Something that
        // avoids live range extension issues and ping pong copy issues. But it doesn't look like
        // that can be done with readily available information and in a cheap manner.
        // Oh well, this approach to copy propagation seems to be doomed.

        for (const auto& pair : lclSsaStackMap)
        {
            unsigned       newLclNum = pair.key;
            GenTreeLclDef* newDef    = pair.value.Top()->m_def;

            if ((lclNum == newLclNum) || (newDef == nullptr))
            {
                continue;
            }

            LclVarDsc* newLcl = m_compiler->lvaGetDesc(newLclNum);

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

            if (!lcl->lvEHLive && newLcl->lvEHLive)
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

            if ((m_compiler->gsShadowVarInfo != nullptr) && newLcl->IsParam() &&
                (m_compiler->gsShadowVarInfo[newLclNum].shadowLclNum == lclNum))
            {
                continue;
            }

            if (!IsAlwaysLiveThisParam(newLclNum) && (pair.value.Top()->m_block != block) &&
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

            JITDUMP("[%06u] replacing V%02u#%u by V%02u#%u\n", use->GetID(), use->GetDef()->GetLclNum(),
                    use->GetDef()->GetSsaNum(), newLclNum, newDef->GetSsaNum());

            use->GetDef()->RemoveUse(use);
            newDef->AddUse(use);

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

void Compiler::phCopyProp()
{
    assert(ssaForm && (vnStore != nullptr));

    CopyPropDomTreeVisitor visitor(this);
    visitor.WalkTree();
}
