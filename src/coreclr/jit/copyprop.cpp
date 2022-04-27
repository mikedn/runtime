// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

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

#include "jitpch.h"
#include "ssabuilder.h"

class CopyPropLivenessUpdater
{
    Compiler* compiler;
    VARSET_TP liveSet;
    INDEBUG(VARSET_TP scratchSet;)

public:
    CopyPropLivenessUpdater::CopyPropLivenessUpdater(Compiler* compiler)
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

typedef JitHashTable<unsigned, JitSmallPrimitiveKeyFuncs<unsigned>, ArrayStack<GenTree*>*> LclNumToGenTreePtrStack;

class Compiler::CopyPropDomTreeVisitor : public DomTreeVisitor<Compiler::CopyPropDomTreeVisitor>
{
public:
    // The map from lclNum to its recently live definitions as a stack.
    LclNumToGenTreePtrStack curSsaName;
    CopyPropLivenessUpdater liveness;

    CopyPropDomTreeVisitor(Compiler* compiler)
        : DomTreeVisitor(compiler, compiler->fgSsaDomTree)
        , curSsaName(compiler->getAllocator(CMK_CopyProp))
        , liveness(compiler)
    {
    }

    void PreOrderVisit(BasicBlock* block)
    {
        // TODO-Cleanup: Move this function from Compiler to this class.
        m_compiler->optBlockCopyProp(block, *this);
    }

    void PostOrderVisit(BasicBlock* block)
    {
        // TODO-Cleanup: Move this function from Compiler to this class.
        m_compiler->optBlockCopyPropPopStacks(block, *this);
    }
};

void Compiler::optBlockCopyPropPopStacks(BasicBlock* block, CopyPropDomTreeVisitor& visitor)
{
    auto& curSsaName = visitor.curSsaName;

    for (Statement* const stmt : block->Statements())
    {
        for (GenTree* const node : stmt->Nodes())
        {
            GenTreeLclVarCommon* lclNode = optIsSsaLocal(node);

            if ((lclNode == nullptr) || ((node->gtFlags & GTF_VAR_DEF) == 0))
            {
                continue;
            }

            ArrayStack<GenTree*>* stack = nullptr;
            curSsaName.Lookup(lclNode->GetLclNum(), &stack);
            stack->Pop();
            if (stack->Empty())
            {
                curSsaName.Remove(lclNode->GetLclNum());
            }
        }
    }
}

#ifdef DEBUG
void Compiler::optDumpCopyPropStack(CopyPropDomTreeVisitor& visitor)
{
    JITDUMP("{ ");
    for (const auto& pair : visitor.curSsaName)
    {
        GenTreeLclVarCommon* lclNode = pair.value->Top()->AsLclVarCommon();
        JITDUMP("%u-[%06u]:V%02u ", pair.key, lclNode->GetID(), lclNode->GetLclNum());
    }
    JITDUMP("}\n\n");
}
#endif

int Compiler::optCopyProp_LclVarScore(LclVarDsc* lclVarDsc, LclVarDsc* copyVarDsc, bool preferOp2)
{
    int score = 0;

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

    // Otherwise we prefer to use the op2LclNum
    return score + ((preferOp2) ? 1 : -1);
}

void Compiler::optCopyProp(GenTreeLclVar* use, CopyPropDomTreeVisitor& visitor)
{
    assert(use->OperIs(GT_LCL_VAR) && ((use->gtFlags & GTF_VAR_DEF) == 0));

    if (use->GetConservativeVN() == ValueNumStore::NoVN)
    {
        return;
    }

    unsigned   lclNum = use->GetLclNum();
    LclVarDsc* lcl    = lvaGetDesc(lclNum);

    for (const auto& pair : visitor.curSsaName)
    {
        unsigned newLclNum = pair.key;

        // Nothing to do if same.
        if (lclNum == newLclNum)
        {
            continue;
        }

        GenTreeLclVarCommon* def = pair.value->Top()->AsLclVarCommon();

        LclVarDsc* newLcl = lvaGetDesc(newLclNum);

        if (varActualType(newLcl->GetType()) != varActualType(lcl->GetType()))
        {
            continue;
        }

        // Do not copy propagate if the old and new lclVar have different 'doNotEnregister' settings.
        // This is primarily to avoid copy propagating to IND(ADDR(LCL_VAR)) where the replacement lclVar
        // is not marked 'lvDoNotEnregister'.
        // However, in addition, it may not be profitable to propagate a 'doNotEnregister' lclVar to an
        // existing use of an enregisterable lclVar.

        if (lcl->lvDoNotEnregister != newLcl->lvDoNotEnregister)
        {
            continue;
        }

        if ((gsShadowVarInfo != nullptr) && newLcl->IsParam() && (gsShadowVarInfo[newLclNum].shadowLclNum == lclNum))
        {
            continue;
        }

        unsigned newSsaNum = SsaConfig::RESERVED_SSA_NUM;

        if ((def->gtFlags & GTF_VAR_DEF) != 0)
        {
            newSsaNum = GetSsaNumForLocalVarDef(def);
        }
        else // parameters, this pointer etc.
        {
            newSsaNum = def->GetSsaNum();
        }

        if (newSsaNum == SsaConfig::RESERVED_SSA_NUM)
        {
            continue;
        }

        // The use must produce the same value number if we substitute the def.
        if (vnLocalLoad(use, newLcl, newSsaNum).GetConservative() != use->GetConservativeVN())
        {
            continue;
        }

        if (optCopyProp_LclVarScore(lcl, newLcl, true) <= 0)
        {
            continue;
        }

        // Check whether the newLclNum is live before being substituted. Otherwise, we could end
        // up in a situation where there must've been a PHI node that got pruned because the variable
        // is not live anymore.
        if (!VarSetOps::IsMember(this, visitor.liveness.GetLiveSet(), newLcl->GetLivenessBitIndex()))
        {
            continue;
        }

        JITDUMP("[%06u] replacing V%02u:%u by V%02u:%u\n", use->GetID(), use->GetLclNum(), use->GetSsaNum(), newLclNum,
                newSsaNum);

        use->SetLclNum(newLclNum);
        use->SetSsaNum(newSsaNum);

        break;
    }
}

GenTreeLclVarCommon* Compiler::optIsSsaLocal(GenTree* node)
{
    return node->OperIs(GT_LCL_VAR, GT_LCL_FLD) && lvaGetDesc(node->AsLclVarCommon())->IsInSsa()
               ? node->AsLclVarCommon()
               : nullptr;
}

void Compiler::optBlockCopyProp(BasicBlock* block, CopyPropDomTreeVisitor& visitor)
{
#ifdef DEBUG
    JITDUMP("Copy Assertion for " FMT_BB "\n", block->bbNum);
    if (verbose)
    {
        printf("  curSsaName stack: ");
        optDumpCopyPropStack(visitor);
    }
#endif

    visitor.liveness.BeginBlock(block);

    auto& curSsaName = visitor.curSsaName;

    for (Statement* const stmt : block->Statements())
    {
        for (GenTree* const node : stmt->Nodes())
        {
            GenTreeLclVarCommon* lclNode = optIsSsaLocal(node);

            if (lclNode == nullptr)
            {
                continue;
            }

            unsigned   lclNum = lclNode->GetLclNum();
            LclVarDsc* lcl    = lvaGetDesc(lclNum);

            if ((lclNode->gtFlags & GTF_VAR_DEF) != 0)
            {
                visitor.liveness.UpdateDef(lclNode);

                ArrayStack<GenTree*>* stack;
                if (!curSsaName.Lookup(lclNum, &stack))
                {
                    stack = new (curSsaName.GetAllocator()) ArrayStack<GenTree*>(curSsaName.GetAllocator());
                }
                stack->Push(lclNode);
                curSsaName.Set(lclNum, stack, LclNumToGenTreePtrStack::Overwrite);

                continue;
            }

            visitor.liveness.UpdateUse(lclNode);

            if (lclNode->OperIs(GT_LCL_VAR))
            {
                // TODO-Review: EH successor/predecessor iteration seems broken.
                if ((block->bbCatchTyp != BBCT_FINALLY) && (block->bbCatchTyp != BBCT_FAULT))
                {
                    optCopyProp(lclNode->AsLclVar(), visitor);
                }

                // If we encounter first use of a param or this pointer add it as a live definition.
                // Since they are always live, do it only once.
                // TODO-MIKE-Review: Hmm, why not LCL_FLDs too?
                if (lcl->IsParam())
                {
                    ArrayStack<GenTree*>* stack;
                    if (!curSsaName.Lookup(lclNum, &stack))
                    {
                        stack = new (curSsaName.GetAllocator()) ArrayStack<GenTree*>(curSsaName.GetAllocator());
                        stack->Push(lclNode);
                        curSsaName.Set(lclNum, stack);
                    }
                }
            }
        }
    }
}

void Compiler::optVnCopyProp()
{
    JITDUMP("*************** In optVnCopyProp()\n");

    assert(ssaForm && (vnStore != nullptr));

    CopyPropDomTreeVisitor visitor(this);
    visitor.WalkTree();
}
