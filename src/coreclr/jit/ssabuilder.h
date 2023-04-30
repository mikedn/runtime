// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "phase.h"

class Compiler;
struct GenTreeLclDef;
class ValueNumStore;
class SsaBuilder;
struct AssertionDsc;
class AssertionProp;

class BoundsAssertion
{
    const AssertionDsc& assertion;

public:
    BoundsAssertion(const AssertionDsc& assertion) : assertion(assertion)
    {
    }

    INDEBUG(const AssertionDsc& GetAssertion() const;)

    bool IsBoundsAssertion() const;
    bool IsEqual() const;
    bool IsCompareCheckedBoundArith() const;
    bool IsCompareCheckedBound() const;
    bool IsRange() const;
    bool IsConstant() const;

    ValueNum GetVN() const;
    ValueNum GetConstantVN() const;
    int      GetRangeMin() const;
    int      GetRangeMax() const;
};

using NodeSsaNumMap = JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, unsigned>;
using LoopDsc       = Compiler::LoopDsc;

struct SsaMemDef
{
    ValueNum vn = NoVN;
};

class SsaOptimizer
{
    Compiler* const        compiler;
    SsaDefArray<SsaMemDef> memorySsaDefs;
    NodeSsaNumMap          memorySsaMap;
    LoopDsc*               loopTable      = nullptr;
    unsigned               loopCount      = 0;
    DomTreeNode*           domTree        = nullptr;
    GenTreeLclDef*         initSsaDefs    = nullptr;
    ValueNumStore*         vnStore        = nullptr;
    AssertionDsc*          assertionTable = nullptr;
    AssertionIndex         assertionCount = 0;

public:
    SsaOptimizer(Compiler* compiler)
        : compiler(compiler)
        , memorySsaMap(compiler->getAllocator(CMK_SSA))
        , loopTable(compiler->optLoopTable)
        , loopCount(compiler->optLoopCount)
    {
    }

    void Run();

    Compiler* GetCompiler() const
    {
        return compiler;
    }

    LoopDsc* GetLoopTable() const
    {
        return loopTable;
    }

    LoopDsc* GetLoop(unsigned index) const
    {
        assert(index < loopCount);
        return &loopTable[index];
    }

    unsigned GetLoopCount() const
    {
        return loopCount;
    }

    DomTreeNode* GetDomTree() const
    {
        return domTree;
    }

    void SetDomTree(DomTreeNode* tree)
    {
        domTree = tree;
    }

    void SetInitSsaDefs(GenTreeLclDef* defs)
    {
        initSsaDefs = defs;
    }

    unsigned AllocMemorySsaNum()
    {
        return memorySsaDefs.AllocSsaNum(compiler->getAllocator(CMK_SSA));
    }

    SsaMemDef& GetMemorySsaDef(unsigned ssaNum)
    {
        return *memorySsaDefs.GetSsaDef(ssaNum);
    }

    void SetMemorySsaNum(GenTree* node, unsigned ssaNum)
    {
        assert(ssaNum != NoSsaNum);
        memorySsaMap.Set(node, ssaNum);
    }

    unsigned GetMemorySsaNum(GenTree* node) const
    {
        unsigned ssaNum;
        return memorySsaMap.Lookup(node, &ssaNum) ? ssaNum : NoSsaNum;
    }

    ValueNumStore* GetVNStore() const
    {
        return vnStore;
    }

    GenTreeLclDef* GetInitSsaDefs() const
    {
        return initSsaDefs;
    }

    static constexpr unsigned MinCseCost = 2;

    bool IsCseCandidate(GenTree* tree);

    void SetAssertionTable(AssertionDsc* table, unsigned count)
    {
        assertionTable = table;
        assertionCount = count;

        INDEBUG(compiler->apAssertionCount = count);
    }

    AssertionIndex GetAssertionCount() const
    {
        return assertionCount;
    }

    BoundsAssertion GetBoundsAssertion(unsigned bitIndex) const;

#ifdef DEBUG
    void DumpAssertion(const AssertionDsc& assertion, unsigned index);
    void DumpAssertionIndices(const char* header, ASSERT_TP assertions, const char* footer);
    void DumpBoundsAssertion(BoundsAssertion assertion);
#endif

private:
#ifdef OPT_CONFIG
    void Reset();
#endif

    void        DoSsaBuild();
    void        DoEarlyProp();
    void        DoValueNumber();
    void        DoLoopHoist();
    void        DoCopyProp();
    PhaseStatus DoRedundantBranches();
    void        DoCse();
    void        DoAssertionProp();
    void        DoRemoveRangeCheck();
    void        DoSsaDestroy();

    void DoPhase(Phases phaseId, PhaseStatus (SsaOptimizer::*action)())
    {
        class SsaPhase final : public Phase<SsaPhase>
        {
            SsaOptimizer& ssa;
            PhaseStatus (SsaOptimizer::*action)();

        public:
            SsaPhase(SsaOptimizer& ssa, Phases phaseId, PhaseStatus (SsaOptimizer::*action)())
                : Phase<SsaPhase>(ssa.compiler, phaseId), ssa(ssa), action(action)
            {
            }

            PhaseStatus DoPhase()
            {
                return (ssa.*action)();
            }
        } phase(*this, phaseId, action);

        phase.Run();
    }

    void DoPhase(Phases phaseId, void (SsaOptimizer::*action)())
    {
        class SsaPhase final : public Phase<SsaPhase>
        {
            SsaOptimizer& ssa;
            void (SsaOptimizer::*action)();

        public:
            SsaPhase(SsaOptimizer& ssa, Phases phaseId, void (SsaOptimizer::*action)())
                : Phase<SsaPhase>(ssa.compiler, phaseId), ssa(ssa), action(action)
            {
            }

            PhaseStatus DoPhase()
            {
                (ssa.*action)();
                return PhaseStatus::MODIFIED_EVERYTHING;
            }
        } phase(*this, phaseId, action);

        phase.Run();
    }
};
