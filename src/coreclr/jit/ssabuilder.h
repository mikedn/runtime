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

using LoopDsc = Compiler::LoopDsc;

struct SsaMemDef
{
    ValueNum vn = NoVN;
    INDEBUG(unsigned num = 0;)
};

using NodeMemDefMap = JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, SsaMemDef>;

struct MemoryPhiArg
{
    SsaMemDef*    m_def;
    MemoryPhiArg* m_nextArg;

    MemoryPhiArg(SsaMemDef* def, MemoryPhiArg* nextArg) : m_def(def), m_nextArg(nextArg)
    {
    }

    SsaMemDef* GetDef() const
    {
        return m_def;
    }

    MemoryPhiArg* GetNext() const
    {
        return m_nextArg;
    }

    void* operator new(size_t sz, class Compiler* comp);
};

class SsaOptimizer
{
    Compiler* const compiler;
    NodeMemDefMap   memoryDefMap;
    LoopDsc*        loopTable      = nullptr;
    unsigned        loopCount      = 0;
    DomTreeNode*    domTree        = nullptr;
    GenTreeLclDef*  initLclDefs    = nullptr;
    SsaMemDef*      initMemDef     = nullptr;
    ValueNumStore*  vnStore        = nullptr;
    AssertionDsc*   assertionTable = nullptr;
    AssertionIndex  assertionCount = 0;
    INDEBUG(unsigned memDefCount = 0;)

public:
    SsaOptimizer(Compiler* compiler)
        : compiler(compiler)
        , memoryDefMap(compiler->getAllocator(CMK_SSA))
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

    void SetInitLclDefs(GenTreeLclDef* defs)
    {
        initLclDefs = defs;
    }

    GenTreeLclDef* GetInitLclDefs() const
    {
        return initLclDefs;
    }

    SsaMemDef* AllocMemoryDef()
    {
        SsaMemDef* def = new (compiler->getAllocator(CMK_SSA)) SsaMemDef();
        INDEBUG(def->num = ++memDefCount);
        return def;
    }

    SsaMemDef* AllocNodeMemoryDef(GenTree* node)
    {
        SsaMemDef* def = memoryDefMap.Emplace(node);
        assert(def->num == 0);
        INDEBUG(def->num = ++memDefCount);
        return def;
    }

    SsaMemDef* GetMemoryDef(GenTree* node) const
    {
        return memoryDefMap.LookupPointer(node);
    }

    SsaMemDef* GetInitMemoryDef() const
    {
        return initMemDef;
    }

    void SetInitMemoryDef(SsaMemDef* def)
    {
        initMemDef = def;
    }

    ValueNumStore* GetVNStore() const
    {
        return vnStore;
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

    PhaseStatus DoSsaBuild();
    PhaseStatus DoEarlyProp();
    PhaseStatus DoValueNumber();
    PhaseStatus DoLoopHoist();
    PhaseStatus DoCopyProp();
    PhaseStatus DoRedundantBranches();
    PhaseStatus DoCse();
    PhaseStatus DoAssertionProp();
    PhaseStatus DoRemoveRangeCheck();
    PhaseStatus DoSsaDestroy();

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
};
