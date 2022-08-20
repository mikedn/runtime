// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

// A phase encapsulates a part of the compilation pipeline for a method.
class Phase
{
#ifdef DEBUG
    // Observations made before a phase runs that should still
    // be true afterwards,if the phase status is MODIFIED_NOTHING.
    class Observations
    {
        Compiler* m_compiler;
        unsigned  m_fgBBcount;
        unsigned  m_fgBBNumMax;
        unsigned  m_compHndBBtabCount;
        unsigned  m_lvaCount;
        unsigned  m_compGenTreeID;
        unsigned  m_compStatementID;
        unsigned  m_compBasicBlockID;

    public:
        Observations(Compiler* compiler);
        void Check(PhaseStatus status);
    };
#endif // DEBUG

    void PrePhase();
    void PostPhase(PhaseStatus status);

    Phases m_phaseId;

protected:
    Compiler* comp;

    Phase(Compiler* compiler, Phases phaseId) : m_phaseId(phaseId), comp(compiler)
    {
    }

    virtual PhaseStatus DoPhase() = 0;

public:
    void Run();
};

template <typename A>
void DoPhase(Compiler* compiler, Phases phaseId, A action)
{
    class ActionPhase final : public Phase
    {
        A action;

    public:
        ActionPhase(Compiler* compiler, Phases phaseId, A action) : Phase(compiler, phaseId), action(action)
        {
        }

    protected:
        virtual PhaseStatus DoPhase() override
        {
            action();
            return PhaseStatus::MODIFIED_EVERYTHING;
        }
    } phase(compiler, phaseId, action);

    phase.Run();
}

inline void DoPhase(Compiler* compiler, Phases phaseId, void (Compiler::*action)())
{
    class CompilerPhase final : public Phase
    {
        void (Compiler::*action)();

    public:
        CompilerPhase(Compiler* compiler, Phases phaseId, void (Compiler::*action)())
            : Phase(compiler, phaseId), action(action)
        {
        }

    protected:
        virtual PhaseStatus DoPhase() override
        {
            (comp->*action)();
            return PhaseStatus::MODIFIED_EVERYTHING;
        }
    } phase(compiler, phaseId, action);

    phase.Run();
}

inline void DoPhase(Compiler* compiler, Phases phaseId, PhaseStatus (Compiler::*action)())
{
    class CompilerPhase final : public Phase
    {
        PhaseStatus (Compiler::*action)();

    public:
        CompilerPhase(Compiler* compiler, Phases phaseId, PhaseStatus (Compiler::*action)())
            : Phase(compiler, phaseId), action(action)
        {
        }

    protected:
        virtual PhaseStatus DoPhase() override
        {
            return (comp->*action)();
        }
    } phase(compiler, phaseId, action);

    phase.Run();
}
