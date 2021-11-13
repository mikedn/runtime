// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef JIT_RATIONALIZE_H
#define JIT_RATIONALIZE_H

#include "phase.h"

class Rationalizer final : public Phase
{
private:
    BasicBlock* m_block;
    Statement*  m_statement;

public:
    Rationalizer(Compiler* comp) : Phase(comp, PHASE_RATIONALIZE)
    {
        INDEBUG(comp->compNumStatementLinksTraversed = 0;)
    }

    virtual PhaseStatus DoPhase() override;

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
    void RewriteAddress(LIR::Use& use);

    // Root visitor
    Compiler::fgWalkResult RewriteNode(GenTree** useEdge, ArrayStack<GenTree*>& parents);
};

#endif
