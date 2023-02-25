// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "compiler.h"
#include "phase.h"

#if !FEATURE_FIXED_OUT_ARGS

class StackLevelSetter final : public Phase<StackLevelSetter>
{
    unsigned currentStackLevel = 0; // current number of stack slots used by arguments.
    unsigned maxStackLevel     = 0; // max number of stack slots for arguments.
    bool     framePointerRequired;  // Is frame pointer required based on the analysis made by this phase.
    bool     throwHelperBlocksUsed; // Were any throw helper blocks created for this method.

public:
    StackLevelSetter(Compiler* compiler);

    PhaseStatus DoPhase();

private:
    void ProcessBlock(BasicBlock* block);
    void SetThrowHelperBlockStackLevel(GenTree* node, BasicBlock* throwBlock);
    void SetThrowHelperBlockStackLevel(ThrowHelperKind kind, BasicBlock* throwBlock);
    unsigned PopArgumentsFromCall(GenTreeCall* call);
    void PushArg(GenTreePutArgStk* putArgStk);
    void PopArg(GenTreePutArgStk* putArgStk);
};

#endif // !FEATURE_FIXED_OUT_ARGS
