// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "compiler.h"
#include "phase.h"

class StackLevelSetter final : public Phase<StackLevelSetter>
{
public:
    StackLevelSetter(Compiler* compiler);

    PhaseStatus DoPhase();

private:
    void ProcessBlock(BasicBlock* block);

#if !FEATURE_FIXED_OUT_ARGS
    void SetThrowHelperBlocks(GenTree* node, BasicBlock* block);
    void SetThrowHelperBlock(SpecialCodeKind kind, BasicBlock* block);
#endif // !FEATURE_FIXED_OUT_ARGS

    unsigned PopArgumentsFromCall(GenTreeCall* call);
    void PushArg(GenTreePutArgStk* putArgStk);
    void PopArg(GenTreePutArgStk* putArgStk);

    void CheckArgCnt();
    void CheckAdditionalArgs();

private:
    unsigned currentStackLevel = 0; // current number of stack slots used by arguments.
    unsigned maxStackLevel = 0;     // max number of stack slots for arguments.

#if !FEATURE_FIXED_OUT_ARGS
    bool framePointerRequired;  // Is frame pointer required based on the analysis made by this phase.
    bool throwHelperBlocksUsed; // Were any throw helper blocks created for this method.
#endif                          // !FEATURE_FIXED_OUT_ARGS
};
