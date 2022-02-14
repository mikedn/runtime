// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "compiler.h"

// Handles changes in variable liveness from a given node.
// Keeps set of temporary VARSET_TP during its lifetime to avoid unnecessary memory allocations.
class CodeGenLivenessUpdater
{
    Compiler* compiler;
    GenTree*  currentNode;
    VARSET_TP currentLife;
    VARSET_TP newLife;
    VARSET_TP varDeltaSet;
    VARSET_TP varStackGCPtrDeltaSet;
#ifdef DEBUG
    VARSET_TP scratchSet1;
    VARSET_TP scratchSet2;
    unsigned  epoch;
#endif

    void UpdateLifeMultiReg(class CodeGen* codeGen, GenTreeLclVar* lclNode);
    void UpdateLifePromoted(class CodeGen* codeGen, GenTreeLclVarCommon* lclNode);

public:
    CodeGenLivenessUpdater(Compiler* compiler) : compiler(compiler)
    {
    }

    void Begin();

    void BeginBlock()
    {
        currentNode = nullptr;
    }

    void ChangeLife(class CodeGen* codeGen, VARSET_VALARG_TP newLife);
    void UpdateLife(class CodeGen* codeGen, GenTreeLclVarCommon* lclNode);

    VARSET_VALARG_TP GetLiveSet() const
    {
        return currentLife;
    }
};
