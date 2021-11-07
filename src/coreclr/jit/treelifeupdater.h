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
    INDEBUG(unsigned epoch;)

public:
    CodeGenLivenessUpdater(Compiler* compiler) : compiler(compiler)
    {
    }

    void Begin();

    void BeginBlock()
    {
        currentNode = nullptr;
    }

    void UpdateLife(class CodeGen* codeGen, GenTreeLclVarCommon* lclNode);
    bool UpdateLifeFieldVar(class CodeGen* codeGen, GenTreeLclVar* lclNode, unsigned regIndex);

    VARSET_VALARG_TP GetLiveSet() const
    {
        return currentLife;
    }

    void SetLiveSet(VARSET_VALARG_TP newLife)
    {
        VarSetOps::Assign(compiler, currentLife, newLife);
    }
};
