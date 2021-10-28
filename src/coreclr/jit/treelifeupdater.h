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
    VARSET_TP newLife;          // a live set after processing an argument tree.
    VARSET_TP stackVarDeltaSet; // a live set of tracked stack ptr lcls.
    VARSET_TP varDeltaSet;      // a set of variables that changed their liveness.
    VARSET_TP gcTrkStkDeltaSet; // // a set of gc tracked stack variables that changed their liveness..
#ifdef DEBUG
    unsigned epoch; // VarSets epoch when the class was created, must stay the same during its using.
#endif

public:
    CodeGenLivenessUpdater(Compiler* compiler);

    void BeginBlock()
    {
        currentNode = nullptr;
    }

    void UpdateLife(class CodeGen* codeGen, GenTree* node);
    bool UpdateLifeFieldVar(class CodeGen* codeGen, GenTreeLclVar* lclNode, unsigned regIndex);

private:
    GenTreeLclVar* IsLocalAddr(GenTree* addr);
};
