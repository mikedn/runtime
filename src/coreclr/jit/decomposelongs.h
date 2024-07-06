// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                               DecomposeLongs                              XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#ifndef _DECOMPOSELONGS_H_
#define _DECOMPOSELONGS_H_

#include "compiler.h"

class DecomposeLongs
{
public:
    DecomposeLongs(Compiler* compiler) : m_compiler(compiler)
    {
    }

    void PrepareForDecomposition();
    void DecomposeBlock(BasicBlock* block);

    static void DecomposeRange(Compiler* compiler, LIR::Range& range);

private:
    inline LIR::Range& Range() const
    {
        return *m_range;
    }

    void PromoteLongVars();

    // Driver functions
    void     DecomposeRangeHelper();
    GenTree* DecomposeNode(GenTree* tree);

    // Per-node type decompose cases
    GenTree* DecomposeLclLoad(LIR::Use& use);
    GenTree* DecomposeLclLoadFld(LIR::Use& use);
    GenTree* DecomposeLclStore(LIR::Use& use);
    GenTree* DecomposeLclStoreFld(LIR::Use& use);
    GenTree* DecomposeCast(LIR::Use& use);
    GenTree* DecomposeCnsLng(LIR::Use& use);
    GenTree* DecomposeFieldList(GenTreeFieldList* fieldList, GenTreeOp* longNode);
    GenTree* DecomposeCall(LIR::Use& use);
    GenTree* DecomposeIndLoad(LIR::Use& use);
    GenTree* DecomposeIndStore(LIR::Use& use);
    GenTree* DecomposeNot(LIR::Use& use);
    GenTree* DecomposeNeg(LIR::Use& use);
    GenTree* DecomposeAddSub(LIR::Use& use);
    GenTree* DecomposeBitwise(LIR::Use& use);
    GenTree* DecomposeShift(LIR::Use& use);
    GenTree* DecomposeRotate(LIR::Use& use);
    GenTree* DecomposeMul(LIR::Use& use);
    GenTree* DecomposeUMod(LIR::Use& use);

#ifdef FEATURE_HW_INTRINSICS
    GenTree* DecomposeHWIntrinsic(LIR::Use& use);
    GenTree* DecomposeHWIntrinsicGetElement(LIR::Use& use, GenTreeHWIntrinsic* node);
#endif // FEATURE_HW_INTRINSICS

    GenTree* OptimizeCastFromDecomposedLong(GenTreeCast* cast, GenTree* nextNode);

    // Helper functions
    GenTree* FinalizeDecomposition(LIR::Use& use, GenTree* loResult, GenTree* hiResult, GenTree* insertResultAfter);
    GenTreeLclLoad* RepresentOpAsLclLoad(GenTree* op, GenTree* user, GenTree** edge);

    GenTree* StoreMultiRegNodeToLcl(LIR::Use& use);

    // Data
    Compiler*   m_compiler;
    LIR::Range* m_range;
};

#endif // _DECOMPOSELONGS_H_
