// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

struct SsaDefStackNode
{
    // Link to the previous stack top node
    SsaDefStackNode* stackPrev;
    // Link to the previously pushed stack (used only when popping blocks)
    SsaDefStack* listPrev;
    // The basic block (used only when popping blocks)
    BasicBlock* block;
    // The actual information this node stores
    union {
        GenTreeLclDef* lclDef;
        SsaMemDef*     memDef;
    };

    SsaDefStackNode(SsaDefStack* listPrev, BasicBlock* block, GenTreeLclDef* def)
        : listPrev(listPrev), block(block), lclDef(def)
    {
    }

    SsaDefStackNode(SsaDefStack* listPrev, BasicBlock* block, SsaMemDef* def)
        : listPrev(listPrev), block(block), memDef(def)
    {
    }

    void SetDef(GenTreeLclDef* def)
    {
        lclDef = def;
    }

    void SetDef(SsaMemDef* def)
    {
        memDef = def;
    }
};

inline SsaDefStack::SsaDefStack(SsaDefStackNode* top) : top(top)
{
}

inline SsaDefStackNode* SsaDefStack::Top() const
{
    return top;
}

inline void SsaDefStack::Push(SsaDefStackNode* node)
{
    node->stackPrev = top;
    top             = node;
}

inline SsaDefStackNode* SsaDefStack::Pop()
{
    SsaDefStackNode* popped = top;
    top                     = popped->stackPrev;
    return popped;
}
