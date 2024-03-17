// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

struct SsaDefStackNode
{
    // Link to the previous stack top node
    SsaDefStackNode* m_stackPrev;
    // Link to the previously pushed stack (used only when popping blocks)
    SsaDefStack* m_listPrev;
    // The basic block (used only when popping blocks)
    BasicBlock* m_block;
    // The actual information this node stores
    union {
        SsaMemDef*     m_memDef;
        GenTreeLclDef* m_lclDef;
    };

    SsaDefStackNode(SsaDefStack* listPrev, BasicBlock* block, SsaMemDef* def)
        : m_listPrev(listPrev), m_block(block), m_memDef(def)
    {
    }

    SsaDefStackNode(SsaDefStack* listPrev, BasicBlock* block, GenTreeLclDef* def)
        : m_listPrev(listPrev), m_block(block), m_lclDef(def)
    {
    }
};

inline SsaDefStack::SsaDefStack() : m_top(nullptr)
{
}

inline SsaDefStack::SsaDefStack(SsaDefStackNode* top) : m_top(top)
{
}

inline SsaDefStackNode* SsaDefStack::Top() const
{
    return m_top;
}

inline void SsaDefStack::Push(SsaDefStackNode* node)
{
    node->m_stackPrev = m_top;
    m_top             = node;
}

inline SsaDefStackNode* SsaDefStack::Pop()
{
    SsaDefStackNode* top = m_top;
    m_top                = top->m_stackPrev;
    return top;
}
