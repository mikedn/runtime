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

class SsaRenameState
{
    using Stack     = SsaDefStack;
    using StackNode = SsaDefStackNode;

private:
    // Memory allocator
    CompAllocator m_alloc;
    // The tail of the list of stacks that have been pushed to
    Stack* m_stackListTail = nullptr;
    // Same state for the special implicit memory variable
    Stack m_memoryStack;
    // A stack of free stack nodes
    Stack m_freeStack;
#ifdef DEBUG
    Compiler* m_compiler;
#endif

public:
    SsaRenameState(Compiler* compiler);

    // Get the SSA def at the top of the stack for the specified variable.
    GenTreeLclDef* Top(LclVarDsc* lcl);

    // Push a SSA def onto the stack for the specified variable.
    void Push(BasicBlock* block, LclVarDsc* lcl, GenTreeLclDef* def);

    // Pop all stacks that have an entry for "block" on top.
    void PopBlockStacks(BasicBlock* block);

    // Similar functions for the special implicit memory variable.
    SsaMemDef* TopMemory()
    {
        return m_memoryStack.Top()->m_memDef;
    }

    void PushMemory(BasicBlock* block, SsaMemDef* def)
    {
        Push(&m_memoryStack, block, def);
    }

private:
    // Allocate a new stack entry (possibly by popping it from the free stack)
    template <class... Args>
    StackNode* AllocStackNode(Args&&... args)
    {
        StackNode* stack = m_freeStack.Top();

        if (stack != nullptr)
        {
            m_freeStack.Pop();
        }
        else
        {
            stack = m_alloc.allocate<StackNode>(1);
        }

        return new (stack) StackNode(std::forward<Args>(args)...);
    }

    void Push(Stack* stack, BasicBlock* block, SsaMemDef* def);
    void Push(Stack* stack, BasicBlock* block, GenTreeLclDef* def);

    INDEBUG(void DumpStack(Stack* stack, LclVarDsc* lcl = nullptr);)
};
