// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

class SsaRenameState
{
public:
    struct StackNode;

    class Stack
    {
        StackNode* m_top;

    public:
        Stack() : m_top(nullptr)
        {
        }

        StackNode* Top() const
        {
            return m_top;
        }

        void Push(StackNode* node)
        {
            node->m_stackPrev = m_top;
            m_top             = node;
        }

        StackNode* Pop()
        {
            StackNode* top = m_top;
            m_top          = top->m_stackPrev;
            return top;
        }
    };

    struct StackNode
    {
        // Link to the previous stack top node
        StackNode* m_stackPrev;
        // Link to the previously pushed stack (used only when popping blocks)
        Stack* m_listPrev;
        // The basic block (used only when popping blocks)
        BasicBlock* m_block;
        // The actual information StackNode stores
        union {
            SsaMemDef*     m_memDef;
            GenTreeLclDef* m_lclDef;
        };

        StackNode(Stack* listPrev, BasicBlock* block, SsaMemDef* def)
            : m_listPrev(listPrev), m_block(block), m_memDef(def)
        {
        }

        StackNode(Stack* listPrev, BasicBlock* block, GenTreeLclDef* def)
            : m_listPrev(listPrev), m_block(block), m_lclDef(def)
        {
        }
    };

private:
    // Memory allocator
    CompAllocator m_alloc;
    // Number of local variables to allocate stacks for
    unsigned m_lvaCount;
    // An array of stack objects, one for each local variable
    Stack* m_stacks;
    // The tail of the list of stacks that have been pushed to
    Stack* m_stackListTail;
    // Same state for the special implicit memory variable
    Stack m_memoryStack;
    // A stack of free stack nodes
    Stack m_freeStack;

public:
    SsaRenameState(CompAllocator alloc, unsigned lvaCount);

    // Get the SSA def at the top of the stack for the specified variable.
    GenTreeLclDef* Top(unsigned lclNum);

    // Push a SSA def onto the stack for the specified variable.
    void Push(BasicBlock* block, unsigned lclNum, GenTreeLclDef* def);

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
    void EnsureStacks();

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

    INDEBUG(void DumpStack(Stack* stack);)
};
