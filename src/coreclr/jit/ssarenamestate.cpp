// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "ssarenamestate.h"
#include "ssabuilder.h"

//------------------------------------------------------------------------
// SsaRenameState: Initialize SsaRenameState
//
// Arguments:
//    alloc - A memory allocator
//    lvaCount - The number of local variables
//
SsaRenameState::SsaRenameState(Compiler* compiler)
    : m_alloc(compiler->getAllocator(CMK_SSA))
#ifdef DEBUG
    , m_compiler(compiler)
#endif
{
}

//------------------------------------------------------------------------
// Top: Get the SSA number at the top of the stack for the specified variable.
//
// Arguments:
//    lclNum - The local variable number
//
// Return Value:
//    The SSA number.
//
// Notes:
//    The stack must not be empty. Method parameters and local variables that are live in at
//    the start of the first block must have associated SSA definitions and their SSA numbers
//    must have been pushed first.
//
GenTreeLclDef* SsaRenameState::Top(LclVarDsc* lcl)
{
    DBG_SSA_JITDUMP("[SsaRenameState::Top] V%02u\n", lcl->GetLclNum());

    SsaDefStackNode* top = lcl->ssa.renameStack.Top();
    noway_assert(top != nullptr);
    assert(top->m_lclDef->GetLcl() == lcl);
    return top->m_lclDef;
}

//------------------------------------------------------------------------
// Push: Push a SSA number onto the stack for the specified variable.
//
// Arguments:
//    block  - The block where the SSA definition occurs
//    lclNum - The local variable number
//    ssaNum - The SSA number
//
void SsaRenameState::Push(BasicBlock* block, LclVarDsc* lcl, GenTreeLclDef* def)
{
    DBG_SSA_JITDUMP("[SsaRenameState::Push] " FMT_BB ", V%02u, def = [%06u]\n", block->bbNum, lcl->GetLclNum(),
                    def->GetID());

    Push(&lcl->ssa.renameStack, block, def);
}

void SsaRenameState::Push(SsaDefStack* stack, BasicBlock* block, SsaMemDef* def)
{
    SsaDefStackNode* top = stack->Top();

    if ((top == nullptr) || (top->m_block != block))
    {
        stack->Push(AllocStackNode(m_stackListTail, block, def));
        // Append the stack to the stack list. The stack list allows
        // PopBlockStacks to easily find stacks that need popping.
        m_stackListTail = stack;
    }
    else
    {
        // If we already have a stack node for this block then simply
        // update the SSA def, the previous one is no longer needed.
        top->m_memDef = def;
    }

    INDEBUG(DumpStack(stack));
}

void SsaRenameState::Push(SsaDefStack* stack, BasicBlock* block, GenTreeLclDef* def)
{
    SsaDefStackNode* top = stack->Top();

    if ((top == nullptr) || (top->m_block != block))
    {
        stack->Push(AllocStackNode(m_stackListTail, block, def));
        // Append the stack to the stack list. The stack list allows
        // PopBlockStacks to easily find stacks that need popping.
        m_stackListTail = stack;
    }
    else
    {
        // If we already have a stack node for this block then simply
        // update the SSA def, the previous one is no longer needed.
        top->m_lclDef = def;
    }

    INDEBUG(DumpStack(stack, def->GetLcl()));
}

void SsaRenameState::PopBlockStacks(BasicBlock* block)
{
    DBG_SSA_JITDUMP("[SsaRenameState::PopBlockStacks] " FMT_BB "\n", block->bbNum);

    while ((m_stackListTail != nullptr) && (m_stackListTail->Top()->m_block == block))
    {
        SsaDefStackNode* top = m_stackListTail->Pop();
        INDEBUG(DumpStack(m_stackListTail));
        m_stackListTail = top->m_listPrev;
        m_freeStack.Push(top);
    }

#ifdef DEBUG
    // It should now be the case that no stack in stacks has an entry for "block" on top --
    // the loop above popped them all.
    for (LclVarDsc* lcl : m_compiler->Locals())
    {
        if (lcl->ssa.renameStack.Top() != nullptr)
        {
            assert(lcl->ssa.renameStack.Top()->m_block != block);
        }
    }
#endif // DEBUG
}

#ifdef DEBUG
//------------------------------------------------------------------------
// DumpStack: Print the specified stack.
//
// Arguments:
//    stack - The stack to print
//
void SsaRenameState::DumpStack(SsaDefStack* stack, LclVarDsc* lcl)
{
    if (JitTls::GetCompiler()->verboseSsa)
    {
        if (stack == &m_memoryStack)
        {
            printf("Memory: ");
        }
        else if (lcl != nullptr)
        {
            printf("V%02u: ", lcl->GetLclNum());
        }
        else
        {
            for (LclVarDsc* lcl : m_compiler->Locals())
            {
                if (&lcl->ssa.renameStack == stack)
                {
                    printf("V%02u: ", lcl->GetLclNum());
                }
            }
        }

        for (SsaDefStackNode* i = stack->Top(); i != nullptr; i = i->m_stackPrev)
        {
            if (stack == &m_memoryStack)
            {
                printf("%s<" FMT_BB ", %u>", (i == stack->Top()) ? "" : ", ", i->m_block->bbNum, i->m_memDef->num);
            }
            else
            {
                printf("%s<" FMT_BB ", [%06u]>", (i == stack->Top()) ? "" : ", ", i->m_block->bbNum,
                       i->m_lclDef->GetID());
            }
        }

        printf("\n");
    }
}
#endif // DEBUG
