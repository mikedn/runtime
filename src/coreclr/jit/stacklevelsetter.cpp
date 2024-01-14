// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#if !FEATURE_FIXED_OUT_ARGS

#include "jitgcinfo.h"

// Sets throw-helper blocks incoming stack depth and set framePointerRequired when
// it is necessary. These values are used to pop pushed args when an exception occurs.
class StackLevelSetter
{
    Compiler* comp;
    unsigned  currentStackLevel = 0; // current number of stack slots used by arguments.
    unsigned  maxStackLevel     = 0; // max number of stack slots for arguments.
    bool      framePointerRequired;  // Is frame pointer required based on the analysis made by this phase.
    bool      throwHelperBlocksUsed; // Were any throw helper blocks created for this method.

public:
    StackLevelSetter(Compiler* compiler);

    void Run();

private:
    void ProcessBlock(BasicBlock* block);
    void SetThrowHelperBlockStackLevel(GenTree* node, BasicBlock* throwBlock);
    void SetThrowHelperBlockStackLevel(ThrowHelperKind kind, BasicBlock* throwBlock);
    unsigned PopArgumentsFromCall(GenTreeCall* call);
    void PushArg(GenTreePutArgStk* putArgStk);
    void PopArg(GenTreePutArgStk* putArgStk);
};

StackLevelSetter::StackLevelSetter(Compiler* compiler)
    : comp(compiler)
    , framePointerRequired(compiler->opts.IsFramePointerRequired())
    , throwHelperBlocksUsed(comp->fgUseThrowHelperBlocks() && comp->compUsesThrowHelper)
{
}

void StackLevelSetter::Run()
{
    for (BasicBlock* const block : comp->Blocks())
    {
        ProcessBlock(block);
    }

    // Profiler hook calls do not appear in the IR and do push an argument.
    if (comp->compIsProfilerHookNeeded() && (maxStackLevel == 0))
    {
        maxStackLevel = 1;
    }

    if ((maxStackLevel >= 4) || framePointerRequired)
    {
        comp->opts.SetFramePointerRequired();
    }

    // The GC encoding for fully interruptible methods does not
    // support more than 1023 pushed arguments, so we have to
    // use a partially interruptible GC info/encoding.
    if (maxStackLevel >= MAX_PTRARG_OFS)
    {
        JITDUMP("Too many pushed arguments for fully interruptible encoding, marking method as partially "
                "interruptible\n");

        comp->codeGen->SetInterruptible(false);
    }
}

//------------------------------------------------------------------------
// ProcessBlock: Do stack level calculations for one block.
//
// Notes:
//   Block starts and ends with an empty outgoing stack.
//   Nodes in blocks are iterated in the reverse order to memorize GT_PUTARG_STK
//   and GT_PUTARG_SPLIT stack sizes.
//
// Arguments:
//   block - the block to process.
//
void StackLevelSetter::ProcessBlock(BasicBlock* block)
{
    assert(currentStackLevel == 0);

    for (GenTree* node : LIR::AsRange(block))
    {
        if (GenTreePutArgStk* putArgStk = node->IsPutArgStk())
        {
            PushArg(putArgStk);
            continue;
        }

        if (throwHelperBlocksUsed && !framePointerRequired)
        {
            if (node->OperMayThrow(comp))
            {
                SetThrowHelperBlockStackLevel(node, block);
            }
        }

        if (GenTreeCall* call = node->IsCall())
        {
            unsigned usedStackSlotsCount = PopArgumentsFromCall(call);
#ifdef UNIX_X86_ABI
            call->fgArgInfo->SetStkSizeBytes(usedStackSlotsCount * TARGET_POINTER_SIZE);
#endif
        }
    }
    assert(currentStackLevel == 0);
}

void StackLevelSetter::SetThrowHelperBlockStackLevel(GenTree* node, BasicBlock* throwBlock)
{
    assert(node->OperMayThrow(comp));

    switch (node->GetOper())
    {
        case GT_BOUNDS_CHECK:
            SetThrowHelperBlockStackLevel(node->AsBoundsChk()->GetThrowKind(), throwBlock);
            break;

        case GT_INDEX_ADDR:
        case GT_ARR_ELEM:
        case GT_ARR_INDEX:
            SetThrowHelperBlockStackLevel(ThrowHelperKind::IndexOutOfRange, throwBlock);
            break;

        case GT_CKFINITE:
            SetThrowHelperBlockStackLevel(ThrowHelperKind::Arithmetic, throwBlock);
            break;

        default:
            if (node->gtOverflowEx())
            {
                SetThrowHelperBlockStackLevel(ThrowHelperKind::Overflow, throwBlock);
            }
            break;
    }
}

void StackLevelSetter::SetThrowHelperBlockStackLevel(ThrowHelperKind kind, BasicBlock* throwBlock)
{
    ThrowHelperBlock* helper = comp->fgFindThrowHelperBlock(kind, throwBlock);

    if (helper->stackLevelSet)
    {
        // If different range checks happen at different stack levels,
        // they can't all jump to the same "call @rngChkFailed" AND have
        // frameless methods, as the rngChkFailed may need to unwind the
        // stack, and we have to be able to report the stack level.
        //
        // The following check forces most methods that reference an
        // array element in a parameter list to have an EBP frame,
        // this restriction could be removed with more careful code
        // generation for BBJ_THROW (i.e. range check failed).
        //
        // For Linux/x86, we possibly need to insert stack alignment adjustment
        // before the first stack argument pushed for every call. But we
        // don't know what the stack alignment adjustment will be when
        // we morph a tree that calls fgGetThrowHelperBlock, so the stack depth
        // number will be incorrect. For now, simply force all functions with
        // these helpers to have EBP frames. It might be possible to make
        // this less conservative. E.g., for top-level (not nested) calls
        // without stack args, the stack pointer hasn't changed and stack
        // depth will be known to be zero. Or, figure out a way to update
        // or generate all required helpers after all stack alignment
        // has been added, and the stack level at each call to fgGetThrowHelperBlock
        // is known, or can be recalculated.
        CLANG_FORMAT_COMMENT_ANCHOR;

#ifndef UNIX_X86_ABI
        if (helper->stackLevel != currentStackLevel)
#endif
        {
            framePointerRequired = true;
        }
    }
    else
    {
        helper->stackLevelSet = true;
        helper->stackLevel    = currentStackLevel;
    }
}

//------------------------------------------------------------------------
// PopArgumentsFromCall: Calculate the number of stack arguments that are used by the call.
//
// Notes:
//   memorize number of slots that each stack argument use.
//
// Arguments:
//   call - the call to process.
//
// Return value:
//   the number of stack slots in stack arguments for the call.
unsigned StackLevelSetter::PopArgumentsFromCall(GenTreeCall* call)
{
    unsigned  usedStackSlotsCount = 0;
    CallInfo* callInfo            = call->GetInfo();
    if (callInfo->HasStackArgs())
    {
        // TODO-MIKE-Cleaup: CallInfo::GetNextSlotNum should provide the total
        // slot count so this loop shouldn't be necessary.

        for (unsigned i = 0; i < callInfo->GetArgCount(); ++i)
        {
            if (GenTreePutArgStk* putArgStk = callInfo->GetArgInfo(i)->GetNode()->IsPutArgStk())
            {
                usedStackSlotsCount += putArgStk->GetSlotCount();
                PopArg(putArgStk);
            }
        }
    }
    return usedStackSlotsCount;
}

void StackLevelSetter::PushArg(GenTreePutArgStk* putArgStk)
{
    currentStackLevel += putArgStk->GetSlotCount();

    if (currentStackLevel > maxStackLevel)
    {
        maxStackLevel = currentStackLevel;
    }
}

void StackLevelSetter::PopArg(GenTreePutArgStk* putArgStk)
{
    assert(currentStackLevel >= putArgStk->GetSlotCount());
    currentStackLevel -= putArgStk->GetSlotCount();
}

PhaseStatus Compiler::phSetThrowHelperBlockStackLevel()
{
    StackLevelSetter stackLevelSetter(this);
    stackLevelSetter.Run();
    return PhaseStatus::MODIFIED_NOTHING;
}

#endif // !FEATURE_FIXED_OUT_ARGS
