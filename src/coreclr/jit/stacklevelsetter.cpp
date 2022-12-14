// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "stacklevelsetter.h"

StackLevelSetter::StackLevelSetter(Compiler* compiler)
    : Phase(compiler, PHASE_STACK_LEVEL_SETTER)
#if !FEATURE_FIXED_OUT_ARGS
    , framePointerRequired(compiler->opts.IsFramePointerRequired())
    , throwHelperBlocksUsed(comp->fgUseThrowHelperBlocks() && comp->compUsesThrowHelper)
#endif
{
}

//------------------------------------------------------------------------
// DoPhase: Calculate stack slots numbers for outgoing args.
//
// Returns:
//   PhaseStatus indicating what, if anything, was changed.
//
// Notes:
//   For non-x86 platforms it calculates the max number of slots
//   that calls inside this method can push on the stack.
//   This value is used for sanity checks in the emitter.
//
//   Stack slots are pointer-sized: 4 bytes for 32-bit platforms, 8 bytes for 64-bit platforms.
//
//   For x86 it also sets throw-helper blocks incoming stack depth and set
//   framePointerRequired when it is necessary. These values are used to pop
//   pushed args when an exception occurs.
//
PhaseStatus StackLevelSetter::DoPhase()
{
    for (BasicBlock* const block : comp->Blocks())
    {
        ProcessBlock(block);
    }

#ifdef TARGET_X86
    // Profiler hook calls do not appear in the IR and do push an argument.
    if (comp->compIsProfilerHookNeeded() && (maxStackLevel == 0))
    {
        maxStackLevel = 1;
    }
#endif

    // TODO-MIKE-Review: What does "maxStackLevel >= 4" has to do with x64 or any other non-x86
    // architectures? This condition does reduce code size but it appears to do so by accident:
    // EBP based address modes have smaller encoding than ESP based ones but then this basically
    // counts arg stores and those always use ESP. What we really need is the number of non-arg
    // stack references that exist, and this has nothing to do with that.

    if (maxStackLevel >= 4
#if !FEATURE_FIXED_OUT_ARGS
        || framePointerRequired
#endif
        )
    {
        comp->opts.SetFramePointerRequired();
    }

#ifdef JIT32_GCENCODER
    // The GC encoding for fully interruptible methods does not
    // support more than 1023 pushed arguments, so we have to
    // use a partially interruptible GC info/encoding.
    if (maxStackLevel >= MAX_PTRARG_OFS)
    {
        JITDUMP("Too many pushed arguments for fully interruptible encoding, marking method as partially "
                "interruptible\n");

        comp->codeGen->SetInterruptible(false);
    }
#endif

    return PhaseStatus::MODIFIED_NOTHING;
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
    // TODO-MIKE-Cleanup: Investigate why we need to run StackLevelSetter at all in the
    // FEATURE_FIXED_OUT_ARGS case. It should not be needed since the stack level doesn't
    // change.

    assert(currentStackLevel == 0);
    for (GenTree* node : LIR::AsRange(block))
    {
        if (GenTreePutArgStk* putArgStk = node->IsPutArgStk())
        {
            PushArg(putArgStk);
            continue;
        }

#if !FEATURE_FIXED_OUT_ARGS
        // Set throw blocks incoming stack depth for x86.
        if (throwHelperBlocksUsed && !framePointerRequired)
        {
            if (node->OperMayThrow(comp))
            {
                SetThrowHelperBlocks(node, block);
            }
        }
#endif // !FEATURE_FIXED_OUT_ARGS

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

#if !FEATURE_FIXED_OUT_ARGS
//------------------------------------------------------------------------
// SetThrowHelperBlocks: Set throw helper blocks incoming stack levels targeted
//                       from the node.
//
// Notes:
//   one node can target several helper blocks, but not all operands that throw do this.
//   So the function can set 0-2 throw blocks depends on oper and overflow flag.
//
// Arguments:
//   node - the node to process;
//   block - the source block for the node.
void StackLevelSetter::SetThrowHelperBlocks(GenTree* node, BasicBlock* block)
{
    assert(node->OperMayThrow(comp));

    // Check that it uses throw block, find its kind, find the block, set level.
    switch (node->OperGet())
    {
        case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
        case GT_HW_INTRINSIC_CHK:
#endif
            SetThrowHelperBlock(node->AsBoundsChk()->GetThrowKind(), block);
            break;

        case GT_INDEX_ADDR:
        case GT_ARR_ELEM:
        case GT_ARR_INDEX:
        {
            SetThrowHelperBlock(SCK_RNGCHK_FAIL, block);
        }
        break;

        case GT_CKFINITE:
        {
            SetThrowHelperBlock(SCK_ARITH_EXCPN, block);
        }
        break;
        default: // Other opers can target throw only due to overflow.
            break;
    }
    if (node->gtOverflowEx())
    {
        SetThrowHelperBlock(SCK_OVERFLOW, block);
    }
}

//------------------------------------------------------------------------
// SetThrowHelperBlock: Set throw helper block incoming stack levels targeted
//                      from the block with this kind.
//
// Notes:
//   Set framePointerRequired if finds that the block has several incoming edges
//   with different stack levels.
//
// Arguments:
//   kind - the special throw-helper kind;
//   block - the source block that targets helper.
void StackLevelSetter::SetThrowHelperBlock(SpecialCodeKind kind, BasicBlock* block)
{
    Compiler::AddCodeDsc* add = comp->fgFindExcptnTarget(kind, comp->bbThrowIndex(block));
    assert(add != nullptr);
    if (add->acdStkLvlInit)
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
        // we morph a tree that calls fgAddCodeRef(), so the stack depth
        // number will be incorrect. For now, simply force all functions with
        // these helpers to have EBP frames. It might be possible to make
        // this less conservative. E.g., for top-level (not nested) calls
        // without stack args, the stack pointer hasn't changed and stack
        // depth will be known to be zero. Or, figure out a way to update
        // or generate all required helpers after all stack alignment
        // has been added, and the stack level at each call to fgAddCodeRef()
        // is known, or can be recalculated.
        CLANG_FORMAT_COMMENT_ANCHOR;

#ifndef UNIX_X86_ABI
        if (add->acdStkLvl != currentStackLevel)
#endif
        {
            framePointerRequired = true;
        }
    }
    else
    {
        add->acdStkLvlInit = true;
        add->acdStkLvl     = currentStackLevel;

        INDEBUG(add->acdDstBlk->bbTgtStkDepth = currentStackLevel);
    }
}

#endif // !FEATURE_FIXED_OUT_ARGS

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
