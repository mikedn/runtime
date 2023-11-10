// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "codegen.h"
#include "instr.h"
#include "emit.h"

void CodeGen::instGen(instruction ins)
{
    GetEmitter()->emitIns(ins);

#ifdef TARGET_XARCH
#ifdef PSEUDORANDOM_NOP_INSERTION
    // A workaround necessitated by limitations of emitter
    // if we are scheduled to insert a nop here, we have to delay it
    // hopefully we have not missed any other prefix instructions or places
    // they could be inserted
    if (ins == INS_lock && GetEmitter()->emitNextNop == 0)
    {
        GetEmitter()->emitNextNop = 1;
    }
#endif // PSEUDORANDOM_NOP_INSERTION
#endif
}

void CodeGen::inst_JMP(emitJumpKind jmp, BasicBlock* tgtBlock)
{
#if !FEATURE_FIXED_OUT_ARGS
    // On the x86 we are pushing (and changing the stack level), but on x64 and other archs we have
    // a fixed outgoing args area that we store into and we never change the stack level when calling methods.
    //
    // Thus only on x86 do we need to assert that the stack level at the target block matches the current stack level.
    //
    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef UNIX_X86_ABI
    // bbTgtStkDepth is a (pure) argument count (stack alignment padding should be excluded).
    assert((tgtBlock->bbTgtStkDepth * sizeof(int) == (genStackLevel - curNestedAlignment)) || isFramePointerUsed());
#else
    assert((tgtBlock->bbTgtStkDepth * sizeof(int) == genStackLevel) || isFramePointerUsed());
#endif
#endif // !FEATURE_FIXED_OUT_ARGS

    GetEmitter()->emitIns_J(emitter::emitJumpKindToIns(jmp), tgtBlock);
}

void CodeGen::inst_Mov(var_types dstType, regNumber dstReg, regNumber srcReg, bool canSkip)
{
    GetEmitter()->emitIns_Mov(ins_Copy(srcReg, dstType), emitActualTypeSize(dstType), dstReg, srcReg, canSkip);
}

bool CodeGen::IsLocalMemoryOperand(GenTree* op, unsigned* lclNum, unsigned* lclOffs)
{
    if (op->isUsedFromSpillTemp())
    {
        assert(op->IsRegOptional());
        assert(op->IsRegSpilled(0));

        SpillTemp* temp = spillTemps.UseSpillTemp(op, 0);

        *lclNum  = temp->GetNum();
        *lclOffs = 0;

        return true;
    }

    assert(op->isContained());

    if (op->OperIs(GT_LCL_FLD))
    {
        *lclNum  = op->AsLclFld()->GetLclNum();
        *lclOffs = op->AsLclFld()->GetLclOffs();

        return true;
    }

    if (op->OperIs(GT_LCL_VAR))
    {
        assert(op->IsRegOptional() || !compiler->lvaGetDesc(op->AsLclVar())->IsRegCandidate());

        *lclNum  = op->AsLclVar()->GetLclNum();
        *lclOffs = 0;

        return true;
    }

    return false;
}
