// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#include "jitpch.h"
#include "lower.h"

#ifdef TARGET_ARM64 // This file is ONLY used for ARM64 architectures

void Lowering::LowerNot(GenTreeUnOp* not)
{
    assert(not->OperIs(GT_NOT));

    GenTree* op1 = not->GetOp(0);

    not->ChangeOper(GT_INSTR);

    GenTreeInstr* instr = not->AsInstr();
    instr->SetIns(INS_mvn);
    instr->SetImmediate(0);
    instr->SetNumOps(1);
    instr->SetOp(0, op1);
}

void Lowering::LowerMultiply(GenTreeOp* mul)
{
    assert(mul->OperIs(GT_MUL, GT_MULHI));

    if (mul->OperIs(GT_MUL) && mul->gtOverflow())
    {
        return;
    }

    GenTree* op1 = mul->GetOp(0);
    GenTree* op2 = mul->GetOp(1);

    if (mul->OperIs(GT_MULHI) && !op1->TypeIs(TYP_LONG))
    {
        assert(mul->TypeIs(TYP_INT));
        assert(varActualType(op1->GetType()) == TYP_INT);
        assert(varActualType(op2->GetType()) == TYP_INT);

        // TODO-MIKE-Cleanup: Magic division should not produce such a MULHI node. There's no corresponding ARM64
        // instruction for this operation and we need to insert a right shift to obtain the "hi" 32 bits of the
        // long result. And in some cases magic division follows up with another right shift that currently doesn't
        // combine with this one.

        GenTreeInstr* mull =
            new (comp, GT_INSTR) GenTreeInstr(TYP_LONG, mul->IsUnsigned() ? INS_umull : INS_smull, op1, op2);
        BlockRange().InsertBefore(mul, mull);

        mul->ChangeOper(GT_INSTR);

        GenTreeInstr* instr = mul->AsInstr();
        instr->SetIns(mul->IsUnsigned() ? INS_lsr : INS_asr, EA_8BYTE);
        instr->SetImmediate(32);
        instr->SetNumOps(1);
        instr->SetOp(0, mull);

        return;
    }

    instruction ins;

    if (varTypeIsFloating(mul->GetType()))
    {
        ins = INS_fmul;
    }
    else if (mul->OperIs(GT_MULHI))
    {
        assert(mul->TypeIs(TYP_LONG));

        ins = mul->IsUnsigned() ? INS_umulh : INS_smulh;
    }
    else
    {
        ins = INS_mul;
    }

    mul->ChangeOper(GT_INSTR);

    GenTreeInstr* instr = mul->AsInstr();
    instr->SetIns(ins);
    instr->SetImmediate(0);
    instr->SetNumOps(2);
    instr->SetOp(0, op1);
    instr->SetOp(1, op2);
}

#endif // TARGET_ARM64
