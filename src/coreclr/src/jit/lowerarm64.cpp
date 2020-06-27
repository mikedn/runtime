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

bool CanEncodeBitmaskImm(ssize_t imm, emitAttr size, unsigned* bitmaskImm)
{
    emitter::bitMaskImm bimm;
    bool                encoded = emitter::canEncodeBitMaskImm(imm, size, &bimm);
    *bitmaskImm                 = bimm.immNRS;
    return encoded;
}

void Lowering::LowerLogical(GenTreeOp* logical)
{
    assert(logical->OperIs(GT_AND, GT_OR, GT_XOR));

    GenTree* op1 = logical->GetOp(0);
    GenTree* op2 = logical->GetOp(1);

    LIR::Use use;
    if (logical->OperIs(GT_AND) && BlockRange().TryGetUse(logical, &use))
    {
        if (use.User()->OperIs(GT_EQ, GT_NE))
        {
            // Don't lower EQ|NE(AND(x, y), imm) for now, it is recognized by OptimizeConstCompare.
            ContainCheckBinary(logical);
            return;
        }

        if (use.User()->OperIs(GT_LSH, GT_RSH, GT_RSZ) && (use.User()->AsOp()->GetOp(1) == logical) && op2->IsIntCon())
        {
            // Don't lower shift(x, AND(y, imm)) for now, it is recognized by LowerShift.
            ContainCheckBinary(logical);
            return;
        }
    }

    instruction ins;

    switch (logical->GetOper())
    {
        case GT_AND:
            ins = INS_and;
            break;
        case GT_OR:
            ins = INS_orr;
            break;
        default:
            ins = INS_eor;
            break;
    }

    logical->ChangeOper(GT_INSTR);

    GenTreeInstr* instr = logical->AsInstr();
    instr->SetIns(ins);

    assert((instr->GetAttr() == EA_4BYTE) || (instr->GetAttr() == EA_8BYTE));

    unsigned encodedBitmaskImm;

    if (op2->IsIntCon() && CanEncodeBitmaskImm(op2->AsIntCon()->GetValue(), instr->GetAttr(), &encodedBitmaskImm))
    {
        assert(!op2->AsIntCon()->ImmedValNeedsReloc(comp));

        instr->SetNumOps(1);
        instr->SetImmediate(encodedBitmaskImm);
        instr->SetOp(0, op1);

        BlockRange().Remove(op2);
    }
    else
    {
        instr->SetNumOps(2);
        instr->SetImmediate(0);
        instr->SetOp(0, op1);
        instr->SetOp(1, op2);
    }
}

void Lowering::LowerNeg(GenTreeUnOp* neg)
{
    assert(neg->OperIs(GT_NEG));

    GenTree* op1 = neg->GetOp(0);

    instruction ins;

    if (varTypeIsFloating(neg->GetType()))
    {
        ins = INS_fneg;
    }
    else
    {
        ins = INS_neg;
    }

    neg->ChangeOper(GT_INSTR);

    GenTreeInstr* instr = neg->AsInstr();
    instr->SetIns(ins);
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
