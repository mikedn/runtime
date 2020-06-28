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

ssize_t DecodeBitmaskImm(unsigned encoded, emitAttr size);

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

void Lowering::LowerShift(GenTreeOp* shift)
{
    assert(shift->OperIs(GT_LSH, GT_RSH, GT_RSZ));

    if (shift->GetOp(1)->IsIntCon())
    {
        LowerShiftImmediate(shift);
    }
    else
    {
        LowerShiftVariable(shift);
    }
}

void Lowering::LowerShiftVariable(GenTreeOp* shift)
{
    GenTree* op1 = shift->GetOp(0);
    GenTree* op2 = shift->GetOp(1);

    // ARM64 shift instructions mask the shift count to 5 bits (or 6 bits for 64 bit operations).
    // TODO-MIKE-Cleanup: This really belongs in morph. The problem is ensuring that various JIT
    // parts (constant folding, VN etc.) handle shifts according to target specifics (ARM32 masks
    // only 8 bits thus (i32 << 32) is 0 on ARM32 and i32 on ARM64).
    // TODO-MIKE-CQ: And of course, any narrowing casts are redundant too, morph doesn't figure
    // that out either.

    while (GenTreeInstr* instr = op2->IsInstr())
    {
        if ((instr->GetNumOps() != 1) || (instr->GetIns() != INS_and))
        {
            break;
        }

        ssize_t shiftImmMask = varTypeIsLong(shift->GetType()) ? 63 : 31;
        ssize_t andImm       = DecodeBitmaskImm(instr->GetImmediate(), instr->GetAttr());

        if ((andImm & shiftImmMask) != shiftImmMask)
        {
            break;
        }

        BlockRange().Remove(instr);
        op2 = instr->GetOp(0);
    }

    instruction ins;

    switch (shift->GetOper())
    {
        case GT_LSH:
            // Use "v" instructions for convenience - when attempting to use shifted register forms
            // one needs to check for LSL/LSR/ASR instructions and assume they're immediate shifts.
            ins = INS_lslv;
            break;
        case GT_RSH:
            ins = INS_asrv;
            break;
        default:
            ins = INS_lsrv;
            break;
    }

    shift->ChangeOper(GT_INSTR);

    GenTreeInstr* instr = shift->AsInstr();
    instr->SetIns(ins);
    instr->SetNumOps(2);
    instr->SetOp(0, op1);
    instr->SetOp(1, op2);
}

void Lowering::LowerShiftImmediate(GenTreeOp* shift)
{
    GenTree* op1 = shift->GetOp(0);
    GenTree* op2 = shift->GetOp(1);

    ssize_t  shiftByMask = varTypeIsLong(shift->GetType()) ? 63 : 31;
    unsigned shiftByBits = static_cast<unsigned>(op2->AsIntCon()->GetValue() & shiftByMask);

    if ((shiftByBits >= 24) && shift->OperIs(GT_LSH) && comp->opts.OptimizationEnabled())
    {
        // Remove source casts if the shift discards the produced sign/zero bits.
        //
        // Some of this would probably be better done during morph or some sort
        // of tree narrowing phase. The problem is that this removes INT to LONG
        // casts, transforming
        //     LSH.long(CAST.long(x.int), 32)
        // into
        //     LSH.long(x.int, 32)
        //
        // While there's nothing intrinsically wrong about having a node with
        // different source and destination types, it is possible that some
        // frontend phases might get confused by such a shift node.

        unsigned consumedBits = varTypeBitSize(shift->GetType());

        assert((consumedBits == 32) || (consumedBits == 64));
        assert(shiftByBits < consumedBits);

        consumedBits -= shiftByBits;

        while (GenTreeCast* cast = op1->IsCast())
        {
            if (cast->gtOverflow() || !varTypeIsIntegral(cast->GetOp(0)->GetType()))
            {
                break;
            }

            var_types castType = cast->GetCastType();

            // A (U)LONG - (U)LONG cast would normally produce 64 bits but since it
            // has no effect we make it produce 32 bits to keep the check simple.
            // Anyway such a cast should have been removed earlier.
            unsigned producedBits = varTypeIsSmall(castType) ? varTypeBitSize(castType) : 32;

            if (consumedBits > producedBits)
            {
                break;
            }

            JITDUMP("Removing CAST [%06d] producing %u bits from LSH [%06d] consuming %u bits\n", cast->gtTreeID,
                    producedBits, shift->gtTreeID, consumedBits);

            BlockRange().Remove(op1);
            op1 = cast->GetOp(0);

            // The CAST operand may be contained but shift doesn't allow contained operands.
            op1->ClearContained();
        }
    }

    instruction ins;

    switch (shift->GetOper())
    {
        case GT_LSH:
            ins = INS_lsl;
            break;
        case GT_RSH:
            ins = INS_asr;
            break;
        default:
            ins = INS_lsr;
            break;
    }

    shift->ChangeOper(GT_INSTR);

    GenTreeInstr* instr = shift->AsInstr();
    instr->SetIns(ins);
    instr->SetImmediate(shiftByBits);
    instr->SetNumOps(1);
    instr->SetOp(0, op1);

    BlockRange().Remove(op2);
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

bool CanEncodeArithmeticImm(ssize_t imm, emitAttr size, unsigned* encodedArithImm)
{
    bool encoded     = emitter::emitIns_valid_imm_for_add(imm, size);
    *encodedArithImm = static_cast<unsigned>(imm) & 0xFFF'FFF;
    return encoded;
}

void Lowering::LowerArithmetic(GenTreeOp* arith)
{
    assert(arith->OperIs(GT_ADD, GT_SUB));

    if (arith->gtOverflow())
    {
        ContainCheckBinary(arith);
        return;
    }

    GenTree* op1 = arith->GetOp(0);
    GenTree* op2 = arith->GetOp(1);

    LIR::Use use;
    if (!varTypeIsFloating(arith->GetType()) && BlockRange().TryGetUse(arith, &use))
    {
        if (use.User()->OperIs(GT_EQ, GT_NE) && use.User()->AsOp()->GetOp(1)->IsIntCon())
        {
            // Don't lower EQ|NE(ADD|SUB(x, y), imm) for now, it is recognized by OptimizeConstCompare.
            ContainCheckBinary(arith);
            return;
        }

        if (use.User()->IsIndir() && use.User()->AsIndir()->GetAddr() == arith)
        {
            // Don't lower indir(ADD|SUB(x, y)) for now, it is recognized by ContainCheckIndir & co.
            ContainCheckBinary(arith);
            return;
        }
    }

    instruction ins;

    if (varTypeIsFloating(arith->GetType()))
    {
        ins = arith->OperIs(GT_ADD) ? INS_fadd : INS_fsub;
    }
    else
    {
        ins = arith->OperIs(GT_ADD) ? INS_add : INS_sub;
    }

    arith->ChangeOper(GT_INSTR);

    GenTreeInstr* instr = arith->AsInstr();

    if (op2->IsIntCon() && !op2->AsIntCon()->ImmedValNeedsReloc(comp))
    {
        ssize_t  imm = op2->AsIntCon()->GetValue();
        unsigned encodedArithImm;

        if (CanEncodeArithmeticImm(abs(imm), emitActualTypeSize(instr->GetType()), &encodedArithImm))
        {
            if (imm < 0)
            {
                ins = (ins == INS_add) ? INS_sub : INS_add;
            }

            instr->SetIns(ins);
            instr->SetImmediate(encodedArithImm);
            instr->SetNumOps(1);
            instr->SetOp(0, op1);

            BlockRange().Remove(op2);

            return;
        }
    }

    instr->SetIns(ins);
    instr->SetImmediate(0);
    instr->SetNumOps(2);
    instr->SetOp(0, op1);
    instr->SetOp(1, op2);
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
