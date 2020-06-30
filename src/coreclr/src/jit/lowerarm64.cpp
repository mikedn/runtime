// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#include "jitpch.h"
#include "lower.h"

#ifdef TARGET_ARM64 // This file is ONLY used for ARM64 architectures

GenTreeInstr* IsInstr(GenTree* node, emitAttr size, instruction ins)
{
    if (node->IsInstr() && (node->AsInstr()->GetAttr() == size) && (node->AsInstr()->GetIns() == ins))
    {
        return node->AsInstr();
    }

    return nullptr;
}

insOpts GetEquivalentShiftOptionLogical(GenTree* node, emitAttr size)
{
    if (node->IsInstr() && (node->AsInstr()->GetAttr() == size))
    {
        switch (node->AsInstr()->GetIns())
        {
            case INS_lsl:
                return INS_OPTS_LSL;
            case INS_lsr:
                return INS_OPTS_LSR;
            case INS_asr:
                return INS_OPTS_ASR;
            case INS_ror:
                return INS_OPTS_ROR;
            default:
                break;
        }
    }

    return INS_OPTS_NONE;
}

void Lowering::LowerNot(GenTreeUnOp* not)
{
    assert(not->OperIs(GT_NOT));

    GenTree* op1 = not->GetOp(0);

    emitAttr size = emitActualTypeSize(not->GetType());
    unsigned imm  = 0;
    insOpts  opt  = GetEquivalentShiftOptionLogical(op1, size);

    if (opt != INS_OPTS_NONE)
    {
        assert(op1->AsInstr()->GetNumOps() == 1);

        BlockRange().Remove(op1);
        imm = op1->AsInstr()->GetImmediate();
        op1 = op1->AsInstr()->GetOp(0);
    }

    not->ChangeOper(GT_INSTR);

    GenTreeInstr* instr = not->AsInstr();
    instr->SetIns(INS_mvn, size, opt);
    instr->SetImmediate(imm);
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

    emitAttr size = emitActualTypeSize(logical->GetType());
    unsigned encodedBitmaskImm;

    if (op2->IsIntCon() && CanEncodeBitmaskImm(op2->AsIntCon()->GetValue(), size, &encodedBitmaskImm))
    {
        assert(!op2->AsIntCon()->ImmedValNeedsReloc(comp));

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
        instr->SetNumOps(1);
        instr->SetImmediate(encodedBitmaskImm);
        instr->SetOp(0, op1);

        BlockRange().Remove(op2);

        return;
    }

    unsigned      imm   = 0;
    insOpts       opt   = INS_OPTS_NONE;
    GenTreeInstr* mvn   = nullptr;
    GenTreeInstr* shift = nullptr;

    if ((mvn = IsInstr(op2, size, INS_mvn)) != nullptr)
    {
        op2 = mvn->GetOp(0);
        imm = mvn->GetImmediate();
        opt = mvn->GetOption();

        BlockRange().Remove(mvn);
    }
    else if ((mvn = IsInstr(op1, size, INS_mvn)) != nullptr)
    {
        op1 = mvn->GetOp(0);
        imm = mvn->GetImmediate();
        opt = mvn->GetOption();

        std::swap(op1, op2);

        BlockRange().Remove(mvn);
    }
    else if ((opt = GetEquivalentShiftOptionLogical(op2, size)) != INS_OPTS_NONE)
    {
        assert(op2->AsInstr()->GetNumOps() == 1);

        shift = op2->AsInstr();
        op2   = shift->GetOp(0);
        imm   = shift->GetImmediate();

        BlockRange().Remove(shift);
    }
    else if ((opt = GetEquivalentShiftOptionLogical(op1, size)) != INS_OPTS_NONE)
    {
        assert(op1->AsInstr()->GetNumOps() == 1);

        shift = op1->AsInstr();
        op1   = shift->GetOp(0);
        imm   = shift->GetImmediate();

        std::swap(op1, op2);

        BlockRange().Remove(shift);
    }

    instruction ins;

    switch (logical->GetOper())
    {
        case GT_AND:
            ins = mvn == nullptr ? INS_and : INS_bic;
            break;
        case GT_OR:
            ins = mvn == nullptr ? INS_orr : INS_orn;
            break;
        default:
            ins = mvn == nullptr ? INS_eor : INS_eon;
            break;
    }

    logical->ChangeOper(GT_INSTR);

    GenTreeInstr* instr = logical->AsInstr();
    instr->SetIns(ins, size, opt);
    instr->SetNumOps(2);
    instr->SetImmediate(imm);
    instr->SetOp(0, op1);
    instr->SetOp(1, op2);
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

    emitAttr size        = emitActualTypeSize(shift->GetType());
    ssize_t  shiftByMask = (size == EA_8BYTE) ? 63 : 31;
    unsigned shiftByBits = static_cast<unsigned>(op2->AsIntCon()->GetValue() & shiftByMask);

    if (op1->IsCast() && !op1->gtOverflow() && varTypeIsIntegral(op1->AsCast()->GetOp(0)->GetType()))
    {
        // Shift instructions do not have an "extending form" like arithmetic instructions but the
        // same operation can be performed using bitfield insertion/extraction instructions.
        // For example:
        //    - SXTB x0, w0 and LSL x0, x0, #12 <=> SBFIZ x0, x0, #12, #8
        //    - SXTW x0, w0 and ASR x0, x0, #12 <=> SBFX x0, x0, #12, #20

        GenTreeCast* cast = op1->AsCast();

        unsigned bitFieldWidth = 0;
        bool     isUnsigned    = false;

        if (varTypeIsSmall(cast->GetCastType()))
        {
            // Currently the JIT IR doesn't allow direct extension from small int types to long.
            // This code likely works fine with such casts but it cannot be tested.
            assert(varActualType(cast->GetType()) == TYP_INT);
            assert(size == EA_4BYTE);

            bitFieldWidth = varTypeBitSize(cast->GetCastType());
            isUnsigned    = varTypeIsUnsigned(cast->GetCastType());
        }
        else if (varTypeIsLong(cast->GetCastType()) && !varTypeIsLong(cast->GetOp(0)->GetType()))
        {
            assert(size == EA_8BYTE);

            bitFieldWidth = 32;
            isUnsigned    = cast->IsUnsigned();
        }

        if (bitFieldWidth != 0)
        {
            unsigned    regWidth = size * 8;
            instruction ins      = INS_none;
            unsigned    lsb      = 0;
            unsigned    width    = 0;

            if (shift->OperIs(GT_LSH))
            {
                // We don't need to insert if all the extension bits produced by the cast are discarded
                // by the shift, we'll just generate a LSL in that case.

                if (shiftByBits + bitFieldWidth < regWidth)
                {
                    ins   = isUnsigned ? INS_ubfiz : INS_sbfiz;
                    lsb   = shiftByBits;
                    width = bitFieldWidth;
                }
            }
            else if (shift->OperIs(GT_RSH))
            {
                ins = isUnsigned ? INS_ubfx : INS_sbfx;

                if (bitFieldWidth > shiftByBits)
                {
                    // We still have some bits from the casted value that need to be extracted. Extraction
                    // needs the width of the extracted bitfield, which is smaller than the width of the
                    // casted value.

                    lsb   = shiftByBits;
                    width = bitFieldWidth - shiftByBits;
                }
                else
                {
                    // All the bits of the casted value are discarded by the shift. The only thing that's
                    // left are the extension bits that the cast produced. These bits can be obtained by
                    // extracting the sign bit from the casted value.

                    lsb   = bitFieldWidth - 1;
                    width = 1;
                }
            }
            else
            {
                assert(shift->OperIs(GT_RSZ));

                // The cast has to be unsigned because RSZ will only insert 0 bits. If the cast was signed
                // then we would end up with some zero bits produced by the shift, followed by some sign
                // bits produced by the cast. Such a mix cannot be reproduced using UBFX/SBFX instructions.

                // Unlike in the RSH case, if all bits are discarded by the shift we'd get 0. We could try
                // to replace the shift with constant 0 but it seems unlikely to be worth the trouble.
                // Besides, there's no reason not to do this in morph.

                if (isUnsigned && (bitFieldWidth > shiftByBits))
                {
                    ins = INS_ubfx;

                    lsb   = shiftByBits;
                    width = bitFieldWidth - shiftByBits;
                }
            }

            if (ins != INS_none)
            {
                assert((0 <= lsb) && (lsb < regWidth));
                assert((1 <= width) && (width <= regWidth - lsb));

                op1 = cast->GetOp(0);
                op1->ClearContained();

                shift->ChangeOper(GT_INSTR);

                GenTreeInstr* instr = shift->AsInstr();
                instr->SetIns(ins, size);
                instr->SetImmediate((ins == INS_lsr) ? lsb : ((lsb << 6) | width));
                instr->SetNumOps(1);
                instr->SetOp(0, op1);

                BlockRange().Remove(cast);
                BlockRange().Remove(op2);

                return;
            }
        }
    }

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

insOpts GetEquivalentShiftOptionArithmetic(GenTree* node, emitAttr size)
{
    if (node->IsInstr() && (node->AsInstr()->GetAttr() == size))
    {
        switch (node->AsInstr()->GetIns())
        {
            case INS_lsl:
                return INS_OPTS_LSL;
            case INS_lsr:
                return INS_OPTS_LSR;
            case INS_asr:
                return INS_OPTS_ASR;
            default:
                break;
        }
    }

    return INS_OPTS_NONE;
}

void Lowering::LowerNeg(GenTreeUnOp* neg)
{
    assert(neg->OperIs(GT_NEG));

    GenTree* op1 = neg->GetOp(0);

    if (varTypeIsFloating(neg->GetType()))
    {
        neg->ChangeOper(GT_INSTR);

        GenTreeInstr* instr = neg->AsInstr();
        instr->SetIns(INS_fneg);
        instr->SetImmediate(0);
        instr->SetNumOps(1);
        instr->SetOp(0, op1);

        return;
    }

    emitAttr size = emitActualTypeSize(neg->GetType());

    if (GenTreeInstr* mul = IsInstr(op1, size, INS_mul))
    {
        neg->ChangeOper(GT_INSTR);

        GenTreeInstr* instr = neg->AsInstr();
        instr->SetIns(INS_mneg);
        instr->SetImmediate(0);
        instr->SetNumOps(2);
        instr->SetOp(0, mul->GetOp(0));
        instr->SetOp(1, mul->GetOp(1));

        BlockRange().Remove(mul);

        return;
    }

    insOpts       opt   = INS_OPTS_NONE;
    unsigned      imm   = 0;
    GenTreeInstr* shift = nullptr;

    if ((opt = GetEquivalentShiftOptionArithmetic(op1, size)) != INS_OPTS_NONE)
    {
        assert(op1->AsInstr()->GetNumOps() == 1);

        shift = op1->AsInstr();
        op1   = shift->GetOp(0);
        imm   = shift->GetImmediate();

        BlockRange().Remove(shift);
    }

    neg->ChangeOper(GT_INSTR);

    GenTreeInstr* instr = neg->AsInstr();
    instr->SetIns(INS_neg, size, opt);
    instr->SetImmediate(imm);
    instr->SetNumOps(1);
    instr->SetOp(0, op1);
}

bool CanEncodeArithmeticImm(ssize_t imm, emitAttr size, unsigned* encodedArithImm)
{
    bool encoded     = emitter::emitIns_valid_imm_for_add(imm, size);
    *encodedArithImm = static_cast<unsigned>(imm) & 0xFFF'FFF;
    return encoded;
}

insOpts GetEquivalentExtendOption(GenTree* node, emitAttr size)
{
    if (node->IsCast() && !node->gtOverflow() && varTypeIsIntegral(node->AsCast()->GetOp(0)->GetType()))
    {
        switch (node->AsCast()->GetCastType())
        {
            case TYP_BOOL:
            case TYP_UBYTE:
                return INS_OPTS_UXTB;
            case TYP_BYTE:
                return INS_OPTS_SXTB;
            case TYP_USHORT:
                return INS_OPTS_UXTH;
            case TYP_SHORT:
                return INS_OPTS_SXTH;
            case TYP_ULONG:
            case TYP_LONG:
                if (!varTypeIsLong(node->AsCast()->GetOp(0)->GetType()))
                {
                    return node->IsUnsigned() ? INS_OPTS_UXTW : INS_OPTS_SXTW;
                }
                break;
            default:
                break;
        }
    }

    return INS_OPTS_NONE;
}

instruction GetMultiplyAddInstruction(GenTree* multiply, instruction ins, emitAttr size)
{
    if (multiply->IsInstr() && (multiply->AsInstr()->GetAttr() == size))
    {
        switch (multiply->AsInstr()->GetIns())
        {
            case INS_mul:
                return (ins == INS_add) ? INS_madd : INS_msub;
            case INS_smull:
                return (ins == INS_add) ? INS_smaddl : INS_smsubl;
            case INS_umull:
                return (ins == INS_add) ? INS_umaddl : INS_umsubl;
            default:
                break;
        }
    }

    return INS_none;
}

void Lowering::LowerArithmetic(GenTreeOp* arith)
{
    assert(arith->OperIs(GT_ADD, GT_SUB));

    GenTree* op1 = arith->GetOp(0);
    GenTree* op2 = arith->GetOp(1);

    if (varTypeIsFloating(arith->GetType()))
    {
        arith->ChangeOper(GT_INSTR);

        GenTreeInstr* instr = arith->AsInstr();
        instr->SetIns(arith->OperIs(GT_ADD) ? INS_fadd : INS_fsub);
        instr->SetImmediate(0);
        instr->SetNumOps(2);
        instr->SetOp(0, op1);
        instr->SetOp(1, op2);

        return;
    }

    if (arith->gtOverflow())
    {
        ContainCheckBinary(arith);
        return;
    }

    LIR::Use use;
    if (BlockRange().TryGetUse(arith, &use) && use.User()->IsIndir() && (use.User()->AsIndir()->GetAddr() == arith))
    {
        // Don't lower indir(ADD|SUB(x, y)) for now, it is recognized by ContainCheckIndir & co.
        ContainCheckBinary(arith);
        return;
    }

    emitAttr size = emitActualTypeSize(arith->GetType());

    if (op2->IsIntCon() && !op2->AsIntCon()->ImmedValNeedsReloc(comp))
    {
        ssize_t  imm = op2->AsIntCon()->GetValue();
        unsigned encodedArithImm;

        if (CanEncodeArithmeticImm(abs(imm), size, &encodedArithImm))
        {
            instruction ins;

            if (imm >= 0)
            {
                ins = arith->OperIs(GT_ADD) ? INS_add : INS_sub;
            }
            else
            {
                ins = arith->OperIs(GT_ADD) ? INS_sub : INS_add;
            }

            arith->ChangeOper(GT_INSTR);

            GenTreeInstr* instr = arith->AsInstr();
            instr->SetIns(ins);
            instr->SetImmediate(encodedArithImm);
            instr->SetNumOps(1);
            instr->SetOp(0, op1);

            BlockRange().Remove(op2);

            return;
        }
    }

    instruction   ins   = arith->OperIs(GT_ADD) ? INS_add : INS_sub;
    insOpts       opt   = INS_OPTS_NONE;
    unsigned      imm   = 0;
    GenTreeCast*  cast  = nullptr;
    GenTreeInstr* shift = nullptr;
    GenTreeInstr* mul   = nullptr;
    instruction   mins  = INS_none;

    if ((opt = GetEquivalentExtendOption(op2, size)) != INS_OPTS_NONE)
    {
        cast = op2->AsCast();
        op2  = cast->GetOp(0);
        op2->ClearContained();

        BlockRange().Remove(cast);
    }
    else if ((ins == INS_add) && (opt = GetEquivalentExtendOption(op1, size)) != INS_OPTS_NONE)
    {
        cast = op1->AsCast();
        op1  = cast->GetOp(0);
        op1->ClearContained();

        BlockRange().Remove(cast);
    }
    else if ((opt = GetEquivalentShiftOptionArithmetic(op2, size)) != INS_OPTS_NONE)
    {
        assert(op2->AsInstr()->GetNumOps() == 1);

        shift = op2->AsInstr();
        op2   = shift->GetOp(0);
        imm   = shift->GetImmediate();

        BlockRange().Remove(shift);

        if ((opt == INS_OPTS_LSL) && (imm <= 4))
        {
            insOpts extendOption = GetEquivalentExtendOption(op2, size);

            if (extendOption != INS_OPTS_NONE)
            {
                cast = op2->AsCast();
                op2  = cast->GetOp(0);
                op2->ClearContained();
                opt = extendOption;

                BlockRange().Remove(cast);
            }
        }
    }
    else if ((ins == INS_add) && (opt = GetEquivalentShiftOptionArithmetic(op1, size)) != INS_OPTS_NONE)
    {
        assert(op1->AsInstr()->GetNumOps() == 1);

        shift = op1->AsInstr();
        op1   = shift->GetOp(0);
        imm   = shift->GetImmediate();

        std::swap(op1, op2);

        BlockRange().Remove(shift);

        if ((opt == INS_OPTS_LSL) && (imm <= 4))
        {
            insOpts extendOption = GetEquivalentExtendOption(op1, size);

            if (extendOption != INS_OPTS_NONE)
            {
                cast = op1->AsCast();
                op1  = cast->GetOp(0);
                op1->ClearContained();
                opt = extendOption;

                BlockRange().Remove(cast);
            }
        }
    }
    else if ((mins = GetMultiplyAddInstruction(op2, ins, size)) != INS_none)
    {
        mul = op2->AsInstr();

        BlockRange().Remove(mul);
    }
    else if ((ins == INS_add) && ((mins = GetMultiplyAddInstruction(op1, ins, size)) != INS_none))
    {
        mul = op1->AsInstr();

        BlockRange().Remove(mul);
    }

    arith->ChangeOper(GT_INSTR);

    GenTreeInstr* instr = arith->AsInstr();

    if (mul == nullptr)
    {
        instr->SetIns(ins, size, opt);
        instr->SetImmediate(imm);
        instr->SetNumOps(2);
        instr->SetOp(0, op1);
        instr->SetOp(1, op2);
    }
    else if (mul != nullptr)
    {
        instr->SetIns(mins, size);
        instr->SetNumOps(3);
        instr->SetOp(0, mul->GetOp(0));
        instr->SetOp(1, mul->GetOp(1));
        instr->SetOp(2, mul == op1 ? op2 : op1);
    }
}

GenTreeCast* IsIntToLongCast(GenTree* node)
{
    if (GenTreeCast* cast = node->IsCast())
    {
        if (!cast->gtOverflow() && cast->TypeIs(TYP_LONG) && (varActualType(cast->GetOp(0)->GetType()) == TYP_INT))
        {
            assert((cast->GetCastType() == TYP_LONG) || (cast->GetCastType() == TYP_ULONG));
            return cast;
        }
    }

    return nullptr;
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

    instruction ins = INS_mul;

    if (varTypeIsFloating(mul->GetType()))
    {
        ins = INS_fmul;
    }
    else if (mul->OperIs(GT_MULHI))
    {
        assert(mul->TypeIs(TYP_LONG));

        ins = mul->IsUnsigned() ? INS_umulh : INS_smulh;
    }
    else if (mul->TypeIs(TYP_LONG))
    {
        assert(op1->TypeIs(TYP_LONG));
        assert(op2->TypeIs(TYP_LONG));

        if (GenTreeCast* cast1 = IsIntToLongCast(op1))
        {
            if (GenTreeCast* cast2 = IsIntToLongCast(op2))
            {
                if (cast1->IsUnsigned() == cast2->IsUnsigned())
                {
                    ins = cast1->IsUnsigned() ? INS_umull : INS_smull;

                    BlockRange().Remove(cast1);
                    BlockRange().Remove(cast2);

                    op1 = cast1->GetOp(0);
                    op2 = cast2->GetOp(0);

                    op1->ClearContained();
                    op2->ClearContained();
                }
            }
            else if (GenTreeIntCon* con = op2->IsIntCon())
            {
                if (cast1->IsUnsigned() ? FitsIn<uint32_t>(con->GetValue()) : FitsIn<int32_t>(con->GetValue()))
                {
                    ins = cast1->IsUnsigned() ? INS_umull : INS_smull;

                    BlockRange().Remove(cast1);

                    op1 = cast1->GetOp(0);

                    op1->ClearContained();
                    op2->SetType(TYP_INT);
                }
            }
        }
    }

    mul->ChangeOper(GT_INSTR);

    GenTreeInstr* instr = mul->AsInstr();
    instr->SetIns(ins);
    instr->SetImmediate(0);
    instr->SetNumOps(2);
    instr->SetOp(0, op1);
    instr->SetOp(1, op2);
}

instruction GetEquivalentCompareOrTestInstruction(GenTreeInstr* instr)
{
    switch (instr->GetIns())
    {
        case INS_add:
            return INS_cmn;
        case INS_sub:
            return INS_cmp;
        case INS_and:
            return INS_tst;

        // TODO-MIKE-CQ: There's no "test" like equivalent for bic and the emitter doesn't seem to support ZR as
        // destination register.

        // case INS_bic:
        //  return INS_bics;

        default:
            return INS_none;
    }
}

GenTree* Lowering::OptimizeRelopImm(GenTreeOp* cmp)
{
    GenTree*       op1      = cmp->GetOp(0);
    GenTreeIntCon* op2      = cmp->GetOp(1)->AsIntCon();
    ssize_t        op2Value = op2->GetValue();

    // Eliminate a narrowing cast by testing only the necessary bits. For example:
    //     GT(CAST.ubyte(x), 0) is TEST_NE(x, 255)
    //     EQ(CAST.short(x), 0) is TEST_EQ(x, 65535)

    if (cmp->OperIs(GT_EQ, GT_NE, GT_GT) && (op2Value == 0) && op1->IsCast() && !op1->gtOverflow() &&
        varTypeIsIntegral(op1->AsCast()->GetOp(0)->GetType()))
    {
        var_types castType = op1->AsCast()->GetCastType();

        if (varTypeIsSmall(castType) && (varTypeIsUnsigned(castType) || !cmp->OperIs(GT_GT)))
        {
            BlockRange().Remove(op1);

            op1 = op1->AsCast()->GetOp(0);
            // CAST may have a contained memory operand but ARM compare/test nodes do not.
            op1->ClearContained();

            cmp->SetOper(cmp->OperIs(GT_EQ) ? GT_TEST_EQ : GT_TEST_NE);
            cmp->SetOp(0, op1);
            op2->SetValue((ssize_t(1) << varTypeBitSize(castType)) - 1);
            op2->SetType(varActualType(op1->GetType()));

            return cmp;
        }
    }

    // Transform ((x AND y) EQ|NE 0) into (x TEST_EQ|TEST_NE y) if possible.

    if (cmp->OperIs(GT_EQ, GT_NE) && op1->IsInstr() && (op1->AsInstr()->GetIns() == INS_and) &&
        (op1->AsInstr()->GetOption() == INS_OPTS_NONE))
    {
        GenTreeInstr* andInstr = op1->AsInstr();

        // If we don't have a 0 compare we can get one by transforming ((x AND mask) EQ|NE mask)
        // into ((x AND mask) NE|EQ 0) when mask is a single bit. A single bit can be encoded in
        // a bitmask imm so we don't need to check the non-immediate case.

        if (isPow2(static_cast<size_t>(op2Value)) && (andInstr->GetNumOps() == 1) &&
            (DecodeBitmaskImm(andInstr->GetImmediate(), andInstr->GetAttr()) == op2Value))
        {
            op2Value = 0;
            cmp->SetOper(GenTree::ReverseRelop(cmp->OperGet()));
        }

        if (op2Value == 0)
        {
            BlockRange().Remove(op1);
            BlockRange().Remove(op2);

            cmp->SetOper(cmp->OperIs(GT_EQ) ? GT_TEST_EQ : GT_TEST_NE);
            cmp->SetOp(0, andInstr->GetOp(0));

            if (andInstr->GetNumOps() == 2)
            {
                cmp->SetOp(1, andInstr->GetOp(1));
            }
            else
            {
                op2->SetValue(DecodeBitmaskImm(andInstr->GetImmediate(), andInstr->GetAttr()));
                cmp->SetOp(1, op2);
                BlockRange().InsertBefore(cmp, op2);
            }

            return cmp;
        }
    }

    // Transform EQ|NE(add|sub|and(x, y), 0) into cmn|cmp|tst(x, y) if possible.

    // TODO-CQ: right now the below peep is inexpensive and gets the benefit in most
    // cases because in majority of cases op1, op2 and cmp would be in that order in
    // execution. In general we should be able to check that all the nodes that come
    // after op1 do not modify the flags so that it is safe to avoid generating a
    // test instruction.

    // TODO-MIKE-Consider: This is based on the XARCH model where there's no alternative
    // to using flags. But it's not clear if this is useful on ARM64 where CBZ & co. are
    // available. It's either
    //     adds x0, x1, x2
    //     b.eq L1
    // or
    //     add x0, x1, x2
    //     cbz L1
    // The ARM Cortex optimization guide states that instruction variants that set flags
    // are less efficient so it seems that the later variant is preferrable. It's also
    // preferrable to the JIT, considering the rather poor representation of flags in IR.
    // The only advantage of the flags version is avoiding the need for a temp register.
    // But the JIT doesn't get that right anyway, the flags version should actually be:
    //     cmn x1, x2
    //     b.eq L1
    //
    // Of course, if the relop isn't used by a branch there's no alternative to the flags
    // version since conditional instructions like CSET do use flags.

    if (cmp->OperIs(GT_EQ, GT_NE) && op1->IsInstr() && (op2Value == 0) && (op1->gtNext == op2) && (op2->gtNext == cmp))
    {
        GenTreeInstr* op1Instr = op1->AsInstr();
        instruction   cmpIns   = GetEquivalentCompareOrTestInstruction(op1Instr);

        if (cmpIns != INS_none)
        {
            op1Instr->SetIns(cmpIns, op1Instr->GetAttr(), op1Instr->GetOption());
            op1Instr->SetType(TYP_VOID);
            op1Instr->gtFlags |= GTF_SET_FLAGS;

            BlockRange().Remove(op2);

            GenTree*   next = cmp->gtNext;
            GenTree*   cc;
            genTreeOps ccOp;
            LIR::Use   cmpUse;

            // Fast check for the common case - relop used by a JTRUE that immediately follows it.
            if ((next != nullptr) && next->OperIs(GT_JTRUE) && (next->AsUnOp()->GetOp(0) == cmp))
            {
                cc   = next;
                ccOp = GT_JCC;
                next = nullptr;
                BlockRange().Remove(cmp);
            }
            else if (BlockRange().TryGetUse(cmp, &cmpUse) && cmpUse.User()->OperIs(GT_JTRUE))
            {
                cc   = cmpUse.User();
                ccOp = GT_JCC;
                next = nullptr;
                BlockRange().Remove(cmp);
            }
            else // The relop is not used by a JTRUE or it is not used at all.
            {
                // Transform the relop node it into a SETCC. If it's not used we could remove
                // it completely but that means doing more work to handle a rare case.
                cc   = cmp;
                ccOp = GT_SETCC;
            }

            GenCondition condition = GenCondition::FromIntegralRelop(cmp);
            cc->ChangeOper(ccOp);
            cc->AsCC()->gtCondition = condition;
            cc->gtFlags |= GTF_USE_FLAGS;

            return next;
        }
    }

    return cmp;
}

GenTree* Lowering::LowerRelop(GenTreeOp* cmp)
{
    if (cmp->GetOp(1)->IsIntCon())
    {
        if (!comp->opts.MinOpts())
        {
            GenTree* next = OptimizeRelopImm(cmp);

            // If OptimizeRelopImm return the compare node as "next" then we need to continue lowering.
            if (next != cmp)
            {
                return next;
            }
        }

        CheckImmedAndMakeContained(cmp, cmp->GetOp(1));
    }

    return cmp->gtNext;
}

GenTree* Lowering::LowerJTrue(GenTreeUnOp* jtrue)
{
    GenTree* relop    = jtrue->GetOp(0);
    GenTree* relopOp2 = relop->AsOp()->GetOp(1);

    if ((relop->gtNext == jtrue) && relopOp2->IsIntCon())
    {
        bool     useJCMP   = false;
        unsigned jcmpFlags = 0;

        if (relop->OperIs(GT_EQ, GT_NE) && relopOp2->IsIntegralConst(0))
        {
            // Generate CBZ/CBNZ
            useJCMP   = true;
            jcmpFlags = relop->OperIs(GT_EQ) ? GTF_JCMP_EQ : 0;
        }
        else if (relop->OperIs(GT_TEST_EQ, GT_TEST_NE) && isPow2(relopOp2->AsIntCon()->GetValue()))
        {
            // Generate TBZ/TBNZ
            useJCMP   = true;
            jcmpFlags = GTF_JCMP_TST | (relop->OperIs(GT_TEST_EQ) ? GTF_JCMP_EQ : 0);
        }

        if (useJCMP)
        {
            relop->SetOper(GT_JCMP);
            relop->SetType(TYP_VOID);
            relop->gtFlags &= ~(GTF_JCMP_TST | GTF_JCMP_EQ);
            relop->gtFlags |= jcmpFlags;

            relopOp2->SetContained();

            BlockRange().Remove(jtrue);

            assert(relop->gtNext == nullptr);
            return nullptr;
        }
    }

    relop->SetType(TYP_VOID);
    relop->gtFlags |= GTF_SET_FLAGS;

    assert(jtrue->gtNext == nullptr);
    return nullptr;
}

#endif // TARGET_ARM64
