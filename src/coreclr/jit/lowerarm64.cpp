// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "lower.h"

#ifdef TARGET_ARM64 // This file is ONLY used for ARM64 architectures

GenTreeInstr* IsInstr(GenTree* node, instruction ins, emitAttr size)
{
    if (node->IsInstr() && (node->AsInstr()->GetSize() == size) && (node->AsInstr()->GetIns() == ins))
    {
        return node->AsInstr();
    }

    return nullptr;
}

GenTreeInstr* IsInstr(GenTree* node, instruction ins, emitAttr size, unsigned numOps)
{
    if (node->IsInstr() && (node->AsInstr()->GetSize() == size) && (node->AsInstr()->GetIns() == ins) &&
        (node->AsInstr()->GetNumOps() == numOps))
    {
        return node->AsInstr();
    }

    return nullptr;
}

GenTreeInstr* IsInstr(GenTree* node, instruction ins, unsigned numOps)
{
    if (node->IsInstr() && (node->AsInstr()->GetIns() == ins) && (node->AsInstr()->GetNumOps() == numOps))
    {
        return node->AsInstr();
    }

    return nullptr;
}

GenTreeInstr* IsInstr(GenTree* node, emitAttr size, unsigned numOps)
{
    if (node->IsInstr() && (node->AsInstr()->GetSize() == size) && (node->AsInstr()->GetNumOps() == numOps))
    {
        return node->AsInstr();
    }

    return nullptr;
}

insOpts GetEquivalentShiftOptionLogical(GenTree* node, emitAttr size)
{
    if (GenTreeInstr* instr = IsInstr(node, size, 1))
    {
        switch (instr->GetIns())
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

GenTreeInstr* Lowering::MakeInstr(GenTree* node, instruction ins, emitAttr size)
{
    node->SetOper(GT_INSTR);

    GenTreeInstr* instr = node->AsInstr();
    instr->SetIns(ins, size);
    instr->SetNumOps(0);
    instr->SetImmediate(0);
    // Currently INSTR nodes never have side effects. This will need to be adjusted if load/store
    // nodes are lowered to load/store instructions.
    instr->gtFlags = GTF_EMPTY;
    return instr;
}

GenTreeInstr* Lowering::MakeInstr(GenTree* node, instruction ins, emitAttr size, GenTree* op1)
{
    GenTreeInstr* instr = MakeInstr(node, ins, size);
    instr->SetNumOps(1);
    instr->SetOp(0, op1);
    return instr;
}

GenTreeInstr* Lowering::MakeInstr(GenTree* node, instruction ins, emitAttr size, GenTree* op1, GenTree* op2)
{
    GenTreeInstr* instr = MakeInstr(node, ins, size);
    instr->SetNumOps(2);
    instr->SetOp(0, op1);
    instr->SetOp(1, op2);
    return instr;
}

GenTreeInstr* Lowering::NewInstrBefore(GenTree* before, var_types type, instruction ins, GenTree* op1)
{
    GenTreeInstr* instr = new (comp, GT_INSTR) GenTreeInstr(type, ins, op1);
    BlockRange().InsertBefore(before, instr);
    return instr;
}

GenTreeInstr* Lowering::NewInstrAfter(GenTree* after, var_types type, instruction ins, GenTree* op1)
{
    GenTreeInstr* instr = new (comp, GT_INSTR) GenTreeInstr(type, ins, op1);
    BlockRange().InsertAfter(after, instr);
    return instr;
}

GenTreeInstr* Lowering::NewInstrBefore(GenTree* before, var_types type, instruction ins, GenTree* op1, GenTree* op2)
{
    GenTreeInstr* instr = new (comp, GT_INSTR) GenTreeInstr(type, ins, op1, op2);
    BlockRange().InsertBefore(before, instr);
    return instr;
}

void Lowering::LowerNot(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_NOT));

    GenTree* op1  = node->GetOp(0);
    emitAttr size = emitActualTypeSize(node->GetType());

    CombineNot(MakeInstr(node, INS_mvn, size, op1));
}

void Lowering::CombineNot(GenTreeInstr* instr)
{
    assert(instr->GetIns() == INS_mvn);

    GenTree* op1 = instr->GetOp(0);

    if (insOpts opt = GetEquivalentShiftOptionLogical(op1, instr->GetSize()))
    {
        GenTreeInstr* shift = op1->AsInstr();

        op1 = shift->GetOp(0);

        assert(IsLegalToMoveUseForward(shift, instr, op1));

        if (shift->GetImmediate() != 0)
        {
            instr->SetOption(opt);
            instr->SetImmediate(shift->GetImmediate());
        }

        instr->SetOp(0, op1);

        BlockRange().Remove(shift);
    }
}

static bool CanEncodeBitmaskImm(int64_t imm, emitAttr size, unsigned* bitmaskImm)
{
    return Arm64Imm::IsBitMaskImm(imm, size, bitmaskImm);
}

int64_t DecodeBitmaskImm(unsigned encoded, emitAttr size)
{
    return Arm64Imm::DecodeBitMaskImm(encoded, size);
}

static int64_t DecodeBitmaskImm(GenTreeInstr* instr)
{
    return DecodeBitmaskImm(instr->GetImmediate(), instr->GetSize());
}

class BitField
{
    const unsigned m_lsb;
    const unsigned m_width;

public:
    BitField(unsigned lsb, unsigned width) : m_lsb(lsb), m_width(width)
    {
        assert(lsb < 64);
        assert(lsb + width <= 64);
    }

    operator bool() const
    {
        return m_width != 0;
    }

    unsigned LSB() const
    {
        return m_lsb;
    }

    unsigned Width() const
    {
        return m_width;
    }

    unsigned MSB() const
    {
        return m_lsb + m_width - 1;
    }
};

BitField IsBitFieldMaskAt(uint64_t mask, unsigned at, unsigned bitSize)
{
    assert(at < bitSize);

    if (bitSize == 32)
    {
        mask &= UINT32_MAX;
    }
    else
    {
        assert(bitSize == 64);
    }

    unsigned width   = 0;
    uint64_t maskLow = (1ULL << at) - 1;

    if ((mask & maskLow) == 0)
    {
        mask >>= at;

        if (isPow2(mask + 1))
        {
            width = genLog2(mask + 1);
        }
    }

    return BitField(at, width);
}

unsigned PackBFIImmediate(unsigned lsb, unsigned width, unsigned bitSize)
{
    assert((0 <= lsb) && (lsb < bitSize));
    assert((1 <= width) && (width <= bitSize - lsb));

    return (lsb << 6) | width;
}

void Lowering::LowerLogical(GenTreeOp* logical)
{
    assert(logical->OperIs(GT_AND, GT_OR, GT_XOR));

    GenTree* op1  = logical->GetOp(0);
    GenTree* op2  = logical->GetOp(1);
    emitAttr size = emitActualTypeSize(logical->GetType());

    if (op2->IsIntCon() && !op2->AsIntCon()->ImmedValNeedsReloc(comp))
    {
        if (logical->OperIs(GT_AND) && IsInstr(op1, size, 1))
        {
            GenTreeInstr* shift       = op1->AsInstr();
            unsigned      shiftAmount = shift->GetImmediate();
            unsigned      bitSize     = EA_SIZE_IN_BYTES(size) * 8;
            uint64_t      mask        = static_cast<uint64_t>(op2->AsIntCon()->GetValue());
            instruction   ins         = INS_none;
            unsigned      imm         = 0;

            if (shift->GetIns() == INS_lsl)
            {
                // The shift zeroes out bits shiftAmount-1..0 so we don't care what bits the mask has there.
                mask &= UINT64_MAX << shiftAmount;

                if (BitField bitfield = IsBitFieldMaskAt(mask, shiftAmount, bitSize))
                {
                    // AND(LSL(x, N), bfmask(N, W)) = UBFIZ(x, N, W)

                    ins = INS_ubfiz;
                    imm = PackBFIImmediate(shiftAmount, bitfield.Width(), bitSize);
                }
            }
            else if (shift->GetIns() == INS_lsr)
            {
                // The shift zeroes out bits bitSize-1..shiftAmount so we don't care what bits the mask has there.
                mask &= ((bitSize == 64) ? UINT64_MAX : UINT32_MAX) >> shiftAmount;

                if (BitField bitfield = IsBitFieldMaskAt(mask, 0, bitSize))
                {
                    BitField shifted{shiftAmount, bitSize - shiftAmount};

                    if (shifted.Width() <= bitfield.Width())
                    {
                        // The mask bitfield totally overlaps the shifted bitfield so masking isn't
                        // needed, we just need a logical right shift to put the bitfield in place
                        // and zero out the upper bits.

                        // TODO-MIKE-Cleanup: This looks like something that belongs in morph.

                        ins = INS_lsr;
                        imm = shiftAmount;
                    }
                    else
                    {
                        // A portion of the shifted bitfield is extracted so we need UBFX:
                        //     AND(LSR(x, N), bfmask(0, W)) = UBFX(x, N, W)

                        ins = INS_ubfx;
                        imm = PackBFIImmediate(shiftAmount, bitfield.Width(), bitSize);
                    }
                }
            }
            else if (shift->GetIns() == INS_asr)
            {
                if (BitField bitfield = IsBitFieldMaskAt(mask, 0, bitSize))
                {
                    BitField shifted{shiftAmount, bitSize - shiftAmount};

                    if (shifted.Width() == bitfield.Width())
                    {
                        // The mask bitfield exactly overlaps the shifted bitfield so masking isn't
                        // needed, we just need a logical right shift to but the bitfield in place
                        // and zero out the upper bits. The sign bits introduced by ASR would be
                        // discarded by the mask anyway.

                        ins = INS_lsr;
                        imm = shiftAmount;
                    }
                    else if (shifted.Width() > bitfield.Width())
                    {
                        // A portion of the shifted bitfield is extracted so we need UBFX:
                        //     AND(LSR(x, N), bfmask(0, W)) = UBFX(x, N, W)

                        ins = INS_ubfx;
                        imm = PackBFIImmediate(shiftAmount, bitfield.Width(), bitSize);
                    }
                }
            }

            if (ins != INS_none)
            {
                op1 = shift->GetOp(0);

                assert(IsLegalToMoveUseForward(shift, logical, op1));

                GenTreeInstr* instr = MakeInstr(logical, ins, size, shift->GetOp(0));
                instr->SetImmediate(imm);

                BlockRange().Remove(shift);
                BlockRange().Remove(op2);
                return;
            }
        }

        unsigned encodedBitmaskImm;

        if (CanEncodeBitmaskImm(op2->AsIntCon()->GetValue(), size, &encodedBitmaskImm))
        {
            BlockRange().Remove(op2);

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

            GenTreeInstr* instr = MakeInstr(logical, ins, size, op1);
            instr->SetImmediate(encodedBitmaskImm);
            return;
        }
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

        assert(IsLegalToMoveUseForward(mvn, logical, op2));
    }
    else if ((mvn = IsInstr(op1, size, INS_mvn)) != nullptr)
    {
        std::swap(op1, op2);

        op2 = mvn->GetOp(0);
        imm = mvn->GetImmediate();
        opt = mvn->GetOption();

        assert(IsLegalToMoveUseForward(mvn, logical, op2));
    }
    else if ((opt = GetEquivalentShiftOptionLogical(op2, size)) != INS_OPTS_NONE)
    {
        shift = op2->AsInstr();
        op2   = shift->GetOp(0);
        imm   = shift->GetImmediate();

        if (imm == 0)
        {
            opt = INS_OPTS_NONE;
        }

        assert(IsLegalToMoveUseForward(shift, logical, op2));
    }
    else if ((opt = GetEquivalentShiftOptionLogical(op1, size)) != INS_OPTS_NONE)
    {
        std::swap(op1, op2);

        shift = op2->AsInstr();
        op2   = shift->GetOp(0);
        imm   = shift->GetImmediate();

        if (imm == 0)
        {
            opt = INS_OPTS_NONE;
        }

        assert(IsLegalToMoveUseForward(shift, logical, op2));
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

    GenTreeInstr* instr = MakeInstr(logical, ins, size, op1, op2);
    instr->SetOption(opt);
    instr->SetImmediate(imm);

    if (mvn != nullptr)
    {
        BlockRange().Remove(mvn);
    }

    if (shift != nullptr)
    {
        BlockRange().Remove(shift);
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
    GenTree* op1  = shift->GetOp(0);
    GenTree* op2  = shift->GetOp(1);
    emitAttr size = emitActualTypeSize(shift->GetType());

    // ARM64 shift instructions mask the shift count to 5 bits (or 6 bits for 64 bit operations).
    //
    // TODO-MIKE-Cleanup: This really belongs in morph. The problem is ensuring that various JIT
    // parts (constant folding, VN etc.) handle shifts according to target specifics (ARM32 masks
    // only 8 bits thus (i32 << 32) is 0 on ARM32 and i32 on ARM64).
    //
    // TODO-MIKE-CQ: And of course, any narrowing casts are redundant too, morph doesn't figure
    // that out either.

    while (GenTreeInstr* andInstr = IsInstr(op2, INS_and, 1))
    {
        ssize_t shiftImmMask = (size == EA_4BYTE) ? 31 : 63;
        ssize_t andImm       = DecodeBitmaskImm(andInstr);

        if ((andImm & shiftImmMask) != shiftImmMask)
        {
            break;
        }

        assert(IsLegalToMoveUseForward(andInstr, shift, andInstr->GetOp(0)));

        op2 = andInstr->GetOp(0);
        BlockRange().Remove(andInstr);
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

    MakeInstr(shift, ins, size, op1, op2);
}

void Lowering::LowerShiftImmediate(GenTreeOp* shift)
{
    GenTree* op1  = shift->GetOp(0);
    GenTree* op2  = shift->GetOp(1);
    emitAttr size = emitActualTypeSize(shift->GetType());

    unsigned bitSize     = EA_SIZE_IN_BYTES(size) * 8;
    unsigned shiftAmount = static_cast<unsigned>(op2->AsIntCon()->GetValue()) & (bitSize - 1);

    if ((shiftAmount >= 24) && shift->OperIs(GT_LSH) && comp->opts.OptimizationEnabled())
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

        unsigned consumedBits = bitSize - shiftAmount;

        if (op1->OperIs(GT_SXT, GT_UXT) && (consumedBits <= 32))
        {
            JITDUMP("Removing SXT/UXT [%06u] producing 32 bits from LSH [%06u] consuming %u bits\n", op1->GetID(),
                    shift->GetID(), consumedBits);

            BlockRange().Remove(op1);
            op1 = op1->AsUnOp()->GetOp(0);
            op1->ClearContained();
        }

        while (op1->OperIs(GT_CONV))
        {
            var_types castType = op1->GetType();
            assert(varTypeIsSmall(castType));

            unsigned producedBits = varTypeBitSize(castType);

            if (consumedBits > producedBits)
            {
                break;
            }

            JITDUMP("Removing CAST [%06u] producing %u bits from LSH [%06u] consuming %u bits\n", op1->GetID(),
                    producedBits, shift->GetID(), consumedBits);

            BlockRange().Remove(op1);
            op1 = op1->AsUnOp()->GetOp(0);
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

    GenTreeInstr* instr = MakeInstr(shift, ins, size, op1);
    instr->SetImmediate(shiftAmount);

    BlockRange().Remove(op2);

    CombineShiftImmediate(instr);
}

void Lowering::CombineShiftImmediate(GenTreeInstr* shift)
{
    GenTree* op1  = shift->GetOp(0);
    emitAttr size = shift->GetSize();

    unsigned bitSize     = EA_SIZE_IN_BYTES(size) * 8;
    unsigned shiftAmount = shift->GetImmediate();

    assert(shiftAmount < bitSize);

    if (shiftAmount == 0)
    {
        return;
    }

    if (GenTreeInstr* andInstr = IsInstr(op1, INS_and, size, 1))
    {
        uint64_t    mask = static_cast<uint64_t>(DecodeBitmaskImm(andInstr));
        instruction ins  = INS_none;
        unsigned    imm  = 0;

        if (shift->GetIns() == INS_lsl)
        {
            // The shift discards bits regSize-1..regSize-shiftAmount so we don't care what bits the mask has there.
            mask &= ((bitSize == 64) ? UINT64_MAX : UINT32_MAX) >> shiftAmount;

            if (BitField bf = IsBitFieldMaskAt(mask, 0, bitSize))
            {
                if (shiftAmount + bf.Width() >= bitSize)
                {
                    // All the bits to the left of the bitfield (and possibly a part of the bitfield itself)
                    // are shifted out. These are the same bits that "and" discards so "and" is not needed.

                    // TODO-MIKE-Cleanup: This transform probably belongs in morph.

                    ins = INS_lsl;
                    imm = shiftAmount;
                }
                else
                {
                    // LSH(AND(x, bfmask(0, W)), N) = UBFIZ(x, N, W)

                    ins = INS_ubfiz;
                    imm = PackBFIImmediate(shiftAmount, bf.Width(), bitSize);
                }
            }
        }
        else
        {
            assert((shift->GetIns() == INS_asr) || (shift->GetIns() == INS_lsr));

            // The shift discards bits shiftAmount-1..0 so we don't care what bits the mask has there.
            mask &= UINT64_MAX << shiftAmount;

            if (BitField bf = IsBitFieldMaskAt(mask, shiftAmount, bitSize))
            {
                if (bf.MSB() == bitSize - 1)
                {
                    // There are no bits to the left of the bitfield so masking isn't needed.
                    // This also covers the case of the sign bit being included in the bitfield,
                    // which would otherwise require sbfx instead of ubfx.

                    // TODO-MIKE-Cleanup: This transform probably belongs in morph.

                    ins = shift->GetIns();
                    imm = shiftAmount;
                }
                else
                {
                    // (RSH|RSZ)(AND(x, bfmask(N, W), N) = UBFX(x, N, W)

                    ins = INS_ubfx;
                    imm = PackBFIImmediate(shiftAmount, bf.Width(), bitSize);
                }
            }
        }

        if (ins != INS_none)
        {
            op1 = andInstr->GetOp(0);

            shift->SetIns(ins, size);
            shift->SetOp(0, op1);
            shift->SetImmediate(imm);

            BlockRange().Remove(andInstr);

            return;
        }
    }

    if (op1->OperIs(GT_SXT, GT_UXT))
    {
        // Shift instructions do not have an "extending form" like arithmetic instructions but the
        // same operation can be performed using bitfield insertion/extraction instructions.
        // For example:
        //    - SXTB x0, w0 and LSL x0, x0, #12 <=> SBFIZ x0, x0, #12, #8
        //    - SXTW x0, w0 and ASR x0, x0, #12 <=> SBFX x0, x0, #12, #20

        unsigned bitFieldWidth = 32;
        bool     isUnsigned    = op1->OperIs(GT_UXT);

        assert(size == EA_8BYTE);

        unsigned    bitSize = EA_SIZE_IN_BYTES(size) * 8;
        instruction ins     = INS_none;
        unsigned    imm     = 0;

        if (shift->GetIns() == INS_lsl)
        {
            if (bitFieldWidth + shiftAmount >= bitSize)
            {
                // We don't need to insert if all the extension bits produced by the cast are discarded
                // by the shift (e.g. LSH(SXT, 32)), we'll just generate a LSL in that case.

                ins = INS_lsl;
                imm = shiftAmount;
            }
            else
            {
                // LSH(CAST(x), N) = UBFIZ|SBFIZ(x, N, cast type size)

                ins = isUnsigned ? INS_ubfiz : INS_sbfiz;
                imm = PackBFIImmediate(shiftAmount, bitFieldWidth, bitSize);
            }
        }
        else if (shift->GetIns() == INS_asr)
        {
            ins = isUnsigned ? INS_ubfx : INS_sbfx;

            if (shiftAmount >= bitFieldWidth)
            {
                // All the bits of the casted value are discarded by the shift. The only thing that's
                // left are the extension bits that the cast produced. These bits can be obtained by
                // extracting the sign bit from the casted value.

                imm = PackBFIImmediate(bitFieldWidth - 1, 1, bitSize);
            }
            else
            {
                // We still have some bits from the casted value that need to be extracted. Extraction
                // needs the width of the extracted bitfield, which is smaller than the width of the
                // casted value.

                imm = PackBFIImmediate(shiftAmount, bitFieldWidth - shiftAmount, bitSize);
            }
        }
        else
        {
            assert(shift->GetIns() == INS_lsr);

            if (isUnsigned)
            {
                if (shiftAmount >= bitFieldWidth)
                {
                    // Unlike in the ASR case, if all bits are discarded by the shift we'd get 0. We could try
                    // to replace the shift with constant 0 but it seems unlikely to be worth the trouble.
                    // Besides, there's no reason not to do this in morph.
                }
                else
                {
                    ins = INS_ubfx;
                    imm = PackBFIImmediate(shiftAmount, bitFieldWidth - shiftAmount, bitSize);
                }
            }
            else
            {
                // Sometimes LSR is used to extract the sign bit of a small int value:
                //   LSR(CAST.short(x), 31) = UBFX(x, 15, 1)

                if (shiftAmount == bitSize - 1)
                {
                    ins = INS_ubfx;
                    imm = PackBFIImmediate(bitFieldWidth - 1, 1, bitSize);
                }
            }
        }

        if (ins != INS_none)
        {
            GenTreeUnOp* ext = op1->AsUnOp();

            op1 = ext->GetOp(0);
            op1->ClearContained();

            shift->SetIns(ins, size);
            shift->SetOp(0, op1);
            shift->SetImmediate(imm);

            BlockRange().Remove(ext);

            return;
        }
    }

    if (op1->OperIs(GT_CONV))
    {
        // Shift instructions do not have an "extending form" like arithmetic instructions but the
        // same operation can be performed using bitfield insertion/extraction instructions.
        // For example:
        //    - SXTB x0, w0 and LSL x0, x0, #12 <=> SBFIZ x0, x0, #12, #8
        //    - SXTW x0, w0 and ASR x0, x0, #12 <=> SBFX x0, x0, #12, #20

        GenTreeUnOp* cast = op1->AsUnOp();

        assert(varTypeIsSmall(cast->GetType()));

        // Currently the JIT IR doesn't allow direct extension from small int types to LONG.
        // This code likely works fine with such casts but it cannot be tested.

        // TODO-MIKE-CQ: This also means that a pattern like "(ulong)x_byte << 6"
        // isn't transformed into "ubfiz x, 6, 8" but in "uxtb x; ubfiz x, 6, 32".
        // Such casts could be described in IR as CAST nodes having LONG instead
        // of INT type. This requires auditing all cast handling code in the JIT,
        // some of it may assume that if the cast type is small int then the CAST
        // node's type is always INT.

        // TODO-MIKE-CQ: Another potential issue is that zero extending casts could
        // be folded together with AND(x, mask) but nothing in the JIT seems to be
        // doing this so code like "(x_byte & 3) << 6" also generates an extra uxtb.

        unsigned bitFieldWidth = varTypeBitSize(cast->GetType());
        bool     isUnsigned    = varTypeIsUnsigned(cast->GetType());

        unsigned    bitSize = EA_SIZE_IN_BYTES(size) * 8;
        instruction ins     = INS_none;
        unsigned    imm     = 0;

        if (shift->GetIns() == INS_lsl)
        {
            if (bitFieldWidth + shiftAmount >= bitSize)
            {
                // We don't need to insert if all the extension bits produced by the cast are discarded
                // by the shift (e.g. LSH(CAST.byte, 56)), we'll just generate a LSL in that case.

                ins = INS_lsl;
                imm = shiftAmount;
            }
            else
            {
                // LSH(CAST(x), N) = UBFIZ|SBFIZ(x, N, cast type size)

                ins = isUnsigned ? INS_ubfiz : INS_sbfiz;
                imm = PackBFIImmediate(shiftAmount, bitFieldWidth, bitSize);
            }
        }
        else if (shift->GetIns() == INS_asr)
        {
            ins = isUnsigned ? INS_ubfx : INS_sbfx;

            if (shiftAmount >= bitFieldWidth)
            {
                // All the bits of the casted value are discarded by the shift. The only thing that's
                // left are the extension bits that the cast produced. These bits can be obtained by
                // extracting the sign bit from the casted value.

                imm = PackBFIImmediate(bitFieldWidth - 1, 1, bitSize);
            }
            else
            {
                // We still have some bits from the casted value that need to be extracted. Extraction
                // needs the width of the extracted bitfield, which is smaller than the width of the
                // casted value.

                imm = PackBFIImmediate(shiftAmount, bitFieldWidth - shiftAmount, bitSize);
            }
        }
        else
        {
            assert(shift->GetIns() == INS_lsr);

            if (isUnsigned)
            {
                if (shiftAmount >= bitFieldWidth)
                {
                    // Unlike in the ASR case, if all bits are discarded by the shift we'd get 0. We could try
                    // to replace the shift with constant 0 but it seems unlikely to be worth the trouble.
                    // Besides, there's no reason not to do this in morph.
                }
                else
                {
                    ins = INS_ubfx;
                    imm = PackBFIImmediate(shiftAmount, bitFieldWidth - shiftAmount, bitSize);
                }
            }
            else
            {
                // Sometimes LSR is used to extract the sign bit of a small int value:
                //   LSR(CAST.short(x), 31) = UBFX(x, 15, 1)

                if (shiftAmount == bitSize - 1)
                {
                    ins = INS_ubfx;
                    imm = PackBFIImmediate(bitFieldWidth - 1, 1, bitSize);
                }
            }
        }

        if (ins != INS_none)
        {
            op1 = cast->GetOp(0);
            op1->ClearContained();

            shift->SetIns(ins, size);
            shift->SetOp(0, op1);
            shift->SetImmediate(imm);

            BlockRange().Remove(cast);

            return;
        }
    }
}

static insOpts GetEquivalentShiftOptionArithmetic(GenTree* node, emitAttr size)
{
    if (GenTreeInstr* instr = IsInstr(node, EA_SIZE(size), 1))
    {
        switch (instr->GetIns())
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

static insOpts GetEquivalentExtendOption(GenTree* node)
{
    if (node->OperIs(GT_CONV))
    {
        switch (node->GetType())
        {
            case TYP_UBYTE:
                return INS_OPTS_UXTB;
            case TYP_BYTE:
                return INS_OPTS_SXTB;
            case TYP_USHORT:
                return INS_OPTS_UXTH;
            case TYP_SHORT:
                return INS_OPTS_SXTH;
            default:
                break;
        }
    }
    else if (node->OperIs(GT_SXT))
    {
        return INS_OPTS_SXTW;
    }
    else if (node->OperIs(GT_UXT))
    {
        return INS_OPTS_UXTW;
    }

    return INS_OPTS_NONE;
}

static instruction GetMultiplyAddInstruction(GenTree* node, instruction ins, emitAttr size)
{
    if (GenTreeInstr* instr = IsInstr(node, EA_SIZE(size), 2))
    {
        switch (instr->GetIns())
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

bool CanEncodeArithmeticImm(ssize_t imm, emitAttr size, unsigned* encodedArithImm)
{
    bool encodable   = Arm64Imm::IsAddImm(imm, size);
    *encodedArithImm = static_cast<unsigned>(imm) & 0xFFFFFF;
    return encodable;
}

void Lowering::LowerNegate(GenTreeUnOp* neg)
{
    assert(neg->OperIs(GT_NEG) && varTypeIsIntegral(neg->GetType()));

    GenTree* op1  = neg->GetOp(0);
    emitAttr size = emitActualTypeSize(neg->GetType());

    if (GenTreeInstr* mul = IsInstr(op1, INS_mul, size, 2))
    {
        MakeInstr(neg, INS_mneg, size, mul->GetOp(0), mul->GetOp(1));
        BlockRange().Remove(mul);
        return;
    }

    GenTreeInstr* shift = nullptr;
    insOpts       opt   = INS_OPTS_NONE;
    unsigned      imm   = 0;

    if ((opt = GetEquivalentShiftOptionArithmetic(op1, size)) != INS_OPTS_NONE)
    {
        shift = op1->AsInstr();
        op1   = shift->GetOp(0);
        imm   = shift->GetImmediate();

        if (imm == 0)
        {
            opt = INS_OPTS_NONE;
        }

        assert(IsLegalToMoveUseForward(shift, neg, op1));
    }

    GenTreeInstr* instr = MakeInstr(neg, INS_neg, size, op1);
    instr->SetOption(opt);
    instr->SetImmediate(imm);

    if (shift != nullptr)
    {
        BlockRange().Remove(shift);
    }
}

void Lowering::LowerArithmetic(GenTreeOp* arith)
{
    assert(arith->OperIs(GT_ADD, GT_SUB) && varTypeIsIntegralOrI(arith->GetType()));

    GenTree* op1  = arith->GetOp(0);
    GenTree* op2  = arith->GetOp(1);
    emitAttr size = emitActualTypeSize(arith->GetType());

    LIR::Use use;
    if (BlockRange().TryGetUse(arith, &use) && use.User()->IsIndir() && (use.User()->AsIndir()->GetAddr() == arith))
    {
        // Don't lower indir(ADD|SUB(x, y)) for now, it is recognized by ContainCheckIndir & co.
        ContainCheckBinary(arith);
        return;
    }

    if (op2->IsIntCon() && !op2->AsIntCon()->ImmedValNeedsReloc(comp))
    {
        ssize_t  imm = op2->AsIntCon()->GetValue();
        unsigned encodedArithImm;

        if (CanEncodeArithmeticImm(abs(imm), size, &encodedArithImm))
        {
            instruction ins;

            if (arith->OperIs(GT_ADD))
            {
                ins = imm >= 0 ? INS_add : INS_sub;
            }
            else
            {
                ins = imm >= 0 ? INS_sub : INS_add;
            }

            GenTreeInstr* instr = MakeInstr(arith, ins, size, op1);
            instr->SetImmediate(encodedArithImm);
            BlockRange().Remove(op2);
            return;
        }
    }

    instruction   ins     = arith->OperIs(GT_ADD) ? INS_add : INS_sub;
    insOpts       opt     = INS_OPTS_NONE;
    unsigned      imm     = 0;
    GenTreeUnOp*  extend  = nullptr;
    GenTreeInstr* shift   = nullptr;
    GenTreeInstr* mul     = nullptr;
    instruction   maddIns = INS_none;

    // TODO-MIKE-CQ: It most cases it doesn't matter which operand tree matches the pattern,
    // one way or another we're going to remove an instruction. Extended register forms are
    // an exception - 2 instructions can be removed, a cast and a shift - so both matches
    // should be tried and the best one selected. Assuming that diffs show enough hits for
    // this to warrant the extra throughput cost...

    if ((opt = GetEquivalentShiftOptionArithmetic(op2, size)) != INS_OPTS_NONE)
    {
        shift = op2->AsInstr();
        op2   = shift->GetOp(0);
        imm   = shift->GetImmediate();

        assert(IsLegalToMoveUseForward(shift, arith, op2));

        if (((opt == INS_OPTS_LSL) && (imm <= 4)) || (imm == 0))
        {
            insOpts extendOption = GetEquivalentExtendOption(op2);

            if (extendOption != INS_OPTS_NONE)
            {
                extend = op2->AsUnOp();
                op2    = extend->GetOp(0);
                op2->ClearContained();
                opt = extendOption;

                assert(IsLegalToMoveUseForward(extend, arith, op2));
            }
            else if (imm == 0)
            {
                opt = INS_OPTS_NONE;
            }
        }
    }
    else if ((ins == INS_add) && (opt = GetEquivalentShiftOptionArithmetic(op1, size)) != INS_OPTS_NONE)
    {
        std::swap(op1, op2);

        shift = op2->AsInstr();
        op2   = shift->GetOp(0);
        imm   = shift->GetImmediate();

        assert(IsLegalToMoveUseForward(shift, arith, op2));

        if (((opt == INS_OPTS_LSL) && (imm <= 4)) || (imm == 0))
        {
            insOpts extendOption = GetEquivalentExtendOption(op2);

            if (extendOption != INS_OPTS_NONE)
            {
                extend = op2->AsUnOp();
                op2    = extend->GetOp(0);
                op2->ClearContained();
                opt = extendOption;

                assert(IsLegalToMoveUseForward(extend, arith, op2));
            }
            else if (imm == 0)
            {
                opt = INS_OPTS_NONE;
            }
        }
    }
    else if ((opt = GetEquivalentExtendOption(op2)) != INS_OPTS_NONE)
    {
        extend = op2->AsUnOp();
        op2    = extend->GetOp(0);
        op2->ClearContained();

        assert(IsLegalToMoveUseForward(extend, arith, op2));
    }
    else if ((ins == INS_add) && (opt = GetEquivalentExtendOption(op1)) != INS_OPTS_NONE)
    {
        std::swap(op1, op2);

        extend = op2->AsUnOp();
        op2    = extend->GetOp(0);
        op2->ClearContained();

        assert(IsLegalToMoveUseForward(extend, arith, op2));
    }
    else if ((maddIns = GetMultiplyAddInstruction(op2, ins, size)) != INS_none)
    {
        mul = op2->AsInstr();
    }
    else if ((ins == INS_add) && ((maddIns = GetMultiplyAddInstruction(op1, ins, size)) != INS_none))
    {
        std::swap(op1, op2);

        mul = op2->AsInstr();
    }

    if (mul == nullptr)
    {
        GenTreeInstr* instr = MakeInstr(arith, ins, size, op1, op2);
        instr->SetOption(opt);
        instr->SetImmediate(imm);

        if (shift != nullptr)
        {
            BlockRange().Remove(shift);
        }

        if (extend != nullptr)
        {
            BlockRange().Remove(extend);
        }
    }
    else
    {
        assert(IsLegalToMoveUseForward(mul, arith, mul->GetOp(0)));
        assert(IsLegalToMoveUseForward(mul, arith, mul->GetOp(1)));

        GenTreeInstr* instr = MakeInstr(arith, maddIns, size);
        instr->SetNumOps(3);
        instr->SetOp(0, mul->GetOp(0));
        instr->SetOp(1, mul->GetOp(1));
        instr->SetOp(2, op1);

        BlockRange().Remove(mul);
    }
}

static GenTreeUnOp* IsIntExtend(GenTree* node)
{
    if (node->OperIs(GT_SXT, GT_UXT))
    {
        return node->AsUnOp();
    }

    return nullptr;
}

void Lowering::LowerMultiply(GenTreeOp* mul)
{
    assert(mul->OperIs(GT_MUL, GT_SMULH, GT_UMULH) && varTypeIsIntegral(mul->GetType()));

    GenTree* op1  = mul->GetOp(0);
    GenTree* op2  = mul->GetOp(1);
    emitAttr size = emitActualTypeSize(mul->GetType());

    if (mul->OperIs(GT_SMULH, GT_UMULH))
    {
        if (size == EA_8BYTE)
        {
            assert(mul->TypeIs(TYP_LONG));
            assert(op1->TypeIs(TYP_LONG));
            assert(op2->TypeIs(TYP_LONG));

            MakeInstr(mul, mul->OperIs(GT_UMULH) ? INS_umulh : INS_smulh, EA_8BYTE, op1, op2);
        }
        else
        {
            assert(mul->TypeIs(TYP_INT));
            assert(varActualType(op1->GetType()) == TYP_INT);
            assert(varActualType(op2->GetType()) == TYP_INT);

            // TODO-MIKE-Cleanup: Magic division should not produce such a MULHI node. There's no corresponding ARM64
            // instruction for this operation and we need to insert a right shift to obtain the "hi" 32 bits of the
            // long result. And in some cases magic division follows up with another right shift that currently doesn't
            // combine with this one.

            instruction   ins   = mul->OperIs(GT_UMULH) ? INS_umull : INS_smull;
            GenTreeInstr* mull  = NewInstrBefore(mul, TYP_LONG, ins, op1, op2);
            GenTreeInstr* instr = MakeInstr(mul, INS_lsr, EA_8BYTE, mull);
            instr->SetImmediate(32);
        }

        return;
    }

    if (GenTreeIntCon* intCon = op2->IsIntCon())
    {
        uint64_t value = static_cast<uint64_t>(intCon->GetValue());

        if (size == EA_4BYTE)
        {
            value &= UINT32_MAX;
        }

        if ((value > 2) && isPow2(value - 1))
        {
            // MUL(x, C) where C is (2 ^ N + 1) can be transformed into ADD(x, x, LSL N)
            // This normally requires making x a LCL_VAR so it can have multiple uses,
            // to avoid that generate a mul instruction with an immediate that codegen
            // will special case to emit an add.

            // TODO-MIKE-CQ: Unfortunately the similar (2 ^ N - 1) case is a bit more
            // complicated and requires either a LCL_VAR for multiple uses or special
            // casing in register allocation as well, since it emits 2 instructions:
            //     lsl t, x, #N
            //     sub d, t, x
            // where t needs to be a temp register or the destination register, if x is
            // marked delay free.

            GenTreeInstr* instr = MakeInstr(mul, INS_mul, size, op1);
            instr->SetImmediate(genLog2(value - 1));

            BlockRange().Remove(op2);

            return;
        }
    }

    instruction ins = INS_mul;

    if (mul->TypeIs(TYP_LONG))
    {
        assert(op1->TypeIs(TYP_LONG));
        assert(op2->TypeIs(TYP_LONG));

        if (GenTreeUnOp* ext1 = IsIntExtend(op1))
        {
            if (GenTreeUnOp* ext2 = IsIntExtend(op2))
            {
                if (ext1->GetOper() == ext2->GetOper())
                {
                    ins = ext1->OperIs(GT_UXT) ? INS_umull : INS_smull;

                    op1 = ext1->GetOp(0);
                    op1->ClearContained();
                    op2 = ext2->GetOp(0);
                    op2->ClearContained();

                    assert(IsLegalToMoveUseForward(ext1, mul, op1));
                    assert(IsLegalToMoveUseForward(ext2, mul, op2));

                    BlockRange().Remove(ext1);
                    BlockRange().Remove(ext2);
                }
            }
            else if (GenTreeIntCon* con = op2->IsIntCon())
            {
                if (ext1->OperIs(GT_UXT) ? FitsIn<uint32_t>(con->GetValue()) : FitsIn<int32_t>(con->GetValue()))
                {
                    ins = ext1->OperIs(GT_UXT) ? INS_umull : INS_smull;

                    op1 = ext1->GetOp(0);
                    op1->ClearContained();

                    op2->SetType(TYP_INT);

                    assert(IsLegalToMoveUseForward(ext1, mul, op1));

                    BlockRange().Remove(ext1);
                }
            }
        }
    }

    MakeInstr(mul, ins, size, op1, op2);
}

void Lowering::LowerUnsignedDiv(GenTreeOp* udiv)
{
    assert(udiv->OperIs(GT_UDIV) && udiv->TypeIs(TYP_INT, TYP_LONG));

    GenTreeIntCon* divisor = udiv->GetOp(1)->IsIntCon();

    if (divisor == nullptr)
    {
        return;
    }

    GenTree* dividend = udiv->GetOp(0);

    if (dividend->IsIntCon())
    {
        // We shouldn't see a UDIV with constant operands here but if we do then it's likely
        // because optimizations are disabled or it's a case that's supposed to throw an
        // exception. Don't optimize this.
        return;
    }

    const var_types type         = udiv->GetType();
    const uint64_t  divisorValue = type == TYP_INT ? divisor->GetUInt32Value() : divisor->GetUInt64Value();

    if (divisorValue == 0)
    {
        return;
    }

    if (isPow2(divisorValue))
    {
        udiv->SetOper(GT_RSZ);
        divisor->SetValue(genLog2(divisorValue));
        ContainCheckShiftRotate(udiv);

        return;
    }

    // If the divisor is greater or equal than 2^(N - 1) then the result is 1
    // iff the dividend is greater or equal than the divisor.
    if (((type == TYP_INT) && (divisorValue > (UINT32_MAX / 2))) ||
        ((type == TYP_LONG) && (divisorValue > (UINT64_MAX / 2))))
    {
        udiv->SetOper(GT_GE);
        udiv->SetRelopUnsigned(true);
        ContainCheckCompare(udiv);

        return;
    }

    if (comp->opts.MinOpts() || (divisorValue < 3))
    {
        return;
    }

    size_t magic;
    bool   increment;
    int    preShift;
    int    postShift;
    bool   simpleMul = false;

    if (type == TYP_INT)
    {
        magic = MagicDivide::GetUnsigned32Magic(static_cast<uint32_t>(divisorValue), &increment, &preShift, &postShift);

        // avoid inc_saturate/multiple shifts by widening to 32x64 MULHI
        if (increment || preShift)
        {
            magic = MagicDivide::GetUnsigned64Magic(divisorValue, &increment, &preShift, &postShift, 32);
        }
        // otherwise just widen to regular multiplication
        else
        {
            postShift += 32;
            simpleMul = true;
        }
    }
    else
    {
        magic = MagicDivide::GetUnsigned64Magic(divisorValue, &increment, &preShift, &postShift);
    }

    GenTree* adjustedDividend = dividend;

    if (increment)
    {
        adjustedDividend = comp->gtNewOperNode(GT_INC_SATURATE, type, adjustedDividend);
        BlockRange().InsertBefore(udiv, adjustedDividend);
        assert(!preShift);
    }
    else if (preShift)
    {
        GenTree* preShiftBy = comp->gtNewIconNode(preShift, TYP_INT);
        adjustedDividend    = comp->gtNewOperNode(GT_RSZ, type, adjustedDividend, preShiftBy);
        BlockRange().InsertBefore(udiv, preShiftBy, adjustedDividend);
        ContainCheckShiftRotate(adjustedDividend->AsOp());
    }
    else if (type != TYP_LONG && !simpleMul)
    {
        adjustedDividend = comp->gtNewOperNode(GT_UXT, TYP_LONG, adjustedDividend);
        BlockRange().InsertBefore(udiv, adjustedDividend);
        LowerUnsignedExtend(adjustedDividend->AsUnOp());
    }

    divisor->SetValue(simpleMul ? TYP_INT : TYP_LONG, magic);

    if (postShift)
    {
        GenTree* mul = NewInstrBefore(udiv, TYP_LONG, simpleMul ? INS_umull : INS_umulh, adjustedDividend, divisor);
        GenTreeInstr* lsr = MakeInstr(udiv, INS_lsr, EA_8BYTE, mul);
        lsr->SetImmediate(postShift);
    }
    else
    {
        MakeInstr(udiv, INS_umulh, EA_8BYTE, adjustedDividend, divisor);
    }
}

GenTree* Lowering::LowerSignedConstDiv(GenTreeOp* div)
{
    assert(div->OperIs(GT_DIV) && div->TypeIs(TYP_INT, TYP_LONG));

    GenTreeIntCon* divisor = div->GetOp(1)->IsIntCon();

    if (divisor == nullptr)
    {
        return nullptr;
    }

    GenTree* dividend = div->GetOp(0);

    if (dividend->IsIntCon())
    {
        // We shouldn't see a DIV with constant operands here but if we do then it's likely
        // because optimizations are disabled or it's a case that's supposed to throw an
        // exception. Don't optimize this.
        return nullptr;
    }

    int64_t divisorValue = divisor->GetValue();

    if (divisorValue == -1 || divisorValue == 0)
    {
        // x / 0 and x % 0 can't be optimized because they are required to throw an exception.

        // x / -1 can't be optimized because INT_MIN / -1 is required to throw an exception.

        // x % -1 is always 0 and the IL spec says that the rem instruction "can" throw an exception if x is
        // the minimum representable integer. However, the C# spec says that an exception "is" thrown in this
        // case so optimizing this case would break C# code.

        // A runtime check could be used to handle this case but it's probably too rare to matter.
        return nullptr;
    }

    const var_types type = div->GetType();

    if ((type == TYP_INT && divisorValue == INT32_MIN) || (type == TYP_LONG && divisorValue == INT64_MIN))
    {
        // If the divisor is the minimum representable integer value then we can use a compare,
        // the result is 1 iff the dividend equals divisor.
        div->SetOper(GT_EQ);

        return div;
    }

    uint64_t absDivisorValue =
        divisorValue == INT64_MIN ? static_cast<uint64_t>(divisorValue) : static_cast<uint64_t>(abs(divisorValue));

    if (!isPow2(absDivisorValue))
    {
        if (comp->opts.MinOpts())
        {
            return nullptr;
        }

        ssize_t magic;
        int     shift;

        if (type == TYP_INT)
        {
            magic = MagicDivide::GetSigned32Magic(static_cast<int32_t>(divisorValue), &shift);
        }
        else
        {
            magic = MagicDivide::GetSigned64Magic(static_cast<int64_t>(divisorValue), &shift);
        }

        divisor->SetValue(magic);

        // Insert a new GT_MULHI node in front of the existing GT_DIV/GT_MOD node.
        // The existing node will later be transformed into a GT_ADD/GT_SUB that
        // computes the final result. This way don't need to find and change the
        // use of the existing node.
        GenTree* mulhi = comp->gtNewOperNode(GT_SMULH, type, divisor, dividend);
        BlockRange().InsertBefore(div, mulhi);

        // mulhi was the easy part. Now we need to generate different code depending
        // on the divisor value:
        // For 3 we need:
        //     div = signbit(smulh) + smulh
        // For 5 we need:
        //     div = signbit(smulh) + sar(smulh, 1) ; requires shift adjust
        // For 7 we need:
        //     mulhi += dividend                    ; requires add adjust
        //     div = signbit(smulh) + sar(smulh, 2) ; requires shift adjust
        // For -3 we need:
        //     mulhi -= dividend                    ; requires sub adjust
        //     div = signbit(smulh) + sar(smulh, 1) ; requires shift adjust
        bool requiresAddSubAdjust     = signum(divisorValue) != signum(magic);
        bool requiresShiftAdjust      = shift != 0;
        bool requiresDividendMultiuse = requiresAddSubAdjust;

        if (requiresDividendMultiuse)
        {
            LIR::Use dividendUse(BlockRange(), &mulhi->AsOp()->gtOp2, mulhi);
            dividend = ReplaceWithLclLoad(dividendUse);
        }

        GenTree* adjusted;

        if (requiresAddSubAdjust)
        {
            dividend = comp->gtNewLclLoad(dividend->AsLclLoad()->GetLcl(), dividend->GetType());
            adjusted = comp->gtNewOperNode(divisorValue > 0 ? GT_ADD : GT_SUB, type, mulhi, dividend);
            BlockRange().InsertBefore(div, dividend, adjusted);
        }
        else
        {
            adjusted = mulhi;
        }

        GenTree* shiftBy = comp->gtNewIconNode(varTypeSize(type) * 8 - 1, type);
        GenTree* signBit = comp->gtNewOperNode(GT_RSZ, type, adjusted, shiftBy);
        BlockRange().InsertBefore(div, shiftBy, signBit);

        LIR::Use adjustedUse(BlockRange(), &signBit->AsOp()->gtOp1, signBit);
        adjusted = ReplaceWithLclLoad(adjustedUse);
        adjusted = comp->gtNewLclLoad(adjusted->AsLclLoad()->GetLcl(), adjusted->GetType());
        BlockRange().InsertBefore(div, adjusted);

        if (requiresShiftAdjust)
        {
            shiftBy  = comp->gtNewIconNode(shift, TYP_INT);
            adjusted = comp->gtNewOperNode(GT_RSH, type, adjusted, shiftBy);
            BlockRange().InsertBefore(div, shiftBy, adjusted);
        }

        div->ChangeOper(GT_ADD);
        div->AsOp()->SetOp(0, adjusted);
        div->AsOp()->SetOp(1, signBit);

        return mulhi;
    }

    // TODO-MIKE-ARM64-CQ: Signed division by 2 generate a LSR that can be combined with
    // the subsequent ADD.

    // We're committed to the conversion now. Go find the use if any.
    LIR::Use use;
    if (!BlockRange().TryGetUse(div, &use))
    {
        return nullptr;
    }

    // We need to use the dividend node multiple times so its value needs to be
    // computed once and stored in a temp variable.
    LIR::Use opDividend(BlockRange(), &div->AsOp()->gtOp1, div);
    dividend = ReplaceWithLclLoad(opDividend);

    GenTree*   shiftBy    = comp->gtNewIconNode(type == TYP_INT ? 31 : 63);
    GenTreeOp* adjustment = comp->gtNewOperNode(GT_RSH, type, dividend, shiftBy);
    BlockRange().InsertAfter(dividend, shiftBy, adjustment);
    ContainCheckShiftRotate(adjustment);

    if (absDivisorValue == 2)
    {
        // If the divisor is +/-2 then we'd end up with a bitwise and between 0/-1 and 1.
        // We can get the same result by using GT_RSZ instead of GT_RSH.
        adjustment->SetOper(GT_RSZ);
    }
    else
    {
        GenTree*   imm  = comp->gtNewIconNode(absDivisorValue - 1, type);
        GenTreeOp* mask = comp->gtNewOperNode(GT_AND, type, adjustment, imm);
        BlockRange().InsertAfter(adjustment, imm, mask);
        ContainCheckBinary(mask);

        adjustment = mask;
    }

    dividend = comp->gtNewLclLoad(dividend->AsLclLoad()->GetLcl(), dividend->GetType());

    GenTreeOp* adjustedDividend = comp->gtNewOperNode(GT_ADD, type, adjustment, dividend);
    BlockRange().InsertAfter(adjustment, dividend, adjustedDividend);
    ContainCheckBinary(adjustedDividend);

    BlockRange().Remove(divisor);

    divisor->SetValue(genLog2(absDivisorValue));

    GenTree* newDivMod = comp->gtNewOperNode(GT_RSH, type, adjustedDividend, divisor);
    BlockRange().InsertAfter(adjustedDividend, divisor, newDivMod);
    ContainCheckShiftRotate(newDivMod->AsOp());

    if (divisorValue < 0)
    {
        GenTree* neg = comp->gtNewOperNode(GT_NEG, type, newDivMod);
        BlockRange().InsertAfter(newDivMod, neg);
        newDivMod = neg;
    }

    use.ReplaceWith(comp, newDivMod);
    BlockRange().Remove(div);

    return newDivMod->gtNext;
}

GenTree* Lowering::LowerSignedDiv(GenTreeOp* div)
{
    assert(div->OperIs(GT_DIV) && div->TypeIs(TYP_INT, TYP_LONG));

    GenTree* next = div->gtNext;

    if (GenTree* newNode = LowerSignedConstDiv(div))
    {
        return newNode;
    }

    return next;
}

void Lowering::LowerSignedExtend(GenTreeUnOp* node)
{
    GenTree* src = node->GetOp(0);

    bool isContainable = IsContainableMemoryOp(src);

    if (GenTreeIndLoad* load = src->IsIndLoad())
    {
        GenTree* addr = load->GetAddr();

        if (load->IsVolatile())
        {
            isContainable = false;
        }
        else if (addr->isContained())
        {
            // Indirs with contained address modes are problematic, thanks in part to messed up
            // address mode formation in LowerArrElem and createAddressNodeForSIMDInit, which
            // produce base+index+offset address modes that are invalid on ARMARCH. Such indirs
            // need a temp register and if the indir itself is contained then nobody's going to
            // reserve it, as this is normally done in LSRA's BuildIndir.
            //
            // Also, when the indir is contained, the type of the generated load instruction may
            // be different from the actual indir type, affecting immediate offset validity.
            //
            // So allow containment if the address mode is definitely always valid: base+index
            // of base+offset, if the offset is valid no matter the indir type is.
            //
            // Perhaps it would be better to not contain the indir and instead retype it
            // and remove the cast. Unfortunately there's at least on case where this is
            // not possible: there's no way to retype the indir in CAST<long>(IND<int>).
            // The best solution would be to lower indir+cast to the actual load instruction
            // to be emitted.

            if (!addr->IsAddrMode())
            {
                isContainable = false;
            }
            else if (addr->AsAddrMode()->HasIndex() && (addr->AsAddrMode()->GetOffset() != 0))
            {
                isContainable = false;
            }
            else if (addr->AsAddrMode()->GetOffset() < -255 || addr->AsAddrMode()->GetOffset() > 255)
            {
                isContainable = false;
            }
        }
    }

    if (isContainable && IsSafeToContainMem(node, src))
    {
        // We can move it right after the source node to avoid the interference check.
        if (node->gtPrev != src)
        {
            BlockRange().Remove(node);
            BlockRange().InsertAfter(src, node);
        }

        src->SetContained();
    }
    else
    {
        src->SetRegOptional();
    }

    if (varTypeIsSmallUnsigned(src->GetType()))
    {
        node->SetOper(GT_UXT);
    }
}

void Lowering::LowerUnsignedExtend(GenTreeUnOp* node)
{
    GenTree* src = node->GetOp(0);

    bool isContainable = IsContainableMemoryOp(src);

    if (GenTreeIndLoad* load = src->IsIndLoad())
    {
        GenTree* addr = load->GetAddr();

        if (load->IsVolatile())
        {
            isContainable = false;
        }
        else if (addr->isContained())
        {
            // Indirs with contained address modes are problematic, thanks in part to messed up
            // address mode formation in LowerArrElem and createAddressNodeForSIMDInit, which
            // produce base+index+offset address modes that are invalid on ARMARCH. Such indirs
            // need a temp register and if the indir itself is contained then nobody's going to
            // reserve it, as this is normally done in LSRA's BuildIndir.
            //
            // Also, when the indir is contained, the type of the generated load instruction may
            // be different from the actual indir type, affecting immediate offset validity.
            //
            // So allow containment if the address mode is definitely always valid: base+index
            // of base+offset, if the offset is valid no matter the indir type is.
            //
            // Perhaps it would be better to not contain the indir and instead retype it
            // and remove the cast. Unfortunately there's at least on case where this is
            // not possible: there's no way to retype the indir in CAST<long>(IND<int>).
            // The best solution would be to lower indir+cast to the actual load instruction
            // to be emitted.

            if (!addr->IsAddrMode())
            {
                isContainable = false;
            }
            else if (addr->AsAddrMode()->HasIndex() && (addr->AsAddrMode()->GetOffset() != 0))
            {
                isContainable = false;
            }
            else if (addr->AsAddrMode()->GetOffset() < -255 || addr->AsAddrMode()->GetOffset() > 255)
            {
                isContainable = false;
            }
        }
    }

    if (isContainable && IsSafeToContainMem(node, src) && !varTypeIsSmallSigned(src->GetType()))
    {
        // We can move it right after the source node to avoid the interference check.
        if (node->gtPrev != src)
        {
            BlockRange().Remove(node);
            BlockRange().InsertAfter(src, node);
        }

        src->SetContained();
    }
    else if (!varTypeIsSmallSigned(src->GetType()) || !src->OperIs(GT_LCL_LOAD))
    {
        src->SetRegOptional();
    }
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
    //     GT(CONV.ubyte(x), 0) is TEST_NE(x, 255)
    //     EQ(CONV.short(x), 0) is TEST_EQ(x, 65535)

    if (cmp->OperIs(GT_EQ, GT_NE, GT_GT) && (op2Value == 0) && op1->OperIs(GT_CONV))
    {
        var_types castType = op1->GetType();

        assert(varTypeIsSmall(castType));

        if (varTypeIsUnsigned(castType) || !cmp->OperIs(GT_GT))
        {
            BlockRange().Remove(op1);

            op1 = op1->AsUnOp()->GetOp(0);
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
            (DecodeBitmaskImm(andInstr) == op2Value))
        {
            op2Value = 0;
            cmp->SetOper(cmp->OperIs(GT_EQ) ? GT_NE : GT_EQ);
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
                op2->SetValue(DecodeBitmaskImm(andInstr));
                cmp->SetOp(1, op2);
                BlockRange().InsertBefore(cmp, op2);
            }

            return cmp;
        }
    }

    if (cmp->OperIs(GT_LT, GT_GE) && (op2Value == 0) && !cmp->IsRelopUnsigned())
    {
        // Materialized signed x < 0 is LSR x, #31/#63.
        // Materialized signed x >= 0 is MVN x, x; LSR x, #31/#63.
        // The later has the same size as the normal CMP/CSET sequence but LSR
        // may combine with a subsequent logical/arithmetic instruction. Even
        // if it doesn't, it's likely still better than the flags based version.

        LIR::Use use;

        if (BlockRange().TryGetUse(cmp, &use) && !use.User()->OperIs(GT_JTRUE))
        {
            if (cmp->OperIs(GT_GE))
            {
                op1 = NewInstrAfter(op1, op1->GetType(), INS_mvn, op1);
                CombineNot(op1->AsInstr());
            }

            emitAttr size = emitActualTypeSize(op1->GetType());

            GenTreeInstr* instr = MakeInstr(cmp, INS_lsr, size, op1);
            instr->SetImmediate(EA_SIZE_IN_BYTES(size) * 8 - 1);
            CombineShiftImmediate(instr);

            BlockRange().Remove(op2);

            return instr->gtNext;
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
    //     cmn x1, x2
    //     b.eq L1
    // or
    //     add x0, x1, x2
    //     cbz L1
    // Older ARM Cortex optimization guides state that instruction variants that set flags
    // are less efficient so it seems that the later variant is preferrable. It's also
    // preferrable to the JIT, considering the rather poor representation of flags in IR.
    // The only advantage of the flags version is avoiding the need for a temp register,
    // which given the rather high number of available registers is not that important.
    //
    // Of course, if the relop isn't used by a branch there's no alternative to the flags
    // version since conditional instructions like CSET do use flags.

    if (cmp->OperIs(GT_EQ, GT_NE) && op1->IsInstr() && (op2Value == 0) && (op1->gtNext == op2) && (op2->gtNext == cmp))
    {
        GenTreeInstr* op1Instr = op1->AsInstr();
        instruction   cmpIns   = GetEquivalentCompareOrTestInstruction(op1Instr);

        if (cmpIns != INS_none)
        {
            op1Instr->SetIns(cmpIns, op1Instr->GetSize(), op1Instr->GetOption());
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
            cc->AsCC()->SetCondition(condition);
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

        ContainImmOperand(cmp, cmp->GetOp(1));
    }

    // TODO-MIKE-CQ: Relop lowering doesn't take advantage of the shifted/extended reg forms of CMP/CMN.
    // Much of the necessary code is already available in LowerArithmetic but transforming a relop into
    // an INST requires finding its user and changing it to SETCC/JCC which more or less require changing
    // a lot of stuff - OptimizeRelopImm which already does something like this in specific cases,
    // LowerJTrue which generates CBZ & co. and possibly liveness/DCE which had some problems with JCCs.

    return cmp->gtNext;
}

GenTree* Lowering::LowerJTrue(GenTreeUnOp* jtrue)
{
    GenTreeOp* relop    = jtrue->GetOp(0)->AsOp();
    GenTree*   relopOp2 = relop->GetOp(1);

    if ((relop->gtNext == jtrue) && relopOp2->IsIntCon())
    {
        bool         useJCMP   = false;
        GenTreeFlags jcmpFlags = GTF_EMPTY;
        size_t       imm       = static_cast<size_t>(relopOp2->AsIntCon()->GetValue());

        if (relop->OperIs(GT_EQ, GT_NE) && (imm == 0))
        {
            // Generate CBZ/CBNZ
            useJCMP   = true;
            jcmpFlags = relop->OperIs(GT_EQ) ? GTF_JCMP_EQ : GTF_EMPTY;
        }
        else if (relop->OperIs(GT_LT, GT_GE) && !relop->IsRelopUnsigned() && (imm == 0))
        {
            // Positive/negative checks can test the sign bit using TBZ/TBNZ
            useJCMP   = true;
            jcmpFlags = GTF_JCMP_TST | (relop->OperIs(GT_GE) ? GTF_JCMP_EQ : GTF_EMPTY);

            if (varTypeIsLong(relop->AsOp()->GetOp(0)->GetType()))
            {
                relopOp2->AsIntCon()->SetValue(63);
            }
            else
            {
                relopOp2->AsIntCon()->SetValue(31);
            }
        }
        else if (relop->OperIs(GT_TEST_EQ, GT_TEST_NE))
        {
            if (!varTypeIsLong(relop->GetOp(0)->GetType()))
            {
                // Discard any spurious sign bits a 32 bit constant may have due to the use of ssize_t.
                imm &= UINT32_MAX;
            }

            if (isPow2(imm))
            {
                // Generate TBZ/TBNZ
                useJCMP   = true;
                jcmpFlags = GTF_JCMP_TST | (relop->OperIs(GT_TEST_EQ) ? GTF_JCMP_EQ : GTF_EMPTY);

                relopOp2->AsIntCon()->SetValue(genLog2(imm));
            }
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

void Lowering::LowerFloatNegate(GenTreeUnOp* neg)
{
    assert(neg->OperIs(GT_FNEG) && varTypeIsFloating(neg->GetType()));

    GenTree* op1  = neg->GetOp(0);
    emitAttr size = emitTypeSize(neg->GetType());

    MakeInstr(neg, INS_fneg, size, op1);
}

void Lowering::LowerFloatArithmetic(GenTreeOp* arith)
{
    assert(arith->OperIs(GT_FADD, GT_FSUB, GT_FMUL, GT_FDIV) && varTypeIsFloating(arith->GetType()));

    static_assert_no_msg(GT_FSUB - GT_FADD == 1);
    static_assert_no_msg(GT_FMUL - GT_FADD == 2);
    static_assert_no_msg(GT_FDIV - GT_FADD == 3);
    static constexpr instruction insMap[]{INS_fadd, INS_fsub, INS_fmul, INS_fdiv};

    GenTree*    op1  = arith->GetOp(0);
    GenTree*    op2  = arith->GetOp(1);
    emitAttr    size = emitTypeSize(arith->GetType());
    instruction ins  = insMap[arith->GetOper() - GT_FADD];

    MakeInstr(arith, ins, size, op1, op2);
}

bool Lowering::IsCallTargetInRange(void* addr)
{
    return Arm64Imm::IsBlImm(reinterpret_cast<ssize_t>(addr), comp);
}

bool Lowering::IsImmOperand(GenTree* operand, GenTree* instr) const
{
    // TODO-CQ: We can contain a floating point 0.0 constant in FCMP

    if (!operand->IsIntCon() || operand->AsIntCon()->ImmedValNeedsReloc(comp))
    {
        return false;
    }

    int64_t  value = operand->AsIntCon()->GetValue();
    emitAttr size  = EA_SIZE(emitActualTypeSize(operand->GetType()));

    switch (instr->GetOper())
    {
        case GT_CMPXCHG:
        case GT_XADD:
            if (comp->compOpportunisticallyDependsOn(InstructionSet_Atomics))
            {
                return false;
            }
            FALLTHROUGH;
        case GT_ADD:
        case GT_SUB:
        case GT_OVF_SADD:
        case GT_OVF_UADD:
        case GT_OVF_SSUB:
        case GT_OVF_USUB:
        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GE:
        case GT_GT:
        case GT_BOUNDS_CHECK:
            return Arm64Imm::IsAddImm(value, size);
        case GT_AND:
        case GT_OR:
        case GT_XOR:
        case GT_TEST_EQ:
        case GT_TEST_NE:
            return Arm64Imm::IsAluImm(value, size);
        case GT_JCMP:
            assert(((instr->gtFlags & GTF_JCMP_TST) == 0) ? (value == 0) : isPow2(value));
            return true;
        case GT_LCL_STORE:
        case GT_LCL_STORE_FLD:
            return value == 0;
        default:
            return false;
    }
}

#ifdef DEBUG
bool Lowering::IsLegalToMoveUseForward(GenTree* oldUser, GenTree* newUser, GenTree* def)
{
    // Moving forward the use of a LCL_LOAD that may be a register candidate requires precautions.
    // The LCL_LOAD node doesn't act like a reg definition, there's only a reg use at the user's
    // location. If the use is moved forward across another definition of the local then that
    // definition's value will be used instead of the previous one.
    //
    // It's not clear if this can ever happen, the LIR is generated from trees that typically
    // have stores only at root or inner but non-interfering stores (e.g. CSE temps).
    //   - This kind of interference is possible in IL but the importer spills trees if it
    //     encounters it.
    //   - There's no forward substitution in LIR (or anywhere else in the JIT).
    //   - VN CopyProp shouldn't be able to introduce such interference because doing so would
    //     break JIT's SSA.
    //
    // So keep this as a debug check for now since it's not clear if the expense of running such
    // checks any time a node is removed from LIR is worthwhile.

    if (!def->OperIs(GT_LCL_LOAD))
    {
        return true;
    }

    LclVarDsc* lcl = def->AsLclLoad()->GetLcl();

    if (lcl->lvDoNotEnregister)
    {
        return true;
    }

    for (GenTree* node = oldUser->gtNext; node != nullptr && node != newUser; node = node->gtNext)
    {
        if (node->OperIs(GT_LCL_STORE) && (node->AsLclStore()->GetLcl() == lcl))
        {
            return false;
        }
    }

    return true;
}
#endif

#endif // TARGET_ARM64
