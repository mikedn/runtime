// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_ARM64

#include "instr.h"
#include "emit.h"
#include "codegen.h"

static bool IsVectorRightShiftIns(instruction ins);

// The return value replaces REG_SP with REG_ZR, SP is encoded using ZR (R31)
static RegNum encodingSPtoZR(RegNum reg)
{
    return (reg == REG_SP) ? REG_ZR : reg;
}

// Returns true if 'value' is a legal signed immediate 26 bit encoding (such as for B or BL).
static bool isValidSimm26(int64_t value)
{
    return (-0x2000000LL <= value) && (value <= 0x1FFFFFFLL);
}

// Returns true if 'value' is a legal signed immediate 19 bit encoding (such as for B.cond, CBNZ, CBZ).
static bool isValidSimm19(int64_t value)
{
    return (-0x40000LL <= value) && (value <= 0x3FFFFLL);
}

// Returns true if 'value' is a legal signed immediate 14 bit encoding (such as for TBNZ, TBZ).
static bool isValidSimm14(int64_t value)
{
    return (-0x2000LL <= value) && (value <= 0x1FFFLL);
}

// Excludes REG_ZR
static bool isGeneralRegister(RegNum reg)
{
    return IsGeneralRegister(reg);
}

// Includes REG_ZR
static bool isGeneralRegisterOrZR(RegNum reg)
{
    return (reg >= REG_INT_FIRST) && (reg <= REG_ZR);
}

// Includes REG_SP, Excludes REG_ZR
static bool isGeneralRegisterOrSP(RegNum reg)
{
    return isGeneralRegister(reg) || (reg == REG_SP);
}

static unsigned getBitWidth(emitAttr size)
{
    assert(size <= EA_8BYTE);
    return EA_BIT_SIZE(size);
}

#ifdef DEBUG

// Returns true if 'reg' represents an integer register.
static bool isIntegerRegister(RegNum reg)
{
    return (reg >= REG_INT_FIRST) && (reg <= REG_INT_LAST);
}

// Returns true if reg encodes for REG_SP or REG_FP, ZR (R31) encodes the SP register.
static bool isStackRegister(RegNum reg)
{
    return (reg == REG_ZR) || (reg == REG_FP);
}

// The return value replaces REG_ZR with REG_SP, ZR (R31) encodes the SP register.
static RegNum encodingZRtoSP(RegNum reg)
{
    return (reg == REG_ZR) ? REG_SP : reg;
}

// Returns true if 'value' is a legal unsigned immediate 8 bit encoding (such as for fMOV).
static bool isValidUimm8(int64_t value)
{
    return (0 <= value) && (value <= 0xFFLL);
}

// Returns true if 'value' is a legal unsigned immediate 12 bit encoding (such as for CMP, CMN).
static bool isValidUimm12(int64_t value)
{
    return (0 <= value) && (value <= 0xFFFLL);
}

// Returns true if 'value' is a legal unsigned immediate 16 bit encoding (such as for MOVZ, MOVN, MOVK).
static bool isValidUimm16(int64_t value)
{
    return (0 <= value) && (value <= 0xFFFFLL);
}

// Returns true if 'value' represents a valid 'bitmask immediate' encoding, any unsigned 13-bit immediate.
static bool isValidImmNRS(size_t value, emitAttr size)
{
    return (value >= 0) && (value < 0x2000);
}

// Returns true if 'value' represents a valid 'halfword immediate' encoding, any unsigned 18-bit immediate.
static bool isValidImmHWVal(size_t value, emitAttr size)
{
    return (value >= 0) && (value < 0x40000);
}

// Returns true if 'value' represents a valid 'byteShifted immediate' encoding, any unsigned 11-bit immediate.
static bool isValidImmBSVal(size_t value, emitAttr size)
{
    return (value >= 0) && (value < 0x800);
}

// Returns true if the imm represents a valid bit shift or bit position for the given 'size' [0..31] or [0..63]
static unsigned isValidImmShift(int64_t imm, emitAttr size)
{
    return (imm >= 0) && (imm < getBitWidth(size));
}

// Returns true if the 'shiftAmount' represents a valid shift for the given 'size'.
static unsigned isValidVectorShiftAmount(int64_t shiftAmount, emitAttr size, bool rightShift)
{
    return (rightShift && (shiftAmount >= 1) && (shiftAmount <= getBitWidth(size))) ||
           ((shiftAmount >= 0) && (shiftAmount < getBitWidth(size)));
}

static bool isValidGeneralDatasize(emitAttr size)
{
    return (size == EA_8BYTE) || (size == EA_4BYTE);
}

static bool isValidScalarDatasize(emitAttr size)
{
    return (size == EA_8BYTE) || (size == EA_4BYTE);
}

static bool isValidGeneralLSDatasize(emitAttr size)
{
    return (size == EA_8BYTE) || (size == EA_4BYTE) || (size == EA_2BYTE) || (size == EA_1BYTE);
}

static bool isValidVectorLSDatasize(emitAttr size)
{
    return (size == EA_16BYTE) || (size == EA_8BYTE) || (size == EA_4BYTE) || (size == EA_2BYTE) || (size == EA_1BYTE);
}

static bool isValidVectorLSPDatasize(emitAttr size)
{
    return (size == EA_16BYTE) || (size == EA_8BYTE) || (size == EA_4BYTE);
}

static bool isValidVectorElemsize(emitAttr size)
{
    return (size == EA_8BYTE) || (size == EA_4BYTE) || (size == EA_2BYTE) || (size == EA_1BYTE);
}

static bool isValidVectorFcvtsize(emitAttr size)
{
    return (size == EA_8BYTE) || (size == EA_4BYTE) || (size == EA_2BYTE);
}

static bool isValidVectorElemsizeFloat(emitAttr size)
{
    return (size == EA_8BYTE) || (size == EA_4BYTE);
}

static bool insOptsPreIndex(insOpts opt)
{
    return opt == INS_OPTS_PRE_INDEX;
}

static bool insOptsLSExtend(insOpts opt)
{
    return (opt == INS_OPTS_NONE) || (opt == INS_OPTS_LSL) || (opt == INS_OPTS_UXTW) || (opt == INS_OPTS_SXTW) ||
           (opt == INS_OPTS_UXTX) || (opt == INS_OPTS_SXTX);
}

static bool insOpts32BitExtend(insOpts opt)
{
    return (opt == INS_OPTS_UXTW) || (opt == INS_OPTS_SXTW);
}

static bool insOptsConvertFloatToFloat(insOpts opt)
{
    return (opt >= INS_OPTS_S_TO_D) && (opt <= INS_OPTS_D_TO_H);
}

static bool insOptsConvertFloatToInt(insOpts opt)
{
    return (opt >= INS_OPTS_S_TO_4BYTE) && (opt <= INS_OPTS_D_TO_8BYTE);
}

static bool insOptsConvertIntToFloat(insOpts opt)
{
    return (opt >= INS_OPTS_4BYTE_TO_S) && (opt <= INS_OPTS_8BYTE_TO_D);
}
#endif // DEBUG

static bool insOptsNone(insOpts opt)
{
    return opt == INS_OPTS_NONE;
}

static bool insOptsAluShift(insOpts opt) // excludes ROR
{
    return (opt >= INS_OPTS_LSL) && (opt <= INS_OPTS_ASR);
}

static bool insOptsIndexed(insOpts opt)
{
    return (opt == INS_OPTS_PRE_INDEX) || (opt == INS_OPTS_POST_INDEX);
}

static bool insOptsAnyExtend(insOpts opt)
{
    return (opt >= INS_OPTS_UXTB) && (opt <= INS_OPTS_SXTX);
}

static bool insOptsLSL(insOpts opt)
{
    return opt == INS_OPTS_LSL;
}

static bool insOptsLSL12(insOpts opt) // special 12-bit shift only used for imm12
{
    return opt == INS_OPTS_LSL12;
}

static bool insOptsAnyShift(insOpts opt)
{
    return (opt >= INS_OPTS_LSL) && (opt <= INS_OPTS_ROR);
}

static bool insOptsPostIndex(insOpts opt)
{
    return opt == INS_OPTS_POST_INDEX;
}

static bool insOptsAnyArrangement(insOpts opt)
{
    return (opt >= INS_OPTS_8B) && (opt <= INS_OPTS_2D);
}

static bool isValidVectorDatasize(emitAttr size)
{
    return (size == EA_16BYTE) || (size == EA_8BYTE);
}

static bool isVectorRegister(RegNum reg)
{
    return IsVectorRegister(reg);
}

insCond JumpKindToInsCond(emitJumpKind kind)
{
    assert((EJ_eq <= kind) && (kind <= EJ_le));

    static const uint8_t map[]{
        0, 0,
#define CC_DEF(cc, rev, cond) INS_COND_##cond,
#include "emitjmps.h"
    };

    assert(kind < _countof(map));
    return static_cast<insCond>(map[kind]);
}

instruction JumpKindToJcc(emitJumpKind kind)
{
    static const instruction map[]{
        INS_nop, INS_b,
#define CC_DEF(cc, rev, cond) INS_b##cc,
#include "emitjmps.h"
    };

    assert(kind < _countof(map));
    return map[kind];
}

emitJumpKind ReverseJumpKind(emitJumpKind kind)
{
    static const uint8_t map[]{
        EJ_NONE, EJ_jmp,
#define CC_DEF(cc, rev, cond) EJ_##rev,
#include "emitjmps.h"
    };

    assert(kind < _countof(map));
    return static_cast<emitJumpKind>(map[kind]);
}

static emitJumpKind JccToJumpKind(instruction ins)
{
    if (ins == INS_b)
    {
        return EJ_jmp;
    }

    assert((INS_beq <= ins) && (ins <= INS_ble));
#define CC_DEF(cc, rev, ...) static_assert_no_msg(INS_b##cc - INS_beq == EJ_##cc - EJ_eq);
#include "emitjmps.h"

    return static_cast<emitJumpKind>(EJ_eq + (ins - INS_beq));
}

enum ID_OPS : uint8_t
{
    ID_OP_NONE,
    ID_OP_JMP,
    ID_OP_CALL
};

static ID_OPS GetFormatOp(insFormat format)
{
    static const ID_OPS ops[]{
#define IF_DEF(en, op1, op2) ID_OP_##op2,
#include "emitfmtsarm64.h"
    };

    assert(format < _countof(ops));
    return ops[format];
}

// Return the allocated size (in bytes) of the given instruction descriptor.
size_t instrDescSmall::GetDescSize() const
{
    if (_idSmallDsc)
    {
        return sizeof(instrDescSmall);
    }

    if (_idLargeCns)
    {
        return sizeof(instrDescCns);
    }

    if (_idLargeCall)
    {
        return sizeof(instrDescCGCA);
    }

    if (GetFormatOp(_idInsFmt) == ID_OP_JMP)
    {
        return sizeof(instrDescJmp);
    }

    return sizeof(instrDesc);
}

int64_t instrDesc::emitGetInsSC() const
{
    return _idLargeCns ? static_cast<const instrDescCns*>(this)->idcCnsVal : _idSmallCns;
}

static emitAttr optGetSrcsize(insOpts conversion);
static emitAttr optGetDstsize(insOpts conversion);
static emitAttr optGetElemsize(insOpts arrangement);
static bool isValidArrangement(emitAttr datasize, insOpts opt);

struct CondImm
{
    insCond   cond;
    insCflags flags;
    uint8_t   imm5;
    uint8_t   pad;
};

static unsigned PackCondImm(insCond cond, insCflags flags = INS_FLAGS_NONE, int imm = 0)
{
    assert(cond <= INS_COND_LE);
    assert(flags <= INS_FLAGS_NZCV);
    assert((imm >= 0) && (imm <= 31));
    return static_cast<unsigned>(cond) | (static_cast<unsigned>(flags) << 4) | (static_cast<unsigned>(imm) << 8);
}

#ifdef DEBUG
static bool IsValidCondImm(int64_t imm)
{
    return (INS_COND_EQ <= imm) && (imm <= INS_COND_LE);
}

static bool IsValidCondFlagsImm(int64_t imm)
{
    return IsValidCondImm(imm & ~0xF0ll);
}

static bool IsValidCondFlagsImm5Imm(int64_t imm)
{
    return IsValidCondImm(imm & ~0x1FF0ll);
}
#endif // DEBUG

static CondImm UnpackCondImm(int64_t imm)
{
    assert(IsValidCondImm(imm));
    return {static_cast<insCond>(imm & 15)};
}

static CondImm UnpackCondFlagsImm(int64_t imm)
{
    assert(IsValidCondFlagsImm(imm));
    return {static_cast<insCond>(imm & 15), static_cast<insCflags>((imm >> 4) & 15)};
}

static CondImm UnpackCondFlagsImm5Imm(int64_t imm)
{
    assert(IsValidCondFlagsImm5Imm(imm));
    return {static_cast<insCond>(imm & 15), static_cast<insCflags>((imm >> 4) & 15),
            static_cast<uint8_t>((imm >> 8) & 31)};
}

// A helper method to return the natural scale for an EA 'size'
static unsigned NaturalScale(emitAttr size)
{
    assert(size == EA_1BYTE || size == EA_2BYTE || size == EA_4BYTE || size == EA_8BYTE || size == EA_16BYTE);

    unsigned result = 0;
    unsigned utemp  = (unsigned)size;

    // Compute log base 2 of utemp (aka 'size')
    while (utemp > 1)
    {
        result++;
        utemp >>= 1;
    }

    return result;
}

// A helper method to perform a Rotate-Right shift operation
// the source is 'value' and it is rotated right by 'sh' bits
// 'value' is considered to be a fixed size 'width' set of bits.
// Example: value is '00001111', sh is 2 and width is 8, result is '11000011'
static uint64_t ImmRor(uint64_t value, unsigned sh, unsigned width)
{
    assert(width <= 64);
    // Check that 'value' fits in 'width' bits
    assert((width == 64) || (value < (1ULL << width)));
    // We don't support shifts >= width
    assert(sh < width);

    uint64_t result;

    unsigned rsh = sh;
    unsigned lsh = width - rsh;

    result = (value >> rsh);
    result |= (value << lsh);

    if (width < 64)
    {
        // mask off any extra bits that we got from the left shift
        result &= ((1ULL << width) - 1);
    }
    return result;
}

// A helper method to perform a 'NOT' bitwise complement operation.
// 'value' is considered to be a fixed size 'width' set of bits.
// Example: value is '01001011', and width is 8, result is '10110100'
static uint64_t ImmNot(uint64_t value, unsigned width)
{
    assert(width <= 64);

    uint64_t result = ~value;

    if (width < 64)
    {
        // Check that 'value' fits in 'width' bits. Don't consider "sign" bits above width.
        uint64_t maxVal       = 1ULL << width;
        uint64_t lowBitsMask  = maxVal - 1;
        uint64_t signBitsMask = ~lowBitsMask | (1ULL << (width - 1)); // The high bits must be set, and the top bit
        // (sign bit) must be set.
        assert((value < maxVal) || ((value & signBitsMask) == signBitsMask));

        // mask off any extra bits that we got from the complement operation
        result &= lowBitsMask;
    }

    return result;
}

// A helper method to perform a bit Replicate operation
// the source is 'value' with a fixed size 'width' set of bits.
// value is replicated to fill out 32 or 64 bits as determined by 'size'.
// Example: value is '11000011' (0xE3), width is 8 and size is EA_8BYTE,
// result is '11000011 11000011 11000011 11000011 11000011 11000011 11000011 11000011' 0xE3E3E3E3E3E3E3E3
static uint64_t ImmReplicate(uint64_t value, unsigned width, emitAttr size)
{
    assert(isValidGeneralDatasize(size));

    unsigned immWidth = (size == EA_8BYTE) ? 64 : 32;
    assert(width <= immWidth);

    uint64_t result     = value;
    unsigned filledBits = width;

    while (filledBits < immWidth)
    {
        value <<= width;
        result |= value;
        filledBits += width;
    }
    return result;
}

// Normalize the 'imm' so that the upper bits, as defined by 'size' are zero
static int64_t ImmNormalize(int64_t imm, emitAttr size)
{
    unsigned immWidth = getBitWidth(size);
    int64_t  result   = imm;

    if (immWidth < 64)
    {
        // Check that 'imm' fits in 'immWidth' bits. Don't consider "sign" bits above width.
        int64_t maxVal      = 1LL << immWidth;
        int64_t lowBitsMask = maxVal - 1;
        int64_t hiBitsMask  = ~lowBitsMask;
        int64_t signBitsMask =
            hiBitsMask | (1LL << (immWidth - 1)); // The high bits must be set, and the top bit (sign bit) must be set.
        assert((imm < maxVal) || ((imm & signBitsMask) == signBitsMask));

        // mask off the hiBits
        result &= lowBitsMask;
    }

    return result;
}

#ifdef DEBUG
static int64_t DecodeHalfwordImm(int64_t imm)
{
    return (imm & 0xFFFF) << ((imm >> 16) << 4);
}
#endif

static bool EncodeHalfwordImm(int64_t imm, emitAttr size, unsigned* encodedImm = nullptr)
{
    assert(isValidGeneralDatasize(size));

    imm = ImmNormalize(imm, size) & (size == EA_8BYTE ? UINT64_MAX : UINT32_MAX);

    for (int shift = 0, bitSize = size == EA_8BYTE ? 64 : 32; shift < bitSize; shift += 16)
    {
        if ((imm & jitstd::rotl<uint64_t>(~0xFFFFull, shift)) == 0)
        {
            if (encodedImm != nullptr)
            {
                *encodedImm = static_cast<unsigned>((imm >> shift) & 0xFFFFu) | ((shift >> 4) << 16);
                assert(DecodeHalfwordImm(*encodedImm) == imm);
            }

            return true;
        }
    }

    return false;
}

static unsigned PackBitMaskImm(int s, int r, emitAttr size)
{
    return (s & 63) | ((r & 63) << 6) | ((size == EA_8BYTE) << 12);
}

#ifdef DEBUG
static void UnpackBitMaskImm(int64_t imm, int* s, int* r, int* n)
{
    *s = imm & 63;
    *r = (imm >> 6) & 63;
    *n = (imm >> 12) & 1;
}
#endif

static int64_t DecodeBitMaskImm(int64_t imm, emitAttr size)
{
    assert(isValidGeneralDatasize(size));

    unsigned N = (imm >> 12) & 1;
    unsigned R = (imm >> 6) & 63;
    unsigned S = imm & 63;

    unsigned elemWidth;

    if (N == 0)
    {
        elemWidth = 32;

        while ((elemWidth > 1) && ((S & elemWidth) != 0))
        {
            elemWidth /= 2;
        }
    }
    else
    {
        assert(size == EA_8BYTE);

        elemWidth = 64;
    }

    unsigned maskSR = elemWidth - 1;

    S &= maskSR;
    R &= maskSR;
    S++;
    assert(S < elemWidth);

    return ImmReplicate(ImmRor((1ull << S) - 1, R, elemWidth), elemWidth, size);
}

static bool EncodeBitMaskImm(int64_t imm, emitAttr size, unsigned* encodedImm = nullptr)
{
    assert(isValidGeneralDatasize(size));

    unsigned immWidth = size == EA_8BYTE ? 64 : 32;
    unsigned maxLen   = size == EA_8BYTE ? 6 : 5;

    imm = ImmNormalize(imm, size);

    // Starting with len=1, elemWidth is 2 bits
    //            len=2, elemWidth is 4 bits
    //            len=3, elemWidth is 8 bits
    //            len=4, elemWidth is 16 bits
    //            len=5, elemWidth is 32 bits
    // (optionally)  len=6, elemWidth is 64 bits

    for (unsigned len = 1; len <= maxLen; len++)
    {
        unsigned elemWidth = 1 << len;
        uint64_t elemMask  = ((uint64_t)-1) >> (64 - elemWidth);
        uint64_t tempImm   = (uint64_t)imm;      // A working copy of 'imm' that we can mutate
        uint64_t elemVal   = tempImm & elemMask; // The low 'elemWidth' bits of 'imm'

        // Check for all 1's or 0's as these can't be encoded
        if ((elemVal == 0) || (elemVal == elemMask))
            continue;

        // 'checkedBits' is the count of bits that are known to match 'elemVal' when replicated
        unsigned checkedBits = elemWidth; // by definition the first 'elemWidth' bits match

        // Now check to see if each of the next bits match...
        //
        while (checkedBits < immWidth)
        {
            tempImm >>= elemWidth;

            uint64_t nextElem = tempImm & elemMask;
            if (nextElem != elemVal)
            {
                // Not matching, exit this loop and checkedBits will not be equal to immWidth
                break;
            }

            // The 'nextElem' is matching, so increment 'checkedBits'
            checkedBits += elemWidth;
        }

        // Did the full immediate contain bits that can be formed by repeating 'elemVal'?
        if (checkedBits == immWidth)
        {
            // We are not quite done, since the only values that we can encode as a
            // 'bitmask immediate' are those that can be formed by starting with a
            // bit string of 0*1* that is rotated by some number of bits.
            //
            // We check to see if 'elemVal' can be formed using these restrictions.
            //
            // Observation:
            // Rotating by one bit any value that passes these restrictions
            // can be xor-ed with the original value and will result it a string
            // of bits that have exactly two 1 bits: 'elemRorXor'
            // Further the distance between the two one bits tells us the value
            // of S and the location of the 1 bits tells us the value of R
            //
            // Some examples:   (immWidth is 8)
            //
            // S=4,R=0   S=5,R=3   S=3,R=6
            // elemVal:        00001111  11100011  00011100
            // elemRor:        10000111  11110001  00001110
            // elemRorXor:     10001000  00010010  00010010
            //   compute S  45678---  ---5678-  ---3210-
            //   compute R  01234567  ---34567  ------67

            uint64_t elemRor    = ImmRor(elemVal, 1, elemWidth); // Rotate 'elemVal' Right by one bit
            uint64_t elemRorXor = elemVal ^ elemRor;             // Xor elemVal and elemRor

            // If we only have a two-bit change in elemROR then we can form a mask for this value
            unsigned bitCount = 0;
            uint64_t oneBit   = 0x1;
            unsigned R        = elemWidth; // R is shift count for ROR (rotate right shift)
            unsigned S        = 0;         // S is number of consecutive one bits
            int      incr     = -1;

            // Loop over the 'elemWidth' bits in 'elemRorXor'
            for (unsigned bitNum = 0; bitNum < elemWidth; bitNum++)
            {
                if (incr == -1)
                {
                    R--; // We decrement R by one whenever incr is -1
                }
                if (bitCount == 1)
                {
                    S += incr; // We incr/decr S, after we find the first one bit in 'elemRorXor'
                }

                // Is this bit position a 1 bit in 'elemRorXor'?
                //
                if (oneBit & elemRorXor)
                {
                    bitCount++;
                    // Is this the first 1 bit that we found in 'elemRorXor'?
                    if (bitCount == 1)
                    {
                        // Does this 1 bit represent a transition to zero bits?
                        bool toZeros = ((oneBit & elemVal) != 0);
                        if (toZeros)
                        {
                            // S :: Count down from elemWidth
                            S    = elemWidth;
                            incr = -1;
                        }
                        else // this 1 bit represent a transition to one bits.
                        {
                            // S :: Count up from zero
                            S    = 0;
                            incr = +1;
                        }
                    }
                    else // bitCount > 1
                    {
                        // We found the second (or third...) 1 bit in 'elemRorXor'
                        incr = 0; // stop decrementing 'R'

                        if (bitCount > 2)
                        {
                            // More than 2 transitions from 0/1 in 'elemVal'
                            // This means that 'elemVal' can't be encoded
                            // using a 'bitmask immediate'.
                            //
                            // Furthermore, it will continue to fail
                            // with any larger 'len' that we try.
                            // so just return false.
                            //
                            return false;
                        }
                    }
                }

                // shift oneBit left by one bit to test the next position
                oneBit <<= 1;
            }

            // We expect that bitCount will always be two at this point
            // but just in case return false for any bad cases.
            assert(bitCount == 2);

            if (bitCount != 2)
            {
                return false;
            }

            // Perform some sanity checks on the values of 'S' and 'R'
            assert(S > 0);
            assert(S < elemWidth);
            assert(R < elemWidth);

            if (encodedImm != nullptr)
            {
                S--;

                if (len != 6)
                {
                    // The encoding used for 'S' here is a bit peculiar.
                    // The upper bits need to be complemented, followed by a zero bit
                    // then the value of 'S-1'
                    S |= 64 - (1 << (len + 1));
                }

                *encodedImm = PackBitMaskImm(S, R, len == 6 ? EA_8BYTE : EA_4BYTE);
                assert(imm == DecodeBitMaskImm(*encodedImm, size));
            }

            return true;
        }
    }

    return false;
}

bool Arm64Imm::IsBitMaskImm(int64_t value, emitAttr size, unsigned* imm)
{
    return EncodeBitMaskImm(value, size, imm);
}

bool Arm64Imm::IsAluImm(int64_t value, emitAttr size)
{
    return EncodeBitMaskImm(value, size);
}

int64_t Arm64Imm::DecodeBitMaskImm(unsigned imm, emitAttr size)
{
    return ::DecodeBitMaskImm(imm, size);
}

bool Arm64Imm::IsMovImm(int64_t imm, emitAttr size)
{
    return EncodeHalfwordImm(imm, size, nullptr) || EncodeHalfwordImm(ImmNot(imm, getBitWidth(size)), size, nullptr) ||
           EncodeBitMaskImm(imm, size, nullptr);
}

struct MoviImm
{
    instruction ins;
    uint8_t     imm;
    uint8_t     shift;
    bool        msl;

    MoviImm() : ins(INS_invalid), imm(0), shift(0), msl(0)
    {
    }

    MoviImm(instruction ins, uint64_t imm, unsigned shift = 0, bool msl = false)
        : ins(ins), imm(static_cast<uint8_t>(imm)), shift(static_cast<uint8_t>(shift)), msl(msl)
    {
        assert(imm <= UINT8_MAX);
        assert(shift <= 24);
    }
};

static MoviImm EncodeMoviImm(uint64_t value, insOpts opt)
{
    auto msl   = [](uint64_t value, unsigned shift) { return ((value & 0xFF) << shift) | ~(UINT64_MAX << shift); };
    auto lsl   = [](uint64_t value, unsigned shift) { return (value & 0xFF) << shift; };
    auto not8  = [](uint64_t value) { return ~value & UINT8_MAX; };
    auto not16 = [](uint64_t value) { return ~value & UINT16_MAX; };
    auto not32 = [](uint64_t value) { return ~value & UINT32_MAX; };

    switch (opt)
    {
        case INS_OPTS_1D:
        case INS_OPTS_2D:
        {
            uint64_t imm = 0;

            for (unsigned bit = 0; value != 0; value >>= 8, bit++)
            {
                if ((value & 0xFF) == 0)
                {
                    continue;
                }

                if ((value & 0xFF) == 0xFF)
                {
                    imm |= 1ULL << bit;
                    continue;
                }

                return MoviImm();
            }

            return MoviImm(INS_movi, imm);
        }

        case INS_OPTS_2S:
        case INS_OPTS_4S:
            value &= UINT32_MAX;

            for (unsigned shift = 0; shift <= 24; shift += 8)
            {
                uint64_t imm = (value >> shift) & 0xFF;

                if (lsl(imm, shift) == value)
                {
                    return MoviImm(INS_movi, imm, shift);
                }

                if (((shift == 8) || (shift == 16)) && (msl(imm, shift) == value))
                {
                    return MoviImm(INS_movi, imm, shift, true);
                }

                imm = not8(imm);

                if (not32(lsl(imm, shift)) == value)
                {
                    return MoviImm(INS_mvni, imm, shift);
                }

                if (((shift == 8) || (shift == 16)) && (not32(msl(imm, shift)) == value))
                {
                    return MoviImm(INS_mvni, imm, shift, true);
                }
            }

            return MoviImm();

        case INS_OPTS_4H:
        case INS_OPTS_8H:
            value &= UINT16_MAX;

            for (unsigned shift = 0; shift <= 8; shift += 8)
            {
                uint64_t imm = (value >> shift) & 0xFF;

                if (lsl(imm, shift) == value)
                {
                    return MoviImm(INS_movi, imm, shift);
                }

                imm = not8(imm);

                if (not16(lsl(imm, shift)) == value)
                {
                    return MoviImm(INS_mvni, imm, shift);
                }
            }

            return MoviImm();

        case INS_OPTS_8B:
        case INS_OPTS_16B:
            return MoviImm(INS_movi, value & 0xFF);

        default:
            unreached();
    }
}

bool Arm64Imm::IsMoviImm(uint64_t value, insOpts opts)
{
    return EncodeMoviImm(value, opts).ins != INS_invalid;
}

#ifdef DEBUG
static unsigned DecodeByteShiftedImm(unsigned imm, emitAttr size)
{
    unsigned value = imm & 255;
    unsigned shift = (imm >> 8) & 3;

    if (shift == 0)
    {
        return value;
    }

    assert((size == EA_2BYTE) || (size == EA_4BYTE));
    assert(shift < (size == EA_2BYTE ? 2u : 4u));

    value <<= shift * 8;

    if ((imm >> 10) & 1)
    {
        value |= (1 << (shift * 8)) - 1;
    }

    return value;
}
#endif

static bool EncodeByteShiftedImm(int64_t imm, emitAttr size, unsigned* encodedImm)
{
    assert((size == EA_2BYTE) || (size == EA_4BYTE));

    imm = ImmNormalize(imm, size) & (size == EA_4BYTE ? UINT32_MAX : UINT16_MAX);

    const unsigned bitSize   = size == EA_4BYTE ? 32 : 16;
    bool           onesShift = false;

    for (unsigned shift = 0; shift < bitSize; shift += 8)
    {
        int mask = 255 << shift;

        if ((imm & ~mask) == 0)
        {
            if (encodedImm != nullptr)
            {
                unsigned imm8 = static_cast<unsigned>((imm & mask) >> shift);
                // TODO-MIKE-Review: onesShift is always false. In general, this
                // things look incomplete and likely not tested, lowering doesn't
                // contain constant vector operands for BIC/ORR.
                *encodedImm = (imm8 & 255) | (((shift >> 3) & 3) << 8) | (onesShift << 10);
                assert(imm == DecodeByteShiftedImm(*encodedImm, size));
            }

            return true;
        }
    }

    return false;
}

static bool IsShiftBy12Imm(int64_t imm)
{
    if (imm < 0)
    {
        imm = -imm;
    }

    if (imm < 0)
    {
        return false; // Must be MIN_INT64
    }

    if ((imm & 0xfff) != 0)
    {
        return false;
    }

    imm >>= 12;

    return imm <= 0x0fff;
}

bool Arm64Imm::IsAddImm(int64_t imm, emitAttr size)
{
    return (unsigned_abs(imm) <= 0x0fff) || IsShiftBy12Imm(imm);
}

// true if this 'imm' can be encoded as the offset in a ldr/str instruction
bool Arm64Imm::IsLdStImm(int64_t imm, emitAttr attr)
{
    if (imm == 0)
        return true; // Encodable using IF_LS_2A

    if ((imm >= -256) && (imm <= 255))
        return true; // Encodable using IF_LS_2C (or possibly IF_LS_2B)

    if (imm < 0)
        return false; // not encodable

    emitAttr size  = EA_SIZE(attr);
    unsigned scale = NaturalScale(size);
    int64_t  mask  = size - 1; // the mask of low bits that must be zero to encode the immediate

    if (((imm & mask) == 0) && ((imm >> scale) < 0x1000))
        return true; // Encodable using IF_LS_2B

    return false; // not encodable
}

#ifdef DEBUG
static double DecodeFMovImm(int64_t imm8)
{
    assert((0 <= imm8) && (imm8 <= 0x0ff));

    unsigned sign  = (imm8 >> 7) & 1;
    unsigned exp   = ((imm8 >> 4) & 7) ^ 0x4;
    unsigned mant  = (imm8 & 15) + 16;
    unsigned scale = 16 * 8;

    while (exp > 0)
    {
        scale /= 2;
        exp--;
    }

    double result = static_cast<double>(mant) / static_cast<double>(scale);

    return sign != 0 ? -result : result;
}
#endif

static bool IsFMovImm(double value, unsigned* encodedImm)
{
    int sign = 0;

    if (value < 0.0)
    {
        value = -value;
        sign  = 1;
    }

    int exp = 0;

    while ((value < 1.0) && (exp >= -4))
    {
        value *= 2.0;
        exp--;
    }

    while ((value >= 2.0) && (exp <= 5))
    {
        value *= 0.5;
        exp++;
    }

    exp += 3;
    value *= 16.0;

    if ((exp < 0) || (exp > 7))
    {
        return false;
    }

    int ival = static_cast<int>(value);

    if (value != static_cast<double>(ival))
    {
        return false;
    }

    if (encodedImm != nullptr)
    {
        ival -= 16;
        assert((ival >= 0) && (ival <= 15));
        *encodedImm = (ival & 15) | (((exp ^ 4) & 7) << 4) | ((sign & 1) << 7);
    }

    return true;
}

static unsigned EncodeFMovImm(double value)
{
    unsigned imm;
    bool     canEncode = IsFMovImm(value, &imm);
    assert(canEncode);
    return imm;
}

bool Arm64Imm::IsFMovImm(double value)
{
    return ::IsFMovImm(value, nullptr);
}

bool Arm64Imm::IsBlImm(int64_t addr, Compiler* compiler)
{
    // On arm64, we always assume a call target is in range and generate a 28-bit relative
    // 'bl' instruction. If this isn't sufficient range, the VM will generate a jump stub when
    // we call recordRelocation(). See the IMAGE_REL_ARM64_BRANCH26 case in jitinterface.cpp
    // (for JIT) or zapinfo.cpp (for NGEN). If we cannot allocate a jump stub, it is fatal.
    return true;
}

#ifdef DEBUG
void EmitterBase::emitInsSanityCheck(instrDesc* id)
{
    switch (id->idInsFmt())
    {
        instruction ins;
        emitAttr    elemsize;
        emitAttr    datasize;
        emitAttr    dstsize;
        emitAttr    srcsize;
        int64_t     imm;
        unsigned    immShift;
        int64_t     index;
        int64_t     index2;

        case IF_BI_0A: // ......iiiiiiiiii iiiiiiiiiiiiiiii               simm26:00
        case IF_BI_0B: // ......iiiiiiiiii iiiiiiiiiiii....               simm19:00
            break;

        case IF_NOP_JMP:
        case IF_LARGEJMP:
        case IF_LARGEADR:
        case IF_LARGELDC:
            // TODO-MIKE-Review: Shouldn't LARGEADR/LARGELDC check for registers?
            break;

        case IF_SMALLADR:
            assert(isGeneralRegister(id->idReg1()));
            break;

        case IF_BI_0C: // ......iiiiiiiiii iiiiiiiiiiiiiiii               simm26:00
            break;

        case IF_BI_1A: // ......iiiiiiiiii iiiiiiiiiiittttt      Rt       simm19:00
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            break;

        case IF_BI_1B: // B.......bbbbbiii iiiiiiiiiiittttt      Rt imm6, simm14:00
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isValidImmShift(id->emitGetInsSC(), id->idOpSize()));
            break;

        case IF_BR_1A: // ................ ......nnnnn.....         Rn
            assert(isGeneralRegister(id->idReg1()));
            break;

        case IF_BR_1B: // ................ ......nnnnn.....         Rn
            assert(isGeneralRegister(id->idReg3()));
            break;

        case IF_LS_1A: // .X......iiiiiiii iiiiiiiiiiittttt      Rt    PC imm(1MB)
            assert(isGeneralRegister(id->idReg1()) || isVectorRegister(id->idReg1()));
            assert(insOptsNone(id->idInsOpt()));
            break;

        case IF_LS_2A:                                // LS_2A   .X.......X...... ......nnnnnttttt      Rt Rn
            assert(isIntegerRegister(id->idReg1()) || // ZR
                   isVectorRegister(id->idReg1()));
            assert(isIntegerRegister(id->idReg2())); // SP
            assert(id->emitGetInsSC() == 0);
            assert(insOptsNone(id->idInsOpt()));
            break;

        case IF_LS_2B:                                // .X.......Xiiiiii iiiiiinnnnnttttt      Rt Rn    imm(0-4095)
            assert(isIntegerRegister(id->idReg1()) || // ZR
                   isVectorRegister(id->idReg1()));
            assert(isIntegerRegister(id->idReg2())); // SP
            assert(isValidUimm12(id->emitGetInsSC()));
            assert(insOptsNone(id->idInsOpt()));
            break;

        case IF_LS_2C: // .X.......X.iiiii iiiiPPnnnnnttttt      Rt Rn    imm(-256..+255) no/pre/post inc
            assert(isIntegerRegister(id->idReg1()) || // ZR
                   isVectorRegister(id->idReg1()));
            assert(isIntegerRegister(id->idReg2())); // SP
            assert(id->emitGetInsSC() >= -0x100);
            assert(id->emitGetInsSC() < 0x100);
            assert(insOptsNone(id->idInsOpt()) || insOptsIndexed(id->idInsOpt()));
            break;

        case IF_LS_2D: // .Q.............. ....ssnnnnnttttt      Vt Rn
        case IF_LS_2E: // .Q.............. ....ssnnnnnttttt      Vt Rn
        case IF_LS_2F: // .Q.............. xx.Sssnnnnnttttt      Vt[] Rn
        case IF_LS_2G: // .Q.............. xx.Sssnnnnnttttt      Vt[] Rn
            assert(isVectorRegister(id->idReg1()));
            assert(isIntegerRegister(id->idReg2())); // SP
            if (insOptsAnyArrangement(id->idInsOpt()))
            {
                datasize = id->idOpSize();
                assert(isValidVectorDatasize(datasize));
                assert(isValidArrangement(id->idOpSize(), id->idInsOpt()));
            }
            else
            {
                elemsize = id->idOpSize();
                assert(isValidVectorElemsize(elemsize));
                assert(insOptsNone(id->idInsOpt()) || insOptsPostIndex(id->idInsOpt()));
            }
            assert(!id->idIsLclVar());
            break;

        case IF_LS_3A:                                // .X.......X.mmmmm oooS..nnnnnttttt      Rt Rn Rm ext(Rm) LSL {}
            assert(isIntegerRegister(id->idReg1()) || // ZR
                   isVectorRegister(id->idReg1()));
            assert(isIntegerRegister(id->idReg2())); // SP
            assert(isGeneralRegister(id->idReg3()));
            assert(insOptsLSExtend(id->idInsOpt()));
            break;

        case IF_LS_3B: // X............... .aaaaannnnnttttt      Rt Ra Rn
            assert((isValidGeneralDatasize(id->idOpSize()) && isIntegerRegister(id->idReg1())) ||
                   (isValidVectorLSPDatasize(id->idOpSize()) && isVectorRegister(id->idReg1())));
            assert(isIntegerRegister(id->idReg1()) || // ZR
                   isVectorRegister(id->idReg1()));
            assert(isIntegerRegister(id->idReg2()) || // ZR
                   isVectorRegister(id->idReg2()));
            assert(isIntegerRegister(id->idReg3())); // SP
            assert(id->emitGetInsSC() == 0);
            assert(insOptsNone(id->idInsOpt()));
            break;

        case IF_LS_3C: // X.........iiiiii iaaaaannnnnttttt      Rt Ra Rn imm(im7,sh)
            assert((isValidGeneralDatasize(id->idOpSize()) && isIntegerRegister(id->idReg1())) ||
                   (isValidVectorLSPDatasize(id->idOpSize()) && isVectorRegister(id->idReg1())));
            assert(isIntegerRegister(id->idReg1()) || // ZR
                   isVectorRegister(id->idReg1()));
            assert(isIntegerRegister(id->idReg2()) || // ZR
                   isVectorRegister(id->idReg2()));
            assert(isIntegerRegister(id->idReg3())); // SP
            assert(id->emitGetInsSC() >= -0x40);
            assert(id->emitGetInsSC() < 0x40);
            assert(insOptsNone(id->idInsOpt()) || insOptsIndexed(id->idInsOpt()));
            break;

        case IF_LS_3D: // .X.......X.mmmmm ......nnnnnttttt      Wm Rt Rn
            assert(isIntegerRegister(id->idReg1()));
            assert(isIntegerRegister(id->idReg2()));
            assert(isIntegerRegister(id->idReg3()));
            assert(id->emitGetInsSC() == 0);
            assert(!id->idIsLclVar());
            assert(insOptsNone(id->idInsOpt()));
            break;

        case IF_LS_3E: // .X.........mmmmm ......nnnnnttttt      Rm Rt Rn ARMv8.1 LSE Atomics
            assert(isIntegerRegister(id->idReg1()));
            assert(isIntegerRegister(id->idReg2()));
            assert(isIntegerRegister(id->idReg3()));
            assert(id->emitGetInsSC() == 0);
            assert(!id->idIsLclVar());
            assert(insOptsNone(id->idInsOpt()));
            break;

        case IF_LS_3F: // .Q.........mmmmm ....ssnnnnnttttt      Vt Rn Rm
        case IF_LS_3G: // .Q.........mmmmm ...Sssnnnnnttttt      Vt[] Rn Rm
            assert(isVectorRegister(id->idReg1()));
            assert(isIntegerRegister(id->idReg2())); // SP
            assert(isGeneralRegister(id->idReg3()));
            if (insOptsAnyArrangement(id->idInsOpt()))
            {
                datasize = id->idOpSize();
                assert(isValidVectorDatasize(datasize));
                assert(isValidArrangement(id->idOpSize(), id->idInsOpt()));
            }
            else
            {
                elemsize = id->idOpSize();
                assert(isValidVectorElemsize(elemsize));
                assert(insOptsNone(id->idInsOpt()) || insOptsPostIndex(id->idInsOpt()));
            }
            assert(!id->idIsLclVar());
            break;

        case IF_DI_1A: // X.......shiiiiii iiiiiinnnnn.....         Rn    imm(i12,sh)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isValidUimm12(id->emitGetInsSC()));
            assert(insOptsNone(id->idInsOpt()) || insOptsLSL12(id->idInsOpt()));
            break;

        case IF_DI_1B: // X........hwiiiii iiiiiiiiiiiddddd      Rd       imm(i16,hw)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isValidImmHWVal(id->emitGetInsSC(), id->idOpSize()));
            break;

        case IF_DI_1C: // X........Nrrrrrr ssssssnnnnn.....         Rn    imm(N,r,s)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isValidImmNRS(id->emitGetInsSC(), id->idOpSize()));
            break;

        case IF_DI_1D: // X........Nrrrrrr ssssss.....ddddd      Rd       imm(N,r,s)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isIntegerRegister(id->idReg1())); // SP
            assert(isValidImmNRS(id->emitGetInsSC(), id->idOpSize()));
            break;

        case IF_DI_1E: // .ii.....iiiiiiii iiiiiiiiiiiddddd      Rd       simm21
            assert(isGeneralRegister(id->idReg1()));
            break;

        case IF_DI_1F: // X..........iiiii cccc..nnnnn.nzcv      Rn imm5  nzcv cond
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(IsValidCondFlagsImm5Imm(id->emitGetInsSC()));
            break;

        case IF_DI_2A: // X.......shiiiiii iiiiiinnnnnddddd      Rd Rn    imm(i12,sh)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isIntegerRegister(id->idReg1())); // SP
            assert(isIntegerRegister(id->idReg2())); // SP
            assert(isValidUimm12(id->emitGetInsSC()));
            assert(insOptsNone(id->idInsOpt()) || insOptsLSL12(id->idInsOpt()));
            break;

        case IF_DI_2B: // X.........Xnnnnn ssssssnnnnnddddd      Rd Rn    imm(0-63)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(isValidImmShift(id->emitGetInsSC(), id->idOpSize()));
            break;

        case IF_DI_2C: // X........Nrrrrrr ssssssnnnnnddddd      Rd Rn    imm(N,r,s)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isIntegerRegister(id->idReg1())); // SP
            assert(isGeneralRegister(id->idReg2()));
            assert(isValidImmNRS(id->emitGetInsSC(), id->idOpSize()));
            break;

        case IF_DI_2D: // X........Nrrrrrr ssssssnnnnnddddd      Rd Rn    imr, imms   (N,r,s)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegisterOrZR(id->idReg2()));
            assert(isValidImmNRS(id->emitGetInsSC(), id->idOpSize()));
            break;

        case IF_DR_1D: // X............... cccc.......ddddd      Rd       cond
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(IsValidCondImm(id->emitGetInsSC()));
            break;

        case IF_DR_2A: // X..........mmmmm ......nnnnn.....         Rn Rm
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            break;

        case IF_DR_2B: // X.......sh.mmmmm ssssssnnnnn.....         Rn Rm {LSL,LSR,ASR,ROR} imm(0-63)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isIntegerRegister(id->idReg1())); // ZR
            assert(isGeneralRegister(id->idReg2()));
            assert(isValidImmShift(id->emitGetInsSC(), id->idOpSize()));
            if (!insOptsNone(id->idInsOpt()))
            {
                if (id->idIns() == INS_tst) // tst allows ROR, cmp/cmn don't
                {
                    assert(insOptsAnyShift(id->idInsOpt()));
                }
                else
                {
                    assert(insOptsAluShift(id->idInsOpt()));
                }
            }
            assert(insOptsNone(id->idInsOpt()) || (id->emitGetInsSC() > 0));
            break;

        case IF_DR_2C: // X..........mmmmm ooosssnnnnn.....         Rn Rm ext(Rm) LSL imm(0-4)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isIntegerRegister(id->idReg1())); // SP
            assert(isGeneralRegister(id->idReg2()));
            assert(insOptsNone(id->idInsOpt()) || insOptsLSL(id->idInsOpt()) || insOptsAnyExtend(id->idInsOpt()));
            assert(id->emitGetInsSC() >= 0);
            assert(id->emitGetInsSC() <= 4);
            if (insOptsLSL(id->idInsOpt()))
            {
                assert(id->emitGetInsSC() > 0);
            }
            break;

        case IF_DR_2D: // X..........nnnnn cccc..nnnnnmmmmm      Rd Rn    cond
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(IsValidCondImm(id->emitGetInsSC()));
            break;

        case IF_DR_2E: // X..........mmmmm ...........ddddd      Rd    Rm
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isIntegerRegister(id->idReg2())); // ZR
            break;

        case IF_DR_2F: // X.......sh.mmmmm ssssss.....ddddd      Rd    Rm {LSL,LSR,ASR} imm(0-63)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(isValidImmShift(id->emitGetInsSC(), id->idOpSize()));
            assert(insOptsNone(id->idInsOpt()) || insOptsAluShift(id->idInsOpt()));
            assert(insOptsNone(id->idInsOpt()) || (id->emitGetInsSC() > 0));
            break;

        case IF_DR_2G: // X............... ......nnnnnddddd      Rd    Rm
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isIntegerRegister(id->idReg1())); // SP
            assert(isIntegerRegister(id->idReg2())); // SP
            break;

        case IF_DR_2H: // X........X...... ......nnnnnddddd      Rd Rn
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            break;

        case IF_DR_2I: // X..........mmmmm cccc..nnnnn.nzcv      Rn Rm    nzcv cond
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(IsValidCondFlagsImm(id->emitGetInsSC()));
            break;

        case IF_DR_3A: // X..........mmmmm ......nnnnnmmmmm      Rd Rn Rm
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isIntegerRegister(id->idReg1())); // SP
            assert(isIntegerRegister(id->idReg2())); // SP
            assert(isGeneralRegister(id->idReg3()));
            assert(insOptsNone(id->idInsOpt()));
            break;

        case IF_DR_3B: // X.......sh.mmmmm ssssssnnnnnddddd      Rd Rn Rm {LSL,LSR,ASR,ROR} imm(0-63)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(isGeneralRegister(id->idReg3()));
            assert(isValidImmShift(id->emitGetInsSC(), id->idOpSize()));
            assert(insOptsNone(id->idInsOpt()) || insOptsAnyShift(id->idInsOpt()));
            assert(insOptsNone(id->idInsOpt()) || (id->emitGetInsSC() > 0));
            break;

        case IF_DR_3C: // X..........mmmmm ooosssnnnnnddddd      Rd Rn Rm ext(Rm) LSL imm(0-4)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isIntegerRegister(id->idReg1())); // SP
            assert(isIntegerRegister(id->idReg2())); // SP
            assert(isGeneralRegister(id->idReg3()));
            assert(insOptsNone(id->idInsOpt()) || insOptsLSL(id->idInsOpt()) || insOptsAnyExtend(id->idInsOpt()));
            assert(id->emitGetInsSC() >= 0);
            assert(id->emitGetInsSC() <= 4);
            if (insOptsLSL(id->idInsOpt()))
            {
                assert((id->emitGetInsSC() > 0) ||
                       (id->idReg2() == REG_ZR)); // REG_ZR encodes SP and we allow a shift of zero
            }
            break;

        case IF_DR_3D: // X..........mmmmm cccc..nnnnnmmmmm      Rd Rn Rm cond
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(isGeneralRegister(id->idReg3()));
            assert(IsValidCondImm(id->emitGetInsSC()));
            break;

        case IF_DR_3E: // X........X.mmmmm ssssssnnnnnddddd      Rd Rn Rm imm(0-63)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(isGeneralRegister(id->idReg3()));
            assert(isValidImmShift(id->emitGetInsSC(), id->idOpSize()));
            assert(insOptsNone(id->idInsOpt()));
            break;

        case IF_DR_4A: // X..........mmmmm .aaaaannnnnddddd      Rd Rn Rm Ra
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(isGeneralRegister(id->idReg3()));
            assert(isGeneralRegister(id->idReg4()));
            break;

        case IF_DV_1A: // .........X.iiiii iii........ddddd      Vd imm8    (fmov - immediate scalar)
            assert(insOptsNone(id->idInsOpt()));
            elemsize = id->idOpSize();
            assert(isValidVectorElemsizeFloat(elemsize));
            assert(isVectorRegister(id->idReg1()));
            assert(isValidUimm8(id->emitGetInsSC()));
            break;

        case IF_DV_1B: // .QX..........iii cmod..iiiiiddddd      Vd imm8    (immediate vector)
            ins      = id->idIns();
            imm      = id->emitGetInsSC() & 0x0ff;
            immShift = (id->emitGetInsSC() & 0x700) >> 8;
            assert(immShift >= 0);
            datasize = id->idOpSize();
            assert(isValidVectorDatasize(datasize));
            assert(isValidArrangement(datasize, id->idInsOpt()));
            elemsize = optGetElemsize(id->idInsOpt());
            if (ins == INS_fmov)
            {
                assert(isValidVectorElemsizeFloat(elemsize));
                assert(id->idInsOpt() != INS_OPTS_1D); // Reserved encoding
                assert(immShift == 0);
            }
            else
            {
                assert(isValidVectorElemsize(elemsize));
                assert((immShift != 4) && (immShift != 7)); // always invalid values
                if (ins != INS_movi)                        // INS_mvni, INS_orr, INS_bic
                {
                    assert((elemsize != EA_1BYTE) && (elemsize != EA_8BYTE)); // only H or S
                    if (elemsize == EA_2BYTE)
                    {
                        assert(immShift < 2);
                    }
                    else // (elemsize == EA_4BYTE)
                    {
                        if (ins != INS_mvni)
                        {
                            assert(immShift < 4);
                        }
                    }
                }
            }
            assert(isVectorRegister(id->idReg1()));
            assert(isValidUimm8(imm));
            break;

        case IF_DV_1C: // .........X...... ......nnnnn.....      Vn #0.0    (fcmp - with zero)
            assert(insOptsNone(id->idInsOpt()));
            elemsize = id->idOpSize();
            assert(isValidVectorElemsizeFloat(elemsize));
            assert(isVectorRegister(id->idReg1()));
            break;

        case IF_DV_2A: // .Q.......X...... ......nnnnnddddd      Vd Vn      (fabs, fcvt - vector)
        case IF_DV_2M: // .Q......XX...... ......nnnnnddddd      Vd Vn      (abs, neg   - vector)
        case IF_DV_2P: // ................ ......nnnnnddddd      Vd Vn      (aes*, sha1su1)
            assert(isValidVectorDatasize(id->idOpSize()));
            assert(isValidArrangement(id->idOpSize(), id->idInsOpt()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            break;

        case IF_DV_2N: // .........iiiiiii ......nnnnnddddd      Vd Vn imm   (shift - scalar)
            ins      = id->idIns();
            datasize = id->idOpSize();
            assert(insOptsNone(id->idInsOpt()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            assert(isValidVectorShiftAmount(id->emitGetInsSC(), datasize, IsVectorRightShiftIns(ins)));
            break;

        case IF_DV_2O: // .Q.......iiiiiii ......nnnnnddddd      Vd Vn imm   (shift - vector)
            ins      = id->idIns();
            datasize = id->idOpSize();
            elemsize = optGetElemsize(id->idInsOpt());
            assert(isValidVectorDatasize(datasize));
            assert(isValidArrangement(datasize, id->idInsOpt()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            assert(isValidVectorShiftAmount(id->emitGetInsSC(), elemsize, IsVectorRightShiftIns(ins)));
            break;

        case IF_DV_2B: // .Q.........iiiii ......nnnnnddddd      Rd Vn[]  (umov/smov    - to general)
            elemsize = id->idOpSize();
            index    = id->emitGetInsSC();
            assert(insOptsNone(id->idInsOpt()));
            assert(Arm64Imm::IsVecIndex(index, EA_16BYTE, elemsize));
            assert(isValidVectorElemsize(elemsize));
            assert(isGeneralRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            break;

        case IF_DV_2C: // .Q.........iiiii ......nnnnnddddd      Vd Rn    (dup/ins - vector from general)
            if (id->idIns() == INS_dup)
            {
                datasize = id->idOpSize();
                assert(isValidVectorDatasize(datasize));
                assert(isValidArrangement(datasize, id->idInsOpt()));
                elemsize = optGetElemsize(id->idInsOpt());
            }
            else // INS_ins
            {
                datasize = EA_16BYTE;
                elemsize = id->idOpSize();
                assert(isValidVectorElemsize(elemsize));
            }
            assert(isVectorRegister(id->idReg1()));
            assert(isGeneralRegisterOrZR(id->idReg2()));
            break;

        case IF_DV_2D: // .Q.........iiiii ......nnnnnddddd      Vd Vn[]  (dup - vector)
            ins      = id->idIns();
            datasize = id->idOpSize();
            assert(isValidVectorDatasize(datasize));
            assert(isValidArrangement(datasize, id->idInsOpt()));
            elemsize = optGetElemsize(id->idInsOpt());
            index    = id->emitGetInsSC();
            assert((ins == INS_dup) || Arm64Imm::IsVecIndex(index, datasize, elemsize));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            break;

        case IF_DV_2E: // ...........iiiii ......nnnnnddddd      Vd Vn[]  (dup - scalar)
            elemsize = id->idOpSize();
            index    = id->emitGetInsSC();
            assert(Arm64Imm::IsVecIndex(index, EA_16BYTE, elemsize));
            assert(isValidVectorElemsize(elemsize));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            break;

        case IF_DV_2F: // ...........iiiii .jjjj.nnnnnddddd      Vd[] Vn[] (ins - element)
            imm      = id->emitGetInsSC();
            index    = (imm >> 4) & 0xf;
            index2   = imm & 0xf;
            elemsize = id->idOpSize();
            assert(isValidVectorElemsize(elemsize));
            assert(Arm64Imm::IsVecIndex(index, EA_16BYTE, elemsize));
            assert(Arm64Imm::IsVecIndex(index2, EA_16BYTE, elemsize));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            break;

        case IF_DV_2L: // ........XX...... ......nnnnnddddd      Vd Vn      (abs, neg - scalar)
            assert(insOptsNone(id->idInsOpt()));
            assert(isValidVectorElemsize(id->idOpSize()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            break;

        case IF_DV_2G: // .........X...... ......nnnnnddddd      Vd Vn      (fmov, fcvtXX - register)
        case IF_DV_2K: // .........X.mmmmm ......nnnnn.....      Vn Vm      (fcmp)
            assert(insOptsNone(id->idInsOpt()));
            assert(isValidVectorElemsizeFloat(id->idOpSize()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            break;

        case IF_DV_2H: // X........X...... ......nnnnnddddd      Rd Vn      (fmov/fcvtXX - to general)
            assert(insOptsConvertFloatToInt(id->idInsOpt()));
            dstsize = optGetDstsize(id->idInsOpt());
            srcsize = optGetSrcsize(id->idInsOpt());
            assert(isValidGeneralDatasize(dstsize));
            assert(isValidVectorElemsizeFloat(srcsize));
            assert(dstsize == id->idOpSize());
            assert(isGeneralRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            break;

        case IF_DV_2I: // X........X...... ......nnnnnddddd      Vd Rn      (fmov/Xcvtf - from general)
            assert(insOptsConvertIntToFloat(id->idInsOpt()));
            dstsize = optGetDstsize(id->idInsOpt());
            srcsize = optGetSrcsize(id->idInsOpt());
            assert(isValidGeneralDatasize(srcsize));
            assert(isValidVectorElemsizeFloat(dstsize));
            assert(dstsize == id->idOpSize());
            assert(isVectorRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            break;

        case IF_DV_2J: // ........SS.....D D.....nnnnnddddd      Vd Vn      (fcvt)
            assert(insOptsConvertFloatToFloat(id->idInsOpt()));
            dstsize = optGetDstsize(id->idInsOpt());
            srcsize = optGetSrcsize(id->idInsOpt());
            assert(isValidVectorFcvtsize(srcsize));
            assert(isValidVectorFcvtsize(dstsize));
            assert(dstsize == id->idOpSize());
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            break;

        case IF_DV_2Q: // .........X...... ......nnnnnddddd      Sd Vn      (faddp, fmaxnmp, fmaxp, fminnmp,
                       // fminp - scalar)
            if (id->idOpSize() == EA_16BYTE)
            {
                assert(id->idInsOpt() == INS_OPTS_2D);
            }
            else
            {
                assert(id->idOpSize() == EA_8BYTE);
                assert(id->idInsOpt() == INS_OPTS_2S);
            }
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            break;

        case IF_DV_2R: // .Q.......X...... ......nnnnnddddd      Sd Vn      (fmaxnmv, fmaxv, fminnmv, fminv)
            assert(id->idOpSize() == EA_16BYTE);
            assert(id->idInsOpt() == INS_OPTS_4S);
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            break;

        case IF_DV_2S: // ........XX...... ......nnnnnddddd      Sd Vn      (addp - scalar)
            assert(id->idOpSize() == EA_16BYTE);
            assert(id->idInsOpt() == INS_OPTS_2D);
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            break;

        case IF_DV_2T: // .Q......XX...... ......nnnnnddddd      Sd Vn      (addv, saddlv, smaxv, sminv, uaddlv,
                       // umaxv, uminv)
            assert(isValidVectorDatasize(id->idOpSize()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            break;

        case IF_DV_2U: // ................ ......nnnnnddddd      Sd Sn    (sha1h)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            break;

        case IF_DV_3A: // .Q......XX.mmmmm ......nnnnnddddd      Vd Vn Vm   (vector)
            assert(isValidVectorDatasize(id->idOpSize()));
            assert(isValidArrangement(id->idOpSize(), id->idInsOpt()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            assert(isVectorRegister(id->idReg3()));
            elemsize = optGetElemsize(id->idInsOpt());
            ins      = id->idIns();
            if (ins == INS_mul)
            {
                assert(elemsize != EA_8BYTE); // can't use 2D or 1D
            }
            else if (ins == INS_pmul)
            {
                assert(elemsize == EA_1BYTE); // only supports 8B or 16B
            }
            break;

        case IF_DV_3AI: // .Q......XXLMmmmm ....H.nnnnnddddd      Vd Vn Vm[] (vector by element)
            assert(isValidVectorDatasize(id->idOpSize()));
            assert(isValidArrangement(id->idOpSize(), id->idInsOpt()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            assert(isVectorRegister(id->idReg3()));
            elemsize = optGetElemsize(id->idInsOpt());
            assert(Arm64Imm::IsVecIndex(id->emitGetInsSC(), EA_16BYTE, elemsize));
            assert((elemsize == EA_2BYTE) || (elemsize == EA_4BYTE));
            break;

        case IF_DV_3B: // .Q.......X.mmmmm ......nnnnnddddd      Vd Vn Vm   (vector)
            assert(isValidVectorDatasize(id->idOpSize()));
            assert(isValidArrangement(id->idOpSize(), id->idInsOpt()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            assert(isVectorRegister(id->idReg3()));
            break;

        case IF_DV_3BI: // .Q.......XLmmmmm ....H.nnnnnddddd      Vd Vn Vm[] (vector by element)
            assert(isValidVectorDatasize(id->idOpSize()));
            assert(isValidArrangement(id->idOpSize(), id->idInsOpt()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            assert(isVectorRegister(id->idReg3()));
            elemsize = optGetElemsize(id->idInsOpt());
            assert(Arm64Imm::IsVecIndex(id->emitGetInsSC(), EA_16BYTE, elemsize));
            break;

        case IF_DV_3C: // .Q.........mmmmm ......nnnnnddddd      Vd Vn Vm   (vector)
            switch (id->idIns())
            {
                case INS_tbl:
                case INS_tbl_2regs:
                case INS_tbl_3regs:
                case INS_tbl_4regs:
                case INS_tbx:
                case INS_tbx_2regs:
                case INS_tbx_3regs:
                case INS_tbx_4regs:
                    elemsize = optGetElemsize(id->idInsOpt());
                    assert(elemsize == EA_1BYTE);
                    break;
                default:
                    break;
            }
            assert(isValidVectorDatasize(id->idOpSize()));
            assert(isValidArrangement(id->idOpSize(), id->idInsOpt()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            assert(isVectorRegister(id->idReg3()));
            break;

        case IF_DV_3D: // .........X.mmmmm ......nnnnnddddd      Vd Vn Vm   (scalar)
            assert(isValidScalarDatasize(id->idOpSize()));
            assert(insOptsNone(id->idInsOpt()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            assert(isVectorRegister(id->idReg3()));
            break;

        case IF_DV_3DI: // .........XLmmmmm ....H.nnnnnddddd      Vd Vn Vm[] (scalar by element)
            assert(isValidScalarDatasize(id->idOpSize()));
            assert(insOptsNone(id->idInsOpt()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            assert(isVectorRegister(id->idReg3()));
            elemsize = id->idOpSize();
            assert(Arm64Imm::IsVecIndex(id->emitGetInsSC(), EA_16BYTE, elemsize));
            break;

        case IF_DV_3E: // ........XX.mmmmm ......nnnnnddddd      Vd Vn Vm  (scalar)
            assert(isValidVectorElemsize(id->idOpSize()));
            assert(insOptsNone(id->idInsOpt()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            assert(isVectorRegister(id->idReg3()));
            elemsize = id->idOpSize();
            index    = id->emitGetInsSC();
            assert(Arm64Imm::IsVecIndex(index, EA_16BYTE, elemsize));
            break;

        case IF_DV_3EI: // ........XXLMmmmm ....H.nnnnnddddd      Vd Vn Vm[] (scalar by element)
            assert(isValidVectorElemsize(id->idOpSize()));
            assert(insOptsNone(id->idInsOpt()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            assert(isVectorRegister(id->idReg3()));
            elemsize = id->idOpSize();
            index    = id->emitGetInsSC();
            assert(Arm64Imm::IsVecIndex(index, EA_16BYTE, elemsize));
            break;

        case IF_DV_3F: // ...........mmmmm ......nnnnnddddd      Vd Vn Vm
            assert(isValidVectorDatasize(id->idOpSize()));
            assert(isValidArrangement(id->idOpSize(), id->idInsOpt()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            assert(isVectorRegister(id->idReg3()));
            break;

        case IF_DV_3G: // .Q.........mmmmm .iiii.nnnnnddddd      Vd Vn Vm imm (vector)
            assert(isValidVectorDatasize(id->idOpSize()));
            assert(isValidArrangement(id->idOpSize(), id->idInsOpt()));
            assert(Arm64Imm::IsVecIndex(id->emitGetInsSC(), id->idOpSize(), EA_1BYTE));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            assert(isVectorRegister(id->idReg3()));
            break;

        case IF_DV_4A: // .........X.mmmmm .aaaaannnnnddddd      Rd Rn Rm Ra (scalar)
            assert(isValidGeneralDatasize(id->idOpSize()));
            assert(isVectorRegister(id->idReg1()));
            assert(isVectorRegister(id->idReg2()));
            assert(isVectorRegister(id->idReg3()));
            assert(isVectorRegister(id->idReg4()));
            break;

        case IF_SN_0A: // ................ ................
        case IF_SI_0A: // ...........iiiii iiiiiiiiiii.....               imm16
        case IF_SI_0B: // ................ ....bbbb........               imm4 - barrier
            break;

        case IF_SR_1A: // ................ ...........ttttt      Rt       (dc zva)
            datasize = id->idOpSize();
            assert(isGeneralRegister(id->idReg1()));
            assert(datasize == EA_8BYTE);
            break;

        default:
            printf("unexpected format %s\n", emitIfName(id->idInsFmt()));
            assert(!"Unexpected format");
            break;
    }
}
#endif // DEBUG

static bool IsStoreIns(instruction ins);

class Arm64Encoder final : public Encoder
{
public:
    Arm64Encoder(Arm64Emitter& emit, GCInfo& gcInfo) : Encoder(emit, gcInfo)
    {
    }

    void EncodeInstr(insGroup* ig, instrDesc* id, uint8_t** dp);

private:
    // Emit the 32-bit Arm64 instruction 'code' into the 'dst'  buffer
    unsigned emitOutput_Instr(uint8_t* dst, uint32_t code);

    uint8_t* emitOutputLJ(uint8_t* dst, instrDescJmp* id, insGroup* ig);
    uint8_t* emitOutputDL(uint8_t* dst, instrDescJmp* id);
    uint8_t* emitOutputLoadLabel(uint8_t* dst, uint8_t* srcAddr, uint8_t* dstAddr, instrDescJmp* id);
    uint8_t* emitOutputShortBranch(uint8_t* dst, instruction ins, insFormat fmt, int64_t distVal, instrDescJmp* id);
    uint8_t* emitOutputShortAddress(uint8_t* dst, instruction ins, int64_t distance, RegNum reg);
    uint8_t* emitOutputShortConstant(
        uint8_t* dst, instruction ins, insFormat fmt, int64_t distVal, RegNum reg, emitAttr opSize);

#ifdef DEBUG
    void PrintIns(instrDesc* id, uint8_t* code, size_t sz);
#endif
};

// Returns true if instruction "id->idIns()" writes to a register that might be used to contain a GC
// pointer. This exempts the SP and PC registers, and floating point registers. Memory access
// instructions that pre- or post-increment their memory address registers are *not* considered to write
// to GC registers, even if that memory address is a by-ref: such an instruction cannot change the GC
// status of that register, since it must be a byref before and remains one after.
// This may return false positives.
static bool emitInsMayWriteToGCReg(instrDesc* id)
{
    instruction ins = id->idIns();
    insFormat   fmt = id->idInsFmt();

    switch (fmt)
    {
        // These are the formats with "destination" registers:

        // TODO-MIKE-Review: Is this missing IF_LARGEADR/IF_LARGELDC?
        case IF_SMALLADR:

        case IF_DI_1B: // X........hwiiiii iiiiiiiiiiiddddd      Rd       imm(i16,hw)
        case IF_DI_1D: // X........Nrrrrrr ssssss.....ddddd      Rd       imm(N,r,s)
        case IF_DI_1E: // .ii.....iiiiiiii iiiiiiiiiiiddddd      Rd       simm21
        case IF_DI_2A: // X.......shiiiiii iiiiiinnnnnddddd      Rd Rn    imm(i12,sh)
        case IF_DI_2B: // X.........Xnnnnn ssssssnnnnnddddd      Rd Rn    imm(0-63)
        case IF_DI_2C: // X........Nrrrrrr ssssssnnnnnddddd      Rd Rn    imm(N,r,s)
        case IF_DI_2D: // X........Nrrrrrr ssssssnnnnnddddd      Rd Rn    imr, imms   (N,r,s)
        case IF_DR_1D: // X............... cccc.......ddddd      Rd       cond
        case IF_DR_2D: // X..........nnnnn cccc..nnnnnddddd      Rd Rn    cond
        case IF_DR_2E: // X..........mmmmm ...........ddddd      Rd    Rm
        case IF_DR_2F: // X.......sh.mmmmm ssssss.....ddddd      Rd    Rm {LSL,LSR,ASR} imm(0-63)
        case IF_DR_2G: // X............... ......nnnnnddddd      Rd Rn
        case IF_DR_2H: // X........X...... ......nnnnnddddd      Rd Rn
        case IF_DR_3A: // X..........mmmmm ......nnnnnddddd      Rd Rn Rm
        case IF_DR_3B: // X.......sh.mmmmm ssssssnnnnnddddd      Rd Rn Rm {LSL,LSR,ASR} imm(0-63)
        case IF_DR_3C: // X..........mmmmm xxxsssnnnnnddddd      Rd Rn Rm ext(Rm) LSL imm(0-4)
        case IF_DR_3D: // X..........mmmmm cccc..nnnnnddddd      Rd Rn Rm cond
        case IF_DR_3E: // X........X.mmmmm ssssssnnnnnddddd      Rd Rn Rm imm(0-63)
        case IF_DR_4A: // X..........mmmmm .aaaaannnnnddddd      Rd Rn Rm Ra
        case IF_DV_2B: // .Q.........iiiii ......nnnnnddddd      Rd Vn[]    (umov - to general)
        case IF_DV_2H: // X........X...... ......nnnnnddddd      Rd Vn      (fmov - to general)
            return true;

        case IF_DV_2C: // .Q.........iiiii ......nnnnnddddd      Vd Rn      (dup/ins - vector from general)
        case IF_DV_2D: // .Q.........iiiii ......nnnnnddddd      Vd Vn[]    (dup - vector)
        case IF_DV_2E: // ...........iiiii ......nnnnnddddd      Vd Vn[]    (dup - scalar)
        case IF_DV_2F: // ...........iiiii .jjjj.nnnnnddddd      Vd[] Vn[]  (ins - element)
        case IF_DV_2G: // .........X...... ......nnnnnddddd      Vd Vn      (fmov, fcvtXX - register)
        case IF_DV_2I: // X........X...... ......nnnnnddddd      Vd Rn      (fmov - from general)
        case IF_DV_2J: // ........SS.....D D.....nnnnnddddd      Vd Vn      (fcvt)
        case IF_DV_2K: // .........X.mmmmm ......nnnnn.....      Vn Vm      (fcmp)
        case IF_DV_2L: // ........XX...... ......nnnnnddddd      Vd Vn      (abs, neg - scalar)
        case IF_DV_2M: // .Q......XX...... ......nnnnnddddd      Vd Vn      (abs, neg - vector)
        case IF_DV_2P: // ................ ......nnnnnddddd      Vd Vn      (aes*, sha1su1) - Vd both source and
                       // destination

        case IF_DV_2Q: // .........X...... ......nnnnnddddd      Sd Vn      (faddp, fmaxnmp, fmaxp, fminnmp,
                       // fminp - scalar)
        case IF_DV_2R: // .Q.......X...... ......nnnnnddddd      Sd Vn      (fmaxnmv, fmaxv, fminnmv, fminv)
        case IF_DV_2S: // ........XX...... ......nnnnnddddd      Sd Vn      (addp - scalar)

        case IF_DV_3A:  // .Q......XX.mmmmm ......nnnnnddddd      Vd Vn Vm   (vector)
        case IF_DV_3AI: // .Q......XXLMmmmm ....H.nnnnnddddd      Vd Vn Vm[] (vector)
        case IF_DV_3B:  // .Q.......X.mmmmm ......nnnnnddddd      Vd Vn Vm   (vector)
        case IF_DV_3BI: // .Q.......XLmmmmm ....H.nnnnnddddd      Vd Vn Vm[] (vector by element)
        case IF_DV_3C:  // .Q.........mmmmm ......nnnnnddddd      Vd Vn Vm   (vector)
        case IF_DV_3D:  // .........X.mmmmm ......nnnnnddddd      Vd Vn Vm   (scalar)
        case IF_DV_3DI: // .........XLmmmmm ....H.nnnnnddddd      Vd Vn Vm[] (scalar by element)
        case IF_DV_3E:  // ........XX.mmmmm ......nnnnnddddd      Vd Vn Vm   (scalar)
        case IF_DV_3EI: // ........XXLMmmmm ....H.nnnnnddddd      Vd Vn Vm[] (scalar by element)
        case IF_DV_3F:  // .Q......XX.mmmmm ......nnnnnddddd      Vd Vn Vm   (vector)
        case IF_DV_3G:  // .Q.........mmmmm .iiii.nnnnnddddd      Vd Vn Vm imm (vector)
        case IF_DV_4A:  // .........X.mmmmm .aaaaannnnnddddd      Vd Va Vn Vm (scalar)
            // Tracked GC pointers cannot be placed into the SIMD registers.
            return false;

        // These are the load/store formats with "target" registers:

        case IF_LS_1A: // XX...V..iiiiiiii iiiiiiiiiiittttt      Rt    PC imm(1MB)
        case IF_LS_2A: // .X.......X...... ......nnnnnttttt      Rt Rn
        case IF_LS_2B: // .X.......Xiiiiii iiiiiinnnnnttttt      Rt Rn    imm(0-4095)
        case IF_LS_2C: // .X.......X.iiiii iiiiP.nnnnnttttt      Rt Rn    imm(-256..+255) pre/post inc
        case IF_LS_2D: // .Q.............. ....ssnnnnnttttt      Vt Rn
        case IF_LS_2E: // .Q.............. ....ssnnnnnttttt      Vt Rn
        case IF_LS_2F: // .Q.............. xx.Sssnnnnnttttt      Vt[] Rn
        case IF_LS_2G: // .Q.............. xx.Sssnnnnnttttt      Vt[] Rn
        case IF_LS_3A: // .X.......X.mmmmm xxxS..nnnnnttttt      Rt Rn Rm ext(Rm) LSL {}
        case IF_LS_3B: // X............... .aaaaannnnnttttt      Rt Ra Rn
        case IF_LS_3C: // X.........iiiiii iaaaaannnnnttttt      Rt Ra Rn imm(im7,sh)
        case IF_LS_3D: // .X.......X.mmmmm ......nnnnnttttt      Wm Rt Rn
        case IF_LS_3F: // .Q.........mmmmm ....ssnnnnnttttt      Vt Rn Rm
        case IF_LS_3G: // .Q.........mmmmm ...Sssnnnnnttttt      Vt[] Rn Rm

            // For the Store instructions the "target" register is actually a "source" value

            if (IsStoreIns(ins))
            {
                return false;
            }
            else
            {
                assert(IsLoadIns(ins));
                return true;
            }

        case IF_LS_3E: // LS_3E   .X.........mmmmm ......nnnnnttttt      Rm Rt Rn ARMv8.1 LSE Atomics
            // ARMv8.1 Atomics
            assert(IsStoreIns(ins));
            assert(IsLoadIns(ins));
            return true;

        default:
            assert(fmt != IF_GC_REG);
            return false;
    }
}

static bool emitInsMayWriteMultipleRegs(instrDesc* id)
{
    switch (id->idIns())
    {
        case INS_ldp:
        case INS_ldpsw:
        case INS_ldnp:
            return true;
        default:
            return false;
    }
}

#ifdef DEBUG
// Takes an instrDesc 'id' and uses the instruction 'ins' to determine the
// size of the target register that is written or read by the instruction.
// Note that even if EA_4BYTE is returned a load instruction will still
// always zero the upper 4 bytes of the target register.
// This method is required so that we can distinguish between loads that are
// sign-extending as they can have two different sizes for their target register.
// Additionally for instructions like 'ldr' and 'str' these can load/store
// either 4 byte or 8 bytes to/from the target register.
// By convention the small unsigned load instructions are considered to write
// a 4 byte sized target register, though since these also zero the upper 4 bytes
// they could equally be considered to write the unsigned value to full 8 byte register.
static emitAttr emitInsTargetRegSize(instrDesc* id)
{
    // This is used to determine the size of the target registers for a load/store instruction

    switch (id->idIns())
    {
        case INS_ldxrb:
        case INS_ldarb:
        case INS_ldaxrb:
        case INS_stxrb:
        case INS_stlrb:
        case INS_stlxrb:
        case INS_ldrb:
        case INS_strb:
        case INS_ldurb:
        case INS_sturb:
            return EA_4BYTE;
        case INS_ldxrh:
        case INS_ldarh:
        case INS_ldaxrh:
        case INS_stxrh:
        case INS_stlrh:
        case INS_stlxrh:
        case INS_ldrh:
        case INS_strh:
        case INS_ldurh:
        case INS_sturh:
            return EA_4BYTE;
        case INS_ldrsb:
        case INS_ldursb:
        case INS_ldrsh:
        case INS_ldursh:
            return id->idOpSize() == EA_8BYTE ? EA_8BYTE : EA_4BYTE;
        case INS_ldrsw:
        case INS_ldursw:
        case INS_ldpsw:
            return EA_8BYTE;
        case INS_ldp:
        case INS_stp:
        case INS_ldnp:
        case INS_stnp:
            return id->idOpSize();
        case INS_ldxr:
        case INS_ldar:
        case INS_ldaxr:
        case INS_stxr:
        case INS_stlr:
        case INS_stlxr:
        case INS_ldr:
        case INS_str:
        case INS_ldur:
        case INS_stur:
            return id->idOpSize();
        default:
            unreached();
    }
}

// Takes an instrDesc and uses the instruction to determine the 'size' of the
// data that is loaded from memory.
static emitAttr emitInsLoadStoreSize(instrDesc* id)
{
    // The 'result' returned is the 'size' of the data that is loaded from memory.

    switch (id->idIns())
    {
        case INS_ldarb:
        case INS_stlrb:
        case INS_ldrb:
        case INS_strb:
        case INS_ldurb:
        case INS_sturb:
        case INS_ldrsb:
        case INS_ldursb:
            return EA_1BYTE;
        case INS_ldarh:
        case INS_stlrh:
        case INS_ldrh:
        case INS_strh:
        case INS_ldurh:
        case INS_sturh:
        case INS_ldrsh:
        case INS_ldursh:
            return EA_2BYTE;
        case INS_ldrsw:
        case INS_ldursw:
        case INS_ldpsw:
            return EA_4BYTE;
        case INS_ldp:
        case INS_stp:
        case INS_ldnp:
        case INS_stnp:
            return id->idOpSize();
        case INS_ldar:
        case INS_stlr:
        case INS_ldr:
        case INS_str:
        case INS_ldur:
        case INS_stur:
            return id->idOpSize();
        default:
            unreached();
    }
}

const char* insName(instruction ins)
{
    // clang-format off
    static const char* const insNames[]
    {
#define INST1(id, nm, ldst, fmt, e1                                 ) nm,
#define INST2(id, nm, ldst, fmt, e1, e2                             ) nm,
#define INST3(id, nm, ldst, fmt, e1, e2, e3                         ) nm,
#define INST4(id, nm, ldst, fmt, e1, e2, e3, e4                     ) nm,
#define INST5(id, nm, ldst, fmt, e1, e2, e3, e4, e5                 ) nm,
#define INST6(id, nm, ldst, fmt, e1, e2, e3, e4, e5, e6             ) nm,
#define INST9(id, nm, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9 ) nm,
#include "instrsarm64.h"
    };
    // clang-format on

    assert(ins < _countof(insNames));
    assert(insNames[ins] != nullptr);

    return insNames[ins];
}
#endif // DEBUG

enum
{
    IF_EN9 = IF_COUNT + 1,
    IF_EN6A,
    IF_EN6B,
    IF_EN5A,
    IF_EN5B,
    IF_EN5C,
    IF_EN4A,
    IF_EN4B,
    IF_EN4C,
    IF_EN4D,
    IF_EN4E,
    IF_EN4F,
    IF_EN4G,
    IF_EN4H,
    IF_EN4I,
    IF_EN4J,
    IF_EN4K,
    IF_EN3A,
    IF_EN3B,
    IF_EN3C,
    IF_EN3D,
    IF_EN3E,
    IF_EN3F,
    IF_EN3G,
    IF_EN3H,
    IF_EN3I,
    IF_EN3J,
    IF_EN2A,
    IF_EN2B,
    IF_EN2C,
    IF_EN2D,
    IF_EN2E,
    IF_EN2F,
    IF_EN2G,
    IF_EN2H,
    IF_EN2I,
    IF_EN2J,
    IF_EN2K,
    IF_EN2L,
    IF_EN2M,
    IF_EN2N,
    IF_EN2O,
    IF_EN2P,
    IF_EN2Q,
    IF_ENCOUNT
};

static uint8_t InsFormat(instruction ins)
{
    static_assert_no_msg(IF_ENCOUNT <= UINT8_MAX);

    const static uint8_t formats[]{
#define INST1(id, nm, info, fmt, ...) fmt,
#define INST2(id, nm, info, fmt, ...) fmt,
#define INST3(id, nm, info, fmt, ...) fmt,
#define INST4(id, nm, info, fmt, ...) fmt,
#define INST5(id, nm, info, fmt, ...) fmt,
#define INST6(id, nm, info, fmt, ...) fmt,
#define INST9(id, nm, info, fmt, ...) fmt,
#include "instrsarm64.h"
    };

    assert(ins < _countof(formats));
    assert(formats[ins] != IF_NONE);

    return formats[ins];
}

enum InsInfo
{
    ISI_LD  = 1,
    ISI_ST  = 2,
    ISI_CMP = 4,
    ISI_RSH = 8,
    ISI_WID = 16,
    ISI_LNG = 32,
    ISI_NRW = 64
};

static uint8_t GetInsInfo(instruction ins)
{
    static const uint8_t info[INS_COUNT + 1]{
#define LD ISI_LD
#define ST ISI_ST
#define CMP ISI_CMP
#define RSH ISI_RSH
#define WID ISI_WID
#define LNG ISI_LNG
#define NRW ISI_NRW
#define INST1(id, nm, info, ...) info,
#define INST2(id, nm, info, ...) info,
#define INST3(id, nm, info, ...) info,
#define INST4(id, nm, info, ...) info,
#define INST5(id, nm, info, ...) info,
#define INST6(id, nm, info, ...) info,
#define INST9(id, nm, info, ...) info,
#include "instrsarm64.h"
#undef LD
#undef ST
#undef CMP
#undef RSH
#undef WID
#undef LNG
#undef NRW
    };

    assert(ins < _countof(info));
    return info[ins];
}

bool IsLoadIns(instruction ins)
{
    return (GetInsInfo(ins) & ISI_LD) != 0;
}

bool IsMovIns(instruction ins)
{
    switch (ins)
    {
        case INS_fmov:
        case INS_mov:
        case INS_sxtb:
        case INS_sxth:
        case INS_sxtw:
        case INS_uxtb:
        case INS_uxth:
            return true;
        default:
            return false;
    }
}

static bool IsStoreIns(instruction ins)
{
    return (GetInsInfo(ins) & ISI_ST) != 0;
}

static bool IsVectorRightShiftIns(instruction ins)
{
    return (GetInsInfo(ins) & ISI_RSH) != 0;
}

#ifdef DEBUG
static bool IsLoadOrStoreIns(instruction ins)
{
    return (GetInsInfo(ins) & (ISI_LD | ISI_ST)) != 0;
}

static bool IsVectorLongIns(instruction ins)
{
    return (GetInsInfo(ins) & ISI_LNG) != 0;
}

static bool IsVectorNarrowIns(instruction ins)
{
    return (GetInsInfo(ins) & ISI_NRW) != 0;
}

static bool IsVectorWideIns(instruction ins)
{
    return (GetInsInfo(ins) & ISI_WID) != 0;
}
#endif

// Returns the specific encoding of the given CPU instruction and format
static uint32_t emitInsCode(instruction ins, insFormat fmt)
{
    // clang-format off
    const static uint32_t insCodes1[]
    {
        #define INST1(id, nm, info, fmt, e1                                ) e1,
        #define INST2(id, nm, info, fmt, e1, e2                            ) e1,
        #define INST3(id, nm, info, fmt, e1, e2, e3                        ) e1,
        #define INST4(id, nm, info, fmt, e1, e2, e3, e4                    ) e1,
        #define INST5(id, nm, info, fmt, e1, e2, e3, e4, e5                ) e1,
        #define INST6(id, nm, info, fmt, e1, e2, e3, e4, e5, e6            ) e1,
        #define INST9(id, nm, info, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e1,
        #include "instrsarm64.h"
    };
    const static uint32_t insCodes2[]
    {
        #define INST1(id, nm, info, fmt, e1                                )
        #define INST2(id, nm, info, fmt, e1, e2                            ) e2,
        #define INST3(id, nm, info, fmt, e1, e2, e3                        ) e2,
        #define INST4(id, nm, info, fmt, e1, e2, e3, e4                    ) e2,
        #define INST5(id, nm, info, fmt, e1, e2, e3, e4, e5                ) e2,
        #define INST6(id, nm, info, fmt, e1, e2, e3, e4, e5, e6            ) e2,
        #define INST9(id, nm, info, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e2,
        #include "instrsarm64.h"
    };
    const static uint32_t insCodes3[]
    {
        #define INST1(id, nm, info, fmt, e1                                )
        #define INST2(id, nm, info, fmt, e1, e2                            )
        #define INST3(id, nm, info, fmt, e1, e2, e3                        ) e3,
        #define INST4(id, nm, info, fmt, e1, e2, e3, e4                    ) e3,
        #define INST5(id, nm, info, fmt, e1, e2, e3, e4, e5                ) e3,
        #define INST6(id, nm, info, fmt, e1, e2, e3, e4, e5, e6            ) e3,
        #define INST9(id, nm, info, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e3,
        #include "instrsarm64.h"
    };
    const static uint32_t insCodes4[]
    {
        #define INST1(id, nm, info, fmt, e1                                )
        #define INST2(id, nm, info, fmt, e1, e2                            )
        #define INST3(id, nm, info, fmt, e1, e2, e3                        )
        #define INST4(id, nm, info, fmt, e1, e2, e3, e4                    ) e4,
        #define INST5(id, nm, info, fmt, e1, e2, e3, e4, e5                ) e4,
        #define INST6(id, nm, info, fmt, e1, e2, e3, e4, e5, e6            ) e4,
        #define INST9(id, nm, info, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e4,
        #include "instrsarm64.h"
    };
    const static uint32_t insCodes5[]
    {
        #define INST1(id, nm, info, fmt, e1                                )
        #define INST2(id, nm, info, fmt, e1, e2                            )
        #define INST3(id, nm, info, fmt, e1, e2, e3                        )
        #define INST4(id, nm, info, fmt, e1, e2, e3, e4                    )
        #define INST5(id, nm, info, fmt, e1, e2, e3, e4, e5                ) e5,
        #define INST6(id, nm, info, fmt, e1, e2, e3, e4, e5, e6            ) e5,
        #define INST9(id, nm, info, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e5,
        #include "instrsarm64.h"
    };
    const static uint32_t insCodes6[]
    {
        #define INST1(id, nm, info, fmt, e1                                )
        #define INST2(id, nm, info, fmt, e1, e2                            )
        #define INST3(id, nm, info, fmt, e1, e2, e3                        )
        #define INST4(id, nm, info, fmt, e1, e2, e3, e4                    )
        #define INST5(id, nm, info, fmt, e1, e2, e3, e4, e5                )
        #define INST6(id, nm, info, fmt, e1, e2, e3, e4, e5, e6            ) e6,
        #define INST9(id, nm, info, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e6,
        #include "instrsarm64.h"
    };
    const static uint32_t insCodes7[]
    {
        #define INST1(id, nm, info, fmt, e1                                )
        #define INST2(id, nm, info, fmt, e1, e2                            )
        #define INST3(id, nm, info, fmt, e1, e2, e3                        )
        #define INST4(id, nm, info, fmt, e1, e2, e3, e4                    )
        #define INST5(id, nm, info, fmt, e1, e2, e3, e4, e5                )
        #define INST6(id, nm, info, fmt, e1, e2, e3, e4, e5, e6            )
        #define INST9(id, nm, info, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e7,
        #include "instrsarm64.h"
    };
    const static uint32_t insCodes8[]
    {
        #define INST1(id, nm, info, fmt, e1                                )
        #define INST2(id, nm, info, fmt, e1, e2                            )
        #define INST3(id, nm, info, fmt, e1, e2, e3                        )
        #define INST4(id, nm, info, fmt, e1, e2, e3, e4                    )
        #define INST5(id, nm, info, fmt, e1, e2, e3, e4, e5                )
        #define INST6(id, nm, info, fmt, e1, e2, e3, e4, e5, e6            )
        #define INST9(id, nm, info, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e8,
        #include "instrsarm64.h"
    };
    const static uint32_t insCodes9[]
    {
        #define INST1(id, nm, info, fmt, e1                                )
        #define INST2(id, nm, info, fmt, e1, e2                            )
        #define INST3(id, nm, info, fmt, e1, e2, e3                        )
        #define INST4(id, nm, info, fmt, e1, e2, e3, e4                    )
        #define INST5(id, nm, info, fmt, e1, e2, e3, e4, e5                )
        #define INST6(id, nm, info, fmt, e1, e2, e3, e4, e5, e6            )
        #define INST9(id, nm, info, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e9,
        #include "instrsarm64.h"
    };
    // clang-format on

    const static insFormat formatEncode9[9]{IF_DR_2E, IF_DR_2G, IF_DI_1B, IF_DI_1D, IF_DV_3C,
                                            IF_DV_2B, IF_DV_2C, IF_DV_2E, IF_DV_2F};
    const static insFormat formatEncode6A[6]{IF_DR_3A, IF_DR_3B, IF_DR_3C, IF_DI_2A, IF_DV_3A, IF_DV_3E};
    const static insFormat formatEncode6B[6]{IF_LS_2D, IF_LS_3F, IF_LS_2E, IF_LS_2F, IF_LS_3G, IF_LS_2G};
    const static insFormat formatEncode5A[5]{IF_LS_2A, IF_LS_2B, IF_LS_2C, IF_LS_3A, IF_LS_1A};
    const static insFormat formatEncode5B[5]{IF_DV_2G, IF_DV_2H, IF_DV_2I, IF_DV_1A, IF_DV_1B};
    const static insFormat formatEncode5C[5]{IF_DR_3A, IF_DR_3B, IF_DI_2C, IF_DV_3C, IF_DV_1B};
    const static insFormat formatEncode4A[4]{IF_LS_2A, IF_LS_2B, IF_LS_2C, IF_LS_3A};
    const static insFormat formatEncode4B[4]{IF_DR_3A, IF_DR_3B, IF_DR_3C, IF_DI_2A};
    const static insFormat formatEncode4C[4]{IF_DR_2A, IF_DR_2B, IF_DR_2C, IF_DI_1A};
    const static insFormat formatEncode4D[4]{IF_DV_3B, IF_DV_3D, IF_DV_3BI, IF_DV_3DI};
    const static insFormat formatEncode4E[4]{IF_DR_3A, IF_DR_3B, IF_DI_2C, IF_DV_3C};
    const static insFormat formatEncode4F[4]{IF_DR_3A, IF_DR_3B, IF_DV_3C, IF_DV_1B};
    const static insFormat formatEncode4G[4]{IF_DR_2E, IF_DR_2F, IF_DV_2M, IF_DV_2L};
    const static insFormat formatEncode4H[4]{IF_DV_3E, IF_DV_3A, IF_DV_2L, IF_DV_2M};
    const static insFormat formatEncode4I[4]{IF_DV_3D, IF_DV_3B, IF_DV_2G, IF_DV_2A};
    const static insFormat formatEncode4J[4]{IF_DV_2N, IF_DV_2O, IF_DV_3E, IF_DV_3A};
    const static insFormat formatEncode4K[4]{IF_DV_3E, IF_DV_3A, IF_DV_3EI, IF_DV_3AI};
    const static insFormat formatEncode3A[3]{IF_DR_3A, IF_DR_3B, IF_DI_2C};
    const static insFormat formatEncode3B[3]{IF_DR_2A, IF_DR_2B, IF_DI_1C};
    const static insFormat formatEncode3C[3]{IF_DR_3A, IF_DR_3B, IF_DV_3C};
    const static insFormat formatEncode3D[3]{IF_DV_2C, IF_DV_2D, IF_DV_2E};
    const static insFormat formatEncode3E[3]{IF_DV_3B, IF_DV_3BI, IF_DV_3DI};
    const static insFormat formatEncode3F[3]{IF_DV_2A, IF_DV_2G, IF_DV_2H};
    const static insFormat formatEncode3G[3]{IF_DV_2A, IF_DV_2G, IF_DV_2I};
    const static insFormat formatEncode3H[3]{IF_DR_3A, IF_DV_3A, IF_DV_3AI};
    const static insFormat formatEncode3I[3]{IF_DR_2E, IF_DR_2F, IF_DV_2M};
    const static insFormat formatEncode3J[3]{IF_LS_2D, IF_LS_3F, IF_LS_2E};
    const static insFormat formatEncode2A[2]{IF_DR_2E, IF_DR_2F};
    const static insFormat formatEncode2B[2]{IF_DR_3A, IF_DR_3B};
    const static insFormat formatEncode2C[2]{IF_DR_3A, IF_DI_2D};
    const static insFormat formatEncode2D[2]{IF_DR_3A, IF_DI_2B};
    const static insFormat formatEncode2E[2]{IF_LS_3B, IF_LS_3C};
    const static insFormat formatEncode2F[2]{IF_DR_2I, IF_DI_1F};
    const static insFormat formatEncode2G[2]{IF_DV_3B, IF_DV_3D};
    const static insFormat formatEncode2H[2]{IF_DV_2C, IF_DV_2F};
    const static insFormat formatEncode2I[2]{IF_DV_2K, IF_DV_1C};
    const static insFormat formatEncode2J[2]{IF_DV_2A, IF_DV_2G};
    const static insFormat formatEncode2K[2]{IF_DV_2M, IF_DV_2L};
    const static insFormat formatEncode2L[2]{IF_DR_2G, IF_DV_2M};
    const static insFormat formatEncode2M[2]{IF_DV_3A, IF_DV_3AI};
    const static insFormat formatEncode2N[2]{IF_DV_2N, IF_DV_2O};
    const static insFormat formatEncode2O[2]{IF_DV_3E, IF_DV_3A};
    const static insFormat formatEncode2P[2]{IF_DV_2Q, IF_DV_3B};
    const static insFormat formatEncode2Q[2]{IF_DV_2S, IF_DV_3A};

    uint32_t code           = BAD_CODE;
    uint8_t  insFmt         = InsFormat(ins);
    bool     encoding_found = false;
    int      index          = -1;

    switch (insFmt)
    {
        case IF_EN9:
            for (index = 0; index < 9; index++)
            {
                if (fmt == formatEncode9[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN6A:
            for (index = 0; index < 6; index++)
            {
                if (fmt == formatEncode6A[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN6B:
            for (index = 0; index < 6; index++)
            {
                if (fmt == formatEncode6B[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN5A:
            for (index = 0; index < 5; index++)
            {
                if (fmt == formatEncode5A[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN5B:
            for (index = 0; index < 5; index++)
            {
                if (fmt == formatEncode5B[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN5C:
            for (index = 0; index < 5; index++)
            {
                if (fmt == formatEncode5C[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN4A:
            for (index = 0; index < 4; index++)
            {
                if (fmt == formatEncode4A[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN4B:
            for (index = 0; index < 4; index++)
            {
                if (fmt == formatEncode4B[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN4C:
            for (index = 0; index < 4; index++)
            {
                if (fmt == formatEncode4C[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN4D:
            for (index = 0; index < 4; index++)
            {
                if (fmt == formatEncode4D[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN4E:
            for (index = 0; index < 4; index++)
            {
                if (fmt == formatEncode4E[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN4F:
            for (index = 0; index < 4; index++)
            {
                if (fmt == formatEncode4F[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN4G:
            for (index = 0; index < 4; index++)
            {
                if (fmt == formatEncode4G[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN4H:
            for (index = 0; index < 4; index++)
            {
                if (fmt == formatEncode4H[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN4I:
            for (index = 0; index < 4; index++)
            {
                if (fmt == formatEncode4I[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN4J:
            for (index = 0; index < 4; index++)
            {
                if (fmt == formatEncode4J[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN4K:
            for (index = 0; index < 4; index++)
            {
                if (fmt == formatEncode4K[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN3A:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3A[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN3B:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3B[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN3C:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3C[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN3D:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3D[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN3E:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3E[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN3F:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3F[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN3G:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3G[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN3H:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3H[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN3I:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3I[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN3J:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3J[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2A:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2A[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2B:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2B[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2C:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2C[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2D:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2D[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2E:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2E[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2F:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2F[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2G:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2G[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2H:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2H[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2I:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2I[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2J:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2J[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2K:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2K[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2L:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2L[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2M:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2M[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2N:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2N[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2O:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2O[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2P:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2P[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        case IF_EN2Q:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2Q[index])
                {
                    encoding_found = true;
                    break;
                }
            }
            break;

        default:
            if (fmt == insFmt)
            {
                encoding_found = true;
                index          = 0;
            }
            else
            {
                encoding_found = false;
            }
            break;
    }

    assert(encoding_found);

    switch (index)
    {
        case 0:
            assert(ins < _countof(insCodes1));
            code = insCodes1[ins];
            break;
        case 1:
            assert(ins < _countof(insCodes2));
            code = insCodes2[ins];
            break;
        case 2:
            assert(ins < _countof(insCodes3));
            code = insCodes3[ins];
            break;
        case 3:
            assert(ins < _countof(insCodes4));
            code = insCodes4[ins];
            break;
        case 4:
            assert(ins < _countof(insCodes5));
            code = insCodes5[ins];
            break;
        case 5:
            assert(ins < _countof(insCodes6));
            code = insCodes6[ins];
            break;
        case 6:
            assert(ins < _countof(insCodes7));
            code = insCodes7[ins];
            break;
        case 7:
            assert(ins < _countof(insCodes8));
            code = insCodes8[ins];
            break;
        case 8:
            assert(ins < _countof(insCodes9));
            code = insCodes9[ins];
            break;
    }

    assert((code != BAD_CODE));

    return code;
}

// For the given 'ins' returns the reverse instruction if one exists, otherwise returns INS_INVALID
static instruction insReverse(instruction ins)
{
    switch (ins)
    {
        case INS_add:
            return INS_sub;
        case INS_adds:
            return INS_subs;

        case INS_sub:
            return INS_add;
        case INS_subs:
            return INS_adds;

        case INS_cmp:
            return INS_cmn;
        case INS_cmn:
            return INS_cmp;

        case INS_ccmp:
            return INS_ccmn;
        case INS_ccmn:
            return INS_ccmp;

        default:
            return INS_invalid;
    }
}

#ifdef DEBUG
// For a given instruction 'ins' which contains a register lists returns a
// number of consecutive SIMD registers the instruction loads to/store from.
static unsigned insGetRegisterListSize(instruction ins)
{
    switch (ins)
    {
        case INS_ld1:
        case INS_ld1r:
        case INS_st1:
        case INS_tbl:
        case INS_tbx:
            return 1;
        case INS_ld1_2regs:
        case INS_ld2:
        case INS_ld2r:
        case INS_st1_2regs:
        case INS_st2:
        case INS_tbl_2regs:
        case INS_tbx_2regs:
            return 2;
        case INS_ld1_3regs:
        case INS_ld3:
        case INS_ld3r:
        case INS_st1_3regs:
        case INS_st3:
        case INS_tbl_3regs:
        case INS_tbx_3regs:
            return 3;
        case INS_ld1_4regs:
        case INS_ld4:
        case INS_ld4r:
        case INS_st1_4regs:
        case INS_st4:
        case INS_tbl_4regs:
        case INS_tbx_4regs:
            return 4;
        default:
            unreached();
    }
}
#endif // DEBUG

// For the given 'datasize' and 'elemsize', make the proper arrangement option
// returns the insOpts that specifies the vector register arrangement
// if one does not exist returns INS_OPTS_NONE
static insOpts optMakeArrangement(emitAttr datasize, emitAttr elemsize)
{
    if (datasize == EA_8BYTE)
    {
        switch (elemsize)
        {
            case EA_1BYTE:
                return INS_OPTS_8B;
            case EA_2BYTE:
                return INS_OPTS_4H;
            case EA_4BYTE:
                return INS_OPTS_2S;
            case EA_8BYTE:
                return INS_OPTS_1D;
            default:
                unreached();
        }
    }

    if (datasize == EA_16BYTE)
    {
        switch (elemsize)
        {
            case EA_1BYTE:
                return INS_OPTS_16B;
            case EA_2BYTE:
                return INS_OPTS_8H;
            case EA_4BYTE:
                return INS_OPTS_4S;
            case EA_8BYTE:
                return INS_OPTS_2D;
            default:
                unreached();
        }
    }

    return INS_OPTS_NONE;
}

emitAttr GetVecElemsize(insOpts arrangement)
{
    switch (arrangement)
    {
        case INS_OPTS_8B:
        case INS_OPTS_16B:
            return EA_1BYTE;
        case INS_OPTS_4H:
        case INS_OPTS_8H:
            return EA_2BYTE;
        case INS_OPTS_2S:
        case INS_OPTS_4S:
            return EA_4BYTE;
        case INS_OPTS_1D:
        case INS_OPTS_2D:
            return EA_8BYTE;
        default:
            assert(!" invalid 'arrangement' value");
            return EA_UNKNOWN;
    }
}

static emitAttr optGetElemsize(insOpts arrangement)
{
    return GetVecElemsize(arrangement);
}

insOpts GetVecArrangementOpt(emitAttr vecSize, var_types elemType)
{
    assert((vecSize == EA_16BYTE) || (vecSize == EA_8BYTE));

    switch (elemType)
    {
        case TYP_DOUBLE:
        case TYP_ULONG:
        case TYP_LONG:
            return vecSize == EA_16BYTE ? INS_OPTS_2D : INS_OPTS_1D;
        case TYP_FLOAT:
        case TYP_UINT:
        case TYP_INT:
            return vecSize == EA_16BYTE ? INS_OPTS_4S : INS_OPTS_2S;
        case TYP_USHORT:
        case TYP_SHORT:
            return vecSize == EA_16BYTE ? INS_OPTS_8H : INS_OPTS_4H;
        case TYP_UBYTE:
        case TYP_BYTE:
            return vecSize == EA_16BYTE ? INS_OPTS_16B : INS_OPTS_8B;
        default:
            unreached();
    }
}

#ifdef DEBUG
// For the given 'datasize' and arrangement 'opts' returns true is the pair spcifies a valid arrangement
static bool isValidArrangement(emitAttr datasize, insOpts opt)
{
    if (datasize == EA_8BYTE)
    {
        return (opt == INS_OPTS_8B) || (opt == INS_OPTS_4H) || (opt == INS_OPTS_2S) || (opt == INS_OPTS_1D);
    }

    if (datasize == EA_16BYTE)
    {
        return (opt == INS_OPTS_16B) || (opt == INS_OPTS_8H) || (opt == INS_OPTS_4S) || (opt == INS_OPTS_2D);
    }

    return false;
}

// For the given 'arrangement' returns the one with the element width that is double that of the 'arrangement' element.
static insOpts optWidenElemsizeArrangement(insOpts arrangement)
{
    switch (arrangement)
    {
        case INS_OPTS_8B:
        case INS_OPTS_16B:
            return INS_OPTS_8H;
        case INS_OPTS_4H:
        case INS_OPTS_8H:
            return INS_OPTS_4S;
        case INS_OPTS_2S:
        case INS_OPTS_4S:
            return INS_OPTS_2D;
        default:
            assert(!" invalid 'arrangement' value");
            return INS_OPTS_NONE;
    }
}

// For the given 'datasize' returns the one that is double that of the 'datasize'.
static emitAttr widenDatasize(emitAttr datasize)
{
    switch (datasize)
    {
        case EA_1BYTE:
            return EA_2BYTE;
        case EA_2BYTE:
            return EA_4BYTE;
        case EA_4BYTE:
            return EA_8BYTE;
        default:
            assert(!" invalid 'datasize' value");
            return EA_UNKNOWN;
    }
}

// For the given 'srcArrangement' returns the "widen" 'dstArrangement' specifying
// the destination vector register arrangement.
// Asserts and returns INS_OPTS_NONE if an invalid 'srcArrangement' value is passed
static insOpts optWidenDstArrangement(insOpts srcArrangement)
{
    switch (srcArrangement)
    {
        case INS_OPTS_8B:
            return INS_OPTS_4H;
        case INS_OPTS_16B:
            return INS_OPTS_8H;
        case INS_OPTS_4H:
            return INS_OPTS_2S;
        case INS_OPTS_8H:
            return INS_OPTS_4S;
        case INS_OPTS_2S:
            return INS_OPTS_1D;
        case INS_OPTS_4S:
            return INS_OPTS_2D;
        default:
            assert(!" invalid 'srcArrangement' value");
            return INS_OPTS_NONE;
    }
}

// For the given 'conversion' returns the 'dstsize' specified by the conversion option
static emitAttr optGetDstsize(insOpts conversion)
{
    switch (conversion)
    {
        case INS_OPTS_S_TO_8BYTE:
        case INS_OPTS_D_TO_8BYTE:
        case INS_OPTS_4BYTE_TO_D:
        case INS_OPTS_8BYTE_TO_D:
        case INS_OPTS_S_TO_D:
        case INS_OPTS_H_TO_D:
            return EA_8BYTE;
        case INS_OPTS_S_TO_4BYTE:
        case INS_OPTS_D_TO_4BYTE:
        case INS_OPTS_4BYTE_TO_S:
        case INS_OPTS_8BYTE_TO_S:
        case INS_OPTS_D_TO_S:
        case INS_OPTS_H_TO_S:
            return EA_4BYTE;
        case INS_OPTS_S_TO_H:
        case INS_OPTS_D_TO_H:
            return EA_2BYTE;
        default:
            assert(!" invalid 'conversion' value");
            return EA_UNKNOWN;
    }
}

// For the given 'conversion' returns the 'srcsize' specified by the conversion option
static emitAttr optGetSrcsize(insOpts conversion)
{
    switch (conversion)
    {
        case INS_OPTS_D_TO_8BYTE:
        case INS_OPTS_D_TO_4BYTE:
        case INS_OPTS_8BYTE_TO_D:
        case INS_OPTS_8BYTE_TO_S:
        case INS_OPTS_D_TO_S:
        case INS_OPTS_D_TO_H:
            return EA_8BYTE;
        case INS_OPTS_S_TO_8BYTE:
        case INS_OPTS_S_TO_4BYTE:
        case INS_OPTS_4BYTE_TO_S:
        case INS_OPTS_4BYTE_TO_D:
        case INS_OPTS_S_TO_D:
        case INS_OPTS_S_TO_H:
            return EA_4BYTE;
        case INS_OPTS_H_TO_S:
        case INS_OPTS_H_TO_D:
            return EA_2BYTE;
        default:
            assert(!" invalid 'conversion' value");
            return EA_UNKNOWN;
    }
}

bool Arm64Imm::IsVecIndex(int64_t index, emitAttr vecSize, emitAttr elemSize)
{
    assert(isValidVectorDatasize(vecSize));
    assert(isValidVectorElemsize(elemSize));

    if (index < 0)
    {
        return false;
    }

    switch (elemSize)
    {
        case EA_1BYTE:
        case EA_2BYTE:
        case EA_4BYTE:
        case EA_8BYTE:
            return index < EA_SIZE_IN_BYTES(vecSize) / EA_SIZE_IN_BYTES(elemSize);
        default:
            unreached();
    }
}
#endif // DEBUG

template <typename T>
T* Arm64Emitter::AllocInstr(bool updateLastIns)
{
    instrDescSmall* id = emitAllocAnyInstr(sizeof(T), updateLastIns);
    memset(id, 0, sizeof(T));
    INDEBUG(id->idDebugOnlyInfo(new (emitComp, CMK_DebugOnly) instrDescDebugInfo(++emitInsCount, sizeof(T))));

    return static_cast<T*>(id);
}

instrDesc* Arm64Emitter::emitNewInstr()
{
    return AllocInstr<instrDesc>();
}

instrDesc* Arm64Emitter::emitNewInstrSmall()
{
    instrDescSmall* id = AllocInstr<instrDescSmall>();
    id->idSetIsSmallDsc();
    return static_cast<instrDesc*>(id);
}

instrDesc* Arm64Emitter::emitNewInstrSC(int64_t cns)
{
    if (!instrDesc::fitsInSmallCns(cns))
    {
        instrDescCns* id = AllocInstr<instrDescCns>();
        id->idSetIsLargeCns();
        id->idcCnsVal = cns;
        return id;
    }

    instrDesc* id = emitNewInstrSmall();
    id->idSmallCns(cns);
    return id;
}

instrDesc* Arm64Emitter::emitNewInstrCns(int32_t cns)
{
    if (!instrDesc::fitsInSmallCns(cns))
    {
        instrDescCns* id = AllocInstr<instrDescCns>();
        id->idSetIsLargeCns();
        id->idcCnsVal = cns;
        return id;
    }

    instrDesc* id = emitNewInstr();
    id->idSmallCns(cns);
    return id;
}

instrDescJmp* Arm64Emitter::emitNewInstrJmp()
{
    instrDescJmp* id = AllocInstr<instrDescJmp>();
    id->idjIG        = emitCurIG;
    id->idjOffs      = emitCurIGsize;
    id->idjNext      = emitCurIGjmpList;
    emitCurIGjmpList = id;
    return id;
}

instrDescCGCA* Arm64Emitter::emitAllocInstrCGCA()
{
    return AllocInstr<instrDescCGCA>();
}

instrDesc* Arm64Emitter::emitNewInstrGCReg(emitAttr attr, RegNum reg)
{
    assert(EA_IS_GCREF_OR_BYREF(attr));
    assert(IsGeneralRegister(reg));

    if ((codeGen->liveness.GetGCRegs(attr) & genRegMask(reg)) != RBM_NONE)
    {
        return nullptr;
    }

    instrDesc* id = static_cast<instrDesc*>(AllocInstr<instrDescSmall>(false));
    id->idSetIsSmallDsc();
    id->idIns(INS_mov);
    id->idInsFmt(IF_GC_REG);
    id->idOpSize(EA_8BYTE);
    id->idGCref(EA_GC_TYPE(attr));
    id->idReg1(reg);
    id->idReg2(reg);

    return id;
}

void Arm64Emitter::emitIns(instruction ins)
{
    insFormat fmt = static_cast<insFormat>(InsFormat(ins));

    assert((fmt == IF_SN_0A) || ((ins == INS_brk) && (fmt == IF_SI_0A)));

    instrDesc* id = emitNewInstrSmall();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idOpSize(EA_8BYTE);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_BRK(uint16_t imm)
{
    instrDesc* id = emitNewInstrSC(imm);
    id->idIns(INS_brk);
    id->idInsFmt(IF_SI_0A);
    id->idOpSize(EA_8BYTE);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R(instruction ins, emitAttr attr, RegNum reg)
{
    assert(attr == EA_8BYTE);

    insFormat fmt;

    if (ins == INS_dczva)
    {
        assert(isGeneralRegister(reg));

        fmt = IF_SR_1A;
    }
    else
    {
        assert((ins == INS_br) || (ins == INS_ret));
        assert(isGeneralRegister(reg));

        fmt = IF_BR_1A;
    }

    instrDesc* id = emitNewInstrSmall();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idOpSize(EA_8BYTE);
    id->idReg1(reg);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_I(instruction ins, emitAttr attr, RegNum reg, int64_t imm, insOpts opt)
{
    emitAttr  size = EA_SIZE(attr);
    insFormat fmt;
    MoviImm   mimm;

    switch (ins)
    {
        unsigned encodedImm;
        bool     canEncode;
        emitAttr elemsize;

        case INS_tst:
            assert(insOptsNone(opt));
            assert(isGeneralRegister(reg));
            canEncode = EncodeBitMaskImm(imm, size, &encodedImm);
            assert(canEncode);
            imm = encodedImm;
            assert(isValidImmNRS(imm, size));
            fmt = IF_DI_1C;
            break;

        case INS_movk:
        case INS_movn:
        case INS_movz:
            assert(isValidGeneralDatasize(size));
            assert(insOptsNone(opt)); // No LSL here (you must use emitIns_R_I_I if a shift is needed)
            assert(isGeneralRegister(reg));
            assert(isValidUimm16(imm));
            fmt = IF_DI_1B;
            break;

        case INS_mov:
            assert(isValidGeneralDatasize(size));
            assert(insOptsNone(opt)); // No explicit LSL here
            // We will automatically determine the shift based upon the imm

            if (EncodeHalfwordImm(imm, size, &encodedImm))
            {
                assert(isGeneralRegister(reg));
                imm = encodedImm;
                assert(isValidImmHWVal(imm, size));
                fmt = IF_DI_1B;
                break;
            }

            if (EncodeHalfwordImm(ImmNot(imm, getBitWidth(size)), size, &encodedImm))
            {
                assert(isGeneralRegister(reg));
                imm = encodedImm;
                ins = INS_movn;
                assert(isValidImmHWVal(imm, size));
                fmt = IF_DI_1B;
                break;
            }

            if (EncodeBitMaskImm(imm, size, &encodedImm))
            {
                assert(isGeneralRegisterOrSP(reg));
                reg = encodingSPtoZR(reg);
                imm = encodedImm;
                assert(isValidImmNRS(imm, size));
                fmt = IF_DI_1D;
                break;
            }
            unreached();

        case INS_movi:
            assert(isVectorRegister(reg));
            assert(isValidArrangement(size, opt));
            mimm = EncodeMoviImm(static_cast<uint64_t>(imm), opt);
            assert(mimm.ins != INS_invalid);
            ins = mimm.ins;
            imm = mimm.imm | (((mimm.msl ? 4 : 0) + (mimm.shift / 8)) << 8);
            fmt = IF_DV_1B;
            break;

        case INS_orr:
        case INS_bic:
            assert(isVectorRegister(reg));
            assert(isValidArrangement(size, opt));
            elemsize = optGetElemsize(opt);
            assert((elemsize == EA_2BYTE) || (elemsize == EA_4BYTE));
            canEncode = EncodeByteShiftedImm(imm, elemsize, &encodedImm);
            assert(canEncode);
            imm = encodedImm;
            assert(isValidImmBSVal(imm, size));
            fmt = IF_DV_1B;
            break;

        case INS_cmp:
        case INS_cmn:
            assert(insOptsNone(opt));
            assert(isGeneralRegister(reg));

            if (unsigned_abs(imm) <= 0x0fff)
            {
                if (imm < 0)
                {
                    ins = insReverse(ins);
                    imm = -imm;
                }

                assert(isValidUimm12(imm));
                fmt = IF_DI_1A;
                break;
            }

            noway_assert(IsShiftBy12Imm(imm));

            opt = INS_OPTS_LSL12;

            if (imm < 0)
            {
                ins = insReverse(ins);
                imm = -imm;
            }

            assert((imm & 0xfff) == 0);
            imm >>= 12;
            assert(isValidUimm12(imm));
            fmt = IF_DI_1A;
            break;

        default:
            unreached();
    }

    instrDesc* id = emitNewInstrSC(imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idInsOpt(opt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_F(instruction ins, emitAttr attr, RegNum reg, double immDbl, insOpts opt)
{
    assert(isVectorRegister(reg));

    insFormat fmt;
    unsigned  imm;

    switch (ins)
    {
        case INS_fcmp:
        case INS_fcmpe:
            assert(insOptsNone(opt));
            assert(isValidVectorElemsizeFloat(attr));
            assert(immDbl == 0.0);
            imm = 0;
            fmt = IF_DV_1C;
            break;

        case INS_fmov:
            imm = EncodeFMovImm(immDbl);

            if (insOptsAnyArrangement(opt))
            {
                assert(isValidVectorDatasize(attr));
                assert(isValidArrangement(attr, opt));
                assert(isValidVectorElemsizeFloat(optGetElemsize(opt)));
                assert(opt != INS_OPTS_1D); // Reserved encoding
                fmt = IF_DV_1B;
            }
            else
            {
                assert(insOptsNone(opt));
                assert(isValidVectorElemsizeFloat(attr));
                fmt = IF_DV_1A;
            }
            break;

        default:
            unreached();
    }

    instrDesc* id = emitNewInstrSC(imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idInsOpt(opt);
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_Mov(instruction ins, emitAttr attr, RegNum dstReg, RegNum srcReg, bool canSkip, insOpts opt)
{
    assert(IsMovIns(ins));

    emitAttr  size     = EA_SIZE(attr);
    emitAttr  elemsize = EA_UNKNOWN;
    insFormat fmt      = IF_NONE;

    switch (ins)
    {
        case INS_mov:
        {
            assert(insOptsNone(opt));

            if (EA_IS_GCREF_OR_BYREF(attr) && (dstReg == srcReg))
            {
                emitNewInstrGCReg(attr, dstReg);
                return;
            }

            if (IsRedundantMov(ins, size, dstReg, srcReg, canSkip))
            {
                // These instructions have no side effect and can be skipped
                return;
            }

            // Check for the 'mov' aliases for the vector registers
            if (isVectorRegister(dstReg))
            {
                if (isVectorRegister(srcReg) && isValidVectorDatasize(size))
                {
                    return emitIns_R_R_R(INS_mov, size, dstReg, srcReg, srcReg);
                }
                else
                {
                    return emitIns_R_R_I(INS_mov, size, dstReg, srcReg, 0);
                }
            }
            else
            {
                if (isVectorRegister(srcReg))
                {
                    assert(isGeneralRegister(dstReg));
                    return emitIns_R_R_I(INS_mov, size, dstReg, srcReg, 0);
                }
            }

            // Is this a MOV to/from SP instruction?
            if ((dstReg == REG_SP) || (srcReg == REG_SP))
            {
                assert(isGeneralRegisterOrSP(dstReg));
                assert(isGeneralRegisterOrSP(srcReg));
                dstReg = encodingSPtoZR(dstReg);
                srcReg = encodingSPtoZR(srcReg);
                fmt    = IF_DR_2G;
            }
            else
            {
                assert(insOptsNone(opt));
                assert(isGeneralRegister(dstReg));
                assert(isGeneralRegisterOrZR(srcReg));
                fmt = IF_DR_2E;
            }
            break;
        }

        case INS_sxtw:
            assert(size == EA_8BYTE);
            FALLTHROUGH;
        case INS_sxtb:
        case INS_sxth:
        case INS_uxtb:
        case INS_uxth:
            if (canSkip && (dstReg == srcReg))
            {
                // There are scenarios such as in genCallInstruction where the sign/zero extension should be elided
                return;
            }

            assert(insOptsNone(opt));
            assert(isValidGeneralDatasize(size));
            assert(isGeneralRegister(dstReg));
            assert(isGeneralRegister(srcReg));
            fmt = IF_DR_2H;
            break;

        case INS_fmov:
        {
            assert(isValidVectorElemsizeFloat(size));

            if (canSkip && (dstReg == srcReg))
            {
                // These instructions have no side effect and can be skipped
                return;
            }

            if (isVectorRegister(dstReg))
            {
                if (isVectorRegister(srcReg))
                {
                    assert(insOptsNone(opt));
                    fmt = IF_DV_2G;
                }
                else
                {
                    assert(isGeneralRegister(srcReg));

                    // if the optional conversion specifier is not present we calculate it
                    if (opt == INS_OPTS_NONE)
                    {
                        opt = (size == EA_4BYTE) ? INS_OPTS_4BYTE_TO_S : INS_OPTS_8BYTE_TO_D;
                    }
                    assert(insOptsConvertIntToFloat(opt));

                    fmt = IF_DV_2I;
                }
            }
            else
            {
                assert(isGeneralRegister(dstReg));
                assert(isVectorRegister(srcReg));

                // if the optional conversion specifier is not present we calculate it
                if (opt == INS_OPTS_NONE)
                {
                    opt = (size == EA_4BYTE) ? INS_OPTS_S_TO_4BYTE : INS_OPTS_D_TO_8BYTE;
                }
                assert(insOptsConvertFloatToInt(opt));

                fmt = IF_DV_2H;
            }
            break;
        }

        default:
            unreached();
    }

    instrDesc* id = emitNewInstrSmall();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idInsOpt(opt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(dstReg);
    id->idReg2(srcReg);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, insOpts opt)
{
    if (IsMovIns(ins))
    {
        assert(!"Please use emitIns_Mov() to correctly handle move elision");
        emitIns_Mov(ins, attr, reg1, reg2, /* canSkip */ false, opt);

        return;
    }

    emitAttr  size = EA_SIZE(attr);
    insFormat fmt;

    switch (ins)
    {
        emitAttr elemsize;

        case INS_dup:
            assert(insOptsAnyArrangement(opt));
            assert(isVectorRegister(reg1));
            assert(isGeneralRegisterOrZR(reg2));
            assert(isValidVectorDatasize(size));
            assert(isValidArrangement(size, opt));
            assert(opt != INS_OPTS_1D); // Reserved encoding
            fmt = IF_DV_2C;
            break;

        case INS_abs:
        case INS_not:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));

            if (ins == INS_not)
            {
                assert(isValidVectorDatasize(size));
                // Bitwise behavior is independent of element size, but is always encoded as 1 Byte
                opt = optMakeArrangement(size, EA_1BYTE);
            }

            if (insOptsNone(opt))
            {
                assert(size == EA_8BYTE); // Only type D is supported
                fmt = IF_DV_2L;
            }
            else
            {
                assert(insOptsAnyArrangement(opt));
                assert(isValidVectorDatasize(size));
                assert(isValidArrangement(size, opt));
                elemsize = optGetElemsize(opt);
                fmt      = IF_DV_2M;
            }
            break;

        case INS_mvn:
        case INS_neg:
            if (isVectorRegister(reg1))
            {
                assert(isVectorRegister(reg2));
                if (ins == INS_mvn)
                {
                    assert(isValidVectorDatasize(size));
                    // Bitwise behavior is independent of element size, but is always encoded as 1 Byte
                    opt = optMakeArrangement(size, EA_1BYTE);
                }
                if (insOptsNone(opt))
                {
                    assert(size == EA_8BYTE); // Only type D is supported
                    fmt = IF_DV_2L;
                }
                else
                {
                    assert(isValidVectorDatasize(size));
                    assert(isValidArrangement(size, opt));
                    elemsize = optGetElemsize(opt);
                    fmt      = IF_DV_2M;
                }
                break;
            }
            FALLTHROUGH;

        case INS_negs:
            assert(insOptsNone(opt));
            assert(isGeneralRegister(reg1));
            assert(isGeneralRegisterOrZR(reg2));
            fmt = IF_DR_2E;
            break;

        case INS_sxtl:
        case INS_sxtl2:
        case INS_uxtl:
        case INS_uxtl2:
            return emitIns_R_R_I(ins, size, reg1, reg2, 0, opt);

        case INS_cls:
        case INS_clz:
        case INS_rbit:
        case INS_rev16:
        case INS_rev32:
        case INS_cnt:
            if (isVectorRegister(reg1))
            {
                assert(isVectorRegister(reg2));
                assert(isValidVectorDatasize(size));
                assert(isValidArrangement(size, opt));
                elemsize = optGetElemsize(opt);
                if ((ins == INS_cls) || (ins == INS_clz))
                {
                    assert(elemsize != EA_8BYTE); // No encoding for type D
                }
                else if (ins == INS_rev32)
                {
                    assert((elemsize == EA_2BYTE) || (elemsize == EA_1BYTE));
                }
                else
                {
                    assert(elemsize == EA_1BYTE); // Only supports 8B or 16B
                }
                fmt = IF_DV_2M;
                break;
            }

            assert(ins != INS_cnt); // Doesn't have general register version(s)
            FALLTHROUGH;
        case INS_rev:
            assert(insOptsNone(opt));
            assert(isGeneralRegister(reg1));
            assert(isGeneralRegister(reg2));
            if (ins == INS_rev32)
            {
                assert(size == EA_8BYTE);
            }
            else
            {
                assert(isValidGeneralDatasize(size));
            }
            fmt = IF_DR_2G;
            break;

        case INS_addv:
        case INS_saddlv:
        case INS_smaxv:
        case INS_sminv:
        case INS_uaddlv:
        case INS_umaxv:
        case INS_uminv:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isValidVectorDatasize(size));
            assert(isValidArrangement(size, opt));
            assert((opt != INS_OPTS_2S) && (opt != INS_OPTS_1D) && (opt != INS_OPTS_2D)); // Reserved encodings
            fmt = IF_DV_2T;
            break;

        case INS_rev64:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isValidVectorDatasize(size));
            assert(isValidArrangement(size, opt));
            elemsize = optGetElemsize(opt);
            assert(elemsize != EA_8BYTE); // No encoding for type D
            fmt = IF_DV_2M;
            break;

        case INS_sqxtn:
        case INS_sqxtun:
        case INS_uqxtn:
            if (insOptsNone(opt))
            {
                assert(isVectorRegister(reg1));
                assert(isVectorRegister(reg2));
                assert(isValidVectorElemsize(size));
                assert(size != EA_8BYTE); // The encoding size = 11 is reserved.
                fmt = IF_DV_2L;
                break;
            }
            FALLTHROUGH;
        case INS_xtn:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(size == EA_8BYTE);
            assert(isValidArrangement(size, opt));
            assert(opt != INS_OPTS_1D); // The encoding size = 11, Q = x is reserved
            fmt = IF_DV_2M;
            break;

        case INS_sqxtn2:
        case INS_sqxtun2:
        case INS_uqxtn2:
        case INS_xtn2:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(size == EA_16BYTE);
            assert(isValidArrangement(size, opt));
            assert(opt != INS_OPTS_2D); // The encoding size = 11, Q = x is reserved
            fmt = IF_DV_2M;
            break;

        case INS_ldar:
        case INS_ldaxr:
        case INS_ldxr:
        case INS_stlr:
            assert(isValidGeneralDatasize(size));
            FALLTHROUGH;
        case INS_ldarb:
        case INS_ldaxrb:
        case INS_ldxrb:
        case INS_ldarh:
        case INS_ldaxrh:
        case INS_ldxrh:
        case INS_stlrb:
        case INS_stlrh:
            assert(isValidGeneralLSDatasize(size));
            assert(isGeneralRegisterOrZR(reg1));
            assert(isGeneralRegisterOrSP(reg2));
            assert(insOptsNone(opt));
            reg2 = encodingSPtoZR(reg2);
            fmt  = IF_LS_2A;
            break;

        case INS_ldr:
        case INS_ldrb:
        case INS_ldrh:
        case INS_ldrsb:
        case INS_ldrsh:
        case INS_ldrsw:
        case INS_str:
        case INS_strb:
        case INS_strh:
        case INS_cmp:
        case INS_cmn:
        case INS_tst:
            assert(insOptsNone(opt));
            emitIns_R_R_I(ins, attr, reg1, reg2, 0, INS_OPTS_NONE);
            return;

        case INS_staddb:
            emitIns_R_R_R(INS_ldaddb, attr, reg1, REG_ZR, reg2);
            return;
        case INS_staddlb:
            emitIns_R_R_R(INS_ldaddlb, attr, reg1, REG_ZR, reg2);
            return;
        case INS_staddh:
            emitIns_R_R_R(INS_ldaddh, attr, reg1, REG_ZR, reg2);
            return;
        case INS_staddlh:
            emitIns_R_R_R(INS_ldaddlh, attr, reg1, REG_ZR, reg2);
            return;
        case INS_stadd:
            emitIns_R_R_R(INS_ldadd, attr, reg1, REG_ZR, reg2);
            return;
        case INS_staddl:
            emitIns_R_R_R(INS_ldaddl, attr, reg1, REG_ZR, reg2);
            return;

        case INS_fcmp:
        case INS_fcmpe:
            assert(insOptsNone(opt));
            assert(isValidVectorElemsizeFloat(size));
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            fmt = IF_DV_2K;
            break;

        case INS_fcvtns:
        case INS_fcvtnu:
        case INS_fcvtas:
        case INS_fcvtau:
        case INS_fcvtps:
        case INS_fcvtpu:
        case INS_fcvtms:
        case INS_fcvtmu:
        case INS_fcvtzs:
        case INS_fcvtzu:
            if (insOptsAnyArrangement(opt))
            {
                assert(isVectorRegister(reg1));
                assert(isVectorRegister(reg2));
                assert(isValidVectorDatasize(size));
                assert(isValidArrangement(size, opt));
                elemsize = optGetElemsize(opt);
                assert(isValidVectorElemsizeFloat(elemsize));
                assert(opt != INS_OPTS_1D); // Reserved encoding
                fmt = IF_DV_2A;
            }
            else
            {
                assert(isVectorRegister(reg2));

                if (isVectorRegister(reg1))
                {
                    assert(insOptsNone(opt));
                    assert(isValidVectorElemsizeFloat(size));
                    fmt = IF_DV_2G;
                }
                else
                {
                    assert(isGeneralRegister(reg1));
                    assert(insOptsConvertFloatToInt(opt));
                    assert(isValidVectorElemsizeFloat(size));
                    fmt = IF_DV_2H;
                }
            }
            break;

        case INS_fcvtl:
        case INS_fcvtn:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(size == EA_8BYTE);
            assert((opt == INS_OPTS_4H) || (opt == INS_OPTS_2S));
            fmt = IF_DV_2A;
            break;

        case INS_fcvtl2:
        case INS_fcvtn2:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(size == EA_16BYTE);
            assert((opt == INS_OPTS_8H) || (opt == INS_OPTS_4S));
            fmt = IF_DV_2A;
            break;

        case INS_fcvtxn:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));

            if (insOptsAnyArrangement(opt))
            {
                assert(size == EA_8BYTE);
                assert(opt == INS_OPTS_2S);
                fmt = IF_DV_2A;
            }
            else
            {
                assert(insOptsNone(opt));
                assert(size == EA_4BYTE);
                fmt = IF_DV_2G;
            }
            break;

        case INS_fcvtxn2:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(size == EA_16BYTE);
            assert(opt == INS_OPTS_4S);
            fmt = IF_DV_2A;
            break;

        case INS_scvtf:
        case INS_ucvtf:
            if (insOptsAnyArrangement(opt))
            {
                assert(isVectorRegister(reg1));
                assert(isVectorRegister(reg2));
                assert(isValidVectorDatasize(size));
                assert(isValidArrangement(size, opt));
                elemsize = optGetElemsize(opt);
                assert(isValidVectorElemsizeFloat(elemsize));
                assert(opt != INS_OPTS_1D); // Reserved encoding
                fmt = IF_DV_2A;
            }
            else
            {
                assert(isVectorRegister(reg1));
                if (isVectorRegister(reg2))
                {
                    assert(insOptsNone(opt));
                    assert(isValidVectorElemsizeFloat(size));
                    fmt = IF_DV_2G;
                }
                else
                {
                    assert(isGeneralRegister(reg2));
                    assert(insOptsConvertIntToFloat(opt));
                    assert(isValidVectorElemsizeFloat(size));
                    fmt = IF_DV_2I;
                }
            }
            break;

        case INS_fabs:
        case INS_fneg:
        case INS_fsqrt:
        case INS_frinta:
        case INS_frinti:
        case INS_frintm:
        case INS_frintn:
        case INS_frintp:
        case INS_frintx:
        case INS_frintz:
            if (insOptsAnyArrangement(opt))
            {
                assert(isVectorRegister(reg1));
                assert(isVectorRegister(reg2));
                assert(isValidVectorDatasize(size));
                assert(isValidArrangement(size, opt));
                elemsize = optGetElemsize(opt);
                assert(isValidVectorElemsizeFloat(elemsize));
                assert(opt != INS_OPTS_1D); // Reserved encoding
                fmt = IF_DV_2A;
            }
            else
            {
                assert(insOptsNone(opt));
                assert(isValidVectorElemsizeFloat(size));
                assert(isVectorRegister(reg1));
                assert(isVectorRegister(reg2));
                fmt = IF_DV_2G;
            }
            break;

        case INS_faddp:
        case INS_fmaxnmp:
        case INS_fmaxp:
        case INS_fminnmp:
        case INS_fminp:
            assert(((size == EA_8BYTE) && (opt == INS_OPTS_2S)) || ((size == EA_16BYTE) && (opt == INS_OPTS_2D)));
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            fmt = IF_DV_2Q;
            break;

        case INS_fmaxnmv:
        case INS_fmaxv:
        case INS_fminnmv:
        case INS_fminv:
            assert(size == EA_16BYTE);
            assert(opt == INS_OPTS_4S);
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            fmt = IF_DV_2R;
            break;

        case INS_addp:
            assert(size == EA_16BYTE);
            assert(opt == INS_OPTS_2D);
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            fmt = IF_DV_2S;
            break;

        case INS_fcvt:
            assert(insOptsConvertFloatToFloat(opt));
            assert(isValidVectorFcvtsize(size));
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            fmt = IF_DV_2J;
            break;

        case INS_cmeq:
        case INS_cmge:
        case INS_cmgt:
        case INS_cmle:
        case INS_cmlt:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));

            if (isValidVectorDatasize(size))
            {
                assert(insOptsAnyArrangement(opt));
                assert(isValidArrangement(size, opt));
                elemsize = optGetElemsize(opt);
                fmt      = IF_DV_2M;
            }
            else
            {
                NYI("Untested");
                assert(size == EA_8BYTE); // Only Double supported
                fmt = IF_DV_2L;
            }
            break;

        case INS_fcmeq:
        case INS_fcmge:
        case INS_fcmgt:
        case INS_fcmle:
        case INS_fcmlt:
        case INS_frecpe:
        case INS_frsqrte:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));

            if (insOptsAnyArrangement(opt))
            {
                assert(isValidVectorDatasize(size));
                assert(isValidArrangement(size, opt));
                elemsize = optGetElemsize(opt);
                assert(isValidVectorElemsizeFloat(elemsize)); // Only Double/Float supported
                assert(opt != INS_OPTS_1D);                   // Reserved encoding
                fmt = IF_DV_2A;
            }
            else
            {
                assert(isValidScalarDatasize(size)); // Only Double/Float supported
                assert(insOptsNone(opt));
                fmt = IF_DV_2G;
            }
            break;

        case INS_aesd:
        case INS_aese:
        case INS_aesmc:
        case INS_aesimc:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isValidVectorDatasize(size));
            elemsize = optGetElemsize(opt);
            assert(elemsize == EA_1BYTE);
            fmt = IF_DV_2P;
            break;

        case INS_sha1h:
            assert(insOptsNone(opt));
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            fmt = IF_DV_2U;
            break;

        case INS_sha256su0:
        case INS_sha1su1:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isValidVectorDatasize(size));
            elemsize = optGetElemsize(opt);
            assert(elemsize == EA_4BYTE);
            fmt = IF_DV_2P;
            break;

        case INS_ld2:
        case INS_ld3:
        case INS_ld4:
        case INS_st2:
        case INS_st3:
        case INS_st4:
            assert(opt != INS_OPTS_1D); // .1D format only permitted with LD1 & ST1
            FALLTHROUGH;

        case INS_ld1:
        case INS_ld1_2regs:
        case INS_ld1_3regs:
        case INS_ld1_4regs:
        case INS_st1:
        case INS_st1_2regs:
        case INS_st1_3regs:
        case INS_st1_4regs:
        case INS_ld1r:
        case INS_ld2r:
        case INS_ld3r:
        case INS_ld4r:
            assert(isVectorRegister(reg1));
            assert(isGeneralRegisterOrSP(reg2));
            assert(isValidVectorDatasize(size));
            assert(isValidArrangement(size, opt));

            // Load/Store multiple structures       base register
            // Load single structure and replicate  base register
            reg2 = encodingSPtoZR(reg2);
            fmt  = IF_LS_2D;
            break;

        case INS_urecpe:
        case INS_ursqrte:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isValidVectorDatasize(size));
            assert(isValidArrangement(size, opt));
            elemsize = optGetElemsize(opt);
            assert(elemsize == EA_4BYTE);
            fmt = IF_DV_2A;
            break;

        case INS_frecpx:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isValidScalarDatasize(size));
            assert(insOptsNone(opt));
            fmt = IF_DV_2G;
            break;

        case INS_sadalp:
        case INS_saddlp:
        case INS_uadalp:
        case INS_uaddlp:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isValidArrangement(size, opt));
            assert((opt != INS_OPTS_1D) && (opt != INS_OPTS_2D)); // The encoding size = 11, Q = x is reserved
            fmt = IF_DV_2T;
            break;

        case INS_sqabs:
        case INS_sqneg:
        case INS_suqadd:
        case INS_usqadd:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));

            if (insOptsAnyArrangement(opt))
            {
                assert(isValidArrangement(size, opt));
                assert(opt != INS_OPTS_1D); // The encoding size = 11, Q = 0 is reserved
                fmt = IF_DV_2M;
            }
            else
            {
                assert(insOptsNone(opt));
                assert(isValidVectorElemsize(size));
                fmt = IF_DV_2L;
            }
            break;

        default:
            unreached();
    }

    instrDesc* id = emitNewInstrSmall();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idInsOpt(opt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_I_I(instruction ins, emitAttr attr, RegNum reg, int64_t imm1, int64_t imm2, insOpts opt)
{
    assert((ins == INS_mov) || (ins == INS_movk) || (ins == INS_movn) || (ins == INS_movz));
    assert(isValidGeneralDatasize(EA_SIZE(attr)));
    assert(isGeneralRegister(reg));
    assert(isValidUimm16(imm1));
    assert((imm2 == 0) || (imm2 == 16) || ((EA_SIZE(attr) == EA_8BYTE) && ((imm2 == 32) || (imm2 == 48))));
    assert(insOptsLSL(opt));

    if (ins == INS_mov)
    {
        ins = INS_movz; // INS_mov with LSL is an alias for INS_movz LSL
    }

    instrDesc* id = emitNewInstrSC(imm1 | ((imm2 >> 4) << 16));
    id->idIns(ins);
    id->idInsFmt(IF_DI_1B);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_R_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, int64_t imm, insOpts opt)
{
    emitAttr  size       = EA_SIZE(attr);
    emitAttr  elemsize   = EA_UNKNOWN;
    insFormat fmt        = IF_NONE;
    bool      isLdSt     = false;
    bool      isSIMD     = false;
    bool      isAddSub   = false;
    bool      setFlags   = false;
    unsigned  scale      = 0;
    bool      unscaledOp = false;

    switch (ins)
    {
        unsigned bmi;
        bool     canEncode;

        case INS_mov:
            // Check for the 'mov' aliases for the vector registers
            assert(insOptsNone(opt));
            assert(isValidVectorElemsize(size));
            elemsize = size;
            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));

            if (isVectorRegister(reg1))
            {
                if (isGeneralRegisterOrZR(reg2))
                {
                    fmt = IF_DV_2C; // Alias for 'ins'
                    break;
                }

                assert(isVectorRegister(reg2));
                fmt = IF_DV_2E; // Alias for 'dup'
                break;
            }

            assert(isGeneralRegister(reg1));
            assert(isVectorRegister(reg2));

            fmt = IF_DV_2B; // Alias for 'umov'
            break;

        case INS_lsl:
        case INS_lsr:
        case INS_asr:
            assert(insOptsNone(opt));
            assert(isValidGeneralDatasize(size));
            assert(isGeneralRegister(reg1));
            assert(isGeneralRegister(reg2));
            assert(isValidImmShift(imm, size));
            fmt = IF_DI_2D;
            break;

        case INS_ror:
            assert(insOptsNone(opt));
            assert(isValidGeneralDatasize(size));
            assert(isGeneralRegister(reg1));
            assert(isGeneralRegister(reg2));
            assert(isValidImmShift(imm, size));
            fmt = IF_DI_2B;
            break;

        case INS_shl:
        case INS_sli:
        case INS_sri:
        case INS_srshr:
        case INS_srsra:
        case INS_sshr:
        case INS_ssra:
        case INS_urshr:
        case INS_ursra:
        case INS_ushr:
        case INS_usra:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));

            if (insOptsAnyArrangement(opt))
            {
                assert(isValidVectorDatasize(size));
                assert(isValidArrangement(size, opt));
                elemsize = optGetElemsize(opt);
                assert(isValidVectorElemsize(elemsize));
                assert(isValidVectorShiftAmount(imm, elemsize, IsVectorRightShiftIns(ins)));
                assert(opt != INS_OPTS_1D); // Reserved encoding
                fmt = IF_DV_2O;
            }
            else
            {
                assert(insOptsNone(opt));
                assert(size == EA_8BYTE); // only supported size
                assert(isValidVectorShiftAmount(imm, size, IsVectorRightShiftIns(ins)));
                fmt = IF_DV_2N;
            }
            break;

        case INS_sqshl:
        case INS_uqshl:
        case INS_sqshlu:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));

            if (insOptsAnyArrangement(opt))
            {
                assert(isValidArrangement(size, opt));
                assert(opt != INS_OPTS_1D); // The encoding immh = 1xxx, Q = 0 is reserved
                elemsize = optGetElemsize(opt);
                assert(isValidVectorShiftAmount(imm, elemsize, IsVectorRightShiftIns(ins)));
                fmt = IF_DV_2O;
            }
            else
            {
                assert(insOptsNone(opt));
                assert(isValidVectorElemsize(size));
                assert(isValidVectorShiftAmount(imm, size, IsVectorRightShiftIns(ins)));
                fmt = IF_DV_2N;
            }
            break;

        case INS_sqrshrn:
        case INS_sqrshrun:
        case INS_sqshrn:
        case INS_sqshrun:
        case INS_uqrshrn:
        case INS_uqshrn:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));

            if (insOptsAnyArrangement(opt))
            {
                assert(isValidArrangement(size, opt));
                assert((opt != INS_OPTS_1D) && (opt != INS_OPTS_2D)); // The encoding immh = 1xxx, Q = x is reserved
                elemsize = optGetElemsize(opt);
                assert(isValidVectorShiftAmount(imm, elemsize, IsVectorRightShiftIns(ins)));
                fmt = IF_DV_2O;
            }
            else
            {
                assert(insOptsNone(opt));
                assert(isValidVectorElemsize(size));
                assert(size != EA_8BYTE); // The encoding immh = 1xxx is reserved
                assert(isValidVectorShiftAmount(imm, size, IsVectorRightShiftIns(ins)));
                fmt = IF_DV_2N;
            }
            break;

        case INS_sxtl:
        case INS_uxtl:
            assert(imm == 0);
            FALLTHROUGH;

        case INS_rshrn:
        case INS_shrn:
        case INS_sshll:
        case INS_ushll:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(size == EA_8BYTE);
            assert(isValidArrangement(size, opt));
            elemsize = optGetElemsize(opt);
            assert(elemsize != EA_8BYTE); // Reserved encodings
            assert(isValidVectorElemsize(elemsize));
            assert(isValidVectorShiftAmount(imm, elemsize, IsVectorRightShiftIns(ins)));
            fmt = IF_DV_2O;
            break;

        case INS_sxtl2:
        case INS_uxtl2:
            assert(imm == 0);
            FALLTHROUGH;

        case INS_rshrn2:
        case INS_shrn2:
        case INS_sqrshrn2:
        case INS_sqrshrun2:
        case INS_sqshrn2:
        case INS_sqshrun2:
        case INS_sshll2:
        case INS_uqrshrn2:
        case INS_uqshrn2:
        case INS_ushll2:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(size == EA_16BYTE);
            assert(isValidArrangement(size, opt));
            elemsize = optGetElemsize(opt);
            assert(elemsize != EA_8BYTE); // The encoding immh = 1xxx, Q = x is reserved
            assert(isValidVectorElemsize(elemsize));
            assert(isValidVectorShiftAmount(imm, elemsize, IsVectorRightShiftIns(ins)));
            fmt = IF_DV_2O;
            break;

        case INS_mvn:
        case INS_neg:
        case INS_negs:
            assert(isValidGeneralDatasize(size));
            assert(isGeneralRegister(reg1));
            assert(isGeneralRegisterOrZR(reg2));

            if (imm == 0)
            {
                assert(insOptsNone(opt)); // a zero imm, means no alu shift kind

                fmt = IF_DR_2E;
            }
            else
            {
                if (ins == INS_mvn)
                {
                    assert(insOptsAnyShift(opt)); // a non-zero imm, must select shift kind
                }
                else // neg or negs
                {
                    assert(insOptsAluShift(opt)); // a non-zero imm, must select shift kind, can't use ROR
                }

                assert(isValidImmShift(imm, size));
                fmt = IF_DR_2F;
            }
            break;

        case INS_tst:
            assert(isValidGeneralDatasize(size));
            assert(isGeneralRegisterOrZR(reg1));
            assert(isGeneralRegister(reg2));

            if (insOptsAnyShift(opt))
            {
                assert(isValidImmShift(imm, size) && (imm != 0));
                fmt = IF_DR_2B;
            }
            else
            {
                assert(insOptsNone(opt)); // a zero imm, means no alu shift kind
                assert(imm == 0);
                fmt = IF_DR_2A;
            }
            break;

        case INS_cmp:
        case INS_cmn:
            assert(isValidGeneralDatasize(size));
            assert(isGeneralRegisterOrSP(reg1));
            assert(isGeneralRegister(reg2));

            reg1 = encodingSPtoZR(reg1);
            if (insOptsAnyExtend(opt))
            {
                assert((imm >= 0) && (imm <= 4));

                fmt = IF_DR_2C;
            }
            else if (imm == 0)
            {
                assert(insOptsNone(opt)); // a zero imm, means no alu shift kind

                fmt = IF_DR_2A;
            }
            else
            {
                assert(insOptsAnyShift(opt)); // a non-zero imm, must select shift kind
                assert(isValidImmShift(imm, size));
                fmt = IF_DR_2B;
            }
            break;

        case INS_ands:
        case INS_and:
        case INS_eor:
        case INS_orr:
            assert(insOptsNone(opt));
            assert(isGeneralRegister(reg2));
            if (ins == INS_ands)
            {
                assert(isGeneralRegister(reg1));
            }
            else
            {
                assert(isGeneralRegisterOrSP(reg1));
                reg1 = encodingSPtoZR(reg1);
            }

            canEncode = EncodeBitMaskImm(imm, size, &bmi);
            assert(canEncode);
            imm = bmi;
            assert(isValidImmNRS(imm, size));
            fmt = IF_DI_2C;
            break;

        case INS_dup: // by element, imm selects the element of reg2
            assert(isVectorRegister(reg1));

            if (isVectorRegister(reg2))
            {
                if (insOptsAnyArrangement(opt))
                {
                    // The size and opt were modified to be based on the
                    // return type but the immediate is based on the operand
                    // which can be of a larger size. As such, we don't
                    // assert the index is valid here and instead do it in
                    // codegen.

                    assert(isValidVectorDatasize(size));
                    assert(isValidArrangement(size, opt));
                    elemsize = optGetElemsize(opt);
                    assert(isValidVectorElemsize(elemsize));
                    assert(opt != INS_OPTS_1D); // Reserved encoding
                    fmt = IF_DV_2D;
                }
                else
                {
                    assert(insOptsNone(opt));
                    elemsize = size;
                    assert(isValidVectorElemsize(elemsize));
                    assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
                    fmt = IF_DV_2E;
                }

                break;
            }
            FALLTHROUGH;

        case INS_ins: // (MOV from general)
            assert(insOptsNone(opt));
            assert(isValidVectorElemsize(size));
            assert(isVectorRegister(reg1));
            assert(isGeneralRegisterOrZR(reg2));
            elemsize = size;
            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
            fmt = IF_DV_2C;
            break;

        case INS_umov: // (MOV to general)
            assert(insOptsNone(opt));
            assert(isValidVectorElemsize(size));
            assert(isGeneralRegister(reg1));
            assert(isVectorRegister(reg2));
            elemsize = size;
            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
            fmt = IF_DV_2B;
            break;

        case INS_smov:
            assert(insOptsNone(opt));
            assert(isValidVectorElemsize(size));
            assert(size != EA_8BYTE); // no encoding, use INS_umov
            assert(isGeneralRegister(reg1));
            assert(isVectorRegister(reg2));
            elemsize = size;
            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
            fmt = IF_DV_2B;
            break;

        case INS_add:
        case INS_sub:
            setFlags = false;
            isAddSub = true;
            break;

        case INS_adds:
        case INS_subs:
            setFlags = true;
            isAddSub = true;
            break;

        case INS_ldrsb:
        case INS_ldursb:
            // 'size' specifies how we sign-extend into 4 or 8 bytes of the target register
            assert(isValidGeneralDatasize(size));
            unscaledOp = (ins == INS_ldursb);
            scale      = 0;
            isLdSt     = true;
            break;

        case INS_ldrsh:
        case INS_ldursh:
            // 'size' specifies how we sign-extend into 4 or 8 bytes of the target register
            assert(isValidGeneralDatasize(size));
            unscaledOp = (ins == INS_ldursh);
            scale      = 1;
            isLdSt     = true;
            break;

        case INS_ldrsw:
        case INS_ldursw:
            // 'size' specifies how we sign-extend into 4 or 8 bytes of the target register
            assert(size == EA_8BYTE);
            unscaledOp = (ins == INS_ldursw);
            scale      = 2;
            isLdSt     = true;
            break;

        case INS_ldrb:
        case INS_strb:
            // size is ignored
            unscaledOp = false;
            scale      = 0;
            isLdSt     = true;
            break;

        case INS_ldurb:
        case INS_sturb:
            // size is ignored
            unscaledOp = true;
            scale      = 0;
            isLdSt     = true;
            break;

        case INS_ldrh:
        case INS_strh:
            // size is ignored
            unscaledOp = false;
            scale      = 1;
            isLdSt     = true;
            break;

        case INS_ldurh:
        case INS_sturh:
            // size is ignored
            unscaledOp = true;
            scale      = 0;
            isLdSt     = true;
            break;

        case INS_ldr:
        case INS_str:
            if (isVectorRegister(reg1))
            {
                assert(isValidVectorLSDatasize(size));
                assert(isGeneralRegisterOrSP(reg2));
                isSIMD = true;
            }
            else
            {
                assert(isValidGeneralDatasize(size));
            }
            unscaledOp = false;
            scale      = NaturalScale(size);
            isLdSt     = true;
            break;

        case INS_ldur:
        case INS_stur:
            if (isVectorRegister(reg1))
            {
                assert(isValidVectorLSDatasize(size));
                assert(isGeneralRegisterOrSP(reg2));
                isSIMD = true;
            }
            else
            {
                assert(isValidGeneralDatasize(size));
            }
            unscaledOp = true;
            scale      = 0;
            isLdSt     = true;
            break;

        case INS_ld2:
        case INS_ld3:
        case INS_ld4:
        case INS_st2:
        case INS_st3:
        case INS_st4:
            assert(opt != INS_OPTS_1D); // .1D format only permitted with LD1 & ST1
            FALLTHROUGH;
        case INS_ld1:
        case INS_ld1_2regs:
        case INS_ld1_3regs:
        case INS_ld1_4regs:
        case INS_st1:
        case INS_st1_2regs:
        case INS_st1_3regs:
        case INS_st1_4regs:
            assert(isVectorRegister(reg1));
            assert(isGeneralRegisterOrSP(reg2));

            reg2 = encodingSPtoZR(reg2);

            if (insOptsAnyArrangement(opt))
            {
                assert(isValidVectorDatasize(size));
                assert(isValidArrangement(size, opt));
                assert(size * insGetRegisterListSize(ins) == imm);
                fmt = IF_LS_2E;
            }
            else
            {
                assert(insOptsNone(opt));
                assert((ins != INS_ld1_2regs) && (ins != INS_ld1_3regs) && (ins != INS_ld1_4regs) &&
                       (ins != INS_st1_2regs) && (ins != INS_st1_3regs) && (ins != INS_st1_4regs));

                elemsize = size;
                assert(isValidVectorElemsize(elemsize));
                assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
                fmt = IF_LS_2F;
            }
            break;

        case INS_ld1r:
        case INS_ld2r:
        case INS_ld3r:
        case INS_ld4r:
            assert(isVectorRegister(reg1));
            assert(isGeneralRegisterOrSP(reg2));
            assert(isValidVectorDatasize(size));
            assert(isValidArrangement(size, opt));
            elemsize = optGetElemsize(opt);
            assert(elemsize * insGetRegisterListSize(ins) == imm);
            reg2 = encodingSPtoZR(reg2);
            fmt  = IF_LS_2E;
            break;

        default:
            unreached();
    }

    if (isLdSt)
    {
        assert(!isAddSub);

        if (isSIMD)
        {
            assert(isValidVectorLSDatasize(size));
            assert(isVectorRegister(reg1));
            assert((scale >= 0) && (scale <= 4));
        }
        else
        {
            assert(isValidGeneralLSDatasize(size));
            assert(isGeneralRegisterOrZR(reg1));
            assert((scale >= 0) && (scale <= 3));
        }

        assert(isGeneralRegisterOrSP(reg2));

        // Load/Store reserved encodings:
        if (insOptsIndexed(opt))
        {
            assert(reg1 != reg2);
        }

        reg2 = encodingSPtoZR(reg2);

        int64_t mask = (1 << scale) - 1; // the mask of low bits that must be zero to encode the immediate
        if (imm == 0)
        {
            assert(insOptsNone(opt)); // PRE/POST Index doesn't make sense with an immediate of zero

            fmt = IF_LS_2A;
        }
        else if (insOptsIndexed(opt) || unscaledOp || (imm < 0) || ((imm & mask) != 0))
        {
            if ((imm >= -256) && (imm <= 255))
            {
                fmt = IF_LS_2C;
            }
            else
            {
                assert(!"Instruction cannot be encoded: IF_LS_2C");
            }
        }
        else if (imm > 0)
        {
            assert(insOptsNone(opt));
            assert(!unscaledOp);

            if (((imm & mask) == 0) && ((imm >> scale) < 0x1000))
            {
                imm >>= scale; // The immediate is scaled by the size of the ld/st

                fmt = IF_LS_2B;
            }
            else
            {
                assert(!"Instruction cannot be encoded: IF_LS_2B");
            }
        }

        // Is the ldr/str even necessary?
        // For volatile load/store, there will be memory barrier instruction before/after the load/store
        // and in such case, IsRedundantLdStr() returns false, because the method just checks for load/store
        // pair next to each other.
        if (emitComp->opts.OptimizationEnabled() && IsRedundantLdStr(ins, reg1, reg2, imm, size, fmt))
        {
            return;
        }
    }
    else if (isAddSub)
    {
        assert(!isLdSt);
        assert(insOptsNone(opt));

        if (setFlags) // Can't encode SP with setFlags
        {
            assert(isGeneralRegister(reg1));
            assert(isGeneralRegister(reg2));
        }
        else
        {
            assert(isGeneralRegisterOrSP(reg1));
            assert(isGeneralRegisterOrSP(reg2));

            // Is it just a mov?
            if (imm == 0)
            {
                emitIns_Mov(INS_mov, attr, reg1, reg2, /* canSkip */ true);
                return;
            }

            reg1 = encodingSPtoZR(reg1);
            reg2 = encodingSPtoZR(reg2);
        }

        if (unsigned_abs(imm) <= 0x0fff)
        {
            if (imm < 0)
            {
                ins = insReverse(ins);
                imm = -imm;
            }

            assert(isValidUimm12(imm));
            fmt = IF_DI_2A;
        }
        else
        {
            assert(IsShiftBy12Imm(imm));
            opt = INS_OPTS_LSL12;

            if (imm < 0)
            {
                ins = insReverse(ins);
                imm = -imm;
            }

            assert((imm & 0xfff) == 0);
            imm >>= 12;
            assert(isValidUimm12(imm));
            fmt = IF_DI_2A;
        }
    }

    instrDesc* id = emitNewInstrSC(imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idInsOpt(opt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_R_Imm(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, int64_t imm)
{
    assert(isGeneralRegister(reg1));
    assert(reg1 != reg2);

    bool immFits = true;

    switch (ins)
    {
        case INS_add:
        case INS_adds:
        case INS_sub:
        case INS_subs:
            immFits = Arm64Imm::IsAddImm(imm, attr);
            break;
        case INS_ands:
        case INS_and:
        case INS_eor:
        case INS_orr:
            immFits = Arm64Imm::IsAluImm(imm, attr);
            break;
        default:
            unreached();
    }

    if (immFits)
    {
        emitIns_R_R_I(ins, attr, reg1, reg2, imm);
    }
    else
    {
        codeGen->instGen_Set_Reg_To_Imm(attr, reg1, imm);
        emitIns_R_R_R(ins, attr, reg1, reg2, reg1);
    }
}

void Arm64Emitter::emitIns_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, insOpts opt)
{
    emitAttr  size = EA_SIZE(attr);
    insFormat fmt  = IF_NONE;

    switch (ins)
    {
        emitAttr elemsize;

        case INS_mul:
        case INS_smull:
        case INS_umull:
            if (insOptsAnyArrangement(opt))
            {
                assert(isVectorRegister(reg1));
                assert(isVectorRegister(reg2));
                assert(isVectorRegister(reg3));
                assert(isValidArrangement(size, opt));
                assert((opt != INS_OPTS_1D) && (opt != INS_OPTS_2D)); // The encoding size = 11, Q = x is reserved
                fmt = IF_DV_3A;
                break;
            }
            FALLTHROUGH;
        case INS_lsl:
        case INS_lsr:
        case INS_asr:
        case INS_ror:
        case INS_adc:
        case INS_adcs:
        case INS_sbc:
        case INS_sbcs:
        case INS_udiv:
        case INS_sdiv:
        case INS_mneg:
        case INS_smnegl:
        case INS_smulh:
        case INS_umnegl:
        case INS_umulh:
        case INS_lslv:
        case INS_lsrv:
        case INS_asrv:
        case INS_rorv:
        case INS_crc32b:
        case INS_crc32h:
        case INS_crc32w:
        case INS_crc32x:
        case INS_crc32cb:
        case INS_crc32ch:
        case INS_crc32cw:
        case INS_crc32cx:
            assert(insOptsNone(opt));
            assert(isValidGeneralDatasize(size));
            assert(isGeneralRegister(reg1));
            assert(isGeneralRegister(reg2));
            assert(isGeneralRegister(reg3));
            fmt = IF_DR_3A;
            break;

        case INS_add:
        case INS_sub:
            if (isVectorRegister(reg1))
            {
                assert(isVectorRegister(reg2));
                assert(isVectorRegister(reg3));

                if (insOptsAnyArrangement(opt))
                {
                    assert(opt != INS_OPTS_1D); // Reserved encoding
                    assert(isValidVectorDatasize(size));
                    assert(isValidArrangement(size, opt));
                    fmt = IF_DV_3A;
                }
                else
                {
                    assert(insOptsNone(opt));
                    assert(size == EA_8BYTE);
                    fmt = IF_DV_3E;
                }
                break;
            }
            FALLTHROUGH;
        case INS_adds:
        case INS_subs:
            emitIns_R_R_R_I(ins, attr, reg1, reg2, reg3, 0, INS_OPTS_NONE);
            return;

        case INS_cmeq:
        case INS_cmge:
        case INS_cmgt:
        case INS_cmhi:
        case INS_cmhs:
        case INS_cmtst:
        case INS_srshl:
        case INS_sshl:
        case INS_urshl:
        case INS_ushl:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));

            if (insOptsAnyArrangement(opt))
            {
                assert(isValidArrangement(size, opt));
                assert(opt != INS_OPTS_1D); // The encoding size = 11, Q = 0 is reserved
                fmt = IF_DV_3A;
            }
            else
            {
                assert(insOptsNone(opt));
                assert(size == EA_8BYTE); // Only Int64/UInt64 supported
                fmt = IF_DV_3E;
            }
            break;

        case INS_sqadd:
        case INS_sqrshl:
        case INS_sqshl:
        case INS_sqsub:
        case INS_uqadd:
        case INS_uqrshl:
        case INS_uqshl:
        case INS_uqsub:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));

            if (insOptsAnyArrangement(opt))
            {
                assert(isValidArrangement(size, opt));
                assert(opt != INS_OPTS_1D); // The encoding size = 11, Q = 0 is reserved
                fmt = IF_DV_3A;
            }
            else
            {
                assert(insOptsNone(opt));
                assert(isValidVectorElemsize(size));
                fmt = IF_DV_3E;
            }
            break;

        case INS_fcmeq:
        case INS_fcmge:
        case INS_fcmgt:
        case INS_frecps:
        case INS_frsqrts:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));

            if (insOptsAnyArrangement(opt))
            {
                assert(isValidVectorDatasize(size));
                assert(isValidArrangement(size, opt));
                elemsize = optGetElemsize(opt);
                assert((elemsize == EA_8BYTE) || (elemsize == EA_4BYTE)); // Only Double/Float supported
                assert(opt != INS_OPTS_1D);                               // Reserved encoding
                fmt = IF_DV_3B;
            }
            else
            {
                assert(insOptsNone(opt));
                assert((size == EA_8BYTE) || (size == EA_4BYTE)); // Only Double/Float supported
                fmt = IF_DV_3D;
            }
            break;

        case INS_mla:
        case INS_mls:
        case INS_saba:
        case INS_sabd:
        case INS_shadd:
        case INS_shsub:
        case INS_smax:
        case INS_smaxp:
        case INS_smin:
        case INS_sminp:
        case INS_srhadd:
        case INS_uaba:
        case INS_uabd:
        case INS_uhadd:
        case INS_uhsub:
        case INS_umax:
        case INS_umaxp:
        case INS_umin:
        case INS_uminp:
        case INS_urhadd:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(isValidArrangement(size, opt));
            assert((opt != INS_OPTS_1D) && (opt != INS_OPTS_2D)); // The encoding size = 11, Q = x is reserved
            fmt = IF_DV_3A;
            break;

        case INS_addp:
        case INS_uzp1:
        case INS_uzp2:
        case INS_zip1:
        case INS_zip2:
        case INS_trn1:
        case INS_trn2:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(isValidArrangement(size, opt));
            assert(opt != INS_OPTS_1D); // The encoding size = 11, Q = 0 is reserved
            fmt = IF_DV_3A;
            break;

        case INS_mov:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(reg2 == reg3);
            assert(isValidVectorDatasize(size));
            // INS_mov is an alias for INS_orr (vector register)
            if (opt == INS_OPTS_NONE)
            {
                elemsize = EA_1BYTE;
                opt      = optMakeArrangement(size, elemsize);
            }
            assert(isValidArrangement(size, opt));
            fmt = IF_DV_3C;
            break;

        case INS_and:
        case INS_bic:
        case INS_eor:
        case INS_orr:
        case INS_orn:
        case INS_tbl:
        case INS_tbl_2regs:
        case INS_tbl_3regs:
        case INS_tbl_4regs:
        case INS_tbx:
        case INS_tbx_2regs:
        case INS_tbx_3regs:
        case INS_tbx_4regs:
            if (isVectorRegister(reg1))
            {
                assert(isValidVectorDatasize(size));
                assert(isVectorRegister(reg2));
                assert(isVectorRegister(reg3));
                if (opt == INS_OPTS_NONE)
                {
                    elemsize = EA_1BYTE;
                    opt      = optMakeArrangement(size, elemsize);
                }
                assert(isValidArrangement(size, opt));
                fmt = IF_DV_3C;
                break;
            }
            FALLTHROUGH;

        case INS_ands:
        case INS_bics:
        case INS_eon:
            emitIns_R_R_R_I(ins, attr, reg1, reg2, reg3, 0, INS_OPTS_NONE);
            return;

        case INS_bsl:
        case INS_bit:
        case INS_bif:
            assert(isValidVectorDatasize(size));
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            if (opt == INS_OPTS_NONE)
            {
                elemsize = EA_1BYTE;
                opt      = optMakeArrangement(size, elemsize);
            }
            assert(isValidArrangement(size, opt));
            fmt = IF_DV_3C;
            break;

        case INS_fadd:
        case INS_fsub:
        case INS_fdiv:
        case INS_fmax:
        case INS_fmaxnm:
        case INS_fmin:
        case INS_fminnm:
        case INS_fabd:
        case INS_fmul:
        case INS_fmulx:
        case INS_facge:
        case INS_facgt:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            if (insOptsAnyArrangement(opt))
            {
                assert(isValidVectorDatasize(size));
                assert(isValidArrangement(size, opt));
                elemsize = optGetElemsize(opt);
                assert(isValidVectorElemsizeFloat(elemsize));
                assert(opt != INS_OPTS_1D); // Reserved encoding
                fmt = IF_DV_3B;
            }
            else
            {
                assert(insOptsNone(opt));
                assert(isValidScalarDatasize(size));
                fmt = IF_DV_3D;
            }
            break;

        case INS_fnmul:
            assert(insOptsNone(opt));
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(isValidScalarDatasize(size));
            fmt = IF_DV_3D;
            break;

        case INS_faddp:
        case INS_fmaxnmp:
        case INS_fmaxp:
        case INS_fminnmp:
        case INS_fminp:

        case INS_fmla:
        case INS_fmls:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(insOptsAnyArrangement(opt)); // no scalar encoding, use 4-operand 'fmadd' or 'fmsub'
            assert(isValidVectorDatasize(size));
            assert(isValidArrangement(size, opt));
            elemsize = optGetElemsize(opt);
            assert(isValidVectorElemsizeFloat(elemsize));
            assert(opt != INS_OPTS_1D); // Reserved encoding
            fmt = IF_DV_3B;
            break;

        case INS_ldr:
        case INS_ldrb:
        case INS_ldrh:
        case INS_ldrsb:
        case INS_ldrsh:
        case INS_ldrsw:
        case INS_str:
        case INS_strb:
        case INS_strh:
            emitIns_R_R_R_Ext(ins, attr, reg1, reg2, reg3, opt);
            return;

        case INS_ldp:
        case INS_ldpsw:
        case INS_ldnp:
        case INS_stp:
        case INS_stnp:
            emitIns_R_R_R_I(ins, attr, reg1, reg2, reg3, 0);
            return;

        case INS_stxr:
        case INS_stxrb:
        case INS_stxrh:
        case INS_stlxr:
        case INS_stlxrb:
        case INS_stlxrh:
            assert(isGeneralRegisterOrZR(reg1));
            assert(isGeneralRegisterOrZR(reg2));
            assert(isGeneralRegisterOrSP(reg3));
            fmt = IF_LS_3D;
            break;

        case INS_casb:
        case INS_casab:
        case INS_casalb:
        case INS_caslb:
        case INS_cash:
        case INS_casah:
        case INS_casalh:
        case INS_caslh:
        case INS_cas:
        case INS_casa:
        case INS_casal:
        case INS_casl:
        case INS_ldaddb:
        case INS_ldaddab:
        case INS_ldaddalb:
        case INS_ldaddlb:
        case INS_ldaddh:
        case INS_ldaddah:
        case INS_ldaddalh:
        case INS_ldaddlh:
        case INS_ldadd:
        case INS_ldadda:
        case INS_ldaddal:
        case INS_ldaddl:
        case INS_ldclral:
        case INS_ldsetal:
        case INS_swpb:
        case INS_swpab:
        case INS_swpalb:
        case INS_swplb:
        case INS_swph:
        case INS_swpah:
        case INS_swpalh:
        case INS_swplh:
        case INS_swp:
        case INS_swpa:
        case INS_swpal:
        case INS_swpl:
            assert(isGeneralRegisterOrZR(reg1));
            assert(isGeneralRegisterOrZR(reg2));
            assert(isGeneralRegisterOrSP(reg3));
            fmt = IF_LS_3E;
            break;

        case INS_sha256h:
        case INS_sha256h2:
        case INS_sha256su1:
        case INS_sha1su0:
        case INS_sha1c:
        case INS_sha1p:
        case INS_sha1m:
            assert(isValidVectorDatasize(size));
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            if (opt == INS_OPTS_NONE)
            {
                elemsize = EA_4BYTE;
                opt      = optMakeArrangement(size, elemsize);
            }
            assert(isValidArrangement(size, opt));
            fmt = IF_DV_3F;
            break;

        case INS_ld2:
        case INS_ld3:
        case INS_ld4:
        case INS_st2:
        case INS_st3:
        case INS_st4:
            assert(opt != INS_OPTS_1D); // .1D format only permitted with LD1 & ST1
            FALLTHROUGH;

        case INS_ld1:
        case INS_ld1_2regs:
        case INS_ld1_3regs:
        case INS_ld1_4regs:
        case INS_st1:
        case INS_st1_2regs:
        case INS_st1_3regs:
        case INS_st1_4regs:
        case INS_ld1r:
        case INS_ld2r:
        case INS_ld3r:
        case INS_ld4r:
            assert(isVectorRegister(reg1));
            assert(isGeneralRegisterOrSP(reg2));
            assert(isGeneralRegister(reg3));
            assert(isValidArrangement(size, opt));

            // Load/Store multiple structures       post-indexed by a register
            // Load single structure and replicate  post-indexed by a register
            reg2 = encodingSPtoZR(reg2);
            fmt  = IF_LS_3F;
            break;

        case INS_addhn:
        case INS_raddhn:
        case INS_rsubhn:
        case INS_subhn:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(size == EA_8BYTE);
            assert(isValidArrangement(size, opt));
            assert(opt != INS_OPTS_1D); // The encoding size = 11, Q = x is reserved.
            fmt = IF_DV_3A;
            break;

        case INS_addhn2:
        case INS_raddhn2:
        case INS_rsubhn2:
        case INS_subhn2:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(size == EA_16BYTE);
            assert(isValidArrangement(size, opt));
            assert(opt != INS_OPTS_2D); // The encoding size = 11, Q = x is reserved.
            fmt = IF_DV_3A;
            break;

        case INS_sabal:
        case INS_sabdl:
        case INS_saddl:
        case INS_saddw:
        case INS_smlal:
        case INS_smlsl:
        case INS_ssubl:
        case INS_ssubw:
        case INS_uabal:
        case INS_uabdl:
        case INS_uaddl:
        case INS_uaddw:
        case INS_umlal:
        case INS_umlsl:
        case INS_usubl:
        case INS_usubw:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(size == EA_8BYTE);
            assert((opt == INS_OPTS_8B) || (opt == INS_OPTS_4H) || (opt == INS_OPTS_2S));
            fmt = IF_DV_3A;
            break;

        case INS_sabal2:
        case INS_sabdl2:
        case INS_saddl2:
        case INS_saddw2:
        case INS_smlal2:
        case INS_smlsl2:
        case INS_ssubl2:
        case INS_ssubw2:
        case INS_umlal2:
        case INS_umlsl2:
        case INS_smull2:
        case INS_uabal2:
        case INS_uabdl2:
        case INS_uaddl2:
        case INS_uaddw2:
        case INS_usubl2:
        case INS_umull2:
        case INS_usubw2:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(size == EA_16BYTE);
            assert((opt == INS_OPTS_16B) || (opt == INS_OPTS_8H) || (opt == INS_OPTS_4S));
            fmt = IF_DV_3A;
            break;

        case INS_sqdmlal:
        case INS_sqdmlsl:
        case INS_sqdmull:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            if (insOptsAnyArrangement(opt))
            {
                assert(size == EA_8BYTE);
                assert((opt == INS_OPTS_4H) || (opt == INS_OPTS_2S));
                fmt = IF_DV_3A;
            }
            else
            {
                assert(insOptsNone(opt));
                assert((size == EA_2BYTE) || (size == EA_4BYTE));
                fmt = IF_DV_3E;
            }
            break;

        case INS_sqdmulh:
        case INS_sqrdmlah:
        case INS_sqrdmlsh:
        case INS_sqrdmulh:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            if (insOptsAnyArrangement(opt))
            {
                assert(isValidVectorDatasize(size));
                elemsize = optGetElemsize(opt);
                assert((elemsize == EA_2BYTE) || (elemsize == EA_4BYTE));
                fmt = IF_DV_3A;
            }
            else
            {
                assert(insOptsNone(opt));
                assert((size == EA_2BYTE) || (size == EA_4BYTE));
                fmt = IF_DV_3E;
            }
            break;

        case INS_sqdmlal2:
        case INS_sqdmlsl2:
        case INS_sqdmull2:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(size == EA_16BYTE);
            assert((opt == INS_OPTS_8H) || (opt == INS_OPTS_4S));
            fmt = IF_DV_3A;
            break;

        case INS_pmul:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(isValidArrangement(size, opt));
            assert((opt == INS_OPTS_8B) || (opt == INS_OPTS_16B));
            fmt = IF_DV_3A;
            break;

        case INS_pmull:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(size == EA_8BYTE);
            assert((opt == INS_OPTS_8B) || (opt == INS_OPTS_1D));
            fmt = IF_DV_3A;
            break;

        case INS_pmull2:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(size == EA_16BYTE);
            assert((opt == INS_OPTS_16B) || (opt == INS_OPTS_2D));
            fmt = IF_DV_3A;
            break;

        case INS_sdot:
        case INS_udot:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(((size == EA_8BYTE) && (opt == INS_OPTS_2S)) || ((size == EA_16BYTE) && (opt == INS_OPTS_4S)));
            fmt = IF_DV_3A;
            break;

        default:
            unreached();
    }

    instrDesc* id = emitNewInstr();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idInsOpt(opt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(reg3);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_R_R_I(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, int32_t imm, insOpts opt, emitAttr attrReg2)
{
    emitAttr  size     = EA_SIZE(attr);
    insFormat fmt      = IF_NONE;
    bool      isLdSt   = false;
    bool      isVector = false;
    bool      isAddSub = false;
    bool      setFlags = false;
    unsigned  scale    = 0;

    switch (ins)
    {
        emitAttr elemsize;

        case INS_extr:
            assert(insOptsNone(opt));
            assert(isValidGeneralDatasize(size));
            assert(isGeneralRegister(reg1));
            assert(isGeneralRegister(reg2));
            assert(isGeneralRegister(reg3));
            assert(isValidImmShift(imm, size));
            fmt = IF_DR_3E;
            break;

        case INS_and:
        case INS_ands:
        case INS_eor:
        case INS_orr:
        case INS_bic:
        case INS_bics:
        case INS_eon:
        case INS_orn:
            assert(isValidGeneralDatasize(size));
            assert(isGeneralRegister(reg1));
            assert(isGeneralRegister(reg2));
            assert(isGeneralRegister(reg3));
            assert(isValidImmShift(imm, size));
            if (imm == 0)
            {
                assert(insOptsNone(opt)); // a zero imm, means no shift kind
                fmt = IF_DR_3A;
            }
            else
            {
                assert(insOptsAnyShift(opt)); // a non-zero imm, must select shift kind
                fmt = IF_DR_3B;
            }
            break;

        case INS_fmul: // by element, imm[0..3] selects the element of reg3
        case INS_fmla:
        case INS_fmls:
        case INS_fmulx:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            if (insOptsAnyArrangement(opt))
            {
                assert(isValidVectorDatasize(size));
                assert(isValidArrangement(size, opt));
                elemsize = optGetElemsize(opt);
                assert(isValidVectorElemsizeFloat(elemsize));
                assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
                assert(opt != INS_OPTS_1D); // Reserved encoding
                fmt = IF_DV_3BI;
            }
            else
            {
                assert(insOptsNone(opt));
                assert(isValidScalarDatasize(size));
                elemsize = size;
                assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
                fmt = IF_DV_3DI;
            }
            break;

        case INS_mul: // by element, imm[0..7] selects the element of reg3
        case INS_mla:
        case INS_mls:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(insOptsAnyArrangement(opt));
            assert(isValidVectorDatasize(size));
            assert(isValidArrangement(size, opt));
            elemsize = optGetElemsize(opt);
            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
            // Only has encodings for H or S elemsize
            assert((elemsize == EA_2BYTE) || (elemsize == EA_4BYTE));
            // Only has encodings for V0..V15
            if ((elemsize == EA_2BYTE) && ((genRegMask(reg3) & RBM_ASIMD_INDEXED_H_ELEMENT_ALLOWED_REGS) == 0))
            {
                noway_assert(!"Invalid reg3");
            }
            fmt = IF_DV_3AI;
            break;

        case INS_add:
        case INS_sub:
            setFlags = false;
            isAddSub = true;
            break;

        case INS_adds:
        case INS_subs:
            setFlags = true;
            isAddSub = true;
            break;

        case INS_ldpsw:
            scale  = 2;
            isLdSt = true;
            break;

        case INS_ldnp:
        case INS_stnp:
            assert(insOptsNone(opt)); // Can't use Pre/Post index on these two instructions
            FALLTHROUGH;

        case INS_ldp:
        case INS_stp:
            if (isVectorRegister(reg1))
            {
                scale    = NaturalScale(size);
                isVector = true;
            }
            else
            {
                scale = (size == EA_8BYTE) ? 3 : 2;
            }
            isLdSt = true;
            break;

        case INS_ld1:
        case INS_ld2:
        case INS_ld3:
        case INS_ld4:
        case INS_st1:
        case INS_st2:
        case INS_st3:
        case INS_st4:
            assert(isVectorRegister(reg1));
            assert(isGeneralRegisterOrSP(reg2));
            assert(isGeneralRegister(reg3));
            assert(insOptsPostIndex(opt));

            elemsize = size;
            assert(isValidVectorElemsize(elemsize));
            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));

            // Load/Store single structure  post-indexed by a register
            reg2 = encodingSPtoZR(reg2);
            fmt  = IF_LS_3G;
            break;

        case INS_ext:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(isValidVectorDatasize(size));
            assert(isValidArrangement(size, opt));
            assert((opt == INS_OPTS_8B) || (opt == INS_OPTS_16B));
            assert(Arm64Imm::IsVecIndex(imm, size, EA_1BYTE));
            fmt = IF_DV_3G;
            break;

        case INS_smlal:
        case INS_smlsl:
        case INS_smull:
        case INS_umlal:
        case INS_umlsl:
        case INS_umull:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(size == EA_8BYTE);
            assert((opt == INS_OPTS_4H) || (opt == INS_OPTS_2S));
            elemsize = optGetElemsize(opt);
            // Restricted to V0-V15 when element size is H.
            if ((elemsize == EA_2BYTE) && ((genRegMask(reg3) & RBM_ASIMD_INDEXED_H_ELEMENT_ALLOWED_REGS) == 0))
            {
                assert(!"Invalid reg3");
            }
            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
            fmt = IF_DV_3AI;
            break;

        case INS_sqdmlal:
        case INS_sqdmlsl:
        case INS_sqdmull:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            if (insOptsAnyArrangement(opt))
            {
                assert(size == EA_8BYTE);
                assert((opt == INS_OPTS_4H) || (opt == INS_OPTS_2S));
                elemsize = optGetElemsize(opt);
                fmt      = IF_DV_3AI;
            }
            else
            {
                assert(insOptsNone(opt));
                assert((size == EA_2BYTE) || (size == EA_4BYTE));
                elemsize = size;
                fmt      = IF_DV_3EI;
            }
            // Restricted to V0-V15 when element size is H.
            if ((elemsize == EA_2BYTE) && ((genRegMask(reg3) & RBM_ASIMD_INDEXED_H_ELEMENT_ALLOWED_REGS) == 0))
            {
                assert(!"Invalid reg3");
            }
            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
            break;

        case INS_sqdmulh:
        case INS_sqrdmlah:
        case INS_sqrdmlsh:
        case INS_sqrdmulh:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            if (insOptsAnyArrangement(opt))
            {
                assert(isValidVectorDatasize(size));
                elemsize = optGetElemsize(opt);
                assert((elemsize == EA_2BYTE) || (elemsize == EA_4BYTE));
                fmt = IF_DV_3AI;
            }
            else
            {
                assert(insOptsNone(opt));
                assert((size == EA_2BYTE) || (size == EA_4BYTE));
                elemsize = size;
                fmt      = IF_DV_3EI;
            }

            // Restricted to V0-V15 when element size is H.
            if ((elemsize == EA_2BYTE) && ((genRegMask(reg3) & RBM_ASIMD_INDEXED_H_ELEMENT_ALLOWED_REGS) == 0))
            {
                assert(!"Invalid reg3");
            }

            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
            break;

        case INS_smlal2:
        case INS_smlsl2:
        case INS_smull2:
        case INS_sqdmlal2:
        case INS_sqdmlsl2:
        case INS_sqdmull2:
        case INS_umlal2:
        case INS_umlsl2:
        case INS_umull2:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(size == EA_16BYTE);
            assert((opt == INS_OPTS_8H) || (opt == INS_OPTS_4S));
            elemsize = optGetElemsize(opt);
            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
            // Restricted to V0-V15 when element size is H
            if ((elemsize == EA_2BYTE) && ((genRegMask(reg3) & RBM_ASIMD_INDEXED_H_ELEMENT_ALLOWED_REGS) == 0))
            {
                assert(!"Invalid reg3");
            }
            fmt = IF_DV_3AI;
            break;

        case INS_sdot:
        case INS_udot:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(((size == EA_8BYTE) && (opt == INS_OPTS_2S)) || ((size == EA_16BYTE) && (opt == INS_OPTS_4S)));
            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, EA_4BYTE));
            fmt = IF_DV_3AI;
            break;

        default:
            unreached();
    }

    if (isLdSt)
    {
        assert(!isAddSub);
        assert(isGeneralRegisterOrSP(reg3));
        assert(insOptsNone(opt) || insOptsIndexed(opt));

        if (isVector)
        {
            assert(isValidVectorLSPDatasize(size));
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert((scale >= 2) && (scale <= 4));
        }
        else
        {
            assert(isValidGeneralDatasize(size));
            assert(isGeneralRegisterOrZR(reg1));
            assert(isGeneralRegisterOrZR(reg2));
            assert((scale == 2) || (scale == 3));
        }

        // Load/Store Pair reserved encodings:
        if (IsLoadIns(ins))
        {
            assert(reg1 != reg2);
        }

        if (insOptsIndexed(opt))
        {
            assert(reg1 != reg3);
            assert(reg2 != reg3);
        }

        reg3 = encodingSPtoZR(reg3);

        int64_t mask = (1ll << scale) - 1; // the mask of low bits that must be zero to encode the immediate

        if (imm == 0)
        {
            assert(insOptsNone(opt)); // PRE/POST Index doesn't make sense with an immediate of zero

            fmt = IF_LS_3B;
        }
        else
        {
            assert((imm & mask) == 0);
            imm >>= scale;
            assert((imm >= -64) && (imm <= 63));
            fmt = IF_LS_3C;
        }
    }
    else if (isAddSub)
    {
        bool reg2IsSP = (reg2 == REG_SP);
        assert(!isLdSt);
        assert(isValidGeneralDatasize(size));
        assert(isGeneralRegister(reg3));

        if (setFlags || insOptsAluShift(opt)) // Can't encode SP in reg1 with setFlags or AluShift option
        {
            assert(isGeneralRegisterOrZR(reg1));
        }
        else
        {
            assert(isGeneralRegisterOrSP(reg1));
            reg1 = encodingSPtoZR(reg1);
        }

        if (insOptsAluShift(opt)) // Can't encode SP in reg2 with AluShift option
        {
            assert(isGeneralRegister(reg2));
        }
        else
        {
            assert(isGeneralRegisterOrSP(reg2));
            reg2 = encodingSPtoZR(reg2);
        }

        if (insOptsAnyExtend(opt))
        {
            assert((imm >= 0) && (imm <= 4));

            fmt = IF_DR_3C;
        }
        else if (insOptsAluShift(opt))
        {
            // imm should be non-zero and in [1..63]
            assert(isValidImmShift(imm, size) && (imm != 0));
            fmt = IF_DR_3B;
        }
        else
        {
            assert(imm == 0);
            assert(insOptsNone(opt));

            if (reg2IsSP)
            {
                // To encode the SP register as reg2 we must use the IF_DR_3C encoding
                // and also specify a LSL of zero (imm == 0)
                opt = INS_OPTS_LSL;
                fmt = IF_DR_3C;
            }
            else
            {
                fmt = IF_DR_3A;
            }
        }
    }

    instrDesc* id = emitNewInstrCns(imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idInsOpt(opt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(reg3);

    assert((attrReg2 == EA_UNKNOWN) || (fmt == IF_LS_3B) || (fmt == IF_LS_3C));
    id->idGCrefReg2(EA_GC_TYPE(attrReg2));

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_R_R_Ext(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, insOpts opt, int shiftAmount)
{
    assert(isGeneralRegisterOrSP(reg2));
    assert(isGeneralRegister(reg3));
    assert(insOptsLSExtend(opt));
    assert(!insOptsIndexed(opt) || (reg1 != reg2));

    emitAttr size = EA_SIZE(attr);
    int      scale;

    switch (ins)
    {
        case INS_ldrb:
        case INS_ldrsb:
        case INS_strb:
            assert(isValidGeneralLSDatasize(size));
            assert(isGeneralRegisterOrZR(reg1));
            scale = 0;
            break;
        case INS_ldrh:
        case INS_ldrsh:
        case INS_strh:
            assert(isValidGeneralLSDatasize(size));
            assert(isGeneralRegisterOrZR(reg1));
            scale = 1;
            break;
        case INS_ldrsw:
            assert(isValidGeneralLSDatasize(size));
            assert(isGeneralRegisterOrZR(reg1));
            scale = 2;
            break;
        case INS_ldr:
        case INS_str:
            if (isVectorRegister(reg1))
            {
                assert(isValidVectorLSDatasize(size));
                assert(isVectorRegister(reg1));
                scale = NaturalScale(size);
            }
            else
            {
                assert(isValidGeneralDatasize(size));
                assert(isGeneralRegisterOrZR(reg1));
                scale = size == EA_8BYTE ? 3 : 2;
            }
            break;
        default:
            unreached();
    }

    if (shiftAmount == -1)
    {
        shiftAmount = insOptsLSL(opt) ? scale : 0;
    }

    assert((shiftAmount == scale) || (shiftAmount == 0));

    instrDesc* id = emitNewInstr();
    id->idIns(ins);
    id->idInsFmt(IF_LS_3A);
    id->idInsOpt(opt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(encodingSPtoZR(reg2));
    id->idReg3(reg3);
    id->idReg3Scaled(shiftAmount == scale);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_R_I_I(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, int imm1, int imm2, insOpts opt)
{
    emitAttr  size = EA_SIZE(attr);
    insFormat fmt;
    ssize_t   imm;

    switch (ins)
    {
        int lsb;
        int width;

        case INS_bfm:
        case INS_sbfm:
        case INS_ubfm:
            assert(isGeneralRegister(reg1));
            assert(ins == INS_bfm ? isGeneralRegisterOrZR(reg2) : isGeneralRegister(reg2));
            assert(isValidImmShift(imm1, size));
            assert(isValidImmShift(imm2, size));
            assert(insOptsNone(opt));
            imm = PackBitMaskImm(imm2, imm1, size);
            fmt = IF_DI_2D;
            break;

        case INS_bfi:
        case INS_sbfiz:
        case INS_ubfiz:
            assert(isGeneralRegister(reg1));
            assert(isGeneralRegister(reg2));
            lsb   = getBitWidth(size) - imm1;
            width = imm2 - 1;
            assert(isValidImmShift(lsb, size));
            assert(isValidImmShift(width, size));
            assert(insOptsNone(opt));
            imm = PackBitMaskImm(width, lsb, size);
            fmt = IF_DI_2D;
            break;

        case INS_bfxil:
        case INS_sbfx:
        case INS_ubfx:
            assert(isGeneralRegister(reg1));
            assert(isGeneralRegister(reg2));
            lsb   = imm1;
            width = imm2 + imm1 - 1;
            assert(isValidImmShift(lsb, size));
            assert(isValidImmShift(width, size));
            assert(insOptsNone(opt));
            imm = PackBitMaskImm(width, lsb, size);
            fmt = IF_DI_2D;
            break;

        case INS_mov:
        case INS_ins:
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isValidVectorElemsize(size));
            assert(Arm64Imm::IsVecIndex(imm1, EA_16BYTE, size));
            assert(Arm64Imm::IsVecIndex(imm2, EA_16BYTE, size));
            assert(insOptsNone(opt));
            imm = (imm1 << 4) + imm2;
            fmt = IF_DV_2F;
            break;

        case INS_ld1:
        case INS_ld2:
        case INS_ld3:
        case INS_ld4:
        case INS_st1:
        case INS_st2:
        case INS_st3:
        case INS_st4:
            assert(isVectorRegister(reg1));
            assert(isGeneralRegisterOrSP(reg2));
            assert(isValidVectorElemsize(size));
            assert(Arm64Imm::IsVecIndex(imm1, EA_16BYTE, size));
            assert(size * insGetRegisterListSize(ins) == static_cast<unsigned>(imm2));
            assert(insOptsPostIndex(opt));
            reg2 = encodingSPtoZR(reg2);
            imm  = imm1;
            fmt  = IF_LS_2G;
            break;

        default:
            unreached();
    }

    instrDesc* id = emitNewInstrSC(imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idInsOpt(opt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, RegNum reg4)
{
    insFormat fmt;

    switch (ins)
    {
        case INS_madd:
        case INS_msub:
        case INS_smaddl:
        case INS_smsubl:
        case INS_umaddl:
        case INS_umsubl:
            assert(isValidGeneralDatasize(EA_SIZE(attr)));
            assert(isGeneralRegister(reg1));
            assert(isGeneralRegister(reg2));
            assert(isGeneralRegister(reg3));
            assert(isGeneralRegister(reg4));
            fmt = IF_DR_4A;
            break;

        case INS_fmadd:
        case INS_fmsub:
        case INS_fnmadd:
        case INS_fnmsub:
            assert(isValidScalarDatasize(EA_SIZE(attr)));
            assert(isVectorRegister(reg1));
            assert(isVectorRegister(reg2));
            assert(isVectorRegister(reg3));
            assert(isVectorRegister(reg4));
            fmt = IF_DV_4A;
            break;

        default:
            unreached();
    }

    instrDesc* id = emitNewInstr();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(reg3);
    id->idReg4(reg4);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_COND(instruction ins, emitAttr attr, RegNum reg, insCond cond)
{
    assert((ins == INS_cset) || (ins == INS_csetm));
    assert(isGeneralRegister(reg));

    instrDesc* id = emitNewInstrSC(PackCondImm(cond));
    id->idIns(ins);
    id->idInsFmt(IF_DR_1D);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_R_COND(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, insCond cond)
{
    assert((ins == INS_cinc) || (ins == INS_cinv) || (ins == INS_cneg));
    assert(isGeneralRegister(reg1));
    assert(isGeneralRegister(reg2));

    instrDesc* id = emitNewInstrSC(PackCondImm(cond));
    id->idIns(ins);
    id->idInsFmt(IF_DR_2D);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_R_R_COND(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, insCond cond)
{
    assert((ins == INS_csel) || (ins == INS_csinc) || (ins == INS_csinv) || (ins == INS_csneg));
    assert(isGeneralRegister(reg1));
    assert(isGeneralRegister(reg2));
    assert(isGeneralRegister(reg3));

    instrDesc* id = emitNewInstr();
    id->idIns(ins);
    id->idInsFmt(IF_DR_3D);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(reg3);
    id->idSmallCns(PackCondImm(cond));

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_R_FLAGS_COND(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, insCflags flags, insCond cond)
{
    assert((ins == INS_ccmp) || (ins == INS_ccmn));
    assert(isGeneralRegister(reg1));
    assert(isGeneralRegister(reg2));

    instrDesc* id = emitNewInstrSC(PackCondImm(cond, flags));
    id->idIns(ins);
    id->idInsFmt(IF_DR_2I);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_I_FLAGS_COND(
    instruction ins, emitAttr attr, RegNum reg, int imm, insCflags flags, insCond cond)
{
    assert((ins == INS_ccmp) || (ins == INS_ccmn));
    assert(isGeneralRegister(reg));

    if (imm < 0)
    {
        ins = insReverse(ins);
        imm = -imm;
    }

    instrDesc* id = emitNewInstrSC(PackCondImm(cond, flags, imm));
    id->idIns(ins);
    id->idInsFmt(IF_DI_1F);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_BARR(instruction ins, insBarrier barrier)
{
    assert((ins == INS_dsb) || (ins == INS_dmb) || (ins == INS_isb));

    instrDesc* id = emitNewInstrSC(static_cast<uint32_t>(barrier));
    id->idIns(ins);
    id->idInsFmt(IF_SI_0B);
    id->idOpSize(EA_8BYTE);

    appendToCurIG(id);
}

#ifdef DEBUG
static bool IsLoad(instruction ins)
{
    return (ins == INS_ldrb) || (ins == INS_ldrsb) || (ins == INS_ldrh) || (ins == INS_ldrsh) || (ins == INS_ldrsw) ||
           (ins == INS_ldr);
}

static bool IsStore(instruction ins)
{
    return (ins == INS_strb) || (ins == INS_strh) || (ins == INS_str);
}
#endif

void Arm64Emitter::emitIns_R_S(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s)
{
    assert(IsLoad(ins) || (ins == INS_lea));
    assert(s.varOffs >= 0);

    Ins_R_S(ins, attr, reg, s);
}

void Arm64Emitter::emitIns_S_R(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s)
{
    assert(IsStore(ins));
    assert(s.varOffs >= 0);

    Ins_R_S(ins, attr, reg, s);
}

void Arm64Emitter::emitIns_R_R_S_S(
    instruction ins, emitAttr attr1, emitAttr attr2, RegNum reg1, RegNum reg2, StackAddrMode s)
{
    assert((ins == INS_ldp) || (ins == INS_ldnp));

    Ins_R_R_S(ins, attr1, attr2, reg1, reg2, s);
}

void Arm64Emitter::emitIns_S_S_R_R(
    instruction ins, emitAttr attr1, emitAttr attr2, RegNum reg1, RegNum reg2, StackAddrMode s)
{
    assert((ins == INS_stp) || (ins == INS_stnp));

    Ins_R_R_S(ins, attr1, attr2, reg1, reg2, s);
}

static constexpr bool IsSignedImm9(int imm)
{
    return (-256 <= imm) && (imm <= 255);
}

static constexpr bool IsUnsignedImm12(int imm, unsigned shift = 0)
{
    return (imm & ~(((1 << 12) - 1) << shift)) == 0;
}

static constexpr bool IsSignedImm7(int imm, unsigned shift)
{
    return ((imm & ((1 << shift) - 1)) == 0) && (-64 <= (imm >> shift)) && ((imm >> shift) < 64);
}

static bool InsMayBeGCSlotStore(instruction ins)
{
    return (ins == INS_str) || (ins == INS_stur);
}

static bool InsMayBeGCSlotStorePair(instruction ins)
{
    return (ins == INS_stp) || (ins == INS_stnp);
}

void Arm64Emitter::Ins_R_S(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s)
{
    assert(IsLoad(ins) || IsStore(ins) || (ins == INS_lea));
    assert(s.varOffs >= 0);

    bool fpBased;
    int  baseOffset = emitComp->lvaFrameAddress(s.varNum, &fpBased) + s.varOffs;

    emitAttr size = EA_SIZE(attr);
    int32_t  imm  = baseOffset;
    unsigned scale;

    switch (ins)
    {
        case INS_ldrb:
        case INS_ldrsb:
            assert(isGeneralRegister(reg));
            FALLTHROUGH;
        case INS_strb:
            assert(isGeneralRegisterOrZR(reg));
            scale = 0;
            break;
        case INS_ldrh:
        case INS_ldrsh:
            assert(isGeneralRegister(reg));
            FALLTHROUGH;
        case INS_strh:
            assert(isGeneralRegisterOrZR(reg));
            scale = 1;
            break;
        case INS_ldr:
        case INS_str:
            scale = genLog2(EA_SIZE_IN_BYTES(size));
            assert((2 <= scale) && (scale <= 4));
            break;
        case INS_ldrsw:
            assert((size == EA_8BYTE) && isGeneralRegister(reg));
            scale = 2;
            break;
        case INS_lea:
            assert((size == EA_8BYTE) && isGeneralRegister(reg));
            scale = 0;
            break;
        default:
            unreached();
    }

    insFormat fmt;
    RegNum    offsReg = REG_NA;

    if (ins == INS_lea)
    {
        if (imm >= 0)
        {
            ins = INS_add;
        }
        else
        {
            ins = INS_sub;
            imm = -imm;
        }

        if (IsUnsignedImm12(imm))
        {
            fmt = IF_DI_2A;
        }
        else
        {
            offsReg = codeGen->rsGetRsvdReg();
            codeGen->instGen_Set_Reg_To_Imm(EA_8BYTE, offsReg, imm);
            fmt = IF_DR_3A;
            imm = 0;
        }
    }
    else if (imm == 0)
    {
        fmt = IF_LS_2A;
    }
    else if (IsUnsignedImm12(imm, scale))
    {
        imm >>= scale;
        fmt = IF_LS_2B;
    }
    else if (IsSignedImm9(imm))
    {
        fmt = IF_LS_2C;
    }
    else
    {
        offsReg = codeGen->rsGetRsvdReg();
        codeGen->instGen_Set_Reg_To_Imm(EA_8BYTE, offsReg, imm);
        fmt = IF_LS_3A;
        imm = 0;
    }

    // TODO-ARM64-CQ: with compLocallocUsed, should we use REG_SAVED_LOCALLOC_SP instead?
    RegNum baseReg = fpBased ? REG_FP : REG_ZR;

    if (emitComp->opts.OptimizationEnabled() && IsRedundantLdStr(ins, reg, baseReg, imm, size, fmt))
    {
        return;
    }

    instrDesc* id = emitNewInstrCns(imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg);
    id->idReg2(baseReg);
    id->SetVarAddr(INDEBUG(s));

    if (offsReg != REG_NA)
    {
        id->idReg3(offsReg);
        id->idReg3Scaled(false);
    }

    if (InsMayBeGCSlotStore(ins) && EA_IS_GCREF_OR_BYREF(attr))
    {
        id->idAddr()->lclOffset = baseOffset;

        if (s.varNum < 0)
        {
            assert(s.varOffs == 0);
            id->idAddr()->isTrackedGCSlotStore = codeGen->spillTemps.TrackGCSpillTemps();
        }
        else if (static_cast<unsigned>(s.varNum) == emitComp->lvaOutgoingArgSpaceVar)
        {
            id->idAddr()->isGCArgStore = true;
        }
        else if ((s.varOffs == 0) && (emitComp->lvaGetDesc(static_cast<unsigned>(s.varNum))->HasGCSlotLiveness()))
        {
            id->idAddr()->isTrackedGCSlotStore = true;
        }
    }

    appendToCurIG(id);
}

void Arm64Emitter::Ins_R_R_S(instruction ins, emitAttr attr1, emitAttr attr2, RegNum reg1, RegNum reg2, StackAddrMode s)
{
    assert((EA_SIZE(attr1) == EA_8BYTE) || (attr1 == EA_4BYTE) || (isVectorRegister(reg1) && (attr1 == EA_16BYTE)));
    assert((EA_SIZE(attr2) == EA_8BYTE) || (attr2 == EA_4BYTE) || (isVectorRegister(reg2) && (attr2 == EA_16BYTE)));
    assert(EA_SIZE(attr1) == EA_SIZE(attr2));
    assert(s.varOffs >= 0);

    bool fpBased;
    int  baseOffset = emitComp->lvaFrameAddress(s.varNum, &fpBased) + s.varOffs;

    RegNum    baseReg = fpBased ? REG_FP : REG_ZR;
    int32_t   imm     = baseOffset;
    unsigned  scale   = genLog2(EA_SIZE_IN_BYTES(attr1));
    insFormat fmt;

    if (imm == 0)
    {
        fmt = IF_LS_3B;
    }
    else if (IsSignedImm7(imm, scale))
    {
        imm >>= scale;
        fmt = IF_LS_3C;
    }
    else
    {
        RegNum tempReg = codeGen->rsGetRsvdReg();
        emitIns_R_R_Imm(INS_add, EA_8BYTE, tempReg, baseReg, imm);
        baseReg = tempReg;
        imm     = 0;
        fmt     = IF_LS_3B;
    }

    instrDesc* id = emitNewInstrCns(imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idOpSize(EA_SIZE(attr1));
    id->idGCref(EA_GC_TYPE(attr1));
    id->idGCrefReg2(EA_GC_TYPE(attr2));
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(baseReg);
    id->SetVarAddr(INDEBUG(s));

    if (InsMayBeGCSlotStorePair(ins) && (EA_IS_GCREF_OR_BYREF(attr1) || EA_IS_GCREF_OR_BYREF(attr2)))
    {
        id->idAddr()->lclOffset = baseOffset;

        noway_assert(s.varNum >= 0);

        if (static_cast<unsigned>(s.varNum) == emitComp->lvaOutgoingArgSpaceVar)
        {
            id->idAddr()->isGCArgStore = true;
        }
        else if ((s.varOffs == 0) && (emitComp->lvaGetDesc(static_cast<unsigned>(s.varNum))->HasGCSlotLiveness()))
        {
            id->idAddr()->isTrackedGCSlotStore = true;
        }
    }

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_R_C(instruction ins, emitAttr attr, RegNum reg, RegNum addrReg, ConstData* data)
{
    emitAttr  size = EA_SIZE(attr);
    insFormat fmt;

    if (ins == INS_adr)
    {
        assert(isGeneralRegister(reg));
        assert(size == EA_8BYTE);

        fmt = IF_LARGEADR;
    }
    else
    {
        assert(ins == INS_ldr);

        if (isVectorRegister(reg))
        {
            assert(isValidVectorLSDatasize(size));
            // For vector (float/double) register, we should have an integer address reg
            // to compute long address which consists of page address and page offset.
            // For integer constant, this is not needed since the dest reg can be used
            // to compute address as well as contain the final contents.
            assert(isGeneralRegister(reg) || (addrReg != REG_NA));
        }
        else
        {
            assert(isGeneralRegister(reg));
            assert(isValidGeneralDatasize(size));
        }

        fmt = IF_LARGELDC;
    }

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idOpSize(size);
    id->idReg1(reg);
    id->SetConstData(data);
    // We put the constant data right after the hot code section, cold code will need relocs.
    id->idSetIsCnsReloc(emitComp->opts.compReloc && emitCurIG->IsCold());

    if (addrReg != REG_NA)
    {
        id->idReg2(addrReg);
    }

    appendToCurIG(id);
}

// This computes address from the immediate which is relocatable.
void Arm64Emitter::emitIns_R_AH(RegNum reg, void* addr DEBUGARG(void* handle) DEBUGARG(HandleKind handleKind))
{
    assert(emitComp->opts.compReloc);

    instrDesc* ij = emitNewInstr();
    ij->idIns(INS_adrp);
    ij->idInsFmt(IF_DI_1E);
    ij->idOpSize(EA_8BYTE);
    ij->idReg1(reg);
    ij->idSetIsCnsReloc();
    ij->SetAddr(addr);
#ifdef DEBUG
    ij->idDebugOnlyInfo()->idHandle     = handle;
    ij->idDebugOnlyInfo()->idHandleKind = handleKind;
#endif

    appendToCurIG(ij);

    instrDesc* id = emitNewInstr();
    id->idIns(INS_add);
    id->idInsFmt(IF_DI_2A);
    id->idOpSize(EA_8BYTE);
    id->idSetIsCnsReloc();
    id->idReg1(reg);
    id->idReg2(reg);
    id->SetAddr(addr);

    appendToCurIG(id);
}

void instrDescJmp::SetShortJump()
{
    assert(!idIsCnsReloc());

    insFormat fmt;

    if (idInsFmt() == IF_LARGEJMP)
    {
        switch (idIns())
        {
            case INS_cbz:
            case INS_cbnz:
                fmt = IF_BI_1A;
                break;
            case INS_tbz:
            case INS_tbnz:
                fmt = IF_BI_1B;
                break;
            default:
                fmt = IF_BI_0B;
                break;
        }
    }
    else if (idInsFmt() == IF_LARGEADR)
    {
        fmt = IF_SMALLADR;
    }
    else
    {
        assert(idInsFmt() == IF_LARGELDC);
        fmt = IF_LS_1A;
    }

    idInsFmt(fmt);
}

void Arm64Emitter::emitIns_R_L(RegNum reg, insGroup* label)
{
    assert(label != nullptr);

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(INS_adr);
    id->idInsFmt(IF_LARGEADR);
    id->idOpSize(EA_8BYTE);
    id->idSetIsCnsReloc(emitComp->opts.compReloc && InDifferentRegions(emitCurIG, label));
    id->idReg1(reg);
    id->SetLabel(label);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_J_R(instruction ins, emitAttr attr, insGroup* label, RegNum reg)
{
    assert((ins == INS_cbz) || (ins == INS_cbnz));
    assert(label != nullptr);

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(IF_LARGEJMP);
    id->idOpSize(EA_SIZE(attr));
    id->idSetIsCnsReloc(emitComp->opts.compReloc && InDifferentRegions(emitCurIG, label));
    id->idReg1(reg);
    id->SetLabel(label);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_J_R_I(instruction ins, emitAttr attr, insGroup* label, RegNum reg, int imm)
{
    assert((ins == INS_tbz) || (ins == INS_tbnz));
    assert((EA_SIZE(attr) == EA_4BYTE) || (EA_SIZE(attr) == EA_8BYTE));
    assert(label != nullptr);
    assert(imm < ((EA_SIZE(attr) == EA_4BYTE) ? 32 : 64));

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(IF_LARGEJMP);
    id->idOpSize(EA_SIZE(attr));
    id->idSetIsCnsReloc(emitComp->opts.compReloc && InDifferentRegions(emitCurIG, label));
    id->idReg1(reg);
    id->idSmallCns(imm);
    id->SetLabel(label);

    appendToCurIG(id);
}

#ifdef DEBUG
static bool IsConditionalBranch(instruction ins)
{
    return (INS_beq <= ins) && (ins <= INS_ble);
}

static bool IsBranch(instruction ins)
{
    return (ins == INS_b) || IsConditionalBranch(ins);
}
#endif

void Arm64Emitter::emitIns_J(instruction ins, int instrCount)
{
    assert(IsMainProlog(emitCurIG));
    assert(IsBranch(ins));
    assert(instrCount < 0);

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(ins == INS_b ? IF_BI_0A : IF_BI_0B);
    id->SetInstrCount(instrCount);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_J(instruction ins, insGroup* label)
{
    assert(IsBranch(ins));
    assert(emitCurIG->GetFuncletIndex() == label->GetFuncletIndex());

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(ins == INS_b ? IF_BI_0A : IF_LARGEJMP);
    id->idSetIsCnsReloc(emitComp->opts.compReloc && InDifferentRegions(emitCurIG, label));
    id->SetLabel(label);

    appendToCurIG(id);
}

void Arm64Emitter::emitIns_CallFinally(insGroup* label)
{
    assert(codeGen->GetCurrentBlock()->bbJumpKind == BBJ_CALLFINALLY);
    INDEBUG(VerifyCallFinally(label));

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(INS_bl_local);
    id->idInsFmt(IF_BI_0A);
    id->SetLabel(label);

    appendToCurIG(id);
}

// Add a call instruction (direct or indirect).
//
// EC_FUNC_TOKEN : addr is the method address
// EC_INDIR_R    : call ireg (addr has to be null)
//
// Please consult the "debugger team notification" comment in genFnProlog().
//
void Arm64Emitter::emitIns_Call(EmitCallType          kind,
                                CORINFO_METHOD_HANDLE methodHandle DEBUGARG(CORINFO_SIG_INFO* sigInfo),
                                void*    addr,
                                emitAttr retRegAttr,
                                emitAttr retReg2Attr,
                                RegNum   reg,
                                bool     isJump)
{
    assert((kind == EC_INDIR_R) || (reg == REG_NA));
    assert((kind != EC_INDIR_R) || (addr == nullptr));
    assert((kind != EC_INDIR_R) || (reg != REG_NA));

    instrDesc* id = emitNewInstrCall(methodHandle, retRegAttr, retReg2Attr);

    if (kind == EC_INDIR_R)
    {
        id->idIns(isJump ? INS_br_tail : INS_blr);
        id->idInsFmt(IF_BR_1B);
        id->idReg3(reg);
    }
    else
    {
        assert(kind == EC_FUNC_TOKEN);
        assert(addr != nullptr);

        id->idIns(isJump ? INS_b_tail : INS_bl);
        id->idInsFmt(IF_BI_0C);
        id->idSetIsCnsReloc(emitComp->opts.compReloc);
        id->SetAddr(addr);
    }

#ifdef DEBUG
    id->idDebugOnlyInfo()->idHandle  = methodHandle;
    id->idDebugOnlyInfo()->idCallSig = sigInfo;
#endif

#ifdef LATE_DISASM
    if (addr != nullptr)
    {
        disSetMethod(reinterpret_cast<size_t>(addr), methodHandle);
    }
#endif

    appendToCurIG(id);
}

void EmitterBase::EncodeCallGCRegs(regMaskTP regs, instrDesc* id)
{
    static_assert_no_msg(instrDesc::RegBits >= 5);
    assert((regs & RBM_CALLEE_TRASH) == RBM_NONE);

    unsigned encoded = 0;

    if ((regs & RBM_R19) != RBM_NONE)
        encoded |= 0x01;
    if ((regs & RBM_R20) != RBM_NONE)
        encoded |= 0x02;
    if ((regs & RBM_R21) != RBM_NONE)
        encoded |= 0x04;
    if ((regs & RBM_R22) != RBM_NONE)
        encoded |= 0x08;
    if ((regs & RBM_R23) != RBM_NONE)
        encoded |= 0x10;

    id->idReg1(static_cast<RegNum>(encoded));

    encoded = 0;

    if ((regs & RBM_R24) != RBM_NONE)
        encoded |= 0x01;
    if ((regs & RBM_R25) != RBM_NONE)
        encoded |= 0x02;
    if ((regs & RBM_R26) != RBM_NONE)
        encoded |= 0x04;
    if ((regs & RBM_R27) != RBM_NONE)
        encoded |= 0x08;
    if ((regs & RBM_R28) != RBM_NONE)
        encoded |= 0x10;

    id->idReg2(static_cast<RegNum>(encoded));
}

unsigned EmitterBase::DecodeCallGCRegs(instrDesc* id)
{
    unsigned encoded = id->idReg1() | (id->idReg2() << 8);
    unsigned regs    = 0;

    if ((encoded & 0x01) != 0)
        regs |= RBM_R19;
    if ((encoded & 0x02) != 0)
        regs |= RBM_R20;
    if ((encoded & 0x04) != 0)
        regs |= RBM_R21;
    if ((encoded & 0x08) != 0)
        regs |= RBM_R22;
    if ((encoded & 0x10) != 0)
        regs |= RBM_R23;

    if ((encoded & 0x0100) != 0)
        regs |= RBM_R24;
    if ((encoded & 0x0200) != 0)
        regs |= RBM_R25;
    if ((encoded & 0x0400) != 0)
        regs |= RBM_R26;
    if ((encoded & 0x0800) != 0)
        regs |= RBM_R27;
    if ((encoded & 0x1000) != 0)
        regs |= RBM_R28;

    return regs;
}

void EmitterBase::ShortenBranches()
{
    if (emitJumpList == nullptr)
    {
        return;
    }

#ifdef DEBUG
    if (JitConfig.JitLongAddress())
    {
        JITDUMP("JitLongAddress is set, not shortening any label access\n");
        return;
    }

    if (emitComp->verbose)
    {
        printf("\nInstruction groups before jump shortening:\n\n");
        emitDispIGlist(true);
    }
#endif

AGAIN:
    INDEBUG(emitCheckIGoffsets());

    uint32_t      totalCodeSize        = GetCodeSize();
    uint32_t      minDistanceOverflow  = UINT32_MAX;
    uint32_t      totalSizeReduction   = 0;
    uint32_t      instrIGSizeReduction = 0;
    instrDescJmp* previousInstr        = nullptr;
    insGroup*     previousInstrIG      = emitJumpList->idjIG;

    for (instrDescJmp *instr = emitJumpList; instr != nullptr; previousInstr = instr, instr = instr->idjNext)
    {
        insGroup* instrIG = instr->idjIG;

        if (previousInstrIG == instrIG)
        {
            instr->idjOffs -= instrIGSizeReduction;

            assert((previousInstr == nullptr) || (instr->idjOffs >= previousInstr->idjOffs));
        }
        else
        {
            instrIGSizeReduction = 0;

            for (insGroup* ig = previousInstrIG->igNext; ig != instrIG->igNext; ig = ig->igNext)
            {
                JITDUMP(FMT_IG " moved back from %04X", ig->GetId(), ig->igOffs);
                ig->igOffs -= totalSizeReduction;
                JITDUMP(" to % 04X\n", ig->igOffs);
            }

            assert(instrIG->igOffs >= previousInstrIG->igOffs);

            previousInstrIG = instrIG;
        }

        if (instr->idIsCnsReloc())
        {
            continue;
        }

        uint32_t instrOffs   = instrIG->igOffs + instr->idjOffs;
        uint32_t currentSize = instr->idCodeSize();
        int32_t  distanceOverflow;

        if (instr->HasConstData())
        {
            if (currentSize <= 4)
            {
                continue;
            }

            uint32_t dataOffs = instr->GetConstData()->offset;

            int64_t imm = instr->emitGetInsSC();
            assert((imm >= 0) && (imm < 0x1000)); // 0x1000 is arbitrary, currently 'imm' is always 0

            dataOffs += static_cast<uint32_t>(imm);
            assert(dataOffs < roData.size);

            // Conservatively assume JIT data starts after the entire code size.
            assert(totalCodeSize > 0);
            dataOffs += totalCodeSize;

            distanceOverflow = (dataOffs - instrOffs) - ((1 << 20) - 1);

            JITDUMP("RoData address IN%04X at %04X (" FMT_IG "), data at %04X, distance %d, overflow %d%s\n",
                    instr->idDebugOnlyInfo()->idNum, instrOffs, instrIG->GetId(), dataOffs, dataOffs - instrOffs,
                    distanceOverflow, distanceOverflow <= 0 ? ", adr" : "adrp/add");
        }
        else if (instr->HasLabel())
        {
            insGroup* label     = instr->GetLabel();
            uint32_t  labelOffs = label->igOffs;

            if (label->igNum > instrIG->igNum)
            {
                labelOffs -= totalSizeReduction;
            }

            if (((instr->idInsFmt() != IF_LARGEADR) || (instr->idInsFmt() != IF_SMALLADR)) &&
                (instrOffs + currentSize == labelOffs))
            {
                // Removing a "jump to next" could produce another "jump to next", we need to force another pass
                // to eliminate that too. Ideally we'd traverse the jump list backwards, but it's a forward only
                // list and given the rarity of such nested jumps it's hard to justify the extra code and memory
                // required to traverse the list both ways.
                minDistanceOverflow = 0;

                instr->idInsFmt(IF_NOP_JMP);

                instrIG->igSize -= static_cast<uint16_t>(currentSize);
                instrIG->igFlags |= IGF_UPD_ISZ;
                instrIGSizeReduction += currentSize;
                totalSizeReduction += currentSize;

                continue;
            }

            if (currentSize <= 4)
            {
                continue;
            }

            int32_t maxNegativeDistance;
            int32_t maxPositiveDistance;

            if ((instr->idIns() == INS_tbz) || (instr->idIns() == INS_tbnz))
            {
                maxNegativeDistance = -(1 << 15);
                maxPositiveDistance = (1 << 15) - 1;
            }
            else
            {
                maxNegativeDistance = -(1 << 20);
                maxPositiveDistance = (1 << 20) - 1;
            }

            if (label->igNum > instrIG->igNum)
            {
                distanceOverflow = (labelOffs - instrOffs) - maxPositiveDistance;
            }
            else
            {
                distanceOverflow = (instrOffs - labelOffs) + maxNegativeDistance;
            }

            JITDUMP("Jump IN%04X from %04X (" FMT_IG ") to %04X (" FMT_IG "), distance %d, overflow %d%s\n",
                    instr->idDebugOnlyInfo()->idNum, instrOffs, instrIG->GetId(), labelOffs, label->GetId(),
                    labelOffs - instrOffs, distanceOverflow, distanceOverflow <= 0 ? ", short" : "");
        }
        else
        {
            continue;
        }

        if (distanceOverflow > 0)
        {
            minDistanceOverflow = Min(minDistanceOverflow, static_cast<uint32_t>(distanceOverflow));

            continue;
        }

        assert(currentSize > 4);

        instr->SetShortJump();
        assert(instr->idCodeSize() == 4);

        uint32_t sizeReduction = currentSize - 4;
        assert((sizeReduction == 4) || (sizeReduction == 8));
        instrIG->igSize -= static_cast<uint16_t>(sizeReduction);
        instrIG->igFlags |= IGF_UPD_ISZ;
        instrIGSizeReduction += sizeReduction;
        totalSizeReduction += sizeReduction;
    }

    if (totalSizeReduction != 0)
    {
        for (insGroup* ig = previousInstrIG->igNext; ig != nullptr; ig = ig->igNext)
        {
            JITDUMP(FMT_IG " moved back from %04X", ig->GetId(), ig->igOffs);
            ig->igOffs -= totalSizeReduction;
            JITDUMP(" to % 04X\n", ig->igOffs);
        }

        JITDUMP("Total size reduction %u, min distance overflow %u\n", totalSizeReduction, minDistanceOverflow);

        if (minDistanceOverflow <= totalSizeReduction)
        {
            JITDUMP("Iterating branch shortening\n");

            goto AGAIN;
        }

        INDEBUG(emitCheckIGoffsets());
    }

#ifdef DEBUG
    if (emitComp->verbose)
    {
        printf("\nLabels list after the jump shortening:\n\n");
        emitDispIGlist(false);
    }
#endif
}

// Returns an encoding for the specified register used in the 'Rd' position
static uint32_t insEncodeReg_Rd(RegNum reg)
{
    assert(isIntegerRegister(reg));
    uint32_t ureg = (uint32_t)reg;
    assert((ureg >= 0) && (ureg <= 31));
    return ureg;
}

// Returns an encoding for the specified register used in the 'Rt' position
static uint32_t insEncodeReg_Rt(RegNum reg)
{
    assert(isIntegerRegister(reg));
    uint32_t ureg = (uint32_t)reg;
    assert((ureg >= 0) && (ureg <= 31));
    return ureg;
}

// Returns an encoding for the specified register used in the 'Rn' position
static uint32_t insEncodeReg_Rn(RegNum reg)
{
    assert(isIntegerRegister(reg));
    uint32_t ureg = (uint32_t)reg;
    assert((ureg >= 0) && (ureg <= 31));
    return ureg << 5;
}

// Returns an encoding for the specified register used in the 'Rm' position
static uint32_t insEncodeReg_Rm(RegNum reg)
{
    assert(isIntegerRegister(reg));
    uint32_t ureg = (uint32_t)reg;
    assert((ureg >= 0) && (ureg <= 31));
    return ureg << 16;
}

// Returns an encoding for the specified register used in the 'Ra' position
static uint32_t insEncodeReg_Ra(RegNum reg)
{
    assert(isIntegerRegister(reg));
    uint32_t ureg = (uint32_t)reg;
    assert((ureg >= 0) && (ureg <= 31));
    return ureg << 10;
}

// Returns an encoding for the specified register used in the 'Vd' position
static uint32_t insEncodeReg_Vd(RegNum reg)
{
    assert(isVectorRegister(reg));
    uint32_t ureg = (uint32_t)reg - (uint32_t)REG_V0;
    assert((ureg >= 0) && (ureg <= 31));
    return ureg;
}

// Returns an encoding for the specified register used in the 'Vt' position
static uint32_t insEncodeReg_Vt(RegNum reg)
{
    assert(isVectorRegister(reg));
    uint32_t ureg = (uint32_t)reg - (uint32_t)REG_V0;
    assert((ureg >= 0) && (ureg <= 31));
    return ureg;
}

// Returns an encoding for the specified register used in the 'Vn' position
static uint32_t insEncodeReg_Vn(RegNum reg)
{
    assert(isVectorRegister(reg));
    uint32_t ureg = (uint32_t)reg - (uint32_t)REG_V0;
    assert((ureg >= 0) && (ureg <= 31));
    return ureg << 5;
}

// Returns an encoding for the specified register used in the 'Vm' position
static uint32_t insEncodeReg_Vm(RegNum reg)
{
    assert(isVectorRegister(reg));
    uint32_t ureg = (uint32_t)reg - (uint32_t)REG_V0;
    assert((ureg >= 0) && (ureg <= 31));
    return ureg << 16;
}

// Returns an encoding for the specified register used in the 'Va' position
static uint32_t insEncodeReg_Va(RegNum reg)
{
    assert(isVectorRegister(reg));
    uint32_t ureg = (uint32_t)reg - (uint32_t)REG_V0;
    assert((ureg >= 0) && (ureg <= 31));
    return ureg << 10;
}

// Returns an encoding for the specified condition code.
static uint32_t insEncodeCond(insCond cond)
{
    uint32_t uimm = (uint32_t)cond;
    return uimm << 12;
}

// Returns an encoding for the condition code with the lowest bit inverted (marked by invert(<cond>) in the
// architecture manual).
static uint32_t insEncodeInvertedCond(insCond cond)
{
    uint32_t uimm = (uint32_t)cond;
    uimm ^= 1; // invert the lowest bit
    return uimm << 12;
}

// Returns an encoding for the specified flags.
static uint32_t insEncodeFlags(insCflags flags)
{
    return (uint32_t)flags;
}

// Returns the encoding for the Shift Count bits to be used for Arm64 encodings
static uint32_t insEncodeShiftCount(int64_t imm, emitAttr size)
{
    assert((imm & 0x003F) == imm);
    assert(((imm & 0x0020) == 0) || (size == EA_8BYTE));

    return (uint32_t)imm << 10;
}

// Returns the encoding to select a 64-bit datasize for an Arm64 instruction
static uint32_t insEncodeDatasize(emitAttr size)
{
    if (size == EA_8BYTE)
    {
        return 0x80000000; // set the bit at location 31
    }
    else
    {
        assert(size == EA_4BYTE);
        return 0;
    }
}

// Returns the encoding to select the datasize for the general load/store Arm64 instructions
static uint32_t insEncodeDatasizeLS(uint32_t code, emitAttr size)
{
    bool exclusive = ((code & 0x35000000) == 0);
    bool atomic    = ((code & 0x31200C00) == 0x30200000);

    if ((code & 0x00800000) && !exclusive && !atomic) // Is this a sign-extending opcode? (i.e. ldrsw, ldrsh, ldrsb)
    {
        if ((code & 0x80000000) == 0) // Is it a ldrsh or ldrsb and not ldrsw ?
        {
            if (EA_SIZE(size) != EA_8BYTE) // Do we need to encode the 32-bit Rt size bit?
            {
                return 0x00400000; // set the bit at location 22
            }
        }
    }
    else if (code & 0x80000000) // Is this a ldr/str/ldur/stur opcode?
    {
        if (EA_SIZE(size) == EA_8BYTE) // Do we need to encode the 64-bit size bit?
        {
            return 0x40000000; // set the bit at location 30
        }
    }
    return 0;
}

// Returns the encoding to select the datasize for the vector load/store Arm64 instructions
static uint32_t insEncodeDatasizeVLS(uint32_t code, emitAttr size)
{
    uint32_t result = 0;

    // Check bit 29
    if ((code & 0x20000000) == 0)
    {
        // LDR literal

        if (size == EA_16BYTE)
        {
            // set the operation size in bit 31
            result = 0x80000000;
        }
        else if (size == EA_8BYTE)
        {
            // set the operation size in bit 30
            result = 0x40000000;
        }
        else
        {
            assert(size == EA_4BYTE);
            // no bits are set
            result = 0x00000000;
        }
    }
    else
    {
        // LDR non-literal

        if (size == EA_16BYTE)
        {
            // The operation size in bits 31 and 30 are zero
            // Bit 23 specifies a 128-bit Load/Store
            result = 0x00800000;
        }
        else if (size == EA_8BYTE)
        {
            // set the operation size in bits 31 and 30
            result = 0xC0000000;
        }
        else if (size == EA_4BYTE)
        {
            // set the operation size in bit 31
            result = 0x80000000;
        }
        else if (size == EA_2BYTE)
        {
            // set the operation size in bit 30
            result = 0x40000000;
        }
        else
        {
            assert(size == EA_1BYTE);
            // The operation size in bits 31 and 30 are zero
            result = 0x00000000;
        }
    }

    // Or in bit 26 to indicate a Vector register is used as 'target'
    result |= 0x04000000;

    return result;
}

// Returns the encoding to select the datasize for the vector load/store Arm64 instructions
static uint32_t insEncodeDatasizeVPLS(uint32_t code, emitAttr size)
{
    uint32_t result = 0;

    if (size == EA_16BYTE)
    {
        // The operation size in bits 31 and 30 are zero
        // Bit 23 specifies a 128-bit Load/Store
        result = 0x80000000;
    }
    else if (size == EA_8BYTE)
    {
        // set the operation size in bits 31 and 30
        result = 0x40000000;
    }
    else if (size == EA_4BYTE)
    {
        // set the operation size in bit 31
        result = 0x00000000;
    }

    // Or in bit 26 to indicate a Vector register is used as 'target'
    result |= 0x04000000;

    return result;
}

// Returns the encoding to set the size bit and the N bits for a 'bitfield' instruction
static uint32_t insEncodeDatasizeBF(uint32_t code, emitAttr size)
{
    // is bit 30 equal to 0?
    if ((code & 0x40000000) == 0) // is the opcode one of extr, sxtb, sxth or sxtw
    {
        if (size == EA_8BYTE) // Do we need to set the sf and N bits?
        {
            return 0x80400000; // set the sf-bit at location 31 and the N-bit at location 22
        }
    }
    return 0; // don't set any bits
}

// Returns the encoding to select the 64/128-bit datasize for an Arm64 vector instruction
static uint32_t insEncodeVectorsize(emitAttr size)
{
    if (size == EA_16BYTE)
    {
        return 0x40000000; // set the bit at location 30
    }
    else
    {
        assert(size == EA_8BYTE);
        return 0;
    }
}

// Returns the encoding to select 'index' for an Arm64 vector elem instruction
static uint32_t insEncodeVectorIndex(emitAttr elemsize, int64_t index)
{
    uint32_t bits = (uint32_t)index;
    if (elemsize == EA_1BYTE)
    {
        bits <<= 1;
        bits |= 1;
    }
    else if (elemsize == EA_2BYTE)
    {
        bits <<= 2;
        bits |= 2;
    }
    else if (elemsize == EA_4BYTE)
    {
        bits <<= 3;
        bits |= 4;
    }
    else
    {
        assert(elemsize == EA_8BYTE);
        bits <<= 4;
        bits |= 8;
    }
    assert((bits >= 1) && (bits <= 0x1f));

    return (bits << 16); // bits at locations [20,19,18,17,16]
}

// Returns the encoding to select 'index2' for an Arm64 'ins' elem instruction
static uint32_t insEncodeVectorIndex2(emitAttr elemsize, int64_t index2)
{
    uint32_t bits = (uint32_t)index2;
    if (elemsize == EA_1BYTE)
    {
        // bits are correct
    }
    else if (elemsize == EA_2BYTE)
    {
        bits <<= 1;
    }
    else if (elemsize == EA_4BYTE)
    {
        bits <<= 2;
    }
    else
    {
        assert(elemsize == EA_8BYTE);
        bits <<= 3;
    }
    assert((bits >= 0) && (bits <= 0xf));

    return (bits << 11); // bits at locations [14,13,12,11]
}

// Returns the encoding to select the 'index' for an Arm64 'mul' by element instruction
static uint32_t insEncodeVectorIndexLMH(emitAttr elemsize, int64_t index)
{
    uint32_t bits = 0;

    if (elemsize == EA_2BYTE)
    {
        assert((index >= 0) && (index <= 7));
        if (index & 0x4)
        {
            bits |= (1 << 11); // set bit 11 'H'
        }
        if (index & 0x2)
        {
            bits |= (1 << 21); // set bit 21 'L'
        }
        if (index & 0x1)
        {
            bits |= (1 << 20); // set bit 20 'M'
        }
    }
    else if (elemsize == EA_4BYTE)
    {
        assert((index >= 0) && (index <= 3));
        if (index & 0x2)
        {
            bits |= (1 << 11); // set bit 11 'H'
        }
        if (index & 0x1)
        {
            bits |= (1 << 21); // set bit 21 'L'
        }
    }
    else
    {
        assert(!"Invalid 'elemsize' value");
    }

    return bits;
}

// Returns the encoding for the SIMD shift (immediate) instructions, the "immh:immb"
// field of the instruction that contains encoded shift amount.
static uint32_t insEncodeVectorShift(emitAttr size, int64_t shiftAmount)
{
    if (shiftAmount < 0)
    {
        shiftAmount = -shiftAmount;
        // The right shift amount must be in the range 1 to the destination element width in bits.
        assert((shiftAmount > 0) && (shiftAmount <= getBitWidth(size)));

        uint32_t imm = (uint32_t)(2 * getBitWidth(size) - shiftAmount);
        return imm << 16;
    }
    else
    {
        // The left shift amount must in the range 0 to the element width in bits minus 1.
        assert(shiftAmount < getBitWidth(size));
        uint32_t imm = (uint32_t)(getBitWidth(size) + shiftAmount);
        return imm << 16;
    }
}

// Returns the encoding to select the 1/2/4/8 byte elemsize for an Arm64 vector instruction
static uint32_t insEncodeElemsize(emitAttr size)
{
    if (size == EA_8BYTE)
    {
        return 0x00C00000; // set the bit at location 23 and 22
    }
    else if (size == EA_4BYTE)
    {
        return 0x00800000; // set the bit at location 23
    }
    else if (size == EA_2BYTE)
    {
        return 0x00400000; // set the bit at location 22
    }
    assert(size == EA_1BYTE);
    return 0x00000000;
}

// Returns the encoding to select the 4/8 byte elemsize for an Arm64 float vector instruction
static uint32_t insEncodeFloatElemsize(emitAttr size)
{
    if (size == EA_8BYTE)
    {
        return 0x00400000; // set the bit at location 22
    }
    assert(size == EA_4BYTE);
    return 0x00000000;
}

// Returns the encoding to select the index for an Arm64 float vector by element instruction
static uint32_t insEncodeFloatIndex(emitAttr elemsize, int64_t index)
{
    uint32_t result = 0x00000000;
    if (elemsize == EA_8BYTE)
    {
        assert((index >= 0) && (index <= 1));
        if (index == 1)
        {
            result |= 0x00000800; // 'H' - set the bit at location 11
        }
    }
    else
    {
        assert(elemsize == EA_4BYTE);
        assert((index >= 0) && (index <= 3));
        if (index & 2)
        {
            result |= 0x00000800; // 'H' - set the bit at location 11
        }
        if (index & 1)
        {
            result |= 0x00200000; // 'L' - set the bit at location 21
        }
    }
    return result;
}

// Returns the encoding to select the vector elemsize for an Arm64 ld/st# vector instruction
static uint32_t insEncodeVLSElemsize(emitAttr size)
{
    uint32_t result = 0x00000000;

    switch (size)
    {
        case EA_1BYTE:
            result |= 0x0000; // clear bits 10 and 11
            break;
        case EA_2BYTE:
            result |= 0x0400; // set bit at location 10, clear bit at location 11
            break;
        case EA_4BYTE:
            result |= 0x0800; // clear bit at location 10, set bit at location 11
            break;
        case EA_8BYTE:
            result |= 0x0C00; // set bits at location 10 and 11
            break;
        default:
            assert(!"Invalid element size");
            break;
    }

    return result;
}

// Returns the encoding to select the index for an Arm64 ld/st# vector by element instruction
static uint32_t insEncodeVLSIndex(emitAttr size, int64_t index)
{
    uint32_t result = 0x00000000;

    switch (size)
    {
        case EA_1BYTE:
            // Q  = ?   - bit location 30
            // xx = 00  - bit location 14 and 15
            // S = ?    - bit location 12
            // ss = ?0  - bit location 10 and 11
            result |= (index & 0x8) << 27;
            result |= (index & 0x4) << 10;
            result |= (index & 0x3) << 10;
            break;
        case EA_2BYTE:
            // Q  = ?   - bit location 30
            // xx = 01  - bit location 14 and 15
            // S = ?    - bit location 12
            // ss = ??  - bit location 10 and 11
            result |= (index & 0x4) << 28;
            result |= 0x4000;
            result |= (index & 0x2) << 11;
            result |= (index & 0x1) << 11;
            break;
        case EA_4BYTE:
            // Q  = ?   - bit location 30
            // xx = 10  - bit location 14 and 15
            // S = ?    - bit location 12
            // ss = 00  - bit location 10 and 11
            result |= (index & 0x2) << 29;
            result |= 0x8000;
            result |= (index & 0x1) << 12;
            break;
        case EA_8BYTE:
            // Q  = ?   - bit location 30
            // xx = 10  - bit location 14 and 15
            // S = 0    - bit location 12
            // ss = 01  - bit location 10 and 11
            result |= (index & 0x1) << 30;
            result |= 0x8400;
            break;
        default:
            assert(!"Invalid element size");
            break;
    }

    return result;
}

// Returns the encoding to select the fcvt operation for Arm64 instructions
static uint32_t insEncodeConvertOpt(insFormat fmt, insOpts conversion)
{
    switch (conversion)
    {
        case INS_OPTS_S_TO_D: // Single to Double
            assert(fmt == IF_DV_2J);
            return 0x00008000; // type=00, opc=01
        case INS_OPTS_D_TO_S:  // Double to Single
            assert(fmt == IF_DV_2J);
            return 0x00400000; // type=01, opc=00
        case INS_OPTS_H_TO_S:  // Half to Single
            assert(fmt == IF_DV_2J);
            return 0x00C00000; // type=11, opc=00
        case INS_OPTS_H_TO_D:  // Half to Double
            assert(fmt == IF_DV_2J);
            return 0x00C08000; // type=11, opc=01
        case INS_OPTS_S_TO_H:  // Single to Half
            assert(fmt == IF_DV_2J);
            return 0x00018000; // type=00, opc=11
        case INS_OPTS_D_TO_H:  // Double to Half
            assert(fmt == IF_DV_2J);
            return 0x00418000;    // type=01, opc=11
        case INS_OPTS_S_TO_4BYTE: // Single to int32_t
            assert(fmt == IF_DV_2H);
            return 0x00000000;    // sf=0, type=00
        case INS_OPTS_D_TO_4BYTE: // Double to int32_t
            assert(fmt == IF_DV_2H);
            return 0x00400000;    // sf=0, type=01
        case INS_OPTS_S_TO_8BYTE: // Single to int64_t
            assert(fmt == IF_DV_2H);
            return 0x80000000;    // sf=1, type=00
        case INS_OPTS_D_TO_8BYTE: // Double to int64_t
            assert(fmt == IF_DV_2H);
            return 0x80400000;    // sf=1, type=01
        case INS_OPTS_4BYTE_TO_S: // int32_t to Single
            assert(fmt == IF_DV_2I);
            return 0x00000000;    // sf=0, type=00
        case INS_OPTS_4BYTE_TO_D: // int32_t to Double
            assert(fmt == IF_DV_2I);
            return 0x00400000;    // sf=0, type=01
        case INS_OPTS_8BYTE_TO_S: // int64_t to Single
            assert(fmt == IF_DV_2I);
            return 0x80000000;    // sf=1, type=00
        case INS_OPTS_8BYTE_TO_D: // int64_t to Double
            assert(fmt == IF_DV_2I);
            return 0x80400000; // sf=1, type=01
        default:
            assert(!"Invalid 'conversion' value");
            return 0;
    }
}

// Returns the encoding to have the Rn register be updated Pre/Post indexed or not updated
static uint32_t insEncodeIndexedOpt(insOpts opt)
{
    assert(insOptsNone(opt) || insOptsIndexed(opt));

    if (insOptsIndexed(opt))
    {
        if (insOptsPostIndex(opt))
        {
            return 0x00000400; // set the bit at location 10
        }
        else
        {
            assert(insOptsPreIndex(opt));
            return 0x00000C00; // set the bit at location 10 and 11
        }
    }
    else
    {
        assert(insOptsNone(opt));
        return 0; // bits 10 and 11 are zero
    }
}

// Returns the encoding for a ldp/stp instruction to have the Rn register be updated Pre/Post indexed or not updated
static uint32_t insEncodePairIndexedOpt(instruction ins, insOpts opt)
{
    if ((ins == INS_ldnp) || (ins == INS_stnp))
    {
        assert(insOptsNone(opt));

        return 0;
    }

    if (insOptsIndexed(opt))
    {
        return insOptsPostIndex(opt) ? 0x00800000 : 0x01800000;
    }

    assert(insOptsNone(opt));
    return 0x01000000;
}

// Returns the encoding to apply a Shift Type on the Rm register
static uint32_t insEncodeShiftType(insOpts opt)
{
    if (insOptsNone(opt))
    {
        // None implies the we encode LSL (with a zero immediate)
        opt = INS_OPTS_LSL;
    }

    assert(insOptsAnyShift(opt));

    uint32_t option = static_cast<uint32_t>(opt) - static_cast<uint32_t>(INS_OPTS_LSL);
    assert(option <= 3);

    return option << 22;
}

// Returns the encoding to apply a 12 bit left shift to the immediate
static uint32_t insEncodeShiftImm12(insOpts opt)
{
    return insOptsLSL12(opt) ? 0x00400000 : 0;
}

// Returns the encoding to have the Rm register use an extend operation
static uint32_t insEncodeExtend(insOpts opt)
{
    if (insOptsNone(opt) || (opt == INS_OPTS_LSL))
    {
        // None or LSL implies the we encode UXTX
        opt = INS_OPTS_UXTX;
    }

    assert(insOptsAnyExtend(opt));

    uint32_t option = static_cast<uint32_t>(opt) - static_cast<uint32_t>(INS_OPTS_UXTB);
    assert(option <= 7);

    return option << 13; // bits 15,14,13
}

// Returns the encoding to scale the Rm register by {0,1,2,3,4} when using an extend operation
static uint32_t insEncodeExtendScale(int64_t imm)
{
    assert((imm >= 0) && (imm <= 4));

    return static_cast<uint32_t>(imm) << 10; // bits 12,11,10
}

// Returns the encoding to have the Rm register be auto scaled by the ld/st size
static uint32_t insEncodeReg3Scale(bool isScaled)
{
    return isScaled ? 0x00001000 : 0; // set the bit at location 12
}

static int64_t ComputeRelPageAddr(void* dstAddr, void* srcAddr)
{
    return (reinterpret_cast<int64_t>(dstAddr) >> 12) - (reinterpret_cast<int64_t>(srcAddr) >> 12);
}

uint8_t* Arm64Encoder::emitOutputLoadLabel(uint8_t* dst, uint8_t* instrAddr, uint8_t* labelAddr, instrDescJmp* id)
{
    instruction ins    = id->idIns();
    insFormat   fmt    = id->idInsFmt();
    RegNum      dstReg = id->idReg1();

    if (fmt == IF_SMALLADR)
    {
        assert(ins == INS_adr);

        return emitOutputShortAddress(dst, INS_adr, labelAddr - instrAddr, dstReg);
    }

    assert(ins == INS_adrp);
    assert(fmt == IF_LARGEADR);

    int64_t relPageAddr = ComputeRelPageAddr(labelAddr, instrAddr);

    dst = emitOutputShortAddress(dst, INS_adrp, relPageAddr, dstReg);

    uint32_t imm12 = static_cast<uint32_t>(reinterpret_cast<uint64_t>(labelAddr) & 0xFFF);

    uint32_t code = emitInsCode(INS_add, IF_DI_2A);
    code |= insEncodeDatasize(EA_8BYTE);
    code |= imm12 << 10;
    code |= insEncodeReg_Rd(dstReg);
    code |= insEncodeReg_Rn(dstReg);

    return dst + emitOutput_Instr(dst, code);
}

uint8_t* Arm64Encoder::emitOutputDL(uint8_t* dst, instrDescJmp* id)
{
    instruction ins = id->idIns();
    insFormat   fmt = id->idInsFmt();

    assert((ins == INS_ldr) || (ins == INS_ldrsw) || (ins == INS_adr) || (ins == INS_adrp));

    if (id->idIsCnsReloc())
    {
        NYI_ARM64("Relocation support for hot/cold jumps");
    }

    uint32_t instrOffs = emitCurCodeOffs(dst);
    uint8_t* instrAddr = emitOffsetToPtr(instrOffs);

    uint32_t dataOffset = id->GetConstData()->offset;

    int64_t imm = id->emitGetInsSC();
    assert((imm >= 0) && (imm < 0x1000)); // 0x1000 is arbitrary, currently 'imm' is always 0

    uint32_t dataOffs = static_cast<uint32_t>(dataOffset + imm);
    assert(dataOffs < roData.size);

    uint8_t* dataAddr = emitDataOffsetToPtr(dataOffs);

    if ((ins == INS_adr) || (ins == INS_adrp))
    {
        return emitOutputLoadLabel(dst, instrAddr, dataAddr, id);
    }

    emitAttr opSize = id->idOpSize();
    RegNum   dstReg = id->idReg1();

    if (fmt == IF_LS_1A)
    {
        // ldr x/v, [rel addr] -- load constant from current addr(ip) + rel addr.
        assert(ins == INS_ldr);

        return emitOutputShortConstant(dst, ins, fmt, dataAddr - instrAddr, dstReg, opSize);
    }

    // adrp x, [rel page addr] -- compute page address: current page addr + rel page addr
    assert(fmt == IF_LARGELDC);

    RegNum addrReg = IsVectorRegister(dstReg) ? id->idReg2() : dstReg;

    dst = emitOutputShortAddress(dst, INS_adrp, ComputeRelPageAddr(dataAddr, instrAddr), addrReg);

    // TODO-MIKE-Fix: SIMD16 constants aren't 16 byte aligned?!?!
    noway_assert((reinterpret_cast<uint64_t>(dataAddr) & 0xFFF) % EA_SIZE_IN_BYTES(opSize) == 0);
    uint32_t imm12 = (reinterpret_cast<uint64_t>(dataAddr) & 0xFFF) / EA_SIZE_IN_BYTES(opSize);
    uint32_t code  = emitInsCode(INS_ldr, IF_LS_2B);

    if (isVectorRegister(dstReg))
    {
        code &= 0x3FFFFFFF;
        code |= insEncodeDatasizeVLS(code, opSize);
        code |= insEncodeReg_Vt(dstReg);
    }
    else
    {
        code |= insEncodeDatasizeLS(code, opSize);
        code |= insEncodeReg_Rt(dstReg);
    }

    code |= imm12 << 10;
    code |= insEncodeReg_Rn(addrReg);

    return dst + emitOutput_Instr(dst, code);
}

uint8_t* Arm64Encoder::emitOutputLJ(uint8_t* dst, instrDescJmp* id, insGroup* ig)
{
    assert(!id->HasConstData());
    assert(id->idInsOpt() == INS_OPTS_NONE);
    assert(id->idGCref() == GCT_NONE);

    if (id->idIsCnsReloc())
    {
        NYI_ARM64("Relocation support for hot/cold jumps");
    }

    uint32_t labelOffs;

    if (id->HasInstrCount())
    {
        assert(ig != nullptr);

        int      instrCount   = id->GetInstrCount();
        unsigned jumpInstrNum = ig->FindInsNum(id);

        assert((instrCount >= 0) || (jumpInstrNum + 1 >= static_cast<unsigned>(-instrCount)));

        labelOffs = ig->igOffs + ig->FindInsOffset(jumpInstrNum + 1 + instrCount);
    }
    else
    {
        labelOffs = id->GetLabel()->igOffs;
    }

    uint32_t instrOffs = emitCurCodeOffs(dst);
    uint8_t* instrAddr = emitOffsetToPtr(instrOffs);
    uint8_t* labelAddr = emitOffsetToPtr(labelOffs);
    int64_t  distance  = labelAddr - instrAddr;

    instruction ins = id->idIns();
    insFormat   fmt = id->idInsFmt();

    if ((ins == INS_adr) || (ins == INS_adrp))
    {
        return emitOutputLoadLabel(dst, instrAddr, labelAddr, id);
    }

#ifdef DEBUG
    if (id->HasInstrCount())
    {
        assert(FitsIn<int8_t>(distance));
        assert((ins != INS_tbz) || (ins != INS_tbnz));
        // Store the jump distance into the (unused) imm field of the instruction,
        // so the printer doesn't need to recompute it.
        // TODO-MIKE-Cleanup: It may be nice to have a distinct instruction field
        // for this, to avoid conflicts with tbz/tbnz. But adding a new field to
        // instrDescJmp increases its size, which then causes some diffs due to
        // changes of number of instructions that fit in a group...
        id->idSmallCns(distance + -INT8_MIN);
    }
#endif

    if (fmt == IF_LARGEJMP)
    {
        // This is a pseudo-instruction format representing a large conditional branch, to allow
        // us to get a greater branch target range than we can get by using a straightforward conditional
        // branch. It is encoded as a short conditional branch that branches around a long unconditional
        // branch.

        instruction reverseIns;
        insFormat   reverseFmt;

        switch (ins)
        {
            case INS_cbz:
                reverseIns = INS_cbnz;
                reverseFmt = IF_BI_1A;
                break;
            case INS_cbnz:
                reverseIns = INS_cbz;
                reverseFmt = IF_BI_1A;
                break;
            case INS_tbz:
                reverseIns = INS_tbnz;
                reverseFmt = IF_BI_1B;
                break;
            case INS_tbnz:
                reverseIns = INS_tbz;
                reverseFmt = IF_BI_1B;
                break;
            default:
                reverseIns = JumpKindToJcc(ReverseJumpKind(JccToJumpKind(ins)));
                reverseFmt = IF_BI_0B;
                break;
        }

        dst = emitOutputShortBranch(dst, reverseIns, reverseFmt, 8, id);

        // Now, pretend we've got a normal unconditional branch, and fall through to the code to emit that.
        ins = INS_b;
        fmt = IF_BI_0A;

        // The distance was computed based on the beginning of the pseudo-instruction,
        // So subtract the size of the conditional branch so that it is relative to the
        // unconditional branch.
        distance -= 4;
    }

    return emitOutputShortBranch(dst, ins, fmt, distance, id);
}

uint8_t* Arm64Encoder::emitOutputShortBranch(
    uint8_t* dst, instruction ins, insFormat fmt, int64_t distance, instrDescJmp* id)
{
    noway_assert((distance & 3) == 0);
    distance >>= 2;

    uint32_t code = emitInsCode(ins, fmt);

    if (fmt == IF_BI_0A)
    {
        // INS_b or INS_bl_local
        noway_assert(isValidSimm26(distance));

        code |= distance & 0x3FFFFFF;
    }
    else if (fmt == IF_BI_0B) // 01010100iiiiiiii iiiiiiiiiiiXXXXX      simm19:00
    {
        // INS_beq, INS_bne, etc...
        noway_assert(isValidSimm19(distance));

        code |= (distance & 0x7FFFF) << 5;
    }
    else if (fmt == IF_BI_1A) // X.......iiiiiiii iiiiiiiiiiittttt      Rt simm19:00
    {
        // INS_cbz or INS_cbnz
        assert(id != nullptr);
        noway_assert(isValidSimm19(distance));

        code |= insEncodeDatasize(id->idOpSize());
        code |= insEncodeReg_Rt(id->idReg1());
        code |= (distance & 0x7FFFF) << 5;
    }
    else
    {
        // INS_tbz or INS_tbnz
        assert(fmt == IF_BI_1B); // B.......bbbbbiii iiiiiiiiiiittttt      Rt imm6, simm14:00
        assert(id != nullptr);

        int64_t imm = id->emitGetInsSC();
        assert(isValidImmShift(imm, id->idOpSize()));
        noway_assert(isValidSimm14(distance));

        if (imm & 0x20)
        {
            code |= 0x80000000; // B
        }

        code |= ((imm & 0x1F) << 19);          // bbbbb
        code |= insEncodeReg_Rt(id->idReg1()); // ttttt
        code |= (distance & 0x3FFF) << 5;
    }

    return dst + emitOutput_Instr(dst, code);
}

uint8_t* Arm64Encoder::emitOutputShortAddress(uint8_t* dst, instruction ins, int64_t distance, RegNum reg)
{
    assert((ins == INS_adr) || (ins == INS_adrp));
    assert(IsGeneralRegister(reg));

    uint32_t code = emitInsCode(ins, IF_DI_1E);
    code |= insEncodeReg_Rd(reg);
    int64_t loBits = (distance & 3);
    distance >>= 2;
    noway_assert(isValidSimm19(distance));
    code |= (distance & 0x7FFFFLL) << 5;
    code |= loBits << 29;

    return dst + emitOutput_Instr(dst, code);
}

uint8_t* Arm64Encoder::emitOutputShortConstant(
    uint8_t* dst, instruction ins, insFormat fmt, int64_t imm, RegNum reg, emitAttr opSize)
{
    assert(ins == INS_ldr);
    assert(fmt == IF_LS_1A);

    uint32_t code = emitInsCode(INS_ldr, IF_LS_1A);

    int64_t loBits = (imm & 3);
    noway_assert(loBits == 0);
    int64_t distance = imm >>= 2;

    noway_assert(isValidSimm19(distance));

    if (isVectorRegister(reg))
    {
        code |= insEncodeDatasizeVLS(code, opSize);
        code |= insEncodeReg_Vt(reg);
    }
    else
    {
        assert(isGeneralRegister(reg));

        if ((ins == INS_ldr) && (opSize == EA_8BYTE))
        {
            code |= 0x40000000;
        }

        code |= insEncodeReg_Rt(reg);
    }

    distance &= 0x7FFFFLL;
    code |= distance << 5;

    return dst + emitOutput_Instr(dst, code);
}

unsigned Arm64Encoder::emitOutput_Instr(uint8_t* dst, uint32_t code)
{
    static_assert_no_msg(sizeof(uint32_t) == 4);

    *reinterpret_cast<uint32_t*>(dst + writeableOffset) = code;
    return 4;
}

void EmitterBase::emitEndCodeGen(GCInfo& gcInfo)
{
    Arm64Emitter& emit = *static_cast<Arm64Emitter*>(this);
    Arm64Encoder  encoder(emit, gcInfo);
    encoder.emitEndCodeGen(emit);
}

size_t Encoder::emitOutputInstr(insGroup* ig, instrDesc* id, uint8_t** dp)
{
    static_cast<Arm64Encoder*>(this)->EncodeInstr(ig, id, dp);
    return id->GetDescSize();
}

void Arm64Encoder::EncodeInstr(insGroup* ig, instrDesc* id, uint8_t** dp)
{
    uint8_t*          dst  = *dp;
    uint8_t*          odst = dst;
    uint32_t          code = 0;
    const instruction ins  = id->idIns();
    const insFormat   fmt  = id->idInsFmt();
    const emitAttr    size = id->idOpSize();

    switch (fmt)
    {
        int64_t  imm;
        int64_t  index;
        int64_t  index2;
        unsigned cmode;
        unsigned immShift;
        emitAttr elemsize;
        emitAttr datasize;
        CondImm  cimm;

        case IF_LS_1A: // XX...V..iiiiiiii iiiiiiiiiiittttt      Rt    PC imm(1MB)
        case IF_SMALLADR:
        case IF_LARGEADR:
        case IF_LARGELDC:
            if (static_cast<instrDescJmp*>(id)->HasConstData())
            {
                dst = emitOutputDL(dst, static_cast<instrDescJmp*>(id));
                break;
            }
            FALLTHROUGH;
        case IF_BI_0A: // ......iiiiiiiiii iiiiiiiiiiiiiiii               simm26:00
        case IF_BI_0B: // ......iiiiiiiiii iiiiiiiiiii.....               simm19:00
        case IF_BI_1A: // ......iiiiiiiiii iiiiiiiiiiittttt      Rt       simm19:00
        case IF_BI_1B: // B.......bbbbbiii iiiiiiiiiiittttt      Rt imm6, simm14:00
        case IF_LARGEJMP:
            dst = emitOutputLJ(dst, static_cast<instrDescJmp*>(id), ig);
            break;

        case IF_NOP_JMP:
            break;

        case IF_DI_1E: // .ii.....iiiiiiii iiiiiiiiiiiddddd      Rd       simm21
            assert(id->idIsCnsReloc());
            code = emitInsCode(ins, fmt);
            code |= insEncodeReg_Rd(id->idReg1()); // ddddd
            dst += emitOutput_Instr(dst, code);
            emitRecordRelocation(odst, id->GetAddr(), IMAGE_REL_ARM64_PAGEBASE_REL21);
            break;

        case IF_BI_0C: // ......iiiiiiiiii iiiiiiiiiiiiiiii               simm26:00
            emitRecordGCCall(id, dst, dst + 4);
            code = emitInsCode(ins, fmt);
            dst += emitOutput_Instr(dst, code);
            // Always call RecordRelocation so that we wire in a JumpStub when we don't reach
            emitRecordRelocation(odst, id->GetAddr(), IMAGE_REL_ARM64_BRANCH26);
            break;

        case IF_BR_1A: // ................ ......nnnnn.....         Rn
            assert(insOptsNone(id->idInsOpt()));
            assert((ins == INS_ret) || (ins == INS_br));
            code = emitInsCode(ins, fmt);
            code |= insEncodeReg_Rn(id->idReg1()); // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_BR_1B: // ................ ......nnnnn.....         Rn
            assert(insOptsNone(id->idInsOpt()));
            assert((ins == INS_br_tail) || (ins == INS_blr));
            emitRecordGCCall(id, dst, dst + 4);
            code = emitInsCode(ins, fmt);
            code |= insEncodeReg_Rn(id->idReg3()); // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_LS_2A: // .X.......X...... ......nnnnnttttt      Rt Rn
            assert(insOptsNone(id->idInsOpt()));
            code = emitInsCode(ins, fmt);
            if (isVectorRegister(id->idReg1()))
            {
                code &= 0x3FFFFFFF;
                code |= insEncodeDatasizeVLS(code, id->idOpSize()); // XX
                code |= insEncodeReg_Vt(id->idReg1());              // ttttt
            }
            else
            {
                code |= insEncodeDatasizeLS(code, id->idOpSize()); // .X.......X
                code |= insEncodeReg_Rt(id->idReg1());             // ttttt
            }
            code |= insEncodeReg_Rn(id->idReg2()); // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_LS_2B: // .X.......Xiiiiii iiiiiinnnnnttttt      Rt Rn    imm(0-4095)
            assert(insOptsNone(id->idInsOpt()));
            imm = id->emitGetInsSC();
            assert(isValidUimm12(imm));
            code = emitInsCode(ins, fmt);
            if (isVectorRegister(id->idReg1()))
            {
                code &= 0x3FFFFFFF;
                code |= insEncodeDatasizeVLS(code, id->idOpSize()); // XX
                code |= insEncodeReg_Vt(id->idReg1());              // ttttt
            }
            else
            {
                code |= insEncodeDatasizeLS(code, id->idOpSize()); // .X.......X
                code |= insEncodeReg_Rt(id->idReg1());             // ttttt
            }
            code |= static_cast<uint32_t>(imm) << 10; // iiiiiiiiiiii
            code |= insEncodeReg_Rn(id->idReg2());    // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_LS_2C: // .X.......X.iiiii iiiiPPnnnnnttttt      Rt Rn    imm(-256..+255) no/pre/post inc
            assert(insOptsNone(id->idInsOpt()) || insOptsIndexed(id->idInsOpt()));
            imm = id->emitGetInsSC();
            assert((imm >= -256) && (imm <= 255));
            code = emitInsCode(ins, fmt);
            if (isVectorRegister(id->idReg1()))
            {
                code &= 0x3FFFFFFF;                                 // clear the size bits
                code |= insEncodeDatasizeVLS(code, id->idOpSize()); // XX
                code |= insEncodeReg_Vt(id->idReg1());              // ttttt
            }
            else
            {
                code |= insEncodeDatasizeLS(code, id->idOpSize()); // .X.......X
                code |= insEncodeReg_Rt(id->idReg1());             // ttttt
            }
            code |= insEncodeIndexedOpt(id->idInsOpt());      // PP
            code |= static_cast<uint32_t>(imm & 0x1ff) << 12; // iiiiiiiii
            code |= insEncodeReg_Rn(id->idReg2());            // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_LS_2D: // .Q.............. ....ssnnnnnttttt      Vt Rn
        case IF_LS_2E: // .Q.............. ....ssnnnnnttttt      Vt Rn
            elemsize = optGetElemsize(id->idInsOpt());
            code     = emitInsCode(ins, fmt);
            code |= insEncodeVectorsize(id->idOpSize()); // Q
            code |= insEncodeVLSElemsize(elemsize);      // ss
            code |= insEncodeReg_Rn(id->idReg2());       // nnnnn
            code |= insEncodeReg_Vt(id->idReg1());       // ttttt
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_LS_2F: // .Q.............. xx.Sssnnnnnttttt      Vt[] Rn
        case IF_LS_2G: // .Q.............. xx.Sssnnnnnttttt      Vt[] Rn
            elemsize = id->idOpSize();
            index    = id->idSmallCns();
            code     = emitInsCode(ins, fmt);
            code |= insEncodeVLSIndex(elemsize, index); // Q xx S ss
            code |= insEncodeReg_Rn(id->idReg2());      // nnnnn
            code |= insEncodeReg_Vt(id->idReg1());      // ttttt
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_LS_3A: // .X.......X.mmmmm oooS..nnnnnttttt      Rt Rn Rm ext(Rm) LSL {}
            assert(insOptsLSExtend(id->idInsOpt()));
            code = emitInsCode(ins, fmt);
            if (isVectorRegister(id->idReg1()))
            {
                code &= 0x3FFFFFFF;                                 // clear the size bits
                code |= insEncodeDatasizeVLS(code, id->idOpSize()); // XX
                code |= insEncodeReg_Vt(id->idReg1());              // ttttt
            }
            else
            {
                code |= insEncodeDatasizeLS(code, id->idOpSize()); // .X.......X
                code |= insEncodeReg_Rt(id->idReg1());             // ttttt
            }
            code |= insEncodeExtend(id->idInsOpt());        // ooo
            code |= insEncodeReg_Rn(id->idReg2());          // nnnnn
            code |= insEncodeReg3Scale(id->idReg3Scaled()); // S
            code |= insEncodeReg_Rm(id->idReg3());          // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_LS_3B: // X............... .aaaaannnnnddddd      Rd Ra Rn
            assert(insOptsNone(id->idInsOpt()));
            code = emitInsCode(ins, fmt);
            if (isVectorRegister(id->idReg1()))
            {
                code &= 0x3FFFFFFF;                                  // clear the size bits
                code |= insEncodeDatasizeVPLS(code, id->idOpSize()); // XX
                code |= insEncodeReg_Vt(id->idReg1());               // ttttt
                code |= insEncodeReg_Va(id->idReg2());               // aaaaa
            }
            else
            {
                code |= insEncodeDatasize(id->idOpSize()); // X
                code |= insEncodeReg_Rt(id->idReg1());     // ttttt
                code |= insEncodeReg_Ra(id->idReg2());     // aaaaa
            }
            code |= insEncodeReg_Rn(id->idReg3()); // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_LS_3C: // X......PP.iiiiii iaaaaannnnnddddd      Rd Ra Rn imm(im7,sh)
            assert(insOptsNone(id->idInsOpt()) || insOptsIndexed(id->idInsOpt()));
            imm = id->emitGetInsSC();
            assert((imm >= -64) && (imm <= 63));
            code = emitInsCode(ins, fmt);
            if (isVectorRegister(id->idReg1()))
            {
                code &= 0x3FFFFFFF;                                  // clear the size bits
                code |= insEncodeDatasizeVPLS(code, id->idOpSize()); // XX
                code |= insEncodeReg_Vt(id->idReg1());               // ttttt
                code |= insEncodeReg_Va(id->idReg2());               // aaaaa
            }
            else
            {
                code |= insEncodeDatasize(id->idOpSize()); // X
                code |= insEncodeReg_Rt(id->idReg1());     // ttttt
                code |= insEncodeReg_Ra(id->idReg2());     // aaaaa
            }
            code |= insEncodePairIndexedOpt(ins, id->idInsOpt()); // PP
            code |= static_cast<uint32_t>(imm & 0x7f) << 15;      // iiiiiiiii
            code |= insEncodeReg_Rn(id->idReg3());                // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_LS_3D: // .X.......X.mmmmm ......nnnnnttttt      Wm Rt Rn
            code = emitInsCode(ins, fmt);
            // Arm64 store exclusive unpredictable cases
            assert(id->idReg1() != id->idReg2());
            assert(id->idReg1() != id->idReg3());
            code |= insEncodeDatasizeLS(code, id->idOpSize()); // X
            code |= insEncodeReg_Rm(id->idReg1());             // mmmmm
            code |= insEncodeReg_Rt(id->idReg2());             // ttttt
            code |= insEncodeReg_Rn(id->idReg3());             // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_LS_3E: // .X.........mmmmm ......nnnnnttttt      Rm Rt Rn ARMv8.1 LSE Atomics
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasizeLS(code, id->idOpSize()); // X
            code |= insEncodeReg_Rm(id->idReg1());             // mmmmm
            code |= insEncodeReg_Rt(id->idReg2());             // ttttt
            code |= insEncodeReg_Rn(id->idReg3());             // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_LS_3F: // .Q.........mmmmm ....ssnnnnnttttt      Vt Rn Rm
            elemsize = optGetElemsize(id->idInsOpt());
            code     = emitInsCode(ins, fmt);
            code |= insEncodeVectorsize(id->idOpSize()); // Q
            code |= insEncodeReg_Rm(id->idReg3());       // mmmmm
            code |= insEncodeVLSElemsize(elemsize);      // ss
            code |= insEncodeReg_Rn(id->idReg2());       // nnnnn
            code |= insEncodeReg_Vt(id->idReg1());       // ttttt
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_LS_3G: // .Q.........mmmmm ...Sssnnnnnttttt      Vt[] Rn Rm
            elemsize = id->idOpSize();
            index    = id->idSmallCns();
            code     = emitInsCode(ins, fmt);
            code |= insEncodeVLSIndex(elemsize, index); // Q xx S ss
            code |= insEncodeReg_Rm(id->idReg3());      // mmmmm
            code |= insEncodeReg_Rn(id->idReg2());      // nnnnn
            code |= insEncodeReg_Vt(id->idReg1());      // ttttt
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DI_1A: // X.......shiiiiii iiiiiinnnnn.....         Rn    imm(i12,sh)
            assert(insOptsNone(id->idInsOpt()) || insOptsLSL12(id->idInsOpt()));
            imm = id->emitGetInsSC();
            assert(isValidUimm12(imm));
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasize(id->idOpSize());   // X
            code |= insEncodeShiftImm12(id->idInsOpt()); // sh
            code |= static_cast<uint32_t>(imm) << 10;    // iiiiiiiiiiii
            code |= insEncodeReg_Rn(id->idReg1());       // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DI_1B: // X........hwiiiii iiiiiiiiiiiddddd      Rd       imm(i16,hw)
            imm = id->emitGetInsSC();
            assert(isValidImmHWVal(imm, id->idOpSize()));
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= static_cast<uint32_t>(imm) << 5;   // hwiiiii iiiiiiiiiii
            code |= insEncodeReg_Rd(id->idReg1());     // ddddd
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DI_1C: // X........Nrrrrrr ssssssnnnnn.....         Rn    imm(N,r,s)
            imm = id->emitGetInsSC();
            assert(isValidImmNRS(imm, id->idOpSize()));
            code = emitInsCode(ins, fmt);
            code |= static_cast<uint32_t>(imm) << 10;  // Nrrrrrrssssss
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeReg_Rn(id->idReg1());     // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DI_1D: // X........Nrrrrrr ssssss.....ddddd      Rd       imm(N,r,s)
            imm = id->emitGetInsSC();
            assert(isValidImmNRS(imm, id->idOpSize()));
            code = emitInsCode(ins, fmt);
            code |= static_cast<uint32_t>(imm) << 10;  // Nrrrrrrssssss
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeReg_Rd(id->idReg1());     // ddddd
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DI_1F: // X..........iiiii cccc..nnnnn.nzcv      Rn imm5  nzcv cond
            cimm = UnpackCondFlagsImm5Imm(id->emitGetInsSC());
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeReg_Rn(id->idReg1());     // nnnnn
            code |= cimm.imm5 << 16;                   // iiiii
            code |= insEncodeFlags(cimm.flags);        // nzcv
            code |= insEncodeCond(cimm.cond);          // cccc
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DI_2A: // X.......shiiiiii iiiiiinnnnnddddd      Rd Rn    imm(i12,sh)
            assert(insOptsNone(id->idInsOpt()) || insOptsLSL12(id->idInsOpt()));
            imm = id->emitGetInsSC();
            assert(isValidUimm12(imm));
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasize(id->idOpSize());   // X
            code |= insEncodeShiftImm12(id->idInsOpt()); // sh
            code |= static_cast<uint32_t>(imm) << 10;    // iiiiiiiiiiii
            code |= insEncodeReg_Rd(id->idReg1());       // ddddd
            code |= insEncodeReg_Rn(id->idReg2());       // nnnnn
            dst += emitOutput_Instr(dst, code);

            if (id->idIsCnsReloc())
            {
                assert(sz == sizeof(instrDesc));
                assert(id->GetAddr() != nullptr);
                emitRecordRelocation(odst, id->GetAddr(), IMAGE_REL_ARM64_PAGEOFFSET_12A);
            }
            break;

        case IF_DI_2B: // X.........Xnnnnn ssssssnnnnnddddd      Rd Rn    imm(0-63)
            code = emitInsCode(ins, fmt);
            imm  = id->emitGetInsSC();
            assert(isValidImmShift(imm, id->idOpSize()));
            code |= insEncodeDatasizeBF(code, id->idOpSize()); // X........X
            code |= insEncodeReg_Rd(id->idReg1());             // ddddd
            code |= insEncodeReg_Rn(id->idReg2());             // nnnnn
            code |= insEncodeReg_Rm(id->idReg2());             // Reg2 also in mmmmm
            code |= insEncodeShiftCount(imm, id->idOpSize());  // ssssss
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DI_2C: // X........Nrrrrrr ssssssnnnnnddddd      Rd Rn    imm(N,r,s)
            imm = id->emitGetInsSC();
            assert(isValidImmNRS(imm, id->idOpSize()));
            code = emitInsCode(ins, fmt);
            code |= static_cast<uint32_t>(imm) << 10;  // Nrrrrrrssssss
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeReg_Rd(id->idReg1());     // ddddd
            code |= insEncodeReg_Rn(id->idReg2());     // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DI_2D: // X........Nrrrrrr ssssssnnnnnddddd      Rd Rn    imr, imms   (N,r,s)
            if (ins == INS_asr || ins == INS_lsl || ins == INS_lsr)
            {
                imm = id->emitGetInsSC();
                assert(isValidImmShift(imm, id->idOpSize()));

                // Shift instructions are aliases of the SBFM/UBFM instructions
                // that actually take 2 registers and 2 constants,

                int R = static_cast<int>(imm);
                int S = size == EA_8BYTE ? 0x3f : 0x1f;

                // R and S are now set correctly for asr and lsr but
                // for lsl we have to adjust the values of R and S.
                if (ins == INS_lsl)
                {
                    R = -static_cast<int>(imm) & S;
                    S -= static_cast<int>(imm);
                }

                imm = PackBitMaskImm(S, R, size);
            }
            else
            {
                // The other instructions have already have encoded N,R and S values
                imm = id->emitGetInsSC();
            }

            assert(isValidImmNRS(imm, id->idOpSize()));

            code = emitInsCode(ins, fmt);
            code |= static_cast<uint32_t>(imm) << 10;  // Nrrrrrrssssss
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeReg_Rd(id->idReg1());     // ddddd
            code |= insEncodeReg_Rn(id->idReg2());     // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_1D: // X............... cccc.......ddddd      Rd       cond
            cimm = UnpackCondImm(id->emitGetInsSC());
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeReg_Rd(id->idReg1());     // ddddd
            code |= insEncodeInvertedCond(cimm.cond);  // cccc
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_2A: // X..........mmmmm ......nnnnn.....         Rn Rm
            assert(insOptsNone(id->idInsOpt()));
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeReg_Rn(id->idReg1());     // nnnnn
            code |= insEncodeReg_Rm(id->idReg2());     // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_2B: // X.......sh.mmmmm ssssssnnnnn.....         Rn Rm {LSL,LSR,ASR,ROR} imm(0-63)
            code = emitInsCode(ins, fmt);
            imm  = id->emitGetInsSC();
            assert(isValidImmShift(imm, id->idOpSize()));
            code |= insEncodeDatasize(id->idOpSize());        // X
            code |= insEncodeShiftType(id->idInsOpt());       // sh
            code |= insEncodeShiftCount(imm, id->idOpSize()); // ssssss
            code |= insEncodeReg_Rn(id->idReg1());            // nnnnn
            code |= insEncodeReg_Rm(id->idReg2());            // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_2C: // X..........mmmmm ooosssnnnnn.....         Rn Rm ext(Rm) LSL imm(0-4)
            code = emitInsCode(ins, fmt);
            imm  = id->emitGetInsSC();
            assert((imm >= 0) && (imm <= 4));
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeExtend(id->idInsOpt());   // ooo
            code |= insEncodeExtendScale(imm);         // sss
            code |= insEncodeReg_Rn(id->idReg1());     // nnnnn
            code |= insEncodeReg_Rm(id->idReg2());     // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_2D: // X..........nnnnn cccc..nnnnnddddd      Rd Rn    cond
            cimm = UnpackCondImm(id->emitGetInsSC());
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeReg_Rd(id->idReg1());     // ddddd
            code |= insEncodeReg_Rn(id->idReg2());     // nnnnn
            code |= insEncodeReg_Rm(id->idReg2());     // mmmmm
            code |= insEncodeInvertedCond(cimm.cond);  // cccc
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_2E: // X..........mmmmm ...........ddddd      Rd    Rm
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeReg_Rd(id->idReg1());     // ddddd
            code |= insEncodeReg_Rm(id->idReg2());     // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_2F: // X.......sh.mmmmm ssssss.....ddddd      Rd    Rm {LSL,LSR,ASR} imm(0-63)
            code = emitInsCode(ins, fmt);
            imm  = id->emitGetInsSC();
            assert(isValidImmShift(imm, id->idOpSize()));
            code |= insEncodeDatasize(id->idOpSize());        // X
            code |= insEncodeShiftType(id->idInsOpt());       // sh
            code |= insEncodeShiftCount(imm, id->idOpSize()); // ssssss
            code |= insEncodeReg_Rd(id->idReg1());            // ddddd
            code |= insEncodeReg_Rm(id->idReg2());            // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_2G: // X............... .....xnnnnnddddd      Rd Rn
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasize(id->idOpSize()); // X
            if ((ins == INS_rev) && (size == EA_8BYTE))
            {
                code |= 0x00000400;
            }
            code |= insEncodeReg_Rd(id->idReg1()); // ddddd
            code |= insEncodeReg_Rn(id->idReg2()); // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_2H: // X........X...... ......nnnnnddddd      Rd Rn
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasizeBF(code, id->idOpSize()); // X........X
            code |= insEncodeReg_Rd(id->idReg1());             // ddddd
            code |= insEncodeReg_Rn(id->idReg2());             // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_2I: // X..........mmmmm cccc..nnnnn.nzcv      Rn Rm    nzcv cond
            cimm = UnpackCondFlagsImm(id->emitGetInsSC());
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeReg_Rn(id->idReg1());     // nnnnn
            code |= insEncodeReg_Rm(id->idReg2());     // mmmmm
            code |= insEncodeFlags(cimm.flags);        // nzcv
            code |= insEncodeCond(cimm.cond);          // cccc
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_3A: // X..........mmmmm ......nnnnnmmmmm      Rd Rn Rm
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeReg_Rd(id->idReg1());     // ddddd
            code |= insEncodeReg_Rn(id->idReg2());     // nnnnn
            code |= insEncodeReg_Rm(id->idReg3());     // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_3B: // X.......sh.mmmmm ssssssnnnnnddddd      Rd Rn Rm {LSL,LSR,ASR} imm(0-63)
            code = emitInsCode(ins, fmt);
            imm  = id->emitGetInsSC();
            assert(isValidImmShift(imm, id->idOpSize()));
            code |= insEncodeDatasize(id->idOpSize());        // X
            code |= insEncodeReg_Rd(id->idReg1());            // ddddd
            code |= insEncodeReg_Rn(id->idReg2());            // nnnnn
            code |= insEncodeReg_Rm(id->idReg3());            // mmmmm
            code |= insEncodeShiftType(id->idInsOpt());       // sh
            code |= insEncodeShiftCount(imm, id->idOpSize()); // ssssss
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_3C: // X..........mmmmm ooosssnnnnnddddd      Rd Rn Rm ext(Rm) LSL imm(0-4)
            code = emitInsCode(ins, fmt);
            imm  = id->emitGetInsSC();
            assert((imm >= 0) && (imm <= 4));
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeExtend(id->idInsOpt());   // ooo
            code |= insEncodeExtendScale(imm);         // sss
            code |= insEncodeReg_Rd(id->idReg1());     // ddddd
            code |= insEncodeReg_Rn(id->idReg2());     // nnnnn
            code |= insEncodeReg_Rm(id->idReg3());     // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_3D: // X..........mmmmm cccc..nnnnnddddd      Rd Rn Rm cond
            cimm = UnpackCondImm(id->emitGetInsSC());
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeReg_Rd(id->idReg1());     // ddddd
            code |= insEncodeReg_Rn(id->idReg2());     // nnnnn
            code |= insEncodeReg_Rm(id->idReg3());     // mmmmm
            code |= insEncodeCond(cimm.cond);          // cccc
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_3E: // X........X.mmmmm ssssssnnnnnddddd      Rd Rn Rm imm(0-63)
            code = emitInsCode(ins, fmt);
            imm  = id->emitGetInsSC();
            assert(isValidImmShift(imm, id->idOpSize()));
            code |= insEncodeDatasizeBF(code, id->idOpSize()); // X........X
            code |= insEncodeReg_Rd(id->idReg1());             // ddddd
            code |= insEncodeReg_Rn(id->idReg2());             // nnnnn
            code |= insEncodeReg_Rm(id->idReg3());             // mmmmm
            code |= insEncodeShiftCount(imm, id->idOpSize());  // ssssss
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DR_4A: // X..........mmmmm .aaaaannnnnmmmmm      Rd Rn Rm Ra
            code = emitInsCode(ins, fmt);
            code |= insEncodeDatasize(id->idOpSize()); // X
            code |= insEncodeReg_Rd(id->idReg1());     // ddddd
            code |= insEncodeReg_Rn(id->idReg2());     // nnnnn
            code |= insEncodeReg_Rm(id->idReg3());     // mmmmm
            code |= insEncodeReg_Ra(id->idReg4());     // aaaaa
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_1A: // .........X.iiiii iii........ddddd      Vd imm8    (fmov - immediate scalar)
            imm      = id->emitGetInsSC();
            elemsize = id->idOpSize();
            code     = emitInsCode(ins, fmt);
            code |= insEncodeFloatElemsize(elemsize); // X
            code |= ((uint32_t)imm << 13);            // iiiii iii
            code |= insEncodeReg_Vd(id->idReg1());    // ddddd
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_1B: // .QX..........iii cmod..iiiiiddddd      Vd imm8    (immediate vector)
            imm      = id->emitGetInsSC() & 0x0ff;
            immShift = (id->emitGetInsSC() & 0x700) >> 8;
            elemsize = optGetElemsize(id->idInsOpt());
            cmode    = 0;
            switch (elemsize)
            {
                case EA_1BYTE:
                    cmode = 0xE;
                    break;
                case EA_2BYTE:
                    cmode = 0x8;
                    cmode |= (immShift << 1);
                    break;
                case EA_4BYTE:
                    if (immShift < 4)
                    {
                        cmode = 0x0;
                        cmode |= (immShift << 1);
                    }
                    else // MSL
                    {
                        cmode = 0x0C + ((immShift & 2) != 0);
                    }
                    break;
                case EA_8BYTE:
                    cmode = 0xE;
                    break;
                default:
                    unreached();
            }

            code = emitInsCode(ins, fmt);
            code |= insEncodeVectorsize(id->idOpSize());
            if (((ins == INS_fmov) || (ins == INS_movi)) && (elemsize == EA_8BYTE))
            {
                code |= 0x20000000;
            }
            if (ins != INS_fmov)
            {
                assert((cmode >= 0) && (cmode <= 0xF));
                code |= (cmode << 12); // cmod
            }
            code |= (((uint32_t)imm >> 5) << 16);  // iii
            code |= (((uint32_t)imm & 0x1f) << 5); // iiiii
            code |= insEncodeReg_Vd(id->idReg1()); // ddddd
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_1C: // .........X...... ......nnnnn.....      Vn #0.0    (fcmp - with zero)
            elemsize = id->idOpSize();
            code     = emitInsCode(ins, fmt);
            code |= insEncodeFloatElemsize(elemsize); // X
            code |= insEncodeReg_Vn(id->idReg1());    // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2A: // .Q.......X...... ......nnnnnddddd      Vd Vn      (fabs, fcvt - vector)
        case IF_DV_2R: // .Q.......X...... ......nnnnnddddd      Sd Vn      (fmaxnmv, fmaxv, fminnmv, fminv)
            elemsize = optGetElemsize(id->idInsOpt());
            code     = emitInsCode(ins, fmt);
            code |= insEncodeVectorsize(id->idOpSize()); // Q
            if ((ins == INS_fcvtl) || (ins == INS_fcvtl2) || (ins == INS_fcvtn) || (ins == INS_fcvtn2))
            {
                // fcvtl{2} and fcvtn{2} encode the element size as
                // esize = 16 << UInt(sz)
                if (elemsize == EA_4BYTE)
                {
                    code |= 0x00400000; // X
                }
                else
                {
                    assert(elemsize == EA_2BYTE);
                }
            }
            else
            {
                code |= insEncodeFloatElemsize(elemsize); // X
            }
            code |= insEncodeReg_Vd(id->idReg1()); // ddddd
            code |= insEncodeReg_Vn(id->idReg2()); // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2B: // .Q.........iiiii ......nnnnnddddd      Rd Vn[] (umov/smov    - to general)
            elemsize = id->idOpSize();
            index    = id->emitGetInsSC();
            datasize = (elemsize == EA_8BYTE) ? EA_16BYTE : EA_8BYTE;
            if (ins == INS_smov)
            {
                datasize = EA_16BYTE;
            }
            code = emitInsCode(ins, fmt);
            code |= insEncodeVectorsize(datasize);         // Q
            code |= insEncodeVectorIndex(elemsize, index); // iiiii
            code |= insEncodeReg_Rd(id->idReg1());         // ddddd
            code |= insEncodeReg_Vn(id->idReg2());         // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2C: // .Q.........iiiii ......nnnnnddddd      Vd Rn   (dup/ins - vector from general)
            if (ins == INS_dup)
            {
                datasize = id->idOpSize();
                elemsize = optGetElemsize(id->idInsOpt());
                index    = 0;
            }
            else // INS_ins
            {
                datasize = EA_16BYTE;
                elemsize = id->idOpSize();
                index    = id->emitGetInsSC();
            }
            code = emitInsCode(ins, fmt);
            code |= insEncodeVectorsize(datasize);         // Q
            code |= insEncodeVectorIndex(elemsize, index); // iiiii
            code |= insEncodeReg_Vd(id->idReg1());         // ddddd
            code |= insEncodeReg_Rn(id->idReg2());         // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2D: // .Q.........iiiii ......nnnnnddddd      Vd Vn[]   (dup - vector)
            index    = id->emitGetInsSC();
            elemsize = optGetElemsize(id->idInsOpt());
            code     = emitInsCode(ins, fmt);
            code |= insEncodeVectorsize(id->idOpSize());   // Q
            code |= insEncodeVectorIndex(elemsize, index); // iiiii
            code |= insEncodeReg_Vd(id->idReg1());         // ddddd
            code |= insEncodeReg_Vn(id->idReg2());         // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2E: // ...........iiiii ......nnnnnddddd      Vd Vn[]   (dup - scalar)
            index    = id->emitGetInsSC();
            elemsize = id->idOpSize();
            code     = emitInsCode(ins, fmt);
            code |= insEncodeVectorIndex(elemsize, index); // iiiii
            code |= insEncodeReg_Vd(id->idReg1());         // ddddd
            code |= insEncodeReg_Vn(id->idReg2());         // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2F: // ...........iiiii .jjjj.nnnnnddddd      Vd[] Vn[] (ins - element)
            elemsize = id->idOpSize();
            imm      = id->emitGetInsSC();
            index    = (imm >> 4) & 0xf;
            index2   = imm & 0xf;
            code     = emitInsCode(ins, fmt);
            code |= insEncodeVectorIndex(elemsize, index);   // iiiii
            code |= insEncodeVectorIndex2(elemsize, index2); // jjjj
            code |= insEncodeReg_Vd(id->idReg1());           // ddddd
            code |= insEncodeReg_Vn(id->idReg2());           // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2G: // .........X...... ......nnnnnddddd      Vd Vn      (fmov, fcvtXX - register)
            elemsize = id->idOpSize();
            code     = emitInsCode(ins, fmt);
            code |= insEncodeFloatElemsize(elemsize); // X
            code |= insEncodeReg_Vd(id->idReg1());    // ddddd
            code |= insEncodeReg_Vn(id->idReg2());    // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2H: // X........X...... ......nnnnnddddd      Rd Vn      (fmov - to general)
            elemsize = id->idOpSize();
            code     = emitInsCode(ins, fmt);
            code |= insEncodeConvertOpt(fmt, id->idInsOpt()); // X   X
            code |= insEncodeReg_Rd(id->idReg1());            // ddddd
            code |= insEncodeReg_Vn(id->idReg2());            // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2I: // X........X...... ......nnnnnddddd      Vd Rn      (fmov - from general)
            elemsize = id->idOpSize();
            code     = emitInsCode(ins, fmt);
            code |= insEncodeConvertOpt(fmt, id->idInsOpt()); // X   X
            code |= insEncodeReg_Vd(id->idReg1());            // ddddd
            code |= insEncodeReg_Rn(id->idReg2());            // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2J: // ........SS.....D D.....nnnnnddddd      Vd Vn      (fcvt)
            code = emitInsCode(ins, fmt);
            code |= insEncodeConvertOpt(fmt, id->idInsOpt()); // SS DD
            code |= insEncodeReg_Vd(id->idReg1());            // ddddd
            code |= insEncodeReg_Vn(id->idReg2());            // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2K: // .........X.mmmmm ......nnnnn.....      Vn Vm      (fcmp)
            elemsize = id->idOpSize();
            code     = emitInsCode(ins, fmt);
            code |= insEncodeFloatElemsize(elemsize); // X
            code |= insEncodeReg_Vn(id->idReg1());    // nnnnn
            code |= insEncodeReg_Vm(id->idReg2());    // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2L: // ........XX...... ......nnnnnddddd      Vd Vn      (abs, neg - scalar)
            elemsize = id->idOpSize();
            code     = emitInsCode(ins, fmt);
            code |= insEncodeElemsize(elemsize);   // XX
            code |= insEncodeReg_Vd(id->idReg1()); // ddddd
            code |= insEncodeReg_Vn(id->idReg2()); // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2M: // .Q......XX...... ......nnnnnddddd   Vd Vn  (abs, neg   - vector)
        case IF_DV_2T: // .Q......XX...... ......nnnnnddddd   Sd Vn  (addv, saddlv, smaxv, sminv, uaddlv, umaxv, uminv)
            elemsize = optGetElemsize(id->idInsOpt());
            code     = emitInsCode(ins, fmt);
            code |= insEncodeVectorsize(id->idOpSize()); // Q
            code |= insEncodeElemsize(elemsize);         // XX
            code |= insEncodeReg_Vd(id->idReg1());       // ddddd
            code |= insEncodeReg_Vn(id->idReg2());       // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2N: // .........iiiiiii ......nnnnnddddd      Vd Vn imm   (shift - scalar)
            imm      = id->emitGetInsSC();
            elemsize = id->idOpSize();
            code     = emitInsCode(ins, fmt);
            code |= insEncodeVectorShift(elemsize, IsVectorRightShiftIns(ins) ? -imm : imm); // iiiiiii
            code |= insEncodeReg_Vd(id->idReg1());                                           // ddddd
            code |= insEncodeReg_Vn(id->idReg2());                                           // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2O: // .Q.......iiiiiii ......nnnnnddddd      Vd Vn imm   (shift - vector)
            imm      = id->emitGetInsSC();
            elemsize = optGetElemsize(id->idInsOpt());
            code     = emitInsCode(ins, fmt);
            code |= insEncodeVectorsize(id->idOpSize());                                     // Q
            code |= insEncodeVectorShift(elemsize, IsVectorRightShiftIns(ins) ? -imm : imm); // iiiiiii
            code |= insEncodeReg_Vd(id->idReg1());                                           // ddddd
            code |= insEncodeReg_Vn(id->idReg2());                                           // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2P: // ............... ......nnnnnddddd      Vd Vn      (aes*, sha1su1)
            elemsize = optGetElemsize(id->idInsOpt());
            code     = emitInsCode(ins, fmt);
            code |= insEncodeReg_Vd(id->idReg1()); // ddddd
            code |= insEncodeReg_Vn(id->idReg2()); // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2Q: // .........X...... ......nnnnnddddd      Vd Vn      (faddp, fmaxnmp, fmaxp, fminnmp,
                       // fminp - scalar)
            elemsize = optGetElemsize(id->idInsOpt());
            code     = emitInsCode(ins, fmt);
            code |= insEncodeFloatElemsize(elemsize); // X
            code |= insEncodeReg_Vd(id->idReg1());    // ddddd
            code |= insEncodeReg_Vn(id->idReg2());    // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2S: // ........XX...... ......nnnnnddddd      Sd Vn      (addp - scalar)
            elemsize = optGetElemsize(id->idInsOpt());
            code     = emitInsCode(ins, fmt);
            code |= insEncodeElemsize(elemsize);   // XX
            code |= insEncodeReg_Vd(id->idReg1()); // ddddd
            code |= insEncodeReg_Vn(id->idReg2()); // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_2U: // ................ ......nnnnnddddd      Sd Sn   (sha1h)
            code = emitInsCode(ins, fmt);
            code |= insEncodeReg_Vd(id->idReg1()); // ddddd
            code |= insEncodeReg_Vn(id->idReg2()); // nnnnn
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_3A: // .Q......XX.mmmmm ......nnnnnddddd      Vd Vn Vm   (vector)
            code     = emitInsCode(ins, fmt);
            elemsize = optGetElemsize(id->idInsOpt());
            code |= insEncodeVectorsize(id->idOpSize()); // Q
            code |= insEncodeElemsize(elemsize);         // XX
            code |= insEncodeReg_Vd(id->idReg1());       // ddddd
            code |= insEncodeReg_Vn(id->idReg2());       // nnnnn
            code |= insEncodeReg_Vm(id->idReg3());       // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_3AI: // .Q......XXLMmmmm ....H.nnnnnddddd      Vd Vn Vm[] (vector)
            code     = emitInsCode(ins, fmt);
            imm      = id->emitGetInsSC();
            elemsize = optGetElemsize(id->idInsOpt());
            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
            code |= insEncodeVectorsize(id->idOpSize());    // Q
            code |= insEncodeElemsize(elemsize);            // XX
            code |= insEncodeVectorIndexLMH(elemsize, imm); // LM H
            code |= insEncodeReg_Vd(id->idReg1());          // ddddd
            code |= insEncodeReg_Vn(id->idReg2());          // nnnnn
            code |= insEncodeReg_Vm(id->idReg3());          // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_3B: // .Q.......X.mmmmm ......nnnnnddddd      Vd Vn Vm   (vector)
            code     = emitInsCode(ins, fmt);
            elemsize = optGetElemsize(id->idInsOpt());
            code |= insEncodeVectorsize(id->idOpSize()); // Q
            code |= insEncodeFloatElemsize(elemsize);    // X
            code |= insEncodeReg_Vd(id->idReg1());       // ddddd
            code |= insEncodeReg_Vn(id->idReg2());       // nnnnn
            code |= insEncodeReg_Vm(id->idReg3());       // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_3BI: // .Q.......XLmmmmm ....H.nnnnnddddd      Vd Vn Vm[] (vector by element)
            code     = emitInsCode(ins, fmt);
            imm      = id->emitGetInsSC();
            elemsize = optGetElemsize(id->idInsOpt());
            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
            code |= insEncodeVectorsize(id->idOpSize()); // Q
            code |= insEncodeFloatElemsize(elemsize);    // X
            code |= insEncodeFloatIndex(elemsize, imm);  // L H
            code |= insEncodeReg_Vd(id->idReg1());       // ddddd
            code |= insEncodeReg_Vn(id->idReg2());       // nnnnn
            code |= insEncodeReg_Vm(id->idReg3());       // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_3C: // .Q.........mmmmm ......nnnnnddddd      Vd Vn Vm   (vector)
            code = emitInsCode(ins, fmt);
            code |= insEncodeVectorsize(id->idOpSize()); // Q
            code |= insEncodeReg_Vd(id->idReg1());       // ddddd
            code |= insEncodeReg_Vn(id->idReg2());       // nnnnn
            code |= insEncodeReg_Vm(id->idReg3());       // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_3D: // .........X.mmmmm ......nnnnnddddd      Vd Vn Vm   (scalar)
            code = emitInsCode(ins, fmt);
            code |= insEncodeFloatElemsize(id->idOpSize()); // X
            code |= insEncodeReg_Vd(id->idReg1());          // ddddd
            code |= insEncodeReg_Vn(id->idReg2());          // nnnnn
            code |= insEncodeReg_Vm(id->idReg3());          // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_3DI: // .........XLmmmmm ....H.nnnnnddddd      Vd Vn Vm[] (scalar by element)
            code     = emitInsCode(ins, fmt);
            imm      = id->emitGetInsSC();
            elemsize = id->idOpSize();
            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
            code |= insEncodeFloatElemsize(elemsize);   // X
            code |= insEncodeFloatIndex(elemsize, imm); // L H
            code |= insEncodeReg_Vd(id->idReg1());      // ddddd
            code |= insEncodeReg_Vn(id->idReg2());      // nnnnn
            code |= insEncodeReg_Vm(id->idReg3());      // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_3E: // DV_3E   ........XX.mmmmm ......nnnnnddddd      Vd Vn Vm   (scalar)
            code     = emitInsCode(ins, fmt);
            elemsize = id->idOpSize();
            code |= insEncodeElemsize(elemsize);   // XX
            code |= insEncodeReg_Vd(id->idReg1()); // ddddd
            code |= insEncodeReg_Vn(id->idReg2()); // nnnnn
            code |= insEncodeReg_Vm(id->idReg3()); // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_3EI: // ........XXLMmmmm ....H.nnnnnddddd      Vd Vn Vm[] (scalar by element)
            code     = emitInsCode(ins, fmt);
            imm      = id->emitGetInsSC();
            elemsize = id->idOpSize();
            assert(Arm64Imm::IsVecIndex(imm, EA_16BYTE, elemsize));
            code |= insEncodeElemsize(elemsize);            // XX
            code |= insEncodeVectorIndexLMH(elemsize, imm); // LM H
            code |= insEncodeReg_Vd(id->idReg1());          // ddddd
            code |= insEncodeReg_Vn(id->idReg2());          // nnnnn
            code |= insEncodeReg_Vm(id->idReg3());          // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_3F: // ...........mmmmm ......nnnnnddddd      Vd Vn Vm   (vector) - source dest regs overlap
            code = emitInsCode(ins, fmt);
            code |= insEncodeReg_Vd(id->idReg1()); // ddddd
            code |= insEncodeReg_Vn(id->idReg2()); // nnnnn
            code |= insEncodeReg_Vm(id->idReg3()); // mmmmm
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_3G: // .Q.........mmmmm .iiii.nnnnnddddd      Vd Vn Vm imm (vector)
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeVectorsize(id->idOpSize()); // Q
            code |= insEncodeReg_Vm(id->idReg3());       // mmmmm
            code |= ((uint32_t)imm << 11);               // iiii
            code |= insEncodeReg_Vn(id->idReg2());       // nnnnn
            code |= insEncodeReg_Vd(id->idReg1());       // ddddd
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_DV_4A: // .........X.mmmmm .aaaaannnnnddddd      Vd Va Vn Vm (scalar)
            code     = emitInsCode(ins, fmt);
            elemsize = id->idOpSize();
            code |= insEncodeFloatElemsize(elemsize); // X
            code |= insEncodeReg_Vd(id->idReg1());    // ddddd
            code |= insEncodeReg_Vn(id->idReg2());    // nnnnn
            code |= insEncodeReg_Vm(id->idReg3());    // mmmmm
            code |= insEncodeReg_Va(id->idReg4());    // aaaaa
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_SN_0A: // ................ ................
            code = emitInsCode(ins, fmt);
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_SI_0A: // ...........iiiii iiiiiiiiiii.....               imm16
            imm = id->emitGetInsSC();
            assert(isValidUimm16(imm));
            code = emitInsCode(ins, fmt);
            code |= ((uint32_t)imm << 5); // iiiii iiiiiiiiiii
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_SI_0B: // ................ ....bbbb........               imm4 - barrier
            imm = id->emitGetInsSC();
            assert((imm >= 0) && (imm <= 15));
            code = emitInsCode(ins, fmt);
            code |= ((uint32_t)imm << 8); // bbbb
            dst += emitOutput_Instr(dst, code);
            break;

        case IF_SR_1A: // ................ ...........ttttt      Rt       (dc zva)
            assert(insOptsNone(id->idInsOpt()));
            code = emitInsCode(ins, fmt);
            code |= insEncodeReg_Rt(id->idReg1()); // ttttt
            dst += emitOutput_Instr(dst, code);
            break;

        default:
            unreached();
    }

    // Determine if any registers now hold GC refs, or whether a register that was overwritten held a GC ref.
    // We assume here that "id->idGCref()" is not GC_NONE only if the instruction described by "id" writes a
    // GC ref to register "id->idReg1()".  (It may, apparently, also not be GC_NONE in other cases, such as
    // for stores, but we ignore those cases here.)
    if (emitInsMayWriteToGCReg(id)) // True if "id->idIns()" writes to a register than can hold GC ref.
    {
        // We assume that "idReg1" is the primary destination register for all instructions
        if (id->idGCref() != GCT_NONE)
        {
            emitGCregLiveUpd(id->idGCref(), id->idReg1(), dst);
        }
        else
        {
            emitGCregDeadUpd(id->idReg1(), dst);
        }

        if (emitInsMayWriteMultipleRegs(id))
        {
            // INS_ldp etc...
            // "idReg2" is the secondary destination register
            if (id->idGCrefReg2() != GCT_NONE)
            {
                emitGCregLiveUpd(id->idGCrefReg2(), id->idReg2(), dst);
            }
            else
            {
                emitGCregDeadUpd(id->idReg2(), dst);
            }
        }
    }

    // Now we determine if the instruction has written to a (local variable) stack location, and either written a GC
    // ref or overwritten one.
    if (id->idIsLclVar() && (id->idAddr()->isTrackedGCSlotStore || id->idAddr()->isGCArgStore))
    {
        bool isArg = id->idAddr()->isGCArgStore;
        int  adr   = id->idAddr()->lclOffset;
        INDEBUG(int varNum = id->idDebugOnlyInfo()->varNum);

        if (id->idGCref() != GCT_NONE)
        {
            if (isArg)
            {
                emitGCargLiveUpd(adr, id->idGCref(), dst DEBUGARG(varNum));
            }
            else
            {
                emitGCvarLiveUpd(adr, id->idGCref(), dst DEBUGARG(varNum));
            }
        }

        // STP is used to copy structs and we don't currently GC-track struct locals. But
        // it's also used to copy structs to the outgoing arg area, and those do require
        // (special) GC tracking.
        if (id->idGCrefReg2() != GCT_NONE)
        {
            assert(InsMayBeGCSlotStorePair(ins));

            adr += REGSIZE_BYTES;

            // TODO-MIKE-Review: This should probably be an assert since we don't
            // currently GC track struct locals so we shouldn't ever see a STP
            // involving anything other that the outgoing arg area.
            if (isArg)
            {
                emitGCargLiveUpd(adr, id->idGCrefReg2(), dst DEBUGARG(varNum));
            }
            else
            {
                emitGCvarLiveUpd(adr, id->idGCrefReg2(), dst DEBUGARG(varNum));
            }
        }
    }

#ifdef DEBUG
    if ((emitComp->opts.disAsm || emitComp->verbose) && (*dp != dst))
    {
        PrintIns(id, *dp, dst - *dp);
    }
#endif

    *dp = dst;
}

#ifdef DEBUG

class Arm64AsmPrinter final : public AsmPrinter
{
public:
    Arm64AsmPrinter(Compiler* compiler, CodeGen* codeGen) : AsmPrinter(compiler, codeGen)
    {
    }

    void Print(instrDesc* id);

private:
    static const char* emitRegName(RegNum reg, emitAttr attr)
    {
        return RegName(reg, attr);
    }

    static const char* emitVectorRegName(RegNum reg)
    {
        static const char* const vRegNames[]{"v0",  "v1",  "v2",  "v3",  "v4",  "v5",  "v6",  "v7",
                                             "v8",  "v9",  "v10", "v11", "v12", "v13", "v14", "v15",
                                             "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23",
                                             "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31"};

        assert((reg >= REG_V0) && (reg <= REG_V31));

        return vRegNames[reg - REG_V0];
    }

    void emitDispInst(instruction ins);
    void emitDispLargeImm(instrDesc* id, insFormat fmt, int64_t imm);
    void emitDispAddrLoadLabel(instrDescJmp* id);
    void emitDispJumpLabel(instrDescJmp* id);
    void emitDispImm(int64_t imm, bool addComma, bool alwaysHex = false);
    void emitDispFrameRef(instrDesc* id);
    void emitDispFloatZero();
    void emitDispFloatImm(int64_t imm8);
    void emitDispImmOptsLSL12(int64_t imm, insOpts opt);
    void emitDispCond(insCond cond);
    void emitDispFlags(insCflags flags);
    void emitDispBarrier(insBarrier barrier);
    void emitDispShiftOpts(insOpts opt);
    void emitDispExtendOpts(insOpts opt);
    void emitDispLSExtendOpts(insOpts opt);
    void emitDispReg(RegNum reg, emitAttr attr, bool addComma);
    void emitDispVectorReg(RegNum reg, insOpts opt, bool addComma);
    void emitDispVectorRegIndex(RegNum reg, emitAttr elemsize, int64_t index, bool addComma);
    void emitDispVectorRegList(RegNum firstReg, unsigned listSize, insOpts opt, bool addComma);
    void emitDispVectorElemList(RegNum firstReg, unsigned listSize, emitAttr elemsize, unsigned index, bool addComma);
    void emitDispArrangement(insOpts opt);
    void emitDispElemsize(emitAttr elemsize);
    void emitDispShiftedReg(RegNum reg, insOpts opt, int64_t imm, emitAttr attr);
    void emitDispExtendReg(RegNum reg, insOpts opt, int64_t imm);
    void emitDispAddrRI(RegNum reg, insOpts opt, int64_t imm);
    void emitDispAddrRRExt(RegNum reg1, RegNum reg2, insOpts opt, bool isScaled, emitAttr size);
};

void Arm64AsmPrinter::emitDispInst(instruction ins)
{
    static const char pad[8] = "       ";
    const char*       name   = insName(ins);

    printf("%s %s", name, pad + Min(sizeof(pad) - 1, strlen(name)));
}

void Arm64AsmPrinter::emitDispAddrLoadLabel(instrDescJmp* id)
{
    assert(insOptsNone(id->idInsOpt()));

    insFormat fmt = id->idInsFmt();

    emitDispReg(id->idReg1(), id->idOpSize(), true);

    if (fmt == IF_LARGEADR)
    {
        printf("(LARGEADR) ");
    }
    else if (fmt == IF_LARGELDC)
    {
        printf("(LARGELDC) ");
    }

    if ((id->idIns() == INS_ldr) || (id->idIns() == INS_ldrsw))
    {
        printf("[");
    }

    if (id->HasConstData())
    {
        printf("RWD%02u", id->GetConstData()->offset);
    }
    else
    {
        PrintLabel(id->GetLabel());
    }

    if (int64_t imm = id->emitGetInsSC())
    {
        printf("%+Id", imm);
    }

    if ((id->idIns() == INS_ldr) || (id->idIns() == INS_ldrsw))
    {
        printf("]");
    }
}

void Arm64AsmPrinter::emitDispJumpLabel(instrDescJmp* id)
{
    assert(insOptsNone(id->idInsOpt()));

    insFormat fmt = id->idInsFmt();

    if (fmt == IF_LARGEJMP)
    {
        printf("(LARGEJMP) ");
    }

    if (fmt == IF_BI_1A)
    {
        emitDispReg(id->idReg1(), id->idOpSize(), true);
    }
    else if (fmt == IF_BI_1B)
    {
        emitDispReg(id->idReg1(), id->idOpSize(), true);
        emitDispImm(id->emitGetInsSC(), true);
    }

    if (id->HasInstrCount())
    {
        int instrCount = id->GetInstrCount();
        int distance   = static_cast<int>(id->idSmallCns()) - -INT8_MIN - 4;

        // TODO-MIKE-Cleanup: The proper assembly format seems to show the jump
        // distance as an immediate value (e.g. beq #24) not this pc-4 thing.
        // And the instruction count should be displayed as a comment.
        printf("pc%s%d (%d instructions)", distance >= 0 ? "+" : "", distance, instrCount);
    }
    else
    {
        PrintLabel(id->GetLabel());
    }
}

void Arm64AsmPrinter::emitDispLargeImm(instrDesc* id, insFormat fmt, int64_t imm)
{
    assert(imm == 0);
    assert(fmt == IF_DI_1E);

    printf("[HIGH RELOC ");

    emitDispImm(reinterpret_cast<int64_t>(id->GetAddr()), false);

    size_t      targetHandle = reinterpret_cast<size_t>(id->idDebugOnlyInfo()->idHandle);
    const char* targetName   = nullptr;

    if (targetHandle == THT_IntializeArrayIntrinsics)
    {
        targetName = "IntializeArrayIntrinsics";
    }
    else if (targetHandle == THT_GSCookieCheck)
    {
        targetName = "GlobalSecurityCookieCheck";
    }
    else if (targetHandle == THT_SetGSCookie)
    {
        targetName = "SetGlobalSecurityCookie";
    }

    printf("]");

    if (targetName != nullptr)
    {
        printf("      // [%s]", targetName);
    }
    else
    {
        PrintHandleComment(id->idDebugOnlyInfo()->idHandle, id->idDebugOnlyInfo()->idHandleKind);
    }
}

void Arm64AsmPrinter::emitDispImm(int64_t imm, bool addComma, bool alwaysHex)
{
    printf("#");

    // Munge any pointers if we want diff-able disassembly.
    // Since some may be emitted as partial words, print as diffable anything that has
    // significant bits beyond the lowest 8-bits.
    if (compiler->opts.disDiffable)
    {
        int64_t top56bits = (imm >> 8);

        if ((top56bits != 0) && (top56bits != -1))
        {
            imm = 0xD1FFAB1E;
        }
    }

    if (!alwaysHex && (imm > -1000) && (imm < 1000))
    {
        printf("%d", imm);
    }
    else
    {
        if ((imm < 0) && ((imm & 0xFFFFFFFF00000000LL) == 0xFFFFFFFF00000000LL))
        {
            printf("-");
            imm = -imm;
        }

        if ((imm & 0xFFFFFFFF00000000LL) != 0)
        {
            printf("0x%llx", imm);
        }
        else
        {
            printf("0x%02x", imm);
        }
    }

    if (addComma)
    {
        printf(", ");
    }
}

void Arm64AsmPrinter::emitDispFloatZero()
{
    printf("#0.0");
}

void Arm64AsmPrinter::emitDispFloatImm(int64_t imm)
{
    printf("#%.4f", DecodeFMovImm(imm));
}

void Arm64AsmPrinter::emitDispImmOptsLSL12(int64_t imm, insOpts opt)
{
    emitDispImm(imm, false);

    if (insOptsLSL12(opt))
    {
        printf(", LSL #12");
    }
}

void Arm64AsmPrinter::emitDispCond(insCond cond)
{
    const static char* armCond[16]{"eq", "ne", "hs", "lo", "mi", "pl", "vs", "vc",
                                   "hi", "ls", "ge", "lt", "gt", "le", "AL", "NV"}; // The last two are invalid
    unsigned imm = (unsigned)cond;
    assert((0 <= imm) && (imm < _countof(armCond)));
    printf(armCond[imm]);
}

void Arm64AsmPrinter::emitDispFlags(insCflags flags)
{
    const static char* armFlags[16]{"0", "v",  "c",  "cv",  "z",  "zv",  "zc",  "zcv",
                                    "n", "nv", "nc", "ncv", "nz", "nzv", "nzc", "nzcv"};
    unsigned imm = (unsigned)flags;
    assert((0 <= imm) && (imm < _countof(armFlags)));
    printf(armFlags[imm]);
}

void Arm64AsmPrinter::emitDispBarrier(insBarrier barrier)
{
    const static char* armBarriers[16]{"#0", "oshld", "oshst", "osh", "#4",  "nshld", "nshst", "nsh",
                                       "#8", "ishld", "ishst", "ish", "#12", "ld",    "st",    "sy"};
    unsigned imm = (unsigned)barrier;
    assert((0 <= imm) && (imm < _countof(armBarriers)));
    printf(armBarriers[imm]);
}

const char* insOptsName(insOpts opt)
{
    switch (opt)
    {
        case INS_OPTS_NONE:
            return "";
        case INS_OPTS_LSL:
            return "LSL";
        case INS_OPTS_LSR:
            return "LSR";
        case INS_OPTS_ASR:
            return "ASR";
        case INS_OPTS_ROR:
            return "ROR";
        case INS_OPTS_MSL:
            return "MSL";
        case INS_OPTS_UXTB:
            return "UXTB";
        case INS_OPTS_UXTH:
            return "UXTH";
        case INS_OPTS_UXTW:
            return "UXTW";
        case INS_OPTS_UXTX:
            return "UXTX";
        case INS_OPTS_SXTB:
            return "SXTB";
        case INS_OPTS_SXTH:
            return "SXTH";
        case INS_OPTS_SXTW:
            return "SXTW";
        case INS_OPTS_SXTX:
            return "SXTX";
        default:
            return "???";
    }
}

void Arm64AsmPrinter::emitDispShiftOpts(insOpts opt)
{
    printf(" %s ", insOptsName(opt));
}

void Arm64AsmPrinter::emitDispExtendOpts(insOpts opt)
{
    printf("%s", insOptsName(opt));
}

void Arm64AsmPrinter::emitDispLSExtendOpts(insOpts opt)
{
    printf("%s", insOptsName(opt));
}

void Arm64AsmPrinter::emitDispReg(RegNum reg, emitAttr attr, bool addComma)
{
    emitAttr size = EA_SIZE(attr);
    printf(emitRegName(reg, size));

    if (addComma)
    {
        printf(", ");
    }
}

void Arm64AsmPrinter::emitDispVectorReg(RegNum reg, insOpts opt, bool addComma)
{
    assert(isVectorRegister(reg));
    printf(emitVectorRegName(reg));
    emitDispArrangement(opt);

    if (addComma)
    {
        printf(", ");
    }
}

void Arm64AsmPrinter::emitDispVectorRegIndex(RegNum reg, emitAttr elemsize, int64_t index, bool addComma)
{
    assert(isVectorRegister(reg));
    printf(emitVectorRegName(reg));
    emitDispElemsize(elemsize);
    printf("[%d]", index);

    if (addComma)
    {
        printf(", ");
    }
}

void Arm64AsmPrinter::emitDispVectorRegList(RegNum firstReg, unsigned listSize, insOpts opt, bool addComma)
{
    assert(isVectorRegister(firstReg));

    RegNum currReg = firstReg;

    printf("{");
    for (unsigned i = 0; i < listSize; i++)
    {
        const bool notLastRegister = (i != listSize - 1);
        emitDispVectorReg(currReg, opt, notLastRegister);
        currReg = (currReg == REG_V31) ? REG_V0 : REG_NEXT(currReg);
    }
    printf("}");

    if (addComma)
    {
        printf(", ");
    }
}

void Arm64AsmPrinter::emitDispVectorElemList(
    RegNum firstReg, unsigned listSize, emitAttr elemsize, unsigned index, bool addComma)
{
    assert(isVectorRegister(firstReg));

    RegNum currReg = firstReg;

    printf("{");
    for (unsigned i = 0; i < listSize; i++)
    {
        printf(emitVectorRegName(currReg));
        emitDispElemsize(elemsize);
        const bool notLastRegister = (i != listSize - 1);
        if (notLastRegister)
        {
            printf(", ");
        }
        currReg = (currReg == REG_V31) ? REG_V0 : REG_NEXT(currReg);
    }
    printf("}");
    printf("[%d]", index);

    if (addComma)
    {
        printf(", ");
    }
}

void Arm64AsmPrinter::emitDispArrangement(insOpts opt)
{
    const char* str;

    switch (opt)
    {
        case INS_OPTS_8B:
            str = "8b";
            break;
        case INS_OPTS_16B:
            str = "16b";
            break;
        case INS_OPTS_4H:
            str = "4h";
            break;
        case INS_OPTS_8H:
            str = "8h";
            break;
        case INS_OPTS_2S:
            str = "2s";
            break;
        case INS_OPTS_4S:
            str = "4s";
            break;
        case INS_OPTS_1D:
            str = "1d";
            break;
        case INS_OPTS_2D:
            str = "2d";
            break;
        default:
            str = "???";
            break;
    }

    printf(".%s", str);
}

void Arm64AsmPrinter::emitDispElemsize(emitAttr elemsize)
{
    const char* str = "???";

    switch (elemsize)
    {
        case EA_1BYTE:
            str = ".b";
            break;
        case EA_2BYTE:
            str = ".h";
            break;
        case EA_4BYTE:
            str = ".s";
            break;
        case EA_8BYTE:
            str = ".d";
            break;

        default:
            assert(!"invalid elemsize");
            break;
    }

    printf(str);
}

void Arm64AsmPrinter::emitDispShiftedReg(RegNum reg, insOpts opt, int64_t imm, emitAttr attr)
{
    emitAttr size = EA_SIZE(attr);
    assert((imm & 0x003F) == imm);
    assert(((imm & 0x0020) == 0) || (size == EA_8BYTE));

    printf(emitRegName(reg, size));

    if (imm > 0)
    {
        printf(",");
        emitDispShiftOpts(opt);
        emitDispImm(imm, false);
    }
}

void Arm64AsmPrinter::emitDispExtendReg(RegNum reg, insOpts opt, int64_t imm)
{
    assert((imm >= 0) && (imm <= 4));
    assert(insOptsNone(opt) || insOptsAnyExtend(opt) || (opt == INS_OPTS_LSL));

    // size is based on the extend option, not the instr size.
    emitAttr size = insOpts32BitExtend(opt) ? EA_4BYTE : EA_8BYTE;

    if (insOptsNone(opt))
    {
        emitDispReg(reg, size, false);
    }
    else
    {
        emitDispReg(reg, size, true);
        if (opt == INS_OPTS_LSL)
            printf("LSL");
        else
            emitDispExtendOpts(opt);
        if ((imm > 0) || (opt == INS_OPTS_LSL))
        {
            printf(" ");
            emitDispImm(imm, false);
        }
    }
}

void Arm64AsmPrinter::emitDispAddrRI(RegNum reg, insOpts opt, int64_t imm)
{
    reg = encodingZRtoSP(reg); // ZR (R31) encodes the SP register

    printf("[");

    emitDispReg(reg, EA_8BYTE, false);

    if (!insOptsPostIndex(opt) && (imm != 0))
    {
        printf(",");
        emitDispImm(imm, false);
    }
    printf("]");

    if (insOptsPreIndex(opt))
    {
        printf("!");
    }
    else if (insOptsPostIndex(opt))
    {
        printf(",");
        emitDispImm(imm, false);
    }
}

void Arm64AsmPrinter::emitDispAddrRRExt(RegNum reg1, RegNum reg2, insOpts opt, bool isScaled, emitAttr size)
{
    printf("[");
    emitDispReg(encodingZRtoSP(reg1), EA_8BYTE, true);
    emitDispExtendReg(reg2, opt, isScaled ? NaturalScale(size) : 0);
    printf("]");
}

static void PrintHexCode(uint8_t* code, size_t sz)
{
    if (sz == 0)
    {
        printf("                    ");
    }
    else if (sz == 4)
    {
        printf("  %08X          ", reinterpret_cast<uint32_t*>(code)[0]);
    }
    else
    {
        assert(sz == 8);
        printf("  %08X %08X ", reinterpret_cast<uint32_t*>(code)[0], reinterpret_cast<uint32_t*>(code)[1]);
    }
}

void Arm64Emitter::PrintIns(instrDesc* id)
{
    Arm64AsmPrinter printer(emitComp, codeGen);
    printer.Print(id);
}

void Arm64AsmPrinter::Print(instrDesc* id)
{
    instruction ins = id->idIns();
    insFormat   fmt = id->idInsFmt();

    if (fmt == IF_GC_REG)
    {
        printf(".gcreg  %s\n", RegName(id->idReg1(), EA_8BYTE));
        return;
    }

    emitDispInst(ins);

    emitAttr size = id->idOpSize();
    emitAttr attr = size;

    if (id->idGCref() == GCT_GCREF)
    {
        attr = EA_GCREF;
    }
    else if (id->idGCref() == GCT_BYREF)
    {
        attr = EA_BYREF;
    }

    switch (fmt)
    {
        int64_t  imm;
        CondImm  cimm;
        unsigned scale;
        unsigned immShift;
        bool     hasShift;
        emitAttr elemsize;
        emitAttr datasize;
        emitAttr srcsize;
        emitAttr dstsize;
        int64_t  index;
        int64_t  index2;
        unsigned registerListSize;

        case IF_BI_0A: // ......iiiiiiiiii iiiiiiiiiiiiiiii               simm26:00
        case IF_BI_0B: // ......iiiiiiiiii iiiiiiiiiii.....               simm19:00
        case IF_BI_1A: // ......iiiiiiiiii iiiiiiiiiiittttt      Rt       simm19:00
        case IF_BI_1B: // B.......bbbbbiii iiiiiiiiiiittttt      Rt imm6, simm14:00
        case IF_LARGEJMP:
            emitDispJumpLabel(static_cast<instrDescJmp*>(id));
            break;

        case IF_LS_1A: // XX...V..iiiiiiii iiiiiiiiiiittttt      Rt    PC imm(1MB)
        case IF_LARGELDC:
        case IF_LARGEADR:
        case IF_SMALLADR:
            emitDispAddrLoadLabel(static_cast<instrDescJmp*>(id));
            break;

        case IF_NOP_JMP:
            break;

        case IF_BI_0C: // ......iiiiiiiiii iiiiiiiiiiiiiiii               simm26:00
            printf("%s",
                   compiler->eeGetMethodFullName(static_cast<CORINFO_METHOD_HANDLE>(id->idDebugOnlyInfo()->idHandle)));
            break;

        case IF_BR_1A: // ................ ......nnnnn.....         Rn
            assert(insOptsNone(id->idInsOpt()));
            emitDispReg(id->idReg1(), size, false);
            break;

        case IF_BR_1B: // ................ ......nnnnn.....         Rn
            assert(insOptsNone(id->idInsOpt()));
            emitDispReg(id->idReg3(), EA_PTRSIZE, false);
            break;

        case IF_DI_1E: // .ii.....iiiiiiii iiiiiiiiiiiddddd      Rd       simm21
            assert(insOptsNone(id->idInsOpt()));
            emitDispReg(id->idReg1(), size, true);
            emitDispLargeImm(id, fmt, id->emitGetInsSC());
            break;

        case IF_LS_2A: // .X.......X...... ......nnnnnttttt      Rt Rn
            assert(insOptsNone(id->idInsOpt()));
            assert(id->emitGetInsSC() == 0);
            emitDispReg(id->idReg1(), emitInsTargetRegSize(id), true);
            emitDispAddrRI(id->idReg2(), id->idInsOpt(), 0);
            break;

        case IF_LS_2B: // .X.......Xiiiiii iiiiiinnnnnttttt      Rt Rn    imm(0-4095)
            assert(insOptsNone(id->idInsOpt()));
            imm   = id->emitGetInsSC();
            scale = NaturalScale(emitInsLoadStoreSize(id));
            imm <<= scale; // The immediate is scaled by the size of the ld/st
            emitDispReg(id->idReg1(), emitInsTargetRegSize(id), true);
            emitDispAddrRI(id->idReg2(), id->idInsOpt(), imm);
            break;

        case IF_LS_2C: // .X.......X.iiiii iiiiPPnnnnnttttt      Rt Rn    imm(-256..+255) no/pre/post inc
            assert(insOptsNone(id->idInsOpt()) || insOptsIndexed(id->idInsOpt()));
            imm = id->emitGetInsSC();
            emitDispReg(id->idReg1(), emitInsTargetRegSize(id), true);
            emitDispAddrRI(id->idReg2(), id->idInsOpt(), imm);
            break;

        case IF_LS_2D: // .Q.............. ....ssnnnnnttttt      Vt Rn
        case IF_LS_2E: // .Q.............. ....ssnnnnnttttt      Vt Rn
            registerListSize = insGetRegisterListSize(id->idIns());
            emitDispVectorRegList(id->idReg1(), registerListSize, id->idInsOpt(), true);

            if (fmt == IF_LS_2D)
            {
                // Load/Store multiple structures       base register
                // Load single structure and replicate  base register
                emitDispAddrRI(id->idReg2(), INS_OPTS_NONE, 0);
            }
            else
            {
                // Load/Store multiple structures       post-indexed by an immediate
                // Load single structure and replicate  post-indexed by an immediate
                emitDispAddrRI(id->idReg2(), INS_OPTS_POST_INDEX, id->idSmallCns());
            }
            break;

        case IF_LS_2F: // .Q.............. xx.Sssnnnnnttttt      Vt[] Rn
        case IF_LS_2G: // .Q.............. xx.Sssnnnnnttttt      Vt[] Rn
            registerListSize = insGetRegisterListSize(id->idIns());
            elemsize         = id->idOpSize();
            emitDispVectorElemList(id->idReg1(), registerListSize, elemsize, id->idSmallCns(), true);

            if (fmt == IF_LS_2F)
            {
                // Load/Store single structure  base register
                emitDispAddrRI(id->idReg2(), INS_OPTS_NONE, 0);
            }
            else
            {
                // Load/Store single structure  post-indexed by an immediate
                emitDispAddrRI(id->idReg2(), INS_OPTS_POST_INDEX, (registerListSize * elemsize));
            }
            break;

        case IF_LS_3A: // .X.......X.mmmmm oooS..nnnnnttttt      Rt Rn Rm ext(Rm) LSL {}
            assert(insOptsLSExtend(id->idInsOpt()));
            emitDispReg(id->idReg1(), emitInsTargetRegSize(id), true);
            emitDispAddrRRExt(id->idReg2(), id->idReg3(), id->idInsOpt(), id->idReg3Scaled(), size);
            break;

        case IF_LS_3B: // X............... .aaaaannnnnddddd      Rt Ra Rn
            assert(insOptsNone(id->idInsOpt()));
            assert(id->emitGetInsSC() == 0);
            emitDispReg(id->idReg1(), emitInsTargetRegSize(id), true);
            emitDispReg(id->idReg2(), emitInsTargetRegSize(id), true);
            emitDispAddrRI(id->idReg3(), id->idInsOpt(), 0);
            break;

        case IF_LS_3C: // X.........iiiiii iaaaaannnnnddddd      Rt Ra Rn imm(im7,sh)
            assert(insOptsNone(id->idInsOpt()) || insOptsIndexed(id->idInsOpt()));
            imm   = id->emitGetInsSC();
            scale = NaturalScale(emitInsLoadStoreSize(id));
            imm <<= scale;
            emitDispReg(id->idReg1(), emitInsTargetRegSize(id), true);
            emitDispReg(id->idReg2(), emitInsTargetRegSize(id), true);
            emitDispAddrRI(id->idReg3(), id->idInsOpt(), imm);
            break;

        case IF_LS_3D: // .X.......X.mmmmm ......nnnnnttttt      Wm Rt Rn
            assert(insOptsNone(id->idInsOpt()));
            emitDispReg(id->idReg1(), EA_4BYTE, true);
            emitDispReg(id->idReg2(), emitInsTargetRegSize(id), true);
            emitDispAddrRI(id->idReg3(), id->idInsOpt(), 0);
            break;

        case IF_LS_3E: // .X.........mmmmm ......nnnnnttttt      Rm Rt Rn ARMv8.1 LSE Atomics
            assert(insOptsNone(id->idInsOpt()));
            assert((EA_SIZE(size) == 4) || (EA_SIZE(size) == 8));
            emitDispReg(id->idReg1(), size, true);
            emitDispReg(id->idReg2(), size, true);
            emitDispAddrRI(id->idReg3(), id->idInsOpt(), 0);
            break;

        case IF_LS_3F: // .Q.........mmmmm ....ssnnnnnttttt      Vt Rn Rm
        case IF_LS_3G: // .Q.........mmmmm ...Sssnnnnnttttt      Vt[] Rn Rm
            registerListSize = insGetRegisterListSize(id->idIns());

            if (fmt == IF_LS_3F)
            {
                emitDispVectorRegList(id->idReg1(), registerListSize, id->idInsOpt(), true);
            }
            else
            {
                elemsize = id->idOpSize();
                emitDispVectorElemList(id->idReg1(), registerListSize, elemsize, id->idSmallCns(), true);
            }

            printf("[");
            emitDispReg(encodingZRtoSP(id->idReg2()), EA_8BYTE, false);
            printf("], ");
            emitDispReg(id->idReg3(), EA_8BYTE, false);
            break;

        case IF_DI_1A: // X.......shiiiiii iiiiiinnnnn.....      Rn       imm(i12,sh)
            emitDispReg(id->idReg1(), size, true);
            emitDispImmOptsLSL12(id->emitGetInsSC(), id->idInsOpt());
            break;

        case IF_DI_1B: // X........hwiiiii iiiiiiiiiiiddddd      Rd       imm(i16,hw)
            emitDispReg(id->idReg1(), size, true);
            if (ins == INS_mov)
            {
                emitDispImm(DecodeHalfwordImm(id->emitGetInsSC()), false);
            }
            else // movz, movn, movk
            {
                int64_t imm = id->emitGetInsSC();
                emitDispImm(imm & UINT16_MAX, false);

                if ((imm >> 16) != 0)
                {
                    emitDispShiftOpts(INS_OPTS_LSL);
                    emitDispImm((imm >> 16) << 4, false);
                }
            }
            break;

        case IF_DI_1C: // X........Nrrrrrr ssssssnnnnn.....         Rn    imm(N,r,s)
            emitDispReg(id->idReg1(), size, true);
            emitDispImm(DecodeBitMaskImm(id->emitGetInsSC(), size), false);
            break;

        case IF_DI_1D: // X........Nrrrrrr ssssss.....ddddd      Rd       imm(N,r,s)
            emitDispReg(encodingZRtoSP(id->idReg1()), size, true);
            emitDispImm(DecodeBitMaskImm(id->emitGetInsSC(), size), false);
            break;

        case IF_DI_2A: // X.......shiiiiii iiiiiinnnnnddddd      Rd Rn    imm(i12,sh)
            if ((ins == INS_add) || (ins == INS_sub))
            {
                emitDispReg(encodingZRtoSP(id->idReg1()), size, true);
                emitDispReg(encodingZRtoSP(id->idReg2()), size, true);
            }
            else
            {
                emitDispReg(id->idReg1(), size, true);
                emitDispReg(id->idReg2(), size, true);
            }

            if (id->idIsCnsReloc())
            {
                assert(ins == INS_add);
                printf("[LOW RELOC ");
                emitDispImm(reinterpret_cast<int64_t>(id->GetAddr()), false);
                printf("]");
            }
            else
            {
                emitDispImmOptsLSL12(id->emitGetInsSC(), id->idInsOpt());
            }
            break;

        case IF_DI_2B: // X........X.nnnnn ssssssnnnnnddddd      Rd Rn    imm(0-63)
            emitDispReg(id->idReg1(), size, true);
            emitDispReg(id->idReg2(), size, true);
            emitDispImm(id->emitGetInsSC(), false);
            break;

        case IF_DI_2C: // X........Nrrrrrr ssssssnnnnnddddd      Rd Rn    imm(N,r,s)
            if (ins == INS_ands)
            {
                emitDispReg(id->idReg1(), size, true);
            }
            else
            {
                emitDispReg(encodingZRtoSP(id->idReg1()), size, true);
            }
            emitDispReg(id->idReg2(), size, true);
            emitDispImm(DecodeBitMaskImm(id->emitGetInsSC(), size), false);
            break;

        case IF_DI_2D: // X........Nrrrrrr ssssssnnnnnddddd      Rd Rn    imr, ims   (N,r,s)
            emitDispReg(id->idReg1(), size, true);
            emitDispReg(id->idReg2(), size, true);

            switch (ins)
            {
                int S, R, N;

                case INS_bfm:
                case INS_sbfm:
                case INS_ubfm:
                    UnpackBitMaskImm(id->emitGetInsSC(), &S, &R, &N);
                    emitDispImm(R, true);
                    emitDispImm(S, false);
                    break;

                case INS_bfi:
                case INS_sbfiz:
                case INS_ubfiz:
                    UnpackBitMaskImm(id->emitGetInsSC(), &S, &R, &N);
                    emitDispImm(getBitWidth(size) - R, true);
                    emitDispImm(S + 1, false);
                    break;

                case INS_bfxil:
                case INS_sbfx:
                case INS_ubfx:
                    UnpackBitMaskImm(id->emitGetInsSC(), &S, &R, &N);
                    emitDispImm(R, true);
                    emitDispImm(S - R + 1, false);
                    break;

                case INS_asr:
                case INS_lsr:
                case INS_lsl:
                    emitDispImm(id->emitGetInsSC(), false);
                    break;

                default:
                    assert(!"Unexpected instruction in IF_DI_2D");
            }

            break;

        case IF_DI_1F: // X..........iiiii cccc..nnnnn.nzcv      Rn imm5  nzcv cond
            emitDispReg(id->idReg1(), size, true);
            cimm = UnpackCondFlagsImm5Imm(id->emitGetInsSC());
            emitDispImm(cimm.imm5, true);
            emitDispFlags(cimm.flags);
            printf(",");
            emitDispCond(cimm.cond);
            break;

        case IF_DR_1D: // X............... cccc.......mmmmm      Rd       cond
            emitDispReg(id->idReg1(), size, true);
            emitDispCond(UnpackCondImm(id->emitGetInsSC()).cond);
            break;

        case IF_DR_2A: // X..........mmmmm ......nnnnn.....         Rn Rm
            emitDispReg(id->idReg1(), size, true);
            emitDispReg(id->idReg2(), size, false);
            break;

        case IF_DR_2B: // X.......sh.mmmmm ssssssnnnnn.....         Rn Rm {LSL,LSR,ASR,ROR} imm(0-63)
            emitDispReg(id->idReg1(), size, true);
            emitDispShiftedReg(id->idReg2(), id->idInsOpt(), id->emitGetInsSC(), size);
            break;

        case IF_DR_2C: // X..........mmmmm ooosssnnnnn.....         Rn Rm ext(Rm) LSL imm(0-4)
            emitDispReg(encodingZRtoSP(id->idReg1()), size, true);
            imm = id->emitGetInsSC();
            emitDispExtendReg(id->idReg2(), id->idInsOpt(), imm);
            break;

        case IF_DR_2D: // X..........nnnnn cccc..nnnnnddddd      Rd Rn    cond
            emitDispReg(id->idReg1(), size, true);
            emitDispReg(id->idReg2(), size, true);
            emitDispCond(UnpackCondImm(id->emitGetInsSC()).cond);
            break;

        case IF_DR_2E: // X..........mmmmm ...........ddddd      Rd    Rm
        case IF_DV_2U: // ................ ......nnnnnddddd      Sd    Sn
            emitDispReg(id->idReg1(), size, true);
            emitDispReg(id->idReg2(), size, false);
            break;

        case IF_DR_2F: // X.......sh.mmmmm ssssss.....ddddd      Rd    Rm {LSL,LSR,ASR} imm(0-63)
            emitDispReg(id->idReg1(), size, true);
            emitDispShiftedReg(id->idReg2(), id->idInsOpt(), id->emitGetInsSC(), size);
            break;

        case IF_DR_2G: // X............... ......nnnnnddddd      Rd Rn
            emitDispReg(encodingZRtoSP(id->idReg1()), size, true);
            emitDispReg(encodingZRtoSP(id->idReg2()), size, false);
            break;

        case IF_DR_2H: // X........X...... ......nnnnnddddd      Rd Rn
            if ((ins == INS_uxtb) || (ins == INS_uxth))
            {
                // There is no 64-bit variant of uxtb and uxth
                // However, we allow idOpSize() to have EA_8BYTE value for these instruction
                emitDispReg(id->idReg1(), EA_4BYTE, true);
                emitDispReg(id->idReg2(), EA_4BYTE, false);
            }
            else
            {
                emitDispReg(id->idReg1(), size, true);
                // sxtb, sxth and sxtb always operate on 32-bit source register
                emitDispReg(id->idReg2(), EA_4BYTE, false);
            }
            break;

        case IF_DR_2I: // X..........mmmmm cccc..nnnnn.nzcv      Rn Rm    nzcv cond
            emitDispReg(id->idReg1(), size, true);
            emitDispReg(id->idReg2(), size, true);
            cimm = UnpackCondFlagsImm(id->emitGetInsSC());
            emitDispFlags(cimm.flags);
            printf(",");
            emitDispCond(cimm.cond);
            break;

        case IF_DR_3A: // X..........mmmmm ......nnnnnmmmmm      Rd Rn Rm
            if ((ins == INS_add) || (ins == INS_sub))
            {
                emitDispReg(encodingZRtoSP(id->idReg1()), size, true);
                emitDispReg(encodingZRtoSP(id->idReg2()), size, true);
            }
            else if ((ins == INS_smulh) || (ins == INS_umulh))
            {
                size = EA_8BYTE;
                // smulh Xd, Xn, Xm
                emitDispReg(id->idReg1(), size, true);
                emitDispReg(id->idReg2(), size, true);
            }
            else if ((ins == INS_smull) || (ins == INS_umull) || (ins == INS_smnegl) || (ins == INS_umnegl))
            {
                // smull Xd, Wn, Wm
                emitDispReg(id->idReg1(), EA_8BYTE, true);
                size = EA_4BYTE;
                emitDispReg(id->idReg2(), size, true);
            }
            else
            {
                emitDispReg(id->idReg1(), size, true);
                emitDispReg(id->idReg2(), size, true);
            }

            emitDispReg(id->idReg3(), size, false);
            break;

        case IF_DR_3B: // X.......sh.mmmmm ssssssnnnnnddddd      Rd Rn Rm {LSL,LSR,ASR} imm(0-63)
            emitDispReg(id->idReg1(), size, true);
            emitDispReg(id->idReg2(), size, true);
            emitDispShiftedReg(id->idReg3(), id->idInsOpt(), id->emitGetInsSC(), size);
            break;

        case IF_DR_3C: // X..........mmmmm ooosssnnnnnddddd      Rd Rn Rm ext(Rm) LSL imm(0-4)
            emitDispReg(encodingZRtoSP(id->idReg1()), size, true);
            emitDispReg(encodingZRtoSP(id->idReg2()), size, true);
            imm = id->emitGetInsSC();
            emitDispExtendReg(id->idReg3(), id->idInsOpt(), imm);
            break;

        case IF_DR_3D: // X..........mmmmm cccc..nnnnnmmmmm      Rd Rn Rm cond
            emitDispReg(id->idReg1(), size, true);
            emitDispReg(id->idReg2(), size, true);
            emitDispReg(id->idReg3(), size, true);
            emitDispCond(UnpackCondImm(id->emitGetInsSC()).cond);
            break;

        case IF_DR_3E: // X........X.mmmmm ssssssnnnnnddddd      Rd Rn Rm imm(0-63)
            emitDispReg(id->idReg1(), size, true);
            emitDispReg(id->idReg2(), size, true);
            emitDispReg(id->idReg3(), size, true);
            emitDispImm(id->emitGetInsSC(), false);
            break;

        case IF_DR_4A: // X..........mmmmm .aaaaannnnnmmmmm      Rd Rn Rm Ra
            if ((ins == INS_smaddl) || (ins == INS_smsubl) || (ins == INS_umaddl) || (ins == INS_umsubl))
            {
                // smaddl Xd, Wn, Wm, Xa
                emitDispReg(id->idReg1(), EA_8BYTE, true);
                emitDispReg(id->idReg2(), EA_4BYTE, true);
                emitDispReg(id->idReg3(), EA_4BYTE, true);
                emitDispReg(id->idReg4(), EA_8BYTE, false);
            }
            else
            {
                emitDispReg(id->idReg1(), size, true);
                emitDispReg(id->idReg2(), size, true);
                emitDispReg(id->idReg3(), size, true);
                emitDispReg(id->idReg4(), size, false);
            }
            break;

        case IF_DV_1A: // .........X.iiiii iii........ddddd      Vd imm8 (fmov - immediate scalar)
            elemsize = id->idOpSize();
            emitDispReg(id->idReg1(), elemsize, true);
            emitDispFloatImm(id->emitGetInsSC());
            break;

        case IF_DV_1B: // .QX..........iii cmod..iiiiiddddd      Vd imm8 (immediate vector)
            imm      = id->emitGetInsSC() & 0x0ff;
            immShift = (id->emitGetInsSC() & 0x700) >> 8;
            hasShift = (immShift != 0);
            elemsize = optGetElemsize(id->idInsOpt());
            if (id->idInsOpt() == INS_OPTS_1D)
            {
                assert(elemsize == size);
                emitDispReg(id->idReg1(), size, true);
            }
            else
            {
                emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
            }
            if (ins == INS_fmov)
            {
                emitDispFloatImm(imm);
                assert(hasShift == false);
            }
            else
            {
                if (elemsize == EA_8BYTE)
                {
                    assert(ins == INS_movi);
                    int64_t       imm64 = 0;
                    const int64_t mask8 = 0xFF;
                    for (unsigned b = 0; b < 8; b++)
                    {
                        if (imm & (int64_t{1} << b))
                        {
                            imm64 |= (mask8 << (b * 8));
                        }
                    }
                    emitDispImm(imm64, hasShift, true);
                }
                else
                {
                    emitDispImm(imm, hasShift, true);
                }
                if (hasShift)
                {
                    insOpts  opt   = (immShift & 0x4) ? INS_OPTS_MSL : INS_OPTS_LSL;
                    unsigned shift = (immShift & 0x3) * 8;
                    emitDispShiftOpts(opt);
                    emitDispImm(shift, false);
                }
            }
            break;

        case IF_DV_1C: // .........X...... ......nnnnn.....      Vn #0.0 (fcmp - with zero)
            elemsize = id->idOpSize();
            emitDispReg(id->idReg1(), elemsize, true);
            emitDispFloatZero();
            break;

        case IF_DV_2A: // .Q.......X...... ......nnnnnddddd      Vd Vn   (fabs, fcvt - vector)
            if (IsVectorLongIns(ins))
            {
                emitDispVectorReg(id->idReg1(), optWidenElemsizeArrangement(id->idInsOpt()), true);
                emitDispVectorReg(id->idReg2(), id->idInsOpt(), false);
            }
            else if (IsVectorNarrowIns(ins))
            {
                emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
                emitDispVectorReg(id->idReg2(), optWidenElemsizeArrangement(id->idInsOpt()), false);
            }
            else
            {
                assert(!IsVectorWideIns(ins));
                emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
                emitDispVectorReg(id->idReg2(), id->idInsOpt(), false);
            }
            break;

        case IF_DV_2P: // ................ ......nnnnnddddd      Vd Vn   (aes*, sha1su1)
            emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
            emitDispVectorReg(id->idReg2(), id->idInsOpt(), false);
            break;

        case IF_DV_2M: // .Q......XX...... ......nnnnnddddd      Vd Vn   (abs, neg - vector)
            if (IsVectorNarrowIns(ins))
            {
                emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
                emitDispVectorReg(id->idReg2(), optWidenElemsizeArrangement(id->idInsOpt()), false);
            }
            else
            {
                assert(!IsVectorLongIns(ins) && !IsVectorWideIns(ins));
                emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
                emitDispVectorReg(id->idReg2(), id->idInsOpt(), false);
            }
            break;

        case IF_DV_2N: // .........iiiiiii ......nnnnnddddd      Vd Vn imm   (shift - scalar)
            elemsize = id->idOpSize();
            if (IsVectorLongIns(ins))
            {
                emitDispReg(id->idReg1(), widenDatasize(elemsize), true);
                emitDispReg(id->idReg2(), elemsize, true);
            }
            else if (IsVectorNarrowIns(ins))
            {
                emitDispReg(id->idReg1(), elemsize, true);
                emitDispReg(id->idReg2(), widenDatasize(elemsize), true);
            }
            else
            {
                assert(!IsVectorWideIns(ins));
                emitDispReg(id->idReg1(), elemsize, true);
                emitDispReg(id->idReg2(), elemsize, true);
            }
            imm = id->emitGetInsSC();
            emitDispImm(imm, false);
            break;

        case IF_DV_2O: // .Q.......iiiiiii ......nnnnnddddd      Vd Vn imm   (shift - vector)
            if ((ins == INS_sxtl) || (ins == INS_sxtl2) || (ins == INS_uxtl) || (ins == INS_uxtl2))
            {
                assert((IsVectorLongIns(ins)));
                emitDispVectorReg(id->idReg1(), optWidenElemsizeArrangement(id->idInsOpt()), true);
                emitDispVectorReg(id->idReg2(), id->idInsOpt(), false);
            }
            else
            {
                if (IsVectorLongIns(ins))
                {
                    emitDispVectorReg(id->idReg1(), optWidenElemsizeArrangement(id->idInsOpt()), true);
                    emitDispVectorReg(id->idReg2(), id->idInsOpt(), true);
                }
                else if (IsVectorNarrowIns(ins))
                {
                    emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
                    emitDispVectorReg(id->idReg2(), optWidenElemsizeArrangement(id->idInsOpt()), true);
                }
                else
                {
                    assert(!IsVectorWideIns(ins));
                    emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
                    emitDispVectorReg(id->idReg2(), id->idInsOpt(), true);
                }

                imm = id->emitGetInsSC();
                emitDispImm(imm, false);
            }
            break;

        case IF_DV_2B: // .Q.........iiiii ......nnnnnddddd      Rd Vn[] (umov/smov    - to general)
            srcsize = id->idOpSize();
            index   = id->emitGetInsSC();
            if (ins == INS_smov)
            {
                dstsize = EA_8BYTE;
            }
            else // INS_umov or INS_mov
            {
                dstsize = (srcsize == EA_8BYTE) ? EA_8BYTE : EA_4BYTE;
            }
            emitDispReg(id->idReg1(), dstsize, true);
            emitDispVectorRegIndex(id->idReg2(), srcsize, index, false);
            break;

        case IF_DV_2C: // .Q.........iiiii ......nnnnnddddd      Vd Rn   (dup/ins - vector from general)
            if (ins == INS_dup)
            {
                datasize = id->idOpSize();
                assert(isValidVectorDatasize(datasize));
                assert(isValidArrangement(datasize, id->idInsOpt()));
                elemsize = optGetElemsize(id->idInsOpt());
                emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
            }
            else // INS_ins
            {
                elemsize = id->idOpSize();
                index    = id->emitGetInsSC();
                assert(isValidVectorElemsize(elemsize));
                emitDispVectorRegIndex(id->idReg1(), elemsize, index, true);
            }
            emitDispReg(id->idReg2(), (elemsize == EA_8BYTE) ? EA_8BYTE : EA_4BYTE, false);
            break;

        case IF_DV_2D: // .Q.........iiiii ......nnnnnddddd      Vd Vn[]   (dup - vector)
            datasize = id->idOpSize();
            assert(isValidVectorDatasize(datasize));
            assert(isValidArrangement(datasize, id->idInsOpt()));
            elemsize = optGetElemsize(id->idInsOpt());
            index    = id->emitGetInsSC();
            emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
            emitDispVectorRegIndex(id->idReg2(), elemsize, index, false);
            break;

        case IF_DV_2E: // ...........iiiii ......nnnnnddddd      Vd Vn[]   (dup - scalar)
            elemsize = id->idOpSize();
            index    = id->emitGetInsSC();
            emitDispReg(id->idReg1(), elemsize, true);
            emitDispVectorRegIndex(id->idReg2(), elemsize, index, false);
            break;

        case IF_DV_2F: // ...........iiiii .jjjj.nnnnnddddd      Vd[] Vn[] (ins - element)
            imm      = id->emitGetInsSC();
            index    = (imm >> 4) & 0xf;
            index2   = imm & 0xf;
            elemsize = id->idOpSize();
            emitDispVectorRegIndex(id->idReg1(), elemsize, index, true);
            emitDispVectorRegIndex(id->idReg2(), elemsize, index2, false);
            break;

        case IF_DV_2G: // .........X...... ......nnnnnddddd      Vd Vn      (fmov, fcvtXX - register)
        case IF_DV_2K: // .........X.mmmmm ......nnnnn.....      Vn Vm      (fcmp)
        case IF_DV_2L: // ........XX...... ......nnnnnddddd      Vd Vn      (abs, neg - scalar)
            size = id->idOpSize();
            if ((ins == INS_fcmeq) || (ins == INS_fcmge) || (ins == INS_fcmgt) || (ins == INS_fcmle) ||
                (ins == INS_fcmlt))
            {
                emitDispReg(id->idReg1(), size, true);
                emitDispReg(id->idReg2(), size, true);
                emitDispImm(0, false);
            }
            else if (IsVectorNarrowIns(ins))
            {
                emitDispReg(id->idReg1(), size, true);
                emitDispReg(id->idReg2(), widenDatasize(size), false);
            }
            else
            {
                emitDispReg(id->idReg1(), size, true);
                emitDispReg(id->idReg2(), size, false);
            }
            break;

        case IF_DV_2H: // X........X...... ......nnnnnddddd      Rd Vn      (fmov, fcvtXX - to general)
        case IF_DV_2I: // X........X...... ......nnnnnddddd      Vd Rn      (fmov, Xcvtf - from general)
        case IF_DV_2J: // ........SS.....D D.....nnnnnddddd      Vd Vn      (fcvt)
            dstsize = optGetDstsize(id->idInsOpt());
            srcsize = optGetSrcsize(id->idInsOpt());

            emitDispReg(id->idReg1(), dstsize, true);
            emitDispReg(id->idReg2(), srcsize, false);
            break;

        case IF_DV_2Q: // .........X...... ......nnnnnddddd      Sd Vn      (faddp, fmaxnmp, fmaxp, fminnmp,
                       // fminp - scalar)
        case IF_DV_2R: // .Q.......X...... ......nnnnnddddd      Sd Vn      (fmaxnmv, fmaxv, fminnmv, fminv)
        case IF_DV_2S: // ........XX...... ......nnnnnddddd      Sd Vn      (addp - scalar)
        case IF_DV_2T: // .Q......XX...... ......nnnnnddddd      Sd Vn      (addv, saddlv, smaxv, sminv, uaddlv,
                       // umaxv, uminv)
            if ((ins == INS_sadalp) || (ins == INS_saddlp) || (ins == INS_uadalp) || (ins == INS_uaddlp))
            {
                emitDispVectorReg(id->idReg1(), optWidenDstArrangement(id->idInsOpt()), true);
                emitDispVectorReg(id->idReg2(), id->idInsOpt(), false);
            }
            else
            {
                if ((ins == INS_saddlv) || (ins == INS_uaddlv))
                {
                    elemsize = optGetElemsize(optWidenDstArrangement(id->idInsOpt()));
                }
                else
                {
                    elemsize = optGetElemsize(id->idInsOpt());
                }
                emitDispReg(id->idReg1(), elemsize, true);
                emitDispVectorReg(id->idReg2(), id->idInsOpt(), false);
            }
            break;

        case IF_DV_3A: // .Q......XX.mmmmm ......nnnnnddddd      Vd Vn Vm   (vector)
            if ((ins == INS_sdot) || (ins == INS_udot))
            {
                // sdot/udot Vd.2s, Vn.8b, Vm.8b
                // sdot/udot Vd.4s, Vn.16b, Vm.16b
                emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
                size = id->idOpSize();
                emitDispVectorReg(id->idReg2(), (size == EA_8BYTE) ? INS_OPTS_8B : INS_OPTS_16B, true);
                emitDispVectorReg(id->idReg3(), (size == EA_8BYTE) ? INS_OPTS_8B : INS_OPTS_16B, false);
            }
            else if (((ins == INS_pmull) && (id->idInsOpt() == INS_OPTS_1D)) ||
                     ((ins == INS_pmull2) && (id->idInsOpt() == INS_OPTS_2D)))
            {
                // pmull Vd.1q, Vn.1d, Vm.1d
                // pmull2 Vd.1q, Vn.2d, Vm.2d
                printf("%s.1q, ", emitVectorRegName(id->idReg1()));
                emitDispVectorReg(id->idReg2(), id->idInsOpt(), true);
                emitDispVectorReg(id->idReg3(), id->idInsOpt(), false);
            }
            else if (IsVectorNarrowIns(ins))
            {
                emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
                emitDispVectorReg(id->idReg2(), optWidenElemsizeArrangement(id->idInsOpt()), true);
                emitDispVectorReg(id->idReg3(), optWidenElemsizeArrangement(id->idInsOpt()), false);
            }
            else
            {
                if (IsVectorLongIns(ins))
                {
                    emitDispVectorReg(id->idReg1(), optWidenElemsizeArrangement(id->idInsOpt()), true);
                    emitDispVectorReg(id->idReg2(), id->idInsOpt(), true);
                }
                else if (IsVectorWideIns(ins))
                {
                    emitDispVectorReg(id->idReg1(), optWidenElemsizeArrangement(id->idInsOpt()), true);
                    emitDispVectorReg(id->idReg2(), optWidenElemsizeArrangement(id->idInsOpt()), true);
                }
                else
                {
                    emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
                    emitDispVectorReg(id->idReg2(), id->idInsOpt(), true);
                }

                emitDispVectorReg(id->idReg3(), id->idInsOpt(), false);
            }
            break;

        case IF_DV_3AI: // .Q......XXLMmmmm ....H.nnnnnddddd      Vd Vn Vm[] (vector by element)
            if ((ins == INS_sdot) || (ins == INS_udot))
            {
                // sdot/udot Vd.2s, Vn.8b, Vm.4b[index]
                // sdot/udot Vd.4s, Vn.16b, Vm.4b[index]
                emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
                size = id->idOpSize();
                emitDispVectorReg(id->idReg2(), (size == EA_8BYTE) ? INS_OPTS_8B : INS_OPTS_16B, true);
                index = id->emitGetInsSC();
                printf("%s.4b[%d]", emitVectorRegName(id->idReg3()), index);
            }
            else
            {
                if (IsVectorLongIns(ins))
                {
                    emitDispVectorReg(id->idReg1(), optWidenElemsizeArrangement(id->idInsOpt()), true);
                    emitDispVectorReg(id->idReg2(), id->idInsOpt(), true);
                }
                else if (IsVectorWideIns(ins))
                {
                    emitDispVectorReg(id->idReg1(), optWidenElemsizeArrangement(id->idInsOpt()), true);
                    emitDispVectorReg(id->idReg2(), optWidenElemsizeArrangement(id->idInsOpt()), true);
                }
                else
                {
                    assert(!IsVectorNarrowIns(ins));
                    emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
                    emitDispVectorReg(id->idReg2(), id->idInsOpt(), true);
                }

                elemsize = optGetElemsize(id->idInsOpt());
                index    = id->emitGetInsSC();
                emitDispVectorRegIndex(id->idReg3(), elemsize, index, false);
            }
            break;

        case IF_DV_3B: // .Q.........mmmmm ......nnnnnddddd      Vd Vn Vm  (vector)
            emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
            emitDispVectorReg(id->idReg2(), id->idInsOpt(), true);
            emitDispVectorReg(id->idReg3(), id->idInsOpt(), false);
            break;

        case IF_DV_3C: //  .Q.........mmmmm ......nnnnnddddd      Vd Vn Vm  (vector)
            emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
            switch (ins)
            {
                case INS_tbl:
                case INS_tbl_2regs:
                case INS_tbl_3regs:
                case INS_tbl_4regs:
                case INS_tbx:
                case INS_tbx_2regs:
                case INS_tbx_3regs:
                case INS_tbx_4regs:
                    registerListSize = insGetRegisterListSize(ins);
                    emitDispVectorRegList(id->idReg2(), registerListSize, INS_OPTS_16B, true);
                    break;
                case INS_mov:
                    break;
                default:
                    emitDispVectorReg(id->idReg2(), id->idInsOpt(), true);
                    break;
            }
            emitDispVectorReg(id->idReg3(), id->idInsOpt(), false);
            break;

        case IF_DV_3BI: // .Q........Lmmmmm ....H.nnnnnddddd      Vd Vn Vm[] (vector by element)
            emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
            emitDispVectorReg(id->idReg2(), id->idInsOpt(), true);
            elemsize = optGetElemsize(id->idInsOpt());
            emitDispVectorRegIndex(id->idReg3(), elemsize, id->emitGetInsSC(), false);
            break;

        case IF_DV_3D: // .........X.mmmmm ......nnnnnddddd      Vd Vn Vm  (scalar)
            emitDispReg(id->idReg1(), size, true);
            emitDispReg(id->idReg2(), size, true);
            emitDispReg(id->idReg3(), size, false);
            break;

        case IF_DV_3E: //  ........XX.mmmmm ......nnnnnddddd      Vd Vn Vm  (scalar)
            if (IsVectorLongIns(ins))
            {
                emitDispReg(id->idReg1(), widenDatasize(size), true);
            }
            else
            {
                assert(!IsVectorNarrowIns(ins) && !IsVectorWideIns(ins));
                emitDispReg(id->idReg1(), size, true);
            }

            emitDispReg(id->idReg2(), size, true);
            emitDispReg(id->idReg3(), size, false);
            break;

        case IF_DV_3EI: // ........XXLMmmmm ....H.nnnnnddddd      Vd Vn Vm[] (scalar by element)
            if (IsVectorLongIns(ins))
            {
                emitDispReg(id->idReg1(), widenDatasize(size), true);
            }
            else
            {
                assert(!IsVectorNarrowIns(ins) && !IsVectorWideIns(ins));
                emitDispReg(id->idReg1(), size, true);
            }
            emitDispReg(id->idReg2(), size, true);
            elemsize = id->idOpSize();
            index    = id->emitGetInsSC();
            emitDispVectorRegIndex(id->idReg3(), elemsize, index, false);
            break;

        case IF_DV_3F: // ..........mmmmm ......nnnnnddddd       Vd Vn Vm (vector)
            if ((ins == INS_sha1c) || (ins == INS_sha1m) || (ins == INS_sha1p))
            {
                // Qd, Sn, Vm (vector)
                emitDispReg(id->idReg1(), size, true);
                emitDispReg(id->idReg2(), EA_4BYTE, true);
                emitDispVectorReg(id->idReg3(), id->idInsOpt(), false);
            }
            else if ((ins == INS_sha256h) || (ins == INS_sha256h2))
            {
                // Qd Qn Vm (vector)
                emitDispReg(id->idReg1(), size, true);
                emitDispReg(id->idReg2(), size, true);
                emitDispVectorReg(id->idReg3(), id->idInsOpt(), false);
            }
            else // INS_sha1su0, INS_sha256su1
            {
                // Vd, Vn, Vm   (vector)
                emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
                emitDispVectorReg(id->idReg2(), id->idInsOpt(), true);
                emitDispVectorReg(id->idReg3(), id->idInsOpt(), false);
            }
            break;

        case IF_DV_3DI: // .........XLmmmmm ....H.nnnnnddddd      Vd Vn Vm[] (scalar by element)
            emitDispReg(id->idReg1(), size, true);
            emitDispReg(id->idReg2(), size, true);
            elemsize = size;
            emitDispVectorRegIndex(id->idReg3(), elemsize, id->emitGetInsSC(), false);
            break;

        case IF_DV_3G: // .Q.........mmmmm .iiii.nnnnnddddd      Vd Vn Vm imm (vector)
            emitDispVectorReg(id->idReg1(), id->idInsOpt(), true);
            emitDispVectorReg(id->idReg2(), id->idInsOpt(), true);
            emitDispVectorReg(id->idReg3(), id->idInsOpt(), true);
            emitDispImm(id->emitGetInsSC(), false);
            break;

        case IF_DV_4A: // .........X.mmmmm .aaaaannnnnddddd      Vd Va Vn Vm (scalar)
            emitDispReg(id->idReg1(), size, true);
            emitDispReg(id->idReg2(), size, true);
            emitDispReg(id->idReg3(), size, true);
            emitDispReg(id->idReg4(), size, false);
            break;

        case IF_SN_0A: // ................ ................
            break;

        case IF_SI_0A: // ...........iiiii iiiiiiiiiii.....               imm16
            emitDispImm(id->emitGetInsSC(), false);
            break;

        case IF_SI_0B: // ................ ....bbbb........               imm4 - barrier
            emitDispBarrier((insBarrier)id->emitGetInsSC());
            break;

        case IF_SR_1A: // ................ ...........ttttt      Rt       (dc zva)
            emitDispReg(id->idReg1(), size, false);
            break;

        default:
            printf("unexpected format %s", EmitterBase::emitIfName(id->idInsFmt()));
            break;
    }

    if (id->idIsLclVar())
    {
        emitDispFrameRef(id);
    }

    printf("\n");
}

void Encoder::PrintAlignmentBoundary(size_t instrAddr, size_t instrEndAddr, const instrDesc* instr, const instrDesc*)
{
    const size_t alignment    = emitComp->opts.compJitAlignLoopBoundary;
    const size_t boundaryAddr = instrEndAddr & ~(alignment - 1);

    if (instrAddr < boundaryAddr)
    {
        // Indicate if instruction is at the alignment boundary or is split
        const size_t bytesCrossedBoundary = instrEndAddr & (alignment - 1);

        if (bytesCrossedBoundary != 0)
        {
            printf("; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ (%s: %d)", insName(instr->idIns()), bytesCrossedBoundary);
        }
        else
        {
            printf("; ...............................");
        }

        printf(" %dB boundary ...............................\n", alignment);
    }
}

void Arm64AsmPrinter::emitDispFrameRef(instrDesc* id)
{
    int varNum  = id->idDebugOnlyInfo()->varNum;
    int varOffs = id->idDebugOnlyInfo()->varOffs;

    printf("\t// [");

    if (varNum < 0)
    {
        printf("T%02d", -varNum);
    }
    else
    {
        compiler->gtDispLclVar(static_cast<unsigned>(varNum), false);
    }

    if (varOffs != 0)
    {
        printf("%c0x%02x", varOffs < 0 ? '-' : '+', abs(varOffs));
    }

    printf("]");
}

void Arm64Encoder::PrintIns(instrDesc* id, uint8_t* code, size_t sz)
{
    JITDUMP("IN%04X: ", id->idDebugOnlyInfo()->idNum);

    PrintInsAddr(code);

    if (emitComp->opts.disDiffable)
    {
        // TODO-MIKE-Cleanup: Remove this, only ARM64 has it and there's no need for it.
        printf("      ");
    }
    else
    {
        PrintHexCode(code, sz);
    }

    Arm64AsmPrinter printer(emitComp, codeGen);
    printer.Print(id);
}

#endif // DEBUG

#if defined(DEBUG) || defined(LATE_DISASM)

void Encoder::getMemoryOperation(instrDesc* id, unsigned* pMemAccessKind, bool* pIsLocalAccess)
{
    unsigned    memAccessKind = PERFSCORE_MEMORY_NONE;
    bool        isLocalAccess = false;
    instruction ins           = id->idIns();

    if (IsLoadOrStoreIns(ins))
    {
        if (IsLoadIns(ins))
        {
            if (IsStoreIns(ins))
            {
                memAccessKind = PERFSCORE_MEMORY_READ_WRITE;
            }
            else
            {
                memAccessKind = PERFSCORE_MEMORY_READ;
            }
        }
        else
        {
            assert(IsStoreIns(ins));
            memAccessKind = PERFSCORE_MEMORY_WRITE;
        }

        insFormat insFmt = id->idInsFmt();

        switch (insFmt)
        {
            case IF_LS_1A:
                isLocalAccess = true;
                break;

            case IF_LS_2A:
            case IF_LS_2B:
            case IF_LS_2C:
            case IF_LS_2D:
            case IF_LS_2E:
            case IF_LS_2F:
            case IF_LS_2G:
            case IF_LS_3A:
            case IF_LS_3F:
            case IF_LS_3G:
                if (isStackRegister(id->idReg2()))
                {
                    isLocalAccess = true;
                }
                break;

            case IF_LS_3B:
            case IF_LS_3C:
            case IF_LS_3D:
            case IF_LS_3E:
                if (isStackRegister(id->idReg3()))
                {
                    isLocalAccess = true;
                }
                break;

            case IF_LARGELDC:
                break;

            default:
                assert(!"Logic Error");
                memAccessKind = PERFSCORE_MEMORY_NONE;
                break;
        }
    }

    *pMemAccessKind = memAccessKind;
    *pIsLocalAccess = isLocalAccess;
}

// Returns the current instruction execution characteristics
// The instruction latencies and throughput values returned by this function
// are from The Arm Cortex-A55 Software Optimization Guide:
// https://static.docs.arm.com/epm128372/20/arm_cortex_a55_software_optimization_guide_v2.pdf
Encoder::insExecutionCharacteristics Encoder::getInsExecutionCharacteristics(instrDesc* id)
{
    instruction ins    = id->idIns();
    insFormat   insFmt = id->idInsFmt();

    unsigned memAccessKind;
    bool     isLocalAccess;
    getMemoryOperation(id, &memAccessKind, &isLocalAccess);

    insExecutionCharacteristics result;
    result.insThroughput = PERFSCORE_THROUGHPUT_ILLEGAL;
    result.insLatency    = PERFSCORE_LATENCY_ILLEGAL;

    // Initialize insLatency based upon the instruction's memAccessKind and local access values
    //
    if (memAccessKind == PERFSCORE_MEMORY_READ)
    {
        result.insLatency = isLocalAccess ? PERFSCORE_LATENCY_RD_STACK : PERFSCORE_LATENCY_RD_GENERAL;
    }
    else if (memAccessKind == PERFSCORE_MEMORY_WRITE)
    {
        result.insLatency = isLocalAccess ? PERFSCORE_LATENCY_WR_STACK : PERFSCORE_LATENCY_WR_GENERAL;
    }
    else if (memAccessKind == PERFSCORE_MEMORY_READ_WRITE)
    {
        result.insLatency = isLocalAccess ? PERFSCORE_LATENCY_RD_WR_STACK : PERFSCORE_LATENCY_RD_WR_GENERAL;
    }

    switch (insFmt)
    {
        //
        // Branch Instructions
        //

        case IF_BI_0A:                                      // b, bl_local
        case IF_BI_0C:                                      // bl, b_tail
            result.insThroughput = PERFSCORE_THROUGHPUT_1C; // but is Dual Issue
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        case IF_BI_0B: // beq, bne, bge, blt, bgt, ble, ...
        case IF_BI_1A: // cbz, cbnz
        case IF_BI_1B: // tbz, tbnz
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        case IF_LARGEJMP: // bcc + b
            result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            result.insLatency    = PERFSCORE_LATENCY_2C;
            break;

        case IF_NOP_JMP:
            result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            result.insLatency    = PERFSCORE_LATENCY_2C;
            break;

        case IF_BR_1B: // blr, br_tail
            if (ins == INS_blr)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                result.insLatency    = PERFSCORE_LATENCY_1C;
                break;
            }
            // otherwise we should have a br_tail instruction
            assert(ins == INS_br_tail);
            FALLTHROUGH;
        case IF_BR_1A: // ret, br
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        //
        // Arithmetic and logical instructions
        //

        // ALU, basic
        case IF_DR_3A: // add, adds, adc, adcs, and, ands, bic, bics,
                       // eon, eor, orn, orr, sub, subs, sbc, sbcs
                       // asr, asrv, lsl, lslv, lsr, lsrv, ror, rorv
                       // sdiv, udiv, mul, smull, smulh, umull, umulh, mneg
        case IF_DR_2A: // cmp, cmn, tst

            switch (ins)
            {
                case INS_mul:
                case INS_smull:
                case INS_umull:
                case INS_mneg:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_3C;
                    break;

                case INS_smulh:
                case INS_umulh:
                    result.insThroughput = PERFSCORE_THROUGHPUT_3C;
                    result.insLatency    = PERFSCORE_LATENCY_6C;
                    break;

                case INS_sdiv:
                case INS_udiv:
                    if (id->idOpSize() == EA_4BYTE)
                    {
                        result.insThroughput = PERFSCORE_THROUGHPUT_4C;
                        result.insLatency    = PERFSCORE_LATENCY_12C;
                        break;
                    }
                    else
                    {
                        assert(id->idOpSize() == EA_8BYTE);
                        result.insThroughput = PERFSCORE_THROUGHPUT_4C;
                        result.insLatency    = PERFSCORE_LATENCY_20C;
                        break;
                    }

                case INS_add:
                case INS_adds:
                case INS_adc:
                case INS_adcs:
                case INS_and:
                case INS_ands:
                case INS_bic:
                case INS_bics:
                case INS_eon:
                case INS_eor:
                case INS_orn:
                case INS_orr:
                case INS_sub:
                case INS_subs:
                case INS_sbc:
                case INS_sbcs:
                case INS_asr:
                case INS_lsl:
                case INS_lsr:
                case INS_ror:
                case INS_cmp:
                case INS_cmn:
                case INS_tst:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_1C;
                    break;

                case INS_asrv:
                case INS_lslv:
                case INS_lsrv:
                case INS_rorv:
                    // variable shift by register
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_1C;
                    break;

                case INS_crc32b:
                case INS_crc32h:
                case INS_crc32cb:
                case INS_crc32ch:
                case INS_crc32x:
                case INS_crc32cx:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_2C;
                    break;

                case INS_crc32w:
                case INS_crc32cw:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_1C;
                    break;

                case INS_smaddl:
                case INS_smsubl:
                case INS_smnegl:
                case INS_umaddl:
                case INS_umsubl:
                case INS_umnegl:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_3C;
                    break;

                default:
                    // all other instructions
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        // ALU, basic immediate
        case IF_DI_1A: // cmp, cmn
        case IF_DI_1C: // tst
        case IF_DI_1D: // mov reg, imm(N,r,s)
        case IF_DI_1E: // adr, adrp
        case IF_DI_1F: // ccmp, ccmn
        case IF_DI_2A: // add, adds, suv, subs
        case IF_DI_2C: // and, ands, eor, orr

            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        case IF_DR_2D: // cinc, cinv, cneg
        case IF_DR_2E: // mov, neg, mvn, negs
        case IF_DI_1B: // mov, movk, movn, movz

            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        case IF_LARGEADR: // adrp + add
        case IF_LARGELDC: // adrp + ldr

            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = PERFSCORE_LATENCY_2C;
            break;

        case IF_SMALLADR: // adr
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        // ALU, shift by immediate
        case IF_DR_3B: // add, adds, and, ands, bic, bics,
                       // eon, eor, orn, orr, sub, subs
        case IF_DR_2B: // cmp, cmn, tst
        case IF_DR_2F: // neg, negs, mvn
        case IF_DI_2B: // ror
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_2C;
            break;

        // ALU, extend, scale
        case IF_DR_3C: // add, adc, and, bic, eon, eor, orn, orr, sub, sbc
        case IF_DR_2C: // cmp
        case IF_DV_2U: // sha1h
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_2C;
            break;
        // ALU, Conditional select
        case IF_DR_1D: // cset, csetm
        case IF_DR_3D: // csel, csinc, csinv, csneg

            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        // ALU, Conditional compare
        case IF_DR_2I: // ccmp , ccmn

            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        // Multiply accumulate
        case IF_DR_4A: // madd, msub, smaddl, smsubl, umaddl, umsubl
            if (id->idOpSize() == EA_4BYTE)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                result.insLatency    = PERFSCORE_LATENCY_3C;
                break;
            }
            else
            {
                assert(id->idOpSize() == EA_8BYTE);
                result.insThroughput = PERFSCORE_THROUGHPUT_5C;
                result.insLatency    = PERFSCORE_LATENCY_3C;
                break;
            }

        // Miscellaneous Data Preocessing instructions
        case IF_DR_3E: // extr
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_2C;
            break;

        case IF_DR_2H: // sxtb, sxth, sxtw, uxtb, uxth, sha1h
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        case IF_DI_2D: // lsl, lsr, asr, sbfm, bfm, ubfm, sbfiz, bfi, ubfiz, sbfx, bfxil, ubfx
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_2C;
            break;

        case IF_DR_2G: // mov sp, cls, clz, rbit, rev16, rev32, rev
            if (ins == INS_rbit)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                result.insLatency    = PERFSCORE_LATENCY_2C;
                break;
            }
            else
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                result.insLatency    = PERFSCORE_LATENCY_1C;
                break;
            }

        //
        // Load/Store Instructions
        //

        case IF_LS_1A: // ldr, ldrsw (literal, pc relative immediate)
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            break;

        case IF_LS_2A: // ldr, ldrsw, ldrb, ldrh, ldrsb, ldrsh, str, strb, strh (no immediate)
                       // ldar, ldarb, ldarh, ldxr, ldxrb, ldxrh,
                       // ldaxr, ldaxrb, ldaxrh, stlr, stlrb, stlrh

            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            // ToDo: store release have 2/4 cycle latency
            break;

        case IF_LS_2B: // ldr, ldrsw, ldrb, ldrh, ldrsb, ldrsh, str, strb, strh (scaled immediate)
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            break;

        case IF_LS_2C: // ldr, ldrsw, ldrb, ldrh, ldrsb, ldrsh, str, strb, strh
                       // ldur, ldurb, ldurh, ldursb, ldursh, ldursw, stur, sturb, sturh
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            break;

        case IF_LS_3A: // ldr, ldrsw, ldrb, ldrh, ldrsb, ldrsh, str, strb strh (register extend, scale 2,4,8)
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            break;

        case IF_LS_3B: // ldp, ldpsw, ldnp, stp, stnp  (load/store pair zero offset)
        case IF_LS_3C: // load/store pair with offset pre/post inc
            if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                // ldp, ldpsw, ldnp
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                if (currentIG->IsMainEpilog() && (ins == INS_ldp))
                {
                    // Reduce latency for ldp instructions in the epilog
                    //
                    result.insLatency = PERFSCORE_LATENCY_2C;
                }
                else if (id->idOpSize() == EA_8BYTE) // X-form
                {
                    // the X-reg variant has an extra cycle of latency
                    // and two cycle throughput
                    result.insLatency += 1.0;
                    result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                }
            }
            else // store instructions
            {
                // stp, stnp
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            }
            break;

        case IF_LS_3D: // stxr, stxrb, stxrh, stlxr, stlxrb, srlxrh
            // Store exclusive register, returning status
            assert(IsStoreIns(ins));
            // @ToDo - find out the actual latency
            result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            result.insLatency    = max(PERFSCORE_LATENCY_4C, result.insLatency);
            break;

        case IF_LS_3E: // ARMv8.1 LSE Atomics
            if (memAccessKind == PERFSCORE_MEMORY_WRITE)
            {
                // staddb, staddlb, staddh, staddlh, stadd. staddl
                result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                result.insLatency    = PERFSCORE_LATENCY_2C;
            }
            else
            {
                assert(memAccessKind == PERFSCORE_MEMORY_READ_WRITE);
                result.insThroughput = PERFSCORE_THROUGHPUT_3C;
                result.insLatency    = max(PERFSCORE_LATENCY_3C, result.insLatency);
            }
            break;

        case IF_LS_2D:
        case IF_LS_2E:
        case IF_LS_3F:
            // Load/Store multiple structures
            // Load single structure and replicate
            switch (ins)
            {
                case INS_ld1:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D-form
                        result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                        result.insLatency    = PERFSCORE_LATENCY_3C;
                    }
                    else
                    {
                        // Q-form
                        assert(id->idOpSize() == EA_16BYTE);
                        result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                        result.insLatency    = PERFSCORE_LATENCY_4C;
                    }
                    break;

                case INS_ld1_2regs:
                case INS_ld2:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D-form
                        result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                        result.insLatency    = PERFSCORE_LATENCY_4C;
                    }
                    else
                    {
                        // Q-form
                        assert(id->idOpSize() == EA_16BYTE);
                        result.insThroughput = PERFSCORE_THROUGHPUT_4C;
                        result.insLatency    = PERFSCORE_LATENCY_6C;
                    }
                    break;

                case INS_ld1_3regs:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D-form
                        result.insThroughput = PERFSCORE_THROUGHPUT_3C;
                        result.insLatency    = PERFSCORE_LATENCY_5C;
                    }
                    else
                    {
                        // Q-form
                        assert(id->idOpSize() == EA_16BYTE);
                        result.insThroughput = PERFSCORE_THROUGHPUT_6C;
                        result.insLatency    = PERFSCORE_LATENCY_8C;
                    }
                    break;

                case INS_ld1_4regs:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D-form
                        result.insThroughput = PERFSCORE_THROUGHPUT_4C;
                        result.insLatency    = PERFSCORE_LATENCY_6C;
                    }
                    else
                    {
                        // Q-form
                        assert(id->idOpSize() == EA_16BYTE);
                        result.insThroughput = PERFSCORE_THROUGHPUT_8C;
                        result.insLatency    = PERFSCORE_LATENCY_10C;
                    }
                    break;

                case INS_ld3:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D-form
                        if (optGetElemsize(id->idInsOpt()) == EA_4BYTE)
                        {
                            // S
                            result.insThroughput = PERFSCORE_THROUGHPUT_3C;
                            result.insLatency    = PERFSCORE_LATENCY_5C;
                        }
                        else
                        {
                            // B/H
                            result.insThroughput = PERFSCORE_THROUGHPUT_4C;
                            result.insLatency    = PERFSCORE_LATENCY_6C;
                        }
                    }
                    else
                    {
                        // Q-form
                        assert(id->idOpSize() == EA_16BYTE);
                        if ((optGetElemsize(id->idInsOpt()) == EA_4BYTE) ||
                            (optGetElemsize(id->idInsOpt()) == EA_8BYTE))
                        {
                            // S/D
                            result.insThroughput = PERFSCORE_THROUGHPUT_6C;
                            result.insLatency    = PERFSCORE_LATENCY_8C;
                        }
                        else
                        {
                            // B/H
                            result.insThroughput = PERFSCORE_THROUGHPUT_7C;
                            result.insLatency    = PERFSCORE_LATENCY_9C;
                        }
                    }
                    break;

                case INS_ld4:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D-form
                        if (optGetElemsize(id->idInsOpt()) == EA_4BYTE)
                        {
                            // S
                            result.insThroughput = PERFSCORE_THROUGHPUT_4C;
                            result.insLatency    = PERFSCORE_LATENCY_6C;
                        }
                        else
                        {
                            // B/H
                            result.insThroughput = PERFSCORE_THROUGHPUT_5C;
                            result.insLatency    = PERFSCORE_LATENCY_7C;
                        }
                    }
                    else
                    {
                        // Q-form
                        assert(id->idOpSize() == EA_16BYTE);
                        if ((optGetElemsize(id->idInsOpt()) == EA_4BYTE) ||
                            (optGetElemsize(id->idInsOpt()) == EA_8BYTE))
                        {
                            // S/D
                            result.insThroughput = PERFSCORE_THROUGHPUT_8C;
                            result.insLatency    = PERFSCORE_LATENCY_10C;
                        }
                        else
                        {
                            // B/H
                            result.insThroughput = PERFSCORE_THROUGHPUT_9C;
                            result.insLatency    = PERFSCORE_LATENCY_11C;
                        }
                    }
                    break;

                case INS_ld1r:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_3C;
                    break;

                case INS_ld2r:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D
                        result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                        result.insLatency    = PERFSCORE_LATENCY_4C;
                    }
                    else
                    {
                        // B/H/S
                        result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                        result.insLatency    = PERFSCORE_LATENCY_3C;
                    }
                    break;

                case INS_ld3r:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D
                        result.insThroughput = PERFSCORE_THROUGHPUT_3C;
                        result.insLatency    = PERFSCORE_LATENCY_5C;
                    }
                    else
                    {
                        // B/H/S
                        result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                        result.insLatency    = PERFSCORE_LATENCY_4C;
                    }
                    break;

                case INS_ld4r:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D
                        result.insThroughput = PERFSCORE_THROUGHPUT_4C;
                        result.insLatency    = PERFSCORE_LATENCY_6C;
                    }
                    else
                    {
                        // B/H/S
                        result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                        result.insLatency    = PERFSCORE_LATENCY_4C;
                    }
                    break;

                case INS_st1:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_1C;
                    break;

                case INS_st1_2regs:
                case INS_st2:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D-form
                        result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                        result.insLatency    = PERFSCORE_LATENCY_1C;
                    }
                    else
                    {
                        // Q-form
                        assert(id->idOpSize() == EA_16BYTE);
                        result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                        result.insLatency    = PERFSCORE_LATENCY_2C;
                    }
                    break;

                case INS_st1_3regs:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D-form
                        result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                        result.insLatency    = PERFSCORE_LATENCY_2C;
                    }
                    else
                    {
                        // Q-form
                        assert(id->idOpSize() == EA_16BYTE);
                        result.insThroughput = PERFSCORE_THROUGHPUT_3C;
                        result.insLatency    = PERFSCORE_LATENCY_3C;
                    }
                    break;

                case INS_st1_4regs:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D-form
                        result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                        result.insLatency    = PERFSCORE_LATENCY_2C;
                    }
                    else
                    {
                        // Q-form
                        assert(id->idOpSize() == EA_16BYTE);
                        result.insThroughput = PERFSCORE_THROUGHPUT_4C;
                        result.insLatency    = PERFSCORE_LATENCY_4C;
                    }
                    break;

                case INS_st3:
                    result.insThroughput = PERFSCORE_THROUGHPUT_3C;
                    result.insLatency    = PERFSCORE_LATENCY_3C;
                    break;

                case INS_st4:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D-form
                        result.insThroughput = PERFSCORE_THROUGHPUT_3C;
                        result.insLatency    = PERFSCORE_LATENCY_3C;
                    }
                    else
                    {
                        assert(id->idOpSize() == EA_16BYTE);
                        if (optGetElemsize(id->idInsOpt()) == EA_8BYTE)
                        {
                            // D
                            result.insThroughput = PERFSCORE_THROUGHPUT_4C;
                            result.insLatency    = PERFSCORE_LATENCY_4C;
                        }
                        else
                        {
                            // B/H/S
                            result.insThroughput = PERFSCORE_THROUGHPUT_5C;
                            result.insLatency    = PERFSCORE_LATENCY_5C;
                        }
                    }
                    break;

                default:
                    unreached();
            }
            break;

        case IF_LS_2F:
        case IF_LS_2G:
        case IF_LS_3G:
            // Load/Store single structure
            switch (ins)
            {
                case INS_ld1:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_3C;
                    break;

                case INS_ld2:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D
                        result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                        result.insLatency    = PERFSCORE_LATENCY_4C;
                    }
                    else
                    {
                        // B/H/S
                        result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                        result.insLatency    = PERFSCORE_LATENCY_3C;
                    }
                    break;

                case INS_ld3:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D
                        result.insThroughput = PERFSCORE_THROUGHPUT_3C;
                        result.insLatency    = PERFSCORE_LATENCY_5C;
                    }
                    else
                    {
                        // B/H/S
                        result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                        result.insLatency    = PERFSCORE_LATENCY_4C;
                    }
                    break;

                case INS_ld4:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D
                        result.insThroughput = PERFSCORE_THROUGHPUT_4C;
                        result.insLatency    = PERFSCORE_LATENCY_6C;
                    }
                    else
                    {
                        // B/H/S
                        result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                        result.insLatency    = PERFSCORE_LATENCY_4C;
                    }
                    break;

                case INS_st1:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_1C;
                    break;

                case INS_st2:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D
                        result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                        result.insLatency    = PERFSCORE_LATENCY_2C;
                    }
                    else
                    {
                        // B/H/S
                        result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                        result.insLatency    = PERFSCORE_LATENCY_1C;
                    }
                    break;

                case INS_st3:
                case INS_st4:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                    result.insLatency    = PERFSCORE_LATENCY_2C;
                    break;

                default:
                    unreached();
            }
            break;

        case IF_SN_0A: // bkpt, brk, nop
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_ZERO;
            break;

        case IF_SI_0B: // dmb, dsb, isb
            // @ToDo - find out the actual latency
            result.insThroughput = PERFSCORE_THROUGHPUT_10C;
            result.insLatency    = PERFSCORE_LATENCY_10C;
            break;

        case IF_DV_2J: // fcvt  Vd Vn
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_4C;
            break;

        case IF_DV_2K: // fcmp  Vd Vn
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        case IF_DV_1A: // fmov - immediate (scalar)
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        case IF_DV_1B: // fmov, orr, bic, movi, mvni  (immediate vector)
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        case IF_DV_1C: // fcmp vn, #0.0
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = PERFSCORE_LATENCY_3C;
            break;

        case IF_DV_2A: // fabs, fneg, fsqrt, fcvtXX, frintX, scvtf, ucvtf, fcmXX (vector)
            switch (ins)
            {
                case INS_fabs:
                case INS_fneg:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency = (id->idOpSize() == EA_8BYTE) ? PERFSCORE_LATENCY_2C : PERFSCORE_LATENCY_3C / 2;
                    break;

                case INS_fsqrt:
                    if ((id->idInsOpt() == INS_OPTS_2S) || (id->idInsOpt() == INS_OPTS_4S))
                    {
                        // S-form
                        result.insThroughput = PERFSCORE_THROUGHPUT_3C;
                        result.insLatency    = PERFSCORE_LATENCY_11C;
                    }
                    else
                    {
                        // D-form
                        assert(id->idInsOpt() == INS_OPTS_2D);
                        result.insThroughput = PERFSCORE_THROUGHPUT_6C;
                        result.insLatency    = PERFSCORE_LATENCY_18C;
                    }
                    break;

                case INS_fcvtas:
                case INS_fcvtau:
                case INS_fcvtms:
                case INS_fcvtmu:
                case INS_fcvtns:
                case INS_fcvtnu:
                case INS_fcvtps:
                case INS_fcvtpu:
                case INS_fcvtzs:
                case INS_fcvtzu:
                case INS_frinta:
                case INS_frinti:
                case INS_frintm:
                case INS_frintn:
                case INS_frintp:
                case INS_frintx:
                case INS_frintz:
                case INS_scvtf:
                case INS_ucvtf:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                case INS_fcmeq:
                case INS_fcmge:
                case INS_fcmgt:
                case INS_fcmle:
                case INS_fcmlt:
                case INS_frecpe:
                case INS_frsqrte:
                case INS_urecpe:
                case INS_ursqrte:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_2C;
                    break;

                case INS_fcvtl:
                case INS_fcvtl2:
                case INS_fcvtn:
                case INS_fcvtn2:
                case INS_fcvtxn:
                case INS_fcvtxn2:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                default:
                    // all other instructions
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        case IF_DV_2G: // fmov, fabs, fneg, fsqrt, fcmXX, fcvtXX, frintX, scvtf, ucvtf (scalar)
            switch (ins)
            {
                case INS_fmov:
                    // FP move, vector register
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_1C;
                    break;

                case INS_fabs:
                case INS_fneg:

                case INS_fcvtas:
                case INS_fcvtau:
                case INS_fcvtms:
                case INS_fcvtmu:
                case INS_fcvtns:
                case INS_fcvtnu:
                case INS_fcvtps:
                case INS_fcvtpu:
                case INS_fcvtzs:
                case INS_fcvtzu:
                case INS_scvtf:
                case INS_ucvtf:

                case INS_frinta:
                case INS_frinti:
                case INS_frintm:
                case INS_frintn:
                case INS_frintp:
                case INS_frintx:
                case INS_frintz:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_3C;
                    break;

                case INS_fcvtxn:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                case INS_fcmeq:
                case INS_fcmge:
                case INS_fcmgt:
                case INS_fcmle:
                case INS_fcmlt:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_2C;
                    break;

                case INS_frecpe:
                case INS_frecpx:
                case INS_frsqrte:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                case INS_fsqrt:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D-form
                        result.insThroughput = PERFSCORE_THROUGHPUT_19C;
                        result.insLatency    = PERFSCORE_LATENCY_22C;
                    }
                    else
                    {
                        // S-form
                        assert(id->idOpSize() == EA_4BYTE);
                        result.insThroughput = PERFSCORE_THROUGHPUT_9C;
                        result.insLatency    = PERFSCORE_LATENCY_12C;
                    }
                    break;

                default:
                    // all other instructions
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        case IF_DV_2Q: // faddp, fmaxnmp, fmaxp, fminnmp, fminp (scalar)
        case IF_DV_2R: // fmaxnmv, fmaxv, fminnmv, fminv
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_4C;
            break;

        case IF_DV_2S: // addp (scalar)
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_3C;
            break;

        case IF_DV_3B: // fadd, fsub, fdiv, fmul, fmulx, fmla, fmls, fmin, fminnm, fmax, fmaxnm, fabd, fcmXX
                       // faddp, fmaxnmp, fmaxp, fminnmp, fminp, addp (vector)
            switch (ins)
            {
                case INS_fmin:
                case INS_fminnm:
                case INS_fmax:
                case INS_fmaxnm:
                case INS_fabd:
                case INS_fadd:
                case INS_fsub:
                case INS_fmul:
                case INS_fmulx:
                case INS_fmla:
                case INS_fmls:
                case INS_frecps:
                case INS_frsqrts:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                case INS_faddp:
                case INS_fmaxnmp:
                case INS_fmaxp:
                case INS_fminnmp:
                case INS_fminp:
                    if (id->idOpSize() == EA_16BYTE)
                    {
                        // Q-form
                        result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                        result.insLatency    = PERFSCORE_LATENCY_4C;
                    }
                    else
                    {
                        result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                        result.insLatency    = PERFSCORE_LATENCY_4C;
                    }
                    break;

                case INS_facge:
                case INS_facgt:
                case INS_fcmeq:
                case INS_fcmge:
                case INS_fcmgt:
                case INS_fcmle:
                case INS_fcmlt:
                    if (id->idOpSize() == EA_16BYTE)
                    {
                        // Q-form
                        result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                        result.insLatency    = PERFSCORE_LATENCY_2C;
                    }
                    else
                    {
                        result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                        result.insLatency    = PERFSCORE_LATENCY_2C;
                    }
                    break;

                case INS_fdiv:
                    if ((id->idInsOpt() == INS_OPTS_2S) || (id->idInsOpt() == INS_OPTS_4S))
                    {
                        // S-form
                        result.insThroughput = PERFSCORE_THROUGHPUT_10C;
                        result.insLatency    = PERFSCORE_LATENCY_13C;
                    }
                    else
                    {
                        // D-form
                        assert(id->idInsOpt() == INS_OPTS_2D);
                        result.insThroughput = PERFSCORE_THROUGHPUT_10C;
                        result.insLatency    = PERFSCORE_LATENCY_22C;
                    }
                    break;

                default:
                    // all other instructions
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        case IF_DV_3AI: // mul, mla, mls (vector by element)
        case IF_DV_3BI: // fmul, fmulx, fmla, fmls (vector by element)
        case IF_DV_3EI: // sqdmlal, sqdmlsl, sqdmulh, sqdmull (scalar by element)
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = PERFSCORE_LATENCY_4C;
            break;

        case IF_DV_4A: // fmadd, fmsub, fnmadd, fnsub (scalar)
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_4C;
            break;

        case IF_DV_3D: // fadd, fsub, fdiv, fmul, fmulx, fmin, fminnm, fmax, fmaxnm, fabd, fcmXX (scalar)
            switch (ins)
            {
                case INS_fadd:
                case INS_fsub:
                case INS_fabd:
                case INS_fmax:
                case INS_fmaxnm:
                case INS_fmin:
                case INS_fminnm:
                case INS_fmul:
                case INS_fmulx:
                case INS_fnmul:
                case INS_frecps:
                case INS_frsqrts:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                case INS_facge:
                case INS_facgt:
                case INS_fcmeq:
                case INS_fcmge:
                case INS_fcmgt:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_2C;
                    break;

                case INS_fdiv:
                    if (id->idOpSize() == EA_8BYTE)
                    {
                        // D-form
                        result.insThroughput = PERFSCORE_THROUGHPUT_6C;
                        result.insLatency    = PERFSCORE_LATENCY_15C;
                    }
                    else
                    {
                        // S-form
                        assert(id->idOpSize() == EA_4BYTE);
                        result.insThroughput = PERFSCORE_THROUGHPUT_3C;
                        result.insLatency    = PERFSCORE_LATENCY_10C;
                    }
                    break;

                default:
                    // all other instructions
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        case IF_DV_2H: // fmov, fcvtXX - to general
            // fmov : FP transfer to general register
            // fcvtaXX : FP convert from vector to general
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_3C;
            break;

        case IF_DV_2I: // fmov, Xcvtf - from general
            switch (ins)
            {
                case INS_fmov:
                    // FP transfer from general register
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_2C;
                    break;

                case INS_scvtf:
                case INS_ucvtf:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_5C;
                    break;

                default:
                    // all other instructions
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        case IF_DV_3C: // mov,and, bic, eor, mov,mvn, orn, bsl, bit, bif,
                       // tbl, tbx (vector)
            switch (ins)
            {
                case INS_tbl:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_1C;
                    break;
                case INS_tbl_2regs:
                    result.insThroughput = PERFSCORE_THROUGHPUT_3X;
                    result.insLatency    = PERFSCORE_LATENCY_2C;
                    break;
                case INS_tbl_3regs:
                    result.insThroughput = PERFSCORE_THROUGHPUT_4X;
                    result.insLatency    = PERFSCORE_LATENCY_3C;
                    break;
                case INS_tbl_4regs:
                    result.insThroughput = PERFSCORE_THROUGHPUT_3X;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;
                case INS_tbx:
                    result.insThroughput = PERFSCORE_THROUGHPUT_3X;
                    result.insLatency    = PERFSCORE_LATENCY_2C;
                    break;
                case INS_tbx_2regs:
                    result.insThroughput = PERFSCORE_THROUGHPUT_4X;
                    result.insLatency    = PERFSCORE_LATENCY_3C;
                    break;
                case INS_tbx_3regs:
                    result.insThroughput = PERFSCORE_THROUGHPUT_5X;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;
                case INS_tbx_4regs:
                    result.insThroughput = PERFSCORE_THROUGHPUT_6X;
                    result.insLatency    = PERFSCORE_LATENCY_5C;
                    break;
                default:
                    // All other instructions
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_1C;
                    break;
            }
            break;

        case IF_DV_2E: // mov, dup (scalar)
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_2C;
            break;

        case IF_DV_2F: // mov, ins (element)
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_2C;
            break;

        case IF_DV_2B: // smov, umov - to general)
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_2C;
            break;

        case IF_DV_2C: // mov, dup, ins - from general)
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            if (ins == INS_dup)
            {
                result.insLatency = PERFSCORE_LATENCY_3C;
            }
            else
            {
                assert((ins == INS_ins) || (ins == INS_mov));
                result.insLatency = PERFSCORE_LATENCY_2C;
            }
            break;

        case IF_DV_2D: // dup (dvector)
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_2C;
            break;

        case IF_DV_3A: // (vector)
            // add, sub, mul, mla, mls, cmeq, cmge, cmgt, cmhi, cmhs, ctst,
            // pmul, saba, uaba, sabd, uabd, umin, uminp, umax, umaxp, smin, sminp, smax, smaxp
            switch (ins)
            {
                case INS_add:
                case INS_sub:
                case INS_cmeq:
                case INS_cmge:
                case INS_cmgt:
                case INS_cmhi:
                case INS_cmhs:
                case INS_shadd:
                case INS_shsub:
                case INS_srhadd:
                case INS_srshl:
                case INS_sshl:
                case INS_smax:
                case INS_smaxp:
                case INS_smin:
                case INS_sminp:
                case INS_umax:
                case INS_umaxp:
                case INS_umin:
                case INS_uminp:
                case INS_uhadd:
                case INS_uhsub:
                case INS_urhadd:
                case INS_urshl:
                case INS_ushl:
                case INS_uzp1:
                case INS_uzp2:
                case INS_zip1:
                case INS_zip2:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_2C;
                    break;

                case INS_trn1:
                case INS_trn2:
                    if (id->idInsOpt() == INS_OPTS_2D)
                    {
                        result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    }
                    else
                    {
                        result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    }

                    result.insLatency = PERFSCORE_LATENCY_2C;
                    break;

                case INS_addp:
                case INS_cmtst:
                case INS_pmul:
                case INS_sabd:
                case INS_sqadd:
                case INS_sqsub:
                case INS_uabd:
                case INS_uqadd:
                case INS_uqsub:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_3C;
                    break;

                case INS_mla:
                case INS_mls:
                case INS_mul:
                case INS_sqdmulh:
                case INS_sqrdmulh:
                case INS_sqrshl:
                case INS_sqshl:
                case INS_uqrshl:
                case INS_uqshl:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                case INS_saba:
                case INS_uaba:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                case INS_sdot:
                case INS_udot:
                    result.insLatency = PERFSCORE_LATENCY_4C;
                    if (id->idOpSize() == EA_16BYTE)
                    {
                        result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    }
                    else
                    {
                        result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    }
                    break;

                case INS_addhn:
                case INS_addhn2:
                case INS_sabdl:
                case INS_sabdl2:
                case INS_saddl2:
                case INS_saddl:
                case INS_saddw:
                case INS_saddw2:
                case INS_ssubl:
                case INS_ssubl2:
                case INS_ssubw:
                case INS_ssubw2:
                case INS_subhn:
                case INS_subhn2:
                case INS_uabdl:
                case INS_uabdl2:
                case INS_uaddl:
                case INS_uaddl2:
                case INS_uaddw:
                case INS_uaddw2:
                case INS_usubl:
                case INS_usubl2:
                case INS_usubw:
                case INS_usubw2:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_3C;
                    break;

                case INS_raddhn:
                case INS_raddhn2:
                case INS_rsubhn:
                case INS_rsubhn2:
                case INS_sabal:
                case INS_sabal2:
                case INS_uabal:
                case INS_uabal2:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                case INS_smlal:
                case INS_smlal2:
                case INS_smlsl:
                case INS_smlsl2:
                case INS_smull:
                case INS_smull2:
                case INS_sqdmlal:
                case INS_sqdmlal2:
                case INS_sqdmlsl:
                case INS_sqdmlsl2:
                case INS_sqdmull:
                case INS_sqdmull2:
                case INS_sqrdmlah:
                case INS_sqrdmlsh:
                case INS_umlal:
                case INS_umlal2:
                case INS_umlsl:
                case INS_umlsl2:
                case INS_umull:
                case INS_umull2:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                case INS_pmull:
                case INS_pmull2:
                    if ((id->idInsOpt() == INS_OPTS_8B) || (id->idInsOpt() == INS_OPTS_16B))
                    {
                        result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                        result.insLatency    = PERFSCORE_LATENCY_3C;
                    }
                    else
                    {
                        // Crypto polynomial (64x64) multiply long
                        assert((id->idInsOpt() == INS_OPTS_1D) || (id->idInsOpt() == INS_OPTS_2D));
                        result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                        result.insLatency    = PERFSCORE_LATENCY_2C;
                    }
                    break;

                default:
                    // all other instructions
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        case IF_DV_3DI: // fmul, fmulx, fmla, fmls (scalar by element)
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = PERFSCORE_LATENCY_4C;
            break;

        case IF_DV_3E: // add, sub, cmeq, cmge, cmgt, cmhi, cmhs, ctst, (scalar)
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_2C;
            break;

        case IF_DV_3G: // ext
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_2C;
            break;

        case IF_DV_2L: // abs, neg, cmeq, cmge, cmgt, cmle, cmlt (scalar)
        case IF_DV_2M: // (vector)
            // abs, neg, mvn, not, cmeq, cmge, cmgt, cmle, cmlt,
            // addv, saddlv,  uaddlv, smaxv, sminv, umaxv, uminv
            // cls, clz, cnt, rbit, rev16, rev32, rev64,
            // xtn, xtn2, shll, shll2
            switch (ins)
            {
                case INS_abs:
                case INS_sqneg:
                case INS_suqadd:
                case INS_usqadd:
                    if (id->idOpSize() == EA_16BYTE)
                    {
                        result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    }
                    else
                    {
                        result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    }

                    result.insLatency = PERFSCORE_LATENCY_3C;
                    break;

                case INS_addv:
                case INS_saddlv:
                case INS_uaddlv:
                case INS_cls:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_3C;
                    break;

                case INS_sminv:
                case INS_smaxv:
                case INS_uminv:
                case INS_umaxv:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                case INS_cmeq:
                case INS_cmge:
                case INS_cmgt:
                case INS_cmle:
                case INS_cmlt:

                case INS_clz:
                case INS_cnt:
                case INS_rbit:
                case INS_rev16:
                case INS_rev32:
                case INS_rev64:
                case INS_xtn:
                case INS_xtn2:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_2C;
                    break;

                case INS_mvn:
                case INS_not:
                case INS_neg:
                case INS_shll:
                case INS_shll2:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_1C;
                    break;

                case INS_sqabs:
                case INS_sqxtn:
                case INS_sqxtn2:
                case INS_sqxtun:
                case INS_sqxtun2:
                case INS_uqxtn:
                case INS_uqxtn2:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                default:
                    // all other instructions
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        case IF_DV_2N: // sshr, ssra, srshr, srsra, shl, ushr, usra, urshr, ursra, sri, sli (shift by immediate -
                       // scalar)
        case IF_DV_2O: // sshr, ssra, srshr, srsra, shl, ushr, usra, urshr, ursra, sri, sli (shift by immediate -
                       // vector)
                       // sshll, sshll2, ushll, ushll2, shrn, shrn2, rshrn, rshrn2, sxrl, sxl2, uxtl, uxtl2
            switch (ins)
            {
                case INS_shl:
                case INS_shrn:
                case INS_shrn2:
                case INS_sli:
                case INS_sri:
                case INS_sshr:
                case INS_ushr:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_2C;
                    break;

                case INS_shll:
                case INS_shll2:
                case INS_sshll:
                case INS_sshll2:
                case INS_ushll:
                case INS_ushll2:
                case INS_sxtl:
                case INS_sxtl2:
                case INS_uxtl:
                case INS_uxtl2:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_2C;
                    break;

                case INS_rshrn:
                case INS_rshrn2:
                case INS_srshr:
                case INS_sqshrn:
                case INS_sqshrn2:
                case INS_ssra:
                case INS_urshr:
                case INS_uqshrn:
                case INS_uqshrn2:
                case INS_usra:
                    if (id->idOpSize() == EA_16BYTE)
                    {
                        result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                        result.insLatency    = PERFSCORE_LATENCY_3C;
                    }
                    else
                    {
                        result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                        result.insLatency    = PERFSCORE_LATENCY_3C;
                    }
                    break;

                case INS_srsra:
                case INS_ursra:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                case INS_sqrshrn:
                case INS_sqrshrn2:
                case INS_sqrshrun:
                case INS_sqrshrun2:
                case INS_sqshrun:
                case INS_sqshrun2:
                case INS_sqshl:
                case INS_sqshlu:
                case INS_uqrshrn:
                case INS_uqrshrn2:
                case INS_uqshl:
                    if (id->idOpSize() == EA_16BYTE)
                    {
                        result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                        result.insLatency    = PERFSCORE_LATENCY_4C;
                    }
                    else
                    {
                        result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                        result.insLatency    = PERFSCORE_LATENCY_4C;
                    }
                    break;

                default:
                    // all other instructions
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        case IF_DV_2P: // aese, aesd, aesmc, aesimc, sha1su1, sha256su0
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = PERFSCORE_LATENCY_2C;
            break;

        case IF_DV_3F: // sha1c, sha1m, sha1p, sha1su0, sha256h, sha256h2, sha256su1 (vector)
            switch (ins)
            {
                case INS_sha1su0:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_2C;
                    break;

                case INS_sha256su0:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_3C;
                    break;

                case INS_sha1c:
                case INS_sha1m:
                case INS_sha1p:
                case INS_sha256h:
                case INS_sha256h2:
                case INS_sha256su1:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                default:
                    // all other instructions
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        case IF_SI_0A: // brk   imm16
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        case IF_SR_1A:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        case IF_DV_2T: // addv, saddlv, smaxv, sminv, uaddlv, umaxv, uminv
            switch (ins)
            {
                case INS_addv:
                case INS_saddlv:
                case INS_uaddlv:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_3C;
                    break;

                case INS_smaxv:
                case INS_sminv:
                case INS_umaxv:
                case INS_uminv:
                case INS_sha256h2:
                case INS_sha256su1:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                case INS_sadalp:
                case INS_uadalp:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                    result.insLatency    = PERFSCORE_LATENCY_4C;
                    break;

                case INS_saddlp:
                case INS_uaddlp:
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_3C;
                    break;

                default:
                    // all other instructions
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        default:
            // all other instructions
            perfScoreUnhandledInstruction(id, &result);
            break;
    }

    return result;
}

#endif // defined(DEBUG) || defined(LATE_DISASM)

// Check if the current `mov` instruction is redundant and can be omitted.
// A `mov` is redundant in following 3 cases:
//
// 1. Move to same register
//    (Except 4-byte movement like "mov w1, w1" which zeros out upper bits of x1 register)
//
//      mov Rx, Rx
//
// 2. Move that is identical to last instruction emitted.
//
//      mov Rx, Ry  # <-- last instruction
//      mov Rx, Ry  # <-- current instruction can be omitted.
//
// 3. Opposite Move as that of last instruction emitted.
//
//      mov Rx, Ry  # <-- last instruction
//      mov Ry, Rx  # <-- current instruction can be omitted.
//
// Arguments:
// ins  - The current instruction
// size - Operand size of current instruction
// dst  - The current destination
// src  - The current source
// canSkip - The move can be skipped as it doesn't represent special semantics
//
// Return Value:
// true if previous instruction moved from current dst to src.

bool Arm64Emitter::IsRedundantMov(instruction ins, emitAttr size, RegNum dst, RegNum src, bool canSkip)
{
    assert(ins == INS_mov);

    if (canSkip && (dst == src))
    {
        // These elisions used to be explicit even when optimizations were disabled
        return true;
    }

    if (!emitComp->opts.OptimizationEnabled())
    {
        // The remaining move elisions should only happen if optimizations are enabled
        return false;
    }

    if (dst == src)
    {
        // A mov with a EA_4BYTE has the side-effect of clearing the upper bits
        // So only eliminate mov instructions that are not clearing the upper bits
        //
        if (isGeneralRegisterOrSP(dst) && (size == EA_8BYTE))
        {
            JITDUMP("\n -- suppressing mov because src and dst is same 8-byte register.\n");
            return true;
        }
        else if (isVectorRegister(dst) && (size == EA_16BYTE))
        {
            JITDUMP("\n -- suppressing mov because src and dst is same 16-byte register.\n");
            return true;
        }
    }

    instrDesc* lastIns = GetLastInsInCurrentBlock();

    if ((lastIns == nullptr) || (lastIns->idIns() != INS_mov) || (lastIns->idOpSize() != size))
    {
        return false;
    }

    // Check if we did same move in prev instruction except dst/src were switched.
    RegNum    prevDst    = lastIns->idReg1();
    RegNum    prevSrc    = lastIns->idReg2();
    insFormat lastInsfmt = lastIns->idInsFmt();

    // Sometimes lastIns can be a mov with single register e.g. "mov reg, #imm". So ensure to
    // optimize formats that does vector-to-vector or scalar-to-scalar register movs.
    //
    const bool isValidLastInsFormats =
        ((lastInsfmt == IF_DV_3C) || (lastInsfmt == IF_DR_2G) || (lastInsfmt == IF_DR_2E));

    if (isValidLastInsFormats && (prevDst == dst) && (prevSrc == src))
    {
        assert(lastIns->idOpSize() == size);
        JITDUMP("\n -- suppressing mov because previous instruction already moved from src to dst register.\n");
        return true;
    }

    if ((prevDst == src) && (prevSrc == dst) && isValidLastInsFormats)
    {
        // For mov with EA_8BYTE, ensure src/dst are both scalar or both vector.
        if (size == EA_8BYTE)
        {
            if (isVectorRegister(src) == isVectorRegister(dst))
            {
                JITDUMP("\n -- suppressing mov because previous instruction already did an opposite move from dst "
                        "to src register.\n");
                return true;
            }
        }

        // For mov with EA_16BYTE, both src/dst will be vector.
        else if (size == EA_16BYTE)
        {
            assert(isVectorRegister(src) && isVectorRegister(dst));
            assert(lastInsfmt == IF_DV_3C);

            JITDUMP("\n -- suppressing mov because previous instruction already did an opposite move from dst to "
                    "src register.\n");
            return true;
        }

        // For mov of other sizes, don't optimize because it has side-effect of clearing the upper bits.
    }

    return false;
}

// For ldr/str pair next to each other, check if the current load or store is needed or is
// the value already present as of previous instruction.
//
// ldr x1,  [x2, #56]
// str x1,  [x2, #56]   <-- redundant
//
//       OR
//
// str x1,  [x2, #56]
// ldr x1,  [x2, #56]   <-- redundant

// Arguments:
// ins  - The current instruction
// dst  - The current destination
// src  - The current source
// imm  - Immediate offset
// size - Operand size
// fmt  - Format of instruction
// Return Value:
// true if previous instruction already has desired value in register/memory location.

bool Arm64Emitter::IsRedundantLdStr(
    instruction ins, RegNum reg1, RegNum reg2, int64_t imm, emitAttr size, insFormat fmt)
{
    instrDesc* lastIns = GetLastInsInCurrentBlock();

    if ((lastIns == nullptr) || ((ins != INS_ldr) && (ins != INS_str)))
    {
        return false;
    }

    RegNum    prevReg1   = lastIns->idReg1();
    RegNum    prevReg2   = lastIns->idReg2();
    insFormat lastInsfmt = lastIns->idInsFmt();
    emitAttr  prevSize   = lastIns->idOpSize();
    int64_t prevImm = lastIns->idIsLargeCns() ? static_cast<instrDescCns*>(lastIns)->idcCnsVal : lastIns->idSmallCns();

    // Only optimize if:
    // 1. "base" or "base plus immediate offset" addressing modes.
    // 2. Addressing mode matches with previous instruction.
    // 3. The operand size matches with previous instruction
    if (((fmt != IF_LS_2A) && (fmt != IF_LS_2B)) || (fmt != lastInsfmt) || (prevSize != size))
    {
        return false;
    }

    if ((ins == INS_ldr) && (lastIns->idIns() == INS_str))
    {
        // If reg1 is of size less than 8-bytes, then eliminating the 'ldr'
        // will not zero the upper bits of reg1.

        // Make sure operand size is 8-bytes
        // str w0, [x1, #4]
        // ldr w0, [x1, #4]  <-- can't eliminate because upper-bits of x0 won't get set.
        if (size != EA_8BYTE)
        {
            return false;
        }

        if ((prevReg1 == reg1) && (prevReg2 == reg2) && (imm == prevImm))
        {
            JITDUMP("\n -- suppressing 'ldr reg%u [reg%u, #%u]' as previous 'str reg%u [reg%u, #%u]' was from same "
                    "location.\n",
                    reg1, reg2, imm, prevReg1, prevReg2, prevImm);
            return true;
        }
    }
    else if ((ins == INS_str) && (lastIns->idIns() == INS_ldr))
    {
        // Make sure src and dst registers are not same.
        // ldr x0, [x0, #4]
        // str x0, [x0, #4]  <-- can't eliminate because [x0+3] is not same destination as previous source.
        // Note, however, that we can not eliminate store in the following sequence
        // ldr wzr, [x0, #4]
        // str wzr, [x0, #4]
        // since load operation doesn't (and can't) change the value of its destination register.
        if ((reg1 != reg2) && (prevReg1 == reg1) && (prevReg2 == reg2) && (imm == prevImm) && (reg1 != REG_ZR))
        {
            JITDUMP("\n -- suppressing 'str reg%u [reg%u, #%u]' as previous 'ldr reg%u [reg%u, #%u]' was from same "
                    "location.\n",
                    reg1, reg2, imm, prevReg1, prevReg2, prevImm);
            return true;
        }
    }

    return false;
}

#endif // TARGET_ARM64
