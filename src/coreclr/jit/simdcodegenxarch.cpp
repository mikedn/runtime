// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                        Amd64 SIMD Code Generator                          XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/
#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#pragma warning(disable : 4310) // cast truncates constant value - happens for (int8_t)SHUFFLE_ZXXX
#endif

#ifdef TARGET_XARCH
#ifdef FEATURE_SIMD

#include "emit.h"
#include "codegen.h"
#include "sideeffects.h"
#include "lower.h"
#include "gcinfo.h"
#include "gcinfoencoder.h"

// Instruction immediates

// Insertps:
// - bits 6 and 7 of the immediate indicate which source item to select (0..3)
// - bits 4 and 5 of the immediate indicate which target item to insert into (0..3)
// - bits 0 to 3 of the immediate indicate which target item to zero
#define INSERTPS_SOURCE_SELECT(i) ((i) << 6)
#define INSERTPS_TARGET_SELECT(i) ((i) << 4)
#define INSERTPS_ZERO(i) (1 << (i))

int8_t ShufpsImm(unsigned i0, unsigned i1, unsigned i2, unsigned i3)
{
    return static_cast<int8_t>(i0 | (i1 << 2) | (i2 << 4) | (i3 << 6));
}

int8_t ShufpsImm(unsigned i)
{
    return ShufpsImm(i, i, i, i);
}

// ROUNDPS/PD:
// - Bit 0 through 1 - Rounding mode
//   * 0b00 - Round to nearest (even)
//   * 0b01 - Round toward Neg. Infinity
//   * 0b10 - Round toward Pos. Infinity
//   * 0b11 - Round toward zero (Truncate)
// - Bit 2 - Source of rounding control, 0b0 for immediate.
// - Bit 3 - Precision exception, 0b1 to ignore. (We don't raise FP exceptions)
#define ROUNDPS_TO_NEAREST_IMM 0b1000
#define ROUNDPS_TOWARD_NEGATIVE_INFINITY_IMM 0b1001
#define ROUNDPS_TOWARD_POSITIVE_INFINITY_IMM 0b1010
#define ROUNDPS_TOWARD_ZERO_IMM 0b1011

// getOpForSIMDIntrinsic: return the opcode for the given SIMD Intrinsic
//
// Arguments:
//   intrinsicId    -   SIMD intrinsic Id
//   baseType       -   Base type of the SIMD vector
//   ival           -   Out param. Any immediate byte operand that needs to be passed to SSE2 opcode
//
//
// Return Value:
//   Instruction (op) to be used, and ival is set if instruction requires an immediate operand.
//
instruction CodeGen::getOpForSIMDIntrinsic(SIMDIntrinsicID intrinsicId, var_types baseType, unsigned* ival /*=nullptr*/)
{
    // Minimal required instruction set is SSE2.
    assert(compiler->getSIMDSupportLevel() >= SIMD_SSE2_Supported);

    instruction result = INS_invalid;
    switch (intrinsicId)
    {
        case SIMDIntrinsicConvertToSingle:
            result = INS_cvtdq2ps;
            break;

        case SIMDIntrinsicConvertToDouble:
            assert(baseType == TYP_LONG);
            result = INS_cvtsi2sd;
            break;

        case SIMDIntrinsicConvertToInt32:
            assert(baseType == TYP_FLOAT);
            result = INS_cvttps2dq;
            break;

        case SIMDIntrinsicConvertToInt64:
            assert(baseType == TYP_DOUBLE);
            result = INS_cvttsd2si;
            break;

        case SIMDIntrinsicUpperSave:
            result = INS_vextractf128;
            break;

        case SIMDIntrinsicUpperRestore:
            result = INS_insertps;
            break;

        default:
            assert(!"Unsupported SIMD intrinsic");
            unreached();
    }

    noway_assert(result != INS_invalid);
    return result;
}

//----------------------------------------------------------------------------------
// genSIMDIntrinsic32BitConvert: Generate code for 32-bit SIMD Convert (int/uint <-> float)
//
// Arguments:
//    simdNode - The GT_SIMD node
//
// Return Value:
//    None.
//
void CodeGen::genSIMDIntrinsic32BitConvert(GenTreeSIMD* simdNode)
{
    SIMDIntrinsicID intrinsicID = simdNode->gtSIMDIntrinsicID;
    assert((intrinsicID == SIMDIntrinsicConvertToSingle) || (intrinsicID == SIMDIntrinsicConvertToInt32));

    GenTree*  op1       = simdNode->GetOp(0);
    var_types baseType  = simdNode->gtSIMDBaseType;
    regNumber targetReg = simdNode->GetRegNum();
    assert(targetReg != REG_NA);
    var_types targetType = simdNode->TypeGet();

    regNumber   op1Reg = genConsumeReg(op1);
    instruction ins    = getOpForSIMDIntrinsic(simdNode->gtSIMDIntrinsicID, baseType);
    if (intrinsicID == SIMDIntrinsicConvertToSingle && baseType == TYP_UINT)
    {
        regNumber tmpIntReg = simdNode->GetSingleTempReg(RBM_ALLINT);
        regNumber tmpReg    = simdNode->ExtractTempReg(RBM_ALLFLOAT);
        regNumber tmpReg2   = simdNode->GetSingleTempReg(RBM_ALLFLOAT);
        assert(tmpReg != op1Reg && tmpReg2 != op1Reg);

        // We will generate the following:
        //   vmovdqu  tmpReg2, op1Reg           (copy the src and put it into tmpReg2)
        //   vmovdqu  targetReg, op1Reg         (copy the src and put it into targetReg)
        //   vpsrld   targetReg, 16             (get upper 16 bits of src and put it into targetReg)
        //   vpslld   tmpReg2, 16
        //   vpsrld   tmpReg2, 16               (get lower 16 bits of src and put it into tmpReg2)
        //   mov      tmpIntReg, 0x5300000053000000
        //   vmovd    tmpReg, tmpIntReg
        //   vpbroadcastd tmpReg, tmpReg        (build mask for converting upper 16 bits of src)
        //   vorps    targetReg, tmpReg
        //   vsubps   targetReg, tmpReg         (convert upper 16 bits of src and put it into targetReg)
        //   vcvtdq2ps tmpReg2, tmpReg2         (convert lower 16 bits of src and put it into tmpReg2)
        //   vaddps   targetReg, tmpReg2        (add upper 16 bits and lower 16 bits)
        inst_Mov(targetType, tmpReg2, op1Reg, /* canSkip */ false);
        inst_Mov(targetType, targetReg, op1Reg, /* canSkip */ true);

        // prepare upper 16 bits
        GetEmitter()->emitIns_R_I(INS_psrld, emitActualTypeSize(targetType), targetReg, 16);

        // prepare lower 16 bits
        GetEmitter()->emitIns_R_I(INS_pslld, emitActualTypeSize(targetType), tmpReg2, 16);
        GetEmitter()->emitIns_R_I(INS_psrld, emitActualTypeSize(targetType), tmpReg2, 16);

// prepare mask
#ifdef TARGET_AMD64
        GetEmitter()->emitIns_R_I(INS_mov, EA_8BYTE, tmpIntReg, (ssize_t)0X5300000053000000);
        inst_Mov(targetType, tmpReg, tmpIntReg, /* canSkip */ false, emitActualTypeSize(TYP_ULONG));
#else
        if (compiler->getSIMDSupportLevel() == SIMD_AVX2_Supported)
        {
            GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, tmpIntReg, (ssize_t)0X53000000);
            inst_Mov(targetType, tmpReg, tmpIntReg, /* canSkip */ false, emitActualTypeSize(TYP_UINT));
        }
        else
        {
            GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, tmpIntReg, (ssize_t)0X00005300);
            inst_RV_RV(INS_pxor, tmpReg, tmpReg, targetType, emitActualTypeSize(targetType));
            GetEmitter()->emitIns_R_R_I(INS_pinsrw, emitTypeSize(TYP_INT), tmpReg, tmpIntReg, 1);
            GetEmitter()->emitIns_R_R_I(INS_pinsrw, emitTypeSize(TYP_INT), tmpReg, tmpIntReg, 3);
        }
#endif
        if (compiler->getSIMDSupportLevel() == SIMD_AVX2_Supported)
        {
            inst_RV_RV(INS_vpbroadcastd, tmpReg, tmpReg, targetType, emitActualTypeSize(targetType));
        }
        else
        {
            inst_RV_RV(INS_movlhps, tmpReg, tmpReg, targetType, emitActualTypeSize(targetType));
        }

        // convert upper 16 bits
        inst_RV_RV(INS_orps, targetReg, tmpReg, targetType, emitActualTypeSize(targetType));
        inst_RV_RV(INS_subps, targetReg, tmpReg, targetType, emitActualTypeSize(targetType));

        // convert lower 16 bits
        inst_RV_RV(ins, tmpReg2, tmpReg2, targetType, emitActualTypeSize(targetType));

        // add lower 16 bits and upper 16 bits
        inst_RV_RV(INS_addps, targetReg, tmpReg2, targetType, emitActualTypeSize(targetType));
    }
    else
    {
        inst_RV_RV(ins, targetReg, op1Reg, targetType, emitActualTypeSize(targetType));
    }
    genProduceReg(simdNode);
}

//----------------------------------------------------------------------------------
// genSIMDLo64BitConvert: Generate code to convert lower-most 64-bit item (long <--> double)
//
// Arguments:
//    intrinsicID      the SIMD intrinsic ID
//    simdType         the SIMD node type
//    baseType         the base type of value to be converted
//    tmpReg           the tmp reg
//    tmpIntReg        the tmp integer reg
//    targetReg        the target reg
//
// Return Value:
//    None.
//
void CodeGen::genSIMDLo64BitConvert(SIMDIntrinsicID intrinsicID,
                                    var_types       simdType,
                                    var_types       baseType,
                                    regNumber       tmpReg,
                                    regNumber       tmpIntReg,
                                    regNumber       targetReg)
{
    instruction ins = getOpForSIMDIntrinsic(intrinsicID, baseType);
    if (intrinsicID == SIMDIntrinsicConvertToDouble)
    {
        inst_Mov(TYP_LONG, tmpIntReg, tmpReg, /* canSkip */ false);
        inst_RV_RV(ins, targetReg, tmpIntReg, baseType, emitActualTypeSize(baseType));
    }
    else
    {
        inst_RV_RV(ins, tmpIntReg, tmpReg, baseType, emitActualTypeSize(baseType));
        inst_Mov(simdType, targetReg, tmpIntReg, /* canSkip */ false, emitActualTypeSize(TYP_LONG));
    }
}

//----------------------------------------------------------------------------------
// genSIMDIntrinsic64BitConvert: Generate code for 64-bit SIMD Convert (long/ulong <-> double)
//
// Arguments:
//    simdNode - The GT_SIMD node
//
// Notes:
//    There are no instructions for converting to/from 64-bit integers, so for these we
//    do the conversion an element at a time.
//
void CodeGen::genSIMDIntrinsic64BitConvert(GenTreeSIMD* simdNode)
{
    SIMDIntrinsicID intrinsicID = simdNode->gtSIMDIntrinsicID;
    assert((intrinsicID == SIMDIntrinsicConvertToDouble) || (intrinsicID == SIMDIntrinsicConvertToInt64));

    GenTree*  op1       = simdNode->GetOp(0);
    var_types baseType  = simdNode->gtSIMDBaseType;
    regNumber targetReg = simdNode->GetRegNum();
    assert(targetReg != REG_NA);
    var_types simdType  = simdNode->TypeGet();
    regNumber op1Reg    = genConsumeReg(op1);
    regNumber tmpIntReg = simdNode->GetSingleTempReg(RBM_ALLINT);
    regNumber tmpReg;
    regNumber tmpReg2;
    regNumber tmpReg3;
    SIMDLevel level = compiler->getSIMDSupportLevel();

#ifdef TARGET_X86
    if (baseType == TYP_LONG)
    {
        tmpReg  = simdNode->ExtractTempReg(RBM_ALLFLOAT);
        tmpReg2 = simdNode->ExtractTempReg(RBM_ALLFLOAT);
        tmpReg3 = simdNode->GetSingleTempReg(RBM_ALLFLOAT);
        assert(tmpReg != op1Reg && tmpReg2 != op1Reg && tmpReg3 != op1Reg);
    }
    else
#endif
        if (level == SIMD_AVX2_Supported || (baseType == TYP_ULONG))
    {
        tmpReg  = simdNode->ExtractTempReg(RBM_ALLFLOAT);
        tmpReg2 = simdNode->GetSingleTempReg(RBM_ALLFLOAT);
        tmpReg3 = REG_NA;
        assert(tmpReg != op1Reg && tmpReg2 != op1Reg);
    }
    else
    {
        tmpReg = simdNode->GetSingleTempReg(RBM_ALLFLOAT);
        assert(tmpReg != op1Reg);
        tmpReg2 = REG_NA;
        tmpReg3 = REG_NA;
    }

    if ((intrinsicID == SIMDIntrinsicConvertToDouble) && (baseType == TYP_ULONG))
    {
        // We will generate the following
        //   vmovdqu  tmpReg2, op1Reg               (copy the src and put it into tmpReg2)
        //   vmovdqu  targetReg, op1Reg             (copy the src and put it into targetReg)
        //   vpsrlq   targetReg, 32                 (get upper 32 bits of src and put it into targetReg)
        //   vpsllq   tmpReg2, 32
        //   vpsrlq   tmpReg2, 32                   (get lower 32 bits of src and put it into tmpReg2)
        //   mov      tmpIntReg, 0x4530000000000000
        //   vmovd    tmpReg, tmpIntReg
        //   vpbroadcastq tmpReg, tmpReg            (build mask for upper 32 bits of src)
        //   vorpd    targetReg, tmpReg
        //   vsubpd   targetReg, tmpReg             (convert upper 32 bits of src and put it into targetReg)
        //   mov      tmpIntReg, 0x4330000000000000
        //   vmovd    tmpReg, tmpIntReg
        //   vpbroadcastq tmpReg, tmpReg            (build mask for lower 32 bits of src)
        //   vorpd    tmpReg2, tmpReg
        //   vsubpd   tmpReg2, tmpReg               (convert lower 32 bits of src and put it into tmpReg2)
        //   vaddpd   targetReg, tmpReg2            (add upper 32 bits and lower 32 bits together)
        inst_Mov(simdType, tmpReg2, op1Reg, /* canSkip */ false);
        inst_Mov(simdType, targetReg, op1Reg, /* canSkip */ true);

        // prepare upper 32 bits
        GetEmitter()->emitIns_R_I(INS_psrlq, emitActualTypeSize(simdType), targetReg, 32);

        // prepare lower 32 bits
        GetEmitter()->emitIns_R_I(INS_psllq, emitActualTypeSize(simdType), tmpReg2, 32);
        GetEmitter()->emitIns_R_I(INS_psrlq, emitActualTypeSize(simdType), tmpReg2, 32);

// prepare mask for converting upper 32 bits
#ifdef TARGET_AMD64
        GetEmitter()->emitIns_R_I(INS_mov, EA_8BYTE, tmpIntReg, (ssize_t)0X4530000000000000);
        inst_Mov(simdType, tmpReg, tmpIntReg, /* canSkip */ false, emitActualTypeSize(TYP_ULONG));
#else
        GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, tmpIntReg, (ssize_t)0X45300000);
        inst_Mov(simdType, tmpReg, tmpIntReg, /* canSkip */ false, emitActualTypeSize(TYP_UINT));
        GetEmitter()->emitIns_R_I(INS_pslldq, EA_16BYTE, tmpReg, 4);
#endif
        if (level == SIMD_AVX2_Supported)
        {
            inst_RV_RV(INS_vpbroadcastq, tmpReg, tmpReg, simdType, emitActualTypeSize(simdType));
        }
        else
        {
            inst_RV_RV(INS_movlhps, tmpReg, tmpReg, simdType, emitActualTypeSize(simdType));
        }

        // convert upper 32 bits
        inst_RV_RV(INS_orpd, targetReg, tmpReg, simdType, emitActualTypeSize(simdType));
        inst_RV_RV(INS_subpd, targetReg, tmpReg, simdType, emitActualTypeSize(simdType));

// prepare mask for converting lower 32 bits
#ifdef TARGET_AMD64
        GetEmitter()->emitIns_R_I(INS_mov, EA_8BYTE, tmpIntReg, (ssize_t)0X4330000000000000);
        inst_Mov(simdType, tmpReg, tmpIntReg, /* canSkip */ false, emitActualTypeSize(TYP_ULONG));
#else
        GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, tmpIntReg, (ssize_t)0X43300000);
        inst_Mov(simdType, tmpReg, tmpIntReg, /* canSkip */ false, emitActualTypeSize(TYP_UINT));
        GetEmitter()->emitIns_R_I(INS_pslldq, EA_16BYTE, tmpReg, 4);
#endif
        if (level == SIMD_AVX2_Supported)
        {
            inst_RV_RV(INS_vpbroadcastq, tmpReg, tmpReg, simdType, emitActualTypeSize(simdType));
        }
        else
        {
            inst_RV_RV(INS_movlhps, tmpReg, tmpReg, simdType, emitActualTypeSize(simdType));
        }

        // convert lower 32 bits
        inst_RV_RV(INS_orpd, tmpReg2, tmpReg, simdType, emitActualTypeSize(simdType));
        inst_RV_RV(INS_subpd, tmpReg2, tmpReg, simdType, emitActualTypeSize(simdType));

        // add lower 32 bits and upper 32 bits
        inst_RV_RV(INS_addpd, targetReg, tmpReg2, simdType, emitActualTypeSize(simdType));
    }
    else if ((intrinsicID == SIMDIntrinsicConvertToDouble) && (baseType == TYP_LONG))
    {
#ifdef TARGET_AMD64
        if (level == SIMD_AVX2_Supported)
        {
            // Extract the high 16-bits
            GetEmitter()->emitIns_R_R_I(INS_vextracti128, EA_32BYTE, tmpReg, op1Reg, 0x01);

            // Put v[3] (the high-order element) in tmpReg2 and convert it.
            inst_Mov(simdType, tmpReg2, tmpReg, /* canSkip */ false);
            GetEmitter()->emitIns_R_I(INS_psrldq, emitActualTypeSize(simdType), tmpReg2, 8);
            genSIMDLo64BitConvert(intrinsicID, simdType, baseType, tmpReg2, tmpIntReg, tmpReg2);

            // Shift the resulting 64-bits left.
            GetEmitter()->emitIns_R_I(INS_pslldq, emitActualTypeSize(simdType), tmpReg2, 8);

            // Convert v[2], in the lo bits of tmpReg.
            // For the convert to double, the convert preserves the upper bits in tmpReg2.
            // For the integer convert, we have to put it in tmpReg and or it in, since movd clears the upper bits.
            genSIMDLo64BitConvert(intrinsicID, simdType, baseType, tmpReg, tmpIntReg, tmpReg2);
        }

        // Put v[1] in tmpReg.
        inst_Mov(simdType, tmpReg, op1Reg, /* canSkip */ false);
        GetEmitter()->emitIns_R_I(INS_psrldq, emitActualTypeSize(simdType), tmpReg, 8);

        // At this point we have v[1] in the low-order 64-bits of tmpReg. Convert it.
        genSIMDLo64BitConvert(intrinsicID, simdType, baseType, tmpReg, tmpIntReg, tmpReg);

        // Shift the resulting 64-bits left.
        GetEmitter()->emitIns_R_I(INS_pslldq, emitActualTypeSize(simdType), tmpReg, 8);

        // Convert the lo 64-bits into targetReg
        genSIMDLo64BitConvert(intrinsicID, simdType, baseType, op1Reg, tmpIntReg, tmpReg);

        // Merge or copy the results (only at this point are we done with op1Reg).
        inst_Mov(simdType, targetReg, tmpReg, /* canSkip */ true);

        if (level == SIMD_AVX2_Supported)
        {
            GetEmitter()->emitIns_R_R_I(INS_vinsertf128, EA_32BYTE, targetReg, tmpReg2, 0x01);
        }
#else
        // get the sign bit and put it in tmpReg3
        inst_Mov(simdType, tmpReg3, op1Reg, /* canSkip */ false);
        GetEmitter()->emitIns_R_I(INS_psrlq, emitActualTypeSize(simdType), tmpReg3, 63);
        GetEmitter()->emitIns_R_I(INS_psllq, emitActualTypeSize(simdType), tmpReg3, 63);

        // get the absolute value of src and put it into tmpReg2 and targetReg
        inst_Mov(simdType, tmpReg2, op1Reg, /* canSkip */ false);
        GetEmitter()->emitIns_R_R_I(INS_pshufd, emitActualTypeSize(simdType), tmpReg, op1Reg, (int8_t)SHUFFLE_WWYY);
        GetEmitter()->emitIns_R_I(INS_psrad, emitActualTypeSize(simdType), tmpReg, 32);
        inst_RV_RV(INS_pxor, tmpReg2, tmpReg, baseType, emitActualTypeSize(simdType));
        inst_RV_RV(INS_psubq, tmpReg2, tmpReg, baseType, emitActualTypeSize(simdType));
        inst_Mov(simdType, targetReg, tmpReg2, /* canSkip */ false);

        // prepare upper 32 bits
        GetEmitter()->emitIns_R_I(INS_psrlq, emitActualTypeSize(simdType), targetReg, 32);

        // prepare lower 32 bits
        GetEmitter()->emitIns_R_I(INS_psllq, emitActualTypeSize(simdType), tmpReg2, 32);
        GetEmitter()->emitIns_R_I(INS_psrlq, emitActualTypeSize(simdType), tmpReg2, 32);

        // prepare mask for converting upper 32 bits
        GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, tmpIntReg, (ssize_t)0X45300000);
        inst_Mov(simdType, tmpReg, tmpIntReg, /* canSkip */ false, emitActualTypeSize(TYP_UINT));
        GetEmitter()->emitIns_R_I(INS_pslldq, EA_16BYTE, tmpReg, 4);

        if (level == SIMD_AVX2_Supported)
        {
            inst_RV_RV(INS_vpbroadcastq, tmpReg, tmpReg, simdType, emitActualTypeSize(simdType));
        }
        else
        {
            inst_RV_RV(INS_movlhps, tmpReg, tmpReg, simdType, emitActualTypeSize(simdType));
        }

        // convert upper 32 bits
        inst_RV_RV(INS_orpd, targetReg, tmpReg, simdType, emitActualTypeSize(simdType));
        inst_RV_RV(INS_subpd, targetReg, tmpReg, simdType, emitActualTypeSize(simdType));

        // prepare mask for converting lower 32 bits
        GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, tmpIntReg, (ssize_t)0X43300000);
        inst_Mov(simdType, tmpReg, tmpIntReg, /* canSkip */ false, emitActualTypeSize(TYP_UINT));
        GetEmitter()->emitIns_R_I(INS_pslldq, EA_16BYTE, tmpReg, 4);

        if (level == SIMD_AVX2_Supported)
        {
            inst_RV_RV(INS_vpbroadcastq, tmpReg, tmpReg, simdType, emitActualTypeSize(simdType));
        }
        else
        {
            inst_RV_RV(INS_movlhps, tmpReg, tmpReg, simdType, emitActualTypeSize(simdType));
        }

        // convert lower 32 bits
        inst_RV_RV(INS_orpd, tmpReg2, tmpReg, simdType, emitActualTypeSize(simdType));
        inst_RV_RV(INS_subpd, tmpReg2, tmpReg, simdType, emitActualTypeSize(simdType));

        // add lower 32 bits and upper 32 bits
        inst_RV_RV(INS_addpd, targetReg, tmpReg2, simdType, emitActualTypeSize(simdType));

        // add sign bit
        inst_RV_RV(INS_por, targetReg, tmpReg3, simdType, emitActualTypeSize(simdType));
#endif
    }
    else
    {
        if (level == SIMD_AVX2_Supported)
        {
            // Extract the high 16-bits
            GetEmitter()->emitIns_R_R_I(INS_vextractf128, EA_32BYTE, tmpReg, op1Reg, 0x01);

            // Put v[3] (the high-order element) in tmpReg2 and convert it.
            inst_Mov(simdType, tmpReg2, tmpReg, /* canSkip */ false);
            GetEmitter()->emitIns_R_I(INS_psrldq, emitActualTypeSize(simdType), tmpReg2, 8);
            genSIMDLo64BitConvert(intrinsicID, simdType, baseType, tmpReg2, tmpIntReg, tmpReg2);

            // Shift the resulting 64-bits left.
            GetEmitter()->emitIns_R_I(INS_pslldq, emitActualTypeSize(simdType), tmpReg2, 8);

            // Convert v[2], in the lo bits of tmpReg.
            // For the convert to double, the convert preserves the upper bits in tmpReg2.
            // For the integer convert, we have to put it in tmpReg and or it in, since movd clears the upper bits.
            genSIMDLo64BitConvert(intrinsicID, simdType, baseType, tmpReg, tmpIntReg, tmpReg);
            inst_RV_RV(INS_por, tmpReg2, tmpReg, simdType, emitActualTypeSize(simdType));
        }

        // Put v[1] in tmpReg.
        inst_Mov(simdType, tmpReg, op1Reg, /* canSkip */ false);
        GetEmitter()->emitIns_R_I(INS_psrldq, emitActualTypeSize(simdType), tmpReg, 8);

        // At this point we have v[1] in the low-order 64-bits of tmpReg. Convert it.
        genSIMDLo64BitConvert(intrinsicID, simdType, baseType, tmpReg, tmpIntReg, tmpReg);

        // Shift the resulting 64-bits left.
        GetEmitter()->emitIns_R_I(INS_pslldq, emitActualTypeSize(simdType), tmpReg, 8);

        // Convert the lo 64-bits into targetReg
        genSIMDLo64BitConvert(intrinsicID, simdType, baseType, op1Reg, tmpIntReg, targetReg);

        // Merge or copy the results (only at this point are we done with op1Reg).
        assert(tmpReg != targetReg);
        inst_RV_RV(INS_por, targetReg, tmpReg, simdType, emitActualTypeSize(simdType));
        if (level == SIMD_AVX2_Supported)
        {
            GetEmitter()->emitIns_R_R_I(INS_vinserti128, EA_32BYTE, targetReg, tmpReg2, 0x01);
        }
    }
    genProduceReg(simdNode);
}

//--------------------------------------------------------------------------------
// genSIMDExtractUpperHalf: Generate code to extract the upper half of a SIMD register
//
// Arguments:
//    simdNode - The GT_SIMD node
//
// Notes:
//    This is used for the WidenHi intrinsic to extract the upper half.
//    On SSE*, this is 8 bytes, and on AVX2 it is 16 bytes.
//
void CodeGen::genSIMDExtractUpperHalf(GenTreeSIMD* simdNode, regNumber srcReg, regNumber tgtReg)
{
    var_types simdType = simdNode->TypeGet();
    emitAttr  emitSize = emitActualTypeSize(simdType);
    if (compiler->getSIMDSupportLevel() == SIMD_AVX2_Supported)
    {
        instruction extractIns = varTypeIsFloating(simdNode->gtSIMDBaseType) ? INS_vextractf128 : INS_vextracti128;
        GetEmitter()->emitIns_R_R_I(extractIns, EA_32BYTE, tgtReg, srcReg, 0x01);
    }
    else
    {
        inst_Mov(simdType, tgtReg, srcReg, /* canSkip */ true);
        GetEmitter()->emitIns_R_I(INS_psrldq, emitSize, tgtReg, 8);
    }
}

//-----------------------------------------------------------------------------
// genStoreSIMD12: Store a TYP_SIMD12 (i.e. Vector3) to memory.
// Since Vector3 is not a hardware supported write size, it is performed
// as two writes: 8 byte followed by 4-byte.
//
void CodeGen::genStoreSIMD12(const GenAddrMode& dst, GenTree* value, regNumber tmpReg)
{
    if (value->isContained())
    {
        GenAddrMode src(value, this);

#ifdef TARGET_64BIT
        inst_R_AM(INS_mov, EA_8BYTE, tmpReg, src, 0);
        inst_AM_R(INS_mov, EA_8BYTE, tmpReg, dst, 0);
        inst_R_AM(INS_mov, EA_4BYTE, tmpReg, src, 8);
        inst_AM_R(INS_mov, EA_4BYTE, tmpReg, dst, 8);
#else
        inst_R_AM(INS_movsdsse2, EA_8BYTE, tmpReg, src, 0);
        inst_AM_R(INS_movsdsse2, EA_8BYTE, tmpReg, dst, 0);
        inst_R_AM(INS_movss, EA_4BYTE, tmpReg, src, 8);
        inst_AM_R(INS_movss, EA_4BYTE, tmpReg, dst, 8);
#endif
        return;
    }

    regNumber valueReg = genConsumeReg(value);

    inst_AM_R(INS_movsdsse2, EA_8BYTE, valueReg, dst, 0);

    if (value->IsHWIntrinsicZero())
    {
        tmpReg = valueReg;
    }
    else
    {
        GetEmitter()->emitIns_R_R(INS_movhlps, EA_16BYTE, tmpReg, valueReg);
    }

    inst_AM_R(INS_movss, EA_4BYTE, tmpReg, dst, 8);
}

//-----------------------------------------------------------------------------
// genLoadSIMD12: Load a TYP_SIMD12 (i.e. Vector3) value from memory.
//
void CodeGen::genLoadSIMD12(GenTree* load)
{
    GenAddrMode src(load, this);

    regNumber tmpReg = load->GetSingleTempReg();
    regNumber dstReg = load->GetRegNum();

    assert(tmpReg != dstReg);

    inst_R_AM(INS_movsdsse2, EA_8BYTE, dstReg, src, 0);
    inst_R_AM(INS_movss, EA_4BYTE, tmpReg, src, 8);
    GetEmitter()->emitIns_R_R(INS_movlhps, EA_16BYTE, dstReg, tmpReg);

    genProduceReg(load);
}

#ifdef TARGET_X86

//-----------------------------------------------------------------------------
// genStoreSIMD12ToStack: store a TYP_SIMD12 (i.e. Vector3) type field to the stack.
// Since Vector3 is not a hardware supported write size, it is performed
// as two stores: 8 byte followed by 4-byte. The stack is assumed to have
// already been adjusted.
//
void CodeGen::genStoreSIMD12ToStack(regNumber valueReg, regNumber tmpReg)
{
    assert(genIsValidFloatReg(valueReg));
    assert(genIsValidFloatReg(tmpReg));

    GetEmitter()->emitIns_AR_R(INS_movsdsse2, EA_8BYTE, valueReg, REG_SPBASE, 0);
    GetEmitter()->emitIns_R_R(INS_movhlps, EA_16BYTE, tmpReg, valueReg);
    GetEmitter()->emitIns_AR_R(INS_movss, EA_4BYTE, tmpReg, REG_SPBASE, 8);
}

#endif // TARGET_X86

//-----------------------------------------------------------------------------
// genSIMDIntrinsicUpperSave: save the upper half of a TYP_SIMD32 vector to
//                            the given register, if any, or to memory.
//
// Arguments:
//    simdNode - The GT_SIMD node
//
// Return Value:
//    None.
//
// Notes:
//    The upper half of all AVX registers is volatile, even the callee-save registers.
//    When a 32-byte SIMD value is live across a call, the register allocator will use this intrinsic
//    to cause the upper half to be saved.  It will first attempt to find another, unused, callee-save
//    register.  If such a register cannot be found, it will save the upper half to the upper half
//    of the localVar's home location.
//    (Note that if there are no caller-save registers available, the entire 32 byte
//    value will be spilled to the stack.)
//
void CodeGen::genSIMDIntrinsicUpperSave(GenTreeSIMD* simdNode)
{
    assert(simdNode->gtSIMDIntrinsicID == SIMDIntrinsicUpperSave);

    GenTree* op1 = simdNode->GetOp(0);
    assert(op1->IsLocal() && op1->TypeGet() == TYP_SIMD32);
    regNumber targetReg = simdNode->GetRegNum();
    regNumber op1Reg    = genConsumeReg(op1);
    assert(op1Reg != REG_NA);
    if (targetReg != REG_NA)
    {
        GetEmitter()->emitIns_R_R_I(INS_vextractf128, EA_32BYTE, targetReg, op1Reg, 0x01);
        genProduceReg(simdNode);
    }
    else
    {
        // The localVar must have a stack home.
        unsigned   varNum = op1->AsLclVarCommon()->GetLclNum();
        LclVarDsc* varDsc = compiler->lvaGetDesc(varNum);
        assert(varDsc->lvOnFrame);
        // We want to store this to the upper 16 bytes of this localVar's home.
        int offs = 16;

        GetEmitter()->emitIns_S_R_I(INS_vextractf128, EA_32BYTE, varNum, offs, op1Reg, 0x01);
    }
}

//-----------------------------------------------------------------------------
// genSIMDIntrinsicUpperRestore: Restore the upper half of a TYP_SIMD32 vector to
//                               the given register, if any, or to memory.
//
// Arguments:
//    simdNode - The GT_SIMD node
//
// Return Value:
//    None.
//
// Notes:
//    For consistency with genSIMDIntrinsicUpperSave, and to ensure that lclVar nodes always
//    have their home register, this node has its targetReg on the lclVar child, and its source
//    on the simdNode.
//
void CodeGen::genSIMDIntrinsicUpperRestore(GenTreeSIMD* simdNode)
{
    assert(simdNode->gtSIMDIntrinsicID == SIMDIntrinsicUpperRestore);

    GenTree* op1 = simdNode->GetOp(0);
    assert(op1->IsLocal() && op1->TypeGet() == TYP_SIMD32);
    regNumber srcReg    = simdNode->GetRegNum();
    regNumber lclVarReg = genConsumeReg(op1);
    assert(lclVarReg != REG_NA);
    if (srcReg != REG_NA)
    {
        GetEmitter()->emitIns_R_R_R_I(INS_vinsertf128, EA_32BYTE, lclVarReg, lclVarReg, srcReg, 0x01);
    }
    else
    {
        // The localVar must have a stack home.
        unsigned   varNum = op1->AsLclVarCommon()->GetLclNum();
        LclVarDsc* varDsc = compiler->lvaGetDesc(varNum);
        assert(varDsc->lvOnFrame);
        // We will load this from the upper 16 bytes of this localVar's home.
        int offs = 16;
        GetEmitter()->emitIns_R_R_S_I(INS_vinsertf128, EA_32BYTE, lclVarReg, lclVarReg, varNum, offs, 0x01);
    }
}

//------------------------------------------------------------------------
// genSIMDIntrinsic: Generate code for a SIMD Intrinsic.  This is the main
// routine which in turn calls appropriate genSIMDIntrinsicXXX() routine.
//
// Arguments:
//    simdNode - The GT_SIMD node
//
// Return Value:
//    None.
//
// Notes:
//    Currently, we only recognize SIMDVector<float> and SIMDVector<int>, and
//    a limited set of methods.
//
void CodeGen::genSIMDIntrinsic(GenTreeSIMD* simdNode)
{
    // NYI for unsupported base types
    if (!varTypeIsArithmetic(simdNode->gtSIMDBaseType))
    {
        noway_assert(!"SIMD intrinsic with unsupported base type.");
    }

    switch (simdNode->gtSIMDIntrinsicID)
    {
        case SIMDIntrinsicConvertToSingle:
        case SIMDIntrinsicConvertToInt32:
            genSIMDIntrinsic32BitConvert(simdNode);
            break;

        case SIMDIntrinsicConvertToDouble:
        case SIMDIntrinsicConvertToInt64:
            genSIMDIntrinsic64BitConvert(simdNode);
            break;

        case SIMDIntrinsicUpperSave:
            genSIMDIntrinsicUpperSave(simdNode);
            break;
        case SIMDIntrinsicUpperRestore:
            genSIMDIntrinsicUpperRestore(simdNode);
            break;

        default:
            noway_assert(!"Unimplemented SIMD intrinsic.");
            unreached();
    }
}

#endif // FEATURE_SIMD
#endif // TARGET_XARCH
