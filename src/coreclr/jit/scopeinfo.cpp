// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "emit.h"
#include "codegen.h"

#if defined(DEBUG) || defined(LATE_DISASM)
static bool IsAmbientSP(RegNum reg)
{
    // TODO-MIKE-Review: This is rather dodgy. They've put JIT's RegNum in siVarLoc
    // even though it's really ICorDebugInfo's RegNum and REGNUM_AMBIENT_SP collides
    // with JIT vector registers. It just happens to work because we only need this
    // in cases where the register can only be one of the frame registers.
    return static_cast<ICorDebugInfo::RegNum>(reg) == ICorDebugInfo::REGNUM_AMBIENT_SP;
}
#endif

static RegNum MapToAmbientSP(RegNum reg, bool isFramePointerUsed)
{
    if (!isFramePointerUsed && (reg == REG_SPBASE))
    {
        reg = static_cast<RegNum>(ICorDebugInfo::REGNUM_AMBIENT_SP);
    }

    return reg;
}

DbgInfoVarLoc::DbgInfoVarLoc(const LclVarDsc* lcl, RegNum baseReg, int offset, bool isFramePointerUsed)
{
    if (lcl->lvIsInReg())
    {
        InitRegLocation(lcl, lcl->GetActualRegisterType(), baseReg, offset, isFramePointerUsed);
    }
    else
    {
        InitStackLocation(lcl, varActualType(lcl->GetType()), baseReg, offset, isFramePointerUsed);
    }
}

void DbgInfoVarLoc::InitStackLocation(
    const LclVarDsc* lcl, var_types type, RegNum baseReg, int offset, bool isFramePointerUsed)
{
    assert(offset != BAD_STK_OFFS);

    switch (type)
    {
        case TYP_LONG:
        case TYP_DOUBLE:
#ifndef TARGET_64BIT
            vlType             = VLT_STK2;
            vlStk2.vls2BaseReg = MapToAmbientSP(baseReg, isFramePointerUsed);
            vlStk2.vls2Offset  = offset;
            break;
#endif
        case TYP_INT:
        case TYP_REF:
        case TYP_BYREF:
        case TYP_FLOAT:
        case TYP_STRUCT:
        case TYP_BLK: // Needed because of the TYP_BLK stress mode
#ifdef FEATURE_SIMD
        case TYP_SIMD8:
        case TYP_SIMD12:
        case TYP_SIMD16:
        case TYP_SIMD32:
#endif
            vlType           = lcl->IsImplicitByRefParam() ? VLT_STK_BYREF : VLT_STK;
            vlStk.vlsBaseReg = MapToAmbientSP(baseReg, isFramePointerUsed);
            vlStk.vlsOffset  = offset;
            break;

        default:
            unreached();
    }
}

void DbgInfoVarLoc::SetStackLocation(RegNum baseReg, int offset)
{
    // TODO-MIKE-Review: What about LONG/DOUBLE on 32 bit targets (VLT_STK2)?
    // What about implicit byref params? Ultimately it's not clear why
    // CreatePrologDbgInfoRanges doesn't use the normal siVarLoc creation means.

    vlType           = VLT_STK;
    vlStk.vlsBaseReg = baseReg;
    vlStk.vlsOffset  = offset;
}

void DbgInfoVarLoc::InitRegLocation(
    const LclVarDsc* lcl, var_types type, RegNum baseReg, int offset, bool isFramePointerUsed)
{
    switch (type)
    {
        case TYP_LONG:
#ifndef TARGET_64BIT // TODO-MIKE-Review: This code is either dead or completely messed up.
            vlType                        = VLT_REG_STK;
            vlRegStk.vlrsReg              = lcl->GetRegNum();
            vlRegStk.vlrsStk.vlrssBaseReg = MapToAmbientSP(baseReg, isFramePointerUsed);
            vlRegStk.vlrsStk.vlrssOffset  = offset + 4;
            break;
#endif
        case TYP_INT:
        case TYP_REF:
        case TYP_BYREF:
            vlType       = VLT_REG;
            vlReg.vlrReg = lcl->GetRegNum();
            break;

        case TYP_FLOAT:
        case TYP_DOUBLE:
#ifdef FEATURE_SIMD
        case TYP_SIMD8:
        case TYP_SIMD12:
        case TYP_SIMD16:
        case TYP_SIMD32:
#endif
#ifdef TARGET_X86
            // TODO-MIKE-Review: Debugger doesn't support VLT_REG_FP on x86 so we use VLT_FPSTK,
            // even if the variable is not actually on the x87 FP stack. It doesn't really matter
            // as the debugger returns CORDBG_E_IL_VAR_NOT_AVAILABLE for VLT_FPSTK anyway.
            vlType         = VLT_FPSTK;
            vlFPstk.vlfReg = lcl->GetRegNum();
#else
            // TODO-MIKE-Review: It looks like for VLT_REG_FP the debugger expects the 0 based index
            // of the vector register in vlrReg (e.g. lcl->GetRegNum() - REG_XMM0).
            // Also note that on ARM the debugger always returns E_NOTIMPL.
            vlType       = VLT_REG_FP;
            vlReg.vlrReg = lcl->GetRegNum();
#endif
            break;

        default:
            unreached();
    }
}

void DbgInfoVarLoc::SetRegLocation(RegNum reg1)
{
    assert(isValidIntArgReg(reg1) || isValidFloatArgReg(reg1));

    // TODO-MIKE-Review: What about FP regs (VLT_REG_FP/VLT_FPSTK)?
    // Ultimately it's not clear why CreatePrologDbgInfoRanges doesn't
    // use the normal siVarLoc creation means.

    vlType       = VLT_REG;
    vlReg.vlrReg = reg1;
}

void DbgInfoVarLoc::SetRegLocation(RegNum reg1, RegNum reg2)
{
    assert(isValidIntArgReg(reg1) || isValidFloatArgReg(reg1));
    assert(isValidIntArgReg(reg2) || isValidFloatArgReg(reg2));

    // TODO-MIKE-Review: UNIX_AMD64_ABI can use both INT and FP
    // regs, while VLT_REG_REG seems to support only INT regs.

    vlType            = VLT_REG_REG;
    vlRegReg.vlrrReg1 = reg1;
    vlRegReg.vlrrReg2 = reg2;
}

bool operator==(const DbgInfoVarLoc& x, const DbgInfoVarLoc& y)
{
    if (x.vlType != y.vlType)
    {
        return false;
    }

    switch (x.vlType)
    {
        case DbgInfoVarLoc::VLT_STK:
        case DbgInfoVarLoc::VLT_STK_BYREF:
            return (x.vlStk.vlsBaseReg == y.vlStk.vlsBaseReg) && (x.vlStk.vlsOffset == y.vlStk.vlsOffset);
        case DbgInfoVarLoc::VLT_STK2:
            return (x.vlStk2.vls2BaseReg == y.vlStk2.vls2BaseReg) && (x.vlStk2.vls2Offset == y.vlStk2.vls2Offset);
        case DbgInfoVarLoc::VLT_REG:
        case DbgInfoVarLoc::VLT_REG_FP:
        case DbgInfoVarLoc::VLT_REG_BYREF:
            return x.vlReg.vlrReg == y.vlReg.vlrReg;
        case DbgInfoVarLoc::VLT_REG_REG:
            return (x.vlRegReg.vlrrReg1 == y.vlRegReg.vlrrReg1) && (x.vlRegReg.vlrrReg2 == y.vlRegReg.vlrrReg2);
        case DbgInfoVarLoc::VLT_REG_STK:
            return (x.vlRegStk.vlrsReg == y.vlRegStk.vlrsReg) &&
                   (x.vlRegStk.vlrsStk.vlrssBaseReg == y.vlRegStk.vlrsStk.vlrssBaseReg) &&
                   (x.vlRegStk.vlrsStk.vlrssOffset == y.vlRegStk.vlrsStk.vlrssOffset);
        case DbgInfoVarLoc::VLT_STK_REG:
            return (x.vlStkReg.vlsrReg == y.vlStkReg.vlsrReg) &&
                   (x.vlStkReg.vlsrStk.vlsrsBaseReg == y.vlStkReg.vlsrStk.vlsrsBaseReg) &&
                   (x.vlStkReg.vlsrStk.vlsrsOffset == y.vlStkReg.vlsrStk.vlsrsOffset);
        case DbgInfoVarLoc::VLT_FPSTK:
            return x.vlFPstk.vlfReg == y.vlFPstk.vlfReg;
        case DbgInfoVarLoc::VLT_FIXED_VA:
            return x.vlFixedVarArg.vlfvOffset == y.vlFixedVarArg.vlfvOffset;
        case DbgInfoVarLoc::VLT_COUNT:
        case DbgInfoVarLoc::VLT_INVALID:
            return true;
        default:
            unreached();
    }
}

bool operator!=(const DbgInfoVarLoc& x, const DbgInfoVarLoc& y)
{
    return !(x == y);
}

#ifdef DEBUG
void DbgInfoVarLoc::Dump(const char* suffix) const
{
    switch (vlType)
    {
        case VLT_REG:
        case VLT_REG_BYREF:
        case VLT_REG_FP:
            printf("%s", getRegName(vlReg.vlrReg));

            if (vlType == VLT_REG_BYREF)
            {
                printf(" byref");
            }
            break;

        case VLT_STK:
        case VLT_STK_BYREF:
            if (IsAmbientSP(vlStk.vlsBaseReg))
            {
                printf(STR_SPBASE "'[%d] (1 slot)", vlStk.vlsOffset);
            }
            else
            {
                printf("%s[%d] (1 slot)", getRegName(vlStk.vlsBaseReg), vlStk.vlsOffset);
            }

            if (vlType == VLT_REG_BYREF)
            {
                printf(" byref");
            }
            break;

#ifndef TARGET_AMD64
        case VLT_REG_REG:
            printf("%s-%s", getRegName(vlRegReg.vlrrReg1), getRegName(vlRegReg.vlrrReg2));
            break;

        case VLT_REG_STK:
            if (IsAmbientSP(vlRegStk.vlrsStk.vlrssBaseReg))
            {
                printf("%s-" STR_SPBASE "'[%d]", getRegName(vlRegStk.vlrsReg), vlRegStk.vlrsStk.vlrssOffset);
            }
            else
            {
                printf("%s-%s[%d]", getRegName(vlRegStk.vlrsReg), getRegName(vlRegStk.vlrsStk.vlrssBaseReg),
                       vlRegStk.vlrsStk.vlrssOffset);
            }
            break;

        case VLT_STK_REG:
            unreached();

        case VLT_STK2:
            if (IsAmbientSP(vlStk2.vls2BaseReg))
            {
                printf(STR_SPBASE "'[%d] (2 slots)", vlStk2.vls2Offset);
            }
            else
            {
                printf("%s[%d] (2 slots)", getRegName(vlStk2.vls2BaseReg), vlStk2.vls2Offset);
            }
            break;

        case VLT_FPSTK:
            printf("ST(L-%d)", vlFPstk.vlfReg);
            break;

        case VLT_FIXED_VA:
            printf("fxd_va[%d]", vlFixedVarArg.vlfvOffset);
            break;
#endif // !TARGET_AMD64

        default:
            unreached();
    }

    if (suffix != nullptr)
    {
        printf("%s", suffix);
    }
}
#endif

#ifdef LATE_DISASM
bool DbgInfoVarLoc::IsInReg(RegNum reg) const
{
    switch (vlType)
    {
        case VLT_STK:
        case VLT_STK2:
        case VLT_FPSTK:
            return false;
        case VLT_REG:
            return vlReg.vlrReg == reg;
        case VLT_REG_REG:
            return (vlRegReg.vlrrReg1 == reg) || (vlRegReg.vlrrReg2 == reg);
        case VLT_REG_STK:
            return vlRegStk.vlrsReg == reg;
        case VLT_STK_REG:
            return vlStkReg.vlsrReg == reg;
        default:
            assert(!"Bad locType");
            return false;
    }
}

bool DbgInfoVarLoc::IsOnStack(RegNum reg, int offset) const
{
    RegNum actualReg;

    switch (vlType)
    {
        case VLT_REG:
        case VLT_REG_FP:
        case VLT_REG_REG:
        case VLT_FPSTK:
            return false;
        case VLT_REG_STK:
            actualReg = IsAmbientSP(vlRegStk.vlrsStk.vlrssBaseReg) ? REG_SPBASE : vlRegStk.vlrsStk.vlrssBaseReg;
            return (actualReg == reg) && (vlRegStk.vlrsStk.vlrssOffset == offset);
        case VLT_STK_REG:
            actualReg = IsAmbientSP(vlStkReg.vlsrStk.vlsrsBaseReg) ? REG_SPBASE : vlStkReg.vlsrStk.vlsrsBaseReg;
            return (actualReg == reg) && (vlStkReg.vlsrStk.vlsrsOffset == offset);
        case VLT_STK:
            actualReg = IsAmbientSP(vlStk.vlsBaseReg) ? REG_SPBASE : vlStk.vlsBaseReg;
            return (actualReg == reg) && (vlStk.vlsOffset == offset);
        case VLT_STK2:
            actualReg = IsAmbientSP(vlStk2.vls2BaseReg) ? REG_SPBASE : vlStk2.vls2BaseReg;
            return (actualReg == reg) && ((vlStk2.vls2Offset == offset) || (vlStk2.vls2Offset == (offset - 4)));
        default:
            assert(!"Bad locType");
            return false;
    }
}
#endif // LATE_DISASM
