// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

// Classes to gather the Scope information from the local variable info.
// Translates the given LocalVarTab from IL instruction offsets into
// native code offsets.
//
// Debuggable code
//
// We break up blocks at the start and end IL ranges of the local variables.
// This is because IL offsets do not correspond exactly to native offsets
// except at block boundaries. No basic-blocks are deleted (not even
// unreachable), so there will not be any missing address-ranges, though the
// blocks themselves may not be ordered. (Also, internal blocks may be added).
// - At the start of each basic block, siBeginBlock() checks if any variables
//   are coming in scope, and adds an open scope to siOpenScopeList if needed.
// - At the end of each basic block, siEndBlock() checks if any variables
//   are going out of scope and moves the open scope from siOpenScopeLast
//   to siScopeList.
//
// Optimized code
//
// We cannot break up the blocks as this will produce different code under
// the debugger. Instead we try to do a best effort.
// - At the start of each basic block, siBeginBlock() adds open scopes
//   corresponding to block->bbLiveIn to siOpenScopeList. Also siUpdate()
//   is called to close scopes for variables which are not live anymore.
// - siEndBlock() closes scopes for any variables which go out of range
//   before bbCodeOffsEnd.
// - siCloseAllOpenScopes() closes any open scopes after all the blocks.
//   This should only be needed if some basic block are deleted/out of order,
//   etc.
// Also,
// - At every assignment to a variable, siCheckVarScope() adds an open scope
//   for the variable being assigned to.
// - UpdateLifeVar() calls siUpdate() which closes scopes for variables which
//   are not live anymore.

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

#ifdef LATE_DISASM
bool CodeGenInterface::siVarLoc::vlIsInReg(RegNum reg) const
{
    switch (vlType)
    {
        case CodeGenInterface::VLT_STK:
        case CodeGenInterface::VLT_STK2:
        case CodeGenInterface::VLT_FPSTK:
            return false;
        case CodeGenInterface::VLT_REG:
            return vlReg.vlrReg == reg;
        case CodeGenInterface::VLT_REG_REG:
            return (vlRegReg.vlrrReg1 == reg) || (vlRegReg.vlrrReg2 == reg);
        case CodeGenInterface::VLT_REG_STK:
            return vlRegStk.vlrsReg == reg;
        case CodeGenInterface::VLT_STK_REG:
            return vlStkReg.vlsrReg == reg;
        default:
            assert(!"Bad locType");
            return false;
    }
}

bool CodeGenInterface::siVarLoc::vlIsOnStack(RegNum reg, int32_t offset) const
{
    RegNum actualReg;

    switch (vlType)
    {
        case CodeGenInterface::VLT_REG:
        case CodeGenInterface::VLT_REG_FP:
        case CodeGenInterface::VLT_REG_REG:
        case CodeGenInterface::VLT_FPSTK:
            return false;
        case CodeGenInterface::VLT_REG_STK:
            actualReg = IsAmbientSP(vlRegStk.vlrsStk.vlrssBaseReg) ? REG_SPBASE : vlRegStk.vlrsStk.vlrssBaseReg;
            return (actualReg == reg) && (vlRegStk.vlrsStk.vlrssOffset == offset);
        case CodeGenInterface::VLT_STK_REG:
            actualReg = IsAmbientSP(vlStkReg.vlsrStk.vlsrsBaseReg) ? REG_SPBASE : vlStkReg.vlsrStk.vlsrsBaseReg;
            return (actualReg == reg) && (vlStkReg.vlsrStk.vlsrsOffset == offset);
        case CodeGenInterface::VLT_STK:
            actualReg = IsAmbientSP(vlStk.vlsBaseReg) ? REG_SPBASE : vlStk.vlsBaseReg;
            return (actualReg == reg) && (vlStk.vlsOffset == offset);
        case CodeGenInterface::VLT_STK2:
            actualReg = IsAmbientSP(vlStk2.vls2BaseReg) ? REG_SPBASE : vlStk2.vls2BaseReg;
            return (actualReg == reg) && ((vlStk2.vls2Offset == offset) || (vlStk2.vls2Offset == (offset - 4)));
        default:
            assert(!"Bad locType");
            return false;
    }
}
#endif // LATE_DISASM

void CodeGenInterface::siVarLoc::storeVariableInRegisters(RegNum reg1, RegNum reg2)
{
    if (reg2 == REG_NA)
    {
        vlType       = VLT_REG;
        vlReg.vlrReg = reg1;
    }
    else
    {
        vlType            = VLT_REG_REG;
        vlRegReg.vlrrReg1 = reg1;
        vlRegReg.vlrrReg2 = reg2;
    }
}

void CodeGenInterface::siVarLoc::storeVariableOnStack(RegNum stackBaseReg, int32_t stackOffset)
{
    vlType           = VLT_STK;
    vlStk.vlsBaseReg = stackBaseReg;
    vlStk.vlsOffset  = stackOffset;
}

bool CodeGenInterface::siVarLoc::Equals(const siVarLoc& x, const siVarLoc& y)
{
    if (x.vlType != y.vlType)
    {
        return false;
    }

    switch (x.vlType)
    {
        case VLT_STK:
        case VLT_STK_BYREF:
            return (x.vlStk.vlsBaseReg == y.vlStk.vlsBaseReg) && (x.vlStk.vlsOffset == y.vlStk.vlsOffset);
        case VLT_STK2:
            return (x.vlStk2.vls2BaseReg == y.vlStk2.vls2BaseReg) && (x.vlStk2.vls2Offset == y.vlStk2.vls2Offset);
        case VLT_REG:
        case VLT_REG_FP:
        case VLT_REG_BYREF:
            return x.vlReg.vlrReg == y.vlReg.vlrReg;
        case VLT_REG_REG:
            return (x.vlRegReg.vlrrReg1 == y.vlRegReg.vlrrReg1) && (x.vlRegReg.vlrrReg2 == y.vlRegReg.vlrrReg2);
        case VLT_REG_STK:
            return (x.vlRegStk.vlrsReg == y.vlRegStk.vlrsReg) &&
                   (x.vlRegStk.vlrsStk.vlrssBaseReg == y.vlRegStk.vlrsStk.vlrssBaseReg) &&
                   (x.vlRegStk.vlrsStk.vlrssOffset == y.vlRegStk.vlrsStk.vlrssOffset);
        case VLT_STK_REG:
            return (x.vlStkReg.vlsrReg == y.vlStkReg.vlsrReg) &&
                   (x.vlStkReg.vlsrStk.vlsrsBaseReg == y.vlStkReg.vlsrStk.vlsrsBaseReg) &&
                   (x.vlStkReg.vlsrStk.vlsrsOffset == y.vlStkReg.vlsrStk.vlsrsOffset);
        case VLT_FPSTK:
            return x.vlFPstk.vlfReg == y.vlFPstk.vlfReg;
        case VLT_FIXED_VA:
            return x.vlFixedVarArg.vlfvOffset == y.vlFixedVarArg.vlfvOffset;
        case VLT_COUNT:
        case VLT_INVALID:
            return true;
        default:
            unreached();
    }
}

void CodeGenInterface::siVarLoc::siFillStackVarLoc(
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

void CodeGenInterface::siVarLoc::siFillRegisterVarLoc(
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

CodeGenInterface::siVarLoc::siVarLoc(const LclVarDsc* lcl, RegNum baseReg, int offset, bool isFramePointerUsed)
{
    if (lcl->lvIsInReg())
    {
        siFillRegisterVarLoc(lcl, lcl->GetActualRegisterType(), baseReg, offset, isFramePointerUsed);
    }
    else
    {
        siFillStackVarLoc(lcl, varActualType(lcl->GetType()), baseReg, offset, isFramePointerUsed);
    }
}

CodeGenInterface::siVarLoc CodeGenInterface::getSiVarLoc(const LclVarDsc* lcl) const
{
    RegNum baseReg;
    signed offset = lcl->GetStackOffset();

    if (!lcl->lvFramePointerBased)
    {
        baseReg = REG_SPBASE;
#if !FEATURE_FIXED_OUT_ARGS
        offset += genStackLevel;
#endif
    }
    else
    {
        baseReg = REG_FPBASE;
    }

    return CodeGenInterface::siVarLoc(lcl, baseReg, offset, isFramePointerUsed());
}

#ifdef DEBUG
void CodeGenInterface::dumpSiVarLoc(const siVarLoc* varLoc)
{
    switch (varLoc->vlType)
    {
        case VLT_REG:
        case VLT_REG_BYREF:
        case VLT_REG_FP:
            printf("%s", getRegName(varLoc->vlReg.vlrReg));

            if (varLoc->vlType == VLT_REG_BYREF)
            {
                printf(" byref");
            }
            break;

        case VLT_STK:
        case VLT_STK_BYREF:
            if (IsAmbientSP(varLoc->vlStk.vlsBaseReg))
            {
                printf(STR_SPBASE "'[%d] (1 slot)", varLoc->vlStk.vlsOffset);
            }
            else
            {
                printf("%s[%d] (1 slot)", getRegName(varLoc->vlStk.vlsBaseReg), varLoc->vlStk.vlsOffset);
            }

            if (varLoc->vlType == VLT_REG_BYREF)
            {
                printf(" byref");
            }
            break;

#ifndef TARGET_AMD64
        case VLT_REG_REG:
            printf("%s-%s", getRegName(varLoc->vlRegReg.vlrrReg1), getRegName(varLoc->vlRegReg.vlrrReg2));
            break;

        case VLT_REG_STK:
            if (IsAmbientSP(varLoc->vlRegStk.vlrsStk.vlrssBaseReg))
            {
                printf("%s-" STR_SPBASE "'[%d]", getRegName(varLoc->vlRegStk.vlrsReg),
                       varLoc->vlRegStk.vlrsStk.vlrssOffset);
            }
            else
            {
                printf("%s-%s[%d]", getRegName(varLoc->vlRegStk.vlrsReg),
                       getRegName(varLoc->vlRegStk.vlrsStk.vlrssBaseReg), varLoc->vlRegStk.vlrsStk.vlrssOffset);
            }
            break;

        case VLT_STK_REG:
            unreached();

        case VLT_STK2:
            if (IsAmbientSP(varLoc->vlStk2.vls2BaseReg))
            {
                printf(STR_SPBASE "'[%d] (2 slots)", varLoc->vlStk2.vls2Offset);
            }
            else
            {
                printf("%s[%d] (2 slots)", getRegName(varLoc->vlStk2.vls2BaseReg), varLoc->vlStk2.vls2Offset);
            }
            break;

        case VLT_FPSTK:
            printf("ST(L-%d)", varLoc->vlFPstk.vlfReg);
            break;

        case VLT_FIXED_VA:
            printf("fxd_va[%d]", varLoc->vlFixedVarArg.vlfvOffset);
            break;
#endif // !TARGET_AMD64

        default:
            unreached();
    }
}
#endif
