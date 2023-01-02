// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
/*****************************************************************************/

#ifndef _EMITINL_H_
#define _EMITINL_H_

#ifdef TARGET_XARCH

/* static */
inline bool emitter::instrIs3opImul(instruction ins)
{
#ifdef TARGET_X86
    return ((ins >= INS_imul_AX) && (ins <= INS_imul_DI));
#else // TARGET_AMD64
    return ((ins >= INS_imul_AX) && (ins <= INS_imul_15));
#endif
}

/* static */
inline bool emitter::instrIsExtendedReg3opImul(instruction ins)
{
#ifdef TARGET_X86
    return false;
#else // TARGET_AMD64
    return ((ins >= INS_imul_08) && (ins <= INS_imul_15));
#endif
}

/* static */
inline bool emitter::instrHasImplicitRegPairDest(instruction ins)
{
    return (ins == INS_mulEAX) || (ins == INS_imulEAX) || (ins == INS_div) || (ins == INS_idiv);
}

// Because we don't actually have support for encoding these 3-op
// multiplies we fake it with special opcodes.  Make sure they are
// contiguous.
/* static */
inline void emitter::check3opImulValues()
{
    assert(INS_imul_AX - INS_imul_AX == REG_EAX);
    assert(INS_imul_BX - INS_imul_AX == REG_EBX);
    assert(INS_imul_CX - INS_imul_AX == REG_ECX);
    assert(INS_imul_DX - INS_imul_AX == REG_EDX);
    assert(INS_imul_BP - INS_imul_AX == REG_EBP);
    assert(INS_imul_SI - INS_imul_AX == REG_ESI);
    assert(INS_imul_DI - INS_imul_AX == REG_EDI);
#ifdef TARGET_AMD64
    assert(INS_imul_08 - INS_imul_AX == REG_R8);
    assert(INS_imul_09 - INS_imul_AX == REG_R9);
    assert(INS_imul_10 - INS_imul_AX == REG_R10);
    assert(INS_imul_11 - INS_imul_AX == REG_R11);
    assert(INS_imul_12 - INS_imul_AX == REG_R12);
    assert(INS_imul_13 - INS_imul_AX == REG_R13);
    assert(INS_imul_14 - INS_imul_AX == REG_R14);
    assert(INS_imul_15 - INS_imul_AX == REG_R15);
#endif
}

/*****************************************************************************
 *
 *  Return the instruction that uses the given register in the imul instruction
 */

/* static */
inline instruction emitter::inst3opImulForReg(regNumber reg)
{
    assert(genIsValidIntReg(reg));

    instruction ins = instruction(reg + INS_imul_AX);
    check3opImulValues();
    assert(instrIs3opImul(ins));

    return ins;
}

/*****************************************************************************
 *
 *  Return the register which is used implicitly by the IMUL_REG instruction
 */

/* static */
inline regNumber emitter::inst3opImulReg(instruction ins)
{
    regNumber reg = ((regNumber)(ins - INS_imul_AX));

    assert(genIsValidIntReg(reg));

    /* Make sure we return the appropriate register */

    check3opImulValues();

    return reg;
}
#endif

#ifdef TARGET_XARCH
inline bool insIsCMOV(instruction ins)
{
    return ((ins >= INS_cmovo) && (ins <= INS_cmovg));
}
#endif

#ifndef JIT32_GCENCODER
template <typename Callback>
void emitter::EnumerateNoGCInsGroups(Callback callback)
{
    for (insGroup* ig = emitIGlist; ig != nullptr; ig = ig->igNext)
    {
        if ((ig->igFlags & IGF_NOGCINTERRUPT) != 0)
        {
            callback(ig->igFuncIdx, ig->igOffs, ig->igSize);
        }
    }
}
#else
template <typename Callback>
void emitter::EnumerateEpilogs(Callback callback)
{
    for (EpilogList* el = emitEpilogList; el != nullptr; el = el->elNext)
    {
        assert((el->elLoc.GetIG()->igFlags & IGF_EPILOG) != 0);

        callback(el->elLoc.CodeOffset(this));
    }
}
#endif // JIT32_GCENCODER

#endif //_EMITINL_H_
