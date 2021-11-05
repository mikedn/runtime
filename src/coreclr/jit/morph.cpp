// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                          Morph                                            XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#include "allocacheck.h" // for alloca

// Convert the given node into a call to the specified helper passing
// the given argument list. Also tries to fold constants.
GenTree* Compiler::fgMorphCastIntoHelper(GenTreeCast* cast, int helper)
{
    GenTree* src = cast->GetOp(0);

    if (src->OperIsConst())
    {
        GenTree* folded = gtFoldExprConst(cast); // This may not fold the constant (NaN ...)

        if (folded != cast)
        {
            return fgMorphTree(folded);
        }

        if (folded->OperIsConst())
        {
            return fgMorphConst(folded);
        }

        noway_assert(cast->OperIs(GT_CAST));
        noway_assert(cast->GetOp(0) == src);
    }

    if (src->TypeIs(TYP_FLOAT))
    {
        // All floating point cast helpers work only with DOUBLE.
        src = gtNewCastNode(TYP_DOUBLE, src, false, TYP_DOUBLE);
    }

    // GenTreeCast nodes are small so they cannot be converted to calls in place. It may
    // be possible to have the importer create large cast nodes as needed but the number
    // of cast nodes that need to be converted to helper calls is typically very small
    // (e.g. 0.03% in corelib x86) so it's not worth the risk. At least in theory, if 2
    // cast nodes somehow combine into one and one is large and the other small then the
    // combining code would need to be careful to preserve the large node, not the small
    // node. Cast morphing code is convoluted enough as it is.
    GenTree* call = new (this, LargeOpOpcode())
        GenTreeCast(cast->GetType(), src, cast->IsUnsigned(), cast->GetCastType() DEBUGARG(/*largeNode*/ true));
    INDEBUG(call->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)
    return fgMorphIntoHelperCall(call, helper, gtNewCallArgs(src));
}

/*****************************************************************************
 *
 *  Convert the given node into a call to the specified helper passing
 *  the given argument list.
 */

GenTree* Compiler::fgMorphIntoHelperCall(GenTree* tree, int helper, GenTreeCall::Use* args, bool morphArgs)
{
    // The helper call ought to be semantically equivalent to the original node, so preserve its VN.
    tree->ChangeOper(GT_CALL, GenTree::PRESERVE_VN);

    GenTreeCall* call = tree->AsCall();
    call->SetRetSigType(tree->GetType());
    call->SetRetLayout(nullptr);

    call->gtCallType            = CT_HELPER;
    call->gtCallMethHnd         = eeFindHelper(helper);
    call->gtCallThisArg         = nullptr;
    call->gtCallArgs            = args;
    call->gtCallLateArgs        = nullptr;
    call->fgArgInfo             = nullptr;
    call->gtCallMoreFlags       = GTF_CALL_M_EMPTY;
    call->gtInlineCandidateInfo = nullptr;
    call->gtControlExpr         = nullptr;

#if DEBUG
    // Helper calls are never candidates.
    call->gtInlineObservation = InlineObservation::CALLSITE_IS_CALL_TO_HELPER;

    call->callSig = nullptr;

#endif // DEBUG

#ifdef FEATURE_READYTORUN_COMPILER
    call->gtEntryPoint.addr       = nullptr;
    call->gtEntryPoint.accessType = IAT_VALUE;
#endif

    call->GetRetDesc()->Reset();
#ifndef TARGET_64BIT
    if (varTypeIsLong(tree->GetType()))
    {
        call->GetRetDesc()->InitializeLong();
    }
#endif

#if FEATURE_MULTIREG_RET
    call->ClearOtherRegs();
    call->ClearOtherRegFlags();
#endif

    if (tree->OperMayThrow(this))
    {
        tree->gtFlags |= GTF_EXCEPT;
    }
    else
    {
        tree->gtFlags &= ~GTF_EXCEPT;
    }
    tree->gtFlags |= GTF_CALL;

    for (GenTreeCall::Use& use : GenTreeCall::UseList(args))
    {
        tree->gtFlags |= (use.GetNode()->gtFlags & GTF_ALL_EFFECT);
    }

    /* Perform the morphing */

    if (morphArgs)
    {
        tree = fgMorphArgs(call);
    }

    return tree;
}

GenTree* Compiler::fgMorphCast(GenTreeCast* cast)
{
    GenTree*  src     = cast->GetOp(0);
    var_types srcType = varActualType(src->GetType());
    var_types dstType = cast->GetCastType();

    if ((dstType == TYP_FLOAT) && (srcType == TYP_DOUBLE) && src->OperIs(GT_CAST))
    {
        // Optimization: conv.r4(conv.r8(?)) -> conv.r4(d)
        // This happens semi-frequently because there is no IL 'conv.r4.un'

        cast->gtFlags &= ~GTF_UNSIGNED;
        cast->gtFlags |= src->gtFlags & GTF_UNSIGNED;
        src = src->AsCast()->GetOp(0);
        cast->SetOp(0, src);
        srcType = varActualType(src->GetType());
    }

    noway_assert(!varTypeIsGC(dstType));

    if (varTypeIsGC(srcType))
    {
        // We are casting away GC information. We would like to just change the type to int,
        // however this gives the emitter fits because it believes the variable is a GC
        // variable at the beginning of the instruction group, but is not turned non-gc by
        // the code generator we fix this by copying the GC pointer to a non-gc pointer temp.

        // We generate an assignment to native int and then do the cast from native int.
        // With this we avoid the gc problem and we allow casts to bytes, longs,  etc...
        unsigned lclNum = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("Cast away GC"));
        src->SetType(TYP_I_IMPL);
        GenTree* asg = gtNewAssignNode(gtNewLclvNode(lclNum, TYP_I_IMPL), src);
        src->SetType(srcType);
        src = gtNewLclvNode(lclNum, TYP_I_IMPL);
        src = gtNewCommaNode(asg, src);
        cast->SetOp(0, src);
        srcType = TYP_I_IMPL;
    }

    if (varTypeIsSmall(dstType) && (varTypeIsFloating(srcType)
#ifndef TARGET_64BIT
                                    || varTypeIsLong(srcType)
#endif
                                        ))
    {
        // CodeGen doesn't support casting from floating point types, or long types on
        // 32 bit targets, directly to small int types. Cast the source to INT first.

        src = gtNewCastNode(TYP_INT, src, cast->IsUnsigned(), TYP_INT);
        src->gtFlags |= (cast->gtFlags & (GTF_OVERFLOW | GTF_EXCEPT));
        cast->SetOp(0, src);
        srcType = TYP_INT;
    }

    if (varTypeIsFloating(srcType) && varTypeIsIntegral(dstType))
    {
        if (cast->gtOverflow())
        {
            switch (dstType)
            {
                case TYP_INT:
                    return fgMorphCastIntoHelper(cast, CORINFO_HELP_DBL2INT_OVF);
                case TYP_UINT:
                    return fgMorphCastIntoHelper(cast, CORINFO_HELP_DBL2UINT_OVF);
                case TYP_LONG:
                    return fgMorphCastIntoHelper(cast, CORINFO_HELP_DBL2LNG_OVF);
                case TYP_ULONG:
                    return fgMorphCastIntoHelper(cast, CORINFO_HELP_DBL2ULNG_OVF);
                default:
                    unreached();
            }
        }
        else
        {
            switch (dstType)
            {
                case TYP_INT:
                    break;
                case TYP_UINT:
#if !defined(TARGET_ARM64) && !defined(TARGET_ARM) && !defined(TARGET_AMD64)
                    return fgMorphCastIntoHelper(cast, CORINFO_HELP_DBL2UINT);
#endif
                    break;
                case TYP_LONG:
#if !defined(TARGET_ARM64) && !defined(TARGET_AMD64)
                    return fgMorphCastIntoHelper(cast, CORINFO_HELP_DBL2LNG);
#endif
                    break;
                case TYP_ULONG:
#if !defined(TARGET_ARM64)
                    return fgMorphCastIntoHelper(cast, CORINFO_HELP_DBL2ULNG);
#endif
                    break;
                default:
                    unreached();
            }
        }
    }
    else if (varTypeIsFloating(dstType))
    {
#if defined(TARGET_AMD64)
        if (cast->IsUnsigned())
        {
            // X64 doesn't have any instruction to cast FP types to unsigned types
            // but codegen handles the ULONG to DOUBLE/FLOAT case by adjusting the
            // result of a ULONG to DOUBLE/FLOAT cast. For UINT to DOUBLE/FLOAT we
            // need to first cast the source to LONG.

            if (srcType == TYP_INT)
            {
                src = gtNewCastNode(TYP_LONG, src, true, TYP_LONG);
                cast->SetOp(0, src);
                cast->gtFlags &= ~GTF_UNSIGNED;
                srcType = TYP_LONG;
            }
        }
#endif

#if defined(TARGET_X86)
        if (cast->IsUnsigned() && (srcType == TYP_INT))
        {
            // There is no support for UINT to FP casts so first cast the source
            // to LONG and then use a helper call to cast to FP.
            src = gtNewCastNode(TYP_LONG, src, true, TYP_LONG);
            cast->SetOp(0, src);
            cast->gtFlags &= ~GTF_UNSIGNED;
            srcType = TYP_LONG;
        }
#endif

#if defined(TARGET_X86) || defined(TARGET_ARM)
        if (srcType == TYP_LONG)
        {
            // We only have helpers for (U)LONG to DOUBLE casts, we may need an extra cast to FLOAT.
            cast->SetCastType(TYP_DOUBLE);

            GenTree* helper =
                fgMorphCastIntoHelper(cast, cast->IsUnsigned() ? CORINFO_HELP_ULNG2DBL : CORINFO_HELP_LNG2DBL);

            if (dstType == TYP_FLOAT)
            {
                helper = gtNewCastNode(TYP_FLOAT, helper, false, TYP_FLOAT);
                INDEBUG(helper->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)
            }

            return helper;
        }
#endif
    }
    else if ((srcType == TYP_LONG) && ((dstType == TYP_INT) || (dstType == TYP_UINT)))
    {
        // Look for narrowing casts ([u]long -> [u]int) and try to push them
        // down into the operand before morphing it.
        //
        // It doesn't matter if this is cast is from ulong or long (i.e. if
        // GTF_UNSIGNED is set) because the transformation is only applied to
        // overflow-insensitive narrowing casts, which always silently truncate.
        //
        // Note that casts from [u]long to small integer types are handled above.

        // As a special case, look for overflow-sensitive casts of an AND
        // expression, and see if the second operand is a small constant. Since
        // the result of an AND is bound by its smaller operand, it may be
        // possible to prove that the cast won't overflow, which will in turn
        // allow the cast's operand to be transformed.
        if (cast->gtOverflow() && src->OperIs(GT_AND))
        {
            GenTree* andOp2 = src->AsOp()->GetOp(1);

            // Special case to the special case: AND with a casted int.
            if (andOp2->OperIs(GT_CAST) && andOp2->AsCast()->GetOp(0)->OperIs(GT_CNS_INT))
            {
                // gtFoldExprConst will deal with whether the cast is signed or
                // unsigned, or overflow-sensitive.
                andOp2 = gtFoldExprConst(andOp2);
                src->AsOp()->SetOp(1, andOp2);
            }

            // Look for a constant less than 2^{32} for a cast to uint, or less
            // than 2^{31} for a cast to int.
            int maxWidth = (dstType == TYP_UINT) ? 32 : 31;

            if (andOp2->OperIs(GT_CNS_NATIVELONG) && ((andOp2->AsIntConCommon()->LngValue() >> maxWidth) == 0))
            {
                // This cast can't overflow.
                cast->gtFlags &= ~(GTF_OVERFLOW | GTF_EXCEPT);
            }
        }

        // Only apply this transformation during global morph,
        // when neither the cast node nor the oper node may throw an exception
        // based on the upper 32 bits.
        //
        if (fgGlobalMorph && !cast->gtOverflow() && !src->gtOverflowEx())
        {
            // For these operations the lower 32 bits of the result only depends
            // upon the lower 32 bits of the operands.
            //
            bool canPushCast = src->OperIs(GT_ADD, GT_SUB, GT_MUL, GT_AND, GT_OR, GT_XOR, GT_NOT, GT_NEG);

            // For long LSH cast to int, there is a discontinuity in behavior
            // when the shift amount is 32 or larger.
            //
            // CAST(INT, LSH(1LL, 31)) == LSH(1, 31)
            // LSH(CAST(INT, 1LL), CAST(INT, 31)) == LSH(1, 31)
            //
            // CAST(INT, LSH(1LL, 32)) == 0
            // LSH(CAST(INT, 1LL), CAST(INT, 32)) == LSH(1, 32) == LSH(1, 0) == 1
            //
            // So some extra validation is needed.
            //
            if (src->OperIs(GT_LSH))
            {
                GenTree* shiftAmount = src->AsOp()->GetOp(1);

                // Expose constant value for shift, if possible, to maximize the number
                // of cases we can handle.
                shiftAmount = gtFoldExpr(shiftAmount);
                src->AsOp()->SetOp(1, shiftAmount);

                // We may remorph the shift amount tree again later, so clear any morphed flag.
                INDEBUG(shiftAmount->gtDebugFlags &= ~GTF_DEBUG_NODE_MORPHED;)

                if (shiftAmount->IsIntegralConst())
                {
                    const ssize_t shiftAmountValue = shiftAmount->AsIntCon()->GetValue();

                    if ((shiftAmountValue >= 64) || (shiftAmountValue < 0))
                    {
                        // Shift amount is large enough or negative so result is undefined.
                        // Don't try to optimize.
                        assert(!canPushCast);
                    }
                    else if (shiftAmountValue >= 32)
                    {
                        // We know that we have a narrowing cast ([u]long -> [u]int)
                        // and that we are casting to a 32-bit value, which will result in zero.
                        //
                        // Check to see if we have any side-effects that we must keep
                        //
                        if ((cast->gtFlags & GTF_ALL_EFFECT) == 0)
                        {
                            // Result of the shift is zero.
                            DEBUG_DESTROY_NODE(cast);
                            return fgMorphTree(gtNewZeroConNode(TYP_INT));
                        }
                        else // We do have a side-effect
                        {
                            // We could create a GT_COMMA node here to keep the side-effect and return a zero
                            // Instead we just don't try to optimize this case.
                            canPushCast = false;
                        }
                    }
                    else
                    {
                        // Shift amount is positive and small enough that we can push the cast through.
                        canPushCast = true;
                    }
                }
                else
                {
                    // Shift amount is unknown. We can't optimize this case.
                    assert(!canPushCast);
                }
            }

            if (canPushCast)
            {
                DEBUG_DESTROY_NODE(cast);

                // Insert narrowing casts for op1 and op2.
                src->AsOp()->SetOp(0, gtNewCastNode(TYP_INT, src->AsOp()->GetOp(0), false, dstType));

                if (src->AsOp()->gtOp2 != nullptr)
                {
                    src->AsOp()->SetOp(1, gtNewCastNode(TYP_INT, src->AsOp()->GetOp(1), false, dstType));
                }

#ifndef TARGET_64BIT
                // Clear the GT_MUL_64RSLT if it is set.
                if (src->OperIs(GT_MUL) && ((src->gtFlags & GTF_MUL_64RSLT) != 0))
                {
                    src->gtFlags &= ~GTF_MUL_64RSLT;
                }
#endif

                // The operation now produces a 32-bit result.
                src->SetType(TYP_INT);

                // Remorph the new tree as the casts that we added may be folded away.
                return fgMorphTree(src);
            }
        }
    }

    src = fgMorphTree(src);
    cast->SetOp(0, src);

    cast->gtFlags &= ~GTF_CALL;
    cast->gtFlags &= ~GTF_ASG;

    if (!cast->gtOverflow())
    {
        cast->gtFlags &= ~GTF_EXCEPT;
    }

    cast->gtFlags |= (src->gtFlags & GTF_ALL_EFFECT);

    if (!gtIsActiveCSE_Candidate(cast) && !gtIsActiveCSE_Candidate(src))
    {
        srcType = src->GetType();

        // See if we can discard the cast
        if (varTypeIsIntegral(srcType) && varTypeIsIntegral(dstType))
        {
            if (cast->IsUnsigned() && !varTypeIsUnsigned(srcType))
            {
                if (varTypeIsSmall(srcType))
                {
                    // Small signed values are automatically sign extended to TYP_INT. If the cast is interpreting the
                    // resulting TYP_INT value as unsigned then the "sign" bits end up being "value" bits and srcType
                    // must be TYP_UINT, not the original small signed type. Otherwise "conv.ovf.i2.un(i1(-1))" is
                    // wrongly treated as a widening conversion from i1 to i2 when in fact it is a narrowing conversion
                    // from u4 to i2.
                    srcType = genActualType(srcType);
                }

                srcType = genUnsignedType(srcType);
            }

            if (srcType == dstType)
            {
                // Certainly if they are identical it is pointless
                goto REMOVE_CAST;
            }

            if (src->OperIs(GT_LCL_VAR) && varTypeIsSmall(dstType))
            {
                LclVarDsc* varDsc = lvaGetDesc(src->AsLclVar());
                if ((varDsc->GetType() == dstType) && varDsc->lvNormalizeOnStore())
                {
                    goto REMOVE_CAST;
                }
            }

            bool     unsignedSrc = varTypeIsUnsigned(srcType);
            bool     unsignedDst = varTypeIsUnsigned(dstType);
            bool     signsDiffer = (unsignedSrc != unsignedDst);
            unsigned srcSize     = genTypeSize(srcType);
            unsigned dstSize     = genTypeSize(dstType);

            // For same sized casts with
            //    the same signs or non-overflow cast we discard them as well
            if (srcSize == dstSize)
            {
                // This should have been handled above
                noway_assert(varTypeIsGC(srcType) == varTypeIsGC(dstType));

                if (!signsDiffer)
                {
                    goto REMOVE_CAST;
                }

                if (!cast->gtOverflow())
                {
                    // For small type casts, when necessary we force
                    // the src operand to the dstType and allow the
                    // implied load from memory to perform the casting
                    if (varTypeIsSmall(srcType))
                    {
                        switch (src->GetOper())
                        {
                            case GT_IND:
                            case GT_CLS_VAR:
                            case GT_LCL_FLD:
                            case GT_ARR_ELEM:
                                src->SetType(dstType);
                                // We're changing the type here so we need to update the VN;
                                // in other cases we discard the cast without modifying oper
                                // so the VN doesn't change.
                                src->SetVNsFromNode(cast);
                                goto REMOVE_CAST;
                            default:
                                break;
                        }
                    }
                    else
                    {
                        goto REMOVE_CAST;
                    }
                }
            }
            else if (srcSize < dstSize) // widening cast
            {
                // Keep any long casts
                if (dstSize == 4)
                {
                    // Only keep signed to unsigned widening cast with overflow check
                    if (!cast->gtOverflow() || !unsignedDst || unsignedSrc)
                    {
                        goto REMOVE_CAST;
                    }
                }

                // Widening casts from unsigned or to signed can never overflow

                if (unsignedSrc || !unsignedDst)
                {
                    cast->gtFlags &= ~GTF_OVERFLOW;
                    if ((src->gtFlags & GTF_EXCEPT) == 0)
                    {
                        cast->gtFlags &= ~GTF_EXCEPT;
                    }
                }
            }
            else // if (srcSize > dstSize)
            {
                // Try to narrow the operand of the cast and discard the cast
                // Note: Do not narrow a cast that is marked as a CSE
                // And do not narrow if the oper is marked as a CSE either
                if (!cast->gtOverflow() && !gtIsActiveCSE_Candidate(src) && ((opts.compFlags & CLFLG_TREETRANS) != 0) &&
                    optNarrowTree(src, srcType, dstType, cast->gtVNPair, false))
                {
                    optNarrowTree(src, srcType, dstType, cast->gtVNPair, true);

                    // If oper is changed into a cast to TYP_INT, or to a GT_NOP, we may need to discard it
                    if (src->OperIs(GT_CAST) &&
                        (src->AsCast()->GetCastType() == varActualType(src->AsCast()->GetOp(0)->GetType())))
                    {
                        src = src->AsCast()->GetOp(0);
                    }

                    goto REMOVE_CAST;
                }
            }
        }

        switch (src->GetOper())
        {
            case GT_CNS_INT:
            case GT_CNS_LNG:
            case GT_CNS_DBL:
            case GT_CNS_STR:
            {
                GenTree* folded = gtFoldExprConst(cast); // This may not fold the constant (NaN ...)

                // Did we get a comma throw as a result of gtFoldExprConst?
                if (folded != cast)
                {
                    noway_assert(fgIsCommaThrow(folded));
                    folded->AsOp()->SetOp(0, fgMorphTree(folded->AsOp()->GetOp(0)));
                    fgMorphTreeDone(folded);
                    return folded;
                }

                if (!folded->OperIs(GT_CAST))
                {
                    return folded;
                }

                noway_assert(cast->GetOp(0) == src); // unchanged
            }
            break;

            case GT_CAST:
                // Check for two consecutive casts into the same dstType
                if (!cast->gtOverflow())
                {
                    var_types dstType2 = src->AsCast()->GetCastType();
                    if (dstType == dstType2)
                    {
                        goto REMOVE_CAST;
                    }

                    // Simplify some cast sequences:
                    //   Successive narrowing - CAST<byte>(CAST<short>(x)) is CAST<byte>(x)
                    //   Sign changing - CAST<byte>(CAST<ubyte>(x)) is CAST<byte>(x)
                    //   Unnecessary widening - CAST<byte>(CAST<long>(x)) is CAST<byte>(x)
                    if ((varTypeSize(dstType) <= varTypeSize(dstType2)) && varTypeIsIntegral(dstType) &&
                        varTypeIsIntegral(dstType2) && varTypeIsIntegral(src->AsCast()->GetOp(0)->GetType()) &&
                        !src->gtOverflow()
#ifndef TARGET_64BIT
                        // 32 bit target codegen does not support casting directly from LONG to small int
                        // types so we can't simplify CAST<byte>(CAST<int>(x.long)) to CAST<byte>(x.long).
                        && (!varTypeIsSmall(dstType) || (varTypeSize(dstType2) != 4) ||
                            (varTypeSize(src->AsCast()->GetOp(0)->GetType()) != 8))
#endif
                            )
                    {
                        src = src->AsCast()->GetOp(0);
                        cast->SetOp(0, src);

                        // We may have had CAST<uint>(CAST<long>(x.int)),
                        // this becomes CAST<uint>(x.int) and can be removed.
                        if (!varTypeIsSmall(dstType) && (varActualType(dstType) == varActualType(src->GetType())))
                        {
                            goto REMOVE_CAST;
                        }
                    }
                }
                break;

            case GT_COMMA:
                // Check for cast of a GT_COMMA with a throw overflow
                // Bug 110829: Since this optimization will bash the types
                // neither oper or commaOp2 can be CSE candidates
                if (fgIsCommaThrow(src) && !gtIsActiveCSE_Candidate(src)) // oper can not be a CSE candidate
                {
                    GenTree* commaOp2 = src->AsOp()->GetOp(1);

                    if (!gtIsActiveCSE_Candidate(commaOp2)) // commaOp2 can not be a CSE candidate
                    {
                        // need type of oper to be same as cast
                        if (cast->TypeIs(TYP_LONG))
                        {
                            commaOp2->ChangeOperConst(GT_CNS_NATIVELONG);
                            commaOp2->AsIntConCommon()->SetLngValue(0);
                            src->SetType(TYP_LONG);
                            commaOp2->SetType(TYP_LONG);
                        }
                        else if (varTypeIsFloating(cast->GetType()))
                        {
                            commaOp2->ChangeOperConst(GT_CNS_DBL);
                            commaOp2->AsDblCon()->SetValue(0.0);
                            src->SetType(cast->GetType());
                            commaOp2->SetType(cast->GetType());
                        }
                        else
                        {
                            commaOp2->ChangeOperConst(GT_CNS_INT);
                            commaOp2->AsIntCon()->SetValue(0);
                            src->SetType(TYP_INT);
                            commaOp2->SetType(TYP_INT);
                        }
                    }

                    if (vnStore != nullptr)
                    {
                        fgValueNumberTreeConst(commaOp2);
                    }

                    // Return the GT_COMMA node as the new tree
                    return src;
                }
                break;

            default:
                break;
        }
    }

    if (cast->gtOverflow())
    {
        fgAddCodeRef(compCurBB, bbThrowIndex(compCurBB), SCK_OVERFLOW);
    }

    return cast;

REMOVE_CAST:
    // Here we've eliminated the cast, so just return its operand
    assert(!gtIsActiveCSE_Candidate(cast));
    DEBUG_DESTROY_NODE(cast);
    return src;
}

#ifdef DEBUG
void CallArgInfo::Dump() const
{
    if (m_isReturn)
    {
        printf("return:");
    }
    else
    {
        printf("arg %u:", m_argNum);
    }

    printf(" [%06u] %s %s", GetNode()->GetID(), GenTree::OpName(GetNode()->OperGet()), varTypeName(m_argType));

    if (IsImplicitByRef())
    {
        printf(", implicit by-ref");
    }

    if (m_regCount != 0)
    {
#ifdef UNIX_AMD64_ABI
        printf(", %u reg%s (", m_regCount, m_regCount == 1 ? "" : "s");
#else
        printf(", %u %s reg%s (", m_regCount, varTypeName(GetRegType()), m_regCount == 1 ? "" : "s");
#endif
        for (unsigned i = 0; i < m_regCount; i++)
        {
#if defined(UNIX_AMD64_ABI)
            printf("%s%s %s", i == 0 ? "" : ", ", getRegName(GetRegNum(i)), varTypeName(GetRegType(i)));
#else
            printf("%s%s", i == 0 ? "" : ", ", getRegName(GetRegNum(i)));
#endif
        }
        printf(")");
    }

    if (m_slotCount == 1)
    {
        printf(", 1 slot (%u)", m_slotNum);
    }
    else if (m_slotCount > 1)
    {
        printf(", %u slots (%u..%u)", m_slotCount, m_slotNum, m_slotNum + m_slotCount - 1);
    }

    if (HasTemp())
    {
        printf(", temp V%02u", m_tempLclNum);
    }

#if FEATURE_FIXED_OUT_ARGS
    if (m_placeholderNeeded)
    {
        printf(", needPlace");
    }
#endif

    if (m_isNonStandard)
    {
        printf(", isNonStandard");
    }

    printf("\n");
}

void CallInfo::Dump() const
{
    for (unsigned i = 0; i < argCount; i++)
    {
        argTable[i]->Dump();
    }
}
#endif

CallInfo::CallInfo(Compiler* comp, GenTreeCall* call, unsigned numArgs)
{
    argCount    = 0; // filled in arg count, starts at zero
    nextSlotNum = INIT_ARG_STACK_SLOT;
#if defined(UNIX_X86_ABI)
    alignmentDone = false;
    stkSizeBytes  = 0;
    padStkAlign   = 0;
#endif

    INDEBUG(argTableSize = numArgs;) // the allocated table size

    hasRegArgs   = false;
    argsComplete = false;

    if (numArgs == 0)
    {
        argTable = nullptr;
    }
    else
    {
        argTable = new (comp, CMK_CallInfo) CallArgInfo*[numArgs];
    }
}

//------------------------------------------------------------------------------
//
//  CallInfo "copy constructor"
//
//  This method needs to act like a copy constructor for CallInfo.
//  The newCall needs to have its CallInfo initialized such that
//  we have newCall that is an exact copy of the oldCall.
//  We have to take care since the argument information
//  in the argTable contains pointers that must point to the
//  new arguments and not the old arguments.
//
CallInfo::CallInfo(Compiler* compiler, GenTreeCall* newCall, GenTreeCall* oldCall)
{
    CallInfo* oldArgInfo = oldCall->AsCall()->GetInfo();

    argCount    = oldArgInfo->argCount;
    nextSlotNum = INIT_ARG_STACK_SLOT;
#if defined(UNIX_X86_ABI)
    alignmentDone = oldArgInfo->alignmentDone;
    stkSizeBytes  = oldArgInfo->stkSizeBytes;
    padStkAlign   = oldArgInfo->padStkAlign;
#endif

    INDEBUG(argTableSize = oldArgInfo->argTableSize;)
    argsComplete = false;
    argTable     = nullptr;

    assert(oldArgInfo->argsComplete);

    if (argCount > 0)
    {
        argTable = new (compiler, CMK_CallInfo) fgArgTabEntry*[argCount];

        // Copy the old arg entries
        for (unsigned i = 0; i < argCount; i++)
        {
            argTable[i] = new (compiler, CMK_CallInfo) fgArgTabEntry(*oldArgInfo->argTable[i]);
        }

        // The copied arg entries contain pointers to old uses, they need
        // to be updated to point to new uses.
        if (newCall->gtCallThisArg != nullptr)
        {
            for (unsigned i = 0; i < argCount; i++)
            {
                if (argTable[i]->use == oldCall->gtCallThisArg)
                {
                    argTable[i]->use = newCall->gtCallThisArg;
                    break;
                }
            }
        }

        GenTreeCall::UseIterator newUse    = newCall->Args().begin();
        GenTreeCall::UseIterator newUseEnd = newCall->Args().end();
        GenTreeCall::UseIterator oldUse    = oldCall->Args().begin();
        GenTreeCall::UseIterator oldUseEnd = newCall->Args().end();

        for (; newUse != newUseEnd; ++newUse, ++oldUse)
        {
            for (unsigned i = 0; i < argCount; i++)
            {
                if (argTable[i]->use == oldUse.GetUse())
                {
                    argTable[i]->use = newUse.GetUse();
                    break;
                }
            }
        }

        newUse    = newCall->LateArgs().begin();
        newUseEnd = newCall->LateArgs().end();
        oldUse    = oldCall->LateArgs().begin();
        oldUseEnd = newCall->LateArgs().end();

        for (; newUse != newUseEnd; ++newUse, ++oldUse)
        {
            for (unsigned i = 0; i < argCount; i++)
            {
                if (argTable[i]->GetLateUse() == oldUse.GetUse())
                {
                    argTable[i]->SetLateUse(newUse.GetUse());
                    break;
                }
            }
        }
    }

    nextSlotNum  = oldArgInfo->nextSlotNum;
    hasRegArgs   = oldArgInfo->hasRegArgs;
    argsComplete = true;
}

void CallInfo::AddArg(CallArgInfo* argInfo)
{
    assert(argCount < argTableSize);
    argTable[argCount] = argInfo;
    argCount++;
    hasRegArgs |= argInfo->GetRegCount() != 0;
}

unsigned CallInfo::AllocateStackSlots(unsigned slotCount, unsigned alignment)
{
    assert(!argsComplete);

    unsigned firstSlot = roundUp(nextSlotNum, alignment);
    nextSlotNum        = firstSlot + slotCount;
    return firstSlot;
}

void CallInfo::ArgsComplete(Compiler* compiler, GenTreeCall* call)
{
    assert(!argsComplete);

    bool needsTemps = false;

    for (unsigned argIndex = 0; argIndex < argCount; argIndex++)
    {
        CallArgInfo* argInfo = argTable[argIndex];
        GenTree*     arg     = argInfo->GetNode();

        if (argInfo->GetRegCount() == 0)
        {
            assert(HasStackArgs());
#if !FEATURE_FIXED_OUT_ARGS
            // On x86 we use push instructions to pass arguments:
            //   The non-register arguments are evaluated and pushed in order
            //   and they are never evaluated into temps
            //
            continue;
#endif
        }
#if FEATURE_ARG_SPLIT
        else if (argInfo->IsSplit())
        {
            assert(HasStackArgs());
        }
#endif // FEATURE_ARG_SPLIT

        if (argInfo->HasTemp())
        {
            needsTemps = true;
        }
        else if ((argInfo->use == call->gtCallThisArg) && call->IsExpandedEarly() && call->IsVirtualVtable() &&
                 !arg->OperIs(GT_LCL_VAR, GT_LCL_FLD))
        {
            argInfo->SetTempNeeded();
            needsTemps = true;
        }

        // If the argument tree contains an assignment (GTF_ASG) then the argument and
        // and every earlier argument (except constants) must be evaluated into temps
        // since there may be other arguments that follow and they may use the value being assigned.
        //
        // EXAMPLE: ArgTab is "a, a=5, a"
        //          -> when we see the second arg "a=5"
        //             we know the first two arguments "a, a=5" have to be evaluated into temps
        //
        // For the case of an assignment, we only know that there exist some assignment someplace
        // in the tree.  We don't know what is being assigned so we are very conservative here
        // and assume that any local variable could have been assigned.

        if ((arg->gtFlags & GTF_ASG) != 0)
        {
            // TODO-MIKE-Review: This check seems overly conservative. If an arg contains an
            // assignment then it only needs a temp if it will be moved to the late arg list
            // (because, say, it's a register arg), so the assignment doesn't reorder with
            // subsequent args that may use whatver location the assignment changes.
            //
            // Likewise, previous arguments only need temps if they're going to be moved to
            // the late arg list.

            if (argCount > 1)
            {
                argInfo->SetTempNeeded();
                needsTemps = true;
            }

            // For all previous arguments, unless they are constants or local addresses
            // we require that they be evaluated into temps

            for (unsigned prevArgIndex = 0; prevArgIndex < argIndex; prevArgIndex++)
            {
                CallArgInfo* prevArgInfo = argTable[prevArgIndex];

                assert(prevArgInfo->GetArgNum() < argInfo->GetArgNum());

                if (!prevArgInfo->GetNode()->OperIsConst() && !prevArgInfo->GetNode()->OperIsLocalAddr())
                {
                    prevArgInfo->SetTempNeeded();
                    needsTemps = true;
                }
            }
        }

        bool treatLikeCall = ((arg->gtFlags & GTF_CALL) != 0);

#if FEATURE_FIXED_OUT_ARGS
        // Like calls, if this argument has a tree that will do an inline throw,
        // a call to a jit helper, then we need to treat it like a call (but only
        // if there are/were any stack args).
        // This means unnesting, sorting, etc.  Technically this is overly
        // conservative, but I want to avoid as much special-case debug-only code
        // as possible, so leveraging the GTF_CALL flag is the easiest.

        if (!treatLikeCall && ((arg->gtFlags & GTF_EXCEPT) != 0) && (argCount > 1) && compiler->opts.compDbgCode &&
            (compiler->fgWalkTreePre(&arg, Compiler::fgChkThrowCB) == Compiler::WALK_ABORT))
        {
            for (unsigned otherArgIndex = 0; otherArgIndex < argCount; otherArgIndex++)
            {
                if ((otherArgIndex != argIndex) && (argTable[otherArgIndex]->GetRegCount() == 0))
                {
                    treatLikeCall = true;
                    break;
                }
            }
        }
#endif // FEATURE_FIXED_OUT_ARGS

        // If it contains a call (GTF_CALL) then itself and everything before the call
        // with a GLOB_EFFECT must eval to temp (this is because everything with SIDE_EFFECT
        // has to be kept in the right order since we will move the call to the first position)
        //
        // For calls we don't have to be quite as conservative as we are with an assignment
        // since the call won't be modifying any non-address taken LclVars.

        if (treatLikeCall)
        {
            if (argCount > 1) // If this is not the only argument
            {
                argInfo->SetTempNeeded();
                needsTemps = true;
            }
            else if (varTypeIsFloating(arg->GetType()) && arg->IsCall())
            {
                // Spill all arguments that are floating point calls
                argInfo->SetTempNeeded();
                needsTemps = true;
            }

            // All previous arguments may need to be evaluated into temps
            for (unsigned prevArgIndex = 0; prevArgIndex < argIndex; prevArgIndex++)
            {
                CallArgInfo* prevArgInfo = argTable[prevArgIndex];

                assert(prevArgInfo->GetArgNum() < argInfo->GetArgNum());

                // For all previous arguments, if they have any GTF_ALL_EFFECT
                //  we require that they be evaluated into a temp
                if ((prevArgInfo->GetNode()->gtFlags & GTF_ALL_EFFECT) != 0)
                {
                    prevArgInfo->SetTempNeeded();
                    needsTemps = true;
                }
#if FEATURE_FIXED_OUT_ARGS
                // Or, if they are stored into the FIXED_OUT_ARG area
                // we require that they be moved to the gtCallLateArgs
                // and replaced with a placeholder node
                else if (prevArgInfo->GetSlotCount() != 0)
                {
                    prevArgInfo->SetPlaceholderNeeded();
                }
#endif
            }
        }
    }

    // TODO-MIKE-Cleanup: It's not entirely clear what the code below is trying to do.
    //   * Introduces temps for args that contain LCLHEAP, only on non-x86 targets.
    //     Obviously, you cannot stack allocate after some args have already been stored
    //     to the outgoing area since stack allocation moves the outgoing area. But:
    //     - The same is true on x86 - once args have been pushed, no stack allocation
    //       can be done. This was lumped together with GTF_EXCEPT checks but the two
    //       are slighly different.
    //     - Introducing a temp for the arg that uses LCLHEAP solves only one part of
    //       the problem. Temps must also be introduced for any previous stack args.
    //     - But IL requires the stack be empty before localalloc so it's not clear
    //       if it's possible to have a LCLHEAP in the middle of the arg list.
    //       Inlining a method that uses LCLHEAP? But that's currently blocked.
    //       And if only the first arg can contain LCLHEAP then we don't need to do
    //       anything, even if it's a stack arg, because the arg will be stored to the
    //       stack only after stack allocation happens.
    //     - Except that on x86 the first IL arg may be pushed last if StdCall/Cdecl
    //       calling conventions are used.
    //   * On x86 this code also introduces temps for reg args that may throw exceptions.
    //     That's because they're going to be moved to the late arg list so we need to
    //     ensure that the exception is still thrown at the right time. But:
    //     - What about non-x86 targets? These too have reg args that get moved to the
    //       late arg list. Well, that's simple - exception are incorectly reordered on
    //       such targets: Sink(x, y, z, x / y, a[x]); throws DivByZero on x86 and NullRef
    //       on all other targets.
    //     - Also, such temps are not always required. They're only needed if subsequent
    //       args have interfering side effects.
    //     - Old comments gave a different justification to the introduction of temps on
    //       x86 - "we previously recorded a stack depth of zero when morphing the register
    //       arguments of any GT_IND with a GTF_IND_RNGCHK flag". The flag no longer exists
    //       and the "recorded stack depth" likely refers to work that's now done post lowering
    //       by StackLevelSetter.
    //   * And on top of it all it's not clear why this need to be done in a loop separate
    //     from the one above that deals with assignments and calls.
    //
    //     The usual mess.
    //
    CLANG_FORMAT_COMMENT_ANCHOR;

#if FEATURE_FIXED_OUT_ARGS
    if (HasStackArgs() && compiler->compLocallocUsed)
#else
    if (HasStackArgs())
#endif
    {
        for (unsigned i = 0; i < argCount; i++)
        {
            CallArgInfo* argInfo = argTable[i];

            if (argInfo->IsTempNeeded() || argInfo->HasTemp() || (argInfo->GetRegCount() == 0))
            {
                continue;
            }

            GenTree* arg = argInfo->GetNode();

            if ((arg->gtFlags & GTF_EXCEPT) != 0)
            {
#if FEATURE_FIXED_OUT_ARGS
                // Returns WALK_ABORT if a GT_LCLHEAP node is encountered in the arg tree
                if (compiler->fgWalkTreePre(&arg, Compiler::fgChkLocAllocCB) == Compiler::WALK_ABORT)
#endif
                {
                    argInfo->SetTempNeeded();
                    needsTemps = true;
                }
            }
        }
    }

    if (HasRegArgs() || needsTemps)
    {
        SortArgs(compiler, call);
        EvalArgsToTemps(compiler, call);
    }

    argsComplete = true;
}

void CallInfo::SortArgs(Compiler* compiler, GenTreeCall* call)
{
    // Shuffle the arguments around before we build the gtCallLateArgs list.
    // The idea is to move all "simple" arguments like constants and local vars
    // to the end of the table, and move the complex arguments towards the beginning
    // of the table. This will help prevent registers from being spilled by
    // allowing us to evaluate the more complex arguments before the simpler arguments.
    // The argTable ends up looking like:
    //     +------------------------------------+  <--- argTable[argCount - 1]
    //     |          constants                 |
    //     +------------------------------------+
    //     |    local var / local field         |
    //     +------------------------------------+
    //     | remaining arguments sorted by cost |
    //     +------------------------------------+
    //     | temps - argTable[].IsTempNeeded()  |
    //     +------------------------------------+
    //     |  args with calls (GTF_CALL)        |
    //     +------------------------------------+  <--- argTable[0]

    // TODO-MIKE-Cleanup: Arg table sorting is kind of weird. It seems that the resulting order is only
    // used in EvalArgsToTemps so:
    //   - It only affects the ordering of the late args, sorting "normal" args is probably a waste.
    //   - The number of args is typically low so it may be better to allocate a separate array and
    //     pass that to EvalArgsToTemps. That would avoid the need to linear search by arg number
    //     in GetArgInfoByArgNum.
    //   - Sorting isn't stable. For example, when const args are moved to the end of the table their
    //     relative order is preserved. But the args they're displacing lose their ordering relative
    //     to the non-displaced args. This would probably be a bug if it weren't for the first issue,
    //     the new order is relevant only to late args. If 2 args with calls get reordered in doesn't
    //     matter because such args get temps and only temp uses in the arg list get ordered. So it
    //     seems that stability doesn't matter, except for the added confusion.

    ssize_t first = 0;
    ssize_t last  = static_cast<ssize_t>(argCount) - 1;

    // Move all constant args to the end of the arg table.
    for (ssize_t i = last; i >= first; i--)
    {
        if (argTable[i]->use->GetNode()->OperIs(GT_CNS_INT))
        {
            std::swap(argTable[i], argTable[last]);
            last--;
        }
    }

    // Move all call args to the beginning of the arg table.
    for (ssize_t i = first; i <= last; i++)
    {
        if ((argTable[i]->use->GetNode()->gtFlags & GTF_CALL) != 0)
        {
            std::swap(argTable[i], argTable[first]);
            first++;
        }
    }

    // Move all args with temps to the beginning of the arg table, after the calls.
    for (ssize_t i = first; i <= last; i++)
    {
        if (argTable[i]->IsTempNeeded() || argTable[i]->HasTemp())
        {
            std::swap(argTable[i], argTable[first]);
            first++;
        }
    }

    // Move all local args to the end of the arg table, before the constants.
    for (ssize_t i = last; i >= first; i--)
    {
        // Ignore STRUCT locals because they were previously wrapped in OBJ(ADDR(...)) so they were treated differently.
        if (argTable[i]->use->GetNode()->OperIs(GT_LCL_VAR, GT_LCL_FLD) &&
            !argTable[i]->use->GetNode()->TypeIs(TYP_STRUCT))
        {
            std::swap(argTable[i], argTable[last]);
            last--;
        }
    }

    // Order any remaining args by execution cost.

    // TODO-MIKE-Cleanup: Why execution cost?! It makes no difference if one arg takes 30 cycles
    // to be computed and another only 3. What matters is how many registers each arg tree needs
    // for evaluation (so the "cost" should really be the value returned by gtSetEvalOrder - a
    // Sethi-Ullman number approximation). Not surprisingly, attempting to change this results
    // in diffs that aren't all improvements.

    if (first < last)
    {
        for (ssize_t i = first; i <= last; i++)
        {
            GenTree* arg = argTable[i]->use->GetNode();

            // Try to keep STRUCT typed LCL_VAR args in the same position they were when wrapped in OBJs
            // by setting the same costs an OBJ(ADDR(LCL_VAR|FLD)) tree would have.

            // TODO-MIKE-Cleanup: These should probably be moved to gtSetEvalOrder, they're here only to
            // keep diffs small.

            if (arg->OperIs(GT_LCL_VAR))
            {
                arg->SetCosts(9, 7);
            }
            else if (arg->OperIs(GT_LCL_FLD))
            {
                arg->SetCosts(9, 9);
            }
            else
            {
                compiler->gtPrepareCost(arg);
            }
        }

        // TODO-MIKE-Cleanup: This could use jitstd::sort but there are a few diffs caused by sorting instability.

        while (first < last)
        {
            unsigned maxCost      = argTable[first]->use->GetNode()->GetCostEx();
            ssize_t  maxCostIndex = first;

            for (ssize_t i = first + 1; i <= last; i++)
            {
                if (argTable[i]->use->GetNode()->GetCostEx() > maxCost)
                {
                    maxCost      = argTable[i]->use->GetNode()->GetCostEx();
                    maxCostIndex = i;
                }
            }

            std::swap(argTable[maxCostIndex], argTable[first]);
            first++;
        }
    }

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("\nSorted arg table:\n");
        Dump();
        printf("\n");
    }
#endif
}

void CallInfo::EvalArgsToTemps(Compiler* compiler, GenTreeCall* call)
{
    GenTreeCall::Use* lateArgUseListTail = nullptr;

    for (unsigned i = 0; i < argCount; i++)
    {
        CallArgInfo* argInfo = argTable[i];

        assert(!argInfo->HasLateUse());

#if !FEATURE_FIXED_OUT_ARGS
        // Only ever set for FEATURE_FIXED_OUT_ARGS
        assert(!argInfo->IsPlaceholderNeeded());

        // On x86 and other archs that use push instructions to pass arguments:
        //   Only the register arguments need to be replaced with placeholder nodes.
        //   Stacked arguments are evaluated and pushed (or stored into the stack) in order.
        //
        if (argInfo->GetRegCount() == 0)
        {
            continue;
        }
#endif

        GenTree* arg      = argInfo->GetNode();
        GenTree* setupArg = nullptr;
        GenTree* lateArg  = nullptr;

        if (argInfo->HasTemp())
        {
            JITDUMPTREE(arg, "Arg temp is already created:\n");

#ifdef TARGET_64BIT
            // fgMorphArgs creates temps only for implicit by-ref args, which makes handling
            // this case trivial - the late arg is the address of the created temp local.

            assert(argInfo->IsImplicitByRef());

            compiler->lvaSetVarAddrExposed(argInfo->GetTempLclNum());
            lateArg = compiler->gtNewLclVarAddrNode(argInfo->GetTempLclNum());
#else
            unreached();
#endif
        }
        else if (argInfo->IsTempNeeded())
        {
            JITDUMPTREE(arg, "Creating temp for arg:\n");

            // We may have started with a struct arg and changed it to a FP/SIMD arg.
            if (varTypeUsesFloatReg(arg->GetType()))
            {
                compiler->compFloatingPointUsed = true;
            }

            unsigned   tempLclNum = compiler->lvaGrabTemp(true DEBUGARG("argument with side effect"));
            LclVarDsc* tempLcl    = compiler->lvaGetDesc(tempLclNum);

            argInfo->SetTempLclNum(tempLclNum);

            if (!varTypeIsStruct(arg->GetType()))
            {
                var_types type = varActualType(arg->GetType());
                tempLcl->SetType(type);
                setupArg = compiler->gtNewAssignNode(compiler->gtNewLclvNode(tempLclNum, type), arg);
            }
            else if (varTypeIsSIMD(arg->GetType()))
            {
                ClassLayout* layout = compiler->typGetVectorLayout(arg);
                if (layout != nullptr)
                {
                    compiler->lvaSetStruct(tempLclNum, layout, /* checkUnsafeBuffer */ false);
                }
                else
                {
                    // We may not be able to recover the struct handle from HWINTRINSIC
                    // nodes and SIMD typed IND nodes.

                    // TODO-MIKE-Cleanup: So why bother at all?

                    assert(arg->OperIsHWIntrinsic() || arg->OperIs(GT_IND));
                    tempLcl->lvType = arg->GetType();
                }

                setupArg = compiler->gtNewAssignNode(compiler->gtNewLclvNode(tempLclNum, arg->GetType()), arg);
                setupArg = compiler->fgMorphStructAssignment(setupArg->AsOp());
            }
#ifndef TARGET_X86
            else if (arg->OperIs(GT_MKREFANY))
            {
                compiler->lvaSetStruct(tempLclNum, compiler->impGetRefAnyClass(), false);
                setupArg = compiler->abiMorphMkRefAnyToStore(tempLclNum, arg->AsOp());
            }
#endif
            else
            {
                ClassLayout* layout = compiler->typGetStructLayout(arg);
                compiler->lvaSetStruct(tempLclNum, layout, /* checkUnsafeBuffer */ false);
                setupArg = compiler->gtNewAssignNode(compiler->gtNewLclvNode(tempLclNum, TYP_STRUCT), arg);
                setupArg = compiler->fgMorphStructAssignment(setupArg->AsOp());
            }

            lateArg = compiler->gtNewLclvNode(tempLclNum, varActualType(arg->GetType()));
        }
        else if ((argInfo->GetRegCount() != 0) || argInfo->IsPlaceholderNeeded())
        {
            JITDUMPTREE(arg, "Creating placeholder for arg:\n");

            setupArg = new (compiler, GT_ARGPLACE) GenTree(GT_ARGPLACE, arg->GetType());

            // No temp needed - the arg tree itself is moved to the late arg list.
            lateArg = arg;
        }

        if (lateArg != nullptr)
        {
            if (setupArg != nullptr)
            {
                JITDUMPTREE(setupArg, "Created arg setup/placeholder tree:\n");

                setupArg->gtFlags |= GTF_LATE_ARG;
                argInfo->use->SetNode(setupArg);

                JITDUMP("\n");
            }
            else
            {
                arg->gtFlags |= GTF_LATE_ARG;
            }

            GenTreeCall::Use* lateArgUse = compiler->gtNewCallArgs(lateArg);

            if (lateArgUseListTail == nullptr)
            {
                call->gtCallLateArgs = lateArgUse;
            }
            else
            {
                lateArgUseListTail->SetNext(lateArgUse);
            }

            lateArgUseListTail = lateArgUse;
            argInfo->SetLateUse(lateArgUse);
        }
    }
}

//------------------------------------------------------------------------------
// fgMakeMultiUse : If the node is a local, clone it, otherwise insert a comma form temp
//
// Arguments:
//    ppTree  - a pointer to the child node we will be replacing with the comma expression that
//              evaluates ppTree to a temp and returns the result
//
// Return Value:
//    A fresh GT_LCL_VAR node referencing the temp which has not been used

GenTree* Compiler::fgMakeMultiUse(GenTree** pOp)
{
    GenTree* tree = *pOp;
    if (tree->IsLocal())
    {
        return gtClone(tree);
    }
    else
    {
        return fgInsertCommaFormTemp(pOp);
    }
}

GenTree* Compiler::fgInsertCommaFormTemp(GenTree** use)
{
    GenTree* tree = *use;
    assert(!varTypeIsStruct(tree->GetType()));

    var_types type   = varActualType(tree->GetType());
    unsigned  lclNum = lvaNewTemp(type, true DEBUGARG("fgInsertCommaFormTemp temp"));
    GenTree*  asg    = gtNewAssignNode(gtNewLclvNode(lclNum, type), tree);
    GenTree*  load   = gtNewLclvNode(lclNum, type);
    *use             = gtNewCommaNode(asg, load, type);
    return gtNewLclvNode(lclNum, type);
}

//------------------------------------------------------------------------
// fgInitArgInfo: Construct the fgArgInfo for the call with the fgArgEntry for each arg
//
// Arguments:
//    callNode - the call for which we are generating the fgArgInfo
//
// Return Value:
//    None
//
// Notes:
//    This method is idempotent in that it checks whether the fgArgInfo has already been
//    constructed, and just returns.
//    This method only computes the arg table and arg entries for the call (the fgArgInfo),
//    and makes no modification of the args themselves.
//
//    The IR for the call args can change for calls with non-standard arguments: some non-standard
//    arguments add new call argument IR nodes.
//
void Compiler::fgInitArgInfo(GenTreeCall* call)
{
    if (call->fgArgInfo != nullptr)
    {
        // We've already initialized and set the fgArgInfo.
        return;
    }

    JITDUMP("\nInitializing call [%06u] arg info\n", call->gtTreeID);

    // At this point, we should never have gtCallLateArgs, as this needs to be done before those are determined.
    assert(call->gtCallLateArgs == nullptr);

    const bool callHasRetBuffArg = call->HasRetBufArg();
    const bool callIsVararg      = call->IsVarargs();

#ifdef TARGET_UNIX
    if (callIsVararg)
    {
        // Currently native varargs is not implemented on non windows targets.
        //
        // Note that some targets like Arm64 Unix should not need much work as
        // the ABI is the same. While other targets may only need small changes
        // such as amd64 Unix, which just expects RAX to pass numFPArguments.
        NYI("Morphing Vararg call not yet implemented on non Windows targets.");
    }
#endif // TARGET_UNIX

    // Data structure for keeping track of non-standard args. Non-standard args are those that are not passed
    // following the normal calling convention or in the normal argument registers. We either mark existing
    // arguments as non-standard (such as the x8 return buffer register on ARM64), or we manually insert the
    // non-standard arguments into the argument list, below.
    class NonStandardArgs
    {
        struct NonStandardArg
        {
            regNumber reg;  // The register to be assigned to this non-standard argument.
            GenTree*  node; // The tree node representing this non-standard argument.
                            //   Note that this must be updated if the tree node changes due to morphing!
        };

        ArrayStack<NonStandardArg> args;

    public:
        NonStandardArgs(CompAllocator alloc) : args(alloc, 3) // We will have at most 3 non-standard arguments
        {
        }

        //-----------------------------------------------------------------------------
        // Add: add a non-standard argument to the table of non-standard arguments
        //
        // Arguments:
        //    node - a GenTree node that has a non-standard argument.
        //    reg - the register to assign to this node.
        //
        // Return Value:
        //    None.
        //
        void Add(GenTree* node, regNumber reg)
        {
            NonStandardArg nsa = {reg, node};
            args.Push(nsa);
        }

        //-----------------------------------------------------------------------------
        // FindReg: Look for a GenTree node in the non-standard arguments set. If found,
        // set the register to use for the node.
        //
        // Arguments:
        //    node - a GenTree node to look for
        //    pReg - an OUT argument. *pReg is set to the non-standard register to use if
        //           'node' is found in the non-standard argument set.
        //
        // Return Value:
        //    'true' if 'node' is a non-standard argument. In this case, *pReg is set to the
        //          register to use.
        //    'false' otherwise (in this case, *pReg is unmodified).
        //
        bool FindReg(GenTree* node, regNumber* pReg)
        {
            for (int i = 0; i < args.Height(); i++)
            {
                NonStandardArg& nsa = args.TopRef(i);
                if (node == nsa.node)
                {
                    *pReg = nsa.reg;
                    return true;
                }
            }
            return false;
        }
    } nonStandardArgs(getAllocator(CMK_ArrayStack));

    // Count of args. On first morph, this is counted before we've filled in the arg table.
    // On remorph, we grab it from the arg table.
    unsigned numArgs = 0;

    // First we need to count the args
    if (call->gtCallThisArg != nullptr)
    {
        numArgs++;
    }
    for (GenTreeCall::Use& use : call->Args())
    {
        numArgs++;
    }

    // Insert or mark non-standard args. These are either outside the normal calling convention, or
    // arguments registers that don't follow the normal progression of argument registers in the calling
    // convention (such as for the ARM64 fixed return buffer argument x8).
    //
    // *********** NOTE *************
    // The logic here must remain in sync with GetNonStandardAddedArgCount(), which is used to map arguments
    // in the implementation of fast tail call.
    // *********** END NOTE *********
    CLANG_FORMAT_COMMENT_ANCHOR;

#if defined(TARGET_X86) || defined(TARGET_ARM)
    // The x86 and arm32 CORINFO_HELP_INIT_PINVOKE_FRAME helpers has a custom calling convention.
    // Set the argument registers correctly here.
    if (call->IsHelperCall(this, CORINFO_HELP_INIT_PINVOKE_FRAME))
    {
        GenTreeCall::Use* args = call->gtCallArgs;
        GenTree*          arg1 = args->GetNode();
        assert(arg1 != nullptr);
        nonStandardArgs.Add(arg1, REG_PINVOKE_FRAME);
    }
#endif // defined(TARGET_X86) || defined(TARGET_ARM)
#if defined(TARGET_ARM)
    // A non-standard calling convention using wrapper delegate invoke is used on ARM, only, for wrapper
    // delegates. It is used for VSD delegate calls where the VSD custom calling convention ABI requires passing
    // R4, a callee-saved register, with a special value. Since R4 is a callee-saved register, its value needs
    // to be preserved. Thus, the VM uses a wrapper delegate IL stub, which preserves R4 and also sets up R4
    // correctly for the VSD call. The VM is simply reusing an existing mechanism (wrapper delegate IL stub)
    // to achieve its goal for delegate VSD call. See COMDelegate::NeedsWrapperDelegate() in the VM for details.
    else if (call->gtCallMoreFlags & GTF_CALL_M_WRAPPER_DELEGATE_INV)
    {
        GenTree* arg = call->gtCallThisArg->GetNode();
        if (arg->OperIsLocal())
        {
            arg = gtClone(arg, true);
        }
        else
        {
            GenTree* tmp = fgInsertCommaFormTemp(&arg);
            call->gtCallThisArg->SetNode(arg);
            call->gtFlags |= GTF_ASG;
            arg = tmp;
        }
        noway_assert(arg != nullptr);

        GenTree* newArg = new (this, GT_LEA)
            GenTreeAddrMode(TYP_BYREF, arg, nullptr, 0, eeGetEEInfo()->offsetOfWrapperDelegateIndirectCell);

        // Append newArg as the last arg
        GenTreeCall::Use** insertionPoint = &call->gtCallArgs;
        for (; *insertionPoint != nullptr; insertionPoint = &((*insertionPoint)->NextRef()))
        {
        }
        *insertionPoint = gtNewCallArgs(newArg);

        numArgs++;
        nonStandardArgs.Add(newArg, virtualStubParamInfo->GetReg());
    }
#endif // defined(TARGET_ARM)
#if defined(TARGET_X86)
    // The x86 shift helpers have custom calling conventions and expect the lo part of the long to be in EAX and the
    // hi part to be in EDX. This sets the argument registers up correctly.
    else if (call->IsHelperCall(this, CORINFO_HELP_LLSH) || call->IsHelperCall(this, CORINFO_HELP_LRSH) ||
             call->IsHelperCall(this, CORINFO_HELP_LRSZ))
    {
        GenTreeCall::Use* args = call->gtCallArgs;
        GenTree*          arg1 = args->GetNode();
        assert(arg1 != nullptr);
        nonStandardArgs.Add(arg1, REG_LNGARG_LO);

        args          = args->GetNext();
        GenTree* arg2 = args->GetNode();
        assert(arg2 != nullptr);
        nonStandardArgs.Add(arg2, REG_LNGARG_HI);
    }
#else  // !TARGET_X86
    // TODO-X86-CQ: Currently RyuJIT/x86 passes args on the stack, so this is not needed.
    // If/when we change that, the following code needs to be changed to correctly support the (TBD) managed calling
    // convention for x86/SSE.

    // If we have a Fixed Return Buffer argument register then we setup a non-standard argument for it.
    //
    // We don't use the fixed return buffer argument if we have the special unmanaged instance call convention.
    // That convention doesn't use the fixed return buffer register.
    //
    CLANG_FORMAT_COMMENT_ANCHOR;

    if (call->HasFixedRetBufArg())
    {
        GenTreeCall::Use* args = call->gtCallArgs;
        assert(args != nullptr);

        // We don't increment numArgs here, since we already counted this argument above.

        nonStandardArgs.Add(args->GetNode(), theFixedRetBuffReg());
    }

    // We are allowed to have a Fixed Return Buffer argument combined
    // with any of the remaining non-standard arguments
    //
    CLANG_FORMAT_COMMENT_ANCHOR;

    if (call->IsVirtualStub())
    {
        if (!call->IsTailCallViaJitHelper())
        {
            GenTree* stubAddrArg = fgGetStubAddrArg(call);
            // And push the stub address onto the list of arguments
            call->gtCallArgs = gtPrependNewCallArg(stubAddrArg, call->gtCallArgs);

            numArgs++;
            nonStandardArgs.Add(stubAddrArg, stubAddrArg->GetRegNum());
        }
        else
        {
            // If it is a VSD call getting dispatched via tail call helper,
            // fgMorphTailCallViaJitHelper() would materialize stub addr as an additional
            // parameter added to the original arg list and hence no need to
            // add as a non-standard arg.
        }
    }
    else
#endif // !TARGET_X86
    if (call->gtCallType == CT_INDIRECT && (call->gtCallCookie != nullptr))
    {
        assert(!call->IsUnmanaged());

        GenTree* arg = call->gtCallCookie;
        noway_assert(arg != nullptr);
        call->gtCallCookie = nullptr;

#if defined(TARGET_X86)
        // x86 passes the cookie on the stack as the final argument to the call.
        GenTreeCall::Use** insertionPoint = &call->gtCallArgs;
        for (; *insertionPoint != nullptr; insertionPoint = &((*insertionPoint)->NextRef()))
        {
        }
        *insertionPoint = gtNewCallArgs(arg);
#else  // !defined(TARGET_X86)
        // All other architectures pass the cookie in a register.
        call->gtCallArgs = gtPrependNewCallArg(arg, call->gtCallArgs);
#endif // defined(TARGET_X86)

        nonStandardArgs.Add(arg, REG_PINVOKE_COOKIE_PARAM);
        numArgs++;

        // put destination into R10/EAX
        arg              = gtClone(call->gtCallAddr, true);
        call->gtCallArgs = gtPrependNewCallArg(arg, call->gtCallArgs);
        numArgs++;

        nonStandardArgs.Add(arg, REG_PINVOKE_TARGET_PARAM);

        // finally change this call to a helper call
        call->gtCallType    = CT_HELPER;
        call->gtCallMethHnd = eeFindHelper(CORINFO_HELP_PINVOKE_CALLI);
    }
#if defined(FEATURE_READYTORUN_COMPILER) && defined(TARGET_ARMARCH)
    // For arm, we dispatch code same as VSD using virtualStubParamInfo->GetReg()
    // for indirection cell address, which ZapIndirectHelperThunk expects.
    if (call->IsR2RRelativeIndir())
    {
        assert(call->gtEntryPoint.addr != nullptr);

        size_t   addrValue           = (size_t)call->gtEntryPoint.addr;
        GenTree* indirectCellAddress = gtNewIconHandleNode(addrValue, GTF_ICON_FTN_ADDR);
#ifdef DEBUG
        indirectCellAddress->AsIntCon()->gtTargetHandle = (size_t)call->gtCallMethHnd;
#endif
        indirectCellAddress->SetRegNum(REG_R2R_INDIRECT_PARAM);
#ifdef TARGET_ARM
        // Issue #xxxx : Don't attempt to CSE this constant on ARM32
        //
        // This constant has specific register requirements, and LSRA doesn't currently correctly
        // handle them when the value is in a CSE'd local.
        indirectCellAddress->SetDoNotCSE();
#endif // TARGET_ARM

        // Push the stub address onto the list of arguments.
        call->gtCallArgs = gtPrependNewCallArg(indirectCellAddress, call->gtCallArgs);

        numArgs++;
        nonStandardArgs.Add(indirectCellAddress, indirectCellAddress->GetRegNum());
    }

#endif // FEATURE_READYTORUN_COMPILER && TARGET_ARMARCH

    call->SetInfo(new (this, CMK_CallInfo) CallInfo(this, call, numArgs));

    unsigned argIndex     = 0;
    unsigned intArgRegNum = 0;
    unsigned fltArgRegNum = 0;

    // Add the 'this' argument value, if present.
    if (call->gtCallThisArg != nullptr)
    {
        GenTree* argx = call->gtCallThisArg->GetNode();
        assert(call->gtCallType == CT_USER_FUNC || call->gtCallType == CT_INDIRECT);
        assert(varTypeIsGC(argx) || (argx->gtType == TYP_I_IMPL));

        CallArgInfo* argInfo = new (this, CMK_CallInfo) CallArgInfo(0, call->gtCallThisArg, 1);
        argInfo->SetRegNum(0, genMapIntRegArgNumToRegNum(intArgRegNum));
        argInfo->SetArgType(argx->GetType());
        call->fgArgInfo->AddArg(argInfo);
        intArgRegNum++;
#ifdef WINDOWS_AMD64_ABI
        // Whenever we pass an integer register argument
        // we skip the corresponding floating point register argument
        fltArgRegNum++;
#endif // WINDOWS_AMD64_ABI
        argIndex++;
    }

#ifndef TARGET_X86
    const unsigned maxRegArgs = MAX_REG_ARG; // other arch: fixed constant number
#else
    unsigned maxRegArgs  = MAX_REG_ARG; // X86: non-const, must be calculated
    // Compute the maximum number of arguments that can be passed in registers.
    // For X86 we handle the varargs and unmanaged calling conventions

    if (call->gtFlags & GTF_CALL_POP_ARGS)
    {
        noway_assert(intArgRegNum < MAX_REG_ARG);
        // No more register arguments for varargs (CALL_POP_ARGS)
        maxRegArgs = intArgRegNum;

        // Add in the ret buff arg
        if (callHasRetBuffArg)
        {
            maxRegArgs++;
        }
    }

    if (call->IsUnmanaged())
    {
        noway_assert(intArgRegNum == 0);

        if (call->gtCallMoreFlags & GTF_CALL_M_UNMGD_THISCALL)
        {
            noway_assert(call->gtCallArgs->GetNode()->TypeGet() == TYP_I_IMPL ||
                         call->gtCallArgs->GetNode()->TypeGet() == TYP_BYREF ||
                         call->gtCallArgs->GetNode()->gtOper ==
                             GT_NOP); // the arg was already morphed to a register (fgMorph called twice)
            maxRegArgs = 1;
        }
        else
        {
            maxRegArgs = 0;
        }
#ifdef UNIX_X86_ABI
        // Add in the ret buff arg
        if (callHasRetBuffArg)
        {
            maxRegArgs++;
        }
#endif
    }
#endif // TARGET_X86

    /* Morph the user arguments */
    CLANG_FORMAT_COMMENT_ANCHOR;

#if defined(TARGET_ARM)

    // The ARM ABI has a concept of back-filling of floating-point argument registers, according
    // to the "Procedure Call Standard for the ARM Architecture" document, especially
    // section 6.1.2.3 "Parameter passing". Back-filling is where floating-point argument N+1 can
    // appear in a lower-numbered register than floating point argument N. That is, argument
    // register allocation is not strictly increasing. To support this, we need to keep track of unused
    // floating-point argument registers that we can back-fill. We only support 4-byte float and
    // 8-byte double types, and one to four element HFAs composed of these types. With this, we will
    // only back-fill single registers, since there is no way with these types to create
    // an alignment hole greater than one register. However, there can be up to 3 back-fill slots
    // available (with 16 FP argument registers). Consider this code:
    //
    // struct HFA { float x, y, z; }; // a three element HFA
    // void bar(float a1,   // passed in f0
    //          double a2,  // passed in f2/f3; skip f1 for alignment
    //          HFA a3,     // passed in f4/f5/f6
    //          double a4,  // passed in f8/f9; skip f7 for alignment. NOTE: it doesn't fit in the f1 back-fill slot
    //          HFA a5,     // passed in f10/f11/f12
    //          double a6,  // passed in f14/f15; skip f13 for alignment. NOTE: it doesn't fit in the f1 or f7 back-fill
    //                      // slots
    //          float a7,   // passed in f1 (back-filled)
    //          float a8,   // passed in f7 (back-filled)
    //          float a9,   // passed in f13 (back-filled)
    //          float a10)  // passed on the stack in [OutArg+0]
    //
    // Note that if we ever support FP types with larger alignment requirements, then there could
    // be more than single register back-fills.
    //
    // Once we assign a floating-pointer register to the stack, they all must be on the stack.
    // See "Procedure Call Standard for the ARM Architecture", section 6.1.2.3, "The back-filling
    // continues only so long as no VFP CPRC has been allocated to a slot on the stack."
    // We set anyFloatStackArgs to true when a floating-point argument has been assigned to the stack
    // and prevent any additional floating-point arguments from going in registers.

    bool anyFloatStackArgs = false;

    regMaskTP argSkippedRegMask    = RBM_NONE;
    regMaskTP fltArgSkippedRegMask = RBM_NONE;
#endif //  TARGET_ARM

    for (GenTreeCall::Use *args = call->gtCallArgs; args != nullptr; args = args->GetNext(), argIndex++)
    {
        GenTree* const argx = args->GetNode();

        // We should never have any ArgPlaceHolder nodes at this point.
        assert(!argx->IsArgPlaceHolderNode());

        unsigned     size            = 0;
        var_types    sigType         = TYP_UNDEF;
        unsigned     argAlign        = 1;
        const bool   isStructArg     = typIsLayoutNum(args->GetSigTypeNum());
        ClassLayout* layout          = isStructArg ? typGetLayoutByNum(args->GetSigTypeNum()) : nullptr;
        unsigned     structSize      = 0;
        var_types    structBaseType  = TYP_STRUCT;
        bool         passStructByRef = false;
        var_types    hfaType         = TYP_UNDEF;
        unsigned     hfaSlots        = 0;

        if (isStructArg)
        {
            structSize = layout->GetSize();
            sigType    = TYP_STRUCT;

            layout->EnsureHfaInfo(this);

            if (layout->IsHfa()
#if defined(TARGET_WINDOWS) && defined(TARGET_ARM64)
                && !callIsVararg
#endif
                )
            {
                hfaType  = layout->GetHfaElementType();
                hfaSlots = layout->GetHfaRegCount();

                // If we have a HFA struct it's possible we transition from a method that originally
                // only had integer types to now start having FP types.  We have to communicate this
                // through this flag since LSRA later on will use this flag to determine whether
                // or not to track the FP register set.
                //
                compFloatingPointUsed = true;
            }

#ifdef TARGET_ARM
            argAlign =
                roundUp(info.compCompHnd->getClassAlignmentRequirement(layout->GetClassHandle()), REGSIZE_BYTES) /
                REGSIZE_BYTES;
#endif

#if defined(TARGET_AMD64)
#ifdef UNIX_AMD64_ABI
            size = roundUp(structSize, REGSIZE_BYTES) / REGSIZE_BYTES;
#else
            size = 1;
#endif
#elif defined(TARGET_ARM64)
            if (hfaType != TYP_UNDEF)
            {
                // HFA structs are passed by value in multiple registers.
                // The "size" in registers may differ the size in pointer-sized units.
                size = hfaSlots;
            }
            else
            {
                // Structs are either passed in 1 or 2 (64-bit) slots.
                // Structs that are the size of 2 pointers are passed by value in multiple registers,
                // if sufficient registers are available.
                // Structs that are larger than 2 pointers (except for HFAs) are passed by
                // reference (to a copy)
                size = roundUp(structSize, REGSIZE_BYTES) / REGSIZE_BYTES;

                if (size > 2)
                {
                    size = 1;
                }
            }
#elif defined(TARGET_ARM) || defined(TARGET_X86)
            size                      = roundUp(structSize, REGSIZE_BYTES) / REGSIZE_BYTES;
#else
#error Unsupported or unset target architecture
#endif // TARGET_XXX

            // TODO-MIKE-Cleanup: Huh, there should be no 0 sized structs...
            structSize = (structSize == 0) ? TARGET_POINTER_SIZE : structSize;

            StructPassing howToPassStruct = abiGetStructParamType(layout, callIsVararg);

            structBaseType  = howToPassStruct.type;
            passStructByRef = (howToPassStruct.kind == SPK_ByReference);

            if (howToPassStruct.kind == SPK_PrimitiveType)
            {
#ifdef TARGET_ARM
                // TODO-CQ: abiGetStructParamType should *not* return TYP_DOUBLE for a double struct,
                // or for a struct of two floats. This causes the struct to be address-taken.
                if (structBaseType == TYP_DOUBLE)
                {
                    size = 2;
                }
                else
#endif // TARGET_ARM
                {
                    size = 1;
                }
            }
            else if (passStructByRef)
            {
                size = 1;
            }
        }
        else
        {
            sigType = static_cast<var_types>(args->GetSigTypeNum());

            // The signature type should type should never be STRUCT, for struct params we should have
            // a layout instead. If it is then it's likely that we have a helper call with struct params.
            assert(sigType != TYP_STRUCT);

            // We may get primitive args for struct params but never the other way around.
            assert(!varTypeIsStruct(argx->GetType()));

            assert((varActualType(sigType) == varActualType(argx->GetType())) ||
                   ((sigType == TYP_BYREF) && argx->TypeIs(TYP_I_IMPL)) ||
                   ((sigType == TYP_I_IMPL) && argx->TypeIs(TYP_BYREF)));

#ifdef TARGET_ARM
            argAlign =
                roundUp(static_cast<unsigned>(genTypeAlignments[argx->GetType()]), REGSIZE_BYTES) / REGSIZE_BYTES;
#endif

#ifdef TARGET_64BIT
            // On 64 bit targets all primitive types are passed in a single reg/slot.
            size = 1;
#else
            // On 32 bit targets LONG and DOUBLE are passed in 2 regs/slots.
            size = genTypeStSz(argx->GetType());
#endif
        }

#ifdef TARGET_ARM
        const bool passUsingFloatRegs =
            !opts.compUseSoftFP && ((hfaType != TYP_UNDEF) || (!isStructArg && varTypeUsesFloatReg(argx->GetType())));

        if (argAlign == 2)
        {
            if (passUsingFloatRegs)
            {
                if (fltArgRegNum % 2 == 1)
                {
                    fltArgSkippedRegMask |= genMapArgNumToRegMask(fltArgRegNum, TYP_FLOAT);
                    fltArgRegNum++;
                }
            }
            else if (intArgRegNum < MAX_REG_ARG)
            {
                if (intArgRegNum % 2 == 1)
                {
                    argSkippedRegMask |= genMapArgNumToRegMask(intArgRegNum, TYP_I_IMPL);
                    intArgRegNum++;
                }
            }
        }
#elif defined(TARGET_ARM64)
        const bool passUsingFloatRegs =
            (hfaType != TYP_UNDEF) || (!isStructArg && varTypeUsesFloatReg(argx->GetType()));
#elif defined(TARGET_AMD64)
        const bool passUsingFloatRegs = !isStructArg && varTypeIsFloating(argx->GetType());
#elif defined(TARGET_X86)
        const bool passUsingFloatRegs = false;
#else
#error Unsupported or unset target architecture
#endif // TARGET*

        bool      isRegArg         = false;
        bool      isBackFilled     = false;
        bool      isNonStandard    = false;
        regNumber nonStdRegNum     = REG_NA;
        unsigned  nextFltArgRegNum = fltArgRegNum; // This is the next floating-point argument register number to use

#if defined(OSX_ARM64_ABI)
        // Arm64 Apple has a special ABI for passing small size arguments on stack,
        // bytes are aligned to 1-byte, shorts to 2-byte, int/float to 4-byte, etc.
        // It means passing 8 1-byte arguments on stack can take as small as 8 bytes.
        unsigned argAlignBytes = eeGetArgAlignment(sigType, hfaType == TYP_FLOAT);
#endif

//
// Figure out if the argument will be passed in a register.
//

#ifdef TARGET_X86
        if ((isRegParamType(argx->GetType()) && !isStructArg) || (isStructArg && isTrivialPointerSizedStruct(layout)))
#else
        if (isRegParamType(argx->GetType()))
#endif
        {
#ifdef TARGET_ARM
            if (passUsingFloatRegs)
            {
                // First, see if it can be back-filled
                if (!anyFloatStackArgs && // Is it legal to back-fill? (We haven't put any FP args on the stack yet)
                    (fltArgSkippedRegMask != RBM_NONE) && // Is there an available back-fill slot?
                    (size == 1))                          // The size to back-fill is one float register
                {
                    // Back-fill the register.
                    isBackFilled              = true;
                    regMaskTP backFillBitMask = genFindLowestBit(fltArgSkippedRegMask);
                    fltArgSkippedRegMask &=
                        ~backFillBitMask; // Remove the back-filled register(s) from the skipped mask
                    nextFltArgRegNum = genMapFloatRegNumToRegArgNum(genRegNumFromMask(backFillBitMask));
                    assert(nextFltArgRegNum < MAX_FLOAT_REG_ARG);
                }

                // Does the entire float, double, or HFA fit in the FP arg registers?
                // Check if the last register needed is still in the argument register range.
                isRegArg = (nextFltArgRegNum + size - 1) < MAX_FLOAT_REG_ARG;

                if (!isRegArg)
                {
                    anyFloatStackArgs = true;
                }
            }
            else
            {
                isRegArg = intArgRegNum < MAX_REG_ARG;
            }
#elif defined(TARGET_ARM64)
            if (passUsingFloatRegs)
            {
                // Check if the last register needed is still in the fp argument register range.
                isRegArg = (nextFltArgRegNum + (size - 1)) < MAX_FLOAT_REG_ARG;

                // Do we have a HFA arg that we wanted to pass in registers, but we ran out of FP registers?
                if ((hfaType != TYP_UNDEF) && !isRegArg)
                {
                    // recompute the 'size' so that it represent the number of stack slots rather than the number of
                    // registers
                    //
                    size = roundUp(structSize, TARGET_POINTER_SIZE) / TARGET_POINTER_SIZE;

                    // We also must update fltArgRegNum so that we no longer try to
                    // allocate any new floating point registers for args
                    // This prevents us from backfilling a subsequent arg into d7
                    //
                    fltArgRegNum = MAX_FLOAT_REG_ARG;
                }
            }
            else
            {
                // Check if the last register needed is still in the int argument register range.
                isRegArg = (intArgRegNum + (size - 1)) < maxRegArgs;

                // Did we run out of registers when we had a 16-byte struct (size===2) ?
                // (i.e we only have one register remaining but we needed two registers to pass this arg)
                // This prevents us from backfilling a subsequent arg into x7
                //
                if (!isRegArg && (size > 1))
                {
#if defined(TARGET_WINDOWS)
                    // Arm64 windows native varargs allows splitting a 16 byte struct between stack
                    // and the last general purpose register.
                    if (callIsVararg)
                    {
                        // Override the decision and force a split.
                        isRegArg = (intArgRegNum + (size - 1)) <= maxRegArgs;
                    }
                    else
#endif // defined(TARGET_WINDOWS)
                    {
                        // We also must update intArgRegNum so that we no longer try to
                        // allocate any new general purpose registers for args
                        //
                        intArgRegNum = maxRegArgs;
                    }
                }
            }
#elif defined(UNIX_AMD64_ABI)
            // Here a struct can be passed in register following the classifications of its members and size.
            // Now make sure there are actually enough registers to do so.
            if (isStructArg)
            {
                if (layout->GetSysVAmd64AbiRegCount() != 0)
                {
                    unsigned structFloatRegs = 0;
                    unsigned structIntRegs   = 0;

                    for (unsigned i = 0; i < layout->GetSysVAmd64AbiRegCount(); i++)
                    {
                        if (varTypeUsesFloatReg(layout->GetSysVAmd64AbiRegType(i)))
                        {
                            structFloatRegs++;
                        }
                        else
                        {
                            structIntRegs++;
                        }
                    }

                    isRegArg = ((nextFltArgRegNum + structFloatRegs) <= MAX_FLOAT_REG_ARG) &&
                               ((intArgRegNum + structIntRegs) <= MAX_REG_ARG);
                }
            }
            else
            {
                if (passUsingFloatRegs)
                {
                    isRegArg = nextFltArgRegNum < MAX_FLOAT_REG_ARG;
                }
                else
                {
                    isRegArg = intArgRegNum < MAX_REG_ARG;
                }
            }
#else  // !defined(UNIX_AMD64_ABI)
            isRegArg                  = (intArgRegNum + (size - 1)) < maxRegArgs;
#endif // !defined(UNIX_AMD64_ABI)
        }

        // If there are nonstandard args (outside the calling convention) they were inserted above
        // and noted them in a table so we can recognize them here and build their argInfo.
        //
        // They should not affect the placement of any other args or stack space required.
        // Example: on AMD64 R10 and R11 are used for indirect VSD (generic interface) and cookie calls.
        isNonStandard = nonStandardArgs.FindReg(argx, &nonStdRegNum);
        if (isNonStandard)
        {
            isRegArg = (nonStdRegNum != REG_STK);
        }
        else if (call->IsTailCallViaJitHelper())
        {
            // We have already (before calling fgMorphArgs()) appended the 4 special args
            // required by the x86 tailcall helper. These args are required to go on the
            // stack. Force them to the stack here.
            assert(numArgs >= 4);
            if (argIndex >= numArgs - 4)
            {
                isRegArg = false;
            }
        }

        // Now we know if the argument goes in registers or not and how big it is.
        CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef TARGET_ARM
        // If we ever allocate a floating point argument to the stack, then all
        // subsequent HFA/float/double arguments go on the stack.
        if (!isRegArg && passUsingFloatRegs)
        {
            for (; fltArgRegNum < MAX_FLOAT_REG_ARG; ++fltArgRegNum)
            {
                fltArgSkippedRegMask |= genMapArgNumToRegMask(fltArgRegNum, TYP_FLOAT);
            }
        }

        // If we think we're going to split a struct between integer registers and the stack, check to
        // see if we've already assigned a floating-point arg to the stack.
        if (isRegArg &&                            // We decided above to use a register for the argument
            !passUsingFloatRegs &&                 // We're using integer registers
            (intArgRegNum + size > MAX_REG_ARG) && // We're going to split a struct type onto registers and stack
            anyFloatStackArgs)                     // We've already used the stack for a floating-point argument
        {
            isRegArg = false; // Change our mind; don't pass this struct partially in registers

            // Skip the rest of the integer argument registers
            for (; intArgRegNum < MAX_REG_ARG; ++intArgRegNum)
            {
                argSkippedRegMask |= genMapArgNumToRegMask(intArgRegNum, TYP_I_IMPL);
            }
        }
#endif // TARGET_ARM

        CallArgInfo* argInfo;

        if (isRegArg)
        {
            regNumber nextRegNum = REG_STK;

#if defined(UNIX_AMD64_ABI)
            regNumber    nextOtherRegNum = REG_STK;
            unsigned int structFloatRegs = 0;
            unsigned int structIntRegs   = 0;
#endif // defined(UNIX_AMD64_ABI)

            if (isNonStandard)
            {
                nextRegNum = nonStdRegNum;
            }
#if defined(UNIX_AMD64_ABI)
            else if (isStructArg && (layout->GetSysVAmd64AbiRegCount() != 0))
            {
                // It is a struct passed in registers. Assign the next available register.
                assert(layout->GetSysVAmd64AbiRegCount() <= 2);
                regNumber* nextRegNumPtrs[2] = {&nextRegNum, &nextOtherRegNum};
                for (unsigned int i = 0; i < layout->GetSysVAmd64AbiRegCount(); i++)
                {
                    if (!varTypeUsesFloatReg(layout->GetSysVAmd64AbiRegType(i)))
                    {
                        *nextRegNumPtrs[i] = genMapIntRegArgNumToRegNum(intArgRegNum + structIntRegs);
                        ++structIntRegs;
                    }
                    else
                    {
                        *nextRegNumPtrs[i] = genMapFloatRegArgNumToRegNum(nextFltArgRegNum + structFloatRegs);
                        ++structFloatRegs;
                    }
                }
            }
#endif // defined(UNIX_AMD64_ABI)
            else
            {
                // fill in or update the argInfo table
                nextRegNum = passUsingFloatRegs ? genMapFloatRegArgNumToRegNum(nextFltArgRegNum)
                                                : genMapIntRegArgNumToRegNum(intArgRegNum);
            }

#ifdef TARGET_AMD64
#ifndef UNIX_AMD64_ABI
            assert(size == 1);
#endif
#endif

            unsigned regCount = size;
#if FEATURE_ARG_SPLIT
            unsigned firstSlot = 0;
            unsigned slotCount = 0;
#endif

            // Set up the next intArgRegNum and fltArgRegNum values.
            if (!isBackFilled)
            {
#if defined(UNIX_AMD64_ABI)
                if (isStructArg)
                {
                    // For this case, we've already set the regNums in the argTabEntry
                    intArgRegNum += structIntRegs;
                    fltArgRegNum += structFloatRegs;
                }
                else
#endif // defined(UNIX_AMD64_ABI)
                    if (!isNonStandard)
                {
                    if (passUsingFloatRegs)
                    {
                        fltArgRegNum += size;

#ifdef WINDOWS_AMD64_ABI
                        // Whenever we pass an integer register argument
                        // we skip the corresponding floating point register argument
                        intArgRegNum = min(intArgRegNum + size, MAX_REG_ARG);
#endif

                        // No supported architecture supports partial structs using float registers.
                        assert(fltArgRegNum <= MAX_FLOAT_REG_ARG);
                    }
                    else
                    {
#if FEATURE_ARG_SPLIT
                        // Check for a split (partially enregistered) struct
                        if ((intArgRegNum + size) > MAX_REG_ARG)
                        {
                            // This indicates a partial enregistration of a struct type
                            assert((isStructArg) || argx->OperIs(GT_FIELD_LIST) ||
                                   (argx->OperIs(GT_ASG) && varTypeIsStruct(argx->AsOp()->GetOp(0)->GetType())) ||
                                   (argx->gtOper == GT_COMMA && (argx->gtFlags & GTF_ASG)));

                            regCount  = MAX_REG_ARG - intArgRegNum;
                            slotCount = size - regCount;
                            firstSlot = call->fgArgInfo->AllocateStackSlots(slotCount, 1);
                        }
#endif // FEATURE_ARG_SPLIT

                        // Increment intArgRegNum by 'size' registers
                        intArgRegNum += size;

#ifdef WINDOWS_AMD64_ABI
                        fltArgRegNum = min(fltArgRegNum + size, MAX_FLOAT_REG_ARG);
#endif
                    }
                }
            }

#ifdef TARGET_ARM
            // Adjust regCount for DOUBLE args, including HFAs, since up to here we counted 2 regs
            // for every DOUBLE reg the arg needs. For most purposes, we don't care about the fact
            // that a DOUBLE reg actually takes 2 FLOAT regs (e.g. a HFA with 3 elements may end up
            // being turned into a FIELD_LIST with 3 fields, no matter if the HFA type is FLOAT or
            // DOUBLE) so it's preferrable to treat DOUBLE regs as single reg.

            if (hfaType != TYP_UNDEF)
            {
                regCount = hfaSlots;
                if (hfaType == TYP_DOUBLE)
                {
                    // Must be an even number of registers.
                    assert((regCount & 1) == 0);
                    regCount = hfaSlots / 2;
                }
            }
            else if (argx->TypeIs(TYP_DOUBLE) && GlobalJitOptions::compFeatureHfa)
            {
                regCount = 1;
            }
#endif

            argInfo = new (this, CMK_CallInfo) CallArgInfo(argIndex, args, regCount);
            argInfo->SetRegNum(0, nextRegNum);
            argInfo->SetNonStandard(isNonStandard);

#ifdef UNIX_AMD64_ABI
            assert(regCount <= 2);

            if (regCount == 2)
            {
                argInfo->SetRegNum(1, nextOtherRegNum);
            }

            if (isStructArg)
            {
                for (unsigned i = 0; i < regCount; i++)
                {
                    // TODO-MIKE-Review: Does this really need to be the actual type?
                    argInfo->SetRegType(i, varActualType(layout->GetSysVAmd64AbiRegType(i)));
                }
            }
            else
            {
                argInfo->SetRegType(0, argx->GetType());
            }
#elif defined(TARGET_ARMARCH)
            if (hfaType != TYP_UNDEF)
            {
                argInfo->SetRegType(hfaType);
            }
            else if (varTypeIsFloating(argx->GetType()) && GlobalJitOptions::compFeatureHfa)
            {
                argInfo->SetRegType(argx->GetType());
            }
#endif

#if FEATURE_ARG_SPLIT
            if (slotCount != 0)
            {
                argInfo->SetSlots(firstSlot, slotCount);
            }
#endif
        }
        else // We have an argument that is not passed in a register
        {
            argInfo = new (this, CMK_CallInfo) CallArgInfo(argIndex, args, 0);
            argInfo->SetSlots(call->fgArgInfo->AllocateStackSlots(size, argAlign), size);
        }

        if (isStructArg)
        {
            argInfo->SetIsImplicitByRef(passStructByRef);
            argInfo->SetArgType((structBaseType == TYP_UNDEF) ? argx->GetType() : structBaseType);
        }
        else
        {
            argInfo->SetArgType(argx->GetType());
        }

        call->fgArgInfo->AddArg(argInfo);
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("Call [%06u] arg table after fgInitArgInfo:\n", call->gtTreeID);
        call->fgArgInfo->Dump();
        printf("\n");
    }
#endif
}

//------------------------------------------------------------------------
// fgMorphArgs: Walk and transform (morph) the arguments of a call
//
// Arguments:
//    callNode - the call for which we are doing the argument morphing
//
// Return Value:
//    Like most morph methods, this method returns the morphed node,
//    though in this case there are currently no scenarios where the
//    node itself is re-created.
//
// Notes:
//    This calls fgInitArgInfo to create the 'fgArgInfo' for the call.
//    If it has already been created, that method will simply return.
//
//    This method changes the state of the call node. It uses the existence
//    of gtCallLateArgs (the late arguments list) to determine if it has
//    already done the first round of morphing.
//
//    The first time it is called (i.e. during global morphing), this method
//    computes the "late arguments". This is when it determines which arguments
//    need to be evaluated to temps prior to the main argument setup, and which
//    can be directly evaluated into the argument location. It also creates a
//    second argument list (gtCallLateArgs) that does the final placement of the
//    arguments, e.g. into registers or onto the stack.
//
//    The "non-late arguments", aka the gtCallArgs, are doing the in-order
//    evaluation of the arguments that might have side-effects, such as embedded
//    assignments, calls or possible throws. In these cases, it and earlier
//    arguments must be evaluated to temps.
//
//    On targets with a fixed outgoing argument area (FEATURE_FIXED_OUT_ARGS),
//    if we have any nested calls, we need to defer the copying of the argument
//    into the fixed argument area until after the call. If the argument did not
//    otherwise need to be computed into a temp, it is moved to gtCallLateArgs and
//    replaced in the "early" arg list (gtCallArgs) with a placeholder node.
//
GenTreeCall* Compiler::fgMorphArgs(GenTreeCall* call)
{
    bool reMorphing = call->AreArgsComplete();
    fgInitArgInfo(call);

    JITDUMP("%s call [%06u] args\n", reMorphing ? "Remorphing" : "Morphing", call->gtTreeID);

    GenTreeFlags argsSideEffects = GTF_EMPTY;
    unsigned     argNum          = 0;
    // Sometimes we need a second pass to morph args, most commonly for arguments
    // that need to be changed FIELD_LISTs. FIELD_LIST doesn't have a class handle
    // so if the args needs a temp EvalArgsToTemps won't be able to allocate one.
    // Then the first pass does minimal or no morphing of the arg and the second
    // pass replaces the arg with a FIELD_LIST node.
    bool requires2ndPass = false;

    if (call->gtCallThisArg != nullptr)
    {
        GenTree* arg = call->gtCallThisArg->GetNode();
        arg          = fgMorphTree(arg);
        call->gtCallThisArg->SetNode(arg);
        argsSideEffects |= arg->gtFlags;
        argNum++;
    }

    for (GenTreeCall::Use *argUse = call->gtCallArgs; argUse != nullptr; argUse = argUse->GetNext(), argNum++)
    {
        CallArgInfo* argInfo = call->GetArgInfoByArgNum(argNum);

        GenTree* arg = argUse->GetNode();
        arg          = fgMorphTree(arg);
        argUse->SetNode(arg);

        if (argInfo->HasLateUse())
        {
            assert(arg->OperIs(GT_ARGPLACE, GT_ASG) || (arg->OperIs(GT_COMMA) && arg->TypeIs(TYP_VOID)));

            argsSideEffects |= arg->gtFlags;
            continue;
        }

        bool paramIsStruct = typIsLayoutNum(argUse->GetSigTypeNum());

        if (!varTypeIsStruct(arg->GetType()) && (!arg->IsIntegralConst(0) || !paramIsStruct))
        {
            if (paramIsStruct && arg->IsCast() && !arg->gtOverflow() && varTypeIsSmall(arg->AsCast()->GetCastType()) &&
                (varTypeSize(arg->AsCast()->GetCastType()) == typGetLayoutByNum(argUse->GetSigTypeNum())->GetSize()))
            {
                // This is a struct arg that became a primitive type arg due to struct promotion.
                // Promoted struct fields are "normalized on load" but we don't need normalization
                // because struct args do not need to be widened so we can drop the normalization
                // cast.

                arg = arg->AsCast()->GetOp(0);
                argUse->SetNode(arg);
            }

#if defined(TARGET_WINDOWS) || defined(TARGET_ARM)
            // win-arm64 varargs and arm-soft-fp pass floating point args in integer registers.
            // win-x64 passes single float/double field structs in integer registers, if we
            // promoted the struct, or if a float/double local was reinterpreted as a single
            // FP field struct we need to bitcast the FP value to integer.
            if ((argInfo->GetRegCount() != 0) && genIsValidIntReg(argInfo->GetRegNum(0)) &&
#ifdef TARGET_ARM
                // Decomposition doesn't support LONG BITCAST so we'll have to handle the DOUBLE
                // case in lowering/codegen.
                arg->TypeIs(TYP_FLOAT)
#else
                arg->TypeIs(TYP_FLOAT, TYP_DOUBLE)
#endif
                    )
            {
                arg = gtNewBitCastNode(arg->TypeIs(TYP_FLOAT) ? TYP_INT : TYP_LONG, arg);
                argUse->SetNode(arg);
            }
#endif // (defined(TARGET_ARM64) && defined(TARGET_WINDOWS)) || defined(TARGET_ARM)

            bool argMatchesRegType =
                (argInfo->GetRegCount() == 0) ||
#ifdef TARGET_ARM
                (arg->TypeIs(TYP_DOUBLE) && (argInfo->GetRegCount() == 2) && genIsValidIntReg(argInfo->GetRegNum(0))) ||
#endif
                (varTypeUsesFloatReg(arg->GetType()) == genIsValidFloatReg(argInfo->GetRegNum(0)));
            assert(argMatchesRegType);

            argsSideEffects |= arg->gtFlags;
            continue;
        }

        // Non-standard args are expected to have primitive type.
        assert(!argInfo->IsNonStandard());

        // TODO-MIKE-Review: Can we get COMMAs here other than those generated for
        // temp arg copies? The struct arg morph code below doesn't handle that.
        GenTree* argVal = arg->SkipComma();

        if (argVal->OperIs(GT_ASG, GT_FIELD_LIST, GT_ARGPLACE))
        {
            // Skip arguments that have already been transformed.
            argsSideEffects |= arg->gtFlags;
            continue;
        }

#ifdef TARGET_64BIT
        if (argInfo->IsImplicitByRef())
        {
            assert(argInfo->IsSingleRegOrSlot());
            abiMorphImplicitByRefStructArg(call, argInfo);
            argsSideEffects |= argUse->GetNode()->gtFlags;
            continue;
        }
#endif

        if (argInfo->GetRegCount() != 0)
        {
#if FEATURE_MULTIREG_ARGS
            if (argInfo->GetRegCount() + argInfo->GetSlotCount() > 1)
            {
                requires2ndPass |= true;
            }
            else
#endif
            {
                abiMorphSingleRegStructArg(argInfo, argVal);
            }

            argsSideEffects |= argUse->GetNode()->gtFlags;
            continue;
        }

        requires2ndPass |= abiMorphStackStructArg(argInfo, argVal);
        argsSideEffects |= argUse->GetNode()->gtFlags;
    }

    if (reMorphing)
    {
        for (GenTreeCall::Use& use : call->LateArgs())
        {
            use.SetNode(fgMorphTree(use.GetNode()));
            argsSideEffects |= use.GetNode()->gtFlags;
        }

        assert(call->fgArgInfo != nullptr);
    }
    else
    {
        call->fgArgInfo->ArgsComplete(this, call);
    }

    if (call->gtCallType == CT_INDIRECT)
    {
        call->gtCallAddr = fgMorphTree(call->gtCallAddr);
        // Const CSE may create an assignment node here
        argsSideEffects |= call->gtCallAddr->gtFlags;
    }

#if defined(UNIX_AMD64_ABI)
    if (!call->IsFastTailCall())
    {
        // This is currently required for the UNIX ABI to work correctly.
        opts.compNeedToAlignFrame = true;
    }
#endif // UNIX_AMD64_ABI

    // Clear the ASG and EXCEPT (if possible) flags on the call node
    call->gtFlags &= ~GTF_ASG;
    if (!call->OperMayThrow(this))
    {
        call->gtFlags &= ~GTF_EXCEPT;
    }

    call->gtFlags |= argsSideEffects & GTF_ALL_EFFECT;

#ifdef TARGET_X86
    assert(!requires2ndPass);
#else
    if (requires2ndPass)
    {
        abiMorphArgs2ndPass(call);
    }
#endif

#ifdef DEBUG
    if (verbose)
    {
        printf("Call [%06u] arg table after fgMorphArgs:\n", call->gtTreeID);
        call->fgArgInfo->Dump();
    }
#endif

    return call;
}

bool Compiler::abiMorphStackStructArg(CallArgInfo* argInfo, GenTree* arg)
{
    assert(argInfo->GetRegCount() == 0);

    if (arg->IsIntegralConst(0))
    {
        return false;
    }

    assert(varTypeIsStruct(arg->GetType()));

    if (arg->OperIs(GT_MKREFANY))
    {
#ifdef TARGET_X86
        abiMorphMkRefAnyToFieldList(argInfo, arg->AsOp());
        return false;
#else
        return true;
#endif
    }

    if (arg->OperIs(GT_LCL_VAR) && varTypeIsStruct(arg->GetType()) &&
        lvaGetDesc(arg->AsLclVar())->IsIndependentPromoted())
    {
        LclVarDsc* lcl = lvaGetDesc(arg->AsLclVar());

        if (lcl->GetPromotedFieldCount() > 1)
        {
            // If we need more than one field we need to generate a FIELD_LIST.
            // If this argument ends up needing a temp then EvalArgsToTemps will
            // need the struct layout to create the temp and FIELD_LIST doesn't
            // have layout. So we have to do this transform after EvalArgsToTemps.
            // On x86 we never need temps for stack args so we do it here.
            CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef TARGET_X86
            abiMorphStackLclArgPromoted(argInfo, arg->AsLclVar());
            return false;
#else
            return true;
#endif
        }

        LclVarDsc* fieldLcl  = lvaGetDesc(lcl->GetPromotedFieldLclNum(0));
        var_types  fieldType = fieldLcl->GetType();

        assert(roundUp(varTypeSize(fieldType), REGSIZE_BYTES) <= argInfo->GetSlotCount() * REGSIZE_BYTES);

        arg->AsLclVar()->SetLclNum(lcl->GetPromotedFieldLclNum(0));
        arg->SetType(fieldType);
        arg->gtFlags = GTF_EMPTY;

        argInfo->SetArgType(fieldType);

        return false;
    }

    if (arg->OperIs(GT_OBJ))
    {
        INDEBUG(GenTreeLclVarCommon* lclNode = arg->AsObj()->GetAddr()->IsLocalAddrExpr();)
        assert((lclNode == nullptr) || lvaGetDesc(lclNode)->lvDoNotEnregister);
    }

    if (arg->TypeIs(TYP_STRUCT) && (argInfo->GetArgType() != TYP_STRUCT))
    {
        // While not required for corectness, we can change the type of a struct arg to
        // be a primitive type of suitable size (e.g. a 2 byte struct can be treated as
        // USHORT. Currently CSE does not handle STRUCT OBJs but it can CSE an IND, even
        // if this is a form a reinterpretation that has other limitations in VN/CSE.
        //
        // TODO-MIKE-CQ: Investigate reinterpretation effect on VN. For example, would
        // VN be able to convert from a "zero map" to any primitive type in order to
        // const propagate default struct initialization?

        assert(argInfo->GetSlotCount() * REGSIZE_BYTES == roundUp(varTypeSize(argInfo->GetArgType()), REGSIZE_BYTES));

        var_types argType   = argInfo->GetArgType();
        bool      canRetype = false;

        if (arg->OperIs(GT_OBJ))
        {
            canRetype = varTypeSize(argType) <= arg->AsObj()->GetLayout()->GetSize();

            if (canRetype)
            {
                arg->ChangeOper(GT_IND);
            }
        }
        else if (arg->OperIs(GT_LCL_FLD))
        {
            canRetype = arg->AsLclFld()->GetLclOffs() + varTypeSize(argType) <= lvaGetDesc(arg->AsLclFld())->lvSize();

            if (canRetype)
            {
                arg->AsLclFld()->SetFieldSeq(FieldSeqStore::NotAField());
            }
        }
        else if (arg->OperIs(GT_LCL_VAR))
        {
            canRetype = true;
            lvaSetVarDoNotEnregister(arg->AsLclVar()->GetLclNum() DEBUGARG(DNER_LocalField));
            arg->ChangeOper(GT_LCL_FLD);
        }

        if (canRetype)
        {
            if (varTypeIsSmall(argType))
            {
                // argType is a signed type but this is a struct so sign extension isn't necessary.
                // On XARCH it causes MOVSX to be generated, which has larger encoding than MOVZX.
                argType = varTypeToUnsigned(argType);
            }

            arg->SetType(argType);
        }
    }

    return false;
}

void Compiler::abiMorphStackLclArgPromoted(CallArgInfo* argInfo, GenTreeLclVar* arg)
{
    assert(argInfo->GetRegCount() == 0);

    if (!lvaGetDesc(arg)->IsIndependentPromoted())
    {
        return;
    }

    LclVarDsc* lcl = lvaGetDesc(arg);
    assert(lcl->GetPromotedFieldCount() > 1);

    GenTreeFieldList* fieldList = abiMakeFieldList(arg);

    for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); i++)
    {
        unsigned       fieldLclNum = lcl->GetPromotedFieldLclNum(i);
        LclVarDsc*     fieldLcl    = lvaGetDesc(fieldLclNum);
        var_types      fieldType   = fieldLcl->GetType();
        unsigned       fieldOffset = fieldLcl->GetPromotedFieldOffset();
        GenTreeLclVar* fieldLclVar = gtNewLclvNode(fieldLclNum, fieldType);

        assert(fieldOffset + varTypeSize(fieldType) <= argInfo->GetSlotCount() * REGSIZE_BYTES);

        fieldList->AddField(this, fieldLclVar, fieldOffset, fieldType);
    }
}

void Compiler::abiMorphMkRefAnyToFieldList(CallArgInfo* argInfo, GenTreeOp* arg)
{
    assert(argInfo->GetRegCount() + argInfo->GetSlotCount() == 2);

    GenTree* dataPtr = arg->GetOp(0);
    GenTree* type    = arg->GetOp(1);

    GenTreeFieldList* fieldList = abiMakeFieldList(arg);
    fieldList->gtFlags          = GTF_EMPTY;
    fieldList->AddField(this, dataPtr, OFFSETOF__CORINFO_TypedReference__dataPtr, TYP_BYREF);
    fieldList->AddField(this, type, OFFSETOF__CORINFO_TypedReference__type, TYP_I_IMPL);
}

GenTreeFieldList* Compiler::abiMakeFieldList(GenTree* arg)
{
    arg->ChangeOper(GT_FIELD_LIST);
    arg->SetType(TYP_STRUCT);
    arg->gtFlags = GTF_CONTAINED;
    return arg->AsFieldList();
}

#ifndef TARGET_X86

void Compiler::abiMorphArgs2ndPass(GenTreeCall* call)
{
    for (unsigned i = 0; i < call->GetInfo()->GetArgCount(); i++)
    {
        CallArgInfo* argInfo = call->GetInfo()->GetArgInfo(i);
        GenTree*     arg     = argInfo->GetNode();

        if (arg->OperIs(GT_MKREFANY))
        {
            abiMorphMkRefAnyToFieldList(argInfo, arg->AsOp());
            continue;
        }

        if (argInfo->GetRegCount() == 0)
        {
            arg = arg->SkipComma();

            if (arg->OperIs(GT_LCL_VAR))
            {
                abiMorphStackLclArgPromoted(argInfo, arg->AsLclVar());
            }
            continue;
        }

#if FEATURE_MULTIREG_ARGS
        if (argInfo->GetRegCount() + argInfo->GetSlotCount() > 1)
        {
            if (arg->IsIntegralConst(0) || (varTypeIsStruct(arg->GetType()) && !arg->IsFieldList()))
            {
                GenTree* newArg = abiMorphMultiRegStructArg(argInfo, arg);

                if (newArg != arg)
                {
                    argInfo->SetNode(newArg);
                }
            }

            continue;
        }
#endif
    }
}

#endif

void Compiler::abiMorphSingleRegStructArg(CallArgInfo* argInfo, GenTree* arg)
{
    assert((argInfo->GetRegCount() == 1) && (argInfo->GetSlotCount() == 0));

    var_types argRegType = argInfo->GetArgType();
    unsigned  argSize    = 0;

    if (varTypeIsSmall(argRegType))
    {
        // This being a struct, sign extension isn't needed so use unsigned small int types.
        // On XARCH we get MOVZX which may end up being shorter than MOVSX.
        argRegType = varTypeToUnsigned(argRegType);
    }

    if (arg->IsIntegralConst(0))
    {
        if (varTypeIsFloating(argRegType))
        {
            arg->ChangeToDblCon(TYP_DOUBLE, 0);
        }
        else if (varTypeIsLong(argRegType))
        {
            arg->SetType(TYP_LONG);
        }
        else if (varTypeIsGC(argRegType))
        {
            arg->SetType(TYP_I_IMPL);
        }
#ifdef FEATURE_HW_INTRINSICS
        else if (varTypeIsSIMD(argRegType))
        {
            arg->ChangeOper(GT_HWINTRINSIC);
            arg->SetType(argRegType);
            arg->AsHWIntrinsic()->SetIntrinsic(GetZeroSimdHWIntrinsic(argRegType), TYP_FLOAT, varTypeSize(argRegType),
                                               0);
        }
#endif
        else
        {
            assert(varActualType(argRegType) == TYP_INT);
        }

        return;
    }

    if (arg->OperIs(GT_OBJ))
    {
        argSize = arg->AsObj()->GetLayout()->GetSize();

        assert(argSize <= argRegType);

        INDEBUG(GenTreeLclVarCommon* lclNode = arg->AsObj()->GetAddr()->IsLocalAddrExpr();)
        assert((lclNode == nullptr) || lvaGetDesc(lclNode)->lvDoNotEnregister);

#if (defined(TARGET_AMD64) && !defined(UNIX_AMD64_ABI)) || defined(TARGET_X86)
        // On win-x64 and x86 only register sized structs are passed in a register, others are passed by reference.
        assert(argSize == varTypeSize(argRegType));
#else
        // On all other targets structs smaller than register size can be passed in a register, this includes
        // structs that not only that they're smaller but they also don't match any available load instruction
        // size (3, 5, 6...) and that will require additional processing.
        assert(argSize <= varTypeSize(argRegType));

        if (!isPow2(argSize))
        {
#ifdef TARGET_64BIT
            assert((argSize == 3) || (argSize == 5) || (argSize == 6) || (argSize == 7));
#else
            assert(argSize == 3);
#endif
            assert(arg->TypeIs(TYP_STRUCT));

            GenTree* addr           = arg->AsObj()->GetAddr();
            ssize_t  addrOffset     = 0;
            GenTree* addrTempAssign = abiMakeIndirAddrMultiUse(&addr, &addrOffset, argSize);

            arg = abiNewMultiLoadIndir(addr, addrOffset, argSize);

            if (addrTempAssign != nullptr)
            {
                arg = gtNewCommaNode(addrTempAssign, arg);
            }

            argInfo->use->SetNode(arg);
            return;
        }
#endif // !defined(TARGET_AMD64) || defined(UNIX_AMD64_ABI)

        arg->ChangeOper(GT_IND);
        arg->SetType(argRegType);

        return;
    }

    if (arg->OperIs(GT_LCL_VAR))
    {
        // Independent promoted locals require special handling. This includes promoted SIMD locals,
        // such as a promoted SIMD8 local being passed in a LONG register on win-x64.

        LclVarDsc* varDsc = lvaGetDesc(arg->AsLclVar());

        if (varDsc->IsPromoted())
        {
            if (argSize == 0)
            {
                argSize = varDsc->GetLayout()->GetSize();
            }

            GenTree* newArg = abiMorphSingleRegLclArgPromoted(arg->AsLclVar(), argRegType, argSize);

            if (newArg != arg)
            {
                argInfo->use->SetNode(newArg);
            }

            return;
        }
    }

#ifdef TARGET_64BIT
    if (arg->TypeIs(TYP_SIMD8) && (argRegType == TYP_LONG))
    {
        // win-x64 and win-arm64 varargs pass SIMD8 in a LONG register.
        argInfo->use->SetNode(gtNewBitCastNode(argRegType, arg));
        return;
    }
#endif

    // At this point we have either an arbitrary SIMD tree or a LCL_VAR|FLD of
    // either SIMD or primitive type. The arbitrary SIMD tree should already be
    // using the correct register type but the LCL_VAR may have the wrong type
    // due to reinterpretation.

    if (!arg->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
#if defined(UNIX_AMD64_ABI)
        // SIMD8 is the only SIMD type passed in a single register on UNIX_AMD64_ABI.
        // SIMD16 & SIMD32 should also be passed in a single XMM register but the ABI
        // is currently broken.
        assert(arg->TypeIs(TYP_SIMD8) && (argRegType == TYP_DOUBLE));
#elif defined(TARGET_AMD64)
        // On win-x64 the only SIMD type passed in a register is SIMD8 and we have
        // already handled that case. vectorcall is not currently supported.
        unreached();
#elif defined(TARGET_ARM64)
        // On ARM64 SIMD8 and SIMD16 types may be passed in a vector register.
        // SIMD12 is always a HFA and SIMD32 doesn't exist.
        assert(arg->TypeIs(TYP_SIMD8, TYP_SIMD16));
        assert(argRegType == arg->GetType());
#else
        // Other targets don't have SIMD types or pass them on the stack (x86).
        unreached();
#endif
        return;
    }

    // Normally at this point we should have a STRUCT or suitable SIMD typed LCL_VAR|FLD
    // but due to reinterpretation via OBJ(ADDR(LCL_VAR)) the LCL_VAR|FLD may also have
    // primtive type or an unexpected SIMD type (e.g. SIMD16 local passed in an INT reg).
    // For now use LCL_FLD to handle all such cases. In some cases BITCAST may be used
    // but it's not clear which such cases, if any, are useful.

    if (arg->TypeIs(TYP_STRUCT) || (varTypeUsesFloatReg(argRegType) != varTypeUsesFloatReg(arg->GetType())))
    {
        if (arg->OperIs(GT_LCL_FLD))
        {
            arg->AsLclFld()->SetFieldSeq(FieldSeqStore::NotAField());
        }
        else
        {
            arg->ChangeOper(GT_LCL_FLD);
        }

        arg->SetType(argRegType);

        lvaSetVarDoNotEnregister(arg->AsLclFld()->GetLclNum() DEBUGARG(DNER_LocalField));
    }
}

GenTree* Compiler::abiMorphSingleRegLclArgPromoted(GenTreeLclVar* arg, var_types argRegType, unsigned argSize)
{
    assert(argSize <= varTypeSize(argRegType));
    assert(varTypeIsSingleReg(argRegType));

    LclVarDsc* lcl = lvaGetDesc(arg);
    assert(argSize <= lcl->GetLayout()->GetSize());

    LclVarDsc* fieldLcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(0));
    assert(!fieldLcl->TypeIs(TYP_STRUCT));

    if ((fieldLcl->GetPromotedFieldOffset() == 0) && (argSize <= varTypeSize(fieldLcl->GetType())) &&
        varTypeIsSingleReg(fieldLcl->GetType()))
    {
        // Handle the common case when the first field of the struct is large enough and can be loaded
        // directly into the arg register. That's almost all single field structs, except those having
        // a floating point or SIMD8 field that may need to be passed in an integer register (win-x64,
        // win-arm64 varargs).

        if (varTypeUsesFloatReg(fieldLcl->GetType()) == varTypeUsesFloatReg(argRegType))
        {
            arg->SetLclNum(lcl->GetPromotedFieldLclNum(0));
            arg->SetType(fieldLcl->GetType());
            return arg;
        }

#ifdef TARGET_64BIT
        // Handle the win-x64 and win-arm64 varargs case. This doesn't occur now for floating point
        // because single FLOAT/DOUBLE field structs aren't promoted. But it can be tested by
        // reinterpreting a struct with more than one field.

        if (((fieldLcl->GetType() == TYP_FLOAT) && (argRegType == TYP_INT)) ||
            ((fieldLcl->GetType() == TYP_DOUBLE) && (argRegType == TYP_LONG)) ||
            ((fieldLcl->GetType() == TYP_SIMD8) && (argRegType == TYP_LONG)))
        {
            arg->SetLclNum(lcl->GetPromotedFieldLclNum(0));
            arg->SetType(fieldLcl->GetType());
            return gtNewBitCastNode(argRegType, arg);
        }
#endif // TARGET_64BIT
    }

    if (lcl->lvDoNotEnregister)
    {
        // If we need to use more than one field and the local is already DNER then just use LCL_FLD.
        // Otherwise we'd just be generating multiple memory loads.

        arg->ChangeOper(GT_LCL_FLD);
        arg->SetType(argRegType);
        return arg;
    }

#ifdef TARGET_AMD64
    // Special case for AMD64 ABIs - 2 float fields are passed in a single XMM reg on linux-x64 and
    // an integer (LONG) reg on win-x64.
    if ((lcl->GetPromotedFieldCount() >= 2) &&
#ifdef UNIX_AMD64_ABI
        (argRegType == TYP_DOUBLE)
#else
        (argRegType == TYP_LONG)
#endif
            )
    {
        LclVarDsc* field0Lcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(0));
        LclVarDsc* field1Lcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(1));

        if ((field0Lcl->GetType() == TYP_FLOAT) && (field0Lcl->GetPromotedFieldOffset() == 0) &&
            (field1Lcl->GetType() == TYP_FLOAT) && (field1Lcl->GetPromotedFieldOffset() == 4))
        {
            GenTreeLclVar* field0LclNode = gtNewLclvNode(lcl->GetPromotedFieldLclNum(0), TYP_FLOAT);
            GenTreeLclVar* field1LclNode = gtNewLclvNode(lcl->GetPromotedFieldLclNum(1), TYP_FLOAT);

            GenTree* doubleValue =
                gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_FLOAT, 16, field0LclNode, field1LclNode);

            return gtNewSimdGetElementNode(TYP_SIMD16, argRegType, doubleValue, gtNewIconNode(0));
        }
    }
#endif // TARGET_AMD64

    // At this point the only remaining interesting cases are structs with more than one integer field
    // passed in an integer register (e.g. 4 UBYTE fields passed in an INT register). Also, on 64 bit
    // targets a FLOAT and an INT may be passed in a LONG register.
    //
    // Other interesting cases may arise due to reinterpretation but it's not clear which ones would be
    // useful so let's keep this simple for now. One potential such case would be passing vector like
    // structs as real vectors (e.g. a 4 FLOAT field struct passed as SIMD16).

    GenTree* newArg = nullptr;

    if (varTypeIsIntegral(argRegType))
    {
        for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); i++)
        {
            unsigned   fieldLclNum = lcl->GetPromotedFieldLclNum(i);
            LclVarDsc* fieldLcl    = lvaGetDesc(fieldLclNum);
            unsigned   fieldOffset = fieldLcl->GetPromotedFieldOffset();
            unsigned   fieldSize   = varTypeSize(fieldLcl->GetType());

            if (fieldOffset >= argSize)
            {
                // The local struct has more fields but we don't need all of them if the arg is smaller.
                break;
            }

#ifdef TARGET_64BIT
            // Even if the arg reg type is LONG we don't have to use LONG all other the place.
            // For example, the first 2 SHORT fields of a 3 SHORT field struct can be packed using
            // 32 bit operations. This reduces the need for INT to LONG casts, that the JIT doesn't
            // handle very well. On X64 this also reduces the need for REX prefixes.
            var_types newArgType = fieldOffset + fieldSize > 4 ? TYP_LONG : TYP_INT;
#else
            var_types newArgType     = TYP_INT;
#endif

            GenTree* field = gtNewLclvNode(fieldLclNum, fieldLcl->GetType());

            if (varTypeIsSmall(fieldLcl->GetType()))
            {
                // Promoted fields are always normalize on load currently.
                assert(fieldLcl->lvNormalizeOnLoad());

                // In general we need to zero extend small int values so the upper bits don't
                // interfere with subsequent fields.
                bool needZeroExtend = true;

                if (fieldOffset + fieldSize >= argSize)
                {
                    // Field is at the end of the arg struct, bits past the end of the arg struct
                    // aren't required to have any particular value in any ABI.
                    needZeroExtend = false;
                }
                else if ((argSize > 4) && (newArgType == TYP_INT) && ((i + 1) < lcl->GetPromotedFieldCount()) &&
                         (lvaGetDesc(lcl->GetPromotedFieldLclNum(i + 1))->GetPromotedFieldOffset() >= 4))
                {
                    // This is the last field in an intermediary INT value. There may be padding
                    // bits after it but they're not required to have a specific value. And we're
                    // going to zero extend to LONG so the upper 32 bit will be zero anyway.
                    // This covers the case of Nullable<int|float> and other similar "tagged" values,
                    // such as SqlInt32.
                    needZeroExtend = false;
                }

                if (needZeroExtend)
                {
                    var_types type = varTypeToUnsigned(field->GetType());

                    if (!optLocalAssertionProp ||
                        (optAssertionIsSubrange(field, TYP_INT, type, apFull) == NO_ASSERTION_INDEX))
                    {
                        field->SetType(TYP_INT);
                        field = gtNewCastNode(TYP_INT, field, false, type);
                    }
                }
            }
#ifdef TARGET_64BIT
            else if (fieldLcl->GetType() == TYP_INT)
            {
                // INT may need to be widened to LONG later.
            }
            else if (fieldLcl->GetType() == TYP_FLOAT)
            {
                // On 64 bit targets a FLOAT and and INT field may end up being passed in a LONG register.
                field = gtNewBitCastNode(TYP_INT, field);
            }
#endif
            else
            {
                // We've run into a LONG/REF/BYREF/DOUBLE/SIMDn field. The normal cases involving such types have
                // already been handled so this can only happen due to weird reinterpretation (e.g. struct with a
                // LONG field being passed as a struct with an INT field). Just give up and use LCL_FLD.
                newArg = nullptr;
                break;
            }

#ifdef TARGET_64BIT
            if (newArgType == TYP_LONG)
            {
                field = gtNewCastNode(TYP_LONG, field, true, TYP_LONG);
            }
#endif

            if (fieldOffset != 0)
            {
                // TODO-MIKE-CQ: On ARM32 OR + LSH should produced a single ORR with shifted register
                // instruction but lowering/codegen doesn't know that. ARM64 does generate ORR with
                // shifter register and UBFX but not BFI and sometimes that would be useful too.
                field = gtNewOperNode(GT_LSH, newArgType, field, gtNewIconNode(static_cast<ssize_t>(fieldOffset) * 8));
            }

            if (newArg == nullptr)
            {
                newArg = field;
            }
            else
            {
#ifdef TARGET_64BIT
                if (newArg->GetType() != newArgType)
                {
                    newArg = gtNewCastNode(TYP_LONG, newArg, true, TYP_LONG);
                }
#endif

                newArg = gtNewOperNode(GT_OR, newArgType, newArg, field);
            }
        }
    }

    if (newArg == nullptr)
    {
        arg->ChangeOper(GT_LCL_FLD);
        arg->SetType(argRegType);
        lvaSetVarDoNotEnregister(arg->GetLclNum() DEBUGARG(DNER_LocalField));
        return arg;
    }

    return newArg;
}

#ifndef TARGET_X86

GenTree* Compiler::abiMorphMkRefAnyToStore(unsigned tempLclNum, GenTreeOp* mkrefany)
{
    GenTreeLclFld* destPtrField = gtNewLclFldNode(tempLclNum, TYP_I_IMPL, OFFSETOF__CORINFO_TypedReference__dataPtr);
    destPtrField->SetFieldSeq(GetFieldSeqStore()->CreateSingleton(GetRefanyDataField()));
    GenTree* asgPtrField = gtNewAssignNode(destPtrField, mkrefany->GetOp(0));

    GenTreeLclFld* destTypeField = gtNewLclFldNode(tempLclNum, TYP_I_IMPL, OFFSETOF__CORINFO_TypedReference__type);
    destTypeField->SetFieldSeq(GetFieldSeqStore()->CreateSingleton(GetRefanyTypeField()));
    GenTree* asgTypeField = gtNewAssignNode(destTypeField, mkrefany->GetOp(1));

    return gtNewCommaNode(asgPtrField, asgTypeField);
}

#endif

#if FEATURE_MULTIREG_ARGS || FEATURE_MULTIREG_RET

GenTree* Compiler::abiMorphMultiRegHfaLclArgPromoted(CallArgInfo* argInfo, GenTreeLclVar* arg)
{
    assert(argInfo->IsHfaArg());
    assert(varTypeUsesFloatReg(argInfo->GetRegType(0)));
    assert(argInfo->GetSlotCount() == 0);

    LclVarDsc* lcl       = lvaGetDesc(arg);
    var_types  regType   = argInfo->GetRegType(0);
    unsigned   regCount  = argInfo->GetRegCount();
    unsigned   regSize   = varTypeSize(regType);
    unsigned   regOffset = 0;

    for (unsigned field = 0; field < lcl->GetPromotedFieldCount(); field++)
    {
        unsigned   fieldLclNum = lcl->GetPromotedFieldLclNum(field);
        LclVarDsc* fieldLcl    = lvaGetDesc(fieldLclNum);
        var_types  fieldType   = fieldLcl->GetType();

        if (regOffset != fieldLcl->GetPromotedFieldOffset())
        {
            break;
        }

        if (regType == fieldType)
        {
            regOffset += regSize;
        }
        else if (varTypeIsSIMD(fieldType) && (regType == TYP_FLOAT))
        {
            regOffset += varTypeSize(fieldType);
        }
        else
        {
            break;
        }
    }

    if (regOffset != regCount * regSize)
    {
        return abiMorphMultiRegLclArg(argInfo, arg);
    }

    GenTreeFieldList* list = new (this, GT_FIELD_LIST) GenTreeFieldList();

    for (unsigned reg = 0, field = 0; reg < regCount; reg++)
    {
        unsigned regOffset = reg * regSize;

        unsigned   fieldLclNum = lcl->GetPromotedFieldLclNum(field);
        LclVarDsc* fieldLcl    = lvaGetDesc(fieldLclNum);
        unsigned   fieldOffset = fieldLcl->GetPromotedFieldOffset();
        var_types  fieldType   = fieldLcl->GetType();

        GenTree* fieldNode = gtNewLclvNode(fieldLclNum, fieldType);

#ifdef FEATURE_SIMD
        if (fieldType != regType)
        {
            assert(regType == TYP_FLOAT);
            assert((regOffset >= fieldOffset) && (regOffset < fieldOffset + varTypeSize(fieldType)));
            assert((regOffset - fieldOffset) % 4 == 0);

            fieldNode =
                gtNewSimdGetElementNode(fieldType, regType, fieldNode, gtNewIconNode((regOffset - fieldOffset) / 4));

            if (regOffset + regSize >= fieldOffset + varTypeSize(fieldType))
            {
                field++;
            }
        }
        else
#endif
        {
            assert(fieldType == regType);
            assert(regOffset == fieldOffset);

            field++;
        }

        list->AddField(this, fieldNode, regOffset, regType);
    }

    return list;
}

struct AbiRegFieldMap
{
    struct
    {
        uint8_t   reg;
        var_types type;
        uint16_t  offset;
        unsigned  lclNum;
    } fields[12];

    unsigned count = 0;
    unsigned firstStackField;

    AbiRegFieldMap(Compiler* compiler, LclVarDsc* lcl, CallArgInfo* argInfo)
    {
        assert(!argInfo->IsHfaArg());

        unsigned regCount = argInfo->GetRegCount();
        unsigned field    = 0;
        unsigned reg      = 0;

        while (reg < regCount && field < lcl->GetPromotedFieldCount())
        {
            unsigned   fieldLclNum = lcl->GetPromotedFieldLclNum(field);
            LclVarDsc* fieldLcl    = compiler->lvaGetDesc(fieldLclNum);
            unsigned   fieldOffset = fieldLcl->GetPromotedFieldOffset();

            if (fieldOffset >= reg * REGSIZE_BYTES + REGSIZE_BYTES)
            {
                if ((count == 0) || (fields[count - 1].reg != reg))
                {
                    // This register doesn't overlap any promoted field. Must be padding
                    // so we can just load 0. We have to add the register to the list
                    // otherwise the backend may not allocate the register correctly.
                    auto& f  = fields[count++];
                    f.reg    = static_cast<uint8_t>(reg);
                    f.lclNum = BAD_VAR_NUM;
                }

                reg++;
                continue;
            }

            assert(count < _countof(fields));

            auto& f  = fields[count++];
            f.reg    = static_cast<uint8_t>(reg);
            f.type   = fieldLcl->GetType();
            f.offset = static_cast<uint16_t>(fieldOffset);
            f.lclNum = fieldLclNum;

            if (fieldOffset + varTypeSize(f.type) <= reg * REGSIZE_BYTES + REGSIZE_BYTES)
            {
                field++;
            }
            else
            {
                reg++;
            }
        }

        firstStackField = count;

        if (argInfo->GetSlotCount() > 0)
        {
            unsigned argSize = (argInfo->GetRegCount() + argInfo->GetSlotCount()) * REGSIZE_BYTES;

            while (field < lcl->GetPromotedFieldCount())
            {
                unsigned   fieldLclNum = lcl->GetPromotedFieldLclNum(field);
                LclVarDsc* fieldLcl    = compiler->lvaGetDesc(fieldLclNum);
                unsigned   fieldOffset = fieldLcl->GetPromotedFieldOffset();

                if (fieldOffset >= argSize)
                {
                    break;
                }

                auto& f  = fields[count++];
                f.reg    = static_cast<uint8_t>(reg++);
                f.type   = fieldLcl->GetType();
                f.offset = static_cast<uint16_t>(fieldOffset);
                f.lclNum = fieldLclNum;

                field++;
            }
        }
    }

    bool IsSupported(CallArgInfo* argInfo) const
    {
        for (unsigned i = 0; i < count && fields[i].reg < argInfo->GetRegCount(); i++)
        {
            // For now we allow only integral fields that have matching register/field
            // offsets. This implies that only a single integral field can be passed
            // in a register. abiMorphSingleRegLclArgPromoted can be used to pack
            // multiple integral fields in a single register but the the CQ results are
            // currently a bit hit and miss.

            if ((fields[i].lclNum != BAD_VAR_NUM) && !varTypeUsesFloatReg(fields[i].type) &&
                (fields[i].offset != fields[i].reg * REGSIZE_BYTES))
            {
                return false;
            }

            if ((i > 0) && (fields[i - 1].reg == fields[i].reg) &&
                !(varTypeUsesFloatReg(fields[i - 1].type) && varTypeUsesFloatReg(fields[i].type)))
            {
                return false;
            }
        }

        // Make sure we don't have a field that crosses the arg boundary, codegen does
        // not handle it properly. This can only happen due to struct reinterpretation in
        // user code. In some cases we could narrow down the field but it's not worth it.

        unsigned fieldSize = fields[count - 1].offset + varTypeSize(fields[count - 1].type);
        unsigned argSize   = (argInfo->GetRegCount() + argInfo->GetSlotCount()) * REGSIZE_BYTES;

        if (fieldSize > argSize)
        {
            return false;
        }

        if (firstStackField < count)
        {
            // The first stack field must not straddle the reg-stack boundary, codegen does
            // not handle this well or at all. Alignment requirements normally prevent this
            // from happening, at least for primitive types. Vector2/3/4 can cross this
            // boundary easily, due to their 4 byte alignment, but they're not supported on
            // ARM and on ARM64 split args are very rare (win-arm64 varargs only).
            //
            // Alignment requirements can be circumvented by reinterpretation (e.g. struct
            // with 2 DOUBLE/LONG fields passed as struct with 4 INT fields, the later has
            // alignment 4 and thus can be passed in r1,r2,r3 and a stack slot,
            // see split-arg-double-reg-and-stack.cs). It's somewhat difficult to handle this
            // kind of splitting for DOUBLE and it's too unusual to even bother trying.
            //
            // We'll make an exception for LONG on ARM though, it gets decomposed to 2 INTs
            // so it works just fine in codegen. Besides, VM messed up the alignment of
            // Vector128 on ARM so this happens "naturally", without any reinterpretation...

            if ((fields[firstStackField].offset < argInfo->GetRegCount() * REGSIZE_BYTES)
#ifdef TARGET_ARM
                && (fields[firstStackField].type != TYP_LONG)
#endif
                    )
            {
                return false;
            }
        }

        return true;
    }
};

GenTree* Compiler::abiMorphMultiRegLclArgPromoted(CallArgInfo* argInfo, const AbiRegFieldMap& map)
{
    GenTreeFieldList* list     = new (this, GT_FIELD_LIST) GenTreeFieldList();
    GenTree*          regValue = nullptr;

    for (unsigned i = 0; i < map.count; i++)
    {
        unsigned regIndex  = map.fields[i].reg;
        unsigned regOffset = regIndex * REGSIZE_BYTES;

        if (map.fields[i].lclNum == BAD_VAR_NUM)
        {
            var_types regType = argInfo->GetRegType(regIndex);
            list->AddField(this, gtNewZeroConNode(regType), regOffset, regType);
            continue;
        }

        GenTree*  fieldValue  = gtNewLclvNode(map.fields[i].lclNum, map.fields[i].type);
        var_types fieldType   = map.fields[i].type;
        unsigned  fieldOffset = map.fields[i].offset;

        if (regIndex >= argInfo->GetRegCount())
        {
            list->AddField(this, fieldValue, fieldOffset, fieldType);
            continue;
        }

#ifdef WINDOWS_X86_ABI
        // Handle win-x86 multireg return of a struct with a single vector field
        // (in principle the field should be SIMD8 but this is the only multireg
        // case on x86 so we don't care what exact SIMD type it is, we just need
        // to get INT values out of it).

        if (varTypeIsSIMD(fieldType))
        {
            assert(argInfo->GetRegType(regIndex) == TYP_INT);

            unsigned extractOffset = regOffset - fieldOffset;
            assert(extractOffset % 4 == 0);

            regValue = NewExtractVectorElement(fieldType, TYP_INT, fieldValue, extractOffset / 4);
        }
        else
#elif defined(TARGET_64BIT)
        // On 64 bit targets we may need to pack 2 FLOAT fields into a LONG/DOUBLE
        // and we also have to deal with vector fields that span multiple registers
        // (x86 doesn't have multireg args and ARM doesn't have vector support).

        if (fieldOffset < regOffset)
        {
            // A vector overlaps the previous register, extract a FLOAT or a DOUBLE,
            // depending on the remaining size. We might have to extract 2 FLOATS if
            // the current vector offset isn't properly aligned to extract a DOUBLE.

            assert(varTypeIsSIMD(fieldType));

            unsigned extractOffset = regOffset - fieldOffset;
            unsigned extractSize   = min(REGSIZE_BYTES, varTypeSize(fieldType) - extractOffset);

            if (extractSize == 4)
            {
                assert((extractOffset % 4) == 0);

                fieldValue  = NewExtractVectorElement(fieldType, TYP_FLOAT, fieldValue, extractOffset / 4);
                fieldOffset = regOffset;
            }
            else if ((extractOffset % 8 == 0))
            {
                fieldValue  = NewExtractVectorElement(fieldType, TYP_DOUBLE, fieldValue, extractOffset / 8);
                fieldOffset = regOffset;
            }
            else
            {
                assert(extractSize == 8);
                assert(extractOffset % 4 == 0);

                regValue   = NewExtractVectorElement(fieldType, TYP_FLOAT, fieldValue, extractOffset / 4);
                fieldValue = gtNewLclvNode(fieldValue->AsLclVar()->GetLclNum(), fieldType);
                fieldValue = NewExtractVectorElement(fieldType, TYP_FLOAT, fieldValue, extractOffset / 4 + 1);

                fieldOffset = regOffset + 4;
                fieldType   = TYP_FLOAT;
            }
        }
        else if (varTypeIsSIMD(fieldType) && (fieldOffset == regOffset + 4))
        {
            // The vector overlaps the second half of an eightbyte, extract a FLOAT
            // and continue as if this was a FLOAT field.

            fieldValue = NewExtractVectorElement(fieldType, TYP_FLOAT, fieldValue, 0);
            fieldType  = TYP_FLOAT;
        }

        if ((fieldType == TYP_FLOAT) && (fieldOffset == regOffset + 4))
        {
            assert(regValue->TypeIs(TYP_FLOAT));

            regValue = gtNewSimdHWIntrinsicNode(
#ifdef UNIX_AMD64_ABI
                TYP_SIMD16, NI_Vector128_Create, TYP_FLOAT, 16,
#elif defined(TARGET_ARM64)
                TYP_SIMD8, NI_Vector64_Create, TYP_FLOAT, 8,
#else
#error Unknown target
#endif
                regValue, fieldValue);
        }
        else
#endif // TARGET_64BIT
        {
            assert(fieldOffset == regOffset);
            regValue = fieldValue;
        }

        if ((i == map.count - 1) || (map.fields[i + 1].reg != regIndex))
        {
            var_types regType = argInfo->GetRegType(regIndex);

#ifdef TARGET_64BIT
            if (regValue->TypeIs(TYP_FLOAT, TYP_DOUBLE, TYP_SIMD8) && !varTypeUsesFloatReg(regType))
            {
                regValue = gtNewBitCastNode(regValue->TypeIs(TYP_FLOAT) ? TYP_INT : TYP_LONG, regValue);
            }
            else if (varTypeIsSIMD(regValue->GetType()) && !varTypeUsesFloatReg(regType))
            {
                regValue = NewExtractVectorElement(regValue->GetType(), TYP_LONG, regValue, 0);
            }
            else if (varTypeIsIntegral(regValue->GetType()) && varTypeUsesFloatReg(regType))
            {
                regValue = gtNewBitCastNode(varTypeSize(regValue->GetType()) <= 4 ? TYP_FLOAT : TYP_DOUBLE, regValue);
            }
#else
            assert(!varTypeUsesFloatReg(regType));

            if (regValue->TypeIs(TYP_FLOAT))
            {
                regValue = gtNewBitCastNode(TYP_INT, regValue);
            }
            else if (regValue->TypeIs(TYP_LONG, TYP_DOUBLE))
            {
                // Skip the next register, decomposition will split the LONG
                // field into 2 INT fields, DOUBLE will be handled in codegen.
                i++;
            }
#endif

            list->AddField(this, regValue, regIndex * REGSIZE_BYTES, regType);
        }
    }

    return list;
}

GenTree* Compiler::abiMorphMultiRegStructArg(CallArgInfo* argInfo, GenTree* arg)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("Morphing multireg struct ");
        argInfo->Dump();
    }
#endif

    assert(argInfo->GetRegCount() != 0);

    if (arg->IsIntegralConst(0))
    {
        if (argInfo->GetRegCount() + argInfo->GetSlotCount() > 4)
        {
            return arg;
        }

        GenTreeFieldList* fieldList = abiMakeFieldList(arg);

        for (unsigned i = 0, regOffset = 0; i < argInfo->GetRegCount() + argInfo->GetSlotCount(); i++)
        {
#if FEATURE_ARG_SPLIT
            var_types regType = i < argInfo->GetRegCount() ? argInfo->GetRegType(i) : TYP_I_IMPL;
#else
            var_types regType = argInfo->GetRegType(i);
#endif

#ifdef FEATURE_HW_INTRINSICS
            if (varTypeIsSIMD(regType))
            {
                fieldList->AddField(this, gtNewZeroSimdHWIntrinsicNode(regType, TYP_FLOAT), regOffset, regType);
            }
            else
#endif
            {
                fieldList->AddField(this, gtNewZeroConNode(regType), regOffset, regType);
            }

            regOffset += varTypeSize(regType);
        }

        return fieldList;
    }

    assert(varTypeIsStruct(arg->GetType()));

    // TODO-MIKE-CQ: It may make more sense to alway use abiMorphMultiRegSimdArg for
    // SIMD args that are memory loads. abiMorphMultiRegObjArg will just generate
    // multiple loads and those loads may have associated optimization issues in VN
    // due to the lack of field sequences. Even if they do get CSEed the result may
    // still be poor - SIMD typed loads from the same location would get a CSE temp
    // and then those multi reg loads from the same location will get their own CSE
    // temps. abiMorphMultiRegSimdArg would only generate one SIMD load and then
    // extract the registers via register to register operations.

    if (arg->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        if (arg->OperIs(GT_LCL_VAR) && lvaGetDesc(arg->AsLclVar())->IsPromoted())
        {
            if (argInfo->IsHfaArg())
            {
                return abiMorphMultiRegHfaLclArgPromoted(argInfo, arg->AsLclVar());
            }

            AbiRegFieldMap map(this, lvaGetDesc(arg->AsLclVar()), argInfo);

            if (map.IsSupported(argInfo))
            {
                return abiMorphMultiRegLclArgPromoted(argInfo, map);
            }
        }

        return abiMorphMultiRegLclArg(argInfo, arg->AsLclVarCommon());
    }

    if (arg->OperIs(GT_OBJ))
    {
        INDEBUG(GenTreeLclVarCommon* lclNode = arg->AsObj()->GetAddr()->IsLocalAddrExpr();)
        assert((lclNode == nullptr) || lvaGetDesc(lclNode)->lvDoNotEnregister);

        return abiMorphMultiRegObjArg(argInfo, arg->AsObj());
    }

#ifdef FEATURE_SIMD
    // If it's neither a local nor OBJ then it must be an arbitrary SIMD tree.
    return abiMorphMultiRegSimdArg(argInfo, arg);
#else
    unreached();
#endif
}

#ifdef FEATURE_SIMD

GenTree* Compiler::abiMorphMultiRegSimdArg(CallArgInfo* argInfo, GenTree* arg)
{
    assert(varTypeIsSIMD(arg->GetType()));
    assert(!arg->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_OBJ));

    unsigned regCount = argInfo->GetRegCount();
#if FEATURE_ARG_SPLIT
    regCount += argInfo->GetSlotCount();
#endif

#if defined(TARGET_ARM64)
    assert((regCount >= 2) && (regCount <= 4));
    assert((argInfo->GetRegType() == TYP_FLOAT) || (argInfo->GetRegType() == TYP_I_IMPL));
#elif defined(UNIX_AMD64_ABI)
    assert(regCount == 2);
    assert(argInfo->GetRegType(0) == TYP_DOUBLE);
    assert((argInfo->GetRegType(1) == TYP_FLOAT) || (argInfo->GetRegType(1) == TYP_DOUBLE));
#elif defined(WINDOWS_X86_ABI)
    assert(regCount == 2);
    assert((argInfo->GetRegType(0) == TYP_INT) && (argInfo->GetRegType(1) == TYP_INT));
#else
#error Unknown target.
#endif

    bool argIsZero      = false;
    bool argIsCreate    = false;
    bool argIsBroadcast = false;

    if (GenTreeHWIntrinsic* hwi = arg->IsHWIntrinsic())
    {
        switch (hwi->GetIntrinsic())
        {
            case NI_Vector128_get_Zero:
#ifdef TARGET_ARM64
            case NI_Vector64_get_Zero:
#endif
                argIsZero = true;
                break;

            case NI_Vector128_Create:
#ifdef TARGET_ARM64
            case NI_Vector64_Create:
#endif
                if ((hwi->GetSimdBaseType() == TYP_FLOAT)
#ifdef TARGET_ARM64
                    && (argInfo->GetRegType() == TYP_FLOAT)
#endif
                        )
                {
                    if (hwi->IsUnary())
                    {
                        argIsBroadcast = true;
                    }
                    else
                    {
                        argIsCreate = true;
                    }
                }
                break;

            default:
                break;
        }
    }

    unsigned tempLclNum = BAD_VAR_NUM;
    GenTree* tempAssign = nullptr;

    if (argIsBroadcast)
    {
        arg = arg->AsHWIntrinsic()->GetOp(0);

        assert(arg->TypeIs(TYP_FLOAT));

        if (!arg->IsDblCon() && !arg->OperIs(GT_LCL_VAR))
        {
            tempLclNum = lvaNewTemp(TYP_FLOAT, true DEBUGARG("multi-reg SIMD arg temp"));
            tempAssign = gtNewAssignNode(gtNewLclvNode(tempLclNum, TYP_FLOAT), arg);

            arg = gtNewLclvNode(tempLclNum, TYP_FLOAT);
        }

#ifdef UNIX_AMD64_ABI
        if (arg->IsDblCon())
        {
            // CSE is dumb - it CSEs FP constants and that prevents us from recognizing
            // constant vector creation in lowering. Well, recognizing constant vector
            // creation is lowering is just as dumb...
            arg->SetDoNotCSE();
        }

        arg = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_FLOAT, 16, arg, gtCloneExpr(arg));
        // TODO-MIKE-Cleanup: We should be able to create a SIMD16 temp but we may
        // not have a layout for it so for now "convert" the SIMD16 to a DOUBLE.
        arg = gtNewSimdGetElementNode(TYP_SIMD16, TYP_DOUBLE, arg, gtNewIconNode(0));

        unsigned dblTempLclNum = lvaNewTemp(TYP_DOUBLE, true DEBUGARG("multi-reg SIMD arg temp"));
        GenTree* dblTempAssign = gtNewAssignNode(gtNewLclvNode(dblTempLclNum, TYP_DOUBLE), arg);

        if (tempAssign != nullptr)
        {
            tempAssign = gtNewCommaNode(tempAssign, dblTempAssign);
        }
        else
        {
            tempAssign = dblTempAssign;
        }

        arg = gtNewLclvNode(dblTempLclNum, TYP_DOUBLE);
#endif // UNIX_AMD64_ABI
    }
    else if (!argIsZero && !argIsCreate)
    {
        ClassLayout* argLayout = typGetLayoutByNum(argInfo->GetSigTypeNum());

        tempLclNum = lvaNewTemp(argLayout, true DEBUGARG("multi-reg SIMD arg temp"));
        tempAssign = gtNewAssignNode(gtNewLclvNode(tempLclNum, arg->GetType()), arg);
    }

    GenTreeFieldList* fieldList = new (this, GT_FIELD_LIST) GenTreeFieldList();

    for (unsigned i = 0, regOffset = 0, createOpIndex = 0; i < regCount; i++)
    {
#if FEATURE_ARG_SPLIT
        var_types regType = i < argInfo->GetRegCount() ? argInfo->GetRegType(i) : TYP_I_IMPL;
#else
        var_types regType = argInfo->GetRegType(i);
#endif
        unsigned regSize = varTypeSize(regType);
        GenTree* regValue;

        if (argIsBroadcast)
        {
            regValue = i == 0 ? arg : gtCloneExpr(arg);
        }
        else if (argIsCreate && (createOpIndex < arg->AsHWIntrinsic()->GetNumOps()))
        {
            regValue = arg->AsHWIntrinsic()->GetOp(createOpIndex++);
            assert(regValue->TypeIs(TYP_FLOAT));
#ifdef TARGET_ARM64
            assert(regType == TYP_FLOAT);
#endif

#ifdef UNIX_AMD64_ABI
            if ((regType == TYP_DOUBLE) && (createOpIndex < arg->AsHWIntrinsic()->GetNumOps()))
            {
                GenTree* regValue2 = arg->AsHWIntrinsic()->GetOp(createOpIndex++);
                assert(regValue2->TypeIs(TYP_FLOAT));
                regValue = gtNewSimdHWIntrinsicNode(TYP_SIMD8, NI_Vector128_Create, TYP_FLOAT, 16, regValue, regValue2);
            }
#endif
        }
        else if (argIsCreate || argIsZero)
        {
            regValue = gtNewZeroConNode(regType);
        }
        else
        {
            regValue = gtNewLclvNode(tempLclNum, arg->GetType());
            regValue = gtNewSimdGetElementNode(arg->GetType(), regType, regValue, gtNewIconNode(regOffset / regSize));
        }

        if ((i == 0) && (tempAssign != nullptr))
        {
            regValue = gtNewCommaNode(tempAssign, regValue);
        }

        fieldList->AddField(this, regValue, regOffset, regType);
        regOffset += regSize;
    }

    return fieldList;
}

#endif // FEATURE_SIMD

GenTree* Compiler::abiMorphMultiRegLclArg(CallArgInfo* argInfo, GenTreeLclVarCommon* arg)
{
    LclVarDsc*   lcl       = lvaGetDesc(arg);
    ClassLayout* argLayout = arg->OperIs(GT_LCL_VAR) ? lcl->GetLayout() : arg->AsLclFld()->GetLayout(this);

#ifdef TARGET_ARM
    // If an argument is passed in registers we'd like to build a FIELD_LIST with
    // one field for each register. But split args are problematic - they are also
    // passed on stack and the number of slots is unbounded. Building a FIELD_LIST
    // with one field per slot isn't an option because there may be too many and
    // having one field for all slots doesn't work either because we don't have a
    // layout to describe such a partial "view" of a struct.
    // So we give up if there are too many slots.
    //
    // For promoted struct locals this means that we'll end up with dependent promotion.
    // This isn't very common (because it means the struct has long or double fields,
    // otherwise the number of promoted fields being limited to 4 it's not easy to exceed
    // the number of reg and slots that is also 4).

    if (argInfo->IsSplit() && (argInfo->GetSlotCount() + argInfo->GetRegCount() > MAX_ARG_REG_COUNT))
    {
        if (arg->OperIs(GT_LCL_VAR))
        {
            lvaSetVarDoNotEnregister(arg->GetLclNum() DEBUGARG(DNER_IsStructArg));
        }

        return arg;
    }

    // TODO-MIKE-CQ: Temps are introduced for independent promoted locals that can't be passed directly
    // only on ARM32 for "historical" reasons, it doesn't really make sense for this to be ARM32 only.
    //
    // However, attempting to enable this on other targets results in code size regressions and disabling
    // this on ARM32 results in code size improvements. More investigation is required to determine which
    // is better so for now let's keep this as it was.
    //
    // Speaking of how it was - this code was originally in fgMorphArgs so the temp was introduced before
    // ArgsComplete/SortArgs/EvalArgsToTemps. Neither place is ideal:
    //    - Introducing one temp before ArgsComplete is problematic because ArgsComplete can blindly
    //      introduce even more temps due to the presence of GTF_ASG.
    //    - Introducing a temp for a promoted local after ArgsComplete "misses" the nested call args case
    //      so we risk spilling the promoted fields before the call, reload them after the call and then
    //      store them again to memory (because this temp is DNER).
    // What may be best is to introduce the temp in ArgsComplete itself so we can do it before any nested
    // call and avoid unnecessary spilling. But it may not be worth the trouble:
    //    - Promoted locals that cannot be loaded directly in registers are relatively rare and they'd
    //      be even more rare with some improvements to abiMorphPromotedStructArgToFieldList.
    //    - Doing this here instead of fgMorphArgs shows practically no diffs (actually a 8 bytes improvement).
    //    - Doing this here minimizes the use of the messy "late" arg mechanism.

    GenTree* tempAssign = nullptr;

    if (arg->OperIs(GT_LCL_VAR) && lvaGetDesc(arg)->IsIndependentPromoted())
    {
        unsigned tempLclNum = abiAllocateStructArgTemp(argLayout);
        lcl                 = lvaGetDesc(tempLclNum);

        tempAssign = gtNewAssignNode(gtNewLclvNode(tempLclNum, lcl->GetType()), arg);
        tempAssign = fgMorphStructAssignment(tempAssign->AsOp());

        arg->SetLclNum(tempLclNum);
    }
#endif // TARGET_ARM

    unsigned regCount = argInfo->GetRegCount();
#if FEATURE_ARG_SPLIT
    regCount += argInfo->GetSlotCount();
#endif

    GenTreeFieldList* fieldList = new (this, GT_FIELD_LIST) GenTreeFieldList();

    unsigned lclNum    = arg->GetLclNum();
    unsigned lclOffset = arg->OperIs(GT_LCL_FLD) ? arg->AsLclFld()->GetLclOffs() : 0;
    unsigned lclSize   = lcl->GetSize();
#ifdef FEATURE_SIMD
    bool lclIsSIMD = varTypeIsSIMD(lcl->GetType()) && !lcl->IsPromoted() && !lcl->lvDoNotEnregister;
#endif

    for (unsigned i = 0, regOffset = 0; i < regCount; i++)
    {
#if FEATURE_ARG_SPLIT
        var_types regType = i < argInfo->GetRegCount() ? argInfo->GetRegType(i) : TYP_I_IMPL;
#else
        var_types regType = argInfo->GetRegType(i);
#endif

        if (regType == TYP_I_IMPL)
        {
            // On UNIX_AMD64_ABI the register types we have in CallArgInfo do have GC info
            // but on other targets we only get TYP_I_IMPL. Also, other targets may have
            // split args and we don't have any type info for stack slots.

            assert(regOffset % REGSIZE_BYTES == 0);

#ifdef UNIX_AMD64_ABI
            assert(regType == argLayout->GetGCPtrType(regOffset / REGSIZE_BYTES));
#else
            regType       = argLayout->GetGCPtrType(regOffset / REGSIZE_BYTES);
#endif
        }

        unsigned regSize = varTypeSize(regType);
        GenTree* regValue;

        if (lclOffset >= lclSize)
        {
            // Make sure we add a field for every arg register, even if we somehow end up with
            // a smaller local variable as source. If a field is missing, the backend may get
            // confused and fail to allocate the register to this arg and instead allocated it
            // to the next arg. Just passing a zero is safer and easier to debug.
            regValue = gtNewZeroConNode(regType);
        }
#ifdef FEATURE_SIMD
        else if (lclIsSIMD && (lclOffset % regSize == 0))
        {
            // Note that the VM ARM64 ABI doesn't recognize HVAs so a struct with a single
            // Vector128 field will be passed in 2 GPRs instead of a single SIMD reg.
            assert(varTypeIsFloating(regType) || (regType == TYP_I_IMPL));

            GenTree* elementIndex = gtNewIconNode(lclOffset / regSize);
            GenTree* simdValue    = gtNewLclvNode(lclNum, lcl->GetType());

            regValue = gtNewSimdGetElementNode(lcl->GetType(), regType, simdValue, elementIndex);
        }
        else
#endif
        {
            lvaSetVarDoNotEnregister(lclNum DEBUG_ARG(DNER_LocalField));

            regValue = gtNewLclFldNode(lclNum, regType, lclOffset);
        }

#ifdef TARGET_ARM
        if (tempAssign != nullptr)
        {
            regValue   = gtNewCommaNode(tempAssign, regValue);
            tempAssign = nullptr;
        }
#endif

        fieldList->AddField(this, regValue, regOffset, regType);
        regOffset += regSize;
        lclOffset += regSize;
    }

    return fieldList;
}

GenTree* Compiler::abiMorphMultiRegObjArg(CallArgInfo* argInfo, GenTreeObj* arg)
{
#ifdef TARGET_ARM
    // If an argument is passed in registers we'd like to build a FIELD_LIST with
    // one field for each register. But split args are problematic - they are also
    // passed on stack and the number of slots is unbounded. Building a FIELD_LIST
    // with one field per slot isn't an option because there may be too many and
    // having one field for all slots doesn't work either because we don't have a
    // layout to describe such a partial "view" of a struct.
    // So we give up if there are too many slots.

    if (argInfo->IsSplit() && (argInfo->GetSlotCount() + argInfo->GetRegCount() > MAX_ARG_REG_COUNT))
    {
        return arg;
    }
#endif

    ClassLayout* argLayout = arg->GetLayout();
    unsigned     argSize   = argLayout->GetSize();

    GenTree* addr           = arg->GetAddr();
    ssize_t  addrOffset     = 0;
    GenTree* addrTempAssign = abiMakeIndirAddrMultiUse(&addr, &addrOffset, argSize);

    GenTreeFieldList* fieldList = new (this, GT_FIELD_LIST) GenTreeFieldList();

    unsigned regCount = argInfo->GetRegCount();
#if FEATURE_ARG_SPLIT
    regCount += argInfo->GetSlotCount();
#endif

    for (unsigned i = 0, regOffset = 0; i < regCount; i++)
    {
#if FEATURE_ARG_SPLIT
        var_types regType = i < argInfo->GetRegCount() ? argInfo->GetRegType(i) : TYP_I_IMPL;
#else
        var_types regType = argInfo->GetRegType(i);
#endif

        if (regType == TYP_I_IMPL)
        {
            // On UNIX_AMD64_ABI the register types we have in CallArgInfo do have GC info
            // but on other targets we only get TYP_I_IMPL. Also, other targets may have
            // split args and we don't have any type info for the stack slots.

            assert(regOffset % REGSIZE_BYTES == 0);

#ifdef UNIX_AMD64_ABI
            assert(regType == argLayout->GetGCPtrType(regOffset / REGSIZE_BYTES));
#else
            regType       = argLayout->GetGCPtrType(regOffset / REGSIZE_BYTES);
#endif
        }

        GenTree* regAddr = (i == 0) ? addr : gtCloneExpr(addr);
        GenTree* regIndir;
        unsigned regIndirSize;

        if (!varTypeIsIntegral(regType) || (argSize - regOffset >= REGSIZE_BYTES))
        {
            regIndirSize = varTypeSize(regType);

            if (addrOffset + regOffset != 0)
            {
                regAddr = gtNewOperNode(GT_ADD, varTypePointerAdd(regAddr->GetType()), regAddr,
                                        gtNewIconNode(addrOffset + regOffset, TYP_I_IMPL));
                regAddr->gtFlags |= GTF_DONT_CSE;
            }

            regIndir = gtNewIndir(regType, regAddr);
            regIndir->gtFlags |= GTF_GLOB_REF;
        }
        else
        {
            regIndirSize = argSize - regOffset;
            regIndir     = abiNewMultiLoadIndir(regAddr, addrOffset + regOffset, regIndirSize);
        }

        if ((i == 0) && (addrTempAssign != nullptr))
        {
            regIndir = gtNewCommaNode(addrTempAssign, regIndir, regType);
        }

        fieldList->AddField(this, regIndir, regOffset, regType);
        regOffset += regIndirSize;

        assert(regOffset <= argSize);
    }

    return fieldList;
}

GenTree* Compiler::abiMakeIndirAddrMultiUse(GenTree** addrInOut, ssize_t* addrOffsetOut, unsigned indirSize)
{
    GenTree* addr       = *addrInOut;
    ssize_t  addrOffset = 0;

    // Extract constant offsets that appear when passing struct typed class fields
    // so they can be folded with the register offset.
    if (addr->OperIs(GT_ADD) && addr->AsOp()->GetOp(1)->IsIntCon())
    {
        ssize_t offset = addr->AsOp()->GetOp(1)->AsIntCon()->GetValue();

#if defined(TARGET_AMD64)
        if ((offset >= INT32_MIN) && (offset <= INT32_MAX - indirSize))
#elif defined(WINDOWS_X86_ABI)
        if (static_cast<unsigned>(offset) <= UINT32_MAX - indirSize)
#elif defined(TARGET_ARMARCH)
        // For simplicity, limit the offset to values that are valid address mode
        // offsets, no matter what the indirection type is.
        if ((offset >= -255) && (offset <= 255 - static_cast<ssize_t>(indirSize)))
#else
#error Unknown target.
#endif
        {
            addr       = addr->AsOp()->GetOp(0);
            addrOffset = offset;
        }
    }

    // We need to use the address tree multiple times. If it has side effects or
    // it is too expensive to evaluate then we need to "spill" it to a temp.
    bool addrTempRequired = (addr->gtFlags & GTF_PERSISTENT_SIDE_EFFECTS) != 0;

    if (!addrTempRequired)
    {
        gtPrepareCost(addr);
        addrTempRequired = addr->GetCostEx() > 4 * IND_COST_EX;
    }

    GenTree* addrAsg = nullptr;

    if (addrTempRequired)
    {
        unsigned addrLclNum = lvaNewTemp(addr->GetType(), true DEBUGARG("call arg addr temp"));
        GenTree* addrDef    = gtNewLclvNode(addrLclNum, addr->GetType());

        addrAsg = gtNewAssignNode(addrDef, addr);
        addr    = gtNewLclvNode(addrLclNum, addr->GetType());
    }

    *addrInOut     = addr;
    *addrOffsetOut = addrOffset;
    return addrAsg;
}

GenTree* Compiler::abiNewMultiLoadIndir(GenTree* addr, ssize_t addrOffset, unsigned indirSize)
{
    auto Indir = [&](var_types type, GenTree* addr, ssize_t offset) {
        if (offset != 0)
        {
            addr = gtNewOperNode(GT_ADD, varTypePointerAdd(addr->GetType()), addr, gtNewIconNode(offset, TYP_I_IMPL));
            addr->gtFlags |= GTF_DONT_CSE;
        }
        GenTree* indir = gtNewIndir(type, addr);
        indir->gtFlags |= GTF_GLOB_REF;
        return indir;
    };
    auto LeftShift = [&](GenTree* op1, unsigned amount) {
        return gtNewOperNode(GT_LSH, varActualType(op1->GetType()), op1, gtNewIconNode(amount));
    };
    auto Or = [&](GenTree* op1, GenTree* op2) { return gtNewOperNode(GT_OR, varActualType(op1->GetType()), op1, op2); };
    auto Clone  = [&](GenTree* expr) { return gtCloneExpr(expr); };
    auto Extend = [&](GenTree* value) { return gtNewCastNode(TYP_LONG, value, true, TYP_LONG); };

    if (indirSize == 1)
    {
        return Indir(TYP_UBYTE, addr, addrOffset);
    }

#ifdef TARGET_64BIT
    if (indirSize < 4)
#else
    assert(indirSize < 4);
#endif
    {
        GenTree* indir = Indir(TYP_USHORT, addr, addrOffset);

        if (indirSize == 3)
        {
            GenTree* indir2 = Indir(TYP_UBYTE, Clone(addr), addrOffset + 2);
            indir           = Or(indir, LeftShift(indir2, 16));
        }

        return indir;
    }

#ifdef TARGET_64BIT
    assert(indirSize < 8);

    GenTree* indir = Indir(TYP_INT, addr, addrOffset);

    if (indirSize > 4)
    {
        GenTree* indir4 = Indir(indirSize == 5 ? TYP_UBYTE : TYP_USHORT, Clone(addr), addrOffset + 4);

        if (indirSize == 7)
        {
            GenTree* indir6 = Indir(TYP_UBYTE, Clone(addr), addrOffset + 6);
            indir4          = Or(indir4, LeftShift(indir6, 16));
        }

        indir = Or(Extend(indir), LeftShift(Extend(indir4), 32));
    }

    return indir;
#endif
}

#endif // FEATURE_MULTIREG_ARGS || FEATURE_MULTIREG_RET

#ifndef TARGET_X86

unsigned Compiler::abiAllocateStructArgTemp(ClassLayout* argLayout)
{
    assert(argLayout->IsValueClass());

    unsigned tempLclNum = BAD_VAR_NUM;

    if (!opts.MinOpts())
    {
        if (m_abiStructArgTemps == nullptr)
        {
            m_abiStructArgTemps      = hashBv::Create(this);
            m_abiStructArgTempsInUse = hashBv::Create(this);
        }
        else
        {
            indexType index = m_abiStructArgTemps->FindFirstBit([this, argLayout](indexType i) {
                return (lvaGetDesc(static_cast<unsigned>(i))->GetLayout() == argLayout) &&
                       !m_abiStructArgTempsInUse->testBit(i);
            });

            if (index != -1)
            {
                tempLclNum = static_cast<unsigned>(index);
                JITDUMP("Reusing struct arg temp V%02u\n", tempLclNum);
            }
        }
    }

    if (tempLclNum == BAD_VAR_NUM)
    {
        tempLclNum = lvaGrabTemp(true DEBUGARG("struct arg temp"));
        lvaSetStruct(tempLclNum, argLayout, false);

        if (m_abiStructArgTemps != nullptr)
        {
            m_abiStructArgTemps->setBit(tempLclNum);
        }
    }

    if (m_abiStructArgTempsInUse != nullptr)
    {
        m_abiStructArgTempsInUse->setBit(tempLclNum);
    }

    return tempLclNum;
}

void Compiler::abiFreeAllStructArgTemps()
{
    if (m_abiStructArgTempsInUse != nullptr)
    {
        m_abiStructArgTempsInUse->ZeroAll();
    }
}

#if TARGET_64BIT

void Compiler::abiMorphImplicitByRefStructArg(GenTreeCall* call, CallArgInfo* argInfo)
{
    GenTree* arg = argInfo->GetNode();

    if (arg->TypeIs(TYP_BYREF, TYP_I_IMPL))
    {
        return;
    }

    if (arg->OperIs(GT_MKREFANY))
    {
        unsigned tempLclNum = abiAllocateStructArgTemp(typGetObjLayout(impGetRefAnyClass()));
        argInfo->SetNode(abiMorphMkRefAnyToStore(tempLclNum, arg->AsOp()));
        argInfo->SetTempLclNum(tempLclNum);

        return;
    }

    // If we're optimizing, see if we can avoid making a copy.
    // We don't need a copy if this is the last use of an implicit by-ref local.
    if (opts.OptimizationEnabled())
    {
        GenTreeLclVar* const lclNode = arg->IsImplicitByrefIndir(this);

        if (lclNode != nullptr)
        {
            const unsigned   lclNum           = lclNode->GetLclNum();
            LclVarDsc* const lcl              = lvaGetDesc(lclNum);
            const unsigned   totalAppearances = lcl->lvRefCnt(RCS_EARLY);

            // We don't have liveness so we rely on other indications of last use.
            //
            // We handle these cases:
            //
            // * (must not copy) If the call is a tail call, the use is a last use.
            //   We must skip the copy if we have a fast tail call.
            //
            // * (may not copy) if the call is noreturn, the use is a last use.
            //   We also check for just one reference here as we are not doing
            //   alias analysis of the call's parameters, or checking if the call
            //   site is not within some try region.
            //
            // * (may not copy) if there is exactly one use of the local in the method,
            //   and the call is not in loop, this is a last use.
            //
            const bool isTailCallLastUse = call->IsTailCall();
            const bool isCallLastUse     = (totalAppearances == 1) && !fgMightHaveLoop();
            const bool isNoReturnLastUse = (totalAppearances == 1) && call->IsNoReturn();
            if (isTailCallLastUse || isCallLastUse || isNoReturnLastUse)
            {
                argInfo->SetNode(lclNode);

                JITDUMP("did not need to make outgoing copy for last use of implicit byref V%02u\n", lclNum);
                return;
            }
        }
    }

    // Note that this is the parameter's layout rather that the argument's layout.
    // They should be identical, unless the IL is invalid or we hit the pesky
    // A<Canon> vs. A<C> issue. In general it doesn't matter if the 2 layouts
    // do not match. VN might get confused due to mismatched field sequences but
    // then this temp is never read from.
    ClassLayout* argLayout = typGetLayoutByNum(argInfo->GetSigTypeNum());

    unsigned tempLclNum = abiAllocateStructArgTemp(argLayout);

    // These temps are passed by reference so they're always address taken.
    // TODO-MIKE-Cleanup: Aren't they actually address exposed? If we only
    // make them DNER they may still be tracked unnecessarily.
    lvaSetVarDoNotEnregister(tempLclNum DEBUGARG(DNER_IsStructArg));

    // Replace the argument with an assignment to the temp, EvalArgsToTemps will later add
    // a use of the temp to the late arg list.

    LclVarDsc* tempLcl = lvaGetDesc(tempLclNum);
    GenTree*   dest;

    // Due to single field struct promotion it is possible that the argument has SIMD
    // type while the temp has STRUCT type. Store the arg to the temp using LCL_FLD
    // because fgMorphCopyStruct has problems with this kind of type mismatches.

    // TODO-MIKE-Cleanup: This should probably be extended to other type mismatches
    // that arise from single field struct promotion. fgMorphCopyStruct shouldn't have
    // to deal with fall out from poorly designed struct promotion. Small int types
    // may need some extra care. Since we're dealing with structs, widening isn't
    // required but we need to be sure that the correct type is used to store the arg
    // value - it may be that the struct has 2 bytes (a SHORT field) and the arg may
    // have type UBYTE (e.g. an IND) - we do need to store 2 bytes, not 1, so using
    // the arg type isn't correct.

    // TODO-MIKE-CQ: SIMD12 stores should be widened to SIMD16 on 64 bit targets.

    if (tempLcl->TypeIs(TYP_STRUCT) && varTypeIsSIMD(arg->GetType()))
    {
        dest = gtNewLclFldNode(tempLclNum, arg->GetType(), 0);
    }
    else
    {
        dest = gtNewLclvNode(tempLclNum, tempLcl->GetType());
    }

    GenTree* asg = gtNewAssignNode(dest, arg);

    if (varTypeIsStruct(dest->GetType()))
    {
        asg = fgMorphStructAssignment(asg->AsOp());
    }

    argInfo->SetNode(asg);
    argInfo->SetTempLclNum(tempLclNum);
}

#endif // TARGET_64BIT

#endif // !TARGET_X86

/*****************************************************************************
 *
 *  A little helper used to rearrange nested commutative operations. The
 *  effect is that nested associative, commutative operations are transformed
 *  into a 'left-deep' tree, i.e. into something like this:
 *
 *      (((a op b) op c) op d) op...
 */

#if REARRANGE_ADDS

void Compiler::fgMoveOpsLeft(GenTree* tree)
{
    GenTree*   op1;
    GenTree*   op2;
    genTreeOps oper;

    do
    {
        op1  = tree->AsOp()->gtOp1;
        op2  = tree->AsOp()->gtOp2;
        oper = tree->OperGet();

        noway_assert(GenTree::OperIsCommutative(oper));
        noway_assert(oper == GT_ADD || oper == GT_XOR || oper == GT_OR || oper == GT_AND || oper == GT_MUL);
        noway_assert(!varTypeIsFloating(tree->TypeGet()) || !opts.genFPorder);
        noway_assert(oper == op2->gtOper);

        // Commutativity doesn't hold if overflow checks are needed

        if (tree->gtOverflowEx() || op2->gtOverflowEx())
        {
            return;
        }

        if (gtIsActiveCSE_Candidate(op2))
        {
            // If we have marked op2 as a CSE candidate,
            // we can't perform a commutative reordering
            // because any value numbers that we computed for op2
            // will be incorrect after performing a commutative reordering
            //
            return;
        }

#ifndef TARGET_64BIT
        if ((oper == GT_MUL) && ((op2->gtFlags & GTF_MUL_64RSLT) != 0))
        {
            return;
        }
#endif

        // Check for GTF_ADDRMODE_NO_CSE flag on add/mul Binary Operators
        if (((oper == GT_ADD) || (oper == GT_MUL)) && ((tree->gtFlags & GTF_ADDRMODE_NO_CSE) != 0))
        {
            return;
        }

        if ((tree->gtFlags | op2->gtFlags) & GTF_BOOLEAN)
        {
            // We could deal with this, but we were always broken and just hit the assert
            // below regarding flags, which means it's not frequent, so will just bail out.
            // See #195514
            return;
        }

        noway_assert(!tree->gtOverflowEx() && !op2->gtOverflowEx());

        GenTree* ad1 = op2->AsOp()->gtOp1;
        GenTree* ad2 = op2->AsOp()->gtOp2;

        // Compiler::optOptimizeBools() can create GT_OR of two GC pointers yeilding a GT_INT
        // We can not reorder such GT_OR trees
        //
        if (varTypeIsGC(ad1->TypeGet()) != varTypeIsGC(op2->TypeGet()))
        {
            break;
        }

        // Don't split up a byref calculation and create a new byref. E.g.,
        // [byref]+ (ref, [int]+ (int, int)) => [byref]+ ([byref]+ (ref, int), int).
        // Doing this transformation could create a situation where the first
        // addition (that is, [byref]+ (ref, int) ) creates a byref pointer that
        // no longer points within the ref object. If a GC happens, the byref won't
        // get updated. This can happen, for instance, if one of the int components
        // is negative. It also requires the address generation be in a fully-interruptible
        // code region.
        //
        if (varTypeIsGC(op1->TypeGet()) && op2->TypeGet() == TYP_I_IMPL)
        {
            assert(varTypeIsGC(tree->TypeGet()) && (oper == GT_ADD));
            break;
        }

        /* Change "(x op (y op z))" to "(x op y) op z" */
        /* ie.    "(op1 op (ad1 op ad2))" to "(op1 op ad1) op ad2" */

        GenTree* new_op1 = op2;

        new_op1->AsOp()->gtOp1 = op1;
        new_op1->AsOp()->gtOp2 = ad1;

        /* Change the flags. */

        // Make sure we arent throwing away any flags
        noway_assert((new_op1->gtFlags &
                      ~(GTF_MAKE_CSE | GTF_DONT_CSE | // It is ok that new_op1->gtFlags contains GTF_DONT_CSE flag.
                        GTF_REVERSE_OPS |             // The reverse ops flag also can be set, it will be re-calculated
                        GTF_NODE_MASK | GTF_ALL_EFFECT | GTF_UNSIGNED)) == 0);

        new_op1->gtFlags =
            (new_op1->gtFlags & (GTF_NODE_MASK | GTF_DONT_CSE)) | // Make sure we propagate GTF_DONT_CSE flag.
            (op1->gtFlags & GTF_ALL_EFFECT) | (ad1->gtFlags & GTF_ALL_EFFECT);

        /* Retype new_op1 if it has not/become a GC ptr. */

        if (varTypeIsGC(op1->TypeGet()))
        {
            noway_assert((varTypeIsGC(tree->TypeGet()) && op2->TypeGet() == TYP_I_IMPL &&
                          oper == GT_ADD) || // byref(ref + (int+int))
                         (varTypeIsI(tree->TypeGet()) && op2->TypeGet() == TYP_I_IMPL &&
                          oper == GT_OR)); // int(gcref | int(gcref|intval))

            new_op1->gtType = tree->gtType;
        }
        else if (varTypeIsGC(ad2->TypeGet()))
        {
            // Neither ad1 nor op1 are GC. So new_op1 isnt either
            noway_assert(op1->gtType == TYP_I_IMPL && ad1->gtType == TYP_I_IMPL);
            new_op1->gtType = TYP_I_IMPL;
        }

        // If new_op1 is a new expression. Assign it a new unique value number.
        // vnStore is null before the ValueNumber phase has run
        if (vnStore != nullptr)
        {
            // We can only keep the old value number on new_op1 if both op1 and ad2
            // have the same non-NoVN value numbers. Since op is commutative, comparing
            // only ad2 and op1 is enough.
            if ((op1->gtVNPair.GetLiberal() == ValueNumStore::NoVN) ||
                (ad2->gtVNPair.GetLiberal() == ValueNumStore::NoVN) ||
                (ad2->gtVNPair.GetLiberal() != op1->gtVNPair.GetLiberal()))
            {
                new_op1->gtVNPair.SetBoth(vnStore->VNForExpr(nullptr, new_op1->TypeGet()));
            }
        }

        tree->AsOp()->gtOp1 = new_op1;
        tree->AsOp()->gtOp2 = ad2;

        /* If 'new_op1' is now the same nested op, process it recursively */

        if ((ad1->gtOper == oper) && !ad1->gtOverflowEx())
        {
            fgMoveOpsLeft(new_op1);
        }

        /* If   'ad2'   is now the same nested op, process it
         * Instead of recursion, we set up op1 and op2 for the next loop.
         */

        op1 = new_op1;
        op2 = ad2;
    } while ((op2->gtOper == oper) && !op2->gtOverflowEx());

    return;
}

#endif

GenTree* Compiler::fgMorphArrayIndex(GenTreeIndex* tree)
{
    // Fold "cns_str"[cns_index] to ushort constant
    if (opts.OptimizationEnabled() && tree->GetArray()->IsStrCon() && tree->GetIndex()->IsIntCnsFitsInI32())
    {
        const int cnsIndex = static_cast<int>(tree->GetIndex()->AsIntCon()->GetValue());
        if (cnsIndex >= 0)
        {
            int             length;
            const char16_t* str = info.compCompHnd->getStringLiteral(tree->GetArray()->AsStrCon()->gtScpHnd,
                                                                     tree->GetArray()->AsStrCon()->gtSconCPX, &length);
            if ((cnsIndex < length) && (str != nullptr))
            {
                assert(tree->TypeIs(TYP_USHORT));
                GenTree* cnsCharNode = gtNewIconNode(str[cnsIndex], TYP_INT);
                INDEBUG(cnsCharNode->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
                return cnsCharNode;
            }
        }
    }

    var_types    elemType   = tree->GetType();
    ClassLayout* elemLayout = tree->GetLayout();
    unsigned     elemSize   = tree->GetElemSize();

    noway_assert((elemType != TYP_STRUCT) || (elemLayout != nullptr));

    bool checkIndexRange = false;

    if ((tree->gtFlags & GTF_INX_RNGCHK) != 0)
    {
        tree->gtFlags &= ~GTF_INX_RNGCHK;
        checkIndexRange = true;
    }

    GenTree* array    = tree->GetArray();
    GenTree* index    = tree->GetIndex();
    uint8_t  lenOffs  = tree->GetLenOffs();
    uint8_t  dataOffs = tree->GetDataOffs();
    GenTree* indir    = tree;

    unsigned elemTypeNum;

    // TODO-MIKE-Review: It's not clear why the type information is discarded for SIMD types.
    // This may have some CQ implications - all arrays having the same SIMD type are treated
    // as aliased in VN (e.g. Vector128<float>[] & Vector128<int>[] & Vector4).
    if (elemType == TYP_STRUCT)
    {
        elemTypeNum = typGetLayoutNum(elemLayout);

        indir->ChangeOper(GT_OBJ);
        indir->AsObj()->SetLayout(elemLayout);
        indir->AsObj()->SetKind(StructStoreKind::Invalid);
    }
    else
    {
        elemTypeNum = static_cast<unsigned>(elemType);

        indir->ChangeOper(GT_IND);
    }

    // In minopts, we expand GT_INDEX to IND(INDEX_ADDR) in order to minimize the size of the IR. As minopts
    // compilation time is roughly proportional to the size of the IR, this helps keep compilation times down.
    // Furthermore, this representation typically saves on code size in minopts w.r.t. the complete expansion
    // performed when optimizing, as it does not require LclVar nodes (which are always stack loads/stores in
    // minopts).

    if (opts.MinOpts())
    {
        array = fgMorphTree(array);
        index = fgMorphTree(index);

        GenTreeIndexAddr* addr = new (this, GT_INDEX_ADDR) GenTreeIndexAddr(array, index, lenOffs, dataOffs, elemSize);
        INDEBUG(addr->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)

        if (checkIndexRange)
        {
            addr->gtFlags |= GTF_INX_RNGCHK | GTF_EXCEPT;
            addr->SetThrowBlock(fgGetRngChkTarget(compCurBB, SCK_RNGCHK_FAIL));
        }

        indir->AsIndir()->SetAddr(addr);
        indir->SetSideEffects(GTF_GLOB_REF | addr->GetSideEffects());

        return indir;
    }

    // When we are optimizing, we fully expand INDEX to something like:
    //
    //   COMMA(ARR_BOUNDS_CHK(index, ARR_LENGTH(array)), IND(ADD(array, ADD(MUL(index, elemSize), dataOffs))))
    //
    // This expansion explicitly exposes the bounds check and the address calculation to the optimizer, which allows
    // for more straightforward bounds-check removal, CSE, etc.

    GenTreeOp*        arrayTmpAsg = nullptr;
    GenTreeOp*        indexTmpAsg = nullptr;
    GenTreeBoundsChk* boundsCheck = nullptr;

    if (checkIndexRange)
    {
        // The array and index will have multiple uses so we need to assign them to temps, unless they're
        // simple, side effect free expressions.
        //
        // Note that if the expression is a GT_FIELD, it has not yet been morphed so its true complexity is
        // not exposed. Without that condition there are cases of local struct fields that were previously,
        // needlessly, marked as GTF_GLOB_REF, and when that was fixed, there were some regressions that
        // were mostly ameliorated by adding this condition.
        //
        // Likewise, allocate a temporary if the expression is a GT_LCL_FLD node. These used to be created
        // after fgMorphArrayIndex from GT_FIELD trees so this preserves the existing behavior. This is
        // perhaps a decision that should be left to CSE but FX diffs show that it is slightly better to
        // do this here.

        constexpr int MAX_ARR_COMPLEXITY   = 4;
        constexpr int MAX_INDEX_COMPLEXITY = 4;

        GenTree* array2 = nullptr;

        if (((array->gtFlags & (GTF_ASG | GTF_CALL | GTF_GLOB_REF)) != 0) || array->OperIs(GT_FIELD, GT_LCL_FLD) ||
            gtComplexityExceeds(array, MAX_ARR_COMPLEXITY))
        {
            unsigned arrayTmpNum = lvaNewTemp(array->GetType(), true DEBUGARG("arr expr"));

            arrayTmpAsg = gtNewAssignNode(gtNewLclvNode(arrayTmpNum, array->GetType()), array);

            array  = gtNewLclvNode(arrayTmpNum, array->GetType());
            array2 = gtNewLclvNode(arrayTmpNum, array->GetType());
        }
        else
        {
            array2 = gtCloneExpr(array);
            noway_assert(array2 != nullptr);
        }

        GenTree* arrLen = gtNewArrLen(array2, lenOffs, compCurBB);

#ifdef TARGET_64BIT
        // The CLI Spec allows an array to be indexed by either an int32 or a native int.  In the case
        // of a 64 bit architecture this means the array index can potentially be a TYP_LONG, so for this case,
        // the comparison will have to be widen to 64 bits.
        if (index->TypeIs(TYP_LONG))
        {
            arrLen = gtNewCastNode(TYP_LONG, arrLen, false, TYP_LONG);
        }
#endif

        GenTree* index2 = nullptr;

        if (((index->gtFlags & (GTF_ASG | GTF_CALL | GTF_GLOB_REF)) != 0) || index->OperIs(GT_FIELD, GT_LCL_FLD) ||
            gtComplexityExceeds(index, MAX_INDEX_COMPLEXITY))
        {
            var_types indexTmpType = varActualType(index->GetType());
            unsigned  indexTmpNum  = lvaNewTemp(indexTmpType, true DEBUGARG("index expr"));

            indexTmpAsg = gtNewAssignNode(gtNewLclvNode(indexTmpNum, indexTmpType), index);

            index  = gtNewLclvNode(indexTmpNum, indexTmpType);
            index2 = gtNewLclvNode(indexTmpNum, indexTmpType);
        }
        else
        {
            index2 = gtCloneExpr(index);
            noway_assert(index2 != nullptr);
        }

        boundsCheck = gtNewArrBoundsChk(index2, arrLen, SCK_RNGCHK_FAIL);
    }

    GenTree* offset = index;

#ifdef TARGET_64BIT
    if (!offset->TypeIs(TYP_LONG))
    {
        if (offset->OperIs(GT_CNS_INT))
        {
            offset->SetType(TYP_LONG);
        }
        else
        {
            offset = gtNewCastNode(TYP_LONG, offset, false, TYP_LONG);
        }
    }
#endif

    if (elemSize > 1)
    {
        GenTree* size = gtNewIconNode(elemSize, TYP_I_IMPL);

        // Fix 392756 WP7 Crossgen
        //
        // During codegen optGetArrayRefScaleAndIndex() makes the assumption that op2 of a GT_MUL node
        // is a constant and is not capable of handling CSE'ing the elemSize constant into a lclvar.

        // TODO-MIKE-Review: It's not clear what optGetArrayRefScaleAndIndex has to do with CSE. It's
        // used to build address modes and of course that if the constant gets CSEd then address mode
        // can't include the "constant". But was this a bug fix or a CQ fix? And why would the kind of
        // constant that can participate in address modes get CSEd anyway?

        size->SetDoNotCSE();

        offset = gtNewOperNode(GT_MUL, TYP_I_IMPL, offset, size);
    }

    // The element address is ADD(array, ADD(MUL(index, elemSize), dataOffs)). Compared to other possible
    // associations this has the advantage that the offset computation depends only on the element size
    // so it can be CSEd on its own (e.g. floatArray[i] and intArray[i] have the same offset expression
    // even if the array and the element type are different). This also minimizes the number of byrefs
    // since only the final ADD produces one.
    // It does slightly complicate array element address pattern matching as done in optIsArrayElemAddr
    // because we need to check the inner ADD to find the data offset constant. That would be simpler
    // with ADD(ADD(array, MUL(index, elemSize)), dataOffs) but then the entire expression depends on
    // "array" so CSEing is more limited.

    FieldSeqNode* arrayElement = GetFieldSeqStore()->GetArrayElement(elemTypeNum, dataOffs);

    offset = gtNewOperNode(GT_ADD, TYP_I_IMPL, offset, gtNewIconNode(dataOffs, arrayElement));

    GenTree* addr = gtNewOperNode(GT_ADD, TYP_BYREF, array, offset);

    indir->gtFlags |= GTF_IND_ARR_INDEX;

    if (boundsCheck == nullptr)
    {
        addr = fgMorphTree(addr);

        indir->AsIndir()->SetAddr(addr);
        indir->SetSideEffects(GTF_GLOB_REF | GTF_EXCEPT | addr->GetSideEffects());

        return indir;
    }

    indir->AsIndir()->SetAddr(addr);
    // If there's a bounds check, the indir itself won't fault since
    // the bounds check ensures that the address is not null.
    indir->gtFlags |= GTF_IND_NONFAULTING;
    indir->SetSideEffects(GTF_GLOB_REF | addr->GetSideEffects());

    // Note that the original INDEX node may have GTF_DONOT_CSE set, either
    // because it's the LHS of an ASG or because it is used by an ADDR. We
    // leave the setting of GTF_DONOT_CSE to ASG/ADDR post-order morphing
    // because attempting to set it here causes other problems (e.g. ADDR
    // morphing actually transforms ADDR(COMMA(_, x)) into COMMA(_, ADDR(x))
    // and forgets to clear GTF_DONOT_CSE from the COMMA node).

    GenTreeOp* comma = gtNewCommaNode(boundsCheck, indir);

    if (indexTmpAsg != nullptr)
    {
        comma = gtNewCommaNode(indexTmpAsg, comma);
    }

    if (arrayTmpAsg != nullptr)
    {
        comma = gtNewCommaNode(arrayTmpAsg, comma);
    }

    return fgMorphTree(comma);
}

GenTree* Compiler::fgMorphLocalVar(GenTree* tree, bool forceRemorph)
{
    assert(tree->OperIs(GT_LCL_VAR));

    unsigned   lclNum  = tree->AsLclVar()->GetLclNum();
    LclVarDsc* lcl     = lvaGetDesc(lclNum);
    var_types  lclType = lcl->GetType();

    if (lcl->lvAddrExposed)
    {
        tree->gtFlags |= GTF_GLOB_REF;
    }

    if (!fgGlobalMorph && !forceRemorph)
    {
        return tree;
    }

    bool varAddr = (tree->gtFlags & GTF_DONT_CSE) != 0;

    noway_assert(((tree->gtFlags & GTF_VAR_DEF) == 0) || varAddr); // GTF_VAR_DEF should always imply varAddr

    if (!varAddr && varTypeIsSmall(lclType) && lcl->lvNormalizeOnLoad())
    {
#if LOCAL_ASSERTION_PROP
        if (!optLocalAssertionProp || (optAssertionIsSubrange(tree, TYP_INT, lclType, apFull) == NO_ASSERTION_INDEX))
        {
#endif
            // Small-typed arguments and aliased locals are normalized on load.
            // Other small-typed locals are normalized on store.
            // Also, under the debugger as the debugger could write to the variable.
            // If this is one of the former, insert a narrowing cast on the load.
            //         ie. Convert: var-short --> cast-short(var-int)

            tree->SetType(TYP_INT);
            fgMorphTreeDone(tree);
            tree = gtNewCastNode(TYP_INT, tree, false, lclType);
            fgMorphTreeDone(tree);
#if LOCAL_ASSERTION_PROP
        }
#endif
    }

    return tree;
}

unsigned Compiler::fgGetLargeFieldOffsetNullCheckTemp(var_types type)
{
    assert(varTypeIsI(type));

    if (fgLargeFieldOffsetNullCheckTemps[type] == BAD_VAR_NUM)
    {
        fgLargeFieldOffsetNullCheckTemps[type] = lvaNewTemp(type, false DEBUGARG("large field offset null check temp"));
    }

    unsigned lclNum = fgLargeFieldOffsetNullCheckTemps[type];
    assert(lvaGetDesc(lclNum)->GetType() == type);
    return lclNum;
}

GenTree* Compiler::fgMorphField(GenTree* tree, MorphAddrContext* mac)
{
    CORINFO_FIELD_HANDLE fldHandle       = tree->AsField()->gtFldHnd;
    unsigned             fldOffset       = tree->AsField()->gtFldOffset;
    GenTree*             objRef          = tree->AsField()->gtFldObj;
    bool                 fieldMayOverlap = false;

    assert(objRef != nullptr);
    INDEBUG(GenTreeLclVarCommon* lclNode = objRef->IsLocalAddrExpr();)
    assert((lclNode == nullptr) || lvaGetDesc(lclNode)->IsAddressExposed());
    assert((tree->gtFlags & GTF_GLOB_REF) != 0);

    if (tree->AsField()->gtFldMayOverlap)
    {
        fieldMayOverlap = true;
        // Reset the flag because we may reuse the node.
        tree->AsField()->gtFldMayOverlap = false;
    }

    GenTree* addr;

    /* We'll create the expression "*(objRef + mem_offs)" */

    noway_assert(varTypeIsGC(objRef->TypeGet()) || objRef->TypeGet() == TYP_I_IMPL);

    /*
        Now we have a tree like this:

                              +--------------------+
                              |      GT_FIELD      |   tree
                              +----------+---------+
                                         |
                          +--------------+-------------+
                          |   tree->AsField()->gtFldObj   |
                          +--------------+-------------+


        We want to make it like this (when fldOffset is <= MAX_UNCHECKED_OFFSET_FOR_NULL_OBJECT):

                              +--------------------+
                              |   GT_IND/GT_OBJ    |   tree
                              +---------+----------+
                                        |
                                        |
                              +---------+----------+
                              |       GT_ADD       |   addr
                              +---------+----------+
                                        |
                                      /   \
                                    /       \
                                  /           \
                     +-------------------+  +----------------------+
                     |       objRef      |  |     fldOffset        |
                     |                   |  | (when fldOffset !=0) |
                     +-------------------+  +----------------------+


        or this (when fldOffset is > MAX_UNCHECKED_OFFSET_FOR_NULL_OBJECT):


                              +--------------------+
                              |   GT_IND/GT_OBJ    |   tree
                              +----------+---------+
                                         |
                              +----------+---------+
                              |       GT_COMMA     |  comma2
                              +----------+---------+
                                         |
                                        / \
                                      /     \
                                    /         \
                                  /             \
             +---------+----------+               +---------+----------+
       comma |      GT_COMMA      |               |  "+" (i.e. GT_ADD) |   addr
             +---------+----------+               +---------+----------+
                       |                                     |
                     /   \                                  /  \
                   /       \                              /      \
                 /           \                          /          \
     +-----+-----+             +-----+-----+      +---------+   +-----------+
 asg |  GT_ASG   |         ind |   GT_IND  |      |  tmpLcl |   | fldOffset |
     +-----+-----+             +-----+-----+      +---------+   +-----------+
           |                         |
          / \                        |
        /     \                      |
      /         \                    |
+-----+-----+   +-----+-----+   +-----------+
|   tmpLcl  |   |   objRef  |   |   tmpLcl  |
+-----------+   +-----------+   +-----------+


    */

    var_types objRefType = objRef->TypeGet();

    GenTree* comma = nullptr;

    // NULL mac means we encounter the GT_FIELD first.  This denotes a dereference of the field,
    // and thus is equivalent to a MACK_Ind with zero offset.
    MorphAddrContext defMAC(MACK_Ind);
    if (mac == nullptr)
    {
        mac = &defMAC;
    }

    // This flag is set to enable the "conservative" style of explicit null-check insertion.
    // This means that we insert an explicit null check whenever we create byref by adding a
    // constant offset to a ref, in a MACK_Addr context (meaning that the byref is not immediately
    // dereferenced).  The alternative is "aggressive", which would not insert such checks (for
    // small offsets); in this plan, we would transfer some null-checking responsibility to
    // callee's of methods taking byref parameters.  They would have to add explicit null checks
    // when creating derived byrefs from argument byrefs by adding constants to argument byrefs, in
    // contexts where the resulting derived byref is not immediately dereferenced (or if the offset is too
    // large).  To make the "aggressive" scheme work, however, we'd also have to add explicit derived-from-null
    // checks for byref parameters to "external" methods implemented in C++, and in P/Invoke stubs.
    // This is left here to point out how to implement it.
    CLANG_FORMAT_COMMENT_ANCHOR;

#define CONSERVATIVE_NULL_CHECK_BYREF_CREATION 1

    bool addExplicitNullCheck = false;

    // Implicit byref locals and string literals are never null.
    if (fgAddrCouldBeNull(objRef))
    {
        // If the objRef is a GT_ADDR node, it, itself, never requires null checking.  The expression
        // whose address is being taken is either a local or static variable, whose address is necessarily
        // non-null, or else it is a field dereference, which will do its own bounds checking if necessary.
        if (objRef->gtOper != GT_ADDR && (mac->m_kind == MACK_Addr || mac->m_kind == MACK_Ind))
        {
            if (!mac->m_allConstantOffsets || fgIsBigOffset(mac->m_totalOffset + fldOffset))
            {
                addExplicitNullCheck = true;
            }
            else
            {
                // In R2R mode the field offset for some fields may change when the code
                // is loaded. So we can't rely on a zero offset here to suppress the null check.
                //
                // See GitHub issue #16454.
                bool fieldHasChangeableOffset = false;

#ifdef FEATURE_READYTORUN_COMPILER
                fieldHasChangeableOffset = (tree->AsField()->GetR2RFieldLookupAddr() != nullptr);
#endif

#if CONSERVATIVE_NULL_CHECK_BYREF_CREATION
                addExplicitNullCheck =
                    (mac->m_kind == MACK_Addr) && ((mac->m_totalOffset + fldOffset > 0) || fieldHasChangeableOffset);
#else
                addExplicitNullCheck = (objRef->gtType == TYP_BYREF && mac->m_kind == MACK_Addr &&
                                        ((mac->m_totalOffset + fldOffset > 0) || fieldHasChangeableOffset));
#endif
            }
        }
    }

    if (addExplicitNullCheck)
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("Before explicit null check morphing:\n");
            gtDispTree(tree);
        }
#endif

        //
        // Create the "comma" subtree
        //
        GenTree* asg = nullptr;
        GenTree* nullchk;

        unsigned lclNum;

        if (objRef->gtOper != GT_LCL_VAR)
        {
            lclNum = fgGetLargeFieldOffsetNullCheckTemp(objRef->GetType());
            asg    = gtNewAssignNode(gtNewLclvNode(lclNum, objRef->GetType()), objRef);
        }
        else
        {
            lclNum = objRef->AsLclVarCommon()->GetLclNum();
        }

        GenTree* lclVar = gtNewLclvNode(lclNum, objRefType);
        nullchk         = gtNewNullCheck(lclVar, compCurBB);

        nullchk->gtFlags |= GTF_DONT_CSE; // Don't try to create a CSE for these TYP_BYTE indirections

        if (asg != nullptr)
        {
            comma = gtNewCommaNode(asg, nullchk);
        }
        else
        {
            comma = nullchk;
        }

        addr = gtNewLclvNode(lclNum, objRefType); // Use "tmpLcl" to create "addr" node.
    }
    else
    {
        addr = objRef;
    }

#ifdef FEATURE_READYTORUN_COMPILER
    if (tree->AsField()->GetR2RFieldLookupAddr() != nullptr)
    {
        GenTree* offsetNode =
            gtNewIndOfIconHandleNode(TYP_I_IMPL, reinterpret_cast<size_t>(tree->AsField()->GetR2RFieldLookupAddr()),
                                     GTF_ICON_CONST_PTR, true);
#ifdef DEBUG
        offsetNode->gtGetOp1()->AsIntCon()->gtTargetHandle = reinterpret_cast<size_t>(fldHandle);
#endif
        var_types addType = (objRefType == TYP_I_IMPL) ? TYP_I_IMPL : TYP_BYREF;
        addr              = gtNewOperNode(GT_ADD, addType, addr, offsetNode);
    }
#endif
    if (fldOffset != 0)
    {
        FieldSeqNode* fieldSeq =
            fieldMayOverlap ? FieldSeqStore::NotAField() : GetFieldSeqStore()->CreateSingleton(fldHandle);
        addr = gtNewOperNode(GT_ADD, (objRefType == TYP_I_IMPL) ? TYP_I_IMPL : TYP_BYREF, addr,
                             gtNewIconNode(fldOffset, fieldSeq));
    }

    // Now let's set the "tree" as a GT_IND tree.

    tree->SetOper(GT_IND);
    tree->AsIndir()->SetAddr(addr);

    tree->SetIndirExceptionFlags(this);

    if (addExplicitNullCheck)
    {
        tree->AsIndir()->SetAddr(gtNewCommaNode(comma, addr));
    }

#ifdef DEBUG
    if (verbose)
    {
        if (addExplicitNullCheck)
        {
            printf("After adding explicit null check:\n");
            gtDispTree(tree);
        }
    }
#endif

    noway_assert(tree->OperIs(GT_IND));

    if (fldOffset == 0)
    {
        GenTree* addr = tree->AsIndir()->GetAddr()->gtEffectiveVal();

        FieldSeqNode* fieldSeq =
            fieldMayOverlap ? FieldSeqStore::NotAField() : GetFieldSeqStore()->CreateSingleton(fldHandle);
        fgAddFieldSeqForZeroOffset(addr, fieldSeq);
    }

    // Pass down the current mac; if non null we are computing an address
    GenTree* result = fgMorphSmpOp(tree, mac);

#ifdef DEBUG
    if (verbose)
    {
        printf("\nFinal value of Compiler::fgMorphField after calling fgMorphSmpOp:\n");
        gtDispTree(result);
    }
#endif

    return result;
}

//------------------------------------------------------------------------
// fgCanFastTailCall: Check to see if this tail call can be optimized as epilog+jmp.
//
// Arguments:
//    callee - The callee to check
//    failReason - If this method returns false, the reason why. Can be nullptr.
//
// Return Value:
//    Returns true or false based on whether the callee can be fastTailCalled
//
// Notes:
//    This function is target specific and each target will make the fastTailCall
//    decision differently. See the notes below.
//
//    This function calls fgInitArgInfo() to initialize the arg info table, which
//    is used to analyze the argument. This function can alter the call arguments
//    by adding argument IR nodes for non-standard arguments.
//
// Windows Amd64:
//    A fast tail call can be made whenever the number of callee arguments
//    is less than or equal to the number of caller arguments, or we have four
//    or fewer callee arguments. This is because, on Windows AMD64, each
//    argument uses exactly one register or one 8-byte stack slot. Thus, we only
//    need to count arguments, and not be concerned with the size of each
//    incoming or outgoing argument.
//
// Can fast tail call examples (amd64 Windows):
//
//    -- Callee will have all register arguments --
//    caller(int, int, int, int)
//    callee(int, int, float, int)
//
//    -- Callee requires stack space that is equal or less than the caller --
//    caller(struct, struct, struct, struct, struct, struct)
//    callee(int, int, int, int, int, int)
//
//    -- Callee requires stack space that is less than the caller --
//    caller(struct, double, struct, float, struct, struct)
//    callee(int, int, int, int, int)
//
//    -- Callee will have all register arguments --
//    caller(int)
//    callee(int, int, int, int)
//
// Cannot fast tail call examples (amd64 Windows):
//
//    -- Callee requires stack space that is larger than the caller --
//    caller(struct, double, struct, float, struct, struct)
//    callee(int, int, int, int, int, double, double, double)
//
//    -- Callee has a byref struct argument --
//    caller(int, int, int)
//    callee(struct(size 3 bytes))
//
// Unix Amd64 && Arm64:
//    A fastTailCall decision can be made whenever the callee's stack space is
//    less than or equal to the caller's stack space. There are many permutations
//    of when the caller and callee have different stack sizes if there are
//    structs being passed to either the caller or callee.
//
// Exceptions:
//    1) If the callee has structs which cannot be enregistered it will be
//    reported as cannot fast tail call. This is an implementation limitation
//    where the callee only is checked for non enregisterable structs. This is
//    tracked with https://github.com/dotnet/runtime/issues/8492.
//
//    2) If the caller or callee has stack arguments and the callee has more
//    arguments then the caller it will be reported as cannot fast tail call.
//    This is due to a bug in LowerFastTailCall which assumes that
//    nCalleeArgs <= nCallerArgs, which is always true on Windows Amd64. This
//    is tracked with https://github.com/dotnet/runtime/issues/8413.
//
//    3) If the callee has a 9 to 16 byte struct argument and the callee has
//    stack arguments, the decision will be to not fast tail call. This is
//    because before fgMorphArgs is done, the struct is unknown whether it
//    will be placed on the stack or enregistered. Therefore, the conservative
//    decision of do not fast tail call is taken. This limitations should be
//    removed if/when fgMorphArgs no longer depends on fgCanFastTailCall.
//
// Can fast tail call examples (amd64 Unix):
//
//    -- Callee will have all register arguments --
//    caller(int, int, int, int)
//    callee(int, int, float, int)
//
//    -- Callee requires stack space that is equal to the caller --
//    caller({ long, long }, { int, int }, { int }, { int }, { int }, { int }) -- 6 int register arguments, 16 byte
//    stack
//    space
//    callee(int, int, int, int, int, int, int, int) -- 6 int register arguments, 16 byte stack space
//
//    -- Callee requires stack space that is less than the caller --
//    caller({ long, long }, int, { long, long }, int, { long, long }, { long, long }) 6 int register arguments, 32 byte
//    stack
//    space
//    callee(int, int, int, int, int, int, { long, long } ) // 6 int register arguments, 16 byte stack space
//
//    -- Callee will have all register arguments --
//    caller(int)
//    callee(int, int, int, int)
//
// Cannot fast tail call examples (amd64 Unix):
//
//    -- Callee requires stack space that is larger than the caller --
//    caller(float, float, float, float, float, float, float, float) -- 8 float register arguments
//    callee(int, int, int, int, int, int, int, int) -- 6 int register arguments, 16 byte stack space
//
//    -- Callee has structs which cannot be enregistered (Implementation Limitation) --
//    caller(float, float, float, float, float, float, float, float, { double, double, double }) -- 8 float register
//    arguments, 24 byte stack space
//    callee({ double, double, double }) -- 24 bytes stack space
//
//    -- Callee requires stack space and has a struct argument >8 bytes and <16 bytes (Implementation Limitation) --
//    caller(int, int, int, int, int, int, { double, double, double }) -- 6 int register arguments, 24 byte stack space
//    callee(int, int, int, int, int, int, { int, int }) -- 6 int registers, 16 byte stack space
//
//    -- Caller requires stack space and nCalleeArgs > nCallerArgs (Bug) --
//    caller({ double, double, double, double, double, double }) // 48 byte stack
//    callee(int, int) -- 2 int registers

bool Compiler::fgCanFastTailCall(GenTreeCall* callee, const char** failReason)
{
#if FEATURE_FASTTAILCALL
    // To reach here means that the return types of the caller and callee are tail call compatible.
    if (callee->IsTailPrefixedCall())
    {
        assert(impTailCallRetTypeCompatible(callee));
    }

    assert(!callee->AreArgsComplete());

    fgInitArgInfo(callee);

    fgArgInfo* argInfo = callee->fgArgInfo;

    unsigned calleeArgStackSize = 0;
    unsigned callerArgStackSize = info.compArgStackSize;

    for (unsigned index = 0; index < argInfo->ArgCount(); ++index)
    {
        fgArgTabEntry* arg = callee->GetArgInfoByArgNum(index);

        calleeArgStackSize += arg->GetSlotCount() * REGSIZE_BYTES;
    }
    calleeArgStackSize = GetOutgoingArgByteSize(calleeArgStackSize);

    auto reportFastTailCallDecision = [&](const char* thisFailReason) {
        if (failReason != nullptr)
        {
            *failReason = thisFailReason;
        }

#ifdef DEBUG
        if ((JitConfig.JitReportFastTailCallDecisions()) == 1)
        {
            if (callee->gtCallType != CT_INDIRECT)
            {
                const char* methodName;

                methodName = eeGetMethodFullName(callee->gtCallMethHnd);

                printf("[Fast tailcall decision]: Caller: %s\n[Fast tailcall decision]: Callee: %s -- Decision: ",
                       info.compFullName, methodName);
            }
            else
            {
                printf("[Fast tailcall decision]: Caller: %s\n[Fast tailcall decision]: Callee: IndirectCall -- "
                       "Decision: ",
                       info.compFullName);
            }

            if (thisFailReason == nullptr)
            {
                printf("Will fast tailcall");
            }
            else
            {
                printf("Will not fast tailcall (%s)", thisFailReason);
            }

            printf(" (CallerArgStackSize: %d, CalleeArgStackSize: %d)\n\n", callerArgStackSize, calleeArgStackSize);
        }
        else
        {
            if (thisFailReason == nullptr)
            {
                JITDUMP("[Fast tailcall decision]: Will fast tailcall\n");
            }
            else
            {
                JITDUMP("[Fast tailcall decision]: Will not fast tailcall (%s)\n", thisFailReason);
            }
        }
#endif // DEBUG
    };

    if (!opts.compFastTailCalls)
    {
        reportFastTailCallDecision("Configuration doesn't allow fast tail calls");
        return false;
    }

    if (callee->IsStressTailCall())
    {
        reportFastTailCallDecision("Fast tail calls are not performed under tail call stress");
        return false;
    }

    // Note on vararg methods:
    // If the caller is vararg method, we don't know the number of arguments passed by caller's caller.
    // But we can be sure that in-coming arg area of vararg caller would be sufficient to hold its
    // fixed args. Therefore, we can allow a vararg method to fast tail call other methods as long as
    // out-going area required for callee is bounded by caller's fixed argument space.
    //
    // Note that callee being a vararg method is not a problem since we can account the params being passed.
    //
    // We will currently decide to not fast tail call on Windows armarch if the caller or callee is a vararg
    // method. This is due to the ABI differences for native vararg methods for these platforms. There is
    // work required to shuffle arguments to the correct locations.
    CLANG_FORMAT_COMMENT_ANCHOR;

#if (defined(TARGET_WINDOWS) && defined(TARGET_ARM)) || (defined(TARGET_WINDOWS) && defined(TARGET_ARM64))
    if (info.compIsVarArgs || callee->IsVarargs())
    {
        reportFastTailCallDecision("Fast tail calls with varargs not supported on Windows ARM/ARM64");
        return false;
    }
#endif // (defined(TARGET_WINDOWS) && defined(TARGET_ARM)) || defined(TARGET_WINDOWS) && defined(TARGET_ARM64))

    if (compLocallocUsed)
    {
        reportFastTailCallDecision("Localloc used");
        return false;
    }

#ifdef TARGET_AMD64
    // Needed for Jit64 compat.
    // In future, enabling fast tail calls from methods that need GS cookie
    // check would require codegen side work to emit GS cookie check before a
    // tail call.
    if (getNeedsGSSecurityCookie())
    {
        reportFastTailCallDecision("GS Security cookie check required");
        return false;
    }
#endif

    // If the NextCallReturnAddress intrinsic is used we should do normal calls.
    if (info.compHasNextCallRetAddr)
    {
        reportFastTailCallDecision("Uses NextCallReturnAddress intrinsic");
        return false;
    }

    if (callee->HasRetBufArg()) // RetBuf
    {
        // If callee has RetBuf param, caller too must have it.
        // Otherwise go the slow route.
        if (info.compRetBuffArg == BAD_VAR_NUM)
        {
            reportFastTailCallDecision("Callee has RetBuf but caller does not.");
            return false;
        }
    }

    // For a fast tail call the caller will use its incoming arg stack space to place
    // arguments, so if the callee requires more arg stack space than is available here
    // the fast tail call cannot be performed. This is common to all platforms.
    // Note that the GC'ness of on stack args need not match since the arg setup area is marked
    // as non-interruptible for fast tail calls.
    if (calleeArgStackSize > callerArgStackSize)
    {
        reportFastTailCallDecision("Not enough incoming arg space");
        return false;
    }

    // For Windows some struct parameters are copied on the local frame
    // and then passed by reference. We cannot fast tail call in these situation
    // as we need to keep our frame around.
    if (fgCallHasMustCopyByrefParameter(callee->GetInfo()))
    {
        reportFastTailCallDecision("Callee has a byref parameter");
        return false;
    }

    reportFastTailCallDecision(nullptr);
    return true;
#else // FEATURE_FASTTAILCALL
    if (failReason)
        *failReason = "Fast tailcalls are not supported on this platform";
    return false;
#endif
}

#if FEATURE_FASTTAILCALL
//------------------------------------------------------------------------
// fgCallHasMustCopyByrefParameter: Check to see if this call has a byref parameter that
//                                  requires a struct copy in the caller.
//
// Arguments:
//    callInfo - Call node info
//
// Return Value:
//    Returns true or false based on whether this call has a byref parameter that
//    requires a struct copy in the caller.
//
bool Compiler::fgCallHasMustCopyByrefParameter(CallInfo* callInfo)
{
    for (unsigned index = 0; index < callInfo->GetArgCount(); ++index)
    {
        CallArgInfo* argInfo = callInfo->GetArgInfo(index);

        if (!argInfo->IsImplicitByRef())
        {
            continue;
        }

        if (!opts.OptimizationEnabled())
        {
            return true;
        }

        GenTreeLclVar* lclNode = argInfo->GetNode()->IsImplicitByrefIndir(this);

        if (lclNode == nullptr)
        {
            // If the implicit by-ref arg isn't itself an implicit by-ref parameter then
            // we need to make a local copy and thus can't fast tail call. N.B. that it
            // may be possible to avoid copying a local variable (e.g. if the arg is its
            // last use) but then we'd be passing the address of that local so we still
            // can't fast tail call.

            return true;
        }

        LclVarDsc* lcl = lvaGetDesc(lclNode);

        JITDUMP("Arg [%06u] is implicit byref V%02u, checking if it's aliased\n", argInfo->GetNode()->gtTreeID,
                lclNode->GetLclNum());

        if (lcl->lvRefCnt(RCS_EARLY) == 1)
        {
            JITDUMP("Arg is the only use of V%02u\n", lclNode->GetLclNum());

            continue;
        }

        // If the param has multiple uses then some of them may appear in other args of this call,
        // either because the param (or a portion of it) is passed in multiple args or because its
        // address is somehow passed in an arg. We need to scan the arg list to find such uses so
        // this has quadratic complexity.

        if (callInfo->GetArgCount() > 6)
        {
            JITDUMP("Arg alias analysis too costly, call has %u args\n", callInfo->GetArgCount());

            return true;
        }

        for (unsigned index2 = 0; index2 < callInfo->GetArgCount(); ++index2)
        {
            if (index2 == index)
            {
                continue;
            }

            CallArgInfo* argInfo2 = callInfo->GetArgInfo(index2);
            GenTree*     argNode2 = argInfo2->GetNode();

            JITDUMPTREE(argNode2, "Checking other arg:\n");

            if (argNode2->OperIsConst())
            {
                // A const arg can't alias a parameter.
                continue;
            }

            if (argInfo2->IsImplicitByRef())
            {
                GenTreeLclVar* const lclNode2 = argNode2->IsImplicitByrefIndir(this);

                if ((lclNode2 != nullptr) && (lclNode->GetLclNum() == lclNode2->GetLclNum()))
                {
                    JITDUMP("Implicit byref param V%02u value is passed in multiple args\n");
                    return true;
                }

                // TODO-MIKE-Review: Old code had a dump message saying "that the arg refers to different
                // implicit byref local". This seems bogus - there's nothing stopping one from storing
                // the address of any param into whatever struct gets passed here.
                continue;
            }

            if ((argInfo2->GetArgType() != TYP_BYREF) && (argInfo2->GetArgType() != TYP_I_IMPL))
            {
                // TODO-MIKE-Review: Similar to the above comment. We could have a struct parameter that
                // contains the address of any param.
                continue;
            }

            if (argNode2->OperIs(GT_LCL_VAR) && (argNode2->AsLclVar()->GetLclNum() == lclNode->GetLclNum()))
            {
                JITDUMP("Implicit byref param V%02u address is also passed in an arg\n");
                return true;
            }

            if (argNode2->OperIs(GT_LCL_VAR) && lvaGetDesc(argNode2->AsLclVar())->lvIsParam)
            {
                // Other params can't alias implicit byref params.

                // TODO-MIKE-Review: Here we go again - the initial param value cannot indeed alias an
                // implicit by-ref param but one could assign the address of one param to another param.
                continue;
            }

            if (lcl->lvHasLdAddrOp)
            {
                // We have no idea what address this argument contains. If the parameter is address
                // exposed then it could contain its address.

                // TODO-MIKE-CQ: lvHasLdAddrOp is likely overly conservative. lvAddrExposed should be
                // used instead but that one gets reset in lvaRetypeImplicitByRefParams.
                // Maybe lvaRetypeImplicitByRefParams could copy lvAddrExposed somewhere so we can use it
                // here, though care needs to be taken because nothing will ever update it.

                JITDUMP("V%02u is address exposed\n", lclNode->GetLclNum());
                return true;
            }
        }

        JITDUMP("No other arg can alias V%02u\n", lclNode->GetLclNum());
    }

    return false;
}
#endif

//------------------------------------------------------------------------
// fgMorphPotentialTailCall: Attempt to morph a call that the importer has
// identified as a potential tailcall to an actual tailcall and return the
// placeholder node to use in this case.
//
// Arguments:
//    call - The call to morph.
//
// Return Value:
//    Returns a node to use if the call was morphed into a tailcall. If this
//    function returns a node the call is done being morphed and the new node
//    should be used. Otherwise the call will have been demoted to a regular call
//    and should go through normal morph.
//
// Notes:
//    This is called only for calls that the importer has already identified as
//    potential tailcalls. It will do profitability and legality checks and
//    classify which kind of tailcall we are able to (or should) do, along with
//    modifying the trees to perform that kind of tailcall.
//
GenTree* Compiler::fgMorphPotentialTailCall(GenTreeCall* call)
{
    // It should either be an explicit (i.e. tail prefixed) or an implicit tail call
    assert(call->IsTailPrefixedCall() ^ call->IsImplicitTailCall());

    // It cannot be an inline candidate
    assert(!call->IsInlineCandidate());

    auto failTailCall = [&](const char* reason, unsigned lclNum = BAD_VAR_NUM) {
#ifdef DEBUG
        if (verbose)
        {
            printf("\nRejecting tail call in morph for call ");
            printTreeID(call);
            printf(": %s", reason);
            if (lclNum != BAD_VAR_NUM)
            {
                printf(" V%02u", lclNum);
            }
            printf("\n");
        }
#endif

        // for non user funcs, we have no handles to report
        info.compCompHnd->reportTailCallDecision(nullptr,
                                                 (call->gtCallType == CT_USER_FUNC) ? call->gtCallMethHnd : nullptr,
                                                 call->IsTailPrefixedCall(), TAILCALL_FAIL, reason);

        // We have checked the candidate so demote.
        call->gtCallMoreFlags &= ~GTF_CALL_M_EXPLICIT_TAILCALL;
#if FEATURE_TAILCALL_OPT
        call->gtCallMoreFlags &= ~GTF_CALL_M_IMPLICIT_TAILCALL;
#endif
    };

    if (call->gtCallMoreFlags & GTF_CALL_M_SPECIAL_INTRINSIC)
    {
        failTailCall("Might turn into an intrinsic");
        return nullptr;
    }

    // Heuristic: regular calls to noreturn methods can sometimes be
    // merged, so if we have multiple such calls, we defer tail calling.
    //
    // TODO: re-examine this; now that we're merging before morph we
    // don't need to worry about interfering with merges.
    //
    if (call->IsNoReturn() && (optNoReturnCallCount > 1))
    {
        failTailCall("Defer tail calling throw helper; anticipating merge");
        return nullptr;
    }

#ifdef DEBUG
    if (opts.compGcChecks && (info.compRetType == TYP_REF))
    {
        failTailCall("COMPlus_JitGCChecks or stress might have interposed a call to CORINFO_HELP_CHECK_OBJ, "
                     "invalidating tailcall opportunity");
        return nullptr;
    }
#endif

    // We have to ensure to pass the incoming retValBuf as the
    // outgoing one. Using a temp will not do as this function will
    // not regain control to do the copy. This can happen when inlining
    // a tailcall which also has a potential tailcall in it: the IL looks
    // like we can do a tailcall, but the trees generated use a temp for the inlinee's
    // result. TODO-CQ: Fix this.
    if (info.compRetBuffArg != BAD_VAR_NUM)
    {
        noway_assert(call->TypeGet() == TYP_VOID);
        GenTree* retValBuf = call->gtCallArgs->GetNode();
        if (retValBuf->gtOper != GT_LCL_VAR || retValBuf->AsLclVarCommon()->GetLclNum() != info.compRetBuffArg)
        {
            failTailCall("Need to copy return buffer");
            return nullptr;
        }
    }

    // We are still not sure whether it can be a tail call. Because, when converting
    // a call to an implicit tail call, we must check that there are no locals with
    // their address taken.  If this is the case, we have to assume that the address
    // has been leaked and the current stack frame must live until after the final
    // call.

    // Verify that none of vars has lvHasLdAddrOp or lvAddrExposed bit set. Note
    // that lvHasLdAddrOp is much more conservative.  We cannot just base it on
    // lvAddrExposed alone since it is not guaranteed to be set on all VarDscs
    // during morph stage. The reason for also checking lvAddrExposed is that in case
    // of vararg methods user args are marked as addr exposed but not lvHasLdAddrOp.
    // The combination of lvHasLdAddrOp and lvAddrExposed though conservative allows us
    // never to be incorrect.
    //
    // TODO-Throughput: have a compiler level flag to indicate whether method has vars whose
    // address is taken. Such a flag could be set whenever lvHasLdAddrOp or LvAddrExposed
    // is set. This avoids the need for iterating through all lcl vars of the current
    // method.  Right now throughout the code base we are not consistently using 'set'
    // method to set lvHasLdAddrOp and lvAddrExposed flags.

    bool isImplicitOrStressTailCall = call->IsImplicitTailCall() || call->IsStressTailCall();
    if (isImplicitOrStressTailCall && compLocallocUsed)
    {
        failTailCall("Localloc used");
        return nullptr;
    }

    bool hasStructParam = false;
    for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum);

        if (lcl->IsPromotedField() && lvaGetDesc(lcl->GetPromotedFieldParentLclNum())->IsImplicitByRefParam())
        {
            // This was a promoted field of an implicit byref param but promotion was
            // undone. It's not used so ignore it.
            continue;
        }

        // If the method is marked as an explicit tail call we will skip the
        // following three hazard checks.
        // We still must check for any struct parameters and set 'hasStructParam'
        // so that we won't transform the recursive tail call into a loop.
        if (isImplicitOrStressTailCall)
        {
            if (lcl->lvHasLdAddrOp && !lcl->IsImplicitByRefParam())
            {
                failTailCall("Local address taken", lclNum);
                return nullptr;
            }

            if (lcl->lvAddrExposed)
            {
                failTailCall("Local address taken", lclNum);
                return nullptr;
            }

            if (lcl->lvPromoted && lcl->lvIsParam)
            {
                failTailCall("Has Struct Promoted Param", lclNum);
                return nullptr;
            }

            if (lcl->lvPinned)
            {
                // A tail call removes the method from the stack, which means the pinning
                // goes away for the callee.  We can't allow that.
                failTailCall("Has Pinned Vars", lclNum);
                return nullptr;
            }
        }

        if (varTypeIsStruct(lcl->TypeGet()) && lcl->lvIsParam)
        {
            hasStructParam = true;
            // This prevents transforming a recursive tail call into a loop
            // but doesn't prevent tail call optimization so we need to
            // look at the rest of parameters.
        }
    }

    if (!fgCheckStmtAfterTailCall())
    {
        failTailCall("Unexpected statements after the tail call");
        return nullptr;
    }

    const char* failReason      = nullptr;
    bool        canFastTailCall = fgCanFastTailCall(call, &failReason);

    CORINFO_TAILCALL_HELPERS tailCallHelpers;
    bool                     tailCallViaJitHelper = false;
    if (!canFastTailCall)
    {
        if (call->IsImplicitTailCall())
        {
            // Implicit or opportunistic tail calls are always dispatched via fast tail call
            // mechanism and never via tail call helper for perf.
            failTailCall(failReason);
            return nullptr;
        }

        assert(call->IsTailPrefixedCall());
        assert(call->tailCallInfo != nullptr);

        // We do not currently handle non-standard args except for VSD stubs.
        if (!call->IsVirtualStub() && call->HasNonStandardAddedArgs(this))
        {
            failTailCall(
                "Method with non-standard args passed in callee trash register cannot be tail called via helper");
            return nullptr;
        }

        // On x86 we have a faster mechanism than the general one which we use
        // in almost all cases. See fgCanTailCallViaJitHelper for more information.
        if (fgCanTailCallViaJitHelper())
        {
            tailCallViaJitHelper = true;
        }
        else
        {
            // Make sure we can get the helpers. We do this last as the runtime
            // will likely be required to generate these.
            CORINFO_RESOLVED_TOKEN* token = nullptr;
            CORINFO_SIG_INFO*       sig   = call->tailCallInfo->GetSig();
            unsigned                flags = 0;
            if (!call->tailCallInfo->IsCalli())
            {
                token = call->tailCallInfo->GetToken();
                if (call->tailCallInfo->IsCallvirt())
                {
                    flags |= CORINFO_TAILCALL_IS_CALLVIRT;
                }
            }

            if (call->gtCallThisArg != nullptr)
            {
                var_types thisArgType = call->gtCallThisArg->GetNode()->TypeGet();
                if (thisArgType != TYP_REF)
                {
                    flags |= CORINFO_TAILCALL_THIS_ARG_IS_BYREF;
                }
            }

            if (!info.compCompHnd->getTailCallHelpers(token, sig, (CORINFO_GET_TAILCALL_HELPERS_FLAGS)flags,
                                                      &tailCallHelpers))
            {
                failTailCall("Tail call help not available");
                return nullptr;
            }
        }
    }

    // Check if we can make the tailcall a loop.
    bool fastTailCallToLoop = false;
#if FEATURE_TAILCALL_OPT
    // TODO-CQ: enable the transformation when the method has a struct parameter that can be passed in a register
    // or return type is a struct that can be passed in a register.
    //
    // TODO-CQ: if the method being compiled requires generic context reported in gc-info (either through
    // hidden generic context param or through keep alive thisptr), then while transforming a recursive
    // call to such a method requires that the generic context stored on stack slot be updated.  Right now,
    // fgMorphRecursiveFastTailCallIntoLoop() is not handling update of generic context while transforming
    // a recursive call into a loop.  Another option is to modify gtIsRecursiveCall() to check that the
    // generic type parameters of both caller and callee generic method are the same.
    if (opts.compTailCallLoopOpt && canFastTailCall && gtIsRecursiveCall(call) && !lvaReportParamTypeArg() &&
        !lvaKeepAliveAndReportThis() && !call->IsVirtual() && !hasStructParam && !varTypeIsStruct(call->TypeGet()))
    {
        fastTailCallToLoop = true;
    }
#endif

    // Ok -- now we are committed to performing a tailcall. Report the decision.
    CorInfoTailCall tailCallResult;
    if (fastTailCallToLoop)
    {
        tailCallResult = TAILCALL_RECURSIVE;
    }
    else if (canFastTailCall)
    {
        tailCallResult = TAILCALL_OPTIMIZED;
    }
    else
    {
        tailCallResult = TAILCALL_HELPER;
    }

    info.compCompHnd->reportTailCallDecision(nullptr,
                                             (call->gtCallType == CT_USER_FUNC) ? call->gtCallMethHnd : nullptr,
                                             call->IsTailPrefixedCall(), tailCallResult, nullptr);

    // Are we currently planning to expand the gtControlExpr as an early virtual call target?
    //
    if (call->IsExpandedEarly() && call->IsVirtualVtable())
    {
        // It isn't alway profitable to expand a virtual call early
        //
        // We alway expand the TAILCALL_HELPER type late.
        // And we exapnd late when we have an optimized tail call
        // and the this pointer needs to be evaluated into a temp.
        //
        if (tailCallResult == TAILCALL_HELPER)
        {
            // We will alway expand this late in lower instead.
            // (see LowerTailCallViaJitHelper as it needs some work
            // for us to be able to expand this earlier in morph)
            //
            call->ClearExpandedEarly();
        }
        else if ((tailCallResult == TAILCALL_OPTIMIZED) &&
                 ((call->gtCallThisArg->GetNode()->gtFlags & GTF_SIDE_EFFECT) != 0))
        {
            // We generate better code when we expand this late in lower instead.
            //
            call->ClearExpandedEarly();
        }
    }

    // Now actually morph the call.
    compTailCallUsed = true;
    // This will prevent inlining this call.
    call->gtCallMoreFlags |= GTF_CALL_M_TAILCALL;
    if (tailCallViaJitHelper)
    {
        call->gtCallMoreFlags |= GTF_CALL_M_TAILCALL_VIA_JIT_HELPER;
    }

#if FEATURE_TAILCALL_OPT
    if (fastTailCallToLoop)
    {
        call->gtCallMoreFlags |= GTF_CALL_M_TAILCALL_TO_LOOP;
    }
#endif

    // Mark that this is no longer a pending tailcall. We need to do this before
    // we call fgMorphCall again (which happens in the fast tailcall case) to
    // avoid recursing back into this method.
    call->gtCallMoreFlags &= ~GTF_CALL_M_EXPLICIT_TAILCALL;
#if FEATURE_TAILCALL_OPT
    call->gtCallMoreFlags &= ~GTF_CALL_M_IMPLICIT_TAILCALL;
#endif

#ifdef DEBUG
    if (verbose)
    {
        printf("\nGTF_CALL_M_TAILCALL bit set for call ");
        printTreeID(call);
        printf("\n");
        if (fastTailCallToLoop)
        {
            printf("\nGTF_CALL_M_TAILCALL_TO_LOOP bit set for call ");
            printTreeID(call);
            printf("\n");
        }
    }
#endif

    // If this block has a flow successor, make suitable updates.
    //
    BasicBlock* const nextBlock = compCurBB->GetUniqueSucc();

    if (nextBlock == nullptr)
    {
        // No unique successor. compCurBB should be a return.
        //
        assert(compCurBB->bbJumpKind == BBJ_RETURN);
    }
    else
    {
        // Flow no longer reaches nextBlock from here.
        //
        fgRemoveRefPred(nextBlock, compCurBB);

        // Adjust profile weights.
        //
        // Note if this is a tail call to loop, further updates
        // are needed once we install the loop edge.
        //
        if (compCurBB->hasProfileWeight() && nextBlock->hasProfileWeight())
        {
            // Since we have linear flow we can update the next block weight.
            //
            BasicBlock::weight_t const blockWeight   = compCurBB->bbWeight;
            BasicBlock::weight_t const nextWeight    = nextBlock->bbWeight;
            BasicBlock::weight_t const newNextWeight = nextWeight - blockWeight;

            // If the math would result in a negative weight then there's
            // no local repair we can do; just leave things inconsistent.
            //
            if (newNextWeight >= 0)
            {
                // Note if we'd already morphed the IR in nextblock we might
                // have done something profile sensitive that we should arguably reconsider.
                //
                JITDUMP("Reducing profile weight of " FMT_BB " from " FMT_WT " to " FMT_WT "\n", nextBlock->bbNum,
                        nextWeight, newNextWeight);

                nextBlock->setBBProfileWeight(newNextWeight);
            }
            else
            {
                JITDUMP("Not reducing profile weight of " FMT_BB " as its weight " FMT_WT
                        " is less than direct flow pred " FMT_BB " weight " FMT_WT "\n",
                        nextBlock->bbNum, nextWeight, compCurBB->bbNum, blockWeight);
            }

            // If nextBlock is not a BBJ_RETURN, it should have a unique successor that
            // is a BBJ_RETURN, as we allow a little bit of flow after a tail call.
            //
            if (nextBlock->bbJumpKind != BBJ_RETURN)
            {
                BasicBlock* nextNextBlock = nextBlock->GetUniqueSucc();

                // Check if we have a sequence of GT_ASG blocks where the same variable is assigned
                // to temp locals over and over.
                //
                // Also allow casts on the RHSs of the assignments, and blocks with GT_NOPs.
                //
                if (nextNextBlock->bbJumpKind != BBJ_RETURN)
                {
                    // Make sure the block has a single statement
                    assert(nextBlock->firstStmt() == nextBlock->lastStmt());
                    // And the root node is "ASG(LCL_VAR, LCL_VAR)"
                    GenTree* asgNode = nextBlock->firstStmt()->GetRootNode();
                    assert(asgNode->OperIs(GT_ASG));

                    unsigned lcl = asgNode->gtGetOp1()->AsLclVarCommon()->GetLclNum();

                    while (nextNextBlock->bbJumpKind != BBJ_RETURN)
                    {
                        assert(nextNextBlock->firstStmt() == nextNextBlock->lastStmt());
                        asgNode = nextNextBlock->firstStmt()->GetRootNode();
                        if (!asgNode->OperIs(GT_NOP))
                        {
                            assert(asgNode->OperIs(GT_ASG));

                            GenTree* rhs = asgNode->gtGetOp2();
                            while (rhs->OperIs(GT_CAST))
                            {
                                assert(!rhs->gtOverflow());
                                rhs = rhs->gtGetOp1();
                            }

                            assert(lcl == rhs->AsLclVarCommon()->GetLclNum());
                            lcl = rhs->AsLclVarCommon()->GetLclNum();
                        }
                        nextNextBlock = nextNextBlock->GetUniqueSucc();
                    }
                }

                assert(nextNextBlock->bbJumpKind == BBJ_RETURN);

                if (nextNextBlock->hasProfileWeight())
                {
                    // Do similar updates here.
                    //
                    BasicBlock::weight_t const nextNextWeight    = nextNextBlock->bbWeight;
                    BasicBlock::weight_t const newNextNextWeight = nextNextWeight - blockWeight;

                    // If the math would result in an negative weight then there's
                    // no local repair we can do; just leave things inconsistent.
                    //
                    if (newNextNextWeight >= 0)
                    {
                        JITDUMP("Reducing profile weight of " FMT_BB " from " FMT_WT " to " FMT_WT "\n",
                                nextNextBlock->bbNum, nextNextWeight, newNextNextWeight);

                        nextNextBlock->setBBProfileWeight(newNextNextWeight);
                    }
                    else
                    {
                        JITDUMP("Not reducing profile weight of " FMT_BB " as its weight " FMT_WT
                                " is less than direct flow pred " FMT_BB " weight " FMT_WT "\n",
                                nextNextBlock->bbNum, nextNextWeight, compCurBB->bbNum, blockWeight);
                    }
                }
            }
        }
    }

#if !FEATURE_TAILCALL_OPT_SHARED_RETURN
    // We enable shared-ret tail call optimization for recursive calls even if
    // FEATURE_TAILCALL_OPT_SHARED_RETURN is not defined.
    if (gtIsRecursiveCall(call))
#endif
    {
        // Many tailcalls will have call and ret in the same block, and thus be
        // BBJ_RETURN, but if the call falls through to a ret, and we are doing a
        // tailcall, change it here.
        compCurBB->bbJumpKind = BBJ_RETURN;
    }

    GenTree* stmtExpr = fgMorphStmt->GetRootNode();

#ifdef DEBUG
    // Tail call needs to be in one of the following IR forms
    //    Either a call stmt or
    //    GT_RETURN(GT_CALL(..)) or GT_RETURN(GT_CAST(GT_CALL(..)))
    //    var = GT_CALL(..) or var = (GT_CAST(GT_CALL(..)))
    //    GT_COMMA(GT_CALL(..), GT_NOP) or GT_COMMA(GT_CAST(GT_CALL(..)), GT_NOP)
    // In the above,
    //    GT_CASTS may be nested.
    genTreeOps stmtOper = stmtExpr->gtOper;
    if (stmtOper == GT_CALL)
    {
        assert(stmtExpr == call);
    }
    else
    {
        assert(stmtOper == GT_RETURN || stmtOper == GT_ASG || stmtOper == GT_COMMA);
        GenTree* treeWithCall;
        if (stmtOper == GT_RETURN)
        {
            treeWithCall = stmtExpr->gtGetOp1();
        }
        else if (stmtOper == GT_COMMA)
        {
            // Second operation must be nop.
            assert(stmtExpr->gtGetOp2()->IsNothingNode());
            treeWithCall = stmtExpr->gtGetOp1();
        }
        else
        {
            treeWithCall = stmtExpr->gtGetOp2();
        }

        // Peel off casts
        while (treeWithCall->gtOper == GT_CAST)
        {
            assert(!treeWithCall->gtOverflow());
            treeWithCall = treeWithCall->gtGetOp1();
        }

        assert(treeWithCall == call);
    }
#endif
    GenTree* result;
    if (!canFastTailCall && !tailCallViaJitHelper)
    {
        // For tailcall via CORINFO_TAILCALL_HELPERS we transform into regular
        // calls with (to the JIT) regular control flow so we do not need to do
        // much special handling.
        result = fgMorphTailCallViaHelpers(call, tailCallHelpers);
    }
    else
    {
        // Otherwise we will transform into something that does not return. For
        // fast tailcalls a "jump" and for tailcall via JIT helper a call to a
        // JIT helper that does not return. So peel off everything after the
        // call.
        Statement* nextMorphStmt = fgMorphStmt->GetNextStmt();
        JITDUMP("Remove all stmts after the call.\n");
        while (nextMorphStmt != nullptr)
        {
            Statement* stmtToRemove = nextMorphStmt;
            nextMorphStmt           = stmtToRemove->GetNextStmt();
            fgRemoveStmt(compCurBB, stmtToRemove);
        }

        bool     isRootReplaced = false;
        GenTree* root           = fgMorphStmt->GetRootNode();

        if (root != call)
        {
            JITDUMP("Replace root node [%06d] with [%06d] tail call node.\n", dspTreeID(root), dspTreeID(call));
            isRootReplaced = true;
            fgMorphStmt->SetRootNode(call);
        }

        var_types retType = call->GetType();

        // Avoid potential extra work for the return (for example, vzeroupper)
        call->SetType(TYP_VOID);
        call->SetRetSigType(TYP_VOID);
        call->SetRetLayout(nullptr);
        call->GetRetDesc()->Reset();

        // Do some target-specific transformations (before we process the args,
        // etc.) for the JIT helper case.
        if (tailCallViaJitHelper)
        {
            fgMorphTailCallViaJitHelper(call);

            // Force re-evaluating the argInfo. fgMorphTailCallViaJitHelper will modify the
            // argument list, invalidating the argInfo.
            call->fgArgInfo = nullptr;
        }

        // Tail call via JIT helper: The VM can't use return address hijacking
        // if we're not going to return and the helper doesn't have enough info
        // to safely poll, so we poll before the tail call, if the block isn't
        // already safe. Since tail call via helper is a slow mechanism it
        // doen't matter whether we emit GC poll. his is done to be in parity
        // with Jit64. Also this avoids GC info size increase if all most all
        // methods are expected to be tail calls (e.g. F#).
        //
        // Note that we can avoid emitting GC-poll if we know that the current
        // BB is dominated by a Gc-SafePoint block. But we don't have dominator
        // info at this point. One option is to just add a place holder node for
        // GC-poll (e.g. GT_GCPOLL) here and remove it in lowering if the block
        // is dominated by a GC-SafePoint. For now it not clear whether
        // optimizing slow tail calls is worth the effort. As a low cost check,
        // we check whether the first and current basic blocks are
        // GC-SafePoints.
        //
        // Fast Tail call as epilog+jmp - No need to insert GC-poll. Instead,
        // fgSetBlockOrder() is going to mark the method as fully interruptible
        // if the block containing this tail call is reachable without executing
        // any call.
        BasicBlock* curBlock = compCurBB;
        if (canFastTailCall || (fgFirstBB->bbFlags & BBF_GC_SAFE_POINT) || (compCurBB->bbFlags & BBF_GC_SAFE_POINT) ||
            (fgCreateGCPoll(GCPOLL_INLINE, compCurBB) == curBlock))
        {
            // We didn't insert a poll block, so we need to morph the call now
            // (Normally it will get morphed when we get to the split poll block)
            GenTree* temp = fgMorphCall(call);
            noway_assert(temp == call);
        }

        // Fast tail call: in case of fast tail calls, we need a jmp epilog and
        // hence mark it as BBJ_RETURN with BBF_JMP flag set.
        noway_assert(compCurBB->bbJumpKind == BBJ_RETURN);
        if (canFastTailCall)
        {
            compCurBB->bbFlags |= BBF_HAS_JMP;
        }
        else
        {
            // We call CORINFO_HELP_TAILCALL which does not return, so we will
            // not need epilogue.
            compCurBB->bbJumpKind = BBJ_THROW;
        }

        if (isRootReplaced)
        {
            // We have replaced the root node of this stmt and deleted the rest,
            // but we still have the deleted, dead nodes on the `fgMorph*` stack
            // if the root node was an `ASG`, `RET` or `CAST`.
            // Return a zero con node to exit morphing of the old trees without asserts
            // and forbid POST_ORDER morphing doing something wrong with our call.

            if (varTypeIsStruct(retType))
            {
                retType = TYP_INT;
            }

            result = fgMorphTree(gtNewZeroConNode(retType));
        }
        else
        {
            result = call;
        }
    }

    return result;
}

//------------------------------------------------------------------------
// fgMorphTailCallViaHelpers: Transform the given GT_CALL tree for tailcall code
// generation.
//
// Arguments:
//     call - The call to transform
//     helpers - The tailcall helpers provided by the runtime.
//
// Return Value:
//    Returns the transformed node.
//
// Notes:
//   This transforms
//     GT_CALL
//         {callTarget}
//         {this}
//         {args}
//   into
//     GT_COMMA
//       GT_CALL StoreArgsStub
//         {callTarget}         (depending on flags provided by the runtime)
//         {this}               (as a regular arg)
//         {args}
//       GT_COMMA
//         GT_CALL Dispatcher
//           GT_ADDR ReturnAddress
//           {CallTargetStub}
//           GT_ADDR ReturnValue
//         GT_LCL ReturnValue
// whenever the call node returns a value. If the call node does not return a
// value the last comma will not be there.
//
GenTree* Compiler::fgMorphTailCallViaHelpers(GenTreeCall* call, CORINFO_TAILCALL_HELPERS& help)
{
    // R2R requires different handling but we don't support tailcall via
    // helpers in R2R yet, so just leave it for now.
    // TODO: R2R: TailCallViaHelper
    assert(!opts.IsReadyToRun());

    JITDUMP("fgMorphTailCallViaHelpers (before):\n");
    DISPTREE(call);

    // Don't support tail calling helper methods
    assert(call->gtCallType != CT_HELPER);

    // We come this route only for tail prefixed calls that cannot be dispatched as
    // fast tail calls
    assert(!call->IsImplicitTailCall());

    // We want to use the following assert, but it can modify the IR in some cases, so we
    // can't do that in an assert.
    // assert(!fgCanFastTailCall(call, nullptr));

    bool virtualCall = call->IsVirtual();

    // If VSD then get rid of arg to VSD since we turn this into a direct call.
    // The extra arg will be the first arg so this needs to be done before we
    // handle the retbuf below.
    if (call->IsVirtualStub())
    {
        JITDUMP("This is a VSD\n");
#if FEATURE_FASTTAILCALL
        call->ResetArgInfo();
#endif

        call->gtFlags &= ~GTF_CALL_VIRT_STUB;
    }

    GenTree* callDispatcherAndGetResult = fgCreateCallDispatcherAndGetResult(call, help.hCallTarget, help.hDispatcher);

    // Change the call to a call to the StoreArgs stub.
    if (call->HasRetBufArg())
    {
        JITDUMP("Removing retbuf");
        call->gtCallArgs = call->gtCallArgs->GetNext();
        call->gtCallMoreFlags &= ~GTF_CALL_M_RETBUFFARG;

        // We changed args so recompute info.
        call->fgArgInfo = nullptr;
    }

    const bool stubNeedsTargetFnPtr = (help.flags & CORINFO_TAILCALL_STORE_TARGET) != 0;

    GenTree* doBeforeStoreArgsStub = nullptr;
    GenTree* thisPtrStubArg        = nullptr;

    // Put 'this' in normal param list
    if (call->gtCallThisArg != nullptr)
    {
        JITDUMP("Moving this pointer into arg list\n");
        GenTree* objp       = call->gtCallThisArg->GetNode();
        GenTree* thisPtr    = nullptr;
        call->gtCallThisArg = nullptr;

        // JIT will need one or two copies of "this" in the following cases:
        //   1) the call needs null check;
        //   2) StoreArgs stub needs the target function pointer address and if the call is virtual
        //      the stub also needs "this" in order to evalute the target.

        const bool callNeedsNullCheck = call->NeedsNullCheck();
        const bool stubNeedsThisPtr   = stubNeedsTargetFnPtr && virtualCall;

        // TODO-Review: The following transformation is implemented under assumption that
        // both conditions can be true. However, I could not construct such example
        // where a virtual tail call would require null check. In case, if the conditions
        // are mutually exclusive the following could be simplified.

        if (callNeedsNullCheck || stubNeedsThisPtr)
        {
            // Clone "this" if "this" has no side effects.
            if ((objp->gtFlags & GTF_SIDE_EFFECT) == 0)
            {
                thisPtr = gtClone(objp, true);
            }

            // Create a temp and spill "this" to the temp if "this" has side effects or "this" was too complex to clone.
            if (thisPtr == nullptr)
            {
                unsigned lclNum = lvaNewTemp(objp->GetType(), true DEBUGARG("tail call thisptr"));

                // tmp = "this"
                doBeforeStoreArgsStub = gtNewAssignNode(gtNewLclvNode(lclNum, objp->GetType()), objp);

                if (callNeedsNullCheck)
                {
                    // COMMA(tmp = "this", deref(tmp))
                    GenTree* tmp          = gtNewLclvNode(lclNum, objp->TypeGet());
                    GenTree* nullcheck    = gtNewNullCheck(tmp, compCurBB);
                    doBeforeStoreArgsStub = gtNewCommaNode(doBeforeStoreArgsStub, nullcheck);
                }

                thisPtr = gtNewLclvNode(lclNum, objp->TypeGet());

                if (stubNeedsThisPtr)
                {
                    thisPtrStubArg = gtNewLclvNode(lclNum, objp->TypeGet());
                }
            }
            else
            {
                if (callNeedsNullCheck)
                {
                    // deref("this")
                    doBeforeStoreArgsStub = gtNewNullCheck(objp, compCurBB);

                    if (stubNeedsThisPtr)
                    {
                        thisPtrStubArg = gtClone(objp, true);
                    }
                }
                else
                {
                    assert(stubNeedsThisPtr);

                    thisPtrStubArg = objp;
                }
            }

            call->gtFlags &= ~GTF_CALL_NULLCHECK;

            assert((thisPtrStubArg != nullptr) == stubNeedsThisPtr);
        }
        else
        {
            thisPtr = objp;
        }

        // During rationalization tmp="this" and null check will be materialized
        // in the right execution order.
        assert(thisPtr != nullptr);
        call->gtCallArgs = gtPrependNewCallArg(thisPtr, call->gtCallArgs);
        call->fgArgInfo  = nullptr;
    }

    // We may need to pass the target, for instance for calli or generic methods
    // where we pass instantiating stub.
    if (stubNeedsTargetFnPtr)
    {
        JITDUMP("Adding target since VM requested it\n");
        GenTree* target;
        if (!virtualCall)
        {
            if (call->gtCallType == CT_INDIRECT)
            {
                noway_assert(call->gtCallAddr != nullptr);
                target = call->gtCallAddr;
            }
            else
            {
                CORINFO_CONST_LOOKUP addrInfo;
                info.compCompHnd->getFunctionEntryPoint(call->gtCallMethHnd, &addrInfo);

                CORINFO_GENERIC_HANDLE handle       = nullptr;
                void*                  pIndirection = nullptr;
                assert(addrInfo.accessType != IAT_PPVALUE && addrInfo.accessType != IAT_RELPVALUE);

                if (addrInfo.accessType == IAT_VALUE)
                {
                    handle = addrInfo.handle;
                }
                else if (addrInfo.accessType == IAT_PVALUE)
                {
                    pIndirection = addrInfo.addr;
                }
                target = gtNewIconEmbHndNode(handle, pIndirection, GTF_ICON_FTN_ADDR, call->gtCallMethHnd);
            }
        }
        else
        {
            assert(!call->tailCallInfo->GetSig()->hasTypeArg());

            CORINFO_CALL_INFO callInfo;
            unsigned          flags = CORINFO_CALLINFO_LDFTN;
            if (call->tailCallInfo->IsCallvirt())
            {
                flags |= CORINFO_CALLINFO_CALLVIRT;
            }

            eeGetCallInfo(call->tailCallInfo->GetToken(), nullptr, (CORINFO_CALLINFO_FLAGS)flags, &callInfo);
            target = getVirtMethodPointerTree(thisPtrStubArg, call->tailCallInfo->GetToken(), &callInfo);
        }

        // Insert target as last arg
        GenTreeCall::Use** newArgSlot = &call->gtCallArgs;
        while (*newArgSlot != nullptr)
        {
            newArgSlot = &(*newArgSlot)->NextRef();
        }

        *newArgSlot = gtNewCallArgs(target);

        call->fgArgInfo = nullptr;
    }

    // This is now a direct call to the store args stub and not a tailcall.
    call->gtCallType    = CT_USER_FUNC;
    call->gtCallMethHnd = help.hStoreArgs;
    call->gtFlags &= ~GTF_CALL_VIRT_KIND_MASK;
    call->gtCallMoreFlags &= ~(GTF_CALL_M_TAILCALL | GTF_CALL_M_DELEGATE_INV | GTF_CALL_M_WRAPPER_DELEGATE_INV);

    // The store-args stub returns no value.
    call->SetType(TYP_VOID);
    call->SetRetSigType(TYP_VOID);
    call->SetRetLayout(nullptr);
    call->GetRetDesc()->Reset();

    GenTree* callStoreArgsStub = call;

    if (doBeforeStoreArgsStub != nullptr)
    {
        callStoreArgsStub = gtNewCommaNode(doBeforeStoreArgsStub, callStoreArgsStub);
    }

    GenTree* finalTree = gtNewCommaNode(callStoreArgsStub, callDispatcherAndGetResult);
    finalTree          = fgMorphTree(finalTree);
    JITDUMPTREE(finalTree, "fgMorphTailCallViaHelpers (after):\n");
    return finalTree;
}

//------------------------------------------------------------------------
// fgCreateCallDispatcherAndGetResult: Given a call
// CALL
//   {callTarget}
//   {retbuf}
//   {this}
//   {args}
// create a similarly typed node that calls the tailcall dispatcher and returns
// the result, as in the following:
// COMMA
//   CALL TailCallDispatcher
//     ADDR ReturnAddress
//     &CallTargetFunc
//     ADDR RetValue
//   RetValue
// If the call has type TYP_VOID, only create the CALL node.
//
// Arguments:
//    origCall - the call
//    callTargetStubHnd - the handle of the CallTarget function (this is a special
//    IL stub created by the runtime)
//    dispatcherHnd - the handle of the tailcall dispatcher function
//
// Return Value:
//    A node that can be used in place of the original call.
//
GenTree* Compiler::fgCreateCallDispatcherAndGetResult(GenTreeCall*          origCall,
                                                      CORINFO_METHOD_HANDLE callTargetStubHnd,
                                                      CORINFO_METHOD_HANDLE dispatcherHnd)
{
    GenTreeCall* callDispatcherNode =
        gtNewCallNode(CT_USER_FUNC, dispatcherHnd, TYP_VOID, nullptr, fgMorphStmt->GetILOffsetX());
    // The dispatcher has signature
    // void DispatchTailCalls(void* callersRetAddrSlot, void* callTarget, void* retValue)

    // Add return value arg.
    GenTree* retValArg;
    GenTree* retVal           = nullptr;
    GenTree* copyToRetBufNode = nullptr;

    if (origCall->HasRetBufArg())
    {
        JITDUMP("Transferring retbuf\n");
        GenTree* retBufArg = origCall->gtCallArgs->GetNode();

        assert(info.compRetBuffArg != BAD_VAR_NUM);

        assert(retBufArg->AsLclVar()->GetLclNum() == info.compRetBuffArg);

        // Caller return buffer argument retBufArg can point to GC heap while the dispatcher expects
        // the return value argument retValArg to point to the stack.
        // We use a temporary stack allocated return buffer to hold the value during the dispatcher call
        // and copy the value back to the caller return buffer after that.
        unsigned tmpRetBufNum =
            lvaNewTemp(origCall->GetRetLayout(), true DEBUGARG("substitute local for return buffer"));
        lvaSetVarAddrExposed(tmpRetBufNum);

        var_types tmpRetBufType = lvaGetDesc(tmpRetBufNum)->TypeGet();

        retValArg = gtNewLclVarAddrNode(tmpRetBufNum);

        var_types callerRetBufType = lvaGetDesc(info.compRetBuffArg)->TypeGet();

        GenTree* dstAddr = gtNewLclvNode(info.compRetBuffArg, callerRetBufType);
        GenTree* dst     = gtNewObjNode(origCall->GetRetLayout(), dstAddr);
        GenTree* src     = gtNewLclvNode(tmpRetBufNum, tmpRetBufType);

        copyToRetBufNode = gtNewAssignNode(dst, src);

        if (origCall->gtType != TYP_VOID)
        {
            retVal = gtClone(retBufArg);
        }
    }
    else if (!origCall->TypeIs(TYP_VOID))
    {
        JITDUMP("Creating a new temp for the return value\n");

        unsigned   newRetLclNum = lvaGrabTemp(false DEBUGARG("Return value for tail call dispatcher"));
        LclVarDsc* newRetLcl    = lvaGetDesc(newRetLclNum);

        if (varTypeIsStruct(origCall->GetType()))
        {
            lvaSetStruct(newRetLclNum, origCall->GetRetLayout(), false);
        }
        else
        {
            newRetLcl->SetType(origCall->GetType());
        }

        lvaSetVarAddrExposed(newRetLclNum);

        if (varTypeIsSmall(origCall->GetRetSigType()))
        {
            // Use a LCL_FLD to widen small int return, the local is already address exposed
            // so it's not worth adding an extra cast by relying on "normalize on load".
            retVal = gtNewLclFldNode(newRetLclNum, origCall->GetRetSigType(), 0);
        }
        else
        {
            retVal = gtNewLclvNode(newRetLclNum, newRetLcl->GetType());
        }

        if (varTypeIsStruct(origCall->GetType()) && (info.retDesc.GetRegCount() > 1))
        {
            retVal->gtFlags |= GTF_DONT_CSE;
        }

        retValArg = gtNewLclVarAddrNode(newRetLclNum);
    }
    else
    {
        JITDUMP("No return value so using null pointer as arg\n");
        retValArg = gtNewZeroConNode(TYP_I_IMPL);
    }

    callDispatcherNode->gtCallArgs = gtPrependNewCallArg(retValArg, callDispatcherNode->gtCallArgs);

    // Add callTarget
    callDispatcherNode->gtCallArgs =
        gtPrependNewCallArg(new (this, GT_FTN_ADDR) GenTreeFptrVal(TYP_I_IMPL, callTargetStubHnd),
                            callDispatcherNode->gtCallArgs);

    // Add the caller's return address slot.
    if (lvaRetAddrVar == BAD_VAR_NUM)
    {
        lvaRetAddrVar = lvaNewTemp(TYP_I_IMPL, false DEBUGARG("Return address"));
        lvaSetVarAddrExposed(lvaRetAddrVar);
    }

    GenTree* retAddrSlot           = gtNewLclVarAddrNode(lvaRetAddrVar);
    callDispatcherNode->gtCallArgs = gtPrependNewCallArg(retAddrSlot, callDispatcherNode->gtCallArgs);

    GenTree* finalTree = callDispatcherNode;

    if (copyToRetBufNode != nullptr)
    {
        finalTree = gtNewCommaNode(callDispatcherNode, copyToRetBufNode);
    }

    if (origCall->gtType == TYP_VOID)
    {
        return finalTree;
    }

    finalTree = gtNewCommaNode(finalTree, retVal, origCall->GetType());

    // The JIT seems to want to CSE this comma and messes up multi-reg ret
    // values in the process. Just avoid CSE'ing this tree entirely in that
    // case.
    if (origCall->HasMultiRegRetVal())
    {
        finalTree->gtFlags |= GTF_DONT_CSE;
    }

    return finalTree;
}

//------------------------------------------------------------------------
// getLookupTree: get a lookup tree
//
// Arguments:
//    pResolvedToken - resolved token of the call
//    pLookup - the lookup to get the tree for
//    handleFlags - flags to set on the result node
//    compileTimeHandle - compile-time handle corresponding to the lookup
//
// Return Value:
//    A node representing the lookup tree
//
GenTree* Compiler::getLookupTree(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                 CORINFO_LOOKUP*         pLookup,
                                 GenTreeFlags            handleFlags,
                                 void*                   compileTimeHandle)
{
    if (!pLookup->lookupKind.needsRuntimeLookup)
    {
        // No runtime lookup is required.
        // Access is direct or memory-indirect (of a fixed address) reference

        CORINFO_GENERIC_HANDLE handle       = nullptr;
        void*                  pIndirection = nullptr;
        assert(pLookup->constLookup.accessType != IAT_PPVALUE && pLookup->constLookup.accessType != IAT_RELPVALUE);

        if (pLookup->constLookup.accessType == IAT_VALUE)
        {
            handle = pLookup->constLookup.handle;
        }
        else if (pLookup->constLookup.accessType == IAT_PVALUE)
        {
            pIndirection = pLookup->constLookup.addr;
        }

        return gtNewIconEmbHndNode(handle, pIndirection, handleFlags, compileTimeHandle);
    }

    return getRuntimeLookupTree(pResolvedToken, pLookup, compileTimeHandle);
}

//------------------------------------------------------------------------
// getRuntimeLookupTree: get a tree for a runtime lookup
//
// Arguments:
//    pResolvedToken - resolved token of the call
//    pLookup - the lookup to get the tree for
//    compileTimeHandle - compile-time handle corresponding to the lookup
//
// Return Value:
//    A node representing the runtime lookup tree
//
GenTree* Compiler::getRuntimeLookupTree(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                        CORINFO_LOOKUP*         pLookup,
                                        void*                   compileTimeHandle)
{
    assert(!compIsForInlining());

    CORINFO_RUNTIME_LOOKUP* pRuntimeLookup = &pLookup->runtimeLookup;

    // If pRuntimeLookup->indirections is equal to CORINFO_USEHELPER, it specifies that a run-time helper should be
    // used; otherwise, it specifies the number of indirections via pRuntimeLookup->offsets array.
    if ((pRuntimeLookup->indirections == CORINFO_USEHELPER) || pRuntimeLookup->testForNull ||
        pRuntimeLookup->testForFixup)
    {
        // If the first condition is true, runtime lookup tree is available only via the run-time helper function.
        // TODO-CQ If the second or third condition is true, we are always using the slow path since we can't
        // introduce control flow at this point. See impRuntimeLookupToTree for the logic to avoid calling the helper.
        // The long-term solution is to introduce a new node representing a runtime lookup, create instances
        // of that node both in the importer and here, and expand the node in lower (introducing control flow if
        // necessary).
        return gtNewRuntimeLookupHelperCallNode(pRuntimeLookup,
                                                getRuntimeContextTree(pLookup->lookupKind.runtimeLookupKind),
                                                compileTimeHandle);
    }

    GenTree* result = getRuntimeContextTree(pLookup->lookupKind.runtimeLookupKind);

    ArrayStack<GenTree*> stmts(getAllocator(CMK_ArrayStack));

    auto cloneTree = [&](GenTree** use DEBUGARG(const char* reason)) -> GenTree* {
        GenTree* tree = *use;

        if ((tree->gtFlags & GTF_GLOB_EFFECT) == 0)
        {
            GenTree* clone = gtClone(tree, true);

            if (clone != nullptr)
            {
                return clone;
            }
        }

        assert(varTypeIsI(tree->GetType()));
        unsigned temp = lvaNewTemp(tree->GetType(), true DEBUGARG(reason));
        stmts.Push(gtNewAssignNode(gtNewLclvNode(temp, tree->GetType()), tree));
        *use = gtNewLclvNode(temp, tree->GetType());
        return gtNewLclvNode(temp, tree->GetType());
    };

    // Apply repeated indirections
    for (unsigned i = 0; i < pRuntimeLookup->indirections; i++)
    {
        GenTree* preInd = nullptr;
        if ((i == 1 && pRuntimeLookup->indirectFirstOffset) || (i == 2 && pRuntimeLookup->indirectSecondOffset))
        {
            preInd = cloneTree(&result DEBUGARG("getRuntimeLookupTree indirectOffset"));
        }

        if (i != 0)
        {
            result = gtNewOperNode(GT_IND, TYP_I_IMPL, result);
            result->gtFlags |= GTF_IND_NONFAULTING;
            result->gtFlags |= GTF_IND_INVARIANT;
        }

        if ((i == 1 && pRuntimeLookup->indirectFirstOffset) || (i == 2 && pRuntimeLookup->indirectSecondOffset))
        {
            result = gtNewOperNode(GT_ADD, TYP_I_IMPL, preInd, result);
        }

        if (pRuntimeLookup->offsets[i] != 0)
        {
            result = gtNewOperNode(GT_ADD, TYP_I_IMPL, result, gtNewIconNode(pRuntimeLookup->offsets[i], TYP_I_IMPL));
        }
    }

    assert(!pRuntimeLookup->testForNull);
    if (pRuntimeLookup->indirections > 0)
    {
        assert(!pRuntimeLookup->testForFixup);
        result = gtNewOperNode(GT_IND, TYP_I_IMPL, result);
        result->gtFlags |= GTF_IND_NONFAULTING;
    }

    // Produces GT_COMMA(stmt1, GT_COMMA(stmt2, ... GT_COMMA(stmtN, result)))

    while (!stmts.Empty())
    {
        result = gtNewCommaNode(stmts.Pop(), result);
    }

    DISPTREE(result);
    return result;
}

//------------------------------------------------------------------------
// getVirtMethodPointerTree: get a tree for a virtual method pointer
//
// Arguments:
//    thisPtr - tree representing `this` pointer
//    pResolvedToken - pointer to the resolved token of the method
//    pCallInfo - pointer to call info
//
// Return Value:
//    A node representing the virtual method pointer

GenTree* Compiler::getVirtMethodPointerTree(GenTree*                thisPtr,
                                            CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                            CORINFO_CALL_INFO*      pCallInfo)
{
    GenTree* exactTypeDesc   = getTokenHandleTree(pResolvedToken, true);
    GenTree* exactMethodDesc = getTokenHandleTree(pResolvedToken, false);

    GenTreeCall::Use* helpArgs = gtNewCallArgs(thisPtr, exactTypeDesc, exactMethodDesc);
    return gtNewHelperCallNode(CORINFO_HELP_VIRTUAL_FUNC_PTR, TYP_I_IMPL, helpArgs);
}

//------------------------------------------------------------------------
// getTokenHandleTree: get a handle tree for a token
//
// Arguments:
//    pResolvedToken - token to get a handle for
//    parent - whether parent should be imported
//
// Return Value:
//    A node representing the virtual method pointer

GenTree* Compiler::getTokenHandleTree(CORINFO_RESOLVED_TOKEN* pResolvedToken, bool parent)
{
    CORINFO_GENERICHANDLE_RESULT embedInfo;
    info.compCompHnd->embedGenericHandle(pResolvedToken, parent, &embedInfo);

    GenTree* result = getLookupTree(pResolvedToken, &embedInfo.lookup, gtTokenToIconFlags(pResolvedToken->token),
                                    embedInfo.compileTimeHandle);

    // If we have a result and it requires runtime lookup, wrap it in a runtime lookup node.
    if ((result != nullptr) && embedInfo.lookup.lookupKind.needsRuntimeLookup)
    {
        result = gtNewRuntimeLookup(embedInfo.compileTimeHandle, embedInfo.handleType, result);
    }

    return result;
}

/*****************************************************************************
 *
 *  Transform the given GT_CALL tree for tail call via JIT helper.
 */
void Compiler::fgMorphTailCallViaJitHelper(GenTreeCall* call)
{
    JITDUMP("fgMorphTailCallViaJitHelper (before):\n");
    DISPTREE(call);

    // The runtime requires that we perform a null check on the `this` argument before
    // tail calling  to a virtual dispatch stub. This requirement is a consequence of limitations
    // in the runtime's ability to map an AV to a NullReferenceException if
    // the AV occurs in a dispatch stub that has unmanaged caller.
    if (call->IsVirtualStub())
    {
        call->gtFlags |= GTF_CALL_NULLCHECK;
    }

    // For the helper-assisted tail calls, we need to push all the arguments
    // into a single list, and then add a few extra at the beginning or end.
    //
    // For x86, the tailcall helper is defined as:
    //
    //      JIT_TailCall(<function args>, int numberOfOldStackArgsWords, int numberOfNewStackArgsWords, int flags, void*
    //      callTarget)
    //
    // Note that the special arguments are on the stack, whereas the function arguments follow
    // the normal convention: there might be register arguments in ECX and EDX. The stack will
    // look like (highest address at the top):
    //      first normal stack argument
    //      ...
    //      last normal stack argument
    //      numberOfOldStackArgs
    //      numberOfNewStackArgs
    //      flags
    //      callTarget
    //
    // Each special arg is 4 bytes.
    //
    // 'flags' is a bitmask where:
    //      1 == restore callee-save registers (EDI,ESI,EBX). The JIT always saves all
    //          callee-saved registers for tailcall functions. Note that the helper assumes
    //          that the callee-saved registers live immediately below EBP, and must have been
    //          pushed in this order: EDI, ESI, EBX.
    //      2 == call target is a virtual stub dispatch.
    //
    // The x86 tail call helper lives in VM\i386\jithelp.asm. See that function for more details
    // on the custom calling convention.

    // Check for PInvoke call types that we don't handle in codegen yet.
    assert(!call->IsUnmanaged());
    assert(call->IsVirtual() || (call->gtCallType != CT_INDIRECT) || (call->gtCallCookie == nullptr));

    // Don't support tail calling helper methods
    assert(call->gtCallType != CT_HELPER);

    // We come this route only for tail prefixed calls that cannot be dispatched as
    // fast tail calls
    assert(!call->IsImplicitTailCall());

    // We want to use the following assert, but it can modify the IR in some cases, so we
    // can't do that in an assert.
    // assert(!fgCanFastTailCall(call, nullptr));

    // First move the 'this' pointer (if any) onto the regular arg list. We do this because
    // we are going to prepend special arguments onto the argument list (for non-x86 platforms),
    // and thus shift where the 'this' pointer will be passed to a later argument slot. In
    // addition, for all platforms, we are going to change the call into a helper call. Our code
    // generation code for handling calls to helpers does not handle 'this' pointers. So, when we
    // do this transformation, we must explicitly create a null 'this' pointer check, if required,
    // since special 'this' pointer handling will no longer kick in.
    //
    // Some call types, such as virtual vtable calls, require creating a call address expression
    // that involves the "this" pointer. Lowering will sometimes create an embedded statement
    // to create a temporary that is assigned to the "this" pointer expression, and then use
    // that temp to create the call address expression. This temp creation embedded statement
    // will occur immediately before the "this" pointer argument, and then will be used for both
    // the "this" pointer argument as well as the call address expression. In the normal ordering,
    // the embedded statement establishing the "this" pointer temp will execute before both uses
    // of the temp. However, for tail calls via a helper, we move the "this" pointer onto the
    // normal call argument list, and insert a placeholder which will hold the call address
    // expression. For non-x86, things are ok, because the order of execution of these is not
    // altered. However, for x86, the call address expression is inserted as the *last* argument
    // in the argument list, *after* the "this" pointer. It will be put on the stack, and be
    // evaluated first. To ensure we don't end up with out-of-order temp definition and use,
    // for those cases where call lowering creates an embedded form temp of "this", we will
    // create a temp here, early, that will later get morphed correctly.

    if (call->gtCallThisArg != nullptr)
    {
        GenTree* thisPtr    = nullptr;
        GenTree* objp       = call->gtCallThisArg->GetNode();
        call->gtCallThisArg = nullptr;

        if ((call->IsDelegateInvoke() || call->IsVirtualVtable()) && !objp->IsLocal())
        {
            // tmp = "this"
            unsigned lclNum = lvaNewTemp(objp->GetType(), true DEBUGARG("tail call thisptr"));
            GenTree* asg    = gtNewAssignNode(gtNewLclvNode(lclNum, objp->GetType()), objp);

            // COMMA(tmp = "this", tmp)
            var_types vt  = objp->TypeGet();
            GenTree*  tmp = gtNewLclvNode(lclNum, vt);
            thisPtr       = gtNewCommaNode(asg, tmp);

            objp = thisPtr;
        }

        if (call->NeedsNullCheck())
        {
            // clone "this" if "this" has no side effects.
            if ((thisPtr == nullptr) && !(objp->gtFlags & GTF_SIDE_EFFECT))
            {
                thisPtr = gtClone(objp, true);
            }

            var_types vt = objp->TypeGet();
            if (thisPtr == nullptr)
            {
                // create a temp if either "this" has side effects or "this" is too complex to clone.

                // tmp = "this"
                unsigned lclNum = lvaNewTemp(objp->GetType(), true DEBUGARG("tail call thisptr"));
                GenTree* asg    = gtNewAssignNode(gtNewLclvNode(lclNum, objp->GetType()), objp);

                // COMMA(tmp = "this", deref(tmp))
                GenTree* tmp       = gtNewLclvNode(lclNum, vt);
                GenTree* nullcheck = gtNewNullCheck(tmp, compCurBB);
                asg                = gtNewCommaNode(asg, nullcheck);
                thisPtr            = gtNewCommaNode(asg, gtNewLclvNode(lclNum, vt));
            }
            else
            {
                // thisPtr = COMMA(deref("this"), "this")
                GenTree* nullcheck = gtNewNullCheck(thisPtr, compCurBB);
                thisPtr            = gtNewCommaNode(nullcheck, gtClone(objp, true));
            }

            call->gtFlags &= ~GTF_CALL_NULLCHECK;
        }
        else
        {
            thisPtr = objp;
        }

        // TODO-Cleanup: we leave it as a virtual stub call to
        // use logic in `LowerVirtualStubCall`, clear GTF_CALL_VIRT_KIND_MASK here
        // and change `LowerCall` to recognize it as a direct call.

        // During rationalization tmp="this" and null check will
        // materialize as embedded stmts in right execution order.
        assert(thisPtr != nullptr);
        call->gtCallArgs = gtPrependNewCallArg(thisPtr, call->gtCallArgs);
    }

    // Find the end of the argument list. ppArg will point at the last pointer; setting *ppArg will
    // append to the list.
    GenTreeCall::Use** ppArg = &call->gtCallArgs;
    for (GenTreeCall::Use& use : call->Args())
    {
        ppArg = &use.NextRef();
    }
    assert(ppArg != nullptr);
    assert(*ppArg == nullptr);

    unsigned nOldStkArgsWords =
        (compArgSize - (codeGen->intRegState.rsCalleeRegArgCount * REGSIZE_BYTES)) / REGSIZE_BYTES;
    GenTree* arg3 = gtNewIconNode((ssize_t)nOldStkArgsWords, TYP_I_IMPL);
    *ppArg        = gtNewCallArgs(arg3); // numberOfOldStackArgs
    ppArg         = &((*ppArg)->NextRef());

    // Inject a placeholder for the count of outgoing stack arguments that the Lowering phase will generate.
    // The constant will be replaced.
    GenTree* arg2 = gtNewIconNode(9, TYP_I_IMPL);
    *ppArg        = gtNewCallArgs(arg2); // numberOfNewStackArgs
    ppArg         = &((*ppArg)->NextRef());

    // Inject a placeholder for the flags.
    // The constant will be replaced.
    GenTree* arg1 = gtNewIconNode(8, TYP_I_IMPL);
    *ppArg        = gtNewCallArgs(arg1);
    ppArg         = &((*ppArg)->NextRef());

    // Inject a placeholder for the real call target that the Lowering phase will generate.
    // The constant will be replaced.
    GenTree* arg0 = gtNewIconNode(7, TYP_I_IMPL);
    *ppArg        = gtNewCallArgs(arg0);

    // It is now a varargs tail call.
    call->gtCallMoreFlags |= GTF_CALL_M_VARARGS;
    call->gtFlags &= ~GTF_CALL_POP_ARGS;

    // The function is responsible for doing explicit null check when it is necessary.
    assert(!call->NeedsNullCheck());

    JITDUMP("fgMorphTailCallViaJitHelper (after):\n");
    DISPTREE(call);
}

//------------------------------------------------------------------------
// fgGetStubAddrArg: Return the virtual stub address for the given call.
//
// Notes:
//    the JIT must place the address of the stub used to load the call target,
//    the "stub indirection cell", in special call argument with special register.
//
// Arguments:
//    call - a call that needs virtual stub dispatching.
//
// Return Value:
//    addr tree with set resister requirements.
//
GenTree* Compiler::fgGetStubAddrArg(GenTreeCall* call)
{
    assert(call->IsVirtualStub());
    GenTree* stubAddrArg;
    if (call->gtCallType == CT_INDIRECT)
    {
        stubAddrArg = gtClone(call->gtCallAddr, true);
    }
    else
    {
        assert(call->gtCallMoreFlags & GTF_CALL_M_VIRTSTUB_REL_INDIRECT);
        ssize_t addr = ssize_t(call->gtStubCallStubAddr);
        stubAddrArg  = gtNewIconHandleNode(addr, GTF_ICON_FTN_ADDR);
#ifdef DEBUG
        stubAddrArg->AsIntCon()->gtTargetHandle = (size_t)call->gtCallMethHnd;
#endif
    }
    assert(stubAddrArg != nullptr);
    stubAddrArg->SetRegNum(virtualStubParamInfo->GetReg());
    return stubAddrArg;
}

//------------------------------------------------------------------------------
// fgMorphRecursiveFastTailCallIntoLoop : Transform a recursive fast tail call into a loop.
//
//
// Arguments:
//    block  - basic block ending with a recursive fast tail call
//    recursiveTailCall - recursive tail call to transform
//
// Notes:
//    The legality of the transformation is ensured by the checks in endsWithTailCallConvertibleToLoop.

void Compiler::fgMorphRecursiveFastTailCallIntoLoop(BasicBlock* block, GenTreeCall* recursiveTailCall)
{
    assert(recursiveTailCall->IsTailCallConvertibleToLoop());
    Statement* lastStmt = block->lastStmt();
    assert(recursiveTailCall == lastStmt->GetRootNode());

    // Transform recursive tail call into a loop.

    Statement* earlyArgInsertionPoint = lastStmt;
    IL_OFFSETX callILOffset           = lastStmt->GetILOffsetX();

    // Hoist arg setup statement for the 'this' argument.
    GenTreeCall::Use* thisArg = recursiveTailCall->gtCallThisArg;
    if ((thisArg != nullptr) && !thisArg->GetNode()->IsNothingNode() && !thisArg->GetNode()->IsArgPlaceHolderNode())
    {
        Statement* thisArgStmt = gtNewStmt(thisArg->GetNode(), callILOffset);
        fgInsertStmtBefore(block, earlyArgInsertionPoint, thisArgStmt);
    }

    // All arguments whose trees may involve caller parameter local variables need to be assigned to temps first;
    // then the temps need to be assigned to the method parameters. This is done so that the caller
    // parameters are not re-assigned before call arguments depending on them  are evaluated.
    // tmpAssignmentInsertionPoint and paramAssignmentInsertionPoint keep track of
    // where the next temp or parameter assignment should be inserted.

    // In the example below the first call argument (arg1 - 1) needs to be assigned to a temp first
    // while the second call argument (const 1) doesn't.
    // Basic block before tail recursion elimination:
    //  ***** BB04, stmt 1 (top level)
    //  [000037] ------------             *  stmtExpr  void  (top level) (IL 0x00A...0x013)
    //  [000033] --C - G------ - \--*  call      void   RecursiveMethod
    //  [000030] ------------ | / --*  const     int - 1
    //  [000031] ------------arg0 in rcx + --*  +int
    //  [000029] ------------ | \--*  lclVar    int    V00 arg1
    //  [000032] ------------arg1 in rdx    \--*  const     int    1
    //
    //
    //  Basic block after tail recursion elimination :
    //  ***** BB04, stmt 1 (top level)
    //  [000051] ------------             *  stmtExpr  void  (top level) (IL 0x00A... ? ? ? )
    //  [000030] ------------ | / --*  const     int - 1
    //  [000031] ------------ | / --*  +int
    //  [000029] ------------ | | \--*  lclVar    int    V00 arg1
    //  [000050] - A----------             \--* = int
    //  [000049] D------N----                \--*  lclVar    int    V02 tmp0
    //
    //  ***** BB04, stmt 2 (top level)
    //  [000055] ------------             *  stmtExpr  void  (top level) (IL 0x00A... ? ? ? )
    //  [000052] ------------ | / --*  lclVar    int    V02 tmp0
    //  [000054] - A----------             \--* = int
    //  [000053] D------N----                \--*  lclVar    int    V00 arg0

    //  ***** BB04, stmt 3 (top level)
    //  [000058] ------------             *  stmtExpr  void  (top level) (IL 0x00A... ? ? ? )
    //  [000032] ------------ | / --*  const     int    1
    //  [000057] - A----------             \--* = int
    //  [000056] D------N----                \--*  lclVar    int    V01 arg1

    Statement* tmpAssignmentInsertionPoint   = lastStmt;
    Statement* paramAssignmentInsertionPoint = lastStmt;

    // Process early args. They may contain both setup statements for late args and actual args.
    // Early args don't include 'this' arg. We need to account for that so that the call to gtArgEntryByArgNum
    // below has the correct second argument.
    int earlyArgIndex = (thisArg == nullptr) ? 0 : 1;
    for (GenTreeCall::Use& use : recursiveTailCall->Args())
    {
        GenTree* earlyArg = use.GetNode();
        if (!earlyArg->IsNothingNode() && !earlyArg->IsArgPlaceHolderNode())
        {
            if ((earlyArg->gtFlags & GTF_LATE_ARG) != 0)
            {
                // This is a setup node so we need to hoist it.
                Statement* earlyArgStmt = gtNewStmt(earlyArg, callILOffset);
                fgInsertStmtBefore(block, earlyArgInsertionPoint, earlyArgStmt);
            }
            else
            {
                // This is an actual argument that needs to be assigned to the corresponding caller parameter.
                fgArgTabEntry* curArgTabEntry = recursiveTailCall->GetArgInfoByArgNum(earlyArgIndex);
                Statement*     paramAssignStmt =
                    fgAssignRecursiveCallArgToCallerParam(earlyArg, curArgTabEntry, block, callILOffset,
                                                          tmpAssignmentInsertionPoint, paramAssignmentInsertionPoint);
                if ((tmpAssignmentInsertionPoint == lastStmt) && (paramAssignStmt != nullptr))
                {
                    // All temp assignments will happen before the first param assignment.
                    tmpAssignmentInsertionPoint = paramAssignStmt;
                }
            }
        }
        earlyArgIndex++;
    }

    // Process late args.
    for (GenTreeCall::Use& use : recursiveTailCall->LateArgs())
    {
        // A late argument is an actual argument that needs to be assigned to the corresponding caller's parameter.
        GenTree*       lateArg        = use.GetNode();
        fgArgTabEntry* curArgTabEntry = recursiveTailCall->GetArgInfoByLateArgUse(&use);
        Statement*     paramAssignStmt =
            fgAssignRecursiveCallArgToCallerParam(lateArg, curArgTabEntry, block, callILOffset,
                                                  tmpAssignmentInsertionPoint, paramAssignmentInsertionPoint);

        if ((tmpAssignmentInsertionPoint == lastStmt) && (paramAssignStmt != nullptr))
        {
            // All temp assignments will happen before the first param assignment.
            tmpAssignmentInsertionPoint = paramAssignStmt;
        }
    }

    // If the method has starg.s 0 or ldarga.s 0 a special local (lvaArg0Var) is created so that
    // compThisArg stays immutable. Normally it's assigned in fgFirstBBScratch block. Since that
    // block won't be in the loop (it's assumed to have no predecessors), we need to update the special local here.
    if (!info.compIsStatic && (lvaArg0Var != info.compThisArg))
    {
        var_types  thisType           = lvaTable[info.compThisArg].TypeGet();
        GenTree*   arg0               = gtNewLclvNode(lvaArg0Var, thisType);
        GenTree*   arg0Assignment     = gtNewAssignNode(arg0, gtNewLclvNode(info.compThisArg, thisType));
        Statement* arg0AssignmentStmt = gtNewStmt(arg0Assignment, callILOffset);
        fgInsertStmtBefore(block, paramAssignmentInsertionPoint, arg0AssignmentStmt);
    }

    // If compInitMem is set, we may need to zero-initialize some locals. Normally it's done in the prolog
    // but this loop can't include the prolog. Since we don't have liveness information, we insert zero-initialization
    // for all non-parameter IL locals as well as temp structs with GC fields.
    // Liveness phase will remove unnecessary initializations.
    if (info.compInitMem || compSuppressedZeroInit)
    {
        unsigned   varNum;
        LclVarDsc* varDsc;
        for (varNum = 0, varDsc = lvaTable; varNum < lvaCount; varNum++, varDsc++)
        {
#if FEATURE_FIXED_OUT_ARGS
            if (varNum == lvaOutgoingArgSpaceVar)
            {
                continue;
            }
#endif // FEATURE_FIXED_OUT_ARGS
            if (!varDsc->lvIsParam)
            {
                var_types lclType            = varDsc->TypeGet();
                bool      isUserLocal        = (varNum < info.compLocalsCount);
                bool      structWithGCFields = ((lclType == TYP_STRUCT) && varDsc->GetLayout()->HasGCPtr());
                bool      hadSuppressedInit  = varDsc->lvSuppressedZeroInit;
                if ((info.compInitMem && (isUserLocal || structWithGCFields)) || hadSuppressedInit)
                {
                    GenTree* lcl  = gtNewLclvNode(varNum, lclType);
                    GenTree* init = nullptr;
                    if (varTypeIsStruct(lclType))
                    {
                        init = gtNewAssignNode(lcl, gtNewIconNode(0));
                        init = fgMorphInitStruct(init->AsOp());
                    }
                    else
                    {
                        GenTree* zero = gtNewZeroConNode(genActualType(lclType));
                        init          = gtNewAssignNode(lcl, zero);
                    }
                    Statement* initStmt = gtNewStmt(init, callILOffset);
                    fgInsertStmtBefore(block, lastStmt, initStmt);
                }
            }
        }
    }

    // Remove the call
    fgRemoveStmt(block, lastStmt);

    // Set the loop edge.
    if (opts.IsOSR())
    {
        // Todo: this may not look like a viable loop header.
        // Might need the moral equivalent of a scratch BB.
        block->bbJumpDest = fgEntryBB;
    }
    else
    {
        // Ensure we have a scratch block and then target the next
        // block.  Loop detection needs to see a pred out of the loop,
        // so mark the scratch block BBF_DONT_REMOVE to prevent empty
        // block removal on it.
        fgEnsureFirstBBisScratch();
        fgFirstBB->bbFlags |= BBF_DONT_REMOVE;
        block->bbJumpDest = fgFirstBB->bbNext;
    }

    // Finish hooking things up.
    block->bbJumpKind = BBJ_ALWAYS;
    fgAddRefPred(block->bbJumpDest, block);
    block->bbFlags &= ~BBF_HAS_JMP;
}

//------------------------------------------------------------------------------
// fgAssignRecursiveCallArgToCallerParam : Assign argument to a recursive call to the corresponding caller parameter.
//
//
// Arguments:
//    arg  -  argument to assign
//    argTabEntry  -  argument table entry corresponding to arg
//    block  --- basic block the call is in
//    callILOffset  -  IL offset of the call
//    tmpAssignmentInsertionPoint  -  tree before which temp assignment should be inserted (if necessary)
//    paramAssignmentInsertionPoint  -  tree before which parameter assignment should be inserted
//
// Return Value:
//    parameter assignment statement if one was inserted; nullptr otherwise.

Statement* Compiler::fgAssignRecursiveCallArgToCallerParam(GenTree*       arg,
                                                           fgArgTabEntry* argTabEntry,
                                                           BasicBlock*    block,
                                                           IL_OFFSETX     callILOffset,
                                                           Statement*     tmpAssignmentInsertionPoint,
                                                           Statement*     paramAssignmentInsertionPoint)
{
    // Call arguments should be assigned to temps first and then the temps should be assigned to parameters because
    // some argument trees may reference parameters directly.

    GenTree* argInTemp             = nullptr;
    unsigned originalArgNum        = argTabEntry->GetArgNum();
    bool     needToAssignParameter = true;

    // TODO-CQ: enable calls with struct arguments passed in registers.
    noway_assert(!varTypeIsStruct(arg->TypeGet()));

    if (argTabEntry->HasTemp() || arg->IsCnsIntOrI() || arg->IsCnsFltOrDbl())
    {
        // The argument is already assigned to a temp or is a const.
        argInTemp = arg;
    }
    else if (arg->OperGet() == GT_LCL_VAR)
    {
        unsigned   lclNum = arg->AsLclVar()->GetLclNum();
        LclVarDsc* varDsc = &lvaTable[lclNum];
        if (!varDsc->lvIsParam)
        {
            // The argument is a non-parameter local so it doesn't need to be assigned to a temp.
            argInTemp = arg;
        }
        else if (lclNum == originalArgNum)
        {
            // The argument is the same parameter local that we were about to assign so
            // we can skip the assignment.
            needToAssignParameter = false;
        }
    }

    // TODO: We don't need temp assignments if we can prove that the argument tree doesn't involve
    // any caller parameters. Some common cases are handled above but we may be able to eliminate
    // more temp assignments.

    Statement* paramAssignStmt = nullptr;
    if (needToAssignParameter)
    {
        if (argInTemp == nullptr)
        {
            // The argument is not assigned to a temp. We need to create a new temp and insert an assignment.
            // TODO: we can avoid a temp assignment if we can prove that the argument tree
            // doesn't involve any caller parameters.
            unsigned tmpNum = lvaNewTemp(arg->GetType(), true DEBUGARG("arg temp"));

            GenTree*   tempDest      = gtNewLclvNode(tmpNum, arg->GetType());
            GenTree*   tmpAssignNode = gtNewAssignNode(tempDest, arg);
            Statement* tmpAssignStmt = gtNewStmt(tmpAssignNode, callILOffset);
            fgInsertStmtBefore(block, tmpAssignmentInsertionPoint, tmpAssignStmt);
            argInTemp = gtNewLclvNode(tmpNum, arg->GetType());
        }

        // Now assign the temp to the parameter.
        LclVarDsc* paramDsc = lvaTable + originalArgNum;
        assert(paramDsc->lvIsParam);
        GenTree* paramDest       = gtNewLclvNode(originalArgNum, paramDsc->lvType);
        GenTree* paramAssignNode = gtNewAssignNode(paramDest, argInTemp);
        paramAssignStmt          = gtNewStmt(paramAssignNode, callILOffset);

        fgInsertStmtBefore(block, paramAssignmentInsertionPoint, paramAssignStmt);
    }
    return paramAssignStmt;
}

/*****************************************************************************
 *
 *  Transform the given GT_CALL tree for code generation.
 */

GenTree* Compiler::fgMorphCall(GenTreeCall* call)
{
    if (call->CanTailCall())
    {
        GenTree* newNode = fgMorphPotentialTailCall(call);
        if (newNode != nullptr)
        {
            return newNode;
        }

        assert(!call->CanTailCall());

#if FEATURE_MULTIREG_RET
        if (fgGlobalMorph && call->HasMultiRegRetVal() && varTypeIsStruct(call->TypeGet()))
        {
            // The tail call has been rejected so we must finish the work deferred
            // by impCanonicalizeMultiRegCall for multi-reg-returning calls and transform
            //     ret call
            // into
            //     temp = call
            //     ret temp

            // Force re-evaluating the argInfo as the return argument has changed.
            call->ResetArgInfo();

            unsigned tmpNum =
                lvaNewTemp(call->GetRetLayout(), false DEBUGARG("multireg return call temp (rejected tail call)"));

            GenTree* dst = gtNewLclvNode(tmpNum, lvaGetDesc(tmpNum)->GetType());
            GenTree* asg = fgMorphTree(gtNewAssignNode(dst, call));

            Statement* asgStmt = gtNewStmt(asg, compCurStmt->GetILOffsetX());
            fgInsertStmtBefore(compCurBB, compCurStmt, asgStmt);

#ifdef DEBUG
            if (verbose)
            {
                printf("\nInserting assignment of a multi-reg call result to a temp:\n");
                gtDispStmt(asgStmt);
            }
#endif

            compCurBB->bbFlags |= BBF_HAS_CALL;

            GenTree* result = gtNewLclvNode(tmpNum, dst->GetType());
            result->gtFlags |= GTF_DONT_CSE;
            INDEBUG(result->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)

            return result;
        }
#endif
    }

    if ((call->gtCallMoreFlags & GTF_CALL_M_SPECIAL_INTRINSIC) == 0 &&
        (call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_VIRTUAL_FUNC_PTR)
#ifdef FEATURE_READYTORUN_COMPILER
         || call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_READYTORUN_VIRTUAL_FUNC_PTR)
#endif
             ) &&
        (call == fgMorphStmt->GetRootNode()))
    {
        // This is call to CORINFO_HELP_VIRTUAL_FUNC_PTR with ignored result.
        // Transform it into a null check.

        GenTree* thisPtr = call->gtCallArgs->GetNode();

        GenTree* nullCheck = gtNewNullCheck(thisPtr, compCurBB);

        return fgMorphTree(nullCheck);
    }

    noway_assert(call->gtOper == GT_CALL);

    //
    // Only count calls once (only in the global morph phase)
    //
    if (fgGlobalMorph)
    {
        if (call->gtCallType == CT_INDIRECT)
        {
            optCallCount++;
            optIndirectCallCount++;
        }
        else if (call->gtCallType == CT_USER_FUNC)
        {
            optCallCount++;
            if (call->IsVirtual())
            {
                optIndirectCallCount++;
            }
        }
    }

    // Couldn't inline - remember that this BB contains method calls

    // Mark the block as a GC safe point for the call if possible.
    // In the event the call indicates the block isn't a GC safe point
    // and the call is unmanaged with a GC transition suppression request
    // then insert a GC poll.
    CLANG_FORMAT_COMMENT_ANCHOR;

    if (IsGcSafePoint(call))
    {
        compCurBB->bbFlags |= BBF_GC_SAFE_POINT;
    }

    // Regardless of the state of the basic block with respect to GC safe point,
    // we will always insert a GC Poll for scenarios involving a suppressed GC
    // transition. Only mark the block for GC Poll insertion on the first morph.
    if (fgGlobalMorph && call->IsUnmanaged() && call->IsSuppressGCTransition())
    {
        compCurBB->bbFlags |= (BBF_HAS_SUPPRESSGC_CALL | BBF_GC_SAFE_POINT);
        optMethodFlags |= OMF_NEEDS_GCPOLLS;
    }

    // Morph Type.op_Equality, Type.op_Inequality, and Enum.HasFlag
    //
    // We need to do these before the arguments are morphed
    if ((call->gtCallMoreFlags & GTF_CALL_M_SPECIAL_INTRINSIC))
    {
        // See if this is foldable
        GenTree* optTree = gtFoldExprCall(call);

        // If we optimized, morph the result
        if (optTree != call)
        {
            return fgMorphTree(optTree);
        }
    }

    compCurBB->bbFlags |= BBF_HAS_CALL; // This block has a call

    /* Process the "normal" argument list */
    call = fgMorphArgs(call);
    noway_assert(call->gtOper == GT_CALL);

    // Should we expand this virtual method call target early here?
    //
    if (call->IsExpandedEarly() && call->IsVirtualVtable())
    {
        // We only expand the Vtable Call target once in the global morph phase
        if (call->gtControlExpr == nullptr)
        {
            assert(fgGlobalMorph);
            call->gtControlExpr = fgExpandVirtualVtableCallTarget(call);
        }
        // We always have to morph or re-morph the control expr
        //
        call->gtControlExpr = fgMorphTree(call->gtControlExpr);

        // Propogate any gtFlags into the call
        call->gtFlags |= call->gtControlExpr->gtFlags;
    }

    // Morph stelem.ref helper call to store a null value, into a store into an array without the helper.
    // This needs to be done after the arguments are morphed to ensure constant propagation has already taken place.
    if (opts.OptimizationEnabled() && (call->gtCallType == CT_HELPER) &&
        (call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_ARRADDR_ST)))
    {
        GenTree* value = call->GetArgNodeByArgNum(2);
        if (value->IsIntegralConst(0))
        {
            assert(value->OperGet() == GT_CNS_INT);

            GenTree* arr   = call->GetArgNodeByArgNum(0);
            GenTree* index = call->GetArgNodeByArgNum(1);

            // Either or both of the array and index arguments may have been spilled to temps by `fgMorphArgs`. Copy
            // the spill trees as well if necessary.
            GenTreeOp* argSetup = nullptr;
            for (GenTreeCall::Use& use : call->Args())
            {
                GenTree* const arg = use.GetNode();
                if (!arg->OperIs(GT_ASG))
                {
                    continue;
                }

                assert(arg != arr);
                assert(arg != index);

                arg->gtFlags &= ~GTF_LATE_ARG;

                GenTree* op1 = argSetup;
                if (op1 == nullptr)
                {
                    op1 = gtNewNothingNode();
                    INDEBUG(op1->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)
                }

                argSetup = gtNewCommaNode(op1, arg);
                INDEBUG(argSetup->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)
            }

#ifdef DEBUG
            auto resetMorphedFlag = [](GenTree** slot, fgWalkData* data) -> fgWalkResult {
                (*slot)->gtDebugFlags &= ~GTF_DEBUG_NODE_MORPHED;
                return WALK_CONTINUE;
            };

            fgWalkTreePost(&arr, resetMorphedFlag);
            fgWalkTreePost(&index, resetMorphedFlag);
            fgWalkTreePost(&value, resetMorphedFlag);
#endif // DEBUG

            GenTree* const nullCheckedArr = impCheckForNullPointer(arr);
            GenTree* const arrIndexNode   = gtNewArrayIndex(TYP_REF, nullCheckedArr, index);
            GenTree* const arrStore       = gtNewAssignNode(arrIndexNode, value);
            arrStore->gtFlags |= GTF_ASG;

            GenTree* result = fgMorphTree(arrStore);
            if (argSetup != nullptr)
            {
                result = gtNewCommaNode(argSetup, result);
                INDEBUG(result->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)
            }

            return result;
        }
    }

    if (call->IsNoReturn())
    {
        //
        // If we know that the call does not return then we can set fgRemoveRestOfBlock
        // to remove all subsequent statements and change the call's basic block to BBJ_THROW.
        // As a result the compiler won't need to preserve live registers across the call.
        //
        // This isn't need for tail calls as there shouldn't be any code after the call anyway.
        // Besides, the tail call code is part of the epilog and converting the block to
        // BBJ_THROW would result in the tail call being dropped as the epilog is generated
        // only for BBJ_RETURN blocks.
        //

        if (!call->IsTailCall())
        {
            fgRemoveRestOfBlock = true;
        }
    }

    return call;
}

/*****************************************************************************
 *
 *  Expand and return the call target address for a VirtualCall
 *  The code here should match that generated by LowerVirtualVtableCall
 */

GenTree* Compiler::fgExpandVirtualVtableCallTarget(GenTreeCall* call)
{
    GenTree* result;

    JITDUMP("Expanding virtual call target for %d.%s:\n", call->gtTreeID, GenTree::OpName(call->gtOper));

    noway_assert(call->gtCallType == CT_USER_FUNC);

    // get a reference to the thisPtr being passed
    CallArgInfo* thisArgTabEntry = call->GetArgInfoByArgNum(0);
    GenTree*     thisPtr         = thisArgTabEntry->GetNode();

    // fgMorphArgs must enforce this invariant by creating a temp
    //
    assert(thisPtr->OperIsLocal());

    // Make a copy of the thisPtr by cloning
    //
    thisPtr = gtClone(thisPtr, true);

    noway_assert(thisPtr != nullptr);

    // Get hold of the vtable offset
    unsigned vtabOffsOfIndirection;
    unsigned vtabOffsAfterIndirection;
    bool     isRelative;
    info.compCompHnd->getMethodVTableOffset(call->gtCallMethHnd, &vtabOffsOfIndirection, &vtabOffsAfterIndirection,
                                            &isRelative);

    // Dereference the this pointer to obtain the method table, it is called vtab below
    GenTree* vtab;
    assert(VPTR_OFFS == 0); // We have to add this value to the thisPtr to get the methodTable
    vtab = gtNewOperNode(GT_IND, TYP_I_IMPL, thisPtr);
    vtab->gtFlags |= GTF_IND_INVARIANT;

    // Get the appropriate vtable chunk
    if (vtabOffsOfIndirection != CORINFO_VIRTUALCALL_NO_CHUNK)
    {
        // Note this isRelative code path is currently never executed
        // as the VM doesn't ever return:  isRelative == true
        //
        if (isRelative)
        {
            // MethodTable offset is a relative pointer.
            //
            // Additional temporary variable is used to store virtual table pointer.
            // Address of method is obtained by the next computations:
            //
            // Save relative offset to tmp (vtab is virtual table pointer, vtabOffsOfIndirection is offset of
            // vtable-1st-level-indirection):
            // tmp = vtab
            //
            // Save address of method to result (vtabOffsAfterIndirection is offset of vtable-2nd-level-indirection):
            // result = [tmp + vtabOffsOfIndirection + vtabOffsAfterIndirection + [tmp + vtabOffsOfIndirection]]
            //
            //
            // When isRelative is true we need to setup two temporary variables
            // var1 = vtab
            // var2 = var1 + vtabOffsOfIndirection + vtabOffsAfterIndirection + [var1 + vtabOffsOfIndirection]
            // result = [var2] + var2
            //
            unsigned varNum1 = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("var1 - vtab"));
            unsigned varNum2 = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("var2 - relative"));
            GenTree* asgVar1 = gtNewAssignNode(gtNewLclvNode(varNum1, TYP_I_IMPL), vtab);

            // [tmp + vtabOffsOfIndirection]
            GenTree* tmpTree1 = gtNewOperNode(GT_ADD, TYP_I_IMPL, gtNewLclvNode(varNum1, TYP_I_IMPL),
                                              gtNewIconNode(vtabOffsOfIndirection, TYP_I_IMPL));
            tmpTree1 = gtNewOperNode(GT_IND, TYP_I_IMPL, tmpTree1, false);
            tmpTree1->gtFlags |= GTF_IND_NONFAULTING | GTF_IND_INVARIANT;

            // var1 + vtabOffsOfIndirection + vtabOffsAfterIndirection
            GenTree* tmpTree2 =
                gtNewOperNode(GT_ADD, TYP_I_IMPL, gtNewLclvNode(varNum1, TYP_I_IMPL),
                              gtNewIconNode(vtabOffsOfIndirection + vtabOffsAfterIndirection, TYP_I_IMPL));

            // var1 + vtabOffsOfIndirection + vtabOffsAfterIndirection + [var1 + vtabOffsOfIndirection]
            tmpTree2         = gtNewOperNode(GT_ADD, TYP_I_IMPL, tmpTree2, tmpTree1);
            GenTree* asgVar2 = gtNewAssignNode(gtNewLclvNode(varNum2, TYP_I_IMPL), tmpTree2);

            // This last indirection is not invariant, but is non-faulting
            result = gtNewOperNode(GT_IND, TYP_I_IMPL, gtNewLclvNode(varNum2, TYP_I_IMPL), false); // [var2]
            result->gtFlags |= GTF_IND_NONFAULTING;

            result = gtNewOperNode(GT_ADD, TYP_I_IMPL, result, gtNewLclvNode(varNum2, TYP_I_IMPL)); // [var2] + var2

            result = gtNewCommaNode(asgVar1, gtNewCommaNode(asgVar2, result));
        }
        else
        {
            // result = [vtab + vtabOffsOfIndirection]
            result = gtNewOperNode(GT_ADD, TYP_I_IMPL, vtab, gtNewIconNode(vtabOffsOfIndirection, TYP_I_IMPL));
            result = gtNewOperNode(GT_IND, TYP_I_IMPL, result, false);
            result->gtFlags |= GTF_IND_NONFAULTING | GTF_IND_INVARIANT;
        }
    }
    else
    {
        result = vtab;
        assert(!isRelative);
    }

    if (!isRelative)
    {
        // Load the function address
        // result = [result + vtabOffsAfterIndirection]
        result = gtNewOperNode(GT_ADD, TYP_I_IMPL, result, gtNewIconNode(vtabOffsAfterIndirection, TYP_INT));
        // This last indirection is not invariant, but is non-faulting
        result = gtNewOperNode(GT_IND, TYP_I_IMPL, result, false);
        result->gtFlags |= GTF_IND_NONFAULTING;
    }

    return result;
}

/*****************************************************************************
 *
 *  Transform the given GTK_CONST tree for code generation.
 */

GenTree* Compiler::fgMorphConst(GenTree* tree)
{
    assert(tree->OperKind() & GTK_CONST);

    /* Clear any exception flags or other unnecessary flags
     * that may have been set before folding this node to a constant */

    tree->gtFlags &= ~(GTF_ALL_EFFECT | GTF_REVERSE_OPS);

    if (tree->OperGet() != GT_CNS_STR)
    {
        return tree;
    }

    // TODO-CQ: Do this for compCurBB->isRunRarely(). Doing that currently will
    // guarantee slow performance for that block. Instead cache the return value
    // of CORINFO_HELP_STRCNS and go to cache first giving reasonable perf.

    bool useLazyStrCns = false;
    if (compCurBB->bbJumpKind == BBJ_THROW)
    {
        useLazyStrCns = true;
    }
    else if (fgGlobalMorph && compCurStmt->GetRootNode()->IsCall())
    {
        // Quick check: if the root node of the current statement happens to be a noreturn call.
        GenTreeCall* call = compCurStmt->GetRootNode()->AsCall();
        useLazyStrCns     = call->IsNoReturn() || fgIsThrow(call);
    }

    if (useLazyStrCns)
    {
        CorInfoHelpFunc helper = info.compCompHnd->getLazyStringLiteralHelper(tree->AsStrCon()->gtScpHnd);
        if (helper != CORINFO_HELP_UNDEF)
        {
            // For un-important blocks, we want to construct the string lazily

            GenTreeCall::Use* args;
            if (helper == CORINFO_HELP_STRCNS_CURRENT_MODULE)
            {
                args = gtNewCallArgs(gtNewIconNode(RidFromToken(tree->AsStrCon()->gtSconCPX), TYP_INT));
            }
            else
            {
                args = gtNewCallArgs(gtNewIconNode(RidFromToken(tree->AsStrCon()->gtSconCPX), TYP_INT),
                                     gtNewIconEmbScpHndNode(tree->AsStrCon()->gtScpHnd));
            }

            tree = gtNewHelperCallNode(helper, TYP_REF, args);
            return fgMorphTree(tree);
        }
    }

    assert(tree->AsStrCon()->gtScpHnd == info.compScopeHnd || !IsUninitialized(tree->AsStrCon()->gtScpHnd));

    LPVOID         pValue;
    InfoAccessType iat =
        info.compCompHnd->constructStringLiteral(tree->AsStrCon()->gtScpHnd, tree->AsStrCon()->gtSconCPX, &pValue);

    tree = gtNewStringLiteralNode(iat, pValue);

    return fgMorphTree(tree);
}

GenTree* Compiler::fgMorphLeaf(GenTree* tree)
{
    assert(tree->OperKind() & GTK_LEAF);

    if (tree->gtOper == GT_LCL_VAR)
    {
        const bool forceRemorph = false;
        return fgMorphLocalVar(tree, forceRemorph);
    }
    else if (tree->gtOper == GT_LCL_FLD)
    {
        if (lvaGetDesc(tree->AsLclFld())->lvAddrExposed)
        {
            tree->gtFlags |= GTF_GLOB_REF;
        }
    }
    else if (tree->gtOper == GT_FTN_ADDR)
    {
        CORINFO_CONST_LOOKUP addrInfo;

#ifdef FEATURE_READYTORUN_COMPILER
        if (tree->AsFptrVal()->gtEntryPoint.addr != nullptr)
        {
            addrInfo = tree->AsFptrVal()->gtEntryPoint;
        }
        else
#endif
        {
            info.compCompHnd->getFunctionFixedEntryPoint(tree->AsFptrVal()->gtFptrMethod, &addrInfo);
        }

        GenTree* indNode = nullptr;
        switch (addrInfo.accessType)
        {
            case IAT_PPVALUE:
                indNode = gtNewIndOfIconHandleNode(TYP_I_IMPL, (size_t)addrInfo.handle, GTF_ICON_CONST_PTR, true);

                // Add the second indirection
                indNode = gtNewOperNode(GT_IND, TYP_I_IMPL, indNode);
                // This indirection won't cause an exception.
                indNode->gtFlags |= GTF_IND_NONFAULTING;
                // This indirection also is invariant.
                indNode->gtFlags |= GTF_IND_INVARIANT;
                break;

            case IAT_PVALUE:
                indNode = gtNewIndOfIconHandleNode(TYP_I_IMPL, (size_t)addrInfo.handle, GTF_ICON_FTN_ADDR, true);
                break;

            case IAT_VALUE:
                // Refer to gtNewIconHandleNode() as the template for constructing a constant handle
                //
                tree->SetOper(GT_CNS_INT);
                tree->AsIntConCommon()->SetIconValue(ssize_t(addrInfo.handle));
                tree->gtFlags |= GTF_ICON_FTN_ADDR;
                break;

            default:
                noway_assert(!"Unknown addrInfo.accessType");
        }

        if (indNode != nullptr)
        {
            DEBUG_DESTROY_NODE(tree);
            tree = fgMorphTree(indNode);
        }
    }

    return tree;
}

//------------------------------------------------------------------------
// fgMorphInitStruct: Morph a block initialization assignment tree.
//
// Arguments:
//    tree - A GT_ASG tree that performs block initialization
//
// Return Value:
//    If the destination is a promoted struct local variable then we will try to
//    perform a field by field assignment for each of the promoted struct fields.
//    This is not always possible (e.g. if the struct has holes and custom layout).
//
//    If the destination is a non-struct local variable then fgMorphInitStruct
//    attempts to convert block initialization to simpler scalar or SIMD assignment.
//
//    Otherwise the orginal GT_ASG tree is returned unmodified (always correct but
//    least desirable because it prevents enregistration and/or blocks independent
//    struct promotion).
//
// Assumptions:
//    GT_ASG's children have already been morphed.
//
GenTree* Compiler::fgMorphInitStruct(GenTreeOp* asg)
{
    JITDUMPTREE(asg, "\nfgMorphInitStruct (before):\n");

    assert(asg->OperIs(GT_ASG));

    GenTree* dest = asg->GetOp(0);
    GenTree* src  = asg->GetOp(1);

    assert(varTypeIsStruct(dest->GetType()));
    assert(src->OperIs(GT_INIT_VAL) || src->IsIntegralConst(0));

    if (dest->OperIs(GT_COMMA))
    {
        dest = fgMorphStructComma(dest);
        asg->SetOp(0, dest);
        asg->SetType(dest->GetType());

        JITDUMPTREE(asg, "fgMorphInitStruct (after fgMorphStructComma):\n");
    }

    unsigned             destSize     = 0;
    GenTreeLclVarCommon* destLclNode  = nullptr;
    unsigned             destLclNum   = BAD_VAR_NUM;
    LclVarDsc*           destLclVar   = nullptr;
    unsigned             destLclOffs  = 0;
    FieldSeqNode*        destFieldSeq = nullptr;

    if (dest->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        destLclNode = dest->AsLclVarCommon();
        destLclNum  = destLclNode->GetLclNum();
        destLclVar  = lvaGetDesc(destLclNum);

        if (dest->OperIs(GT_LCL_VAR))
        {
            if (destLclNode->TypeIs(TYP_STRUCT))
            {
                destSize = destLclVar->GetLayout()->GetSize();
            }
            else
            {
                destSize = genTypeSize(destLclVar->GetType());
            }
        }
        else
        {
            destSize =
                dest->TypeIs(TYP_STRUCT) ? dest->AsLclFld()->GetLayout(this)->GetSize() : genTypeSize(dest->GetType());
            destLclOffs  = dest->AsLclFld()->GetLclOffs();
            destFieldSeq = dest->AsLclFld()->GetFieldSeq();
        }
    }
    else
    {
        if (dest->OperIs(GT_IND))
        {
            assert(varTypeIsSIMD(dest->GetType()));

            destSize = varTypeSize(dest->GetType());
        }
        else
        {
            destSize = dest->AsObj()->GetLayout()->GetSize();
        }

        INDEBUG(GenTreeLclVarCommon* lclNode = dest->AsIndir()->GetAddr()->IsLocalAddrExpr();)
        assert((lclNode == nullptr) || lvaGetDesc(lclNode)->IsAddressExposed());
    }

#if LOCAL_ASSERTION_PROP
    if (optLocalAssertionProp && (destLclNum != BAD_VAR_NUM) && (optAssertionCount > 0))
    {
        fgKillDependentAssertions(destLclNum DEBUGARG(asg));
    }
#endif

    GenTree* initVal = src->OperIs(GT_INIT_VAL) ? src->AsUnOp()->GetOp(0) : src;

    if ((destLclVar != nullptr) && (destSize != 0) && (destLclVar->GetType() != TYP_BLK))
    {
        unsigned destLclVarSize = destLclVar->GetSize();

        if (destLclVar->lvPromoted && (destLclOffs == 0) && (destSize == destLclVarSize))
        {
            assert(varTypeIsStruct(destLclVar->GetType()));

            GenTree* promotedTree = fgMorphPromoteLocalInitStruct(destLclVar, initVal);

            if (promotedTree != nullptr)
            {
                promotedTree->gtFlags |= (asg->gtFlags & GTF_LATE_ARG);
                INDEBUG(promotedTree->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)
                JITDUMPTREE(promotedTree, "fgMorphInitStruct (after promotion):\n");
                return promotedTree;
            }
        }

        if (initVal->OperIs(GT_CNS_INT))
        {
            // TODO-MIKE-Cleanup/CQ Attempting to convert block init into scalar/SIMD init
            // results in all sorts of diffs. One may expect that not blocking enregistration
            // due to the use block init would be an improvement but it turns out that there
            // are all sorts of issues in the backend that produce diffs and CQ issues:
            //   - `byte_location = 0;` generates
            //         mov byte ptr [rsi], 0
            //     but block init produces
            //         xor eax, eax
            //         mov byte ptr [rsi], al
            //     Block init is worse, unless a register happens to already contain 0,
            //     then scalar init is worse due to the extra immediate.
            //   - `float_location = 0;` generates
            //         xorps xmm0, xmm0
            //         movss [rsi], xmm0
            //     but block init generates
            //         xor eax, eax
            //         mov [rsi], eax
            //     which is smaller. Either version can be better depending on the availabily
            //     of a zero in an integer or float register.
            //   - Vector3 block init generates
            //         xor eax, eax
            //         mov [rsi], rax
            //         mov [rsi+8], eax
            //     but SIMD init generates
            //         xorps xmm0, xmm0
            //         movsd [rsi], xmm0
            //         pshufd xmm1, xmm0, 2
            //         movss [rsi], xmm1
            //     Removing the extra suffle should be easy but then we hit again the float/int
            //     constant issue - what's better depends on what kind of zero register happens
            //     to be available.
            //   - Integer 0 and REF/BYREF 0 aren't the same thing for LSRA and thus block init
            //     of a GC pointer location tends to generate better code by reusing a 0 register.
            //   - On 32 bit targets there are also differences between long scalar init and
            //     block init. Scalar init usually produces better code but not always it seems.
            //
            //     So to sum it up - this is pretty much restricted to INT now to minimize diffs.

            GenTreeFlags destFlags = dest->gtFlags & GTF_COLON_COND;

            var_types initType     = TYP_UNDEF;
            var_types initBaseType = TYP_UNDEF;

            if ((destFieldSeq != nullptr) && destFieldSeq->GetTail()->IsField())
            {
                CORINFO_CLASS_HANDLE fieldClassHandle;
                var_types            fieldBaseType = TYP_UNDEF;
                var_types            fieldType     = JITtype2varType(
                    info.compCompHnd->getFieldType(destFieldSeq->GetTail()->GetFieldHandle(), &fieldClassHandle));

                assert(!varTypeIsSIMD(fieldType));

                if (fieldType == TYP_STRUCT)
                {
                    fieldType = typGetStructType(fieldClassHandle, &fieldBaseType);
                }

                if ((destSize == genTypeSize(fieldType)) && !varTypeIsSmall(fieldType) && !varTypeIsSIMD(fieldType) &&
                    !varTypeIsFloating(fieldType) && !varTypeIsGC(fieldType)
#ifndef TARGET_64BIT
                    && !varTypeIsLong(fieldType)
#endif
                        )
                {
                    initType     = fieldType;
                    initBaseType = fieldBaseType;

                    destLclNode->ChangeOper(GT_LCL_FLD);
                    destLclNode->AsLclFld()->SetLclOffs(destLclOffs);
                    destLclNode->AsLclFld()->SetFieldSeq(destFieldSeq);
                }
            }

            if ((initType == TYP_UNDEF) && (initVal->IsIntegralConst(0) || (destLclVar->GetType() != TYP_STRUCT)))
            {
                if ((destLclOffs == 0) && (destSize == destLclVarSize) && !varTypeIsFloating(destLclVar->GetType())
#ifndef TARGET_64BIT
                    && !varTypeIsLong(destLclVar->GetType())
#endif
                        )
                {
                    initType = destLclVar->GetType();

                    if (varTypeIsSIMD(initType))
                    {
                        initBaseType = destLclVar->GetLayout()->GetElementType();
                    }

                    destLclNode->ChangeOper(GT_LCL_VAR);
                }
            }

            if (initType == TYP_UNDEF)
            {
                // TODO-MIKE-Review: The backend was changed in master to no longer support zeroing SIMD
                // typed locals by assigning a CNS_INT(0) so we have to change this to be a SIMD init.
                // Still, zeroing a Vector2/3 in memory using GPRs produces smaller code so perhaps we
                // need to do something else - if it's a LCL_FLD (or a LCL_VAR if the local is already
                // DNER) we can change its type to TYP_STRUCT to get the normal block initialization.
                // But then retyping can cause other issues (VN, CSE etc.) so perhaps we should actually
                // convert it here to SIMD init and then deal with it in lowering/codegen.

                if (!dest->OperIs(GT_BLK) && varTypeIsSIMD(dest->GetType()))
                {
                    initType     = dest->GetType();
                    initBaseType = TYP_FLOAT;
                }
            }

            if (initType != TYP_UNDEF)
            {
                destLclNode->SetType(initType);
                destLclNode->AsLclVarCommon()->SetLclNum(destLclNum);

                destFlags |= GTF_DONT_CSE | GTF_VAR_DEF | (destLclVar->lvAddrExposed ? GTF_GLOB_REF : GTF_EMPTY);

                if (destLclNode->OperIs(GT_LCL_FLD))
                {
                    lvaSetVarDoNotEnregister(destLclNum DEBUGARG(DNER_LocalField));

                    if ((destLclNode->AsLclFld()->GetLclOffs() > 0) ||
                        (genTypeSize(destLclNode->GetType()) < destLclVarSize))
                    {
                        destFlags |= GTF_VAR_USEASG;
                    }
                }

                destLclNode->gtFlags = destFlags;

                if (initType == TYP_STRUCT)
                {
                    lvaSetVarDoNotEnregister(destLclNum DEBUGARG(DNER_BlockOp));
                }
                else
                {
                    initVal =
                        fgMorphInitStructConstant(initVal->AsIntCon(), initType,
                                                  destLclNode->OperIs(GT_LCL_VAR) && destLclVar->lvNormalizeOnStore(),
                                                  initBaseType);
                }

                asg->SetType(initType);
                asg->SetOp(0, destLclNode);
                asg->SetOp(1, initVal);
                asg->gtFlags &= ~GTF_ALL_EFFECT;
                asg->gtFlags |= GTF_ASG | ((asg->GetOp(0)->gtFlags | asg->GetOp(1)->gtFlags) & GTF_ALL_EFFECT);

                JITDUMPTREE(asg, "fgMorphInitStruct (after converting to scalar init):\n");

                return asg;
            }
        }
    }

    asg->gtFlags &= ~GTF_ALL_EFFECT;
    asg->gtFlags |= GTF_ASG | ((asg->GetOp(0)->gtFlags | asg->GetOp(1)->gtFlags) & GTF_ALL_EFFECT);

    if (destLclVar != nullptr)
    {
        lvaSetVarDoNotEnregister(destLclNum DEBUGARG(DNER_BlockOp));
    }

    JITDUMPTREE(asg, "fgMorphInitStruct (after):\n");

    return asg;
}

//------------------------------------------------------------------------
// fgMorphInitStructConstant: Morph a block initialization constant node
//    to be suitable for non-block initialization.
//
// Arguments:
//    initVal - The GT_CNS_INT node
//    type - The type of the destination value
//    extendToActualType - extend small int initialization patterns to int
//    simdBaseType - The SIMD base type for SIMD destinations
//
// Return Value:
//    The original node, changed to a suitable typed constant node or a new
//    SIMD node for SIMD destinations.
//
GenTree* Compiler::fgMorphInitStructConstant(GenTreeIntCon* initVal,
                                             var_types      type,
                                             bool           extendToActualType,
                                             var_types      simdBaseType)
{
    assert(type != TYP_STRUCT);

    var_types initPatternType;

    if (varTypeIsSIMD(type))
    {
        if (varTypeIsLong(simdBaseType))
        {
            // It is not useful to produce a 64 bit init pattern, especially on 32 bit targets.
            initPatternType = TYP_INT;
        }
        else
        {
#ifdef TARGET_ARM64
            // TODO-MIKE-ARM64-CQ Codegen doesn't properly recognize zero if the base type is float.
            initPatternType = TYP_INT;
#else
            // TODO-MIKE-Review: This may be unnecessary when VPBROADCAST is available
            initPatternType = varActualType(simdBaseType);
#endif
        }
    }
    else
    {
        initPatternType = type;
    }

    int64_t initPattern = (initVal->AsIntCon()->GetValue() & 0xFF) * 0x0101010101010101LL;

    if (initPatternType == TYP_FLOAT)
    {
        float floatPattern;
        memcpy(&floatPattern, &initPattern, 4);
        initVal->ChangeOperConst(GT_CNS_DBL);
        initVal->SetType(TYP_FLOAT);
        initVal->AsDblCon()->SetValue(floatPattern);
    }
    else if (initPatternType == TYP_DOUBLE)
    {
        double doublePatern;
        memcpy(&doublePatern, &initPattern, 8);
        initVal->ChangeOperConst(GT_CNS_DBL);
        initVal->SetType(TYP_DOUBLE);
        initVal->AsDblCon()->SetValue(doublePatern);
    }
#ifndef TARGET_64BIT
    else if (varTypeIsLong(initPatternType))
    {
        initVal->ChangeOperConst(GT_CNS_LNG);
        initVal->SetType(TYP_LONG);
        initVal->AsLngCon()->SetValue(initPattern);
    }
#endif
    else
    {
        if (genTypeSize(initPatternType) <= 4)
        {
            // Keep only as many bits as are needed to avoid creating "large" constants.
            initPattern &= (int64_t(1) << (genTypeSize(initPatternType) * 8)) - 1;

            if (extendToActualType)
            {
                if (initPatternType == TYP_BYTE)
                {
                    initPattern = static_cast<int8_t>(initPattern);
                }
                else if (initPatternType == TYP_SHORT)
                {
                    initPattern = static_cast<int16_t>(initPattern);
                }
            }
        }

#ifdef TARGET_64BIT
        initVal->AsIntCon()->SetValue(initPattern);
#else
        initVal->AsIntCon()->SetValue(static_cast<int32_t>(initPattern));
#endif
        initVal->SetType(genActualType(initPatternType));
    }

#ifdef FEATURE_SIMD
    if (varTypeIsSIMD(type))
    {
        if (initPattern == 0)
        {
            return gtNewZeroSimdHWIntrinsicNode(type, initPatternType);
        }
        else
        {
            return gtNewSimdHWIntrinsicNode(type, GetCreateSimdHWIntrinsic(type), initPatternType, varTypeSize(type),
                                            initVal);
        }
    }
#endif

    return initVal;
}

//------------------------------------------------------------------------
// fgMorphPromoteLocalInitStruct: Attempts to promote a local struct init tree
// to a tree of promoted field initialization assignments.
//
// Arguments:
//    destLclVar - The destination LclVar
//    initVal - The initialization value
//
// Return Value:
//    A tree that performs field by field initialization of the destination
//    struct variable if various conditions are met, nullptr otherwise.
//
// Notes:
//    This transforms a single block initialization assignment like:
//
//    *  ASG       struct (init)
//    +--*  BLK(12)   struct
//    |  \--*  ADDR      long
//    |     \--*  LCL_VAR   struct(P) V02 loc0
//    |     \--*    int    V02.a (offs=0x00) -> V06 tmp3
//    |     \--*    ubyte  V02.c (offs=0x04) -> V07 tmp4
//    |     \--*    float  V02.d (offs=0x08) -> V08 tmp5
//    \--*  INIT_VAL  int
//       \--*  CNS_INT   int    42
//
//    into a COMMA tree of assignments that initialize each promoted struct
//    field:
//
//    *  COMMA     void
//    +--*  COMMA     void
//    |  +--*  ASG       int
//    |  |  +--*  LCL_VAR   int    V06 tmp3
//    |  |  \--*  CNS_INT   int    0x2A2A2A2A
//    |  \--*  ASG       ubyte
//    |     +--*  LCL_VAR   ubyte  V07 tmp4
//    |     \--*  CNS_INT   int    42
//    \--*  ASG       float
//       +--*  LCL_VAR   float  V08 tmp5
//       \--*  CNS_DBL   float  1.5113661732714390e-13
//
GenTree* Compiler::fgMorphPromoteLocalInitStruct(LclVarDsc* destLclVar, GenTree* initVal)
{
    assert(varTypeIsStruct(destLclVar->GetType()));
    assert(destLclVar->lvPromoted);

    if (destLclVar->lvDoNotEnregister && (destLclVar->GetPromotedFieldCount() > 1))
    {
        JITDUMP(" dest is already DNER and has more than one field.\n");
        return nullptr;
    }

    if (destLclVar->lvAddrExposed && destLclVar->lvContainsHoles)
    {
        JITDUMP(" dest is address exposed and contains holes.\n");
        return nullptr;
    }

    if (destLclVar->lvCustomLayout && destLclVar->lvContainsHoles)
    {
        JITDUMP(" dest has custom layout and contains holes.\n");
        return nullptr;
    }

    if (!initVal->OperIs(GT_CNS_INT))
    {
        JITDUMP(" source is not constant.\n");
        return nullptr;
    }

    if ((initVal->AsIntCon()->GetValue() & 0xFF) != 0)
    {
        for (unsigned i = 0; i < destLclVar->GetPromotedFieldCount(); ++i)
        {
            unsigned   destFieldLclNum = destLclVar->GetPromotedFieldLclNum(i);
            LclVarDsc* destFieldLclVar = lvaGetDesc(destFieldLclNum);

            if (varTypeIsGC(destFieldLclVar->GetType()))
            {
                JITDUMP(" dest contains GC and fields and source constant is not 0.\n");
                return nullptr;
            }
        }
    }

    JITDUMP(" using field by field initialization.\n");

    GenTree* tree = nullptr;

    for (unsigned i = 0; i < destLclVar->GetPromotedFieldCount(); ++i)
    {
        unsigned   destFieldLclNum = destLclVar->GetPromotedFieldLclNum(i);
        LclVarDsc* destFieldLcl    = lvaGetDesc(destFieldLclNum);

        GenTree* destField = gtNewLclvNode(destFieldLclNum, destFieldLcl->GetType());
        destField->gtFlags |= destFieldLcl->lvAddrExposed ? GTF_GLOB_REF : GTF_EMPTY;

        var_types type     = destFieldLcl->GetType();
        var_types baseType = varTypeIsSIMD(type) ? destFieldLcl->GetLayout()->GetElementType() : TYP_UNDEF;

        GenTree* asg =
            gtNewAssignNode(destField, fgMorphInitStructConstant(gtNewIconNode(initVal->AsIntCon()->GetValue()), type,
                                                                 destFieldLcl->lvNormalizeOnStore(), baseType));

#if LOCAL_ASSERTION_PROP
        if (optLocalAssertionProp)
        {
            optAssertionGen(asg);
        }
#endif

        if (tree != nullptr)
        {
            tree = gtNewCommaNode(tree, asg);
        }
        else
        {
            tree = asg;
        }
    }

    return tree;
}

GenTree* Compiler::fgMorphStructComma(GenTree* tree)
{
    assert(tree->OperIs(GT_COMMA) && varTypeIsStruct(tree->GetType()));

    // In order to CSE and value number array index expressions and bounds checks,
    // the commas in which they are contained need to match.
    // The pattern is that the COMMA should be the address expression.
    // Therefore, we insert a GT_ADDR just above the node, and wrap it in an obj or ind.
    // TODO-1stClassStructs: Consider whether this can be improved.
    // Example:
    //   before: [3] comma struct <- [2] comma struct <- [1] LCL_VAR struct
    //   after: [3] comma byref <- [2] comma byref <- [4] addr byref <- [1] LCL_VAR struct

    ArrayStack<GenTreeOp*> commas(getAllocator(CMK_ArrayStack));
    for (GenTree* comma = tree; comma->OperIs(GT_COMMA); comma = comma->AsOp()->GetOp(1))
    {
        assert(comma->GetType() == tree->GetType());
        commas.Push(comma->AsOp());
    }

    GenTree* effectiveVal = commas.Top()->GetOp(1);

    // TODO-MIKE-Review: Weird, effectiveVal could be a LCL_VAR node so we end up taking
    // the address of a local without DNERing/address exposing it. Some code below tried
    // to DNER the local but it was simply checking for ADDR(LCL_VAR) but that is burried
    // under COMMAs.
    //
    // Not clear what should be done in this case, if DNERing the local is sufficient or
    // it has to be address exposed as well. Various "is local address" utility functions
    // don't seem to check for COMMA chains (e.g. IsLocalAddrExpr) so it may be that this
    // kind of local access may be missed by liveness, SSA etc.
    //
    // But then DNER/address exposed are bad because this sometimes happens with SIMD
    // copies of locals...
    //
    // To make things worse, this transform can occur during CSE, after value numbering
    // and nothing updates the value numbers of the COMMA chain, they'll still have the
    // VN of the original effective value instead of its address.
    //
    // PS: Ultimately this logic exists mainly to deal with struct COMMAs generated by
    // INDEX morphing. Once INDEX is replaced by INDEX_ADDR it this will no longer be
    // needed. There's no need to change a ASG(x, COMMA(..., LCL_VAR)) tree that CSE
    // may generate into ASG(x, OBJ(COMMA(..., LCL_VAR_ADDR)).

    GenTree* effectiveValAddr = gtNewAddrNode(effectiveVal);
    INDEBUG(effectiveValAddr->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)
    commas.Top()->SetOp(1, effectiveValAddr);
    GenTreeFlags sideEffects = tree->GetSideEffects();

    while (!commas.Empty())
    {
        GenTreeOp* comma = commas.Pop();
        comma->SetType(TYP_BYREF);
        comma->SetSideEffects(comma->GetOp(0)->GetSideEffects() | comma->GetOp(1)->GetSideEffects());
    }

    GenTree* indir;

    if (effectiveVal->OperIs(GT_IND))
    {
        indir = gtNewOperNode(GT_IND, effectiveVal->GetType(), tree);
    }
    else
    {
        indir = gtNewObjNode(typGetStructLayout(effectiveVal), tree);
    }

    indir->SetSideEffects(sideEffects);
    INDEBUG(indir->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)
    return indir;
}

GenTree* Compiler::fgMorphStructAssignment(GenTreeOp* asg)
{
    assert(asg->OperIs(GT_ASG));
    assert(varTypeIsStruct(asg->GetOp(0)->GetType()));

    if (asg->GetOp(0)->OperIs(GT_BLK, GT_DYN_BLK))
    {
        return fgMorphBlockAssignment(asg);
    }

    if (asg->GetOp(1)->OperIs(GT_INIT_VAL, GT_CNS_INT))
    {
        return fgMorphInitStruct(asg);
    }
    else
    {
        return fgMorphCopyStruct(asg);
    }
}

#ifdef FEATURE_SIMD

GenTreeOp* Compiler::fgMorphPromoteSimdAssignmentSrc(GenTreeOp* asg, unsigned srcLclNum)
{
    LclVarDsc* srcLcl = lvaGetDesc(srcLclNum);
    assert(varTypeIsSIMD(srcLcl->GetType()));
    // Only Vector2/3/4 are promoted.
    assert(lvaGetDesc(srcLcl->GetPromotedFieldLclNum(0))->TypeIs(TYP_FLOAT));

    var_types dstType = asg->GetOp(0)->GetType();
#ifdef TARGET_XARCH
    NamedIntrinsic create = NI_Vector128_Create;
    unsigned       numOps = 4;
#elif defined(TARGET_ARM64)
    NamedIntrinsic create  = dstType == TYP_SIMD8 ? NI_Vector64_Create : NI_Vector128_Create;
    unsigned       numOps  = dstType == TYP_SIMD8 ? 2 : 4;
#else
#error Unsupported platform
#endif

    unsigned srcFieldCount = srcLcl->GetPromotedFieldCount();
    GenTree* ops[4];

    for (unsigned i = 0; i < numOps; i++)
    {
        if (i < srcFieldCount)
        {
            ops[i] = gtNewLclvNode(srcLcl->GetPromotedFieldLclNum(i), TYP_FLOAT);
        }
        else
        {
            ops[i] = gtNewDconNode(0, TYP_FLOAT);
        }
    }

    GenTree* src = gtNewSimdHWIntrinsicNode(dstType, create, TYP_FLOAT, numOps * 4, numOps, ops);

    asg->SetOp(1, src);
    asg->SetSideEffects(GTF_ASG | asg->GetOp(0)->GetSideEffects() | src->GetSideEffects());

    JITDUMPTREE(asg, "fgMorphCopyStruct (after SIMD source promotion):\n\n");

    return asg;
}

GenTreeOp* Compiler::fgMorphPromoteSimdAssignmentDst(GenTreeOp* asg, unsigned dstLclNum)
{
    LclVarDsc* dstLcl = lvaGetDesc(dstLclNum);
    assert(varTypeIsSIMD(dstLcl->GetType()));
    // Only Vector2/3/4 are promoted.
    assert(lvaGetDesc(dstLcl->GetPromotedFieldLclNum(0))->TypeIs(TYP_FLOAT));

    GenTree* src         = asg->GetOp(1);
    bool     srcIsZero   = false;
    bool     srcIsCreate = false;

    if (GenTreeHWIntrinsic* hwi = src->IsHWIntrinsic())
    {
        switch (hwi->GetIntrinsic())
        {
#ifdef TARGET_ARM64
            case NI_Vector64_get_Zero:
#endif
            case NI_Vector128_get_Zero:
                srcIsZero = true;
                break;

#ifdef TARGET_ARM64
            case NI_Vector64_Create:
#endif
            case NI_Vector128_Create:
                // TODO-MIKE-CQ: Promote broadcast create.
                srcIsCreate = !hwi->IsUnary();

                // We can use Create's operands directly only if they don't interfere with the field
                // assignments we're going to generate. Otherwise we'll treat Create as any other
                // intrinsic - store it into a temp.
                // TODO-MIKE-CQ: It would be better to add a temp for each Create operand, packing and
                // unpacking SIMD values is rather expensive.
                for (unsigned i = 0; i < hwi->GetNumOps() && srcIsCreate; i++)
                {
                    GenTree* op = hwi->GetOp(i);

                    if (op->OperIs(GT_LCL_VAR))
                    {
                        LclVarDsc* lcl = lvaGetDesc(op->AsLclVar());

                        if (lcl->IsPromotedField() && (lcl->GetPromotedFieldParentLclNum() == dstLclNum))
                        {
                            srcIsCreate = false;
                        }
                    }
                    else if (!op->IsDblCon())
                    {
                        // TODO-MIKE-CQ: This is overly conservative, we need to check if the op tree contains
                        // any references to the destination local, including its promoted fields. Basically
                        // something like gtHasRef but that also checks for promoted fields.
                        srcIsCreate = false;
                    }
                }
                break;

#ifdef TARGET_XARCH
            case NI_SSE2_ShiftRightLogical128BitLane:
                // TODO-MIKE-Review: Hmm, ARM64 doesn't zero out the upper Vector3 element?
                // TODO-MIKE-CQ: It would be better to insert zeroes instead of shifting...
                if (dstLcl->GetPromotedFieldCount() < 4)
                {
                    unsigned expectedShiftImm = (4 - dstLcl->GetPromotedFieldCount()) * 4;

                    if (hwi->GetOp(1)->IsIntegralConst(expectedShiftImm))
                    {
                        if (GenTreeHWIntrinsic* shl = hwi->GetOp(0)->IsHWIntrinsic())
                        {
                            if ((shl->GetIntrinsic() == NI_SSE2_ShiftLeftLogical128BitLane) &&
                                shl->GetOp(1)->IsIntegralConst(expectedShiftImm))
                            {
                                src = shl->GetOp(0);
                            }
                        }
                    }
                }
                break;
#endif
            default:
                break;
        }
    }

    GenTreeOp* comma = nullptr;

    if (!srcIsZero && !srcIsCreate && !src->OperIs(GT_LCL_VAR))
    {
        unsigned tempLclNum = lvaNewTemp(src, true DEBUGARG("promoted SIMD copy temp"));

        dstLcl = lvaGetDesc(dstLclNum);
        comma  = gtNewAssignNode(gtNewLclvNode(tempLclNum, src->GetType()), src);
        src    = gtNewLclvNode(tempLclNum, src->GetType());
    }

    for (unsigned i = 0; i < dstLcl->GetPromotedFieldCount(); i++)
    {
        unsigned fieldIndex  = dstLcl->GetPromotedFieldCount() - 1 - i;
        unsigned fieldLclNum = dstLcl->GetPromotedFieldLclNum(fieldIndex);

        GenTree* fieldSrc;

        if (srcIsCreate && (i < src->AsHWIntrinsic()->GetNumOps()))
        {
            fieldSrc = src->AsHWIntrinsic()->GetOp(fieldIndex);
            assert(fieldSrc->TypeIs(TYP_FLOAT));
        }
        else if (srcIsCreate || srcIsZero)
        {
            fieldSrc = gtNewDconNode(0, TYP_FLOAT);
        }
        else
        {
            src      = gtNewLclvNode(src->AsLclVar()->GetLclNum(), src->GetType());
            fieldSrc = gtNewSimdGetElementNode(src->GetType(), TYP_FLOAT, src, gtNewIconNode(fieldIndex));
        }

        GenTree*   fieldDst = gtNewLclvNode(fieldLclNum, TYP_FLOAT);
        GenTreeOp* fieldAsg = gtNewAssignNode(fieldDst, fieldSrc);

#if LOCAL_ASSERTION_PROP
        if (optLocalAssertionProp)
        {
            optAssertionGen(fieldAsg);
        }
#endif

        if (comma == nullptr)
        {
            comma = fieldAsg;
        }
        else
        {
            comma = gtNewCommaNode(comma, fieldAsg);
        }
    }

    comma->gtFlags |= (asg->gtFlags & GTF_LATE_ARG);

    INDEBUG(comma->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)
    JITDUMPTREE(comma, "fgMorphCopyStruct (after SIMD destination promotion):\n\n");

    return comma;
}

#endif // FEATURE_SIMD

GenTree* Compiler::fgMorphBlockAssignment(GenTreeOp* asg)
{
    assert(asg->OperIs(GT_ASG) && asg->TypeIs(TYP_STRUCT));
    assert((asg->gtFlags & GTF_LATE_ARG) == 0);

    GenTree* dst = asg->GetOp(0);
    GenTree* src = asg->GetOp(1);

    INDEBUG(GenTreeLclVarCommon* lclNode = dst->AsIndir()->GetAddr()->IsLocalAddrExpr();)
    assert((lclNode == nullptr) || lvaGetDesc(lclNode)->IsAddressExposed());

    if (src->IsIndir())
    {
        assert(src->TypeIs(TYP_STRUCT));
        INDEBUG(lclNode = src->AsIndir()->GetAddr()->IsLocalAddrExpr();)
        assert((lclNode == nullptr) || lvaGetDesc(lclNode)->IsAddressExposed());
    }

    if (dst->OperIs(GT_DYN_BLK))
    {
        assert(src->OperIs(GT_IND, GT_INIT_VAL) || src->IsIntegralConst(0));

        if (GenTreeIntCon* constSize = dst->AsDynBlk()->GetSize()->IsIntCon())
        {
            ClassLayout* layout = typGetBlkLayout(constSize->GetUInt32Value());

            dst->ChangeOper(GT_BLK);
            dst->AsBlk()->SetLayout(layout);

            if (src->OperIs(GT_IND))
            {
                src->ChangeOper(GT_BLK);
                src->AsBlk()->SetLayout(layout);
            }
        }
    }

    if (dst->OperIs(GT_BLK))
    {
        if (src->OperIs(GT_BLK))
        {
            assert(src->AsBlk()->Size() == dst->AsBlk()->Size());
        }
        else
        {
            assert(src->OperIs(GT_INIT_VAL) || src->IsIntegralConst(0));
        }

        assert(dst->AsBlk()->GetLayout()->IsBlockLayout());

        if (dst->AsBlk()->GetLayout()->GetSize() == 0)
        {
            GenTree* nop = gtNewNothingNode();
            INDEBUG(nop->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
            return nop;
        }
    }

    return asg;
}

//------------------------------------------------------------------------
// fgMorphCopyStruct: Morph a struct copy.
//
// Arguments:
//    asg - a struct copy (i.e. an assignment with a struct typed destination).
//
// Return Value:
//    The original assignment or a tree of assignments that performs field by
//    field copying of promoted structs to avoid dependent promotion.
//
// Assumptions:
//    The source and destination have already been morphed.
//
// Notes:
//    If we leave it as a struct copy we will call lvaSetVarDoNotEnregister() on both Source() and Dest().
//    When performing a field by field assignment we can have one of Source() or Dest treated as a blob of bytes
//    and in such cases we will call lvaSetVarDoNotEnregister() on the one treated as a blob of bytes.
//    if the Source() or Dest() is a a struct that has a "CustomLayout" and "ConstainsHoles" then we
//    can not use a field by field assignment and must leave the orginal struct copy unmodified.
//
GenTree* Compiler::fgMorphCopyStruct(GenTreeOp* asg)
{
    JITDUMPTREE(asg, "\nfgMorphCopyBlock: (before)\n");

    assert(asg->OperIs(GT_ASG));

    GenTree* dest = asg->GetOp(0);
    GenTree* src  = asg->GetOp(1);

    assert(varTypeIsStruct(dest->GetType()));
    assert(dest->TypeIs(TYP_STRUCT) ? src->TypeIs(TYP_STRUCT) : varTypeIsSIMD(src->GetType()));
    assert(!src->OperIs(GT_INIT_VAL, GT_CNS_INT));

    if (GenTreeCall* call = src->IsCall())
    {
#if FEATURE_MULTIREG_RET
        if (call->HasMultiRegRetVal())
        {
            assert(dest->OperIs(GT_LCL_VAR));
            JITDUMP(" not morphing a multireg call return\n");
            return asg;
        }
#endif

#ifdef WINDOWS_AMD64_ABI
        if (src->TypeIs(TYP_SIMD8) && (call->GetRegType(0) == TYP_LONG))
        {
            src->SetType(TYP_LONG);
            src = gtNewBitCastNode(TYP_SIMD8, src);
            asg->SetOp(1, src);
        }
#endif
    }

    if (dest->OperIs(GT_COMMA))
    {
        dest = fgMorphStructComma(dest);
        asg->SetOp(0, dest);
    }

    if (src->OperIs(GT_COMMA) && src->TypeIs(TYP_STRUCT))
    {
        src = fgMorphStructComma(src);
        asg->SetOp(1, src);
    }

    JITDUMPTREE(asg, "fgMorphCopyStruct: (after fgMorphStructComma)\n");

    unsigned             destSize     = 0;
    GenTreeLclVarCommon* destLclNode  = nullptr;
    unsigned             destLclNum   = BAD_VAR_NUM;
    LclVarDsc*           destLclVar   = nullptr;
    unsigned             destLclOffs  = 0;
    FieldSeqNode*        destFieldSeq = nullptr;
    ClassLayout*         destLayout   = nullptr;
    bool                 destPromote  = false;

    if (dest->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        destLclNode = dest->AsLclVarCommon();
        destLclNum  = destLclNode->GetLclNum();
        destLclVar  = lvaGetDesc(destLclNum);

        if (dest->OperIs(GT_LCL_VAR))
        {
            if (dest->TypeIs(TYP_STRUCT))
            {
                destLayout = destLclVar->GetLayout();
                destSize   = destLayout->GetSize();
            }
            else
            {
                destSize = varTypeSize(destLclVar->GetType());
            }
        }
        else
        {
            if (dest->TypeIs(TYP_STRUCT))
            {
                destLayout = dest->AsLclFld()->GetLayout(this);
                destSize   = destLayout->GetSize();
            }
            else
            {
                destSize = varTypeSize(dest->GetType());
            }

            destLclOffs  = dest->AsLclFld()->GetLclOffs();
            destFieldSeq = dest->AsLclFld()->GetFieldSeq();
        }
    }
    else
    {
        if (dest->OperIs(GT_IND))
        {
            assert(varTypeIsSIMD(dest->GetType()));

            destSize = varTypeSize(dest->GetType());
        }
        else if (dest->OperIs(GT_OBJ))
        {
            destLayout = dest->AsObj()->GetLayout();
            destSize   = destLayout->GetSize();
        }

        noway_assert(dest->AsIndir()->GetAddr()->TypeIs(TYP_BYREF, TYP_I_IMPL));

        INDEBUG(GenTreeLclVarCommon* lclNode = dest->AsIndir()->GetAddr()->IsLocalAddrExpr();)
        assert((lclNode == nullptr) || lvaGetDesc(lclNode)->IsAddressExposed());
    }

#if LOCAL_ASSERTION_PROP
    if (optLocalAssertionProp && (destLclNum != BAD_VAR_NUM) && (optAssertionCount > 0))
    {
        fgKillDependentAssertions(destLclNum DEBUGARG(asg));
    }
#endif

    GenTreeLclVarCommon* srcLclNode  = nullptr;
    unsigned             srcLclNum   = BAD_VAR_NUM;
    LclVarDsc*           srcLclVar   = nullptr;
    unsigned             srcLclOffs  = 0;
    FieldSeqNode*        srcFieldSeq = nullptr;
    bool                 srcPromote  = false;

    if (src->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        srcLclNode = src->AsLclVarCommon();
        srcLclNum  = srcLclNode->GetLclNum();
        srcLclVar  = lvaGetDesc(srcLclNum);

        if (src->OperIs(GT_LCL_FLD))
        {
            srcLclOffs  = src->AsLclFld()->GetLclOffs();
            srcFieldSeq = src->AsLclFld()->GetFieldSeq();
        }
    }
    else if (src->OperIs(GT_IND, GT_OBJ))
    {
        INDEBUG(GenTreeLclVarCommon* lclNode = src->AsIndir()->GetAddr()->IsLocalAddrExpr();)
        assert((lclNode == nullptr) || lvaGetDesc(lclNode)->IsAddressExposed());

        // TODO-MIKE-Cleanup: This is only needed due to stupid FIELD morphing...

        if (src->OperIs(GT_IND) && src->TypeIs(TYP_STRUCT))
        {
            src->ChangeOper(GT_OBJ);
            src->AsObj()->SetLayout(destLayout);
            src->AsObj()->SetKind(StructStoreKind::Invalid);
        }
    }
    else
    {
        // For SIMD copies the source can be any SIMD typed tree or a CALL.
        assert(src->OperIs(GT_CALL) || varTypeIsSIMD(src->GetType()));
    }

    // Check to see if we are doing a copy to/from the same local block.
    // If so, morph it to a nop.
    if ((destLclVar != nullptr) && (srcLclVar == destLclVar) && (destLclOffs == srcLclOffs))
    {
        JITDUMP("Self-copy; replaced with a NOP.\n");
        GenTree* nop = gtNewNothingNode();
        INDEBUG(nop->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
        return nop;
    }

    if ((destLclVar != nullptr) && destLclVar->IsPromoted() && (destLclOffs == 0) &&
        (destLclVar->GetSize() == destSize) &&
        (!destLclVar->lvDoNotEnregister || (destLclVar->GetPromotedFieldCount() == 1)))
    {
        assert(varTypeIsStruct(destLclVar->GetType()));

        // We may decide later that a copyblk is required when this struct has holes
        destPromote = true;

        JITDUMP(" (destPromote=true)");
    }
    else
    {
        JITDUMP(" with mismatched dest offset/size");
    }

    if ((srcLclVar != nullptr) && srcLclVar->IsPromoted() && (srcLclOffs == 0) && (srcLclVar->GetSize() == destSize) &&
        (!srcLclVar->lvDoNotEnregister || (srcLclVar->GetPromotedFieldCount() == 1)))
    {
        assert(varTypeIsStruct(srcLclVar->GetType()));

        // We may decide later that a copyblk is required when this struct has holes
        srcPromote = true;

        JITDUMP(" (srcPromote=true)");
    }
    else
    {
        JITDUMP(" with mismatched src offset/size");
    }

#ifdef FEATURE_SIMD
    if (!destPromote && srcPromote && varTypeIsSIMD(srcLclVar->GetType()) && dest->OperIs(GT_LCL_VAR))
    {
        return fgMorphPromoteSimdAssignmentSrc(asg, srcLclNum);
    }

    if (destPromote && !srcPromote && varTypeIsSIMD(destLclVar->GetType()) &&
        src->OperIs(GT_LCL_VAR, GT_BITCAST, GT_HWINTRINSIC))
    {
        return fgMorphPromoteSimdAssignmentDst(asg, destLclNum);
    }
#endif // FEATURE_SIMD

    bool requiresCopyBlock = false;

    if (!destPromote && !srcPromote)
    {
        JITDUMP(" with no promoted structs");
        requiresCopyBlock = true;
    }
    else if ((destLclVar != nullptr) && destLclVar->lvRegStruct)
    {
        JITDUMP(" dest is register struct");
        requiresCopyBlock = true;
    }
    else if ((srcLclVar != nullptr) && srcLclVar->lvRegStruct)
    {
        JITDUMP(" src is register structs");
        requiresCopyBlock = true;
    }
    else if (destPromote && destLclVar->lvCustomLayout && destLclVar->lvContainsHoles)
    {
        JITDUMP(" dest has custom layout and contains holes");
        requiresCopyBlock = true;
    }
    else if (srcPromote && srcLclVar->lvCustomLayout && srcLclVar->lvContainsHoles)
    {
        JITDUMP(" src has custom layout and contains holes");
        requiresCopyBlock = true;
    }
    else if (src->OperIs(GT_CALL))
    {
        JITDUMP(" src is a call");
        requiresCopyBlock = true;
    }
    else if (src->OperIsHWIntrinsic())
    {
        JITDUMP(" src is a HWINTRINSIC node");
        requiresCopyBlock = true;
    }
#if defined(TARGET_ARM)
    else if (src->OperIsIndir() && src->AsIndir()->IsUnaligned())
    {
        JITDUMP(" src is unaligned");
        requiresCopyBlock = true;
    }
    else if (dest->OperIsIndir() && dest->AsIndir()->IsUnaligned())
    {
        JITDUMP(" dest is unaligned");
        requiresCopyBlock = true;
    }
#endif // TARGET_ARM
    else if (destPromote && srcPromote)
    {
        // Both structs should be of the same type, or each have the same number of fields, each having
        // the same type and offset. Actually, the destination could have less fields than the source
        // but there doesn't appear to be any such case in the entire FX. Copies between variables of
        // different types but same layout do occur though - Memory's implicit operator ReadOnlyMemory
        // uses Unsafe.As to perform the conversion, instead of copying the struct field by field.
        if (destLclVar->GetLayout() != srcLclVar->GetLayout())
        {
            bool sameLayout = destLclVar->GetPromotedFieldCount() == srcLclVar->GetPromotedFieldCount();

            for (unsigned i = 0; sameLayout && i < destLclVar->GetPromotedFieldCount(); i++)
            {
                LclVarDsc* destFieldLclVar = lvaGetDesc(destLclVar->GetPromotedFieldLclNum(i));
                LclVarDsc* srcFieldLclVar  = lvaGetDesc(srcLclVar->GetPromotedFieldLclNum(i));

                assert(destFieldLclVar->GetType() != TYP_STRUCT);

                if ((destFieldLclVar->GetPromotedFieldOffset() != srcFieldLclVar->GetPromotedFieldOffset()) ||
                    (destFieldLclVar->GetType() != srcFieldLclVar->GetType()))
                {
                    sameLayout = false;
                }
            }

            if (!sameLayout)
            {
                requiresCopyBlock = true;
                JITDUMP(" with mismatched types");
            }
        }
    }

    if (requiresCopyBlock)
    {
        JITDUMP(" this requires a CopyBlock.\n");

        if (asg->TypeIs(TYP_STRUCT))
        {
            if (srcLclVar != nullptr)
            {
                lvaSetVarDoNotEnregister(srcLclNum DEBUGARG(DNER_BlockOp));
            }

            if (destLclVar != nullptr)
            {
                lvaSetVarDoNotEnregister(destLclNum DEBUGARG(DNER_BlockOp));
            }
        }
        else
        {
            assert(varTypeIsSIMD(asg->GetType()));

            if (src->OperIs(GT_OBJ))
            {
                src->ChangeOper(GT_IND);
            }

            if (dest->OperIs(GT_OBJ))
            {
                dest->ChangeOper(GT_IND);
            }
        }

        dest->gtFlags |= GTF_DONT_CSE;

        asg->SetOp(0, dest);
        asg->SetOp(1, src);
        asg->SetSideEffects(GTF_ASG | asg->GetOp(0)->GetSideEffects() | asg->GetOp(1)->GetSideEffects());

        JITDUMPTREE(asg, "fgMorphCopyStruct: (after)\n");

        return asg;
    }

    JITDUMP(" using field by field assignments.\n");

    assert(destPromote || srcPromote);
    assert(!destPromote || (destLclNum != BAD_VAR_NUM) && (destLclVar != nullptr));
    assert(!srcPromote || (srcLclNum != BAD_VAR_NUM) && (srcLclVar != nullptr));

    GenTree* addr = nullptr;
    unsigned fieldCount;

    if (destPromote && srcPromote)
    {
        // To do fieldwise assignments for both sides, they'd better be the same struct type!
        assert(destLclVar->GetPromotedFieldCount() == srcLclVar->GetPromotedFieldCount());

        fieldCount = destLclVar->GetPromotedFieldCount();
    }
    else if (destPromote)
    {
        fieldCount = destLclVar->GetPromotedFieldCount();

        if (srcLclVar == nullptr)
        {
            addr = src->AsIndir()->GetAddr();
        }
        else if (fieldCount > 1)
        {
            // TODO-MIKE-CQ: Continue marking the unpromoted variable address exposed, to match the behavior
            // of the previous implementation. This isn't needed and one might expect that not marking locals
            // address exposed would be an improvement. However, the diffs are a bit of a grab bag so this
            // should be investigated separately.
            lvaSetVarAddrExposed(srcLclNum);
        }
    }
    else
    {
        fieldCount = srcLclVar->GetPromotedFieldCount();

        if (destLclVar == nullptr)
        {
            addr = dest->AsIndir()->GetAddr();
        }
        else if (fieldCount > 1)
        {
            // TODO-MIKE-CQ: Continue marking the unpromoted variable address exposed...
            lvaSetVarAddrExposed(destLclNum);
        }
    }

    GenTree* asgFieldCommaTree = nullptr;
    unsigned addrSpillLclNum   = BAD_VAR_NUM;

    if (addr != nullptr)
    {
        if (gtClone(addr) != nullptr)
        {
            // addr is a simple expression, no need to spill.
            noway_assert((addr->gtFlags & GTF_PERSISTENT_SIDE_EFFECTS) == 0);
        }
        else if (fieldCount > 1)
        {
            // addr is a complex expression and we need to use it multiple times, spill it.

            // A part of the address tree was already morphed and we're morphing
            // it again, GTF_DEBUG_NODE_MORPHED seems pretty useless...
            INDEBUG(fgMorphClearDebugNodeMorphed(addr);)

            // Simplify the address if possible, and mark as DONT_CSE as needed.
            addr = fgMorphTree(addr);

            addrSpillLclNum   = lvaNewTemp(TYP_BYREF, true DEBUGARG("BlockOp address local"));
            asgFieldCommaTree = gtNewAssignNode(gtNewLclvNode(addrSpillLclNum, TYP_BYREF), addr);

            // Update destLclVar and srcLclVar in case they were invalidated by lvaNewTemp expanding lvaTable
            if (destLclNum != BAD_VAR_NUM)
            {
                destLclVar = lvaGetDesc(destLclNum);
            }

            if (srcLclNum != BAD_VAR_NUM)
            {
                srcLclVar = lvaGetDesc(srcLclNum);
            }
        }
    }

    for (unsigned i = 0; i < fieldCount; ++i)
    {
        GenTree* destField;

        if (destPromote)
        {
            unsigned   destFieldLclNum = destLclVar->GetPromotedFieldLclNum(i);
            LclVarDsc* destFieldLclVar = lvaGetDesc(destFieldLclNum);

            destField = gtNewLclvNode(destFieldLclNum, destFieldLclVar->GetType());
            destField->gtFlags |= destFieldLclVar->lvAddrExposed ? GTF_GLOB_REF : GTF_EMPTY;
        }
        else if (destLclVar != nullptr)
        {
            unsigned   srcFieldLclNum = srcLclVar->GetPromotedFieldLclNum(i);
            LclVarDsc* srcFieldLclVar = lvaGetDesc(srcFieldLclNum);

            destField = gtNewLclFldNode(destLclNum, srcFieldLclVar->GetType(),
                                        destLclOffs + srcFieldLclVar->GetPromotedFieldOffset());
            destField->gtFlags |= destLclVar->lvAddrExposed ? GTF_GLOB_REF : GTF_EMPTY;

            // We don't have a field sequence for the destination field but one can be obtained from
            // the source field if the destination and source have the same type. Of course, other
            // cases could be handled by querying the VM for destination fields and trying to find
            // ones that are suitable for the current offset and type but this should be a rare case.
            //
            // TODO-MIKE-CQ: Currently this is only done if the destination is not itself a field,
            // otherwise we could combine the two field sequences if they match. Doesn't seem to be
            // worth the trouble, even the currently implemented trivial case has only a minor impact.
            if ((destLclOffs == 0) && (destFieldSeq == nullptr) && varTypeIsStruct(destLclVar->GetType()) &&
                (destLclVar->GetLayout() == srcLclVar->GetLayout()))
            {
                destField->AsLclFld()->SetFieldSeq(srcFieldLclVar->GetPromotedFieldSeq());
            }

            lvaSetVarDoNotEnregister(destLclNum DEBUGARG(DNER_LocalField));
        }
        else
        {
            GenTree* destFieldAddr;

            if (addrSpillLclNum != BAD_VAR_NUM)
            {
                destFieldAddr = gtNewLclvNode(addrSpillLclNum, TYP_BYREF);
            }
            else
            {
                destFieldAddr = (i == 0) ? addr : gtClone(addr);

                noway_assert(destFieldAddr != nullptr);
            }

            unsigned   srcFieldLclNum = srcLclVar->GetPromotedFieldLclNum(i);
            LclVarDsc* srcFieldLclVar = lvaGetDesc(srcFieldLclNum);

            // TODO-MIKE-Review: This looks fishy - it's only correct if the destination has the same type as the
            // source. If reinterpretation has ocurred then it would likely be wiser to use NotAField.

            FieldSeqNode* srcFieldSeq = srcFieldLclVar->GetPromotedFieldSeq();

            if (srcFieldLclVar->GetPromotedFieldOffset() == 0)
            {
                fgAddFieldSeqForZeroOffset(destFieldAddr, srcFieldSeq);
            }
            else
            {
                destFieldAddr = gtNewOperNode(GT_ADD, TYP_BYREF, destFieldAddr,
                                              gtNewIconNode(srcFieldLclVar->GetPromotedFieldOffset(), srcFieldSeq));
            }

            destField = gtNewIndir(srcFieldLclVar->GetType(), destFieldAddr);
            destField->gtFlags |= GTF_GLOB_REF;
        }

        GenTree* srcField = nullptr;

        if (srcPromote)
        {
            unsigned   srcFieldLclNum = srcLclVar->GetPromotedFieldLclNum(i);
            LclVarDsc* srcFieldLclVar = lvaGetDesc(srcFieldLclNum);

            srcField = gtNewLclvNode(srcFieldLclNum, srcFieldLclVar->GetType());
            srcField->gtFlags |= srcFieldLclVar->lvAddrExposed ? GTF_GLOB_REF : GTF_EMPTY;
        }
        else if (srcLclVar != nullptr)
        {
            unsigned   destFieldLclNum = destLclVar->GetPromotedFieldLclNum(i);
            LclVarDsc* destFieldLclVar = lvaGetDesc(destFieldLclNum);

            srcField = gtNewLclFldNode(srcLclNum, destFieldLclVar->GetType(),
                                       srcLclOffs + destFieldLclVar->GetPromotedFieldOffset());
            srcField->gtFlags |= srcLclVar->lvAddrExposed ? GTF_GLOB_REF : GTF_EMPTY;

            // We don't have a field sequence for the source field but one can be obtained from
            // the destination field if the destination and source have the same type.
            if ((srcLclOffs == 0) && (srcFieldSeq == nullptr) && varTypeIsStruct(srcLclVar->GetType()) &&
                (srcLclVar->GetLayout() == destLclVar->GetLayout()))
            {
                srcField->AsLclFld()->SetFieldSeq(destFieldLclVar->GetPromotedFieldSeq());
            }

            lvaSetVarDoNotEnregister(srcLclNum DEBUGARG(DNER_LocalField));
        }
        else
        {
            GenTree* srcFieldAddr = nullptr;

            if (addrSpillLclNum != BAD_VAR_NUM)
            {
                srcFieldAddr = gtNewLclvNode(addrSpillLclNum, TYP_BYREF);
            }
            else
            {
                srcFieldAddr = (i == 0) ? addr : gtClone(addr);

                noway_assert(srcFieldAddr != nullptr);
            }

            unsigned      destFieldLclNum = destLclVar->GetPromotedFieldLclNum(i);
            LclVarDsc*    destFieldLclVar = lvaGetDesc(destFieldLclNum);
            FieldSeqNode* destFieldSeq    = destFieldLclVar->GetPromotedFieldSeq();

            if (destFieldLclVar->GetPromotedFieldOffset() == 0)
            {
                fgAddFieldSeqForZeroOffset(srcFieldAddr, destFieldSeq);
            }
            else
            {
                srcFieldAddr = gtNewOperNode(GT_ADD, TYP_BYREF, srcFieldAddr,
                                             gtNewIconNode(destFieldLclVar->GetPromotedFieldOffset(), destFieldSeq));
            }

            srcField = gtNewIndir(destFieldLclVar->GetType(), srcFieldAddr);
            srcField->gtFlags |= GTF_GLOB_REF;
        }

        noway_assert(destField->GetType() == srcField->GetType());

        GenTreeOp* asgField = gtNewAssignNode(destField, srcField);

#if LOCAL_ASSERTION_PROP
        if (optLocalAssertionProp)
        {
            optAssertionGen(asgField);
        }
#endif

        if (asgFieldCommaTree != nullptr)
        {
            asgFieldCommaTree = gtNewCommaNode(asgFieldCommaTree, asgField);
        }
        else
        {
            asgFieldCommaTree = asgField;
        }
    }

    asgFieldCommaTree->gtFlags |= (asg->gtFlags & GTF_LATE_ARG);

    INDEBUG(asgFieldCommaTree->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)

    JITDUMPTREE(asgFieldCommaTree, "fgMorphCopyStruct (after promotion):\n\n");

    return asgFieldCommaTree;
}

// insert conversions and normalize to make tree amenable to register
// FP architectures
GenTree* Compiler::fgMorphForRegisterFP(GenTree* tree)
{
    if (tree->OperIsArithmetic())
    {
        if (varTypeIsFloating(tree))
        {
            GenTree* op1 = tree->AsOp()->gtOp1;
            GenTree* op2 = tree->gtGetOp2();

            assert(varTypeIsFloating(op1->TypeGet()) && varTypeIsFloating(op2->TypeGet()));

            if (op1->TypeGet() != tree->TypeGet())
            {
                tree->AsOp()->gtOp1 = gtNewCastNode(tree->TypeGet(), op1, false, tree->TypeGet());
            }
            if (op2->TypeGet() != tree->TypeGet())
            {
                tree->AsOp()->gtOp2 = gtNewCastNode(tree->TypeGet(), op2, false, tree->TypeGet());
            }
        }
    }
    else if (tree->OperIsCompare())
    {
        GenTree* op1 = tree->AsOp()->gtOp1;

        if (varTypeIsFloating(op1))
        {
            GenTree* op2 = tree->gtGetOp2();
            assert(varTypeIsFloating(op2));

            if (op1->TypeGet() != op2->TypeGet())
            {
                // both had better be floating, just one bigger than other
                if (op1->TypeGet() == TYP_FLOAT)
                {
                    assert(op2->TypeGet() == TYP_DOUBLE);
                    tree->AsOp()->gtOp1 = gtNewCastNode(TYP_DOUBLE, op1, false, TYP_DOUBLE);
                }
                else if (op2->TypeGet() == TYP_FLOAT)
                {
                    assert(op1->TypeGet() == TYP_DOUBLE);
                    tree->AsOp()->gtOp2 = gtNewCastNode(TYP_DOUBLE, op2, false, TYP_DOUBLE);
                }
            }
        }
    }

    return tree;
}

//------------------------------------------------------------------------------
// fgMorphAssociative : Try to simplify "(X op C1) op C2" to "X op C3"
//                      for associative operators.
//
// Arguments:
//       tree - node to fold
//
// return value:
//       A folded GenTree* instance or nullptr if something prevents folding.
//

GenTree* Compiler::fgMorphAssociative(GenTreeOp* tree)
{
    assert(varTypeIsIntegralOrI(tree->TypeGet()));
    assert(tree->OperIs(GT_ADD, GT_MUL, GT_OR, GT_AND, GT_XOR));

    // op1 can be GT_COMMA, in this case we're going to fold
    // "(op (COMMA(... (op X C1))) C2)" to "(COMMA(... (op X C3)))"
    GenTree*   op1  = tree->GetOp(0)->SkipComma();
    genTreeOps oper = tree->OperGet();

    if (!op1->OperIs(oper) || !tree->gtGetOp2()->IsCnsIntOrI() || !op1->gtGetOp2()->IsCnsIntOrI() ||
        op1->gtGetOp1()->IsCnsIntOrI() || gtIsActiveCSE_Candidate(op1))
    {
        return nullptr;
    }

    if (tree->OperMayOverflow() && (tree->gtOverflow() || op1->gtOverflow()))
    {
        return nullptr;
    }

    GenTreeIntCon* cns1 = op1->gtGetOp2()->AsIntCon();
    GenTreeIntCon* cns2 = tree->gtGetOp2()->AsIntCon();

    if (!varTypeIsIntegralOrI(tree->TypeGet()) || cns1->TypeIs(TYP_REF) || !cns1->TypeIs(cns2->TypeGet()))
    {
        return nullptr;
    }

    GenTree* foldedCns = gtFoldExprConst(gtNewOperNode(oper, cns1->TypeGet(), cns1, cns2));
    if (!foldedCns->IsCnsIntOrI())
    {
        // Give up if we can't fold "C1 op C2"
        return nullptr;
    }

    cns1->SetValue(foldedCns->AsIntCon()->GetValue());
    cns1->SetFieldSeq(foldedCns->AsIntCon()->GetFieldSeq());

    GenTreeOp* newTree = tree->gtGetOp1()->AsOp();
    DEBUG_DESTROY_NODE(tree);
    DEBUG_DESTROY_NODE(cns2);
    DEBUG_DESTROY_NODE(foldedCns);
    INDEBUG(newTree->gtOp2->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
    return newTree;
}

GenTree* Compiler::fgMorphNormalizeLclVarStore(GenTreeOp* asg)
{
    assert(asg->OperIs(GT_ASG));
    assert(fgGlobalMorph);

    GenTree* op1 = asg->GetOp(0);
    GenTree* op2 = asg->GetOp(1);

    if (varActualType(op1->GetType()) == TYP_INT)
    {
        LclVarDsc* lcl = lvaGetDesc(op1->AsLclVar());

        if (lcl->lvNormalizeOnStore())
        {
            op1->SetType(TYP_INT);

            if (gtIsSmallIntCastNeeded(op2, lcl->GetType()))
            {
                op2 = gtNewCastNode(TYP_INT, op2, false, lcl->GetType());
                op2->gtFlags |= asg->gtFlags & GTF_COLON_COND;
                asg->SetOp(1, op2);
            }
        }
    }

    return op2;
}

/*****************************************************************************
 *
 *  Transform the given GTK_SMPOP tree for code generation.
 */

#ifdef _PREFAST_
#pragma warning(push)
#pragma warning(disable : 21000) // Suppress PREFast warning about overly large function
#endif
GenTree* Compiler::fgMorphSmpOp(GenTree* tree, MorphAddrContext* mac)
{
    ALLOCA_CHECK();
    assert(tree->OperKind() & GTK_SMPOP);

    /* The steps in this function are :
       o Perform required preorder processing
       o Process the first, then second operand, if any
       o Perform required postorder morphing
       o Perform optional postorder morphing if optimizing
     */

    bool isQmarkColon = false;

#if LOCAL_ASSERTION_PROP
    AssertionIndex origAssertionCount = DUMMY_INIT(0);
    AssertionDsc*  origAssertionTab   = DUMMY_INIT(NULL);

    AssertionIndex thenAssertionCount = DUMMY_INIT(0);
    AssertionDsc*  thenAssertionTab   = DUMMY_INIT(NULL);
#endif

    if (fgGlobalMorph)
    {
        tree = fgMorphForRegisterFP(tree);
    }

    genTreeOps oper = tree->OperGet();
    var_types  typ  = tree->TypeGet();
    GenTree*   op1  = tree->AsOp()->gtOp1;
    GenTree*   op2  = tree->gtGetOp2IfPresent();

    /*-------------------------------------------------------------------------
     * First do any PRE-ORDER processing
     */

    switch (oper)
    {
        // Some arithmetic operators need to use a helper call to the EE
        int helper;

        case GT_ASG:
            if (fgGlobalMorph && op1->OperIs(GT_LCL_VAR))
            {
                op2 = fgMorphNormalizeLclVarStore(tree->AsOp());
            }
            FALLTHROUGH;
        case GT_ADDR:
            assert(!op1->OperIsHWIntrinsic());
            // op1 of a ADDR/ASG is an l-value. Only r-values can be CSEed
            op1->gtFlags |= GTF_DONT_CSE;
            break;

        case GT_QMARK:
        case GT_JTRUE:

            noway_assert(op1);

            if (op1->OperKind() & GTK_RELOP)
            {
                noway_assert((oper == GT_JTRUE) || (op1->gtFlags & GTF_RELOP_QMARK));
                /* Mark the comparison node with GTF_RELOP_JMP_USED so it knows that it does
                   not need to materialize the result as a 0 or 1. */

                /* We also mark it as DONT_CSE, as we don't handle QMARKs with nonRELOP op1s */
                op1->gtFlags |= (GTF_RELOP_JMP_USED | GTF_DONT_CSE);

                // Request that the codegen for op1 sets the condition flags
                // when it generates the code for op1.
                //
                // Codegen for op1 must set the condition flags if
                // this method returns true.
                //
                op1->gtRequestSetFlags();
            }
            else
            {
                GenTree* effOp1 = op1->gtEffectiveVal();
                noway_assert((effOp1->gtOper == GT_CNS_INT) &&
                             (effOp1->IsIntegralConst(0) || effOp1->IsIntegralConst(1)));
            }
            break;

        case GT_COLON:
#if LOCAL_ASSERTION_PROP
            if (optLocalAssertionProp)
#endif
            {
                isQmarkColon = true;
            }
            break;

        case GT_INDEX:
            return fgMorphArrayIndex(tree->AsIndex());

        case GT_CAST:
            return fgMorphCast(tree->AsCast());

        case GT_MUL:
            if (opts.OptimizationEnabled() && !optValnumCSE_phase && !tree->gtOverflow())
            {
                // MUL(NEG(a), C) => MUL(a, NEG(C))
                if (op1->OperIs(GT_NEG) && !op1->gtGetOp1()->IsCnsIntOrI() && op2->IsCnsIntOrI() &&
                    !op2->IsIconHandle())
                {
                    GenTree* newOp1   = op1->gtGetOp1();
                    GenTree* newConst = gtNewIconNode(-op2->AsIntCon()->IconValue(), op2->TypeGet());
                    DEBUG_DESTROY_NODE(op1);
                    DEBUG_DESTROY_NODE(op2);
                    tree->AsOp()->gtOp1 = newOp1;
                    tree->AsOp()->gtOp2 = newConst;
                    return fgMorphSmpOp(tree, mac);
                }
            }

#ifndef TARGET_64BIT
            if (typ == TYP_LONG)
            {
                /* For (long)int1 * (long)int2, we dont actually do the
                   casts, and just multiply the 32 bit values, which will
                   give us the 64 bit result in edx:eax */

                noway_assert(op2);
                if ((op1->gtOper == GT_CAST && op2->gtOper == GT_CAST &&
                     genActualType(op1->CastFromType()) == TYP_INT && genActualType(op2->CastFromType()) == TYP_INT) &&
                    !op1->gtOverflow() && !op2->gtOverflow())
                {
                    // The casts have to be of the same signedness.
                    if ((op1->gtFlags & GTF_UNSIGNED) != (op2->gtFlags & GTF_UNSIGNED))
                    {
                        // We see if we can force an int constant to change its signedness
                        GenTree* constOp;
                        if (op1->AsCast()->CastOp()->gtOper == GT_CNS_INT)
                            constOp = op1;
                        else if (op2->AsCast()->CastOp()->gtOper == GT_CNS_INT)
                            constOp = op2;
                        else
                            goto NO_MUL_64RSLT;

                        if (((unsigned)(constOp->AsCast()->CastOp()->AsIntCon()->gtIconVal) < (unsigned)(0x80000000)))
                            constOp->gtFlags ^= GTF_UNSIGNED;
                        else
                            goto NO_MUL_64RSLT;
                    }

                    // The only combination that can overflow
                    if (tree->gtOverflow() && (tree->gtFlags & GTF_UNSIGNED) && !(op1->gtFlags & GTF_UNSIGNED))
                        goto NO_MUL_64RSLT;

                    /* Remaining combinations can never overflow during long mul. */

                    tree->gtFlags &= ~GTF_OVERFLOW;

                    /* Do unsigned mul only if the casts were unsigned */

                    tree->gtFlags &= ~GTF_UNSIGNED;
                    tree->gtFlags |= op1->gtFlags & GTF_UNSIGNED;

                    /* Since we are committing to GTF_MUL_64RSLT, we don't want
                       the casts to be folded away. So morph the castees directly */

                    op1->AsOp()->gtOp1 = fgMorphTree(op1->AsOp()->gtOp1);
                    op2->AsOp()->gtOp1 = fgMorphTree(op2->AsOp()->gtOp1);

                    // Propagate side effect flags up the tree
                    op1->gtFlags &= ~GTF_ALL_EFFECT;
                    op1->gtFlags |= (op1->AsOp()->gtOp1->gtFlags & GTF_ALL_EFFECT);
                    op2->gtFlags &= ~GTF_ALL_EFFECT;
                    op2->gtFlags |= (op2->AsOp()->gtOp1->gtFlags & GTF_ALL_EFFECT);

                    // If the GT_MUL can be altogether folded away, we should do that.

                    if ((op1->AsCast()->CastOp()->OperKind() & op2->AsCast()->CastOp()->OperKind() & GTK_CONST) &&
                        opts.OptEnabled(CLFLG_CONSTANTFOLD))
                    {
                        tree->AsOp()->gtOp1 = op1 = gtFoldExprConst(op1);
                        tree->AsOp()->gtOp2 = op2 = gtFoldExprConst(op2);
                        noway_assert(op1->OperKind() & op2->OperKind() & GTK_CONST);
                        tree = gtFoldExprConst(tree);
                        noway_assert(tree->OperIsConst());
                        return tree;
                    }

                    tree->gtFlags |= GTF_MUL_64RSLT;

                    // If op1 and op2 are unsigned casts, we need to do an unsigned mult
                    tree->gtFlags |= (op1->gtFlags & GTF_UNSIGNED);

                    // Insert GT_NOP nodes for the cast operands so that they do not get folded
                    // And propagate the new flags. We don't want to CSE the casts because
                    // codegen expects GTF_MUL_64RSLT muls to have a certain layout.

                    if (op1->AsCast()->CastOp()->OperGet() != GT_NOP)
                    {
                        op1->AsOp()->gtOp1 = gtNewOperNode(GT_NOP, TYP_INT, op1->AsCast()->CastOp());
                        op1->gtFlags &= ~GTF_ALL_EFFECT;
                        op1->gtFlags |= (op1->AsCast()->CastOp()->gtFlags & GTF_ALL_EFFECT);
                    }

                    if (op2->AsCast()->CastOp()->OperGet() != GT_NOP)
                    {
                        op2->AsOp()->gtOp1 = gtNewOperNode(GT_NOP, TYP_INT, op2->AsCast()->CastOp());
                        op2->gtFlags &= ~GTF_ALL_EFFECT;
                        op2->gtFlags |= (op2->AsCast()->CastOp()->gtFlags & GTF_ALL_EFFECT);
                    }

                    op1->gtFlags |= GTF_DONT_CSE;
                    op2->gtFlags |= GTF_DONT_CSE;

                    tree->gtFlags &= ~GTF_ALL_EFFECT;
                    tree->gtFlags |= ((op1->gtFlags | op2->gtFlags) & GTF_ALL_EFFECT);

                    goto DONE_MORPHING_CHILDREN;
                }
                else if ((tree->gtFlags & GTF_MUL_64RSLT) == 0)
                {
                NO_MUL_64RSLT:
                    if (tree->gtOverflow())
                        helper = (tree->gtFlags & GTF_UNSIGNED) ? CORINFO_HELP_ULMUL_OVF : CORINFO_HELP_LMUL_OVF;
                    else
                        helper = CORINFO_HELP_LMUL;

                    goto USE_HELPER_FOR_ARITH;
                }
                else
                {
                    // We are seeing this node again. We have decided to use
                    // GTF_MUL_64RSLT, so leave it alone.

                    assert(tree->gtIsValid64RsltMul());
                }
            }
#endif // !TARGET_64BIT
            break;

        case GT_ARR_LENGTH:
            if (op1->OperIs(GT_CNS_STR))
            {
                // Optimize `ldstr + String::get_Length()` to CNS_INT
                // e.g. "Hello".Length => 5
                GenTreeIntCon* iconNode = gtNewStringLiteralLength(op1->AsStrCon());
                if (iconNode != nullptr)
                {
                    INDEBUG(iconNode->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
                    return iconNode;
                }
            }
            break;

        case GT_DIV:
            // Replace "val / dcon" with "val * (1.0 / dcon)" if dcon is a power of two.
            // Powers of two within range are always exactly represented,
            // so multiplication by the reciprocal is safe in this scenario
            if (fgGlobalMorph && op2->IsCnsFltOrDbl())
            {
                double divisor = op2->AsDblCon()->gtDconVal;
                if (((typ == TYP_DOUBLE) && FloatingPointUtils::hasPreciseReciprocal(divisor)) ||
                    ((typ == TYP_FLOAT) && FloatingPointUtils::hasPreciseReciprocal(forceCastToFloat(divisor))))
                {
                    oper = GT_MUL;
                    tree->ChangeOper(oper);
                    op2->AsDblCon()->gtDconVal = 1.0 / divisor;
                }
            }

            // array.Length is always positive so GT_DIV can be changed to GT_UDIV
            // if op2 is a positive cns
            if (!optValnumCSE_phase && op1->OperIs(GT_ARR_LENGTH) && op2->IsIntegralConst() &&
                op2->AsIntCon()->IconValue() >= 2) // for 0 and 1 it doesn't matter if it's UDIV or DIV
            {
                assert(tree->OperIs(GT_DIV));
                tree->ChangeOper(GT_UDIV);
                return fgMorphSmpOp(tree, mac);
            }

            if (opts.OptimizationEnabled() && !optValnumCSE_phase)
            {
                // DIV(NEG(a), C) => DIV(a, NEG(C))
                if (op1->OperIs(GT_NEG) && !op1->gtGetOp1()->IsCnsIntOrI() && op2->IsCnsIntOrI() &&
                    !op2->IsIconHandle())
                {
                    ssize_t op2Value = op2->AsIntCon()->IconValue();
                    if (op2Value != 1 && op2Value != -1) // Div must throw exception for int(long).MinValue / -1.
                    {
                        tree->AsOp()->gtOp1 = op1->gtGetOp1();
                        DEBUG_DESTROY_NODE(op1);
                        tree->AsOp()->gtOp2 = gtNewIconNode(-op2Value, op2->TypeGet());
                        DEBUG_DESTROY_NODE(op2);
                        return fgMorphSmpOp(tree, mac);
                    }
                }
            }

#ifndef TARGET_64BIT
            if (typ == TYP_LONG)
            {
                helper = CORINFO_HELP_LDIV;
                goto USE_HELPER_FOR_ARITH;
            }

#if USE_HELPERS_FOR_INT_DIV
            if (typ == TYP_INT)
            {
                helper = CORINFO_HELP_DIV;
                goto USE_HELPER_FOR_ARITH;
            }
#endif
#endif // !TARGET_64BIT

            if (op2->gtOper == GT_CAST && op2->AsOp()->gtOp1->IsCnsIntOrI())
            {
                op2 = gtFoldExprConst(op2);
            }
            break;

        case GT_UDIV:

#ifndef TARGET_64BIT
            if (typ == TYP_LONG)
            {
                helper = CORINFO_HELP_ULDIV;
                goto USE_HELPER_FOR_ARITH;
            }
#if USE_HELPERS_FOR_INT_DIV
            if (typ == TYP_INT)
            {
                helper = CORINFO_HELP_UDIV;
                goto USE_HELPER_FOR_ARITH;
            }
#endif
#endif // TARGET_64BIT
            break;

        case GT_MOD:

            if (varTypeIsFloating(typ))
            {
                helper = CORINFO_HELP_DBLREM;
                noway_assert(op2);
                if (op1->TypeGet() == TYP_FLOAT)
                {
                    if (op2->TypeGet() == TYP_FLOAT)
                    {
                        helper = CORINFO_HELP_FLTREM;
                    }
                    else
                    {
                        tree->AsOp()->gtOp1 = op1 = gtNewCastNode(TYP_DOUBLE, op1, false, TYP_DOUBLE);
                    }
                }
                else if (op2->TypeGet() == TYP_FLOAT)
                {
                    tree->AsOp()->gtOp2 = op2 = gtNewCastNode(TYP_DOUBLE, op2, false, TYP_DOUBLE);
                }
                goto USE_HELPER_FOR_ARITH;
            }

            // array.Length is always positive so GT_DIV can be changed to GT_UDIV
            // if op2 is a positive cns
            if (!optValnumCSE_phase && op1->OperIs(GT_ARR_LENGTH) && op2->IsIntegralConst() &&
                op2->AsIntCon()->IconValue() >= 2) // for 0 and 1 it doesn't matter if it's UMOD or MOD
            {
                assert(tree->OperIs(GT_MOD));
                tree->ChangeOper(GT_UMOD);
                return fgMorphSmpOp(tree, mac);
            }

            // Do not use optimizations (unlike UMOD's idiv optimizing during codegen) for signed mod.
            // A similar optimization for signed mod will not work for a negative perfectly divisible
            // HI-word. To make it correct, we would need to divide without the sign and then flip the
            // result sign after mod. This requires 18 opcodes + flow making it not worthy to inline.
            goto ASSIGN_HELPER_FOR_MOD;

        case GT_UMOD:

#ifdef TARGET_ARMARCH
//
// Note for TARGET_ARMARCH we don't have  a remainder instruction, so we don't do this optimization
//
#else  // TARGET_XARCH
            /* If this is an unsigned long mod with op2 which is a cast to long from a
               constant int, then don't morph to a call to the helper.  This can be done
               faster inline using idiv.
            */

            noway_assert(op2);
            if ((typ == TYP_LONG) && opts.OptEnabled(CLFLG_CONSTANTFOLD) &&
                ((tree->gtFlags & GTF_UNSIGNED) == (op1->gtFlags & GTF_UNSIGNED)) &&
                ((tree->gtFlags & GTF_UNSIGNED) == (op2->gtFlags & GTF_UNSIGNED)))
            {
                if (op2->gtOper == GT_CAST && op2->AsCast()->CastOp()->gtOper == GT_CNS_INT &&
                    op2->AsCast()->CastOp()->AsIntCon()->gtIconVal >= 2 &&
                    op2->AsCast()->CastOp()->AsIntCon()->gtIconVal <= 0x3fffffff &&
                    (tree->gtFlags & GTF_UNSIGNED) == (op2->AsCast()->CastOp()->gtFlags & GTF_UNSIGNED))
                {
                    tree->AsOp()->gtOp2 = op2 = fgMorphCast(op2->AsCast());
                    noway_assert(op2->gtOper == GT_CNS_NATIVELONG);
                }

                if (op2->gtOper == GT_CNS_NATIVELONG && op2->AsIntConCommon()->LngValue() >= 2 &&
                    op2->AsIntConCommon()->LngValue() <= 0x3fffffff)
                {
                    tree->AsOp()->gtOp1 = op1 = fgMorphTree(op1);
                    noway_assert(op1->TypeGet() == TYP_LONG);

                    // Update flags for op1 morph
                    tree->gtFlags &= ~GTF_ALL_EFFECT;

                    tree->gtFlags |= (op1->gtFlags & GTF_ALL_EFFECT); // Only update with op1 as op2 is a constant

                    // If op1 is a constant, then do constant folding of the division operator
                    if (op1->gtOper == GT_CNS_NATIVELONG)
                    {
                        tree = gtFoldExpr(tree);
                    }

                    // We may fail to fold
                    if (!tree->OperIsConst())
                    {
                        tree->AsOp()->CheckDivideByConstOptimized(this);
                    }

                    return tree;
                }
            }
#endif // TARGET_XARCH

        ASSIGN_HELPER_FOR_MOD:

            // For "val % 1", return 0 if op1 doesn't have any side effects
            // and we are not in the CSE phase, we cannot discard 'tree'
            // because it may contain CSE expressions that we haven't yet examined.
            //
            if (((op1->gtFlags & GTF_SIDE_EFFECT) == 0) && !optValnumCSE_phase)
            {
                if (op2->IsIntegralConst(1))
                {
                    GenTree* zeroNode = gtNewZeroConNode(typ);
#ifdef DEBUG
                    zeroNode->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;
#endif
                    DEBUG_DESTROY_NODE(tree);
                    return zeroNode;
                }
            }

#ifndef TARGET_64BIT
            if (typ == TYP_LONG)
            {
                helper = (oper == GT_UMOD) ? CORINFO_HELP_ULMOD : CORINFO_HELP_LMOD;
                goto USE_HELPER_FOR_ARITH;
            }

#if USE_HELPERS_FOR_INT_DIV
            if (typ == TYP_INT)
            {
                if (oper == GT_UMOD)
                {
                    helper = CORINFO_HELP_UMOD;
                    goto USE_HELPER_FOR_ARITH;
                }
                else if (oper == GT_MOD)
                {
                    helper = CORINFO_HELP_MOD;
                    goto USE_HELPER_FOR_ARITH;
                }
            }
#endif
#endif // !TARGET_64BIT

            if (op2->gtOper == GT_CAST && op2->AsOp()->gtOp1->IsCnsIntOrI())
            {
                op2 = gtFoldExprConst(op2);
            }

#ifdef TARGET_ARM64
            // For ARM64 we don't have a remainder instruction,
            // The architecture manual suggests the following transformation to
            // generate code for such operator:
            //
            // a % b = a - (a / b) * b;
            //
            // TODO: there are special cases where it can be done better, for example
            // when the modulo operation is unsigned and the divisor is a
            // integer constant power of two.  In this case, we can make the transform:
            //
            // a % b = a & (b - 1);
            //
            // Lower supports it for all cases except when `a` is constant, but
            // in Morph we can't guarantee that `a` won't be transformed into a constant,
            // so can't guarantee that lower will be able to do this optimization.
            {
                // Do "a % b = a - (a / b) * b" morph always, see TODO before this block.
                bool doMorphModToSubMulDiv = true;

                if (doMorphModToSubMulDiv)
                {
                    assert(!optValnumCSE_phase);

                    tree = fgMorphModToSubMulDiv(tree->AsOp());
                    op1  = tree->AsOp()->gtOp1;
                    op2  = tree->AsOp()->gtOp2;
                }
            }
#else  // !TARGET_ARM64
            // If b is not a power of 2 constant then lowering replaces a % b
            // with a - (a / b) * b and applies magic division optimization to
            // a / b. The code may already contain an a / b expression (e.g.
            // x = a / 10; y = a % 10;) and then we end up with redundant code.
            // If we convert % to / here we give CSE the opportunity to eliminate
            // the redundant division. If there's no redundant division then
            // nothing is lost, lowering would have done this transform anyway.

            if (!optValnumCSE_phase && ((tree->OperGet() == GT_MOD) && op2->IsIntegralConst()))
            {
                ssize_t divisorValue    = op2->AsIntCon()->IconValue();
                size_t  absDivisorValue = (divisorValue == SSIZE_T_MIN) ? static_cast<size_t>(divisorValue)
                                                                       : static_cast<size_t>(abs(divisorValue));

                if (!isPow2(absDivisorValue))
                {
                    tree = fgMorphModToSubMulDiv(tree->AsOp());
                    op1  = tree->AsOp()->gtOp1;
                    op2  = tree->AsOp()->gtOp2;
                }
            }
#endif // !TARGET_ARM64
            break;

        USE_HELPER_FOR_ARITH:
        {
            // TODO: this comment is wrong now, do an appropriate fix.
            /* We have to morph these arithmetic operations into helper calls
               before morphing the arguments (preorder), else the arguments
               won't get correct values of fgPtrArgCntCur.
               However, try to fold the tree first in case we end up with a
               simple node which won't need a helper call at all */

            noway_assert(tree->OperIsBinary());

            GenTree* oldTree = tree;

            tree = gtFoldExpr(tree);

            // Were we able to fold it ?
            // Note that gtFoldExpr may return a non-leaf even if successful
            // e.g. for something like "expr / 1" - see also bug #290853
            if (tree->OperIsLeaf() || (oldTree != tree))
            {
                return (oldTree != tree) ? fgMorphTree(tree) : fgMorphLeaf(tree);
            }

            // Did we fold it into a comma node with throw?
            if (tree->gtOper == GT_COMMA)
            {
                noway_assert(fgIsCommaThrow(tree));
                return fgMorphTree(tree);
            }
        }
            return fgMorphIntoHelperCall(tree, helper, gtNewCallArgs(op1, op2));

        case GT_RETURN:
            if (fgGlobalMorph && varTypeIsSmall(info.compRetType) && gtIsSmallIntCastNeeded(op1, info.compRetType))
            {
                // Small-typed return values are extended by the callee.

                op1 = gtNewCastNode(TYP_INT, op1, false, info.compRetType);
                op1->gtFlags |= (tree->gtFlags & GTF_COLON_COND);
                op1 = fgMorphCast(op1->AsCast());

                tree->AsUnOp()->SetOp(0, op1);
                tree->gtFlags &= ~GTF_ALL_EFFECT;
                tree->gtFlags |= (op1->gtFlags & GTF_ALL_EFFECT);

                return tree;
            }
            break;

        case GT_EQ:
        case GT_NE:
        {
            GenTree* optimizedTree = gtFoldTypeCompare(tree);

            if (optimizedTree != tree)
            {
                return fgMorphTree(optimizedTree);
            }
        }

            FALLTHROUGH;

        case GT_GT:
        {
            // Try and optimize nullable boxes feeding compares
            GenTree* optimizedTree = gtFoldBoxNullable(tree->AsOp());

            if (optimizedTree->OperGet() != tree->OperGet())
            {
                return optimizedTree;
            }
            else
            {
                tree = optimizedTree;
            }

            op1 = tree->AsOp()->gtOp1;
            op2 = tree->gtGetOp2IfPresent();

            break;
        }

        case GT_RUNTIMELOOKUP:
            return fgMorphTree(op1);

#ifdef TARGET_ARM
        case GT_INTRINSIC:
            if (tree->AsIntrinsic()->gtIntrinsicName == NI_System_Math_Round)
            {
                switch (tree->TypeGet())
                {
                    case TYP_DOUBLE:
                        return fgMorphIntoHelperCall(tree, CORINFO_HELP_DBLROUND, gtNewCallArgs(op1));
                    case TYP_FLOAT:
                        return fgMorphIntoHelperCall(tree, CORINFO_HELP_FLTROUND, gtNewCallArgs(op1));
                    default:
                        unreached();
                }
            }
            break;
#endif

        default:
            break;
    }

    /*-------------------------------------------------------------------------
     * Process the first operand, if any
     */

    if (op1)
    {

#if LOCAL_ASSERTION_PROP
        // If we are entering the "then" part of a Qmark-Colon we must
        // save the state of the current copy assignment table
        // so that we can restore this state when entering the "else" part
        if (isQmarkColon)
        {
            noway_assert(optLocalAssertionProp);
            if (optAssertionCount)
            {
                noway_assert(optAssertionCount <= optMaxAssertionCount); // else ALLOCA() is a bad idea
                unsigned tabSize   = optAssertionCount * sizeof(AssertionDsc);
                origAssertionTab   = (AssertionDsc*)ALLOCA(tabSize);
                origAssertionCount = optAssertionCount;
                memcpy(origAssertionTab, optAssertionTabPrivate, tabSize);
            }
            else
            {
                origAssertionCount = 0;
                origAssertionTab   = nullptr;
            }
        }
#endif // LOCAL_ASSERTION_PROP

        // We might need a new MorphAddressContext context.  (These are used to convey
        // parent context about how addresses being calculated will be used; see the
        // specification comment for MorphAddrContext for full details.)
        // Assume it's an Ind context to start.
        MorphAddrContext  subIndMac1(MACK_Ind);
        MorphAddrContext* subMac1 = mac;
        if (subMac1 == nullptr || subMac1->m_kind == MACK_Ind)
        {
            switch (tree->gtOper)
            {
                case GT_ADDR:
                    // A non-null mac here implies this node is part of an address computation.
                    // If so, we need to pass the existing mac down to the child node.
                    //
                    // Otherwise, use a new mac.
                    if (subMac1 == nullptr)
                    {
                        subMac1         = &subIndMac1;
                        subMac1->m_kind = MACK_Addr;
                    }
                    break;
                case GT_COMMA:
                    // In a comma, the incoming context only applies to the rightmost arg of the
                    // comma list.  The left arg (op1) gets a fresh context.
                    subMac1 = nullptr;
                    break;
                case GT_OBJ:
                case GT_BLK:
                case GT_DYN_BLK:
                case GT_IND:
                    // A non-null mac here implies this node is part of an address computation (the tree parent is
                    // GT_ADDR).
                    // If so, we need to pass the existing mac down to the child node.
                    //
                    // Otherwise, use a new mac.
                    if (subMac1 == nullptr)
                    {
                        subMac1 = &subIndMac1;
                    }
                    break;
                default:
                    break;
            }
        }

        // For additions, if we're in an IND context keep track of whether
        // all offsets added to the address are constant, and their sum.
        if (tree->gtOper == GT_ADD && subMac1 != nullptr)
        {
            assert(subMac1->m_kind == MACK_Ind || subMac1->m_kind == MACK_Addr); // Can't be a CopyBlock.
            GenTree* otherOp = tree->AsOp()->gtOp2;
            // Is the other operator a constant?
            if (otherOp->IsCnsIntOrI())
            {
                ClrSafeInt<size_t> totalOffset(subMac1->m_totalOffset);
                totalOffset += otherOp->AsIntConCommon()->IconValue();
                if (totalOffset.IsOverflow())
                {
                    // We will consider an offset so large as to overflow as "not a constant" --
                    // we will do a null check.
                    subMac1->m_allConstantOffsets = false;
                }
                else
                {
                    subMac1->m_totalOffset += otherOp->AsIntConCommon()->IconValue();
                }
            }
            else
            {
                subMac1->m_allConstantOffsets = false;
            }
        }

        // If op1 is a GT_FIELD or indir, we need to pass down the mac if
        // its parent is GT_ADDR, since the address of op1
        // is part of an ongoing address computation. Otherwise
        // op1 represents the value of the field and so any address
        // calculations it does are in a new context.
        if (((op1->gtOper == GT_FIELD) || op1->OperIsIndir()) && (tree->gtOper != GT_ADDR))
        {
            subMac1 = nullptr;

            // The impact of op1's value to any ongoing
            // address computation is handled below when looking
            // at op2.
        }

        tree->AsOp()->gtOp1 = op1 = fgMorphTree(op1, subMac1);

#if LOCAL_ASSERTION_PROP
        // If we are exiting the "then" part of a Qmark-Colon we must
        // save the state of the current copy assignment table
        // so that we can merge this state with the "else" part exit
        if (isQmarkColon)
        {
            noway_assert(optLocalAssertionProp);
            if (optAssertionCount)
            {
                noway_assert(optAssertionCount <= optMaxAssertionCount); // else ALLOCA() is a bad idea
                unsigned tabSize   = optAssertionCount * sizeof(AssertionDsc);
                thenAssertionTab   = (AssertionDsc*)ALLOCA(tabSize);
                thenAssertionCount = optAssertionCount;
                memcpy(thenAssertionTab, optAssertionTabPrivate, tabSize);
            }
            else
            {
                thenAssertionCount = 0;
                thenAssertionTab   = nullptr;
            }
        }
#endif // LOCAL_ASSERTION_PROP

        /* Morphing along with folding and inlining may have changed the
         * side effect flags, so we have to reset them
         *
         * NOTE: Don't reset the exception flags on nodes that may throw */

        assert(tree->gtOper != GT_CALL);

        if (!tree->OperRequiresCallFlag(this))
        {
            tree->gtFlags &= ~GTF_CALL;
        }

        /* Propagate the new flags */
        tree->gtFlags |= (op1->gtFlags & GTF_ALL_EFFECT);

        // &aliasedVar doesn't need GTF_GLOB_REF, though alisasedVar does
        // Similarly for clsVar
        if (oper == GT_ADDR && (op1->gtOper == GT_LCL_VAR || op1->gtOper == GT_CLS_VAR))
        {
            tree->gtFlags &= ~GTF_GLOB_REF;
        }
    } // if (op1)

    /*-------------------------------------------------------------------------
     * Process the second operand, if any
     */

    if (op2)
    {

#if LOCAL_ASSERTION_PROP
        // If we are entering the "else" part of a Qmark-Colon we must
        // reset the state of the current copy assignment table
        if (isQmarkColon)
        {
            noway_assert(optLocalAssertionProp);
            optAssertionReset(0);
            if (origAssertionCount)
            {
                size_t tabSize = origAssertionCount * sizeof(AssertionDsc);
                memcpy(optAssertionTabPrivate, origAssertionTab, tabSize);
                optAssertionReset(origAssertionCount);
            }
        }
#endif // LOCAL_ASSERTION_PROP

        // We might need a new MorphAddressContext context to use in evaluating op2.
        // (These are used to convey parent context about how addresses being calculated
        // will be used; see the specification comment for MorphAddrContext for full details.)
        // Assume it's an Ind context to start.
        switch (tree->gtOper)
        {
            case GT_ADD:
                if (mac != nullptr && mac->m_kind == MACK_Ind)
                {
                    GenTree* otherOp = tree->AsOp()->gtOp1;
                    // Is the other operator a constant?
                    if (otherOp->IsCnsIntOrI())
                    {
                        mac->m_totalOffset += otherOp->AsIntConCommon()->IconValue();
                    }
                    else
                    {
                        mac->m_allConstantOffsets = false;
                    }
                }
                break;
            default:
                break;
        }

        // If op2 is a GT_FIELD or indir, we must be taking its value,
        // so it should evaluate its address in a new context.
        if ((op2->gtOper == GT_FIELD) || op2->OperIsIndir())
        {
            // The impact of op2's value to any ongoing
            // address computation is handled above when looking
            // at op1.
            mac = nullptr;
        }

        tree->AsOp()->gtOp2 = op2 = fgMorphTree(op2, mac);

        /* Propagate the side effect flags from op2 */

        tree->gtFlags |= (op2->gtFlags & GTF_ALL_EFFECT);

#if LOCAL_ASSERTION_PROP
        // If we are exiting the "else" part of a Qmark-Colon we must
        // merge the state of the current copy assignment table with
        // that of the exit of the "then" part.
        if (isQmarkColon)
        {
            noway_assert(optLocalAssertionProp);
            // If either exit table has zero entries then
            // the merged table also has zero entries
            if (optAssertionCount == 0 || thenAssertionCount == 0)
            {
                optAssertionReset(0);
            }
            else
            {
                size_t tabSize = optAssertionCount * sizeof(AssertionDsc);
                if ((optAssertionCount != thenAssertionCount) ||
                    (memcmp(thenAssertionTab, optAssertionTabPrivate, tabSize) != 0))
                {
                    // Yes they are different so we have to find the merged set
                    // Iterate over the copy asgn table removing any entries
                    // that do not have an exact match in the thenAssertionTab
                    AssertionIndex index = 1;
                    while (index <= optAssertionCount)
                    {
                        AssertionDsc* curAssertion = optGetAssertion(index);

                        for (unsigned j = 0; j < thenAssertionCount; j++)
                        {
                            AssertionDsc* thenAssertion = &thenAssertionTab[j];

                            // Do the left sides match?
                            if ((curAssertion->op1.lcl.lclNum == thenAssertion->op1.lcl.lclNum) &&
                                (curAssertion->assertionKind == thenAssertion->assertionKind))
                            {
                                // Do the right sides match?
                                if ((curAssertion->op2.kind == thenAssertion->op2.kind) &&
                                    (curAssertion->op2.lconVal == thenAssertion->op2.lconVal))
                                {
                                    goto KEEP;
                                }
                                else
                                {
                                    goto REMOVE;
                                }
                            }
                        }
                    //
                    // If we fall out of the loop above then we didn't find
                    // any matching entry in the thenAssertionTab so it must
                    // have been killed on that path so we remove it here
                    //
                    REMOVE:
                        // The data at optAssertionTabPrivate[i] is to be removed
                        CLANG_FORMAT_COMMENT_ANCHOR;
#ifdef DEBUG
                        if (verbose)
                        {
                            printf("The QMARK-COLON ");
                            printTreeID(tree);
                            printf(" removes assertion candidate #%d\n", index);
                        }
#endif
                        optAssertionRemove(index);
                        continue;
                    KEEP:
                        // The data at optAssertionTabPrivate[i] is to be kept
                        index++;
                    }
                }
            }
        }
#endif // LOCAL_ASSERTION_PROP
    }  // if (op2)

DONE_MORPHING_CHILDREN:

    if (tree->OperIsIndirOrArrLength())
    {
        tree->SetIndirExceptionFlags(this);
    }
    else
    {
        if (tree->OperMayThrow(this))
        {
            // Mark the tree node as potentially throwing an exception
            tree->gtFlags |= GTF_EXCEPT;
        }
        else
        {
            if (((op1 == nullptr) || ((op1->gtFlags & GTF_EXCEPT) == 0)) &&
                ((op2 == nullptr) || ((op2->gtFlags & GTF_EXCEPT) == 0)))
            {
                tree->gtFlags &= ~GTF_EXCEPT;
            }
        }
    }

    if (tree->OperRequiresAsgFlag())
    {
        tree->gtFlags |= GTF_ASG;
    }
    else
    {
        if (((op1 == nullptr) || ((op1->gtFlags & GTF_ASG) == 0)) &&
            ((op2 == nullptr) || ((op2->gtFlags & GTF_ASG) == 0)))
        {
            tree->gtFlags &= ~GTF_ASG;
        }
    }

    if (tree->OperRequiresCallFlag(this))
    {
        tree->gtFlags |= GTF_CALL;
    }
    else
    {
        if (((op1 == nullptr) || ((op1->gtFlags & GTF_CALL) == 0)) &&
            ((op2 == nullptr) || ((op2->gtFlags & GTF_CALL) == 0)))
        {
            tree->gtFlags &= ~GTF_CALL;
        }
    }
    /*-------------------------------------------------------------------------
     * Now do POST-ORDER processing
     */

    if (varTypeIsGC(tree->TypeGet()) && (op1 && !varTypeIsGC(op1->TypeGet())) && (op2 && !varTypeIsGC(op2->TypeGet())))
    {
        // The tree is really not GC but was marked as such. Now that the
        // children have been unmarked, unmark the tree too.

        // Remember that GT_COMMA inherits it's type only from op2
        if (tree->gtOper == GT_COMMA)
        {
            tree->gtType = genActualType(op2->TypeGet());
        }
        else
        {
            tree->gtType = genActualType(op1->TypeGet());
        }
    }

    GenTree* oldTree = tree;

    GenTree* qmarkOp1 = nullptr;
    GenTree* qmarkOp2 = nullptr;

    if ((tree->OperGet() == GT_QMARK) && (tree->AsOp()->gtOp2->OperGet() == GT_COLON))
    {
        qmarkOp1 = oldTree->AsOp()->gtOp2->AsOp()->gtOp1;
        qmarkOp2 = oldTree->AsOp()->gtOp2->AsOp()->gtOp2;
    }

    // Try to fold it, maybe we get lucky,
    tree = gtFoldExpr(tree);

    if (oldTree != tree)
    {
        /* if gtFoldExpr returned op1 or op2 then we are done */
        if ((tree == op1) || (tree == op2) || (tree == qmarkOp1) || (tree == qmarkOp2))
        {
            return tree;
        }

        /* If we created a comma-throw tree then we need to morph op1 */
        if (fgIsCommaThrow(tree))
        {
            tree->AsOp()->gtOp1 = fgMorphTree(tree->AsOp()->gtOp1);
            fgMorphTreeDone(tree);
            return tree;
        }

        return tree;
    }
    else if (tree->OperKind() & GTK_CONST)
    {
        return tree;
    }

    /* gtFoldExpr could have used setOper to change the oper */
    oper = tree->OperGet();
    typ  = tree->TypeGet();

    /* gtFoldExpr could have changed op1 and op2 */
    op1 = tree->AsOp()->gtOp1;
    op2 = tree->gtGetOp2IfPresent();

    // Do we have an integer compare operation?
    //
    if (tree->OperIsCompare() && varTypeIsIntegralOrI(tree->TypeGet()))
    {
        // Are we comparing against zero?
        //
        if (op2->IsIntegralConst(0))
        {
            // Request that the codegen for op1 sets the condition flags
            // when it generates the code for op1.
            //
            // Codegen for op1 must set the condition flags if
            // this method returns true.
            //
            op1->gtRequestSetFlags();
        }
    }
    /*-------------------------------------------------------------------------
     * Perform the required oper-specific postorder morphing
     */

    GenTree*      cns1;
    GenTree*      cns2;
    size_t        ival1, ival2;
    GenTree*      effectiveOp1;
    FieldSeqNode* fieldSeq = nullptr;

    switch (oper)
    {
        case GT_ASG:
            effectiveOp1 = op1->gtEffectiveVal();

            if (effectiveOp1->OperIs(GT_IND, GT_OBJ, GT_BLK, GT_DYN_BLK))
            {
                effectiveOp1->gtFlags |= GTF_IND_ASG_LHS;
            }
            else if (effectiveOp1->OperIs(GT_CLS_VAR))
            {
                effectiveOp1->gtFlags |= GTF_CLS_VAR_ASG_LHS;
            }
            else
            {
                // TODO-MIKE-Review: Old code never checked for a LCL_VAR|FLD under
                // a COMMA so presumably it can't happen.
                assert(effectiveOp1 == op1);

                gtAssignSetVarDef(effectiveOp1);
            }

            // If we are storing a small type, we might be able to omit a cast.
            // We may also omit a cast when storing to a "normalize on load"
            // local since we know that a load from that local has to cast anyway.
            if (varTypeIsSmall(effectiveOp1->TypeGet()) &&
                (effectiveOp1->OperIs(GT_IND, GT_LCL_FLD) ||
                 (effectiveOp1->OperIs(GT_LCL_VAR) && lvaGetDesc(effectiveOp1->AsLclVar())->lvNormalizeOnLoad())))
            {
                if (!gtIsActiveCSE_Candidate(op2) && (op2->gtOper == GT_CAST) && !op2->gtOverflow())
                {
                    var_types castType = op2->CastToType();

                    // If we are performing a narrowing cast and
                    // castType is larger or the same as op1's type
                    // then we can discard the cast.

                    if (varTypeIsSmall(castType) && (genTypeSize(castType) >= genTypeSize(effectiveOp1->TypeGet())))
                    {
                        tree->AsOp()->gtOp2 = op2 = op2->AsCast()->CastOp();
                    }
                }
            }

            /* We can't CSE the LHS of an assignment */
            /* We also must set in the pre-morphing phase, otherwise assertionProp doesn't see it */
            if (op1->IsLocal() || (op1->TypeGet() != TYP_STRUCT))
            {
                op1->gtFlags |= GTF_DONT_CSE;
            }

            if (varTypeIsStruct(typ) && !op2->OperIs(GT_PHI))
            {
                return fgMorphStructAssignment(tree->AsOp());
            }

            break;

        case GT_RETURN:
            if (varTypeIsStruct(tree->GetType()))
            {
                // If we have merged returns then we only need to transform the one that returns
                // the merged return temp, the rest will be transformed into assignments to that
                // temp when block morphing is complete.

                if ((genReturnLocal == BAD_VAR_NUM) || ((tree->gtFlags & GTF_RET_MERGED) != 0))
                {
                    abiMorphStructReturn(tree->AsUnOp(), op1);
                    op1 = tree->AsUnOp()->GetOp(0);
                }
            }
            break;

        case GT_EQ:
        case GT_NE:

            /* Make sure we're allowed to do this */

            if (optValnumCSE_phase)
            {
                // It is not safe to reorder/delete CSE's
                break;
            }

            // Pattern-matching optimization:
            //    (a % c) ==/!= 0
            // for power-of-2 constant `c`
            // =>
            //    a & (c - 1) ==/!= 0
            // For integer `a`, even if negative.
            if (opts.OptimizationEnabled())
            {
                GenTree* op1 = tree->AsOp()->gtOp1;
                GenTree* op2 = tree->AsOp()->gtOp2;
                if (op1->OperIs(GT_MOD) && varTypeIsIntegral(op1->TypeGet()) && op2->IsIntegralConst(0))
                {
                    GenTree* op1op2 = op1->AsOp()->gtOp2;
                    if (op1op2->IsCnsIntOrI())
                    {
                        ssize_t modValue = op1op2->AsIntCon()->IconValue();
                        if (isPow2(modValue))
                        {
                            op1->SetOper(GT_AND);                                 // Change % => &
                            op1op2->AsIntConCommon()->SetIconValue(modValue - 1); // Change c => c - 1
                        }
                    }
                }
            }

            cns2 = op2;

            /* Check for "(expr +/- icon1) ==/!= (non-zero-icon2)" */

            if (cns2->gtOper == GT_CNS_INT && cns2->AsIntCon()->gtIconVal != 0)
            {
                op1 = tree->AsOp()->gtOp1;

                /* Since this can occur repeatedly we use a while loop */

                while ((op1->gtOper == GT_ADD || op1->gtOper == GT_SUB) && (op1->AsOp()->gtOp2->gtOper == GT_CNS_INT) &&
                       (op1->gtType == TYP_INT) && (op1->gtOverflow() == false))
                {
                    /* Got it; change "x+icon1==icon2" to "x==icon2-icon1" */

                    ival1 = op1->AsOp()->gtOp2->AsIntCon()->gtIconVal;
                    ival2 = cns2->AsIntCon()->gtIconVal;

                    if (op1->gtOper == GT_ADD)
                    {
                        ival2 -= ival1;
                    }
                    else
                    {
                        ival2 += ival1;
                    }
                    cns2->AsIntCon()->gtIconVal = ival2;

#ifdef TARGET_64BIT
                    // we need to properly re-sign-extend or truncate as needed.
                    cns2->AsIntCon()->TruncateOrSignExtend32();
#endif // TARGET_64BIT

                    op1 = tree->AsOp()->gtOp1 = op1->AsOp()->gtOp1;
                }
            }

            //
            // Here we look for the following tree
            //
            //                        EQ/NE
            //                        /  \.
            //                      op1   CNS 0/1
            //
            ival2 = INT_MAX; // The value of INT_MAX for ival2 just means that the constant value is not 0 or 1

            // cast to unsigned allows test for both 0 and 1
            if ((cns2->gtOper == GT_CNS_INT) && (((size_t)cns2->AsIntConCommon()->IconValue()) <= 1U))
            {
                ival2 = (size_t)cns2->AsIntConCommon()->IconValue();
            }
            else // cast to UINT64 allows test for both 0 and 1
                if ((cns2->gtOper == GT_CNS_LNG) && (((UINT64)cns2->AsIntConCommon()->LngValue()) <= 1ULL))
            {
                ival2 = (size_t)cns2->AsIntConCommon()->LngValue();
            }

            if (ival2 != INT_MAX)
            {
                // If we don't have a comma and relop, we can't do this optimization
                //
                if ((op1->gtOper == GT_COMMA) && (op1->AsOp()->gtOp2->OperIsCompare()))
                {
                    // Here we look for the following transformation
                    //
                    //                  EQ/NE                    Possible REVERSE(RELOP)
                    //                  /  \                           /      \.
                    //               COMMA CNS 0/1             ->   COMMA   relop_op2
                    //              /   \                          /    \.
                    //             x  RELOP                       x     relop_op1
                    //               /    \.
                    //         relop_op1  relop_op2
                    //
                    //
                    //
                    GenTree* comma = op1;
                    GenTree* relop = comma->AsOp()->gtOp2;

                    GenTree* relop_op1 = relop->AsOp()->gtOp1;

                    bool reverse = ((ival2 == 0) == (oper == GT_EQ));

                    if (reverse)
                    {
                        gtReverseCond(relop);
                    }

                    relop->AsOp()->gtOp1 = comma;
                    comma->AsOp()->gtOp2 = relop_op1;

                    // Comma now has fewer nodes underneath it, so we need to regenerate its flags
                    comma->gtFlags &= ~GTF_ALL_EFFECT;
                    comma->gtFlags |= (comma->AsOp()->gtOp1->gtFlags) & GTF_ALL_EFFECT;
                    comma->gtFlags |= (comma->AsOp()->gtOp2->gtFlags) & GTF_ALL_EFFECT;

                    noway_assert((relop->gtFlags & GTF_RELOP_JMP_USED) == 0);
                    noway_assert((relop->gtFlags & GTF_REVERSE_OPS) == 0);
                    relop->gtFlags |=
                        tree->gtFlags & (GTF_RELOP_JMP_USED | GTF_RELOP_QMARK | GTF_DONT_CSE | GTF_ALL_EFFECT);

                    return relop;
                }

                if (op1->gtOper == GT_COMMA)
                {
                    // Here we look for the following tree
                    // and when the LCL_VAR is a temp we can fold the tree:
                    //
                    //                        EQ/NE                  EQ/NE
                    //                        /  \                   /  \.
                    //                     COMMA  CNS 0/1  ->     RELOP CNS 0/1
                    //                     /   \                   / \.
                    //                   ASG  LCL_VAR
                    //                  /  \.
                    //           LCL_VAR   RELOP
                    //                      / \.
                    //

                    GenTree* asg = op1->AsOp()->gtOp1;
                    GenTree* lcl = op1->AsOp()->gtOp2;

                    /* Make sure that the left side of the comma is the assignment of the LCL_VAR */
                    if (asg->gtOper != GT_ASG)
                    {
                        goto SKIP;
                    }

                    /* The right side of the comma must be a LCL_VAR temp */
                    if (lcl->gtOper != GT_LCL_VAR)
                    {
                        goto SKIP;
                    }

                    unsigned lclNum = lcl->AsLclVarCommon()->GetLclNum();
                    noway_assert(lclNum < lvaCount);

                    /* If the LCL_VAR is not a temp then bail, a temp has a single def */
                    if (!lvaTable[lclNum].lvIsTemp)
                    {
                        goto SKIP;
                    }

                    /* If the LCL_VAR is a CSE temp then bail, it could have multiple defs/uses */
                    // Fix 383856 X86/ARM ILGEN
                    if (lclNumIsCSE(lclNum))
                    {
                        goto SKIP;
                    }

                    /* We also must be assigning the result of a RELOP */
                    if (asg->AsOp()->gtOp1->gtOper != GT_LCL_VAR)
                    {
                        goto SKIP;
                    }

                    /* Both of the LCL_VAR must match */
                    if (asg->AsOp()->gtOp1->AsLclVarCommon()->GetLclNum() != lclNum)
                    {
                        goto SKIP;
                    }

                    /* If right side of asg is not a RELOP then skip */
                    if (!asg->AsOp()->gtOp2->OperIsCompare())
                    {
                        goto SKIP;
                    }

                    /* Set op1 to the right side of asg, (i.e. the RELOP) */
                    op1 = asg->AsOp()->gtOp2;

                    DEBUG_DESTROY_NODE(asg->AsOp()->gtOp1);
                    DEBUG_DESTROY_NODE(lcl);
                }

                if (op1->OperIsCompare())
                {
                    // Here we look for the following tree
                    //
                    //                        EQ/NE           ->      RELOP/!RELOP
                    //                        /  \                       /    \.
                    //                     RELOP  CNS 0/1
                    //                     /   \.
                    //
                    // Note that we will remove/destroy the EQ/NE node and move
                    // the RELOP up into it's location.

                    /* Here we reverse the RELOP if necessary */

                    bool reverse = ((ival2 == 0) == (oper == GT_EQ));

                    if (reverse)
                    {
                        gtReverseCond(op1);
                    }

                    /* Propagate gtType of tree into op1 in case it is TYP_BYTE for setcc optimization */
                    op1->gtType = tree->gtType;

                    noway_assert((op1->gtFlags & GTF_RELOP_JMP_USED) == 0);
                    op1->gtFlags |= tree->gtFlags & (GTF_RELOP_JMP_USED | GTF_RELOP_QMARK | GTF_DONT_CSE);

                    DEBUG_DESTROY_NODE(tree);
                    return op1;
                }

                //
                // Now we check for a compare with the result of an '&' operator
                //
                // Here we look for the following transformation:
                //
                //                        EQ/NE                  EQ/NE
                //                        /  \                   /  \.
                //                      AND   CNS 0/1  ->      AND   CNS 0
                //                     /   \                  /   \.
                //                RSZ/RSH   CNS 1            x     CNS (1 << y)
                //                  /  \.
                //                 x   CNS_INT +y

                if (op1->gtOper == GT_AND)
                {
                    GenTree* andOp    = op1;
                    GenTree* rshiftOp = andOp->AsOp()->gtOp1;

                    if ((rshiftOp->gtOper != GT_RSZ) && (rshiftOp->gtOper != GT_RSH))
                    {
                        goto SKIP;
                    }

                    if (!rshiftOp->AsOp()->gtOp2->IsCnsIntOrI())
                    {
                        goto SKIP;
                    }

                    ssize_t shiftAmount = rshiftOp->AsOp()->gtOp2->AsIntCon()->gtIconVal;

                    if (shiftAmount < 0)
                    {
                        goto SKIP;
                    }

                    if (!andOp->AsOp()->gtOp2->IsIntegralConst(1))
                    {
                        goto SKIP;
                    }

                    if (andOp->gtType == TYP_INT)
                    {
                        if (shiftAmount > 31)
                        {
                            goto SKIP;
                        }

                        UINT32 newAndOperand = ((UINT32)1) << shiftAmount;

                        andOp->AsOp()->gtOp2->AsIntCon()->gtIconVal = newAndOperand;

                        // Reverse the cond if necessary
                        if (ival2 == 1)
                        {
                            gtReverseCond(tree);
                            cns2->AsIntCon()->gtIconVal = 0;
                            oper                        = tree->gtOper;
                        }
                    }
                    else if (andOp->gtType == TYP_LONG)
                    {
                        if (shiftAmount > 63)
                        {
                            goto SKIP;
                        }

                        UINT64 newAndOperand = ((UINT64)1) << shiftAmount;

                        andOp->AsOp()->gtOp2->AsIntConCommon()->SetLngValue(newAndOperand);

                        // Reverse the cond if necessary
                        if (ival2 == 1)
                        {
                            gtReverseCond(tree);
                            cns2->AsIntConCommon()->SetLngValue(0);
                            oper = tree->gtOper;
                        }
                    }

                    andOp->AsOp()->gtOp1 = rshiftOp->AsOp()->gtOp1;

                    DEBUG_DESTROY_NODE(rshiftOp->AsOp()->gtOp2);
                    DEBUG_DESTROY_NODE(rshiftOp);
                }
            } // END if (ival2 != INT_MAX)

        SKIP:
            /* Now check for compares with small constant longs that can be cast to int */

            if (!cns2->OperIsConst())
            {
                goto COMPARE;
            }

            if (cns2->TypeGet() != TYP_LONG)
            {
                goto COMPARE;
            }

            /* Is the constant 31 bits or smaller? */

            if ((cns2->AsIntConCommon()->LngValue() >> 31) != 0)
            {
                goto COMPARE;
            }

            /* Is the first comparand mask operation of type long ? */

            if (op1->gtOper != GT_AND)
            {
                /* Another interesting case: cast from int */

                if (op1->gtOper == GT_CAST && op1->CastFromType() == TYP_INT &&
                    !gtIsActiveCSE_Candidate(op1) && // op1 cannot be a CSE candidate
                    !op1->gtOverflow())              // cannot be an overflow checking cast
                {
                    /* Simply make this into an integer comparison */

                    tree->AsOp()->gtOp1 = op1->AsCast()->CastOp();
                    tree->AsOp()->gtOp2 = gtNewIconNode((int)cns2->AsIntConCommon()->LngValue(), TYP_INT);
                }

                goto COMPARE;
            }

            noway_assert(op1->TypeGet() == TYP_LONG && op1->OperGet() == GT_AND);

            /* Is the result of the mask effectively an INT ? */

            GenTree* andMask;
            andMask = op1->AsOp()->gtOp2;
            if (andMask->gtOper != GT_CNS_NATIVELONG)
            {
                goto COMPARE;
            }
            if ((andMask->AsIntConCommon()->LngValue() >> 32) != 0)
            {
                goto COMPARE;
            }

            /* Now we know that we can cast AsOp()->gtOp1 of AND to int */

            op1->AsOp()->gtOp1 = gtNewCastNode(TYP_INT, op1->AsOp()->gtOp1, false, TYP_INT);

            /* now replace the mask node (AsOp()->gtOp2 of AND node) */

            noway_assert(andMask == op1->AsOp()->gtOp2);

            ival1 = (int)andMask->AsIntConCommon()->LngValue();
            andMask->SetOper(GT_CNS_INT);
            andMask->gtType                = TYP_INT;
            andMask->AsIntCon()->gtIconVal = ival1;

            /* now change the type of the AND node */

            op1->gtType = TYP_INT;

            /* finally we replace the comparand */

            ival2 = (int)cns2->AsIntConCommon()->LngValue();
            cns2->SetOper(GT_CNS_INT);
            cns2->gtType = TYP_INT;

            noway_assert(cns2 == op2);
            cns2->AsIntCon()->gtIconVal = ival2;

            goto COMPARE;

        case GT_LT:
        case GT_LE:
        case GT_GE:
        case GT_GT:

            if (op2->gtOper == GT_CNS_INT)
            {
                cns2 = op2;
                /* Check for "expr relop 1" */
                if (cns2->IsIntegralConst(1))
                {
                    /* Check for "expr >= 1" */
                    if (oper == GT_GE)
                    {
                        /* Change to "expr != 0" for unsigned and "expr > 0" for signed */
                        oper = (tree->IsUnsigned()) ? GT_NE : GT_GT;
                        goto SET_OPER;
                    }
                    /* Check for "expr < 1" */
                    else if (oper == GT_LT)
                    {
                        /* Change to "expr == 0" for unsigned and "expr <= 0" for signed */
                        oper = (tree->IsUnsigned()) ? GT_EQ : GT_LE;
                        goto SET_OPER;
                    }
                }
                /* Check for "expr relop -1" */
                else if (!tree->IsUnsigned() && cns2->IsIntegralConst(-1))
                {
                    /* Check for "expr <= -1" */
                    if (oper == GT_LE)
                    {
                        /* Change to "expr < 0" */
                        oper = GT_LT;
                        goto SET_OPER;
                    }
                    /* Check for "expr > -1" */
                    else if (oper == GT_GT)
                    {
                        /* Change to "expr >= 0" */
                        oper = GT_GE;

                    SET_OPER:
                        // IF we get here we should be changing 'oper'
                        assert(tree->OperGet() != oper);

                        // Keep the old ValueNumber for 'tree' as the new expr
                        // will still compute the same value as before
                        tree->SetOper(oper, GenTree::PRESERVE_VN);
                        cns2->AsIntCon()->gtIconVal = 0;

                        // vnStore is null before the ValueNumber phase has run
                        if (vnStore != nullptr)
                        {
                            // Update the ValueNumber for 'cns2', as we just changed it to 0
                            fgValueNumberTreeConst(cns2);
                        }
                        op2 = tree->AsOp()->gtOp2 = gtFoldExpr(op2);
                    }
                }
                else if (tree->IsUnsigned() && op2->IsIntegralConst(0))
                {
                    if ((oper == GT_GT) || (oper == GT_LE))
                    {
                        // IL doesn't have a cne instruction so compilers use cgt.un instead. The JIT
                        // recognizes certain patterns that involve GT_NE (e.g (x & 4) != 0) and fails
                        // if GT_GT is used instead. Transform (x GT_GT.unsigned 0) into (x GT_NE 0)
                        // and (x GT_LE.unsigned 0) into (x GT_EQ 0). The later case is rare, it sometimes
                        // occurs as a result of branch inversion.
                        oper = (oper == GT_LE) ? GT_EQ : GT_NE;
                        tree->SetOper(oper, GenTree::PRESERVE_VN);
                        tree->gtFlags &= ~GTF_UNSIGNED;
                    }
                }
            }

        COMPARE:

            noway_assert(tree->OperKind() & GTK_RELOP);
            break;

        case GT_MUL:
#ifndef TARGET_64BIT
            if (typ == TYP_LONG)
            {
                // This must be GTF_MUL_64RSLT
                assert(tree->gtIsValid64RsltMul());
                return tree;
            }
#endif
            goto CM_OVF_OP;

        case GT_SUB:

            if (tree->gtOverflow())
            {
                goto CM_OVF_OP;
            }

            // TODO #4104: there are a lot of other places where
            // this condition is not checked before transformations.
            if (fgGlobalMorph)
            {
                /* Check for "op1 - cns2" , we change it to "op1 + (-cns2)" */

                noway_assert(op2);
                if (op2->IsIntCon() && !op2->IsIconHandle())
                {
                    // Negate the constant and change the node to be "+",
                    // except when `op2` is a const byref.

                    op2->AsIntCon()->SetValue(-op2->AsIntCon()->GetValue());
                    oper = GT_ADD;
                    tree->ChangeOper(oper);
                    goto CM_ADD_OP;
                }

                /* Check for "cns1 - op2" , we change it to "(cns1 + (-op2))" */

                noway_assert(op1);
                if (op1->IsCnsIntOrI())
                {
                    noway_assert(varTypeIsIntOrI(tree));

                    // The type of the new GT_NEG node cannot just be op2->TypeGet().
                    // Otherwise we may sign-extend incorrectly in cases where the GT_NEG
                    // node ends up feeding directly into a cast, for example in
                    // GT_CAST<ubyte>(GT_SUB(0, s_1.ubyte))
                    tree->AsOp()->gtOp2 = op2 = gtNewOperNode(GT_NEG, genActualType(op2->TypeGet()), op2);
                    fgMorphTreeDone(op2);

                    oper = GT_ADD;
                    tree->ChangeOper(oper);
                    goto CM_ADD_OP;
                }

                /* No match - exit */
            }

            // Skip optimization if non-NEG operand is constant.
            // Both op1 and op2 are not constant because it was already checked above.
            if (opts.OptimizationEnabled() && fgGlobalMorph &&
                (((op1->gtFlags & GTF_EXCEPT) == 0) || ((op2->gtFlags & GTF_EXCEPT) == 0)))
            {
                // a - -b = > a + b
                // SUB(a, (NEG(b)) => ADD(a, b)

                if (!op1->OperIs(GT_NEG) && op2->OperIs(GT_NEG))
                {
                    // tree: SUB
                    // op1: a
                    // op2: NEG
                    // op2Child: b

                    GenTree* op2Child = op2->AsOp()->gtOp1; // b
                    oper              = GT_ADD;
                    tree->SetOper(oper, GenTree::PRESERVE_VN);
                    tree->AsOp()->gtOp2 = op2Child;

                    DEBUG_DESTROY_NODE(op2);

                    op2 = op2Child;
                }

                // -a - -b = > b - a
                // SUB(NEG(a), (NEG(b)) => SUB(b, a)

                if (op1->OperIs(GT_NEG) && op2->OperIs(GT_NEG))
                {
                    // tree: SUB
                    // op1: NEG
                    // op1Child: a
                    // op2: NEG
                    // op2Child: b

                    GenTree* op1Child   = op1->AsOp()->gtOp1; // a
                    GenTree* op2Child   = op2->AsOp()->gtOp1; // b
                    tree->AsOp()->gtOp1 = op2Child;
                    tree->AsOp()->gtOp2 = op1Child;

                    DEBUG_DESTROY_NODE(op1);
                    DEBUG_DESTROY_NODE(op2);

                    op1 = op2Child;
                    op2 = op1Child;
                }
            }

            break;

#ifdef TARGET_ARM64
        case GT_DIV:
            if (!varTypeIsFloating(tree->gtType))
            {
                // Codegen for this instruction needs to be able to throw two exceptions:
                fgAddCodeRef(compCurBB, bbThrowIndex(compCurBB), SCK_OVERFLOW);
                fgAddCodeRef(compCurBB, bbThrowIndex(compCurBB), SCK_DIV_BY_ZERO);
            }
            break;
        case GT_UDIV:
            // Codegen for this instruction needs to be able to throw one exception:
            fgAddCodeRef(compCurBB, bbThrowIndex(compCurBB), SCK_DIV_BY_ZERO);
            break;
#endif

        case GT_ADD:

        CM_OVF_OP:
            if (tree->gtOverflow())
            {
                tree->gtRequestSetFlags();

                // Add the excptn-throwing basic block to jump to on overflow

                fgAddCodeRef(compCurBB, bbThrowIndex(compCurBB), SCK_OVERFLOW);

                // We can't do any commutative morphing for overflow instructions

                break;
            }

        CM_ADD_OP:

        case GT_OR:
        case GT_XOR:
        case GT_AND:

            /* Commute any non-REF constants to the right */

            noway_assert(op1);
            if (op1->OperIsConst() && (op1->gtType != TYP_REF))
            {
                // TODO-Review: We used to assert here that
                // noway_assert(!op2->OperIsConst() || !opts.OptEnabled(CLFLG_CONSTANTFOLD));
                // With modifications to AddrTaken==>AddrExposed, we did more assertion propagation,
                // and would sometimes hit this assertion.  This may indicate a missed "remorph".
                // Task is to re-enable this assertion and investigate.

                /* Swap the operands */
                tree->AsOp()->gtOp1 = op2;
                tree->AsOp()->gtOp2 = op1;

                op1 = op2;
                op2 = tree->AsOp()->gtOp2;
            }

            // Fold "cmp & 1" to just "cmp"
            if (tree->OperIs(GT_AND) && tree->TypeIs(TYP_INT) && op1->OperIsCompare() && op2->IsIntegralConst(1) &&
                !gtIsActiveCSE_Candidate(tree) && !gtIsActiveCSE_Candidate(op2))
            {
                DEBUG_DESTROY_NODE(op2);
                DEBUG_DESTROY_NODE(tree);
                return op1;
            }

            // See if we can fold floating point operations (can regress minopts mode)
            if (opts.OptimizationEnabled() && varTypeIsFloating(tree->TypeGet()) && !optValnumCSE_phase)
            {
                if ((oper == GT_MUL) && !op1->IsCnsFltOrDbl() && op2->IsCnsFltOrDbl())
                {
                    if (op2->AsDblCon()->gtDconVal == 2.0)
                    {
                        bool needsComma = !op1->OperIsLeaf() && !op1->IsLocal();
                        // if op1 is not a leaf/local we have to introduce a temp via GT_COMMA.
                        // Unfortunately, it's not optHoistLoopCode-friendly yet so let's do it later.
                        if (!needsComma || (fgOrder == FGOrderLinear))
                        {
                            // Fold "x*2.0" to "x+x"
                            op2  = fgMakeMultiUse(&tree->AsOp()->gtOp1);
                            op1  = tree->AsOp()->gtOp1;
                            oper = GT_ADD;
                            tree = gtNewOperNode(oper, tree->TypeGet(), op1, op2);
                            INDEBUG(tree->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
                        }
                    }
                    else if (op2->AsDblCon()->gtDconVal == 1.0)
                    {
                        // Fold "x*1.0" to "x"
                        DEBUG_DESTROY_NODE(op2);
                        DEBUG_DESTROY_NODE(tree);
                        return op1;
                    }
                }
            }

            /* See if we can fold GT_ADD nodes. */

            if (oper == GT_ADD)
            {
                /* Fold "((x+icon1)+(y+icon2)) to ((x+y)+(icon1+icon2))" */

                if (op1->gtOper == GT_ADD && op2->gtOper == GT_ADD && !gtIsActiveCSE_Candidate(op2) &&
                    op1->AsOp()->gtOp2->gtOper == GT_CNS_INT && op2->AsOp()->gtOp2->gtOper == GT_CNS_INT &&
                    !op1->gtOverflow() && !op2->gtOverflow())
                {
                    // Don't create a byref pointer that may point outside of the ref object.
                    // If a GC happens, the byref won't get updated. This can happen if one
                    // of the int components is negative. It also requires the address generation
                    // be in a fully-interruptible code region.
                    if (!varTypeIsGC(op1->AsOp()->gtOp1->TypeGet()) && !varTypeIsGC(op2->AsOp()->gtOp1->TypeGet()))
                    {
                        cns1 = op1->AsOp()->gtOp2;
                        cns2 = op2->AsOp()->gtOp2;
                        cns1->AsIntCon()->gtIconVal += cns2->AsIntCon()->gtIconVal;
#ifdef TARGET_64BIT
                        if (cns1->TypeGet() == TYP_INT)
                        {
                            // we need to properly re-sign-extend or truncate after adding two int constants above
                            cns1->AsIntCon()->TruncateOrSignExtend32();
                        }
#endif // TARGET_64BIT

                        tree->AsOp()->gtOp2 = cns1;
                        DEBUG_DESTROY_NODE(cns2);

                        op1->AsOp()->gtOp2 = op2->AsOp()->gtOp1;
                        op1->gtFlags |= (op1->AsOp()->gtOp2->gtFlags & GTF_ALL_EFFECT);
                        DEBUG_DESTROY_NODE(op2);
                        op2 = tree->AsOp()->gtOp2;
                    }
                }

                if (op2->IsCnsIntOrI() && varTypeIsIntegralOrI(typ))
                {
                    CLANG_FORMAT_COMMENT_ANCHOR;

                    // Fold (x + 0).

                    if ((op2->AsIntConCommon()->IconValue() == 0) && !gtIsActiveCSE_Candidate(tree))
                    {

                        // If this addition is adding an offset to a null pointer,
                        // avoid the work and yield the null pointer immediately.
                        // Dereferencing the pointer in either case will have the
                        // same effect.

                        if (!optValnumCSE_phase && varTypeIsGC(op2->TypeGet()) &&
                            ((op1->gtFlags & GTF_ALL_EFFECT) == 0))
                        {
                            op2->gtType = tree->gtType;
                            DEBUG_DESTROY_NODE(op1);
                            DEBUG_DESTROY_NODE(tree);
                            return op2;
                        }

                        // Remove the addition iff it won't change the tree type
                        // to TYP_REF.

                        if (!gtIsActiveCSE_Candidate(op2) &&
                            ((op1->TypeGet() == tree->TypeGet()) || (op1->TypeGet() != TYP_REF)))
                        {
                            if (fgGlobalMorph && op2->IsIntCon() && (op2->AsIntCon()->GetFieldSeq() != nullptr) &&
                                (op2->AsIntCon()->GetFieldSeq() != FieldSeqStore::NotAField()))
                            {
                                fgAddFieldSeqForZeroOffset(op1, op2->AsIntCon()->GetFieldSeq());
                            }

                            DEBUG_DESTROY_NODE(op2);
                            DEBUG_DESTROY_NODE(tree);

                            return op1;
                        }
                    }
                }

                if (opts.OptimizationEnabled() && fgGlobalMorph &&
                    (((op1->gtFlags & GTF_EXCEPT) == 0) || ((op2->gtFlags & GTF_EXCEPT) == 0)))
                {
                    // - a + b = > b - a
                    // ADD((NEG(a), b) => SUB(b, a)

                    // Skip optimization if non-NEG operand is constant.
                    if (op1->OperIs(GT_NEG) && !op2->OperIs(GT_NEG) &&
                        !(op2->IsCnsIntOrI() && varTypeIsIntegralOrI(typ)))
                    {
                        // tree: ADD
                        // op1: NEG
                        // op2: b
                        // op1Child: a

                        GenTree* op1Child = op1->AsOp()->gtOp1; // a
                        oper              = GT_SUB;
                        tree->SetOper(oper, GenTree::PRESERVE_VN);
                        tree->AsOp()->gtOp1 = op2;
                        tree->AsOp()->gtOp2 = op1Child;

                        DEBUG_DESTROY_NODE(op1);

                        op1 = op2;
                        op2 = op1Child;
                    }

                    // a + -b = > a - b
                    // ADD(a, (NEG(b)) => SUB(a, b)

                    if (!op1->OperIs(GT_NEG) && op2->OperIs(GT_NEG))
                    {
                        // a is non cosntant because it was already canonicalized to have
                        // variable on the left and constant on the right.

                        // tree: ADD
                        // op1: a
                        // op2: NEG
                        // op2Child: b

                        GenTree* op2Child = op2->AsOp()->gtOp1; // a
                        oper              = GT_SUB;
                        tree->SetOper(oper, GenTree::PRESERVE_VN);
                        tree->AsOp()->gtOp2 = op2Child;

                        DEBUG_DESTROY_NODE(op2);

                        op2 = op2Child;
                    }
                }
            }
            /* See if we can fold GT_MUL by const nodes */
            else if (oper == GT_MUL && op2->IsCnsIntOrI() && !optValnumCSE_phase)
            {
#ifndef TARGET_64BIT
                noway_assert(typ <= TYP_UINT);
#endif // TARGET_64BIT
                noway_assert(!tree->gtOverflow());

                ssize_t mult = op2->AsIntConCommon()->IconValue();

                if (mult == 0)
                {
                    // We may be able to throw away op1 (unless it has side-effects)

                    if ((op1->gtFlags & GTF_SIDE_EFFECT) == 0)
                    {
                        DEBUG_DESTROY_NODE(op1);
                        DEBUG_DESTROY_NODE(tree);
                        return op2; // Just return the "0" node
                    }

                    // We need to keep op1 for the side-effects. Hang it off
                    // a GT_COMMA node

                    tree->ChangeOper(GT_COMMA);
                    return tree;
                }

                size_t abs_mult      = (mult >= 0) ? mult : -mult;
                size_t lowestBit     = genFindLowestBit(abs_mult);
                bool   changeToShift = false;

                // is it a power of two? (positive or negative)
                if (abs_mult == lowestBit)
                {
                    // if negative negate (min-int does not need negation)
                    if (mult < 0 && mult != SSIZE_T_MIN)
                    {
                        // The type of the new GT_NEG node cannot just be op1->TypeGet().
                        // Otherwise we may sign-extend incorrectly in cases where the GT_NEG
                        // node ends up feeding directly a cast, for example in
                        // GT_CAST<ubyte>(GT_MUL(-1, s_1.ubyte))
                        tree->AsOp()->gtOp1 = op1 = gtNewOperNode(GT_NEG, genActualType(op1->TypeGet()), op1);
                        fgMorphTreeDone(op1);
                    }

                    if (abs_mult == 1)
                    {
                        DEBUG_DESTROY_NODE(op2);
                        DEBUG_DESTROY_NODE(tree);
                        return op1;
                    }

                    /* Change the multiplication into a shift by log2(val) bits */
                    op2->AsIntConCommon()->SetIconValue(genLog2(abs_mult));
                    changeToShift = true;
                }
#if LEA_AVAILABLE
                else if ((lowestBit > 1) && jitIsScaleIndexMul(lowestBit) && optAvoidIntMult())
                {
                    int     shift  = genLog2(lowestBit);
                    ssize_t factor = abs_mult >> shift;

                    if (factor == 3 || factor == 5 || factor == 9)
                    {
                        // if negative negate (min-int does not need negation)
                        if (mult < 0 && mult != SSIZE_T_MIN)
                        {
                            tree->AsOp()->gtOp1 = op1 = gtNewOperNode(GT_NEG, genActualType(op1->TypeGet()), op1);
                            fgMorphTreeDone(op1);
                        }

                        GenTree* factorIcon = gtNewIconNode(factor, TYP_I_IMPL);

                        // change the multiplication into a smaller multiplication (by 3, 5 or 9) and a shift
                        tree->AsOp()->gtOp1 = op1 = gtNewOperNode(GT_MUL, tree->gtType, op1, factorIcon);
                        fgMorphTreeDone(op1);

                        op2->AsIntConCommon()->SetIconValue(shift);
                        changeToShift = true;
                    }
                }
#endif // LEA_AVAILABLE
                if (changeToShift)
                {
                    // vnStore is null before the ValueNumber phase has run
                    if (vnStore != nullptr)
                    {
                        // Update the ValueNumber for 'op2', as we just changed the constant
                        fgValueNumberTreeConst(op2);
                    }
                    oper = GT_LSH;
                    // Keep the old ValueNumber for 'tree' as the new expr
                    // will still compute the same value as before
                    tree->ChangeOper(oper, GenTree::PRESERVE_VN);

                    goto DONE_MORPHING_CHILDREN;
                }
            }
            else if (fgOperIsBitwiseRotationRoot(oper))
            {
                tree = fgRecognizeAndMorphBitwiseRotation(tree);

                // fgRecognizeAndMorphBitwiseRotation may return a new tree
                oper = tree->OperGet();
                typ  = tree->TypeGet();
                op1  = tree->AsOp()->gtOp1;
                op2  = tree->AsOp()->gtOp2;
            }

            if (varTypeIsIntegralOrI(tree->TypeGet()) && tree->OperIs(GT_ADD, GT_MUL, GT_AND, GT_OR, GT_XOR))
            {
                GenTree* foldedTree = fgMorphAssociative(tree->AsOp());
                if (foldedTree != nullptr)
                {
                    tree = foldedTree;
                    op1  = tree->gtGetOp1();
                    op2  = tree->gtGetOp2();
                    if (!tree->OperIs(oper))
                    {
                        return tree;
                    }
                }
            }

            break;

        case GT_NOT:
        case GT_NEG:
            // Remove double negation/not.
            // Note: this is not a safe tranformation if "tree" is a CSE candidate.
            // Consider for example the following expression: NEG(NEG(OP)), where the top-level
            // NEG is a CSE candidate. Were we to morph this to just OP, CSE would fail to find
            // the original NEG in the statement.
            if (op1->OperIs(oper) && opts.OptimizationEnabled() && !gtIsActiveCSE_Candidate(tree))
            {
                GenTree* child = op1->AsOp()->gtGetOp1();
                return child;
            }

            // Distribute negation over simple multiplication/division expressions
            if (opts.OptimizationEnabled() && !optValnumCSE_phase && tree->OperIs(GT_NEG) &&
                op1->OperIs(GT_MUL, GT_DIV))
            {
                GenTreeOp* mulOrDiv = op1->AsOp();
                GenTree*   op1op1   = mulOrDiv->gtGetOp1();
                GenTree*   op1op2   = mulOrDiv->gtGetOp2();

                if (!op1op1->IsCnsIntOrI() && op1op2->IsCnsIntOrI() && !op1op2->IsIconHandle())
                {
                    // NEG(MUL(a, C)) => MUL(a, -C)
                    // NEG(DIV(a, C)) => DIV(a, -C), except when C = {-1, 1}
                    ssize_t constVal = op1op2->AsIntCon()->IconValue();
                    if ((mulOrDiv->OperIs(GT_DIV) && (constVal != -1) && (constVal != 1)) ||
                        (mulOrDiv->OperIs(GT_MUL) && !mulOrDiv->gtOverflow()))
                    {
                        GenTree* newOp1 = op1op1;                                      // a
                        GenTree* newOp2 = gtNewIconNode(-constVal, op1op2->TypeGet()); // -C
                        mulOrDiv->gtOp1 = newOp1;
                        mulOrDiv->gtOp2 = newOp2;

                        DEBUG_DESTROY_NODE(tree);
                        DEBUG_DESTROY_NODE(op1op2);

                        return mulOrDiv;
                    }
                }
            }

            /* Any constant cases should have been folded earlier */
            noway_assert(!op1->OperIsConst() || !opts.OptEnabled(CLFLG_CONSTANTFOLD) || optValnumCSE_phase);
            break;

        case GT_CKFINITE:

            noway_assert(varTypeIsFloating(op1->TypeGet()));

            fgAddCodeRef(compCurBB, bbThrowIndex(compCurBB), SCK_ARITH_EXCPN);
            break;

        case GT_OBJ:
            // If we have GT_OBJ(GT_ADDR(X)) and X has GTF_GLOB_REF, we must set GTF_GLOB_REF on
            // the GT_OBJ. Note that the GTF_GLOB_REF will have been cleared on ADDR(X) where X
            // is a local or clsVar, even if it has been address-exposed.
            if (op1->OperGet() == GT_ADDR)
            {
                tree->gtFlags |= (op1->gtGetOp1()->gtFlags & GTF_GLOB_REF);
            }
            break;

        case GT_IND:
        {
            // Can not remove a GT_IND if it is currently a CSE candidate.
            if (gtIsActiveCSE_Candidate(tree))
            {
                break;
            }

#ifdef TARGET_ARM
            // Check for a misalignment floating point indirection.
            // TODO-MIKE-Cleanup: This should be moved to lowering
            // (or perhaps decomposition to deal with DOUBLE).
            if (op1->OperIs(GT_ADD) && varTypeIsFloating(typ))
            {
                GenTree* addOp2 = op1->AsOp()->gtGetOp2();
                if (addOp2->IsCnsIntOrI())
                {
                    ssize_t offset = addOp2->AsIntCon()->gtIconVal;
                    if ((offset % emitTypeSize(TYP_FLOAT)) != 0)
                    {
                        tree->gtFlags |= GTF_IND_UNALIGNED;
                    }
                }
            }
#endif // TARGET_ARM

            // Only do this optimization when we are in the global optimizer. Doing this after value numbering
            // could result in an invalid value number for the newly generated GT_IND node.
            if ((op1->OperGet() == GT_COMMA) && fgGlobalMorph)
            {
                // Perform the transform IND(COMMA(x, ..., z)) == COMMA(x, ..., IND(z)).
                // TBD: this transformation is currently necessary for correctness -- it might
                // be good to analyze the failures that result if we don't do this, and fix them
                // in other ways.  Ideally, this should be optional.
                GenTree*     commaNode = op1;
                GenTreeFlags treeFlags = tree->gtFlags;
                commaNode->gtType      = typ;
                commaNode->gtFlags     = (treeFlags & ~GTF_REVERSE_OPS); // Bashing the GT_COMMA flags here is
                                                                         // dangerous, clear the GTF_REVERSE_OPS at
                                                                         // least.

                INDEBUG(commaNode->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)

                while (commaNode->AsOp()->gtOp2->gtOper == GT_COMMA)
                {
                    commaNode         = commaNode->AsOp()->gtOp2;
                    commaNode->gtType = typ;
                    commaNode->gtFlags =
                        (treeFlags & ~GTF_REVERSE_OPS & ~GTF_ASG & ~GTF_CALL); // Bashing the GT_COMMA flags here is
                    // dangerous, clear the GTF_REVERSE_OPS, GT_ASG, and GT_CALL at
                    // least.
                    commaNode->gtFlags |= ((commaNode->AsOp()->gtOp1->gtFlags | commaNode->AsOp()->gtOp2->gtFlags) &
                                           (GTF_ASG | GTF_CALL));

                    INDEBUG(commaNode->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)
                }

                tree          = op1;
                GenTree* addr = commaNode->AsOp()->gtOp2;
                op1           = gtNewIndir(typ, addr);
                // This is very conservative
                op1->gtFlags |= treeFlags & ~GTF_ALL_EFFECT & ~GTF_IND_NONFAULTING;
                op1->gtFlags |= (addr->gtFlags & GTF_ALL_EFFECT);

                INDEBUG(op1->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)

                commaNode->AsOp()->gtOp2 = op1;
                commaNode->gtFlags |= (op1->gtFlags & GTF_ALL_EFFECT);
                return tree;
            }

            break;
        }

        case GT_ADDR:

            // Can not remove op1 if it is currently a CSE candidate.
            if (gtIsActiveCSE_Candidate(op1))
            {
                break;
            }

            if (op1->OperIs(GT_IND, GT_OBJ))
            {
                if ((op1->gtFlags & GTF_IND_ARR_INDEX) == 0)
                {
                    // Can not remove a GT_ADDR if it is currently a CSE candidate.
                    if (gtIsActiveCSE_Candidate(tree))
                    {
                        break;
                    }

                    // Perform the transform ADDR(IND(...)) == (...).
                    GenTree* addr = op1->AsIndir()->GetAddr();

                    // If tree has a zero field sequence annotation, update the annotation
                    // on addr node.
                    FieldSeqNode* zeroFieldSeq = nullptr;
                    if (GetZeroOffsetFieldMap()->Lookup(tree, &zeroFieldSeq))
                    {
                        fgAddFieldSeqForZeroOffset(addr, zeroFieldSeq);
                    }

                    noway_assert(varTypeIsGC(addr->gtType) || addr->gtType == TYP_I_IMPL);

                    DEBUG_DESTROY_NODE(op1);
                    DEBUG_DESTROY_NODE(tree);

                    return addr;
                }
            }
            else if (op1->OperIs(GT_COMMA) && !optValnumCSE_phase)
            {
                // Perform the transform ADDR(COMMA(x, ..., z)) == COMMA(x, ..., ADDR(z)).

                ArrayStack<GenTreeOp*> commas(getAllocator(CMK_ArrayStack));
                for (GenTree* comma = op1; comma->OperIs(GT_COMMA); comma = comma->AsOp()->GetOp(1))
                {
                    commas.Push(comma->AsOp());
                }

                GenTreeOp* lastComma = commas.Top();
                GenTree*   location  = lastComma->GetOp(1);
                GenTree*   addr      = nullptr;

                if (location->OperIs(GT_OBJ))
                {
                    location->SetOper(GT_IND);
                }

                if (location->OperIs(GT_IND))
                {
                    if ((location->gtFlags & GTF_IND_ARR_INDEX) == 0)
                    {
                        addr = location->AsIndir()->GetAddr();

                        // The morphed ADDR might be annotated with a zero offset field sequence.
                        FieldSeqNode* zeroFieldSeq = nullptr;
                        if (GetZeroOffsetFieldMap()->Lookup(tree, &zeroFieldSeq))
                        {
                            fgAddFieldSeqForZeroOffset(addr, zeroFieldSeq);
                        }
                    }
                    else
                    {
                        // If the node we're about to put under a GT_ADDR is an indirection, it doesn't
                        // need to be materialized, since we only want its address. Because of this, the
                        // GT_IND is not a faulting indirection.

                        // TODO: the flag update below is conservative and can be improved.
                        // For example, if we made the ADDR(IND(x)) == x transformation, we may be able to
                        // get rid of some other IND flags (e.g., GTF_GLOB_REF).

                        location->gtFlags |= GTF_IND_NONFAULTING;
                        location->gtFlags &= ~GTF_EXCEPT;
                        location->gtFlags |= (location->AsIndir()->GetAddr()->gtFlags & GTF_EXCEPT);
                    }
                }

                if (addr == nullptr)
                {
                    addr = tree;
                    addr->AsUnOp()->SetOp(0, location);
                    addr->SetSideEffects(location->GetSideEffects());

                    location->SetDoNotCSE();
                }

                lastComma->SetOp(1, addr);

                // TODO-MIKE-Cleanup: Like the similar transform in fgMorphStructComma, this doesn't update
                // value numbers on COMMAs. It's likely that this transform doesn't happen past global
                // morph so probabily this doesn't matter too much.

                // TODO-MIKE-CQ: The first COMMA has GTF_DONT_CSE set because it's under ADDR.
                // GTF_DONT_CSE is no longer necessary and should be removed.

                while (!commas.Empty())
                {
                    GenTreeOp* comma = commas.Pop();
                    comma->SetType(addr->GetType());
                    comma->SetSideEffects(comma->GetOp(0)->GetSideEffects() | comma->GetOp(1)->GetSideEffects());
                }

                return op1;
            }
            break;

        case GT_COLON:
            if (fgGlobalMorph)
            {
                /* Mark the nodes that are conditionally executed */
                fgWalkTreePre(&tree, gtMarkColonCond);
            }
            /* Since we're doing this postorder we clear this if it got set by a child */
            fgRemoveRestOfBlock = false;
            break;

        case GT_COMMA:

            /* Special case: trees that don't produce a value */
            if (op2->OperIs(GT_ASG) || (op2->OperGet() == GT_COMMA && op2->TypeGet() == TYP_VOID) || fgIsThrow(op2))
            {
                typ = tree->gtType = TYP_VOID;
            }

            // If we are in the Valuenum CSE phase then don't morph away anything as these
            // nodes may have CSE defs/uses in them.
            //
            if (!optValnumCSE_phase)
            {
                // Extract the side effects from the left side of the comma.  Since they don't "go" anywhere, this
                // is all we need.

                GenTree* op1SideEffects = nullptr;
                // The addition of "GTF_MAKE_CSE" below prevents us from throwing away (for example)
                // hoisted expressions in loops.
                gtExtractSideEffList(op1, &op1SideEffects, (GTF_SIDE_EFFECT | GTF_MAKE_CSE));
                if (op1SideEffects)
                {
                    // Replace the left hand side with the side effect list.
                    tree->AsOp()->gtOp1 = op1SideEffects;
                    gtUpdateNodeSideEffects(tree);
                }
                else
                {
                    op2->gtFlags |= (tree->gtFlags & (GTF_DONT_CSE | GTF_LATE_ARG));
                    DEBUG_DESTROY_NODE(tree);
                    DEBUG_DESTROY_NODE(op1);
                    return op2;
                }

                /* If the right operand is just a void nop node, throw it away */
                if (op2->IsNothingNode() && op1->gtType == TYP_VOID)
                {
                    op1->gtFlags |= (tree->gtFlags & (GTF_DONT_CSE | GTF_LATE_ARG));
                    DEBUG_DESTROY_NODE(tree);
                    DEBUG_DESTROY_NODE(op2);
                    return op1;
                }
            }

            break;

        case GT_JTRUE:

            /* Special case if fgRemoveRestOfBlock is set to true */
            if (fgRemoveRestOfBlock)
            {
                if (fgIsCommaThrow(op1, true))
                {
                    GenTree* throwNode = op1->AsOp()->gtOp1;

                    JITDUMP("Removing [%06d] GT_JTRUE as the block now unconditionally throws an exception.\n",
                            dspTreeID(tree));
                    DEBUG_DESTROY_NODE(tree);

                    return throwNode;
                }

                noway_assert(op1->OperKind() & GTK_RELOP);
                noway_assert(op1->gtFlags & GTF_EXCEPT);

                // We need to keep op1 for the side-effects. Hang it off
                // a GT_COMMA node

                JITDUMP("Keeping side-effects by bashing [%06d] GT_JTRUE into a GT_COMMA.\n", dspTreeID(tree));

                tree->ChangeOper(GT_COMMA);
                tree->AsOp()->gtOp2 = op2 = gtNewNothingNode();

                // Additionally since we're eliminating the JTRUE
                // codegen won't like it if op1 is a RELOP of longs, floats or doubles.
                // So we change it into a GT_COMMA as well.
                JITDUMP("Also bashing [%06d] (a relop) into a GT_COMMA.\n", dspTreeID(op1));
                op1->ChangeOper(GT_COMMA);
                op1->gtFlags &= ~GTF_UNSIGNED; // Clear the unsigned flag if it was set on the relop
                op1->gtType = op1->AsOp()->gtOp1->gtType;

                return tree;
            }
            break;

        default:
            break;
    }

    assert(oper == tree->gtOper);

    // If we are in the Valuenum CSE phase then don't morph away anything as these
    // nodes may have CSE defs/uses in them.
    //
    if (!optValnumCSE_phase && (oper != GT_ASG) && (oper != GT_COLON))
    {
        /* Check for op1 as a GT_COMMA with a unconditional throw node */
        if (op1 && fgIsCommaThrow(op1, true))
        {
            if ((op1->gtFlags & GTF_COLON_COND) == 0)
            {
                /* We can safely throw out the rest of the statements */
                fgRemoveRestOfBlock = true;
            }

            GenTree* throwNode = op1->AsOp()->gtOp1;

            if (oper == GT_COMMA)
            {
                /* Both tree and op1 are GT_COMMA nodes */
                /* Change the tree's op1 to the throw node: op1->AsOp()->gtOp1 */
                tree->AsOp()->gtOp1 = throwNode;

                // Possibly reset the assignment flag
                if (((throwNode->gtFlags & GTF_ASG) == 0) && ((op2 == nullptr) || ((op2->gtFlags & GTF_ASG) == 0)))
                {
                    tree->gtFlags &= ~GTF_ASG;
                }

                return tree;
            }
            else if (oper != GT_NOP)
            {
                if (genActualType(typ) == genActualType(op1->gtType))
                {
                    /* The types match so, return the comma throw node as the new tree */
                    return op1;
                }
                else
                {
                    if (typ == TYP_VOID)
                    {
                        // Return the throw node
                        return throwNode;
                    }
                    else
                    {
                        GenTree* commaOp2 = op1->AsOp()->gtOp2;

                        // need type of oper to be same as tree
                        if (typ == TYP_LONG)
                        {
                            commaOp2->ChangeOperConst(GT_CNS_NATIVELONG);
                            commaOp2->AsIntConCommon()->SetLngValue(0);
                            /* Change the types of oper and commaOp2 to TYP_LONG */
                            op1->gtType = commaOp2->gtType = TYP_LONG;
                        }
                        else if (varTypeIsFloating(typ))
                        {
                            commaOp2->ChangeOperConst(GT_CNS_DBL);
                            commaOp2->AsDblCon()->gtDconVal = 0.0;
                            /* Change the types of oper and commaOp2 to TYP_DOUBLE */
                            op1->gtType = commaOp2->gtType = TYP_DOUBLE;
                        }
                        else
                        {
                            commaOp2->ChangeOperConst(GT_CNS_INT);
                            commaOp2->AsIntConCommon()->SetIconValue(0);
                            /* Change the types of oper and commaOp2 to TYP_INT */
                            op1->gtType = commaOp2->gtType = TYP_INT;
                        }

                        /* Return the GT_COMMA node as the new tree */
                        return op1;
                    }
                }
            }
        }

        /* Check for op2 as a GT_COMMA with a unconditional throw */

        if (op2 && fgIsCommaThrow(op2, true))
        {
            if ((op2->gtFlags & GTF_COLON_COND) == 0)
            {
                /* We can safely throw out the rest of the statements */
                fgRemoveRestOfBlock = true;
            }

            // If op1 has no side-effects
            if ((op1->gtFlags & GTF_ALL_EFFECT) == 0)
            {
                // If tree is an asg node
                if (tree->OperIs(GT_ASG))
                {
                    /* Return the throw node as the new tree */
                    return op2->AsOp()->gtOp1;
                }

                if (tree->OperGet() == GT_ARR_BOUNDS_CHECK)
                {
                    /* Return the throw node as the new tree */
                    return op2->AsOp()->gtOp1;
                }

                // If tree is a comma node
                if (tree->OperGet() == GT_COMMA)
                {
                    /* Return the throw node as the new tree */
                    return op2->AsOp()->gtOp1;
                }

                /* for the shift nodes the type of op2 can differ from the tree type */
                if ((typ == TYP_LONG) && (genActualType(op2->gtType) == TYP_INT))
                {
                    noway_assert(GenTree::OperIsShiftOrRotate(oper));

                    GenTree* commaOp2 = op2->AsOp()->gtOp2;

                    commaOp2->ChangeOperConst(GT_CNS_NATIVELONG);
                    commaOp2->AsIntConCommon()->SetLngValue(0);

                    /* Change the types of oper and commaOp2 to TYP_LONG */
                    op2->gtType = commaOp2->gtType = TYP_LONG;
                }

                if ((genActualType(typ) == TYP_INT) &&
                    (genActualType(op2->gtType) == TYP_LONG || varTypeIsFloating(op2->TypeGet())))
                {
                    // An example case is comparison (say GT_GT) of two longs or floating point values.

                    GenTree* commaOp2 = op2->AsOp()->gtOp2;

                    commaOp2->ChangeOperConst(GT_CNS_INT);
                    commaOp2->AsIntCon()->gtIconVal = 0;
                    /* Change the types of oper and commaOp2 to TYP_INT */
                    op2->gtType = commaOp2->gtType = TYP_INT;
                }

                if ((typ == TYP_BYREF) && (genActualType(op2->gtType) == TYP_I_IMPL))
                {
                    noway_assert(tree->OperGet() == GT_ADD);

                    GenTree* commaOp2 = op2->AsOp()->gtOp2;

                    commaOp2->ChangeOperConst(GT_CNS_INT);
                    commaOp2->AsIntCon()->gtIconVal = 0;
                    /* Change the types of oper and commaOp2 to TYP_BYREF */
                    op2->gtType = commaOp2->gtType = TYP_BYREF;
                }

                /* types should now match */
                noway_assert((genActualType(typ) == genActualType(op2->gtType)));

                /* Return the GT_COMMA node as the new tree */
                return op2;
            }
        }
    }

    /*-------------------------------------------------------------------------
     * Optional morphing is done if tree transformations is permitted
     */

    if ((opts.compFlags & CLFLG_TREETRANS) == 0)
    {
        return tree;
    }

    tree = fgMorphSmpOpOptional(tree->AsOp());

    return tree;
}

void Compiler::abiMorphStructReturn(GenTreeUnOp* ret, GenTree* val)
{
    assert(varTypeIsStruct(ret->GetType()));

#if FEATURE_MULTIREG_RET
    if (info.retDesc.GetRegCount() > 1)
    {
        assert(varTypeIsStruct(val->GetType()) || val->IsIntegralConst(0));

        ArrayStack<GenTreeOp*> commas(getAllocator(CMK_ArrayStack));

        while (val->OperIs(GT_COMMA))
        {
            commas.Push(val->AsOp());
            val = val->AsOp()->GetOp(1);
        }

        if (val->IsFieldList())
        {
            return;
        }

        if (val->TypeIs(TYP_STRUCT) && val->OperIs(GT_IND))
        {
            val->ChangeOper(GT_OBJ);
            val->AsObj()->SetLayout(info.GetRetLayout());
            val->AsObj()->SetKind(StructStoreKind::Invalid);
        }
        else if (varTypeIsSIMD(val->GetType()) && val->OperIs(GT_LCL_FLD))
        {
            val->AsLclFld()->SetLayout(info.GetRetLayout(), this);
        }

        GenTreeCall::Use use(val);
        use.SetSigTypeNum(typGetLayoutNum(info.GetRetLayout()));
        CallArgInfo argInfo(0, &use, info.retDesc.GetRegCount(), true);
        argInfo.SetArgType(val->GetType());

#ifdef UNIX_AMD64_ABI
        for (unsigned i = 0; i < info.retDesc.GetRegCount(); i++)
        {
            argInfo.SetRegNum(i, info.retDesc.GetRegNum(i));
            argInfo.SetRegType(i, info.retDesc.GetRegType(i));
        }
#else
        var_types  regType = info.retDesc.GetRegType(0);

        if (varTypeIsGC(regType))
        {
            regType = TYP_I_IMPL;
        }

        argInfo.SetRegType(regType);
#endif

        val = abiMorphMultiRegStructArg(&argInfo, val);

        var_types retType = varActualType(val->GetType());

#ifdef WINDOWS_X86_ABI
        // TODO-MIKE-Cleanup: abiMorphMultiRegStructArg manages to generate a FIELD_LIST
        // with a single LONG/DOUBLE field, this doesn't seem right and it certainly doesn't
        // work correctly on x86. ARM32 multireg args may also be affected.
        if (GenTreeFieldList* fieldList = val->IsFieldList())
        {
            GenTreeFieldList::Use* field = fieldList->Uses().GetHead();

            if (field->GetNext() == nullptr)
            {
                val = field->GetNode();
                assert(val->TypeIs(TYP_LONG, TYP_DOUBLE));

                if (val->TypeIs(TYP_DOUBLE))
                {
                    retType = TYP_LONG;
                }
            }
        }
#endif

        if (!commas.Empty())
        {
            commas.Top()->SetOp(1, val);

            for (int i = 0; i < commas.Height(); i++)
            {
                GenTreeOp* comma = commas.Top(i);
                comma->SetSideEffects(comma->GetOp(0)->GetSideEffects() | comma->GetOp(1)->GetSideEffects());
                comma->SetType(retType);
            }

            ret->SetSideEffects(commas.Bottom()->GetSideEffects());
        }
        else
        {
            ret->SetOp(0, val);
            ret->SetSideEffects(val->GetSideEffects());
        }

        ret->SetType(retType);

        return;
    }
#endif // FEATURE_MULTIREG_RET

    if (val->IsIntegralConst(0))
    {
        var_types regType = varActualType(info.retDesc.GetRegType(0));

        if (varTypeIsFloating(regType))
        {
            val->ChangeToDblCon(TYP_DOUBLE, 0);
        }
        else if (varTypeIsLong(regType))
        {
            val->SetType(TYP_LONG);
        }
        else if (varTypeIsGC(regType))
        {
            val->SetType(TYP_I_IMPL);
        }
#ifdef FEATURE_HW_INTRINSICS
        else if (varTypeIsSIMD(regType))
        {
            // TODO-MIKE-Cleanup: Somehow we still generate INT(0) for SIMD types...
            val->ChangeOper(GT_HWINTRINSIC);
            val->SetType(regType);
            val->AsHWIntrinsic()->SetIntrinsic(GetZeroSimdHWIntrinsic(regType), TYP_FLOAT, varTypeSize(regType), 0);
        }
#endif
        else
        {
            assert(regType == TYP_INT);
        }

        ret->SetType(regType);

        return;
    }

    if (val->OperIs(GT_LCL_VAR))
    {
        GenTreeLclVar* lclVar = val->AsLclVar();
        LclVarDsc*     lcl    = lvaGetDesc(lclVar);

#ifdef TARGET_AMD64
        if (varTypeIsSIMD(lcl->GetType()) && lcl->IsPromoted())
        {
            // Only Vector2/3/4 are promoted.
            assert(lvaGetDesc(lcl->GetPromotedFieldLclNum(0))->TypeIs(TYP_FLOAT));

            // TODO-MIKE-Review: Could we get here on ARM64 due to Vector2/Vector64
            // reinterpretation?

            val = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_FLOAT, 16,
                                           gtNewLclvNode(lcl->GetPromotedFieldLclNum(0), TYP_FLOAT),
                                           gtNewLclvNode(lcl->GetPromotedFieldLclNum(1), TYP_FLOAT));

            var_types regType = varActualType(info.retDesc.GetRegType(0));
            assert((regType == TYP_LONG) || (regType == TYP_DOUBLE));

            val = gtNewSimdGetElementNode(TYP_SIMD16, regType, val, gtNewIconNode(0));

            ret->SetOp(0, val);
            ret->SetType(regType);

            return;
        }
#endif
    }

#ifdef WINDOWS_AMD64_ABI
    if (varTypeIsSIMD(val->GetType()) && (info.retDesc.GetRegType(0) == TYP_LONG))
    {
        if (val->IsCall())
        {
            assert(val->TypeIs(TYP_SIMD8));
            val->SetType(TYP_LONG);
        }
        else if (val->TypeIs(TYP_SIMD8))
        {
            val = gtNewBitCastNode(TYP_LONG, val);
        }
        else
        {
            val = gtNewSimdGetElementNode(TYP_SIMD16, TYP_LONG, val, gtNewIconNode(0));
        }

        ret->SetOp(0, val);
        ret->SetType(TYP_LONG);
    }
#endif
}

#ifdef _PREFAST_
#pragma warning(pop)
#endif

GenTree* Compiler::fgMorphSmpOpOptional(GenTreeOp* tree)
{
    genTreeOps oper = tree->gtOper;
    GenTree*   op1  = tree->gtOp1;
    GenTree*   op2  = tree->gtOp2;
    var_types  typ  = tree->TypeGet();

    if (fgGlobalMorph && GenTree::OperIsCommutative(oper))
    {
        /* Swap the operands so that the more expensive one is 'op1' */

        if (tree->gtFlags & GTF_REVERSE_OPS)
        {
            tree->gtOp1 = op2;
            tree->gtOp2 = op1;

            op2 = op1;
            op1 = tree->gtOp1;

            tree->gtFlags &= ~GTF_REVERSE_OPS;
        }

        if (oper == op2->gtOper)
        {
            /*  Reorder nested operators at the same precedence level to be
                left-recursive. For example, change "(a+(b+c))" to the
                equivalent expression "((a+b)+c)".
             */

            /* Things are handled differently for floating-point operators */

            if (!varTypeIsFloating(tree->TypeGet()))
            {
                fgMoveOpsLeft(tree);
                op1 = tree->gtOp1;
                op2 = tree->gtOp2;
            }
        }
    }

#if REARRANGE_ADDS

    /* Change "((x+icon)+y)" to "((x+y)+icon)"
       Don't reorder floating-point operations */

    if (fgGlobalMorph && (oper == GT_ADD) && !tree->gtOverflow() && (op1->gtOper == GT_ADD) && !op1->gtOverflow() &&
        varTypeIsIntegralOrI(typ))
    {
        GenTree* ad1 = op1->AsOp()->gtOp1;
        GenTree* ad2 = op1->AsOp()->gtOp2;

        if (!op2->OperIsConst() && ad2->OperIsConst())
        {
            //  This takes
            //        + (tree)
            //       / \.
            //      /   \.
            //     /     \.
            //    + (op1) op2
            //   / \.
            //  /   \.
            // ad1  ad2
            //
            // and it swaps ad2 and op2.

            // Don't create a byref pointer that may point outside of the ref object.
            // If a GC happens, the byref won't get updated. This can happen if one
            // of the int components is negative. It also requires the address generation
            // be in a fully-interruptible code region.
            if (!varTypeIsGC(ad1->TypeGet()) && !varTypeIsGC(op2->TypeGet()))
            {
                tree->gtOp2 = ad2;

                op1->AsOp()->gtOp2 = op2;
                op1->gtFlags |= op2->gtFlags & GTF_ALL_EFFECT;

                op2 = tree->gtOp2;
            }
        }
    }

#endif

    /*-------------------------------------------------------------------------
     * Perform optional oper-specific postorder morphing
     */

    switch (oper)
    {
        case GT_ASG:
            // Make sure we're allowed to do this.
            if (optValnumCSE_phase)
            {
                // It is not safe to reorder/delete CSE's
                break;
            }

            if (typ == TYP_LONG)
            {
                break;
            }

            if (op2->gtFlags & GTF_ASG)
            {
                break;
            }

            if ((op2->gtFlags & GTF_CALL) && (op1->gtFlags & GTF_ALL_EFFECT))
            {
                break;
            }

            /* Special case: a cast that can be thrown away */

            // TODO-Cleanup: fgMorphSmp does a similar optimization. However, it removes only
            // one cast and sometimes there is another one after it that gets removed by this
            // code. fgMorphSmp should be improved to remove all redundant casts so this code
            // can be removed.

            if (op1->gtOper == GT_IND && op2->gtOper == GT_CAST && !op2->gtOverflow())
            {
                var_types srct;
                var_types cast;
                var_types dstt;

                srct = op2->AsCast()->CastOp()->TypeGet();
                cast = (var_types)op2->CastToType();
                dstt = op1->TypeGet();

                /* Make sure these are all ints and precision is not lost */

                if (genTypeSize(cast) >= genTypeSize(dstt) && dstt <= TYP_INT && srct <= TYP_INT)
                {
                    op2 = tree->gtOp2 = op2->AsCast()->CastOp();
                }
            }

            break;

        case GT_MUL:

            /* Check for the case "(val + icon) * icon" */

            if (op2->gtOper == GT_CNS_INT && op1->gtOper == GT_ADD)
            {
                GenTree* add = op1->AsOp()->gtOp2;

                if (add->IsCnsIntOrI() && (op2->GetScaleIndexMul() != 0))
                {
                    if (tree->gtOverflow() || op1->gtOverflow())
                    {
                        break;
                    }

                    ssize_t imul = op2->AsIntCon()->gtIconVal;
                    ssize_t iadd = add->AsIntCon()->gtIconVal;

                    /* Change '(val + iadd) * imul' -> '(val * imul) + (iadd * imul)' */

                    oper = GT_ADD;
                    tree->ChangeOper(oper);

                    op2->AsIntCon()->gtIconVal = iadd * imul;

                    op1->ChangeOper(GT_MUL);

                    add->AsIntCon()->gtIconVal = imul;
#ifdef TARGET_64BIT
                    if (add->gtType == TYP_INT)
                    {
                        // we need to properly re-sign-extend or truncate after multiplying two int constants above
                        add->AsIntCon()->TruncateOrSignExtend32();
                    }
#endif // TARGET_64BIT
                }
            }

            break;

        case GT_DIV:

            /* For "val / 1", just return "val" */

            if (op2->IsIntegralConst(1))
            {
                DEBUG_DESTROY_NODE(tree);
                return op1;
            }
            break;

        case GT_UDIV:
        case GT_UMOD:
            tree->CheckDivideByConstOptimized(this);
            break;

        case GT_LSH:

            /* Check for the case "(val + icon) << icon" */

            if (!optValnumCSE_phase && op2->IsCnsIntOrI() && op1->gtOper == GT_ADD && !op1->gtOverflow())
            {
                GenTree* cns = op1->AsOp()->gtOp2;

                if (cns->IsCnsIntOrI() && (op2->GetScaleIndexShf() != 0))
                {
                    ssize_t ishf = op2->AsIntConCommon()->IconValue();
                    ssize_t iadd = cns->AsIntConCommon()->IconValue();

                    // printf("Changing '(val+icon1)<<icon2' into '(val<<icon2+icon1<<icon2)'\n");

                    /* Change "(val + iadd) << ishf" into "(val<<ishf + iadd<<ishf)" */

                    tree->ChangeOper(GT_ADD);
                    ssize_t result = iadd << ishf;
                    op2->AsIntConCommon()->SetIconValue(result);
#ifdef TARGET_64BIT
                    if (op1->gtType == TYP_INT)
                    {
                        op2->AsIntCon()->TruncateOrSignExtend32();
                    }
#endif // TARGET_64BIT

                    // we are reusing the shift amount node here, but the type we want is that of the shift result
                    op2->gtType = op1->gtType;

                    op1->ChangeOper(GT_LSH);

                    cns->AsIntConCommon()->SetIconValue(ishf);
                }
            }

            break;

        case GT_XOR:

            if (!optValnumCSE_phase)
            {
                /* "x ^ -1" is "~x" */

                if (op2->IsIntegralConst(-1))
                {
                    tree->ChangeOper(GT_NOT);
                    tree->gtOp2 = nullptr;
                    DEBUG_DESTROY_NODE(op2);
                }
                else if (op2->IsIntegralConst(1) && op1->OperIsCompare())
                {
                    /* "binaryVal ^ 1" is "!binaryVal" */
                    gtReverseCond(op1);
                    DEBUG_DESTROY_NODE(op2);
                    DEBUG_DESTROY_NODE(tree);
                    return op1;
                }
            }

            break;

        case GT_INIT_VAL:
            // Initialization values for initBlk have special semantics - their lower
            // byte is used to fill the struct. However, we allow 0 as a "bare" value,
            // which enables them to get a VNForZero, and be propagated.
            if (op1->IsIntegralConst(0))
            {
                return op1;
            }
            break;

        default:
            break;
    }
    return tree;
}

//------------------------------------------------------------------------
// fgMorphModToSubMulDiv: Transform a % b into the equivalent a - (a / b) * b
// (see ECMA III 3.55 and III.3.56).
//
// Arguments:
//    tree - The GT_MOD/GT_UMOD tree to morph
//
// Returns:
//    The morphed tree
//
// Notes:
//    For ARM64 we don't have a remainder instruction so this transform is
//    always done. For XARCH this transform is done if we know that magic
//    division will be used, in that case this transform allows CSE to
//    eliminate the redundant div from code like "x = a / 3; y = a % 3;".
//
//    This method will produce the above expression in 'a' and 'b' are
//    leaf nodes, otherwise, if any of them is not a leaf it will spill
//    its value into a temporary variable, an example:
//    (x * 2 - 1) % (y + 1) ->  t1 - (t2 * ( comma(t1 = x * 2 - 1, t1) / comma(t2 = y + 1, t2) ) )
//
GenTree* Compiler::fgMorphModToSubMulDiv(GenTreeOp* tree)
{
    if (tree->OperGet() == GT_MOD)
    {
        tree->SetOper(GT_DIV);
    }
    else if (tree->OperGet() == GT_UMOD)
    {
        tree->SetOper(GT_UDIV);
    }
    else
    {
        noway_assert(!"Illegal gtOper in fgMorphModToSubMulDiv");
    }

    var_types type        = tree->gtType;
    GenTree*  denominator = tree->gtOp2;
    GenTree*  numerator   = tree->gtOp1;

    if (!numerator->OperIsLeaf())
    {
        numerator = fgMakeMultiUse(&tree->gtOp1);
    }

    if (!denominator->OperIsLeaf())
    {
        denominator = fgMakeMultiUse(&tree->gtOp2);
    }

    // The numerator and denominator may have been assigned to temps, in which case
    // their defining assignments are in the current tree. Therefore, we need to
    // set the execuction order accordingly on the nodes we create.
    // That is, the "mul" will be evaluated in "normal" order, and the "sub" must
    // be set to be evaluated in reverse order.
    //
    GenTree* mul = gtNewOperNode(GT_MUL, type, tree, gtCloneExpr(denominator));
    assert(!mul->IsReverseOp());
    GenTree* sub = gtNewOperNode(GT_SUB, type, gtCloneExpr(numerator), mul);
    sub->gtFlags |= GTF_REVERSE_OPS;

#ifdef DEBUG
    sub->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;
#endif

    tree->CheckDivideByConstOptimized(this);

    return sub;
}

//------------------------------------------------------------------------------
// fgOperIsBitwiseRotationRoot : Check if the operation can be a root of a bitwise rotation tree.
//
//
// Arguments:
//    oper  - Operation to check
//
// Return Value:
//    True if the operation can be a root of a bitwise rotation tree; false otherwise.

bool Compiler::fgOperIsBitwiseRotationRoot(genTreeOps oper)
{
    return (oper == GT_OR) || (oper == GT_XOR);
}

//------------------------------------------------------------------------------
// fgRecognizeAndMorphBitwiseRotation : Check if the tree represents a left or right rotation. If so, return
//                                      an equivalent GT_ROL or GT_ROR tree; otherwise, return the original tree.
//
// Arguments:
//    tree  - tree to check for a rotation pattern
//
// Return Value:
//    An equivalent GT_ROL or GT_ROR tree if a pattern is found; original tree otherwise.
//
// Assumption:
//    The input is a GT_OR or a GT_XOR tree.

GenTree* Compiler::fgRecognizeAndMorphBitwiseRotation(GenTree* tree)
{
    //
    // Check for a rotation pattern, e.g.,
    //
    //                         OR                      ROL
    //                      /      \                   / \.
    //                    LSH      RSZ      ->        x   y
    //                    / \      / \.
    //                   x  AND   x  AND
    //                      / \      / \.
    //                     y  31   ADD  31
    //                             / \.
    //                            NEG 32
    //                             |
    //                             y
    // The patterns recognized:
    // (x << (y & M)) op (x >>> ((-y + N) & M))
    // (x >>> ((-y + N) & M)) op (x << (y & M))
    //
    // (x << y) op (x >>> (-y + N))
    // (x >> > (-y + N)) op (x << y)
    //
    // (x >>> (y & M)) op (x << ((-y + N) & M))
    // (x << ((-y + N) & M)) op (x >>> (y & M))
    //
    // (x >>> y) op (x << (-y + N))
    // (x << (-y + N)) op (x >>> y)
    //
    // (x << c1) op (x >>> c2)
    // (x >>> c1) op (x << c2)
    //
    // where
    // c1 and c2 are const
    // c1 + c2 == bitsize(x)
    // N == bitsize(x)
    // M is const
    // M & (N - 1) == N - 1
    // op is either | or ^

    if (((tree->gtFlags & GTF_PERSISTENT_SIDE_EFFECTS) != 0) || ((tree->gtFlags & GTF_ORDER_SIDEEFF) != 0))
    {
        // We can't do anything if the tree has assignments, calls, or volatile
        // reads. Note that we allow GTF_EXCEPT side effect since any exceptions
        // thrown by the original tree will be thrown by the transformed tree as well.
        return tree;
    }

    genTreeOps oper = tree->OperGet();
    assert(fgOperIsBitwiseRotationRoot(oper));

    // Check if we have an LSH on one side of the OR and an RSZ on the other side.
    GenTree* op1            = tree->gtGetOp1();
    GenTree* op2            = tree->gtGetOp2();
    GenTree* leftShiftTree  = nullptr;
    GenTree* rightShiftTree = nullptr;
    if ((op1->OperGet() == GT_LSH) && (op2->OperGet() == GT_RSZ))
    {
        leftShiftTree  = op1;
        rightShiftTree = op2;
    }
    else if ((op1->OperGet() == GT_RSZ) && (op2->OperGet() == GT_LSH))
    {
        leftShiftTree  = op2;
        rightShiftTree = op1;
    }
    else
    {
        return tree;
    }

    // Check if the trees representing the value to shift are identical.
    // We already checked that there are no side effects above.
    if (GenTree::Compare(leftShiftTree->gtGetOp1(), rightShiftTree->gtGetOp1()))
    {
        GenTree*  rotatedValue           = leftShiftTree->gtGetOp1();
        var_types rotatedValueActualType = genActualType(rotatedValue->gtType);
        ssize_t   rotatedValueBitSize    = genTypeSize(rotatedValueActualType) * 8;
        noway_assert((rotatedValueBitSize == 32) || (rotatedValueBitSize == 64));
        GenTree* leftShiftIndex  = leftShiftTree->gtGetOp2();
        GenTree* rightShiftIndex = rightShiftTree->gtGetOp2();

        // The shift index may be masked. At least (rotatedValueBitSize - 1) lower bits
        // shouldn't be masked for the transformation to be valid. If additional
        // higher bits are not masked, the transformation is still valid since the result
        // of MSIL shift instructions is unspecified if the shift amount is greater or equal
        // than the width of the value being shifted.
        ssize_t minimalMask    = rotatedValueBitSize - 1;
        ssize_t leftShiftMask  = -1;
        ssize_t rightShiftMask = -1;

        if ((leftShiftIndex->OperGet() == GT_AND))
        {
            if (leftShiftIndex->gtGetOp2()->IsCnsIntOrI())
            {
                leftShiftMask  = leftShiftIndex->gtGetOp2()->AsIntCon()->gtIconVal;
                leftShiftIndex = leftShiftIndex->gtGetOp1();
            }
            else
            {
                return tree;
            }
        }

        if ((rightShiftIndex->OperGet() == GT_AND))
        {
            if (rightShiftIndex->gtGetOp2()->IsCnsIntOrI())
            {
                rightShiftMask  = rightShiftIndex->gtGetOp2()->AsIntCon()->gtIconVal;
                rightShiftIndex = rightShiftIndex->gtGetOp1();
            }
            else
            {
                return tree;
            }
        }

        if (((minimalMask & leftShiftMask) != minimalMask) || ((minimalMask & rightShiftMask) != minimalMask))
        {
            // The shift index is overmasked, e.g., we have
            // something like (x << y & 15) or
            // (x >> (32 - y) & 15 with 32 bit x.
            // The transformation is not valid.
            return tree;
        }

        GenTree*   shiftIndexWithAdd    = nullptr;
        GenTree*   shiftIndexWithoutAdd = nullptr;
        genTreeOps rotateOp             = GT_NONE;
        GenTree*   rotateIndex          = nullptr;

        if (leftShiftIndex->OperGet() == GT_ADD)
        {
            shiftIndexWithAdd    = leftShiftIndex;
            shiftIndexWithoutAdd = rightShiftIndex;
            rotateOp             = GT_ROR;
        }
        else if (rightShiftIndex->OperGet() == GT_ADD)
        {
            shiftIndexWithAdd    = rightShiftIndex;
            shiftIndexWithoutAdd = leftShiftIndex;
            rotateOp             = GT_ROL;
        }

        if (shiftIndexWithAdd != nullptr)
        {
            if (shiftIndexWithAdd->gtGetOp2()->IsCnsIntOrI())
            {
                if (shiftIndexWithAdd->gtGetOp2()->AsIntCon()->gtIconVal == rotatedValueBitSize)
                {
                    if (shiftIndexWithAdd->gtGetOp1()->OperGet() == GT_NEG)
                    {
                        if (GenTree::Compare(shiftIndexWithAdd->gtGetOp1()->gtGetOp1(), shiftIndexWithoutAdd))
                        {
                            // We found one of these patterns:
                            // (x << (y & M)) | (x >>> ((-y + N) & M))
                            // (x << y) | (x >>> (-y + N))
                            // (x >>> (y & M)) | (x << ((-y + N) & M))
                            // (x >>> y) | (x << (-y + N))
                            // where N == bitsize(x), M is const, and
                            // M & (N - 1) == N - 1
                            CLANG_FORMAT_COMMENT_ANCHOR;

#ifndef TARGET_64BIT
                            if (!shiftIndexWithoutAdd->IsCnsIntOrI() && (rotatedValueBitSize == 64))
                            {
                                // TODO-X86-CQ: we need to handle variable-sized long shifts specially on x86.
                                // GT_LSH, GT_RSH, and GT_RSZ have helpers for this case. We may need
                                // to add helpers for GT_ROL and GT_ROR.
                                return tree;
                            }
#endif

                            rotateIndex = shiftIndexWithoutAdd;
                        }
                    }
                }
            }
        }
        else if ((leftShiftIndex->IsCnsIntOrI() && rightShiftIndex->IsCnsIntOrI()))
        {
            if (leftShiftIndex->AsIntCon()->gtIconVal + rightShiftIndex->AsIntCon()->gtIconVal == rotatedValueBitSize)
            {
                // We found this pattern:
                // (x << c1) | (x >>> c2)
                // where c1 and c2 are const and c1 + c2 == bitsize(x)
                rotateOp    = GT_ROL;
                rotateIndex = leftShiftIndex;
            }
        }

        if (rotateIndex != nullptr)
        {
            noway_assert(GenTree::OperIsRotate(rotateOp));

            unsigned inputTreeEffects = tree->gtFlags & GTF_ALL_EFFECT;

            // We can use the same tree only during global morph; reusing the tree in a later morph
            // may invalidate value numbers.
            if (fgGlobalMorph)
            {
                tree->AsOp()->gtOp1 = rotatedValue;
                tree->AsOp()->gtOp2 = rotateIndex;
                tree->ChangeOper(rotateOp);

                unsigned childFlags = 0;
                for (GenTree* op : tree->Operands())
                {
                    childFlags |= (op->gtFlags & GTF_ALL_EFFECT);
                }

                // The parent's flags should be a superset of its operands' flags
                noway_assert((inputTreeEffects & childFlags) == childFlags);
            }
            else
            {
                tree = gtNewOperNode(rotateOp, rotatedValueActualType, rotatedValue, rotateIndex);
                noway_assert(inputTreeEffects == (tree->gtFlags & GTF_ALL_EFFECT));
            }

            return tree;
        }
    }
    return tree;
}

#ifdef DEBUG
void Compiler::fgMorphClearDebugNodeMorphed(GenTree* tree)
{
    tree->gtDebugFlags &= ~GTF_DEBUG_NODE_MORPHED;
    tree->VisitOperands([this](GenTree* child) {
        fgMorphClearDebugNodeMorphed(child);
        return GenTree::VisitResult::Continue;
    });
}
#endif

/*****************************************************************************
 *
 *  Transform the given tree for code generation and return an equivalent tree.
 */

GenTree* Compiler::fgMorphTree(GenTree* tree, MorphAddrContext* mac)
{
    assert(tree);

#ifdef DEBUG
    if (verbose)
    {
        if ((unsigned)JitConfig.JitBreakMorphTree() == tree->gtTreeID)
        {
            noway_assert(!"JitBreakMorphTree hit");
        }
    }
#endif

#ifdef DEBUG
    int thisMorphNum = 0;
    if (verbose && treesBeforeAfterMorph)
    {
        thisMorphNum = morphNum++;
        printf("\nfgMorphTree (before %d):\n", thisMorphNum);
        gtDispTree(tree);
    }
#endif

/*-------------------------------------------------------------------------
 * fgMorphTree() can potentially replace a tree with another, and the
 * caller has to store the return value correctly.
 * Turn this on to always make copy of "tree" here to shake out
 * hidden/unupdated references.
 */

#ifdef DEBUG

    if (compStressCompile(STRESS_GENERIC_CHECK, 0))
    {
        GenTree* copy;

        if (GenTree::s_gtNodeSizes[tree->gtOper] == TREE_NODE_SZ_SMALL)
        {
            copy = gtNewLargeOperNode(GT_ADD, TYP_INT);
        }
        else
        {
            copy = new (this, GT_CALL) GenTreeCall();
        }

        copy->ReplaceWith(tree, this);

#if defined(LATE_DISASM)
        // GT_CNS_INT is considered small, so ReplaceWith() won't copy all fields
        if ((tree->gtOper == GT_CNS_INT) && tree->IsIconHandle())
        {
            copy->AsIntCon()->gtCompileTimeHandle = tree->AsIntCon()->gtCompileTimeHandle;
        }
#endif

        DEBUG_DESTROY_NODE(tree);
        tree = copy;
    }
#endif // DEBUG

    if (fgGlobalMorph)
    {
        /* Ensure that we haven't morphed this node already */
        assert(((tree->gtDebugFlags & GTF_DEBUG_NODE_MORPHED) == 0) && "ERROR: Already morphed this node!");

#if LOCAL_ASSERTION_PROP
        /* Before morphing the tree, we try to propagate any active assertions */
        if (optLocalAssertionProp)
        {
            /* Do we have any active assertions? */

            if (optAssertionCount > 0)
            {
                GenTree* newTree = tree;
                while (newTree != nullptr)
                {
                    tree = newTree;
                    /* newTree is non-Null if we propagated an assertion */
                    newTree = optAssertionProp(apFull, tree, nullptr, nullptr);
                }
                assert(tree != nullptr);
            }
        }
        PREFAST_ASSUME(tree != nullptr);
#endif
    }

    /* Save the original un-morphed tree for fgMorphTreeDone */

    GenTree* oldTree = tree;

    /* Figure out what kind of a node we have */

    unsigned kind = tree->OperKind();

    /* Is this a constant node? */

    if (kind & GTK_CONST)
    {
        tree = fgMorphConst(tree);
        goto DONE;
    }

    /* Is this a leaf node? */

    if (kind & GTK_LEAF)
    {
        tree = fgMorphLeaf(tree);
        goto DONE;
    }

    /* Is it a 'simple' unary/binary operator? */

    if (kind & GTK_SMPOP)
    {
        tree = fgMorphSmpOp(tree, mac);
        goto DONE;
    }

    /* See what kind of a special operator we have here */

    switch (tree->OperGet())
    {
        case GT_FIELD:
            tree = fgMorphField(tree, mac);
            break;

        case GT_CALL:
            if (tree->OperMayThrow(this))
            {
                tree->gtFlags |= GTF_EXCEPT;
            }
            else
            {
                tree->gtFlags &= ~GTF_EXCEPT;
            }
            tree = fgMorphCall(tree->AsCall());
            break;

        case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
        case GT_HW_INTRINSIC_CHK:
#endif
        {
            GenTreeBoundsChk* check  = tree->AsBoundsChk();
            GenTree*          index  = check->GetIndex();
            GenTree*          length = check->GetLength();

            index  = fgMorphTree(index);
            length = fgMorphTree(length);

            // If the index is a COMMA(throw, x), just return that.
            if (!optValnumCSE_phase && fgIsCommaThrow(index))
            {
                tree = index;
            }
            else
            {
                check->SetIndex(index);
                check->SetLength(length);
                check->SetSideEffects(GTF_EXCEPT | index->GetSideEffects() | length->GetSideEffects());

                // TODO-MIKE-Review: This doesn't make a lot of sense. One way or another, fgSimpleLowering
                // creates and sets the throw block (if throw helper blocks are used). Why would we do this
                // here only in minopts? It probably makes sense to delay throw block creation to lowering
                // so the optimizer has fewer blocks to process (throw blocks don't do anything interesting,
                // they're just helper calls without any arguments). But if we delay when optimizations are
                // enabled why not also delay in minopts? Is this a leftover from when the throw block stack
                // level for x86 args was computed during morph?

                if (opts.MinOpts())
                {
                    check->SetThrowBlock(fgGetRngChkTarget(compCurBB, check->GetThrowKind()));
                }
            }
        }
        break;

        case GT_ARR_ELEM:
            tree->AsArrElem()->gtArrObj = fgMorphTree(tree->AsArrElem()->gtArrObj);

            unsigned dim;
            for (dim = 0; dim < tree->AsArrElem()->gtArrRank; dim++)
            {
                tree->AsArrElem()->gtArrInds[dim] = fgMorphTree(tree->AsArrElem()->gtArrInds[dim]);
            }

            tree->gtFlags &= ~GTF_CALL;

            tree->gtFlags |= tree->AsArrElem()->gtArrObj->gtFlags & GTF_ALL_EFFECT;

            for (dim = 0; dim < tree->AsArrElem()->gtArrRank; dim++)
            {
                tree->gtFlags |= tree->AsArrElem()->gtArrInds[dim]->gtFlags & GTF_ALL_EFFECT;
            }

            if (fgGlobalMorph)
            {
                fgGetRngChkTarget(compCurBB, SCK_RNGCHK_FAIL);
            }
            break;

        case GT_ARR_OFFSET:
            // GT_ARR_OFFSET nodes are created during lowering.
            noway_assert(!fgGlobalMorph);

            tree->AsArrOffs()->gtOffset = fgMorphTree(tree->AsArrOffs()->gtOffset);
            tree->AsArrOffs()->gtIndex  = fgMorphTree(tree->AsArrOffs()->gtIndex);
            tree->AsArrOffs()->gtArrObj = fgMorphTree(tree->AsArrOffs()->gtArrObj);

            tree->gtFlags &= ~GTF_CALL;
            tree->gtFlags |= tree->AsArrOffs()->gtOffset->gtFlags & GTF_ALL_EFFECT;
            tree->gtFlags |= tree->AsArrOffs()->gtIndex->gtFlags & GTF_ALL_EFFECT;
            tree->gtFlags |= tree->AsArrOffs()->gtArrObj->gtFlags & GTF_ALL_EFFECT;
            break;

        case GT_PHI:
            tree->gtFlags &= ~GTF_ALL_EFFECT;
            for (GenTreePhi::Use& use : tree->AsPhi()->Uses())
            {
                use.SetNode(fgMorphTree(use.GetNode()));
                tree->gtFlags |= use.GetNode()->gtFlags & GTF_ALL_EFFECT;
            }
            break;

        case GT_FIELD_LIST:
            tree->gtFlags &= ~GTF_ALL_EFFECT;
            for (GenTreeFieldList::Use& use : tree->AsFieldList()->Uses())
            {
                use.SetNode(fgMorphTree(use.GetNode()));
                tree->gtFlags |= (use.GetNode()->gtFlags & GTF_ALL_EFFECT);
            }
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            fgMorphHWIntrinsic(tree->AsHWIntrinsic());
            break;
#endif

        case GT_INSTR:
            assert(compRationalIRForm);
            for (GenTreeInstr::Use& use : tree->AsInstr()->Uses())
            {
                use.SetNode(fgMorphTree(use.GetNode()));
            }
            break;

        case GT_CMPXCHG:
            tree->AsCmpXchg()->gtOpLocation  = fgMorphTree(tree->AsCmpXchg()->gtOpLocation);
            tree->AsCmpXchg()->gtOpValue     = fgMorphTree(tree->AsCmpXchg()->gtOpValue);
            tree->AsCmpXchg()->gtOpComparand = fgMorphTree(tree->AsCmpXchg()->gtOpComparand);

            tree->gtFlags &= (~GTF_EXCEPT & ~GTF_CALL);

            tree->gtFlags |= tree->AsCmpXchg()->gtOpLocation->gtFlags & GTF_ALL_EFFECT;
            tree->gtFlags |= tree->AsCmpXchg()->gtOpValue->gtFlags & GTF_ALL_EFFECT;
            tree->gtFlags |= tree->AsCmpXchg()->gtOpComparand->gtFlags & GTF_ALL_EFFECT;
            break;

        case GT_STORE_DYN_BLK:
        case GT_DYN_BLK:
            if (tree->OperGet() == GT_STORE_DYN_BLK)
            {
                tree->AsDynBlk()->Data() = fgMorphTree(tree->AsDynBlk()->Data());
            }
            tree->AsDynBlk()->Addr()        = fgMorphTree(tree->AsDynBlk()->Addr());
            tree->AsDynBlk()->gtDynamicSize = fgMorphTree(tree->AsDynBlk()->gtDynamicSize);

            tree->gtFlags &= ~GTF_CALL;
            tree->SetIndirExceptionFlags(this);

            if (tree->OperGet() == GT_STORE_DYN_BLK)
            {
                tree->gtFlags |= tree->AsDynBlk()->Data()->gtFlags & GTF_ALL_EFFECT;
            }
            tree->gtFlags |= tree->AsDynBlk()->Addr()->gtFlags & GTF_ALL_EFFECT;
            tree->gtFlags |= tree->AsDynBlk()->gtDynamicSize->gtFlags & GTF_ALL_EFFECT;
            break;

        case GT_INDEX_ADDR:
            GenTreeIndexAddr* indexAddr;
            indexAddr = tree->AsIndexAddr();
            indexAddr->SetIndex(fgMorphTree(indexAddr->GetIndex()));
            indexAddr->SetArray(fgMorphTree(indexAddr->GetArray()));

            tree->gtFlags &= ~GTF_CALL;

            tree->gtFlags |= indexAddr->GetIndex()->gtFlags & GTF_ALL_EFFECT;
            tree->gtFlags |= indexAddr->GetArray()->gtFlags & GTF_ALL_EFFECT;
            break;

        default:
#ifdef DEBUG
            gtDispTree(tree);
#endif
            noway_assert(!"unexpected operator");
    }
DONE:

    fgMorphTreeDone(tree, oldTree DEBUGARG(thisMorphNum));

    return tree;
}

#if LOCAL_ASSERTION_PROP
//------------------------------------------------------------------------
// fgKillDependentAssertionsSingle: Kill all assertions specific to lclNum
//
// Arguments:
//    lclNum - The varNum of the lclVar for which we're killing assertions.
//    tree   - (DEBUG only) the tree responsible for killing its assertions.
//
void Compiler::fgKillDependentAssertionsSingle(unsigned lclNum DEBUGARG(GenTree* tree))
{
    /* All dependent assertions are killed here */

    ASSERT_TP killed = BitVecOps::MakeCopy(apTraits, GetAssertionDep(lclNum));

    if (killed)
    {
        AssertionIndex index = optAssertionCount;
        while (killed && (index > 0))
        {
            if (BitVecOps::IsMember(apTraits, killed, index - 1))
            {
#ifdef DEBUG
                AssertionDsc* curAssertion = optGetAssertion(index);
                noway_assert((curAssertion->op1.lcl.lclNum == lclNum) ||
                             ((curAssertion->op2.kind == O2K_LCLVAR_COPY) && (curAssertion->op2.lcl.lclNum == lclNum)));
                if (verbose)
                {
                    printf("\nThe assignment ");
                    printTreeID(tree);
                    printf(" using V%02u removes: ", curAssertion->op1.lcl.lclNum);
                    optPrintAssertion(curAssertion);
                }
#endif
                // Remove this bit from the killed mask
                BitVecOps::RemoveElemD(apTraits, killed, index - 1);

                optAssertionRemove(index);
            }

            index--;
        }

        // killed mask should now be zero
        noway_assert(BitVecOps::IsEmpty(apTraits, killed));
    }
}
//------------------------------------------------------------------------
// fgKillDependentAssertions: Kill all dependent assertions with regard to lclNum.
//
// Arguments:
//    lclNum - The varNum of the lclVar for which we're killing assertions.
//    tree   - (DEBUG only) the tree responsible for killing its assertions.
//
// Notes:
//    For structs and struct fields, it will invalidate the children and parent
//    respectively.
//    Calls fgKillDependentAssertionsSingle to kill the assertions for a single lclVar.
//
void Compiler::fgKillDependentAssertions(unsigned lclNum DEBUGARG(GenTree* tree))
{
    LclVarDsc* varDsc = lvaGetDesc(lclNum);

    if (varDsc->lvPromoted)
    {
        noway_assert(varTypeIsStruct(varDsc));

        // Kill the field locals.
        for (unsigned i = varDsc->lvFieldLclStart; i < varDsc->lvFieldLclStart + varDsc->lvFieldCnt; ++i)
        {
            fgKillDependentAssertionsSingle(i DEBUGARG(tree));
        }

        // Kill the struct local itself.
        fgKillDependentAssertionsSingle(lclNum DEBUGARG(tree));
    }
    else if (varDsc->lvIsStructField)
    {
        // Kill the field local.
        fgKillDependentAssertionsSingle(lclNum DEBUGARG(tree));

        // Kill the parent struct.
        fgKillDependentAssertionsSingle(varDsc->lvParentLcl DEBUGARG(tree));
    }
    else
    {
        fgKillDependentAssertionsSingle(lclNum DEBUGARG(tree));
    }
}
#endif // LOCAL_ASSERTION_PROP

/*****************************************************************************
 *
 *  This function is called to complete the morphing of a tree node
 *  It should only be called once for each node.
 *  If DEBUG is defined the flag GTF_DEBUG_NODE_MORPHED is checked and updated,
 *  to enforce the invariant that each node is only morphed once.
 *  If LOCAL_ASSERTION_PROP is enabled the result tree may be replaced
 *  by an equivalent tree.
 *
 */

void Compiler::fgMorphTreeDone(GenTree* tree,
                               GenTree* oldTree /* == NULL */
                               DEBUGARG(int morphNum))
{
#ifdef DEBUG
    if (verbose && treesBeforeAfterMorph)
    {
        printf("\nfgMorphTree (after %d):\n", morphNum);
        gtDispTree(tree);
        printf(""); // in our logic this causes a flush
    }
#endif

    if (!fgGlobalMorph)
    {
        return;
    }

    if ((oldTree != nullptr) && (oldTree != tree))
    {
        /* Ensure that we have morphed this node */
        assert((tree->gtDebugFlags & GTF_DEBUG_NODE_MORPHED) && "ERROR: Did not morph this node!");
    }
    else
    {
        // Ensure that we haven't morphed this node already
        assert(((tree->gtDebugFlags & GTF_DEBUG_NODE_MORPHED) == 0) && "ERROR: Already morphed this node!");
    }

    if (tree->OperKind() & GTK_CONST)
    {
        goto DONE;
    }

#if LOCAL_ASSERTION_PROP

    if (!optLocalAssertionProp)
    {
        goto DONE;
    }

    if (optAssertionCount > 0)
    {
        if (tree->OperIs(GT_ASG) && tree->AsOp()->GetOp(0)->OperIs(GT_LCL_VAR, GT_LCL_FLD))
        {
            GenTreeLclVarCommon* lclNode = tree->AsOp()->GetOp(0)->AsLclVarCommon();
            fgKillDependentAssertions(lclNode->GetLclNum() DEBUGARG(tree));
        }
        else
        {
            // We should not have LIR stores during global morph.
            assert(!tree->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));
        }
    }

    optAssertionGen(tree);

#endif // LOCAL_ASSERTION_PROP

DONE:;

#ifdef DEBUG
    /* Mark this node as being morphed */
    tree->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;
#endif
}

/*****************************************************************************
 *
 *  Check and fold blocks of type BBJ_COND and BBJ_SWITCH on constants
 *  Returns true if we modified the flow graph
 */

bool Compiler::fgFoldConditional(BasicBlock* block)
{
    bool result = false;

    // We don't want to make any code unreachable
    if (opts.OptimizationDisabled())
    {
        return false;
    }

    if (block->bbJumpKind == BBJ_COND)
    {
        noway_assert(block->bbStmtList != nullptr && block->bbStmtList->GetPrevStmt() != nullptr);

        Statement* lastStmt = block->lastStmt();

        noway_assert(lastStmt->GetNextStmt() == nullptr);

        if (lastStmt->GetRootNode()->gtOper == GT_CALL)
        {
            noway_assert(fgRemoveRestOfBlock);

            /* Unconditional throw - transform the basic block into a BBJ_THROW */
            fgConvertBBToThrowBB(block);

#ifdef DEBUG
            if (verbose)
            {
                printf("\nConditional folded at " FMT_BB "\n", block->bbNum);
                printf(FMT_BB " becomes a BBJ_THROW\n", block->bbNum);
            }
#endif
            goto DONE_COND;
        }

        noway_assert(lastStmt->GetRootNode()->gtOper == GT_JTRUE);

        /* Did we fold the conditional */

        noway_assert(lastStmt->GetRootNode()->AsOp()->gtOp1);
        GenTree* condTree;
        condTree = lastStmt->GetRootNode()->AsOp()->gtOp1;
        GenTree* cond;
        cond = condTree->SkipComma();

        if (cond->OperKind() & GTK_CONST)
        {
            /* Yupee - we folded the conditional!
             * Remove the conditional statement */

            noway_assert(cond->gtOper == GT_CNS_INT);
            noway_assert((block->bbNext->countOfInEdges() > 0) && (block->bbJumpDest->countOfInEdges() > 0));

            if (condTree != cond)
            {
                // Preserve any side effects
                assert(condTree->OperIs(GT_COMMA));
                lastStmt->SetRootNode(condTree);
            }
            else
            {
                // no side effects, remove the jump entirely
                fgRemoveStmt(block, lastStmt);
            }
            // block is a BBJ_COND that we are folding the conditional for.
            // bTaken is the path that will always be taken from block.
            // bNotTaken is the path that will never be taken from block.
            //
            BasicBlock* bTaken;
            BasicBlock* bNotTaken;

            if (cond->AsIntCon()->gtIconVal != 0)
            {
                /* JTRUE 1 - transform the basic block into a BBJ_ALWAYS */
                block->bbJumpKind = BBJ_ALWAYS;
                bTaken            = block->bbJumpDest;
                bNotTaken         = block->bbNext;
            }
            else
            {
                /* Unmark the loop if we are removing a backwards branch */
                /* dest block must also be marked as a loop head and     */
                /* We must be able to reach the backedge block           */
                if ((block->bbJumpDest->isLoopHead()) && (block->bbJumpDest->bbNum <= block->bbNum) &&
                    fgReachable(block->bbJumpDest, block))
                {
                    optUnmarkLoopBlocks(block->bbJumpDest, block);
                }

                /* JTRUE 0 - transform the basic block into a BBJ_NONE   */
                block->bbJumpKind = BBJ_NONE;
                bTaken            = block->bbNext;
                bNotTaken         = block->bbJumpDest;
            }

            if (fgHaveValidEdgeWeights)
            {
                // We are removing an edge from block to bNotTaken
                // and we have already computed the edge weights, so
                // we will try to adjust some of the weights
                //
                flowList*   edgeTaken = fgGetPredForBlock(bTaken, block);
                BasicBlock* bUpdated  = nullptr; // non-NULL if we updated the weight of an internal block

                // We examine the taken edge (block -> bTaken)
                // if block has valid profile weight and bTaken does not we try to adjust bTaken's weight
                // else if bTaken has valid profile weight and block does not we try to adjust block's weight
                // We can only adjust the block weights when (the edge block -> bTaken) is the only edge into bTaken
                //
                if (block->hasProfileWeight())
                {
                    // The edge weights for (block -> bTaken) are 100% of block's weight

                    edgeTaken->setEdgeWeights(block->bbWeight, block->bbWeight, bTaken);

                    if (!bTaken->hasProfileWeight())
                    {
                        if ((bTaken->countOfInEdges() == 1) || (bTaken->bbWeight < block->bbWeight))
                        {
                            // Update the weight of bTaken
                            bTaken->inheritWeight(block);
                            bUpdated = bTaken;
                        }
                    }
                }
                else if (bTaken->hasProfileWeight())
                {
                    if (bTaken->countOfInEdges() == 1)
                    {
                        // There is only one in edge to bTaken
                        edgeTaken->setEdgeWeights(bTaken->bbWeight, bTaken->bbWeight, bTaken);

                        // Update the weight of block
                        block->inheritWeight(bTaken);
                        bUpdated = block;
                    }
                }

                if (bUpdated != nullptr)
                {
                    BasicBlock::weight_t newMinWeight;
                    BasicBlock::weight_t newMaxWeight;

                    flowList* edge;
                    // Now fix the weights of the edges out of 'bUpdated'
                    switch (bUpdated->bbJumpKind)
                    {
                        case BBJ_NONE:
                            edge         = fgGetPredForBlock(bUpdated->bbNext, bUpdated);
                            newMaxWeight = bUpdated->bbWeight;
                            newMinWeight = min(edge->edgeWeightMin(), newMaxWeight);
                            edge->setEdgeWeights(newMinWeight, newMaxWeight, bUpdated->bbNext);
                            break;

                        case BBJ_COND:
                            edge         = fgGetPredForBlock(bUpdated->bbNext, bUpdated);
                            newMaxWeight = bUpdated->bbWeight;
                            newMinWeight = min(edge->edgeWeightMin(), newMaxWeight);
                            edge->setEdgeWeights(newMinWeight, newMaxWeight, bUpdated->bbNext);
                            FALLTHROUGH;

                        case BBJ_ALWAYS:
                            edge         = fgGetPredForBlock(bUpdated->bbJumpDest, bUpdated);
                            newMaxWeight = bUpdated->bbWeight;
                            newMinWeight = min(edge->edgeWeightMin(), newMaxWeight);
                            edge->setEdgeWeights(newMinWeight, newMaxWeight, bUpdated->bbNext);
                            break;

                        default:
                            // We don't handle BBJ_SWITCH
                            break;
                    }
                }
            }

            /* modify the flow graph */

            /* Remove 'block' from the predecessor list of 'bNotTaken' */
            fgRemoveRefPred(bNotTaken, block);

#ifdef DEBUG
            if (verbose)
            {
                printf("\nConditional folded at " FMT_BB "\n", block->bbNum);
                printf(FMT_BB " becomes a %s", block->bbNum,
                       block->bbJumpKind == BBJ_ALWAYS ? "BBJ_ALWAYS" : "BBJ_NONE");
                if (block->bbJumpKind == BBJ_ALWAYS)
                {
                    printf(" to " FMT_BB, block->bbJumpDest->bbNum);
                }
                printf("\n");
            }
#endif

            /* if the block was a loop condition we may have to modify
             * the loop table */

            for (unsigned loopNum = 0; loopNum < optLoopCount; loopNum++)
            {
                /* Some loops may have been already removed by
                 * loop unrolling or conditional folding */

                if (optLoopTable[loopNum].lpFlags & LPFLG_REMOVED)
                {
                    continue;
                }

                /* We are only interested in the loop bottom */

                if (optLoopTable[loopNum].lpBottom == block)
                {
                    if (cond->AsIntCon()->gtIconVal == 0)
                    {
                        /* This was a bogus loop (condition always false)
                         * Remove the loop from the table */

                        optLoopTable[loopNum].lpFlags |= LPFLG_REMOVED;
#if FEATURE_LOOP_ALIGN
                        optLoopTable[loopNum].lpFirst->bbFlags &= ~BBF_LOOP_ALIGN;
                        JITDUMP("Removing LOOP_ALIGN flag from bogus loop in " FMT_BB "\n",
                                optLoopTable[loopNum].lpFirst->bbNum);
#endif

#ifdef DEBUG
                        if (verbose)
                        {
                            printf("Removing loop " FMT_LP " (from " FMT_BB " to " FMT_BB ")\n\n", loopNum,
                                   optLoopTable[loopNum].lpFirst->bbNum, optLoopTable[loopNum].lpBottom->bbNum);
                        }
#endif
                    }
                }
            }
        DONE_COND:
            result = true;
        }
    }
    else if (block->bbJumpKind == BBJ_SWITCH)
    {
        noway_assert(block->bbStmtList != nullptr && block->bbStmtList->GetPrevStmt() != nullptr);

        Statement* lastStmt = block->lastStmt();

        noway_assert(lastStmt->GetNextStmt() == nullptr);

        if (lastStmt->GetRootNode()->gtOper == GT_CALL)
        {
            noway_assert(fgRemoveRestOfBlock);

            /* Unconditional throw - transform the basic block into a BBJ_THROW */
            fgConvertBBToThrowBB(block);

#ifdef DEBUG
            if (verbose)
            {
                printf("\nConditional folded at " FMT_BB "\n", block->bbNum);
                printf(FMT_BB " becomes a BBJ_THROW\n", block->bbNum);
            }
#endif
            goto DONE_SWITCH;
        }

        noway_assert(lastStmt->GetRootNode()->gtOper == GT_SWITCH);

        /* Did we fold the conditional */

        noway_assert(lastStmt->GetRootNode()->AsOp()->gtOp1);
        GenTree* condTree;
        condTree = lastStmt->GetRootNode()->AsOp()->gtOp1;
        GenTree* cond;
        cond = condTree->SkipComma();

        if (cond->OperKind() & GTK_CONST)
        {
            /* Yupee - we folded the conditional!
             * Remove the conditional statement */

            noway_assert(cond->gtOper == GT_CNS_INT);

            if (condTree != cond)
            {
                // Preserve any side effects
                assert(condTree->OperIs(GT_COMMA));
                lastStmt->SetRootNode(condTree);
            }
            else
            {
                // no side effects, remove the switch entirely
                fgRemoveStmt(block, lastStmt);
            }

            /* modify the flow graph */

            /* Find the actual jump target */
            unsigned switchVal;
            switchVal = (unsigned)cond->AsIntCon()->gtIconVal;
            unsigned jumpCnt;
            jumpCnt = block->bbJumpSwt->bbsCount;
            BasicBlock** jumpTab;
            jumpTab = block->bbJumpSwt->bbsDstTab;
            bool foundVal;
            foundVal = false;

            for (unsigned val = 0; val < jumpCnt; val++, jumpTab++)
            {
                BasicBlock* curJump = *jumpTab;

                assert(curJump->countOfInEdges() > 0);

                // If val matches switchVal or we are at the last entry and
                // we never found the switch value then set the new jump dest

                if ((val == switchVal) || (!foundVal && (val == jumpCnt - 1)))
                {
                    if (curJump != block->bbNext)
                    {
                        /* transform the basic block into a BBJ_ALWAYS */
                        block->bbJumpKind = BBJ_ALWAYS;
                        block->bbJumpDest = curJump;
                    }
                    else
                    {
                        /* transform the basic block into a BBJ_NONE */
                        block->bbJumpKind = BBJ_NONE;
                    }
                    foundVal = true;
                }
                else
                {
                    /* Remove 'block' from the predecessor list of 'curJump' */
                    fgRemoveRefPred(curJump, block);
                }
            }
#ifdef DEBUG
            if (verbose)
            {
                printf("\nConditional folded at " FMT_BB "\n", block->bbNum);
                printf(FMT_BB " becomes a %s", block->bbNum,
                       block->bbJumpKind == BBJ_ALWAYS ? "BBJ_ALWAYS" : "BBJ_NONE");
                if (block->bbJumpKind == BBJ_ALWAYS)
                {
                    printf(" to " FMT_BB, block->bbJumpDest->bbNum);
                }
                printf("\n");
            }
#endif
        DONE_SWITCH:
            result = true;
        }
    }
    return result;
}

//------------------------------------------------------------------------
// fgMorphBlockStmt: morph a single statement in a block.
//
// Arguments:
//    block - block containing the statement
//    stmt - statement to morph
//    msg - string to identify caller in a dump
//
// Returns:
//    true if 'stmt' was removed from the block.
//  s false if 'stmt' is still in the block (even if other statements were removed).
//
// Notes:
//   Can be called anytime, unlike fgMorphStmts() which should only be called once.
//
bool Compiler::fgMorphBlockStmt(BasicBlock* block, Statement* stmt DEBUGARG(const char* msg))
{
    assert(block != nullptr);
    assert(stmt != nullptr);

    // Reset some ambient state
    fgRemoveRestOfBlock = false;
    compCurBB           = block;
    compCurStmt         = stmt;

    GenTree* morph = fgMorphTree(stmt->GetRootNode());

    // Bug 1106830 - During the CSE phase we can't just remove
    // morph->AsOp()->gtOp2 as it could contain CSE expressions.
    // This leads to a noway_assert in OptCSE.cpp when
    // searching for the removed CSE ref. (using gtFindLink)
    //
    if (!optValnumCSE_phase)
    {
        // Check for morph as a GT_COMMA with an unconditional throw
        if (fgIsCommaThrow(morph, true))
        {
#ifdef DEBUG
            if (verbose)
            {
                printf("Folding a top-level fgIsCommaThrow stmt\n");
                printf("Removing op2 as unreachable:\n");
                gtDispTree(morph->AsOp()->gtOp2);
                printf("\n");
            }
#endif
            // Use the call as the new stmt
            morph = morph->AsOp()->gtOp1;
            noway_assert(morph->gtOper == GT_CALL);
        }

        // we can get a throw as a statement root
        if (fgIsThrow(morph))
        {
#ifdef DEBUG
            if (verbose)
            {
                printf("We have a top-level fgIsThrow stmt\n");
                printf("Removing the rest of block as unreachable:\n");
            }
#endif
            noway_assert((morph->gtFlags & GTF_COLON_COND) == 0);
            fgRemoveRestOfBlock = true;
        }
    }

    stmt->SetRootNode(morph);

    // Can the entire tree be removed?
    bool removedStmt = false;

    // Defer removing statements during CSE so we don't inadvertently remove any CSE defs.
    if (!optValnumCSE_phase)
    {
        removedStmt = fgCheckRemoveStmt(block, stmt);
    }

    // Or this is the last statement of a conditional branch that was just folded?
    if (!removedStmt && (stmt->GetNextStmt() == nullptr) && !fgRemoveRestOfBlock)
    {
        if (fgFoldConditional(block))
        {
            if (block->bbJumpKind != BBJ_THROW)
            {
                removedStmt = true;
            }
        }
    }

    if (!removedStmt)
    {
        // Have to re-do the evaluation order since for example some later code does not expect constants as op1
        gtSetStmtInfo(stmt);

        // Have to re-link the nodes for this statement
        fgSetStmtSeq(stmt);
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("%s %s tree:\n", msg, (removedStmt ? "removed" : "morphed"));
        gtDispTree(morph);
        printf("\n");
    }
#endif

    if (fgRemoveRestOfBlock)
    {
        // Remove the rest of the stmts in the block
        for (Statement* removeStmt : StatementList(stmt->GetNextStmt()))
        {
            fgRemoveStmt(block, removeStmt);
        }

        // The rest of block has been removed and we will always throw an exception.
        //
        // For compDbgCode, we prepend an empty BB as the firstBB, it is BBJ_NONE.
        // We should not convert it to a ThrowBB.
        if ((block != fgFirstBB) || ((fgFirstBB->bbFlags & BBF_INTERNAL) == 0))
        {
            // Convert block to a throw bb
            fgConvertBBToThrowBB(block);
        }

#ifdef DEBUG
        if (verbose)
        {
            printf("\n%s Block " FMT_BB " becomes a throw block.\n", msg, block->bbNum);
        }
#endif
        fgRemoveRestOfBlock = false;
    }

    return removedStmt;
}

/*****************************************************************************
 *
 *  Morph the statements of the given block.
 *  This function should be called just once for a block. Use fgMorphBlockStmt()
 *  for reentrant calls.
 */

void Compiler::fgMorphStmts(BasicBlock* block)
{
    assert(fgGlobalMorph);

    fgRemoveRestOfBlock = false;

    for (Statement* stmt : block->Statements())
    {
        if (fgRemoveRestOfBlock)
        {
            fgRemoveStmt(block, stmt);
            continue;
        }

        fgMorphStmt = stmt;
        compCurStmt = stmt;

#ifdef DEBUG
        unsigned oldHash = verbose ? gtHashValue(stmt->GetRootNode()) : DUMMY_INIT(~0);

        if (verbose)
        {
            printf("\nfgMorphTree " FMT_BB ", " FMT_STMT " (before)\n", block->bbNum, stmt->GetID());
            gtDispTree(stmt->GetRootNode());
        }
#endif

#if (defined(TARGET_AMD64) && !defined(UNIX_AMD64_ABI)) || defined(TARGET_ARM64) || defined(TARGET_X86)
        fgMorphIndirectParams(stmt);
#endif

        GenTree* oldTree = stmt->GetRootNode();

        /* Morph this statement tree */

        GenTree* morphedTree = fgMorphTree(oldTree);

// mark any outgoing arg temps as free so we can reuse them in the next statement.

#ifndef TARGET_X86
        abiFreeAllStructArgTemps();
#endif

        // Has fgMorphStmt been sneakily changed ?

        if ((stmt->GetRootNode() != oldTree) || (block != compCurBB))
        {
            if (stmt->GetRootNode() != oldTree)
            {
                /* This must be tailcall. Ignore 'morphedTree' and carry on with
                the tail-call node */

                morphedTree = stmt->GetRootNode();
            }
            else
            {
                /* This must be a tailcall that caused a GCPoll to get
                injected. We haven't actually morphed the call yet
                but the flag still got set, clear it here...  */
                CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef DEBUG
                morphedTree->gtDebugFlags &= ~GTF_DEBUG_NODE_MORPHED;
#endif
            }

            noway_assert(compTailCallUsed);
            noway_assert(morphedTree->gtOper == GT_CALL);
            GenTreeCall* call = morphedTree->AsCall();
            // Could be
            //   - a fast call made as jmp in which case block will be ending with
            //   BBJ_RETURN (as we need epilog) and marked as containing a jmp.
            //   - a tailcall dispatched via JIT helper, on x86, in which case
            //   block will be ending with BBJ_THROW.
            //   - a tail call dispatched via runtime help (IL stubs), in which
            //   case there will not be any tailcall and the block will be ending
            //   with BBJ_RETURN (as normal control flow)
            noway_assert((call->IsFastTailCall() && (compCurBB->bbJumpKind == BBJ_RETURN) &&
                          ((compCurBB->bbFlags & BBF_HAS_JMP)) != 0) ||
                         (call->IsTailCallViaJitHelper() && (compCurBB->bbJumpKind == BBJ_THROW)) ||
                         (!call->IsTailCall() && (compCurBB->bbJumpKind == BBJ_RETURN)));
        }

#ifdef DEBUG
        if (compStressCompile(STRESS_CLONE_EXPR, 30))
        {
            // Clone all the trees to stress gtCloneExpr()

            if (verbose)
            {
                printf("\nfgMorphTree (stressClone from):\n");
                gtDispTree(morphedTree);
            }

            morphedTree = gtCloneExpr(morphedTree);
            noway_assert(morphedTree != nullptr);

            if (verbose)
            {
                printf("\nfgMorphTree (stressClone to):\n");
                gtDispTree(morphedTree);
            }
        }

        /* If the hash value changes. we modified the tree during morphing */
        if (verbose)
        {
            unsigned newHash = gtHashValue(morphedTree);
            if (newHash != oldHash)
            {
                printf("\nfgMorphTree " FMT_BB ", " FMT_STMT " (after)\n", block->bbNum, stmt->GetID());
                gtDispTree(morphedTree);
            }
        }
#endif

        /* Check for morphedTree as a GT_COMMA with an unconditional throw */
        if (!gtIsActiveCSE_Candidate(morphedTree) && fgIsCommaThrow(morphedTree, true))
        {
            /* Use the call as the new stmt */
            morphedTree = morphedTree->AsOp()->gtOp1;
            noway_assert(morphedTree->gtOper == GT_CALL);
            noway_assert((morphedTree->gtFlags & GTF_COLON_COND) == 0);

            fgRemoveRestOfBlock = true;
        }

        stmt->SetRootNode(morphedTree);

        if (fgRemoveRestOfBlock)
        {
            continue;
        }

        /* Has the statement been optimized away */

        if (fgCheckRemoveStmt(block, stmt))
        {
            continue;
        }

        /* Check if this block ends with a conditional branch that can be folded */

        if (fgFoldConditional(block))
        {
            continue;
        }

        if (ehBlockHasExnFlowDsc(block))
        {
            continue;
        }
    }

    if (fgRemoveRestOfBlock)
    {
        if ((block->bbJumpKind == BBJ_COND) || (block->bbJumpKind == BBJ_SWITCH))
        {
            Statement* first = block->firstStmt();
            noway_assert(first);
            Statement* lastStmt = block->lastStmt();
            noway_assert(lastStmt && lastStmt->GetNextStmt() == nullptr);
            GenTree* last = lastStmt->GetRootNode();

            if (((block->bbJumpKind == BBJ_COND) && (last->gtOper == GT_JTRUE)) ||
                ((block->bbJumpKind == BBJ_SWITCH) && (last->gtOper == GT_SWITCH)))
            {
                GenTree* op1 = last->AsOp()->gtOp1;

                if (op1->OperKind() & GTK_RELOP)
                {
                    /* Unmark the comparison node with GTF_RELOP_JMP_USED */
                    op1->gtFlags &= ~GTF_RELOP_JMP_USED;
                }

                lastStmt->SetRootNode(fgMorphTree(op1));
            }
        }

        /* Mark block as a BBJ_THROW block */
        fgConvertBBToThrowBB(block);
    }

#if FEATURE_FASTTAILCALL
    GenTree* recursiveTailCall = nullptr;
    if (block->endsWithTailCallConvertibleToLoop(this, &recursiveTailCall))
    {
        fgMorphRecursiveFastTailCallIntoLoop(block, recursiveTailCall->AsCall());
    }
#endif

    // Reset this back so that it doesn't leak out impacting other blocks
    fgRemoveRestOfBlock = false;
}

/*****************************************************************************
 *
 *  Morph the blocks of the method.
 *  Returns true if the basic block list is modified.
 *  This function should be called just once.
 */

void Compiler::fgMorphBlocks()
{
#ifdef DEBUG
    if (verbose)
    {
        printf("\n*************** In fgMorphBlocks()\n");
    }
#endif

    /* Since fgMorphTree can be called after various optimizations to re-arrange
     * the nodes we need a global flag to signal if we are during the one-pass
     * global morphing */

    fgGlobalMorph = true;

#if LOCAL_ASSERTION_PROP
    //
    // Local assertion prop is enabled if we are optimized
    //
    optLocalAssertionProp = opts.OptimizationEnabled();

    if (optLocalAssertionProp)
    {
        //
        // Initialize for local assertion prop
        //
        optAssertionInit(true);
    }
#elif ASSERTION_PROP
    //
    // If LOCAL_ASSERTION_PROP is not set
    // and we have global assertion prop
    // then local assertion prop is always off
    //
    optLocalAssertionProp = false;

#endif

    /*-------------------------------------------------------------------------
     * Process all basic blocks in the function
     */

    BasicBlock* block = fgFirstBB;
    noway_assert(block);

    do
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("\nMorphing " FMT_BB " of '%s'\n", block->bbNum, info.compFullName);
        }
#endif

#if LOCAL_ASSERTION_PROP
        if (optLocalAssertionProp)
        {
            //
            // Clear out any currently recorded assertion candidates
            // before processing each basic block,
            // also we must  handle QMARK-COLON specially
            //
            optAssertionReset(0);
        }
#endif
        // Make the current basic block address available globally.
        compCurBB = block;

        // Process all statement trees in the basic block.
        fgMorphStmts(block);

        // Do we need to merge the result of this block into a single return block?
        if ((block->bbJumpKind == BBJ_RETURN) && ((block->bbFlags & BBF_HAS_JMP) == 0))
        {
            if ((genReturnBB != nullptr) && (genReturnBB != block))
            {
                fgMergeBlockReturn(block);
            }
        }

        block = block->bbNext;
    } while (block != nullptr);

    // We are done with the global morphing phase
    fgGlobalMorph = false;
    compCurBB     = nullptr;

    // Under OSR, we no longer need to specially protect the original method entry
    //
    if (opts.IsOSR() && (fgEntryBB != nullptr) && (fgEntryBB->bbFlags & BBF_IMPORTED))
    {
        JITDUMP("OSR: un-protecting original method entry " FMT_BB "\n", fgEntryBB->bbNum);
        assert(fgEntryBB->bbRefs > 0);
        fgEntryBB->bbRefs--;
        // We don't need to remember this block anymore.
        fgEntryBB = nullptr;
    }

#ifdef DEBUG
    if (verboseTrees)
    {
        fgDispBasicBlocks(true);
    }
#endif
}

//------------------------------------------------------------------------
// fgMergeBlockReturn: assign the block return value (if any) into the single return temp
//   and branch to the single return block.
//
// Arguments:
//   block - the block to process.
//
// Notes:
//   A block is not guaranteed to have a last stmt if its jump kind is BBJ_RETURN.
//   For example a method returning void could have an empty block with jump kind BBJ_RETURN.
//   Such blocks do materialize as part of in-lining.
//
//   A block with jump kind BBJ_RETURN does not necessarily need to end with GT_RETURN.
//   It could end with a tail call or rejected tail call or monitor.exit or a GT_INTRINSIC.
//   For now it is safe to explicitly check whether last stmt is GT_RETURN if genReturnLocal
//   is BAD_VAR_NUM.
//
void Compiler::fgMergeBlockReturn(BasicBlock* block)
{
    assert((block->bbJumpKind == BBJ_RETURN) && ((block->bbFlags & BBF_HAS_JMP) == 0));
    assert((genReturnBB != nullptr) && (genReturnBB != block));

    // TODO: Need to characterize the last top level stmt of a block ending with BBJ_RETURN.

    Statement* lastStmt = block->lastStmt();
    GenTree*   ret      = (lastStmt != nullptr) ? lastStmt->GetRootNode() : nullptr;

    if ((ret != nullptr) && ret->OperIs(GT_RETURN) && ((ret->gtFlags & GTF_RET_MERGED) != 0))
    {
        // This return was generated during epilog merging, so leave it alone

        return;
    }

#ifndef TARGET_X86
    if ((info.compFlags & CORINFO_FLG_SYNCH) != 0)
    {
        fgConvertSyncReturnToLeave(block);
    }
    else
#endif
    {
        block->bbJumpKind = BBJ_ALWAYS;
        block->bbJumpDest = genReturnBB;

        fgAddRefPred(genReturnBB, block);
        fgReturnCount--;
    }

    if (genReturnLocal != BAD_VAR_NUM)
    {
        noway_assert(info.retDesc.GetRegCount() != 0);

        noway_assert(ret != nullptr);
        noway_assert(ret->OperIs(GT_RETURN));
        noway_assert(ret->AsUnOp()->gtOp1 != nullptr);

        noway_assert(lastStmt->GetNextStmt() == nullptr);

        // Replace the RETURN with an assignment to the merged return temp.

        GenTree* value = ret->AsUnOp()->GetOp(0);

        // TODO-MIKE-Cleanup: Is this really needed? fgMorphCopyStruct already handles COMMAs
        // but the approach taken here is perhaps preferable. It eliminates the COMMA by
        // extracting its side effect into a separate statement. fgMorphCopyStruct keeps the
        // COMMA but transforms it into an address and adds an OBJ. For locals this is bad
        // because it makes them address exposed. Fortunately it seems that struct locals
        // never appear under COMMAs. Known sources of struct COMMAs are static struct fields,
        // struct array elements and struct returns in synchronized methods.

        while (value->OperIs(GT_COMMA) && varTypeIsStruct(value->GetType()))
        {
            Statement* newStmt = gtNewStmt(value->AsOp()->GetOp(0), lastStmt->GetILOffsetX());
            fgInsertStmtBefore(block, lastStmt, newStmt);
            value = value->AsOp()->GetOp(1);
        }

        // MKREFANY too requires a separate statement since it really generates 2 assignments.
        // TypedReference is not a valid return type so don't bother with it.
        noway_assert(!value->OperIs(GT_MKREFANY));

        unsigned   lclNum = genReturnLocal;
        LclVarDsc* lcl    = lvaGetDesc(lclNum);
        GenTree*   dst;

        if (lcl->IsPromoted() && !value->TypeIs(TYP_STRUCT))
        {
            assert(lcl->GetPromotedFieldCount() == 1);

            lclNum = lcl->GetPromotedFieldLclNum(0);
            lcl    = lvaGetDesc(lclNum);

            dst = gtNewLclvNode(lclNum, lcl->GetType());
        }
        else if (lcl->TypeIs(TYP_STRUCT) && !value->TypeIs(TYP_STRUCT))
        {
            dst = gtNewLclFldNode(lclNum, value->GetType(), 0);
        }
        else
        {
            dst = gtNewLclvNode(lclNum, lcl->GetType());
        }

        GenTree* asg = gtNewAssignNode(dst, value);

        if (varTypeIsStruct(asg->GetType()))
        {
            asg = fgMorphStructAssignment(asg->AsOp());
        }

        lastStmt->SetRootNode(asg);
    }
    else if ((ret != nullptr) && ret->OperIs(GT_RETURN))
    {
        // If the return buffer address is being returned then we don't have a merged return
        // temp because the address is just a LCL_VAR. Otherwise this has to be a VOID RETURN.

        if (info.compRetBuffArg == BAD_VAR_NUM)
        {
            noway_assert(ret->TypeIs(TYP_VOID));
            noway_assert(ret->AsUnOp()->gtOp1 == nullptr);
        }

        noway_assert(lastStmt->GetNextStmt() == nullptr);

        fgRemoveStmt(block, lastStmt);
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("Return block " FMT_BB " now jumps to merged return block " FMT_BB "\n", block->bbNum,
               genReturnBB->bbNum);
        fgTableDispBasicBlock(block);
    }
#endif

    if (block->hasProfileWeight())
    {
        BasicBlock::weight_t const oldWeight = genReturnBB->hasProfileWeight() ? genReturnBB->bbWeight : BB_ZERO_WEIGHT;
        BasicBlock::weight_t const newWeight = oldWeight + block->bbWeight;

        JITDUMP("merging profile weight " FMT_WT " from " FMT_BB " to common return " FMT_BB "\n", block->bbWeight,
                block->bbNum, genReturnBB->bbNum);

        genReturnBB->setBBProfileWeight(newWeight);
        DISPBLOCK(genReturnBB);
    }
}

/*****************************************************************************
 *
 *  Make some decisions about the kind of code to generate.
 */

void Compiler::fgSetOptions()
{
#ifdef DEBUG
    /* Should we force fully interruptible code ? */
    if (JitConfig.JitFullyInt() || compStressCompile(STRESS_GENERIC_VARN, 30))
    {
        noway_assert(!codeGen->isGCTypeFixed());
        SetInterruptible(true);
    }
#endif

    if (opts.compDbgCode)
    {
        assert(!codeGen->isGCTypeFixed());
        SetInterruptible(true); // debugging is easier this way ...
    }

    /* Assume we won't need an explicit stack frame if this is allowed */

    if (compLocallocUsed)
    {
        codeGen->setFramePointerRequired(true);
    }

#ifdef TARGET_X86

    if (compTailCallUsed)
        codeGen->setFramePointerRequired(true);

#endif // TARGET_X86

    if (!opts.genFPopt)
    {
        codeGen->setFramePointerRequired(true);
    }

    // Assert that the EH table has been initialized by now. Note that
    // compHndBBtabAllocCount never decreases; it is a high-water mark
    // of table allocation. In contrast, compHndBBtabCount does shrink
    // if we delete a dead EH region, and if it shrinks to zero, the
    // table pointer compHndBBtab is unreliable.
    assert(compHndBBtabAllocCount >= info.compXcptnsCount);

#ifdef TARGET_X86

    // Note: this case, and the !X86 case below, should both use the
    // !X86 path. This would require a few more changes for X86 to use
    // compHndBBtabCount (the current number of EH clauses) instead of
    // info.compXcptnsCount (the number of EH clauses in IL), such as
    // in ehNeedsShadowSPslots(). This is because sometimes the IL has
    // an EH clause that we delete as statically dead code before we
    // get here, leaving no EH clauses left, and thus no requirement
    // to use a frame pointer because of EH. But until all the code uses
    // the same test, leave info.compXcptnsCount here.
    if (info.compXcptnsCount > 0)
    {
        codeGen->setFramePointerRequiredEH(true);
    }

#else // !TARGET_X86

    if (compHndBBtabCount > 0)
    {
        codeGen->setFramePointerRequiredEH(true);
    }

#endif // TARGET_X86

#ifdef UNIX_X86_ABI
    if (info.compXcptnsCount > 0)
    {
        assert(!codeGen->isGCTypeFixed());
        // Enforce fully interruptible codegen for funclet unwinding
        SetInterruptible(true);
    }
#endif // UNIX_X86_ABI

    if (compMethodRequiresPInvokeFrame())
    {
        codeGen->setFramePointerRequired(true); // Setup of Pinvoke frame currently requires an EBP style frame
    }

    if (info.compPublishStubParam)
    {
        codeGen->setFramePointerRequiredGCInfo(true);
    }

    if (compIsProfilerHookNeeded())
    {
        codeGen->setFramePointerRequired(true);
    }

    if (info.compIsVarArgs)
    {
        // Code that initializes lvaVarargsBaseOfStkArgs requires this to be EBP relative.
        codeGen->setFramePointerRequiredGCInfo(true);
    }

    if (lvaReportParamTypeArg())
    {
        codeGen->setFramePointerRequiredGCInfo(true);
    }

    // printf("method will %s be fully interruptible\n", GetInterruptible() ? "   " : "not");
}

/*****************************************************************************/

GenTree* Compiler::fgInitThisClass()
{
    noway_assert(!compIsForInlining());

    CORINFO_LOOKUP_KIND kind;
    info.compCompHnd->getLocationOfThisType(info.compMethodHnd, &kind);

    if (!kind.needsRuntimeLookup)
    {
        return fgGetSharedCCtor(info.compClassHnd);
    }
    else
    {
#ifdef FEATURE_READYTORUN_COMPILER
        // Only CoreRT understands CORINFO_HELP_READYTORUN_GENERIC_STATIC_BASE. Don't do this on CoreCLR.
        if (opts.IsReadyToRun() && IsTargetAbi(CORINFO_CORERT_ABI))
        {
            CORINFO_RESOLVED_TOKEN resolvedToken;
            memset(&resolvedToken, 0, sizeof(resolvedToken));

            // We are in a shared method body, but maybe we don't need a runtime lookup after all.
            // This covers the case of a generic method on a non-generic type.
            if (!(info.compClassAttr & CORINFO_FLG_SHAREDINST))
            {
                resolvedToken.hClass = info.compClassHnd;
                return impReadyToRunHelperToTree(&resolvedToken, CORINFO_HELP_READYTORUN_STATIC_BASE, TYP_BYREF);
            }

            // We need a runtime lookup.
            GenTree* ctxTree = getRuntimeContextTree(kind.runtimeLookupKind);

            // CORINFO_HELP_READYTORUN_GENERIC_STATIC_BASE with a zeroed out resolvedToken means "get the static
            // base of the class that owns the method being compiled". If we're in this method, it means we're not
            // inlining and there's no ambiguity.
            return impReadyToRunHelperToTree(&resolvedToken, CORINFO_HELP_READYTORUN_GENERIC_STATIC_BASE, TYP_BYREF,
                                             gtNewCallArgs(ctxTree), &kind);
        }
#endif

        // Collectible types requires that for shared generic code, if we use the generic context paramter
        // that we report it. (This is a conservative approach, we could detect some cases particularly when the
        // context parameter is this that we don't need the eager reporting logic.)
        lvaGenericsContextInUse = true;

        switch (kind.runtimeLookupKind)
        {
            case CORINFO_LOOKUP_THISOBJ:
            {
                // This code takes a this pointer; but we need to pass the static method desc to get the right point in
                // the hierarchy
                GenTree* vtTree = gtNewLclvNode(info.compThisArg, TYP_REF);
                vtTree->gtFlags |= GTF_VAR_CONTEXT;
                // Vtable pointer of this object
                vtTree             = gtNewMethodTableLookup(vtTree);
                GenTree* methodHnd = gtNewIconEmbMethHndNode(info.compMethodHnd);

                return gtNewHelperCallNode(CORINFO_HELP_INITINSTCLASS, TYP_VOID, gtNewCallArgs(vtTree, methodHnd));
            }

            case CORINFO_LOOKUP_CLASSPARAM:
            {
                GenTree* vtTree = gtNewLclvNode(info.compTypeCtxtArg, TYP_I_IMPL);
                vtTree->gtFlags |= GTF_VAR_CONTEXT;
                return gtNewHelperCallNode(CORINFO_HELP_INITCLASS, TYP_VOID, gtNewCallArgs(vtTree));
            }

            case CORINFO_LOOKUP_METHODPARAM:
            {
                GenTree* methHndTree = gtNewLclvNode(info.compTypeCtxtArg, TYP_I_IMPL);
                methHndTree->gtFlags |= GTF_VAR_CONTEXT;
                return gtNewHelperCallNode(CORINFO_HELP_INITINSTCLASS, TYP_VOID,
                                           gtNewCallArgs(gtNewIconNode(0), methHndTree));
            }

            default:
                noway_assert(!"Unknown LOOKUP_KIND");
                UNREACHABLE();
        }
    }
}

#ifdef DEBUG
Compiler::fgWalkResult Compiler::fgAssertNoQmark(GenTree** tree, fgWalkData* data)
{
    assert(!(*tree)->IsQmark());
    return WALK_CONTINUE;
}

/*****************************************************************************
 *
 *  Verify that the importer has created GT_QMARK nodes in a way we can
 *  process them. The following is allowed:
 *
 *  1. A top level qmark. Top level qmark is of the form:
 *      a) (bool) ? (void) : (void) OR
 *      b) V0N = (bool) ? (type) : (type)
 *
 *  2. Recursion is allowed at the top level, i.e., a GT_QMARK can be a child
 *     of either op1 of colon or op2 of colon but not a child of any other
 *     operator.
 */
void Compiler::fgPreExpandQmarkChecks(GenTree* expr)
{
    GenTreeLclVar* destLclVar = nullptr;
    GenTree*       topQmark   = fgGetTopLevelQmark(expr, &destLclVar);

    // If the top level Qmark is null, then scan the tree to make sure
    // there are no qmarks within it.
    if (topQmark == nullptr)
    {
        fgWalkTreePre(&expr, fgAssertNoQmark, nullptr);
    }
    else
    {
        // We could probably expand the cond node also, but don't think the extra effort is necessary,
        // so let's just assert the cond node of a top level qmark doesn't have further top level qmarks.
        fgWalkTreePre(&topQmark->AsOp()->gtOp1, fgAssertNoQmark, nullptr);

        fgPreExpandQmarkChecks(topQmark->AsOp()->gtOp2->AsOp()->gtOp1);
        fgPreExpandQmarkChecks(topQmark->AsOp()->gtOp2->AsOp()->gtOp2);
    }
}
#endif // DEBUG

GenTreeQmark* Compiler::fgGetTopLevelQmark(GenTree* expr, GenTreeLclVar** destLclVar)
{
    *destLclVar = nullptr;

    if (expr->OperIs(GT_QMARK))
    {
        return expr->AsQmark();
    }

    if (expr->OperIs(GT_ASG) && expr->AsOp()->GetOp(1)->OperIs(GT_QMARK))
    {
        *destLclVar = expr->AsOp()->GetOp(0)->AsLclVar();
        return expr->AsOp()->GetOp(1)->AsQmark();
    }

    return nullptr;
}

/*********************************************************************************
 *
 *  For a castclass helper call,
 *  Importer creates the following tree:
 *      tmp = (op1 == null) ? op1 : ((*op1 == (cse = op2, cse)) ? op1 : helper());
 *
 *  This method splits the qmark expression created by the importer into the
 *  following blocks: (block, asg, cond1, cond2, helper, remainder)
 *  Notice that op1 is the result for both the conditions. So we coalesce these
 *  assignments into a single block instead of two blocks resulting a nested diamond.
 *
 *                       +---------->-----------+
 *                       |          |           |
 *                       ^          ^           v
 *                       |          |           |
 *  block-->asg-->cond1--+-->cond2--+-->helper--+-->remainder
 *
 *  We expect to achieve the following codegen:
 *     mov      rsi, rdx                           tmp = op1                  // asgBlock
 *     test     rsi, rsi                           goto skip if tmp == null ? // cond1Block
 *     je       SKIP
 *     mov      rcx, 0x76543210                    cns = op2                  // cond2Block
 *     cmp      qword ptr [rsi], rcx               goto skip if *tmp == op2
 *     je       SKIP
 *     call     CORINFO_HELP_CHKCASTCLASS_SPECIAL  tmp = helper(cns, tmp)     // helperBlock
 *     mov      rsi, rax
 *  SKIP:                                                                     // remainderBlock
 *     tmp has the result.
 *
 */
void Compiler::fgExpandQmarkForCastInstOf(BasicBlock* block, Statement* stmt)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("\nExpanding CastInstOf qmark in " FMT_BB " (before)\n", block->bbNum);
        fgDispBasicBlocks(block, block, true);
    }
#endif // DEBUG

    GenTree* expr = stmt->GetRootNode();

    GenTreeLclVar* dst   = nullptr;
    GenTreeQmark*  qmark = fgGetTopLevelQmark(expr, &dst);
    noway_assert(dst != nullptr);

    assert(qmark->gtFlags & GTF_QMARK_CAST_INSTOF);

    // Get cond, true, false exprs for the qmark.
    GenTree* condExpr  = qmark->gtGetOp1();
    GenTree* trueExpr  = qmark->gtGetOp2()->AsColon()->ThenNode();
    GenTree* falseExpr = qmark->gtGetOp2()->AsColon()->ElseNode();

    // Get cond, true, false exprs for the nested qmark.
    GenTree* nestedQmark = falseExpr;
    GenTree* cond2Expr;
    GenTree* true2Expr;
    GenTree* false2Expr;

    if (nestedQmark->gtOper == GT_QMARK)
    {
        cond2Expr  = nestedQmark->gtGetOp1();
        true2Expr  = nestedQmark->gtGetOp2()->AsColon()->ThenNode();
        false2Expr = nestedQmark->gtGetOp2()->AsColon()->ElseNode();

        assert(cond2Expr->gtFlags & GTF_RELOP_QMARK);
        cond2Expr->gtFlags &= ~GTF_RELOP_QMARK;
    }
    else
    {
        // This is a rare case that arises when we are doing minopts and encounter isinst of null
        // gtFoldExpr was still is able to optimize away part of the tree (but not all).
        // That means it does not match our pattern.

        // Rather than write code to handle this case, just fake up some nodes to make it match the common
        // case.  Synthesize a comparison that is always true, and for the result-on-true, use the
        // entire subtree we expected to be the nested question op.

        cond2Expr  = gtNewOperNode(GT_EQ, TYP_INT, gtNewIconNode(0, TYP_I_IMPL), gtNewIconNode(0, TYP_I_IMPL));
        true2Expr  = nestedQmark;
        false2Expr = gtNewIconNode(0, TYP_I_IMPL);
    }
    assert(false2Expr->OperGet() == trueExpr->OperGet());

    // Clear flags as they are now going to be part of JTRUE.
    assert(condExpr->gtFlags & GTF_RELOP_QMARK);
    condExpr->gtFlags &= ~GTF_RELOP_QMARK;

    // Create the chain of blocks. See method header comment.
    // The order of blocks after this is the following:
    //     block ... asgBlock ... cond1Block ... cond2Block ... helperBlock ... remainderBlock
    //
    // We need to remember flags that exist on 'block' that we want to propagate to 'remainderBlock',
    // if they are going to be cleared by fgSplitBlockAfterStatement(). We currently only do this only
    // for the GC safe point bit, the logic being that if 'block' was marked gcsafe, then surely
    // remainderBlock will still be GC safe.
    BasicBlockFlags propagateFlags = block->bbFlags & BBF_GC_SAFE_POINT;
    BasicBlock*     remainderBlock = fgSplitBlockAfterStatement(block, stmt);
    fgRemoveRefPred(remainderBlock, block); // We're going to put more blocks between block and remainderBlock.

    BasicBlock* helperBlock = fgNewBBafter(BBJ_NONE, block, true);
    BasicBlock* cond2Block  = fgNewBBafter(BBJ_COND, block, true);
    BasicBlock* cond1Block  = fgNewBBafter(BBJ_COND, block, true);
    BasicBlock* asgBlock    = fgNewBBafter(BBJ_NONE, block, true);

    remainderBlock->bbFlags |= propagateFlags;

    // These blocks are only internal if 'block' is (but they've been set as internal by fgNewBBafter).
    // If they're not internal, mark them as imported to avoid asserts about un-imported blocks.
    if ((block->bbFlags & BBF_INTERNAL) == 0)
    {
        helperBlock->bbFlags &= ~BBF_INTERNAL;
        cond2Block->bbFlags &= ~BBF_INTERNAL;
        cond1Block->bbFlags &= ~BBF_INTERNAL;
        asgBlock->bbFlags &= ~BBF_INTERNAL;
        helperBlock->bbFlags |= BBF_IMPORTED;
        cond2Block->bbFlags |= BBF_IMPORTED;
        cond1Block->bbFlags |= BBF_IMPORTED;
        asgBlock->bbFlags |= BBF_IMPORTED;
    }

    // Chain the flow correctly.
    fgAddRefPred(asgBlock, block);
    fgAddRefPred(cond1Block, asgBlock);
    fgAddRefPred(cond2Block, cond1Block);
    fgAddRefPred(helperBlock, cond2Block);
    fgAddRefPred(remainderBlock, helperBlock);
    fgAddRefPred(remainderBlock, cond1Block);
    fgAddRefPred(remainderBlock, cond2Block);

    cond1Block->bbJumpDest = remainderBlock;
    cond2Block->bbJumpDest = remainderBlock;

    // Set the weights; some are guesses.
    asgBlock->inheritWeight(block);
    cond1Block->inheritWeight(block);
    cond2Block->inheritWeightPercentage(cond1Block, 50);
    helperBlock->inheritWeightPercentage(cond2Block, 50);

    // Append cond1 as JTRUE to cond1Block
    GenTree*   jmpTree = gtNewOperNode(GT_JTRUE, TYP_VOID, condExpr);
    Statement* jmpStmt = fgNewStmtFromTree(jmpTree, stmt->GetILOffsetX());
    fgInsertStmtAtEnd(cond1Block, jmpStmt);

    // Append cond2 as JTRUE to cond2Block
    jmpTree = gtNewOperNode(GT_JTRUE, TYP_VOID, cond2Expr);
    jmpStmt = fgNewStmtFromTree(jmpTree, stmt->GetILOffsetX());
    fgInsertStmtAtEnd(cond2Block, jmpStmt);

    unsigned  dstLclNum = dst->GetLclNum();
    var_types dstType   = lvaGetDesc(dstLclNum)->GetType();

    assert(varTypeIsI(dstType));

    // AsgBlock should get tmp = op1 assignment.
    trueExpr = gtNewAssignNode(gtNewLclvNode(dstLclNum, dstType), trueExpr);
    fgInsertStmtAtEnd(asgBlock, fgNewStmtFromTree(trueExpr, stmt->GetILOffsetX()));

    // Since we are adding helper in the JTRUE false path, reverse the cond2 and add the helper.
    gtReverseCond(cond2Expr);

    if (true2Expr->IsCall() && true2Expr->AsCall()->IsNoReturn())
    {
        fgConvertBBToThrowBB(helperBlock);
    }
    else
    {
        true2Expr = gtNewAssignNode(gtNewLclvNode(dstLclNum, dstType), true2Expr);
    }

    fgInsertStmtAtEnd(helperBlock, fgNewStmtFromTree(true2Expr, stmt->GetILOffsetX()));
    fgRemoveStmt(block, stmt);

#ifdef DEBUG
    if (verbose)
    {
        printf("\nExpanding CastInstOf qmark in " FMT_BB " (after)\n", block->bbNum);
        fgDispBasicBlocks(block, remainderBlock, true);
    }
#endif // DEBUG
}

/*****************************************************************************
 *
 *  Expand a statement with a top level qmark node. There are three cases, based
 *  on whether the qmark has both "true" and "false" arms, or just one of them.
 *
 *     S0;
 *     C ? T : F;
 *     S1;
 *
 *     Generates ===>
 *
 *                       bbj_always
 *                       +---->------+
 *                 false |           |
 *     S0 -->-- ~C -->-- T   F -->-- S1
 *              |            |
 *              +--->--------+
 *              bbj_cond(true)
 *
 *     -----------------------------------------
 *
 *     S0;
 *     C ? T : NOP;
 *     S1;
 *
 *     Generates ===>
 *
 *                 false
 *     S0 -->-- ~C -->-- T -->-- S1
 *              |                |
 *              +-->-------------+
 *              bbj_cond(true)
 *
 *     -----------------------------------------
 *
 *     S0;
 *     C ? NOP : F;
 *     S1;
 *
 *     Generates ===>
 *
 *                false
 *     S0 -->-- C -->-- F -->-- S1
 *              |               |
 *              +-->------------+
 *              bbj_cond(true)
 *
 *  If the qmark assigns to a variable, then create tmps for "then"
 *  and "else" results and assign the temp to the variable as a writeback step.
 */
void Compiler::fgExpandQmarkStmt(BasicBlock* block, Statement* stmt)
{
    GenTree* expr = stmt->GetRootNode();

    GenTreeLclVar* dst   = nullptr;
    GenTreeQmark*  qmark = fgGetTopLevelQmark(expr, &dst);

    if (qmark == nullptr)
    {
        return;
    }

    if ((qmark->gtFlags & GTF_QMARK_CAST_INSTOF) != 0)
    {
        fgExpandQmarkForCastInstOf(block, stmt);
        return;
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("\nExpanding top-level qmark in " FMT_BB " (before)\n", block->bbNum);
        fgDispBasicBlocks(block, block, true);
    }
#endif // DEBUG

    // Retrieve the operands.
    GenTree* condExpr  = qmark->gtGetOp1();
    GenTree* trueExpr  = qmark->gtGetOp2()->AsColon()->ThenNode();
    GenTree* falseExpr = qmark->gtGetOp2()->AsColon()->ElseNode();

    assert(condExpr->gtFlags & GTF_RELOP_QMARK);
    condExpr->gtFlags &= ~GTF_RELOP_QMARK;

    assert(!varTypeIsFloating(condExpr->TypeGet()));

    bool hasTrueExpr  = (trueExpr->OperGet() != GT_NOP);
    bool hasFalseExpr = (falseExpr->OperGet() != GT_NOP);
    assert(hasTrueExpr || hasFalseExpr); // We expect to have at least one arm of the qmark!

    // Create remainder, cond and "else" blocks. After this, the blocks are in this order:
    //     block ... condBlock ... elseBlock ... remainderBlock
    //
    // We need to remember flags that exist on 'block' that we want to propagate to 'remainderBlock',
    // if they are going to be cleared by fgSplitBlockAfterStatement(). We currently only do this only
    // for the GC safe point bit, the logic being that if 'block' was marked gcsafe, then surely
    // remainderBlock will still be GC safe.
    BasicBlockFlags propagateFlags = block->bbFlags & BBF_GC_SAFE_POINT;
    BasicBlock*     remainderBlock = fgSplitBlockAfterStatement(block, stmt);
    fgRemoveRefPred(remainderBlock, block); // We're going to put more blocks between block and remainderBlock.

    BasicBlock* condBlock = fgNewBBafter(BBJ_COND, block, true);
    BasicBlock* elseBlock = fgNewBBafter(BBJ_NONE, condBlock, true);

    // These blocks are only internal if 'block' is (but they've been set as internal by fgNewBBafter).
    // If they're not internal, mark them as imported to avoid asserts about un-imported blocks.
    if ((block->bbFlags & BBF_INTERNAL) == 0)
    {
        condBlock->bbFlags &= ~BBF_INTERNAL;
        elseBlock->bbFlags &= ~BBF_INTERNAL;
        condBlock->bbFlags |= BBF_IMPORTED;
        elseBlock->bbFlags |= BBF_IMPORTED;
    }

    remainderBlock->bbFlags |= propagateFlags;

    condBlock->inheritWeight(block);

    fgAddRefPred(condBlock, block);
    fgAddRefPred(elseBlock, condBlock);
    fgAddRefPred(remainderBlock, elseBlock);

    BasicBlock* thenBlock = nullptr;
    if (hasTrueExpr && hasFalseExpr)
    {
        //                       bbj_always
        //                       +---->------+
        //                 false |           |
        //     S0 -->-- ~C -->-- T   F -->-- S1
        //              |            |
        //              +--->--------+
        //              bbj_cond(true)
        //
        gtReverseCond(condExpr);
        condBlock->bbJumpDest = elseBlock;

        thenBlock             = fgNewBBafter(BBJ_ALWAYS, condBlock, true);
        thenBlock->bbJumpDest = remainderBlock;
        if ((block->bbFlags & BBF_INTERNAL) == 0)
        {
            thenBlock->bbFlags &= ~BBF_INTERNAL;
            thenBlock->bbFlags |= BBF_IMPORTED;
        }

        fgAddRefPred(thenBlock, condBlock);
        fgAddRefPred(remainderBlock, thenBlock);

        thenBlock->inheritWeightPercentage(condBlock, 50);
        elseBlock->inheritWeightPercentage(condBlock, 50);
    }
    else if (hasTrueExpr)
    {
        //                 false
        //     S0 -->-- ~C -->-- T -->-- S1
        //              |                |
        //              +-->-------------+
        //              bbj_cond(true)
        //
        gtReverseCond(condExpr);
        condBlock->bbJumpDest = remainderBlock;
        fgAddRefPred(remainderBlock, condBlock);
        // Since we have no false expr, use the one we'd already created.
        thenBlock = elseBlock;
        elseBlock = nullptr;

        thenBlock->inheritWeightPercentage(condBlock, 50);
    }
    else if (hasFalseExpr)
    {
        //                false
        //     S0 -->-- C -->-- F -->-- S1
        //              |               |
        //              +-->------------+
        //              bbj_cond(true)
        //
        condBlock->bbJumpDest = remainderBlock;
        fgAddRefPred(remainderBlock, condBlock);

        elseBlock->inheritWeightPercentage(condBlock, 50);
    }

    GenTree*   jmpTree = gtNewOperNode(GT_JTRUE, TYP_VOID, qmark->gtGetOp1());
    Statement* jmpStmt = fgNewStmtFromTree(jmpTree, stmt->GetILOffsetX());
    fgInsertStmtAtEnd(condBlock, jmpStmt);

    // Remove the original qmark statement.
    fgRemoveStmt(block, stmt);

    if (dst == nullptr)
    {
        assert(qmark->TypeIs(TYP_VOID));
    }
    else
    {
        unsigned  lclNum  = dst->GetLclNum();
        var_types lclType = lvaGetDesc(lclNum)->GetType();

        // Other non-struct types should work but such QMARKs are never generated by
        // the JIT so this cannot be tested. Small int types might have issues.

        assert(varTypeIsI(lclType));
        assert(lclType == qmark->GetType());

        if (hasTrueExpr)
        {
            trueExpr = gtNewAssignNode(gtNewLclvNode(lclNum, lclType), trueExpr);
        }

        if (hasFalseExpr)
        {
            falseExpr = gtNewAssignNode(gtNewLclvNode(lclNum, lclType), falseExpr);
        }
    }

    if (hasTrueExpr)
    {
        fgInsertStmtAtEnd(thenBlock, fgNewStmtFromTree(trueExpr, stmt->GetILOffsetX()));
    }

    if (hasFalseExpr)
    {
        fgInsertStmtAtEnd(elseBlock, fgNewStmtFromTree(falseExpr, stmt->GetILOffsetX()));
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("\nExpanding top-level qmark in " FMT_BB " (after)\n", block->bbNum);
        fgDispBasicBlocks(block, remainderBlock, true);
    }
#endif // DEBUG
}

void Compiler::fgExpandQmarkNodes()
{
    if (compQmarkUsed)
    {
        for (BasicBlock* block = fgFirstBB; block != nullptr; block = block->bbNext)
        {
            for (Statement* stmt : block->Statements())
            {
                GenTree* expr = stmt->GetRootNode();
                INDEBUG(fgPreExpandQmarkChecks(expr);)
                fgExpandQmarkStmt(block, stmt);
            }
        }

        INDEBUG(fgPostExpandQmarkChecks();)
    }

    compQmarkRationalized = true;
}

#ifdef DEBUG
/*****************************************************************************
 *
 *  Make sure we don't have any more GT_QMARK nodes.
 *
 */
void Compiler::fgPostExpandQmarkChecks()
{
    for (BasicBlock* block = fgFirstBB; block != nullptr; block = block->bbNext)
    {
        for (Statement* stmt : block->Statements())
        {
            GenTree* expr = stmt->GetRootNode();
            fgWalkTreePre(&expr, fgAssertNoQmark, nullptr);
        }
    }
}
#endif

//------------------------------------------------------------------------
// fgAddFieldSeqForZeroOffset:
//    Associate a fieldSeq (with a zero offset) with the GenTree node 'addr'
//
// Arguments:
//    addr - A GenTree node
//    fieldSeqZero - a fieldSeq (with a zero offset)
//
// Notes:
//    A sequence of FIELD nodes usually gets converted to a LCL_FLD or an indirection
//    where the address is ADD(objRef, CNS_INT(fieldOffset)). In either case we can
//    record the field sequence in the LCL_FLD/CNS_INT node, if one exists. If the
//    field offset is 0 the ADD node may get folded to just objRef and then we have
//    no place where to record the field sequence.
//
//    This can happen for local field accesses that haven't yet been converted to
//    LCL_FLD in LocalAddressVisitor or for static fields (the first static field
//    in an R2R compiled assembly hits this case so it's rather rare).
//
//    For now such a field sequence is recorded in a hash table where the key is the
//    address tree node, usually an ADDR(LCL_VAR) tree for the first field of a local
//    struct variable or a helper call for static field.
//
void Compiler::fgAddFieldSeqForZeroOffset(GenTree* addr, FieldSeqNode* fieldSeqZero)
{
    assert(addr->TypeIs(TYP_BYREF, TYP_I_IMPL, TYP_REF));
    addr = addr->SkipComma();
    assert(addr->TypeIs(TYP_BYREF, TYP_I_IMPL, TYP_REF));
    assert(fieldSeqZero != nullptr);

#ifdef DEBUG
    if (verbose)
    {
        printf("\nfgAddFieldSeqForZeroOffset ");
        dmpFieldSeqFields(fieldSeqZero);
        printf(" to address\n");
        gtDispTree(addr, nullptr, nullptr, true, false);
    }
#endif

    FieldSeqNode* fieldSeqUpdate   = fieldSeqZero;
    GenTree*      fieldSeqNode     = addr;
    bool          fieldSeqRecorded = false;

    switch (addr->OperGet())
    {
        case GT_CNS_INT:
            fieldSeqUpdate = GetFieldSeqStore()->Append(addr->AsIntCon()->GetFieldSeq(), fieldSeqZero);
            addr->AsIntCon()->SetFieldSeq(fieldSeqUpdate);
            fieldSeqRecorded = true;
            break;

        case GT_ADDR:
            if (addr->AsOp()->gtOp1->OperGet() == GT_LCL_FLD)
            {
                fieldSeqNode = addr->AsOp()->gtOp1;

                GenTreeLclFld* lclFld = addr->AsOp()->gtOp1->AsLclFld();
                fieldSeqUpdate        = GetFieldSeqStore()->Append(lclFld->GetFieldSeq(), fieldSeqZero);
                lclFld->SetFieldSeq(fieldSeqUpdate);
                fieldSeqRecorded = true;
            }
            break;

        case GT_ADD:
            if (addr->AsOp()->GetOp(0)->IsIntCon())
            {
                fieldSeqNode = addr->AsOp()->GetOp(0);

                fieldSeqUpdate = GetFieldSeqStore()->Append(fieldSeqNode->AsIntCon()->GetFieldSeq(), fieldSeqZero);
                fieldSeqNode->AsIntCon()->SetFieldSeq(fieldSeqUpdate);
                fieldSeqRecorded = true;
            }
            else if (addr->AsOp()->GetOp(1)->IsIntCon())
            {
                fieldSeqNode = addr->AsOp()->GetOp(1);

                fieldSeqUpdate = GetFieldSeqStore()->Append(fieldSeqNode->AsIntCon()->GetFieldSeq(), fieldSeqZero);
                fieldSeqNode->AsIntCon()->SetFieldSeq(fieldSeqUpdate);
                fieldSeqRecorded = true;
            }
            break;

        default:
            break;
    }

    if (!fieldSeqRecorded)
    {
        // Record in the general zero-offset map.

        // The "addr" node might already be annotated with a zero-offset field sequence.
        FieldSeqNode* existingFieldSeq = nullptr;
        if (GetZeroOffsetFieldMap()->Lookup(addr, &existingFieldSeq))
        {
            // Append the zero field sequences
            fieldSeqUpdate = GetFieldSeqStore()->Append(existingFieldSeq, fieldSeqZero);
        }
        // Overwrite the field sequence annotation for op1
        GetZeroOffsetFieldMap()->Set(addr, fieldSeqUpdate, NodeToFieldSeqMap::Overwrite);
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("     (After)\n");
        gtDispTree(fieldSeqNode, nullptr, nullptr, true, false);
    }
#endif
}

//------------------------------------------------------------------------
// fgCheckStmtAfterTailCall: check that statements after the tail call stmt
// candidate are in one of expected forms, that are desctibed below.
//
// Return Value:
//    'true' if stmts are in the expected form, else 'false'.
//
bool Compiler::fgCheckStmtAfterTailCall()
{

    // For void calls, we would have created a GT_CALL in the stmt list.
    // For non-void calls, we would have created a GT_RETURN(GT_CAST(GT_CALL)).
    // For calls returning structs, we would have a void call, followed by a void return.
    // For debuggable code, it would be an assignment of the call to a temp
    // We want to get rid of any of this extra trees, and just leave
    // the call.
    Statement* callStmt = fgMorphStmt;

    Statement* nextMorphStmt = callStmt->GetNextStmt();

    // Check that the rest stmts in the block are in one of the following pattern:
    //  1) ret(void)
    //  2) ret(cast*(callResultLclVar))
    //  3) lclVar = callResultLclVar, the actual ret(lclVar) in another block
    //  4) nop
    if (nextMorphStmt != nullptr)
    {
        GenTree* callExpr = callStmt->GetRootNode();
        if (callExpr->gtOper != GT_ASG)
        {
            // The next stmt can be GT_RETURN(TYP_VOID) or GT_RETURN(lclVar),
            // where lclVar was return buffer in the call for structs or simd.
            Statement* retStmt = nextMorphStmt;
            GenTree*   retExpr = retStmt->GetRootNode();
            noway_assert(retExpr->gtOper == GT_RETURN);

            nextMorphStmt = retStmt->GetNextStmt();
        }
        else
        {
            noway_assert(callExpr->gtGetOp1()->OperIsLocal());
            unsigned callResultLclNumber = callExpr->gtGetOp1()->AsLclVarCommon()->GetLclNum();

#if FEATURE_TAILCALL_OPT_SHARED_RETURN

            // We can have a chain of assignments from the call result to
            // various inline return spill temps. These are ok as long
            // as the last one ultimately provides the return value or is ignored.
            //
            // And if we're returning a small type we may see a cast
            // on the source side.
            while ((nextMorphStmt != nullptr) && (nextMorphStmt->GetRootNode()->OperIs(GT_ASG, GT_NOP)))
            {
                if (nextMorphStmt->GetRootNode()->OperIs(GT_NOP))
                {
                    nextMorphStmt = nextMorphStmt->GetNextStmt();
                    continue;
                }
                Statement* moveStmt = nextMorphStmt;
                GenTree*   moveExpr = nextMorphStmt->GetRootNode();
                GenTree*   moveDest = moveExpr->gtGetOp1();
                noway_assert(moveDest->OperIsLocal());

                // Tunnel through any casts on the source side.
                GenTree* moveSource = moveExpr->gtGetOp2();
                while (moveSource->OperIs(GT_CAST))
                {
                    noway_assert(!moveSource->gtOverflow());
                    moveSource = moveSource->gtGetOp1();
                }
                noway_assert(moveSource->OperIsLocal());

                // Verify we're just passing the value from one local to another
                // along the chain.
                const unsigned srcLclNum = moveSource->AsLclVarCommon()->GetLclNum();
                noway_assert(srcLclNum == callResultLclNumber);
                const unsigned dstLclNum = moveDest->AsLclVarCommon()->GetLclNum();
                callResultLclNumber      = dstLclNum;

                nextMorphStmt = moveStmt->GetNextStmt();
            }
            if (nextMorphStmt != nullptr)
#endif
            {
                Statement* retStmt = nextMorphStmt;
                GenTree*   retExpr = nextMorphStmt->GetRootNode();
                noway_assert(retExpr->gtOper == GT_RETURN);

                GenTree* treeWithLcl = retExpr->gtGetOp1();
                while (treeWithLcl->gtOper == GT_CAST)
                {
                    noway_assert(!treeWithLcl->gtOverflow());
                    treeWithLcl = treeWithLcl->gtGetOp1();
                }

                noway_assert(callResultLclNumber == treeWithLcl->AsLclVarCommon()->GetLclNum());

                nextMorphStmt = retStmt->GetNextStmt();
            }
        }
    }
    return nextMorphStmt == nullptr;
}

//------------------------------------------------------------------------
// fgCanTailCallViaJitHelper: check whether we can use the faster tailcall
// JIT helper on x86.
//
// Return Value:
//    'true' if we can; or 'false' if we should use the generic tailcall mechanism.
//
bool Compiler::fgCanTailCallViaJitHelper()
{
#ifndef TARGET_X86
    // On anything except X86 we have no faster mechanism available.
    return false;
#else
    // The JIT helper does not properly handle the case where localloc was used.
    if (compLocallocUsed)
        return false;

    return true;
#endif
}

#ifdef FEATURE_HW_INTRINSICS
GenTree* Compiler::fgMorphHWIntrinsic(GenTreeHWIntrinsic* tree)
{
#ifdef TARGET_AMD64
    if (tree->TypeIs(TYP_LONG, TYP_DOUBLE) && (tree->GetIntrinsic() == NI_Vector128_GetElement) &&
        tree->GetOp(1)->IsIntegralConst(0))
    {
        if (GenTreeHWIntrinsic* create = tree->GetOp(0)->IsHWIntrinsic())
        {
            if ((create->GetIntrinsic() == NI_Vector128_Create) && (create->GetSimdBaseType() == TYP_FLOAT) &&
                (create->GetNumOps() >= 2) && create->GetOp(0)->IsDblCon() && create->GetOp(1)->IsDblCon())
            {
                uint64_t bits0 = create->GetOp(0)->AsDblCon()->GetFloatBits();
                uint64_t bits1 = create->GetOp(1)->AsDblCon()->GetFloatBits();
                uint64_t bits  = bits0 | (bits1 << 32);

                if (tree->TypeIs(TYP_LONG))
                {
                    return tree->ChangeToIntCon(static_cast<ssize_t>(bits));
                }

                return tree->ChangeToDblCon(TYP_DOUBLE, jitstd::bit_cast<double>(bits));
            }
        }
    }
#endif

    GenTreeFlags sideEffects = GTF_NONE;

    if (tree->AsHWIntrinsic()->OperIsMemoryLoadOrStore())
    {
        sideEffects |= GTF_EXCEPT | GTF_GLOB_REF;
        if (tree->AsHWIntrinsic()->OperIsMemoryStore())
        {
            sideEffects |= GTF_ASG;
        }
    }

    for (GenTreeHWIntrinsic::Use& use : tree->AsHWIntrinsic()->Uses())
    {
        use.SetNode(fgMorphTree(use.GetNode()));
        sideEffects |= use.GetNode()->GetSideEffects();
    }

    tree->SetSideEffects(sideEffects);

    return tree;
}
#endif
