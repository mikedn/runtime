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
#include "allocacheck.h"
#include "valuenum.h"

// Convert the given node into a call to the specified helper passing
// the given argument list. Also tries to fold constants.
GenTree* Compiler::fgMorphCastIntoHelper(GenTreeCast* cast, int helper)
{
    GenTree* src = cast->GetOp(0);

    if (src->IsNumericConst())
    {
        GenTree* folded = gtFoldExprConst(cast); // This may not fold the constant (NaN ...)

        if (folded != cast)
        {
            return fgMorphTree(folded);
        }

        if (folded->IsNumericConst())
        {
            return folded;
        }

        noway_assert(cast->OperIs(GT_CAST));
        noway_assert(cast->GetOp(0) == src);
    }

    if (src->TypeIs(TYP_FLOAT))
    {
        // All floating point cast helpers work only with DOUBLE.
        src = gtNewCastNode(src, false, TYP_DOUBLE);
    }

    // GenTreeCast nodes are small so they cannot be converted to calls in place. It may
    // be possible to have the importer create large cast nodes as needed but the number
    // of cast nodes that need to be converted to helper calls is typically very small
    // (e.g. 0.03% in corelib x86) so it's not worth the risk. At least in theory, if 2
    // cast nodes somehow combine into one and one is large and the other small then the
    // combining code would need to be careful to preserve the large node, not the small
    // node. Cast morphing code is convoluted enough as it is.
    GenTree* call = new (this, GT_CALL) GenTreeCast(cast DEBUGARG(/*largeNode*/ true));
    INDEBUG(call->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)
    return fgMorphIntoHelperCall(call, helper, gtNewCallArgs(src));
}

/*****************************************************************************
 *
 *  Convert the given node into a call to the specified helper passing
 *  the given argument list.
 */

GenTreeCall* Compiler::fgMorphIntoHelperCall(GenTree* tree, int helper, GenTreeCall::Use* args, bool morphArgs)
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
#ifdef UNIX_X86_ABI
    call->gtFlags |= GTF_CALL_POP_ARGS;
#endif

#if DEBUG
    call->gtInlineObservation = InlineObservation::CALLSITE_IS_CALL_TO_HELPER;
    call->callSig             = nullptr;
#endif

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
#endif

    if (call->CallMayThrow(this))
    {
        call->gtFlags |= GTF_EXCEPT;
    }
    else
    {
        call->gtFlags &= ~GTF_EXCEPT;
    }

    call->gtFlags |= GTF_CALL;

    for (GenTreeCall::Use& use : GenTreeCall::UseList(args))
    {
        call->gtFlags |= use.GetNode()->GetSideEffects();
    }

    if (morphArgs)
    {
        fgMorphArgs(call);
    }

    return call;
}

GenTree* Compiler::fgMorphCast(GenTreeCast* cast)
{
    GenTree*  src     = cast->GetOp(0);
    var_types srcType = varActualType(src->GetType());
    var_types dstType = cast->GetCastType();

    if (cast->TypeIs(TYP_FLOAT) && src->TypeIs(TYP_DOUBLE) && src->OperIs(GT_CAST))
    {
        // Optimization: conv.r4(conv.r8(?)) -> conv.r4(d)
        // This happens semi-frequently because there is no IL 'conv.r4.un'

        cast->gtFlags &= ~GTF_UNSIGNED;
        cast->gtFlags |= src->gtFlags & GTF_UNSIGNED;
        src = src->AsCast()->GetOp(0);
        cast->SetOp(0, src);
        srcType = varActualType(src->GetType());
    }

    if (varTypeIsSmall(dstType) && src->IsCast() && varTypeIsSmall(src->GetType()) &&
        (varTypeSize(src->GetType()) >= varTypeSize(dstType)) && !cast->gtOverflow() && !src->gtOverflow()
#ifndef TARGET_64BIT
        && !src->AsCast()->GetOp(0)->TypeIs(TYP_LONG)
#endif
            )
    {
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
        LclVarDsc* lcl = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("Cast away GC"));
        src->SetType(TYP_I_IMPL);
        GenTree* asg = gtNewAssignNode(gtNewLclvNode(lcl, TYP_I_IMPL), src);
        src->SetType(srcType);
        src = gtNewLclvNode(lcl, TYP_I_IMPL);
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

        src = gtNewCastNode(src, cast->IsUnsigned(), TYP_INT);
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
                src = gtNewCastNode(src, true, TYP_LONG);
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
            src = gtNewCastNode(src, true, TYP_LONG);
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
                helper = gtNewCastNode(helper, false, TYP_FLOAT);
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
                src->AsOp()->SetOp(0, gtNewCastNode(src->AsOp()->GetOp(0), false, TYP_INT));

                if (src->AsOp()->gtOp2 != nullptr)
                {
                    src->AsOp()->SetOp(1, gtNewCastNode(src->AsOp()->GetOp(1), false, TYP_INT));
                }

#ifndef TARGET_64BIT
                if (src->OperIs(GT_MUL) && src->TypeIs(TYP_LONG))
                {
                    GenTreeCast* op1 = src->AsOp()->GetOp(0)->IsCast();
                    GenTreeCast* op2 = src->AsOp()->GetOp(1)->IsCast();

                    if (op1 != nullptr)
                    {
                        op1->ClearDoNotCSE();
                    }

                    if (op2 != nullptr)
                    {
                        op1->ClearDoNotCSE();
                    }
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

    return fgMorphCastPost(cast);
}

GenTree* Compiler::fgMorphCastPost(GenTreeCast* cast)
{
    GenTree*  src     = cast->GetOp(0);
    var_types srcType = src->GetType();
    var_types dstType = cast->GetCastType();

    if (varTypeIsIntegral(srcType) && varTypeIsIntegral(dstType))
    {
        if (src->OperIs(GT_LCL_VAR) && varTypeIsSmall(dstType))
        {
            LclVarDsc* lcl = src->AsLclVar()->GetLcl();

            if ((lcl->GetType() == dstType) && lcl->lvNormalizeOnStore())
            {
                assert(src->TypeIs(TYP_INT, dstType));

                goto REMOVE_CAST;
            }
        }

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

            srcType = varTypeToUnsigned(srcType);
        }

        bool     unsignedSrc = varTypeIsUnsigned(srcType);
        bool     unsignedDst = varTypeIsUnsigned(dstType);
        unsigned srcSize     = varTypeSize(srcType);
        unsigned dstSize     = varTypeSize(dstType);

        // For same sized casts with
        //    the same signs or non-overflow cast we discard them as well
        if (srcSize == dstSize)
        {
            // This should have been handled above
            noway_assert(varTypeIsGC(srcType) == varTypeIsGC(dstType));

            if (unsignedSrc == unsignedDst)
            {
                goto REMOVE_CAST;
            }

            if (!cast->gtOverflow())
            {
                if (!varTypeIsSmall(srcType))
                {
                    goto REMOVE_CAST;
                }

                // For small type casts, when necessary we force
                // the src operand to the dstType and allow the
                // implied load from memory to perform the casting
                if (src->OperIs(GT_IND, GT_LCL_FLD))
                {
                    src->SetType(dstType);
                    // We're changing the type here so we need to update the VN;
                    // in other cases we discard the cast without modifying oper
                    // so the VN doesn't change.
                    src->SetVNP(cast->GetVNP());

                    goto REMOVE_CAST;
                }
            }
        }
        else if (srcSize < dstSize) // widening cast
        {
            if ((dstSize == 4) && (!cast->gtOverflow() || !unsignedDst || unsignedSrc))
            {
                goto REMOVE_CAST;
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
            if (!cast->gtOverflow() && opts.OptEnabled(CLFLG_TREETRANS) &&
                fgMorphNarrowTree(src, srcType, dstType, cast->GetVNP(), false))
            {
                fgMorphNarrowTree(src, srcType, dstType, cast->GetVNP(), true);

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
#ifndef TARGET_64BIT
        case GT_CNS_LNG:
#endif
        case GT_CNS_DBL:
        {
            GenTree* folded = gtFoldExprConst(cast); // This may not fold the constant (NaN ...)

            // Did we get a comma throw as a result of gtFoldExprConst?
            if (folded != cast)
            {
                noway_assert(fgIsCommaThrow(folded DEBUGARG(false)));
                folded->AsOp()->SetOp(0, fgMorphTree(folded->AsOp()->GetOp(0)));
                INDEBUG(folded->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
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
            if (fgIsCommaThrow(src DEBUGARG(false)))
            {
                GenTree* val = src->AsOp()->GetOp(1);

                if (varTypeIsFloating(cast->GetType()))
                {
                    val->ChangeToDblCon(cast->GetType(), 0.0);
                    src->SetType(cast->GetType());

                    if (vnStore != nullptr)
                    {
                        val->SetVNP(ValueNumPair{vnStore->VNForDblCon(cast->GetType(), 0.0)});
                    }
                }
                else if (cast->TypeIs(TYP_LONG))
                {
                    val->ChangeOperConst(GT_CNS_NATIVELONG);
                    val->AsIntConCommon()->SetLngValue(0);
                    val->SetType(TYP_LONG);
                    src->SetType(TYP_LONG);

                    if (vnStore != nullptr)
                    {
                        val->SetVNP(ValueNumPair{vnStore->VNForLongCon(0)});
                    }
                }
                else
                {
                    val->ChangeToIntCon(TYP_INT, 0);
                    src->SetType(TYP_INT);

                    if (vnStore != nullptr)
                    {
                        val->SetVNP(ValueNumPair{vnStore->VNForIntCon(0)});
                    }
                }

                return src;
            }
            break;

        default:
            break;
    }

    if (cast->gtOverflow())
    {
        fgGetThrowHelperBlock(ThrowHelperKind::Overflow, fgMorphBlock);
    }

    return cast;

REMOVE_CAST:
    // Here we've eliminated the cast, so just return its operand
    DEBUG_DESTROY_NODE(cast);
    return src;
}

// See if the given tree can be computed in the given precision (which must
// be smaller than the type of the tree for this to make sense). If 'doit'
// is false, we merely check to see whether narrowing is possible; if we
// get called with 'doit' being true, we actually perform the narrowing.
bool Compiler::fgMorphNarrowTree(
    GenTree* const tree, const var_types srct, const var_types dstt, const ValueNumPair vnpNarrow, const bool doit)
{
    assert(varActualType(tree->GetType()) == varActualType(srct));
    assert(varActualTypeIsInt(dstt));
    const unsigned dstSize = varTypeSize(dstt);
    assert(dstSize < varTypeSize(srct));

    // TODO-MIKE-Review: This stuff is rather dubious. It mixes up 2 different optimizations
    // and as a result it kind of sucks at both:
    //   1. Attempts to remove a redundant narrowing cast (e.g. (byte)(a & 2) doesn't really
    //      need a cast to byte). This is useful and likely doesn't negatively impact other
    //      optimizations, but it's rather limited so the attempt fails in many cases.
    //   2. Narrows LONG expressions to INT. This is obviously useful on 32 bit targets but
    //      on 64 bit targets the situation is a bit more convoluted:
    //      a. on x64, 32 bit instructions can have shorter encoding, since they don't need
    //         a REX prefix. But the REX prefix may be needed for other reasons so code size
    //         reduction isn't guaranteed.
    //      b. on ARM64 there's no difference in instruction encoding length.
    //      c. 32 bit instructions can be more efficient. But the obvious one, division,
    //         cannot usually be narrowed down.
    //      d. this transform blocks CSE - if the same LONG expression exists somewhere else
    //         and it is not casted to INT, it won't be transformed and prevent CSE. At the
    //         same time, this transform is unlikely to enable other optimizations so it
    //         it should probably be done during lowering.
    //
    // These 2 optimzations interact poorly - the second can be done in many more cases than
    // the first one. For example, LONG ADD can always be narrowed to INT but the first
    // optimization rejects such narrowing when casting to small int types. What we need to
    // do is narrow the tree but block the small int cast removal.
    //
    // Other dubious that happens here is the narrowing of LCL_VAR, LCL_FLD and IND nodes.
    // In particular, LCL_VARs are narrowed from LONG to INT, requiring all sorts of special
    // casing downstream. This is highly questionable, it could be done in lowering and the
    // only reason to do it here is to some potential throughput gains due to reduced IR size,
    // which is a pretty dumb reason, considering just how craptastic the rest of the JIT code
    // is when it comes to throughput.
    // Narrowing LCL_FLD and IND nodes is also questionable, due to it potentially blocking
    // CSE as described above. That too looks like something that could be done in lowering.

    switch (tree->GetOper())
    {
        GenTree* op1;
        GenTree* op2;

#ifndef TARGET_64BIT
        case GT_CNS_LNG:
        {
            int64_t lval  = tree->AsLngCon()->GetValue();
            int64_t lmask = 0;

            switch (dstt)
            {
                case TYP_BYTE:
                    lmask = 0x0000007F;
                    break;
                case TYP_BOOL:
                case TYP_UBYTE:
                    lmask = 0x000000FF;
                    break;
                case TYP_SHORT:
                    lmask = 0x00007FFF;
                    break;
                case TYP_USHORT:
                    lmask = 0x0000FFFF;
                    break;
                case TYP_INT:
                    lmask = 0x7FFFFFFF;
                    break;
                case TYP_UINT:
                    lmask = 0xFFFFFFFF;
                    break;
                default:
                    return false;
            }

            if ((lval & lmask) != lval)
            {
                return false;
            }

            if (doit)
            {
                int32_t value = static_cast<int32_t>(lval);

                tree->ChangeToIntCon(TYP_INT, value);

                if (vnStore != nullptr)
                {
                    tree->SetVNP(ValueNumPair{vnStore->VNForIntCon(value)});
                }
            }

            return true;
        }
#endif
        case GT_CNS_INT:
        {
            ssize_t ival  = tree->AsIntCon()->GetValue();
            ssize_t imask = 0;

            switch (dstt)
            {
                case TYP_BYTE:
                    imask = 0x0000007F;
                    break;
                case TYP_BOOL:
                case TYP_UBYTE:
                    imask = 0x000000FF;
                    break;
                case TYP_SHORT:
                    imask = 0x00007FFF;
                    break;
                case TYP_USHORT:
                    imask = 0x0000FFFF;
                    break;
#ifdef TARGET_64BIT
                case TYP_INT:
                    imask = 0x7FFFFFFF;
                    break;
                case TYP_UINT:
                    imask = 0xFFFFFFFF;
                    break;
#endif
                default:
                    return false;
            }

            if ((ival & imask) != ival)
            {
                return false;
            }

#ifdef TARGET_64BIT
            if (doit)
            {
                int32_t value = static_cast<int32_t>(ival);

                tree->SetType(TYP_INT);
                tree->AsIntCon()->SetValue(value);

                if (vnStore != nullptr)
                {
                    tree->SetVNP(ValueNumPair{vnStore->VNForIntCon(value)});
                }
            }
#endif
            return true;
        }
        case GT_LCL_VAR:
        {
            // Allow implicit narrowing to INT of LONG locals.
            if (!varTypeIsInt(dstt))
            {
                return false;
            }

            assert(tree->TypeIs(TYP_LONG));

            if (doit)
            {
                tree->SetType(TYP_INT);
                tree->SetVNP(vnpNarrow);
            }

            return true;
        }
        case GT_LCL_FLD:
        case GT_IND:
        {
            if ((dstSize > varTypeSize(tree->GetType())) &&
                (varTypeIsUnsigned(dstt) && !varTypeIsUnsigned(tree->GetType())))
            {
                return false;
            }

            if (doit && (dstSize <= varTypeSize(tree->GetType())))
            {
                tree->SetType(varTypeNodeType(dstt));
                tree->SetVNP(vnpNarrow);
            }

            return true;
        }
        case GT_CAST:
        {
            if (tree->gtOverflow())
            {
                return false;
            }

            GenTree*  castSrc     = tree->AsCast()->GetOp(0);
            var_types castSrcType = castSrc->GetType();

            if (tree->AsCast()->GetCastType() != srct)
            {
                return false;
            }

            if (!varTypeIsIntegral(castSrcType))
            {
                return false;
            }

            if (varTypeSize(castSrcType) > dstSize)
            {
                return false;
            }

            if (doit)
            {
                var_types nodeType = varTypeNodeType(dstt);

                if ((varTypeSize(castSrcType) == dstSize) &&
                    ((varTypeIsUnsigned(nodeType) == varTypeIsUnsigned(castSrcType)) || !varTypeIsSmall(nodeType)))
                {
                    // Same size and there is no signedness mismatch for small types,
                    // change the CAST into a NOP

                    JITDUMP("Cast operation has no effect, replacing [%06u] CAST with NOP.\n", tree->GetID());

                    tree->ClearUnsigned();
                    tree->ChangeOper(GT_NOP);
                    tree->SetType(nodeType);
                    tree->SetVNP(castSrc->GetVNP());
                }
                else
                {
                    // oprSize is smaller or there is a signedness mismatch for small types

                    tree->AsCast()->SetCastType(nodeType);
                    tree->SetVNP(vnpNarrow);
                }
            }

            return true;
        }
        case GT_COMMA:
        {
            op2 = tree->AsOp()->GetOp(1);
            noway_assert(varActualType(tree->GetType()) == varActualType(op2->GetType()));

            if (!fgMorphNarrowTree(op2, srct, dstt, vnpNarrow, doit))
            {
                return false;
            }

            if (doit)
            {
                tree->SetType(varActualType(dstt));
                tree->SetVNP(vnpNarrow);
            }

            return true;
        }
        case GT_AND:
        {
            op1 = tree->AsOp()->GetOp(0);
            op2 = tree->AsOp()->GetOp(1);
            noway_assert(varActualType(tree->GetType()) == varActualType(op1->GetType()));
            noway_assert(varActualType(tree->GetType()) == varActualType(op2->GetType()));

            GenTree*  opToNarrow                      = nullptr;
            GenTree** otherOpUse                      = nullptr;
            bool      foundOperandThatBlocksNarrowing = false;

            // If 'dstt' is unsigned and one of the operands can be narrowed into 'dsst',
            // the result of the GT_AND will also fit into 'dstt' and can be narrowed.
            // The same is true if one of the operands is an int const and can be narrowed into 'dsst'.
            if (op2->IsIntCon() || varTypeIsUnsigned(dstt))
            {
                if (fgMorphNarrowTree(op2, srct, dstt, ValueNumPair(), false))
                {
                    opToNarrow = op2;
                    otherOpUse = &tree->AsOp()->gtOp1;
                }
                else
                {
                    foundOperandThatBlocksNarrowing = true;
                }
            }

            if ((opToNarrow == nullptr) && (op1->IsIntCon() || varTypeIsUnsigned(dstt)))
            {
                if (fgMorphNarrowTree(op1, srct, dstt, ValueNumPair(), false))
                {
                    opToNarrow = op1;
                    otherOpUse = &tree->AsOp()->gtOp2;
                }
                else
                {
                    foundOperandThatBlocksNarrowing = true;
                }
            }

            if (opToNarrow != nullptr)
            {
                if (doit)
                {
                    bool isLong = tree->TypeIs(TYP_LONG);

                    tree->SetType(TYP_INT);
                    tree->SetVNP(vnpNarrow);

                    fgMorphNarrowTree(opToNarrow, srct, dstt, ValueNumPair(), true);

                    // We may also need to cast away the upper bits of the other operand.
                    if (isLong)
                    {
                        assert(tree->TypeIs(TYP_INT));

                        GenTree* castOp = gtNewCastNode(*otherOpUse, false, TYP_INT);
                        INDEBUG(castOp->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
                        *otherOpUse = castOp;
                    }
                }

                return true;
            }

            if (foundOperandThatBlocksNarrowing)
            {
                noway_assert(!doit);

                return false;
            }
            goto COMMON_BINOP;
        }
        case GT_ADD:
        case GT_MUL:
        {
            if (tree->gtOverflow() || varTypeIsSmall(dstt))
            {
                noway_assert(!doit);

                return false;
            }

            FALLTHROUGH;
        }
        case GT_OR:
        case GT_XOR:
        {
            op1 = tree->AsOp()->GetOp(0);
            op2 = tree->AsOp()->GetOp(1);
            noway_assert(varActualType(tree->GetType()) == varActualType(op1->GetType()));
            noway_assert(varActualType(tree->GetType()) == varActualType(op2->GetType()));

        COMMON_BINOP:
            if (!fgMorphNarrowTree(op1, srct, dstt, ValueNumPair(), doit) ||
                !fgMorphNarrowTree(op2, srct, dstt, ValueNumPair(), doit))
            {
                noway_assert(!doit);

                return false;
            }

            if (doit)
            {
#ifndef TARGET_64BIT
                if (tree->OperIs(GT_MUL) && tree->TypeIs(TYP_LONG))
                {
                    assert(dstt == TYP_INT);

                    GenTreeCast* op1 = tree->AsOp()->GetOp(0)->IsCast();
                    GenTreeCast* op2 = tree->AsOp()->GetOp(1)->IsCast();

                    if (op1 != nullptr)
                    {
                        op1->ClearDoNotCSE();
                    }

                    if (op2 != nullptr)
                    {
                        op1->ClearDoNotCSE();
                    }
                }
#endif

                tree->SetType(TYP_INT);
                tree->SetVNP(vnpNarrow);
            }

            return true;
        }
        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GT:
        case GT_GE:
            return true;
        default:
            noway_assert(!doit);
            return false;
    }
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

#if FEATURE_FIXED_OUT_ARGS
static bool HasInlineThrowHelperCall(Compiler* compiler, GenTree* tree)
{
    if (compiler->fgUseThrowHelperBlocks() || ((tree->gtFlags & GTF_EXCEPT) == 0))
    {
        return false;
    }

    return compiler->fgWalkTreePre(&tree, [](GenTree** use, Compiler::fgWalkData* data) {
        GenTree* node = *use;

        if ((node->gtFlags & GTF_EXCEPT) == 0)
        {
            return Compiler::WALK_SKIP_SUBTREES;
        }

        switch (node->GetOper())
        {
            case GT_MUL:
            case GT_ADD:
            case GT_SUB:
            case GT_CAST:
                if (node->gtOverflow())
                {
                    return Compiler::WALK_ABORT;
                }
                break;

            case GT_INDEX_ADDR:
                if ((node->gtFlags & GTF_INX_RNGCHK) != 0)
                {
                    return Compiler::WALK_ABORT;
                }
                break;

            case GT_BOUNDS_CHECK:
                return Compiler::WALK_ABORT;

            default:
                break;
        }

        return Compiler::WALK_CONTINUE;
    }) == Compiler::WALK_ABORT;
}

static bool HasLclHeap(Compiler* compiler, GenTree* tree)
{
    return compiler->fgWalkTreePre(&tree, [](GenTree** use, Compiler::fgWalkData* data) {
        return (*use)->OperIs(GT_LCLHEAP) ? Compiler::WALK_ABORT : Compiler::WALK_CONTINUE;
    }) == Compiler::WALK_ABORT;
}
#endif // FEATURE_FIXED_OUT_ARGS

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
            // subsequent args that may use whatever location the assignment changes.
            //
            // Likewise, previous arguments only need temps if they're going to be moved to
            // the late arg list.
            //
            // This should probably be more precise, even at the cost of throughput. It's
            // rare for normal code to contain assignments in arg trees, it's more likely
            // that such assignments were introduced by the JIT (e.g. bound checks, null
            // checks, multi use etc.) and assign to new temps that aren't used anywhere
            // else except this particular arg tree.

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

                GenTree* node = prevArgInfo->GetNode();

                // Constants and local addresses obviously do not need temps.
                // The `this` arg also does not need a temp, if it's ever stored
                // to the importer replaces it with a normal local.

                if (!node->OperIsConst() && !node->OperIs(GT_LCL_ADDR) &&
                    !(node->OperIs(GT_LCL_VAR) &&
                      (node->AsLclVar()->GetLcl()->GetLclNum() == compiler->info.GetThisParamLclNum())))
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

        if (!treatLikeCall && (argCount > 1) && HasInlineThrowHelperCall(compiler, arg))
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
    //       are slightly different.
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
    //       late arg list. Well, that's simple - exception are incorrectly reordered on
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
                if (HasLclHeap(compiler, arg))
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
        CallArgInfo*  sortedArgTableArray[32];
        CallArgInfo** sortedArgTable = sortedArgTableArray;

        if (argCount > _countof(sortedArgTableArray))
        {
            sortedArgTable = compiler->getAllocator(CMK_CallInfo).allocate<CallArgInfo*>(argCount);
        }

        memcpy(sortedArgTable, argTable, argCount * sizeof(argTable[0]));

        SortArgs(compiler, call, sortedArgTable);
        EvalArgsToTemps(compiler, call, sortedArgTable);
    }

    argsComplete = true;
}

void CallInfo::SortArgs(Compiler* compiler, GenTreeCall* call, CallArgInfo** argTable)
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
                compiler->gtSetCosts(arg);
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

        for (unsigned i = 0; i < argCount; i++)
        {
            argTable[i]->Dump();
        }

        printf("\n");
    }
#endif
}

void CallInfo::EvalArgsToTemps(Compiler* compiler, GenTreeCall* call, CallArgInfo** argTable)
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

            LclVarDsc* tempLcl = compiler->lvaGetDesc(argInfo->GetTempLclNum());
            compiler->lvaSetAddressExposed(tempLcl);
            lateArg = compiler->gtNewLclVarAddrNode(tempLcl);
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

            LclVarDsc* tempLcl = compiler->lvaAllocTemp(true DEBUGARG("argument with side effect"));

            argInfo->SetTempLclNum(tempLcl->GetLclNum());

            if (!varTypeIsStruct(arg->GetType()))
            {
                var_types type = varActualType(arg->GetType());
                tempLcl->SetType(type);
                setupArg = compiler->gtNewStoreLclVar(tempLcl, type, arg);
            }
            else if (arg->IsCall() && (arg->AsCall()->GetRegCount() > 1))
            {
                compiler->lvaSetStruct(tempLcl, arg->AsCall()->GetRetLayout(), false);
                tempLcl->lvIsMultiRegRet = true;
                tempLcl->lvFieldAccessed = true;

                LclVarDsc* dstLcl     = tempLcl;
                var_types  dstLclType = tempLcl->GetType();

                StructPromotionHelper structPromotion(compiler);

                if (structPromotion.TryPromoteStructLocal(tempLcl))
                {
                    if (tempLcl->GetPromotedFieldCount() == 1)
                    {
                        LclVarDsc* promotedFieldLcl = compiler->lvaGetDesc(tempLcl->GetPromotedFieldLclNum(0));

                        if (varTypeIsSIMD(promotedFieldLcl->GetType()))
                        {
                            arg->SetType(promotedFieldLcl->GetType());

                            dstLcl     = promotedFieldLcl;
                            dstLclType = promotedFieldLcl->GetType();

                            tempLcl->lvIsMultiRegRet = false;
                        }
                    }
                }

                setupArg = compiler->gtNewStoreLclVar(dstLcl, dstLclType, arg);
            }
            else if (varTypeIsSIMD(arg->GetType()))
            {
                ClassLayout* layout = compiler->typGetVectorLayout(arg);
                if (layout != nullptr)
                {
                    compiler->lvaSetStruct(tempLcl, layout, /* checkUnsafeBuffer */ false);
                }
                else
                {
                    // We may not be able to recover the struct handle from HWINTRINSIC
                    // nodes and SIMD typed IND nodes.

                    // TODO-MIKE-Cleanup: So why bother at all?

                    assert(arg->OperIsHWIntrinsic() || arg->OperIs(GT_IND));
                    tempLcl->lvType = arg->GetType();
                }

                setupArg = compiler->gtNewAssignNode(compiler->gtNewLclvNode(tempLcl, arg->GetType()), arg);
                setupArg = compiler->fgMorphStructAssignment(setupArg->AsOp());
            }
#ifndef TARGET_X86
            else if (arg->OperIs(GT_MKREFANY))
            {
#ifdef WINDOWS_AMD64_ABI
                unreached();
#else
                compiler->lvaSetStruct(tempLcl, compiler->typGetObjLayout(compiler->impGetRefAnyClass()), false);
                setupArg = compiler->abiMorphMkRefAnyToStore(tempLcl, arg->AsOp());
#endif
            }
#endif
            else
            {
                ClassLayout* layout = compiler->typGetStructLayout(arg);
                compiler->lvaSetStruct(tempLcl, layout, /* checkUnsafeBuffer */ false);
                setupArg = compiler->gtNewAssignNode(compiler->gtNewLclvNode(tempLcl, TYP_STRUCT), arg);
                setupArg = compiler->fgMorphStructAssignment(setupArg->AsOp());
            }

            lateArg = compiler->gtNewLclvNode(tempLcl, varActualType(tempLcl->GetType()));
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
                argInfo->use->SetNode(setupArg);
                JITDUMP("\n");
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

    // TODO-MIKE-Review: We could clone LCL_ADDR too but it looks like
    // fgMakeMultiUse has very few uses and none them could reasonably
    // have LCL_ADDRs involved.

    if (tree->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        return gtClone(tree);
    }
    else
    {
        return fgInsertCommaFormTemp(pOp);
    }
}

GenTreeLclVar* Compiler::fgInsertCommaFormTemp(GenTree** use)
{
    GenTree* tree = *use;
    assert(!varTypeIsStruct(tree->GetType()));

    var_types  type  = varActualType(tree->GetType());
    LclVarDsc* lcl   = lvaNewTemp(type, true DEBUGARG("fgInsertCommaFormTemp temp"));
    GenTree*   store = gtNewStoreLclVar(lcl, type, tree);
    GenTree*   load  = gtNewLclvNode(lcl, type);
    *use             = gtNewCommaNode(store, load, type);
    return gtNewLclvNode(lcl, type);
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
        NonStandardArgs(CompAllocator alloc) : args(alloc)
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
            for (unsigned i = 0; i < args.Size(); i++)
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

        // TODO-MIKE-Review: This is probably one of those cases where allowing LCL_FLD
        // is bad for CQ, especially on ARM where we don't have memory operands.
        if (arg->OperIs(GT_LCL_VAR, GT_LCL_FLD))
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

        GenTree* newArg =
            gtNewOperNode(GT_ADD, TYP_BYREF, arg,
                          gtNewIconNode(static_cast<ssize_t>(eeGetEEInfo()->offsetOfWrapperDelegateIndirectCell)));

        // Append newArg as the last arg
        GenTreeCall::Use** insertionPoint = &call->gtCallArgs;
        for (; *insertionPoint != nullptr; insertionPoint = &((*insertionPoint)->NextRef()))
        {
        }
        *insertionPoint = gtNewCallArgs(newArg);

        numArgs++;
        nonStandardArgs.Add(newArg, info.virtualStubParamRegNum);
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

        args                         = args->GetNext();
        GenTree* numNewStackSlotsArg = args->GetNode();
        assert(numNewStackSlotsArg != nullptr);
        nonStandardArgs.Add(numNewStackSlotsArg, REG_LNGARG_HI);
    }
#else // !TARGET_X86
    // TODO-X86-CQ: Currently RyuJIT/x86 passes args on the stack, so this is not needed.
    // If/when we change that, the following code needs to be changed to correctly support the (TBD) managed calling
    // convention for x86/SSE.

    // If we have a Fixed Return Buffer argument register then we setup a non-standard argument for it.
    //
    // We don't use the fixed return buffer argument if we have the special unmanaged instance call convention.
    // That convention doesn't use the fixed return buffer register.
    //
    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef TARGET_ARM64
    if (call->HasFixedRetBufArg())
    {
        GenTreeCall::Use* args = call->gtCallArgs;
        assert(args != nullptr);

        // We don't increment numArgs here, since we already counted this argument above.

        nonStandardArgs.Add(args->GetNode(), REG_ARG_RET_BUFF);
    }
#endif

    // We are allowed to have a Fixed Return Buffer argument combined
    // with any of the remaining non-standard arguments
    //
    CLANG_FORMAT_COMMENT_ANCHOR;

    if (call->IsVirtualStub())
    {
#ifdef TARGET_X86
        if (call->IsTailCallViaJitHelper())
        {
            // If it is a VSD call getting dispatched via tail call helper,
            // fgMorphTailCallViaJitHelper() would materialize stub addr as an additional
            // parameter added to the original arg list and hence no need to
            // add as a non-standard arg.
        }
        else
#endif
        {
            GenTree* stubAddrArg = fgGetStubAddrArg(call);
            // And push the stub address onto the list of arguments
            call->gtCallArgs = gtPrependNewCallArg(stubAddrArg, call->gtCallArgs);

            numArgs++;
            nonStandardArgs.Add(stubAddrArg, info.virtualStubParamRegNum);
        }
    }
    else
#endif // !TARGET_X86
    if (call->IsIndirectCall() && (call->gtCallCookie != nullptr))
    {
        assert(!call->IsUnmanaged());

#ifdef TARGET_X86
        // x86 passes the cookie on the stack as the final argument to the call.
        GenTreeCall::Use** insertionPoint = &call->gtCallArgs;
        for (; *insertionPoint != nullptr; insertionPoint = &((*insertionPoint)->NextRef()))
        {
        }

        *insertionPoint = gtNewCallArgs(call->gtCallCookie);
#else
        // All other architectures pass the cookie in a register.
        call->gtCallArgs = gtPrependNewCallArg(call->gtCallCookie, call->gtCallArgs);
#endif
        nonStandardArgs.Add(call->gtCallCookie, REG_PINVOKE_COOKIE_PARAM);
        numArgs++;
        call->gtCallCookie = nullptr;

        GenTree* target  = gtClone(call->gtCallAddr, true);
        call->gtCallArgs = gtPrependNewCallArg(target, call->gtCallArgs);
        nonStandardArgs.Add(target, REG_PINVOKE_TARGET_PARAM);
        numArgs++;

        call->gtCallType    = CT_HELPER;
        call->gtCallMethHnd = eeFindHelper(CORINFO_HELP_PINVOKE_CALLI);
    }

#if defined(FEATURE_READYTORUN_COMPILER) && defined(TARGET_ARMARCH)
    // For arm, we dispatch code same as VSD using info.virtualStubParamRegNum
    // for indirection cell address, which ZapIndirectHelperThunk expects.
    if (call->IsR2RRelativeIndir())
    {
        assert(call->gtEntryPoint.addr != nullptr);

        size_t   addrValue           = (size_t)call->gtEntryPoint.addr;
        GenTree* indirectCellAddress = gtNewIconHandleNode(addrValue, HandleKind::MethodAddr);
        indirectCellAddress->AsIntCon()->SetDumpHandle(call->GetMethodHandle());

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
        nonStandardArgs.Add(indirectCellAddress, REG_R2R_INDIRECT_PARAM);
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

#ifndef UNIX_X86_ABI
    if (call->CallerPop())
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
#endif // UNIX_X86_ABI

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
        assert(!argx->OperIs(GT_ARGPLACE));

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
            size         = 1;
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
                   ((sigType == TYP_I_IMPL) && argx->TypeIs(TYP_BYREF)) ||
                   ((sigType == TYP_BYREF) && argx->TypeIs(TYP_REF)));

#ifdef TARGET_ARM
            argAlign =
                roundUp(static_cast<unsigned>(genTypeAlignments[argx->GetType()]), REGSIZE_BYTES) / REGSIZE_BYTES;
#endif

#ifdef TARGET_64BIT
            // On 64 bit targets all primitive types are passed in a single reg/slot.
            size = 1;
#else
            // On 32 bit targets LONG and DOUBLE are passed in 2 regs/slots.
            size = argx->TypeIs(TYP_LONG, TYP_DOUBLE) ? 2 : 1;
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
                    fltArgSkippedRegMask |= genMapFloatRegArgNumToRegMask(fltArgRegNum);
                    fltArgRegNum++;
                }
            }
            else if (intArgRegNum < MAX_REG_ARG)
            {
                if (intArgRegNum % 2 == 1)
                {
                    argSkippedRegMask |= genMapIntRegArgNumToRegMask(intArgRegNum);
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

#ifdef OSX_ARM64_ABI
        unsigned argAlignBytes = lvaGetParamAlignment(sigType, hfaType == TYP_FLOAT);
#endif

#ifdef TARGET_X86
        if (!isStructArg ? varTypeIsI(varActualType(argx->GetType())) : isTrivialPointerSizedStruct(layout))
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
                    // Remove the back-filled register(s) from the skipped mask
                    fltArgSkippedRegMask &= ~backFillBitMask;
                    nextFltArgRegNum = genRegNumFromMask(backFillBitMask) - REG_F0;
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
#ifdef TARGET_X86
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
#endif

        // Now we know if the argument goes in registers or not and how big it is.
        CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef TARGET_ARM
        // If we ever allocate a floating point argument to the stack, then all
        // subsequent HFA/float/double arguments go on the stack.
        if (!isRegArg && passUsingFloatRegs)
        {
            for (; fltArgRegNum < MAX_FLOAT_REG_ARG; ++fltArgRegNum)
            {
                fltArgSkippedRegMask |= genMapFloatRegArgNumToRegMask(fltArgRegNum);
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
                argSkippedRegMask |= genMapIntRegArgNumToRegMask(intArgRegNum);
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
            else if (argx->TypeIs(TYP_DOUBLE) && opts.UseHfa())
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
            else if (varTypeIsFloating(argx->GetType()) && opts.UseHfa())
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
void Compiler::fgMorphArgs(GenTreeCall* const call)
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
            assert(arg->OperIs(GT_ARGPLACE, GT_LCL_DEF, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD) ||
                   (arg->OperIs(GT_COMMA) && arg->TypeIs(TYP_VOID)));

            argsSideEffects |= arg->gtFlags;
            continue;
        }

        bool paramIsStruct = typIsLayoutNum(argUse->GetSigTypeNum());

        if (!varTypeIsStruct(arg->GetType()) && (!arg->IsIntegralConst(0) || !paramIsStruct))
        {
            if (paramIsStruct && arg->IsCast() && !arg->gtOverflow() && varTypeIsSmall(arg->GetType()) &&
                (varTypeSize(arg->GetType()) == typGetLayoutByNum(argUse->GetSigTypeNum())->GetSize()))
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

        if (argVal->OperIs(GT_FIELD_LIST, GT_ARGPLACE, GT_LCL_DEF, GT_STORE_LCL_VAR))
        {
            // Skip arguments that have already been transformed.
            argsSideEffects |= arg->gtFlags;
            continue;
        }

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
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

    if (call->IsIndirectCall())
    {
        call->gtCallAddr = fgMorphTree(call->gtCallAddr);
        argsSideEffects |= call->gtCallAddr->gtFlags;
    }

    // Clear the ASG and EXCEPT (if possible) flags on the call node
    call->gtFlags &= ~GTF_ASG;

    if (!call->CallMayThrow(this))
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
}

bool Compiler::abiMorphStackStructArg(CallArgInfo* argInfo, GenTree* arg)
{
    assert(argInfo->GetRegCount() == 0);

    if (arg->IsIntegralConst(0))
    {
#ifdef TARGET_64BIT
        // Args that require more than one slot are handled in codegen but in the single slot
        // case we need to ensure that the entire slot is zeroed, not just the low 4 bytes.
        if ((argInfo->GetSlotCount() == 1) &&
            (typGetLayoutByNum(argInfo->GetSigTypeNum())->GetSize() > varTypeSize(arg->GetType())))
        {
            arg->SetType(TYP_LONG);
        }
#endif

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
        arg->AsLclVar()->GetLcl()->IsIndependentPromoted())
    {
        LclVarDsc* lcl = arg->AsLclVar()->GetLcl();

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

        arg->AsLclVar()->SetLcl(lvaGetDesc(lcl->GetPromotedFieldLclNum(0)));
        arg->SetType(fieldType);
        arg->gtFlags = GTF_EMPTY;

        argInfo->SetArgType(fieldType);

        return false;
    }

#ifndef TARGET_X86
    // On x86 we need to keep multireg calls unchanged since there are no multireg
    // args and thus no second arg morphing pass. This also avoids the need for a
    // temp and it's good for CQ, even if that's not exactly useful on x86, given
    // the limited use of the native calling convention.
    // TODO-MIKE-CQ: Avoiding the temp would be good on other targets as well, but
    // currently that doesn't work because the rest of the arg morphing code always
    // spills arg containing calls to temps, even the first evaluated arg, that has
    // no outgoing arg area interference.
    if (arg->IsCall() && (arg->AsCall()->GetRegCount() > 1))
    {
        return true;
    }
#endif

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
            canRetype =
                arg->AsLclFld()->GetLclOffs() + varTypeSize(argType) <= arg->AsLclFld()->GetLcl()->GetTypeSize();

            if (canRetype)
            {
                arg->AsLclFld()->SetFieldSeq(FieldSeqStore::NotAField());
            }
        }
        else if (arg->OperIs(GT_LCL_VAR))
        {
            canRetype = true;
            lvaSetDoNotEnregister(arg->AsLclVar()->GetLcl() DEBUGARG(DNER_LocalField));
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

    LclVarDsc* lcl = arg->GetLcl();

    if (!lcl->IsIndependentPromoted())
    {
        return;
    }

    assert(lcl->GetPromotedFieldCount() > 1);

    GenTreeFieldList* fieldList = abiMakeFieldList(arg);

    for (LclVarDsc* fieldLcl : PromotedFields(lcl))
    {
        var_types      fieldType   = fieldLcl->GetType();
        unsigned       fieldOffset = fieldLcl->GetPromotedFieldOffset();
        GenTreeLclVar* fieldLclVar = gtNewLclvNode(fieldLcl, fieldType);

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
                    call->AddSideEffects(newArg->GetSideEffects());
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

        LclVarDsc* varDsc = arg->AsLclVar()->GetLcl();

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

        lvaSetDoNotEnregister(arg->AsLclFld()->GetLcl() DEBUGARG(DNER_LocalField));
    }
}

GenTree* Compiler::abiMorphSingleRegLclArgPromoted(GenTreeLclVar* arg, var_types argRegType, unsigned argSize)
{
    assert(argSize <= varTypeSize(argRegType));
    assert(varTypeIsSingleReg(argRegType));

    LclVarDsc* lcl = arg->GetLcl();
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
            arg->SetLcl(fieldLcl);
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
            arg->SetLcl(fieldLcl);
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
            GenTreeLclVar* field0LclNode = gtNewLclvNode(field0Lcl, TYP_FLOAT);
            GenTreeLclVar* field1LclNode = gtNewLclvNode(field1Lcl, TYP_FLOAT);

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
            LclVarDsc* fieldLcl    = lvaGetDesc(lcl->GetPromotedFieldLclNum(i));
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
            var_types newArgType      = TYP_INT;
#endif

            GenTree* field = gtNewLclvNode(fieldLcl, fieldLcl->GetType());

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

#if LOCAL_ASSERTION_PROP
                    if ((morphAssertionCount == 0) || !morphAssertionIsTypeRange(field->AsLclVar(), type))
#endif
                    {
                        field->SetType(TYP_INT);
                        field = gtNewCastNode(field, false, type);
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
                field = gtNewCastNode(field, true, TYP_LONG);
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
                if (varActualType(newArg->GetType()) != newArgType)
                {
                    newArg = gtNewCastNode(newArg, true, TYP_LONG);
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
        lvaSetDoNotEnregister(arg->GetLcl() DEBUGARG(DNER_LocalField));
        return arg;
    }

    return newArg;
}

#ifndef TARGET_X86

GenTree* Compiler::abiMorphMkRefAnyToStore(LclVarDsc* tempLcl, GenTreeOp* mkrefany)
{
    GenTreeLclFld* storePtrField =
        gtNewStoreLclFld(TYP_BYREF, tempLcl, OFFSETOF__CORINFO_TypedReference__dataPtr, mkrefany->GetOp(0));
    storePtrField->SetFieldSeq(GetRefanyValueField());
    GenTreeLclFld* storeTypeField =
        gtNewStoreLclFld(TYP_I_IMPL, tempLcl, OFFSETOF__CORINFO_TypedReference__type, mkrefany->GetOp(1));
    storeTypeField->SetFieldSeq(GetRefanyTypeField());

#ifdef WINDOWS_AMD64_ABI
    assert(tempLcl->lvIsImplicitByRefArgTemp);
    storePtrField->AddSideEffects(GTF_GLOB_REF);
    storeTypeField->AddSideEffects(GTF_GLOB_REF);
#endif

    return gtNewCommaNode(storePtrField, storeTypeField);
}

#endif

#if FEATURE_MULTIREG_ARGS || FEATURE_MULTIREG_RET

GenTree* Compiler::abiMorphMultiRegHfaLclArgPromoted(CallArgInfo* argInfo, GenTreeLclVar* arg)
{
    assert(argInfo->IsHfaArg());
    assert(varTypeUsesFloatReg(argInfo->GetRegType(0)));
    assert(argInfo->GetSlotCount() == 0);

    LclVarDsc* lcl       = arg->GetLcl();
    var_types  regType   = argInfo->GetRegType(0);
    unsigned   regCount  = argInfo->GetRegCount();
    unsigned   regSize   = varTypeSize(regType);
    unsigned   regOffset = 0;

    for (LclVarDsc* fieldLcl : PromotedFields(lcl))
    {
        var_types fieldType = fieldLcl->GetType();

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

        LclVarDsc* fieldLcl    = lvaGetDesc(lcl->GetPromotedFieldLclNum(field));
        unsigned   fieldOffset = fieldLcl->GetPromotedFieldOffset();
        var_types  fieldType   = fieldLcl->GetType();

        GenTree* fieldNode = gtNewLclvNode(fieldLcl, fieldType);

        if (fieldLcl->IsAddressExposed())
        {
            fieldNode->AddSideEffects(GTF_GLOB_REF);
        }

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

        GenTree*  fieldValue  = gtNewLclvNode(lvaGetDesc(map.fields[i].lclNum), map.fields[i].type);
        var_types fieldType   = map.fields[i].type;
        unsigned  fieldOffset = map.fields[i].offset;

        if (lvaGetDesc(map.fields[i].lclNum)->IsAddressExposed())
        {
            fieldValue->AddSideEffects(GTF_GLOB_REF);
        }

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
                fieldValue = gtNewLclvNode(fieldValue->AsLclVar()->GetLcl(), fieldType);
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
        if (arg->OperIs(GT_LCL_VAR) && arg->AsLclVar()->GetLcl()->IsPromoted())
        {
            if (argInfo->IsHfaArg())
            {
                return abiMorphMultiRegHfaLclArgPromoted(argInfo, arg->AsLclVar());
            }

            AbiRegFieldMap map(this, arg->AsLclVar()->GetLcl(), argInfo);

            if (map.IsSupported(argInfo))
            {
                return abiMorphMultiRegLclArgPromoted(argInfo, map);
            }
        }

        return abiMorphMultiRegLclArg(argInfo, arg->AsLclVarCommon());
    }

    if (arg->OperIs(GT_OBJ))
    {
        return abiMorphMultiRegObjArg(argInfo, arg->AsObj());
    }

    if (arg->OperIs(GT_CALL) && arg->TypeIs(TYP_STRUCT))
    {
        return abiMorphMultiRegCallArg(argInfo, arg->AsCall());
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

    LclVarDsc* tempLcl    = nullptr;
    GenTree*   tempAssign = nullptr;

    if (argIsBroadcast)
    {
        arg = arg->AsHWIntrinsic()->GetOp(0);

        assert(arg->TypeIs(TYP_FLOAT));

        if (!arg->IsDblCon() && !arg->OperIs(GT_LCL_VAR))
        {
            tempLcl    = lvaNewTemp(TYP_FLOAT, true DEBUGARG("multi-reg SIMD arg temp"));
            tempAssign = gtNewStoreLclVar(tempLcl, TYP_FLOAT, arg);

            arg = gtNewLclvNode(tempLcl, TYP_FLOAT);
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

        LclVarDsc* dblTempLcl    = lvaNewTemp(TYP_DOUBLE, true DEBUGARG("multi-reg SIMD arg temp"));
        GenTree*   dblTempAssign = gtNewStoreLclVar(dblTempLcl, TYP_DOUBLE, arg);

        if (tempAssign != nullptr)
        {
            tempAssign = gtNewCommaNode(tempAssign, dblTempAssign);
        }
        else
        {
            tempAssign = dblTempAssign;
        }

        arg = gtNewLclvNode(dblTempLcl, TYP_DOUBLE);
#endif // UNIX_AMD64_ABI
    }
    else if (!argIsZero && !argIsCreate)
    {
        ClassLayout* argLayout = typGetLayoutByNum(argInfo->GetSigTypeNum());

        tempLcl    = lvaNewTemp(argLayout, true DEBUGARG("multi-reg SIMD arg temp"));
        tempAssign = gtNewStoreLclVar(tempLcl, arg->GetType(), arg);
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
            regValue = gtNewLclvNode(tempLcl, arg->GetType());
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
    LclVarDsc*   lcl       = arg->GetLcl();
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
            lvaSetDoNotEnregister(lcl DEBUGARG(DNER_IsStructArg));
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

    if (arg->OperIs(GT_LCL_VAR) && lcl->IsIndependentPromoted())
    {
        lcl = abiAllocateStructArgTemp(argLayout);

        tempAssign = gtNewAssignNode(gtNewLclvNode(lcl, lcl->GetType()), arg);
        tempAssign = fgMorphStructAssignment(tempAssign->AsOp());

        arg->SetLcl(lcl);
    }
#endif // TARGET_ARM

    unsigned regCount = argInfo->GetRegCount();
#if FEATURE_ARG_SPLIT
    regCount += argInfo->GetSlotCount();
#endif

    GenTreeFieldList* fieldList = new (this, GT_FIELD_LIST) GenTreeFieldList();

    unsigned lclOffset = arg->OperIs(GT_LCL_FLD) ? arg->AsLclFld()->GetLclOffs() : 0;
    unsigned lclSize   = lcl->GetTypeSize();
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
            GenTree* simdValue    = gtNewLclvNode(lcl, lcl->GetType());

            if (lcl->IsAddressExposed())
            {
                simdValue->AddSideEffects(GTF_GLOB_REF);
            }

            regValue = gtNewSimdGetElementNode(lcl->GetType(), regType, simdValue, elementIndex);
        }
        else
#endif
        {
            lvaSetDoNotEnregister(lcl DEBUG_ARG(DNER_LocalField));

            regValue = gtNewLclFldNode(lcl, regType, lclOffset);

            if (lcl->IsAddressExposed())
            {
                regValue->AddSideEffects(GTF_GLOB_REF);
            }
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
                regAddr = gtNewOperNode(GT_ADD, varTypeAddrAdd(regAddr->GetType()), regAddr,
                                        gtNewIconNode(addrOffset + regOffset, TYP_I_IMPL));
                regAddr->gtFlags |= GTF_DONT_CSE;
            }

            regIndir = gtNewIndir(regType, regAddr);
            regIndir->gtFlags |= GTF_GLOB_REF | gtGetIndirExceptionFlags(regAddr);
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
        gtSetCosts(addr);
        addrTempRequired = addr->GetCostEx() > 4 * IND_COST_EX;
    }

    GenTree* addrAsg = nullptr;

    if (addrTempRequired)
    {
        LclVarDsc* addrLcl = lvaNewTemp(addr->GetType(), true DEBUGARG("call arg addr temp"));

        addrAsg = gtNewStoreLclVar(addrLcl, addr->GetType(), addr);
        addr    = gtNewLclvNode(addrLcl, addr->GetType());
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
            addr = gtNewOperNode(GT_ADD, varTypeAddrAdd(addr->GetType()), addr, gtNewIconNode(offset, TYP_I_IMPL));
            addr->gtFlags |= GTF_DONT_CSE;
        }
        GenTreeIndir* indir = gtNewIndir(type, addr);
        indir->gtFlags |= GTF_GLOB_REF | gtGetIndirExceptionFlags(addr);
        return indir;
    };
    auto LeftShift = [&](GenTree* op1, unsigned amount) {
        return gtNewOperNode(GT_LSH, varActualType(op1->GetType()), op1, gtNewIconNode(amount));
    };
    auto Or = [&](GenTree* op1, GenTree* op2) { return gtNewOperNode(GT_OR, varActualType(op1->GetType()), op1, op2); };
    auto Clone  = [&](GenTree* expr) { return gtCloneExpr(expr); };
    auto Extend = [&](GenTree* value) { return gtNewCastNode(value, true, TYP_LONG); };

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

GenTree* Compiler::abiMorphMultiRegCallArg(CallArgInfo* argInfo, GenTreeCall* arg)
{
    LclVarDsc*        lcl = lvaNewTemp(arg->GetRetLayout(), true DEBUGARG("multireg call arg temp"));
    GenTreeLclVar*    src = gtNewLclvNode(lcl, lcl->GetType());
    GenTreeFieldList* fieldList;

    StructPromotionHelper structPromotion(this);
    lcl->lvIsMultiRegRet = true;
    lcl->lvFieldAccessed = true;

    LclVarDsc* storeLcl  = nullptr;
    var_types  storeType = TYP_UNDEF;

    if (!structPromotion.TryPromoteStructLocal(lcl))
    {
        fieldList = abiMorphMultiRegLclArg(argInfo, src)->AsFieldList();

        storeLcl  = lcl;
        storeType = lcl->GetType();
    }
    else
    {
        if (argInfo->IsHfaArg())
        {
            fieldList = abiMorphMultiRegHfaLclArgPromoted(argInfo, src)->AsFieldList();
        }
        else
        {
            AbiRegFieldMap regMap(this, lcl, argInfo);

            if (regMap.IsSupported(argInfo))
            {
                fieldList = abiMorphMultiRegLclArgPromoted(argInfo, regMap)->AsFieldList();
            }
            else
            {
                fieldList = abiMorphMultiRegLclArg(argInfo, src)->AsFieldList();
            }
        }

        if (lcl->IsIndependentPromoted() && (lcl->GetPromotedFieldCount() == 1))
        {
            LclVarDsc* promotedFieldLcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(0));

            if (varTypeIsSIMD(promotedFieldLcl->GetType()))
            {
                arg->SetType(promotedFieldLcl->GetType());

                lcl->lvIsMultiRegRet = false;

                storeLcl  = promotedFieldLcl;
                storeType = promotedFieldLcl->GetType();
            }
        }

        if (storeLcl == nullptr)
        {
            lcl->lvIsMultiRegRet = lcl->IsIndependentPromoted();

            storeLcl  = lcl;
            storeType = lcl->GetType();
        }
    }

    GenTree* store = gtNewStoreLclVar(storeLcl, storeType, arg);

    GenTreeFieldList::Use* firstUse = fieldList->Uses().GetHead();
    firstUse->SetNode(gtNewCommaNode(store, firstUse->GetNode()));
    fieldList->AddSideEffects(store->GetSideEffects());

    return fieldList;
}

#endif // FEATURE_MULTIREG_ARGS || FEATURE_MULTIREG_RET

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64) || defined(TARGET_ARM)

LclVarDsc* Compiler::abiAllocateStructArgTemp(ClassLayout* argLayout)
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
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
        LclVarDsc* lcl = lvaAllocTemp(true DEBUGARG("implicit-by-ref arg temp"));
#else
        LclVarDsc* lcl = lvaAllocTemp(true DEBUGARG("struct arg temp"));
#endif

        lvaSetStruct(lcl, argLayout, false);
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
        lvaSetAddressExposed(lcl);
        lcl->lvIsImplicitByRefArgTemp = true;
#endif

        tempLclNum = lcl->GetLclNum();

        if (m_abiStructArgTemps != nullptr)
        {
            m_abiStructArgTemps->setBit(tempLclNum);
        }
    }

    if (m_abiStructArgTempsInUse != nullptr)
    {
        m_abiStructArgTempsInUse->setBit(tempLclNum);
    }

    return lvaGetDesc(tempLclNum);
}

void Compiler::abiFreeAllStructArgTemps()
{
    if (m_abiStructArgTempsInUse != nullptr)
    {
        m_abiStructArgTempsInUse->ZeroAll();
    }
}

#endif // defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64) || defined(TARGET_ARM)

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)

void Compiler::abiMorphImplicitByRefStructArg(GenTreeCall* call, CallArgInfo* argInfo)
{
    GenTree* arg = argInfo->GetNode();

    if (arg->TypeIs(TYP_BYREF, TYP_I_IMPL))
    {
        return;
    }

    if (arg->OperIs(GT_MKREFANY))
    {
#if defined(WINDOWS_AMD64_ABI)
        LclVarDsc* tempLcl = abiAllocateStructArgTemp(typGetObjLayout(impGetRefAnyClass()));
        argInfo->SetNode(abiMorphMkRefAnyToStore(tempLcl, arg->AsOp()));
        argInfo->SetTempLclNum(tempLcl->GetLclNum());

        return;
#else
        unreached();
#endif
    }

    // If we're optimizing, see if we can avoid making a copy.
    // We don't need a copy if this is the last use of an implicit by-ref local.
    if (opts.OptimizationEnabled())
    {
        GenTreeLclVar* const lclNode = arg->IsImplicitByrefIndir(this);

        if (lclNode != nullptr)
        {
            LclVarDsc* const lcl              = lclNode->GetLcl();
            const unsigned   totalAppearances = lcl->GetImplicitByRefParamAnyRefCount();

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

                JITDUMP("did not need to make outgoing copy for last use of implicit byref V%02u\n", lcl->GetLclNum());
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

    LclVarDsc* tempLcl = abiAllocateStructArgTemp(argLayout);

    // Replace the argument with an assignment to the temp, EvalArgsToTemps will later add
    // a use of the temp to the late arg list.

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

    GenTree* dest;

    if (tempLcl->TypeIs(TYP_STRUCT) && varTypeIsSIMD(arg->GetType()))
    {
        dest = gtNewLclFldNode(tempLcl, arg->GetType(), 0);
    }
    else
    {
        dest = gtNewLclvNode(tempLcl, tempLcl->GetType());
    }

    dest->gtFlags |= GTF_GLOB_REF;

    GenTree* asg = gtNewAssignNode(dest, arg);

    if (varTypeIsStruct(dest->GetType()))
    {
        asg = fgMorphStructAssignment(asg->AsOp());
    }

    argInfo->SetNode(asg);
    argInfo->SetTempLclNum(tempLcl->GetLclNum());
}

#endif // defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)

// A little helper used to rearrange nested commutative operations. The
// effect is that nested associative, commutative operations are transformed
// into a 'left-deep' tree, i.e. into something like this:
//
//     (((a op b) op c) op d) op...
//
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

        noway_assert(oper == GT_ADD || oper == GT_XOR || oper == GT_OR || oper == GT_AND || oper == GT_MUL);
        noway_assert(oper == op2->gtOper);

        // Commutativity doesn't hold if overflow checks are needed

        if (tree->gtOverflowEx() || op2->gtOverflowEx())
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
                        GTF_ALL_EFFECT | GTF_UNSIGNED)) == 0);

        new_op1->gtFlags = (new_op1->gtFlags & GTF_DONT_CSE) | op1->GetSideEffects() | ad1->GetSideEffects();

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
            if ((op1->GetLiberalVN() == NoVN) || (ad2->GetLiberalVN() == NoVN) ||
                (ad2->GetLiberalVN() != op1->GetLiberalVN()))
            {
                new_op1->SetVNP(ValueNumPair{vnStore->VNForExpr(nullptr, new_op1->GetType())});
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

GenTree* Compiler::fgMorphStringIndexIndir(GenTreeIndexAddr* index, GenTreeStrCon* str)
{
    assert(index->GetArray() == str);

    if (GenTreeIntCon* intCon = index->GetIndex()->IsIntConFitsInInt32())
    {
        const int32_t cnsIndex = intCon->GetInt32Value();

        if (cnsIndex >= 0)
        {
            int             length;
            const char16_t* chars =
                info.compCompHnd->getStringLiteral(str->GetModuleHandle(), str->GetToken(), &length);

            if ((cnsIndex < length) && (str != nullptr))
            {
                GenTree* cnsCharNode = gtNewIconNode(chars[cnsIndex], TYP_INT);
                INDEBUG(cnsCharNode->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
                return cnsCharNode;
            }
        }
    }

    return nullptr;
}

GenTree* Compiler::fgMorphIndexAddr(GenTreeIndexAddr* tree)
{
    // In minopts, we don't expand INDEX_ADDR in order to minimize the size of the IR. As minopts compilation
    // time is roughly proportional to the size of the IR, this helps keep compilation times down.
    // Furthermore, this representation typically saves on code size in minopts w.r.t. the complete expansion
    // performed when optimizing, as it does not require LclVar nodes (which are always stack loads/stores in
    // minopts).

    assert(!opts.MinOpts());

    // When we are optimizing, we fully expand INDEX_ADDR to something like:
    //
    //   COMMA(ARR_BOUNDS_CHK(index, ARR_LENGTH(array)), IND(ADD(array, ADD(MUL(index, elemSize), dataOffs))))
    //
    // This expansion explicitly exposes the bounds check and the address calculation to the optimizer, which allows
    // for more straightforward bounds-check removal, CSE, etc.

    GenTreeLclVar*    arrayTmpStore = nullptr;
    GenTreeLclVar*    indexTmpStore = nullptr;
    GenTreeBoundsChk* boundsCheck   = nullptr;

    GenTree* array       = tree->GetArray();
    GenTree* index       = tree->GetIndex();
    uint8_t  lenOffs     = tree->GetLenOffs();
    uint8_t  dataOffs    = tree->GetDataOffs();
    unsigned elemSize    = tree->GetElemSize();
    unsigned elemTypeNum = tree->GetElemTypeNum();

    if ((tree->gtFlags & GTF_INX_RNGCHK) != 0)
    {
        // The array and index will have multiple uses so we need to assign them to temps, unless they're
        // simple, side effect free expressions.

        constexpr int MAX_ARR_COMPLEXITY   = 4;
        constexpr int MAX_INDEX_COMPLEXITY = 4;

        GenTree* array2 = nullptr;

        if (((array->gtFlags & (GTF_ASG | GTF_CALL | GTF_GLOB_REF)) != 0) || array->OperIs(GT_LCL_FLD) ||
            gtComplexityExceeds(array, MAX_ARR_COMPLEXITY))
        {
            LclVarDsc* arrayTmpLcl = lvaNewTemp(array->GetType(), true DEBUGARG("arr expr"));

            arrayTmpStore = gtNewStoreLclVar(arrayTmpLcl, array->GetType(), array);

            array  = gtNewLclvNode(arrayTmpLcl, array->GetType());
            array2 = gtNewLclvNode(arrayTmpLcl, array->GetType());
        }
        else
        {
            array2 = gtCloneExpr(array);
            noway_assert(array2 != nullptr);
        }

        GenTree* arrLen = gtNewArrLen(array2, lenOffs);

#ifdef TARGET_64BIT
        // The CLI Spec allows an array to be indexed by either an int32 or a native int.  In the case
        // of a 64 bit architecture this means the array index can potentially be a TYP_LONG, so for this case,
        // the comparison will have to be widen to 64 bits.
        if (index->TypeIs(TYP_LONG))
        {
            arrLen = gtNewCastNode(arrLen, false, TYP_LONG);
        }
#endif

        GenTree* index2 = nullptr;

        if (((index->gtFlags & (GTF_ASG | GTF_CALL | GTF_GLOB_REF)) != 0) || index->OperIs(GT_LCL_FLD) ||
            gtComplexityExceeds(index, MAX_INDEX_COMPLEXITY))
        {
            var_types  indexTmpType = varActualType(index->GetType());
            LclVarDsc* indexTmpLcl  = lvaNewTemp(indexTmpType, true DEBUGARG("index expr"));

            indexTmpStore = gtNewStoreLclVar(indexTmpLcl, indexTmpType, index);

            index  = gtNewLclvNode(indexTmpLcl, indexTmpType);
            index2 = gtNewLclvNode(indexTmpLcl, indexTmpType);
        }
        else
        {
            index2 = gtCloneExpr(index);
            noway_assert(index2 != nullptr);
        }

        boundsCheck = gtNewBoundsChk(index2, arrLen, ThrowHelperKind::IndexOutOfRange);
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
            offset = gtNewCastNode(offset, false, TYP_LONG);
        }
    }
#endif

    if (elemSize > 1)
    {
        offset = gtNewOperNode(GT_MUL, TYP_I_IMPL, offset, gtNewIconNode(elemSize, TYP_I_IMPL));
    }

    // The element address is ADD(array, ADD(MUL(index, elemSize), dataOffs)). Compared to other possible
    // associations this has the advantage that the offset computation depends only on the element size
    // so it can be CSEd on its own (e.g. floatArray[i] and intArray[i] have the same offset expression
    // even if the array and the element type are different). This also minimizes the number of byrefs
    // since only the final ADD produces one.
    // It does slightly complicate array element address pattern matching as done in vnIsArrayElemAddr
    // because we need to check the inner ADD to find the data offset constant. That would be simpler
    // with ADD(ADD(array, MUL(index, elemSize)), dataOffs) but then the entire expression depends on
    // "array" so CSEing is more limited.

    FieldSeqNode* arrayElement = GetFieldSeqStore()->GetArrayElement(elemTypeNum, dataOffs);

    offset = gtNewOperNode(GT_ADD, TYP_I_IMPL, offset, gtNewIconNode(dataOffs, arrayElement));

    GenTree* addr = tree;

    // TODO-MIKE-Cleanup: The tree may have a zero field sequence, is should be transferred to the
    // offset node. Doing so results in some diffs that need to be reviewed so do this separately.

    addr->SetOper(GT_ADD);
    addr->AsOp()->SetOp(0, array);
    addr->AsOp()->SetOp(1, offset);
    addr->SetSideEffects(array->GetSideEffects() | offset->GetSideEffects());
    addr->gtFlags &= ~GTF_INX_RNGCHK;

    if (boundsCheck != nullptr)
    {
        addr = gtNewCommaNode(boundsCheck, addr);

        if (indexTmpStore != nullptr)
        {
            addr = gtNewCommaNode(indexTmpStore, addr);
        }

        if (arrayTmpStore != nullptr)
        {
            addr = gtNewCommaNode(arrayTmpStore, addr);
        }
    }

    return addr;
}

GenTree* Compiler::fgMorphLclVar(GenTreeLclVar* lclVar)
{
    assert(lclVar->OperIs(GT_LCL_VAR));

    LclVarDsc* lcl = lclVar->GetLcl();

    if (lcl->IsAddressExposed())
    {
        lclVar->AddSideEffects(GTF_GLOB_REF);
    }

    // Small int params, address exposed locals and promoted fields are widened on load.
    // We may need to insert a widening cast, if assertion propagation doesn't tell us
    // that the value previously stored in the local isn't already widened.

    if (!fgGlobalMorph || !lcl->lvNormalizeOnLoad())
    {
        return lclVar;
    }

#if LOCAL_ASSERTION_PROP
    if ((morphAssertionCount != 0) && morphAssertionIsTypeRange(lclVar, lcl->GetType()))
    {
        return lclVar;
    }
#endif

    // TODO-MIKE-Review: Doing this for P-DEP fields is dubious. And this should not
    // be needed for address exposed locals, we could just keeep the small int typed
    // LCL_VAR as it performs implicit widening like LCL_FLD and INT do.
    lclVar->SetType(TYP_INT);
    fgMorphTreeDone(lclVar);

    GenTreeCast* cast = gtNewCastNode(lclVar, false, lcl->GetType());
    INDEBUG(cast->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
    return cast;
}

unsigned Compiler::fgGetLargeFieldOffsetNullCheckTemp(var_types type)
{
    unsigned index;

    switch (type)
    {
        case TYP_I_IMPL:
            index = 0;
            break;
        case TYP_BYREF:
            index = 1;
            break;
        case TYP_REF:
            index = 2;
            break;
        default:
            unreached();
    }

    assert(index < _countof(fgLargeFieldOffsetNullCheckTemps));

    if (fgLargeFieldOffsetNullCheckTemps[index] == BAD_VAR_NUM)
    {
        fgLargeFieldOffsetNullCheckTemps[index] =
            lvaNewTemp(type, false DEBUGARG("large field offset null check temp"))->GetLclNum();
    }

    unsigned lclNum = fgLargeFieldOffsetNullCheckTemps[index];
    assert(lvaGetDesc(lclNum)->GetType() == type);
    return lclNum;
}

GenTree* Compiler::fgMorphFieldAddr(GenTreeFieldAddr* field, MorphAddrContext* mac)
{
    GenTreeFieldAddr* firstField    = field;
    GenTree*          addr          = field->GetAddr();
    target_size_t     offset        = field->GetOffset();
    FieldSeqStore*    fieldSeqStore = GetFieldSeqStore();
    FieldSeqNode*     fieldSeq      = field->MayOverlap() ? FieldSeqNode::NotAField() : field->GetFieldSeq();

    while (GenTreeFieldAddr* nextField = addr->IsFieldAddr())
    {
        field = nextField;
        addr  = field->GetAddr();
        offset += field->GetOffset();

        if (field->MayOverlap())
        {
            fieldSeq = FieldSeqNode::NotAField();
        }
        else
        {
            fieldSeq = fieldSeqStore->Append(field->GetFieldSeq(), fieldSeq);
        }

#ifdef FEATURE_READYTORUN_COMPILER
        if (field->GetR2RFieldLookupAddr() != nullptr)
        {
            // R2R field lookup is used only for fields of reference
            // types so this must be the last field in the sequence.
            assert(!addr->IsFieldAddr());
            break;
        }
#endif
    }

    // Static struct fields are boxed and never null.
    bool addrMayBeNull = !field->GetFieldSeq()->IsBoxedValueField();

    // Note that using fgAddrCouldBeNull with field addresses is a bit of a chicken & egg
    // case due to it returning false for ADDR(FIELD). But then ADDR(FIELD) is guaranteed
    // to be non null only if we add an explicit null check and we don't rely on the IND
    // to fault. But we've already ensured that addr isn't ADDR(FIELD) so we can rely on
    // addr being non null both to elide the explicit null check and remove the exception
    // side effect from the IND node.
    addrMayBeNull = addrMayBeNull && fgAddrCouldBeNull(addr);

    // Static field addresses are never null.
    if (addrMayBeNull)
    {
        FieldSeqNode* lastField = field->GetFieldSeq();

        if (lastField->IsField() && info.compCompHnd->isFieldStatic(lastField->GetFieldHandle()))
        {
            addrMayBeNull = false;
        }
    }

    INDEBUG(GenTreeLclAddr* lclAddr = addr->IsLocalAddrExpr();)
    assert((lclAddr == nullptr) || lclAddr->GetLcl()->IsAddressExposed());

    // null MAC means we encounter the FIELD_ADDR first. This is equivalent to a MAC_Addr
    // with zero offset.
    MorphAddrContext defMAC(true);
    if (mac == nullptr)
    {
        mac = &defMAC;
    }

    bool explicitNullCheckRequired = false;

    if (addrMayBeNull)
    {
        target_size_t totalOffset = mac->offset + offset;

        // TODO-MIKE-Review: It's odd that in the address take case we check for R2R
        // fields, but not in the indir case. In theory, the R2R field offset could
        // change enough to put us over the "big offset" limit.

        if (!mac->isOffsetConstant || fgIsBigOffset(totalOffset))
        {
            explicitNullCheckRequired = true;
        }
        else if (mac->isAddressTaken)
        {
            // TODO-MIKE-Review: This code is dubious. The original thinking was probably that
            // null + 0 = null so we don't need a null check because an indir that uses the
            // resulting address will fault anyway. But the resulting address may be further
            // adjusted by an ADD that isn't included in the MAC offset (e.g. the ADD is in a
            // different tree) or the indir is somewhere else, after other side effects, so we
            // end up reordering side effects.

            // In R2R mode the field offset for some fields may change when the code
            // is loaded. So we can't rely on a zero offset here to suppress the null check.
            // See GitHub issue #16454.

            explicitNullCheckRequired = (totalOffset != 0)
#ifdef FEATURE_READYTORUN_COMPILER
                                        || (field->GetR2RFieldLookupAddr() != nullptr)
#endif
                ;
        }
    }

    var_types      addrType      = addr->GetType();
    GenTree*       nullCheck     = nullptr;
    GenTreeLclVar* nullCheckAddr = nullptr;

    if (explicitNullCheckRequired)
    {
        GenTreeLclVar* store = nullptr;
        LclVarDsc*     lcl;

        if (addr->OperIs(GT_LCL_VAR))
        {
            nullCheckAddr = addr->AsLclVar();
            lcl           = nullCheckAddr->GetLcl();
        }
        else
        {
            lcl   = lvaGetDesc(fgGetLargeFieldOffsetNullCheckTemp(addrType));
            store = gtNewStoreLclVar(lcl, addrType, addr);
        }

        nullCheck = gtNewNullCheck(gtNewLclvNode(lcl, addrType));

        if (store != nullptr)
        {
            nullCheck = gtNewCommaNode(store, nullCheck);
        }

        addr = gtNewLclvNode(lcl, addrType);
    }

#ifdef FEATURE_READYTORUN_COMPILER
    if (field->GetR2RFieldLookupAddr() != nullptr)
    {
        GenTree* r2rOffset =
            gtNewIndOfIconHandleNode(TYP_I_IMPL, reinterpret_cast<size_t>(field->GetR2RFieldLookupAddr()),
                                     HandleKind::ConstData, true);

        addr = gtNewOperNode(GT_ADD, varTypeAddrAdd(addrType), addr, r2rOffset);
    }
#endif

    // TODO-MIKE-Cleanup: The ADD node should always be returned to avoid the zero offset field map crap.

    if (offset != 0)
    {
        GenTree* addOffset = firstField;

        addOffset->ChangeOper(GT_ADD);
        addOffset->SetType(varTypeAddrAdd(addr->GetType()));
        addOffset->AsOp()->SetOp(0, addr);
        addOffset->AsOp()->SetOp(1, gtNewIntConFieldOffset(offset, fieldSeq));
        addOffset->SetSideEffects(addr->GetSideEffects());

        addr = addOffset;
    }
    else
    {
        AddZeroOffsetFieldSeq(addr, fieldSeq);
    }

    if (nullCheck != nullptr)
    {
        addr = gtNewCommaNode(nullCheck, addr);

        // TODO-MIKE-Cleanup: Workaround to reduce diffs when switching from FIELD to FIELD_ADDR.
        // ADDR(FIELD) required FIELD to have DONT_CSE and this flag survived through a series of
        // convoluted tranforms: ADDR(FIELD(x)) - ADDR(IND(COMMA(n, x))) - ADDR(COMMA(n, IND(x)))
        // - COMMA(n, ADDR(IND(x))) - COMMA(n, x). So pretty much all field addresses that needed
        // explicit null checks were not CSE candidates. Which isn't necessarily bad as CSE seems
        // far too happy to CSE cheap constant additions, even if there's no good mechanism to
        // ensure that such CSEing doesn't result in worse register allocation and/or spilling.
        // But this didn't affect all explicit null checks - since the decision to insert the null
        // check doesn't take into account local assertion propagation, sometimes the COMMA ended
        // up being removed before having a chance to infect other nodes with DONT_CSE.
        //
        // So we're going to set DONT_CSE when we detect and address taken field case and if the
        // null check cannot be eliminated by local assertion propagation. This almost matches
        // the original behaviour with one exception: DONT_CSE was also applied if the field
        // address was immediately used by an IND - IND(ADDR(FIELD)) - but we can't detect this
        // case anymore because everything is IND(FIELD_ADDR) now. But since we have an IND the
        // DONT_CSE should be set as part of address mode marking. And hey, that's bonkers too.
        //
        // Now, can we just skip adding the null check COMMA if assertion propagation tells that
        // the address is not null? In theory yes, but it looks like there are other problems
        // that are hidden in this convoluted morphing process. One is that eliminating a null
        // check based on local assertion propagation would require us to set GTF_ORDER_SIDEEFF
        // on some node and it's not quite clear which one. optAssertionProp sets it on the
        // NULLCHECK node but then it looks like COMMA morphing code manages to drop it, likely
        // by accident. In any case, attempting to skip NULLCHECK generation like this produces
        // other diffs.

        if (mac->isAddressTaken)
        {
#if LOCAL_ASSERTION_PROP
            if ((nullCheckAddr == nullptr) || (morphAssertionCount == 0) || !morphAssertionIsNotNull(nullCheckAddr))
#endif
            {
                addr->SetDoNotCSE();
            }
        }
    }

    return addr;
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
//    If the callee has a 9 to 16 byte struct argument and the callee has
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

#if FEATURE_FASTTAILCALL
bool Compiler::fgCanFastTailCall(GenTreeCall* callee, const char** failReason)
{
    // To reach here means that the return types of the caller and callee are tail call compatible.
    if (callee->IsTailPrefixedCall())
    {
        assert(impTailCallRetTypeCompatible(callee, false));
    }

    assert(!callee->AreArgsComplete());

    fgInitArgInfo(callee);

    unsigned calleeArgStackSize = 0;
    unsigned callerArgStackSize = codeGen->paramsStackSize;

    // TODO-MIKE-Cleanup: This can probably be replaced with callee->GetInfo()->GetNextSlotNum().

    for (unsigned index = 0; index < callee->GetInfo()->GetArgCount(); ++index)
    {
        CallArgInfo* arg = callee->GetArgInfoByArgNum(index);

        if (arg->GetSlotCount() != 0)
        {
            unsigned argEndOffset = (arg->GetSlotNum() + arg->GetSlotCount()) * REGSIZE_BYTES;

            if (argEndOffset > calleeArgStackSize)
            {
                calleeArgStackSize = argEndOffset;
            }
        }
    }

    calleeArgStackSize = roundUp(calleeArgStackSize, REGSIZE_BYTES);

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

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
    // For Windows some struct parameters are copied on the local frame
    // and then passed by reference. We cannot fast tail call in these situation
    // as we need to keep our frame around.
    if (fgCallHasMustCopyByrefParameter(callee->GetInfo()))
    {
        reportFastTailCallDecision("Callee has a byref parameter");
        return false;
    }
#endif

    reportFastTailCallDecision(nullptr);
    return true;
}
#endif // FEATURE_FASTTAILCALL

#if FEATURE_FASTTAILCALL && (defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64))
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

        LclVarDsc* lcl = lclNode->GetLcl();

        JITDUMP("Arg [%06u] is implicit byref V%02u, checking if it's aliased\n", argInfo->GetNode()->gtTreeID,
                lcl->GetLclNum());

        const unsigned totalAppearances = lcl->GetImplicitByRefParamAnyRefCount();
        const unsigned callAppearances  = lcl->GetImplicitByRefParamCallRefCount();
        assert(totalAppearances >= callAppearances);

        if (totalAppearances == 1)
        {
            JITDUMP("Arg is the only use of V%02u\n", lcl->GetLclNum());

            continue;
        }

        if (totalAppearances > callAppearances)
        {
            // TODO-MIKE-Review: Main added this to fix a bug (likely the one noted in the TODOs
            // bellow). However, it seems that this is more conservative than it needs to be,
            // we probably only care if those non call appearances are due to the local being
            // address exposed.

            JITDUMP("Arg V%02u has %u non arg uses\n", lcl->GetLclNum(), totalAppearances - callAppearances);

            return true;
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

                if ((lclNode2 != nullptr) && (lclNode->GetLcl() == lclNode2->GetLcl()))
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

            if (argNode2->OperIs(GT_LCL_VAR) && (argNode2->AsLclVar()->GetLcl() == lclNode->GetLcl()))
            {
                JITDUMP("Implicit byref param V%02u address is also passed in an arg\n");
                return true;
            }

            if (argNode2->OperIs(GT_LCL_VAR) && argNode2->AsLclVar()->GetLcl()->IsParam())
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
                // Maybe lvaRetypeImplicitByRefParams could copy lvAddrExposed somewhere so we can use
                // it here, though care needs to be taken because nothing will ever update it.
                // Or use lvHasLdAddrOp for implicit byref params and lvAddrExposed for anything else.

                JITDUMP("V%02u is address exposed\n", lcl->GetLclNum());
                return true;
            }
        }

        JITDUMP("No other arg can alias V%02u\n", lcl->GetLclNum());
    }

    return false;
}
#endif // FEATURE_FASTTAILCALL && (defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64))

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
GenTree* Compiler::fgMorphPotentialTailCall(GenTreeCall* call, Statement* stmt)
{
    // It should either be an explicit (i.e. tail prefixed) or an implicit tail call
    assert(call->IsTailPrefixedCall() ^ call->IsImplicitTailCall());

    // It cannot be an inline candidate
    assert(!call->IsInlineCandidate());

    auto failTailCall = [&](const char* reason, LclVarDsc* lcl = nullptr) {
#ifdef DEBUG
        if (verbose)
        {
            printf("\nRejecting tail call in morph for call [%06u]: %s", call->GetID(), reason);
            if (lcl != nullptr)
            {
                printf(" " FMT_LCL, lcl->GetLclNum());
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
        if (!retValBuf->OperIs(GT_LCL_VAR) || (retValBuf->AsLclVar()->GetLcl()->GetLclNum() != info.compRetBuffArg))
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
    for (LclVarDsc* lcl : Locals())
    {
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
            // TODO-MIKE-Review: Shouldn't this check only lvAddrExposed? This is likely
            // the same issue fgCallHasMustCopyByrefParameter has that it can't use
            // lvAddrExposed because it's reset by lvaRetypeImplicitByRefParams.
            if (lcl->lvHasLdAddrOp && !lcl->IsImplicitByRefParam())
            {
                failTailCall("Local address taken", lcl);
                return nullptr;
            }

            if (lcl->IsAddressExposed())
            {
                failTailCall("Local address taken", lcl);
                return nullptr;
            }

            if (lcl->IsPromoted() && lcl->IsParam())
            {
                failTailCall("Has Struct Promoted Param", lcl);
                return nullptr;
            }

            // A tail call removes the method from the stack, which means the pinning
            // goes away for the callee.
            if (lcl->IsPinning())
            {
                failTailCall("Has pinning local", lcl);
                return nullptr;
            }
        }

        if (varTypeIsStruct(lcl->GetType()) && lcl->IsParam())
        {
            hasStructParam = true;
            // This prevents transforming a recursive tail call into a loop
            // but doesn't prevent tail call optimization so we need to
            // look at the rest of parameters.
        }
    }

    if (!fgCheckStmtAfterTailCall(stmt))
    {
        failTailCall("Unexpected statements after the tail call");
        return nullptr;
    }

#if FEATURE_FASTTAILCALL
    const char* failReason      = nullptr;
    const bool  canFastTailCall = fgCanFastTailCall(call, &failReason);
#else
    const char*       failReason      = "Fast tailcalls are not supported on this platform";
    constexpr bool    canFastTailCall = false;
#endif

    CORINFO_TAILCALL_HELPERS tailCallHelpers;
#ifdef TARGET_X86
    bool tailCallViaJitHelper = false;
#endif

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

#ifdef TARGET_X86
        if (fgCanTailCallViaJitHelper())
        {
            tailCallViaJitHelper = true;
        }
        else
#endif
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

#ifdef TARGET_X86
    if (tailCallViaJitHelper)
    {
        call->gtCallMoreFlags |= GTF_CALL_M_TAILCALL_VIA_JIT_HELPER;
    }
#endif

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

    JITDUMP("\nGTF_CALL_M_TAILCALL bit set for call [%06u]\n", call->GetID());

    if (fastTailCallToLoop)
    {
        JITDUMP("\nGTF_CALL_M_TAILCALL_TO_LOOP bit set for call [%06u]\n", call->GetID());
    }

    // If this block has a flow successor, make suitable updates.
    BasicBlock*       callBlock = fgMorphBlock;
    BasicBlock* const nextBlock = callBlock->GetUniqueSucc();

    if (nextBlock == nullptr)
    {
        assert(callBlock->bbJumpKind == BBJ_RETURN);
    }
    else
    {
        // Flow no longer reaches nextBlock from here.
        //
        fgRemoveRefPred(nextBlock, callBlock);

        // Adjust profile weights.
        //
        // Note if this is a tail call to loop, further updates
        // are needed once we install the loop edge.
        //
        if (callBlock->hasProfileWeight() && nextBlock->hasProfileWeight())
        {
            // Since we have linear flow we can update the next block weight.
            //
            BasicBlock::weight_t const blockWeight   = callBlock->bbWeight;
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
                        nextBlock->bbNum, nextWeight, callBlock->bbNum, blockWeight);
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

                    LclVarDsc* lcl = asgNode->gtGetOp1()->AsLclVarCommon()->GetLcl();

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

                            assert(lcl == rhs->AsLclVarCommon()->GetLcl());
                            lcl = rhs->AsLclVarCommon()->GetLcl();
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
                                nextNextBlock->bbNum, nextNextWeight, callBlock->bbNum, blockWeight);
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
        callBlock->bbJumpKind = BBJ_RETURN;
    }

    GenTree* stmtExpr = stmt->GetRootNode();

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

    if (!canFastTailCall X86_ONLY(&&!tailCallViaJitHelper))
    {
        // For tailcall via CORINFO_TAILCALL_HELPERS we transform into regular
        // calls with (to the JIT) regular control flow so we do not need to do
        // much special handling.

        return fgMorphTailCallViaHelpers(call, tailCallHelpers, stmt);
    }

    // Otherwise we will transform into something that does not return. For
    // fast tailcalls a "jump" and for tailcall via JIT helper a call to a
    // JIT helper that does not return. So peel off everything after the
    // call.
    Statement* nextMorphStmt = stmt->GetNextStmt();
    JITDUMP("Remove all stmts after the call.\n");
    while (nextMorphStmt != nullptr)
    {
        Statement* stmtToRemove = nextMorphStmt;
        nextMorphStmt           = stmtToRemove->GetNextStmt();
        fgRemoveStmt(callBlock, stmtToRemove);
    }

    bool     isRootReplaced = false;
    GenTree* root           = stmt->GetRootNode();

    if (root != call)
    {
        JITDUMP("Replace root node [%06d] with [%06d] tail call node.\n", dspTreeID(root), dspTreeID(call));
        isRootReplaced = true;
        stmt->SetRootNode(call);
    }

    var_types retType = call->GetType();

    // Avoid potential extra work for the return (for example, vzeroupper)
    call->SetType(TYP_VOID);
    call->SetRetSigType(TYP_VOID);
    call->SetRetLayout(nullptr);
    call->GetRetDesc()->Reset();

#ifdef TARGET_X86
    static_assert_no_msg(!canFastTailCall);
    assert(tailCallViaJitHelper);

    fgMorphTailCallViaJitHelper(call);

    // Force re-evaluating the argInfo. fgMorphTailCallViaJitHelper will
    // modify the argument list, invalidating the argInfo.
    call->fgArgInfo = nullptr;

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
    // we check whether the first and current basic blocks are GC-SafePoints.

    BasicBlock* newCallBlock = callBlock;

    if (!fgFirstBB->HasGCSafePoint() && !callBlock->HasGCSafePoint())
    {
        newCallBlock = fgCreateGCPoll(GCPOLL_INLINE, callBlock);
    }

    if (newCallBlock == callBlock)
    {
        // We didn't insert a poll block, so we need to morph the call now
        // (normally it will get morphed when we get to the poll block).
        GenTree* temp = fgMorphCall(call, stmt);
        noway_assert(temp == call);
    }
    else
    {
        // fgCreateGCPoll has created new blocks and moved the call to one of them.
        callBlock    = newCallBlock;
        fgMorphBlock = callBlock;
    }

    noway_assert(callBlock->bbJumpKind == BBJ_RETURN);

    // We call CORINFO_HELP_TAILCALL which does not return, so we will
    // not need epilogue.
    callBlock->bbJumpKind = BBJ_THROW;
#else
    // Fast Tail call as epilog + jmp - No need to insert GC-poll. Instead,
    // fgSetFullyInterruptiblePhase is going to mark the method as fully
    // interruptible if the block containing this tail call is reachable
    // without executing any call.

    GenTree* temp = fgMorphCall(call, stmt);
    noway_assert(temp == call);
    noway_assert(callBlock->bbJumpKind == BBJ_RETURN);
    callBlock->bbFlags |= BBF_HAS_JMP;
#endif

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

        return fgMorphTree(gtNewZeroConNode(retType));
    }

    return call;
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
//           ReturnAddress address
//           {CallTargetStub}
//           ReturnValue address
//         GT_LCL ReturnValue
// whenever the call node returns a value. If the call node does not return a
// value the last comma will not be there.
//
GenTree* Compiler::fgMorphTailCallViaHelpers(GenTreeCall* call, CORINFO_TAILCALL_HELPERS& help, Statement* stmt)
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

    GenTree* callDispatcherAndGetResult =
        fgCreateCallDispatcherAndGetResult(call, help.hCallTarget, help.hDispatcher, stmt);

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
                LclVarDsc* lcl = lvaNewTemp(objp->GetType(), true DEBUGARG("tail call thisptr"));

                // tmp = "this"
                doBeforeStoreArgsStub = gtNewStoreLclVar(lcl, objp->GetType(), objp);

                if (callNeedsNullCheck)
                {
                    // COMMA(tmp = "this", deref(tmp))
                    GenTree* tmp          = gtNewLclvNode(lcl, objp->TypeGet());
                    GenTree* nullcheck    = gtNewNullCheck(tmp);
                    doBeforeStoreArgsStub = gtNewCommaNode(doBeforeStoreArgsStub, nullcheck);
                }

                thisPtr = gtNewLclvNode(lcl, objp->TypeGet());

                if (stubNeedsThisPtr)
                {
                    thisPtrStubArg = gtNewLclvNode(lcl, objp->TypeGet());
                }
            }
            else
            {
                if (callNeedsNullCheck)
                {
                    // deref("this")
                    doBeforeStoreArgsStub = gtNewNullCheck(objp);

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
            if (call->IsIndirectCall())
            {
                target = call->gtCallAddr;

                noway_assert(target != nullptr);
            }
            else
            {
                CORINFO_CONST_LOOKUP lookup;
                info.compCompHnd->getFunctionEntryPoint(call->GetMethodHandle(), &lookup);
                target = gtNewConstLookupTree(lookup, HandleKind::MethodAddr, call->GetMethodHandle());
            }
        }
        else
        {
            assert(!call->tailCallInfo->GetSig()->hasTypeArg());

            CORINFO_CALLINFO_FLAGS flags = CORINFO_CALLINFO_LDFTN;

            if (call->tailCallInfo->IsCallvirt())
            {
                flags = static_cast<CORINFO_CALLINFO_FLAGS>(flags | CORINFO_CALLINFO_CALLVIRT);
            }

            CORINFO_CALL_INFO callInfo;
            eeGetCallInfo(call->tailCallInfo->GetToken(), nullptr, flags, &callInfo);
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
                                                      CORINFO_METHOD_HANDLE dispatcherHnd,
                                                      Statement*            stmt)
{
    GenTreeCall* callDispatcherNode = gtNewUserCallNode(dispatcherHnd, TYP_VOID, nullptr, stmt->GetILOffsetX());
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

        assert(retBufArg->AsLclVar()->GetLcl()->GetLclNum() == info.compRetBuffArg);

        // Caller return buffer argument retBufArg can point to GC heap while the dispatcher expects
        // the return value argument retValArg to point to the stack.
        // We use a temporary stack allocated return buffer to hold the value during the dispatcher call
        // and copy the value back to the caller return buffer after that.
        LclVarDsc* tmpRetBufLcl =
            lvaNewTemp(origCall->GetRetLayout(), true DEBUGARG("substitute local for return buffer"));
        lvaSetAddressExposed(tmpRetBufLcl);

        var_types tmpRetBufType = tmpRetBufLcl->TypeGet();

        retValArg = gtNewLclVarAddrNode(tmpRetBufLcl);

        LclVarDsc* retBuffLcl       = lvaGetDesc(info.compRetBuffArg);
        var_types  callerRetBufType = retBuffLcl->GetType();

        GenTree* dstAddr = gtNewLclvNode(retBuffLcl, callerRetBufType);
        GenTree* dst     = gtNewObjNode(origCall->GetRetLayout(), dstAddr);
        GenTree* src     = gtNewLclvNode(tmpRetBufLcl, tmpRetBufType);

        copyToRetBufNode         = dst;
        copyToRetBufNode->gtOper = copyToRetBufNode->TypeIs(TYP_STRUCT) ? GT_STORE_OBJ : GT_STOREIND;
        copyToRetBufNode->AsIndir()->SetValue(src);

        if (origCall->gtType != TYP_VOID)
        {
            retVal = gtClone(retBufArg);
        }
    }
    else if (!origCall->TypeIs(TYP_VOID))
    {
        JITDUMP("Creating a new temp for the return value\n");

        LclVarDsc* newRetLcl = lvaAllocTemp(false DEBUGARG("Return value for tail call dispatcher"));

        if (varTypeIsStruct(origCall->GetType()))
        {
            lvaSetStruct(newRetLcl, origCall->GetRetLayout(), false);
        }
        else
        {
            newRetLcl->SetType(origCall->GetType());
        }

        lvaSetAddressExposed(newRetLcl);

        if (varTypeIsSmall(origCall->GetRetSigType()))
        {
            // Use a LCL_FLD to widen small int return, the local is already address exposed
            // so it's not worth adding an extra cast by relying on "normalize on load".
            retVal = gtNewLclFldNode(newRetLcl, origCall->GetRetSigType(), 0);
        }
        else
        {
            retVal = gtNewLclvNode(newRetLcl, newRetLcl->GetType());
        }

        if (varTypeIsStruct(origCall->GetType()) && (info.retDesc.GetRegCount() > 1))
        {
            retVal->gtFlags |= GTF_DONT_CSE;
        }

        retValArg = gtNewLclVarAddrNode(newRetLcl);
    }
    else
    {
        JITDUMP("No return value so using null pointer as arg\n");
        retValArg = gtNewZeroConNode(TYP_I_IMPL);
    }

    callDispatcherNode->gtCallArgs = gtPrependNewCallArg(retValArg, callDispatcherNode->gtCallArgs);

    // Add callTarget
    callDispatcherNode->gtCallArgs =
        gtPrependNewCallArg(new (this, GT_METHOD_ADDR) GenTreeMethodAddr(callTargetStubHnd),
                            callDispatcherNode->gtCallArgs);

    // Add the caller's return address slot.
    if (lvaRetAddrVar == BAD_VAR_NUM)
    {
        LclVarDsc* lcl = lvaNewTemp(TYP_I_IMPL, false DEBUGARG("Return address"));
        lvaSetAddressExposed(lcl);
        lvaRetAddrVar = lcl->GetLclNum();
    }

    GenTree* retAddrSlot           = gtNewLclVarAddrNode(lvaGetDesc(lvaRetAddrVar));
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

GenTree* Compiler::getRuntimeLookupTree(CORINFO_RUNTIME_LOOKUP_KIND kind,
                                        CORINFO_RUNTIME_LOOKUP&     lookup,
                                        void*                       compileTimeHandle)
{
    assert(!compIsForInlining());

    // If pRuntimeLookup->indirections is equal to CORINFO_USEHELPER, it specifies that a run-time helper should be
    // used; otherwise, it specifies the number of indirections via pRuntimeLookup->offsets array.
    if ((lookup.indirections == CORINFO_USEHELPER) || lookup.testForNull || lookup.testForFixup)
    {
        // If the first condition is true, runtime lookup tree is available only via the run-time helper function.
        // TODO-CQ If the second or third condition is true, we are always using the slow path since we can't
        // introduce control flow at this point. See impRuntimeLookupToTree for the logic to avoid calling the helper.
        // The long-term solution is to introduce a new node representing a runtime lookup, create instances
        // of that node both in the importer and here, and expand the node in lower (introducing control flow if
        // necessary).
        return gtNewRuntimeLookupHelperCallNode(&lookup, gtNewRuntimeContextTree(kind), compileTimeHandle);
    }

    GenTree* result = gtNewRuntimeContextTree(kind);

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
        LclVarDsc* tempLcl = lvaNewTemp(tree->GetType(), true DEBUGARG(reason));
        stmts.Push(gtNewAssignNode(gtNewLclvNode(tempLcl, tree->GetType()), tree));
        *use = gtNewLclvNode(tempLcl, tree->GetType());
        return gtNewLclvNode(tempLcl, tree->GetType());
    };

    // Apply repeated indirections
    for (unsigned i = 0; i < lookup.indirections; i++)
    {
        GenTree* preInd = nullptr;
        if ((i == 1 && lookup.indirectFirstOffset) || (i == 2 && lookup.indirectSecondOffset))
        {
            preInd = cloneTree(&result DEBUGARG("getRuntimeLookupTree indirectOffset"));
        }

        if (i != 0)
        {
            result = gtNewIndir(TYP_I_IMPL, result);
            result->gtFlags |= GTF_IND_NONFAULTING | GTF_IND_INVARIANT;
        }

        if ((i == 1 && lookup.indirectFirstOffset) || (i == 2 && lookup.indirectSecondOffset))
        {
            result = gtNewOperNode(GT_ADD, TYP_I_IMPL, preInd, result);
        }

        if (lookup.offsets[i] != 0)
        {
            result = gtNewOperNode(GT_ADD, TYP_I_IMPL, result, gtNewIconNode(lookup.offsets[i], TYP_I_IMPL));
        }
    }

    assert(!lookup.testForNull);

    if (lookup.indirections > 0)
    {
        assert(!lookup.testForFixup);

        result = gtNewIndir(TYP_I_IMPL, result);
        result->gtFlags |= GTF_IND_NONFAULTING;
    }

    // Produces GT_COMMA(stmt1, GT_COMMA(stmt2, ... GT_COMMA(stmtN, result)))

    while (!stmts.Empty())
    {
        result = gtNewCommaNode(stmts.Pop(), result);
    }

    JITDUMPTREE(result, "Created runtime lookup tree:");

    return result;
}

GenTree* Compiler::getVirtMethodPointerTree(GenTree*                thisPtr,
                                            CORINFO_RESOLVED_TOKEN* resolvedToken,
                                            CORINFO_CALL_INFO*      callInfo)
{
    GenTree* exactTypeDesc   = getTokenHandleTree(resolvedToken, true);
    GenTree* exactMethodDesc = getTokenHandleTree(resolvedToken, false);

    return gtNewHelperCallNode(CORINFO_HELP_VIRTUAL_FUNC_PTR, TYP_I_IMPL,
                               gtNewCallArgs(thisPtr, exactTypeDesc, exactMethodDesc));
}

GenTree* Compiler::getTokenHandleTree(CORINFO_RESOLVED_TOKEN* resolvedToken, bool parent)
{
    CORINFO_GENERICHANDLE_RESULT embedInfo;
    info.compCompHnd->embedGenericHandle(resolvedToken, parent, &embedInfo);

    if (!embedInfo.lookup.lookupKind.needsRuntimeLookup)
    {
        return gtNewConstLookupTree(embedInfo.lookup.constLookup, TokenToHandleKind(resolvedToken->token),
                                    embedInfo.compileTimeHandle);
    }

    GenTree* result = getRuntimeLookupTree(embedInfo.lookup.lookupKind.runtimeLookupKind,
                                           embedInfo.lookup.runtimeLookup, embedInfo.compileTimeHandle);

    if (result != nullptr)
    {
        result = gtNewRuntimeLookup(embedInfo.compileTimeHandle, embedInfo.handleType, result);
    }

    return result;
}

#ifdef TARGET_X86
bool Compiler::fgCanTailCallViaJitHelper()
{
    // The JIT helper does not properly handle the case where localloc was used.
    return !compLocallocUsed;
}

void Compiler::fgMorphTailCallViaJitHelper(GenTreeCall* call)
{
    JITDUMPTREE(call, "fgMorphTailCallViaJitHelper (before):\n");

    assert(!call->IsUnmanaged());
    assert(!call->IsHelperCall());
    assert(call->IsVirtual() || !call->IsIndirectCall() || (call->gtCallCookie == nullptr));
    assert(!call->IsImplicitTailCall());

    // The call will be transformed into a helper call so it can no longer have a special
    // `this` arg, we need to change it to a normal (first) argument. This may result in
    // an ordering problem
    //   - the first argument is passed in a register and will become a late arg
    //   - the target argument is passed on stack so it will be evaluated before late args
    //   - the target expression may depend on `this` (e.g. virtual vtable call)
    // So we need to ensure that the value of `this` is available for target's expression.
    // We do this by spilling `this` to a temp, if it's not already a local.
    if (call->gtCallThisArg != nullptr)
    {
        GenTree* thisArg    = call->gtCallThisArg->GetNode();
        call->gtCallThisArg = nullptr;

        GenTree* newThisArg = nullptr;

        // TODO-MIKE-Review: Not adding a temp if `this` is LCL_VAR is dubious, what if some
        // other argument expression modifies it?
        if ((call->IsDelegateInvoke() || call->IsVirtualVtable()) && !thisArg->OperIs(GT_LCL_VAR))
        {
            LclVarDsc* lcl = lvaNewTemp(thisArg->GetType(), true DEBUGARG("tail call target this temp"));

            // TODO-MIKE-Review: fgMorphArgs freaks out when it sees side effects and adds
            // another temp for this argument...
            // What we probably want is to have fgMorphArgs deal with this.
            GenTree* asg = gtNewStoreLclVar(lcl, thisArg->GetType(), thisArg);
            newThisArg   = gtNewCommaNode(asg, gtNewLclvNode(lcl, thisArg->GetType()));

            thisArg = newThisArg;
        }

        // The runtime requires that we perform a null check on the `this` argument before tail
        // calling to a virtual dispatch stub. This requirement is a consequence of limitations
        // in the runtime's ability to map an AV to a NullReferenceException if the AV occurs
        // in a dispatch stub that has unmanaged caller.
        if (call->IsVirtualStub())
        {
            call->gtFlags |= GTF_CALL_NULLCHECK;
        }

        if (call->NeedsNullCheck())
        {
            if ((newThisArg == nullptr) && ((thisArg->gtFlags & GTF_SIDE_EFFECT) == 0))
            {
                newThisArg = gtClone(thisArg, true);
            }

            if (newThisArg == nullptr)
            {
                LclVarDsc* lcl = lvaNewTemp(thisArg->GetType(), true DEBUGARG("tail call nullcheck this temp"));

                // TODO-MIKE-Review: The NULLCHECK gets added in the wrong place, in the first
                // argument tree. This means it happens before other arguments are evaluated,
                // instead of happening after, right before the call.
                GenTree* asg = gtNewStoreLclVar(lcl, thisArg->GetType(), thisArg);
                newThisArg   = gtNewCommaNode(asg, gtNewNullCheck(gtNewLclvNode(lcl, thisArg->GetType())));
                newThisArg   = gtNewCommaNode(newThisArg, gtNewLclvNode(lcl, thisArg->GetType()));
            }
            else
            {
                newThisArg = gtNewCommaNode(gtNewNullCheck(newThisArg), gtClone(thisArg, true));
            }

            call->gtFlags &= ~GTF_CALL_NULLCHECK;
        }
        else
        {
            newThisArg = thisArg;
        }

        // TODO-Cleanup: We leave it as a virtual stub call to use logic in LowerVirtualStubCall,
        // clear GTF_CALL_VIRT_KIND_MASK here and change LowerCall to recognize it as a direct call.

        call->gtCallArgs = gtPrependNewCallArg(newThisArg, call->gtCallArgs);
    }

    // The tailcall helper has 4 extra arguments:
    //   JIT_TailCall(<function args>, int numOldStackSlots, int numNewStackSlots, int flags, void* target)
    //
    // Note that the special arguments are on the stack, whereas the function arguments follow
    // the normal convention: there might be register arguments in ECX and EDX. The stack will
    // look like (highest address at the top):
    //      first normal stack argument
    //      ...
    //      last normal stack argument
    //      numOldStackSlots - the number of stack slots used by caller parameters
    //      numNewStackSlots - the number of stack slots needed for callee parameters
    //      flags
    //      target
    //
    // 'flags' is a bitmask where:
    //      1 - restore callee-save registers (EDI, ESI, EBX). The JIT always saves all
    //          callee-saved registers for tailcall functions. Note that the helper assumes
    //          that the callee-saved registers live immediately below EBP, and must have been
    //          pushed in this order: EDI, ESI, EBX.
    //      2 - call target is a virtual stub dispatch.
    //
    // The tail call helper lives in vm\i386\jithelp.asm. See that function for more details
    // on the custom calling convention.

    GenTree* numOldStackSlotsArg = gtNewIconNode(static_cast<int>(codeGen->paramsStackSize / REGSIZE_BYTES));
    // We haven't yet morphed the args so we don't know the number of stack slots this call uses.
    // Lowering will change this to the correct value.
    GenTree* numNewStackSlotsArg = gtNewIconNode(0);
    // TODO-MIKE-Review: Seems like we could set the real flags here, not in lowering.
    GenTree* flagsArg = gtNewIconNode(0);
    GenTree* targetArg;

    if (call->IsIndirectCall())
    {
        // Use the indirect call target as target argument. Note that since that target argument is
        // last this doesn't change eveluation order.
        targetArg = call->gtCallAddr;
        // Put a dummy 0 node so we can keep the call as indirect for now.
        // TODO-MIKE-Review: Why not transform into the actual helper call here?
        call->gtCallAddr = gtNewIconNode(0);
    }
    else
    {
        // We haven't created the target expression yet (e.g. for vtable calls), lowering will change
        // this as needed.
        targetArg = gtNewIconNode(0);
    }

    GenTreeCall::Use* newArgs = gtNewCallArgs(numOldStackSlotsArg, numNewStackSlotsArg, flagsArg, targetArg);
    GenTreeCall::Use* lastArg = nullptr;

    for (GenTreeCall::Use& use : call->Args())
    {
        lastArg = &use;
    }

    if (lastArg == nullptr)
    {
        call->gtCallArgs = newArgs;
    }
    else
    {
        lastArg->SetNext(newArgs);
    }

    call->gtFlags &= ~GTF_CALL_POP_ARGS;

    assert(!call->NeedsNullCheck());

    JITDUMPTREE(call, "fgMorphTailCallViaJitHelper (after):\n");
}
#endif // TARGET_X86

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

    if (call->IsIndirectCall())
    {
        return gtClone(call->gtCallAddr, true);
    }

    assert(call->gtCallMoreFlags & GTF_CALL_M_VIRTSTUB_REL_INDIRECT);
    GenTreeIntCon* stubAddrArg = gtNewIconHandleNode(call->gtStubCallStubAddr, HandleKind::MethodAddr);
    stubAddrArg->SetDumpHandle(call->GetMethodHandle());
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
    if ((thisArg != nullptr) && !thisArg->GetNode()->IsNothingNode() && !thisArg->GetNode()->OperIs(GT_ARGPLACE))
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
        if (!earlyArg->IsNothingNode() && !earlyArg->OperIs(GT_ARGPLACE))
        {
            // TODO-MIKE-Cleanup: It should be possible to avoid calling GetArgInfoByArgNode here,
            // and the linear search it performs...
            CallArgInfo* argInfo = recursiveTailCall->GetArgInfoByArgNode(earlyArg);
            if (argInfo->HasLateUse())
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

    // If the method has starg.s 0 or ldarga.s 0 a special local (lvaThisLclNum) is created so that
    // compThisArg stays immutable. Normally it's assigned in fgFirstBBScratch block. Since that
    // block won't be in the loop (it's assumed to have no predecessors), we need to update the special local here.
    if (!info.compIsStatic && (lvaThisLclNum != info.GetThisParamLclNum()))
    {
        LclVarDsc* thisParamLcl = lvaGetDesc(info.GetThisParamLclNum());
        GenTree*   value        = gtNewLclvNode(thisParamLcl, thisParamLcl->GetType());
        GenTree*   store        = gtNewStoreLclVar(lvaGetDesc(lvaThisLclNum), thisParamLcl->GetType(), value);
        fgInsertStmtBefore(block, paramAssignmentInsertionPoint, gtNewStmt(store, callILOffset));
    }

    // If compInitMem is set, we may need to zero-initialize some locals. Normally it's done in the prolog
    // but this loop can't include the prolog. Since we don't have liveness information, we insert zero-initialization
    // for all non-parameter IL locals as well as temp structs with GC fields.
    // Liveness phase will remove unnecessary initializations.
    if (info.compInitMem || compSuppressedZeroInit)
    {
        for (LclVarDsc* lcl : Locals())
        {
            if (!lcl->IsParam())
            {
#if FEATURE_FIXED_OUT_ARGS
                if (lcl->GetLclNum() == lvaOutgoingArgSpaceVar)
                {
                    continue;
                }
#endif
                bool isUserLocal        = lcl->GetLclNum() < info.compLocalsCount;
                bool structWithGCFields = lcl->TypeIs(TYP_STRUCT) && lcl->GetLayout()->HasGCPtr();
                bool hadSuppressedInit  = lcl->lvSuppressedZeroInit;

                if ((info.compInitMem && (isUserLocal || structWithGCFields)) || hadSuppressedInit)
                {
                    fgMorphCreateLclInit(lcl, block, lastStmt, callILOffset);
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

void Compiler::fgMorphCreateLclInit(LclVarDsc* lcl, BasicBlock* block, Statement* beforeStmt, IL_OFFSETX ilOffset)
{
    if (lcl->IsIndependentPromoted())
    {
        for (LclVarDsc* fieldLcl : PromotedFields(lcl))
        {
            fgMorphCreateLclInit(fieldLcl, block, beforeStmt, ilOffset);
        }

        return;
    }

    var_types lclType = lcl->GetType();
    GenTree*  init    = nullptr;

    if (!varTypeIsStruct(lclType))
    {
        init = gtNewZeroConNode(varActualType(lclType));
    }
#ifdef FEATURE_SIMD
    else if (varTypeIsSIMD(lclType))
    {
        init = gtNewZeroSimdHWIntrinsicNode(lcl->GetLayout());
    }
#endif
    else
    {
        init = gtNewIconNode(0);
    }

    init = gtNewStoreLclVar(lcl, lclType, init);

    if (lcl->IsAddressExposed())
    {
        init->gtFlags |= GTF_GLOB_REF;
    }

    fgInsertStmtBefore(block, beforeStmt, gtNewStmt(init, ilOffset));
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

    GenTree*   argInTemp             = nullptr;
    LclVarDsc* originalArgLcl        = lvaGetDesc(argTabEntry->GetArgNum());
    bool       needToAssignParameter = true;

    // TODO-CQ: enable calls with struct arguments passed in registers.
    noway_assert(!varTypeIsStruct(arg->GetType()));

    if (argTabEntry->HasTemp() || arg->IsIntCon() || arg->IsDblCon())
    {
        // The argument is already assigned to a temp or is a const.
        argInTemp = arg;
    }
    else if (arg->OperIs(GT_LCL_VAR))
    {
        LclVarDsc* lcl = arg->AsLclVar()->GetLcl();

        if (!lcl->IsParam())
        {
            // The argument is a non-parameter local so it doesn't need to be assigned to a temp.
            argInTemp = arg;
        }
        else if (lcl == originalArgLcl)
        {
            // The argument is the same parameter local that we were about to assign so
            // we can skip the assignment.
            needToAssignParameter = false;
        }
    }

    // TODO: We don't need temp assignments if we can prove that the argument tree doesn't involve
    // any caller parameters. Some common cases are handled above but we may be able to eliminate
    // more temp assignments.

    Statement* paramStoreStmt = nullptr;
    if (needToAssignParameter)
    {
        if (argInTemp == nullptr)
        {
            // The argument is not stored to a temp. We need to create a new temp and insert a store.
            // TODO: we can avoid a temp store if we can prove that the argument tree
            // doesn't involve any caller parameters.
            LclVarDsc* tmpLcl = lvaNewTemp(arg->GetType(), true DEBUGARG("arg temp"));

            GenTree*   tmpStore     = gtNewStoreLclVar(tmpLcl, arg->GetType(), arg);
            Statement* tmpStoreStmt = gtNewStmt(tmpStore, callILOffset);
            fgInsertStmtBefore(block, tmpAssignmentInsertionPoint, tmpStoreStmt);
            argInTemp = gtNewLclvNode(tmpLcl, arg->GetType());
        }

        // Now store the temp to the parameter.
        assert(originalArgLcl->IsParam());
        GenTree* paramStore = gtNewStoreLclVar(originalArgLcl, originalArgLcl->GetType(), argInTemp);
        paramStoreStmt      = gtNewStmt(paramStore, callILOffset);

        fgInsertStmtBefore(block, paramAssignmentInsertionPoint, paramStoreStmt);
    }
    return paramStoreStmt;
}

bool Compiler::IsCallGCSafePoint(GenTreeCall* call)
{
    if (call->IsFastTailCall())
    {
        return false;
    }

    if (call->IsUnmanaged() && call->IsSuppressGCTransition())
    {
        return false;
    }

    if (call->IsIndirectCall())
    {
        return true;
    }

    if (call->IsUserCall() && ((call->gtCallMoreFlags & GTF_CALL_M_NOGCCHECK) == 0))
    {
        return true;
    }

    return false;
}

GenTree* Compiler::fgMorphCall(GenTreeCall* call, Statement* stmt)
{
    if (call->CanTailCall())
    {
        GenTree* newNode = fgMorphPotentialTailCall(call, stmt);
        if (newNode != nullptr)
        {
            return newNode;
        }

        assert(!call->CanTailCall());
    }

    if (((call->gtCallMoreFlags & GTF_CALL_M_SPECIAL_INTRINSIC) == 0) &&
        ((call->GetMethodHandle() == eeFindHelper(CORINFO_HELP_VIRTUAL_FUNC_PTR))
#ifdef FEATURE_READYTORUN_COMPILER
         || (call->GetMethodHandle() == eeFindHelper(CORINFO_HELP_READYTORUN_VIRTUAL_FUNC_PTR))
#endif
             ) &&
        (stmt != nullptr) && (call == stmt->GetRootNode()))
    {
        // This is call to CORINFO_HELP_VIRTUAL_FUNC_PTR with ignored result.
        // Transform it into a null check.

        GenTree* thisPtr = call->gtCallArgs->GetNode();

        return fgMorphTree(gtNewNullCheck(thisPtr));
    }

    if (fgGlobalMorph)
    {
        // TODO-MIKE-Cleanup: This should be moved to lowering (or LSRA's "build",
        // only rpMustCreateEBPFrame needs it and doing it here is premature as
        // calls can be removed later.
        if (call->IsIndirectCall())
        {
            optCallCount++;
            optIndirectCallCount++;
        }
        else if (call->IsUserCall())
        {
            optCallCount++;

            if (call->IsVirtual())
            {
                optIndirectCallCount++;
            }
        }
    }

    BasicBlock* callBlock = fgMorphBlock;

    // Mark the block as a GC safe point for the call if possible.
    // In the event the call indicates the block isn't a GC safe point
    // and the call is unmanaged with a GC transition suppression request
    // then insert a GC poll.
    if (IsCallGCSafePoint(call))
    {
        callBlock->bbFlags |= BBF_GC_SAFE_POINT;
    }

    // Regardless of the state of the basic block with respect to GC safe point,
    // we will always insert a GC Poll for scenarios involving a suppressed GC
    // transition. Only mark the block for GC Poll insertion on the first morph.
    if (fgGlobalMorph && call->IsUnmanaged() && call->IsSuppressGCTransition())
    {
        callBlock->bbFlags |= (BBF_HAS_SUPPRESSGC_CALL | BBF_GC_SAFE_POINT);
        optMethodFlags |= OMF_NEEDS_GCPOLLS;
    }

    // Morph Type.op_Equality, Type.op_Inequality, and Enum.HasFlag
    // We need to do these before the arguments are morphed.
    if ((call->gtCallMoreFlags & GTF_CALL_M_SPECIAL_INTRINSIC) != 0)
    {
        // TODO-MIKE-Review: Hrm, above we already marked the block as
        // containing a GC safe point and are we removing the call?!?

        GenTree* optTree = gtFoldExprCall(call);

        if (optTree != call)
        {
            return fgMorphTree(optTree);
        }
    }

    // TODO-MIKE-Cleanup: Only CSE needs this.
    // Move this to SsaBuilder, where BBF_HAS_NULLCHECK and other similar flags are set.
    // Note that there is code below that can remove the call, but we're still setting
    // this flag here. Also note that INTRINSIC nodes may become calls but we don't set
    // this flag for those, that may affect CSE.
    callBlock->bbFlags |= BBF_HAS_CALL;

    fgMorphArgs(call);

    if (call->IsExpandedEarly() && call->IsVirtualVtable())
    {
        if (call->gtControlExpr == nullptr)
        {
            assert(fgGlobalMorph);
            call->gtControlExpr = fgExpandVirtualVtableCallTarget(call);
        }

        call->gtControlExpr = fgMorphTree(call->gtControlExpr);
        call->AddSideEffects(call->gtControlExpr->GetSideEffects());
    }

    // Morph stelem.ref helper call to store a null value, into a store into an array without the helper.
    // This needs to be done after the arguments are morphed to ensure constant propagation has already taken place.
    if (opts.OptimizationEnabled() && call->IsHelperCall(eeFindHelper(CORINFO_HELP_ARRADDR_ST)))
    {
        GenTree* value = call->GetArgNodeByArgNum(2);
        if (value->IsIntegralConst(0))
        {
            return fgRemoveArrayStoreHelperCall(call, value);
        }
    }

    if (call->IsNoReturn())
    {
        // If we know that the call does not return then we can set fgRemoveRestOfBlock
        // to remove all subsequent statements and change the call's basic block to BBJ_THROW.
        // As a result the compiler won't need to preserve live registers across the call.
        //
        // This isn't need for tail calls as there shouldn't be any code after the call anyway.
        // Besides, the tail call code is part of the epilog and converting the block to
        // BBJ_THROW would result in the tail call being dropped as the epilog is generated
        // only for BBJ_RETURN blocks.

        if (!call->IsTailCall())
        {
            fgRemoveRestOfBlock = true;
        }
    }

    return call;
}

GenTree* Compiler::fgRemoveArrayStoreHelperCall(GenTreeCall* call, GenTree* value)
{
    assert((value == call->GetArgNodeByArgNum(2)) && (value->AsIntCon()->GetValue() == 0));

    GenTree* arr   = call->GetArgNodeByArgNum(0);
    GenTree* index = call->GetArgNodeByArgNum(1);

    // Either or both of the array and index arguments may have been spilled to temps by `fgMorphArgs`. Copy
    // the spill trees as well if necessary.
    GenTreeOp* argSetup = nullptr;
    for (GenTreeCall::Use& use : call->Args())
    {
        GenTree* const arg = use.GetNode();
        if (!arg->OperIs(GT_LCL_DEF, GT_STORE_LCL_VAR))
        {
            continue;
        }

        assert(arg != arr);
        assert(arg != index);

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

    GenTree* result;

    if (arr->IsIntegralConst(0))
    {
        result = gtNewIndir(TYP_I_IMPL, arr);
    }
    else
    {
        GenTreeIndexAddr* addr         = gtNewArrayIndexAddr(arr, index, TYP_REF);
        GenTreeIndir*     arrIndexNode = gtNewIndexIndir(TYP_REF, addr);
        if (!fgGlobalMorph && !opts.MinOpts())
        {
            arrIndexNode->SetAddr(fgMorphIndexAddr(addr));
        }
        result = gtNewAssignNode(arrIndexNode, value);
    }

    result = fgMorphTree(result);

    if (argSetup != nullptr)
    {
        result = gtNewCommaNode(argSetup, result);
        INDEBUG(result->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)
    }

    return result;
}

// Expand and return the call target address for a VirtualCall
// The code here should match that generated by LowerVirtualVtableCall
GenTree* Compiler::fgExpandVirtualVtableCallTarget(GenTreeCall* call)
{
    JITDUMP("Expanding virtual call target for %d.%s:\n", call->GetID(), GenTree::OpName(call->GetOper()));

    noway_assert(call->IsUserCall());

    GenTree* thisPtr = call->GetArgInfoByArgNum(0)->GetNode();

    // fgMorphArgs must enforce this invariant by creating a temp
    // TODO-MIKE-Review: Allowing LCL_FLD (or DNER LCL_VAR) may be bad for CQ.
    noway_assert(thisPtr->OperIs(GT_LCL_VAR, GT_LCL_FLD));
    thisPtr = gtClone(thisPtr, true);
    assert(thisPtr != nullptr);

    unsigned vtabOffsOfIndirection;
    unsigned vtabOffsAfterIndirection;
    bool     isRelative;
    info.compCompHnd->getMethodVTableOffset(call->gtCallMethHnd, &vtabOffsOfIndirection, &vtabOffsAfterIndirection,
                                            &isRelative);

    // Dereference the this pointer to obtain the method table, it is called vtab below
    static_assert_no_msg(VPTR_OFFS == 0);
    GenTree* vtab = gtNewIndir(TYP_I_IMPL, thisPtr);
    vtab->gtFlags |= GTF_IND_INVARIANT;

    GenTree* result;

    // Get the appropriate vtable chunk
    if (vtabOffsOfIndirection != CORINFO_VIRTUALCALL_NO_CHUNK)
    {
        // Note this isRelative code path is currently never executed
        // as the VM doesn't ever return:  isRelative == true
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
            LclVarDsc* varNum1 = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("var1 - vtab"));
            LclVarDsc* varNum2 = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("var2 - relative"));
            GenTree*   asgVar1 = gtNewAssignNode(gtNewLclvNode(varNum1, TYP_I_IMPL), vtab);

            // [tmp + vtabOffsOfIndirection]
            GenTree* tmpTree1 = gtNewOperNode(GT_ADD, TYP_I_IMPL, gtNewLclvNode(varNum1, TYP_I_IMPL),
                                              gtNewIconNode(vtabOffsOfIndirection, TYP_I_IMPL));
            tmpTree1 = gtNewIndir(TYP_I_IMPL, tmpTree1);
            tmpTree1->gtFlags |= GTF_IND_NONFAULTING | GTF_IND_INVARIANT;

            // var1 + vtabOffsOfIndirection + vtabOffsAfterIndirection
            GenTree* tmpTree2 =
                gtNewOperNode(GT_ADD, TYP_I_IMPL, gtNewLclvNode(varNum1, TYP_I_IMPL),
                              gtNewIconNode(vtabOffsOfIndirection + vtabOffsAfterIndirection, TYP_I_IMPL));

            // var1 + vtabOffsOfIndirection + vtabOffsAfterIndirection + [var1 + vtabOffsOfIndirection]
            tmpTree2         = gtNewOperNode(GT_ADD, TYP_I_IMPL, tmpTree2, tmpTree1);
            GenTree* asgVar2 = gtNewAssignNode(gtNewLclvNode(varNum2, TYP_I_IMPL), tmpTree2);

            result = gtNewIndir(TYP_I_IMPL, gtNewLclvNode(varNum2, TYP_I_IMPL)); // [var2]
            // This last indirection is not invariant.
            result->gtFlags |= GTF_IND_NONFAULTING;

            result = gtNewOperNode(GT_ADD, TYP_I_IMPL, result, gtNewLclvNode(varNum2, TYP_I_IMPL)); // [var2] + var2
            result = gtNewCommaNode(asgVar1, gtNewCommaNode(asgVar2, result));
        }
        else
        {
            // result = [vtab + vtabOffsOfIndirection]
            result = gtNewOperNode(GT_ADD, TYP_I_IMPL, vtab, gtNewIconNode(vtabOffsOfIndirection, TYP_I_IMPL));
            result = gtNewIndir(TYP_I_IMPL, result);
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
        result = gtNewIndir(TYP_I_IMPL, result);
        // This last indirection is not invariant.
        result->gtFlags |= GTF_IND_NONFAULTING;
    }

    return result;
}

GenTree* Compiler::fgMorphStrCon(GenTreeStrCon* tree, Statement* stmt, BasicBlock* block)
{
    assert(fgGlobalMorph);

    // TODO-CQ: Do this for block->isRunRarely(). Doing that currently will
    // guarantee slow performance for that block. Instead cache the return value
    // of CORINFO_HELP_STRCNS and go to cache first giving reasonable perf.

    bool useLazyStrCns = false;
    if (block->bbJumpKind == BBJ_THROW)
    {
        useLazyStrCns = true;
    }
    else if (GenTreeCall* call = stmt->GetRootNode()->IsCall())
    {
        // Quick check: if the root node of the current statement happens to be a noreturn call.
        useLazyStrCns = call->IsNoReturn() || fgIsThrow(call);
    }

    if (useLazyStrCns)
    {
        CorInfoHelpFunc helper = info.compCompHnd->getLazyStringLiteralHelper(tree->GetModuleHandle());

        if (helper != CORINFO_HELP_UNDEF)
        {
            GenTree*          token = gtNewIconNode(RidFromToken(tree->GetToken()));
            GenTreeCall::Use* args;

            if (helper == CORINFO_HELP_STRCNS_CURRENT_MODULE)
            {
                args = gtNewCallArgs(token);
            }
            else
            {
                assert(helper == CORINFO_HELP_STRCNS);
                args = gtNewCallArgs(token, gtNewIconEmbModHndNode(tree->GetModuleHandle()));
            }

            return gtNewHelperCallNode(helper, TYP_REF, args);
        }
    }

    void*          addr;
    InfoAccessType iat = info.compCompHnd->constructStringLiteral(tree->GetModuleHandle(), tree->GetToken(), &addr);
    return gtNewStringLiteralNode(iat, addr);
}

GenTree* Compiler::fgMorphLeaf(GenTree* tree)
{
    assert(tree->OperIsLeaf());

    if (tree->OperIs(GT_LCL_VAR))
    {
        return fgMorphLclVar(tree->AsLclVar());
    }

    if (tree->OperIs(GT_LCL_FLD))
    {
        if (tree->AsLclFld()->GetLcl()->IsAddressExposed())
        {
            tree->gtFlags |= GTF_GLOB_REF;
        }

        return tree;
    }

    if (GenTreeMethodAddr* method = tree->IsMethodAddr())
    {
        CORINFO_CONST_LOOKUP entry;

#ifdef FEATURE_READYTORUN_COMPILER
        if (method->GetEntryPoint().addr != nullptr)
        {
            entry = method->GetEntryPoint();
        }
        else
#endif
        {
            info.compCompHnd->getFunctionFixedEntryPoint(method->GetMethodHandle(), &entry);
        }

        switch (entry.accessType)
        {
            case IAT_PPVALUE:
                DEBUG_DESTROY_NODE(tree);
                tree = gtNewIndOfIconHandleNode(TYP_I_IMPL, reinterpret_cast<size_t>(entry.addr), HandleKind::ConstData,
                                                true);
                tree = gtNewIndir(TYP_I_IMPL, tree);
                tree->gtFlags |= GTF_IND_NONFAULTING | GTF_IND_INVARIANT;
                return fgMorphTree(tree);
            case IAT_PVALUE:
                DEBUG_DESTROY_NODE(tree);
                tree = gtNewIndOfIconHandleNode(TYP_I_IMPL, reinterpret_cast<size_t>(entry.addr),
                                                HandleKind::MethodAddr, true);
                return fgMorphTree(tree);
            case IAT_VALUE:
                tree->ChangeToIntCon(reinterpret_cast<size_t>(entry.handle));
                tree->AsIntCon()->SetHandleKind(HandleKind::MethodAddr);
                return tree;
            default:
                unreached();
        }
    }

    return tree;
}

GenTree* Compiler::fgMorphInitStruct(GenTreeOp* asg)
{
    JITDUMPTREE(asg, "\nfgMorphInitStruct (before):\n");

    assert(asg->OperIs(GT_ASG));

    GenTree* dest = asg->GetOp(0);
    GenTree* src  = asg->GetOp(1);

    assert(varTypeIsStruct(dest->GetType()));
    assert(src->OperIs(GT_INIT_VAL) || src->IsIntegralConst(0));

    unsigned             destSize     = 0;
    GenTreeLclVarCommon* destLclNode  = nullptr;
    LclVarDsc*           destLcl      = nullptr;
    unsigned             destLclOffs  = 0;
    FieldSeqNode*        destFieldSeq = nullptr;

    if (dest->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        destLclNode = dest->AsLclVarCommon();
        destLcl     = destLclNode->GetLcl();

        if (dest->OperIs(GT_LCL_VAR))
        {
            if (destLclNode->TypeIs(TYP_STRUCT))
            {
                destSize = destLcl->GetLayout()->GetSize();
            }
            else
            {
                destSize = varTypeSize(destLcl->GetType());
            }
        }
        else
        {
            destSize =
                dest->TypeIs(TYP_STRUCT) ? dest->AsLclFld()->GetLayout(this)->GetSize() : varTypeSize(dest->GetType());
            destLclOffs  = dest->AsLclFld()->GetLclOffs();
            destFieldSeq = dest->AsLclFld()->GetFieldSeq();
        }

#if LOCAL_ASSERTION_PROP
        if (morphAssertionCount != 0)
        {
            morphAssertionKill(destLcl DEBUGARG(asg));
        }
#endif
    }
    else if (dest->OperIs(GT_IND))
    {
        assert(varTypeIsSIMD(dest->GetType()));

        destSize = varTypeSize(dest->GetType());
    }
    else
    {
        destSize = dest->AsObj()->GetLayout()->GetSize();
    }

    GenTree* initVal = src->OperIs(GT_INIT_VAL) ? src->AsUnOp()->GetOp(0) : src;

    if ((destLcl != nullptr) && (destSize != 0) && (destLcl->GetType() != TYP_BLK))
    {
        unsigned destLclSize = destLcl->GetTypeSize();

        if (destLcl->IsPromoted() && (destLclOffs == 0) && (destSize == destLclSize))
        {
            assert(varTypeIsStruct(destLcl->GetType()));

            GenTree* promotedTree = fgMorphPromoteLocalInitStruct(asg, destLcl, initVal);

            if (promotedTree != nullptr)
            {
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

            GenTreeFlags destFlags = GTF_EMPTY;

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

            if ((initType == TYP_UNDEF) && (initVal->IsIntegralConst(0) || (destLcl->GetType() != TYP_STRUCT)))
            {
                if ((destLclOffs == 0) && (destSize == destLclSize) && !varTypeIsFloating(destLcl->GetType())
#ifndef TARGET_64BIT
                    && !varTypeIsLong(destLcl->GetType())
#endif
                        )
                {
                    initType = destLcl->GetType();

                    if (varTypeIsSIMD(initType))
                    {
                        initBaseType = destLcl->GetLayout()->GetElementType();
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
                destLclNode->AsLclVarCommon()->SetLcl(destLcl);

                destFlags |= GTF_DONT_CSE | (destLcl->IsAddressExposed() ? GTF_GLOB_REF : GTF_EMPTY);

                if (destLclNode->OperIs(GT_LCL_FLD))
                {
                    lvaSetDoNotEnregister(destLcl DEBUGARG(DNER_LocalField));
                }

                destLclNode->gtFlags = destFlags;

                if (initType == TYP_STRUCT)
                {
                    lvaSetDoNotEnregister(destLcl DEBUGARG(DNER_BlockOp));
                }
                else
                {
                    initVal =
                        fgMorphInitStructConstant(initVal->AsIntCon(), initType,
                                                  destLclNode->OperIs(GT_LCL_VAR) && destLcl->lvNormalizeOnStore(),
                                                  initBaseType);
                }

                asg->SetType(initType);
                asg->SetOp(0, destLclNode);
                asg->SetOp(1, initVal);
                asg->gtFlags &= ~GTF_ALL_EFFECT;
                asg->gtFlags |= GTF_ASG | ((asg->GetOp(0)->gtFlags | asg->GetOp(1)->gtFlags) & GTF_ALL_EFFECT);

                if (destLclNode->OperIs(GT_LCL_VAR))
                {
                    asg->ChangeOper(GT_STORE_LCL_VAR);
                    asg->AsLclVar()->SetLcl(destLcl);
                    asg->AsLclVar()->SetOp(0, initVal);
                    asg->gtFlags |= destLclNode->gtFlags & GTF_SPECIFIC_MASK;
                }
                else
                {
                    asg->ChangeOper(GT_STORE_LCL_FLD);
                    asg->AsLclVar()->SetLcl(destLcl);
                    asg->AsLclFld()->SetLclOffs(destLclNode->AsLclFld()->GetLclOffs());
                    asg->AsLclFld()->SetFieldSeq(destLclNode->AsLclFld()->GetFieldSeq());
                    asg->AsLclFld()->SetLayoutNum(destLclNode->AsLclFld()->GetLayoutNum());
                    asg->AsLclFld()->SetOp(0, initVal);
                    asg->gtFlags |= destLclNode->gtFlags & GTF_SPECIFIC_MASK;
                }

                JITDUMPTREE(asg, "fgMorphInitStruct (after converting to scalar init):\n");

                return asg;
            }
        }
    }

    asg->gtFlags &= ~GTF_ALL_EFFECT;
    asg->gtFlags |= GTF_ASG | ((asg->GetOp(0)->gtFlags | asg->GetOp(1)->gtFlags) & GTF_ALL_EFFECT);

    if (destLcl != nullptr)
    {
        lvaSetDoNotEnregister(destLcl DEBUGARG(DNER_BlockOp));
    }

    asg->SetType(dest->GetType());

    if (dest->OperIs(GT_LCL_VAR))
    {
        asg->ChangeOper(GT_STORE_LCL_VAR);
        asg->AsLclVar()->SetLcl(dest->AsLclVar()->GetLcl());
        asg->AsLclVar()->SetOp(0, src);
        asg->gtFlags |= dest->gtFlags & GTF_SPECIFIC_MASK;
    }
    else if (dest->OperIs(GT_LCL_FLD))
    {
        asg->ChangeOper(GT_STORE_LCL_FLD);
        asg->AsLclFld()->SetLcl(dest->AsLclFld()->GetLcl());
        asg->AsLclFld()->SetLclOffs(dest->AsLclFld()->GetLclOffs());
        asg->AsLclFld()->SetFieldSeq(dest->AsLclFld()->GetFieldSeq());
        asg->AsLclFld()->SetLayoutNum(dest->AsLclFld()->GetLayoutNum());
        asg->AsLclFld()->SetOp(0, src);
        asg->gtFlags |= dest->gtFlags & GTF_SPECIFIC_MASK;
    }
    else
    {
        asg->ChangeOper(dest->OperIs(GT_OBJ) ? GT_STORE_OBJ : GT_STOREIND);
        asg->AsIndir()->SetAddr(dest->AsIndir()->GetAddr());
        asg->AsIndir()->SetValue(src);
        asg->gtFlags |= dest->gtFlags & GTF_SPECIFIC_MASK;

        if (dest->OperIs(GT_OBJ))
        {
            asg->AsObj()->SetLayout(dest->AsObj()->GetLayout());
        }
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
        double doublePattern;
        memcpy(&doublePattern, &initPattern, 8);
        initVal->ChangeOperConst(GT_CNS_DBL);
        initVal->SetType(TYP_DOUBLE);
        initVal->AsDblCon()->SetValue(doublePattern);
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
        if (varTypeSize(initPatternType) <= 4)
        {
            // Keep only as many bits as are needed to avoid creating "large" constants.
            initPattern &= (int64_t(1) << (varTypeSize(initPatternType) * 8)) - 1;

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

GenTree* Compiler::fgMorphPromoteLocalInitStruct(GenTreeOp* asg, LclVarDsc* destLcl, GenTree* initVal)
{
    assert(varTypeIsStruct(destLcl->GetType()));
    assert(destLcl->IsPromoted());

    if (destLcl->lvDoNotEnregister && (destLcl->GetPromotedFieldCount() > 1))
    {
        JITDUMP(" dest is already DNER and has more than one field.\n");
        return nullptr;
    }

    if (destLcl->IsAddressExposed() && destLcl->lvContainsHoles)
    {
        JITDUMP(" dest is address exposed and contains holes.\n");
        return nullptr;
    }

    if (destLcl->lvCustomLayout && destLcl->lvContainsHoles)
    {
        // TODO-1stClassStructs: there are no reasons for this pessimization, delete it.
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
        for (LclVarDsc* destFieldLcl : PromotedFields(destLcl))
        {
            if (varTypeIsGC(destFieldLcl->GetType()))
            {
                JITDUMP(" dest contains GC and fields and source constant is not 0.\n");
                return nullptr;
            }
        }
    }

    JITDUMP(" using field by field initialization.\n");

    const unsigned fieldCount = destLcl->GetPromotedFieldCount();
    GenTree*       fieldStores[StructPromotionHelper::GetMaxFieldCount()];

    for (unsigned i = 0; i < fieldCount; ++i)
    {
        LclVarDsc* destFieldLcl = lvaGetDesc(destLcl->GetPromotedFieldLclNum(i));

        var_types type     = destFieldLcl->GetType();
        var_types baseType = varTypeIsSIMD(type) ? destFieldLcl->GetLayout()->GetElementType() : TYP_UNDEF;

        GenTree* value = fgMorphInitStructConstant(gtNewIconNode(initVal->AsIntCon()->GetValue()), type,
                                                   destFieldLcl->lvNormalizeOnStore(), baseType);

        fieldStores[i] = gtNewStoreLclVar(destFieldLcl, type, value);

        if (destFieldLcl->IsAddressExposed())
        {
            fieldStores[i]->AddSideEffects(GTF_GLOB_REF);
        }
    }

    return fgMorphPromoteStore(asg, nullptr, fieldStores, destLcl->GetPromotedFieldCount());
}

GenTree* Compiler::fgMorphStructComma(GenTree* tree)
{
    assert(tree->OperIs(GT_COMMA) && varTypeIsStruct(tree->GetType()));

    // Change COMMA(..., indir(addr)) into indir(COMMA(..., addr)).
    //
    // Only do this for indirs, STRUCT COMMA(..., LCL_VAR) may appear during CSE and
    // we can't take local's address because that would make it addr-exposed.
    // This transform is done to simplify indir promotion, when we need the indir
    // address anyway. But the temps created by CSE are not promoted so we can keep
    // the COMMA as is. And if they were promoted, the correct way to promote in this
    // case would be to extract the COMMA side effects and reattach them to the first
    // promoted field assignment.

    // TODO-MIKE-Review: Should this be blocked entirely post global morph?
    // No promotion should be needed after global morph and this transform
    // does NOT update COMMA value numbers.

    ArrayStack<GenTreeOp*> commas(getAllocator(CMK_ArrayStack));
    for (GenTree* comma = tree; comma->OperIs(GT_COMMA); comma = comma->AsOp()->GetOp(1))
    {
        assert(comma->GetType() == tree->GetType());
        commas.Push(comma->AsOp());
    }

    GenTree* effectiveVal = commas.Top()->GetOp(1);

    if (!effectiveVal->IsIndir())
    {
        return tree;
    }

    GenTree* effectiveValAddr = effectiveVal->AsIndir()->GetAddr();
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
        indir = gtNewIndir(effectiveVal->GetType(), tree);
    }
    else
    {
        indir = gtNewObjNode(effectiveVal->AsObj()->GetLayout(), tree);
    }

    indir->SetSideEffects(sideEffects);
    INDEBUG(indir->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)
    return indir;
}

GenTree* Compiler::fgMorphStructAssignment(GenTreeOp* asg)
{
    assert(asg->OperIs(GT_ASG));
    assert(varTypeIsStruct(asg->GetOp(0)->GetType()));

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

GenTree* Compiler::fgMorphPromoteSimdAssignmentSrc(GenTreeOp* asg, LclVarDsc* srcLcl)
{
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
            ops[i] = gtNewLclvNode(lvaGetDesc(srcLcl->GetPromotedFieldLclNum(i)), TYP_FLOAT);
        }
        else
        {
            ops[i] = gtNewDconNode(0, TYP_FLOAT);
        }
    }

    GenTree* dst = asg->GetOp(0);
    GenTree* src = gtNewSimdHWIntrinsicNode(dstType, create, TYP_FLOAT, numOps * 4, numOps, ops);

    if (dst->OperIs(GT_LCL_VAR))
    {
        asg->ChangeOper(GT_STORE_LCL_VAR);
        asg->AsLclVar()->SetOp(0, src);
        asg->AsLclVar()->SetLcl(dst->AsLclVar()->GetLcl());
    }
    else if (dst->OperIs(GT_LCL_FLD))
    {
        asg->ChangeOper(GT_STORE_LCL_FLD);
        asg->AsLclFld()->SetOp(0, src);
        asg->AsLclFld()->SetLcl(dst->AsLclFld()->GetLcl());
        asg->AsLclFld()->SetLclOffs(dst->AsLclFld()->GetLclOffs());
        asg->AsLclFld()->SetFieldSeq(dst->AsLclFld()->GetFieldSeq());
        asg->AsLclFld()->SetLayoutNum(dst->AsLclFld()->GetLayoutNum());
    }
    else
    {
        asg->ChangeOper(GT_STOREIND);
        asg->AsIndir()->SetAddr(dst->AsIndir()->GetAddr());
        asg->AsIndir()->SetValue(src);
    }

    asg->gtFlags |= dst->gtFlags & GTF_SPECIFIC_MASK;

    JITDUMPTREE(asg, "fgMorphCopyStruct (after SIMD source promotion):\n\n");

    return asg;
}

GenTree* Compiler::fgMorphPromoteSimdAssignmentDst(GenTreeOp* asg, LclVarDsc* dstLcl)
{
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
                        LclVarDsc* lcl = op->AsLclVar()->GetLcl();

                        if (lcl->IsPromotedField() && (lcl->GetPromotedFieldParentLclNum() == dstLcl->GetLclNum()))
                        {
                            srcIsCreate = false;
                        }
                    }
                    else if (!op->IsDblCon())
                    {
                        // TODO-MIKE-CQ: This is overly conservative, we need to check if the op tree contains
                        // any references to the destination local, including its promoted fields. Basically
                        // something like impHasLclRef but that also checks for promoted fields.
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

    GenTree* tempStore = nullptr;

    if (!srcIsZero && !srcIsCreate && !src->OperIs(GT_LCL_VAR))
    {
        LclVarDsc* tmpLcl = lvaNewTemp(src, true DEBUGARG("promoted SIMD copy temp"));

        tempStore = gtNewStoreLclVar(tmpLcl, src->GetType(), src);
        src       = gtNewLclvNode(tmpLcl, src->GetType());
    }

    GenTree*       fieldStores[StructPromotionHelper::GetMaxFieldCount()];
    const unsigned fieldCount = dstLcl->GetPromotedFieldCount();

    for (unsigned i = 0; i < fieldCount; i++)
    {
        unsigned   fieldIndex = dstLcl->GetPromotedFieldCount() - 1 - i;
        LclVarDsc* fieldLcl   = lvaGetDesc(dstLcl->GetPromotedFieldLclNum(fieldIndex));

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
            src = gtNewLclvNode(src->AsLclVar()->GetLcl(), src->GetType());
            // TODO-MIKE-Cleanup: If the source is DNER we should create a LCL_FLD instead.
            // Or maybe copy it to a temp to avoid generating multiple loads, that would
            // work well when we have extractps, but if we don't know how the value was
            // stored to memory in the first place we risk blocking store forwarding.
            src->AddSideEffects(src->AsLclVar()->GetLcl()->IsAddressExposed() ? GTF_GLOB_REF : GTF_NONE);
            fieldSrc = gtNewSimdGetElementNode(src->GetType(), TYP_FLOAT, src, gtNewIconNode(fieldIndex));
        }

        fieldStores[i] = gtNewStoreLclVar(fieldLcl, TYP_FLOAT, fieldSrc);
    }

    return fgMorphPromoteStore(asg, tempStore, fieldStores, fieldCount);
}

#endif // FEATURE_SIMD

GenTree* Compiler::fgMorphDynBlk(GenTreeDynBlk* dynBlk)
{
    assert(dynBlk->TypeIs(TYP_VOID));

    GenTreeIntCon* constSize = dynBlk->GetSize()->IsIntCon();

    if (constSize == nullptr)
    {
        return dynBlk;
    }

    if (constSize->GetUInt32Value() == 0)
    {
        GenTree* nop = gtNewNothingNode();
        INDEBUG(nop->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
        return nop;
    }

    ClassLayout* layout  = typGetBlkLayout(constSize->GetUInt32Value());
    GenTree*     dstAddr = dynBlk->GetAddr();
    GenTree*     src     = dynBlk->GetValue();

    if (dynBlk->OperIs(GT_COPY_BLK))
    {
        src = new (this, GT_BLK) GenTreeBlk(src, layout);
        src->AddSideEffects(GTF_GLOB_REF | GTF_EXCEPT);

        if (dynBlk->IsVolatile())
        {
            src->AsBlk()->SetVolatile();
        }
    }
    else if (!src->IsIntegralConst(0))
    {
        src = gtNewOperNode(GT_INIT_VAL, TYP_INT, src);
    }

    dynBlk->ChangeOper(GT_STORE_BLK);

    GenTreeBlk* store = dynBlk->AsBlk();
    store->SetType(TYP_STRUCT);
    store->SetLayout(layout);
    store->SetKind(StructStoreKind::Invalid);
    store->SetAddr(dstAddr);
    store->SetValue(src);

    return dynBlk;
}

GenTree* Compiler::fgMorphBlockAssignment(GenTreeOp* asg)
{
    assert(asg->OperIs(GT_ASG) && asg->TypeIs(TYP_STRUCT));

    GenTreeBlk* dst = asg->GetOp(0)->AsBlk();
    GenTree*    src = asg->GetOp(1);

    assert(dst->GetLayout()->IsBlockLayout());
    assert(dst->GetLayout()->GetSize() != 0);

    if (src->OperIs(GT_BLK))
    {
        assert(src->AsBlk()->GetLayout()->GetSize() == dst->GetLayout()->GetSize());
    }
    else
    {
        assert(src->OperIs(GT_INIT_VAL) || src->IsIntegralConst(0));
    }

    asg->ChangeOper(GT_STORE_BLK);

    GenTreeBlk* store = asg->AsBlk();

    store->SetAddr(dst->GetAddr());
    store->SetValue(src);
    store->SetLayout(dst->GetLayout());
    store->gtFlags |= dst->gtFlags & GTF_SPECIFIC_MASK;

    return store;
}

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
            if (dest->OperIs(GT_LCL_VAR))
            {
                asg->ChangeOper(GT_STORE_LCL_VAR);
                asg->AsLclVar()->SetLcl(dest->AsLclVar()->GetLcl());
                asg->AsLclVar()->SetOp(0, src);
                asg->gtFlags |= dest->gtFlags & GTF_SPECIFIC_MASK;

                LclVarDsc* lcl = dest->AsLclVar()->GetLcl();

                // TODO-MIKE-Cleanup: This isn't quite right, lvIsMultiRegRet should be set before promoting.
                // The problem is that the importer doesn't set it if the local is indirectly accessed (via
                // an OBJ). LocalAddressVisitor then eliminates the OBJ and avoids dependent promotion but
                // lvIsMultiRegRet isn't set and that breaks SSA. Setting lvIsMultiRegRet allows things to
                // work correctly but we may still end up with dependent promotion instead of not promoting
                // to beging with.

                if (lcl->IsIndependentPromoted())
                {
                    lcl->lvIsMultiRegRet = true;
                }
            }
            else if (dest->OperIs(GT_LCL_FLD))
            {
                asg->ChangeOper(GT_STORE_LCL_FLD);
                asg->AsLclFld()->SetLcl(dest->AsLclFld()->GetLcl());
                asg->AsLclFld()->SetLclOffs(dest->AsLclFld()->GetLclOffs());
                asg->AsLclFld()->SetFieldSeq(dest->AsLclFld()->GetFieldSeq());
                asg->AsLclFld()->SetLayoutNum(dest->AsLclFld()->GetLayoutNum());
                asg->AsLclFld()->SetOp(0, src);
                asg->gtFlags |= dest->gtFlags & GTF_SPECIFIC_MASK;
            }
            else
            {
                asg->ChangeOper(dest->OperIs(GT_OBJ) ? GT_STORE_OBJ : GT_STOREIND);
                asg->AsIndir()->SetAddr(dest->AsIndir()->GetAddr());
                asg->AsIndir()->SetValue(src);
                asg->gtFlags |= dest->gtFlags & GTF_SPECIFIC_MASK;

                if (dest->OperIs(GT_OBJ))
                {
                    asg->AsObj()->SetLayout(dest->AsObj()->GetLayout());
                }
            }

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

    if (src->OperIs(GT_COMMA))
    {
        src = fgMorphStructComma(src);
        asg->SetOp(1, src);
    }

    JITDUMPTREE(asg, "fgMorphCopyStruct: (after fgMorphStructComma)\n");

    GenTreeLclVarCommon* destLclNode = nullptr;
    LclVarDsc*           destLcl     = nullptr;
    unsigned             destLclOffs = 0;
    bool                 destPromote = false;

    if (dest->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        destLclNode = dest->AsLclVarCommon();
        destLclOffs = destLclNode->GetLclOffs();
        destLcl     = destLclNode->GetLcl();

#if LOCAL_ASSERTION_PROP
        if (morphAssertionCount != 0)
        {
            morphAssertionKill(destLcl DEBUGARG(asg));
        }
#endif
    }
    else
    {
        assert(dest->OperIs(GT_OBJ) || (dest->OperIs(GT_IND) && varTypeIsSIMD(dest->GetType())));
    }

    LclVarDsc* srcLcl     = nullptr;
    unsigned   srcLclOffs = 0;
    bool       srcPromote = false;

    if (src->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        srcLclOffs = src->AsLclVarCommon()->GetLclOffs();
        srcLcl     = src->AsLclVarCommon()->GetLcl();
    }
    else if (GenTreeLclUse* use = src->IsLclUse())
    {
        srcLcl = use->GetDef()->GetLcl();
    }
    else if (src->IsExtract())
    {
        // TODO-MIKE-SSA: Figure out what to do with this, do we need to treat it as a local?
    }
    else if (src->OperIs(GT_COMMA))
    {
        // During CSE we may see COMMA(..., LCL_VAR) but neither the CSE temp
        // nor the assignment destination are expected to be promoted so we
        // don't need to do anything, we'll just keep the struct copy as is.
        assert(src->SkipComma()->IsLclUse() || !src->SkipComma()->AsLclVar()->GetLcl()->IsPromoted());
        assert((destLcl == nullptr) || !destLcl->IsIndependentPromoted());
    }
    else if (!src->OperIs(GT_OBJ))
    {
        // For SIMD copies the source can be any SIMD typed tree or a CALL.
        assert(src->OperIs(GT_CALL) || varTypeIsSIMD(src->GetType()));
    }

    // Check to see if we are doing a copy to/from the same local block.
    // If so, morph it to a nop.
    if ((destLcl != nullptr) && (srcLcl == destLcl) && (destLclOffs == srcLclOffs))
    {
        JITDUMP("Self-copy; replaced with a NOP.\n");
        GenTree* nop = gtNewNothingNode();
        INDEBUG(nop->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
        return nop;
    }

    unsigned destSize;

    if (!dest->TypeIs(TYP_STRUCT))
    {
        destSize = varTypeSize(dest->GetType());
    }
    else if (dest->OperIs(GT_LCL_VAR))
    {
        destSize = destLcl->GetLayout()->GetSize();
    }
    else if (dest->OperIs(GT_LCL_FLD))
    {
        destSize = dest->AsLclFld()->GetLayout(this)->GetSize();
    }
    else
    {
        destSize = dest->AsObj()->GetLayout()->GetSize();
    }

    if ((destLcl != nullptr) && destLcl->IsPromoted() && (destLclOffs == 0) && (destLcl->GetTypeSize() == destSize) &&
        (!destLcl->lvDoNotEnregister || (destLcl->GetPromotedFieldCount() == 1)))
    {
        assert(varTypeIsStruct(destLcl->GetType()));

        destPromote = true;

        JITDUMP("dest is promoted local\n");
    }

    if ((srcLcl != nullptr) && srcLcl->IsPromoted() && (srcLclOffs == 0) && (srcLcl->GetTypeSize() == destSize) &&
        (!srcLcl->lvDoNotEnregister || (srcLcl->GetPromotedFieldCount() == 1)))
    {
        assert(varTypeIsStruct(srcLcl->GetType()));

        srcPromote = true;

        JITDUMP("src is promoted local\n");
    }

#ifdef FEATURE_SIMD
    if (!destPromote && srcPromote && varTypeIsSIMD(srcLcl->GetType()) && dest->OperIs(GT_LCL_VAR))
    {
        return fgMorphPromoteSimdAssignmentSrc(asg, srcLcl);
    }

    if (destPromote && !srcPromote && varTypeIsSIMD(destLcl->GetType()) &&
        src->OperIs(GT_LCL_VAR, GT_BITCAST, GT_HWINTRINSIC))
    {
        return fgMorphPromoteSimdAssignmentDst(asg, destLcl);
    }
#endif // FEATURE_SIMD

    bool promote = true;

    if (!destPromote && !srcPromote)
    {
        promote = false;
    }
    else if (destPromote && destLcl->lvCustomLayout && destLcl->lvContainsHoles)
    {
        JITDUMP("dest has custom layout and contains holes\n");
        promote = false;
    }
    else if (srcPromote && srcLcl->lvCustomLayout && srcLcl->lvContainsHoles)
    {
        JITDUMP("src has custom layout and contains holes\n");
        promote = false;
    }
    else if (src->OperIs(GT_CALL))
    {
        JITDUMP("src is a call\n");
        promote = false;
    }
    else if (src->OperIsHWIntrinsic())
    {
        JITDUMP("src is a HWINTRINSIC node\n");
        promote = false;
    }
#if defined(TARGET_ARM)
    else if (src->IsIndir() && src->AsIndir()->IsUnaligned())
    {
        JITDUMP("src is unaligned\n");
        promote = false;
    }
    else if (dest->IsIndir() && dest->AsIndir()->IsUnaligned())
    {
        JITDUMP("dest is unaligned\n");
        promote = false;
    }
#endif // TARGET_ARM
    else if (destPromote && srcPromote)
    {
        // Both structs should be of the same type, or each have the same number of fields, each having
        // the same type and offset. Actually, the destination could have less fields than the source
        // but there doesn't appear to be any such case in the entire FX. Copies between variables of
        // different types but same layout do occur though - Memory's implicit operator ReadOnlyMemory
        // uses Unsafe.As to perform the conversion, instead of copying the struct field by field.
        if (destLcl->GetLayout() != srcLcl->GetLayout())
        {
            bool sameLayout = destLcl->GetPromotedFieldCount() == srcLcl->GetPromotedFieldCount();

            for (unsigned i = 0; sameLayout && i < destLcl->GetPromotedFieldCount(); i++)
            {
                LclVarDsc* destFieldLclVar = lvaGetDesc(destLcl->GetPromotedFieldLclNum(i));
                LclVarDsc* srcFieldLclVar  = lvaGetDesc(srcLcl->GetPromotedFieldLclNum(i));

                assert(destFieldLclVar->GetType() != TYP_STRUCT);

                if ((destFieldLclVar->GetPromotedFieldOffset() != srcFieldLclVar->GetPromotedFieldOffset()) ||
                    (destFieldLclVar->GetType() != srcFieldLclVar->GetType()))
                {
                    sameLayout = false;
                }
            }

            if (!sameLayout)
            {
                promote = false;
                JITDUMP("dest and src have different layout\n");
            }
        }
    }

    if (!promote)
    {
        if (asg->TypeIs(TYP_STRUCT))
        {
            if (srcLcl != nullptr)
            {
                lvaSetDoNotEnregister(srcLcl DEBUGARG(DNER_BlockOp));
            }

            if (destLcl != nullptr)
            {
                lvaSetDoNotEnregister(destLcl DEBUGARG(DNER_BlockOp));
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

        asg->SetType(dest->GetType());

        if (dest->OperIs(GT_LCL_VAR))
        {
            asg->ChangeOper(GT_STORE_LCL_VAR);
            asg->AsLclVar()->SetLcl(dest->AsLclVar()->GetLcl());
            asg->AsLclVar()->SetOp(0, src);
            asg->gtFlags |= dest->gtFlags & GTF_SPECIFIC_MASK;
        }
        else if (dest->OperIs(GT_LCL_FLD))
        {
            asg->ChangeOper(GT_STORE_LCL_FLD);
            asg->AsLclFld()->SetLcl(dest->AsLclFld()->GetLcl());
            asg->AsLclFld()->SetLclOffs(dest->AsLclFld()->GetLclOffs());
            asg->AsLclFld()->SetFieldSeq(dest->AsLclFld()->GetFieldSeq());
            asg->AsLclFld()->SetLayoutNum(dest->AsLclFld()->GetLayoutNum());
            asg->AsLclFld()->SetOp(0, src);
            asg->gtFlags |= dest->gtFlags & GTF_SPECIFIC_MASK;
        }
        else
        {
            asg->ChangeOper(dest->OperIs(GT_OBJ) ? GT_STORE_OBJ : GT_STOREIND);
            asg->AsIndir()->SetAddr(dest->AsIndir()->GetAddr());
            asg->AsIndir()->SetValue(src);
            asg->gtFlags |= dest->gtFlags & GTF_SPECIFIC_MASK;

            if (dest->OperIs(GT_OBJ))
            {
                asg->AsObj()->SetLayout(dest->AsObj()->GetLayout());
            }
        }

        JITDUMPTREE(asg, "fgMorphCopyStruct: (after)\n");

        return asg;
    }

    assert(destPromote || srcPromote);
    assert(!destPromote || (destLcl != nullptr));
    assert(!srcPromote || (srcLcl != nullptr));
    assert(!destPromote || !srcPromote || (destLcl->GetPromotedFieldCount() == srcLcl->GetPromotedFieldCount()));

    auto PromoteLocal = [this](GenTree* fields[], LclVarDsc* lcl) {
        for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); i++)
        {
            LclVarDsc* fieldLcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

            GenTree* field = gtNewLclvNode(fieldLcl, fieldLcl->GetType());
            field->gtFlags |= fieldLcl->IsAddressExposed() ? GTF_GLOB_REF : GTF_EMPTY;
            fields[i] = field;
        }
    };

    auto SplitLocal = [this](GenTree* fields[], GenTreeLclVarCommon* lclNode, LclVarDsc* promotedLcl) {
        LclVarDsc* lcl     = lclNode->GetLcl();
        unsigned   lclOffs = lclNode->GetLclOffs();

        for (unsigned i = 0; i < promotedLcl->GetPromotedFieldCount(); i++)
        {
            LclVarDsc* promotedFieldLcl = lvaGetDesc(promotedLcl->GetPromotedFieldLclNum(i));

            GenTreeLclFld* field =
                gtNewLclFldNode(lcl, promotedFieldLcl->GetType(), lclOffs + promotedFieldLcl->GetPromotedFieldOffset());
            field->gtFlags |= lcl->IsAddressExposed() ? GTF_GLOB_REF : GTF_EMPTY;

            // We don't have a field sequence for the destination field but one can be obtained from
            // the source field if the destination and source have the same type. Of course, other
            // cases could be handled by querying the VM for destination fields and trying to find
            // ones that are suitable for the current offset and type but this should be a rare case.

            if (lclNode->OperIs(GT_LCL_VAR))
            {
                if (lcl->GetLayout() == promotedLcl->GetLayout())
                {
                    field->SetFieldSeq(promotedFieldLcl->GetPromotedFieldSeq());
                }
            }
            else if (GenTreeLclFld* lclFldNode = lclNode->IsLclFld())
            {
                FieldSeqNode* fieldSeq = lclFldNode->GetFieldSeq();

                if ((lclFldNode->GetLayout(this) == promotedLcl->GetLayout()) && (fieldSeq != nullptr) &&
                    fieldSeq->IsField())
                {
                    fieldSeq = GetFieldSeqStore()->Append(fieldSeq, promotedFieldLcl->GetPromotedFieldSeq());
                    field->SetFieldSeq(fieldSeq);
                }
            }

            fields[i] = field;
        }

        lvaSetDoNotEnregister(lcl DEBUGARG(DNER_LocalField));
    };

    auto SplitIndir = [this](GenTree* fields[], GenTreeIndir* indir, LclVarDsc* promotedLcl,
                             bool isPromotedLclStore) -> GenTree* {
        GenTree*      addr         = indir->GetAddr();
        LclVarDsc*    addrSpillLcl = nullptr;
        unsigned      addrOffset   = 0;
        FieldSeqNode* addrFieldSeq = FieldSeqNode::NotAField();
        GenTree*      addrAssign   = nullptr;

        if (promotedLcl->GetPromotedFieldCount() > 1)
        {
            // TODO-MIKE-CQ: This doesn't handle the array case. The array data offset (or the entire
            // element offset if the index is constant) is hidden under a COMMA and another ADD. We
            // could try to extract the offset from that tree but then vnIsArrayElemAddr will have
            // a difficult time recovering array element information.

            if (addr->OperIs(GT_ADD) && !addr->gtOverflow())
            {
                if (GenTreeIntCon* offset = addr->AsOp()->GetOp(1)->IsIntCon())
                {
#ifdef TARGET_XARCH
                    ssize_t maxOffset = INT32_MAX;
#else
                    ssize_t maxOffset = 4095;
#endif
                    // The maximum offset depends on the last promoted field offset, not the struct
                    // size. But such cases are so rare that it's not worth the trouble to get the
                    // last field offset.
                    maxOffset -= promotedLcl->GetTypeSize();

                    if ((offset->GetValue() > 0) && (offset->GetValue() <= maxOffset))
                    {
                        addrOffset   = offset->GetUInt32Value();
                        addrFieldSeq = offset->GetFieldSeq();
                        addr         = addr->AsOp()->GetOp(0);

                        if (!indir->IsObj() || (indir->AsObj()->GetLayout() != promotedLcl->GetLayout()))
                        {
                            addrFieldSeq = FieldSeqNode::NotAField();
                        }
                    }
                }
            }

            if (addr->OperIs(GT_LCL_VAR))
            {
                LclVarDsc* addrLcl = addr->AsLclVar()->GetLcl();

                bool isMemoryLoadOrAliased = addrLcl->lvDoNotEnregister || addrLcl->IsAddressExposed();
                bool isStoredPromotedField = isPromotedLclStore && addrLcl->IsPromotedField() &&
                                             (addrLcl->GetPromotedFieldParentLclNum() == promotedLcl->GetLclNum());

                if (!isMemoryLoadOrAliased && !isStoredPromotedField)
                {
                    assert(addr->GetSideEffects() == 0);

                    addrSpillLcl = addr->AsLclVar()->GetLcl();
                }
            }

            if (addrSpillLcl == nullptr)
            {
                addrSpillLcl = lvaNewTemp(addr->GetType(), true DEBUGARG("promoted struct address"));
                addrAssign   = gtNewStoreLclVar(addrSpillLcl, addr->GetType(), addr);
                addr         = gtNewLclvNode(addrSpillLcl, addr->GetType());
            }
        }

        for (unsigned i = 0; i < promotedLcl->GetPromotedFieldCount(); i++)
        {
            LclVarDsc* promotedFieldLcl = lvaGetDesc(promotedLcl->GetPromotedFieldLclNum(i));

            unsigned fieldOffset = promotedFieldLcl->GetPromotedFieldOffset();
            // TODO-MIKE-Review: This looks fishy - it's only correct if the destination has the same type as the
            // source. If reinterpretation has occurred then it would likely be wiser to use NotAField.
            FieldSeqNode* fieldSeq = promotedFieldLcl->GetPromotedFieldSeq();

            if (addrOffset != 0)
            {
                fieldOffset += addrOffset;
                fieldSeq = GetFieldSeqStore()->Append(addrFieldSeq, fieldSeq);
            }

            GenTree* fieldAddr = (i == 0) ? addr : gtNewLclvNode(addrSpillLcl, addr->GetType());

            if (fieldOffset == 0)
            {
                AddZeroOffsetFieldSeq(fieldAddr, fieldSeq);
            }
            else
            {
                fieldAddr = gtNewOperNode(GT_ADD, TYP_BYREF, fieldAddr, gtNewIconNode(fieldOffset, fieldSeq));
            }

            // TODO-MIKE-Fix: This ignores volatile. It's not clear how volatile should be handled,
            // block promotion, transfer volatile to all field indirs, transfer volatile only to the
            // first/last field indir? Luckily volatile is unusual on struct indirs.

            GenTree* field = gtNewIndir(promotedFieldLcl->GetType(), fieldAddr);
            field->gtFlags |=
                (indir->gtFlags & (GTF_GLOB_REF | GTF_IND_NONFAULTING | GTF_IND_TGT_NOT_HEAP | GTF_IND_TGT_HEAP));

            if ((indir->gtFlags & GTF_IND_NONFAULTING) == 0)
            {
                field->gtFlags |= GTF_EXCEPT;
            }

            fields[i] = field;
        }

        return addrAssign;
    };

    unsigned fieldCount = 0;
    GenTree* srcFields[StructPromotionHelper::GetMaxFieldCount()];
    GenTree* destFields[StructPromotionHelper::GetMaxFieldCount()];

    if (destPromote)
    {
        fieldCount = destLcl->GetPromotedFieldCount();
        PromoteLocal(destFields, destLcl);
    }

    if (srcPromote)
    {
        fieldCount = srcLcl->GetPromotedFieldCount();
        PromoteLocal(srcFields, srcLcl);
    }

    GenTree* tempStore = nullptr;

    if (!destPromote || !srcPromote)
    {
        LclVarDsc* promotedLcl;
        GenTree*   splitNode;
        GenTree**  splitNodeFields;

        if (destPromote)
        {
            promotedLcl     = destLcl;
            splitNode       = src;
            splitNodeFields = srcFields;
        }
        else
        {
            promotedLcl     = srcLcl;
            splitNode       = dest;
            splitNodeFields = destFields;
        }

        if (splitNode->OperIs(GT_LCL_VAR, GT_LCL_FLD))
        {
            SplitLocal(splitNodeFields, splitNode->AsLclVarCommon(), promotedLcl);
        }
        else
        {
            tempStore = SplitIndir(splitNodeFields, splitNode->AsIndir(), promotedLcl, destPromote);
        }
    }

    GenTree* fieldStores[StructPromotionHelper::GetMaxFieldCount()];

    for (unsigned i = 0; i < fieldCount; ++i)
    {
        GenTree* destField = destFields[i];
        GenTree* srcField  = srcFields[i];

        assert(destField->GetType() == srcField->GetType());

        if (destField->OperIs(GT_LCL_VAR))
        {
            destField->gtOper = GT_STORE_LCL_VAR;
            destField->gtFlags |= GTF_ASG;
            destField->AsLclVar()->SetOp(0, srcField);
        }
        else if (destField->OperIs(GT_LCL_FLD))
        {
            destField->gtOper = GT_STORE_LCL_FLD;
            destField->gtFlags |= GTF_ASG;
            destField->AsLclFld()->SetOp(0, srcField);
        }
        else
        {
            destField->gtOper = GT_STOREIND;
            destField->gtFlags |= GTF_ASG;
            destField->AsIndir()->SetValue(srcField);
        }

        destField->AddSideEffects(srcField->GetSideEffects());

        fieldStores[i] = destField;
    }

    return fgMorphPromoteStore(asg, tempStore, fieldStores, fieldCount);
}

GenTree* Compiler::fgMorphPromoteStore(GenTreeOp* store, GenTree* tempStore, GenTree** fieldStores, unsigned fieldCount)
{
    const bool isStmtRoot = (fgGlobalMorphStmt != nullptr) && (fgGlobalMorphStmt->GetRootNode() == store);
    GenTree*   tree       = tempStore;

    if (tree == nullptr)
    {
        tree = fieldStores[0];
        fieldStores++;
        fieldCount--;
    }

    for (unsigned i = 0; i < fieldCount; i++)
    {
        GenTree* fieldStore = fieldStores[i];
        assert(fieldStore->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD, GT_STOREIND));

        if (isStmtRoot)
        {
            Statement* stmt = gtNewStmt(tree, fgGlobalMorphStmt->GetILOffsetX());
            fgInsertStmtBefore(fgMorphBlock, fgGlobalMorphStmt, stmt);
            JITDUMPTREE(tree, "Promoted struct field store statement " FMT_STMT ":\n", stmt->GetID());

#if LOCAL_ASSERTION_PROP
            if (morphAssertionTable != nullptr)
            {
                if (tree->OperIs(GT_STORE_LCL_VAR))
                {
                    morphAssertionGenerate(tree);
                }
            }
#endif

            tree = fieldStore;
        }
        else
        {
#if LOCAL_ASSERTION_PROP
            if (morphAssertionTable != nullptr)
            {
                if (tree->OperIs(GT_STORE_LCL_VAR))
                {
                    morphAssertionGenerate(tree);
                }

                morphAssertionGenerate(fieldStore);
            }
#endif

            tree = gtNewCommaNode(tree, fieldStore);
        }
    }

    INDEBUG(tree->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED;)
    JITDUMPTREE(tree, "Promoted struct store:\n");

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
    assert(varTypeIsIntegralOrI(tree->GetType()));
    assert(tree->OperIs(GT_ADD, GT_MUL, GT_OR, GT_AND, GT_XOR));

    // op1 can be GT_COMMA, in this case we're going to fold
    // "(op (COMMA(... (op X C1))) C2)" to "(COMMA(... (op X C3)))"
    GenTree* op1 = tree->GetOp(0)->SkipComma();

    if ((op1->GetOper() != tree->GetOper()) || !tree->GetOp(1)->IsIntCon() || !op1->gtGetOp2()->IsIntCon() ||
        op1->gtGetOp1()->IsIntCon())
    {
        return nullptr;
    }

    if (!fgGlobalMorph && (op1 != tree->GetOp(0)))
    {
        // Since 'tree->gtGetOp1()' can have complex structure (e.g. COMMA(..(COMMA(..,op1)))
        // don't run the optimization for such trees outside of global morph.
        // Otherwise, there is a chance of violating VNs invariants and/or modifying a tree
        // that is an active CSE candidate.
        return nullptr;
    }

    if (tree->OperMayOverflow() && (tree->gtOverflow() || op1->gtOverflow()))
    {
        return nullptr;
    }

    GenTreeIntCon* cns1 = op1->gtGetOp2()->AsIntCon();
    GenTreeIntCon* cns2 = tree->GetOp(1)->AsIntCon();

    if (!varTypeIsIntegralOrI(tree->GetType()) || cns1->TypeIs(TYP_REF) || !cns1->TypeIs(cns2->GetType()))
    {
        return nullptr;
    }

    GenTree* folded = gtFoldExprConst(gtNewOperNode(tree->GetOper(), cns1->GetType(), cns1, cns2));

    if (!folded->IsIntCon())
    {
        // Give up if we can't fold "C1 op C2"
        return nullptr;
    }

    auto foldedCns = folded->AsIntCon();

    cns1->SetValue(foldedCns->GetValue());
    cns1->SetVNP(foldedCns->GetVNP());
    cns1->SetFieldSeq(foldedCns->GetFieldSeq());

    op1 = tree->gtGetOp1();
    op1->SetVNP(tree->GetVNP());

    DEBUG_DESTROY_NODE(tree);
    DEBUG_DESTROY_NODE(cns2);
    DEBUG_DESTROY_NODE(foldedCns);
    INDEBUG(cns1->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);

    return op1;
}

GenTree* Compiler::fgMorphNormalizeLclVarStore(GenTreeOp* asg)
{
    assert(asg->OperIs(GT_ASG));
    assert(fgGlobalMorph);

    GenTree* op1 = asg->GetOp(0);
    GenTree* op2 = asg->GetOp(1);

    if (varActualTypeIsInt(op1->GetType()))
    {
        LclVarDsc* lcl = op1->AsLclVar()->GetLcl();

        if (lcl->lvNormalizeOnStore())
        {
            op1->SetType(TYP_INT);

            if (gtIsSmallIntCastNeeded(op2, lcl->GetType()))
            {
                op2 = gtNewCastNode(op2, false, lcl->GetType());
                asg->SetOp(1, op2);
            }
        }
    }

    return op2;
}

GenTree* Compiler::fgMorphQmark(GenTreeQmark* qmark, MorphAddrContext* mac)
{
    ALLOCA_CHECK();
    assert(fgGlobalMorph);
    assert(!csePhase);

    GenTree* condExpr = qmark->GetCondition();
    GenTree* thenExpr = qmark->GetThen();
    GenTree* elseExpr = qmark->GetElse();

    if (condExpr->OperIsCompare())
    {
        condExpr->gtFlags |= GTF_RELOP_JMP_USED | GTF_DONT_CSE;
    }
    else
    {
        noway_assert(condExpr->gtEffectiveVal()->IsIntCon());
    }

    condExpr = fgMorphTree(condExpr, mac);
    qmark->SetCondition(condExpr);

    if (GenTreeIntCon* cond = condExpr->IsIntCon())
    {
        GenTree* result = cond->GetValue() != 0 ? thenExpr : elseExpr;

        return fgMorphTree(result);
    }

    if (fgIsCommaThrow(condExpr DEBUGARG(true)))
    {
        fgRemoveRestOfBlock = true;
        assert(condExpr->OperIs(GT_COMMA));

        if (varActualType(qmark->GetType()) == varActualType(condExpr->GetType()))
        {
            return condExpr;
        }

        if (qmark->TypeIs(TYP_VOID))
        {
            return condExpr->AsOp()->GetOp(0);
        }

        GenTree* value = condExpr->AsOp()->GetOp(1);

        if (varTypeIsFloating(qmark->GetType()))
        {
            value->ChangeOperConst(GT_CNS_DBL);
            value->AsDblCon()->SetValue(0.0);
        }
        else if (qmark->TypeIs(TYP_LONG))
        {
            value->ChangeOperConst(GT_CNS_NATIVELONG);
            value->AsIntConCommon()->SetLngValue(0);
        }
        else
        {
            assert(varTypeIsIntOrI(qmark->GetType()));

            value->ChangeOperConst(GT_CNS_INT);
            value->AsIntConCommon()->SetIconValue(0);
        }

        value->SetType(varActualType(qmark->GetType()));
        condExpr->SetType(value->GetType());

        return condExpr;
    }

    // If only one of the then/else expressions throws then the rest of the block is
    // still reachable. We have to ignore the setting of fgRemoveRestOfBlock during
    // then/else morphing. We could also handle the case of both then/else throwing
    // but that doesn't seem to ever happen currently.
    bool removeRestOfBlock = fgRemoveRestOfBlock;

#if LOCAL_ASSERTION_PROP
    // The local assertion propagation state after morphing the condition expression
    // applies to both then and else expressions, we need to save it before morphing
    // one expression and restore it before morphing the other expression.

    unsigned        entryAssertionCount = 0;
    MorphAssertion* entryAssertionTable = nullptr;

    if (morphAssertionCount != 0)
    {
        static_assert(morphAssertionMaxCount <= 64, "ALLOCA() may be bad idea");
        unsigned tableSize  = morphAssertionTableSize(morphAssertionCount);
        entryAssertionTable = (MorphAssertion*)ALLOCA(tableSize);
        entryAssertionCount = morphAssertionCount;
        morphAssertionGetTable(entryAssertionTable, entryAssertionCount);
    }
#endif // LOCAL_ASSERTION_PROP

    elseExpr = fgMorphTree(elseExpr, mac);
    qmark->SetElse(elseExpr);
    fgRemoveRestOfBlock = removeRestOfBlock;

#if LOCAL_ASSERTION_PROP
    unsigned        elseAssertionCount = 0;
    MorphAssertion* elseAssertionTable = nullptr;

    // We also need to save the local assertion propagation state after morphing the
    // first of the then/else expressions. Later we need to merge the two states by
    // removing assertions that are not present in both states.
    if (morphAssertionCount != 0)
    {
        static_assert(morphAssertionMaxCount <= 64, "ALLOCA() may be a bad idea");
        unsigned tableSize = morphAssertionTableSize(morphAssertionCount);
        elseAssertionTable = (MorphAssertion*)ALLOCA(tableSize);
        elseAssertionCount = morphAssertionCount;
        morphAssertionGetTable(elseAssertionTable, elseAssertionCount);
        morphAssertionSetCount(0);
    }

    if (entryAssertionCount != 0)
    {
        morphAssertionSetTable(entryAssertionTable, entryAssertionCount);
    }
#endif // LOCAL_ASSERTION_PROP

    thenExpr = fgMorphTree(thenExpr, mac);
    qmark->SetThen(thenExpr);
    fgRemoveRestOfBlock = removeRestOfBlock;

#if LOCAL_ASSERTION_PROP
    // Merge assertions after then/else morphing.
    if (morphAssertionCount != 0)
    {
        morphAssertionMerge(elseAssertionCount, elseAssertionTable DEBUGARG(qmark));
    }
#endif // LOCAL_ASSERTION_PROP

    qmark->SetSideEffects(condExpr->GetSideEffects() | thenExpr->GetSideEffects() | elseExpr->GetSideEffects());

    if (varTypeIsGC(qmark->GetType()) && !varTypeIsGC(elseExpr->GetType()) && !varTypeIsGC(thenExpr->GetType()))
    {
        qmark->SetType(varActualType(elseExpr->GetType()));
    }

    compQmarkUsed = true;

    return qmark;
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
    assert(tree->OperKind() & GTK_SMPOP);

    /* The steps in this function are :
       o Perform required preorder processing
       o Process the first, then second operand, if any
       o Perform required postorder morphing
       o Perform optional postorder morphing if optimizing
     */

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
            // TODO-MIKE-Review: This is probably useless now...
            op1->gtFlags |= GTF_DONT_CSE;

            // Ensure that the destination tree has all the necessary flags before it is morphed,
            // gtNewAssignNode should have set these flags but there may be bozo code that uses
            // gtNewOperNode, or SetOper and doesn't update the flags as needed.
            // We also need to add the small int local "normalization" cast so it is morphed too.
            if (op1->OperIs(GT_LCL_VAR, GT_LCL_FLD))
            {
                LclVarDsc* lcl = op1->AsLclVarCommon()->GetLcl();

                if (lcl->IsAddressExposed())
                {
                    tree->AddSideEffects(GTF_GLOB_REF);
                    op1->AddSideEffects(GTF_GLOB_REF);
                }

                if (fgGlobalMorph && op1->OperIs(GT_LCL_VAR))
                {
                    op2 = fgMorphNormalizeLclVarStore(tree->AsOp());
                }

                // Skip morphing op1 so we don't need to deal with a "def" local node.
                op1 = nullptr;
            }
            else
            {
                assert(op1->OperIs(GT_IND, GT_OBJ, GT_BLK));
            }
            break;

        case GT_JTRUE:

            noway_assert(op1);

            if (op1->OperIsCompare())
            {
                /* Mark the comparison node with GTF_RELOP_JMP_USED so it knows that it does
                   not need to materialize the result as a 0 or 1. */

                /* We also mark it as DONT_CSE, as we don't handle QMARKs with nonRELOP op1s */
                op1->gtFlags |= (GTF_RELOP_JMP_USED | GTF_DONT_CSE);
            }
            else
            {
                GenTree* effOp1 = op1->gtEffectiveVal();
                noway_assert((effOp1->gtOper == GT_CNS_INT) &&
                             (effOp1->IsIntegralConst(0) || effOp1->IsIntegralConst(1)));
            }
            break;

        case GT_STOREIND:
        case GT_STORE_OBJ:
            if (op1->OperIs(GT_LCL_ADDR) && !tree->AsIndir()->IsVolatile())
            {
                ClassLayout* layout = tree->IsObj() ? tree->AsObj()->GetLayout() : nullptr;

                // Just change it to a LCL_FLD. Since these locals are already address exposed
                // it's not worth the complication to figure out if the types match and change
                // to a LCL_VAR instead. Also don't bother with field sequences for the same
                // reason, VN doesn't do anything interesting for address exposed locals.

                tree->ChangeOper(GT_STORE_LCL_FLD);
                tree->AsLclFld()->SetLcl(op1->AsLclAddr()->GetLcl());
                tree->AsLclFld()->SetLclOffs(op1->AsLclAddr()->GetLclOffs());
                tree->AsLclFld()->SetLayout(layout, this);
                tree->AsLclFld()->SetOp(0, op2);
                tree->SetSideEffects(GTF_ASG | GTF_GLOB_REF | op2->GetSideEffects());
                tree->gtFlags &= ~GTF_REVERSE_OPS;

                oper = GT_STORE_LCL_FLD;
                op1  = op2;
                op2  = nullptr;
            }
            else if (((tree->gtFlags & GTF_IND_NONFAULTING) != 0) && !op1->HasAnySideEffect(GTF_EXCEPT) &&
                     !op2->HasAnySideEffect(GTF_EXCEPT))
            {
                tree->gtFlags &= ~GTF_EXCEPT;
            }
            break;

        case GT_IND:
        case GT_OBJ:
            if (op1->OperIs(GT_LCL_ADDR) && !tree->AsIndir()->IsVolatile())
            {
                ClassLayout* layout = tree->IsObj() ? tree->AsObj()->GetLayout() : nullptr;

                // Just change it to a LCL_FLD. Since these locals are already address exposed
                // it's not worth the complication to figure out if the types match and change
                // to a LCL_VAR instead. Also don't bother with field sequences for the same
                // reason, VN doesn't do anything interesting for address exposed locals.

                tree->ChangeOper(GT_LCL_FLD);
                tree->SetSideEffects(GTF_GLOB_REF);
                tree->AsLclFld()->SetLcl(op1->AsLclAddr()->GetLcl());
                tree->AsLclFld()->SetLclOffs(op1->AsLclAddr()->GetLclOffs());
                tree->AsLclFld()->SetLayout(layout, this);

                return tree;
            }

            if (GenTreeIndexAddr* index = op1->IsIndexAddr())
            {
                if ((typ == TYP_USHORT) && opts.OptimizationEnabled() && index->GetArray()->IsStrCon())
                {
                    GenTree* morphed = fgMorphStringIndexIndir(index, index->GetArray()->AsStrCon());

                    if (morphed != nullptr)
                    {
                        return morphed;
                    }
                }

                // TODO-MIKE-Cleanup: Ideally this should be done when the indir is created.
                tree->gtFlags |= GTF_IND_NONFAULTING;
            }

            if (((tree->gtFlags & GTF_IND_NONFAULTING) != 0) && ((op1->gtFlags & GTF_EXCEPT) == 0))
            {
                tree->gtFlags &= ~GTF_EXCEPT;
            }
            break;

        case GT_CAST:
            return fgMorphCast(tree->AsCast());

        case GT_MUL:
            noway_assert(op2 != nullptr);

            if (op1->IsIntConCommon() && !op2->IsIntConCommon())
            {
                std::swap(op1, op2);
                tree->AsOp()->SetOp(0, op1);
                tree->AsOp()->SetOp(1, op2);
            }

            if (opts.OptimizationEnabled() && !tree->gtOverflow())
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
                MulLongCandidateKind kind = fgMorphIsMulLongCandidate(tree->AsOp());

                if (kind != MulLongCandidateKind::None)
                {
                    return fgMorphMulLongCandidate(tree->AsOp(), kind);
                }

                if (tree->gtOverflow())
                {
                    helper = (tree->gtFlags & GTF_UNSIGNED) ? CORINFO_HELP_ULMUL_OVF : CORINFO_HELP_LMUL_OVF;
                }
                else
                {
                    helper = CORINFO_HELP_LMUL;
                }

                goto USE_HELPER_FOR_ARITH;
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
            // array.Length is always positive so GT_DIV can be changed to GT_UDIV
            // if op2 is a positive cns
            if (op1->OperIs(GT_ARR_LENGTH) && op2->IsIntegralConst() &&
                op2->AsIntCon()->IconValue() >= 2) // for 0 and 1 it doesn't matter if it's UDIV or DIV
            {
                assert(tree->OperIs(GT_DIV));
                tree->ChangeOper(GT_UDIV);
                return fgMorphSmpOp(tree, mac);
            }

            if (opts.OptimizationEnabled())
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
            // array.Length is always positive so GT_DIV can be changed to GT_UDIV
            // if op2 is a positive cns
            if (op1->OperIs(GT_ARR_LENGTH) && op2->IsIntegralConst() &&
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
#ifdef TARGET_XARCH
            // If this is an unsigned long mod with a constant divisor, then don't
            // morph to a helper call - it can be done faster inline using idiv.

            if ((typ == TYP_LONG) && op2->OperIs(GT_CNS_NATIVELONG) && opts.OptEnabled(CLFLG_CONSTANTFOLD) &&
                (op2->AsIntConCommon()->LngValue() >= 2) && (op2->AsIntConCommon()->LngValue() <= 0x3fffffff))
            {
                op1 = fgMorphTree(op1);
                noway_assert(op1->TypeIs(TYP_LONG));
                tree->AsOp()->SetOp(0, op1);

                // Update flags for op1 morph.
                tree->gtFlags &= ~GTF_ALL_EFFECT;

                // Only update with op1 as op2 is a constant.
                tree->gtFlags |= (op1->gtFlags & GTF_ALL_EFFECT);

                // If op1 is a constant, then do constant folding of the division operator.
                if (op1->OperIs(GT_CNS_NATIVELONG))
                {
                    tree = gtFoldExpr(tree);
                }

                if (!tree->IsNumericConst())
                {
                    tree->AsOp()->CheckDivideByConstOptimized(this);
                }

                return tree;
            }
#endif // TARGET_XARCH

        ASSIGN_HELPER_FOR_MOD:
            // For "val % 1", return 0 if op1 doesn't have any side effects

            if ((op1->gtFlags & GTF_SIDE_EFFECT) == 0)
            {
                if (op2->IsIntegralConst(1))
                {
                    GenTree* zeroNode = gtNewZeroConNode(typ);
                    INDEBUG(zeroNode->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
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
                    assert(!csePhase);

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

            if (tree->OperIs(GT_MOD) && op2->IsIntegralConst())
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

        case GT_FDIV:
            // Replace "val / C" with "val * (1.0 / C)" if C is a power of two.
            // Powers of two within range are always exactly represented,
            // so multiplication by the reciprocal is safe in this scenario
            if (fgGlobalMorph && op2->IsDblCon())
            {
                double divisor = op2->AsDblCon()->GetValue();

                if (((typ == TYP_DOUBLE) && FloatingPointUtils::hasPreciseReciprocal(divisor)) ||
                    ((typ == TYP_FLOAT) && FloatingPointUtils::hasPreciseReciprocal(forceCastToFloat(divisor))))
                {
                    oper = GT_FMUL;
                    tree->ChangeOper(oper);
                    op2->AsDblCon()->SetValue(1.0 / divisor);
                }
            }
            break;

        case GT_FMOD:
            helper = CORINFO_HELP_DBLREM;
            if (op1->TypeIs(TYP_FLOAT))
            {
                if (op2->TypeIs(TYP_FLOAT))
                {
                    helper = CORINFO_HELP_FLTREM;
                }
                else
                {
                    op1 = gtNewCastNode(op1, false, TYP_DOUBLE);
                    tree->AsOp()->SetOp(0, op1);
                }
            }
            else if (op2->TypeIs(TYP_FLOAT))
            {
                op2 = gtNewCastNode(op2, false, TYP_DOUBLE);
                tree->AsOp()->SetOp(1, op2);
            }

#ifndef TARGET_64BIT
        USE_HELPER_FOR_ARITH:
#endif
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
                noway_assert(fgIsCommaThrow(tree DEBUGARG(false)));
                return fgMorphTree(tree);
            }
        }
            return fgMorphIntoHelperCall(tree, helper, gtNewCallArgs(op1, op2));

        case GT_RETURN:
            if (fgGlobalMorph && varTypeIsSmall(info.compRetType) && gtIsSmallIntCastNeeded(op1, info.compRetType))
            {
                // Small-typed return values are extended by the callee.

                op1 = gtNewCastNode(op1, false, info.compRetType);
                op1 = fgMorphTree(op1);

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
            if (tree->AsIntrinsic()->GetIntrinsic() == NI_System_Math_Round)
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

    if (op1 != nullptr)
    {
        MorphAddrContext  newOp1Mac(false);
        MorphAddrContext* op1Mac = mac;

        if (!op1->IsIndir())
        {
            if (tree->OperIs(GT_IND, GT_OBJ, GT_BLK))
            {
                if (op1Mac == nullptr)
                {
                    op1Mac = &newOp1Mac;
                }
            }
            else if (op1Mac != nullptr)
            {
                if (tree->OperIs(GT_ADD) && tree->AsOp()->GetOp(1)->IsIntCon())
                {
                    op1Mac->offset +=
                        static_cast<target_size_t>(tree->AsOp()->GetOp(1)->AsIntCon()->GetUnsignedValue());
                }
                else if (tree->OperIs(GT_COMMA))
                {
                    // COMMA's first operand has nothing to do with any existing address context.
                    op1Mac = nullptr;
                }
                else
                {
                    op1Mac->isOffsetConstant = false;
                }
            }
        }

        op1 = fgMorphTree(op1, op1Mac);
        tree->AsOp()->SetOp(0, op1);
    }

    if (op2 != nullptr)
    {
        if (!op2->IsIndir() && (mac != nullptr))
        {
            if (tree->OperIs(GT_ADD) && tree->AsOp()->GetOp(0)->IsIntCon())
            {
                mac->offset += static_cast<target_size_t>(tree->AsOp()->GetOp(0)->AsIntCon()->GetUnsignedValue());
            }
            else if (!tree->OperIs(GT_COMMA))
            {
                mac->isOffsetConstant = false;
            }
        }

        op2 = fgMorphTree(op2, mac);
        tree->AsOp()->SetOp(1, op2);
    }

DONE_MORPHING_CHILDREN:
    GenTreeFlags flags = tree->gtFlags & ~(GTF_ASG | GTF_CALL | GTF_EXCEPT);

    if (op1 != nullptr)
    {
        flags |= op1->GetSideEffects();
    }

    if (op2 != nullptr)
    {
        flags |= op2->GetSideEffects();
    }

    if (tree->OperRequiresAsgFlag())
    {
        flags |= GTF_ASG;
    }

    if (tree->OperRequiresCallFlag(this))
    {
        flags |= GTF_CALL;
    }

    if (tree->OperMayThrow(this))
    {
        flags |= GTF_EXCEPT;
    }
    else if (tree->OperIsIndirOrArrLength())
    {
        flags |= GTF_IND_NONFAULTING;
    }

    tree->gtFlags = flags;

    // Now do POST-ORDER processing

    if (varTypeIsGC(tree->GetType()) && ((op1 != nullptr) && !varTypeIsGC(op1->GetType())) &&
        ((op2 != nullptr) && !varTypeIsGC(op2->GetType())))
    {
        // The tree is really not GC but was marked as such. Now that the
        // children have been unmarked, unmark the tree too.

        tree->SetType(varActualType((tree->OperIs(GT_COMMA) ? op2 : op1)->GetType()));
    }

    GenTree* oldTree = tree;

    // Try to fold it, maybe we get lucky,
    tree = gtFoldExpr(tree);

    if (oldTree != tree)
    {
        /* if gtFoldExpr returned op1 or op2 then we are done */
        if ((tree == op1) || (tree == op2))
        {
            return tree;
        }

        /* If we created a comma-throw tree then we need to morph op1 */
        if (fgIsCommaThrow(tree DEBUGARG(false)))
        {
            tree->AsOp()->gtOp1 = fgMorphTree(tree->AsOp()->gtOp1);
            INDEBUG(tree->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
            return tree;
        }

        return tree;
    }
    else if (tree->OperIsConst())
    {
        return tree;
    }

    oper = tree->GetOper();
    typ  = tree->GetType();
    op1  = tree->AsOp()->gtOp1;
    op2  = tree->gtGetOp2IfPresent();

    BasicBlock* currentBlock = fgMorphBlock;

    // Perform the required oper-specific postorder morphing
    switch (oper)
    {
        GenTree* cns1;
        GenTree* cns2;
        size_t   ival1, ival2;

        case GT_ASG:
            if (op1->OperIs(GT_BLK))
            {
                return fgMorphBlockAssignment(tree->AsOp());
            }

            if (varTypeIsStruct(op1->GetType()))
            {
                return fgMorphStructAssignment(tree->AsOp());
            }

            // If we are storing a small type, we might be able to omit a cast.
            // We may also omit a cast when storing to a "normalize on load"
            // local since we know that a load from that local has to cast anyway.

            if (varTypeIsSmall(op1->GetType()) &&
                (op1->OperIs(GT_IND, GT_LCL_FLD) ||
                 (op1->OperIs(GT_LCL_VAR) && op1->AsLclVar()->GetLcl()->lvNormalizeOnLoad())))
            {
                if (op2->IsCast() && varTypeIsIntegral(op2->AsCast()->GetOp(0)) && !op2->gtOverflow() &&
                    varTypeIsSmall(op2->GetType()) && (varTypeSize(op2->GetType()) >= varTypeSize(op1->GetType())))
                {
                    op2 = op2->AsCast()->GetOp(0);
                    tree->AsOp()->SetOp(1, op2);
                }
            }

            tree->SetType(op1->GetType());

            if (op1->OperIs(GT_LCL_VAR))
            {
                tree->ChangeOper(GT_STORE_LCL_VAR);
                tree->AsLclVar()->SetLcl(op1->AsLclVar()->GetLcl());
                tree->AsLclVar()->SetOp(0, op2);
                tree->gtFlags |= op1->gtFlags & GTF_SPECIFIC_MASK;
            }
            else if (op1->OperIs(GT_LCL_FLD))
            {
                tree->ChangeOper(GT_STORE_LCL_FLD);
                tree->AsLclFld()->SetLcl(op1->AsLclFld()->GetLcl());
                tree->AsLclFld()->SetLclOffs(op1->AsLclFld()->GetLclOffs());
                tree->AsLclFld()->SetFieldSeq(op1->AsLclFld()->GetFieldSeq());
                tree->AsLclFld()->SetLayoutNum(op1->AsLclFld()->GetLayoutNum());
                tree->AsLclFld()->SetOp(0, op2);
                tree->gtFlags |= op1->gtFlags & GTF_SPECIFIC_MASK;
            }
            else
            {
                tree->ChangeOper(GT_STOREIND);
                tree->AsIndir()->SetAddr(op1->AsIndir()->GetAddr());
                tree->AsIndir()->SetValue(op2);
                tree->gtFlags |= op1->gtFlags & GTF_SPECIFIC_MASK;
            }

            return tree;

        case GT_INIT_VAL:
            if (op1->IsIntegralConst(0))
            {
                tree = op1;
            }

            return tree;

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
            if (opts.OptimizationEnabled() && op2->IsIntCon())
            {
                if (op1->OperIs(GT_MOD) && (op2->AsIntCon()->GetValue() >= 0))
                {
                    // (x MOD pow2) EQ|NE 0 => (x AND (pow2 - 1)) EQ|NE 0
                    // (x MOD pow2) EQ|NE [1..pow2 - 1] => (x AND (sign_bit | (pow2 - 1))) EQ|NE [1..pow2 - 1]

                    // TODO-MIKE-Review: The second case might need to be moved to lowering, doing it here
                    // prevents assertion prop from transforming MOD into UMOD. Assertion prop could drop
                    // the sign bit from the AND mask, but it does it in cases where it isn't necessary and
                    // result in larger immediates being generated (e.g. -1 is imm8 on x86/64 but if you drop
                    // the sign bit it becomes 0x7fffffff which is imm32).

                    if (GenTreeIntCon* modOp2 = op1->AsOp()->GetOp(1)->IsIntCon())
                    {
                        if (isPow2(modOp2->GetValue()) && (op2->AsIntCon()->GetValue() < modOp2->GetValue()))
                        {
                            ssize_t mask = modOp2->GetValue() - 1;

                            if (!op2->IsIntCon(0))
                            {
                                if (varTypeSize(modOp2->GetType()) <= 4)
                                {
                                    mask = static_cast<int32_t>(mask | INT32_MIN);
                                }
                                else
                                {
                                    mask |= INT64_MIN;
                                }
                            }

                            op1->SetOper(GT_AND);
                            modOp2->SetValue(mask);
                        }
                    }
                }
                else if (op1->OperIs(GT_AND) && op2->AsIntCon()->HasSingleSetBit())
                {
                    // (x AND pow2) EQ|NE pow2 => (x AND pow2) NE|EQ 0

                    if (op1->AsOp()->GetOp(1)->IsIntCon(op2->AsIntCon()->GetValue()))
                    {
                        op2->AsIntCon()->SetValue(0);
                        oper = oper == GT_EQ ? GT_NE : GT_EQ;
                        tree->SetOperRaw(oper);
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

            if (cns2->IsIntCon() && (cns2->AsIntCon()->GetUnsignedValue() <= 1u))
            {
                ival2 = cns2->AsIntCon()->GetUnsignedValue();
            }
#ifndef TARGET_64BIT
            else if (cns2->IsLngCon() && (cns2->AsLngCon()->GetUInt64Value() <= 1ull))
            {
                ival2 = static_cast<uint32_t>(cns2->AsLngCon()->GetUInt64Value());
            }
#endif

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
                    GenTree*   comma = op1;
                    GenTreeOp* relop = comma->AsOp()->GetOp(1)->AsOp();

                    GenTree* relop_op1 = relop->GetOp(0);

                    bool reverse = ((ival2 == 0) == (oper == GT_EQ));

                    if (reverse)
                    {
                        gtReverseRelop(relop);
                    }

                    relop->SetOp(0, comma);
                    comma->AsOp()->SetOp(1, relop_op1);

                    // Comma now has fewer nodes underneath it, so we need to regenerate its flags
                    comma->gtFlags &= ~GTF_ALL_EFFECT;
                    comma->gtFlags |= (comma->AsOp()->gtOp1->gtFlags) & GTF_ALL_EFFECT;
                    comma->gtFlags |= (comma->AsOp()->gtOp2->gtFlags) & GTF_ALL_EFFECT;

                    noway_assert((relop->gtFlags & GTF_RELOP_JMP_USED) == 0);
                    noway_assert((relop->gtFlags & GTF_REVERSE_OPS) == 0);
                    relop->gtFlags |= tree->gtFlags & (GTF_RELOP_JMP_USED | GTF_DONT_CSE | GTF_ALL_EFFECT);

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

                    // The right side of the comma must be a LCL_VAR temp
                    if (!lcl->OperIs(GT_LCL_VAR))
                    {
                        goto SKIP;
                    }

                    LclVarDsc* lclVar = lcl->AsLclVar()->GetLcl();

                    // If the LCL_VAR is not a temp then bail, a temp has a single def
                    if (!lclVar->lvIsTemp)
                    {
                        goto SKIP;
                    }

                    // If the LCL_VAR is a CSE temp then bail, it could have multiple defs/uses
                    if (lclVar->lvIsCSE)
                    {
                        goto SKIP;
                    }

                    // We also must be assigning the result of a RELOP
                    if (asg->AsOp()->gtOp1->gtOper != GT_LCL_VAR)
                    {
                        goto SKIP;
                    }

                    // Both of the LCL_VAR must match
                    if (asg->AsOp()->gtOp1->AsLclVar()->GetLcl() != lcl->AsLclVar()->GetLcl())
                    {
                        goto SKIP;
                    }

                    // If right side of asg is not a RELOP then skip
                    if (!asg->AsOp()->gtOp2->OperIsCompare())
                    {
                        goto SKIP;
                    }

                    // Set op1 to the right side of asg, (i.e. the RELOP)
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
                        gtReverseRelop(op1->AsOp());
                    }

                    noway_assert((op1->gtFlags & GTF_RELOP_JMP_USED) == 0);
                    op1->gtFlags |= tree->gtFlags & (GTF_RELOP_JMP_USED | GTF_DONT_CSE);

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
                            gtReverseRelop(tree->AsOp());
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
                            gtReverseRelop(tree->AsOp());
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

            if (!cns2->OperIsConst() || !cns2->TypeIs(TYP_LONG) || ((cns2->AsIntConCommon()->LngValue() >> 31) != 0))
            {
                noway_assert(tree->OperIsCompare());
                break;
            }

            /* Is the first comparand mask operation of type long ? */

            if (op1->gtOper != GT_AND)
            {
                /* Another interesting case: cast from int */

                if (op1->IsCast() && op1->AsCast()->GetOp(0)->TypeIs(TYP_INT) && !op1->gtOverflow())
                {
                    /* Simply make this into an integer comparison */

                    tree->AsOp()->SetOp(0, op1->AsCast()->GetOp(0));
                    tree->AsOp()->SetOp(1, gtNewIconNode((int)cns2->AsIntConCommon()->LngValue(), TYP_INT));
                }

                noway_assert(tree->OperIsCompare());
                break;
            }

            noway_assert(op1->TypeGet() == TYP_LONG && op1->OperGet() == GT_AND);

            // The transform below cannot preserve VNs.
            if (fgGlobalMorph)
            {
                // Is the result of the mask effectively an INT ?

                GenTree* andMask = op1->AsOp()->gtOp2;

                if (!andMask->OperIs(GT_CNS_NATIVELONG) || ((andMask->AsIntConCommon()->LngValue() >> 32) != 0))
                {
                    noway_assert(tree->OperIsCompare());
                    break;
                }

                // Now we narrow AsOp()->gtOp1 of AND to int.
                if (fgMorphNarrowTree(op1->AsOp()->gtGetOp1(), TYP_LONG, TYP_INT, ValueNumPair(), false))
                {
                    fgMorphNarrowTree(op1->AsOp()->gtGetOp1(), TYP_LONG, TYP_INT, ValueNumPair(), true);
                }
                else
                {
                    op1->AsOp()->gtOp1 = gtNewCastNode(op1->AsOp()->gtGetOp1(), false, TYP_INT);
                }

                // now replace the mask node (AsOp()->gtOp2 of AND node).

                noway_assert(andMask == op1->AsOp()->gtOp2);

                ival1 = (int)andMask->AsIntConCommon()->LngValue();
                andMask->SetOper(GT_CNS_INT);
                andMask->gtType                = TYP_INT;
                andMask->AsIntCon()->gtIconVal = ival1;

                // now change the type of the AND node.

                op1->gtType = TYP_INT;

                // finally we replace the comparand.

                ival2 = (int)cns2->AsIntConCommon()->LngValue();
                cns2->SetOper(GT_CNS_INT);
                cns2->gtType = TYP_INT;

                noway_assert(cns2 == op2);
                cns2->AsIntCon()->gtIconVal = ival2;
            }

            noway_assert(tree->OperIsCompare());
            break;

        case GT_LT:
        case GT_LE:
        case GT_GE:
        case GT_GT:
            if (!op2->OperIs(GT_CNS_INT))
            {
                if (!op1->OperIs(GT_CNS_INT))
                {
                    break;
                }

                oper = GenTree::SwapRelop(oper);
                std::swap(op1, op2);

                tree->SetOper(oper, GenTree::PRESERVE_VN);
                tree->AsOp()->SetOp(0, op1);
                tree->AsOp()->SetOp(1, op2);
            }

            if (op2->IsIntegralConst(0))
            {
                if (((oper == GT_GT) || (oper == GT_LE)) && tree->IsUnsigned())
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

                break;
            }

            // Convert 1 compares to 0 compares (e.g. x < 1 becomes x <= 0)

            if (op2->IsIntegralConst(1))
            {
                if (oper == GT_GE)
                {
                    oper = tree->IsUnsigned() ? GT_NE : GT_GT;
                }
                else if (oper == GT_LT)
                {
                    oper = tree->IsUnsigned() ? GT_EQ : GT_LE;
                }
            }
            else if (op2->IsIntegralConst(-1))
            {
                if (oper == GT_LE)
                {
                    oper = tree->IsUnsigned() ? oper : GT_LT;
                }
                else if (oper == GT_GT)
                {
                    oper = tree->IsUnsigned() ? oper : GT_GE;
                }
            }

            if (tree->GetOper() != oper)
            {
                tree->SetOper(oper, GenTree::PRESERVE_VN);
                op2->AsIntCon()->SetValue(0);

                if (vnStore != nullptr)
                {
                    op2->SetVNP(ValueNumPair{vnStore->VNZeroForType(op2->GetType())});
                }
            }
            break;

        case GT_FADD:
            if (op1->IsDblCon())
            {
                std::swap(op1, op2);

                tree->AsOp()->SetOp(0, op1);
                tree->AsOp()->SetOp(1, op2);
            }

            // ADD(op1, NEG(a)) => SUB(op1, a)
            // ADD(NEG(a), op2) => SUB(op2, a)
            if (opts.OptimizationEnabled() && fgGlobalMorph)
            {
                if (op2->OperIs(GT_FNEG))
                {
                    GenTree* a = op2->AsOp()->GetOp(0);

                    oper = GT_FSUB;
                    tree->SetOper(oper);
                    tree->AsOp()->SetOp(1, a);

                    DEBUG_DESTROY_NODE(op2);

                    op2 = a;
                }
                else if (op1->OperIs(GT_FNEG) && gtCanSwapOrder(op1, op2))
                {
                    GenTree* a = op1->AsOp()->GetOp(0);

                    oper = GT_FSUB;
                    tree->SetOper(oper);
                    tree->AsOp()->SetOp(0, op2);
                    tree->AsOp()->SetOp(1, a);

                    DEBUG_DESTROY_NODE(op1);

                    op1 = op2;
                    op2 = a;
                }
            }
            break;

        case GT_FSUB:
            // SUB(op1, NEG(b)) => ADD(op1, b)
            // SUB(NEG(a), NEG(b)) => SUB(b, a)
            if (opts.OptimizationEnabled() && fgGlobalMorph && op2->OperIs(GT_FNEG))
            {
                GenTree* b = op2->AsUnOp()->GetOp(0);

                if (!op1->OperIs(GT_FNEG))
                {
                    oper = GT_FADD;
                    tree->SetOper(oper);
                    tree->AsOp()->SetOp(1, b);

                    DEBUG_DESTROY_NODE(op2);

                    op2 = b;
                }
                else if (gtCanSwapOrder(op1, op2))
                {
                    GenTree* a = op1->AsUnOp()->GetOp(0);

                    tree->AsOp()->SetOp(0, b);
                    tree->AsOp()->SetOp(1, a);

                    DEBUG_DESTROY_NODE(op1);
                    DEBUG_DESTROY_NODE(op2);

                    op1 = b;
                    op2 = a;
                }
            }
            break;

        case GT_FMUL:
            if (op1->IsDblCon())
            {
                std::swap(op1, op2);

                tree->AsOp()->SetOp(0, op1);
                tree->AsOp()->SetOp(1, op2);
            }

            if (opts.OptimizationEnabled() && !op1->IsDblCon())
            {
                if (GenTreeDblCon* con = op2->IsDblCon())
                {
                    if (con->GetValue() == 2.0)
                    {
                        // TODO-MIKE-Review: Allowing LCL_FLD (or DNER LCL_VAR) may result in poor CQ
                        // if CSE doesn't pick it up. But then the question is why the crap is this
                        // being done here instead of codegen to begin with...
                        // P.S. So this will add a COMMA if it ever runs in LIR?!? Dumbness never ends...
                        // Basically this only works because morphing does not normally run in LIR
                        // and it's only useful if this node happens to appear in the argument tree
                        // of an INTRINSIC call, which is morphed during rationalization.
                        bool needsComma = !op1->OperIsLeaf() && !op1->OperIs(GT_LCL_VAR, GT_LCL_FLD);
                        // if op1 is not a leaf/local we have to introduce a temp via GT_COMMA.
                        // Unfortunately, it's not hoist loop code-friendly yet so let's do it later.
                        if (!needsComma || ((currentBlock->bbFlags & BBF_IS_LIR) != 0))
                        {
                            // Fold "x*2.0" to "x+x"
                            op2  = fgMakeMultiUse(&tree->AsOp()->gtOp1);
                            op1  = tree->AsOp()->GetOp(0);
                            oper = GT_FADD;
                            tree = gtNewOperNode(GT_FADD, tree->GetType(), op1, op2);
                            INDEBUG(tree->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
                        }
                    }
                    else if (con->GetValue() == 1.0)
                    {
                        // Fold "x*1.0" to "x"
                        DEBUG_DESTROY_NODE(op2);
                        DEBUG_DESTROY_NODE(tree);
                        return op1;
                    }
                }
            }
            break;

#ifdef TARGET_ARM64
        case GT_DIV:
            fgGetThrowHelperBlock(ThrowHelperKind::Overflow, currentBlock);
            fgGetThrowHelperBlock(ThrowHelperKind::DivideByZero, currentBlock);
            break;
        case GT_UDIV:
            fgGetThrowHelperBlock(ThrowHelperKind::DivideByZero, currentBlock);
            break;
#endif

        case GT_SUB:
            if (tree->gtOverflow())
            {
                fgGetThrowHelperBlock(ThrowHelperKind::Overflow, currentBlock);
                break;
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
                if (op1->IsIntCon())
                {
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
            if (opts.OptimizationEnabled() && fgGlobalMorph)
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

                if (op1->OperIs(GT_NEG) && op2->OperIs(GT_NEG) && gtCanSwapOrder(op1, op2))
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

        case GT_MUL:
#ifndef TARGET_64BIT
            assert(typ != TYP_LONG);
#endif
            FALLTHROUGH;
        case GT_ADD:
            if (tree->gtOverflow())
            {
                fgGetThrowHelperBlock(ThrowHelperKind::Overflow, currentBlock);
                break;
            }
        CM_ADD_OP:
            FALLTHROUGH;
        case GT_OR:
        case GT_XOR:
        case GT_AND:
            // Commute any non-REF constants to the right
            noway_assert(op1);
            assert(varTypeIsIntegralOrI(tree->GetType()));

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
            if (tree->OperIs(GT_AND) && tree->TypeIs(TYP_INT) && op1->OperIsCompare() && op2->IsIntegralConst(1))
            {
                DEBUG_DESTROY_NODE(op2);
                DEBUG_DESTROY_NODE(tree);
                return op1;
            }

            if (tree->OperIs(GT_ADD, GT_MUL, GT_AND, GT_OR, GT_XOR))
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

            /* See if we can fold GT_ADD nodes. */

            if (oper == GT_ADD)
            {
                /* Fold "((x+icon1)+(y+icon2)) to ((x+y)+(icon1+icon2))" */

                if (op1->gtOper == GT_ADD && op2->gtOper == GT_ADD && op1->AsOp()->gtOp2->gtOper == GT_CNS_INT &&
                    op2->AsOp()->gtOp2->gtOper == GT_CNS_INT && !op1->gtOverflow() && !op2->gtOverflow())
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

                if (op2->IsIntCon())
                {
                    CLANG_FORMAT_COMMENT_ANCHOR;

                    // Fold (x + 0).

                    if (op2->AsIntConCommon()->IconValue() == 0)
                    {
                        // If this addition is adding an offset to a null pointer,
                        // avoid the work and yield the null pointer immediately.
                        // Dereferencing the pointer in either case will have the
                        // same effect.

                        if (varTypeIsGC(op2->TypeGet()) && ((op1->gtFlags & GTF_ALL_EFFECT) == 0))
                        {
                            op2->gtType = tree->gtType;
                            DEBUG_DESTROY_NODE(op1);
                            DEBUG_DESTROY_NODE(tree);
                            return op2;
                        }

                        // Remove the addition iff it won't change the tree type
                        // to TYP_REF.

                        if ((op1->TypeGet() == tree->TypeGet()) || (op1->TypeGet() != TYP_REF))
                        {
                            if (fgGlobalMorph && op2->IsIntCon() && (op2->AsIntCon()->GetFieldSeq() != nullptr) &&
                                (op2->AsIntCon()->GetFieldSeq() != FieldSeqStore::NotAField()))
                            {
                                AddZeroOffsetFieldSeq(op1, op2->AsIntCon()->GetFieldSeq());
                            }

                            DEBUG_DESTROY_NODE(op2);
                            DEBUG_DESTROY_NODE(tree);

                            return op1;
                        }
                    }
                }

                if (opts.OptimizationEnabled() && fgGlobalMorph)
                {
                    // - a + b = > b - a
                    // ADD((NEG(a), b) => SUB(b, a)

                    // Skip optimization if non-NEG operand is constant.
                    if (op1->OperIs(GT_NEG) && !op2->OperIs(GT_NEG) && !op2->IsIntegralConst() &&
                        gtCanSwapOrder(op1, op2))
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
            else if ((oper == GT_MUL) && op2->IsIntCon())
            {
                noway_assert(typ == op2->GetType());
                noway_assert(!tree->gtOverflow());

                ssize_t mult = op2->AsIntCon()->GetValue();

                if (mult == 0)
                {
                    if ((op1->gtFlags & GTF_SIDE_EFFECT) == 0)
                    {
                        DEBUG_DESTROY_NODE(op1);
                        DEBUG_DESTROY_NODE(tree);

                        return op2;
                    }

                    tree->ChangeOper(GT_COMMA);

                    return tree;
                }

                size_t abs_mult      = (mult >= 0) ? mult : -mult;
                size_t lowestBit     = genFindLowestBit(abs_mult);
                bool   changeToShift = false;

                if (abs_mult == lowestBit)
                {
                    if ((mult < 0) && (mult != SSIZE_T_MIN))
                    {
                        op1 = gtNewOperNode(GT_NEG, typ, op1);
                        fgMorphTreeDone(op1);
                        tree->AsOp()->SetOp(0, op1);
                    }

                    if (abs_mult == 1)
                    {
                        DEBUG_DESTROY_NODE(op2);
                        DEBUG_DESTROY_NODE(tree);

                        return op1;
                    }

                    op2->AsIntCon()->SetValue(genLog2(abs_mult));
                    changeToShift = true;
                }
#if LEA_AVAILABLE
                else if ((lowestBit > 1) && AddrMode::IsIndexScale(lowestBit) && optAvoidIntMult())
                {
                    int     shift  = genLog2(lowestBit);
                    ssize_t factor = abs_mult >> shift;

                    if ((factor == 3) || (factor == 5) || (factor == 9))
                    {
                        if ((mult < 0) && (mult != SSIZE_T_MIN))
                        {
                            op1 = gtNewOperNode(GT_NEG, typ, op1);
                            fgMorphTreeDone(op1);
                            tree->AsOp()->SetOp(0, op1);
                        }

                        op1 = gtNewOperNode(GT_MUL, typ, op1, gtNewIconNode(factor, typ));
                        fgMorphTreeDone(op1);
                        tree->AsOp()->SetOp(0, op1);

                        op2->AsIntCon()->SetValue(shift);
                        changeToShift = true;
                    }
                }
#endif // LEA_AVAILABLE

                if (changeToShift)
                {
                    if (vnStore != nullptr)
                    {
#ifdef TARGET_64BIT
                        op2->SetVNP(ValueNumPair{op2->TypeIs(TYP_LONG)
                                                     ? vnStore->VNForLongCon(op2->AsIntCon()->GetInt64Value())
                                                     : vnStore->VNForIntCon(op2->AsIntCon()->GetInt32Value())});
#else
                        op2->SetVNP(ValueNumPair{vnStore->VNForIntCon(op2->AsIntCon()->GetInt32Value())});
#endif
                    }

                    oper = GT_LSH;
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
            break;

        case GT_NEG:
            // Distribute integer negation over simple multiplication/division expressions
            if (opts.OptimizationEnabled() && op1->OperIs(GT_MUL, GT_DIV))
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
            FALLTHROUGH;
        case GT_FNEG:
        case GT_NOT:
            // Remove double negation/not.
            if (op1->OperIs(oper) && opts.OptimizationEnabled())
            {
                JITDUMP("Remove double negation/not\n")
                GenTree* op1op1 = op1->gtGetOp1();
                DEBUG_DESTROY_NODE(tree);
                DEBUG_DESTROY_NODE(op1);
                return op1op1;
            }
            break;

        case GT_CKFINITE:
            noway_assert(varTypeIsFloating(op1->GetType()));
            fgGetThrowHelperBlock(ThrowHelperKind::Arithmetic, currentBlock);
            break;

        case GT_INDEX_ADDR:
            assert(opts.MinOpts());

            if ((tree->gtFlags & GTF_INX_RNGCHK) != 0)
            {
                tree->AsIndexAddr()->SetThrowBlock(
                    fgGetThrowHelperBlock(ThrowHelperKind::IndexOutOfRange, currentBlock));
            }
            break;

#ifdef TARGET_ARM
        case GT_IND:
            // Check for a misalignment floating point indirection.
            // TODO-MIKE-Cleanup: This should be moved to lowering
            // (or perhaps decomposition to deal with DOUBLE).
            // Besides, it's not clear why does this exist in the first place.
            // Work around for broken IL code?
            if (op1->OperIs(GT_ADD) && varTypeIsFloating(typ))
            {
                if (GenTreeIntCon* offset = op1->AsOp()->GetOp(1)->IsIntCon())
                {
                    if ((offset->GetValue() % 4) != 0)
                    {
                        tree->AsIndir()->SetUnaligned();
                    }
                }
            }
            break;
#endif // TARGET_ARM

        case GT_COMMA:
            // Special case: trees that don't produce a value
            if (op2->OperIs(GT_ASG, GT_LCL_DEF, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD) ||
                (op2->OperIs(GT_COMMA) && op2->TypeIs(TYP_VOID)) || fgIsThrow(op2))
            {
                tree->SetType(TYP_VOID);
                typ = TYP_VOID;
            }

            {
                // Extract the side effects from the left side of the comma.  Since they don't "go" anywhere, this
                // is all we need.

                // The addition of "GTF_MAKE_CSE" below prevents us from throwing away (for example)
                // hoisted expressions in loops.
                GenTree* op1SideEffects = gtExtractSideEffList(op1, GTF_SIDE_EFFECT | GTF_MAKE_CSE);

                if (op1SideEffects != nullptr)
                {
                    // TODO-MIKE-Review: This doesn't update `op1`!?
                    tree->AsOp()->SetOp(0, op1SideEffects);
                    tree->SetSideEffects(op1SideEffects->GetSideEffects() | op2->GetSideEffects());
                }
                else
                {
                    op2->gtFlags |= (tree->gtFlags & GTF_DONT_CSE);
                    DEBUG_DESTROY_NODE(tree);
                    DEBUG_DESTROY_NODE(op1);
                    return op2;
                }

                /* If the right operand is just a void nop node, throw it away */
                if (op2->IsNothingNode() && op1->gtType == TYP_VOID)
                {
                    op1->gtFlags |= (tree->gtFlags & GTF_DONT_CSE);
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
                if (fgIsCommaThrow(op1 DEBUGARG(true)))
                {
                    GenTree* throwNode = op1->AsOp()->gtOp1;

                    JITDUMP("Removing [%06d] GT_JTRUE as the block now unconditionally throws an exception.\n",
                            dspTreeID(tree));
                    DEBUG_DESTROY_NODE(tree);

                    return throwNode;
                }

                noway_assert(op1->OperIsCompare());
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

    // If the tree always throws an exception we can drop most of it, except, of course
    // the exception throwing parts. But we cannot remove assignments as that will mess
    // up call arg setup, which expects an assignment tree and doesn't know that the
    // tree will always throw an exception. Likewise, we cannot remove indirections as
    // they could be assignment destinations. We cannot remove INIT_VAL either since it
    // can be the source of an BLK assignment source, which does not expect the throwing
    // COMMA thing.
    // TODO-MIKE-Review: Why bother do anything here to begin with? Can't we just set
    // fgRemoveRestOfBlock and have fgMorphTree callers deal with it?
    if ((oper != GT_ASG) && (oper != GT_LCL_DEF) && (oper != GT_STORE_LCL_VAR) && (oper != GT_IND) &&
        (oper != GT_OBJ) && (oper != GT_BLK) && (oper != GT_INIT_VAL))
    {
        /* Check for op1 as a GT_COMMA with a unconditional throw node */
        if (op1 && fgIsCommaThrow(op1 DEBUGARG(true)))
        {
            /* We can safely throw out the rest of the statements */
            fgRemoveRestOfBlock = true;

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

        if ((op2 != nullptr) && fgIsCommaThrow(op2 DEBUGARG(true)))
        {
            fgRemoveRestOfBlock = true;

            if ((op1->gtFlags & GTF_ALL_EFFECT) == 0)
            {
                if (tree->OperIs(GT_ASG, GT_BOUNDS_CHECK, GT_COMMA))
                {
                    return op2->AsOp()->gtOp1;
                }

                // for the shift nodes the type of op2 can differ from the tree type
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

    if (opts.OptEnabled(CLFLG_TREETRANS))
    {
        tree = fgMorphSmpOpOptional(tree->AsOp());
    }

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

        if (GenTreeCall* call = val->IsCall())
        {
            bool registersMatch = call->GetRegCount() == info.retDesc.GetRegCount();

            for (unsigned i = 0; i < call->GetRegCount() && registersMatch; i++)
            {
                if (call->GetRetDesc()->GetRegNum(i) != info.retDesc.GetRegNum(i))
                {
                    registersMatch = false;
                }
            }

            if (registersMatch)
            {
                return;
            }
        }
        else if (val->OperIs(GT_LCL_FLD) && varTypeIsSIMD(val->GetType()))
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

            for (unsigned i = 0; i < commas.Size(); i++)
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
        LclVarDsc*     lcl    = lclVar->GetLcl();

#ifdef TARGET_AMD64
        if (varTypeIsSIMD(lcl->GetType()) && lcl->IsPromoted())
        {
            // Only Vector2/3/4 are promoted.
            assert(lvaGetDesc(lcl->GetPromotedFieldLclNum(0))->TypeIs(TYP_FLOAT));

            // TODO-MIKE-Review: Could we get here on ARM64 due to Vector2/Vector64
            // reinterpretation?

            val = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_FLOAT, 16,
                                           gtNewLclvNode(lvaGetDesc(lcl->GetPromotedFieldLclNum(0)), TYP_FLOAT),
                                           gtNewLclvNode(lvaGetDesc(lcl->GetPromotedFieldLclNum(1)), TYP_FLOAT));

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
    genTreeOps oper = tree->GetOper();
    GenTree*   op1  = tree->gtOp1;
    GenTree*   op2  = tree->gtOp2;

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

        if (tree->OperIs(GT_ADD, GT_XOR, GT_OR, GT_AND, GT_MUL) && (op2->GetOper() == oper))
        {
            /*  Reorder nested operators at the same precedence level to be
                left-recursive. For example, change "(a+(b+c))" to the
                equivalent expression "((a+b)+c)".
             */

            fgMoveOpsLeft(tree);
            op1 = tree->gtOp1;
            op2 = tree->gtOp2;
        }
    }

    // Change "((x+icon)+y)" to "((x+y)+icon)"
    // Don't reorder floating-point operations.

    if (fgGlobalMorph && (oper == GT_ADD) && !tree->gtOverflow() && (op1->gtOper == GT_ADD) && !op1->gtOverflow() &&
        varTypeIsIntegralOrI(tree->GetType()))
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

    // Perform optional oper-specific postorder morphing
    switch (oper)
    {
        case GT_MUL:

            /* Check for the case "(val + icon) * icon" */

            if (op2->gtOper == GT_CNS_INT && op1->gtOper == GT_ADD)
            {
                GenTree* add = op1->AsOp()->gtOp2;

                if (add->IsCnsIntOrI() && (AddrMode::GetMulIndexScale(op2) != 0))
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

        case GT_UDIV:
        case GT_UMOD:
            tree->CheckDivideByConstOptimized(this);
            break;

        case GT_LSH:

            /* Check for the case "(val + icon) << icon" */

            if (op2->IsCnsIntOrI() && op1->gtOper == GT_ADD && !op1->gtOverflow())
            {
                GenTree* cns = op1->AsOp()->gtOp2;

                if (cns->IsCnsIntOrI() && (AddrMode::GetLshIndexScale(op2) != 0))
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
                gtReverseRelop(op1->AsOp());
                DEBUG_DESTROY_NODE(op2);
                DEBUG_DESTROY_NODE(tree);
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

#ifndef TARGET_64BIT
Compiler::MulLongCandidateKind Compiler::fgMorphIsMulLongCandidate(GenTreeOp* mul)
{
    assert(mul->OperIs(GT_MUL) && mul->TypeIs(TYP_LONG));

    // Operands have to be constants or INT to LONG, non-overflow checking casts.

    GenTreeLngCon* longConst1 = mul->GetOp(0)->IsLngCon();
    GenTreeLngCon* longConst2 = mul->GetOp(1)->IsLngCon();

    if ((longConst1 != nullptr) && (longConst2 != nullptr))
    {
        return opts.OptEnabled(CLFLG_CONSTANTFOLD) ? MulLongCandidateKind::Const : MulLongCandidateKind::None;
    }

    GenTreeCast* cast1   = nullptr;
    GenTreeCast* cast2   = nullptr;
    unsigned     op1Sign = 0;
    unsigned     op2Sign = 0;

    if (longConst1 == nullptr)
    {
        cast1 = mul->GetOp(0)->IsCast();

        if ((cast1 == nullptr) || (varActualType(cast1->GetOp(0)->GetType()) != TYP_INT) || cast1->gtOverflow())
        {
            if (!mul->gtOverflow() && (longConst2 != nullptr) && isPow2(longConst2->GetUInt64Value()))
            {
                return MulLongCandidateKind::Shift;
            }

            return MulLongCandidateKind::None;
        }

        // We don't care about cast signedness if the cast operand is known to be positive.

        GenTreeIntCon* intConst1 = cast1->GetOp(0)->IsIntCon();

        // TODO-MIKE-CQ: There are other values that could be easily recognized
        // as always positive: ARR_LENGTH, relops, small unsigned int casts...

        // TODO-MIKE-CQ: Microsoft.VisualBasic.Core's Operators:MultiplyUInt16(ushort,ushort)
        // has a signed multiply with overflow and unsigned cast operands that is currently
        // rejected due to signedness mismatch.
        // However, those are casts from USHORT so overflow checking is not actually required.
        // Old code managed to accept that due to incorrect overflow signedness checks.

        if ((intConst1 == nullptr) || (intConst1->GetInt32Value() < 0))
        {
            op1Sign = cast1->IsUnsigned() ? 1 : 2;
        }
    }

    if (longConst2 == nullptr)
    {
        cast2 = mul->GetOp(1)->IsCast();

        if ((cast2 == nullptr) || (varActualType(cast2->GetOp(0)->GetType()) != TYP_INT) || cast2->gtOverflow())
        {
            if (!mul->gtOverflow() && (longConst1 != nullptr) && isPow2(longConst1->GetUInt64Value()))
            {
                return MulLongCandidateKind::Shift;
            }

            return MulLongCandidateKind::None;
        }

        GenTreeIntCon* intConst2 = cast2->GetOp(0)->IsIntCon();

        if ((intConst2 == nullptr) || (intConst2->GetInt32Value() < 0))
        {
            op2Sign = cast2->IsUnsigned() ? 1 : 2;
        }
    }

    // If an operand is constant then treat it as an INT to LONG cast of that constant,
    // provided that the constant has a suitable range. For simplicity, we consider
    // this implied cast to have the same sign as the cast operand, otherwise we'd need
    // to tell fgMorphMulLongCandidate what kind of cast to create.

    if ((cast1 == nullptr) || (cast2 == nullptr))
    {
        int64_t      value = cast1 == nullptr ? longConst1->GetValue() : longConst2->GetValue();
        GenTreeCast* cast  = cast1 == nullptr ? cast2 : cast1;

        if (cast->IsUnsigned() ? ((value < 0) || (value > UINT32_MAX)) : ((value < INT32_MIN) || (value > INT32_MAX)))
        {
            return MulLongCandidateKind::None;
        }
    }

    // Both casts should have the same signedness and they should also match
    // the multiply signedness if overflow checking is required.

    unsigned mulSign = 0;

    if (mul->gtOverflow())
    {
        mulSign = mul->IsUnsigned() ? 1 : 2;
    }

    switch (op1Sign | op2Sign | mulSign)
    {
        case 1:
            return MulLongCandidateKind::Unsigned;
        case 0:
        case 2:
            return MulLongCandidateKind::Signed;
        default:
            return MulLongCandidateKind::None;
    }
}

GenTree* Compiler::fgMorphMulLongCandidate(GenTreeOp* mul, MulLongCandidateKind kind)
{
    assert(mul->OperIs(GT_MUL) && mul->TypeIs(TYP_LONG));
    assert(kind != MulLongCandidateKind::None);

    if (kind == MulLongCandidateKind::Const)
    {
        GenTree* con = gtFoldExprConst(mul);

        if (!con->IsLngCon())
        {
            noway_assert(fgIsCommaThrow(con DEBUGARG(false)));
            con = fgMorphTree(con);
        }

        return con;
    }

    GenTree* op1 = mul->GetOp(0);
    GenTree* op2 = mul->GetOp(1);

    if (kind == MulLongCandidateKind::Shift)
    {
        if (op1->IsLngCon())
        {
            std::swap(op1, op2);
        }

        assert(isPow2(op2->AsLngCon()->GetUInt64Value()));

        op1 = fgMorphTree(op1);

        op2->ChangeToIntCon(TYP_INT, genLog2(op2->AsLngCon()->GetUInt64Value()));

        mul->SetOper(GT_LSH);
        mul->SetOp(0, op1);
        mul->SetOp(1, op2);
        mul->SetSideEffects(op1->GetSideEffects() | op2->GetSideEffects());

        return mul;
    }

    if (GenTreeLngCon* longConst1 = op1->IsLngCon())
    {
        op1->ChangeToIntCon(TYP_INT, static_cast<int32_t>(longConst1->GetValue()));
        op1 = gtNewCastNode(op1, op2->AsCast()->IsUnsigned(), TYP_LONG);
        mul->SetOp(0, op1);
    }
    else if (GenTreeLngCon* longConst2 = op2->IsLngCon())
    {
        op2->ChangeToIntCon(TYP_INT, static_cast<int32_t>(longConst2->GetValue()));
        op2 = gtNewCastNode(op2, op1->AsCast()->IsUnsigned(), TYP_LONG);
        mul->SetOp(1, op2);
    }

    GenTreeCast* cast1 = op1->AsCast();
    GenTreeCast* cast2 = op2->AsCast();

    mul->gtFlags &= ~GTF_OVERFLOW;

    if (kind == MulLongCandidateKind::Unsigned)
    {
        cast1->gtFlags |= GTF_UNSIGNED;
        cast2->gtFlags |= GTF_UNSIGNED;
    }
    else
    {
        assert(kind == MulLongCandidateKind::Signed);

        cast1->gtFlags &= ~GTF_UNSIGNED;
        cast2->gtFlags &= ~GTF_UNSIGNED;
    }

    // fgMorphCast doesn't know about long multiply and may remove the casts,
    // morph the cast operands directly.
    cast1->SetOp(0, fgMorphTree(cast1->GetOp(0)));
    cast2->SetOp(0, fgMorphTree(cast2->GetOp(0)));

    cast1->SetSideEffects(cast1->GetOp(0)->GetSideEffects());
    cast2->SetSideEffects(cast2->GetOp(0)->GetSideEffects());
    mul->SetSideEffects(cast1->GetSideEffects() | cast2->GetSideEffects());

    // Because we keep the casts we also need to do constant folding directly.
    if (cast1->GetOp(0)->IsIntCon() && cast2->GetOp(0)->IsIntCon() && opts.OptEnabled(CLFLG_CONSTANTFOLD))
    {
        GenTree* const1 = gtFoldExprConst(cast1);
        GenTree* const2 = gtFoldExprConst(cast2);
        noway_assert(const1->OperIsConst() && const2->OperIsConst());

        mul->SetOp(0, const1);
        mul->SetOp(1, const2);

        GenTree* con = gtFoldExprConst(mul);
        noway_assert(con->IsLngCon());

        return con;
    }

    // These casts cannot and need not be CSEd, they'll be removed during decomposition.
    cast1->SetDoNotCSE();
    cast2->SetDoNotCSE();

    // Insert NOP nodes for the cast operands so that they do not get folded.

    if (!cast1->GetOp(0)->OperIs(GT_NOP))
    {
        GenTree* nop = gtNewOperNode(GT_NOP, TYP_INT, cast1->GetOp(0));
        nop->SetDoNotCSE();
        cast1->SetOp(0, nop);
    }

    if (!cast2->GetOp(0)->OperIs(GT_NOP))
    {
        GenTree* nop = gtNewOperNode(GT_NOP, TYP_INT, cast2->GetOp(0));
        nop->SetDoNotCSE();
        cast2->SetOp(0, nop);
    }

    return mul;
}
#endif // TARGET_64BIT

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
    assert(tree != nullptr);
    assert(!csePhase);

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
    if (verbose && JitConfig.TreesBeforeAfterMorph())
    {
        thisMorphNum = morphNum++;
        printf("\nfgMorphTree (before %d):\n", thisMorphNum);
        gtDispTree(tree);
    }
#endif

#ifdef DEBUG
    // fgMorphTree() can potentially replace a tree with another, and the
    // caller has to store the return value correctly.
    // Turn this on to always make copy of "tree" here to shake out
    // hidden/un-updated references.

    if (compStressCompile(STRESS_GENERIC_CHECK, 0))
    {
        GenTree* copy;

        if (GenTree::s_gtNodeSizes[tree->GetOper()] == TREE_NODE_SZ_SMALL)
        {
            copy = gtNewLargeOperNode(GT_ADD, TYP_INT);
        }
        else
        {
            copy = new (this, GT_CALL) GenTreeCall();
        }

        copy->ReplaceWith(tree, this);

        DEBUG_DESTROY_NODE(tree);
        tree = copy;
    }
#endif // DEBUG

    if (fgGlobalMorph)
    {
        // FIELD_ADDR sequences may collapse to nothing, deal with them first so that
        // we can continue as if they didn't exist in the first place.
        if (GenTreeFieldAddr* field = tree->IsFieldAddr())
        {
            tree = fgMorphFieldAddr(field, mac);
        }

        // INDEX_ADDR cannot just collapse to nothing but let's handle it in similar
        // manner for the sake of consistency. These nodes are not supposed to appear
        // after global morph.
        if (GenTreeIndexAddr* index = tree->IsIndexAddr())
        {
            if (!opts.MinOpts())
            {
                tree = fgMorphIndexAddr(index);
            }
        }

        if (tree->IsIndir())
        {
            // An indirection gets a default address context if it isn't address taken.
            mac = nullptr;
        }

        /* Ensure that we haven't morphed this node already */
        assert(((tree->gtDebugFlags & GTF_DEBUG_NODE_MORPHED) == 0) && "ERROR: Already morphed this node!");

#if LOCAL_ASSERTION_PROP
        if (morphAssertionCount != 0)
        {
            tree = morphAssertionPropagate(tree);
        }
#endif
    }

    // Save the original un-morphed tree for fgMorphTreeDone.
    GenTree*    oldTree      = tree;
    BasicBlock* currentBlock = fgMorphBlock;

    unsigned kind = tree->OperKind();

    if (kind & GTK_LEAF)
    {
        if (tree->IsNumericConst())
        {
            // Clear any exception flags or other unnecessary flags that
            // may have been set before folding this node to a constant.
            tree->gtFlags &= ~(GTF_ALL_EFFECT | GTF_REVERSE_OPS);
        }
        else if (GenTreeStrCon* str = tree->IsStrCon())
        {
            tree = fgMorphTree(fgMorphStrCon(str, fgGlobalMorphStmt, currentBlock));
        }
        else
        {
            tree = fgMorphLeaf(tree);
        }

        goto DONE;
    }

    if (kind & GTK_SMPOP)
    {
        tree = fgMorphSmpOp(tree, mac);
        goto DONE;
    }

    switch (tree->GetOper())
    {
        case GT_QMARK:
            tree = fgMorphQmark(tree->AsQmark(), mac);
            break;

        case GT_CALL:
            if (tree->CallMayThrow(this))
            {
                tree->gtFlags |= GTF_EXCEPT;
            }
            else
            {
                tree->gtFlags &= ~GTF_EXCEPT;
            }
            tree = fgMorphCall(tree->AsCall(), fgGlobalMorphStmt);
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
                fgGetThrowHelperBlock(ThrowHelperKind::IndexOutOfRange, currentBlock);
            }
            break;

        case GT_PHI:
            tree->gtFlags &= ~GTF_ALL_EFFECT;
            for (GenTreePhi::Use& use : tree->AsPhi()->Uses())
            {
                use.SetNode(use.GetNode());
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

        case GT_ARR_OFFSET:
        case GT_CMPXCHG:
        case GT_COPY_BLK:
        case GT_INIT_BLK:
            tree->AsTernaryOp()->SetOp(0, fgMorphTree(tree->AsTernaryOp()->GetOp(0)));
            tree->AsTernaryOp()->SetOp(1, fgMorphTree(tree->AsTernaryOp()->GetOp(1)));
            tree->AsTernaryOp()->SetOp(2, fgMorphTree(tree->AsTernaryOp()->GetOp(2)));

            if (tree->OperIs(GT_ARR_OFFSET))
            {
                // GT_ARR_OFFSET nodes are created during lowering.
                noway_assert(!fgGlobalMorph);

                tree->gtFlags &= ~GTF_CALL;
            }
            else if (tree->OperIs(GT_CMPXCHG))
            {
                tree->gtFlags &= (~GTF_EXCEPT & ~GTF_CALL);
            }
            else
            {
                tree = fgMorphDynBlk(tree->AsDynBlk());

                if (!tree->IsDynBlk())
                {
                    goto DONE;
                }

                tree->gtFlags &= ~GTF_CALL;

                if (!tree->AsDynBlk()->GetSize()->IsIntegralConst(0))
                {
                    tree->gtFlags |= GTF_EXCEPT;
                }
            }

            tree->gtFlags |= tree->AsTernaryOp()->GetOp(0)->GetSideEffects();
            tree->gtFlags |= tree->AsTernaryOp()->GetOp(1)->GetSideEffects();
            tree->gtFlags |= tree->AsTernaryOp()->GetOp(2)->GetSideEffects();
            break;

        default:
            INDEBUG(gtDispTree(tree);)
            noway_assert(!"unexpected operator");
    }
DONE:

    fgMorphTreeDone(tree, oldTree DEBUGARG(thisMorphNum));

    return tree;
}

/*****************************************************************************
 *
 *  This function is called to complete the morphing of a tree node
 *  It should only be called once for each node.
 *  If DEBUG is defined the flag GTF_DEBUG_NODE_MORPHED is checked and updated,
 *  to enforce the invariant that each node is only morphed once.
 *
 */

void Compiler::fgMorphTreeDone(GenTree* tree,
                               GenTree* oldTree /* == NULL */
                               DEBUGARG(int morphNum))
{
#ifdef DEBUG
    if (verbose && JitConfig.TreesBeforeAfterMorph())
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

    // The way global morph tries to prevent double/lack of morphing of a node
    // is rather convoluted. The overall idea is that fgMorphTree is called on
    // `tree` and:
    //   - it modifies the tree node itself (e.g. by changing its type or even
    //     its oper), but does not create new nodes and returns the same `tree`.
    //     Then fgMorphTreeDone sets GTF_DEBUG_NODE_MORPHED on `tree` (and does
    //     expect that it hasn't been set).
    //   - it inserts new nodes (typically as `tree` operands) but still returns
    //     the same `tree`.
    //     For `tree` this works as in the previous case.
    //     For inserted nodes the morphing code is more or less expected to call
    //     fgMorphTreeDone, at least when nodes may affect assertion propagation
    //     (e.g. new assignment node, new indir node etc.). Failure to do so is
    //     not immediately detected, but can later result in asserts if other
    //     nodes are removed such that these new nodes reach fgMorphTreeDone.
    //   - it returns a new node and possibly drops `tree` on the floor.
    //     Here things are a bit bizarre - fgMorphTreeDone expects that the new
    //     node has GTF_DEBUG_NODE_MORPHED already set. Not clear why, perhaps
    //     the idea was that if you create a new node you should also pass it to
    //     fgMorphTree. But if you create a new node in morph then it is likely
    //     that it can be considered to already be morphed. In any case, it's
    //     fine for the morphing code to simply set GTF_DEBUG_NODE_MORPHED on
    //     the new node. It should not call fgMorphTreeDone though, that's done
    //     anyway in fgMorphTree.
    //     If the original `tree` is not dropped then fgMorphTreeDone should be
    //     called on it too, unless morphing code is certain that `tree` does
    //     not influence assertion propagation.
    //   - it simply drops `tree` on the floor and returns a descendant node.
    //     Such a node should have been morphed and have GTF_DEBUG_NODE_MORPHED
    //     set so this will work fine.
    //     However, it can result in double assertion generation, which should
    //     be harmless but may have some throughput cost.
    // Note that removing nodes from the tree after it has been morphed is not
    // tracked in any way, even though node removal could theoretically affect
    // assertion propagation. But this should be impossible, assignments and
    // indirections are side effects and morph does not have enough information
    // to remove them, except in the "expression always throws" case. And then
    // assertion propagation is no longer relevant since the rest of the code
    // in the basic block will also be removed.

    if ((oldTree != nullptr) && (oldTree != tree))
    {
        assert(((tree->gtDebugFlags & GTF_DEBUG_NODE_MORPHED) != 0) && "ERROR: Did not morph this node!");
    }
    else
    {
        assert(((tree->gtDebugFlags & GTF_DEBUG_NODE_MORPHED) == 0) && "ERROR: Already morphed this node!");
    }

#if LOCAL_ASSERTION_PROP
    if (!tree->OperIsConst() && (morphAssertionTable != nullptr))
    {
        if (morphAssertionCount != 0)
        {
            if (tree->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
            {
                morphAssertionKill(tree->AsLclVarCommon()->GetLcl() DEBUGARG(tree->AsOp()));
            }
        }

        morphAssertionGenerate(tree);
    }
#endif // LOCAL_ASSERTION_PROP

    INDEBUG(tree->gtDebugFlags |= GTF_DEBUG_NODE_MORPHED);
}

// Check and fold blocks of type BBJ_COND and BBJ_SWITCH on constants
// Returns true if we modified the flow graph
bool Compiler::fgFoldConditional(BasicBlock* block)
{
    assert(opts.OptimizationEnabled());

    if (block->bbJumpKind == BBJ_COND)
    {
        noway_assert((block->bbStmtList != nullptr) && (block->bbStmtList->GetPrevStmt() != nullptr));
        Statement* lastStmt = block->GetLastStatement();
        noway_assert(lastStmt->GetNextStmt() == nullptr);
        noway_assert(lastStmt->GetRootNode()->OperIs(GT_JTRUE));

        GenTree* condTree = lastStmt->GetRootNode()->AsUnOp()->GetOp(0);
        GenTree* cond     = condTree->SkipComma();

        if (!cond->IsIntCon())
        {
            return false;
        }

        noway_assert((block->bbNext->countOfInEdges() > 0) && (block->bbJumpDest->countOfInEdges() > 0));

        if (condTree != cond)
        {
            assert(condTree->OperIs(GT_COMMA));
            lastStmt->SetRootNode(condTree);
        }
        else
        {
            fgRemoveStmt(block, lastStmt);
        }

        BasicBlock* bTaken;
        BasicBlock* bNotTaken;

        if (cond->AsIntCon()->GetValue() != 0)
        {
            block->bbJumpKind = BBJ_ALWAYS;
            bTaken            = block->bbJumpDest;
            bNotTaken         = block->bbNext;
        }
        else
        {
            // Unmark the loop if we are removing a backwards branch
            // dest block must also be marked as a loop head and
            // We must be able to reach the backedge block
            if ((block->bbJumpDest->isLoopHead()) && (block->bbJumpDest->bbNum <= block->bbNum) &&
                fgReachable(block->bbJumpDest, block))
            {
                optUnmarkLoopBlocks(block->bbJumpDest, block);
            }

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

        fgRemoveRefPred(bNotTaken, block);

#ifdef DEBUG
        if (verbose)
        {
            printf("\nConditional folded at " FMT_BB "\n", block->bbNum);
            printf(FMT_BB " becomes a %s", block->bbNum, block->bbJumpKind == BBJ_ALWAYS ? "BBJ_ALWAYS" : "BBJ_NONE");
            if (block->bbJumpKind == BBJ_ALWAYS)
            {
                printf(" to " FMT_BB, block->bbJumpDest->bbNum);
            }
            printf("\n");
        }
#endif

        // If the block was a loop condition we may have to modify the loop table.
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

        return true;
    }

    if (block->bbJumpKind == BBJ_SWITCH)
    {
        noway_assert((block->bbStmtList != nullptr) && (block->bbStmtList->GetPrevStmt() != nullptr));
        Statement* lastStmt = block->GetLastStatement();
        noway_assert(lastStmt->GetNextStmt() == nullptr);
        noway_assert(lastStmt->GetRootNode()->OperIs(GT_SWITCH));

        GenTree* condTree = lastStmt->GetRootNode()->AsUnOp()->GetOp(0);
        GenTree* cond     = condTree->SkipComma();

        if (!cond->IsIntCon())
        {
            return false;
        }

        if (condTree != cond)
        {
            assert(condTree->OperIs(GT_COMMA));
            lastStmt->SetRootNode(condTree);
        }
        else
        {
            fgRemoveStmt(block, lastStmt);
        }

        unsigned const     switchVal = cond->AsIntCon()->GetUInt32Value();
        unsigned const     jumpCnt   = block->bbJumpSwt->bbsCount;
        BasicBlock** const jumpTab   = block->bbJumpSwt->bbsDstTab;
        bool               foundVal  = false;

        for (unsigned val = 0; val < jumpCnt; val++)
        {
            BasicBlock* curJump = jumpTab[val];

            assert(curJump->countOfInEdges() > 0);

            // If val matches switchVal or we are at the last entry and
            // we never found the switch value then set the new jump dest

            if ((val == switchVal) || (!foundVal && (val == jumpCnt - 1)))
            {
                if (curJump != block->bbNext)
                {
                    block->bbJumpKind = BBJ_ALWAYS;
                    block->bbJumpDest = curJump;
                }
                else
                {
                    block->bbJumpKind = BBJ_NONE;
                }

                foundVal = true;
            }
            else
            {
                fgRemoveRefPred(curJump, block);
            }
        }

#ifdef DEBUG
        if (verbose)
        {
            printf("\nConditional folded at " FMT_BB "\n", block->bbNum);
            printf(FMT_BB " becomes a %s", block->bbNum, block->bbJumpKind == BBJ_ALWAYS ? "BBJ_ALWAYS" : "BBJ_NONE");
            if (block->bbJumpKind == BBJ_ALWAYS)
            {
                printf(" to " FMT_BB, block->bbJumpDest->bbNum);
            }
            printf("\n");
        }
#endif

        return true;
    }

    return false;
}

GenTreeCall* Compiler::fgIsThrow(GenTree* tree)
{
    return tree->IsHelperCall() && s_helperCallProperties.AlwaysThrow(eeGetHelperNum(tree->AsCall()->GetMethodHandle()))
               ? tree->AsCall()
               : nullptr;
}

GenTreeOp* Compiler::fgIsCommaThrow(GenTree* tree DEBUGARG(bool forFolding))
{
#ifdef DEBUG
    if (forFolding && compStressCompile(STRESS_FOLD, 50))
    {
        return nullptr;
    }
#endif

    return tree->OperIs(GT_COMMA) && tree->HasAllSideEffects(GTF_CALL | GTF_EXCEPT) && fgIsThrow(tree->AsOp()->GetOp(0))
               ? tree->AsOp()
               : nullptr;
}

bool Compiler::fgMorphRemoveUselessStmt(BasicBlock* block, Statement* stmt)
{
    if (opts.compDbgCode)
    {
        return false;
    }

    GenTree* tree = stmt->GetRootNode();

    if (tree->IsControlFlow() || tree->OperIsHWIntrinsic() || tree->OperIs(GT_NO_OP))
    {
        return false;
    }

    if (tree->HasAnySideEffect(GTF_SIDE_EFFECT))
    {
        return false;
    }

    fgRemoveStmt(block, stmt);

    return true;
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
    assert(!csePhase);

    fgRemoveRestOfBlock = false;
    fgMorphBlock        = block;

    GenTree* morph = fgMorphTree(stmt->GetRootNode());

    if (GenTreeOp* comma = fgIsCommaThrow(morph DEBUGARG(true)))
    {
        JITDUMPTREE(comma->GetOp(1), "Removing unreachable tree from COMMA throw:\n");
        morph               = comma->GetOp(0)->AsCall();
        fgRemoveRestOfBlock = true;
    }
    else if (fgIsThrow(morph))
    {
        fgRemoveRestOfBlock = true;
    }

    stmt->SetRootNode(morph);

    bool removedStmt = fgMorphRemoveUselessStmt(block, stmt);

    JITDUMPTREE(morph, "%s %s tree:\n", msg, removedStmt ? "removed" : "morphed");

    if (fgRemoveRestOfBlock)
    {
        JITDUMP("\n%s Block " FMT_BB " becomes a throw block.\n", msg, block->bbNum);

        fgRemoveRestOfBlock = false;

        for (Statement* removeStmt : StatementList(stmt->GetNextStmt()))
        {
            fgRemoveStmt(block, removeStmt);
        }

        // The rest of block has been removed and we will always throw an exception.
        // For compDbgCode, we prepend an empty BB as the firstBB, it is BBJ_NONE.
        // We should not convert it to a BBJ_THROW.

        if ((block != fgFirstBB) || ((fgFirstBB->bbFlags & BBF_INTERNAL) == 0))
        {
            fgConvertBBToThrowBB(block);
        }
    }
    else if (morph->OperIs(GT_JTRUE, GT_SWITCH) && opts.OptimizationEnabled())
    {
        assert(!removedStmt);
        removedStmt = fgFoldConditional(block);
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
    fgMorphBlock        = block;

    for (Statement* const stmt : block->Statements())
    {
        if (fgRemoveRestOfBlock)
        {
            fgRemoveStmt(block, stmt);
            continue;
        }

        fgGlobalMorphStmt = stmt;

#ifdef DEBUG
        unsigned oldHash = 0;

        if (verbose)
        {
            oldHash = gtHashValue(stmt->GetRootNode());
            printf("\nfgMorphTree " FMT_BB ", " FMT_STMT " (before)\n", block->bbNum, stmt->GetID());
            gtDispTree(stmt->GetRootNode());
        }
#endif

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64) || defined(TARGET_X86)
        fgMorphIndirectParams(stmt);
#endif

        GenTree* oldTree     = stmt->GetRootNode();
        GenTree* morphedTree = fgMorphTree(oldTree);

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64) || defined(TARGET_ARM)
        // Mark any outgoing arg temps as free so we can reuse them in the next statement.
        abiFreeAllStructArgTemps();
#endif

        BasicBlock* currentBlock = fgMorphBlock;

        if ((stmt->GetRootNode() != oldTree) || (block != currentBlock))
        {
            if (stmt->GetRootNode() != oldTree)
            {
                // This must be tailcall. Ignore 'morphedTree' and carry on with
                // the tail-call node
                morphedTree = stmt->GetRootNode();
            }
            else
            {
                // This must be a tailcall that caused a GCPoll to get
                // injected. We haven't actually morphed the call yet
                // but the flag still got set, clear it here...
                INDEBUG(morphedTree->gtDebugFlags &= ~GTF_DEBUG_NODE_MORPHED);
            }

            noway_assert(compTailCallUsed);
            noway_assert(morphedTree->OperIs(GT_CALL));

            GenTreeCall* call = morphedTree->AsCall();
            // Could be
            //   - a fast call made as jmp in which case block will be ending with
            //   BBJ_RETURN (as we need epilog) and marked as containing a jmp.
            //   - a tailcall dispatched via JIT helper, on x86, in which case
            //   block will be ending with BBJ_THROW.
            //   - a tail call dispatched via runtime help (IL stubs), in which
            //   case there will not be any tailcall and the block will be ending
            //   with BBJ_RETURN (as normal control flow)
            noway_assert((call->IsFastTailCall() && (currentBlock->bbJumpKind == BBJ_RETURN) &&
                          ((currentBlock->bbFlags & BBF_HAS_JMP)) != 0) ||
                         X86_ONLY((call->IsTailCallViaJitHelper() && (currentBlock->bbJumpKind == BBJ_THROW)) ||)(
                             !call->IsTailCall() && (currentBlock->bbJumpKind == BBJ_RETURN)));
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
            if (gtHashValue(morphedTree) != oldHash)
            {
                printf("\nfgMorphTree " FMT_BB ", " FMT_STMT " (after)\n", block->bbNum, stmt->GetID());
                gtDispTree(morphedTree);
            }
        }
#endif

        if (GenTreeOp* comma = fgIsCommaThrow(morphedTree DEBUGARG(true)))
        {
            JITDUMPTREE(comma->GetOp(1), "Removing unreachable tree from COMMA throw:\n");
            morphedTree         = comma->GetOp(0)->AsCall();
            fgRemoveRestOfBlock = true;
        }

        stmt->SetRootNode(morphedTree);

        if (fgRemoveRestOfBlock)
        {
            continue;
        }

        /* Has the statement been optimized away */

        if (fgMorphRemoveUselessStmt(block, stmt))
        {
            continue;
        }
    }

    if (fgRemoveRestOfBlock)
    {
        fgConvertBBToThrowBB(block);
    }
    else if (block->KindIs(BBJ_COND, BBJ_SWITCH) && opts.OptimizationEnabled())
    {
        fgFoldConditional(block);
    }

#if FEATURE_FASTTAILCALL
    if (GenTreeCall* recursiveTailCall = block->EndsWithTailCallConvertibleToLoop(this))
    {
        fgMorphRecursiveFastTailCallIntoLoop(block, recursiveTailCall);
    }
#endif

    // Reset this back so that it doesn't leak out impacting other blocks
    fgRemoveRestOfBlock = false;
}

void Compiler::phMorph()
{
    unsigned prevBBCount = fgBBcount;

    // Since fgMorphTree can be called after various optimizations to re-arrange
    // the nodes we need a global flag to signal if we are during the one-pass
    // global morphing.
    fgGlobalMorph = true;

    fgMorphBlocks();

    // We are done with the global morphing phase
    fgGlobalMorph     = false;
    fgGlobalMorphStmt = nullptr;
    fgMorphBlock      = nullptr;

#if (defined(TARGET_AMD64) && !defined(UNIX_AMD64_ABI)) || defined(TARGET_ARM64)
    // Fix any LclVar annotations on discarded struct promotion temps for implicit by-ref params
    lvaDemoteImplicitByRefParams();
    lvaRefCountState = RCS_INVALID;
#endif

#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
    if (fgNeedToAddFinallyTargetBits)
    {
        // We previously wiped out the BBF_FINALLY_TARGET bits due to some morphing; add them back.
        fgAddFinallyTargetFlags();
        fgNeedToAddFinallyTargetBits = false;
    }
#endif // defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)

    fgExpandQmarkNodes();

    // If we needed to create any new BasicBlocks then renumber the blocks
    if (fgBBcount > prevBBCount)
    {
        fgRenumberBlocks();
    }

    // We can now enable all phase checking
    activePhaseChecks = PhaseChecks::CHECK_ALL;
}

void Compiler::fgMorphBlocks()
{
    JITDUMP("\n*************** In fgMorphBlocks()\n");

#if LOCAL_ASSERTION_PROP
    morphAssertionInit();
#endif

    if (!compEnregLocals())
    {
        // Morph is checking if lvDoNotEnregister is already set for some optimizations.
        // If we are running without `CLFLG_REGVAR` flag set (`compEnregLocals() == false`)
        // then we already know that we won't enregister any locals and it is better to set
        // this flag before we start reading it.
        // The main reason why this flag is not set is that we are running in minOpts.
        lvSetMinOptsDoNotEnreg();
    }

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
        if (morphAssertionCount != 0)
        {
            // Clear out the assertion table before processing each basic block.
            morphAssertionSetCount(0);
        }
#endif

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

#if LOCAL_ASSERTION_PROP
    morphAssertionDone();
#endif

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

        LclVarDsc* lcl = lvaGetDesc(genReturnLocal);

        if (!varTypeIsStruct(lcl->GetType()))
        {
            lastStmt->SetRootNode(gtNewStoreLclVar(lcl, lcl->GetType(), value));
        }
        else if (lcl->IsPromoted() && !value->TypeIs(TYP_STRUCT))
        {
            assert(lcl->GetPromotedFieldCount() == 1);

            lcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(0));
            lastStmt->SetRootNode(gtNewStoreLclVar(lcl, lcl->GetType(), value));
        }
        else if (lcl->TypeIs(TYP_STRUCT) && !value->TypeIs(TYP_STRUCT))
        {
            GenTreeLclFld* store = gtNewStoreLclFld(value->GetType(), lcl, 0, value);
            lastStmt->SetRootNode(store);
        }
        else
        {
            GenTreeOp* asg = gtNewAssignNode(gtNewLclvNode(lcl, lcl->GetType()), value);
            lastStmt->SetRootNode(fgMorphStructAssignment(asg));
        }
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
    GenTreeQmark*  topQmark   = fgGetTopLevelQmark(expr, &destLclVar);

    // If the top level Qmark is null, then scan the tree to make sure
    // there are no qmarks within it.
    if (topQmark == nullptr)
    {
        fgWalkTreePre(&expr, fgAssertNoQmark);
    }
    else
    {
        // We could probably expand the cond node also, but don't think the extra effort is necessary,
        // so let's just assert the cond node of a top level qmark doesn't have further top level qmarks.
        fgWalkTreePre(&topQmark->gtOp1, fgAssertNoQmark);
        fgPreExpandQmarkChecks(topQmark->gtOp2);
        fgPreExpandQmarkChecks(topQmark->gtOp3);
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

    if (expr->OperIs(GT_STORE_LCL_VAR) && expr->AsLclVar()->GetOp(0)->IsQmark())
    {
        *destLclVar = expr->AsLclVar();
        return expr->AsLclVar()->GetOp(0)->AsQmark();
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

    GenTree* condExpr  = qmark->GetCondition();
    GenTree* trueExpr  = qmark->GetThen();
    GenTree* falseExpr = qmark->GetElse();

    // Get cond, true, false exprs for the nested qmark.
    GenTree* nestedQmark = falseExpr;
    GenTree* cond2Expr;
    GenTree* true2Expr;
    GenTree* false2Expr;

    if (nestedQmark->gtOper == GT_QMARK)
    {
        cond2Expr  = nestedQmark->AsQmark()->GetCondition();
        true2Expr  = nestedQmark->AsQmark()->GetThen();
        false2Expr = nestedQmark->AsQmark()->GetElse();
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
    Statement* jmpStmt = gtNewStmt(jmpTree, stmt->GetILOffsetX());
    fgInsertStmtAtEnd(cond1Block, jmpStmt);

    // Append cond2 as JTRUE to cond2Block
    jmpTree = gtNewOperNode(GT_JTRUE, TYP_VOID, cond2Expr);
    jmpStmt = gtNewStmt(jmpTree, stmt->GetILOffsetX());
    fgInsertStmtAtEnd(cond2Block, jmpStmt);

    LclVarDsc* dstLcl  = dst->GetLcl();
    var_types  dstType = dstLcl->GetType();

    assert(varTypeIsI(dstType));

    // AsgBlock should get tmp = op1 assignment.
    trueExpr = gtNewStoreLclVar(dstLcl, dstType, trueExpr);
    fgInsertStmtAtEnd(asgBlock, gtNewStmt(trueExpr, stmt->GetILOffsetX()));

    // Since we are adding helper in the JTRUE false path, reverse the cond2 and add the helper.
    gtReverseCond(cond2Expr);

    if (true2Expr->IsCall() && true2Expr->AsCall()->IsNoReturn())
    {
        fgConvertBBToThrowBB(helperBlock);
    }
    else
    {
        true2Expr = gtNewStoreLclVar(dstLcl, dstType, true2Expr);
    }

    fgInsertStmtAtEnd(helperBlock, gtNewStmt(true2Expr, stmt->GetILOffsetX()));
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

    GenTree* condExpr  = qmark->GetCondition();
    GenTree* trueExpr  = qmark->GetThen();
    GenTree* falseExpr = qmark->GetElse();

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

    GenTree*   jmpTree = gtNewOperNode(GT_JTRUE, TYP_VOID, qmark->GetCondition());
    Statement* jmpStmt = gtNewStmt(jmpTree, stmt->GetILOffsetX());
    fgInsertStmtAtEnd(condBlock, jmpStmt);

    // Remove the original qmark statement.
    fgRemoveStmt(block, stmt);

    if (dst == nullptr)
    {
        assert(qmark->TypeIs(TYP_VOID));
    }
    else
    {
        LclVarDsc* lcl     = dst->GetLcl();
        var_types  lclType = lcl->GetType();

        // Other non-struct types should work but such QMARKs are never generated by
        // the JIT so this cannot be tested. Small int types might have issues.

        assert(varTypeIsI(lclType));
        assert(lclType == qmark->GetType());

        if (hasTrueExpr)
        {
            trueExpr = gtNewStoreLclVar(lcl, lclType, trueExpr);
        }

        if (hasFalseExpr)
        {
            falseExpr = gtNewStoreLclVar(lcl, lclType, falseExpr);
        }
    }

    if (hasTrueExpr)
    {
        fgInsertStmtAtEnd(thenBlock, gtNewStmt(trueExpr, stmt->GetILOffsetX()));
    }

    if (hasFalseExpr)
    {
        fgInsertStmtAtEnd(elseBlock, gtNewStmt(falseExpr, stmt->GetILOffsetX()));
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
        for (BasicBlock* const block : Blocks())
        {
            for (Statement* const stmt : block->Statements())
            {
                GenTree* expr = stmt->GetRootNode();
                INDEBUG(fgPreExpandQmarkChecks(expr);)
                fgExpandQmarkStmt(block, stmt);
            }
        }

        INDEBUG(fgPostExpandQmarkChecks();)
    }

    INDEBUG(compQmarkRationalized = true;)
}

#ifdef DEBUG
/*****************************************************************************
 *
 *  Make sure we don't have any more GT_QMARK nodes.
 *
 */
void Compiler::fgPostExpandQmarkChecks()
{
    for (BasicBlock* const block : Blocks())
    {
        for (Statement* const stmt : block->Statements())
        {
            GenTree* expr = stmt->GetRootNode();
            fgWalkTreePre(&expr, fgAssertNoQmark);
        }
    }
}
#endif

FieldSeqNode* Compiler::GetZeroOffsetFieldSeq(GenTree* node)
{
    if (m_zeroOffsetFieldMap == nullptr)
    {
        return nullptr;
    }

    FieldSeqNode** fieldSeq = m_zeroOffsetFieldMap->LookupPointer(node);
    return fieldSeq == nullptr ? nullptr : *fieldSeq;
}

void Compiler::CopyZeroOffsetFieldSeq(GenTree* from, GenTree* to)
{
    if (FieldSeqNode* fieldSeq = GetZeroOffsetFieldSeq(from))
    {
        m_zeroOffsetFieldMap->Set(to, fieldSeq);
    }
}

void Compiler::AddZeroOffsetFieldSeq(GenTree* addr, FieldSeqNode* fieldSeq)
{
    assert(varTypeIsI(addr->GetType()));
    addr = addr->SkipComma();
    assert(varTypeIsI(addr->GetType()));
    assert(fieldSeq != nullptr);

#ifdef DEBUG
    if (verbose)
    {
        printf("\nAddZeroOffsetFieldSeq ");
        dmpFieldSeqFields(fieldSeq);
        printf(" to address\n");
        gtDispTree(addr, true, false);
    }
#endif

    if (GenTreeIntCon* intCon = addr->IsIntCon())
    {
        intCon->SetFieldSeq(GetFieldSeqStore()->Append(intCon->GetFieldSeq(), fieldSeq));
        return;
    }

    if (addr->OperIs(GT_ADD))
    {
        GenTree* op1 = addr->AsOp()->GetOp(0);
        GenTree* op2 = addr->AsOp()->GetOp(1);

        if (op1->IsIntCon())
        {
            std::swap(op1, op2);
        }

        if (GenTreeIntCon* intCon = op2->IsIntCon())
        {
            intCon->SetFieldSeq(GetFieldSeqStore()->Append(intCon->GetFieldSeq(), fieldSeq));
            return;
        }
    }

    // The "addr" node might already be annotated with a zero-offset field sequence.
    if (FieldSeqNode* existingFieldSeq = GetZeroOffsetFieldSeq(addr))
    {
        fieldSeq = GetFieldSeqStore()->Append(existingFieldSeq, fieldSeq);
    }

    if (m_zeroOffsetFieldMap == nullptr)
    {
        CompAllocator alloc(getAllocator(CMK_ZeroOffsetFieldMap));
        m_zeroOffsetFieldMap = new (alloc) NodeToFieldSeqMap(alloc);
    }

    m_zeroOffsetFieldMap->Set(addr, fieldSeq, NodeToFieldSeqMap::Overwrite);
}

//------------------------------------------------------------------------
// fgCheckStmtAfterTailCall: check that statements after the tail call stmt
// candidate are in one of expected forms, that are desctibed below.
//
// Return Value:
//    'true' if stmts are in the expected form, else 'false'.
//
bool Compiler::fgCheckStmtAfterTailCall(Statement* callStmt)
{
    // For void calls, we would have created a GT_CALL in the stmt list.
    // For non-void calls, we would have created a GT_RETURN(GT_CAST(GT_CALL)).
    // For calls returning structs, we would have a void call, followed by a void return.
    // For debuggable code, it would be an assignment of the call to a temp
    // We want to get rid of any of this extra trees, and just leave
    // the call.

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
            noway_assert(callExpr->gtGetOp1()->OperIs(GT_LCL_VAR));
            LclVarDsc* callResultLcl = callExpr->gtGetOp1()->AsLclVar()->GetLcl();

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
                noway_assert(moveDest->OperIs(GT_LCL_VAR));

                // Tunnel through any casts on the source side.
                GenTree* moveSource = moveExpr->gtGetOp2();
                while (moveSource->OperIs(GT_CAST))
                {
                    noway_assert(!moveSource->gtOverflow());
                    moveSource = moveSource->gtGetOp1();
                }
                noway_assert(moveSource->OperIs(GT_LCL_VAR));

                // Verify we're just passing the value from one local to another
                // along the chain.
                noway_assert(moveSource->AsLclVar()->GetLcl() == callResultLcl);
                callResultLcl = moveDest->AsLclVar()->GetLcl();

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

                noway_assert(treeWithLcl->OperIs(GT_LCL_VAR) && (callResultLcl == treeWithLcl->AsLclVar()->GetLcl()));

                nextMorphStmt = retStmt->GetNextStmt();
            }
        }
    }
    return nextMorphStmt == nullptr;
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
