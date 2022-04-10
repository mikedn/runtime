// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                           Lowering for AMD64, x86                         XX
XX                                                                           XX
XX  This encapsulates all the logic for lowering trees for the AMD64         XX
XX  architecture.  For a more detailed view of what is lowering, please      XX
XX  take a look at Lower.cpp                                                 XX
XX                                                                           XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#ifdef TARGET_XARCH // This file is only used for xarch

#include "jit.h"
#include "sideeffects.h"
#include "lower.h"

// xarch supports both ROL and ROR instructions so no lowering is required.
void Lowering::LowerRotate(GenTree* tree)
{
    ContainCheckShiftRotate(tree->AsOp());
}

void Lowering::LowerStoreLclVarArch(GenTreeLclVar* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR));

    GenTree* src = store->GetOp(0);

    if (src->OperIs(GT_CNS_INT))
    {
        GenTreeIntCon* con  = src->AsIntCon();
        ssize_t        ival = con->gtIconVal;

        unsigned   varNum = store->GetLclNum();
        LclVarDsc* varDsc = comp->lvaTable + varNum;

        unsigned size = genTypeSize(store);
        // If we are storing a constant into a local variable
        // we extend the size of the store here
        if ((size < 4) && !varTypeIsStruct(varDsc))
        {
            if (!varTypeIsUnsigned(varDsc))
            {
                if (genTypeSize(store) == 1)
                {
                    if ((ival & 0x7f) != ival)
                    {
                        ival = ival | 0xffffff00;
                    }
                }
                else
                {
                    assert(genTypeSize(store) == 2);
                    if ((ival & 0x7fff) != ival)
                    {
                        ival = ival | 0xffff0000;
                    }
                }
            }

            // A local stack slot is at least 4 bytes in size, regardless of
            // what the local var is typed as, so auto-promote it here
            // unless it is a field of a promoted struct
            // TODO-XArch-CQ: if the field is promoted shouldn't we also be able to do this?
            if (!varDsc->lvIsStructField)
            {
                store->gtType = TYP_INT;
                con->SetIconValue(ival);
            }
        }
    }

    ContainCheckStoreLcl(store);
}

void Lowering::LowerStoreIndirArch(GenTreeStoreInd* store)
{
    GenTree* value = store->GetValue();

    if (varTypeIsByte(store->GetType()) && (value->OperIsCompare() || value->OperIs(GT_SETCC)))
    {
        value->SetType(store->GetType());
    }
    if (GenTreeDblCon* dblCon = store->GetValue()->IsDblCon())
    {
        // Optimize *x = DCON to *x = ICON which is slightly faster on xarch

        assert(dblCon->GetType() == store->GetType());

        var_types type = TYP_UNDEF;
        ssize_t   bits = 0;

        if (dblCon->TypeIs(TYP_FLOAT))
        {
            type = TYP_INT;
            bits = static_cast<int32_t>(dblCon->GetFloatBits());
        }
#ifdef TARGET_AMD64
        else
        {
            assert(dblCon->TypeIs(TYP_DOUBLE));
            type = TYP_LONG;
            bits = static_cast<int64_t>(dblCon->GetDoubleBits());
        }
#endif

        if (type != TYP_UNDEF)
        {
            GenTree* intCon = dblCon;

            intCon->ChangeOperConst(GT_CNS_INT);
            intCon->SetType(type);
            intCon->AsIntCon()->SetValue(bits);
            store->SetType(type);
        }
    }

    ContainCheckStoreIndir(store);

    if (varTypeIsIntegralOrI(store->GetType()) && value->OperIsRMWMemOp() && !value->gtOverflowEx())
    {
        LowerStoreIndRMW(store);
    }
}

void Lowering::ContainStructStoreAddress(GenTree* store, unsigned size, GenTree* addr)
{
#if FEATURE_MULTIREG_RET
    assert(store->OperIs(GT_PUTARG_STK) || store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD) ||
           (store->OperIs(GT_STORE_BLK, GT_STORE_OBJ) && ((store->AsBlk()->GetKind() == StructStoreKind::UnrollInit) ||
                                                          (store->AsBlk()->GetKind() == StructStoreKind::UnrollCopy) ||
                                                          (store->AsBlk()->GetKind() == StructStoreKind::UnrollRegs))));
#else
    assert(store->OperIs(GT_PUTARG_STK) || store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD) ||
           (store->OperIs(GT_STORE_BLK, GT_STORE_OBJ) && ((store->AsBlk()->GetKind() == StructStoreKind::UnrollInit) ||
                                                          (store->AsBlk()->GetKind() == StructStoreKind::UnrollCopy))));
#endif

    assert(size < INT32_MAX);

    if (addr->OperIsLocalAddr())
    {
        addr->SetContained();
        return;
    }

    if (!addr->IsAddrMode() && !TryCreateAddrMode(addr, true))
    {
        return;
    }

    GenTreeAddrMode* addrMode = addr->AsAddrMode();

    // On x64 the address mode displacement is signed so it must not exceed INT32_MAX. This check is
    // an approximation since the last displacement we generate in an unrolled block operation can be
    // up to 16 bytes lower than offset + size. But offsets large enough to hit this case are likely
    // to be extremely rare for this to ever be a CQ issue.
    // On x86 this shouldn't be needed but then again, offsets large enough to hit this are rare.
    if (addrMode->GetOffset() > (INT32_MAX - static_cast<int>(size)))
    {
        return;
    }

#if defined(TARGET_X86) || defined(UNIX_AMD64_ABI)
    if (GenTreePutArgStk* putArg = store->IsPutArgStk())
    {
#if defined(TARGET_X86)
        if (putArg->GetKind() == GenTreePutArgStk::Kind::Push)
        {
            // Containing the address mode avoids generating an extra LEA instruction but may increase the size
            // of the load/store instructions due to extra SIB bytes and/or 32 bit displacements. Unlike Unroll,
            // Push places no upper bound on the size of the struct and anyway it requires more instructions
            // than Unroll because it copies only 4 bytes at a time. Besides, if we need to push a lot of slots
            // the cost of the extra LEA is likely to be irrelevant.

            if ((addrMode->HasIndex() && (size > 32)) || ((addrMode->GetOffset() > 128 - 16) && (size > 16)))
            {
                return;
            }
        }
#else
        if ((putArg->GetKind() == GenTreePutArgStk::Kind::GCUnroll) ||
            (putArg->GetKind() == GenTreePutArgStk::Kind::GCUnrollXMM))
        {
            // Like in the x86 PUSH case, do not contain in cases where unrolling isn't limited. Use a higher
            // size treshold as on x64 we copy 8 and even 16 bytes at a time. Not that RepInstr/RepInstr also
            // do unlimited unroll but unlike GCUnroll/GCUnrollXMM they use the address mode only once.

            if ((addrMode->HasIndex() && (size > 64)) || ((addrMode->GetOffset() > 128 - 32) && (size > 32)))
            {
                return;
            }
        }
#endif
    }
#endif

    // Note that the parentNode is always the store node, even if we're dealing with the source address.
    // The source address is not directly used by the store node but by an indirection that is always
    // contained.
    if (!IsSafeToContainMem(store, addrMode))
    {
        return;
    }

    addrMode->SetContained();
}

void Lowering::ContainStructStoreAddressUnrollRegsWB(GenTree* addr)
{
    if (!addr->OperIs(GT_ADD) || addr->gtOverflow())
    {
        return;
    }

    int offset;

    if (GenTreeIntCon* intCon = addr->AsOp()->GetOp(1)->IsIntCon())
    {
        if (intCon->GetValue() > INT32_MAX - TARGET_POINTER_SIZE)
        {
            return;
        }

        if (intCon->GetValue() < INT32_MIN)
        {
            return;
        }

        offset = intCon->GetInt32Value();

        BlockRange().Remove(intCon);
    }
    else
    {
        return;
    }

    addr->ChangeToAddrMode(addr->AsOp()->GetOp(0), nullptr, 1, offset);
    addr->SetContained();
}

void Lowering::LowerPutArgStk(GenTreePutArgStk* putArgStk)
{
    GenTree* src = putArgStk->GetOp(0);

    if (src->OperIs(GT_FIELD_LIST))
    {
#ifdef TARGET_X86
        GenTreeFieldList* fieldList = src->AsFieldList();

        // The code generator will push these fields in reverse order by offset. Reorder the list here s.t. the order
        // of uses is visible to LSRA.
        assert(fieldList->Uses().IsSorted());
        fieldList->Uses().Reverse();

        // Now that the fields have been sorted, the kind of code we will generate.
        bool     allFieldsAreSlots = true;
        unsigned prevOffset        = putArgStk->GetArgSize();
        for (GenTreeFieldList::Use& use : fieldList->Uses())
        {
            GenTree* const fieldNode   = use.GetNode();
            const unsigned fieldOffset = use.GetOffset();

            assert(!fieldNode->TypeIs(TYP_LONG));

            // We can treat as a slot any field that is stored at a slot boundary, where the previous
            // field is not in the same slot. (Note that we store the fields in reverse order.)
            const bool fieldIsSlot = ((fieldOffset % 4) == 0) && ((prevOffset - fieldOffset) >= 4);
            if (!fieldIsSlot)
            {
                allFieldsAreSlots = false;
            }

            // For x86 we must mark all integral fields as contained or reg-optional, and handle them
            // accordingly in code generation, since we may have up to 8 fields, which cannot all be in
            // registers to be consumed atomically by the call.
            if (varTypeIsIntegralOrI(fieldNode))
            {
                if (fieldNode->OperGet() == GT_LCL_VAR)
                {
                    LclVarDsc* varDsc = &(comp->lvaTable[fieldNode->AsLclVarCommon()->GetLclNum()]);
                    if (!varDsc->lvDoNotEnregister)
                    {
                        fieldNode->SetRegOptional();
                    }
                    else
                    {
                        MakeSrcContained(putArgStk, fieldNode);
                    }
                }
                else if (fieldNode->IsIntCnsFitsInI32())
                {
                    MakeSrcContained(putArgStk, fieldNode);
                }
                else
                {
                    // For the case where we cannot directly push the value, if we run out of registers,
                    // it would be better to defer computation until we are pushing the arguments rather
                    // than spilling, but this situation is not all that common, as most cases of promoted
                    // structs do not have a large number of fields, and of those most are lclVars or
                    // copy-propagated constants.
                    fieldNode->SetRegOptional();
                }
            }

            prevOffset = fieldOffset;
        }

        // Set the copy kind.
        // TODO-X86-CQ: Even if we are using push, if there are contiguous floating point fields, we should
        // adjust the stack once for those fields. The latter is really best done in code generation, but
        // this tuning should probably be undertaken as a whole.
        // Also, if there are  floating point fields, it may be better to use the "Unroll" mode
        // of copying the struct as a whole, if the fields are not register candidates.
        if (allFieldsAreSlots)
        {
            putArgStk->SetKind(GenTreePutArgStk::Kind::PushAllSlots);
        }
        else
        {
            putArgStk->SetKind(GenTreePutArgStk::Kind::Push);
        }
#endif // TARGET_X86
        return;
    }

#ifdef TARGET_X86
    if (src->IsMultiRegCall() && varTypeIsStruct(src->GetType()))
    {
        return;
    }
#endif

    if (src->TypeIs(TYP_STRUCT))
    {
        ClassLayout* layout;
        unsigned     size;

        if (src->OperIs(GT_LCL_VAR))
        {
            layout = comp->lvaGetDesc(src->AsLclVar())->GetLayout();
            size   = roundUp(layout->GetSize(), REGSIZE_BYTES);
        }
        else if (src->OperIs(GT_LCL_FLD))
        {
            layout = src->AsLclFld()->GetLayout(comp);
            size   = roundUp(layout->GetSize(), REGSIZE_BYTES);
        }
        else
        {
            layout = src->AsObj()->GetLayout();
            size   = layout->GetSize();
        }

        // In case of a CpBlk we could use a helper call. In case of putarg_stk we
        // can't do that since the helper call could kill some already set up outgoing args.
        // TODO-Amd64-Unix: converge the code for putarg_stk with cpyblk/cpyobj.
        // The cpyXXXX code is rather complex and this could cause it to be more complex, but
        // it might be the right thing to do.

        // TODO-X86-CQ: The helper call either is not supported on x86 or required more work
        // (I don't know which).

        if (!layout->HasGCPtr()
#ifdef TARGET_X86
            && (size != 8)
#endif
                )
        {
            putArgStk->SetKind(size <= CPBLK_UNROLL_LIMIT ? GenTreePutArgStk::Kind::Unroll
                                                          : GenTreePutArgStk::Kind::RepInstr);
        }
        else
        {
#ifdef TARGET_X86
            // On x86, we must use `push` to store GC references to the stack in order for the emitter to properly
            // update the function's GC info. These `putargstk` nodes will generate a sequence of `push` instructions.
            putArgStk->SetKind(GenTreePutArgStk::Kind::Push);
#else
            // On Linux-x64, any GC pointers the struct contains must be stored to the argument outgoing area using
            // MOV instructions that the emitter can recognize, e.g. "mov qword ptr [esp+8], rax". XMM stores or
            // "indirect" stores, including MOVSQ, cannot be used because the emitter wouldn't be able to figure
            // out which slot is being stored do.
            //
            // If the struct contains only GC pointers then we the only option is to generate a series of load/store
            // instructions, 2 MOVs for each GC pointer.
            //
            // If the struct also contains non-GC slots then we have more options:
            //   - same MOV load/store as for GC slots - starts at around 8 bytes of code for 8 bytes of data
            //     but can reach 16 bytes of code with 32 bit address mode displacements and SIB bytes.
            //   - XMM load/store - copies 16 bytes at once but also generates larger code, ~11 - 18 bytes
            //     depending on encoding and address modes.
            //   - REP MOVSQ - basically required for large copies. It's only 3 bytes but addresses and count
            //     have to be loaded in specific registers so a complete REP MOVSQ sequence can have ~16 - 21
            //     bytes of code.
            //   - Individual MOVSQ instructions. Like REP MOVSQ, it's very small, if addresses are already in
            //     the right registers.
            //
            // A previous implementation used (REP) MOVSQ for all non-GC slots but that generates horrible code:
            //   - If the struct contains only GC slots then the source and destination addresses are still
            //     loaded in RSI and RDI respectively, even if MOVSQ will never be used. In fact, RDI is loaded
            //     and not used at all since all GC stores use RSP instead.
            //   - When transitioning from a GC slot sequence to a non-GC slot sequence, RSI and RDI have to be
            //     adjusted to account for the already copied GC slots. This requires at least 8 bytes of code.
            //     Together with a single MOVSQ it's 10 bytes of code to copy 8 bytes of data. So the code may
            //     end up being larger than a simple MOV load/store, especially if the initial RSI/RDI setup is
            //     also taken into consideration.
            //   - The performance of MOVSQ is quite bad - throughput is only 0.25 and it wastes additional
            //     execution resources by adding 8 to RSI and RDI when normally such additions would be folded
            //     in address modes.
            //
            // As a compromise, continue to use MOVSQ for code size reasons, but with a few exceptions:
            //   - Copy single non-GC slot sequences using MOV.
            //   - Copy 2 non-GC slot sequences using XMM.
            //   - Do not use RDI/RSI/RCX temp registers in cases where (REP) MOVSQ isn't actually used.
            //
            // This results in smaller code, except in a few cases where large address mode displacements
            // and/or many transitions between GC and non-GC slot sequences make for larger code.
            //
            // TODO-MIKE-CQ: This mostly deals with code size issues seen in FX diffs, MOVSQ is still being
            // used to copy 3-4 non-GC slots and that probably has poor performance. And using REP MOVSQ
            // for more than 4 slots isn't great either.

            bool     hasXmmSequence      = false;
            bool     hasRepMovsSequence  = false;
            unsigned nonGCSequenceLength = 0;

            for (unsigned i = 0; i < layout->GetSlotCount(); i++)
            {
                if (layout->IsGCPtr(i))
                {
                    hasXmmSequence |= (nonGCSequenceLength == 2);
                    hasRepMovsSequence |= (nonGCSequenceLength > 2);
                    nonGCSequenceLength = 0;
                }
                else
                {
                    nonGCSequenceLength++;
                }
            }

            hasXmmSequence |= (nonGCSequenceLength == 2);
            hasRepMovsSequence |= (nonGCSequenceLength > 2);

            if (hasRepMovsSequence)
            {
                putArgStk->SetKind(hasXmmSequence ? GenTreePutArgStk::Kind::RepInstrXMM
                                                  : GenTreePutArgStk::Kind::RepInstr);
            }
            else
            {
                putArgStk->SetKind(hasXmmSequence ? GenTreePutArgStk::Kind::GCUnrollXMM
                                                  : GenTreePutArgStk::Kind::GCUnroll);
            }
#endif
        }

        if (src->OperIs(GT_OBJ))
        {
            ContainStructStoreAddress(putArgStk, size, src->AsObj()->GetAddr());
        }

        return;
    }

#ifdef WINDOWS_AMD64_ABI
    assert(putArgStk->GetSlotCount() == 1);
#else
    if (src->IsIntegralConst(0) && (putArgStk->GetSlotCount() > 1))
    {
        assert(comp->typIsLayoutNum(putArgStk->GetArgInfo()->GetSigTypeNum()));

        if (putArgStk->GetArgSize() > INITBLK_UNROLL_LIMIT)
        {
            putArgStk->SetKind(GenTreePutArgStk::Kind::RepInstrZero);
        }
        else
        {
            putArgStk->SetKind(GenTreePutArgStk::Kind::UnrollZero);
            src->SetContained();
        }

        return;
    }

#ifdef TARGET_64BIT
    if (src->IsIntegralConst(0) && comp->typIsLayoutNum(putArgStk->GetArgInfo()->GetSigTypeNum()))
    {
        ClassLayout* layout = comp->typGetLayoutByNum(putArgStk->GetArgInfo()->GetSigTypeNum());
        assert(layout->GetSize() <= REGSIZE_BYTES);

        if (layout->GetSize() > 4)
        {
            src->SetType(TYP_LONG);
        }

        return;
    }
#endif
#endif // !WINDOWS_AMD64_ABI

    // If the child of GT_PUTARG_STK is a constant, we don't need a register to
    // move it to memory (stack location).
    //
    // On AMD64, we don't want to make 0 contained, because we can generate smaller code
    // by zeroing a register and then storing it. E.g.:
    //      xor rdx, rdx
    //      mov gword ptr [rsp+28H], rdx
    // is 2 bytes smaller than:
    //      mov gword ptr [rsp+28H], 0
    //
    // On x86, we push stack arguments; we don't use 'mov'. So:
    //      push 0
    // is 1 byte smaller than:
    //      xor rdx, rdx
    //      push rdx

    if (IsContainableImmed(putArgStk, src)
#if defined(TARGET_AMD64)
        && !src->IsIntegralConst(0)
#endif // TARGET_AMD64
            )
    {
        src->SetContained();
    }
#if defined(TARGET_X86)
    else if (src->IsDblCon() && src->TypeIs(TYP_FLOAT))
    {
        float value = static_cast<float>(src->AsDblCon()->GetValue());
        src->ChangeOperConst(GT_CNS_INT);
        src->SetType(TYP_INT);
        src->AsIntCon()->SetValue(jitstd::bit_cast<int>(value));
        src->SetContained();
    }
    else
    {
        unsigned srcSize = varTypeSize(src->GetType());

        // For containment we need a slot sized memory operand - INT, FLOAT, REF, BYREF. Yes, it can be FLOAT
        // because it's a memory operation and the type doesn't really matter, only the size does.
        //
        // For reg optional things are a bit more complicated:
        //    - anything other than LCL_VAR can be reg-optional even if it's a small int type because the
        //      spilled value is really INT (e.g. ushort IND automatically zero extends to INT and the
        //      resulting value is spilled to an INT spill temp).
        //    - LCL_VAR must be slot sized because we don't know yet if the local will be a reg candidate.
        //      If it's not a reg candidate then it is treated as contained thus the size restriction.
        //      Note that the local itself may have small int type but if we get a LCL_VAR here then it
        //      means that it is "normalize on store" or that the frontend elided the normalization cast.
        //      Most LCL_VARs that reference small int local end up having type INT, with the notable
        //      exception of promoted struct field which may have small int type.

        if ((srcSize == REGSIZE_BYTES) && IsContainableMemoryOp(src) && IsSafeToContainMem(putArgStk, src))
        {
            src->SetContained();
        }
        else if (src->OperIs(GT_LCL_VAR) ? (srcSize == REGSIZE_BYTES) : (srcSize <= REGSIZE_BYTES))
        {
            src->SetRegOptional();
        }
    }
#endif
}

//------------------------------------------------------------------------
// Lowering::OptimizeConstCompare: Performs various "compare with const" optimizations.
//
// Arguments:
//    cmp - the compare node
//
// Return Value:
//    The original compare node if lowering should proceed as usual or the next node
//    to lower if the compare node was changed in such a way that lowering is no
//    longer needed.
//
// Notes:
//    - Narrow operands to enable memory operand containment (XARCH specific).
//    - Transform cmp(and(x, y), 0) into test(x, y) (XARCH/Arm64 specific but could
//      be used for ARM as well if support for GT_TEST_EQ/GT_TEST_NE is added).
//    - Transform TEST(x, LSH(1, y)) into BT(x, y) (XARCH specific)
//    - Transform RELOP(OP, 0) into SETCC(OP) or JCC(OP) if OP can set the
//      condition flags appropriately (XARCH/ARM64 specific but could be extended
//      to ARM32 as well if ARM32 codegen supports GTF_SET_FLAGS).
//
GenTree* Lowering::OptimizeConstCompare(GenTree* cmp)
{
    assert(cmp->gtGetOp2()->IsIntegralConst());

    GenTree*       op1      = cmp->gtGetOp1();
    GenTreeIntCon* op2      = cmp->gtGetOp2()->AsIntCon();
    ssize_t        op2Value = op2->IconValue();

    var_types op1Type = op1->TypeGet();
    if (IsContainableMemoryOp(op1) && varTypeIsSmall(op1Type) && genSmallTypeCanRepresentValue(op1Type, op2Value))
    {
        //
        // If op1's type is small then try to narrow op2 so it has the same type as op1.
        // Small types are usually used by memory loads and if both compare operands have
        // the same type then the memory load can be contained. In certain situations
        // (e.g "cmp ubyte, 200") we also get a smaller instruction encoding.
        //

        op2->gtType = op1Type;
    }
    else if (op1->OperIs(GT_CAST) && !op1->gtOverflow())
    {
        GenTreeCast* cast       = op1->AsCast();
        var_types    castToType = cast->CastToType();
        GenTree*     castOp     = cast->gtGetOp1();

        if (((castToType == TYP_BOOL) || (castToType == TYP_UBYTE)) && FitsIn<UINT8>(op2Value))
        {
            //
            // Since we're going to remove the cast we need to be able to narrow the cast operand
            // to the cast type. This can be done safely only for certain opers (e.g AND, OR, XOR).
            // Some opers just can't be narrowed (e.g DIV, MUL) while other could be narrowed but
            // doing so would produce incorrect results (e.g. RSZ, RSH).
            //
            // The below list of handled opers is conservative but enough to handle the most common
            // situations. In particular this include CALL, sometimes the JIT unnecessarilly widens
            // the result of bool returning calls.
            //
            bool removeCast =
                (castOp->OperIs(GT_CALL, GT_LCL_VAR) || castOp->OperIsLogical() || IsContainableMemoryOp(castOp));

            if (removeCast)
            {
                assert(!castOp->gtOverflowEx()); // Must not be an overflow checking operation

                castOp->SetType(castToType);
                op2->SetType(castToType);

                // If we have any contained memory ops on castOp, they must now not be contained.
                if (castOp->OperIsLogical())
                {
                    GenTree* op1 = castOp->gtGetOp1();
                    if ((op1 != nullptr) && !op1->IsCnsIntOrI())
                    {
                        op1->ClearContained();
                    }
                    GenTree* op2 = castOp->gtGetOp2();
                    if ((op2 != nullptr) && !op2->IsCnsIntOrI())
                    {
                        op2->ClearContained();
                    }
                }
                cmp->AsOp()->gtOp1 = castOp;

                BlockRange().Remove(cast);
            }
        }
    }
    else if (op1->OperIs(GT_AND) && cmp->OperIs(GT_EQ, GT_NE))
    {
        //
        // Transform ((x AND y) EQ|NE 0) into (x TEST_EQ|TEST_NE y) when possible.
        //

        GenTree* andOp1 = op1->gtGetOp1();
        GenTree* andOp2 = op1->gtGetOp2();

        if (op2Value != 0)
        {
            //
            // If we don't have a 0 compare we can get one by transforming ((x AND mask) EQ|NE mask)
            // into ((x AND mask) NE|EQ 0) when mask is a single bit.
            //

            if (isPow2<target_size_t>(static_cast<target_size_t>(op2Value)) && andOp2->IsIntegralConst(op2Value))
            {
                op2Value = 0;
                op2->SetIconValue(0);
                cmp->SetOperRaw(GenTree::ReverseRelop(cmp->OperGet()));
            }
        }

        if (op2Value == 0)
        {
            BlockRange().Remove(op1);
            BlockRange().Remove(op2);

            cmp->SetOperRaw(cmp->OperIs(GT_EQ) ? GT_TEST_EQ : GT_TEST_NE);
            cmp->AsOp()->gtOp1 = andOp1;
            cmp->AsOp()->gtOp2 = andOp2;
            // We will re-evaluate containment below
            andOp1->ClearContained();
            andOp2->ClearContained();

            if (IsContainableMemoryOp(andOp1) && andOp2->IsIntegralConst())
            {
                //
                // For "test" we only care about the bits that are set in the second operand (mask).
                // If the mask fits in a small type then we can narrow both operands to generate a "test"
                // instruction with a smaller encoding ("test" does not have a r/m32, imm8 form) and avoid
                // a widening load in some cases.
                //
                // For 16 bit operands we narrow only if the memory operand is already 16 bit. This matches
                // the behavior of a previous implementation and avoids adding more cases where we generate
                // 16 bit instructions that require a length changing prefix (0x66). These suffer from
                // significant decoder stalls on Intel CPUs.
                //
                // We could also do this for 64 bit masks that fit into 32 bit but it doesn't help.
                // In such cases morph narrows down the existing GT_AND by inserting a cast between it and
                // the memory operand so we'd need to add more code to recognize and eliminate that cast.
                //

                size_t mask = static_cast<size_t>(andOp2->AsIntCon()->IconValue());

                if (FitsIn<UINT8>(mask))
                {
                    andOp1->gtType = TYP_UBYTE;
                    andOp2->gtType = TYP_UBYTE;
                }
                else if (FitsIn<UINT16>(mask) && genTypeSize(andOp1) == 2)
                {
                    andOp1->gtType = TYP_USHORT;
                    andOp2->gtType = TYP_USHORT;
                }
            }
        }
    }

    if (cmp->OperIs(GT_TEST_EQ, GT_TEST_NE))
    {
        //
        // Transform TEST_EQ|NE(x, LSH(1, y)) into BT(x, y) when possible. Using BT
        // results in smaller and faster code. It also doesn't have special register
        // requirements, unlike LSH that requires the shift count to be in ECX.
        // Note that BT has the same behavior as LSH when the bit index exceeds the
        // operand bit size - it uses (bit_index MOD bit_size).
        //

        GenTree* lsh = cmp->gtGetOp2();
        LIR::Use cmpUse;

        if (lsh->OperIs(GT_LSH) && varTypeIsIntOrI(lsh->TypeGet()) && lsh->gtGetOp1()->IsIntegralConst(1) &&
            BlockRange().TryGetUse(cmp, &cmpUse))
        {
            GenCondition condition = cmp->OperIs(GT_TEST_NE) ? GenCondition::C : GenCondition::NC;

            cmp->SetOper(GT_BT);
            cmp->gtType = TYP_VOID;
            cmp->gtFlags |= GTF_SET_FLAGS;
            cmp->AsOp()->gtOp2 = lsh->gtGetOp2();
            cmp->gtGetOp2()->ClearContained();

            BlockRange().Remove(lsh->gtGetOp1());
            BlockRange().Remove(lsh);

            GenTreeCC* cc;

            if (cmpUse.User()->OperIs(GT_JTRUE))
            {
                cmpUse.User()->ChangeOper(GT_JCC);
                cc              = cmpUse.User()->AsCC();
                cc->gtCondition = condition;
            }
            else
            {
                cc = new (comp, GT_SETCC) GenTreeCC(GT_SETCC, condition, TYP_INT);
                BlockRange().InsertAfter(cmp, cc);
                cmpUse.ReplaceWith(comp, cc);
            }

            cc->gtFlags |= GTF_USE_FLAGS;

            return cmp->gtNext;
        }
    }
    else if (cmp->OperIs(GT_EQ, GT_NE))
    {
        GenTree* op1 = cmp->gtGetOp1();
        GenTree* op2 = cmp->gtGetOp2();

        // TODO-CQ: right now the below peep is inexpensive and gets the benefit in most
        // cases because in majority of cases op1, op2 and cmp would be in that order in
        // execution. In general we should be able to check that all the nodes that come
        // after op1 do not modify the flags so that it is safe to avoid generating a
        // test instruction.

        if (op2->IsIntegralConst(0) && (op1->gtNext == op2) && (op2->gtNext == cmp) &&
            op1->OperIs(GT_AND, GT_OR, GT_XOR, GT_ADD, GT_SUB, GT_NEG))
        {
            op1->gtFlags |= GTF_SET_FLAGS;
            op1->SetUnusedValue();

            BlockRange().Remove(op2);

            GenTree*   next = cmp->gtNext;
            GenTree*   cc;
            genTreeOps ccOp;
            LIR::Use   cmpUse;

            // Fast check for the common case - relop used by a JTRUE that immediately follows it.
            if ((next != nullptr) && next->OperIs(GT_JTRUE) && (next->gtGetOp1() == cmp))
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

GenTree* Lowering::LowerCompare(GenTreeOp* cmp)
{
#ifndef TARGET_64BIT
    if (cmp->GetOp(0)->TypeIs(TYP_LONG))
    {
        return DecomposeLongCompare(cmp);
    }
#endif

    if (cmp->GetOp(1)->IsIntegralConst() && !comp->opts.MinOpts())
    {
        GenTree* next = OptimizeConstCompare(cmp);

        // If OptimizeConstCompare return the compare node as "next" then we need to continue lowering.
        if (next != cmp)
        {
            return next;
        }
    }

    if (cmp->GetOp(0)->GetType() == cmp->GetOp(1)->GetType())
    {
        if (varTypeIsSmall(cmp->GetOp(0)->GetType()) && varTypeIsUnsigned(cmp->GetOp(0)->GetType()))
        {
            // If both operands have the same type then codegen will use the common operand type to
            // determine the instruction type. For small types this would result in performing a
            // signed comparison of two small unsigned values without zero extending them to TYP_INT
            // which is incorrect. Note that making the comparison unsigned doesn't imply that codegen
            // has to generate a small comparison, it can still correctly generate a TYP_INT comparison.
            cmp->gtFlags |= GTF_UNSIGNED;
        }
    }

    ContainCheckCompare(cmp);
    return cmp->gtNext;
}

GenTree* Lowering::LowerJTrue(GenTreeUnOp* jtrue)
{
    ContainCheckJTrue(jtrue);

    assert(jtrue->gtNext == nullptr);
    return nullptr;
}

#ifdef FEATURE_HW_INTRINSICS

//----------------------------------------------------------------------------------------------
// LowerNodeCC: Lowers a node that produces a boolean value by setting the condition flags.
//
// Arguments:
//     node - The node to lower
//     condition - The condition code of the generated SETCC/JCC node
//
// Return Value:
//     A SETCC/JCC node or nullptr if `node` is not used.
//
// Notes:
//     This simply replaces `node`'s use with an appropiate SETCC/JCC node,
//     `node` is not actually changed, except by having its GTF_SET_FLAGS set.
//     It's the caller's responsibility to change `node` such that it only
//     sets the condition flags, without producing a boolean value.
//
GenTreeCC* Lowering::LowerNodeCC(GenTree* node, GenCondition condition)
{
    // Skip over a chain of EQ/NE(x, 0) relops. This may be present either
    // because `node` is not a relop and so it cannot be used directly by a
    // JTRUE, or because the frontend failed to remove a EQ/NE(x, 0) that's
    // used as logical negation.
    //
    // Usually there's only one such relop but there's little difference
    // between removing one or all so we may as well remove them all.
    //
    // We can't allow any other nodes between `node` and its user because we
    // have no way of knowing if those nodes change flags or not. So we're looking
    // to skip over a sequence of appropriately connected zero and EQ/NE nodes.

    // The x in EQ/NE(x, 0)
    GenTree* relop = node;
    // The first node of the relop sequence
    GenTree* first = node->gtNext;
    // The node following the relop sequence
    GenTree* next = first;

    while ((next != nullptr) && next->IsIntegralConst(0) && (next->gtNext != nullptr) &&
           next->gtNext->OperIs(GT_EQ, GT_NE) && (next->gtNext->AsOp()->gtGetOp1() == relop) &&
           (next->gtNext->AsOp()->gtGetOp2() == next))
    {
        relop = next->gtNext;
        next  = relop->gtNext;

        if (relop->OperIs(GT_EQ))
        {
            condition = GenCondition::Reverse(condition);
        }
    }

    GenTreeCC* cc = nullptr;

    // Next may be null if `node` is not used. In that case we don't need to generate a SETCC node.
    if (next != nullptr)
    {
        if (next->OperIs(GT_JTRUE))
        {
            // If the instruction immediately following 'relop', i.e. 'next' is a conditional branch,
            // it should always have 'relop' as its 'op1'. If it doesn't, then we have improperly
            // constructed IL (the setting of a condition code should always immediately precede its
            // use, since the JIT doesn't track dataflow for condition codes). Still, if it happens
            // it's not our problem, it simply means that `node` is not used and can be removed.
            if (next->AsUnOp()->gtGetOp1() == relop)
            {
                assert(relop->OperIsCompare());

                next->ChangeOper(GT_JCC);
                cc              = next->AsCC();
                cc->gtCondition = condition;
            }
        }
        else
        {
            // If the node is used by something other than a JTRUE then we need to insert a
            // SETCC node to materialize the boolean value.
            LIR::Use use;

            if (BlockRange().TryGetUse(relop, &use))
            {
                cc = new (comp, GT_SETCC) GenTreeCC(GT_SETCC, condition, TYP_INT);
                BlockRange().InsertAfter(node, cc);
                use.ReplaceWith(comp, cc);
            }
        }
    }

    if (cc != nullptr)
    {
        node->gtFlags |= GTF_SET_FLAGS;
        cc->gtFlags |= GTF_USE_FLAGS;
    }

    // Remove the chain of EQ/NE(x, 0) relop nodes, if any. Note that if a SETCC was
    // inserted after `node`, `first` still points to the node that was initially
    // after `node`.
    if (relop != node)
    {
        BlockRange().Remove(first, relop);
    }

    return cc;
}

//----------------------------------------------------------------------------------------------
// LowerHWIntrinsicCC: Lowers a hardware intrinsic node that produces a boolean value by
//     setting the condition flags.
//
//  Arguments:
//     node - The hardware intrinsic node
//     newIntrinsicId - The intrinsic id of the lowered intrinsic node
//     condition - The condition code of the generated SETCC/JCC node
//
void Lowering::LowerHWIntrinsicCC(GenTreeHWIntrinsic* node, NamedIntrinsic newIntrinsicId, GenCondition condition)
{
    GenTreeCC* cc = LowerNodeCC(node, condition);

    node->SetIntrinsic(newIntrinsicId);
    node->SetType(TYP_VOID);
    node->ClearUnusedValue();

    bool swapOperands    = false;
    bool canSwapOperands = false;

    switch (newIntrinsicId)
    {
        case NI_SSE_COMISS:
        case NI_SSE_UCOMISS:
        case NI_SSE2_COMISD:
        case NI_SSE2_UCOMISD:
            // In some cases we can generate better code if we swap the operands:
            //   - If the condition is not one of the "preferred" floating point conditions we can swap
            //     the operands and change the condition to avoid generating an extra JP/JNP branch.
            //   - If the first operand can be contained but the second cannot, we can swap operands in
            //     order to be able to contain the first operand and avoid the need for a temp reg.
            // We can't handle both situations at the same time and since an extra branch is likely to
            // be worse than an extra temp reg (x64 has a reasonable number of XMM registers) we'll favor
            // the branch case:
            //   - If the condition is not preferred then swap, even if doing this will later prevent
            //     containment.
            //   - Allow swapping for containment purposes only if this doesn't result in a non-"preferred"
            //     condition being generated.
            if ((cc != nullptr) && cc->gtCondition.PreferSwap())
            {
                swapOperands = true;
            }
            else
            {
                canSwapOperands = (cc == nullptr) || !GenCondition::Swap(cc->gtCondition).PreferSwap();
            }
            break;

        case NI_SSE41_PTEST:
        case NI_AVX_PTEST:
            // If we need the Carry flag then we can't swap operands.
            canSwapOperands = (cc == nullptr) || cc->gtCondition.Is(GenCondition::EQ, GenCondition::NE);
            break;

        default:
            unreached();
    }

    if (canSwapOperands)
    {
        bool op1SupportsRegOptional = false;
        bool op2SupportsRegOptional = false;

        if (!IsContainableHWIntrinsicOp(node, node->GetOp(1), &op2SupportsRegOptional) &&
            IsContainableHWIntrinsicOp(node, node->GetOp(0), &op1SupportsRegOptional))
        {
            // Swap operands if op2 cannot be contained but op1 can.
            swapOperands = true;
        }
    }

    if (swapOperands)
    {
        std::swap(node->GetUse(0).NodeRef(), node->GetUse(1).NodeRef());

        if (cc != nullptr)
        {
            cc->gtCondition = GenCondition::Swap(cc->gtCondition);
        }
    }
}

//----------------------------------------------------------------------------------------------
// LowerFusedMultiplyAdd: Changes NI_FMA_MultiplyAddScalar produced by Math(F).FusedMultiplyAdd
//     to a better FMA intrinsics if there are GT_NEG around in order to eliminate them.
//
//  Arguments:
//     node - The hardware intrinsic node
//
//  Notes:
//     Math(F).FusedMultiplyAdd is expanded into NI_FMA_MultiplyAddScalar and
//     depending on additional GT_NEG nodes around it can be:
//
//      x *  y + z -> NI_FMA_MultiplyAddScalar
//      x * -y + z -> NI_FMA_MultiplyAddNegatedScalar
//     -x *  y + z -> NI_FMA_MultiplyAddNegatedScalar
//     -x * -y + z -> NI_FMA_MultiplyAddScalar
//      x *  y - z -> NI_FMA_MultiplySubtractScalar
//      x * -y - z -> NI_FMA_MultiplySubtractNegatedScalar
//     -x *  y - z -> NI_FMA_MultiplySubtractNegatedScalar
//     -x * -y - z -> NI_FMA_MultiplySubtractScalar
//
void Lowering::LowerFusedMultiplyAdd(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_FMA_MultiplyAddScalar);
    assert(node->IsTernary());

    GenTreeHWIntrinsic::Use* uses[3];
    unsigned                 useCount = 0;

    for (GenTreeHWIntrinsic::Use& use : node->Uses())
    {
        if (!use.GetNode()->OperIsHWIntrinsic() ||
            (use.GetNode()->AsHWIntrinsic()->GetIntrinsic() != NI_Vector128_CreateScalarUnsafe))
        {
            // Math(F).FusedMultiplyAdd is expected to emit three NI_Vector128_CreateScalarUnsafe
            // but it's also possible to use NI_FMA_MultiplyAddScalar directly with any operands
            return;
        }

        uses[useCount++] = &use.GetNode()->AsHWIntrinsic()->GetUse(0);
    }

    GenTree* argX = uses[0]->GetNode();
    GenTree* argY = uses[1]->GetNode();
    GenTree* argZ = uses[2]->GetNode();

    const bool negMul = argX->OperIs(GT_NEG) != argY->OperIs(GT_NEG);
    if (argX->OperIs(GT_NEG))
    {
        uses[0]->SetNode(argX->gtGetOp1());
        BlockRange().Remove(argX);
    }
    if (argY->OperIs(GT_NEG))
    {
        uses[1]->SetNode(argY->gtGetOp1());
        BlockRange().Remove(argY);
    }
    if (argZ->OperIs(GT_NEG))
    {
        uses[2]->SetNode(argZ->gtGetOp1());
        BlockRange().Remove(argZ);
        node->SetIntrinsic(negMul ? NI_FMA_MultiplySubtractNegatedScalar : NI_FMA_MultiplySubtractScalar);
    }
    else
    {
        node->SetIntrinsic(negMul ? NI_FMA_MultiplyAddNegatedScalar : NI_FMA_MultiplyAddScalar);
    }
}

//----------------------------------------------------------------------------------------------
// Lowering::LowerHWIntrinsic: Perform containment analysis for a hardware intrinsic node.
//
//  Arguments:
//     node - The hardware intrinsic node.
//
void Lowering::LowerHWIntrinsic(GenTreeHWIntrinsic* node)
{
    if (node->TypeGet() == TYP_SIMD12)
    {
        // GT_HWINTRINSIC node requiring to produce TYP_SIMD12 in fact
        // produces a TYP_SIMD16 result
        node->gtType = TYP_SIMD16;
    }

    NamedIntrinsic intrinsicId = node->GetIntrinsic();

    switch (intrinsicId)
    {
        case NI_Vector128_Create:
        case NI_Vector256_Create:
            if (node->IsUnary())
            {
                LowerHWIntrinsicCreateBroadcast(node);
            }
            else
            {
                LowerHWIntrinsicCreate(node);
            }
            assert(!node->OperIsHWIntrinsic() || (node->GetIntrinsic() != intrinsicId));
            LowerNode(node);
            return;

        case NI_Vector128_CreateScalarUnsafe:
        case NI_Vector256_CreateScalarUnsafe:
            LowerHWIntrinsicCreateScalarUnsafe(node);
            break;

        case NI_Vector128_Sum:
            LowerHWIntrinsicSum128(node);
            return;
        case NI_Vector256_Sum:
            LowerHWIntrinsicSum256(node);
            return;

        case NI_Vector128_GetElement:
        case NI_Vector256_GetElement:
            LowerHWIntrinsicGetElement(node);
            return;

        case NI_Vector128_WithElement:
        case NI_Vector256_WithElement:
            LowerHWIntrinsicWithElement(node);
            return;

        case NI_Vector128_op_Equality:
        case NI_Vector256_op_Equality:
            LowerHWIntrinsicEquality(node, GT_EQ);
            return;
        case NI_Vector128_op_Inequality:
        case NI_Vector256_op_Inequality:
            LowerHWIntrinsicEquality(node, GT_NE);
            return;

        case NI_Vector128_ToScalar:
        case NI_Vector256_ToScalar:
            unreached();

        case NI_SSE41_Extract:
            // Make sure the importer did not blindly import intrinsic with bogus return type
            // "float Sse41.Extract(Vector128<float>)", the return type should have been int.
            assert(!varTypeIsFloating(node->GetType()));
            break;

        case NI_SSE41_Insert:
            if (node->GetSimdBaseType() == TYP_FLOAT)
            {
                LowerHWIntrinsicInsertFloat(node);
                return;
            }
            FALLTHROUGH;
        case NI_SSE2_Insert:
        case NI_SSE41_X64_Insert:
            assert(node->IsTernary());
            // Insert takes either a 32-bit register or a memory operand.
            // In either case, only gtSIMDBaseType bits are read and so
            // widening or narrowing the operand may be unnecessary and it
            // can just be used directly.
            node->SetOp(1, TryRemoveCastIfPresent(node->GetSimdBaseType(), node->GetOp(1)));
            break;

        case NI_SSE42_Crc32:
            if (varTypeIsSmall(node->GetSimdBaseType()))
            {
                // NI_SSE42_Crc32 nodes have type INT or LONG but the input value can be
                // treated as a small integer, in which case we can remove some narrowing
                // casts. The type of the input value (basically type of the operation)
                // is stored as "SIMD base type", even if no SIMD types are involved.
                node->SetOp(1, TryRemoveCastIfPresent(node->GetSimdBaseType(), node->GetOp(1)));
            }
            break;

        case NI_SSE2_CompareGreaterThan:
        {
            if (node->GetSimdBaseType() != TYP_DOUBLE)
            {
                assert(varTypeIsIntegral(node->GetSimdBaseType()));
                break;
            }

            FALLTHROUGH;
        }

        case NI_SSE_CompareGreaterThan:
        case NI_SSE_CompareGreaterThanOrEqual:
        case NI_SSE_CompareNotGreaterThan:
        case NI_SSE_CompareNotGreaterThanOrEqual:
        case NI_SSE2_CompareGreaterThanOrEqual:
        case NI_SSE2_CompareNotGreaterThan:
        case NI_SSE2_CompareNotGreaterThanOrEqual:
        {
            assert((node->GetSimdBaseType() == TYP_FLOAT) || (node->GetSimdBaseType() == TYP_DOUBLE));

            if (comp->compOpportunisticallyDependsOn(InstructionSet_AVX))
            {
                break;
            }

            // pre-AVX doesn't actually support these intrinsics in hardware so we need to swap the operands around
            std::swap(node->GetUse(0).NodeRef(), node->GetUse(1).NodeRef());
            break;
        }

        case NI_SSE2_CompareLessThan:
        case NI_SSE42_CompareLessThan:
        case NI_AVX2_CompareLessThan:
        {
            if (node->GetSimdBaseType() == TYP_DOUBLE)
            {
                break;
            }
            assert(varTypeIsIntegral(node->GetSimdBaseType()));

            // this isn't actually supported in hardware so we need to swap the operands around
            std::swap(node->GetUse(0).NodeRef(), node->GetUse(1).NodeRef());
            break;
        }

        case NI_SSE_CompareScalarOrderedEqual:
            LowerHWIntrinsicCC(node, NI_SSE_COMISS, GenCondition::FEQ);
            break;
        case NI_SSE_CompareScalarOrderedNotEqual:
            LowerHWIntrinsicCC(node, NI_SSE_COMISS, GenCondition::FNEU);
            break;
        case NI_SSE_CompareScalarOrderedLessThan:
            LowerHWIntrinsicCC(node, NI_SSE_COMISS, GenCondition::FLT);
            break;
        case NI_SSE_CompareScalarOrderedLessThanOrEqual:
            LowerHWIntrinsicCC(node, NI_SSE_COMISS, GenCondition::FLE);
            break;
        case NI_SSE_CompareScalarOrderedGreaterThan:
            LowerHWIntrinsicCC(node, NI_SSE_COMISS, GenCondition::FGT);
            break;
        case NI_SSE_CompareScalarOrderedGreaterThanOrEqual:
            LowerHWIntrinsicCC(node, NI_SSE_COMISS, GenCondition::FGE);
            break;

        case NI_SSE_CompareScalarUnorderedEqual:
            LowerHWIntrinsicCC(node, NI_SSE_UCOMISS, GenCondition::FEQ);
            break;
        case NI_SSE_CompareScalarUnorderedNotEqual:
            LowerHWIntrinsicCC(node, NI_SSE_UCOMISS, GenCondition::FNEU);
            break;
        case NI_SSE_CompareScalarUnorderedLessThanOrEqual:
            LowerHWIntrinsicCC(node, NI_SSE_UCOMISS, GenCondition::FLE);
            break;
        case NI_SSE_CompareScalarUnorderedLessThan:
            LowerHWIntrinsicCC(node, NI_SSE_UCOMISS, GenCondition::FLT);
            break;
        case NI_SSE_CompareScalarUnorderedGreaterThanOrEqual:
            LowerHWIntrinsicCC(node, NI_SSE_UCOMISS, GenCondition::FGE);
            break;
        case NI_SSE_CompareScalarUnorderedGreaterThan:
            LowerHWIntrinsicCC(node, NI_SSE_UCOMISS, GenCondition::FGT);
            break;

        case NI_SSE2_CompareScalarOrderedEqual:
            LowerHWIntrinsicCC(node, NI_SSE2_COMISD, GenCondition::FEQ);
            break;
        case NI_SSE2_CompareScalarOrderedNotEqual:
            LowerHWIntrinsicCC(node, NI_SSE2_COMISD, GenCondition::FNEU);
            break;
        case NI_SSE2_CompareScalarOrderedLessThan:
            LowerHWIntrinsicCC(node, NI_SSE2_COMISD, GenCondition::FLT);
            break;
        case NI_SSE2_CompareScalarOrderedLessThanOrEqual:
            LowerHWIntrinsicCC(node, NI_SSE2_COMISD, GenCondition::FLE);
            break;
        case NI_SSE2_CompareScalarOrderedGreaterThan:
            LowerHWIntrinsicCC(node, NI_SSE2_COMISD, GenCondition::FGT);
            break;
        case NI_SSE2_CompareScalarOrderedGreaterThanOrEqual:
            LowerHWIntrinsicCC(node, NI_SSE2_COMISD, GenCondition::FGE);
            break;

        case NI_SSE2_CompareScalarUnorderedEqual:
            LowerHWIntrinsicCC(node, NI_SSE2_UCOMISD, GenCondition::FEQ);
            break;
        case NI_SSE2_CompareScalarUnorderedNotEqual:
            LowerHWIntrinsicCC(node, NI_SSE2_UCOMISD, GenCondition::FNEU);
            break;
        case NI_SSE2_CompareScalarUnorderedLessThanOrEqual:
            LowerHWIntrinsicCC(node, NI_SSE2_UCOMISD, GenCondition::FLE);
            break;
        case NI_SSE2_CompareScalarUnorderedLessThan:
            LowerHWIntrinsicCC(node, NI_SSE2_UCOMISD, GenCondition::FLT);
            break;
        case NI_SSE2_CompareScalarUnorderedGreaterThanOrEqual:
            LowerHWIntrinsicCC(node, NI_SSE2_UCOMISD, GenCondition::FGE);
            break;
        case NI_SSE2_CompareScalarUnorderedGreaterThan:
            LowerHWIntrinsicCC(node, NI_SSE2_UCOMISD, GenCondition::FGT);
            break;

        case NI_SSE41_TestC:
            LowerHWIntrinsicCC(node, NI_SSE41_PTEST, GenCondition::C);
            break;
        case NI_SSE41_TestZ:
            LowerHWIntrinsicCC(node, NI_SSE41_PTEST, GenCondition::EQ);
            break;
        case NI_SSE41_TestNotZAndNotC:
            LowerHWIntrinsicCC(node, NI_SSE41_PTEST, GenCondition::UGT);
            break;

        case NI_AVX_TestC:
            LowerHWIntrinsicCC(node, NI_AVX_PTEST, GenCondition::C);
            break;
        case NI_AVX_TestZ:
            LowerHWIntrinsicCC(node, NI_AVX_PTEST, GenCondition::EQ);
            break;
        case NI_AVX_TestNotZAndNotC:
            LowerHWIntrinsicCC(node, NI_AVX_PTEST, GenCondition::UGT);
            break;

        case NI_FMA_MultiplyAddScalar:
            LowerFusedMultiplyAdd(node);
            break;

        default:
            break;
    }

    ContainCheckHWIntrinsic(node);
}

void Lowering::LowerHWIntrinsicEquality(GenTreeHWIntrinsic* node, genTreeOps cmpOp)
{
    NamedIntrinsic intrinsicId = node->GetIntrinsic();
    var_types      baseType    = node->GetSimdBaseType();
    unsigned       simdSize    = node->GetSimdSize();
    var_types      simdType    = getSIMDTypeForSize(simdSize);

    assert((intrinsicId == NI_Vector128_op_Equality) || (intrinsicId == NI_Vector128_op_Inequality) ||
           (intrinsicId == NI_Vector256_op_Equality) || (intrinsicId == NI_Vector256_op_Inequality));

    assert(varTypeIsIntegral(baseType));
    assert(comp->compOpportunisticallyDependsOn(InstructionSet_SSE41));
    assert(node->gtType == TYP_BOOL);
    assert((cmpOp == GT_EQ) || (cmpOp == GT_NE));
    assert((simdSize == 16) || (simdSize == 32));

    GenTree* op1 = node->GetOp(0);
    GenTree* op2 = node->GetOp(1);

    GenCondition cmpCnd = (cmpOp == GT_EQ) ? GenCondition::EQ : GenCondition::NE;

    if (op1->IsHWIntrinsicZero())
    {
        std::swap(op1, op2);
    }

    if (op2->IsHWIntrinsicZero())
    {
        // On SSE4.1 or higher we can optimize comparisons against zero to
        // just use PTEST. We can't support it for floating-point, however,
        // as it has both +0.0 and -0.0 where +0.0 == -0.0

        BlockRange().Remove(op2);

        node->SetOp(0, op1);
        LIR::Use op1Use(BlockRange(), &node->GetUse(0).NodeRef(), node);
        op1 = ReplaceWithLclVar(op1Use);
        op2 = comp->gtNewLclvNode(op1->AsLclVar()->GetLclNum(), op1->GetType());
        BlockRange().InsertAfter(op1, op2);
        node->SetOp(1, op2);

        NamedIntrinsic testz = simdSize == 32 ? NI_AVX_TestZ : NI_SSE41_TestZ;
        NamedIntrinsic ptest = simdSize == 32 ? NI_AVX_PTEST : NI_SSE41_PTEST;

        node->SetIntrinsic(testz);
        LowerHWIntrinsicCC(node, ptest, cmpCnd);

        return;
    }

    NamedIntrinsic cmpIntrinsic = simdSize == 32 ? NI_AVX2_CompareEqual : NI_SSE2_CompareEqual;
    NamedIntrinsic mskIntrinsic = simdSize == 32 ? NI_AVX2_MoveMask : NI_SSE2_MoveMask;
    int            mskConstant  = simdSize == 32 ? -1 : 0xFFFF;

    GenTree* cmp    = comp->gtNewSimdHWIntrinsicNode(simdType, cmpIntrinsic, TYP_UBYTE, simdSize, op1, op2);
    GenTree* msk    = comp->gtNewSimdHWIntrinsicNode(TYP_INT, mskIntrinsic, TYP_UBYTE, simdSize, cmp);
    GenTree* mskCns = comp->gtNewIconNode(mskConstant, TYP_INT);
    BlockRange().InsertBefore(node, cmp, msk, mskCns);
    LowerNode(cmp);
    LowerNode(msk);

    node->ChangeOper(cmpOp);

    GenTreeOp* relop = static_cast<GenTree*>(node)->AsOp();
    relop->SetType(TYP_INT);
    relop->SetOp(0, msk);
    relop->SetOp(1, mskCns);

    GenTree* cc = LowerNodeCC(relop, cmpCnd);

    relop->SetType(TYP_VOID);
    relop->ClearUnusedValue();

    LowerNode(relop);
}

#ifdef TARGET_X86
void Lowering::LowerHWIntrinsicCreateScalarUnsafeLong(GenTreeHWIntrinsic* node)
{
    GenTree* op = node->GetOp(0);

    assert(op->OperIs(GT_LONG));

    if (node->GetIntrinsic() == NI_Vector256_CreateScalarUnsafe)
    {
        GenTree* create128 =
            comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_LONG, 16, op);
        BlockRange().InsertAfter(op, create128);
        node->SetIntrinsic(NI_Vector128_ToVector256Unsafe);
        node->SetOp(0, create128);
        LowerNode(create128);
        LowerNode(node);

        return;
    }

    // TODO-MIKE-Cleanup: This should just use Vector128_Create(long, 0), with appropiate optimizations
    // to prevent redundant 0 inserts. There's really no such thing as "unsafe" when it comes to integer
    // vector element types since one way or another we end up zeroing the upper bits.

    // TODO-MIKE-CQ: This doesn't work so well when the operand is in memory. We could simply load it
    // with MOVQ but the operand has already been decomposed and "re-composing" it back is way too much
    // trouble. We could recognize CreateScalarUnsafe(IND|LCL_FLD<long>) while morphing and change to
    // LoadScalarVector128. Though for LCL_FLD that may require making the local address exposed which
    // isn't exactly ideal. Eh, x86...

    GenTree* op1 = op->AsOp()->GetOp(0);
    GenTree* op2 = op->AsOp()->GetOp(1);
    BlockRange().Remove(op);

    if (op1->IsIntegralConst(0) && op2->IsIntegralConst(0))
    {
        node->SetIntrinsic(GetZeroSimdHWIntrinsic(node->GetType()), 0);
        BlockRange().Remove(op1);
        BlockRange().Remove(op2);

        return;
    }

    if (op2->IsIntegralConst(0))
    {
        node->SetIntrinsic(NI_SSE2_ConvertScalarToVector128Int32, TYP_INT, 16, 1);
        node->SetOp(0, op1);
        BlockRange().Remove(op2);
        LowerNode(node);

        return;
    }

    GenTree* movd1;

    if (op1->IsIntegralConst(0))
    {
        movd1 = comp->gtNewZeroSimdHWIntrinsicNode(TYP_SIMD16, TYP_LONG);
        BlockRange().Remove(op1);
        BlockRange().InsertBefore(node, movd1);
    }
    else
    {
        movd1 = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ConvertScalarToVector128Int32, TYP_INT, 16, op1);
        BlockRange().InsertAfter(op1, movd1);
    }

    if (comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        GenTree* idx = comp->gtNewIconNode(1);
        node->SetIntrinsic(NI_SSE41_Insert, TYP_INT, 16, 3);
        node->SetOp(0, movd1);
        node->SetOp(1, op2);
        node->SetOp(2, idx);
        BlockRange().InsertBefore(node, idx);
        LowerNode(movd1);
    }
    else
    {
        GenTree* movd2 =
            comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ConvertScalarToVector128Int32, TYP_INT, 16, op2);
        BlockRange().InsertAfter(op2, movd2);

        node->SetIntrinsic(NI_SSE2_UnpackLow, TYP_INT, 16, 2);
        node->SetOp(0, movd1);
        node->SetOp(1, movd2);
        LowerNode(movd1);
        LowerNode(movd2);
    }

    LowerNode(node);
}
#endif // TARGET_X86

void Lowering::LowerHWIntrinsicCreateScalarUnsafe(GenTreeHWIntrinsic* node)
{
    GenTree* op = node->GetOp(0);

#ifdef TARGET_X86
    if (op->OperIs(GT_LONG))
    {
        LowerHWIntrinsicCreateScalarUnsafeLong(node);
        return;
    }
#endif

    if (op->IsDblConPositiveZero() || op->IsIntegralConst(0))
    {
        BlockRange().Remove(op);
        node->SetIntrinsic(GetZeroSimdHWIntrinsic(node->GetType()), 0);
    }
}

void Lowering::LowerHWIntrinsicCreate(GenTreeHWIntrinsic* node)
{
    var_types eltType = node->GetSimdBaseType();
    unsigned  size    = node->GetSimdSize();
    unsigned  numOps  = node->GetNumOps();

    assert(varTypeIsSIMD(node->GetType()));
    assert(varTypeIsArithmetic(eltType));
    assert((size == 16) || (size == 32));
    assert((numOps == (size / varTypeSize(eltType))) || ((numOps == 2) && (eltType == TYP_FLOAT)));

#ifndef TARGET_64BIT
    if (varTypeIsLong(eltType))
    {
        assert((numOps == 2) || (numOps == 4));

        GenTree* ops[8];

        for (unsigned i = 0; i < numOps; i++)
        {
            GenTree* op = node->GetOp(i);
            assert(op->OperIs(GT_LONG));
            ops[i * 2]     = op->AsOp()->GetOp(0);
            ops[i * 2 + 1] = op->AsOp()->GetOp(1);
            BlockRange().Remove(op);
        }

        numOps *= 2;
        eltType = TYP_INT;

        node->SetNumOps(0);
        node->SetNumOps(numOps, comp->getAllocator(CMK_ASTNode));
        node->SetSimdBaseType(eltType);

        for (unsigned i = 0; i < numOps; i++)
        {
            node->SetOp(i, ops[i]);
        }
    }
#endif

    // TODO-XARCH-CQ: We should be able to modify at least the paths that use Insert to trivially support partial
    // vector constants. With this, we can create a constant if say 50% of the inputs are also constant and just
    // insert the non-constant values which should still allow some gains.

    VectorConstant vecConst;

    if (vecConst.Create(node))
    {
        LowerHWIntrinsicCreateConst(node, vecConst);
        return;
    }

    // TODO-MIKE-Review: Much of this code assumes that operand order matches evaluation order.
    // This assumption only holds because gtSetEvalOrder/GTF_REVERSE_OPS aren't able to control
    // the ordering of intrinsic nodes with more than 2 operands.

    if (node->GetIntrinsic() == NI_Vector256_Create)
    {
        assert(comp->compIsaSupportedDebugOnly(InstructionSet_AVX));

        GenTreeHWIntrinsic* lo = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, eltType, 16);
        GenTreeHWIntrinsic* hi = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, eltType, 16);

        assert(numOps % 2 == 0);

        lo->SetNumOps(numOps / 2, comp->getAllocator(CMK_ASTNode));
        hi->SetNumOps(numOps / 2, comp->getAllocator(CMK_ASTNode));

        for (unsigned i = 0; i < numOps / 2; i++)
        {
            lo->SetOp(i, node->GetOp(i));
            hi->SetOp(i, node->GetOp(numOps / 2 + i));
        }

        BlockRange().InsertAfter(lo->GetLastOp(), lo);
        BlockRange().InsertAfter(hi->GetLastOp(), hi);

        GenTree* idx = comp->gtNewIconNode(1);
        BlockRange().InsertBefore(node, idx);
        node->SetIntrinsic(NI_AVX_InsertVector128, 3);
        node->SetOp(0, lo);
        node->SetOp(1, hi);
        node->SetOp(2, idx);
        LowerNode(lo);
        LowerNode(hi);

        return;
    }

    auto ScalarToVector128 = [this](var_types eltType, GenTree* scalar) -> GenTree* {
        if (scalar->IsIntegralConst(0) || scalar->IsDblConPositiveZero())
        {
            scalar->ChangeOper(GT_HWINTRINSIC);
            scalar->SetType(TYP_SIMD16);
            scalar->AsHWIntrinsic()->SetIntrinsic(NI_Vector128_get_Zero, eltType, 16, 0);
            return scalar;
        }

        GenTree* vec = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, eltType, 16, scalar);
        BlockRange().InsertAfter(scalar, vec);
        return vec;
    };

    GenTree* op1 = node->GetOp(0);

    if (varTypeIsLong(eltType) && comp->compOpportunisticallyDependsOn(InstructionSet_SSE41_X64))
    {
#ifndef TARGET_AMD64
        unreached();
#else
        assert(numOps == 2);

        GenTree* movd1 = ScalarToVector128(eltType, op1);
        GenTree* idx   = comp->gtNewIconNode(1);
        BlockRange().InsertBefore(node, idx);
        GenTree* op2 = node->GetOp(1);
        node->SetIntrinsic(NI_SSE41_X64_Insert, 3);
        node->SetOp(0, movd1);
        node->SetOp(1, op2);
        node->SetOp(2, idx);
        LowerNode(movd1);

        return;
#endif // TARGET_AMD64
    }

    if (varTypeIsLong(eltType))
    {
#ifndef TARGET_AMD64
        unreached();
#else
        assert(numOps == 2);

        GenTree* movd1 = ScalarToVector128(eltType, op1);
        GenTree* movd2 = ScalarToVector128(eltType, node->GetOp(1));
        node->SetIntrinsic(NI_SSE2_UnpackLow, 2);
        node->SetOp(0, movd1);
        node->SetOp(1, movd2);
        LowerNode(movd1);
        LowerNode(movd2);

        return;
#endif // TARGET_AMD64
    }

    if (eltType == TYP_DOUBLE)
    {
        assert(numOps == 2);

        GenTree* vec1 = ScalarToVector128(TYP_DOUBLE, op1);
        GenTree* vec2 = ScalarToVector128(TYP_DOUBLE, node->GetOp(1));
        node->SetIntrinsic(NI_SSE_MoveLowToHigh, TYP_FLOAT, 2);
        node->SetOp(0, vec1);
        node->SetOp(1, vec2);
        LowerNode(vec1);
        LowerNode(vec2);

        return;
    }

    if ((eltType == TYP_FLOAT) && (numOps == 2))
    {
        GenTree* op2 = node->GetOp(1);

        // Special case of Create with 2 operands for the x64 ABI. If both operands are in registers
        // then unpcklps is preferrable to insertps as it's shorter. However, insertps can contain
        // FLOAT memory operands so try to use that when we definitly know we have a memory operand.

        bool op2IsMem = IsContainableMemoryOp(op2) || op2->IsDblCon();

        if (!op2IsMem || !comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
        {
            op1 = ScalarToVector128(TYP_FLOAT, op1);
            op2 = ScalarToVector128(TYP_FLOAT, op2);
            node->SetIntrinsic(NI_SSE_UnpackLow);
            node->SetOp(0, op1);
            node->SetOp(1, op2);
            LowerNode(node);

            return;
        }
    }

    if ((eltType == TYP_FLOAT) && comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        assert(numOps <= 4);

        unsigned nonZeroOpMask = 0;

        for (unsigned i = 0; i < numOps; i++)
        {
            GenTree* op = node->GetOp(i);

            if (op->IsDblConPositiveZero())
            {
                BlockRange().Remove(op);
            }
            else
            {
                nonZeroOpMask |= 1 << i;
            }
        }

        unsigned zeroOpMask = ~nonZeroOpMask & 0b1111;
        GenTree* vec        = nullptr;

        for (unsigned i = 0; nonZeroOpMask != 0; nonZeroOpMask >>= 1, i++)
        {
            if ((nonZeroOpMask & 1) == 0)
            {
                continue;
            }

            GenTree* op = node->GetOp(i);

            // There are other non-zero operands so we can generate a movaps for the
            // first operand and leave any necessary zeroing to the next insertps.
            // Otherwise it means that only the first operand is non-zero so we have
            // no choice but to generate an insertps for it. This requires us to
            // also generate a 0 vector to have something to insert into and hope
            // that containment will prevent generating a useless xorps.
            if ((i == 0) && ((nonZeroOpMask >> 1) != 0))
            {
                vec = ScalarToVector128(TYP_FLOAT, op);
                continue;
            }

            GenTree* zero = nullptr;

            if (vec == nullptr)
            {
                zero = comp->gtNewZeroSimdHWIntrinsicNode(TYP_SIMD16, TYP_FLOAT);
                vec  = zero;
            }

            GenTree* idx = comp->gtNewIconNode((i << 4) | zeroOpMask);

            if (nonZeroOpMask != 1)
            {
                vec = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE41_Insert, TYP_FLOAT, 16, vec, op, idx);

                if (zero == nullptr)
                {
                    BlockRange().InsertAfter(op, idx, vec);
                }
                else
                {
                    BlockRange().InsertAfter(op, zero, idx, vec);
                }

                LowerHWIntrinsicInsertFloat(vec->AsHWIntrinsic());
            }
            else
            {
                if (zero == nullptr)
                {
                    BlockRange().InsertBefore(node, idx);
                }
                else
                {
                    BlockRange().InsertBefore(node, zero, idx);
                }

                node->SetIntrinsic(NI_SSE41_Insert, TYP_FLOAT, 16, 3);
                node->SetOp(0, vec);
                node->SetOp(1, op);
                node->SetOp(2, idx);
                LowerHWIntrinsicInsertFloat(node);
            }
        }

        return;
    }

    if (varTypeIsShort(eltType) || comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        op1 = TryRemoveCastIfPresent(eltType, op1);

        GenTree* vec = ScalarToVector128(eltType, op1);
        LowerNode(vec);

        NamedIntrinsic insert = varTypeIsShort(eltType) ? NI_SSE2_Insert : NI_SSE41_Insert;

        for (unsigned i = 1; i < numOps; i++)
        {
            GenTree* op  = node->GetOp(i);
            GenTree* idx = comp->gtNewIconNode(i);

            if (i < numOps - 1)
            {
                vec = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, insert, eltType, 16, vec, op, idx);
                BlockRange().InsertAfter(op, idx, vec);
                LowerNode(vec);
            }
            else
            {
                BlockRange().InsertBefore(node, idx);
                node->SetIntrinsic(insert, 3);
                node->SetOp(0, vec);
                node->SetOp(1, op);
                node->SetOp(2, idx);
                LowerNode(node);
            }
        }

        return;
    }

    assert((varTypeSize(eltType) == 1) || (varTypeSize(eltType) == 4));
    assert((numOps == 16) || (numOps == 4));

    GenTree* v[16];

    for (unsigned i = 0; i < numOps; i++)
    {
        GenTree* op = node->GetOp(i);

        op   = TryRemoveCastIfPresent(eltType, op);
        v[i] = ScalarToVector128(eltType, op);
        LowerNode(v[i]);
    }

    auto UnpackLow = [this](var_types eltType, GenTree* op1, GenTree* op2) -> GenTree* {
        if (op1->IsHWIntrinsicZero() && op2->IsHWIntrinsicZero())
        {
            BlockRange().Remove(op1);
            return op2;
        }

        NamedIntrinsic intrinsic = eltType == TYP_FLOAT ? NI_SSE_UnpackLow : NI_SSE2_UnpackLow;
        GenTree*       unpack    = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, eltType, 16, op1, op2);
        BlockRange().InsertAfter(op2, unpack);
        LowerNode(unpack);
        return unpack;
    };

    if (varTypeIsByte(eltType))
    {
        assert(numOps == 16);

        for (unsigned i = 0; i < 16; i += 4)
        {
            v[i]     = UnpackLow(TYP_UBYTE, v[i], v[i + 1]);
            v[i + 1] = UnpackLow(TYP_UBYTE, v[i + 2], v[i + 3]);
            v[i / 4] = UnpackLow(TYP_USHORT, v[i], v[i + 1]);
        }

        eltType = TYP_UINT;
        numOps  = 4;
    }

    assert(numOps == 4);

    v[0] = UnpackLow(eltType, v[0], v[1]);
    v[1] = UnpackLow(eltType, v[2], v[3]);

    NamedIntrinsic intrinsic;

    if (eltType != TYP_FLOAT)
    {
        assert((eltType == TYP_INT) || (eltType == TYP_UINT));
        intrinsic = NI_SSE2_UnpackLow;
        eltType   = TYP_ULONG;
    }
    else
    {
        intrinsic = NI_SSE_MoveLowToHigh;
    }

    node->SetIntrinsic(intrinsic, eltType, 16, 2);
    node->SetOp(0, v[0]);
    node->SetOp(1, v[1]);
}

void Lowering::LowerHWIntrinsicCreateBroadcast(GenTreeHWIntrinsic* node)
{
    assert(node->IsUnary());

    NamedIntrinsic intrinsic = node->GetIntrinsic();
    var_types      eltType   = node->GetSimdBaseType();
    unsigned       size      = node->GetSimdSize();
    GenTree*       op1       = node->GetOp(0);

    assert(varTypeIsSIMD(node->GetType()));
    assert(varTypeIsArithmetic(eltType));
    assert((size == 8) || (size == 12) || (size == 16) || (size == 32));

    VectorConstant vecConst;

    if (vecConst.Broadcast(node))
    {
        LowerHWIntrinsicCreateConst(node, vecConst);
        return;
    }

    if ((intrinsic == NI_Vector256_Create) && !comp->compOpportunisticallyDependsOn(InstructionSet_AVX2))
    {
        assert(comp->compIsaSupportedDebugOnly(InstructionSet_AVX));

        GenTree* half = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, eltType, 16, op1);
        BlockRange().InsertAfter(op1, half);

        node->SetOp(0, half);
        LIR::Use       use(BlockRange(), &node->GetUse(0).NodeRef(), node);
        GenTreeLclVar* tmp1 = ReplaceWithLclVar(use);
        GenTreeLclVar* tmp2 = comp->gtNewLclvNode(tmp1->GetLclNum(), TYP_SIMD16);
        GenTree* vec = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector128_ToVector256Unsafe, eltType, 16, tmp1);
        GenTree* idx = comp->gtNewIconNode(1);
        BlockRange().InsertBefore(node, tmp2, vec, idx);
        node->SetIntrinsic(NI_AVX_InsertVector128, 3);
        node->SetOp(0, vec);
        node->SetOp(1, tmp2);
        node->SetOp(2, idx);
        LowerNode(half);
        LowerNode(vec);

        return;
    }

    GenTree* vec;

#ifndef TARGET_AMD64
    if (op1->OperIs(GT_LONG))
    {
        GenTree* lo = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_INT, 16,
                                                     op1->AsOp()->GetOp(0));
        GenTree* hi = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_INT, 16,
                                                     op1->AsOp()->GetOp(1));

        vec = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_UnpackLow, TYP_INT, 16, lo, hi);
        BlockRange().InsertAfter(op1, lo, hi, vec);
        BlockRange().Remove(op1);
        LowerNode(lo);
        LowerNode(hi);
        LowerNode(vec);
    }
    else
#endif
    {
        op1 = TryRemoveCastIfPresent(eltType, op1);
        vec = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, eltType, 16, op1);
        BlockRange().InsertAfter(op1, vec);
        LowerNode(vec);
    }

    if (intrinsic == NI_Vector256_Create)
    {
        assert(comp->compIsaSupportedDebugOnly(InstructionSet_AVX2));

        node->SetIntrinsic(NI_AVX2_BroadcastScalarToVector256, 1);
        node->SetOp(0, vec);

        return;
    }

    // TODO-MIKE-Consider: Do we really need to special case Vector2/3? This was accidentally
    // lost when deleting GenTreeSIMD and of course tests didn't fail. It should not be needed
    // for corectness because Vector2/3 operations that depend on the upper unused element(s)
    // (e.g. equality, dot product) simply ignore those. However, having arbitrary values in
    // the unused elements could result in FP exceptions (if they somehow become unmasked) or
    // poor performance due to denormals so perhaps the element zeroing cost is justified.
    // It is however definitely unnecessary in certain cases, for example when the produced
    // value is stored to a memory via a SIMD8/12 indirection.
    // Also, ARM64 doesn't do this for Vector3 and it's not clear if someone simply forgot to
    // do it or specifically decided that it isn't needed.

    if ((size == 8) || (size == 12))
    {
        assert(eltType == TYP_FLOAT);

        NamedIntrinsic intrinsic;
        GenTree*       ops[3];

        if (size == 8)
        {
            ops[0] = vec;
            ops[1] = comp->gtNewZeroSimdHWIntrinsicNode(TYP_SIMD16, TYP_FLOAT);
            ops[2] = comp->gtNewIconNode(0);
            BlockRange().InsertBefore(node, ops[1], ops[2]);

            intrinsic = NI_SSE_Shuffle;
        }
        else if (comp->compOpportunisticallyDependsOn(InstructionSet_AVX2))
        {
            ops[0] = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AVX2_BroadcastScalarToVector128, TYP_FLOAT, 16, vec);
            ops[1] = comp->gtNewDconNode(0, TYP_FLOAT);
            ops[2] = comp->gtNewIconNode(0b00111000);
            BlockRange().InsertBefore(node, ops[0], ops[1], ops[2]);
            LowerNode(ops[0]);

            intrinsic = NI_SSE41_Insert;
        }
        else if (comp->compOpportunisticallyDependsOn(InstructionSet_AVX))
        {
            GenTree* imm = comp->gtNewIconNode(0);
            ops[0]       = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AVX_Permute, TYP_FLOAT, 16, vec, imm);
            ops[1]       = comp->gtNewDconNode(0, TYP_FLOAT);
            ops[2]       = comp->gtNewIconNode(0b00111000);
            BlockRange().InsertBefore(node, imm, ops[0], ops[1], ops[2]);
            LowerNode(ops[0]);

            intrinsic = NI_SSE41_Insert;
        }
        else
        {
            node->SetOp(0, vec);
            LIR::Use use(BlockRange(), &node->GetUse(0).NodeRef(), node);
            ops[0]        = ReplaceWithLclVar(use);
            GenTree* zero = comp->gtNewZeroSimdHWIntrinsicNode(TYP_SIMD16, TYP_FLOAT);
            GenTree* tmp  = comp->gtNewLclvNode(ops[0]->AsLclVar()->GetLclNum(), TYP_SIMD16);
            ops[1]        = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_MoveScalar, TYP_FLOAT, 16, zero, tmp);
            ops[2]        = comp->gtNewIconNode(0b01000000);
            BlockRange().InsertBefore(node, tmp, zero, ops[1], ops[2]);

            intrinsic = NI_SSE_Shuffle;
        }

        node->SetIntrinsic(intrinsic, TYP_FLOAT, 16, 3);
        node->SetOp(0, ops[0]);
        node->SetOp(1, ops[1]);
        node->SetOp(2, ops[2]);

        return;
    }

    if ((eltType != TYP_DOUBLE) && comp->compOpportunisticallyDependsOn(InstructionSet_AVX2))
    {
        node->SetIntrinsic(NI_AVX2_BroadcastScalarToVector128, 1);
        node->SetOp(0, vec);

        return;
    }

    if ((eltType == TYP_FLOAT) && comp->compOpportunisticallyDependsOn(InstructionSet_AVX))
    {
        GenTree* imm = comp->gtNewIconNode(0);
        BlockRange().InsertBefore(node, imm);
        node->SetIntrinsic(NI_AVX_Permute, 2);
        node->SetOp(0, vec);
        node->SetOp(1, imm);

        return;
    }

    if (eltType == TYP_FLOAT)
    {
        node->SetOp(0, vec);
        LIR::Use use(BlockRange(), &node->GetUse(0).NodeRef(), node);
        GenTree* tmp1 = ReplaceWithLclVar(use);
        GenTree* tmp2 = comp->gtNewLclvNode(tmp1->AsLclVar()->GetLclNum(), TYP_SIMD16);
        GenTree* idx  = comp->gtNewIconNode(0);
        BlockRange().InsertBefore(node, tmp2, idx);
        node->SetIntrinsic(NI_SSE_Shuffle, 3);
        node->SetOp(0, tmp1);
        node->SetOp(1, tmp2);
        node->SetOp(2, idx);

        return;
    }

    if ((eltType == TYP_DOUBLE) && comp->compOpportunisticallyDependsOn(InstructionSet_SSE3))
    {
        node->SetIntrinsic(NI_SSE3_MoveAndDuplicate, 1);
        node->SetOp(0, vec);

        return;
    }

    if (eltType == TYP_DOUBLE)
    {
        node->SetOp(0, vec);
        LIR::Use use(BlockRange(), &node->GetUse(0).NodeRef(), node);
        GenTree* tmp1 = ReplaceWithLclVar(use);
        GenTree* tmp2 = comp->gtNewLclvNode(tmp1->AsLclVar()->GetLclNum(), TYP_SIMD16);
        BlockRange().InsertBefore(node, tmp2);
        node->SetIntrinsic(NI_SSE_MoveLowToHigh, TYP_FLOAT, 2);
        node->SetOp(0, tmp1);
        node->SetOp(1, tmp2);

        return;
    }

    if (varTypeIsLong(eltType))
    {
        node->SetOp(0, vec);
        LIR::Use use(BlockRange(), &node->GetUse(0).NodeRef(), node);
        GenTree* tmp1 = ReplaceWithLclVar(use);
        GenTree* tmp2 = comp->gtNewLclvNode(tmp1->AsLclVar()->GetLclNum(), TYP_SIMD16);
        BlockRange().InsertBefore(node, tmp2);
        node->SetIntrinsic(NI_SSE2_UnpackLow, 2);
        node->SetOp(0, tmp1);
        node->SetOp(1, tmp2);

        return;
    }

    if (varTypeIsByte(eltType) && comp->compOpportunisticallyDependsOn(InstructionSet_SSSE3))
    {
        GenTree* zero = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_get_Zero, TYP_UBYTE, 16);
        BlockRange().InsertBefore(node, zero);
        node->SetIntrinsic(NI_SSSE3_Shuffle, 2);
        node->SetOp(0, vec);
        node->SetOp(1, zero);

        return;
    }

    assert(varTypeIsIntegral(eltType) && !varTypeIsLong(eltType));

    if (varTypeIsByte(eltType))
    {
        node->SetOp(0, vec);
        LIR::Use use(BlockRange(), &node->GetUse(0).NodeRef(), node);
        GenTree* tmp1 = ReplaceWithLclVar(use);
        GenTree* tmp2 = comp->gtNewLclvNode(tmp1->AsLclVar()->GetLclNum(), TYP_SIMD16);
        vec           = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_UnpackLow, TYP_UBYTE, 16, tmp1, tmp2);
        BlockRange().InsertAfter(tmp1, tmp2, vec);
        LowerNode(vec);

        eltType = TYP_USHORT;
    }

    if (varTypeIsShort(eltType))
    {
        node->SetOp(0, vec);
        LIR::Use use(BlockRange(), &node->GetUse(0).NodeRef(), node);
        GenTree* tmp1 = ReplaceWithLclVar(use);
        GenTree* tmp2 = comp->gtNewLclvNode(tmp1->AsLclVar()->GetLclNum(), TYP_SIMD16);
        vec           = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_UnpackLow, TYP_USHORT, 16, tmp1, tmp2);
        BlockRange().InsertAfter(tmp1, tmp2, vec);
        LowerNode(vec);

        eltType = TYP_UINT;
    }

    assert((eltType == TYP_INT) || (eltType == TYP_UINT));

    GenTree* idx = comp->gtNewIconNode(0);
    BlockRange().InsertBefore(node, idx);
    node->SetIntrinsic(NI_SSE2_Shuffle, TYP_UINT, 2);
    node->SetOp(0, vec);
    node->SetOp(1, idx);
}

void Lowering::LowerHWIntrinsicCreateConst(GenTreeHWIntrinsic* node, const VectorConstant& vecConst)
{
    unsigned numOps = node->GetNumOps();

    for (unsigned i = 0; i < numOps; i++)
    {
#ifndef TARGET_64BIT
        if (node->GetOp(i)->OperIs(GT_LONG))
        {
            BlockRange().Remove(node->GetOp(i)->AsOp()->GetOp(0));
            BlockRange().Remove(node->GetOp(i)->AsOp()->GetOp(1));
        }
#endif

        BlockRange().Remove(node->GetOp(i));
    }

    unsigned size = node->GetSimdSize();

    if (vecConst.AllBitsZero(size))
    {
        node->SetIntrinsic((size <= 16) ? NI_Vector128_get_Zero : NI_Vector256_get_Zero);
        node->SetNumOps(0);
        return;
    }

    if ((size >= 16) && vecConst.AllBitsOne(size))
    {
        node->SetIntrinsic((size == 16) ? NI_Vector128_get_AllBitsSet : NI_Vector256_get_AllBitsSet);
        node->SetNumOps(0);
        return;
    }

    var_types type = getSIMDTypeForSize(size);
    size           = (size != 12) ? size : 16;
    unsigned align = (comp->compCodeOpt() != Compiler::SMALL_CODE) ? size : emitter::dataSection::MIN_DATA_ALIGN;

    UNATIVE_OFFSET       offset = comp->GetEmitter()->emitDataConst(vecConst.u8, size, align, type);
    CORINFO_FIELD_HANDLE handle = comp->eeFindJitDataOffs(offset);

    GenTree* addr = new (comp, GT_CLS_VAR_ADDR) GenTreeClsVar(handle);
    BlockRange().InsertBefore(node, addr);

    GenTree* indir = node;
    indir->ChangeOper(GT_IND);
    indir->AsIndir()->SetAddr(addr);
}

void Lowering::LowerHWIntrinsicGetElement(GenTreeHWIntrinsic* node)
{
    GenTree* vec = node->GetOp(0);
    GenTree* idx = node->GetOp(1);

    if (IsContainableMemoryOp(vec) && IsSafeToContainMem(node, vec))
    {
        vec->SetContained();
    }

    if (!idx->IsIntCon())
    {
        if (!vec->isContained())
        {
            unsigned tempLclNum = GetSimdMemoryTemp(vec->GetType());
            GenTree* store      = comp->gtNewStoreLclVar(tempLclNum, vec->GetType(), vec);
            BlockRange().InsertAfter(vec, store);

            vec = comp->gtNewLclvNode(tempLclNum, vec->GetType());
            BlockRange().InsertBefore(node, vec);
            node->SetOp(0, vec);
            vec->SetContained();
        }
        else if (GenTreeIndir* indir = vec->IsIndir())
        {
            indir->GetAddr()->ClearContained();
        }

#ifdef TARGET_64BIT
        // TODO-MIKE-CQ: Most of the time this isn't necessary as the index is usually
        // produced by a 32 bit instruction that implicitly zero extends. CAST codegen
        // attempts to eliminate such redundant casts but it rarely succeeds.
        idx = comp->gtNewCastNode(TYP_LONG, idx, true, TYP_LONG);
        BlockRange().InsertBefore(node, idx);
        node->SetOp(1, idx);
#endif

        return;
    }

    var_types eltType = node->GetSimdBaseType();

    // We should have a bounds check inserted for any index outside the allowed range
    // but we need to generate some code anyways, and so we'll mask here for simplicity.

    unsigned count = node->GetSimdSize() / varTypeSize(eltType);
    unsigned index = idx->AsIntCon()->GetUInt32Value() % count;

    idx->AsIntCon()->SetValue(index);
    idx->SetContained();

    if (vec->isContained())
    {
        if (GenTreeIndir* indir = vec->IsIndir())
        {
            GenTree* addr = indir->GetAddr();

            if (addr->isContained())
            {
                int offset = static_cast<int>(index * varTypeSize(eltType));

                addr->SetContained(addr->IsAddrMode() && (addr->AsAddrMode()->GetOffset() <= INT32_MAX - offset));
            }
        }

        return;
    }

    if (node->GetIntrinsic() == NI_Vector256_GetElement)
    {
        assert(comp->compIsaSupportedDebugOnly(InstructionSet_AVX));

        if (index >= count / 2)
        {
            index -= count / 2;
            idx->AsIntCon()->SetValue(index);

            GenTree* one = comp->gtNewIconNode(1);
            vec          = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AVX_ExtractVector128, eltType, 32, vec, one);
            BlockRange().InsertBefore(node, one, vec);
        }
        else
        {
            vec = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector256_GetLower, eltType, 16, vec);
            BlockRange().InsertBefore(node, vec);
        }

        LowerNode(vec);

        node->SetIntrinsic(NI_Vector128_GetElement);
        node->SetSimdSize(16);
        node->SetOp(0, vec);
    }

    if (varTypeIsFloating(eltType))
    {
        // Defer to codegen to avoid having to create temps for shuffle/unpack.
        return;
    }

    if ((index != 0) && !varTypeIsShort(eltType) && !comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        idx->AsIntCon()->SetValue(index * varTypeSize(eltType));
        vec = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical128BitLane, eltType, 16, vec, idx);
        BlockRange().InsertBefore(node, vec);
        node->SetOp(0, vec);
        index = 0;
        idx   = nullptr;
    }

    if ((index != 0) || (eltType == TYP_USHORT) ||
        ((eltType == TYP_UBYTE) && comp->compOpportunisticallyDependsOn(InstructionSet_SSE41)))
    {
        switch (eltType)
        {
            case TYP_LONG:
            case TYP_ULONG:
                node->SetIntrinsic(NI_SSE41_X64_Extract);
                break;
            case TYP_BYTE:
            case TYP_UBYTE:
            case TYP_INT:
            case TYP_UINT:
                node->SetIntrinsic(NI_SSE41_Extract);
                break;
            case TYP_SHORT:
            case TYP_USHORT:
                node->SetIntrinsic(NI_SSE2_Extract);
                break;
            default:
                unreached();
        }
    }
    else
    {
        switch (eltType)
        {
            case TYP_LONG:
                node->SetIntrinsic(NI_SSE2_X64_ConvertToInt64, eltType, 1);
                break;
            case TYP_ULONG:
                node->SetIntrinsic(NI_SSE2_X64_ConvertToUInt64, eltType, 1);
                break;
            case TYP_INT:
                node->SetIntrinsic(NI_SSE2_ConvertToInt32, eltType, 1);
                break;
            case TYP_UINT:
                node->SetIntrinsic(NI_SSE2_ConvertToUInt32, eltType, 1);
                break;
            case TYP_SHORT:
            case TYP_USHORT:
            case TYP_BYTE:
            case TYP_UBYTE:
                node->SetIntrinsic(NI_SSE2_ConvertToInt32, TYP_INT, 1);
                break;
            default:
                unreached();
        }

        node->SetOp(0, vec);

        if (idx != nullptr)
        {
            BlockRange().Remove(idx);
        }
    }

    LowerNode(node);

    if ((eltType == TYP_BYTE) || (eltType == TYP_SHORT) ||
        ((eltType == TYP_UBYTE) && !comp->compOpportunisticallyDependsOn(InstructionSet_SSE41)))
    {
        LIR::Use use;
        if (BlockRange().TryGetUse(node, &use))
        {
            GenTreeCast* cast = comp->gtNewCastNode(TYP_INT, node, false, eltType);
            BlockRange().InsertAfter(node, cast);
            use.ReplaceWith(comp, cast);
            LowerNode(cast);
        }
    }
}

void Lowering::LowerHWIntrinsicWithElement(GenTreeHWIntrinsic* node)
{
    var_types      eltType = node->GetSimdBaseType();
    GenTree*       vec     = node->GetOp(0);
    GenTreeIntCon* idx     = node->GetOp(1)->AsIntCon();
    GenTree*       elt     = node->GetOp(2);
    unsigned       index   = idx->GetUInt32Value();
    unsigned       count   = node->GetSimdSize() / varTypeSize(eltType);

    assert(index < count);

    unsigned vec256TempLclNum = BAD_VAR_NUM;
    unsigned index256         = index;

    if (node->GetIntrinsic() == NI_Vector256_WithElement)
    {
        assert(comp->compIsaSupportedDebugOnly(InstructionSet_AVX));

        LIR::Use vecUse(BlockRange(), &node->GetUse(0).NodeRef(), node);
        vec              = ReplaceWithLclVar(vecUse);
        vec256TempLclNum = vec->AsLclVar()->GetLclNum();

        if (index >= count / 2)
        {
            index256 = index;
            index -= count / 2;
            idx->AsIntCon()->SetValue(index);

            GenTree* one = comp->gtNewIconNode(1);
            vec          = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AVX_ExtractVector128, eltType, 32, vec, one);
            BlockRange().InsertBefore(node, one, vec);
        }
        else
        {
            vec = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector256_GetLower, eltType, 16, vec);
            BlockRange().InsertBefore(node, vec);
        }

        LowerNode(vec);
    }

#ifndef TARGET_64BIT
    if (varTypeIsLong(eltType))
    {
        assert(elt->OperIs(GT_LONG));
        assert(comp->compIsaSupportedDebugOnly(InstructionSet_SSE41));

        index *= 2;
        index256 *= 2;
        eltType = TYP_INT;

        GenTree* eltLo = elt->AsOp()->GetOp(0);
        GenTree* idxLo = comp->gtNewIconNode(index);
        vec = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_WithElement, eltType, 16, vec, idxLo, eltLo);
        BlockRange().InsertBefore(node, idxLo, vec);
        LowerNode(vec);

        index++;
        BlockRange().Remove(elt);
        elt = elt->AsOp()->GetOp(1);

        node->SetSimdBaseType(eltType);
        node->SetOp(0, vec);
        idx->SetValue(index);
        node->SetOp(2, elt);
    }
#endif

    NamedIntrinsic intrinsic;

    switch (eltType)
    {
        case TYP_SHORT:
        case TYP_USHORT:
            intrinsic = NI_SSE2_Insert;
            break;
        case TYP_BYTE:
        case TYP_UBYTE:
        case TYP_INT:
        case TYP_UINT:
            assert(comp->compIsaSupportedDebugOnly(InstructionSet_SSE41));
            intrinsic = NI_SSE41_Insert;
            break;
#ifdef TARGET_64BIT
        case TYP_LONG:
        case TYP_ULONG:
            assert(comp->compIsaSupportedDebugOnly(InstructionSet_SSE41_X64));
            intrinsic = NI_SSE41_X64_Insert;
            break;
#endif

        case TYP_DOUBLE:
            intrinsic = (index == 0) ? NI_SSE2_MoveScalar : NI_SSE2_UnpackLow;
            BlockRange().Remove(idx);
            idx = nullptr;
            elt = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_DOUBLE, 16, elt);
            BlockRange().InsertBefore(node, elt);
            LowerNode(elt);
            break;

        case TYP_FLOAT:
            if (comp->compOpportunisticallyDependsOn(InstructionSet_SSE41) && ((index != 0) || elt->IsDblCon()))
            {
                intrinsic = NI_SSE41_Insert;
                idx->AsIntCon()->SetIconValue(index << 4);
            }
            else if (index == 0)
            {
                intrinsic = NI_SSE_MoveScalar;
                BlockRange().Remove(idx);
                idx = nullptr;
                elt = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_FLOAT, 16, elt);
                BlockRange().InsertBefore(node, elt);
                LowerNode(elt);
            }
            else
            {
                node->SetOp(0, vec);
                LIR::Use op1Use(BlockRange(), &node->GetUse(0).NodeRef(), node);
                vec = ReplaceWithLclVar(op1Use);

                elt = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_FLOAT, 16, elt);
                BlockRange().InsertBefore(node, elt);
                LowerNode(elt);

                GenTree*      vec2 = comp->gtNewLclvNode(vec->AsLclVar()->GetLclNum(), TYP_SIMD16);
                constexpr int controlBits1[]{0, 0, 0b00110000, 0b00100000};
                GenTree*      imm = comp->gtNewIconNode(controlBits1[index]);
                elt = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_Shuffle, TYP_FLOAT, 16, elt, vec2, imm);
                BlockRange().InsertBefore(node, vec2, imm, elt);
                LowerNode(elt);

                intrinsic = NI_SSE_Shuffle;
                constexpr int controlBits2[]{0, 0b11100010, 0b10000100, 0b00100100};
                idx->AsIntCon()->SetValue(controlBits2[index]);

                if (index == 1)
                {
                    std::swap(vec, elt);
                }
            }
            break;

        default:
            unreached();
    }

    if (vec256TempLclNum != BAD_VAR_NUM)
    {
        if (idx == nullptr)
        {
            elt = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, eltType, 16, vec, elt);
        }
        else
        {
            elt = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, eltType, 16, vec, elt, idx);
        }

        intrinsic = NI_AVX_InsertVector128;

        vec = comp->gtNewLclvNode(vec256TempLclNum, TYP_SIMD32);
        idx = comp->gtNewIconNode((index256 >= count / 2) ? 1 : 0);

        BlockRange().InsertBefore(node, vec, elt, idx);
        LowerNode(vec);
        LowerNode(elt);
    }

    node->SetIntrinsic(intrinsic, idx == nullptr ? 2 : 3);
    node->SetOp(0, vec);
    node->SetOp(1, elt);

    if (idx != nullptr)
    {
        node->SetOp(2, idx);
    }

    LowerNode(node);
}

void Lowering::LowerHWIntrinsicInsertFloat(GenTreeHWIntrinsic* node)
{
    assert((node->GetIntrinsic() == NI_SSE41_Insert) && (node->GetSimdBaseType() == TYP_FLOAT));

    GenTree* vec = node->GetOp(0);
    GenTree* elt = node->GetOp(1);
    GenTree* imm = node->GetOp(2);

    if (!imm->IsIntCon())
    {
        return;
    }

    if ((imm->AsIntCon()->GetUInt8Value() >> 6) == 0)
    {
        // If the first element of a vector is inserted then we may be able to change that
        // to a float value so it can be contained if it's a contant or memory location.

        if (GenTreeHWIntrinsic* vecElt = elt->IsHWIntrinsic())
        {
            switch (vecElt->GetIntrinsic())
            {
                case NI_Vector128_CreateScalarUnsafe:
                    elt = vecElt->GetOp(0);
                    node->SetOp(1, elt);
                    BlockRange().Remove(vecElt);
                    break;
                case NI_SSE_LoadScalarVector128:
                case NI_SSE_LoadVector128:
                case NI_SSE_LoadAlignedVector128:
                    GenTree* addr;
                    addr = vecElt->GetOp(0);
                    elt->ChangeOper(GT_IND);
                    elt->SetType(TYP_FLOAT);
                    elt->AsIndir()->SetAddr(addr);
                    break;
                default:
                    break;
            }
        }
        else if (elt->OperIs(GT_IND, GT_LCL_FLD))
        {
            elt->SetType(TYP_FLOAT);
        }
        else if (elt->OperIs(GT_LCL_VAR) && comp->lvaGetDesc(elt->AsLclVar())->lvDoNotEnregister)
        {
            elt->ChangeOper(GT_LCL_FLD);
            elt->SetType(TYP_FLOAT);
        }
    }

    ContainHWIntrinsicInsertFloat(node);
}

void Lowering::ContainHWIntrinsicInsertFloat(GenTreeHWIntrinsic* node)
{
    assert((node->GetIntrinsic() == NI_SSE41_Insert) && (node->GetSimdBaseType() == TYP_FLOAT));

    GenTree* vec = node->GetOp(0);
    GenTree* elt = node->GetOp(1);
    GenTree* imm = node->GetOp(2);

    if (!imm->IsIntCon())
    {
        return;
    }

    imm->SetContained();

    // FLOAT constants and memory operands can be contained. This is true even for 0.0f, codegen
    // will just use the vector source register as the element source register and adjust immValue to
    // zero out the element.

    if (elt->TypeIs(TYP_FLOAT))
    {
        if (elt->IsDblCon() || (IsContainableMemoryOp(elt) && IsSafeToContainMem(node, elt)))
        {
            elt->SetContained();
            return;
        }
    }

    // We can contain 0 if we insert into it, codegen will use the element source register as the
    // vector source register and adjust the immValue to zero out the rest of the elements. That means
    // that we cannot make elt reg optional, we'd be left with no source registers. So we'll make
    // trade-off - only make elt reg optional if it is a LCL_VAR, otherwise it means that it's
    // more likely to already be in a register so reg optional isn't useful.

    if (vec->IsHWIntrinsicZero() && !elt->OperIs(GT_LCL_VAR) && comp->canUseVexEncoding())
    {
        vec->SetContained();
    }
    else if (elt->TypeIs(TYP_FLOAT))
    {
        elt->SetRegOptional();
    }
}

void Lowering::LowerHWIntrinsicSum128(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_Vector128_Sum);

    var_types eltType = node->GetSimdBaseType();
    unsigned  size    = node->GetSimdSize();

    assert(varTypeIsFloating(eltType) || (eltType == TYP_INT) || (eltType == TYP_LONG) || (eltType == TYP_SHORT));
    assert((size == 16) || ((eltType == TYP_FLOAT) && ((size == 8) || (size == 12))));

    NamedIntrinsic hadd = NI_Illegal;

    // TODO-MIKE-CQ: This still generates poor code. If VEX isn't available then we
    // get extra reg-reg moves due to poor register allocation and/or inability to
    // describe the exact requirements to LSRA. This happens even when HADD is used,
    // normally hadd should not need any moves because the operand is normally not a
    // LCL_VAR so we should simply get 2 x "HADD reg, reg".
    //
    // And then using HADD is rather questionable because it's slower than a shuffle
    // and an addition. HADD's only advantage is code size and not even that, it has
    // 3 uops (even 4 on Ryzen) instead of the 2 you get for suffle + add. So HADD
    // takes less space in the code cache but more space in the uop cache.
    //
    // It may be better to get rid of the HADD part and move the rest to codegen, to
    // ensure that no extra reg-reg moves are generated. The drawback of doing this
    // in codegen is that we may have to allocate a temp register that's not always
    // needed. But since the current register allocation isn't ideal that's unlikely
    // to be an issue. And moving this to codegen alsos avoid the need to allocate 2
    // temps (though it may be possible to allocate only 1 but the code will be more
    // complicated).

    if (((eltType == TYP_INT) || (eltType == TYP_SHORT)) && comp->compOpportunisticallyDependsOn(InstructionSet_SSSE3))
    {
        hadd = NI_SSSE3_HorizontalAdd;
    }
    else if (varTypeIsFloating(eltType) && comp->compOpportunisticallyDependsOn(InstructionSet_SSE3))
    {
        hadd = NI_SSE3_HorizontalAdd;
    }

    unsigned haddCount = genLog2(roundUp(size, 8) / varTypeSize(eltType));
    assert(haddCount <= 3);

    LIR::Use       vecUse(BlockRange(), &node->GetUse(0).NodeRef(), node);
    GenTreeLclVar* vec = ReplaceWithLclVar(vecUse);

    GenTree*       sum  = vec;
    GenTree*       sum2 = nullptr;
    NamedIntrinsic add  = NI_Illegal;

    for (unsigned i = 0; i < haddCount; i++)
    {
        sum2 = comp->gtNewLclvNode(sum->AsLclVar()->GetLclNum(), TYP_SIMD16);
        BlockRange().InsertBefore(node, sum2);

        if ((hadd != NI_Illegal) && ((size != 12) || (i == 0)))
        {
            add = hadd;
        }
        else
        {
            if ((eltType == TYP_INT) || (eltType == TYP_LONG) || (eltType == TYP_SHORT))
            {
                GenTree* imm = comp->gtNewIconNode(i == 0 ? 0b11101110 : 0b00010001);
                sum2         = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, sum2, imm);
                BlockRange().InsertBefore(node, imm, sum2);
            }
            else if ((i == 0) && (eltType == TYP_FLOAT))
            {
                GenTree* sum3 = comp->gtNewLclvNode(sum2->AsLclVar()->GetLclNum(), TYP_SIMD16);
                GenTree* imm  = comp->gtNewIconNode(0b10110001);
                sum2 = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_Shuffle, TYP_FLOAT, 16, sum2, sum3, imm);
                BlockRange().InsertBefore(node, sum3, imm, sum2);
            }
            else
            {
                assert(varTypeIsFloating(eltType));
                // For Vector3 we need to add the original vec[2] element,
                // not sum[2] which would be wrong if vec[3] wasn't 0.
                unsigned lclNum = size == 12 ? vec->GetLclNum() : sum2->AsLclVar()->GetLclNum();
                GenTree* sum3   = comp->gtNewLclvNode(lclNum, TYP_SIMD16);
                sum2 = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_MoveHighToLow, TYP_FLOAT, 16, sum2, sum3);
                BlockRange().InsertBefore(node, sum3, sum2);
            }

            LowerNode(sum2);
            add = eltType == TYP_FLOAT ? NI_SSE_Add : NI_SSE2_Add;
        }

        if (i < haddCount - 1)
        {
            sum = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, add, eltType, 16, sum, sum2);
            BlockRange().InsertBefore(node, sum);
            LowerNode(sum);
            node->SetOp(0, sum);
            LIR::Use sumUse(BlockRange(), &node->GetUse(0).NodeRef(), node);
            sum = ReplaceWithLclVar(sumUse);
        }
    }

    node->SetIntrinsic(add, eltType, 16, 2);
    node->SetOp(0, sum);
    node->SetOp(1, sum2);
    LowerNode(node);
}

void Lowering::LowerHWIntrinsicSum256(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_Vector256_Sum);
    assert(node->GetSimdSize() == 32);

    var_types eltType = node->GetSimdBaseType();
    GenTree*  vec     = node->GetOp(0);

    assert(varTypeIsArithmetic(eltType));

    NamedIntrinsic extract = varTypeIsFloating(eltType) ? NI_AVX_ExtractVector128 : NI_AVX2_ExtractVector128;
    NamedIntrinsic add     = eltType == TYP_FLOAT ? NI_SSE_Add : NI_SSE2_Add;

    LIR::Use vecUse(BlockRange(), &node->GetUse(0).NodeRef(), node);
    vec = ReplaceWithLclVar(vecUse);

    GenTree* vec2     = comp->gtNewLclvNode(vec->AsLclVar()->GetLclNum(), TYP_SIMD32);
    GenTree* imm      = comp->gtNewIconNode(1);
    GenTree* vecUpper = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, extract, eltType, 32, vec2, imm);
    BlockRange().InsertBefore(node, vec2, imm, vecUpper);
    LowerNode(vecUpper);

    node->SetIntrinsic(add, eltType, 16, 2);
    node->SetOp(0, vec);
    node->SetOp(1, vecUpper);
    LowerNode(node);
}

#endif // FEATURE_HW_INTRINSICS

bool Lowering::IsLoadIndRMWCandidate(GenTreeStoreInd* store, GenTreeIndir* load, GenTree* src)
{
    GenTree* loadAddr  = load->GetAddr();
    GenTree* storeAddr = store->GetAddr();

    if ((loadAddr->GetOper() != storeAddr->GetOper()) || !IndirsAreEquivalent(load, store))
    {
        return false;
    }

    // RMW stores require multiple interference checks to ensure corectness:
    //  - The RMW operation (e.g. ADD) needs to be moved before the store. This is
    //    trivial for the operation itself, it's always side effect free, but binary
    //    operations have a source operand that needs checking if it's a LCL_VAR.
    //  - The load needs to be moved before the store.
    //  - Load and store addresses need not be moved, it would be perfectly fine if
    //    the store address is computed into a register anywhere before the store.
    //    But this gets more complicated if addresses contain LCL_VAR uses because
    //    IndirsAreEquivalent only checks that the 2 addresses expressions are the
    //    same, not that they produce the same value.
    // Reg candidate LCL_VARs are treated as if they're contained - the register is
    // guaranteed to be available at user's position, not at LCL_VAR's position. So:
    //  - The LCL_VAR source of a binary operation must be safe to move before the store
    //    because we have to move the binary operation itself.
    //  - If addresses are LCL_VARs then the load address LCL_VAR must be safe to move
    //    before the store, it does not matter where the store address LCL_VAR is.
    //  - If addresses are LEAs that use LCL_VARs then the load address LCL_VARs must
    //    be safe to move before the store address LEA.
    // The last case is another complication when the LEA is not contained. We might
    // need to run two separate interference check traversals, one starting from the
    // store and one starting from the LEA. And then we don't even know where the load
    // address LCL_VARs are, they could be after the store address LEA. This kind of
    // interference is very rare anyway so to keep things simple we require that the
    // store address LEA is contained. So pretty much all the nodes involved in the
    // RMW store must be safe to move before the store.

    // TODO-MIKE-Review: We should only check LCL_VARs that are reg candidates. For
    // now we check all of them because existing code (AddNode) does it like this.

    m_scratchSideEffects.Clear();
    unsigned markCount = 0;

    m_scratchSideEffects.AddNode(comp, load);
    load->SetLIRMark();
    markCount++;

    if (GenTreeAddrMode* am = loadAddr->IsAddrMode())
    {
        // AddNode automatically adds the load address if it's a LCL_VAR but
        // it doesn not add LCL_VARs that are used as part of an address mode.
        // Note that we could pass the address mode directly to AddNode but
        // we still need to mark the LCL_VARs.

        if (GenTree* base = am->GetBase())
        {
            if (base->OperIs(GT_LCL_VAR))
            {
                m_scratchSideEffects.AddNode(comp, base);
                base->SetLIRMark();
                markCount++;
            }
        }

        if (GenTree* index = am->GetIndex())
        {
            if (index->OperIs(GT_LCL_VAR))
            {
                m_scratchSideEffects.AddNode(comp, index);
                index->SetLIRMark();
                markCount++;
            }
        }
    }
    else if (loadAddr->OperIs(GT_LCL_VAR))
    {
        // AddNode(load) already added this but we still need to mark it.
        loadAddr->SetLIRMark();
        markCount++;
    }

    if ((src != nullptr) && src->OperIs(GT_LCL_VAR))
    {
        m_scratchSideEffects.AddNode(comp, src);
        src->SetLIRMark();
        markCount++;
    }

    if (storeAddr->IsAddrMode())
    {
        assert(storeAddr->isContained());
    }
    else if (storeAddr->OperIs(GT_LCL_VAR))
    {
        m_scratchSideEffects.AddNode(comp, storeAddr);
        storeAddr->SetLIRMark();
        markCount++;
    }

    bool hasInterference = false;

    for (GenTree* node = store->gtPrev; markCount > 0; node = node->gtPrev)
    {
        if (node->HasLIRMark())
        {
            node->ClearLIRMark();
            markCount--;
            continue;
        }

        // TODO-MIKE-Review: Why does IsSafeToContainMem uses strict checking while this doesn't?
        hasInterference = hasInterference || m_scratchSideEffects.InterferesWith(comp, node, false);
    }

    return !hasInterference;
}

bool Lowering::IndirsAreEquivalent(GenTreeIndir* indir1, GenTreeIndir* indir2)
{
    assert(indir1->OperIs(GT_IND));
    assert(indir2->OperIs(GT_STOREIND));

    if (varTypeSize(indir1->GetType()) != varTypeSize(indir2->GetType()))
    {
        return false;
    }

    GenTree* addr1 = indir1->GetAddr();
    GenTree* addr2 = indir2->GetAddr();

    if (addr1->GetOper() != addr2->GetOper())
    {
        return false;
    }

    switch (addr1->GetOper())
    {
        case GT_LCL_VAR:
        case GT_CLS_VAR_ADDR:
        case GT_CNS_INT:
            return NodesAreEquivalentLeaves(addr1, addr2);

        case GT_LEA:
        {
            GenTreeAddrMode* am1 = addr1->AsAddrMode();
            GenTreeAddrMode* am2 = addr2->AsAddrMode();
            return NodesAreEquivalentLeaves(am1->GetBase(), am2->GetBase()) &&
                   NodesAreEquivalentLeaves(am1->GetIndex(), am2->GetIndex()) && (am1->GetScale() == am2->GetScale()) &&
                   (am1->GetOffset() == am2->GetOffset());
        }

        default:
            return false;
    }
}

bool Lowering::NodesAreEquivalentLeaves(GenTree* node1, GenTree* node2)
{
    if ((node1 == nullptr) || (node2 == nullptr))
    {
        return node1 == node2;
    }

    if ((node1->GetOper() != node2->GetOper()) || (node1->GetType() != node2->GetType()))
    {
        return false;
    }

    switch (node1->GetOper())
    {
        case GT_CNS_INT:
            return (node1->AsIntCon()->GetValue() == node2->AsIntCon()->GetValue()) &&
                   (node1->IsIconHandle() == node2->IsIconHandle());
        case GT_LCL_VAR:
            return node1->AsLclVar()->GetLclNum() == node2->AsLclVar()->GetLclNum();
        case GT_CLS_VAR_ADDR:
            return node1->AsClsVar()->GetFieldHandle() == node2->AsClsVar()->GetFieldHandle();
        default:
            return false;
    }
}

GenTreeIndir* Lowering::IsStoreIndRMW(GenTreeStoreInd* store)
{
    assert(varTypeIsIntegralOrI(store->GetType()));

    GenTree* storeAddr = store->GetAddr();

    if (!storeAddr->OperIs(GT_LEA, GT_LCL_VAR, GT_CLS_VAR_ADDR, GT_CNS_INT))
    {
        return nullptr;
    }

    if (storeAddr->IsAddrMode() && !storeAddr->isContained())
    {
        // Give up if the address is an uncontained LEA (likely due to base/index interference).
        // This is rare and ignoring it simplifies IsLoadIndRMWCandidate interference checking.
        return nullptr;
    }

    GenTree* op = store->GetValue();
    assert(op->OperIsRMWMemOp() && !op->gtOverflowEx());

    if (op->OperIsBinary())
    {
        if (op->OperIsShiftOrRotate() && varTypeIsSmall(store->GetType()))
        {
            return nullptr;
        }

        GenTree* op1 = op->AsOp()->GetOp(0);
        GenTree* op2 = op->AsOp()->GetOp(1);

        if (op->OperIsCommutative() && op2->OperIs(GT_IND) && IsLoadIndRMWCandidate(store, op2->AsIndir(), op1))
        {
            return op2->AsIndir();
        }

        if (op1->OperIs(GT_IND) && IsLoadIndRMWCandidate(store, op1->AsIndir(), op2))
        {
            return op1->AsIndir();
        }
    }
    else
    {
        assert(op->OperIsUnary());

        GenTree* op1 = op->AsUnOp()->GetOp(0);

        if (op1->OperIs(GT_IND) && IsLoadIndRMWCandidate(store, op1->AsIndir(), nullptr))
        {
            return op1->AsIndir();
        }
    }

    return nullptr;
}

// anything is in range for AMD64
bool Lowering::IsCallTargetInRange(void* addr)
{
    return true;
}

// return true if the immediate can be folded into an instruction, for example small enough and non-relocatable
bool Lowering::IsContainableImmed(GenTree* parentNode, GenTree* childNode) const
{
    return childNode->IsIntCnsFitsInI32() && !childNode->AsIntCon()->ImmedValNeedsReloc(comp);
}

//-----------------------------------------------------------------------
// PreferredRegOptionalOperand: returns one of the operands of given
// binary oper that is to be preferred for marking as reg optional.
//
// Since only one of op1 or op2 can be a memory operand on xarch, only
// one of  them have to be marked as reg optional.  Since Lower doesn't
// know apriori which of op1 or op2 is not likely to get a register, it
// has to make a guess. This routine encapsulates heuristics that
// guess whether it is likely to be beneficial to mark op1 or op2 as
// reg optional.
//
//
// Arguments:
//     tree  -  a binary-op tree node that is either commutative
//              or a compare oper.
//
// Returns:
//     Returns op1 or op2 of tree node that is preferred for
//     marking as reg optional.
//
// Note: if the tree oper is neither commutative nor a compare oper
// then only op2 can be reg optional on xarch and hence no need to
// call this routine.
GenTree* Lowering::PreferredRegOptionalOperand(GenTree* tree)
{
    assert(GenTree::OperIsBinary(tree->OperGet()));
    assert(tree->OperIsCommutative() || tree->OperIsCompare() || tree->OperIs(GT_CMP));

    GenTree* op1 = tree->gtGetOp1();
    GenTree* op2 = tree->gtGetOp2();
    assert(!op1->IsRegOptional() && !op2->IsRegOptional());

    // We default to op1, as op2 is likely to have the shorter lifetime.
    GenTree* preferredOp = op1;

    // This routine uses the following heuristics:
    //
    // a) If both are register candidates, marking the one with lower weighted
    // ref count as reg-optional would likely be beneficial as it has
    // higher probability of not getting a register. Note that we use !lvDoNotEnregister
    // here because this is being done while we are adding lclVars for Lowering.
    //
    // b) op1 = tracked local and op2 = untracked local: LSRA creates two
    // ref positions for op2: a def and use position. op2's def position
    // requires a reg and it is allocated a reg by spilling another
    // interval (if required) and that could be even op1.  For this reason
    // it is beneficial to mark op1 as reg optional.
    //
    // TODO: It is not always mandatory for a def position of an untracked
    // local to be allocated a register if it is on rhs of an assignment
    // and its use position is reg-optional and has not been assigned a
    // register.  Reg optional def positions is currently not yet supported.
    //
    // c) op1 = untracked local and op2 = tracked local: marking op1 as
    // reg optional is beneficial, since its use position is less likely
    // to get a register.
    //
    // d) If both are untracked locals (i.e. treated like tree temps by
    // LSRA): though either of them could be marked as reg optional,
    // marking op1 as reg optional is likely to be beneficial because
    // while allocating op2's def position, there is a possibility of
    // spilling op1's def and in which case op1 is treated as contained
    // memory operand rather than requiring to reload.
    //
    // e) If only one of them is a local var, prefer to mark it as
    // reg-optional.  This is heuristic is based on the results
    // obtained against CQ perf benchmarks.
    //
    // f) If neither of them are local vars (i.e. tree temps), prefer to
    // mark op1 as reg optional for the same reason as mentioned in (d) above.
    if (op1->OperGet() == GT_LCL_VAR && op2->OperGet() == GT_LCL_VAR)
    {
        LclVarDsc* v1 = comp->lvaTable + op1->AsLclVarCommon()->GetLclNum();
        LclVarDsc* v2 = comp->lvaTable + op2->AsLclVarCommon()->GetLclNum();

        bool v1IsRegCandidate = !v1->lvDoNotEnregister;
        bool v2IsRegCandidate = !v2->lvDoNotEnregister;
        if (v1IsRegCandidate && v2IsRegCandidate)
        {
            // Both are enregisterable locals.  The one with lower weight is less likely
            // to get a register and hence beneficial to mark the one with lower
            // weight as reg optional.
            // If either is not tracked, it may be that it was introduced after liveness
            // was run, in which case we will always prefer op1 (should we use raw refcnt??).
            if (v1->lvTracked && v2->lvTracked && (v1->lvRefCntWtd() >= v2->lvRefCntWtd()))
            {
                preferredOp = op2;
            }
        }
    }
    else if (!(op1->OperGet() == GT_LCL_VAR) && (op2->OperGet() == GT_LCL_VAR))
    {
        preferredOp = op2;
    }

    return preferredOp;
}

//------------------------------------------------------------------------
// Containment analysis
//------------------------------------------------------------------------

//------------------------------------------------------------------------
// ContainCheckCallOperands: Determine whether operands of a call should be contained.
//
// Arguments:
//    call       - The call node of interest
//
// Return Value:
//    None.
//
void Lowering::ContainCheckCallOperands(GenTreeCall* call)
{
    GenTree* ctrlExpr = call->gtControlExpr;
    if (call->gtCallType == CT_INDIRECT)
    {
        // either gtControlExpr != null or gtCallAddr != null.
        // Both cannot be non-null at the same time.
        assert(ctrlExpr == nullptr);
        assert(call->gtCallAddr != nullptr);
        ctrlExpr = call->gtCallAddr;

#ifdef TARGET_X86
        // Fast tail calls aren't currently supported on x86, but if they ever are, the code
        // below that handles indirect VSD calls will need to be fixed.
        assert(!call->IsFastTailCall() || !call->IsVirtualStub());
#endif // TARGET_X86
    }

    // set reg requirements on call target represented as control sequence.
    if (ctrlExpr != nullptr)
    {
        // we should never see a gtControlExpr whose type is void.
        assert(ctrlExpr->TypeGet() != TYP_VOID);

        // In case of fast tail implemented as jmp, make sure that gtControlExpr is
        // computed into a register.
        if (!call->IsFastTailCall())
        {
#ifdef TARGET_X86
            // On x86, we need to generate a very specific pattern for indirect VSD calls:
            //
            //    3-byte nop
            //    call dword ptr [eax]
            //
            // Where EAX is also used as an argument to the stub dispatch helper. Make
            // sure that the call target address is computed into EAX in this case.
            if (call->IsVirtualStub() && (call->gtCallType == CT_INDIRECT))
            {
                assert(ctrlExpr->isIndir());
                MakeSrcContained(call, ctrlExpr);
            }
            else
#endif // TARGET_X86
                if (ctrlExpr->isIndir())
            {
                // We may have cases where we have set a register target on the ctrlExpr, but if it
                // contained we must clear it.
                ctrlExpr->SetRegNum(REG_NA);
                MakeSrcContained(call, ctrlExpr);
            }
        }
    }
}

//------------------------------------------------------------------------
// ContainCheckIndir: Determine whether operands of an indir should be contained.
//
// Arguments:
//    node       - The indirection node of interest
//
// Notes:
//    This is called for both store and load indirections. In the former case, it is assumed that
//    LowerStoreIndir() has already been called to check for RMW opportunities.
//
// Return Value:
//    None.
//
void Lowering::ContainCheckIndir(GenTreeIndir* node)
{
    // If this is the rhs of a block copy it will be handled when we handle the store.
    if (node->TypeGet() == TYP_STRUCT)
    {
        return;
    }

    if ((node->gtFlags & GTF_IND_REQ_ADDR_IN_REG) != 0)
    {
        return;
    }

    GenTree* addr = node->GetAddr();

#ifdef FEATURE_SIMD
    if (node->TypeIs(TYP_SIMD12) && (!addr->IsAddrMode() || (addr->AsAddrMode()->GetOffset() > INT32_MAX - 8)))
    {
        return;
    }
#endif

    if (addr->OperIs(GT_CLS_VAR_ADDR, GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR))
    {
        addr->SetContained();
    }
    else if (addr->IsIntCon() && addr->AsIntCon()->FitsInAddrBase(comp))
    {
        addr->SetContained();
    }
    else if (addr->IsAddrMode() && IsSafeToMoveForward(addr, node))
    {
        addr->SetContained();
    }
}

void Lowering::ContainCheckStoreIndir(GenTreeStoreInd* store)
{
    ContainCheckIndir(store);

    GenTree* value = store->GetValue();

#ifdef FEATURE_SIMD
    if (store->TypeIs(TYP_SIMD12))
    {
        ContainSIMD12MemToMemCopy(store, value);
        return;
    }
#endif

    // If the source is a containable immediate, make it contained, unless it is
    // an int-size or larger store of zero to memory, because we can generate smaller code
    // by zeroing a register and then storing it.

    if (IsContainableImmed(store, value) &&
        (!value->IsIntegralConst(0) || varTypeIsSmall(store->GetType()) || store->GetAddr()->OperIs(GT_CLS_VAR_ADDR)))
    {
        MakeSrcContained(store, value);
    }
}

//------------------------------------------------------------------------
// ContainCheckMul: determine whether the sources of a MUL node should be contained.
//
// Arguments:
//    node - pointer to the node
//
void Lowering::ContainCheckMul(GenTreeOp* node)
{
#if defined(TARGET_X86)
    assert(node->OperIs(GT_MUL, GT_MULHI, GT_MUL_LONG));
#else
    assert(node->OperIs(GT_MUL, GT_MULHI));
#endif

    // Case of float/double mul.
    if (varTypeIsFloating(node->TypeGet()))
    {
        ContainCheckFloatBinary(node);
        return;
    }

    GenTree* op1 = node->AsOp()->gtOp1;
    GenTree* op2 = node->AsOp()->gtOp2;

    bool isSafeToContainOp1 = true;
    bool isSafeToContainOp2 = true;

    bool     isUnsignedMultiply    = ((node->gtFlags & GTF_UNSIGNED) != 0);
    bool     requiresOverflowCheck = node->gtOverflowEx();
    bool     useLeaEncoding        = false;
    GenTree* memOp                 = nullptr;

    bool                 hasImpliedFirstOperand = false;
    GenTreeIntConCommon* imm                    = nullptr;
    GenTree*             other                  = nullptr;

    // Multiply should never be using small types
    assert(!varTypeIsSmall(node->TypeGet()));

    // We do use the widening multiply to implement
    // the overflow checking for unsigned multiply
    //
    if (isUnsignedMultiply && requiresOverflowCheck)
    {
        hasImpliedFirstOperand = true;
    }
    else if (node->OperGet() == GT_MULHI)
    {
        hasImpliedFirstOperand = true;
    }
#if defined(TARGET_X86)
    else if (node->OperGet() == GT_MUL_LONG)
    {
        hasImpliedFirstOperand = true;
    }
#endif
    else if (IsContainableImmed(node, op2) || IsContainableImmed(node, op1))
    {
        if (IsContainableImmed(node, op2))
        {
            imm   = op2->AsIntConCommon();
            other = op1;
        }
        else
        {
            imm   = op1->AsIntConCommon();
            other = op2;
        }

        // CQ: We want to rewrite this into a LEA
        ssize_t immVal = imm->AsIntConCommon()->IconValue();
        if (!requiresOverflowCheck && (immVal == 3 || immVal == 5 || immVal == 9))
        {
            useLeaEncoding = true;
        }

        MakeSrcContained(node, imm); // The immValue is always contained
        if (IsContainableMemoryOp(other))
        {
            memOp = other; // memOp may be contained below
        }
    }

    // We allow one operand to be a contained memory operand.
    // The memory op type must match with the 'node' type.
    // This is because during codegen we use 'node' type to derive EmitTypeSize.
    // E.g op1 type = byte, op2 type = byte but GT_MUL node type is int.
    //
    if (memOp == nullptr)
    {
        if ((op2->TypeGet() == node->TypeGet()) && IsContainableMemoryOp(op2))
        {
            isSafeToContainOp2 = IsSafeToContainMem(node, op2);
            if (isSafeToContainOp2)
            {
                memOp = op2;
            }
        }

        if ((memOp == nullptr) && (op1->TypeGet() == node->TypeGet()) && IsContainableMemoryOp(op1))
        {
            isSafeToContainOp1 = IsSafeToContainMem(node, op1);
            if (isSafeToContainOp1)
            {
                memOp = op1;
            }
        }
    }
    else
    {
        if ((memOp->TypeGet() != node->TypeGet()))
        {
            memOp = nullptr;
        }
        else if (!IsSafeToContainMem(node, memOp))
        {
            if (memOp == op1)
            {
                isSafeToContainOp1 = false;
            }
            else
            {
                isSafeToContainOp2 = false;
            }
            memOp = nullptr;
        }
    }
    // To generate an LEA we need to force memOp into a register
    // so don't allow memOp to be 'contained'
    //
    if (!useLeaEncoding)
    {
        if (memOp != nullptr)
        {
            MakeSrcContained(node, memOp);
        }
        else
        {
            // IsSafeToContainMem is expensive so we call it at most once for each operand
            // in this method. If we already called IsSafeToContainMem, it must have returned false;
            // otherwise, memOp would be set to the corresponding operand (op1 or op2).
            if (imm != nullptr)
            {
                // Has a contained immediate operand.
                // Only 'other' operand can be marked as reg optional.
                assert(other != nullptr);

                isSafeToContainOp1 = ((other == op1) && isSafeToContainOp1 && IsSafeToContainMem(node, op1));
                isSafeToContainOp2 = ((other == op2) && isSafeToContainOp2 && IsSafeToContainMem(node, op2));
            }
            else if (hasImpliedFirstOperand)
            {
                // Only op2 can be marked as reg optional.
                isSafeToContainOp1 = false;
                isSafeToContainOp2 = isSafeToContainOp2 && IsSafeToContainMem(node, op2);
            }
            else
            {
                // If there are no containable operands, we can make either of op1 or op2
                // as reg optional.
                isSafeToContainOp1 = isSafeToContainOp1 && IsSafeToContainMem(node, op1);
                isSafeToContainOp2 = isSafeToContainOp2 && IsSafeToContainMem(node, op2);
            }
            SetRegOptionalForBinOp(node, isSafeToContainOp1, isSafeToContainOp2);
        }
    }
}

//------------------------------------------------------------------------
// ContainCheckDivOrMod: determine which operands of a div/mod should be contained.
//
// Arguments:
//    node - pointer to the node
//
void Lowering::ContainCheckDivOrMod(GenTreeOp* node)
{
    assert(node->OperIs(GT_DIV, GT_MOD, GT_UDIV, GT_UMOD));

    if (varTypeIsFloating(node->TypeGet()))
    {
        ContainCheckFloatBinary(node);
        return;
    }

    GenTree* divisor = node->gtGetOp2();

    bool divisorCanBeRegOptional = true;
#ifdef TARGET_X86
    GenTree* dividend = node->gtGetOp1();
    if (dividend->OperGet() == GT_LONG)
    {
        divisorCanBeRegOptional = false;
        MakeSrcContained(node, dividend);
    }
#endif

    // divisor can be an r/m, but the memory indirection must be of the same size as the divide
    if (IsContainableMemoryOp(divisor) && (divisor->TypeGet() == node->TypeGet()))
    {
        MakeSrcContained(node, divisor);
    }
    else if (divisorCanBeRegOptional)
    {
        // If there are no containable operands, we can make an operand reg optional.
        // Div instruction allows only divisor to be a memory op.
        divisor->SetRegOptional();
    }
}

//------------------------------------------------------------------------
// ContainCheckShiftRotate: determine whether the sources of a shift/rotate node should be contained.
//
// Arguments:
//    node - pointer to the node
//
void Lowering::ContainCheckShiftRotate(GenTreeOp* node)
{
    assert(node->OperIsShiftOrRotate());
#ifdef TARGET_X86
    GenTree* source = node->gtOp1;
    if (node->OperIsShiftLong())
    {
        assert(source->OperGet() == GT_LONG);
        MakeSrcContained(node, source);
    }
#endif // !TARGET_X86

    GenTree* shiftBy = node->gtOp2;
    if (IsContainableImmed(node, shiftBy) && (shiftBy->AsIntConCommon()->IconValue() <= 255) &&
        (shiftBy->AsIntConCommon()->IconValue() >= 0))
    {
        MakeSrcContained(node, shiftBy);
    }
}

void Lowering::ContainCheckStoreLcl(GenTreeLclVarCommon* store)
{
    assert(store->OperIsLocalStore());
    GenTree* src = store->gtGetOp1();

    if (src->OperIs(GT_BITCAST))
    {
        // If we know that the source of the bitcast will be in a register, then we can make
        // the bitcast itself contained. This will allow us to store directly from the other
        // type if this node doesn't get a register.
        GenTree* bitCastSrc = src->AsUnOp()->GetOp(0);

        // TODO-MIKE-Cleanup: Magic division lowering creates broken LONG to INT BITCASTs
        // that causes problems in codegen. Only allow containment if types have the same
        // size.

        if (!bitCastSrc->isContained() && !bitCastSrc->IsRegOptional() &&
            (varTypeSize(bitCastSrc->GetType()) == varTypeSize(src->GetType())))
        {
            src->SetContained();
            return;
        }
    }

#ifdef FEATURE_SIMD
    if (varTypeIsSIMD(store->GetType()))
    {
        assert(!src->IsIntCon());

        if (store->TypeIs(TYP_SIMD12) && IsContainableMemoryOp(store))
        {
            ContainSIMD12MemToMemCopy(store, src);
        }

        return;
    }
#endif

#ifdef TARGET_X86
    if (src->OperIs(GT_LONG))
    {
        src->SetContained();
        return;
    }
#endif

    // If the source is a containable immediate, make it contained, unless it is
    // an int-size or larger store of zero to memory, because we can generate smaller code
    // by zeroing a register and then storing it.

    var_types type = comp->lvaGetDesc(store)->GetRegisterType(store);

    if (IsContainableImmed(store, src) && (!src->IsIntegralConst(0) || varTypeIsSmall(type)))
    {
        src->SetContained();
        return;
    }

    if (IsContainableMemoryOp(store) && varTypeIsIntegral(store->GetType()) && src->OperIsRMWMemOp() &&
        !src->gtOverflowEx() && ((src->gtFlags & (GTF_SET_FLAGS | GTF_USE_FLAGS)) == 0))
    {
        // TODO-MIKE-CQ: This usually fails when address exposed small int LCL_VARs
        // are involved due to useless casts. The load is hidden by a widening cast
        // that's not really needed because LCL_VARs that load from memory do implicit
        // widening. There may also be a narrowing cast on stores to such locals, even
        // though it's not required due to load widening.

        GenTree* op1     = src->OperIsBinary() ? src->AsOp()->GetOp(0) : src->AsUnOp()->GetOp(0);
        GenTree* op2     = src->OperIsBinary() ? src->AsOp()->GetOp(1) : nullptr;
        unsigned lclNum  = store->GetLclNum();
        unsigned lclOffs = store->GetLclOffs();
        GenTree* load    = nullptr;

        if (op1->IsLclVarCommon() && (op1->AsLclVarCommon()->GetLclNum() == lclNum) &&
            (op1->AsLclVarCommon()->GetLclOffs() == lclOffs))
        {
            load = op1;
        }
        else if ((op2 != nullptr) && src->OperIsCommutative() && op2->IsLclVarCommon() &&
                 (op2->AsLclVarCommon()->GetLclNum() == lclNum) && (op2->AsLclVarCommon()->GetLclOffs() == lclOffs))
        {
            load = op2;
        }

        if ((load != nullptr) && (varTypeSize(load->GetType()) == varTypeSize(store->GetType())) &&
            (!varTypeIsSmall(load->GetType()) || !src->OperIsRotate()) &&
            (!varTypeIsSmallSigned(load->GetType()) || !src->OperIs(GT_RSZ)) && IsSafeToContainMem(store, load))
        {
            if (src->OperIs(GT_RSH) && varTypeIsSmallUnsigned(load->GetType()))
            {
                src->SetOper(GT_RSZ);
            }

            src->SetContained();
            load->SetContained();

            if (load == op2)
            {
                src->AsOp()->SetOp(0, load);
                src->AsOp()->SetOp(1, op1);
                op2 = op1;
            }

            if ((op2 != nullptr) && !op2->IsIntCon())
            {
                op2->ClearContained();
                op2->ClearRegOptional();
            }
        }
    }
}

void Lowering::ContainCheckCast(GenTreeCast* cast)
{
    GenTree* src = cast->GetOp(0);

#if !defined(TARGET_64BIT)
    if (src->OperIs(GT_LONG))
    {
        src->SetContained();
        return;
    }
#endif

    var_types srcType = src->GetType();
    var_types dstType = cast->GetCastType();

    if (varTypeIsIntegral(dstType) && varTypeIsIntegral(srcType))
    {
        if (IsContainableMemoryOp(src) && (!cast->gtOverflow() || IsSafeToContainMem(cast, src)))
        {
            // If this isn't an overflow checking cast then we can move it
            // right after the source node to avoid the interference check.
            if (!cast->gtOverflow() && (cast->gtPrev != src))
            {
                BlockRange().Remove(cast);
                BlockRange().InsertAfter(src, cast);
            }

            src->SetContained();
        }
        else
        {
            src->SetRegOptional();
        }
    }
    else if (varTypeIsFloating(dstType) || varTypeIsFloating(srcType))
    {
        assert(!cast->gtOverflow());

        // The source of cvtsi2sd and similar instructions can be a memory operand but it must
        // be 4 or 8 bytes in size so it cannot be a small int. It's likely possible to make a
        // "normalize on store" local reg-optional but it's probably not worth the extra work.
        // Also, ULONG to DOUBLE/FLOAT casts require checking the sign of the source so allowing
        // a memory operand would result in 2 loads instead of 1.
        if (!varTypeIsSmall(srcType) && ((srcType != TYP_LONG) || !cast->IsUnsigned()))
        {
            if (IsContainableMemoryOp(src))
            {
                // Since a floating point cast can't throw we can move the cast
                // right after the source node to avoid the interference check.
                if (cast->gtPrev != src)
                {
                    BlockRange().Remove(cast);
                    BlockRange().InsertAfter(src, cast);
                }

                src->SetContained();
            }
            else
            {
                src->SetRegOptional();
            }
        }
    }
}

//------------------------------------------------------------------------
// ContainCheckCompare: determine whether the sources of a compare node should be contained.
//
// Arguments:
//    node - pointer to the node
//
void Lowering::ContainCheckCompare(GenTreeOp* cmp)
{
    assert(cmp->OperIsCompare() || cmp->OperIs(GT_CMP));

    GenTree*  op1     = cmp->AsOp()->gtOp1;
    GenTree*  op2     = cmp->AsOp()->gtOp2;
    var_types op1Type = op1->TypeGet();
    var_types op2Type = op2->TypeGet();

    // If either of op1 or op2 is floating point values, then we need to use
    // ucomiss or ucomisd to compare, both of which support the following form:
    //     ucomis[s|d] xmm, xmm/mem
    // That is only the second operand can be a memory op.
    //
    // Second operand is a memory Op:  Note that depending on comparison operator,
    // the operands of ucomis[s|d] need to be reversed.  Therefore, either op1 or
    // op2 can be a memory op depending on the comparison operator.
    if (varTypeIsFloating(op1Type))
    {
        // The type of the operands has to be the same and no implicit conversions at this stage.
        assert(op1Type == op2Type);

        GenTree* otherOp;
        if (GenCondition::FromFloatRelop(cmp).PreferSwap())
        {
            otherOp = op1;
        }
        else
        {
            otherOp = op2;
        }

        assert(otherOp != nullptr);
        bool isSafeToContainOtherOp = true;
        if (otherOp->IsCnsNonZeroFltOrDbl())
        {
            MakeSrcContained(cmp, otherOp);
        }
        else if (IsContainableMemoryOp(otherOp))
        {
            isSafeToContainOtherOp = IsSafeToContainMem(cmp, otherOp);
            if (isSafeToContainOtherOp)
            {
                MakeSrcContained(cmp, otherOp);
            }
        }

        if (!otherOp->isContained() && isSafeToContainOtherOp && IsSafeToContainMem(cmp, otherOp))
        {
            // SSE2 allows only otherOp to be a memory-op. Since otherOp is not
            // contained, we can mark it reg-optional.
            // IsSafeToContainMem is expensive so we call it at most once for otherOp.
            // If we already called IsSafeToContainMem, it must have returned false;
            // otherwise, otherOp would be contained.
            otherOp->SetRegOptional();
        }

        return;
    }

    // TODO-XArch-CQ: factor out cmp optimization in 'genCondSetFlags' to be used here
    // or in other backend.

    if (CheckImmedAndMakeContained(cmp, op2))
    {
        // If the types are the same, or if the constant is of the correct size,
        // we can treat the MemoryOp as contained.
        if (op1Type == op2Type)
        {
            if (IsContainableMemoryOp(op1))
            {
                MakeSrcContained(cmp, op1);
            }
            else
            {
                op1->SetRegOptional();
            }
        }
    }
    else if (op1Type == op2Type)
    {
        // Note that TEST does not have a r,rm encoding like CMP has but we can still
        // contain the second operand because the emitter maps both r,rm and rm,r to
        // the same instruction code. This avoids the need to special case TEST here.

        bool isSafeToContainOp1 = true;
        bool isSafeToContainOp2 = true;

        if (IsContainableMemoryOp(op2))
        {
            isSafeToContainOp2 = IsSafeToContainMem(cmp, op2);
            if (isSafeToContainOp2)
            {
                MakeSrcContained(cmp, op2);
            }
        }

        if (!op2->isContained() && IsContainableMemoryOp(op1))
        {
            isSafeToContainOp1 = IsSafeToContainMem(cmp, op1);
            if (isSafeToContainOp1)
            {
                MakeSrcContained(cmp, op1);
            }
        }

        if (!op1->isContained() && !op2->isContained())
        {
            // One of op1 or op2 could be marked as reg optional
            // to indicate that codegen can still generate code
            // if one of them is on stack.
            GenTree* regOptionalCandidate = op1->IsCnsIntOrI() ? op2 : PreferredRegOptionalOperand(cmp);

            // IsSafeToContainMem is expensive so we call it at most once for each operand
            // in this method. If we already called IsSafeToContainMem, it must have returned false;
            // otherwise, the corresponding operand (op1 or op2) would be contained.
            bool setRegOptional = (regOptionalCandidate == op1) ? isSafeToContainOp1 && IsSafeToContainMem(cmp, op1)
                                                                : isSafeToContainOp2 && IsSafeToContainMem(cmp, op2);
            if (setRegOptional)
            {
                regOptionalCandidate->SetRegOptional();
            }
        }
    }
}

void Lowering::LowerStoreIndRMW(GenTreeStoreInd* store)
{
    assert(store->OperIs(GT_STOREIND) && varTypeIsIntegralOrI(store->GetType()));

    GenTreeIndir* load = IsStoreIndRMW(store);

    if (load == nullptr)
    {
        return;
    }

    // We've went through a lot of trouble to ensure that all the nodes directly involved
    // in the RMW store can be moved forward, we may as well actually move them now.
    // In many cases where these nodes are does not matter, many are side effect free to
    // begin with - LEA, ADD, CNS_INT etc. But we may have multiple LCL_VAR uses of the
    // same local and some of them may be contained as part of the load address. In this
    // case the order matters. Most backend code effectively ignores containment, except
    // liveness which will happily mark a contained LCL_VAR as last-use even if for all
    // intents and purposes it's not a real use. This can happen if load address LCL_VARs
    // appear after store address LCL_VARs, which is rather unlikely but not impossible.
    // So we'll just move everthing that can be moved before store and ensure that any
    // load address LCL_VARs always come first:
    //     load address, load, src (if needed), op, store address, store

    // TODO-MIKE-Cleanup: This is still kind of dodgy, though it's preferable to fixing
    // last-use in LSRA. The real problem is of course the fact that we need to keep
    // load address related nodes in the IR and mark them as contained. But that's not
    // the same as normal containment, where any sort of effects a contained node may
    // have (like keeping a local alive) still occur, just at a different place.
    // One simple alternative might be to replace the load address with a constant,
    // but that seems slightly risky as there are places in lowering that sometimes
    // undo containment. Though that's very unlikely to ever happen in the RMW case.
    // Ideally we'd just use INSTR and remove a lot of this circus but that doesn't
    // currently work on XARCH.

    GenTree* insertBefore = store;
    GenTree* storeAddr    = store->GetAddr();

    insertBefore = BlockRange().MoveBefore(insertBefore, storeAddr);

    if (GenTreeAddrMode* addrMode = storeAddr->IsAddrMode())
    {
        assert(addrMode->isContained());

        if (GenTree* base = addrMode->GetBase())
        {
            if (base->OperIs(GT_LCL_VAR))
            {
                insertBefore = BlockRange().MoveBefore(insertBefore, base);
            }
        }

        if (GenTree* index = addrMode->GetIndex())
        {
            if (index->OperIs(GT_LCL_VAR))
            {
                insertBefore = BlockRange().MoveBefore(insertBefore, index);
            }
        }
    }

    GenTree* op = store->GetValue();
    op->SetContained();
    insertBefore = BlockRange().MoveBefore(insertBefore, op);

    if (op->OperIsBinary())
    {
        GenTree* src = op->AsOp()->GetOp(1);

        if (load == src)
        {
            assert(op->OperIsCommutative());

            src = op->AsOp()->GetOp(0);
            op->AsOp()->SetOp(0, load);
            op->AsOp()->SetOp(1, src);
        }

        if (!src->IsIntCon())
        {
            src->ClearContained();
        }

        assert(!src->IsRegOptional());

        if (src->OperIs(GT_LCL_VAR, GT_CNS_INT))
        {
            insertBefore = BlockRange().MoveBefore(insertBefore, src);
        }
    }

    load->ClearRegOptional();
    load->SetContained();
    insertBefore = BlockRange().MoveBefore(insertBefore, load);

    // Part of the load address may have already been contained during load lowering.
    // But we need to contain everything because the entire load and its address are
    // now subsumed by the store.
    GenTree* loadAddr = load->GetAddr();
    loadAddr->SetContained();
    insertBefore = BlockRange().MoveBefore(insertBefore, loadAddr);

    if (GenTreeAddrMode* addrMode = loadAddr->IsAddrMode())
    {
        if (GenTree* base = addrMode->GetBase())
        {
            assert(base->OperIsLeaf());
            base->SetContained();
            insertBefore = BlockRange().MoveBefore(insertBefore, base);
        }

        if (GenTree* index = addrMode->GetIndex())
        {
            assert(index->OperIsLeaf());
            index->SetContained();
            insertBefore = BlockRange().MoveBefore(insertBefore, index);
        }
    }
}

void Lowering::ContainCheckBinary(GenTreeOp* node)
{
    assert(node->OperIsBinary());

    if (varTypeIsFloating(node))
    {
        assert(node->OperIs(GT_ADD, GT_SUB));
        ContainCheckFloatBinary(node);
        return;
    }

    GenTree* op1 = node->GetOp(0);
    GenTree* op2 = node->GetOp(1);

    if (IsContainableImmed(node, op2))
    {
        op2->SetContained();
        return;
    }

    bool isSafeToContainOp1 = true;
    bool isSafeToContainOp2 = true;

    const unsigned operatorSize = varTypeSize(node->GetType());

    if ((varTypeSize(op2->GetType()) == operatorSize) && IsContainableMemoryOp(op2))
    {
        isSafeToContainOp2 = IsSafeToContainMem(node, op2);

        if (isSafeToContainOp2)
        {
            op2->SetContained();
            return;
        }
    }

    if (node->OperIsCommutative())
    {
        if ((varTypeSize(op1->GetType()) == operatorSize) && IsContainableMemoryOp(op1))
        {
            isSafeToContainOp1 = IsSafeToContainMem(node, op1);

            if (isSafeToContainOp1)
            {
                op1->SetContained();
                return;
            }
        }
    }

    // TODO-MIKE-Review: The use of IsSafeToContainMem for reg-optional is dubious.
    // It's meaningless for non-LCL_VAR nodes, making these reg-optional doesn't
    // change where their reg def is placed, it only prevents unspilling, which is
    // rare to begin with in these cases.
    // Even for LCL_VAR nodes this seems pointless. These are supposed to be reg
    // candidates so their reg use was anyway placed where the user node is, not
    // where the LCL_VAR node is.

    // IsSafeToContainMem is expensive so we call it at most once for each operand
    // in this method. If we already called IsSafeToContainMem, it must have returned
    // false; otherwise we would have already contained an operand.
    isSafeToContainOp1 = isSafeToContainOp1 && IsSafeToContainMem(node, op1);
    isSafeToContainOp2 = isSafeToContainOp2 && IsSafeToContainMem(node, op2);

    SetRegOptionalForBinOp(node, isSafeToContainOp1, isSafeToContainOp2);
}

// ------------------------------------------------------------------
// SetRegOptionalBinOp - Indicates which of the operands of a bin-op
// register requirement is optional. Xarch instruction set allows
// either of op1 or op2 of binary operation (e.g. add, mul etc) to be
// a memory operand.  This routine provides info to register allocator
// which of its operands optionally require a register.  Lsra might not
// allocate a register to RefTypeUse positions of such operands if it
// is beneficial. In such a case codegen will treat them as memory
// operands.
//
// Arguments:
//     tree  -             Gentree of a binary operation.
//     isSafeToMarkOp1     True if it's safe to mark op1 as register optional
//     isSafeToMarkOp2     True if it's safe to mark op2 as register optional
//
// Returns
//     The caller is expected to get isSafeToMarkOp1 and isSafeToMarkOp2
//     by calling IsSafeToContainMem.
//
// Note: At most one of the operands will be marked as reg optional,
// even when both operands could be considered register optional.
//
void Lowering::SetRegOptionalForBinOp(GenTree* tree, bool isSafeToMarkOp1, bool isSafeToMarkOp2)
{
    assert(GenTree::OperIsBinary(tree->OperGet()));

    GenTree* const op1 = tree->gtGetOp1();
    GenTree* const op2 = tree->gtGetOp2();

    const unsigned operatorSize = genTypeSize(tree->TypeGet());

    const bool op1Legal = isSafeToMarkOp1 && tree->OperIsCommutative() && (operatorSize == genTypeSize(op1->TypeGet()));
    const bool op2Legal = isSafeToMarkOp2 && (operatorSize == genTypeSize(op2->TypeGet()));

    GenTree* regOptionalOperand = nullptr;
    if (op1Legal)
    {
        regOptionalOperand = op2Legal ? PreferredRegOptionalOperand(tree) : op1;
    }
    else if (op2Legal)
    {
        regOptionalOperand = op2;
    }
    if (regOptionalOperand != nullptr)
    {
        regOptionalOperand->SetRegOptional();
    }
}

void Lowering::ContainCheckBoundsChk(GenTreeBoundsChk* node)
{
    GenTree* index  = node->GetIndex();
    GenTree* length = node->GetLength();
    GenTree* other;

    if (CheckImmedAndMakeContained(node, index))
    {
        other = length;
    }
    else if (CheckImmedAndMakeContained(node, length))
    {
        other = index;
    }
    else if (IsContainableMemoryOp(index))
    {
        other = index;
    }
    else
    {
        other = length;
    }

    if (index->GetType() == length->GetType())
    {
        if (IsContainableMemoryOp(other))
        {
            other->SetContained();
        }
        else
        {
            other->SetRegOptional();
        }
    }
}

//------------------------------------------------------------------------
// ContainCheckIntrinsic: determine whether the source of an INTRINSIC node should be contained.
//
// Arguments:
//    node - pointer to the node
//
void Lowering::ContainCheckIntrinsic(GenTreeOp* node)
{
    assert(node->OperIs(GT_INTRINSIC));

    NamedIntrinsic intrinsicName = node->AsIntrinsic()->gtIntrinsicName;

    if ((intrinsicName == NI_System_Math_Ceiling) || (intrinsicName == NI_System_Math_Floor) ||
        (intrinsicName == NI_System_Math_Round) || (intrinsicName == NI_System_Math_Sqrt))
    {
        GenTree* op1 = node->gtGetOp1();

        if (IsContainableMemoryOp(op1) || op1->IsCnsNonZeroFltOrDbl())
        {
            MakeSrcContained(node, op1);
        }
        else
        {
            // Mark the operand as reg optional since codegen can still
            // generate code if op1 is on stack.
            op1->SetRegOptional();
        }
    }
}

#ifdef FEATURE_HW_INTRINSICS
//----------------------------------------------------------------------------------------------
// IsContainableHWIntrinsicOp: Return true if 'node' is a containable HWIntrinsic op.
//
//  Arguments:
//     containingNode - The hardware intrinsic node which contains 'node'
//     node - The node to check
//     [Out] supportsRegOptional - On return, this will be true if 'containingNode' supports regOptional operands;
//     otherwise, false.
//
// Return Value:
//    true if 'node' is a containable hardware intrinsic node; otherwise, false.
//
bool Lowering::IsContainableHWIntrinsicOp(GenTreeHWIntrinsic* containingNode, GenTree* node, bool* supportsRegOptional)
{
    NamedIntrinsic      containingIntrinsicId = containingNode->GetIntrinsic();
    HWIntrinsicCategory category              = HWIntrinsicInfo::lookupCategory(containingIntrinsicId);

    // We shouldn't have called in here if containingNode doesn't support containment
    assert(HWIntrinsicInfo::SupportsContainment(containingIntrinsicId));

    // containingNode supports nodes that read from an aligned memory address
    //
    // This will generally be an explicit LoadAligned instruction and is false for
    // machines with VEX support when minOpts is enabled. This is because there is
    // currently no way to guarantee that the address read from will always be
    // aligned and we want to assert that the address is aligned when optimizations
    // aren't enabled. However, when optimizations are enabled, we want to allow
    // folding of memory operands as it produces better codegen and allows simpler
    // coding patterns on the managed side.
    bool supportsAlignedSIMDLoads = false;

    // containingNode supports nodes that read from general memory
    //
    // We currently have to assume all "general" loads are unaligned. As such, this is
    // generally used to determine if we can mark the node as `regOptional` in the case
    // where `node` is not containable. However, this can also be used to determine whether
    // we can mark other types of reads as contained (such as when directly reading a local).
    bool supportsGeneralLoads = false;

    // containingNode supports nodes that read from a scalar memory address
    //
    // This will generally be an explicit LoadScalar instruction but is also used to determine
    // whether we can read an address of type T (we don't support this when the load would
    // read more than sizeof(T) bytes).
    bool supportsSIMDScalarLoads = false;

    // containingNode supports nodes that read from an unaligned memory address
    //
    // This will generally be an explicit Load instruction and is generally false for machines
    // without VEX support. This is because older hardware required that the SIMD operand always
    // be aligned to the 'natural alignment' of the type.
    bool supportsUnalignedSIMDLoads = false;

    switch (category)
    {
        case HW_Category_MemoryLoad:
            supportsGeneralLoads = (!node->OperIsHWIntrinsic());
            break;

        case HW_Category_SimpleSIMD:
        {
            switch (containingIntrinsicId)
            {
                case NI_SSE41_ConvertToVector128Int16:
                case NI_SSE41_ConvertToVector128Int32:
                case NI_SSE41_ConvertToVector128Int64:
                case NI_AVX2_ConvertToVector256Int16:
                case NI_AVX2_ConvertToVector256Int32:
                case NI_AVX2_ConvertToVector256Int64:
                {
                    supportsGeneralLoads = (!node->OperIsHWIntrinsic());
                    break;
                }

                default:
                {
                    if (!node->TypeIs(TYP_SIMD16, TYP_SIMD32))
                    {
                        *supportsRegOptional = false;
                        return false;
                    }

                    if (!comp->canUseVexEncoding())
                    {
                        // Most instructions under the non-VEX encoding require aligned operands.
                        // Those used for Sse2.ConvertToVector128Double (CVTDQ2PD and CVTPS2PD)
                        // are exceptions and don't fail for unaligned inputs.

                        supportsAlignedSIMDLoads   = (containingIntrinsicId != NI_SSE2_ConvertToVector128Double);
                        supportsUnalignedSIMDLoads = !supportsAlignedSIMDLoads;
                    }
                    else
                    {
                        supportsAlignedSIMDLoads   = !comp->opts.MinOpts();
                        supportsUnalignedSIMDLoads = true;
                    }

                    supportsGeneralLoads = supportsUnalignedSIMDLoads;
                    break;
                }
            }

            assert(supportsSIMDScalarLoads == false);
            break;
        }

        case HW_Category_IMM:
        {
            switch (containingIntrinsicId)
            {
                case NI_SSE_Shuffle:
                case NI_SSE2_ShiftLeftLogical:
                case NI_SSE2_ShiftRightArithmetic:
                case NI_SSE2_ShiftRightLogical:
                case NI_SSE2_Shuffle:
                case NI_SSE2_ShuffleHigh:
                case NI_SSE2_ShuffleLow:
                case NI_SSSE3_AlignRight:
                case NI_SSE41_Blend:
                case NI_SSE41_DotProduct:
                case NI_SSE41_MultipleSumAbsoluteDifferences:
                case NI_AES_KeygenAssist:
                case NI_PCLMULQDQ_CarrylessMultiply:
                case NI_AVX_Blend:
                case NI_AVX_Compare:
                case NI_AVX_DotProduct:
                case NI_AVX_InsertVector128:
                case NI_AVX_Permute:
                case NI_AVX_Permute2x128:
                case NI_AVX2_Blend:
                case NI_AVX2_InsertVector128:
                case NI_AVX2_MultipleSumAbsoluteDifferences:
                case NI_AVX2_Permute2x128:
                case NI_AVX2_Permute4x64:
                case NI_AVX2_ShiftLeftLogical:
                case NI_AVX2_ShiftRightArithmetic:
                case NI_AVX2_ShiftRightLogical:
                case NI_AVX2_ShuffleHigh:
                case NI_AVX2_ShuffleLow:
                {
                    if (!node->TypeIs(TYP_SIMD16, TYP_SIMD32))
                    {
                        *supportsRegOptional = false;
                        return false;
                    }

                    assert(supportsSIMDScalarLoads == false);

                    supportsAlignedSIMDLoads   = !comp->canUseVexEncoding() || !comp->opts.MinOpts();
                    supportsUnalignedSIMDLoads = comp->canUseVexEncoding();
                    supportsGeneralLoads       = supportsUnalignedSIMDLoads;

                    break;
                }

                case NI_SSE2_Insert:
                case NI_SSE41_Insert:
                case NI_SSE41_X64_Insert:
                {
                    assert(containingNode->GetOp(1) == node);
                    // insertps has its own special handling
                    assert(containingNode->GetSimdBaseType() != TYP_FLOAT);
                    // We should only get here for integral nodes.
                    assert(varTypeIsIntegral(node->TypeGet()));

                    assert(supportsAlignedSIMDLoads == false);
                    assert(supportsUnalignedSIMDLoads == false);
                    assert(supportsSIMDScalarLoads == false);

                    unsigned expectedSize = varTypeSize(containingNode->GetSimdBaseType());
                    unsigned operandSize  = varTypeSize(node->GetType());

                    supportsGeneralLoads = (operandSize >= expectedSize);
                    break;
                }

                case NI_AVX_CompareScalar:
                {
                    if ((genTypeSize(node->TypeGet()) != 16) && (genTypeSize(node->TypeGet()) != 32))
                    {
                        // These intrinsics only expect 16 or 32-byte nodes for containment
                        break;
                    }

                    assert(supportsAlignedSIMDLoads == false);
                    assert(supportsUnalignedSIMDLoads == false);

                    supportsSIMDScalarLoads = true;
                    supportsGeneralLoads    = supportsSIMDScalarLoads;
                    break;
                }

                default:
                {
                    assert(supportsAlignedSIMDLoads == false);
                    assert(supportsGeneralLoads == false);
                    assert(supportsSIMDScalarLoads == false);
                    assert(supportsUnalignedSIMDLoads == false);
                    break;
                }
            }
            break;
        }

        case HW_Category_SIMDScalar:
        {
            assert(supportsAlignedSIMDLoads == false);
            assert(supportsUnalignedSIMDLoads == false);

            switch (containingIntrinsicId)
            {
                case NI_Vector128_CreateScalarUnsafe:
                case NI_Vector256_CreateScalarUnsafe:
                {
                    assert(supportsSIMDScalarLoads == false);

                    unsigned expectedSize = varTypeSize(varActualType(containingNode->GetSimdBaseType()));
                    unsigned operandSize  = varTypeSize(node->GetType());

                    supportsGeneralLoads = (operandSize == expectedSize);
                    break;
                }

                case NI_AVX2_BroadcastScalarToVector128:
                case NI_AVX2_BroadcastScalarToVector256:
                {
                    // The memory form of this already takes a pointer, and cannot be further contained.
                    // The containable form is the one that takes a SIMD value, that may be in memory.
                    supportsGeneralLoads = (node->TypeGet() == TYP_SIMD16);
                    break;
                }

                case NI_SSE_ConvertScalarToVector128Single:
                case NI_SSE2_ConvertScalarToVector128Double:
                case NI_SSE2_ConvertScalarToVector128Int32:
                case NI_SSE2_ConvertScalarToVector128UInt32:
                case NI_SSE_X64_ConvertScalarToVector128Single:
                case NI_SSE2_X64_ConvertScalarToVector128Double:
                case NI_SSE2_X64_ConvertScalarToVector128Int64:
                case NI_SSE2_X64_ConvertScalarToVector128UInt64:
                {
                    if (!varTypeIsIntegral(node->TypeGet()))
                    {
                        // The floating-point overload doesn't require any special semantics
                        assert(containingIntrinsicId == NI_SSE2_ConvertScalarToVector128Double);
                        supportsSIMDScalarLoads = true;
                        supportsGeneralLoads    = supportsSIMDScalarLoads;
                        break;
                    }

                    assert(supportsSIMDScalarLoads == false);

                    unsigned expectedSize = varTypeSize(varActualType(containingNode->GetSimdBaseType()));
                    unsigned operandSize  = varTypeSize(node->GetType());

                    supportsGeneralLoads = (operandSize == expectedSize);
                    break;
                }

                default:
                {
                    if ((genTypeSize(node->TypeGet()) != 16) && (genTypeSize(node->TypeGet()) != 32))
                    {
                        // These intrinsics only expect 16 or 32-byte nodes for containment
                        break;
                    }

                    supportsSIMDScalarLoads = true;
                    supportsGeneralLoads    = supportsSIMDScalarLoads;
                    break;
                }
            }
            break;
        }

        case HW_Category_Scalar:
        {
            // We should only get here for integral nodes.
            assert(varTypeIsIntegral(node->TypeGet()));

            assert(supportsAlignedSIMDLoads == false);
            assert(supportsUnalignedSIMDLoads == false);
            assert(supportsSIMDScalarLoads == false);

            unsigned       expectedSize = genTypeSize(containingNode->TypeGet());
            const unsigned operandSize  = genTypeSize(node->TypeGet());

            // CRC32 codegen depends on its second oprand's type.
            // Currently, we are using SIMDBaseType to store the op2Type info.
            if (containingIntrinsicId == NI_SSE42_Crc32)
            {
                expectedSize = varTypeSize(containingNode->GetSimdBaseType());
            }

            supportsGeneralLoads = (operandSize >= expectedSize);
            break;
        }

        default:
        {
            assert(supportsAlignedSIMDLoads == false);
            assert(supportsGeneralLoads == false);
            assert(supportsSIMDScalarLoads == false);
            assert(supportsUnalignedSIMDLoads == false);
            break;
        }
    }

    noway_assert(supportsRegOptional != nullptr);
    *supportsRegOptional = supportsGeneralLoads;

    if (!node->OperIsHWIntrinsic())
    {
        return supportsGeneralLoads && IsContainableMemoryOp(node);
    }

    // TODO-XArch: Update this to be table driven, if possible.

    switch (node->AsHWIntrinsic()->GetIntrinsic())
    {
        case NI_SSE_LoadAlignedVector128:
        case NI_SSE2_LoadAlignedVector128:
        case NI_AVX_LoadAlignedVector256:
            return supportsAlignedSIMDLoads;

        case NI_SSE_LoadScalarVector128:
        case NI_SSE2_LoadScalarVector128:
            return supportsSIMDScalarLoads;

        case NI_SSE_LoadVector128:
        case NI_SSE2_LoadVector128:
        case NI_AVX_LoadVector256:
            return supportsUnalignedSIMDLoads;

        default:
            return false;
    }
}

//----------------------------------------------------------------------------------------------
// ContainCheckHWIntrinsicAddr: Perform containment analysis for an address operand of a hardware
//                              intrinsic node.
//
//  Arguments:
//     node - The hardware intrinsic node
//     addr - The address node to try contain
//
void Lowering::ContainCheckHWIntrinsicAddr(GenTreeHWIntrinsic* node, GenTree* addr)
{
    assert((addr->TypeGet() == TYP_I_IMPL) || (addr->TypeGet() == TYP_BYREF));
    TryCreateAddrMode(addr, true);
    if ((addr->OperIs(GT_CLS_VAR_ADDR, GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR, GT_LEA) ||
         (addr->IsIntCon() && addr->AsIntCon()->FitsInAddrBase(comp))) &&
        IsSafeToContainMem(node, addr))
    {
        MakeSrcContained(node, addr);
    }
}

//----------------------------------------------------------------------------------------------
// ContainCheckHWIntrinsic: Perform containment analysis for a hardware intrinsic node.
//
//  Arguments:
//     node - The hardware intrinsic node.
//
void Lowering::ContainCheckHWIntrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic      intrinsicId = node->GetIntrinsic();
    HWIntrinsicCategory category    = HWIntrinsicInfo::lookupCategory(intrinsicId);
    int                 numArgs     = node->GetNumOps();
    var_types           baseType    = node->GetSimdBaseType();
    unsigned            simdSize    = node->GetSimdSize();

    if (!HWIntrinsicInfo::SupportsContainment(intrinsicId))
    {
        // AVX2 gather are not containable and always have constant IMM argument
        if (HWIntrinsicInfo::isAVX2GatherIntrinsic(intrinsicId))
        {
            MakeSrcContained(node, node->GetLastOp());
        }
        // Exit early if containment isn't supported
        return;
    }

    if (HWIntrinsicInfo::lookupCategory(intrinsicId) == HW_Category_IMM)
    {
        if ((intrinsicId == NI_SSE41_Insert) && (baseType == TYP_FLOAT))
        {
            ContainHWIntrinsicInsertFloat(node);
            return;
        }

        GenTree* lastOp = node->GetLastOp();
        assert(lastOp != nullptr);

        if (HWIntrinsicInfo::isImmOp(intrinsicId, lastOp) && lastOp->IsCnsIntOrI())
        {
            MakeSrcContained(node, lastOp);
        }
    }

    if ((node->GetSimdSize() == 8) || (node->GetSimdSize() == 12))
    {
        // We want to handle GetElement still for Vector2/3
        if ((intrinsicId != NI_Vector128_GetElement) && (intrinsicId != NI_Vector256_GetElement))
        {
            // TODO-XArch-CQ: Ideally we would key this off of the size containingNode
            // expects vs the size node actually is or would be if spilled to the stack
            return;
        }
    }

    // TODO-XArch-CQ: Non-VEX encoded instructions can have both ops contained

    const bool isCommutative = HWIntrinsicInfo::IsCommutative(intrinsicId);

    if (numArgs == 1)
    {
        // One argument intrinsics cannot be commutative
        assert(!isCommutative);

        switch (category)
        {
            case HW_Category_MemoryLoad:
                ContainCheckHWIntrinsicAddr(node, node->GetOp(0));
                break;

            case HW_Category_SimpleSIMD:
            case HW_Category_SIMDScalar:
            case HW_Category_Scalar:
            {
                switch (intrinsicId)
                {
                    case NI_SSE_ReciprocalScalar:
                    case NI_SSE_ReciprocalSqrtScalar:
                    case NI_SSE_SqrtScalar:
                    case NI_SSE2_SqrtScalar:
                    case NI_SSE41_CeilingScalar:
                    case NI_SSE41_FloorScalar:
                    case NI_SSE41_RoundCurrentDirectionScalar:
                    case NI_SSE41_RoundToNearestIntegerScalar:
                    case NI_SSE41_RoundToNegativeInfinityScalar:
                    case NI_SSE41_RoundToPositiveInfinityScalar:
                    case NI_SSE41_RoundToZeroScalar:
                    {
                        // These intrinsics have both 1 and 2-operand overloads.
                        //
                        // The 1-operand overload basically does `intrinsic(op1, op1)`
                        //
                        // Because of this, the operand must be loaded into a register
                        // and cannot be contained.
                        return;
                    }

                    case NI_SSE2_ConvertToInt32:
                    case NI_SSE2_X64_ConvertToInt64:
                    case NI_SSE2_ConvertToUInt32:
                    case NI_SSE2_X64_ConvertToUInt64:
                    case NI_AVX2_ConvertToInt32:
                    case NI_AVX2_ConvertToUInt32:
                    {
                        if (varTypeIsIntegral(baseType))
                        {
                            // TODO-XARCH-CQ: These intrinsics are "ins reg/mem, xmm" and don't
                            // currently support containment.
                            return;
                        }

                        break;
                    }

                    case NI_SSE41_ConvertToVector128Int16:
                    case NI_SSE41_ConvertToVector128Int32:
                    case NI_SSE41_ConvertToVector128Int64:
                    case NI_AVX2_ConvertToVector256Int16:
                    case NI_AVX2_ConvertToVector256Int32:
                    case NI_AVX2_ConvertToVector256Int64:
                        if (!varTypeIsSIMD(node->GetOp(0)->TypeGet()))
                        {
                            ContainCheckHWIntrinsicAddr(node, node->GetOp(0));
                            return;
                        }
                        break;

                    default:
                    {
                        break;
                    }
                }

                bool supportsRegOptional = false;

                if (IsContainableHWIntrinsicOp(node, node->GetOp(0), &supportsRegOptional))
                {
                    MakeSrcContained(node, node->GetOp(0));
                }
                else if (supportsRegOptional)
                {
                    node->GetOp(0)->SetRegOptional();
                }
                break;
            }

            default:
            {
                unreached();
                break;
            }
        }
    }
    else
    {
        if (numArgs == 2)
        {
            GenTree* op1 = node->GetOp(0);
            GenTree* op2 = node->GetOp(1);

            switch (category)
            {
                case HW_Category_MemoryLoad:
                    if ((intrinsicId == NI_AVX_MaskLoad) || (intrinsicId == NI_AVX2_MaskLoad))
                    {
                        ContainCheckHWIntrinsicAddr(node, op1);
                    }
                    else
                    {
                        ContainCheckHWIntrinsicAddr(node, op2);
                    }
                    break;

                case HW_Category_MemoryStore:
                    ContainCheckHWIntrinsicAddr(node, node->GetOp(0));

                    if (((intrinsicId == NI_SSE_Store) || (intrinsicId == NI_SSE2_Store)) && op2->OperIsHWIntrinsic() &&
                        ((op2->AsHWIntrinsic()->GetIntrinsic() == NI_AVX_ExtractVector128) ||
                         (op2->AsHWIntrinsic()->GetIntrinsic() == NI_AVX2_ExtractVector128)) &&
                        op2->gtGetOp2()->IsIntegralConst())
                    {
                        MakeSrcContained(node, op2);
                    }
                    break;

                case HW_Category_SimpleSIMD:
                case HW_Category_SIMDScalar:
                case HW_Category_Scalar:
                {
                    bool op2SupportsRegOptional = false;
                    bool op1SupportsRegOptional = false;

                    if (IsContainableHWIntrinsicOp(node, op2, &op2SupportsRegOptional))
                    {
                        MakeSrcContained(node, op2);
                    }
                    else if ((isCommutative || (intrinsicId == NI_BMI2_MultiplyNoFlags) ||
                              (intrinsicId == NI_BMI2_X64_MultiplyNoFlags)) &&
                             IsContainableHWIntrinsicOp(node, op1, &op1SupportsRegOptional))
                    {
                        MakeSrcContained(node, op1);

                        // Swap the operands here to make the containment checks in codegen significantly simpler
                        node->SetOp(0, op2);
                        node->SetOp(1, op1);
                    }
                    else if (op2SupportsRegOptional)
                    {
                        op2->SetRegOptional();

                        // TODO-XArch-CQ: For commutative nodes, either operand can be reg-optional.
                        //                https://github.com/dotnet/runtime/issues/6358
                    }
                    break;
                }

                case HW_Category_IMM:
                {
                    // We don't currently have any IMM intrinsics which are also commutative
                    assert(!isCommutative);
                    bool supportsRegOptional = false;

                    switch (intrinsicId)
                    {
                        case NI_SSE2_Extract:
                        case NI_AVX_ExtractVector128:
                        case NI_AVX2_ExtractVector128:
                        {
                            // TODO-XARCH-CQ: These intrinsics are "ins reg/mem, xmm, imm8" and don't
                            // currently support containment.
                            break;
                        }

                        case NI_SSE2_ShiftLeftLogical:
                        case NI_SSE2_ShiftRightArithmetic:
                        case NI_SSE2_ShiftRightLogical:
                        case NI_AVX2_ShiftLeftLogical:
                        case NI_AVX2_ShiftRightArithmetic:
                        case NI_AVX2_ShiftRightLogical:
                        {
                            // These intrinsics can have op2 be immValue or reg/mem

                            if (!HWIntrinsicInfo::isImmOp(intrinsicId, op2))
                            {
                                if (IsContainableHWIntrinsicOp(node, op2, &supportsRegOptional))
                                {
                                    MakeSrcContained(node, op2);
                                }
                                else if (supportsRegOptional)
                                {
                                    op2->SetRegOptional();
                                }
                            }
                            break;
                        }

                        case NI_SSE2_Shuffle:
                        case NI_SSE2_ShuffleHigh:
                        case NI_SSE2_ShuffleLow:
                        case NI_AVX2_Permute4x64:
                        case NI_AVX2_Shuffle:
                        case NI_AVX2_ShuffleHigh:
                        case NI_AVX2_ShuffleLow:
                        {
                            // These intrinsics have op2 as an immValue and op1 as a reg/mem

                            if (IsContainableHWIntrinsicOp(node, op1, &supportsRegOptional))
                            {
                                MakeSrcContained(node, op1);
                            }
                            else if (supportsRegOptional)
                            {
                                op1->SetRegOptional();
                            }
                            break;
                        }

                        case NI_SSE41_Extract:
                        case NI_SSE41_X64_Extract:
                        {
                            assert(!varTypeIsFloating(baseType));
                            // TODO-XARCH-CQ: These intrinsics are "ins reg/mem, xmm, imm8" and don't
                            // currently support containment.
                            break;
                        }

                        case NI_AVX_Permute:
                        {
                            // These intrinsics can have op2 be immValue or reg/mem
                            // They also can have op1 be reg/mem and op2 be immValue

                            if (HWIntrinsicInfo::isImmOp(intrinsicId, op2))
                            {
                                if (IsContainableHWIntrinsicOp(node, op1, &supportsRegOptional))
                                {
                                    MakeSrcContained(node, op1);
                                }
                                else if (supportsRegOptional)
                                {
                                    op1->SetRegOptional();
                                }
                            }
                            else if (IsContainableHWIntrinsicOp(node, op2, &supportsRegOptional))
                            {
                                MakeSrcContained(node, op2);
                            }
                            else if (supportsRegOptional)
                            {
                                op2->SetRegOptional();
                            }
                            break;
                        }

                        case NI_AES_KeygenAssist:
                        {
                            if (IsContainableHWIntrinsicOp(node, op1, &supportsRegOptional))
                            {
                                MakeSrcContained(node, op1);
                            }
                            else if (supportsRegOptional)
                            {
                                op1->SetRegOptional();
                            }
                            break;
                        }

                        case NI_SSE2_ShiftLeftLogical128BitLane:
                        case NI_SSE2_ShiftRightLogical128BitLane:
                        case NI_AVX2_ShiftLeftLogical128BitLane:
                        case NI_AVX2_ShiftRightLogical128BitLane:
                        {
#if DEBUG
                            // These intrinsics should have been marked contained by the general-purpose handling
                            // earlier in the method.

                            GenTree* lastOp = node->GetLastOp();
                            assert(lastOp != nullptr);

                            if (HWIntrinsicInfo::isImmOp(intrinsicId, lastOp) && lastOp->IsCnsIntOrI())
                            {
                                assert(lastOp->isContained());
                            }
#endif
                            break;
                        }

                        default:
                        {
                            assert(!"Unhandled containment for binary hardware intrinsic with immediate indir1");
                            break;
                        }
                    }

                    break;
                }

                case HW_Category_Helper:
                    // We don't currently have any IMM intrinsics which are also commutative
                    assert(!isCommutative);
                    assert(!"Unhandled containment for helper binary hardware intrinsic");
                    break;

                default:
                    unreached();
                    break;
            }
        }
        else if (numArgs == 3)
        {
            // three argument intrinsics should not be marked commutative
            assert(!isCommutative);

            GenTree* op1 = node->GetOp(0);
            GenTree* op2 = node->GetOp(1);
            GenTree* op3 = node->GetOp(2);

            switch (category)
            {
                case HW_Category_MemoryStore:
                    ContainCheckHWIntrinsicAddr(node, op1);
                    break;

                case HW_Category_SimpleSIMD:
                case HW_Category_SIMDScalar:
                case HW_Category_Scalar:
                {
                    if ((intrinsicId >= NI_FMA_MultiplyAdd) && (intrinsicId <= NI_FMA_MultiplySubtractNegatedScalar))
                    {
                        bool supportsRegOptional = false;

                        if (IsContainableHWIntrinsicOp(node, op3, &supportsRegOptional))
                        {
                            // 213 form: op1 = (op2 * op1) + [op3]
                            MakeSrcContained(node, op3);
                        }
                        else if (IsContainableHWIntrinsicOp(node, op2, &supportsRegOptional))
                        {
                            // 132 form: op1 = (op1 * op3) + [op2]
                            MakeSrcContained(node, op2);
                        }
                        else if (IsContainableHWIntrinsicOp(node, op1, &supportsRegOptional))
                        {
                            // Intrinsics with CopyUpperBits semantics cannot have op1 be contained

                            if (!HWIntrinsicInfo::CopiesUpperBits(intrinsicId))
                            {
                                // 231 form: op3 = (op2 * op3) + [op1]
                                MakeSrcContained(node, op1);
                            }
                        }
                        else
                        {
                            assert(supportsRegOptional);

                            // TODO-XArch-CQ: Technically any one of the three operands can
                            //                be reg-optional. With a limitation on op1 where
                            //                it can only be so if CopyUpperBits is off.
                            //                https://github.com/dotnet/runtime/issues/6358

                            // 213 form: op1 = (op2 * op1) + op3
                            op3->SetRegOptional();
                        }
                    }
                    else
                    {
                        bool supportsRegOptional = false;

                        switch (intrinsicId)
                        {
                            case NI_SSE41_BlendVariable:
                            case NI_AVX_BlendVariable:
                            case NI_AVX2_BlendVariable:
                            {
                                if (IsContainableHWIntrinsicOp(node, op2, &supportsRegOptional))
                                {
                                    MakeSrcContained(node, op2);
                                }
                                else if (supportsRegOptional)
                                {
                                    op2->SetRegOptional();
                                }
                                break;
                            }
                            case NI_AVXVNNI_MultiplyWideningAndAdd:
                            case NI_AVXVNNI_MultiplyWideningAndAddSaturate:
                            {
                                if (IsContainableHWIntrinsicOp(node, op3, &supportsRegOptional))
                                {
                                    MakeSrcContained(node, op3);
                                }
                                else if (supportsRegOptional)
                                {
                                    op3->SetRegOptional();
                                }
                                break;
                            }
                            case NI_BMI2_MultiplyNoFlags:
                            case NI_BMI2_X64_MultiplyNoFlags:
                            {
                                if (IsContainableHWIntrinsicOp(node, op2, &supportsRegOptional))
                                {
                                    MakeSrcContained(node, op2);
                                }
                                else if (IsContainableHWIntrinsicOp(node, op1, &supportsRegOptional))
                                {
                                    MakeSrcContained(node, op1);
                                    // MultiplyNoFlags is a Commutative operation, so swap the first two operands here
                                    // to make the containment checks in codegen significantly simpler
                                    node->SetOp(0, op2);
                                    node->SetOp(1, op1);
                                }
                                else if (supportsRegOptional)
                                {
                                    op2->SetRegOptional();
                                }
                                break;
                            }

                            default:
                            {
                                unreached();
                                break;
                            }
                        }
                    }
                    break;
                }

                case HW_Category_IMM:
                {
                    bool supportsRegOptional = false;

                    switch (intrinsicId)
                    {
                        case NI_SSE41_Insert:
                            assert(baseType != TYP_FLOAT);
                            FALLTHROUGH;
                        case NI_SSE_Shuffle:
                        case NI_SSE2_Insert:
                        case NI_SSE2_Shuffle:
                        case NI_SSSE3_AlignRight:
                        case NI_SSE41_Blend:
                        case NI_SSE41_DotProduct:
                        case NI_SSE41_X64_Insert:
                        case NI_SSE41_MultipleSumAbsoluteDifferences:
                        case NI_AVX_Blend:
                        case NI_AVX_Compare:
                        case NI_AVX_CompareScalar:
                        case NI_AVX_DotProduct:
                        case NI_AVX_InsertVector128:
                        case NI_AVX_Permute2x128:
                        case NI_AVX_Shuffle:
                        case NI_AVX2_AlignRight:
                        case NI_AVX2_Blend:
                        case NI_AVX2_InsertVector128:
                        case NI_AVX2_MultipleSumAbsoluteDifferences:
                        case NI_AVX2_Permute2x128:
                        case NI_PCLMULQDQ_CarrylessMultiply:
                        {
                            if (IsContainableHWIntrinsicOp(node, op2, &supportsRegOptional))
                            {
                                MakeSrcContained(node, op2);
                            }
                            else if (supportsRegOptional)
                            {
                                op2->SetRegOptional();
                            }
                            break;
                        }

                        default:
                        {
                            assert(!"Unhandled containment for ternary hardware intrinsic with immediate indir1");
                            break;
                        }
                    }

                    break;
                }

                default:
                {
                    unreached();
                    break;
                }
            }
        }
        else
        {
            unreached();
        }
    }
}
#endif // FEATURE_HW_INTRINSICS

//------------------------------------------------------------------------
// ContainCheckFloatBinary: determine whether the sources of a floating point binary node should be contained.
//
// Arguments:
//    node - pointer to the node
//
void Lowering::ContainCheckFloatBinary(GenTreeOp* node)
{
    assert(node->OperIs(GT_ADD, GT_SUB, GT_MUL, GT_DIV) && varTypeIsFloating(node));

    // overflow operations aren't supported on float/double types.
    assert(!node->gtOverflowEx());

    GenTree* op1 = node->gtGetOp1();
    GenTree* op2 = node->gtGetOp2();

    // No implicit conversions at this stage as the expectation is that
    // everything is made explicit by adding casts.
    assert(op1->TypeGet() == op2->TypeGet());

    bool isSafeToContainOp1 = true;
    bool isSafeToContainOp2 = true;

    if (op2->IsCnsNonZeroFltOrDbl())
    {
        MakeSrcContained(node, op2);
    }
    else if (IsContainableMemoryOp(op2))
    {
        isSafeToContainOp2 = IsSafeToContainMem(node, op2);
        if (isSafeToContainOp2)
        {
            MakeSrcContained(node, op2);
        }
    }

    if (!op2->isContained() && node->OperIsCommutative())
    {
        // Though we have GT_ADD(op1=memOp, op2=non-memOp, we try to reorder the operands
        // as long as it is safe so that the following efficient code sequence is generated:
        //      addss/sd targetReg, memOp    (if op1Reg == targetReg) OR
        //      movaps targetReg, op2Reg; addss/sd targetReg, [memOp]
        //
        // Instead of
        //      movss op1Reg, [memOp]; addss/sd targetReg, Op2Reg  (if op1Reg == targetReg) OR
        //      movss op1Reg, [memOp]; movaps targetReg, op1Reg, addss/sd targetReg, Op2Reg

        if (op1->IsCnsNonZeroFltOrDbl())
        {
            MakeSrcContained(node, op1);
        }
        else if (IsContainableMemoryOp(op1))
        {
            isSafeToContainOp1 = IsSafeToContainMem(node, op1);
            if (isSafeToContainOp1)
            {
                MakeSrcContained(node, op1);
            }
        }
    }

    if (!op1->isContained() && !op2->isContained())
    {
        // If there are no containable operands, we can make an operand reg optional.
        // IsSafeToContainMem is expensive so we call it at most once for each operand
        // in this method. If we already called IsSafeToContainMem, it must have returned false;
        // otherwise, the corresponding operand (op1 or op2) would be contained.
        isSafeToContainOp1 = isSafeToContainOp1 && IsSafeToContainMem(node, op1);
        isSafeToContainOp2 = isSafeToContainOp2 && IsSafeToContainMem(node, op2);
        SetRegOptionalForBinOp(node, isSafeToContainOp1, isSafeToContainOp2);
    }
}

#endif // TARGET_XARCH
