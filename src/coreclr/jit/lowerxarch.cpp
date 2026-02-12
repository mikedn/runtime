// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_XARCH

#include "sideeffects.h"
#include "lower.h"

GenTree* Lowering::LowerFloatConvert(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_FTRUNC, GT_FXT));

    GenTree* src = node->GetOp(0);

    assert((node->OperIs(GT_FTRUNC) && src->TypeIs(TYP_DOUBLE) && node->TypeIs(TYP_FLOAT)) ||
           (node->OperIs(GT_FXT) && src->TypeIs(TYP_FLOAT) && node->TypeIs(TYP_DOUBLE)));

    if (IsMemOperand(src))
    {
        // These cannot throw we can move the cast right after
        // the source node to avoid the interference check.

        if (node->gtPrev != src)
        {
            BlockRange().Unlink(node);
            BlockRange().InsertAfter(src, node);
        }

        src->SetContained();
    }
    else
    {
        src->SetRegOptional();
    }

    return node->gtNext;
}

void Lowering::LowerRotateLeft(GenTreeOp* node)
{
    assert(node->OperIs(GT_ROL) && node->TypeIs(TYP_INT, TYP_I_IMPL));

    ContainCheckShiftRotate(node);
}

void Lowering::LowerRotateRight(GenTreeOp* node)
{
    assert(node->OperIs(GT_ROR) && node->TypeIs(TYP_INT, TYP_I_IMPL));

    ContainCheckShiftRotate(node);
}

void Lowering::LowerStoreLclVarArch(GenTreeLclStore* store)
{
    GenTree* src = store->GetValue();

    if (GenTreeIntCon* con = src->IsIntCon())
    {
        LclVarDsc* lcl = store->GetLcl();

        // TODO-MIKE-Review: Is there any point in widening byte stores?
        // For short stores we avoid a 66h prefix but for byte store we
        // just end up with a 32 bit imm for no obvious reason.
        // And the imm adjustment is as dubious as it gets, such a store
        // will use a 32 bit imm anyway, unlike other 32 bit instructions
        // that may use a 8 bit sign extended imm. It doesn't even do it
        // correctly on x64, only the lower 32 bits are adjusted...

        if (varTypeIsSmall(store->GetType()) && !lcl->IsPromotedField() && !lcl->lvWasStructField)
        {
            assert(varActualTypeIsInt(lcl->GetType()));

            if (!varTypeIsSmallUnsigned(lcl->GetType()))
            {
                ssize_t value = con->GetValue();

                if (varTypeIsByte(store->GetType()))
                {
                    if ((value & 0x7f) != value)
                    {
                        value |= 0xffffff00;
                    }
                }
                else
                {
                    assert(varTypeIsShort(store->GetType()));

                    if ((value & 0x7fff) != value)
                    {
                        value |= 0xffff0000;
                    }
                }

                con->SetValue(value);
            }

            store->SetType(TYP_INT);
        }
    }

    ContainCheckStoreLcl(store);
}

void Lowering::LowerIndStoreArch(GenTreeIndStore* store)
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

            intCon->ChangeToIntCon(type, bits);
            store->SetType(type);
        }
    }

    ContainCheckIndStore(store);

    if (varTypeIsIntegralOrI(store->GetType()) && value->OperIsRMWMemOp())
    {
        LowerStoreIndRMW(store);
    }
}

void Lowering::ContainStructStoreAddress(GenTree* store, unsigned size, GenTree* addr)
{
#if FEATURE_MULTIREG_RET
    assert(store->IsArgStore() || store->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD) ||
           (store->OperIs(GT_IND_STORE_BLK, GT_IND_STORE_OBJ) &&
            ((store->AsBlk()->GetKind() == StructStoreKind::UnrollInit) ||
             (store->AsBlk()->GetKind() == StructStoreKind::UnrollCopy) ||
             (store->AsBlk()->GetKind() == StructStoreKind::UnrollRegs))));
#else
    assert(store->IsArgStore() || store->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD) ||
           (store->OperIs(GT_IND_STORE_BLK, GT_IND_STORE_OBJ) &&
            ((store->AsBlk()->GetKind() == StructStoreKind::UnrollInit) ||
             (store->AsBlk()->GetKind() == StructStoreKind::UnrollCopy))));
#endif

    assert(size < INT32_MAX);

    if (addr->OperIs(GT_LCL_ADDR))
    {
        addr->SetContained();
        return;
    }

    if (!addr->IsAddrMode() && (!addr->OperIs(GT_ADD) || !TryCreateAddrMode(addr, true)))
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
    if (GenTreeArgStore* argStore = store->IsArgStore())
    {
#ifdef TARGET_X86
        if (argStore->GetKind() == GenTreeArgStore::Kind::Push)
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
        if ((argStore->GetKind() == GenTreeArgStore::Kind::GCUnroll) ||
            (argStore->GetKind() == GenTreeArgStore::Kind::GCUnrollXMM))
        {
            // Like in the x86 PUSH case, do not contain in cases where unrolling isn't limited. Use a higher
            // size threshold as on x64 we copy 8 and even 16 bytes at a time. Not that RepInstr/RepInstr also
            // do unlimited unroll but unlike GCUnroll/GCUnrollXMM they use the address mode only once.

            if ((addrMode->HasIndex() && (size > 64)) || ((addrMode->GetOffset() > 128 - 32) && (size > 32)))
            {
                return;
            }
        }
#endif
    }
#endif

    if (!IsSafeToMoveAddrModeForward(store, addrMode))
    {
        return;
    }

    addrMode->SetContained();
}

void Lowering::ContainStructStoreAddressUnrollRegsWB(GenTree* addr)
{
    if (!addr->OperIs(GT_ADD))
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

        BlockRange().Unlink(intCon);
    }
    else
    {
        return;
    }

    addr->ChangeToAddrMode(addr->AsOp()->GetOp(0), nullptr, 1, offset);
    addr->SetContained();
}

void Lowering::LowerArgStore(GenTreeArgStore* store)
{
    GenTree* src = store->GetOp(0);

    unsigned     argTypeNum = store->GetArgTypeNum();
    unsigned     argSize;
    ClassLayout* layout = nullptr;

    if (Compiler::typIsLayoutNum(argTypeNum))
    {
        layout  = comp->typGetLayoutByNum(argTypeNum);
        argSize = layout->GetSize();
    }
    else
    {
        argSize = varTypeSize(static_cast<var_types>(argTypeNum));
    }

    if (src->TypeIs(TYP_STRUCT))
    {
        if (src->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
        {
            argSize = roundUp(argSize, REGSIZE_BYTES);
        }

        // For normal stores we could use a helper call but for PUTARG_STK we can't do
        // that since the helper call could kill some already set up outgoing args.

        // TODO-X86-CQ: The helper call either is not supported on x86 or required more work
        // (I don't know which).

        if (!layout->HasGCPtr()
#ifdef TARGET_X86
            && (argSize != 8)
#endif
                )
        {
            store->SetKind(argSize <= CPBLK_UNROLL_LIMIT ? GenTreeArgStore::Kind::Unroll
                                                         : GenTreeArgStore::Kind::RepInstr);
        }
        else
        {
#ifdef TARGET_X86
            // On x86, we must use `push` to store GC references to the stack in order for the emitter to properly
            // update the function's GC info. These nodes will generate a sequence of `push` instructions.
            store->SetKind(GenTreeArgStore::Kind::Push);
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
                store->SetKind(hasXmmSequence ? GenTreeArgStore::Kind::RepInstrXMM : GenTreeArgStore::Kind::RepInstr);
            }
            else
            {
                store->SetKind(hasXmmSequence ? GenTreeArgStore::Kind::GCUnrollXMM : GenTreeArgStore::Kind::GCUnroll);
            }
#endif
        }

        if (src->OperIs(GT_IND_LOAD_OBJ))
        {
            ContainStructStoreAddress(store, argSize, src->AsIndLoadObj()->GetAddr());
        }

        return;
    }

    argSize = roundUp(argSize, REGSIZE_BYTES);

#ifdef WINDOWS_AMD64_ABI
    assert(argSize <= REGSIZE_BYTES);
#else
    if (src->IsIntCon(0) && (argSize > REGSIZE_BYTES))
    {
        if (argSize > INITBLK_UNROLL_LIMIT)
        {
            store->SetKind(GenTreeArgStore::Kind::RepInstrZero);
        }
#ifdef TARGET_X86
        else if (argSize < XMM_REGSIZE_BYTES)
        {
            store->SetKind(GenTreeArgStore::Kind::Push);
            src->SetContained();
        }
#endif
        else
        {
            store->SetKind(GenTreeArgStore::Kind::UnrollZero);
            src->SetContained();
        }

        return;
    }
#endif // !WINDOWS_AMD64_ABI

    // On AMD64, storing a zero register instead of an immediate generates smaller code.

    if (IsImmOperand(src, store) AMD64_ONLY(&&!src->IsIntCon(0)))
    {
        src->SetContained();
    }
#ifdef TARGET_X86
    else if (src->IsDblCon() && src->TypeIs(TYP_FLOAT))
    {
        src->ChangeToIntCon(TYP_INT, src->AsDblCon()->GetFloatBits());
        src->SetContained();
    }
    else
    {
        unsigned srcSize = varTypeSize(src->GetType());

        // For containment we need a slot sized memory operand - INT, FLOAT, REF, BYREF. Yes, it can be FLOAT
        // because it's a memory operation and the type doesn't really matter, only the size does.
        //
        // For reg optional things are a bit more complicated:
        //    - anything other than LCL_LOAD can be reg-optional even if it's a small int type because the
        //      spilled value is really INT (e.g. ushort IND automatically zero extends to INT and the
        //      resulting value is spilled to an INT spill temp).
        //    - LCL_LOAD must be slot sized because we don't know yet if the local will be a reg candidate.
        //      If it's not a reg candidate then it is treated as contained thus the size restriction.
        //      Note that the local itself may have small int type but if we get a LCL_LOAD here then it
        //      means that it is "normalize on store" or that the frontend elided the normalization cast.
        //      Most LCL_LOADs that reference small int local end up having type INT, with the notable
        //      exception of promoted struct field which may have small int type.

        if ((srcSize == REGSIZE_BYTES) && IsMemOperand(src) && IsSafeToMoveMemOperandForward(store, src))
        {
            src->SetContained();
        }
        else if (src->OperIs(GT_LCL_LOAD) ? (srcSize == REGSIZE_BYTES) : (srcSize <= REGSIZE_BYTES))
        {
            src->SetRegOptional();
        }
    }
#endif
}

#ifdef TARGET_X86
// Lower a tail call to a helper call to CORINFO_HELP_TAILCALL.
// Morph has already inserted helper special arguments. This function inserts
// actual data for some placeholders.
// Note that the special arguments are on the stack, whereas normal function
// arguments follow the normal convention.
// Also inserts PInvoke method epilog if required.
void Lowering::LowerTailCallViaJitHelper(GenTreeCall* call)
{
    assert(call->IsTailCallViaJitHelper());
    assert(!call->IsUnmanaged());
    assert(!comp->info.IsSynchronized());
    assert(!comp->compLocallocUsed);

    // CORINFO_HELP_TAILCALL never returns to the caller and is not GC interruptible.
    // Therefore the block containing the tail call should be a GC safe point to avoid
    // GC starvation. It is legal for the block to be unmarked iff the entry block is a
    // GC safe point, as the entry block trivially dominates every reachable block.
    assert(m_block->HasGCSafePoint() || comp->fgFirstBB->HasGCSafePoint());

    CallInfo* callInfo = call->GetInfo();

    // Verify the special args are what we expect, and replace the dummy args with real values.
    // We need to figure out the size of the outgoing stack arguments, not including the special args.
    // The number of 4-byte words is passed to the helper for the incoming and outgoing argument sizes.
    // This number is exactly the next slot number in the call's argument info struct.
    unsigned numNewStackSlots = callInfo->GetStackArgsSize() / REGSIZE_BYTES;
    assert(numNewStackSlots >= 4);
    numNewStackSlots -= 4;

    unsigned       numArgs             = callInfo->GetArgCount();
    GenTreeIntCon* numNewStackSlotsArg = call->GetArgNodeByArgNum(numArgs - 3)->AsIntCon();
    GenTreeIntCon* numOldStackSlotsArg = call->GetArgNodeByArgNum(numArgs - 4)->AsIntCon();

    if (comp->info.IsPInvokeFrameRequired())
    {
        InsertPInvokeMethodEpilog(INDEBUG(call));
    }

    numNewStackSlotsArg->SetValue(numNewStackSlots);
    assert(numOldStackSlotsArg->GetValue() == static_cast<int>(comp->codeGen->paramsStackSize / REGSIZE_BYTES));

    call->SetCallAddr(LowerDirectCall(call));

    if (comp->opts.IsProfilerHookNeeded())
    {
        BlockRange().InsertBefore(call, new (comp, GT_PROF_HOOK) GenTree(GT_PROF_HOOK, TYP_VOID));
    }
}
#endif // TARGET_X86

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
GenTree* Lowering::OptimizeConstCompare(GenTreeOp* cmp)
{
    GenTree*       op1      = cmp->GetOp(0);
    GenTreeIntCon* op2      = cmp->GetOp(1)->AsIntCon();
    var_types      op1Type  = op1->GetType();
    ssize_t        op2Value = op2->GetValue();

    if (IsMemOperand(op1))
    {
        // If op1's type is small then try to narrow op2 so it has the same type as op1.
        // Small types are usually used by memory loads and if both compare operands have
        // the same type then the memory load can be contained. In certain situations
        // (e.g "cmp ubyte, 200") we also get a smaller instruction encoding.
        if (varTypeIsSmall(op1Type) && varTypeSmallIntCanRepresentValue(op1Type, op2Value))
        {
            op2->SetType(op1Type);
        }

        return cmp;
    }

    if (op1->OperIs(GT_AND) && cmp->OperIs(GT_EQ, GT_NE) && (op2Value == 0))
    {
        // ((x AND y) EQ|NE 0) => (x TEST_EQ|TEST_NE y)

        GenTree* andOp1 = op1->AsOp()->GetOp(0);
        GenTree* andOp2 = op1->AsOp()->GetOp(1);

        BlockRange().Unlink(op1);
        BlockRange().Unlink(op2);

        cmp->ChangeOper(cmp->OperIs(GT_EQ) ? GT_TEST_EQ : GT_TEST_NE);
        cmp->SetOp(0, andOp1);
        cmp->SetOp(1, andOp2);
        // We will re-evaluate containment below
        andOp1->ClearContained();
        andOp2->ClearContained();

        if (IsMemOperand(andOp1) && andOp2->IsIntCon())
        {
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

            size_t mask = static_cast<size_t>(andOp2->AsIntCon()->GetValue());

            if (FitsIn<uint8_t>(mask))
            {
                andOp1->SetType(TYP_UBYTE);
                andOp2->SetType(TYP_UBYTE);
            }
            else if (FitsIn<uint16_t>(mask) && varTypeIsShort(andOp1->GetType()))
            {
                andOp1->SetType(TYP_USHORT);
                andOp2->SetType(TYP_USHORT);
            }
        }

        // Transform TEST_EQ|NE(x, LSH(1, y)) into BT(x, y) when possible. Using BT
        // results in smaller and faster code. It also doesn't have special register
        // requirements, unlike LSH that requires the shift count to be in ECX.
        // Note that BT has the same behavior as LSH when the bit index exceeds the
        // operand bit size - it uses (bit_index MOD bit_size).

        GenTree* lsh = andOp2;
        LIR::Use cmpUse;

        if (lsh->OperIs(GT_LSH) && varTypeIsIntOrI(lsh->GetType()) && lsh->AsOp()->GetOp(0)->IsIntCon(1) &&
            BlockRange().TryGetUse(cmp, &cmpUse))
        {
            GenCondition condition = cmp->OperIs(GT_TEST_NE) ? GenCondition::C : GenCondition::NC;

            cmp->SetOper(GT_BT);
            cmp->SetType(TYP_VOID);
            cmp->AddImplicitFlagsDef();
            cmp->AsOp()->SetOp(1, lsh->AsOp()->GetOp(1));
            cmp->GetOp(1)->ClearContained();

            BlockRange().Unlink(lsh->AsOp()->GetOp(0));
            BlockRange().Unlink(lsh);

            GenTreeCC* cc;

            if (cmpUse.User()->OperIs(GT_JTRUE))
            {
                cmpUse.User()->ChangeOper(GT_JCC);
                cc = cmpUse.User()->AsCC();
                cc->SetCondition(condition);
            }
            else
            {
                cc = new (comp, GT_SETCC) GenTreeCC(GT_SETCC, condition, TYP_INT);
                BlockRange().InsertAfter(cmp, cc);
                cmpUse.SetDef(cc);
            }

            cc->AddImplicitFlagsUse();

            return cmp->gtNext;
        }

        return cmp;
    }

    if (op1->OperIs(GT_CONV) && op1->TypeIs(TYP_UBYTE) && FitsIn<uint8_t>(op2Value))
    {
        GenTreeUnOp* cast   = op1->AsUnOp();
        GenTree*     castOp = cast->GetOp(0);

        // Since we're going to remove the cast we need to be able to narrow the cast operand
        // to the cast type. This can be done safely only for certain opers (e.g AND, OR, XOR).
        // Some opers just can't be narrowed (e.g DIV, MUL) while other could be narrowed but
        // doing so would produce incorrect results (e.g. RSZ, RSH).
        //
        // The below list of handled opers is conservative but enough to handle the most common
        // situations. In particular this include CALL, sometimes the JIT unnecessarily widens
        // the result of bool returning calls.

        if (castOp->OperIs(GT_CALL, GT_LCL_LOAD, GT_AND, GT_OR, GT_XOR) || IsMemOperand(castOp))
        {
            // Any contained memory ops on castOp must be narrowed too.
            if (castOp->OperIs(GT_AND, GT_OR, GT_XOR))
            {
                GenTree* op1 = castOp->AsOp()->GetOp(0);
                GenTree* op2 = castOp->AsOp()->GetOp(1);

                if (!op1->IsIntCon() && op1->isContained())
                {
                    assert(IsMemOperand(op1));
                    op1->SetType(TYP_UBYTE);
                }

                if (!op2->IsIntCon() && op2->isContained())
                {
                    assert(IsMemOperand(op2));
                    op2->SetType(TYP_UBYTE);
                }
            }

            op1 = castOp;
            op1->SetType(TYP_UBYTE);
            op2->SetType(TYP_UBYTE);
            cmp->SetOp(0, op1);

            BlockRange().Unlink(cast);
        }
    }

    if (op1->OperIs(GT_AND, GT_OR, GT_XOR, GT_ADD, GT_OVF_SADD, GT_OVF_UADD, GT_SUB, GT_OVF_SSUB, GT_OVF_USUB,
                    GT_NEG) &&
        cmp->OperIs(GT_EQ, GT_NE) && (op2Value == 0))
    {
        // TODO-CQ: We can also do this for shifts, if the shift count is known to
        // be non-zero (const basically), otherwise the condition flags are not set.

        // TODO-CQ: right now the below peep is inexpensive and gets the benefit in most
        // cases because in majority of cases op1, op2 and cmp would be in that order in
        // execution. In general we should be able to check that all the nodes that come
        // after op1 do not modify the flags so that it is safe to avoid generating a
        // test instruction.

        if ((op1->gtNext == op2) && (op2->gtNext == cmp))
        {
            op1->AddImplicitFlagsDef();
            op1->SetUnusedValue();

            BlockRange().Unlink(op2);

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
                BlockRange().Unlink(cmp);
            }
            else if (BlockRange().TryGetUse(cmp, &cmpUse) && cmpUse.User()->OperIs(GT_JTRUE))
            {
                cc   = cmpUse.User();
                ccOp = GT_JCC;
                next = nullptr;
                BlockRange().Unlink(cmp);
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
            cc->AddImplicitFlagsUse();

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

    if (cmp->GetOp(1)->IsIntCon() && comp->opts.OptimizationEnabled())
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
        if (varTypeIsSmallUnsigned(cmp->GetOp(0)->GetType()))
        {
            // If both operands have the same type then codegen will use the common operand type to
            // determine the instruction type. For small types this would result in performing a
            // signed comparison of two small unsigned values without zero extending them to TYP_INT
            // which is incorrect. Note that making the comparison unsigned doesn't imply that codegen
            // has to generate a small comparison, it can still correctly generate a TYP_INT comparison.
            cmp->SetRelopUnsigned(true);
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

bool Lowering::LowerUnsignedDivRem(GenTreeOp* divMod)
{
    assert(divMod->OperIs(GT_UDIV, GT_UREM) && divMod->TypeIs(TYP_INT, TYP_LONG));

    GenTree* dividend = divMod->GetOp(0);
    GenTree* divisor  = divMod->GetOp(1);

#ifndef TARGET_64BIT
    if (dividend->OperIs(GT_LONG))
    {
        return false;
    }
#endif

    if (!divisor->IsIntCon())
    {
        return false;
    }

    if (dividend->IsIntCon())
    {
        // We shouldn't see a UDIV/UREM with constant operands here but if we do then it's likely
        // because optimizations are disabled or it's a case that's supposed to throw an exception.
        // Don't optimize this.
        return false;
    }

    const var_types type = divMod->GetType();

    size_t divisorValue = divisor->AsIntCon()->GetBits();

    if ((divisorValue == 0) || isPow2(divisorValue) || comp->opts.MinOpts())
    {
        return false;
    }

    const bool isDiv = divMod->OperIs(GT_UDIV);
    size_t     magic;
    bool       increment;
    int        preShift;
    int        postShift;
    bool       simpleMul = false;

    if (type == TYP_INT)
    {
        if (isDiv && divisorValue > UINT32_MAX / 2)
        {
            return false;
        }

        magic = MagicDivide::GetUnsigned32Magic(static_cast<uint32_t>(divisorValue), &increment, &preShift, &postShift);

#ifdef TARGET_64BIT
        // avoid inc_saturate/multiple shifts by widening to 32x64 MULHI
        if (increment || (preShift
                          // IMUL reg,reg,imm32 can't be used if magic<0 because of sign-extension
                          && static_cast<int32_t>(magic) < 0))
        {
            magic = MagicDivide::GetUnsigned64Magic(static_cast<uint64_t>(divisorValue), &increment, &preShift,
                                                    &postShift, 32);
        }
        // otherwise just widen to regular multiplication
        else
        {
            postShift += 32;
            simpleMul = true;
        }
#endif
    }
    else
    {
        if (isDiv && divisorValue > UINT64_MAX / 2)
        {
            return false;
        }

#ifdef TARGET_64BIT
        magic = MagicDivide::GetUnsigned64Magic(static_cast<uint64_t>(divisorValue), &increment, &preShift, &postShift);
#else
        unreached();
#endif
    }

    const bool requiresDividendMultiuse = !isDiv;

    if (requiresDividendMultiuse)
    {
        LIR::Use dividendUse(BlockRange(), &divMod->gtOp1, divMod);
        dividend = ReplaceWithLclLoad(dividendUse);
    }

    GenTree* adjustedDividend = dividend;

    if (increment)
    {
        adjustedDividend = comp->gtNewOperNode(GT_INC_SATURATE, type, adjustedDividend);
        BlockRange().InsertBefore(divMod, adjustedDividend);
        assert(!preShift);
    }
    else if (preShift)
    {
        GenTree* preShiftBy = comp->gtNewIconNode(preShift, TYP_INT);
        adjustedDividend    = comp->gtNewOperNode(GT_RSZ, type, adjustedDividend, preShiftBy);
        preShiftBy->SetContained();
        BlockRange().InsertBefore(divMod, preShiftBy, adjustedDividend);
    }
#ifdef TARGET_64BIT
    else if (type != TYP_LONG)
    {
        adjustedDividend = comp->gtNewOperNode(GT_UXT, TYP_LONG, adjustedDividend);
        BlockRange().InsertBefore(divMod, adjustedDividend);
        LowerUnsignedExtend(adjustedDividend->AsUnOp());
    }
#endif

    // Force input transformation to RAX because the following MULHI will
    // kill RDX:RAX anyway and LSRA often causes redundant copies otherwise
    if ((adjustedDividend != dividend) && !simpleMul)
    {
        adjustedDividend->SetRegNum(REG_RAX);
    }

    divisor->AsIntCon()->SetValue(TYP_I_IMPL, magic);
    BlockRange().MoveBefore(divMod, divisor);

    if (isDiv && !postShift && (type == TYP_I_IMPL))
    {
        divMod->SetOper(GT_UMULH);
        divMod->SetOp(0, adjustedDividend);
        ContainCheckMul(divMod);

        return true;
    }

    // Insert a new UMULH node before the existing UDIV/UMOD node.
    // The existing node will later be transformed into a RSZ/SUB that
    // computes the final result. This way don't need to find and change
    // the use of the existing node.

    GenTreeOp* mulhi = comp->gtNewOperNode(simpleMul ? GT_MUL : GT_UMULH, TYP_I_IMPL, adjustedDividend, divisor);
    BlockRange().InsertBefore(divMod, mulhi);
    ContainCheckMul(mulhi);

    if (postShift)
    {
        GenTree* shiftBy = comp->gtNewIconNode(postShift, TYP_INT);
        shiftBy->SetContained();
        BlockRange().InsertBefore(divMod, shiftBy);

        if (isDiv && (type == TYP_I_IMPL))
        {
            divMod->SetOper(GT_RSZ);
            divMod->SetOp(0, mulhi);
            divMod->SetOp(1, shiftBy);
        }
        else
        {
            mulhi = comp->gtNewOperNode(GT_RSZ, TYP_I_IMPL, mulhi, shiftBy);
            BlockRange().InsertBefore(divMod, mulhi);
        }
    }

    if (!isDiv)
    {
        // divisor UMOD dividend = dividend SUB (div MUL divisor)
        GenTree* divisor = comp->gtNewIconNode(divisorValue, type);
        GenTree* mul     = comp->gtNewOperNode(GT_MUL, type, mulhi, divisor);
        dividend         = comp->gtNewLclLoad(dividend->AsLclLoad()->GetLcl(), dividend->GetType());

        divMod->SetOper(GT_SUB);
        divMod->SetOp(0, dividend);
        divMod->SetOp(1, mul);

        BlockRange().InsertBefore(divMod, divisor, mul, dividend);

        if (FitsIn<int32_t>(divisorValue))
        {
            divisor->SetContained();
        }
    }
    else if (type != TYP_I_IMPL)
    {
        divMod->SetOper(GT_BITCAST);
        divMod->gtOp1 = mulhi;
        divMod->gtOp2 = nullptr;
    }

    return true;
}

GenTree* Lowering::LowerConstIntDivRem(GenTreeOp* node)
{
    assert(node->OperIs(GT_SDIV, GT_SREM));

    GenTree* dividend = node->GetOp(0);
    GenTree* divisor  = node->GetOp(1);

    const var_types type = node->GetType();
    assert((type == TYP_INT) || (type == TYP_LONG));

    if (!divisor->IsIntCon())
    {
        return nullptr;
    }

    if (dividend->IsIntCon())
    {
        // We shouldn't see a SDIV/SREM with constant operands here but if we do then it's likely
        // because optimizations are disabled or it's a case that's supposed to throw an exception.
        // Don't optimize this.
        return nullptr;
    }

    ssize_t divisorValue = divisor->AsIntCon()->GetValue();

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

    bool   isDiv           = node->OperIs(GT_SDIV);
    size_t absDivisorValue = UAbs(divisorValue);

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
#ifdef TARGET_64BIT
            magic = MagicDivide::GetSigned64Magic(static_cast<int64_t>(divisorValue), &shift);
#else
            unreached();
#endif
        }

        divisor->AsIntCon()->SetValue(magic);

        // Insert a new SMULH node in front of the existing DIV/REM node.
        // The existing node will later be transformed into a ADD/SUB that
        // computes the final result. This way don't need to find and change
        // the use of the existing node.
        GenTree* mulhi = comp->gtNewOperNode(GT_SMULH, type, divisor, dividend);
        BlockRange().InsertBefore(node, mulhi);

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
        bool requiresDividendMultiuse = requiresAddSubAdjust || !isDiv;

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
            BlockRange().InsertBefore(node, dividend, adjusted);
        }
        else
        {
            adjusted = mulhi;
        }

        GenTree* shiftBy = comp->gtNewIconNode(varTypeBitSize(type) - 1, type);
        GenTree* signBit = comp->gtNewOperNode(GT_RSZ, type, adjusted, shiftBy);
        BlockRange().InsertBefore(node, shiftBy, signBit);

        LIR::Use adjustedUse(BlockRange(), &signBit->AsOp()->gtOp1, signBit);
        adjusted = ReplaceWithLclLoad(adjustedUse);
        adjusted = comp->gtNewLclLoad(adjusted->AsLclLoad()->GetLcl(), adjusted->GetType());
        BlockRange().InsertBefore(node, adjusted);

        if (requiresShiftAdjust)
        {
            shiftBy  = comp->gtNewIconNode(shift, TYP_INT);
            adjusted = comp->gtNewOperNode(GT_RSH, type, adjusted, shiftBy);
            BlockRange().InsertBefore(node, shiftBy, adjusted);
        }

        if (isDiv)
        {
            node->ChangeOper(GT_ADD);
            node->AsOp()->SetOp(0, adjusted);
            node->AsOp()->SetOp(1, signBit);
        }
        else
        {
            GenTree* div = comp->gtNewOperNode(GT_ADD, type, adjusted, signBit);

            dividend = comp->gtNewLclLoad(dividend->AsLclLoad()->GetLcl(), dividend->GetType());

            // divisor % dividend = dividend - divisor x div
            GenTree* divisor = comp->gtNewIconNode(divisorValue, type);
            GenTree* mul     = comp->gtNewOperNode(GT_MUL, type, div, divisor);
            BlockRange().InsertBefore(node, dividend, div, divisor, mul);

            node->ChangeOper(GT_SUB);
            node->AsOp()->SetOp(0, dividend);
            node->AsOp()->SetOp(1, mul);
        }

        return mulhi;
    }

    LIR::Use use;
    if (!BlockRange().TryGetUse(node, &use))
    {
        return nullptr;
    }

    // We need to use the dividend node multiple times so its value needs to be
    // computed once and stored in a temp variable.
    LIR::Use opDividend(BlockRange(), &node->gtOp1, node);
    dividend = ReplaceWithLclLoad(opDividend);

    GenTree*   shiftBy    = comp->gtNewIconNode(type == TYP_INT ? 31 : 63);
    GenTreeOp* adjustment = comp->gtNewOperNode(GT_RSH, type, dividend, shiftBy);
    shiftBy->SetContained();
    BlockRange().InsertAfter(dividend, shiftBy, adjustment);

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

    dividend                    = comp->gtNewLclLoad(dividend->AsLclLoad()->GetLcl(), dividend->GetType());
    GenTreeOp* adjustedDividend = comp->gtNewOperNode(GT_ADD, type, adjustment, dividend);
    BlockRange().InsertAfter(adjustment, dividend, adjustedDividend);
    ContainCheckBinary(adjustedDividend);

    GenTree* newDivMod;
    BlockRange().Unlink(divisor);

    if (isDiv)
    {
        // perform the division by right shifting the adjusted dividend
        divisor->AsIntCon()->SetValue(genLog2(absDivisorValue));

        newDivMod = comp->gtNewOperNode(GT_RSH, type, adjustedDividend, divisor);
        divisor->SetContained();
        BlockRange().InsertAfter(adjustedDividend, divisor, newDivMod);

        if (divisorValue < 0)
        {
            // negate the result if the divisor is negative
            GenTree* neg = comp->gtNewOperNode(GT_NEG, type, newDivMod);
            BlockRange().InsertAfter(newDivMod, neg);
            newDivMod = neg;
        }
    }
    else
    {
        // divisor % dividend = dividend - divisor x (dividend / divisor)
        // divisor x (dividend / divisor) translates to (dividend >> log2(divisor)) << log2(divisor)
        // which simply discards the low log2(divisor) bits, that's just dividend & ~(divisor - 1)
        divisor->AsIntCon()->SetValue(~(absDivisorValue - 1));

        GenTreeOp* mask = comp->gtNewOperNode(GT_AND, type, adjustedDividend, divisor);
        dividend        = comp->gtNewLclLoad(dividend->AsLclLoad()->GetLcl(), dividend->GetType());
        newDivMod       = comp->gtNewOperNode(GT_SUB, type, dividend, mask);

        BlockRange().InsertAfter(adjustedDividend, divisor, mask, dividend, newDivMod);
        ContainCheckBinary(mask);
    }

    use.SetDef(newDivMod);
    BlockRange().Unlink(node);

    return newDivMod->gtNext;
}

GenTree* Lowering::LowerSignedDivRem(GenTree* node)
{
    assert(node->OperIs(GT_SDIV, GT_SREM) && varTypeIsIntegral(node->GetType()));

    GenTree* next = node->gtNext;

    if (GenTree* newNode = LowerConstIntDivRem(node->AsOp()))
    {
        return newNode;
    }

    ContainCheckDivRem(node->AsOp());

    return next;
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
//     This simply replaces `node`'s use with an appropriate SETCC/JCC node,
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

    while ((next != nullptr) && next->IsIntCon(0) && (next->gtNext != nullptr) && next->gtNext->OperIs(GT_EQ, GT_NE) &&
           (next->gtNext->AsOp()->GetOp(0) == relop) && (next->gtNext->AsOp()->GetOp(1) == next))
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
            if (next->AsUnOp()->GetOp(0) == relop)
            {
                assert(relop->OperIsCompare());

                next->ChangeOper(GT_JCC);
                cc = next->AsCC();
                cc->SetCondition(condition);
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
                use.SetDef(cc);
            }
        }
    }

    if (cc != nullptr)
    {
        node->AddImplicitFlagsDef();
        cc->AddImplicitFlagsUse();
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
            if ((cc != nullptr) && cc->GetCondition().PreferSwap())
            {
                swapOperands = true;
            }
            else
            {
                canSwapOperands = (cc == nullptr) || !GenCondition::Swap(cc->GetCondition()).PreferSwap();
            }
            break;

        case NI_SSE41_PTEST:
        case NI_AVX_PTEST:
            // If we need the Carry flag then we can't swap operands.
            canSwapOperands = (cc == nullptr) || cc->GetCondition().Is(GenCondition::EQ, GenCondition::NE);
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
            cc->SetCondition(GenCondition::Swap(cc->GetCondition()));
        }
    }
}

void Lowering::LowerFusedMultiplyAdd(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_FMA_MultiplyAddScalar);
    assert(node->IsTernary());

    GenTreeHWIntrinsic::Use* uses[3];
    unsigned                 useCount = 0;

    for (GenTreeHWIntrinsic::Use& use : node->Uses())
    {
        if (!use.GetNode()->IsHWIntrinsic() ||
            (use.GetNode()->AsHWIntrinsic()->GetIntrinsic() != NI_Vector128_CreateScalarUnsafe))
        {
            return;
        }

        uses[useCount++] = &use.GetNode()->AsHWIntrinsic()->GetUse(0);
    }

    GenTree* argX = uses[0]->GetNode();
    GenTree* argY = uses[1]->GetNode();
    GenTree* argZ = uses[2]->GetNode();

    const bool negMul = argX->OperIs(GT_FNEG) != argY->OperIs(GT_FNEG);
    if (argX->OperIs(GT_FNEG))
    {
        uses[0]->SetNode(argX->AsUnOp()->GetOp(0));
        BlockRange().Unlink(argX);
    }
    if (argY->OperIs(GT_FNEG))
    {
        uses[1]->SetNode(argY->AsUnOp()->GetOp(0));
        BlockRange().Unlink(argY);
    }
    if (argZ->OperIs(GT_FNEG))
    {
        uses[2]->SetNode(argZ->AsUnOp()->GetOp(0));
        BlockRange().Unlink(argZ);
        node->SetIntrinsic(negMul ? NI_FMA_MultiplySubtractNegatedScalar : NI_FMA_MultiplySubtractScalar);
    }
    else
    {
        node->SetIntrinsic(negMul ? NI_FMA_MultiplyAddNegatedScalar : NI_FMA_MultiplyAddScalar);
    }
}

void Lowering::LowerHWIntrinsic(GenTreeHWIntrinsic* node)
{
    if (node->TypeIs(TYP_SIMD12))
    {
        // SIMD12 HWINTRINSIC nodes produce in fact a SIMD16 value.
        node->SetType(TYP_SIMD16);
    }

    NamedIntrinsic intrinsicId = node->GetIntrinsic();

    switch (intrinsicId)
    {
        case NI_VEC_PACK:
            if (node->IsUnary())
            {
                LowerHWIntrinsicCreateBroadcast(node);
            }
            else
            {
                LowerHWIntrinsicCreate(node);
            }
            assert(!node->IsHWIntrinsic() || (node->GetIntrinsic() != intrinsicId));
            LowerNode(node);
            return;

        case NI_Vector128_CreateScalarUnsafe:
        case NI_Vector256_CreateScalarUnsafe:
            LowerHWIntrinsicCreateScalarUnsafe(node);
            break;

        case NI_VEC_SUM:
            if (node->GetOp(0)->TypeIs(TYP_SIMD32))
            {
                LowerVecSum256(node);
            }
            else
            {
                LowerVecSum128(node);
            }
            return;

        case NI_VEC_EXTRACT:
            LowerVecExtract(node);
            return;
        case NI_VEC_INSERT:
            LowerVecInsert(node);
            return;

        case NI_VEC_EQ:
            LowerVecEquality(node, GT_EQ);
            return;
        case NI_VEC_NE:
            LowerVecEquality(node, GT_NE);
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

        case NI_SSE42_CRC32B:
            node->SetOp(1, TryRemoveCastIfPresent(TYP_BYTE, node->GetOp(1)));
            break;

        case NI_SSE42_CRC32W:
            node->SetOp(1, TryRemoveCastIfPresent(TYP_SHORT, node->GetOp(1)));
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

void Lowering::LowerVecEquality(GenTreeHWIntrinsic* node, genTreeOps cmpOp)
{
    assert((node->GetIntrinsic() == NI_VEC_EQ) || (node->GetIntrinsic() == NI_VEC_NE));
    assert(node->TypeIs(TYP_UBYTE));
    assert(varTypeIsIntegral(node->GetSimdBaseType()));
    assert(comp->opts.IsIsaSupported(InstructionSet_SSE41));
    assert((cmpOp == GT_EQ) || (cmpOp == GT_NE));

    GenTree* op1 = node->GetOp(0);
    GenTree* op2 = node->GetOp(1);

    var_types type = varTypeTargetVec(op1->GetType());
    assert(type == varTypeTargetVec(op2->GetType()));

    GenCondition cmpCnd = (cmpOp == GT_EQ) ? GenCondition::EQ : GenCondition::NE;

    if (op1->IsVecZero())
    {
        std::swap(op1, op2);
    }

    if (op2->IsVecZero())
    {
        BlockRange().Unlink(op2);

        node->SetOp(0, op1);
        LIR::Use op1Use(BlockRange(), &node->GetUse(0).NodeRef(), node);
        op1 = ReplaceWithLclLoad(op1Use);
        op2 = comp->gtNewLclLoad(op1->AsLclLoad()->GetLcl(), op1->GetType());
        BlockRange().InsertAfter(op1, op2);
        node->SetOp(1, op2);

        NamedIntrinsic testz = type == TYP_SIMD32 ? NI_AVX_TestZ : NI_SSE41_TestZ;
        NamedIntrinsic ptest = type == TYP_SIMD32 ? NI_AVX_PTEST : NI_SSE41_PTEST;

        node->SetIntrinsic(testz);
        LowerHWIntrinsicCC(node, ptest, cmpCnd);

        return;
    }

    NamedIntrinsic cmpIntrinsic = type == TYP_SIMD32 ? NI_AVX2_CompareEqual : NI_SSE2_CompareEqual;
    NamedIntrinsic mskIntrinsic = type == TYP_SIMD32 ? NI_AVX2_MoveMask : NI_SSE2_MoveMask;
    int            mskConstant  = type == TYP_SIMD32 ? -1 : 0xFFFF;

    GenTree* cmp    = comp->gtNewVecNode(type, cmpIntrinsic, TYP_UBYTE, op1, op2);
    GenTree* msk    = comp->gtNewSimdHWIntrinsicNode(TYP_INT, mskIntrinsic, TYP_UBYTE, varTypeSize(type), cmp);
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
        GenTree* create128 = comp->gtNewVecNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_LONG, op);
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
    BlockRange().Unlink(op);

    if (op1->IsIntCon(0) && op2->IsIntCon(0))
    {
        node->SetIntrinsic(NI_VEC_ZERO, 0);
        BlockRange().Unlink(op1);
        BlockRange().Unlink(op2);

        return;
    }

    if (op2->IsIntCon(0))
    {
        node->SetIntrinsic(NI_SSE2_ConvertScalarToVector128Int32, TYP_INT, 16, 1);
        node->SetOp(0, op1);
        BlockRange().Unlink(op2);
        LowerNode(node);

        return;
    }

    GenTree* movd1;

    if (op1->IsIntCon(0))
    {
        movd1 = comp->gtNewVecZeroNode(TYP_SIMD16, TYP_LONG);
        BlockRange().Unlink(op1);
        BlockRange().InsertBefore(node, movd1);
    }
    else
    {
        movd1 = comp->gtNewVecNode(TYP_SIMD16, NI_SSE2_ConvertScalarToVector128Int32, TYP_INT, op1);
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
        GenTree* movd2 = comp->gtNewVecNode(TYP_SIMD16, NI_SSE2_ConvertScalarToVector128Int32, TYP_INT, op2);
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

    if (op->IsDblConPositiveZero() || op->IsIntCon(0))
    {
        BlockRange().Unlink(op);
        node->SetIntrinsic(NI_VEC_ZERO, 0);
    }
}

void Lowering::LowerHWIntrinsicCreate(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_VEC_PACK);

    var_types type    = node->GetType();
    var_types eltType = node->GetSimdBaseType();
    unsigned  numOps  = node->GetNumOps();

    assert(varTypeIsTargetVec(type));
    assert(varTypeIsArithmetic(eltType));
    assert(numOps == varTypeSize(type) / varTypeSize(eltType));

#ifndef TARGET_64BIT
    if (eltType == TYP_LONG)
    {
        assert((numOps == 2) || (numOps == 4));

        GenTree* ops[8];

        for (unsigned i = 0; i < numOps; i++)
        {
            GenTree* op = node->GetOp(i);
            assert(op->OperIs(GT_LONG));
            ops[i * 2]     = op->AsOp()->GetOp(0);
            ops[i * 2 + 1] = op->AsOp()->GetOp(1);
            BlockRange().Unlink(op);
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

    if (type == TYP_SIMD32)
    {
        assert(comp->opts.IsIsaSupported(InstructionSet_AVX));

        GenTreeHWIntrinsic* lo = comp->gtNewVecNode(TYP_SIMD16, NI_VEC_PACK, eltType);
        GenTreeHWIntrinsic* hi = comp->gtNewVecNode(TYP_SIMD16, NI_VEC_PACK, eltType);

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
        if (scalar->IsIntCon(0) || scalar->IsDblConPositiveZero())
        {
            scalar->ChangeOper(GT_HWINTRINSIC);
            scalar->SetType(TYP_SIMD16);
            scalar->AsHWIntrinsic()->SetIntrinsic(NI_VEC_ZERO, eltType, 16, 0);
            return scalar;
        }

        GenTree* vec = comp->gtNewVecNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, eltType, scalar);
        BlockRange().InsertAfter(scalar, vec);
        return vec;
    };

    GenTree* op1 = node->GetOp(0);

    if ((eltType == TYP_LONG) && comp->compOpportunisticallyDependsOn(InstructionSet_SSE41_X64))
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

    if (eltType == TYP_LONG)
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

        bool op2IsMem = IsMemOperand(op2) || op2->IsDblCon();

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
                BlockRange().Unlink(op);
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
                zero = comp->gtNewVecZeroNode(TYP_SIMD16, TYP_FLOAT);
                vec  = zero;
            }

            GenTree* idx = comp->gtNewIconNode((i << 4) | zeroOpMask);

            if (nonZeroOpMask != 1)
            {
                vec = comp->gtNewVecNode(TYP_SIMD16, NI_SSE41_Insert, TYP_FLOAT, vec, op, idx);

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
                vec = comp->gtNewVecNode(TYP_SIMD16, insert, eltType, vec, op, idx);
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
            BlockRange().Unlink(op1);
            return op2;
        }

        NamedIntrinsic intrinsic = eltType == TYP_FLOAT ? NI_SSE_UnpackLow : NI_SSE2_UnpackLow;
        GenTree*       unpack    = comp->gtNewVecNode(TYP_SIMD16, intrinsic, eltType, op1, op2);
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

        eltType = TYP_INT;
        numOps  = 4;
    }

    assert(numOps == 4);

    v[0] = UnpackLow(eltType, v[0], v[1]);
    v[1] = UnpackLow(eltType, v[2], v[3]);

    NamedIntrinsic intrinsic;

    if (eltType != TYP_FLOAT)
    {
        assert(eltType == TYP_INT);
        intrinsic = NI_SSE2_UnpackLow;
        eltType   = TYP_LONG;
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
    assert(node->GetIntrinsic() == NI_VEC_PACK);
    assert(node->IsUnary());

    var_types type    = node->GetType();
    var_types eltType = node->GetSimdBaseType();
    GenTree*  op1     = node->GetOp(0);

    assert(varTypeIsTargetVec(type));
    assert(varTypeIsArithmetic(eltType) && (varTypeNodeType(eltType) == eltType));

    VectorConstant vecConst;

    if (vecConst.Broadcast(node))
    {
        LowerHWIntrinsicCreateConst(node, vecConst);
        return;
    }

    if ((type == TYP_SIMD32) && !comp->compOpportunisticallyDependsOn(InstructionSet_AVX2))
    {
        assert(comp->opts.IsIsaSupported(InstructionSet_AVX));

        GenTree* half = comp->gtNewVecNode(TYP_SIMD16, NI_VEC_PACK, eltType, op1);
        BlockRange().InsertAfter(op1, half);

        node->SetOp(0, half);
        LIR::Use        use(BlockRange(), &node->GetUse(0).NodeRef(), node);
        GenTreeLclLoad* tmp1 = ReplaceWithLclLoad(use);
        GenTreeLclLoad* tmp2 = comp->gtNewLclLoad(tmp1->GetLcl(), TYP_SIMD16);
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
        GenTree* lo = comp->gtNewVecNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_INT, op1->AsOp()->GetOp(0));
        GenTree* hi = comp->gtNewVecNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_INT, op1->AsOp()->GetOp(1));

        vec = comp->gtNewVecNode(TYP_SIMD16, NI_SSE2_UnpackLow, TYP_INT, lo, hi);
        BlockRange().InsertAfter(op1, lo, hi, vec);
        BlockRange().Unlink(op1);
        LowerNode(lo);
        LowerNode(hi);
        LowerNode(vec);
    }
    else
#endif
    {
        op1 = TryRemoveCastIfPresent(eltType, op1);
        vec = comp->gtNewVecNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, eltType, op1);
        BlockRange().InsertAfter(op1, vec);
        LowerNode(vec);
    }

    if (type == TYP_SIMD32)
    {
        assert(comp->opts.IsIsaSupported(InstructionSet_AVX2));

        node->SetIntrinsic(NI_AVX2_BroadcastScalarToVector256, 1);
        node->SetOp(0, vec);

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
        GenTree* tmp1 = ReplaceWithLclLoad(use);
        GenTree* tmp2 = comp->gtNewLclLoad(tmp1->AsLclLoad()->GetLcl(), TYP_SIMD16);
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
        GenTree* tmp1 = ReplaceWithLclLoad(use);
        GenTree* tmp2 = comp->gtNewLclLoad(tmp1->AsLclLoad()->GetLcl(), TYP_SIMD16);
        BlockRange().InsertBefore(node, tmp2);
        node->SetIntrinsic(NI_SSE_MoveLowToHigh, TYP_FLOAT, 2);
        node->SetOp(0, tmp1);
        node->SetOp(1, tmp2);

        return;
    }

    if (eltType == TYP_LONG)
    {
        node->SetOp(0, vec);
        LIR::Use use(BlockRange(), &node->GetUse(0).NodeRef(), node);
        GenTree* tmp1 = ReplaceWithLclLoad(use);
        GenTree* tmp2 = comp->gtNewLclLoad(tmp1->AsLclLoad()->GetLcl(), TYP_SIMD16);
        BlockRange().InsertBefore(node, tmp2);
        node->SetIntrinsic(NI_SSE2_UnpackLow, 2);
        node->SetOp(0, tmp1);
        node->SetOp(1, tmp2);

        return;
    }

    if (varTypeIsByte(eltType) && comp->compOpportunisticallyDependsOn(InstructionSet_SSSE3))
    {
        GenTree* zero = comp->gtNewVecNode(TYP_SIMD16, NI_VEC_ZERO, TYP_UBYTE);
        BlockRange().InsertBefore(node, zero);
        node->SetIntrinsic(NI_SSSE3_Shuffle, 2);
        node->SetOp(0, vec);
        node->SetOp(1, zero);

        return;
    }

    assert(varTypeIsIntegral(eltType) && (eltType != TYP_LONG));

    if (varTypeIsByte(eltType))
    {
        node->SetOp(0, vec);
        LIR::Use use(BlockRange(), &node->GetUse(0).NodeRef(), node);
        GenTree* tmp1 = ReplaceWithLclLoad(use);
        GenTree* tmp2 = comp->gtNewLclLoad(tmp1->AsLclLoad()->GetLcl(), TYP_SIMD16);
        vec           = comp->gtNewVecNode(TYP_SIMD16, NI_SSE2_UnpackLow, TYP_UBYTE, tmp1, tmp2);
        BlockRange().InsertAfter(tmp1, tmp2, vec);
        LowerNode(vec);

        eltType = TYP_USHORT;
    }

    if (varTypeIsShort(eltType))
    {
        node->SetOp(0, vec);
        LIR::Use use(BlockRange(), &node->GetUse(0).NodeRef(), node);
        GenTree* tmp1 = ReplaceWithLclLoad(use);
        GenTree* tmp2 = comp->gtNewLclLoad(tmp1->AsLclLoad()->GetLcl(), TYP_SIMD16);
        vec           = comp->gtNewVecNode(TYP_SIMD16, NI_SSE2_UnpackLow, TYP_USHORT, tmp1, tmp2);
        BlockRange().InsertAfter(tmp1, tmp2, vec);
        LowerNode(vec);

        INDEBUG(eltType = TYP_INT);
    }

    assert(eltType == TYP_INT);

    GenTree* idx = comp->gtNewIconNode(0);
    BlockRange().InsertBefore(node, idx);
    node->SetIntrinsic(NI_SSE2_Shuffle, TYP_INT, 2);
    node->SetOp(0, vec);
    node->SetOp(1, idx);
}

void Lowering::LowerHWIntrinsicCreateConst(GenTreeHWIntrinsic* node, const VectorConstant& vecConst)
{
    var_types type    = node->GetType();
    var_types eltType = node->GetSimdBaseType();
    unsigned  numOps  = node->GetNumOps();

    assert(varTypeIsTargetVec(type));
    assert(varTypeIsArithmetic(eltType));

    for (unsigned i = 0; i < numOps; i++)
    {
#ifndef TARGET_64BIT
        if (node->GetOp(i)->OperIs(GT_LONG))
        {
            BlockRange().Unlink(node->GetOp(i)->AsOp()->GetOp(0));
            BlockRange().Unlink(node->GetOp(i)->AsOp()->GetOp(1));
        }
#endif

        BlockRange().Unlink(node->GetOp(i));
    }

    if (vecConst.AllBitsZero(type))
    {
        node->SetIntrinsic(NI_VEC_ZERO);
        node->SetNumOps(0);
        return;
    }

    if (vecConst.AllBitsOne(type))
    {
        node->SetIntrinsic(NI_VEC_ONE_BITS);
        node->SetNumOps(0);
        return;
    }

    unsigned align = comp->compCodeOpt() == SMALL_CODE ? 1 : varTypeSize(type);

    ConstData* data = comp->codeGen->GetConst(vecConst.u8, varTypeSize(type), align DEBUGARG(type));

    GenTree* addr = new (comp, GT_CONST_ADDR) GenTreeConstAddr(data);
    BlockRange().InsertBefore(node, addr);

    GenTree* indir = node;
    indir->ChangeOper(GT_IND_LOAD);
    indir->AsIndLoad()->SetAddr(addr);
}

void Lowering::LowerVecExtract(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_VEC_EXTRACT);

    GenTree* vec = node->GetOp(0);
    GenTree* idx = node->GetOp(1);

    if (IsMemOperand(vec) && IsSafeToMoveMemOperandForward(node, vec))
    {
        vec->SetContained();
    }

    if (!idx->IsIntCon())
    {
        if (!vec->isContained())
        {
            LclVarDsc*       tempLcl = GetSimdMemoryTemp(vec->GetType());
            GenTreeLclStore* store   = comp->gtNewLclStore(tempLcl, vec->GetType(), vec);
            BlockRange().InsertAfter(vec, store);

            vec = comp->gtNewLclLoad(tempLcl, vec->GetType());
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
        idx = comp->gtNewOperNode(GT_UXT, TYP_LONG, idx);
        BlockRange().InsertBefore(node, idx);
        node->SetOp(1, idx);
#endif

        return;
    }

    var_types eltType = node->GetSimdBaseType();

    // We should have a bounds check inserted for any index outside the allowed range
    // but we need to generate some code anyways, and so we'll mask here for simplicity.

    unsigned count = varTypeTargetVecSize(vec->GetType()) / varTypeSize(eltType);
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

    if (vec->TypeIs(TYP_SIMD32))
    {
        assert(comp->opts.IsIsaSupported(InstructionSet_AVX));

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
            vec = comp->gtNewVecNode(TYP_SIMD16, NI_Vector256_GetLower, eltType, vec);
            BlockRange().InsertBefore(node, vec);
        }

        LowerNode(vec);

        node->SetIntrinsic(NI_VEC_EXTRACT);
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
        vec = comp->gtNewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical128BitLane, eltType, vec, idx);
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
                node->SetIntrinsic(NI_SSE41_X64_Extract);
                break;
            case TYP_BYTE:
            case TYP_UBYTE:
            case TYP_INT:
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
            case TYP_BYTE:
            case TYP_UBYTE:
            case TYP_SHORT:
            case TYP_USHORT:
            case TYP_INT:
                node->SetIntrinsic(NI_SSE2_ConvertToInt32, TYP_INT, 1);
                node->SetType(TYP_INT);
                break;
            case TYP_LONG:
                node->SetIntrinsic(NI_SSE2_X64_ConvertToInt64, TYP_LONG, 1);
                node->SetType(TYP_LONG);
                break;
            default:
                unreached();
        }

        node->SetOp(0, vec);

        if (idx != nullptr)
        {
            BlockRange().Unlink(idx);
        }
    }

    LowerNode(node);

    if ((eltType == TYP_BYTE) || (eltType == TYP_SHORT) ||
        ((eltType == TYP_UBYTE) && !comp->compOpportunisticallyDependsOn(InstructionSet_SSE41)))
    {
        LIR::Use use;
        if (BlockRange().TryGetUse(node, &use))
        {
            GenTreeUnOp* conv = comp->gtNewOperNode(GT_CONV, eltType, node);
            BlockRange().InsertAfter(node, conv);
            use.SetDef(conv);
            LowerNode(conv);
        }
    }
}

void Lowering::LowerVecInsert(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_VEC_INSERT);

    var_types      eltType = node->GetSimdBaseType();
    GenTree*       vec     = node->GetOp(0);
    GenTreeIntCon* idx     = node->GetOp(1)->AsIntCon();
    GenTree*       elt     = node->GetOp(2);
    unsigned       index   = idx->GetUInt32Value();
    unsigned       count   = varTypeSize(node->GetType()) / varTypeSize(eltType);

    assert(index < count);
    assert(varTypeIsArithmetic(eltType) && (varTypeNodeType(eltType) == eltType));

    LclVarDsc* vec256TempLcl = nullptr;
    unsigned   index256      = index;

    if (vec->TypeIs(TYP_SIMD32))
    {
        assert(comp->opts.IsIsaSupported(InstructionSet_AVX));

        LIR::Use vecUse(BlockRange(), &node->GetUse(0).NodeRef(), node);
        vec           = ReplaceWithLclLoad(vecUse);
        vec256TempLcl = vec->AsLclLoad()->GetLcl();

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
            vec = comp->gtNewVecNode(TYP_SIMD16, NI_Vector256_GetLower, eltType, vec);
            BlockRange().InsertBefore(node, vec);
        }

        LowerNode(vec);
    }

#ifndef TARGET_64BIT
    if (eltType == TYP_LONG)
    {
        assert(elt->OperIs(GT_LONG));
        assert(comp->opts.IsIsaSupported(InstructionSet_SSE41));

        index *= 2;
        index256 *= 2;
        eltType = TYP_INT;

        GenTree* eltLo = elt->AsOp()->GetOp(0);
        GenTree* idxLo = comp->gtNewIconNode(index);
        vec            = comp->gtNewVecNode(TYP_SIMD16, NI_VEC_INSERT, eltType, vec, idxLo, eltLo);
        BlockRange().InsertBefore(node, idxLo, vec);
        LowerNode(vec);

        index++;
        BlockRange().Unlink(elt);
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
            assert(comp->opts.IsIsaSupported(InstructionSet_SSE41));
            intrinsic = NI_SSE41_Insert;
            break;
#ifdef TARGET_64BIT
        case TYP_LONG:
            assert(comp->opts.IsIsaSupported(InstructionSet_SSE41_X64));
            intrinsic = NI_SSE41_X64_Insert;
            break;
#endif

        case TYP_DOUBLE:
            intrinsic = (index == 0) ? NI_SSE2_MoveScalar : NI_SSE2_UnpackLow;
            BlockRange().Unlink(idx);
            idx = nullptr;
            elt = comp->gtNewVecNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_DOUBLE, elt);
            BlockRange().InsertBefore(node, elt);
            LowerNode(elt);
            break;

        case TYP_FLOAT:
            if (comp->compOpportunisticallyDependsOn(InstructionSet_SSE41) && ((index != 0) || elt->IsDblCon()))
            {
                intrinsic = NI_SSE41_Insert;
                idx->AsIntCon()->SetValue(index << 4);
            }
            else if (index == 0)
            {
                intrinsic = NI_SSE_MoveScalar;
                BlockRange().Unlink(idx);
                idx = nullptr;
                elt = comp->gtNewVecNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_FLOAT, elt);
                BlockRange().InsertBefore(node, elt);
                LowerNode(elt);
            }
            else
            {
                node->SetOp(0, vec);
                LIR::Use op1Use(BlockRange(), &node->GetUse(0).NodeRef(), node);
                vec = ReplaceWithLclLoad(op1Use);

                elt = comp->gtNewVecNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_FLOAT, elt);
                BlockRange().InsertBefore(node, elt);
                LowerNode(elt);

                GenTree*      vec2 = comp->gtNewLclLoad(vec->AsLclLoad()->GetLcl(), TYP_SIMD16);
                constexpr int controlBits1[]{0, 0, 0b00110000, 0b00100000};
                GenTree*      imm = comp->gtNewIconNode(controlBits1[index]);
                elt               = comp->gtNewVecNode(TYP_SIMD16, NI_SSE_Shuffle, TYP_FLOAT, elt, vec2, imm);
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

    if (vec256TempLcl != nullptr)
    {
        if (idx == nullptr)
        {
            elt = comp->gtNewVecNode(TYP_SIMD16, intrinsic, eltType, vec, elt);
        }
        else
        {
            elt = comp->gtNewVecNode(TYP_SIMD16, intrinsic, eltType, vec, elt, idx);
        }

        intrinsic = NI_AVX_InsertVector128;

        vec = comp->gtNewLclLoad(vec256TempLcl, TYP_SIMD32);
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
                    BlockRange().Unlink(vecElt);
                    break;
                case NI_SSE_LoadScalarVector128:
                case NI_SSE_LoadVector128:
                case NI_SSE_LoadAlignedVector128:
                    GenTree* addr;
                    addr = vecElt->GetOp(0);
                    elt->ChangeOper(GT_IND_LOAD);
                    elt->SetType(TYP_FLOAT);
                    elt->AsIndLoad()->SetAddr(addr);
                    break;
                default:
                    break;
            }
        }
        else if (elt->OperIs(GT_IND_LOAD, GT_LCL_LOAD_FLD))
        {
            elt->SetType(TYP_FLOAT);
        }
        else if (elt->OperIs(GT_LCL_LOAD) && elt->AsLclLoad()->GetLcl()->lvDoNotEnregister)
        {
            elt->ChangeToLclLoadFld(TYP_FLOAT, elt->AsLclLoad()->GetLcl(), 0, FieldSeqStore::NotAField());
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
        if (elt->IsDblCon() || (IsMemOperand(elt) && IsSafeToMoveMemOperandForward(node, elt)))
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

    if (vec->IsHWIntrinsicZero() && !elt->OperIs(GT_LCL_LOAD) && comp->codeGen->UseVexEncoding())
    {
        vec->SetContained();
    }
    else if (elt->TypeIs(TYP_FLOAT))
    {
        elt->SetRegOptional();
    }
}

void Lowering::LowerVecSum128(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_VEC_SUM);

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

    LIR::Use        vecUse(BlockRange(), &node->GetUse(0).NodeRef(), node);
    GenTreeLclLoad* vec = ReplaceWithLclLoad(vecUse);

    GenTree*       sum  = vec;
    GenTree*       sum2 = nullptr;
    NamedIntrinsic add  = NI_Illegal;

    for (unsigned i = 0; i < haddCount; i++)
    {
        sum2 = comp->gtNewLclLoad(sum->AsLclLoad()->GetLcl(), TYP_SIMD16);
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
                sum2         = comp->gtNewVecNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, sum2, imm);
                BlockRange().InsertBefore(node, imm, sum2);
            }
            else if ((i == 0) && (eltType == TYP_FLOAT))
            {
                GenTree* sum3 = comp->gtNewLclLoad(sum2->AsLclLoad()->GetLcl(), TYP_SIMD16);
                GenTree* imm  = comp->gtNewIconNode(0b10110001);
                sum2          = comp->gtNewVecNode(TYP_SIMD16, NI_SSE_Shuffle, TYP_FLOAT, sum2, sum3, imm);
                BlockRange().InsertBefore(node, sum3, imm, sum2);
            }
            else
            {
                assert(varTypeIsFloating(eltType));
                // For Vector3 we need to add the original vec[2] element,
                // not sum[2] which would be wrong if vec[3] wasn't 0.
                LclVarDsc* lcl  = size == 12 ? vec->GetLcl() : sum2->AsLclLoad()->GetLcl();
                GenTree*   sum3 = comp->gtNewLclLoad(lcl, TYP_SIMD16);
                sum2            = comp->gtNewVecNode(TYP_SIMD16, NI_SSE_MoveHighToLow, TYP_FLOAT, sum2, sum3);
                BlockRange().InsertBefore(node, sum3, sum2);
            }

            LowerNode(sum2);
            add = eltType == TYP_FLOAT ? NI_SSE_Add : NI_SSE2_Add;
        }

        if (i < haddCount - 1)
        {
            sum = comp->gtNewVecNode(TYP_SIMD16, add, eltType, sum, sum2);
            BlockRange().InsertBefore(node, sum);
            LowerNode(sum);
            node->SetOp(0, sum);
            LIR::Use sumUse(BlockRange(), &node->GetUse(0).NodeRef(), node);
            sum = ReplaceWithLclLoad(sumUse);
        }
    }

    node->SetIntrinsic(add, eltType, 16, 2);
    node->SetOp(0, sum);
    node->SetOp(1, sum2);
    LowerNode(node);
}

void Lowering::LowerVecSum256(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_VEC_SUM);

    var_types eltType = node->GetSimdBaseType();
    GenTree*  vec     = node->GetOp(0);

    assert(vec->TypeIs(TYP_SIMD32));
    assert(varTypeIsArithmetic(eltType));

    NamedIntrinsic extract = varTypeIsFloating(eltType) ? NI_AVX_ExtractVector128 : NI_AVX2_ExtractVector128;
    NamedIntrinsic add     = eltType == TYP_FLOAT ? NI_SSE_Add : NI_SSE2_Add;

    LIR::Use vecUse(BlockRange(), &node->GetUse(0).NodeRef(), node);
    vec = ReplaceWithLclLoad(vecUse);

    GenTree* vec2     = comp->gtNewLclLoad(vec->AsLclLoad()->GetLcl(), TYP_SIMD32);
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

bool Lowering::IsIndLoadRMWCandidate(GenTreeIndStore* store, GenTreeIndir* load, GenTree* src)
{
    GenTree* loadAddr  = load->GetAddr();
    GenTree* storeAddr = store->GetAddr();

    if ((loadAddr->GetOper() != storeAddr->GetOper()) || !IndirsAreRMWEquivalent(load, store))
    {
        return false;
    }

    // RMW stores require multiple interference checks to ensure correctness:
    //  - The RMW operation (e.g. ADD) needs to be moved before the store. This is
    //    trivial for the operation itself, it's always side effect free, but binary
    //    operations have a source operand that needs checking if it's a LCL_VAR.
    //  - The load needs to be moved before the store.
    //  - Load and store addresses need not be moved, it would be perfectly fine if
    //    the store address is computed into a register anywhere before the store.
    //    But this gets more complicated if addresses contain LCL_VAR uses because
    //    IndirsAreRMWEquivalent only checks that the 2 addresses expressions are the
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
            if (base->OperIs(GT_LCL_LOAD))
            {
                m_scratchSideEffects.AddNode(comp, base);
                base->SetLIRMark();
                markCount++;
            }
        }

        if (GenTree* index = am->GetIndex())
        {
            if (index->OperIs(GT_LCL_LOAD))
            {
                m_scratchSideEffects.AddNode(comp, index);
                index->SetLIRMark();
                markCount++;
            }
        }
    }
    else if (loadAddr->OperIs(GT_LCL_LOAD))
    {
        // AddNode(load) already added this but we still need to mark it.
        loadAddr->SetLIRMark();
        markCount++;
    }

    if ((src != nullptr) && src->OperIs(GT_LCL_LOAD))
    {
        m_scratchSideEffects.AddNode(comp, src);
        src->SetLIRMark();
        markCount++;
    }

    if (storeAddr->IsAddrMode())
    {
        assert(storeAddr->isContained());
    }
    else if (storeAddr->OperIs(GT_LCL_LOAD))
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

        // TODO-MIKE-Review: Why does IsSafeToMoveMemOperandForward uses strict checking while this doesn't?
        hasInterference = hasInterference || m_scratchSideEffects.InterferesWith(comp, node, false);
    }

    return !hasInterference;
}

bool Lowering::IndirsAreRMWEquivalent(GenTreeIndir* indir1, GenTreeIndir* indir2)
{
    assert(indir1->OperIs(GT_IND_LOAD));
    assert(indir2->OperIs(GT_IND_STORE));

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
        case GT_LCL_LOAD:
        case GT_CNS_INT:
            return LeavesAreRMWEquivalent(addr1, addr2);

        case GT_LEA:
        {
            GenTreeAddrMode* am1 = addr1->AsAddrMode();
            GenTreeAddrMode* am2 = addr2->AsAddrMode();
            return LeavesAreRMWEquivalent(am1->GetBase(), am2->GetBase()) &&
                   LeavesAreRMWEquivalent(am1->GetIndex(), am2->GetIndex()) && (am1->GetScale() == am2->GetScale()) &&
                   (am1->GetOffset() == am2->GetOffset());
        }

        default:
            return false;
    }
}

bool Lowering::LeavesAreRMWEquivalent(GenTree* node1, GenTree* node2)
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
                   (node1->AsIntCon()->IsHandle() == node2->AsIntCon()->IsHandle());
        case GT_LCL_LOAD:
            return node1->AsLclLoad()->GetLcl() == node2->AsLclLoad()->GetLcl();
        default:
            return false;
    }
}

GenTreeIndir* Lowering::IsStoreIndRMW(GenTreeIndStore* store)
{
    assert(varTypeIsIntegralOrI(store->GetType()));

    GenTree* storeAddr = store->GetAddr();

    if (!storeAddr->OperIs(GT_LEA, GT_LCL_LOAD, GT_CNS_INT))
    {
        return nullptr;
    }

    if (storeAddr->IsAddrMode() && !storeAddr->isContained())
    {
        // Give up if the address is an uncontained LEA (likely due to base/index interference).
        // This is rare and ignoring it simplifies IsIndLoadRMWCandidate interference checking.
        return nullptr;
    }

    GenTree* op = store->GetValue();
    assert(op->OperIsRMWMemOp());

    if (op->OperIsBinary())
    {
        if (op->OperIsShiftOrRotate() && varTypeIsSmall(store->GetType()))
        {
            return nullptr;
        }

        GenTree* op1 = op->AsOp()->GetOp(0);
        GenTree* op2 = op->AsOp()->GetOp(1);

        if (op->AsOp()->IsCommutative() && op2->OperIs(GT_IND_LOAD) &&
            IsIndLoadRMWCandidate(store, op2->AsIndLoad(), op1))
        {
            return op2->AsIndLoad();
        }

        if (op1->OperIs(GT_IND_LOAD) && IsIndLoadRMWCandidate(store, op1->AsIndLoad(), op2))
        {
            return op1->AsIndLoad();
        }
    }
    else
    {
        assert(op->OperIsUnary());

        GenTree* op1 = op->AsUnOp()->GetOp(0);

        if (op1->OperIs(GT_IND_LOAD) && IsIndLoadRMWCandidate(store, op1->AsIndLoad(), nullptr))
        {
            return op1->AsIndLoad();
        }
    }

    return nullptr;
}

bool Lowering::IsCallTargetInRange(void* addr)
{
    // Anything is in range for x64.
    return true;
}

bool Lowering::IsImmOperand(GenTree* operand, GenTree* instr) const
{
    return operand->IsIntConFitsInInt32() && !operand->AsIntCon()->ImmedValNeedsReloc(comp);
}

// Returns one of the operands of given binary oper that is to be preferred
// for marking as reg optional.
//
// Since only one of op1 or op2 can be a memory operand on xarch, only
// one of  them have to be marked as reg optional.  Since Lower doesn't
// know apriori which of op1 or op2 is not likely to get a register, it
// has to make a guess. This routine encapsulates heuristics that
// guess whether it is likely to be beneficial to mark op1 or op2 as
// reg optional.
//
// TODO-MIKE-Review: It's not clear why only one operand can be marked
// reg-optional. With some exceptions like CMP, the binary operator
// will get a destination register anyway so if both operands end up
// being spilled we can simply load the first into the destination reg
// and use the second as a memory operand.
//
// Note: if the tree oper is neither commutative nor a compare oper
// then only op2 can be reg optional on xarch and hence no need to
// call this routine.
GenTree* Lowering::GetPreferredRegOptionalOperand(GenTree* op1, GenTree* op2)
{
    assert(!op1->IsRegOptional() && !op2->IsRegOptional());

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
    // TODO: Reg optional def positions is currently not yet supported.
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

    // We default to op1, as op2 is likely to have the shorter lifetime.
    GenTree* preferredOp = op1;

    if (op1->OperIs(GT_LCL_LOAD) && op2->OperIs(GT_LCL_LOAD))
    {
        LclVarDsc* lcl1 = op1->AsLclLoad()->GetLcl();
        LclVarDsc* lcl2 = op2->AsLclLoad()->GetLcl();

        if (!lcl1->lvDoNotEnregister && !lcl2->lvDoNotEnregister)
        {
            // Both are enregisterable locals. The one with lower weight is less likely to get a
            // register and hence beneficial to mark the one with lower weight as reg optional.
            // If either is not tracked, it may be that it was introduced after liveness was run,
            // in which case we will always prefer op1.
            // TODO: Should we use raw ref count instead of weight?

            if (lcl1->HasLiveness() && lcl2->HasLiveness() && (lcl1->GetRefWeight() >= lcl2->GetRefWeight()))
            {
                preferredOp = op2;
            }
        }
    }
    else if (!op1->OperIs(GT_LCL_LOAD) && op2->OperIs(GT_LCL_LOAD))
    {
        preferredOp = op2;
    }

    return preferredOp;
}

void Lowering::ContainCheckCallAddr(GenTreeCall* call)
{
#ifdef TARGET_X86
    // Fast tail calls aren't currently supported on x86, but if they ever are, the code
    // below that handles indirect VSD calls will need to be fixed.
    assert(!call->IsIndirectCall() || !call->IsFastTailCall() || !call->IsVirtualStub());
#endif

    if (GenTree* addr = call->GetCallAddr())
    {
        assert(addr->TypeIs(TYP_I_IMPL));

        if (call->IsFastTailCall())
        {
            // For fast tail calls the address has to be computed into a register,
            // to be used by the jmp instruction in the epilog.
            return;
        }

        if (addr->OperIs(GT_IND_LOAD))
        {
            addr->SetContained();
        }
    }
}

#ifdef FEATURE_HW_INTRINSICS
void Lowering::ContainCheckHWIntrinsicAddr(GenTreeHWIntrinsic* node, GenTree* addr)
{
    assert(addr->TypeIs(TYP_I_IMPL, TYP_BYREF));

    if (addr->OperIs(GT_CONST_ADDR, GT_LCL_ADDR) ||
        (addr->IsIntCon() AMD64_ONLY(&&comp->IsRIPRelativeAddress(addr->AsIntCon()))))
    {
        addr->SetContained();
        return;
    }

    if (addr->OperIs(GT_ADD) && !TryCreateAddrMode(addr, true))
    {
        return;
    }

    if (GenTreeAddrMode* am = addr->IsAddrMode())
    {
        if (!IsSafeToMoveAddrModeForward(node, am))
        {
            return;
        }

        addr->SetContained();
    }
}
#endif // FEATURE_HW_INTRINSICS

void Lowering::ContainCheckIndir(GenTreeIndir* node)
{
    assert(!node->TypeIs(TYP_STRUCT));

    GenTree* addr = node->GetAddr();

    if (GenTreeAddrMode* am = addr->IsAddrMode())
    {
#ifdef FEATURE_SIMD
        if (node->TypeIs(TYP_SIMD12) && (am->GetOffset() > INT32_MAX - 8))
        {
            return;
        }
#endif

        if (!IsSafeToMoveAddrModeForward(node, am))
        {
            return;
        }

        addr->SetContained();
    }
    else if (addr->OperIs(GT_CONST_ADDR, GT_LCL_ADDR) ||
             (addr->IsIntCon() AMD64_ONLY(&&comp->IsRIPRelativeAddress(addr->AsIntCon()))))
    {
#ifdef FEATURE_SIMD
        if (node->TypeIs(TYP_SIMD12))
        {
            return;
        }
#endif

        addr->SetContained();
    }
}

void Lowering::ContainCheckIndStore(GenTreeIndStore* store)
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
    // and INT or LONG store of zero to memory, because we can generate smaller
    // code by zeroing a register and then storing it.

    if (IsImmOperand(value, store) && (!value->IsIntCon(0) || varTypeIsSmall(store->GetType())))
    {
        value->SetContained();
    }
}

void Lowering::ContainCheckDivRem(GenTreeOp* node)
{
    assert(node->OperIs(GT_SDIV, GT_SREM, GT_UDIV, GT_UREM) && varTypeIsIntegral(node->GetType()));

#ifdef TARGET_X86
    GenTree* dividend = node->GetOp(0);

    if (dividend->OperIs(GT_LONG))
    {
        assert(node->OperIs(GT_UREM));
        dividend->SetContained();
        return;
    }
#endif

    GenTree* divisor = node->GetOp(1);

    if (IsMemOperand(divisor) && (divisor->GetType() == node->GetType()))
    {
        divisor->SetContained();
    }
    else
    {
        divisor->SetRegOptional();
    }
}

void Lowering::ContainCheckShiftRotate(GenTreeOp* node)
{
    assert(node->OperIsShiftOrRotate());

#ifdef TARGET_X86
    if (node->OperIs(GT_LSH_HI, GT_RSH_LO))
    {
        GenTree* value = node->GetOp(0);
        assert(value->OperIs(GT_LONG));
        value->SetContained();
    }
#endif

    GenTree* shiftBy = node->GetOp(1);

    if (IsImmOperand(shiftBy, node) && FitsIn<uint8_t>(shiftBy->AsIntCon()->GetValue()))
    {
        shiftBy->SetContained();
    }
}

void Lowering::ContainCheckStoreLcl(GenTreeLclRef* store)
{
    assert(store->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD));

    GenTree* src = store->GetOp(0);

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

        if (store->TypeIs(TYP_SIMD12) && IsMemStore(store))
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

    var_types type = store->GetLcl()->GetRegisterType(store);

    if (IsImmOperand(src, store) && (!src->IsIntCon(0) || varTypeIsSmall(type)))
    {
        src->SetContained();
        return;
    }

    if (src->OperIsRMWMemOp() && IsMemStore(store) && varTypeIsIntegral(store->GetType()) &&
        !src->HasImplicitFlagsDef() && !src->HasImplicitFlagsUse())
    {
        // TODO-MIKE-CQ: This usually fails when address exposed small int LCL_LOADs
        // are involved due to useless casts. The load is hidden by a widening cast
        // that's not really needed because LCL_LOADs that load from memory do implicit
        // widening. There may also be a narrowing cast on stores to such locals, even
        // though it's not required due to load widening.

        GenTree*   op1     = src->OperIsBinary() ? src->AsOp()->GetOp(0) : src->AsUnOp()->GetOp(0);
        GenTree*   op2     = src->OperIsBinary() ? src->AsOp()->GetOp(1) : nullptr;
        LclVarDsc* lcl     = store->GetLcl();
        unsigned   lclOffs = store->GetLclOffs();
        GenTree*   load    = nullptr;

        if (op1->IsLclRef() && (op1->AsLclRef()->GetLcl() == lcl) && (op1->AsLclRef()->GetLclOffs() == lclOffs))
        {
            load = op1;
        }
        else if ((op2 != nullptr) && src->AsOp()->IsCommutative() && op2->IsLclRef() &&
                 (op2->AsLclRef()->GetLcl() == lcl) && (op2->AsLclRef()->GetLclOffs() == lclOffs))
        {
            load = op2;
        }

        if ((load != nullptr) && (varTypeSize(load->GetType()) == varTypeSize(store->GetType())) &&
            (!varTypeIsSmall(load->GetType()) || !src->OperIs(GT_ROL, GT_ROR)) &&
            (!varTypeIsSmallSigned(load->GetType()) || !src->OperIs(GT_RSZ)) &&
            IsSafeToMoveMemOperandForward(store, load))
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

void Lowering::ContainCheckIntToFloat(GenTreeUnOp* cast)
{
    GenTree* src = cast->GetOp(0);

    // The source of cvtsi2sd and similar instructions can be a memory operand but it must
    // be 4 or 8 bytes in size so it cannot be a small int. It's likely possible to make a
    // "normalize on store" local reg-optional but it's probably not worth the extra work.
    // Also, ULONG to DOUBLE/FLOAT casts require checking the sign of the source so allowing
    // a memory operand would result in 2 loads instead of 1.

    if (!varTypeIsSmall(src->GetType()) && (!src->TypeIs(TYP_LONG) || cast->OperIs(GT_STOF)))
    {
        if (IsMemOperand(src))
        {
            // Since a floating point cast can't throw we can move the cast
            // right after the source node to avoid the interference check.
            if (cast->gtPrev != src)
            {
                BlockRange().Unlink(cast);
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

void Lowering::ContainCheckFloatToInt(GenTreeUnOp* cast)
{
    assert(cast->OperIs(GT_FTOS, GT_FTOU) && cast->TypeIs(TYP_INT, TYP_LONG));

    GenTree* src = cast->GetOp(0);

    if (IsMemOperand(src))
    {
        // Since a floating point cast can't throw we can move the cast
        // right after the source node to avoid the interference check.
        if (cast->gtPrev != src)
        {
            BlockRange().Unlink(cast);
            BlockRange().InsertAfter(src, cast);
        }

        src->SetContained();
    }
    else
    {
        src->SetRegOptional();
    }
}

#ifdef TARGET_64BIT

void Lowering::ContainCheckIntExtend(GenTreeUnOp* node, GenTree* src)
{
    assert(node->OperIs(GT_SXT, GT_UXT) && node->TypeIs(TYP_LONG));

    if (IsMemOperand(src))
    {
        // We can move it right after the source node to avoid the interference check.
        if (node->gtPrev != src)
        {
            BlockRange().Unlink(node);
            BlockRange().InsertAfter(src, node);
        }

        src->SetContained();
    }
    else
    {
        src->SetRegOptional();
    }
}

#endif // TARGET_64BIT

void Lowering::LowerStoreIndRMW(GenTreeIndStore* store)
{
    assert(store->OperIs(GT_IND_STORE) && varTypeIsIntegralOrI(store->GetType()));

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
    // So we'll just move everything that can be moved before store and ensure that any
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
            if (base->OperIs(GT_LCL_LOAD))
            {
                insertBefore = BlockRange().MoveBefore(insertBefore, base);
            }
        }

        if (GenTree* index = addrMode->GetIndex())
        {
            if (index->OperIs(GT_LCL_LOAD))
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
            assert(op->AsOp()->IsCommutative());

            src = op->AsOp()->GetOp(0);
            op->AsOp()->SetOp(0, load);
            op->AsOp()->SetOp(1, src);
        }

        if (!src->IsIntCon())
        {
            src->ClearContained();
        }

        assert(!src->IsRegOptional());

        if (src->OperIs(GT_LCL_LOAD, GT_CNS_INT))
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

void Lowering::ContainCheckFloatBinary(GenTreeOp* node)
{
    assert(node->OperIs(GT_FADD, GT_FSUB, GT_FMUL, GT_FDIV) && varTypeIsFloating(node->GetType()));

    GenTree* op1 = node->GetOp(0);
    GenTree* op2 = node->GetOp(1);

    assert(op1->GetType() == op2->GetType());

    if (op2->IsDblConNonPositiveZero() || (IsMemOperand(op2) && IsSafeToMoveMemOperandForward(node, op2)))
    {
        op2->SetContained();
        return;
    }

    if (node->IsCommutative())
    {
        if (op1->IsDblConNonPositiveZero() || (IsMemOperand(op1) && IsSafeToMoveMemOperandForward(node, op1)))
        {
            node->SetOp(0, op2);
            node->SetOp(1, op1);
            op1->SetContained();
            return;
        }

        GenTree* regOptionalOp = GetPreferredRegOptionalOperand(op1, op2);

        if (regOptionalOp == op1)
        {
            node->SetOp(0, op2);
            node->SetOp(1, op1);
        }

        regOptionalOp->SetRegOptional();
        return;
    }

    op2->SetRegOptional();
}

void Lowering::ContainCheckBinary(GenTreeOp* node)
{
    assert(node->OperIsBinary() && varTypeIsIntegralOrI(node->GetType()));

    GenTree* op1 = node->GetOp(0);
    GenTree* op2 = node->GetOp(1);

    if (IsImmOperand(op2, node))
    {
        op2->SetContained();
        return;
    }

    const unsigned size  = varTypeSize(node->GetType());
    const unsigned size1 = varTypeSize(op1->GetType());
    const unsigned size2 = varTypeSize(op2->GetType());

    if ((size2 == size) && IsMemOperand(op2) && IsSafeToMoveMemOperandForward(node, op2))
    {
        op2->SetContained();
    }
    else if (node->IsCommutative() && (size1 == size))
    {
        if (IsMemOperand(op1) && IsSafeToMoveMemOperandForward(node, op1))
        {
            op1->SetContained();
        }
        else if (size2 != size)
        {
            op1->SetRegOptional();
        }
        else
        {
            GetPreferredRegOptionalOperand(op1, op2)->SetRegOptional();
        }
    }
    else if (size2 == size)
    {
        op2->SetRegOptional();
    }
}

void Lowering::ContainCheckMul(GenTreeOp* node)
{
    assert(node->OperIs(GT_MUL, GT_OVF_SMUL, GT_OVF_UMUL, GT_SMULH, GT_UMULH X86_ARG(GT_SMULL) X86_ARG(GT_UMULL)));
    assert(varTypeIsIntOrI(node->GetType()) X86_ONLY(|| (node->OperIs(GT_SMULL, GT_UMULL) && node->TypeIs(TYP_LONG))));

    var_types      type  = node->GetType();
    GenTree*       op1   = node->GetOp(0);
    GenTree*       op2   = node->GetOp(1);
    GenTree*       memOp = nullptr;
    GenTreeIntCon* immOp = nullptr;

    if (node->OperIs(GT_MUL, GT_OVF_SMUL) && IsImmOperand(op2, node))
    {
        immOp = op2->AsIntCon();
        immOp->SetContained();

        if (node->OperIs(GT_MUL) && (immOp->GetValue() == 3 || immOp->GetValue() == 5 || immOp->GetValue() == 9))
        {
            // We use LEA so the other op has to be in a register.
            return;
        }

        if (op1->GetType() == type)
        {
            if (IsMemOperand(op1) && IsSafeToMoveMemOperandForward(node, op1))
            {
                op1->SetContained();
            }
            else
            {
                op1->SetRegOptional();
            }
        }

        return;
    }

    if ((op2->GetType() == type) && IsMemOperand(op2))
    {
        memOp = op2;
    }
    else if ((op1->GetType() == type) && IsMemOperand(op1))
    {
        memOp = op1;
    }

    if ((memOp != nullptr) && IsSafeToMoveMemOperandForward(node, memOp))
    {
        memOp->SetContained();

        return;
    }

    GenTree* regOptionalOp = nullptr;

    if ((op1->GetType() == type) && (op2->GetType() == type))
    {
        regOptionalOp = GetPreferredRegOptionalOperand(op1, op2);
    }
    else if (op2->GetType() == type)
    {
        regOptionalOp = op2;
    }
    else if (op1->GetType() == type)
    {
        regOptionalOp = op1;
    }

    if (regOptionalOp != nullptr)
    {
        regOptionalOp->SetRegOptional();
    }
}

void Lowering::ContainCheckCompare(GenTreeOp* cmp)
{
    assert(cmp->OperIsCompare() || cmp->OperIs(GT_CMP));

    GenTree*  op1   = cmp->GetOp(0);
    GenTree*  op2   = cmp->GetOp(1);
    var_types type1 = op1->GetType();
    var_types type2 = op2->GetType();

    if (varTypeIsFloating(type1))
    {
        assert(type1 == type2);

        op2 = GenCondition::FromFloatRelop(cmp).PreferSwap() ? op1 : op2;

        if (op2->IsDblConNonPositiveZero())
        {
            op2->SetContained();
        }
        else if (IsMemOperand(op2) && IsSafeToMoveMemOperandForward(cmp, op2))
        {
            op2->SetContained();
        }
        else
        {
            op2->SetRegOptional();
        }

        return;
    }

    if (ContainImmOperand(cmp, op2))
    {
        if (type1 == type2)
        {
            if (IsMemOperand(op1) && IsSafeToMoveMemOperandForward(cmp, op1))
            {
                op1->SetContained();
            }
            else
            {
                op1->SetRegOptional();
            }
        }

        return;
    }

    // Small int memory operands can only be contained if we can generate a 8/16 bit
    // compare instruction, which is only possible if both operands have the same
    // small int type.

    bool canContainOp1 = !varTypeIsSmall(type1) || (type1 == type2);
    bool canContainOp2 = !varTypeIsSmall(type2) || (type1 == type2);

    // Note that TEST does not have a r,rm encoding like CMP has but we can still
    // contain the second operand because the emitter maps both r,rm and rm,r to
    // the same instruction code. This avoids the need to special case TEST here.

    if (canContainOp2 && IsMemOperand(op2) && IsSafeToMoveMemOperandForward(cmp, op2))
    {
        op2->SetContained();
        return;
    }

    if (canContainOp1 && IsMemOperand(op1) && IsSafeToMoveMemOperandForward(cmp, op1))
    {
        op1->SetContained();
        return;
    }

    GenTree* regOptionalCandidate = op1->IsIntCon() ? op2 : GetPreferredRegOptionalOperand(op1, op2);

    if (regOptionalCandidate == op1 ? canContainOp1 : canContainOp2)
    {
        regOptionalCandidate->SetRegOptional();
    }
}

void Lowering::ContainCheckBoundsChk(GenTreeBoundsChk* node)
{
    GenTree* index  = node->GetIndex();
    GenTree* length = node->GetLength();
    GenTree* other;

    if (ContainImmOperand(node, index))
    {
        other = length;
    }
    else if (ContainImmOperand(node, length))
    {
        other = index;
    }
    else if (IsMemOperand(index))
    {
        other = index;
    }
    else
    {
        other = length;
    }

    if (index->GetType() == length->GetType())
    {
        if (IsMemOperand(other))
        {
            other->SetContained();
        }
        else
        {
            other->SetRegOptional();
        }
    }
}

void Lowering::ContainCheckIntrinsic(GenTreeIntrinsic* node)
{
    switch (node->GetIntrinsic())
    {
        case NI_System_Math_Ceiling:
        case NI_System_Math_Floor:
        case NI_System_Math_Round:
        case NI_System_Math_Sqrt:
        {
            GenTree* op1 = node->GetOp(0);

            if (IsMemOperand(op1) || op1->IsDblConNonPositiveZero())
            {
                op1->SetContained();
            }
            else
            {
                op1->SetRegOptional();
            }
        }
        break;

        default:
            break;
    }
}

#ifdef FEATURE_HW_INTRINSICS

bool Lowering::IsContainableHWIntrinsicOp(Compiler*           comp,
                                          GenTreeHWIntrinsic* instr,
                                          GenTree*            op,
                                          bool*               supportsRegOptional)
{
    NamedIntrinsic      intrinsic = instr->GetIntrinsic();
    HWIntrinsicCategory category  = HWIntrinsicInfo::GetCategory(intrinsic);

    assert(HWIntrinsicInfo::SupportsContainment(intrinsic));

    // instr supports nodes that read from an aligned memory address
    //
    // This will generally be an explicit LoadAligned instruction and is false for
    // machines with VEX support when minOpts is enabled. This is because there is
    // currently no way to guarantee that the address read from will always be
    // aligned and we want to assert that the address is aligned when optimizations
    // aren't enabled. However, when optimizations are enabled, we want to allow
    // folding of memory operands as it produces better codegen and allows simpler
    // coding patterns on the managed side.
    bool supportsAlignedVecLoads = false;

    // instr supports nodes that read from general memory
    //
    // We currently have to assume all "general" loads are unaligned. As such, this is
    // generally used to determine if we can mark the node as `regOptional` in the case
    // where `node` is not containable. However, this can also be used to determine whether
    // we can mark other types of reads as contained (such as when directly reading a local).
    bool supportsGeneralLoads = false;

    // instr supports nodes that read from a scalar memory address
    //
    // This will generally be an explicit LoadScalar instruction but is also used to determine
    // whether we can read an address of type T (we don't support this when the load would
    // read more than sizeof(T) bytes).
    bool supportsScalarVecLoads = false;

    // instr supports nodes that read from an unaligned memory address
    //
    // This will generally be an explicit Load instruction and is generally false for machines
    // without VEX support. This is because older hardware required that the SIMD operand always
    // be aligned to the 'natural alignment' of the type.
    bool supportsUnalignedVecLoads = false;

    switch (category)
    {
        case HW_Category_SimpleSIMD:
        case HW_Category_IMM:
        case HW_Category_SIMDScalar:
        case HW_Category_Scalar:
            switch (intrinsic)
            {
                case NI_SSE41_ConvertToVector128Int16:
                case NI_SSE41_ConvertToVector128Int32:
                case NI_SSE41_ConvertToVector128Int64:
                case NI_AVX2_ConvertToVector256Int16:
                case NI_AVX2_ConvertToVector256Int32:
                case NI_AVX2_ConvertToVector256Int64:
                    supportsGeneralLoads = !op->IsHWIntrinsic();
                    break;

                case NI_SSE2_ConvertToVector128Double:
                    assert(op->TypeIs(TYP_SIMD16));

                    // ConvertToVector128Double has Vector128 operands but the memory versions of
                    // CVTDQ2PD and CVTPS2PD have 64 bit operands and don't care about alignment.

                    supportsAlignedVecLoads   = !comp->opts.MinOpts();
                    supportsUnalignedVecLoads = true;
                    supportsGeneralLoads      = true;
                    break;

                case NI_AVX_CompareScalar:
                    assert(op->TypeIs(TYP_SIMD16));

                    // CompareScalar has Vector128 operands but the memory versions of CMPSS
                    // and CMPSD have 32/64 bit operands and don't care about alignment.

                    supportsAlignedVecLoads   = !comp->opts.MinOpts();
                    supportsUnalignedVecLoads = true;
                    supportsScalarVecLoads    = true;
                    supportsGeneralLoads      = true;
                    break;

                case NI_SSE2_Insert:
                case NI_SSE41_Insert:
                case NI_SSE41_X64_Insert:
                    assert(instr->GetOp(1) == op);
                    // insertps has its own special handling
                    assert(instr->GetSimdBaseType() != TYP_FLOAT);
                    assert(varTypeIsIntegral(op->GetType()));

                    supportsGeneralLoads = (varTypeSize(op->GetType()) >= varTypeSize(instr->GetSimdBaseType()));
                    break;

                case NI_Vector128_CreateScalarUnsafe:
                case NI_Vector256_CreateScalarUnsafe:
                    supportsGeneralLoads =
                        (varTypeSize(op->GetType()) == varTypeSize(varActualType(instr->GetSimdBaseType())));
                    break;

                case NI_AVX2_BroadcastScalarToVector128:
                case NI_AVX2_BroadcastScalarToVector256:
                    // The memory form of this already takes a pointer, and cannot be further contained.
                    // The containable form is the one that takes a SIMD value, that may be in memory.
                    supportsGeneralLoads = op->TypeIs(TYP_SIMD16);
                    break;

                case NI_SSE_ConvertScalarToVector128Single:
                case NI_SSE2_ConvertScalarToVector128Double:
                case NI_SSE2_ConvertScalarToVector128Int32:
                case NI_SSE_X64_ConvertScalarToVector128Single:
                case NI_SSE2_X64_ConvertScalarToVector128Double:
                case NI_SSE2_X64_ConvertScalarToVector128Int64:
                    if (!varTypeIsIntegral(op->GetType()))
                    {
                        // The floating-point overload doesn't require any special semantics
                        assert(intrinsic == NI_SSE2_ConvertScalarToVector128Double);

                        supportsScalarVecLoads = true;
                        supportsGeneralLoads   = true;
                    }
                    else
                    {
                        supportsGeneralLoads =
                            (varTypeSize(op->GetType()) == varTypeSize(varActualType(instr->GetSimdBaseType())));
                    }
                    break;

                case NI_SSE42_CRC32B:
                    supportsGeneralLoads = true;
                    break;

                case NI_SSE42_CRC32W:
                    supportsGeneralLoads = varTypeSize(op->GetType()) >= varTypeSize(TYP_SHORT);
                    break;

                default:
                    if (category == HW_Category_Scalar)
                    {
                        assert(varTypeIsIntegral(op->GetType()));

                        supportsGeneralLoads = varTypeSize(op->GetType()) >= varTypeSize(instr->GetType());
                        break;
                    }

                    if (category == HW_Category_SIMDScalar)
                    {
                        if (op->TypeIs(TYP_SIMD16, TYP_SIMD32))
                        {
                            supportsScalarVecLoads = true;
                            supportsGeneralLoads   = true;
                        }
                        break;
                    }

                    if (category == HW_Category_IMM)
                    {
                        break;
                    }
                    FALLTHROUGH;
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
                case NI_AVX_Shuffle:
                case NI_AVX2_AlignRight:
                case NI_AVX2_Blend:
                case NI_AVX2_InsertVector128:
                case NI_AVX2_MultipleSumAbsoluteDifferences:
                case NI_AVX2_Permute2x128:
                case NI_AVX2_Permute4x64:
                case NI_AVX2_ShiftLeftLogical:
                case NI_AVX2_ShiftRightArithmetic:
                case NI_AVX2_ShiftRightLogical:
                case NI_AVX2_Shuffle:
                case NI_AVX2_ShuffleHigh:
                case NI_AVX2_ShuffleLow:
                    if (!op->TypeIs(TYP_SIMD16, TYP_SIMD32))
                    {
                        *supportsRegOptional = false;
                        return false;
                    }

                    supportsUnalignedVecLoads = comp->codeGen->UseVexEncoding();
                    supportsAlignedVecLoads   = !supportsUnalignedVecLoads || !comp->opts.MinOpts();
                    supportsGeneralLoads      = supportsUnalignedVecLoads;
                    break;
            }
            break;

        default:
            unreached();
    }

    *supportsRegOptional = supportsGeneralLoads;

    if (!op->IsHWIntrinsic())
    {
        return supportsGeneralLoads && IsMemOperand(op);
    }

    // TODO-XArch: Update this to be table driven, if possible.

    switch (op->AsHWIntrinsic()->GetIntrinsic())
    {
        case NI_SSE_LoadAlignedVector128:
        case NI_SSE2_LoadAlignedVector128:
        case NI_AVX_LoadAlignedVector256:
            return supportsAlignedVecLoads;

        case NI_SSE_LoadScalarVector128:
        case NI_SSE2_LoadScalarVector128:
            return supportsScalarVecLoads;

        case NI_SSE_LoadVector128:
        case NI_SSE2_LoadVector128:
        case NI_AVX_LoadVector256:
            return supportsUnalignedVecLoads;

        default:
            return false;
    }
}

void Lowering::ContainHWIntrinsicOperand(GenTreeHWIntrinsic* node, GenTree* op)
{
    var_types intrinsicLoadType = TYP_UNDEF;
    GenTree*  intrinsicLoadAddr = nullptr;

    if (GenTreeHWIntrinsic* hwi = op->IsHWIntrinsic())
    {
        switch (hwi->GetIntrinsic())
        {
            case NI_SSE_LoadScalarVector128:
                assert(hwi->GetSimdBaseType() == TYP_FLOAT);
                intrinsicLoadType = TYP_FLOAT;
                intrinsicLoadAddr = hwi->GetOp(0);
                break;
            case NI_SSE2_LoadScalarVector128:
                // TODO-MIKE-Review: This likely needs only DOUBLE.
                intrinsicLoadType = hwi->GetSimdBaseType();
                intrinsicLoadAddr = hwi->GetOp(0);
                break;
            case NI_SSE_LoadAlignedVector128:
            case NI_SSE2_LoadAlignedVector128:
            case NI_AVX_LoadAlignedVector256:
            case NI_SSE_LoadVector128:
            case NI_SSE2_LoadVector128:
            case NI_AVX_LoadVector256:
                assert(hwi->TypeIs(TYP_SIMD16, TYP_SIMD32));
                intrinsicLoadType = hwi->GetType();
                intrinsicLoadAddr = hwi->GetOp(0);
                break;
            default:
                break;
        }
    }

    if (intrinsicLoadType != TYP_UNDEF)
    {
        op->ChangeOper(GT_IND_LOAD);
        op->SetType(intrinsicLoadType);
        op->AsIndLoad()->SetAddr(intrinsicLoadAddr);
    }

    op->SetContained();
}

void Lowering::ContainCheckHWIntrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic      intrinsic = node->GetIntrinsic();
    HWIntrinsicCategory category  = HWIntrinsicInfo::GetCategory(intrinsic);
    var_types           baseType  = node->GetSimdBaseType();

    if (!HWIntrinsicInfo::SupportsContainment(intrinsic))
    {
        if (HWIntrinsicInfo::IsAvx2GatherIntrinsic(intrinsic))
        {
            node->GetLastOp()->SetContained();
        }

        return;
    }

    if ((category != HW_Category_Scalar) && (node->GetSimdSize() < 16))
    {
        // Ignore anything having a non-target vector size, such
        // intrinsic nodes should not appear but just in case...
        return;
    }

    if (category == HW_Category_IMM)
    {
        if ((intrinsic == NI_SSE41_Insert) && (baseType == TYP_FLOAT))
        {
            ContainHWIntrinsicInsertFloat(node);
            return;
        }

        GenTree* lastOp = node->GetLastOp();
        assert(lastOp != nullptr);

        if (HWIntrinsicInfo::IsImmOp(intrinsic, lastOp) && lastOp->IsIntCon())
        {
            lastOp->SetContained();
        }
    }

    // TODO-XArch-CQ: Non-VEX encoded instructions can have both ops contained

    const bool     isCommutative = HWIntrinsicInfo::IsCommutative(intrinsic);
    const unsigned numArgs       = node->GetNumOps();

    if (numArgs == 1)
    {
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
                switch (intrinsic)
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
                        // These intrinsics have both 1 and 2-operand overloads.
                        // The 1-operand overload basically does `intrinsic(op1, op1)`
                        // Because of this, the operand must be loaded into a register
                        // and cannot be contained.
                        return;

                    case NI_SSE2_ConvertToInt32:
                    case NI_SSE2_X64_ConvertToInt64:
                        if (varTypeIsIntegral(baseType))
                        {
                            // TODO-XARCH-CQ: These intrinsics are "ins reg/mem, xmm" and don't
                            // currently support containment.
                            return;
                        }
                        break;

                    case NI_SSE41_ConvertToVector128Int16:
                    case NI_SSE41_ConvertToVector128Int32:
                    case NI_SSE41_ConvertToVector128Int64:
                    case NI_AVX2_ConvertToVector256Int16:
                    case NI_AVX2_ConvertToVector256Int32:
                    case NI_AVX2_ConvertToVector256Int64:
                        if (!varTypeIsSIMD(node->GetOp(0)->GetType()))
                        {
                            ContainCheckHWIntrinsicAddr(node, node->GetOp(0));
                            return;
                        }
                        break;

                    default:
                        break;
                }

                bool supportsRegOptional = false;

                if (IsContainableHWIntrinsicOp(node, node->GetOp(0), &supportsRegOptional))
                {
                    ContainHWIntrinsicOperand(node, node->GetOp(0));
                }
                else if (supportsRegOptional)
                {
                    node->GetOp(0)->SetRegOptional();
                }
                break;
            }

            default:
                unreached();
        }
    }
    else if (numArgs == 2)
    {
        GenTree* op1 = node->GetOp(0);
        GenTree* op2 = node->GetOp(1);

        switch (category)
        {
            case HW_Category_MemoryLoad:
                if ((intrinsic == NI_AVX_MaskLoad) || (intrinsic == NI_AVX2_MaskLoad))
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

                if (((intrinsic == NI_SSE_Store) || (intrinsic == NI_SSE2_Store)) && op2->IsHWIntrinsic() &&
                    ((op2->AsHWIntrinsic()->GetIntrinsic() == NI_AVX_ExtractVector128) ||
                     (op2->AsHWIntrinsic()->GetIntrinsic() == NI_AVX2_ExtractVector128)) &&
                    op2->AsHWIntrinsic()->GetOp(1)->IsIntCon())
                {
                    ContainHWIntrinsicOperand(node, op2);
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
                    ContainHWIntrinsicOperand(node, op2);
                }
                else if ((isCommutative || (intrinsic == NI_BMI2_MultiplyNoFlags) ||
                          (intrinsic == NI_BMI2_X64_MultiplyNoFlags)) &&
                         IsContainableHWIntrinsicOp(node, op1, &op1SupportsRegOptional))
                {
                    ContainHWIntrinsicOperand(node, op1);

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

                switch (intrinsic)
                {
                    case NI_SSE2_Extract:
                    case NI_AVX_ExtractVector128:
                    case NI_AVX2_ExtractVector128:
                        // TODO-XARCH-CQ: These intrinsics are "ins reg/mem, xmm, imm8" and don't
                        // currently support containment.
                        break;

                    case NI_SSE2_ShiftLeftLogical:
                    case NI_SSE2_ShiftRightArithmetic:
                    case NI_SSE2_ShiftRightLogical:
                    case NI_AVX2_ShiftLeftLogical:
                    case NI_AVX2_ShiftRightArithmetic:
                    case NI_AVX2_ShiftRightLogical:
                        // These intrinsics can have op2 be immValue or reg/mem
                        if (!HWIntrinsicInfo::IsImmOp(intrinsic, op2))
                        {
                            if (IsContainableHWIntrinsicOp(node, op2, &supportsRegOptional))
                            {
                                ContainHWIntrinsicOperand(node, op2);
                            }
                            else if (supportsRegOptional)
                            {
                                op2->SetRegOptional();
                            }
                        }
                        break;

                    case NI_AVX2_Shuffle:
                        if (varTypeIsByte(node->GetSimdBaseType()))
                        {
                            if (IsContainableHWIntrinsicOp(node, op2, &supportsRegOptional))
                            {
                                ContainHWIntrinsicOperand(node, op2);
                            }
                            else if (supportsRegOptional)
                            {
                                op2->SetRegOptional();
                            }
                            break;
                        }
                        FALLTHROUGH;
                    case NI_SSE2_Shuffle:
                    case NI_SSE2_ShuffleHigh:
                    case NI_SSE2_ShuffleLow:
                    case NI_AVX2_Permute4x64:
                    case NI_AVX2_ShuffleHigh:
                    case NI_AVX2_ShuffleLow:
                        // These intrinsics have op2 as an immValue and op1 as a reg/mem
                        if (IsContainableHWIntrinsicOp(node, op1, &supportsRegOptional))
                        {
                            ContainHWIntrinsicOperand(node, op1);
                        }
                        else if (supportsRegOptional)
                        {
                            op1->SetRegOptional();
                        }
                        break;

                    case NI_SSE41_Extract:
                    case NI_SSE41_X64_Extract:
                        assert(!varTypeIsFloating(baseType));
                        // TODO-XARCH-CQ: These intrinsics are "ins reg/mem, xmm, imm8" and don't
                        // currently support containment.
                        break;

                    case NI_AVX_Permute:
                        // These intrinsics can have op2 be immValue or reg/mem
                        // They also can have op1 be reg/mem and op2 be immValue
                        if (HWIntrinsicInfo::IsImmOp(intrinsic, op2))
                        {
                            if (IsContainableHWIntrinsicOp(node, op1, &supportsRegOptional))
                            {
                                ContainHWIntrinsicOperand(node, op1);
                            }
                            else if (supportsRegOptional)
                            {
                                op1->SetRegOptional();
                            }
                        }
                        else if (IsContainableHWIntrinsicOp(node, op2, &supportsRegOptional))
                        {
                            ContainHWIntrinsicOperand(node, op2);
                        }
                        else if (supportsRegOptional)
                        {
                            op2->SetRegOptional();
                        }
                        break;

                    case NI_AES_KeygenAssist:
                        if (IsContainableHWIntrinsicOp(node, op1, &supportsRegOptional))
                        {
                            ContainHWIntrinsicOperand(node, op1);
                        }
                        else if (supportsRegOptional)
                        {
                            op1->SetRegOptional();
                        }
                        break;

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

                        if (HWIntrinsicInfo::IsImmOp(intrinsic, lastOp) && lastOp->IsIntCon())
                        {
                            assert(lastOp->isContained());
                        }
#endif
                        break;
                    }

                    default:
                        assert(!"Unhandled containment for binary hardware intrinsic with immediate indir1");
                        break;
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
        }
    }
    else if (numArgs == 3)
    {
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
                if ((intrinsic >= NI_FMA_MultiplyAdd) && (intrinsic <= NI_FMA_MultiplySubtractNegatedScalar))
                {
                    bool supportsRegOptional = false;

                    if (IsContainableHWIntrinsicOp(node, op3, &supportsRegOptional))
                    {
                        // 213 form: op1 = (op2 * op1) + [op3]
                        ContainHWIntrinsicOperand(node, op3);
                    }
                    else if (IsContainableHWIntrinsicOp(node, op2, &supportsRegOptional))
                    {
                        // 132 form: op1 = (op1 * op3) + [op2]
                        ContainHWIntrinsicOperand(node, op2);
                    }
                    else if (IsContainableHWIntrinsicOp(node, op1, &supportsRegOptional))
                    {
                        // Intrinsics with CopyUpperBits semantics cannot have op1 be contained

                        if (!HWIntrinsicInfo::CopiesUpperBits(intrinsic))
                        {
                            // 231 form: op3 = (op2 * op3) + [op1]
                            ContainHWIntrinsicOperand(node, op1);
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

                    switch (intrinsic)
                    {
                        case NI_SSE41_BlendVariable:
                        case NI_AVX_BlendVariable:
                        case NI_AVX2_BlendVariable:
                            if (IsContainableHWIntrinsicOp(node, op2, &supportsRegOptional))
                            {
                                ContainHWIntrinsicOperand(node, op2);
                            }
                            else if (supportsRegOptional)
                            {
                                op2->SetRegOptional();
                            }
                            break;

                        case NI_AVXVNNI_MultiplyWideningAndAdd:
                        case NI_AVXVNNI_MultiplyWideningAndAddSaturate:
                            if (IsContainableHWIntrinsicOp(node, op3, &supportsRegOptional))
                            {
                                ContainHWIntrinsicOperand(node, op3);
                            }
                            else if (supportsRegOptional)
                            {
                                op3->SetRegOptional();
                            }
                            break;

                        case NI_BMI2_MultiplyNoFlags:
                        case NI_BMI2_X64_MultiplyNoFlags:
                            if (IsContainableHWIntrinsicOp(node, op2, &supportsRegOptional))
                            {
                                ContainHWIntrinsicOperand(node, op2);
                            }
                            else if (IsContainableHWIntrinsicOp(node, op1, &supportsRegOptional))
                            {
                                ContainHWIntrinsicOperand(node, op1);
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

                        default:
                            unreached();
                    }
                }
                break;

            case HW_Category_IMM:
                switch (intrinsic)
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
                        bool supportsRegOptional = false;

                        if (IsContainableHWIntrinsicOp(node, op2, &supportsRegOptional))
                        {
                            ContainHWIntrinsicOperand(node, op2);
                        }
                        else if (supportsRegOptional)
                        {
                            op2->SetRegOptional();
                        }
                        break;
                    }

                    default:
                        assert(!"Unhandled containment for ternary hardware intrinsic with immediate indir1");
                        break;
                }
                break;

            default:
                unreached();
        }
    }
    else
    {
        unreached();
    }
}
#endif // FEATURE_HW_INTRINSICS

void Lowering::ContainCheckXAdd(GenTreeOp* node)
{
    if (node->IsUnusedValue())
    {
        // Make sure the types are identical, since the node type is changed to VOID
        // CodeGen relies on op2's type to determine the instruction size.
        // Note that the node type cannot be a small int but the data operand can.
        assert(varActualType(node->GetOp(1)->GetType()) == node->GetType());

        node->ClearUnusedValue();
        node->SetOper(GT_LOCKADD);
        node->SetType(TYP_VOID);

        ContainImmOperand(node, node->GetOp(1));
    }
}

#endif // TARGET_XARCH
