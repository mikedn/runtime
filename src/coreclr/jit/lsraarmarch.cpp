// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_ARMARCH

#include "sideeffects.h"
#include "lower.h"
#include "lsra.h"
#include "emit.h"

void LinearScan::BuildIndir(GenTreeIndir* indir)
{
    assert(!indir->TypeIs(TYP_STRUCT));

    GenTree* addr = indir->GetAddr();

#ifdef TARGET_ARM
    if (indir->IsUnaligned() && varTypeIsFloating(indir->GetType()))
    {
        BuildInternalIntDef(indir);

        if (indir->TypeIs(TYP_DOUBLE))
        {
            BuildInternalIntDef(indir);
        }
    }
#endif

    if (addr->isContained())
    {
        if (GenTreeAddrMode* lea = addr->IsAddrMode())
        {
            if (((lea->GetIndex() != nullptr) && (lea->GetOffset() != 0)) ||
                !emitter::emitIns_valid_imm_for_ldst_offset(lea->GetOffset(), emitTypeSize(indir->GetType())))
            {
                BuildInternalIntDef(indir);
            }
        }
#ifdef TARGET_ARM64
        else if (addr->OperIs(GT_CONST_ADDR))
        {
            BuildInternalIntDef(indir);
        }
#endif
    }

#ifdef FEATURE_SIMD
    if (indir->TypeIs(TYP_SIMD12))
    {
        BuildInternalIntDef(indir);

        if (GenTreeStoreInd* store = indir->IsStoreInd())
        {
            GenTree* value = store->GetValue();

            if (value->isContained())
            {
                BuildAddrUses(store->GetAddr());

                if (value->OperIs(GT_IND))
                {
                    BuildAddrUses(value->AsIndir()->GetAddr());
                }

                BuildInternalUses();

                return;
            }
        }
    }
#endif // FEATURE_SIMD

    BuildAddrUses(indir->GetAddr());
    BuildInternalUses();

    if (!indir->OperIs(GT_STOREIND, GT_NULLCHECK))
    {
        BuildDef(indir);
    }
}

void LinearScan::BuildCall(GenTreeCall* call)
{
    GenTree* ctrlExpr = call->IsIndirectCall() ? call->gtCallAddr : call->gtControlExpr;

    if (ctrlExpr != nullptr)
    {
        assert(ctrlExpr->TypeIs(TYP_I_IMPL));

        regMaskTP ctrlExprCandidates = RBM_NONE;

#if FEATURE_FASTTAILCALL
        // In case of fast tail implemented as jmp, make sure that gtControlExpr is
        // computed into a register.
        if (call->IsFastTailCall())
        {
            // Fast tail call - make sure that call target is always computed in R12(ARM32)/IP0(ARM64)
            // so that epilog sequence can generate "br xip0/r12" to achieve fast tail call.
            ctrlExprCandidates = RBM_FASTTAILCALL_TARGET;
        }
#endif
    }
#ifdef FEATURE_READYTORUN_COMPILER
    else if (call->IsR2ROrVirtualStubRelativeIndir())
    {
        BuildInternalIntDef(call);
    }
#endif
#ifdef TARGET_ARM
    else
    {
        BuildInternalIntDef(call);
    }

    if (call->NeedsNullCheck())
    {
        BuildInternalIntDef(call);
    }

#endif // TARGET_ARM

    for (GenTreeCall::Use& arg : call->LateArgs())
    {
        GenTree* argNode = arg.GetNode();

        INDEBUG(CallArgInfo* argInfo = call->GetArgInfoByArgNode(argNode);)

        if (argNode->OperIs(GT_PUTARG_STK))
        {
            assert(argInfo->GetRegCount() == 0);
            assert(!argNode->isContained());

            continue;
        }

        if (argNode->OperIs(GT_FIELD_LIST))
        {
            assert(argNode->isContained());

            unsigned regIndex = 0;
            for (GenTreeFieldList::Use& use : argNode->AsFieldList()->Uses())
            {
                assert(use.GetNode()->GetRegNum() == argInfo->GetRegNum(regIndex));

                BuildUse(use.GetNode(), genRegMask(use.GetNode()->GetRegNum()));
                regIndex++;

#ifdef TARGET_ARM
                if (use.GetNode()->TypeIs(TYP_LONG))
                {
                    BuildUse(use.GetNode(), genRegMask(REG_NEXT(use.GetNode()->GetRegNum())), 1);
                    regIndex++;
                }
#endif
            }

            continue;
        }

#if FEATURE_ARG_SPLIT
        if (argNode->OperIs(GT_PUTARG_SPLIT))
        {
            unsigned regCount = argNode->AsPutArgSplit()->GetRegCount();

            for (unsigned int i = 0; i < regCount; i++)
            {
                assert(argNode->GetRegNum(i) == argInfo->GetRegNum(i));

                BuildUse(argNode, genRegMask(argNode->GetRegNum(i)), i);
            }

            continue;
        }
#endif

        assert(argNode->OperIs(GT_PUTARG_REG));
        assert(argNode->GetRegNum() == argInfo->GetRegNum());

#ifdef TARGET_ARM
        if (argNode->TypeIs(TYP_LONG))
        {
            assert(argNode->IsMultiRegNode());

            BuildUse(argNode, genRegMask(argNode->GetRegNum()), 0);
            BuildUse(argNode, genRegMask(REG_NEXT(argNode->GetRegNum())), 1);

            continue;
        }
#endif

        BuildUse(argNode, genRegMask(argNode->GetRegNum()));
    }

    if (ctrlExpr != nullptr)
    {
        BuildUse(ctrlExpr);
    }

    BuildInternalUses();
    BuildKills(call, getKillSetForCall(call));

#ifdef TARGET_ARM
    if (call->IsHelperCall(compiler, CORINFO_HELP_INIT_PINVOKE_FRAME))
    {
        BuildDef(call, RBM_PINVOKE_TCB);
    }
    else
#endif
        if (call->HasMultiRegRetVal() || varTypeIsStruct(call->GetType()))
    {
        for (unsigned i = 0; i < call->GetRegCount(); i++)
        {
            BuildDef(call, call->GetRegType(i), genRegMask(call->GetRetDesc()->GetRegNum(i)), i);
        }
    }
    else if (varTypeUsesFloatReg(call->GetType()))
    {
        BuildDef(call, RBM_FLOATRET);
    }
    else if (!call->TypeIs(TYP_VOID))
    {
        BuildDef(call, RBM_INTRET);
    }
}

void LinearScan::BuildPutArgStk(GenTreePutArgStk* putArg)
{
    GenTree* src = putArg->GetOp(0);

    if (src->OperIs(GT_FIELD_LIST))
    {
        assert(src->isContained());

        for (GenTreeFieldList::Use& use : src->AsFieldList()->Uses())
        {
            if (!use.GetNode()->isContained())
            {
                BuildUse(use.GetNode());

#if defined(FEATURE_SIMD) && defined(OSX_ARM64_ABI)
                if (use.GetType() == TYP_SIMD12)
                {
                    // Vector3 is read/written as two reads/writes: 8 byte and 4 byte.
                    // To assemble the vector properly we would need an additional int register.
                    // The other platforms can write it as 16-byte using 1 write.
                    BuildInternalIntDef(use.GetNode());
                }
#endif // FEATURE_SIMD && OSX_ARM64_ABI
            }
        }

        return;
    }

    if (src->TypeIs(TYP_STRUCT))
    {
        assert(src->isContained());

        // We can use a ldp/stp sequence so we need two internal registers for ARM64; one for ARM.
        BuildInternalIntDef(putArg);
#ifdef TARGET_ARM64
        BuildInternalIntDef(putArg);
#endif

        if (src->OperIs(GT_OBJ))
        {
            BuildAddrUses(src->AsObj()->GetAddr());
        }

        BuildInternalUses();

        return;
    }

    if (!src->isContained())
    {
        BuildUse(src);
    }
}

#if FEATURE_ARG_SPLIT
void LinearScan::BuildPutArgSplit(GenTreePutArgSplit* putArg)
{
    CallArgInfo* argInfo    = putArg->GetArgInfo();
    regMaskTP    argRegMask = RBM_NONE;

    for (unsigned i = 0; i < argInfo->GetRegCount(); i++)
    {
        argRegMask |= genRegMask(argInfo->GetRegNum(i));
    }

    GenTree* src = putArg->GetOp(0);

    if (src->IsIntegralConst(0))
    {
        BuildUse(src);
    }
    else
    {
        assert(src->TypeIs(TYP_STRUCT));
        assert(src->isContained());

        if (src->OperIs(GT_FIELD_LIST))
        {
            unsigned regIndex = 0;
            for (GenTreeFieldList::Use& use : src->AsFieldList()->Uses())
            {
                GenTree*  node    = use.GetNode();
                regMaskTP regMask = RBM_NONE;

                if (regIndex < argInfo->GetRegCount())
                {
                    regMask = genRegMask(argInfo->GetRegNum(regIndex));
                }

                BuildUse(node, regMask);
                regIndex++;

#ifdef TARGET_ARM
                if (node->TypeIs(TYP_LONG))
                {
                    assert(node->OperIs(GT_BITCAST));

                    regMask = genRegMask(argInfo->GetRegNum(regIndex));

                    BuildUse(node, regMask, 1);
                    regIndex++;
                }
#endif
            }
        }
        else
        {
            BuildInternalIntDef(putArg, allRegs(TYP_INT) & ~argRegMask);

            if (src->OperIs(GT_OBJ))
            {
                BuildAddrUses(src->AsObj()->GetAddr());
            }

            BuildInternalUses();
        }
    }

    for (unsigned i = 0; i < argInfo->GetRegCount(); i++)
    {
        BuildDef(putArg, putArg->GetRegType(i), genRegMask(argInfo->GetRegNum(i)), i);
    }
}
#endif // FEATURE_ARG_SPLIT

void LinearScan::BuildStructStore(GenTree* store, StructStoreKind kind, ClassLayout* layout)
{
#ifdef TARGET_ARM64
    if (kind == StructStoreKind::UnrollRegsWB)
    {
        BuildStructStoreUnrollRegsWB(store->AsObj(), layout);

        return;
    }
#endif

    GenTree* dstAddr = nullptr;
    GenTree* src;

    if (store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        src = store->AsLclVarCommon()->GetOp(0);
    }
    else
    {
        dstAddr = store->AsBlk()->GetAddr();
        src     = store->AsBlk()->GetValue();
    }

    GenTree* srcAddrOrFill = nullptr;

    if (kind == StructStoreKind::UnrollRegs)
    {
        assert(src->IsCall());
    }
    else if (src->OperIs(GT_INIT_VAL, GT_CNS_INT))
    {
        if (src->OperIs(GT_INIT_VAL))
        {
            assert(src->isContained());
            src = src->AsUnOp()->GetOp(0);
        }

        srcAddrOrFill = src;
    }
    else if (src->OperIs(GT_IND, GT_OBJ, GT_BLK))
    {
        assert(src->isContained());
        srcAddrOrFill = src->AsIndir()->GetAddr();
    }
    else
    {
        assert(src->OperIs(GT_LCL_VAR, GT_LCL_FLD));
        assert(src->isContained());
    }

    regMaskTP dstAddrRegMask     = RBM_NONE;
    regMaskTP srcRegMask         = RBM_NONE;
    regMaskTP sizeRegMask        = RBM_NONE;
    regMaskTP internalIntRegMask = allRegs(TYP_INT);

    switch (kind)
    {
        case StructStoreKind::UnrollRegs:
        case StructStoreKind::UnrollInit:
            break;

        case StructStoreKind::UnrollCopyWB:
            dstAddrRegMask = RBM_WRITE_BARRIER_DST_BYREF;

            // If we have a source address we want it in REG_WRITE_BARRIER_SRC_BYREF.
            // Otherwise, if it is a local, codegen will put its address in REG_WRITE_BARRIER_SRC_BYREF,
            // which is killed and thus needn't be reserved as an internal register.

            // TODO-MIKE-Review: XARCH lowering does reserve an internal register for a local source.

            if (srcAddrOrFill != nullptr)
            {
                assert(!srcAddrOrFill->isContained());
                srcRegMask = RBM_WRITE_BARRIER_SRC_BYREF;
            }

            internalIntRegMask &= ~(dstAddrRegMask | RBM_WRITE_BARRIER_SRC_BYREF);
            FALLTHROUGH;
        case StructStoreKind::UnrollCopy:
            BuildInternalIntDef(store, internalIntRegMask);
#ifdef TARGET_ARM64
            if (layout->GetSize() >= 2 * REGSIZE_BYTES)
            {
                // Reserve an additional temp register for LDP/STP.
                BuildInternalIntDef(store, internalIntRegMask);
            }
#endif
            break;

        case StructStoreKind::MemSet:
            assert(!src->isContained());
            dstAddrRegMask = RBM_ARG_0;
            srcRegMask     = RBM_ARG_1;
            sizeRegMask    = RBM_ARG_2;
            break;

        case StructStoreKind::MemCpy:
            dstAddrRegMask = RBM_ARG_0;
            srcRegMask     = RBM_ARG_1;
            sizeRegMask    = RBM_ARG_2;
            break;

        default:
            unreached();
    }

    // TODO-MIKE-Review: Should temp registers be reserved for src/dest like on XARCH?
    // They're not needed for correctness due to the kill set but on XARCH they avoid
    // poor register allocation by imposing constraints that that the kill set doesn't.
    // On the other hand, ARM doesn't have UnrollCopyWBRepMovs and large struct copies
    // that use helper calls and thus have register constraints are relatively rare.

    if (sizeRegMask != RBM_NONE)
    {
        // Reserve a temp register for the block size argument.
        BuildInternalIntDef(store, sizeRegMask);
    }

    if (dstAddr != nullptr)
    {
        if (!dstAddr->isContained())
        {
            BuildUse(dstAddr, dstAddrRegMask);
        }
        else if (dstAddr->IsAddrMode())
        {
            BuildAddrUses(dstAddr->AsAddrMode()->GetBase());
        }
    }

    if (kind == StructStoreKind::UnrollRegs)
    {
        for (unsigned i = 0, count = src->AsCall()->GetRegCount(); i < count; i++)
        {
            BuildUse(src, RBM_NONE, i);
        }
    }
    else if (srcAddrOrFill != nullptr)
    {
        if (!srcAddrOrFill->isContained())
        {
            BuildUse(srcAddrOrFill, srcRegMask);
        }
        else if (srcAddrOrFill->IsAddrMode())
        {
            BuildAddrUses(srcAddrOrFill->AsAddrMode()->GetBase());
        }
    }

    BuildInternalUses();
    BuildKills(store, getKillSetForStructStore(kind));
}

void LinearScan::BuildStructStoreUnrollRegsWB(GenTreeObj* store, ClassLayout* layout)
{
#ifndef TARGET_ARM64
    unreached();
#else
    assert(layout == store->GetLayout());
    assert(layout->GetSlotCount() == 2);

    GenTree*     addr  = store->GetAddr();
    GenTreeCall* value = store->GetValue()->AsCall();

    assert(value->GetRegCount() == 2);

    if (!addr->isContained())
    {
        BuildUse(addr);
    }
    else if (GenTreeAddrMode* am = addr->IsAddrMode())
    {
        BuildUse(am->GetBase());
        assert(am->GetIndex() == nullptr);
    }

    BuildUse(value, RBM_NONE, 0);
    BuildUse(value, RBM_NONE, 1);
    BuildInternalUses();
    BuildKills(store, compiler->compHelperCallKillSet(CORINFO_HELP_CHECKED_ASSIGN_REF));
#endif
}

void LinearScan::BuildBoundsChk(GenTreeBoundsChk* node)
{
    if (!node->GetOp(0)->IsContainedIntCon())
    {
        BuildUse(node->GetOp(0));
    }

    if (!node->GetOp(1)->IsContainedIntCon())
    {
        BuildUse(node->GetOp(1));
    }
}

void LinearScan::BuildCast(GenTreeCast* cast)
{
    GenTree* src = cast->GetOp(0);

#ifdef TARGET_ARM
    var_types srcType = varActualType(src->GetType());
    var_types dstType = cast->GetType();

    assert(!varTypeIsLong(srcType) || (src->OperIs(GT_LONG) && src->isContained()));

    // Floating point to integer casts requires a temporary register.
    if (varTypeIsFloating(srcType) && !varTypeIsFloating(dstType))
    {
        BuildInternalFloatDef(cast, RBM_ALLFLOAT);
        setInternalRegsDelayFree = true;
    }
#endif

    if (!src->isContained())
    {
        BuildUse(src);
    }
    else if (src->OperIs(GT_IND))
    {
        BuildAddrUses(src->AsIndir()->GetAddr());
    }
#ifdef TARGET_ARM
    else if (src->OperIs(GT_LONG))
    {
        BuildUse(src->AsOp()->GetOp(0));
        BuildUse(src->AsOp()->GetOp(1));
    }
#endif
    else
    {
        assert(src->OperIs(GT_LCL_VAR, GT_LCL_FLD));
    }

    BuildInternalUses();
    BuildDef(cast);
}

void LinearScan::BuildCmp(GenTreeOp* cmp)
{
    assert(cmp->OperIsCompare() || cmp->OperIs(GT_CMP) ARM64_ONLY(|| cmp->OperIs(GT_JCMP)));

    BuildUse(cmp->GetOp(0));

    if (!cmp->GetOp(1)->IsContainedIntCon())
    {
        BuildUse(cmp->GetOp(1));
    }

    if (!cmp->TypeIs(TYP_VOID))
    {
        BuildDef(cmp);
    }
}

#endif // TARGET_ARMARCH
