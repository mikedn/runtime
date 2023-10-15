// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX              Register Requirements for ARM and ARM64 common code          XX
XX                                                                           XX
XX  This encapsulates common logic for setting register requirements for     XX
XX  the ARM and ARM64 architectures.                                         XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"

#ifdef TARGET_ARMARCH

#include "sideeffects.h"
#include "lower.h"
#include "lsra.h"

//------------------------------------------------------------------------
// BuildIndir: Specify register requirements for address expression
//                       of an indirection operation.
//
// Arguments:
//    indirTree - GT_IND, GT_STOREIND or block gentree node
//
// Return Value:
//    The number of sources consumed by this node.
//
int LinearScan::BuildIndir(GenTreeIndir* indirTree)
{
    // struct typed indirs are expected only on rhs of a block copy,
    // but in this case they must be contained.
    assert(indirTree->TypeGet() != TYP_STRUCT);

    GenTree* addr  = indirTree->Addr();
    GenTree* index = nullptr;
    int      cns   = 0;

#ifdef TARGET_ARM
    // Unaligned loads/stores for floating point values must first be loaded into integer register(s)
    if (indirTree->gtFlags & GTF_IND_UNALIGNED)
    {
        var_types type = TYP_UNDEF;
        if (indirTree->OperGet() == GT_STOREIND)
        {
            type = indirTree->AsStoreInd()->GetValue()->TypeGet();
        }
        else if (indirTree->OperGet() == GT_IND)
        {
            type = indirTree->TypeGet();
        }

        if (type == TYP_FLOAT)
        {
            buildInternalIntRegisterDefForNode(indirTree);
        }
        else if (type == TYP_DOUBLE)
        {
            buildInternalIntRegisterDefForNode(indirTree);
            buildInternalIntRegisterDefForNode(indirTree);
        }
    }
#endif

    if (addr->isContained())
    {
        if (GenTreeAddrMode* lea = addr->IsAddrMode())
        {
            index = lea->GetIndex();
            cns   = lea->GetOffset();

            // On ARM we may need a single internal register
            // (when both conditions are true then we still only need a single internal register)
            if ((index != nullptr) && (cns != 0))
            {
                // ARM does not support both Index and offset so we need an internal register
                buildInternalIntRegisterDefForNode(indirTree);
            }
            else if (!emitter::emitIns_valid_imm_for_ldst_offset(cns, emitTypeSize(indirTree)))
            {
                // This offset can't be contained in the ldr/str instruction, so we need an internal register
                buildInternalIntRegisterDefForNode(indirTree);
            }
        }
#ifdef TARGET_ARM64
        else if (addr->OperGet() == GT_CLS_VAR_ADDR)
        {
            // Reserve int to load constant from memory (IF_LARGELDC)
            buildInternalIntRegisterDefForNode(indirTree);
        }
#endif // TARGET_ARM64
    }

#ifdef FEATURE_SIMD
    if (indirTree->TypeIs(TYP_SIMD12))
    {
        // Vector3 is read/written as two reads/writes: 8 byte and 4 byte.
        // To assemble the vector properly we would need an additional int register
        buildInternalIntRegisterDefForNode(indirTree);

        if (indirTree->OperIs(GT_STOREIND))
        {
            GenTree* value = indirTree->AsStoreInd()->GetValue();

            if (value->isContained())
            {
                int srcCount = BuildIndirUses(indirTree);
                srcCount += value->OperIs(GT_IND) ? BuildIndirUses(value->AsIndir()) : 0;
                buildInternalRegisterUses();
                return srcCount;
            }
        }
    }
#endif // FEATURE_SIMD

    int srcCount = BuildIndirUses(indirTree);
    buildInternalRegisterUses();

    if (!indirTree->OperIs(GT_STOREIND, GT_NULLCHECK))
    {
        BuildDef(indirTree);
    }
    return srcCount;
}

int LinearScan::BuildCall(GenTreeCall* call)
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
        buildInternalIntRegisterDefForNode(call);
    }
#endif
#ifdef TARGET_ARM
    else
    {
        buildInternalIntRegisterDefForNode(call);
    }

    if (call->NeedsNullCheck())
    {
        buildInternalIntRegisterDefForNode(call);
    }

#endif // TARGET_ARM

    int srcCount = 0;

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
                srcCount++;
                regIndex++;

#ifdef TARGET_ARM
                if (use.GetNode()->TypeIs(TYP_LONG))
                {
                    BuildUse(use.GetNode(), genRegMask(REG_NEXT(use.GetNode()->GetRegNum())), 1);
                    srcCount++;
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
                srcCount++;
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
            srcCount += 2;
            continue;
        }
#endif

        BuildUse(argNode, genRegMask(argNode->GetRegNum()));
        srcCount++;
    }

    if (ctrlExpr != nullptr)
    {
        BuildUse(ctrlExpr);
        srcCount++;
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

    return srcCount;
}

int LinearScan::BuildPutArgStk(GenTreePutArgStk* putArg)
{
    GenTree* src = putArg->GetOp(0);

    if (src->OperIs(GT_FIELD_LIST))
    {
        assert(src->isContained());

        int srcCount = 0;
        for (GenTreeFieldList::Use& use : src->AsFieldList()->Uses())
        {
            if (!use.GetNode()->isContained())
            {
                BuildUse(use.GetNode());
                srcCount++;

#if defined(FEATURE_SIMD) && defined(OSX_ARM64_ABI)
                if (use.GetType() == TYP_SIMD12)
                {
                    // Vector3 is read/written as two reads/writes: 8 byte and 4 byte.
                    // To assemble the vector properly we would need an additional int register.
                    // The other platforms can write it as 16-byte using 1 write.
                    buildInternalIntRegisterDefForNode(use.GetNode());
                }
#endif // FEATURE_SIMD && OSX_ARM64_ABI
            }
        }
        return srcCount;
    }

    if (src->TypeIs(TYP_STRUCT))
    {
        assert(src->isContained());

        // We can use a ldp/stp sequence so we need two internal registers for ARM64; one for ARM.
        buildInternalIntRegisterDefForNode(putArg);
#ifdef TARGET_ARM64
        buildInternalIntRegisterDefForNode(putArg);
#endif
        int srcCount = src->OperIs(GT_OBJ) ? BuildAddrUses(src->AsObj()->GetAddr()) : 0;
        buildInternalRegisterUses();
        return srcCount;
    }

    if (!src->isContained())
    {
        BuildUse(src);
        return 1;
    }

    return 0;
}

#if FEATURE_ARG_SPLIT
int LinearScan::BuildPutArgSplit(GenTreePutArgSplit* putArg)
{
    CallArgInfo* argInfo    = putArg->GetArgInfo();
    regMaskTP    argRegMask = RBM_NONE;

    for (unsigned i = 0; i < argInfo->GetRegCount(); i++)
    {
        argRegMask |= genRegMask(argInfo->GetRegNum(i));
    }

    GenTree* src      = putArg->GetOp(0);
    unsigned srcCount = 0;

    if (src->IsIntegralConst(0))
    {
        BuildUse(src);
        srcCount++;
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
                srcCount++;
                regIndex++;

#ifdef TARGET_ARM
                if (node->TypeIs(TYP_LONG))
                {
                    assert(node->OperIs(GT_BITCAST));

                    regMask = genRegMask(argInfo->GetRegNum(regIndex));

                    BuildUse(node, regMask, 1);
                    srcCount++;
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
                srcCount += BuildAddrUses(src->AsObj()->GetAddr());
            }

            BuildInternalUses();
        }
    }

    for (unsigned i = 0; i < argInfo->GetRegCount(); i++)
    {
        BuildDef(putArg, putArg->GetRegType(i), genRegMask(argInfo->GetRegNum(i)), i);
    }

    return srcCount;
}
#endif // FEATURE_ARG_SPLIT

int LinearScan::BuildStructStore(GenTree* store, StructStoreKind kind, ClassLayout* layout)
{
#ifdef TARGET_ARM64
    if (kind == StructStoreKind::UnrollRegsWB)
    {
        return BuildStructStoreUnrollRegsWB(store->AsObj(), layout);
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

    int useCount = 0;

    if (dstAddr != nullptr)
    {
        if (!dstAddr->isContained())
        {
            useCount++;
            BuildUse(dstAddr, dstAddrRegMask);
        }
        else if (dstAddr->IsAddrMode())
        {
            useCount += BuildAddrUses(dstAddr->AsAddrMode()->GetBase());
        }
    }

    if (kind == StructStoreKind::UnrollRegs)
    {
        unsigned regCount = src->AsCall()->GetRegCount();
        useCount += regCount;

        for (unsigned i = 0; i < regCount; i++)
        {
            BuildUse(src, RBM_NONE, i);
        }
    }
    else if (srcAddrOrFill != nullptr)
    {
        if (!srcAddrOrFill->isContained())
        {
            useCount++;
            BuildUse(srcAddrOrFill, srcRegMask);
        }
        else if (srcAddrOrFill->IsAddrMode())
        {
            useCount += BuildAddrUses(srcAddrOrFill->AsAddrMode()->GetBase());
        }
    }

    BuildInternalUses();
    BuildKills(store, getKillSetForStructStore(kind));

    return useCount;
}

int LinearScan::BuildStructStoreUnrollRegsWB(GenTreeObj* store, ClassLayout* layout)
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

    return 3;
#endif
}

int LinearScan::BuildCast(GenTreeCast* cast)
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

    int srcCount = BuildOperandUses(src);
    buildInternalRegisterUses();
    BuildDef(cast);
    return srcCount;
}

#endif // TARGET_ARMARCH
