// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                               GSChecks                                    XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

void Compiler::phGSCookie()
{
    assert(getNeedsGSSecurityCookie());

    unsigned prevBBCount = fgBBcount;
    gsGSChecksInitCookie();

    if (compGSReorderStackLayout)
    {
        gsCopyShadowParams();
    }

    // If we needed to create any new BasicBlocks then renumber the blocks
    if (fgBBcount > prevBBCount)
    {
        fgRenumberBlocks();
    }
}

void Compiler::gsGSChecksInitCookie()
{
    LclVarDsc* lcl = lvaNewTemp(TYP_I_IMPL, false DEBUGARG("GSCookie"));
    lvaSetImplicitlyReferenced(lcl);
    lvaGSSecurityCookie = lcl->GetLclNum();
}

/*****************************************************************************
 * gsCopyShadowParams
 * The current function has an unsafe buffer on the stack.  Search for vulnerable
 * parameters which could be used to modify a code address and take over the process
 * in the case of a buffer overrun. Create a safe local copy for each vulnerable parameter,
 * which will be allocated bellow the unsafe buffer.  Change uses of the param to the
 * shadow copy.
 *
 * A pointer under indirection is considered vulnerable. A malicious user could read from
 * protected memory or write to it. If a parameter is assigned/computed into another variable,
 * and is a pointer (i.e., under indirection), then we consider the variable to be part of the
 * equivalence class with the parameter. All parameters in the equivalence class are shadowed.
 */
void Compiler::gsCopyShadowParams()
{
    if (info.compIsVarArgs)
    {
        return;
    }

    // Find groups of variables assigned to each other, and also
    // tracks variables which are dereferenced and marks them as ptrs.
    // Look for assignments to *p, and ptrs passed to functions
    if (gsFindVulnerableParams())
    {
        // Replace vulnerable params by shadow copies.
        gsParamsToShadows();
    }
}

// This struct tracks how a tree is being used

struct MarkPtrsInfo
{
    FixedBitVect** lclAssignGroups;
    LclVarDsc*     storeLcl     = nullptr; // Which local variable is the tree being assigned to?
    bool           isUnderIndir = false;   // Is this a pointer value tree that is being dereferenced?

    MarkPtrsInfo(Compiler* comp) : lclAssignGroups(new (comp, CMK_GS) FixedBitVect*[comp->lvaCount]())
    {
    }
};

Compiler::fgWalkResult Compiler::gsMarkPtrsAndAssignGroups(GenTree** use, fgWalkData* data)
{
    MarkPtrsInfo* state = static_cast<MarkPtrsInfo*>(data->pCallbackData);
    Compiler*     comp  = data->compiler;
    GenTree*      tree  = *use;

    switch (tree->GetOper())
    {
        case GT_IND:
        case GT_BLK:
        case GT_OBJ:
        {
            bool wasUnderIndir  = state->isUnderIndir;
            state->isUnderIndir = true;
            comp->fgWalkTreePre(&tree->AsIndir()->gtOp1, comp->gsMarkPtrsAndAssignGroups, state);
            state->isUnderIndir = wasUnderIndir;

            return WALK_SKIP_SUBTREES;
        }
        case GT_ARR_ELEM:
        {
            bool wasUnderIndir  = state->isUnderIndir;
            state->isUnderIndir = true;
            for (unsigned i = 0; i < tree->AsArrElem()->GetNumOps(); i++)
            {
                comp->fgWalkTreePre(tree->AsArrElem()->GetUse(i), comp->gsMarkPtrsAndAssignGroups, state);
            }
            state->isUnderIndir = wasUnderIndir;

            return WALK_SKIP_SUBTREES;
        }
        case GT_LCL_VAR:
        case GT_LCL_FLD:
        {
            LclVarDsc* loadLcl = tree->AsLclVarCommon()->GetLcl();

            if (state->isUnderIndir)
            {
                loadLcl->lvIsPtr = true;
            }

            if (state->storeLcl != nullptr)
            {
                unsigned       loadLclNum      = loadLcl->GetLclNum();
                unsigned       storeLclNum     = state->storeLcl->GetLclNum();
                FixedBitVect** lclAssignGroups = state->lclAssignGroups;

                // Add storeLclNum and loadLclNum to a common assign group
                if (lclAssignGroups[storeLclNum] != nullptr)
                {
                    if (lclAssignGroups[loadLclNum] != nullptr)
                    {
                        // OR both bit vector
                        lclAssignGroups[storeLclNum]->bitVectOr(lclAssignGroups[loadLclNum]);
                    }
                    else
                    {
                        lclAssignGroups[storeLclNum]->bitVectSet(loadLclNum);
                    }

                    // Point both to the same bit vector
                    lclAssignGroups[loadLclNum] = lclAssignGroups[storeLclNum];
                }
                else if (lclAssignGroups[loadLclNum] != nullptr)
                {
                    lclAssignGroups[loadLclNum]->bitVectSet(storeLclNum);

                    // Point both to the same bit vector
                    lclAssignGroups[storeLclNum] = lclAssignGroups[loadLclNum];
                }
                else
                {
                    FixedBitVect* bv = FixedBitVect::bitVectInit(comp->lvaCount, comp);

                    // (shadowVarInfo[state->storeLclNum] == NULL && shadowVarInfo[lclNew] == NULL);
                    // Neither of them has an assign group yet.  Make a new one.
                    lclAssignGroups[storeLclNum] = bv;
                    lclAssignGroups[loadLclNum]  = bv;
                    bv->bitVectSet(storeLclNum);
                    bv->bitVectSet(loadLclNum);
                }
            }

            return WALK_SKIP_SUBTREES;
        }
        case GT_CALL:
        {
            LclVarDsc* prevStoreLcl  = state->storeLcl;
            bool       wasUnderIndir = state->isUnderIndir;
            state->storeLcl          = nullptr;
            state->isUnderIndir      = false;

            GenTreeCall* call = tree->AsCall();

            if (call->gtCallThisArg != nullptr)
            {
                state->isUnderIndir = true;
                comp->fgWalkTreePre(&call->gtCallThisArg->NodeRef(), gsMarkPtrsAndAssignGroups, state);
                // TODO-MIKE-Review: This should reset isUnderIndir probably...
            }

            for (GenTreeCall::Use& use : call->Args())
            {
                // Skip STRUCT typed LCL_VAR|FLD call args, previously these were wrapped in OBJs,
                // which this code ignored. Which is probably a bug since a struct can contain
                // pointers. Needless to say that fixing this will result in regressions due to
                // extra copying of current method's parameters.

                if (!use.GetNode()->OperIs(GT_LCL_VAR, GT_LCL_FLD) || !use.GetNode()->TypeIs(TYP_STRUCT))
                {
                    comp->fgWalkTreePre(&use.NodeRef(), gsMarkPtrsAndAssignGroups, state);
                }
            }

            for (GenTreeCall::Use& use : call->LateArgs())
            {
                if (!use.GetNode()->OperIs(GT_LCL_VAR, GT_LCL_FLD) || !use.GetNode()->TypeIs(TYP_STRUCT))
                {
                    comp->fgWalkTreePre(&use.NodeRef(), gsMarkPtrsAndAssignGroups, state);
                }
            }

            if (call->IsIndirectCall())
            {
                // A function pointer is treated like a write-through pointer since
                // it controls what code gets executed, and so indirectly can cause
                // a write to memory.

                state->isUnderIndir = true;
                comp->fgWalkTreePre(&call->gtCallAddr, gsMarkPtrsAndAssignGroups, state);
            }

            state->storeLcl     = prevStoreLcl;
            state->isUnderIndir = wasUnderIndir;

            return WALK_SKIP_SUBTREES;
        }
        case GT_STOREIND:
        case GT_STORE_BLK:
        case GT_STORE_OBJ:
        {
            GenTreeIndir* store = tree->AsIndir();
            GenTree*      addr  = store->GetAddr();
            GenTree*      value = store->GetValue();

            bool wasUnderIndir  = state->isUnderIndir;
            state->isUnderIndir = true;
            comp->fgWalkTreePre(&addr, comp->gsMarkPtrsAndAssignGroups, state);
            state->isUnderIndir = false;
            comp->fgWalkTreePre(&value, comp->gsMarkPtrsAndAssignGroups, state);
            state->isUnderIndir = wasUnderIndir;

            return WALK_SKIP_SUBTREES;
        }
        case GT_STORE_LCL_VAR:
        case GT_STORE_LCL_FLD:
        {
            GenTreeLclVarCommon* store = tree->AsLclVarCommon();
            GenTree*             value = store->GetOp(0);

            LclVarDsc* prevStoreLcl = state->storeLcl;
            state->storeLcl         = store->GetLcl();
            comp->fgWalkTreePre(&value, comp->gsMarkPtrsAndAssignGroups, state);
            state->storeLcl = prevStoreLcl;

            return WALK_SKIP_SUBTREES;
        }

        case GT_ARR_INDEX:
        case GT_ARR_OFFSET:
            unreached();

        default:
            return WALK_CONTINUE;
    }
}

/*****************************************************************************
 * gsFindVulnerableParams
 * Walk all the trees looking for ptrs, args, assign groups, *p stores, etc.
 * Then use that info to figure out vulnerable pointers.
 *
 * It returns true if it found atleast one vulnerable pointer parameter that
 * needs to be shadow-copied.
 */

bool Compiler::gsFindVulnerableParams()
{
    MarkPtrsInfo info(this);

    // Walk all the trees setting lvIsWritePtr, lvIsOutgoingArg, lvIsPtr and assignGroup.
    for (BasicBlock* const block : Blocks())
    {
        for (Statement* const stmt : block->Statements())
        {
            fgWalkTreePre(stmt->GetRootNodePointer(), gsMarkPtrsAndAssignGroups, &info);
        }
    }

    // Compute has vulnerable at the end of the loop.
    bool hasOneVulnerable = false;

    // Initialize propagated[v0...vn] = {0}^n, so we can skip the ones propagated through
    // some assign group.
    FixedBitVect* propagated = (lvaCount > 0) ? FixedBitVect::bitVectInit(lvaCount, this) : nullptr;

    for (LclVarDsc* varDsc : Locals())
    {
        unsigned      lclNum      = varDsc->GetLclNum();
        FixedBitVect* assignGroup = info.lclAssignGroups[lclNum];

        // If there was an indirection or if unsafe buffer, then we'd call it vulnerable.
        if (varDsc->lvIsPtr || varDsc->lvIsUnsafeBuffer)
        {
            hasOneVulnerable = true;
        }

        // Now, propagate the info through the assign group (an equivalence class of vars transitively assigned.)
        if ((assignGroup == nullptr) || propagated->bitVectTest(lclNum))
        {
            continue;
        }

        // Propagate lvIsPtr, so that:
        //   1. Any parameter in the equivalence class can be identified as lvIsPtr and hence shadowed.
        //   2. Buffers with pointers are placed at lower memory addresses than buffers without pointers.
        bool isUnderIndir = varDsc->lvIsPtr;

        // First pass -- find if any variable is vulnerable.
        for (unsigned i = assignGroup->bitVectGetFirst(); i != UINT_MAX && !isUnderIndir;
             i          = assignGroup->bitVectGetNext(i))
        {
            isUnderIndir |= lvaGetDesc(i)->lvIsPtr;
        }

        if (!isUnderIndir)
        {
            continue;
        }

        hasOneVulnerable = true;

        // Second pass -- mark all are vulnerable.
        for (unsigned i = assignGroup->bitVectGetFirst(); i != UINT_MAX; i = assignGroup->bitVectGetNext(i))
        {
            lvaGetDesc(i)->lvIsPtr = true;
            propagated->bitVectSet(i);
        }

#ifdef DEBUG
        if (verbose)
        {
            printf("Equivalence assign group %s: ", isUnderIndir ? "isPtr " : "");
            for (unsigned i = assignGroup->bitVectGetFirst(); i != UINT_MAX; i = assignGroup->bitVectGetNext(i))
            {
                gtDispLclVar(i, false);
                printf(" ");
            }
            printf("\n");
        }
#endif
    }

    return hasOneVulnerable;
}

static bool MayNeedShadowCopy(LclVarDsc* lcl)
{
#ifdef TARGET_AMD64
    // GS cookie logic to create shadow slots, create trees to copy reg args to shadow
    // slots and update all trees to refer to shadow slots is done immediately after
    // fgMorph().  Lsra could potentially mark a param as DoNotEnregister after JIT determines
    // not to shadow a parameter.  Also, LSRA could potentially spill a param which is passed
    // in register. Therefore, conservatively all params may need a shadow copy.  Note that
    // GS cookie logic further checks whether the param is a ptr or an unsafe buffer before
    // creating a shadow slot even though this routine returns true.
    //
    // TODO-AMD64-CQ: Revisit this conservative approach as it could create more shadow slots than
    // required. There are two cases under which a reg arg could potentially be used from its
    // home location:
    //   a) LSRA marks it as DoNotEnregister (see LinearScan::identifyCandidates())
    //   b) LSRA spills it
    //
    // Possible solution to address case (a)
    //   - The conditions under which LSRA marks a varDsc as DoNotEnregister could be checked
    //     in this routine.  Note that live out of exception handler is something we may not be
    //     able to do it here since GS cookie logic is invoked ahead of liveness computation.
    //     Therefore, for methods with exception handling and need GS cookie check we might have
    //     to take conservative approach.
    //
    // Possible solution to address case (b)
    //   - Whenever a parameter passed in an argument register needs to be spilled by LSRA, we
    //     create a new spill temp if the method needs GS cookie check.
    return lcl->IsParam();
#else
    return lcl->IsParam() && !lcl->IsRegParam();
#endif
}

// Copy each vulnerable param ptr or buffer to a local shadow
// copy and replace uses of the param by the shadow copy.
void Compiler::gsParamsToShadows()
{
    jitstd::span<LclVarDsc*> locals = Locals();

    gsLclShadowMap = new (this, CMK_GS) unsigned[locals.size()];

    for (LclVarDsc* lcl : locals)
    {
        unsigned lclNum = lcl->GetLclNum();

        if (!MayNeedShadowCopy(lcl) || (!lcl->lvIsPtr && !lcl->lvIsUnsafeBuffer))
        {
            gsLclShadowMap[lclNum] = BAD_VAR_NUM;

            continue;
        }

        LclVarDsc* shadowLcl = lvaAllocTemp(false DEBUGARG("shadow copy"));
        JITDUMP("V%02u is shadow param candidate. Shadow copy is V%02u.\n", lclNum, shadowLcl->GetLclNum());
        gsLclShadowMap[lclNum] = shadowLcl->GetLclNum();

        // TODO-MIKE-Cleanup: varActualType is likely useless, there should be no need to shadow
        // copy small int locals.
        var_types type = varActualType(lcl->GetType());

        if (varTypeIsStruct(type))
        {
            // We don't need checkUnsafeBuffer here since we are copying the params and
            // this flag would have been set on the original param before reaching here.
            lvaSetStruct(shadowLcl, lcl->GetLayout(), /* checkUnsafeBuffer */ false);

            shadowLcl->lvIsMultiRegArg = lcl->lvIsMultiRegArg;
            shadowLcl->lvIsMultiRegRet = lcl->lvIsMultiRegRet;
        }
        else if (type == TYP_BLK)
        {
            shadowLcl->SetBlockType(lcl->GetBlockSize());
        }
        else
        {
            shadowLcl->SetType(type);
        }

        shadowLcl->lvAddrExposed     = lcl->IsAddressExposed();
        shadowLcl->lvDoNotEnregister = lcl->lvDoNotEnregister;
        shadowLcl->lvIsUnsafeBuffer  = lcl->lvIsUnsafeBuffer;
        shadowLcl->lvIsPtr           = lcl->lvIsPtr;
#ifdef DEBUG
        shadowLcl->lvLiveInOutOfHndlr = lcl->lvLiveInOutOfHndlr;
        shadowLcl->lvLclFieldExpr     = lcl->lvLclFieldExpr;
#endif
    }

    class ReplaceShadowParamsVisitor final : public GenTreeVisitor<ReplaceShadowParamsVisitor>
    {
        // Walk the locals of the method (i.e. GT_LCL_FLD and GT_LCL_VAR nodes) and replace the ones that correspond to
        // "vulnerable" parameters with their shadow copies. If an original local variable has small type then replace
        // the GT_LCL_VAR node type with TYP_INT.
    public:
        enum
        {
            DoPreOrder    = true,
            DoLclVarsOnly = true
        };

        ReplaceShadowParamsVisitor(Compiler* compiler) : GenTreeVisitor<ReplaceShadowParamsVisitor>(compiler)
        {
        }

        Compiler::fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
        {
            GenTree* tree = *use;

            LclVarDsc* lcl          = tree->AsLclVarCommon()->GetLcl();
            unsigned   shadowLclNum = m_compiler->gsLclShadowMap[lcl->GetLclNum()];

            if (shadowLclNum != BAD_VAR_NUM)
            {
                tree->AsLclVarCommon()->SetLcl(m_compiler->lvaGetDesc(shadowLclNum));

                if (varTypeIsSmall(lcl->GetType()) && tree->OperIs(GT_LCL_VAR, GT_STORE_LCL_VAR))
                {
                    tree->SetType(TYP_INT);
                }
            }

            return WALK_CONTINUE;
        }
    };

    for (BasicBlock* const block : Blocks())
    {
        for (Statement* const stmt : block->Statements())
        {
            ReplaceShadowParamsVisitor replaceShadowParamsVisitor(this);
            replaceShadowParamsVisitor.WalkTree(stmt->GetRootNodePointer(), nullptr);
        }
    }

    // Now insert code to copy the params to their shadow copy.
    for (unsigned lclNum = 0; lclNum < locals.size(); lclNum++)
    {
        unsigned shadowLclNum = gsLclShadowMap[lclNum];
        if (shadowLclNum == BAD_VAR_NUM)
        {
            continue;
        }

        LclVarDsc* lcl       = lvaGetDesc(lclNum);
        LclVarDsc* shadowLcl = lvaGetDesc(shadowLclNum);

        GenTree* src = gtNewLclvNode(lcl, lcl->GetType());
        GenTree* dst = gtNewLclvNode(shadowLcl, shadowLcl->GetType());

        src->gtFlags |= GTF_DONT_CSE;
        dst->gtFlags |= GTF_DONT_CSE;

        fgEnsureFirstBBisScratch();
        // TODO-MIKE-Review: Do we need to morph? This is a trivial assignment between
        // 2 local variables. The destination is not promoted, could the source be?
        fgNewStmtAtBeg(fgFirstBB, gtMorphTree(gtNewAssignNode(dst, src)));
    }

    // If the method has "Jmp CalleeMethod", then we need to copy shadow params back to original
    // params before "jmp" to CalleeMethod.
    if (compJmpOpUsed)
    {
        // There could be more than one basic block ending with a "Jmp" type tail call.
        // We would have to insert assignments in all such blocks, just before GT_JMP stmnt.
        for (BasicBlock* const block : Blocks())
        {
            if ((block->bbJumpKind != BBJ_RETURN) || ((block->bbFlags & BBF_HAS_JMP) == 0))
            {
                continue;
            }

            for (LclVarDsc* lcl : Params())
            {
                unsigned lclNum       = lcl->GetLclNum();
                unsigned shadowLclNum = gsLclShadowMap[lclNum];
                if (shadowLclNum == BAD_VAR_NUM)
                {
                    continue;
                }

                LclVarDsc* lcl       = lvaGetDesc(lclNum);
                LclVarDsc* shadowLcl = lvaGetDesc(shadowLclNum);

                GenTree* src = gtNewLclvNode(shadowLcl, shadowLcl->GetType());
                GenTree* dst = gtNewLclvNode(lcl, lcl->GetType());

                src->gtFlags |= GTF_DONT_CSE;
                dst->gtFlags |= GTF_DONT_CSE;

                fgNewStmtNearEnd(block, gtMorphTree(gtNewAssignNode(dst, src)));
            }
        }
    }
}
