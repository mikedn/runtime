#include "jitpch.h"
#include "treelifeupdater.h"
#include "codegen.h"

void CodeGenLivenessUpdater::Begin()
{
    currentLife = VarSetOps::MakeEmpty(compiler);
    liveGCLcl   = VarSetOps::MakeEmpty(compiler);

    if (compiler->opts.compDbgInfo)
    {
        varCount   = compiler->info.compLocalsCount;
        paramCount = compiler->info.compArgsCount;
        assert(paramCount <= varCount);

        if (varCount != 0)
        {
            bodyVars   = new (compiler, CMK_VariableLiveRanges) DbgInfoVar[varCount + paramCount];
            prologVars = bodyVars + varCount;
        }
    }

#ifdef DEBUG
    scratchSet1 = VarSetOps::MakeEmpty(compiler);
    scratchSet2 = VarSetOps::MakeEmpty(compiler);
    epoch       = compiler->GetCurLVEpoch();
#endif
}

void CodeGenLivenessUpdater::End(CodeGen* codeGen)
{
    if (VarSetOps::IsEmpty(compiler, currentLife))
    {
        return;
    }

    liveGCRefRegs   = RBM_NONE;
    liveGCByRefRegs = RBM_NONE;
    liveLclRegs     = RBM_NONE;

    VarSetOps::ClearD(compiler, currentLife);
    VarSetOps::ClearD(compiler, liveGCLcl);
}

static regMaskTP GetLclRegs(const LclVarDsc* lcl)
{
    assert(lcl->lvIsInReg());

    if (varTypeUsesFloatReg(lcl->GetType()))
    {
        return genRegMaskFloat(lcl->GetRegNum(), lcl->GetType());
    }

    return genRegMask(lcl->GetRegNum());
}

void CodeGenLivenessUpdater::BeginBlockCodeGen(CodeGen* codeGen, BasicBlock* block, const RegNumSmall* varRegMap)
{
    regMaskTP newLclRegs     = RBM_NONE;
    regMaskTP newGCRefRegs   = RBM_NONE;
    regMaskTP newGCByrefRegs = RBM_NONE;

    if (compiler->lvaTrackedCount == 0)
    {
        JITDUMP("No variables have liveness\n");
        assert(varRegMap == nullptr);
    }
    else
    {
        VARSET_TP newLife = block->bbLiveIn;

        DBEXEC(compiler->verbose, compiler->dmpVarSetDiff("Live vars at start of block: ", currentLife, newLife);)
        JITDUMP("Var regs:");

        for (VarSetOps::Enumerator en(compiler, newLife); en.MoveNext();)
        {
            unsigned   lclNum = compiler->lvaTrackedIndexToLclNum(en.Current());
            LclVarDsc* lcl    = compiler->lvaGetDesc(lclNum);

            if (!lcl->IsRegCandidate())
            {
                continue;
            }

            regNumber oldRegNum = lcl->GetRegNum();
            regNumber newRegNum = static_cast<regNumber>(varRegMap[en.Current()]);

            if (oldRegNum != newRegNum)
            {
                lcl->SetRegNum(newRegNum);

                JITDUMP(" V%02u (%s -> %s)", lclNum, getRegName(oldRegNum), getRegName(newRegNum));

                if ((block->bbPrev != nullptr) && VarSetOps::IsMember(compiler, block->bbPrev->bbLiveOut, en.Current()))
                {
                    // lcl was alive on previous block end ("bb->bbPrev->bbLiveOut"), so it has an open
                    // "DbgInfoVarRange" which should change to be according "getInVarToRegMap"
                    UpdateRange(codeGen, lcl, lclNum);
                }
            }
            else if (newRegNum != REG_STK)
            {
                JITDUMP(" V%02u (%s)", lclNum, getRegName(newRegNum));
            }
        }

        JITDUMP("\n");

        if (!VarSetOps::Equal(compiler, currentLife, newLife))
        {
            auto SymmetricDiff = [](size_t x, size_t y) { return x ^ y; };

            for (auto e = VarSetOps::EnumOp(compiler, SymmetricDiff, currentLife, newLife); e.MoveNext();)
            {
                unsigned   lclNum = compiler->lvaTrackedIndexToLclNum(e.Current());
                LclVarDsc* lcl    = compiler->lvaGetDesc(lclNum);

                if (VarSetOps::IsMember(compiler, currentLife, e.Current()))
                {
                    if (lcl->HasGCSlotLiveness())
                    {
                        VarSetOps::RemoveElemD(compiler, liveGCLcl, e.Current());
                    }

                    EndRange(codeGen, lclNum);
                }
                else
                {
                    StartRange(codeGen, lcl, lclNum);
                }
            }

            VarSetOps::Assign(compiler, currentLife, newLife);
        }

        DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet1, liveGCLcl));

        for (VarSetOps::Enumerator en(compiler, newLife); en.MoveNext();)
        {
            LclVarDsc* lcl = compiler->lvaGetDescByTrackedIndex(en.Current());

            if (lcl->lvIsInReg())
            {
                regMaskTP lclRegs = GetLclRegs(lcl);

                newLclRegs |= lclRegs;

                if (lcl->TypeIs(TYP_REF))
                {
                    newGCRefRegs |= lclRegs;
                }
                else if (lcl->TypeIs(TYP_BYREF))
                {
                    newGCByrefRegs |= lclRegs;
                }
            }

            if (lcl->HasGCSlotLiveness())
            {
                if (lcl->lvIsInReg() && !lcl->IsAlwaysAliveInMemory())
                {
                    VarSetOps::RemoveElemD(compiler, liveGCLcl, en.Current());
                }
                else
                {
                    VarSetOps::AddElemD(compiler, liveGCLcl, en.Current());
                }
            }
        }

        DBEXEC(compiler->verbose, compiler->dmpVarSetDiff("Live GC stack vars: ", scratchSet1, liveGCLcl));
    }

    if (handlerGetsXcptnObj(block->bbCatchTyp))
    {
        for (GenTree* node : LIR::AsRange(block))
        {
            if (node->OperIs(GT_CATCH_ARG))
            {
                newGCRefRegs |= RBM_EXCEPTION_OBJECT;
                break;
            }
        }
    }

#ifdef DEBUG
    if (compiler->verbose)
    {
        DumpRegSetDiff("Live regs: ", liveLclRegs, newLclRegs);
        DumpRegSetDiff("GC regs: ", liveGCRefRegs, newGCRefRegs);
        DumpRegSetDiff("Byref regs: ", liveGCByRefRegs, newGCByrefRegs);
    }
#endif

    liveLclRegs     = newLclRegs;
    liveGCRefRegs   = newGCRefRegs;
    liveGCByRefRegs = newGCByrefRegs;
}

void CodeGenLivenessUpdater::UpdateLife(CodeGen* codeGen, GenTreeLclVarCommon* lclNode)
{
    assert(lclNode->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD) && !lclNode->IsMultiRegLclVar());
    assert(compiler->GetCurLVEpoch() == epoch);

    LclVarDsc* lcl = compiler->lvaGetDesc(lclNode);

    if (!lcl->HasLiveness())
    {
        if (!lcl->IsAddressExposed() && lcl->IsPromoted())
        {
            UpdateLifePromoted(codeGen, lclNode);
        }

        return;
    }

    bool isBorn =
        lclNode->OperIs(GT_STORE_LCL_VAR) || (lclNode->OperIs(GT_STORE_LCL_FLD) && !lclNode->IsPartialLclFld(compiler));
    bool isDying = (lclNode->gtFlags & GTF_VAR_DEATH) != 0;
    bool spill   = lclNode->IsAnyRegSpill();

    if (isBorn || isDying)
    {
        DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet1, currentLife);)
        DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet2, liveGCLcl);)

        if (isBorn && lcl->IsRegCandidate() && (lclNode->GetRegNum() != REG_NA))
        {
            lcl->SetRegNum(lclNode->GetRegNum());
        }

        bool isInReg    = lcl->lvIsInReg() && (lclNode->GetRegNum() != REG_NA);
        bool isInMemory = !isInReg || lcl->IsAlwaysAliveInMemory();

        if (isInReg)
        {
            UpdateLiveLclRegs(lcl, isDying DEBUGARG(lclNode));
        }

        DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet1, currentLife);)

        bool changed;

        if (isDying)
        {
            // TODO-MIKE-Review: Why does the assert below fail? Old comment
            // mentions QMARKs but there's no such thing in LIR. CopyProp
            // liveness had a similar issue but the same fix isn't sufficient
            // here.
            //
            // assert(VarSetOps::IsMember(compiler, newLife, lcl->GetLivenessBitIndex()));

            changed = VarSetOps::TryRemoveElemD(compiler, currentLife, lcl->GetLivenessBitIndex());
        }
        else
        {
            // This shouldn't be in newLife, unless this is debug code, in which
            // case we keep vars live everywhere, OR the variable is address-exposed,
            // OR this block is part of a try block, in which case it may be live at the handler
            // Could add a check that, if it's in newLife, that it's also in
            // fgGetHandlerLiveVars(codeGen->m_currentBlock), but seems excessive
            //
            // For a dead store, it can be the case that we set both isBorn and isDying to true.
            // (We don't eliminate dead stores under MinOpts, so we can't assume they're always
            // eliminated.)  If it's both, we handled it above.
            changed = VarSetOps::TryAddElemD(compiler, currentLife, lcl->GetLivenessBitIndex());
        }

        if (changed)
        {
            if (isInMemory && lcl->HasGCSlotLiveness())
            {
                if (isBorn)
                {
                    VarSetOps::AddElemD(compiler, liveGCLcl, lcl->GetLivenessBitIndex());
                }
                else
                {
                    VarSetOps::RemoveElemD(compiler, liveGCLcl, lcl->GetLivenessBitIndex());
                }
            }

            DBEXEC(compiler->verbose, DumpDiff(codeGen);)

            if (isBorn && !isDying)
            {
                StartRange(codeGen, lcl, lclNode->GetLclNum());
            }

            if (isDying && !isBorn)
            {
                EndRange(codeGen, lclNode->GetLclNum());
            }
        }
    }

    if (spill)
    {
        // TODO-MIKE-Review: There's something dubious going on here, or perhaps in LSRA. On ARM64 a last-use
        // gets spilled and that results in an assert in "variable range". The range was already closed above
        // and now we're trying to update it to account for spilling. But why would a last-use need spilling
        // to begin with?
        if (codeGen->SpillRegCandidateLclVar(lclNode->AsLclVar()))
        {
            UpdateRange(codeGen, lcl, lclNode->GetLclNum());
        }

        if (lcl->HasGCSlotLiveness() && VarSetOps::TryAddElemD(compiler, liveGCLcl, lcl->lvVarIndex))
        {
            JITDUMP("GC pointer V%02u becoming live on stack\n", lclNode->GetLclNum());
        }
    }
}

void CodeGenLivenessUpdater::UpdateLifeMultiReg(CodeGen* codeGen, GenTreeLclVar* lclNode)
{
    assert(lclNode->OperIs(GT_STORE_LCL_VAR));

    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet1, currentLife);)
    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet2, liveGCLcl);)

    LclVarDsc* lcl = compiler->lvaGetDesc(lclNode);

    assert(lcl->IsIndependentPromoted());

    for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
    {
        LclVarDsc* fieldLcl = compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

        bool isInReg      = fieldLcl->lvIsInReg();
        bool isInMemory   = !isInReg || fieldLcl->IsAlwaysAliveInMemory();
        bool isFieldDying = lclNode->IsLastUse(i);

        if (isInReg)
        {
            UpdateLiveLclRegs(fieldLcl, isFieldDying DEBUGARG(lclNode));
        }

        if (isFieldDying)
        {
            VarSetOps::RemoveElemD(compiler, currentLife, fieldLcl->GetLivenessBitIndex());
        }
        else
        {
            VarSetOps::AddElemD(compiler, currentLife, fieldLcl->GetLivenessBitIndex());
        }

        if (isInMemory && fieldLcl->HasGCSlotLiveness())
        {
            // TODO-MIKE-Review: Should we remove the local from the GC var set when the field is dying?
            // The "scalar" version of this code doesn't do it, it checks "isBorn" instead of "isDying".

            VarSetOps::AddElemD(compiler, liveGCLcl, fieldLcl->GetLivenessBitIndex());
        }
    }

    DBEXEC(compiler->verbose, DumpDiff(codeGen);)
}

void CodeGenLivenessUpdater::UpdateLifePromoted(CodeGen* codeGen, GenTreeLclVarCommon* lclNode)
{
    assert(!lclNode->IsMultiRegLclVar() && !lclNode->IsAnyRegSpill());

    bool isBorn  = lclNode->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD);
    bool isDying = lclNode->HasLastUse();

    if (!isBorn && !isDying)
    {
        return;
    }

    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet1, currentLife);)
    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet2, liveGCLcl);)

    LclVarDsc* lcl = compiler->lvaGetDesc(lclNode);

    unsigned lclOffset    = 0;
    unsigned lclEndOffset = lcl->TypeIs(TYP_STRUCT) ? lcl->GetLayout()->GetSize() : varTypeSize(lcl->GetType());

    if (GenTreeLclFld* lclFld = lclNode->IsLclFld())
    {
        lclOffset    = lclFld->GetLclOffs();
        lclEndOffset = lclOffset + (lclFld->TypeIs(TYP_STRUCT) ? lclFld->GetLayout(compiler)->GetSize()
                                                               : varTypeSize(lclFld->GetType()));
    }

    for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
    {
        LclVarDsc* fieldLcl = compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

        assert(!fieldLcl->TypeIs(TYP_STRUCT));

        unsigned fieldOffset    = fieldLcl->GetPromotedFieldOffset();
        unsigned fieldEndOffset = fieldOffset + varTypeSize(fieldLcl->GetType());
        bool     partialOverlap = (fieldOffset < lclEndOffset) && (fieldEndOffset > lclOffset);

        if (!partialOverlap)
        {
            continue;
        }

        if (!fieldLcl->HasLiveness())
        {
            continue;
        }

        bool totalOverlap = (lclOffset <= fieldOffset) && (fieldEndOffset <= lclEndOffset);

        if (lclNode->IsLastUse(i))
        {
            VarSetOps::RemoveElemD(compiler, currentLife, fieldLcl->GetLivenessBitIndex());
        }
        else
        {
            VarSetOps::AddElemD(compiler, currentLife, fieldLcl->GetLivenessBitIndex());
        }

        if (fieldLcl->HasGCSlotLiveness())
        {
            // TODO-MIKE-Review: Should we remove the local from the GC var set when the field is dying?
            // The "scalar" version of this code doesn't do it, it checks "isBorn" instead of "isDying".

            if (isBorn)
            {
                VarSetOps::AddElemD(compiler, liveGCLcl, fieldLcl->GetLivenessBitIndex());
            }
            else
            {
                VarSetOps::RemoveElemD(compiler, liveGCLcl, fieldLcl->GetLivenessBitIndex());
            }
        }
    }

    DBEXEC(compiler->verbose, DumpDiff(codeGen);)
}

void CodeGenLivenessUpdater::MoveReg(CodeGen* codeGen, LclVarDsc* lcl, GenTreeLclVar* src, GenTreeCopyOrReload* dst)
{
    assert(src->OperIs(GT_LCL_VAR));
    assert(lcl->GetRegNum() != REG_STK);

    RegNum srcReg = src->GetRegNum();
    RegNum dstReg = dst->GetRegNum();

    UpdateLiveLclRegs(lcl, /* isDying */ true DEBUGARG(src));
    RemoveGCRegs(genRegMask(srcReg));
    lcl->SetRegNum(dstReg);
    UpdateRange(codeGen, lcl, src->AsLclVar()->GetLclNum());
    UpdateLiveLclRegs(lcl, /* isDying */ false DEBUGARG(dst));
    SetGCRegType(dstReg, dst->GetType());
}

void CodeGenLivenessUpdater::Spill(LclVarDsc* lcl, GenTreeLclVar* lclNode)
{
    assert(lclNode->OperIs(GT_LCL_VAR, GT_STORE_LCL_VAR));

    UpdateLiveLclRegs(lcl, /* isDying */ true DEBUGARG(lclNode));
    RemoveGCRegs(GetLclRegs(lcl));
    AddGCSlot(lcl);
}

void CodeGenLivenessUpdater::Unspill(
    CodeGen* codeGen, LclVarDsc* lcl, GenTreeLclVar* src, RegNum dstReg, var_types dstType)
{
    assert(src->OperIs(GT_LCL_VAR));

    // Don't update the variable's location if we are just re-spilling it again.
    if (!src->IsRegSpill(0))
    {
        lcl->SetRegNum(dstReg);

        // We want DbgInfoVarRange inclusive on the beginning and exclusive on the ending.
        // For that we shouldn't report an update of the variable location if is becoming
        // dead on the same native offset.
        if (!src->IsLastUse(0))
        {
            UpdateRange(codeGen, lcl, src->GetLclNum());
        }

        if (!lcl->IsAlwaysAliveInMemory())
        {
            RemoveGCSlot(lcl);
        }

        JITDUMP("V%02u in reg %s is becoming live at [%06u]\n", src->GetLclNum(), getRegName(lcl->GetRegNum()),
                src->GetID());

        AddLiveLclRegs(GetLclRegs(lcl));
    }

    SetGCRegType(dstReg, dstType);
}

void CodeGenLivenessUpdater::BeginPrologEpilogCodeGen()
{
    // No stack locals are live inside prologs/epilogs, they can't be accessed anyway.
    // Param and return registers may be live but since prologs and epilogs are not
    // interruptible we can ignore them for GC purposes.

    VarSetOps::ClearD(compiler, liveGCLcl);
    liveGCRefRegs   = RBM_NONE;
    liveGCByRefRegs = RBM_NONE;
}

void CodeGenLivenessUpdater::AddGCSlot(LclVarDsc* lcl)
{
    if (!lcl->HasGCSlotLiveness())
    {
        return;
    }

    JITDUMP(FMT_LCL " GC slot is %s live\n", lcl - compiler->lvaTable,
            VarSetOps::IsMember(compiler, liveGCLcl, lcl->GetLivenessBitIndex()) ? "already" : "becoming");

    VarSetOps::AddElemD(compiler, liveGCLcl, lcl->GetLivenessBitIndex());
}

void CodeGenLivenessUpdater::RemoveGCSlot(LclVarDsc* lcl)
{
    if (!lcl->HasGCSlotLiveness())
    {
        return;
    }

    JITDUMP(FMT_LCL " GC slot is %s dead\n", lcl - compiler->lvaTable,
            VarSetOps::IsMember(compiler, liveGCLcl, lcl->GetLivenessBitIndex()) ? "becoming" : "already");

    VarSetOps::RemoveElemD(compiler, liveGCLcl, lcl->GetLivenessBitIndex());
}

void CodeGenLivenessUpdater::UpdateLiveLclRegs(const LclVarDsc* lcl, bool isDying DEBUGARG(GenTree* node))
{
    regMaskTP regs = GetLclRegs(lcl);

    JITDUMP("V%02u in reg %s is becoming %s at [%06u]\n", (lcl - compiler->lvaTable), getRegName(lcl->GetRegNum()),
            isDying ? "dead" : "live", node == nullptr ? 0 : node->GetID());

    if (isDying)
    {
        // We'd like to be able to assert the following, however if we are walking
        // through a qmark/colon tree, we may encounter multiple last-use nodes.
        // assert((liveness.GetLiveLclRegs() & regMask) == regMask);

        RemoveLiveLclRegs(regs);
    }
    else
    {
        // If this is going live, the register must not have a variable in it, except
        // in the case of an exception or "spill at single-def" variable, which may be
        // already treated as live in the register.
        assert(lcl->IsAlwaysAliveInMemory() || ((liveLclRegs & regs) == RBM_NONE));

        AddLiveLclRegs(regs);
    }
}

void CodeGenLivenessUpdater::SetLiveLclRegs(regMaskTP regs)
{
    DBEXEC(compiler->verbose, DumpRegSetDiff("Live regs: ", liveLclRegs, regs);)

    liveLclRegs = regs;
}

void CodeGenLivenessUpdater::AddGCRefRegs(regMaskTP regs DEBUGARG(bool forceOutput))
{
    assert((liveGCByRefRegs & regs) == RBM_NONE);

    regMaskTP newRefRegs   = liveGCRefRegs | regs;
    regMaskTP newByRefRegs = liveGCByRefRegs & ~regs;

    INDEBUG(DumpGCRefRegsDiff(newRefRegs, forceOutput));
    INDEBUG(DumpGCByRefRegsDiff(newByRefRegs));

    liveGCRefRegs   = newRefRegs;
    liveGCByRefRegs = newByRefRegs;
}

void CodeGenLivenessUpdater::AddGCByRefRegs(regMaskTP regs DEBUGARG(bool forceOutput))
{
    regMaskTP newRefRegs   = liveGCRefRegs & ~regs;
    regMaskTP newByRefRegs = liveGCByRefRegs | regs;

    INDEBUG(DumpGCRefRegsDiff(newRefRegs));
    INDEBUG(DumpGCByRefRegsDiff(newByRefRegs, forceOutput));

    liveGCRefRegs   = newRefRegs;
    liveGCByRefRegs = newByRefRegs;
}

void CodeGenLivenessUpdater::RemoveGCRegs(regMaskTP regs DEBUGARG(bool forceOutput))
{
    regMaskTP newRefRegs   = liveGCRefRegs & ~(regs & ~liveLclRegs);
    regMaskTP newByRefRegs = liveGCByRefRegs & ~(regs & ~liveLclRegs);

    INDEBUG(DumpGCRefRegsDiff(newRefRegs, forceOutput));
    INDEBUG(DumpGCByRefRegsDiff(newByRefRegs, forceOutput));

    liveGCRefRegs   = newRefRegs;
    liveGCByRefRegs = newByRefRegs;
}

void CodeGenLivenessUpdater::SetGCRegType(regNumber reg, var_types type)
{
    regMaskTP regs = genRegMask(reg);

    switch (type)
    {
        case TYP_REF:
            AddGCRefRegs(regs);
            break;
        case TYP_BYREF:
            AddGCByRefRegs(regs);
            break;
        default:
            RemoveGCRegs(regs);
            break;
    }
}

void CodeGenLivenessUpdater::TransferGCRegType(regNumber dst, regNumber src)
{
    regMaskTP srcMask = genRegMask(src);
    regMaskTP dstMask = genRegMask(dst);

    if ((GetGCRegs(TYP_REF) & srcMask) != RBM_NONE)
    {
        AddGCRefRegs(dstMask);
    }
    else if ((GetGCRegs(TYP_BYREF) & srcMask) != RBM_NONE)
    {
        AddGCByRefRegs(dstMask);
    }
    else
    {
        RemoveGCRegs(dstMask);
    }
}

#ifdef DEBUG
void CodeGenLivenessUpdater::DumpDiff(CodeGen* codeGen)
{
    if (!VarSetOps::Equal(compiler, scratchSet1, currentLife))
    {
        compiler->dmpVarSetDiff("Live vars: ", scratchSet1, currentLife);
    }

    if (!VarSetOps::Equal(compiler, scratchSet2, liveGCLcl))
    {
        compiler->dmpVarSetDiff("GC stack vars: ", scratchSet2, liveGCLcl);
    }
}

void CodeGenLivenessUpdater::DumpGCRefRegsDiff(regMaskTP newRegs DEBUGARG(bool forceOutput))
{
    if (compiler->verbose && (forceOutput || (liveGCRefRegs != newRegs)))
    {
        DumpRegSetDiff("GC regs: ", liveGCRefRegs, newRegs);
    }
}

void CodeGenLivenessUpdater::DumpGCByRefRegsDiff(regMaskTP newRegs DEBUGARG(bool forceOutput))
{
    if (compiler->verbose && (forceOutput || (liveGCByRefRegs != newRegs)))
    {
        DumpRegSetDiff("Byref regs: ", liveGCByRefRegs, newRegs);
    }
}
#endif // DEBUG

DbgInfoVarRange* DbgInfoVar::StartRange(CodeGen* codeGen, const DbgInfoVarLoc& varLoc)
{
    noway_assert(!HasOpenRange());

    if ((lastRange != nullptr) && (varLoc == lastRange->location) &&
        // TODO-MIKE-Review: IsPreviousInsNum's handling of the cross block case is dubious,
        // it may be the reason why moving BeginBlockCodeGen around produces debug info diffs.
        lastRange->endOffset.IsPreviousInsNum(codeGen->GetEmitter()))
    {
        assert(lastRange->startOffset.Valid());
        // The variable is being born just after the instruction at which it died.
        // In this case, i.e. an update of the variable's value, we coalesce the live ranges.
        lastRange->endOffset = {};
    }
    else
    {
        DbgInfoVarRange* newRange = new (codeGen->GetCompiler(), CMK_VariableLiveRanges) DbgInfoVarRange(varLoc);
        newRange->startOffset.CaptureLocation(codeGen->GetEmitter());

        if (lastRange != nullptr)
        {
            lastRange->next = newRange;
        }
        else
        {
            firstRange = newRange;
        }

        lastRange = newRange;
        count++;
    }

#ifdef DEBUG
    if (dumpRange == nullptr)
    {
        dumpRange = lastRange;
    }
#endif

    return lastRange;
}

DbgInfoVarRange* DbgInfoVar::EndRange(CodeGen* codeGen)
{
    noway_assert(HasOpenRange());

    // Using [close, open) ranges so as to not compute the size of the last instruction
    lastRange->endOffset.CaptureLocation(codeGen->GetEmitter());

#ifdef DEBUG
    if (dumpRange == nullptr)
    {
        dumpRange = lastRange;
    }
#endif

    return lastRange;
}

void CodeGenLivenessUpdater::StartRange(CodeGen* codeGen, const LclVarDsc* lcl, unsigned lclNum)
{
    if (lclNum >= varCount)
    {
        return;
    }

    DbgInfoVarRange* range = bodyVars[lclNum].StartRange(codeGen, GetVarLocation(codeGen, lcl));

    JITDUMP("Debug info: " FMT_LCL " available in ", lclNum);
    DBEXEC(compiler->verbose, range->location.Dump(" at "));
    DBEXEC(compiler->verbose, range->startOffset.Print("\n"));
}

void CodeGenLivenessUpdater::EndRange(CodeGen* codeGen, unsigned lclNum)
{
    if ((lclNum >= varCount) || lastBasicBlockHasBeenEmited)
    {
        return;
    }

    DbgInfoVarRange* range = bodyVars[lclNum].EndRange(codeGen);

    JITDUMP("Debug info: " FMT_LCL " no longer available in ", lclNum);
    DBEXEC(compiler->verbose, range->location.Dump(" at "));
    DBEXEC(compiler->verbose, range->endOffset.Print("\n"));
}

void CodeGenLivenessUpdater::UpdateRange(CodeGen* codeGen, const LclVarDsc* lcl, unsigned lclNum)
{
    if (lclNum >= varCount || lastBasicBlockHasBeenEmited)
    {
        return;
    }

    DbgInfoVarRange* oldRange = bodyVars[lclNum].EndRange(codeGen);
    DbgInfoVarRange* newRange = bodyVars[lclNum].StartRange(codeGen, GetVarLocation(codeGen, lcl));

    // If we are reporting again the same home, that means we are doing something twice?
    // assert(oldRange->location != newRange->location);

    JITDUMP("Debug info: " FMT_LCL " no longer available in ", lclNum);
    DBEXEC(compiler->verbose, oldRange->location.Dump(", available in "));
    DBEXEC(compiler->verbose, newRange->location.Dump(" at "));
    DBEXEC(compiler->verbose, newRange->startOffset.Print("\n"));
}

void CodeGenLivenessUpdater::EndCodeGen(CodeGen* codeGen)
{
    if (varCount != 0)
    {
        if ((compiler->lvaTrackedCount > 0) || !compiler->opts.OptimizationDisabled())
        {
            for (VarSetOps::Enumerator en(compiler, codeGen->GetLiveSet()); en.MoveNext();)
            {
                EndRange(codeGen, compiler->lvaTrackedIndexToLclNum(en.Current()));
            }
        }
        else
        {
            for (unsigned i = 0; i < varCount; i++)
            {
                if (bodyVars[i].HasOpenRange())
                {
                    EndRange(codeGen, i);
                }
            }
        }
    }

    lastBasicBlockHasBeenEmited = true;
}

DbgInfoVarRange* CodeGenLivenessUpdater::GetBodyRanges(unsigned lclNum) const
{
    return lclNum >= varCount ? nullptr : bodyVars[lclNum].GetRanges();
}

DbgInfoVarRange* CodeGenLivenessUpdater::GetPrologRanges(unsigned lclNum) const
{
    return lclNum >= paramCount ? nullptr : prologVars[lclNum].GetRanges();
}

unsigned CodeGenLivenessUpdater::GetRangeCount() const
{
    unsigned count = 0;

    for (unsigned lclNum = 0; lclNum < varCount; lclNum++)
    {
        if (compiler->compMap2ILvarNum(lclNum) != ICorDebugInfo::UNKNOWN_ILNUM)
        {
            count += bodyVars[lclNum].GetRangeCount();
        }
    }

    for (unsigned lclNum = 0; lclNum < paramCount; lclNum++)
    {
        if (compiler->compMap2ILvarNum(lclNum) != ICorDebugInfo::UNKNOWN_ILNUM)
        {
            count += prologVars[lclNum].GetRangeCount();
        }
    }

    return count;
}

void CodeGenLivenessUpdater::BeginBlock(CodeGen*    codeGen,
                                        BasicBlock* block,
                                        unsigned*   nextEnterScope,
                                        unsigned*   nextExitScope)
{
    assert(compiler->opts.compScopeInfo);

    if (compiler->info.compVarScopesCount == 0)
    {
        return;
    }

#ifdef FEATURE_EH_FUNCLETS
    if (inFuncletRegion)
    {
        return;
    }

    if (block->bbFlags & BBF_FUNCLET_BEG)
    {
        // For now, don't report any scopes in funclets. JIT64 doesn't.
        inFuncletRegion = true;

        JITDUMP("Scope info: found beginning of funclet region at block " FMT_BB "; ignoring following blocks\n",
                block->bbNum);

        return;
    }
#endif // FEATURE_EH_FUNCLETS

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("\nScope info: begin block " FMT_BB ", IL range ", block->bbNum);
        block->dspBlockILRange();
        printf("\n");
    }
#endif // DEBUG

    unsigned startILOffset = block->bbCodeOffs;

    if (startILOffset == BAD_IL_OFFSET)
    {
        JITDUMP("Scope info: ignoring block beginning\n");
        return;
    }

    // If we have tracked locals, use liveness to update the debug state.
    //
    // Note: we can improve on this some day -- if there are any tracked
    // locals, untracked locals will fail to be reported.
    if (compiler->lvaTrackedCount == 0)
    {
        StartUntrackedVarsRanges(codeGen, block, nextEnterScope, nextExitScope);
    }
}

void CodeGenLivenessUpdater::StartUntrackedVarsRanges(CodeGen*    codeGen,
                                                      BasicBlock* block,
                                                      unsigned*   nextEnterScope,
                                                      unsigned*   nextExitScope)
{
    if (compiler->opts.OptimizationEnabled())
    {
        return;
    }

    unsigned startILOffset = block->bbCodeOffs;

    // If we find a spot where the code offset isn't what we expect, because
    // there is a gap, it might be because we've moved the funclets out of
    // line. Catch up with the enter and exit scopes of the current block.
    // Ignore the enter/exit scope changes of the missing scopes, which for
    // funclets must be matched.
    if (lastBlockEndILOffset != startILOffset)
    {
        assert(lastBlockEndILOffset < startILOffset);

#ifndef FEATURE_EH_FUNCLETS
        return;
#else
        assert(startILOffset > 0);

        JITDUMP("Scope info: found offset hole. lastOffs=%u, currOffs=%u\n", lastBlockEndILOffset, startILOffset);

        // Skip enter & exit scopes
        while (VarScopeDsc* scope = compiler->compGetNextEnterScopeScan(startILOffset - 1, nextEnterScope))
        {
            JITDUMP("Scope info: Skipping " FMT_LCL " enter scope\n", scope->lclNum);
        }

        while (VarScopeDsc* scope = compiler->compGetNextExitScopeScan(startILOffset - 1, nextExitScope))
        {
            JITDUMP("Scope info: Skipping " FMT_LCL " exit scope\n", scope->lclNum);
        }
#endif // FEATURE_EH_FUNCLETS
    }

    // When there we are jitting methods compiled in debug mode, no variable is
    // tracked and there is no info that shows variable liveness like block->bbLiveIn.
    // On debug code variables are not enregistered the whole method so we can just
    // report them as beign born from here on the stack until the whole method is
    // generated.

    while (VarScopeDsc* scope = compiler->compGetNextEnterScope(startILOffset, nextEnterScope))
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(scope->lclNum);

        assert(!lcl->HasLiveness());

        if (!compiler->opts.compDbgCode && (lcl->GetRefCount() == 0))
        {
            JITDUMP("Skipping unreferenced " FMT_LCL " scope\n", scope->lclNum);

            continue;
        }

        StartRange(codeGen, lcl, scope->lclNum);
    }
}

void CodeGenLivenessUpdater::EndBlock(BasicBlock* block)
{
    assert(compiler->opts.compScopeInfo && (compiler->info.compVarScopesCount > 0));

#ifdef FEATURE_EH_FUNCLETS
    if (inFuncletRegion)
    {
        return;
    }
#endif

    unsigned endILOffset = block->bbCodeOffsEnd;

    if (endILOffset == BAD_IL_OFFSET)
    {
        JITDUMP("Scope info: ignoring block end\n");
        return;
    }

    lastBlockEndILOffset = endILOffset;
}

void CodeGenLivenessUpdater::BeginProlog(CodeGen* codeGen)
{
    assert(codeGen->generatingProlog);

    unsigned nextEnterScope = 0;

    while (VarScopeDsc* scope = compiler->compGetNextEnterScope(0, &nextEnterScope))
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(scope->lclNum);

        if (!lcl->IsParam())
        {
            continue;
        }

        noway_assert(scope->lclNum < paramCount);

        DbgInfoVarLoc loc;

        if (!lcl->IsRegParam())
        {
            loc.SetStackLocation(REG_SPBASE, GetVarStackOffset(codeGen, lcl));
        }
#ifdef UNIX_AMD64_ABI
        // TODO-MIKE-Review: What about ARM?
        else if (lcl->GetParamRegCount() > 1)
        {
            loc.SetRegLocation(lcl->GetParamReg(0), lcl->GetParamReg(1));
        }
#endif
        else
        {
            loc.SetRegLocation(lcl->GetParamReg());
        }

        DbgInfoVarRange* range = prologVars[scope->lclNum].StartRange(codeGen, loc);

        JITDUMP("Debug info: Param " FMT_LCL " available in ", scope->lclNum);
        DBEXEC(compiler->verbose, range->location.Dump());
        DBEXEC(compiler->verbose, range->startOffset.Print("\n"));
    }
}

void CodeGenLivenessUpdater::EndProlog(CodeGen* codeGen)
{
    for (unsigned i = 0; i < paramCount; i++)
    {
        if (prologVars[i].HasOpenRange())
        {
            prologVars[i].EndRange(codeGen);
        }
    }
}

DbgInfoVarLoc CodeGenLivenessUpdater::GetVarLocation(CodeGen* codeGen, const LclVarDsc* lcl) const
{
    RegNum baseReg;
    int    offset = lcl->GetStackOffset();

    if (!lcl->lvFramePointerBased)
    {
        baseReg = REG_SPBASE;
#if !FEATURE_FIXED_OUT_ARGS
        offset += codeGen->GetCurrentStackLevel();
#endif
    }
    else
    {
        baseReg = REG_FPBASE;
    }

    return DbgInfoVarLoc(lcl, baseReg, offset, codeGen->isFramePointerUsed());
}

int CodeGenLivenessUpdater::GetVarStackOffset(CodeGen* codeGen, const LclVarDsc* lcl) const
{
#ifdef TARGET_AMD64
    // scOffset = offset from caller SP - REGSIZE_BYTES
    // TODO-Cleanup - scOffset needs to be understood. For now just matching with the existing definition.
    return compiler->lvaToCallerSPRelativeOffset(lcl->GetStackOffset(), lcl->lvFramePointerBased) + REGSIZE_BYTES;
#else
    return lcl->GetStackOffset() - (codeGen->IsFramePointerRequired() ? REGSIZE_BYTES : codeGen->genTotalFrameSize());
#endif
}

#ifdef DEBUG
void DbgInfoVar::DumpNewRanges()
{
    for (const DbgInfoVarRange* r = dumpRange; r != nullptr; r = r->next)
    {
        if (r != dumpRange)
        {
            printf("; ");
        }

        r->Dump();
    }

    dumpRange = nullptr;
}

bool DbgInfoVar::HasNewRangesToDump() const
{
    return dumpRange != nullptr;
}

void DbgInfoVarRange::Dump(const char* suffix) const
{
    location.Dump();

    printf(" [");
    startOffset.Print();
    printf(", ");

    if (endOffset.Valid())
    {
        endOffset.Print();
    }
    else
    {
        printf("...");
    }

    printf("]%s", suffix == nullptr ? "" : suffix);
}

void CodeGenLivenessUpdater::DumpNewRanges(const BasicBlock* block)
{
    bool hasDumpedHistory = false;

    printf("\nVariable Live Range History Dump for " FMT_BB "\n", block->bbNum);

    for (unsigned i = 0; i < varCount; i++)
    {
        if (bodyVars[i].HasNewRangesToDump())
        {
            printf(FMT_LCL ": ", i);
            bodyVars[i].DumpNewRanges();
            printf("\n");
            hasDumpedHistory = true;
        }
    }

    if (!hasDumpedHistory)
    {
        printf("None.\n");
    }
}

void CodeGenLivenessUpdater::VerifyLiveGCRegs(BasicBlock* block)
{
    regMaskTP gcRegs       = liveGCRefRegs | liveGCByRefRegs;
    regMaskTP lclRegs      = liveLclRegs;
    regMaskTP nonLclGCRegs = gcRegs & ~lclRegs;

    // Remove return registers.
    if ((block->lastNode() != nullptr) && block->lastNode()->OperIs(GT_RETURN))
    {
        const ReturnTypeDesc& retDesc = compiler->info.retDesc;

        for (unsigned i = 0; i < retDesc.GetRegCount(); ++i)
        {
            if (varTypeIsGC(retDesc.GetRegType(i)))
            {
                nonLclGCRegs &= ~genRegMask(retDesc.GetRegNum(i));
            }
        }
    }

    if (nonLclGCRegs != RBM_NONE)
    {
        printf("Regs after " FMT_BB " ref-regs", block->bbNum);
        DumpRegSet(liveGCRefRegs & ~lclRegs);
        printf(", byref-regs");
        DumpRegSet(liveGCByRefRegs & ~lclRegs);
        printf(", lcl-regs");
        DumpRegSet(lclRegs);
        printf("\n");
    }

    assert(nonLclGCRegs == RBM_NONE);
}

void CodeGenLivenessUpdater::VerifyLiveRegVars(BasicBlock* block)
{
    DBEXEC(compiler->verbose, compiler->dmpVarSetDiff("Live out vars: ", block->bbLiveOut, currentLife));

    // The current live set should be equal to the block's live out set, except that
    // we don't keep it up to date for locals that are not register candidates.

    bool foundMismatch = false;
    auto SymmetricDiff = [](size_t x, size_t y) { return x ^ y; };

    for (auto en = VarSetOps::EnumOp(compiler, SymmetricDiff, currentLife, block->bbLiveOut); en.MoveNext();)
    {
        LclVarDsc* lcl = compiler->lvaGetDescByTrackedIndex(en.Current());

        if (lcl->IsRegCandidate())
        {
            if (!foundMismatch)
            {
                JITDUMP("Mismatched live reg vars after " FMT_BB ":", block->bbNum);
                foundMismatch = true;
            }

            JITDUMP(" " FMT_LCL, compiler->lvaTrackedIndexToLclNum(en.Current()));
        }
    }

    if (foundMismatch)
    {
        JITDUMP("\n");
        assert(!"Found mismatched live reg var(s) after block");
    }
}
#endif // DEBUG
