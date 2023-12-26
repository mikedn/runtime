#include "jitpch.h"
#include "treelifeupdater.h"
#include "codegen.h"

void CodeGenLivenessUpdater::Begin()
{
    currentLife = VarSetOps::MakeEmpty(compiler);
    liveGCLcl   = VarSetOps::MakeEmpty(compiler);

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

void CodeGenLivenessUpdater::BeginBlockCodeGen(CodeGen* codeGen, BasicBlock* block)
{
    currentNode = nullptr;

    VARSET_TP newLife = block->bbLiveIn;

    DBEXEC(compiler->verbose, compiler->dmpVarSetDiff("Live vars at start of block: ", currentLife, newLife);)
    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet1, liveGCLcl));

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

                codeGen->getVariableLiveKeeper()->siEndVariableLiveRange(lclNum);
            }
            else
            {
                codeGen->getVariableLiveKeeper()->siStartVariableLiveRange(lcl, lclNum);
            }
        }

        VarSetOps::Assign(compiler, currentLife, newLife);
    }

    regMaskTP newLclRegs     = RBM_NONE;
    regMaskTP newGCRefRegs   = RBM_NONE;
    regMaskTP newGCByrefRegs = RBM_NONE;

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
        compiler->dmpVarSetDiff("GC stack vars: ", scratchSet1, liveGCLcl);
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

    // TODO-Cleanup: We shouldn't really be calling this more than once
    if (lclNode == currentNode)
    {
        return;
    }

    currentNode = lclNode;

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

            codeGen->getVariableLiveKeeper()->siStartOrCloseVariableLiveRange(lcl, lclNode->GetLclNum(), isBorn,
                                                                              isDying);
        }
    }

    if (spill)
    {
        // TODO-MIKE-Review: There's something dubious going on here, or perhaps in LSRA. On ARM64 a last-use
        // gets spilled and that results in an assert in "variable range". The range was already closed above
        // and SpillRegCandidateLclVar tries to update it for spill. It may be that SpillRegCandidateLclVar
        // needs to use siStartOrCloseVariableLiveRange instead of siUpdateVariableLiveRange in this case but
        // then it's not clear why would a last-use need spilling to begin with.
        codeGen->SpillRegCandidateLclVar(lclNode->AsLclVar());

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

void CodeGenLivenessUpdater::BeginPrologEpilogCodeGen()
{
    // No stack locals are live inside prologs/epilogs, they can't be accessed anyway.
    // Param and return registers may be live but since prologs and epilogs are not
    // interruptible we can ignore them for GC purposes.

    VarSetOps::ClearD(compiler, liveGCLcl);
    liveGCRefRegs   = RBM_NONE;
    liveGCByRefRegs = RBM_NONE;
}

void CodeGenLivenessUpdater::SpillGCSlot(LclVarDsc* lcl)
{
    RemoveGCRegs(GetLclRegs(lcl));

    if (!lcl->HasGCSlotLiveness())
    {
        return;
    }

#ifdef DEBUG
    if (!VarSetOps::IsMember(compiler, liveGCLcl, lcl->GetLivenessBitIndex()))
    {
        JITDUMP("GC slot V%02u becoming live\n", lcl - compiler->lvaTable);
    }
    else
    {
        JITDUMP("GC slot V%02u continuing live\n", lcl - compiler->lvaTable);
    }
#endif

    VarSetOps::AddElemD(compiler, liveGCLcl, lcl->GetLivenessBitIndex());
}

void CodeGenLivenessUpdater::UnspillGCSlot(LclVarDsc* lcl DEBUGARG(GenTreeLclVar* lclVar))
{
    if (!lcl->IsAlwaysAliveInMemory())
    {
        RemoveGCSlot(lcl);
    }

    JITDUMP("V%02u in reg %s is becoming live at [%06u]\n", lcl - compiler->lvaTable, getRegName(lcl->GetRegNum()),
            lclVar->GetID());

    AddLiveLclRegs(GetLclRegs(lcl));
}

void CodeGenLivenessUpdater::RemoveGCSlot(LclVarDsc* lcl)
{
    if (!lcl->HasGCSlotLiveness())
    {
        return;
    }

#ifdef DEBUG
    if (VarSetOps::IsMember(compiler, liveGCLcl, lcl->GetLivenessBitIndex()))
    {
        JITDUMP("GC slot V%02u becoming dead\n", lcl - compiler->lvaTable);
    }
#endif

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

CodeGen::VariableLiveKeeper::VariableLiveDescriptor::VariableLiveDescriptor(CompAllocator allocator)
    : m_VariableLiveRanges(new (allocator) LiveRangeList(allocator))
#ifdef DEBUG
    , m_VariableLifeBarrier(new (allocator) LiveRangeDumper(m_VariableLiveRanges))
#endif
{
}

bool CodeGen::VariableLiveKeeper::VariableLiveDescriptor::hasVariableLiveRangeOpen() const
{
    return !m_VariableLiveRanges->empty() && !m_VariableLiveRanges->back().m_EndEmitLocation.Valid();
}

CodeGen::VariableLiveKeeper::LiveRangeList* CodeGen::VariableLiveKeeper::VariableLiveDescriptor::getLiveRanges() const
{
    return m_VariableLiveRanges;
}

void CodeGen::VariableLiveKeeper::VariableLiveDescriptor::startLiveRangeFromEmitter(
    CodeGenInterface::siVarLoc varLocation, emitter* emit) const
{
    noway_assert(emit != nullptr);

    noway_assert(m_VariableLiveRanges->empty() || m_VariableLiveRanges->back().m_EndEmitLocation.Valid());

    if (!m_VariableLiveRanges->empty() &&
        siVarLoc::Equals(&varLocation, &(m_VariableLiveRanges->back().m_VarLocation)) &&
        m_VariableLiveRanges->back().m_EndEmitLocation.IsPreviousInsNum(emit))
    {
        JITDUMP("Extending debug range...\n");

        // The variable is being born just after the instruction at which it died.
        // In this case, i.e. an update of the variable's value, we coalesce the live ranges.
        m_VariableLiveRanges->back().m_EndEmitLocation.Init();
    }
    else
    {
        JITDUMP("New debug range: %s\n",
                m_VariableLiveRanges->empty()
                    ? "first"
                    : siVarLoc::Equals(&varLocation, &(m_VariableLiveRanges->back().m_VarLocation))
                          ? "new var or location"
                          : "not adjacent");

        // Creates new live range with invalid end
        m_VariableLiveRanges->emplace_back(varLocation, emitLocation(), emitLocation());
        m_VariableLiveRanges->back().m_StartEmitLocation.CaptureLocation(emit);
    }

#ifdef DEBUG
    if (!m_VariableLifeBarrier->hasLiveRangesToDump())
    {
        m_VariableLifeBarrier->setDumperStartAt(m_VariableLiveRanges->backPosition());
    }
#endif

    noway_assert(m_VariableLiveRanges->back().m_StartEmitLocation.Valid());
    noway_assert(!m_VariableLiveRanges->back().m_EndEmitLocation.Valid());
}

void CodeGen::VariableLiveKeeper::VariableLiveDescriptor::endLiveRangeAtEmitter(emitter* emit) const
{
    noway_assert(emit != nullptr);
    noway_assert(hasVariableLiveRangeOpen());

    // Using [close, open) ranges so as to not compute the size of the last instruction
    m_VariableLiveRanges->back().m_EndEmitLocation.CaptureLocation(emit);

    noway_assert(m_VariableLiveRanges->back().m_EndEmitLocation.Valid());
}

void CodeGen::VariableLiveKeeper::VariableLiveDescriptor::updateLiveRangeAtEmitter(siVarLoc varLocation,
                                                                                   emitter* emit) const
{
    noway_assert((m_VariableLiveRanges != nullptr) && !m_VariableLiveRanges->empty());
    noway_assert(!m_VariableLiveRanges->back().m_EndEmitLocation.Valid());

    // If we are reporting again the same home, that means we are doing something twice?
    // noway_assert(! CodeGenInterface::siVarLoc::Equals(&m_VariableLiveRanges->back().m_VarLocation, varLocation));

    endLiveRangeAtEmitter(emit);
    startLiveRangeFromEmitter(varLocation, emit);
}

void CodeGen::initializeVariableLiveKeeper()
{
    CompAllocator allocator = compiler->getAllocator(CMK_VariableLiveRanges);

    varLiveKeeper = new (allocator) VariableLiveKeeper(compiler, allocator);
}

CodeGen::VariableLiveKeeper* CodeGen::getVariableLiveKeeper() const
{
    return varLiveKeeper;
};

CodeGen::VariableLiveKeeper::VariableLiveKeeper(Compiler* comp, CompAllocator allocator)
    : m_Compiler(comp)
    , m_LiveDscCount(comp->opts.compDbgInfo ? comp->info.compLocalsCount : 0)
    , m_LiveArgsCount(comp->opts.compDbgInfo ? comp->info.compArgsCount : 0)
{
    if (m_LiveDscCount > 0)
    {
        m_vlrLiveDsc          = allocator.allocate<VariableLiveDescriptor>(m_LiveDscCount);
        m_vlrLiveDscForProlog = allocator.allocate<VariableLiveDescriptor>(m_LiveDscCount);

        for (unsigned int varNum = 0; varNum < m_LiveDscCount; varNum++)
        {
            new (m_vlrLiveDsc + varNum) VariableLiveDescriptor(allocator);
            new (m_vlrLiveDscForProlog + varNum) VariableLiveDescriptor(allocator);
        }
    }
}

void CodeGen::VariableLiveKeeper::siStartOrCloseVariableLiveRange(const LclVarDsc* varDsc,
                                                                  unsigned int     varNum,
                                                                  bool             isBorn,
                                                                  bool             isDying)
{
    noway_assert(varDsc != nullptr);

    if (varNum < m_LiveDscCount)
    {
        if (isBorn && !isDying)
        {
            siStartVariableLiveRange(varDsc, varNum);
        }

        if (isDying && !isBorn)
        {
            siEndVariableLiveRange(varNum);
        }
    }
}

void CodeGen::VariableLiveKeeper::siStartVariableLiveRange(const LclVarDsc* varDsc, unsigned int varNum)
{
    noway_assert(varDsc != nullptr);

    if (varNum < m_LiveDscCount)
    {
        CodeGenInterface::siVarLoc varLocation =
            m_Compiler->codeGen->getSiVarLoc(varDsc
#if !FEATURE_FIXED_OUT_ARGS
                                             ,
                                             m_Compiler->codeGen->getCurrentStackLevel()
#endif
                                                 );

        VariableLiveDescriptor* varLiveDsc = &m_vlrLiveDsc[varNum];
        varLiveDsc->startLiveRangeFromEmitter(varLocation, m_Compiler->GetEmitter());
    }
}

void CodeGen::VariableLiveKeeper::siEndVariableLiveRange(unsigned int varNum)
{
    if (varNum < m_LiveDscCount && !m_LastBasicBlockHasBeenEmited)
    {
        m_vlrLiveDsc[varNum].endLiveRangeAtEmitter(m_Compiler->GetEmitter());
    }
}

void CodeGen::VariableLiveKeeper::siUpdateVariableLiveRange(const LclVarDsc* varDsc, unsigned int varNum)
{
    noway_assert(varDsc != nullptr);

    if (varNum < m_LiveDscCount && !m_LastBasicBlockHasBeenEmited)
    {
        CodeGenInterface::siVarLoc siVarLoc =
            m_Compiler->codeGen->getSiVarLoc(varDsc
#if !FEATURE_FIXED_OUT_ARGS
                                             ,
                                             m_Compiler->codeGen->getCurrentStackLevel()
#endif
                                                 );

        VariableLiveDescriptor* varLiveDsc = &m_vlrLiveDsc[varNum];
        varLiveDsc->updateLiveRangeAtEmitter(siVarLoc, m_Compiler->GetEmitter());
    }
}

void CodeGen::VariableLiveKeeper::siEndAllVariableLiveRange(VARSET_VALARG_TP varsToClose)
{
    if (m_LiveDscCount != 0)
    {
        if (m_Compiler->lvaTrackedCount > 0 || !m_Compiler->opts.OptimizationDisabled())
        {
            VarSetOps::Iter iter(m_Compiler, varsToClose);
            unsigned        varIndex = 0;
            while (iter.NextElem(&varIndex))
            {
                unsigned int varNum = m_Compiler->lvaTrackedIndexToLclNum(varIndex);
                siEndVariableLiveRange(varNum);
            }
        }
        else
        {
            // We are jitting debug code, so we don't have variable liveness info.
            siEndAllVariableLiveRange();
        }
    }

    m_LastBasicBlockHasBeenEmited = true;
}

void CodeGen::VariableLiveKeeper::siEndAllVariableLiveRange()
{
    // TODO: we can improve this keeping a set for the variables with
    // open VariableLiveRanges

    for (unsigned int varNum = 0; varNum < m_LiveDscCount; varNum++)
    {
        const VariableLiveDescriptor* varLiveDsc = m_vlrLiveDsc + varNum;
        if (varLiveDsc->hasVariableLiveRangeOpen())
        {
            siEndVariableLiveRange(varNum);
        }
    }
}

CodeGen::VariableLiveKeeper::LiveRangeList* CodeGen::VariableLiveKeeper::getLiveRangesForVarForBody(
    unsigned int varNum) const
{
    noway_assert(varNum < m_LiveDscCount);

    return m_vlrLiveDsc[varNum].getLiveRanges();
}

CodeGen::VariableLiveKeeper::LiveRangeList* CodeGen::VariableLiveKeeper::getLiveRangesForVarForProlog(
    unsigned int varNum) const
{
    noway_assert(varNum < m_LiveDscCount);

    return m_vlrLiveDscForProlog[varNum].getLiveRanges();
}

size_t CodeGen::VariableLiveKeeper::getLiveRangesCount() const
{
    size_t liveRangesCount = 0;

    for (unsigned int varNum = 0; varNum < m_LiveDscCount; varNum++)
    {
        for (int i = 0; i < 2; i++)
        {
            VariableLiveDescriptor* varLiveDsc = (i == 0 ? m_vlrLiveDscForProlog : m_vlrLiveDsc) + varNum;

            if (m_Compiler->compMap2ILvarNum(varNum) != (unsigned int)ICorDebugInfo::UNKNOWN_ILNUM)
            {
                liveRangesCount += varLiveDsc->getLiveRanges()->size();
            }
        }
    }

    return liveRangesCount;
}

void CodeGen::VariableLiveKeeper::psiStartVariableLiveRange(CodeGenInterface::siVarLoc varLocation, unsigned int varNum)
{
    noway_assert(varNum < m_LiveArgsCount);

    VariableLiveDescriptor* varLiveDsc = &m_vlrLiveDscForProlog[varNum];
    varLiveDsc->startLiveRangeFromEmitter(varLocation, m_Compiler->GetEmitter());
}

void CodeGen::VariableLiveKeeper::psiClosePrologVariableRanges()
{
    noway_assert(m_LiveArgsCount <= m_LiveDscCount);

    for (unsigned int varNum = 0; varNum < m_LiveArgsCount; varNum++)
    {
        VariableLiveDescriptor* varLiveDsc = m_vlrLiveDscForProlog + varNum;

        if (varLiveDsc->hasVariableLiveRangeOpen())
        {
            varLiveDsc->endLiveRangeAtEmitter(m_Compiler->GetEmitter());
        }
    }
}

void CodeGen::siBeginBlock(BasicBlock* block, unsigned* nextEnterScope, unsigned* nextExitScope)
{
    assert(compiler->opts.compScopeInfo);

    if (compiler->info.compVarScopesCount == 0)
    {
        return;
    }

#ifdef FEATURE_EH_FUNCLETS
    if (siInFuncletRegion)
    {
        return;
    }

    if (block->bbFlags & BBF_FUNCLET_BEG)
    {
        // For now, don't report any scopes in funclets. JIT64 doesn't.
        siInFuncletRegion = true;

        JITDUMP("Scope info: found beginning of funclet region at block " FMT_BB "; ignoring following blocks\n",
                block->bbNum);

        return;
    }
#endif // FEATURE_EH_FUNCLETS

#ifdef DEBUG
    if (verbose)
    {
        printf("\nScope info: begin block " FMT_BB ", IL range ", block->bbNum);
        block->dspBlockILRange();
        printf("\n");
    }
#endif // DEBUG

    unsigned beginOffs = block->bbCodeOffs;

    if (beginOffs == BAD_IL_OFFSET)
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
        siOpenScopesForNonTrackedVars(block, siLastEndOffs, nextEnterScope, nextExitScope);
    }
}

void CodeGen::siOpenScopesForNonTrackedVars(const BasicBlock* block,
                                            unsigned          lastBlockILEndOffset,
                                            unsigned*         nextEnterScope,
                                            unsigned*         nextExitScope)
{
    unsigned beginOffs = block->bbCodeOffs;

    if (compiler->opts.OptimizationEnabled())
    {
        return;
    }

#ifdef FEATURE_EH_FUNCLETS
    // If we find a spot where the code offset isn't what we expect, because
    // there is a gap, it might be because we've moved the funclets out of
    // line. Catch up with the enter and exit scopes of the current block.
    // Ignore the enter/exit scope changes of the missing scopes, which for
    // funclets must be matched.
    if (lastBlockILEndOffset != beginOffs)
    {
        assert(beginOffs > 0);
        assert(lastBlockILEndOffset < beginOffs);

        JITDUMP("Scope info: found offset hole. lastOffs=%u, currOffs=%u\n", lastBlockILEndOffset, beginOffs);

        // Skip enter & exit scopes
        while (VarScopeDsc* scope = compiler->compGetNextEnterScopeScan(beginOffs - 1, nextEnterScope))
        {
            JITDUMP("Scope info: skipping enter scope, LVnum=%u\n", scope->scopeNum);
        }

        while (VarScopeDsc* scope = compiler->compGetNextExitScopeScan(beginOffs - 1, nextExitScope))
        {
            JITDUMP("Scope info: skipping exit scope, LVnum=%u\n", scope->scopeNum);
        }
    }
#else  // !FEATURE_EH_FUNCLETS
    if (lastBlockILEndOffset != beginOffs)
    {
        assert(lastBlockILEndOffset < beginOffs);
        return;
    }
#endif // !FEATURE_EH_FUNCLETS

    // When there we are jitting methods compiled in debug mode, no variable is
    // tracked and there is no info that shows variable liveness like block->bbLiveIn.
    // On debug code variables are not enregistered the whole method so we can just
    // report them as beign born from here on the stack until the whole method is
    // generated.

    while (VarScopeDsc* scope = compiler->compGetNextEnterScope(beginOffs, nextEnterScope))
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(scope->lclNum);

        if (!compiler->opts.compDbgCode && (lcl->GetRefCount() == 0))
        {
            JITDUMP("Skipping open scope for V%02u, unreferenced\n", scope->lclNum);

            continue;
        }

        JITDUMP("Scope info: opening scope, LVnum=%u [%03X..%03X)\n", scope->scopeNum, scope->startOffset,
                scope->endOffset);

        varLiveKeeper->siStartVariableLiveRange(lcl, scope->lclNum);

        INDEBUG(assert(!lcl->lvTracked || VarSetOps::IsMember(compiler, block->bbLiveIn, lcl->lvVarIndex)));
    }
}

void CodeGen::siEndBlock(BasicBlock* block)
{
    assert(compiler->opts.compScopeInfo && (compiler->info.compVarScopesCount > 0));

#ifdef FEATURE_EH_FUNCLETS
    if (siInFuncletRegion)
    {
        return;
    }
#endif

    unsigned endOffs = block->bbCodeOffsEnd;

    if (endOffs == BAD_IL_OFFSET)
    {
        JITDUMP("Scope info: ignoring block end\n");
        return;
    }

    siLastEndOffs = endOffs;
}

void CodeGen::psiBegProlog()
{
    assert(generatingProlog);

    unsigned nextEnterScope = 0;

    while (VarScopeDsc* scope = compiler->compGetNextEnterScope(0, &nextEnterScope))
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(scope->lclNum);

        if (!lcl->IsParam())
        {
            continue;
        }

        siVarLoc loc;

        if (lcl->IsRegParam())
        {
            RegNum regs[2]{lcl->GetParamReg(), REG_NA};

#ifdef UNIX_AMD64_ABI
            if (lcl->GetParamRegCount() > 1)
            {
                regs[1] = lcl->GetParamReg(1);
            }
#endif

            assert(isValidIntArgReg(regs[0]) || isValidFloatArgReg(regs[0]));
            assert((regs[1] == REG_NA) || isValidIntArgReg(regs[1]) || isValidFloatArgReg(regs[1]));

            loc.storeVariableInRegisters(regs[0], regs[1]);
        }
        else
        {
            loc.storeVariableOnStack(REG_SPBASE, psiGetVarStackOffset(lcl));
        }

        varLiveKeeper->psiStartVariableLiveRange(loc, scope->lclNum);
    }
}

#ifdef DEBUG
void CodeGen::VariableLiveKeeper::VariableLiveDescriptor::dumpAllRegisterLiveRangesForBlock(
    emitter* emit, const CodeGenInterface* codeGen) const
{
    bool first = true;
    for (LiveRangeListIterator it = m_VariableLiveRanges->begin(); it != m_VariableLiveRanges->end(); it++)
    {
        if (!first)
        {
            printf("; ");
        }
        it->dumpVariableLiveRange(emit, codeGen);
        first = false;
    }
}

void CodeGen::VariableLiveKeeper::VariableLiveDescriptor::dumpRegisterLiveRangesForBlockBeforeCodeGenerated(
    const CodeGenInterface* codeGen) const
{
    bool first = true;
    for (LiveRangeListIterator it = m_VariableLifeBarrier->getStartForDump(); it != m_VariableLiveRanges->end(); it++)
    {
        if (!first)
        {
            printf("; ");
        }
        it->dumpVariableLiveRange(codeGen);
        first = false;
    }
}

bool CodeGen::VariableLiveKeeper::VariableLiveDescriptor::hasVarLiveRangesToDump() const
{
    return !m_VariableLiveRanges->empty();
}

bool CodeGen::VariableLiveKeeper::VariableLiveDescriptor::hasVarLiveRangesFromLastBlockToDump() const
{
    return m_VariableLifeBarrier->hasLiveRangesToDump();
}

void CodeGen::VariableLiveKeeper::VariableLiveDescriptor::endBlockLiveRanges()
{
    m_VariableLifeBarrier->resetDumper(m_VariableLiveRanges);
}

void CodeGen::VariableLiveKeeper::VariableLiveRange::dumpVariableLiveRange(const CodeGenInterface* codeGen) const
{
    codeGen->dumpSiVarLoc(&m_VarLocation);

    printf(" [");
    m_StartEmitLocation.Print(codeGen->GetCompiler()->compMethodID);
    printf(", ");
    if (m_EndEmitLocation.Valid())
    {
        m_EndEmitLocation.Print(codeGen->GetCompiler()->compMethodID);
    }
    else
    {
        printf("...");
    }
    printf("]");
}

void CodeGen::VariableLiveKeeper::VariableLiveRange::dumpVariableLiveRange(emitter*                emit,
                                                                           const CodeGenInterface* codeGen) const
{
    assert(emit != nullptr);

    codeGen->dumpSiVarLoc(&m_VarLocation);

    UNATIVE_OFFSET endAssemblyOffset = m_EndEmitLocation.Valid() ? m_EndEmitLocation.CodeOffset(emit) : -1;
    printf(" [%X, %X)", m_StartEmitLocation.CodeOffset(emit), m_EndEmitLocation.CodeOffset(emit));
}

void CodeGen::VariableLiveKeeper::LiveRangeDumper::resetDumper(const LiveRangeList* liveRanges)
{
    assert(m_hasLiveRangestoDump);

    if (liveRanges->back().m_EndEmitLocation.Valid())
    {
        m_hasLiveRangestoDump = false;
    }
    else
    {
        m_StartingLiveRange = liveRanges->backPosition();
    }
}

void CodeGen::VariableLiveKeeper::LiveRangeDumper::setDumperStartAt(const LiveRangeListIterator liveRangeIt)
{
    m_hasLiveRangestoDump = true;
    m_StartingLiveRange   = liveRangeIt;
}

CodeGen::VariableLiveKeeper::LiveRangeListIterator CodeGen::VariableLiveKeeper::LiveRangeDumper::getStartForDump() const
{
    return m_StartingLiveRange;
}

bool CodeGen::VariableLiveKeeper::LiveRangeDumper::hasLiveRangesToDump() const
{
    return m_hasLiveRangestoDump;
}

void CodeGen::VariableLiveKeeper::dumpBlockVariableLiveRanges(const BasicBlock* block)
{
    assert(block != nullptr);

    bool hasDumpedHistory = false;

    printf("\nVariable Live Range History Dump for " FMT_BB "\n", block->bbNum);

    for (unsigned int varNum = 0; varNum < m_LiveDscCount; varNum++)
    {
        VariableLiveDescriptor* varLiveDsc = m_vlrLiveDsc + varNum;

        if (varLiveDsc->hasVarLiveRangesFromLastBlockToDump())
        {
            hasDumpedHistory = true;
            m_Compiler->gtDispLclVar(varNum, false);
            printf(": ");
            varLiveDsc->dumpRegisterLiveRangesForBlockBeforeCodeGenerated(m_Compiler->codeGen);
            varLiveDsc->endBlockLiveRanges();
            printf("\n");
        }
    }

    if (!hasDumpedHistory)
    {
        printf("..None..\n");
    }
}

void CodeGen::VariableLiveKeeper::dumpLvaVariableLiveRanges() const
{
    bool hasDumpedHistory = false;

    printf("VARIABLE LIVE RANGES:\n");

    for (unsigned int varNum = 0; varNum < m_LiveDscCount; varNum++)
    {
        VariableLiveDescriptor* varLiveDsc = m_vlrLiveDsc + varNum;

        if (varLiveDsc->hasVarLiveRangesToDump())
        {
            hasDumpedHistory = true;
            m_Compiler->gtDispLclVar(varNum, false);
            printf(": ");
            varLiveDsc->dumpAllRegisterLiveRangesForBlock(m_Compiler->GetEmitter(), m_Compiler->codeGen);
            printf("\n");
        }
    }

    if (!hasDumpedHistory)
    {
        printf("..None..\n");
    }
}
#endif // DEBUG
