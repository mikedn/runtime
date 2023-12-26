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

//------------------------------------------------------------------------
//                      VariableLiveDescriptor
//------------------------------------------------------------------------

CodeGen::VariableLiveKeeper::VariableLiveDescriptor::VariableLiveDescriptor(CompAllocator allocator)
    : m_VariableLiveRanges(new (allocator) LiveRangeList(allocator))
#ifdef DEBUG
    , m_VariableLifeBarrier(new (allocator) LiveRangeDumper(m_VariableLiveRanges))
#endif
{
}

//------------------------------------------------------------------------
// hasVariableLiveRangeOpen: Return true if the variable is still alive,
//  false in other case.
//
bool CodeGen::VariableLiveKeeper::VariableLiveDescriptor::hasVariableLiveRangeOpen() const
{
    return !m_VariableLiveRanges->empty() && !m_VariableLiveRanges->back().m_EndEmitLocation.Valid();
}

//------------------------------------------------------------------------
// getLiveRanges: Return the list of variable locations for this variable.
//
// Return Value:
//  A const LiveRangeList* pointing to the first variable location if it has
//  any or the end of the list in other case.
//
CodeGen::VariableLiveKeeper::LiveRangeList* CodeGen::VariableLiveKeeper::VariableLiveDescriptor::getLiveRanges() const
{
    return m_VariableLiveRanges;
}

//------------------------------------------------------------------------
// startLiveRangeFromEmitter: Report this variable as being born in "varLocation"
//  since the instruction where "emit" is located.
//
// Arguments:
//  varLocation  - the home of the variable.
//  emit - an emitter* instance located at the first instruction from
//  where "varLocation" becomes valid.
//
// Assumptions:
//  This variable is being born so it should be dead.
//
// Notes:
//  The position of "emit" matters to ensure intervals inclusive of the
//  beginning and exclusive of the end.
//
void CodeGen::VariableLiveKeeper::VariableLiveDescriptor::startLiveRangeFromEmitter(
    CodeGenInterface::siVarLoc varLocation, emitter* emit) const
{
    noway_assert(emit != nullptr);

    // Is the first "VariableLiveRange" or the previous one has been closed so its "m_EndEmitLocation" is valid
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
#endif // DEBUG

    // startEmitLocationendEmitLocation has to be Valid and endEmitLocationendEmitLocation  not
    noway_assert(m_VariableLiveRanges->back().m_StartEmitLocation.Valid());
    noway_assert(!m_VariableLiveRanges->back().m_EndEmitLocation.Valid());
}

//------------------------------------------------------------------------
// endLiveRangeAtEmitter: Report this variable as becoming dead since the
//  instruction where "emit" is located.
//
// Arguments:
//  emit - an emitter* instance located at the first instruction from
//   this variable becomes dead.
//
// Assumptions:
//  This variable is becoming dead so it should be alive.
//
// Notes:
//  The position of "emit" matters to ensure intervals inclusive of the
//  beginning and exclusive of the end.
//
void CodeGen::VariableLiveKeeper::VariableLiveDescriptor::endLiveRangeAtEmitter(emitter* emit) const
{
    noway_assert(emit != nullptr);
    noway_assert(hasVariableLiveRangeOpen());

    // Using [close, open) ranges so as to not compute the size of the last instruction
    m_VariableLiveRanges->back().m_EndEmitLocation.CaptureLocation(emit);

    // No m_EndEmitLocation has to be Valid
    noway_assert(m_VariableLiveRanges->back().m_EndEmitLocation.Valid());
}

//------------------------------------------------------------------------
// UpdateLiveRangeAtEmitter: Report this variable as changing its variable
//  home to "varLocation" since the instruction where "emit" is located.
//
// Arguments:
//  varLocation  - the new variable location.
//  emit - an emitter* instance located at the first instruction from
//   where "varLocation" becomes valid.
//
// Assumptions:
//  This variable is being born so it should be dead.
//
// Notes:
//  The position of "emit" matters to ensure intervals inclusive of the
//  beginning and exclusive of the end.
//
void CodeGen::VariableLiveKeeper::VariableLiveDescriptor::updateLiveRangeAtEmitter(
    CodeGenInterface::siVarLoc varLocation, emitter* emit) const
{
    // This variable is changing home so it has been started before during this block
    noway_assert(m_VariableLiveRanges != nullptr && !m_VariableLiveRanges->empty());

    // And its last m_EndEmitLocation has to be invalid
    noway_assert(!m_VariableLiveRanges->back().m_EndEmitLocation.Valid());

    // If we are reporting again the same home, that means we are doing something twice?
    // noway_assert(! CodeGenInterface::siVarLoc::Equals(&m_VariableLiveRanges->back().m_VarLocation, varLocation));

    // Close previous live range
    endLiveRangeAtEmitter(emit);

    startLiveRangeFromEmitter(varLocation, emit);
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

// Returns true if a live range for this variable has been recorded
bool CodeGen::VariableLiveKeeper::VariableLiveDescriptor::hasVarLiveRangesToDump() const
{
    return !m_VariableLiveRanges->empty();
}

// Returns true if a live range for this variable has been recorded from last call to EndBlock
bool CodeGen::VariableLiveKeeper::VariableLiveDescriptor::hasVarLiveRangesFromLastBlockToDump() const
{
    return m_VariableLifeBarrier->hasLiveRangesToDump();
}

// Reset the barrier so as to dump only next block changes on next block
void CodeGen::VariableLiveKeeper::VariableLiveDescriptor::endBlockLiveRanges()
{
    // make "m_VariableLifeBarrier->m_StartingLiveRange" now points to nullptr for printing purposes
    m_VariableLifeBarrier->resetDumper(m_VariableLiveRanges);
}
#endif // DEBUG

//------------------------------------------------------------------------
//                      VariableLiveKeeper
//------------------------------------------------------------------------
// Initialize structures for VariableLiveRanges
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
        // Allocate memory for "m_vlrLiveDsc" and initialize each "VariableLiveDescriptor"
        m_vlrLiveDsc          = allocator.allocate<VariableLiveDescriptor>(m_LiveDscCount);
        m_vlrLiveDscForProlog = allocator.allocate<VariableLiveDescriptor>(m_LiveDscCount);

        for (unsigned int varNum = 0; varNum < m_LiveDscCount; varNum++)
        {
            new (m_vlrLiveDsc + varNum) VariableLiveDescriptor(allocator);
            new (m_vlrLiveDscForProlog + varNum) VariableLiveDescriptor(allocator);
        }
    }
}

//------------------------------------------------------------------------
// siStartOrCloseVariableLiveRange: Reports the given variable as beign born
//  or becoming dead.
//
// Arguments:
//    varDsc    - the variable for which a location changed will be reported
//    varNum    - the index of the variable in the "compiler->lvaTable"
//    isBorn    - whether the variable is being born from where the emitter is located.
//    isDying   - whether the variable is dying from where the emitter is located.
//
// Assumptions:
//    The emitter should be located on the first instruction from where is true that
//    the variable becoming valid (when isBorn is true) or invalid (when isDying is true).
//
// Notes:
//    This method is being called when the variable is being born,
//    becoming dead, or both.
//
void CodeGen::VariableLiveKeeper::siStartOrCloseVariableLiveRange(const LclVarDsc* varDsc,
                                                                  unsigned int     varNum,
                                                                  bool             isBorn,
                                                                  bool             isDying)
{
    noway_assert(varDsc != nullptr);

    // Only the variables that exists in the IL, "this", and special arguments
    // are reported.
    if (varNum < m_LiveDscCount)
    {
        if (isBorn && !isDying)
        {
            // "varDsc" is valid from this point
            siStartVariableLiveRange(varDsc, varNum);
        }
        if (isDying && !isBorn)
        {
            // this variable live range is no longer valid from this point
            siEndVariableLiveRange(varNum);
        }
    }
}

//------------------------------------------------------------------------
// siStartVariableLiveRange: Reports the given variable as being born.
//
// Arguments:
//    varDsc    - the variable for which a location changed will be reported
//    varNum    - the index of the variable to report home in lvLiveDsc
//
// Assumptions:
//    The emitter should be pointing to the first instruction from where the VariableLiveRange is
//    becoming valid.
//    The given "varDsc" should have its VariableRangeLists initialized.
//
// Notes:
//    This method should be called on every place a Variable is becoming alive.
void CodeGen::VariableLiveKeeper::siStartVariableLiveRange(const LclVarDsc* varDsc, unsigned int varNum)
{
    noway_assert(varDsc != nullptr);

    // Only the variables that exists in the IL, "this", and special arguments
    // are reported.
    if (varNum < m_LiveDscCount)
    {
        // Build siVarLoc for this born "varDsc"
        CodeGenInterface::siVarLoc varLocation =
            m_Compiler->codeGen->getSiVarLoc(varDsc
#if !FEATURE_FIXED_OUT_ARGS
                                             ,
                                             m_Compiler->codeGen->getCurrentStackLevel()
#endif
                                                 );

        VariableLiveDescriptor* varLiveDsc = &m_vlrLiveDsc[varNum];
        // this variable live range is valid from this point
        varLiveDsc->startLiveRangeFromEmitter(varLocation, m_Compiler->GetEmitter());
    }
}

//------------------------------------------------------------------------
// siEndVariableLiveRange: Reports the variable as becoming dead.
//
// Arguments:
//    varNum    - the index of the variable at m_vlrLiveDsc or lvaTable in that
//       is becoming dead.
//
// Assumptions:
//    The given variable should be alive.
//    The emitter should be pointing to the first instruction from where the VariableLiveRange is
//    becoming invalid.
//
// Notes:
//    This method should be called on every place a Variable is becoming dead.
void CodeGen::VariableLiveKeeper::siEndVariableLiveRange(unsigned int varNum)
{
    // Only the variables that exists in the IL, "this", and special arguments
    // will be reported.

    // This method is being called from genUpdateLife, and that one is called after
    // code for BasicBlock have been generated, but the emitter has no longer
    // a valid IG so we don't report the close of a "VariableLiveRange" after code is
    // emitted.

    if (varNum < m_LiveDscCount && !m_LastBasicBlockHasBeenEmited)
    {
        // this variable live range is no longer valid from this point
        m_vlrLiveDsc[varNum].endLiveRangeAtEmitter(m_Compiler->GetEmitter());
    }
}

//------------------------------------------------------------------------
// siUpdateVariableLiveRange: Reports the change of variable location for the
//  given variable.
//
// Arguments:
//    varDsc    - the variable for which tis home has changed.
//    varNum    - the index of the variable to report home in lvLiveDsc
//
// Assumptions:
//    The given variable should be alive.
//    The emitter should be pointing to the first instruction from where
//    the new variable location is becoming valid.
//
void CodeGen::VariableLiveKeeper::siUpdateVariableLiveRange(const LclVarDsc* varDsc, unsigned int varNum)
{
    noway_assert(varDsc != nullptr);

    // Only the variables that exists in the IL, "this", and special arguments
    // will be reported. This are locals and arguments, and are counted in
    // "info.compLocalsCount".

    // This method is being called when the prolog is being generated, and
    // the emitter has no longer a valid IG so we don't report the close of
    //  a "VariableLiveRange" after code is emitted.
    if (varNum < m_LiveDscCount && !m_LastBasicBlockHasBeenEmited)
    {
        // Build the location of the variable
        CodeGenInterface::siVarLoc siVarLoc =
            m_Compiler->codeGen->getSiVarLoc(varDsc
#if !FEATURE_FIXED_OUT_ARGS
                                             ,
                                             m_Compiler->codeGen->getCurrentStackLevel()
#endif
                                                 );

        // Report the home change for this variable
        VariableLiveDescriptor* varLiveDsc = &m_vlrLiveDsc[varNum];
        varLiveDsc->updateLiveRangeAtEmitter(siVarLoc, m_Compiler->GetEmitter());
    }
}

//------------------------------------------------------------------------
// siEndAllVariableLiveRange: Reports the set of variables as becoming dead.
//
// Arguments:
//    newLife    - the set of variables that are becoming dead.
//
// Assumptions:
//    All the variables in the set are alive.
//
// Notes:
//    This method is called when the last block being generated to killed all
//    the live variables and set a flag to avoid reporting variable locations for
//    on next calls to method that update variable liveness.
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
            // It seems we are jitting debug code, so we don't have variable
            //  liveness info
            siEndAllVariableLiveRange();
        }
    }

    m_LastBasicBlockHasBeenEmited = true;
}

//------------------------------------------------------------------------
// siEndAllVariableLiveRange: Reports all live variables as dead.
//
// Notes:
//    This overload exists for the case we are jitting code compiled in
//    debug mode. When that happen we don't have variable liveness info
//    as "BaiscBlock::bbLiveIn" or "BaiscBlock::bbLiveOut" and there is no
//    tracked variable.
//
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

//------------------------------------------------------------------------
// getLiveRangesForVarForBody: Return the "VariableLiveRange" that correspond to
//  the given "varNum".
//
// Arguments:
//  varNum  - the index of the variable in m_vlrLiveDsc, which is the same as
//      in lvaTable.
//
// Return Value:
//  A const pointer to the list of variable locations reported for the variable.
//
// Assumptions:
//  This variable should be an argument, a special argument or an IL local
//  variable.
CodeGen::VariableLiveKeeper::LiveRangeList* CodeGen::VariableLiveKeeper::getLiveRangesForVarForBody(
    unsigned int varNum) const
{
    // There should be at least one variable for which its liveness is tracked
    noway_assert(varNum < m_LiveDscCount);

    return m_vlrLiveDsc[varNum].getLiveRanges();
}

//------------------------------------------------------------------------
// getLiveRangesForVarForProlog: Return the "VariableLiveRange" that correspond to
//  the given "varNum".
//
// Arguments:
//  varNum  - the index of the variable in m_vlrLiveDsc, which is the same as
//      in lvaTable.
//
// Return Value:
//  A const pointer to the list of variable locations reported for the variable.
//
// Assumptions:
//  This variable should be an argument, a special argument or an IL local
//  variable.
CodeGen::VariableLiveKeeper::LiveRangeList* CodeGen::VariableLiveKeeper::getLiveRangesForVarForProlog(
    unsigned int varNum) const
{
    // There should be at least one variable for which its liveness is tracked
    noway_assert(varNum < m_LiveDscCount);

    return m_vlrLiveDscForProlog[varNum].getLiveRanges();
}

//------------------------------------------------------------------------
// getLiveRangesCount: Returns the count of variable locations reported for the tracked
//  variables, which are arguments, special arguments, and local IL variables.
//
// Return Value:
//    size_t - the count of variable locations
//
// Notes:
//    This method is being called from "genSetScopeInfo" to know the count of
//    "varResultInfo" that should be created on eeSetLVcount.
//
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

//------------------------------------------------------------------------
// psiStartVariableLiveRange: Reports the given variable as being born.
//
// Arguments:
//  varLcation  - the variable location
//  varNum      - the index of the variable in "compiler->lvaTable" or
//      "VariableLivekeeper->m_vlrLiveDsc"
//
// Notes:
//  This function is expected to be called from "psiBegProlog" during
//  prolog code generation.
//
void CodeGen::VariableLiveKeeper::psiStartVariableLiveRange(CodeGenInterface::siVarLoc varLocation, unsigned int varNum)
{
    // This descriptor has to correspond to a parameter. The first slots in lvaTable
    // are arguments and special arguments.
    noway_assert(varNum < m_LiveArgsCount);

    VariableLiveDescriptor* varLiveDsc = &m_vlrLiveDscForProlog[varNum];
    varLiveDsc->startLiveRangeFromEmitter(varLocation, m_Compiler->GetEmitter());
}

//------------------------------------------------------------------------
// psiClosePrologVariableRanges: Report all the parameters as becoming dead.
//
// Notes:
//  This function is expected to be called from preffix "psiEndProlog" after
//  code for prolog has been generated.
//
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

#ifdef DEBUG
//------------------------------------------------------------------------
//                      VariableLiveRanges dumpers
//------------------------------------------------------------------------

// Dump "VariableLiveRange" when code has not been generated and we don't have so the assembly native offset
// but at least "emitLocation"s and "siVarLoc"
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

// Dump "VariableLiveRange" when code has been generated and we have the assembly native offset of each "emitLocation"
void CodeGen::VariableLiveKeeper::VariableLiveRange::dumpVariableLiveRange(emitter*                emit,
                                                                           const CodeGenInterface* codeGen) const
{
    assert(emit != nullptr);

    // "VariableLiveRanges" are created setting its location ("m_VarLocation") and the initial native offset
    // ("m_StartEmitLocation")
    codeGen->dumpSiVarLoc(&m_VarLocation);

    // If this is an open "VariableLiveRange", "m_EndEmitLocation" is non-valid and print -1
    UNATIVE_OFFSET endAssemblyOffset = m_EndEmitLocation.Valid() ? m_EndEmitLocation.CodeOffset(emit) : -1;

    printf(" [%X, %X)", m_StartEmitLocation.CodeOffset(emit), m_EndEmitLocation.CodeOffset(emit));
}

//------------------------------------------------------------------------
//                      LiveRangeDumper
//------------------------------------------------------------------------
//------------------------------------------------------------------------
// resetDumper: If the the "liveRange" has its last "VariableLiveRange" closed, it makes
//  the "LiveRangeDumper" points to end of "liveRange" (nullptr). In other case,
//  it makes the "LiveRangeDumper" points to the last "VariableLiveRange" of
//  "liveRange", which is opened.
//
// Arguments:
//  liveRanges - the "LiveRangeList" of the "VariableLiveDescriptor" we want to
//      udpate its "LiveRangeDumper".
//
// Notes:
//  This method is expected to be called once a the code for a BasicBlock has been
//  generated and all the new "VariableLiveRange"s of the variable during this block
//  has been dumped.
void CodeGen::VariableLiveKeeper::LiveRangeDumper::resetDumper(const LiveRangeList* liveRanges)
{
    // There must have reported something in order to reset
    assert(m_hasLiveRangestoDump);

    if (liveRanges->back().m_EndEmitLocation.Valid())
    {
        // the last "VariableLiveRange" is closed and the variable
        // is no longer alive
        m_hasLiveRangestoDump = false;
    }
    else
    {
        // the last "VariableLiveRange" remains opened because it is
        // live at "BasicBlock"s "bbLiveOut".
        m_StartingLiveRange = liveRanges->backPosition();
    }
}

//------------------------------------------------------------------------
// setDumperStartAt: Make "LiveRangeDumper" instance points the last "VariableLiveRange"
// added so we can starts dumping from there after the actual "BasicBlock"s code is generated.
//
// Arguments:
//  liveRangeIt - an iterator to a position in "VariableLiveDescriptor::m_VariableLiveRanges"
//
// Return Value:
//  A const pointer to the "LiveRangeList" containing all the "VariableLiveRange"s
//  of the variable with index "varNum".
//
// Notes:
//  "varNum" should be always a valid inde ("varnum" < "m_LiveDscCount")
void CodeGen::VariableLiveKeeper::LiveRangeDumper::setDumperStartAt(const LiveRangeListIterator liveRangeIt)
{
    m_hasLiveRangestoDump = true;
    m_StartingLiveRange   = liveRangeIt;
}

//------------------------------------------------------------------------
// getStartForDump: Return an iterator to the first "VariableLiveRange" edited/added
//  during the current "BasicBlock"
//
// Return Value:
//  A LiveRangeListIterator to the first "VariableLiveRange" in "LiveRangeList" which
//  was used during last "BasicBlock".
//
CodeGen::VariableLiveKeeper::LiveRangeListIterator CodeGen::VariableLiveKeeper::LiveRangeDumper::getStartForDump() const
{
    return m_StartingLiveRange;
}

//------------------------------------------------------------------------
// hasLiveRangesToDump: Retutn wheter at least a "VariableLiveRange" was alive during
//  the current "BasicBlock"'s code generation
//
// Return Value:
//  A boolean indicating indicating if there is at least a "VariableLiveRange"
//  that has been used for the variable during last "BasicBlock".
//
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
