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

                codeGen->getVariableLiveKeeper()->EndRange(lclNum);
            }
            else
            {
                codeGen->getVariableLiveKeeper()->StartRange(lcl, lclNum);
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

            codeGen->getVariableLiveKeeper()->StartOrCloseRange(lcl, lclNode->GetLclNum(), isBorn, isDying);
        }
    }

    if (spill)
    {
        // TODO-MIKE-Review: There's something dubious going on here, or perhaps in LSRA. On ARM64 a last-use
        // gets spilled and that results in an assert in "variable range". The range was already closed above
        // and SpillRegCandidateLclVar tries to update it for spill. It may be that SpillRegCandidateLclVar
        // needs to use StartOrCloseRange instead of UpdateRange in this case but then it's not clear why
        // would a last-use need spilling to begin with.
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

CodeGen::VariableLiveDescriptor::VariableLiveDescriptor(CompAllocator allocator)
    : ranges(new (allocator) VariableLiveRangeList(allocator))
#ifdef DEBUG
    , dumper(new (allocator) VariableLiveRangeDumper(ranges))
#endif
{
}

bool CodeGen::VariableLiveDescriptor::HasOpenRange() const
{
    return !ranges->empty() && !ranges->back().endOffset.Valid();
}

CodeGen::VariableLiveRangeList* CodeGen::VariableLiveDescriptor::GetRanges() const
{
    return ranges;
}

void CodeGen::VariableLiveDescriptor::StartRange(siVarLoc varLoc, emitter* emit) const
{
    noway_assert(ranges->empty() || ranges->back().endOffset.Valid());

    if (!ranges->empty() && siVarLoc::Equals(varLoc, ranges->back().location) &&
        ranges->back().endOffset.IsPreviousInsNum(emit))
    {
        JITDUMP("Extending debug range...\n");

        // The variable is being born just after the instruction at which it died.
        // In this case, i.e. an update of the variable's value, we coalesce the live ranges.
        ranges->back().endOffset.Init();
    }
    else
    {
        JITDUMP("New debug range: %s\n", ranges->empty() ? "first" : siVarLoc::Equals(varLoc, ranges->back().location)
                                                                         ? "new var or location"
                                                                         : "not adjacent");

        // Creates new live range with invalid end
        ranges->emplace_back(varLoc, emitLocation(), emitLocation());
        ranges->back().startOffset.CaptureLocation(emit);
    }

#ifdef DEBUG
    if (!dumper->HasRangesToDump())
    {
        dumper->SetDumpStart(ranges->backPosition());
    }
#endif

    noway_assert(ranges->back().startOffset.Valid());
    noway_assert(!ranges->back().endOffset.Valid());
}

void CodeGen::VariableLiveDescriptor::EndRange(emitter* emit) const
{
    noway_assert(HasOpenRange());

    // Using [close, open) ranges so as to not compute the size of the last instruction
    ranges->back().endOffset.CaptureLocation(emit);

    noway_assert(ranges->back().endOffset.Valid());
}

void CodeGen::VariableLiveDescriptor::UpdateRange(siVarLoc varLoc, emitter* emit) const
{
    noway_assert(!ranges->empty() && !ranges->back().endOffset.Valid());

    // If we are reporting again the same home, that means we are doing something twice?
    // noway_assert(!siVarLoc::Equals(m_ranges->back().m_location, varLoc));

    EndRange(emit);
    StartRange(varLoc, emit);
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
    : compiler(comp)
    , varCount(comp->opts.compDbgInfo ? comp->info.compLocalsCount : 0)
    , paramCount(comp->opts.compDbgInfo ? comp->info.compArgsCount : 0)
{
    assert(paramCount <= varCount);

    if (varCount == 0)
    {
        return;
    }

    bodyVars   = allocator.allocate<VariableLiveDescriptor>(varCount);
    prologVars = allocator.allocate<VariableLiveDescriptor>(varCount);

    for (unsigned i = 0; i < varCount; i++)
    {
        new (&bodyVars[i]) VariableLiveDescriptor(allocator);
        new (&prologVars[i]) VariableLiveDescriptor(allocator);
    }
}

void CodeGen::VariableLiveKeeper::StartOrCloseRange(const LclVarDsc* lcl, unsigned lclNum, bool isBorn, bool isDying)
{
    if (lclNum >= varCount)
    {
        return;
    }

    if (isBorn && !isDying)
    {
        StartRange(lcl, lclNum);
    }

    if (isDying && !isBorn)
    {
        EndRange(lclNum);
    }
}

void CodeGen::VariableLiveKeeper::StartRange(const LclVarDsc* lcl, unsigned lclNum)
{
    if (lclNum < varCount)
    {
        bodyVars[lclNum].StartRange(compiler->codeGen->getSiVarLoc(lcl), compiler->GetEmitter());
    }
}

void CodeGen::VariableLiveKeeper::EndRange(unsigned lclNum)
{
    if ((lclNum < varCount) && !lastBasicBlockHasBeenEmited)
    {
        bodyVars[lclNum].EndRange(compiler->GetEmitter());
    }
}

void CodeGen::VariableLiveKeeper::UpdateRange(const LclVarDsc* lcl, unsigned lclNum)
{
    if (lclNum < varCount && !lastBasicBlockHasBeenEmited)
    {
        bodyVars[lclNum].UpdateRange(compiler->codeGen->getSiVarLoc(lcl), compiler->GetEmitter());
    }
}

void CodeGen::VariableLiveKeeper::EndAllRanges(VARSET_VALARG_TP varsToClose)
{
    if (varCount != 0)
    {
        if ((compiler->lvaTrackedCount > 0) || !compiler->opts.OptimizationDisabled())
        {
            for (VarSetOps::Enumerator en(compiler, varsToClose); en.MoveNext();)
            {
                EndRange(compiler->lvaTrackedIndexToLclNum(en.Current()));
            }
        }
        else
        {
            // We are jitting debug code, so we don't have variable liveness info.
            EndAllRanges();
        }
    }

    lastBasicBlockHasBeenEmited = true;
}

void CodeGen::VariableLiveKeeper::EndAllRanges()
{
    for (unsigned i = 0; i < varCount; i++)
    {
        if (bodyVars[i].HasOpenRange())
        {
            EndRange(i);
        }
    }
}

CodeGen::VariableLiveRangeList* CodeGen::VariableLiveKeeper::GetBodyRanges(unsigned lclNum) const
{
    noway_assert(lclNum < varCount);

    return bodyVars[lclNum].GetRanges();
}

CodeGen::VariableLiveRangeList* CodeGen::VariableLiveKeeper::GetPrologRanges(unsigned lclNum) const
{
    noway_assert(lclNum < varCount);

    return prologVars[lclNum].GetRanges();
}

unsigned CodeGen::VariableLiveKeeper::GetRangeCount() const
{
    unsigned count = 0;

    for (unsigned lclNum = 0; lclNum < varCount; lclNum++)
    {
        if (compiler->compMap2ILvarNum(lclNum) != static_cast<unsigned>(ICorDebugInfo::UNKNOWN_ILNUM))
        {
            count += static_cast<unsigned>(bodyVars[lclNum].GetRanges()->size());
            count += static_cast<unsigned>(prologVars[lclNum].GetRanges()->size());
        }
    }

    return count;
}

void CodeGen::VariableLiveKeeper::StartPrologRange(siVarLoc varLoc, unsigned lclNum)
{
    noway_assert(lclNum < paramCount);

    prologVars[lclNum].StartRange(varLoc, compiler->GetEmitter());
}

void CodeGen::VariableLiveKeeper::EndPrologRange()
{
    for (unsigned i = 0; i < paramCount; i++)
    {
        if (prologVars[i].HasOpenRange())
        {
            prologVars[i].EndRange(compiler->GetEmitter());
        }
    }
}

void CodeGen::VariableLiveKeeper::BeginBlock(BasicBlock* block, unsigned* nextEnterScope, unsigned* nextExitScope)
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
        StartUntrackedVarsRanges(block, nextEnterScope, nextExitScope);
    }
}

void CodeGen::VariableLiveKeeper::StartUntrackedVarsRanges(const BasicBlock* block,
                                                           unsigned*         nextEnterScope,
                                                           unsigned*         nextExitScope)
{
    if (compiler->opts.OptimizationEnabled())
    {
        return;
    }

    unsigned startILOffset = block->bbCodeOffs;

#ifdef FEATURE_EH_FUNCLETS
    // If we find a spot where the code offset isn't what we expect, because
    // there is a gap, it might be because we've moved the funclets out of
    // line. Catch up with the enter and exit scopes of the current block.
    // Ignore the enter/exit scope changes of the missing scopes, which for
    // funclets must be matched.
    if (lastBlockEndILOffset != startILOffset)
    {
        assert(startILOffset > 0);
        assert(lastBlockEndILOffset < startILOffset);

        JITDUMP("Scope info: found offset hole. lastOffs=%u, currOffs=%u\n", lastBlockEndILOffset, startILOffset);

        // Skip enter & exit scopes
        while (VarScopeDsc* scope = compiler->compGetNextEnterScopeScan(startILOffset - 1, nextEnterScope))
        {
            JITDUMP("Scope info: skipping enter scope, LVnum=%u\n", scope->scopeNum);
        }

        while (VarScopeDsc* scope = compiler->compGetNextExitScopeScan(startILOffset - 1, nextExitScope))
        {
            JITDUMP("Scope info: skipping exit scope, LVnum=%u\n", scope->scopeNum);
        }
    }
#else  // !FEATURE_EH_FUNCLETS
    if (lastBlockEndILOffset != startILOffset)
    {
        assert(lastBlockEndILOffset < startILOffset);
        return;
    }
#endif // !FEATURE_EH_FUNCLETS

    // When there we are jitting methods compiled in debug mode, no variable is
    // tracked and there is no info that shows variable liveness like block->bbLiveIn.
    // On debug code variables are not enregistered the whole method so we can just
    // report them as beign born from here on the stack until the whole method is
    // generated.

    while (VarScopeDsc* scope = compiler->compGetNextEnterScope(startILOffset, nextEnterScope))
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(scope->lclNum);

        if (!compiler->opts.compDbgCode && (lcl->GetRefCount() == 0))
        {
            JITDUMP("Skipping open scope for V%02u, unreferenced\n", scope->lclNum);

            continue;
        }

        JITDUMP("Scope info: opening scope, LVnum=%u [%03X..%03X)\n", scope->scopeNum, scope->startOffset,
                scope->endOffset);

        StartRange(lcl, scope->lclNum);

        assert(!lcl->HasLiveness() || VarSetOps::IsMember(compiler, block->bbLiveIn, lcl->GetLivenessBitIndex()));
    }
}

void CodeGen::VariableLiveKeeper::EndBlock(BasicBlock* block)
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

void CodeGen::VariableLiveKeeper::BeginProlog(CodeGen* codeGen)
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
            loc.storeVariableOnStack(REG_SPBASE, GetVarStackOffset(lcl, codeGen));
        }

        StartPrologRange(loc, scope->lclNum);
    }
}

int32_t CodeGen::VariableLiveKeeper::GetVarStackOffset(const LclVarDsc* lcl, CodeGen* codeGen) const
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
void CodeGen::VariableLiveDescriptor::DumpNewRanges() const
{
    for (auto it = dumper->GetDumpStart(); it != ranges->end(); it++)
    {
        if (it != dumper->GetDumpStart())
        {
            printf("; ");
        }

        it->Dump();
    }

    dumper->Reset(ranges);
}

bool CodeGen::VariableLiveDescriptor::HasNewRangesToDump() const
{
    return dumper->HasRangesToDump();
}

void CodeGen::VariableLiveRange::Dump() const
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

    printf("]");
}

void CodeGen::VariableLiveRangeDumper::Reset(const VariableLiveRangeList* ranges)
{
    assert(hasRangestoDump);

    if (ranges->back().endOffset.Valid())
    {
        hasRangestoDump = false;
    }
    else
    {
        startingRange = ranges->backPosition();
    }
}

void CodeGen::VariableLiveRangeDumper::SetDumpStart(const VariableLiveRangeListIterator start)
{
    hasRangestoDump = true;
    startingRange   = start;
}

CodeGen::VariableLiveRangeListIterator CodeGen::VariableLiveRangeDumper::GetDumpStart() const
{
    return startingRange;
}

bool CodeGen::VariableLiveRangeDumper::HasRangesToDump() const
{
    return hasRangestoDump;
}

void CodeGen::VariableLiveKeeper::DumpNewRanges(const BasicBlock* block)
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
#endif // DEBUG
