// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                               Lower                                       XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#ifndef _LOWER_H_
#define _LOWER_H_

#include "compiler.h"
#include "phase.h"
#include "lsra.h"
#include "sideeffects.h"

class Lowering final : public Phase
{
    LinearScan*   m_lsra;
    SideEffectSet m_scratchSideEffects; // SideEffectSet used for IsSafeToContainMem and isRMWIndirCandidate
    BasicBlock*   m_block;
    unsigned      vtableCallTemp = BAD_VAR_NUM; // local variable we use as a temp for vtable calls
#ifdef FEATURE_HW_INTRINSICS
#ifdef TARGET_ARM64
    unsigned m_simd8MemoryTemp = BAD_VAR_NUM;
#endif
    unsigned m_simd16MemoryTemp = BAD_VAR_NUM;
#ifdef TARGET_XARCH
    unsigned m_simd32MemoryTemp = BAD_VAR_NUM;
#endif
#endif // FEATURE_HW_INTRINSICS

public:
    inline Lowering(Compiler* compiler, LinearScanInterface* lsra)
        : Phase(compiler, PHASE_LOWERING), m_lsra(static_cast<LinearScan*>(lsra))
    {
        assert(m_lsra != nullptr);
    }

    virtual PhaseStatus DoPhase() override;

    // This variant of LowerRange is called from outside of the main Lowering pass,
    // so it creates its own instance of Lowering to do so.
    void LowerRange(BasicBlock* block, LIR::ReadOnlyRange& range)
    {
        Lowering lowerer(comp, m_lsra);
        lowerer.m_block = block;

        lowerer.LowerRange(range);
    }

private:
    // LowerRange handles new code that is introduced by or after Lowering.
    void LowerRange(LIR::ReadOnlyRange& range)
    {
        for (GenTree* newNode : range)
        {
            LowerNode(newNode);
        }
    }
    void LowerRange(GenTree* firstNode, GenTree* lastNode)
    {
        LIR::ReadOnlyRange range(firstNode, lastNode);
        LowerRange(range);
    }

    // ContainCheckRange handles new code that is introduced by or after Lowering,
    // and that is known to be already in Lowered form.
    void ContainCheckRange(LIR::ReadOnlyRange& range)
    {
        for (GenTree* newNode : range)
        {
            ContainCheckNode(newNode);
        }
    }
    void ContainCheckRange(GenTree* firstNode, GenTree* lastNode)
    {
        LIR::ReadOnlyRange range(firstNode, lastNode);
        ContainCheckRange(range);
    }

    void InsertTreeBeforeAndContainCheck(GenTree* insertionPoint, GenTree* tree)
    {
        LIR::Range range = LIR::SeqTree(comp, tree);
        ContainCheckRange(range);
        BlockRange().InsertBefore(insertionPoint, std::move(range));
    }

    void ContainCheckNode(GenTree* node);

    void ContainCheckDivOrMod(GenTreeOp* node);
    void ContainCheckReturnTrap(GenTreeOp* node);
    void ContainCheckArrOffset(GenTreeArrOffs* node);
    void ContainCheckLclHeap(GenTreeOp* node);
    void ContainCheckRet(GenTreeUnOp* ret);
    void ContainCheckJTrue(GenTreeUnOp* node);

    void ContainCheckCallOperands(GenTreeCall* call);
    void ContainCheckIndir(GenTreeIndir* indirNode);
    void ContainCheckStoreIndir(GenTreeStoreInd* store);
    void ContainCheckMul(GenTreeOp* node);
    void ContainCheckShiftRotate(GenTreeOp* node);
    void ContainCheckStoreLcl(GenTreeLclVarCommon* store);
    void ContainCheckCast(GenTreeCast* node);
    void ContainCheckCompare(GenTreeOp* node);
    void ContainCheckBinary(GenTreeOp* node);
    void ContainCheckBoundsChk(GenTreeBoundsChk* node);
#ifdef TARGET_XARCH
    void ContainCheckFloatBinary(GenTreeOp* node);
    void ContainCheckIntrinsic(GenTreeOp* node);
#endif // TARGET_XARCH
#ifdef FEATURE_SIMD
    bool ContainSIMD12MemToMemCopy(GenTree* store, GenTree* value);
#endif // FEATURE_SIMD
#ifdef FEATURE_HW_INTRINSICS
    void ContainCheckHWIntrinsicAddr(GenTreeHWIntrinsic* node, GenTree* addr);
    void ContainCheckHWIntrinsic(GenTreeHWIntrinsic* node);
#endif // FEATURE_HW_INTRINSICS

#ifdef DEBUG
    static void CheckCallArg(GenTree* arg);
    static void CheckCall(GenTreeCall* call);
    static void CheckNode(Compiler* compiler, GenTree* node);
    static bool CheckBlock(Compiler* compiler, BasicBlock* block);
#endif // DEBUG

    void LowerBlock(BasicBlock* block);
    GenTree* LowerNode(GenTree* node);

    // ------------------------------
    // Call Lowering
    // ------------------------------
    void LowerCall(GenTree* call);
#ifndef TARGET_64BIT
    GenTree* DecomposeLongCompare(GenTree* cmp);
#endif
#ifndef TARGET_ARM64
    GenTree* OptimizeConstCompare(GenTree* cmp);
    GenTree* LowerCompare(GenTree* cmp);
#endif
    GenTree* LowerJTrue(GenTreeUnOp* jtrue);
    GenTreeCC* LowerNodeCC(GenTree* node, GenCondition condition);
    void LowerJmpMethod(GenTree* jmp);
    void LowerRet(GenTreeUnOp* ret);
    void LowerLclVar(GenTreeLclVar* lclVar);
    void LowerStoreLclVar(GenTreeLclVar* store);
    void LowerStoreLclVarArch(GenTreeLclVar* store);
    void LowerLclFld(GenTreeLclFld* lclFld);
    void LowerStoreLclFld(GenTreeLclFld* store);
    void LowerRetStruct(GenTreeUnOp* ret);
    void LowerRetSingleRegStructLclVar(GenTreeUnOp* ret);
    void LowerCallStruct(GenTreeCall* call);
    void LowerStoreSingleRegCallStruct(GenTreeObj* store);
#if !defined(WINDOWS_AMD64_ABI)
    GenTreeLclVar* SpillStructCallResult(GenTreeCall* call);
#endif // WINDOWS_AMD64_ABI
    GenTree* LowerDelegateInvoke(GenTreeCall* call);
    GenTree* LowerIndirectNonvirtCall(GenTreeCall* call);
    GenTree* LowerDirectCall(GenTreeCall* call);
    GenTree* LowerNonvirtPinvokeCall(GenTreeCall* call);
    GenTree* LowerTailCallViaJitHelper(GenTreeCall* callNode, GenTree* callTarget);
    void LowerFastTailCall(GenTreeCall* callNode);
    void RehomeParamForFastTailCall(unsigned paramLclNum,
                                    GenTree* insertTempBefore,
                                    GenTree* rangeStart,
                                    GenTree* rangeEnd);
    void InsertProfTailCallHook(GenTreeCall* callNode, GenTree* insertionPoint);
    GenTree* LowerVirtualVtableCall(GenTreeCall* call);
    GenTree* LowerVirtualStubCall(GenTreeCall* call);
    void LowerCallArgs(GenTreeCall* call);
    GenTree* InsertPutArg(GenTreeCall* call, CallArgInfo* argInfo);
    GenTree* InsertPutArgReg(GenTree* arg, CallArgInfo* argInfo, unsigned regIndex);
    void LowerCallArg(GenTreeCall* call, CallArgInfo* argInfo);

    void InsertPInvokeCallProlog(GenTreeCall* call);
    void InsertPInvokeCallEpilog(GenTreeCall* call);
    void InsertPInvokeMethodProlog();
    void InsertPInvokeMethodEpilog(BasicBlock* returnBB DEBUGARG(GenTree* lastExpr));
    GenTreeStoreInd* SetGCState(int cns);
    GenTree* CreateReturnTrapSeq();
    enum FrameLinkAction
    {
        PushFrame,
        PopFrame
    };
    GenTreeStoreInd* CreateFrameLinkUpdate(FrameLinkAction);
    GenTree* AddrGen(ssize_t addr);
    GenTree* AddrGen(void* addr);

    GenTree* Ind(GenTree* tree, var_types type = TYP_I_IMPL)
    {
        return comp->gtNewOperNode(GT_IND, type, tree);
    }

    GenTree* PhysReg(regNumber reg, var_types type = TYP_I_IMPL)
    {
        return comp->gtNewPhysRegNode(reg, type);
    }

    GenTree* ThisReg(GenTreeCall* call)
    {
        return PhysReg(comp->codeGen->genGetThisArgReg(call), TYP_REF);
    }

    GenTree* Offset(GenTree* base, unsigned offset)
    {
        var_types resultType = (base->TypeGet() == TYP_REF) ? TYP_BYREF : base->TypeGet();
        return new (comp, GT_LEA) GenTreeAddrMode(resultType, base, nullptr, 0, offset);
    }

    GenTree* OffsetByIndex(GenTree* base, GenTree* index)
    {
        var_types resultType = (base->TypeGet() == TYP_REF) ? TYP_BYREF : base->TypeGet();
        return new (comp, GT_LEA) GenTreeAddrMode(resultType, base, index, 0, 0);
    }

    GenTree* OffsetByIndexWithScale(GenTree* base, GenTree* index, unsigned scale)
    {
        var_types resultType = (base->TypeGet() == TYP_REF) ? TYP_BYREF : base->TypeGet();
        return new (comp, GT_LEA) GenTreeAddrMode(resultType, base, index, scale, 0);
    }

    GenTree* NewStoreLclVar(unsigned lclNum, var_types type, GenTree* value)
    {
        LclVarDsc* lcl = comp->lvaGetDesc(lclNum);
        GenTree*   store;

        if (type == TYP_STRUCT)
        {
            assert(lcl->TypeIs(TYP_STRUCT));
            GenTree* addr = comp->gtNewLclVarAddrNode(lclNum);
            addr->gtFlags |= GTF_VAR_DEF;
            store = new (comp, GT_STORE_OBJ) GenTreeObj(TYP_STRUCT, addr, value, lcl->GetLayout());
            store->gtFlags |= GTF_IND_NONFAULTING;
            store->gtFlags &= ~GTF_GLOB_REF;
        }
        else
        {
            store = new (comp, GT_STORE_LCL_VAR) GenTreeLclVar(type, lclNum, value);
        }

        if (lcl->lvAddrExposed)
        {
            store->gtFlags |= GTF_GLOB_REF;
        }

        return store;
    }

    // Replace the definition of the given use with a lclVar, allocating a new temp
    // if 'tempNum' is BAD_VAR_NUM. Returns the LclVar node.
    GenTreeLclVar* ReplaceWithLclVar(LIR::Use& use, unsigned tempNum = BAD_VAR_NUM)
    {
        GenTree* def = use.Def();

        if (def->OperIs(GT_LCL_VAR) && (tempNum == BAD_VAR_NUM))
        {
            return def->AsLclVar();
        }

        use.ReplaceWithLclVar(comp, tempNum);

        GenTreeLclVar* newDef = use.Def()->AsLclVar();
        ContainCheckRange(def->gtNext, newDef);
        return newDef;
    }

    // return true if this call target is within range of a pc-rel call on the machine
    bool IsCallTargetInRange(void* addr);

#if defined(TARGET_XARCH)
    GenTree* PreferredRegOptionalOperand(GenTree* tree);

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
    // Note: On xarch at most only one of the operands will be marked as
    // reg optional, even when both operands could be considered register
    // optional.
    void SetRegOptionalForBinOp(GenTree* tree, bool isSafeToMarkOp1, bool isSafeToMarkOp2)
    {
        assert(GenTree::OperIsBinary(tree->OperGet()));

        GenTree* const op1 = tree->gtGetOp1();
        GenTree* const op2 = tree->gtGetOp2();

        const unsigned operatorSize = genTypeSize(tree->TypeGet());

        const bool op1Legal =
            isSafeToMarkOp1 && tree->OperIsCommutative() && (operatorSize == genTypeSize(op1->TypeGet()));
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
#endif // defined(TARGET_XARCH)

    // Per tree node member functions
    void LowerStoreIndirCommon(GenTreeStoreInd* ind);
    void LowerIndir(GenTreeIndir* ind);
    void LowerStoreIndir(GenTreeStoreInd* store);
    GenTree* LowerAdd(GenTreeOp* node);
    bool LowerUnsignedDivOrMod(GenTreeOp* divMod);
    GenTree* LowerConstIntDivOrMod(GenTree* node);
    GenTree* LowerSignedDivOrMod(GenTree* node);
    void LowerStructStore(GenTreeBlk* store);
    void LowerBlockStoreCommon(GenTreeBlk* blkNode);
    void ContainBlockStoreAddress(GenTree* store, unsigned size, GenTree* addr);
    void LowerPutArgStk(GenTreePutArgStk* tree);

#ifdef TARGET_ARM64
    void LowerNot(GenTreeUnOp* node);
    void CombineNot(GenTreeInstr* instr);
    void LowerLogical(GenTreeOp* logical);
    void LowerNegate(GenTreeUnOp* neg);
    void LowerArithmetic(GenTreeOp* arith);
    void LowerMultiply(GenTreeOp* mul);
    void LowerShiftImmediate(GenTreeOp* shift);
    void CombineShiftImmediate(GenTreeInstr* shift);
    void LowerShiftVariable(GenTreeOp* shift);
    GenTree* LowerRelop(GenTreeOp* relop);
    GenTree* OptimizeRelopImm(GenTreeOp* relop);
    GenTreeInstr* MakeInstr(GenTree* node, instruction ins, emitAttr size);
    GenTreeInstr* MakeInstr(GenTree* node, instruction ins, emitAttr size, GenTree* op1);
    GenTreeInstr* MakeInstr(GenTree* node, instruction ins, emitAttr size, GenTree* op1, GenTree* op2);
    GenTreeInstr* NewInstrBefore(GenTree* before, var_types type, instruction ins, GenTree* op1);
    GenTreeInstr* NewInstrAfter(GenTree* after, var_types type, instruction ins, GenTree* op1);
    GenTreeInstr* NewInstrBefore(GenTree* before, var_types type, instruction ins, GenTree* op1, GenTree* op2);
    INDEBUG(bool IsLegalToMoveUseForward(GenTree* oldUser, GenTree* newUser, GenTree* def);)
#endif

    bool TryCreateAddrMode(GenTree* addr, bool isContainable);

    bool TryTransformStoreObjAsStoreInd(GenTreeBlk* blkNode);

    GenTree* LowerSwitch(GenTreeUnOp* node);
    bool TryLowerSwitchToBitTest(
        BasicBlock* jumpTable[], unsigned jumpCount, unsigned targetCount, BasicBlock* bbSwitch, GenTree* switchValue);

    GenTree* LowerBitCast(GenTreeUnOp* bitcast);
    GenTree* LowerCast(GenTreeCast* cast);

#if !CPU_LOAD_STORE_ARCH
    bool IsRMWIndirCandidate(GenTree* operand, GenTree* storeInd);
    bool IsBinOpInRMWStoreInd(GenTree* tree);
    bool IsRMWMemOpRootedAtStoreInd(GenTree* storeIndTree, GenTree** indirCandidate, GenTree** indirOpSource);
    bool LowerRMWMemOp(GenTreeIndir* storeInd);
#endif

    void WidenSIMD12IfNecessary(GenTreeLclVarCommon* node);
#if FEATURE_MULTIREG_RET
    void MakeMultiRegLclVar(GenTreeLclVar* lclVar, const ReturnTypeDesc* retTypeDesc);
#endif
    GenTree* LowerArrElem(GenTree* node);
    void LowerRotate(GenTree* tree);
    void LowerShift(GenTreeOp* shift);
#ifdef FEATURE_HW_INTRINSICS
    void LowerHWIntrinsic(GenTreeHWIntrinsic* node);
    void LowerHWIntrinsicCC(GenTreeHWIntrinsic* node, NamedIntrinsic newIntrinsicId, GenCondition condition);
    void LowerHWIntrinsicEquality(GenTreeHWIntrinsic* node, genTreeOps cmpOp);
    void LowerHWIntrinsicCreateScalarUnsafe(GenTreeHWIntrinsic* node);
    void LowerHWIntrinsicCreate(GenTreeHWIntrinsic* node);
    void LowerHWIntrinsicCreateBroadcast(GenTreeHWIntrinsic* node);
    unsigned GetSimdMemoryTemp(var_types type);
#if defined(TARGET_XARCH)
#ifdef TARGET_X86
    void LowerHWIntrinsicCreateScalarUnsafeLong(GenTreeHWIntrinsic* node);
#endif
    void LowerFusedMultiplyAdd(GenTreeHWIntrinsic* node);
    void LowerHWIntrinsicSum128(GenTreeHWIntrinsic* node);
    void LowerHWIntrinsicSum256(GenTreeHWIntrinsic* node);
    void LowerHWIntrinsicGetElement(GenTreeHWIntrinsic* node);
    void LowerHWIntrinsicWithElement(GenTreeHWIntrinsic* node);
    void LowerHWIntrinsicInsertFloat(GenTreeHWIntrinsic* node);
    void ContainHWIntrinsicInsertFloat(GenTreeHWIntrinsic* node);
#elif defined(TARGET_ARM64)
    bool IsValidConstForMovImm(GenTreeHWIntrinsic* node);
    void LowerHWIntrinsicFusedMultiplyAddScalar(GenTreeHWIntrinsic* node);
    void LowerHWIntrinsicSum(GenTreeHWIntrinsic* node);
    void LowerHWIntrinsicGetElement(GenTreeHWIntrinsic* node);
    void LowerHWIntrinsicWithElement(GenTreeHWIntrinsic* node);
#endif // !TARGET_XARCH && !TARGET_ARM64

    struct VectorConstant
    {
        union {
            uint8_t  u8[32];
            uint16_t u16[16];
            uint32_t u32[8];
            uint64_t u64[4];
        };

        VectorConstant() : u64{}
        {
        }

        bool AllBitsZero(unsigned vectorByteSize) const;
        bool AllBitsOne(unsigned vectorByteSize) const;
        bool Insert(var_types type, int index, GenTree* value);
        bool Create(GenTreeHWIntrinsic* create);
        bool Broadcast(GenTreeHWIntrinsic* create);
    };

    void LowerHWIntrinsicCreateConst(GenTreeHWIntrinsic* create, const VectorConstant& vecConst);
    GenTree* TryRemoveCastIfPresent(var_types expectedType, GenTree* op);
#endif // FEATURE_HW_INTRINSICS

    // Utility functions
public:
    static bool IndirsAreEquivalent(GenTree* pTreeA, GenTree* pTreeB);

    // return true if 'childNode' is an immediate that can be contained
    //  by the 'parentNode' (i.e. folded into an instruction)
    //  for example small enough and non-relocatable
    bool IsContainableImmed(GenTree* parentNode, GenTree* childNode) const;

    // Return true if 'node' is a containable memory op.
    bool IsContainableMemoryOp(GenTree* node)
    {
        return m_lsra->isContainableMemoryOp(node);
    }

#ifdef FEATURE_HW_INTRINSICS
    // Return true if 'node' is a containable HWIntrinsic op.
    bool IsContainableHWIntrinsicOp(GenTreeHWIntrinsic* containingNode, GenTree* node, bool* supportsRegOptional);
#endif // FEATURE_HW_INTRINSICS

    static void TransformUnusedIndirection(GenTreeIndir* ind, Compiler* comp, BasicBlock* block);

private:
    static bool NodesAreEquivalentLeaves(GenTree* candidate, GenTree* storeInd);

    bool AreSourcesPossiblyModifiedLocals(GenTree* addr, GenTree* base, GenTree* index);

    // Makes 'childNode' contained in the 'parentNode'
    void MakeSrcContained(GenTree* parentNode, GenTree* childNode) const;

    // Checks and makes 'childNode' contained in the 'parentNode'
    bool CheckImmedAndMakeContained(GenTree* parentNode, GenTree* childNode);

    // Checks for memory conflicts in the instructions between childNode and parentNode, and returns true if childNode
    // can be contained.
    bool IsSafeToContainMem(GenTree* parentNode, GenTree* childNode);

    inline LIR::Range& BlockRange() const
    {
        return LIR::AsRange(m_block);
    }

    // Any tracked lclVar accessed by a LCL_FLD or STORE_LCL_FLD should be marked doNotEnregister.
    // This method checks, and asserts in the DEBUG case if it is not so marked,
    // but in the non-DEBUG case (asserts disabled) set the flag so that we don't generate bad code.
    // This ensures that the local's value is valid on-stack as expected for a *LCL_FLD.
    void verifyLclFldDoNotEnregister(unsigned lclNum)
    {
        LclVarDsc* varDsc = &(comp->lvaTable[lclNum]);
        // Do a couple of simple checks before setting lvDoNotEnregister.
        // This may not cover all cases in 'isRegCandidate()' but we don't want to
        // do an expensive check here. For non-candidates it is not harmful to set lvDoNotEnregister.
        if (varDsc->lvTracked && !varDsc->lvDoNotEnregister)
        {
            assert(!m_lsra->isRegCandidate(varDsc));
            comp->lvaSetVarDoNotEnregister(lclNum DEBUG_ARG(Compiler::DNER_LocalField));
        }
    }
};

#endif // _LOWER_H_
