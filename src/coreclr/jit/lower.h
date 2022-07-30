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
    Lowering(Compiler* compiler) : Phase(compiler, PHASE_LOWERING)
    {
    }

    virtual PhaseStatus DoPhase() override;

    void LowerNode(BasicBlock* block, GenTree* node)
    {
        m_block = block;
        LowerNode(node);
    }

private:
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
#ifdef TARGET_XARCH
    void ContainHWIntrinsicOperand(GenTreeHWIntrinsic* node, GenTree* op);
#endif
#endif // FEATURE_HW_INTRINSICS

#ifdef DEBUG
    static void CheckCallArg(GenTree* arg);
    static void CheckCall(GenTreeCall* call);
    static void CheckNode(Compiler* compiler, GenTree* node);
    static bool CheckBlock(Compiler* compiler, BasicBlock* block);
#endif // DEBUG

    void LowerBlock(BasicBlock* block);
    GenTree* LowerNode(GenTree* node);

    void LowerCall(GenTreeCall* call);
#ifndef TARGET_64BIT
    GenTree* DecomposeLongCompare(GenTree* cmp);
#endif
#ifndef TARGET_ARM64
    GenTree* OptimizeConstCompare(GenTree* cmp);
    GenTree* LowerCompare(GenTreeOp* cmp);
#endif
    GenTree* LowerJTrue(GenTreeUnOp* jtrue);
#ifdef TARGET_XARCH
    GenTreeCC* LowerNodeCC(GenTree* node, GenCondition condition);
#endif
    void LowerJmpMethod(GenTree* jmp);
    void LowerReturn(GenTreeUnOp* ret);
    void LowerLclVar(GenTreeLclVar* lclVar);
    void LowerStoreLclVar(GenTreeLclVar* store);
    void LowerStoreLclVarArch(GenTreeLclVar* store);
    void LowerLclFld(GenTreeLclFld* lclFld);
    void LowerStoreLclFld(GenTreeLclFld* store);
    void LowerStructReturn(GenTreeUnOp* ret);
    void LowerRetSingleRegStructLclVar(GenTreeUnOp* ret);
    void LowerStructCall(GenTreeCall* call);
    GenTree* SpillStructCall(GenTreeCall* call, GenTree* user);
    GenTree* LowerDelegateInvoke(GenTreeCall* call);
    GenTree* LowerDirectCall(GenTreeCall* call);
    GenTree* LowerPInvokeCall(GenTreeCall* call);
    GenTree* ExpandConstLookupCallTarget(const CORINFO_CONST_LOOKUP& entryPoint DEBUGARG(GenTreeCall* call));
#ifdef TARGET_X86
    void LowerTailCallViaJitHelper(GenTreeCall* call);
#endif
#if FEATURE_FASTTAILCALL
    void LowerFastTailCall(GenTreeCall* callNode);
#endif
    void RehomeParamForFastTailCall(unsigned paramLclNum,
                                    GenTree* insertTempBefore,
                                    GenTree* rangeStart,
                                    GenTree* rangeEnd);
    void InsertProfTailCallHook(GenTreeCall* callNode, GenTree* insertionPoint);
    GenTree* LowerVirtualVtableCall(GenTreeCall* call);
    void LowerVirtualStubCallIndirect(GenTreeCall* call);
    GenTree* LowerVirtualStubCall(GenTreeCall* call);
    void LowerCallArgs(GenTreeCall* call);
    GenTree* InsertPutArg(GenTreeCall* call, CallArgInfo* argInfo);
    GenTree* InsertPutArgReg(GenTree* arg, CallArgInfo* argInfo, unsigned regIndex);
    void LowerCallArg(GenTreeCall* call, CallArgInfo* argInfo);

    void InsertPInvokeCallProlog(GenTreeCall* call);
    void InsertPInvokeCallEpilog(GenTreeCall* call);
    void InsertPInvokeMethodProlog();
    void InsertPInvokeMethodEpilog(BasicBlock* returnBB DEBUGARG(GenTree* lastExpr));
    void InsertSetGCState(GenTree* before, int cns);
    void InsertReturnTrap(GenTree* before);
    enum FrameLinkAction
    {
        PushFrame,
        PopFrame
    };
    void InsertFrameLinkUpdate(LIR::Range& block, GenTree* before, FrameLinkAction action);

    // Replace the definition of the given use with a lclVar, allocating a new temp
    // if 'tempNum' is BAD_VAR_NUM. Returns the LclVar node.
    GenTreeLclVar* ReplaceWithLclVar(LIR::Use& use, unsigned tempNum = BAD_VAR_NUM)
    {
        GenTree* def = use.Def();

        if (def->OperIs(GT_LCL_VAR) && (tempNum == BAD_VAR_NUM))
        {
            return def->AsLclVar();
        }

        GenTree* assign;
        use.ReplaceWithLclVar(comp, tempNum, &assign);

        GenTreeLclVar* newDef = use.Def()->AsLclVar();
        ContainCheckRange(def->gtNext, newDef);

        // We need to lower the LclVar and assignment since there may be certain
        // types or scenarios, such as TYP_SIMD12, that need special handling
        LowerNode(assign);
        LowerNode(newDef);

        return newDef;
    }

    // return true if this call target is within range of a pc-rel call on the machine
    bool IsCallTargetInRange(void* addr);

#ifdef TARGET_XARCH
    GenTree* PreferredRegOptionalOperand(GenTree* tree);
    void SetRegOptionalForBinOp(GenTree* tree, bool isSafeToMarkOp1, bool isSafeToMarkOp2);
#endif

    void LowerIndir(GenTreeIndir* ind);
    void LowerStoreIndir(GenTreeStoreInd* store);
    void LowerStoreIndirArch(GenTreeStoreInd* store);
    GenTree* LowerAdd(GenTreeOp* node);
    bool LowerUnsignedDivOrMod(GenTreeOp* divMod);
    GenTree* LowerConstIntDivOrMod(GenTree* node);
    GenTree* LowerSignedDivOrMod(GenTree* node);
    void LowerStructStore(GenTree* store, StructStoreKind kind, ClassLayout* layout);
    void LowerStoreObj(GenTreeObj* store);
    void LowerStoreBlk(GenTreeBlk* store);
    void ContainStructStoreAddress(GenTree* store, unsigned size, GenTree* addr);
    void ContainStructStoreAddressUnrollRegsWB(GenTree* addr);
    void LowerPutArgStk(GenTreePutArgStk* tree);

#ifdef TARGET_ARM64
    void LowerNot(GenTreeUnOp* node);
    void CombineNot(GenTreeInstr* instr);
    void LowerLogical(GenTreeOp* logical);
    void LowerNegate(GenTreeUnOp* neg);
    void LowerFloatNegate(GenTreeUnOp* neg);
    void LowerFloatArithmetic(GenTreeOp* arith);
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

    bool TryTransformStoreObjToStoreInd(GenTreeObj* store);

    GenTree* LowerSwitch(GenTreeUnOp* node);
    bool TryLowerSwitchToBitTest(
        BasicBlock* jumpTable[], unsigned jumpCount, unsigned targetCount, BasicBlock* bbSwitch, GenTree* switchValue);

    GenTree* LowerBitCast(GenTreeUnOp* bitcast);
    GenTree* LowerCast(GenTreeCast* cast);

#ifdef TARGET_XARCH
    bool IsLoadIndRMWCandidate(GenTreeStoreInd* store, GenTreeIndir* load, GenTree* src);
    GenTreeIndir* IsStoreIndRMW(GenTreeStoreInd* store);
    void LowerStoreIndRMW(GenTreeStoreInd* store);
    static bool IndirsAreEquivalent(GenTreeIndir* indir1, GenTreeIndir* indir2);
    static bool NodesAreEquivalentLeaves(GenTree* node1, GenTree* node2);

private:
#endif

    void WidenSIMD12IfNecessary(GenTreeLclVarCommon* node);
#if FEATURE_MULTIREG_RET
    void MakeMultiRegStoreLclVar(GenTreeLclVar* store, GenTree* value);
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
    // return true if 'childNode' is an immediate that can be contained
    //  by the 'parentNode' (i.e. folded into an instruction)
    //  for example small enough and non-relocatable
    bool IsContainableImmed(GenTree* parentNode, GenTree* childNode) const;

    static bool IsContainableMemoryOp(Compiler* comp, GenTree* node);

    bool IsContainableMemoryOp(GenTree* node)
    {
        return IsContainableMemoryOp(comp, node);
    }

#ifdef FEATURE_HW_INTRINSICS
    static bool IsContainableHWIntrinsicOp(Compiler*           comp,
                                           GenTreeHWIntrinsic* containingNode,
                                           GenTree*            node,
                                           bool*               supportsRegOptional);

    bool IsContainableHWIntrinsicOp(GenTreeHWIntrinsic* containingNode, GenTree* node, bool* supportsRegOptional)
    {
        return IsContainableHWIntrinsicOp(comp, containingNode, node, supportsRegOptional);
    }
#endif

    static void TransformUnusedIndirection(GenTreeIndir* ind);

private:
    bool AreSourcesPossiblyModifiedLocals(GenTree* addr, GenTree* base, GenTree* index);

    // Makes 'childNode' contained in the 'parentNode'
    void MakeSrcContained(GenTree* parentNode, GenTree* childNode) const;

    // Checks and makes 'childNode' contained in the 'parentNode'
    bool CheckImmedAndMakeContained(GenTree* parentNode, GenTree* childNode);

    // Checks for memory conflicts in the instructions between childNode and parentNode, and returns true if childNode
    // can be contained.
    bool IsSafeToContainMem(GenTree* parentNode, GenTree* childNode)
    {
        return IsSafeToMoveForward(childNode, parentNode);
    }

    bool IsSafeToMoveForward(GenTree* move, GenTree* before);

    inline LIR::Range& BlockRange() const
    {
        return LIR::AsRange(m_block);
    }

    INDEBUG(void CheckAllLocalsImplicitlyReferenced();)
};

#endif // _LOWER_H_
