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
#include "lsra.h"
#include "sideeffects.h"

class Lowering
{
    Compiler*     comp;
    SideEffectSet m_scratchSideEffects; // SideEffectSet used for IsSafeToContainMem and isRMWIndirCandidate
    BasicBlock*   m_block;
    LclVarDsc*    vtableCallTempLcl = nullptr; // local variable we use as a temp for vtable calls
#if FEATURE_FIXED_OUT_ARGS
    unsigned outgoingArgAreaSize = 0;
#endif
#ifdef FEATURE_HW_INTRINSICS
#ifdef TARGET_ARM64
    LclVarDsc* m_simd8MemoryTempLcl = nullptr;
#endif
    LclVarDsc* m_simd16MemoryTempLcl = nullptr;
#ifdef TARGET_XARCH
    LclVarDsc* m_simd32MemoryTempLcl = nullptr;
#endif
#endif // FEATURE_HW_INTRINSICS

public:
    Lowering(Compiler* compiler) : comp(compiler)
    {
    }

    void Run();

    void LowerNode(BasicBlock* block, GenTree* node)
    {
        m_block = block;
        LowerNode(node);
    }

private:
    void ContainCheckDivOrMod(GenTreeOp* node);
    void ContainCheckReturnTrap(GenTreeOp* node);
    void ContainCheckArrOffset(GenTreeArrOffs* node);
    void LowerLclHeap(GenTreeUnOp* node);
    void ContainCheckRet(GenTreeUnOp* ret);
    void ContainCheckJTrue(GenTreeUnOp* node);
    void ContainCheckCallOperands(GenTreeCall* call);
    void ContainCheckIndir(GenTreeIndir* indirNode);
    void ContainCheckIndStore(GenTreeIndStore* store);
    void ContainCheckMul(GenTreeOp* node);
    void ContainCheckShiftRotate(GenTreeOp* node);
    void ContainCheckStoreLcl(GenTreeLclVarCommon* store);
    void ContainCheckCast(GenTreeCast* node);
    void ContainCheckCompare(GenTreeOp* cmp);
    void ContainCheckBinary(GenTreeOp* node);
    void ContainCheckBoundsChk(GenTreeBoundsChk* node);
#ifdef TARGET_XARCH
    void ContainCheckFloatBinary(GenTreeOp* node);
    void ContainCheckIntrinsic(GenTreeIntrinsic* node);
    void ContainCheckXAdd(GenTreeOp* node);
#endif
#ifdef FEATURE_SIMD
    bool ContainSIMD12MemToMemCopy(GenTree* store, GenTree* value);
#endif
#ifdef FEATURE_HW_INTRINSICS
    void ContainCheckHWIntrinsicAddr(GenTreeHWIntrinsic* node, GenTree* addr);
    void ContainCheckHWIntrinsic(GenTreeHWIntrinsic* node);
#ifdef TARGET_XARCH
    void ContainHWIntrinsicOperand(GenTreeHWIntrinsic* node, GenTree* op);
#endif
#endif

#ifdef DEBUG
    void CheckCallArg(GenTree* arg);
    void CheckCall(GenTreeCall* call);
    void CheckNode(GenTree* node);
    bool CheckBlock(BasicBlock* block);
#endif

    void LowerBlock(BasicBlock* block);
    GenTree* LowerNode(GenTree* node);

    void LowerCall(GenTreeCall* call);
#ifndef TARGET_64BIT
    GenTree* DecomposeLongCompare(GenTreeOp* cmp);
#endif
#ifndef TARGET_ARM64
    GenTree* OptimizeConstCompare(GenTreeOp* cmp);
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
    GenTree* LowerDelegateInvoke(GenTreeCall* call X86_ARG(GenTree* insertBefore = nullptr));
    GenTree* LowerDirectCall(GenTreeCall* call X86_ARG(GenTree* insertBefore = nullptr));
    GenTree* LowerDirectPInvokeCall(GenTreeCall* call);
    GenTree* ExpandConstLookupCallTarget(const CORINFO_CONST_LOOKUP& entryPoint,
                                         GenTree* insertBefore DEBUGARG(GenTreeCall* call));
#ifdef TARGET_X86
    void LowerTailCallViaJitHelper(GenTreeCall* call);
#endif
#if FEATURE_FASTTAILCALL
    void LowerFastTailCall(GenTreeCall* callNode);
#endif
    void RehomeParamForFastTailCall(LclVarDsc* paramLcl,
                                    GenTree*   insertTempBefore,
                                    GenTree*   rangeStart,
                                    GenTree*   rangeEnd);
    void InsertProfTailCallHook(GenTreeCall* callNode, GenTree* insertionPoint);
    GenTree* LowerVirtualVtableCall(GenTreeCall* call X86_ARG(GenTree* insertBefore = nullptr));
    void LowerIndirectVirtualStubCall(GenTreeCall* call);
    GenTree* LowerVirtualStubCall(GenTreeCall* call X86_ARG(GenTree* insertBefore = nullptr));
    void LowerCallArgs(GenTreeCall* call);
    GenTree* InsertPutArg(GenTreeCall* call, CallArgInfo* argInfo);
    GenTree* InsertPutArgReg(GenTree* arg, CallArgInfo* argInfo, unsigned regIndex);
    void LowerCallArg(GenTreeCall* call, CallArgInfo* argInfo);

    void InsertPInvokeCallPrologAndEpilog(GenTreeCall* call);
    void InsertPInvokeCallProlog(GenTreeCall* call);
    void InsertPInvokeCallEpilog(GenTreeCall* call);
    void InsertPInvokeMethodProlog();
    void InsertPInvokeMethodEpilog(INDEBUG(GenTree* lastExpr));
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
    GenTreeLclVar* ReplaceWithLclVar(LIR::Use& use, LclVarDsc* tempLcl = nullptr);

    // return true if this call target is within range of a pc-rel call on the machine
    bool IsCallTargetInRange(void* addr);

#ifdef TARGET_XARCH
    GenTree* PreferredRegOptionalOperand(GenTreeOp* tree);
    void SetRegOptionalForBinOp(GenTreeOp* tree, bool isSafeToMarkOp1, bool isSafeToMarkOp2);
#endif

    void LowerIndir(GenTreeIndir* ind);
    void LowerIndStore(GenTreeIndStore* store);
    void LowerIndStoreArch(GenTreeIndStore* store);
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
    bool IsIndLoadRMWCandidate(GenTreeIndStore* store, GenTreeIndir* load, GenTree* src);
    GenTreeIndir* IsStoreIndRMW(GenTreeIndStore* store);
    void LowerStoreIndRMW(GenTreeIndStore* store);
    static bool IndirsAreRMWEquivalent(GenTreeIndir* indir1, GenTreeIndir* indir2);
    static bool LeavesAreRMWEquivalent(GenTree* node1, GenTree* node2);

private:
#endif

#ifdef FEATURE_SIMD
    void WidenSIMD12IfNecessary(GenTreeLclVar* node);
    bool CanWidenSimd12ToSimd16(const LclVarDsc* lcl);
#endif
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
    LclVarDsc* GetSimdMemoryTemp(var_types type);
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
    bool IsImmOperand(GenTree* operand, GenTree* instr) const;

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

    void MakeSrcContained(GenTree* instr, GenTree* operand) const;
    bool ContainImmOperand(GenTree* instr, GenTree* operand) const;

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
