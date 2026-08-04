// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "compiler.h"
#include "sideeffects.h"

class Lowering
{
    Compiler*     comp;
    SideEffectSet m_scratchSideEffects;
    BasicBlock*   m_block;
    LclVarDsc*    vtableCallTempLcl = nullptr;
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
    LclVarDsc* mdArrayLengthTempLcl = nullptr;
    LclVarDsc* mdArrayIndex1TempLcl = nullptr;
    LclVarDsc* mdArrayIndex2TempLcl = nullptr;

public:
    Lowering(Compiler* compiler) : comp(compiler)
    {
    }

    static void TransformUnusedIndirection(GenTreeIndir* ind);
#ifdef FEATURE_HW_INTRINSICS
    static bool IsHWIntrinsicMemOp(Compiler* comp, GenTreeHWIntrinsic* instr, GenTree* node, bool* supportsRegOptional);
#endif

    void Run();

    void LowerNode(BasicBlock* block, GenTree* node)
    {
        m_block = block;
        LowerNode(node);
    }

private:
    void LowerBlock(BasicBlock* block);
    GenTree* LowerNode(GenTree* node);

    void ContainCheckReturnTrap(GenTreeOp* node);
    void ContainCheckRet(GenTreeUnOp* ret);
    void ContainCheckJTrue(GenTreeUnOp* node);
    void ContainCheckIndir(GenTreeIndir* indirNode);
    void ContainCheckIndStore(GenTreeIndStore* store);
    void ContainCheckShiftRotate(GenTreeOp* node);
    void ContainCheckStoreLcl(GenTreeLclRef* store);
    void ContainCheckCompare(GenTreeOp* cmp);
    void ContainCheckBinary(GenTreeOp* node);
    void ContainCheckBoundsChk(GenTreeBoundsChk* node);
#ifdef TARGET_XARCH
    void ContainCheckDivRem(GenTreeOp* node);
    void ContainCheckMul(GenTreeOp* node);
    void ContainCheckCallAddr(GenTreeCall* call);
    void ContainCheckIntToFloat(GenTreeUnOp* node);
    void ContainCheckFloatToInt(GenTreeUnOp* node);
    void ContainCheckFloatBinary(GenTreeOp* node);
    void ContainCheckIntrinsic(GenTreeIntrinsic* node);
    void ContainCheckXAdd(GenTreeOp* node);
#endif
#ifdef TARGET_64BIT
    void ContainCheckIntExtend(GenTreeUnOp* node, GenTree* src);
#endif
#ifdef FEATURE_SIMD
    bool ContainSIMD12MemToMemCopy(GenTree* store, GenTree* value);
#endif

    void LowerLclLoad(GenTreeLclLoad* load);
    void LowerLclStore(GenTreeLclStore* store);
    void LowerLclStoreArch(GenTreeLclStore* store);
    void LowerLclLoadFld(GenTreeLclLoadFld* load);
    void LowerLclStoreFld(GenTreeLclStoreFld* store);
    void LowerLclHeap(GenTreeUnOp* node);
    GenTree* LowerArrElem(GenTreeArrElem* node);
    void LowerShift(GenTreeOp* shift);
    void LowerRotateRight(GenTreeOp* node);
#ifdef TARGET_XARCH
    void LowerRotateLeft(GenTreeOp* node);
    GenTree* LowerFloatConvert(GenTreeUnOp* node);
#endif
    GenTree* LowerKeepAlive(GenTreeUnOp* node);
    void LowerIndir(GenTreeIndir* ind);
    void LowerIndStore(GenTreeIndStore* store);
    void LowerIndStoreArch(GenTreeIndStore* store);
    GenTree* LowerAdd(GenTreeOp* node);
    GenTree* LowerBitCast(GenTreeUnOp* bitcast);
    void LowerOvfConv(GenTreeUnOp* node);
    void LowerOvfUnsigned(GenTreeUnOp* node);
    void LowerOvfTruncate(GenTreeUnOp* node);
    GenTree* LowerConv(GenTreeUnOp* cast);
    GenTree* LowerTruncate(GenTreeUnOp* node);
    void LowerIntToFloat(GenTreeUnOp* node);
    void LowerFloatToInt(GenTreeUnOp* node);
#ifdef TARGET_64BIT
    void LowerSignedExtend(GenTreeUnOp* node);
    void LowerUnsignedExtend(GenTreeUnOp* node);
#endif
#ifdef TARGET_ARM
    void LowerFloatMul(GenTreeOp* mul);
#endif
#ifndef USE_HELPERS_FOR_INT_DIV
    bool LowerUnsignedDivRem(GenTreeOp* divMod);
#endif
#ifndef TARGET_ARM64
    GenTree* LowerConstIntDivRem(GenTreeOp* node);
    GenTree* LowerSignedDivRem(GenTree* node);
#endif
    GenTree* LowerJTrue(GenTreeUnOp* jtrue);
    void LowerCall(GenTreeCall* call);
    void LowerJmp(GenTreeJmp* jmp);
    void LowerReturn(GenTreeUnOp* ret);
    void LowerStructReturn(GenTreeUnOp* ret);
    void LowerRetSingleRegStructLclVar(GenTreeUnOp* ret);
    void LowerStructCall(GenTreeCall* call);
    GenTree* SpillStructCall(GenTreeCall* call, GenTree* user);
    GenTree* LowerDelegateInvoke(GenTreeCall* call);
    GenTree* LowerDirectCall(GenTreeCall* call);
    GenTree* LowerDirectUnmanagedCall(GenTreeCall* call);
    GenTree* ExpandConstLookupCallTarget(const CORINFO_CONST_LOOKUP& entryPoint,
                                         GenTree* insertBefore DEBUGARG(GenTreeCall* call));
#ifdef TARGET_X86
    void LowerTailCallViaJitHelper(GenTreeCall* call);
#endif
#ifdef TARGET_XARCH
    GenTreeCC* LowerNodeCC(GenTree* node, GenCondition condition);
#endif
#ifndef TARGET_64BIT
    GenTree* DecomposeLongCompare(GenTreeOp* cmp);
#endif
#ifndef TARGET_ARM64
    GenTree* OptimizeConstCompare(GenTreeOp* cmp);
    GenTree* LowerCompare(GenTreeOp* cmp);
#endif
    void RemoveNonRegCallArgs(GenTreeCall* call);
#if FEATURE_FASTTAILCALL
    void LowerFastTailCall(GenTreeCall* call);
    void InsertProfTailCallHook(GenTreeCall* call, GenTree* startNonGCNode);
    void RehomeParamForFastTailCall(LclVarDsc* paramLcl,
                                    GenTree*   insertTempBefore,
                                    GenTree*   rangeStart,
                                    GenTree*   rangeEnd);
#endif
    GenTree* LowerVirtualVtableCall(GenTreeCall* call);
    GenTree* LowerIndirectVirtualStubCall(GenTreeCall* call);
    GenTree* LowerVirtualStubCall(GenTreeCall* call);
    void LowerCallArgs(GenTreeCall* call);
    void LowerCallArg(GenTreeCall* call, CallArgInfo* argInfo);
    void InsertPutArg(GenTreeCall* call, CallArgInfo* argInfo);
#ifndef TARGET_64BIT
    void InsertLongPutArg(GenTreeCall* call, CallArgInfo* argInfo);
#endif
    void InsertFieldListPutArg(GenTreeCall* call, CallArgInfo* argInfo);
    void InsertFieldListArgStore(GenTreeFieldList* fieldList, GenTreeCall* call, CallArgInfo* argInfo);
#if FEATURE_MULTIREG_ARGS
    void InsertFieldListArgReg(GenTreeFieldList* fieldList, GenTreeCall* call, CallArgInfo* argInfo);
#endif
#ifdef TARGET_ARMARCH
    void InsertFieldListArgSplit(GenTreeFieldList* fieldList, GenTreeCall* call, CallArgInfo* argInfo);
#endif
#ifdef TARGET_ARM
    void InsertPutArgSplit(GenTreeCall* call, CallArgInfo* argInfo);
#endif
    GenTree* InsertPutArgReg(GenTree* arg, CallArgInfo* argInfo, unsigned regIndex);
    GenTreeArgStore* NewArgStore(GenTree* value, GenTreeCall* call);
    GenTreeArgStore* NewArgStore(GenTree* value, CallArgInfo* argInfo, GenTreeCall* call);
    void LowerArgStore(GenTreeArgStore* store);

    void InsertUnmanagedCallPrologAndEpilog(GenTreeCall* call);
    void InsertUnmanagedCallProlog(GenTreeCall* call);
    void InsertUnmanagedCallEpilog(GenTreeCall* call);
    void InsertPInvokeMethodProlog();
    void InsertPInvokeMethodEpilog(INDEBUG(GenTree* lastNode));
    void InsertSetGCState(GenTree* before, int cns);
    void InsertReturnTrap(GenTree* before);
    enum FrameLinkAction
    {
        PushFrame,
        PopFrame
    };
    void InsertFrameLinkUpdate(LIR::Range& block, GenTree* before, FrameLinkAction action);

    // Replace the definition of the given use with a local, allocating a new temp
    // if 'tempNum' is BAD_VAR_NUM. Returns the local load node.
    GenTreeLclLoad* ReplaceWithLclLoad(LIR::Use& use, LclVarDsc* tempLcl = nullptr);

    // return true if this call target is within range of a pc-rel call on the machine
    bool IsCallTargetInRange(void* addr);

#ifdef TARGET_XARCH
    GenTree* GetPreferredRegOptionalOperand(GenTree* op1, GenTree* op2);
#endif

    void LowerStructStore(GenTree* store, StructStoreKind kind, ClassLayout* layout);
    void LowerIndStoreObj(GenTreeIndStoreObj* store);
    void LowerIndStoreBlk(GenTreeIndStoreBlk* store);
    void ContainStructStoreAddress(GenTree* store, unsigned size, GenTree* addr);
    void ContainStructStoreAddressUnrollRegsWB(GenTree* addr);

#ifdef TARGET_XARCH
    bool IsIndLoadRMWCandidate(GenTreeIndStore* store, GenTreeIndir* load, GenTree* src);
    GenTreeIndir* IsStoreIndRMW(GenTreeIndStore* store);
    void LowerStoreIndRMW(GenTreeIndStore* store);
    static bool IndirsAreRMWEquivalent(GenTreeIndir* indir1, GenTreeIndir* indir2);
    static bool LeavesAreRMWEquivalent(GenTree* node1, GenTree* node2);
#endif

#ifdef TARGET_ARM64
    void LowerNot(GenTreeUnOp* node);
    void CombineNot(GenTreeInstr* instr);
    void LowerLogical(GenTreeOp* logical);
    void LowerNegate(GenTreeUnOp* neg);
    void LowerFloatExtend(GenTreeUnOp* node);
    void LowerFloatTruncate(GenTreeUnOp* node);
    void LowerFloatNegate(GenTreeUnOp* neg);
    void LowerFloatArithmetic(GenTreeOp* arith);
    void LowerIntrinsic(GenTreeIntrinsic* intrinsic);
    void LowerArithmetic(GenTreeOp* arith);
    void LowerMultiply(GenTreeOp* mul);
    void LowerUnsignedDiv(GenTreeOp* udiv);
    GenTree* LowerSignedConstDiv(GenTreeOp* node);
    GenTree* LowerSignedDiv(GenTreeOp* div);
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
#ifdef DEBUG
    bool IsLegalToMoveUseForward(GenTree* oldUser, GenTree* newUser, GenTree* def);
#endif
#endif

    bool TryCreateAddrMode(GenTree* addr, bool isContainable);

    bool TryTransformStoreObjToStoreInd(GenTreeIndStoreObj* store);

    GenTree* LowerSwitch(GenTreeUnOp* node);
    bool TryLowerSwitchToBitTest(BasicBlock*     jumpTable[],
                                 unsigned        jumpCount,
                                 unsigned        targetCount,
                                 BasicBlock*     bbSwitch,
                                 GenTreeLclLoad* switchValue);

#ifdef FEATURE_SIMD
    void WidenSIMD12IfNecessary(GenTreeLclVar* node);
    bool CanWidenSimd12ToSimd16(const LclVarDsc* lcl);
#endif
#if FEATURE_MULTIREG_RET
    void MakeMultiRegLclStore(GenTreeLclStore* store, GenTree* value);
#endif

#ifdef FEATURE_HW_INTRINSICS
    void LowerHWIntrinsic(GenTreeHWIntrinsic* node);
    void LowerHWIntrinsicCC(GenTreeHWIntrinsic* node, NamedIntrinsic newIntrinsicId, GenCondition condition);
    void LowerVecEquality(GenTreeHWIntrinsic* node, genTreeOps cmpOp);
    void LowerVecIToV(GenTreeHWIntrinsic* node);
    void LowerVecPack(GenTreeHWIntrinsic* node);
    void LowerVecSplat(GenTreeHWIntrinsic* node);
    void LowerVecRegCast(GenTreeHWIntrinsic* node);
    LclVarDsc* GetSimdMemoryTemp(var_types type);
#ifdef TARGET_X86
    void LowerVecItoVLong(GenTreeHWIntrinsic* node);
#endif
#ifdef TARGET_XARCH
    void ContainCheckHWIntrinsic(GenTreeHWIntrinsic* node);
    void LowerFmaIntrinsic(GenTreeHWIntrinsic* node);
    void ContainFmaIntrinsic(GenTreeHWIntrinsic* node);
    void LowerVecSum128(GenTreeHWIntrinsic* node);
    void LowerVecSum256(GenTreeHWIntrinsic* node);
    void LowerVecExtract(GenTreeHWIntrinsic* node);
    void LowerVecInsert(GenTreeHWIntrinsic* node);
    void LowerSse41InsertFloat(GenTreeHWIntrinsic* node);
    void ContainSse41InsertFloat(GenTreeHWIntrinsic* node);
    void TryMakeHWIntrinsicAddrMode(GenTreeHWIntrinsic* node, GenTree* addr);
    void MakeHWIntrinsicMemOp(GenTreeHWIntrinsic* node, GenTree* op);
    void TryMakeHWIntrinsicMemOp(GenTreeHWIntrinsic* node, GenTree* op);
#endif
#ifdef TARGET_ARM64
    bool IsValidConstForMovImm(GenTreeHWIntrinsic* node);
    bool IsValidConstForFMovImm(GenTreeHWIntrinsic* node);
    void LowerAdvSimdInsert(GenTreeHWIntrinsic* node);
    void LowerAdvSimdFusedMultiplyAddScalar(GenTreeHWIntrinsic* node);
    void LowerVecFToV(GenTreeHWIntrinsic* node);
    void LowerVecSum(GenTreeHWIntrinsic* node);
    void LowerVecExtract(GenTreeHWIntrinsic* node);
    void LowerVecInsert(GenTreeHWIntrinsic* node);
#endif

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

        bool AllBitsZero(var_types type) const;
        bool AllBitsOne(var_types type) const;
        bool Insert(var_types type, int index, GenTree* value);
        bool Pack(GenTreeHWIntrinsic* create);
        bool Splat(GenTreeHWIntrinsic* create);
    };

    void LowerVecPackConst(GenTreeHWIntrinsic* create, const VectorConstant& vecConst);
    GenTree* TryRemoveCastIfPresent(var_types expectedType, GenTree* op);

    bool IsHWIntrinsicMemOp(GenTreeHWIntrinsic* instr, GenTree* node, bool* supportsRegOptional)
    {
        return IsHWIntrinsicMemOp(comp, instr, node, supportsRegOptional);
    }
#endif // FEATURE_HW_INTRINSICS

    bool IsImmOperand(GenTree* operand, GenTree* instr) const;

    static bool IsMemStore(GenTree* node);
    static bool IsMemOperand(GenTree* node);

    LIR::Range& BlockRange() const
    {
        return LIR::AsRange(m_block);
    }

    bool ContainImmOperand(GenTree* instr, GenTree* operand) const;

    bool IsSafeToMoveForward(GenTree* move, GenTree* before);
    bool IsSafeToMoveMemOperandForward(GenTree* before, GenTree* mem);
    bool IsSafeToMoveAddrModeForward(GenTree* before, GenTreeAddrMode* addr) const;
    bool IsSafeToMoveLclRegUseForward(GenTree* before, GenTree* use1, GenTree* use2) const;

#ifdef DEBUG
    void VerifyAllLocalsImplicitlyReferenced();
    void VerifyCallArg(GenTree* arg);
    void VerifyCall(GenTreeCall* call);
    void VerifyNode(GenTree* node);
    bool VerifyBlock(BasicBlock* block);
#endif
};
