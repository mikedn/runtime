// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "hwintrinsic.h"

#ifdef FEATURE_SIMD

#ifdef TARGET_XARCH
CORINFO_InstructionSetFlags FilterInstructionSet(CORINFO_InstructionSetFlags instructionSetFlags);
#endif

unsigned GetVectorTSize(CORJIT_FLAGS flags)
{
    assert(!flags.IsSet(CORJIT_FLAGS::CORJIT_FLAG_PREJIT));
    assert(flags.IsSet(CORJIT_FLAGS::CORJIT_FLAG_FEATURE_SIMD));

#ifdef TARGET_XARCH
    if (JitConfig.EnableHWIntrinsic())
    {
        CORINFO_InstructionSetFlags isaFlags;
        isaFlags.SetFromFlagsRaw(flags.GetInstructionSetFlagsRaw());

        if (FilterInstructionSet(isaFlags).HasInstructionSet(InstructionSet_AVX2))
        {
            return 32;
        }
    }
#endif

    return 16;
}

#ifdef TARGET_XARCH
unsigned Compiler::GetVectorTSize() const
{
    assert(JitConfig.EnableHWIntrinsic() && opts.SIMDFeature());

    return opts.IsIsaSupported(InstructionSet_AVX2) ? 32 : 16;
}
#endif

// Answer the question: Is a particular ISA allowed to be used implicitly by optimizations?
// The result of this api call will match the target machine if the result is true
// If the result is false, then the target machine may have support for the instruction
bool Compiler::compOpportunisticallyDependsOn(CORINFO_InstructionSet isa)
{
    return opts.IsIsaSupported(isa) && compExactlyDependsOn(isa);
}

// Answer the question: Is a particular ISA allowed to be used implicitly by optimizations?
// The result of this api call will exactly match the target machine
// on which the function is executed (except for CoreLib, where there are special rules)
bool Compiler::compExactlyDependsOn(CORINFO_InstructionSet isa)
{
#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
    uint64_t isaBit = (1ULL << isa);

    if ((opts.compSupportsISAReported & isaBit) == 0)
    {
        if (notifyInstructionSetUsage(isa, opts.IsIsaSupported(isa)))
        {
            opts.compSupportsISAExactly |= isaBit;
        }

        opts.compSupportsISAReported |= isaBit;
    }

    return (opts.compSupportsISAExactly & isaBit) != 0;
#else
    return false;
#endif
}

bool Compiler::notifyInstructionSetUsage(CORINFO_InstructionSet isa, bool supported) const
{
    JITDUMP("Notify VM instruction set (%s) %s be supported.\n", InstructionSetToString(isa),
            supported ? "must" : "must not");
    return info.compCompHnd->notifyInstructionSetUsage(isa, supported);
}

#endif // FEATURE_SIMD

#ifdef FEATURE_HW_INTRINSICS

struct HWIntrinsicInfoEntry
{
    const char*            name;
    CORINFO_InstructionSet isa;
    int                    vecSize;
    int                    numArgs;
    HWIntrinsicFlag        flags;
    instruction            ins[10];
};

static const HWIntrinsicInfoEntry hwIntrinsicInfoArray[]
{
// clang-format off
#if defined(TARGET_XARCH)
#define InstructionSet_VEC InstructionSet_ILLEGAL
#define HARDWARE_INTRINSIC(isa, name, size, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, flag) \
    {#name, InstructionSet_##isa, size, numarg, static_cast<HWIntrinsicFlag>(flag), t1, t2, t3, t4, t5, t6, t7, t8, t9, t10},
#include "hwintrinsiclistxarch.h"
#undef InstructionSet_VEC
#elif defined (TARGET_ARM64)
#define InstructionSet_VEC InstructionSet_ILLEGAL
#define HARDWARE_INTRINSIC(isa, name, size, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, flag) \
    {#name, InstructionSet_##isa, size, numarg, static_cast<HWIntrinsicFlag>(flag), t1, t2, t3, t4, t5, t6, t7, t8, t9, t10},
#include "hwintrinsiclistarm64.h"
#undef InstructionSet_VEC
#else
#error Unsupported platform
#endif
    // clang-format on
};

static const HWIntrinsicInfoEntry& GetHWIntrinsicInfo(NamedIntrinsic id)
{
    assert(NI_HW_INTRINSIC_FIRST <= id && id <= NI_HW_INTRINSIC_LAST);

    return hwIntrinsicInfoArray[id - NI_HW_INTRINSIC_FIRST];
}

CORINFO_InstructionSet HWIntrinsicInfo::GetIsa(NamedIntrinsic id)
{
    return GetHWIntrinsicInfo(id).isa;
}

static unsigned GetVecSize(NamedIntrinsic id)
{
    return static_cast<unsigned>(GetHWIntrinsicInfo(id).vecSize);
}

instruction HWIntrinsicInfo::GetIns(NamedIntrinsic id, var_types type)
{
    if ((type < TYP_BYTE) || (type > TYP_DOUBLE))
    {
        assert(!"Unexpected type");
        return INS_invalid;
    }
    return GetHWIntrinsicInfo(id).ins[type - TYP_BYTE];
}

bool HWIntrinsicInfo::HasFlag(NamedIntrinsic id, HWIntrinsicFlag flag)
{
    return (GetHWIntrinsicInfo(id).flags & flag) != 0;
}

#ifdef DEBUG
const char* GetHWIntrinsicIdName(NamedIntrinsic id)
{
    static const char* const names[]
    {
#if defined(TARGET_XARCH)
#define HARDWARE_INTRINSIC(isa, name, ...) #isa "_" #name,
#include "hwintrinsiclistxarch.h"
#elif defined(TARGET_ARM64)
#define HARDWARE_INTRINSIC(isa, name, ...) #isa "_" #name,
#include "hwintrinsiclistarm64.h"
#endif
    };

    return (NI_HW_INTRINSIC_FIRST <= id && id <= NI_HW_INTRINSIC_LAST) ? names[id - NI_HW_INTRINSIC_FIRST] : "NI_???";
}
#endif

NamedIntrinsic HWIntrinsicInfo::lookupId(Compiler*             comp,
                                         CORINFO_METHOD_HANDLE method,
                                         const char*           className,
                                         const char*           methodName,
                                         const char*           enclosingClassName)
{
    assert(JitConfig.EnableHWIntrinsic() && comp->opts.SIMDTypes());

    // TODO-Throughput: replace sequential search by binary search
    CORINFO_InstructionSet isa = lookupIsa(className, enclosingClassName);

    if (isa == InstructionSet_ILLEGAL)
    {
        return NI_Illegal;
    }

    bool isSupported = comp->compExactlyDependsOn(isa);

    // TODO-MIKE-Review: This probably picks up the internal VectorN.IsSupported,
    // resulting in the element type validity check being ignored.
    if (strcmp(methodName, "get_IsSupported") == 0)
    {
        if (!isSupported)
        {
            return comp->opts.IsIsaSupported(isa) ? NI_IsSupported_Dynamic : NI_IsSupported_False;
        }

        return NI_IsSupported_True;
    }

    if (!isSupported)
    {
        return NI_Throw_PlatformNotSupportedException;
    }

    CORINFO_SIG_INFO sig;
    comp->info.compCompHnd->getMethodSig(method, &sig);

    for (unsigned i = 0; i < NI_HW_INTRINSIC_LAST - NI_HW_INTRINSIC_FIRST + 1; i++)
    {
        const HWIntrinsicInfoEntry& info = hwIntrinsicInfoArray[i];

        if (isa != info.isa)
        {
            continue;
        }

        unsigned numArgs = static_cast<unsigned>(info.numArgs);

        if ((numArgs != UINT_MAX) && (sig.numArgs != numArgs))
        {
            continue;
        }

        if (strcmp(methodName, info.name) == 0)
        {
            return static_cast<NamedIntrinsic>(NI_HW_INTRINSIC_FIRST + i);
        }
    }

    // There are several helper intrinsics that are implemented in managed code
    // Those intrinsics will hit this code path and need to return NI_Illegal
    return NI_Illegal;
}

GenTree* Importer::PopHWIntrinsicArg(var_types paramType, ClassLayout* paramLayout)
{
    if (!varTypeIsStruct(paramType))
    {
        assert(varTypeIsArithmetic(paramType));

        GenTree* arg = impPopStack().val;
        assert(varActualType(arg->GetType()) == varActualType(paramType));
        return arg;
    }

    GenTree* arg = PopVec(paramType);
    assert(varTypeIsVec(arg->GetType()));
    return arg;
}

GenTree* Importer::PopVec(var_types type)
{
    assert(varTypeIsVec(type));

    GenTree* tree = impPopStack().val;

    if (tree->OperIs(GT_RET_EXPR, GT_CALL))
    {
        // TODO-MIKE-Cleanup: This is probably not needed when the SIMD type is returned in a register.

        ClassLayout* layout = tree->IsRetExpr() ? tree->AsRetExpr()->GetLayout() : tree->AsCall()->GetRetLayout();

        LclVarDsc* tmpLcl = lvaAllocTemp(true DEBUGARG("struct address for call/obj"));
        impAppendTempStore(tmpLcl, tree, layout, CHECK_SPILL_ALL);
        tree = comp->gtNewLclLoad(tmpLcl, tmpLcl->GetType());
    }

    assert(varTypeTargetVec(tree->GetType()) == varTypeTargetVec(type));

    return tree;
}

GenTree* Importer::PopVecAddrLoad(var_types type)
{
    assert(varTypeIsVec(type));

    GenTree* addr = impPopStack().val;

    if (!addr->TypeIs(TYP_BYREF, TYP_I_IMPL))
    {
        BADCODE("incompatible stack type");
    }

    if (addr->OperIs(GT_LCL_ADDR) && (addr->AsLclAddr()->GetLcl()->GetType() == type))
    {
        LclVarDsc* lcl = addr->AsLclAddr()->GetLcl();
        // Currently the importer doesn't generate local field addresses.
        assert(addr->AsLclAddr()->GetLclOffs() == 0);

        return addr->ChangeToLclLoad(type, lcl);
    }

    return comp->gtNewIndLoad(type, addr);
}

#ifdef TARGET_ARM64
GenTree* Importer::AddHWIntrinsicRangeCheckIfNeeded(
    NamedIntrinsic intrinsic, GenTree* immOp, bool mustExpand, int lowerBound, int upperBound)
{
    if (!mustExpand || !HWIntrinsicInfo::HasImm(intrinsic) || !varActualTypeIsInt(immOp->GetType()))
    {
        return immOp;
    }

    assert(!immOp->IsIntCon());

    return AddHWIntrinsicRangeCheck(immOp, lowerBound, upperBound);
}
#endif // TARGET_ARM64

GenTree* Importer::AddHWIntrinsicRangeCheck(GenTree* immOp, int lowerBound, int upperBound)
{
#ifdef TARGET_XARCH
    assert(lowerBound == 0);
    assert(upperBound <= 255);
    assert(upperBound > lowerBound);
#elif defined(TARGET_ARM64)
    assert((lowerBound == 0) || (lowerBound == 1));
    assert(upperBound <= 64);
    assert((upperBound > lowerBound) || ((upperBound == 0) && (lowerBound == 0)));
#endif

    GenTree* immOpUses[2];
    impMakeMultiUse(immOp, 2, immOpUses, CHECK_SPILL_ALL DEBUGARG("vector index check temp"));

#ifdef TARGET_ARM64
    if (lowerBound != 0)
    {
        immOpUses[1] = comp->gtNewOperNode(GT_SUB, TYP_INT, immOpUses[1], comp->gtNewIconNode(lowerBound));
        upperBound -= lowerBound;
    }
#endif

    return comp->gtNewCommaNode(comp->gtNewBoundsChk(immOpUses[1], comp->gtNewIconNode(upperBound + 1),
                                                     ThrowHelperKind::ArgumentOutOfRange),
                                immOpUses[0]);
}

void HWIntrinsicSignature::Read(Compiler* compiler, CORINFO_SIG_INFO* sig)
{
    // Most HW intrinsics have return and parameters of the same type
    // so in many cases we can avoid a ClassLayout table lookup.
    CORINFO_CLASS_HANDLE prevClass  = NO_CLASS_HANDLE;
    ClassLayout*         prevLayout = nullptr;

    retType = CorTypeToPreciseVarType(sig->retType);

    if (retType != TYP_STRUCT)
    {
        retLayout = nullptr;
    }
    else
    {
        prevClass  = sig->retTypeClass;
        prevLayout = compiler->typGetObjLayout(prevClass);

        retLayout = prevLayout;
        retType   = prevLayout->IsVector() ? prevLayout->GetVectorType() : TYP_STRUCT;
    }

    ICorJitInfo*            vm    = compiler->info.compCompHnd;
    CORINFO_ARG_LIST_HANDLE param = sig->args;

    hasThisParam = sig->hasThis();
    paramCount   = sig->numArgs;

    for (unsigned i = 0; i < min(_countof(paramType), sig->numArgs); i++, param = vm->getArgNext(param))
    {
        CORINFO_CLASS_HANDLE paramClass;
        CorInfoType          corType = strip(vm->getArgType(sig, param, &paramClass));

        paramType[i] = CorTypeToPreciseVarType(corType);

        if (corType == CORINFO_TYPE_PTR)
        {
            CORINFO_CLASS_HANDLE pointerClass;

            paramLayout[i]      = nullptr;
            paramPointerType[i] = CorTypeToVarType(vm->getChildType(vm->getArgClass(sig, param), &pointerClass));

            continue;
        }

        paramPointerType[i] = TYP_UNDEF;

        if (paramType[i] != TYP_STRUCT)
        {
            paramLayout[i] = nullptr;
            continue;
        }

        if (prevClass != paramClass)
        {
            prevClass  = paramClass;
            prevLayout = compiler->typGetObjLayout(prevClass);
        }

        paramLayout[i] = prevLayout;
        paramType[i]   = prevLayout->IsVector() ? prevLayout->GetVectorType() : TYP_STRUCT;
    }
}

var_types HWIntrinsicSignature::GetBaseTypeFromParam(unsigned index, ClassLayout** layout) const
{
    assert(index < paramCount);

    if (var_types pointerType = paramPointerType[index])
    {
        *layout = nullptr;
        return pointerType;
    }

    if (!varTypeIsStruct(paramType[index]))
    {
        *layout = nullptr;
        return paramType[index];
    }

    *layout = paramLayout[index];
    return paramLayout[index]->GetElementType();
}

GenTree* Importer::ImportHWIntrinsic(NamedIntrinsic        intrinsic,
                                     CORINFO_CLASS_HANDLE  clsHnd,
                                     CORINFO_METHOD_HANDLE method,
                                     CORINFO_SIG_INFO*     sigInfo,
                                     bool                  mustExpand)
{
    GenTree* node = ImportHWIntrinsic2(intrinsic, clsHnd, method, sigInfo, mustExpand);

    if (node != nullptr)
    {
        if (varTypeUsesVecReg(node->GetType()))
        {
            comp->compFloatingPointUsed = true;
        }

        return node;
    }

    if (!mustExpand)
    {
        return nullptr;
    }

    return ImportUnsupportedNamedIntrinsic(CORINFO_HELP_THROW_NOT_IMPLEMENTED, method, sigInfo, mustExpand);
}

GenTree* Importer::ImportHWIntrinsic2(NamedIntrinsic        intrinsic,
                                      CORINFO_CLASS_HANDLE  clsHnd,
                                      CORINFO_METHOD_HANDLE method,
                                      CORINFO_SIG_INFO*     sigInfo,
                                      bool                  mustExpand)
{
    HWIntrinsicSignature sig;
    sig.isIntrinsicMethod = mustExpand;
    sig.Read(comp, sigInfo);

    var_types    baseType  = TYP_UNDEF;
    var_types    retType   = sig.retType;
    ClassLayout* retLayout = sig.retLayout;

    if (retLayout != nullptr)
    {
        // Currently all intrinsics return either vectors or primitive types, not structs.
        if (!retLayout->IsVector() || retLayout->ElementTypeIsNInt())
        {
            return nullptr;
        }

        baseType = retLayout->GetElementType();
        retType  = retLayout->GetVectorType();
    }

    unsigned vecSize = GetVecSize(intrinsic);

    if (HWIntrinsicInfo::TypeFromArg0(intrinsic) || HWIntrinsicInfo::TypeFromArg1(intrinsic))
    {
        ClassLayout* argLayout = nullptr;
        baseType               = sig.GetBaseTypeFromParam(HWIntrinsicInfo::TypeFromArg1(intrinsic), &argLayout);

        if (argLayout != nullptr)
        {
            if (!argLayout->IsVector() || argLayout->ElementTypeIsNInt())
            {
                return nullptr;
            }

            if (vecSize == UINT32_MAX)
            {
                vecSize = argLayout->GetSize();
            }
        }
        else
        {
            assert(varTypeIsArithmetic(baseType));
        }
    }
    else if (retLayout != nullptr)
    {
        if (vecSize == UINT32_MAX)
        {
            vecSize = retLayout->GetSize();
        }
    }

    if (baseType == TYP_UNDEF)
    {
        switch (intrinsic)
        {
#ifdef TARGET_ARM64
            case NI_Vector64_get_Count:
#endif
            case NI_Vector128_get_Count:
#ifdef TARGET_XARCH
            case NI_Vector256_get_Count:
#endif
                assert(sig.paramCount == 0);
                assert(sig.retType == TYP_INT);

                if (var_types eltType = typGetObjLayout(clsHnd)->GetElementType())
                {
                    GenTreeIntCon* countNode = comp->gtNewIconNode(varTypeVecLength(vecSize, eltType));
                    countNode->gtFlags |= GTF_ICON_SIMD_COUNT;
                    return countNode;
                }

                return nullptr;

#ifdef TARGET_XARCH
            case NI_SSE_Prefetch0:
            case NI_SSE_Prefetch1:
            case NI_SSE_Prefetch2:
            case NI_SSE_PrefetchNonTemporal:
            case NI_SSE_StoreFence:
            case NI_SSE2_LoadFence:
            case NI_SSE2_MemoryFence:
                assert(retType == TYP_VOID);
                break;
#endif

            default:
                assert(HWIntrinsicInfo::IsScalar(intrinsic));
                assert(varTypeIsArithmetic(retType));
                break;
        }

        baseType = retType;
    }

#ifdef TARGET_XARCH
    if ((sig.paramCount > 0) && (HWIntrinsicInfo::HasImm(intrinsic)) &&
        varActualTypeIsInt(impStackTop().val->GetType()))
    {
        assert((HWIntrinsicInfo::GetImmOpUpperBound(intrinsic) == 255) || HWIntrinsicInfo::HasSpecialImport(intrinsic));

        if (!impStackTop().val->IsIntCon() && !mustExpand)
        {
            return nullptr;
        }
    }
#endif // TARGET_XARCH

#ifdef TARGET_ARM64
    GenTree* immOp = nullptr;

    if (HWIntrinsicInfo::HasImm(intrinsic))
    {
        if ((intrinsic == NI_AdvSimd_Insert) || (intrinsic == NI_AdvSimd_InsertScalar) ||
            (intrinsic == NI_AdvSimd_LoadAndInsertScalar))
        {
            assert(sig.paramCount == 3);

            immOp = impStackTop(1).val;
            assert(varActualTypeIsInt(immOp->GetType()));
        }
        else if (intrinsic == NI_AdvSimd_Arm64_InsertSelectedScalar)
        {
            assert(sig.paramCount == 4);
            assert(HWIntrinsicInfo::NoImmFallback(intrinsic));

            GenTree* srcImmOp = impStackTop().val;

            assert(varActualTypeIsInt(srcImmOp->GetType()));

            if (!srcImmOp->IsIntCon())
            {
                return nullptr;
            }

            ClassLayout* srcVecLayout = sig.paramLayout[2];
            assert(srcVecLayout->IsVector());
            assert(srcVecLayout->GetElementType() == baseType);
            unsigned srcVecSize = srcVecLayout->GetSize();

            int srcImmLowerBound = 0;
            int srcImmUpperBound = 0;
            HWIntrinsicInfo::GetImmOpBounds(intrinsic, srcVecSize, baseType, &srcImmLowerBound, &srcImmUpperBound);

            const int srcImmVal = srcImmOp->AsIntCon()->GetInt32Value();

            if ((srcImmVal < srcImmLowerBound) || (srcImmVal > srcImmUpperBound))
            {
                assert(!mustExpand);
                return nullptr;
            }

            immOp = impStackTop(2).val;
            assert(varActualTypeIsInt(immOp->GetType()));
        }
        else if ((sig.paramCount > 0) && varActualTypeIsInt(impStackTop(0).val->GetType()))
        {
            immOp = impStackTop(0).val;
        }
    }

    int immLowerBound = 0;
    int immUpperBound = 0;

    if (immOp != nullptr)
    {
        unsigned immVecSize;

        if (!HWIntrinsicInfo::IsVecByElt(intrinsic))
        {
            immVecSize = vecSize;
        }
        else
        {
            ClassLayout* indexedLayout;

            if (sig.paramCount == 3)
            {
                indexedLayout = sig.paramLayout[1];
            }
            else
            {
                assert(sig.paramCount == 4);
                indexedLayout = sig.paramLayout[2];
            }

            assert(indexedLayout->IsVector());

            if (intrinsic == NI_Dp_DotProductBySelectedQuadruplet)
            {
                assert(((baseType == TYP_INT) && (indexedLayout->GetElementType() == TYP_BYTE)) ||
                       ((baseType == TYP_UINT) && (indexedLayout->GetElementType() == TYP_UBYTE)));
            }
            else
            {
                assert(indexedLayout->GetElementType() == baseType);
            }

            immVecSize = indexedLayout->GetSize();
        }

        HWIntrinsicInfo::GetImmOpBounds(intrinsic, immVecSize, baseType, &immLowerBound, &immUpperBound);

        if (GenTreeIntCon* immIntCon = immOp->IsIntCon())
        {
            const int imm = immIntCon->GetInt32Value();

            if ((imm < immLowerBound) || (immUpperBound < imm))
            {
                assert(!mustExpand);
                return nullptr;
            }
        }
        else
        {
            if (HWIntrinsicInfo::NoImmFallback(intrinsic) || !mustExpand)
            {
                return nullptr;
            }
        }
    }
#endif // TARGET_ARM64

    if (HWIntrinsicInfo::HasSpecialImport(intrinsic))
    {
        return ImportSpecialIntrinsic(intrinsic, sig);
    }

    const bool isScalar = HWIntrinsicInfo::IsScalar(intrinsic);

    if (!isScalar && ((HWIntrinsicInfo::GetIns(intrinsic, baseType) == INS_invalid) ||
                      ((vecSize != 8) && (vecSize != 16) && (vecSize != 32))))
    {
        assert(!"Unexpected HW Intrinsic");
        return nullptr;
    }

    GenTree*            op1     = nullptr;
    GenTree*            op2     = nullptr;
    GenTree*            op3     = nullptr;
    GenTree*            op4     = nullptr;
    GenTreeHWIntrinsic* retNode = nullptr;

    var_types nodeType = varTypeNodeType(retType);

    switch (sig.paramCount)
    {
        case 0:
            assert(!isScalar);
            return NewVecNode(nodeType, intrinsic, baseType, vecSize);

        case 1:
            op1 = PopHWIntrinsicArg(sig.paramType[0], sig.paramLayout[0]);

            if (HWIntrinsicInfo::IsLoad(intrinsic))
            {
                if (op1->OperIs(GT_BITCAST) && op1->AsUnOp()->GetOp(0)->TypeIs(TYP_BYREF))
                {
                    op1 = op1->AsUnOp()->GetOp(0);
                }
            }

            retNode = isScalar ? comp->gtNewScalarHWIntrinsicNode(nodeType, intrinsic, op1)
                               : NewVecNode(nodeType, intrinsic, baseType, vecSize, op1);
            break;

        case 2:
#ifdef TARGET_ARM64
            switch (intrinsic)
            {
                case NI_AdvSimd_AddWideningLower:
                    if (baseType == sig.paramLayout[0]->GetElementType())
                    {
                        intrinsic = NI_AdvSimd_ADDL;
                    }
                    break;
                case NI_AdvSimd_SubtractWideningLower:
                    if (baseType == sig.paramLayout[0]->GetElementType())
                    {
                        intrinsic = NI_AdvSimd_SUBL;
                    }
                    break;
                case NI_AdvSimd_AddWideningUpper:
                    if (baseType == sig.paramLayout[0]->GetElementType())
                    {
                        intrinsic = NI_AdvSimd_ADDL2;
                    }
                    break;
                case NI_AdvSimd_SubtractWideningUpper:
                    if (baseType == sig.paramLayout[0]->GetElementType())
                    {
                        intrinsic = NI_AdvSimd_SUBL2;
                    }
                    break;
                case NI_AdvSimd_Arm64_AddSaturateScalar:
                    if (baseType != sig.paramLayout[1]->GetElementType())
                    {
                        intrinsic = NI_AdvSimd_Arm64_SUQADD;
                    }
                    break;
                default:
                    break;
            }
#endif
            op2 = PopHWIntrinsicArg(sig.paramType[1], sig.paramLayout[1]);
#ifdef TARGET_ARM64
            op2 = AddHWIntrinsicRangeCheckIfNeeded(intrinsic, op2, mustExpand, immLowerBound, immUpperBound);
#endif
            op1 = PopHWIntrinsicArg(sig.paramType[0], sig.paramLayout[0]);

            if (!isScalar)
            {
                retNode = NewVecNode(nodeType, intrinsic, baseType, vecSize, op1, op2);
            }
            else
            {
                retNode = comp->gtNewScalarHWIntrinsicNode(nodeType, intrinsic, op1, op2);
            }

            break;

        case 3:
            op3 = PopHWIntrinsicArg(sig.paramType[2], sig.paramLayout[2]);
            op2 = PopHWIntrinsicArg(sig.paramType[1], sig.paramLayout[1]);
            op1 = PopHWIntrinsicArg(sig.paramType[0], sig.paramLayout[0]);

#ifdef TARGET_ARM64
            assert(!HWIntrinsicInfo::IsVecByElt(intrinsic) || varTypeIsVec(op2->GetType()));

            if (intrinsic == NI_AdvSimd_LoadAndInsertScalar)
            {
                if (op1->OperIs(GT_BITCAST) && op1->AsUnOp()->GetOp(0)->TypeIs(TYP_BYREF))
                {
                    op1 = op1->AsUnOp()->GetOp(0);
                }

                op2 = AddHWIntrinsicRangeCheckIfNeeded(intrinsic, op2, mustExpand, immLowerBound, immUpperBound);
            }
            else if ((intrinsic == NI_AdvSimd_Insert) || (intrinsic == NI_AdvSimd_InsertScalar))
            {
                op2 = AddHWIntrinsicRangeCheckIfNeeded(intrinsic, op2, mustExpand, immLowerBound, immUpperBound);
            }
            else
            {
                op3 = AddHWIntrinsicRangeCheckIfNeeded(intrinsic, op3, mustExpand, immLowerBound, immUpperBound);
            }
#endif

            retNode = isScalar ? comp->gtNewScalarHWIntrinsicNode(nodeType, intrinsic, op1, op2, op3)
                               : NewVecNode(nodeType, intrinsic, baseType, vecSize, op1, op2, op3);

            break;

#ifdef TARGET_ARM64
        case 4:
            op4 = PopHWIntrinsicArg(sig.paramType[3], sig.paramLayout[3]);
            op4 = AddHWIntrinsicRangeCheckIfNeeded(intrinsic, op4, mustExpand, immLowerBound, immUpperBound);
            op3 = PopHWIntrinsicArg(sig.paramType[2], sig.paramLayout[2]);
            op2 = PopHWIntrinsicArg(sig.paramType[1], sig.paramLayout[1]);
            op1 = PopHWIntrinsicArg(sig.paramType[0], sig.paramLayout[0]);

            assert(!HWIntrinsicInfo::IsVecByElt(intrinsic) || varTypeIsVec(op3->GetType()));
            assert(!isScalar);

            retNode = NewVecNode(nodeType, intrinsic, baseType, vecSize, op1, op2, op3, op4);
            break;
#endif

        default:
            return nullptr;
    }

    const bool isMemoryStore = retNode->IsMemoryStore();

    if (isMemoryStore || retNode->IsMemoryLoad())
    {
        if (isMemoryStore)
        {
            retNode->AddSideEffects(GTF_ASG);
        }

        retNode->AddSideEffects(GTF_GLOB_REF | GTF_EXCEPT);
    }

    return retNode;
}

GenTree* Importer::impVecExtract(ClassLayout* layout, GenTree* value, GenTree* index)
{
    assert(value->GetType() == layout->GetSIMDType());
    assert(varActualType(index->GetType()) == TYP_INT);

    int  maxIndexValue = static_cast<int>(layout->GetElementCount() - 1);
    bool rangeCheckNeeded;

    if (GenTreeIntCon* intCon = index->IsIntCon())
    {
        rangeCheckNeeded = (intCon->GetInt32Value() < 0) || (intCon->GetInt32Value() > maxIndexValue);
    }
    else
    {
        rangeCheckNeeded = true;
    }

    if (rangeCheckNeeded)
    {
        index = AddHWIntrinsicRangeCheck(index, 0, maxIndexValue);
    }

    return NewVecExtractNode(layout->GetElementType(), value, index);
}

#endif // FEATURE_HW_INTRINSICS
