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

    unsigned length = 16;

#ifdef TARGET_XARCH
    if (JitConfig.EnableHWIntrinsic())
    {
        CORINFO_InstructionSetFlags isaFlags;
        isaFlags.SetFromFlagsRaw(flags.GetInstructionSetFlagsRaw());

        if (FilterInstructionSet(isaFlags).HasInstructionSet(InstructionSet_AVX2))
        {
            length = 32;
        }
    }
#endif

    return length;
}

var_types Compiler::GetVectorTSimdType()
{
#if defined(TARGET_XARCH)
    if (compOpportunisticallyDependsOn(InstructionSet_AVX2))
    {
        return JitConfig.EnableHWIntrinsic() && opts.SIMDFeature() ? TYP_SIMD32 : TYP_SIMD16;
    }

    bool isaUseable = compExactlyDependsOn(InstructionSet_AVX2);
    assert(!isaUseable);

    return TYP_SIMD16;
#elif defined(TARGET_ARM64)
    return TYP_SIMD16;
#else
#error Unsupported platform
#endif
}

#ifdef DEBUG
// Answer the question: Is a particular ISA supported?
// Use this api when asking the question so that future
// ISA questions can be asked correctly or when asserting
// support/nonsupport for an instruction set
bool Compiler::compIsaSupportedDebugOnly(CORINFO_InstructionSet isa) const
{
#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
    return opts.IsIsaSupported(isa);
#else
    return false;
#endif
}
#endif // DEBUG

// Answer the question: Is a particular ISA supported for explicit hardware intrinsics?
bool Compiler::compHWIntrinsicDependsOn(CORINFO_InstructionSet isa)
{
    compExactlyDependsOn(isa);

    return opts.IsIsaSupported(isa) && JitConfig.EnableHWIntrinsic() &&
           (opts.SIMDFeature() || HWIntrinsicInfo::IsScalarIsa(isa)) && HWIntrinsicInfo::IsImplementedIsa(isa);
}

#ifdef TARGET_XARCH
bool Compiler::canUseVexEncoding()
{
    return compOpportunisticallyDependsOn(InstructionSet_AVX);
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
    int                    simdSize;
    int                    numArgs;
    HWIntrinsicCategory    category;
    HWIntrinsicFlag        flags;
    instruction            ins[10];
};

static const HWIntrinsicInfoEntry hwIntrinsicInfoArray[]
{
// clang-format off
#if defined(TARGET_XARCH)
#define INS_movsdsse2 INS_movsd
#define HARDWARE_INTRINSIC(isa, name, size, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, category, flag) \
    {#name, InstructionSet_##isa, size, numarg, category, static_cast<HWIntrinsicFlag>(flag), t1, t2, t3, t4, t5, t6, t7, t8, t9, t10},
#include "hwintrinsiclistxarch.h"
#undef INS_movsdsse2
#elif defined (TARGET_ARM64)
#define HARDWARE_INTRINSIC(isa, name, size, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, category, flag) \
    {#name, InstructionSet_##isa, size, numarg, category, static_cast<HWIntrinsicFlag>(flag), t1, t2, t3, t4, t5, t6, t7, t8, t9, t10},
#include "hwintrinsiclistarm64.h"
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

HWIntrinsicCategory HWIntrinsicInfo::GetCategory(NamedIntrinsic id)
{
    return GetHWIntrinsicInfo(id).category;
}

static unsigned GetSimdSize(NamedIntrinsic id)
{
    return static_cast<unsigned>(GetHWIntrinsicInfo(id).simdSize);
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

NamedIntrinsic HWIntrinsicInfo::lookupId(Compiler*         comp,
                                         CORINFO_SIG_INFO* sig,
                                         const char*       className,
                                         const char*       methodName,
                                         const char*       enclosingClassName)
{
    // TODO-Throughput: replace sequential search by binary search
    CORINFO_InstructionSet isa = lookupIsa(className, enclosingClassName);

    if (isa == InstructionSet_ILLEGAL)
    {
        return NI_Illegal;
    }

    bool isIsaSupported = comp->compHWIntrinsicDependsOn(isa);

    if (strcmp(methodName, "get_IsSupported") == 0)
    {
        return isIsaSupported ? (comp->compExactlyDependsOn(isa) ? NI_IsSupported_True : NI_IsSupported_Dynamic)
                              : NI_IsSupported_False;
    }

    if (!isIsaSupported)
    {
        return NI_Throw_PlatformNotSupportedException;
    }

    for (unsigned i = 0; i < NI_HW_INTRINSIC_LAST - NI_HW_INTRINSIC_FIRST + 1; i++)
    {
        const HWIntrinsicInfoEntry& info = hwIntrinsicInfoArray[i];

        if (isa != info.isa)
        {
            continue;
        }

        unsigned numArgs = static_cast<unsigned>(info.numArgs);

        if ((numArgs != UINT_MAX) && (sig->numArgs != numArgs))
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

bool HWIntrinsicInfo::isImmOp(NamedIntrinsic id, const GenTree* op)
{
#ifdef TARGET_XARCH
    if (HWIntrinsicInfo::GetCategory(id) != HW_Category_IMM)
    {
        return false;
    }

    if (!HWIntrinsicInfo::MaybeImm(id))
    {
        return true;
    }
#elif defined(TARGET_ARM64)
    if (!HWIntrinsicInfo::HasImmediateOperand(id))
    {
        return false;
    }
#else
#error Unsupported platform
#endif

    return varActualTypeIsInt(op->GetType());
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
    assert(varTypeIsSIMD(arg->GetType()));
    return arg;
}

GenTree* Importer::PopVec(var_types type)
{
    assert(varTypeIsSIMD(type));

    GenTree* tree = impPopStack().val;

    if (tree->OperIs(GT_RET_EXPR, GT_CALL))
    {
        // TODO-MIKE-Cleanup: This is probably not needed when the SIMD type is returned in a register.

        ClassLayout* layout = tree->IsRetExpr() ? tree->AsRetExpr()->GetLayout() : tree->AsCall()->GetRetLayout();

        LclVarDsc* tmpLcl = lvaAllocTemp(true DEBUGARG("struct address for call/obj"));
        impAppendTempStore(tmpLcl, tree, layout, CHECK_SPILL_ALL);
        tree = comp->gtNewLclLoad(tmpLcl, tmpLcl->GetType());
    }

    assert(varTypeGetTargetVec(tree->GetType()) == varTypeGetTargetVec(type));

    return tree;
}

GenTree* Importer::PopVecAddrLoad(var_types type)
{
    assert(varTypeIsSIMD(type));

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

GenTree* Importer::AddHWIntrinsicRangeCheckIfNeeded(
    NamedIntrinsic intrinsic, GenTree* immOp, bool mustExpand, int lowerBound, int upperBound)
{
#ifdef TARGET_XARCH
    // AVX2 Gather intrinsics no not need the range-check (their imm has discrete valid
    // values that are handle by managed code) and have special importing code.
    assert(!HWIntrinsicInfo::isAVX2GatherIntrinsic(intrinsic));
#endif

    // Full-range imm-intrinsics do not need the range-check
    // because the imm-parameter of the intrinsic method is a byte.
    if (!mustExpand || !HWIntrinsicInfo::isImmOp(intrinsic, immOp)
#ifdef TARGET_XARCH
        || HWIntrinsicInfo::HasFullRangeImm(intrinsic)
#endif
            )
    {
        return immOp;
    }

    assert(!immOp->IsIntCon());

    return AddHWIntrinsicRangeCheck(immOp, lowerBound, upperBound);
}

GenTree* Importer::AddHWIntrinsicRangeCheck(GenTree* immOp, int immLowerBound, int immUpperBound)
{
    // Bounds check for value of an immediate operand
    //   (immLowerBound <= immOp) && (immOp <= immUpperBound)
    //
    // implemented as a single comparison in the form of
    //
    // if ((immOp - immLowerBound) >= (immUpperBound - immLowerBound + 1))
    // {
    //     throw new ArgumentOutOfRangeException();
    // }
    //
    // The value of (immUpperBound - immLowerBound + 1) is denoted as adjustedUpperBound.

    const ssize_t adjustedUpperBound     = (ssize_t)immUpperBound - immLowerBound + 1;
    GenTree*      adjustedUpperBoundNode = comp->gtNewIconNode(adjustedUpperBound, TYP_INT);

    GenTree* immOpUses[2];
    impMakeMultiUse(immOp, 2, immOpUses, CHECK_SPILL_ALL DEBUGARG("vector index check temp"));

    if (immLowerBound != 0)
    {
        immOpUses[1] = comp->gtNewOperNode(GT_SUB, TYP_INT, immOpUses[1], comp->gtNewIconNode(immLowerBound, TYP_INT));
    }

    GenTreeBoundsChk* check =
        comp->gtNewBoundsChk(immOpUses[1], adjustedUpperBoundNode, ThrowHelperKind::ArgumentOutOfRange);
    return comp->gtNewCommaNode(check, immOpUses[0]);
}

static bool impIsTableDrivenHWIntrinsic(NamedIntrinsic intrinsicId, HWIntrinsicCategory category)
{
    return NOT_ARM64((category != HW_Category_Special) &&) HWIntrinsicInfo::RequiresCodegen(intrinsicId) &&
           !HWIntrinsicInfo::HasSpecialImport(intrinsicId);
}

static bool isSupportedBaseType(NamedIntrinsic intrinsic, var_types baseType)
{
    // We don't actually check the intrinsic outside of the false case as we expect
    // the exposed managed signatures are either generic and support all types
    // or they are explicit and support the type indicated.

    if (varTypeIsArithmetic(baseType))
    {
        return true;
    }

#ifdef TARGET_XARCH
    assert((intrinsic == NI_Vector128_As) || (intrinsic == NI_Vector128_AsByte) ||
           (intrinsic == NI_Vector128_AsDouble) || (intrinsic == NI_Vector128_AsInt16) ||
           (intrinsic == NI_Vector128_AsInt32) || (intrinsic == NI_Vector128_AsInt64) ||
           (intrinsic == NI_Vector128_AsSByte) || (intrinsic == NI_Vector128_AsSingle) ||
           (intrinsic == NI_Vector128_AsUInt16) || (intrinsic == NI_Vector128_AsUInt32) ||
           (intrinsic == NI_Vector128_AsUInt64) || (intrinsic == NI_Vector128_get_AllBitsSet) ||
           (intrinsic == NI_Vector128_get_Count) || (intrinsic == NI_Vector128_get_Zero) ||
           (intrinsic == NI_Vector128_GetElement) || (intrinsic == NI_Vector128_WithElement) ||
           (intrinsic == NI_Vector128_ToScalar) || (intrinsic == NI_Vector128_ToVector256) ||
           (intrinsic == NI_Vector128_ToVector256Unsafe) || (intrinsic == NI_Vector256_As) ||
           (intrinsic == NI_Vector256_AsByte) || (intrinsic == NI_Vector256_AsDouble) ||
           (intrinsic == NI_Vector256_AsInt16) || (intrinsic == NI_Vector256_AsInt32) ||
           (intrinsic == NI_Vector256_AsInt64) || (intrinsic == NI_Vector256_AsSByte) ||
           (intrinsic == NI_Vector256_AsSingle) || (intrinsic == NI_Vector256_AsUInt16) ||
           (intrinsic == NI_Vector256_AsUInt32) || (intrinsic == NI_Vector256_AsUInt64) ||
           (intrinsic == NI_Vector256_get_AllBitsSet) || (intrinsic == NI_Vector256_get_Count) ||
           (intrinsic == NI_Vector256_get_Zero) || (intrinsic == NI_Vector256_GetElement) ||
           (intrinsic == NI_Vector256_WithElement) || (intrinsic == NI_Vector256_GetLower) ||
           (intrinsic == NI_Vector256_ToScalar));
#endif // TARGET_XARCH
#ifdef TARGET_ARM64
    assert((intrinsic == NI_Vector64_As) || (intrinsic == NI_Vector64_AsByte) || (intrinsic == NI_Vector64_AsDouble) ||
           (intrinsic == NI_Vector64_AsInt16) || (intrinsic == NI_Vector64_AsInt32) ||
           (intrinsic == NI_Vector64_AsInt64) || (intrinsic == NI_Vector64_AsSByte) ||
           (intrinsic == NI_Vector64_AsSingle) || (intrinsic == NI_Vector64_AsUInt16) ||
           (intrinsic == NI_Vector64_AsUInt32) || (intrinsic == NI_Vector64_AsUInt64) ||
           (intrinsic == NI_Vector64_get_AllBitsSet) || (intrinsic == NI_Vector64_get_Count) ||
           (intrinsic == NI_Vector64_get_Zero) || (intrinsic == NI_Vector64_GetElement) ||
           (intrinsic == NI_Vector64_ToScalar) || (intrinsic == NI_Vector64_ToVector128) ||
           (intrinsic == NI_Vector64_ToVector128Unsafe) || (intrinsic == NI_Vector64_WithElement) ||
           (intrinsic == NI_Vector128_As) || (intrinsic == NI_Vector128_AsByte) ||
           (intrinsic == NI_Vector128_AsDouble) || (intrinsic == NI_Vector128_AsInt16) ||
           (intrinsic == NI_Vector128_AsInt32) || (intrinsic == NI_Vector128_AsInt64) ||
           (intrinsic == NI_Vector128_AsSByte) || (intrinsic == NI_Vector128_AsSingle) ||
           (intrinsic == NI_Vector128_AsUInt16) || (intrinsic == NI_Vector128_AsUInt32) ||
           (intrinsic == NI_Vector128_AsUInt64) || (intrinsic == NI_Vector128_get_AllBitsSet) ||
           (intrinsic == NI_Vector128_get_Count) || (intrinsic == NI_Vector128_get_Zero) ||
           (intrinsic == NI_Vector128_GetElement) || (intrinsic == NI_Vector128_GetLower) ||
           (intrinsic == NI_Vector128_GetUpper) || (intrinsic == NI_Vector128_ToScalar) ||
           (intrinsic == NI_Vector128_WithElement));
#endif // TARGET_ARM64
    return false;
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
        retType   = prevLayout->IsVector() ? prevLayout->GetSIMDType() : TYP_STRUCT;
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
        paramType[i]   = prevLayout->IsVector() ? prevLayout->GetSIMDType() : TYP_STRUCT;
    }
}

var_types HWIntrinsicSignature::GetBaseTypeFromParam(NamedIntrinsic intrinsic, ClassLayout** layout) const
{
    assert(HWIntrinsicInfo::BaseTypeFromSecondArg(intrinsic) || HWIntrinsicInfo::BaseTypeFromFirstArg(intrinsic));

    unsigned index = HWIntrinsicInfo::BaseTypeFromSecondArg(intrinsic);

    if (var_types pointerType = paramPointerType[index])
    {
        *layout = nullptr;
        return pointerType;
    }

    if (!varTypeIsSIMD(paramType[index]))
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
    HWIntrinsicSignature sig;
    sig.Read(comp, sigInfo);

    var_types    baseType  = TYP_UNDEF;
    unsigned     simdSize  = GetSimdSize(intrinsic);
    var_types    retType   = sig.retType;
    ClassLayout* retLayout = sig.retLayout;

    if ((retLayout != nullptr) && opts.SIMDFeature())
    {
        // Currently all HW intrinsics return either vectors or primitive types, not structs.
        if (!retLayout->IsVector() || retLayout->ElementTypeIsNInt())
        {
            return nullptr;
        }

        baseType = retLayout->GetElementType();
        retType  = retLayout->GetSIMDType();
    }

    if (HWIntrinsicInfo::BaseTypeFromSecondArg(intrinsic) || HWIntrinsicInfo::BaseTypeFromFirstArg(intrinsic))
    {
        ClassLayout* argLayout = nullptr;
        baseType               = sig.GetBaseTypeFromParam(intrinsic, &argLayout);

        if ((argLayout != nullptr) && argLayout->IsVector())
        {
            if (argLayout->ElementTypeIsNInt())
            {
                return nullptr;
            }

            if (simdSize == UINT32_MAX)
            {
                simdSize = argLayout->GetSize();
            }
        }
    }
    else if (retLayout != nullptr)
    {
        if (simdSize == UINT32_MAX)
        {
            simdSize = retLayout->GetSize();
        }
    }

    HWIntrinsicCategory category = HWIntrinsicInfo::GetCategory(intrinsic);

    if (baseType == TYP_UNDEF)
    {
        if (category != HW_Category_Scalar)
        {
            baseType = typGetObjLayout(clsHnd)->GetElementType();
        }
        else
        {
            baseType = retType;
        }
    }

    // Immediately return if the category is other than scalar/special and this is not a supported base type.
    if (NOT_ARM64((category != HW_Category_Special) &&)(category != HW_Category_Scalar) &&
        !isSupportedBaseType(intrinsic, baseType))
    {
        return nullptr;
    }

    GenTree* immOp = nullptr;

#ifdef TARGET_ARM64
    if ((intrinsic == NI_AdvSimd_Insert) || (intrinsic == NI_AdvSimd_InsertScalar) ||
        (intrinsic == NI_AdvSimd_LoadAndInsertScalar))
    {
        assert(sig.paramCount == 3);
        immOp = impStackTop(1).val;
        assert(HWIntrinsicInfo::isImmOp(intrinsic, immOp));
    }
    else if (intrinsic == NI_AdvSimd_Arm64_InsertSelectedScalar)
    {
        // InsertSelectedScalar intrinsic has two immediate operands.
        // Since all the remaining intrinsics on both platforms have only one immediate
        // operand, in order to not complicate the shared logic even further we ensure here that
        // 1) The second immediate operand immOp2 is constant and
        // 2) its value belongs to [0, sizeof(op3) / sizeof(op3.BaseType)).
        // If either is false, we should fallback to the managed implementation Insert(dst, dstIdx, Extract(src,
        // srcIdx)).
        // The check for the first immediate operand immOp will use the same logic as other intrinsics that have an
        // immediate operand.

        GenTree* immOp2 = nullptr;

        assert(sig.paramCount == 4);

        immOp  = impStackTop(2).val;
        immOp2 = impStackTop().val;

        assert(HWIntrinsicInfo::isImmOp(intrinsic, immOp));
        assert(HWIntrinsicInfo::isImmOp(intrinsic, immOp2));

        if (!immOp2->IsIntCon())
        {
            assert(HWIntrinsicInfo::NoJmpTableImm(intrinsic));
            return ImportNonConstFallback(intrinsic, retType, baseType);
        }

        ClassLayout* sourceVectorLayout = sig.paramLayout[2];
        assert(sourceVectorLayout->IsVector());
        unsigned  otherSimdSize = sourceVectorLayout->GetSize();
        var_types otherBaseType = sourceVectorLayout->GetElementType();

        assert(otherBaseType == baseType);

        int immLowerBound2 = 0;
        int immUpperBound2 = 0;

        HWIntrinsicInfo::LookupImmBounds(intrinsic, otherSimdSize, otherBaseType, &immLowerBound2, &immUpperBound2);

        const int immVal2 = immOp2->AsIntCon()->GetInt32Value();

        if ((immVal2 < immLowerBound2) || (immVal2 > immUpperBound2))
        {
            assert(!mustExpand);
            return nullptr;
        }
    }
    else
#endif
        if ((sig.paramCount > 0) && HWIntrinsicInfo::isImmOp(intrinsic, impStackTop().val))
    {
        // NOTE: The following code assumes that for all intrinsics
        // taking an immediate operand, that operand will be last.
        immOp = impStackTop().val;
    }

    int  immLowerBound   = 0;
    int  immUpperBound   = 0;
    bool hasFullRangeImm = false;

    if (immOp != nullptr)
    {
#ifdef TARGET_XARCH
        immUpperBound   = HWIntrinsicInfo::lookupImmUpperBound(intrinsic);
        hasFullRangeImm = HWIntrinsicInfo::HasFullRangeImm(intrinsic);
#elif defined(TARGET_ARM64)
        if (category == HW_Category_SIMDByIndexedElement)
        {
            var_types indexedElementBaseType;
            unsigned  indexedElementSimdSize = 0;

            if (sig.paramCount == 3)
            {
                ClassLayout* layout = sig.paramLayout[1];
                assert(layout->IsVector());
                indexedElementBaseType = layout->GetElementType();
                indexedElementSimdSize = layout->GetSize();
            }
            else
            {
                assert(sig.paramCount == 4);

                ClassLayout* layout = sig.paramLayout[2];
                assert(layout->IsVector());
                indexedElementBaseType = layout->GetElementType();
                indexedElementSimdSize = layout->GetSize();

                if (intrinsic == NI_Dp_DotProductBySelectedQuadruplet)
                {
                    assert(((baseType == TYP_INT) && (indexedElementBaseType == TYP_BYTE)) ||
                           ((baseType == TYP_UINT) && (indexedElementBaseType == TYP_UBYTE)));
                    // The second source operand of sdot, udot instructions is an indexed 32-bit element.
                    indexedElementBaseType = baseType;
                }
            }

            assert(indexedElementBaseType == baseType);
            HWIntrinsicInfo::LookupImmBounds(intrinsic, indexedElementSimdSize, baseType, &immLowerBound,
                                             &immUpperBound);
        }
        else
        {
            HWIntrinsicInfo::LookupImmBounds(intrinsic, simdSize, baseType, &immLowerBound, &immUpperBound);
        }
#endif

        if (!hasFullRangeImm && immOp->IsIntCon())
        {
            const int ival = immOp->AsIntCon()->GetInt32Value();
            bool      immOutOfRange;

#ifdef TARGET_XARCH
            if (HWIntrinsicInfo::isAVX2GatherIntrinsic(intrinsic))
            {
                immOutOfRange = (ival != 1) && (ival != 2) && (ival != 4) && (ival != 8);
            }
            else
#endif
            {
                immOutOfRange = (ival < immLowerBound) || (ival > immUpperBound);
            }

            if (immOutOfRange)
            {
                assert(!mustExpand);
                // The imm-HWintrinsics that do not accept all imm8 values may throw
                // ArgumentOutOfRangeException when the imm argument is not in the valid range
                return nullptr;
            }
        }
        else if (!immOp->IsIntCon())
        {
            if (HWIntrinsicInfo::NoJmpTableImm(intrinsic))
            {
                return ImportNonConstFallback(intrinsic, retType, baseType);
            }

            if (!mustExpand)
            {
                // When the imm-argument is not a constant and we are not being forced to expand, we need to
                // return nullptr so a GT_CALL to the intrinsic method is emitted instead. The
                // intrinsic method is recursive and will be forced to expand, at which point
                // we emit some less efficient fallback code.
                return nullptr;
            }
        }
    }

    if (HWIntrinsicInfo::IsFloatingPointUsed(intrinsic))
    {
        // Set `compFloatingPointUsed` to cover the scenario where an intrinsic is operating on SIMD fields, but
        // where no SIMD local vars are in use. This is the same logic as is used for FEATURE_SIMD.
        comp->compFloatingPointUsed = true;
    }

    if (!impIsTableDrivenHWIntrinsic(intrinsic, category))
    {
        switch (intrinsic)
        {
            case NI_Vector128_get_Count:
#ifdef TARGET_ARM64
            case NI_Vector64_get_Count:
#endif
#ifdef TARGET_XARCH
            case NI_Vector256_get_Count:
#endif
                assert(sig.paramCount == 0);
                assert(sig.retType == TYP_INT);

                {
                    GenTreeIntCon* countNode = comp->gtNewIconNode(getSIMDVectorLength(simdSize, baseType));
                    countNode->gtFlags |= GTF_ICON_SIMD_COUNT;
                    return countNode;
                }

            default:
                return ImportSpecialIntrinsic(intrinsic, sig);
        }
    }

    const bool isScalar = (category == HW_Category_Scalar);

    if (!isScalar && ((HWIntrinsicInfo::GetIns(intrinsic, baseType) == INS_invalid) ||
                      ((simdSize != 8) && (simdSize != 16) && (simdSize != 32))))
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
            return NewVecNode(nodeType, intrinsic, baseType, simdSize);

        case 1:
            op1 = PopHWIntrinsicArg(sig.paramType[0], sig.paramLayout[0]);

            if ((category == HW_Category_MemoryLoad) && op1->OperIs(GT_BITCAST))
            {
                // Although the API specifies a pointer, if what we have is a BYREF, that's what
                // we really want, so throw away the cast.
                if (op1->AsUnOp()->GetOp(0)->TypeIs(TYP_BYREF))
                {
                    op1 = op1->AsUnOp()->GetOp(0);
                }
            }

            retNode = isScalar ? gtNewScalarHWIntrinsicNode(nodeType, intrinsic, op1)
                               : NewVecNode(nodeType, intrinsic, baseType, simdSize, op1);
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
            op2 = AddHWIntrinsicRangeCheckIfNeeded(intrinsic, op2, mustExpand, immLowerBound, immUpperBound);
            op1 = PopHWIntrinsicArg(sig.paramType[0], sig.paramLayout[0]);

            if (!isScalar)
            {
                retNode = NewVecNode(nodeType, intrinsic, baseType, simdSize, op1, op2);
            }
            else
            {
                retNode = gtNewScalarHWIntrinsicNode(nodeType, intrinsic, op1, op2);

                switch (intrinsic)
                {
#ifdef TARGET_XARCH
                    case NI_SSE42_Crc32:
                    case NI_SSE42_X64_Crc32:
#endif
#ifdef TARGET_ARM64
                    case NI_ArmBase_Arm64_MultiplyHigh:
                        assert(sig.retType == TYP_LONG || sig.retType == TYP_ULONG);
                        assert(sig.retType == sig.paramType[0]);
                        assert(sig.retType == sig.paramType[1]);
                        FALLTHROUGH;
                    case NI_Crc32_ComputeCrc32:
                    case NI_Crc32_ComputeCrc32C:
                    case NI_Crc32_Arm64_ComputeCrc32:
                    case NI_Crc32_Arm64_ComputeCrc32C:
#endif
                        retNode->AsHWIntrinsic()->SetSimdBaseType(sig.paramType[1]);
                        break;
                    default:
                        break;
                }
            }

            break;

        case 3:
            op3 = PopHWIntrinsicArg(sig.paramType[2], sig.paramLayout[2]);
            op2 = PopHWIntrinsicArg(sig.paramType[1], sig.paramLayout[1]);
            op1 = PopHWIntrinsicArg(sig.paramType[0], sig.paramLayout[0]);

#ifdef TARGET_ARM64
            assert((category != HW_Category_SIMDByIndexedElement) || varTypeIsSIMD(op2->GetType()));

            if (intrinsic == NI_AdvSimd_LoadAndInsertScalar)
            {
                op2 = AddHWIntrinsicRangeCheckIfNeeded(intrinsic, op2, mustExpand, immLowerBound, immUpperBound);

                // Although the API specifies a pointer, if what we have is a BYREF, that's what
                // we really want, so throw away the cast.
                if (op1->OperIs(GT_BITCAST) && op1->AsUnOp()->GetOp(0)->TypeIs(TYP_BYREF))
                {
                    op1 = op1->AsUnOp()->GetOp(0);
                }
            }
            else if ((intrinsic == NI_AdvSimd_Insert) || (intrinsic == NI_AdvSimd_InsertScalar))
            {
                op2 = AddHWIntrinsicRangeCheckIfNeeded(intrinsic, op2, mustExpand, immLowerBound, immUpperBound);
            }
            else
#endif
            {
                op3 = AddHWIntrinsicRangeCheckIfNeeded(intrinsic, op3, mustExpand, immLowerBound, immUpperBound);
            }

            retNode = isScalar ? gtNewScalarHWIntrinsicNode(nodeType, intrinsic, op1, op2, op3)
                               : NewVecNode(nodeType, intrinsic, baseType, simdSize, op1, op2, op3);

            break;

#ifdef TARGET_ARM64
        case 4:
            op4 = PopHWIntrinsicArg(sig.paramType[3], sig.paramLayout[3]);
            op4 = AddHWIntrinsicRangeCheckIfNeeded(intrinsic, op4, mustExpand, immLowerBound, immUpperBound);
            op3 = PopHWIntrinsicArg(sig.paramType[2], sig.paramLayout[2]);
            op2 = PopHWIntrinsicArg(sig.paramType[1], sig.paramLayout[1]);
            op1 = PopHWIntrinsicArg(sig.paramType[0], sig.paramLayout[0]);

            assert((category != HW_Category_SIMDByIndexedElement) || varTypeIsSIMD(op3->GetType()));
            assert(!isScalar);

            retNode = NewVecNode(nodeType, intrinsic, baseType, simdSize, op1, op2, op3, op4);
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

GenTree* Importer::impVectorGetElement(ClassLayout* layout, GenTree* value, GenTree* index)
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

    return NewVecExtractNode(layout->GetSIMDType(), layout->GetElementType(), value, index);
}

#endif // FEATURE_HW_INTRINSICS
