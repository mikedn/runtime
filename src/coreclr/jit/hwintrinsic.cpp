// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "hwintrinsic.h"

#ifdef FEATURE_HW_INTRINSICS

static const HWIntrinsicInfo hwIntrinsicInfoArray[] = {
// clang-format off
#if defined(TARGET_XARCH)
#define HARDWARE_INTRINSIC(isa, name, size, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, category, flag) \
    {NI_##isa##_##name, #name, InstructionSet_##isa, size, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, category, static_cast<HWIntrinsicFlag>(flag)},
#include "hwintrinsiclistxarch.h"
#elif defined (TARGET_ARM64)
#define HARDWARE_INTRINSIC(isa, name, size, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, category, flag) \
    {NI_##isa##_##name, #name, InstructionSet_##isa, size, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, category, static_cast<HWIntrinsicFlag>(flag)},
#include "hwintrinsiclistarm64.h"
#else
#error Unsupported platform
#endif
    // clang-format on
};

//------------------------------------------------------------------------
// lookup: Gets the HWIntrinsicInfo associated with a given NamedIntrinsic
//
// Arguments:
//    id -- The NamedIntrinsic associated with the HWIntrinsic to lookup
//
// Return Value:
//    The HWIntrinsicInfo associated with id
const HWIntrinsicInfo& HWIntrinsicInfo::lookup(NamedIntrinsic id)
{
    assert(id != NI_Illegal);

    assert(id > NI_HW_INTRINSIC_START);
    assert(id < NI_HW_INTRINSIC_END);

    return hwIntrinsicInfoArray[id - NI_HW_INTRINSIC_START - 1];
}

var_types Compiler::impGetHWIntrinsicBaseTypeFromArg(NamedIntrinsic    intrinsic,
                                                     CORINFO_SIG_INFO* sig,
                                                     var_types         baseType,
                                                     ClassLayout**     argLayout)
{
    assert(HWIntrinsicInfo::BaseTypeFromSecondArg(intrinsic) || HWIntrinsicInfo::BaseTypeFromFirstArg(intrinsic));

    CORINFO_ARG_LIST_HANDLE arg = sig->args;

    if (HWIntrinsicInfo::BaseTypeFromSecondArg(intrinsic))
    {
        arg = info.compCompHnd->getArgNext(arg);
    }

    CORINFO_CLASS_HANDLE argClass;
    CorInfoType          argCorType = strip(info.compCompHnd->getArgType(sig, arg, &argClass));

    if (argCorType == CORINFO_TYPE_PTR)
    {
        argClass = info.compCompHnd->getArgClass(sig, arg);
        CORINFO_CLASS_HANDLE childClassHandle;
        return JITtype2varType(info.compCompHnd->getChildType(argClass, &childClassHandle));
    }

    var_types argType = JITtype2varType(argCorType);

    if (argType != TYP_STRUCT)
    {
        return argType;
    }

    *argLayout = typGetObjLayout(argClass);
    return (*argLayout)->GetElementType();
}

//------------------------------------------------------------------------
// vnEncodesResultTypeForHWIntrinsic(NamedIntrinsic hwIntrinsicID):
//
// Arguments:
//    hwIntrinsicID -- The id for the HW intrinsic
//
// Return Value:
//   Returns true if this intrinsic requires value numbering to add an
//   extra SimdType argument that encodes the resulting type.
//   If we don't do this overloaded versions can return the same VN
//   leading to incorrect CSE subsitutions.
//
/* static */ bool Compiler::vnEncodesResultTypeForHWIntrinsic(NamedIntrinsic hwIntrinsicID)
{
    int numArgs = HWIntrinsicInfo::lookupNumArgs(hwIntrinsicID);

    // HW Intrinsic's with -1 for numArgs have a varying number of args, so we currently
    // give themm a unique value number them, and don't add an extra argument.
    //
    if (numArgs == -1)
    {
        return false;
    }

    // We iterate over all of the different baseType's for this intrinsic in the HWIntrinsicInfo table
    // We set  diffInsCount to the number of instructions that can execute differently.
    //
    unsigned diffInsCount = 0;
#ifdef TARGET_XARCH
    instruction lastIns = INS_invalid;
#endif
    for (var_types baseType = TYP_BYTE; (baseType <= TYP_DOUBLE); baseType = (var_types)(baseType + 1))
    {
        instruction curIns = HWIntrinsicInfo::lookupIns(hwIntrinsicID, baseType);
        if (curIns != INS_invalid)
        {
#ifdef TARGET_XARCH
            if (curIns != lastIns)
            {
                diffInsCount++;
                // remember the last valid instruction that we saw
                lastIns = curIns;
            }
#elif defined(TARGET_ARM64)
            // On ARM64 we use the same instruction and specify an insOpt arrangement
            // so we always consider the instruction operation to be different
            //
            diffInsCount++;
#endif // TARGET
            if (diffInsCount >= 2)
            {
                // We can  early exit the loop now
                break;
            }
        }
    }

    // If we see two (or more) different instructions we need the extra VNF_SimdType arg
    return (diffInsCount >= 2);
}

//------------------------------------------------------------------------
// lookupId: Gets the NamedIntrinsic for a given method name and InstructionSet
//
// Arguments:
//    comp       -- The compiler
//    sig        -- The signature of the intrinsic
//    className  -- The name of the class associated with the HWIntrinsic to lookup
//    methodName -- The name of the method associated with the HWIntrinsic to lookup
//    enclosingClassName -- The name of the enclosing class of X64 classes
//
// Return Value:
//    The NamedIntrinsic associated with methodName and isa
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

    bool isIsaSupported = comp->compHWIntrinsicDependsOn(isa) && comp->compSupportsHWIntrinsic(isa);

    if (strcmp(methodName, "get_IsSupported") == 0)
    {
        return isIsaSupported ? (comp->compExactlyDependsOn(isa) ? NI_IsSupported_True : NI_IsSupported_Dynamic)
                              : NI_IsSupported_False;
    }
    else if (!isIsaSupported)
    {
        return NI_Throw_PlatformNotSupportedException;
    }

    for (int i = 0; i < (NI_HW_INTRINSIC_END - NI_HW_INTRINSIC_START - 1); i++)
    {
        const HWIntrinsicInfo& intrinsicInfo = hwIntrinsicInfoArray[i];

        if (isa != hwIntrinsicInfoArray[i].isa)
        {
            continue;
        }

        int numArgs = static_cast<unsigned>(intrinsicInfo.numArgs);

        if ((numArgs != -1) && (sig->numArgs != static_cast<unsigned>(intrinsicInfo.numArgs)))
        {
            continue;
        }

        if (strcmp(methodName, intrinsicInfo.name) == 0)
        {
            return intrinsicInfo.id;
        }
    }

    // There are several helper intrinsics that are implemented in managed code
    // Those intrinsics will hit this code path and need to return NI_Illegal
    return NI_Illegal;
}

//------------------------------------------------------------------------
// isImmOp: Checks whether the HWIntrinsic node has an imm operand
//
// Arguments:
//    id -- The NamedIntrinsic associated with the HWIntrinsic to lookup
//    op -- The operand to check
//
// Return Value:
//     true if the node has an imm operand; otherwise, false
bool HWIntrinsicInfo::isImmOp(NamedIntrinsic id, const GenTree* op)
{
#ifdef TARGET_XARCH
    if (HWIntrinsicInfo::lookupCategory(id) != HW_Category_IMM)
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

    if (genActualType(op->TypeGet()) != TYP_INT)
    {
        return false;
    }

    return true;
}

GenTree* Compiler::impPopArgForHWIntrinsic(var_types paramType, ClassLayout* paramLayout, bool expectAddr)
{
    if (!varTypeIsStruct(paramType))
    {
        assert(varTypeIsArithmetic(paramType));
        assert(!expectAddr);

        GenTree* arg = impPopStack().val;
        assert(varActualType(arg->GetType()) == varActualType(paramType));
        return arg;
    }

    GenTree* arg = expectAddr ? impSIMDPopStackAddr(paramType) : impSIMDPopStack(paramType);
    assert(varTypeIsSIMD(arg->TypeGet()));
    return arg;
}

//------------------------------------------------------------------------
// addRangeCheckIfNeeded: add a GT_HW_INTRINSIC_CHK node for non-full-range imm-intrinsic
//
// Arguments:
//    intrinsic     -- intrinsic ID
//    immOp         -- the immediate operand of the intrinsic
//    mustExpand    -- true if the compiler is compiling the fallback(GT_CALL) of this intrinsics
//    immLowerBound -- lower incl. bound for a value of the immediate operand (for a non-full-range imm-intrinsic)
//    immUpperBound -- upper incl. bound for a value of the immediate operand (for a non-full-range imm-intrinsic)
//
// Return Value:
//     add a GT_HW_INTRINSIC_CHK node for non-full-range imm-intrinsic, which would throw ArgumentOutOfRangeException
//     when the imm-argument is not in the valid range
//
GenTree* Compiler::addRangeCheckIfNeeded(
    NamedIntrinsic intrinsic, GenTree* immOp, bool mustExpand, int immLowerBound, int immUpperBound)
{
    assert(immOp != nullptr);
    // Full-range imm-intrinsics do not need the range-check
    // because the imm-parameter of the intrinsic method is a byte.
    // AVX2 Gather intrinsics no not need the range-check
    // because their imm-parameter have discrete valid values that are handle by managed code
    if (!mustExpand || !HWIntrinsicInfo::isImmOp(intrinsic, immOp)
#ifdef TARGET_XARCH
        || HWIntrinsicInfo::isAVX2GatherIntrinsic(intrinsic) || HWIntrinsicInfo::HasFullRangeImm(intrinsic)
#endif
            )
    {
        return immOp;
    }

    assert(!immOp->IsIntCon());
    assert(varTypeIsUnsigned(immOp));

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
    GenTree*      adjustedUpperBoundNode = gtNewIconNode(adjustedUpperBound, TYP_INT);

    GenTree* immOpUses[2];
    impMakeMultiUse(immOp, 2, immOpUses, CHECK_SPILL_ALL DEBUGARG("hw intrinsic range check temp"));

    if (immLowerBound != 0)
    {
        immOpUses[1] = gtNewOperNode(GT_SUB, TYP_INT, immOpUses[1], gtNewIconNode(immLowerBound, TYP_INT));
    }

    GenTreeBoundsChk* hwIntrinsicChk = new (this, GT_HW_INTRINSIC_CHK)
        GenTreeBoundsChk(GT_HW_INTRINSIC_CHK, immOpUses[1], adjustedUpperBoundNode, SCK_ARG_RNG_EXCPN);

    return gtNewCommaNode(hwIntrinsicChk, immOpUses[0]);
}

//------------------------------------------------------------------------
// compSupportsHWIntrinsic: check whether a given instruction is enabled via configuration
//
// Arguments:
//    isa - Instruction set
//
// Return Value:
//    true iff the given instruction set is enabled via configuration (environment variables, etc.).
bool Compiler::compSupportsHWIntrinsic(CORINFO_InstructionSet isa)
{
    return JitConfig.EnableHWIntrinsic() && (featureSIMD || HWIntrinsicInfo::isScalarIsa(isa)) &&
           (
#ifdef DEBUG
               JitConfig.EnableIncompleteISAClass() ||
#endif
               HWIntrinsicInfo::isFullyImplementedIsa(isa));
}

//------------------------------------------------------------------------
// impIsTableDrivenHWIntrinsic:
//
// Arguments:
//    intrinsicId - HW intrinsic id
//    category - category of a HW intrinsic
//
// Return Value:
//    returns true if this category can be table-driven in the importer
//
static bool impIsTableDrivenHWIntrinsic(NamedIntrinsic intrinsicId, HWIntrinsicCategory category)
{
    return (category != HW_Category_Special) && HWIntrinsicInfo::RequiresCodegen(intrinsicId) &&
           !HWIntrinsicInfo::HasSpecialImport(intrinsicId);
}

//------------------------------------------------------------------------
// isSupportedBaseType
//
// Arguments:
//    intrinsicId - HW intrinsic id
//    baseType - Base type of the intrinsic.
//
// Return Value:
//    returns true if the baseType is supported for given intrinsic.
//
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

    retType = JITtype2varType(sig->retType);

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
        paramType[i] = JITtype2varType(strip(vm->getArgType(sig, param, &paramClass)));

        if (paramType[i] != TYP_STRUCT)
        {
            paramLayout[i] = nullptr;
        }
        else
        {
            if (prevClass != paramClass)
            {
                prevClass  = paramClass;
                prevLayout = compiler->typGetObjLayout(prevClass);
            }

            paramLayout[i] = prevLayout;
            paramType[i]   = prevLayout->IsVector() ? prevLayout->GetSIMDType() : TYP_STRUCT;
        }
    }
}

//------------------------------------------------------------------------
// impHWIntrinsic: Import a hardware intrinsic as a GT_HWINTRINSIC node if possible
//
// Arguments:
//    intrinsic  -- id of the intrinsic function.
//    clsHnd     -- class handle containing the intrinsic function.
//    method     -- method handle of the intrinsic function.
//    sig        -- signature of the intrinsic call
//    mustExpand -- true if the intrinsic must return a GenTree*; otherwise, false

// Return Value:
//    The GT_HWINTRINSIC node, or nullptr if not a supported intrinsic
//
GenTree* Compiler::impHWIntrinsic(NamedIntrinsic        intrinsic,
                                  CORINFO_CLASS_HANDLE  clsHnd,
                                  CORINFO_METHOD_HANDLE method,
                                  CORINFO_SIG_INFO*     sig,
                                  bool                  mustExpand)
{
    CORINFO_InstructionSet isa      = HWIntrinsicInfo::lookupIsa(intrinsic);
    HWIntrinsicCategory    category = HWIntrinsicInfo::lookupCategory(intrinsic);
    var_types              baseType = TYP_UNDEF;
    unsigned               simdSize = static_cast<unsigned>(HWIntrinsicInfo::lookup(intrinsic).simdSize);

    HWIntrinsicSignature sigReader;
    sigReader.Read(this, sig);
    var_types    retType   = sigReader.retType;
    ClassLayout* retLayout = sigReader.retLayout;

    if ((retLayout != nullptr) && featureSIMD)
    {
        // Currently all HW intrinsics return either vectors or primitive types, not structs.
        if (!retLayout->IsVector())
        {
            return nullptr;
        }

        baseType = retLayout->GetElementType();
        retType  = retLayout->GetSIMDType();
    }

    if (HWIntrinsicInfo::BaseTypeFromSecondArg(intrinsic) || HWIntrinsicInfo::BaseTypeFromFirstArg(intrinsic))
    {
        ClassLayout* argLayout = nullptr;
        baseType               = impGetHWIntrinsicBaseTypeFromArg(intrinsic, sig, baseType, &argLayout);

        if ((simdSize == UINT32_MAX) && (argLayout != nullptr))
        {
            simdSize = argLayout->GetSize();
        }
    }
    else if (retLayout != nullptr)
    {
        if (simdSize == UINT32_MAX)
        {
            simdSize = retLayout->GetSize();
        }
    }

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
    if ((category != HW_Category_Special) && (category != HW_Category_Scalar) &&
        !isSupportedBaseType(intrinsic, baseType))
    {
        return nullptr;
    }

    GenTree* immOp = nullptr;

#ifdef TARGET_ARM64
    if ((intrinsic == NI_AdvSimd_Insert) || (intrinsic == NI_AdvSimd_InsertScalar) ||
        (intrinsic == NI_AdvSimd_LoadAndInsertScalar))
    {
        assert(sigReader.paramCount == 3);
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

        assert(sigReader.paramCount == 4);

        immOp  = impStackTop(2).val;
        immOp2 = impStackTop().val;

        assert(HWIntrinsicInfo::isImmOp(intrinsic, immOp));
        assert(HWIntrinsicInfo::isImmOp(intrinsic, immOp2));

        if (!immOp2->IsCnsIntOrI())
        {
            assert(HWIntrinsicInfo::NoJmpTableImm(intrinsic));
            return impNonConstFallback(intrinsic, retType, baseType);
        }

        ClassLayout* sourceVectorLayout = sigReader.paramLayout[2];
        assert(sourceVectorLayout->IsVector());
        unsigned  otherSimdSize = sourceVectorLayout->GetSize();
        var_types otherBaseType = sourceVectorLayout->GetElementType();

        assert(otherBaseType == baseType);

        int immLowerBound2 = 0;
        int immUpperBound2 = 0;

        HWIntrinsicInfo::lookupImmBounds(intrinsic, otherSimdSize, otherBaseType, &immLowerBound2, &immUpperBound2);

        const int immVal2 = (int)immOp2->AsIntCon()->IconValue();

        if ((immVal2 < immLowerBound2) || (immVal2 > immUpperBound2))
        {
            assert(!mustExpand);
            return nullptr;
        }
    }
    else
#endif
        if ((sigReader.paramCount > 0) && HWIntrinsicInfo::isImmOp(intrinsic, impStackTop().val))
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
            var_types    indexedElementBaseType;
            unsigned int indexedElementSimdSize = 0;

            if (sigReader.paramCount == 3)
            {
                ClassLayout* layout = sigReader.paramLayout[1];
                assert(layout->IsVector());
                indexedElementBaseType = layout->GetElementType();
                indexedElementSimdSize = layout->GetSize();
            }
            else
            {
                assert(sigReader.paramCount == 4);

                ClassLayout* layout = sigReader.paramLayout[2];
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
            HWIntrinsicInfo::lookupImmBounds(intrinsic, indexedElementSimdSize, baseType, &immLowerBound,
                                             &immUpperBound);
        }
        else
        {
            HWIntrinsicInfo::lookupImmBounds(intrinsic, simdSize, baseType, &immLowerBound, &immUpperBound);
        }
#endif

        if (!hasFullRangeImm && immOp->IsCnsIntOrI())
        {
            const int ival = (int)immOp->AsIntCon()->IconValue();
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
        else if (!immOp->IsCnsIntOrI())
        {
            if (HWIntrinsicInfo::NoJmpTableImm(intrinsic))
            {
                return impNonConstFallback(intrinsic, retType, baseType);
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
        compFloatingPointUsed = true;
    }

    if (!impIsTableDrivenHWIntrinsic(intrinsic, category))
    {
        return impSpecialIntrinsic(intrinsic, sigReader, baseType, retType, simdSize);
    }

    const bool isScalar = (category == HW_Category_Scalar);

    if (!isScalar && ((HWIntrinsicInfo::lookupIns(intrinsic, baseType) == INS_invalid) ||
                      ((simdSize != 8) && (simdSize != 16) && (simdSize != 32))))
    {
        assert(!"Unexpected HW Intrinsic");
        return nullptr;
    }

    if (sigReader.paramCount == 0)
    {
        assert(!isScalar);
        return gtNewSimdHWIntrinsicNode(retType, intrinsic, baseType, simdSize);
    }

    GenTree*            op1     = nullptr;
    GenTree*            op2     = nullptr;
    GenTree*            op3     = nullptr;
    GenTree*            op4     = nullptr;
    GenTreeHWIntrinsic* retNode = nullptr;

    switch (sigReader.paramCount)
    {
        case 1:
            op1 = impPopArgForHWIntrinsic(sigReader.paramType[0], sigReader.paramLayout[0]);

            if ((category == HW_Category_MemoryLoad) && op1->OperIs(GT_CAST))
            {
                // Although the API specifies a pointer, if what we have is a BYREF, that's what
                // we really want, so throw away the cast.
                if (op1->gtGetOp1()->TypeGet() == TYP_BYREF)
                {
                    op1 = op1->gtGetOp1();
                }
            }

            retNode = isScalar ? gtNewScalarHWIntrinsicNode(retType, intrinsic, op1)
                               : gtNewSimdHWIntrinsicNode(retType, intrinsic, baseType, simdSize, op1);
            break;

        case 2:
            op2 = impPopArgForHWIntrinsic(sigReader.paramType[1], sigReader.paramLayout[1]);
            op2 = addRangeCheckIfNeeded(intrinsic, op2, mustExpand, immLowerBound, immUpperBound);
            op1 = impPopArgForHWIntrinsic(sigReader.paramType[0], sigReader.paramLayout[0]);

            retNode = isScalar ? gtNewScalarHWIntrinsicNode(retType, intrinsic, op1, op2)
                               : gtNewSimdHWIntrinsicNode(retType, intrinsic, baseType, simdSize, op1, op2);

#ifdef TARGET_XARCH
            if ((intrinsic == NI_SSE42_Crc32) || (intrinsic == NI_SSE42_X64_Crc32))
            {
                // TODO-XArch-Cleanup: currently we use the BaseType to bring the type of the second argument
                // to the code generator. May encode the overload info in other way.
                retNode->AsHWIntrinsic()->gtSIMDBaseType = sigReader.paramType[1];
            }
#elif defined(TARGET_ARM64)
            switch (intrinsic)
            {
                case NI_Crc32_ComputeCrc32:
                case NI_Crc32_ComputeCrc32C:
                case NI_Crc32_Arm64_ComputeCrc32:
                case NI_Crc32_Arm64_ComputeCrc32C:
                    retNode->AsHWIntrinsic()->gtSIMDBaseType = sigReader.paramType[1];
                    break;

                case NI_AdvSimd_AddWideningUpper:
                case NI_AdvSimd_SubtractWideningUpper:
                    assert(varTypeIsSIMD(op1->TypeGet()));
                    retNode->AsHWIntrinsic()->SetAuxiliaryType(sigReader.paramLayout[0]->GetElementType());
                    break;

                case NI_AdvSimd_Arm64_AddSaturateScalar:
                    assert(varTypeIsSIMD(op2->TypeGet()));
                    retNode->AsHWIntrinsic()->SetAuxiliaryType(sigReader.paramLayout[1]->GetElementType());
                    break;

                case NI_ArmBase_Arm64_MultiplyHigh:
                    if (sig->retType == CORINFO_TYPE_ULONG)
                    {
                        retNode->AsHWIntrinsic()->gtSIMDBaseType = TYP_ULONG;
                    }
                    else
                    {
                        assert(sig->retType == CORINFO_TYPE_LONG);
                        retNode->AsHWIntrinsic()->gtSIMDBaseType = TYP_LONG;
                    }
                    break;

                default:
                    break;
            }
#endif
            break;

        case 3:
            op3 = impPopArgForHWIntrinsic(sigReader.paramType[2], sigReader.paramLayout[2]);
            op2 = impPopArgForHWIntrinsic(sigReader.paramType[1], sigReader.paramLayout[1]);
            op1 = impPopArgForHWIntrinsic(sigReader.paramType[0], sigReader.paramLayout[0]);

#ifdef TARGET_ARM64
            if (intrinsic == NI_AdvSimd_LoadAndInsertScalar)
            {
                op2 = addRangeCheckIfNeeded(intrinsic, op2, mustExpand, immLowerBound, immUpperBound);

                if (op1->OperIs(GT_CAST))
                {
                    // Although the API specifies a pointer, if what we have is a BYREF, that's what
                    // we really want, so throw away the cast.
                    if (op1->gtGetOp1()->TypeGet() == TYP_BYREF)
                    {
                        op1 = op1->gtGetOp1();
                    }
                }
            }
            else if ((intrinsic == NI_AdvSimd_Insert) || (intrinsic == NI_AdvSimd_InsertScalar))
            {
                op2 = addRangeCheckIfNeeded(intrinsic, op2, mustExpand, immLowerBound, immUpperBound);
            }
            else
#endif
            {
                op3 = addRangeCheckIfNeeded(intrinsic, op3, mustExpand, immLowerBound, immUpperBound);
            }

            retNode = isScalar ? gtNewScalarHWIntrinsicNode(retType, intrinsic, op1, op2, op3)
                               : gtNewSimdHWIntrinsicNode(retType, intrinsic, baseType, simdSize, op1, op2, op3);

#ifdef TARGET_XARCH
            if ((intrinsic == NI_AVX2_GatherVector128) || (intrinsic == NI_AVX2_GatherVector256))
            {
                assert(varTypeIsSIMD(op2->TypeGet()));
                retNode->AsHWIntrinsic()->SetAuxiliaryType(sigReader.paramLayout[1]->GetElementType());
            }
#elif defined(TARGET_ARM64)
            if (category == HW_Category_SIMDByIndexedElement)
            {
                assert(varTypeIsSIMD(op2->TypeGet()));
                retNode->AsHWIntrinsic()->SetAuxiliaryType(op2->TypeGet());
            }
#endif
            break;

#ifdef TARGET_ARM64
        case 4:
            op4 = impPopArgForHWIntrinsic(sigReader.paramType[3], sigReader.paramLayout[3]);
            op4 = addRangeCheckIfNeeded(intrinsic, op4, mustExpand, immLowerBound, immUpperBound);
            op3 = impPopArgForHWIntrinsic(sigReader.paramType[2], sigReader.paramLayout[2]);
            op2 = impPopArgForHWIntrinsic(sigReader.paramType[1], sigReader.paramLayout[1]);
            op1 = impPopArgForHWIntrinsic(sigReader.paramType[0], sigReader.paramLayout[0]);

            assert(!isScalar);
            retNode = gtNewSimdHWIntrinsicNode(retType, intrinsic, baseType, simdSize, op1, op2, op3, op4);

            if (category == HW_Category_SIMDByIndexedElement)
            {
                assert(varTypeIsSIMD(op3->TypeGet()));
                retNode->AsHWIntrinsic()->SetAuxiliaryType(op3->TypeGet());
            }
            break;
#endif

        default:
            return nullptr;
    }

    const bool isMemoryStore = retNode->OperIsMemoryStore();

    if (isMemoryStore || retNode->OperIsMemoryLoad())
    {
        if (isMemoryStore)
        {
            // A MemoryStore operation is an assignment
            retNode->gtFlags |= GTF_ASG;
        }

        // This operation contains an implicit indirection
        //   it could point into the gloabal heap or
        //   it could throw a null reference exception.
        //
        retNode->gtFlags |= (GTF_GLOB_REF | GTF_EXCEPT);
    }
    return retNode;
}

#ifdef DEBUG
const char* GetHWIntrinsicIdName(NamedIntrinsic id)
{
    static const char* const names[] = {
#if defined(TARGET_XARCH)
#define HARDWARE_INTRINSIC(isa, name, ...) #isa "_" #name,
#include "hwintrinsiclistxarch.h"
#elif defined(TARGET_ARM64)
#define HARDWARE_INTRINSIC(isa, name, ...) #isa "_" #name,
#include "hwintrinsiclistarm64.h"
#endif // !defined(TARGET_XARCH) && !defined(TARGET_ARM64)
    };

    return (id > NI_HW_INTRINSIC_START && id < NI_HW_INTRINSIC_END) ? names[id - NI_HW_INTRINSIC_START - 1] : "NI_???";
}
#endif

#endif // FEATURE_HW_INTRINSICS
