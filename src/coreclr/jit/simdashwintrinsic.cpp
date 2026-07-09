// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef FEATURE_SIMD

static constexpr bool varTypeIsInt(var_types t)
{
    return (t == TYP_INT) || (t == TYP_UINT);
}

static constexpr bool varTypeIsLong(var_types t)
{
    return (t == TYP_LONG) || (t == TYP_ULONG);
}

static constexpr bool varTypeIsSigned(var_types t)
{
    return varTypeIsSmallSigned(t) || (t == TYP_INT) || (t == TYP_LONG);
}

static constexpr bool varTypeIsUnsigned(var_types t)
{
    return varTypeIsSmallUnsigned(t) || (t == TYP_UINT) || (t == TYP_ULONG);
}

#endif // FEATURE_SIMD

#ifdef FEATURE_HW_INTRINSICS

enum class SysNumSimdIntrinsicClassId : uint8_t
{
    Unknown,
    Vector2,
    Vector3,
    Vector4,
    VectorT128,
    VectorT256,
};

enum class SysNumSimdIntrinsicFlag : uint8_t
{
    None    = 0,
    HasThis = 1
};

static constexpr SysNumSimdIntrinsicFlag operator|(SysNumSimdIntrinsicFlag lhs, SysNumSimdIntrinsicFlag rhs)
{
    return static_cast<SysNumSimdIntrinsicFlag>(static_cast<unsigned>(lhs) | static_cast<unsigned>(rhs));
}

static constexpr SysNumSimdIntrinsicFlag operator&(SysNumSimdIntrinsicFlag lhs, SysNumSimdIntrinsicFlag rhs)
{
    return static_cast<SysNumSimdIntrinsicFlag>(static_cast<unsigned>(lhs) & static_cast<unsigned>(rhs));
}

struct SysNumSimdIntrinsicInfo
{
    const char*                name;
    SysNumSimdIntrinsicClassId classId : 4;
    SysNumSimdIntrinsicFlag    flags : 4;
    uint8_t                    numArgs;
    NamedIntrinsic             hwIntrinsic[10];

    NamedIntrinsic HWIntrinsic(var_types type) const
    {
        if ((type < TYP_BYTE) || (type > TYP_DOUBLE))
        {
            assert(!"Unexpected type");
            return NI_Illegal;
        }
        return hwIntrinsic[type - TYP_BYTE];
    }

    bool HasThis() const
    {
        return (flags & SysNumSimdIntrinsicFlag::HasThis) == SysNumSimdIntrinsicFlag::HasThis;
    }
};

static constexpr SysNumSimdIntrinsicInfo sysNumSimdIntrinsicInfo[]
{
// clang-format off
#define SIMD_AS_HWINTRINSIC(classId, id, name, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, flags) \
    {name, SysNumSimdIntrinsicClassId::classId, flags, numarg, {t1, t2, t3, t4, t5, t6, t7, t8, t9, t10}},
// clang-format on
#if defined(TARGET_XARCH)
#include "simdashwintrinsiclistxarch.h"
#elif defined(TARGET_ARM64)
#include "simdashwintrinsiclistarm64.h"
#else
#error Unsupported platform
#endif
};

static_assert_no_msg(_countof(sysNumSimdIntrinsicInfo) ==
                     NI_SIMD_AS_HWINTRINSIC_LAST - NI_SIMD_AS_HWINTRINSIC_FIRST + 1);

static const SysNumSimdIntrinsicInfo& GetIntrinsicInfo(NamedIntrinsic id)
{
    assert(NI_SIMD_AS_HWINTRINSIC_FIRST <= id && id <= NI_SIMD_AS_HWINTRINSIC_LAST);

    return sysNumSimdIntrinsicInfo[id - NI_SIMD_AS_HWINTRINSIC_FIRST];
}

static SysNumSimdIntrinsicClassId FindClassId(const char* className)
{
    assert(className != nullptr);

    if (className[0] != 'V')
    {
        return SysNumSimdIntrinsicClassId::Unknown;
    }
    if (strcmp(className, "Vector2") == 0)
    {
        return SysNumSimdIntrinsicClassId::Vector2;
    }
    if (strcmp(className, "Vector3") == 0)
    {
        return SysNumSimdIntrinsicClassId::Vector3;
    }
    if (strcmp(className, "Vector4") == 0)
    {
        return SysNumSimdIntrinsicClassId::Vector4;
    }
    if ((strcmp(className, "Vector") == 0) || (strcmp(className, "Vector`1") == 0))
    {
        return SysNumSimdIntrinsicClassId::VectorT128;
    }

    return SysNumSimdIntrinsicClassId::Unknown;
}

NamedIntrinsic Compiler::impFindSysNumSimdIntrinsic(CORINFO_METHOD_HANDLE method,
                                                    const char*           className,
                                                    const char*           methodName)
{
    if (!JitConfig.EnableHWIntrinsic() || !opts.SIMDFeature())
    {
        return NI_Illegal;
    }

    SysNumSimdIntrinsicClassId classId = FindClassId(className);

    if (classId == SysNumSimdIntrinsicClassId::Unknown)
    {
        return NI_Illegal;
    }

#ifdef TARGET_XARCH
    if ((classId == SysNumSimdIntrinsicClassId::VectorT128) && (GetVectorTSize() == 32))
    {
        classId = SysNumSimdIntrinsicClassId::VectorT256;
    }
#endif

    CORINFO_SIG_INFO sig;
    info.compCompHnd->getMethodSig(method, &sig);
    bool     hasThis = sig.hasThis();
    unsigned numArgs = sig.numArgs + hasThis;

    for (unsigned i = 0; i < _countof(sysNumSimdIntrinsicInfo); i++)
    {
        const SysNumSimdIntrinsicInfo& info = sysNumSimdIntrinsicInfo[i];

        if ((classId != info.classId) || (numArgs != info.numArgs) || (hasThis != info.HasThis()))
        {
            continue;
        }

        if (strcmp(methodName, info.name) != 0)
        {
            continue;
        }

        return static_cast<NamedIntrinsic>(NI_SIMD_AS_HWINTRINSIC_FIRST + i);
    }

    return NI_Illegal;
}

#if defined(TARGET_XARCH)
constexpr CORINFO_InstructionSet MinimumIsa = InstructionSet_SSE2;
#elif defined(TARGET_ARM64)
constexpr CORINFO_InstructionSet MinimumIsa = InstructionSet_AdvSimd;
#else
#error Unsupported platform
#endif

GenTree* Importer::ImportSysNumVecIntrinsic(NamedIntrinsic        intrinsic,
                                            CORINFO_CLASS_HANDLE  clsHnd,
                                            CORINFO_METHOD_HANDLE method,
                                            CORINFO_SIG_INFO*     sig,
                                            bool                  isNewObj)
{
    assert(JitConfig.EnableHWIntrinsic() && opts.SIMDFeature());

    bool isSupported = comp->compOpportunisticallyDependsOn(MinimumIsa);

    if (intrinsic == NI_VectorT128_get_IsHardwareAccelerated
#ifdef TARGET_XARCH
        || intrinsic == NI_VectorT256_get_IsHardwareAccelerated
#endif
        )
    {
        return comp->gtNewIconNode(isSupported);
    }

    if (!isSupported)
    {
        return nullptr;
    }

#ifdef TARGET_XARCH
    if ((NI_VectorT256_Abs <= intrinsic) && (intrinsic <= NI_VectorT256_Widen))
    {
        comp->compExactlyDependsOn(InstructionSet_AVX2);
    }
#endif

    HWIntrinsicSignature signature;
    signature.Read(comp, sig);

    const char* namespaceName = nullptr;
    const char* className     = info.compCompHnd->getClassNameFromMetadata(clsHnd, &namespaceName);

    ClassLayout* layout = nullptr;

    if (strcmp(className, "Vector") == 0)
    {
        assert(!signature.hasThisParam);
        assert(signature.paramCount != 0);

        layout = signature.paramLayout[0];

        // Ignore generic instantiations that use invalid element types. Note that
        // for As<To, From> the first parameter and the return are both supposed
        // to be vectors but they can have different element types.
        if (!layout->IsVector() || ((signature.retLayout != nullptr) && !signature.retLayout->IsVector()))
        {
            return nullptr;
        }
    }
    else
    {
        // If it isn't the static Vector class then this must be one of the vector types
        // in System.Numerics - Vector2/3/4/<T>. Note that all System.Numerics intrinsic
        // types are structs so we shouldn't need the isValueClass check but it looks
        // like we can also get here when devirtualizing IEquatable`1.Equals and then the
        // class is IEquatable`1 and not the original vector struct.

        if (!info.compCompHnd->isValueClass(clsHnd))
        {
            return nullptr;
        }

        layout = typGetObjLayout(clsHnd);

        // Ignore generic instantiations that use invalid element types.
        if (!layout->IsVector())
        {
            return nullptr;
        }
    }

    NamedIntrinsic hwIntrinsic = GetIntrinsicInfo(intrinsic).HWIntrinsic(layout->GetElementType());

    if (hwIntrinsic == NI_Illegal)
    {
        return nullptr;
    }

    comp->compFloatingPointUsed = true;

    if (hwIntrinsic == NI_SIMD_AS_HWINTRINSIC_START)
    {
        return impVector234TSpecial(intrinsic, signature, layout, isNewObj);
    }

    if (!comp->compOpportunisticallyDependsOn(HWIntrinsicInfo::GetIsa(hwIntrinsic)))
    {
        return nullptr;
    }

    var_types eltType = layout->GetElementType();
    unsigned  size    = layout->GetSize();

    assert(!signature.hasThisParam);

#if defined(TARGET_XARCH)
    if (size < 16)
#elif defined(TARGET_ARM64)
    if (size == 12)
#else
#error Unsupported platform
#endif
    {
        size = 16;

        if (varTypeIsNonTargetVec(signature.retType))
        {
            signature.retType = varTypeTargetVec(signature.retType);
        }

        if ((signature.paramCount > 0) && varTypeIsNonTargetVec(signature.paramType[0]))
        {
            signature.paramType[0] = varTypeTargetVec(signature.paramType[0]);
        }

        if ((signature.paramCount > 1) && varTypeIsNonTargetVec(signature.paramType[1]))
        {
            signature.paramType[1] = varTypeTargetVec(signature.paramType[1]);
        }
    }

    switch (signature.paramCount)
    {
        GenTree* ops[2];

        case 0:
            assert(varTypeIsVec(signature.retType));

            switch (hwIntrinsic)
            {
#ifdef TARGET_ARM64
                case NI_Vector64_get_Zero:
#endif
                case NI_Vector128_get_Zero:
#ifdef TARGET_XARCH
                case NI_Vector256_get_Zero:
#endif
                    hwIntrinsic = NI_VEC_ZERO;
                    break;
                case NI_Vector128_get_AllBitsSet:
#ifdef TARGET_XARCH
                case NI_Vector256_get_AllBitsSet:
#endif
                    hwIntrinsic = NI_VEC_ONE_BITS;
                    break;
                default:
                    break;
            }

            return NewVecNode(signature.retType, hwIntrinsic, eltType, size);

        case 1:
            assert(signature.retType == signature.paramType[0]);
            ops[0] = PopVec(signature.paramType[0]);
            return NewVecNode(signature.retType, hwIntrinsic, eltType, size, ops[0]);

        default:
            assert(signature.paramCount == 2);
            assert(signature.retType == signature.paramType[0]);
            assert(signature.paramLayout[0] == signature.paramLayout[1]);
            ops[1] = PopVec(signature.paramType[1]);
            ops[0] = PopVec(signature.paramType[0]);
            return NewVecNode(signature.retType, hwIntrinsic, eltType, size, ops[0], ops[1]);
    }
}

GenTree* Importer::impVector234TSpecial(NamedIntrinsic              intrinsic,
                                        const HWIntrinsicSignature& sig,
                                        ClassLayout*                layout,
                                        bool                        isNewObj)
{
    // Intrinsics that have non-SIMD parameters or aren't always supported
    // and thus may not need to pop the arguments from the stack.

    switch (intrinsic)
    {
        case NI_Vector2_get_One:
        case NI_Vector3_get_One:
        case NI_Vector4_get_One:
        case NI_VectorT128_get_One:
#ifdef TARGET_XARCH
        case NI_VectorT256_get_One:
#endif
            return impVector234TOne(sig);
        case NI_VectorT128_get_Count:
#ifdef TARGET_XARCH
        case NI_VectorT256_get_Count:
#endif
            return impVectorTCount(sig, layout);
        case NI_VectorT128_FromArray:
#ifdef TARGET_XARCH
        case NI_VectorT256_FromArray:
#endif
            return impVectorTFromArray(sig, layout, isNewObj);
        case NI_VectorT128_CreateBroadcast:
#ifdef TARGET_XARCH
        case NI_VectorT256_CreateBroadcast:
#endif
            if (sig.paramType[0] == TYP_REF)
            {
                return impVectorTFromArray(sig, layout, isNewObj);
            }
            FALLTHROUGH;
        case NI_Vector2_CtorSplat:
        case NI_Vector3_CtorSplat:
        case NI_Vector4_CtorSplat:
            return impVector234TCtorSplat(sig, layout, isNewObj);
        case NI_Vector2_CtorPack:
        case NI_Vector3_CtorPack:
        case NI_Vector4_CtorPack:
            return impVector234CtorPack(sig, layout, isNewObj);
        case NI_Vector3_CtorExtend1:
        case NI_Vector4_CtorExtend1:
        case NI_Vector4_CtorExtend2:
            return impVector34CtorExtend(sig, layout, isNewObj);
        case NI_Vector2_CopyTo:
        case NI_Vector2_CopyToAt:
        case NI_Vector3_CopyTo:
        case NI_Vector3_CopyToAt:
        case NI_Vector4_CopyTo:
        case NI_Vector4_CopyToAt:
        case NI_VectorT128_CopyTo:
        case NI_VectorT128_CopyToAt:
#ifdef TARGET_XARCH
        case NI_VectorT256_CopyTo:
        case NI_VectorT256_CopyToAt:
#endif
            return impVector234TCopyTo(sig, layout);
        case NI_VectorT128_get_Item:
#ifdef TARGET_XARCH
        case NI_VectorT256_get_Item:
#endif
            return impVectorTGetItem(sig, layout);
        case NI_Vector2_Equals:
        case NI_Vector3_Equals:
        case NI_Vector4_Equals:
        case NI_VectorT128_EqualsInstance:
#ifdef TARGET_XARCH
        case NI_VectorT256_EqualsInstance:
#endif
            return impVector234TInstanceEquals(sig);
        case NI_VectorT128_op_Multiply:
#ifdef TARGET_XARCH
        case NI_VectorT256_op_Multiply:
#endif
            return impVectorTMultiply(sig);
        case NI_VectorT128_Widen:
            return impVectorT128Widen(sig);
#ifdef TARGET_XARCH
        case NI_VectorT256_Widen:
            return impVectorT256Widen(sig);
        case NI_VectorT128_ConvertToInt64:
            return impVectorT128ConvertDoubleToInt64(sig);
        case NI_VectorT256_ConvertToInt64:
            return impVectorT256ConvertDoubleToInt64(sig);
        case NI_VectorT128_Sum:
            return impVectorT128Sum(sig);
        case NI_VectorT256_Sum:
            return impVectorT256Sum(sig);
        case NI_VectorT128_Dot:
            return impVectorT128Dot(sig);
        case NI_VectorT256_Dot:
            return impVectorT256Dot(sig);
#endif
        default:
            break;
    }

    // Intrinsics that have only SIMD parameters and are always supported.

    assert(!sig.hasThisParam);
    assert((1 <= sig.paramCount) && (sig.paramCount <= 3));

    GenTree* ops[3];

    for (unsigned i = sig.paramCount; i != 0; i--)
    {
        ops[i - 1] = PopVec(sig.paramType[i - 1]);
    }

    switch (intrinsic)
    {
        case NI_VectorT128_op_Explicit:
        case NI_VectorT128_As:
#ifdef TARGET_XARCH
        case NI_VectorT256_op_Explicit:
        case NI_VectorT256_As:
#endif
            assert(sig.paramCount == 1);
            assert(sig.paramType[0] == sig.retType);
            return ops[0];
        case NI_Vector2_op_Equality:
        case NI_Vector3_op_Equality:
        case NI_Vector4_op_Equality:
        case NI_VectorT128_op_Equality:
#ifdef TARGET_XARCH
        case NI_VectorT256_op_Equality:
#endif
            return impVector234TEquals(sig, ops[0], ops[1]);
        case NI_Vector2_op_Inequality:
        case NI_Vector3_op_Inequality:
        case NI_Vector4_op_Inequality:
        case NI_VectorT128_op_Inequality:
#ifdef TARGET_XARCH
        case NI_VectorT256_op_Inequality:
#endif
            return impVector234TEquals(sig, ops[0], ops[1], true);
        case NI_Vector2_Dot:
        case NI_Vector3_Dot:
        case NI_Vector4_Dot:
            return impVector234Dot(sig, ops[0], ops[1]);
        case NI_VectorT128_ConditionalSelect:
            return impVectorT128ConditionalSelect(sig, ops[0], ops[1], ops[2]);
        case NI_VectorT128_Max:
            return impVectorT128MinMax(sig, ops[0], ops[1], true);
        case NI_VectorT128_Min:
            return impVectorT128MinMax(sig, ops[0], ops[1], false);
        case NI_VectorT128_Narrow:
            return impVectorT128Narrow(sig, ops[0], ops[1]);

#ifdef TARGET_ARM64
        case NI_VectorT128_Abs:
            assert(sig.paramCount == 1);
            assert(varTypeIsUnsigned(sig.retLayout->GetElementType()));
            return ops[0];
        case NI_VectorT128_Sum:
            return impVectorT128Sum(sig, ops[0]);
        case NI_VectorT128_Dot:
            return impVectorT128Dot(sig, ops[0], ops[1]);
#endif // TARGET_ARM64

#ifdef TARGET_XARCH
        case NI_Vector2_Abs:
        case NI_Vector3_Abs:
        case NI_Vector4_Abs:
        case NI_VectorT128_Abs:
            return impVector234T128Abs(sig, ops[0]);
        case NI_VectorT256_Abs:
            return impVectorT256Abs(sig, ops[0]);
        case NI_VectorT128_AndNot:
        case NI_VectorT256_AndNot:
            return impVectorTAndNot(sig, ops[0], ops[1]);
        case NI_VectorT256_ConvertToInt32:
            return NewVecNode(TYP_SIMD32, NI_AVX_ConvertToVector256Int32WithTruncation, TYP_INT, ops[0]);
        case NI_VectorT128_ConvertToSingle:
            return impVectorT128ConvertUInt32ToSingle(sig, ops[0]);
        case NI_VectorT256_ConvertToSingle:
            assert(sig.paramCount == 1);
            assert((sig.retType == TYP_SIMD32) && (sig.retType == sig.paramType[0]));
            if (sig.paramLayout[0]->GetElementType() == TYP_INT)
            {
                assert(sig.retLayout->GetElementType() == TYP_FLOAT);
                return NewVecNode(TYP_SIMD32, NI_AVX_ConvertToVector256Single, TYP_FLOAT, ops[0]);
            }
            return impVectorT256ConvertUInt32ToSingle(sig, ops[0]);
        case NI_VectorT128_ConvertToDouble:
            assert(sig.paramCount == 1);
            assert((sig.retType == TYP_SIMD16) && (sig.retType == sig.paramType[0]));
            if (sig.paramLayout[0]->GetElementType() == TYP_ULONG)
            {
                return impVectorT128ConvertUInt64ToDouble(sig, ops[0]);
            }
            return impVectorT128ConvertInt64ToDouble(sig, ops[0]);
        case NI_VectorT256_ConvertToDouble:
            assert(sig.paramCount == 1);
            assert((sig.retType == TYP_SIMD32) && (sig.retType == sig.paramType[0]));
            if (sig.paramLayout[0]->GetElementType() == TYP_ULONG)
            {
                return impVectorT256ConvertUInt64ToDouble(sig, ops[0]);
            }
            return impVectorT256ConvertInt64ToDouble(sig, ops[0]);
        case NI_VectorT128_Equals:
            return impVectorT128LongEquals(sig, ops[0], ops[1]);
        case NI_VectorT128_GreaterThan:
        case NI_VectorT128_GreaterThanOrEqual:
        case NI_VectorT128_LessThan:
        case NI_VectorT128_LessThanOrEqual:
            return impVectorT128Compare(sig, intrinsic, ops[0], ops[1]);
        case NI_VectorT256_GreaterThan:
        case NI_VectorT256_LessThan:
            assert(sig.paramCount == 2);
            assert(varTypeIsUnsigned(sig.paramLayout[0]->GetElementType()));
            FALLTHROUGH;
        case NI_VectorT256_GreaterThanOrEqual:
        case NI_VectorT256_LessThanOrEqual:
            return impVectorT256Compare(sig, intrinsic, ops[0], ops[1]);
        case NI_VectorT256_Max:
            return impVectorT256MinMax(sig, ops[0], ops[1], true);
        case NI_VectorT256_Min:
            return impVectorT256MinMax(sig, ops[0], ops[1], false);
        case NI_VectorT256_Narrow:
            return impVectorT256Narrow(sig, ops[0], ops[1]);
        case NI_VectorT256_ConditionalSelect:
            return impVectorT256ConditionalSelect(sig, ops[0], ops[1], ops[2]);
#endif // TARGET_XARCH

        default:
            unreached();
    }
}

GenTree* Importer::impVector234TOne(const HWIntrinsicSignature& sig)
{
    assert(varTypeIsSIMD(sig.retType));
    assert(sig.paramCount == 0);

    var_types vecType = varTypeTargetVec(sig.retLayout->GetVectorType());
    var_types eltType = varTypeNodeType(sig.retLayout->GetElementType());

    return NewVecNode(vecType, NI_VEC_SPLAT, eltType, gtNewOneConNode(eltType));
}

GenTree* Importer::impVectorTCount(const HWIntrinsicSignature& sig, ClassLayout* layout)
{
    assert(sig.retType == TYP_INT);
    assert(sig.paramCount == 0);

    GenTreeIntCon* countNode = comp->gtNewIconNode(layout->GetElementCount(), TYP_INT);
    countNode->gtFlags |= GTF_ICON_SIMD_COUNT;
    return countNode;
}

GenTree* Importer::impVector234TCtorSplat(const HWIntrinsicSignature& sig, ClassLayout* layout, bool isNewObj)
{
    assert(sig.retType == TYP_VOID);
    assert(sig.hasThisParam);
    assert(layout->IsVector());
    assert(sig.paramCount == 1);

    var_types type     = layout->GetVectorType();
    var_types vecType  = varTypeTargetVec(type);
    var_types eltType  = varTypeNodeType(layout->GetElementType());
    GenTree*  arg      = impPopStackCoerceArg(varActualType(sig.paramType[0]));
    GenTree*  destAddr = isNewObj ? nullptr : impPopStack().val;
    GenTree*  create;

    if (arg->IsIntegralConst(0) || arg->IsDblConPositiveZero())
    {
        create = NewVecZeroNode(vecType, eltType);
    }
    else
    {
        create = NewVecNode(vecType, NI_VEC_SPLAT, eltType, arg);
    }

    if (destAddr != nullptr)
    {
        return impVectorStore(type, destAddr, create);
    }

    return create;
}

GenTree* Importer::impVector234CtorPack(const HWIntrinsicSignature& sig, ClassLayout* layout, bool isNewObj)
{
    assert(sig.retType == TYP_VOID);
    assert(sig.hasThisParam);
    assert(sig.paramCount == layout->GetElementCount());
    assert(layout->GetVectorKind() == VectorKind::Vector234);
    assert(layout->GetElementType() == TYP_FLOAT);

    GenTree* args[4];
    assert(sig.paramCount <= _countof(args));
    bool areArgsContiguous = sig.paramCount > 1;
    bool areArgsZero       = true;

    for (unsigned i = 0; i < sig.paramCount; i++)
    {
        unsigned argIndex = sig.paramCount - i - 1;
        assert(sig.paramType[i] == TYP_FLOAT);
        args[argIndex] = impPopStackCoerceArg(TYP_FLOAT);

        if ((i > 0) && areArgsContiguous)
        {
            // We're popping the args off the stack in reverse order so we already have the next arg.
            areArgsContiguous = SIMDCoalescingBuffer::AreContiguousLoads(args[argIndex], args[argIndex + 1]);
        }

        if (!args[argIndex]->IsDblConPositiveZero())
        {
            areArgsZero = false;
        }
    }

    var_types type     = layout->GetVectorType();
    var_types vecType  = varTypeTargetVec(type);
    var_types eltType  = varTypeNodeType(layout->GetElementType());
    GenTree*  destAddr = isNewObj ? nullptr : impPopStack().val;
    GenTree*  create;

    if (areArgsContiguous)
    {
        SIMDCoalescingBuffer::ChangeToSIMDLoad(comp, args[0], type);

        create = args[0];

        if ((destAddr != nullptr) && destAddr->OperIs(GT_LCL_ADDR) && (destAddr->AsLclAddr()->GetLclOffs() == 0))
        {
            comp->lvaRecordSimdIntrinsicUse(destAddr->AsLclAddr()->GetLcl());
        }
    }
    else if (areArgsZero)
    {
        create = NewVecZeroNode(vecType, eltType);
    }
    else
    {
        unsigned argCount = sig.paramCount;

        switch (argCount)
        {
#ifdef TARGET_XARCH
            case 2:
                args[2] = comp->gtNewDconNode(0, TYP_FLOAT);
                FALLTHROUGH;
#endif
            case 3:
                args[3]  = comp->gtNewDconNode(0, TYP_FLOAT);
                argCount = 4;
                break;
            default:
                break;
        }

        create = NewVecNode(vecType, NI_VEC_PACK, TYP_FLOAT, argCount, args);
    }

    if (destAddr != nullptr)
    {
        return impVectorStore(type, destAddr, create);
    }

    return create;
}

GenTree* Importer::impVector34CtorExtend(const HWIntrinsicSignature& sig, ClassLayout* layout, bool isNewObj)
{
    assert(sig.retType == TYP_VOID);
    assert(sig.hasThisParam);
    assert(layout->GetVectorKind() == VectorKind::Vector234);

    GenTree* args[3];
    assert(sig.paramCount <= _countof(args));

    for (unsigned i = sig.paramCount - 1; i > 0; i--)
    {
        args[i] = impPopStackCoerceArg(TYP_FLOAT);
    }

    args[0] = PopVec(sig.paramType[0]);

    GenTree* destAddr = isNewObj ? nullptr : impPopStack().val;
    GenTree* create;

    unsigned insertIndex = sig.paramType[0] == TYP_SIMD12 ? 3 : 2;

#ifdef TARGET_ARM64
    create = args[0];

    for (unsigned i = 1; i < sig.paramCount; i++)
    {
        create = NewVecNode(TYP_SIMD16, NI_AdvSimd_Insert, TYP_FLOAT, create, comp->gtNewIconNode(insertIndex + i - 1),
                            args[i]);
    }
#elif defined(TARGET_XARCH)
    if (sig.paramCount == 3)
    {
        args[1] = NewVecNode(TYP_SIMD16, NI_VEC_REGCAST, TYP_FLOAT, args[1]);
        args[2] = NewVecNode(TYP_SIMD16, NI_VEC_REGCAST, TYP_FLOAT, args[2]);
        create  = NewVecNode(TYP_SIMD16, NI_SSE_UnpackLow, TYP_FLOAT, args[1], args[2]);
        create  = NewVecNode(TYP_SIMD16, NI_SSE_MoveLowToHigh, TYP_FLOAT, args[0], create);
    }
    else if (comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        create =
            NewVecNode(TYP_SIMD16, NI_SSE41_Insert, TYP_FLOAT, args[0], args[1], comp->gtNewIconNode(insertIndex << 4));
    }
    else if (insertIndex == 2)
    {
        create = NewVecZeroNode(TYP_SIMD16, TYP_FLOAT);
        create = NewVecNode(TYP_SIMD16, NI_SSE_MoveScalar, TYP_FLOAT, create, args[1]);
        create = NewVecNode(TYP_SIMD16, NI_SSE_MoveLowToHigh, TYP_FLOAT, args[0], create);
    }
    else
    {
        assert(insertIndex == 3);

        GenTree* arg0Uses[3];
        impMakeMultiUse(args[0], 3, arg0Uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector3 extend temp"));

        args[1] = NewVecNode(TYP_SIMD16, NI_VEC_REGCAST, TYP_FLOAT, args[1]);

        create = NewVecNode(TYP_SIMD16, NI_SSE_MoveHighToLow, TYP_FLOAT, arg0Uses[0], arg0Uses[1]);
        create = NewVecNode(TYP_SIMD16, NI_SSE_UnpackLow, TYP_FLOAT, create, args[1]);
        create = NewVecNode(TYP_SIMD16, NI_SSE_MoveLowToHigh, TYP_FLOAT, arg0Uses[2], create);
    }
#else
#error Unsupported platform
#endif

    if (destAddr != nullptr)
    {
        return impVectorStore(layout->GetSIMDType(), destAddr, create);
    }

    return create;
}

GenTree* Importer::impVectorStore(var_types type, GenTree* destAddr, GenTree* src)
{
    assert(destAddr->TypeIs(TYP_BYREF, TYP_I_IMPL));
    assert(src->OperIs(GT_IND_LOAD, GT_HWINTRINSIC));
    assert(varTypeTargetVec(type) == varTypeTargetVec(src->GetType()));

    GenTree* store;

    if (destAddr->OperIs(GT_LCL_ADDR) && (destAddr->AsLclAddr()->GetLcl()->GetType() == type))
    {
        LclVarDsc* lcl = destAddr->AsLclAddr()->GetLcl();
        // Currently the importer doesn't generate local field addresses.
        assert(destAddr->AsLclAddr()->GetLclOffs() == 0);

        store = destAddr->ChangeToLclStore(lcl->GetType(), lcl, src);

        if (GenTreeHWIntrinsic* hwi = src->IsHWIntrinsic())
        {
            comp->lvaRecordSimdIntrinsicDef(store->AsLclStore(), hwi);
        }
    }
    else
    {
        store = comp->gtNewIndStore(type, destAddr, src);
        store->gtFlags |= GTF_GLOB_REF | comp->gtGetIndirExceptionFlags(destAddr);
    }

    return store;
}

GenTree* Importer::impGetArrayElementsAsVectorAddr(ClassLayout*    layout,
                                                   GenTree*        array,
                                                   GenTree*        index,
                                                   ThrowHelperKind indexThrowKind,
                                                   ThrowHelperKind lastIndexThrowKind)
{
    assert(array->TypeIs(TYP_REF));
    assert((index == nullptr) || (varActualType(index->GetType()) == TYP_INT));

    if ((index != nullptr) && index->IsIntCon(0))
    {
        index = nullptr;
    }

    GenTree* arrayUses[3];
    impMakeMultiUse(array, index == nullptr ? 2 : 3, arrayUses, CHECK_SPILL_ALL DEBUGARG("Vector<T>.CopyTo temp"));
    array = arrayUses[0];

    GenTree* lastIndex = comp->gtNewIconNode(layout->GetElementCount() - 1);
    GenTree* arrLen    = comp->gtNewArrLen(arrayUses[1], OFFSETOF__CORINFO_Array__length);

    if (index != nullptr)
    {
        GenTree* indexUses[3];
        impMakeMultiUse(index, indexUses, CHECK_SPILL_ALL DEBUGARG("Vector<T>.CopyTo temp"));
        index = indexUses[0];

        lastIndex = comp->gtNewOperNode(GT_ADD, TYP_INT, indexUses[1], lastIndex);
        array     = comp->gtNewCommaNode(comp->gtNewBoundsChk(lastIndex, arrLen, lastIndexThrowKind), array);
        arrLen    = comp->gtNewArrLen(arrayUses[2], OFFSETOF__CORINFO_Array__length);
        array     = comp->gtNewCommaNode(comp->gtNewBoundsChk(indexUses[2], arrLen, indexThrowKind), array);
    }
    else
    {
        array = comp->gtNewCommaNode(comp->gtNewBoundsChk(lastIndex, arrLen, lastIndexThrowKind), array);
    }

    GenTree* offset = comp->gtNewIconNode(OFFSETOF__CORINFO_Array__data, TYP_I_IMPL);

    if (index != nullptr)
    {
        GenTree* elementSize = comp->gtNewIconNode(varTypeSize(layout->GetElementType()), TYP_I_IMPL);
#ifdef TARGET_64BIT
        index = comp->gtNewOperNode(GT_SXT, TYP_LONG, index);
#endif
        index = comp->gtNewOperNode(GT_MUL, TYP_I_IMPL, index, elementSize);
        // TODO-MIKE-CQ: This should be removed, it's here only to minimize diffs
        // from the previous implementation that imported SIMDIntrinsicInitArray
        // as is, hiding the address mode and thus blocking CSE.
        index->gtFlags |= GTF_DONT_CSE;
        offset = comp->gtNewOperNode(GT_ADD, TYP_I_IMPL, index, offset);
        offset->gtFlags |= GTF_DONT_CSE;
    }

    GenTree* addr = comp->gtNewOperNode(GT_ADD, TYP_BYREF, array, offset);
    addr->gtFlags |= GTF_DONT_CSE;

    return addr;
}

GenTree* Importer::impVectorTFromArray(const HWIntrinsicSignature& sig, ClassLayout* layout, bool isNewObj)
{
    assert((sig.paramCount == 1) || (sig.paramCount == 2));
    assert(sig.paramType[0] == TYP_REF);
    assert((sig.paramCount == 1) || (sig.paramType[1] == TYP_INT));

    GenTree* index    = sig.paramCount == 1 ? nullptr : impPopStackCoerceArg(TYP_INT);
    GenTree* array    = impPopStackCoerceArg(TYP_REF);
    GenTree* destAddr = isNewObj ? nullptr : impPopStack().val;

    GenTree* srcAddr = impGetArrayElementsAsVectorAddr(layout, array, index, ThrowHelperKind::IndexOutOfRange,
                                                       ThrowHelperKind::IndexOutOfRange);

    GenTreeIndir* src = comp->gtNewIndLoad(layout->GetSIMDType(), srcAddr);
    src->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;

    return destAddr == nullptr ? src : impVectorStore(src->GetType(), destAddr, src);
}

GenTree* Importer::impVector234TCopyTo(const HWIntrinsicSignature& sig, ClassLayout* layout)
{
    assert(sig.retType == TYP_VOID);
    assert(sig.hasThisParam);
    assert((sig.paramCount == 1) || (sig.paramCount == 2));
    assert(sig.paramType[0] == TYP_REF);
    assert((sig.paramCount == 1) || (sig.paramType[1] == TYP_INT));

    GenTree* index = sig.paramCount == 1 ? nullptr : impPopStackCoerceArg(TYP_INT);
    GenTree* array = impPopStackCoerceArg(TYP_REF);
    GenTree* value = PopVecAddrLoad(layout->GetSIMDType());

    GenTree* destAddr = impGetArrayElementsAsVectorAddr(layout, array, index, ThrowHelperKind::ArgumentOutOfRange,
                                                        ThrowHelperKind::Argument);

    GenTreeIndir* dest = comp->gtNewIndStore(layout->GetSIMDType(), destAddr, value);
    dest->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;
    return dest;
}

GenTree* Importer::impVectorTGetItem(const HWIntrinsicSignature& sig, ClassLayout* layout)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_INT);

    GenTree* index = impPopStackCoerceArg(TYP_INT);
    GenTree* value = PopVecAddrLoad(layout->GetSIMDType());

    return impVecExtract(layout, value, index);
}

GenTree* Importer::impVector234TInstanceEquals(const HWIntrinsicSignature& sig)
{
    assert(sig.retType == TYP_BOOL);
    assert(sig.hasThisParam && (sig.paramCount == 1));

    GenTree* op1 = PopVec(sig.paramType[0]);
    GenTree* op2 = PopVecAddrLoad(sig.paramType[0]);

    return impVector234TEquals(sig, op1, op2);
}

#ifdef TARGET_ARMARCH

GenTree* Importer::impVector234TEquals(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool notEqual)
{
    assert(sig.retType == TYP_BOOL);
    assert((sig.hasThisParam && (sig.paramCount == 1)) || (sig.paramCount == 2));
    assert(sig.hasThisParam || (sig.paramLayout[0] == sig.paramLayout[1]));

    ClassLayout* layout    = sig.paramLayout[0];
    var_types    type      = layout->GetVectorType();
    var_types    eltType   = layout->GetElementType();
    unsigned     size      = layout->GetSize();
    bool         isVector3 = type == TYP_SIMD12;

    if (isVector3)
    {
        type = TYP_SIMD16;
        size = 16;
    }

    NamedIntrinsic intrinsic = varTypeSize(eltType) == 8 ? NI_AdvSimd_Arm64_CompareEqual : NI_AdvSimd_CompareEqual;
    op1                      = NewVecNode(type, intrinsic, eltType, size, op1, op2);

    if (isVector3)
    {
        op1 = comp->gtNewVecInsertNode(TYP_INT, op1, comp->gtNewIconNode(3), comp->gtNewIconNode(-1));
    }

    op1 = NewVecNode(type, NI_AdvSimd_Arm64_MinAcross, TYP_UBYTE, size, op1);
    op1 = NewVecExtractNode(TYP_UBYTE, op1, comp->gtNewIconNode(0));
    return comp->gtNewOperNode(notEqual ? GT_EQ : GT_NE, TYP_INT, op1, comp->gtNewIconNode(0));
}

GenTree* Importer::impVectorT128ConditionalSelect(const HWIntrinsicSignature& sig,
                                                  GenTree*                    mask,
                                                  GenTree*                    op1,
                                                  GenTree*                    op2)
{
    assert(sig.paramCount == 3);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert((sig.retLayout == sig.paramLayout[1]) && (sig.retLayout == sig.paramLayout[2]));
    assert(sig.retType == TYP_SIMD16);

    var_types eltType = sig.retLayout->GetElementType();

    return NewVecNode(TYP_SIMD16, NI_AdvSimd_BitwiseSelect, eltType, mask, op1, op2);
}

GenTree* Importer::impVector234Dot(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.paramCount == 2);
    assert(sig.paramType[0] != TYP_SIMD32);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.retType == TYP_FLOAT);

    if (sig.paramType[0] == TYP_SIMD8)
    {
        op1 = NewVecNode(TYP_SIMD8, NI_AdvSimd_Multiply, TYP_FLOAT, op1, op2);
        return NewVecNode(TYP_FLOAT, NI_AdvSimd_Arm64_AddPairwiseScalar, TYP_FLOAT, 8, op1);
    }

    op1 = NewVecNode(TYP_SIMD16, NI_AdvSimd_Multiply, TYP_FLOAT, op1, op2);

    if (sig.paramType[0] == TYP_SIMD12)
    {
        op1 = comp->gtNewVecInsertNode(TYP_FLOAT, op1, comp->gtNewIconNode(3), comp->gtNewDconNode(0, TYP_FLOAT));
    }

    return NewVecNode(TYP_FLOAT, NI_VEC_SUM, TYP_FLOAT, 16, op1);
}

GenTree* Importer::impVectorT128Sum(const HWIntrinsicSignature& sig, GenTree* op1)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD16);

    var_types eltType = sig.paramLayout[0]->GetElementType();

    if (eltType == TYP_FLOAT)
    {
        return NewVecNode(TYP_FLOAT, NI_VEC_SUM, TYP_FLOAT, 16, op1);
    }

    if (eltType == TYP_DOUBLE)
    {
        return NewVecNode(TYP_DOUBLE, NI_AdvSimd_Arm64_AddPairwiseScalar, TYP_DOUBLE, 16, op1);
    }

    eltType = varTypeToSigned(eltType);

    op1 = NewVecNode(TYP_SIMD16, eltType == TYP_LONG ? NI_AdvSimd_Arm64_AddPairwiseScalar : NI_AdvSimd_Arm64_AddAcross,
                     eltType, op1);
    return NewVecExtractNode(sig.retType, op1, comp->gtNewIconNode(0));
}

GenTree* Importer::impVectorT128Dot(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.paramCount == 2);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);

    ClassLayout* layout  = sig.paramLayout[0];
    var_types    eltType = layout->GetElementType();

    if (eltType == TYP_FLOAT)
    {
        op1 = NewVecNode(TYP_SIMD16, NI_AdvSimd_Multiply, TYP_FLOAT, op1, op2);
        return NewVecNode(TYP_FLOAT, NI_VEC_SUM, TYP_FLOAT, 16, op1);
    }

    if (eltType == TYP_DOUBLE)
    {
        op1 = NewVecNode(TYP_SIMD16, NI_AdvSimd_Arm64_Multiply, TYP_DOUBLE, op1, op2);
        return NewVecNode(TYP_DOUBLE, NI_AdvSimd_Arm64_AddPairwiseScalar, TYP_DOUBLE, 16, op1);
    }

    eltType = varTypeToSigned(eltType);

    if (eltType == TYP_LONG)
    {
        // Since we eventually need a scalar result it's cheaper to simply extract
        // the 2 long elements and perform scalar multiplication/addition.

        GenTree* op1Uses[2];
        impMakeMultiUse(op1, op1Uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<long>.Multiply temp"));
        GenTree* op2Uses[2];
        impMakeMultiUse(op2, op2Uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<long>.Multiply temp"));

        op1 = NewVecExtractNode(TYP_LONG, op1Uses[0], comp->gtNewIconNode(0));
        op2 = NewVecExtractNode(TYP_LONG, op2Uses[0], comp->gtNewIconNode(0));

        GenTree* mul1 = comp->gtNewOperNode(GT_MUL, TYP_LONG, op1, op2);

        op1 = NewVecExtractNode(TYP_LONG, op1Uses[1], comp->gtNewIconNode(1));
        op2 = NewVecExtractNode(TYP_LONG, op2Uses[1], comp->gtNewIconNode(1));

        GenTree* mul2 = comp->gtNewOperNode(GT_MUL, TYP_LONG, op1, op2);

        return comp->gtNewOperNode(GT_ADD, TYP_LONG, mul1, mul2);
    }

    op1 = NewVecNode(TYP_SIMD16, NI_AdvSimd_Multiply, eltType, op1, op2);
    op1 = NewVecNode(TYP_SIMD16, NI_AdvSimd_Arm64_AddAcross, eltType, op1);
    return NewVecExtractNode(eltType, op1, comp->gtNewIconNode(0));
}

GenTree* Importer::impVectorT128MinMax(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool isMax)
{
    assert(sig.paramCount == 2);
    assert((sig.retLayout == sig.paramLayout[0]) && (sig.retLayout == sig.paramLayout[1]));
    assert(sig.retType == TYP_SIMD16);
    assert(varTypeIsLong(sig.retLayout->GetElementType()));

    ClassLayout* layout  = sig.retLayout;
    var_types    eltType = layout->GetElementType();

    NamedIntrinsic intrinsic = isMax ? NI_AdvSimd_Arm64_CompareGreaterThan : NI_AdvSimd_Arm64_CompareLessThan;

    GenTree* uses[2][2];
    impMakeMultiUse(op1, uses[0], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Max/Min temp"));
    impMakeMultiUse(op2, uses[1], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Max/Min temp"));

    GenTree* condition = NewVecNode(TYP_SIMD16, intrinsic, eltType, uses[0][0], uses[1][0]);
    return NewVecNode(TYP_SIMD16, NI_AdvSimd_BitwiseSelect, eltType, condition, uses[0][1], uses[1][1]);
}

GenTree* Importer::impVectorT128Narrow(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.retType == TYP_SIMD16);
    assert(sig.paramCount == 2);
    assert((sig.paramType[0] == TYP_SIMD16) && (sig.paramLayout[0] == sig.paramLayout[1]));
    assert(varTypeSize(sig.retLayout->GetElementType()) == varTypeSize(sig.paramLayout[0]->GetElementType()) / 2);

    NamedIntrinsic lower;
    NamedIntrinsic upper;

    if (sig.paramLayout[0]->GetElementType() == TYP_DOUBLE)
    {
        lower = NI_AdvSimd_Arm64_ConvertToSingleLower;
        upper = NI_AdvSimd_Arm64_ConvertToSingleUpper;
    }
    else
    {
        lower = NI_AdvSimd_ExtractNarrowingLower;
        upper = NI_AdvSimd_ExtractNarrowingUpper;
    }

    var_types retEltType = sig.retLayout->GetElementType();

    op1 = NewVecNode(TYP_SIMD16, lower, retEltType, 8, op1);
    return NewVecNode(TYP_SIMD16, upper, retEltType, op1, op2);
}

GenTree* Importer::impVectorT128Widen(const HWIntrinsicSignature& sig)
{
    assert(sig.retType == TYP_VOID);
    assert(sig.paramCount == 3);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramType[1] == TYP_BYREF);
    assert(sig.paramType[2] == TYP_BYREF);

    GenTree* hiAddr = impPopStack().val;
    GenTree* loAddr = impPopStack().val;
    GenTree* value  = PopVec(sig.paramType[0]);

    GenTree* uses[2];
    impMakeMultiUse(value, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));

    var_types      eltType = sig.paramLayout[0]->GetElementType();
    NamedIntrinsic lower;
    NamedIntrinsic upper;

    if (eltType == TYP_FLOAT)
    {
        lower = NI_AdvSimd_Arm64_ConvertToDouble;
        upper = NI_AdvSimd_Arm64_ConvertToDoubleUpper;
    }
    else if (varTypeIsSigned(eltType))
    {
        lower = NI_AdvSimd_SignExtendWideningLower;
        upper = NI_AdvSimd_SignExtendWideningUpper;
    }
    else
    {
        lower = NI_AdvSimd_ZeroExtendWideningLower;
        upper = NI_AdvSimd_ZeroExtendWideningUpper;
    }

    GenTree* lo = NewVecNode(TYP_SIMD16, lower, eltType, 8, uses[0]);
    GenTree* hi = NewVecNode(TYP_SIMD16, upper, eltType, uses[1]);
    impSpillAllAppendTree(impVectorStore(TYP_SIMD16, loAddr, lo));
    return impVectorStore(TYP_SIMD16, hiAddr, hi);
}

GenTree* Importer::impVectorTMultiplyLong(const HWIntrinsicSignature& sig)
{
    assert(sig.retType == TYP_SIMD16);
    assert(varTypeIsLong(sig.retLayout->GetElementType()));
    assert(sig.paramCount == 2);

    GenTree* op1;
    GenTree* op2;

    if (sig.paramLayout[0] == nullptr)
    {
        assert(sig.paramType[0] == TYP_LONG);
        assert(sig.paramLayout[1] == sig.retLayout);

        op2 = PopVec(sig.paramType[1]);
        op1 = impPopStack().val;
    }
    else if (sig.paramLayout[1] == nullptr)
    {
        assert(sig.paramLayout[0] == sig.retLayout);
        assert(sig.paramType[1] == TYP_LONG);

        op2 = impPopStack().val;
        op1 = PopVec(sig.paramType[0]);
    }
    else
    {
        assert((sig.retLayout == sig.paramLayout[0]) && (sig.retLayout == sig.paramLayout[1]));

        op2 = PopVec(sig.paramType[0]);
        op1 = PopVec(sig.paramType[0]);
    }

    GenTree* op1Uses[2];
    GenTree* op2Uses[2];

    if (sig.paramLayout[0] != nullptr)
    {
        impMakeMultiUse(op1, op1Uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<long>.Multiply temp"));
        op1Uses[0] = NewVecExtractNode(TYP_LONG, op1Uses[0], comp->gtNewIconNode(0));
        op1Uses[1] = NewVecExtractNode(TYP_LONG, op1Uses[1], comp->gtNewIconNode(1));
    }
    else
    {
        impMakeMultiUse(op1, op1Uses, CHECK_SPILL_ALL DEBUGARG("Vector<long>.Multiply temp"));
    }

    if (sig.paramLayout[1] != nullptr)
    {
        impMakeMultiUse(op2, op2Uses, sig.paramLayout[1], CHECK_SPILL_ALL DEBUGARG("Vector<long>.Multiply temp"));
        op2Uses[0] = NewVecExtractNode(TYP_LONG, op2Uses[0], comp->gtNewIconNode(0));
        op2Uses[1] = NewVecExtractNode(TYP_LONG, op2Uses[1], comp->gtNewIconNode(1));
    }
    else
    {
        impMakeMultiUse(op2, op2Uses, CHECK_SPILL_ALL DEBUGARG("Vector<long>.Multiply temp"));
    }

    GenTree* mul1 = comp->gtNewOperNode(GT_MUL, TYP_LONG, op1Uses[0], op2Uses[0]);
    GenTree* mul2 = comp->gtNewOperNode(GT_MUL, TYP_LONG, op1Uses[1], op2Uses[1]);

    return NewVecNode(TYP_SIMD16, NI_VEC_PACK, TYP_LONG, mul1, mul2);
}

GenTree* Importer::impVectorTMultiply(const HWIntrinsicSignature& sig)
{
    assert(sig.retType == TYP_SIMD16);
    assert(sig.paramCount == 2);

    var_types vecType = sig.retType;
    var_types eltType = sig.retLayout->GetElementType();

    if (varTypeIsLong(eltType))
    {
        return impVectorTMultiplyLong(sig);
    }

    assert(vecType == TYP_SIMD16);

    NamedIntrinsic intrinsic = eltType == TYP_DOUBLE ? NI_AdvSimd_Arm64_Multiply : NI_AdvSimd_Multiply;
    GenTree*       op1;
    GenTree*       op2;

    if (sig.paramLayout[0] == nullptr)
    {
        assert(sig.paramType[0] == eltType);
        assert(sig.paramLayout[1] == sig.retLayout);

        op2 = PopVec(sig.paramType[1]);
        op1 = impPopStack().val;

        const bool canSwap = !op1->HasSideEffects() && !op2->HasSideEffects();

        if (varTypeIsFloating(eltType) && canSwap)
        {
            intrinsic = eltType == TYP_DOUBLE ? NI_AdvSimd_Arm64_MultiplyByScalar : NI_AdvSimd_MultiplyByScalar;

            op1 = NewVecNode(TYP_SIMD16, NI_VEC_REGCAST, eltType, op1);
        }
        else
        {
            op1 = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, eltType, op1);
        }

        if (canSwap)
        {
            std::swap(op1, op2);
        }
    }
    else if (sig.paramLayout[1] == nullptr)
    {
        assert(sig.paramLayout[0] == sig.retLayout);
        assert(sig.paramType[1] == eltType);

        op2 = impPopStack().val;
        op1 = PopVec(sig.paramType[0]);

        if (varTypeIsFloating(eltType))
        {
            intrinsic = eltType == TYP_DOUBLE ? NI_AdvSimd_Arm64_MultiplyByScalar : NI_AdvSimd_MultiplyByScalar;

            op2 = NewVecNode(TYP_SIMD16, NI_VEC_REGCAST, eltType, op2);
        }
        else
        {
            op2 = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, eltType, op2);
        }
    }
    else
    {
        assert((sig.retLayout == sig.paramLayout[0]) && (sig.retLayout == sig.paramLayout[1]));

        op2 = PopVec(sig.paramType[0]);
        op1 = PopVec(sig.paramType[0]);
    }

    return NewVecNode(TYP_SIMD16, intrinsic, eltType, op1, op2);
}

#endif // TARGET_ARMARCH

#ifdef TARGET_XARCH

GenTree* Importer::impVector234T128Abs(const HWIntrinsicSignature& sig, GenTree* op1)
{
    assert(sig.retType != TYP_SIMD32);
    assert(sig.paramCount == 1);
    assert(sig.retLayout == sig.paramLayout[0]);

    ClassLayout* layout  = sig.retLayout;
    var_types    eltType = layout->GetElementType();

    if (varTypeIsUnsigned(eltType))
    {
        return op1;
    }

    if (varTypeIsFloating(eltType))
    {
        GenTree*       mask;
        NamedIntrinsic intrinsic;

        if (eltType == TYP_FLOAT)
        {
            mask      = comp->gtNewDconNode(jitstd::bit_cast<float, int32_t>(0x7fffffff), TYP_FLOAT);
            intrinsic = NI_SSE_And;
        }
        else
        {
            assert(eltType == TYP_DOUBLE);
            mask      = comp->gtNewDconNode(jitstd::bit_cast<double, int64_t>(0x7fffffffffffffffLL), TYP_DOUBLE);
            intrinsic = NI_SSE2_And;
        }

        mask = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, eltType, mask);
        return NewVecNode(TYP_SIMD16, intrinsic, eltType, op1, mask);
    }

    if ((eltType != TYP_LONG) && comp->compOpportunisticallyDependsOn(InstructionSet_SSSE3))
    {
        return NewVecNode(TYP_SIMD16, NI_SSSE3_Abs, eltType, op1);
    }

    GenTree* uses[2];
    impMakeMultiUse(op1, uses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Abs temp"));

    GenTree* sign;

    if (eltType == TYP_BYTE)
    {
        sign = NewVecZeroNode(layout);
        sign = NewVecNode(TYP_SIMD16, NI_SSE2_CompareGreaterThan, TYP_BYTE, sign, uses[0]);
    }
    else if ((eltType == TYP_SHORT) || (eltType == TYP_INT))
    {
        sign = comp->gtNewIconNode(varTypeBitSize(eltType) - 1);
        sign = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightArithmetic, eltType, uses[0], sign);
    }
    else if (comp->compOpportunisticallyDependsOn(InstructionSet_SSE42))
    {
        sign = NewVecZeroNode(layout);
        sign = NewVecNode(TYP_SIMD16, NI_SSE42_CompareGreaterThan, TYP_LONG, sign, uses[0]);
    }
    else
    {
        sign = comp->gtNewIconNode(31);
        sign = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightArithmetic, TYP_INT, uses[0], sign);
        sign = NewVecNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, sign, comp->gtNewIconNode(0b11110101));
    }

    GenTree* signUses[2];
    impMakeMultiUse(sign, signUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Abs sign temp"));

    GenTree* tmp = NewVecNode(TYP_SIMD16, NI_SSE2_Xor, eltType, signUses[0], uses[1]);
    return NewVecNode(TYP_SIMD16, NI_SSE2_Subtract, eltType, tmp, signUses[1]);
}

GenTree* Importer::impVectorT256Abs(const HWIntrinsicSignature& sig, GenTree* op1)
{
    assert(sig.paramCount == 1);
    assert(sig.retLayout == sig.paramLayout[0]);
    assert(sig.retType == TYP_SIMD32);

    ClassLayout* layout  = sig.retLayout;
    var_types    eltType = layout->GetElementType();

    if (varTypeIsUnsigned(eltType))
    {
        return op1;
    }

    if (varTypeIsFloating(eltType))
    {
        GenTree* mask;

        if (eltType == TYP_FLOAT)
        {
            mask = comp->gtNewDconNode(jitstd::bit_cast<float, int32_t>(0x7fffffff), TYP_FLOAT);
        }
        else
        {
            assert(eltType == TYP_DOUBLE);
            mask = comp->gtNewDconNode(jitstd::bit_cast<double, int64_t>(0x7fffffffffffffffLL), TYP_DOUBLE);
        }

        mask = NewVecNode(TYP_SIMD32, NI_VEC_SPLAT, eltType, mask);
        return NewVecNode(TYP_SIMD32, NI_AVX_And, eltType, op1, mask);
    }

    if (eltType == TYP_LONG)
    {
        GenTree* uses[2];
        impMakeMultiUse(op1, uses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Abs temp"));

        GenTree* zero = NewVecZeroNode(layout);
        GenTree* sign = NewVecNode(TYP_SIMD32, NI_AVX2_CompareGreaterThan, TYP_LONG, zero, uses[0]);
        GenTree* signUses[2];
        impMakeMultiUse(sign, signUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Abs sign temp"));

        GenTree* tmp = NewVecNode(TYP_SIMD32, NI_AVX2_Xor, eltType, signUses[0], uses[1]);
        return NewVecNode(TYP_SIMD32, NI_AVX2_Subtract, eltType, tmp, signUses[1]);
    }

    return NewVecNode(TYP_SIMD32, NI_AVX2_Abs, eltType, op1);
}

GenTree* Importer::impVectorTAndNot(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.paramCount == 2);
    assert((sig.retLayout == sig.paramLayout[0]) && (sig.retLayout == sig.paramLayout[1]));

    // PANDN/ANDNPS/ANDNPS is actually ~x & y rather than x & ~y
    // so we need to swap the operand order.

    if (!gtCanSwapOrder(op1, op2))
    {
        // TODO-MIKE-Review: Can we simply set GTF_REVERSE_OPS to avoid creating a temp?

        LclVarDsc* tempLcl = lvaAllocTemp(true DEBUGARG("Vector<T>.AndNot temp"));
        impAppendTempStore(tempLcl, op1, sig.paramLayout[0], CHECK_SPILL_ALL);
        op1 = comp->gtNewLclLoad(tempLcl, sig.paramType[0]);
    }

    var_types type    = sig.retLayout->GetSIMDType();
    var_types eltType = sig.retLayout->GetElementType();
    unsigned  size    = sig.retLayout->GetSize();

    NamedIntrinsic intrinsic;

    if (type == TYP_SIMD16)
    {
        intrinsic = (eltType == TYP_FLOAT) ? NI_SSE_AndNot : NI_SSE2_AndNot;
    }
    else
    {
        intrinsic = varTypeIsFloating(eltType) ? NI_AVX_AndNot : NI_AVX2_AndNot;
    }

    return NewVecNode(type, intrinsic, eltType, size, op2, op1);
}

constexpr ssize_t SHUFFLE_XXZX = 0x08; // 00 00 10 00
constexpr ssize_t SHUFFLE_ZWXY = 0xB1; // 10 11 00 01
constexpr ssize_t SHUFFLE_WWYY = 0xF5; // 11 11 01 01
constexpr ssize_t SHUFFLE_ZZXX = 0xA0; // 10 10 00 00

GenTree* Importer::impVectorT128ConvertUInt32ToSingle(const HWIntrinsicSignature& sig, GenTree* op1)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramLayout[0]->GetElementType() == TYP_UINT);
    assert(sig.retType == TYP_SIMD16);
    assert(sig.retLayout->GetElementType() == TYP_FLOAT);

    GenTree* uses[2];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* c;
    uses[0] = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical, TYP_INT, uses[0], comp->gtNewIconNode(16));
    c       = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, TYP_INT, comp->gtNewIconNode(0x53000000));
    uses[0] = NewVecNode(TYP_SIMD16, NI_SSE_Or, TYP_FLOAT, uses[0], c);
    c       = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, TYP_INT, comp->gtNewIconNode(0x53000000));
    uses[0] = NewVecNode(TYP_SIMD16, NI_SSE_Subtract, TYP_FLOAT, uses[0], c);

    uses[1] = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical, TYP_INT, uses[1], comp->gtNewIconNode(16));
    uses[1] = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical, TYP_INT, uses[1], comp->gtNewIconNode(16));
    uses[1] = NewVecNode(TYP_SIMD16, NI_SSE2_ConvertToVector128Single, TYP_INT, uses[1]);

    return NewVecNode(TYP_SIMD16, NI_SSE_Add, TYP_FLOAT, uses[0], uses[1]);
}

GenTree* Importer::impVectorT256ConvertUInt32ToSingle(const HWIntrinsicSignature& sig, GenTree* op1)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert(sig.paramLayout[0]->GetElementType() == TYP_UINT);
    assert(sig.retType == TYP_SIMD32);
    assert(sig.retLayout->GetElementType() == TYP_FLOAT);

    GenTree* uses[2];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* c;
    uses[0] = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, TYP_INT, uses[0], comp->gtNewIconNode(16));
    c       = NewVecNode(TYP_SIMD32, NI_VEC_SPLAT, TYP_INT, comp->gtNewIconNode(0x53000000));
    uses[0] = NewVecNode(TYP_SIMD32, NI_AVX_Or, TYP_FLOAT, uses[0], c);
    c       = NewVecNode(TYP_SIMD32, NI_VEC_SPLAT, TYP_INT, comp->gtNewIconNode(0x53000000));
    uses[0] = NewVecNode(TYP_SIMD32, NI_AVX_Subtract, TYP_FLOAT, uses[0], c);

    uses[1] = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftLeftLogical, TYP_INT, uses[1], comp->gtNewIconNode(16));
    uses[1] = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, TYP_INT, uses[1], comp->gtNewIconNode(16));
    uses[1] = NewVecNode(TYP_SIMD32, NI_AVX_ConvertToVector256Single, TYP_FLOAT, uses[1]);

    return NewVecNode(TYP_SIMD32, NI_AVX_Add, TYP_FLOAT, uses[0], uses[1]);
}

GenTree* Importer::impVectorT128ConvertInt64ToDouble(const HWIntrinsicSignature& sig, GenTree* op1)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramLayout[0]->GetElementType() == TYP_LONG);
    assert(sig.retType == TYP_SIMD16);
    assert(sig.retLayout->GetElementType() == TYP_DOUBLE);

#ifndef TARGET_64BIT
    GenTree* uses[3];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* sign = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical, TYP_LONG, uses[0], comp->gtNewIconNode(63));
    sign          = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical, TYP_LONG, sign, comp->gtNewIconNode(63));

    GenTree* uns = NewVecNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, uses[1], comp->gtNewIconNode(0xF5));
    uns          = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightArithmetic, TYP_INT, uns, comp->gtNewIconNode(32));
    GenTree* unsUses[2];
    impMakeMultiUse(uns, unsUses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));
    uns = NewVecNode(TYP_SIMD16, NI_SSE2_Xor, TYP_LONG, uses[2], unsUses[0]);
    uns = NewVecNode(TYP_SIMD16, NI_SSE2_Subtract, TYP_LONG, uns, unsUses[1]);

    GenTree* e[2];
    GenTree* c[2];

    impMakeMultiUse(uns, 2, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    e[0] = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical, TYP_LONG, uses[0], comp->gtNewIconNode(32));
    c[0] = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4530000000000000));
    e[0] = NewVecNode(TYP_SIMD16, NI_SSE2_Or, TYP_DOUBLE, e[0], c[0]);
    c[0] = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4530000000000000));
    e[0] = NewVecNode(TYP_SIMD16, NI_SSE2_Subtract, TYP_DOUBLE, e[0], c[0]);

    e[1] = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical, TYP_LONG, uses[1], comp->gtNewIconNode(32));
    e[1] = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical, TYP_LONG, e[1], comp->gtNewIconNode(32));
    c[1] = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4330000000000000));
    e[1] = NewVecNode(TYP_SIMD16, NI_SSE2_Or, TYP_DOUBLE, e[1], c[1]);
    c[1] = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4330000000000000));
    e[1] = NewVecNode(TYP_SIMD16, NI_SSE2_Subtract, TYP_DOUBLE, e[1], c[1]);

    uns = NewVecNode(TYP_SIMD16, NI_SSE2_Add, TYP_DOUBLE, e[0], e[1]);

    return NewVecNode(TYP_SIMD16, NI_SSE_Or, TYP_FLOAT, uns, sign);
#else
    GenTree* uses[4];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* e[2];

    e[0] = NewVecNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64, TYP_LONG, 16, uses[0]);
    e[0] = NewVecNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Double, TYP_LONG, uses[1], e[0]);

    if (comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        e[1] = NewVecNode(TYP_LONG, NI_SSE41_X64_Extract, TYP_LONG, 16, uses[2], comp->gtNewIconNode(1));
    }
    else
    {
        e[1] = NewVecNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, uses[2], comp->gtNewIconNode(0b00001110));
        e[1] = NewVecNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64, TYP_LONG, 16, e[1]);
    }

    e[1] = NewVecNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Double, TYP_LONG, uses[3], e[1]);
    return NewVecNode(TYP_SIMD16, NI_SSE2_UnpackLow, TYP_DOUBLE, e[0], e[1]);
#endif
}

GenTree* Importer::impVectorT256ConvertInt64ToDouble(const HWIntrinsicSignature& sig, GenTree* op1)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert(sig.paramLayout[0]->GetElementType() == TYP_LONG);
    assert(sig.retType == TYP_SIMD32);
    assert(sig.retLayout->GetElementType() == TYP_DOUBLE);

#ifndef TARGET_64BIT
    GenTree* uses[3];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* sign = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, TYP_LONG, uses[0], comp->gtNewIconNode(63));
    sign          = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftLeftLogical, TYP_LONG, sign, comp->gtNewIconNode(63));

    GenTree* uns = NewVecNode(TYP_SIMD32, NI_AVX2_Shuffle, TYP_INT, uses[1], comp->gtNewIconNode(0xF5));
    uns          = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftRightArithmetic, TYP_INT, uns, comp->gtNewIconNode(32));
    GenTree* unsUses[2];
    impMakeMultiUse(uns, unsUses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));
    uns = NewVecNode(TYP_SIMD32, NI_AVX2_Xor, TYP_LONG, uses[2], unsUses[0]);
    uns = NewVecNode(TYP_SIMD32, NI_AVX2_Subtract, TYP_LONG, uns, unsUses[1]);

    GenTree* e[2];
    GenTree* c[2];

    impMakeMultiUse(uns, 2, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    e[0] = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, TYP_LONG, uses[0], comp->gtNewIconNode(32));
    c[0] = NewVecNode(TYP_SIMD32, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4530000000000000));
    e[0] = NewVecNode(TYP_SIMD32, NI_AVX_Or, TYP_DOUBLE, e[0], c[0]);
    c[0] = NewVecNode(TYP_SIMD32, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4530000000000000));
    e[0] = NewVecNode(TYP_SIMD32, NI_AVX_Subtract, TYP_DOUBLE, e[0], c[0]);

    e[1] = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftLeftLogical, TYP_LONG, uses[1], comp->gtNewIconNode(32));
    e[1] = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, TYP_LONG, e[1], comp->gtNewIconNode(32));
    c[1] = NewVecNode(TYP_SIMD32, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4330000000000000));
    e[1] = NewVecNode(TYP_SIMD32, NI_AVX_Or, TYP_DOUBLE, e[1], c[1]);
    c[1] = NewVecNode(TYP_SIMD32, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4330000000000000));
    e[1] = NewVecNode(TYP_SIMD32, NI_AVX_Subtract, TYP_DOUBLE, e[1], c[1]);

    uns = NewVecNode(TYP_SIMD32, NI_AVX_Add, TYP_DOUBLE, e[0], e[1]);

    return NewVecNode(TYP_SIMD32, NI_AVX_Or, TYP_FLOAT, uns, sign);
#else
    // TODO-MIKE-Cleanup: These temps should be SIMD16 but we don't have a SIMD16 layout
    // handy so they're SIMD32 instead. This would mean that all the uses should really
    // be NI_VEC_TRUNC but that's kind of overkill, ignore it for now.
    GenTree* uses[5];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* e[4];
    e[0] = NewVecNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64, TYP_LONG, 16, uses[0]);
    e[0] = NewVecNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Double, TYP_LONG, uses[1], e[0]);
    e[1] = NewVecNode(TYP_LONG, NI_SSE41_X64_Extract, TYP_LONG, 16, uses[2], comp->gtNewIconNode(1));
    e[1] = NewVecNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Double, TYP_LONG, uses[3], e[1]);
    e[0] = NewVecNode(TYP_SIMD16, NI_SSE2_UnpackLow, TYP_DOUBLE, e[0], e[1]);

    uses[0] = NewVecNode(TYP_SIMD16, NI_AVX_ExtractVector128, TYP_FLOAT, 32, uses[4], comp->gtNewIconNode(1));
    uses[0] = NewVecNode(TYP_SIMD32, NI_Vector128_ToVector256Unsafe, TYP_LONG, 16, uses[0]);
    impMakeMultiUse(uses[0], 4, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    e[2] = NewVecNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64, TYP_LONG, 16, uses[0]);
    e[2] = NewVecNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Double, TYP_LONG, uses[1], e[2]);
    e[3] = NewVecNode(TYP_LONG, NI_SSE41_X64_Extract, TYP_LONG, 16, uses[2], comp->gtNewIconNode(1));
    e[3] = NewVecNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Double, TYP_LONG, uses[3], e[3]);
    e[2] = NewVecNode(TYP_SIMD16, NI_SSE2_UnpackLow, TYP_DOUBLE, e[2], e[3]);

    return NewVecNode(TYP_SIMD32, NI_AVX_InsertVector128, TYP_DOUBLE, e[0], e[2], comp->gtNewIconNode(1));
#endif
}

GenTree* Importer::impVectorT128ConvertUInt64ToDouble(const HWIntrinsicSignature& sig, GenTree* op1)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramLayout[0]->GetElementType() == TYP_ULONG);
    assert(sig.retType == TYP_SIMD16);
    assert(sig.retLayout->GetElementType() == TYP_DOUBLE);

    GenTree* uses[2];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* e[2];
    GenTree* c[2];

    e[0] = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical, TYP_LONG, uses[0], comp->gtNewIconNode(32));
    c[0] = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4530000000000000));
    e[0] = NewVecNode(TYP_SIMD16, NI_SSE2_Or, TYP_DOUBLE, e[0], c[0]);
    c[0] = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4530000000000000));
    e[0] = NewVecNode(TYP_SIMD16, NI_SSE2_Subtract, TYP_DOUBLE, e[0], c[0]);

    e[1] = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical, TYP_LONG, uses[1], comp->gtNewIconNode(32));
    e[1] = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical, TYP_LONG, e[1], comp->gtNewIconNode(32));
    c[1] = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4330000000000000));
    e[1] = NewVecNode(TYP_SIMD16, NI_SSE2_Or, TYP_DOUBLE, e[1], c[1]);
    c[1] = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4330000000000000));
    e[1] = NewVecNode(TYP_SIMD16, NI_SSE2_Subtract, TYP_DOUBLE, e[1], c[1]);

    return NewVecNode(TYP_SIMD16, NI_SSE2_Add, TYP_DOUBLE, e[0], e[1]);
}

GenTree* Importer::impVectorT256ConvertUInt64ToDouble(const HWIntrinsicSignature& sig, GenTree* op1)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert(sig.paramLayout[0]->GetElementType() == TYP_ULONG);
    assert(sig.retType == TYP_SIMD32);
    assert(sig.retLayout->GetElementType() == TYP_DOUBLE);

    GenTree* uses[2];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* e[2];
    GenTree* c[2];

    e[0] = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, TYP_LONG, uses[0], comp->gtNewIconNode(32));
    c[0] = NewVecNode(TYP_SIMD32, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4530000000000000));
    e[0] = NewVecNode(TYP_SIMD32, NI_AVX_Or, TYP_DOUBLE, e[0], c[0]);
    c[0] = NewVecNode(TYP_SIMD32, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4530000000000000));
    e[0] = NewVecNode(TYP_SIMD32, NI_AVX_Subtract, TYP_DOUBLE, e[0], c[0]);

    e[1] = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftLeftLogical, TYP_LONG, uses[1], comp->gtNewIconNode(32));
    e[1] = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, TYP_LONG, e[1], comp->gtNewIconNode(32));
    c[1] = NewVecNode(TYP_SIMD32, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4330000000000000));
    e[1] = NewVecNode(TYP_SIMD32, NI_AVX_Or, TYP_DOUBLE, e[1], c[1]);
    c[1] = NewVecNode(TYP_SIMD32, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(0x4330000000000000));
    e[1] = NewVecNode(TYP_SIMD32, NI_AVX_Subtract, TYP_DOUBLE, e[1], c[1]);

    return NewVecNode(TYP_SIMD32, NI_AVX_Add, TYP_DOUBLE, e[0], e[1]);
}

GenTree* Importer::impVectorT128ConvertDoubleToInt64(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramLayout[0]->GetElementType() == TYP_DOUBLE);
    assert(sig.retType == TYP_SIMD16);
    assert(sig.retLayout->GetElementType() == TYP_LONG);

#ifndef TARGET_64BIT
    return nullptr;
#else
    GenTree* op1 = PopVec(TYP_SIMD16);

    GenTree* uses[4];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* e[2];
    e[0] = NewVecNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64WithTruncation, TYP_DOUBLE, 16, uses[2]);
    e[0] = NewVecNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Int64, TYP_LONG, e[0]);
    e[1] = NewVecNode(TYP_SIMD16, NI_SSE2_UnpackHigh, TYP_DOUBLE, uses[0], uses[1]);
    e[1] = NewVecNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64WithTruncation, TYP_DOUBLE, 16, e[1]);

    if (!comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        e[1] = NewVecNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Int64, TYP_LONG, e[1]);
        return NewVecNode(TYP_SIMD16, NI_SSE2_UnpackLow, TYP_LONG, e[0], e[1]);
    }

    return NewVecNode(TYP_SIMD16, NI_SSE41_X64_Insert, TYP_LONG, e[0], e[1], comp->gtNewIconNode(1));
#endif
}

GenTree* Importer::impVectorT256ConvertDoubleToInt64(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert(sig.paramLayout[0]->GetElementType() == TYP_DOUBLE);
    assert(sig.retType == TYP_SIMD32);
    assert(sig.retLayout->GetElementType() == TYP_LONG);

#ifndef TARGET_64BIT
    return nullptr;
#else
    GenTree* op1 = PopVec(TYP_SIMD32);

    // TODO-MIKE-Cleanup: These temps should be SIMD16 but we don't have a SIMD16 layout
    // handy so they're SIMD32 instead. This would mean that all the uses should really
    // be NI_VEC_TRUNC but that's kind of overkill, ignore it for now.
    GenTree* uses[4];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* e[4];
    e[0] = NewVecNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64WithTruncation, TYP_DOUBLE, 16, uses[2]);
    e[0] = NewVecNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Int64, TYP_LONG, e[0]);
    e[1] = NewVecNode(TYP_SIMD16, NI_SSE2_UnpackHigh, TYP_DOUBLE, uses[0], uses[1]);
    e[1] = NewVecNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64WithTruncation, TYP_DOUBLE, 16, e[1]);

    uses[3] = NewVecNode(TYP_SIMD16, NI_AVX_ExtractVector128, TYP_FLOAT, 32, uses[3], comp->gtNewIconNode(1));
    uses[3] = NewVecNode(TYP_SIMD32, NI_Vector128_ToVector256Unsafe, TYP_LONG, 16, uses[3]);
    impMakeMultiUse(uses[3], 3, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    e[2] = NewVecNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64WithTruncation, TYP_DOUBLE, 16, uses[2]);
    e[2] = NewVecNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Int64, TYP_LONG, e[2]);
    e[3] = NewVecNode(TYP_SIMD16, NI_SSE2_UnpackHigh, TYP_DOUBLE, uses[0], uses[1]);
    e[3] = NewVecNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64WithTruncation, TYP_DOUBLE, 16, e[3]);

    e[0] = NewVecNode(TYP_SIMD16, NI_SSE41_X64_Insert, TYP_LONG, e[0], e[1], comp->gtNewIconNode(1));
    e[0] = NewVecNode(TYP_SIMD32, NI_Vector128_ToVector256Unsafe, TYP_LONG, 16, e[0]);
    e[1] = NewVecNode(TYP_SIMD16, NI_SSE41_X64_Insert, TYP_LONG, e[2], e[3], comp->gtNewIconNode(1));

    return NewVecNode(TYP_SIMD32, NI_AVX2_InsertVector128, TYP_LONG, e[0], e[1], comp->gtNewIconNode(1));
#endif
}

GenTree* Importer::impVector234Dot(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.paramCount == 2);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.paramLayout[0]->GetElementType() == TYP_FLOAT);
    assert(sig.retType == TYP_FLOAT);

    ClassLayout* layout = sig.paramLayout[0];

    if (comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        uint8_t imm = 0b11110000;
        imm >>= 4 - layout->GetElementCount();
        imm &= 0b11110000;
        imm |= 0b00000001;

        op1 = NewVecNode(TYP_SIMD16, NI_SSE41_DotProduct, TYP_FLOAT, op1, op2, comp->gtNewIconNode(imm));
    }
    else
    {
        op1 = NewVecNode(TYP_SIMD16, NI_SSE_Multiply, TYP_FLOAT, op1, op2);
        op1 = NewVecNode(TYP_SIMD16, NI_VEC_SUM, TYP_FLOAT, layout->GetSize(), op1);
    }

    return NewVecExtractNode(TYP_FLOAT, op1, comp->gtNewIconNode(0));
}

GenTree* Importer::impVectorT128Sum(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD16);

    var_types eltType = varTypeToSigned(sig.paramLayout[0]->GetElementType());

    if ((eltType != TYP_FLOAT) && !comp->compOpportunisticallyDependsOn(InstructionSet_SSE2))
    {
        return nullptr;
    }

    GenTree* vec = PopVec(TYP_SIMD16);

    return impVectorT128Sum(vec, eltType, sig.retType);
}

GenTree* Importer::impVectorT128Sum(GenTree* vec, var_types eltType, var_types retType)
{
    if (eltType == TYP_BYTE)
    {
        vec = NewVecNode(TYP_SIMD16, NI_SSE2_SumAbsoluteDifferences, TYP_USHORT, vec,
                         NewVecZeroNode(TYP_SIMD16, TYP_BYTE));
        eltType = TYP_LONG;
    }

    vec = NewVecNode(TYP_SIMD16, NI_VEC_SUM, eltType, vec);
    return NewVecExtractNode(retType, vec, comp->gtNewIconNode(0));
}

GenTree* Importer::impVectorT256Sum(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD32);

    var_types eltType = varTypeToSigned(sig.paramLayout[0]->GetElementType());
    GenTree*  vec     = PopVec(TYP_SIMD32);

    vec = NewVecNode(TYP_SIMD16, NI_VEC_SUM, eltType, 32, vec);
    return impVectorT128Sum(vec, eltType, sig.retType);
}

GenTree* Importer::impVectorT128Dot(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 2);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);

    var_types eltType = varTypeNodeType(sig.paramLayout[0]->GetElementType());

    bool hasSse41 = comp->compOpportunisticallyDependsOn(InstructionSet_SSE41);

    if ((eltType == TYP_INT || eltType == TYP_LONG) && !hasSse41)
    {
        return nullptr;
    }

    GenTree* op1 = PopVec(TYP_SIMD16);
    GenTree* op2 = PopVec(TYP_SIMD16);

    if (varTypeIsFloating(eltType) && hasSse41)
    {
        uint8_t imm = eltType == TYP_FLOAT ? 0b11110001 : 0b00110001;

        op1 = NewVecNode(TYP_SIMD16, NI_SSE41_DotProduct, eltType, op1, op2, comp->gtNewIconNode(imm));
        return NewVecExtractNode(eltType, op1, comp->gtNewIconNode(0));
    }

    switch (eltType)
    {
        case TYP_FLOAT:
            op1 = NewVecNode(TYP_SIMD16, NI_SSE_Multiply, TYP_FLOAT, op1, op2);
            break;
        case TYP_DOUBLE:
            op1 = NewVecNode(TYP_SIMD16, NI_SSE2_Multiply, TYP_DOUBLE, op1, op2);
            break;
        case TYP_LONG:
            op1     = impVectorTMultiplyLong(sig.paramLayout[0], op1, op2);
            eltType = TYP_LONG;
            break;
        case TYP_INT:
            op1     = NewVecNode(TYP_SIMD16, NI_SSE41_MultiplyLow, TYP_INT, op1, op2);
            eltType = TYP_INT;
            break;
        case TYP_SHORT:
        case TYP_USHORT:
            op1     = NewVecNode(TYP_SIMD16, NI_SSE2_MultiplyAddAdjacent, TYP_INT, op1, op2);
            eltType = TYP_INT;
            break;
        default:
            assert(varTypeIsByte(eltType));
            op1     = impVectorTMultiplyAddAdjacentByte(sig, op1, op2);
            eltType = TYP_INT;
            break;
    }

    op1 = NewVecNode(TYP_SIMD16, NI_VEC_SUM, eltType, op1);
    return NewVecExtractNode(sig.retType, op1, comp->gtNewIconNode(0));
}

GenTree* Importer::impVectorT256Dot(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 2);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);

    var_types eltType = varTypeNodeType(sig.paramLayout[0]->GetElementType());

    GenTree* op1 = PopVec(TYP_SIMD32);
    GenTree* op2 = PopVec(TYP_SIMD32);

    if (eltType == TYP_FLOAT)
    {
        op1 = NewVecNode(TYP_SIMD32, NI_AVX_DotProduct, TYP_FLOAT, op1, op2, comp->gtNewIconNode(0b11110001));
        op1 = NewVecNode(TYP_SIMD16, NI_VEC_SUM, TYP_FLOAT, 32, op1);
        return NewVecExtractNode(TYP_FLOAT, op1, comp->gtNewIconNode(0));
    }

    switch (eltType)
    {
        case TYP_DOUBLE:
            op1 = NewVecNode(TYP_SIMD32, NI_AVX_Multiply, TYP_DOUBLE, op1, op2);
            break;
        case TYP_LONG:
            op1     = impVectorTMultiplyLong(sig.paramLayout[0], op1, op2);
            eltType = TYP_LONG;
            break;
        case TYP_INT:
            op1     = NewVecNode(TYP_SIMD32, NI_AVX2_MultiplyLow, TYP_INT, op1, op2);
            eltType = TYP_INT;
            break;
        case TYP_SHORT:
        case TYP_USHORT:
            op1     = NewVecNode(TYP_SIMD32, NI_AVX2_MultiplyAddAdjacent, TYP_INT, op1, op2);
            eltType = TYP_INT;
            break;
        default:
            assert(varTypeIsByte(eltType));
            op1     = impVectorTMultiplyAddAdjacentByte(sig, op1, op2);
            eltType = TYP_INT;
            break;
    }

    op1 = NewVecNode(TYP_SIMD16, NI_VEC_SUM, eltType, 32, op1);
    op1 = NewVecNode(TYP_SIMD16, NI_VEC_SUM, eltType, op1);
    return NewVecExtractNode(sig.retType, op1, comp->gtNewIconNode(0));
}

GenTree* Importer::impVectorTMultiplyAddAdjacentByte(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(varTypeIsByte(sig.paramLayout[0]->GetElementType()));

    GenTree* op1Uses[2];
    impMakeMultiUse(op1, op1Uses, sig.paramLayout[0],
                    CHECK_SPILL_ALL DEBUGARG("Vector<byte>.MultiplyAddAdjacent temp"));
    GenTree* op2Uses[2];
    impMakeMultiUse(op2, op2Uses, sig.paramLayout[0],
                    CHECK_SPILL_ALL DEBUGARG("Vector<byte>.MultiplyAddAdjacent temp"));

    var_types type = sig.paramType[0];
    unsigned  size = varTypeSize(type);

    NamedIntrinsic madd = size == 32 ? NI_AVX2_MultiplyAddAdjacent : NI_SSE2_MultiplyAddAdjacent;
    NamedIntrinsic srlw = size == 32 ? NI_AVX2_ShiftRightLogical : NI_SSE2_ShiftRightLogical;
    NamedIntrinsic add  = size == 32 ? NI_AVX2_Add : NI_SSE2_Add;

    GenTree* lo  = NewVecNode(type, madd, TYP_INT, size, op1Uses[0], op2Uses[0]);
    GenTree* hi1 = NewVecNode(type, srlw, TYP_SHORT, size, op1Uses[1], comp->gtNewIconNode(8));
    GenTree* hi2 = NewVecNode(type, srlw, TYP_SHORT, size, op2Uses[1], comp->gtNewIconNode(8));
    GenTree* hi  = NewVecNode(type, madd, TYP_INT, size, hi1, hi2);
    return NewVecNode(type, add, TYP_INT, size, lo, hi);
}

GenTree* Importer::impVector234TEquals(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool notEqual)
{
    assert((sig.hasThisParam && (sig.paramCount == 1)) || (sig.paramCount == 2));
    assert(sig.hasThisParam || (sig.paramLayout[0] == sig.paramLayout[1]));
    assert(sig.retType == TYP_BOOL);

    ClassLayout* layout  = sig.paramLayout[0];
    var_types    type    = layout->GetVectorType();
    var_types    vecType = varTypeTargetVec(type);
    var_types    eltType = layout->GetElementType();
    unsigned     size    = layout->GetSize();

    // Import integral vector equality as NI_VEC_EQ/NE if we have PTEST.
    // It's too early to use PTEST here because op2 may not be a constant zero vector yet
    // and it's rather cumbersome to import to CompareEqual/MoveMask and pattern match in
    // lowering to change to PTEST.
    if (varTypeIsIntegral(eltType) && comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        return NewVecNode(TYP_BOOL, notEqual ? NI_VEC_NE : NI_VEC_EQ, eltType, size, op1, op2);
    }

    NamedIntrinsic cmpeq;
    NamedIntrinsic movmsk;
    int32_t        mask;

    if (type == TYP_SIMD32)
    {
        assert(varTypeIsFloating(eltType));
        cmpeq  = NI_AVX_CompareEqual;
        movmsk = NI_AVX_MoveMask;
        mask   = eltType == TYP_FLOAT ? 0xFF : 0x0F;
    }
    else if (eltType == TYP_FLOAT)
    {
        cmpeq  = NI_SSE_CompareEqual;
        movmsk = NI_SSE_MoveMask;
        mask   = 0b1111 >> (4 - layout->GetElementCount());
        size   = max(16, size);
    }
    else if (eltType == TYP_DOUBLE)
    {
        cmpeq  = NI_SSE2_CompareEqual;
        movmsk = NI_SSE2_MoveMask;
        mask   = 0b0011;
    }
    else
    {
        cmpeq   = NI_SSE2_CompareEqual;
        movmsk  = NI_SSE2_MoveMask;
        mask    = 0xFFFF;
        eltType = TYP_UBYTE;
    }

    op1 = NewVecNode(vecType, cmpeq, eltType, size, op1, op2);
    op1 = NewVecNode(TYP_INT, movmsk, eltType, size, op1);

    if ((type == TYP_SIMD8) || (type == TYP_SIMD12))
    {
        op1 = comp->gtNewOperNode(GT_AND, TYP_INT, op1, comp->gtNewIconNode(mask));
    }

    return comp->gtNewOperNode(notEqual ? GT_NE : GT_EQ, TYP_INT, op1, comp->gtNewIconNode(mask));
}

GenTree* Importer::impVectorT128MinMax(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool isMax)
{
    assert(sig.paramCount == 2);
    assert(sig.retType == TYP_SIMD16);
    assert((sig.retLayout == sig.paramLayout[0]) && (sig.retLayout == sig.paramLayout[1]));

    ClassLayout* layout  = sig.paramLayout[0];
    var_types    eltType = layout->GetElementType();

    if (((eltType == TYP_BYTE) || (eltType == TYP_USHORT) || (eltType == TYP_INT) || (eltType == TYP_UINT)) &&
        comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        return NewVecNode(TYP_SIMD16, isMax ? NI_SSE41_Max : NI_SSE41_Min, eltType, op1, op2);
    }

    if ((eltType == TYP_BYTE) || (eltType == TYP_USHORT))
    {
        GenTree*       constVal;
        NamedIntrinsic preIntrinsic;
        NamedIntrinsic intrinsic = isMax ? NI_SSE2_Max : NI_SSE2_Min;
        NamedIntrinsic postIntrinsic;

        if (eltType == TYP_BYTE)
        {
            constVal      = comp->gtNewIconNode(0x80808080);
            preIntrinsic  = NI_SSE2_Subtract;
            postIntrinsic = NI_SSE2_Add;
            eltType       = TYP_UBYTE;
        }
        else
        {
            constVal      = comp->gtNewIconNode(0x80008000);
            preIntrinsic  = NI_SSE2_Add;
            postIntrinsic = NI_SSE2_Subtract;
            eltType       = TYP_SHORT;
        }

        GenTree* constVector = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, TYP_INT, constVal);
        GenTree* constUses[3];
        impMakeMultiUse(constVector, constUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.MinMax const temp"));

        op1 = NewVecNode(TYP_SIMD16, preIntrinsic, eltType, op1, constUses[0]);
        op2 = NewVecNode(TYP_SIMD16, preIntrinsic, eltType, op2, constUses[1]);
        op1 = NewVecNode(TYP_SIMD16, intrinsic, eltType, op1, op2);
        return NewVecNode(TYP_SIMD16, postIntrinsic, eltType, op1, constUses[2]);
    }

    assert(varTypeIsInt(eltType) || varTypeIsLong(eltType));

    GenTree* uses[2][2];
    impMakeMultiUse(op1, uses[0], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.MinMax temp"));
    impMakeMultiUse(op2, uses[1], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.MinMax temp"));

    GenTree* mask;

    if (!varTypeIsLong(eltType) || comp->compOpportunisticallyDependsOn(InstructionSet_SSE42))
    {
        NamedIntrinsic ni = !varTypeIsLong(eltType) ? NI_SSE2_CompareGreaterThan : NI_SSE42_CompareGreaterThan;

        if (varTypeIsUnsigned(eltType))
        {
            eltType = impVectorTUnsignedCompareAdjust(layout, eltType, &uses[0][0], &uses[1][0]);
        }

        mask = NewVecNode(TYP_SIMD16, ni, eltType, uses[0][0], uses[1][0]);
    }
    else if (eltType == TYP_LONG)
    {
        mask = impVectorT128LongGreaterThanSse2(layout, uses[0][0], uses[1][0]);
    }
    else
    {
        mask = impVectorT128ULongGreaterThanSse2(layout, uses[0][0], uses[1][0]);
    }

    if (isMax)
    {
        std::swap(uses[0][1], uses[1][1]);
    }

    if (!comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        GenTree* maskUses[2];
        impMakeMultiUse(mask, maskUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.MinMax mask temp"));
        op1 = NewVecNode(TYP_SIMD16, NI_SSE2_And, eltType, uses[1][1], maskUses[0]);
        op2 = NewVecNode(TYP_SIMD16, NI_SSE2_AndNot, eltType, maskUses[1], uses[0][1]);
        return NewVecNode(TYP_SIMD16, NI_SSE2_Or, eltType, op1, op2);
    }

    return NewVecNode(TYP_SIMD16, NI_SSE41_BlendVariable, TYP_UBYTE, uses[0][1], uses[1][1], mask);
}

GenTree* Importer::impVectorT256MinMax(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool isMax)
{
    assert(sig.paramCount == 2);
    assert(sig.retType == TYP_SIMD32);
    assert((sig.retLayout == sig.paramLayout[0]) && (sig.retLayout == sig.paramLayout[1]));

    ClassLayout* layout  = sig.paramLayout[0];
    var_types    eltType = layout->GetElementType();

    assert(varTypeIsLong(eltType));

    GenTree* uses[2][2];
    impMakeMultiUse(op1, uses[0], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.MinMax temp"));
    impMakeMultiUse(op2, uses[1], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.MinMax temp"));

    if (eltType == TYP_ULONG)
    {
        impVectorTUnsignedCompareAdjust(layout, eltType, &uses[0][0], &uses[1][0]);
    }

    GenTree* mask = NewVecNode(TYP_SIMD32, NI_AVX2_CompareGreaterThan, TYP_LONG, uses[0][0], uses[1][0]);

    if (isMax)
    {
        std::swap(uses[0][1], uses[1][1]);
    }

    return NewVecNode(TYP_SIMD32, NI_AVX2_BlendVariable, TYP_UBYTE, uses[0][1], uses[1][1], mask);
}

GenTree* Importer::impVectorT128Narrow(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.retType == TYP_SIMD16);
    assert(sig.paramCount == 2);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(varTypeSize(sig.retLayout->GetElementType()) == varTypeSize(sig.paramLayout[0]->GetElementType()) / 2);

    var_types eltType = varTypeToSigned(sig.paramLayout[0]->GetElementType());

    if (eltType == TYP_DOUBLE)
    {
        op1 = NewVecNode(TYP_SIMD16, NI_SSE2_ConvertToVector128Single, TYP_DOUBLE, op1);
        op2 = NewVecNode(TYP_SIMD16, NI_SSE2_ConvertToVector128Single, TYP_DOUBLE, op2);

        return NewVecNode(TYP_SIMD16, NI_SSE_MoveLowToHigh, TYP_FLOAT, op1, op2);
    }

    if (eltType == TYP_LONG)
    {
        op1 = NewVecNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, op1, comp->gtNewIconNode(128));
        op1 = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical128BitLane, TYP_LONG, op1, comp->gtNewIconNode(8));
        op2 = NewVecNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, op2, comp->gtNewIconNode(8));
        op2 = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical128BitLane, TYP_LONG, op2, comp->gtNewIconNode(8));

        return NewVecNode(TYP_SIMD16, NI_SSE2_Or, TYP_INT, op1, op2);
    }

    assert((eltType == TYP_SHORT) || (eltType == TYP_INT));

    var_types retEltType = varTypeToSigned(sig.retLayout->GetElementType());
    ssize_t   retEltSize = varTypeSize(retEltType) * 8;

    op1 = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical, eltType, op1, comp->gtNewIconNode(retEltSize));
    op1 = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightArithmetic, eltType, op1, comp->gtNewIconNode(retEltSize));
    op2 = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical, eltType, op2, comp->gtNewIconNode(retEltSize));
    op2 = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightArithmetic, eltType, op2, comp->gtNewIconNode(retEltSize));

    return NewVecNode(TYP_SIMD16, NI_SSE2_PackSignedSaturate, retEltType, op1, op2);
}

GenTree* Importer::impVectorT256Narrow(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.retType == TYP_SIMD32);
    assert(sig.paramCount == 2);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert(varTypeSize(sig.retLayout->GetElementType()) == varTypeSize(sig.paramLayout[0]->GetElementType()) / 2);

    var_types eltType = varTypeToSigned(sig.paramLayout[0]->GetElementType());

    if (eltType == TYP_DOUBLE)
    {
        op1 = NewVecNode(TYP_SIMD16, NI_AVX_ConvertToVector128Single, TYP_FLOAT, 32, op1);
        op2 = NewVecNode(TYP_SIMD16, NI_AVX_ConvertToVector128Single, TYP_FLOAT, 32, op2);

        return NewVecNode(TYP_SIMD32, NI_AVX_InsertVector128, TYP_FLOAT, op1, op2, comp->gtNewIconNode(1));
    }

    GenTree* uses[2][2];
    impMakeMultiUse(op1, uses[0], sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Narrow temp"));
    impMakeMultiUse(op2, uses[1], sig.paramLayout[1], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Narrow temp"));

    op1 = NewVecNode(TYP_SIMD32, NI_AVX2_Permute2x128, eltType, uses[0][0], uses[1][0], comp->gtNewIconNode(32));
    op2 = NewVecNode(TYP_SIMD32, NI_AVX2_Permute2x128, eltType, uses[0][1], uses[1][1], comp->gtNewIconNode(49));

    if (eltType == TYP_LONG)
    {
        op1 = NewVecNode(TYP_SIMD32, NI_AVX2_Shuffle, TYP_INT, op1, comp->gtNewIconNode(8));
        op2 = NewVecNode(TYP_SIMD32, NI_AVX2_Shuffle, TYP_INT, op2, comp->gtNewIconNode(8));

        return NewVecNode(TYP_SIMD32, NI_AVX2_UnpackLow, TYP_LONG, 32, op1, op2);
    }

    assert((eltType == TYP_SHORT) || (eltType == TYP_INT));

    var_types retEltType = varTypeToSmallUnsigned(sig.retLayout->GetElementType());
    ssize_t   retEltSize = varTypeSize(retEltType) * 8;

    op1 = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftLeftLogical, eltType, op1, comp->gtNewIconNode(retEltSize));
    op1 = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, eltType, op1, comp->gtNewIconNode(retEltSize));
    op2 = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftLeftLogical, eltType, op2, comp->gtNewIconNode(retEltSize));
    op2 = NewVecNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, eltType, op2, comp->gtNewIconNode(retEltSize));

    return NewVecNode(TYP_SIMD32, NI_AVX2_PackUnsignedSaturate, retEltType, op1, op2);
}

GenTree* Importer::impVectorT128Widen(const HWIntrinsicSignature& sig)
{
    assert(sig.retType == TYP_VOID);
    assert(sig.paramCount == 3);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramType[1] == TYP_BYREF);
    assert(sig.paramType[2] == TYP_BYREF);

    var_types eltType = sig.paramLayout[0]->GetElementType();
    GenTree*  hiAddr  = impPopStack().val;
    GenTree*  loAddr  = impPopStack().val;
    GenTree*  value   = PopVec(sig.paramType[0]);
    GenTree*  hi;
    GenTree*  lo;

    if (eltType == TYP_FLOAT)
    {
        GenTree* uses[3];
        impMakeMultiUse(value, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));

        lo = NewVecNode(TYP_SIMD16, NI_SSE2_ConvertToVector128Double, TYP_FLOAT, uses[0]);
        hi = NewVecNode(TYP_SIMD16, NI_SSE_MoveHighToLow, TYP_FLOAT, uses[1], uses[2]);
        hi = NewVecNode(TYP_SIMD16, NI_SSE2_ConvertToVector128Double, TYP_FLOAT, hi);
    }
    else if (comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        NamedIntrinsic intrinsic;

        switch (eltType)
        {
            case TYP_BYTE:
            case TYP_UBYTE:
                intrinsic = NI_SSE41_ConvertToVector128Int16;
                break;
            case TYP_SHORT:
            case TYP_USHORT:
                intrinsic = NI_SSE41_ConvertToVector128Int32;
                break;
            default:
                assert((eltType == TYP_INT) || (eltType == TYP_UINT));
                intrinsic = NI_SSE41_ConvertToVector128Int64;
                break;
        }

        GenTree* uses[2];
        impMakeMultiUse(value, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));
        // TODO-MIKE-CQ: NI_SSE2_UnpackHigh would be better but it tends to generate an extra movaps.
        uses[1] = NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical128BitLane, eltType, uses[1], comp->gtNewIconNode(8));

        lo = NewVecNode(TYP_SIMD16, intrinsic, eltType, uses[0]);
        hi = NewVecNode(TYP_SIMD16, intrinsic, eltType, uses[1]);
    }
    else
    {
        GenTree* sign[2]{NewVecZeroNode(sig.paramLayout[0])};
        GenTree* uses[3];

        if (varTypeIsSigned(eltType))
        {
            impMakeMultiUse(value, 3, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));
            sign[0] = NewVecNode(TYP_SIMD16, NI_SSE2_CompareGreaterThan, eltType, sign[0], uses[2]);
            impMakeMultiUse(sign[0], sign, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));
        }
        else
        {
            impMakeMultiUse(value, 2, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));
            sign[1] = NewVecZeroNode(sig.paramLayout[0]);
        }

        lo = NewVecNode(TYP_SIMD16, NI_SSE2_UnpackLow, eltType, uses[0], sign[0]);
        hi = NewVecNode(TYP_SIMD16, NI_SSE2_UnpackHigh, eltType, uses[1], sign[1]);
    }

    impSpillAllAppendTree(impVectorStore(TYP_SIMD16, loAddr, lo));
    return impVectorStore(TYP_SIMD16, hiAddr, hi);
}

GenTree* Importer::impVectorT256Widen(const HWIntrinsicSignature& sig)
{
    assert(sig.retType == TYP_VOID);
    assert(sig.paramCount == 3);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert(sig.paramType[1] == TYP_BYREF);
    assert(sig.paramType[2] == TYP_BYREF);

    var_types eltType = sig.paramLayout[0]->GetElementType();
    GenTree*  hiAddr  = impPopStack().val;
    GenTree*  loAddr  = impPopStack().val;
    GenTree*  value   = PopVec(sig.paramType[0]);
    GenTree*  hi;
    GenTree*  lo;

    GenTree* uses[2];
    impMakeMultiUse(value, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));

    NamedIntrinsic widenIntrinsic;
    NamedIntrinsic extractIntrinsic;

    switch (eltType)
    {
        case TYP_BYTE:
        case TYP_UBYTE:
            widenIntrinsic   = NI_AVX2_ConvertToVector256Int16;
            extractIntrinsic = NI_AVX2_ExtractVector128;
            break;
        case TYP_SHORT:
        case TYP_USHORT:
            widenIntrinsic   = NI_AVX2_ConvertToVector256Int32;
            extractIntrinsic = NI_AVX2_ExtractVector128;
            break;
        case TYP_INT:
        case TYP_UINT:
            widenIntrinsic   = NI_AVX2_ConvertToVector256Int64;
            extractIntrinsic = NI_AVX2_ExtractVector128;
            break;
        default:
            assert(eltType == TYP_FLOAT);
            widenIntrinsic   = NI_AVX_ConvertToVector256Double;
            extractIntrinsic = NI_AVX_ExtractVector128;
            break;
    }

    lo = NewVecNode(TYP_SIMD32, widenIntrinsic, eltType, uses[0]);
    hi = NewVecNode(TYP_SIMD32, extractIntrinsic, eltType, uses[1], comp->gtNewIconNode(1));
    hi = NewVecNode(TYP_SIMD32, widenIntrinsic, eltType, hi);

    impSpillAllAppendTree(impVectorStore(TYP_SIMD32, loAddr, lo));
    return impVectorStore(TYP_SIMD32, hiAddr, hi);
}

GenTree* Importer::impVectorTMultiply(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 2);

    var_types vecType = sig.retType;
    var_types eltType = varTypeNodeType(sig.retLayout->GetElementType());

    assert((vecType == TYP_SIMD16) || (vecType == TYP_SIMD32));

    GenTree* op1;
    GenTree* op2;

    if (sig.paramLayout[0] == nullptr)
    {
        assert(sig.paramType[0] == sig.retLayout->GetElementType());
        assert(sig.paramLayout[1] == sig.retLayout);

        op2 = PopVec(sig.paramType[1]);
        op1 = impPopStack().val;

        op1 = NewVecNode(vecType, NI_VEC_SPLAT, eltType, op1);
    }
    else if (sig.paramLayout[1] == nullptr)
    {
        assert(sig.paramLayout[0] == sig.retLayout);
        assert(sig.paramType[1] == sig.retLayout->GetElementType());

        op2 = impPopStack().val;
        op1 = PopVec(sig.paramType[0]);

        op2 = NewVecNode(vecType, NI_VEC_SPLAT, eltType, op2);
    }
    else
    {
        assert((sig.retLayout == sig.paramLayout[0]) && (sig.retLayout == sig.paramLayout[1]));

        op2 = PopVec(sig.paramType[0]);
        op1 = PopVec(sig.paramType[0]);
    }

    if (varTypeIsByte(eltType))
    {
        return impVectorTMultiplyByte(sig.retLayout, op1, op2);
    }

    if (eltType == TYP_LONG)
    {
        return impVectorTMultiplyLong(sig.retLayout, op1, op2);
    }

    bool           isAVX = vecType == TYP_SIMD32;
    NamedIntrinsic intrinsic;

    switch (eltType)
    {
        case TYP_FLOAT:
            intrinsic = isAVX ? NI_AVX_Multiply : NI_SSE_Multiply;
            break;
        case TYP_DOUBLE:
            intrinsic = isAVX ? NI_AVX_Multiply : NI_SSE2_Multiply;
            break;
        case TYP_SHORT:
        case TYP_USHORT:
            intrinsic = isAVX ? NI_AVX2_MultiplyLow : NI_SSE2_MultiplyLow;
            break;
        default:
            assert(eltType == TYP_INT);
            intrinsic = isAVX ? NI_AVX2_MultiplyLow : NI_SSE41_MultiplyLow;
            break;
    }

    if ((intrinsic != NI_SSE41_MultiplyLow) || comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        return NewVecNode(vecType, intrinsic, eltType, op1, op2);
    }

    GenTree* uses[2][2];
    impMakeMultiUse(op1, uses[0], sig.retLayout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Multiply temp"));
    impMakeMultiUse(op2, uses[1], sig.retLayout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Multiply temp"));

    GenTree* t =
        NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical128BitLane, TYP_INT, uses[0][0], comp->gtNewIconNode(4));
    GenTree* u =
        NewVecNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical128BitLane, TYP_INT, uses[1][0], comp->gtNewIconNode(4));

    u = NewVecNode(TYP_SIMD16, NI_SSE2_Multiply, TYP_ULONG, u, t);
    u = NewVecNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, u, comp->gtNewIconNode(SHUFFLE_XXZX));

    t = NewVecNode(TYP_SIMD16, NI_SSE2_Multiply, TYP_ULONG, uses[0][1], uses[1][1]);
    t = NewVecNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, t, comp->gtNewIconNode(SHUFFLE_XXZX));

    return NewVecNode(TYP_SIMD16, NI_SSE2_UnpackLow, eltType, t, u);
}

GenTree* Importer::impVectorTMultiplyLong(ClassLayout* layout, GenTree* op1, GenTree* op2)
{
    assert(varTypeIsLong(layout->GetElementType()));

    GenTree* op1Uses[3];
    impMakeMultiUse(op1, op1Uses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<long>.Multiply temp"));
    GenTree* op2Uses[3];
    impMakeMultiUse(op2, op2Uses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<long>.Multiply temp"));

    var_types type = layout->GetVectorType();

    NamedIntrinsic mul = type == TYP_SIMD32 ? NI_AVX2_Multiply : NI_SSE2_Multiply;
    NamedIntrinsic add = type == TYP_SIMD32 ? NI_AVX2_Add : NI_SSE2_Add;
    NamedIntrinsic srl = type == TYP_SIMD32 ? NI_AVX2_ShiftRightLogical : NI_SSE2_ShiftRightLogical;
    NamedIntrinsic sll = type == TYP_SIMD32 ? NI_AVX2_ShiftLeftLogical : NI_SSE2_ShiftLeftLogical;

    GenTree* lo  = NewVecNode(type, mul, TYP_ULONG, op1Uses[0], op2Uses[0]);
    GenTree* hi1 = NewVecNode(type, srl, TYP_LONG, op1Uses[1], comp->gtNewIconNode(32));
    hi1          = NewVecNode(type, mul, TYP_ULONG, hi1, op2Uses[1]);
    GenTree* hi2 = NewVecNode(type, srl, TYP_LONG, op2Uses[2], comp->gtNewIconNode(32));
    hi2          = NewVecNode(type, mul, TYP_ULONG, hi2, op1Uses[2]);
    GenTree* hi  = NewVecNode(type, add, TYP_LONG, hi1, hi2);
    hi           = NewVecNode(type, sll, TYP_LONG, hi, comp->gtNewIconNode(32));
    return NewVecNode(type, add, TYP_LONG, lo, hi);
}

GenTree* Importer::impVectorTMultiplyByte(ClassLayout* layout, GenTree* op1, GenTree* op2)
{
    assert(varTypeIsByte(layout->GetElementType()));

    GenTree* op1Uses[2];
    impMakeMultiUse(op1, op1Uses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<byte>.Multiply temp"));
    GenTree* op2Uses[2];
    impMakeMultiUse(op2, op2Uses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<byte>.Multiply temp"));

    var_types type = layout->GetVectorType();

    NamedIntrinsic mul  = type == TYP_SIMD32 ? NI_AVX2_MultiplyLow : NI_SSE2_MultiplyLow;
    NamedIntrinsic srlw = type == TYP_SIMD32 ? NI_AVX2_ShiftRightLogical : NI_SSE2_ShiftRightLogical;
    NamedIntrinsic sllw = type == TYP_SIMD32 ? NI_AVX2_ShiftLeftLogical : NI_SSE2_ShiftLeftLogical;
    NamedIntrinsic pand = type == TYP_SIMD32 ? NI_AVX2_And : NI_SSE2_And;
    NamedIntrinsic por  = type == TYP_SIMD32 ? NI_AVX2_Or : NI_SSE2_Or;

    GenTree* lo  = NewVecNode(type, mul, TYP_SHORT, op1Uses[0], op2Uses[0]);
    GenTree* hi1 = NewVecNode(type, srlw, TYP_SHORT, op1Uses[1], comp->gtNewIconNode(8));
    GenTree* hi2 = NewVecNode(type, srlw, TYP_SHORT, op2Uses[1], comp->gtNewIconNode(8));
    GenTree* hi  = NewVecNode(type, mul, TYP_SHORT, hi1, hi2);
    hi           = NewVecNode(type, sllw, TYP_SHORT, hi, comp->gtNewIconNode(8));
    GenTree* m   = NewVecNode(type, NI_VEC_SPLAT, TYP_SHORT, comp->gtNewIconNode(0xff));
    lo           = NewVecNode(type, pand, TYP_SHORT, lo, m);
    return NewVecNode(type, por, TYP_INT, lo, hi);
}

GenTree* Importer::impVectorT128ConditionalSelect(const HWIntrinsicSignature& sig,
                                                  GenTree*                    mask,
                                                  GenTree*                    op1,
                                                  GenTree*                    op2)
{
    assert(sig.paramCount == 3);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert((sig.retLayout == sig.paramLayout[1]) && (sig.retLayout == sig.paramLayout[2]));
    assert(sig.retType == TYP_SIMD16);

    GenTree* maskUses[2];
    impMakeMultiUse(mask, maskUses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.ConditionalSelect temp"));

    var_types eltType = sig.retLayout->GetElementType();
    bool      sse     = eltType == TYP_FLOAT;

    op1 = NewVecNode(TYP_SIMD16, sse ? NI_SSE_And : NI_SSE2_And, eltType, op1, maskUses[0]);
    op2 = NewVecNode(TYP_SIMD16, sse ? NI_SSE_AndNot : NI_SSE2_AndNot, eltType, maskUses[1], op2);
    return NewVecNode(TYP_SIMD16, sse ? NI_SSE_Or : NI_SSE2_Or, eltType, op1, op2);
}

GenTree* Importer::impVectorT256ConditionalSelect(const HWIntrinsicSignature& sig,
                                                  GenTree*                    mask,
                                                  GenTree*                    op1,
                                                  GenTree*                    op2)
{
    assert(sig.paramCount == 3);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert((sig.retLayout == sig.paramLayout[1]) && (sig.retLayout == sig.paramLayout[2]));
    assert(sig.retType == TYP_SIMD32);

    GenTree* maskUses[2];
    impMakeMultiUse(mask, maskUses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.ConditionalSelect temp"));

    var_types eltType = sig.retLayout->GetElementType();
    bool      avx     = varTypeIsFloating(eltType);

    op1 = NewVecNode(TYP_SIMD32, avx ? NI_AVX_And : NI_AVX2_And, eltType, op1, maskUses[0]);
    op2 = NewVecNode(TYP_SIMD32, avx ? NI_AVX_AndNot : NI_AVX2_AndNot, eltType, maskUses[1], op2);
    return NewVecNode(TYP_SIMD32, avx ? NI_AVX_Or : NI_AVX2_Or, eltType, op1, op2);
}

var_types Importer::impVectorTUnsignedCompareAdjust(ClassLayout* layout,
                                                    var_types    eltType,
                                                    GenTree**    op1,
                                                    GenTree**    op2)
{
    GenTree* constVal = nullptr;

    switch (eltType)
    {
        case TYP_UBYTE:
            constVal = comp->gtNewIconNode(0x80808080);
            eltType  = TYP_BYTE;
            break;
        case TYP_USHORT:
            constVal = comp->gtNewIconNode(0x80008000);
            eltType  = TYP_SHORT;
            break;
        case TYP_UINT:
            constVal = comp->gtNewIconNode(0x80000000);
            eltType  = TYP_INT;
            break;
        case TYP_ULONG:
            constVal = comp->gtNewLconNode(0x8000000000000000);
            eltType  = TYP_LONG;
            break;
        default:
            unreached();
    }

    var_types type = layout->GetVectorType();
    // We don't have carry so SUB(x, INT_MIN) is the same as XOR(x, INT_MIN).
    // On Ryzen XOR has slightly higher throuput.
    NamedIntrinsic pxor = type == TYP_SIMD32 ? NI_AVX2_Xor : NI_SSE2_Xor;

    GenTree* constVector = NewVecNode(type, NI_VEC_SPLAT, constVal->GetType(), constVal);
    GenTree* constUses[2];
    impMakeMultiUse(constVector, constUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThan const temp"));
    *op1 = NewVecNode(type, pxor, eltType, *op1, constUses[0]);
    *op2 = NewVecNode(type, pxor, eltType, *op2, constUses[1]);

    return eltType;
}

GenTree* Importer::impVectorT128LongGreaterThanSse2(ClassLayout* layout, GenTree* op1, GenTree* op2, bool lessThan)
{
    assert(layout->GetSIMDType() == TYP_SIMD16);

    // Signed long compares can be implemented by comparing the 2 int halves:
    //   x > y =>
    //   (xh, xl) > (yh, yl) =>
    //   (xh > yh) || ((xh == yh) && (xl unsigned > yl))
    // so we generate:
    //   ; make the lower halves unsigned by adjusting the operands
    //   gt = PCMPGTD x, y
    //   gl = PSHUFD gt, ZZXX ; move the xl > yl result to the upper halves
    //   eq = PCMPEQD x, y
    //   g  = PAND eq, gl
    //   g  = POR gt, g
    //   g  = PSHUFD g, WWYY  ; copy result in upper halves to lower halves

    GenTree* uses[2][2];
    impMakeMultiUse(op1, uses[0], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThan temp"));
    impMakeMultiUse(op2, uses[1], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThan temp"));

    NamedIntrinsic intrinsic = lessThan ? NI_SSE2_CompareLessThan : NI_SSE2_CompareGreaterThan;

    GenTree* sign = NewVecNode(TYP_SIMD16, NI_VEC_SPLAT, TYP_LONG, comp->gtNewLconNode(1LL << 31));
    GenTree* signUses[2];
    impMakeMultiUse(sign, signUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThan const temp"));

    uses[0][1] = NewVecNode(TYP_SIMD16, NI_SSE2_Xor, TYP_LONG, uses[0][1], signUses[0]);
    uses[1][1] = NewVecNode(TYP_SIMD16, NI_SSE2_Xor, TYP_LONG, uses[1][1], signUses[1]);

    GenTree* gt = NewVecNode(TYP_SIMD16, intrinsic, TYP_INT, uses[0][1], uses[1][1]);
    GenTree* gtUses[2];
    impMakeMultiUse(gt, gtUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThan temp"));

    GenTree* im = comp->gtNewIconNode(SHUFFLE_ZZXX);
    GenTree* gl = NewVecNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, gtUses[0], im);
    GenTree* eq = NewVecNode(TYP_SIMD16, NI_SSE2_CompareEqual, TYP_INT, uses[0][0], uses[1][0]);

    gt = NewVecNode(TYP_SIMD16, NI_SSE2_And, TYP_INT, eq, gl);
    gt = NewVecNode(TYP_SIMD16, NI_SSE2_Or, TYP_INT, gtUses[1], gt);
    return NewVecNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, gt, comp->gtNewIconNode(SHUFFLE_WWYY));
}

GenTree* Importer::impVectorT128ULongGreaterThanSse2(ClassLayout* layout, GenTree* op1, GenTree* op2, bool lessThan)
{
    assert(layout->GetSIMDType() == TYP_SIMD16);

    // Unsigned long compares can be implemented by comparing the 2 unsigned int halves:
    //   x > y =>
    //   (xh, xl) > (yh, yl) =>
    //   (xh > yh) || ((xh == yh) && (xl > yl))
    // so we generate:
    //   gt = PCMPGTD x, y    ; make it unsigned by adjusting the operands
    //   gl = PSHUFD gt, ZZXX ; move the xl > yl result to the upper halves
    //   eq = PCMPEQD x, y
    //   g  = PAND eq, gl
    //   g  = POR gt, g
    //   g  = PSHUFD g, WWYY  ; copy result from upper halves to lower halves

    GenTree* uses[2][2];
    impMakeMultiUse(op1, uses[0], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThan temp"));
    impMakeMultiUse(op2, uses[1], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThan temp"));

    NamedIntrinsic intrinsic = lessThan ? NI_SSE2_CompareLessThan : NI_SSE2_CompareGreaterThan;
    impVectorTUnsignedCompareAdjust(layout, TYP_UINT, &uses[0][1], &uses[1][1]);
    GenTree* gt = NewVecNode(TYP_SIMD16, intrinsic, TYP_INT, uses[0][1], uses[1][1]);
    GenTree* gtUses[2];
    impMakeMultiUse(gt, gtUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThan temp"));

    GenTree* im = comp->gtNewIconNode(SHUFFLE_ZZXX);
    GenTree* gl = NewVecNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, gtUses[0], im);
    GenTree* eq = NewVecNode(TYP_SIMD16, NI_SSE2_CompareEqual, TYP_INT, uses[0][0], uses[1][0]);

    gt = NewVecNode(TYP_SIMD16, NI_SSE2_And, TYP_INT, eq, gl);
    gt = NewVecNode(TYP_SIMD16, NI_SSE2_Or, TYP_INT, gtUses[1], gt);
    return NewVecNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, gt, comp->gtNewIconNode(SHUFFLE_WWYY));
}

GenTree* Importer::impVectorT128LongEquals(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.paramCount == 2);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.retType == TYP_SIMD16);

    ClassLayout* layout  = sig.paramLayout[0];
    var_types    eltType = layout->GetElementType();
    assert(varTypeIsLong(eltType));

    if (comp->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        return NewVecNode(TYP_SIMD16, NI_SSE41_CompareEqual, TYP_LONG, op1, op2);
    }

    GenTree* eq = NewVecNode(TYP_SIMD16, NI_SSE2_CompareEqual, TYP_INT, op1, op2);
    GenTree* eqUses[2];
    impMakeMultiUse(eq, eqUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Equals temp"));
    GenTree* shuffleEq = NewVecNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, eqUses[0], comp->gtNewIconNode(SHUFFLE_ZWXY));
    return NewVecNode(TYP_SIMD16, NI_SSE2_And, TYP_LONG, shuffleEq, eqUses[1]);
}

GenTree* Importer::impVectorT128Compare(const HWIntrinsicSignature& sig,
                                        NamedIntrinsic              intrinsic,
                                        GenTree*                    op1,
                                        GenTree*                    op2)
{
    assert(sig.paramCount == 2);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.retType == TYP_SIMD16);

    ClassLayout* layout  = sig.paramLayout[0];
    var_types    eltType = layout->GetElementType();

    assert(varTypeIsIntegral(eltType));

    bool greaterThan = true;
    bool orEqual     = false;

    switch (intrinsic)
    {
        case NI_VectorT128_GreaterThanOrEqual:
            orEqual     = true;
            greaterThan = false;
            break;
        case NI_VectorT128_LessThanOrEqual:
            orEqual = true;
            break;
        case NI_VectorT128_LessThan:
            greaterThan = false;
            break;
        default:
            assert(intrinsic == NI_VectorT128_GreaterThan);
            break;
    }

    GenTree* gt;

    if (!varTypeIsLong(eltType) || comp->compOpportunisticallyDependsOn(InstructionSet_SSE42))
    {
        if (!varTypeIsLong(eltType))
        {
            intrinsic = greaterThan ? NI_SSE2_CompareGreaterThan : NI_SSE2_CompareLessThan;
        }
        else
        {
            intrinsic = greaterThan ? NI_SSE42_CompareGreaterThan : NI_SSE42_CompareLessThan;
        }

        if (varTypeIsUnsigned(eltType))
        {
            eltType = impVectorTUnsignedCompareAdjust(layout, eltType, &op1, &op2);
        }

        gt = NewVecNode(TYP_SIMD16, intrinsic, eltType, op1, op2);
    }
    else if (eltType == TYP_LONG)
    {
        gt = impVectorT128LongGreaterThanSse2(layout, op1, op2, !greaterThan);
    }
    else
    {
        gt = impVectorT128ULongGreaterThanSse2(layout, op1, op2, !greaterThan);
    }

    if (!orEqual)
    {
        return gt;
    }

    GenTree* allBitsSet = NewVecNode(TYP_SIMD16, NI_VEC_ONE_BITS, varTypeNodeType(eltType));
    return NewVecNode(TYP_SIMD16, NI_SSE2_Xor, varTypeNodeType(eltType), gt, allBitsSet);
}

GenTree* Importer::impVectorT256Compare(const HWIntrinsicSignature& sig,
                                        NamedIntrinsic              intrinsic,
                                        GenTree*                    op1,
                                        GenTree*                    op2)
{
    assert(sig.paramCount == 2);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert(sig.retType == TYP_SIMD32);

    ClassLayout* layout  = sig.paramLayout[0];
    var_types    eltType = layout->GetElementType();

    assert(varTypeIsIntegral(eltType));

    if (varTypeIsUnsigned(eltType))
    {
        eltType = impVectorTUnsignedCompareAdjust(layout, eltType, &op1, &op2);
    }

    bool orEqual = false;

    switch (intrinsic)
    {
        case NI_VectorT256_GreaterThanOrEqual:
            orEqual = true;
            FALLTHROUGH;
        case NI_VectorT256_LessThan:
            intrinsic = NI_AVX2_CompareLessThan;
            break;
        case NI_VectorT256_LessThanOrEqual:
            orEqual = true;
            FALLTHROUGH;
        default:
            intrinsic = NI_AVX2_CompareGreaterThan;
            break;
    }

    GenTree* gt = NewVecNode(TYP_SIMD32, intrinsic, eltType, op1, op2);

    if (!orEqual)
    {
        return gt;
    }

    GenTree* allBitsSet = NewVecNode(TYP_SIMD32, NI_VEC_ONE_BITS, varTypeNodeType(eltType));
    return NewVecNode(TYP_SIMD32, NI_AVX2_Xor, varTypeNodeType(eltType), gt, allBitsSet);
}
#endif // TARGET_XARCH

// Check whether two memory locations are contiguous.
//
// This recognizes trivial patterns such as IND(FIELD_ADDR(o, 4)) & IND(FIELD_ADDR(o, 8)) or
// IND(INDEX_ADDR(a, 1)) & IND(INDEX_ADDR(a, 2)).
// Pointer arithmetic isn't recognized (and probably not very useful anyway) and in the case of
// arrays only constant indices are recognized. Might be useful to also recognize i, i+1, i+2...
// If the locations are determined to be adjacent this also implies that the trees are also free
// of persistent side effects and they can be discarded. They may have exception side effects that
// may need to be preserved - a[1] doesn't imply that a[2] is also a valid array element.
//
bool SIMDCoalescingBuffer::AreContiguousLoads(GenTree* l1, GenTree* l2)
{
    assert(l1->TypeIs(TYP_FLOAT));
    assert(l2->TypeIs(TYP_FLOAT));

    if (l1->GetOper() != l2->GetOper())
    {
        return false;
    }

    auto AreValuesEqual = [](GenTree* v1, GenTree* v2) {
        while (v1->GetOper() == v2->GetOper())
        {
            if (v1->OperIs(GT_FIELD_ADDR))
            {
                if (v1->AsFieldAddr()->GetOffset() == v2->AsFieldAddr()->GetOffset())
                {
                    v1 = v1->AsFieldAddr()->GetAddr();
                    v2 = v2->AsFieldAddr()->GetAddr();

                    continue;
                }

                return false;
            }

            if (v1->OperIs(GT_IND_LOAD) && !v1->AsIndLoad()->IsVolatile() && !v2->AsIndLoad()->IsVolatile())
            {
                v1 = v1->AsIndLoad()->GetAddr();
                v2 = v2->AsIndLoad()->GetAddr();

                continue;
            }

            if (v1->OperIs(GT_LCL_LOAD))
            {
                return v1->AsLclLoad()->GetLcl() == v2->AsLclLoad()->GetLcl();
            }

            if (v1->OperIs(GT_LCL_ADDR))
            {
                return (v1->AsLclAddr()->GetLcl() == v2->AsLclAddr()->GetLcl()) &&
                       (v1->AsLclAddr()->GetLclOffs() == v2->AsLclAddr()->GetLclOffs());
            }

            break;
        }

        return false;
    };

    auto AreConsecutiveConstants = [](GenTree* i1, GenTree* i2) {
        return i1->OperIs(GT_CNS_INT) && i2->OperIs(GT_CNS_INT) &&
               (i1->AsIntCon()->GetValue() + 1 == i2->AsIntCon()->GetValue());
    };

    auto AreContiguosArrayElementAddresses = [&](GenTreeIndexAddr* e1, GenTreeIndexAddr* e2, var_types indirType) {
        return (varTypeSize(indirType) == e1->GetElemSize()) &&
               AreConsecutiveConstants(e1->GetIndex(), e2->GetIndex()) &&
               AreValuesEqual(e1->GetArray(), e2->GetArray());
    };

    auto AreContiguosFieldAddresses = [&](GenTreeFieldAddr* f1, GenTreeFieldAddr* f2, var_types indirType) {
        return (f1->GetOffset() + varTypeSize(indirType) == f2->GetOffset()) &&
               AreValuesEqual(f1->GetAddr(), f2->GetAddr());
    };

    auto AreContiguosIndirs = [&](GenTreeIndir* i1, GenTreeIndir* i2) {
        return !i1->IsVolatile() && !i2->IsVolatile() &&
               ((i1->GetAddr()->IsFieldAddr() && i2->GetAddr()->IsFieldAddr() &&
                 AreContiguosFieldAddresses(i1->GetAddr()->AsFieldAddr(), i2->GetAddr()->AsFieldAddr(),
                                            i1->GetType())) ||
                (i1->GetAddr()->IsIndexAddr() && i2->GetAddr()->IsIndexAddr() &&
                 AreContiguosArrayElementAddresses(i1->GetAddr()->AsIndexAddr(), i2->GetAddr()->AsIndexAddr(),
                                                   i1->GetType())));
    };

    auto AreContiguosLocalFields = [](GenTreeLclFld* f1, GenTreeLclFld* f2) {
        return (f1->GetLcl() == f2->GetLcl()) && (f1->GetLclOffs() + varTypeSize(f1->GetType()) == f2->GetLclOffs());
    };

    switch (l1->GetOper())
    {
        case GT_IND_LOAD:
            return AreContiguosIndirs(l1->AsIndLoad(), l2->AsIndLoad());
        case GT_LCL_LOAD_FLD:
            return AreContiguosLocalFields(l1->AsLclLoadFld(), l2->AsLclLoadFld());
        default:
            return false;
    }
}

bool SIMDCoalescingBuffer::AreContiguousStores(GenTree* s1, GenTree* s2)
{
    assert(s1->OperIs(GT_IND_STORE, GT_LCL_STORE_FLD) && s1->TypeIs(TYP_FLOAT));
    assert(s2->OperIs(GT_IND_STORE, GT_LCL_STORE_FLD) && s2->TypeIs(TYP_FLOAT));

    if (s1->GetOper() != s2->GetOper())
    {
        return false;
    }

    // TODO-MIKE-Cleanup: There's a lot of duplicate code here, see AreContiguousLoads.

    auto AreValuesEqual = [](GenTree* v1, GenTree* v2) {
        while (v1->GetOper() == v2->GetOper())
        {
            if (v1->OperIs(GT_FIELD_ADDR))
            {
                if (v1->AsFieldAddr()->GetOffset() == v2->AsFieldAddr()->GetOffset())
                {
                    v1 = v1->AsFieldAddr()->GetAddr();
                    v2 = v2->AsFieldAddr()->GetAddr();

                    continue;
                }

                return false;
            }

            if (v1->OperIs(GT_IND_LOAD) && !v1->AsIndLoad()->IsVolatile() && !v2->AsIndLoad()->IsVolatile())
            {
                v1 = v1->AsIndLoad()->GetAddr();
                v2 = v2->AsIndLoad()->GetAddr();

                continue;
            }

            if (v1->OperIs(GT_LCL_LOAD))
            {
                return v1->AsLclLoad()->GetLcl() == v2->AsLclLoad()->GetLcl();
            }

            if (v1->OperIs(GT_LCL_ADDR))
            {
                return (v1->AsLclAddr()->GetLcl() == v2->AsLclAddr()->GetLcl()) &&
                       (v1->AsLclAddr()->GetLclOffs() == v2->AsLclAddr()->GetLclOffs());
            }

            break;
        }

        return false;
    };

    auto AreConsecutiveConstants = [](GenTree* i1, GenTree* i2) {
        return i1->OperIs(GT_CNS_INT) && i2->OperIs(GT_CNS_INT) &&
               (i1->AsIntCon()->GetValue() + 1 == i2->AsIntCon()->GetValue());
    };

    auto AreContiguosArrayElementAddresses = [&](GenTreeIndexAddr* e1, GenTreeIndexAddr* e2, var_types indirType) {
        return (varTypeSize(indirType) == e1->GetElemSize()) &&
               AreConsecutiveConstants(e1->GetIndex(), e2->GetIndex()) &&
               AreValuesEqual(e1->GetArray(), e2->GetArray());
    };

    auto AreContiguosFieldAddresses = [&](GenTreeFieldAddr* f1, GenTreeFieldAddr* f2, var_types indirType) {
        return (f1->GetOffset() + varTypeSize(indirType) == f2->GetOffset()) &&
               AreValuesEqual(f1->GetAddr(), f2->GetAddr());
    };

    auto AreContiguosIndirs = [&](GenTreeIndir* i1, GenTreeIndir* i2) {
        return !i1->IsVolatile() && !i2->IsVolatile() &&
               ((i1->GetAddr()->IsFieldAddr() && i2->GetAddr()->IsFieldAddr() &&
                 AreContiguosFieldAddresses(i1->GetAddr()->AsFieldAddr(), i2->GetAddr()->AsFieldAddr(),
                                            i1->GetType())) ||
                (i1->GetAddr()->IsIndexAddr() && i2->GetAddr()->IsIndexAddr() &&
                 AreContiguosArrayElementAddresses(i1->GetAddr()->AsIndexAddr(), i2->GetAddr()->AsIndexAddr(),
                                                   i1->GetType())));
    };

    auto AreContiguosLocalFields = [](GenTreeLclFld* f1, GenTreeLclFld* f2) {
        return (f1->GetLcl() == f2->GetLcl()) && (f1->GetLclOffs() + varTypeSize(f1->GetType()) == f2->GetLclOffs());
    };

    switch (s1->GetOper())
    {
        case GT_IND_STORE:
            return AreContiguosIndirs(s1->AsIndStore(), s2->AsIndStore());
        case GT_LCL_STORE_FLD:
            return AreContiguosLocalFields(s1->AsLclStoreFld(), s2->AsLclStoreFld());
        default:
            return false;
    }
}

// Change a FLOAT typed IND_LOAD/LCL_LOAD_FLD node into a SIMD typed IND_LOAD/LCL_LOAD_FLD.
//
void SIMDCoalescingBuffer::ChangeToSIMDLoad(Compiler* compiler, GenTree* load, var_types simdType)
{
    assert(load->TypeIs(TYP_FLOAT));

    if (load->OperIs(GT_LCL_LOAD_FLD))
    {
        load->SetType(simdType);
        load->AsLclLoadFld()->SetFieldSeq(FieldSeqStore::NotAField());

        return;
    }

    GenTree* addr   = nullptr;
    unsigned offset = 0;

    if (GenTreeIndir* indir = load->IsIndir())
    {
        assert(!indir->IsVolatile());

        addr = indir->GetAddr();

        if (GenTreeFieldAddr* field = addr->IsFieldAddr())
        {
            // TODO-MIKE-Fix: This code replaces FIELD_ADDR with and ADD(addr, offset) without adding
            // a NULLCHECK when the field offset is large enough to require it. It's not worth fixing
            // this until FIELD is replaced by FIELD_ADDR, otherwise we need to add ADDR on top of
            // the existing FIELD and then use that as the address of the indir.

            addr   = field->GetAddr();
            offset = field->GetOffset();

            if (addr->OperIs(GT_LCL_ADDR) && (addr->AsLclAddr()->GetLclOffs() == 0))
            {
                // If this is the field of a local struct variable then set lvUsedInSIMDIntrinsic to prevent
                // the local from being promoted. If it gets promoted then it will be dependent-promoted due
                // to the indirection we're creating.

                // TODO-MIKE-Cleanup: This is done only for SIMD locals but it really should be done for any
                // struct local since the whole point is to block poor promotion.

                LclVarDsc* lcl = addr->AsLclAddr()->GetLcl();

                if (varTypeIsSIMD(lcl->GetType()))
                {
                    lcl->lvUsedInSIMDIntrinsic = true;
                }
            }

            // TODO-MIKE-Fix: This code replaces FIELD_ADDR with and ADD(addr, offset) without adding
            // a NULLCHECK when the field offset is large enough to require it. We need to keep the
            // FIELD_ADDR node and retype retype the indir.
        }
        else if (GenTreeIndexAddr* element = addr->IsIndexAddr())
        {
            GenTree* array = element->GetArray();
            unsigned index = static_cast<unsigned>(element->GetIndex()->AsIntCon()->GetValue());

            // Generate a bounds check for the array access. We access multiple array elements but for
            // bounds checking purposes it's sufficient to check if the last element index is valid,
            // then all the element indices before it will also be valid.

            unsigned simdElementCount = varTypeSize(simdType) / varTypeSize(TYP_FLOAT);

            GenTree* lastIndex  = compiler->gtNewIconNode(index + simdElementCount - 1, TYP_INT);
            GenTree* arrLen     = compiler->gtNewArrLen(compiler->gtCloneExpr(array), OFFSETOF__CORINFO_Array__length);
            GenTree* arrBndsChk = compiler->gtNewBoundsChk(lastIndex, arrLen, ThrowHelperKind::IndexOutOfRange);

            addr   = compiler->gtNewCommaNode(arrBndsChk, array);
            offset = OFFSETOF__CORINFO_Array__data + index * varTypeSize(TYP_FLOAT);
        }
        else
        {
            unreached();
        }
    }
    else
    {
        unreached();
    }

    if (offset != 0)
    {
        addr = compiler->gtNewOperNode(GT_ADD, TYP_BYREF, addr, compiler->gtNewIconNode(offset, TYP_I_IMPL));
    }

    load->ChangeOper(GT_IND_LOAD);
    load->SetType(simdType);
    load->AsIndLoad()->SetAddr(addr);
}

void SIMDCoalescingBuffer::ChangeToSIMDStore(Compiler* compiler, GenTree* store, var_types simdType, GenTree* value)
{
    assert(store->TypeIs(TYP_FLOAT));

    if (store->OperIs(GT_LCL_STORE_FLD))
    {
        store->SetType(simdType);
        store->AsLclStoreFld()->SetFieldSeq(FieldSeqStore::NotAField());
        store->AsLclStoreFld()->SetValue(value);

        return;
    }

    assert(store->OperIs(GT_IND_STORE));

    GenTreeIndir* indir  = store->IsIndir();
    GenTree*      addr   = indir->GetAddr();
    unsigned      offset = 0;

    assert(!indir->IsVolatile());

    // TODO-MIKE-Cleanup: There's a lot of duplicate code here, see ChangeToSIMDLoad.

    if (GenTreeFieldAddr* field = addr->IsFieldAddr())
    {
        // TODO-MIKE-Fix: This code replaces FIELD_ADDR with and ADD(addr, offset) without adding
        // a NULLCHECK when the field offset is large enough to require it. It's not worth fixing
        // this until FIELD is replaced by FIELD_ADDR, otherwise we need to add ADDR on top of
        // the existing FIELD and then use that as the address of the indir.

        addr   = field->GetAddr();
        offset = field->GetOffset();

        if (addr->OperIs(GT_LCL_ADDR) && (addr->AsLclAddr()->GetLclOffs() == 0))
        {
            // If this is the field of a local struct variable then set lvUsedInSIMDIntrinsic to prevent
            // the local from being promoted. If it gets promoted then it will be dependent-promoted due
            // to the indirection we're creating.

            // TODO-MIKE-Cleanup: This is done only for SIMD locals but it really should be done for any
            // struct local since the whole point is to block poor promotion.

            LclVarDsc* lcl = addr->AsLclAddr()->GetLcl();

            if (varTypeIsSIMD(lcl->GetType()))
            {
                lcl->lvUsedInSIMDIntrinsic = true;
            }
        }

        // TODO-MIKE-Fix: This code replaces FIELD_ADDR with and ADD(addr, offset) without adding
        // a NULLCHECK when the field offset is large enough to require it. We need to keep the
        // FIELD_ADDR node and retype retype the indir.
    }
    else if (GenTreeIndexAddr* element = addr->IsIndexAddr())
    {
        GenTree* array = element->GetArray();
        unsigned index = static_cast<unsigned>(element->GetIndex()->AsIntCon()->GetValue());

        // Generate a bounds check for the array access. We access multiple array elements but for
        // bounds checking purposes it's sufficient to check if the last element index is valid,
        // then all the element indices before it will also be valid.

        unsigned simdElementCount = varTypeSize(simdType) / varTypeSize(TYP_FLOAT);

        GenTree* lastIndex  = compiler->gtNewIconNode(index + simdElementCount - 1, TYP_INT);
        GenTree* arrLen     = compiler->gtNewArrLen(compiler->gtCloneExpr(array), OFFSETOF__CORINFO_Array__length);
        GenTree* arrBndsChk = compiler->gtNewBoundsChk(lastIndex, arrLen, ThrowHelperKind::IndexOutOfRange);

        addr   = compiler->gtNewCommaNode(arrBndsChk, array);
        offset = OFFSETOF__CORINFO_Array__data + index * varTypeSize(TYP_FLOAT);
    }
    else
    {
        unreached();
    }

    if (offset != 0)
    {
        addr = compiler->gtNewOperNode(GT_ADD, TYP_BYREF, addr, compiler->gtNewIconNode(offset, TYP_I_IMPL));
    }

    store->SetType(simdType);
    store->AsIndir()->SetAddr(addr);
    store->AsIndir()->SetValue(value);
}

// Recognize a field of a SIMD local variable (Vector2/3/4 fields).
LclVarDsc* SIMDCoalescingBuffer::IsSimdLocalField(GenTree* node, Compiler* compiler) const
{
    // We only care about Vector2/3/4 so the element type is always FLOAT.
    assert(node->TypeIs(TYP_FLOAT));

    if (!node->OperIs(GT_IND_LOAD))
    {
        return nullptr;
    }

    if (node->AsIndir()->IsVolatile())
    {
        // It probably doesn't make sense to coalesce volatile fields. Anyway LocalAddressVisitor
        // doesn't generate SIMDIntrinsicGetItem out of a volatile field and ChangeToSIMDMem does
        // not bother to make the indir it creates volatile...

        return nullptr;
    }

    node = node->AsIndir()->GetAddr();

    if (!node->IsFieldAddr())
    {
        return nullptr;
    }

    if (node->AsFieldAddr()->GetOffset() != m_index * varTypeSize(TYP_FLOAT))
    {
        return nullptr;
    }

    GenTree* addr = node->AsFieldAddr()->GetAddr();

    if (!addr->OperIs(GT_LCL_ADDR))
    {
        return nullptr;
    }

    GenTreeLclAddr* lclAddr = addr->AsLclAddr();

    if ((lclAddr->GetLclOffs() != 0) || !varTypeIsSIMD(lclAddr->GetLcl()->GetType()))
    {
        return nullptr;
    }

    return lclAddr->GetLcl();
}

LclVarDsc* SIMDCoalescingBuffer::IsSimdLocalExtract(GenTree* node) const
{
    // We only care about Vector2/3/4 so the element type is always FLOAT.
    assert(node->TypeIs(TYP_FLOAT));

    if (GenTreeHWIntrinsic* extract = node->IsHWIntrinsic())
    {
        if ((extract->GetIntrinsic() != NI_VEC_EXTRACT) || !extract->GetOp(0)->OperIs(GT_LCL_LOAD) ||
            !extract->GetOp(1)->IsIntCon(m_index))
        {
            return nullptr;
        }

        return extract->GetOp(0)->AsLclLoad()->GetLcl();
    }

    if (GenTreeLclFld* lclFld = node->IsLclLoadFld())
    {
        if (lclFld->GetLclOffs() != m_index * varTypeSize(TYP_FLOAT))
        {
            return nullptr;
        }

        return lclFld->GetLcl();
    }

    return nullptr;
};

// Try to add a store statement to the coalescing buffer (common code for Add and Mark).
// Return true if the statement is added and the number of statements in the buffer equals the number of SIMD elements.
bool SIMDCoalescingBuffer::AddStore(Compiler* compiler, Statement* stmt, GenTree* store, LclVarDsc* simdLcl)
{
    assert(store->OperIs(GT_LCL_STORE_FLD, GT_IND_STORE) && store->TypeIs(TYP_FLOAT));

    if (simdLcl == nullptr)
    {
        Clear();
        return false;
    }

    if (m_index == 0)
    {
        m_firstStmt = stmt;
        m_lastStmt  = stmt;
        m_lcl       = simdLcl;
        m_index++;
        return false;
    }

    if (simdLcl != m_lcl)
    {
        Clear();
        return false;
    }

    GenTree* lastStore = m_lastStmt->GetRootNode();

    if (!AreContiguousStores(lastStore, store))
    {
        Clear();
        return false;
    }

    m_lastStmt = stmt;
    m_index++;

    return (m_index == varTypeSize(simdLcl->GetType()) / varTypeSize(TYP_FLOAT));
}

// Mark local variables that may be subject to SIMD coalescing to prevent struct promotion.
//
// TODO-MIKE-Cleanup: It's unfortunate that we need to do SIMD coalescing in two steps: first mark
// locals that are subject to coalescing, to prevent struct promotion, and then actually do coalescing.
// In general phase ordering in this area is messy and it's likely better to be:
//     - import (no SIMD coalescing marking)
//     - other unrelated phases (e.g. inlining)
//     - "local address visitor" - convert every (recognized) indirect local access to LCL_VAR/LCL_FLD
//       and record some information to help guide struct promotion (though it's questionable if this
//       phase needs to exist at all, most of it can be done during import and it's really importer's
//       job to deal with issues arising from unfortunate IL characteristics)
//     - struct promotion + implicit byref params + DNER marking
//     - SIMD coalescing (likely done during the same flow graph traversal as struct promotion)
//     - global morph
//
// That said, SIMD coalescing (or any other kind of memory coalescing) is better done in lowering,
// doing it in the frontend interferes with VN and anything it depends on it. Unfortunately after
// global morph it's more difficult to recognize contiguous memory locations because INDEX_ADDR gets
// expanded into more complex trees. But then the coalescing code only recognizes constant array
// indices and COMMAs aren't present in LIR so probably there's not much difference.
//
void SIMDCoalescingBuffer::Mark(Compiler* compiler, Statement* stmt)
{
    GenTree* store = stmt->GetRootNode();

    if (!store->TypeIs(TYP_FLOAT) || !store->OperIs(GT_IND_STORE, GT_LCL_STORE_FLD))
    {
        Clear();
        return;
    }

    GenTree* value = store->OperIs(GT_IND_STORE) ? store->AsIndStore()->GetValue() : store->AsLclStoreFld()->GetValue();
    LclVarDsc* simdLcl = IsSimdLocalField(value, compiler);

    if (!AddStore(compiler, stmt, store, simdLcl))
    {
        return;
    }

    compiler->lvaRecordSimdIntrinsicUse(simdLcl);

    if (GenTreeIndir* indir = store->IsIndir())
    {
        if (GenTreeFieldAddr* field = indir->GetAddr()->IsFieldAddr())
        {
            GenTree* addr = field->GetAddr();

            if (addr->OperIs(GT_LCL_ADDR) && (addr->AsLclAddr()->GetLclOffs() == 0))
            {
                compiler->lvaRecordSimdIntrinsicUse(addr->AsLclAddr()->GetLcl());
            }
        }
    }

    Clear();
}

// Try to add a store statement to the coalescing buffer.
// Return true if the statement is added and the number of statements in the buffer equals the number of SIMD elements.
bool SIMDCoalescingBuffer::Add(Compiler* compiler, Statement* stmt)
{
    GenTree* store = stmt->GetRootNode();

    if (!store->TypeIs(TYP_FLOAT) || !store->OperIs(GT_IND_STORE, GT_LCL_STORE_FLD))
    {
        Clear();
        return false;
    }

    GenTree* value = store->OperIs(GT_IND_STORE) ? store->AsIndStore()->GetValue() : store->AsLclStoreFld()->GetValue();
    LclVarDsc* simdLcl = IsSimdLocalExtract(value);

    return AddStore(compiler, stmt, store, simdLcl);
}

// Transform the first store in the buffer into a SIMD store
// and remove the rest of the statements from the block.
void SIMDCoalescingBuffer::Coalesce(Compiler* compiler, BasicBlock* block)
{
    var_types type;

    switch (m_index)
    {
        case 2:
            type = TYP_SIMD8;
            break;
        case 3:
            type = TYP_SIMD12;
            break;
        default:
            assert(m_index == 4);
            type = TYP_SIMD16;
            break;
    }

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("Found %u contiguous assignments from a %s local to memory in " FMT_BB ":\n", m_index, varTypeName(type),
               block->bbNum);
        for (Statement* s = m_firstStmt; s != m_lastStmt->GetNextStmt(); s = s->GetNextStmt())
        {
            compiler->gtDispStmt(s);
        }
    }
#endif

    for (unsigned i = 1; i < m_index; i++)
    {
        compiler->fgRemoveStmt(block, m_firstStmt->GetNextStmt() DEBUGARG(false));
    }

    ChangeToSIMDStore(compiler, m_firstStmt->GetRootNode(), type, compiler->gtNewLclLoad(m_lcl, m_lcl->GetType()));

    JITDUMPSTMT(m_firstStmt, "Changed to a single %s store:\n", varTypeName(type));

    Clear();
}

#endif // FEATURE_HW_INTRINSICS
