// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef FEATURE_SIMD

var_types Compiler::GetVectorTSimdType()
{
#if defined(TARGET_XARCH)
    if (compOpportunisticallyDependsOn(InstructionSet_AVX2))
    {
        return JitConfig.EnableHWIntrinsic() ? TYP_SIMD32 : TYP_SIMD16;
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
                     NI_SIMD_AS_HWINTRINSIC_END - NI_SIMD_AS_HWINTRINSIC_START - 1);

static const SysNumSimdIntrinsicInfo& GetIntrinsicInfo(NamedIntrinsic id)
{
    assert((NI_SIMD_AS_HWINTRINSIC_START < id) && (id < NI_SIMD_AS_HWINTRINSIC_END));

    return sysNumSimdIntrinsicInfo[id - NI_SIMD_AS_HWINTRINSIC_START - 1];
}

static SysNumSimdIntrinsicClassId FindClassId(const char* className, const char* enclosingClassName)
{
    assert(className != nullptr);

    if ((enclosingClassName != nullptr) || (className[0] != 'V'))
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
                                                    const char*           methodName,
                                                    const char*           enclosingClassName)
{
    SysNumSimdIntrinsicClassId classId = FindClassId(className, enclosingClassName);

    if (classId == SysNumSimdIntrinsicClassId::Unknown)
    {
        return NI_Illegal;
    }

#ifdef TARGET_XARCH
    if ((classId == SysNumSimdIntrinsicClassId::VectorT128) && (GetVectorTSimdType() == TYP_SIMD32))
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

        return static_cast<NamedIntrinsic>(NI_SIMD_AS_HWINTRINSIC_START + 1 + i);
    }

    return NI_Illegal;
}

GenTree* Compiler::impImportSysNumSimdIntrinsic(NamedIntrinsic        intrinsic,
                                                CORINFO_CLASS_HANDLE  clsHnd,
                                                CORINFO_METHOD_HANDLE method,
                                                CORINFO_SIG_INFO*     sig,
                                                bool                  isNewObj)
{
#if defined(TARGET_XARCH)
    CORINFO_InstructionSet minimumIsa = InstructionSet_SSE2;
#elif defined(TARGET_ARM64)
    CORINFO_InstructionSet minimumIsa = InstructionSet_AdvSimd;
#else
#error Unsupported platform
#endif

    bool isSupported = featureSIMD && JitConfig.EnableHWIntrinsic() && compOpportunisticallyDependsOn(minimumIsa);

    if (intrinsic == NI_VectorT128_get_IsHardwareAccelerated
#ifdef TARGET_XARCH
        || intrinsic == NI_VectorT256_get_IsHardwareAccelerated
#endif
        )
    {
        return gtNewIconNode(isSupported);
    }

    if (!isSupported)
    {
        return nullptr;
    }

    HWIntrinsicSignature signature;
    signature.Read(this, sig);

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

    compFloatingPointUsed = true;

    if (hwIntrinsic == NI_SIMD_AS_HWINTRINSIC_START)
    {
        return impVector234TSpecial(intrinsic, signature, layout, isNewObj);
    }

    if (!compOpportunisticallyDependsOn(HWIntrinsicInfo::lookupIsa(hwIntrinsic)))
    {
        return nullptr;
    }

    var_types eltType = layout->GetElementType();
    unsigned  size    = layout->GetSize();
    GenTree*  ops[2];

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
    }

    switch (signature.paramCount)
    {
        case 0:
            assert(varTypeIsSIMD(signature.retType));
            return gtNewSimdHWIntrinsicNode(signature.retType, hwIntrinsic, eltType, size);

        case 1:
            assert(signature.retType == signature.paramType[0]);
            ops[0] = impSIMDPopStack(signature.paramType[0]);
            return gtNewSimdHWIntrinsicNode(signature.retType, hwIntrinsic, eltType, size, ops[0]);

        default:
            assert(signature.paramCount == 2);
            assert(signature.retType == signature.paramType[0]);
            assert(signature.paramLayout[0] == signature.paramLayout[1]);
            ops[1] = impSIMDPopStack(signature.paramType[1]);
            ops[0] = impSIMDPopStack(signature.paramType[0]);
            return gtNewSimdHWIntrinsicNode(signature.retType, hwIntrinsic, eltType, size, ops[0], ops[1]);
    }
}

GenTree* Compiler::impVector234TSpecial(NamedIntrinsic              intrinsic,
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
        case NI_Vector2_CreateBroadcast:
        case NI_Vector3_CreateBroadcast:
        case NI_Vector4_CreateBroadcast:
            return impVector234TCreateBroadcast(sig, layout, isNewObj);
        case NI_Vector2_Create:
        case NI_Vector3_Create:
        case NI_Vector4_Create:
            return impVector234Create(sig, layout, isNewObj);
        case NI_Vector3_CreateExtend1:
        case NI_Vector4_CreateExtend1:
        case NI_Vector4_CreateExtend2:
            return impVector234CreateExtend(sig, layout, isNewObj);
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
        ops[i - 1] = impSIMDPopStack(sig.paramType[i - 1]);
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
            return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_ConvertToVector256Int32WithTruncation, TYP_INT, 32,
                                            ops[0]);
        case NI_VectorT128_ConvertToSingle:
            return impVectorT128ConvertUInt32ToSingle(sig, ops[0]);
        case NI_VectorT256_ConvertToSingle:
            assert(sig.paramCount == 1);
            assert((sig.retType == TYP_SIMD32) && (sig.retType == sig.paramType[0]));
            if (sig.paramLayout[0]->GetElementType() == TYP_INT)
            {
                assert(sig.retLayout->GetElementType() == TYP_FLOAT);
                return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_ConvertToVector256Single, TYP_FLOAT, 32, ops[0]);
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
        case NI_Vector2_op_Division:
        case NI_Vector3_op_Division:
            return impVector23Division(sig, ops[0], ops[1]);
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

GenTree* Compiler::impVector234TOne(const HWIntrinsicSignature& sig)
{
    assert(varTypeIsSIMD(sig.retType));
    assert(sig.paramCount == 0);

    var_types type    = sig.retLayout->GetSIMDType();
    var_types eltType = sig.retLayout->GetElementType();
    unsigned  size    = sig.retLayout->GetSize();

    GenTree* one = gtNewOneConNode(eltType);
    return gtNewSimdHWIntrinsicNode(type, GetCreateSimdHWIntrinsic(type), eltType, size, one);
}

GenTree* Compiler::impVectorTCount(const HWIntrinsicSignature& sig, ClassLayout* layout)
{
    assert(sig.retType == TYP_INT);
    assert(sig.paramCount == 0);

    GenTreeIntCon* countNode = gtNewIconNode(layout->GetElementCount(), TYP_INT);
    countNode->gtFlags |= GTF_ICON_SIMD_COUNT;
    return countNode;
}

GenTree* Compiler::impVector234TCreateBroadcast(const HWIntrinsicSignature& sig, ClassLayout* layout, bool isNewObj)
{
    assert(sig.retType == TYP_VOID);
    assert(sig.hasThisParam);
    assert(layout->IsVector());
    assert(sig.paramCount == 1);

    GenTree* arg      = impPopStackCoerceArg(varActualType(sig.paramType[0]));
    GenTree* destAddr = isNewObj ? nullptr : impPopStack().val;
    GenTree* create;

    if (arg->IsIntegralConst(0) || arg->IsDblConPositiveZero())
    {
        create = gtNewZeroSimdHWIntrinsicNode(layout);
    }
    else
    {
        create = gtNewSimdHWIntrinsicNode(layout->GetSIMDType(), GetCreateSimdHWIntrinsic(layout->GetSIMDType()),
                                          layout->GetElementType(), layout->GetSize(), arg);
    }

    if (destAddr != nullptr)
    {
        return impAssignSIMDAddr(destAddr, create);
    }

    return create;
}

GenTree* Compiler::impVector234Create(const HWIntrinsicSignature& sig, ClassLayout* layout, bool isNewObj)
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
            areArgsContiguous = SIMDCoalescingBuffer::AreContiguousMemoryLocations(args[argIndex], args[argIndex + 1]);
        }

        if (!args[argIndex]->IsDblConPositiveZero())
        {
            areArgsZero = false;
        }
    }

    GenTree* destAddr = isNewObj ? nullptr : impPopStack().val;
    GenTree* create;

    if (areArgsContiguous)
    {
        SIMDCoalescingBuffer::ChangeToSIMDMem(this, args[0], layout->GetSIMDType());

        create = args[0];

        if ((destAddr != nullptr) && destAddr->OperIs(GT_ADDR) && destAddr->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR))
        {
            lvaRecordSimdIntrinsicUse(destAddr->AsUnOp()->GetOp(0)->AsLclVar());
        }
    }
    else if (areArgsZero)
    {
        create = gtNewZeroSimdHWIntrinsicNode(layout);
    }
    else
    {
        unsigned size     = layout->GetSize();
        unsigned argCount = sig.paramCount;

        switch (size)
        {
#ifdef TARGET_XARCH
            case 8:
                args[2] = gtNewDconNode(0, TYP_FLOAT);
                FALLTHROUGH;
#endif
            case 12:
                args[3]  = gtNewDconNode(0, TYP_FLOAT);
                size     = 16;
                argCount = 4;
                break;
            default:
                break;
        }

        create = gtNewSimdHWIntrinsicNode(layout->GetSIMDType(), GetCreateSimdHWIntrinsic(layout->GetSIMDType()),
                                          TYP_FLOAT, size, argCount, args);
    }

    if (destAddr != nullptr)
    {
        return impAssignSIMDAddr(destAddr, create);
    }

    return create;
}

GenTree* Compiler::impVector234CreateExtend(const HWIntrinsicSignature& sig, ClassLayout* layout, bool isNewObj)
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

    args[0] = impSIMDPopStack(sig.paramType[0]);

    GenTree* destAddr = isNewObj ? nullptr : impPopStack().val;
    GenTree* create;

    unsigned insertIndex = sig.paramType[0] == TYP_SIMD12 ? 3 : 2;

#ifdef TARGET_ARM64
    create = args[0];

    for (unsigned i = 1; i < sig.paramCount; i++)
    {
        create = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AdvSimd_Insert, TYP_FLOAT, 16, create,
                                          gtNewIconNode(insertIndex + i - 1), args[i]);
    }
#elif defined(TARGET_XARCH)
    if (sig.paramCount == 3)
    {
        args[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_FLOAT, 16, args[1]);
        args[2] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_FLOAT, 16, args[2]);
        create  = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_UnpackLow, TYP_FLOAT, 16, args[1], args[2]);
        create  = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_MoveLowToHigh, TYP_FLOAT, 16, args[0], create);
    }
    else if (compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        create = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE41_Insert, TYP_FLOAT, 16, args[0], args[1],
                                          gtNewIconNode(insertIndex << 4));
    }
    else if (insertIndex == 2)
    {
        create = gtNewZeroSimdHWIntrinsicNode(TYP_SIMD16, TYP_FLOAT);
        create = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_MoveScalar, TYP_FLOAT, 16, create, args[1]);
        create = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_MoveLowToHigh, TYP_FLOAT, 16, args[0], create);
    }
    else
    {
        assert(insertIndex == 3);

        GenTree* arg0Uses[3];
        impMakeMultiUse(args[0], 3, arg0Uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector3 extend temp"));

        args[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_FLOAT, 16, args[1]);

        create = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_MoveHighToLow, TYP_FLOAT, 16, arg0Uses[0], arg0Uses[1]);
        create = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_UnpackLow, TYP_FLOAT, 16, create, args[1]);
        create = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_MoveLowToHigh, TYP_FLOAT, 16, arg0Uses[2], create);
    }
#else
#error Unsupported platform
#endif

    create->SetType(layout->GetSIMDType());

    if (destAddr != nullptr)
    {
        return impAssignSIMDAddr(destAddr, create);
    }

    return create;
}

GenTree* Compiler::impPopStackAddrAsVector(var_types type)
{
    assert(varTypeIsSIMD(type));

    GenTree* addr = impPopStack().val;

    if (!addr->TypeIs(TYP_BYREF, TYP_I_IMPL))
    {
        BADCODE("incompatible stack type");
    }

    if (addr->OperIs(GT_ADDR))
    {
        GenTree* location = addr->AsUnOp()->GetOp(0);

        if (location->GetType() == type)
        {
            return location;
        }
    }

    return gtNewOperNode(GT_IND, type, addr);
}

GenTree* Compiler::impAssignSIMDAddr(GenTree* destAddr, GenTree* src)
{
    assert(destAddr->TypeIs(TYP_BYREF, TYP_I_IMPL));
    assert(src->OperIs(GT_IND, GT_HWINTRINSIC));
    assert(varTypeIsSIMD(src->GetType()));

    GenTree* dest;

    if (destAddr->OperIs(GT_ADDR) && destAddr->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR) &&
        (destAddr->AsUnOp()->GetOp(0)->GetType() == src->GetType()))
    {
        dest = destAddr->AsUnOp()->GetOp(0);

        if (GenTreeHWIntrinsic* hwi = src->IsHWIntrinsic())
        {
            lvaRecordSimdIntrinsicDef(dest->AsLclVar(), hwi);
        }

        assert(lvaGetDesc(dest->AsLclVar())->GetType() == src->GetType());
    }
    else
    {
        dest = gtNewIndir(src->GetType(), destAddr);
        dest->gtFlags |= GTF_GLOB_REF;
    }

    return gtNewAssignNode(dest, src);
}

GenTree* Compiler::impGetArrayElementsAsVector(ClassLayout*    layout,
                                               GenTree*        array,
                                               GenTree*        index,
                                               SpecialCodeKind indexThrowKind,
                                               SpecialCodeKind lastIndexThrowKind)
{
    assert(array->TypeIs(TYP_REF));
    assert((index == nullptr) || (varActualType(index->GetType()) == TYP_INT));

    if ((index != nullptr) && index->IsIntegralConst(0))
    {
        index = nullptr;
    }

    GenTree* arrayUses[3];
    impMakeMultiUse(array, index == nullptr ? 2 : 3, arrayUses, CHECK_SPILL_ALL DEBUGARG("Vector<T>.CopyTo temp"));
    array = arrayUses[0];

    GenTree* lastIndex = gtNewIconNode(layout->GetElementCount() - 1);
    GenTree* arrLen    = gtNewArrLen(arrayUses[1], OFFSETOF__CORINFO_Array__length, compCurBB);

    if (index != nullptr)
    {
        GenTree* indexUses[3];
        impMakeMultiUse(index, indexUses, CHECK_SPILL_ALL DEBUGARG("Vector<T>.CopyTo temp"));
        index = indexUses[0];

        lastIndex = gtNewOperNode(GT_ADD, TYP_INT, indexUses[1], lastIndex);
        array     = gtNewCommaNode(gtNewArrBoundsChk(lastIndex, arrLen, lastIndexThrowKind), array);
        arrLen    = gtNewArrLen(arrayUses[2], OFFSETOF__CORINFO_Array__length, compCurBB);
        array     = gtNewCommaNode(gtNewArrBoundsChk(indexUses[2], arrLen, indexThrowKind), array);
    }
    else
    {
        array = gtNewCommaNode(gtNewArrBoundsChk(lastIndex, arrLen, lastIndexThrowKind), array);
    }

    GenTree* offset = gtNewIconNode(OFFSETOF__CORINFO_Array__data, TYP_I_IMPL);

    if (index != nullptr)
    {
        GenTree* elementSize = gtNewIconNode(varTypeSize(layout->GetElementType()), TYP_I_IMPL);
#ifdef TARGET_64BIT
        index = gtNewCastNode(TYP_LONG, index, false, TYP_LONG);
#endif
        index = gtNewOperNode(GT_MUL, TYP_I_IMPL, index, elementSize);
        // TODO-MIKE-CQ: This should be removed, it's here only to minimize diffs
        // from the previous implementation that imported SIMDIntrinsicInitArray
        // as is, hiding the address mode and thus blocking CSE.
        index->gtFlags |= GTF_DONT_CSE;
        offset = gtNewOperNode(GT_ADD, TYP_I_IMPL, index, offset);
        offset->gtFlags |= GTF_DONT_CSE;
    }

    offset = gtNewOperNode(GT_ADD, TYP_BYREF, array, offset);
    offset->gtFlags |= GTF_DONT_CSE;

    GenTree* indir = gtNewOperNode(GT_IND, layout->GetSIMDType(), offset)->AsIndir();
    indir->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;
    return indir;
}

GenTree* Compiler::impVectorTFromArray(const HWIntrinsicSignature& sig, ClassLayout* layout, bool isNewObj)
{
    assert((sig.paramCount == 1) || (sig.paramCount == 2));
    assert(sig.paramType[0] == TYP_REF);
    assert((sig.paramCount == 1) || (sig.paramType[1] == TYP_INT));

    GenTree* index    = sig.paramCount == 1 ? nullptr : impPopStackCoerceArg(TYP_INT);
    GenTree* array    = impPopStackCoerceArg(TYP_REF);
    GenTree* destAddr = isNewObj ? nullptr : impPopStack().val;

    GenTree* indir = impGetArrayElementsAsVector(layout, array, index, SCK_RNGCHK_FAIL, SCK_RNGCHK_FAIL);

    if (destAddr != nullptr)
    {
        return impAssignSIMDAddr(destAddr, indir);
    }

    return indir;
}

GenTree* Compiler::impVector234TCopyTo(const HWIntrinsicSignature& sig, ClassLayout* layout)
{
    assert(sig.retType == TYP_VOID);
    assert(sig.hasThisParam);
    assert((sig.paramCount == 1) || (sig.paramCount == 2));
    assert(sig.paramType[0] == TYP_REF);
    assert((sig.paramCount == 1) || (sig.paramType[1] == TYP_INT));

    GenTree* index = sig.paramCount == 1 ? nullptr : impPopStackCoerceArg(TYP_INT);
    GenTree* array = impPopStackCoerceArg(TYP_REF);
    GenTree* value = impPopStackAddrAsVector(layout->GetSIMDType());

    GenTree* indir = impGetArrayElementsAsVector(layout, array, index, SCK_ARG_RNG_EXCPN, SCK_ARG_EXCPN);
    return gtNewAssignNode(indir, value);
}

GenTree* Compiler::impVectorTGetItem(const HWIntrinsicSignature& sig, ClassLayout* layout)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_INT);

    GenTree* index = impPopStackCoerceArg(TYP_INT);
    GenTree* value = impPopStackAddrAsVector(layout->GetSIMDType());

    return impVectorGetElement(layout, value, index);
}

GenTree* Compiler::impVector234TInstanceEquals(const HWIntrinsicSignature& sig)
{
    assert(sig.retType == TYP_BOOL);
    assert(sig.hasThisParam && (sig.paramCount == 1));

    GenTree* op1 = impSIMDPopStack(sig.paramType[0]);
    GenTree* op2 = impPopStackAddrAsVector(sig.paramType[0]);

    return impVector234TEquals(sig, op1, op2);
}

#ifdef TARGET_ARMARCH

GenTree* Compiler::impVector234TEquals(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool notEqual)
{
    assert(sig.retType == TYP_BOOL);
    assert((sig.hasThisParam && (sig.paramCount == 1)) || (sig.paramCount == 2));
    assert(sig.hasThisParam || (sig.paramLayout[0] == sig.paramLayout[1]));

    ClassLayout* layout    = sig.paramLayout[0];
    var_types    type      = layout->GetSIMDType();
    var_types    eltType   = layout->GetElementType();
    unsigned     size      = layout->GetSize();
    bool         isVector3 = type == TYP_SIMD12;

    if (isVector3)
    {
        type = TYP_SIMD16;
        size = 16;
    }

    NamedIntrinsic intrinsic = varTypeSize(eltType) == 8 ? NI_AdvSimd_Arm64_CompareEqual : NI_AdvSimd_CompareEqual;
    op1                      = gtNewSimdHWIntrinsicNode(type, intrinsic, eltType, size, op1, op2);

    if (isVector3)
    {
        op1 = gtNewSimdWithElementNode(type, TYP_INT, op1, gtNewIconNode(3), gtNewIconNode(-1));
    }

    op1 = gtNewSimdHWIntrinsicNode(type, NI_AdvSimd_Arm64_MinAcross, TYP_UBYTE, size, op1);
    op1 = gtNewSimdGetElementNode(type, TYP_UBYTE, op1, gtNewIconNode(0));
    return gtNewOperNode(notEqual ? GT_EQ : GT_NE, TYP_INT, op1, gtNewIconNode(0));
}

GenTree* Compiler::impVectorT128ConditionalSelect(const HWIntrinsicSignature& sig,
                                                  GenTree*                    mask,
                                                  GenTree*                    op1,
                                                  GenTree*                    op2)
{
    assert(sig.paramCount == 3);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert((sig.retLayout == sig.paramLayout[1]) && (sig.retLayout == sig.paramLayout[2]));
    assert(sig.retType == TYP_SIMD16);

    var_types eltType = sig.retLayout->GetElementType();

    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AdvSimd_BitwiseSelect, eltType, 16, mask, op1, op2);
}

GenTree* Compiler::impVector234Dot(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.paramCount == 2);
    assert(sig.paramType[0] != TYP_SIMD32);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.retType == TYP_FLOAT);

    if (sig.paramType[0] == TYP_SIMD8)
    {
        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD8, NI_AdvSimd_Multiply, TYP_FLOAT, 8, op1, op2);
        return gtNewSimdHWIntrinsicNode(TYP_FLOAT, NI_AdvSimd_Arm64_AddPairwiseScalar, TYP_FLOAT, 8, op1);
    }

    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AdvSimd_Multiply, TYP_FLOAT, 16, op1, op2);

    if (sig.paramType[0] == TYP_SIMD12)
    {
        op1 = gtNewSimdWithElementNode(TYP_SIMD16, TYP_FLOAT, op1, gtNewIconNode(3), gtNewDconNode(0, TYP_FLOAT));
    }

    return gtNewSimdHWIntrinsicNode(TYP_FLOAT, NI_Vector128_Sum, TYP_FLOAT, 16, op1);
}

GenTree* Compiler::impVectorT128Dot(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.paramCount == 2);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);

    ClassLayout* layout  = sig.paramLayout[0];
    var_types    eltType = layout->GetElementType();

    if (eltType == TYP_FLOAT)
    {
        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AdvSimd_Multiply, TYP_FLOAT, 16, op1, op2);
        return gtNewSimdHWIntrinsicNode(TYP_FLOAT, NI_Vector128_Sum, TYP_FLOAT, 16, op1);
    }

    if (eltType == TYP_DOUBLE)
    {
        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AdvSimd_Arm64_Multiply, TYP_DOUBLE, 16, op1, op2);
        return gtNewSimdHWIntrinsicNode(TYP_DOUBLE, NI_AdvSimd_Arm64_AddPairwiseScalar, TYP_DOUBLE, 16, op1);
    }

    if (varTypeIsLong(eltType))
    {
        // Since we eventually need a scalar result it's cheaper to simply extract
        // the 2 long elements and perform scalar multiplication/addition.

        GenTree* op1Uses[2];
        impMakeMultiUse(op1, op1Uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<long>.Multiply temp"));
        GenTree* op2Uses[2];
        impMakeMultiUse(op2, op2Uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<long>.Multiply temp"));

        op1 = gtNewSimdGetElementNode(TYP_SIMD16, TYP_LONG, op1Uses[0], gtNewIconNode(0));
        op2 = gtNewSimdGetElementNode(TYP_SIMD16, TYP_LONG, op2Uses[0], gtNewIconNode(0));

        GenTree* mul1 = gtNewOperNode(GT_MUL, TYP_LONG, op1, op2);

        op1 = gtNewSimdGetElementNode(TYP_SIMD16, TYP_LONG, op1Uses[1], gtNewIconNode(1));
        op2 = gtNewSimdGetElementNode(TYP_SIMD16, TYP_LONG, op2Uses[1], gtNewIconNode(1));

        GenTree* mul2 = gtNewOperNode(GT_MUL, TYP_LONG, op1, op2);

        return gtNewOperNode(GT_ADD, TYP_LONG, mul1, mul2);
    }

    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AdvSimd_Multiply, eltType, 16, op1, op2);
    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AdvSimd_Arm64_AddAcross, eltType, 16, op1);
    return gtNewSimdGetElementNode(TYP_SIMD16, eltType, op1, gtNewIconNode(0));
}

GenTree* Compiler::impVectorT128MinMax(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool isMax)
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

    GenTree* condition = gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, eltType, 16, uses[0][0], uses[1][0]);
    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AdvSimd_BitwiseSelect, eltType, 16, condition, uses[0][1],
                                    uses[1][1]);
}

GenTree* Compiler::impVectorT128Narrow(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
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

    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, lower, retEltType, 8, op1);
    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, upper, retEltType, 16, op1, op2);
}

GenTree* Compiler::impVectorT128Widen(const HWIntrinsicSignature& sig)
{
    assert(sig.retType == TYP_VOID);
    assert(sig.paramCount == 3);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramType[1] == TYP_BYREF);
    assert(sig.paramType[2] == TYP_BYREF);

    GenTree* hiAddr = impPopStack().val;
    GenTree* loAddr = impPopStack().val;
    GenTree* value  = impSIMDPopStack(sig.paramType[0]);

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

    GenTree* lo = gtNewSimdHWIntrinsicNode(TYP_SIMD16, lower, eltType, 8, uses[0]);
    GenTree* hi = gtNewSimdHWIntrinsicNode(TYP_SIMD16, upper, eltType, 16, uses[1]);
    impAppendTree(impAssignSIMDAddr(loAddr, lo), CHECK_SPILL_ALL, impCurStmtOffs);
    return impAssignSIMDAddr(hiAddr, hi);
}

GenTree* Compiler::impVectorTMultiply(const HWIntrinsicSignature& sig)
{
    assert(sig.retType == TYP_SIMD16);
    assert(sig.paramCount == 2);

    var_types vecType = sig.retType;
    var_types eltType = sig.retLayout->GetElementType();

    assert(vecType == TYP_SIMD16);

    NamedIntrinsic intrinsic = eltType == TYP_DOUBLE ? NI_AdvSimd_Arm64_Multiply : NI_AdvSimd_Multiply;
    GenTree*       op1;
    GenTree*       op2;

    if (sig.paramLayout[0] == nullptr)
    {
        assert(sig.paramType[0] == eltType);
        assert(sig.paramLayout[1] == sig.retLayout);

        op2 = impSIMDPopStack(sig.paramType[1]);
        op1 = impPopStack().val;

        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, eltType, 16, op1);
    }
    else if (sig.paramLayout[1] == nullptr)
    {
        assert(sig.paramLayout[0] == sig.retLayout);
        assert(sig.paramType[1] == eltType);

        op2 = impPopStack().val;
        op1 = impSIMDPopStack(sig.paramType[0]);

        if (varTypeIsByte(eltType))
        {
            op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, eltType, 16, op2);
        }
        else
        {
            intrinsic = eltType == TYP_DOUBLE ? NI_AdvSimd_Arm64_MultiplyByScalar : NI_AdvSimd_MultiplyByScalar;
            op2       = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, eltType, 16, op2);
        }
    }
    else
    {
        assert((sig.retLayout == sig.paramLayout[0]) && (sig.retLayout == sig.paramLayout[1]));

        op2 = impSIMDPopStack(sig.paramType[0]);
        op1 = impSIMDPopStack(sig.paramType[0]);
    }

    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, eltType, 16, op1, op2);
}

#endif // TARGET_ARMARCH

#ifdef TARGET_XARCH

GenTree* Compiler::impVector234T128Abs(const HWIntrinsicSignature& sig, GenTree* op1)
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
            mask      = gtNewDconNode(jitstd::bit_cast<float, int32_t>(0x7fffffff), TYP_FLOAT);
            intrinsic = NI_SSE_And;
        }
        else
        {
            assert(eltType == TYP_DOUBLE);
            mask      = gtNewDconNode(jitstd::bit_cast<double, int64_t>(0x7fffffffffffffffLL), TYP_DOUBLE);
            intrinsic = NI_SSE2_And;
        }

        mask = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, eltType, 16, mask);
        return gtNewSimdHWIntrinsicNode(sig.retType, intrinsic, eltType, 16, op1, mask);
    }

    if ((eltType != TYP_LONG) && compOpportunisticallyDependsOn(InstructionSet_SSSE3))
    {
        return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSSE3_Abs, eltType, 16, op1);
    }

    GenTree* uses[2];
    impMakeMultiUse(op1, uses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Abs temp"));

    GenTree* sign;

    if (eltType == TYP_BYTE)
    {
        sign = gtNewZeroSimdHWIntrinsicNode(layout);
        sign = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_CompareGreaterThan, TYP_BYTE, 16, sign, uses[0]);
    }
    else if ((eltType == TYP_SHORT) || (eltType == TYP_INT))
    {
        sign = gtNewIconNode(varTypeBitSize(eltType) - 1);
        sign = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightArithmetic, eltType, 16, uses[0], sign);
    }
    else if (compOpportunisticallyDependsOn(InstructionSet_SSE42))
    {
        sign = gtNewZeroSimdHWIntrinsicNode(layout);
        sign = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE42_CompareGreaterThan, TYP_LONG, 16, sign, uses[0]);
    }
    else
    {
        sign = gtNewIconNode(31);
        sign = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightArithmetic, TYP_INT, 16, uses[0], sign);
        sign = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, sign, gtNewIconNode(0b11110101));
    }

    GenTree* signUses[2];
    impMakeMultiUse(sign, signUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Abs sign temp"));

    GenTree* tmp = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Xor, eltType, 16, signUses[0], uses[1]);
    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Subtract, eltType, 16, tmp, signUses[1]);
}

GenTree* Compiler::impVectorT256Abs(const HWIntrinsicSignature& sig, GenTree* op1)
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
            mask = gtNewDconNode(jitstd::bit_cast<float, int32_t>(0x7fffffff), TYP_FLOAT);
        }
        else
        {
            assert(eltType == TYP_DOUBLE);
            mask = gtNewDconNode(jitstd::bit_cast<double, int64_t>(0x7fffffffffffffffLL), TYP_DOUBLE);
        }

        mask = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector256_Create, eltType, 32, mask);
        return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_And, eltType, 32, op1, mask);
    }

    if (eltType == TYP_LONG)
    {
        GenTree* uses[2];
        impMakeMultiUse(op1, uses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Abs temp"));

        GenTree* zero = gtNewZeroSimdHWIntrinsicNode(layout);
        GenTree* sign = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_CompareGreaterThan, TYP_LONG, 32, zero, uses[0]);
        GenTree* signUses[2];
        impMakeMultiUse(sign, signUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Abs sign temp"));

        GenTree* tmp = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_Xor, eltType, 32, signUses[0], uses[1]);
        return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_Subtract, eltType, 32, tmp, signUses[1]);
    }

    return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_Abs, eltType, 32, op1);
}

GenTree* Compiler::impVectorTAndNot(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.paramCount == 2);
    assert((sig.retLayout == sig.paramLayout[0]) && (sig.retLayout == sig.paramLayout[1]));

    // PANDN/ANDNPS/ANDNPS is actually ~x & y rather than x & ~y
    // so we need to swap the operand order.

    if (!gtCanSwapOrder(op1, op2))
    {
        // TODO-MIKE-Review: Can we simply set GTF_REVERSE_OPS to avoid creating a temp?

        unsigned lclNum = lvaGrabTemp(true DEBUGARG("Vector<T>.AndNot temp"));
        impAppendTempAssign(lclNum, op1, sig.paramLayout[0], CHECK_SPILL_ALL);
        op1 = gtNewLclvNode(lclNum, sig.paramType[0]);
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

    return gtNewSimdHWIntrinsicNode(type, intrinsic, eltType, size, op2, op1);
}

constexpr ssize_t SHUFFLE_XXZX = 0x08; // 00 00 10 00
constexpr ssize_t SHUFFLE_ZWXY = 0xB1; // 10 11 00 01
constexpr ssize_t SHUFFLE_WWYY = 0xF5; // 11 11 01 01
constexpr ssize_t SHUFFLE_ZZXX = 0xA0; // 10 10 00 00

GenTree* Compiler::impVectorT128ConvertUInt32ToSingle(const HWIntrinsicSignature& sig, GenTree* op1)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramLayout[0]->GetElementType() == TYP_UINT);
    assert(sig.retType == TYP_SIMD16);
    assert(sig.retLayout->GetElementType() == TYP_FLOAT);

    GenTree* uses[2];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* c;
    uses[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical, TYP_INT, 16, uses[0], gtNewIconNode(16));
    c       = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_INT, 16, gtNewIconNode(0x53000000));
    uses[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_Or, TYP_FLOAT, 16, uses[0], c);
    c       = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_INT, 16, gtNewIconNode(0x53000000));
    uses[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_Subtract, TYP_FLOAT, 16, uses[0], c);

    uses[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical, TYP_INT, 16, uses[1], gtNewIconNode(16));
    uses[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical, TYP_INT, 16, uses[1], gtNewIconNode(16));
    uses[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ConvertToVector128Single, TYP_INT, 16, uses[1]);

    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_Add, TYP_FLOAT, 16, uses[0], uses[1]);
}

GenTree* Compiler::impVectorT256ConvertUInt32ToSingle(const HWIntrinsicSignature& sig, GenTree* op1)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert(sig.paramLayout[0]->GetElementType() == TYP_UINT);
    assert(sig.retType == TYP_SIMD32);
    assert(sig.retLayout->GetElementType() == TYP_FLOAT);

    GenTree* uses[2];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* c;
    uses[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, TYP_INT, 32, uses[0], gtNewIconNode(16));
    c       = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector256_Create, TYP_INT, 32, gtNewIconNode(0x53000000));
    uses[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Or, TYP_FLOAT, 32, uses[0], c);
    c       = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector256_Create, TYP_INT, 32, gtNewIconNode(0x53000000));
    uses[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Subtract, TYP_FLOAT, 32, uses[0], c);

    uses[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftLeftLogical, TYP_INT, 32, uses[1], gtNewIconNode(16));
    uses[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, TYP_INT, 32, uses[1], gtNewIconNode(16));
    uses[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_ConvertToVector256Single, TYP_FLOAT, 32, uses[1]);

    return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Add, TYP_FLOAT, 32, uses[0], uses[1]);
}

GenTree* Compiler::impVectorT128ConvertInt64ToDouble(const HWIntrinsicSignature& sig, GenTree* op1)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramLayout[0]->GetElementType() == TYP_LONG);
    assert(sig.retType == TYP_SIMD16);
    assert(sig.retLayout->GetElementType() == TYP_DOUBLE);

#ifndef TARGET_64BIT
    GenTree* uses[3];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* sign =
        gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical, TYP_LONG, 16, uses[0], gtNewIconNode(63));
    sign = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical, TYP_LONG, 16, sign, gtNewIconNode(63));

    GenTree* uns = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, uses[1], gtNewIconNode(0xF5));
    uns = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightArithmetic, TYP_INT, 16, uns, gtNewIconNode(32));
    GenTree* unsUses[2];
    impMakeMultiUse(uns, unsUses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));
    uns = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Xor, TYP_LONG, 16, uses[2], unsUses[0]);
    uns = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Subtract, TYP_LONG, 16, uns, unsUses[1]);

    GenTree* e[2];
    GenTree* c[2];

    impMakeMultiUse(uns, 2, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical, TYP_LONG, 16, uses[0], gtNewIconNode(32));
    c[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_LONG, 16, gtNewLconNode(0x4530000000000000));
    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Or, TYP_DOUBLE, 16, e[0], c[0]);
    c[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_LONG, 16, gtNewLconNode(0x4530000000000000));
    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Subtract, TYP_DOUBLE, 16, e[0], c[0]);

    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical, TYP_LONG, 16, uses[1], gtNewIconNode(32));
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical, TYP_LONG, 16, e[1], gtNewIconNode(32));
    c[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_LONG, 16, gtNewLconNode(0x4330000000000000));
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Or, TYP_DOUBLE, 16, e[1], c[1]);
    c[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_LONG, 16, gtNewLconNode(0x4330000000000000));
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Subtract, TYP_DOUBLE, 16, e[1], c[1]);

    uns = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Add, TYP_DOUBLE, 16, e[0], e[1]);

    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_Or, TYP_FLOAT, 16, uns, sign);
#else
    GenTree* uses[4];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* e[2];

    e[0] = gtNewSimdHWIntrinsicNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64, TYP_LONG, 16, uses[0]);
    e[0] =
        gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Double, TYP_LONG, 16, uses[1], e[0]);

    if (compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        e[1] = gtNewSimdHWIntrinsicNode(TYP_LONG, NI_SSE41_X64_Extract, TYP_LONG, 16, uses[2], gtNewIconNode(1));
    }
    else
    {
        e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, uses[2], gtNewIconNode(0b00001110));
        e[1] = gtNewSimdHWIntrinsicNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64, TYP_LONG, 16, e[1]);
    }

    e[1] =
        gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Double, TYP_LONG, 16, uses[3], e[1]);
    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_UnpackLow, TYP_DOUBLE, 16, e[0], e[1]);
#endif
}

GenTree* Compiler::impVectorT256ConvertInt64ToDouble(const HWIntrinsicSignature& sig, GenTree* op1)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert(sig.paramLayout[0]->GetElementType() == TYP_LONG);
    assert(sig.retType == TYP_SIMD32);
    assert(sig.retLayout->GetElementType() == TYP_DOUBLE);

#ifndef TARGET_64BIT
    GenTree* uses[3];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* sign =
        gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, TYP_LONG, 32, uses[0], gtNewIconNode(63));
    sign = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftLeftLogical, TYP_LONG, 32, sign, gtNewIconNode(63));

    GenTree* uns = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_Shuffle, TYP_INT, 32, uses[1], gtNewIconNode(0xF5));
    uns = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftRightArithmetic, TYP_INT, 32, uns, gtNewIconNode(32));
    GenTree* unsUses[2];
    impMakeMultiUse(uns, unsUses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));
    uns = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_Xor, TYP_LONG, 32, uses[2], unsUses[0]);
    uns = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_Subtract, TYP_LONG, 32, uns, unsUses[1]);

    GenTree* e[2];
    GenTree* c[2];

    impMakeMultiUse(uns, 2, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, TYP_LONG, 32, uses[0], gtNewIconNode(32));
    c[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector256_Create, TYP_LONG, 32, gtNewLconNode(0x4530000000000000));
    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Or, TYP_DOUBLE, 32, e[0], c[0]);
    c[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector256_Create, TYP_LONG, 32, gtNewLconNode(0x4530000000000000));
    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Subtract, TYP_DOUBLE, 32, e[0], c[0]);

    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftLeftLogical, TYP_LONG, 32, uses[1], gtNewIconNode(32));
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, TYP_LONG, 32, e[1], gtNewIconNode(32));
    c[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector128_Create, TYP_LONG, 32, gtNewLconNode(0x4330000000000000));
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Or, TYP_DOUBLE, 32, e[1], c[1]);
    c[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector128_Create, TYP_LONG, 32, gtNewLconNode(0x4330000000000000));
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Subtract, TYP_DOUBLE, 32, e[1], c[1]);

    uns = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Add, TYP_DOUBLE, 32, e[0], e[1]);

    return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Or, TYP_FLOAT, 32, uns, sign);
#else
    // TODO-MIKE-Cleanup: These temps should be SIMD16 but we don't have a SIMD16 layout
    // handy so they're SIMD32 instead. This would mean that all the uses should really
    // be NI_Vector256_GetLower but that's kind of overkill, ignore it for now.
    GenTree* uses[5];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* e[4];
    e[0] = gtNewSimdHWIntrinsicNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64, TYP_LONG, 16, uses[0]);
    e[0] =
        gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Double, TYP_LONG, 16, uses[1], e[0]);
    e[1] = gtNewSimdHWIntrinsicNode(TYP_LONG, NI_SSE41_X64_Extract, TYP_LONG, 16, uses[2], gtNewIconNode(1));
    e[1] =
        gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Double, TYP_LONG, 16, uses[3], e[1]);
    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_UnpackLow, TYP_DOUBLE, 16, e[0], e[1]);

    uses[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AVX_ExtractVector128, TYP_FLOAT, 32, uses[4], gtNewIconNode(1));
    uses[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector128_ToVector256Unsafe, TYP_LONG, 16, uses[0]);
    impMakeMultiUse(uses[0], 4, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    e[2] = gtNewSimdHWIntrinsicNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64, TYP_LONG, 16, uses[0]);
    e[2] =
        gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Double, TYP_LONG, 16, uses[1], e[2]);
    e[3] = gtNewSimdHWIntrinsicNode(TYP_LONG, NI_SSE41_X64_Extract, TYP_LONG, 16, uses[2], gtNewIconNode(1));
    e[3] =
        gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Double, TYP_LONG, 16, uses[3], e[3]);
    e[2] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_UnpackLow, TYP_DOUBLE, 16, e[2], e[3]);

    return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_InsertVector128, TYP_DOUBLE, 32, e[0], e[2], gtNewIconNode(1));
#endif
}

GenTree* Compiler::impVectorT128ConvertUInt64ToDouble(const HWIntrinsicSignature& sig, GenTree* op1)
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

    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical, TYP_LONG, 16, uses[0], gtNewIconNode(32));
    c[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_LONG, 16, gtNewLconNode(0x4530000000000000));
    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Or, TYP_DOUBLE, 16, e[0], c[0]);
    c[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_LONG, 16, gtNewLconNode(0x4530000000000000));
    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Subtract, TYP_DOUBLE, 16, e[0], c[0]);

    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical, TYP_LONG, 16, uses[1], gtNewIconNode(32));
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical, TYP_LONG, 16, e[1], gtNewIconNode(32));
    c[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_LONG, 16, gtNewLconNode(0x4330000000000000));
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Or, TYP_DOUBLE, 16, e[1], c[1]);
    c[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_LONG, 16, gtNewLconNode(0x4330000000000000));
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Subtract, TYP_DOUBLE, 16, e[1], c[1]);

    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Add, TYP_DOUBLE, 16, e[0], e[1]);
}

GenTree* Compiler::impVectorT256ConvertUInt64ToDouble(const HWIntrinsicSignature& sig, GenTree* op1)
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

    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, TYP_LONG, 32, uses[0], gtNewIconNode(32));
    c[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector256_Create, TYP_LONG, 32, gtNewLconNode(0x4530000000000000));
    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Or, TYP_DOUBLE, 32, e[0], c[0]);
    c[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector256_Create, TYP_LONG, 32, gtNewLconNode(0x4530000000000000));
    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Subtract, TYP_DOUBLE, 32, e[0], c[0]);

    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftLeftLogical, TYP_LONG, 32, uses[1], gtNewIconNode(32));
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, TYP_LONG, 32, e[1], gtNewIconNode(32));
    c[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector256_Create, TYP_LONG, 32, gtNewLconNode(0x4330000000000000));
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Or, TYP_DOUBLE, 32, e[1], c[1]);
    c[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector256_Create, TYP_LONG, 32, gtNewLconNode(0x4330000000000000));
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Subtract, TYP_DOUBLE, 32, e[1], c[1]);

    return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Add, TYP_DOUBLE, 32, e[0], e[1]);
}

GenTree* Compiler::impVectorT128ConvertDoubleToInt64(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramLayout[0]->GetElementType() == TYP_DOUBLE);
    assert(sig.retType == TYP_SIMD16);
    assert(sig.retLayout->GetElementType() == TYP_LONG);

#ifndef TARGET_64BIT
    return nullptr;
#else
    GenTree* op1 = impSIMDPopStack(TYP_SIMD16);

    GenTree* uses[4];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* e[2];
    e[0] = gtNewSimdHWIntrinsicNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64WithTruncation, TYP_DOUBLE, 16, uses[2]);
    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Int64, TYP_LONG, 16, e[0]);
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_UnpackHigh, TYP_DOUBLE, 16, uses[0], uses[1]);
    e[1] = gtNewSimdHWIntrinsicNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64WithTruncation, TYP_DOUBLE, 16, e[1]);

    if (!compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Int64, TYP_LONG, 16, e[1]);
        return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_UnpackLow, TYP_LONG, 16, e[0], e[1]);
    }

    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE41_X64_Insert, TYP_LONG, 16, e[0], e[1], gtNewIconNode(1));
#endif
}

GenTree* Compiler::impVectorT256ConvertDoubleToInt64(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert(sig.paramLayout[0]->GetElementType() == TYP_DOUBLE);
    assert(sig.retType == TYP_SIMD32);
    assert(sig.retLayout->GetElementType() == TYP_LONG);

#ifndef TARGET_64BIT
    return nullptr;
#else
    GenTree* op1 = impSIMDPopStack(TYP_SIMD32);

    // TODO-MIKE-Cleanup: These temps should be SIMD16 but we don't have a SIMD16 layout
    // handy so they're SIMD32 instead. This would mean that all the uses should really
    // be NI_Vector256_GetLower but that's kind of overkill, ignore it for now.
    GenTree* uses[4];
    impMakeMultiUse(op1, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    GenTree* e[4];
    e[0] = gtNewSimdHWIntrinsicNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64WithTruncation, TYP_DOUBLE, 16, uses[2]);
    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Int64, TYP_LONG, 16, e[0]);
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_UnpackHigh, TYP_DOUBLE, 16, uses[0], uses[1]);
    e[1] = gtNewSimdHWIntrinsicNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64WithTruncation, TYP_DOUBLE, 16, e[1]);

    uses[3] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AVX_ExtractVector128, TYP_FLOAT, 32, uses[3], gtNewIconNode(1));
    uses[3] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector128_ToVector256Unsafe, TYP_LONG, 16, uses[3]);
    impMakeMultiUse(uses[3], 3, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Convert temp"));

    e[2] = gtNewSimdHWIntrinsicNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64WithTruncation, TYP_DOUBLE, 16, uses[2]);
    e[2] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Int64, TYP_LONG, 16, e[2]);
    e[3] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_UnpackHigh, TYP_DOUBLE, 16, uses[0], uses[1]);
    e[3] = gtNewSimdHWIntrinsicNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64WithTruncation, TYP_DOUBLE, 16, e[3]);

    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE41_X64_Insert, TYP_LONG, 16, e[0], e[1], gtNewIconNode(1));
    e[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector128_ToVector256Unsafe, TYP_LONG, 16, e[0]);
    e[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE41_X64_Insert, TYP_LONG, 16, e[2], e[3], gtNewIconNode(1));

    return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_InsertVector128, TYP_LONG, 32, e[0], e[1], gtNewIconNode(1));
#endif
}

GenTree* Compiler::impVector23Division(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.paramCount == 2);
    assert((sig.retLayout == sig.paramLayout[0]) && (sig.retLayout == sig.paramLayout[1]));
    assert((sig.retLayout->GetSize() == 8) || (sig.retLayout->GetSize() == 12));
    assert(sig.retLayout->GetElementType() == TYP_FLOAT);

    ClassLayout* layout = sig.retLayout;
    var_types    type   = layout->GetSIMDType();
    unsigned     size   = layout->GetSize();

    GenTree* d = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_Divide, TYP_FLOAT, 16, op1, op2);

    // Since the top-most elements will be zero, we end up perfoming 0 / 0 which is NaN.
    // Therefore, post division we need to set the top-most elements to zero. This is
    // achieved by left logical shift followed by right logical shift of the result.
    d = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical128BitLane, TYP_INT, 16, d,
                                 gtNewIconNode(16 - size));
    d = gtNewSimdHWIntrinsicNode(type, NI_SSE2_ShiftRightLogical128BitLane, TYP_INT, 16, d, gtNewIconNode(16 - size));

    return d;
}

GenTree* Compiler::impVector234Dot(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.paramCount == 2);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.paramLayout[0]->GetElementType() == TYP_FLOAT);
    assert(sig.retType == TYP_FLOAT);

    ClassLayout* layout = sig.paramLayout[0];
    unsigned     size   = layout->GetSize();

    if (compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        uint8_t imm = 0b11110000;
        imm >>= 4 - layout->GetElementCount();
        imm &= 0b11110000;
        imm |= 0b00000001;

        op1 = gtNewSimdHWIntrinsicNode(TYP_FLOAT, NI_SSE41_DotProduct, TYP_FLOAT, 16, op1, op2, gtNewIconNode(imm));
        return gtNewSimdHWIntrinsicNode(TYP_FLOAT, NI_Vector128_GetElement, TYP_FLOAT, 16, op1, gtNewIconNode(0));
    }

    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_Multiply, TYP_FLOAT, 16, op1, op2);
    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Sum, TYP_FLOAT, size, op1);
    return gtNewSimdGetElementNode(TYP_SIMD16, TYP_FLOAT, op1, gtNewIconNode(0));
}

GenTree* Compiler::impVectorT128Dot(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 2);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);

    var_types eltType = sig.paramLayout[0]->GetElementType();

#ifndef TARGET_64BIT
    // Vector128_Dot<long> works on x86 but it cannot be decomposed.
    // TODO-MIKE-CQ: If we change the type of Vector128_Sum to be vector rather
    // than LONG we could extract the LONG value from the vector and extraction
    // can be decomposed.
    if (varTypeIsLong(eltType))
    {
        return nullptr;
    }
#endif

    bool hasSse41 = compOpportunisticallyDependsOn(InstructionSet_SSE41);

    if ((varTypeIsInt(eltType) || varTypeIsLong(eltType)) && !hasSse41)
    {
        return nullptr;
    }

    GenTree* op1 = impSIMDPopStack(TYP_SIMD16);
    GenTree* op2 = impSIMDPopStack(TYP_SIMD16);

    if (varTypeIsFloating(eltType) && hasSse41)
    {
        uint8_t imm = eltType == TYP_FLOAT ? 0b11110001 : 0b00110001;

        op1 = gtNewSimdHWIntrinsicNode(eltType, NI_SSE41_DotProduct, eltType, 16, op1, op2, gtNewIconNode(imm));
        return gtNewSimdHWIntrinsicNode(eltType, NI_Vector128_GetElement, eltType, 16, op1, gtNewIconNode(0));
    }

    switch (eltType)
    {
        case TYP_FLOAT:
            op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_Multiply, TYP_FLOAT, 16, op1, op2);
            break;
        case TYP_DOUBLE:
            op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Multiply, TYP_DOUBLE, 16, op1, op2);
            break;
        case TYP_LONG:
        case TYP_ULONG:
            op1     = impVectorTMultiplyLong(sig.paramLayout[0], op1, op2);
            eltType = TYP_LONG;
            break;
        case TYP_INT:
        case TYP_UINT:
            op1     = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE41_MultiplyLow, TYP_INT, 16, op1, op2);
            eltType = TYP_INT;
            break;
        case TYP_SHORT:
        case TYP_USHORT:
            op1     = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_MultiplyAddAdjacent, TYP_INT, 16, op1, op2);
            eltType = TYP_INT;
            break;
        default:
            assert(varTypeIsByte(eltType));
            op1     = impVectorTMultiplyAddAdjacentByte(sig, op1, op2);
            eltType = TYP_INT;
            break;
    }

    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Sum, eltType, 16, op1);
    return gtNewSimdGetElementNode(TYP_SIMD16, sig.retType, op1, gtNewIconNode(0));
}

GenTree* Compiler::impVectorT256Dot(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 2);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);

    var_types eltType = sig.paramLayout[0]->GetElementType();

#ifndef TARGET_64BIT
    if (varTypeIsLong(eltType))
    {
        return nullptr;
    }
#endif

    GenTree* op1 = impSIMDPopStack(TYP_SIMD32);
    GenTree* op2 = impSIMDPopStack(TYP_SIMD32);

    if (eltType == TYP_FLOAT)
    {
        uint8_t imm = 0b11110001;
        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_DotProduct, TYP_FLOAT, 32, op1, op2, gtNewIconNode(imm));
        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector256_Sum, TYP_FLOAT, 32, op1);
        return gtNewSimdHWIntrinsicNode(eltType, NI_Vector128_GetElement, TYP_FLOAT, 16, op1, gtNewIconNode(0));
    }

    switch (eltType)
    {
        case TYP_DOUBLE:
            op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_Multiply, TYP_DOUBLE, 32, op1, op2);
            break;
        case TYP_LONG:
        case TYP_ULONG:
            op1     = impVectorTMultiplyLong(sig.paramLayout[0], op1, op2);
            eltType = TYP_LONG;
            break;
        case TYP_INT:
        case TYP_UINT:
            op1     = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_MultiplyLow, TYP_INT, 32, op1, op2);
            eltType = TYP_INT;
            break;
        case TYP_SHORT:
        case TYP_USHORT:
            op1     = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_MultiplyAddAdjacent, TYP_INT, 32, op1, op2);
            eltType = TYP_INT;
            break;
        default:
            assert(varTypeIsByte(eltType));
            op1     = impVectorTMultiplyAddAdjacentByte(sig, op1, op2);
            eltType = TYP_INT;
            break;
    }

    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector256_Sum, eltType, 32, op1);
    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Sum, eltType, 16, op1);
    return gtNewSimdGetElementNode(TYP_SIMD16, sig.retType, op1, gtNewIconNode(0));
}

GenTree* Compiler::impVectorTMultiplyAddAdjacentByte(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
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

    GenTree* lo  = gtNewSimdHWIntrinsicNode(type, madd, TYP_INT, size, op1Uses[0], op2Uses[0]);
    GenTree* hi1 = gtNewSimdHWIntrinsicNode(type, srlw, TYP_SHORT, size, op1Uses[1], gtNewIconNode(8));
    GenTree* hi2 = gtNewSimdHWIntrinsicNode(type, srlw, TYP_SHORT, size, op2Uses[1], gtNewIconNode(8));
    GenTree* hi  = gtNewSimdHWIntrinsicNode(type, madd, TYP_INT, size, hi1, hi2);
    return gtNewSimdHWIntrinsicNode(type, add, TYP_INT, size, lo, hi);
}

GenTree* Compiler::impVector234TEquals(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool notEqual)
{
    assert((sig.hasThisParam && (sig.paramCount == 1)) || (sig.paramCount == 2));
    assert(sig.hasThisParam || (sig.paramLayout[0] == sig.paramLayout[1]));
    assert(sig.retType == TYP_BOOL);

    ClassLayout* layout  = sig.paramLayout[0];
    var_types    type    = layout->GetSIMDType();
    var_types    eltType = layout->GetElementType();
    unsigned     size    = layout->GetSize();

    // Import integral vector equality as NI_Vector128_op_Equality & co. if we have PTEST.
    // It's too early to use PTEST here because op2 may not be a constant zero vector yet
    // and it's rather cumbersome to import to CompareEqual/MoveMask and pattern match in
    // lowering to change to PTEST.
    if (varTypeIsIntegral(eltType) && compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        NamedIntrinsic eq;

        if (type == TYP_SIMD32)
        {
            eq = notEqual ? NI_Vector256_op_Inequality : NI_Vector256_op_Equality;
        }
        else
        {
            eq = notEqual ? NI_Vector128_op_Inequality : NI_Vector128_op_Equality;
        }

        return gtNewSimdHWIntrinsicNode(sig.retType, eq, eltType, size, op1, op2);
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

    op1 = gtNewSimdHWIntrinsicNode(type, cmpeq, eltType, size, op1, op2);
    op1 = gtNewSimdHWIntrinsicNode(TYP_INT, movmsk, eltType, size, op1);

    if ((type == TYP_SIMD8) || (type == TYP_SIMD12))
    {
        op1 = gtNewOperNode(GT_AND, TYP_INT, op1, gtNewIconNode(mask));
    }

    return gtNewOperNode(notEqual ? GT_NE : GT_EQ, TYP_INT, op1, gtNewIconNode(mask));
}

GenTree* Compiler::impVectorT128MinMax(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool isMax)
{
    assert(sig.paramCount == 2);
    assert(sig.retType == TYP_SIMD16);
    assert((sig.retLayout == sig.paramLayout[0]) && (sig.retLayout == sig.paramLayout[1]));

    ClassLayout* layout  = sig.paramLayout[0];
    var_types    eltType = layout->GetElementType();

    if (((eltType == TYP_BYTE) || (eltType == TYP_USHORT) || (eltType == TYP_INT) || (eltType == TYP_UINT)) &&
        compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        return gtNewSimdHWIntrinsicNode(TYP_SIMD16, isMax ? NI_SSE41_Max : NI_SSE41_Min, eltType, 16, op1, op2);
    }

    if ((eltType == TYP_BYTE) || (eltType == TYP_USHORT))
    {
        GenTree*       constVal;
        NamedIntrinsic preIntrinsic;
        NamedIntrinsic intrinsic = isMax ? NI_SSE2_Max : NI_SSE2_Min;
        NamedIntrinsic postIntrinsic;

        if (eltType == TYP_BYTE)
        {
            constVal      = gtNewIconNode(0x80808080);
            preIntrinsic  = NI_SSE2_Subtract;
            postIntrinsic = NI_SSE2_Add;
            eltType       = TYP_UBYTE;
        }
        else
        {
            constVal      = gtNewIconNode(0x80008000);
            preIntrinsic  = NI_SSE2_Add;
            postIntrinsic = NI_SSE2_Subtract;
            eltType       = TYP_SHORT;
        }

        GenTree* constVector = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_INT, 16, constVal);
        GenTree* constUses[3];
        impMakeMultiUse(constVector, constUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.MinMax const temp"));

        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, preIntrinsic, eltType, 16, op1, constUses[0]);
        op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, preIntrinsic, eltType, 16, op2, constUses[1]);
        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, eltType, 16, op1, op2);
        return gtNewSimdHWIntrinsicNode(TYP_SIMD16, postIntrinsic, eltType, 16, op1, constUses[2]);
    }

    assert((eltType == TYP_INT) || (eltType == TYP_UINT) || varTypeIsLong(eltType));

    GenTree* uses[2][2];
    impMakeMultiUse(op1, uses[0], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.MinMax temp"));
    impMakeMultiUse(op2, uses[1], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.MinMax temp"));

    GenTree* mask;

    if (!varTypeIsLong(eltType) || compOpportunisticallyDependsOn(InstructionSet_SSE42))
    {
        NamedIntrinsic ni = !varTypeIsLong(eltType) ? NI_SSE2_CompareGreaterThan : NI_SSE42_CompareGreaterThan;

        if (varTypeIsUnsigned(eltType))
        {
            eltType = impVectorTUnsignedCompareAdjust(layout, eltType, &uses[0][0], &uses[1][0]);
        }

        mask = gtNewSimdHWIntrinsicNode(TYP_SIMD16, ni, eltType, 16, uses[0][0], uses[1][0]);
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

    if (!compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        GenTree* maskUses[2];
        impMakeMultiUse(mask, maskUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.MinMax mask temp"));
        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_And, eltType, 16, uses[1][1], maskUses[0]);
        op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_AndNot, eltType, 16, maskUses[1], uses[0][1]);
        return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Or, eltType, 16, op1, op2);
    }

    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE41_BlendVariable, TYP_UBYTE, 16, uses[0][1], uses[1][1], mask);
}

GenTree* Compiler::impVectorT256MinMax(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool isMax)
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

    GenTree* mask =
        gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_CompareGreaterThan, TYP_LONG, 32, uses[0][0], uses[1][0]);

    if (isMax)
    {
        std::swap(uses[0][1], uses[1][1]);
    }

    return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_BlendVariable, TYP_UBYTE, 32, uses[0][1], uses[1][1], mask);
}

GenTree* Compiler::impVectorT128Narrow(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.retType == TYP_SIMD16);
    assert(sig.paramCount == 2);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(varTypeSize(sig.retLayout->GetElementType()) == varTypeSize(sig.paramLayout[0]->GetElementType()) / 2);

    var_types eltType = varTypeToSigned(sig.paramLayout[0]->GetElementType());

    if (eltType == TYP_DOUBLE)
    {
        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ConvertToVector128Single, TYP_DOUBLE, 16, op1);
        op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ConvertToVector128Single, TYP_DOUBLE, 16, op2);

        return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_MoveLowToHigh, TYP_FLOAT, 16, op1, op2);
    }

    if (eltType == TYP_LONG)
    {
        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, op1, gtNewIconNode(128));
        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical128BitLane, TYP_LONG, 16, op1,
                                       gtNewIconNode(8));
        op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, op2, gtNewIconNode(8));
        op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical128BitLane, TYP_LONG, 16, op2,
                                       gtNewIconNode(8));

        return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Or, TYP_INT, 16, op1, op2);
    }

    assert((eltType == TYP_SHORT) || (eltType == TYP_INT));

    var_types retEltType = varTypeToSigned(sig.retLayout->GetElementType());
    ssize_t   retEltSize = varTypeSize(retEltType) * 8;

    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical, eltType, 16, op1, gtNewIconNode(retEltSize));
    op1 =
        gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightArithmetic, eltType, 16, op1, gtNewIconNode(retEltSize));
    op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical, eltType, 16, op2, gtNewIconNode(retEltSize));
    op2 =
        gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightArithmetic, eltType, 16, op2, gtNewIconNode(retEltSize));

    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_PackSignedSaturate, retEltType, 16, op1, op2);
}

GenTree* Compiler::impVectorT256Narrow(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.retType == TYP_SIMD32);
    assert(sig.paramCount == 2);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert(varTypeSize(sig.retLayout->GetElementType()) == varTypeSize(sig.paramLayout[0]->GetElementType()) / 2);

    var_types eltType = varTypeToSigned(sig.paramLayout[0]->GetElementType());

    if (eltType == TYP_DOUBLE)
    {
        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AVX_ConvertToVector128Single, TYP_FLOAT, 32, op1);
        op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AVX_ConvertToVector128Single, TYP_FLOAT, 32, op2);

        return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_InsertVector128, TYP_FLOAT, 32, op1, op2, gtNewIconNode(1));
    }

    GenTree* uses[2][2];
    impMakeMultiUse(op1, uses[0], sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Narrow temp"));
    impMakeMultiUse(op2, uses[1], sig.paramLayout[1], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Narrow temp"));

    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_Permute2x128, eltType, 32, uses[0][0], uses[1][0],
                                   gtNewIconNode(32));
    op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_Permute2x128, eltType, 32, uses[0][1], uses[1][1],
                                   gtNewIconNode(49));

    if (eltType == TYP_LONG)
    {
        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_Shuffle, TYP_INT, 32, op1, gtNewIconNode(8));
        op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_Shuffle, TYP_INT, 32, op2, gtNewIconNode(8));

        return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_UnpackLow, TYP_LONG, 32, op1, op2);
    }

    assert((eltType == TYP_SHORT) || (eltType == TYP_INT));

    var_types retEltType = varTypeToUnsigned(sig.retLayout->GetElementType());
    ssize_t   retEltSize = varTypeSize(retEltType) * 8;

    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftLeftLogical, eltType, 32, op1, gtNewIconNode(retEltSize));
    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, eltType, 32, op1, gtNewIconNode(retEltSize));
    op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftLeftLogical, eltType, 32, op2, gtNewIconNode(retEltSize));
    op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_ShiftRightLogical, eltType, 32, op2, gtNewIconNode(retEltSize));

    return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_PackUnsignedSaturate, retEltType, 32, op1, op2);
}

GenTree* Compiler::impVectorT128Widen(const HWIntrinsicSignature& sig)
{
    assert(sig.retType == TYP_VOID);
    assert(sig.paramCount == 3);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.paramType[1] == TYP_BYREF);
    assert(sig.paramType[2] == TYP_BYREF);

    var_types eltType = sig.paramLayout[0]->GetElementType();
    GenTree*  hiAddr  = impPopStack().val;
    GenTree*  loAddr  = impPopStack().val;
    GenTree*  value   = impSIMDPopStack(sig.paramType[0]);
    GenTree*  hi;
    GenTree*  lo;

    if (eltType == TYP_FLOAT)
    {
        GenTree* uses[3];
        impMakeMultiUse(value, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));

        lo = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ConvertToVector128Double, TYP_FLOAT, 16, uses[0]);
        hi = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_MoveHighToLow, TYP_FLOAT, 16, uses[1], uses[2]);
        hi = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ConvertToVector128Double, TYP_FLOAT, 16, hi);
    }
    else if (compOpportunisticallyDependsOn(InstructionSet_SSE41))
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
        uses[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical128BitLane, eltType, 16, uses[1],
                                           gtNewIconNode(8));

        lo = gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, eltType, 16, uses[0]);
        hi = gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, eltType, 16, uses[1]);
    }
    else
    {
        GenTree* sign[2]{gtNewZeroSimdHWIntrinsicNode(sig.paramLayout[0])};
        GenTree* uses[3];

        if (varTypeIsSigned(eltType))
        {
            impMakeMultiUse(value, 3, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));
            sign[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_CompareGreaterThan, eltType, 16, sign[0], uses[2]);
            impMakeMultiUse(sign[0], sign, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));
        }
        else
        {
            impMakeMultiUse(value, 2, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));
            sign[1] = gtNewZeroSimdHWIntrinsicNode(sig.paramLayout[0]);
        }

        lo = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_UnpackLow, eltType, 16, uses[0], sign[0]);
        hi = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_UnpackHigh, eltType, 16, uses[1], sign[1]);
    }

    impAppendTree(impAssignSIMDAddr(loAddr, lo), CHECK_SPILL_ALL, impCurStmtOffs);
    return impAssignSIMDAddr(hiAddr, hi);
}

GenTree* Compiler::impVectorT256Widen(const HWIntrinsicSignature& sig)
{
    assert(sig.retType == TYP_VOID);
    assert(sig.paramCount == 3);
    assert(sig.paramType[0] == TYP_SIMD32);
    assert(sig.paramType[1] == TYP_BYREF);
    assert(sig.paramType[2] == TYP_BYREF);

    var_types eltType = sig.paramLayout[0]->GetElementType();
    GenTree*  hiAddr  = impPopStack().val;
    GenTree*  loAddr  = impPopStack().val;
    GenTree*  value   = impSIMDPopStack(sig.paramType[0]);
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

    lo = gtNewSimdHWIntrinsicNode(TYP_SIMD32, widenIntrinsic, eltType, 32, uses[0]);
    hi = gtNewSimdHWIntrinsicNode(TYP_SIMD32, extractIntrinsic, eltType, 32, uses[1], gtNewIconNode(1));
    hi = gtNewSimdHWIntrinsicNode(TYP_SIMD32, widenIntrinsic, eltType, 32, hi);

    impAppendTree(impAssignSIMDAddr(loAddr, lo), CHECK_SPILL_ALL, impCurStmtOffs);
    return impAssignSIMDAddr(hiAddr, hi);
}

GenTree* Compiler::impVectorTMultiply(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 2);

    var_types vecType = sig.retType;
    var_types eltType = sig.retLayout->GetElementType();

    assert((vecType == TYP_SIMD16) || (vecType == TYP_SIMD32));

    GenTree* op1;
    GenTree* op2;

    if (sig.paramLayout[0] == nullptr)
    {
        assert(sig.paramType[0] == eltType);
        assert(sig.paramLayout[1] == sig.retLayout);

        op2 = impSIMDPopStack(sig.paramType[1]);
        op1 = impPopStack().val;

        op1 = gtNewSimdHWIntrinsicNode(vecType, GetCreateSimdHWIntrinsic(vecType), eltType, varTypeSize(vecType), op1);
    }
    else if (sig.paramLayout[1] == nullptr)
    {
        assert(sig.paramLayout[0] == sig.retLayout);
        assert(sig.paramType[1] == eltType);

        op2 = impPopStack().val;
        op1 = impSIMDPopStack(sig.paramType[0]);

        op2 = gtNewSimdHWIntrinsicNode(vecType, GetCreateSimdHWIntrinsic(vecType), eltType, varTypeSize(vecType), op2);
    }
    else
    {
        assert((sig.retLayout == sig.paramLayout[0]) && (sig.retLayout == sig.paramLayout[1]));

        op2 = impSIMDPopStack(sig.paramType[0]);
        op1 = impSIMDPopStack(sig.paramType[0]);
    }

    if (varTypeIsByte(eltType))
    {
        return impVectorTMultiplyByte(sig.retLayout, op1, op2);
    }

    if (varTypeIsLong(eltType))
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
            assert((eltType == TYP_INT) || (eltType == TYP_UINT));
            intrinsic = isAVX ? NI_AVX2_MultiplyLow : NI_SSE41_MultiplyLow;
            break;
    }

    if ((intrinsic != NI_SSE41_MultiplyLow) || compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        return gtNewSimdHWIntrinsicNode(vecType, intrinsic, eltType, varTypeSize(vecType), op1, op2);
    }

    GenTree* uses[2][2];
    impMakeMultiUse(op1, uses[0], sig.retLayout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Multiply temp"));
    impMakeMultiUse(op2, uses[1], sig.retLayout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Multiply temp"));

    GenTree* t = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical128BitLane, TYP_INT, 16, uses[0][0],
                                          gtNewIconNode(4));
    GenTree* u = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftRightLogical128BitLane, TYP_INT, 16, uses[1][0],
                                          gtNewIconNode(4));

    u = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Multiply, TYP_ULONG, 16, u, t);
    u = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, u, gtNewIconNode(SHUFFLE_XXZX));

    t = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Multiply, TYP_ULONG, 16, uses[0][1], uses[1][1]);
    t = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, t, gtNewIconNode(SHUFFLE_XXZX));

    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_UnpackLow, eltType, 16, t, u);
}

GenTree* Compiler::impVectorTMultiplyLong(ClassLayout* layout, GenTree* op1, GenTree* op2)
{
    assert(varTypeIsLong(layout->GetElementType()));

    GenTree* op1Uses[3];
    impMakeMultiUse(op1, op1Uses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<long>.Multiply temp"));
    GenTree* op2Uses[3];
    impMakeMultiUse(op2, op2Uses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<long>.Multiply temp"));

    var_types type = layout->GetSIMDType();
    unsigned  size = layout->GetSize();

    NamedIntrinsic mul = size == 32 ? NI_AVX2_Multiply : NI_SSE2_Multiply;
    NamedIntrinsic add = size == 32 ? NI_AVX2_Add : NI_SSE2_Add;
    NamedIntrinsic srl = size == 32 ? NI_AVX2_ShiftRightLogical : NI_SSE2_ShiftRightLogical;
    NamedIntrinsic sll = size == 32 ? NI_AVX2_ShiftLeftLogical : NI_SSE2_ShiftLeftLogical;

    GenTree* lo  = gtNewSimdHWIntrinsicNode(type, mul, TYP_ULONG, size, op1Uses[0], op2Uses[0]);
    GenTree* hi1 = gtNewSimdHWIntrinsicNode(type, srl, TYP_LONG, size, op1Uses[1], gtNewIconNode(32));
    hi1          = gtNewSimdHWIntrinsicNode(type, mul, TYP_ULONG, size, hi1, op2Uses[1]);
    GenTree* hi2 = gtNewSimdHWIntrinsicNode(type, srl, TYP_LONG, size, op2Uses[2], gtNewIconNode(32));
    hi2          = gtNewSimdHWIntrinsicNode(type, mul, TYP_ULONG, size, hi2, op1Uses[2]);
    GenTree* hi  = gtNewSimdHWIntrinsicNode(type, add, TYP_ULONG, size, hi1, hi2);
    hi           = gtNewSimdHWIntrinsicNode(type, sll, TYP_ULONG, size, hi, gtNewIconNode(32));
    return gtNewSimdHWIntrinsicNode(type, add, TYP_LONG, size, lo, hi);
}

GenTree* Compiler::impVectorTMultiplyByte(ClassLayout* layout, GenTree* op1, GenTree* op2)
{
    assert(varTypeIsByte(layout->GetElementType()));

    GenTree* op1Uses[2];
    impMakeMultiUse(op1, op1Uses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<byte>.Multiply temp"));
    GenTree* op2Uses[2];
    impMakeMultiUse(op2, op2Uses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<byte>.Multiply temp"));

    var_types type = layout->GetSIMDType();
    unsigned  size = layout->GetSize();

    NamedIntrinsic mul  = size == 32 ? NI_AVX2_MultiplyLow : NI_SSE2_MultiplyLow;
    NamedIntrinsic srlw = size == 32 ? NI_AVX2_ShiftRightLogical : NI_SSE2_ShiftRightLogical;
    NamedIntrinsic sllw = size == 32 ? NI_AVX2_ShiftLeftLogical : NI_SSE2_ShiftLeftLogical;
    NamedIntrinsic pand = size == 32 ? NI_AVX2_And : NI_SSE2_And;
    NamedIntrinsic por  = size == 32 ? NI_AVX2_Or : NI_SSE2_Or;

    GenTree* lo  = gtNewSimdHWIntrinsicNode(type, mul, TYP_SHORT, size, op1Uses[0], op2Uses[0]);
    GenTree* hi1 = gtNewSimdHWIntrinsicNode(type, srlw, TYP_SHORT, size, op1Uses[1], gtNewIconNode(8));
    GenTree* hi2 = gtNewSimdHWIntrinsicNode(type, srlw, TYP_SHORT, size, op2Uses[1], gtNewIconNode(8));
    GenTree* hi  = gtNewSimdHWIntrinsicNode(type, mul, TYP_SHORT, size, hi1, hi2);
    hi           = gtNewSimdHWIntrinsicNode(type, sllw, TYP_SHORT, size, hi, gtNewIconNode(8));
    GenTree* m   = gtNewSimdHWIntrinsicNode(type, GetCreateSimdHWIntrinsic(type), TYP_SHORT, size, gtNewIconNode(0xff));
    lo           = gtNewSimdHWIntrinsicNode(type, pand, TYP_SHORT, size, lo, m);
    return gtNewSimdHWIntrinsicNode(type, por, TYP_INT, size, lo, hi);
}

GenTree* Compiler::impVectorT128ConditionalSelect(const HWIntrinsicSignature& sig,
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

    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, sse ? NI_SSE_And : NI_SSE2_And, eltType, 16, op1, maskUses[0]);
    op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, sse ? NI_SSE_AndNot : NI_SSE2_AndNot, eltType, 16, maskUses[1], op2);
    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, sse ? NI_SSE_Or : NI_SSE2_Or, eltType, 16, op1, op2);
}

GenTree* Compiler::impVectorT256ConditionalSelect(const HWIntrinsicSignature& sig,
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

    op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD32, avx ? NI_AVX_And : NI_AVX2_And, eltType, 32, op1, maskUses[0]);
    op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD32, avx ? NI_AVX_AndNot : NI_AVX2_AndNot, eltType, 32, maskUses[1], op2);
    return gtNewSimdHWIntrinsicNode(TYP_SIMD32, avx ? NI_AVX_Or : NI_AVX2_Or, eltType, 32, op1, op2);
}

var_types Compiler::impVectorTUnsignedCompareAdjust(ClassLayout* layout,
                                                    var_types    eltType,
                                                    GenTree**    op1,
                                                    GenTree**    op2)
{
    GenTree* constVal = nullptr;

    switch (eltType)
    {
        case TYP_UBYTE:
            constVal = gtNewIconNode(0x80808080);
            eltType  = TYP_BYTE;
            break;
        case TYP_USHORT:
            constVal = gtNewIconNode(0x80008000);
            eltType  = TYP_SHORT;
            break;
        case TYP_UINT:
            constVal = gtNewIconNode(0x80000000);
            eltType  = TYP_INT;
            break;
        case TYP_ULONG:
            constVal = gtNewLconNode(0x8000000000000000);
            eltType  = TYP_LONG;
            break;
        default:
            unreached();
    }

    var_types      type   = layout->GetSIMDType();
    unsigned       size   = layout->GetSize();
    NamedIntrinsic create = GetCreateSimdHWIntrinsic(type);
    // We don't have carry so SUB(x, INT_MIN) is the same as XOR(x, INT_MIN).
    // On Ryzen XOR has slightly higher throuput.
    NamedIntrinsic pxor = (type == TYP_SIMD32) ? NI_AVX2_Xor : NI_SSE2_Xor;

    GenTree* constVector = gtNewSimdHWIntrinsicNode(type, create, constVal->GetType(), size, constVal);
    GenTree* constUses[2];
    impMakeMultiUse(constVector, constUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThan const temp"));
    *op1 = gtNewSimdHWIntrinsicNode(type, pxor, eltType, size, *op1, constUses[0]);
    *op2 = gtNewSimdHWIntrinsicNode(type, pxor, eltType, size, *op2, constUses[1]);

    return eltType;
}

GenTree* Compiler::impVectorT128LongGreaterThanSse2(ClassLayout* layout, GenTree* op1, GenTree* op2, bool lessThan)
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

    GenTree* sign = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_LONG, 16, gtNewLconNode(1LL << 31));
    GenTree* signUses[2];
    impMakeMultiUse(sign, signUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThan const temp"));

    uses[0][1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Xor, TYP_LONG, 16, uses[0][1], signUses[0]);
    uses[1][1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Xor, TYP_LONG, 16, uses[1][1], signUses[1]);

    GenTree* gt = gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, TYP_INT, 16, uses[0][1], uses[1][1]);
    GenTree* gtUses[2];
    impMakeMultiUse(gt, gtUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThan temp"));

    GenTree* im = gtNewIconNode(SHUFFLE_ZZXX);
    GenTree* gl = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, gtUses[0], im);
    GenTree* eq = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_CompareEqual, TYP_INT, 16, uses[0][0], uses[1][0]);

    gt = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_And, TYP_INT, 16, eq, gl);
    gt = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Or, TYP_INT, 16, gtUses[1], gt);
    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, gt, gtNewIconNode(SHUFFLE_WWYY));
}

GenTree* Compiler::impVectorT128ULongGreaterThanSse2(ClassLayout* layout, GenTree* op1, GenTree* op2, bool lessThan)
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
    GenTree* gt = gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, TYP_INT, 16, uses[0][1], uses[1][1]);
    GenTree* gtUses[2];
    impMakeMultiUse(gt, gtUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThan temp"));

    GenTree* im = gtNewIconNode(SHUFFLE_ZZXX);
    GenTree* gl = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, gtUses[0], im);
    GenTree* eq = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_CompareEqual, TYP_INT, 16, uses[0][0], uses[1][0]);

    gt = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_And, TYP_INT, 16, eq, gl);
    gt = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Or, TYP_INT, 16, gtUses[1], gt);
    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, gt, gtNewIconNode(SHUFFLE_WWYY));
}

GenTree* Compiler::impVectorT128LongEquals(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.paramCount == 2);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.paramType[0] == TYP_SIMD16);
    assert(sig.retType == TYP_SIMD16);

    ClassLayout* layout  = sig.paramLayout[0];
    var_types    eltType = layout->GetElementType();
    assert(varTypeIsLong(eltType));

    if (compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE41_CompareEqual, eltType, 16, op1, op2);
    }

    GenTree* eq = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_CompareEqual, TYP_INT, 16, op1, op2);
    GenTree* eqUses[2];
    impMakeMultiUse(eq, eqUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Equals temp"));

    GenTree* shuffleEq =
        gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, eqUses[0], gtNewIconNode(SHUFFLE_ZWXY));

    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_And, eltType, 16, shuffleEq, eqUses[1]);
}

GenTree* Compiler::impVectorT128Compare(const HWIntrinsicSignature& sig,
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

    if (!varTypeIsLong(eltType) || compOpportunisticallyDependsOn(InstructionSet_SSE42))
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

        gt = gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, eltType, 16, op1, op2);
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

    GenTree* allBitsSet = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_get_AllBitsSet, eltType, 16);
    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Xor, eltType, 16, gt, allBitsSet);
}

GenTree* Compiler::impVectorT256Compare(const HWIntrinsicSignature& sig,
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

    GenTree* gt = gtNewSimdHWIntrinsicNode(TYP_SIMD32, intrinsic, eltType, 32, op1, op2);

    if (!orEqual)
    {
        return gt;
    }

    GenTree* allBitsSet = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector256_get_AllBitsSet, eltType, 32);
    return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_Xor, eltType, 32, gt, allBitsSet);
}
#endif // TARGET_XARCH

// Check whether two memory locations are contiguous.
//
// This recognizes trivial patterns such as FIELD(o, 4) & FIELD(o, 8) or INDEX(a, 1) & INDEX(a, 2).
// Pointer arithmetic isn't recognized (and probably not very useful anyway) and in the case of
// arrays only constant indices are recognized. Might be useful to also recognize i, i+1, i+2...
// If the locations are determined to be adjacent this also implies that the trees are also free
// of persistent side effects and they can be discarded. They may have exception side effects that
// may need to be preserved - a[1] doesn't imply that a[2] is also a valid array element.
//
bool Compiler::SIMDCoalescingBuffer::AreContiguousMemoryLocations(GenTree* l1, GenTree* l2)
{
    if ((l1->GetOper() != l2->GetOper()) || l1->TypeIs(TYP_STRUCT))
    {
        return false;
    }

    auto AreValuesEqual = [](GenTree* v1, GenTree* v2) {
        while (v1->GetOper() == v2->GetOper())
        {
            if (v1->OperIs(GT_ADDR))
            {
                v1 = v1->AsUnOp()->GetOp(0);
                v2 = v2->AsUnOp()->GetOp(0);

                continue;
            }

            if (v1->OperIs(GT_FIELD))
            {
                if ((v1->AsField()->GetFieldHandle() == v2->AsField()->GetFieldHandle()) &&
                    !v1->AsField()->IsVolatile() && !v2->AsField()->IsVolatile())
                {
                    v1 = v1->AsField()->GetAddr();
                    v2 = v2->AsField()->GetAddr();

                    continue;
                }

                return false;
            }

            if (v1->OperIs(GT_LCL_VAR))
            {
                return v1->AsLclVar()->GetLclNum() == v2->AsLclVar()->GetLclNum();
            }

            break;
        }

        return false;
    };

    auto AreConsecutiveConstants = [](GenTree* i1, GenTree* i2) {
        return i1->OperIs(GT_CNS_INT) && i2->OperIs(GT_CNS_INT) &&
               (i1->AsIntCon()->GetValue() + 1 == i2->AsIntCon()->GetValue());
    };

    auto AreContiguosArrayElements = [&](GenTreeIndex* e1, GenTreeIndex* e2) {
        return AreConsecutiveConstants(e1->GetIndex(), e2->GetIndex()) &&
               AreValuesEqual(e1->GetArray(), e2->GetArray());
    };

    auto AreContiguosFields = [&](GenTreeField* f1, GenTreeField* f2) {
        return (f1->GetOffset() + varTypeSize(f1->GetType()) == f2->GetOffset()) && !f1->IsVolatile() &&
               !f2->IsVolatile() && AreValuesEqual(f1->GetAddr(), f2->GetAddr());
    };

    auto AreContiguosLocalFields = [](GenTreeLclFld* f1, GenTreeLclFld* f2) {
        return (f1->GetLclNum() == f2->GetLclNum()) &&
               (f1->GetLclOffs() + varTypeSize(f1->GetType()) == f2->GetLclOffs());
    };

    switch (l1->GetOper())
    {
        case GT_INDEX:
            return AreContiguosArrayElements(l1->AsIndex(), l2->AsIndex());
        case GT_FIELD:
            return AreContiguosFields(l1->AsField(), l2->AsField());
        case GT_LCL_FLD:
            return AreContiguosLocalFields(l1->AsLclFld(), l2->AsLclFld());
        default:
            return false;
    }
}

// Change a FIELD/INDEX/LCL_FLD node into a SIMD typed IND/LCL_FLD.
//
void Compiler::SIMDCoalescingBuffer::ChangeToSIMDMem(Compiler* compiler, GenTree* tree, var_types simdType)
{
    assert(tree->TypeIs(TYP_FLOAT));

    if (tree->OperIs(GT_LCL_FLD))
    {
        tree->SetType(simdType);
        tree->AsLclFld()->SetFieldSeq(FieldSeqStore::NotAField());

        // This may have changed a partial local field into full local field
        if (tree->IsPartialLclFld(compiler))
        {
            tree->gtFlags |= GTF_VAR_USEASG;
        }
        else
        {
            tree->gtFlags &= ~GTF_VAR_USEASG;
        }

        return;
    }

    GenTree* addr   = nullptr;
    unsigned offset = 0;

    if (GenTreeField* field = tree->IsField())
    {
        assert(!tree->AsField()->IsVolatile());

        addr   = field->GetAddr();
        offset = field->GetOffset();

        if (addr->OperIs(GT_ADDR))
        {
            GenTree* location = addr->AsUnOp()->GetOp(0);

            // If this is the field of a local struct variable then set lvUsedInSIMDIntrinsic to prevent
            // the local from being promoted. If it gets promoted then it will be dependent-promoted due
            // to the indirection we're creating.

            // TODO-MIKE-Cleanup: This is done only for SIMD locals but it really should be done for any
            // struct local since the whole point is to block poor promotion.

            if (varTypeIsSIMD(location->GetType()) && location->OperIs(GT_LCL_VAR))
            {
                compiler->lvaRecordSimdIntrinsicUse(location->AsLclVar());
            }
        }

        // TODO-MIKE-Fix: This code replaces FIELD with and ADD(addr, offset) without adding
        // a NULLCHECK when the field offset is large enough to require it. It's not worth
        // fixing this until FIELD is replaced by FIELD_ADDR, otherwise we need to add ADDR
        // on top of the existing FIELD and then use that as the address of the indir.
    }
    else if (GenTreeIndex* element = tree->IsIndex())
    {
        GenTree* array = element->GetArray();
        unsigned index = static_cast<unsigned>(element->GetIndex()->AsIntCon()->GetValue());

        // Generate a bounds check for the array access. We access multiple array elements but for
        // bounds checking purposes it's sufficient to check if the last element index is valid,
        // then all the element indices before it will also be valid.

        unsigned simdElementCount = varTypeSize(simdType) / varTypeSize(TYP_FLOAT);

        GenTree* lastIndex = compiler->gtNewIconNode(index + simdElementCount - 1, TYP_INT);
        GenTree* arrLen =
            compiler->gtNewArrLen(compiler->gtCloneExpr(array), OFFSETOF__CORINFO_Array__length, compiler->compCurBB);
        GenTree* arrBndsChk = compiler->gtNewArrBoundsChk(lastIndex, arrLen, SCK_RNGCHK_FAIL);

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

    tree->ChangeOper(GT_IND);
    tree->SetType(simdType);
    tree->AsIndir()->SetAddr(addr);
}

// Recognize a FIELD of a SIMD local variable (Vector2/3/4 fields).
//
unsigned Compiler::SIMDCoalescingBuffer::IsSimdLocalField(GenTree* node)
{
    // We only care about Vector2/3/4 so the element type is always FLOAT.
    assert(node->TypeIs(TYP_FLOAT));

    if (!node->OperIs(GT_FIELD))
    {
        return BAD_VAR_NUM;
    }

    if (node->AsField()->IsVolatile())
    {
        // It probably doesn't make sense to coalesce volatile fields. Anyway LocalAddressVisitor
        // doesn't generate SIMDIntrinsicGetItem out of a volatile field and ChangeToSIMDMem does
        // not bother to make the indir it creates volatile...

        return BAD_VAR_NUM;
    }

    if (node->AsField()->GetOffset() != m_index * varTypeSize(TYP_FLOAT))
    {
        return BAD_VAR_NUM;
    }

    GenTree* addr = node->AsField()->GetAddr();

    if ((addr == nullptr) || !addr->OperIs(GT_ADDR) || !addr->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR))
    {
        return BAD_VAR_NUM;
    }

    GenTreeLclVar* lclVar = addr->AsUnOp()->GetOp(0)->AsLclVar();

    if (!varTypeIsSIMD(lclVar->GetType()))
    {
        return BAD_VAR_NUM;
    }

    return lclVar->GetLclNum();
}

// Recognize a NI_Vector128_GetElement or LCL_FLD that uses a SIMD local variable.
//
unsigned Compiler::SIMDCoalescingBuffer::IsSimdLocalExtract(GenTree* node)
{
    // We only care about Vector2/3/4 so the element type is always FLOAT.
    assert(node->TypeIs(TYP_FLOAT));

    if (GenTreeHWIntrinsic* extract = node->IsHWIntrinsic())
    {
        if ((extract->GetIntrinsic() != NI_Vector128_GetElement) || !extract->GetOp(0)->OperIs(GT_LCL_VAR) ||
            !extract->GetOp(1)->IsIntegralConst(m_index))
        {
            return BAD_VAR_NUM;
        }

        return extract->GetOp(0)->AsLclVar()->GetLclNum();
    }

    if (GenTreeLclFld* lclFld = node->IsLclFld())
    {
        if (lclFld->GetLclOffs() != m_index * varTypeSize(TYP_FLOAT))
        {
            return BAD_VAR_NUM;
        }

        return lclFld->GetLclNum();
    }

    return BAD_VAR_NUM;
};

// Try to add an assignment statement to the coalescing buffer (common code for Add and Mark).
// Return true if the statment is added and the number of statements in the buffer equals the number of SIMD elements.
//
bool Compiler::SIMDCoalescingBuffer::Add(Compiler* compiler, Statement* stmt, GenTreeOp* asg, unsigned simdLclNum)
{
    if (simdLclNum == BAD_VAR_NUM)
    {
        Clear();
        return false;
    }

    if (m_index == 0)
    {
        m_firstStmt = stmt;
        m_lastStmt  = stmt;
        m_lclNum    = simdLclNum;
        m_index++;
        return false;
    }

    if (simdLclNum != m_lclNum)
    {
        Clear();
        return false;
    }

    GenTreeOp* lastAsg = m_lastStmt->GetRootNode()->AsOp();

    if (!AreContiguousMemoryLocations(lastAsg->GetOp(0), asg->AsOp()->GetOp(0)))
    {
        Clear();
        return false;
    }

    m_lastStmt = stmt;
    m_index++;

    return (m_index == varTypeSize(compiler->lvaGetDesc(simdLclNum)->GetType()) / varTypeSize(TYP_FLOAT));
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
// global morph it's more difficult to recognize contiguous memory locations because INDEX gets
// expanded into more complex trees. But then the coalescing code only recognizes constant array
// indices and COMMAs aren't present in LIR so probably there's not much difference.
//
void Compiler::SIMDCoalescingBuffer::Mark(Compiler* compiler, Statement* stmt)
{
    GenTree* asg = stmt->GetRootNode();

    if (!asg->TypeIs(TYP_FLOAT) || !asg->OperIs(GT_ASG))
    {
        Clear();
        return;
    }

    unsigned simdLclNum = IsSimdLocalField(asg->AsOp()->GetOp(1));

    if (!Add(compiler, stmt, asg->AsOp(), simdLclNum))
    {
        return;
    }

    compiler->lvaRecordSimdIntrinsicUse(simdLclNum);

    GenTree* dest = asg->AsOp()->GetOp(0);

    if (GenTreeField* field = dest->IsField())
    {
        GenTree* addr = field->GetAddr();

        if ((addr != nullptr) && addr->OperIs(GT_ADDR) && addr->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR))
        {
            compiler->lvaRecordSimdIntrinsicUse(addr->AsUnOp()->GetOp(0)->AsLclVar());
        }
    }

    Clear();
}

// Try to add an assignment statement to the coalescing buffer.
// Return true if the statment is added and the number of statements in the buffer equals the number of SIMD elements.
//
bool Compiler::SIMDCoalescingBuffer::Add(Compiler* compiler, Statement* stmt)
{
    GenTree* asg = stmt->GetRootNode();

    if (!asg->TypeIs(TYP_FLOAT) || !asg->OperIs(GT_ASG))
    {
        Clear();
        return false;
    }

    unsigned simdLclNum = IsSimdLocalExtract(asg->AsOp()->GetOp(1));

    return Add(compiler, stmt, asg->AsOp(), simdLclNum);
}

// Transform the first assignment in the buffer into a SIMD assignment
// and remove the rest of the statements from the block.
//
void Compiler::SIMDCoalescingBuffer::Coalesce(Compiler* compiler, BasicBlock* block)
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
        printf("\nFound %u contiguous assignments from a %s local to memory in " FMT_BB ":\n", m_index,
               varTypeName(type), block->bbNum);
        for (Statement* s = m_firstStmt; s != m_lastStmt->GetNextStmt(); s = s->GetNextStmt())
        {
            compiler->gtDispStmt(s);
        }
    }
#endif

    for (unsigned i = 1; i < m_index; i++)
    {
        compiler->fgRemoveStmt(block, m_firstStmt->GetNextStmt());
    }

    GenTreeOp* asg = m_firstStmt->GetRootNode()->AsOp();
    asg->SetType(type);
    ChangeToSIMDMem(compiler, asg->GetOp(0), type);
    asg->SetOp(1, compiler->gtNewLclvNode(m_lclNum, compiler->lvaGetDesc(m_lclNum)->GetType()));

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("Changed to a single %s assignment:\n", varTypeName(type));
        compiler->gtDispStmt(m_firstStmt);
        printf("\n");
    }
#endif

    Clear();
}

#endif // FEATURE_HW_INTRINSICS
