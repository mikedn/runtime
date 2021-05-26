// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "simdashwintrinsic.h"

#ifdef FEATURE_HW_INTRINSICS

static const SimdAsHWIntrinsicInfo simdAsHWIntrinsicInfoArray[] = {
// clang-format off
#if defined(TARGET_XARCH)
#define SIMD_AS_HWINTRINSIC(classId, id, name, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, flag)                      \
    {NI_##classId##_##id, name, SimdAsHWIntrinsicClassId::classId, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, static_cast<SimdAsHWIntrinsicFlag>(flag)},
#include "simdashwintrinsiclistxarch.h"
#elif defined(TARGET_ARM64)
#define SIMD_AS_HWINTRINSIC(classId, id, name, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, flag)                      \
    {NI_##classId##_##id, name, SimdAsHWIntrinsicClassId::classId, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, static_cast<SimdAsHWIntrinsicFlag>(flag)},
#include "simdashwintrinsiclistarm64.h"
#else
#error Unsupported platform
#endif
    // clang-format on
};

//------------------------------------------------------------------------
// lookup: Gets the SimdAsHWIntrinsicInfo associated with a given NamedIntrinsic
//
// Arguments:
//    id -- The NamedIntrinsic associated with the SimdAsHWIntrinsic to lookup
//
// Return Value:
//    The SimdAsHWIntrinsicInfo associated with id
const SimdAsHWIntrinsicInfo& SimdAsHWIntrinsicInfo::lookup(NamedIntrinsic id)
{
    assert(id != NI_Illegal);

    assert(id > NI_SIMD_AS_HWINTRINSIC_START);
    assert(id < NI_SIMD_AS_HWINTRINSIC_END);

    return simdAsHWIntrinsicInfoArray[id - NI_SIMD_AS_HWINTRINSIC_START - 1];
}

//------------------------------------------------------------------------
// lookupId: Gets the NamedIntrinsic for a given method name and InstructionSet
//
// Arguments:
//    className          -- The name of the class associated with the SimdIntrinsic to lookup
//    methodName         -- The name of the method associated with the SimdIntrinsic to lookup
//    enclosingClassName -- The name of the enclosing class
//    sizeOfVectorT      -- The size of Vector<T> in bytes
//
// Return Value:
//    The NamedIntrinsic associated with methodName and classId
NamedIntrinsic SimdAsHWIntrinsicInfo::lookupId(CORINFO_SIG_INFO* sig,
                                               const char*       className,
                                               const char*       methodName,
                                               const char*       enclosingClassName,
                                               int               sizeOfVectorT)
{
    SimdAsHWIntrinsicClassId classId = lookupClassId(className, enclosingClassName, sizeOfVectorT);

    if (classId == SimdAsHWIntrinsicClassId::Unknown)
    {
        return NI_Illegal;
    }

    bool     isInstanceMethod = sig->hasThis();
    unsigned numArgs          = sig->numArgs + (isInstanceMethod ? 1 : 0);

    for (int i = 0; i < (NI_SIMD_AS_HWINTRINSIC_END - NI_SIMD_AS_HWINTRINSIC_START - 1); i++)
    {
        const SimdAsHWIntrinsicInfo& intrinsicInfo = simdAsHWIntrinsicInfoArray[i];

        if (classId != intrinsicInfo.classId)
        {
            continue;
        }

        if (numArgs != static_cast<unsigned>(intrinsicInfo.numArgs))
        {
            continue;
        }

        if (isInstanceMethod != SimdAsHWIntrinsicInfo::IsInstanceMethod(intrinsicInfo.id))
        {
            continue;
        }

        if (strcmp(methodName, intrinsicInfo.name) != 0)
        {
            continue;
        }

        return intrinsicInfo.id;
    }

    return NI_Illegal;
}

//------------------------------------------------------------------------
// lookupClassId: Gets the SimdAsHWIntrinsicClassId for a given class name and enclsoing class name
//
// Arguments:
//    className          -- The name of the class associated with the SimdAsHWIntrinsicClassId to lookup
//    enclosingClassName -- The name of the enclosing class
//    sizeOfVectorT      -- The size of Vector<T> in bytes
//
// Return Value:
//    The SimdAsHWIntrinsicClassId associated with className and enclosingClassName
SimdAsHWIntrinsicClassId SimdAsHWIntrinsicInfo::lookupClassId(const char* className,
                                                              const char* enclosingClassName,
                                                              int         sizeOfVectorT)
{
    assert(className != nullptr);

    if ((enclosingClassName != nullptr) || (className[0] != 'V'))
    {
        return SimdAsHWIntrinsicClassId::Unknown;
    }
    if (strcmp(className, "Vector2") == 0)
    {
        return SimdAsHWIntrinsicClassId::Vector2;
    }
    if (strcmp(className, "Vector3") == 0)
    {
        return SimdAsHWIntrinsicClassId::Vector3;
    }
    if (strcmp(className, "Vector4") == 0)
    {
        return SimdAsHWIntrinsicClassId::Vector4;
    }
    if ((strcmp(className, "Vector") == 0) || (strcmp(className, "Vector`1") == 0))
    {
#if defined(TARGET_XARCH)
        if (sizeOfVectorT == 32)
        {
            return SimdAsHWIntrinsicClassId::VectorT256;
        }
#endif // TARGET_XARCH

        assert(sizeOfVectorT == 16);
        return SimdAsHWIntrinsicClassId::VectorT128;
    }

    return SimdAsHWIntrinsicClassId::Unknown;
}

#ifdef TARGET_XARCH
NamedIntrinsic MapVectorTIntrinsic(NamedIntrinsic intrinsic, bool isAVX)
{
    if (!isAVX)
    {
        assert((NI_VectorT128_Abs <= intrinsic) && (intrinsic < NI_VectorT256_Abs));
        return intrinsic;
    }

    switch (intrinsic)
    {
        case NI_VectorT128_AndNot:
            return NI_VectorT256_AndNot;
        case NI_VectorT128_Equals:
            return NI_VectorT256_Equals;
        case NI_VectorT128_LessThan:
            return NI_VectorT256_LessThan;
        case NI_VectorT128_GreaterThan:
            return NI_VectorT256_GreaterThan;
        case NI_VectorT128_op_BitwiseAnd:
            return NI_VectorT256_op_BitwiseAnd;
        case NI_VectorT128_op_BitwiseOr:
            return NI_VectorT256_op_BitwiseOr;
        default:
            unreached();
    }
}
#endif // TARGET_XARCH

//------------------------------------------------------------------------
// impSimdAsIntrinsic: Import a SIMD intrinsic as a GT_HWINTRINSIC node if possible
//
// Arguments:
//    intrinsic  -- id of the intrinsic function.
//    clsHnd     -- class handle containing the intrinsic function.
//    method     -- method handle of the intrinsic function.
//    sig        -- signature of the intrinsic call
//    mustExpand -- true if the intrinsic must return a GenTree*; otherwise, false
//
// Return Value:
//    The GT_HWINTRINSIC node, or nullptr if not a supported intrinsic
//
GenTree* Compiler::impSimdAsHWIntrinsic(NamedIntrinsic        intrinsic,
                                        CORINFO_CLASS_HANDLE  clsHnd,
                                        CORINFO_METHOD_HANDLE method,
                                        CORINFO_SIG_INFO*     sig,
                                        GenTree*              newobjThis)
{
    if (!featureSIMD)
    {
        return nullptr;
    }

#if defined(TARGET_XARCH)
    CORINFO_InstructionSet minimumIsa = InstructionSet_SSE2;
#elif defined(TARGET_ARM64)
    CORINFO_InstructionSet minimumIsa = InstructionSet_AdvSimd;
#else
#error Unsupported platform
#endif

    if (!compOpportunisticallyDependsOn(minimumIsa) || !JitConfig.EnableHWIntrinsic())
    {
        // The user disabled support for the baseline ISA so
        // don't emit any SIMD intrinsics as they all require
        // this at a minimum
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
        // IsHardwareAccelerated is imported by the old SIMD intrinsic code.
        assert(signature.paramCount != 0);

        layout = signature.paramLayout[0];

        if (!layout->IsVector() || ((signature.retLayout != nullptr) && !signature.retLayout->IsVector()))
        {
            // Ignore generic instantiations that use invalid element types.
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

        if (!layout->IsVector())
        {
            // Ignore generic instantiations that use invalid element types.
            return nullptr;
        }

        if (signature.retLayout != nullptr)
        {
            // For member intrinsic methods the return vector type (if any, the return
            // may also be void or a primitive type) should be the same as the instance
            // vector type, except for op_Explicit operators where the base is different.
            // op_Explicit are all no-op so we don't really care about the base type.

            assert(signature.retLayout->IsVector());
            assert(signature.retLayout->GetSIMDType() == layout->GetSIMDType());
            assert(signature.retLayout->GetSize() == layout->GetSize());
#ifdef TARGET_XARCH
            assert((signature.retLayout->GetElementType() == layout->GetElementType()) ||
                   (intrinsic == NI_VectorT128_op_Explicit) || (intrinsic == NI_VectorT256_op_Explicit));
#else
            assert((signature.retLayout->GetElementType() == layout->GetElementType()) ||
                   (intrinsic == NI_VectorT128_op_Explicit));
#endif
        }
        else if (signature.paramCount != 0)
        {
            if (signature.paramLayout[0] == nullptr)
            {
                if (signature.paramType[0] == TYP_REF)
                {
                    // Create from array intrinsics are handled by the old SIMD import code.
                    return nullptr;
                }

                // This must be one of the Create intrinsics or get_Item.
                assert(signature.hasThisParam);
                assert((signature.paramCount == 1) || (signature.paramCount == layout->GetElementCount()));
                assert((signature.paramType[0] == layout->GetElementType()) || (signature.paramType[0] == TYP_INT));
            }
            else
            {
                // If the first parameter has struct type then it is expected to be the same
                // as the instance type, with the the exception of Vector3/4 constructors that
                // have overloads with different vector types.

                if (signature.paramLayout[0] != layout)
                {
                    assert(layout->GetVectorKind() == VectorKind::Vector234);
                    assert(signature.paramLayout[0]->GetVectorKind() == VectorKind::Vector234);
                    assert(layout->GetElementCount() - signature.paramLayout[0]->GetElementCount() ==
                           signature.paramCount - 1);
                }
            }
        }
    }

    NamedIntrinsic hwIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, layout->GetElementType());

    if (hwIntrinsic == NI_Illegal)
    {
        return nullptr;
    }

    if (SimdAsHWIntrinsicInfo::IsFloatingPointUsed(intrinsic))
    {
        compFloatingPointUsed = true;
    }

    if (hwIntrinsic == intrinsic)
    {
        switch (intrinsic)
        {
            case NI_Vector2_CreateBroadcast:
            case NI_Vector2_Create:
            case NI_Vector3_CreateBroadcast:
            case NI_Vector3_Create:
            case NI_Vector4_CreateBroadcast:
            case NI_Vector4_Create:
            case NI_VectorT128_CreateBroadcast:
#ifdef TARGET_XARCH
            case NI_VectorT256_CreateBroadcast:
#endif
                return impSimdAsHWIntrinsicCreate(signature, layout, newobjThis);
            case NI_Vector3_CreateExtend1:
            case NI_Vector4_CreateExtend1:
            case NI_Vector4_CreateExtend2:
                return impSimdAsHWIntrinsicCreateExtend(signature, layout, newobjThis);
            case NI_VectorT128_get_Item:
#ifdef TARGET_XARCH
            case NI_VectorT256_get_Item:
#endif
                assert(newobjThis == nullptr);
                return impSimdAsHWIntrinsicGetItem(signature, layout);
            case NI_VectorT128_Widen:
                return impSimdAsHWIntrinsicWiden128(signature);
#ifdef TARGET_XARCH
            case NI_VectorT256_Widen:
                return impSimdAsHWIntrinsicWiden256(signature);
#endif
            default:
                assert(newobjThis == nullptr);
                return impSimdAsHWIntrinsicSpecial(intrinsic, signature, layout);
        }
    }

    if (!compOpportunisticallyDependsOn(HWIntrinsicInfo::lookupIsa(hwIntrinsic)))
    {
        return nullptr;
    }

    var_types baseType = layout->GetElementType();
    unsigned  size     = layout->GetSize();

    if (signature.hasThisParam)
    {
        assert(SimdAsHWIntrinsicInfo::IsInstanceMethod(intrinsic));

        switch (intrinsic)
        {
            GenTree* ops[2];

            case NI_Vector2_EqualsInstance:
            case NI_Vector3_EqualsInstance:
            case NI_Vector4_EqualsInstance:
            case NI_VectorT128_EqualsInstance:
#ifdef TARGET_XARCH
            case NI_VectorT256_EqualsInstance:
#endif
                assert(signature.paramCount == 1);

                ops[1] = impPopArgForHWIntrinsic(signature.paramType[0], signature.paramLayout[0]);
                ops[0] = impSIMDPopStackAddr(signature.paramType[0]);
                return gtNewSimdHWIntrinsicNode(signature.retType, hwIntrinsic, baseType, size, ops[0], ops[1]);

            default:
                return nullptr;
        }
    }

    switch (signature.paramCount)
    {
        GenTree* ops[2];

        case 0:
            return gtNewSimdHWIntrinsicNode(signature.retType, hwIntrinsic, baseType, size);

        case 1:
            ops[0] = impPopArgForHWIntrinsic(signature.paramType[0], signature.paramLayout[0]);
            return gtNewSimdHWIntrinsicNode(signature.retType, hwIntrinsic, baseType, size, ops[0]);

        case 2:
            ops[1] = impPopArgForHWIntrinsic(signature.paramType[1], signature.paramLayout[1]);
            ops[0] = impPopArgForHWIntrinsic(signature.paramType[0], signature.paramLayout[0]);

            if (SimdAsHWIntrinsicInfo::NeedsOperandsSwapped(intrinsic))
            {
                // TODO-MIKE-Fix: This is nonsense, it changes the order of evaluation.
                std::swap(ops[0], ops[1]);
            }

            return gtNewSimdHWIntrinsicNode(signature.retType, hwIntrinsic, baseType, size, ops[0], ops[1]);

        default:
            assert(!"Unexpected SimdAsHWIntrinsic");
            return nullptr;
    }
}

GenTree* Compiler::impSimdAsHWIntrinsicSpecial(NamedIntrinsic              intrinsic,
                                               const HWIntrinsicSignature& sig,
                                               ClassLayout*                layout)
{
    assert(featureSIMD);
    assert(!SimdAsHWIntrinsicInfo::NeedsOperandsSwapped(intrinsic));
    assert(!sig.hasThisParam);
    assert(sig.paramCount <= 3);
#if defined(TARGET_XARCH)
    bool isAVX = (SimdAsHWIntrinsicInfo::lookupClassId(intrinsic) == SimdAsHWIntrinsicClassId::VectorT256);
    assert(compIsaSupportedDebugOnly(InstructionSet_SSE2));
    assert(!isAVX || compIsaSupportedDebugOnly(InstructionSet_AVX2));
#elif defined(TARGET_ARM64)
    assert(compIsaSupportedDebugOnly(InstructionSet_AdvSimd));
#else
#error Unsupported platform
#endif

#ifdef TARGET_XARCH
    if ((intrinsic == NI_VectorT128_Dot) && !compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        // We need to exit early if this is Vector<T>.Dot for INT or UINT and SSE41 is
        // not supported. The other types should be handled via the table driven paths.

        var_types opType = sig.paramLayout[0]->GetElementType();
        assert((opType == TYP_INT) || (opType == TYP_UINT));
        return nullptr;
    }
#endif

    var_types    retType   = sig.retType;
    ClassLayout* retLayout = sig.retLayout;
    GenTree*     ops[3];

    for (unsigned i = sig.paramCount; i != 0; i--)
    {
        ops[i - 1] = impPopArgForHWIntrinsic(sig.paramType[i - 1], sig.paramLayout[i - 1]);
    }

    switch (intrinsic)
    {
        case NI_Vector2_get_One:
        case NI_Vector3_get_One:
        case NI_Vector4_get_One:
        case NI_VectorT128_get_One:
#ifdef TARGET_XARCH
        case NI_VectorT256_get_One:
#endif
        {
            assert(sig.paramCount == 0);
            var_types retBaseType = retLayout->GetElementType();
            unsigned  retSize     = retLayout->GetSize();
            return gtNewSimdHWIntrinsicNode(retType, GetCreateSimdHWIntrinsic(retType), retBaseType, retSize,
                                            gtNewOneConNode(retBaseType));
        }

        case NI_VectorT128_get_Count:
#ifdef TARGET_XARCH
        case NI_VectorT256_get_Count:
#endif
        {
            assert(sig.paramCount == 0);
            GenTreeIntCon* countNode = gtNewIconNode(layout->GetElementCount(), TYP_INT);
            countNode->gtFlags |= GTF_ICON_SIMD_COUNT;
            return countNode;
        }

        case NI_VectorT128_op_Explicit:
        case NI_VectorT128_As:
#ifdef TARGET_XARCH
        case NI_VectorT256_op_Explicit:
        case NI_VectorT256_As:
#endif
        {
            assert(sig.paramCount == 1);
            SetOpLclRelatedToSIMDIntrinsic(ops[0]);
            assert(ops[0]->GetType() == retLayout->GetSIMDType());
            return ops[0];
        }

#ifdef TARGET_XARCH
        case NI_Vector2_Abs:
        case NI_Vector3_Abs:
        case NI_Vector4_Abs:
        case NI_VectorT128_Abs:
        case NI_VectorT256_Abs:
        {
            assert(sig.paramCount == 1);
            assert(retLayout == sig.paramLayout[0]);

            var_types retBaseType = retLayout->GetElementType();
            unsigned  retSize     = retLayout->GetSize();

            if (varTypeIsFloating(retBaseType))
            {
                // Abs(vf) = vf & new SIMDVector<float>(0x7fffffff);
                // Abs(vd) = vf & new SIMDVector<double>(0x7fffffffffffffff);
                GenTree* bitMask;

                if (retBaseType == TYP_FLOAT)
                {
                    bitMask = gtNewDconNode(jitstd::bit_cast<float, int32_t>(0x7fffffff), TYP_FLOAT);
                }
                else
                {
                    assert(retBaseType == TYP_DOUBLE);
                    bitMask = gtNewDconNode(jitstd::bit_cast<double, int64_t>(0x7fffffffffffffffLL), TYP_DOUBLE);
                }

                bitMask =
                    gtNewSimdHWIntrinsicNode(retType, GetCreateSimdHWIntrinsic(retType), retBaseType, retSize, bitMask);

                intrinsic = MapVectorTIntrinsic(NI_VectorT128_op_BitwiseAnd, isAVX);
                intrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, retBaseType);

                return gtNewSimdHWIntrinsicNode(retType, intrinsic, retBaseType, retSize, ops[0], bitMask);
            }

            if (varTypeIsUnsigned(retBaseType))
            {
                return ops[0];
            }

            if ((retBaseType != TYP_LONG) && compOpportunisticallyDependsOn(InstructionSet_SSSE3))
            {
                return gtNewSimdHWIntrinsicNode(retType, NI_SSSE3_Abs, retBaseType, retSize, ops[0]);
            }

            GenTree* uses[2];
            impMakeMultiUse(ops[0], uses, retLayout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Abs temp"));

            GenTree* sign;

            if ((retBaseType == TYP_SHORT) || (retBaseType == TYP_INT) ||
                ((retBaseType == TYP_LONG) && !compOpportunisticallyDependsOn(InstructionSet_SSE42)))
            {
                NamedIntrinsic sraIntrinsic = isAVX ? NI_AVX2_ShiftRightArithmetic : NI_SSE2_ShiftRightArithmetic;

                sign = gtNewSimdHWIntrinsicNode(retType, sraIntrinsic, retBaseType == TYP_LONG ? TYP_INT : retBaseType,
                                                retSize, uses[0], gtNewIconNode(31));

                if (retBaseType == TYP_LONG)
                {
                    NamedIntrinsic shufdIntrinsic = isAVX ? NI_AVX2_Shuffle : NI_SSE2_Shuffle;

                    sign = gtNewSimdHWIntrinsicNode(retType, shufdIntrinsic, TYP_INT, retSize, sign,
                                                    gtNewIconNode(0b11110101));
                }
            }
            else
            {
                NamedIntrinsic lessIntrinsic = MapVectorTIntrinsic(NI_VectorT128_LessThan, isAVX);

                sign = gtNewZeroSimdHWIntrinsicNode(retLayout);
                sign = impSimdAsHWIntrinsicRelOp(lessIntrinsic, retBaseType, retLayout, uses[0], sign);
            }

            GenTree* signUses[2];
            impMakeMultiUse(sign, signUses, retLayout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Abs sign temp"));

            NamedIntrinsic xorIntrinsic = isAVX ? NI_AVX2_Xor : NI_SSE2_Xor;
            NamedIntrinsic subIntrinsic = isAVX ? NI_AVX2_Subtract : NI_SSE2_Subtract;

            GenTree* tmp = gtNewSimdHWIntrinsicNode(retType, xorIntrinsic, retBaseType, retSize, signUses[0], uses[1]);
            return gtNewSimdHWIntrinsicNode(retType, subIntrinsic, retBaseType, retSize, tmp, signUses[1]);
        }

        case NI_Vector2_op_Division:
        case NI_Vector3_op_Division:
        {
            assert(sig.paramCount == 2);
            assert((retLayout == sig.paramLayout[0]) && (retLayout == sig.paramLayout[1]));
            assert((retLayout->GetSize() == 8) || (retLayout->GetSize() == 12));
            assert(retLayout->GetElementType() == TYP_FLOAT);

            unsigned retSize = retLayout->GetSize();
            GenTree* retNode = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_Divide, TYP_FLOAT, retSize, ops[0], ops[1]);

            // Vector2/3 div: since the top-most elements will be zero, we end up
            // perfoming 0/0 which is a NAN. Therefore, post division we need to set the
            // top-most elements to zero. This is achieved by left logical shift followed
            // by right logical shift of the result.
            retNode = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical128BitLane, TYP_INT, retSize,
                                               retNode, gtNewIconNode(16 - retSize));
            retNode = gtNewSimdHWIntrinsicNode(retType, NI_SSE2_ShiftRightLogical128BitLane, TYP_INT, retSize, retNode,
                                               gtNewIconNode(16 - retSize));
            return retNode;
        }

        case NI_VectorT128_Dot:
        {
            assert(sig.paramCount == 2);
            assert(sig.paramLayout[0] == sig.paramLayout[1]);

            var_types opBaseType = sig.paramLayout[0]->GetElementType();
            unsigned  opSize     = sig.paramLayout[0]->GetSize();
            assert((opBaseType == TYP_INT) || (opBaseType == TYP_UINT));
            assert(compIsaSupportedDebugOnly(InstructionSet_SSE41));
            return gtNewSimdHWIntrinsicNode(retType, NI_Vector128_Dot, opBaseType, opSize, ops[0], ops[1]);
        }

        case NI_VectorT256_GreaterThan:
        case NI_VectorT256_LessThan:
            assert(varTypeIsUnsigned(sig.paramLayout[0]->GetElementType()));
            FALLTHROUGH;
        case NI_VectorT256_GreaterThanOrEqual:
        case NI_VectorT256_LessThanOrEqual:
        case NI_VectorT128_Equals:
        case NI_VectorT128_GreaterThan:
        case NI_VectorT128_GreaterThanOrEqual:
        case NI_VectorT128_LessThan:
        case NI_VectorT128_LessThanOrEqual:
        {
            assert(sig.paramCount == 2);
            assert(sig.paramLayout[0] == sig.paramLayout[1]);

            ClassLayout* opLayout = sig.paramLayout[0];

            // Return base type may be different from the param base type (e.g. int/float)
            // but otherwise the return and param types must be identical.
            assert(retType == opLayout->GetSIMDType());
            assert(retLayout->GetSize() == opLayout->GetSize());

            return impSimdAsHWIntrinsicRelOp(intrinsic, opLayout->GetElementType(), opLayout, ops[0], ops[1]);
        }

        case NI_VectorT256_Max:
        case NI_VectorT256_Min:
            assert(varTypeIsLong(retLayout->GetElementType()));
            FALLTHROUGH;
        case NI_VectorT128_Max:
        case NI_VectorT128_Min:
        {
            assert(sig.paramCount == 2);
            assert((retLayout == sig.paramLayout[0]) && (retLayout == sig.paramLayout[1]));
            assert(!varTypeIsFloating(retLayout->GetElementType()));

            var_types retBaseType = retLayout->GetElementType();
            unsigned  retSize     = retLayout->GetSize();

            if ((retBaseType == TYP_BYTE) || (retBaseType == TYP_USHORT))
            {
                intrinsic = (intrinsic == NI_VectorT128_Max) ? NI_SSE2_Max : NI_SSE2_Min;

                GenTree*       constVal;
                NamedIntrinsic preIntrinsic;
                NamedIntrinsic postIntrinsic;

                if (retBaseType == TYP_BYTE)
                {
                    constVal      = gtNewIconNode(0x80808080);
                    preIntrinsic  = NI_SSE2_Subtract;
                    postIntrinsic = NI_SSE2_Add;
                    retBaseType   = TYP_UBYTE;
                }
                else
                {
                    constVal      = gtNewIconNode(0x80008000);
                    preIntrinsic  = NI_SSE2_Add;
                    postIntrinsic = NI_SSE2_Subtract;
                    retBaseType   = TYP_SHORT;
                }

                GenTree* constVector = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, TYP_INT, 16, constVal);
                GenTree* constUses[3];
                impMakeMultiUse(constVector, constUses, retLayout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Max/Min temp"));

                ops[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, preIntrinsic, retBaseType, 16, ops[0], constUses[0]);
                ops[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, preIntrinsic, retBaseType, 16, ops[1], constUses[1]);
                ops[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, retBaseType, 16, ops[0], ops[1]);
                return gtNewSimdHWIntrinsicNode(TYP_SIMD16, postIntrinsic, retBaseType, 16, ops[0], constUses[2]);
            }

            GenTree* uses[2][2];
            impMakeMultiUse(ops[0], uses[0], retLayout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Max/Min temp"));
            impMakeMultiUse(ops[1], uses[1], retLayout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Max/Min temp"));

            switch (intrinsic)
            {
                case NI_VectorT128_Max:
                    intrinsic = NI_VectorT128_GreaterThan;
                    break;
                case NI_VectorT256_Max:
                    intrinsic = NI_VectorT256_GreaterThan;
                    break;
                case NI_VectorT128_Min:
                    intrinsic = NI_VectorT128_LessThan;
                    break;
                default:
                    intrinsic = NI_VectorT256_LessThan;
                    break;
            }

            GenTree* condition = impSimdAsHWIntrinsicRelOp(intrinsic, retBaseType, retLayout, uses[0][0], uses[1][0]);
            return impSimdAsHWIntrinsicCndSel(retLayout, condition, uses[0][1], uses[1][1]);
        }

        case NI_VectorT128_Narrow:
            return impSimdAsHWIntrinsicNarrow128(sig, ops[0], ops[1]);
        case NI_VectorT256_Narrow:
            return impSimdAsHWIntrinsicNarrow256(sig, ops[0], ops[1]);

        case NI_VectorT128_op_Multiply:
        case NI_VectorT256_op_Multiply:
            return impSimdAsHWIntrinsicMultiply(sig, ops[0], ops[1]);

        case NI_VectorT128_ConditionalSelect:
        case NI_VectorT256_ConditionalSelect:
            assert(sig.paramCount == 3);
            assert(sig.paramLayout[1] == sig.paramLayout[2]);
            assert(sig.paramLayout[0]->GetSIMDType() == sig.paramLayout[1]->GetSIMDType());
            assert(retLayout == sig.paramLayout[1]);

            return impSimdAsHWIntrinsicCndSel(retLayout, ops[0], ops[1], ops[2]);
#endif // TARGET_XARCH

#ifdef TARGET_ARM64
        case NI_VectorT128_Abs:
            assert(sig.paramCount == 1);
            assert(varTypeIsUnsigned(retLayout->GetElementType()));
            return ops[0];

        case NI_VectorT128_Max:
        case NI_VectorT128_Min:
        {
            assert(sig.paramCount == 2);
            assert((retLayout == sig.paramLayout[0]) && (retLayout == sig.paramLayout[1]));
            assert(retLayout->GetSIMDType() == TYP_SIMD16);
            assert(varTypeIsLong(retLayout->GetElementType()));

            var_types retBaseType = retLayout->GetElementType();

            intrinsic = (intrinsic == NI_VectorT128_Max) ? NI_AdvSimd_Arm64_CompareGreaterThan
                                                         : NI_AdvSimd_Arm64_CompareLessThan;

            GenTree* uses[2][2];
            impMakeMultiUse(ops[0], uses[0], retLayout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Max/Min temp"));
            impMakeMultiUse(ops[1], uses[1], retLayout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Max/Min temp"));

            GenTree* condition =
                gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, retBaseType, 16, uses[0][0], uses[1][0]);
            return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AdvSimd_BitwiseSelect, retBaseType, 16, condition,
                                            uses[0][1], uses[1][1]);
        }

        case NI_VectorT128_Narrow:
            return impSimdAsHWIntrinsicNarrow128(sig, ops[0], ops[1]);

        case NI_VectorT128_op_Multiply:
            return impSimdAsHWIntrinsicMultiply(sig, ops[0], ops[1]);

        case NI_VectorT128_ConditionalSelect:
            assert(sig.paramCount == 3);
            assert(sig.paramLayout[1] == sig.paramLayout[2]);
            assert(sig.paramLayout[0]->GetSIMDType() == sig.paramLayout[1]->GetSIMDType());
            assert(retLayout == sig.paramLayout[1]);
            assert(retLayout->GetSIMDType() == TYP_SIMD16);

            return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AdvSimd_BitwiseSelect, retLayout->GetElementType(), 16,
                                            ops[0], ops[1], ops[2]);
#endif // TARGET_ARM64

        default:
            assert(!"Unexpected SimdAsHWIntrinsic");
            return nullptr;
    }
}

GenTree* Compiler::impSimdAsHWIntrinsicCreate(const HWIntrinsicSignature& sig, ClassLayout* layout, GenTree* newobjThis)
{
    assert(sig.retType == TYP_VOID);
    assert(sig.hasThisParam);
    assert(layout->IsVector());
    assert((sig.paramCount == 1) || (sig.paramCount == layout->GetElementCount()));

    GenTree* args[4];
    assert(sig.paramCount <= _countof(args));
    bool areArgsContiguous = sig.paramCount > 1;

    for (unsigned i = 0; i < sig.paramCount; i++)
    {
        unsigned argIndex = sig.paramCount - i - 1;
        args[argIndex]    = impPopStackCoerceArg(varActualType(sig.paramType[argIndex]));

        if ((i > 0) && areArgsContiguous)
        {
            // We're popping the args off the stack in reverse order so we already have the next arg.
            areArgsContiguous = SIMDCoalescingBuffer::AreContiguousMemoryLocations(args[argIndex], args[argIndex + 1]);
        }
    }

    GenTree* addr = impSimdAsHWIntrinsicGetCtorThis(layout, newobjThis);
    GenTree* create;

    if (areArgsContiguous)
    {
        SIMDCoalescingBuffer::ChangeToSIMDMem(this, args[0], layout->GetSIMDType());

        create = args[0];

        if (addr->OperIs(GT_ADDR) && addr->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR))
        {
            // Prevent the destination from being promoted since it would end up being dependent promoted.
            setLclRelatedToSIMDIntrinsic(addr->AsUnOp()->GetOp(0));
        }
    }
    else if ((sig.paramCount == 1) && (args[0]->IsIntegralConst(0) || args[0]->IsDblConPositiveZero()))
    {
        create = gtNewZeroSimdHWIntrinsicNode(layout);
    }
    else
    {
        create = gtNewSimdHWIntrinsicNode(layout->GetSIMDType(), GetCreateSimdHWIntrinsic(layout->GetSIMDType()),
                                          layout->GetElementType(), layout->GetSize(), sig.paramCount, args);
    }

    return impAssignSIMDAddr(addr, create);
}

GenTree* Compiler::impSimdAsHWIntrinsicCreateExtend(const HWIntrinsicSignature& sig,
                                                    ClassLayout*                layout,
                                                    GenTree*                    newobjThis)
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

    GenTree* addr = impSimdAsHWIntrinsicGetCtorThis(layout, newobjThis);
    GenTree* create;

    unsigned insertIndex = sig.paramType[0] == TYP_SIMD12 ? 3 : 2;

#ifdef TARGET_ARM64
    create = args[0];

    for (unsigned i = 1; i < sig.paramCount; i++)
    {
        args[i] = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, TYP_FLOAT, 16, args[i]);
        create  = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AdvSimd_Insert, TYP_FLOAT, 16, create,
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
    return impAssignSIMDAddr(addr, create);
}

GenTree* Compiler::impSimdAsHWIntrinsicGetCtorThis(ClassLayout* layout, GenTree* newobjThis)
{
    if (newobjThis != nullptr)
    {
        assert(newobjThis->OperIs(GT_ADDR) && newobjThis->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR));
        unsigned  lclNum  = newobjThis->AsUnOp()->GetOp(0)->AsLclVar()->GetLclNum();
        var_types lclType = lvaGetDesc(lclNum)->GetType();
        impPushOnStack(gtNewLclvNode(lclNum, lclType), typeInfo(TI_STRUCT, layout->GetClassHandle()));
        return newobjThis;
    }

    return impPopStack().val;
}

GenTree* Compiler::impSimdAsHWIntrinsicGetItem(const HWIntrinsicSignature& sig, ClassLayout* layout)
{
    assert(sig.paramCount == 1);
    assert(sig.paramType[0] == TYP_INT);

    var_types elementType = layout->GetElementType();

    switch (elementType)
    {
        case TYP_BYTE:
        case TYP_UBYTE:
        case TYP_INT:
        case TYP_UINT:
        case TYP_LONG:
        case TYP_ULONG:
#ifdef TARGET_XARCH
            if (!compExactlyDependsOn(InstructionSet_SSE41))
            {
                return nullptr;
            }
#endif
            break;

        case TYP_DOUBLE:
        case TYP_FLOAT:
        case TYP_SHORT:
        case TYP_USHORT:
            break;

        default:
            unreached();
    }

    GenTree* index = impPopStack().val;
    GenTree* value = impSIMDPopStackAddr(layout->GetSIMDType());

    assert(value->GetType() == layout->GetSIMDType());
    assert(varActualType(index->GetType()) == TYP_INT);

    return gtNewSimdGetElementNode(elementType, layout->GetSize(), value, index);
}

#ifdef TARGET_ARMARCH

GenTree* Compiler::impSimdAsHWIntrinsicNarrow128(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.retType == TYP_SIMD16);
    assert(sig.paramCount == 2);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.retLayout->GetSIMDType() == sig.paramLayout[0]->GetSIMDType());
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

GenTree* Compiler::impSimdAsHWIntrinsicWiden128(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 3);
    assert(sig.paramLayout[0]->GetSIMDType() == TYP_SIMD16);
    assert(sig.paramType[1] == TYP_BYREF);
    assert(sig.paramType[2] == TYP_BYREF);

    GenTree* hiAddr = impPopStack().val;
    GenTree* loAddr = impPopStack().val;
    GenTree* value  = impPopArgForHWIntrinsic(sig.paramType[0], sig.paramLayout[0]);

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

GenTree* Compiler::impSimdAsHWIntrinsicMultiply(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.paramCount == 2);

    var_types vecType = sig.retType;
    var_types eltType = sig.retLayout->GetElementType();

    assert(vecType == TYP_SIMD16);

    NamedIntrinsic intrinsic = eltType == TYP_DOUBLE ? NI_AdvSimd_Arm64_Multiply : NI_AdvSimd_Multiply;

    if (sig.paramLayout[0] == nullptr)
    {
        assert(sig.paramType[0] == eltType);
        assert(sig.paramLayout[1] == sig.retLayout);

        op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_Create, eltType, 16, op1);
    }
    else if (sig.paramLayout[1] == nullptr)
    {
        assert(sig.paramLayout[0] == sig.retLayout);
        assert(sig.paramType[1] == eltType);

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
    }

    return gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, eltType, 16, op1, op2);
}

#endif // TARGET_ARMARCH

#ifdef TARGET_XARCH

GenTree* Compiler::impSimdAsHWIntrinsicNarrow128(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.retType == TYP_SIMD16);
    assert(sig.paramCount == 2);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.retLayout->GetSIMDType() == sig.paramLayout[0]->GetSIMDType());
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

GenTree* Compiler::impSimdAsHWIntrinsicNarrow256(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.retType == TYP_SIMD32);
    assert(sig.paramCount == 2);
    assert(sig.paramLayout[0] == sig.paramLayout[1]);
    assert(sig.retLayout->GetSIMDType() == sig.paramLayout[0]->GetSIMDType());
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

GenTree* Compiler::impSimdAsHWIntrinsicWiden128(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 3);
    assert(sig.paramLayout[0]->GetSIMDType() == TYP_SIMD16);
    assert(sig.paramType[1] == TYP_BYREF);
    assert(sig.paramType[2] == TYP_BYREF);

    GenTree* hiAddr = impPopStack().val;
    GenTree* loAddr = impPopStack().val;
    GenTree* value  = impPopArgForHWIntrinsic(sig.paramType[0], sig.paramLayout[0]);
    GenTree* hi;
    GenTree* lo;

    if (sig.paramLayout[0]->GetElementType() == TYP_FLOAT)
    {
        GenTree* uses[3];
        impMakeMultiUse(value, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));

        lo = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ConvertToVector128Double, TYP_FLOAT, 16, uses[0]);
        hi = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE_MoveHighToLow, TYP_FLOAT, 16, uses[1], uses[2]);
        hi = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ConvertToVector128Double, TYP_FLOAT, 16, hi);
    }
    else
    {
        var_types eltType = sig.paramLayout[0]->GetElementType();
        GenTree*  sign[2]{gtNewZeroSimdHWIntrinsicNode(sig.paramLayout[0])};
        GenTree*  uses[3];

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

GenTree* Compiler::impSimdAsHWIntrinsicWiden256(const HWIntrinsicSignature& sig)
{
    assert(sig.paramCount == 3);
    assert(sig.paramLayout[0]->GetSIMDType() == TYP_SIMD32);
    assert(sig.paramType[1] == TYP_BYREF);
    assert(sig.paramType[2] == TYP_BYREF);

    GenTree* hiAddr = impPopStack().val;
    GenTree* loAddr = impPopStack().val;
    GenTree* value  = impPopArgForHWIntrinsic(sig.paramType[0], sig.paramLayout[0]);
    GenTree* hi;
    GenTree* lo;

    GenTree* uses[2];
    impMakeMultiUse(value, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));

    if (sig.paramLayout[0]->GetElementType() == TYP_FLOAT)
    {
        lo = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_ConvertToVector256Double, TYP_FLOAT, 32, uses[0]);
        hi = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_ExtractVector128, TYP_FLOAT, 32, uses[1], gtNewIconNode(1));
        hi = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX_ConvertToVector256Double, TYP_FLOAT, 32, hi);
    }
    else
    {
        lo =
            gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_Permute4x64, TYP_LONG, 32, uses[0], gtNewIconNode(0b11010100));
        hi =
            gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_Permute4x64, TYP_LONG, 32, uses[1], gtNewIconNode(0b11101000));

        var_types eltType = sig.paramLayout[0]->GetElementType();
        GenTree*  sign[2]{gtNewZeroSimdHWIntrinsicNode(sig.paramLayout[0]),
                         gtNewZeroSimdHWIntrinsicNode(sig.paramLayout[0])};

        if (varTypeIsSigned(eltType))
        {
            impMakeMultiUse(lo, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));
            sign[0] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_CompareGreaterThan, eltType, 32, sign[0], uses[0]);
            lo      = uses[1];

            impMakeMultiUse(hi, uses, sig.paramLayout[0], CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));
            sign[1] = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_CompareGreaterThan, eltType, 32, sign[1], uses[0]);
            hi      = uses[1];
        }

        lo = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_UnpackLow, eltType, 32, lo, sign[0]);
        hi = gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_UnpackHigh, eltType, 32, hi, sign[1]);
    }

    impAppendTree(impAssignSIMDAddr(loAddr, lo), CHECK_SPILL_ALL, impCurStmtOffs);
    return impAssignSIMDAddr(hiAddr, hi);
}

GenTree* Compiler::impSimdAsHWIntrinsicMultiply(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2)
{
    assert(sig.paramCount == 2);

    var_types vecType = sig.retType;
    var_types eltType = sig.retLayout->GetElementType();

    assert((vecType == TYP_SIMD16) || (vecType == TYP_SIMD32));

    if (sig.paramLayout[0] == nullptr)
    {
        assert(sig.paramType[0] == eltType);
        assert(sig.paramLayout[1] == sig.retLayout);

        op1 = gtNewSimdHWIntrinsicNode(vecType, GetCreateSimdHWIntrinsic(vecType), eltType, varTypeSize(vecType), op1);
    }
    else if (sig.paramLayout[1] == nullptr)
    {
        assert(sig.paramLayout[0] == sig.retLayout);
        assert(sig.paramType[1] == eltType);

        op2 = gtNewSimdHWIntrinsicNode(vecType, GetCreateSimdHWIntrinsic(vecType), eltType, varTypeSize(vecType), op2);
    }
    else
    {
        assert((sig.retLayout == sig.paramLayout[0]) && (sig.retLayout == sig.paramLayout[1]));
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

GenTree* Compiler::impSimdAsHWIntrinsicCndSel(ClassLayout* layout, GenTree* op1, GenTree* op2, GenTree* op3)
{
    var_types type     = layout->GetSIMDType();
    var_types baseType = layout->GetElementType();
    unsigned  size     = layout->GetSize();

    NamedIntrinsic andIntrinsic  = MapVectorTIntrinsic(NI_VectorT128_op_BitwiseAnd, type == TYP_SIMD32);
    NamedIntrinsic andnIntrinsic = MapVectorTIntrinsic(NI_VectorT128_AndNot, type == TYP_SIMD32);
    NamedIntrinsic orIntrinsic   = MapVectorTIntrinsic(NI_VectorT128_op_BitwiseOr, type == TYP_SIMD32);

    assert(SimdAsHWIntrinsicInfo::NeedsOperandsSwapped(andnIntrinsic));

    andIntrinsic  = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(andIntrinsic, baseType);
    andnIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(andnIntrinsic, baseType);
    orIntrinsic   = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(orIntrinsic, baseType);

    GenTree* uses[2];
    impMakeMultiUse(op1, uses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.ConditionalSelect temp"));
    op2 = gtNewSimdHWIntrinsicNode(type, andIntrinsic, baseType, size, op2, uses[0]);
    op3 = gtNewSimdHWIntrinsicNode(type, andnIntrinsic, baseType, size, uses[1], op3);
    return gtNewSimdHWIntrinsicNode(type, orIntrinsic, baseType, size, op2, op3);
}

GenTree* Compiler::impSimdAsHWIntrinsicRelOp(
    NamedIntrinsic intrinsic, var_types baseType, ClassLayout* layout, GenTree* op1, GenTree* op2)
{
    var_types type  = layout->GetSIMDType();
    unsigned  size  = layout->GetSize();
    bool      isAVX = type == TYP_SIMD32;

    switch (intrinsic)
    {
        case NI_VectorT128_Equals:
        case NI_VectorT256_Equals:
        {
            // These ones aren't "special", but they are used by the other
            // relational operators and so are defined for convenience.

            if (isAVX || ((baseType != TYP_LONG) && (baseType != TYP_ULONG)))
            {
                NamedIntrinsic hwIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, baseType);
                assert(hwIntrinsic != intrinsic);
                return gtNewSimdHWIntrinsicNode(type, hwIntrinsic, baseType, size, op1, op2);
            }

            if (compOpportunisticallyDependsOn(InstructionSet_SSE41))
            {
                return gtNewSimdHWIntrinsicNode(type, NI_SSE41_CompareEqual, baseType, size, op1, op2);
            }

            // There is no direct SSE2 support for comparing LONG vectors.
            // Compare as INT vectors and combine the lo/hi INT result parts
            // using PSHUFD and AND:
            //     PAND(PCMPEQD(op1, op2), PSHUFD(PCMPEQD(op1, op2), (2, 3, 0, 1)))

            GenTree* eq = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_CompareEqual, TYP_INT, 16, op1, op2);
            GenTree* eqUses[2];
            impMakeMultiUse(eq, eqUses, layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Equals temp"));

            GenTree* shuffleEq = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, eqUses[0],
                                                          gtNewIconNode(SHUFFLE_ZWXY));
            return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_And, baseType, 16, shuffleEq, eqUses[1]);
        }

        case NI_VectorT128_GreaterThanOrEqual:
        case NI_VectorT128_LessThanOrEqual:
        case NI_VectorT256_GreaterThanOrEqual:
        case NI_VectorT256_LessThanOrEqual:
        {
            // There is no direct support for doing a combined comparison and equality for integral types.
            // These have to be implemented by performing both halves and combining their results.
            //
            // op1 = Greater/LessThan(op1, op2)
            // op2 = Equals(op1Dup, op2Dup)
            // result = POR(op1, op2)

            NamedIntrinsic eqIntrinsic;
            NamedIntrinsic orIntrinsic;

            switch (intrinsic)
            {
                case NI_VectorT128_GreaterThanOrEqual:
                    intrinsic   = NI_VectorT128_GreaterThan;
                    eqIntrinsic = NI_VectorT128_Equals;
                    orIntrinsic = NI_SSE2_Or;
                    break;
                case NI_VectorT128_LessThanOrEqual:
                    intrinsic   = NI_VectorT128_LessThan;
                    eqIntrinsic = NI_VectorT128_Equals;
                    orIntrinsic = NI_SSE2_Or;
                    break;
                case NI_VectorT256_GreaterThanOrEqual:
                    intrinsic   = NI_VectorT256_GreaterThan;
                    eqIntrinsic = NI_VectorT256_Equals;
                    orIntrinsic = NI_AVX2_Or;
                    break;
                case NI_VectorT256_LessThanOrEqual:
                    intrinsic   = NI_VectorT256_LessThan;
                    eqIntrinsic = NI_VectorT256_Equals;
                    orIntrinsic = NI_AVX2_Or;
                    break;
                default:
                    unreached();
            }

            GenTree* uses[2][2];
            impMakeMultiUse(op1, uses[0], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThanOrEqual temp"));
            impMakeMultiUse(op2, uses[1], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThanOrEqual temp"));

            GenTree* eq   = impSimdAsHWIntrinsicRelOp(eqIntrinsic, baseType, layout, uses[0][0], uses[1][0]);
            GenTree* less = impSimdAsHWIntrinsicRelOp(intrinsic, baseType, layout, uses[0][1], uses[1][1]);
            return gtNewSimdHWIntrinsicNode(type, orIntrinsic, baseType, size, eq, less);
        }

        case NI_VectorT128_GreaterThan:
        case NI_VectorT128_LessThan:
        case NI_VectorT256_GreaterThan:
        case NI_VectorT256_LessThan:
        {
            if (varTypeIsUnsigned(baseType))
            {
                // Vector<byte>, Vector<ushort>, Vector<uint> and Vector<ulong>:
                // Hardware supports > for signed comparison. Therefore, to use it for
                // comparing unsigned numbers, we subtract a constant from both the
                // operands such that the result fits within the corresponding signed
                // type. The resulting signed numbers are compared using signed comparison.
                //
                // We need to treat op1 and op2 as signed for comparison purpose after
                // the transformation.

                GenTree* constVal = nullptr;

                switch (baseType)
                {
                    case TYP_UBYTE:
                        constVal = gtNewIconNode(0x80808080, TYP_INT);
                        baseType = TYP_BYTE;
                        break;
                    case TYP_USHORT:
                        constVal = gtNewIconNode(0x80008000, TYP_INT);
                        baseType = TYP_SHORT;
                        break;
                    case TYP_UINT:
                        constVal = gtNewIconNode(0x80000000, TYP_INT);
                        baseType = TYP_INT;
                        break;
                    case TYP_ULONG:
                        constVal = gtNewLconNode(0x8000000000000000);
                        baseType = TYP_LONG;
                        break;
                    default:
                        unreached();
                }

                GenTree* constVector =
                    gtNewSimdHWIntrinsicNode(type, GetCreateSimdHWIntrinsic(type), constVal->GetType(), size, constVal);

                GenTree* constUses[2];
                impMakeMultiUse(constVector, constUses, layout,
                                CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThan const temp"));

                NamedIntrinsic subIntrinsic = isAVX ? NI_AVX2_Subtract : NI_SSE2_Subtract;
                op1 = gtNewSimdHWIntrinsicNode(type, subIntrinsic, baseType, size, op1, constUses[0]);
                op2 = gtNewSimdHWIntrinsicNode(type, subIntrinsic, baseType, size, op2, constUses[1]);
            }

            if (isAVX || (baseType != TYP_LONG))
            {
                NamedIntrinsic hwIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, baseType);
                assert(hwIntrinsic != intrinsic);
                return gtNewSimdHWIntrinsicNode(type, hwIntrinsic, baseType, size, op1, op2);
            }

            if (compOpportunisticallyDependsOn(InstructionSet_SSE42))
            {
                intrinsic =
                    (intrinsic == NI_VectorT128_GreaterThan) ? NI_SSE42_CompareGreaterThan : NI_SSE42_CompareLessThan;
                return gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, baseType, 16, op1, op2);
            }

            // There is no direct SSE2 support for comparing TYP_LONG vectors.
            // These have to be implemented in terms of TYP_INT vector comparison operations.
            //
            // Let us consider the case of single long element comparison.
            // Say op1 = (x1, y1) and op2 = (x2, y2) where x1, y1, x2, and y2 are 32-bit integers that comprise the
            // longs op1 and op2.
            //
            // GreaterThan(op1, op2) can be expressed in terms of > relationship between 32-bit integers that
            // comprise op1 and op2 as
            //                    =  (x1, y1) > (x2, y2)
            //                    =  (x1 > x2) || [(x1 == x2) && (y1 > y2)]   - eq (1)
            //
            // t = PCMPGTD(op1, op2)
            // u = PCMPEQD(op1, op2)
            // v = unsigned emulated PCMPGTD(op1, op2)
            // t = PSHUFD(t, (3, 3, 1, 1)) - This corresponds to (x1 > x2) in eq(1) above
            // v = PSHUFD(v, (2, 2, 0, 0)) - This corresponds to (y1 > y2) in eq(1) above
            // u = PSHUFD(u, (3, 3, 1, 1)) - This corresponds to (x1 == x2) in eq(1) above
            // v = PAND(v, u)              - This corresponds to [(x1 == x2) && (y1 > y2)] in eq(1) above
            // result = POR(t, v)

            GenTree* uses[2][3];
            impMakeMultiUse(op1, uses[0], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.GreaterLessThan temp"));
            impMakeMultiUse(op2, uses[1], layout, CHECK_SPILL_ALL DEBUGARG("Vector<T>.Greater/LessThan temp"));

            GenTree* v = impSimdAsHWIntrinsicRelOp(intrinsic, TYP_UINT, layout, uses[0][0], uses[1][0]);
            v = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, v, gtNewIconNode(SHUFFLE_ZZXX));
            intrinsic = (intrinsic == NI_VectorT128_GreaterThan) ? NI_SSE2_CompareGreaterThan : NI_SSE2_CompareLessThan;
            GenTree* t = gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, TYP_INT, 16, uses[0][1], uses[1][1]);
            t = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, t, gtNewIconNode(SHUFFLE_WWYY));
            GenTree* u =
                gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_CompareEqual, TYP_INT, 16, uses[0][2], uses[1][2]);
            u = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, u, gtNewIconNode(SHUFFLE_WWYY));
            v = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_And, TYP_INT, 16, v, u);
            return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Or, TYP_LONG, 16, t, v);
        }

        default:
            assert(!"Unexpected SimdAsHWIntrinsic");
            return nullptr;
    }
}
#endif // TARGET_XARCH

#endif // FEATURE_HW_INTRINSICS
