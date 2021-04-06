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

    var_types    baseType   = TYP_UNDEF;
    unsigned     simdSize   = 0;
    ClassLayout* simdLayout = nullptr;

    if (strcmp(className, "Vector") == 0)
    {
        assert(!signature.hasThisParam);

        if (signature.paramCount == 0)
        {
            // IsHardwareAccelerated is imported by the old SIMD intrinsic code.
            return nullptr;
        }

        ClassLayout* layout = signature.paramLayout[0];

        if (!layout->IsVector())
        {
            // Many Vector methods are generic, ignore instantiations that use
            // invalid element types.
            return nullptr;
        }

        baseType = layout->GetElementType();
        simdSize = layout->GetSize();
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

        simdLayout = typGetObjLayout(clsHnd);

        if (!simdLayout->IsVector())
        {
            // Ignore Vector<T> instantiations that use invalid element types.
            return nullptr;
        }

        if (varTypeIsStruct(signature.retType))
        {
            ClassLayout* retLayout = signature.retLayout;
            assert(retLayout->IsVector());
            baseType = retLayout->GetElementType();
            simdSize = retLayout->GetSize();
        }
        else if (signature.paramCount == 0)
        {
            baseType = simdLayout->GetElementType();
            simdSize = simdLayout->GetSize();
        }
        else
        {
            // TODO-MIKE-Cleanup: Old code was bogus, this needs to be done only
            // if the first argument is known to be a vector type. It could be a
            // primitive type though (e.g. Vector2/3/4 constructors). This results
            // in CreateBroadcast being ignored.

            if (varTypeIsStruct(signature.paramType[0]))
            {
                ClassLayout* layout = signature.paramLayout[0];
                assert(layout->IsVector());
                baseType = layout->GetElementType();
                simdSize = layout->GetSize();
            }
            else
            {
                return nullptr;
            }
        }
    }

    NamedIntrinsic hwIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, baseType);

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
            case NI_Vector3_CreateBroadcast:
            case NI_Vector4_CreateBroadcast:
            case NI_VectorT128_CreateBroadcast:
#ifdef TARGET_XARCH
            case NI_VectorT256_CreateBroadcast:
#endif
                return impSimdAsHWIntrinsicCreate(intrinsic, signature, simdLayout, newobjThis);

            default:
                assert(newobjThis == nullptr);
                assert(!signature.hasThisParam);
                return impSimdAsHWIntrinsicSpecial(intrinsic, signature, simdLayout);
        }
    }

    CORINFO_InstructionSet hwIntrinsicIsa = HWIntrinsicInfo::lookupIsa(hwIntrinsic);

    if (!compOpportunisticallyDependsOn(hwIntrinsicIsa))
    {
        return nullptr;
    }

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
                ops[0] = impPopArgForHWIntrinsic(signature.paramType[0], signature.paramLayout[0], true);
                assert(!SimdAsHWIntrinsicInfo::NeedsOperandsSwapped(intrinsic));
                return gtNewSimdAsHWIntrinsicNode(signature.retType, hwIntrinsic, baseType, simdSize, ops[0], ops[1]);

            default:
                return nullptr;
        }
    }

    switch (signature.paramCount)
    {
        GenTree* ops[2];

        case 0:
            return gtNewSimdAsHWIntrinsicNode(signature.retType, hwIntrinsic, baseType, simdSize);

        case 1:
            ops[0] = impPopArgForHWIntrinsic(signature.paramType[0], signature.paramLayout[0]);
            return gtNewSimdAsHWIntrinsicNode(signature.retType, hwIntrinsic, baseType, simdSize, ops[0]);

        case 2:
            ops[1] = impPopArgForHWIntrinsic(signature.paramType[1], signature.paramLayout[1]);
            ops[0] = impPopArgForHWIntrinsic(signature.paramType[0], signature.paramLayout[0]);

            if (SimdAsHWIntrinsicInfo::NeedsOperandsSwapped(intrinsic))
            {
                // TODO-MIKE-Fix: This is nonsense, it changes the order of evaluation.
                std::swap(ops[0], ops[1]);
            }

            return gtNewSimdAsHWIntrinsicNode(signature.retType, hwIntrinsic, baseType, simdSize, ops[0], ops[1]);

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
#if defined(TARGET_XARCH)
    bool isAVX = (SimdAsHWIntrinsicInfo::lookupClassId(intrinsic) == SimdAsHWIntrinsicClassId::VectorT256);
    assert(compIsaSupportedDebugOnly(InstructionSet_SSE2));
    assert(!isAVX || compIsaSupportedDebugOnly(InstructionSet_AVX2));
#elif defined(TARGET_ARM64)
    assert(compIsaSupportedDebugOnly(InstructionSet_AdvSimd));
#else
#error Unsupported platform
#endif

    if (sig.paramCount > 3)
    {
        return nullptr;
    }

    switch (intrinsic)
    {
#ifdef TARGET_XARCH
        case NI_VectorT256_As:
#endif
        case NI_VectorT128_As:
        {
            if (!sig.retLayout->IsVector())
            {
                return nullptr;
            }
            break;
        }

#ifdef TARGET_XARCH
        case NI_VectorT128_Dot:
        {
            if (!compOpportunisticallyDependsOn(InstructionSet_SSE41))
            {
                // We need to exit early if this is Vector<T>.Dot for int or uint and SSE41 is not supported
                // The other types should be handled via the table driven paths

                var_types opType = sig.paramLayout[0]->GetElementType();
                assert((opType == TYP_INT) || (opType == TYP_UINT));
                return nullptr;
            }
            break;
        }
#endif

        default:
            // Most intrinsics have some path that works even if only SSE2/AdvSimd is available
            break;
    }

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
            return gtNewSimdCreateBroadcastNode(retType, gtNewOneConNode(retBaseType), retBaseType, retSize,
                                                /* isSimdAsHWIntrinsic */ true);
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

                bitMask = gtNewSimdCreateBroadcastNode(retType, bitMask, retBaseType, retSize,
                                                       /* isSimdAsHWIntrinsic */ true);

                intrinsic = MapVectorTIntrinsic(NI_VectorT128_op_BitwiseAnd, isAVX);
                intrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, retBaseType);

                return gtNewSimdAsHWIntrinsicNode(retType, intrinsic, retBaseType, retSize, ops[0], bitMask);
            }

            if (varTypeIsUnsigned(retBaseType))
            {
                return ops[0];
            }

            if ((retBaseType != TYP_LONG) && compOpportunisticallyDependsOn(InstructionSet_SSSE3))
            {
                return gtNewSimdAsHWIntrinsicNode(retType, NI_SSSE3_Abs, retBaseType, retSize, ops[0]);
            }

            GenTree* dup[2];
            ops[0] = impCloneExpr(ops[0], &dup[0], retLayout, CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.Abs"));
            dup[0] = impCloneExpr(dup[0], &dup[1], retLayout, CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.Abs"));

            // op1 = op1 < Zero
            GenTree* tmp = gtNewSIMDVectorZero(retType, retBaseType, retSize);

            ops[0] = impSimdAsHWIntrinsicRelOp(MapVectorTIntrinsic(NI_VectorT128_LessThan, isAVX), retBaseType,
                                               retLayout, ops[0], tmp);

            // tmp = Zero - op1Dup1
            tmp = gtNewSIMDVectorZero(retType, retBaseType, retSize);
            tmp = gtNewSimdAsHWIntrinsicNode(retType, isAVX ? NI_AVX2_Subtract : NI_SSE2_Subtract, retBaseType, retSize,
                                             tmp, dup[0]);

            return impSimdAsHWIntrinsicCndSel(retLayout, ops[0], tmp, dup[1]);
        }

        case NI_Vector2_op_Division:
        case NI_Vector3_op_Division:
        {
            assert(sig.paramCount == 2);
            assert((retLayout == sig.paramLayout[0]) && (retLayout == sig.paramLayout[1]));
            assert((retLayout->GetSize() == 8) || (retLayout->GetSize() == 12));
            assert(retLayout->GetElementType() == TYP_FLOAT);

            unsigned retSize = retLayout->GetSize();
            GenTree* retNode =
                gtNewSimdAsHWIntrinsicNode(TYP_SIMD16, NI_SSE_Divide, TYP_FLOAT, retSize, ops[0], ops[1]);

            // Vector2/3 div: since the top-most elements will be zero, we end up
            // perfoming 0/0 which is a NAN. Therefore, post division we need to set the
            // top-most elements to zero. This is achieved by left logical shift followed
            // by right logical shift of the result.
            retNode = gtNewSimdAsHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ShiftLeftLogical128BitLane, TYP_INT, retSize,
                                                 retNode, gtNewIconNode(16 - retSize));
            retNode = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_ShiftRightLogical128BitLane, TYP_INT, retSize,
                                                 retNode, gtNewIconNode(16 - retSize));
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
            return gtNewSimdAsHWIntrinsicNode(retType, NI_Vector128_Dot, opBaseType, opSize, ops[0], ops[1]);
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
                GenTree*  constVal = nullptr;
                var_types opType   = retBaseType;

                NamedIntrinsic opIntrinsic;

                switch (retBaseType)
                {
                    case TYP_BYTE:
                        constVal = gtNewIconNode(0x80808080, TYP_INT);
                        opIntrinsic =
                            SimdAsHWIntrinsicInfo::lookupHWIntrinsic(NI_VectorT128_op_Subtraction, retBaseType);
                        retBaseType = TYP_UBYTE;
                        break;
                    case TYP_USHORT:
                        constVal    = gtNewIconNode(0x80008000, TYP_INT);
                        opIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(NI_VectorT128_op_Addition, retBaseType);
                        retBaseType = TYP_SHORT;
                        break;
                    default:
                        unreached();
                }

                GenTree* constVector[3];
                constVector[0] = gtNewSimdCreateBroadcastNode(retType, constVal, TYP_INT, retSize,
                                                              /* isSimdAsHWIntrinsic */ true);
                constVector[0] = impCloneExpr(constVector[0], &constVector[1], retLayout,
                                              CHECK_SPILL_ALL DEBUGARG("Clone constVector for Vector<T>.Max/Min"));
                constVector[1] = impCloneExpr(constVector[1], &constVector[2], retLayout,
                                              CHECK_SPILL_ALL DEBUGARG("Clone constVector for Vector<T>.Max/Min"));

                ops[0] = gtNewSimdAsHWIntrinsicNode(retType, opIntrinsic, opType, retSize, ops[0], constVector[0]);
                ops[1] = gtNewSimdAsHWIntrinsicNode(retType, opIntrinsic, opType, retSize, ops[1], constVector[1]);

                intrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, retBaseType);
                ops[0]    = gtNewSimdAsHWIntrinsicNode(retType, intrinsic, retBaseType, retSize, ops[0], ops[1]);

                intrinsic = (intrinsic == NI_VectorT128_op_Subtraction) ? NI_VectorT128_op_Addition
                                                                        : NI_VectorT128_op_Subtraction;
                intrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, opType);
                return gtNewSimdAsHWIntrinsicNode(retType, intrinsic, opType, retSize, ops[0], constVector[2]);
            }

            intrinsic = (intrinsic == NI_VectorT128_Max) || (intrinsic == NI_VectorT256_Max) ? NI_VectorT128_GreaterThan
                                                                                             : NI_VectorT128_LessThan;
            intrinsic = MapVectorTIntrinsic(intrinsic, isAVX);

            GenTree* dup[2];
            ops[0] =
                impCloneExpr(ops[0], &dup[0], retLayout, CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.Max/Min"));
            ops[1] =
                impCloneExpr(ops[1], &dup[1], retLayout, CHECK_SPILL_ALL DEBUGARG("Clone op2 for Vector<T>.Max/Min"));

            ops[0] = impSimdAsHWIntrinsicRelOp(intrinsic, retBaseType, retLayout, ops[0], ops[1]);

            return impSimdAsHWIntrinsicCndSel(retLayout, ops[0], dup[0], dup[1]);
        }

        case NI_VectorT128_op_Multiply:
        {
            assert(sig.paramCount == 2);
            assert((retLayout == sig.paramLayout[0]) && (retLayout == sig.paramLayout[1]));
            assert(retLayout->GetElementType() == TYP_INT);

            var_types retBaseType = retLayout->GetElementType();
            unsigned  retSize     = retLayout->GetSize();

            if (compOpportunisticallyDependsOn(InstructionSet_SSE41))
            {
                return gtNewSimdAsHWIntrinsicNode(retType, NI_SSE41_MultiplyLow, retBaseType, retSize, ops[0], ops[1]);
            }

            GenTree* dup[2];
            ops[0] =
                impCloneExpr(ops[0], &dup[0], retLayout, CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.Multiply"));
            ops[1] =
                impCloneExpr(ops[1], &dup[1], retLayout, CHECK_SPILL_ALL DEBUGARG("Clone op2 for Vector<T>.Multiply"));

            ops[0] = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_ShiftRightLogical128BitLane, retBaseType, retSize,
                                                ops[0], gtNewIconNode(4, TYP_INT));
            ops[1] = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_ShiftRightLogical128BitLane, retBaseType, retSize,
                                                ops[1], gtNewIconNode(4, TYP_INT));
            ops[1] = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_Multiply, TYP_ULONG, retSize, ops[1], ops[0]);
            ops[1] = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_Shuffle, retBaseType, retSize, ops[1],
                                                gtNewIconNode(SHUFFLE_XXZX, TYP_INT));
            ops[0] = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_Multiply, TYP_ULONG, retSize, dup[0], dup[1]);
            ops[0] = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_Shuffle, retBaseType, retSize, ops[0],
                                                gtNewIconNode(SHUFFLE_XXZX, TYP_INT));

            return gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_UnpackLow, retBaseType, retSize, ops[0], ops[1]);
        }

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
            assert(varTypeIsLong(retLayout->GetElementType()));

            var_types retBaseType = retLayout->GetElementType();
            unsigned  retSize     = retLayout->GetSize();

            intrinsic = (intrinsic == NI_VectorT128_Max) ? NI_VectorT128_GreaterThan : NI_VectorT128_LessThan;
            intrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, retBaseType);

            GenTree* dup[2];
            ops[0] =
                impCloneExpr(ops[0], &dup[0], retLayout, CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.Max/Min"));
            ops[1] =
                impCloneExpr(ops[1], &dup[1], retLayout, CHECK_SPILL_ALL DEBUGARG("Clone op2 for Vector<T>.Max/Min"));
            ops[0] = gtNewSimdAsHWIntrinsicNode(retType, intrinsic, retBaseType, retSize, ops[0], ops[1]);
            return gtNewSimdAsHWIntrinsicNode(retType, NI_AdvSimd_BitwiseSelect, retBaseType, retSize, ops[0], dup[0],
                                              dup[1]);
        }

        case NI_VectorT128_ConditionalSelect:
        {
            assert(sig.paramCount == 3);
            // TODO-MIKE-Cleanup: Types are messed up - baseType is extracted from the first ConditionalSelect
            // parameter, this may be different from the return type (int/float, long/double).
            var_types retBaseType = sig.paramLayout[0]->GetElementType();
            unsigned  retSize     = sig.paramLayout[0]->GetSize();
            return gtNewSimdAsHWIntrinsicNode(retType, NI_AdvSimd_BitwiseSelect, retBaseType, retSize, ops[0], ops[1],
                                              ops[2]);
        }
#endif // TARGET_ARM64

        default:
            assert(!"Unexpected SimdAsHWIntrinsic");
            return nullptr;
    }
}

GenTree* Compiler::impSimdAsHWIntrinsicCreate(NamedIntrinsic              intrinsic,
                                              const HWIntrinsicSignature& sig,
                                              ClassLayout*                thisLayout,
                                              GenTree*                    newobjThis)
{
    assert(thisLayout->IsVector());
    assert(SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, thisLayout->GetElementType()) == intrinsic);
#if defined(TARGET_XARCH)
    bool isVectorT256 = (SimdAsHWIntrinsicInfo::lookupClassId(intrinsic) == SimdAsHWIntrinsicClassId::VectorT256);
    assert(compIsaSupportedDebugOnly(InstructionSet_SSE2));
    assert(!isVectorT256 || compIsaSupportedDebugOnly(InstructionSet_AVX2));
#elif defined(TARGET_ARM64)
    assert(compIsaSupportedDebugOnly(InstructionSet_AdvSimd));
#else
#error Unsupported platform
#endif

    switch (intrinsic)
    {
        case NI_Vector2_CreateBroadcast:
        case NI_Vector3_CreateBroadcast:
        case NI_Vector4_CreateBroadcast:
        case NI_VectorT128_CreateBroadcast:
#ifdef TARGET_XARCH
        case NI_VectorT256_CreateBroadcast:
#endif
        {
            assert(sig.retType == TYP_VOID);
            assert(sig.hasThisParam);
            assert(sig.paramCount == 1);
            assert(SimdAsHWIntrinsicInfo::IsInstanceMethod(intrinsic));

#ifndef TARGET_64BIT
            if (varTypeIsLong(baseType))
            {
                // TODO-XARCH-CQ: It may be beneficial to emit the movq
                // instruction, which takes a 64-bit memory address and
                // works on 32-bit x86 systems.
                return nullptr;
            }
        }
#endif

            GenTree* op2 = impPopArgForHWIntrinsic(sig.paramType[0], nullptr);
            GenTree* op1 = impPopArgForHWIntrinsic(thisLayout->GetSIMDType(), thisLayout, true, newobjThis);

            return impAssignSIMDAddr(op1,
                                     gtNewSimdCreateBroadcastNode(thisLayout->GetSIMDType(), op2,
                                                                  thisLayout->GetElementType(), thisLayout->GetSize(),
                                                                  /* isSimdAsHWIntrinsic */ true));
    }

    default:
        unreached();
}
}

#if defined(TARGET_XARCH)

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

    GenTree* op1Dup;
    op1 = impCloneExpr(op1, &op1Dup, layout, CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.ConditionalSelect"));
    op2 = gtNewSimdAsHWIntrinsicNode(type, andIntrinsic, baseType, size, op2, op1);
    op3 = gtNewSimdAsHWIntrinsicNode(type, andnIntrinsic, baseType, size, op1Dup, op3);
    return gtNewSimdAsHWIntrinsicNode(type, orIntrinsic, baseType, size, op2, op3);
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
                return gtNewSimdAsHWIntrinsicNode(type, hwIntrinsic, baseType, size, op1, op2);
            }

            if (compOpportunisticallyDependsOn(InstructionSet_SSE41))
            {
                return gtNewSimdAsHWIntrinsicNode(type, NI_SSE41_CompareEqual, baseType, size, op1, op2);
            }

            // There is no direct SSE2 support for comparing TYP_LONG vectors.
            // These have to be implemented in terms of TYP_INT vector comparison operations.
            //
            // PAND(PCMPEQD(op1, op2), PSHUFD(PCMPEQD(op1, op2), (2, 3, 0, 1)))
            //
            // Shuffle is meant to swap the comparison results of low-32-bits and high 32-bits of
            // respective long elements.

            GenTree* eq = gtNewSimdAsHWIntrinsicNode(TYP_SIMD16, NI_SSE2_CompareEqual, TYP_INT, 16, op1, op2);
            GenTree* shuffleEq;
            eq = impCloneExpr(eq, &shuffleEq, layout, CHECK_SPILL_ALL DEBUGARG("Clone tmp for Vector<T>.Equals"));
            shuffleEq = gtNewSimdAsHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, shuffleEq,
                                                   gtNewIconNode(SHUFFLE_ZWXY, TYP_INT));
            return gtNewSimdAsHWIntrinsicNode(TYP_SIMD16, NI_SSE2_And, baseType, 16, shuffleEq, eq);
        }

        case NI_VectorT128_GreaterThanOrEqual:
        case NI_VectorT128_LessThanOrEqual:
        case NI_VectorT256_GreaterThanOrEqual:
        case NI_VectorT256_LessThanOrEqual:
        {
            // There is no direct support for doing a combined comparison and equality for integral types.
            // These have to be implemented by performing both halves and combining their results.
            //
            // op1Dup = op1
            // op2Dup = op2
            //
            // op1 = GreaterThan(op1, op2)
            // op2 = Equals(op1Dup, op2Dup)
            //
            // result = BitwiseOr(op1, op2)
            //
            // Where the GreaterThan(op1, op2) comparison could also be LessThan(op1, op2)

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

            GenTree* dup[2];
            op1 = impCloneExpr(op1, &dup[0], layout,
                               CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.GreaterThanOrEqual/LessThanOrEqual"));
            op2 = impCloneExpr(op2, &dup[1], layout,
                               CHECK_SPILL_ALL DEBUGARG("Clone op2 for Vector<T>.GreaterThanOrEqual/LessThanOrEqual"));

            op1 = impSimdAsHWIntrinsicRelOp(eqIntrinsic, baseType, layout, op1, op2);
            op2 = impSimdAsHWIntrinsicRelOp(intrinsic, baseType, layout, dup[0], dup[1]);
            return gtNewSimdAsHWIntrinsicNode(type, orIntrinsic, baseType, size, op1, op2);
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

                GenTree* constVector[2];
                constVector[0] = gtNewSimdCreateBroadcastNode(type, constVal, constVal->GetType(), size,
                                                              /* isSimdAsHWIntrinsic */ true);
                constVector[0] =
                    impCloneExpr(constVector[0], &constVector[1], layout,
                                 CHECK_SPILL_ALL DEBUGARG("Clone constVector for Vector<T>.GreaterThan/LessThan"));

                NamedIntrinsic hwIntrinsic = isAVX ? NI_AVX2_Subtract : NI_SSE2_Subtract;
                op1 = gtNewSimdAsHWIntrinsicNode(type, hwIntrinsic, baseType, size, op1, constVector[0]);
                op2 = gtNewSimdAsHWIntrinsicNode(type, hwIntrinsic, baseType, size, op2, constVector[1]);
            }

            if (isAVX || (baseType != TYP_LONG))
            {
                NamedIntrinsic hwIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, baseType);
                assert(hwIntrinsic != intrinsic);
                return gtNewSimdAsHWIntrinsicNode(type, hwIntrinsic, baseType, size, op1, op2);
            }

            if (compOpportunisticallyDependsOn(InstructionSet_SSE42))
            {
                intrinsic =
                    (intrinsic == NI_VectorT128_GreaterThan) ? NI_SSE42_CompareGreaterThan : NI_SSE42_CompareLessThan;
                return gtNewSimdAsHWIntrinsicNode(TYP_SIMD16, intrinsic, baseType, 16, op1, op2);
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
            // op1Dup1 = op1
            // op1Dup2 = op1Dup1
            // op2Dup1 = op2
            // op2Dup2 = op2Dup1
            //
            // t = PCMPGTD(op1, op2)
            // u = PCMPEQD(op1Dup1, op2Dup1)
            // v = unsigned emulated PCMPGTD(op1Dup2, op2Dup2)
            //
            // op1 = PSHUFD(t, (3, 3, 1, 1)) - This corresponds to (x1 > x2) in eq(1) above
            // v = PSHUFD(v, (2, 2, 0, 0))   - This corresponds to (y1 > y2) in eq(1) above
            // u = PSHUFD(u, (3, 3, 1, 1))   - This corresponds to (x1 == x2) in eq(1) above
            // op2 = PAND(v, u)         - This corresponds to [(x1 == x2) && (y1 > y2)] in eq(1) above
            //
            // result = POR(op1, op2)

            GenTree* dup[2][2];
            op1 = impCloneExpr(op1, &dup[0][0], layout,
                               CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.GreaterThan/LessThan"));
            dup[0][0] = impCloneExpr(dup[0][0], &dup[0][1], layout,
                                     CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.GreaterThan/LessThan"));
            op2 = impCloneExpr(op2, &dup[1][0], layout,
                               CHECK_SPILL_ALL DEBUGARG("Clone op2 for Vector<T>.GreaterThan/LessThan"));
            dup[1][0] = impCloneExpr(dup[1][0], &dup[1][1], layout,
                                     CHECK_SPILL_ALL DEBUGARG("Clone op2 Vector<T>.GreaterThan/LessThan"));

            GenTree* v = impSimdAsHWIntrinsicRelOp(intrinsic, TYP_UINT, layout, op1, op2);
            v = gtNewSimdAsHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, v, gtNewIconNode(SHUFFLE_ZZXX));
            intrinsic = (intrinsic == NI_VectorT128_GreaterThan) ? NI_SSE2_CompareGreaterThan : NI_SSE2_CompareLessThan;
            GenTree* t = gtNewSimdAsHWIntrinsicNode(TYP_SIMD16, intrinsic, TYP_INT, 16, dup[0][0], dup[1][0]);
            t = gtNewSimdAsHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, t, gtNewIconNode(SHUFFLE_WWYY));
            GenTree* u =
                gtNewSimdAsHWIntrinsicNode(TYP_SIMD16, NI_SSE2_CompareEqual, TYP_INT, 16, dup[0][1], dup[1][1]);
            u = gtNewSimdAsHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Shuffle, TYP_INT, 16, u, gtNewIconNode(SHUFFLE_WWYY));
            v = gtNewSimdAsHWIntrinsicNode(TYP_SIMD16, NI_SSE2_And, TYP_INT, 16, v, u);
            return gtNewSimdAsHWIntrinsicNode(TYP_SIMD16, NI_SSE2_Or, TYP_LONG, 16, t, v);
        }

        default:
            assert(!"Unexpected SimdAsHWIntrinsic");
            return nullptr;
    }
}
#endif // TARGET_XARCH

#endif // FEATURE_HW_INTRINSICS
