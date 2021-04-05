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

    unsigned numArgs          = sig->numArgs;
    bool     isInstanceMethod = false;

    if (sig->hasThis())
    {
        numArgs++;
        isInstanceMethod = true;
    }

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
NamedIntrinsic MapVectorTIntrinsic(NamedIntrinsic intrinsic, bool isVectorT256)
{
    if (!isVectorT256)
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

        clsHnd   = layout->GetClassHandle();
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
                return impSimdAsHWIntrinsicCreate(intrinsic, clsHnd, signature, baseType, simdSize, newobjThis);

            default:
                assert(newobjThis == nullptr);
                assert(!signature.hasThisParam);
                return impSimdAsHWIntrinsicSpecial(intrinsic, clsHnd, signature, baseType, simdSize);
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

//------------------------------------------------------------------------
// impSimdAsHWIntrinsicSpecial: Import a SIMD intrinsic as a GT_HWINTRINSIC node if possible
//                              This method handles cases which cannot be table driven
//
// Arguments:
//    intrinsic  -- id of the intrinsic function.
//    clsHnd     -- class handle containing the intrinsic function.
//    sig        -- signature of the intrinsic call
//    baseType   -- the base type of SIMD type of the intrinsic
//    simdSize   -- the size of the SIMD type of the intrinsic
//
// Return Value:
//    The GT_HWINTRINSIC node, or nullptr if not a supported intrinsic
//
GenTree* Compiler::impSimdAsHWIntrinsicSpecial(NamedIntrinsic              intrinsic,
                                               CORINFO_CLASS_HANDLE        clsHnd,
                                               const HWIntrinsicSignature& sig,
                                               var_types                   baseType,
                                               unsigned                    simdSize)
{
    assert(featureSIMD);
    assert(varTypeIsArithmetic(baseType));
    assert(simdSize != 0);
    assert(SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, baseType) == intrinsic);
    assert(!sig.hasThisParam);
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

                assert((baseType == TYP_INT) || (baseType == TYP_UINT));
                return nullptr;
            }
            break;
        }
#endif

        default:
            // Most intrinsics have some path that works even if only SSE2/AdvSimd is available
            break;
    }

    var_types retType = sig.retType;

    switch (intrinsic)
    {
        case NI_Vector2_get_One:
        case NI_Vector3_get_One:
        case NI_Vector4_get_One:
        case NI_VectorT128_get_One:
#ifdef TARGET_XARCH
        case NI_VectorT256_get_One:
#endif
            assert(sig.paramCount == 0);
            return gtNewSimdCreateBroadcastNode(retType, gtNewOneConNode(baseType), baseType, simdSize,
                                                /* isSimdAsHWIntrinsic */ true);

        case NI_VectorT128_get_Count:
#ifdef TARGET_XARCH
        case NI_VectorT256_get_Count:
#endif
        {
            assert(sig.paramCount == 0);
            GenTreeIntCon* countNode = gtNewIconNode(getSIMDVectorLength(simdSize, baseType), TYP_INT);
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
            GenTree* op1 = impSIMDPopStack(retType);
            SetOpLclRelatedToSIMDIntrinsic(op1);
            assert(op1->GetType() == sig.retLayout->GetSIMDType());
            return op1;
        }

        default:
            break;
    }

    if ((sig.paramCount < 1) || (sig.paramCount > 3))
    {
        return nullptr;
    }

    GenTree* ops[3];

    for (unsigned i = sig.paramCount; i != 0; i--)
    {
        ops[i - 1] = impPopArgForHWIntrinsic(sig.paramType[i - 1], sig.paramLayout[i - 1]);
    }

    assert(!SimdAsHWIntrinsicInfo::NeedsOperandsSwapped(intrinsic));

    switch (intrinsic)
    {
#ifdef TARGET_XARCH
        case NI_Vector2_Abs:
        case NI_Vector3_Abs:
        case NI_Vector4_Abs:
        case NI_VectorT128_Abs:
        case NI_VectorT256_Abs:
        {
            assert(sig.paramCount == 1);

            if (varTypeIsFloating(baseType))
            {
                // Abs(vf) = vf & new SIMDVector<float>(0x7fffffff);
                // Abs(vd) = vf & new SIMDVector<double>(0x7fffffffffffffff);
                GenTree* bitMask;

                if (baseType == TYP_FLOAT)
                {
                    bitMask = gtNewDconNode(jitstd::bit_cast<float, int32_t>(0x7fffffff), TYP_FLOAT);
                }
                else
                {
                    assert(baseType == TYP_DOUBLE);
                    bitMask = gtNewDconNode(jitstd::bit_cast<double, int64_t>(0x7fffffffffffffffLL), TYP_DOUBLE);
                }

                bitMask = gtNewSimdCreateBroadcastNode(retType, bitMask, baseType, simdSize,
                                                       /* isSimdAsHWIntrinsic */ true);

                intrinsic = MapVectorTIntrinsic(NI_VectorT128_op_BitwiseAnd, isVectorT256);
                intrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, baseType);

                return gtNewSimdAsHWIntrinsicNode(retType, intrinsic, baseType, simdSize, ops[0], bitMask);
            }

            if (varTypeIsUnsigned(baseType))
            {
                return ops[0];
            }

            if ((baseType != TYP_LONG) && compOpportunisticallyDependsOn(InstructionSet_SSSE3))
            {
                return gtNewSimdAsHWIntrinsicNode(retType, NI_SSSE3_Abs, baseType, simdSize, ops[0]);
            }

            GenTree* dup[2];
            ops[0] = impCloneExpr(ops[0], &dup[0], clsHnd, CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.Abs"));
            dup[0] = impCloneExpr(dup[0], &dup[1], clsHnd, CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.Abs"));

            // op1 = op1 < Zero
            GenTree* tmp = gtNewSIMDVectorZero(retType, baseType, simdSize);
            ops[0]       = impSimdAsHWIntrinsicRelOp(MapVectorTIntrinsic(NI_VectorT128_LessThan, isVectorT256), clsHnd,
                                               retType, baseType, simdSize, ops[0], tmp);

            // tmp = Zero - op1Dup1
            tmp = gtNewSIMDVectorZero(retType, baseType, simdSize);
            tmp = gtNewSimdAsHWIntrinsicNode(retType, isVectorT256 ? NI_AVX2_Subtract : NI_SSE2_Subtract, baseType,
                                             simdSize, tmp, dup[0]);

            return impSimdAsHWIntrinsicCndSel(clsHnd, retType, baseType, simdSize, ops[0], tmp, dup[1]);
        }

        case NI_Vector2_op_Division:
        case NI_Vector3_op_Division:
        {
            assert(sig.paramCount == 2);
            assert((simdSize == 8) || (simdSize == 12));

            // Vector2/3 div: since the top-most elements will be zero, we end up
            // perfoming 0/0 which is a NAN. Therefore, post division we need to set the
            // top-most elements to zero. This is achieved by left logical shift followed
            // by right logical shift of the result.

            GenTree* retNode = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE_Divide, baseType, simdSize, ops[0], ops[1]);
            retNode = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_ShiftLeftLogical128BitLane, TYP_INT, simdSize,
                                                 retNode, gtNewIconNode(16 - simdSize, TYP_INT));
            retNode = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_ShiftRightLogical128BitLane, TYP_INT, simdSize,
                                                 retNode, gtNewIconNode(16 - simdSize, TYP_INT));

            return retNode;
        }

        case NI_VectorT128_Dot:
            assert(sig.paramCount == 2);
            assert((baseType == TYP_INT) || (baseType == TYP_UINT));
            assert(compIsaSupportedDebugOnly(InstructionSet_SSE41));
            return gtNewSimdAsHWIntrinsicNode(retType, NI_Vector128_Dot, baseType, simdSize, ops[0], ops[1]);

        case NI_VectorT128_Equals:
        case NI_VectorT128_GreaterThan:
        case NI_VectorT128_GreaterThanOrEqual:
        case NI_VectorT128_LessThan:
        case NI_VectorT128_LessThanOrEqual:
        case NI_VectorT256_GreaterThan:
        case NI_VectorT256_GreaterThanOrEqual:
        case NI_VectorT256_LessThan:
        case NI_VectorT256_LessThanOrEqual:
            assert(sig.paramCount == 2);
            return impSimdAsHWIntrinsicRelOp(intrinsic, clsHnd, retType, baseType, simdSize, ops[0], ops[1]);

        case NI_VectorT128_Max:
        case NI_VectorT128_Min:
        case NI_VectorT256_Max:
        case NI_VectorT256_Min:
        {
            assert(sig.paramCount == 2);

            if ((baseType == TYP_BYTE) || (baseType == TYP_USHORT))
            {
                GenTree*  constVal = nullptr;
                var_types opType   = baseType;

                NamedIntrinsic opIntrinsic;

                switch (baseType)
                {
                    case TYP_BYTE:
                        constVal    = gtNewIconNode(0x80808080, TYP_INT);
                        opIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(NI_VectorT128_op_Subtraction, baseType);
                        baseType    = TYP_UBYTE;
                        break;
                    case TYP_USHORT:
                        constVal    = gtNewIconNode(0x80008000, TYP_INT);
                        opIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(NI_VectorT128_op_Addition, baseType);
                        baseType    = TYP_SHORT;
                        break;
                    default:
                        unreached();
                }

                GenTree* constVector[3];
                constVector[0] = gtNewSimdCreateBroadcastNode(retType, constVal, TYP_INT, simdSize,
                                                              /* isSimdAsHWIntrinsic */ true);
                constVector[0] = impCloneExpr(constVector[0], &constVector[1], clsHnd,
                                              CHECK_SPILL_ALL DEBUGARG("Clone constVector for Vector<T>.Max/Min"));
                constVector[1] = impCloneExpr(constVector[1], &constVector[2], clsHnd,
                                              CHECK_SPILL_ALL DEBUGARG("Clone constVector for Vector<T>.Max/Min"));

                ops[0] = gtNewSimdAsHWIntrinsicNode(retType, opIntrinsic, opType, simdSize, ops[0], constVector[0]);
                ops[1] = gtNewSimdAsHWIntrinsicNode(retType, opIntrinsic, opType, simdSize, ops[1], constVector[1]);

                intrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, baseType);
                ops[0]    = gtNewSimdAsHWIntrinsicNode(retType, intrinsic, baseType, simdSize, ops[0], ops[1]);

                intrinsic = (intrinsic == NI_VectorT128_op_Subtraction) ? NI_VectorT128_op_Addition
                                                                        : NI_VectorT128_op_Subtraction;
                intrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, opType);
                return gtNewSimdAsHWIntrinsicNode(retType, intrinsic, opType, simdSize, ops[0], constVector[2]);
            }

            intrinsic = (intrinsic == NI_VectorT128_Max) || (intrinsic == NI_VectorT256_Max) ? NI_VectorT128_GreaterThan
                                                                                             : NI_VectorT128_LessThan;
            intrinsic = MapVectorTIntrinsic(intrinsic, isVectorT256);

            GenTree* dup[2];
            ops[0] = impCloneExpr(ops[0], &dup[0], clsHnd, CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.Max/Min"));
            ops[1] = impCloneExpr(ops[1], &dup[1], clsHnd, CHECK_SPILL_ALL DEBUGARG("Clone op2 for Vector<T>.Max/Min"));
            ops[0] = impSimdAsHWIntrinsicRelOp(intrinsic, clsHnd, retType, baseType, simdSize, ops[0], ops[1]);
            return impSimdAsHWIntrinsicCndSel(clsHnd, retType, baseType, simdSize, ops[0], dup[0], dup[1]);
        }

        case NI_VectorT128_op_Multiply:
        {
            assert(baseType == TYP_INT);
            assert(sig.paramCount == 2);

            if (compOpportunisticallyDependsOn(InstructionSet_SSE41))
            {
                return gtNewSimdAsHWIntrinsicNode(retType, NI_SSE41_MultiplyLow, baseType, simdSize, ops[0], ops[1]);
            }

            GenTree* dup[2];
            ops[0] =
                impCloneExpr(ops[0], &dup[0], clsHnd, CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.Multiply"));
            ops[1] =
                impCloneExpr(ops[1], &dup[1], clsHnd, CHECK_SPILL_ALL DEBUGARG("Clone op2 for Vector<T>.Multiply"));

            ops[0] = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_ShiftRightLogical128BitLane, baseType, simdSize,
                                                ops[0], gtNewIconNode(4, TYP_INT));
            ops[1] = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_ShiftRightLogical128BitLane, baseType, simdSize,
                                                ops[1], gtNewIconNode(4, TYP_INT));
            ops[1] = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_Multiply, TYP_ULONG, simdSize, ops[1], ops[0]);
            ops[1] = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_Shuffle, baseType, simdSize, ops[1],
                                                gtNewIconNode(SHUFFLE_XXZX, TYP_INT));
            ops[0] = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_Multiply, TYP_ULONG, simdSize, dup[0], dup[1]);
            ops[0] = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_Shuffle, baseType, simdSize, ops[0],
                                                gtNewIconNode(SHUFFLE_XXZX, TYP_INT));

            return gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_UnpackLow, baseType, simdSize, ops[0], ops[1]);
        }
#endif // TARGET_XARCH

#ifdef TARGET_ARM64
        case NI_VectorT128_Abs:
            assert(sig.paramCount == 1);
            assert(varTypeIsUnsigned(baseType));
            return ops[0];

        case NI_VectorT128_Max:
        case NI_VectorT128_Min:
        {
            assert((baseType == TYP_LONG) || (baseType == TYP_ULONG));
            assert(sig.paramCount == 2);

            intrinsic = (intrinsic == NI_VectorT128_Max) ? NI_VectorT128_GreaterThan : NI_VectorT128_LessThan;
            intrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, baseType);

            GenTree* dup[2];
            ops[0] = impCloneExpr(ops[0], &dup[0], clsHnd, CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.Max/Min"));
            ops[1] = impCloneExpr(ops[1], &dup[1], clsHnd, CHECK_SPILL_ALL DEBUGARG("Clone op2 for Vector<T>.Max/Min"));
            ops[0] = gtNewSimdAsHWIntrinsicNode(retType, intrinsic, baseType, simdSize, ops[0], ops[1]);
            return impSimdAsHWIntrinsicCndSel(clsHnd, retType, baseType, simdSize, ops[0], dup[0], dup[1]);
        }
#endif // TARGET_ARM64

        case NI_VectorT128_ConditionalSelect:
#ifdef TARGET_XARCH
        case NI_VectorT256_ConditionalSelect:
#endif
            assert(sig.paramCount == 3);
            return impSimdAsHWIntrinsicCndSel(clsHnd, retType, baseType, simdSize, ops[0], ops[1], ops[2]);

        default:
            break;
    }

    assert(!"Unexpected SimdAsHWIntrinsic");
    return nullptr;
}

GenTree* Compiler::impSimdAsHWIntrinsicCreate(NamedIntrinsic              intrinsic,
                                              CORINFO_CLASS_HANDLE        clsHnd,
                                              const HWIntrinsicSignature& sig,
                                              var_types                   baseType,
                                              unsigned                    simdSize,
                                              GenTree*                    newobjThis)
{
    assert(featureSIMD);
    assert(varTypeIsArithmetic(baseType));
    assert(simdSize != 0);
    assert(SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, baseType) == intrinsic);
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

            var_types simdType = getSIMDTypeForSize(simdSize);
            GenTree*  op2      = impPopArgForHWIntrinsic(sig.paramType[0], nullptr);
            GenTree*  op1      = impPopArgForHWIntrinsic(simdType, typGetObjLayout(clsHnd), true, newobjThis);

            return impAssignSIMDAddr(op1, gtNewSimdCreateBroadcastNode(simdType, op2, baseType, simdSize,
                                                                       /* isSimdAsHWIntrinsic */ true));
    }

    default:
        unreached();
}
}

//------------------------------------------------------------------------
// impSimdAsHWIntrinsicCndSel: Import a SIMD conditional select intrinsic
//
// Arguments:
//    clsHnd     -- class handle containing the intrinsic function.
//    retType    -- the return type of the intrinsic call
//    baseType   -- the base type of SIMD type of the intrinsic
//    simdSize   -- the size of the SIMD type of the intrinsic
//    op1        -- the first operand of the intrinsic
//    op2        -- the second operand of the intrinsic
//    op3        -- the third operand of the intrinsic
//
// Return Value:
//    The GT_HWINTRINSIC node representing the conditional select
//
GenTree* Compiler::impSimdAsHWIntrinsicCndSel(CORINFO_CLASS_HANDLE clsHnd,
                                              var_types            retType,
                                              var_types            baseType,
                                              unsigned             simdSize,
                                              GenTree*             op1,
                                              GenTree*             op2,
                                              GenTree*             op3)
{
    assert(featureSIMD);
    assert(retType != TYP_UNKNOWN);
    assert(varTypeIsArithmetic(baseType));
    assert(simdSize != 0);
    assert(varTypeIsSIMD(getSIMDTypeForSize(simdSize)));
    assert(op1 != nullptr);
    assert(op2 != nullptr);
    assert(op3 != nullptr);

#if defined(TARGET_XARCH)
    assert(compIsaSupportedDebugOnly(InstructionSet_SSE2));
    assert((simdSize != 32) || compIsaSupportedDebugOnly(InstructionSet_AVX2));

    NamedIntrinsic andIntrinsic  = MapVectorTIntrinsic(NI_VectorT128_op_BitwiseAnd, simdSize == 32);
    NamedIntrinsic andnIntrinsic = MapVectorTIntrinsic(NI_VectorT128_AndNot, simdSize == 32);
    NamedIntrinsic orIntrinsic   = MapVectorTIntrinsic(NI_VectorT128_op_BitwiseOr, simdSize == 32);

    assert(SimdAsHWIntrinsicInfo::NeedsOperandsSwapped(andnIntrinsic));

    andIntrinsic  = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(andIntrinsic, baseType);
    andnIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(andnIntrinsic, baseType);
    orIntrinsic   = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(orIntrinsic, baseType);

    GenTree* op1Dup;
    op1 = impCloneExpr(op1, &op1Dup, clsHnd, CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.ConditionalSelect"));
    op2 = gtNewSimdAsHWIntrinsicNode(retType, andIntrinsic, baseType, simdSize, op2, op1);
    op3 = gtNewSimdAsHWIntrinsicNode(retType, andnIntrinsic, baseType, simdSize, op1Dup, op3);
    return gtNewSimdAsHWIntrinsicNode(retType, orIntrinsic, baseType, simdSize, op2, op3);
#elif defined(TARGET_ARM64)
    return gtNewSimdAsHWIntrinsicNode(retType, NI_AdvSimd_BitwiseSelect, baseType, simdSize, op1, op2, op3);
#else
#error Unsupported platform
#endif // !TARGET_XARCH && !TARGET_ARM64
}

#if defined(TARGET_XARCH)
//------------------------------------------------------------------------
// impSimdAsHWIntrinsicRelOp: Import a SIMD relational operator intrinsic
//
// Arguments:
//    intrinsic  -- id of the intrinsic function.
//    clsHnd     -- class handle containing the intrinsic function.
//    retType    -- the return type of the intrinsic call
//    baseType   -- the base type of SIMD type of the intrinsic
//    simdSize   -- the size of the SIMD type of the intrinsic
//    op1        -- the first operand of the intrinsic
//    op2        -- the second operand of the intrinsic
//
// Return Value:
//    The GT_HWINTRINSIC node representing the relational operator
//
GenTree* Compiler::impSimdAsHWIntrinsicRelOp(NamedIntrinsic       intrinsic,
                                             CORINFO_CLASS_HANDLE clsHnd,
                                             var_types            retType,
                                             var_types            baseType,
                                             unsigned             simdSize,
                                             GenTree*             op1,
                                             GenTree*             op2)
{
    assert(featureSIMD);
    assert(retType != TYP_UNKNOWN);
    assert(varTypeIsIntegral(baseType));
    assert(simdSize != 0);
    assert(varTypeIsSIMD(getSIMDTypeForSize(simdSize)));
    assert(op1 != nullptr);
    assert(op2 != nullptr);
    assert(!SimdAsHWIntrinsicInfo::IsInstanceMethod(intrinsic));

    bool isVectorT256 = (SimdAsHWIntrinsicInfo::lookupClassId(intrinsic) == SimdAsHWIntrinsicClassId::VectorT256);

    // Vector<T> for the rel-ops covered here requires at least SSE2
    assert(compIsaSupportedDebugOnly(InstructionSet_SSE2));

    // Vector<T>, when 32-bytes, requires at least AVX2
    assert(!isVectorT256 || compIsaSupportedDebugOnly(InstructionSet_AVX2));

    switch (intrinsic)
    {
        case NI_VectorT128_Equals:
        case NI_VectorT256_Equals:
        {
            // These ones aren't "special", but they are used by the other
            // relational operators and so are defined for convenience.

            NamedIntrinsic hwIntrinsic = NI_Illegal;

            if (isVectorT256 || ((baseType != TYP_LONG) && (baseType != TYP_ULONG)))
            {
                hwIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, baseType);
                assert(hwIntrinsic != intrinsic);
            }
            else if (compOpportunisticallyDependsOn(InstructionSet_SSE41))
            {
                hwIntrinsic = NI_SSE41_CompareEqual;
            }
            else
            {
                // There is no direct SSE2 support for comparing TYP_LONG vectors.
                // These have to be implemented in terms of TYP_INT vector comparison operations.
                //
                // tmp = (op1 == op2) i.e. compare for equality as if op1 and op2 are Vector<int>
                // op1 = tmp
                // op2 = Shuffle(tmp, (2, 3, 0, 1))
                // result = BitwiseAnd(op1, op2)
                //
                // Shuffle is meant to swap the comparison results of low-32-bits and high 32-bits of
                // respective long elements.

                hwIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, TYP_INT);
                assert(hwIntrinsic != intrinsic);

                GenTree* tmp = gtNewSimdAsHWIntrinsicNode(retType, hwIntrinsic, TYP_INT, simdSize, op1, op2);

                tmp = impCloneExpr(tmp, &op1, clsHnd, CHECK_SPILL_ALL DEBUGARG("Clone tmp for Vector<T>.Equals"));

                op2 = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_Shuffle, TYP_INT, simdSize, tmp,
                                                 gtNewIconNode(SHUFFLE_ZWXY, TYP_INT));

                hwIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(NI_VectorT128_op_BitwiseAnd, baseType);
                assert(hwIntrinsic != NI_VectorT128_op_BitwiseAnd);
            }
            assert(hwIntrinsic != NI_Illegal);

            return gtNewSimdAsHWIntrinsicNode(retType, hwIntrinsic, baseType, simdSize, op1, op2);
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

            switch (intrinsic)
            {
                case NI_VectorT128_GreaterThanOrEqual:
                    intrinsic = NI_VectorT128_GreaterThan;
                    break;
                case NI_VectorT128_LessThanOrEqual:
                    intrinsic = NI_VectorT128_LessThan;
                    break;
                case NI_VectorT256_GreaterThanOrEqual:
                    intrinsic = NI_VectorT256_GreaterThan;
                    break;
                case NI_VectorT256_LessThanOrEqual:
                    intrinsic = NI_VectorT256_LessThan;
                    break;
                default:
                    unreached();
            }

            NamedIntrinsic eqIntrinsic = MapVectorTIntrinsic(NI_VectorT128_Equals, isVectorT256);

            GenTree* dup[2];
            op1 = impCloneExpr(op1, &dup[0], clsHnd,
                               CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.GreaterThanOrEqual/LessThanOrEqual"));
            op2 = impCloneExpr(op2, &dup[1], clsHnd,
                               CHECK_SPILL_ALL DEBUGARG("Clone op2 for Vector<T>.GreaterThanOrEqual/LessThanOrEqual"));
            op1 = impSimdAsHWIntrinsicRelOp(eqIntrinsic, clsHnd, retType, baseType, simdSize, op1, op2);
            op2 = impSimdAsHWIntrinsicRelOp(intrinsic, clsHnd, retType, baseType, simdSize, dup[0], dup[1]);

            intrinsic = MapVectorTIntrinsic(NI_VectorT128_op_BitwiseOr, isVectorT256);
            intrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, baseType);
            return gtNewSimdAsHWIntrinsicNode(retType, intrinsic, baseType, simdSize, op1, op2);
        }

        case NI_VectorT128_GreaterThan:
        case NI_VectorT128_LessThan:
        case NI_VectorT256_GreaterThan:
        case NI_VectorT256_LessThan:
        {
            NamedIntrinsic hwIntrinsic = NI_Illegal;

            if (varTypeIsUnsigned(baseType))
            {
                // Vector<byte>, Vector<ushort>, Vector<uint> and Vector<ulong>:
                // Hardware supports > for signed comparison. Therefore, to use it for
                // comparing unsigned numbers, we subtract a constant from both the
                // operands such that the result fits within the corresponding signed
                // type. The resulting signed numbers are compared using signed comparison.
                //
                // Vector<byte>: constant to be subtracted is 2^7
                // Vector<ushort> constant to be subtracted is 2^15
                // Vector<uint> constant to be subtracted is 2^31
                // Vector<ulong> constant to be subtracted is 2^63
                //
                // We need to treat op1 and op2 as signed for comparison purpose after
                // the transformation.

                GenTree*  constVal = nullptr;
                var_types opType   = baseType;

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
                constVector[0] = gtNewSimdCreateBroadcastNode(retType, constVal, constVal->GetType(), simdSize,
                                                              /* isSimdAsHWIntrinsic */ true);
                constVector[0] =
                    impCloneExpr(constVector[0], &constVector[1], clsHnd,
                                 CHECK_SPILL_ALL DEBUGARG("Clone constVector for Vector<T>.GreaterThan/LessThan"));

                NamedIntrinsic hwIntrinsic = isVectorT256 ? NI_AVX2_Subtract : NI_SSE2_Subtract;
                op1 = gtNewSimdAsHWIntrinsicNode(retType, hwIntrinsic, opType, simdSize, op1, constVector[0]);
                op2 = gtNewSimdAsHWIntrinsicNode(retType, hwIntrinsic, opType, simdSize, op2, constVector[1]);
            }

            // This should have been mutated by the above path
            assert(varTypeIsIntegral(baseType) && !varTypeIsUnsigned(baseType));

            if (isVectorT256 || (baseType != TYP_LONG))
            {
                hwIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(intrinsic, baseType);
                assert(hwIntrinsic != intrinsic);
            }
            else if (compOpportunisticallyDependsOn(InstructionSet_SSE42))
            {
                hwIntrinsic =
                    (intrinsic == NI_VectorT128_GreaterThan) ? NI_SSE42_CompareGreaterThan : NI_SSE42_CompareLessThan;
            }
            else
            {
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
                // t = (op1 > op2)                - 32-bit signed comparison
                // u = (op1Dup1 == op2Dup1)       - 32-bit equality comparison
                // v = (op1Dup2 > op2Dup2)        - 32-bit unsigned comparison
                //
                // op1 = Shuffle(t, (3, 3, 1, 1)) - This corresponds to (x1 > x2) in eq(1) above
                // v = Shuffle(v, (2, 2, 0, 0))   - This corresponds to (y1 > y2) in eq(1) above
                // u = Shuffle(u, (3, 3, 1, 1))   - This corresponds to (x1 == x2) in eq(1) above
                // op2 = BitwiseAnd(v, u)         - This corresponds to [(x1 == x2) && (y1 > y2)] in eq(1) above
                //
                // result = BitwiseOr(op1, op2)

                GenTree* dup[2][2];
                op1 = impCloneExpr(op1, &dup[0][0], clsHnd,
                                   CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.GreaterThan/LessThan"));
                dup[0][0] = impCloneExpr(dup[0][0], &dup[0][1], clsHnd,
                                         CHECK_SPILL_ALL DEBUGARG("Clone op1 for Vector<T>.GreaterThan/LessThan"));
                op2 = impCloneExpr(op2, &dup[1][0], clsHnd,
                                   CHECK_SPILL_ALL DEBUGARG("Clone op2 for Vector<T>.GreaterThan/LessThan"));
                dup[1][0] = impCloneExpr(dup[1][0], &dup[1][1], clsHnd,
                                         CHECK_SPILL_ALL DEBUGARG("Clone op2 Vector<T>.GreaterThan/LessThan"));

                GenTree* t = impSimdAsHWIntrinsicRelOp(intrinsic, clsHnd, retType, TYP_INT, simdSize, op1, op2);
                GenTree* u = impSimdAsHWIntrinsicRelOp(NI_VectorT128_Equals, clsHnd, retType, TYP_INT, simdSize,
                                                       dup[0][0], dup[1][0]);
                GenTree* v =
                    impSimdAsHWIntrinsicRelOp(intrinsic, clsHnd, retType, TYP_UINT, simdSize, dup[0][1], dup[1][1]);

                op1 = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_Shuffle, TYP_INT, simdSize, t,
                                                 gtNewIconNode(SHUFFLE_WWYY, TYP_INT));

                v = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_Shuffle, TYP_INT, simdSize, v,
                                               gtNewIconNode(SHUFFLE_ZZXX, TYP_INT));
                u = gtNewSimdAsHWIntrinsicNode(retType, NI_SSE2_Shuffle, TYP_INT, simdSize, u,
                                               gtNewIconNode(SHUFFLE_WWYY, TYP_INT));

                hwIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(NI_VectorT128_op_BitwiseAnd, baseType);
                op2         = gtNewSimdAsHWIntrinsicNode(retType, hwIntrinsic, baseType, simdSize, v, u);

                hwIntrinsic = SimdAsHWIntrinsicInfo::lookupHWIntrinsic(NI_VectorT128_op_BitwiseOr, baseType);
            }
            assert(hwIntrinsic != NI_Illegal);

            return gtNewSimdAsHWIntrinsicNode(retType, hwIntrinsic, baseType, simdSize, op1, op2);
        }

        default:
            assert(!"Unexpected SimdAsHWIntrinsic");
            return nullptr;
    }
}
#endif // TARGET_XARCH

#endif // FEATURE_HW_INTRINSICS
