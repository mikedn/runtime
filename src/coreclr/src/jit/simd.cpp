// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

//
//   SIMD Support
//
// IMPORTANT NOTES AND CAVEATS:
//
// This implementation is preliminary, and may change dramatically.
//
// New JIT types, TYP_SIMDxx, are introduced, and the SIMD intrinsics are created as GT_SIMD nodes.
// Nodes of SIMD types will be typed as TYP_SIMD* (e.g. TYP_SIMD8, TYP_SIMD16, etc.).
//
// Note that currently the "reference implementation" is the same as the runtime dll.  As such, it is currently
// providing implementations for those methods not currently supported by the JIT as intrinsics.
//
// These are currently recognized using string compares, in order to provide an implementation in the JIT
// without taking a dependency on the VM.
// Furthermore, in the CTP, in order to limit the impact of doing these string compares
// against assembly names, we only look for the SIMDVector assembly if we are compiling a class constructor.  This
// makes it somewhat more "pay for play" but is a significant usability compromise.
// This has been addressed for RTM by doing the assembly recognition in the VM.
// --------------------------------------------------------------------------------------

#include "jitpch.h"
#include "simd.h"

#ifdef _MSC_VER
#pragma hdrstop
#endif

#ifdef FEATURE_SIMD

// Intrinsic Id to intrinsic info map
const SIMDIntrinsicInfo simdIntrinsicInfoArray[] = {
#define SIMD_INTRINSIC(mname, inst, id, retType, argCount, arg1, arg2, arg3, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)  \
    {SIMDIntrinsic##id, mname, inst, retType, argCount, arg1, arg2, arg3, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10},
#include "simdintrinsiclist.h"
};

//------------------------------------------------------------------------
// getSIMDVectorLength: Get the length (number of elements of base type) of
//                      SIMD Vector given its size and base (element) type.
//
// Arguments:
//    simdSize   - size of the SIMD vector
//    baseType   - type of the elements of the SIMD vector
//
// static
int Compiler::getSIMDVectorLength(unsigned simdSize, var_types baseType)
{
    return simdSize / genTypeSize(baseType);
}

//------------------------------------------------------------------------
// Get the length (number of elements of base type) of SIMD Vector given by typeHnd.
//
// Arguments:
//    typeHnd  - type handle of the SIMD vector
//
int Compiler::getSIMDVectorLength(CORINFO_CLASS_HANDLE typeHnd)
{
    unsigned  sizeBytes = 0;
    var_types baseType  = getBaseTypeAndSizeOfSIMDType(typeHnd, &sizeBytes);
    return getSIMDVectorLength(sizeBytes, baseType);
}

//------------------------------------------------------------------------
// Get the preferred alignment of SIMD vector type for better performance.
//
// Arguments:
//    typeHnd  - type handle of the SIMD vector
//
int Compiler::getSIMDTypeAlignment(var_types simdType)
{
    unsigned size = genTypeSize(simdType);

#ifdef TARGET_XARCH
    // Fixed length vectors have the following alignment preference
    // Vector2   = 8 byte alignment
    // Vector3/4 = 16-byte alignment

    // preferred alignment for SSE2 128-bit vectors is 16-bytes
    if (size == 8)
    {
        return 8;
    }
    else if (size <= 16)
    {
        assert((size == 12) || (size == 16));
        return 16;
    }
    else
    {
        assert(size == 32);
        return 32;
    }
#elif defined(TARGET_ARM64)
    // preferred alignment for 64-bit vectors is 8-bytes.
    // For everything else, 16-bytes.
    return (size == 8) ? 8 : 16;
#else
    assert(!"getSIMDTypeAlignment() unimplemented on target arch");
    unreached();
#endif
}

//----------------------------------------------------------------------------------
// Return the base type and size of SIMD vector type given its type handle.
//
// Arguments:
//    typeHnd   - The handle of the type we're interested in.
//    sizeBytes - out param
//
// Return Value:
//    base type of SIMD vector.
//    sizeBytes if non-null is set to size in bytes.
//
// Notes:
//    If the size of the struct is already known call structSizeMightRepresentSIMDType
//    to determine if this api needs to be called.
//
// TODO-Throughput: current implementation parses class name to find base type. Change
//         this when we implement  SIMD intrinsic identification for the final
//         product.
var_types Compiler::getBaseTypeAndSizeOfSIMDType(CORINFO_CLASS_HANDLE typeHnd, unsigned* sizeBytes /*= nullptr */)
{
    assert(supportSIMDTypes());

    if (m_simdHandleCache == nullptr)
    {
        if (impInlineInfo == nullptr)
        {
            m_simdHandleCache = new (this, CMK_Generic) SIMDHandlesCache();
        }
        else
        {
            // Steal the inliner compiler's cache (create it if not available).

            if (impInlineInfo->InlineRoot->m_simdHandleCache == nullptr)
            {
                impInlineInfo->InlineRoot->m_simdHandleCache = new (this, CMK_Generic) SIMDHandlesCache();
            }

            m_simdHandleCache = impInlineInfo->InlineRoot->m_simdHandleCache;
        }
    }

    if (typeHnd == nullptr)
    {
        return TYP_UNKNOWN;
    }

    // fast path search using cached type handles of important types
    var_types simdBaseType = TYP_UNKNOWN;
    unsigned  size         = 0;

    // TODO - Optimize SIMD type recognition by IntrinsicAttribute
    if (isSIMDClass(typeHnd))
    {
        // The most likely to be used type handles are looked up first followed by
        // less likely to be used type handles
        if (typeHnd == m_simdHandleCache->SIMDFloatHandle)
        {
            simdBaseType = TYP_FLOAT;
            size         = getSIMDVectorRegisterByteLength();
        }
        else if (typeHnd == m_simdHandleCache->SIMDIntHandle)
        {
            simdBaseType = TYP_INT;
            size         = getSIMDVectorRegisterByteLength();
        }
        else if (typeHnd == m_simdHandleCache->SIMDVector2Handle)
        {
            simdBaseType = TYP_FLOAT;
            size         = 2 * genTypeSize(TYP_FLOAT);
            assert(size == roundUp(info.compCompHnd->getClassSize(typeHnd), TARGET_POINTER_SIZE));
        }
        else if (typeHnd == m_simdHandleCache->SIMDVector3Handle)
        {
            simdBaseType = TYP_FLOAT;
            size         = 3 * genTypeSize(TYP_FLOAT);
            assert(size == info.compCompHnd->getClassSize(typeHnd));
        }
        else if (typeHnd == m_simdHandleCache->SIMDVector4Handle)
        {
            simdBaseType = TYP_FLOAT;
            size         = 4 * genTypeSize(TYP_FLOAT);
            assert(size == roundUp(info.compCompHnd->getClassSize(typeHnd), TARGET_POINTER_SIZE));
        }
        else if (typeHnd == m_simdHandleCache->SIMDVectorHandle)
        {
            size = getSIMDVectorRegisterByteLength();
        }
        else if (typeHnd == m_simdHandleCache->SIMDUShortHandle)
        {
            simdBaseType = TYP_USHORT;
            size         = getSIMDVectorRegisterByteLength();
        }
        else if (typeHnd == m_simdHandleCache->SIMDUByteHandle)
        {
            simdBaseType = TYP_UBYTE;
            size         = getSIMDVectorRegisterByteLength();
        }
        else if (typeHnd == m_simdHandleCache->SIMDDoubleHandle)
        {
            simdBaseType = TYP_DOUBLE;
            size         = getSIMDVectorRegisterByteLength();
        }
        else if (typeHnd == m_simdHandleCache->SIMDLongHandle)
        {
            simdBaseType = TYP_LONG;
            size         = getSIMDVectorRegisterByteLength();
        }
        else if (typeHnd == m_simdHandleCache->SIMDShortHandle)
        {
            simdBaseType = TYP_SHORT;
            size         = getSIMDVectorRegisterByteLength();
        }
        else if (typeHnd == m_simdHandleCache->SIMDByteHandle)
        {
            simdBaseType = TYP_BYTE;
            size         = getSIMDVectorRegisterByteLength();
        }
        else if (typeHnd == m_simdHandleCache->SIMDUIntHandle)
        {
            simdBaseType = TYP_UINT;
            size         = getSIMDVectorRegisterByteLength();
        }
        else if (typeHnd == m_simdHandleCache->SIMDULongHandle)
        {
            simdBaseType = TYP_ULONG;
            size         = getSIMDVectorRegisterByteLength();
        }

        // slow path search
        if (simdBaseType == TYP_UNKNOWN)
        {
            // Doesn't match with any of the cached type handles.
            // Obtain base type by parsing fully qualified class name.
            //
            // TODO-Throughput: implement product shipping solution to query base type.
            WCHAR  className[256] = {0};
            WCHAR* pbuf           = &className[0];
            int    len            = _countof(className);
            info.compCompHnd->appendClassName(&pbuf, &len, typeHnd, TRUE, FALSE, FALSE);
            noway_assert(pbuf < &className[256]);

            if (wcsncmp(className, W("System.Numerics."), 16) == 0)
            {
                if (wcsncmp(&(className[16]), W("Vector`1["), 9) == 0)
                {
                    size = getSIMDVectorRegisterByteLength();

                    if (wcsncmp(&(className[25]), W("System.Single"), 13) == 0)
                    {
                        m_simdHandleCache->SIMDFloatHandle = typeHnd;
                        simdBaseType                       = TYP_FLOAT;
                    }
                    else if (wcsncmp(&(className[25]), W("System.Int32"), 12) == 0)
                    {
                        m_simdHandleCache->SIMDIntHandle = typeHnd;
                        simdBaseType                     = TYP_INT;
                    }
                    else if (wcsncmp(&(className[25]), W("System.UInt16"), 13) == 0)
                    {
                        m_simdHandleCache->SIMDUShortHandle = typeHnd;
                        simdBaseType                        = TYP_USHORT;
                    }
                    else if (wcsncmp(&(className[25]), W("System.Byte"), 11) == 0)
                    {
                        m_simdHandleCache->SIMDUByteHandle = typeHnd;
                        simdBaseType                       = TYP_UBYTE;
                    }
                    else if (wcsncmp(&(className[25]), W("System.Double"), 13) == 0)
                    {
                        m_simdHandleCache->SIMDDoubleHandle = typeHnd;
                        simdBaseType                        = TYP_DOUBLE;
                    }
                    else if (wcsncmp(&(className[25]), W("System.Int64"), 12) == 0)
                    {
                        m_simdHandleCache->SIMDLongHandle = typeHnd;
                        simdBaseType                      = TYP_LONG;
                    }
                    else if (wcsncmp(&(className[25]), W("System.Int16"), 12) == 0)
                    {
                        m_simdHandleCache->SIMDShortHandle = typeHnd;
                        simdBaseType                       = TYP_SHORT;
                    }
                    else if (wcsncmp(&(className[25]), W("System.SByte"), 12) == 0)
                    {
                        m_simdHandleCache->SIMDByteHandle = typeHnd;
                        simdBaseType                      = TYP_BYTE;
                    }
                    else if (wcsncmp(&(className[25]), W("System.UInt32"), 13) == 0)
                    {
                        m_simdHandleCache->SIMDUIntHandle = typeHnd;
                        simdBaseType                      = TYP_UINT;
                    }
                    else if (wcsncmp(&(className[25]), W("System.UInt64"), 13) == 0)
                    {
                        m_simdHandleCache->SIMDULongHandle = typeHnd;
                        simdBaseType                       = TYP_ULONG;
                    }
                    else
                    {
                        JITDUMP("Unknown SIMD type %s\n", eeGetClassName(typeHnd));
                    }
                }
                else if (wcsncmp(&(className[16]), W("Vector2"), 8) == 0)
                {
                    m_simdHandleCache->SIMDVector2Handle = typeHnd;

                    simdBaseType = TYP_FLOAT;
                    size         = 2 * genTypeSize(TYP_FLOAT);
                    assert(size == roundUp(info.compCompHnd->getClassSize(typeHnd), TARGET_POINTER_SIZE));
                }
                else if (wcsncmp(&(className[16]), W("Vector3"), 8) == 0)
                {
                    m_simdHandleCache->SIMDVector3Handle = typeHnd;

                    simdBaseType = TYP_FLOAT;
                    size         = 3 * genTypeSize(TYP_FLOAT);
                    assert(size == info.compCompHnd->getClassSize(typeHnd));
                }
                else if (wcsncmp(&(className[16]), W("Vector4"), 8) == 0)
                {
                    m_simdHandleCache->SIMDVector4Handle = typeHnd;

                    simdBaseType = TYP_FLOAT;
                    size         = 4 * genTypeSize(TYP_FLOAT);
                    assert(size == roundUp(info.compCompHnd->getClassSize(typeHnd), TARGET_POINTER_SIZE));
                }
                else if (wcsncmp(&(className[16]), W("Vector"), 6) == 0)
                {
                    m_simdHandleCache->SIMDVectorHandle = typeHnd;
                    size                                = getSIMDVectorRegisterByteLength();
                }
                else
                {
                    JITDUMP("Unknown SIMD type %s\n", eeGetClassName(typeHnd));
                }
            }
        }
    }
#ifdef FEATURE_HW_INTRINSICS
    else if (isIntrinsicType(typeHnd))
    {
        const size_t Vector64SizeBytes  = 64 / 8;
        const size_t Vector128SizeBytes = 128 / 8;
        const size_t Vector256SizeBytes = 256 / 8;

#if defined(TARGET_XARCH)
        static_assert_no_msg(YMM_REGSIZE_BYTES == Vector256SizeBytes);
        static_assert_no_msg(XMM_REGSIZE_BYTES == Vector128SizeBytes);

        if (typeHnd == m_simdHandleCache->Vector256FloatHandle)
        {
            simdBaseType = TYP_FLOAT;
            size         = Vector256SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector256DoubleHandle)
        {
            simdBaseType = TYP_DOUBLE;
            size         = Vector256SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector256IntHandle)
        {
            simdBaseType = TYP_INT;
            size         = Vector256SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector256UIntHandle)
        {
            simdBaseType = TYP_UINT;
            size         = Vector256SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector256ShortHandle)
        {
            simdBaseType = TYP_SHORT;
            size         = Vector256SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector256UShortHandle)
        {
            simdBaseType = TYP_USHORT;
            size         = Vector256SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector256ByteHandle)
        {
            simdBaseType = TYP_BYTE;
            size         = Vector256SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector256UByteHandle)
        {
            simdBaseType = TYP_UBYTE;
            size         = Vector256SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector256LongHandle)
        {
            simdBaseType = TYP_LONG;
            size         = Vector256SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector256ULongHandle)
        {
            simdBaseType = TYP_ULONG;
            size         = Vector256SizeBytes;
        }
        else
#endif // defined(TARGET_XARCH)
            if (typeHnd == m_simdHandleCache->Vector128FloatHandle)
        {
            simdBaseType = TYP_FLOAT;
            size         = Vector128SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector128DoubleHandle)
        {
            simdBaseType = TYP_DOUBLE;
            size         = Vector128SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector128IntHandle)
        {
            simdBaseType = TYP_INT;
            size         = Vector128SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector128UIntHandle)
        {
            simdBaseType = TYP_UINT;
            size         = Vector128SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector128ShortHandle)
        {
            simdBaseType = TYP_SHORT;
            size         = Vector128SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector128UShortHandle)
        {
            simdBaseType = TYP_USHORT;
            size         = Vector128SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector128ByteHandle)
        {
            simdBaseType = TYP_BYTE;
            size         = Vector128SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector128UByteHandle)
        {
            simdBaseType = TYP_UBYTE;
            size         = Vector128SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector128LongHandle)
        {
            simdBaseType = TYP_LONG;
            size         = Vector128SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector128ULongHandle)
        {
            simdBaseType = TYP_ULONG;
            size         = Vector128SizeBytes;
        }
        else
#if defined(TARGET_ARM64)
            if (typeHnd == m_simdHandleCache->Vector64FloatHandle)
        {
            simdBaseType = TYP_FLOAT;
            size         = Vector64SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector64DoubleHandle)
        {
            simdBaseType = TYP_DOUBLE;
            size         = Vector64SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector64IntHandle)
        {
            simdBaseType = TYP_INT;
            size         = Vector64SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector64UIntHandle)
        {
            simdBaseType = TYP_UINT;
            size         = Vector64SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector64ShortHandle)
        {
            simdBaseType = TYP_SHORT;
            size         = Vector64SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector64UShortHandle)
        {
            simdBaseType = TYP_USHORT;
            size         = Vector64SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector64ByteHandle)
        {
            simdBaseType = TYP_BYTE;
            size         = Vector64SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector64UByteHandle)
        {
            simdBaseType = TYP_UBYTE;
            size         = Vector64SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector64LongHandle)
        {
            simdBaseType = TYP_LONG;
            size         = Vector64SizeBytes;
        }
        else if (typeHnd == m_simdHandleCache->Vector64ULongHandle)
        {
            simdBaseType = TYP_ULONG;
            size         = Vector64SizeBytes;
        }
#endif // defined(TARGET_ARM64)

        // slow path search
        if (simdBaseType == TYP_UNKNOWN)
        {
            // Doesn't match with any of the cached type handles.
            const char*          className   = getClassNameFromMetadata(typeHnd, nullptr);
            CORINFO_CLASS_HANDLE baseTypeHnd = getTypeInstantiationArgument(typeHnd, 0);

            if (baseTypeHnd != nullptr)
            {
                CorInfoType type = info.compCompHnd->getTypeForPrimitiveNumericClass(baseTypeHnd);

#if defined(TARGET_XARCH)
                if (strcmp(className, "Vector256`1") == 0)
                {
                    size = Vector256SizeBytes;
                    switch (type)
                    {
                        case CORINFO_TYPE_FLOAT:
                            m_simdHandleCache->Vector256FloatHandle = typeHnd;
                            simdBaseType                            = TYP_FLOAT;
                            break;
                        case CORINFO_TYPE_DOUBLE:
                            m_simdHandleCache->Vector256DoubleHandle = typeHnd;
                            simdBaseType                             = TYP_DOUBLE;
                            break;
                        case CORINFO_TYPE_INT:
                            m_simdHandleCache->Vector256IntHandle = typeHnd;
                            simdBaseType                          = TYP_INT;
                            break;
                        case CORINFO_TYPE_UINT:
                            m_simdHandleCache->Vector256UIntHandle = typeHnd;
                            simdBaseType                           = TYP_UINT;
                            break;
                        case CORINFO_TYPE_SHORT:
                            m_simdHandleCache->Vector256ShortHandle = typeHnd;
                            simdBaseType                            = TYP_SHORT;
                            break;
                        case CORINFO_TYPE_USHORT:
                            m_simdHandleCache->Vector256UShortHandle = typeHnd;
                            simdBaseType                             = TYP_USHORT;
                            break;
                        case CORINFO_TYPE_LONG:
                            m_simdHandleCache->Vector256LongHandle = typeHnd;
                            simdBaseType                           = TYP_LONG;
                            break;
                        case CORINFO_TYPE_ULONG:
                            m_simdHandleCache->Vector256ULongHandle = typeHnd;
                            simdBaseType                            = TYP_ULONG;
                            break;
                        case CORINFO_TYPE_UBYTE:
                            m_simdHandleCache->Vector256UByteHandle = typeHnd;
                            simdBaseType                            = TYP_UBYTE;
                            break;
                        case CORINFO_TYPE_BYTE:
                            m_simdHandleCache->Vector256ByteHandle = typeHnd;
                            simdBaseType                           = TYP_BYTE;
                            break;

                        default:
                            JITDUMP("Unknown HW SIMD type %s\n", eeGetClassName(typeHnd));
                    }
                }
                else
#endif // defined(TARGET_XARCH)
                    if (strcmp(className, "Vector128`1") == 0)
                {
                    size = Vector128SizeBytes;
                    switch (type)
                    {
                        case CORINFO_TYPE_FLOAT:
                            m_simdHandleCache->Vector128FloatHandle = typeHnd;
                            simdBaseType                            = TYP_FLOAT;
                            break;
                        case CORINFO_TYPE_DOUBLE:
                            m_simdHandleCache->Vector128DoubleHandle = typeHnd;
                            simdBaseType                             = TYP_DOUBLE;
                            break;
                        case CORINFO_TYPE_INT:
                            m_simdHandleCache->Vector128IntHandle = typeHnd;
                            simdBaseType                          = TYP_INT;
                            break;
                        case CORINFO_TYPE_UINT:
                            m_simdHandleCache->Vector128UIntHandle = typeHnd;
                            simdBaseType                           = TYP_UINT;
                            break;
                        case CORINFO_TYPE_SHORT:
                            m_simdHandleCache->Vector128ShortHandle = typeHnd;
                            simdBaseType                            = TYP_SHORT;
                            break;
                        case CORINFO_TYPE_USHORT:
                            m_simdHandleCache->Vector128UShortHandle = typeHnd;
                            simdBaseType                             = TYP_USHORT;
                            break;
                        case CORINFO_TYPE_LONG:
                            m_simdHandleCache->Vector128LongHandle = typeHnd;
                            simdBaseType                           = TYP_LONG;
                            break;
                        case CORINFO_TYPE_ULONG:
                            m_simdHandleCache->Vector128ULongHandle = typeHnd;
                            simdBaseType                            = TYP_ULONG;
                            break;
                        case CORINFO_TYPE_UBYTE:
                            m_simdHandleCache->Vector128UByteHandle = typeHnd;
                            simdBaseType                            = TYP_UBYTE;
                            break;
                        case CORINFO_TYPE_BYTE:
                            m_simdHandleCache->Vector128ByteHandle = typeHnd;
                            simdBaseType                           = TYP_BYTE;
                            break;

                        default:
                            JITDUMP("Unknown HW SIMD type %s\n", eeGetClassName(typeHnd));
                    }
                }
#if defined(TARGET_ARM64)
                else if (strcmp(className, "Vector64`1") == 0)
                {
                    size = Vector64SizeBytes;
                    switch (type)
                    {
                        case CORINFO_TYPE_FLOAT:
                            m_simdHandleCache->Vector64FloatHandle = typeHnd;
                            simdBaseType                           = TYP_FLOAT;
                            break;
                        case CORINFO_TYPE_DOUBLE:
                            m_simdHandleCache->Vector64DoubleHandle = typeHnd;
                            simdBaseType                            = TYP_DOUBLE;
                            break;
                        case CORINFO_TYPE_INT:
                            m_simdHandleCache->Vector64IntHandle = typeHnd;
                            simdBaseType                         = TYP_INT;
                            break;
                        case CORINFO_TYPE_UINT:
                            m_simdHandleCache->Vector64UIntHandle = typeHnd;
                            simdBaseType                          = TYP_UINT;
                            break;
                        case CORINFO_TYPE_SHORT:
                            m_simdHandleCache->Vector64ShortHandle = typeHnd;
                            simdBaseType                           = TYP_SHORT;
                            break;
                        case CORINFO_TYPE_USHORT:
                            m_simdHandleCache->Vector64UShortHandle = typeHnd;
                            simdBaseType                            = TYP_USHORT;
                            break;
                        case CORINFO_TYPE_LONG:
                            m_simdHandleCache->Vector64LongHandle = typeHnd;
                            simdBaseType                          = TYP_LONG;
                            break;
                        case CORINFO_TYPE_ULONG:
                            m_simdHandleCache->Vector64ULongHandle = typeHnd;
                            simdBaseType                           = TYP_ULONG;
                            break;
                        case CORINFO_TYPE_UBYTE:
                            m_simdHandleCache->Vector64UByteHandle = typeHnd;
                            simdBaseType                           = TYP_UBYTE;
                            break;
                        case CORINFO_TYPE_BYTE:
                            m_simdHandleCache->Vector64ByteHandle = typeHnd;
                            simdBaseType                          = TYP_BYTE;
                            break;

                        default:
                            JITDUMP("Unknown HW SIMD type %s\n", eeGetClassName(typeHnd));
                    }
                }
#endif // defined(TARGET_ARM64)
            }
        }

#if defined(TARGET_XARCH)
        // Even though Vector256 is TYP_SIMD32, if AVX isn't supported, then it must
        // be treated as a regular struct
        if (size == YMM_REGSIZE_BYTES && (simdBaseType != TYP_UNKNOWN) && !compExactlyDependsOn(InstructionSet_AVX))
        {
            simdBaseType = TYP_UNKNOWN;
        }
#endif // TARGET_XARCH
    }
#endif // FEATURE_HW_INTRINSICS

    if (sizeBytes != nullptr)
    {
        *sizeBytes = size;
    }

    if (simdBaseType != TYP_UNKNOWN)
    {
        setUsesSIMDTypes(true);
    }

    return simdBaseType;
}

//--------------------------------------------------------------------------------------
// getSIMDIntrinsicInfo: get SIMD intrinsic info given the method handle.
//
// Arguments:
//    inOutTypeHnd    - The handle of the type on which the method is invoked.  This is an in-out param.
//    methodHnd       - The handle of the method we're interested in.
//    sig             - method signature info
//    isNewObj        - whether this call represents a newboj constructor call
//    argCount        - argument count - out pram
//    baseType        - base type of the intrinsic - out param
//    sizeBytes       - size of SIMD vector type on which the method is invoked - out param
//
// Return Value:
//    SIMDIntrinsicInfo struct initialized corresponding to methodHnd.
//    Sets SIMDIntrinsicInfo.id to SIMDIntrinsicInvalid if methodHnd doesn't correspond
//    to any SIMD intrinsic.  Also, sets the out params inOutTypeHnd, argCount, baseType and
//    sizeBytes.
//
//    Note that VectorMath class doesn't have a base type and first argument of the method
//    determines the SIMD vector type on which intrinsic is invoked. In such a case inOutTypeHnd
//    is modified by this routine.
//
// TODO-Throughput: The current implementation is based on method name string parsing.
//         Although we now have type identification from the VM, the parsing of intrinsic names
//         could be made more efficient.
//
const SIMDIntrinsicInfo* Compiler::getSIMDIntrinsicInfo(CORINFO_CLASS_HANDLE* inOutTypeHnd,
                                                        CORINFO_METHOD_HANDLE methodHnd,
                                                        CORINFO_SIG_INFO*     sig,
                                                        bool                  isNewObj,
                                                        unsigned*             argCount,
                                                        var_types*            baseType,
                                                        unsigned*             sizeBytes)
{
    assert(featureSIMD);
    assert(baseType != nullptr);
    assert(sizeBytes != nullptr);

    // get baseType and size of the type
    CORINFO_CLASS_HANDLE typeHnd = *inOutTypeHnd;
    *baseType                    = getBaseTypeAndSizeOfSIMDType(typeHnd, sizeBytes);

    if (typeHnd == m_simdHandleCache->SIMDVectorHandle)
    {
        // All of the supported intrinsics on this static class take a first argument that's a vector,
        // which determines the baseType.
        // The exception is the IsHardwareAccelerated property, which is handled as a special case.
        assert(*baseType == TYP_UNKNOWN);
        if (sig->numArgs == 0)
        {
            const SIMDIntrinsicInfo* hwAccelIntrinsicInfo = &(simdIntrinsicInfoArray[SIMDIntrinsicHWAccel]);
            if ((strcmp(eeGetMethodName(methodHnd, nullptr), hwAccelIntrinsicInfo->methodName) == 0) &&
                JITtype2varType(sig->retType) == hwAccelIntrinsicInfo->retType)
            {
                // Sanity check
                assert(hwAccelIntrinsicInfo->argCount == 0 && hwAccelIntrinsicInfo->isInstMethod == false);
                return hwAccelIntrinsicInfo;
            }
            return nullptr;
        }
        else
        {
            typeHnd       = info.compCompHnd->getArgClass(sig, sig->args);
            *inOutTypeHnd = typeHnd;
            *baseType     = getBaseTypeAndSizeOfSIMDType(typeHnd, sizeBytes);
        }
    }

    if (*baseType == TYP_UNKNOWN)
    {
        JITDUMP("NOT a SIMD Intrinsic: unsupported baseType\n");
        return nullptr;
    }

    // account for implicit "this" arg
    *argCount = sig->numArgs;
    if (sig->hasThis())
    {
        *argCount += 1;
    }

    // Get the Intrinsic Id by parsing method name.
    //
    // TODO-Throughput: replace sequential search by binary search by arranging entries
    // sorted by method name.
    SIMDIntrinsicID intrinsicId = SIMDIntrinsicInvalid;
    const char*     methodName  = eeGetMethodName(methodHnd, nullptr);
    for (int i = SIMDIntrinsicNone + 1; i < SIMDIntrinsicInvalid; ++i)
    {
        if (strcmp(methodName, simdIntrinsicInfoArray[i].methodName) == 0)
        {
            // Found an entry for the method; further check whether it is one of
            // the supported base types.
            bool found = false;
            for (int j = 0; j < SIMD_INTRINSIC_MAX_BASETYPE_COUNT; ++j)
            {
                // Convention: if there are fewer base types supported than MAX_BASETYPE_COUNT,
                // the end of the list is marked by TYP_UNDEF.
                if (simdIntrinsicInfoArray[i].supportedBaseTypes[j] == TYP_UNDEF)
                {
                    break;
                }

                if (simdIntrinsicInfoArray[i].supportedBaseTypes[j] == *baseType)
                {
                    found = true;
                    break;
                }
            }

            if (!found)
            {
                continue;
            }

            // Now, check the arguments.
            unsigned int fixedArgCnt    = simdIntrinsicInfoArray[i].argCount;
            unsigned int expectedArgCnt = fixedArgCnt;

            // First handle SIMDIntrinsicInitN, where the arg count depends on the type.
            // The listed arg types include the vector and the first two init values, which is the expected number
            // for Vector2.  For other cases, we'll check their types here.
            if (*argCount > expectedArgCnt)
            {
                if (i == SIMDIntrinsicInitN)
                {
                    if (*argCount == 3 && typeHnd == m_simdHandleCache->SIMDVector2Handle)
                    {
                        expectedArgCnt = 3;
                    }
                    else if (*argCount == 4 && typeHnd == m_simdHandleCache->SIMDVector3Handle)
                    {
                        expectedArgCnt = 4;
                    }
                    else if (*argCount == 5 && typeHnd == m_simdHandleCache->SIMDVector4Handle)
                    {
                        expectedArgCnt = 5;
                    }
                }
                else if (i == SIMDIntrinsicInitFixed)
                {
                    if (*argCount == 4 && typeHnd == m_simdHandleCache->SIMDVector4Handle)
                    {
                        expectedArgCnt = 4;
                    }
                }
            }
            if (*argCount != expectedArgCnt)
            {
                continue;
            }

            // Validate the types of individual args passed are what is expected of.
            // If any of the types don't match with what is expected, don't consider
            // as an intrinsic.  This will make an older JIT with SIMD capabilities
            // resilient to breaking changes to SIMD managed API.
            //
            // Note that from IL type stack, args get popped in right to left order
            // whereas args get listed in method signatures in left to right order.

            int stackIndex = (expectedArgCnt - 1);

            // Track the arguments from the signature - we currently only use this to distinguish
            // integral and pointer types, both of which will by TYP_I_IMPL on the importer stack.
            CORINFO_ARG_LIST_HANDLE argLst = sig->args;

            CORINFO_CLASS_HANDLE argClass;
            for (unsigned int argIndex = 0; found == true && argIndex < expectedArgCnt; argIndex++)
            {
                bool isThisPtr = ((argIndex == 0) && sig->hasThis());

                // In case of "newobj SIMDVector<T>(T val)", thisPtr won't be present on type stack.
                // We don't check anything in that case.
                if (!isThisPtr || !isNewObj)
                {
                    GenTree*  arg     = impStackTop(stackIndex).val;
                    var_types argType = arg->TypeGet();

                    var_types expectedArgType;
                    if (argIndex < fixedArgCnt)
                    {
                        // Convention:
                        //   - intrinsicInfo.argType[i] == TYP_UNDEF - intrinsic doesn't have a valid arg at position i
                        //   - intrinsicInfo.argType[i] == TYP_UNKNOWN - arg type should be same as basetype
                        // Note that we pop the args off in reverse order.
                        expectedArgType = simdIntrinsicInfoArray[i].argType[argIndex];
                        assert(expectedArgType != TYP_UNDEF);
                        if (expectedArgType == TYP_UNKNOWN)
                        {
                            // The type of the argument will be genActualType(*baseType).
                            expectedArgType = genActualType(*baseType);
                            argType         = genActualType(argType);
                        }
                    }
                    else
                    {
                        expectedArgType = *baseType;
                    }

                    if (!isThisPtr && argType == TYP_I_IMPL)
                    {
                        // The reference implementation has a constructor that takes a pointer.
                        // We don't want to recognize that one.  This requires us to look at the CorInfoType
                        // in order to distinguish a signature with a pointer argument from one with an
                        // integer argument of pointer size, both of which will be TYP_I_IMPL on the stack.
                        // TODO-Review: This seems quite fragile.  We should consider beefing up the checking
                        // here.
                        CorInfoType corType = strip(info.compCompHnd->getArgType(sig, argLst, &argClass));
                        if (corType == CORINFO_TYPE_PTR)
                        {
                            found = false;
                        }
                    }

                    if (varTypeIsSIMD(argType))
                    {
                        argType = TYP_STRUCT;
                    }
                    if (argType != expectedArgType)
                    {
                        found = false;
                    }
                }
                if (argIndex != 0 || !sig->hasThis())
                {
                    argLst = info.compCompHnd->getArgNext(argLst);
                }
                stackIndex--;
            }

            // Cross check return type and static vs. instance is what we are expecting.
            // If not, don't consider it as an intrinsic.
            // Note that ret type of TYP_UNKNOWN means that it is not known apriori and must be same as baseType
            if (found)
            {
                var_types expectedRetType = simdIntrinsicInfoArray[i].retType;
                if (expectedRetType == TYP_UNKNOWN)
                {
                    // JIT maps uint/ulong type vars to TYP_INT/TYP_LONG.
                    expectedRetType =
                        (*baseType == TYP_UINT || *baseType == TYP_ULONG) ? genActualType(*baseType) : *baseType;
                }

                if (JITtype2varType(sig->retType) != expectedRetType ||
                    sig->hasThis() != simdIntrinsicInfoArray[i].isInstMethod)
                {
                    found = false;
                }
            }

            if (found)
            {
                intrinsicId = (SIMDIntrinsicID)i;
                break;
            }
        }
    }

    if (intrinsicId != SIMDIntrinsicInvalid)
    {
        JITDUMP("Method %s maps to SIMD intrinsic %s\n", methodName, simdIntrinsicNames[intrinsicId]);
        return &simdIntrinsicInfoArray[intrinsicId];
    }
    else
    {
        JITDUMP("Method %s is NOT a SIMD intrinsic\n", methodName);
    }

    return nullptr;
}

/* static */ bool Compiler::vnEncodesResultTypeForSIMDIntrinsic(SIMDIntrinsicID intrinsicId)
{
    switch (intrinsicId)
    {
        case SIMDIntrinsicInit:
        case SIMDIntrinsicGetItem:
        case SIMDIntrinsicSub:
        case SIMDIntrinsicEqual:
        case SIMDIntrinsicBitwiseAnd:
        case SIMDIntrinsicBitwiseOr:
        case SIMDIntrinsicCast:
        case SIMDIntrinsicConvertToSingle:
        case SIMDIntrinsicConvertToDouble:
        case SIMDIntrinsicConvertToInt32:
        case SIMDIntrinsicConvertToInt64:
        case SIMDIntrinsicNarrow:
        case SIMDIntrinsicWidenHi:
        case SIMDIntrinsicWidenLo:
            return true;

        default:
            break;
    }
    return false;
}

// impSIMDPopStack: Pops and returns GenTree node from importer's type stack.
//
// Arguments:
//    type -  the type of value that the caller expects to be popped off the stack.
//
// Notes:
//    If the popped value is a struct, and the expected type is a simd type, it will be set
//    to that type, otherwise it will assert if the type being popped is not the expected type.
//
GenTree* Compiler::impSIMDPopStack(var_types type)
{
    assert(varTypeIsSIMD(type));

    GenTree* tree = impPopStack().val;

    if (tree->OperIs(GT_RET_EXPR, GT_CALL))
    {
        // TODO-MIKE-Cleanup: This is probably not needed when the SIMD type is returned in a register.

        ClassLayout* layout =
            tree->OperIs(GT_RET_EXPR) ? tree->AsRetExpr()->GetRetLayout() : tree->AsCall()->GetRetLayout();

        unsigned tmpNum = lvaGrabTemp(true DEBUGARG("struct address for call/obj"));
        impAssignTempGen(tmpNum, tree, layout->GetClassHandle(), CHECK_SPILL_ALL);
        tree = gtNewLclvNode(tmpNum, lvaGetDesc(tmpNum)->GetType());
    }

    // Now set the type of the tree to the specialized SIMD struct type, if applicable.
    if (tree->GetType() != type)
    {
        // TODO-MIKE-Cleanup: This is nonsense. If we get here it likely means that the IL is invalid.
        // SIMD intrinsics are calls as far as IL is concerned and it is not valid to pass a struct
        // argument to a call that has a different struct type than what's specified in the call
        // signature. Allowing this and retyping the tree has dubious consequences, such as indirs that
        // read garbage from memory and mistyped LCL_VAR nodes (having SIMD type when the local itself
        // has STRUCT type).
        //
        // Even the assert is nonsense, this should really use BADCODE. Otherwise in release builds
        // invalid IL will simply result in bad code being silently generated.

        assert(tree->TypeIs(TYP_STRUCT));
        tree->SetType(type);
    }

    return tree;
}

#ifdef TARGET_XARCH
// impSIMDLongRelOpEqual: transforms operands and returns the SIMD intrinsic to be applied on
// transformed operands to obtain == comparison result.
//
// Arguments:
//    typeHnd  -  type handle of SIMD vector
//    size     -  SIMD vector size
//    op1      -  in-out parameter; first operand
//    op2      -  in-out parameter; second operand
//
// Return Value:
//    Modifies in-out params op1, op2 and returns intrinsic ID to be applied to modified operands
//
SIMDIntrinsicID Compiler::impSIMDLongRelOpEqual(CORINFO_CLASS_HANDLE typeHnd,
                                                unsigned             size,
                                                GenTree**            pOp1,
                                                GenTree**            pOp2)
{
    var_types simdType = (*pOp1)->TypeGet();
    assert(varTypeIsSIMD(simdType) && ((*pOp2)->TypeGet() == simdType));

    // There is no direct SSE2 support for comparing TYP_LONG vectors.
    // These have to be implemented in terms of TYP_INT vector comparison operations.
    //
    // Equality(v1, v2):
    // tmp = (v1 == v2) i.e. compare for equality as if v1 and v2 are vector<int>
    // result = BitwiseAnd(t, shuffle(t, (2, 3, 0, 1)))
    // Shuffle is meant to swap the comparison results of low-32-bits and high 32-bits of respective long elements.

    // Compare vector<long> as if they were vector<int> and assign the result to a temp
    GenTree* compResult = gtNewSIMDNode(simdType, SIMDIntrinsicEqual, TYP_INT, size, *pOp1, *pOp2);
    unsigned lclNum     = lvaGrabTemp(true DEBUGARG("SIMD Long =="));
    lvaSetStruct(lclNum, typeHnd, false);
    GenTree* tmp = gtNewLclvNode(lclNum, simdType);
    GenTree* asg = gtNewTempAssign(lclNum, compResult);

    // op1 = GT_COMMA(tmp=compResult, tmp)
    // op2 = Shuffle(tmp, 0xB1)
    // IntrinsicId = BitwiseAnd
    *pOp1 = gtNewOperNode(GT_COMMA, simdType, asg, tmp);
    *pOp2 = gtNewSIMDNode(simdType, SIMDIntrinsicShuffleSSE2, TYP_INT, size, gtNewLclvNode(lclNum, simdType),
                          gtNewIconNode(SHUFFLE_ZWXY, TYP_INT));
    return SIMDIntrinsicBitwiseAnd;
}
#endif // TARGET_XARCH

// Transforms operands and returns the SIMD intrinsic to be applied on
// transformed operands to obtain given relop result.
//
// Arguments:
//    relOpIntrinsicId - Relational operator SIMD intrinsic
//    typeHnd          - type handle of SIMD vector
//    size             -  SIMD vector size
//    inOutBaseType    - base type of SIMD vector
//    pOp1             -  in-out parameter; first operand
//    pOp2             -  in-out parameter; second operand
//
// Return Value:
//    Modifies in-out params pOp1, pOp2, inOutBaseType and returns intrinsic ID to be applied to modified operands
//
SIMDIntrinsicID Compiler::impSIMDRelOp(SIMDIntrinsicID      relOpIntrinsicId,
                                       CORINFO_CLASS_HANDLE typeHnd,
                                       unsigned             size,
                                       var_types*           inOutBaseType,
                                       GenTree**            pOp1,
                                       GenTree**            pOp2)
{
    var_types simdType = (*pOp1)->TypeGet();
    assert(varTypeIsSIMD(simdType) && ((*pOp2)->TypeGet() == simdType));

    assert(isRelOpSIMDIntrinsic(relOpIntrinsicId));

    SIMDIntrinsicID intrinsicID = relOpIntrinsicId;
#ifdef TARGET_XARCH
    var_types baseType = *inOutBaseType;

    if (varTypeIsFloating(baseType))
    {
    }
    else if (varTypeIsIntegral(baseType))
    {
        if ((getSIMDSupportLevel() == SIMD_SSE2_Supported) && baseType == TYP_LONG)
        {
            // There is no direct SSE2 support for comparing TYP_LONG vectors.
            // These have to be implemented interms of TYP_INT vector comparison operations.
            if (intrinsicID == SIMDIntrinsicEqual)
            {
                intrinsicID = impSIMDLongRelOpEqual(typeHnd, size, pOp1, pOp2);
            }
            else
            {
                unreached();
            }
        }
        // SSE2 and AVX direct support for signed comparison of int32, int16 and int8 types
        else if (varTypeIsUnsigned(baseType))
        {
            // Vector<byte>, Vector<ushort>, Vector<uint> and Vector<ulong>:
            // SSE2 supports > for signed comparison. Therefore, to use it for
            // comparing unsigned numbers, we subtract a constant from both the
            // operands such that the result fits within the corresponding signed
            // type.  The resulting signed numbers are compared using SSE2 signed
            // comparison.
            //
            // Vector<byte>: constant to be subtracted is 2^7
            // Vector<ushort> constant to be subtracted is 2^15
            // Vector<uint> constant to be subtracted is 2^31
            // Vector<ulong> constant to be subtracted is 2^63
            //
            // We need to treat op1 and op2 as signed for comparison purpose after
            // the transformation.
            __int64 constVal = 0;
            switch (baseType)
            {
                case TYP_UBYTE:
                    constVal       = 0x80808080;
                    *inOutBaseType = TYP_BYTE;
                    break;
                case TYP_USHORT:
                    constVal       = 0x80008000;
                    *inOutBaseType = TYP_SHORT;
                    break;
                case TYP_UINT:
                    constVal       = 0x80000000;
                    *inOutBaseType = TYP_INT;
                    break;
                case TYP_ULONG:
                    constVal       = 0x8000000000000000LL;
                    *inOutBaseType = TYP_LONG;
                    break;
                default:
                    unreached();
                    break;
            }
            assert(constVal != 0);

            // This transformation is not required for equality.
            if (intrinsicID != SIMDIntrinsicEqual)
            {
                // For constructing const vector use either long or int base type.
                var_types tempBaseType;
                GenTree*  initVal;
                if (baseType == TYP_ULONG)
                {
                    tempBaseType = TYP_LONG;
                    initVal      = gtNewLconNode(constVal);
                }
                else
                {
                    tempBaseType = TYP_INT;
                    initVal      = gtNewIconNode((ssize_t)constVal);
                }
                initVal->gtType      = tempBaseType;
                GenTree* constVector = gtNewSIMDNode(simdType, SIMDIntrinsicInit, tempBaseType, size, initVal);

                // Assign constVector to a temp, since we intend to use it more than once
                // TODO-CQ: We have quite a few such constant vectors constructed during
                // the importation of SIMD intrinsics.  Make sure that we have a single
                // temp per distinct constant per method.
                GenTree* tmp = fgInsertCommaFormTemp(&constVector, typeHnd);

                // op1 = op1 - constVector
                // op2 = op2 - constVector
                *pOp1 = gtNewSIMDNode(simdType, SIMDIntrinsicSub, baseType, size, *pOp1, constVector);
                *pOp2 = gtNewSIMDNode(simdType, SIMDIntrinsicSub, baseType, size, *pOp2, tmp);
            }

            return impSIMDRelOp(intrinsicID, typeHnd, size, inOutBaseType, pOp1, pOp2);
        }
    }
#elif !defined(TARGET_ARM64)
    assert(!"impSIMDRelOp() unimplemented on target arch");
    unreached();
#endif // !TARGET_XARCH

    return intrinsicID;
}

//------------------------------------------------------------------------
// getOp1ForConstructor: Get the op1 for a constructor call.
//
// Arguments:
//    opcode     - the opcode being handled (needed to identify the CEE_NEWOBJ case)
//    newobjThis - For CEE_NEWOBJ, this is the temp grabbed for the allocated uninitalized object.
//    clsHnd    - The handle of the class of the method.
//
// Return Value:
//    The tree node representing the object to be initialized with the constructor.
//
// Notes:
//    This method handles the differences between the CEE_NEWOBJ and constructor cases.
//
GenTree* Compiler::getOp1ForConstructor(OPCODE opcode, GenTree* newobjThis, CORINFO_CLASS_HANDLE clsHnd)
{
    if (opcode == CEE_NEWOBJ)
    {
        assert(newobjThis->OperIs(GT_ADDR) && newobjThis->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR));

        // push newobj result on type stack
        unsigned tmpLclNum = newobjThis->AsUnOp()->GetOp(0)->AsLclVar()->GetLclNum();
        impPushOnStack(gtNewLclvNode(tmpLclNum, lvaGetRealType(tmpLclNum)), typeInfo(TI_STRUCT, clsHnd));

        return newobjThis;
    }

    return impPopStackCoerceArg(TYP_BYREF);
}

//-------------------------------------------------------------------
// Set the flag that indicates that the lclVar referenced by this tree
// is used in a SIMD intrinsic.
// Arguments:
//      tree - GenTree*

void Compiler::setLclRelatedToSIMDIntrinsic(GenTree* tree)
{
    assert(tree->OperIsLocal());
    unsigned   lclNum                = tree->AsLclVarCommon()->GetLclNum();
    LclVarDsc* lclVarDsc             = &lvaTable[lclNum];
    lclVarDsc->lvUsedInSIMDIntrinsic = true;
}

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
        while ((v1 != nullptr) && (v2 != nullptr) && (v1->GetOper() == v2->GetOper()))
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
                compiler->setLclRelatedToSIMDIntrinsic(location);
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

        addr   = compiler->gtNewOperNode(GT_COMMA, array->GetType(), arrBndsChk, array);
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

// Recognize a field of a SIMD local variable (Vector2/3/4 fields).
//
GenTreeLclVar* Compiler::SIMDCoalescingBuffer::IsSIMDField(GenTree* node)
{
    if (!node->OperIs(GT_FIELD))
    {
        return nullptr;
    }

    if (node->AsField()->IsVolatile())
    {
        // It probably doesn't make sense to coalesce volatile fields. Anyway LocalAddressVisitor
        // doesn't generate SIMDIntrinsicGetItem out of a volatile field and ChangeToSIMDMem does
        // not bother to make the indir it creates volatile...

        return nullptr;
    }

    if (node->AsField()->GetOffset() != m_index * varTypeSize(TYP_FLOAT))
    {
        return nullptr;
    }

    GenTree* addr = node->AsField()->GetAddr();

    if ((addr == nullptr) || !addr->OperIs(GT_ADDR) || !addr->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR))
    {
        return nullptr;
    }

    GenTreeLclVar* lclVar = addr->AsUnOp()->GetOp(0)->AsLclVar();

    if (!varTypeIsSIMD(lclVar->GetType()))
    {
        return nullptr;
    }

    assert(node->TypeIs(TYP_FLOAT));

    return lclVar;
}

// Recognize a SIMDIntrinsicGetItem that uses a SIMD local variable.
//
GenTreeLclVar* Compiler::SIMDCoalescingBuffer::IsSIMDGetItem(GenTree* node)
{
    if (!node->OperIs(GT_SIMD))
    {
        return nullptr;
    }

    GenTreeSIMD* getItem = node->AsSIMD();

    if ((getItem->GetIntrinsic() != SIMDIntrinsicGetItem) || !getItem->GetOp(0)->OperIs(GT_LCL_VAR) ||
        !getItem->GetOp(1)->IsIntegralConst(m_index))
    {
        return nullptr;
    }

    assert(getItem->GetSIMDBaseType() == TYP_FLOAT);
    assert(getItem->TypeIs(TYP_FLOAT));

    return getItem->GetOp(0)->AsLclVar();
};

// Try to add an assignment statement to the coalescing buffer (common code for Add and Mark).
// Return true if the statment is added and the number of statements in the buffer equals the number of SIMD elements.
//
bool Compiler::SIMDCoalescingBuffer::Add(Compiler* compiler, Statement* stmt, GenTreeOp* asg, GenTreeLclVar* simdLclVar)
{
    if (simdLclVar == nullptr)
    {
        Clear();
        return false;
    }

    if (m_index == 0)
    {
        m_firstStmt = stmt;
        m_lastStmt  = stmt;
        m_lclNum    = simdLclVar->GetLclNum();
        m_index++;
        return false;
    }

    if (simdLclVar->GetLclNum() != m_lclNum)
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

    return (m_index == varTypeSize(compiler->lvaGetDesc(simdLclVar)->GetType()) / varTypeSize(TYP_FLOAT));
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

    GenTreeLclVar* simdLclVar = IsSIMDField(asg->AsOp()->GetOp(1));

    if (!Add(compiler, stmt, asg->AsOp(), simdLclVar))
    {
        return;
    }

    compiler->setLclRelatedToSIMDIntrinsic(simdLclVar);

    GenTree* dest = asg->AsOp()->GetOp(0);

    if (GenTreeField* field = dest->IsField())
    {
        GenTree* addr = field->GetAddr();

        if ((addr != nullptr) && addr->OperIs(GT_ADDR) && addr->AsUnOp()->GetOp(0)->OperIsLocal())
        {
            compiler->setLclRelatedToSIMDIntrinsic(addr->AsUnOp()->GetOp(0));
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

    GenTreeLclVar* simdLclVar = IsSIMDGetItem(asg->AsOp()->GetOp(1));

    return Add(compiler, stmt, asg->AsOp(), simdLclVar);
}

// Transform the first assignment in the buffer into a SIMD assignment
// and remove the rest of the statements from the block.
//
void Compiler::SIMDCoalescingBuffer::Coalesce(Compiler* compiler, BasicBlock* block)
{
    assert(m_index > 1);

    GenTreeOp*   asg      = m_firstStmt->GetRootNode()->AsOp();
    GenTreeSIMD* getItem  = asg->GetOp(1)->AsSIMD();
    var_types    simdType = getSIMDTypeForSize(getItem->GetSIMDSize());

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("\nFound %u contiguous assignments from a %s local to memory in " FMT_BB ":\n", m_index,
               varTypeName(simdType), block->bbNum);
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

    asg->SetType(simdType);
    ChangeToSIMDMem(compiler, asg->GetOp(0), simdType);
    asg->SetOp(1, getItem->GetOp(0)->AsLclVar());

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("Changed to a single %s assignment:\n", varTypeName(simdType));
        compiler->gtDispStmt(m_firstStmt);
        printf("\n");
    }
#endif

    Clear();
}

//------------------------------------------------------------------------
// impSIMDIntrinsic: Check method to see if it is a SIMD method
//
// Arguments:
//    opcode     - the opcode being handled (needed to identify the CEE_NEWOBJ case)
//    newobjThis - For CEE_NEWOBJ, this is the temp grabbed for the allocated uninitalized object.
//    clsHnd     - The handle of the class of the method.
//    method     - The handle of the method.
//    sig        - The call signature for the method.
//    memberRef  - The memberRef token for the method reference.
//
// Return Value:
//    If clsHnd is a known SIMD type, and 'method' is one of the methods that are
//    implemented as an intrinsic in the JIT, then return the tree that implements
//    it.
//
GenTree* Compiler::impSIMDIntrinsic(OPCODE                opcode,
                                    GenTree*              newobjThis,
                                    CORINFO_CLASS_HANDLE  clsHnd,
                                    CORINFO_METHOD_HANDLE methodHnd,
                                    CORINFO_SIG_INFO*     sig,
                                    unsigned              methodFlags,
                                    int                   memberRef)
{
    assert(featureSIMD);

    // Exit early if we are not in one of the SIMD types.
    if (!isSIMDClass(clsHnd))
    {
        return nullptr;
    }

    // Exit early if the method is not a JIT Intrinsic (which requires the [Intrinsic] attribute).
    if ((methodFlags & CORINFO_FLG_JIT_INTRINSIC) == 0)
    {
        return nullptr;
    }

    // Get base type and intrinsic Id
    var_types                baseType = TYP_UNKNOWN;
    unsigned                 size     = 0;
    unsigned                 argCount = 0;
    const SIMDIntrinsicInfo* intrinsicInfo =
        getSIMDIntrinsicInfo(&clsHnd, methodHnd, sig, (opcode == CEE_NEWOBJ), &argCount, &baseType, &size);

    // Exit early if the intrinsic is invalid or unrecognized
    if ((intrinsicInfo == nullptr) || (intrinsicInfo->id == SIMDIntrinsicInvalid))
    {
        return nullptr;
    }

#if defined(TARGET_XARCH)
    CORINFO_InstructionSet minimumIsa = InstructionSet_SSE2;
#elif defined(TARGET_ARM64)
    CORINFO_InstructionSet minimumIsa = InstructionSet_AdvSimd;
#else
#error Unsupported platform
#endif // !TARGET_XARCH && !TARGET_ARM64

    if (!compOpportunisticallyDependsOn(minimumIsa) || !JitConfig.EnableHWIntrinsic())
    {
        // The user disabled support for the baseline ISA so
        // don't emit any SIMD intrinsics as they all require
        // this at a minimum. We will, however, return false
        // for IsHardwareAccelerated as that will help with
        // dead code elimination.

        return (intrinsicInfo->id == SIMDIntrinsicHWAccel) ? gtNewIconNode(0, TYP_INT) : nullptr;
    }

    SIMDIntrinsicID simdIntrinsicID = intrinsicInfo->id;
    var_types       simdType;
    if (baseType != TYP_UNKNOWN)
    {
        simdType = getSIMDTypeForSize(size);
    }
    else
    {
        assert(simdIntrinsicID == SIMDIntrinsicHWAccel);
        simdType = TYP_UNKNOWN;
    }
    bool      instMethod = intrinsicInfo->isInstMethod;
    var_types callType   = JITtype2varType(sig->retType);
    if (callType == TYP_STRUCT)
    {
        // Note that here we are assuming that, if the call returns a struct, that it is the same size as the
        // struct on which the method is declared. This is currently true for all methods on Vector types,
        // but if this ever changes, we will need to determine the callType from the signature.
        assert(info.compCompHnd->getClassSize(sig->retTypeClass) == genTypeSize(simdType));
        callType = simdType;
    }

    GenTree* simdTree = nullptr;
    GenTree* op1      = nullptr;
    GenTree* op2      = nullptr;
    GenTree* op3      = nullptr;
    GenTree* retVal   = nullptr;

    switch (simdIntrinsicID)
    {
        case SIMDIntrinsicInit:
        {
            // SIMDIntrinsicInit:
            //    op2 - the initializer value
            //    op1 - byref of vector
            op2 = impPopStackCoerceArg(varActualType(baseType));
            op1 = getOp1ForConstructor(opcode, newobjThis, clsHnd);

            assert(op1->TypeGet() == TYP_BYREF);
            assert(genActualType(op2->TypeGet()) == genActualType(baseType));

            if (op2->IsIntegralConst(0) || op2->IsDblConPositiveZero())
            {
                simdTree = gtNewSimdAsHWIntrinsicNode(simdType, NI_Vector128_get_Zero, baseType, size);
            }
            else if (varTypeIsSmallInt(baseType))
            {
                // For integral base types of size less than TYP_INT, expand the initializer
                // to fill size of TYP_INT bytes.

                unsigned baseSize = genTypeSize(baseType);
                int      multiplier;
                if (baseSize == 1)
                {
                    multiplier = 0x01010101;
                }
                else
                {
                    assert(baseSize == 2);
                    multiplier = 0x00010001;
                }

                GenTree* t1 = nullptr;
                if (baseType == TYP_BYTE)
                {
                    // What we have is a signed byte initializer,
                    // which when loaded to a reg will get sign extended to TYP_INT.
                    // But what we need is the initializer without sign extended or
                    // rather zero extended to 32-bits.
                    t1 = gtNewOperNode(GT_AND, TYP_INT, op2, gtNewIconNode(0xff, TYP_INT));
                }
                else if (baseType == TYP_SHORT)
                {
                    // What we have is a signed short initializer,
                    // which when loaded to a reg will get sign extended to TYP_INT.
                    // But what we need is the initializer without sign extended or
                    // rather zero extended to 32-bits.
                    t1 = gtNewOperNode(GT_AND, TYP_INT, op2, gtNewIconNode(0xffff, TYP_INT));
                }
                else
                {
                    assert(baseType == TYP_UBYTE || baseType == TYP_USHORT);
                    t1 = gtNewCastNode(TYP_INT, op2, false, TYP_INT);
                }

                assert(t1 != nullptr);
                GenTree* t2 = gtNewIconNode(multiplier, TYP_INT);
                op2         = gtNewOperNode(GT_MUL, TYP_INT, t1, t2);

                // Construct a vector of TYP_INT with the new initializer and cast it back to vector of baseType
                simdTree = gtNewSIMDNode(simdType, SIMDIntrinsicInit, TYP_INT, size, op2);
                simdTree = gtNewSIMDNode(simdType, SIMDIntrinsicCast, baseType, size, simdTree);
            }
            else
            {
                simdTree = gtNewSIMDNode(simdType, SIMDIntrinsicInit, baseType, size, op2);
            }

            retVal = impAssignSIMDAddr(op1, simdTree);
        }
        break;

        case SIMDIntrinsicInitN:
        {
            // SIMDIntrinsicInitN
            //    op2 - list of initializer values stitched into a list
            //    op1 - byref of vector
            assert(baseType == TYP_FLOAT);

            unsigned initCount    = argCount - 1;
            unsigned elementCount = getSIMDVectorLength(size, baseType);
            noway_assert(initCount == elementCount);

            // We must maintain left-to-right order of the args, but we will pop
            // them off in reverse order (the Nth arg was pushed onto the stack last).

            GenTree* args[SIMD_INTRINSIC_MAX_PARAM_COUNT - 1];

            bool areArgsContiguous = true;
            for (unsigned i = 0; i < initCount; i++)
            {
                args[initCount - 1 - i] = impPopStackCoerceArg(baseType);

                if (areArgsContiguous && (i > 0))
                {
                    // Recall that we are popping the args off the stack in reverse order.
                    areArgsContiguous = SIMDCoalescingBuffer::AreContiguousMemoryLocations(args[initCount - 1 - i],
                                                                                           args[initCount - 1 - i + 1]);
                }
            }

            op1 = getOp1ForConstructor(opcode, newobjThis, clsHnd);
            assert(op1->TypeGet() == TYP_BYREF);

            if (areArgsContiguous)
            {
                SIMDCoalescingBuffer::ChangeToSIMDMem(this, args[0], simdType);

                simdTree = args[0];

                if (op1->AsOp()->gtOp1->OperIsLocal())
                {
                    // label the dst struct's lclvar is used for SIMD intrinsic,
                    // so that this dst struct won't be promoted.
                    setLclRelatedToSIMDIntrinsic(op1->AsOp()->gtOp1);
                }
            }
            else
            {
                simdTree = gtNewSIMDNode(simdType, SIMDIntrinsicInitN, baseType, size, initCount, args);
            }

            retVal = impAssignSIMDAddr(op1, simdTree);
        }
        break;

        case SIMDIntrinsicInitArray:
        case SIMDIntrinsicInitArrayX:
        case SIMDIntrinsicCopyToArray:
        case SIMDIntrinsicCopyToArrayX:
        {
            // op3 - index into array in case of SIMDIntrinsicCopyToArrayX and SIMDIntrinsicInitArrayX
            // op2 - array itself
            // op1 - byref to vector struct

            unsigned int vectorLength = getSIMDVectorLength(size, baseType);
            // (This constructor takes only the zero-based arrays.)
            // We will add one or two bounds checks:
            // 1. If we have an index, we must do a check on that first.
            //    We can't combine it with the index + vectorLength check because
            //    a. It might be negative, and b. It may need to raise a different exception
            //    (captured as SCK_ARG_RNG_EXCPN for CopyTo and SCK_RNGCHK_FAIL for Init).
            // 2. We need to generate a check (SCK_ARG_EXCPN for CopyTo and SCK_RNGCHK_FAIL for Init)
            //    for the last array element we will access.
            //    We'll either check against (vectorLength - 1) or (index + vectorLength - 1).

            GenTree* checkIndexExpr = new (this, GT_CNS_INT) GenTreeIntCon(TYP_INT, vectorLength - 1);

            // Get the index into the array.  If it has been provided, it will be on the
            // top of the stack.  Otherwise, it is null.
            if (argCount == 3)
            {
                op3 = impPopStackCoerceArg(TYP_INT);

                if (op3->IsIntegralConst(0))
                {
                    op3 = nullptr;
                }
            }
            else
            {
                // TODO-CQ: Here, or elsewhere, check for the pattern where op2 is a newly constructed array, and
                // change this to the InitN form.
                // op3 = new (this, GT_CNS_INT) GenTreeIntCon(TYP_INT, 0);
                op3 = nullptr;
            }

            op2 = impPopStackCoerceArg(TYP_REF);

            // Clone the array for use in the bounds check.
            GenTree* arrayRefForArgChk = op2;
            GenTree* argRngChk         = nullptr;
            if ((arrayRefForArgChk->gtFlags & GTF_SIDE_EFFECT) != 0)
            {
                op2 = fgInsertCommaFormTemp(&arrayRefForArgChk);
            }
            else
            {
                op2 = gtCloneExpr(arrayRefForArgChk);
            }
            assert(op2 != nullptr);

            if (op3 != nullptr)
            {
                SpecialCodeKind op3CheckKind;
                if (simdIntrinsicID == SIMDIntrinsicInitArrayX)
                {
                    op3CheckKind = SCK_RNGCHK_FAIL;
                }
                else
                {
                    assert(simdIntrinsicID == SIMDIntrinsicCopyToArrayX);
                    op3CheckKind = SCK_ARG_RNG_EXCPN;
                }
                // We need to use the original expression on this, which is the first check.
                GenTree* arrayRefForArgRngChk = arrayRefForArgChk;
                // Then we clone the clone we just made for the next check.
                arrayRefForArgChk = gtCloneExpr(op2);
                // We know we MUST have had a cloneable expression.
                assert(arrayRefForArgChk != nullptr);
                GenTree* index = op3;
                if ((index->gtFlags & GTF_SIDE_EFFECT) != 0)
                {
                    op3 = fgInsertCommaFormTemp(&index);
                }
                else
                {
                    op3 = gtCloneExpr(index);
                }

                GenTreeArrLen* arrLen = gtNewArrLen(arrayRefForArgRngChk, OFFSETOF__CORINFO_Array__length, compCurBB);
                argRngChk             = new (this, GT_ARR_BOUNDS_CHECK)
                    GenTreeBoundsChk(GT_ARR_BOUNDS_CHECK, TYP_VOID, index, arrLen, op3CheckKind);
                // Now, clone op3 to create another node for the argChk
                GenTree* index2 = gtCloneExpr(op3);
                assert(index != nullptr);
                checkIndexExpr = gtNewOperNode(GT_ADD, TYP_INT, index2, checkIndexExpr);
            }

            // Insert a bounds check for index + offset - 1.
            // This must be a "normal" array.
            SpecialCodeKind op2CheckKind;
            if (simdIntrinsicID == SIMDIntrinsicInitArray || simdIntrinsicID == SIMDIntrinsicInitArrayX)
            {
                op2CheckKind = SCK_RNGCHK_FAIL;
            }
            else
            {
                op2CheckKind = SCK_ARG_EXCPN;
            }
            GenTreeArrLen*    arrLen = gtNewArrLen(arrayRefForArgChk, OFFSETOF__CORINFO_Array__length, compCurBB);
            GenTreeBoundsChk* argChk = new (this, GT_ARR_BOUNDS_CHECK)
                GenTreeBoundsChk(GT_ARR_BOUNDS_CHECK, TYP_VOID, checkIndexExpr, arrLen, op2CheckKind);

            // Create a GT_COMMA tree for the bounds check(s).
            op2 = gtNewOperNode(GT_COMMA, op2->TypeGet(), argChk, op2);
            if (argRngChk != nullptr)
            {
                op2 = gtNewOperNode(GT_COMMA, op2->TypeGet(), argRngChk, op2);
            }

            if (op3 == nullptr)
            {
                op3 = gtNewIconNode(OFFSETOF__CORINFO_Array__data, TYP_I_IMPL);
            }
            else
            {
#ifdef TARGET_64BIT
                op3 = gtNewCastNode(TYP_I_IMPL, op3, false, TYP_I_IMPL);
#endif
                op3 = gtNewOperNode(GT_MUL, TYP_I_IMPL, op3, gtNewIconNode(genTypeSize(baseType), TYP_I_IMPL));
                // TODO-MIKE-CQ: This should be removed, it's here only to minimize diffs
                // from the previous implementation that imported SIMDIntrinsicInitArray
                // as is, hiding the address mode and thus blocking CSE.
                op3->gtFlags |= GTF_DONT_CSE;
                op3 = gtNewOperNode(GT_ADD, TYP_I_IMPL, op3, gtNewIconNode(OFFSETOF__CORINFO_Array__data, TYP_I_IMPL));
                op3->gtFlags |= GTF_DONT_CSE;
            }

            op2 = gtNewOperNode(GT_ADD, TYP_BYREF, op2, op3);
            op2->gtFlags |= GTF_DONT_CSE;

            if ((simdIntrinsicID == SIMDIntrinsicInitArray) || (simdIntrinsicID == SIMDIntrinsicInitArrayX))
            {
                op1      = getOp1ForConstructor(opcode, newobjThis, clsHnd);
                simdTree = gtNewOperNode(GT_IND, simdType, op2);
                simdTree->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;
                retVal = impAssignSIMDAddr(op1, simdTree);
            }
            else
            {
                assert((simdIntrinsicID == SIMDIntrinsicCopyToArray) || (simdIntrinsicID == SIMDIntrinsicCopyToArrayX));
                assert(instMethod);

                op1 = impSIMDPopStackAddr(simdType);
                assert(op1->TypeGet() == simdType);
                retVal = gtNewOperNode(GT_IND, simdType, op2);
                retVal->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;
                retVal = gtNewAssignNode(retVal, op1);
            }
        }
        break;

        case SIMDIntrinsicInitFixed:
        {
            // We are initializing a fixed-length vector VLarge with a smaller fixed-length vector VSmall, plus 1 or 2
            // additional floats.
            //    op4 (optional) - float value for VLarge.W, if VLarge is Vector4, and VSmall is Vector2
            //    op3 - float value for VLarge.Z or VLarge.W
            //    op2 - VSmall
            //    op1 - byref of VLarge
            assert(baseType == TYP_FLOAT);

            GenTree* op4 = nullptr;
            if (argCount == 4)
            {
                op4 = impPopStackCoerceArg(TYP_FLOAT);
            }

            op3 = impPopStackCoerceArg(TYP_FLOAT);

            // The input vector will either be TYP_SIMD8 or TYP_SIMD12.
            var_types smallSIMDType = TYP_SIMD8;
            if ((op4 == nullptr) && (simdType == TYP_SIMD16))
            {
                smallSIMDType = TYP_SIMD12;
            }
            op2 = impSIMDPopStack(smallSIMDType);
            op1 = getOp1ForConstructor(opcode, newobjThis, clsHnd);

            // We are going to redefine the operands so that:
            // - op3 is the value that's going into the Z position, or null if it's a Vector4 constructor with a single
            // operand, and
            // - op4 is the W position value, or null if this is a Vector3 constructor.
            if (size == 16 && argCount == 3)
            {
                op4 = op3;
                op3 = nullptr;
            }

            simdTree = op2;
            if (op3 != nullptr)
            {
                simdTree = gtNewSIMDNode(simdType, SIMDIntrinsicSetZ, baseType, size, simdTree, op3);
            }
            if (op4 != nullptr)
            {
                simdTree = gtNewSIMDNode(simdType, SIMDIntrinsicSetW, baseType, size, simdTree, op4);
            }

            retVal = impAssignSIMDAddr(op1, simdTree);
        }
        break;

        case SIMDIntrinsicEqual:
        {
            assert(instMethod);
            op2 = impSIMDPopStack(simdType);
            op1 = impSIMDPopStackAddr(simdType);

            SIMDIntrinsicID intrinsicID = impSIMDRelOp(simdIntrinsicID, clsHnd, size, &baseType, &op1, &op2);
            simdTree                    = gtNewSIMDNode(genActualType(callType), intrinsicID, baseType, size, op1, op2);
            retVal                      = simdTree;
        }
        break;

        case SIMDIntrinsicSub:
        case SIMDIntrinsicBitwiseAnd:
        case SIMDIntrinsicBitwiseOr:
        {
            assert(!instMethod);
            // op1 is the first operand; if instance method, op1 is "this" arg
            // op2 is the second operand
            op2 = impSIMDPopStack(simdType);
            op1 = impSIMDPopStack(simdType);

            simdTree = gtNewSIMDNode(simdType, simdIntrinsicID, baseType, size, op1, op2);
            retVal   = simdTree;
        }
        break;

        case SIMDIntrinsicGetItem:
        {
            assert(instMethod);
            // op1 is a SIMD variable that is "this" arg
            // op2 is an index of TYP_INT
            op2              = impPopStackCoerceArg(TYP_INT);
            op1              = impSIMDPopStackAddr(simdType);
            int vectorLength = getSIMDVectorLength(size, baseType);
            if (!op2->IsCnsIntOrI() || op2->AsIntCon()->gtIconVal >= vectorLength || op2->AsIntCon()->gtIconVal < 0)
            {
                // We need to bounds-check the length of the vector.
                // For that purpose, we need to clone the index expression.
                GenTree* index = op2;
                if ((index->gtFlags & GTF_SIDE_EFFECT) != 0)
                {
                    op2 = fgInsertCommaFormTemp(&index);
                }
                else
                {
                    op2 = gtCloneExpr(index);
                }

                // For the non-constant case, we don't want to CSE the SIMD value, as we will just need to store
                // it to the stack to do the indexing anyway.
                op1->gtFlags |= GTF_DONT_CSE;

                GenTree*          lengthNode = new (this, GT_CNS_INT) GenTreeIntCon(TYP_INT, vectorLength);
                GenTreeBoundsChk* simdChk =
                    new (this, GT_SIMD_CHK) GenTreeBoundsChk(GT_SIMD_CHK, TYP_VOID, index, lengthNode, SCK_RNGCHK_FAIL);

                // Create a GT_COMMA tree for the bounds check.
                op2 = gtNewOperNode(GT_COMMA, op2->TypeGet(), simdChk, op2);
            }

            assert(op1->TypeGet() == simdType);
            assert(op2->TypeGet() == TYP_INT);

            simdTree = gtNewSIMDNode(genActualType(callType), simdIntrinsicID, baseType, size, op1, op2);
            retVal   = simdTree;
        }
        break;

        // Unary operators that take and return a Vector.
        case SIMDIntrinsicCast:
        case SIMDIntrinsicConvertToSingle:
        case SIMDIntrinsicConvertToDouble:
        case SIMDIntrinsicConvertToInt32:
        {
            assert(!instMethod);
            op1 = impSIMDPopStack(simdType);

            simdTree = gtNewSIMDNode(simdType, simdIntrinsicID, baseType, size, op1);
            retVal   = simdTree;
        }
        break;

        case SIMDIntrinsicConvertToInt64:
        {
            assert(!instMethod);
#ifdef TARGET_64BIT
            op1 = impSIMDPopStack(simdType);

            simdTree = gtNewSIMDNode(simdType, simdIntrinsicID, baseType, size, op1);
            retVal   = simdTree;
#else
            JITDUMP("SIMD Conversion to Int64 is not supported on this platform\n");
            return nullptr;
#endif
        }
        break;

        case SIMDIntrinsicNarrow:
        {
            assert(!instMethod);
            op2 = impSIMDPopStack(simdType);
            op1 = impSIMDPopStack(simdType);
            // op1 and op2 are two input Vector<T>.
            simdTree = gtNewSIMDNode(simdType, simdIntrinsicID, baseType, size, op1, op2);
            retVal   = simdTree;
        }
        break;

        case SIMDIntrinsicWiden:
        {
            GenTree* dstAddrHi = impPopStackCoerceArg(TYP_BYREF);
            GenTree* dstAddrLo = impPopStackCoerceArg(TYP_BYREF);
            op1                = impSIMDPopStack(simdType);
            GenTree* dupOp1    = fgInsertCommaFormTemp(&op1, clsHnd);

            GenTree* wideLo = gtNewSIMDNode(simdType, SIMDIntrinsicWidenLo, baseType, size, op1);
            GenTree* asgLo  = impAssignSIMDAddr(dstAddrLo, wideLo);
            GenTree* wideHi = gtNewSIMDNode(simdType, SIMDIntrinsicWidenHi, baseType, size, dupOp1);
            GenTree* asgHi  = impAssignSIMDAddr(dstAddrHi, wideHi);
            retVal          = gtNewOperNode(GT_COMMA, simdType, asgLo, asgHi);
        }
        break;

        case SIMDIntrinsicHWAccel:
        {
            GenTreeIntCon* intConstTree = new (this, GT_CNS_INT) GenTreeIntCon(TYP_INT, 1);
            retVal                      = intConstTree;
        }
        break;

        default:
            assert(!"Unimplemented SIMD Intrinsic");
            return nullptr;
    }

#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
    // XArch/Arm64: also indicate that we use floating point registers.
    // The need for setting this here is that a method may not have SIMD
    // type lclvars, but might be exercising SIMD intrinsics on fields of
    // SIMD type.
    //
    // e.g.  public Vector<float> ComplexVecFloat::sqabs() { return this.r * this.r + this.i * this.i; }
    compFloatingPointUsed = true;
#endif // defined(TARGET_XARCH) || defined(TARGET_ARM64)

    return retVal;
}

GenTreeOp* Compiler::impAssignSIMDAddr(GenTree* destAddr, GenTree* src)
{
    assert(destAddr->TypeIs(TYP_BYREF, TYP_I_IMPL));
    assert(src->OperIs(GT_SIMD, GT_IND, GT_HWINTRINSIC));
    assert(varTypeIsSIMD(src->GetType()));

    // TODO-MIKE-CQ: This should be removed, it's here only to minimize diffs
    // from the previous implementation that did this (in gtNewBlkOpNode).
    src->gtFlags |= GTF_DONT_CSE;

    GenTree* dest;

    if (destAddr->OperIs(GT_ADDR) && destAddr->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR) &&
        (destAddr->AsUnOp()->GetOp(0)->GetType() == src->GetType()))
    {
        dest = destAddr->AsUnOp()->GetOp(0);

        if (src->OperIs(GT_SIMD, GT_HWINTRINSIC))
        {
            setLclRelatedToSIMDIntrinsic(dest->AsLclVar());
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

#endif // FEATURE_SIMD
