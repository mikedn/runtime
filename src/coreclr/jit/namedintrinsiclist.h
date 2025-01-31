// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

// Named jit intrinsics

enum NamedIntrinsic : uint16_t
{
    NI_Illegal = 0,

    NI_CORINFO_INTRINSIC_START,
    NI_CORINFO_INTRINSIC_Array_Get,
    NI_CORINFO_INTRINSIC_Array_Address,
    NI_CORINFO_INTRINSIC_Array_Set,
    NI_CORINFO_INTRINSIC_InitializeArray,
    NI_CORINFO_INTRINSIC_RTH_GetValueInternal,
    NI_CORINFO_INTRINSIC_Object_GetType,
    NI_CORINFO_INTRINSIC_StubHelpers_GetStubContext,
    NI_CORINFO_INTRINSIC_StubHelpers_GetStubContextAddr,
    NI_CORINFO_INTRINSIC_StubHelpers_NextCallReturnAddress,
    NI_CORINFO_INTRINSIC_InterlockedAdd32,
    NI_CORINFO_INTRINSIC_InterlockedAdd64,
    NI_CORINFO_INTRINSIC_InterlockedXAdd32,
    NI_CORINFO_INTRINSIC_InterlockedXAdd64,
    NI_CORINFO_INTRINSIC_InterlockedXchg32,
    NI_CORINFO_INTRINSIC_InterlockedXchg64,
    NI_CORINFO_INTRINSIC_InterlockedCmpXchg32,
    NI_CORINFO_INTRINSIC_InterlockedCmpXchg64,
    NI_CORINFO_INTRINSIC_MemoryBarrier,
    NI_CORINFO_INTRINSIC_MemoryBarrierLoad,
    NI_CORINFO_INTRINSIC_ByReference_Ctor,
    NI_CORINFO_INTRINSIC_ByReference_Value,
    NI_CORINFO_INTRINSIC_GetRawHandle,
    NI_CORINFO_INTRINSIC_END,

    NI_System_Enum_HasFlag,

    NI_SYSTEM_MATH_START,
    NI_System_Math_Abs,
    NI_System_Math_Acos,
    NI_System_Math_Acosh,
    NI_System_Math_Asin,
    NI_System_Math_Asinh,
    NI_System_Math_Atan,
    NI_System_Math_Atanh,
    NI_System_Math_Atan2,
    NI_System_Math_Cbrt,
    NI_System_Math_Ceiling,
    NI_System_Math_Cos,
    NI_System_Math_Cosh,
    NI_System_Math_Exp,
    NI_System_Math_Floor,
    NI_System_Math_FMod,
    NI_System_Math_FusedMultiplyAdd,
    NI_System_Math_ILogB,
    NI_System_Math_Log,
    NI_System_Math_Log2,
    NI_System_Math_Log10,
    NI_System_Math_Pow,
    NI_System_Math_Round,
    NI_System_Math_Sin,
    NI_System_Math_Sinh,
    NI_System_Math_Sqrt,
    NI_System_Math_Tan,
    NI_System_Math_Tanh,
    NI_SYSTEM_MATH_END,

    NI_System_Collections_Generic_Comparer_get_Default,
    NI_System_Collections_Generic_EqualityComparer_get_Default,
    NI_System_Buffers_Binary_BinaryPrimitives_ReverseEndianness,
    NI_System_Numerics_BitOperations_PopCount,
    NI_System_GC_KeepAlive,
    NI_System_Threading_Thread_get_CurrentThread,
    NI_System_Threading_Thread_get_ManagedThreadId,
    NI_System_Type_get_IsValueType,
    NI_System_Type_IsAssignableFrom,
    NI_System_Type_IsAssignableTo,
    NI_System_Type_op_Equality,
    NI_System_Type_op_Inequality,
    NI_System_Type_GetTypeFromHandle,
    NI_System_Array_Clone,
    NI_System_Object_MemberwiseClone,

    NI_System_String_get_Chars,
    NI_System_String_get_Length,
    NI_System_Span_get_Item,
    NI_System_ReadOnlySpan_get_Item,

    // These are used by HWIntrinsics but are defined more generally
    // to allow dead code optimization and handle the recursion case

    NI_IsSupported_True,
    NI_IsSupported_False,
    NI_IsSupported_Dynamic,
    NI_Throw_PlatformNotSupportedException,

    NI_System_Threading_Interlocked_And,
    NI_System_Threading_Interlocked_Or,

#ifdef FEATURE_HW_INTRINSICS
    NI_HW_INTRINSIC_START,
#ifdef TARGET_XARCH
#define HARDWARE_INTRINSIC(isa, name, ...) NI_##isa##_##name,
#include "hwintrinsiclistxarch.h"
#endif

#ifdef TARGET_ARM64
#define HARDWARE_INTRINSIC(isa, name, ...) NI_##isa##_##name,
#include "hwintrinsiclistarm64.h"
#endif
    NI_HW_INTRINSIC_END,

    NI_SIMD_AS_HWINTRINSIC_START,
#ifdef TARGET_XARCH
#define SIMD_AS_HWINTRINSIC(classId, id, ...) NI_##classId##_##id,
#include "simdashwintrinsiclistxarch.h"
#endif

#ifdef TARGET_ARM64
#define SIMD_AS_HWINTRINSIC(classId, id, ...) NI_##classId##_##id,
#include "simdashwintrinsiclistarm64.h"
#endif
    NI_SIMD_AS_HWINTRINSIC_END,

    NI_HW_INTRINSIC_FIRST        = NI_HW_INTRINSIC_START + 1,
    NI_HW_INTRINSIC_LAST         = NI_HW_INTRINSIC_END - 1,
    NI_SIMD_AS_HWINTRINSIC_FIRST = NI_SIMD_AS_HWINTRINSIC_START + 1,
    NI_SIMD_AS_HWINTRINSIC_LAST  = NI_SIMD_AS_HWINTRINSIC_END - 1
#endif // FEATURE_HW_INTRINSICS
};
