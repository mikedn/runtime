// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "error.h"

enum VarTypeKind
{
    VTK_NONE     = 0x0000,
    VTK_INT      = 0x0001,
    VTK_UNSIGNED = 0x0002,
    VTK_FLOAT    = 0x0004,
    VTK_REF      = 0x0008,
    VTK_BYREF    = 0x0010,
    VTK_I        = 0x0020,
    VTK_STRUCT   = 0x0040,

    VTK_TYPE_MASK = VTK_INT | VTK_FLOAT | VTK_REF | VTK_BYREF | VTK_STRUCT,
    VTK_GC_MASK   = VTK_REF | VTK_BYREF,

#ifdef TARGET_64BIT
    VTK_I32 = VTK_NONE,
    VTK_I64 = VTK_I,
#else
    VTK_I32 = VTK_I,
    VTK_I64 = VTK_NONE,
#endif
};

inline constexpr uint8_t varTypeSizes[]{
#define DEF_TP(tn, nm, jitType, sz, sze, asze, al, tf) sz,
#include "typelist.h"
};

inline constexpr uint8_t varTypeAlignments[]{
#define DEF_TP(tn, nm, jitType, sz, sze, asze, al, tf) al,
#include "typelist.h"
};

inline constexpr uint8_t varTypeActualTypes[]{
#define DEF_TP(tn, nm, jitType, sz, sze, asze, al, tf) jitType,
#include "typelist.h"
};

inline constexpr uint8_t varTypeKinds[]{
#define DEF_TP(tn, nm, jitType, sz, sze, asze, al, tf) tf,
#include "typelist.h"
};

const char* varTypeName(var_types type);

constexpr unsigned varTypeSize(var_types type)
{
    assert(type < _countof(varTypeSizes));
    return varTypeSizes[type];
}

constexpr unsigned varTypeBitSize(var_types type)
{
    return varTypeSize(type) * 8;
}

constexpr unsigned varTypeAlignment(var_types type)
{
    assert(type < _countof(varTypeAlignments));
    return varTypeAlignments[type];
}

constexpr var_types varActualType(var_types type)
{
    assert(type < _countof(varTypeActualTypes));
    return static_cast<var_types>(varTypeActualTypes[type]);
}

constexpr bool varTypeIsByte(var_types vt)
{
    return (vt >= TYP_BOOL) && (vt <= TYP_UBYTE);
}

constexpr bool varTypeIsShort(var_types vt)
{
    return (vt == TYP_SHORT) || (vt == TYP_USHORT);
}

constexpr bool varTypeIsSmall(var_types vt)
{
    return (vt >= TYP_BOOL) && (vt <= TYP_USHORT);
}

constexpr bool varTypeIsSmallInt(var_types vt)
{
    return (vt >= TYP_BYTE) && (vt <= TYP_USHORT);
}

constexpr bool varTypeIsSmallSigned(var_types t)
{
    return (t == TYP_BYTE) || (t == TYP_SHORT);
}

constexpr bool varTypeIsSmallUnsigned(var_types t)
{
    return (t == TYP_BOOL) || (t == TYP_UBYTE) || (t == TYP_USHORT);
}

constexpr bool varActualTypeIsInt(var_types vt)
{
    return (vt >= TYP_BOOL) && (vt <= TYP_UINT);
}

constexpr bool varTypeIsIntegral(var_types vt)
{
    return (TYP_INT_MIN <= vt) && (vt <= TYP_INT_MAX);
}

constexpr bool varActualTypeIsIntOrI(var_types vt)
{
    return (vt >= TYP_BOOL) && (vt <= TYP_U_IMPL);
}

constexpr bool varTypeIsIntOrI(var_types t)
{
    return (t == TYP_INT) || (t == TYP_I_IMPL);
}

constexpr bool varTypeIsFloating(var_types vt)
{
    return (vt == TYP_FLOAT) || (vt == TYP_DOUBLE);
}

constexpr bool varTypeIsArithmetic(var_types t)
{
    return varTypeIsIntegral(t) || varTypeIsFloating(t);
}

constexpr bool varTypeIsGC(var_types vt)
{
    return (vt == TYP_REF) || (vt == TYP_BYREF);
}

constexpr bool varTypeIsIntegralOrI(var_types vt)
{
    return varTypeIsIntegral(vt) || varTypeIsGC(vt);
}

constexpr bool varTypeIsI(var_types vt)
{
    return (vt == TYP_I_IMPL) || varTypeIsGC(vt);
}

constexpr bool varTypeIsVecElt(var_types vt)
{
    return varTypeIsArithmetic(vt);
}

constexpr bool varTypeIsVec(var_types vt)
{
#ifdef FEATURE_SIMD
    return (TYP_VEC_MIN <= vt) && (vt <= TYP_VEC_MAX);
#else
    return false;
#endif
}

constexpr bool varTypeIsSIMD(var_types vt)
{
    return varTypeIsVec(vt);
}

constexpr bool varTypeIsTargetVec(var_types vt)
{
#ifdef FEATURE_SIMD
    return (vt == TYP_SIMD16)
#ifdef TARGET_XARCH
           || (vt == TYP_SIMD32)
#endif
#ifdef TARGET_ARM64
           || (vt == TYP_SIMD8)
#endif
        ;
#else
    return false;
#endif
}

constexpr bool varTypeIsNonTargetVec(var_types vt)
{
#ifdef FEATURE_SIMD
    return (vt == TYP_SIMD12)
#ifndef TARGET_ARM64
           || (vt == TYP_SIMD8)
#endif
        ;
#else
    return false;
#endif
}

inline var_types varTypeTargetVec(var_types vt)
{
#ifdef FEATURE_SIMD
    if (varTypeIsNonTargetVec(vt))
    {
        return TYP_SIMD16;
    }
#endif

    assert(varTypeIsTargetVec(vt));

    return vt;
}

inline unsigned varTypeTargetVecSize(var_types vt)
{
    return varTypeSize(varTypeTargetVec(vt));
}

constexpr bool varTypeIsStruct(var_types vt)
{
    return (vt == TYP_STRUCT) || varTypeIsVec(vt);
}

constexpr bool varTypeIsComposite(var_types t)
{
    return varTypeIsStruct(t) || varTypeIsGC(t);
}

constexpr VarTypeKind varTypeKind(var_types type)
{
    return static_cast<VarTypeKind>(varTypeKinds[type] & VTK_TYPE_MASK);
}

constexpr VarTypeKind varTypeGCKind(var_types type)
{
    return static_cast<VarTypeKind>(varTypeKinds[type] & VTK_GC_MASK);
}

constexpr bool varTypeIsMultiReg(var_types vt)
{
#ifdef TARGET_64BIT
    return false;
#else
    return vt == TYP_LONG;
#endif
}

constexpr bool varTypeIsSingleReg(var_types vt)
{
    return !varTypeIsMultiReg(vt);
}

constexpr bool varTypeUsesVecReg(var_types vt)
{
    return varTypeIsFloating(vt) || varTypeIsVec(vt);
}

constexpr bool varTypeUsesFloatReg(var_types vt)
{
    return varTypeUsesVecReg(vt);
}

constexpr bool varTypeUsesFloatArgReg(var_types vt)
{
#ifdef TARGET_ARM64
    return varTypeUsesVecReg(vt);
#else
    return varTypeIsFloating(vt);
#endif
}

constexpr var_types varConvType(var_types type)
{
    assert(varTypeIsSmall(type));
    return type == TYP_BOOL ? TYP_UBYTE : type;
}

constexpr var_types varTypeAddrAdd(var_types type)
{
    return type == TYP_REF ? TYP_BYREF : type;
}

constexpr var_types varTypeNodeType(var_types type)
{
    switch (type)
    {
    case TYP_UINT:
        return TYP_INT;
    case TYP_ULONG:
        return TYP_LONG;
    default:
        return type;
    }
}

constexpr var_types varTypeToSigned(var_types type)
{
    switch (type)
    {
    case TYP_BOOL:
    case TYP_UBYTE:
        return TYP_BYTE;
    case TYP_USHORT:
        return TYP_SHORT;
    case TYP_UINT:
        return TYP_INT;
    case TYP_ULONG:
        return TYP_LONG;
    default:
        return type;
    }
}

constexpr var_types varTypeToSmallUnsigned(var_types type)
{
    switch (type)
    {
    case TYP_BYTE:
        return TYP_UBYTE;
    case TYP_SHORT:
        return TYP_USHORT;
    default:
        return type;
    }
}

constexpr var_types varTypeFromTypeNum(unsigned typeNum)
{
    return typeNum < TYP_COUNT ? static_cast<var_types>(typeNum) : TYP_STRUCT;
}

constexpr unsigned varTypeToTypeNum(var_types type, unsigned layoutNum)
{
    // TODO-MIKE-SSA: We may need to preserve the layout for SIMD types too,
    // otherwise we may run into problems during VN.
    assert((type != TYP_STRUCT) || (layoutNum != 0));
    return type == TYP_STRUCT ? layoutNum : static_cast<unsigned>(type);
}

constexpr bool varTypeIsValidLclType(var_types type)
{
    switch (type)
    {
    case TYP_UNDEF:
    case TYP_VOID:
    case TYP_UINT:
    case TYP_ULONG:
    case TYP_UNKNOWN:
        return false;
    default:
        return true;
    }
}

constexpr bool varTypeSmallIntCanRepresentValue(var_types type, ssize_t value)
{
    switch (type)
    {
    case TYP_UBYTE:
    case TYP_BOOL:
        return FitsIn<uint8_t>(value);
    case TYP_BYTE:
        return FitsIn<int8_t>(value);
    case TYP_USHORT:
        return FitsIn<uint16_t>(value);
    case TYP_SHORT:
        return FitsIn<int16_t>(value);
    default:
        unreached();
    }
}

#ifdef FEATURE_SIMD
constexpr int varTypeVecLength(unsigned vecSize, var_types eltType)
{
    return vecSize / varTypeSize(eltType);
}
#endif // FEATURE_SIMD
