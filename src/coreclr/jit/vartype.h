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

extern const uint8_t varTypeKinds[TYP_COUNT];
extern const uint8_t varTypeSizes[TYP_COUNT];
extern const uint8_t varTypeAlignments[TYP_COUNT];
extern const uint8_t varTypeActualTypes[TYP_COUNT];

const char* varTypeName(var_types type);

inline unsigned varTypeSize(var_types type)
{
    assert(type < _countof(varTypeSizes));
    return varTypeSizes[type];
}

inline unsigned varTypeBitSize(var_types type)
{
    return varTypeSize(type) * 8;
}

inline unsigned varTypeAlignment(var_types type)
{
    assert(type < _countof(varTypeAlignments));
    return varTypeAlignments[type];
}

inline var_types varActualType(var_types type)
{
    assert(type < _countof(varTypeActualTypes));
    return static_cast<var_types>(varTypeActualTypes[type]);
}

// make any class with a TypeGet member also have a function TypeGet() that does the same thing
template <class T>
inline var_types TypeGet(T* t)
{
    return t->TypeGet();
}

// make a TypeGet function which is the identity function for var_types
// the point of this and the preceding template is now you can make template functions
// that work on var_types as well as any object that exposes a TypeGet method.
// such as all of these varTypeIs* functions
inline var_types TypeGet(var_types v)
{
    return v;
}

inline bool varTypeIsByte(var_types vt)
{
    return (vt >= TYP_BOOL) && (vt <= TYP_UBYTE);
}

inline bool varTypeIsShort(var_types vt)
{
    return (vt == TYP_SHORT) || (vt == TYP_USHORT);
}

inline bool varTypeIsSmall(var_types vt)
{
    return (vt >= TYP_BOOL) && (vt <= TYP_USHORT);
}

inline bool varTypeIsSmallInt(var_types vt)
{
    return (vt >= TYP_BYTE) && (vt <= TYP_USHORT);
}

inline bool varTypeIsSmallSigned(var_types t)
{
    return (t == TYP_BYTE) || (t == TYP_SHORT);
}

inline bool varTypeIsSmallUnsigned(var_types t)
{
    return (t == TYP_BOOL) || (t == TYP_UBYTE) || (t == TYP_USHORT);
}

constexpr bool varTypeIsInt(var_types t)
{
    return (t == TYP_INT) || (t == TYP_UINT);
}

inline bool varActualTypeIsInt(var_types vt)
{
    return (vt >= TYP_BOOL) && (vt <= TYP_UINT);
}

inline bool varTypeIsLong(var_types vt)
{
    return (vt >= TYP_LONG) && (vt <= TYP_ULONG);
}

inline bool varTypeIsIntegral(var_types vt)
{
    return (varTypeKinds[vt] & VTK_INT) != 0;
}

inline bool varActualTypeIsIntOrI(var_types vt)
{
    return (vt >= TYP_BOOL) && (vt <= TYP_U_IMPL);
}

inline bool varTypeIsIntOrI(var_types t)
{
    return (t == TYP_INT) || (t == TYP_I_IMPL);
}

inline bool varTypeIsIntegralOrI(var_types vt)
{
    return (varTypeKinds[vt] & (VTK_INT | VTK_I)) != 0;
}

inline bool varTypeIsUnsigned(var_types vt)
{
    return (varTypeKinds[vt] & VTK_UNSIGNED) != 0;
}

inline bool varTypeIsSigned(var_types t)
{
    return varTypeIsIntegralOrI(t) && !varTypeIsUnsigned(t);
}

inline bool varTypeIsFloating(var_types vt)
{
    return (vt == TYP_FLOAT) || (vt == TYP_DOUBLE);
}

inline bool varTypeIsArithmetic(var_types t)
{
    return (varTypeKinds[t] & (VTK_INT | VTK_FLOAT)) != 0;
}

inline bool varTypeIsGC(var_types vt)
{
    return (vt == TYP_REF) || (vt == TYP_BYREF);
}

inline bool varTypeIsI(var_types vt)
{
    return (varTypeKinds[vt] & VTK_I) != 0;
}

inline bool varTypeIsSIMD(var_types vt)
{
    switch (vt)
    {
#ifdef FEATURE_SIMD
        case TYP_SIMD8:
        case TYP_SIMD12:
        case TYP_SIMD16:
        case TYP_SIMD32:
            return true;
#endif
        default:
            return false;
    }
}

inline bool varTypeIsStruct(var_types vt)
{
    return (varTypeKinds[vt] & VTK_STRUCT) != 0;
}

inline bool varTypeIsComposite(var_types t)
{
    return varTypeIsStruct(t) || varTypeIsGC(t);
}

inline VarTypeKind varTypeKind(var_types type)
{
    return static_cast<VarTypeKind>(varTypeKinds[type] & VTK_TYPE_MASK);
}

inline VarTypeKind varTypeGCKind(var_types type)
{
    return static_cast<VarTypeKind>(varTypeKinds[type] & VTK_GC_MASK);
}

inline bool varTypeIsMultiReg(var_types vt)
{
#ifdef TARGET_64BIT
    return false;
#else
    return vt == TYP_LONG;
#endif
}

inline bool varTypeIsSingleReg(var_types vt)
{
    return !varTypeIsMultiReg(vt);
}

inline bool varTypeUsesFloatReg(var_types vt)
{
    // Note that not all targets support SIMD, but if they don't, varTypeIsSIMD will
    // always return false.
    return varTypeIsFloating(vt) || varTypeIsSIMD(vt);
}

inline bool varTypeUsesFloatArgReg(var_types vt)
{
#ifdef TARGET_ARM64
    // Arm64 passes SIMD types in floating point registers.
    return varTypeUsesFloatReg(vt);
#else
    // Other targets pass them as regular structs - by reference or by value.
    return varTypeIsFloating(vt);
#endif
}

inline var_types varConvType(var_types type)
{
    assert(varTypeIsSmall(type));
    return type == TYP_BOOL ? TYP_UBYTE : type;
}

inline var_types varTypeAddrAdd(var_types type)
{
    return type == TYP_REF ? TYP_BYREF : type;
}

inline var_types varTypeNodeType(var_types type)
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

inline var_types varTypeToSigned(var_types type)
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

inline var_types varTypeToUnsigned(var_types type)
{
    switch (type)
    {
        case TYP_BYTE:
            return TYP_UBYTE;
        case TYP_SHORT:
            return TYP_USHORT;
        case TYP_INT:
            return TYP_UINT;
        case TYP_LONG:
            return TYP_ULONG;
        default:
            return type;
    }
}

inline var_types varTypeFromTypeNum(unsigned typeNum)
{
    return typeNum < TYP_COUNT ? static_cast<var_types>(typeNum) : TYP_STRUCT;
}

inline unsigned varTypeToTypeNum(var_types type, unsigned layoutNum)
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

inline bool varTypeSmallIntCanRepresentValue(var_types type, ssize_t value)
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
constexpr var_types getSIMDTypeForSize(unsigned size)
{
    switch (size)
    {
        case 8:
            return TYP_SIMD8;
        case 12:
            return TYP_SIMD12;
        case 16:
            return TYP_SIMD16;
        case 32:
            return TYP_SIMD32;
        default:
            unreached();
    }
}

inline int getSIMDVectorLength(unsigned simdSize, var_types baseType)
{
    return simdSize / varTypeSize(baseType);
}
#endif // FEATURE_SIMD
