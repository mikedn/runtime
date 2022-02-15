// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*****************************************************************************/
#ifndef _VARTYPE_H_
#define _VARTYPE_H_
/*****************************************************************************/
#include "error.h"

enum var_types_classification
{
    VTF_ANY = 0x0000,
    VTF_INT = 0x0001,
    VTF_UNS = 0x0002, // type is unsigned
    VTF_FLT = 0x0004,
    VTF_GCR = 0x0008, // type is GC ref
    VTF_BYR = 0x0010, // type is Byref
    VTF_I   = 0x0020, // is machine sized
    VTF_S   = 0x0040, // is a struct type
};

#include "vartypesdef.h"

/*****************************************************************************
 * C-style pointers are implemented as TYP_INT or TYP_LONG depending on the
 * platform
 */

#ifdef TARGET_64BIT
#define TYP_I_IMPL TYP_LONG
#define TYP_U_IMPL TYP_ULONG
#else
#define TYP_I_IMPL TYP_INT
#define TYP_U_IMPL TYP_UINT
#ifdef _PREFAST_
// We silence this in the 32-bit build because for portability, we like to have asserts like this:
// assert(op2->gtType == TYP_INT || op2->gtType == TYP_I_IMPL);
// This is obviously redundant for 32-bit builds, but we don't want to have ifdefs and different
// asserts just for 64-bit builds, so for now just silence the assert
#pragma warning(disable : 6287) // warning 6287: the left and right sub-expressions are identical
#endif                          //_PREFAST_
#endif

/*****************************************************************************/

const extern BYTE varTypeClassification[TYP_COUNT];

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

#ifdef FEATURE_SIMD
template <class T>
inline bool varTypeIsSIMD(T vt)
{
    switch (TypeGet(vt))
    {
        case TYP_SIMD8:
        case TYP_SIMD12:
        case TYP_SIMD16:
        case TYP_SIMD32:
            return true;
        default:
            return false;
    }
}
#else  // FEATURE_SIMD

// Always return false if FEATURE_SIMD is not enabled
template <class T>
inline bool varTypeIsSIMD(T vt)
{
    return false;
}
#endif // !FEATURE_SIMD

template <class T>
inline bool varTypeIsIntegral(T vt)
{
    return ((varTypeClassification[TypeGet(vt)] & (VTF_INT)) != 0);
}

template <class T>
inline bool varTypeIsIntegralOrI(T vt)
{
    return ((varTypeClassification[TypeGet(vt)] & (VTF_INT | VTF_I)) != 0);
}

template <class T>
inline bool varTypeIsUnsigned(T vt)
{
    return ((varTypeClassification[TypeGet(vt)] & (VTF_UNS)) != 0);
}

template <class T>
inline bool varTypeIsSigned(T vt)
{
    return varTypeIsIntegralOrI(vt) && !varTypeIsUnsigned(vt);
}

inline var_types_classification varTypeKind(var_types type)
{
    return static_cast<var_types_classification>(varTypeClassification[type] &
                                                 (VTF_INT | VTF_FLT | VTF_GCR | VTF_BYR | VTF_S));
}

// If "vt" represents an unsigned integral type, returns the corresponding signed integral type,
// otherwise returns the original type.
template <class T>
inline var_types varTypeToSigned(T vt)
{
    var_types type = TypeGet(vt);
    if (varTypeIsUnsigned(type))
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
                unreached();
        }
    }

    return type;
}

// If "vt" represents a signed integral type, returns the corresponding unsigned integral type,
// otherwise returns the original type.
template <class T>
inline var_types varTypeToUnsigned(T vt)
{
    // Force signed types into corresponding unsigned type.
    var_types type = TypeGet(vt);
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

template <class T>
inline bool varTypeIsFloating(T vt)
{
    return ((varTypeClassification[TypeGet(vt)] & (VTF_FLT)) != 0);
}

template <class T>
inline bool varTypeIsArithmetic(T vt)
{
    return ((varTypeClassification[TypeGet(vt)] & (VTF_INT | VTF_FLT)) != 0);
}

template <class T>
inline unsigned varTypeGCtype(T vt)
{
    return (unsigned)(varTypeClassification[TypeGet(vt)] & (VTF_GCR | VTF_BYR));
}

template <class T>
inline bool varTypeIsGC(T vt)
{
    return (varTypeGCtype(vt) != 0);
}

template <class T>
inline bool varTypeIsI(T vt)
{
    return ((varTypeClassification[TypeGet(vt)] & VTF_I) != 0);
}

template <class T>
inline bool varTypeIsByte(T vt)
{
    return (TypeGet(vt) >= TYP_BOOL) && (TypeGet(vt) <= TYP_UBYTE);
}

template <class T>
inline bool varTypeIsShort(T vt)
{
    return (TypeGet(vt) == TYP_SHORT) || (TypeGet(vt) == TYP_USHORT);
}

template <class T>
inline bool varTypeIsSmall(T vt)
{
    return (TypeGet(vt) >= TYP_BOOL) && (TypeGet(vt) <= TYP_USHORT);
}

template <class T>
inline bool varTypeIsSmallInt(T vt)
{
    return (TypeGet(vt) >= TYP_BYTE) && (TypeGet(vt) <= TYP_USHORT);
}

template <class T>
inline bool varTypeIsSmallSigned(T vt)
{
    return (TypeGet(vt) == TYP_BYTE) || (TypeGet(vt) == TYP_SHORT);
}

template <class T>
inline bool varTypeIsSmallUnsigned(T vt)
{
    return (TypeGet(vt) == TYP_BOOL) || (TypeGet(vt) == TYP_UBYTE) || (TypeGet(vt) == TYP_USHORT);
}

template <class T>
inline bool varTypeIsIntOrI(T vt)
{
    return ((TypeGet(vt) == TYP_INT)
#ifdef TARGET_64BIT
            || (TypeGet(vt) == TYP_I_IMPL)
#endif // TARGET_64BIT
                );
}

template <class T>
inline bool varActualTypeIsInt(T vt)
{
    return ((TypeGet(vt) >= TYP_BOOL) && (TypeGet(vt) <= TYP_UINT));
}

template <class T>
inline bool varActualTypeIsIntOrI(T vt)
{
    return ((TypeGet(vt) >= TYP_BOOL) && (TypeGet(vt) <= TYP_U_IMPL));
}

template <class T>
inline bool genActualTypeIsIntOrI(T vt)
{
    return varActualTypeIsIntOrI(vt);
}

constexpr bool varTypeIsInt(var_types t)
{
    return (t == TYP_INT) || (t == TYP_UINT);
}

template <class T>
inline bool varTypeIsLong(T vt)
{
    return (TypeGet(vt) >= TYP_LONG) && (TypeGet(vt) <= TYP_ULONG);
}

template <class T>
inline bool varTypeIsMultiReg(T vt)
{
#ifdef TARGET_64BIT
    return false;
#else
    return (TypeGet(vt) == TYP_LONG);
#endif
}

template <class T>
inline bool varTypeIsSingleReg(T vt)
{
    return !varTypeIsMultiReg(vt);
}

template <class T>
inline bool varTypeIsComposite(T vt)
{
    return (!varTypeIsArithmetic(TypeGet(vt)) && TypeGet(vt) != TYP_VOID);
}

template <class T>
inline bool varTypeIsStruct(T vt)
{
    return ((varTypeClassification[TypeGet(vt)] & VTF_S) != 0);
}

template <class T>
inline bool varTypeUsesFloatReg(T vt)
{
    // Note that not all targets support SIMD, but if they don't, varTypeIsSIMD will
    // always return false.
    return varTypeIsFloating(vt) || varTypeIsSIMD(vt);
}

template <class T>
inline bool varTypeUsesFloatArgReg(T vt)
{
#ifdef TARGET_ARM64
    // Arm64 passes SIMD types in floating point registers.
    return varTypeUsesFloatReg(vt);
#else
    // Other targets pass them as regular structs - by reference or by value.
    return varTypeIsFloating(vt);
#endif
}

extern const BYTE genTypeSizes[TYP_COUNT];

inline unsigned varTypeSize(var_types type)
{
    assert(type < _countof(genTypeSizes));
    return genTypeSizes[type];
}

inline unsigned varTypeBitSize(var_types type)
{
    return varTypeSize(type) * 8;
}

extern const BYTE genActualTypes[TYP_COUNT];

inline var_types varActualType(var_types type)
{
    // Spot check to make certain the table is in synch with the enum
    assert(genActualTypes[TYP_DOUBLE] == TYP_DOUBLE);
    assert(genActualTypes[TYP_REF] == TYP_REF);

    assert(static_cast<unsigned>(type) < _countof(genActualTypes));
    return static_cast<var_types>(genActualTypes[type]);
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

inline var_types varTypeAddrAdd(var_types type)
{
    return (type == TYP_REF) ? TYP_BYREF : type;
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

#endif // _VARTYPE_H_
