// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#include "_typeinfo.h"

const ti_types g_jit_types_map[] = {
#define DEF_TP(tn, nm, jitType, verType, sz, sze, asze, st, al, tf) verType,
#include "typelist.h"
#undef DEF_TP
};

const ti_types g_ti_types_map[CORINFO_TYPE_COUNT] = {
    // see the definition of enum CorInfoType in file inc/corinfo.h
    TI_ERROR,  // CORINFO_TYPE_UNDEF           = 0x0,
    TI_ERROR,  // CORINFO_TYPE_VOID            = 0x1,
    TI_BYTE,   // CORINFO_TYPE_BOOL            = 0x2,
    TI_SHORT,  // CORINFO_TYPE_CHAR            = 0x3,
    TI_BYTE,   // CORINFO_TYPE_BYTE            = 0x4,
    TI_BYTE,   // CORINFO_TYPE_UBYTE           = 0x5,
    TI_SHORT,  // CORINFO_TYPE_SHORT           = 0x6,
    TI_SHORT,  // CORINFO_TYPE_USHORT          = 0x7,
    TI_INT,    // CORINFO_TYPE_INT             = 0x8,
    TI_INT,    // CORINFO_TYPE_UINT            = 0x9,
    TI_LONG,   // CORINFO_TYPE_LONG            = 0xa,
    TI_LONG,   // CORINFO_TYPE_ULONG           = 0xb,
    TI_I_IMPL, // CORINFO_TYPE_NATIVEINT       = 0xc,
    TI_I_IMPL, // CORINFO_TYPE_NATIVEUINT      = 0xd,
    TI_FLOAT,  // CORINFO_TYPE_FLOAT           = 0xe,
    TI_DOUBLE, // CORINFO_TYPE_DOUBLE          = 0xf,
    TI_REF,    // CORINFO_TYPE_STRING          = 0x10,
    TI_ERROR,  // CORINFO_TYPE_PTR             = 0x11,
    TI_ERROR,  // CORINFO_TYPE_BYREF           = 0x12,
    TI_STRUCT, // CORINFO_TYPE_VALUECLASS      = 0x13,
    TI_REF,    // CORINFO_TYPE_CLASS           = 0x14,
    TI_STRUCT, // CORINFO_TYPE_REFANY          = 0x15,
    TI_REF,    // CORINFO_TYPE_VAR             = 0x16,
};

#ifdef DEBUG

BOOL Compiler::tiCompatibleWith(const typeInfo& child, const typeInfo& parent, bool normalisedForStack) const
{
    return typeInfo::tiCompatibleWith(info.compCompHnd, child, parent, normalisedForStack);
}

// Note that we specifically ignore the permanent byref here. The rationale is that
// the type system doesn't know about this (it's jit only), ie, signatures don't specify if
// a byref is safe, so they are fully equivalent for the jit, except for the RET instruction,
// instructions that load safe byrefs and the stack merging logic, which need to know about
// the bit
bool typeInfo::AreEquivalent(const typeInfo& li, const typeInfo& ti)
{
    DWORD allFlags =
        TI_FLAG_DATA_MASK | TI_FLAG_BYREF | TI_FLAG_BYREF_READONLY | TI_FLAG_GENERIC_TYPE_VAR | TI_FLAG_UNINIT_OBJREF;
#ifdef TARGET_64BIT
    allFlags |= TI_FLAG_NATIVE_INT;
#endif // TARGET_64BIT

    if ((li.m_flags & allFlags) != (ti.m_flags & allFlags))
    {
        return false;
    }

    unsigned type = li.m_flags & TI_FLAG_DATA_MASK;
    // TI_ERROR looks like it needs more than enum.  This optimises the success case a bit
    assert(TI_ERROR < TI_ONLY_ENUM);
    if (type > TI_ONLY_ENUM)
    {
        return true;
    }
    if (type == TI_ERROR)
    {
        return false; // TI_ERROR != TI_ERROR
    }
    assert(li.m_cls != NO_CLASS_HANDLE && ti.m_cls != NO_CLASS_HANDLE);
    return li.m_cls == ti.m_cls;
}

static BOOL tiCompatibleWithByRef(COMP_HANDLE CompHnd, const typeInfo& child, const typeInfo& parent)
{
    assert(parent.IsByRef());

    if (!child.IsByRef())
    {
        return FALSE;
    }

    if (child.IsReadonlyByRef() && !parent.IsReadonlyByRef())
    {
        return FALSE;
    }

    // Byrefs are compatible if the underlying types are equivalent
    typeInfo childTarget  = ::DereferenceByRef(child);
    typeInfo parentTarget = ::DereferenceByRef(parent);

    if (typeInfo::AreEquivalent(childTarget, parentTarget))
    {
        return TRUE;
    }

    // Make sure that both types have a valid m_cls
    if ((childTarget.IsType(TI_REF) || childTarget.IsType(TI_STRUCT)) &&
        (parentTarget.IsType(TI_REF) || parentTarget.IsType(TI_STRUCT)))
    {
        return CompHnd->areTypesEquivalent(childTarget.GetClassHandle(), parentTarget.GetClassHandle());
    }

    return FALSE;
}

/*****************************************************************************
 * Verify child is compatible with the template parent.  Basically, that
 * child is a "subclass" of parent -it can be substituted for parent
 * anywhere.  Note that if parent contains fancy flags, such as "uninitialized"
 * , "is this ptr", or  "has byref local/field" info, then child must also
 * contain those flags, otherwise FALSE will be returned !
 *
 * Rules for determining compatibility:
 *
 * If parent is a primitive type or value class, then child must be the
 * same primitive type or value class.  The exception is that the built in
 * value classes System/Boolean etc. are treated as synonyms for
 * TI_BYTE etc.
 *
 * If parent is a byref of a primitive type or value class, then child
 * must be a byref of the same (rules same as above case).
 *
 * Byrefs are compatible only with byrefs.
 *
 * If parent is an object, child must be a subclass of it, implement it
 * (if it is an interface), or be null.
 *
 * If parent is an array, child must be the same or subclassed array.
 *
 * If parent is a null objref, only null is compatible with it.
 *
 * If the "uninitialized", "by ref local/field", "this pointer" or other flags
 * are different, the items are incompatible.
 *
 * parent CANNOT be an undefined (dead) item.
 *
 */

BOOL typeInfo::tiCompatibleWith(COMP_HANDLE     CompHnd,
                                const typeInfo& child,
                                const typeInfo& parent,
                                bool            normalisedForStack)
{
    assert(child.IsDead() || !normalisedForStack || typeInfo::AreEquivalent(::NormaliseForStack(child), child));
    assert(parent.IsDead() || !normalisedForStack || typeInfo::AreEquivalent(::NormaliseForStack(parent), parent));

    if (typeInfo::AreEquivalent(child, parent))
    {
        return TRUE;
    }

    if (parent.IsUnboxedGenericTypeVar() || child.IsUnboxedGenericTypeVar())
    {
        return (FALSE); // need to have had child == parent
    }
    else if (parent.IsType(TI_REF))
    {
        // An uninitialized objRef is not compatible to initialized.
        if (child.IsUninitialisedObjRef() && !parent.IsUninitialisedObjRef())
        {
            return FALSE;
        }

        if (child.IsNullObjRef())
        { // NULL can be any reference type
            return TRUE;
        }
        if (!child.IsType(TI_REF))
        {
            return FALSE;
        }

        return CompHnd->canCast(child.m_cls, parent.m_cls);
    }
    else if (parent.IsType(TI_METHOD))
    {
        if (!child.IsType(TI_METHOD))
        {
            return FALSE;
        }

        // Right now we don't bother merging method handles
        return FALSE;
    }
    else if (parent.IsType(TI_STRUCT))
    {
        if (!child.IsType(TI_STRUCT))
        {
            return FALSE;
        }

        // Structures are compatible if they are equivalent
        return CompHnd->areTypesEquivalent(child.m_cls, parent.m_cls);
    }
    else if (parent.IsByRef())
    {
        return tiCompatibleWithByRef(CompHnd, child, parent);
    }
#ifdef TARGET_64BIT
    // On 64-bit targets we have precise representation for native int, so these rules
    // represent the fact that the ECMA spec permits the implicit conversion
    // between an int32 and a native int.
    else if (parent.IsType(TI_INT) && typeInfo::AreEquivalent(nativeInt(), child))
    {
        return TRUE;
    }
    else if (typeInfo::AreEquivalent(nativeInt(), parent) && child.IsType(TI_INT))
    {
        return TRUE;
    }
#endif // TARGET_64BIT
    return FALSE;
}

#endif // DEBUG
