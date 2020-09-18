// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*****************************************************************************
 This header file is named _typeInfo.h to be distinguished from typeinfo.h
 in the NT SDK
******************************************************************************/

#ifndef _TYPEINFO_H_
#define _TYPEINFO_H_

enum ti_types
{
    TI_ERROR,
    TI_REF,
    TI_STRUCT,
    TI_METHOD,
    TI_INT,
    TI_LONG,
    TI_DOUBLE,
    TI_NULL,
    TI_ONLY_ENUM = TI_METHOD, // Enum values with greater value are completely described by the enumeration.
};

#if defined(TARGET_64BIT)
#define TI_I_IMPL TI_LONG
#else
#define TI_I_IMPL TI_INT
#endif

extern const ti_types g_ti_types_map[CORINFO_TYPE_COUNT];

// Convert the type returned from the VM to a ti_type.

inline ti_types JITtype2tiType(CorInfoType type)
{
    // spot check to make certain enumerations have not changed

    assert(g_ti_types_map[CORINFO_TYPE_CLASS] == TI_REF);
    assert(g_ti_types_map[CORINFO_TYPE_BYREF] == TI_ERROR);
    assert(g_ti_types_map[CORINFO_TYPE_DOUBLE] == TI_DOUBLE);
    assert(g_ti_types_map[CORINFO_TYPE_VALUECLASS] == TI_STRUCT);
    assert(g_ti_types_map[CORINFO_TYPE_STRING] == TI_REF);

    type = CorInfoType(type & CORINFO_TYPE_MASK); // strip off modifiers

    assert(type < CORINFO_TYPE_COUNT);
    assert(g_ti_types_map[type] != TI_ERROR || type == CORINFO_TYPE_VOID);

    return g_ti_types_map[type];
};

/*****************************************************************************
 * Declares the typeInfo class, which represents the type of an entity on the
 * stack, in a local variable or an argument.
 *
 * Flags: LLLLLLLLLLLLLLLLffffffffffTTTTTT
 *
 * L = local var # or instance field #
 * x = unused
 * f = flags
 * T = type
 *
 * The lower bits are used to store the type component, and may be one of:
 *
 * TI_* (primitive)   - see tyelist.h for enumeration (BYTE, SHORT, INT..)
 * TI_REF             - OBJREF / ARRAY use m_cls for the type
 *                       (including arrays and null objref)
 * TI_STRUCT          - VALUE type, use m_cls for the actual type
 *
 * NOTE carefully that BYREF info is not stored here.  You will never see a
 * TI_BYREF in this component.  For example, the type component
 * of a "byref TI_INT" is TI_FLAG_BYREF | TI_INT.
 *
 * NOTE carefully that Generic Type Variable info is
 * only stored here in part.  Values of type "T" (e.g "!0" in ILASM syntax),
 * i.e. some generic variable type, appear only when verifying generic
 * code.  They come in two flavours: unboxed and boxed.  Unboxed
 * is the norm, e.g. a local, field or argument of type T.  Boxed
 * values arise from an IL instruction such as "box !0".
 * The EE provides type handles for each different type
 * variable and the EE's "canCast" operation decides casting
 * for boxed type variable. Thus:
 *
 *    (TI_REF, <type-variable-type-handle>) == boxed type variable
 *
 *    (TI_REF, <type-variable-type-handle>)
 *          + TI_FLAG_GENERIC_TYPE_VAR      == unboxed type variable
 *
 * Using TI_REF for these may seem odd but using TI_STRUCT means the
 * code-generation parts of the importer get confused when they
 * can't work out the size, GC-ness etc. of the "struct".  So using TI_REF
 * just tricks these backend parts into generating pseudo-trees for
 * the generic code we're verifying.  These trees then get thrown away
 * anyway as we do verification of generic code in import-only mode.
 *
 */

#define TI_FLAG_DATA_BITS 6
#define TI_FLAG_DATA_MASK ((1 << TI_FLAG_DATA_BITS) - 1)

// Flag indicating this item is a byref <something>

#define TI_FLAG_BYREF 0x00000080

// This item is a byref generated using the readonly. prefix
// to a ldelema or Address function on an array type.  The
// runtime type check is ignored in these cases, but the
// resulting byref can only be used in order to perform a
// constraint call.

#define TI_FLAG_BYREF_READONLY 0x00000100

// This item is the MSIL 'I' type which is pointer-sized
// (different size depending on platform) but which on ALL platforms
// is implicitly convertible with a 32-bit int but not with a 64-bit one.

// Note:  this flag is currently used only in 64-bit systems to annotate
// native int types.  In 32 bits, since you can transparently coalesce int32
// and native-int and both are the same size, JIT32 had no need to model
// native-ints as a separate entity.  For 64-bit though, since they have
// different size, it's important to discern between a long and a native int
// since conversions between them are not verifiable.
#define TI_FLAG_NATIVE_INT 0x00000200

// This item contains resolved token. It is used for ctor delegate optimization.
#define TI_FLAG_TOKEN 0x00000400

// This is for use when verifying generic code.
// This indicates that the type handle is really an unboxed
// generic type variable (e.g. the result of loading an argument
// of type T in a class List<T>).  Without this flag
// the same type handle indicates a boxed generic value,
// e.g. the result of a "box T" instruction.
#define TI_FLAG_GENERIC_TYPE_VAR 0x00004000

// Number of bits local var # is shifted

#define TI_FLAG_LOCAL_VAR_SHIFT 16
#define TI_FLAG_LOCAL_VAR_MASK 0xFFFF0000

// Field info uses the same space as the local info

#define TI_FLAG_FIELD_SHIFT TI_FLAG_LOCAL_VAR_SHIFT
#define TI_FLAG_FIELD_MASK TI_FLAG_LOCAL_VAR_MASK

#define TI_ALL_BYREF_FLAGS (TI_FLAG_BYREF | TI_FLAG_BYREF_READONLY)

/*****************************************************************************
 * A typeInfo can be one of several types:
 * - A primitive type (I4,I8,R4,R8,I)
 * - A type (ref, array, value type) (m_cls describes the type)
 * - An array (m_cls describes the array type)
 * - A byref (byref flag set, otherwise the same as the above),
 * - A Function Pointer (m_token)
 * - A byref local variable (byref and byref local flags set), can be
 *   uninitialized
 *
 * The reason that there can be 2 types of byrefs (general byrefs, and byref
 * locals) is that byref locals initially point to uninitialized items.
 * Therefore these byrefs must be tracked specially.
 */

class typeInfo
{
    unsigned m_flags;

    union {
        CORINFO_CLASS_HANDLE m_cls;
        // Valid only for TI_TOKEN with IsToken
        CORINFO_RESOLVED_TOKEN* m_token;
    };

    template <typename T>
    static bool isInvalidHandle(const T handle)
    {
        static_assert(std::is_same<T, CORINFO_CLASS_HANDLE>::value || std::is_same<T, CORINFO_METHOD_HANDLE>::value,
                      "");
#ifdef HOST_64BIT
        return handle == reinterpret_cast<T>(0xcccccccccccccccc);
#else
        return handle == reinterpret_cast<T>(0xcccccccc);
#endif
    }

    // Get this item's type
    // If primitive, returns the primitive type (TI_*)
    // If not primitive, returns:
    //  - TI_ERROR if a byref anything
    //  - TI_REF if a class or array or null or a generic type variable
    //  - TI_STRUCT if a value class
    ti_types GetType() const
    {
        if ((m_flags & TI_FLAG_BYREF) != 0)
        {
            return TI_ERROR;
        }

        // objref/array/null (objref), value class, ptr, primitive
        return static_cast<ti_types>(m_flags & TI_FLAG_DATA_MASK);
    }

    // Returns whether this is a primitive type (not a byref, objref,
    // array, null, value class, invalid value)
    BOOL IsPrimitiveType() const
    {
        unsigned type = GetType();

        // boolean, char, u1,u2 never appear on the operand stack
        return (type == TI_INT) || (type == TI_LONG) || (type == TI_DOUBLE);
    }

public:
    typeInfo() : m_flags(TI_ERROR), m_cls(NO_CLASS_HANDLE)
    {
    }

    typeInfo(ti_types tiType) : m_flags(static_cast<unsigned>(tiType)), m_cls(NO_CLASS_HANDLE)
    {
        assert((tiType >= TI_INT) && (tiType <= TI_NULL));
    }

    typeInfo(ti_types tiType, CORINFO_CLASS_HANDLE cls, bool typeVar = false)
        : m_flags(typeVar ? tiType | TI_FLAG_GENERIC_TYPE_VAR : tiType), m_cls(cls)
    {
        assert((tiType == TI_STRUCT) || (tiType == TI_REF));
        assert((cls != nullptr) && !isInvalidHandle(cls));
    }

    typeInfo(CORINFO_RESOLVED_TOKEN* token) : m_flags(TI_METHOD | TI_FLAG_TOKEN), m_token(token)
    {
        assert(token != nullptr);
        assert(token->hMethod != nullptr);
        assert(!isInvalidHandle(token->hMethod));
    }

    static typeInfo nativeInt()
    {
        typeInfo result = typeInfo(TI_I_IMPL);
#ifdef TARGET_64BIT
        result.m_flags |= TI_FLAG_NATIVE_INT;
#endif
        return result;
    }

    static bool AreEquivalent(const typeInfo& li, const typeInfo& ti);

#ifdef DEBUG
    static BOOL tiCompatibleWith(ICorJitInfo* vm, const typeInfo& child, const typeInfo& parent);
#endif // DEBUG

    /////////////////////////////////////////////////////////////////////////
    // Operations
    /////////////////////////////////////////////////////////////////////////

    void SetIsReadonlyByRef()
    {
        assert(IsByRef());
        m_flags |= TI_FLAG_BYREF_READONLY;
    }

    typeInfo& DereferenceByRef()
    {
        if (!IsByRef())
        {
            m_flags = TI_ERROR;
            INDEBUG(m_cls = NO_CLASS_HANDLE);
        }
        m_flags &= ~TI_ALL_BYREF_FLAGS;
        return *this;
    }

    typeInfo& MakeByRef()
    {
        assert(!IsByRef());
        m_flags |= TI_FLAG_BYREF;
        return *this;
    }

    /////////////////////////////////////////////////////////////////////////
    // Getters
    /////////////////////////////////////////////////////////////////////////

    CORINFO_CLASS_HANDLE GetClassHandle() const
    {
        return m_cls;
    }

    CORINFO_CLASS_HANDLE GetClassHandleForValueClass() const
    {
        assert(IsType(TI_STRUCT));
        assert(m_cls != NO_CLASS_HANDLE);
        return m_cls;
    }

    CORINFO_CLASS_HANDLE GetClassHandleForObjRef() const
    {
        assert(IsType(TI_REF));
        assert(m_cls != NO_CLASS_HANDLE);
        return m_cls;
    }

    CORINFO_RESOLVED_TOKEN* GetToken() const
    {
        assert(IsToken());
        return m_token;
    }

    BOOL IsType(ti_types type) const
    {
        assert(type != TI_ERROR);
        return (m_flags & (TI_FLAG_DATA_MASK | TI_FLAG_BYREF | TI_FLAG_BYREF_READONLY | TI_FLAG_GENERIC_TYPE_VAR)) ==
               static_cast<unsigned>(type);
    }

    // Returns whether this is a by-ref
    BOOL IsByRef() const
    {
        return (m_flags & TI_FLAG_BYREF) != 0;
    }

#ifdef DEBUG
    BOOL IsUnboxedGenericTypeVar() const
    {
        return !IsByRef() && ((m_flags & TI_FLAG_GENERIC_TYPE_VAR) != 0);
    }

    BOOL IsReadonlyByRef() const
    {
        return IsByRef() && ((m_flags & TI_FLAG_BYREF_READONLY) != 0);
    }
#endif

    // A byref value class is NOT a value class
    BOOL IsValueClass() const
    {
        return IsType(TI_STRUCT) || IsPrimitiveType();
    }

    // Does not return true for primitives. Will return true for value types that behave
    // as primitives
    BOOL IsValueClassWithClsHnd() const
    {
        unsigned type = GetType();

        return (type == TI_STRUCT) ||
               ((m_cls != NO_CLASS_HANDLE) && (type != TI_REF) && (type != TI_METHOD) &&
                (type != TI_ERROR)); // necessary because if byref bit is set, we return TI_ERROR)
    }

    BOOL IsToken() const
    {
        return (GetType() == TI_METHOD) && ((m_flags & TI_FLAG_TOKEN) != 0);
    }
};

#endif // _TYPEINFO_H_
