// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*****************************************************************************
 This header file is named _typeInfo.h to be distinguished from typeinfo.h
 in the NT SDK
******************************************************************************/

#pragma once

enum ti_types
{
    TI_ERROR,
    TI_REF,
    TI_STRUCT,
    TI_METHOD,
    TI_INT,
    TI_LONG,
    TI_DOUBLE,

    TI_MAX             = TI_DOUBLE,
    TI_FIRST_PRIMITIVE = TI_INT,
#ifdef DEBUG
#ifdef TARGET_64BIT
    TI_I_IMPL = TI_LONG,
#else
    TI_I_IMPL = TI_INT,
#endif
#endif
};

// Declares the typeInfo class, which represents the type of an entity on the stack.
class typeInfo
{
    static_assert_no_msg(TI_MAX < 8);
    static constexpr unsigned TI_FLAG_TYPE_BITS = 3;
    static constexpr unsigned TI_FLAG_TYPE_MASK = (1u << TI_FLAG_TYPE_BITS) - 1;

    // Flag indicating this item is a byref <something>
    static constexpr unsigned TI_FLAG_BYREF = 1u << (TI_FLAG_TYPE_BITS + 1);
#ifdef DEBUG
#ifdef TARGET_64BIT
    static constexpr unsigned TI_FLAG_NATIVE_INT = 1u << (TI_FLAG_TYPE_BITS + 2);
#endif
#endif

    unsigned m_flags;

    union {
        CORINFO_CLASS_HANDLE m_cls;
        // Valid only for TI_TOKEN with IsToken
        CORINFO_RESOLVED_TOKEN* m_token;
    };

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
        return static_cast<ti_types>(m_flags & TI_FLAG_TYPE_MASK);
    }

    // Returns whether this is a primitive type (not a byref, objref,
    // array, null, value class, invalid value)
    bool IsPrimitiveType() const
    {
        unsigned type = GetType();

        return (type == TI_INT) || (type == TI_LONG) || (type == TI_DOUBLE);
    }

public:
    typeInfo() : m_flags(TI_ERROR), m_cls(NO_CLASS_HANDLE)
    {
    }

#ifdef DEBUG
    typeInfo(ti_types tiType) : m_flags(static_cast<unsigned>(tiType)), m_cls(NO_CLASS_HANDLE)
    {
        assert((tiType >= TI_INT) && (tiType <= TI_DOUBLE));
    }
#endif

    typeInfo(ti_types tiType, CORINFO_CLASS_HANDLE cls) : m_flags(tiType), m_cls(cls)
    {
        assert((tiType == TI_STRUCT) || (tiType == TI_REF));
        assert(cls != nullptr);
    }

    typeInfo(CORINFO_RESOLVED_TOKEN* token) : m_flags(TI_METHOD), m_token(token)
    {
        assert(token->hMethod != nullptr);
    }

    CORINFO_CLASS_HANDLE GetClassHandle() const
    {
        return m_cls;
    }

    CORINFO_CLASS_HANDLE GetClassHandleForValueClass() const
    {
        assert(IsStruct());
        assert(m_cls != NO_CLASS_HANDLE);
        return m_cls;
    }

    CORINFO_RESOLVED_TOKEN* GetToken() const
    {
        assert(IsMethod());
        return m_token;
    }

#ifdef DEBUG
    bool IsRef() const
    {
        return (m_flags & (TI_FLAG_TYPE_MASK | TI_FLAG_BYREF)) == TI_REF;
    }
#endif

    bool IsStruct() const
    {
        return (m_flags & (TI_FLAG_TYPE_MASK | TI_FLAG_BYREF)) == TI_STRUCT;
    }

    // A byref value class is NOT a value class
    bool IsValueClass() const
    {
        return IsStruct() || IsPrimitiveType();
    }

#ifdef TARGET_64BIT
    bool IsInt() const
    {
        return (m_flags & (TI_FLAG_TYPE_MASK | TI_FLAG_BYREF)) == TI_INT;
    }
#endif

    bool IsMethod() const
    {
        return (m_flags & (TI_FLAG_TYPE_MASK | TI_FLAG_BYREF)) == TI_METHOD;
    }

#ifdef DEBUG
    static typeInfo NativeInt()
    {
        typeInfo result = typeInfo(TI_I_IMPL);
#ifdef TARGET_64BIT
        result.m_flags |= TI_FLAG_NATIVE_INT;
#endif
        return result;
    }

    typeInfo& MakeByRef()
    {
        assert(!IsByRef());
        m_flags |= TI_FLAG_BYREF;
        return *this;
    }

    bool IsByRef() const
    {
        return (m_flags & TI_FLAG_BYREF) != 0;
    }

    typeInfo& DereferenceByRef()
    {
        if (!IsByRef())
        {
            m_flags = TI_ERROR;
            m_cls   = NO_CLASS_HANDLE;
        }
        return *this;
    }

    static bool AreEquivalent(const typeInfo& li, const typeInfo& ti);

    static bool tiCompatibleWith(ICorJitInfo* vm, const typeInfo& child, const typeInfo& parent);
#endif // DEBUG
};
