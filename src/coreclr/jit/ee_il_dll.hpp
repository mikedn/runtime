// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

extern ICorJitHost* g_jitHost;

constexpr int VPTR_OFFS = 0; // offset of vtable pointer from obj ptr

inline var_types CorTypeToVarType(CorInfoType type)
{
    static constexpr var_types map[CORINFO_TYPE_COUNT]{
        TYP_UNDEF,  // CORINFO_TYPE_UNDEF
        TYP_VOID,   // CORINFO_TYPE_VOID
        TYP_BOOL,   // CORINFO_TYPE_BOOL
        TYP_USHORT, // CORINFO_TYPE_CHAR
        TYP_BYTE,   // CORINFO_TYPE_BYTE
        TYP_UBYTE,  // CORINFO_TYPE_UBYTE
        TYP_SHORT,  // CORINFO_TYPE_SHORT
        TYP_USHORT, // CORINFO_TYPE_USHORT
        TYP_INT,    // CORINFO_TYPE_INT
        TYP_INT,    // CORINFO_TYPE_UINT
        TYP_LONG,   // CORINFO_TYPE_LONG
        TYP_LONG,   // CORINFO_TYPE_ULONG
        TYP_I_IMPL, // CORINFO_TYPE_NATIVEINT
        TYP_I_IMPL, // CORINFO_TYPE_NATIVEUINT
        TYP_FLOAT,  // CORINFO_TYPE_FLOAT
        TYP_DOUBLE, // CORINFO_TYPE_DOUBLE
        TYP_REF,    // CORINFO_TYPE_STRING
        TYP_I_IMPL, // CORINFO_TYPE_PTR
        TYP_BYREF,  // CORINFO_TYPE_BYREF
        TYP_STRUCT, // CORINFO_TYPE_VALUECLASS
        TYP_REF,    // CORINFO_TYPE_CLASS
        TYP_STRUCT, // CORINFO_TYPE_REFANY
        TYP_UNDEF,  // CORINFO_TYPE_VAR
    };

    // Spot check to make certain enumerations have not changed.
    static_assert_no_msg(map[CORINFO_TYPE_CLASS] == TYP_REF);
    static_assert_no_msg(map[CORINFO_TYPE_BYREF] == TYP_BYREF);
    static_assert_no_msg(map[CORINFO_TYPE_PTR] == TYP_I_IMPL);
    static_assert_no_msg(map[CORINFO_TYPE_INT] == TYP_INT);
    static_assert_no_msg(map[CORINFO_TYPE_UINT] == TYP_INT);
    static_assert_no_msg(map[CORINFO_TYPE_DOUBLE] == TYP_DOUBLE);
    static_assert_no_msg(map[CORINFO_TYPE_VOID] == TYP_VOID);
    static_assert_no_msg(map[CORINFO_TYPE_VALUECLASS] == TYP_STRUCT);
    static_assert_no_msg(map[CORINFO_TYPE_REFANY] == TYP_STRUCT);

    assert(type < _countof(map));
    assert(map[type] != TYP_UNDEF);

    return map[type];
}

inline var_types CorTypeToPreciseVarType(CorInfoType type)
{
    static constexpr var_types map[CORINFO_TYPE_COUNT]{
        TYP_UNDEF,  // CORINFO_TYPE_UNDEF
        TYP_VOID,   // CORINFO_TYPE_VOID
        TYP_BOOL,   // CORINFO_TYPE_BOOL
        TYP_USHORT, // CORINFO_TYPE_CHAR
        TYP_BYTE,   // CORINFO_TYPE_BYTE
        TYP_UBYTE,  // CORINFO_TYPE_UBYTE
        TYP_SHORT,  // CORINFO_TYPE_SHORT
        TYP_USHORT, // CORINFO_TYPE_USHORT
        TYP_INT,    // CORINFO_TYPE_INT
        TYP_UINT,   // CORINFO_TYPE_UINT
        TYP_LONG,   // CORINFO_TYPE_LONG
        TYP_ULONG,  // CORINFO_TYPE_ULONG
        TYP_I_IMPL, // CORINFO_TYPE_NATIVEINT
        TYP_U_IMPL, // CORINFO_TYPE_NATIVEUINT
        TYP_FLOAT,  // CORINFO_TYPE_FLOAT
        TYP_DOUBLE, // CORINFO_TYPE_DOUBLE
        TYP_REF,    // CORINFO_TYPE_STRING
        TYP_I_IMPL, // CORINFO_TYPE_PTR
        TYP_BYREF,  // CORINFO_TYPE_BYREF
        TYP_STRUCT, // CORINFO_TYPE_VALUECLASS
        TYP_REF,    // CORINFO_TYPE_CLASS
        TYP_STRUCT, // CORINFO_TYPE_REFANY
        TYP_UNDEF,  // CORINFO_TYPE_VAR
    };

    // Spot check to make certain enumerations have not changed.
    static_assert_no_msg(map[CORINFO_TYPE_CLASS] == TYP_REF);
    static_assert_no_msg(map[CORINFO_TYPE_BYREF] == TYP_BYREF);
    static_assert_no_msg(map[CORINFO_TYPE_PTR] == TYP_I_IMPL);
    static_assert_no_msg(map[CORINFO_TYPE_INT] == TYP_INT);
    static_assert_no_msg(map[CORINFO_TYPE_UINT] == TYP_UINT);
    static_assert_no_msg(map[CORINFO_TYPE_DOUBLE] == TYP_DOUBLE);
    static_assert_no_msg(map[CORINFO_TYPE_VOID] == TYP_VOID);
    static_assert_no_msg(map[CORINFO_TYPE_VALUECLASS] == TYP_STRUCT);
    static_assert_no_msg(map[CORINFO_TYPE_REFANY] == TYP_STRUCT);

    assert(type < _countof(map));
    assert(map[type] != TYP_UNDEF);

    return map[type];
}

inline CORINFO_CALLINFO_FLAGS operator|(CORINFO_CALLINFO_FLAGS flag1, CORINFO_CALLINFO_FLAGS flag2)
{
    return static_cast<CORINFO_CALLINFO_FLAGS>(static_cast<int>(flag1) | static_cast<int>(flag2));
}
