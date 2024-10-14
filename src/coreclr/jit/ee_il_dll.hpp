// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

extern ICorJitHost* g_jitHost;

class CILJit : public ICorJitCompiler
{
    CorJitResult compileMethod(ICorJitInfo*         comp,
                               CORINFO_METHOD_INFO* methodInfo,
                               unsigned             flags,
                               uint8_t**            nativeEntry,
                               uint32_t*            nativeSizeOfCode);
    void ProcessShutdownWork(ICorStaticInfo* statInfo);
    void getVersionIdentifier(GUID* versionIdentifier);
    unsigned getMaxIntrinsicSIMDVectorLength(CORJIT_FLAGS cpuCompileFlags);
};

inline var_types CorTypeToVarType(CorInfoType type);

FORCEINLINE
void Compiler::eeGetCallInfo(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                             CORINFO_RESOLVED_TOKEN* pConstrainedToken,
                             CORINFO_CALLINFO_FLAGS  flags,
                             CORINFO_CALL_INFO*      pResult)
{
    info.compCompHnd->getCallInfo(pResolvedToken, pConstrainedToken, info.compMethodHnd, flags, pResult);
}

FORCEINLINE
void Compiler::eeGetFieldInfo(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                              CORINFO_ACCESS_FLAGS    accessFlags,
                              CORINFO_FIELD_INFO*     pResult)
{
    info.compCompHnd->getFieldInfo(pResolvedToken, info.compMethodHnd, accessFlags, pResult);
}

FORCEINLINE
void Compiler::eeGetSig(unsigned               sigTok,
                        CORINFO_MODULE_HANDLE  scope,
                        CORINFO_CONTEXT_HANDLE context,
                        CORINFO_SIG_INFO*      retSig)
{
    info.compCompHnd->findSig(scope, sigTok, context, retSig);

    assert(!varTypeIsComposite(CorTypeToVarType(retSig->retType)) || retSig->retTypeClass != nullptr);
}

FORCEINLINE
void Compiler::eeGetMethodSig(CORINFO_METHOD_HANDLE methHnd, CORINFO_SIG_INFO* sigRet, CORINFO_CLASS_HANDLE owner)
{
    info.compCompHnd->getMethodSig(methHnd, sigRet, owner);

    assert(!varTypeIsComposite(CorTypeToVarType(sigRet->retType)) || sigRet->retTypeClass != nullptr);
}

FORCEINLINE
void Compiler::eeGetCallSiteSig(unsigned               sigTok,
                                CORINFO_MODULE_HANDLE  scope,
                                CORINFO_CONTEXT_HANDLE context,
                                CORINFO_SIG_INFO*      sigRet)
{
    info.compCompHnd->findCallSiteSig(scope, sigTok, context, sigRet);

    assert(!varTypeIsComposite(CorTypeToVarType(sigRet->retType)) || sigRet->retTypeClass != nullptr);
}

inline var_types Compiler::eeGetArgType(CORINFO_ARG_LIST_HANDLE list, CORINFO_SIG_INFO* sig)
{
    CORINFO_CLASS_HANDLE argClass;
    return CorTypeToVarType(strip(info.compCompHnd->getArgType(sig, list, &argClass)));
}

inline var_types Compiler::eeGetArgType(CORINFO_ARG_LIST_HANDLE list, CORINFO_SIG_INFO* sig, bool* isPinned)
{
    CORINFO_CLASS_HANDLE argClass;
    CorInfoTypeWithMod   type = info.compCompHnd->getArgType(sig, list, &argClass);

    *isPinned = (type & ~CORINFO_TYPE_MASK) != 0;
    return CorTypeToVarType(strip(type));
}

inline CORINFO_CLASS_HANDLE Compiler::eeGetArgClass(CORINFO_SIG_INFO* sig, CORINFO_ARG_LIST_HANDLE list)
{
    CORINFO_CLASS_HANDLE argClass = info.compCompHnd->getArgClass(sig, list);
    return argClass;
}

inline CORINFO_CLASS_HANDLE Compiler::eeGetClassFromContext(CORINFO_CONTEXT_HANDLE context)
{
    if (context == METHOD_BEING_COMPILED_CONTEXT())
    {
        return impInlineRoot()->info.compClassHnd;
    }

    size_t contextBits = reinterpret_cast<size_t>(context);

    if ((contextBits & CORINFO_CONTEXTFLAGS_MASK) == CORINFO_CONTEXTFLAGS_CLASS)
    {
        return reinterpret_cast<CORINFO_CLASS_HANDLE>(contextBits & ~CORINFO_CONTEXTFLAGS_MASK);
    }
    else
    {
        return info.compCompHnd->getMethodClass(
            reinterpret_cast<CORINFO_METHOD_HANDLE>(contextBits & ~CORINFO_CONTEXTFLAGS_MASK));
    }
}

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
        TYP_U_IMPL, // CORINFO_TYPE_PTR
        TYP_BYREF,  // CORINFO_TYPE_BYREF
        TYP_STRUCT, // CORINFO_TYPE_VALUECLASS
        TYP_REF,    // CORINFO_TYPE_CLASS
        TYP_STRUCT, // CORINFO_TYPE_REFANY
        TYP_UNDEF,  // CORINFO_TYPE_VAR
    };

    // Spot check to make certain enumerations have not changed.
    static_assert_no_msg(map[CORINFO_TYPE_CLASS] == TYP_REF);
    static_assert_no_msg(map[CORINFO_TYPE_BYREF] == TYP_BYREF);
    static_assert_no_msg(map[CORINFO_TYPE_PTR] == TYP_U_IMPL);
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

inline HandleKind TokenToHandleKind(unsigned token)
{
    switch (TypeFromToken(token))
    {
        case mdtTypeRef:
        case mdtTypeDef:
        case mdtTypeSpec:
            return HandleKind::Class;
        case mdtMethodDef:
            return HandleKind::Method;
        case mdtFieldDef:
            return HandleKind::Field;
        default:
            return HandleKind::Token;
    }
}
