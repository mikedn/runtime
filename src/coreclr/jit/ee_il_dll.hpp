// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

extern ICorJitHost* g_jitHost;

class CILJit : public ICorJitCompiler
{
    CorJitResult compileMethod(ICorJitInfo*         comp,            /* IN */
                               CORINFO_METHOD_INFO* methodInfo,      /* IN */
                               unsigned             flags,           /* IN */
                               uint8_t**            nativeEntry,     /* OUT */
                               uint32_t*            nativeSizeOfCode /* OUT */
                               );

    void ProcessShutdownWork(ICorStaticInfo* statInfo);

    void getVersionIdentifier(GUID* versionIdentifier /* OUT */
                              );

    unsigned getMaxIntrinsicSIMDVectorLength(CORJIT_FLAGS cpuCompileFlags);
};

/*****************************************************************************
 *
 *              Functions to get various handles
 */

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

    assert(!varTypeIsComposite(JITtype2varType(retSig->retType)) || retSig->retTypeClass != nullptr);
}

FORCEINLINE
void Compiler::eeGetMethodSig(CORINFO_METHOD_HANDLE methHnd, CORINFO_SIG_INFO* sigRet, CORINFO_CLASS_HANDLE owner)
{
    info.compCompHnd->getMethodSig(methHnd, sigRet, owner);

    assert(!varTypeIsComposite(JITtype2varType(sigRet->retType)) || sigRet->retTypeClass != nullptr);
}

/**********************************************************************
 * For varargs we need the number of arguments at the call site
 */

FORCEINLINE
void Compiler::eeGetCallSiteSig(unsigned               sigTok,
                                CORINFO_MODULE_HANDLE  scope,
                                CORINFO_CONTEXT_HANDLE context,
                                CORINFO_SIG_INFO*      sigRet)
{
    info.compCompHnd->findCallSiteSig(scope, sigTok, context, sigRet);

    assert(!varTypeIsComposite(JITtype2varType(sigRet->retType)) || sigRet->retTypeClass != nullptr);
}

/*****************************************************************************/
inline var_types Compiler::eeGetArgType(CORINFO_ARG_LIST_HANDLE list, CORINFO_SIG_INFO* sig)
{
    CORINFO_CLASS_HANDLE argClass;
    return (JITtype2varType(strip(info.compCompHnd->getArgType(sig, list, &argClass))));
}

/*****************************************************************************/
inline var_types Compiler::eeGetArgType(CORINFO_ARG_LIST_HANDLE list, CORINFO_SIG_INFO* sig, bool* isPinned)
{
    CORINFO_CLASS_HANDLE argClass;
    CorInfoTypeWithMod   type = info.compCompHnd->getArgType(sig, list, &argClass);
    *isPinned                 = ((type & ~CORINFO_TYPE_MASK) != 0);
    return JITtype2varType(strip(type));
}

/*****************************************************************************/
inline CORINFO_CLASS_HANDLE Compiler::eeGetArgClass(CORINFO_SIG_INFO* sig, CORINFO_ARG_LIST_HANDLE list)
{
    CORINFO_CLASS_HANDLE argClass = info.compCompHnd->getArgClass(sig, list);
    return argClass;
}

/*****************************************************************************/
inline CORINFO_CLASS_HANDLE Compiler::eeGetClassFromContext(CORINFO_CONTEXT_HANDLE context)
{
    if (context == METHOD_BEING_COMPILED_CONTEXT())
    {
        return impInlineRoot()->info.compClassHnd;
    }

    if (((SIZE_T)context & CORINFO_CONTEXTFLAGS_MASK) == CORINFO_CONTEXTFLAGS_CLASS)
    {
        return CORINFO_CLASS_HANDLE((SIZE_T)context & ~CORINFO_CONTEXTFLAGS_MASK);
    }
    else
    {
        return info.compCompHnd->getMethodClass(CORINFO_METHOD_HANDLE((SIZE_T)context & ~CORINFO_CONTEXTFLAGS_MASK));
    }
}

inline var_types JITtype2varType(CorInfoType type)
{
    static constexpr var_types map[CORINFO_TYPE_COUNT]{
        // see the definition of enum CorInfoType in file inc/corinfo.h
        TYP_UNDEF,  // CORINFO_TYPE_UNDEF           = 0x0,
        TYP_VOID,   // CORINFO_TYPE_VOID            = 0x1,
        TYP_BOOL,   // CORINFO_TYPE_BOOL            = 0x2,
        TYP_USHORT, // CORINFO_TYPE_CHAR            = 0x3,
        TYP_BYTE,   // CORINFO_TYPE_BYTE            = 0x4,
        TYP_UBYTE,  // CORINFO_TYPE_UBYTE           = 0x5,
        TYP_SHORT,  // CORINFO_TYPE_SHORT           = 0x6,
        TYP_USHORT, // CORINFO_TYPE_USHORT          = 0x7,
        TYP_INT,    // CORINFO_TYPE_INT             = 0x8,
        TYP_INT,    // CORINFO_TYPE_UINT            = 0x9,
        TYP_LONG,   // CORINFO_TYPE_LONG            = 0xa,
        TYP_LONG,   // CORINFO_TYPE_ULONG           = 0xb,
        TYP_I_IMPL, // CORINFO_TYPE_NATIVEINT       = 0xc,
        TYP_I_IMPL, // CORINFO_TYPE_NATIVEUINT      = 0xd,
        TYP_FLOAT,  // CORINFO_TYPE_FLOAT           = 0xe,
        TYP_DOUBLE, // CORINFO_TYPE_DOUBLE          = 0xf,
        TYP_REF,    // CORINFO_TYPE_STRING          = 0x10,         // Not used, should remove
        TYP_I_IMPL, // CORINFO_TYPE_PTR             = 0x11,
        TYP_BYREF,  // CORINFO_TYPE_BYREF           = 0x12,
        TYP_STRUCT, // CORINFO_TYPE_VALUECLASS      = 0x13,
        TYP_REF,    // CORINFO_TYPE_CLASS           = 0x14,
        TYP_STRUCT, // CORINFO_TYPE_REFANY          = 0x15,
        TYP_UNDEF,  // CORINFO_TYPE_VAR             = 0x16,
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

inline var_types CorTypeToVarType(CorInfoType type)
{
    return JITtype2varType(type);
}

inline var_types CorTypeToPreciseVarType(CorInfoType type)
{
    static constexpr var_types map[CORINFO_TYPE_COUNT]{
        // see the definition of enum CorInfoType in file inc/corinfo.h
        TYP_UNDEF,  // CORINFO_TYPE_UNDEF           = 0x0,
        TYP_VOID,   // CORINFO_TYPE_VOID            = 0x1,
        TYP_BOOL,   // CORINFO_TYPE_BOOL            = 0x2,
        TYP_USHORT, // CORINFO_TYPE_CHAR            = 0x3,
        TYP_BYTE,   // CORINFO_TYPE_BYTE            = 0x4,
        TYP_UBYTE,  // CORINFO_TYPE_UBYTE           = 0x5,
        TYP_SHORT,  // CORINFO_TYPE_SHORT           = 0x6,
        TYP_USHORT, // CORINFO_TYPE_USHORT          = 0x7,
        TYP_INT,    // CORINFO_TYPE_INT             = 0x8,
        TYP_UINT,   // CORINFO_TYPE_UINT            = 0x9,
        TYP_LONG,   // CORINFO_TYPE_LONG            = 0xa,
        TYP_ULONG,  // CORINFO_TYPE_ULONG           = 0xb,
        TYP_I_IMPL, // CORINFO_TYPE_NATIVEINT       = 0xc,
        TYP_U_IMPL, // CORINFO_TYPE_NATIVEUINT      = 0xd,
        TYP_FLOAT,  // CORINFO_TYPE_FLOAT           = 0xe,
        TYP_DOUBLE, // CORINFO_TYPE_DOUBLE          = 0xf,
        TYP_REF,    // CORINFO_TYPE_STRING          = 0x10,         // Not used, should remove
        TYP_U_IMPL, // CORINFO_TYPE_PTR             = 0x11,
        TYP_BYREF,  // CORINFO_TYPE_BYREF           = 0x12,
        TYP_STRUCT, // CORINFO_TYPE_VALUECLASS      = 0x13,
        TYP_REF,    // CORINFO_TYPE_CLASS           = 0x14,
        TYP_STRUCT, // CORINFO_TYPE_REFANY          = 0x15,
        TYP_UNDEF,  // CORINFO_TYPE_VAR             = 0x16,
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
