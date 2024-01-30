// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                            ee_jit.cpp                                     XX
XX                                                                           XX
XX   The functionality needed for the JIT DLL. Includes the DLL entry point  XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#include "corexcep.h"
#include "jitstd/algorithm.h"
#include "codegen.h"

#ifndef HOST_UNIX
#include <io.h>    // For _dup, _setmode
#include <fcntl.h> // For _O_TEXT
#include <errno.h> // For EINVAL
#endif

#ifndef DLLEXPORT
#define DLLEXPORT
#endif // !DLLEXPORT

/*****************************************************************************/

FILE* jitstdout;

ICorJitHost*     g_jitHost;
ICorJitCompiler* g_jit;

INDEBUG(extern ConfigMethodRange fJitStressRange;)

/*****************************************************************************/

extern "C" DLLEXPORT void jitStartup(ICorJitHost* jitHost)
{
    if (g_jit != nullptr)
    {
        if (jitHost != g_jitHost)
        {
            // We normally don't expect jitStartup() to be invoked more than once.
            // (We check whether it has been called once due to an abundance of caution.)
            // However, during SuperPMI playback of MCH file, we need to JIT many different methods.
            // Each one carries its own environment configuration state.
            // So, we need the JIT to reload the JitConfig state for each change in the environment state of the
            // replayed compilations.
            // We do this by calling jitStartup with a different ICorJitHost,
            // and have the JIT re-initialize its JitConfig state when this happens.
            JitConfig.destroy(g_jitHost);
            JitConfig.initialize(jitHost);
            g_jitHost = jitHost;
        }
        return;
    }

#ifdef HOST_UNIX
    int err = PAL_InitializeDLL();
    if (err != 0)
    {
        return;
    }
#endif

    g_jitHost = jitHost;

    assert(!JitConfig.isInitialized());
    JitConfig.initialize(jitHost);

    INDEBUG(fJitStressRange.EnsureInit(JitConfig.JitStressRange()));

#ifdef DEBUG
    const WCHAR* jitStdOutFile = JitConfig.JitStdOutFile();
    if (jitStdOutFile != nullptr)
    {
        jitstdout = _wfopen(jitStdOutFile, W("a"));
        assert(jitstdout != nullptr);
    }
#endif // DEBUG

#if !defined(HOST_UNIX)
    if (jitstdout == nullptr)
    {
        int stdoutFd = _fileno(procstdout());
        // Check fileno error output(s) -1 may overlap with errno result
        // but is included for completness.
        // We want to detect the case where the initial handle is null
        // or bogus and avoid making further calls.
        if ((stdoutFd != -1) && (stdoutFd != -2) && (errno != EINVAL))
        {
            int jitstdoutFd = _dup(_fileno(procstdout()));
            // Check the error status returned by dup.
            if (jitstdoutFd != -1)
            {
                _setmode(jitstdoutFd, _O_TEXT);
                jitstdout = _fdopen(jitstdoutFd, "w");
                assert(jitstdout != nullptr);

                // Prevent the FILE* from buffering its output in order to avoid calls to
                // `fflush()` throughout the code.
                setvbuf(jitstdout, nullptr, _IONBF, 0);
            }
        }
    }
#endif // !HOST_UNIX

    // If jitstdout is still null, fallback to whatever procstdout() was
    // initially set to.
    if (jitstdout == nullptr)
    {
        jitstdout = procstdout();
    }

    Compiler::compStartup();

    alignas(CILJit) static char jitMem[sizeof(CILJit)];
    g_jit = new (jitMem) CILJit();
}

void jitShutdown(bool processIsTerminating)
{
    if (g_jit == nullptr)
    {
        return;
    }

    Compiler::compShutdown();

    if (jitstdout != procstdout())
    {
        // When the process is terminating, the fclose call is unnecessary and is also prone to
        // crashing since the UCRT itself often frees the backing memory earlier on in the
        // termination sequence.
        if (!processIsTerminating)
        {
            fclose(jitstdout);
        }
    }

    g_jit = nullptr;
}

DLLEXPORT ICorJitCompiler* getJit()
{
    return g_jit;
}

// Information kept in thread-local storage. This is used in the noway_assert exceptional path.
// If you are using it more broadly in retail code, you would need to understand the
// performance implications of accessing TLS.

#ifdef DEBUG

thread_local JitTls* gJitTls = nullptr;

static JitTls* GetJitTls()
{
    return gJitTls;
}

void SetJitTls(JitTls* value)
{
    gJitTls = value;
}

JitTls::JitTls(ICorJitInfo* jitInfo)
    : m_compiler(nullptr), m_logCompiler(nullptr), m_jitInfo(jitInfo), m_next(GetJitTls())
{
    SetJitTls(this);
}

JitTls::~JitTls()
{
    SetJitTls(m_next);
}

ICorJitInfo* JitTls::GetJitInfo()
{
    return GetJitTls()->m_jitInfo;
}

Compiler* JitTls::GetLogCompiler()
{
    return GetJitTls()->m_logCompiler;
}

void JitTls::SetLogCompiler(Compiler* compiler)
{
    GetJitTls()->m_logCompiler = compiler;
}

Compiler* JitTls::GetCompiler()
{
    return GetJitTls()->m_compiler;
}

void JitTls::SetCompiler(Compiler* compiler)
{
    GetJitTls()->m_compiler = compiler;
}

#else // !defined(DEBUG)

thread_local Compiler* gJitTls = nullptr;

static Compiler* GetJitTls()
{
    return gJitTls;
}

void SetJitTls(Compiler* value)
{
    gJitTls = value;
}

JitTls::JitTls(ICorJitInfo* jitInfo)
{
}

JitTls::~JitTls()
{
}

Compiler* JitTls::GetCompiler()
{
    return GetJitTls();
}

void JitTls::SetCompiler(Compiler* compiler)
{
    SetJitTls(compiler);
}

#endif // !defined(DEBUG)

static CorJitResult jitNativeCode(ICorJitInfo*         jitInfo,
                                  CORINFO_METHOD_INFO* methodInfo,
                                  void**               nativeCode,
                                  uint32_t*            nativeCodeSize,
                                  JitFlags*            jitFlags)
{
    bool jitFallbackCompile = false;

START:
    struct Param : ErrorTrapParam
    {
        ArenaAllocator       allocator;
        Compiler*            compiler     = nullptr;
        Compiler*            prevCompiler = nullptr;
        bool                 jitFallbackCompile;
        CORINFO_METHOD_INFO* methodInfo;
        void**               nativeCode;
        uint32_t*            nativeCodeSize;
        JitFlags*            jitFlags;
        CorJitResult         result = CORJIT_INTERNALERROR;
        CORINFO_EE_INFO      eeInfo;
    } param;

    param.jitInfo            = jitInfo;
    param.jitFallbackCompile = jitFallbackCompile;
    param.methodInfo         = methodInfo;
    param.nativeCode         = nativeCode;
    param.nativeCodeSize     = nativeCodeSize;
    param.jitFlags           = jitFlags;

    PAL_TRY(Param&, p, param)
    {
        NestedErrorTrapParam<Param&> param2(p);

        PAL_TRY(NestedErrorTrapParam<Param&>&, p2, param2)
        {
            Param& p = p2.param;

            p.jitInfo->getEEInfo(&p.eeInfo);

            p.compiler = static_cast<Compiler*>(p.allocator.allocateMemory(sizeof(Compiler)));
            new (p.compiler) Compiler(&p.allocator, &p.eeInfo, p.methodInfo, p.jitInfo);
            p.prevCompiler = JitTls::GetCompiler();
            JitTls::SetCompiler(p.compiler);
            INDEBUG(JitTls::SetLogCompiler(p.compiler));

#if MEASURE_CLRAPI_CALLS
            if (!p.jitFallbackCompile)
            {
                WrapICorJitInfo::WrapJitInfo(p.compiler);
            }
#endif

            p.compiler->compInitMethodName();
            p.compiler->compInit();
            INDEBUG(p.compiler->jitFallbackCompile = p.jitFallbackCompile;)
            p.result = p.compiler->compCompileMain(p.nativeCode, p.nativeCodeSize, p.jitFlags);
        }
        PAL_FINALLY
        {
            // If OOM is thrown when allocating the Compiler object, we will
            // end up here and p.compiler will be nullptr.

            if (p.compiler != nullptr)
            {
                p.compiler->info.compCode = nullptr;

                assert(JitTls::GetCompiler() == p.compiler);
                JitTls::SetCompiler(p.prevCompiler);
            }

            p.allocator.destroy();
        }
        PAL_ENDTRY
    }
    PAL_EXCEPT_FILTER(JitErrorTrapFilter)
    {
        param.result = param.error;
    }
    PAL_ENDTRY

    CorJitResult result = param.result;

    if (!jitFallbackCompile &&
        ((result == CORJIT_INTERNALERROR) || (result == CORJIT_RECOVERABLEERROR) || (result == CORJIT_IMPLLIMITATION)))
    {
        // If we failed the JIT, reattempt with debuggable code.
        jitFallbackCompile = true;

        // Update the flags for 'safer' code generation.
        jitFlags->Set(JitFlags::JIT_FLAG_MIN_OPT);
        jitFlags->Clear(JitFlags::JIT_FLAG_SIZE_OPT);
        jitFlags->Clear(JitFlags::JIT_FLAG_SPEED_OPT);

        goto START;
    }

    return result;
}

CorJitResult CILJit::compileMethod(ICorJitInfo*         jitInfo,
                                   CORINFO_METHOD_INFO* methodInfo,
                                   unsigned             flags,
                                   uint8_t**            entryAddress,
                                   uint32_t*            nativeCodeSize)
{
    assert(flags == CORJIT_FLAGS::CORJIT_FLAG_CALL_GETJITFLAGS);
    assert(methodInfo->ILCode != nullptr);

    JitTls jitTls(jitInfo);

    CORJIT_FLAGS corJitFlags;
    uint32_t     corJitFlagsSize = jitInfo->getJitFlags(&corJitFlags, sizeof(corJitFlags));
    assert(corJitFlagsSize == sizeof(corJitFlags));
    JitFlags jitFlags;
    jitFlags.SetFromFlags(corJitFlags);

    void* nativeCode = nullptr;
    int   result     = jitNativeCode(jitInfo, methodInfo, &nativeCode, nativeCodeSize, &jitFlags);

    if (result == CORJIT_OK)
    {
        *entryAddress = static_cast<BYTE*>(nativeCode);
    }

    return static_cast<CorJitResult>(result);
}

void CILJit::ProcessShutdownWork(ICorStaticInfo* statInfo)
{
    jitShutdown(false);
}

/*****************************************************************************
 * Verify the JIT/EE interface identifier.
 */
void CILJit::getVersionIdentifier(GUID* versionIdentifier)
{
    assert(versionIdentifier != nullptr);
    memcpy(versionIdentifier, &JITEEVersionIdentifier, sizeof(GUID));
}

/*****************************************************************************
 * Determine the maximum length of SIMD vector supported by this JIT.
 */

unsigned CILJit::getMaxIntrinsicSIMDVectorLength(CORJIT_FLAGS cpuCompileFlags)
{
    JitFlags jitFlags;
    jitFlags.SetFromFlags(cpuCompileFlags);

#ifdef FEATURE_SIMD
#if defined(TARGET_XARCH)
    if (!jitFlags.IsSet(JitFlags::JIT_FLAG_PREJIT) && jitFlags.IsSet(JitFlags::JIT_FLAG_FEATURE_SIMD) &&
        jitFlags.GetInstructionSetFlags().HasInstructionSet(InstructionSet_AVX2))
    {
        // Since the ISAs can be disabled individually and since they are hierarchical in nature (that is
        // disabling SSE also disables SSE2 through AVX2), we need to check each ISA in the hierarchy to
        // ensure that AVX2 is actually supported. Otherwise, we will end up getting asserts downstream.
        if ((JitConfig.EnableAVX2() != 0) && (JitConfig.EnableAVX() != 0) && (JitConfig.EnableSSE42() != 0) &&
            (JitConfig.EnableSSE41() != 0) && (JitConfig.EnableSSSE3() != 0) && (JitConfig.EnableSSE3_4() != 0) &&
            (JitConfig.EnableSSE3() != 0) && (JitConfig.EnableSSE2() != 0) && (JitConfig.EnableSSE() != 0) &&
            (JitConfig.EnableHWIntrinsic() != 0))
        {
            if (GetJitTls() != nullptr && JitTls::GetCompiler() != nullptr)
            {
                JITDUMP("getMaxIntrinsicSIMDVectorLength: returning 32\n");
            }
            return 32;
        }
    }
#endif // defined(TARGET_XARCH)
    if (GetJitTls() != nullptr && JitTls::GetCompiler() != nullptr)
    {
        JITDUMP("getMaxIntrinsicSIMDVectorLength: returning 16\n");
    }
    return 16;
#else  // !FEATURE_SIMD
    if (GetJitTls() != nullptr && JitTls::GetCompiler() != nullptr)
    {
        JITDUMP("getMaxIntrinsicSIMDVectorLength: returning 0\n");
    }
    return 0;
#endif // !FEATURE_SIMD
}

//------------------------------------------------------------------------
// eeGetArrayDataOffset: Gets the offset of a SDArray's first element
//
// Arguments:
//    type - The array element type
//
// Return Value:
//    The offset to the first array element.

unsigned Compiler::eeGetArrayDataOffset(var_types type)
{
    return OFFSETOF__CORINFO_Array__data;
}

//------------------------------------------------------------------------
// eeGetMDArrayDataOffset: Gets the offset of a MDArray's first element
//
// Arguments:
//    type - The array element type
//    rank - The array rank
//
// Return Value:
//    The offset to the first array element.
//
// Assumptions:
//    The rank should be greater than 0.

unsigned Compiler::eeGetMDArrayDataOffset(var_types type, unsigned rank)
{
    assert(rank > 0);
    // Note that below we're specifically using genTypeSize(TYP_INT) because array
    // indices are not native int.
    return eeGetArrayDataOffset(type) + 2 * genTypeSize(TYP_INT) * rank;
}

void Importer::eeGetStmtOffsets()
{
    assert(!compIsForInlining());

    unsigned                     offsetsCount;
    uint32_t*                    offsets;
    ICorDebugInfo::BoundaryTypes offsetsImplicit;

    info.compCompHnd->getBoundaries(info.compMethodHnd, &offsetsCount, &offsets, &offsetsImplicit);

    compStmtOffsetsImplicit = offsetsImplicit;

    if (offsetsCount == 0)
    {
        assert(compStmtOffsetsCount == 0);
        return;
    }

    IL_OFFSET* offsetsCopy      = new (comp, CMK_DebugInfo) IL_OFFSET[offsetsCount];
    unsigned   offsetsCopyCount = 0;
    IL_OFFSET  maxOffset        = info.compILCodeSize;

    for (unsigned i = 0; i < offsetsCount; i++)
    {
        if (offsets[i] <= maxOffset)
        {
            offsetsCopy[offsetsCopyCount++] = offsets[i];
        }
    }

    info.compCompHnd->freeArray(offsets);

    compStmtOffsets      = offsetsCopy;
    compStmtOffsetsCount = offsetsCopyCount;
}

void Compiler::eeGetVars()
{
    ICorDebugInfo::ILVarInfo* varTable;
    uint32_t                  varCount;
    bool                      extendOthers;

    info.compCompHnd->getVars(info.compMethodHnd, &varCount, &varTable, &extendOthers);
    JITDUMP("getVars() returned cVars = %u, extendOthers = %d\n", varCount, extendOthers);

    if (varCount != 0)
    {
        eeGetVars(varTable, varCount, extendOthers);
        info.compCompHnd->freeArray(varTable);

        compInitSortedScopeLists();

        return;
    }

    if ((info.compLocalsCount == 0) || !extendOthers)
    {
        assert(info.compVarScopesCount == 0);
        return;
    }

    // TODO-MIKE-Review: Why do we need to allocate an array that basically
    // tells us that all IL args & locals are live everywhere?!?

    VarScopeDsc* scopes = new (this, CMK_DebugInfo) VarScopeDsc[info.compLocalsCount];

    for (unsigned i = 0; i < info.compLocalsCount; i++)
    {
        scopes[i].lclNum      = i;
        scopes[i].startOffset = 0;
        scopes[i].endOffset   = info.compILCodeSize;
    }

    info.compVarScopesCount = info.compLocalsCount;
    info.compVarScopes      = scopes;
    compVarScopeExtended    = true;
}

void Compiler::eeGetVars(ICorDebugInfo::ILVarInfo* varInfoTable, uint32_t varInfoCount, bool extendOthers)
{
    assert(varInfoCount != 0);

    // TODO-MIKE-Review: Get rid of this? The runtime only returns a var table in varargs
    // functions and then the table has only one variable that is live everywhere.

    unsigned varInfoCountExtra = varInfoCount;

    if (extendOthers)
    {
        varInfoCountExtra += info.compLocalsCount;
    }

    info.compVarScopes = new (this, CMK_DebugInfo) VarScopeDsc[varInfoCountExtra];

    VarScopeDsc*              scopes = info.compVarScopes;
    ICorDebugInfo::ILVarInfo* vars   = varInfoTable;

    for (unsigned i = 0; i < varInfoCount; i++, vars++, scopes++)
    {
        JITDUMP("var:%d start:%d end:%d\n", vars->varNumber, vars->startOffset, vars->endOffset);

        if (vars->startOffset >= vars->endOffset)
        {
            continue;
        }

        assert(vars->startOffset <= info.compILCodeSize);
        assert(vars->endOffset <= info.compILCodeSize);

        scopes->lclNum      = compMapILvarNum(vars->varNumber);
        scopes->startOffset = vars->startOffset;
        scopes->endOffset   = vars->endOffset;

        info.compVarScopesCount++;
    }

    if (extendOthers)
    {
        bool* varInfoProvided = getAllocator(CMK_DebugInfo).allocate<bool>(info.compLocalsCount);

        for (unsigned i = 0; i < info.compLocalsCount; i++)
        {
            varInfoProvided[i] = false;
        }

        for (unsigned i = 0; i < info.compVarScopesCount; i++)
        {
            varInfoProvided[info.compVarScopes[i].lclNum] = true;
        }

        for (unsigned lclNum = 0; lclNum < info.compLocalsCount; lclNum++, scopes++)
        {
            if (varInfoProvided[lclNum])
            {
                continue;
            }

            scopes->lclNum      = lclNum;
            scopes->startOffset = 0;
            scopes->endOffset   = info.compILCodeSize;

            info.compVarScopesCount++;
        }
    }

    assert(scopes <= info.compVarScopes + varInfoCountExtra);

    DBEXEC(verbose, compDispLocalVars();)
}

unsigned Compiler::compMapILvarNum(unsigned ilVarNum)
{
    unsigned compILlocalsCount = info.compILargsCount + info.compMethodInfo->locals.numArgs;

    noway_assert((ilVarNum < compILlocalsCount) || (ilVarNum > ICorDebugInfo::UNKNOWN_ILNUM));

    unsigned varNum;

    if (ilVarNum == ICorDebugInfo::VARARGS_HND_ILNUM)
    {
        noway_assert(info.compIsVarArgs);

        varNum = lvaVarargsHandleArg;

        noway_assert(lvaGetDesc(varNum)->IsParam());
    }
    else if (ilVarNum == ICorDebugInfo::RETBUF_ILNUM)
    {
        noway_assert(info.compRetBuffArg != BAD_VAR_NUM);

        varNum = info.compRetBuffArg;
    }
    else if (ilVarNum == ICorDebugInfo::TYPECTXT_ILNUM)
    {
        noway_assert(info.compTypeCtxtArg != BAD_VAR_NUM);

        varNum = info.compTypeCtxtArg;
    }
    else if (ilVarNum < info.compILargsCount)
    {
        varNum = compMapILargNum(ilVarNum);

        noway_assert(lvaGetDesc(varNum)->IsParam());
    }
    else if (ilVarNum < compILlocalsCount)
    {
        varNum = info.compArgsCount + ilVarNum - info.compILargsCount;

        noway_assert(!lvaGetDesc(varNum)->IsParam());
    }
    else
    {
        unreached();
    }

    noway_assert(varNum < info.compLocalsCount);

    return varNum;
}

void Compiler::compInitSortedScopeLists()
{
    assert(info.compVarScopesCount != 0);

    compEnterScopeList = new (this, CMK_DebugInfo) VarScopeDsc*[info.compVarScopesCount];
    compExitScopeList  = new (this, CMK_DebugInfo) VarScopeDsc*[info.compVarScopesCount];

    for (unsigned i = 0; i < info.compVarScopesCount; i++)
    {
        compEnterScopeList[i] = &info.compVarScopes[i];
        compExitScopeList[i]  = &info.compVarScopes[i];
    }

    jitstd::sort(compEnterScopeList, compEnterScopeList + info.compVarScopesCount,
                 [](const VarScopeDsc* elem1, const VarScopeDsc* elem2) {
                     return elem1->startOffset < elem2->startOffset;
                 });

    jitstd::sort(compExitScopeList, compExitScopeList + info.compVarScopesCount,
                 [](const VarScopeDsc* elem1, const VarScopeDsc* elem2) {
                     return elem1->endOffset < elem2->endOffset;
                 });
}

VarScopeDsc* Compiler::compGetNextEnterScope(unsigned offs, unsigned* nextEnterScope)
{
    if (*nextEnterScope < info.compVarScopesCount)
    {
        if (compVarScopeExtended)
        {
            if (offs == 0)
            {
                return &info.compVarScopes[(*nextEnterScope)++];
            }
        }
        else
        {
            unsigned nextEnterOffs = compEnterScopeList[*nextEnterScope]->startOffset;
            assert(offs <= nextEnterOffs);

            if (nextEnterOffs == offs)
            {
                return compEnterScopeList[(*nextEnterScope)++];
            }
        }
    }

    return nullptr;
}

VarScopeDsc* Compiler::compGetNextExitScope(unsigned offs, unsigned* nextExitScope)
{
    if (*nextExitScope < info.compVarScopesCount)
    {
        if (compVarScopeExtended)
        {
            if (offs == info.compILCodeSize)
            {
                return &info.compVarScopes[(*nextExitScope)++];
            }
        }
        else
        {
            unsigned nextExitOffs = compExitScopeList[*nextExitScope]->endOffset;
            assert(offs <= nextExitOffs);

            if (nextExitOffs == offs)
            {
                return compExitScopeList[(*nextExitScope)++];
            }
        }
    }

    return nullptr;
}

VarScopeDsc* Compiler::compGetNextEnterScopeScan(unsigned offs, unsigned* nextEnterScope)
{
    if (*nextEnterScope < info.compVarScopesCount)
    {
        if (compVarScopeExtended)
        {
            return &info.compVarScopes[(*nextEnterScope)++];
        }
        else if (offs >= compEnterScopeList[*nextEnterScope]->startOffset)
        {
            return compEnterScopeList[(*nextEnterScope)++];
        }
    }

    return nullptr;
}

VarScopeDsc* Compiler::compGetNextExitScopeScan(unsigned offs, unsigned* nextExitScope)
{
    if (*nextExitScope < info.compVarScopesCount)
    {
        if (compVarScopeExtended)
        {
            if (offs >= info.compILCodeSize)
            {
                return &info.compVarScopes[(*nextExitScope)++];
            }
        }
        else if (offs >= compExitScopeList[*nextExitScope]->endOffset)
        {
            return compExitScopeList[(*nextExitScope)++];
        }
    }

    return nullptr;
}

#ifdef DEBUG
void Compiler::compDispLocalVars()
{
    printf("info.compVarScopesCount = %d\n", info.compVarScopesCount);

    if (info.compVarScopesCount > 0)
    {
        printf("    \tVarNum \tLVNum \t      Name \tBeg \tEnd\n");
    }

    for (unsigned i = 0; i < info.compVarScopesCount; i++)
    {
        VarScopeDsc& scope = info.compVarScopes[i];
        const char*  name  = gtGetLclVarName(scope.lclNum);

        printf("%2u: " FMT_LCL " %10s %03Xh %03Xh\n", i, scope.lclNum, name == nullptr ? "UNKNOWN" : name,
               scope.startOffset, scope.endOffset);
    }
}
#endif // DEBUG

/*****************************************************************************
 *
 *                      ICorJitInfo wrapper functions
 *
 * In many cases here, we don't tell the VM about various unwind or EH information if
 * we're an altjit for an unexpected architecture. If it's not a same architecture JIT
 * (e.g., host AMD64, target ARM64), then VM will get confused anyway.
 */

#ifdef TARGET_AMD64
bool Compiler::eeIsRIPRelativeAddress(void* addr) const
{
    return info.compMatchedVM && info.compCompHnd->getRelocTypeHint(addr) == IMAGE_REL_BASED_REL32;
}

bool Compiler::IsRIPRelativeAddress(GenTreeIntCon* intCon) const
{
#ifdef DEBUG
    if (!opts.enableRIPRelativeAddressing)
    {
        return false;
    }
#endif

    void* addr = reinterpret_cast<void*>(intCon->GetValue());

    if (opts.compReloc)
    {
        // In the crossgen case the only addresses that we care about are those derived from handles
        // returned by crossgen. The handles aren't real addresses but they should eventually resolve
        // to addresses within the generated PE image, which is limited to 2GB in size. This means
        // that every handle is expected to be RIP relative (but we still ask crossgen to avoid making
        // assumptions).
        // User code could contain hardcoded addresses but such addresses can't be RIP relative since
        // the load address of the generated PE image is arbitrary.

        return intCon->IsHandle() && eeIsRIPRelativeAddress(addr);
    }

    // At JIT time we get real memory addresses instead of handles, but we don't know the address of
    // the generated code yet. The runtime tries to keep code and data close enough and optimistically
    // assumes that RIP relative addressing can be used. Once this assumption fails, it recompiles the
    // method that triggered the failure and rejects future attempts of using RIP relative addressing.
    //
    // TODO-MIKE-Review: This code also allows addresses that happen to fit in disp32 even if these
    // aren't actually RIP relative. This is fine, as the callers only care if the address can be made
    // part of an address mode, but it's likely pointless as most of the time code and data end up
    // above the 2GB range. It may also be risky, as this requires a different address mode encoding
    // and that may not be tested properly.

    return FitsIn<int32_t>(reinterpret_cast<int64_t>(addr)) || eeIsRIPRelativeAddress(addr);
}
#endif

#ifdef TARGET_ARM
bool Compiler::eeIsThumbBranch24TargetAddress(void* target)
{
    assert(info.compMatchedVM);
    return info.compCompHnd->getRelocTypeHint(target) == IMAGE_REL_BASED_THUMB_BRANCH24;
}
#endif

/*****************************************************************************
 *
 *                      ICorStaticInfo wrapper functions
 */

bool Compiler::eeTryResolveToken(CORINFO_RESOLVED_TOKEN* resolvedToken)
{
    return info.compCompHnd->tryResolveToken(resolvedToken);
}

bool Compiler::eeRunWithErrorTrapImp(void (*function)(void*), void* param)
{
    return info.compCompHnd->runWithErrorTrap(function, param);
}

bool Compiler::eeRunWithSPMIErrorTrapImp(void (*function)(void*), void* param)
{
    return info.compCompHnd->runWithSPMIErrorTrap(function, param);
}

/*****************************************************************************
 *
 *                      Utility functions
 */

#if defined(DEBUG) || defined(FEATURE_JIT_METHOD_PERF) || defined(FEATURE_SIMD)

/*****************************************************************************
*
*  Filter wrapper to handle exception filtering.
*  On Unix compilers don't support SEH.
*/

struct FilterSuperPMIExceptionsParam_ee_il
{
    Compiler*             pThis;
    CompiledMethodInfo*   pJitInfo;
    CORINFO_FIELD_HANDLE  field;
    CORINFO_METHOD_HANDLE method;
    CORINFO_CLASS_HANDLE  clazz;
    const char**          classNamePtr;
    const char*           fieldOrMethodOrClassNamePtr;
    EXCEPTION_POINTERS    exceptionPointers;
};

static bool eeIsNativeMethod(CORINFO_METHOD_HANDLE method)
{
    return ((((size_t)method) & 0x2) == 0x2);
}

static CORINFO_METHOD_HANDLE eeGetMethodHandleForNative(CORINFO_METHOD_HANDLE method)
{
    assert((((size_t)method) & 0x3) == 0x2);
    return (CORINFO_METHOD_HANDLE)(((size_t)method) & ~0x3);
}

const char* Compiler::eeGetMethodName(CORINFO_METHOD_HANDLE method, const char** classNamePtr)
{
    static const char* const jitHelperName[CORINFO_HELP_COUNT]{
#define JITHELPER(code, pfnHelper, sig) #code,
#define DYNAMICJITHELPER(code, pfnHelper, sig) #code,
#include "jithelpers.h"
    };

    if (eeGetHelperNum(method) != CORINFO_HELP_UNDEF)
    {
        if (classNamePtr != nullptr)
        {
            *classNamePtr = "HELPER";
        }
        CorInfoHelpFunc ftnNum = eeGetHelperNum(method);
        const char*     name   = info.compCompHnd->getHelperName(ftnNum);

        // If it's something unknown from a RET VM, or from SuperPMI, then use our own helper name table.
        if ((strcmp(name, "AnyJITHelper") == 0) || (strcmp(name, "Yickish helper name") == 0))
        {
            if ((unsigned)ftnNum < CORINFO_HELP_COUNT)
            {
                name = jitHelperName[ftnNum];
            }
        }
        return name;
    }

    if (eeIsNativeMethod(method))
    {
        if (classNamePtr != nullptr)
        {
            *classNamePtr = "NATIVE";
        }
        method = eeGetMethodHandleForNative(method);
    }

    FilterSuperPMIExceptionsParam_ee_il param;

    param.pThis        = this;
    param.pJitInfo     = &info;
    param.method       = method;
    param.classNamePtr = classNamePtr;

    bool success = eeRunWithSPMIErrorTrap<FilterSuperPMIExceptionsParam_ee_il>(
        [](FilterSuperPMIExceptionsParam_ee_il* pParam) {
            pParam->fieldOrMethodOrClassNamePtr =
                pParam->pJitInfo->compCompHnd->getMethodName(pParam->method, pParam->classNamePtr);
        },
        &param);

    if (!success)
    {
        if (param.classNamePtr != nullptr)
        {
            *(param.classNamePtr) = "hackishClassName";
        }

        param.fieldOrMethodOrClassNamePtr = "hackishMethodName";
    }

    return param.fieldOrMethodOrClassNamePtr;
}

const char* Compiler::eeGetFieldName(CORINFO_FIELD_HANDLE field, const char** classNamePtr)
{
    FilterSuperPMIExceptionsParam_ee_il param;

    param.pThis        = this;
    param.pJitInfo     = &info;
    param.field        = field;
    param.classNamePtr = classNamePtr;

    bool success = eeRunWithSPMIErrorTrap<FilterSuperPMIExceptionsParam_ee_il>(
        [](FilterSuperPMIExceptionsParam_ee_il* pParam) {
            pParam->fieldOrMethodOrClassNamePtr =
                pParam->pJitInfo->compCompHnd->getFieldName(pParam->field, pParam->classNamePtr);
        },
        &param);

    if (!success)
    {
        param.fieldOrMethodOrClassNamePtr = "hackishFieldName";
    }

    return param.fieldOrMethodOrClassNamePtr;
}

const char* Compiler::eeGetClassName(CORINFO_CLASS_HANDLE clsHnd)
{
    FilterSuperPMIExceptionsParam_ee_il param;

    param.pThis    = this;
    param.pJitInfo = &info;
    param.clazz    = clsHnd;

    bool success = eeRunWithSPMIErrorTrap<FilterSuperPMIExceptionsParam_ee_il>(
        [](FilterSuperPMIExceptionsParam_ee_il* pParam) {
            pParam->fieldOrMethodOrClassNamePtr = pParam->pJitInfo->compCompHnd->getClassName(pParam->clazz);
        },
        &param);

    if (!success)
    {
        param.fieldOrMethodOrClassNamePtr = "hackishClassName";
    }
    return param.fieldOrMethodOrClassNamePtr;
}

const char* Compiler::eeGetSimpleClassName(CORINFO_CLASS_HANDLE clsHnd)
{
    FilterSuperPMIExceptionsParam_ee_il param;

    param.pThis    = this;
    param.pJitInfo = &info;
    param.clazz    = clsHnd;

    bool success = eeRunWithSPMIErrorTrap<FilterSuperPMIExceptionsParam_ee_il>(
        [](FilterSuperPMIExceptionsParam_ee_il* pParam) {
            if (pParam->pJitInfo->compCompHnd->getTypeInstantiationArgument(pParam->clazz, 0) == NO_CLASS_HANDLE)
            {
                pParam->fieldOrMethodOrClassNamePtr =
                    pParam->pJitInfo->compCompHnd->getClassNameFromMetadata(pParam->clazz, nullptr);
            }
            else
            {
                pParam->fieldOrMethodOrClassNamePtr = pParam->pJitInfo->compCompHnd->getClassName(pParam->clazz);
            }
        },
        &param);

    if (!success)
    {
        param.fieldOrMethodOrClassNamePtr = "hackishClassName";
    }
    return param.fieldOrMethodOrClassNamePtr;
}

const char* Compiler::eeGetMethodFullName(CORINFO_METHOD_HANDLE hnd)
{
    const char* className;
    const char* methodName = eeGetMethodName(hnd, &className);
    if ((eeGetHelperNum(hnd) != CORINFO_HELP_UNDEF) || eeIsNativeMethod(hnd))
    {
        return methodName;
    }

    struct FilterSuperPMIExceptionsParam_eeinterface
    {
        Compiler*               pThis;
        CompiledMethodInfo*     pJitInfo;
        bool                    hasThis;
        size_t                  siglength;
        CORINFO_SIG_INFO        sig;
        CORINFO_ARG_LIST_HANDLE argLst;
        CORINFO_METHOD_HANDLE   hnd;
        const char*             returnType;
        const char**            pArgNames;
        EXCEPTION_POINTERS      exceptionPointers;
    };

    FilterSuperPMIExceptionsParam_eeinterface param;
    param.returnType = nullptr;
    param.pThis      = this;
    param.hasThis    = false;
    param.siglength  = 0;
    param.hnd        = hnd;
    param.pJitInfo   = &info;

    size_t   length = 0;
    unsigned i;

    /* Generating the full signature is a two-pass process. First we have to walk
       the components in order to assess the total size, then we allocate the buffer
       and copy the elements into it.
     */

    /* Right now there is a race-condition in the EE, className can be nullptr */

    /* initialize length with length of className and '.' */

    if (className)
    {
        length = strlen(className) + 1;
    }
    else
    {
        assert(strlen("<NULL>.") == 7);
        length = 7;
    }

    /* add length of methodName and opening bracket */
    length += strlen(methodName) + 1;

    bool success = eeRunWithSPMIErrorTrap<FilterSuperPMIExceptionsParam_eeinterface>(
        [](FilterSuperPMIExceptionsParam_eeinterface* pParam) {

            /* figure out the signature */

            pParam->pThis->eeGetMethodSig(pParam->hnd, &pParam->sig);

            // allocate space to hold the class names for each of the parameters

            if (pParam->sig.numArgs > 0)
            {
                pParam->pArgNames =
                    pParam->pThis->getAllocator(CMK_DebugOnly).allocate<const char*>(pParam->sig.numArgs);
            }
            else
            {
                pParam->pArgNames = nullptr;
            }

            unsigned i;
            pParam->argLst = pParam->sig.args;

            for (i = 0; i < pParam->sig.numArgs; i++)
            {
                var_types type = pParam->pThis->eeGetArgType(pParam->argLst, &pParam->sig);
                switch (type)
                {
                    case TYP_REF:
                    case TYP_STRUCT:
                    {
                        CORINFO_CLASS_HANDLE clsHnd = pParam->pThis->eeGetArgClass(&pParam->sig, pParam->argLst);
                        // For some SIMD struct types we can get a nullptr back from eeGetArgClass on Linux/X64
                        if (clsHnd != NO_CLASS_HANDLE)
                        {
                            const char* clsName = pParam->pThis->eeGetClassName(clsHnd);
                            if (clsName != nullptr)
                            {
                                pParam->pArgNames[i] = clsName;
                                break;
                            }
                        }
                    }
                        FALLTHROUGH;
                    default:
                        pParam->pArgNames[i] = varTypeName(type);
                        break;
                }
                pParam->siglength += strlen(pParam->pArgNames[i]);
                pParam->argLst = pParam->pJitInfo->compCompHnd->getArgNext(pParam->argLst);
            }

            /* add ',' if there is more than one argument */

            if (pParam->sig.numArgs > 1)
            {
                pParam->siglength += (pParam->sig.numArgs - 1);
            }

            var_types retType = JITtype2varType(pParam->sig.retType);
            if (retType != TYP_VOID)
            {
                switch (retType)
                {
                    case TYP_REF:
                    case TYP_STRUCT:
                    {
                        CORINFO_CLASS_HANDLE clsHnd = pParam->sig.retTypeClass;
                        if (clsHnd != NO_CLASS_HANDLE)
                        {
                            const char* clsName = pParam->pThis->eeGetClassName(clsHnd);
                            if (clsName != nullptr)
                            {
                                pParam->returnType = clsName;
                                break;
                            }
                        }
                    }
                        FALLTHROUGH;
                    default:
                        pParam->returnType = varTypeName(retType);
                        break;
                }
                pParam->siglength += strlen(pParam->returnType) + 1; // don't forget the delimiter ':'
            }

            // Does it have a 'this' pointer? Don't count explicit this, which has the this pointer type as the first
            // element of the arg type list
            if (pParam->sig.hasThis() && !pParam->sig.hasExplicitThis())
            {
                assert(strlen(":this") == 5);
                pParam->siglength += 5;
                pParam->hasThis = true;
            }
        },
        &param);

    if (!success)
    {
        param.siglength = 0;
    }

    /* add closing bracket and null terminator */

    length += param.siglength + 2;

    char* retName = getAllocator(CMK_DebugOnly).allocate<char>(length);

    /* Now generate the full signature string in the allocated buffer */

    if (className)
    {
        strcpy_s(retName, length, className);
        strcat_s(retName, length, ":");
    }
    else
    {
        strcpy_s(retName, length, "<NULL>.");
    }

    strcat_s(retName, length, methodName);

    // append the signature
    strcat_s(retName, length, "(");

    if (param.siglength > 0)
    {
        param.argLst = param.sig.args;

        for (i = 0; i < param.sig.numArgs; i++)
        {
            var_types type = eeGetArgType(param.argLst, &param.sig);
            strcat_s(retName, length, param.pArgNames[i]);
            param.argLst = info.compCompHnd->getArgNext(param.argLst);
            if (i + 1 < param.sig.numArgs)
            {
                strcat_s(retName, length, ",");
            }
        }
    }

    strcat_s(retName, length, ")");

    if (param.returnType != nullptr)
    {
        strcat_s(retName, length, ":");
        strcat_s(retName, length, param.returnType);
    }

    if (param.hasThis)
    {
        strcat_s(retName, length, ":this");
    }

    assert(strlen(retName) == (length - 1));

    return (retName);
}

#endif // DEBUG || FEATURE_JIT_METHOD_PERF || FEATURE_SIMD

#ifdef DEBUG

const WCHAR* Compiler::eeGetCPString(void* strHandle)
{
#ifdef HOST_UNIX
    return nullptr;
#else
    // TODO-MIKE-Cleanup: This stuff is as dodgy as it gets. The string may live in the GC heap
    // so it can move around. Avoid blowing up the JIT if the string memory happens to be freed
    // and make sure the returned string is nul terminated, even if we read garbage from memory.
    // What this should do is to use getStringLiteral to get the string literal itself instead
    // of the string object. But we don't have the metadata token here so this will have to do,
    // for now...

    static WCHAR    buffers[4][256];
    static unsigned freeBuffer;

    __try
    {
        CORINFO_String* str    = static_cast<CORINFO_String*>(*reinterpret_cast<void**>(strHandle));
        size_t          length = str->stringLen;

        if ((length + 1 > _countof(buffers[0])) || (str->chars[str->stringLen] != 0))
        {
            return nullptr;
        }

        WCHAR* buffer = buffers[freeBuffer++ % _countof(buffers)];
        memcpy(buffer, str->chars, length * sizeof(WCHAR));
        buffer[length] = 0;
        return buffer;
    }
    __except (EXCEPTION_EXECUTE_HANDLER)
    {
        return nullptr;
    }
#endif // HOST_UNIX
}

#endif // DEBUG
