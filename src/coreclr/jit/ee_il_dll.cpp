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
#ifdef _MSC_VER
#pragma hdrstop
#endif
#include "emit.h"
#include "corexcep.h"
#include "jitstd/algorithm.h"
#include "codegen.h"

#if !defined(HOST_UNIX)
#include <io.h>    // For _dup, _setmode
#include <fcntl.h> // For _O_TEXT
#include <errno.h> // For EINVAL
#endif

#ifndef DLLEXPORT
#define DLLEXPORT
#endif // !DLLEXPORT

/*****************************************************************************/

FILE* jitstdout = nullptr;

ICorJitHost*   g_jitHost        = nullptr;
static CILJit* ILJitter         = nullptr; // The one and only JITTER I return
bool           g_jitInitialized = false;

INDEBUG(extern ConfigMethodRange fJitStressRange;)

/*****************************************************************************/

extern "C" DLLEXPORT void jitStartup(ICorJitHost* jitHost)
{
    if (g_jitInitialized)
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

    g_jitInitialized = true;
}

void jitShutdown(bool processIsTerminating)
{
    if (!g_jitInitialized)
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

    g_jitInitialized = false;
}

/*****************************************************************************/

struct CILJitSingletonAllocator
{
    int x;
};
const CILJitSingletonAllocator CILJitSingleton = {0};

void* __cdecl operator new(size_t, const CILJitSingletonAllocator&)
{
    static char CILJitBuff[sizeof(CILJit)];
    return CILJitBuff;
}

DLLEXPORT ICorJitCompiler* getJit()
{
    if (!g_jitInitialized)
    {
        return nullptr;
    }

    if (ILJitter == nullptr)
    {
        ILJitter = new (CILJitSingleton) CILJit();
    }
    return (ILJitter);
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

/*****************************************************************************
 *
 *                  Debugging support - Local var info
 */

void CodeGen::eeSetLVcount(unsigned count)
{
    assert(compiler->opts.compScopeInfo);

    JITDUMP("VarLocInfo count is %d\n", count);

    eeVarsCount = count;
    if (eeVarsCount)
    {
        eeVars =
            static_cast<VarResultInfo*>(compiler->info.compCompHnd->allocateArray(eeVarsCount * sizeof(eeVars[0])));
    }
    else
    {
        eeVars = nullptr;
    }
}

void CodeGen::eeSetLVinfo(unsigned                          which,
                          UNATIVE_OFFSET                    startOffs,
                          UNATIVE_OFFSET                    length,
                          unsigned                          varNum,
                          const CodeGenInterface::siVarLoc& varLoc)
{
    // ICorDebugInfo::VarLoc and CodeGenInterface::siVarLoc have to overlap
    // This is checked in siInit()

    assert(compiler->opts.compScopeInfo);
    assert(eeVarsCount > 0);
    assert(which < eeVarsCount);

    if (eeVars != nullptr)
    {
        eeVars[which].startOffset = startOffs;
        eeVars[which].endOffset   = startOffs + length;
        eeVars[which].varNumber   = varNum;
        eeVars[which].loc         = varLoc;
    }
}

void CodeGen::eeSetLVdone()
{
    // necessary but not sufficient condition that the 2 struct definitions overlap
    assert(sizeof(eeVars[0]) == sizeof(ICorDebugInfo::NativeVarInfo));
    assert(compiler->opts.compScopeInfo);

#ifdef DEBUG
    if (verbose || compiler->opts.dspDebugInfo)
    {
        eeDispVars(compiler->info.compMethodHnd, eeVarsCount, (ICorDebugInfo::NativeVarInfo*)eeVars);
    }
#endif // DEBUG

    compiler->info.compCompHnd->setVars(compiler->info.compMethodHnd, eeVarsCount,
                                        (ICorDebugInfo::NativeVarInfo*)eeVars);

    eeVars = nullptr; // We give up ownership after setVars()
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
#ifdef USING_SCOPE_INFO
        compInitVarScopeMap();
#endif

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
        scopes[i].vsdLifeBeg = 0;
        scopes[i].vsdLifeEnd = info.compILCodeSize;
        scopes[i].vsdVarNum  = i;
        scopes[i].vsdLVnum   = i;

        INDEBUG(scopes[i].vsdName = gtGetLclVarName(i));
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

    for (unsigned i = 0; i < varInfoCount; i++, vars++)
    {
        JITDUMP("var:%d start:%d end:%d\n", vars->varNumber, vars->startOffset, vars->endOffset);

        if (vars->startOffset >= vars->endOffset)
        {
            continue;
        }

        assert(vars->startOffset <= info.compILCodeSize);
        assert(vars->endOffset <= info.compILCodeSize);

        scopes->vsdLifeBeg = vars->startOffset;
        scopes->vsdLifeEnd = vars->endOffset;
        scopes->vsdLVnum   = i;
        scopes->vsdVarNum  = compMapILvarNum(vars->varNumber);

        INDEBUG(scopes->vsdName = gtGetLclVarName(scopes->vsdVarNum));

        scopes++;
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
            varInfoProvided[info.compVarScopes[i].vsdVarNum] = true;
        }

        for (unsigned varNum = 0; varNum < info.compLocalsCount; varNum++)
        {
            if (varInfoProvided[varNum])
            {
                continue;
            }

            scopes->vsdLifeBeg = 0;
            scopes->vsdLifeEnd = info.compILCodeSize;
            scopes->vsdVarNum  = varNum;
            scopes->vsdLVnum   = info.compVarScopesCount;

            INDEBUG(scopes->vsdName = gtGetLclVarName(scopes->vsdVarNum));

            scopes++;
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
                     return elem1->vsdLifeBeg < elem2->vsdLifeBeg;
                 });

    jitstd::sort(compExitScopeList, compExitScopeList + info.compVarScopesCount,
                 [](const VarScopeDsc* elem1, const VarScopeDsc* elem2) {
                     return elem1->vsdLifeEnd < elem2->vsdLifeEnd;
                 });
}

void Compiler::compResetScopeLists()
{
    if (info.compVarScopesCount == 0)
    {
        return;
    }

    assert(compVarScopeExtended || (compEnterScopeList != nullptr) && (compExitScopeList != nullptr));

    compNextEnterScope = 0;
    compNextExitScope  = 0;
}

VarScopeDsc* Compiler::compGetNextEnterScope(unsigned offs)
{
    if (compNextEnterScope < info.compVarScopesCount)
    {
        if (compVarScopeExtended)
        {
            if (offs == 0)
            {
                return &info.compVarScopes[compNextEnterScope++];
            }
        }
        else
        {
            unsigned nextEnterOffs = compEnterScopeList[compNextEnterScope]->vsdLifeBeg;
            assert(offs <= nextEnterOffs);

            if (nextEnterOffs == offs)
            {
                return compEnterScopeList[compNextEnterScope++];
            }
        }
    }

    return nullptr;
}

VarScopeDsc* Compiler::compGetNextExitScope(unsigned offs)
{
    if (compNextExitScope < info.compVarScopesCount)
    {
        if (compVarScopeExtended)
        {
            if (offs == info.compILCodeSize)
            {
                return &info.compVarScopes[compNextExitScope++];
            }
        }
        else
        {
            unsigned nextExitOffs = compExitScopeList[compNextExitScope]->vsdLifeEnd;
            assert(offs <= nextExitOffs);

            if (nextExitOffs == offs)
            {
                return compExitScopeList[compNextExitScope++];
            }
        }
    }

    return nullptr;
}

VarScopeDsc* Compiler::compGetNextEnterScopeScan(unsigned offs)
{
    if (compNextEnterScope < info.compVarScopesCount)
    {
        if (compVarScopeExtended)
        {
            return &info.compVarScopes[compNextEnterScope++];
        }
        else if (offs >= compEnterScopeList[compNextEnterScope]->vsdLifeBeg)
        {
            return compEnterScopeList[compNextEnterScope++];
        }
    }

    return nullptr;
}

VarScopeDsc* Compiler::compGetNextExitScopeScan(unsigned offs)
{
    if (compNextExitScope < info.compVarScopesCount)
    {
        if (compVarScopeExtended)
        {
            if (offs >= info.compILCodeSize)
            {
                return &info.compVarScopes[compNextExitScope++];
            }
        }
        else if (offs >= compExitScopeList[compNextExitScope]->vsdLifeEnd)
        {
            return compExitScopeList[compNextExitScope++];
        }
    }

    return nullptr;
}

#ifdef USING_SCOPE_INFO
VarScopeDsc* Compiler::compFindLocalVarLinear(unsigned varNum, unsigned offs)
{
    for (unsigned i = 0; i < info.compVarScopesCount; i++)
    {
        VarScopeDsc& dsc = info.compVarScopes[i];

        if ((dsc.vsdVarNum == varNum) && (dsc.vsdLifeBeg <= offs) && (dsc.vsdLifeEnd > offs))
        {
            return &dsc;
        }
    }

    return nullptr;
}

VarScopeDsc* Compiler::compFindLocalVar(unsigned varNum, unsigned offs)
{
    if (compVarScopeExtended)
    {
        assert(info.compVarScopes[varNum].vsdVarNum == varNum);
        assert((info.compVarScopes[varNum].vsdLifeBeg == 0) && (offs <= info.compVarScopes[varNum].vsdLifeEnd));

        return &info.compVarScopes[varNum];
    }

    if (compVarScopeMap == nullptr)
    {
        return compFindLocalVarLinear(varNum, offs);
    }

    VarScopeDsc* scope = compFindLocalVarMapped(varNum, offs);
    assert(scope == compFindLocalVarLinear(varNum, offs));
    return scope;
}

void Compiler::compInitVarScopeMap()
{
    assert(compVarScopeMap == nullptr);

    if (info.compVarScopesCount < 32)
    {
        return;
    }

    compVarScopeMap = new (getAllocator(CMK_DebugInfo))
        JitHashTable<unsigned, JitSmallPrimitiveKeyFuncs<unsigned>, VarScopeListNode*>(getAllocator(CMK_DebugInfo));
    // 599 prime to limit huge allocations; for ex: duplicated scopes on single var.
    compVarScopeMap->Reallocate(min(info.compVarScopesCount, 599));

    for (unsigned i = 0; i < info.compVarScopesCount; ++i)
    {
        VarScopeListNode** head = compVarScopeMap->Emplace(info.compVarScopes[i].vsdVarNum);

        *head = new (getAllocator(CMK_DebugInfo)) VarScopeListNode(&info.compVarScopes[i], *head);
    }
}

VarScopeDsc* Compiler::compFindLocalVarMapped(unsigned varNum, unsigned offs)
{
    VarScopeListNode* node;

    if (compVarScopeMap->Lookup(varNum, &node))
    {
        for (; node != nullptr; node = node->next)
        {
            if ((node->scope->vsdLifeBeg <= offs) && (node->scope->vsdLifeEnd > offs))
            {
                return node->scope;
            }
        }
    }

    return nullptr;
}

bool Compiler::compVerifyVarScopes()
{
    if (compVarScopeExtended)
    {
        return true;
    }

    // No entries with overlapping lives should have the same slot.

    for (unsigned i = 0; i < info.compVarScopesCount; i++)
    {
        for (unsigned j = i + 1; j < compiler->info.compVarScopesCount; j++)
        {
            unsigned slot1 = info.compVarScopes[i].vsdVarNum;
            unsigned beg1  = info.compVarScopes[i].vsdLifeBeg;
            unsigned end1  = info.compVarScopes[i].vsdLifeEnd;

            unsigned slot2 = info.compVarScopes[j].vsdVarNum;
            unsigned beg2  = info.compVarScopes[j].vsdLifeBeg;
            unsigned end2  = info.compVarScopes[j].vsdLifeEnd;

            if (slot1 == slot2 && (end1 > beg2 && beg1 < end2))
            {
                return false;
            }
        }
    }

    return true;
}
#endif // USING_SCOPE_INFO

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
        VarScopeDsc* varScope = &info.compVarScopes[i];
        printf("%2d: \t%02Xh \t%02Xh \t%10s \t%03Xh   \t%03Xh\n", i, varScope->vsdVarNum, varScope->vsdLVnum,
               varScope->vsdName == nullptr ? "UNKNOWN" : varScope->vsdName, varScope->vsdLifeBeg,
               varScope->vsdLifeEnd);
    }
}

void CodeGen::eeDispVar(ICorDebugInfo::NativeVarInfo* var)
{
    const char* name = nullptr;

    if (var->varNumber == (DWORD)ICorDebugInfo::VARARGS_HND_ILNUM)
    {
        name = "varargsHandle";
    }
    else if (var->varNumber == (DWORD)ICorDebugInfo::RETBUF_ILNUM)
    {
        name = "retBuff";
    }
    else if (var->varNumber == (DWORD)ICorDebugInfo::TYPECTXT_ILNUM)
    {
        name = "typeCtx";
    }
    printf("%3d(%10s) : From %08Xh to %08Xh, in ", var->varNumber, (name == nullptr) ? "UNKNOWN" : name,
           var->startOffset, var->endOffset);

    switch ((CodeGenInterface::siVarLocType)var->loc.vlType)
    {
        case CodeGenInterface::VLT_REG:
        case CodeGenInterface::VLT_REG_BYREF:
        case CodeGenInterface::VLT_REG_FP:
            printf("%s", getRegName(var->loc.vlReg.vlrReg));
            if (var->loc.vlType == (ICorDebugInfo::VarLocType)CodeGenInterface::VLT_REG_BYREF)
            {
                printf(" byref");
            }
            break;

        case CodeGenInterface::VLT_STK:
        case CodeGenInterface::VLT_STK_BYREF:
            if ((int)var->loc.vlStk.vlsBaseReg != (int)ICorDebugInfo::REGNUM_AMBIENT_SP)
            {
                printf("%s[%d] (1 slot)", getRegName(var->loc.vlStk.vlsBaseReg), var->loc.vlStk.vlsOffset);
            }
            else
            {
                printf(STR_SPBASE "'[%d] (1 slot)", var->loc.vlStk.vlsOffset);
            }
            if (var->loc.vlType == (ICorDebugInfo::VarLocType)CodeGenInterface::VLT_REG_BYREF)
            {
                printf(" byref");
            }
            break;

        case CodeGenInterface::VLT_REG_REG:
            printf("%s-%s", getRegName(var->loc.vlRegReg.vlrrReg1), getRegName(var->loc.vlRegReg.vlrrReg2));
            break;

#ifndef TARGET_AMD64
        case CodeGenInterface::VLT_REG_STK:
            if ((int)var->loc.vlRegStk.vlrsStk.vlrssBaseReg != (int)ICorDebugInfo::REGNUM_AMBIENT_SP)
            {
                printf("%s-%s[%d]", getRegName(var->loc.vlRegStk.vlrsReg),
                       getRegName(var->loc.vlRegStk.vlrsStk.vlrssBaseReg), var->loc.vlRegStk.vlrsStk.vlrssOffset);
            }
            else
            {
                printf("%s-" STR_SPBASE "'[%d]", getRegName(var->loc.vlRegStk.vlrsReg),
                       var->loc.vlRegStk.vlrsStk.vlrssOffset);
            }
            break;

        case CodeGenInterface::VLT_STK_REG:
            unreached(); // unexpected

        case CodeGenInterface::VLT_STK2:
            if ((int)var->loc.vlStk2.vls2BaseReg != (int)ICorDebugInfo::REGNUM_AMBIENT_SP)
            {
                printf("%s[%d] (2 slots)", getRegName(var->loc.vlStk2.vls2BaseReg), var->loc.vlStk2.vls2Offset);
            }
            else
            {
                printf(STR_SPBASE "'[%d] (2 slots)", var->loc.vlStk2.vls2Offset);
            }
            break;

        case CodeGenInterface::VLT_FPSTK:
            printf("ST(L-%d)", var->loc.vlFPstk.vlfReg);
            break;

        case CodeGenInterface::VLT_FIXED_VA:
            printf("fxd_va[%d]", var->loc.vlFixedVarArg.vlfvOffset);
            break;
#endif // !TARGET_AMD64

        default:
            unreached(); // unexpected
    }

    printf("\n");
}

// Same parameters as ICorStaticInfo::setVars().
void CodeGen::eeDispVars(CORINFO_METHOD_HANDLE ftn, ULONG32 cVars, ICorDebugInfo::NativeVarInfo* vars)
{
    BitVecTraits varTraits(compiler->lvaCount, compiler);
    BitVec       uniqueVars = BitVecOps::MakeEmpty(&varTraits);
    unsigned     varCount   = 0;

    for (unsigned i = 0; i < cVars; i++)
    {
        if ((vars[i].varNumber < compiler->lvaCount) &&
            BitVecOps::TryAddElemD(&varTraits, uniqueVars, vars[i].varNumber))
        {
            varCount++;
        }
    }

    printf("; Variable debug info: %d live ranges, %d vars for method %s\n", cVars, varCount,
           compiler->info.compFullName);

    for (unsigned i = 0; i < cVars; i++)
    {
        eeDispVar(&vars[i]);
    }
}
#endif // DEBUG

/*****************************************************************************
 *
 *                  Debugging support - Line number info
 */

void CodeGen::eeSetLIcount(unsigned count)
{
    assert(compiler->opts.compDbgInfo);

    eeBoundariesCount = count;
    if (eeBoundariesCount)
    {
        eeBoundaries = static_cast<boundariesDsc*>(
            compiler->info.compCompHnd->allocateArray(eeBoundariesCount * sizeof(eeBoundaries[0])));
    }
    else
    {
        eeBoundaries = nullptr;
    }
}

void CodeGen::eeSetLIinfo(
    unsigned which, UNATIVE_OFFSET nativeOffset, IL_OFFSET ilOffset, bool stkEmpty, bool callInstruction)
{
    assert(compiler->opts.compDbgInfo);
    assert(eeBoundariesCount > 0);
    assert(which < eeBoundariesCount);

    if (eeBoundaries != nullptr)
    {
        eeBoundaries[which].nativeIP     = nativeOffset;
        eeBoundaries[which].ilOffset     = ilOffset;
        eeBoundaries[which].sourceReason = stkEmpty ? ICorDebugInfo::STACK_EMPTY : 0;
        eeBoundaries[which].sourceReason |= callInstruction ? ICorDebugInfo::CALL_INSTRUCTION : 0;
    }
}

void CodeGen::eeSetLIdone()
{
    assert(compiler->opts.compDbgInfo);

#if defined(DEBUG)
    if (verbose || compiler->opts.dspDebugInfo)
    {
        eeDispLineInfos();
    }
#endif // DEBUG

    // necessary but not sufficient condition that the 2 struct definitions overlap
    assert(sizeof(eeBoundaries[0]) == sizeof(ICorDebugInfo::OffsetMapping));

    compiler->info.compCompHnd->setBoundaries(compiler->info.compMethodHnd, eeBoundariesCount,
                                              (ICorDebugInfo::OffsetMapping*)eeBoundaries);

    eeBoundaries = nullptr; // we give up ownership after setBoundaries();
}

#ifdef DEBUG

void CodeGen::eeDispILOffs(IL_OFFSET offs)
{
    const char* specialOffs[] = {"EPILOG", "PROLOG", "NO_MAP"};

    switch ((int)offs) // Need the cast since offs is unsigned and the case statements are comparing to signed.
    {
        case ICorDebugInfo::EPILOG:
        case ICorDebugInfo::PROLOG:
        case ICorDebugInfo::NO_MAPPING:
            assert(DWORD(ICorDebugInfo::EPILOG) + 1 == (unsigned)ICorDebugInfo::PROLOG);
            assert(DWORD(ICorDebugInfo::EPILOG) + 2 == (unsigned)ICorDebugInfo::NO_MAPPING);
            int specialOffsNum;
            specialOffsNum = offs - DWORD(ICorDebugInfo::EPILOG);
            printf("%s", specialOffs[specialOffsNum]);
            break;
        default:
            printf("0x%04X", offs);
    }
}

void CodeGen::eeDispLineInfo(const boundariesDsc* line)
{
    printf("IL offs ");

    eeDispILOffs(line->ilOffset);

    printf(" : 0x%08X", line->nativeIP);
    if (line->sourceReason != 0)
    {
        // It seems like it should probably never be zero since ICorDebugInfo::SOURCE_TYPE_INVALID is zero.
        // However, the JIT has always generated this and printed "stack non-empty".

        printf(" ( ");
        if ((line->sourceReason & ICorDebugInfo::STACK_EMPTY) != 0)
        {
            printf("STACK_EMPTY ");
        }
        if ((line->sourceReason & ICorDebugInfo::CALL_INSTRUCTION) != 0)
        {
            printf("CALL_INSTRUCTION ");
        }
        if ((line->sourceReason & ICorDebugInfo::CALL_SITE) != 0)
        {
            printf("CALL_SITE ");
        }
        printf(")");
    }
    printf("\n");

    // We don't expect to see any other bits.
    assert((line->sourceReason & ~(ICorDebugInfo::STACK_EMPTY | ICorDebugInfo::CALL_INSTRUCTION)) == 0);
}

void CodeGen::eeDispLineInfos()
{
    printf("IP mapping count : %d\n", eeBoundariesCount); // this might be zero
    for (unsigned i = 0; i < eeBoundariesCount; i++)
    {
        eeDispLineInfo(&eeBoundaries[i]);
    }
    printf("\n");
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

void Compiler::eeReserveUnwindInfo(bool isFunclet, bool isColdCode, ULONG unwindSize)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("reserveUnwindInfo(isFunclet=%s, isColdCode=%s, unwindSize=0x%x)\n", isFunclet ? "true" : "false",
               isColdCode ? "true" : "false", unwindSize);
    }
#endif // DEBUG

    if (info.compMatchedVM)
    {
        info.compCompHnd->reserveUnwindInfo(isFunclet, isColdCode, unwindSize);
    }
}

void Compiler::eeAllocUnwindInfo(BYTE*          pHotCode,
                                 BYTE*          pColdCode,
                                 ULONG          startOffset,
                                 ULONG          endOffset,
                                 ULONG          unwindSize,
                                 BYTE*          pUnwindBlock,
                                 CorJitFuncKind funcKind)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("allocUnwindInfo(pHotCode=0x%p, pColdCode=0x%p, startOffset=0x%x, endOffset=0x%x, unwindSize=0x%x, "
               "pUnwindBlock=0x%p, funKind=%d",
               dspPtr(pHotCode), dspPtr(pColdCode), startOffset, endOffset, unwindSize, dspPtr(pUnwindBlock), funcKind);
        switch (funcKind)
        {
            case CORJIT_FUNC_ROOT:
                printf(" (main function)");
                break;
            case CORJIT_FUNC_HANDLER:
                printf(" (handler)");
                break;
            case CORJIT_FUNC_FILTER:
                printf(" (filter)");
                break;
            default:
                printf(" (ILLEGAL)");
                break;
        }
        printf(")\n");
    }
#endif // DEBUG

    if (info.compMatchedVM)
    {
        info.compCompHnd->allocUnwindInfo(pHotCode, pColdCode, startOffset, endOffset, unwindSize, pUnwindBlock,
                                          funcKind);
    }
}

void Compiler::eeSetEHcount(unsigned cEH)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("setEHcount(cEH=%u)\n", cEH);
    }
#endif // DEBUG

    if (info.compMatchedVM)
    {
        info.compCompHnd->setEHcount(cEH);
    }
}

void Compiler::eeSetEHinfo(unsigned EHnumber, const CORINFO_EH_CLAUSE* clause)
{
#ifdef DEBUG
    if (opts.dspEHTable)
    {
        dispOutgoingEHClause(EHnumber, *clause);
    }
#endif // DEBUG

    if (info.compMatchedVM)
    {
        info.compCompHnd->setEHinfo(EHnumber, clause);
    }
}

#ifdef TARGET_AMD64
bool Compiler::eeIsRIPRelativeAddress(void* addr)
{
    return info.compMatchedVM && info.compCompHnd->getRelocTypeHint(addr) == IMAGE_REL_BASED_REL32;
}
#endif

#ifdef TARGET_ARM
bool Compiler::eeIsThumbBranch24TargetAddress(void* target)
{
    assert(info.compMatchedVM);
    return info.compCompHnd->getRelocTypeHint(target) == IMAGE_REL_BASED_THUMB_BRANCH24;
}
#endif

CORINFO_FIELD_HANDLE Compiler::eeFindJitDataOffs(unsigned dataOffs)
{
    // Data offsets are marked by the fact that the low two bits are 0b01 0x1
    assert(dataOffs < 0x40000000);
    return (CORINFO_FIELD_HANDLE)(size_t)((dataOffs << iaut_SHIFT) | iaut_DATA_OFFSET);
}

bool Compiler::eeIsJitDataOffs(CORINFO_FIELD_HANDLE field)
{
    // if 'field' is a jit data offset it has to fit into a 32-bit unsigned int
    unsigned value = static_cast<unsigned>(reinterpret_cast<uintptr_t>(field));
    if (((CORINFO_FIELD_HANDLE)(size_t)value) != field)
    {
        return false; // some bits in the upper 32 bits were set, not a jit data offset
    }

    // Data offsets are marked by the fact that the low two bits are 0b01
    return (value & iaut_MASK) == iaut_DATA_OFFSET;
}

int Compiler::eeGetJitDataOffs(CORINFO_FIELD_HANDLE field)
{
    // Data offsets are marked by the fact that the low two bits are 0b01 0x1
    if (eeIsJitDataOffs(field))
    {
        unsigned dataOffs = static_cast<unsigned>(reinterpret_cast<uintptr_t>(field));
        assert(((CORINFO_FIELD_HANDLE)(size_t)dataOffs) == field);
        assert(dataOffs < 0x40000000);

        // Shift away the low two bits
        return (static_cast<int>(reinterpret_cast<intptr_t>(field))) >> iaut_SHIFT;
    }
    else
    {
        return -1;
    }
}

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

/*****************************************************************************/

// static helper names - constant array
const char* jitHlpFuncTable[CORINFO_HELP_COUNT] = {
#define JITHELPER(code, pfnHelper, sig) #code,
#define DYNAMICJITHELPER(code, pfnHelper, sig) #code,
#include "jithelpers.h"
};

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

#if defined(DEBUG) || defined(FEATURE_JIT_METHOD_PERF) || defined(FEATURE_SIMD)

bool Compiler::eeIsNativeMethod(CORINFO_METHOD_HANDLE method)
{
    return ((((size_t)method) & 0x2) == 0x2);
}

CORINFO_METHOD_HANDLE Compiler::eeGetMethodHandleForNative(CORINFO_METHOD_HANDLE method)
{
    assert((((size_t)method) & 0x3) == 0x2);
    return (CORINFO_METHOD_HANDLE)(((size_t)method) & ~0x3);
}

#endif

const char* Compiler::eeGetMethodName(CORINFO_METHOD_HANDLE method, const char** classNamePtr)
{
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
                name = jitHlpFuncTable[ftnNum];
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

#endif // DEBUG || FEATURE_JIT_METHOD_PERF

#ifdef DEBUG

const WCHAR* Compiler::eeGetCPString(void* strHandle)
{
#ifdef HOST_UNIX
    return nullptr;
#else
    char buff[512 + sizeof(CORINFO_String)];

    // make this bulletproof, so it works even if we are wrong.
    if (ReadProcessMemory(GetCurrentProcess(), (void*)strHandle, buff, 4, nullptr) == 0)
    {
        return (nullptr);
    }

    CORINFO_String* asString = *((CORINFO_String**)strHandle);

    if (ReadProcessMemory(GetCurrentProcess(), asString, buff, sizeof(buff), nullptr) == 0)
    {
        return (nullptr);
    }

    if (asString->stringLen >= 255 || asString->chars[asString->stringLen] != 0)
    {
        return nullptr;
    }

    return (WCHAR*)(asString->chars);
#endif // HOST_UNIX
}

#endif // DEBUG
