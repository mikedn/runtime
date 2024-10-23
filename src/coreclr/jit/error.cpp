// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "compiler.h"

#if MEASURE_FATAL
unsigned fatal_badCode;
unsigned fatal_noWay;
unsigned fatal_implLimitation;
unsigned fatal_NOMEM;
unsigned fatal_noWayAssertBody;
unsigned fatal_NYI;
INDEBUG(unsigned fatal_noWayAssertBodyArgs;)
#endif

constexpr DWORD FATAL_JIT_EXCEPTION = 0x02345678;

void DECLSPEC_NORETURN fatal(int errCode)
{
#ifdef DEBUG
    if (errCode != CORJIT_SKIPPED) // Don't stop on NYI: use COMPlus_AltJitAssertOnNYI for that.
    {
        if (JitConfig.DebugBreakOnVerificationFailure())
        {
            DebugBreak();
        }
    }
#endif

    ULONG_PTR exceptArg = errCode;
    RaiseException(FATAL_JIT_EXCEPTION, EXCEPTION_NONCONTINUABLE, 1, &exceptArg);
    __UNREACHABLE();
}

void DECLSPEC_NORETURN badCode()
{
#if MEASURE_FATAL
    fatal_badCode += 1;
#endif

    fatal(CORJIT_BADCODE);
}

void DECLSPEC_NORETURN noWay()
{
#if MEASURE_FATAL
    fatal_noWay += 1;
#endif

    fatal(CORJIT_INTERNALERROR);
}

void DECLSPEC_NORETURN implLimitation()
{
#if MEASURE_FATAL
    fatal_implLimitation += 1;
#endif

    fatal(CORJIT_IMPLLIMITATION);
}

void DECLSPEC_NORETURN NOMEM()
{
#if MEASURE_FATAL
    fatal_NOMEM += 1;
#endif

    fatal(CORJIT_OUTOFMEM);
}

void DECLSPEC_NORETURN noWayAssertBody()
{
#if MEASURE_FATAL
    fatal_noWayAssertBody += 1;
#endif

#ifndef DEBUG
    // Even in retail, if we hit a noway, and we have this variable set, we don't want to fall back
    // to MinOpts, which might hide a regression. Instead, hit a breakpoint (and crash). We don't
    // have the assert code to fall back on here.
    // The debug path goes through this function also, to do the call to 'fatal'.
    // This kind of noway is hit for unreached().
    if (JitConfig.JitEnableNoWayAssert())
    {
        DebugBreak();
    }
#endif

    fatal(CORJIT_RECOVERABLEERROR);
}

static bool ShouldThrowOnNoway()
{
    return JitTls::GetCompiler() == nullptr || JitTls::GetCompiler()->compShouldThrowOnNoway();
}

void NOINLINE noWayAssertBodyConditional()
{
    if (ShouldThrowOnNoway())
    {
        noWayAssertBody();
    }
}

void notYetImplemented(const char* msg, const char* filename, unsigned line)
{
    Compiler* compiler = JitTls::GetCompiler();

    if ((compiler == nullptr) || (compiler->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_ALT_JIT)))
    {
#ifdef DEBUG
        noWayAssertBodyConditional(msg, filename, line);
#else
        noWayAssertBodyConditional();
#endif
        return;
    }

#if FUNC_INFO_LOGGING
#ifdef DEBUG
    Compiler* logCompiler = JitTls::GetLogCompiler();

    if ((logCompiler != nullptr) && logCompiler->verbose)
    {
        jitprintf("\n\n%s - NYI (%s:%u - %s)\n", logCompiler->info.compFullName, filename, line, msg);
    }
#endif

    if (FILE* funcInfoFile = Compiler::compJitFuncInfoFile)
    {
#ifdef DEBUG
        fprintf(funcInfoFile, "%s - NYI (%s:%u - %s)\n",
                logCompiler == nullptr ? "UNKNOWN" : logCompiler->info.compFullName, filename, line, msg);
#else
        fprintf(funcInfoFile, "NYI (%s:%u - %s)\n", filename, line, msg);
#endif
        fflush(funcInfoFile);
    }
#endif // FUNC_INFO_LOGGING

#ifdef DEBUG
    // Assume we're within a compFunctionTrace boundary, which might not be true.
    compiler->compFunctionTraceEnd(nullptr, 0, true);
#endif

    int value = JitConfig.AltJitAssertOnNYI();

    // 0 means just silently skip
    // If we are in retail builds, assume ignore
    // 1 means popup the assert (abort=abort, retry=debugger, ignore=skip)
    // 2 means silently don't skip (same as 3 for retail)
    // 3 means popup the assert (abort=abort, retry=debugger, ignore=don't skip)
    if ((value & 1) != 0)
    {
#ifdef DEBUG
        assertAbort(msg, filename, line);
#endif
    }

    if ((value & 2) == 0)
    {
#if MEASURE_FATAL
        fatal_NYI += 1;
#endif

        fatal(CORJIT_SKIPPED);
    }
}

LONG JitErrorTrapFilter(PEXCEPTION_POINTERS pExceptionPointers, ErrorTrapParam& param)
{
    if (pExceptionPointers->ExceptionRecord->ExceptionCode != FATAL_JIT_EXCEPTION)
    {
        return EXCEPTION_CONTINUE_SEARCH;
    }

    assert(pExceptionPointers->ExceptionRecord->NumberParameters == 1);
    param.error = static_cast<CorJitResult>(pExceptionPointers->ExceptionRecord->ExceptionInformation[0]);

    ICorJitInfo* jitInfo = param.jitInfo;

    if (jitInfo != nullptr)
    {
        jitInfo->reportFatalError(param.error);
    }

    return EXCEPTION_EXECUTE_HANDLER;
}

#ifdef DEBUG

static int jitvfprintf(FILE* file, const char* fmt, va_list args)
{
    // 0-length string means flush
    if (fmt[0] == '\0')
    {
        fflush(file);
        return 0;
    }

    return vfprintf(file, fmt, args);
}

int jitprintf(const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    int written = jitvfprintf(jitstdout, fmt, args);
    va_end(args);
    return written;
}

int jitfprintf(FILE* file, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    int written = jitvfprintf(file, fmt, args);
    va_end(args);
    return written;
}

static bool vmvlogf(unsigned level, const char* fmt, va_list args)
{
    static bool enabled = true;

    enabled = enabled && JitTls::GetJitInfo()->logMsg(level, fmt, args);

    return enabled;
}

static void vmlogf(unsigned level, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vmvlogf(level, fmt, args);
    va_end(args);
}

#if defined(HOST_X86) && !defined(TARGET_UNIX)

// While debugging in an Debugger, the "int 3" will cause the program to break
// Outside, the exception handler will just filter out the "int 3".

#define BreakIfDebuggerPresent()                                                                                       \
    do                                                                                                                 \
    {                                                                                                                  \
        __try                                                                                                          \
        {                                                                                                              \
            __asm {int 3}                                                                                              \
        }                                                                                                              \
        __except (EXCEPTION_EXECUTE_HANDLER)                                                                           \
        {                                                                                                              \
        }                                                                                                              \
    } while (0)

#else
#define BreakIfDebuggerPresent()                                                                                       \
    do                                                                                                                 \
    {                                                                                                                  \
        if (IsDebuggerPresent())                                                                                       \
            DebugBreak();                                                                                              \
    } while (0)
#endif

void debugError(const char* msg, const char* file, unsigned line)
{
    const char* tail = strrchr(file, '\\');
    if (tail != nullptr)
    {
        tail = tail + 1;
    }
    else
    {
        tail = file;
    }

    vmlogf(LL_ERROR, "COMPILATION FAILED: file: %s:%u compiling method %s reason %s\n", tail, line,
           JitTls::GetLogCompiler()->info.compFullName, msg);

    // We now only assert when user explicitly set ComPlus_JitRequired=1
    // If ComPlus_JitRequired is 0 or is not set, we will not assert.
    if ((JitConfig.JitRequired() == 1) || JitConfig.JitBreakOnBadCode())
    {
        assertAbort(msg, file, line);
    }

    BreakIfDebuggerPresent();
}

void assertAbort(const char* why, const char* file, unsigned line)
{
    const char* msg         = why;
    Compiler*   logCompiler = JitTls::GetLogCompiler();
    const int   BUFF_SIZE   = 8192;
    char*       buff        = static_cast<char*>(alloca(BUFF_SIZE));
    const char* phaseName   = "unknown phase";

    if (logCompiler != nullptr)
    {
        phaseName = PhaseNames[logCompiler->mostRecentlyActivePhase];
        _snprintf_s(buff, BUFF_SIZE, _TRUNCATE, "Assertion failed '%s' in '%s' (%x) during '%s' (IL size %u)\n", why,
                    logCompiler->info.compFullName, logCompiler->info.compMethodHash(), phaseName,
                    logCompiler->info.compILCodeSize);
        msg = buff;
    }

    fflush(jitstdout);

#if FUNC_INFO_LOGGING
    if (FILE* funcInfoFile = Compiler::compJitFuncInfoFile)
    {
        fprintf(funcInfoFile, "%s - Assertion failed (%s:%u - %s) during %s\n",
                logCompiler == nullptr ? "UNKNOWN" : logCompiler->info.compFullName, file, line, why, phaseName);
    }
#endif

    if (JitTls::GetJitInfo()->doAssert(file, line, msg))
    {
        DebugBreak();
    }

    Compiler* comp = JitTls::GetCompiler();

    if ((comp != nullptr) && comp->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_ALT_JIT))
    {
        // If we hit an assert, and we got here, it's either because the user hit "ignore" on the
        // dialog pop-up, or they set COMPlus_ContinueOnAssert=1 to not emit a pop-up, but just continue.
        // If we're an altjit, we have two options: (1) silently continue, as a normal JIT would, probably
        // leading to additional asserts, or (2) tell the VM that the AltJit wants to skip this function,
        // thus falling back to the fallback JIT. Setting COMPlus_AltJitSkipOnAssert=1 chooses this "skip"
        // to the fallback JIT behavior. This is useful when doing ASM diffs, where we only want to see
        // the first assert for any function, but we don't want to kill the whole ngen process on the
        // first assert (which would happen if you used COMPlus_NoGuiOnAssert=1 for example).
        if (JitConfig.AltJitSkipOnAssert() != 0)
        {
            fatal(CORJIT_SKIPPED);
        }
    }
}

void gcDump_logf(const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    bool enabled = vmvlogf(LL_INFO1000, fmt, args);
    va_end(args);

    if (!enabled)
    {
        // if the EE refuses to log it, we try to send it to stdout
        va_start(args, fmt);
        jitvfprintf(jitstdout, fmt, args);
        va_end(args);
    }
#if 0  // Enable this only when you need it
    else
    {
        static ConfigDWORD fJitBreakOnDumpToken;
        DWORD breakOnDumpToken = fJitBreakOnDumpToken.val(CLRConfig::INTERNAL_BreakOnDumpToken);
        static bool forbidEntry = false;

        if ((breakOnDumpToken != 0xffffffff) && !forbidEntry)
        {
            forbidEntry = true;

            // Use value of 0 to get the dump
            static unsigned currentLine = 1;

            if (currentLine == breakOnDumpToken)
            {
                assert(!"Dump token reached");
            }

            jitprintf("(Token=0x%x) ", currentLine++);
            forbidEntry = false;
        }
    }
#endif // 0
}

void JitLogEE(unsigned level, const char* fmt, ...)
{
    va_list args;

    if (JitTls::GetCompiler()->verbose)
    {
        va_start(args, fmt);
        jitvfprintf(jitstdout, fmt, args);
        va_end(args);
    }

    va_start(args, fmt);
    vmvlogf(level, fmt, args);
    va_end(args);
}

void DECLSPEC_NORETURN badCode3(const char* msg, const char* msg2, int arg, const char* file, unsigned line)
{
    const int BUFF_SIZE = 512;
    char      buf1[BUFF_SIZE];
    char      buf2[BUFF_SIZE];
    sprintf_s(buf1, BUFF_SIZE, "%s%s", msg, msg2);
    sprintf_s(buf2, BUFF_SIZE, buf1, arg);

    debugError(buf2, file, line);
    badCode();
}

static void noWayAssertAbortHelper(const char* cond, const char* file, unsigned line)
{
    if (JitConfig.JitEnableNoWayAssert())
    {
        assertAbort(cond, file, line);
    }
}

void noWayAssertBodyConditional(const char* cond, const char* file, unsigned line)
{
    if (ShouldThrowOnNoway())
    {
        noWayAssertBody(cond, file, line);
    }
    // In CHK we want the assert UI to show up in min-opts.
    else
    {
        noWayAssertAbortHelper(cond, file, line);
    }
}

void DECLSPEC_NORETURN noWayAssertBody(const char* cond, const char* file, unsigned line)
{
#if MEASURE_FATAL
    fatal_noWayAssertBodyArgs += 1;
#endif

    noWayAssertAbortHelper(cond, file, line);
    noWayAssertBody();
}

#endif // DEBUG
