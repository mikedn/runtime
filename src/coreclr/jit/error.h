// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include <corjit.h>

class Compiler;

struct ErrorTrapParam
{
    CorJitResult       error   = CORJIT_INTERNALERROR;
    ICorJitInfo*       jitInfo = nullptr;
    EXCEPTION_POINTERS exceptionPointers;
};

template <typename T>
struct NestedErrorTrapParam : ErrorTrapParam
{
    T param;

    NestedErrorTrapParam(T param) : param(param)
    {
    }
};

// Only catch JIT internal errors (will not catch EE generated Errors)
LONG JitErrorTrapFilter(PEXCEPTION_POINTERS pExceptionPointers, ErrorTrapParam& param);

void debugError(const char* msg, const char* file, unsigned line);

void DECLSPEC_NORETURN badCode();
void DECLSPEC_NORETURN badCode3(const char* msg, const char* msg2, int arg, const char* file, unsigned line);
void DECLSPEC_NORETURN noWay();
void DECLSPEC_NORETURN implLimitation();
void DECLSPEC_NORETURN NOMEM();
void DECLSPEC_NORETURN fatal(int errCode);

void DECLSPEC_NORETURN noWayAssertBody();
void DECLSPEC_NORETURN noWayAssertBody(const char* cond, const char* file, unsigned line);

// Conditionally invoke the noway assert body. The conditional predicate is evaluated using a method on the tlsCompiler.
// If a noway_assert is hit, we ask the Compiler whether to raise an exception (i.e., conditionally raise exception.)
// To have backward compatibility between v4.5 and v4.0, in min-opts we take a shot at codegen rather than rethrow.
void ANALYZER_NORETURN noWayAssertBodyConditional();

void ANALYZER_NORETURN noWayAssertBodyConditional(const char* cond, const char* file, unsigned line);

// Define MEASURE_NOWAY to 1 to enable code to count and rank individual noway_assert calls by occurrence.
// These asserts would be dynamically executed, but not necessarily fail. The provides some insight into
// the dynamic prevalence of these (if not a direct measure of their cost), which exist in non-DEBUG as
// well as DEBUG builds.
#ifdef DEBUG
#define MEASURE_NOWAY 1
#else
#define MEASURE_NOWAY 0
#endif

#if MEASURE_NOWAY
void RecordNowayAssertGlobal(const char* filename, unsigned line, const char* condStr);
#define RECORD_NOWAY_ASSERT(condStr) RecordNowayAssertGlobal(__FILE__, __LINE__, condStr);
#else
#define RECORD_NOWAY_ASSERT(condStr)
#endif

// clang-format off

#ifdef DEBUG

#define NO_WAY(msg) (debugError(msg, __FILE__, __LINE__), noWay())
// Used for fallback stress mode
#define NO_WAY_NOASSERT(msg) noWay()
#define BADCODE(msg) (debugError(msg, __FILE__, __LINE__), badCode())
#define BADCODE3(msg, msg2, arg) badCode3(msg, msg2, arg, __FILE__, __LINE__)
// Used for an assert that we want to convert into BADCODE to force minopts, or in minopts to force codegen.
#define noway_assert(cond)                                                                                             \
    do                                                                                                                 \
    {                                                                                                                  \
        RECORD_NOWAY_ASSERT(#cond)                                                                                     \
        if (!(cond))                                                                                                   \
        {                                                                                                              \
            noWayAssertBodyConditional(#cond, __FILE__, __LINE__);                                                     \
        }                                                                                                              \
    } while (0)
#define unreached() noWayAssertBody("unreached", __FILE__, __LINE__)

// IMPL_LIMITATION is called when we encounter valid IL that is not
// supported by our current implementation because of various
// limitations (that could be removed in the future)
#define IMPL_LIMITATION(msg) (debugError(msg, __FILE__, __LINE__), implLimitation())

#else // !DEBUG

#define NO_WAY(msg) noWay()
#define BADCODE(msg) badCode()
#define BADCODE3(msg, msg2, arg) badCode()

// IMPL_LIMITATION is called when we encounter valid IL that is not
// supported by our current implementation because of various
// limitations (that could be removed in the future)
#define IMPL_LIMITATION(msg) implLimitation()

#define noway_assert(cond)                                                                                             \
    do                                                                                                                 \
    {                                                                                                                  \
        RECORD_NOWAY_ASSERT(#cond)                                                                                     \
        if (!(cond))                                                                                                   \
        {                                                                                                              \
            noWayAssertBodyConditional();                                                                              \
            noWayAssertBodyConditional();                                                                              \
        }                                                                                                              \
    } while (0)
#define unreached() noWayAssertBody()

#endif // !DEBUG


#if 1 // All platforms currently enable NYI; this should be a tighter condition to exclude some platforms from NYI

// This can return based on Config flag/Debugger
void notYetImplemented(const char* msg, const char* file, unsigned line);
#define NYIRAW(msg) notYetImplemented(msg, __FILE__, __LINE__)

#define NYI(msg)                    NYIRAW("NYI: " msg)
#define NYI_IF(cond, msg) if (cond) NYIRAW("NYI: " msg)

#ifdef TARGET_AMD64

#define NYI_AMD64(msg)  NYIRAW("NYI_AMD64: " msg)
#define NYI_X86(msg)    do { } while (0)
#define NYI_ARM(msg)    do { } while (0)
#define NYI_ARM64(msg)  do { } while (0)

#elif defined(TARGET_X86)

#define NYI_AMD64(msg)  do { } while (0)
#define NYI_X86(msg)    NYIRAW("NYI_X86: " msg)
#define NYI_ARM(msg)    do { } while (0)
#define NYI_ARM64(msg)  do { } while (0)

#elif defined(TARGET_ARM)

#define NYI_AMD64(msg)  do { } while (0)
#define NYI_X86(msg)    do { } while (0)
#define NYI_ARM(msg)    NYIRAW("NYI_ARM: " msg)
#define NYI_ARM64(msg)  do { } while (0)

#elif defined(TARGET_ARM64)

#define NYI_AMD64(msg)  do { } while (0)
#define NYI_X86(msg)    do { } while (0)
#define NYI_ARM(msg)    do { } while (0)
#define NYI_ARM64(msg)  NYIRAW("NYI_ARM64: " msg)

#else

#error "Unknown platform, not x86, ARM, or AMD64?"

#endif

#else // NYI not available; make it an assert.

#define NYI(msg)        assert(!(msg))
#define NYI_AMD64(msg)  do { } while (0)
#define NYI_ARM(msg)    do { } while (0)
#define NYI_ARM64(msg)  do { } while (0)

#endif // NYI not available

// clang-format on
