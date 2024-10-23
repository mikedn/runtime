// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef DEBUG

#undef printf
#define printf jitprintf

#undef fprintf
#define fprintf jitfprintf

int jitprintf(const char* fmt, ...);
int jitfprintf(FILE* file, const char* fmt, ...);
void gcDump_logf(const char* fmt, ...);
void JitLogEE(unsigned level, const char* fmt, ...);

void assertAbort(const char* why, const char* file, unsigned line);

#undef assert
#define assert(p) (void)((p) || (assertAbort(#p, __FILE__, __LINE__), 0))

#define JITLOG(...) JitLogEE(__VA_ARGS__)

#else // DEBUG

#undef assert
#define assert(p) (void)0

#define JITLOG(...)

#endif // DEBUG

#ifndef _HOST_H_
#define _HOST_H_

extern FILE* jitstdout;

inline FILE* procstdout()
{
    return stdout;
}
#undef stdout
#define stdout use_jitstdout

#endif
