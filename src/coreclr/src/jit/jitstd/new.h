// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#pragma once

#if !defined(HOST_UNIX)
inline void* __cdecl operator new(size_t sz, void* p)
{
    return p;
}
#endif

inline void* __cdecl operator new[](size_t sz, void* p)
{
    return p;
}
