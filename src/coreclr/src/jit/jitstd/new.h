// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#pragma once

namespace jitstd
{

struct placement_t
{
};

}

inline void* __cdecl operator new(size_t sz, void* p, const jitstd::placement_t& /* syntax_difference */)
{
    return p;
}

inline void* __cdecl operator new[](size_t sz, void* p, const jitstd::placement_t& /* syntax_difference */)
{
    return p;
}
