// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

enum var_types : uint8_t
{
#define DEF_TP(tn, nm, jitType, sz, sze, asze, al, tf) TYP_##tn,
#include "typelist.h"
    TYP_COUNT,
#ifdef TARGET_64BIT
    TYP_I_IMPL = TYP_LONG,
    TYP_U_IMPL = TYP_ULONG
#else
    TYP_I_IMPL = TYP_INT,
    TYP_U_IMPL = TYP_UINT
#endif
};
