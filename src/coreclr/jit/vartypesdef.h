// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

enum var_types : BYTE
{
#define DEF_TP(tn, nm, jitType, sz, sze, asze, st, al, tf) TYP_##tn,
#include "typelist.h"
    TYP_COUNT
};
