// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

enum SM_OPCODE
{
#define SMOPDEF(smname, string) smname,
#include "smopcode.def"
#undef SMOPDEF
    SM_COUNT // number of state machine opcodes
};
