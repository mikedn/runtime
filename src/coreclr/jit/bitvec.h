// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "compilerbitsettraits.h"

using BitVec          = BitSetShortLongRep;
using BitVecOps       = BitSetOps<BitVec, BSShortLong, BitVecTraits*, BitVecTraits>;
using BitVec_ValArg_T = BitVecOps::ValArgType;
using BitVec_ValRet_T = BitVecOps::RetValType;
