// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

// A VARSET_TP is a set of (small) integers representing local variables.
// We implement varsets using the BitSet abstraction, which supports
// several different implementations.
//
// The set of tracked variables may change during a compilation, and variables may be
// re-sorted, so the tracked variable index of a variable is decidedly *not* stable.  The
// bitset abstraction supports labeling of bitsets with "epochs", and supports a
// debugging mode in which live bitsets must have the current epoch.  To use this feature,
// divide a compilation up into epochs, during which tracked variable indices are
// stable.

// Some implementations of BitSet may use a level of indirection.  Therefore, we
// must be careful about about assignment and initialization.  We often want to
// reason about VARSET_TP as immutable values, and just copying the contents would
// introduce sharing in the indirect case, which is usually not what's desired.  On
// the other hand, there are many cases in which the RHS value has just been
// created functionally, and the initialization/assignment is obviously its last
// use.  In these cases, allocating a new indirect representation for the lhs (if
// it does not already have one) would be unnecessary and wasteful.  Thus, for both
// initialization and assignment, we have normal versions, which do make copies to
// prevent sharing and definitely preserve value semantics, and "NOCOPY" versions,
// which do not.  Obviously, the latter should be used with care.

#include "compilerbitsettraits.h"

using VARSET_TP        = BitSetShortLongRep;
using VarSetOps        = BitSetOps<VARSET_TP, BSShortLong, Compiler*, TrackedVarBitSetTraits>;
using VARSET_VALARG_TP = VarSetOps::ValArgType;
using VARSET_VALRET_TP = VarSetOps::RetValType;
