// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "bitsetasshortlong.h"

class BitVecTraits
{
    unsigned  size;
    Compiler* comp;

public:
    using Env = BitVecTraits*;

    BitVecTraits(unsigned size, Compiler* comp) : size(size), comp(comp)
    {
    }

    static void* Alloc(BitVecTraits* b, size_t byteSize);

    static unsigned GetSize(BitVecTraits* b)
    {
        return b->size;
    }

    static unsigned GetArrSize(BitVecTraits* b, unsigned elemSize)
    {
        assert(elemSize == sizeof(size_t));
        unsigned elemBits = 8 * elemSize;
        return roundUp(b->size, elemBits) / elemBits;
    }

#ifdef DEBUG
    static void* DebugAlloc(BitVecTraits* b, size_t byteSize);

    static unsigned GetEpoch(BitVecTraits* b)
    {
        return b->size;
    }
#endif
};

using BitVecOps       = BitSetOps<BitSetShortLongRep, BitVecTraits>;
using BitVec          = BitVecOps::Rep;
using BitVec_ValArg_T = BitVecOps::ValArgType;
using BitVec_ValRet_T = BitVecOps::RetValType;

using ASSERT_TP        = BitVec;
using ASSERT_VALARG_TP = BitVec_ValArg_T;
using ASSERT_VALRET_TP = BitVec_ValRet_T;

class CompAllocBitSetTraits
{
public:
    static void* Alloc(Compiler* comp, size_t byteSize);

#ifdef DEBUG
    static void* DebugAlloc(Compiler* comp, size_t byteSize);
#endif
};

// A VARSET_TP is a set of (small) integers representing local variables.
//
// The set of tracked variables may change during a compilation, and variables may be
// re-sorted, so the tracked variable index of a variable is decidedly *not* stable.
// The bitset abstraction supports labeling of bitsets with "epochs", and supports a
// debugging mode in which live bitsets must have the current epoch. To use this feature,
// divide a compilation up into epochs, during which tracked variable indices are
// stable.

// Some implementations of BitSet may use a level of indirection. Therefore, we
// must be careful about about assignment and initialization. We often want to
// reason about VARSET_TP as immutable values, and just copying the contents would
// introduce sharing in the indirect case, which is usually not what's desired.
// On the other hand, there are many cases in which the RHS value has just been
// created functionally, and the initialization/assignment is obviously its last
// use. In these cases, allocating a new indirect representation for the lhs (if
// it does not already have one) would be unnecessary and wasteful. Thus, for both
// initialization and assignment, we have normal versions, which do make copies to
// prevent sharing and definitely preserve value semantics, and "NOCOPY" versions,
// which do not. Obviously, the latter should be used with care.

// This class is customizes the bit set to represent sets of tracked local vars.
// The size of the bitset is determined by the # of tracked locals (up to some internal
// maximum), and the Compiler* tracks the tracked local epochs.
class TrackedVarBitSetTraits : public CompAllocBitSetTraits
{
public:
    using Env = Compiler*;

    static unsigned GetSize(Compiler* comp);
    static unsigned GetArrSize(Compiler* comp, unsigned elemSize);

#ifdef DEBUG
    static unsigned GetEpoch(class Compiler* comp);
#endif
};

using VarSetOps        = BitSetOps<BitSetShortLongRep, TrackedVarBitSetTraits>;
using VARSET_TP        = VarSetOps::Rep;
using VARSET_VALARG_TP = VarSetOps::ValArgType;
using VARSET_VALRET_TP = VarSetOps::RetValType;

// A BlockSet is a set of BasicBlocks, represented by the BasicBlock number (bbNum).
//
// Note that BasicBlocks in the JIT are numbered starting at 1. We always just waste the
// 0th bit to avoid having to do "bbNum - 1" calculations everywhere (at the BlockSet call
// sites). This makes reading the code easier, and avoids potential problems of forgetting
// to do a "- 1" somewhere.
//
// Basic blocks can be renumbered during compilation, so it is important to not mix
// BlockSets created before and after a renumbering. Every time the blocks are renumbered
// creates a different "epoch", during which the basic block numbers are stable.

// This class is customizes the bit set to represent sets of BasicBlocks.
// The size of the bitset is determined by maximum assigned BasicBlock number
// (Compiler::fgBBNumMax) (Note that fgBBcount is not equal to this during inlining,
// when fgBBcount is the number of blocks in the inlined function, but the assigned
// block numbers are higher than the inliner function. fgBBNumMax counts both.
// Thus, if you only care about the inlinee, during inlining, this bit set will waste
// the lower numbered block bits.) The Compiler* tracks the BasicBlock epochs.
class BasicBlockBitSetTraits : public CompAllocBitSetTraits
{
public:
    using Env = Compiler*;

    static unsigned GetSize(Compiler* comp);
    static unsigned GetArrSize(Compiler* comp, unsigned elemSize);

#ifdef DEBUG
    static unsigned GetEpoch(class Compiler* comp);
#endif
};

class BlockSetOps : public BitSetOps<BitSetShortLongRep, BasicBlockBitSetTraits>
{
public:
    // Specialize BlockSetOps::MakeFull(). Since we number basic blocks from one, we
    // remove bit zero from the block set. Otherwise, IsEmpty() would never return true.
    static Rep MakeFull(Compiler* env)
    {
        Rep retval = BitSetOps<BitSetShortLongRep, BasicBlockBitSetTraits>::MakeFull(env);
        RemoveElemD(env, retval, 0);
        return retval;
    }
};

using BlockSet          = BlockSetOps::Rep;
using BlockSet_ValArg_T = BlockSetOps::ValArgType;
using BlockSet_ValRet_T = BlockSetOps::RetValType;
