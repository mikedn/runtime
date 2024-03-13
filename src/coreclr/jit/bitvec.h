// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "bitsetasshortlong.h"

class BitVecTraits
{
    unsigned  size;
    Compiler* comp;

public:
    using Word = size_t;

    // TODO-MIKE-Cleanup: This should not be needed but there's a ton of code
    // that insists on passing a pointer to traits instead of a saner reference.
    // In fact, it may be even better to pass by value, since all these functions
    // are supposed to be inlined. Just beware that MSVC might do stupid things
    // with struct copies.
    struct Env
    {
        const BitVecTraits* traits;

        Env(const BitVecTraits* traits) : traits(traits)
        {
        }

        Env(const BitVecTraits& traits) : traits(&traits)
        {
        }

        operator const BitVecTraits*() const
        {
            return traits;
        }
    };

    BitVecTraits(unsigned size, Compiler* comp) : size(size), comp(comp)
    {
    }

    static Word* Alloc(const BitVecTraits* b, unsigned wordCount);

    static unsigned GetSize(const BitVecTraits* b)
    {
        return b->size;
    }

    static unsigned GetWordCount(const BitVecTraits* b)
    {
        unsigned wordBitSize = sizeof(Word) * CHAR_BIT;
        return roundUp(b->size, wordBitSize) / wordBitSize;
    }

    static bool IsShort(const BitVecTraits* t)
    {
        return t->size <= sizeof(Word) * CHAR_BIT;
    }
};

using BitVecOps = BitSetOps<BitVecTraits>;
using BitVec    = BitVecOps::Set;
using ASSERT_TP = BitVec;

class CompAllocBitSetTraits
{
public:
    using Word = size_t;

    static Word* Alloc(Compiler* comp, unsigned wordCount);

    static unsigned ComputeWordCount(unsigned bitCount)
    {
        unsigned wordBitSize = sizeof(Word) * CHAR_BIT;
        return roundUp(bitCount, wordBitSize) / wordBitSize;
    }
};

class ILLabelSetTraits : public CompAllocBitSetTraits
{
public:
    using Env = Compiler*;

    static unsigned GetSize(Compiler* comp);
    static unsigned GetWordCount(Compiler* comp);

    static bool IsShort(Compiler* comp)
    {
        return false;
    }
};

using ILLabelSetOps = BitSetOps<ILLabelSetTraits>;
using ILLabelSet    = ILLabelSetOps::Set;

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
class LiveBitSetTraits : public CompAllocBitSetTraits
{
public:
    using Env = Compiler*;

    static unsigned GetSize(const Compiler* comp);
    static unsigned GetWordCount(const Compiler* comp);

    static bool IsShort(const Compiler* comp)
    {
        return GetWordCount(comp) <= 1;
    }
};

using LiveSetOps = BitSetOps<LiveBitSetTraits>;
using LiveSet    = LiveSetOps::Set;
using VarSetOps  = LiveSetOps;
using VARSET_TP  = LiveSet;

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
class BlockSetTraits : public CompAllocBitSetTraits
{
public:
    using Env = Compiler*;

    static unsigned GetSize(const Compiler* comp);
    static unsigned GetWordCount(const Compiler* comp);

    static bool IsShort(const Compiler* comp)
    {
        return GetWordCount(comp) <= 1;
    }
};

class BlockSetOps : public BitSetOps<BlockSetTraits>
{
public:
    // Specialize BlockSetOps::MakeFull(). Since we number basic blocks from one, we
    // remove bit zero from the block set. Otherwise, IsEmpty() would never return true.
    static Set MakeFull(Compiler* env)
    {
        Set retval = BitSetOps<BlockSetTraits>::MakeFull(env);
        RemoveElemD(env, retval, 0);
        return retval;
    }
};

using BlockSet = BlockSetOps::Set;
