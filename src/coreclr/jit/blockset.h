// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

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

#include "compilerbitsettraits.h"

using BlockSet = BitSetShortLongRep;

class BlockSetOps : public BitSetOps<BlockSet, BSShortLong, Compiler*, BasicBlockBitSetTraits>
{
public:
    // Specialize BlockSetOps::MakeFull(). Since we number basic blocks from one, we
    // remove bit zero from the block set. Otherwise, IsEmpty() would never return true.
    static BlockSet MakeFull(Compiler* env)
    {
        BlockSet retval = BitSetOps<BitSetShortLongRep, BSShortLong, Compiler*, BasicBlockBitSetTraits>::MakeFull(env);
        RemoveElemD(env, retval, 0);
        return retval;
    }
};

typedef BlockSetOps::ValArgType BlockSet_ValArg_T;
typedef BlockSetOps::RetValType BlockSet_ValRet_T;
