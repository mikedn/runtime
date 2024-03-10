// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

inline CompAllocBitSetTraits::Word* CompAllocBitSetTraits::Alloc(Compiler* comp, unsigned wordCount)
{
    return comp->getAllocator(CMK_bitset).allocate<Word>(wordCount);
}

inline unsigned TrackedVarBitSetTraits::GetSize(Compiler* comp)
{
    return comp->lvaTrackedCount;
}

inline unsigned TrackedVarBitSetTraits::GetWordCount(Compiler* comp)
{
    return comp->lvaTrackedCountInSizeTUnits;
}

inline unsigned BasicBlockBitSetTraits::GetSize(Compiler* comp)
{
    return comp->fgCurBBEpochSize;
}

inline unsigned BasicBlockBitSetTraits::GetWordCount(Compiler* comp)
{
    // Assert that the epoch has been initialized. This is a convenient place to assert this because
    // GetWordCount() is called for every function, via IsShort().
    assert(comp->GetCurBasicBlockEpoch() != 0);

    return comp->fgBBSetCountInSizeTUnits; // This is precomputed to avoid doing math every time this function is called
}

inline BitVecTraits::Word* BitVecTraits::Alloc(const BitVecTraits* b, unsigned wordCount)
{
    return b->comp->getAllocator(CMK_bitset).allocate<Word>(wordCount);
}
