// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

inline CompAllocBitSetTraits::Word* CompAllocBitSetTraits::Alloc(Compiler* comp, unsigned wordCount)
{
    return comp->getAllocator(CMK_bitset).allocate<Word>(wordCount);
}

inline unsigned ILLabelSetTraits::GetSize(Compiler* comp)
{
    return comp->info.compILCodeSize;
}

inline unsigned ILLabelSetTraits::GetWordCount(Compiler* comp)
{
    return ComputeWordCount(comp->info.compILCodeSize);
}

inline unsigned LiveBitSetTraits::GetSize(const Compiler* comp)
{
    return comp->lvaTrackedCount;
}

inline unsigned LiveBitSetTraits::GetWordCount(const Compiler* comp)
{
    return comp->lvaLiveSetWordCount;
}

inline unsigned BlockSetTraits::GetSize(const Compiler* comp)
{
    return comp->fgBlockSetSize;
}

inline unsigned BlockSetTraits::GetWordCount(const Compiler* comp)
{
    // Assert that the version has been initialized. This is a convenient place to
    // assert this because GetWordCount() is called for every function, via IsShort().
    assert(comp->GetBlockSetVersion() != 0);

    return comp->fgBlockSetWordCount; // This is precomputed to avoid doing math every time this function is called
}

inline BitVecTraits::Word* BitVecTraits::Alloc(const BitVecTraits* b, unsigned wordCount)
{
    return b->comp->getAllocator(CMK_bitset).allocate<Word>(wordCount);
}
