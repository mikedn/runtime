// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

inline void* CompAllocBitSetTraits::Alloc(Compiler* comp, size_t byteSize)
{
    return comp->getAllocator(CMK_bitset).allocate<char>(byteSize);
}

inline unsigned TrackedVarBitSetTraits::GetSize(Compiler* comp)
{
    return comp->lvaTrackedCount;
}

inline unsigned TrackedVarBitSetTraits::GetArrSize(Compiler* comp, unsigned elemSize)
{
    assert(elemSize == sizeof(size_t));
    return comp->lvaTrackedCountInSizeTUnits;
}

inline unsigned BasicBlockBitSetTraits::GetSize(Compiler* comp)
{
    return comp->fgCurBBEpochSize;
}

inline unsigned BasicBlockBitSetTraits::GetArrSize(Compiler* comp, unsigned elemSize)
{
    // Assert that the epoch has been initialized. This is a convenient place to assert this because
    // GetArrSize() is called for every function, via IsShort().
    assert(comp->GetCurBasicBlockEpoch() != 0);

    assert(elemSize == sizeof(size_t));
    return comp->fgBBSetCountInSizeTUnits; // This is precomputed to avoid doing math every time this function is called
}

inline void* BitVecTraits::Alloc(const BitVecTraits* b, size_t byteSize)
{
    return b->comp->getAllocator(CMK_bitset).allocate<char>(byteSize);
}
