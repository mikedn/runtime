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

#ifdef DEBUG
inline void* CompAllocBitSetTraits::DebugAlloc(Compiler* comp, size_t byteSize)
{
    return comp->getAllocator(CMK_DebugOnly).allocate<char>(byteSize);
}

inline unsigned TrackedVarBitSetTraits::GetEpoch(Compiler* comp)
{
    return comp->GetCurLVEpoch();
}

inline BitSetSupport::BitSetOpCounter* TrackedVarBitSetTraits::GetOpCounter(Compiler* comp)
{
#if VARSET_COUNTOPS
    return &Compiler::m_varsetOpCounter;
#else
    return nullptr;
#endif
}
#endif

inline unsigned BasicBlockBitSetTraits::GetSize(Compiler* comp)
{
    return comp->fgCurBBEpochSize;
}

inline unsigned BasicBlockBitSetTraits::GetArrSize(Compiler* comp, unsigned elemSize)
{
    // Assert that the epoch has been initialized. This is a convenient place to assert this because
    // GetArrSize() is called for every function, via IsShort().
    assert(GetEpoch(comp) != 0);

    assert(elemSize == sizeof(size_t));
    return comp->fgBBSetCountInSizeTUnits; // This is precomputed to avoid doing math every time this function is called
}

#ifdef DEBUG
inline unsigned BasicBlockBitSetTraits::GetEpoch(Compiler* comp)
{
    return comp->GetCurBasicBlockEpoch();
}

inline BitSetSupport::BitSetOpCounter* BasicBlockBitSetTraits::GetOpCounter(Compiler* comp)
{
    return nullptr;
}
#endif

inline void* BitVecTraits::Alloc(BitVecTraits* b, size_t byteSize)
{
    return b->comp->getAllocator(CMK_bitset).allocate<char>(byteSize);
}

#ifdef DEBUG
inline void* BitVecTraits::DebugAlloc(BitVecTraits* b, size_t byteSize)
{
    return b->comp->getAllocator(CMK_DebugOnly).allocate<char>(byteSize);
}
#endif
