// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "iallocator.h"
#ifndef _HOST_H_
#include "host.h"
#endif

// CompMemKind values are used to tag memory allocations performed via
// the compiler's allocator so that the memory usage of various compiler
// components can be tracked separately (when MEASURE_MEM_ALLOC is defined).

enum CompMemKind
{
#define CompMemKindMacro(kind) CMK_##kind,
#include "compmemkind.h"
    CMK_Count
};

class ArenaAllocator
{
    struct PageDescriptor
    {
        PageDescriptor* m_next;

        size_t m_pageBytes; // # of bytes allocated
        size_t m_usedBytes; // # of bytes actually used. (This is only valid when we've allocated a new page.)
                            // See ArenaAllocator::allocateNewPage.

        uint8_t m_contents[];
    };

    static constexpr size_t DEFAULT_PAGE_SIZE = 0x10000;

    // These two pointers (when non-null) will always point into 'm_lastPage'.
    uint8_t* m_nextFreeByte = nullptr;
    uint8_t* m_lastFreeByte = nullptr;

    PageDescriptor* m_firstPage = nullptr;
    PageDescriptor* m_lastPage  = nullptr;

    void* allocateNewPage(size_t size);

    static void* allocateHostMemory(size_t size, size_t* pActualSize);
    static void freeHostMemory(void* block, size_t size);

#if MEASURE_MEM_ALLOC
    struct MemStats
    {
        uint64_t allocCnt;                 // # of allocs
        uint64_t allocSz;                  // total size of those alloc.
        uint64_t allocSzMax;               // Maximum single allocation.
        uint64_t allocSzByKind[CMK_Count]; // Classified by "kind".
        uint64_t nraTotalSizeAlloc;
        uint64_t nraTotalSizeUsed;

        void AddAlloc(size_t sz, CompMemKind cmk)
        {
            allocCnt += 1;
            allocSz += sz;
            if (sz > allocSzMax)
            {
                allocSzMax = sz;
            }
            allocSzByKind[cmk] += sz;
        }

        void Print(FILE* f) const;
        void PrintByKind(FILE* f) const;
    };

    struct AggregateMemStats : public MemStats
    {
        unsigned nMethods;

        void Add(const MemStats& ms)
        {
            nMethods++;
            allocCnt += ms.allocCnt;
            allocSz += ms.allocSz;
            allocSzMax = Max(allocSzMax, ms.allocSzMax);
            for (size_t i = 0; i < CMK_Count; i++)
            {
                allocSzByKind[i] += ms.allocSzByKind[i];
            }
            nraTotalSizeAlloc += ms.nraTotalSizeAlloc;
            nraTotalSizeUsed += ms.nraTotalSizeUsed;
        }

        void Print(FILE* f) const;
    };

public:
    ArenaAllocator()                            = default;
    ArenaAllocator(const ArenaAllocator& other) = delete;
    ArenaAllocator& operator=(const ArenaAllocator& other) = delete;
    ArenaAllocator& operator=(ArenaAllocator&& other) = delete;

    struct MemStatsAllocator
    {
        ArenaAllocator* m_arena;
        CompMemKind     m_kind;

        void* allocateMemory(size_t sz)
        {
            m_arena->m_stats.AddAlloc(sz, m_kind);
            return m_arena->allocateMemory(sz);
        }
    };

private:
    static CritSecObject     s_statsLock; // This lock protects the data structures below.
    static MemStats          s_maxStats;  // Stats for the allocator with the largest amount allocated.
    static AggregateMemStats s_aggStats;  // Aggregates statistics for all allocators.

    MemStats          m_stats{};
    MemStatsAllocator m_statsAllocators[CMK_Count]{};

public:
    MemStatsAllocator* getMemStatsAllocator(CompMemKind kind);
    void finishMemStats();
    void dumpMemStats(FILE* file);

    static void dumpMaxMemStats(FILE* file);
    static void dumpAggregateMemStats(FILE* file);
#endif // MEASURE_MEM_ALLOC

#ifdef DEBUG
    int GetUninitializedByte();
#endif

public:
    // NOTE: it would be nice to have a destructor on this type to ensure that any value that
    //       goes out of scope is either uninitialized or has been torn down via a call to
    //       destroy(), but this interacts badly in methods that use SEH. #3058 tracks
    //       revisiting EH in the JIT; such a destructor could be added if SEH is removed
    //       as part of that work.

    void destroy();

    inline void* allocateMemory(size_t sz);

    size_t getTotalBytesAllocated();
    size_t getTotalBytesUsed();

    static bool   bypassHostAllocator();
    static size_t getDefaultPageSize();
};

//------------------------------------------------------------------------
// ArenaAllocator::allocateMemory:
//    Allocates memory using an `ArenaAllocator`.
//
// Arguments:
//    size - The number of bytes to allocate.
//
// Return Value:
//    A pointer to the allocated memory.
//
// Note:
//    The DEBUG version of the method has some abilities that the release
//    version does not: it may inject faults into the allocator and
//    seeds all allocations with a specified pattern to help catch
//    use-before-init problems.
//
inline void* ArenaAllocator::allocateMemory(size_t size)
{
    assert(size != 0);

    // Ensure that we always allocate in pointer sized increments.
    size = roundUp(size, sizeof(size_t));

#ifdef DEBUG
    if (JitConfig.ShouldInjectFault() != 0)
    {
        // Force the underlying memory allocator (either the OS or the CLR hoster)
        // to allocate the memory. Any fault injection will kick in.
        size_t size;
        void*  p = allocateHostMemory(1, &size);
        freeHostMemory(p, size);
    }
#endif

    void* block = m_nextFreeByte;
    m_nextFreeByte += size;

    if (m_nextFreeByte > m_lastFreeByte)
    {
        block = allocateNewPage(size);
    }

    INDEBUG(memset(block, GetUninitializedByte(), size));

    return block;
}

// Allows general purpose code (e.g. collection classes) to allocate
// memory of a pre-determined kind via an arena allocator.

class CompAllocator
{
#if MEASURE_MEM_ALLOC
    ArenaAllocator::MemStatsAllocator* m_arena;
#else
    ArenaAllocator* m_arena;
#endif

public:
    CompAllocator(ArenaAllocator* arena, CompMemKind cmk)
#if MEASURE_MEM_ALLOC
        : m_arena(arena->getMemStatsAllocator(cmk))
#else
        : m_arena(arena)
#endif
    {
    }

    // Allocate a block of memory suitable to store `count` objects of type `T`.
    // Zero-length allocations are not allowed.
    template <typename T>
    T* allocate(size_t count)
    {
        // Ensure that count * sizeof(T) does not overflow.
        if (count > (SIZE_MAX / sizeof(T)))
        {
            NOMEM();
        }

        void* p = m_arena->allocateMemory(count * sizeof(T));

        // Ensure that the allocator returned sizeof(size_t) aligned memory.
        assert((size_t(p) & (sizeof(size_t) - 1)) == 0);

#ifdef _MSC_VER
        // MSVC still hasn't learned that the throwing new operator is not supposed
        // to return null, nor can it deduce on its own that this function never
        // returns null. The result is lots of null checks to prevent constructors
        // running with a null this pointer.
        __assume(p != nullptr);
#endif

        return static_cast<T*>(p);
    }

    // Deallocate a block of memory previously allocated by `allocate`.
    // The arena allocator does not release memory so this doesn't do anything.
    void deallocate(void* p)
    {
    }
};

// Global operator new overloads that work with CompAllocator

inline void* __cdecl operator new(size_t n, CompAllocator alloc)
{
    return alloc.allocate<char>(n);
}

inline void* __cdecl operator new[](size_t n, CompAllocator alloc)
{
    return alloc.allocate<char>(n);
}

// A CompAllocator wrapper that implements IAllocator and allows zero-length
// memory allocations (the arena allocator does not support zero-length
// allocation).

class CompIAllocator : public IAllocator
{
    CompAllocator m_alloc;
    static char   m_zeroLenAlloc;

public:
    CompIAllocator(CompAllocator alloc) : m_alloc(alloc)
    {
    }

    virtual void* Alloc(size_t sz) override
    {
        if (sz == 0)
        {
            return &m_zeroLenAlloc;
        }

        return m_alloc.allocate<char>(sz);
    }

    virtual void* ArrayAlloc(size_t elems, size_t elemSize) override
    {
        if ((elems == 0) || (elemSize == 0))
        {
            return &m_zeroLenAlloc;
        }

        // Ensure that elems * elemSize does not overflow.
        if (elems > (SIZE_MAX / elemSize))
        {
            NOMEM();
        }

        return m_alloc.allocate<char>(elems * elemSize);
    }

    virtual void Free(void* p) override
    {
        m_alloc.deallocate(p);
    }
};
