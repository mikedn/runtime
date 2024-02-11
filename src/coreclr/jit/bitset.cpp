// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "bitset.h"
#include "bitsetasuint64.h"
#include "bitsetasshortlong.h"
#include "bitsetasuint64inclass.h"

const unsigned BitSetSupport::BitCountTable[16]{0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4};

#ifdef DEBUG
template <typename BitSetType, typename BitSetTraits>
void BitSetSupport::RunTests(typename BitSetTraits::Env env)
{
    using LclBitSetOps = BitSetOps<BitSetType, BitSetTraits>;

    // The tests require that the Size is at least 52...
    assert(BitSetTraits::GetSize(env) > 51);

    BitSetType bs1;
    LclBitSetOps::AssignNoCopy(env, bs1, LclBitSetOps::MakeEmpty(env));
    unsigned bs1bits[] = {0, 10, 44, 45};
    LclBitSetOps::AddElemD(env, bs1, bs1bits[0]);
    LclBitSetOps::AddElemD(env, bs1, bs1bits[1]);
    LclBitSetOps::AddElemD(env, bs1, bs1bits[2]);
    LclBitSetOps::AddElemD(env, bs1, bs1bits[3]);

    unsigned k = 0;
    for (typename LclBitSetOps::Enumerator bsi(env, bs1); bsi.MoveNext();)
    {
        assert(bsi.Current() == bs1bits[k]);
        k++;
    }
    assert(k == 4);

    assert(LclBitSetOps::Equal(env, bs1, LclBitSetOps::Union(env, bs1, bs1)));
    assert(LclBitSetOps::Equal(env, bs1, LclBitSetOps::Intersection(env, bs1, bs1)));
    assert(LclBitSetOps::IsSubset(env, bs1, bs1));

    BitSetType bs2;
    LclBitSetOps::AssignNoCopy(env, bs2, LclBitSetOps::MakeEmpty(env));
    unsigned bs2bits[] = {0, 10, 50, 51};
    LclBitSetOps::AddElemD(env, bs2, bs2bits[0]);
    LclBitSetOps::AddElemD(env, bs2, bs2bits[1]);
    LclBitSetOps::AddElemD(env, bs2, bs2bits[2]);
    LclBitSetOps::AddElemD(env, bs2, bs2bits[3]);

    unsigned   unionBits[] = {0, 10, 44, 45, 50, 51};
    BitSetType bsU12;
    LclBitSetOps::AssignNoCopy(env, bsU12, LclBitSetOps::Union(env, bs1, bs2));
    k = 0;
    for (typename LclBitSetOps::Enumerator bsi(env, bsU12); bsi.MoveNext();)
    {
        assert(bsi.Current() == unionBits[k]);
        k++;
    }
    assert(k == 6);

    k = 0;
    for (typename LclBitSetOps::Enumerator bsiL(env, bsU12); bsiL.MoveNext();)
    {
        assert(bsiL.Current() == unionBits[k]);
        k++;
    }
    assert(k == 6);

    unsigned   intersectionBits[] = {0, 10};
    BitSetType bsI12;
    LclBitSetOps::AssignNoCopy(env, bsI12, LclBitSetOps::Intersection(env, bs1, bs2));
    k = 0;
    for (typename LclBitSetOps::Enumerator bsi(env, bsI12); bsi.MoveNext();)
    {
        assert(bsi.Current() == intersectionBits[k]);
        k++;
    }
    assert(k == 2);
}

class TestBitSetTraits
{
public:
    using Env = CompAllocator;

    static void* Alloc(CompAllocator alloc, size_t byteSize)
    {
        return alloc.allocate<char>(byteSize);
    }
    static unsigned GetSize(CompAllocator alloc)
    {
        return 64;
    }
    static unsigned GetArrSize(CompAllocator alloc, unsigned elemSize)
    {
        assert(elemSize == sizeof(size_t));
        return (64 / 8) / sizeof(size_t);
    }
    static unsigned GetEpoch(CompAllocator alloc)
    {
        return 0;
    }
};

void BitSetSupport::TestSuite(CompAllocator env)
{
    BitSetSupport::RunTests<UINT64, TestBitSetTraits>(env);
    BitSetSupport::RunTests<BitSetShortLongRep, TestBitSetTraits>(env);
    BitSetSupport::RunTests<BitSetUint64<TestBitSetTraits>, TestBitSetTraits>(env);
}

#endif // DEBUG
