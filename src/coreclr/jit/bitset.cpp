// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "bitset.h"
#include "bitsetasshortlong.h"

const unsigned BitSetSupport::BitCountTable[16]{0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4};

#ifdef DEBUG
template <typename Set, typename BitSetTraits>
static void RunTests(typename BitSetTraits::Env env)
{
    // The tests require that the Size is at least 52...
    assert(BitSetTraits::GetSize(env) > 51);

    using Ops = BitSetOps<Set, BitSetTraits>;

    Set bs1 = Ops::MakeEmpty(env);

    unsigned bs1bits[]{0, 10, 44, 45};
    Ops::AddElemD(env, bs1, bs1bits[0]);
    Ops::AddElemD(env, bs1, bs1bits[1]);
    Ops::AddElemD(env, bs1, bs1bits[2]);
    Ops::AddElemD(env, bs1, bs1bits[3]);

    unsigned k = 0;
    for (typename Ops::Enumerator bsi(env, bs1); bsi.MoveNext();)
    {
        assert(bsi.Current() == bs1bits[k]);
        k++;
    }
    assert(k == 4);

    Set temp = Ops::MakeCopy(env, bs1);
    Ops::UnionD(env, temp, bs1);
    assert(Ops::Equal(env, bs1, temp));

    temp = Ops::MakeCopy(env, bs1);
    Ops::IntersectionD(env, temp, bs1);
    assert(Ops::Equal(env, bs1, temp));

    assert(Ops::IsSubset(env, bs1, bs1));

    Set bs2 = Ops::MakeEmpty(env);

    unsigned bs2bits[]{0, 10, 50, 51};
    Ops::AddElemD(env, bs2, bs2bits[0]);
    Ops::AddElemD(env, bs2, bs2bits[1]);
    Ops::AddElemD(env, bs2, bs2bits[2]);
    Ops::AddElemD(env, bs2, bs2bits[3]);

    temp = Ops::MakeCopy(env, bs1);
    Ops::UnionD(env, temp, bs2);
    unsigned unionBits[]{0, 10, 44, 45, 50, 51};

    k = 0;
    for (typename Ops::Enumerator bsi(env, temp); bsi.MoveNext();)
    {
        assert(bsi.Current() == unionBits[k]);
        k++;
    }
    assert(k == 6);

    k = 0;
    for (typename Ops::Enumerator bsiL(env, temp); bsiL.MoveNext();)
    {
        assert(bsiL.Current() == unionBits[k]);
        k++;
    }
    assert(k == 6);

    temp = Ops::MakeCopy(env, bs1);
    Ops::IntersectionD(env, temp, bs2);
    unsigned intersectionBits[]{0, 10};

    k = 0;
    for (typename Ops::Enumerator bsi(env, temp); bsi.MoveNext();)
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
};

void BitSetSupport::TestSuite(CompAllocator env)
{
    RunTests<BitSetShortLongRep, TestBitSetTraits>(env);
}

#endif // DEBUG
