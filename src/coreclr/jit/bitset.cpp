// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "bitset.h"
#include "bitsetasshortlong.h"

const unsigned BitSetSupport::BitCountTable[16]{0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4};

#ifdef DEBUG
template <typename Traits>
static void RunTests(typename Traits::Env env)
{
    // The tests require that the Size is at least 52...
    assert(Traits::GetSize(env) > 51);

    using Ops = BitSetOps<Traits>;
    using Set = typename Ops::Set;

    Set s1 = Ops::MakeEmpty(env);

    unsigned bits1[]{0, 10, 44, 45};
    Ops::AddElemD(env, s1, bits1[0]);
    Ops::AddElemD(env, s1, bits1[1]);
    Ops::AddElemD(env, s1, bits1[2]);
    Ops::AddElemD(env, s1, bits1[3]);

    unsigned k = 0;
    for (typename Ops::Enumerator bsi(env, s1); bsi.MoveNext();)
    {
        assert(bsi.Current() == bits1[k]);
        k++;
    }
    assert(k == 4);

    Set temp = Ops::Alloc(env);
    Ops::Union(env, temp, s1, s1);
    assert(Ops::Equal(env, s1, temp));

    Ops::Intersection(env, temp, s1, s1);
    assert(Ops::Equal(env, s1, temp));

    assert(Ops::IsSubset(env, s1, s1));

    Set s2 = Ops::MakeEmpty(env);

    unsigned bits2[]{0, 10, 50, 51};
    Ops::AddElemD(env, s2, bits2[0]);
    Ops::AddElemD(env, s2, bits2[1]);
    Ops::AddElemD(env, s2, bits2[2]);
    Ops::AddElemD(env, s2, bits2[3]);

    Ops::Union(env, temp, s1, s2);
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

    Ops::Intersection(env, temp, s1, s2);
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
    using Env  = CompAllocator;
    using Word = size_t;

    static void* Alloc(CompAllocator alloc, unsigned wordCount)
    {
        return alloc.allocate<Word>(wordCount);
    }

    static unsigned GetSize(CompAllocator alloc)
    {
        return 64;
    }

    static unsigned GetWordCount(CompAllocator alloc)
    {
        return GetSize(alloc) / sizeof(Word);
    }
};

void BitSetSupport::TestSuite(CompAllocator env)
{
    RunTests<TestBitSetTraits>(env);
}

#endif // DEBUG
