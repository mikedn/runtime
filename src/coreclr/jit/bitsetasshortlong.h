// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

// A set of integers in the range [0..N], for some N defined by the "Env" (via "Traits").
//
// Represented as a pointer-sized item.  If N bits can fit in this item, the representation is "direct"; otherwise,
// the item is a pointer to an array of K size_t's, where K is the number of size_t's necessary to hold N bits.

#pragma once

#include "bitset.h"

template <typename Traits>
class BitSetOps
{
public:
    using Env      = typename Traits::Env;
    using Word     = typename Traits::Word;
    using Set      = Word*;
    using ConstSet = const Word*;

private:
    static const unsigned WordBitSize = sizeof(Word) * CHAR_BIT;

public:
    static Set UninitVal()
    {
        return nullptr;
    }

    static bool MayBeUninit(Set s)
    {
        return s == UninitVal();
    }

    static Set Alloc(Env env)
    {
        if (Traits::IsShort(env))
        {
            return nullptr;
        }
        else
        {
            return AllocLong(env);
        }
    }

    static Set MakeEmpty(Env env)
    {
        if (Traits::IsShort(env))
        {
            return nullptr;
        }
        else
        {
            return MakeEmptyLong(env);
        }
    }

    static Set MakeFull(Env env)
    {
        if (Traits::IsShort(env))
        {
            unsigned size = Traits::GetSize(env);
            return Set(size == 0 ? 0 : (~Word(0) >> (WordBitSize - size)));
        }

        return MakeFullLong(env);
    }

    static Set MakeCopy(Env env, ConstSet s)
    {
        if (Traits::IsShort(env))
        {
            return Set(Word(s));
        }
        else
        {
            return MakeCopyLong(env, s);
        }
    }

    static Set MakeSingleton(Env env, unsigned index)
    {
        assert(index < Traits::GetSize(env));
        if (Traits::IsShort(env))
        {
            return Set(Word(1) << index);
        }
        else
        {
            return MakeSingletonLong(env, index);
        }
    }

    static void Assign(Env env, Set& lhs, Set rhs)
    {
        if (Traits::IsShort(env))
        {
            lhs = rhs;
        }
        else
        {
            AssignLong(Traits::GetWordCount(env), lhs, rhs);
        }
    }

    static void ClearD(Env env, Set& s)
    {
        if (Traits::IsShort(env))
        {
            s = nullptr;
        }
        else
        {
            assert(s != UninitVal());
            ClearDLong(Traits::GetWordCount(env), s);
        }
    }

    static bool IsEmpty(Env env, ConstSet s)
    {
        if (Traits::IsShort(env))
        {
            return s == nullptr;
        }
        else
        {
            assert(s != UninitVal());
            return IsEmptyLong(Traits::GetWordCount(env), s);
        }
    }

    static unsigned Count(Env env, ConstSet s)
    {
        if (Traits::IsShort(env))
        {
            return BitSetSupport::CountBitsInIntegral(Word(s));
        }
        else
        {
            assert(s != UninitVal());
            return CountLong(Traits::GetWordCount(env), s);
        }
    }

    static bool IsEmptyUnion(Env env, ConstSet s1, ConstSet s2)
    {
        if (Traits::IsShort(env))
        {
            return (Word(s1) | Word(s2)) == 0;
        }
        else
        {
            return IsEmptyUnionLong(Traits::GetWordCount(env), s1, s2);
        }
    }

    static void UnionD(Env env, Set& s1, ConstSet s2)
    {
        if (Traits::IsShort(env))
        {
            s1 = Set(Word(s1) | Word(s2));
        }
        else
        {
            UnionDLong(Traits::GetWordCount(env), s1, s2);
        }
    }

    static void Union(Env env, Set& r, ConstSet s1, ConstSet s2)
    {
        if (Traits::IsShort(env))
        {
            r = Set(Word(s1) | Word(s2));
        }
        else
        {
            UnionLong(Traits::GetWordCount(env), r, s1, s2);
        }
    }

    static void DiffD(Env env, Set& s1, ConstSet s2)
    {
        if (Traits::IsShort(env))
        {
            s1 = Set(Word(s1) & ~Word(s2));
        }
        else
        {
            DiffDLong(Traits::GetWordCount(env), s1, s2);
        }
    }

    static void Diff(Env env, Set& r, ConstSet s1, ConstSet s2)
    {
        if (Traits::IsShort(env))
        {
            r = Set(Word(s1) & ~Word(s2));
        }
        else
        {
            DiffLong(Traits::GetWordCount(env), r, s1, s2);
        }
    }

    static void SymmetricDiff(Env env, Set& r, ConstSet s1, ConstSet s2)
    {
        if (Traits::IsShort(env))
        {
            r = Set(Word(s1) ^ Word(s2));
        }
        else
        {
            SymmetricDiffLong(Traits::GetWordCount(env), r, s1, s2);
        }
    }

    static void RemoveElemD(Env env, Set& s, unsigned i)
    {
        assert(i < Traits::GetSize(env));

        if (Traits::IsShort(env))
        {
            s = Set(Word(s) & ~(Word(1) << i));
        }
        else
        {
            RemoveElemDLong(s, i);
        }
    }

    static bool TryRemoveElemD(Env env, Set& s, unsigned i)
    {
        assert(i < Traits::GetSize(env));

        if (Traits::IsShort(env))
        {
            Word mask    = Word(1) << i;
            Word bits    = Word(s);
            bool removed = (bits & mask) != 0;
            s            = Set(bits & ~mask);
            return removed;
        }
        else
        {
            return TryRemoveElemDLong(s, i);
        }
    }

    static void AddElemD(Env env, Set& s, unsigned i)
    {
        assert(i < Traits::GetSize(env));

        if (Traits::IsShort(env))
        {
            s = Set(Word(s) | (Word(1) << i));
        }
        else
        {
            AddElemDLong(s, i);
        }
    }

    static bool TryAddElemD(Env env, Set& s, unsigned i)
    {
        assert(i < Traits::GetSize(env));

        if (Traits::IsShort(env))
        {
            Word mask  = Word(1) << i;
            Word bits  = Word(s);
            bool added = (bits & mask) == 0;
            s          = Set(bits | mask);
            return added;
        }
        else
        {
            return TryAddElemDLong(s, i);
        }
    }

    static bool IsMember(Env env, ConstSet s, unsigned i)
    {
        assert(i < Traits::GetSize(env));

        Word w;

        if (Traits::IsShort(env))
        {
            w = Word(s);
        }
        else
        {
            assert(s != UninitVal());
            w = s[i / WordBitSize];
        }

        return (w & (Word(1) << i)) != 0;
    }

    static void IntersectionD(Env env, Set& s1, ConstSet s2)
    {
        if (Traits::IsShort(env))
        {
            s1 = Set(Word(s1) & Word(s2));
        }
        else
        {
            IntersectionDLong(Traits::GetWordCount(env), s1, s2);
        }
    }

    static void Intersection(Env env, Set& r, ConstSet s1, ConstSet s2)
    {
        if (Traits::IsShort(env))
        {
            r = Set(Word(s1) & Word(s2));
        }
        else
        {
            IntersectionLong(Traits::GetWordCount(env), r, s1, s2);
        }
    }

    static bool IsEmptyIntersection(Env env, ConstSet s1, ConstSet s2)
    {
        if (Traits::IsShort(env))
        {
            return (Word(s1) & Word(s2)) == 0;
        }
        else
        {
            return IsEmptyIntersectionLong(Traits::GetWordCount(env), s1, s2);
        }
    }

    static void DataFlowD(Env env, Set& out, ConstSet gen, ConstSet in)
    {
        if (Traits::IsShort(env))
        {
            out = Set(Word(out) & (Word(gen) | Word(in)));
        }
        else
        {
            DataFlowDLong(Traits::GetWordCount(env), out, gen, in);
        }
    }

    static void LivenessD(Env env, Set& in, ConstSet def, ConstSet use, ConstSet out)
    {
        if (Traits::IsShort(env))
        {
            in = Set(Word(use) | (Word(out) & ~Word(def)));
        }
        else
        {
            LivenessDLong(Traits::GetWordCount(env), in, def, use, out);
        }
    }

    static bool IsSubset(Env env, ConstSet s1, ConstSet s2)
    {
        if (Traits::IsShort(env))
        {
            return (Word(s1) & Word(s2)) == Word(s1);
        }
        else
        {
            return IsSubsetLong(Traits::GetWordCount(env), s1, s2);
        }
    }

    static bool Equal(Env env, ConstSet s1, ConstSet s2)
    {
        return (s1 == s2) || (!Traits::IsShort(env) && EqualLong(Traits::GetWordCount(env), s1, s2));
    }

    class Iter
    {
        ConstSet m_current;
        ConstSet m_end;
        Word     m_bits;
        unsigned m_index = 0;

    public:
        Iter(Env env, ConstSet s) : m_current(s)
        {
            if (Traits::IsShort(env))
            {
                m_bits = Word(s);
                // Set the iteration end condition, valid even though this is not a pointer in the short case.
                m_end = s + 1;
            }
            else
            {
                assert(s != UninitVal());
                m_bits = s[0];
                m_end  = s + Traits::GetWordCount(env);
            }
        }

        bool NextElem(unsigned* pElem)
        {
            for (;;)
            {
                DWORD nextBit;
                bool  hasBit;
#ifdef HOST_64BIT
                static_assert_no_msg(sizeof(Word) == 8);
                hasBit = BitScanForward64(&nextBit, m_bits);
#else
                static_assert_no_msg(sizeof(Word) == 4);
                hasBit = BitScanForward(&nextBit, m_bits);
#endif

                // If there's a bit, doesn't matter if we're short or long.
                if (hasBit)
                {
                    *pElem = m_index + nextBit;
                    m_bits &= ~(Word(1) << nextBit); // clear bit we just found so we don't find it again

                    return true;
                }

                // Go to the next word. For short bitsets, this will hit the end condition and exit.
                ++m_current;

                if (m_current == m_end)
                {
                    return false;
                }

                // If we get here, it's not a short type, so get the next word.
                m_index += WordBitSize;
                m_bits = *m_current;
            }
        }
    };

    class Enumerator
    {
        Iter     iter;
        unsigned index = 0;

    public:
        Enumerator(Env env, ConstSet s) : iter(env, s)
        {
        }

        bool MoveNext()
        {
            return iter.NextElem(&index);
        }

        unsigned Current() const
        {
            return index;
        }
    };

    template <Word (*op)(Word, Word)>
    class EnumOp
    {
        ConstSet s1;
        ConstSet s2;
        Word     word;
        unsigned lastWordIndex;
        unsigned wordIndex = 0;
        unsigned bitIndex  = 0;

    public:
        EnumOp(Env env, ConstSet s1, ConstSet s2) : s1(s1), s2(s2)
        {
            if (Traits::IsShort(env))
            {
                word          = op(Word(s1), Word(s2));
                lastWordIndex = 0;
            }
            else
            {
                word          = op(s1[0], s2[0]);
                lastWordIndex = Traits::GetWordCount(env) - 1;
            }
        }

        bool MoveNext()
        {
            while (word == 0)
            {
                if (wordIndex == lastWordIndex)
                {
                    return false;
                }

                wordIndex++;
                word = op(s1[wordIndex], s2[wordIndex]);
            }

            DWORD nextBit;
#ifdef HOST_64BIT
            static_assert_no_msg(sizeof(Word) == 8);
            if (!BitScanForward64(&nextBit, word))
#else
            static_assert_no_msg(sizeof(Word) == 4);
            if (!BitScanForward(&nextBit, word))
#endif
            {
                return false;
            }

            bitIndex = wordIndex * WordBitSize + nextBit;
            word &= ~(Word(1) << nextBit);

            return true;
        }

        unsigned Current() const
        {
            return bitIndex;
        }
    };

    static Word IntersectionOp(Word x, Word y)
    {
        return x & y;
    }

    static Word SymmetricDiffOp(Word x, Word y)
    {
        return x ^ y;
    }

    static Word DiffOp(Word x, Word y)
    {
        return x & ~y;
    }

private:
    static Set AllocLong(Env env)
    {
        assert(!Traits::IsShort(env));
        return Set(Traits::Alloc(env, Traits::GetWordCount(env)));
    }

    static Set MakeEmptyLong(Env env)
    {
        assert(!Traits::IsShort(env));

        unsigned len = Traits::GetWordCount(env);
        Set      r   = Set(Traits::Alloc(env, len));

        for (unsigned i = 0; i < len; i++)
        {
            r[i] = 0;
        }

        return r;
    }

    static Set MakeFullLong(Env env)
    {
        assert(!Traits::IsShort(env));

        unsigned len = Traits::GetWordCount(env);
        Set      r   = Set(Traits::Alloc(env, len));

        for (unsigned i = 0; i < len - 1; i++)
        {
            r[i] = ~Word(0);
        }

        r[len - 1] = ~Word(0) >> (WordBitSize - (Traits::GetSize(env) - (len - 1) * WordBitSize));

        return r;
    }

    static Set MakeCopyLong(Env env, ConstSet s)
    {
        assert(!Traits::IsShort(env));

        unsigned len = Traits::GetWordCount(env);
        Set      r   = Set(Traits::Alloc(env, len));

        for (unsigned i = 0; i < len; i++)
        {
            r[i] = s[i];
        }

        return r;
    }

    static Set MakeSingletonLong(Env env, unsigned index)
    {
        Set s                  = MakeEmptyLong(env);
        s[index / WordBitSize] = Word(1) << (index % WordBitSize);
        return s;
    }

    static void AssignLong(unsigned len, Set lhs, ConstSet rhs)
    {
        for (unsigned i = 0; i < len; i++)
        {
            lhs[i] = rhs[i];
        }
    }

    static bool IsEmptyLong(unsigned len, ConstSet s)
    {
        for (unsigned i = 0; i < len; i++)
        {
            if (s[i] != 0)
            {
                return false;
            }
        }
        return true;
    }

    static unsigned CountLong(unsigned len, ConstSet s)
    {
        unsigned count = 0;
        for (unsigned i = 0; i < len; i++)
        {
            count += BitSetSupport::CountBitsInIntegral(s[i]);
        }
        return count;
    }

    static void UnionDLong(unsigned len, Set s1, ConstSet s2)
    {
        for (unsigned i = 0; i < len; i++)
        {
            s1[i] |= s2[i];
        }
    }

    static void UnionLong(unsigned len, Set r, ConstSet s1, ConstSet s2)
    {
        for (unsigned i = 0; i < len; i++)
        {
            r[i] = s1[i] | s2[i];
        }
    }

    static void DiffDLong(unsigned len, Set s1, ConstSet s2)
    {
        for (unsigned i = 0; i < len; i++)
        {
            s1[i] &= ~s2[i];
        }
    }

    static void DiffLong(unsigned len, Set r, ConstSet s1, ConstSet s2)
    {
        for (unsigned i = 0; i < len; i++)
        {
            r[i] = s1[i] & ~s2[i];
        }
    }

    static void SymmetricDiffLong(unsigned len, Set r, ConstSet s1, ConstSet s2)
    {
        for (unsigned i = 0; i < len; i++)
        {
            r[i] = s1[i] ^ s2[i];
        }
    }

    static void AddElemDLong(Set s, unsigned i)
    {
        s[i / WordBitSize] |= Word(1) << (i % WordBitSize);
    }

    static bool TryAddElemDLong(Set s, unsigned i)
    {
        unsigned index = i / WordBitSize;
        Word     mask  = Word(1) << (i % WordBitSize);
        Word     bits  = s[index];
        bool     added = (bits & mask) == 0;
        s[index]       = bits | mask;
        return added;
    }

    static void RemoveElemDLong(Set s, unsigned i)
    {
        s[i / WordBitSize] &= ~(Word(1) << (i % WordBitSize));
    }

    static bool TryRemoveElemDLong(Set s, unsigned i)
    {
        unsigned index   = i / WordBitSize;
        Word     mask    = Word(1) << (i % WordBitSize);
        Word     bits    = s[index];
        bool     removed = (bits & mask) != 0;
        s[index]         = bits & ~mask;
        return removed;
    }

    static void ClearDLong(unsigned len, Set s)
    {
        for (unsigned i = 0; i < len; i++)
        {
            s[i] = 0;
        }
    }

    static bool IsMemberLong(ConstSet s, unsigned i)
    {
        return (s[i / WordBitSize] & (Word(1) << (i % WordBitSize))) != 0;
    }

    static void IntersectionDLong(unsigned len, Set s1, ConstSet s2)
    {
        for (unsigned i = 0; i < len; i++)
        {
            s1[i] &= s2[i];
        }
    }

    static void IntersectionLong(unsigned len, Set r, ConstSet s1, ConstSet s2)
    {
        for (unsigned i = 0; i < len; i++)
        {
            r[i] = s1[i] & s2[i];
        }
    }

    static bool IsEmptyIntersectionLong(unsigned len, ConstSet s1, ConstSet s2)
    {
        for (unsigned i = 0; i < len; i++)
        {
            if ((s1[i] & s2[i]) != 0)
            {
                return false;
            }
        }
        return true;
    }

    static bool IsEmptyUnionLong(unsigned len, ConstSet s1, ConstSet s2)
    {
        for (unsigned i = 0; i < len; i++)
        {
            if ((s1[i] | s2[i]) != 0)
            {
                return false;
            }
        }
        return true;
    }

    static void DataFlowDLong(unsigned len, Set out, ConstSet gen, ConstSet in)
    {
        for (unsigned i = 0; i < len; i++)
        {
            out[i] = out[i] & (gen[i] | in[i]);
        }
    }

    static void LivenessDLong(unsigned len, Set in, ConstSet def, ConstSet use, ConstSet out)
    {
        for (unsigned i = 0; i < len; i++)
        {
            in[i] = use[i] | (out[i] & ~def[i]);
        }
    }

    static bool EqualLong(unsigned len, ConstSet s1, ConstSet s2)
    {
        for (unsigned i = 0; i < len; i++)
        {
            if (s1[i] != s2[i])
            {
                return false;
            }
        }
        return true;
    }

    static bool IsSubsetLong(unsigned len, ConstSet s1, ConstSet s2)
    {
        for (unsigned i = 0; i < len; i++)
        {
            if ((s1[i] & s2[i]) != s1[i])
            {
                return false;
            }
        }
        return true;
    }
};
