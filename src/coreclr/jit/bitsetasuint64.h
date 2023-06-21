// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "bitset.h"

template <typename BitSetTraits>
class BitSetOps<uint64_t, BitSetTraits>
{
public:
    using Env        = typename BitSetTraits::Env;
    using Rep        = uint64_t;
    using ValArgType = const uint64_t;
    using RetValType = uint64_t;

private:
    static uint64_t Singleton(unsigned bitNum)
    {
        assert(bitNum < sizeof(uint64_t) * CHAR_BIT);
        return (uint64_t)1 << bitNum;
    }

public:
    static void Assign(Env env, uint64_t& lhs, uint64_t rhs)
    {
        lhs = rhs;
    }

    static void AssignNouninit(Env env, uint64_t& lhs, uint64_t rhs)
    {
        lhs = rhs;
    }

    static void AssignAllowUninitRhs(Env env, uint64_t& lhs, uint64_t rhs)
    {
        lhs = rhs;
    }

    static void AssignNoCopy(Env env, uint64_t& lhs, uint64_t rhs)
    {
        lhs = rhs;
    }

    static void ClearD(Env env, uint64_t& bs)
    {
        bs = 0;
    }

    static uint64_t MakeSingleton(Env env, unsigned bitNum)
    {
        assert(bitNum < BitSetTraits::GetSize(env));
        return Singleton(bitNum);
    }

    static uint64_t MakeCopy(Env env, uint64_t bs)
    {
        return bs;
    }

    static bool IsEmpty(Env env, uint64_t bs)
    {
        return bs == 0;
    }

    static unsigned Count(Env env, uint64_t bs)
    {
        return BitSetSupport::CountBitsInIntegral(bs);
    }

    static bool IsEmptyUnion(Env env, uint64_t bs1, uint64_t bs2)
    {
        return (bs1 | bs2) == 0;
    }

    static void UnionD(Env env, uint64_t& bs1, uint64_t bs2)
    {
        bs1 |= bs2;
    }

    static uint64_t Union(Env env, uint64_t& bs1, uint64_t bs2)
    {
        return bs1 | bs2;
    }

    static void DiffD(Env env, uint64_t& bs1, uint64_t bs2)
    {
        bs1 = bs1 & ~bs2;
    }

    static uint64_t Diff(Env env, uint64_t bs1, uint64_t bs2)
    {
        return bs1 & ~bs2;
    }

    static void RemoveElemD(Env env, uint64_t& bs1, unsigned i)
    {
        assert(i < BitSetTraits::GetSize(env));
        bs1 &= ~Singleton(i);
    }

    static uint64_t RemoveElem(Env env, uint64_t bs1, unsigned i)
    {
        return bs1 & ~Singleton(i);
    }

    static void AddElemD(Env env, uint64_t& bs1, unsigned i)
    {
        assert(i < BitSetTraits::GetSize(env));
        bs1 |= Singleton(i);
    }

    static uint64_t AddElem(Env env, uint64_t bs1, unsigned i)
    {
        assert(i < BitSetTraits::GetSize(env));
        return bs1 | Singleton(i);
    }

    static bool IsMember(Env env, const uint64_t bs1, unsigned i)
    {
        assert(i < BitSetTraits::GetSize(env));
        return (bs1 & Singleton(i)) != 0;
    }

    static void IntersectionD(Env env, uint64_t& bs1, uint64_t bs2)
    {
        bs1 &= bs2;
    }

    static uint64_t Intersection(Env env, uint64_t bs1, uint64_t bs2)
    {
        return bs1 & bs2;
    }

    static bool IsEmptyIntersection(Env env, uint64_t bs1, uint64_t bs2)
    {
        return (bs1 & bs2) == 0;
    }

    static void LivenessD(Env env, uint64_t& in, const uint64_t def, const uint64_t use, const uint64_t out)
    {
        in = use | (out & ~def);
    }

    static bool IsSubset(Env env, uint64_t bs1, uint64_t bs2)
    {
        return ((bs1 & bs2) == bs1);
    }

    static bool Equal(Env env, uint64_t bs1, uint64_t bs2)
    {
        return bs1 == bs2;
    }

    static uint64_t MakeEmpty(Env env)
    {
        return 0;
    }

    static uint64_t MakeFull(Env env)
    {
        unsigned sz = BitSetTraits::GetSize(env);
        if (sz == sizeof(uint64_t) * 8)
        {
            return uint64_t(-1);
        }
        else
        {
            return (uint64_t(1) << sz) - 1;
        }
    }

#ifdef DEBUG
    static const char* ToString(Env env, uint64_t bs)
    {
        const int CharsForUINT64 = sizeof(uint64_t) * 2;
        char*     res            = nullptr;
        const int AllocSize      = CharsForUINT64 + 4;
        res                      = (char*)BitSetTraits::DebugAlloc(env, AllocSize);
        uint64_t bits            = bs;
        unsigned remaining       = AllocSize;
        char*    ptr             = res;
        for (unsigned bytesDone = 0; bytesDone < sizeof(uint64_t); bytesDone += sizeof(unsigned))
        {
            unsigned bits0 = (unsigned)bits;
            sprintf_s(ptr, remaining, "%08X", bits0);
            ptr += 8;
            remaining -= 8;
            bytesDone += 4;
            assert(sizeof(unsigned) == 4);
            // Doing this twice by 16, rather than once by 32, avoids warnings when size_t == unsigned.
            bits = bits >> 16;
            bits = bits >> 16;
        }
        return res;
    }
#endif

    static uint64_t UninitVal()
    {
        return 0;
    }

    static bool MayBeUninit(uint64_t bs)
    {
        return bs == UninitVal();
    }

    class Iter
    {
        uint64_t m_bits;

        // The number of bits that have already been iterated over (set or clear).
        unsigned m_bitNum;

    public:
        Iter(Env env, const uint64_t& bits) : m_bits(bits), m_bitNum(0)
        {
        }

        bool NextElem(unsigned* pElem)
        {
            // TODO-Throughtput: use BitScanForward64() intrinsic (see short/long implementation).
            if (m_bits)
            {
                unsigned bitNum = m_bitNum;
                while ((m_bits & 0x1) == 0)
                {
                    bitNum++;
                    m_bits >>= 1;
                }
                *pElem   = bitNum;
                m_bitNum = bitNum + 1;
                m_bits >>= 1;
                return true;
            }
            else
            {
                return false;
            }
        }
    };
};
