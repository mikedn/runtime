// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

class BitSetSupport
{
public:
    // This maps 4-bit ("nibble") values into the number of 1 bits they contain.
    static const unsigned BitCountTable[16];

    // Returns the number of 1 bits in the binary representation of "u".
    template <typename T>
    static unsigned CountBitsInIntegral(T u)
    {
        unsigned res = 0;
        // We process "u" in 4-bit nibbles, hence the "*2" below.
        for (unsigned int i = 0; i < sizeof(T) * 2; i++)
        {
            res += BitCountTable[u & 0xf];
            u >>= 4;
        }
        return res;
    }

#ifdef DEBUG
    // This runs the "TestSuite" method for a few important instantiations of BitSet.
    static void TestSuite(CompAllocator env);
#endif
};

template <>
FORCEINLINE unsigned BitSetSupport::CountBitsInIntegral<uint64_t>(uint64_t c)
{
    // TODO-MIKE-Throughput: Use popcnt.

    c = c - ((c >> 1) & 0x5555555555555555ull);
    c = (c & 0x3333333333333333ull) + ((c >> 2) & 0x3333333333333333ull);
    c = ((c + (c >> 4)) & 0x0F0F0F0F0F0F0F0Full) * 0x0101010101010101ull;
    return static_cast<unsigned>(c >> 56);
}

template <>
FORCEINLINE unsigned BitSetSupport::CountBitsInIntegral<uint32_t>(uint32_t c)
{
    c = c - ((c >> 1) & 0x55555555u);
    c = (c & 0x33333333u) + ((c >> 2) & 0x33333333u);
    c = ((c + (c >> 4)) & 0xF0F0F0Fu) * 0x1010101u;
    return static_cast<unsigned>(c >> 24);
}

template <typename BitSetType, typename BitSetTraits>
class BitSetOps
{
};

template <typename T>
inline T genFindLowestBit(T value)
{
    return (value & (0 - value));
}

template <typename T>
inline unsigned genCountBits(T bits)
{
    unsigned cnt = 0;

    while (bits)
    {
        cnt++;
        bits -= genFindLowestBit(bits);
    }

    return cnt;
}

template <>
inline unsigned genCountBits<uint64_t>(uint64_t c)
{
    return BitSetSupport::CountBitsInIntegral(c);
}

template <>
inline unsigned genCountBits<uint32_t>(uint32_t c)
{
    return BitSetSupport::CountBitsInIntegral(c);
}
