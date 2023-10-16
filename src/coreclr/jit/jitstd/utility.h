// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.



#pragma once

#include "clr_std/type_traits"

namespace jitstd
{

    template<class To, class From>
    To bit_cast(const From& from)
    {
        static_assert_no_msg(sizeof(To) == sizeof(From));

        To to;
        memcpy(&to, &from, sizeof(to));
        return to;
    }

    template<class T>
    constexpr T byteswap(T) = delete;

    template<>
    constexpr uint16_t byteswap(uint16_t v)
    {
        return ((v >> 8) & 0xFF) | ((v << 8) & 0xFF00);
    }

    template<>
    constexpr uint32_t byteswap(uint32_t v)
    {
        return
            ((v >> 24) & 0xFFu) |
            ((v >> 8) & 0xFF00u) |
            ((v << 8) & 0xFF0000u) |
            ((v << 24) & 0xFF000000u);
    }

    template<>
    constexpr uint64_t byteswap(uint64_t v)
    {
        return
            ((v >> 56) & 0xFFull) |
            ((v >> 40) & 0xFF00ull) |
            ((v >> 24) & 0xFF0000ull) |
            ((v >> 8) & 0xFF000000ull) |
            ((v << 8) & 0xFF00000000ull) |
            ((v << 24) & 0xFF0000000000ull) |
            ((v << 40) & 0xFF000000000000ull) |
            ((v << 56) & 0xFF00000000000000ull);
    }

    template<>
    constexpr int32_t byteswap(int32_t v)
    {
        return static_cast<int32_t>(byteswap(static_cast<uint32_t>(v)));
    }

    template<>
    constexpr int64_t byteswap(int64_t v)
    {
        return static_cast<int64_t>(byteswap(static_cast<uint64_t>(v)));
    }

    template<class T>
    constexpr T rotl(T, int) = delete;

    template<>
    constexpr uint32_t rotl(uint32_t v, int s)
    {
        return (v << (s & 31)) | (v >> ((32 - s) & 31));
    }

    template<>
    constexpr uint64_t rotl(uint64_t v, int s)
    {
        return (v << (s & 63)) | (v >> ((64 - s) & 63));
    }

    template<class T>
    constexpr T rotr(T, int) = delete;

    template<>
    constexpr uint32_t rotr(uint32_t v, int s)
    {
        return (v << ((32 - s) & 31)) | (v >> (s & 31));
    }

    template<>
    constexpr uint64_t rotr(uint64_t v, int s)
    {
        return (v << ((64 - s) & 63)) | (v >> (s & 63));
    }

namespace utility
{
    // Template class for scoped execution of a lambda.
    // Usage:
    //
    //  auto code = [&]
    //  {
    //      JITDUMP("finally()");
    //  };
    //  jitstd::utility::scoped_code<decltype(code)> finally(code);
    //  "code" will execute when "finally" goes out of scope.
    template <typename T>
    class scoped_code
    {
    public:
        const T& l;
        scoped_code(const T& l) : l(l) { }
        ~scoped_code() { l(); }
    };
} // end of namespace utility.

} // end of namespace jitstd.
