// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

namespace jitstd
{
template <class Type, size_t Size>
class array
{
    static_assert(Size != 0, "0-sized array not supported");

    Type m_data[Size];

public:
    constexpr Type& operator[](size_t i)
    {
        assert(i < Size);
        return m_data[i];
    }

    constexpr const Type& operator[](size_t i) const
    {
        assert(i < Size);
        return m_data[i];
    }

    constexpr const Type* data() const
    {
        return m_data;
    }

    constexpr size_t size() const
    {
        return Size;
    }
};
}
