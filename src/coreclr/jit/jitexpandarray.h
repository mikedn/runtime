// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

template <class T>
class JitExpandArray
{
    CompAllocator m_alloc;
    T*            m_data = nullptr;
    unsigned      m_size = 0;
    unsigned      m_minSize;

    void EnsureSize(unsigned size)
    {
        assert(size >= m_size);

        unsigned oldSize = m_size;
        T*       oldData = m_data;
        unsigned newSize = Max(size, Max(m_minSize, oldSize * 2));
        T*       newData = m_alloc.allocate<T>(newSize);

        if (oldData != nullptr)
        {
            for (unsigned i = 0; i < oldSize; i++)
            {
                newData[i] = oldData[i];
            }

            m_alloc.deallocate(oldData);
        }

        for (unsigned i = oldSize; i < newSize; i++)
        {
            newData[i] = T();
        }

        m_data = newData;
        m_size = newSize;
    }

public:
    JitExpandArray(CompAllocator alloc, unsigned minSize) : m_alloc(alloc), m_minSize(minSize)
    {
    }

    T& operator[](unsigned i)
    {
        if (i >= m_size)
        {
            EnsureSize(i + 1);
        }

        return m_data[i];
    }
};

template <class T>
class JitVector
{
    CompAllocator m_alloc;
    T*            m_data     = nullptr;
    unsigned      m_size     = 0;
    unsigned      m_capacity = 0;

    void EnsureCapacity(unsigned newCapacity)
    {
        assert(newCapacity >= m_capacity);

        newCapacity = Max(newCapacity, Max(m_capacity * 2, 4u));

        T* oldData = m_data;
        T* newData = m_alloc.allocate<T>(newCapacity);

        if (oldData != nullptr)
        {
            for (unsigned i = 0; i < m_size; i++)
            {
                newData[i] = oldData[i];
            }

            m_alloc.deallocate(oldData);
        }

        m_data     = newData;
        m_capacity = newCapacity;
    }

public:
    JitVector(CompAllocator alloc) : m_alloc(alloc)
    {
    }

    unsigned Size() const
    {
        return m_size;
    }

    bool Empty() const
    {
        return m_size == 0;
    }

    void Clear()
    {
        m_size = 0;
    }

    T& operator[](unsigned i)
    {
        assert(i < m_size);
        return m_data[i];
    }

    const T& operator[](unsigned i) const
    {
        assert(i < m_size);
        return m_data[i];
    }

    void Reserve(unsigned capacity)
    {
        if (capacity > m_capacity)
        {
            EnsureCapacity(capacity);
        }
    }

    void Add(const T& value)
    {
        if (m_size == m_capacity)
        {
            EnsureCapacity(m_capacity + 1);
        }

        m_data[m_size++] = value;
    }

    template <typename... Args>
    void Emplace(Args&&... args)
    {
        if (m_size == m_capacity)
        {
            EnsureCapacity(m_capacity + 1);
        }

        new (&m_data[m_size++]) T(std::forward<Args>(args)...);
    }

    void Remove(unsigned i)
    {
        assert(i < m_size);

        for (unsigned j = i + 1; j < m_size; j++)
        {
            m_data[j - 1] = m_data[j];
        }

        m_size--;
    }

    T* begin()
    {
        return m_data;
    }

    const T* begin() const
    {
        return m_data;
    }

    T* end()
    {
        return m_data + m_size;
    }

    const T* end() const
    {
        return m_data + m_size;
    }
};
