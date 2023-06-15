// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

template <class T>
class JitExpandArray
{
protected:
    CompAllocator m_alloc;
    T*            m_data;
    unsigned      m_size;
    unsigned      m_minSize;

    void EnsureCoversInd(unsigned idx);

    void InitializeRange(unsigned low, unsigned high)
    {
        assert(m_data != nullptr);
        assert((low <= high) && (high <= m_size));
        for (unsigned i = low; i < high; i++)
        {
            m_data[i] = T();
        }
    }

public:
    JitExpandArray(CompAllocator alloc, unsigned minSize = 1)
        : m_alloc(alloc), m_data(nullptr), m_size(0), m_minSize(minSize)
    {
        assert(minSize > 0);
    }

    T& GetRef(unsigned idx)
    {
        if (idx >= m_size)
        {
            EnsureCoversInd(idx);
        }

        return m_data[idx];
    }
};

template <class T>
class JitExpandArrayStack : public JitExpandArray<T>
{
    unsigned m_used;

public:
    JitExpandArrayStack(CompAllocator alloc, unsigned minSize = 1) : JitExpandArray<T>(alloc, minSize), m_used(0)
    {
    }

    void Set(unsigned idx, T val)
    {
        if (idx >= m_size)
        {
            EnsureCoversInd(idx);
        }

        m_data[idx] = val;
        m_used      = max((idx + 1), m_used);
    }

    void Reset()
    {
        if (m_minSize > m_size)
        {
            EnsureCoversInd(m_minSize - 1);
        }

        InitializeRange(0, m_size);

        m_used = 0;
    }

    void Push(T val)
    {
        unsigned res = m_used;

        if (m_used >= m_size)
        {
            EnsureCoversInd(m_used);
        }

        m_data[m_used] = val;
        m_used++;
    }

    T& operator[](unsigned i) const
    {
        assert(i < m_used);
        return m_data[i];
    }

    void Remove(unsigned idx)
    {
        assert(idx < m_used);
        if (idx < m_used - 1)
        {
            memmove(&m_data[idx], &m_data[idx + 1], (m_used - idx - 1) * sizeof(T));
        }
        m_used--;
    }

    unsigned Size() const
    {
        return m_used;
    }
};

template <class T>
NOINLINE void JitExpandArray<T>::EnsureCoversInd(unsigned idx)
{
    assert(idx >= m_size);

    unsigned oldSize    = m_size;
    T*       oldMembers = m_data;
    m_size              = max(idx + 1, max(m_minSize, m_size * 2));
    m_data              = m_alloc.allocate<T>(m_size);
    if (oldMembers != nullptr)
    {
        memcpy(m_data, oldMembers, oldSize * sizeof(T));
        m_alloc.deallocate(oldMembers);
    }
    InitializeRange(oldSize, m_size);
}
