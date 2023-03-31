// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

template <class T, unsigned InlineCapacity = 8>
class ArrayStack
{
    T*            m_data;
    unsigned      m_size;
    unsigned      m_capacity;
    CompAllocator m_alloc;
    char          m_inlineData[InlineCapacity * sizeof(T)];

    NOINLINE void Realloc()
    {
        unsigned newCapacity = m_capacity * 2;
        noway_assert(newCapacity > m_capacity);
        T* newData = m_alloc.allocate<T>(newCapacity);

        for (size_t i = 0; i < m_capacity; i++)
        {
            newData[i] = m_data[i];
        }

        m_capacity = newCapacity;
        m_data     = newData;
    }

public:
    ArrayStack(CompAllocator alloc)
        : m_data(reinterpret_cast<T*>(m_inlineData)), m_size(0), m_capacity(InlineCapacity), m_alloc(alloc)
    {
    }

    void Push(T item)
    {
        if (m_size == m_capacity)
        {
            Realloc();
        }

        m_data[m_size] = item;
        m_size++;
    }

    template <typename... Args>
    void Emplace(Args&&... args)
    {
        if (m_size == m_capacity)
        {
            Realloc();
        }

        new (&m_data[m_size]) T(std::forward<Args>(args)...);
        m_size++;
    }

    T Pop()
    {
        assert(m_size > 0);
        m_size--;
        return m_data[m_size];
    }

    // Pop `count` elements from the stack
    void Pop(unsigned count)
    {
        assert(count <= m_size);
        m_size -= count;
    }

    // Return the i'th element from the top
    T Top(unsigned i = 0) const
    {
        assert(i < m_size);
        return m_data[m_size - 1 - i];
    }

    // Return a reference to the i'th element from the top
    T& TopRef(unsigned i = 0)
    {
        assert(i < m_size);
        return m_data[m_size - 1 - i];
    }

    // Return the i'th element from the bottom
    T Bottom(unsigned i = 0)
    {
        assert(i < m_size);
        return m_data[i];
    }

    // Return a reference to the i'th element from the bottom
    T& BottomRef(unsigned i = 0)
    {
        assert(i < m_size);
        return m_data[i];
    }

    T Get(unsigned i) const
    {
        assert(i < m_size);
        return m_data[i];
    }

    T& GetRef(unsigned i) const
    {
        assert(i < m_size);
        return m_data[i];
    }

    void Set(unsigned i, const T& item)
    {
        assert(i < m_size);
        m_data[i] = item;
    }

    unsigned Size() const
    {
        return m_size;
    }

    bool Empty()
    {
        return m_size == 0;
    }

    void Clear()
    {
        m_size = 0;
    }

    bool Contains(const T& value) const
    {
        for (unsigned i = 0; i < m_size; i++)
        {
            if (m_data[i] == value)
            {
                return true;
            }
        }

        return false;
    }
};
