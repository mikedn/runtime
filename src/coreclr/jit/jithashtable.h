// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

template <class T>
struct JitHashFuncs
{
    static bool Equals(const T& x, const T& y)
    {
        return T::Equals(x, y);
    }

    static unsigned GetHashCode(const T& val)
    {
        return T::GetHashCode(val);
    }
};

// Stores info about primes, including the magic number and shift amount needed
// to implement a divide without using the divide instruction
class JitPrimeInfo
{
    unsigned prime = 0;
    unsigned magic = 0;
    unsigned shift = 0;

public:
    constexpr JitPrimeInfo() = default;

    constexpr JitPrimeInfo(unsigned p, unsigned m, unsigned s) : prime(p), magic(m), shift(s + 32)
    {
    }

    unsigned GetPrime() const
    {
        return prime;
    }

    unsigned Divide(unsigned numerator) const
    {
        uint64_t num     = numerator;
        uint64_t mag     = magic;
        uint64_t product = (num * mag) >> shift;
        return static_cast<unsigned>(product);
    }

    unsigned Remainder(unsigned numerator) const
    {
        unsigned div    = Divide(numerator);
        unsigned result = numerator - (div * prime);
        assert(result == numerator % prime);
        return result;
    }
};

extern const JitPrimeInfo jitPrimeInfo[27];

struct JitHashTableBehavior
{
    // Factor to grow allocation
    static constexpr unsigned s_growth_factor_numerator   = 3;
    static constexpr unsigned s_growth_factor_denominator = 2;

    // Maximum occupied density of table before growth occurs
    static constexpr unsigned s_density_factor_numerator   = 3;
    static constexpr unsigned s_density_factor_denominator = 4;

    // Minimum table allocation count (size on first growth).
    // It is probably preferable to call Reallocate on initialization
    // rather than override this from the default traits.
    static constexpr unsigned s_minimum_allocation = 7;

    static void DECLSPEC_NORETURN NoMemory()
    {
        NOMEM();
    }
};

template <class Key,
          class Value,
          class HashFuncs = JitHashFuncs<Key>,
          class Allocator = CompAllocator,
          class Behavior  = JitHashTableBehavior>
class JitHashMap
{
    struct Node;

    Allocator    m_alloc;
    Node**       m_buckets{nullptr};
    JitPrimeInfo m_sizeInfo;
    unsigned     m_count{0};
    unsigned     m_maxCount{0};

public:
    struct Entry
    {
        const Key key;
        Value     value;
    };

    class iterator;

    JitHashMap(Allocator alloc) : m_alloc(alloc)
    {
        static_assert_no_msg(Behavior::s_growth_factor_numerator > Behavior::s_growth_factor_denominator);
        static_assert_no_msg(Behavior::s_density_factor_numerator < Behavior::s_density_factor_denominator);
    }

    ~JitHashMap()
    {
        Clear();
    }

    bool Find(Key k, Value* value) const
    {
        if (Node* n = FindNode(k))
        {
            *value = n->value.value;
            return true;
        }

        return false;
    }

    Value* Find(Key k) const
    {
        if (Node* n = FindNode(k))
        {
            return &n->value.value;
        }

        return nullptr;
    }

    Value& at(Key k) const
    {
        Node* n = FindNode(k);
        assert(n != nullptr);
        return n->value.value;
    }

    void Add(Key k, Value v)
    {
        CheckGrowth();

        assert(GetBucketCount() != 0);

        unsigned index = GetBucketIndex(k);

        for (Node* n = m_buckets[index]; n != nullptr; n = n->next)
        {
            if (HashFuncs::Equals(k, n->value.key))
            {
                unreached();
            }
        }

        m_buckets[index] = new (m_alloc) Node(m_buckets[index], k, v);
        m_count++;
    }

    template <class... Args>
    Value* Emplace(Key k, Args&&... args)
    {
        CheckGrowth();

        assert(GetBucketCount() != 0);

        unsigned index = GetBucketIndex(k);

        Node* n = m_buckets[index];

        while ((n != nullptr) && !HashFuncs::Equals(k, n->value.key))
        {
            n = n->next;
        }

        if (n == nullptr)
        {
            n = new (m_alloc) Node(m_buckets[index], k, std::forward<Args>(args)...);

            m_buckets[index] = n;
            m_count++;
        }

        return &n->value.value;
    }

    Value& operator[](Key k)
    {
        return *Emplace(k);
    }

    bool Remove(Key k)
    {
        unsigned index = GetBucketIndex(k);

        Node*  node = m_buckets[index];
        Node** link = &m_buckets[index];

        while ((node != nullptr) && !HashFuncs::Equals(k, node->value.key))
        {
            link = &node->next;
            node = node->next;
        }

        if (node == nullptr)
        {
            return false;
        }

        *link = node->next;
        m_count--;
        Node::operator delete(node, m_alloc);
        return true;
    }

    void Clear()
    {
        for (unsigned i = 0, count = GetBucketCount(); i < count; i++)
        {
            for (Node* n = m_buckets[i]; n != nullptr;)
            {
                Node* next = n->next;
                Node::operator delete(n, m_alloc);
                n = next;
            }
        }

        m_alloc.deallocate(m_buckets);

        m_buckets  = nullptr;
        m_sizeInfo = {};
        m_count    = 0;
        m_maxCount = 0;
    }

    iterator begin() const
    {
        return iterator(this);
    }

    iterator end() const
    {
        return iterator();
    }

    unsigned GetCount() const
    {
        return m_count;
    }

    Allocator GetAllocator()
    {
        return m_alloc;
    }

private:
    unsigned GetBucketCount() const
    {
        return m_sizeInfo.GetPrime();
    }

    unsigned GetBucketIndex(Key k) const
    {
        return m_sizeInfo.Remainder(HashFuncs::GetHashCode(k));
    }

    Node* FindNode(Key k) const
    {
        if (GetBucketCount() == 0)
        {
            return nullptr;
        }

        unsigned index = GetBucketIndex(k);

        Node* n = m_buckets[index];

        while ((n != nullptr) && !HashFuncs::Equals(k, n->value.key))
        {
            n = n->next;
        }

        return n;
    }

    void Grow()
    {
        unsigned newSize = m_count * Behavior::s_growth_factor_numerator / Behavior::s_growth_factor_denominator *
                           Behavior::s_density_factor_denominator / Behavior::s_density_factor_numerator;

        if (newSize < Behavior::s_minimum_allocation)
        {
            newSize = Behavior::s_minimum_allocation;
        }

        if (newSize < m_count)
        {
            Behavior::NoMemory();
        }

        Reallocate(newSize);
    }

    void CheckGrowth()
    {
        if (m_count == m_maxCount)
        {
            Grow();
        }
    }

public:
    void Reallocate(unsigned newTableSize)
    {
        assert(newTableSize >=
               (m_count * Behavior::s_density_factor_denominator / Behavior::s_density_factor_numerator));

        JitPrimeInfo newPrime = NextPrime(newTableSize);
        newTableSize          = newPrime.GetPrime();

        Node** newTable = m_alloc.template allocate<Node*>(newTableSize);

        for (unsigned i = 0; i < newTableSize; i++)
        {
            newTable[i] = nullptr;
        }

        for (unsigned i = 0, count = GetBucketCount(); i < count; i++)
        {
            for (Node* n = m_buckets[i]; n != nullptr;)
            {
                Node* next = n->next;

                unsigned newIndex  = newPrime.Remainder(HashFuncs::GetHashCode(n->value.key));
                n->next            = newTable[newIndex];
                newTable[newIndex] = n;

                n = next;
            }
        }

        if (m_buckets != nullptr)
        {
            m_alloc.deallocate(m_buckets);
        }

        m_buckets  = newTable;
        m_sizeInfo = newPrime;
        m_maxCount = newTableSize * Behavior::s_density_factor_numerator / Behavior::s_density_factor_denominator;
    }

    class iterator
    {
        friend class JitHashMap;

        Node*  m_node = nullptr;
        Node** m_buckets;
        Node** m_bucketsEnd;

        iterator() = default;

        iterator(const JitHashMap* hash)
            : m_buckets(hash->m_buckets), m_bucketsEnd(hash->m_buckets + hash->GetBucketCount())
        {
            if (hash->m_count > 0)
            {
                FindNextBucket();
            }
        }

        void FindNextBucket()
        {
            while ((m_buckets < m_bucketsEnd) && ((m_node = *m_buckets++) == nullptr))
            {
            }
        }

    public:
        bool operator==(const iterator& i) const
        {
            return i.m_node == m_node;
        }

        bool operator!=(const iterator& i) const
        {
            return i.m_node != m_node;
        }

        void operator++()
        {
            m_node = m_node->next;

            if (m_node == nullptr)
            {
                FindNextBucket();
            }
        }

        Entry& operator*()
        {
            return m_node->value;
        }

        Entry* operator->()
        {
            return &m_node->value;
        }

        void SetValue(const Value& value) const
        {
            m_node->value.value = value;
        }
    };

private:
    static const JitPrimeInfo& NextPrime(unsigned number)
    {
        for (const JitPrimeInfo& info : jitPrimeInfo)
        {
            if (info.GetPrime() >= number)
            {
                return info;
            }
        }

        Behavior::NoMemory();
    }

    struct Node
    {
        Node* next;
        Entry value;

        template <class... Args>
        Node(Node* next, Key k, Args&&... args) : next(next), value{k, Value(std::forward<Args>(args)...)}
        {
        }

        void* operator new(size_t sz, Allocator alloc)
        {
            return alloc.template allocate<uint8_t>(sz);
        }

        void operator delete(void* p, Allocator alloc)
        {
            alloc.deallocate(p);
        }
    };
};

template <class Value,
          class HashFuncs = JitHashFuncs<Value>,
          class Allocator = CompAllocator,
          class Behavior  = JitHashTableBehavior>
class JitHashSet
{
    struct Node
    {
        Node* next;
        Value value;

        Node(Node* next, Value value) : next(next), value(value)
        {
        }

        void* operator new(size_t sz, Allocator alloc)
        {
            return alloc.template allocate<uint8_t>(sz);
        }

        void operator delete(void* p, Allocator alloc)
        {
            alloc.deallocate(p);
        }
    };

    Allocator    m_alloc;
    Node**       m_buckets{nullptr};
    JitPrimeInfo m_sizeInfo;
    unsigned     m_count{0};
    unsigned     m_maxCount{0};

public:
    JitHashSet(Allocator alloc) : m_alloc(alloc)
    {
    }

    ~JitHashSet()
    {
        for (unsigned i = 0, count = GetBucketCount(); i < count; i++)
        {
            for (Node *node = m_buckets[i], *next; node != nullptr; node = next)
            {
                next = node->next;
                Node::operator delete(node, m_alloc);
            }
        }

        m_alloc.deallocate(m_buckets);
        m_buckets = nullptr;
    }

    bool Add(Value value)
    {
        if (m_count == m_maxCount)
        {
            Grow();
        }

        unsigned index = GetBucketIndex(value);
        Node*    node  = m_buckets[index];

        while ((node != nullptr) && !HashFuncs::Equals(value, node->value))
        {
            node = node->next;
        }

        if (node != nullptr)
        {
            return false;
        }

        m_buckets[index] = new (m_alloc) Node(m_buckets[index], value);
        m_count++;

        return true;
    }

    bool Contains(Value value) const
    {
        if (m_count == 0)
        {
            return false;
        }

        unsigned index = GetBucketIndex(value);
        Node*    node  = m_buckets[index];

        while ((node != nullptr) && !HashFuncs::Equals(value, node->value))
        {
            node = node->next;
        }

        return node != nullptr;
    }

    bool Remove(Value value)
    {
        if (m_count == 0)
        {
            return false;
        }

        unsigned index    = GetBucketIndex(value);
        Node*    node     = m_buckets[index];
        Node**   nodeLink = &m_buckets[index];

        while ((node != nullptr) && !HashFuncs::Equals(value, node->value))
        {
            nodeLink = &node->next;
            node     = node->next;
        }

        if (node == nullptr)
        {
            return false;
        }

        *nodeLink = node->next;
        m_count--;
        Node::operator delete(node, m_alloc);

        return true;
    }

    void Clear()
    {
        for (unsigned i = 0, count = GetBucketCount(); i < count; i++)
        {
            for (Node *node = m_buckets[i], *next; node != nullptr; node = next)
            {
                next = node->next;
                Node::operator delete(node, m_alloc);
            }
        }

        m_alloc.deallocate(m_buckets);

        m_buckets  = nullptr;
        m_sizeInfo = {};
        m_count    = 0;
        m_maxCount = 0;
    }

    unsigned GetCount() const
    {
        return m_count;
    }

    Allocator GetAllocator()
    {
        return m_alloc;
    }

    class iterator
    {
        friend class JitHashSet;

        Node*  m_node = nullptr;
        Node** m_buckets;
        Node** m_bucketsEnd;

        iterator() = default;

        iterator(const JitHashSet* hash)
            : m_buckets(hash->m_buckets), m_bucketsEnd(hash->m_buckets + hash->GetBucketCount())
        {
            if (hash->m_count > 0)
            {
                FindNextBucket();
            }
        }

        void FindNextBucket()
        {
            while ((m_buckets < m_bucketsEnd) && ((m_node = *m_buckets++) == nullptr))
            {
            }
        }

    public:
        bool operator==(const iterator& i) const
        {
            return i.m_node == m_node;
        }

        bool operator!=(const iterator& i) const
        {
            return i.m_node != m_node;
        }

        void operator++()
        {
            m_node = m_node->next;

            if (m_node == nullptr)
            {
                FindNextBucket();
            }
        }

        const Value& operator*() const
        {
            return m_node->value;
        }
    };

    iterator begin() const
    {
        return iterator(this);
    }

    iterator end() const
    {
        return iterator();
    }

private:
    unsigned GetBucketCount() const
    {
        return m_sizeInfo.GetPrime();
    }

    unsigned GetBucketIndex(Value value) const
    {
        return m_sizeInfo.Remainder(HashFuncs::GetHashCode(value));
    }

    void Grow()
    {
        unsigned newCount = m_count * Behavior::s_growth_factor_numerator / Behavior::s_growth_factor_denominator *
                            Behavior::s_density_factor_denominator / Behavior::s_density_factor_numerator;

        if (newCount < Behavior::s_minimum_allocation)
        {
            newCount = Behavior::s_minimum_allocation;
        }

        if (newCount < m_count)
        {
            Behavior::NoMemory();
        }

        Reallocate(newCount);
    }

    void Reallocate(unsigned newCount)
    {
        assert(newCount >= m_count * Behavior::s_density_factor_denominator / Behavior::s_density_factor_numerator);

        JitPrimeInfo newPrimeInfo   = NextPrime(newCount);
        unsigned     newBucketCount = newPrimeInfo.GetPrime();

        Node** newBuckets = m_alloc.template allocate<Node*>(newBucketCount);

        for (unsigned i = 0; i < newBucketCount; i++)
        {
            newBuckets[i] = nullptr;
        }

        for (unsigned i = 0, count = GetBucketCount(); i < count; i++)
        {
            for (Node *node = m_buckets[i], *next; node != nullptr; node = next)
            {
                next = node->next;

                unsigned newIndex    = newPrimeInfo.Remainder(HashFuncs::GetHashCode(node->value));
                node->next           = newBuckets[newIndex];
                newBuckets[newIndex] = node;
            }
        }

        if (m_buckets != nullptr)
        {
            m_alloc.deallocate(m_buckets);
        }

        m_buckets  = newBuckets;
        m_sizeInfo = newPrimeInfo;
        m_maxCount = newBucketCount * Behavior::s_density_factor_numerator / Behavior::s_density_factor_denominator;
    }

    static const JitPrimeInfo& NextPrime(unsigned number)
    {
        for (const JitPrimeInfo& info : jitPrimeInfo)
        {
            if (info.GetPrime() >= number)
            {
                return info;
            }
        }

        Behavior::NoMemory();
    }
};

template <>
struct JitHashFuncs<int32_t>
{
    static bool Equals(int32_t x, int32_t y)
    {
        return x == y;
    }

    static unsigned GetHashCode(int32_t value)
    {
        return static_cast<unsigned>(value);
    };
};

template <>
struct JitHashFuncs<uint32_t>
{
    static bool Equals(uint32_t x, uint32_t y)
    {
        return x == y;
    }

    static unsigned GetHashCode(uint32_t value)
    {
        return value;
    };
};

template <>
struct JitHashFuncs<uint64_t>
{
    static bool Equals(uint64_t x, uint64_t y)
    {
        return x == y;
    }

    static unsigned GetHashCode(uint64_t value)
    {
        uint32_t upper32 = static_cast<uint32_t>(value >> 32);
        uint32_t lower32 = static_cast<uint32_t>(value & UINT32_MAX);

        return static_cast<unsigned>(upper32 ^ lower32);
    };
};

template <>
struct JitHashFuncs<int64_t>
{
    static bool Equals(int64_t x, int64_t y)
    {
        return x == y;
    }

    static unsigned GetHashCode(int64_t value)
    {
        uint32_t upper32 = static_cast<uint32_t>((value & UINT32_MAX) >> 32);
        uint32_t lower32 = static_cast<uint32_t>(value & UINT32_MAX);

        return static_cast<unsigned>(upper32 ^ lower32);
    };
};

template <class T>
struct JitHashFuncs<T*>
{
    static bool Equals(T* x, T* y)
    {
        return x == y;
    }

    static unsigned GetHashCode(T* value)
    {
        // Using the lower 32 bits of a pointer as a hash code should be good enough.
        // In fact, this should result in an unique hash code unless we allocate
        // more than 4 gigabytes or if the virtual address space is fragmented.
        return static_cast<unsigned>(reinterpret_cast<uintptr_t>(value));
    };
};
