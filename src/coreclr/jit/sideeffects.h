// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

// AliasLclSet represents a set of lclVars. Optimized for the case that the set
// never holds more than a single element. This type is used internally
// by `AliasSet` to track the sets of lclVars that are read and
// written for a given alias set.
class AliasLclSet final
{
    union {
        hashBv*    m_bitVector = nullptr;
        LclVarDsc* m_lcl;
    };

    bool m_hasAnyLcl    = false;
    bool m_hasBitVector = false;

public:
    bool IsEmpty() const
    {
        assert(!m_hasBitVector || m_bitVector->anySet());
        return !m_hasAnyLcl && !m_hasBitVector;
    }

    void Add(Compiler* compiler, LclVarDsc* lcl);
    bool Intersects(const AliasLclSet& other) const;
    bool Contains(LclVarDsc* lcl) const;
    void Clear();
};

// AliasSet represents a set of reads and writes for the purposes of alias
// analysis. This type partitions storage into two categories:
// locals and addressable locations. The definition of the former is
// intuitive. The latter is the union of the set of address-exposed
// lclVars with the set of all other memory locations. Any memory
// access is assumed to alias any other memory access.
class AliasSet final
{
    AliasLclSet m_lclLoads;
    AliasLclSet m_lclStores;

    bool m_loadsAddressableLocation  = false;
    bool m_storesAddressableLocation = false;

public:
    // NodeInfo represents basic alias information for a single IR node.
    class NodeInfo final
    {
        enum : unsigned
        {
            ALIAS_NONE                        = 0x0,
            ALIAS_LOADS_ADDRESSABLE_LOCATION  = 0x1,
            ALIAS_STORES_ADDRESSABLE_LOCATION = 0x2,
            ALIAS_LOADS_LCL                   = 0x4,
            ALIAS_STORES_LCL                  = 0x8
        };

        GenTree*   m_node;
        LclVarDsc* m_lcl   = nullptr;
        unsigned   m_flags = ALIAS_NONE;

    public:
        NodeInfo(GenTree* node);

        GenTree* Node() const
        {
            return m_node;
        }

        bool LoadsAddressableLocation() const
        {
            return (m_flags & ALIAS_LOADS_ADDRESSABLE_LOCATION) != 0;
        }

        bool StoresAddressableLocation() const
        {
            return (m_flags & ALIAS_STORES_ADDRESSABLE_LOCATION) != 0;
        }

        bool IsLclLoad() const
        {
            return (m_flags & ALIAS_LOADS_LCL) != 0;
        }

        bool IsLclStore() const
        {
            return (m_flags & ALIAS_STORES_LCL) != 0;
        }

        LclVarDsc* Lcl() const
        {
            assert(IsLclLoad() || IsLclStore());
            return m_lcl;
        }

        bool StoresAnyLocation() const
        {
            return (m_flags & (ALIAS_STORES_ADDRESSABLE_LOCATION | ALIAS_STORES_LCL)) != 0;
        }
    };

    bool StoresAnyLocation() const
    {
        return m_storesAddressableLocation || !m_lclStores.IsEmpty();
    }

    void AddNode(Compiler* compiler, GenTree* node);
    bool InterferesWith(const AliasSet& other) const;
    bool InterferesWith(const NodeInfo& node) const;
    void Clear();
};

// SideEffectSet represents a set of side effects for the purposes of analyzing
// code motion.
// Note that for non-fixed-size frames without a frame pointer (currently
// x86-only), we don't track the modification of the stack level that occurs
// with a GT_PUTARG_STK as a side-effect. If we ever support general code
// reordering, that would have to be taken into account. As it happens,
// we currently do not reorder any other side-effecting nodes relative to
// these.
class SideEffectSet final
{
    unsigned m_sideEffectFlags = 0; // A mask of GTF_* flags that represents exceptional and barrier side effects.
    AliasSet m_aliasSet;            // An AliasSet that represents read and write side effects.

    template <typename TOtherAliasInfo>
    bool InterferesWith(unsigned otherSideEffectFlags, const TOtherAliasInfo& otherAliasInfo, bool strict) const;

public:
    void AddNode(Compiler* compiler, GenTree* node);
    bool InterferesWith(const SideEffectSet& other, bool strict) const;
    bool InterferesWith(Compiler* comiler, GenTree* node, bool strict) const;
    void Clear();
};
