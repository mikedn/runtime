// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "sideeffects.h"

// Adds the given lclNum to the AliasLclSet.
void AliasLclSet::Add(Compiler* compiler, LclVarDsc* lcl)
{
    if (!m_hasAnyLcl)
    {
        m_lcl       = lcl;
        m_hasAnyLcl = true;
    }
    else
    {
        if (!m_hasBitVector)
        {
            m_hasBitVector = true;
            m_bitVector    = hashBv::Create(compiler);
        }

        m_bitVector->setBit(lcl->GetLclNum());
    }
}

// Returns true if this AliasLclSet intersects with the given AliasLclSet.
bool AliasLclSet::Intersects(const AliasLclSet& other) const
{
    // If neither set has ever contained anything, the sets do not intersect.
    if (!m_hasAnyLcl || !other.m_hasAnyLcl)
    {
        return false;
    }

    // If this set is not represented by a bit vector, see if the single lclNum is contained in the other set.
    if (!m_hasBitVector)
    {
        if (!other.m_hasBitVector)
        {
            return m_lcl == other.m_lcl;
        }

        return other.m_bitVector->testBit(m_lcl->GetLclNum());
    }

    // If this set is represented by a bit vector but the other set is not, see if the single lclNum in the other
    // set is contained in this set.
    if (!other.m_hasBitVector)
    {
        return m_bitVector->testBit(other.m_lcl->GetLclNum());
    }

    // Both sets are represented by bit vectors. Check to see if they intersect.
    return m_bitVector->Intersects(other.m_bitVector);
}

// Returns true if this AliasLclSet contains the given local.
bool AliasLclSet::Contains(LclVarDsc* lcl) const
{
    // If this set has never contained anything, it does not contain the lclNum.
    if (!m_hasAnyLcl)
    {
        return false;
    }

    // If this set is not represented by a bit vector, see if its single lclNum is the same as the given lclNum.
    if (!m_hasBitVector)
    {
        return m_lcl == lcl;
    }

    // This set is represented by a bit vector. See if the bit vector contains the given lclNum.
    return m_bitVector->testBit(lcl->GetLclNum());
}

void AliasLclSet::Clear()
{
    if (m_hasBitVector)
    {
        assert(m_hasAnyLcl);
        m_bitVector->ZeroAll();
    }
    else if (m_hasAnyLcl)
    {
        m_hasAnyLcl = false;
    }
}

// Computes the alias info for a given node. Note that this does not
// include the set of local accesses for a node unless the node is
// itself a local access (e.g. a GT_LCL_VAR, GT_STORE_LCL_VAR, etc.).
AliasSet::NodeInfo::NodeInfo(GenTree* node) : m_node(node)
{
    if (node->IsCall())
    {
        m_flags = node->AsCall()->IsPure() ? ALIAS_NONE
                                           : (ALIAS_LOADS_ADDRESSABLE_LOCATION | ALIAS_STORES_ADDRESSABLE_LOCATION);

        return;
    }

    if (node->OperIsAtomicOp())
    {
        m_flags = ALIAS_LOADS_ADDRESSABLE_LOCATION | ALIAS_STORES_ADDRESSABLE_LOCATION;

        return;
    }

    // `node` is the location being accessed. Determine whether or not it is a memory or local variable access, and if
    // it is the latter, get the number of the local.
    bool       isMemoryAccess = false;
    bool       isLclVarAccess = false;
    LclVarDsc* lcl            = nullptr;

    if (GenTreeIndir* indir = node->IsIndir())
    {
        // If the indirection targets a local, we can be more precise with regards to aliasing by treating the
        // indirection as a local access.
        if (GenTreeLclAddr* lclAddr = indir->GetAddr()->IsLclAddr())
        {
            isLclVarAccess = true;
            lcl            = lclAddr->GetLcl();
        }
        else
        {
            isMemoryAccess = true;
        }
    }
    else if (node->OperIs(GT_COPY_BLK, GT_INIT_BLK))
    {
        // TODO-MIKE-Review: We could probably handle this like a normal IND as far as locals
        // are concerned (though we'd need to handle both load and store for COPY_BLK).
        isMemoryAccess = true;
    }
#ifdef FEATURE_HW_INTRINSICS
    else if (node->IsHWIntrinsic() && node->AsHWIntrinsic()->OperIsMemoryLoadOrStore())
    {
        isMemoryAccess = true;
    }
#endif
    else if (node->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        isLclVarAccess = true;
        lcl            = node->AsLclVarCommon()->GetLcl();
    }
    else
    {
        // This is neither a memory nor a local var access.
        m_flags = ALIAS_NONE;

        return;
    }

    assert(isMemoryAccess || isLclVarAccess);

    // Now that we've determined whether or not this access is a load or a store and whether the accessed location is
    // memory or a local, determine whther or not the location is addressable and update the alias set.
    const bool isAddressableLocation = isMemoryAccess || lcl->IsAddressExposed();

    // TODO-MIKE-Review: Is this missing HWINTRINSIC stores?
    if (!node->OperIsStore())
    {
        if (isAddressableLocation)
        {
            m_flags |= ALIAS_LOADS_ADDRESSABLE_LOCATION;
        }

        if (isLclVarAccess)
        {
            m_flags |= ALIAS_LOADS_LCL;
            m_lcl = lcl;
        }
    }
    else
    {
        if (isAddressableLocation)
        {
            m_flags |= ALIAS_STORES_ADDRESSABLE_LOCATION;
        }

        if (isLclVarAccess)
        {
            m_flags |= ALIAS_STORES_LCL;
            m_lcl = lcl;
        }
    }
}

// Adds the given node's accesses to this AliasSet.
void AliasSet::AddNode(Compiler* compiler, GenTree* node)
{
    // First, add all local uses associated with the node to the set. This is necessary because the
    // local loads occur at the position of the user, not at the position of the GenTreeLclVar node.
    node->VisitOperands([compiler, this](GenTree* operand) -> GenTree::VisitResult {
        if (operand->OperIs(GT_LCL_VAR, GT_LCL_FLD))
        {
            LclVarDsc* lcl = operand->AsLclVarCommon()->GetLcl();

            if (lcl->IsAddressExposed())
            {
                m_loadsAddressableLocation = true;
            }

            m_lclLoads.Add(compiler, lcl);
        }

        if (!operand->OperIs(GT_ARGPLACE) && operand->isContained())
        {
            AddNode(compiler, operand);
        }

        return GenTree::VisitResult::Continue;
    });

    NodeInfo nodeInfo(node);

    if (nodeInfo.LoadsAddressableLocation())
    {
        m_loadsAddressableLocation = true;
    }

    if (nodeInfo.StoresAddressableLocation())
    {
        m_storesAddressableLocation = true;
    }

    if (nodeInfo.IsLclLoad())
    {
        m_lclLoads.Add(compiler, nodeInfo.Lcl());
    }

    if (nodeInfo.IsLclStore())
    {
        m_lclStores.Add(compiler, nodeInfo.Lcl());
    }
}

// Returns true if the loads and stores in this alias set interfere
// with the given alias set.
//
// Two alias sets interfere under any of the following conditions:
//   - Both sets stores to any addressable location (e.g. the heap,
//     address-exposed locals)
//   - One set loads any addressable location and the other set stores
//     to any addressable location
//   - Both sets store to the same local
//   - One set stores to a local that is loaded by the other set
bool AliasSet::InterferesWith(const AliasSet& other) const
{
    // If both sets store to any addressable location, the sets interfere.
    if (m_storesAddressableLocation && other.m_storesAddressableLocation)
    {
        return true;
    }

    // If one set stores to any addressable location and the other loads from any addressable location,
    // the sets interfere.
    if ((m_loadsAddressableLocation && other.m_storesAddressableLocation) ||
        (m_storesAddressableLocation && other.m_loadsAddressableLocation))
    {
        return true;
    }

    // If the set of lclVars written by this alias set intersects with the set of lclVars accessed by the other alias
    // set, the alias sets interfere.
    if (m_lclStores.Intersects(other.m_lclLoads) || m_lclStores.Intersects(other.m_lclStores))
    {
        return true;
    }

    // If the set of locals loaded by this alias set intersects with the set of locals stored to
    // by the other alias set, the alias sets interfere. Otherwise, the alias sets do not interfere.
    return m_lclLoads.Intersects(other.m_lclStores);
}

// Returns true if the loads and stores in this alias set interfere
// with those for the given node.
// An alias set interferes with a given node iff it interferes with the
// alias set for that node.
bool AliasSet::InterferesWith(const NodeInfo& other) const
{
    // First check whether or not this set interferes with the local uses associated with the given node.
    if (m_storesAddressableLocation || !m_lclStores.IsEmpty())
    {
        for (GenTree* operand : other.Node()->Operands())
        {
            if (operand->OperIs(GT_LCL_VAR, GT_LCL_FLD))
            {
                // If this set stores any addressable location and the node uses an address-exposed local,
                // the set interferes with the node.
                LclVarDsc* lcl = operand->AsLclVarCommon()->GetLcl();

                if (lcl->IsAddressExposed() && m_storesAddressableLocation)
                {
                    return true;
                }

                // If this set stores to a local used by the node, the set interferes with the node.
                if (m_lclStores.Contains(lcl))
                {
                    return true;
                }
            }
        }
    }

    // If the node and the set both store to any addressable location, they interfere.
    if (m_storesAddressableLocation && other.StoresAddressableLocation())
    {
        return true;
    }

    // If the node or the set store to any addressable location and the other loads any addressable location,
    // they interfere.
    if ((m_loadsAddressableLocation && other.StoresAddressableLocation()) ||
        (m_storesAddressableLocation && other.LoadsAddressableLocation()))
    {
        return true;
    }

    // If the set stores to a local accessed by the node, they interfere.
    if ((other.IsLclLoad() || other.IsLclStore()) && m_lclStores.Contains(other.Lcl()))
    {
        return true;
    }

    // If the set loads a local stored to by the node, they interfere.
    return other.IsLclStore() && m_lclLoads.Contains(other.Lcl());
}

void AliasSet::Clear()
{
    m_loadsAddressableLocation  = false;
    m_storesAddressableLocation = false;

    m_lclLoads.Clear();
    m_lclStores.Clear();
}

// Adds the given node's accesses to this SideEffectSet.
void SideEffectSet::AddNode(Compiler* compiler, GenTree* node)
{
    m_sideEffectFlags |= (node->gtFlags & GTF_ALL_EFFECT);
    m_aliasSet.AddNode(compiler, node);
}

// Returns true if the side effects in this set interfere with the
// given side effect flags and alias information.
//
// Two side effect sets interfere under any of the following conditions:
// - If the analysis is strict, and:
//     - One set contains a compiler barrier and the other set contains a global reference, or
//     - Both sets produce an exception
// - Whether or not the analysis is strict:
//     - One set produces an exception and the other set contains a store
//     - One set's loads and stores interfere with the other set's loads and stores
template <typename TOtherAliasInfo>
bool SideEffectSet::InterferesWith(unsigned               otherSideEffectFlags,
                                   const TOtherAliasInfo& otherAliasInfo,
                                   bool                   strict) const
{
    const bool thisProducesException  = (m_sideEffectFlags & GTF_EXCEPT) != 0;
    const bool otherProducesException = (otherSideEffectFlags & GTF_EXCEPT) != 0;

    if (strict)
    {
        // If either set contains a compiler barrier, and the other set contains a global reference,
        // the sets interfere.
        if (((m_sideEffectFlags & GTF_ORDER_SIDEEFF) != 0) && ((otherSideEffectFlags & GTF_GLOB_REF) != 0))
        {
            return true;
        }

        if (((otherSideEffectFlags & GTF_ORDER_SIDEEFF) != 0) && ((m_sideEffectFlags & GTF_GLOB_REF) != 0))
        {
            return true;
        }

        // If both sets produce an exception, the sets interfere.
        if (thisProducesException && otherProducesException)
        {
            return true;
        }
    }

    // If one set produces an exception and the other set stores to any location, the sets interfere.
    if ((thisProducesException && otherAliasInfo.StoresAnyLocation()) ||
        (otherProducesException && m_aliasSet.StoresAnyLocation()))
    {
        return true;
    }

    // At this point, the only interference between the sets will arise from their alias sets.
    return m_aliasSet.InterferesWith(otherAliasInfo);
}

// Returns true if the side effects in this set interfere with the side
// effects in the given side effect set.
//
// Two side effect sets interfere under any of the following conditions:
// - If the analysis is strict, and:
//     - Either set contains a compiler barrier, or
//     - Both sets produce an exception
// - Whether or not the analysis is strict:
//     - One set produces an exception and the other set contains a store
//     - One set's loads and stores interfere with the other set's loads and stores
bool SideEffectSet::InterferesWith(const SideEffectSet& other, bool strict) const
{
    return InterferesWith(other.m_sideEffectFlags, other.m_aliasSet, strict);
}

// Returns true if the side effects in this set interfere with the side
// effects for the given node.
//
// A side effect set interferes with a given node iff it interferes
// with the side effect set of the node.
bool SideEffectSet::InterferesWith(Compiler* compiler, GenTree* node, bool strict) const
{
    return InterferesWith(node->GetSideEffects(), AliasSet::NodeInfo(node), strict);
}

void SideEffectSet::Clear()
{
    m_sideEffectFlags = 0;
    m_aliasSet.Clear();
}
