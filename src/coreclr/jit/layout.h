// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef LAYOUT_H
#define LAYOUT_H

#include "jit.h"

enum class VectorKind : uint8_t
{
    None,
#ifdef FEATURE_SIMD
    Vector234,
    VectorT,
#ifdef FEATURE_HW_INTRINSICS
    VectorNT,
#endif
#endif
};

// Encapsulates layout information about a class (typically a value class but this can also be
// be used for reference classes when they are stack allocated). The class handle is optional,
// allowing the creation of "block" layout objects having a specific size but lacking any other
// layout information. The JIT uses such layout objects in cases where a class handle is not
// available (cpblk/initblk operations) or not necessary (classes that do not contain GC pointers).
class ClassLayout
{
    // Class handle or NO_CLASS_HANDLE for "block" layouts.
    const CORINFO_CLASS_HANDLE m_classHandle;

    // Size of the layout in bytes (as reported by ICorJitInfo::getClassSize/getHeapClassSize
    // for non "block" layouts). For "block" layouts this may be 0 due to 0 being a valid size
    // for cpblk/initblk.
    unsigned m_size;

    unsigned m_isValueClass : 1;
    // The number of GC pointers in this layout. Since the the maximum size is 2^32-1 the count
    // can fit in at most 30 bits.
    unsigned m_gcPtrCount : 30;

    struct LayoutInfo
    {
        VectorKind vectorKind : 2;
        bool       elementTypeIsNInt : 1;
        var_types  simdType;
        var_types  elementType;
        var_types  hfaElementType;
    };

    union {
        // Array of CorInfoGCType (as BYTE) that describes the GC layout of the class.
        // For small classes the array is stored inline, avoiding an extra allocation
        // and the pointer size overhead.
        BYTE* m_gcPtrs;
        BYTE  m_gcPtrsArray[sizeof(BYTE*)];

        // Layout information for vector and HFA structs. Valid when m_gcPtrCount is 0.
        LayoutInfo m_layoutInfo;
    };

#ifdef UNIX_AMD64_ABI
    struct SysVAmd64AbiInfo
    {
        bool      initialized : 1;
        uint8_t   regCount : 7;
        var_types regTypes[2];

        SysVAmd64AbiInfo() : initialized{false}, regCount{0}, regTypes{TYP_UNDEF, TYP_UNDEF}
        {
        }
    };

    SysVAmd64AbiInfo m_sysVAmd64AbiInfo;
#endif

    // Class name as reported by ICorJitInfo::getClassName
    INDEBUG(const char* m_className;)

    // ClassLayout instances should only be obtained via ClassLayoutTable.
    friend class ClassLayoutTable;

    ClassLayout(unsigned size)
        : m_classHandle(NO_CLASS_HANDLE)
        , m_size(size)
        , m_isValueClass(false)
        , m_gcPtrCount(0)
        , m_gcPtrs(nullptr)
#ifdef DEBUG
        , m_className("block")
#endif
    {
    }

    ClassLayout(CORINFO_CLASS_HANDLE classHandle, Compiler* compiler);

    ClassLayout(CORINFO_CLASS_HANDLE classHandle, bool isValueClass, unsigned size DEBUGARG(const char* className))
        : m_classHandle(classHandle)
        , m_size(size)
        , m_isValueClass(isValueClass)
        , m_gcPtrCount(0)
        , m_gcPtrs(nullptr)
#ifdef DEBUG
        , m_className(className)
#endif
    {
        assert(size != 0);
    }

public:
    void EnsureHfaInfo(Compiler* compiler);
    void EnsureSysVAmd64AbiInfo(Compiler* compiler);

    CORINFO_CLASS_HANDLE GetClassHandle() const
    {
        return m_classHandle;
    }

    bool IsBlockLayout() const
    {
        return m_classHandle == NO_CLASS_HANDLE;
    }

#ifdef DEBUG
    const char* GetClassName() const
    {
        return m_className;
    }
#endif

    bool IsValueClass() const
    {
        assert(!IsBlockLayout());

        return m_isValueClass;
    }

    unsigned GetSize() const
    {
        return m_size;
    }

    bool IsHfa() const
    {
#ifdef FEATURE_HFA
        if (m_gcPtrCount != 0)
        {
            return false;
        }

        assert(m_layoutInfo.hfaElementType != TYP_UNDEF);
        return m_layoutInfo.hfaElementType != TYP_VOID;
#else
        return false;
#endif
    }

    var_types GetHfaElementType() const
    {
        assert(IsHfa());
        return m_layoutInfo.hfaElementType;
    }

    uint8_t GetHfaElementCount() const
    {
        assert(IsHfa());
        return static_cast<uint8_t>(m_size / varTypeSize(m_layoutInfo.hfaElementType));
    }

    unsigned GetHfaRegCount() const
    {
        unsigned count = GetHfaElementCount();
#ifdef TARGET_ARM
        if (m_layoutInfo.hfaElementType == TYP_DOUBLE)
        {
            // On ARM32 each double HFA element requires 2 float registers.
            count *= 2;
        }
#endif
        return count;
    }

    uint8_t GetSysVAmd64AbiRegCount() const
    {
#ifdef UNIX_AMD64_ABI
        assert(m_sysVAmd64AbiInfo.initialized);
        return m_sysVAmd64AbiInfo.regCount;
#else
        return 0;
#endif
    }

    var_types GetSysVAmd64AbiRegType(unsigned i) const
    {
#ifdef UNIX_AMD64_ABI
        assert(m_sysVAmd64AbiInfo.initialized);
        assert(i < m_sysVAmd64AbiInfo.regCount);
        return m_sysVAmd64AbiInfo.regTypes[i];
#else
        return TYP_UNDEF;
#endif
    }

    bool IsVector() const
    {
#ifdef FEATURE_SIMD
        return (m_gcPtrCount == 0) && (m_layoutInfo.vectorKind != VectorKind::None);
#else
        return false;
#endif
    }

    bool IsOpaqueVector() const
    {
#ifdef FEATURE_SIMD
        return (m_gcPtrCount == 0) && (m_layoutInfo.vectorKind >= VectorKind::VectorT);
#else
        return false;
#endif
    }

    VectorKind GetVectorKind() const
    {
#ifdef FEATURE_SIMD
        return (m_gcPtrCount == 0) ? m_layoutInfo.vectorKind : VectorKind::None;
#else
        return VectorKind::None;
#endif
    }

    bool ElementTypeIsNInt() const
    {
#ifdef FEATURE_SIMD
        return (m_gcPtrCount == 0) ? m_layoutInfo.elementTypeIsNInt : false;
#else
        return false;
#endif
    }

    var_types GetSIMDType() const
    {
#ifdef FEATURE_SIMD
        return (m_gcPtrCount > 0) ? TYP_UNDEF : m_layoutInfo.simdType;
#else
        return TYP_UNDEF;
#endif
    }

    var_types GetElementType() const
    {
#ifdef FEATURE_SIMD
        return (m_gcPtrCount > 0) ? TYP_UNDEF : m_layoutInfo.elementType;
#else
        return TYP_UNDEF;
#endif
    }

    unsigned GetElementCount() const
    {
#ifdef FEATURE_SIMD
        return ((m_gcPtrCount > 0) || (m_layoutInfo.vectorKind == VectorKind::None))
                   ? 0
                   : (m_size / varTypeSize(m_layoutInfo.elementType));
#else
        return 0;
#endif
    }

    //------------------------------------------------------------------------
    // GetRegisterType: Determine register type for the layout.
    //
    // Return Value:
    //    TYP_UNDEF if the layout is enregistrable, register type otherwise.
    //
    var_types GetRegisterType() const
    {
        if (HasGCPtr())
        {
            return (GetSlotCount() == 1) ? GetGCPtrType(0) : TYP_UNDEF;
        }

        switch (m_size)
        {
            case 1:
                return TYP_UBYTE;
            case 2:
                return TYP_USHORT;
            case 4:
                return TYP_INT;
#ifdef TARGET_64BIT
            case 8:
                return TYP_LONG;
#endif
#ifdef FEATURE_SIMD
            // TODO: check TYP_SIMD12 profitability,
            // it will need additional support in `BuildStoreLoc`.
            case 16:
                return TYP_SIMD16;
#endif
            default:
                return TYP_UNDEF;
        }
    }

    unsigned GetSlotCount() const
    {
        return roundUp(m_size, TARGET_POINTER_SIZE) / TARGET_POINTER_SIZE;
    }

    unsigned GetGCPtrCount() const
    {
        return m_gcPtrCount;
    }

    bool HasGCPtr() const
    {
        return m_gcPtrCount != 0;
    }

    bool HasGCRef() const
    {
        if (m_gcPtrCount > 0)
        {
            // TODO-MIKE-Cleanup: It may be good to not have to loop through all slots
            // to get this information. But it's good enough for now as only multireg
            // stores need it and those only deal with a very small number of slots.
            for (unsigned i = 0, count = GetSlotCount(); i < count; i++)
            {
                if (GetGCPtr(i) == TYPE_GC_REF)
                {
                    return true;
                }
            }
        }

        return false;
    }

    bool IsGCPtr(unsigned slot) const
    {
        return GetGCPtr(slot) != TYPE_GC_NONE;
    }

    bool IsGCRef(unsigned slot) const
    {
        return GetGCPtr(slot) == TYPE_GC_REF;
    }

    var_types GetGCPtrType(unsigned slot) const
    {
        switch (GetGCPtr(slot))
        {
            case TYPE_GC_NONE:
                return TYP_I_IMPL;
            case TYPE_GC_REF:
                return TYP_REF;
            case TYPE_GC_BYREF:
                return TYP_BYREF;
            default:
                unreached();
        }
    }

    static bool AreCompatible(const ClassLayout* layout1, const ClassLayout* layout2);

private:
    CorInfoGCType GetGCPtr(unsigned slot) const
    {
        assert(!IsBlockLayout());
        assert(slot < GetSlotCount());

        if (m_gcPtrCount == 0)
        {
            return TYPE_GC_NONE;
        }

        const BYTE* gcPtrs = GetSlotCount() > sizeof(m_gcPtrsArray) ? m_gcPtrs : m_gcPtrsArray;
        return static_cast<CorInfoGCType>(gcPtrs[slot]);
    }

#ifdef FEATURE_SIMD
    static LayoutInfo GetVectorLayoutInfo(CORINFO_CLASS_HANDLE classHandle, Compiler* compiler);
#endif

#ifdef UNIX_AMD64_ABI
    static var_types GetEightbyteType(const SYSTEMV_AMD64_CORINFO_STRUCT_REG_PASSING_DESCRIPTOR& desc, unsigned i);
#endif
};

#endif // LAYOUT_H
