#include "jitpch.h"
#include "layout.h"
#include "compiler.h"

// Keeps track of layout objects associated to class handles or block sizes. A layout is usually
// referenced by a pointer (ClassLayout*) but can also be referenced by a number (unsigned,
// FirstLayoutNum-based), when space constraints or other needs make numbers more appealing.
// Layout objects are immutable and there's always a 1:1 mapping between class handles/block sizes,
// pointers and numbers (e.g. class handle equality implies ClassLayout pointer equality).
class ClassLayoutTable
{
    // Each layout is assigned a number, starting with TYP_UNKNOWN + 1. This way one could use a single
    // unsigned value to represent the notion of type - values below TYP_UNKNOWN are var_types and values
    // above it are struct layouts.
    static constexpr unsigned FirstLayoutNum = TYP_UNKNOWN + 1;

    typedef JitHashTable<unsigned, JitSmallPrimitiveKeyFuncs<unsigned>, unsigned>               BlkLayoutIndexMap;
    typedef JitHashTable<CORINFO_CLASS_HANDLE, JitPtrKeyFuncs<CORINFO_CLASS_STRUCT_>, unsigned> ObjLayoutIndexMap;

    union {
        // Up to 3 layouts can be stored "inline" and finding a layout by handle/size can be done using linear search.
        // Most methods need no more than 2 layouts.
        ClassLayout* m_layoutArray[3];
        // Otherwise a dynamic array is allocated and hashtables are used to map from handle/size to layout array index.
        struct
        {
            ClassLayout**      m_layoutLargeArray;
            BlkLayoutIndexMap* m_blkLayoutMap;
            ObjLayoutIndexMap* m_objLayoutMap;
        };
    };
    // The number of layout objects stored in this table.
    unsigned m_layoutCount;
    // The capacity of m_layoutLargeArray (when more than 3 layouts are stored).
    unsigned m_layoutLargeCapacity;

public:
    ClassLayoutTable() : m_layoutCount(0), m_layoutLargeCapacity(0)
    {
    }

    // Get the layout number (FirstLayoutNum-based) of the specified layout.
    unsigned GetLayoutNum(ClassLayout* layout) const
    {
        return GetLayoutIndex(layout) + FirstLayoutNum;
    }

    static bool IsLayoutNum(unsigned layoutNum)
    {
        return layoutNum >= FirstLayoutNum;
    }

    // Get the layout having the specified layout number (FirstLayoutNum-based)
    ClassLayout* GetLayoutByNum(unsigned num) const
    {
        assert(num >= FirstLayoutNum);
        return GetLayoutByIndex(num - FirstLayoutNum);
    }

    // Get the layout having the specified size but no class handle.
    ClassLayout* GetBlkLayout(Compiler* compiler, unsigned blockSize)
    {
        return GetLayoutByIndex(GetBlkLayoutIndex(compiler, blockSize));
    }

    // Get the number of a layout having the specified size but no class handle.
    unsigned GetBlkLayoutNum(Compiler* compiler, unsigned blockSize)
    {
        return GetBlkLayoutIndex(compiler, blockSize) + FirstLayoutNum;
    }

    // Get the layout for the specified class handle.
    ClassLayout* GetObjLayout(Compiler* compiler, CORINFO_CLASS_HANDLE classHandle)
    {
        return GetLayoutByIndex(GetObjLayoutIndex(compiler, classHandle));
    }

    // Get the number of a layout for the specified class handle.
    unsigned GetObjLayoutNum(Compiler* compiler, CORINFO_CLASS_HANDLE classHandle)
    {
        return GetObjLayoutIndex(compiler, classHandle) + FirstLayoutNum;
    }

private:
    bool HasSmallCapacity() const
    {
        return m_layoutCount <= _countof(m_layoutArray);
    }

    ClassLayout* GetLayoutByIndex(unsigned index) const
    {
        assert(index < m_layoutCount);

        if (HasSmallCapacity())
        {
            return m_layoutArray[index];
        }
        else
        {
            return m_layoutLargeArray[index];
        }
    }

    unsigned GetLayoutIndex(ClassLayout* layout) const
    {
        assert(layout != nullptr);

        if (HasSmallCapacity())
        {
            for (unsigned i = 0; i < m_layoutCount; i++)
            {
                if (m_layoutArray[i] == layout)
                {
                    return i;
                }
            }
        }
        else
        {
            unsigned index = 0;
            if ((layout->IsBlockLayout() && m_blkLayoutMap->Lookup(layout->GetSize(), &index)) ||
                m_objLayoutMap->Lookup(layout->GetClassHandle(), &index))
            {
                return index;
            }
        }

        unreached();
    }

    unsigned GetBlkLayoutIndex(Compiler* compiler, unsigned blockSize)
    {
        if (HasSmallCapacity())
        {
            for (unsigned i = 0; i < m_layoutCount; i++)
            {
                if (m_layoutArray[i]->IsBlockLayout() && (m_layoutArray[i]->GetSize() == blockSize))
                {
                    return i;
                }
            }
        }
        else
        {
            unsigned index;
            if (m_blkLayoutMap->Lookup(blockSize, &index))
            {
                return index;
            }
        }

        return AddBlkLayout(compiler, CreateBlkLayout(compiler, blockSize));
    }

    ClassLayout* CreateBlkLayout(Compiler* compiler, unsigned blockSize)
    {
        return new (compiler, CMK_ClassLayout) ClassLayout(blockSize);
    }

    unsigned AddBlkLayout(Compiler* compiler, ClassLayout* layout)
    {
        if (m_layoutCount < _countof(m_layoutArray))
        {
            m_layoutArray[m_layoutCount] = layout;
            return m_layoutCount++;
        }

        unsigned index = AddLayoutLarge(compiler, layout);
        m_blkLayoutMap->Set(layout->GetSize(), index);
        return index;
    }

    unsigned GetObjLayoutIndex(Compiler* compiler, CORINFO_CLASS_HANDLE classHandle)
    {
        assert(classHandle != NO_CLASS_HANDLE);

        if (HasSmallCapacity())
        {
            for (unsigned i = 0; i < m_layoutCount; i++)
            {
                if (m_layoutArray[i]->GetClassHandle() == classHandle)
                {
                    return i;
                }
            }
        }
        else
        {
            unsigned index;
            if (m_objLayoutMap->Lookup(classHandle, &index))
            {
                return index;
            }
        }

        return AddObjLayout(compiler, CreateObjLayout(compiler, classHandle));
    }

    ClassLayout* CreateObjLayout(Compiler* compiler, CORINFO_CLASS_HANDLE classHandle)
    {
        return new (compiler, CMK_ClassLayout) ClassLayout(classHandle, compiler);
    }

    unsigned AddObjLayout(Compiler* compiler, ClassLayout* layout)
    {
        if (m_layoutCount < _countof(m_layoutArray))
        {
            m_layoutArray[m_layoutCount] = layout;
            return m_layoutCount++;
        }

        unsigned index = AddLayoutLarge(compiler, layout);
        m_objLayoutMap->Set(layout->GetClassHandle(), index);
        return index;
    }

    unsigned AddLayoutLarge(Compiler* compiler, ClassLayout* layout)
    {
        if (m_layoutCount >= m_layoutLargeCapacity)
        {
            CompAllocator alloc       = compiler->getAllocator(CMK_ClassLayout);
            unsigned      newCapacity = m_layoutCount * 2;
            ClassLayout** newArray    = alloc.allocate<ClassLayout*>(newCapacity);

            if (m_layoutCount <= _countof(m_layoutArray))
            {
                BlkLayoutIndexMap* blkLayoutMap = new (alloc) BlkLayoutIndexMap(alloc);
                ObjLayoutIndexMap* objLayoutMap = new (alloc) ObjLayoutIndexMap(alloc);

                for (unsigned i = 0; i < m_layoutCount; i++)
                {
                    ClassLayout* l = m_layoutArray[i];
                    newArray[i]    = l;

                    if (l->IsBlockLayout())
                    {
                        blkLayoutMap->Set(l->GetSize(), i);
                    }
                    else
                    {
                        objLayoutMap->Set(l->GetClassHandle(), i);
                    }
                }

                m_blkLayoutMap = blkLayoutMap;
                m_objLayoutMap = objLayoutMap;
            }
            else
            {
                memcpy(newArray, m_layoutLargeArray, m_layoutCount * sizeof(newArray[0]));
            }

            m_layoutLargeArray    = newArray;
            m_layoutLargeCapacity = newCapacity;
        }

        m_layoutLargeArray[m_layoutCount] = layout;
        return m_layoutCount++;
    }
};

ClassLayoutTable* Compiler::typCreateClassLayoutTable()
{
    assert(m_classLayoutTable == nullptr);

    if (compIsForInlining())
    {
        m_classLayoutTable = impInlineInfo->InlinerCompiler->m_classLayoutTable;

        if (m_classLayoutTable == nullptr)
        {
            m_classLayoutTable = new (this, CMK_ClassLayout) ClassLayoutTable();

            impInlineInfo->InlinerCompiler->m_classLayoutTable = m_classLayoutTable;
        }
    }
    else
    {
        m_classLayoutTable = new (this, CMK_ClassLayout) ClassLayoutTable();
    }

    return m_classLayoutTable;
}

ClassLayoutTable* Compiler::typGetClassLayoutTable()
{
    if (m_classLayoutTable == nullptr)
    {
        return typCreateClassLayoutTable();
    }

    return m_classLayoutTable;
}

bool Compiler::typIsLayoutNum(unsigned layoutNum)
{
    return ClassLayoutTable::IsLayoutNum(layoutNum);
}

ClassLayout* Compiler::typGetLayoutByNum(unsigned layoutNum)
{
    return typGetClassLayoutTable()->GetLayoutByNum(layoutNum);
}

unsigned Compiler::typGetLayoutNum(ClassLayout* layout)
{
    return typGetClassLayoutTable()->GetLayoutNum(layout);
}

unsigned Compiler::typGetBlkLayoutNum(unsigned blockSize)
{
    return typGetClassLayoutTable()->GetBlkLayoutNum(this, blockSize);
}

ClassLayout* Compiler::typGetBlkLayout(unsigned blockSize)
{
    return typGetClassLayoutTable()->GetBlkLayout(this, blockSize);
}

unsigned Compiler::typGetObjLayoutNum(CORINFO_CLASS_HANDLE classHandle)
{
    return typGetClassLayoutTable()->GetObjLayoutNum(this, classHandle);
}

ClassLayout* Compiler::typGetObjLayout(CORINFO_CLASS_HANDLE classHandle)
{
    return typGetClassLayoutTable()->GetObjLayout(this, classHandle);
}

var_types Compiler::typGetStructType(CORINFO_CLASS_HANDLE classHandle, var_types* simdBaseType)
{
#ifdef FEATURE_SIMD
    if (supportSIMDTypes())
    {
        ClassLayout* layout = typGetObjLayout(classHandle);
        if (layout->IsVector())
        {
            if (simdBaseType != nullptr)
            {
                *simdBaseType = layout->GetElementType();
            }
            return layout->GetSIMDType();
        }
    }
#endif

    return TYP_STRUCT;
}

var_types Compiler::typGetStructType(ClassLayout* layout, var_types* simdBaseType)
{
#ifdef FEATURE_SIMD
    if (supportSIMDTypes())
    {
        if (layout->IsVector())
        {
            if (simdBaseType != nullptr)
            {
                *simdBaseType = layout->GetElementType();
            }
            return layout->GetSIMDType();
        }
    }
#endif

    return TYP_STRUCT;
}

ClassLayout::ClassLayout(CORINFO_CLASS_HANDLE classHandle, Compiler* compiler)
    : m_classHandle(classHandle)
    , m_size(0)
    , m_isValueClass(true)
    , m_gcPtrCount(0)
    , m_gcPtrs(nullptr)
#ifdef TARGET_AMD64
    , m_pppQuirkLayout(nullptr)
#endif
#ifdef DEBUG
    , m_className(compiler->info.compCompHnd->getClassName(classHandle))
#endif
{
    uint32_t attribs = compiler->info.compCompHnd->getClassAttribs(m_classHandle);

    if ((attribs & CORINFO_FLG_VALUECLASS) != 0)
    {
        // Normally we should not create layout instances for primitive types but the importer
        // manages to do that by importing `initobj T` where T may be a primitive type in
        // generic code. Ideally the importer should convert that to scalar initialization.

        // assert(compiler->info.compCompHnd->getTypeForPrimitiveValueClass(classHandle) == CORINFO_TYPE_UNDEF);

        m_size = compiler->info.compCompHnd->getClassSize(classHandle);
    }
    else
    {
        m_isValueClass = false;
        m_size         = compiler->info.compCompHnd->getHeapClassSize(classHandle);
    }

    if (m_size < TARGET_POINTER_SIZE)
    {
        assert(GetSlotCount() == 1);

        // This class can't contain GC pointers nor can it be a SIMD type.

        return;
    }

    if ((attribs & (CORINFO_FLG_CONTAINS_GC_PTR | CORINFO_FLG_CONTAINS_STACK_PTR)) == 0)
    {
#ifdef FEATURE_SIMD
        if (m_isValueClass && ((attribs & CORINFO_FLG_INTRINSIC_TYPE) != 0) && compiler->supportSIMDTypes() &&
            compiler->structSizeMightRepresentSIMDType(m_size))
        {
            m_layoutInfo = GetVectorLayoutInfo(classHandle, compiler);

            if (m_layoutInfo.simdType != TYP_UNDEF)
            {
                compiler->compFloatingPointUsed = true;
                // Populate SIMDHandlesCache
                compiler->getBaseTypeAndSizeOfSIMDType(classHandle);
            }
        }
#endif

        return;
    }

    unsigned slotCount = GetSlotCount();
    BYTE*    gcPtrs;

    if (slotCount > sizeof(m_gcPtrsArray))
    {
        gcPtrs = new (compiler, CMK_ClassLayout) BYTE[slotCount];
    }
    else
    {
        gcPtrs = m_gcPtrsArray;
    }

    m_gcPtrCount = compiler->info.compCompHnd->getClassGClayout(m_classHandle, gcPtrs);

    assert(m_gcPtrCount <= slotCount);

    // We need to set m_gcPtrs only after we figure out that the class really has GC pointers,
    // it it assumed that if there are no GC pointers then m_simdType has a valid value, be it
    // an actual SIMD type or TYP_UNDEF.

    // TODO-MIKE-Cleanup: It would seem that CORINFO_FLG_CONTAINS_GC_PTR cannot be relied on,
    // "by ref like" types may not have it set but still contain GC pointers.
    // That's unfortunate, both due to the unnecessary getClassGClayout call and the useless
    // array allocation.

    if ((m_gcPtrCount != 0) && (slotCount > sizeof(m_gcPtrsArray)))
    {
        m_gcPtrs = gcPtrs;
    }

    // We assume that we cannot have a struct with GC pointers that is not a multiple
    // of the register size.
    // The EE currently does not allow this, but it could change.
    // Let's assert it just to be safe.
    // This doesn't work for heap classes because getHeapClassSize returns an incorrect
    // class size, that doesn't include the padding required for alignment.

    // TODO-MIKE-Cleanup: getHeapClassSize should be fixed instead, it's only used in
    // ClassLayout::Create. Not only that omitting the padding in size is unexpected
    // but it also serves no purpose - we only need the heap size for stack allocated
    // objects and the JIT always allocates entire stack slots.
    noway_assert(!m_isValueClass || (m_gcPtrCount == 0) || (roundUp(m_size, REGSIZE_BYTES) == m_size));
}

#ifdef FEATURE_SIMD

ClassLayout::LayoutInfo ClassLayout::GetVectorLayoutInfo(CORINFO_CLASS_HANDLE classHandle, Compiler* compiler)
{
    ICorJitInfo* vm               = compiler->info.compCompHnd;
    const char*  namespaceName    = nullptr;
    const char*  className        = vm->getClassNameFromMetadata(classHandle, &namespaceName);
    bool         isNumericsVector = strcmp(namespaceName, "System.Numerics") == 0;

    if (isNumericsVector)
    {
        if (strcmp(className, "Vector2") == 0)
        {
            return {VectorKind::Vector234, TYP_SIMD8, TYP_FLOAT, true};
        }

        if (strcmp(className, "Vector3") == 0)
        {
            return {VectorKind::Vector234, TYP_SIMD12, TYP_FLOAT, true};
        }

        if (strcmp(className, "Vector4") == 0)
        {
            return {VectorKind::Vector234, TYP_SIMD16, TYP_FLOAT, true};
        }
    }

    CORINFO_CLASS_HANDLE elementTypeHandle = vm->getTypeInstantiationArgument(classHandle, 0);

    if (elementTypeHandle == NO_CLASS_HANDLE)
    {
        return {VectorKind::None, TYP_UNDEF, TYP_UNDEF, false};
    }

    CorInfoType elementCorType = vm->getTypeForPrimitiveNumericClass(elementTypeHandle);

    if (elementCorType == CORINFO_TYPE_UNDEF)
    {
        JITDUMP("Unexpected vector element type %s.%s\n", vm->getClassName(elementTypeHandle));
        return {VectorKind::None, TYP_UNDEF, TYP_UNDEF, false};
    }

    assert((elementCorType >= CORINFO_TYPE_BYTE) && (elementCorType <= CORINFO_TYPE_DOUBLE));
    assert((elementCorType != CORINFO_TYPE_NATIVEINT) && (elementCorType != CORINFO_TYPE_NATIVEUINT));

    var_types elementType;

    switch (elementCorType)
    {
        case CORINFO_TYPE_UINT:
            elementType = TYP_UINT;
            break;
        case CORINFO_TYPE_ULONG:
            elementType = TYP_ULONG;
            break;
        default:
            elementType = JITtype2varType(elementCorType);
            break;
    }

    if (isNumericsVector && strcmp(className, "Vector`1") == 0)
    {
        unsigned size = compiler->getSIMDVectorRegisterByteLength();
        assert((size == 16) || (size == 32));
        return {VectorKind::VectorT, size == 32 ? TYP_SIMD32 : TYP_SIMD16, elementType, false};
    }

#ifdef FEATURE_HW_INTRINSICS
    if (strcmp(className, "Vector128`1") == 0)
    {
        return {VectorKind::VectorNT, TYP_SIMD16, elementType, false};
    }

#ifdef TARGET_ARM64
    if (strcmp(className, "Vector64`1") == 0)
    {
        return {VectorKind::VectorNT, TYP_SIMD8, elementType, false};
    }
#endif

#ifdef TARGET_XARCH
    if (strcmp(className, "Vector256`1") == 0)
    {
        if (!compiler->compExactlyDependsOn(InstructionSet_AVX))
        {
            JITDUMP("SIMD32/AVX is not available\n");
            return {VectorKind::None, TYP_UNDEF, TYP_UNDEF, false};
        }

        return {VectorKind::VectorNT, TYP_SIMD32, elementType, false};
    }
#endif

#endif // FEATURE_HW_INTRINSICS

    return {VectorKind::None, TYP_UNDEF, TYP_UNDEF, false};
}

#endif // FEATURE_SIMD

#ifdef TARGET_AMD64
ClassLayout* ClassLayout::GetPPPQuirkLayout(CompAllocator alloc)
{
    assert(m_classHandle != NO_CLASS_HANDLE);
    assert(m_isValueClass);
    assert(m_size == 32);
    assert(GetSIMDType() == TYP_UNDEF);

    if (m_pppQuirkLayout == nullptr)
    {
        m_pppQuirkLayout = new (alloc) ClassLayout(m_classHandle, m_isValueClass, 64 DEBUGARG(m_className));
        m_pppQuirkLayout->m_gcPtrCount = m_gcPtrCount;

        static_assert_no_msg(_countof(m_gcPtrsArray) == 8);

        for (int i = 0; i < 4; i++)
        {
            m_pppQuirkLayout->m_gcPtrsArray[i] = m_gcPtrsArray[i];
        }

        for (int i = 4; i < 8; i++)
        {
            m_pppQuirkLayout->m_gcPtrsArray[i] = TYPE_GC_NONE;
        }
    }

    return m_pppQuirkLayout;
}
#endif // TARGET_AMD64

//------------------------------------------------------------------------
// AreCompatible: check if 2 layouts are the same for copying.
//
// Arguments:
//    layout1 - the first layout;
//    layout2 - the second layout.
//
// Return value:
//    true if compatible, false otherwise.
//
// Notes:
//    Layouts are called compatible if they are equal or if
//    they have the same size and the same GC slots.
//
// static
bool ClassLayout::AreCompatible(const ClassLayout* layout1, const ClassLayout* layout2)
{
    assert((layout1 != nullptr) && (layout2 != nullptr));

    if (layout1 == layout2)
    {
        return true;
    }

    if (layout1->GetSize() != layout2->GetSize())
    {
        return false;
    }

    if (layout1->GetGCPtrCount() != layout2->GetGCPtrCount())
    {
        return false;
    }

    assert(layout1->GetSlotCount() == layout2->GetSlotCount());
    unsigned slotsCount = layout1->GetSlotCount();

    for (unsigned i = 0; i < slotsCount; ++i)
    {
        if (layout1->GetGCPtrType(i) != layout2->GetGCPtrType(i))
        {
            return false;
        }
    }
    return true;
}
