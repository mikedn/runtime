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
    unsigned m_layoutCount = 0;
    // The capacity of m_layoutLargeArray (when more than 3 layouts are stored).
    unsigned m_layoutLargeCapacity = 0;

#ifdef FEATURE_SIMD
    // The vector layout table has 10 entries for each supported generic Vector (Vector<T>, Vector64/128/256<T>)
    // plus another 3 entries for Vector2/3/4.
    static_assert_no_msg(TYP_DOUBLE - TYP_BYTE == 9);
    static constexpr unsigned VectorElementTypesCount = 10;
#ifdef TARGET_ARM64
    static constexpr unsigned Vector64BaseIndex  = 0;
    static constexpr unsigned Vector128BaseIndex = Vector64BaseIndex + VectorElementTypesCount;
    static constexpr unsigned VectorTBaseIndex   = Vector128BaseIndex + VectorElementTypesCount;
#elif defined(TARGET_XARCH)
    static constexpr unsigned Vector128BaseIndex = 0;
    static constexpr unsigned Vector256BaseIndex = Vector128BaseIndex + VectorElementTypesCount;
    static constexpr unsigned VectorTBaseIndex   = Vector256BaseIndex + VectorElementTypesCount;
#endif
    static constexpr unsigned Vector234BaseIndex    = VectorTBaseIndex + VectorElementTypesCount;
    static constexpr unsigned VectorLayoutTableSize = Vector234BaseIndex + 3;

    ClassLayout** m_vectorLayoutTable = nullptr;
#endif

public:
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

#ifdef FEATURE_SIMD
    ClassLayout* GetVectorLayout(var_types simdType, var_types elementType)
    {
        if (m_vectorLayoutTable == nullptr)
        {
            return nullptr;
        }

        // TODO-MIKE-Cleanup: This might be more complicated than it needs to be.
        // Usually we just need a vector layout that happens to have the required
        // SIMD type, the element type is irrelevant. The layout is usually needed
        // to create SIMD typed locals and that need is questionable, only a few
        // places in the JIT code really need the layout (the main one would be
        // struct promotion because it needs to be able to promoted Vector2/3/4).
        // For now just try to be reasonably accurate:
        //     - ignore the vector kind, it's more trouble than it's worth
        //     - prefer System.Runtime.Intrinsics vector types to System.Numerics ones
        //     - prefer Vector<T> to Vector2/3/4
        //     - prefer a layout having the requested element type but allow fallback
        //       to any other element type

        if (elementType == TYP_UNDEF)
        {
            elementType = TYP_FLOAT;
        }

        unsigned elementTypeIndex = elementType - TYP_BYTE;
        assert(elementTypeIndex < VectorElementTypesCount);

        auto FindLayout = [this](unsigned baseIndex) -> ClassLayout* {
            for (unsigned i = 0; i < VectorElementTypesCount; i++)
            {
                if (m_vectorLayoutTable[baseIndex + i] != nullptr)
                {
                    return m_vectorLayoutTable[baseIndex + i];
                }
            }
            return nullptr;
        };

        switch (simdType)
        {
            ClassLayout* layout;

            case TYP_SIMD8:
#ifdef TARGET_ARM64
                layout = m_vectorLayoutTable[Vector64BaseIndex + elementTypeIndex];
                if (layout != nullptr)
                {
                    return layout;
                }
                if (elementType == TYP_FLOAT)
                {
                    layout = m_vectorLayoutTable[Vector234BaseIndex + 0];
                    if (layout != nullptr)
                    {
                        return layout;
                    }
                }
                layout = FindLayout(Vector64BaseIndex);
                if (layout != nullptr)
                {
                    return layout;
                }
#endif
                return m_vectorLayoutTable[Vector234BaseIndex + 0];

            case TYP_SIMD12:
                return m_vectorLayoutTable[Vector234BaseIndex + 1];

            case TYP_SIMD16:
                layout = m_vectorLayoutTable[Vector128BaseIndex + elementTypeIndex];
                if (layout != nullptr)
                {
                    return layout;
                }
                layout = m_vectorLayoutTable[VectorTBaseIndex + elementTypeIndex];
                if ((layout != nullptr) && (layout->GetSIMDType() == TYP_SIMD16))
                {
                    return layout;
                }
                if (elementType == TYP_FLOAT)
                {
                    layout = m_vectorLayoutTable[Vector234BaseIndex + 2];
                    if (layout != nullptr)
                    {
                        return layout;
                    }
                }
                layout = FindLayout(Vector128BaseIndex);
                if (layout != nullptr)
                {
                    return layout;
                }
                layout = FindLayout(VectorTBaseIndex);
                if (layout != nullptr)
                {
                    return layout;
                }
                return m_vectorLayoutTable[Vector234BaseIndex + 2];

#ifdef TARGET_XARCH
            case TYP_SIMD32:
                layout = m_vectorLayoutTable[Vector256BaseIndex + elementTypeIndex];
                if (layout != nullptr)
                {
                    return layout;
                }
                layout = m_vectorLayoutTable[VectorTBaseIndex + elementTypeIndex];
                if ((layout != nullptr) && (layout->GetSIMDType() == TYP_SIMD32))
                {
                    return layout;
                }
                layout = FindLayout(Vector256BaseIndex);
                if (layout != nullptr)
                {
                    return layout;
                }
                layout = FindLayout(VectorTBaseIndex);
                if ((layout != nullptr) && (layout->GetSIMDType() == simdType))
                {
                    return layout;
                }
                return nullptr;
#endif

            default:
                return nullptr;
        }
    }
#endif // FEATURE_SIMD

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
#ifdef FEATURE_SIMD
        if (layout->IsVector())
        {
            AddVectorLayout(compiler, layout);
        }
#endif

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

#ifdef FEATURE_SIMD
    void AddVectorLayout(Compiler* compiler, ClassLayout* layout)
    {
        assert(layout->IsVector());

        if (m_vectorLayoutTable == nullptr)
        {
            m_vectorLayoutTable = compiler->getAllocator(CMK_ClassLayout).allocate<ClassLayout*>(VectorLayoutTableSize);
            memset(m_vectorLayoutTable, 0, sizeof(m_vectorLayoutTable[0]) * VectorLayoutTableSize);
        }

        unsigned index = GetVectorLayoutIndex(layout->GetSIMDType(), layout->GetElementType(), layout->GetVectorKind());
        assert(index < VectorLayoutTableSize);
        m_vectorLayoutTable[index] = layout;
    }

    unsigned GetVectorLayoutIndex(var_types simdType, var_types elementType, VectorKind kind)
    {
        if (kind == VectorKind::Vector234)
        {
            assert(elementType == TYP_FLOAT);

            switch (simdType)
            {
                case TYP_SIMD8:
                    return Vector234BaseIndex + 0;
                case TYP_SIMD12:
                    return Vector234BaseIndex + 1;
                default:
                    assert(simdType == TYP_SIMD16);
                    return Vector234BaseIndex + 2;
            }
        }

        unsigned index = elementType - TYP_BYTE;
        assert(index < VectorElementTypesCount);

        switch (simdType)
        {
#ifdef TARGET_ARM64
            case TYP_SIMD8:
                index += Vector64BaseIndex;
                break;
#endif
#ifdef TARGET_XARCH
            case TYP_SIMD32:
                index += (kind == VectorKind::VectorNT ? Vector256BaseIndex : VectorTBaseIndex);
                break;
#endif
            default:
                assert(simdType == TYP_SIMD16);
                index += (kind == VectorKind::VectorNT ? Vector128BaseIndex : VectorTBaseIndex);
                break;
        }

        return index;
    }
#endif // FEATURE_SIMD
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

var_types Compiler::typGetStructType(CORINFO_CLASS_HANDLE classHandle, var_types* elementType)
{
#ifdef FEATURE_SIMD
    if (supportSIMDTypes())
    {
        ClassLayout* layout = typGetObjLayout(classHandle);
        if (layout->IsVector())
        {
            if (elementType != nullptr)
            {
                *elementType = layout->GetElementType();
            }
            return layout->GetSIMDType();
        }
    }
#endif

    return TYP_STRUCT;
}

var_types Compiler::typGetStructType(ClassLayout* layout)
{
#ifdef FEATURE_SIMD
    if (layout->IsVector())
    {
        return layout->GetSIMDType();
    }
#endif

    return TYP_STRUCT;
}

ClassLayout* Compiler::typGetStructLayout(GenTree* node)
{
    assert(varTypeIsStruct(node->GetType()));

    node = node->gtEffectiveVal();

    switch (node->GetOper())
    {
        case GT_OBJ:
            return node->AsObj()->GetLayout();
        case GT_CALL:
            return node->AsCall()->GetRetLayout();
        case GT_LCL_VAR:
            return lvaGetDesc(node->AsLclVar())->GetLayout();
        case GT_LCL_FLD:
            return node->AsLclFld()->GetLayout(this);
        case GT_IND:
#ifdef FEATURE_SIMD
        case GT_SIMD:
#endif
#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
#endif
            return nullptr;
        default:
            // This is not intended to be used before global morph so FIELD and INDEX are not handled.
            unreached();
    }
}

ClassLayout* Compiler::typGetVectorLayout(GenTree* node)
{
    assert(varTypeIsSIMD(node->GetType()));

#ifdef FEATURE_SIMD
    node = node->gtEffectiveVal();

    switch (node->GetOper())
    {
        case GT_OBJ:
            return node->AsObj()->GetLayout();
        case GT_CALL:
            return node->AsCall()->GetRetLayout();
        case GT_LCL_VAR:
            return lvaGetDesc(node->AsLclVar())->GetLayout();
        case GT_LCL_FLD:
            if (ClassLayout* layout = node->AsLclFld()->GetLayout(this))
            {
                return layout;
            }
            FALLTHROUGH;
        case GT_IND:
            return typGetVectorLayout(node->GetType(), TYP_UNDEF);
        case GT_SIMD:
            return typGetVectorLayout(node->GetType(), node->AsSIMD()->GetSIMDBaseType());
#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            return typGetVectorLayout(node->GetType(), node->AsHWIntrinsic()->GetSIMDBaseType());
#endif
        default:
            // This is not intended to be used before global morph so FIELD and INDEX are not handled.
            unreached();
    }
#else
    return nullptr;
#endif
}

ClassLayout* Compiler::typGetVectorLayout(var_types simdType, var_types elementType)
{
#ifdef FEATURE_SIMD
    return typGetClassLayoutTable()->GetVectorLayout(simdType, elementType);
#else
    return nullptr;
#endif
}

ClassLayout::ClassLayout(CORINFO_CLASS_HANDLE classHandle, Compiler* compiler)
    : m_classHandle(classHandle)
    , m_size(0)
    , m_isValueClass(true)
    , m_gcPtrCount(0)
    , m_gcPtrs(nullptr)
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

void ClassLayout::EnsureHfaInfo(Compiler* compiler)
{
    assert(IsValueClass());

    if ((m_gcPtrCount != 0) || (m_layoutInfo.hfaElementType != TYP_UNDEF))
    {
        return;
    }

#ifndef FEATURE_HFA
    m_layoutInfo.hfaElementType = TYP_UNKNOWN;
#else
    if ((m_size > MAX_PASS_MULTIREG_BYTES) || compiler->opts.compUseSoftFP)
    {
        m_layoutInfo.hfaElementType = TYP_UNKNOWN;

        return;
    }

    CorInfoHFAElemType hfaType = compiler->info.compCompHnd->getHFAType(m_classHandle);

    switch (hfaType)
    {
        case CORINFO_HFA_ELEM_FLOAT:
            m_layoutInfo.hfaElementType = TYP_FLOAT;
            break;
        case CORINFO_HFA_ELEM_DOUBLE:
            m_layoutInfo.hfaElementType = TYP_DOUBLE;
            break;
#ifdef FEATURE_SIMD
        case CORINFO_HFA_ELEM_VECTOR64:
            m_layoutInfo.hfaElementType = TYP_SIMD8;
            break;
        case CORINFO_HFA_ELEM_VECTOR128:
            m_layoutInfo.hfaElementType = TYP_SIMD16;
            break;
#endif
        default:
            assert(hfaType == CORINFO_HFA_ELEM_NONE);
            m_layoutInfo.hfaElementType = TYP_UNKNOWN;
            break;
    }

    assert((m_layoutInfo.hfaElementType == TYP_UNKNOWN) || (m_size % varTypeSize(m_layoutInfo.hfaElementType) == 0));
#endif
}

void ClassLayout::EnsureSysVAmd64AbiInfo(Compiler* compiler)
{
    assert(IsValueClass());

#ifdef UNIX_AMD64_ABI
    if (m_sysVAmd64AbiInfo.initialized)
    {
        return;
    }

    m_sysVAmd64AbiInfo.initialized = true;

    SYSTEMV_AMD64_CORINFO_STRUCT_REG_PASSING_DESCRIPTOR desc;
    compiler->info.compCompHnd->getSystemVAmd64PassStructInRegisterDescriptor(m_classHandle, &desc);

    if (desc.passedInRegisters)
    {
        m_sysVAmd64AbiInfo.regCount = desc.eightByteCount;

        for (unsigned i = 0; i < desc.eightByteCount; i++)
        {
            m_sysVAmd64AbiInfo.regTypes[i] = GetEightbyteType(desc, i);
        }
    }
#endif
}

#ifdef UNIX_AMD64_ABI
var_types ClassLayout::GetEightbyteType(const SYSTEMV_AMD64_CORINFO_STRUCT_REG_PASSING_DESCRIPTOR& desc, unsigned i)
{
    assert(i < desc.eightByteCount);
    // Make sure the VM doesn't get any funny ideas...
    assert(desc.eightByteOffsets[i] == i * 8);

    switch (desc.eightByteClassifications[i])
    {
        case SystemVClassificationTypeIntegerReference:
            assert(desc.eightByteSizes[i] == 8);
            return TYP_REF;

        case SystemVClassificationTypeIntegerByRef:
            assert(desc.eightByteSizes[i] == 8);
            return TYP_BYREF;

        case SystemVClassificationTypeSSE:
            if (desc.eightByteSizes[i] == 4)
            {
                return TYP_FLOAT;
            }

            assert(desc.eightByteSizes[i] == 8);
            return TYP_DOUBLE;

        default:
            assert(desc.eightByteClassifications[i] == SystemVClassificationTypeInteger);
            switch (desc.eightByteSizes[i])
            {
                case 1:
                    return TYP_BYTE;
                case 2:
                    return TYP_SHORT;
                case 3:
                case 4:
                    return TYP_INT;
                default:
                    assert(desc.eightByteSizes[i] <= 8);
                    return TYP_LONG;
            }
    }
}
#endif // UNIX_AMD64_ABI

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
            return {VectorKind::Vector234, TYP_SIMD8, TYP_FLOAT};
        }

        if (strcmp(className, "Vector3") == 0)
        {
            return {VectorKind::Vector234, TYP_SIMD12, TYP_FLOAT};
        }

        if (strcmp(className, "Vector4") == 0)
        {
            return {VectorKind::Vector234, TYP_SIMD16, TYP_FLOAT};
        }
    }

    CORINFO_CLASS_HANDLE elementTypeHandle = vm->getTypeInstantiationArgument(classHandle, 0);

    if (elementTypeHandle == NO_CLASS_HANDLE)
    {
        return {VectorKind::None, TYP_UNDEF, TYP_UNDEF};
    }

    CorInfoType elementCorType = vm->getTypeForPrimitiveNumericClass(elementTypeHandle);

    if (elementCorType == CORINFO_TYPE_UNDEF)
    {
        JITDUMP("Unexpected vector element type %s.%s\n", vm->getClassName(elementTypeHandle));
        return {VectorKind::None, TYP_UNDEF, TYP_UNDEF};
    }

    assert((elementCorType >= CORINFO_TYPE_BYTE) && (elementCorType <= CORINFO_TYPE_DOUBLE));
    assert((elementCorType != CORINFO_TYPE_NATIVEINT) && (elementCorType != CORINFO_TYPE_NATIVEUINT));

    var_types elementType = CorTypeToPreciseVarType(elementCorType);

    if (isNumericsVector && strcmp(className, "Vector`1") == 0)
    {
        unsigned size = compiler->getSIMDVectorRegisterByteLength();
        assert((size == 16) || (size == 32));
        return {VectorKind::VectorT, size == 32 ? TYP_SIMD32 : TYP_SIMD16, elementType};
    }

#ifdef FEATURE_HW_INTRINSICS
    if (strcmp(className, "Vector128`1") == 0)
    {
        return {VectorKind::VectorNT, TYP_SIMD16, elementType};
    }

#ifdef TARGET_ARM64
    if (strcmp(className, "Vector64`1") == 0)
    {
        return {VectorKind::VectorNT, TYP_SIMD8, elementType};
    }
#endif

#ifdef TARGET_XARCH
    if (strcmp(className, "Vector256`1") == 0)
    {
        if (!compiler->compExactlyDependsOn(InstructionSet_AVX))
        {
            JITDUMP("SIMD32/AVX is not available\n");
            return {VectorKind::None, TYP_UNDEF, TYP_UNDEF};
        }

        return {VectorKind::VectorNT, TYP_SIMD32, elementType};
    }
#endif

#endif // FEATURE_HW_INTRINSICS

    return {VectorKind::None, TYP_UNDEF, TYP_UNDEF};
}

#endif // FEATURE_SIMD

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
