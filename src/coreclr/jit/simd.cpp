// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

//
//   SIMD Support
//
// IMPORTANT NOTES AND CAVEATS:
//
// This implementation is preliminary, and may change dramatically.
//
// New JIT types, TYP_SIMDxx, are introduced, and the SIMD intrinsics are created as GT_SIMD nodes.
// Nodes of SIMD types will be typed as TYP_SIMD* (e.g. TYP_SIMD8, TYP_SIMD16, etc.).
//
// Note that currently the "reference implementation" is the same as the runtime dll.  As such, it is currently
// providing implementations for those methods not currently supported by the JIT as intrinsics.
//
// These are currently recognized using string compares, in order to provide an implementation in the JIT
// without taking a dependency on the VM.
// Furthermore, in the CTP, in order to limit the impact of doing these string compares
// against assembly names, we only look for the SIMDVector assembly if we are compiling a class constructor.  This
// makes it somewhat more "pay for play" but is a significant usability compromise.
// This has been addressed for RTM by doing the assembly recognition in the VM.
// --------------------------------------------------------------------------------------

#include "jitpch.h"
#include "simd.h"

#ifdef _MSC_VER
#pragma hdrstop
#endif

#ifdef FEATURE_SIMD

// Intrinsic Id to intrinsic info map
const SIMDIntrinsicInfo simdIntrinsicInfoArray[] = {
#define SIMD_INTRINSIC(mname, inst, id, retType, argCount, arg1, arg2, arg3, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)  \
    {SIMDIntrinsic##id, mname, inst, retType, argCount, arg1, arg2, arg3, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10},
#include "simdintrinsiclist.h"
};

//------------------------------------------------------------------------
// getSIMDVectorLength: Get the length (number of elements of base type) of
//                      SIMD Vector given its size and base (element) type.
//
// Arguments:
//    simdSize   - size of the SIMD vector
//    baseType   - type of the elements of the SIMD vector
//
// static
int Compiler::getSIMDVectorLength(unsigned simdSize, var_types baseType)
{
    return simdSize / genTypeSize(baseType);
}

//------------------------------------------------------------------------
// Get the preferred alignment of SIMD vector type for better performance.
//
// Arguments:
//    typeHnd  - type handle of the SIMD vector
//
int Compiler::getSIMDTypeAlignment(var_types simdType)
{
    unsigned size = varTypeSize(simdType);

#ifdef TARGET_XARCH
    if (size == 8)
    {
        return 8;
    }

    if (size <= 16)
    {
        assert((size == 12) || (size == 16));
        return 16;
    }

    assert(size == 32);
    return 32;
#elif defined(TARGET_ARM64)
    // preferred alignment for 64-bit vectors is 8-bytes.
    // For everything else, 16-bytes.
    return (size == 8) ? 8 : 16;
#else
    assert(!"getSIMDTypeAlignment() unimplemented on target arch");
    unreached();
#endif
}

const SIMDIntrinsicInfo* Compiler::getSIMDIntrinsicInfo(const char*           className,
                                                        const char*           methodName,
                                                        CORINFO_SIG_INFO*     sig,
                                                        bool                  isNewObj,
                                                        CORINFO_CLASS_HANDLE* inOutTypeHnd,
                                                        unsigned*             argCount,
                                                        var_types*            baseType,
                                                        unsigned*             sizeBytes)
{
    assert(featureSIMD);
    assert(baseType != nullptr);
    assert(sizeBytes != nullptr);

    if (strcmp(className, "Vector") == 0)
    {
        // All of the supported intrinsics on this static class take a first argument that's a vector,
        // which determines the baseType.
        // The exception is the IsHardwareAccelerated property, which is handled as a special case.

        if (sig->numArgs == 0)
        {
            const SIMDIntrinsicInfo& info = simdIntrinsicInfoArray[SIMDIntrinsicHWAccel];

            if (strcmp(methodName, info.methodName) == 0)
            {
                // Sanity check
                assert((info.argCount == 0) && !info.isInstMethod && (JITtype2varType(sig->retType) == info.retType));

                return &info;
            }

            return nullptr;
        }

        *inOutTypeHnd = info.compCompHnd->getArgClass(sig, sig->args);
    }

    CORINFO_CLASS_HANDLE typeHnd = *inOutTypeHnd;

    // Avoid calling typGetObjLayout if the type isn't a struct, it would create an unnecessary layout instance.
    // TODO-MIKE-Cleanup: Split typGetObjLayout into typGetStructLayout and typGetClassLayout? Only object stack
    // allocation needs class layouts.
    if (!info.compCompHnd->isValueClass(typeHnd))
    {
        return nullptr;
    }

    ClassLayout* layout = typGetObjLayout(typeHnd);

    if (!layout->IsVector())
    {
        JITDUMP("NOT a SIMD Intrinsic: unsupported baseType\n");
        return nullptr;
    }

    assert((layout->GetVectorKind() == VectorKind::Vector234) || (layout->GetVectorKind() == VectorKind::VectorT));

    *baseType  = layout->GetElementType();
    *sizeBytes = layout->GetSize();

    // account for implicit "this" arg
    *argCount = sig->numArgs;
    if (sig->hasThis())
    {
        *argCount += 1;
    }

    // Get the Intrinsic Id by parsing method name.
    //
    // TODO-Throughput: replace sequential search by binary search by arranging entries
    // sorted by method name.
    SIMDIntrinsicID intrinsicId = SIMDIntrinsicInvalid;
    for (int i = SIMDIntrinsicNone + 1; i < SIMDIntrinsicInvalid; ++i)
    {
        if (strcmp(methodName, simdIntrinsicInfoArray[i].methodName) == 0)
        {
            // Found an entry for the method; further check whether it is one of
            // the supported base types.
            bool found = false;
            for (int j = 0; j < SIMD_INTRINSIC_MAX_BASETYPE_COUNT; ++j)
            {
                // Convention: if there are fewer base types supported than MAX_BASETYPE_COUNT,
                // the end of the list is marked by TYP_UNDEF.
                if (simdIntrinsicInfoArray[i].supportedBaseTypes[j] == TYP_UNDEF)
                {
                    break;
                }

                if (simdIntrinsicInfoArray[i].supportedBaseTypes[j] == *baseType)
                {
                    found = true;
                    break;
                }
            }

            if (!found)
            {
                continue;
            }

            // Now, check the arguments.
            unsigned int fixedArgCnt    = simdIntrinsicInfoArray[i].argCount;
            unsigned int expectedArgCnt = fixedArgCnt;

            // First handle SIMDIntrinsicInitN, where the arg count depends on the type.
            // The listed arg types include the vector and the first two init values, which is the expected number
            // for Vector2.  For other cases, we'll check their types here.
            if (*argCount > expectedArgCnt)
            {
                if (i == SIMDIntrinsicInitN)
                {
                    if (layout->GetVectorKind() != VectorKind::Vector234)
                    {
                        continue;
                    }

                    if ((*argCount == 3) && (layout->GetSIMDType() == TYP_SIMD8))
                    {
                        expectedArgCnt = 3;
                    }
                    else if ((*argCount == 4) && (layout->GetSIMDType() == TYP_SIMD12))
                    {
                        expectedArgCnt = 4;
                    }
                    else if ((*argCount == 5) && (layout->GetSIMDType() == TYP_SIMD16))
                    {
                        expectedArgCnt = 5;
                    }
                }
                else if (i == SIMDIntrinsicInitFixed)
                {
                    if (layout->GetVectorKind() != VectorKind::Vector234)
                    {
                        continue;
                    }

                    if ((*argCount == 4) && (layout->GetSIMDType() == TYP_SIMD16))
                    {
                        expectedArgCnt = 4;
                    }
                }
            }
            if (*argCount != expectedArgCnt)
            {
                continue;
            }

            // Validate the types of individual args passed are what is expected of.
            // If any of the types don't match with what is expected, don't consider
            // as an intrinsic.  This will make an older JIT with SIMD capabilities
            // resilient to breaking changes to SIMD managed API.
            //
            // Note that from IL type stack, args get popped in right to left order
            // whereas args get listed in method signatures in left to right order.

            int stackIndex = (expectedArgCnt - 1);

            // Track the arguments from the signature - we currently only use this to distinguish
            // integral and pointer types, both of which will by TYP_I_IMPL on the importer stack.
            CORINFO_ARG_LIST_HANDLE argLst = sig->args;

            CORINFO_CLASS_HANDLE argClass;
            for (unsigned int argIndex = 0; found == true && argIndex < expectedArgCnt; argIndex++)
            {
                bool isThisPtr = ((argIndex == 0) && sig->hasThis());

                // In case of "newobj SIMDVector<T>(T val)", thisPtr won't be present on type stack.
                // We don't check anything in that case.
                if (!isThisPtr || !isNewObj)
                {
                    GenTree*  arg     = impStackTop(stackIndex).val;
                    var_types argType = arg->TypeGet();

                    var_types expectedArgType;
                    if (argIndex < fixedArgCnt)
                    {
                        // Convention:
                        //   - intrinsicInfo.argType[i] == TYP_UNDEF - intrinsic doesn't have a valid arg at position i
                        //   - intrinsicInfo.argType[i] == TYP_UNKNOWN - arg type should be same as basetype
                        // Note that we pop the args off in reverse order.
                        expectedArgType = simdIntrinsicInfoArray[i].argType[argIndex];
                        assert(expectedArgType != TYP_UNDEF);
                        if (expectedArgType == TYP_UNKNOWN)
                        {
                            // The type of the argument will be genActualType(*baseType).
                            expectedArgType = genActualType(*baseType);
                            argType         = genActualType(argType);
                        }
                    }
                    else
                    {
                        expectedArgType = *baseType;
                    }

                    if (!isThisPtr && argType == TYP_I_IMPL)
                    {
                        // The reference implementation has a constructor that takes a pointer.
                        // We don't want to recognize that one.  This requires us to look at the CorInfoType
                        // in order to distinguish a signature with a pointer argument from one with an
                        // integer argument of pointer size, both of which will be TYP_I_IMPL on the stack.
                        // TODO-Review: This seems quite fragile.  We should consider beefing up the checking
                        // here.
                        CorInfoType corType = strip(info.compCompHnd->getArgType(sig, argLst, &argClass));
                        if (corType == CORINFO_TYPE_PTR)
                        {
                            found = false;
                        }
                    }

                    if (varTypeIsSIMD(argType))
                    {
                        argType = TYP_STRUCT;
                    }
                    if (argType != expectedArgType)
                    {
                        found = false;
                    }
                }
                if (argIndex != 0 || !sig->hasThis())
                {
                    argLst = info.compCompHnd->getArgNext(argLst);
                }
                stackIndex--;
            }

            // Cross check return type and static vs. instance is what we are expecting.
            // If not, don't consider it as an intrinsic.
            // Note that ret type of TYP_UNKNOWN means that it is not known apriori and must be same as baseType
            if (found)
            {
                var_types expectedRetType = simdIntrinsicInfoArray[i].retType;
                if (expectedRetType == TYP_UNKNOWN)
                {
                    // JIT maps uint/ulong type vars to TYP_INT/TYP_LONG.
                    expectedRetType =
                        (*baseType == TYP_UINT || *baseType == TYP_ULONG) ? genActualType(*baseType) : *baseType;
                }

                if (JITtype2varType(sig->retType) != expectedRetType ||
                    sig->hasThis() != simdIntrinsicInfoArray[i].isInstMethod)
                {
                    found = false;
                }
            }

            if (found)
            {
                intrinsicId = (SIMDIntrinsicID)i;
                break;
            }
        }
    }

    if (intrinsicId != SIMDIntrinsicInvalid)
    {
        JITDUMP("Method %s maps to SIMD intrinsic %s\n", methodName, simdIntrinsicNames[intrinsicId]);
        return &simdIntrinsicInfoArray[intrinsicId];
    }
    else
    {
        JITDUMP("Method %s is NOT a SIMD intrinsic\n", methodName);
    }

    return nullptr;
}

/* static */ bool Compiler::vnEncodesResultTypeForSIMDIntrinsic(SIMDIntrinsicID intrinsicId)
{
    switch (intrinsicId)
    {
        case SIMDIntrinsicInit:
        case SIMDIntrinsicGetItem:
        case SIMDIntrinsicCast:
        case SIMDIntrinsicConvertToSingle:
        case SIMDIntrinsicConvertToDouble:
        case SIMDIntrinsicConvertToInt32:
        case SIMDIntrinsicConvertToInt64:
        case SIMDIntrinsicNarrow:
        case SIMDIntrinsicWidenHi:
        case SIMDIntrinsicWidenLo:
            return true;

        default:
            break;
    }
    return false;
}

// impSIMDPopStack: Pops and returns GenTree node from importer's type stack.
//
// Arguments:
//    type -  the type of value that the caller expects to be popped off the stack.
//
// Notes:
//    If the popped value is a struct, and the expected type is a simd type, it will be set
//    to that type, otherwise it will assert if the type being popped is not the expected type.
//
GenTree* Compiler::impSIMDPopStack(var_types type)
{
    assert(varTypeIsSIMD(type));

    GenTree* tree = impPopStack().val;

    if (tree->OperIs(GT_RET_EXPR, GT_CALL))
    {
        // TODO-MIKE-Cleanup: This is probably not needed when the SIMD type is returned in a register.

        ClassLayout* layout = tree->IsRetExpr() ? tree->AsRetExpr()->GetLayout() : tree->AsCall()->GetRetLayout();

        unsigned tmpNum = lvaGrabTemp(true DEBUGARG("struct address for call/obj"));
        impAssignTempGen(tmpNum, tree, layout->GetClassHandle(), CHECK_SPILL_ALL);
        tree = gtNewLclvNode(tmpNum, lvaGetDesc(tmpNum)->GetType());
    }

    assert(tree->GetType() == type);

    return tree;
}

//------------------------------------------------------------------------
// getOp1ForConstructor: Get the op1 for a constructor call.
//
// Arguments:
//    opcode     - the opcode being handled (needed to identify the CEE_NEWOBJ case)
//    newobjThis - For CEE_NEWOBJ, this is the temp grabbed for the allocated uninitalized object.
//    clsHnd    - The handle of the class of the method.
//
// Return Value:
//    The tree node representing the object to be initialized with the constructor.
//
// Notes:
//    This method handles the differences between the CEE_NEWOBJ and constructor cases.
//
GenTree* Compiler::getOp1ForConstructor(OPCODE opcode, GenTree* newobjThis, CORINFO_CLASS_HANDLE clsHnd)
{
    if (opcode == CEE_NEWOBJ)
    {
        assert(newobjThis->OperIs(GT_ADDR) && newobjThis->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR));

        // push newobj result on type stack
        unsigned tmpLclNum = newobjThis->AsUnOp()->GetOp(0)->AsLclVar()->GetLclNum();
        impPushOnStack(gtNewLclvNode(tmpLclNum, lvaGetRealType(tmpLclNum)), typeInfo(TI_STRUCT, clsHnd));

        return newobjThis;
    }

    return impPopStackCoerceArg(TYP_BYREF);
}

//-------------------------------------------------------------------
// Set the flag that indicates that the lclVar referenced by this tree
// is used in a SIMD intrinsic.
// Arguments:
//      tree - GenTree*

void Compiler::setLclRelatedToSIMDIntrinsic(GenTree* tree)
{
    assert(tree->OperIsLocal());
    unsigned   lclNum                = tree->AsLclVarCommon()->GetLclNum();
    LclVarDsc* lclVarDsc             = &lvaTable[lclNum];
    lclVarDsc->lvUsedInSIMDIntrinsic = true;
}

// Check whether two memory locations are contiguous.
//
// This recognizes trivial patterns such as FIELD(o, 4) & FIELD(o, 8) or INDEX(a, 1) & INDEX(a, 2).
// Pointer arithmetic isn't recognized (and probably not very useful anyway) and in the case of
// arrays only constant indices are recognized. Might be useful to also recognize i, i+1, i+2...
// If the locations are determined to be adjacent this also implies that the trees are also free
// of persistent side effects and they can be discarded. They may have exception side effects that
// may need to be preserved - a[1] doesn't imply that a[2] is also a valid array element.
//
bool Compiler::SIMDCoalescingBuffer::AreContiguousMemoryLocations(GenTree* l1, GenTree* l2)
{
    if ((l1->GetOper() != l2->GetOper()) || l1->TypeIs(TYP_STRUCT))
    {
        return false;
    }

    auto AreValuesEqual = [](GenTree* v1, GenTree* v2) {
        while (v1->GetOper() == v2->GetOper())
        {
            if (v1->OperIs(GT_ADDR))
            {
                v1 = v1->AsUnOp()->GetOp(0);
                v2 = v2->AsUnOp()->GetOp(0);

                continue;
            }

            if (v1->OperIs(GT_FIELD))
            {
                if ((v1->AsField()->GetFieldHandle() == v2->AsField()->GetFieldHandle()) &&
                    !v1->AsField()->IsVolatile() && !v2->AsField()->IsVolatile())
                {
                    v1 = v1->AsField()->GetAddr();
                    v2 = v2->AsField()->GetAddr();

                    continue;
                }

                return false;
            }

            if (v1->OperIs(GT_LCL_VAR))
            {
                return v1->AsLclVar()->GetLclNum() == v2->AsLclVar()->GetLclNum();
            }

            break;
        }

        return false;
    };

    auto AreConsecutiveConstants = [](GenTree* i1, GenTree* i2) {
        return i1->OperIs(GT_CNS_INT) && i2->OperIs(GT_CNS_INT) &&
               (i1->AsIntCon()->GetValue() + 1 == i2->AsIntCon()->GetValue());
    };

    auto AreContiguosArrayElements = [&](GenTreeIndex* e1, GenTreeIndex* e2) {
        return AreConsecutiveConstants(e1->GetIndex(), e2->GetIndex()) &&
               AreValuesEqual(e1->GetArray(), e2->GetArray());
    };

    auto AreContiguosFields = [&](GenTreeField* f1, GenTreeField* f2) {
        return (f1->GetOffset() + varTypeSize(f1->GetType()) == f2->GetOffset()) && !f1->IsVolatile() &&
               !f2->IsVolatile() && AreValuesEqual(f1->GetAddr(), f2->GetAddr());
    };

    auto AreContiguosLocalFields = [](GenTreeLclFld* f1, GenTreeLclFld* f2) {
        return (f1->GetLclNum() == f2->GetLclNum()) &&
               (f1->GetLclOffs() + varTypeSize(f1->GetType()) == f2->GetLclOffs());
    };

    switch (l1->GetOper())
    {
        case GT_INDEX:
            return AreContiguosArrayElements(l1->AsIndex(), l2->AsIndex());
        case GT_FIELD:
            return AreContiguosFields(l1->AsField(), l2->AsField());
        case GT_LCL_FLD:
            return AreContiguosLocalFields(l1->AsLclFld(), l2->AsLclFld());
        default:
            return false;
    }
}

// Change a FIELD/INDEX/LCL_FLD node into a SIMD typed IND/LCL_FLD.
//
void Compiler::SIMDCoalescingBuffer::ChangeToSIMDMem(Compiler* compiler, GenTree* tree, var_types simdType)
{
    assert(tree->TypeIs(TYP_FLOAT));

    if (tree->OperIs(GT_LCL_FLD))
    {
        tree->SetType(simdType);
        tree->AsLclFld()->SetFieldSeq(FieldSeqStore::NotAField());

        // This may have changed a partial local field into full local field
        if (tree->IsPartialLclFld(compiler))
        {
            tree->gtFlags |= GTF_VAR_USEASG;
        }
        else
        {
            tree->gtFlags &= ~GTF_VAR_USEASG;
        }

        return;
    }

    GenTree* addr   = nullptr;
    unsigned offset = 0;

    if (GenTreeField* field = tree->IsField())
    {
        assert(!tree->AsField()->IsVolatile());

        addr   = field->GetAddr();
        offset = field->GetOffset();

        if (addr->OperIs(GT_ADDR))
        {
            GenTree* location = addr->AsUnOp()->GetOp(0);

            // If this is the field of a local struct variable then set lvUsedInSIMDIntrinsic to prevent
            // the local from being promoted. If it gets promoted then it will be dependent-promoted due
            // to the indirection we're creating.

            // TODO-MIKE-Cleanup: This is done only for SIMD locals but it really should be done for any
            // struct local since the whole point is to block poor promotion.

            if (varTypeIsSIMD(location->GetType()) && location->OperIs(GT_LCL_VAR))
            {
                compiler->setLclRelatedToSIMDIntrinsic(location);
            }
        }

        // TODO-MIKE-Fix: This code replaces FIELD with and ADD(addr, offset) without adding
        // a NULLCHECK when the field offset is large enough to require it. It's not worth
        // fixing this until FIELD is replaced by FIELD_ADDR, otherwise we need to add ADDR
        // on top of the existing FIELD and then use that as the address of the indir.
    }
    else if (GenTreeIndex* element = tree->IsIndex())
    {
        GenTree* array = element->GetArray();
        unsigned index = static_cast<unsigned>(element->GetIndex()->AsIntCon()->GetValue());

        // Generate a bounds check for the array access. We access multiple array elements but for
        // bounds checking purposes it's sufficient to check if the last element index is valid,
        // then all the element indices before it will also be valid.

        unsigned simdElementCount = varTypeSize(simdType) / varTypeSize(TYP_FLOAT);

        GenTree* lastIndex = compiler->gtNewIconNode(index + simdElementCount - 1, TYP_INT);
        GenTree* arrLen =
            compiler->gtNewArrLen(compiler->gtCloneExpr(array), OFFSETOF__CORINFO_Array__length, compiler->compCurBB);
        GenTree* arrBndsChk = compiler->gtNewArrBoundsChk(lastIndex, arrLen, SCK_RNGCHK_FAIL);

        addr   = compiler->gtNewOperNode(GT_COMMA, array->GetType(), arrBndsChk, array);
        offset = OFFSETOF__CORINFO_Array__data + index * varTypeSize(TYP_FLOAT);
    }
    else
    {
        unreached();
    }

    if (offset != 0)
    {
        addr = compiler->gtNewOperNode(GT_ADD, TYP_BYREF, addr, compiler->gtNewIconNode(offset, TYP_I_IMPL));
    }

    tree->ChangeOper(GT_IND);
    tree->SetType(simdType);
    tree->AsIndir()->SetAddr(addr);
}

// Recognize a field of a SIMD local variable (Vector2/3/4 fields).
//
GenTreeLclVar* Compiler::SIMDCoalescingBuffer::IsSIMDField(GenTree* node)
{
    if (!node->OperIs(GT_FIELD))
    {
        return nullptr;
    }

    if (node->AsField()->IsVolatile())
    {
        // It probably doesn't make sense to coalesce volatile fields. Anyway LocalAddressVisitor
        // doesn't generate SIMDIntrinsicGetItem out of a volatile field and ChangeToSIMDMem does
        // not bother to make the indir it creates volatile...

        return nullptr;
    }

    if (node->AsField()->GetOffset() != m_index * varTypeSize(TYP_FLOAT))
    {
        return nullptr;
    }

    GenTree* addr = node->AsField()->GetAddr();

    if ((addr == nullptr) || !addr->OperIs(GT_ADDR) || !addr->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR))
    {
        return nullptr;
    }

    GenTreeLclVar* lclVar = addr->AsUnOp()->GetOp(0)->AsLclVar();

    if (!varTypeIsSIMD(lclVar->GetType()))
    {
        return nullptr;
    }

    assert(node->TypeIs(TYP_FLOAT));

    return lclVar;
}

// Recognize a SIMDIntrinsicGetItem that uses a SIMD local variable.
//
GenTreeLclVar* Compiler::SIMDCoalescingBuffer::IsSIMDGetItem(GenTree* node)
{
    if (!node->OperIs(GT_SIMD))
    {
        return nullptr;
    }

    GenTreeSIMD* getItem = node->AsSIMD();

    if ((getItem->GetIntrinsic() != SIMDIntrinsicGetItem) || !getItem->GetOp(0)->OperIs(GT_LCL_VAR) ||
        !getItem->GetOp(1)->IsIntegralConst(m_index))
    {
        return nullptr;
    }

    assert(getItem->GetSIMDBaseType() == TYP_FLOAT);
    assert(getItem->TypeIs(TYP_FLOAT));

    return getItem->GetOp(0)->AsLclVar();
};

// Try to add an assignment statement to the coalescing buffer (common code for Add and Mark).
// Return true if the statment is added and the number of statements in the buffer equals the number of SIMD elements.
//
bool Compiler::SIMDCoalescingBuffer::Add(Compiler* compiler, Statement* stmt, GenTreeOp* asg, GenTreeLclVar* simdLclVar)
{
    if (simdLclVar == nullptr)
    {
        Clear();
        return false;
    }

    if (m_index == 0)
    {
        m_firstStmt = stmt;
        m_lastStmt  = stmt;
        m_lclNum    = simdLclVar->GetLclNum();
        m_index++;
        return false;
    }

    if (simdLclVar->GetLclNum() != m_lclNum)
    {
        Clear();
        return false;
    }

    GenTreeOp* lastAsg = m_lastStmt->GetRootNode()->AsOp();

    if (!AreContiguousMemoryLocations(lastAsg->GetOp(0), asg->AsOp()->GetOp(0)))
    {
        Clear();
        return false;
    }

    m_lastStmt = stmt;
    m_index++;

    return (m_index == varTypeSize(compiler->lvaGetDesc(simdLclVar)->GetType()) / varTypeSize(TYP_FLOAT));
}

// Mark local variables that may be subject to SIMD coalescing to prevent struct promotion.
//
// TODO-MIKE-Cleanup: It's unfortunate that we need to do SIMD coalescing in two steps: first mark
// locals that are subject to coalescing, to prevent struct promotion, and then actually do coalescing.
// In general phase ordering in this area is messy and it's likely better to be:
//     - import (no SIMD coalescing marking)
//     - other unrelated phases (e.g. inlining)
//     - "local address visitor" - convert every (recognized) indirect local access to LCL_VAR/LCL_FLD
//       and record some information to help guide struct promotion (though it's questionable if this
//       phase needs to exist at all, most of it can be done during import and it's really importer's
//       job to deal with issues arising from unfortunate IL characteristics)
//     - struct promotion + implicit byref params + DNER marking
//     - SIMD coalescing (likely done during the same flow graph traversal as struct promotion)
//     - global morph
//
// That said, SIMD coalescing (or any other kind of memory coalescing) is better done in lowering,
// doing it in the frontend interferes with VN and anything it depends on it. Unfortunately after
// global morph it's more difficult to recognize contiguous memory locations because INDEX gets
// expanded into more complex trees. But then the coalescing code only recognizes constant array
// indices and COMMAs aren't present in LIR so probably there's not much difference.
//
void Compiler::SIMDCoalescingBuffer::Mark(Compiler* compiler, Statement* stmt)
{
    GenTree* asg = stmt->GetRootNode();

    if (!asg->TypeIs(TYP_FLOAT) || !asg->OperIs(GT_ASG))
    {
        Clear();
        return;
    }

    GenTreeLclVar* simdLclVar = IsSIMDField(asg->AsOp()->GetOp(1));

    if (!Add(compiler, stmt, asg->AsOp(), simdLclVar))
    {
        return;
    }

    compiler->setLclRelatedToSIMDIntrinsic(simdLclVar);

    GenTree* dest = asg->AsOp()->GetOp(0);

    if (GenTreeField* field = dest->IsField())
    {
        GenTree* addr = field->GetAddr();

        if ((addr != nullptr) && addr->OperIs(GT_ADDR) && addr->AsUnOp()->GetOp(0)->OperIsLocal())
        {
            compiler->setLclRelatedToSIMDIntrinsic(addr->AsUnOp()->GetOp(0));
        }
    }

    Clear();
}

// Try to add an assignment statement to the coalescing buffer.
// Return true if the statment is added and the number of statements in the buffer equals the number of SIMD elements.
//
bool Compiler::SIMDCoalescingBuffer::Add(Compiler* compiler, Statement* stmt)
{
    GenTree* asg = stmt->GetRootNode();

    if (!asg->TypeIs(TYP_FLOAT) || !asg->OperIs(GT_ASG))
    {
        Clear();
        return false;
    }

    GenTreeLclVar* simdLclVar = IsSIMDGetItem(asg->AsOp()->GetOp(1));

    return Add(compiler, stmt, asg->AsOp(), simdLclVar);
}

// Transform the first assignment in the buffer into a SIMD assignment
// and remove the rest of the statements from the block.
//
void Compiler::SIMDCoalescingBuffer::Coalesce(Compiler* compiler, BasicBlock* block)
{
    assert(m_index > 1);

    GenTreeOp*   asg      = m_firstStmt->GetRootNode()->AsOp();
    GenTreeSIMD* getItem  = asg->GetOp(1)->AsSIMD();
    var_types    simdType = getSIMDTypeForSize(getItem->GetSIMDSize());

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("\nFound %u contiguous assignments from a %s local to memory in " FMT_BB ":\n", m_index,
               varTypeName(simdType), block->bbNum);
        for (Statement* s = m_firstStmt; s != m_lastStmt->GetNextStmt(); s = s->GetNextStmt())
        {
            compiler->gtDispStmt(s);
        }
    }
#endif

    for (unsigned i = 1; i < m_index; i++)
    {
        compiler->fgRemoveStmt(block, m_firstStmt->GetNextStmt());
    }

    asg->SetType(simdType);
    ChangeToSIMDMem(compiler, asg->GetOp(0), simdType);
    asg->SetOp(1, getItem->GetOp(0)->AsLclVar());

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("Changed to a single %s assignment:\n", varTypeName(simdType));
        compiler->gtDispStmt(m_firstStmt);
        printf("\n");
    }
#endif

    Clear();
}

//------------------------------------------------------------------------
// impSIMDIntrinsic: Check method to see if it is a SIMD method
//
// Arguments:
//    opcode     - the opcode being handled (needed to identify the CEE_NEWOBJ case)
//    newobjThis - For CEE_NEWOBJ, this is the temp grabbed for the allocated uninitalized object.
//    clsHnd     - The handle of the class of the method.
//    method     - The handle of the method.
//    sig        - The call signature for the method.
//    memberRef  - The memberRef token for the method reference.
//
// Return Value:
//    If clsHnd is a known SIMD type, and 'method' is one of the methods that are
//    implemented as an intrinsic in the JIT, then return the tree that implements
//    it.
//
GenTree* Compiler::impSIMDIntrinsic(OPCODE                opcode,
                                    GenTree*              newobjThis,
                                    CORINFO_CLASS_HANDLE  clsHnd,
                                    CORINFO_METHOD_HANDLE methodHnd,
                                    CORINFO_SIG_INFO*     sig,
                                    unsigned              methodFlags,
                                    int                   memberRef)
{
    assert(featureSIMD);

    // Exit early if the method is not a JIT Intrinsic (which requires the [Intrinsic] attribute).
    if ((methodFlags & CORINFO_FLG_JIT_INTRINSIC) == 0)
    {
        return nullptr;
    }

    const char* className          = nullptr;
    const char* namespaceName      = nullptr;
    const char* enclosingClassName = nullptr;
    const char* methodName =
        info.compCompHnd->getMethodNameFromMetadata(methodHnd, &className, &namespaceName, &enclosingClassName);

    if ((namespaceName == nullptr) || (strcmp(namespaceName, "System.Numerics") != 0))
    {
        return nullptr;
    }

    unsigned  size     = 0;
    var_types baseType = TYP_UNKNOWN;
    unsigned  argCount = 0;

    const SIMDIntrinsicInfo* intrinsicInfo =
        getSIMDIntrinsicInfo(className, methodName, sig, (opcode == CEE_NEWOBJ), &clsHnd, &argCount, &baseType, &size);

    // Exit early if the intrinsic is invalid or unrecognized
    if ((intrinsicInfo == nullptr) || (intrinsicInfo->id == SIMDIntrinsicInvalid))
    {
        return nullptr;
    }

#if defined(TARGET_XARCH)
    CORINFO_InstructionSet minimumIsa = InstructionSet_SSE2;
#elif defined(TARGET_ARM64)
    CORINFO_InstructionSet minimumIsa = InstructionSet_AdvSimd;
#else
#error Unsupported platform
#endif // !TARGET_XARCH && !TARGET_ARM64

    if (!compOpportunisticallyDependsOn(minimumIsa) || !JitConfig.EnableHWIntrinsic())
    {
        // The user disabled support for the baseline ISA so
        // don't emit any SIMD intrinsics as they all require
        // this at a minimum. We will, however, return false
        // for IsHardwareAccelerated as that will help with
        // dead code elimination.

        return (intrinsicInfo->id == SIMDIntrinsicHWAccel) ? gtNewIconNode(0, TYP_INT) : nullptr;
    }

    SIMDIntrinsicID simdIntrinsicID = intrinsicInfo->id;
    var_types       simdType;
    if (baseType != TYP_UNKNOWN)
    {
        simdType = getSIMDTypeForSize(size);
    }
    else
    {
        assert(simdIntrinsicID == SIMDIntrinsicHWAccel);
        simdType = TYP_UNKNOWN;
    }
    bool      instMethod = intrinsicInfo->isInstMethod;
    var_types callType   = JITtype2varType(sig->retType);
    if (callType == TYP_STRUCT)
    {
        // Note that here we are assuming that, if the call returns a struct, that it is the same size as the
        // struct on which the method is declared. This is currently true for all methods on Vector types,
        // but if this ever changes, we will need to determine the callType from the signature.
        assert(info.compCompHnd->getClassSize(sig->retTypeClass) == genTypeSize(simdType));
        callType = simdType;
    }

    GenTree* simdTree = nullptr;
    GenTree* op1      = nullptr;
    GenTree* op2      = nullptr;
    GenTree* op3      = nullptr;
    GenTree* retVal   = nullptr;

    switch (simdIntrinsicID)
    {
        case SIMDIntrinsicInit:
        {
            // SIMDIntrinsicInit:
            //    op2 - the initializer value
            //    op1 - byref of vector
            op2 = impPopStackCoerceArg(varActualType(baseType));
            op1 = getOp1ForConstructor(opcode, newobjThis, clsHnd);

            assert(op1->TypeGet() == TYP_BYREF);
            assert(genActualType(op2->TypeGet()) == genActualType(baseType));

            if (op2->IsIntegralConst(0) || op2->IsDblConPositiveZero())
            {
                simdTree = gtNewSimdAsHWIntrinsicNode(simdType, NI_Vector128_get_Zero, baseType, size);
            }
            else if (varTypeIsSmallInt(baseType))
            {
                // For integral base types of size less than TYP_INT, expand the initializer
                // to fill size of TYP_INT bytes.

                unsigned baseSize = genTypeSize(baseType);
                int      multiplier;
                if (baseSize == 1)
                {
                    multiplier = 0x01010101;
                }
                else
                {
                    assert(baseSize == 2);
                    multiplier = 0x00010001;
                }

                GenTree* t1 = nullptr;
                if (baseType == TYP_BYTE)
                {
                    // What we have is a signed byte initializer,
                    // which when loaded to a reg will get sign extended to TYP_INT.
                    // But what we need is the initializer without sign extended or
                    // rather zero extended to 32-bits.
                    t1 = gtNewOperNode(GT_AND, TYP_INT, op2, gtNewIconNode(0xff, TYP_INT));
                }
                else if (baseType == TYP_SHORT)
                {
                    // What we have is a signed short initializer,
                    // which when loaded to a reg will get sign extended to TYP_INT.
                    // But what we need is the initializer without sign extended or
                    // rather zero extended to 32-bits.
                    t1 = gtNewOperNode(GT_AND, TYP_INT, op2, gtNewIconNode(0xffff, TYP_INT));
                }
                else
                {
                    assert(baseType == TYP_UBYTE || baseType == TYP_USHORT);
                    t1 = gtNewCastNode(TYP_INT, op2, false, TYP_INT);
                }

                assert(t1 != nullptr);
                GenTree* t2 = gtNewIconNode(multiplier, TYP_INT);
                op2         = gtNewOperNode(GT_MUL, TYP_INT, t1, t2);

                // Construct a vector of TYP_INT with the new initializer and cast it back to vector of baseType
                simdTree = gtNewSIMDNode(simdType, SIMDIntrinsicInit, TYP_INT, size, op2);
                simdTree = gtNewSIMDNode(simdType, SIMDIntrinsicCast, baseType, size, simdTree);
            }
            else
            {
                simdTree = gtNewSIMDNode(simdType, SIMDIntrinsicInit, baseType, size, op2);
            }

            retVal = impAssignSIMDAddr(op1, simdTree);
        }
        break;

        case SIMDIntrinsicInitN:
        {
            // SIMDIntrinsicInitN
            //    op2 - list of initializer values stitched into a list
            //    op1 - byref of vector
            assert(baseType == TYP_FLOAT);

            unsigned initCount    = argCount - 1;
            unsigned elementCount = getSIMDVectorLength(size, baseType);
            noway_assert(initCount == elementCount);

            // We must maintain left-to-right order of the args, but we will pop
            // them off in reverse order (the Nth arg was pushed onto the stack last).

            GenTree* args[SIMD_INTRINSIC_MAX_PARAM_COUNT - 1];

            bool areArgsContiguous = true;
            for (unsigned i = 0; i < initCount; i++)
            {
                args[initCount - 1 - i] = impPopStackCoerceArg(baseType);

                if (areArgsContiguous && (i > 0))
                {
                    // Recall that we are popping the args off the stack in reverse order.
                    areArgsContiguous = SIMDCoalescingBuffer::AreContiguousMemoryLocations(args[initCount - 1 - i],
                                                                                           args[initCount - 1 - i + 1]);
                }
            }

            op1 = getOp1ForConstructor(opcode, newobjThis, clsHnd);
            assert(op1->TypeGet() == TYP_BYREF);

            if (areArgsContiguous)
            {
                SIMDCoalescingBuffer::ChangeToSIMDMem(this, args[0], simdType);

                simdTree = args[0];

                if (op1->AsOp()->gtOp1->OperIsLocal())
                {
                    // label the dst struct's lclvar is used for SIMD intrinsic,
                    // so that this dst struct won't be promoted.
                    setLclRelatedToSIMDIntrinsic(op1->AsOp()->gtOp1);
                }
            }
            else
            {
                simdTree = gtNewSIMDNode(simdType, SIMDIntrinsicInitN, baseType, size, initCount, args);
            }

            retVal = impAssignSIMDAddr(op1, simdTree);
        }
        break;

        case SIMDIntrinsicInitArray:
        case SIMDIntrinsicInitArrayX:
        case SIMDIntrinsicCopyToArray:
        case SIMDIntrinsicCopyToArrayX:
        {
            // op3 - index into array in case of SIMDIntrinsicCopyToArrayX and SIMDIntrinsicInitArrayX
            // op2 - array itself
            // op1 - byref to vector struct

            unsigned int vectorLength = getSIMDVectorLength(size, baseType);
            // (This constructor takes only the zero-based arrays.)
            // We will add one or two bounds checks:
            // 1. If we have an index, we must do a check on that first.
            //    We can't combine it with the index + vectorLength check because
            //    a. It might be negative, and b. It may need to raise a different exception
            //    (captured as SCK_ARG_RNG_EXCPN for CopyTo and SCK_RNGCHK_FAIL for Init).
            // 2. We need to generate a check (SCK_ARG_EXCPN for CopyTo and SCK_RNGCHK_FAIL for Init)
            //    for the last array element we will access.
            //    We'll either check against (vectorLength - 1) or (index + vectorLength - 1).

            GenTree* checkIndexExpr = new (this, GT_CNS_INT) GenTreeIntCon(TYP_INT, vectorLength - 1);

            // Get the index into the array.  If it has been provided, it will be on the
            // top of the stack.  Otherwise, it is null.
            if (argCount == 3)
            {
                op3 = impPopStackCoerceArg(TYP_INT);

                if (op3->IsIntegralConst(0))
                {
                    op3 = nullptr;
                }
            }
            else
            {
                // TODO-CQ: Here, or elsewhere, check for the pattern where op2 is a newly constructed array, and
                // change this to the InitN form.
                // op3 = new (this, GT_CNS_INT) GenTreeIntCon(TYP_INT, 0);
                op3 = nullptr;
            }

            op2 = impPopStackCoerceArg(TYP_REF);

            // Clone the array for use in the bounds check.
            GenTree* arrayRefForArgChk = op2;
            GenTree* argRngChk         = nullptr;
            if ((arrayRefForArgChk->gtFlags & GTF_SIDE_EFFECT) != 0)
            {
                op2 = fgInsertCommaFormTemp(&arrayRefForArgChk);
            }
            else
            {
                op2 = gtCloneExpr(arrayRefForArgChk);
            }
            assert(op2 != nullptr);

            if (op3 != nullptr)
            {
                SpecialCodeKind op3CheckKind;
                if (simdIntrinsicID == SIMDIntrinsicInitArrayX)
                {
                    op3CheckKind = SCK_RNGCHK_FAIL;
                }
                else
                {
                    assert(simdIntrinsicID == SIMDIntrinsicCopyToArrayX);
                    op3CheckKind = SCK_ARG_RNG_EXCPN;
                }
                // We need to use the original expression on this, which is the first check.
                GenTree* arrayRefForArgRngChk = arrayRefForArgChk;
                // Then we clone the clone we just made for the next check.
                arrayRefForArgChk = gtCloneExpr(op2);
                // We know we MUST have had a cloneable expression.
                assert(arrayRefForArgChk != nullptr);
                GenTree* index = op3;
                if ((index->gtFlags & GTF_SIDE_EFFECT) != 0)
                {
                    op3 = fgInsertCommaFormTemp(&index);
                }
                else
                {
                    op3 = gtCloneExpr(index);
                }

                GenTreeArrLen* arrLen = gtNewArrLen(arrayRefForArgRngChk, OFFSETOF__CORINFO_Array__length, compCurBB);
                argRngChk             = gtNewArrBoundsChk(index, arrLen, op3CheckKind);
                // Now, clone op3 to create another node for the argChk
                GenTree* index2 = gtCloneExpr(op3);
                assert(index != nullptr);
                checkIndexExpr = gtNewOperNode(GT_ADD, TYP_INT, index2, checkIndexExpr);
            }

            // Insert a bounds check for index + offset - 1.
            // This must be a "normal" array.
            SpecialCodeKind op2CheckKind;
            if (simdIntrinsicID == SIMDIntrinsicInitArray || simdIntrinsicID == SIMDIntrinsicInitArrayX)
            {
                op2CheckKind = SCK_RNGCHK_FAIL;
            }
            else
            {
                op2CheckKind = SCK_ARG_EXCPN;
            }
            GenTreeArrLen*    arrLen = gtNewArrLen(arrayRefForArgChk, OFFSETOF__CORINFO_Array__length, compCurBB);
            GenTreeBoundsChk* argChk = gtNewArrBoundsChk(checkIndexExpr, arrLen, op2CheckKind);

            // Create a GT_COMMA tree for the bounds check(s).
            op2 = gtNewOperNode(GT_COMMA, op2->TypeGet(), argChk, op2);
            if (argRngChk != nullptr)
            {
                op2 = gtNewOperNode(GT_COMMA, op2->TypeGet(), argRngChk, op2);
            }

            if (op3 == nullptr)
            {
                op3 = gtNewIconNode(OFFSETOF__CORINFO_Array__data, TYP_I_IMPL);
            }
            else
            {
#ifdef TARGET_64BIT
                op3 = gtNewCastNode(TYP_I_IMPL, op3, false, TYP_I_IMPL);
#endif
                op3 = gtNewOperNode(GT_MUL, TYP_I_IMPL, op3, gtNewIconNode(genTypeSize(baseType), TYP_I_IMPL));
                // TODO-MIKE-CQ: This should be removed, it's here only to minimize diffs
                // from the previous implementation that imported SIMDIntrinsicInitArray
                // as is, hiding the address mode and thus blocking CSE.
                op3->gtFlags |= GTF_DONT_CSE;
                op3 = gtNewOperNode(GT_ADD, TYP_I_IMPL, op3, gtNewIconNode(OFFSETOF__CORINFO_Array__data, TYP_I_IMPL));
                op3->gtFlags |= GTF_DONT_CSE;
            }

            op2 = gtNewOperNode(GT_ADD, TYP_BYREF, op2, op3);
            op2->gtFlags |= GTF_DONT_CSE;

            if ((simdIntrinsicID == SIMDIntrinsicInitArray) || (simdIntrinsicID == SIMDIntrinsicInitArrayX))
            {
                op1      = getOp1ForConstructor(opcode, newobjThis, clsHnd);
                simdTree = gtNewOperNode(GT_IND, simdType, op2);
                simdTree->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;
                retVal = impAssignSIMDAddr(op1, simdTree);
            }
            else
            {
                assert((simdIntrinsicID == SIMDIntrinsicCopyToArray) || (simdIntrinsicID == SIMDIntrinsicCopyToArrayX));
                assert(instMethod);

                op1 = impSIMDPopStackAddr(simdType);
                assert(op1->TypeGet() == simdType);
                retVal = gtNewOperNode(GT_IND, simdType, op2);
                retVal->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;
                retVal = gtNewAssignNode(retVal, op1);
            }
        }
        break;

        case SIMDIntrinsicInitFixed:
        {
            // We are initializing a fixed-length vector VLarge with a smaller fixed-length vector VSmall, plus 1 or 2
            // additional floats.
            //    op4 (optional) - float value for VLarge.W, if VLarge is Vector4, and VSmall is Vector2
            //    op3 - float value for VLarge.Z or VLarge.W
            //    op2 - VSmall
            //    op1 - byref of VLarge
            assert(baseType == TYP_FLOAT);

            GenTree* op4 = nullptr;
            if (argCount == 4)
            {
                op4 = impPopStackCoerceArg(TYP_FLOAT);
            }

            op3 = impPopStackCoerceArg(TYP_FLOAT);

            // The input vector will either be TYP_SIMD8 or TYP_SIMD12.
            var_types smallSIMDType = TYP_SIMD8;
            if ((op4 == nullptr) && (simdType == TYP_SIMD16))
            {
                smallSIMDType = TYP_SIMD12;
            }
            op2 = impSIMDPopStack(smallSIMDType);
            op1 = getOp1ForConstructor(opcode, newobjThis, clsHnd);

            // We are going to redefine the operands so that:
            // - op3 is the value that's going into the Z position, or null if it's a Vector4 constructor with a single
            // operand, and
            // - op4 is the W position value, or null if this is a Vector3 constructor.
            if (size == 16 && argCount == 3)
            {
                op4 = op3;
                op3 = nullptr;
            }

            simdTree = op2;
            if (op3 != nullptr)
            {
                simdTree = gtNewSIMDNode(simdType, SIMDIntrinsicSetZ, baseType, size, simdTree, op3);
            }
            if (op4 != nullptr)
            {
                simdTree = gtNewSIMDNode(simdType, SIMDIntrinsicSetW, baseType, size, simdTree, op4);
            }

            retVal = impAssignSIMDAddr(op1, simdTree);
        }
        break;

        case SIMDIntrinsicGetItem:
        {
            assert(instMethod);
            // op1 is a SIMD variable that is "this" arg
            // op2 is an index of TYP_INT
            op2              = impPopStackCoerceArg(TYP_INT);
            op1              = impSIMDPopStackAddr(simdType);
            int vectorLength = getSIMDVectorLength(size, baseType);
            if (!op2->IsCnsIntOrI() || op2->AsIntCon()->gtIconVal >= vectorLength || op2->AsIntCon()->gtIconVal < 0)
            {
                // We need to bounds-check the length of the vector.
                // For that purpose, we need to clone the index expression.
                GenTree* index = op2;
                if ((index->gtFlags & GTF_SIDE_EFFECT) != 0)
                {
                    op2 = fgInsertCommaFormTemp(&index);
                }
                else
                {
                    op2 = gtCloneExpr(index);
                }

                // For the non-constant case, we don't want to CSE the SIMD value, as we will just need to store
                // it to the stack to do the indexing anyway.
                op1->gtFlags |= GTF_DONT_CSE;

                GenTree*          lengthNode = new (this, GT_CNS_INT) GenTreeIntCon(TYP_INT, vectorLength);
                GenTreeBoundsChk* simdChk =
                    new (this, GT_SIMD_CHK) GenTreeBoundsChk(GT_SIMD_CHK, index, lengthNode, SCK_RNGCHK_FAIL);

                // Create a GT_COMMA tree for the bounds check.
                op2 = gtNewOperNode(GT_COMMA, op2->TypeGet(), simdChk, op2);
            }

            assert(op1->TypeGet() == simdType);
            assert(op2->TypeGet() == TYP_INT);

            simdTree = gtNewSIMDNode(genActualType(callType), simdIntrinsicID, baseType, size, op1, op2);
            retVal   = simdTree;
        }
        break;

        // Unary operators that take and return a Vector.
        case SIMDIntrinsicCast:
        case SIMDIntrinsicConvertToSingle:
        case SIMDIntrinsicConvertToDouble:
        case SIMDIntrinsicConvertToInt32:
        {
            assert(!instMethod);
            op1 = impSIMDPopStack(simdType);

            simdTree = gtNewSIMDNode(simdType, simdIntrinsicID, baseType, size, op1);
            retVal   = simdTree;
        }
        break;

        case SIMDIntrinsicConvertToInt64:
        {
            assert(!instMethod);
#ifdef TARGET_64BIT
            op1 = impSIMDPopStack(simdType);

            simdTree = gtNewSIMDNode(simdType, simdIntrinsicID, baseType, size, op1);
            retVal   = simdTree;
#else
            JITDUMP("SIMD Conversion to Int64 is not supported on this platform\n");
            return nullptr;
#endif
        }
        break;

        case SIMDIntrinsicNarrow:
        {
            assert(!instMethod);
            op2 = impSIMDPopStack(simdType);
            op1 = impSIMDPopStack(simdType);
            // op1 and op2 are two input Vector<T>.
            simdTree = gtNewSIMDNode(simdType, simdIntrinsicID, baseType, size, op1, op2);
            retVal   = simdTree;
        }
        break;

        case SIMDIntrinsicWiden:
        {
            GenTree* dstAddrHi = impPopStackCoerceArg(TYP_BYREF);
            GenTree* dstAddrLo = impPopStackCoerceArg(TYP_BYREF);
            GenTree* op1       = impSIMDPopStack(simdType);

            GenTree* uses[2];
            impMakeMultiUse(op1, uses, typGetObjLayout(clsHnd), CHECK_SPILL_ALL DEBUGARG("Vector<T>.Widen temp"));

            GenTree* wideLo = gtNewSIMDNode(simdType, SIMDIntrinsicWidenLo, baseType, size, uses[0]);
            GenTree* asgLo  = impAssignSIMDAddr(dstAddrLo, wideLo);
            GenTree* wideHi = gtNewSIMDNode(simdType, SIMDIntrinsicWidenHi, baseType, size, uses[1]);
            GenTree* asgHi  = impAssignSIMDAddr(dstAddrHi, wideHi);
            retVal          = gtNewOperNode(GT_COMMA, simdType, asgLo, asgHi);
        }
        break;

        case SIMDIntrinsicHWAccel:
        {
            GenTreeIntCon* intConstTree = new (this, GT_CNS_INT) GenTreeIntCon(TYP_INT, 1);
            retVal                      = intConstTree;
        }
        break;

        default:
            assert(!"Unimplemented SIMD Intrinsic");
            return nullptr;
    }

#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
    // XArch/Arm64: also indicate that we use floating point registers.
    // The need for setting this here is that a method may not have SIMD
    // type lclvars, but might be exercising SIMD intrinsics on fields of
    // SIMD type.
    //
    // e.g.  public Vector<float> ComplexVecFloat::sqabs() { return this.r * this.r + this.i * this.i; }
    compFloatingPointUsed = true;
#endif // defined(TARGET_XARCH) || defined(TARGET_ARM64)

    return retVal;
}

GenTreeOp* Compiler::impAssignSIMDAddr(GenTree* destAddr, GenTree* src)
{
    assert(destAddr->TypeIs(TYP_BYREF, TYP_I_IMPL));
    assert(src->OperIs(GT_SIMD, GT_IND, GT_HWINTRINSIC));
    assert(varTypeIsSIMD(src->GetType()));

    // TODO-MIKE-CQ: This should be removed, it's here only to minimize diffs
    // from the previous implementation that did this (in gtNewBlkOpNode).
    src->gtFlags |= GTF_DONT_CSE;

    GenTree* dest;

    if (destAddr->OperIs(GT_ADDR) && destAddr->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR) &&
        (destAddr->AsUnOp()->GetOp(0)->GetType() == src->GetType()))
    {
        dest = destAddr->AsUnOp()->GetOp(0);

        if (src->OperIs(GT_SIMD, GT_HWINTRINSIC))
        {
            setLclRelatedToSIMDIntrinsic(dest->AsLclVar());
        }

        assert(lvaGetDesc(dest->AsLclVar())->GetType() == src->GetType());
    }
    else
    {
        dest = gtNewIndir(src->GetType(), destAddr);
        dest->gtFlags |= GTF_GLOB_REF;
    }

    return gtNewAssignNode(dest, src);
}

#endif // FEATURE_SIMD
