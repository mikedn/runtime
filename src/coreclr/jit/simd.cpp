// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "simd.h"

#ifdef _MSC_VER
#pragma hdrstop
#endif

#ifdef FEATURE_SIMD

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
        impAppendTempAssign(tmpNum, tree, layout, CHECK_SPILL_ALL);
        tree = gtNewLclvNode(tmpNum, lvaGetDesc(tmpNum)->GetType());
    }

    assert(tree->GetType() == type);

    return tree;
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

        addr   = compiler->gtNewCommaNode(arrBndsChk, array);
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
GenTreeLclVar* Compiler::SIMDCoalescingBuffer::IsSIMDGetElement(GenTree* node)
{
    if (!node->OperIs(GT_HWINTRINSIC))
    {
        return nullptr;
    }

    GenTreeHWIntrinsic* getItem = node->AsHWIntrinsic();

    if ((getItem->GetIntrinsic() != NI_Vector128_GetElement) || !getItem->GetOp(0)->OperIs(GT_LCL_VAR) ||
        !getItem->GetOp(1)->IsIntegralConst(m_index))
    {
        return nullptr;
    }

    // We only care about Vector2/3/4 so the element type is always FLOAT.
    if (!getItem->TypeIs(TYP_FLOAT))
    {
        return nullptr;
    }

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

    GenTreeLclVar* simdLclVar = IsSIMDGetElement(asg->AsOp()->GetOp(1));

    return Add(compiler, stmt, asg->AsOp(), simdLclVar);
}

// Transform the first assignment in the buffer into a SIMD assignment
// and remove the rest of the statements from the block.
//
void Compiler::SIMDCoalescingBuffer::Coalesce(Compiler* compiler, BasicBlock* block)
{
    assert(m_index > 1);

    GenTreeOp*          asg        = m_firstStmt->GetRootNode()->AsOp();
    GenTreeHWIntrinsic* getElement = asg->GetOp(1)->AsHWIntrinsic();
    var_types           simdType   = getSIMDTypeForSize(getElement->GetSimdSize());

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
    asg->SetOp(1, getElement->GetOp(0)->AsLclVar());

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

#endif // FEATURE_SIMD
