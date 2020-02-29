// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#include "jitpch.h"

class LocalAddressVisitor final : public GenTreeVisitor<LocalAddressVisitor>
{
    // During tree traversal every GenTree node produces a "value" that represents:
    //   - the memory location associated with a local variable, including an offset
    //     accumulated from GT_LCL_FLD and GT_FIELD nodes.
    //   - the address of local variable memory location, including an offset as well.
    //   - an unknown value - the result of a node we don't know how to process. This
    //     also includes the result of TYP_VOID nodes (or any other nodes that don't
    //     actually produce values in IR) in order to support the invariant that every
    //     node produces a value.
    //
    // The existence of GT_ADDR nodes and their use together with GT_FIELD to form
    // FIELD/ADDR/FIELD/ADDR/LCL_VAR sequences complicate things a bit. A typical
    // GT_FIELD node acts like an indirection and should produce an unknown value,
    // local address analysis doesn't know or care what value the field stores.
    // But a GT_FIELD can also be used as an operand for a GT_ADDR node and then
    // the GT_FIELD node does not perform an indirection, it's just represents a
    // location, similar to GT_LCL_VAR and GT_LCL_FLD.
    //
    // To avoid this issue, the semantics of GT_FIELD (and for simplicity's sake any other
    // indirection) nodes slightly deviates from the IR semantics - an indirection does not
    // actually produce an unknown value but a location value, if the indirection address
    // operand is an address value.
    //
    // The actual indirection is performed when the indirection's user node is processed:
    //   - A GT_ADDR user turns the location value produced by the indirection back
    //     into an address value.
    //   - Any other user node performs the indirection and produces an unknown value.
    //
    class Value
    {
        GenTree*      m_node;
        FieldSeqNode* m_fieldSeq;
        unsigned      m_lclNum;
        unsigned      m_offset;
        bool          m_address;
        INDEBUG(bool m_consumed;)

    public:
        // Produce an unknown value associated with the specified node.
        Value(GenTree* node)
            : m_node(node)
            , m_fieldSeq(nullptr)
            , m_lclNum(BAD_VAR_NUM)
            , m_offset(0)
            , m_address(false)
#ifdef DEBUG
            , m_consumed(false)
#endif // DEBUG
        {
        }

        // Get the node that produced this value.
        GenTree* Node() const
        {
            return m_node;
        }

        // Does this value represent a location?
        bool IsLocation() const
        {
            return (m_lclNum != BAD_VAR_NUM) && !m_address;
        }

        // Does this value represent the address of a location?
        bool IsAddress() const
        {
            assert((m_lclNum != BAD_VAR_NUM) || !m_address);

            return m_address;
        }

        // Get the location's variable number.
        unsigned LclNum() const
        {
            assert(IsLocation() || IsAddress());

            return m_lclNum;
        }

        // Get the location's byte offset.
        unsigned Offset() const
        {
            assert(IsLocation() || IsAddress());

            return m_offset;
        }

        // Get the location's field sequence.
        FieldSeqNode* FieldSeq() const
        {
            return m_fieldSeq;
        }

        //------------------------------------------------------------------------
        // Location: Produce a location value.
        //
        // Arguments:
        //    lclVar - a GT_LCL_VAR node that defines the location
        //
        // Notes:
        //   - (lclnum) => LOCATION(lclNum, 0)
        //
        void Location(GenTreeLclVar* lclVar)
        {
            assert(lclVar->OperIs(GT_LCL_VAR));
            assert(!IsLocation() && !IsAddress());

            m_lclNum = lclVar->GetLclNum();

            assert(m_offset == 0);
            assert(m_fieldSeq == nullptr);
        }

        //------------------------------------------------------------------------
        // Address: Produce an address value from a GT_LCL_VAR_ADDR node.
        //
        // Arguments:
        //    lclVar - a GT_LCL_VAR_ADDR node that defines the address
        //
        // Notes:
        //   - (lclnum) => ADDRESS(lclNum, 0)
        //
        void Address(GenTreeLclVar* lclVar)
        {
            assert(lclVar->OperIs(GT_LCL_VAR_ADDR));
            assert(!IsLocation() && !IsAddress());

            m_lclNum  = lclVar->GetLclNum();
            m_address = true;

            assert(m_offset == 0);
            assert(m_fieldSeq == nullptr);
        }

        //------------------------------------------------------------------------
        // Location: Produce a location value.
        //
        // Arguments:
        //    lclFld - a GT_LCL_FLD node that defines the location
        //
        // Notes:
        //   - (lclnum, lclOffs) => LOCATION(lclNum, offset)
        //
        void Location(GenTreeLclFld* lclFld)
        {
            assert(lclFld->OperIs(GT_LCL_FLD));
            assert(!IsLocation() && !IsAddress());

            m_lclNum   = lclFld->GetLclNum();
            m_offset   = lclFld->GetLclOffs();
            m_fieldSeq = lclFld->GetFieldSeq();
        }

        //------------------------------------------------------------------------
        // Address: Produce an address value from a LCL_FLD_ADDR node.
        //
        // Arguments:
        //    lclFld - a GT_LCL_FLD_ADDR node that defines the address
        //
        // Notes:
        //   - (lclnum, lclOffs) => ADDRESS(lclNum, offset)
        //
        void Address(GenTreeLclFld* lclFld)
        {
            assert(lclFld->OperIs(GT_LCL_FLD_ADDR));
            assert(!IsLocation() && !IsAddress());

            m_lclNum   = lclFld->GetLclNum();
            m_offset   = lclFld->GetLclOffs();
            m_fieldSeq = lclFld->GetFieldSeq();
            m_address  = true;
        }

        //------------------------------------------------------------------------
        // Address: Produce an address value from a location value.
        //
        // Arguments:
        //    val - the input value
        //
        // Notes:
        //   - LOCATION(lclNum, offset) => ADDRESS(lclNum, offset)
        //   - ADDRESS(lclNum, offset) => invalid, we should never encounter something like ADDR(ADDR(...))
        //   - UNKNOWN => UNKNOWN
        //
        void Address(Value& val)
        {
            assert(!IsLocation() && !IsAddress());
            assert(!val.IsAddress());

            if (val.IsLocation())
            {
                m_address  = true;
                m_lclNum   = val.m_lclNum;
                m_offset   = val.m_offset;
                m_fieldSeq = val.m_fieldSeq;
            }

            INDEBUG(val.Consume();)
        }

        //------------------------------------------------------------------------
        // Field: Produce a location value from an address value.
        //
        // Arguments:
        //    val - the input value
        //    field - the FIELD node that uses the input address value
        //    fieldSeqStore - the compiler's field sequence store
        //
        // Return Value:
        //    `true` if the value was consumed. `false` if the input value
        //    cannot be consumed because it is itsef a location or because
        //    the offset overflowed. In this case the caller is expected
        //    to escape the input value.
        //
        // Notes:
        //   - LOCATION(lclNum, offset) => not representable, must escape
        //   - ADDRESS(lclNum, offset) => LOCATION(lclNum, offset + field.Offset)
        //     if the offset overflows then location is not representable, must escape
        //   - UNKNOWN => UNKNOWN
        //
        bool Field(Value& val, GenTreeField* field, FieldSeqStore* fieldSeqStore)
        {
            assert(!IsLocation() && !IsAddress());

            if (val.IsLocation())
            {
                return false;
            }

            if (val.IsAddress())
            {
                ClrSafeInt<unsigned> newOffset =
                    ClrSafeInt<unsigned>(val.m_offset) + ClrSafeInt<unsigned>(field->gtFldOffset);

                if (newOffset.IsOverflow())
                {
                    return false;
                }

                m_lclNum = val.m_lclNum;
                m_offset = newOffset.Value();

                if (field->gtFldMayOverlap)
                {
                    m_fieldSeq = FieldSeqStore::NotAField();
                }
                else
                {
                    m_fieldSeq = fieldSeqStore->Append(val.m_fieldSeq, fieldSeqStore->CreateSingleton(field->gtFldHnd));
                }
            }

            INDEBUG(val.Consume();)
            return true;
        }

        //------------------------------------------------------------------------
        // Indir: Produce a location value from an address value.
        //
        // Arguments:
        //    val - the input value
        //
        // Return Value:
        //    `true` if the value was consumed. `false` if the input value
        //    cannot be consumed because it is itsef a location. In this
        //    case the caller is expected to escape the input value.
        //
        // Notes:
        //   - LOCATION(lclNum, offset) => not representable, must escape
        //   - ADDRESS(lclNum, offset) => LOCATION(lclNum, offset)
        //   - UNKNOWN => UNKNOWN
        //
        bool Indir(Value& val)
        {
            assert(!IsLocation() && !IsAddress());

            if (val.IsLocation())
            {
                return false;
            }

            if (val.IsAddress())
            {
                m_lclNum   = val.m_lclNum;
                m_offset   = val.m_offset;
                m_fieldSeq = val.m_fieldSeq;
            }

            INDEBUG(val.Consume();)
            return true;
        }

#ifdef DEBUG
        void Consume()
        {
            assert(!m_consumed);
            // Mark the value as consumed so that PopValue can ensure that values
            // aren't popped from the stack without being processed appropriately.
            m_consumed = true;
        }

        bool IsConsumed()
        {
            return m_consumed;
        }
#endif // DEBUG
    };

    ArrayStack<Value> m_valueStack;
    INDEBUG(bool m_stmtModified;)

public:
    enum
    {
        DoPreOrder        = true,
        DoPostOrder       = true,
        ComputeStack      = true,
        DoLclVarsOnly     = false,
        UseExecutionOrder = false,
    };

    LocalAddressVisitor(Compiler* comp)
        : GenTreeVisitor<LocalAddressVisitor>(comp), m_valueStack(comp->getAllocator(CMK_LocalAddressVisitor))
    {
    }

    void VisitStmt(Statement* stmt)
    {
#ifdef DEBUG
        if (m_compiler->verbose)
        {
            printf("LocalAddressVisitor visiting statement:\n");
            m_compiler->gtDispStmt(stmt);
            m_stmtModified = false;
        }
#endif // DEBUG

        WalkTree(stmt->GetRootNodePointer(), nullptr);

        // We could have something a statement like IND(ADDR(LCL_VAR)) so we need to escape
        // the location here. This doesn't seem to happen often, if ever. The importer
        // tends to wrap such a tree in a COMMA.
        if (TopValue(0).IsLocation())
        {
            EscapeLocation(TopValue(0), nullptr);
        }
        else
        {
            // If we have an address on the stack then we don't need to do anything.
            // The address tree isn't actually used and it will be discarded during
            // morphing. So just mark any value as consumed to keep PopValue happy.
            INDEBUG(TopValue(0).Consume();)
        }

        PopValue();
        assert(m_valueStack.Empty());

#ifdef DEBUG
        if (m_compiler->verbose)
        {
            if (m_stmtModified)
            {
                printf("LocalAddressVisitor modified statement:\n");
                m_compiler->gtDispStmt(stmt);
            }

            printf("\n");
        }
#endif // DEBUG
    }

    // Morph promoted struct fields and count implict byref argument occurrences.
    // Also create and push the value produced by the visited node. This is done here
    // rather than in PostOrderVisit because it makes it easy to handle nodes with an
    // arbitrary number of operands - just pop values until the value corresponding
    // to the visited node is encountered.
    fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
    {
        GenTree* node = *use;

        if (node->OperIs(GT_FIELD))
        {
            MorphStructField(node, user);
        }
        else if (node->OperIs(GT_LCL_FLD))
        {
            MorphLocalField(node, user);
        }

        if (node->OperIsLocal())
        {
            unsigned lclNum = node->AsLclVarCommon()->GetLclNum();

            LclVarDsc* varDsc = m_compiler->lvaGetDesc(lclNum);
            if (varDsc->lvIsStructField)
            {
                // Promoted field, increase counter for the parent lclVar.
                assert(!m_compiler->lvaIsImplicitByRefLocal(lclNum));
                unsigned parentLclNum = varDsc->lvParentLcl;
                UpdateEarlyRefCountForImplicitByRef(parentLclNum);
            }
            else
            {
                UpdateEarlyRefCountForImplicitByRef(lclNum);
            }
        }

        PushValue(node);

        return Compiler::WALK_CONTINUE;
    }

    // Evaluate a node. Since this is done in postorder, the node's operands have already been
    // evaluated and are available on the value stack. The value produced by the visited node
    // is left on the top of the evaluation stack.
    fgWalkResult PostOrderVisit(GenTree** use, GenTree* user)
    {
        GenTree* node = *use;

        switch (node->OperGet())
        {
            case GT_LCL_VAR:
                assert(TopValue(0).Node() == node);

                TopValue(0).Location(node->AsLclVar());
                break;

            case GT_LCL_VAR_ADDR:
                assert(TopValue(0).Node() == node);

                TopValue(0).Address(node->AsLclVar());
                break;

            case GT_LCL_FLD:
                assert(TopValue(0).Node() == node);

                TopValue(0).Location(node->AsLclFld());
                break;

            case GT_LCL_FLD_ADDR:
                assert(TopValue(0).Node() == node);

                TopValue(0).Address(node->AsLclFld());
                break;

            case GT_ADDR:
                assert(TopValue(1).Node() == node);
                assert(TopValue(0).Node() == node->gtGetOp1());

                TopValue(1).Address(TopValue(0));
                PopValue();
                break;

            case GT_FIELD:
                if (node->AsField()->gtFldObj != nullptr)
                {
                    assert(TopValue(1).Node() == node);
                    assert(TopValue(0).Node() == node->AsField()->gtFldObj);

                    if (!TopValue(1).Field(TopValue(0), node->AsField(), m_compiler->GetFieldSeqStore()))
                    {
                        // Either the address comes from a location value (e.g. FIELD(IND(...)))
                        // or the field offset has overflowed.
                        EscapeValue(TopValue(0), node);
                    }

                    PopValue();
                }
                else
                {
                    assert(TopValue(0).Node() == node);
                }
                break;

            case GT_OBJ:
            case GT_BLK:
            case GT_IND:
                assert(TopValue(1).Node() == node);
                assert(TopValue(0).Node() == node->gtGetOp1());

                if ((node->gtFlags & GTF_IND_VOLATILE) != 0)
                {
                    // Volatile indirections must not be removed so the address,
                    // if any, must be escaped.
                    EscapeValue(TopValue(0), node);
                }
                else if (!TopValue(1).Indir(TopValue(0)))
                {
                    // If the address comes from another indirection (e.g. IND(IND(...))
                    // then we need to escape the location.
                    EscapeLocation(TopValue(0), node);
                }

                PopValue();
                break;

            case GT_DYN_BLK:
                assert(TopValue(2).Node() == node);
                assert(TopValue(1).Node() == node->AsDynBlk()->Addr());
                assert(TopValue(0).Node() == node->AsDynBlk()->gtDynamicSize);

                // The block size may be the result of an indirection so we need
                // to escape the location that may be associated with it.
                EscapeValue(TopValue(0), node);

                if (!TopValue(2).Indir(TopValue(1)))
                {
                    // If the address comes from another indirection (e.g. DYN_BLK(IND(...))
                    // then we need to escape the location.
                    EscapeLocation(TopValue(1), node);
                }

                PopValue();
                PopValue();
                break;

            default:
                while (TopValue(0).Node() != node)
                {
                    EscapeValue(TopValue(0), node);
                    PopValue();
                }
                break;
        }

        assert(TopValue(0).Node() == node);
        return Compiler::WALK_CONTINUE;
    }

private:
    void PushValue(GenTree* node)
    {
        m_valueStack.Push(node);
    }

    Value& TopValue(unsigned index)
    {
        return m_valueStack.TopRef(index);
    }

    void PopValue()
    {
        assert(TopValue(0).IsConsumed());
        m_valueStack.Pop();
    }

    //------------------------------------------------------------------------
    // EscapeValue: Process an escaped value
    //
    // Arguments:
    //    val - the escaped address value
    //    user - the node that uses the escaped value
    //
    void EscapeValue(Value& val, GenTree* user)
    {
        if (val.IsLocation())
        {
            EscapeLocation(val, user);
        }
        else if (val.IsAddress())
        {
            EscapeAddress(val, user);
        }
        else
        {
            INDEBUG(val.Consume();)
        }
    }

    //------------------------------------------------------------------------
    // EscapeAddress: Process an escaped address value
    //
    // Arguments:
    //    val - the escaped address value
    //    user - the node that uses the address value
    //
    void EscapeAddress(Value& val, GenTree* user)
    {
        assert(val.IsAddress());

        LclVarDsc* varDsc = m_compiler->lvaGetDesc(val.LclNum());

        // In general we don't know how an exposed struct field address will be used - it may be used to
        // access only that specific field or it may be used to access other fields in the same struct
        // be using pointer/ref arithmetic. It seems reasonable to make an exception for the "this" arg
        // of calls - it would be highly unsual for a struct member method to attempt to access memory
        // beyond "this" instance. And calling struct member methods is common enough that attempting to
        // mark the entire struct as address exposed results in CQ regressions.
        bool isThisArg = user->IsCall() && (user->AsCall()->gtCallThisArg != nullptr) &&
                         (val.Node() == user->AsCall()->gtCallThisArg->GetNode());
        bool exposeParentLcl = varDsc->lvIsStructField && !isThisArg;

        m_compiler->lvaSetVarAddrExposed(exposeParentLcl ? varDsc->lvParentLcl : val.LclNum());

#ifdef TARGET_64BIT
        // If the address of a variable is passed in a call and the allocation size of the variable
        // is 32 bits we will quirk the size to 64 bits. Some PInvoke signatures incorrectly specify
        // a ByRef to an INT32 when they actually write a SIZE_T or INT64. There are cases where
        // overwriting these extra 4 bytes corrupts some data (such as a saved register) that leads
        // to A/V. Wheras previously the JIT64 codegen did not lead to an A/V.
        if (!varDsc->lvIsParam && !varDsc->lvIsStructField && (genActualType(varDsc->TypeGet()) == TYP_INT))
        {
            // TODO-Cleanup: This should simply check if the user is a call node, not if a call ancestor exists.
            if (Compiler::gtHasCallOnStack(&m_ancestors))
            {
                varDsc->lvQuirkToLong = true;
                JITDUMP("Adding a quirk for the storage size of V%02u of type %s", val.LclNum(),
                        varTypeName(varDsc->TypeGet()));
            }
        }
#endif // TARGET_64BIT

        // TODO-ADDR: For now use LCL_VAR_ADDR and LCL_FLD_ADDR only as call arguments and assignment sources.
        // Other usages require more changes. For example, a tree like OBJ(ADD(ADDR(LCL_VAR), 4))
        // could be changed to OBJ(LCL_FLD_ADDR) but then DefinesLocalAddr does not recognize
        // LCL_FLD_ADDR (even though it does recognize LCL_VAR_ADDR).
        if (user->OperIs(GT_CALL, GT_ASG))
        {
            MorphLocalAddress(val);
        }

        INDEBUG(val.Consume();)
    }

    //------------------------------------------------------------------------
    // EscapeLocation: Process an escaped location value
    //
    // Arguments:
    //    val - the escaped location value
    //    user - the node that uses the location value
    //
    // Notes:
    //    Unlike EscapeAddress, this does not necessarily mark the lclvar associated
    //    with the value as address exposed. This is needed only if the indirection
    //    is wider than the lclvar.
    //
    void EscapeLocation(Value& val, GenTree* user)
    {
        assert(val.IsLocation());

        GenTree* node = val.Node();

        if (node->OperIs(GT_LCL_VAR, GT_LCL_FLD))
        {
            // If the location is accessed directly then we don't need to do anything.

            assert(node->AsLclVarCommon()->GetLclNum() == val.LclNum());
        }
        else
        {
            // Otherwise it must be accessed through some kind of indirection. Usually this is
            // something like IND(ADDR(LCL_VAR)), global morph will change it to GT_LCL_VAR or
            // GT_LCL_FLD so the lclvar does not need to be address exposed.
            //
            // However, it is possible for the indirection to be wider than the lclvar
            // (e.g. *(long*)&int32Var) or to have a field offset that pushes the indirection
            // past the end of the lclvar memory location. In such cases morph doesn't do
            // anything so the lclvar needs to be address exposed.
            //
            // More importantly, if the lclvar is a promoted struct field then the parent lclvar
            // also needs to be address exposed so we get dependent struct promotion. Code like
            // *(long*)&int32Var has undefined behavior and it's practically useless but reading,
            // say, 2 consecutive Int32 struct fields as Int64 has more practical value.

            LclVarDsc* varDsc    = m_compiler->lvaGetDesc(val.LclNum());
            unsigned   indirSize = GetIndirSize(node, user);
            bool       isWide;

            if (indirSize == 0)
            {
                // If we can't figure out the indirection size then treat it as a wide indirection.
                isWide = true;
            }
            else
            {
                ClrSafeInt<unsigned> endOffset = ClrSafeInt<unsigned>(val.Offset()) + ClrSafeInt<unsigned>(indirSize);

                if (endOffset.IsOverflow())
                {
                    isWide = true;
                }
                else if (varDsc->TypeGet() == TYP_STRUCT)
                {
                    isWide = (endOffset.Value() > varDsc->lvExactSize);
                }
                else
                {
                    // For small int types use the real type size, not the stack slot size.
                    // Morph does manage to transform `*(int*)&byteVar` into just byteVar where
                    // the LCL_VAR node has type TYP_INT. But such code is simply bogus and
                    // there's no reason to attempt to optimize it. It makes more sense to
                    // mark the variable address exposed in such circumstances.
                    //
                    // Same for "small" SIMD types - SIMD8/12 have 8/12 bytes, even if the
                    // stack location may have 16 bytes.
                    //
                    // For TYP_BLK variables the type size is 0 so they're always address
                    // exposed.
                    isWide = (endOffset.Value() > genTypeSize(varDsc->TypeGet()));
                }
            }

            if (isWide)
            {
                m_compiler->lvaSetVarAddrExposed(varDsc->lvIsStructField ? varDsc->lvParentLcl : val.LclNum());
            }
            else
            {
                MorphLocalIndir(val, user);
            }
        }

        INDEBUG(val.Consume();)
    }

    //------------------------------------------------------------------------
    // GetIndirSize: Return the size (in bytes) of an indirection node.
    //
    // Arguments:
    //    indir - the indirection node
    //    user - the node that uses the indirection
    //
    // Notes:
    //    This returns 0 for indirection of unknown size, typically GT_DYN_BLK.
    //    GT_IND nodes that have type TYP_STRUCT are expected to only appears
    //    on the RHS of an assignment, in which case the LHS size will be used instead.
    //    Otherwise 0 is returned as well.
    //
    unsigned GetIndirSize(GenTree* indir, GenTree* user)
    {
        assert(indir->OperIs(GT_IND, GT_OBJ, GT_BLK, GT_DYN_BLK, GT_FIELD));

        if (indir->TypeGet() != TYP_STRUCT)
        {
            return genTypeSize(indir->TypeGet());
        }

        // A struct indir that is the RHS of an assignment needs special casing:
        // - It can be a GT_IND of type TYP_STRUCT, in which case the size is given by the LHS.
        // - It can be a GT_OBJ that has a correct size, but different than the size of the LHS.
        //   The LHS size takes precedence.
        // Just take the LHS size in all cases.
        if (user != nullptr && user->OperIs(GT_ASG) && (indir == user->gtGetOp2()))
        {
            indir = user->gtGetOp1();

            if (indir->TypeGet() != TYP_STRUCT)
            {
                return genTypeSize(indir->TypeGet());
            }

            // The LHS may be a LCL_VAR/LCL_FLD, these are not indirections so we need to handle them here.
            // It can also be a GT_INDEX, this is an indirection but it never applies to lclvar addresses
            // so it needs to be handled here as well.

            switch (indir->OperGet())
            {
                case GT_LCL_VAR:
                    return m_compiler->lvaGetDesc(indir->AsLclVar())->lvExactSize;
                case GT_LCL_FLD:
                    return genTypeSize(indir->TypeGet());
                case GT_INDEX:
                    return indir->AsIndex()->gtIndElemSize;
                default:
                    break;
            }
        }

        switch (indir->OperGet())
        {
            case GT_FIELD:
                return m_compiler->info.compCompHnd->getClassSize(
                    m_compiler->info.compCompHnd->getFieldClass(indir->AsField()->gtFldHnd));
            case GT_BLK:
            case GT_OBJ:
                return indir->AsBlk()->GetLayout()->GetSize();
            default:
                assert(indir->OperIs(GT_IND, GT_DYN_BLK));
                return 0;
        }
    }

    //------------------------------------------------------------------------
    // MorphLocalAddress: Change a tree that represents a local variable address
    //    to a single LCL_VAR_ADDR or LCL_FLD_ADDR node.
    //
    // Arguments:
    //    val - a value that represents the local address
    //
    void MorphLocalAddress(const Value& val)
    {
        assert(val.IsAddress());
        assert(val.Node()->TypeIs(TYP_BYREF, TYP_I_IMPL));
        assert(m_compiler->lvaVarAddrExposed(val.LclNum()));

        LclVarDsc* varDsc = m_compiler->lvaGetDesc(val.LclNum());

        if (varDsc->lvPromoted || varDsc->lvIsStructField || m_compiler->lvaIsImplicitByRefLocal(val.LclNum()))
        {
            // TODO-ADDR: For now we ignore promoted and "implict by ref" variables,
            // they require additional changes in subsequent phases.
            return;
        }

#ifdef TARGET_X86
        if (m_compiler->info.compIsVarArgs && varDsc->lvIsParam && !varDsc->lvIsRegArg)
        {
            // TODO-ADDR: For now we ignore all stack parameters of varargs methods,
            // fgMorphStackArgForVarArgs does not handle LCL_VAR|FLD_ADDR nodes.
            return;
        }
#endif

        GenTree* addr = val.Node();

        if (val.Offset() > UINT16_MAX)
        {
            // The offset is too large to store in a LCL_FLD_ADDR node,
            // use ADD(LCL_VAR_ADDR, offset) instead.
            addr->ChangeOper(GT_ADD);
            addr->AsOp()->gtOp1 = m_compiler->gtNewLclVarAddrNode(val.LclNum());
            addr->AsOp()->gtOp2 = m_compiler->gtNewIconNode(val.Offset(), val.FieldSeq());
        }
        else if ((val.Offset() != 0) || (val.FieldSeq() != nullptr))
        {
            addr->ChangeOper(GT_LCL_FLD_ADDR);
            addr->AsLclFld()->SetLclNum(val.LclNum());
            addr->AsLclFld()->SetLclOffs(val.Offset());
            addr->AsLclFld()->SetFieldSeq(val.FieldSeq());
        }
        else
        {
            addr->ChangeOper(GT_LCL_VAR_ADDR);
            addr->AsLclVar()->SetLclNum(val.LclNum());
        }

        // Local address nodes never have side effects (nor any other flags, at least at this point).
        addr->gtFlags = 0;

        INDEBUG(m_stmtModified = true;)
    }

    //------------------------------------------------------------------------
    // MorphLocalIndir: Change a tree that represents an indirect access to a struct
    //    variable to a single LCL_VAR or LCL_FLD node.
    //
    // Arguments:
    //    val - a value that represents the local indirection
    //    user - the indirection's user node
    //
    void MorphLocalIndir(const Value& val, GenTree* user)
    {
        assert(val.IsLocation());

        GenTree* indir = val.Node();
        assert(indir->OperIs(GT_IND, GT_OBJ, GT_BLK, GT_FIELD));

        if (val.Offset() > UINT16_MAX)
        {
            // TODO-ADDR: We can't use LCL_FLD because the offset is too large but we should
            // transform the tree into IND(ADD(LCL_VAR_ADDR, offset)) instead of leaving this
            // this to fgMorphField.
            return;
        }

        if (indir->OperIs(GT_FIELD) ? indir->AsField()->IsVolatile() : indir->AsIndir()->IsVolatile())
        {
            // TODO-ADDR: We shouldn't remove the indir because it's volatile but we should
            // transform the tree into IND(LCL_VAR|FLD_ADDR) instead of leaving this to
            // fgMorphField.
            return;
        }

        LclVarDsc* varDsc = m_compiler->lvaGetDesc(val.LclNum());

        if (!varTypeIsStruct(varDsc->TypeGet()))
        {
            if ((indir->TypeGet() == varDsc->TypeGet()) && (val.Offset() == 0))
            {
                indir->ChangeOper(GT_LCL_VAR);
                indir->AsLclVar()->SetLclNum(val.LclNum());
                indir->gtFlags = 0;

                if ((user != nullptr) && user->OperIs(GT_ASG) && (user->AsOp()->gtGetOp1() == indir))
                {
                    indir->gtFlags |= GTF_VAR_DEF | GTF_DONT_CSE;
                }

                INDEBUG(m_stmtModified = true;)
            }

            // TODO-MIKE: Handle type mismatches to prevent blocking enregistration:
            //   - float/int and double/long can be handled by using BITCAST (trouble: what to do about
            //     double/long on 32 bit targets?)
            //   - integer sign mismatches can be handled by inserting a CAST node (trouble: CAST is a
            //     large node so we can't simply convert the indir node into a cast node)
            //   - many (all?) integer size mismatches can also be handled by inserting casts
            //   - non zero offsets can be handled by inserting bit shifts and casts
            // Alternatively - use LCL_FLD and delay setting DNER_LocalField until lowering, then either
            // apply the above solutions in lowering or teach codegen to handle such LCL_FLDs without
            // requiring the variable to be in memory.

            return;
        }

        if (varTypeIsSIMD(varDsc->TypeGet()))
        {
            // TODO-ADDR: Skip SIMD variables for now, fgMorphFieldAssignToSIMDIntrinsicSet
            // and others need to be updated to recognize LCL_FLDs.
            return;
        }

        if (varDsc->lvPromoted || varDsc->lvIsStructField || m_compiler->lvaIsImplicitByRefLocal(val.LclNum()))
        {
            // TODO-ADDR: For now we ignore promoted and "implict by ref" variables,
            // they require additional changes in subsequent phases
            // (e.g. fgMorphImplicitByRefArgs does not handle LCL_FLD nodes).
            return;
        }

#ifdef TARGET_X86
        if (m_compiler->info.compIsVarArgs && varDsc->lvIsParam && !varDsc->lvIsRegArg)
        {
            // TODO-ADDR: For now we ignore all stack parameters of varargs methods,
            // fgMorphStackArgForVarArgs does not handle LCL_FLD nodes.
            return;
        }
#endif

        ClassLayout*  structLayout = nullptr;
        FieldSeqNode* fieldSeq     = val.FieldSeq();

        if ((fieldSeq != nullptr) && (fieldSeq != FieldSeqStore::NotAField()))
        {
            // TODO-ADDR: ObjectAllocator produces FIELD nodes with FirstElemPseudoField as field
            // handle so we cannot use FieldSeqNode::GetFieldHandle() because it asserts on such
            // handles. ObjectAllocator should be changed to create LCL_FLD nodes directly.
            assert(!indir->OperIs(GT_FIELD) || (indir->AsField()->gtFldHnd == fieldSeq->GetTail()->m_fieldHnd));
        }
        else
        {
            // Normalize fieldSeq to null so we don't need to keep checking for both null and NotAField.
            fieldSeq = nullptr;
        }

        if (varTypeIsSIMD(indir->TypeGet()))
        {
            // TODO-ADDR: Skip SIMD indirs for now, SIMD typed LCL_FLDs works most of the time
            // but there are exceptions - fgMorphFieldAssignToSIMDIntrinsicSet for example.
            // And more importantly, SIMD call args have to be wrapped in OBJ nodes currently.
            return;
        }

        if (indir->TypeGet() != TYP_STRUCT)
        {
            if ((fieldSeq != nullptr) && !indir->OperIs(GT_FIELD))
            {
                // If we have an indirection node and a field sequence then they should have the same type.
                // Otherwise it's best to forget the field sequence since the resulting LCL_FLD
                // doesn't match a real struct field. Value numbering protects itself from such
                // mismatches but there doesn't seem to be any good reason to generate a LCL_FLD
                // with a mismatched field sequence only to have to ignore it later.

                if (indir->TypeGet() !=
                    JITtype2varType(m_compiler->info.compCompHnd->getFieldType(fieldSeq->GetTail()->GetFieldHandle())))
                {
                    fieldSeq = nullptr;
                }
            }
        }
        else
        {
            if (indir->OperIs(GT_IND))
            {
                // Skip TYP_STRUCT IND nodes, it's not clear what we can do with them.
                // Normally these should appear only as sources of variable sized copy block
                // operations (DYN_BLK) so it probably doesn't make much sense to try to
                // convert these to local nodes.
                return;
            }

            if ((user == nullptr) || !user->OperIs(GT_ASG))
            {
                // TODO-ADDR: Skip TYP_STRUCT indirs for now, unless they're used by an ASG.
                // At least call args will require extra work because currently they must be
                // wrapped in OBJ nodes so we can't replace those with local nodes.
                return;
            }

            if (indir->OperIs(GT_FIELD))
            {
                CORINFO_CLASS_HANDLE fieldClassHandle;
                CorInfoType          corType =
                    m_compiler->info.compCompHnd->getFieldType(indir->AsField()->gtFldHnd, &fieldClassHandle);
                assert(corType == CORINFO_TYPE_VALUECLASS);

                structLayout = m_compiler->typGetObjLayout(fieldClassHandle);
            }
            else
            {
                structLayout = indir->AsBlk()->GetLayout();
            }

            // We're not going to produce a TYP_STRUCT LCL_FLD so we don't need the field sequence.
            fieldSeq = nullptr;
        }

        // We're only processing TYP_STRUCT variables now so the layout should never be null,
        // otherwise the below layout equality check would be insufficient.
        assert(varDsc->GetLayout() != nullptr);

        if ((val.Offset() == 0) && (structLayout == varDsc->GetLayout()))
        {
            indir->ChangeOper(GT_LCL_VAR);
            indir->AsLclVar()->SetLclNum(val.LclNum());
        }
        else if (!varTypeIsStruct(indir->TypeGet()))
        {
            indir->ChangeOper(GT_LCL_FLD);
            indir->AsLclFld()->SetLclNum(val.LclNum());
            indir->AsLclFld()->SetLclOffs(val.Offset());
            indir->AsLclFld()->SetFieldSeq(fieldSeq == nullptr ? FieldSeqStore::NotAField() : fieldSeq);

            // Promoted struct vars aren't currently handled here so the created LCL_FLD can't be
            // later transformed into a LCL_VAR and the variable cannot be enregistered.
            m_compiler->lvaSetVarDoNotEnregister(val.LclNum() DEBUGARG(Compiler::DNER_LocalField));
        }
        else
        {
            // TODO-ADDR: Add TYP_STRUCT support to LCL_FLD.
            return;
        }

        unsigned flags = 0;

        if ((user != nullptr) && user->OperIs(GT_ASG) && (user->AsOp()->gtGetOp1() == indir))
        {
            flags |= GTF_VAR_DEF | GTF_DONT_CSE;

            if (indir->OperIs(GT_LCL_FLD))
            {
                // Currently we don't generate TYP_STRUCT LCL_FLDs so we do not need to
                // bother to find out the size of the LHS for "partial definition" purposes.
                assert(!varTypeIsStruct(indir->TypeGet()));

                if (genTypeSize(indir->TypeGet()) < m_compiler->lvaLclExactSize(val.LclNum()))
                {
                    flags |= GTF_VAR_USEASG;
                }
            }
        }

        indir->gtFlags = flags;

        INDEBUG(m_stmtModified = true;)
    }

    //------------------------------------------------------------------------
    // MorphStructField: Replaces a GT_FIELD based promoted/normed struct field access
    //    (e.g. FIELD(ADDR(LCL_VAR))) with a GT_LCL_VAR that references the struct field.
    //
    // Arguments:
    //    node - the GT_FIELD node
    //    user - the node that uses the field
    //
    // Notes:
    //    This does not do anything if the field access does not denote
    //    a promoted/normed struct field.
    //
    void MorphStructField(GenTree* node, GenTree* user)
    {
        GenTreeField* field = node->AsField();

        if ((field->gtFldObj == nullptr) || !field->gtFldObj->OperIs(GT_ADDR))
        {
            return;
        }

        GenTree* obj = field->gtFldObj->AsUnOp()->gtGetOp1();

        if (!obj->OperIs(GT_LCL_VAR))
        {
            return;
        }

        unsigned   lclNum = obj->AsLclVar()->GetLclNum();
        LclVarDsc* varDsc = m_compiler->lvaGetDesc(lclNum);

        if (!varTypeIsStruct(obj->TypeGet()))
        {
            // Normed struct
            // A "normed struct" is a struct that the VM tells us is a basic type. This can only happen if
            // the struct contains a single element, and that element is 4 bytes (on x64 it can also be 8
            // bytes). Normally, the type of the local var and the type of GT_FIELD are equivalent. However,
            // there is one extremely rare case where that won't be true. An enum type is a special value type
            // that contains exactly one element of a primitive integer type (that, for CLS programs is named
            // "value__"). The VM tells us that a local var of that enum type is the primitive type of the
            // enum's single field. It turns out that it is legal for IL to access this field using ldflda or
            // ldfld. For example:
            //
            //  .class public auto ansi sealed mynamespace.e_t extends [mscorlib]System.Enum
            //  {
            //    .field public specialname rtspecialname int16 value__
            //    .field public static literal valuetype mynamespace.e_t one = int16(0x0000)
            //  }
            //  .method public hidebysig static void  Main() cil managed
            //  {
            //     .locals init (valuetype mynamespace.e_t V_0)
            //     ...
            //     ldloca.s   V_0
            //     ldflda     int16 mynamespace.e_t::value__
            //     ...
            //  }
            //
            // Normally, compilers will not generate the ldflda, since it is superfluous.
            //
            // In the example, the lclVar is short, but the JIT promotes all trees using this local to the
            // "actual type", that is, INT. But the GT_FIELD is still SHORT. So, in the case of a type
            // mismatch like this, don't do this morphing. The local var may end up getting marked as
            // address taken, and the appropriate SHORT load will be done from memory in that case.

            if (node->TypeGet() == obj->TypeGet())
            {
                node->ChangeOper(GT_LCL_VAR);
                node->AsLclVar()->SetLclNum(lclNum);
                node->gtFlags = 0;

                if (user->OperIs(GT_ASG) && (user->AsOp()->gtGetOp1() == node))
                {
                    node->gtFlags |= GTF_VAR_DEF | GTF_DONT_CSE;
                }

                JITDUMP("Replaced the field in normed struct with local var V%02u\n", lclNum);
                INDEBUG(m_stmtModified = true;)
            }

            return;
        }

        if (!varDsc->lvPromoted)
        {
            return;
        }

        unsigned fieldLclIndex = m_compiler->lvaGetFieldLocal(varDsc, field->gtFldOffset);

        if (fieldLclIndex == BAD_VAR_NUM)
        {
            // Access a promoted struct's field with an offset that doesn't correspond to any field.
            // It can happen if the struct was cast to another struct with different offsets.
            return;
        }

        const LclVarDsc* fieldDsc = m_compiler->lvaGetDesc(fieldLclIndex);

        // promoted LCL_VAR can't have a struct type.
        assert(fieldDsc->TypeGet() != TYP_STRUCT);

        if (node->TypeGet() != fieldDsc->TypeGet())
        {
            if (node->TypeGet() != TYP_STRUCT)
            {
                // This is going to be an incorrect instruction promotion.
                // For example when we try to read int as long.
                return;
            }

            if (field->gtFldHnd != fieldDsc->lvFieldHnd)
            {
                CORINFO_CLASS_HANDLE fieldClass = nullptr;
                CorInfoType fieldType = m_compiler->info.compCompHnd->getFieldType(field->gtFldHnd, &fieldClass);
                CORINFO_CLASS_HANDLE fieldDscClass = nullptr;
                CorInfoType          fieldDscType =
                    m_compiler->info.compCompHnd->getFieldType(fieldDsc->lvFieldHnd, &fieldDscClass);

                if ((fieldType != fieldDscType) || (fieldClass != fieldDscClass))
                {
                    // Access the promoted field with a different class handle, can't check that types
                    // match.
                    return;
                }

                // Access the promoted field as a field of a non-promoted struct with the same class handle.
            }
#ifdef DEBUG
            else if (node->TypeIs(TYP_STRUCT))
            {
                // The field tree accesses it as a struct, but the promoted lcl var for the field
                // says that it has another type. It can happen only if struct promotion faked
                // field type for a struct of single field of scalar type aligned at their natural boundary.
                assert(m_compiler->structPromotionHelper != nullptr);
                m_compiler->structPromotionHelper->CheckRetypedAsScalar(field->gtFldHnd, fieldDsc->TypeGet());
            }
#endif // DEBUG
        }

        node->SetOper(GT_LCL_VAR);
        node->AsLclVar()->SetLclNum(fieldLclIndex);
        node->gtType  = fieldDsc->TypeGet();
        node->gtFlags = 0;

        if (user->OperIs(GT_ASG))
        {
            if (user->AsOp()->gtGetOp1() == node)
            {
                node->gtFlags |= GTF_VAR_DEF | GTF_DONT_CSE;
            }
            else
            {
                assert(user->AsOp()->gtGetOp2() == node);

                // Promotion of struct containing struct fields where the field
                // is a struct with a single pointer sized scalar type field: in
                // this case struct promotion uses the type  of the underlying
                // scalar field as the type of struct field instead of recursively
                // promoting. This can lead to a case where we have a block-asgn
                // with its RHS replaced with a scalar type.  Mark RHS value as
                // DONT_CSE so that assertion prop will not do const propagation.
                // The reason this is required is that if RHS of a block-asg is a
                // constant, then it is interpreted as init-block incorrectly.
                //
                // TODO - This can also be avoided if we implement recursive struct
                // promotion, tracked by #10019.
                if (varTypeIsStruct(user->TypeGet()) && !varTypeIsStruct(node->TypeGet()))
                {
                    node->gtFlags |= GTF_DONT_CSE;
                }
            }
        }

        JITDUMP("Replaced the field in promoted struct with local var V%02u\n", fieldLclIndex);
        INDEBUG(m_stmtModified = true;)
    }

    //------------------------------------------------------------------------
    // MorphLocalField: Replaces a GT_LCL_FLD based promoted struct field access
    //    with a GT_LCL_VAR that references the struct field.
    //
    // Arguments:
    //    node - the GT_LCL_FLD node
    //    user - the node that uses the field
    //
    // Notes:
    //    This does not do anything if the field access does not denote
    //    involved a promoted struct local.
    //    If the GT_LCL_FLD offset does not have a coresponding promoted struct
    //    field then no transformation is done and struct local's enregistration
    //    is disabled.
    //
    void MorphLocalField(GenTree* node, GenTree* user)
    {
        unsigned   lclNum = node->AsLclFld()->GetLclNum();
        LclVarDsc* varDsc = m_compiler->lvaGetDesc(lclNum);

        if (!varTypeIsStruct(varDsc->TypeGet()))
        {
            return;
        }

        if (!varDsc->lvPromoted)
        {
            if (varTypeIsSIMD(varDsc->TypeGet()) && (genTypeSize(node->TypeGet()) == genTypeSize(varDsc->TypeGet())))
            {
                assert(node->AsLclFld()->GetLclOffs() == 0);

                node->ChangeOper(GT_LCL_VAR);
                node->gtType = varDsc->TypeGet();

                JITDUMP("Replaced GT_LCL_FLD of struct with local var V%02u\n", lclNum);
                INDEBUG(m_stmtModified = true;)
            }

            return;
        }

        unsigned fieldLclIndex = m_compiler->lvaGetFieldLocal(varDsc, node->AsLclFld()->GetLclOffs());
        noway_assert(fieldLclIndex != BAD_VAR_NUM);
        LclVarDsc* fldVarDsc = m_compiler->lvaGetDesc(fieldLclIndex);

        if (genTypeSize(fldVarDsc->TypeGet()) != genTypeSize(node->TypeGet()))
        {
            // There is no existing field that has all the parts that we need
            // So we must ensure that the struct lives in memory.
            m_compiler->lvaSetVarDoNotEnregister(lclNum DEBUGARG(Compiler::DNER_LocalField));

            // We can't convert this guy to a float because he really does have his
            // address taken..
            INDEBUG(varDsc->lvKeepType = 1;)
            return;
        }

        // There is an existing sub-field we can use.

        // The field must be an enregisterable type; otherwise it would not be a promoted field.
        // The tree type may not match, e.g. for return types that have been morphed, but both
        // must be enregisterable types.
        assert(varTypeIsEnregisterable(node->TypeGet()) && varTypeIsEnregisterable(fldVarDsc->TypeGet()));

        node->ChangeOper(GT_LCL_VAR);
        node->AsLclVar()->SetLclNum(fieldLclIndex);
        node->gtType = fldVarDsc->TypeGet();

        if (user->OperIs(GT_ASG) && (user->AsOp()->gtGetOp1() == node))
        {
            node->gtFlags |= GTF_VAR_DEF | GTF_DONT_CSE;
        }

        JITDUMP("Replaced the GT_LCL_FLD in promoted struct with local var V%02u\n", fieldLclIndex);
        INDEBUG(m_stmtModified = true;)
    }

    //------------------------------------------------------------------------
    // UpdateEarlyRefCountForImplicitByRef: updates the ref count for implicit byref params.
    //
    // Arguments:
    //    lclNum - the local number to update the count for.
    //
    // Notes:
    //    fgMakeOutgoingStructArgCopy checks the ref counts for implicit byref params when it decides
    //    if it's legal to elide certain copies of them;
    //    fgRetypeImplicitByRefArgs checks the ref counts when it decides to undo promotions.
    //
    void UpdateEarlyRefCountForImplicitByRef(unsigned lclNum)
    {
        if (!m_compiler->lvaIsImplicitByRefLocal(lclNum))
        {
            return;
        }
        LclVarDsc* varDsc = m_compiler->lvaGetDesc(lclNum);
        JITDUMP("LocalAddressVisitor incrementing ref count from %d to %d for V%02d\n", varDsc->lvRefCnt(RCS_EARLY),
                varDsc->lvRefCnt(RCS_EARLY) + 1, lclNum);
        varDsc->incLvRefCnt(1, RCS_EARLY);
    }
};

//------------------------------------------------------------------------
// fgMarkAddressExposedLocals: Traverses the entire method and marks address
//    exposed locals.
//
// Notes:
//    Trees such as IND(ADDR(LCL_VAR)), that morph is expected to fold
//    to just LCL_VAR, do not result in the involved local being marked
//    address exposed.
//
void Compiler::fgMarkAddressExposedLocals()
{
#ifdef DEBUG
    if (verbose)
    {
        printf("\n*************** In fgMarkAddressExposedLocals()\n");
    }
#endif // DEBUG

    LocalAddressVisitor visitor(this);

    for (BasicBlock* block = fgFirstBB; block != nullptr; block = block->bbNext)
    {
        // Make the current basic block address available globally
        compCurBB = block;

        for (Statement* stmt : block->Statements())
        {
            visitor.VisitStmt(stmt);
        }
    }
}

//------------------------------------------------------------------------
// fgMarkAddressExposedLocals: Traverses the specified statement and marks address
//    exposed locals.
//
// Arguments:
//    stmt - the statement to traverse
//
// Notes:
//    Trees such as IND(ADDR(LCL_VAR)), that morph is expected to fold
//    to just LCL_VAR, do not result in the involved local being marked
//    address exposed.
//
void Compiler::fgMarkAddressExposedLocals(Statement* stmt)
{
    LocalAddressVisitor visitor(this);
    visitor.VisitStmt(stmt);
}

#if (defined(TARGET_AMD64) && !defined(UNIX_AMD64_ABI)) || defined(TARGET_ARM64)

class IndirectArgMorphVisitor final : public GenTreeVisitor<IndirectArgMorphVisitor>
{
    INDEBUG(bool m_stmtModified = false;)

public:
    enum
    {
        DoPreOrder        = true,
        DoPostOrder       = false,
        ComputeStack      = false,
        DoLclVarsOnly     = false,
        UseExecutionOrder = false,
    };

    IndirectArgMorphVisitor(Compiler* comp) : GenTreeVisitor<IndirectArgMorphVisitor>(comp)
    {
    }

    void VisitStmt(Statement* stmt)
    {
#ifdef DEBUG
        if (m_compiler->verbose)
        {
            printf("IndirectArgMorphVisitor visiting statement:\n");
            m_compiler->gtDispStmt(stmt);
            m_stmtModified = false;
        }
#endif

        WalkTree(stmt->GetRootNodePointer(), nullptr);

#ifdef DEBUG
        if (m_compiler->verbose)
        {
            if (m_stmtModified)
            {
                printf("IndirectArgMorphVisitor modified statement:\n");
                m_compiler->gtDispStmt(stmt);
            }

            printf("\n");
        }
#endif
    }

    fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
    {
        GenTree* node = *use;

        switch (node->OperGet())
        {
            case GT_ADDR:
                if (node->AsUnOp()->gtGetOp1()->OperIs(GT_LCL_VAR))
                {
                    GenTree* newTree = MorphImplicitByRefArg(node);
                    INDEBUG(m_stmtModified |= (newTree != nullptr);)
                    assert((newTree == nullptr) || (newTree == node));
                    return Compiler::WALK_SKIP_SUBTREES;
                }
                return Compiler::WALK_CONTINUE;

            case GT_LCL_VAR:
            {
                GenTree* newTree = MorphImplicitByRefArg(node);
                INDEBUG(m_stmtModified |= (newTree != nullptr);)
                if (newTree != nullptr)
                {
                    *use = newTree;
                }
            }
                return Compiler::WALK_SKIP_SUBTREES;

            default:
                return Compiler::WALK_CONTINUE;
        }
    }

    GenTree* MorphImplicitByRefArg(GenTree* tree)
    {
        assert(tree->OperIs(GT_LCL_VAR) || (tree->OperIs(GT_ADDR) && tree->AsUnOp()->gtGetOp1()->OperIs(GT_LCL_VAR)));

        GenTreeLclVar* lclNode   = tree->OperIs(GT_ADDR) ? tree->AsUnOp()->gtGetOp1()->AsLclVar() : tree->AsLclVar();
        LclVarDsc*     lclVarDsc = m_compiler->lvaGetDesc(lclNode);

        if (m_compiler->lvaIsImplicitByRefLocal(lclNode->GetLclNum()))
        {
            // fgRetypeImplicitByRefArgs creates LCL_VAR nodes that reference
            // implicit byref args and are already TYP_BYREF, ignore them.
            if (lclNode->TypeIs(TYP_BYREF))
            {
                return nullptr;
            }

            assert(varTypeIsStruct(lclNode->TypeGet()));

            if (lclVarDsc->lvPromoted)
            {
                // fgRetypeImplicitByRefArgs created a new promoted struct local to represent this
                // arg.  Rewrite this to refer to the new local.
                assert(lclVarDsc->lvFieldLclStart != 0);
                lclNode->SetLclNum(lclVarDsc->lvFieldLclStart);
                return tree;
            }

            if (tree->OperIs(GT_ADDR))
            {
                // Change ADDR<BYREF|I_IMPL>(LCL_VAR<STRUCT>(arg)) into LCL_VAR<BYREF>(arg)
                tree->ChangeOper(GT_LCL_VAR);
                tree->gtType  = TYP_BYREF;
                tree->gtFlags = 0;
                tree->AsLclVar()->SetLclNum(lclNode->GetLclNum());
                return tree;
            }

            lclNode->gtType  = TYP_BYREF;
            lclNode->gtFlags = 0;

            // Change LCL_VAR<STRUCT>(arg) into OBJ<STRUCT>(LCL_VAR<BYREF>(arg))
            tree = m_compiler->gtNewObjNode(lclVarDsc->lvVerTypeInfo.GetClassHandle(), lclNode);
            m_compiler->gtSetObjGcInfo(tree->AsObj());
            return tree;
        }

        if (lclVarDsc->lvIsStructField && m_compiler->lvaIsImplicitByRefLocal(lclVarDsc->lvParentLcl))
        {
            // This was a field reference to an implicit-by-reference struct parameter that was
            // dependently promoted and now it is being demoted; update it to a field reference
            // off the original argument.

            assert(lclVarDsc->TypeGet() != TYP_STRUCT);
            assert(lclVarDsc->lvFieldHnd != nullptr);

            var_types fieldType = lclNode->TypeGet();

            lclNode->SetLclNum(lclVarDsc->lvParentLcl);
            lclNode->gtType  = TYP_BYREF;
            lclNode->gtFlags = 0;

            GenTreeField* field =
                m_compiler->gtNewFieldRef(fieldType, lclVarDsc->lvFieldHnd, lclNode, lclVarDsc->lvFldOffset);

            if (tree->OperIs(GT_ADDR))
            {
                // Change ADDR(LCL_VAR(argPromotedField)) into ADDR(FIELD(LCL_VAR<BYREF>(arg), offset))
                tree->AsUnOp()->gtOp1 = field;
                return tree;
            }

            // Change LCL_VAR<fieldType>(argPromotedField) into FIELD<fieldType>(LCL_VAR<BYREF>(arg), offset)
            tree = field;
            // TODO-CQ: If the VM ever stops violating the ABI and passing heap references we could remove TGTANYWHERE
            tree->gtFlags |= GTF_IND_TGTANYWHERE;
            return tree;
        }

        return nullptr;
    }
};

//------------------------------------------------------------------------
// fgMorphImplicitByRefArgs: Traverse the entire statement tree and morph
//    implicit-by-ref argument references in it.
//
// Arguments:
//    stmt - the statement to traverse
//
void Compiler::fgMorphImplicitByRefArgs(Statement* stmt)
{
    assert(fgGlobalMorph);

    IndirectArgMorphVisitor visitor(this);
    visitor.VisitStmt(stmt);
}

#endif // (TARGET_AMD64 && !UNIX_AMD64_ABI) || TARGET_ARM64
