// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

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
        INDEBUG(mutable bool m_consumed;)

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

        void Location(unsigned lclNum, unsigned lclOffs, FieldSeqNode* fieldSeq)
        {
            assert(!IsLocation() && !IsAddress());

            m_lclNum   = lclNum;
            m_offset   = lclOffs;
            m_fieldSeq = fieldSeq;
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
        void Consume() const
        {
            assert(!m_consumed);
            // Mark the value as consumed so that PopValue can ensure that values
            // aren't popped from the stack without being processed appropriately.
            m_consumed = true;
        }

        bool IsConsumed() const
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

        // We could have a statement like IND(ADDR(LCL_VAR)) that EscapeLocation would simplify
        // to LCL_VAR. But since it's unused (and currently can't have any side effects) we'll
        // just change the statment to NOP. This way we avoid complications associated with
        // passing a null user to EscapeLocation.
        // This doesn't seem to happen often, if ever. The importer tends to wrap such a tree
        // in a COMMA.
        if (TopValue(0).IsLocation() || TopValue(0).IsAddress())
        {
            stmt->SetRootNode(m_compiler->gtNewNothingNode());
        }

        INDEBUG(TopValue(0).Consume();)
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

    // Morph promoted struct fields and count implicit byref argument occurrences.
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

#if (defined(TARGET_AMD64) && !defined(UNIX_AMD64_ABI)) || defined(TARGET_ARM64)
        if (node->OperIs(GT_LCL_VAR, GT_LCL_FLD) && m_compiler->lvaHasImplicitByRefParams)
        {
            UpdateImplicitByRefParamRefCounts(node->AsLclVarCommon()->GetLclNum());
        }
#endif

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
                assert(TopValue(1).Node() == node);
                assert(TopValue(0).Node() == node->AsField()->gtFldObj);

                if (!TopValue(1).Field(TopValue(0), node->AsField(), m_compiler->GetFieldSeqStore()))
                {
                    // Either the address comes from a location value (e.g. FIELD(IND(...)))
                    // or the field offset has overflowed.
                    EscapeValue(TopValue(0), node);
                }

                PopValue();
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

            case GT_RETURN:
                if (TopValue(0).Node() != node)
                {
                    assert(TopValue(1).Node() == node);
                    assert(TopValue(0).Node() == node->gtGetOp1());
                    GenTreeUnOp* ret    = node->AsUnOp();
                    GenTree*     retVal = ret->gtGetOp1();
                    if (retVal->OperIs(GT_LCL_VAR))
                    {
                        // TODO-1stClassStructs: this block is a temporary workaround to keep diffs small,
                        // having `doNotEnreg` affect block init and copy transformations that affect many methods.
                        // I have a change that introduces more precise and effective solution for that, but it would
                        // be merged separatly.
                        GenTreeLclVar* lclVar = retVal->AsLclVar();
                        unsigned       lclNum = lclVar->GetLclNum();
                        LclVarDsc*     lcl    = m_compiler->lvaGetDesc(lclNum);

                        if ((m_compiler->info.retDesc.GetRegCount() == 1) && !lcl->IsImplicitByRefParam() &&
                            lcl->IsPromoted() && (lcl->GetPromotedFieldCount() > 1) && !varTypeIsSIMD(lcl->GetType()))
                        {
                            m_compiler->lvaSetVarDoNotEnregister(lclNum DEBUGARG(Compiler::DNER_BlockOp));
                        }
                    }

                    EscapeValue(TopValue(0), node);
                    PopValue();
                }
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
                JITDUMP("Adding a quirk for the storage size of V%02u of type %s\n", val.LclNum(),
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
    void EscapeLocation(const Value& val, GenTree* user)
    {
        assert(val.IsLocation());
        INDEBUG(val.Consume();)
        assert(user != nullptr);

        LclVarDsc* lcl  = m_compiler->lvaGetDesc(val.LclNum());
        GenTree*   node = val.Node();

        if (node->OperIs(GT_LCL_VAR, GT_LCL_FLD))
        {
            assert(node->AsLclVarCommon()->GetLclNum() == val.LclNum());

            // TODO-MIKE-Cleanup: This shouldn't be restricted to call args but ASG LHS
            // requires special handling. In fact, the LCL_VAR|FLD special casing should
            // probably be removed and left to a generalized MorphLocalIndir.

            if (node->OperIs(GT_LCL_VAR) && lcl->IsPromoted() && (lcl->GetPromotedFieldCount() == 1))
            {
                if (user->IsCall())
                {
                    PromoteSingleFieldStructCallArg(lcl, node->AsLclVar());
                }
                else if (user->OperIs(GT_RETURN))
                {
                    PromoteSingleFieldStructReturn(user->AsUnOp(), lcl, node->AsLclVar());
                }
            }

            return;
        }

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

        unsigned indirSize = GetIndirSize(node, user);
        bool     isWide;

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
            else if (lcl->GetType() == TYP_STRUCT)
            {
                isWide = (endOffset.Value() > lcl->GetLayout()->GetSize());
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
                isWide = (endOffset.Value() > varTypeSize(lcl->GetType()));
            }
        }

        if (isWide)
        {
            m_compiler->lvaSetVarAddrExposed(lcl->lvIsStructField ? lcl->lvParentLcl : val.LclNum());
            return;
        }

        if (varTypeIsStruct(lcl->GetType()) && lcl->IsPromoted())
        {
            // If this is a promoted variable then we can use a promoted field if it completly
            // overlaps the indirection. With a lot of work, we could also handle cases where
            // the indirection spans multiple fields (e.g. reading two consecutive INT fields
            // as LONG) which would prevent dependent promotion.

            unsigned fieldLclNum = FindPromotedField(lcl, val.Offset(), indirSize);

            if (fieldLclNum != BAD_VAR_NUM)
            {
                LclVarDsc*    fieldLcl    = m_compiler->lvaGetDesc(fieldLclNum);
                unsigned      fieldOffset = val.Offset() - fieldLcl->GetPromotedFieldOffset();
                FieldSeqNode* fieldSeq    = val.FieldSeq();

                if ((fieldSeq != nullptr) && fieldSeq->IsField())
                {
                    fieldSeq = fieldSeq->RemovePrefix(fieldLcl->GetPromotedFieldSeq());

                    if (fieldSeq == val.FieldSeq())
                    {
                        // There was no prefix, this means that the field access sequence doesn't
                        // match the promoted field sequence, ignore the field access sequence.
                        fieldSeq = nullptr;
                    }
                }

                Value fieldVal(val.Node());
                fieldVal.Location(fieldLclNum, fieldOffset, fieldSeq);
                MorphLocalIndir(fieldVal, user, indirSize);

                return;
            }
        }

        MorphLocalIndir(val, user, indirSize);
    }

    //------------------------------------------------------------------------
    // FindPromotedField: Find a promoted struct field that completely overlaps
    //     a location of specified size at the specified offset.
    //
    // Arguments:
    //    lcl - the promoted local variable
    //    offset - the location offset
    //    size - the location size
    //
    // Return Value:
    //    The overlapping promoted struct field local number or BAD_VAR_NUM if
    //    no overlapping field exists.
    //
    unsigned FindPromotedField(LclVarDsc* lcl, unsigned offset, unsigned size) const
    {
        for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); i++)
        {
            unsigned   fieldLclNum = lcl->GetPromotedFieldLclNum(i);
            LclVarDsc* fieldLcl    = m_compiler->lvaGetDesc(fieldLclNum);

            assert(fieldLcl->GetType() != TYP_STRUCT);

            if ((offset >= fieldLcl->GetPromotedFieldOffset()) &&
                (offset - fieldLcl->GetPromotedFieldOffset() + size <= varTypeSize(fieldLcl->GetType())))
            {
                return fieldLclNum;
            }
        }

        return BAD_VAR_NUM;
    }

    // TODO-MIKE-Cleanup: Replace with FindPromotedField?
    unsigned GetPromotedFieldLclNumByOffset(const LclVarDsc* lcl, unsigned offset) const
    {
        for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); i++)
        {
            unsigned   fieldLclNum = lcl->GetPromotedFieldLclNum(i);
            LclVarDsc* fieldLcl    = m_compiler->lvaGetDesc(fieldLclNum);

            if (fieldLcl->GetPromotedFieldOffset() == offset)
            {
                return fieldLclNum;
            }
        }

        return BAD_VAR_NUM;
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
    //    GT_IND nodes that have type TYP_STRUCT are expected to only appear
    //    on the RHS of an assignment to DYN_BLK so they're also considered to
    //    have unknown size.
    //
    unsigned GetIndirSize(GenTree* indir, GenTree* user)
    {
        assert(indir->OperIs(GT_IND, GT_OBJ, GT_BLK, GT_DYN_BLK, GT_FIELD));

        if (indir->GetType() != TYP_STRUCT)
        {
            return varTypeSize(indir->GetType());
        }

        if (indir->OperIs(GT_IND, GT_DYN_BLK))
        {
            // STRUCT typed IND nodes are only used as the source of DYN_BLK
            // so their size is unknown.

            return 0;
        }

        if (user->OperIs(GT_ASG) && (indir == user->AsOp()->GetOp(1)))
        {
            // A struct indir that is the RHS of an assignment should get its size from the LHS,
            // in case the LHS and RHS have different types the LHS size is used in codegen.
            // This shouldn't happen as it would mean the IL is invalid but the importer's too
            // messed up to expect it to properly reject invalid IL.

            indir = user->AsOp()->GetOp(0);

            if (indir->GetType() != TYP_STRUCT)
            {
                return varTypeSize(indir->GetType());
            }

            // The LHS may be a LCL_VAR/LCL_FLD, these are not indirections so we need to handle them here.
            // It can also be a GT_INDEX, this is an indirection but it never applies to lclvar addresses
            // so it needs to be handled here as well.

            switch (indir->GetOper())
            {
                case GT_LCL_VAR:
                    return m_compiler->lvaGetDesc(indir->AsLclVar())->GetLayout()->GetSize();
                case GT_LCL_FLD:
                    return indir->AsLclFld()->GetLayout(m_compiler)->GetSize();
                case GT_INDEX:
                    return indir->AsIndex()->GetElemSize();
                default:
                    break;
            }
        }

        if (GenTreeField* field = indir->IsField())
        {
            CORINFO_CLASS_HANDLE fieldClassHandle;
            CorInfoType          corType =
                m_compiler->info.compCompHnd->getFieldType(field->GetFieldHandle(), &fieldClassHandle);
            assert(JITtype2varType(corType) == TYP_STRUCT);
            return m_compiler->info.compCompHnd->getClassSize(fieldClassHandle);
        }

        return indir->AsBlk()->GetLayout()->GetSize();
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

        GenTree* addr = val.Node();

        if (val.Offset() > UINT16_MAX)
        {
            // The offset is too large to store in a LCL_FLD_ADDR node,
            // use ADD(LCL_VAR_ADDR, offset) instead.
            addr->ChangeOper(GT_ADD);
            addr->AsOp()->gtOp1 = m_compiler->gtNewLclVarAddrNode(val.LclNum());
            addr->AsOp()->gtOp2 = m_compiler->gtNewIconNode(val.Offset(), val.FieldSeq());
        }
        else if ((val.Offset() != 0) || ((val.FieldSeq() != nullptr) && (val.FieldSeq() != FieldSeqStore::NotAField())))
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
        addr->gtFlags = GTF_EMPTY;

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
    void MorphLocalIndir(const Value& val, GenTree* user, unsigned indirSize)
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

            // For now make the local address exposed to workaround a bug in fgMorphSmpOp's
            // IND morphing code. It completly ignores volatile indirs and in doing so it
            // fails to DNER the local which leads to asserts in the backend.
            m_compiler->lvaSetVarAddrExposed(val.LclNum());
            return;
        }

        LclVarDsc* varDsc = m_compiler->lvaGetDesc(val.LclNum());

        if (indir->OperIs(GT_BLK))
        {
            // Keep BLKs and mark any involved locals address exposed for now.
            //
            // CPBLK and INITBLK are rarely used (the C# compiler doesn't emit them at all).
            // Only the VM sometimes uses CPBLK in PInvoke IL stubs, to deal with x86 ABI
            // mismatches it seems (Windows x86 ABI can return structs in 2 registers but
            // CLR uses a return buffer instead).
            //
            // TODO-MIKE-CQ: A previous implementation avoided address exposed locals, but
            // still forced locals in memory because PInvoke IL stubs copy from a LONG local
            // to a STRUCT local and one way or another this results in DNER/P-DEP locals.
            // This can be avoided but it's unlikely that it's worth the effort for x86...

            m_compiler->lvaSetVarAddrExposed(val.LclNum());

            if (varDsc->IsPromotedField())
            {
                m_compiler->lvaSetVarAddrExposed(varDsc->GetPromotedFieldParentLclNum());
            }

            return;
        }

        const bool isDef = user->OperIs(GT_ASG) && (user->AsOp()->GetOp(0) == indir);

        if (!varTypeIsStruct(varDsc->GetType()))
        {
            // TODO-MIKE-Cleanup: This likely makes a bunch of IND morphing code in fgMorphSmpOp redundant.

            // A non-struct local may be accessed via a struct indir due to reinterpretation in user
            // code or due to single field struct promotion. Reinterpretation isn't common (but it
            // does happen - e.g. ILCompiler.Reflection.ReadyToRun.dll reinterprets array references
            // as ImmutableArray) but promotion is quite common. If the indir is a call arg then we
            // can simply replace it with the local variable because it doesn't really matter if it's
            // a struct value or a primitive type value, we just need to put the value in the correct
            // register or stack slot.

            // TODO-ADDR: This also needs to be done for struct assignments.
            //
            // For assignments it can be easily done if the indir is the source, fgMorphCopyBlock
            // handles the case of a struct assignment with a primitive typed source. But if the
            // indir is the destination then we need to retype the entire assignment.

            if (indir->TypeIs(TYP_STRUCT))
            {
                if (indir->OperIs(GT_OBJ) && user->IsCall())
                {
                    MorphStructCallArgLocalIndir(val, indir, indirSize);
                    return;
                }

                if (indir->OperIs(GT_OBJ, GT_FIELD) && user->OperIs(GT_RETURN))
                {
                    MorphStructReturnLocalIndir(val, user->AsUnOp(), indir, indirSize);
                    return;
                }
            }

            var_types lclType   = varDsc->GetType();
            var_types indirType = indir->GetType();

            if ((val.Offset() == 0) && !varTypeIsStruct(indirType))
            {
                if (varTypeSize(indirType) == varTypeSize(lclType))
                {
                    if (isDef)
                    {
                        // Handle the "store" variant of the "indirect local load" cases below.
                        // The only difference is that on store the signedness of small int types
                        // is not relevant. If the destination local ends up being "normalize on
                        // store" then global morph will later do the required widening.

                        bool isAssignable = varTypeKind(indirType) == varTypeKind(lclType);

                        if (!isAssignable && CanBitCastTo(lclType))
                        {
                            user->AsOp()->SetOp(1, NewBitCastNode(lclType, user->AsOp()->GetOp(1)));
                            isAssignable = true;
                        }

                        if (isAssignable)
                        {
                            indir->ChangeOper(GT_LCL_VAR);
                            indir->SetType(lclType);
                            indir->AsLclVar()->SetLclNum(val.LclNum());
                            indir->gtFlags = GTF_VAR_DEF | GTF_DONT_CSE;
                        }
                    }
                    else if (varTypeIsSmall(indirType) && (varTypeIsUnsigned(indirType) != varTypeIsUnsigned(lclType)))
                    {
                        // There's no reason to write something like `*(short*)&ushortLocal` instead of just
                        // `(short)ushortLocal`, except that generics code can't do such casts and sometimes
                        // uses `Unsafe.As` as a substitute.

                        indir->ChangeOper(GT_CAST);
                        indir->AsCast()->SetCastType(indirType);
                        indir->AsCast()->SetOp(0, NewLclVarNode(lclType, val.LclNum()));
                        indir->gtFlags = GTF_EMPTY;
                    }
                    else if (varTypeKind(indirType) != varTypeKind(lclType))
                    {
                        // Handle the relatively common case of floating point/integer reinterpretation.
                        // Also handle GC pointer/native int reinterpretation, some corelib tracing code
                        // uses object references as if they're normal pointers for logging purposes.

                        if (CanBitCastTo(indirType))
                        {
                            indir->ChangeOper(GT_BITCAST);
                            indir->AsUnOp()->SetOp(0, NewLclVarNode(lclType, val.LclNum()));
                            indir->gtFlags = GTF_EMPTY;
                        }
                    }
                    else
                    {
                        // Like in the signedess mismatch case above, it's not common to have `*(int*)&intLocal`
                        // but such cases may arise either from fancy generic code or, more likely, due to the
                        // inlining of primitive type methods. Turns out that having methods on primitive types,
                        // while elegant, can cause problems as any call to such a method requires taking the
                        // address of the primitive type local variable.

                        assert((indirType == lclType) || ((indirType == TYP_BOOL) && (lclType == TYP_UBYTE)) ||
                               ((indirType == TYP_UBYTE) && (lclType == TYP_BOOL)));

                        indir->ChangeOper(GT_LCL_VAR);
                        indir->AsLclVar()->SetLclNum(val.LclNum());
                        indir->gtFlags = GTF_EMPTY;
                    }
                }
                else
                {
                    // The indir has to be smaller than the local, the "wide" indir case is handled in EscapeLocation.
                    assert(varTypeSize(indirType) < varTypeSize(lclType));

                    // Storing to a local via a smaller indirection is more difficult to handle, we need something
                    // like ASG(lcl, OR(lcl, value)) in the integer case plus some BITCASTs or intrinsics in the
                    // float case. CoreLib has a few cases in Vector128 & co. WithElement methods but these are
                    // normally recognized as intrinsics so it's not worth the trouble to handle this case.

                    // Loading via a smaller indirection isn't common either but this case can be easily handled
                    // by using a CAST. There's no reason to write something like `*(short*)&intLocal` instead of
                    // just `(short)intLocal`, except that generics code can't do such casts and sometimes uses
                    // `Unsafe.As` as a substitute.

                    if (!isDef && varTypeIsIntegral(indirType) && varTypeIsIntegral(lclType))
                    {
                        indir->ChangeOper(GT_CAST);
                        indir->AsCast()->SetCastType(indirType);
                        indir->AsCast()->SetOp(0, NewLclVarNode(lclType, val.LclNum()));
                        indir->gtFlags = GTF_EMPTY;
                    }
                }
            }

            // If we haven't been able to get rid of the indir until now then just use a LCL_FLD.
            //
            // Except in the odd case when the indir has STRUCT type. One might expect this case
            // to be very rare but thanks to pseudo-recursive struct promotion it is actually
            // quite common to have OBJ(ADDR(LCL_VAR)) with a promoted struct field local.
            // Using a LCL_FLD in this case would then lead to dependent struct promotion.
            // Though perhaps the solution is to use LCL_FLD but defer DNERing the local to morph
            // or lowering.

            if (indir->OperIs(GT_IND, GT_OBJ, GT_FIELD) && (indirType != TYP_STRUCT))
            {
                indir->ChangeOper(GT_LCL_FLD);
                indir->AsLclFld()->SetLclNum(val.LclNum());
                indir->AsLclFld()->SetLclOffs(val.Offset());
                indir->gtFlags = GTF_EMPTY;

                if (isDef)
                {
                    indir->gtFlags |= GTF_VAR_DEF | GTF_DONT_CSE;

                    if (varTypeSize(indirType) < varTypeSize(lclType))
                    {
                        indir->gtFlags |= GTF_VAR_USEASG;
                    }

                    if (varTypeIsSmall(varDsc->GetType()))
                    {
                        // If LCL_FLD is used to store to a small type local then "normalize on store"
                        // isn't possible so the local has to be "normalize on load". The only way to
                        // do this is by making the local address exposed which is a big hammer. But
                        // such an indirect store is highly unusual so it's not worth the trouble to
                        // introduce another mechanism.
                        m_compiler->lvaSetVarAddrExposed(val.LclNum());
                    }
                }

                m_compiler->lvaSetVarDoNotEnregister(val.LclNum() DEBUGARG(Compiler::DNER_LocalField));
            }

            INDEBUG(m_stmtModified |= !indir->OperIs(GT_IND, GT_OBJ, GT_FIELD);)

            return;
        }

#ifdef FEATURE_SIMD
        if (varTypeIsSIMD(varDsc->GetType()) && varDsc->lvIsUsedInSIMDIntrinsic() && indir->TypeIs(TYP_FLOAT) &&
            (val.Offset() % 4 == 0) && (!isDef || !varDsc->IsImplicitByRefParam()) && !varDsc->lvDoNotEnregister)
        {
            // Recognize fields X/Y/Z/W of Vector2/3/4. These fields have type FLOAT so this is the only type
            // we recognize here but any other type supported by GetItem/SetItem would work. But other vector
            // types don't have fields and the only way this would be useful is if someone uses unsafe code
            // to access the vector elements. That's not very useful as the other vector types offer element
            // access by other means - Vector's indexer and Vector64/128/256's GetElement.

            // TODO-MIKE-CQ: Doing this here is a bit of a problem if the local variable ends up in memory,
            // if it is DNER for whatever reasons (we haven't determined that exactly at this point) or if
            // it is an implicit byref param. We could produce a LCL_FLD here, without DNERing the local and
            // let morph convert it to GetItem/SetItem or leave it alone if the local was already DNERed for
            // other reasons. But creating a LCL_FLD without DNERing the corresponding local seems somewhat
            // risky at the moment.
            //
            // Implicit byref params are a bit more complicated. We could simply skip this transform if the
            // local is an implicit byref param but that's not always a clear improvement because of CSE.
            // If multiple vector elements are accessed then we can have a single SIMD load and multiple
            // GetItem to extract the elements. Otherwise we need to do a separate FLOAT load for each.
            // Having a single load may be faster but multiple loads can result in smaller code if loads
            // end up as memory operands on other instructions (on x86/64).
            //
            // Ultimately the best option may be to keep doing this here and compensate for the memory case
            // in lowering. This is already done for GetItem but doesn't work very well. And SetItem is a
            // bit more cumbersome to handle, though perhaps it would fit into the existing RMW lowering.

            // TODO-MIKE-CQ: The lvIsUsedInSIMDIntrinsic check is bogus. It's really intended to block
            // struct promotion and this transform doesn't have anything to do with that. In fact using
            // it here hurts promotion because SIMD typed promoted fields don't have it set so accessing
            // their X/Y/Z/W fields will just result in DNER.

            if (isDef)
            {
                indir->ChangeOper(GT_LCL_VAR);
                indir->SetType(varDsc->GetType());
                indir->AsLclVar()->SetLclNum(val.LclNum());
                indir->gtFlags = GTF_VAR_DEF | GTF_DONT_CSE;

                user->AsOp()->SetOp(1, NewInsertElement(varDsc->GetType(), val.Offset() / 4, TYP_FLOAT,
                                                        NewLclVarNode(varDsc->GetType(), val.LclNum()),
                                                        user->AsOp()->GetOp(1)));
                user->SetType(varDsc->GetType());
            }
            else
            {
                indir->ChangeOper(GT_HWINTRINSIC);
                indir->AsHWIntrinsic()->SetIntrinsic(NI_Vector128_GetElement, TYP_FLOAT, 16, 2);
                indir->AsHWIntrinsic()->SetOp(0, NewLclVarNode(varDsc->GetType(), val.LclNum()));
                indir->AsHWIntrinsic()->SetOp(1, NewIntConNode(TYP_INT, val.Offset() / 4));
            }

            INDEBUG(m_stmtModified = true;)

            return;
        }
#endif // FEATURE_SIMD

        ClassLayout*  indirLayout = nullptr;
        FieldSeqNode* fieldSeq    = val.FieldSeq();

        if ((fieldSeq != nullptr) && (fieldSeq != FieldSeqStore::NotAField()))
        {
            assert(!indir->OperIs(GT_FIELD) ||
                   (indir->AsField()->GetFieldHandle() == fieldSeq->GetTail()->GetFieldHandle()));
        }
        else
        {
            // Normalize fieldSeq to null so we don't need to keep checking for both null and NotAField.
            fieldSeq = nullptr;
        }

        if (!varTypeIsStruct(indir->GetType()))
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
        else if (indir->OperIs(GT_IND))
        {
            // Can't have STRUCT typed IND nodes here, they should have been rejected earlier.

            assert(varTypeIsSIMD(indir->GetType()));
        }
        else if (indir->OperIs(GT_FIELD))
        {
            CORINFO_CLASS_HANDLE fieldClassHandle;
            CorInfoType          corType =
                m_compiler->info.compCompHnd->getFieldType(indir->AsField()->gtFldHnd, &fieldClassHandle);
            assert(corType == CORINFO_TYPE_VALUECLASS);

            indirLayout = m_compiler->typGetObjLayout(fieldClassHandle);
        }
        else
        {
            indirLayout = indir->AsObj()->GetLayout();

            assert(!indirLayout->IsBlockLayout());

            if (fieldSeq != nullptr)
            {
                CORINFO_CLASS_HANDLE fieldClassHandle;
                CorInfoType corType = m_compiler->info.compCompHnd->getFieldType(fieldSeq->GetTail()->GetFieldHandle(),
                                                                                 &fieldClassHandle);

                if ((corType != CORINFO_TYPE_VALUECLASS) || (fieldClassHandle != indirLayout->GetClassHandle()))
                {
                    fieldSeq = nullptr;
                }
            }
        }

        if ((val.Offset() == 0) && (indirLayout != nullptr))
        {
            if (varTypeIsSIMD(varDsc->GetType()) && (indirLayout->GetSize() == varTypeSize(varDsc->GetType())) &&
                user->IsCall())
            {
                // A SIMD local may be accessed as a struct of the same size due to promoting
                // single SIMD field structs - use the SIMD field directly and let call arg
                // morphing figure out how to load it in registers if needed.

                // TODO-MIKE-CQ: This should also be done for returns.

                indir->SetType(varDsc->GetType());
            }
            else if ((indirLayout != varDsc->GetLayout()) && varDsc->IsPromoted() &&
                     (indir->GetType() == varDsc->GetType()) &&
                     (indirLayout->GetSize() == varDsc->GetLayout()->GetSize()))
            {
                // If the indir layout doesn't match and the local is promoted then ignore the
                // indir layout to avoid having to make a LCL_FLD and dependent promote the
                // local. The indir layout isn't really needed anymore, since call arg morphing
                // uses the one from the call signature.

                // The only thing other than the ABI the layout influences is GCness of stores
                // to the local but in that case it really does make more sense to ignore the
                // indir layout and use the local variable layout as that is the "real" one when
                // it comes to GC. Reinterpreting a local variable in an attempt to avoid GC safe
                // copies doesn't make a lot of sense.

                // TODO-MIKE-Consider: This should work for non promoted locals as well but it's
                // not clear if it's worth doing and safe.
                // Avoiding LCL_FLDs may improve assertion copy propagation but on the other hand
                // this can create more assignments with different source and destination types
                // and it's not clear how well VN maps handles those.
                // Also, discarding type information is not that great in general and it may be
                // better to instead teach assertion propagation to deal with LCL_FLDs.

                indirLayout = varDsc->GetLayout();
            }
        }

        // For SIMD locals/indirs we don't care about the layout, only that the types match.
        // This could probably be relaxed to allow cases like Vector2 indir and Vector4 local
        // since they all use the same registers. Might need to zero out the upper elements
        // though.

        if ((val.Offset() == 0) && (indir->GetType() == varDsc->GetType()) &&
            (varTypeIsSIMD(indir->GetType()) || (indirLayout == varDsc->GetLayout())))
        {
            indir->ChangeOper(GT_LCL_VAR);
            indir->AsLclVar()->SetLclNum(val.LclNum());
        }
        else
        {
            indir->ChangeOper(GT_LCL_FLD);
            indir->AsLclFld()->SetLclNum(val.LclNum());
            indir->AsLclFld()->SetLclOffs(val.Offset());
            indir->AsLclFld()->SetFieldSeq(fieldSeq == nullptr ? FieldSeqStore::NotAField() : fieldSeq);

            if (indirLayout != nullptr)
            {
                indir->AsLclFld()->SetLayout(indirLayout, m_compiler);
            }

            // Promoted struct vars aren't currently handled here so the created LCL_FLD can't be
            // later transformed into a LCL_VAR and the variable cannot be enregistered.
            m_compiler->lvaSetVarDoNotEnregister(val.LclNum() DEBUGARG(Compiler::DNER_LocalField));
        }

        GenTreeFlags flags = GTF_EMPTY;

        if (user->OperIs(GT_ASG) && (user->AsOp()->GetOp(0) == indir))
        {
            flags |= GTF_VAR_DEF | GTF_DONT_CSE;

            if (indir->OperIs(GT_LCL_FLD))
            {
                if ((val.Offset() != 0) || (indirSize < m_compiler->lvaGetDesc(val.LclNum())->GetSize()))
                {
                    flags |= GTF_VAR_USEASG;
                }
            }
        }
        else if (indir->TypeIs(TYP_STRUCT) && user->IsCall())
        {
            flags |= GTF_DONT_CSE;
        }

        indir->gtFlags = flags;

        INDEBUG(m_stmtModified = true;)
    }

    void MorphStructCallArgLocalIndir(const Value& val, GenTree* indir, unsigned indirSize)
    {
        LclVarDsc*   lcl                 = m_compiler->lvaGetDesc(val.LclNum());
        ClassLayout* indirLayout         = indir->AsObj()->GetLayout();
        var_types    lclType             = lcl->GetType();
        bool         isSingleFieldStruct = false;

        if ((val.Offset() == 0) && (indirSize == varTypeSize(lclType)))
        {
            // For now do this only if the struct has a single field since this is the common case
            // generated by struct promotion. Eventually this could also work for reinterpretation
            // cases like "struct with 3 byte fields, local variable of type int or long".

            if (lcl->IsPromotedField())
            {
                // If the local is a promoted field try checking if the indir has the same type as
                // the parent local. This way we avoid querying the VM for class fields when dealing
                // with single field promoted structs. Note that this doesn't cover the recursive
                // struct promotion case because we have lost the type of the inner struct. It may
                // be recovered from the promoted struct field handle but we may as well go the other
                // way around and check the indir's class.

                LclVarDsc* parentLcl = m_compiler->lvaGetDesc(lcl->GetPromotedFieldParentLclNum());

                if ((parentLcl->GetLayout() == indirLayout) && (parentLcl->GetPromotedFieldCount() == 1))
                {
                    isSingleFieldStruct = true;
                }
            }

            if (!isSingleFieldStruct)
            {
                // This doesn't cover the case of nested single field structs but it's sufficient to
                // handle JIT's struct promotion.

                ICorJitInfo* vm = m_compiler->info.compCompHnd;
                if (vm->getClassNumInstanceFields(indirLayout->GetClassHandle()) == 1)
                {
                    CORINFO_FIELD_HANDLE fieldHandle = vm->getFieldInClass(indirLayout->GetClassHandle(), 0);
                    isSingleFieldStruct = (lcl->GetType() == JITtype2varType(vm->getFieldType(fieldHandle)));
                }
            }
        }

        if (isSingleFieldStruct)
        {
            indir->ChangeOper(GT_LCL_VAR);
            indir->SetType(lclType);
            indir->AsLclVar()->SetLclNum(val.LclNum());
            indir->gtFlags = GTF_EMPTY;
        }
        else
        {
            indir->ChangeOper(GT_LCL_FLD);
            indir->AsLclFld()->SetLclNum(val.LclNum());
            indir->AsLclFld()->SetLclOffs(val.Offset());
            indir->AsLclFld()->SetLayoutNum(m_compiler->typGetLayoutNum(indirLayout));
            indir->gtFlags = GTF_EMPTY;

            m_compiler->lvaSetVarDoNotEnregister(val.LclNum() DEBUGARG(Compiler::DNER_LocalField));
        }

        INDEBUG(m_stmtModified = true;)
    }

    void PromoteSingleFieldStructCallArg(LclVarDsc* lcl, GenTreeLclVar* node)
    {
        assert(lcl->GetPromotedFieldCount() == 1);

        unsigned   fieldLclNum = lcl->GetPromotedFieldLclNum(0);
        LclVarDsc* fieldLcl    = m_compiler->lvaGetDesc(fieldLclNum);

        if (lcl->GetLayout()->GetSize() == varTypeSize(fieldLcl->GetType()))
        {
            node->AsLclVar()->SetLclNum(fieldLclNum);
            node->SetType(fieldLcl->GetType());
        }

        INDEBUG(m_stmtModified = true;)
    }

    void MorphStructReturnLocalIndir(const Value& val, GenTreeUnOp* ret, GenTree* indir, unsigned indirSize)
    {
        // The merged return temp should never be accessed indirectly
        // so we don't need to worry about it here.
        assert(val.LclNum() != m_compiler->genReturnLocal);

        LclVarDsc* lcl = m_compiler->lvaGetDesc(val.LclNum());

        assert(indir->TypeIs(TYP_STRUCT));
        assert(!varTypeIsStruct(lcl->GetType()));

        ClassLayout* indirLayout;

        if (GenTreeField* field = indir->IsField())
        {
            CORINFO_CLASS_HANDLE fieldClassHandle;
            CorInfoType          corType =
                m_compiler->info.compCompHnd->getFieldType(field->GetFieldHandle(), &fieldClassHandle);
            assert(corType == CORINFO_TYPE_VALUECLASS);

            indirLayout = m_compiler->typGetObjLayout(fieldClassHandle);
        }
        else
        {
            indirLayout = indir->AsObj()->GetLayout();
        }

        var_types             lclType     = lcl->GetType();
        const ReturnTypeDesc& retDesc     = m_compiler->info.retDesc;
        bool                  useLcl      = false;
        var_types             bitcastType = TYP_UNDEF;
        var_types             retType     = TYP_UNDEF;

        if ((val.Offset() == 0) && (indirSize <= varTypeSize(lclType)))
        {
            // Since the local isn't a struct we expect it to be returned in a single
            // register, with the exception of the win-x86 native ABI, where we may
            // need to return LONG/DOUBLE in 2 INT registers. We may need to bitcast
            // between integer and floating point (e.g. a struct with a single FLOAT
            // is returned in an integer register on win-x64) but we don't care about
            // the precise size of the involved types. For example, reinterpretation
            // in user code may result in a DOUBLE local returned as a struct with a
            // single FLOAT field, in this case the caller only cares about the low 4
            // bytes in XMM0, there is no need to attempt to zero out the rest of the
            // bytes.

            // This covers the needs of single field struct promotion and reasonable
            // reinterpretation in user code. Anything else (e.g. INT local returned
            // as a 16 byte struct that needs 2 registers on linux/arm-64 is handled
            // by creating a LCL_FLD matching the return type.

            if (retDesc.GetRegCount() == 1)
            {
                var_types retRegType = varActualType(retDesc.GetRegType(0));

                if (varTypeUsesFloatReg(retRegType) == varTypeUsesFloatReg(lclType))
                {
                    useLcl  = true;
                    retType = retRegType;
                }
                else if (varTypeSize(retRegType) == varTypeSize(varActualType(lclType)))
                {
                    useLcl      = true;
                    bitcastType = retRegType;
                    retType     = retRegType;
                }
            }
#ifdef TARGET_X86
            else if (retDesc.GetRegCount() == 2)
            {
                if ((lclType == TYP_LONG) || (lclType == TYP_DOUBLE))
                {
                    assert((retDesc.GetRegType(0) == TYP_INT) && (retDesc.GetRegType(1) == TYP_INT));
                    retType = TYP_LONG;
                }
            }
#endif
        }

        if (useLcl && (bitcastType != TYP_UNDEF))
        {
            assert(varTypeSize(bitcastType) <= REGSIZE_BYTES);

            GenTree* addr = indir->IsField() ? indir->AsField()->GetAddr() : indir->AsObj()->GetAddr();

            addr->ChangeOper(GT_LCL_VAR);
            addr->SetType(lclType);
            addr->AsLclVar()->SetLclNum(val.LclNum());
            addr->gtFlags = GTF_EMPTY;

            indir->ChangeOper(GT_BITCAST);
            indir->SetType(bitcastType);
            indir->AsUnOp()->SetOp(0, addr);
            indir->gtFlags = GTF_EMPTY;

            ret->SetType(retType);
        }
        else if (useLcl)
        {
            indir->ChangeOper(GT_LCL_VAR);
            indir->SetType(lclType);
            indir->AsLclVar()->SetLclNum(val.LclNum());
            indir->gtFlags = GTF_EMPTY;

            ret->SetType(retType);
        }
        else
        {
            indir->ChangeOper(GT_LCL_FLD);
            indir->AsLclFld()->SetLclNum(val.LclNum());
            indir->AsLclFld()->SetLclOffs(val.Offset());
            indir->AsLclFld()->SetLayoutNum(m_compiler->typGetLayoutNum(indirLayout));
            indir->gtFlags = GTF_EMPTY;

            m_compiler->lvaSetVarDoNotEnregister(val.LclNum() DEBUGARG(Compiler::DNER_LocalField));
        }

        INDEBUG(m_stmtModified = true;)
    }

    void PromoteSingleFieldStructReturn(GenTreeUnOp* ret, LclVarDsc* lcl, GenTreeLclVar* node)
    {
        assert(varTypeIsStruct(ret->GetType()));
        assert(lcl->GetPromotedFieldCount() == 1);

        if ((m_compiler->genReturnLocal != BAD_VAR_NUM) && (node->GetLclNum() != m_compiler->genReturnLocal))
        {
            // This is a merged return, it will be transformed into a struct
            // assignment so leave it to fgMorphCopyBlock to promote it.
            return;
        }

        unsigned   fieldLclNum = lcl->GetPromotedFieldLclNum(0);
        LclVarDsc* fieldLcl    = m_compiler->lvaGetDesc(fieldLclNum);

        const ReturnTypeDesc& retDesc = m_compiler->info.retDesc;

#ifdef WINDOWS_X86_ABI
        if ((retDesc.GetRegCount() == 2) && fieldLcl->TypeIs(TYP_LONG, TYP_DOUBLE))
        {
            assert((retDesc.GetRegType(0) == TYP_INT) && (retDesc.GetRegType(1) == TYP_INT));

            node->AsLclVar()->SetLclNum(fieldLclNum);
            node->SetType(fieldLcl->GetType());
            ret->SetType(TYP_LONG);

            INDEBUG(m_stmtModified = true;)

            return;
        }
#endif

        if (retDesc.GetRegCount() > 1)
        {
            // We either have a SIMD field that's returned in multiple registers (e.g. HFA)
            // or perhaps the IL is invalid (e.g. struct with a single DOUBLE field returned
            // as a Vector2 or some other 2 FLOAT field struct that is a HFA).
            // Either way, leave it to morph to produce a FIELD_LIST in this case.
            // We could probably do it here but it's not clear if it has any benefits.

            node->AsLclVar()->SetLclNum(fieldLclNum);
            node->SetType(fieldLcl->GetType());

            // TODO-MIKE-Cleanup: This should be done at import time - RETURN should have SIMD
            // type only if it uses a single register to return a SIMD value.
            ret->SetType(TYP_STRUCT);

            INDEBUG(m_stmtModified = true;)

            return;
        }

        var_types retRegType = varActualType(retDesc.GetRegType(0));

        bool useLcl     = varTypeUsesFloatReg(retRegType) == varTypeUsesFloatReg(fieldLcl->GetType());
        bool useBitcast = !useLcl && (varTypeSize(retRegType) <= REGSIZE_BYTES) &&
                          (varTypeSize(retRegType) == varTypeSize(varActualType(fieldLcl->GetType())));

        if (useLcl || useBitcast)
        {
            node->AsLclVar()->SetLclNum(fieldLclNum);
            node->SetType(fieldLcl->GetType());

            if (useBitcast)
            {
                ret->SetOp(0, NewBitCastNode(retRegType, node));
            }
        }
        else
        {
            node->ChangeOper(GT_LCL_FLD);
            node->AsLclFld()->SetLclNum(fieldLclNum);
            node->SetType(retRegType);

            m_compiler->lvaSetVarDoNotEnregister(fieldLclNum DEBUGARG(Compiler::DNER_LocalField));
        }

        ret->SetType(retRegType);

        INDEBUG(m_stmtModified = true;)
    }

    static bool CanBitCastTo(var_types type)
    {
        assert(((TYP_INT <= type) && (type <= TYP_DOUBLE)) || (type == TYP_REF) || (type == TYP_BYREF));

#ifdef TARGET_64BIT
        return true;
#else
        // Currently long/double BITCAST isn't supported by decomposition on 32 bit targets.
        return varTypeSize(type) <= REGSIZE_BYTES;
#endif
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
        if ((user != nullptr) && user->OperIs(GT_RETURN))
        {
            return;
        }

        GenTreeField* field = node->AsField();

        if (!field->GetAddr()->OperIs(GT_ADDR))
        {
            return;
        }

        GenTree* location = field->GetAddr()->AsUnOp()->GetOp(0);

        if (!location->OperIs(GT_LCL_VAR) || !varTypeIsStruct(location->GetType()))
        {
            return;
        }

        LclVarDsc* lcl = m_compiler->lvaGetDesc(location->AsLclVar());

        if (!lcl->IsPromoted())
        {
            return;
        }

        unsigned fieldLclNum = GetPromotedFieldLclNumByOffset(lcl, field->GetOffset());

        if (fieldLclNum == BAD_VAR_NUM)
        {
            // Access a promoted struct's field with an offset that doesn't correspond to any field.
            // It can happen if the struct was cast to another struct with different offsets.
            return;
        }

        LclVarDsc* fieldLcl = m_compiler->lvaGetDesc(fieldLclNum);

        // Promoted LCL_VAR can't have a struct type.
        assert(!fieldLcl->TypeIs(TYP_STRUCT));

        if (node->GetType() != fieldLcl->GetType())
        {
            if (!node->TypeIs(TYP_STRUCT))
            {
                // This is going to be an incorrect instruction promotion.
                // For example when we try to read int as long.
                return;
            }

            CORINFO_FIELD_HANDLE fieldHandle    = field->GetFieldHandle();
            CORINFO_FIELD_HANDLE fieldLclHandle = fieldLcl->GetPromotedFieldSeq()->GetFieldHandle();

            if (fieldHandle != fieldLclHandle)
            {
                // In general, the accessed field and the promoted field are the same. However, we don't
                // really care if they're the same, we only care if they have the same type or not.
                // In fact, we don't even care if they have the same type, we only care if they have the
                // same layout - a single field having the promoted field local type. But it's not worth
                // bothering with that since such cases arise only due to reinterpretation in user code
                // (and anyway MorphLocalIndir should handle this better).

                CORINFO_CLASS_HANDLE fieldClass    = nullptr;
                CORINFO_CLASS_HANDLE fieldLclClass = nullptr;

                CorInfoType fieldType    = m_compiler->info.compCompHnd->getFieldType(fieldHandle, &fieldClass);
                CorInfoType fieldLclType = m_compiler->info.compCompHnd->getFieldType(fieldLclHandle, &fieldLclClass);

                if ((fieldType != fieldLclType) || (fieldClass != fieldLclClass))
                {
                    return;
                }
            }
        }

        node->SetOper(GT_LCL_VAR);
        node->AsLclVar()->SetLclNum(fieldLclNum);
        node->SetType(fieldLcl->GetType());
        node->gtFlags = GTF_EMPTY;

        if (user->OperIs(GT_ASG))
        {
            if (user->AsOp()->GetOp(0) == node)
            {
                node->gtFlags |= GTF_VAR_DEF | GTF_DONT_CSE;
            }
            else
            {
                assert(user->AsOp()->GetOp(1) == node);

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
                if (varTypeIsStruct(user->GetType()) && !varTypeIsStruct(node->GetType()))
                {
                    node->gtFlags |= GTF_DONT_CSE;
                }
            }
        }

        JITDUMP("Replaced the field in promoted struct with local var V%02u\n", fieldLclNum);
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

        if (!varDsc->IsPromoted())
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

        unsigned fieldLclIndex = GetPromotedFieldLclNumByOffset(varDsc, node->AsLclFld()->GetLclOffs());
        noway_assert(fieldLclIndex != BAD_VAR_NUM);
        LclVarDsc* fldVarDsc = m_compiler->lvaGetDesc(fieldLclIndex);

        if ((varTypeSize(fldVarDsc->GetType()) != varTypeSize(node->GetType())) &&
            (varDsc->GetPromotedFieldCount() != 1))
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
    // UpdateImplicitByRefParamRefCounts: updates the ref count for implicit byref params.
    //
    // Arguments:
    //    lclNum - the local number to update the count for.
    //
    // Notes:
    //    abiMakeImplicityByRefStructArgCopy checks the ref counts for implicit byref params when it decides
    //    if it's legal to elide certain copies of them;
    //    lvaRetypeImplicitByRefParams checks the ref counts when it decides to undo promotions.
    //
    void UpdateImplicitByRefParamRefCounts(unsigned lclNum)
    {
        LclVarDsc* lcl = m_compiler->lvaGetDesc(lclNum);

        if (!lcl->IsImplicitByRefParam())
        {
            if (!lcl->IsPromotedField())
            {
                return;
            }

            lclNum = lcl->GetPromotedFieldParentLclNum();
            lcl    = m_compiler->lvaGetDesc(lclNum);

            if (!lcl->IsImplicitByRefParam())
            {
                return;
            }
        }

        JITDUMP("LocalAddressVisitor incrementing ref count from %d to %d for implict byref V%02d\n",
                lcl->lvRefCnt(RCS_EARLY), lcl->lvRefCnt(RCS_EARLY) + 1, lclNum);
        lcl->incLvRefCnt(1, RCS_EARLY);

        // See if this struct is an argument to a call. This information is recorded
        // via the weighted early ref count for the local, and feeds the undo promotion
        // heuristic.
        //
        // It can be approximate, so the pattern match below need not be exhaustive.
        // But the pattern should at least subset the implicit byref cases that are
        // handled in fgCanFastTailCall and abiMakeImplicityByRefStructArgCopy.
        //
        // CALL(OBJ(ADDR(LCL_VAR...)))

        // TODO-MIKE-Cleanup: The OBJ check is likely useless since the importer no
        // longer wraps struct args in OBJs.

        if (((m_ancestors.Height() >= 4) && m_ancestors.Top(0)->OperIs(GT_LCL_VAR) &&
             m_ancestors.Top(1)->OperIs(GT_ADDR) && m_ancestors.Top(2)->OperIs(GT_OBJ) &&
             m_ancestors.Top(3)->OperIs(GT_CALL)) ||
            ((m_ancestors.Height() >= 2) && m_ancestors.Top(0)->OperIs(GT_LCL_VAR) &&
             m_ancestors.Top(0)->TypeIs(TYP_STRUCT) && m_ancestors.Top(1)->OperIs(GT_CALL)))
        {
            JITDUMP("LocalAddressVisitor incrementing weighted ref count from %f to %f"
                    " for implict byref V%02d arg passed to call\n",
                    lcl->lvRefCntWtd(RCS_EARLY), lcl->lvRefCntWtd(RCS_EARLY) + 1, lclNum);
            lcl->incLvRefCntWtd(1, RCS_EARLY);
        }
    }

    GenTreeUnOp* NewBitCastNode(var_types type, GenTree* op)
    {
        return m_compiler->gtNewBitCastNode(type, op);
    }

    GenTreeLclVar* NewLclVarNode(var_types type, unsigned lclNum)
    {
        assert((type == m_compiler->lvaGetDesc(lclNum)->GetType()) ||
               (genActualType(type) == genActualType(m_compiler->lvaGetDesc(lclNum)->GetType())));

        return m_compiler->gtNewLclvNode(lclNum, type);
    }

    GenTreeIntCon* NewIntConNode(var_types type, ssize_t value)
    {
        return m_compiler->gtNewIconNode(value, type);
    }

#ifdef FEATURE_SIMD
    GenTreeHWIntrinsic* NewInsertElement(
        var_types type, unsigned index, var_types elementType, GenTree* dest, GenTree* value)
    {
        return m_compiler->gtNewSimdWithElementNode(type, elementType, dest, m_compiler->gtNewIconNode(index), value);
    }
#endif
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
    JITDUMP("\n*************** In fgMarkAddressExposedLocals()\n");

#if (defined(TARGET_AMD64) && !defined(UNIX_AMD64_ABI)) || defined(TARGET_ARM64)
    lvaResetImplicitByRefParamsRefCount();
#endif

    LocalAddressVisitor visitor(this);
#ifdef FEATURE_SIMD
    SIMDCoalescingBuffer buffer;
#endif

    for (BasicBlock* block = fgFirstBB; block != nullptr; block = block->bbNext)
    {
        // Make the current basic block address available globally
        compCurBB = block;

#ifdef FEATURE_SIMD
        buffer.Clear();
#endif

        for (Statement* stmt : block->Statements())
        {
            visitor.VisitStmt(stmt);

#ifdef FEATURE_SIMD
            if (opts.OptimizationEnabled() && buffer.Add(this, stmt))
            {
                buffer.Coalesce(this, block);

                // Since we generated a new address node which didn't exist before,
                // we should expose this address manually here.
                // TODO-ADDR: Remove this when LocalAddressVisitor transforms all
                // local field access into LCL_FLDs, at that point we would be
                // combining 2 existing LCL_FLDs or 2 FIELDs that do not reference
                // a local and thus cannot result in a new address exposed local.
                // Note that this also results in overcounting of implicit byref
                // parameters, that may be a CQ issue but SIMD coalescing is rare
                // so it's not worth fixing now.
                visitor.VisitStmt(stmt);
            }
#endif
        }
    }

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
    lvaRetypeImplicitByRefParams();
#endif
}

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64) || defined(TARGET_X86)

class IndirectParamMorphVisitor final : public GenTreeVisitor<IndirectParamMorphVisitor>
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

    IndirectParamMorphVisitor(Compiler* comp) : GenTreeVisitor<IndirectParamMorphVisitor>(comp)
    {
    }

    void VisitStmt(Statement* stmt)
    {
        WalkTree(stmt->GetRootNodePointer(), nullptr);

#ifdef DEBUG
        if (m_compiler->verbose && m_stmtModified)
        {
            printf("IndirectParamMorphVisitor modified statement:\n");
            m_compiler->gtDispTree(stmt->GetRootNode());
        }
#endif
    }

    fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
    {
        GenTree* node = *use;

        switch (node->OperGet())
        {
#if (defined(TARGET_AMD64) && !defined(UNIX_AMD64_ABI)) || defined(TARGET_ARM64)
            case GT_LCL_VAR_ADDR:
            case GT_LCL_FLD_ADDR:
                MorphImplicitByRefParamAddr(node->AsLclVarCommon());
                return Compiler::WALK_SKIP_SUBTREES;

            case GT_ADDR:
                if (node->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR, GT_LCL_FLD))
                {
                    MorphImplicitByRefParam(node);
                    return Compiler::WALK_SKIP_SUBTREES;
                }
                return Compiler::WALK_CONTINUE;

            case GT_LCL_VAR:
            case GT_LCL_FLD:
                MorphImplicitByRefParam(node);
                return Compiler::WALK_SKIP_SUBTREES;
#elif defined(TARGET_X86)
            case GT_LCL_VAR_ADDR:
            case GT_LCL_FLD_ADDR:
                MorphVarargsStackParamAddr(node->AsLclVarCommon());
                return Compiler::WALK_SKIP_SUBTREES;

            case GT_LCL_VAR:
            case GT_LCL_FLD:
                MorphVarargsStackParam(node->AsLclVarCommon());
                return Compiler::WALK_SKIP_SUBTREES;
#endif

            default:
                return Compiler::WALK_CONTINUE;
        }
    }

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
    void MorphImplicitByRefParamAddr(GenTreeLclVarCommon* lclAddrNode)
    {
        assert(lclAddrNode->OperIs(GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR));

        LclVarDsc* lcl = m_compiler->lvaGetDesc(lclAddrNode);

        if (lcl->IsImplicitByRefParam())
        {
            // Can't assert lvAddrExposed/lvDoNotEnregister because lvaRetypeImplicitByRefParams
            // already cleared both of them.
            // assert(lclVarDsc->lvAddrExposed);
            // assert(lclVarDsc->lvDoNotEnregister);

            // Locals referenced by GT_LCL_VAR|FLD_ADDR cannot be enregistered and currently
            // lvaRetypeImplicitByRefParams undoes promotion of such arguments. In this case
            // lvFieldCnt remains set to the number of promoted fields.
            assert((lcl->lvFieldLclStart == 0) || (lcl->lvFieldCnt != 0));

            if (lclAddrNode->OperIs(GT_LCL_FLD_ADDR))
            {
                unsigned      lclNum   = lclAddrNode->GetLclNum();
                unsigned      lclOffs  = lclAddrNode->AsLclFld()->GetLclOffs();
                FieldSeqNode* fieldSeq = lclAddrNode->AsLclFld()->GetFieldSeq();

                // LocalAddressVisitor should not create unnecessary LCL_FLD_ADDR nodes.
                assert((lclOffs != 0) || (fieldSeq != FieldSeqStore::NotAField()));

                GenTree* add = lclAddrNode;
                add->ChangeOper(GT_ADD);
                add->SetType(TYP_BYREF);
                add->AsOp()->SetOp(0, m_compiler->gtNewLclvNode(lclNum, TYP_BYREF));
                add->AsOp()->SetOp(1, m_compiler->gtNewIconNode(lclOffs, fieldSeq));
                add->gtFlags = GTF_EMPTY;
            }
            else
            {
                lclAddrNode->ChangeOper(GT_LCL_VAR);
                lclAddrNode->SetType(TYP_BYREF);
                lclAddrNode->gtFlags = GTF_EMPTY;
            }

            INDEBUG(m_stmtModified = true;)
        }
        else if (lcl->IsPromotedField() &&
                 m_compiler->lvaGetDesc(lcl->GetPromotedFieldParentLclNum())->IsImplicitByRefParam())
        {
            // This was a field reference to an implicit-by-reference struct parameter that was dependently
            // promoted and now it is being demoted; update it to reference the original parameter.

            assert(lcl->GetPromotedFieldSeq() != nullptr);

            unsigned      lclNum   = lcl->GetPromotedFieldParentLclNum();
            unsigned      lclOffs  = lcl->GetPromotedFieldOffset();
            FieldSeqNode* fieldSeq = lcl->GetPromotedFieldSeq();

            if (lclAddrNode->OperIs(GT_LCL_FLD_ADDR))
            {
                lclOffs += lclAddrNode->AsLclFld()->GetLclOffs();
                fieldSeq = m_compiler->GetFieldSeqStore()->Append(fieldSeq, lclAddrNode->AsLclFld()->GetFieldSeq());
            }

            // Change LCL_VAR|FLD_ADDR(paramPromotedField) into ADD(LCL_VAR<BYREF>(param), offset)
            GenTree* add = lclAddrNode;
            add->ChangeOper(GT_ADD);
            add->SetType(TYP_BYREF);
            add->AsOp()->SetOp(0, m_compiler->gtNewLclvNode(lclNum, TYP_BYREF));
            add->AsOp()->SetOp(1, m_compiler->gtNewIconNode(lclOffs, fieldSeq));
            add->gtFlags = GTF_EMPTY;

            INDEBUG(m_stmtModified = true;)
        }
    }

    void MorphImplicitByRefParam(GenTree* tree)
    {
        assert(tree->OperIs(GT_LCL_VAR, GT_LCL_FLD) ||
               (tree->OperIs(GT_ADDR) && tree->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR, GT_LCL_FLD)));

        GenTreeLclVarCommon* lclNode = (tree->OperIs(GT_ADDR) ? tree->AsUnOp()->GetOp(0) : tree)->AsLclVarCommon();
        unsigned             lclNum  = lclNode->GetLclNum();
        LclVarDsc*           lcl     = m_compiler->lvaGetDesc(lclNode);

        if (lcl->IsImplicitByRefParam())
        {
            // lvaRetypeImplicitByRefParams creates LCL_VAR nodes that reference
            // implicit byref params and are already TYP_BYREF, ignore them.
            if (lclNode->OperIs(GT_LCL_VAR) && lclNode->TypeIs(TYP_BYREF))
            {
                return;
            }

            assert(varTypeIsStruct(lclNode->GetType()) || lclNode->OperIs(GT_LCL_FLD));

            if ((lcl->lvFieldLclStart != 0) && (lcl->lvFieldCnt == 0))
            {
                // lvaRetypeImplicitByRefParams created a new promoted struct local to represent this
                // param. Rewrite this to refer to the new local. We should never encounter a LCL_FLD
                // because promotion is aborted if it turns out that it is dependent.

                assert(lclNode->OperIs(GT_LCL_VAR));
                assert(varTypeIsStruct(m_compiler->lvaGetDesc(lcl->lvFieldLclStart)->GetType()));

                lclNode->SetLclNum(lcl->lvFieldLclStart);
            }
            else
            {
                GenTreeIntCon* offset = nullptr;

                if (lclNode->OperIs(GT_LCL_FLD))
                {
                    unsigned      lclOffs  = lclNode->AsLclFld()->GetLclOffs();
                    FieldSeqNode* fieldSeq = lclNode->AsLclFld()->GetFieldSeq();

                    if ((lclOffs != 0) || (fieldSeq != FieldSeqStore::NotAField()))
                    {
                        offset = m_compiler->gtNewIconNode(lclOffs, fieldSeq);
                    }
                }

                if (tree->OperIs(GT_ADDR))
                {
                    if (offset != nullptr)
                    {
                        // Change ADDR<BYREF|I_IMPL>(LCL_FLD<>(param)) into ADD(LCL_VAR<BYREF>(param), lclOffs))
                        lclNode->SetType(TYP_BYREF);
                        lclNode->gtFlags = GTF_EMPTY;

                        tree->ChangeOper(GT_ADD);
                        tree->SetType(TYP_BYREF);
                        tree->AsOp()->SetOp(0, lclNode);
                        tree->AsOp()->SetOp(1, offset);
                        tree->gtFlags = GTF_EMPTY;
                    }
                    else
                    {
                        // Change ADDR<BYREF|I_IMPL>(LCL_VAR<STRUCT>(param)) into LCL_VAR<BYREF>(param)
                        tree->ChangeOper(GT_LCL_VAR);
                        tree->SetType(TYP_BYREF);
                        tree->AsLclVar()->SetLclNum(lclNum);
                        tree->gtFlags = GTF_EMPTY;
                    }
                }
                else
                {
                    GenTree* addr = m_compiler->gtNewLclvNode(lclNum, TYP_BYREF);

                    if (offset != nullptr)
                    {
                        addr = m_compiler->gtNewOperNode(GT_ADD, TYP_BYREF, addr, offset);
                    }

                    ClassLayout* layout = nullptr;

                    if (varTypeIsStruct(lclNode->GetType()))
                    {
                        layout = lclNode->OperIs(GT_LCL_VAR) ? lcl->GetImplicitByRefParamLayout()
                                                             : lclNode->AsLclFld()->GetLayout(m_compiler);
                    }

                    if (layout != nullptr)
                    {
                        tree->ChangeOper(GT_OBJ);
                        tree->AsObj()->SetLayout(layout);
                        tree->AsObj()->SetAddr(addr);
                        tree->AsObj()->SetKind(StructStoreKind::Invalid);
                    }
                    else
                    {
                        tree->ChangeOper(GT_IND);
                    }

                    tree->AsIndir()->SetAddr(addr);
                    tree->gtFlags = GTF_GLOB_REF | GTF_IND_NONFAULTING;
                }
            }

            INDEBUG(m_stmtModified = true;)
        }
        else if (lcl->IsPromotedField() &&
                 m_compiler->lvaGetDesc(lcl->GetPromotedFieldParentLclNum())->IsImplicitByRefParam())
        {
            // This was a field reference to an implicit-by-reference struct parameter that was
            // dependently promoted and now it is being demoted; update it to a field reference
            // off the original parameter.

            assert(lcl->GetType() != TYP_STRUCT);
            assert(lcl->GetPromotedFieldSeq() != nullptr);

            unsigned      lclNum   = lcl->GetPromotedFieldParentLclNum();
            unsigned      lclOffs  = lcl->GetPromotedFieldOffset();
            FieldSeqNode* fieldSeq = lcl->GetPromotedFieldSeq();

            lcl = m_compiler->lvaGetDesc(lclNum);

            if (lclNode->OperIs(GT_LCL_FLD))
            {
                lclOffs += lclNode->AsLclFld()->GetLclOffs();
                fieldSeq = m_compiler->GetFieldSeqStore()->Append(fieldSeq, lclNode->AsLclFld()->GetFieldSeq());
            }

            GenTreeIntCon* offset = m_compiler->gtNewIconNode(lclOffs, fieldSeq);

            if (tree->OperIs(GT_ADDR))
            {
                // Change ADDR(LCL_VAR(paramPromotedField)) into ADD(LCL_VAR<BYREF>(param), offset)
                lclNode->SetLclNum(lclNum);
                lclNode->SetType(TYP_BYREF);
                lclNode->gtFlags = GTF_EMPTY;

                tree->ChangeOper(GT_ADD);
                tree->AsOp()->SetOp(0, lclNode);
                tree->AsOp()->SetOp(1, offset);
                tree->gtFlags = GTF_EMPTY;
            }
            else
            {
                // Change LCL_VAR<fieldType>(paramPromotedField) into IND<fieldType>(ADD(LCL_VAR<BYREF>(param), offset))

                if (lclNode->TypeIs(TYP_STRUCT))
                {
                    ClassLayout* layout = nullptr;

                    if (lclNode->OperIs(GT_LCL_FLD))
                    {
                        layout = lclNode->AsLclFld()->GetLayout(m_compiler);
                    }
                    else
                    {
                        layout = lcl->GetImplicitByRefParamLayout();
                    }

                    tree->ChangeOper(GT_OBJ);
                    tree->AsObj()->SetLayout(layout);
                }
                else
                {
                    tree->ChangeOper(GT_IND);
                }

                lclNode = m_compiler->gtNewLclvNode(lclNum, TYP_BYREF);

                tree->AsIndir()->SetAddr(m_compiler->gtNewOperNode(GT_ADD, TYP_BYREF, lclNode, offset));
                tree->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;
            }

            INDEBUG(m_stmtModified = true;)
        }
    }
#elif defined(TARGET_X86)
    void MorphVarargsStackParamAddr(GenTreeLclVarCommon* lclNode)
    {
        assert(lclNode->OperIs(GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR));

        if (!IsVarargsStackParam(lclNode))
        {
            return;
        }

        GenTree* base   = m_compiler->gtNewLclvNode(m_compiler->lvaVarargsBaseOfStkArgs, TYP_I_IMPL);
        GenTree* offset = GetVarargsStackParamOffset(lclNode);
        GenTree* addr   = lclNode;

        addr->ChangeOper(GT_ADD);
        addr->SetType(TYP_I_IMPL);
        addr->AsOp()->SetOp(0, base);
        addr->AsOp()->SetOp(1, offset);

        INDEBUG(m_stmtModified = true;)
    }

    void MorphVarargsStackParam(GenTreeLclVarCommon* lclNode)
    {
        assert(lclNode->OperIs(GT_LCL_VAR, GT_LCL_FLD));

        if (!IsVarargsStackParam(lclNode))
        {
            return;
        }

        GenTree* base   = m_compiler->gtNewLclvNode(m_compiler->lvaVarargsBaseOfStkArgs, TYP_I_IMPL);
        GenTree* offset = GetVarargsStackParamOffset(lclNode);
        GenTree* addr   = m_compiler->gtNewOperNode(GT_ADD, TYP_I_IMPL, base, offset);
        GenTree* indir  = lclNode;

        if (varTypeIsStruct(lclNode->GetType()))
        {
            ClassLayout* layout = lclNode->OperIs(GT_LCL_VAR) ? m_compiler->lvaGetDesc(lclNode)->GetLayout()
                                                              : lclNode->AsLclFld()->GetLayout(m_compiler);

            indir->ChangeOper(GT_OBJ);
            indir->AsObj()->SetLayout(layout);
        }
        else
        {
            indir->ChangeOper(GT_IND);
        }

        indir->AsIndir()->SetAddr(addr);
        indir->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;

        INDEBUG(m_stmtModified = true;)
    }

    bool IsVarargsStackParam(GenTreeLclVarCommon* lclNode) const
    {
        LclVarDsc* lcl = m_compiler->lvaGetDesc(lclNode);
        return lcl->IsParam() && !lcl->IsRegParam() && (lclNode->GetLclNum() != m_compiler->lvaVarargsHandleArg);
    }

    GenTreeIntCon* GetVarargsStackParamOffset(GenTreeLclVarCommon* lclNode) const
    {
        int stkOffs = m_compiler->lvaGetDesc(lclNode)->GetStackOffset();
        stkOffs -= static_cast<int>(m_compiler->codeGen->intRegState.rsCalleeRegArgCount) * REGSIZE_BYTES;
        int lclOffs = lclNode->OperIs(GT_LCL_VAR, GT_LCL_VAR_ADDR) ? 0 : lclNode->AsLclFld()->GetLclOffs();
        return m_compiler->gtNewIconNode(-stkOffs + lclOffs, TYP_I_IMPL);
    }
#endif // !TARGET_X86
};

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
// Reset the ref count of implicit byref params; fgMarkAddressTakenLocals
// will increment it per appearance of implicit byref param so that call
// arg morphing can do an optimization for single-use implicit byref
// params whose single use is as an outgoing call argument.
void Compiler::lvaResetImplicitByRefParamsRefCount()
{
    JITDUMP("\n*************** In lvaResetImplicitByRefParamsRefCount()\n");

    lvaRefCountState          = RCS_EARLY;
    lvaHasImplicitByRefParams = false;

    for (unsigned lclNum = 0; lclNum < info.compArgsCount; ++lclNum)
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum);

        if (lcl->IsImplicitByRefParam())
        {
            // We haven't use ref counts until now so they should be 0.
            assert(lcl->lvRefCnt(RCS_EARLY) == 0);
            assert(lcl->lvRefCntWtd(RCS_EARLY) == 0);

            lvaHasImplicitByRefParams = true;
        }
    }
}

// Change the type of implicit byref parameters from struct to byref.
// Also choose (based on address-exposed analysis) which struct promotions
// of implicit byrefs to keep or discard.
// For those which are kept, insert the appropriate initialization code.
// For those which are to be discarded, annotate the promoted field locals
// so that fgMorphIndirectParams will know to rewrite their appearances.
void Compiler::lvaRetypeImplicitByRefParams()
{
    JITDUMP("\n*************** In lvaRetypeImplicitByRefParams()\n");

    if (!lvaHasImplicitByRefParams)
    {
        return;
    }

    for (unsigned lclNum = 0; lclNum < info.compArgsCount; lclNum++)
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum);

        if (!lcl->IsImplicitByRefParam())
        {
            continue;
        }

        if (!lcl->IsPromoted())
        {
            // Make sure lvFieldCnt is 0 if we did not promote, lvaDemoteImplicitByRefParams
            // relies on it to know when to cleanup unused promoted fields.

            assert(lcl->lvFieldCnt == 0);
        }
        else
        {
            // This implicit-byref was promoted; create a new temp to represent the
            // promoted struct before rewriting this parameter as a pointer.

            // If the promotion is dependent, the promoted temp would just be committed
            // to memory anyway, so we'll rewrite its appearances to be indirections
            // through the pointer parameter, the same as we'd do for this
            // parameter if it weren't promoted at all (otherwise the initialization
            // of the new temp would just be a needless memcpy at method entry).
            //
            // Otherwise, see how many appearances there are. We keep two early ref counts:
            // total number of references to the struct or some field, and how many of these
            // are arguments to calls. We undo promotion unless we see enough non-call uses.

            unsigned totalAppearances = lcl->lvRefCnt(RCS_EARLY);
            unsigned callAppearances  = static_cast<unsigned>(lcl->lvRefCntWtd(RCS_EARLY));
            assert(totalAppearances >= callAppearances);
            unsigned nonCallAppearances  = totalAppearances - callAppearances;
            bool     isDependentPromoted = lcl->IsDependentPromoted();

            bool undoPromotion = isDependentPromoted || (nonCallAppearances <= lcl->lvFieldCnt);

            JITDUMP("%s promotion of implicit byref V%02u: %s total: %u non-call: %u fields: %u\n",
                    undoPromotion ? "Undoing" : "Keeping", lclNum, isDependentPromoted ? "dependent;" : "",
                    totalAppearances, nonCallAppearances, lcl->lvFieldCnt);

            if (undoPromotion)
            {
                for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); i++)
                {
                    LclVarDsc* fieldLcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

                    // Leave lvParentLcl pointing to the parameter so that fgMorphIndirectParams
                    // will know to rewrite appearances of this local.
                    assert(fieldLcl->lvParentLcl == lclNum);

                    // The fields shouldn't inherit any register preferences from the parameter.
                    fieldLcl->lvIsParam       = false;
                    fieldLcl->lvIsRegArg      = false;
                    fieldLcl->lvIsMultiRegArg = false;
                    fieldLcl->SetArgReg(REG_NA);
#if FEATURE_MULTIREG_ARGS
                    fieldLcl->SetOtherArgReg(REG_NA);
#endif
                }

                // Reset lvPromoted since the param is no longer promoted but keep lvFieldLclStart
                // and lvFieldCnt unchanged so lvaDemoteImplicitByRefParams can find the fields
                // to "delete" them.
                lcl->lvPromoted = false;
            }
            else
            {
                unsigned structLclNum = lvaNewTemp(lcl->GetLayout(), false DEBUGARG("promoted implicit byref param"));
                // Update varDsc since lvaGrabTemp might have re-allocated the var dsc array.
                lcl = lvaGetDesc(lclNum);

                LclVarDsc* structLcl       = lvaGetDesc(structLclNum);
                structLcl->lvPromoted      = true;
                structLcl->lvFieldLclStart = lcl->lvFieldLclStart;
                structLcl->lvFieldCnt      = lcl->lvFieldCnt;

                structLcl->lvContainsHoles   = lcl->lvContainsHoles;
                structLcl->lvCustomLayout    = lcl->lvCustomLayout;
                structLcl->lvAddrExposed     = lcl->lvAddrExposed;
                structLcl->lvDoNotEnregister = lcl->lvDoNotEnregister;

#ifdef DEBUG
                structLcl->lvLclBlockOpAddr   = lcl->lvLclBlockOpAddr;
                structLcl->lvLclFieldExpr     = lcl->lvLclFieldExpr;
                structLcl->lvLiveInOutOfHndlr = lcl->lvLiveInOutOfHndlr;
                structLcl->lvLiveAcrossUCall  = lcl->lvLiveAcrossUCall;
                structLcl->lvKeepType         = true;
#endif // DEBUG

#if defined(TARGET_WINDOWS) && defined(TARGET_ARM64)
                // TODO-MIKE-Review: What do varargs/HFA have to do with this temp?!?
                if (info.compIsVarArgs)
                {
                    structLcl->SetIsHfa(false);
                }
#endif

                fgEnsureFirstBBisScratch();
                GenTree* lhs  = gtNewLclvNode(structLclNum, lcl->GetType());
                GenTree* addr = gtNewLclvNode(lclNum, TYP_BYREF);
                GenTree* rhs  = gtNewObjNode(structLcl->GetType(), structLcl->GetLayout(), addr);
                fgNewStmtAtBeg(fgFirstBB, gtNewAssignNode(lhs, rhs));

                // Update the locals corresponding to the promoted fields.

                for (unsigned i = 0; i < structLcl->GetPromotedFieldCount(); i++)
                {
                    LclVarDsc* fieldLcl = lvaGetDesc(structLcl->GetPromotedFieldLclNum(i));

                    // Set the new parent.
                    fieldLcl->lvParentLcl = structLclNum;

                    // The fields shouldn't inherit any register preferences from the parameter.
                    fieldLcl->lvIsParam       = false;
                    fieldLcl->lvIsRegArg      = false;
                    fieldLcl->lvIsMultiRegArg = false;
                    fieldLcl->SetArgReg(REG_NA);
#if FEATURE_MULTIREG_ARGS
                    fieldLcl->SetOtherArgReg(REG_NA);
#endif
                }

                // Reset lvPromoted since the param is no longer promoted but set lvFieldLclStart
                // to the new local's number so fgMorphIndirectParams knows how to replace param
                // references. Set lvFieldCnt to 0 so lvaDemoteImplicitByRefParams doesn't
                // attempt to "delete" the promoted fields as unused.

                lcl->lvPromoted      = false;
                lcl->lvFieldLclStart = structLclNum;
                lcl->lvFieldCnt      = 0;
            }
        }

        // Since the parameter in this position is really a pointer, its type is TYP_BYREF.
        lcl->SetType(TYP_BYREF);

        // Since this previously was a TYP_STRUCT and we have changed it to a TYP_BYREF
        // make sure that the following flag is not set as these will force SSA to
        // exclude tracking/enregistering these LclVars. (see SsaBuilder::IncludeInSsa)
        lcl->lvOverlappingFields = false;

        // The struct parameter may have had its address taken, but the pointer parameter
        // cannot -- any uses of the struct parameter's address are uses of the pointer
        // parameter's value, and there's no way for the MSIL to reference the pointer
        // parameter's address.  So clear the address-taken bit for the parameter.
        lcl->lvAddrExposed     = false;
        lcl->lvDoNotEnregister = false;

        // This should not be converted to a double in stress mode,
        // because it is really a pointer.
        INDEBUG(lcl->lvKeepType = true;)

        JITDUMP("Changed the type of struct parameter V%02d to TYP_BYREF.\n", lclNum);
    }
}
#endif // WINDOWS_AMD64_ABI || TARGET_ARM64

// Traverse the entire statement tree and morph implicit byref or
// x86 vararg stack parameter references in it.
void Compiler::fgMorphIndirectParams(Statement* stmt)
{
    assert(fgGlobalMorph);

#if defined(TARGET_X86)
    if (!info.compIsVarArgs)
#else
    if (!lvaHasImplicitByRefParams)
#endif
    {
        return;
    }

    IndirectParamMorphVisitor visitor(this);
    visitor.VisitStmt(stmt);
}

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
// Clear annotations for any implicit byrefs params that struct promotion
// asked to promote. Appearances of these have now been rewritten by
// fgMorphIndirectParams using indirections from the pointer parameter
// or references to the promoted fields, as appropriate.
void Compiler::lvaDemoteImplicitByRefParams()
{
    if (!lvaHasImplicitByRefParams)
    {
        return;
    }

    for (unsigned lclNum = 0; lclNum < info.compArgsCount; lclNum++)
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum);

        if (!lcl->IsImplicitByRefParam())
        {
            continue;
        }

        assert(!lcl->IsPromoted());

        if (lcl->lvFieldCnt != 0)
        {
            // Promotion was undone so all the promoted fields are no longer used.

            for (unsigned i = 0; i < lcl->lvFieldCnt; i++)
            {
                LclVarDsc* fieldLcl = lvaGetDesc(lcl->lvFieldLclStart + i);
                assert(fieldLcl->lvParentLcl == lclNum);
                fieldLcl->lvParentLcl     = 0;
                fieldLcl->lvIsStructField = false;
                fieldLcl->lvAddrExposed   = false;
                INDEBUG(fieldLcl->lvReason = "unused implicit byref promoted field";)
            }

            lcl->lvFieldLclStart = 0;
            lcl->lvFieldCnt      = 0;
        }
    }
}
#endif // WINDOWS_AMD64_ABI || TARGET_ARM64

#endif // WINDOWS_AMD64_ABI || TARGET_ARM64 || TARGET_X86
