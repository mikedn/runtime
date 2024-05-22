// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "jitstd/algorithm.h"

class LocalAddressVisitor final : public GenTreeVisitor<LocalAddressVisitor>
{
    // During tree traversal every GenTree node produces a "value" that represents:
    //   - the address of local variable memory location, including an offset as well.
    //   - an unknown value - the result of a node we don't know how to process. This
    //     also includes the result of TYP_VOID nodes (or any other nodes that don't
    //     actually produce values in IR) in order to support the invariant that every
    //     node produces a value.
    //
    class Value
    {
        GenTree*      m_node;
        FieldSeqNode* m_fieldSeq = nullptr;
        LclVarDsc*    m_lcl      = nullptr;
        unsigned      m_offset   = 0;
        INDEBUG(mutable bool m_consumed = false;)

    public:
        Value(GenTree* node) : m_node(node)
        {
        }

        GenTree* Node() const
        {
            return m_node;
        }

        bool IsAddress() const
        {
            return m_lcl != nullptr;
        }

        LclVarDsc* Lcl() const
        {
            assert(IsAddress());

            return m_lcl;
        }

        unsigned Offset() const
        {
            assert(IsAddress());

            return m_offset;
        }

        FieldSeqNode* FieldSeq() const
        {
            return m_fieldSeq;
        }

        bool Cast(const Value& val, GenTreeCast* cast)
        {
            assert(!IsAddress());

            if (!val.IsAddress() || !cast->TypeIs(TYP_I_IMPL))
            {
                return false;
            }

            m_lcl      = val.m_lcl;
            m_offset   = val.m_offset;
            m_fieldSeq = val.m_fieldSeq;

            INDEBUG(val.Consume();)
            return true;
        }

        bool Add(const Value& op1, const Value& op2)
        {
            assert(!IsAddress());

            GenTreeIntCon* offset;
            const Value*   op;

            if (op1.IsAddress() && op2.Node()->IsIntCon())
            {
                op     = &op1;
                offset = op2.Node()->AsIntCon();
            }
            else if (op2.IsAddress() && op1.Node()->IsIntCon())
            {
                op     = &op2;
                offset = op1.Node()->AsIntCon();
            }
            else
            {
                return false;
            }

            ClrSafeInt<unsigned> newOffset =
                ClrSafeInt<unsigned>(op->m_offset) + ClrSafeInt<unsigned>(offset->GetValue());

            if (newOffset.IsOverflow())
            {
                return false;
            }

            m_lcl      = op->m_lcl;
            m_offset   = newOffset.Value();
            m_fieldSeq = FieldSeqNode::NotAField();

            INDEBUG(op1.Consume();)
            INDEBUG(op2.Consume();)
            return true;
        }

        void Address(GenTreeLclAddr* lclAddr)
        {
            assert(!IsAddress());

            m_lcl      = lclAddr->GetLcl();
            m_offset   = lclAddr->GetLclOffs();
            m_fieldSeq = lclAddr->GetFieldSeq();
        }

        void Address(LclVarDsc* lcl, unsigned lclOffs, FieldSeqNode* fieldSeq)
        {
            assert(!IsAddress());

            m_lcl      = lcl;
            m_offset   = lclOffs;
            m_fieldSeq = fieldSeq;
        }

        void Promote(LclVarDsc* fieldLcl, unsigned lclOffs, FieldSeqNode* fieldSeq)
        {
            assert(IsAddress());
            assert(fieldLcl->GetPromotedFieldParentLclNum() == m_lcl->GetLclNum());

            m_lcl      = fieldLcl;
            m_offset   = lclOffs;
            m_fieldSeq = fieldSeq;
        }

        bool FieldAddress(Value& val, GenTreeFieldAddr* field, FieldSeqStore* fieldSeqStore)
        {
            assert(!IsAddress());

            if (val.IsAddress())
            {
                ClrSafeInt<unsigned> newOffset =
                    ClrSafeInt<unsigned>(val.m_offset) + ClrSafeInt<unsigned>(field->GetOffset());

                if (newOffset.IsOverflow())
                {
                    return false;
                }

                m_lcl    = val.m_lcl;
                m_offset = newOffset.Value();

                if (field->MayOverlap())
                {
                    m_fieldSeq = FieldSeqStore::NotAField();
                }
                else
                {
                    m_fieldSeq = fieldSeqStore->Append(val.m_fieldSeq, field->GetFieldSeq());
                }
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

    ArrayStack<Value, 16> m_valueStack;
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

    void VisitStmt(Statement* stmt DEBUGARG(BasicBlock* block))
    {
#ifdef DEBUG
        char message[64];

        if (m_compiler->verbose)
        {
            sprintf_s(message, sizeof(message), "LocalAddressVisitor visiting statement " FMT_BB, block->bbNum);
            m_compiler->gtDispStmt(stmt, message);
            m_stmtModified = false;
        }
#endif // DEBUG

        WalkTree(stmt->GetRootNodePointer(), nullptr);

        // We could have a statement like IND_LOAD(LCL_ADDR) that EscapeLocation would simplify
        // to LCL_LOAD. But since it's unused (and currently can't have any side effects) we'll
        // just change the statement to NOP. This way we avoid complications associated with
        // passing a null user to EscapeLocation.
        // This doesn't seem to happen often, if ever. The importer tends to wrap such a tree
        // in a COMMA.
        if (TopValue(0).IsAddress())
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
                sprintf_s(message, sizeof(message), "LocalAddressVisitor modified statement " FMT_BB, block->bbNum);
                m_compiler->gtDispStmt(stmt, message);
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

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
        if (m_compiler->lvaRefCountState == RCS_MORPH)
        {
            if (node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD, GT_LCL_STORE, GT_LCL_STORE_FLD))
            {
                UpdateImplicitByRefParamRefCounts(node->AsLclVarCommon()->GetLcl());
            }
            else if (node->OperIs(GT_LCL_ADDR))
            {
                UpdateImplicitByRefParamRefCounts(node->AsLclAddr()->GetLcl());
            }
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

        switch (node->GetOper())
        {
            case GT_LCL_ADDR:
                assert(TopValue(0).Node() == node);

                TopValue(0).Address(node->AsLclAddr());
                break;

            case GT_LCL_LOAD:
                assert(TopValue(0).Node() == node);

                if (node->TypeIs(TYP_STRUCT))
                {
                    node        = MorphStructLclLoad(node->AsLclLoad(), user);
                    TopValue(0) = node;
                }
                break;

            case GT_LCL_LOAD_FLD:
                assert(TopValue(0).Node() == node);

                MorphLclLoadFld(node->AsLclLoadFld(), user);
                break;

            case GT_LCL_STORE:
                assert(TopValue(1).Node() == node);
                assert(TopValue(0).Node() == node->AsLclStore()->GetValue());

                EscapeValue(TopValue(0), node);
                PopValue();

                if (node->TypeIs(TYP_STRUCT))
                {
                    MorphStructLclStore(node->AsLclStore());
                }
                break;

            case GT_IND_LOAD_OBJ:
            case GT_IND_LOAD_BLK:
            case GT_IND_LOAD:
                assert(TopValue(1).Node() == node);
                assert(TopValue(0).Node() == node->AsIndir()->GetAddr());

                if (TopValue(0).IsAddress())
                {
                    MorphLocalIndLoad(node->AsIndir(), TopValue(0), user);
                }
                else
                {
                    EscapeValue(TopValue(0), node);
                }

                PopValue();
                break;

            case GT_IND_STORE:
                assert(TopValue(2).Node() == node);
                assert(TopValue(1).Node() == node->AsIndStore()->GetAddr());
                assert(TopValue(0).Node() == node->AsIndStore()->GetValue());

                if (TopValue(1).IsAddress())
                {
                    MorphLocalIndStore(node->AsIndStore(), TopValue(1));
                }
                else
                {
                    EscapeValue(TopValue(1), node);
                }

                EscapeValue(TopValue(0), node);
                PopValue();
                PopValue();
                break;

            case GT_IND_STORE_OBJ:
                assert(TopValue(2).Node() == node);
                assert(TopValue(1).Node() == node->AsIndStoreObj()->GetAddr());
                assert(TopValue(0).Node() == node->AsIndStoreObj()->GetValue());

                EscapeValue(TopValue(0), node);
                MorphIndStoreObj(node->AsIndStoreObj(), TopValue(1));

                PopValue();
                PopValue();
                break;

            case GT_FIELD_ADDR:
                assert(TopValue(1).Node() == node);
                assert(TopValue(0).Node() == node->AsFieldAddr()->GetAddr());

                if (!TopValue(1).FieldAddress(TopValue(0), node->AsFieldAddr(), m_compiler->GetFieldSeqStore()))
                {
                    EscapeValue(TopValue(0), node);
                }

                PopValue();
                break;

            case GT_ADD:
                assert(TopValue(2).Node() == node);
                assert(TopValue(1).Node() == node->AsOp()->GetOp(0));
                assert(TopValue(0).Node() == node->AsOp()->GetOp(1));

                if (node->gtOverflow() || !TopValue(2).Add(TopValue(1), TopValue(0)))
                {
                    EscapeValue(TopValue(1), node);
                    EscapeValue(TopValue(0), node);
                }

                PopValue();
                PopValue();
                break;

            case GT_SUB:
                assert(TopValue(2).Node() == node);
                assert(TopValue(1).Node() == node->AsOp()->GetOp(0));
                assert(TopValue(0).Node() == node->AsOp()->GetOp(1));

                if (!node->gtOverflow() && node->TypeIs(TYP_I_IMPL))
                {
                    const Value& v1 = TopValue(1);
                    const Value& v2 = TopValue(0);

                    // We could handle SUB(local addr, constant) but those don't seem to exist nor
                    // they're likely to be useful. Instead, handle SUB(local addr1, local addr2),
                    // which also doesn't seem to exist but it could serve an useful purpose: to
                    // obtain the offset of a struct field as a cheap constant.

                    if (v1.IsAddress() && v2.IsAddress() && (v1.Lcl() == v2.Lcl()))
                    {
                        node->ChangeToIntCon(static_cast<ssize_t>(v1.Offset()) - static_cast<ssize_t>(v2.Offset()));

                        INDEBUG(v1.Consume();)
                        INDEBUG(v2.Consume();)
                    }
                }

                if (node->OperIs(GT_SUB))
                {
                    EscapeValue(TopValue(1), node);
                    EscapeValue(TopValue(0), node);
                }

                PopValue();
                PopValue();
                break;

            case GT_CAST:
                assert(TopValue(1).Node() == node);
                assert(TopValue(0).Node() == node->AsCast()->GetOp(0));

                if (!TopValue(1).Cast(TopValue(0), node->AsCast()))
                {
                    EscapeValue(TopValue(0), node);
                }

                PopValue();
                break;

            case GT_RETURN:
                if (TopValue(0).Node() != node)
                {
                    assert(TopValue(1).Node() == node);
                    assert(TopValue(0).Node() == node->AsUnOp()->GetOp(0));

                    GenTreeUnOp* ret   = node->AsUnOp();
                    GenTree*     value = ret->GetOp(0);

                    if (value->OperIs(GT_LCL_LOAD))
                    {
                        // TODO-1stClassStructs: this block is a temporary workaround to keep diffs small, having
                        // `doNotEnregister` affect block init and copy transformations that affect many methods.
                        // I have a change that introduces more precise and effective solution for that, but it would
                        // be merged separately.

                        LclVarDsc* lcl = value->AsLclLoad()->GetLcl();

                        if ((m_compiler->info.retDesc.GetRegCount() == 1) && !lcl->IsImplicitByRefParam() &&
                            lcl->IsPromoted() && (lcl->GetPromotedFieldCount() > 1) && !varTypeIsSIMD(lcl->GetType()))
                        {
                            m_compiler->lvaSetDoNotEnregister(lcl DEBUGARG(Compiler::DNER_BlockOp));
                        }
                    }

                    EscapeValue(TopValue(0), node);
                    PopValue();

                    if (value->TypeIs(TYP_STRUCT) && IsMergedReturn(ret))
                    {
                        LclVarDsc* lcl = m_compiler->lvaGetDesc(m_compiler->genReturnLocal);

                        if (lcl->IsPromoted())
                        {
                            assert(lcl->GetPromotedFieldCount() == 1);
                            RetypeMergedReturnValue(ret, value, lcl);
                        }
                    }
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

    void EscapeValue(const Value& val, GenTree* user)
    {
        if (val.IsAddress())
        {
            EscapeAddress(val, user);
        }
        else
        {
            INDEBUG(val.Consume();)
        }
    }

    void EscapeAddress(const Value& val, GenTree* user)
    {
        assert(val.IsAddress());
        INDEBUG(val.Consume();)
        assert(user != nullptr);

        if (user->OperIs(GT_EQ, GT_NE) || (user->OperIs(GT_GT) && user->IsUnsigned()))
        {
            GenTree* op1 = user->AsOp()->GetOp(0);
            GenTree* op2 = user->AsOp()->GetOp(1);

            if (op2 == val.Node())
            {
                std::swap(op1, op2);
            }

            if (op2->IsIntegralConst(0))
            {
                op1->ChangeToIntCon(TYP_I_IMPL, 1);

                INDEBUG(m_stmtModified = true;)

                return;
            }

            // TODO-MIKE-CQ: Other comparisons results in the local being address exposed
            // even though that isn't necessary, DNER would be sufficient. However, such
            // cases are very rare - Dragon4 in corelib has a few and avoiding address
            // exposed in those case is unlikely to provide any benefits.
        }

        LclVarDsc* lcl      = val.Lcl();
        LclVarDsc* fieldLcl = nullptr;

        // Try to use the address of a promoted field, if any. We'll have to mark the local
        // address exposed anyway but if we can restrict this to just a promoted field we
        // can avoid dependent promotion of the entire struct local. This is somewhat dodgy
        // in the call case - we assume that if this address is passed as the "this" arg of
        // a call then only the promoted field will be accessed. But due to reinterpretation
        // we could end up with a call to a method that accesses other promoted fields, that
        // may not be P-DEP/DNER.

        // TODO-MIKE-Review: Perhaps we should use the promoted field only if we can avoid
        // P-DEP? If we can't avoid P-DEP and the local is anyway address exposed there's
        // no point in using promoted fields. Actually we shouldn't promote to begin with,
        // but the JIT's struct promotion is upside down...
        // This is done in part to avoid diffs due to old promotion code doing it.

        if (lcl->IsPromoted() && (val.FieldSeq() != nullptr) && val.FieldSeq()->IsField())
        {
            fieldLcl = FindPromotedField(lcl, val.Offset(), 1);

            if ((fieldLcl != nullptr) && (fieldLcl->GetPromotedFieldOffset() == val.Offset()))
            {
                lcl = fieldLcl;
            }
        }

        // In general we don't know how an exposed struct field address will be used - it may be used to
        // access only that specific field or it may be used to access other fields in the same struct
        // be using pointer/ref arithmetic.
        bool exposeParentLcl = lcl->IsPromotedField();

        if (exposeParentLcl)
        {
            if (GenTreeCall* call = user->IsCall())
            {
                // It seems reasonable to make an exception for the "this" arg of calls - it would
                // be highly unusual for a struct member method to attempt to access memory beyond
                // "this" instance. And calling struct member methods is common enough that attempting
                // to mark the entire struct as address exposed results in CQ regressions.

                if ((call->gtCallThisArg != nullptr) && (val.Node() == call->gtCallThisArg->GetNode()))
                {
                    exposeParentLcl = false;
                }
            }
            else if (user->OperIsAtomicOp())
            {
                assert(!user->TypeIs(TYP_STRUCT) && !lcl->TypeIs(TYP_STRUCT));

                if ((varTypeSize(user->GetType()) <= varTypeSize(lcl->GetType())) &&
                    (val.Node() == (user->IsCmpXchg() ? user->AsCmpXchg()->GetAddr() : user->AsOp()->GetOp(0))))
                {
                    exposeParentLcl = false;
                }
            }
        }

        m_compiler->lvaSetAddressExposed(exposeParentLcl ? m_compiler->lvaGetDesc(lcl->GetPromotedFieldParentLclNum())
                                                         : lcl);

#ifdef TARGET_64BIT
        // If the address of a variable is passed in a call and the allocation size of the variable
        // is 32 bits we will quirk the size to 64 bits. Some PInvoke signatures incorrectly specify
        // a ByRef to an INT32 when they actually write a SIZE_T or INT64. There are cases where
        // overwriting these extra 4 bytes corrupts some data (such as a saved register) that leads
        // to A/V. Wheras previously the JIT64 codegen did not lead to an A/V.
        if (!lcl->IsParam() && !lcl->IsPromotedField() && varActualTypeIsInt(lcl->GetType()))
        {
            // TODO-Cleanup: This should simply check if the user is a call node, not if a call ancestor exists.
            if (Compiler::gtHasCallOnStack(&m_ancestors))
            {
                lcl->lvQuirkToLong = true;
                JITDUMP("Adding a quirk for the storage size of V%02u of type %s\n", val.Lcl()->GetLclNum(),
                        varTypeName(lcl->GetType()));
            }
        }
#endif // TARGET_64BIT

        if (lcl == fieldLcl)
        {
            Value fieldVal(val.Node());
            fieldVal.Address(lcl, 0, nullptr);
            CanonicalizeLocalAddress(fieldVal, user);
        }
        else
        {
            CanonicalizeLocalAddress(val, user);
        }
    }

    GenTree* MorphStructLclLoad(GenTreeLclLoad* load, GenTree* user)
    {
        assert(user != nullptr);

        LclVarDsc* lcl = load->GetLcl();

        if (lcl->IsPromoted() && (lcl->GetPromotedFieldCount() == 1))
        {
            // TODO-MIKE-Cleanup: The user could be null here (unused load).
            if (user->OperIs(GT_RETURN))
            {
                return PromoteSingleFieldStructLclLoadReturn(lcl, load, user->AsUnOp());
            }

            // Let's hope the importer doesn't produce STRUCT COMMAs again.
            noway_assert(user->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD, GT_IND_STORE_OBJ, GT_CALL));

            PromoteSingleFieldStructLclLoadStoreValue(lcl, load, user);
        }

        return load;
    }

    void MorphLclLoadFld(GenTreeLclLoadFld* load, GenTree* user)
    {
        assert(user != nullptr);

        LclVarDsc* lcl = load->GetLcl();

        if (!lcl->IsPromoted() || !PromoteLclLoadFld(load, lcl))
        {
            m_compiler->lvaSetDoNotEnregister(lcl DEBUGARG(Compiler::DNER_LocalField));
        }
    }

    void MorphStructLclStore(GenTreeLclStore* store)
    {
        assert(store->TypeIs(TYP_STRUCT));

        LclVarDsc* lcl   = store->GetLcl();
        GenTree*   value = store->GetValue();

        assert(!value->OperIs(GT_INIT_VAL));

        if (lcl->IsPromoted() && (lcl->GetPromotedFieldCount() == 1))
        {
            LclVarDsc* fieldLcl = m_compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(0));
            store->SetLcl(fieldLcl);
            store->SetType(fieldLcl->GetType());
            RetypeSingleFieldStructLclStore(store, value);
        }
        else if (!value->TypeIs(TYP_STRUCT) && !value->IsIntCon(0))
        {
            var_types type = value->GetType();

            ClassLayout*   fieldLayout = nullptr;
            FieldSeqNode*  fieldSeq    = GetFieldSequence(lcl->GetLayout()->GetClassHandle(), type, &fieldLayout);
            GenTreeLclFld* fieldStore  = store->ChangeToLclStoreFld(type, lcl, 0, fieldSeq, store->GetValue());

            if (varTypeIsSIMD(type) && (fieldLayout != nullptr) && (fieldLayout->GetSIMDType() == type))
            {
                fieldStore->SetLayout(fieldLayout, m_compiler);
            }

            m_compiler->lvaSetDoNotEnregister(lcl DEBUGARG(Compiler::DNER_LocalField));

            INDEBUG(m_stmtModified = true;)
        }
    }

    void MorphLocalIndLoad(GenTreeIndir* load, Value& addrVal, GenTree* user)
    {
        assert(load->OperIs(GT_IND_LOAD, GT_IND_LOAD_BLK, GT_IND_LOAD_OBJ));
        assert(addrVal.IsAddress());
        INDEBUG(addrVal.Consume();)
        assert(user != nullptr);

        LclVarDsc*    lcl      = addrVal.Lcl();
        var_types     lclType  = lcl->GetType();
        unsigned      lclSize  = lcl->GetTypeSize();
        unsigned      lclOffs  = addrVal.Offset();
        FieldSeqNode* fieldSeq = addrVal.FieldSeq();
        var_types     loadType = load->GetType();
        ClassLayout*  loadLayout;
        unsigned      loadSize;

        if (load->OperIs(GT_IND_LOAD))
        {
            loadLayout = nullptr;
            loadSize   = varTypeSize(loadType);
        }
        else
        {
            loadLayout = load->AsBlk()->GetLayout();
            loadSize   = loadLayout->GetSize();
        }

        if (lclOffs > UINT16_MAX)
        {
            CanonicalizeLocalIndLoad(load, addrVal);

            return;
        }

        if ((lclOffs > UINT32_MAX - loadSize) || (lclOffs + loadSize > lclSize))
        {
            CanonicalizeLocalIndLoad(load, addrVal);

            return;
        }

        if (lcl->IsPromoted())
        {
            if (LclVarDsc* fieldLcl = FindPromotedField(lcl, addrVal.Offset(), loadSize))
            {
                lcl     = fieldLcl;
                lclType = fieldLcl->GetType();
                lclOffs -= fieldLcl->GetPromotedFieldOffset();

                FieldSeqNode* fieldSeq = addrVal.FieldSeq();

                if ((fieldSeq != nullptr) && fieldSeq->IsField())
                {
                    fieldSeq = fieldSeq->RemovePrefix(fieldLcl->GetPromotedFieldSeq());

                    if (fieldSeq == addrVal.FieldSeq())
                    {
                        // There was no prefix, this means that the field access sequence doesn't
                        // match the promoted field sequence, ignore the field access sequence.
                        fieldSeq = FieldSeqNode::NotAField();
                    }
                }

                addrVal.Promote(fieldLcl, lclOffs, fieldSeq);
            }
        }

        // TODO-MIKE-Review: For now ignore volatile on a FIELD that accesses a promoted field,
        // to avoid diffs due to old promotion code ignoring volatile as well.

        if (load->IsVolatile() && (!load->GetAddr()->OperIs(GT_FIELD_ADDR) || !lcl->IsPromotedField()))
        {
            CanonicalizeLocalIndLoad(load, addrVal);

            return;
        }

        if (load->OperIs(GT_IND_LOAD_BLK))
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

            CanonicalizeLocalIndLoad(load, addrVal);

            return;
        }

        if ((lclOffs == 0) && (lclType != TYP_BLK))
        {
            // A non-struct local may be accessed via a struct indir due to reinterpretation in user
            // code or due to single field struct promotion. Reinterpretation isn't common (but it
            // does happen - e.g. ILCompiler.Reflection.ReadyToRun.dll reinterprets array references
            // as ImmutableArray) but promotion is quite common. If the indir is a call arg then we
            // can simply replace it with the local variable because it doesn't really matter if it's
            // a struct value or a primitive type value, we just need to put the value in the correct
            // register or stack slot.

            if ((loadType == TYP_STRUCT) && (lclType != TYP_STRUCT))
            {
                ClassLayout* loadLayout = load->AsIndLoadObj()->GetLayout();

                if (user->OperIs(GT_RETURN))
                {
                    if (PromoteSingleFieldStructReturn(addrVal, load, loadLayout, user->AsUnOp()))
                    {
                        return;
                    }
                }
                else
                {
                    // Let's hope the importer doesn't produce STRUCT COMMAs again.
                    noway_assert(user->OperIs(GT_LCL_STORE, GT_IND_STORE_OBJ, GT_CALL));

                    if (PromoteSingleFieldStructCopy(addrVal, load->AsIndLoadObj()))
                    {
                        return;
                    }
                }
            }
            else if (!varTypeIsStruct(loadType) && !varTypeIsStruct(lclType))
            {
                if (varTypeSize(loadType) == varTypeSize(lclType))
                {
                    if (varTypeIsSmall(loadType) && (varTypeIsUnsigned(loadType) != varTypeIsUnsigned(lclType)))
                    {
                        // There's no reason to write something like `*(short*)&ushortLocal` instead of just
                        // `(short)ushortLocal`, except that generics code can't do such casts and sometimes
                        // uses `Unsafe.As` as a substitute.

                        load->ChangeOper(GT_CAST);
                        load->AsCast()->SetCastType(loadType);
                        load->AsCast()->SetOp(0, NewLclLoad(lclType, lcl));
                        load->gtFlags = GTF_EMPTY;
                        INDEBUG(m_stmtModified = true);

                        return;
                    }

                    if (varTypeKind(loadType) == varTypeKind(lclType))
                    {
                        // Like in the signedness mismatch case above, it's not common to have `*(int*)&intLocal`
                        // but such cases may arise either from fancy generic code or, more likely, due to the
                        // inlining of primitive type methods. Turns out that having methods on primitive types,
                        // while elegant, can cause problems as any call to such a method requires taking the
                        // address of the primitive type local variable.

                        assert((loadType == lclType) || ((loadType == TYP_BOOL) && (lclType == TYP_UBYTE)) ||
                               ((loadType == TYP_UBYTE) && (lclType == TYP_BOOL)));

                        load->ChangeToLclLoad(loadType, lcl);
                        load->gtFlags = GTF_EMPTY;
                        INDEBUG(m_stmtModified = true);

                        return;
                    }

                    if (CanBitCastTo(loadType))
                    {
                        // Handle the relatively common case of floating point/integer reinterpretation.
                        // Also handle GC pointer/native int reinterpretation, some corelib tracing code
                        // uses object references as if they're normal pointers for logging purposes.

                        load->ChangeOper(GT_BITCAST);
                        load->AsUnOp()->SetOp(0, NewLclLoad(lclType, lcl));
                        load->gtFlags = GTF_EMPTY;
                        INDEBUG(m_stmtModified = true);

                        return;
                    }
                }
                else if (varTypeIsIntegral(loadType) && varTypeIsIntegral(lclType))
                {
                    // The load has to be smaller than the local, we already handled loads wider than the local.
                    assert(varTypeSize(loadType) < varTypeSize(lclType));

                    // Storing to a local via a smaller indirection is more difficult to handle, we need something
                    // like LCL_STORE(lcl, OR(lcl, value)) in the integer case plus some BITCASTs or intrinsics
                    // in the float case. CoreLib has a few cases in Vector128 & co. WithElement methods but these
                    // are normally recognized as intrinsics so it's not worth the trouble to handle this case.

                    // Loading via a smaller indirection isn't common either but this case can be easily handled
                    // by using a CAST. There's no reason to write something like `*(short*)&intLocal` instead of
                    // just `(short)intLocal`, except that generics code can't do such casts and sometimes uses
                    // `Unsafe.As` as a substitute.

                    load->ChangeOper(GT_CAST);
                    load->AsCast()->SetCastType(loadType);
                    load->AsCast()->SetOp(0, NewLclLoad(lclType, lcl));
                    load->gtFlags = GTF_EMPTY;
                    INDEBUG(m_stmtModified = true);

                    return;
                }
            }
        }

#ifdef FEATURE_SIMD
        if (varTypeIsSIMD(lclType) && (loadType == TYP_FLOAT) && (lclOffs % 4 == 0) && lcl->lvIsUsedInSIMDIntrinsic() &&
            !lcl->lvDoNotEnregister)
        {
            // Recognize fields X/Y/Z/W of Vector2/3/4. These fields have type FLOAT so this is the only type
            // we recognize here but any other type supported by GetItem/SetItem would work. But other vector
            // types don't have fields and the only way this would be useful is if someone uses unsafe code
            // to access the vector elements. That's not very useful as the other vector types offer element
            // access by other means - Vector's indexer and Vector64/128/256's GetElement.

            // TODO-MIKE-CQ: Doing this here is a bit of a problem if the local variable ends up in memory,
            // if it is DNER for whatever reasons (we haven't determined that exactly at this point) or if
            // it is an implicit byref param. We could produce a LCL_LOAD_FLD here, without DNERing the local
            // and let morph convert it to GetItem/SetItem or leave it alone if the local was already DNERed
            // for other reasons. But creating a LCL_LOAD_FLD without DNERing the corresponding local seems
            // somewhat risky at the moment.
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

            load->ChangeOper(GT_HWINTRINSIC);
            load->AsHWIntrinsic()->SetIntrinsic(NI_Vector128_GetElement, TYP_FLOAT, 16, 2);
            load->AsHWIntrinsic()->SetOp(0, NewLclLoad(lclType, lcl));
            load->AsHWIntrinsic()->SetOp(1, NewIntConNode(TYP_INT, lclOffs / 4));
            INDEBUG(m_stmtModified = true;)

            return;
        }
#endif // FEATURE_SIMD

        // For STRUCT locals, if the indir layout doesn't match and the local is promoted
        // then ignore the indir layout to avoid having to make a LCL_LOAD_FLD and dependent
        // promote the local. The indir layout isn't really needed anymore, since call arg
        // morphing uses the one from the call signature.

        // The only thing other than the ABI the layout influences is the GCness of stores
        // to the local, but in that case it really does make more sense to ignore the
        // indir layout and use the local variable layout as that is the "real" one when
        // it comes to GC. Reinterpreting a local variable in an attempt to avoid GC safe
        // copies doesn't make a lot of sense.

        // TODO-MIKE-Consider: This should work for non promoted locals as well but it's
        // not clear if it's worth doing and safe.
        // Avoiding LCL_FLDs may improve assertion copy propagation but on the other hand
        // this can create more assignments with different source and destination types
        // and it's not clear how well VN maps handles those.
        // Also, discarding type information is not that great in general and it may be
        // better to instead teach assertion propagation to deal with LCL_LOAD_FLDs.

        if ((lclOffs == 0) && (loadType == lclType) && ((loadType != TYP_STRUCT) || (loadLayout == lcl->GetLayout()) ||
                                                        (lcl->IsPromoted() && (loadSize == lclSize))))
        {
            load->ChangeToLclLoad(loadType, lcl);
        }
        else
        {
            if (!varTypeIsStruct(lclType))
            {
                fieldSeq = FieldSeqNode::NotAField();
            }
            else if ((fieldSeq != nullptr) && fieldSeq->IsField())
            {
                if (!varTypeIsStruct(loadType))
                {
#if 0
                    // TODO-MIKE-Cleanup: This should be removed, it's VN's job to deal with such type mismatches.
                    // For now just disable it instead of fixing importer's Span::_pointer field sequence as this
                    // generates less diffs.

                    // If we have an indirection node and a field sequence then they should have the same type.
                    // Otherwise it's best to forget the field sequence since the resulting LCL_LOAD_FLD doesn't
                    // match a real struct field. Value numbering protects itself from such mismatches but there
                    // doesn't seem to be any good reason to generate a LCL_LOAD_FLD with a mismatched field
                    // sequence only to have to ignore it later.

                    if (loadType != GetScalarFieldType(fieldSeq->GetTail()->GetFieldHandle()))
                    {
                        fieldSeq = nullptr;
                    }
#endif
                }
                else if (loadLayout != nullptr)
                {
                    if (loadLayout->GetClassHandle() != GetStructFieldType(fieldSeq->GetTail()->GetFieldHandle()))
                    {
                        fieldSeq = nullptr;
                    }
                }
            }

            load->ChangeToLclLoadFld(loadType, lcl, lclOffs, fieldSeq);

            if (loadLayout != nullptr)
            {
                load->AsLclLoadFld()->SetLayout(loadLayout, m_compiler);
            }

            m_compiler->lvaSetDoNotEnregister(lcl DEBUGARG(Compiler::DNER_LocalField));
        }

        GenTreeFlags flags = GTF_EMPTY;

        if (load->TypeIs(TYP_STRUCT) && user->IsCall())
        {
            flags |= GTF_DONT_CSE;
        }

        load->gtFlags = flags;

        INDEBUG(m_stmtModified = true;)
    }

    void MorphIndStoreObj(GenTreeIndStoreObj* store, Value& addrVal)
    {
        GenTree* value = store->GetValue();

        if ((value->GetType() != store->GetType()) && !value->IsIntCon(0))
        {
            assert(store->GetLayout()->GetSize() == varTypeSize(value->GetType()));

            store->SetOper(GT_IND_STORE);
            store->SetType(store->GetValue()->GetType());

            if (TopValue(1).IsAddress())
            {
                MorphLocalIndStore(store, addrVal);
            }
            else
            {
                EscapeValue(TopValue(1), store);
            }
        }
        else
        {
            if (TopValue(1).IsAddress())
            {
                MorphLocalIndStoreObj(store, addrVal);
            }
            else
            {
                EscapeValue(TopValue(1), store);
            }
        }
    }

    void MorphLocalIndStore(GenTreeIndir* store, Value& addrVal)
    {
        assert(store->OperIs(GT_IND_STORE) && (varTypeSize(store->GetType()) != 0));
        assert(addrVal.IsAddress());
        INDEBUG(addrVal.Consume());

        LclVarDsc*    lcl       = addrVal.Lcl();
        var_types     lclType   = lcl->GetType();
        unsigned      lclSize   = lcl->GetTypeSize();
        unsigned      lclOffs   = addrVal.Offset();
        FieldSeqNode* fieldSeq  = addrVal.FieldSeq();
        var_types     storeType = store->GetType();
        unsigned      storeSize = varTypeSize(storeType);

        if (lclOffs > UINT16_MAX)
        {
            CanonicalizeLocalIndStore(store, addrVal);

            return;
        }

        if ((lclOffs > UINT32_MAX - storeSize) || (lclOffs + storeSize > lclSize))
        {
            CanonicalizeLocalIndStore(store, addrVal);

            return;
        }

        if (lcl->IsPromoted())
        {
            if (LclVarDsc* fieldLcl = FindPromotedField(lcl, lclOffs, varTypeSize(storeType)))
            {
                lcl     = fieldLcl;
                lclType = fieldLcl->GetType();
                lclOffs -= fieldLcl->GetPromotedFieldOffset();

                if (lclOffs != 0)
                {
                    fieldSeq = FieldSeqNode::NotAField();
                }
                else if ((fieldSeq != nullptr) && fieldSeq->IsField())
                {
                    fieldSeq = fieldSeq->RemovePrefix(fieldLcl->GetPromotedFieldSeq());

                    if (fieldSeq == addrVal.FieldSeq())
                    {
                        // There was no prefix, this means that the field access sequence doesn't
                        // match the promoted field sequence, ignore the address field sequence.
                        fieldSeq = FieldSeqNode::NotAField();
                    }
                }

                addrVal.Promote(fieldLcl, lclOffs, fieldSeq);
            }
        }

        // TODO-MIKE-Review: For now ignore volatile on promoted field stores,
        // to avoid diffs due to old promotion code ignoring volatile as well.

        if (store->IsVolatile() && (!store->GetAddr()->OperIs(GT_FIELD_ADDR) || !lcl->IsPromotedField()))
        {
            CanonicalizeLocalIndStore(store, addrVal);
            return;
        }

        GenTree* value = store->GetValue();

        if ((lclOffs == 0) && (varTypeSize(storeType) == varTypeSize(lclType)))
        {
            bool isAssignable = varTypeKind(storeType) == varTypeKind(lclType);

            if (!isAssignable && CanBitCastTo(lclType))
            {
                value        = NewBitCastNode(lclType, value);
                isAssignable = true;
            }

            if (isAssignable)
            {
                store->ChangeToLclStore(lclType, lcl, value);
                INDEBUG(m_stmtModified = true);

                return;
            }
        }

#ifdef FEATURE_SIMD
        if (varTypeIsSIMD(lclType) && (storeType == TYP_FLOAT) && (lclOffs % 4 == 0) &&
            lcl->lvIsUsedInSIMDIntrinsic() && !lcl->IsImplicitByRefParam() && !lcl->lvDoNotEnregister)
        {
            // Recognize fields X/Y/Z/W of Vector2/3/4. These fields have type FLOAT so this is the only type
            // we recognize here but any other type supported by GetItem/SetItem would work. But other vector
            // types don't have fields and the only way this would be useful is if someone uses unsafe code
            // to access the vector elements. That's not very useful as the other vector types offer element
            // access by other means - Vector's indexer and Vector64/128/256's GetElement.

            // TODO-MIKE-CQ: Doing this here is a bit of a problem if the local variable ends up in memory,
            // if it is DNER for whatever reasons (we haven't determined that exactly at this point) or if
            // it is an implicit byref param. We could produce a LCL_STORE_FLD here, without DNERing the local
            // and let morph convert it to GetItem/SetItem or leave it alone if the local was already DNERed
            // for other reasons. But creating a LCL_STORE_FLD without DNERing the corresponding local seems
            // somewhat risky at the moment.
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

            value = NewInsertElement(lcl->GetType(), lclOffs / 4, TYP_FLOAT, NewLclLoad(lcl->GetType(), lcl), value);
            store->ChangeToLclStore(lclType, lcl, value);
            INDEBUG(m_stmtModified = true);

            return;
        }
#endif // FEATURE_SIMD

        if ((fieldSeq == nullptr) || !varTypeIsStruct(lclType))
        {
            fieldSeq = FieldSeqStore::NotAField();
        }

        store->ChangeToLclStoreFld(storeType, lcl, lclOffs, fieldSeq, value);

        m_compiler->lvaSetDoNotEnregister(lcl DEBUGARG(Compiler::DNER_LocalField));

        if (varTypeIsSmall(lclType))
        {
            // If LCL_STORE_FLD is used to store to a small type local then "normalize on store"
            // isn't possible so the local has to be "normalize on load". The only way to do this
            // is by making the local address exposed, which is a big hammer. But such an indirect
            // store is highly unusual so it's not worth the trouble to introduce another mechanism.
            m_compiler->lvaSetAddressExposed(lcl);
        }

        INDEBUG(m_stmtModified = true);
    }

    void MorphLocalIndStoreObj(GenTreeIndStoreObj* store, Value& addrVal)
    {
        INDEBUG(addrVal.Consume());

        LclVarDsc*    lcl         = addrVal.Lcl();
        var_types     lclType     = lcl->GetType();
        unsigned      lclSize     = lcl->GetTypeSize();
        unsigned      lclOffs     = addrVal.Offset();
        FieldSeqNode* fieldSeq    = addrVal.FieldSeq();
        var_types     storeType   = store->GetType();
        ClassLayout*  storeLayout = store->GetLayout();
        unsigned      storeSize   = storeLayout->GetSize();

        if (lclOffs > UINT16_MAX)
        {
            CanonicalizeLocalIndStore(store, addrVal);

            return;
        }

        if ((lclOffs > UINT32_MAX - storeSize) || (lclOffs + storeSize > lclSize))
        {
            CanonicalizeLocalIndStore(store, addrVal);

            return;
        }

        if (lcl->IsPromoted() && !lcl->lvDoNotEnregister)
        {
            if (LclVarDsc* fieldLcl = FindPromotedField(lcl, lclOffs, storeLayout->GetSize()))
            {
                lcl     = fieldLcl;
                lclType = fieldLcl->GetType();
                lclOffs -= fieldLcl->GetPromotedFieldOffset();

                if (lclOffs != 0)
                {
                    fieldSeq = FieldSeqNode::NotAField();
                }
                else if ((fieldSeq != nullptr) && fieldSeq->IsField())
                {
                    fieldSeq = fieldSeq->RemovePrefix(fieldLcl->GetPromotedFieldSeq());

                    if (fieldSeq == addrVal.FieldSeq())
                    {
                        // There was no prefix, this means that the field access sequence doesn't
                        // match the promoted field sequence, ignore the address field sequence.
                        fieldSeq = FieldSeqNode::NotAField();
                    }
                }

                addrVal.Promote(fieldLcl, lclOffs, fieldSeq);
            }
        }

        // TODO-MIKE-Review: For now ignore volatile on promoted field stores,
        // to avoid diffs due to old promotion code ignoring volatile as well.

        if (store->IsVolatile() && (!store->GetAddr()->OperIs(GT_FIELD_ADDR) || !lcl->IsPromotedField()))
        {
            CanonicalizeLocalIndStore(store, addrVal);
            return;
        }

        GenTree* value = store->GetValue();

        if (lclOffs == 0)
        {
            if (varTypeIsStruct(lcl->GetType()) &&
                ((storeLayout == lcl->GetLayout()) ||
                 (lcl->IsIndependentPromoted() && (lcl->GetLayout()->GetSize() == storeLayout->GetSize()))))
            {
                store->ChangeToLclStore(lclType, lcl, value);
                INDEBUG(m_stmtModified = true);

                return;
            }

            if (!lcl->lvDoNotEnregister && (varTypeSize(lclType) == storeLayout->GetSize()))
            {
                store->ChangeToLclStore(lcl->GetType(), lcl, value);
                RetypeSingleFieldStructLclStore(store->AsLclStore(), value);

                return;
            }
        }

        if ((fieldSeq == nullptr) || !varTypeIsStruct(lclType))
        {
            fieldSeq = FieldSeqStore::NotAField();
        }

        store->ChangeToLclStoreFld(storeType, lcl, lclOffs, fieldSeq, value)->SetLayout(storeLayout, m_compiler);

        m_compiler->lvaSetDoNotEnregister(lcl DEBUGARG(Compiler::DNER_LocalField));

        if (varTypeIsSmall(lclType))
        {
            // If LCL_STORE_FLD is used to store to a small type local then "normalize on store"
            // isn't possible so the local has to be "normalize on load". The only way to do this
            // is by making the local address exposed, which is a big hammer. But such an indirect
            // store is highly unusual so it's not worth the trouble to introduce another mechanism.
            m_compiler->lvaSetAddressExposed(lcl);
        }

        INDEBUG(m_stmtModified = true);
    }

    void CanonicalizeLocalAddress(const Value& addrVal, GenTree* user)
    {
        assert(addrVal.IsAddress());
        assert(addrVal.Node()->TypeIs(TYP_BYREF, TYP_I_IMPL));

        GenTree*   addr = addrVal.Node();
        LclVarDsc* lcl  = addrVal.Lcl();

        assert(lcl->IsAddressExposed());

        if ((addrVal.Offset() > UINT16_MAX) || (addrVal.Offset() >= lcl->GetTypeSize()))
        {
            // The offset is too large to store in a LCL_ADDR node, use ADD(LCL_ADDR, offset) instead.
            addr->ChangeOper(GT_ADD);
            addr->AsOp()->SetOp(0, m_compiler->gtNewLclAddr(lcl));
            addr->AsOp()->SetOp(1, m_compiler->gtNewIconNode(addrVal.Offset(), addrVal.FieldSeq()));
        }
        else if ((addrVal.Offset() != 0) ||
                 ((addrVal.FieldSeq() != nullptr) && (addrVal.FieldSeq() != FieldSeqStore::NotAField())))
        {
            addr->ChangeToLclAddr(addr->GetType(), lcl, addrVal.Offset(), addrVal.FieldSeq());
        }
        else
        {
            addr->ChangeToLclAddr(addr->GetType(), lcl);
        }

        // Local address nodes never have side effects (nor any other flags, at least at this point).
        addr->gtFlags = GTF_EMPTY;

        if (!user->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD, GT_IND_STORE))
        {
            addr->SetType(TYP_I_IMPL);
        }

        INDEBUG(m_stmtModified = true;)
    }

    void CanonicalizeLocalIndLoad(GenTreeIndir* load, const Value& addrVal)
    {
        assert(addrVal.IsAddress());

        GenTree*   addr = load->GetAddr();
        LclVarDsc* lcl  = addrVal.Lcl();

        m_compiler->lvaSetAddressExposed(lcl);

        CanonicalizeLocalAddress(addrVal, load);

        load->SetAddr(addr);
        load->gtFlags &= GTF_IND_UNALIGNED | GTF_IND_VOLATILE;
        load->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING | GTF_IND_TGT_NOT_HEAP;

        if (load->IsVolatile())
        {
            load->AddSideEffects(GTF_ORDER_SIDEEFF);
        }

        INDEBUG(m_stmtModified = true;)
    }

    void CanonicalizeLocalIndStore(GenTreeIndir* store, const Value& addrVal)
    {
        assert(addrVal.IsAddress());

        m_compiler->lvaSetAddressExposed(addrVal.Lcl());

        CanonicalizeLocalAddress(addrVal, store);

        store->gtFlags &= GTF_IND_UNALIGNED | GTF_IND_VOLATILE;
        store->gtFlags |= GTF_ASG | GTF_GLOB_REF | GTF_IND_NONFAULTING | GTF_IND_TGT_NOT_HEAP;

        if (store->IsVolatile())
        {
            store->AddSideEffects(GTF_ORDER_SIDEEFF);
        }

        INDEBUG(m_stmtModified = true);
    }

    LclVarDsc* FindPromotedField(LclVarDsc* lcl, unsigned offset, unsigned size) const
    {
        for (LclVarDsc* fieldLcl : m_compiler->PromotedFields(lcl))
        {
            assert(fieldLcl->GetType() != TYP_STRUCT);

            if ((offset >= fieldLcl->GetPromotedFieldOffset()) &&
                (offset - fieldLcl->GetPromotedFieldOffset() + size <= varTypeSize(fieldLcl->GetType())))
            {
                return fieldLcl;
            }
        }

        return nullptr;
    }

    void PromoteSingleFieldStructLclLoadStoreValue(LclVarDsc* lcl, GenTreeLclLoad* load, GenTree* user)
    {
        assert(lcl->TypeIs(TYP_STRUCT));
        assert(lcl->GetPromotedFieldCount() == 1);
        assert(user->IsCall() ||
               (user->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD, GT_IND_STORE_OBJ) && user->TypeIs(TYP_STRUCT)));

        LclVarDsc* fieldLcl = m_compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(0));

        assert(lcl->GetLayout()->GetSize() == varTypeSize(fieldLcl->GetType()));

        load->SetLcl(fieldLcl);
        load->SetType(fieldLcl->GetType());

        INDEBUG(m_stmtModified = true;)
    }

    GenTree* PromoteSingleFieldStructLclLoadReturn(LclVarDsc* lcl, GenTreeLclLoad* load, GenTreeUnOp* ret)
    {
        assert(lcl->TypeIs(TYP_STRUCT));
        assert(lcl->GetPromotedFieldCount() == 1);
        assert(ret->TypeIs(TYP_STRUCT));

        LclVarDsc* fieldLcl = m_compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(0));

        assert(lcl->GetLayout()->GetSize() == varTypeSize(fieldLcl->GetType()));

        load->SetLcl(fieldLcl);
        load->SetType(fieldLcl->GetType());

        INDEBUG(m_stmtModified = true;)

        if (IsMergedReturn(ret))
        {
            // This is a merged return, it will be transformed into a struct
            // copy so leave it to fgMorphCopyStruct to promote it.
            return load;
        }

        const ReturnTypeDesc& retDesc = m_compiler->info.retDesc;

        if (retDesc.GetRegCount() == 1)
        {
            var_types retRegType = varActualType(retDesc.GetRegType(0));
            ret->SetType(retRegType);

            if (varTypeUsesFloatReg(retRegType) != varTypeUsesFloatReg(fieldLcl->GetType()))
            {
                ret->SetOp(0, NewBitCastNode(retRegType, load));
            }

            return ret->GetOp(0);
        }

#ifdef WINDOWS_X86_ABI
        if (retDesc.GetRegCount() == 2)
        {
            assert((retDesc.GetRegType(0) == TYP_INT) && (retDesc.GetRegType(1) == TYP_INT));

            ret->SetType(TYP_LONG);

            if (fieldLcl->TypeIs(TYP_SIMD8))
            {
                // TODO-MIKE-CQ: This generates rather poor code. Vector2 should be handled
                // like DOUBLE, in codegen.
                ret->SetOp(0, NewExtractElement(TYP_LONG, load, TYP_SIMD16, 0));
            }
            else
            {
                assert(fieldLcl->TypeIs(TYP_LONG, TYP_DOUBLE));
            }

            return ret->GetOp(0);
        }
#endif

        // We either have a SIMD field that's returned in multiple registers (e.g. HFA)
        // or perhaps the IL is invalid (e.g. struct with a single DOUBLE field returned
        // as a Vector2 or some other 2 FLOAT field struct that is a HFA).
        // Either way, leave it to morph to produce a FIELD_LIST in this case.
        // We could probably do it here but it's not clear if it has any benefits.
        assert(varTypeIsSIMD(fieldLcl->GetType()));

        return load;
    }

    bool PromoteLclLoadFld(GenTreeLclLoadFld* load, LclVarDsc* lcl)
    {
        // The importer does not currently produce STRUCT LCL_LOAD_FLDs.
        assert(!load->TypeIs(TYP_STRUCT));

        LclVarDsc* fieldLcl = FindPromotedField(lcl, load->GetLclOffs(), varTypeSize(load->GetType()));

        if (fieldLcl == nullptr)
        {
            return false;
        }

        // The importer rarely produces LCL_LOAD_FLDs, currently only when importing refanytype,
        // so we can get away with handling only the trivial case when types match exactly.
        // Otherwise we'll just DNER/P-DEP the promoted local so assert to know about it.
        assert(fieldLcl->GetType() == load->GetType());

        if (fieldLcl->GetType() != load->GetType())
        {
            return false;
        }

        load->ChangeToLclLoad(fieldLcl->GetType(), fieldLcl);
        INDEBUG(m_stmtModified = true;)

        return true;
    }

    bool PromoteSingleFieldStructCopy(const Value& addrVal, GenTreeIndLoadObj* load)
    {
        assert(addrVal.Offset() == 0);
        assert(load->TypeIs(TYP_STRUCT));

        LclVarDsc* lcl = addrVal.Lcl();
        assert(!lcl->TypeIs(TYP_STRUCT));

        if (load->GetLayout()->GetSize() == varTypeSize(lcl->GetType()))
        {
            load->ChangeToLclLoad(lcl->GetType(), lcl);
            INDEBUG(m_stmtModified = true;)

            return true;
        }

        return false;
    }

    bool PromoteSingleFieldStructReturn(const Value&  addrVal,
                                        GenTreeIndir* load,
                                        ClassLayout*  loadLayout,
                                        GenTreeUnOp*  ret)
    {
        assert(addrVal.Offset() == 0);
        assert(load->OperIs(GT_IND_LOAD_OBJ) && load->TypeIs(TYP_STRUCT));
        assert(ret->OperIs(GT_RETURN) && ret->TypeIs(TYP_STRUCT));
        assert(m_compiler->info.GetRetLayout()->GetSize() == loadLayout->GetSize());

        LclVarDsc* const lcl = addrVal.Lcl();
        assert(!lcl->TypeIs(TYP_STRUCT));

        var_types             lclType     = lcl->GetType();
        const ReturnTypeDesc& retDesc     = m_compiler->info.retDesc;
        bool                  useLcl      = false;
        var_types             bitcastType = TYP_UNDEF;

        if (IsMergedReturn(ret))
        {
            // This is a merged return, it will be transformed into a struct
            // copy so leave it to fgMorphCopyStruct to handle it.

            LclVarDsc* mergedLcl = m_compiler->lvaGetDesc(m_compiler->genReturnLocal);
            assert(mergedLcl->TypeIs(TYP_STRUCT));

            if (!mergedLcl->IsPromoted())
            {
                // The merged return temp is a struct and it's not promoted.
                // In general we can use a LCL_STORE_FLD to store the return value.

                useLcl = mergedLcl->GetLayout()->GetSize() == varTypeSize(lclType);
            }
            else
            {
                // Currently we only promote merged return temps with a single field.
                assert(mergedLcl->GetPromotedFieldCount() == 1);

                LclVarDsc* mergedFieldLcl = m_compiler->lvaGetDesc(mergedLcl->GetPromotedFieldLclNum(0));

                // Normally the returned value and the merged return temp type should be
                // the same so the promoted fields should also have the same type. We
                // may get different types due to reinterpretation but don't bother for
                // now, merged returns are already too messy.
                if (varActualType(mergedFieldLcl->GetType()) == varActualType(lclType))
                {
                    useLcl = true;
                }
                else if ((varTypeSize(mergedFieldLcl->GetType()) == varTypeSize(varActualType(lclType)) &&
                          (varTypeUsesFloatReg(mergedFieldLcl->GetType()) != varTypeUsesFloatReg(lclType)) &&
                          (varTypeSize(varActualType(lclType)) <= REGSIZE_BYTES)))
                {
                    useLcl      = true;
                    bitcastType = varActualType(mergedFieldLcl->GetType());
                }
            }
        }
        else if (retDesc.GetRegCount() == 1)
        {
            // Since the local isn't a struct we expect it to be returned in a single
            // register, with the exception of the win-x86 native ABI, where we need
            // to return LONG/DOUBLE in 2 INT registers.
            // We may need to bitcast between integer and floating point (e.g. a struct
            // with a single FLOAT is returned in an integer register on win-x64) but
            // we don't care about the precise size of the involved types. For example,
            // reinterpretation in user code may result in a DOUBLE local returned as a
            // struct with a single FLOAT field, in this case the caller only cares about
            // the low 4 bytes in XMM0, there is no need to attempt to zero out the rest
            // of the bytes.

            // This covers the needs of single field struct promotion and reasonable
            // reinterpretation in user code. Anything else (e.g. INT local returned
            // as a 16 byte struct that needs 2 registers on linux/arm-64 is handled
            // by creating a LCL_LOAD_FLD matching the return type.

            var_types retRegType = varActualType(retDesc.GetRegType(0));

            if (varTypeUsesFloatReg(retRegType) == varTypeUsesFloatReg(lclType))
            {
                useLcl = true;

                ret->SetType(retRegType);
            }
            else if (varTypeSize(retRegType) == varTypeSize(varActualType(lclType)))
            {
                useLcl      = true;
                bitcastType = retRegType;

                ret->SetType(retRegType);
            }
        }
#ifdef WINDOWS_X86_ABI
        else if (retDesc.GetRegCount() == 2)
        {
            if ((lclType == TYP_LONG) || (lclType == TYP_DOUBLE))
            {
                assert((retDesc.GetRegType(0) == TYP_INT) && (retDesc.GetRegType(1) == TYP_INT));

                ret->SetType(TYP_LONG);
            }
        }
#endif

        if (useLcl && (bitcastType != TYP_UNDEF))
        {
            assert(varTypeSize(bitcastType) <= REGSIZE_BYTES);

            GenTree* addr = load->AsIndLoadObj()->GetAddr();

            addr->ChangeToLclLoad(lclType, lcl);

            load->ChangeOper(GT_BITCAST);
            load->SetType(bitcastType);
            load->AsUnOp()->SetOp(0, addr);
            load->gtFlags = addr->GetSideEffects();
            INDEBUG(m_stmtModified = true;)

            return true;
        }

        if (useLcl)
        {
            load->ChangeToLclLoad(lclType, lcl);
            INDEBUG(m_stmtModified = true;)

            return true;
        }

        return false;
    }

    void RetypeSingleFieldStructLclStore(GenTreeLclStore* store, GenTree* value)
    {
        if (value->IsIntCon(0))
        {
            value = RetypeStructZeroInit(value, store->GetType());
        }
        else if (!value->TypeIs(TYP_STRUCT))
        {
            value = RetypeScalarLclStoreValue(store, value);
        }
        else if (GenTreeCall* call = value->IsCall())
        {
            value = RetypeStructCall(call, store->GetType());
        }
        else if (GenTreeIndLoadObj* load = value->IsIndLoadObj())
        {
            value = RetypeStructIndLoad(load, store->GetType());
        }
        else
        {
            value = RetypeStructLclLoad(value->AsLclVarCommon(), store->GetType());
        }

        store->SetValue(value);
        store->SetSideEffects(value->GetSideEffects());

        INDEBUG(m_stmtModified = true;)
    }

    GenTree* RetypeScalarLclStoreValue(GenTreeLclStore* store, GenTree* value)
    {
        assert(!store->TypeIs(TYP_STRUCT));
        assert(value->OperIs(GT_LCL_LOAD) && !value->TypeIs(TYP_STRUCT));

        // The store operand was changed to scalar, currently we only do this if the
        // struct size matches the scalar type size and since the operand of a struct
        // store is supposed to have the same struct type, the resulting scalar type
        // should also have the same size. It may be possible to tolerate size mismatches
        // for small int types but it's unlikely to be worth the trouble.
        //
        // TODO-MIKE-Review: The importer doesn't do proper IL validation and we may get
        // here with different struct types. Probably it doesn't matter, unless it results
        // in JIT crashes...
        assert(varTypeSize(store->GetType()) == varTypeSize(value->GetType()));

        // We don't allow type size changes but we don't otherwise care about the scalar
        // types so we could end up with INT/FLOAT and DOUBLE/LONG/SIMD8 mismatches.
        if ((varTypeUsesFloatReg(store->GetType()) != varTypeUsesFloatReg(value->GetType())) &&
            (varTypeSize(store->GetType()) <= REGSIZE_BYTES))
        {
            return NewBitCastNode(varActualType(store->GetType()), value);
        }

        if (varActualType(store->GetType()) != varActualType(value->GetType()))
        {
            value->ChangeOper(GT_LCL_LOAD_FLD);
            value->SetType(store->GetType());

            m_compiler->lvaSetDoNotEnregister(value->AsLclLoadFld()->GetLcl() DEBUGARG(Compiler::DNER_LocalField));
        }

        return value;
    }

    void RetypeMergedReturnValue(GenTreeUnOp* ret, GenTree* value, LclVarDsc* lcl)
    {
        assert(ret->OperIs(GT_RETURN) && ret->TypeIs(TYP_STRUCT) && IsMergedReturn(ret));
        assert(ret->GetOp(0) == value);
        assert(lcl->IsPromoted() && (lcl->GetPromotedFieldCount() == 1));

        var_types type = m_compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(0))->GetType();

        if (value->IsIntCon(0))
        {
            value = RetypeStructZeroInit(value, type);
        }
        else if (GenTreeCall* call = value->IsCall())
        {
            value = RetypeStructCall(call, type);
        }
        else if (GenTreeIndLoadObj* load = value->IsIndLoadObj())
        {
            value = RetypeStructIndLoad(load, type);
        }
        else
        {
            value = RetypeStructLclLoad(value->AsLclVarCommon(), type);
        }

        ret->SetType(type);
        ret->SetOp(0, value);
        ret->SetSideEffects(value->GetSideEffects());

        INDEBUG(m_stmtModified = true;)
    }

    GenTree* RetypeStructZeroInit(GenTree* zero, var_types type)
    {
        assert(zero->IsIntCon(0));
        assert(type != TYP_STRUCT);

        switch (varActualType(type))
        {
            case TYP_INT:
                break;
            case TYP_LONG:
#ifdef TARGET_64BIT
                zero->SetType(TYP_LONG);
#else
                zero->ChangeToLngCon(0);
#endif
                break;
            case TYP_BYREF:
            case TYP_REF:
                zero->SetType(type);
                break;
            case TYP_FLOAT:
            case TYP_DOUBLE:
                zero->ChangeToDblCon(type, 0);
                break;
#ifdef FEATURE_SIMD
            case TYP_SIMD8:
            case TYP_SIMD12:
            case TYP_SIMD16:
            case TYP_SIMD32:
                zero->ChangeOper(GT_HWINTRINSIC);
                zero->SetType(type);
                zero->AsHWIntrinsic()->SetIntrinsic(GetZeroSimdHWIntrinsic(type), TYP_FLOAT, varTypeSize(type), 0);
                break;
#endif
            default:
                unreached();
        }

        return zero;
    }

    GenTree* RetypeStructCall(GenTreeCall* call, var_types type)
    {
        assert(call->TypeIs(TYP_STRUCT));
        assert(type != TYP_STRUCT);

        const ReturnTypeDesc& retDesc = *call->GetRetDesc();

        if (retDesc.GetRegCount() == 1)
        {
            var_types retRegType = retDesc.GetRegType(0);

            if (varTypeUsesFloatReg(retRegType) != varTypeUsesFloatReg(type))
            {
                call->SetType(varActualType(retRegType));
                return NewBitCastNode(type, call);
            }

            call->SetType(varActualType(type));

            return call;
        }

#ifdef WINDOWS_X86_ABI
        if (retDesc.GetRegCount() == 2)
        {
            assert((retDesc.GetRegType(0) == TYP_INT) && (retDesc.GetRegType(1) == TYP_INT));
            call->SetType(TYP_LONG);

            if (type == TYP_SIMD8)
            {
                return m_compiler->gtNewSimdHWIntrinsicNode(TYP_SIMD8, NI_Vector128_CreateScalarUnsafe, TYP_LONG, 16,
                                                            call);
            }

            if (type == TYP_DOUBLE)
            {
                // TODO-MIKE-CQ: We could probably make BITCAST LONG to DOUBLE work on x86.
                // But anyway decomposition manages to force the CALL return value into
                // memory so it's all messed up anyway. Luckily this is a very rare case.
                return NewExtractElement(TYP_DOUBLE,
                                         m_compiler->gtNewSimdHWIntrinsicNode(TYP_SIMD16,
                                                                              NI_Vector128_CreateScalarUnsafe, TYP_LONG,
                                                                              16, call),
                                         TYP_SIMD16, 0);
            };

            assert(type == TYP_LONG);

            return call;
        }
#endif

        // Otherwise we're on arm64 or unix-x64 and we have promoted a single
        // SIMD field struct that will be transformed during global morph.
        assert(varTypeIsSIMD(type));

#ifdef TARGET_ARM64
        assert(call->GetRetLayout()->IsHfa());
#elif defined(UNIX_AMD64_ABI)
        assert(varTypeIsFloating(retDesc.GetRegType(0)) && varTypeIsFloating(retDesc.GetRegType(1)));
#endif
        // TODO-MIKE-Fix: This doesn't handle reinterpretation weirdness,
        // see arm64-multireg-call-single-simd-field.il.
        call->SetType(type);

        return call;
    }

    GenTree* RetypeStructLclLoad(GenTreeLclVarCommon* load, var_types type)
    {
        assert(load->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD) && load->TypeIs(TYP_STRUCT));
        assert(type != TYP_STRUCT);

        ClassLayout* fieldLayout = nullptr;

        if (GenTreeLclLoadFld* lclFld = load->IsLclLoadFld())
        {
            lclFld->SetType(type);
            lclFld->SetLayoutNum(0);
            lclFld->SetFieldSeq(ExtendFieldSequence(lclFld->GetFieldSeq(), type, &fieldLayout));
        }
        else
        {
            LclVarDsc*    lcl      = load->GetLcl();
            FieldSeqNode* fieldSeq = GetFieldSequence(lcl->GetLayout()->GetClassHandle(), type, &fieldLayout);

            load->ChangeToLclLoadFld(type, lcl, 0, fieldSeq);

            m_compiler->lvaSetDoNotEnregister(lcl DEBUGARG(Compiler::DNER_LocalField));
        }

        if (varTypeIsSIMD(type) && (fieldLayout != nullptr) && (fieldLayout->GetSIMDType() == type))
        {
            load->AsLclLoadFld()->SetLayout(fieldLayout, m_compiler);
        }

        return load;
    }

    GenTreeIndir* RetypeStructIndLoad(GenTreeIndLoadObj* load, var_types type)
    {
        assert(load->TypeIs(TYP_STRUCT));
        assert(type != TYP_STRUCT);

        GenTree*     addr       = load->GetAddr();
        ClassLayout* addrLayout = nullptr;

        if (GenTreeFieldAddr* field = addr->IsFieldAddr())
        {
            addrLayout = field->GetLayout(m_compiler);
        }
        else if (GenTreeIndexAddr* index = addr->IsIndexAddr())
        {
            addrLayout = index->GetLayout(m_compiler);
        }

        if (addrLayout != nullptr)
        {
            ClassLayout*  fieldLayout;
            FieldSeqNode* fieldSeq = GetFieldSequence(addrLayout->GetClassHandle(), type, &fieldLayout);

            if (fieldSeq->IsField())
            {
                // TODO-MIKE-Cleanup: It may be good to set the layout on these FIELD_ADDRs
                // for the sake of consistency, though at this point nothing needs it.
                // Also, if the original address is also a FIELD_ADDR we could simply extend
                // its field sequence.
                load->SetAddr(new (m_compiler, GT_FIELD_ADDR) GenTreeFieldAddr(addr, fieldSeq, 0));
            }
        }

        load->SetOper(GT_IND_LOAD);
        load->SetType(type);

        return load;
    }

    bool IsMergedReturn(GenTreeUnOp* ret)
    {
        assert(ret->OperIs(GT_RETURN));

        return (m_compiler->genReturnLocal != BAD_VAR_NUM) && ((ret->gtFlags & GTF_RET_MERGED) == 0);
    }

    var_types GetScalarFieldType(CORINFO_FIELD_HANDLE fieldHandle)
    {
        return CorTypeToVarType(m_compiler->info.compCompHnd->getFieldType(fieldHandle));
    }

    CORINFO_CLASS_HANDLE GetStructFieldType(CORINFO_FIELD_HANDLE fieldHandle)
    {
        CORINFO_CLASS_HANDLE fc;
        var_types            ft = CorTypeToVarType(m_compiler->info.compCompHnd->getFieldType(fieldHandle, &fc));
        assert((fc != nullptr) || (ft != TYP_STRUCT));

        return ft == TYP_STRUCT ? fc : nullptr;
    }

    FieldSeqNode* GetFieldSequence(CORINFO_CLASS_HANDLE classHandle, var_types fieldType, ClassLayout** fieldLayout)
    {
        assert(fieldType != TYP_STRUCT);

        ICorJitInfo*         vm       = m_compiler->info.compCompHnd;
        FieldSeqNode*        fieldSeq = nullptr;
        CORINFO_FIELD_HANDLE fieldHandle;

        *fieldLayout = nullptr;

        for (var_types classType = TYP_STRUCT; classType == TYP_STRUCT;)
        {
            if (vm->getClassNumInstanceFields(classHandle) < 1)
            {
                return FieldSeqNode::NotAField();
            }

            // In theory we should look at all fields and find the one having offset 0
            // and the requested type. But since this is needed for single field struct
            // promotion there should be only one field anyway.

            fieldHandle = vm->getFieldInClass(classHandle, 0);

            if (vm->getFieldOffset(fieldHandle) != 0)
            {
                return FieldSeqNode::NotAField();
            }

            classType = CorTypeToVarType(vm->getFieldType(fieldHandle, &classHandle));

#ifdef FEATURE_SIMD
            if (classType == TYP_STRUCT)
            {
                ClassLayout* layout = m_compiler->typGetObjLayout(classHandle);

                if (layout->IsVector())
                {
                    classType = layout->GetSIMDType();
                }

                *fieldLayout = layout;
            }
#endif

            if ((classType != fieldType) && (classType != TYP_STRUCT))
            {
                return FieldSeqNode::NotAField();
            }

            fieldSeq = m_compiler->GetFieldSeqStore()->Append(fieldSeq, fieldHandle);

            if (!fieldSeq->IsField())
            {
                return FieldSeqNode::NotAField();
            }
        }

        return fieldSeq;
    }

    FieldSeqNode* ExtendFieldSequence(FieldSeqNode* fieldSeq, var_types fieldType, ClassLayout** fieldLayout)
    {
        if ((fieldSeq == nullptr) || !fieldSeq->IsField())
        {
            return FieldSeqNode::NotAField();
        }

        CORINFO_CLASS_HANDLE classHandle = GetStructFieldType(fieldSeq->GetTail()->GetFieldHandle());

        if (classHandle == nullptr)
        {
            return FieldSeqNode::NotAField();
        }

        return m_compiler->GetFieldSeqStore()->Append(fieldSeq, GetFieldSequence(classHandle, fieldType, fieldLayout));
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

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
    // Updates the ref count for implicit byref params.
    // abiMakeImplicitlyByRefStructArgCopy checks the ref counts for implicit byref params when
    // it decides if it's legal to elide certain copies of them;
    // lvaRetypeImplicitByRefParams checks the ref counts when it decides to undo promotions.
    void UpdateImplicitByRefParamRefCounts(LclVarDsc* lcl)
    {
        // We should not encounter any promoted fields yet.
        assert(!lcl->IsPromotedField());

        if (!lcl->IsImplicitByRefParam())
        {
            return;
        }

        JITDUMP("Adding V%02u implicit-by-ref param any ref\n", lcl->GetLclNum());

        lcl->AddImplicitByRefParamAnyRef();

        // See if this struct is an argument to a call. This information is recorded
        // via the weighted early ref count for the local, and feeds the undo promotion
        // heuristic.
        //
        // It can be approximate, so the pattern match below need not be exhaustive.
        // But the pattern should at least subset the implicit byref cases that are
        // handled in fgCanFastTailCall and abiMakeImplicitlyByRefStructArgCopy.
        //
        // CALL(IND_LOAD_OBJ(LCL_ADDR))

        // TODO-MIKE-Cleanup: The OBJ check is likely useless since the importer no
        // longer wraps struct args in OBJs.

        if (((m_ancestors.Size() >= 3) && m_ancestors.Top(0)->OperIs(GT_LCL_ADDR) &&
             m_ancestors.Top(1)->OperIs(GT_IND_LOAD_OBJ) && m_ancestors.Top(2)->OperIs(GT_CALL)) ||
            ((m_ancestors.Size() >= 2) && m_ancestors.Top(0)->OperIs(GT_LCL_LOAD) &&
             m_ancestors.Top(0)->TypeIs(TYP_STRUCT) && m_ancestors.Top(1)->OperIs(GT_CALL)))
        {
            JITDUMP("Adding V%02u implicit-by-ref param call ref\n", lcl->GetLclNum());

            lcl->AddImplicitByRefParamCallRef();
        }
    }
#endif // defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)

    GenTreeUnOp* NewBitCastNode(var_types type, GenTree* op)
    {
        assert(varTypeSize(type) <= REGSIZE_BYTES);

        return m_compiler->gtNewBitCastNode(type, op);
    }

    GenTreeLclLoad* NewLclLoad(var_types type, LclVarDsc* lcl)
    {
        assert((type == lcl->GetType()) || (varActualType(type) == varActualType(lcl->GetType())));

        return m_compiler->gtNewLclLoad(lcl, type);
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

    GenTreeHWIntrinsic* NewExtractElement(var_types eltType, GenTree* vec, var_types vecType, unsigned index)
    {
        return m_compiler->gtNewSimdGetElementNode(vecType, eltType, vec, m_compiler->gtNewIconNode(index));
    }
#endif
};

#ifdef FEATURE_SIMD
// Mark locals used by SIMD intrinsics to prevent struct promotion.
void Compiler::lvaRecordSimdIntrinsicUse(GenTree* op)
{
    if (op->OperIs(GT_IND_LOAD_OBJ, GT_IND_LOAD))
    {
        // TODO-MIKE-Review: See if this indir check is still necessary,
        // such indirs should no longer be generated or should be very rare.
        // And even if they're generated, we should probably check the types
        // of the indir and the local variable too.

        GenTree* addr = op->AsIndir()->GetAddr();

        if (addr->OperIs(GT_LCL_ADDR) && (addr->AsLclAddr()->GetLclOffs() == 0))
        {
            lvaRecordSimdIntrinsicUse(addr->AsLclAddr()->GetLcl());
        }
    }
    else if (op->OperIs(GT_LCL_LOAD))
    {
        lvaRecordSimdIntrinsicUse(op->AsLclLoad()->GetLcl());
    }
}

void Compiler::lvaRecordSimdIntrinsicUse(GenTreeLclLoad* load)
{
    lvaRecordSimdIntrinsicUse(load->GetLcl());
}

void Compiler::lvaRecordSimdIntrinsicUse(LclVarDsc* lcl)
{
    lcl->lvUsedInSIMDIntrinsic = true;
}

void Compiler::lvaRecordSimdIntrinsicDef(GenTreeLclStore* store, GenTreeHWIntrinsic* src)
{
    lvaRecordSimdIntrinsicDef(store->GetLcl(), src);
}

void Compiler::lvaRecordSimdIntrinsicDef(LclVarDsc* lcl, GenTreeHWIntrinsic* src)
{
    // Don't block promotion due to Create/Zero intrinsics, we can promote these.
    switch (src->GetIntrinsic())
    {
#ifdef TARGET_ARM64
        case NI_Vector64_Create:
        case NI_Vector64_get_Zero:
#endif
        case NI_Vector128_Create:
        case NI_Vector128_get_Zero:
            return;
        default:
            break;
    }

    lcl->lvUsedInSIMDIntrinsic = true;
}
#endif // FEATURE_SIMD

bool StructPromotionHelper::TryPromoteStructLocal(LclVarDsc* lcl)
{
    if (CanPromoteStructLocal(lcl) && ShouldPromoteStructLocal(lcl))
    {
        PromoteStructLocal(lcl);
        return true;
    }

    // If we don't promote then lvIsMultiRegRet is meaningless.
    lcl->lvIsMultiRegRet = false;

    return false;
}

bool StructPromotionHelper::CanPromoteStructType(CORINFO_CLASS_HANDLE typeHandle)
{
    // TODO-ObjectStackAllocation: Enable promotion of fields of stack-allocated objects.
    assert(compiler->info.compCompHnd->isValueClass(typeHandle));

    if (info.typeHandle == typeHandle)
    {
        // Asking for the same type of struct as the last time.
        // Nothing need to be done.
        return info.canPromoteStructType;
    }

    ICorJitInfo* vm = compiler->info.compCompHnd;

    unsigned structSize = vm->getClassSize(typeHandle);

    if (structSize > MaxStructSize)
    {
        return false;
    }

    unsigned fieldCount = vm->getClassNumInstanceFields(typeHandle);

    if ((fieldCount < 1) || (fieldCount > MaxFieldCount))
    {
        return false;
    }

    uint32_t typeFlags = vm->getClassAttribs(typeHandle);

    if ((typeFlags & CORINFO_FLG_DONT_PROMOTE) != 0)
    {
        // In AOT ReadyToRun compilation, don't try to promote fields of types
        // outside of the current version bubble.
        return false;
    }

    if ((typeFlags & CORINFO_FLG_OVERLAPPING_FIELDS) != 0)
    {
        return false;
    }

    if ((typeFlags & CORINFO_FLG_CUSTOMLAYOUT) != 0)
    {
        // Don't promote HFAs with custom layout.
        // TODO-MIKE-Review: How exactly a HFA can have custom layout?!?

        ClassLayout* layout = compiler->typGetObjLayout(typeHandle);
        layout->EnsureHfaInfo(compiler);

        if (layout->IsHfa())
        {
            return false;
        }
    }

#ifdef TARGET_ARM
    // On ARM, we have a requirement on the struct alignment; see below.
    unsigned structAlignment = roundUp(vm->getClassAlignmentRequirement(typeHandle), TARGET_POINTER_SIZE);
#endif

    unsigned totalFieldSize     = 0;
    bool     containsGCpointers = false;

    new (&info) StructInfo(typeHandle, fieldCount);

    for (unsigned index = 0; index < fieldCount; ++index)
    {
        GetFieldInfo(index);

        const FieldInfo& field = info.fields[index];

        if (field.type == TYP_STRUCT)
        {
            return false;
        }

        unsigned size = varTypeSize(field.type);

        // The end offset for this field should never be larger than our structSize.
        noway_assert(field.offset + size <= structSize);

        if (size > 1)
        {
            unsigned alignment = varTypeSize(field.type);

#ifdef FEATURE_SIMD
            if (varTypeIsSIMD(field.type) && (field.layout->GetVectorKind() == VectorKind::Vector234))
            {
                // Vector2/3/4 doesn't have special alignment rules in the VM,
                // it has only FLOAT fields so it's 4 byte aligned.
                alignment = 4;
            }
#endif

            if (field.offset % alignment != 0)
            {
                // Call arg and return transformations depend on natural alignment
                // so that they don't have to deal with primitive types that need
                // to be split between multiple registers.
                return false;
            }
        }

#ifdef TARGET_ARM
        // On ARM, for struct types that don't use explicit layout, the alignment of the struct is
        // at least the max alignment of its fields.  We take advantage of this invariant in struct
        // promotion, so verify it here.
        if (size > structAlignment)
        {
            return false;
        }
#endif

        containsGCpointers |= varTypeIsGC(field.type);
        totalFieldSize += size;
    }

    noway_assert(!containsGCpointers ||
                 ((typeFlags & (CORINFO_FLG_CONTAINS_GC_PTR | CORINFO_FLG_CONTAINS_STACK_PTR)) != 0));

    // If we have "Custom Layout" then we might have an explicit Size attribute
    // Managed C++ uses this for its structs, such C++ types will not contain GC pointers.
    //
    // The current VM implementation also incorrectly sets the CORINFO_FLG_CUSTOMLAYOUT
    // whenever a managed value class contains any GC pointers.
    // (See the comment for VMFLAG_NOT_TIGHTLY_PACKED in class.h)
    //
    // It is important to struct promote managed value classes that have GC pointers
    // So we compute the correct value for "CustomLayout" here
    info.customLayout =
        (typeFlags & (CORINFO_FLG_CUSTOMLAYOUT | CORINFO_FLG_CONTAINS_GC_PTR)) == CORINFO_FLG_CUSTOMLAYOUT;

    // If sizes do not match it means we have an overlapping fields or holes.
    // Overlapping fields were rejected early, so here it can mean only holes.
    info.containsHoles = totalFieldSize != structSize;

    if (info.containsHoles && (fieldCount == 1))
    {
        // Single field structs can only have holes due to explicit size/layout.
        // Don't bother promoting, they're rare and a potential source of bugs
        // (especially considering that the most commun occurrence of such structs
        // are "fixed buffer" structs created by the C# compiler and those must
        // not be promoted).
        return false;
    }

    info.canPromoteStructType = true;
    return true;
}

bool StructPromotionHelper::CanPromoteStructLocal(LclVarDsc* lcl)
{
    assert(varTypeIsStruct(lcl->GetType()));
    assert(!lcl->IsPromoted());

    if (varTypeIsSIMD(lcl->GetType()))
    {
        if (lcl->lvIsUsedInSIMDIntrinsic())
        {
            // If the local is used by vector intrinsics, then we do not want to promote
            // since the cost of packing individual fields into a single SIMD register
            // can be pretty high.
            JITDUMP("  promotion of V%02u is disabled due to SIMD intrinsic uses\n", lcl->GetLclNum());
            return false;
        }

        if (lcl->GetLayout()->IsOpaqueVector())
        {
            // Vector<T>/Vector64/128/256 are never promoted, even if they may have
            // private fields as an implementation detail.
            return false;
        }
    }

#ifdef TARGET_ARM
    if (lcl->IsParam())
    {
        // TODO-MIKE-CQ: Promote ARM struct params.
        return false;
    }
#endif

    // If GS stack reordering is enabled we may introduce shadow copies of parameters.
    if (lcl->IsParam() && compiler->compGSReorderStackLayout)
    {
        JITDUMP("  promotion of param V%02u is disabled due to GS stack layout reordering\n", lcl->GetLclNum());
        return false;
    }

    if (!compiler->compEnregLocals())
    {
        if (lcl->lvIsMultiRegArg)
        {
            JITDUMP("  promotion of V%02u is disabled because it is a multi-reg arg\n", lcl->GetLclNum());
            return false;
        }

        if (lcl->lvIsMultiRegRet)
        {
            JITDUMP("  promotion of V%02u is disabled because it is a multi-reg return\n", lcl->GetLclNum());
            return false;
        }
    }

    // TODO-CQ: enable promotion for OSR locals
    if (compiler->lvaIsOSRLocal(lcl))
    {
        JITDUMP("  promotion of V%02u is disabled because it is an OSR local\n", lcl->GetLclNum());
        return false;
    }

    CORINFO_CLASS_HANDLE typeHandle = lcl->GetLayout()->GetClassHandle();

    if (!CanPromoteStructType(typeHandle))
    {
        return false;
    }

#ifdef WINDOWS_AMD64_ABI
    // TODO-MIKE-CQ: Promoting single FP field structs almost works on x64, the main
    // problem is the handling of method parameters and unfortunately the code that
    // does that is a pile of garbage...
    if (lcl->IsParam() && (info.fieldCount == 1) && varTypeIsFloating(info.fields[0].type))
    {
        JITDUMP("Not promoting param V%02u: struct has a single float/double field.\n", lcl->GetLclNum(),
                info.fieldCount);
        return false;
    }
#endif

    if (!lcl->lvIsMultiRegArg && !lcl->lvIsMultiRegRet)
    {
        return true;
    }

    if (info.fieldCount > MAX_MULTIREG_COUNT)
    {
        return false;
    }

#if defined(TARGET_ARMARCH)
    if (info.fieldCount == 1)
    {
        return true;
    }

    ClassLayout* layout = lcl->GetLayout();
    layout->EnsureHfaInfo(compiler);

    if (layout->IsHfa())
    {
        return layout->GetHfaRegCount() == info.fieldCount;
    }

#ifdef TARGET_ARM64
    if ((layout->GetSize() > 16) || (info.fieldCount > 2))
#else
    if ((layout->GetSize() > 16) || (info.fieldCount > 4))
#endif
    {
        return false;
    }

    for (unsigned i = 0; i < info.fieldCount; i++)
    {
        const FieldInfo& field = info.fields[i];

        if (field.offset % REGSIZE_BYTES != 0)
        {
            return false;
        }

        // If the struct isn't a HFA then all used registers are integer and
        // prolog codegen does not move args from integer to FP registers.
        if (lcl->lvIsMultiRegArg && varTypeUsesFloatReg(field.type))
        {
            return false;
        }
    }
#elif defined(UNIX_AMD64_ABI)
    // In general we can promote only if there are 1 or 2 fields, each smaller
    // than 8 byte and having 8 byte alignment.
    ClassLayout* layout = lcl->GetLayout();

    if (layout->GetSize() > 16)
    {
        return false;
    }

    // Prolog codegen also handles the case of a single Vector3/4 field, that
    // is passed in 2 registers.
    if ((info.fieldCount == 1) && varTypeIsSIMD(info.fields[0].type))
    {
        return true;
    }

    layout->EnsureSysVAmd64AbiInfo(compiler);

    if (info.fieldCount > layout->GetSysVAmd64AbiRegCount())
    {
        return false;
    }

    for (unsigned i = 0; i < info.fieldCount; i++)
    {
        const FieldInfo& field = info.fields[i];

        if (field.offset % REGSIZE_BYTES != 0)
        {
            return false;
        }
    }
#endif // UNIX_AMD64_ABI

    return true;
}

bool StructPromotionHelper::ShouldPromoteStructLocal(LclVarDsc* lcl)
{
    assert(varTypeIsStruct(lcl->GetType()));
    assert(lcl->GetLayout()->GetClassHandle() == info.typeHandle);
    assert(info.canPromoteStructType);

    // We *can* promote; *should* we promote?
    //
    // We should only do so if promotion has potential savings. One source of savings
    // is if a field of the struct is accessed, since this access will be turned into
    // an access of the corresponding promoted field variable. Even if there are no
    // field accesses, but only block-level operations on the whole struct, if the struct
    // has only one or two fields, then doing those block operations field-wise is probably
    // faster than doing a whole-variable block operation (e.g., REP MOVSB).
    //
    // Struct promotion also provides the following benefits: reduce stack frame size,
    // reduce the need for zero init of stack frame and fine grained constant/copy prop.
    // Asm diffs indicate that promoting structs up to 3 fields is a net size win.
    // So if no fields are accessed independently, and there are four or more fields,
    // then do not promote.

    // TODO: Ideally we would want to consider the impact of whether the struct is
    // passed as a parameter or assigned the return value of a call. Because once promoted,
    // struct copying is done by field by field copy instead of a more efficient rep.stos
    // or xmm reg based copy.

    // TODO: If the lvRefCnt is zero and we have a struct promoted parameter we can end up
    // with an extra store of the the incoming register into the stack frame slot.
    // In that case, we would like to avoid promotion.
    // However we haven't yet computed the lvRefCnt values so we can't do that.

    // TODO-MIKE-CQ: SIMD promotion is still messy as lvFieldAccessed is way too limited.
    // A few "get"/"set"s may be better expressed as "get element"/"with element". Repeated
    // "get"s may be best handled by CSE. Only repeated "set"s are likely to benefit from
    // promotion, especially when we don't have SSE41.
    //
    // On ARM64 it may be useful to treat Vector2/3/4 params and locals that are used by
    // a RETURN as "field accessed" (if they don't have intrinsic uses). Otherwise we end
    // up with pretty stupid codegen in some rather trivial cases (e.g. Vector3 param that
    // is immediately passed as an arg to another call - currently we pack all the param regs
    // into a single one in the prolog and then unpack the single reg to multiple regs before
    // the call). But if the param is passed as an argument to multiple calls then things
    // are more complicated, it may better to not promote to avoid unnecessary register
    // pressure. The current struct promotion is way too limited to be able to do the right
    // thing in this case.

    if (!lcl->lvFieldAccessed && (info.fieldCount > 3 || varTypeIsSIMD(lcl->GetType())))
    {
        JITDUMP("Not promoting V%02u: type = %s, #fields = %d, fieldAccessed = %d.\n", lcl->GetLclNum(),
                varTypeName(lcl->GetType()), info.fieldCount, lcl->lvFieldAccessed);
        return false;
    }

    if (lcl->lvIsMultiRegRet && info.containsHoles && info.customLayout)
    {
        JITDUMP("Not promoting multi-reg returned V%02u with holes.\n", lcl->GetLclNum());
        return false;
    }

    if (lcl->IsParam() && !lcl->IsImplicitByRefParam() && !lcl->IsHfaParam())
    {
#if defined(UNIX_AMD64_ABI) || defined(TARGET_ARM64)
        // TODO-MIKE-Review: This is dubious. These checks are more suitable for reg params
        // but it looks like they are applied to all params. Stack params would have very
        // different restrictions, mostly around small int fields that cannot be widened
        // to INT when doing independent promotion. But that may actually work fine too,
        // since such promoted field are still marked as params they'd end up "normalized
        // on load".
        if (compiler->abiGetStructParamType(lcl->GetLayout(), compiler->info.compIsVarArgs).kind == SPK_ByValue)
        {
            if ((info.fieldCount != 2) && ((info.fieldCount != 1) || !varTypeIsSIMD(info.fields[0].type)))
            {
                JITDUMP("Not promoting multireg param V%02u, #fields != 2 and it's not SIMD.\n", lcl->GetLclNum());
                return false;
            }
        }
        else
#endif
            // TODO-PERF - Implement struct promotion for incoming single-register structs.
            //             Also the implementation of jmp uses the 4 byte move to store
            //             byte parameters to the stack, so that if we have a byte field
            //             with something else occupying the same 4-byte slot, it will
            //             overwrite other fields.
            if (info.fieldCount != 1)
        {
            JITDUMP("Not promoting param V%02u, #fields = %u.\n", lcl->GetLclNum(), info.fieldCount);
            return false;
        }
    }

    if ((lcl->GetLclNum() == compiler->genReturnLocal) && (info.fieldCount > 1))
    {
        // TODO-1stClassStructs: a temporary solution to keep diffs small, it will be fixed later.
        return false;
    }

#ifdef DEBUG
    if (compiler->compPromoteFewerStructs(lcl))
    {
        JITDUMP("Not promoting promotable V%02u, because of STRESS_PROMOTE_FEWER_STRUCTS\n", lcl->GetLclNum());
        return false;
    }
#endif

    return true;
}

void StructPromotionHelper::SortFields()
{
    if (!info.fieldsSorted)
    {
        info.fieldsSorted = true;

        jitstd::sort(info.fields, info.fields + info.fieldCount,
                     [](const FieldInfo& f1, const FieldInfo& f2) { return f1.offset < f2.offset; });
    }
}

void StructPromotionHelper::GetFieldInfo(unsigned index)
{
    ICorJitInfo* vm = compiler->info.compCompHnd;

    CORINFO_FIELD_HANDLE fieldHandle = vm->getFieldInClass(info.typeHandle, index);
    CORINFO_CLASS_HANDLE typeHandle;
    var_types            type = JITtype2varType(vm->getFieldType(fieldHandle, &typeHandle));

    FieldInfo& field     = info.fields[index];
    field.fieldSeq[0]    = fieldHandle;
    field.fieldSeqLength = 1;
    field.offset         = vm->getFieldOffset(fieldHandle);
    field.type           = type;
    field.layout         = nullptr;

    if (field.type == TYP_STRUCT)
    {
#ifdef FEATURE_SIMD
        ClassLayout* layout = compiler->supportSIMDTypes() ? compiler->typGetObjLayout(typeHandle) : nullptr;

        if ((layout != nullptr) && layout->IsVector())
        {
            field.type   = layout->GetSIMDType();
            field.layout = layout;
        }
        else
#endif
        {
            GetSingleFieldStructInfo(field, typeHandle);
        }
    }
}

void StructPromotionHelper::GetSingleFieldStructInfo(FieldInfo& field, CORINFO_CLASS_HANDLE typeHandle)
{
    assert(field.type == TYP_STRUCT);
    assert(field.fieldSeqLength == 1);
    assert(typeHandle != NO_CLASS_HANDLE);

    ICorJitInfo* vm = compiler->info.compCompHnd;

    uint8_t      depth  = 1;
    var_types    type   = TYP_STRUCT;
    ClassLayout* layout = nullptr;
    unsigned     size   = 0;

    while ((depth < _countof(field.fieldSeq)) && (type == TYP_STRUCT))
    {
        // TODO-MIKE-Review: Should we check for CORINFO_FLG_DONT_PROMOTE?

        if (vm->getClassNumInstanceFields(typeHandle) != 1)
        {
            return;
        }

        CORINFO_FIELD_HANDLE fieldHandle = vm->getFieldInClass(typeHandle, 0);
        CORINFO_CLASS_HANDLE fieldTypeHandle;
        type   = CorTypeToVarType(vm->getFieldType(fieldHandle, &fieldTypeHandle));
        size   = varTypeSize(type);
        layout = nullptr;

        if (type == TYP_STRUCT)
        {
            layout = compiler->typGetObjLayout(fieldTypeHandle);
            size   = layout->GetSize();

            if (layout->IsVector())
            {
                type = layout->GetSIMDType();
            }
        }

        if ((vm->getFieldOffset(fieldHandle) != 0) || (vm->getClassSize(typeHandle) != size))
        {
            JITDUMP("Promotion blocked: single field struct contains padding\n");
            return;
        }

        field.fieldSeq[depth++] = fieldHandle;
        typeHandle              = fieldTypeHandle;
    }

    if (type != TYP_STRUCT)
    {
        field.fieldSeqLength = depth;
        field.type           = type;
        field.layout         = layout;
    }
}

void StructPromotionHelper::PromoteStructLocal(LclVarDsc* lcl)
{
    assert(lcl->GetLayout()->GetClassHandle() == info.typeHandle);
    assert(info.canPromoteStructType);

    lcl->SetPromotedFields(compiler->lvaCount, info.fieldCount);
    lcl->lvContainsHoles = info.containsHoles;
    lcl->lvCustomLayout  = info.customLayout;

    JITDUMP("\nPromoting struct local V%02u (%s):", lcl->GetLclNum(), lcl->GetLayout()->GetClassName());

    SortFields();

#ifdef TARGET_ARMARCH
    unsigned regIndex = 0;
#endif

    for (unsigned index = 0; index < info.fieldCount; ++index)
    {
        const FieldInfo& field = info.fields[index];
        assert((index == 0) || (field.offset > info.fields[index - 1].offset));

        if (varTypeUsesFloatReg(field.type))
        {
            compiler->compFloatingPointUsed = true;
        }

        FieldSeqNode* fieldSeq = nullptr;

        for (unsigned i = 0; i < field.fieldSeqLength; i++)
        {
            FieldSeqNode* fieldSeqNode = compiler->GetFieldSeqStore()->CreateSingleton(field.fieldSeq[i]);
            fieldSeq                   = compiler->GetFieldSeqStore()->Append(fieldSeq, fieldSeqNode);
        }

        LclVarDsc* fieldLcl = compiler->lvaAllocTemp(false DEBUGARG("promoted struct field"));
        fieldLcl->MakePromotedField(lcl->GetLclNum(), field.offset, fieldSeq);

        if (varTypeIsSIMD(field.type))
        {
            compiler->lvaSetStruct(fieldLcl, field.layout, false);
        }
        else
        {
            fieldLcl->SetType(field.type);
        }

        fieldLcl->lvIsParam = lcl->IsParam();

#ifdef TARGET_ARM64
        // TODO-MIKE-Fix: This is rather stupid but some code (the genPrologMoveParamRegs garbage
        // in particular) depends on vector params being marked as HFA even when they're not HFAs.
        if (lcl->IsHfaParam() && varTypeIsSIMD(fieldLcl->GetType()))
        {
            fieldLcl->SetIsHfaParam();
        }
#endif

        if (!lcl->IsRegParam())
        {
            continue;
        }

#if !FEATURE_MULTIREG_ARGS
        fieldLcl->SetParamRegs(lcl->GetParamReg());
#else
        if (lcl->IsImplicitByRefParam())
        {
            fieldLcl->SetParamRegs(lcl->GetParamReg());

            continue;
        }

#ifdef UNIX_AMD64_ABI
        if (varTypeIsSIMD(fieldLcl->GetType()) && (lcl->GetPromotedFieldCount() == 1))
        {
            fieldLcl->SetParamRegs(lcl->GetParamReg(0), lcl->GetParamReg(1));
        }
        else
        {
            assert(field.offset == REGSIZE_BYTES * index);

            fieldLcl->SetParamRegs(lcl->GetParamReg(index));
        }
#else // !UNIX_AMD64_ABI
        unsigned regCount = 1;

        if (lcl->IsHfaParam())
        {
            // We've sorted the field by offset so for HFA's we should have a 1:1 mapping
            // between fields and registers. Also, ensure that we didn't promote a HFA
            // with holes. Is that even a thing? The VM seems to think that it is...
            assert(field.offset == index * varTypeSize(lcl->GetLayout()->GetHfaElementType()));

#ifdef TARGET_ARM
            if (lcl->GetLayout()->GetHfaElementType() == TYP_DOUBLE)
            {
                // On ARM we count FLOAT rather than DOUBLE registers.
                regCount = 2;
            }
#elif defined(TARGET_ARM64)
            if ((lcl->GetLayout()->GetHfaElementType() == TYP_FLOAT) && !fieldLcl->TypeIs(TYP_FLOAT))
            {
                assert(varTypeIsSIMD(fieldLcl->GetType()));

                regCount = varTypeSize(fieldLcl->GetType()) / 4;
            }
#endif
        }
        else
        {
            assert(field.offset == index * REGSIZE_BYTES);

            // TODO-ARMARCH: Need to determine if/how to handle split args.
        }

        fieldLcl->SetParamRegs(lcl->GetParamReg(regIndex), regCount);
        regIndex += regCount;
#endif // !UNIX_AMD64_ABI
#endif // FEATURE_MULTIREG_ARGS
    }
}

void Compiler::phPromoteStructs()
{
    if (!opts.OptEnabled(CLFLG_STRUCTPROMOTE))
    {
        JITDUMP("  promotion opt flag not enabled\n");
        return;
    }

    if (fgNoStructPromotion)
    {
        JITDUMP("  promotion disabled by JitNoStructPromotion\n");
        return;
    }

    if (info.compIsVarArgs)
    {
        JITDUMP("  promotion disabled because of varargs\n");
        return;
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("\nlvaTable before phPromoteStructs\n");
        lvaTableDump();
    }
#endif

    StructPromotionHelper helper(this);

    for (LclVarDsc* lcl : Locals())
    {
        if (lvaHaveManyLocals())
        {
            JITDUMP("Stopped promoting struct fields, due to too many locals.\n");
            break;
        }

        // TODO-ObjectStackAllocation: Enable promotion of fields of stack-allocated objects.
        if (!varTypeIsStruct(lcl->GetType()) || !lcl->GetLayout()->IsValueClass())
        {
            continue;
        }

#ifdef DEBUG
        if (lcl->IsParam() && fgNoStructParamPromotion)
        {
            continue;
        }
#endif

        helper.TryPromoteStructLocal(lcl);
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("\nlvaTable after phPromoteStructs\n");
        lvaTableDump();
    }
#endif
}

// Traverses the entire method and marks address exposed locals.
// Trees such as IND_LOAD(LCL_ADDR) are transformed to just LCL_LOAD and
// do not result in the involved local being marked address exposed.
void Compiler::phMarkAddressExposedLocals()
{
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
    lvaResetImplicitByRefParamsRefCount();
#endif

    LocalAddressVisitor visitor(this);
#ifdef FEATURE_SIMD
    SIMDCoalescingBuffer buffer;
#endif

    for (BasicBlock* const block : Blocks())
    {
#ifdef FEATURE_SIMD
        buffer.Clear();
#endif

        for (Statement* stmt : block->Statements())
        {
            visitor.VisitStmt(stmt DEBUGARG(block));

#ifdef FEATURE_SIMD
            if (opts.OptimizationEnabled() && buffer.Add(this, stmt))
            {
                buffer.Coalesce(this, block);
            }
#endif
        }
    }

    lvaAddressExposedLocalsMarked = true;

    DBEXEC(verbose, lvaTableDump());

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

        switch (node->GetOper())
        {
            case GT_LCL_ADDR:
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
                MorphImplicitByRefParamAddr(node->AsLclAddr());
#else
                MorphVarargsStackParamAddr(node->AsLclAddr());
#endif
                return Compiler::WALK_SKIP_SUBTREES;
            case GT_LCL_STORE:
            case GT_LCL_STORE_FLD:
            case GT_LCL_LOAD:
            case GT_LCL_LOAD_FLD:
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
                MorphImplicitByRefParam(node);
#else
                MorphVarargsStackParam(node->AsLclVarCommon());
#endif
                return Compiler::WALK_CONTINUE;
            default:
                return Compiler::WALK_CONTINUE;
        }
    }

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
    void MorphImplicitByRefParamAddr(GenTreeLclAddr* addr)
    {
        LclVarDsc* lcl = addr->GetLcl();

        if (lcl->IsImplicitByRefParam())
        {
            // Can't assert lvAddrExposed/lvDoNotEnregister because lvaRetypeImplicitByRefParams
            // already cleared both of them.
            // assert(lclVarDsc->lvAddrExposed);
            // assert(lclVarDsc->lvDoNotEnregister);

            // Locals referenced by LCL_LOAD_FLD|_ADDR cannot be enregistered and currently
            // lvaRetypeImplicitByRefParams undoes promotion of such arguments. In this case
            // lvFieldCnt remains set to the number of promoted fields.
            assert((lcl->lvFieldLclStart == 0) || (lcl->lvFieldCnt != 0));

            unsigned      lclOffs  = addr->GetLclOffs();
            FieldSeqNode* fieldSeq = addr->GetFieldSeq();

            if ((lclOffs != 0) || (fieldSeq != nullptr))
            {
                // LocalAddressVisitor should not create unnecessary LCL_FLD_ADDR nodes.
                assert((lclOffs != 0) || (fieldSeq != FieldSeqStore::NotAField()));

                GenTree* add = addr;
                add->ChangeOper(GT_ADD);
                add->SetType(TYP_BYREF);
                add->AsOp()->SetOp(0, m_compiler->gtNewLclLoad(lcl, TYP_BYREF));
                add->AsOp()->SetOp(1, m_compiler->gtNewIconNode(lclOffs, fieldSeq));
                add->gtFlags = GTF_EMPTY;
            }
            else
            {
                addr->ChangeOper(GT_LCL_LOAD);
                addr->SetType(TYP_BYREF);
                addr->AsLclLoad()->SetLcl(lcl);
                addr->gtFlags = GTF_EMPTY;
            }

            INDEBUG(m_stmtModified = true;)
        }
        else if (lcl->IsPromotedField() &&
                 m_compiler->lvaGetDesc(lcl->GetPromotedFieldParentLclNum())->IsImplicitByRefParam())
        {
            // This was a field reference to an implicit-by-reference struct parameter that was dependently
            // promoted and now it is being demoted; update it to reference the original parameter.

            assert(lcl->GetPromotedFieldSeq() != nullptr);

            LclVarDsc*    paramLcl = m_compiler->lvaGetDesc(lcl->GetPromotedFieldParentLclNum());
            unsigned      lclOffs  = lcl->GetPromotedFieldOffset();
            FieldSeqNode* fieldSeq = lcl->GetPromotedFieldSeq();

            if ((addr->GetLclOffs() != 0) || (addr->GetFieldSeq() != nullptr))
            {
                lclOffs += addr->GetLclOffs();
                fieldSeq = m_compiler->GetFieldSeqStore()->Append(fieldSeq, addr->GetFieldSeq());
            }

            // Change LCL_LOAD|FLD_ADDR(paramPromotedField) into ADD(LCL_LOAD<BYREF>(param), offset)
            GenTree* add = addr;
            add->ChangeOper(GT_ADD);
            add->SetType(TYP_BYREF);
            add->AsOp()->SetOp(0, m_compiler->gtNewLclLoad(paramLcl, TYP_BYREF));
            add->AsOp()->SetOp(1, m_compiler->gtNewIconNode(lclOffs, fieldSeq));
            add->gtFlags = GTF_EMPTY;

            INDEBUG(m_stmtModified = true;)
        }
    }

    void MorphImplicitByRefParam(GenTree* tree)
    {
        assert(tree->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD, GT_LCL_STORE, GT_LCL_STORE_FLD));

        GenTreeLclVarCommon* lclNode = tree->AsLclVarCommon();
        LclVarDsc*           lcl     = lclNode->GetLcl();

        if (lcl->IsImplicitByRefParam())
        {
            // lvaRetypeImplicitByRefParams creates LCL_LOAD nodes that reference
            // implicit byref params and are already TYP_BYREF, ignore them.
            if (lclNode->OperIs(GT_LCL_LOAD) && lclNode->TypeIs(TYP_BYREF))
            {
                return;
            }

            assert(varTypeIsStruct(lclNode->GetType()) || lclNode->OperIs(GT_LCL_LOAD_FLD, GT_LCL_STORE_FLD));

            if ((lcl->lvFieldLclStart != 0) && (lcl->lvFieldCnt == 0))
            {
                // lvaRetypeImplicitByRefParams created a new promoted struct local to represent this param.
                // Rewrite this to refer to the new local. We should never encounter a LCL_LOAD|STORE_FLD
                // because promotion is aborted if it turns out that it is dependent.

                LclVarDsc* promotedLcl = m_compiler->lvaGetDesc(lcl->lvFieldLclStart);

                assert(lclNode->OperIs(GT_LCL_LOAD, GT_LCL_STORE));
                assert(varTypeIsStruct(promotedLcl->GetType()));

                lclNode->SetLcl(promotedLcl);
            }
            else
            {
                GenTreeIntCon* offset = nullptr;

                if (lclNode->OperIs(GT_LCL_LOAD_FLD, GT_LCL_STORE_FLD))
                {
                    unsigned      lclOffs  = lclNode->AsLclFld()->GetLclOffs();
                    FieldSeqNode* fieldSeq = lclNode->AsLclFld()->GetFieldSeq();

                    if ((lclOffs != 0) || (fieldSeq != FieldSeqStore::NotAField()))
                    {
                        offset = m_compiler->gtNewIconNode(lclOffs, fieldSeq);
                    }
                }

                GenTree* addr  = m_compiler->gtNewLclLoad(lcl, TYP_BYREF);
                GenTree* value = nullptr;

                if (offset != nullptr)
                {
                    addr = m_compiler->gtNewOperNode(GT_ADD, TYP_BYREF, addr, offset);
                }

                if (lclNode->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD))
                {
                    value = lclNode->GetOp(0);
                }

                ClassLayout* layout = nullptr;

                if (varTypeIsStruct(lclNode->GetType()))
                {
                    layout = lclNode->OperIs(GT_LCL_LOAD, GT_LCL_STORE) ? lcl->GetImplicitByRefParamLayout()
                                                                        : lclNode->AsLclFld()->GetLayout(m_compiler);
                }

                if (layout != nullptr)
                {
                    tree->ChangeOper(value == nullptr ? GT_IND_LOAD_OBJ : GT_IND_STORE_OBJ);
                    tree->AsBlk()->SetLayout(layout);
                    tree->AsBlk()->SetAddr(addr);
                    tree->AsBlk()->SetKind(StructStoreKind::Invalid);
                }
                else
                {
                    tree->ChangeOper(value == nullptr ? GT_IND_LOAD : GT_IND_STORE);
                }

                tree->AsIndir()->SetAddr(addr);

                if (value != nullptr)
                {
                    tree->AsIndir()->SetValue(value);
                }

                tree->gtFlags = GTF_GLOB_REF | GTF_IND_NONFAULTING;
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

            if (lclNode->OperIs(GT_LCL_LOAD_FLD, GT_LCL_STORE_FLD))
            {
                lclOffs += lclNode->AsLclFld()->GetLclOffs();
                fieldSeq = m_compiler->GetFieldSeqStore()->Append(fieldSeq, lclNode->AsLclFld()->GetFieldSeq());
            }

            GenTreeIntCon* offset = m_compiler->gtNewIconNode(lclOffs, fieldSeq);

            // Change LCL_LOAD|STORE<fieldType>(paramPromotedField) into
            // IND_LOAD|STORE<fieldType>(ADD(LCL_LOAD<BYREF>(param), offset))

            GenTree* value = nullptr;

            if (lclNode->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD))
            {
                value = lclNode->GetOp(0);
            }

            if (lclNode->TypeIs(TYP_STRUCT))
            {
                ClassLayout* layout = nullptr;

                if (lclNode->OperIs(GT_LCL_LOAD_FLD, GT_LCL_STORE_FLD))
                {
                    layout = lclNode->AsLclFld()->GetLayout(m_compiler);
                }
                else
                {
                    layout = lcl->GetImplicitByRefParamLayout();
                }

                tree->ChangeOper(value == nullptr ? GT_IND_LOAD_OBJ : GT_IND_STORE_OBJ);
                tree->AsBlk()->SetLayout(layout);
            }
            else
            {
                tree->ChangeOper(value == nullptr ? GT_IND_LOAD : GT_IND_STORE);
            }

            // TODO-MIKE-Review: Are implicit by ref params really BYREF? They should
            // have local storage, unless the JIT suddenly acquires magical powers that
            // let it elide local copies if a param is known to be not modified/aliased.
            lclNode = m_compiler->gtNewLclLoad(lcl, TYP_BYREF);

            tree->AsIndir()->SetAddr(m_compiler->gtNewOperNode(GT_ADD, TYP_BYREF, lclNode, offset));

            if (value != nullptr)
            {
                tree->AsIndir()->SetValue(value);
            }

            tree->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;

            INDEBUG(m_stmtModified = true;)
        }
    }
#elif defined(TARGET_X86)
    void MorphVarargsStackParamAddr(GenTreeLclAddr* lclAddr)
    {
        if (!IsVarargsStackParam(lclAddr->GetLcl()))
        {
            return;
        }

        GenTree* base   = m_compiler->gtNewLclLoad(m_compiler->lvaVarargsBaseOfStkLcl, TYP_I_IMPL);
        GenTree* offset = GetVarargsStackParamOffset(lclAddr->GetLcl(), lclAddr->GetLclOffs());
        GenTree* addr   = lclAddr;

        addr->ChangeOper(GT_ADD);
        addr->SetType(TYP_I_IMPL);
        addr->AsOp()->SetOp(0, base);
        addr->AsOp()->SetOp(1, offset);

        INDEBUG(m_stmtModified = true;)
    }

    void MorphVarargsStackParam(GenTreeLclVarCommon* lclNode)
    {
        assert(lclNode->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD, GT_LCL_STORE, GT_LCL_STORE_FLD));

        if (!IsVarargsStackParam(lclNode->GetLcl()))
        {
            return;
        }

        GenTree* base   = m_compiler->gtNewLclLoad(m_compiler->lvaVarargsBaseOfStkLcl, TYP_I_IMPL);
        GenTree* offset = GetVarargsStackParamOffset(lclNode->GetLcl(), lclNode->GetLclOffs());
        GenTree* addr   = m_compiler->gtNewOperNode(GT_ADD, TYP_I_IMPL, base, offset);
        GenTree* value  = nullptr;
        GenTree* indir  = lclNode;

        if (lclNode->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD))
        {
            value = lclNode->GetOp(0);
        }

        if (varTypeIsStruct(lclNode->GetType()))
        {
            ClassLayout* layout = lclNode->OperIs(GT_LCL_LOAD, GT_LCL_STORE)
                                      ? lclNode->GetLcl()->GetLayout()
                                      : lclNode->AsLclFld()->GetLayout(m_compiler);

            indir->ChangeOper(value == nullptr ? GT_IND_LOAD_OBJ : GT_IND_STORE_OBJ);
            indir->AsBlk()->SetLayout(layout);
        }
        else
        {
            indir->ChangeOper(value == nullptr ? GT_IND_LOAD : GT_IND_STORE);
        }

        indir->AsIndir()->SetAddr(addr);

        if (value != nullptr)
        {
            indir->AsIndir()->SetValue(value);
        }

        indir->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;

        INDEBUG(m_stmtModified = true;)
    }

    bool IsVarargsStackParam(LclVarDsc* lcl) const
    {
        return lcl->IsParam() && !lcl->IsRegParam() && (lcl->GetLclNum() != m_compiler->info.compVarargsHandleArg);
    }

    GenTreeIntCon* GetVarargsStackParamOffset(LclVarDsc* lcl, unsigned lclOffs) const
    {
        int stkOffs = lcl->GetStackOffset();
        return m_compiler->gtNewIconNode(stkOffs + lclOffs - m_compiler->codeGen->paramsStackSize, TYP_INT);
    }
#endif // !TARGET_X86
};

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)

void LclVarDsc::AddImplicitByRefParamAnyRef()
{
    assert(JitTls::GetCompiler()->lvaRefCountState == RCS_MORPH);

    m_refWeight++;
}

unsigned LclVarDsc::GetImplicitByRefParamAnyRefCount() const
{
    assert(JitTls::GetCompiler()->lvaRefCountState == RCS_MORPH);

    return m_refWeight;
}

void LclVarDsc::AddImplicitByRefParamCallRef()
{
    assert(JitTls::GetCompiler()->lvaRefCountState == RCS_MORPH);

    m_refCount = m_refCount == UINT16_MAX ? UINT16_MAX : (m_refCount + 1);
}

unsigned LclVarDsc::GetImplicitByRefParamCallRefCount() const
{
    assert(JitTls::GetCompiler()->lvaRefCountState == RCS_MORPH);

    return m_refCount;
}

// Reset the ref count of implicit byref params; fgMarkAddressTakenLocals
// will increment it per appearance of implicit byref param so that call
// arg morphing can do an optimization for single-use implicit byref
// params whose single use is as an outgoing call argument.
void Compiler::lvaResetImplicitByRefParamsRefCount()
{
    JITDUMP("\n*************** In lvaResetImplicitByRefParamsRefCount()\n");

    lvaRefCountState          = RCS_MORPH;
    lvaHasImplicitByRefParams = false;

    for (LclVarDsc* lcl : Params())
    {
        if (lcl->IsImplicitByRefParam())
        {
            // We haven't use ref counts until now so they should be 0.
            assert(lcl->GetImplicitByRefParamAnyRefCount() == 0);
            assert(lcl->GetImplicitByRefParamCallRefCount() == 0);

            lvaHasImplicitByRefParams = true;
        }
    }

    if (!opts.OptimizationEnabled() || !lvaHasImplicitByRefParams)
    {
        // It turns out that we do not actually need ref counting.
        lvaRefCountState = RCS_INVALID;
    }
}

// Change the type of implicit byref parameters from struct to byref.
// Also choose (based on address-exposed analysis) which struct promotions
// of implicit byref params to keep or discard.
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

    for (LclVarDsc* lcl : Params())
    {
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

            unsigned totalAppearances = lcl->GetImplicitByRefParamAnyRefCount();
            unsigned callAppearances  = lcl->GetImplicitByRefParamCallRefCount();
            assert(totalAppearances >= callAppearances);
            unsigned nonCallAppearances  = totalAppearances - callAppearances;
            bool     isDependentPromoted = lcl->IsDependentPromoted();

            bool undoPromotion = isDependentPromoted || (nonCallAppearances <= lcl->GetPromotedFieldCount());

            JITDUMP("%s promotion of implicit byref V%02u: %s total: %u non-call: %u fields: %u\n",
                    undoPromotion ? "Undoing" : "Keeping", lcl->GetLclNum(), isDependentPromoted ? "dependent;" : "",
                    totalAppearances, nonCallAppearances, lcl->GetPromotedFieldCount());

            if (undoPromotion)
            {
                for (LclVarDsc* fieldLcl : PromotedFields(lcl))
                {
                    // Leave lvParentLcl pointing to the parameter so that fgMorphIndirectParams
                    // will know to rewrite appearances of this local.
                    assert(fieldLcl->GetPromotedFieldParentLclNum() == lcl->GetLclNum());

                    // The fields shouldn't inherit any register preferences from the parameter.
                    fieldLcl->lvIsParam = false;
                    fieldLcl->ClearParamRegs();
                }

                // Reset lvPromoted since the param is no longer promoted but keep lvFieldLclStart
                // and lvFieldCnt unchanged so lvaDemoteImplicitByRefParams can find the fields
                // to "delete" them.
                lcl->lvPromoted = false;
            }
            else
            {
                LclVarDsc* structLcl = lvaNewTemp(lcl->GetLayout(), false DEBUGARG("promoted implicit byref param"));

                structLcl->SetPromotedFields(lcl->GetPromotedFieldLclNum(0), lcl->GetPromotedFieldCount());
                structLcl->lvContainsHoles   = lcl->lvContainsHoles;
                structLcl->lvCustomLayout    = lcl->lvCustomLayout;
                structLcl->lvAddrExposed     = lcl->IsAddressExposed();
                structLcl->lvDoNotEnregister = lcl->lvDoNotEnregister;
#ifdef DEBUG
                structLcl->lvLclBlockOpAddr   = lcl->lvLclBlockOpAddr;
                structLcl->lvLclFieldExpr     = lcl->lvLclFieldExpr;
                structLcl->lvLiveInOutOfHndlr = lcl->lvLiveInOutOfHndlr;
#endif

                unsigned structLclNum = structLcl->GetLclNum();

                fgEnsureFirstBBisScratch();
                GenTree* addr = gtNewLclLoad(lcl, TYP_BYREF);
                GenTree* load = gtNewIndLoadObj(structLcl->GetType(), structLcl->GetLayout(), addr);
                fgNewStmtAtBeg(fgFirstBB, gtNewLclStore(structLcl, lcl->GetType(), load));

                // Update the locals corresponding to the promoted fields.

                for (LclVarDsc* fieldLcl : PromotedFields(structLcl))
                {
                    fieldLcl->lvParentLcl = structLclNum;

                    // The fields shouldn't inherit any register preferences from the parameter.
                    fieldLcl->lvIsParam = false;
                    fieldLcl->ClearParamRegs();
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

        JITDUMP("Changed the type of struct parameter V%02d to TYP_BYREF.\n", lcl->GetLclNum());
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
// Clear annotations for any implicit byref params that struct promotion
// asked to promote. Appearances of these have now been rewritten by
// fgMorphIndirectParams using indirections from the pointer parameter
// or references to the promoted fields, as appropriate.
void Compiler::lvaDemoteImplicitByRefParams() const
{
    if (!lvaHasImplicitByRefParams)
    {
        return;
    }

    for (LclVarDsc* lcl : Params())
    {
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
                assert(fieldLcl->lvParentLcl == lcl->GetLclNum());
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
