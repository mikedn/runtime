// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "jitstd/algorithm.h"

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
        char message[64];

        if (m_compiler->verbose)
        {
            sprintf_s(message, sizeof(message), "LocalAddressVisitor visiting statement " FMT_BB,
                      m_compiler->compCurBB->bbNum);
            m_compiler->gtDispStmt(stmt, message);
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
                sprintf_s(message, sizeof(message), "LocalAddressVisitor modified statement " FMT_BB,
                          m_compiler->compCurBB->bbNum);
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

                    GenTreeUnOp* ret = node->AsUnOp();

                    if (ret->GetOp(0)->OperIs(GT_LCL_VAR))
                    {
                        // TODO-1stClassStructs: this block is a temporary workaround to keep diffs small,
                        // having `doNotEnreg` affect block init and copy transformations that affect many methods.
                        // I have a change that introduces more precise and effective solution for that, but it would
                        // be merged separatly.

                        unsigned   lclNum = ret->GetOp(0)->AsLclVar()->GetLclNum();
                        LclVarDsc* lcl    = m_compiler->lvaGetDesc(lclNum);

                        if ((m_compiler->info.retDesc.GetRegCount() == 1) && !lcl->IsImplicitByRefParam() &&
                            lcl->IsPromoted() && (lcl->GetPromotedFieldCount() > 1) && !varTypeIsSIMD(lcl->GetType()))
                        {
                            m_compiler->lvaSetVarDoNotEnregister(lclNum DEBUGARG(Compiler::DNER_BlockOp));
                        }
                    }

                    EscapeValue(TopValue(0), node);
                    PopValue();

                    if (ret->GetOp(0)->TypeIs(TYP_STRUCT) && IsMergedReturnAssignment(ret))
                    {
                        LclVarDsc* lcl = m_compiler->lvaGetDesc(m_compiler->genReturnLocal);

                        if (lcl->IsPromoted())
                        {
                            assert(lcl->GetPromotedFieldCount() == 1);
                            RetypeMergedReturnStructAssignment(ret, lcl);
                        }
                    }
                }
                break;

            case GT_ASG:
                if (node->TypeIs(TYP_STRUCT))
                {
                    PostOrderVisitStructAssignment(node->AsOp());
                    break;
                }
                FALLTHROUGH;
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

    void PostOrderVisitStructAssignment(GenTreeOp* asg)
    {
        assert(asg->OperIs(GT_ASG) && asg->TypeIs(TYP_STRUCT));
        assert(TopValue(2).Node() == asg);
        assert(TopValue(1).Node() == asg->GetOp(0));
        assert(TopValue(0).Node() == asg->GetOp(1));

        // TODO-MIKE-Cleanup: We can't assert here yet because MorphStructField already messed up the types.
        // assert(op1->TypeIs(TYP_STRUCT));
        // assert(op2->TypeIs(TYP_STRUCT) || op2->IsIntegralConst(0) || op2->OperIs(GT_INIT_VAL));

        EscapeValue(TopValue(0), asg);
        EscapeValue(TopValue(1), asg);

        GenTree* op1 = asg->GetOp(0);
        GenTree* op2 = asg->GetOp(1);

        if (op2->IsIntegralConst(0))
        {
            if (!op1->TypeIs(TYP_STRUCT))
            {
                asg->SetOp(1, RetypeStructZeroInit(op2, op1->GetType()));
                asg->SetType(op1->GetType());
            }
        }
        else if (op2->OperIs(GT_INIT_VAL))
        {
            // Currently MorphLocalIndir doesn't touch BLKs so we don't need to deal with INIT_VAL.
            assert(op1->TypeIs(TYP_STRUCT));
        }
        else if (!op1->TypeIs(TYP_STRUCT) && !op2->TypeIs(TYP_STRUCT))
        {
            RetypeScalarAssignment(asg, op1, op2);
        }
        else if (op1->TypeIs(TYP_STRUCT) != op2->TypeIs(TYP_STRUCT))
        {
            RetypeStructAssignment(asg, op1, op2);
        }

        PopValue();
        PopValue();
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
    void EscapeValue(const Value& val, GenTree* user)
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
    void EscapeAddress(const Value& val, GenTree* user)
    {
        assert(val.IsAddress());

        LclVarDsc* lcl = m_compiler->lvaGetDesc(val.LclNum());

        // In general we don't know how an exposed struct field address will be used - it may be used to
        // access only that specific field or it may be used to access other fields in the same struct
        // be using pointer/ref arithmetic.
        bool exposeParentLcl = lcl->IsPromotedField();

        if (exposeParentLcl)
        {
            if (GenTreeCall* call = user->IsCall())
            {
                // It seems reasonable to make an exception for the "this" arg
                // of calls - it would be highly unsual for a struct member method to attempt to access memory
                // beyond "this" instance. And calling struct member methods is common enough that attempting to
                // mark the entire struct as address exposed results in CQ regressions.

                if ((call->gtCallThisArg != nullptr) && (val.Node() == call->gtCallThisArg->GetNode()))
                {
                    exposeParentLcl = false;
                }
            }
            else if (user->OperIsAtomicOp())
            {
                assert(!user->TypeIs(TYP_STRUCT) && !lcl->TypeIs(TYP_STRUCT));

                if ((varTypeSize(user->GetType()) <= varTypeSize(lcl->GetType())) &&
                    (val.Node() == (user->IsCmpXchg() ? user->AsCmpXchg()->gtOpLocation : user->AsOp()->GetOp(0))))
                {
                    exposeParentLcl = false;
                }
            }
        }

        m_compiler->lvaSetVarAddrExposed(exposeParentLcl ? lcl->GetPromotedFieldParentLclNum() : val.LclNum());

#ifdef TARGET_64BIT
        // If the address of a variable is passed in a call and the allocation size of the variable
        // is 32 bits we will quirk the size to 64 bits. Some PInvoke signatures incorrectly specify
        // a ByRef to an INT32 when they actually write a SIZE_T or INT64. There are cases where
        // overwriting these extra 4 bytes corrupts some data (such as a saved register) that leads
        // to A/V. Wheras previously the JIT64 codegen did not lead to an A/V.
        if (!lcl->IsParam() && !lcl->IsPromotedField() && (varActualType(lcl->GetType()) == TYP_INT))
        {
            // TODO-Cleanup: This should simply check if the user is a call node, not if a call ancestor exists.
            if (Compiler::gtHasCallOnStack(&m_ancestors))
            {
                lcl->lvQuirkToLong = true;
                JITDUMP("Adding a quirk for the storage size of V%02u of type %s\n", val.LclNum(),
                        varTypeName(lcl->GetType()));
            }
        }
#endif // TARGET_64BIT

        // TODO-ADDR: For now use LCL_VAR_ADDR and LCL_FLD_ADDR only in certain cases.
        // Other usages require more changes. For example, a tree like OBJ(ADD(ADDR(LCL_VAR), 4))
        // could be changed to OBJ(LCL_FLD_ADDR) but then DefinesLocalAddr does not recognize
        // LCL_FLD_ADDR (even though it does recognize LCL_VAR_ADDR).
        if (user->OperIs(GT_CALL, GT_ASG, GT_CMPXCHG))
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

            if (node->OperIs(GT_LCL_VAR) && lcl->IsPromoted() && (lcl->GetPromotedFieldCount() == 1))
            {
                switch (user->GetOper())
                {
                    case GT_ASG:
                        PromoteSingleFieldStructLocalAssignment(lcl, node->AsLclVar(), user->AsOp());
                        break;
                    case GT_CALL:
                        PromoteSingleFieldStructLocalCallArg(lcl, node->AsLclVar());
                        break;
                    case GT_RETURN:
                        PromoteSingleFieldStructLocalReturn(lcl, node->AsLclVar(), user->AsUnOp());
                        break;
                    default:
                        // Let's hope the importer doesn't produce STRUCT COMMAs again.
                        unreached();
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

    void PromoteSingleFieldStructLocalAssignment(LclVarDsc* lcl, GenTreeLclVar* lclVar, GenTreeOp* asg)
    {
        assert(lcl->TypeIs(TYP_STRUCT));
        assert(lcl->GetPromotedFieldCount() == 1);
        assert(asg->OperIs(GT_ASG) && asg->TypeIs(TYP_STRUCT));

        unsigned   fieldLclNum = lcl->GetPromotedFieldLclNum(0);
        LclVarDsc* fieldLcl    = m_compiler->lvaGetDesc(fieldLclNum);

        assert(lcl->GetLayout()->GetSize() == varTypeSize(fieldLcl->GetType()));

        lclVar->SetLclNum(fieldLclNum);
        lclVar->SetType(fieldLcl->GetType());

        INDEBUG(m_stmtModified = true;)
    }

    void PromoteSingleFieldStructLocalCallArg(LclVarDsc* lcl, GenTreeLclVar* lclVar)
    {
        assert(lcl->TypeIs(TYP_STRUCT));
        assert(lcl->GetPromotedFieldCount() == 1);

        unsigned   fieldLclNum = lcl->GetPromotedFieldLclNum(0);
        LclVarDsc* fieldLcl    = m_compiler->lvaGetDesc(fieldLclNum);

        assert(lcl->GetLayout()->GetSize() == varTypeSize(fieldLcl->GetType()));

        lclVar->SetLclNum(fieldLclNum);
        lclVar->SetType(fieldLcl->GetType());

        INDEBUG(m_stmtModified = true;)
    }

    void PromoteSingleFieldStructLocalReturn(LclVarDsc* lcl, GenTreeLclVar* lclVar, GenTreeUnOp* ret)
    {
        assert(lcl->TypeIs(TYP_STRUCT));
        assert(lcl->GetPromotedFieldCount() == 1);
        assert(ret->TypeIs(TYP_STRUCT));

        unsigned   fieldLclNum = lcl->GetPromotedFieldLclNum(0);
        LclVarDsc* fieldLcl    = m_compiler->lvaGetDesc(fieldLclNum);

        assert(lcl->GetLayout()->GetSize() == varTypeSize(fieldLcl->GetType()));

        lclVar->SetLclNum(fieldLclNum);
        lclVar->SetType(fieldLcl->GetType());

        INDEBUG(m_stmtModified = true;)

        if (IsMergedReturnAssignment(ret))
        {
            // This is a merged return, it will be transformed into a struct
            // assignment so leave it to fgMorphCopyBlock to promote it.
            return;
        }

        const ReturnTypeDesc& retDesc = m_compiler->info.retDesc;

        if (retDesc.GetRegCount() == 1)
        {
            var_types retRegType = varActualType(retDesc.GetRegType(0));
            ret->SetType(retRegType);

            if (varTypeUsesFloatReg(retRegType) != varTypeUsesFloatReg(fieldLcl->GetType()))
            {
                ret->SetOp(0, NewBitCastNode(retRegType, lclVar));
            }

            return;
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
                ret->SetOp(0, NewExtractElement(TYP_LONG, lclVar, TYP_SIMD16, 0));
            }
            else
            {
                assert(fieldLcl->TypeIs(TYP_LONG, TYP_DOUBLE));
            }

            return;
        }
#endif

        // We either have a SIMD field that's returned in multiple registers (e.g. HFA)
        // or perhaps the IL is invalid (e.g. struct with a single DOUBLE field returned
        // as a Vector2 or some other 2 FLOAT field struct that is a HFA).
        // Either way, leave it to morph to produce a FIELD_LIST in this case.
        // We could probably do it here but it's not clear if it has any benefits.
        assert(varTypeIsSIMD(fieldLcl->GetType()));
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
            return m_compiler->info.compCompHnd->getClassSize(GetStructFieldType(field));
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

            // TODO-MIKE-CQ: Can we use another mechanism to avoid dependent promotion of SpinLock?
            // DNER would do half of the job, it keeps the local in memory but does not prevent other
            // optimizations that may interfere with volatile. Strictly speaking, neither address
            // exposed provided any such guarantees, it's just that the JIT is very conservative with
            // address exposed locals. Maybe DNER + DONT_CSE + ORDER_SIDEEFF?
            // It's unlikely to have SpinLock local variable in real world code but we do get local
            // temps for SpinLock construction. And currently they are independent promoted because
            // MorphStructField doesn't check for volatile access, which is basically a bug...
            // At the same time, it's unlikely that SpinLock's constructor really needs volatile,
            // only Enter/Exit do.

            // For now make the local address exposed to workaround a bug in fgMorphSmpOp's
            // IND morphing code. It completly ignores volatile indirs and in doing so it
            // fails to DNER the local which leads to asserts in the backend.
            m_compiler->lvaSetVarAddrExposed(val.LclNum());
            return;
        }

        LclVarDsc* varDsc = m_compiler->lvaGetDesc(val.LclNum());

        if (indir->OperIs(GT_BLK) || (indir->OperIs(GT_IND) && indir->TypeIs(TYP_STRUCT)))
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

        var_types lclType   = varDsc->GetType();
        var_types indirType = indir->GetType();

        // A non-struct local may be accessed via a struct indir due to reinterpretation in user
        // code or due to single field struct promotion. Reinterpretation isn't common (but it
        // does happen - e.g. ILCompiler.Reflection.ReadyToRun.dll reinterprets array references
        // as ImmutableArray) but promotion is quite common. If the indir is a call arg then we
        // can simply replace it with the local variable because it doesn't really matter if it's
        // a struct value or a primitive type value, we just need to put the value in the correct
        // register or stack slot.

        if ((val.Offset() == 0) && (indirType == TYP_STRUCT) && (lclType != TYP_STRUCT))
        {
            ClassLayout* indirLayout = GetStructIndirLayout(indir);

            switch (user->GetOper())
            {
                case GT_ASG:
                    if (MorphLocalStructIndirAssignment(val, indir, indirLayout))
                    {
                        return;
                    }
                    break;
                case GT_CALL:
                    if (MorphLocalStructIndirCallArg(val, indir, indirLayout))
                    {
                        return;
                    }
                    break;
                case GT_RETURN:
                    if (MorphLocalStructIndirReturn(val, indir, indirLayout, user->AsUnOp()))
                    {
                        return;
                    }
                    break;
                default:
                    // Let's hope the importer doesn't produce STRUCT COMMAs again.
                    unreached();
            }
        }

        if (!varTypeIsStruct(lclType))
        {
            // TODO-MIKE-Cleanup: This likely makes a bunch of IND morphing code in fgMorphSmpOp redundant.

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

            if (indir->OperIs(GT_IND, GT_OBJ, GT_FIELD))
            {
                ClassLayout* layout = varTypeIsStruct(indir->GetType()) ? GetStructIndirLayout(indir) : nullptr;

                indir->ChangeOper(GT_LCL_FLD);
                indir->AsLclFld()->SetLclNum(val.LclNum());
                indir->AsLclFld()->SetLclOffs(val.Offset());
                indir->AsLclFld()->SetLayoutNum(layout == nullptr ? 0 : m_compiler->typGetLayoutNum(layout));
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

                if (indir->GetType() !=
                    CorTypeToVarType(m_compiler->info.compCompHnd->getFieldType(fieldSeq->GetTail()->GetFieldHandle())))
                {
                    fieldSeq = nullptr;
                }
            }
        }
        else if (indir->OperIs(GT_IND))
        {
            // Can't have STRUCT typed IND nodes here, they're only generated as BLK sources
            // and we have been rejected BLKs earlier.

            assert(varTypeIsSIMD(indir->GetType()));
        }
        else
        {
            indirLayout = GetStructIndirLayout(indir);

            assert(!indirLayout->IsBlockLayout());

            if ((fieldSeq != nullptr) && !indir->OperIs(GT_FIELD) &&
                (indirLayout->GetClassHandle() != GetStructFieldType(fieldSeq->GetTail()->GetFieldHandle())))
            {
                fieldSeq = nullptr;
            }
        }

        // For STRUCT locals, if the indir layout doesn't match and the local is promoted
        // then ignore the indir layout to avoid having to make a LCL_FLD and dependent
        // promote the local. The indir layout isn't really needed anymore, since call arg
        // morphing uses the one from the call signature.

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

        if ((val.Offset() == 0) && (indir->GetType() == varDsc->GetType()) &&
            (!indir->TypeIs(TYP_STRUCT) || (indirLayout == varDsc->GetLayout()) ||
             (varDsc->IsPromoted() && indirLayout->GetSize() == varDsc->GetLayout()->GetSize())))
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

    bool MorphLocalStructIndirAssignment(const Value& val, GenTree* indir, ClassLayout* indirLayout)
    {
        assert(val.Offset() == 0);
        assert(indir->TypeIs(TYP_STRUCT));

        LclVarDsc* lcl = m_compiler->lvaGetDesc(val.LclNum());
        assert(!lcl->TypeIs(TYP_STRUCT));

        if (indirLayout->GetSize() == varTypeSize(lcl->GetType()))
        {
            indir->ChangeOper(GT_LCL_VAR);
            indir->SetType(lcl->GetType());
            indir->AsLclVar()->SetLclNum(val.LclNum());
            indir->gtFlags = GTF_EMPTY;

            INDEBUG(m_stmtModified = true;)

            return true;
        }

        return false;
    }

    bool MorphLocalStructIndirCallArg(const Value& val, GenTree* indir, ClassLayout* indirLayout)
    {
        return MorphLocalStructIndirAssignment(val, indir, indirLayout);
    }

    bool MorphLocalStructIndirReturn(const Value& val, GenTree* indir, ClassLayout* indirLayout, GenTreeUnOp* ret)
    {
        assert(val.Offset() == 0);
        assert(indir->TypeIs(TYP_STRUCT));
        assert(ret->OperIs(GT_RETURN) && ret->TypeIs(TYP_STRUCT));
        assert(m_compiler->info.GetRetLayout()->GetSize() == indirLayout->GetSize());

        LclVarDsc* lcl = m_compiler->lvaGetDesc(val.LclNum());
        assert(!lcl->TypeIs(TYP_STRUCT));

        var_types             lclType     = lcl->GetType();
        const ReturnTypeDesc& retDesc     = m_compiler->info.retDesc;
        bool                  useLcl      = false;
        var_types             bitcastType = TYP_UNDEF;

        if (IsMergedReturnAssignment(ret))
        {
            // This is a merged return, it will be transformed into a struct
            // assignment so leave it to fgMorphCopyBlock to handle it.

            LclVarDsc* mergedLcl = m_compiler->lvaGetDesc(m_compiler->genReturnLocal);
            assert(mergedLcl->TypeIs(TYP_STRUCT));

            if (!mergedLcl->IsPromoted())
            {
                // The merged return temp is a struct and it's not promoted.
                // In general we can use a LCL_FLD to store the return value.

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
            // by creating a LCL_FLD matching the return type.

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

            GenTree* addr = indir->IsField() ? indir->AsField()->GetAddr() : indir->AsObj()->GetAddr();

            addr->ChangeOper(GT_LCL_VAR);
            addr->SetType(lclType);
            addr->AsLclVar()->SetLclNum(val.LclNum());
            addr->gtFlags = GTF_EMPTY;

            indir->ChangeOper(GT_BITCAST);
            indir->SetType(bitcastType);
            indir->AsUnOp()->SetOp(0, addr);
            indir->gtFlags = GTF_EMPTY;

            INDEBUG(m_stmtModified = true;)

            return true;
        }

        if (useLcl)
        {
            indir->ChangeOper(GT_LCL_VAR);
            indir->SetType(lclType);
            indir->AsLclVar()->SetLclNum(val.LclNum());
            indir->gtFlags = GTF_EMPTY;

            INDEBUG(m_stmtModified = true;)

            return true;
        }

        return false;
    }

    void RetypeScalarAssignment(GenTreeOp* asg, GenTree* op1, GenTree* op2)
    {
        assert(asg->OperIs(GT_ASG) && asg->TypeIs(TYP_STRUCT));
        assert(!op1->TypeIs(TYP_STRUCT) && !op2->TypeIs(TYP_STRUCT));

        // Both struct operands were changed to scalars, currently we only do this if the
        // struct size matches the scalar type size and since both operands of a struct
        // assignment are supposed to have the same struct type the resulting scalar types
        // should also have the same size. It may be possible to tolerate size mismatches
        // for small int types but it's unlikely to be worth the trouble.
        //
        // TODO-MIKE-Review: The importer doesn't do proper IL validation and we may get
        // here with different struct types. Probably it doesn't matter, unless it results
        // in JIT crashes...
        assert(varTypeSize(op1->GetType()) == varTypeSize(op2->GetType()));

        // We only change a struct operand to a scalar if we access scalar locals as structs
        // (either due to promotion or reinterpretation). Otherwise we'd simply generate
        // struct LCL_FLDs.
        assert(op1->OperIs(GT_LCL_VAR));
        assert(op2->OperIs(GT_LCL_VAR));

        // We don't allow type size changes but we don't otherwise care about the scalar
        // types so we could end up with INT/FLOAT and DOUBLE/LONG/SIMD8 mismatches.
        if ((varTypeUsesFloatReg(op1->GetType()) != varTypeUsesFloatReg(op2->GetType())) &&
            (varTypeSize(op1->GetType()) <= REGSIZE_BYTES))
        {
            asg->SetOp(1, NewBitCastNode(varActualType(op1->GetType()), op2));
        }
        else if (varActualType(op1->GetType()) != varActualType(op2->GetType()))
        {
            op2->ChangeOper(GT_LCL_FLD);
            op2->SetType(op1->GetType());

            m_compiler->lvaSetVarDoNotEnregister(op2->AsLclFld()->GetLclNum() DEBUGARG(Compiler::DNER_LocalField));
        }

        asg->SetType(op1->GetType());
    }

    void RetypeStructAssignment(GenTreeOp* asg, GenTree* op1, GenTree* op2)
    {
        assert(asg->OperIs(GT_ASG) && asg->TypeIs(TYP_STRUCT));
        assert(op1->TypeIs(TYP_STRUCT) != op2->TypeIs(TYP_STRUCT));

        bool     isStructDef = op1->TypeIs(TYP_STRUCT);
        GenTree* structOp    = isStructDef ? op1 : op2;
        GenTree* scalarOp    = isStructDef ? op2 : op1;

        // We only change a struct operand to a scalar if we access scalar locals as structs
        // (either due to promotion or reinterpretation). Otherwise we'd simply generate
        // struct LCL_FLDs.
        assert(scalarOp->OperIs(GT_LCL_VAR));

        var_types type = scalarOp->GetType();

        if (GenTreeCall* call = structOp->IsCall())
        {
            structOp = RetypeStructCall(call, type);
        }
        else if (structOp->OperIs(GT_LCL_FLD, GT_LCL_VAR))
        {
            structOp = RetypeStructLocal(structOp->AsLclVarCommon(), type);
        }
        else
        {
            structOp = RetypeStructIndir(structOp, type);
        }

        asg->SetOp(isStructDef ? 0 : 1, structOp);
        asg->SetType(type);
        asg->SetSideEffects(structOp->GetSideEffects() | scalarOp->GetSideEffects());

        INDEBUG(m_stmtModified = true;)
    }

    void RetypeMergedReturnStructAssignment(GenTreeUnOp* ret, LclVarDsc* lcl)
    {
        assert(ret->OperIs(GT_RETURN) && ret->TypeIs(TYP_STRUCT) && IsMergedReturnAssignment(ret));
        assert(lcl->IsPromoted() && (lcl->GetPromotedFieldCount() == 1));

        GenTree*  val  = ret->GetOp(0);
        var_types type = m_compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(0))->GetType();

        if (GenTreeCall* call = val->IsCall())
        {
            val = RetypeStructCall(call, type);
        }
        else if (val->IsIntegralConst(0))
        {
            val = RetypeStructZeroInit(val, type);
        }
        else if (val->OperIs(GT_LCL_FLD, GT_LCL_VAR))
        {
            val = RetypeStructLocal(val->AsLclVarCommon(), type);
        }
        else
        {
            val = RetypeStructIndir(val, type);
        }

        ret->SetOp(0, val);
        ret->SetType(type);
        ret->SetSideEffects(val->GetSideEffects());

        INDEBUG(m_stmtModified = true;)
    }

    GenTree* RetypeStructZeroInit(GenTree* zero, var_types type)
    {
        assert(zero->IsIntegralConst(0));
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
            var_types retRegType = varActualType(retDesc.GetRegType(0));
            call->SetType(retRegType);

            if (varTypeUsesFloatReg(retRegType) != varTypeUsesFloatReg(type))
            {
                return NewBitCastNode(type, call);
            }

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
        return call;
    }

    GenTree* RetypeStructLocal(GenTreeLclVarCommon* structLcl, var_types type)
    {
        assert(structLcl->TypeIs(TYP_STRUCT));
        assert(type != TYP_STRUCT);

        if (GenTreeLclFld* lclFld = structLcl->IsLclFld())
        {
            lclFld->SetType(type);
            lclFld->SetFieldSeq(ExtendFieldSequence(lclFld->GetFieldSeq(), type));
        }
        else
        {
            assert(structLcl->OperIs(GT_LCL_VAR));

            LclVarDsc*    lcl      = m_compiler->lvaGetDesc(structLcl);
            FieldSeqNode* fieldSeq = GetFieldSequence(lcl->GetLayout()->GetClassHandle(), type);

            structLcl->ChangeToLclFld(type, structLcl->GetLclNum(), 0, fieldSeq);
        }

        return structLcl;
    }

    GenTree* RetypeStructIndir(GenTree* structIndir, var_types type)
    {
        assert(structIndir->TypeIs(TYP_STRUCT));
        assert(type != TYP_STRUCT);

        GenTree*      addr     = nullptr;
        FieldSeqNode* fieldSeq = FieldSeqNode::NotAField();

        if (GenTreeField* field = structIndir->IsField())
        {
            addr     = m_compiler->gtNewAddrNode(field, varTypePointerAdd(field->GetAddr()->GetType()));
            fieldSeq = GetFieldSequence(GetStructFieldType(field), type);
        }
        else if (GenTreeIndex* index = structIndir->IsIndex())
        {
            addr     = m_compiler->gtNewAddrNode(index, TYP_BYREF);
            fieldSeq = GetFieldSequence(index->GetLayout()->GetClassHandle(), type);
        }
        else
        {
            addr = structIndir->AsObj()->GetAddr();

            if (addr->OperIs(GT_ADDR) && addr->AsUnOp()->GetOp(0)->TypeIs(TYP_STRUCT))
            {
                if (GenTreeField* field = addr->AsUnOp()->GetOp(0)->IsField())
                {
                    fieldSeq = GetFieldSequence(GetStructFieldType(field), type);
                }
                else if (GenTreeIndex* index = addr->AsUnOp()->GetOp(0)->IsIndex())
                {
                    fieldSeq = GetFieldSequence(index->GetLayout()->GetClassHandle(), type);
                }
            }
        }

        if (fieldSeq->IsField())
        {
            GenTree* field = m_compiler->gtNewFieldRef(type, fieldSeq->GetFieldHandle(), addr, 0);

            for (fieldSeq = fieldSeq->GetNext(); fieldSeq != nullptr; fieldSeq = fieldSeq->GetNext())
            {
                addr  = m_compiler->gtNewAddrNode(field, addr->GetType());
                field = m_compiler->gtNewFieldRef(type, fieldSeq->GetFieldHandle(), addr, 0);
            }

            return field;
        }

        if (!structIndir->IsObj())
        {
            return m_compiler->gtNewOperNode(GT_IND, type, addr);
        }

        structIndir->ChangeOper(GT_IND);
        structIndir->SetType(type);
        return structIndir;
    }

    bool IsMergedReturnAssignment(GenTreeUnOp* ret)
    {
        assert(ret->OperIs(GT_RETURN));

        return (m_compiler->genReturnLocal != BAD_VAR_NUM) && ((ret->gtFlags & GTF_RET_MERGED) == 0);
    }

    ClassLayout* GetStructIndirLayout(GenTree* indir)
    {
        if (GenTreeField* field = indir->IsField())
        {
            return m_compiler->typGetObjLayout(GetStructFieldType(field));
        }

        return indir->AsObj()->GetLayout();
    }

    CORINFO_CLASS_HANDLE GetStructFieldType(GenTreeField* field)
    {
        assert(varTypeIsStruct(field->GetType()));

        return GetStructFieldType(field->GetFieldHandle());
    }

    CORINFO_CLASS_HANDLE GetStructFieldType(CORINFO_FIELD_HANDLE fieldHandle)
    {
        CORINFO_CLASS_HANDLE fc;
        var_types            ft = CorTypeToVarType(m_compiler->info.compCompHnd->getFieldType(fieldHandle, &fc));
        assert((fc != nullptr) || (ft != TYP_STRUCT));

        return ft == TYP_STRUCT ? fc : nullptr;
    }

    FieldSeqNode* GetFieldSequence(CORINFO_CLASS_HANDLE classHandle, var_types fieldType)
    {
        assert(fieldType != TYP_STRUCT);

        ICorJitInfo*         vm       = m_compiler->info.compCompHnd;
        FieldSeqNode*        fieldSeq = nullptr;
        CORINFO_FIELD_HANDLE fieldHandle;

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

    FieldSeqNode* ExtendFieldSequence(FieldSeqNode* fieldSeq, var_types fieldType)
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

        return m_compiler->GetFieldSeqStore()->Append(fieldSeq, GetFieldSequence(classHandle, fieldType));
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
        if ((user != nullptr) && user->OperIs(GT_RETURN, GT_CALL))
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
        assert(varTypeSize(type) <= REGSIZE_BYTES);

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

    GenTreeHWIntrinsic* NewExtractElement(var_types eltType, GenTree* vec, var_types vecType, unsigned index)
    {
        return m_compiler->gtNewSimdGetElementNode(vecType, eltType, vec, m_compiler->gtNewIconNode(index));
    }
#endif
};

bool StructPromotionHelper::TryPromoteStructLocal(unsigned lclNum)
{
    if (CanPromoteStructLocal(lclNum) && ShouldPromoteStructLocal(lclNum))
    {
        PromoteStructLocal(lclNum);
        return true;
    }

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
        // (especially considering that the most commun occurence of such structs
        // are "fixed buffer" structs created by the C# compiler and those must
        // not be promoted).
        return false;
    }

    info.canPromoteStructType = true;
    return true;
}

bool StructPromotionHelper::CanPromoteStructLocal(unsigned lclNum)
{
    LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

    assert(varTypeIsStruct(lcl->GetType()));
    assert(!lcl->IsPromoted());

    // If this local is used by SIMD intrinsics, then we don't want to promote it.
    // Note, however, that Vector2/3/4 local that are NOT used by SIMD intrinsics
    // may be profitably promoted.
    if (lcl->lvIsUsedInSIMDIntrinsic())
    {
        JITDUMP("  promotion of V%02u is disabled due to SIMD intrinsic uses\n", lclNum);
        return false;
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
        JITDUMP("  promotion of param V%02u is disabled due to GS stack layout reordering\n", lclNum);
        return false;
    }

    if (!compiler->lvaEnregMultiRegVars)
    {
        if (lcl->lvIsMultiRegArg)
        {
            JITDUMP("  promotion of V%02u is disabled because it is a multi-reg arg\n", lclNum);
            return false;
        }

        if (lcl->lvIsMultiRegRet)
        {
            JITDUMP("  promotion of V%02u is disabled because it is a multi-reg return\n", lclNum);
            return false;
        }
    }

    // TODO-CQ: enable promotion for OSR locals
    if (compiler->lvaIsOSRLocal(lclNum))
    {
        JITDUMP("  promotion of V%02u is disabled because it is an OSR local\n", lclNum);
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
        JITDUMP("Not promoting param V%02u: struct has a single float/double field.\n", lclNum, info.fieldCount);
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
    for (unsigned i = 0; i < info.fieldCount; i++)
    {
        const FieldInfo& field = info.fields[i];

        // Non-HFA structs are always passed in general purpose registers.
        // If there are any floating point fields, don't promote for now.
        // TODO-1stClassStructs: add support in Lowering and prolog generation
        // to enable promoting these types.
        if (lcl->IsParam() && !lcl->lvIsHfa() && varTypeUsesFloatReg(field.type))
        {
            return false;
        }

#ifdef FEATURE_SIMD
        // If we have a register-passed struct with mixed non-opaque SIMD types (i.e. with defined fields)
        // and non-SIMD types, we don't currently handle that case in the prolog, so we can't promote.
        if ((info.fieldCount > 1) && varTypeIsStruct(field.type) && !field.layout->IsOpaqueVector())
        {
            return false;
        }
#endif
    }
#elif defined(UNIX_AMD64_ABI)
    // Only promote if the field types match the registers, unless we have a single SIMD field
    // that we can handle in prolog.

    if ((info.fieldCount == 1) && varTypeIsSIMD(info.fields[0].type))
    {
        return true;
    }

    ClassLayout* layout = lcl->GetLayout();
    layout->EnsureSysVAmd64AbiInfo(compiler);

    if (info.fieldCount != layout->GetSysVAmd64AbiRegCount())
    {
        return false;
    }

    SortFields();

    for (unsigned i = 0; i < info.fieldCount; i++)
    {
        const FieldInfo& field = info.fields[i];

        // We don't currently support passing SIMD types in registers.
        if (varTypeIsSIMD(field.type))
        {
            return false;
        }

        if (varTypeUsesFloatReg(field.type) != varTypeUsesFloatReg(layout->GetSysVAmd64AbiRegType(i)))
        {
            return false;
        }
    }
#endif // UNIX_AMD64_ABI

    return true;
}

bool StructPromotionHelper::ShouldPromoteStructLocal(unsigned lclNum)
{
    LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

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
    // struct copying is done by field by field assignment instead of a more efficient
    // rep.stos or xmm reg based copy.

    // TODO: If the lvRefCnt is zero and we have a struct promoted parameter we can end up
    // with an extra store of the the incoming register into the stack frame slot.
    // In that case, we would like to avoid promortion.
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
        JITDUMP("Not promoting V%02u: type = %s, #fields = %d, fieldAccessed = %d.\n", lclNum,
                varTypeName(lcl->GetType()), info.fieldCount, lcl->lvFieldAccessed);
        return false;
    }

    if (lcl->lvIsMultiRegRet && info.containsHoles && info.customLayout)
    {
        JITDUMP("Not promoting multi-reg returned V%02u with holes.\n", lclNum);
        return false;
    }

    if (lcl->IsParam() && !lcl->IsImplicitByRefParam() && !lcl->lvIsHfa())
    {
#if FEATURE_MULTIREG_STRUCT_PROMOTE
        if (compiler->lvaIsMultiRegStructParam(lcl))
        {
            if ((info.fieldCount != 2) && ((info.fieldCount != 1) || !varTypeIsSIMD(info.fields[0].type)))
            {
                JITDUMP("Not promoting multireg param V%02u, #fields != 2 and it's not SIMD.\n", lclNum);
                return false;
            }
        }
        else
#endif // !FEATURE_MULTIREG_STRUCT_PROMOTE

            // TODO-PERF - Implement struct promotion for incoming single-register structs.
            //             Also the implementation of jmp uses the 4 byte move to store
            //             byte parameters to the stack, so that if we have a byte field
            //             with something else occupying the same 4-byte slot, it will
            //             overwrite other fields.
            if (info.fieldCount != 1)
        {
            JITDUMP("Not promoting param V%02u, #fields = %u.\n", lclNum, info.fieldCount);
            return false;
        }
    }

    if ((lclNum == compiler->genReturnLocal) && (info.fieldCount > 1))
    {
        // TODO-1stClassStructs: a temporary solution to keep diffs small, it will be fixed later.
        return false;
    }

#ifdef DEBUG
    if (compiler->compPromoteFewerStructs(lclNum))
    {
        JITDUMP("Not promoting promotable V%02u, because of STRESS_PROMOTE_FEWER_STRUCTS\n", lclNum);
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

void StructPromotionHelper::PromoteStructLocal(unsigned lclNum)
{
    LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

    // We should never see a reg-sized non-field-addressed struct here.
    assert(!lcl->lvRegStruct);

    assert(lcl->GetLayout()->GetClassHandle() == info.typeHandle);
    assert(info.canPromoteStructType);

    lcl->lvFieldCnt      = info.fieldCount;
    lcl->lvFieldLclStart = compiler->lvaCount;
    lcl->lvPromoted      = true;
    lcl->lvContainsHoles = info.containsHoles;
    lcl->lvCustomLayout  = info.customLayout;

    INDEBUG(lcl->lvKeepType = 1;)

    JITDUMP("\nPromoting struct local V%02u (%s):", lclNum, lcl->GetLayout()->GetClassName());

    SortFields();

    for (unsigned index = 0; index < info.fieldCount; ++index)
    {
        const FieldInfo& field = info.fields[index];
        assert((index == 0) || (field.offset > info.fields[index - 1].offset));

        if (varTypeUsesFloatReg(field.type))
        {
            compiler->compFloatingPointUsed = true;
        }
        else if (field.type == TYP_LONG)
        {
            compiler->compLongUsed = true;
        }

        FieldSeqNode* fieldSeq = nullptr;

        for (unsigned i = 0; i < field.fieldSeqLength; i++)
        {
            FieldSeqNode* fieldSeqNode = compiler->GetFieldSeqStore()->CreateSingleton(field.fieldSeq[i]);
            fieldSeq                   = compiler->GetFieldSeqStore()->Append(fieldSeq, fieldSeqNode);
        }

        unsigned fieldLclNum = compiler->lvaGrabTemp(false DEBUGARG("promoted struct field"));
        // lvaGrabTemp can reallocate the lvaTable, so refresh the cached lcl for lclNum.
        lcl = compiler->lvaGetDesc(lclNum);

        LclVarDsc* fieldLcl = compiler->lvaGetDesc(fieldLclNum);
        fieldLcl->MakePromotedStructField(lclNum, field.offset, fieldSeq);

        if (varTypeIsSIMD(field.type))
        {
            compiler->lvaSetStruct(fieldLclNum, field.layout, false);
            // We will not recursively promote this, so mark it as 'lvRegStruct' (note that we wouldn't
            // be promoting this if we didn't think it could be enregistered.
            fieldLcl->lvRegStruct = true;
        }
        else
        {
            fieldLcl->SetType(field.type);
        }

        fieldLcl->lvIsParam = lcl->IsParam();

        if (!lcl->IsRegParam())
        {
            continue;
        }

        fieldLcl->lvIsRegArg = true;

#if !FEATURE_MULTIREG_ARGS
        fieldLcl->SetArgReg(lcl->GetArgReg());
#else
        if (lcl->IsImplicitByRefParam())
        {
            fieldLcl->SetArgReg(lcl->GetArgReg());
            continue;
        }

#ifdef UNIX_AMD64_ABI
        if (varTypeIsSIMD(fieldLcl->GetType()) && (lcl->GetPromotedFieldCount() == 1))
        {
            fieldLcl->SetArgReg(lcl->GetArgReg());
            fieldLcl->SetOtherArgReg(lcl->GetOtherArgReg());
            continue;
        }
#endif

        if (lcl->lvIsHfa())
        {
            // We've sorted the field by offset so for HFA's we should have a 1:1 mapping
            // between fields and registers. Also, ensure that we didn't promote a HFA
            // with holes. Is that even a thing? The VM seems to think that it is...
            assert(field.offset == index * varTypeSize(lcl->GetLayout()->GetHfaElementType()));

            unsigned regIndex = index;

#ifdef TARGET_ARM
            if (lcl->GetLayout()->GetHfaElementType() == TYP_DOUBLE)
            {
                // On ARM we count FLOAT rather than DOUBLE registers.
                regIndex *= 2;
            }
#endif

            fieldLcl->SetArgReg(static_cast<regNumber>(lcl->GetArgReg() + regIndex));
            continue;
        }

        // TODO-ARMARCH: Need to determine if/how to handle split args.

        if (index == 0)
        {
            fieldLcl->SetArgReg(lcl->GetArgReg());
        }
        else
        {
            assert(index == 1);
            fieldLcl->SetArgReg(lcl->GetOtherArgReg());
        }
#endif // FEATURE_MULTIREG_ARGS
    }
}

void Compiler::fgPromoteStructs()
{
    JITDUMP("*************** In fgPromoteStructs()\n");

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
        printf("\nlvaTable before fgPromoteStructs\n");
        lvaTableDump();
    }
#endif

    StructPromotionHelper helper(this);

    for (unsigned lclNum = 0, lclCount = lvaCount; lclNum < lclCount; lclNum++)
    {
        if (lvaHaveManyLocals())
        {
            JITDUMP("Stopped promoting struct fields, due to too many locals.\n");
            break;
        }

        LclVarDsc* lcl = lvaGetDesc(lclNum);

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

        if (varTypeIsSIMD(lcl->GetType()) && (lcl->lvIsUsedInSIMDIntrinsic() || lcl->GetLayout()->IsOpaqueVector()))
        {
            // If we have marked this as lvUsedInSIMDIntrinsic, then we do not want to promote
            // its fields. Instead, we will attempt to enregister the entire struct.
            lcl->lvRegStruct = true;
            continue;
        }

        if (!helper.TryPromoteStructLocal(lclNum))
        {
            // If we don't promote then lvIsMultiRegRet is meaningless.
            lcl->lvIsMultiRegRet = false;

            if (varTypeIsSIMD(lcl->GetType()) && !lcl->lvFieldAccessed)
            {
                // Even if we have not used this in a SIMD intrinsic, if it is not being promoted,
                // we will treat it as a reg struct.
                lcl->lvRegStruct = true;
            }
        }
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("\nlvaTable after fgPromoteStructs\n");
        lvaTableDump();
    }
#endif
}

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
