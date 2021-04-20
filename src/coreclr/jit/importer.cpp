// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                           Importer                                        XX
XX                                                                           XX
XX   Imports the given method and converts it to semantic trees              XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

void Compiler::impInit()
{
    impStmtList = impLastStmt = nullptr;
#ifdef DEBUG
    impInlinedCodeSize = 0;
#endif // DEBUG
}

/*****************************************************************************
 *
 *  Pushes the given tree on the stack.
 */

void Compiler::impPushOnStack(GenTree* tree, typeInfo ti)
{
    /* Check for overflow. If inlining, we may be using a bigger stack */

    if ((verCurrentState.esStackDepth >= info.compMaxStack) &&
        (verCurrentState.esStackDepth >= impStkSize || ((compCurBB->bbFlags & BBF_IMPORTED) == 0)))
    {
        BADCODE("stack overflow");
    }

    // If we are pushing a struct, make certain we know the precise type!
    if (tree->TypeIs(TYP_STRUCT))
    {
        assert(ti.GetClassHandleForValueClass() != NO_CLASS_HANDLE);
    }

    verCurrentState.esStack[verCurrentState.esStackDepth].seTypeInfo = ti;
    verCurrentState.esStack[verCurrentState.esStackDepth++].val      = tree;

    if ((tree->gtType == TYP_LONG) && (compLongUsed == false))
    {
        compLongUsed = true;
    }
    else if (((tree->gtType == TYP_FLOAT) || (tree->gtType == TYP_DOUBLE)) && (compFloatingPointUsed == false))
    {
        compFloatingPointUsed = true;
    }
}

typeInfo Compiler::impMakeTypeInfo(CorInfoType type, CORINFO_CLASS_HANDLE classHandle)
{
    assert((type != CORINFO_TYPE_UNDEF) && (type != CORINFO_TYPE_VOID) && (type != CORINFO_TYPE_VAR));
    assert(classHandle != NO_CLASS_HANDLE);

    switch (type)
    {
        case CORINFO_TYPE_CLASS:
        case CORINFO_TYPE_STRING:
            assert(!info.compCompHnd->isValueClass(classHandle));
            return typeInfo(TI_REF, classHandle);

        case CORINFO_TYPE_VALUECLASS:
        case CORINFO_TYPE_REFANY:
            assert(info.compCompHnd->isValueClass(classHandle));
            return typeInfo(TI_STRUCT, classHandle);

        case CORINFO_TYPE_BYREF:
        case CORINFO_TYPE_PTR:
            // Ignore the handle for byrefs and pointers. Currently it's not needed and
            // for byrefs we'd need to call getChildType to get the correct handle. For
            // pointers the class handle may be IntPtr/UIntPtr, not only that's totally
            // useless but building a TI_STRUCT typeInfo for it would make it look like
            // a "normed type". The (x86) VM does treat a struct containing a single
            // pointer typed field as a "normed type" but it also lies about the field
            // type - the "normed type" of such a struct is CORINFO_TYPE_NATIVEUINT
            // rather than CORINFO_TYPE_PTR.
            return typeInfo();

        default:
            assert(info.compCompHnd->isValueClass(classHandle));

            if (info.compCompHnd->getTypeForPrimitiveValueClass(classHandle) == CORINFO_TYPE_UNDEF)
            {
                // This is a "normed type", a struct type with a single field that the VM claims
                // to be a primitive type. We need to record the fact that it is really a struct
                // so LDFLD import doesn't confuse it with the unmanaged pointer which too is a
                // primitive type (INT or LONG).
                return typeInfo(TI_STRUCT, classHandle);
            }

            return typeInfo();
    }
}

// helper function that will tell us if the IL instruction at the addr passed
// by param consumes an address at the top of the stack. We use it to save
// us lvAddrTaken
bool Compiler::impILConsumesAddr(const BYTE* codeAddr)
{
    assert(!compIsForInlining());

    OPCODE opcode;

    opcode = (OPCODE)getU1LittleEndian(codeAddr);

    switch (opcode)
    {
        // case CEE_LDFLDA: We're taking this one out as if you have a sequence
        // like
        //
        //          ldloca.0
        //          ldflda whatever
        //
        // of a primitivelike struct, you end up after morphing with addr of a local
        // that's not marked as addrtaken, which is wrong. Also ldflda is usually used
        // for structs that contain other structs, which isnt a case we handle very
        // well now for other reasons.

        case CEE_LDFLD:
        {
            // We won't collapse small fields. This is probably not the right place to have this
            // check, but we're only using the function for this purpose, and is easy to factor
            // out if we need to do so.

            CORINFO_RESOLVED_TOKEN resolvedToken;
            impResolveToken(codeAddr + sizeof(__int8), &resolvedToken, CORINFO_TOKENKIND_Field);

            var_types lclTyp = JITtype2varType(info.compCompHnd->getFieldType(resolvedToken.hField));

            // Preserve 'small' int types
            if (!varTypeIsSmall(lclTyp))
            {
                lclTyp = genActualType(lclTyp);
            }

            if (varTypeIsSmall(lclTyp))
            {
                return false;
            }

            return true;
        }
        default:
            break;
    }

    return false;
}

void Compiler::impResolveToken(const BYTE* addr, CORINFO_RESOLVED_TOKEN* pResolvedToken, CorInfoTokenKind kind)
{
    pResolvedToken->tokenContext = impTokenLookupContextHandle;
    pResolvedToken->tokenScope   = info.compScopeHnd;
    pResolvedToken->token        = getU4LittleEndian(addr);
    pResolvedToken->tokenType    = kind;

    info.compCompHnd->resolveToken(pResolvedToken);
}

/*****************************************************************************
 *
 *  Pop one tree from the stack.
 */

StackEntry Compiler::impPopStack()
{
    if (verCurrentState.esStackDepth == 0)
    {
        BADCODE("stack underflow");
    }

    return verCurrentState.esStack[--verCurrentState.esStackDepth];
}

GenTree* Compiler::impPopStackCoerceArg(var_types signatureType)
{
    // Not currently supported for structs (it would need to check the struct handle).
    assert(!varTypeIsStruct(signatureType));
    // Not currently supported for small int (it's not clear if truncation has to be done or not).
    assert(!varTypeIsSmall(signatureType));

    // TODO-MIKE-Cleanup: impPopCallArgs has some similar logic...

    if (verCurrentState.esStackDepth == 0)
    {
        BADCODE("stack underflow");
    }

    GenTree* tree = verCurrentState.esStack[--verCurrentState.esStackDepth].val;

    var_types stackType = varActualType(tree->GetType());

    if (signatureType != stackType)
    {
        if ((varTypeIsFloating(signatureType) && varTypeIsFloating(stackType))
#ifdef TARGET_64BIT
            // TODO-MIKE-Review: This should only be done when the stack type is 'native int'
            // but we don't track this exact type so we have to do it whenever the stack type
            // is LONG, which is a relaxation of the ECMA III.1.6 Implicit argument coercion.
            || ((signatureType == TYP_INT) && (stackType == TYP_LONG))
#endif
                )
        {
            tree = gtNewCastNode(signatureType, tree, false, signatureType);
        }
        else if ((signatureType == TYP_BYREF) && (stackType == TYP_I_IMPL))
        {
            // TODO-MIKE-Review: ECMA III.1.6 "Implicit argument coercion" states that in this
            // case GC tracking should start. This could probably be achieved by inserting a
            // BITCAST to TYP_BYREF but it's not clear if this is really needed.
        }
        else
        {
            BADCODE("incompatible stack type");
        }
    }

    return tree;
}

#ifdef FEATURE_SIMD
GenTree* Compiler::impSIMDPopStackAddr(var_types type)
{
    assert(varTypeIsSIMD(type));

    GenTree* addr = impPopStack().val;

    if (!addr->TypeIs(TYP_BYREF, TYP_I_IMPL))
    {
        BADCODE("incompatible stack type");
    }

    if (addr->OperIs(GT_ADDR))
    {
        GenTree* location = addr->AsUnOp()->GetOp(0);

        if (location->GetType() == type)
        {
            return location;
        }
    }

    return gtNewOperNode(GT_IND, type, addr);
}
#endif

/*****************************************************************************
 *
 *  Peep at n'th (0-based) tree on the top of the stack.
 */

StackEntry& Compiler::impStackTop(unsigned n)
{
    if (verCurrentState.esStackDepth <= n)
    {
        BADCODE("stack underflow");
    }

    return verCurrentState.esStack[verCurrentState.esStackDepth - n - 1];
}

unsigned Compiler::impStackHeight()
{
    return verCurrentState.esStackDepth;
}

//------------------------------------------------------------------------
// impBeginTreeList: Get the tree list started for a new basic block.
//
inline void Compiler::impBeginTreeList()
{
    assert(impStmtList == nullptr && impLastStmt == nullptr);
}

/*****************************************************************************
 *
 *  Store the given start and end stmt in the given basic block. This is
 *  mostly called by impEndTreeList(BasicBlock *block). It is called
 *  directly only for handling CEE_LEAVEs out of finally-protected try's.
 */

inline void Compiler::impEndTreeList(BasicBlock* block, Statement* firstStmt, Statement* lastStmt)
{
    if (firstStmt != nullptr)
    {
        // Make the list circular, so that we can easily walk it backwards
        firstStmt->SetPrevStmt(lastStmt);
    }

    block->bbStmtList = firstStmt;

    // The block should not already be marked as imported
    assert((block->bbFlags & BBF_IMPORTED) == 0);

    block->bbFlags |= BBF_IMPORTED;
}

//------------------------------------------------------------------------
// impEndTreeList: Store the current tree list in the given basic block.
//
// Arguments:
//    block - the basic block to store into.
//
inline void Compiler::impEndTreeList(BasicBlock* block)
{
    impEndTreeList(block, impStmtList, impLastStmt);
    impStmtList = nullptr;
    impLastStmt = nullptr;

#ifdef DEBUG
    if (impLastILoffsStmt != nullptr)
    {
        impLastILoffsStmt->SetLastILOffset(compIsForInlining() ? BAD_IL_OFFSET : impCurOpcOffs);
        impLastILoffsStmt = nullptr;
    }
#endif
}

/*****************************************************************************
 *
 *  Check that storing the given tree doesnt mess up the semantic order. Note
 *  that this has only limited value as we can only check [0..chkLevel).
 */

inline void Compiler::impAppendStmtCheck(Statement* stmt, unsigned chkLevel)
{
#ifndef DEBUG
    return;
#else

    if (chkLevel == (unsigned)CHECK_SPILL_ALL)
    {
        chkLevel = verCurrentState.esStackDepth;
    }

    if (verCurrentState.esStackDepth == 0 || chkLevel == 0 || chkLevel == (unsigned)CHECK_SPILL_NONE)
    {
        return;
    }

    GenTree* tree = stmt->GetRootNode();

    // Calls can only be appended if there are no GTF_GLOB_EFFECT on the stack

    if (tree->gtFlags & GTF_CALL)
    {
        for (unsigned level = 0; level < chkLevel; level++)
        {
            GenTree* tree = verCurrentState.esStack[level].val;
            assert(((tree->gtFlags & GTF_GLOB_EFFECT) == 0) || impIsAddressInLocal(tree));
        }
    }

    if (tree->gtOper == GT_ASG)
    {
        // For an assignment to a local variable, all references of that
        // variable have to be spilled. If it is aliased, all calls and
        // indirect accesses have to be spilled

        if (tree->AsOp()->gtOp1->gtOper == GT_LCL_VAR)
        {
            unsigned lclNum = tree->AsOp()->gtOp1->AsLclVarCommon()->GetLclNum();
            for (unsigned level = 0; level < chkLevel; level++)
            {
                assert(!gtHasRef(verCurrentState.esStack[level].val, lclNum));
                assert(!lvaTable[lclNum].lvAddrExposed ||
                       (verCurrentState.esStack[level].val->gtFlags & GTF_SIDE_EFFECT) == 0);
            }
        }

        // If the access may be to global memory, all side effects have to be spilled.

        else if (tree->AsOp()->gtOp1->gtFlags & GTF_GLOB_REF)
        {
            for (unsigned level = 0; level < chkLevel; level++)
            {
                assert((verCurrentState.esStack[level].val->gtFlags & GTF_GLOB_REF) == 0);
            }
        }
    }
#endif
}

/*****************************************************************************
 *
 *  Append the given statement to the current block's tree list.
 *  [0..chkLevel) is the portion of the stack which we will check for
 *    interference with stmt and spill if needed.
 */

inline void Compiler::impAppendStmt(Statement* stmt, unsigned chkLevel)
{
    if (chkLevel == (unsigned)CHECK_SPILL_ALL)
    {
        chkLevel = verCurrentState.esStackDepth;
    }

    if ((chkLevel != 0) && (chkLevel != (unsigned)CHECK_SPILL_NONE))
    {
        assert(chkLevel <= verCurrentState.esStackDepth);

        /* If the statement being appended has any side-effects, check the stack
           to see if anything needs to be spilled to preserve correct ordering. */

        GenTree* expr  = stmt->GetRootNode();
        unsigned flags = expr->gtFlags & GTF_GLOB_EFFECT;

        // Assignment to (unaliased) locals don't count as a side-effect as
        // we handle them specially using impSpillLclRefs(). Temp locals should
        // be fine too.

        if ((expr->gtOper == GT_ASG) && (expr->AsOp()->gtOp1->gtOper == GT_LCL_VAR) &&
            ((expr->AsOp()->gtOp1->gtFlags & GTF_GLOB_REF) == 0) && !gtHasAddressTakenLocals(expr->AsOp()->gtOp2))
        {
            unsigned op2Flags = expr->AsOp()->gtOp2->gtFlags & GTF_GLOB_EFFECT;
            assert(flags == (op2Flags | GTF_ASG));
            flags = op2Flags;
        }

        if (flags != 0)
        {
            bool spillGlobEffects = false;

            if ((flags & GTF_CALL) != 0)
            {
                // If there is a call, we have to spill global refs
                spillGlobEffects = true;
            }
            else if (!expr->OperIs(GT_ASG))
            {
                if ((flags & GTF_ASG) != 0)
                {
                    // The expression is not an assignment node but it has an assignment side effect, it
                    // must be an atomic op, HW intrinsic or some other kind of node that stores to memory.
                    // Since we don't know what it assigns to, we need to spill global refs.
                    spillGlobEffects = true;
                }
            }
            else
            {
                GenTree* lhs = expr->gtGetOp1();
                GenTree* rhs = expr->gtGetOp2();

                if (((rhs->gtFlags | lhs->gtFlags) & GTF_ASG) != 0)
                {
                    // Either side of the assignment node has an assignment side effect.
                    // Since we don't know what it assigns to, we need to spill global refs.
                    spillGlobEffects = true;
                }
                else if ((lhs->gtFlags & GTF_GLOB_REF) != 0)
                {
                    spillGlobEffects = true;
                }
            }

            impSpillSideEffects(spillGlobEffects, chkLevel DEBUGARG("impAppendStmt"));
        }
        else
        {
            impSpillSpecialSideEff();
        }
    }

    impAppendStmtCheck(stmt, chkLevel);

    impAppendStmt(stmt);

#ifdef FEATURE_SIMD
    if (opts.OptimizationEnabled() && featureSIMD)
    {
        m_impSIMDCoalescingBuffer.Mark(this, stmt);
    }
#endif

    /* Once we set impCurStmtOffs in an appended tree, we are ready to
       report the following offsets. So reset impCurStmtOffs */

    if (impLastStmt->GetILOffsetX() == impCurStmtOffs)
    {
        impCurStmtOffsSet(BAD_IL_OFFSET);
    }

#ifdef DEBUG
    if (impLastILoffsStmt == nullptr)
    {
        impLastILoffsStmt = stmt;
    }

    if (verbose)
    {
        printf("\n\n");
        gtDispStmt(stmt);
    }
#endif
}

//------------------------------------------------------------------------
// impAppendStmt: Add the statement to the current stmts list.
//
// Arguments:
//    stmt - the statement to add.
//
inline void Compiler::impAppendStmt(Statement* stmt)
{
    if (impStmtList == nullptr)
    {
        // The stmt is the first in the list.
        impStmtList = stmt;
    }
    else
    {
        // Append the expression statement to the existing list.
        impLastStmt->SetNextStmt(stmt);
        stmt->SetPrevStmt(impLastStmt);
    }
    impLastStmt = stmt;
}

//------------------------------------------------------------------------
// impExtractLastStmt: Extract the last statement from the current stmts list.
//
// Return Value:
//    The extracted statement.
//
// Notes:
//    It assumes that the stmt will be reinserted later.
//
Statement* Compiler::impExtractLastStmt()
{
    assert(impLastStmt != nullptr);

    Statement* stmt = impLastStmt;
    impLastStmt     = impLastStmt->GetPrevStmt();
    if (impLastStmt == nullptr)
    {
        impStmtList = nullptr;
    }
    return stmt;
}

//-------------------------------------------------------------------------
// impInsertStmtBefore: Insert the given "stmt" before "stmtBefore".
//
// Arguments:
//    stmt       - a statement to insert;
//    stmtBefore - an insertion point to insert "stmt" before.
//
inline void Compiler::impInsertStmtBefore(Statement* stmt, Statement* stmtBefore)
{
    assert(stmt != nullptr);
    assert(stmtBefore != nullptr);

    if (stmtBefore == impStmtList)
    {
        impStmtList = stmt;
    }
    else
    {
        Statement* stmtPrev = stmtBefore->GetPrevStmt();
        stmt->SetPrevStmt(stmtPrev);
        stmtPrev->SetNextStmt(stmt);
    }
    stmt->SetNextStmt(stmtBefore);
    stmtBefore->SetPrevStmt(stmt);
}

/*****************************************************************************
 *
 *  Append the given expression tree to the current block's tree list.
 *  Return the newly created statement.
 */

Statement* Compiler::impAppendTree(GenTree* tree, unsigned chkLevel, IL_OFFSETX offset)
{
    assert(tree);

    /* Allocate an 'expression statement' node */

    Statement* stmt = gtNewStmt(tree, offset);

    /* Append the statement to the current block's stmt list */

    impAppendStmt(stmt, chkLevel);

    return stmt;
}

/*****************************************************************************
 *
 *  Insert the given expression tree before "stmtBefore"
 */

void Compiler::impInsertTreeBefore(GenTree* tree, IL_OFFSETX offset, Statement* stmtBefore)
{
    /* Allocate an 'expression statement' node */

    Statement* stmt = gtNewStmt(tree, offset);

    /* Append the statement to the current block's stmt list */

    impInsertStmtBefore(stmt, stmtBefore);
}

/*****************************************************************************
 *
 *  Append an assignment of the given value to a temp to the current tree list.
 *  curLevel is the stack level for which the spill to the temp is being done.
 */

void Compiler::impAssignTempGen(unsigned tmp, GenTree* val, unsigned curLevel)
{
    GenTree* asg = gtNewTempAssign(tmp, val);

    if (!asg->IsNothingNode())
    {
        impAppendTree(asg, curLevel, impCurStmtOffs);
    }
}

void Compiler::impAssignTempGen(unsigned tmpNum, GenTree* val, ClassLayout* layout, unsigned curLevel)
{
    assert((layout == nullptr) || !layout->IsBlockLayout());

    impAssignTempGen(tmpNum, val, layout == nullptr ? NO_CLASS_HANDLE : layout->GetClassHandle(), curLevel);
}

void Compiler::impAssignTempGen(unsigned tmpNum, GenTree* val, CORINFO_CLASS_HANDLE structType, unsigned curLevel)
{
    GenTree* asg;

    assert((val->GetType() != TYP_STRUCT) || (structType != NO_CLASS_HANDLE));

    if (varTypeIsStruct(val->GetType()) && (structType != NO_CLASS_HANDLE))
    {
        lvaSetStruct(tmpNum, structType, false);

        var_types lclType = lvaGetDesc(tmpNum)->GetType();

        // Now, set the type of the struct value. Note that lvaSetStruct may modify the type
        // of the lclVar to a specialized type (e.g. TYP_SIMD), based on the handle (structType)
        // that has been passed in for the value being assigned to the temp, in which case we
        // need to set 'val' to that same type.
        // Note also that if we always normalized the types of any node that might be a struct
        // type, this would not be necessary - but that requires additional JIT/EE interface
        // calls that may not actually be required - e.g. if we only access a field of a struct.

        GenTree* dst = gtNewLclvNode(tmpNum, lclType);
        asg          = impAssignStruct(dst, val, structType, curLevel);
    }
    else
    {
        asg = gtNewTempAssign(tmpNum, val);
    }

    if (!asg->IsNothingNode())
    {
        impAppendTree(asg, curLevel, impCurStmtOffs);
    }
}

/*****************************************************************************
 *
 *  Pop the given number of values from the stack and return a list node with
 *  their values.
 *  The 'prefixTree' argument may optionally contain an argument
 *  list that is prepended to the list returned from this function.
 *
 *  The notion of prepended is a bit misleading in that the list is backwards
 *  from the way I would expect: The first element popped is at the end of
 *  the returned list, and prefixTree is 'before' that, meaning closer to
 *  the end of the list.  To get to prefixTree, you have to walk to the
 *  end of the list.
 *
 *  For ARG_ORDER_R2L prefixTree is only used to insert extra arguments, as
 *  such we reverse its meaning such that returnValue has a reversed
 *  prefixTree at the head of the list.
 */

GenTreeCall::Use* Compiler::impPopCallArgs(unsigned count, CORINFO_SIG_INFO* sig, GenTreeCall::Use* prefixArgs)
{
    assert(sig == nullptr || count == sig->numArgs);

    GenTreeCall::Use* argList;

    if (Target::g_tgtArgOrder == Target::ARG_ORDER_R2L)
    {
        argList = nullptr;
    }
    else
    { // ARG_ORDER_L2R
        argList = prefixArgs;
    }

    while (count--)
    {
        StackEntry se  = impPopStack();
        GenTree*   arg = se.val;

        if (varTypeIsStruct(arg->GetType()))
        {
            ClassLayout* argLayout = typGetObjLayout(se.seTypeInfo.GetClassHandleForValueClass());
            JITDUMPTREE(arg, "Calling impCanonicalizeStructCallArg(%s) on:\n", argLayout->GetClassName());
            arg = impCanonicalizeStructCallArg(arg, argLayout, CHECK_SPILL_ALL);
            JITDUMPTREE(arg, "resulting tree:\n");
        }

        // NOTE: we defer bashing the type for I_IMPL to fgMorphArgs
        argList = gtPrependNewCallArg(arg, argList);
    }

    if (sig != nullptr)
    {
        if (sig->retTypeSigClass != nullptr && sig->retType != CORINFO_TYPE_CLASS &&
            sig->retType != CORINFO_TYPE_BYREF && sig->retType != CORINFO_TYPE_PTR && sig->retType != CORINFO_TYPE_VAR)
        {
            // Make sure that all valuetypes (including enums) that we push are loaded.
            // This is to guarantee that if a GC is triggerred from the prestub of this methods,
            // all valuetypes in the method signature are already loaded.
            // We need to be able to find the size of the valuetypes, but we cannot
            // do a class-load from within GC.
            info.compCompHnd->classMustBeLoadedBeforeCodeIsRun(sig->retTypeSigClass);
        }

        CORINFO_ARG_LIST_HANDLE sigArgs = sig->args;
        GenTreeCall::Use*       arg;

        for (arg = argList, count = sig->numArgs; count > 0; arg = arg->GetNext(), count--)
        {
            PREFIX_ASSUME(arg != nullptr);

            CORINFO_CLASS_HANDLE argClass;
            CorInfoType          corType = strip(info.compCompHnd->getArgType(sig, sigArgs, &argClass));

            assert(corType != CORINFO_TYPE_VAR);

            var_types jitSigType = JITtype2varType(corType);

            if (!impCheckImplicitArgumentCoercion(jitSigType, arg->GetNode()->TypeGet()))
            {
                BADCODE("the call argument has a type that can't be implicitly converted to the signature type");
            }

            // insert implied casts (from float to double or double to float)

            if ((jitSigType == TYP_DOUBLE) && arg->GetNode()->TypeIs(TYP_FLOAT))
            {
                arg->SetNode(gtNewCastNode(TYP_DOUBLE, arg->GetNode(), false, TYP_DOUBLE));
            }
            else if ((jitSigType == TYP_FLOAT) && arg->GetNode()->TypeIs(TYP_DOUBLE))
            {
                arg->SetNode(gtNewCastNode(TYP_FLOAT, arg->GetNode(), false, TYP_FLOAT));
            }

            // insert any widening or narrowing casts for backwards compatibility

            // TODO-MIKE-Fix: This gets it wrong when the arg is int32 and the param is native uint,
            // the spec requires zero extension but sign extension is done because JITtype2varType
            // erases the signedness of the signature type. Probably doesn't really matter, at least
            // the C# compiler has the habit of inserting its own casts.

            arg->SetNode(impImplicitIorI4Cast(arg->GetNode(), jitSigType));

            if ((corType != CORINFO_TYPE_CLASS) && (corType != CORINFO_TYPE_BYREF) && (corType != CORINFO_TYPE_PTR))
            {
                CORINFO_CLASS_HANDLE argRealClass = info.compCompHnd->getArgClass(sig, sigArgs);
                if (argRealClass != nullptr)
                {
                    // Make sure that all valuetypes (including enums) that we push are loaded.
                    // This is to guarantee that if a GC is triggered from the prestub of this methods,
                    // all valuetypes in the method signature are already loaded.
                    // We need to be able to find the size of the valuetypes, but we cannot
                    // do a class-load from within GC.
                    info.compCompHnd->classMustBeLoadedBeforeCodeIsRun(argRealClass);
                }
            }

            if ((corType == CORINFO_TYPE_VALUECLASS) || (corType == CORINFO_TYPE_REFANY))
            {
                assert(argClass != NO_CLASS_HANDLE);
                arg->SetSigTypeNum(typGetObjLayoutNum(argClass));
            }
            else
            {
                arg->SetSigTypeNum(static_cast<unsigned>(JITtype2varType(corType)));
            }

            sigArgs = info.compCompHnd->getArgNext(sigArgs);
        }
    }

    if (Target::g_tgtArgOrder == Target::ARG_ORDER_R2L)
    {
        // Prepend the prefixTree

        // Simple in-place reversal to place treeList
        // at the end of a reversed prefixTree
        while (prefixArgs != nullptr)
        {
            GenTreeCall::Use* next = prefixArgs->GetNext();
            prefixArgs->SetNext(argList);
            argList    = prefixArgs;
            prefixArgs = next;
        }
    }
    return argList;
}

static bool TypeIs(var_types type1, var_types type2)
{
    return type1 == type2;
}

// Check if type1 matches any type from the list.
template <typename... T>
static bool TypeIs(var_types type1, var_types type2, T... rest)
{
    return TypeIs(type1, type2) || TypeIs(type1, rest...);
}

//------------------------------------------------------------------------
// impCheckImplicitArgumentCoercion: check that the node's type is compatible with
//   the signature's type using ECMA implicit argument coercion table.
//
// Arguments:
//    sigType  - the type in the call signature;
//    nodeType - the node type.
//
// Return Value:
//    true if they are compatible, false otherwise.
//
// Notes:
//   - it is currently allowing byref->long passing, should be fixed in VM;
//   - it can't check long -> native int case on 64-bit platforms,
//      so the behavior is different depending on the target bitness.
//
bool Compiler::impCheckImplicitArgumentCoercion(var_types sigType, var_types nodeType) const
{
    if (sigType == nodeType)
    {
        return true;
    }

    if (TypeIs(sigType, TYP_BOOL, TYP_UBYTE, TYP_BYTE, TYP_USHORT, TYP_SHORT, TYP_UINT, TYP_INT))
    {
        if (TypeIs(nodeType, TYP_BOOL, TYP_UBYTE, TYP_BYTE, TYP_USHORT, TYP_SHORT, TYP_UINT, TYP_INT, TYP_I_IMPL))
        {
            return true;
        }
    }
    else if (TypeIs(sigType, TYP_ULONG, TYP_LONG))
    {
        if (TypeIs(nodeType, TYP_LONG))
        {
            return true;
        }
    }
    else if (TypeIs(sigType, TYP_FLOAT, TYP_DOUBLE))
    {
        if (TypeIs(nodeType, TYP_FLOAT, TYP_DOUBLE))
        {
            return true;
        }
    }
    else if (TypeIs(sigType, TYP_BYREF))
    {
        if (TypeIs(nodeType, TYP_I_IMPL))
        {
            return true;
        }

        // This condition tolerates such IL:
        // ;  V00 this              ref  this class-hnd
        // ldarg.0
        // call(byref)
        if (TypeIs(nodeType, TYP_REF))
        {
            return true;
        }
    }
    else if (varTypeIsStruct(sigType))
    {
        if (varTypeIsStruct(nodeType))
        {
            return true;
        }
    }

    // This condition should not be under `else` because `TYP_I_IMPL`
    // intersects with `TYP_LONG` or `TYP_INT`.
    if (TypeIs(sigType, TYP_I_IMPL, TYP_U_IMPL))
    {
        // Note that it allows `ldc.i8 1; call(nint)` on 64-bit platforms,
        // but we can't distinguish `nint` from `long` there.
        if (TypeIs(nodeType, TYP_I_IMPL, TYP_U_IMPL, TYP_INT, TYP_UINT))
        {
            return true;
        }

        // It tolerates IL that ECMA does not allow but that is commonly used.
        // Example:
        //   V02 loc1           struct <RTL_OSVERSIONINFOEX, 32>
        //   ldloca.s     0x2
        //   call(native int)
        if (TypeIs(nodeType, TYP_BYREF))
        {
            return true;
        }
    }

    return false;
}

/*****************************************************************************
 *
 *  Pop the given number of values from the stack in reverse order (STDCALL/CDECL etc.)
 *  The first "skipReverseCount" items are not reversed.
 */

GenTreeCall::Use* Compiler::impPopReverseCallArgs(unsigned count, CORINFO_SIG_INFO* sig, unsigned skipReverseCount)
{
    assert(skipReverseCount <= count);

    GenTreeCall::Use* list = impPopCallArgs(count, sig);

    // reverse the list
    if (list == nullptr || skipReverseCount == count)
    {
        return list;
    }

    GenTreeCall::Use* ptr          = nullptr; // Initialized to the first node that needs to be reversed
    GenTreeCall::Use* lastSkipNode = nullptr; // Will be set to the last node that does not need to be reversed

    if (skipReverseCount == 0)
    {
        ptr = list;
    }
    else
    {
        lastSkipNode = list;
        // Get to the first node that needs to be reversed
        for (unsigned i = 0; i < skipReverseCount - 1; i++)
        {
            lastSkipNode = lastSkipNode->GetNext();
        }

        PREFIX_ASSUME(lastSkipNode != nullptr);
        ptr = lastSkipNode->GetNext();
    }

    GenTreeCall::Use* reversedList = nullptr;

    do
    {
        GenTreeCall::Use* tmp = ptr->GetNext();
        ptr->SetNext(reversedList);
        reversedList = ptr;
        ptr          = tmp;
    } while (ptr != nullptr);

    if (skipReverseCount)
    {
        lastSkipNode->SetNext(reversedList);
        return list;
    }
    else
    {
        return reversedList;
    }
}

GenTree* Compiler::impAssignStruct(GenTree* dest, GenTree* src, ClassLayout* layout, unsigned curLevel)
{
    return impAssignStruct(dest, src, layout->GetClassHandle(), curLevel);
}

GenTree* Compiler::impAssignStruct(GenTree* dest, GenTree* src, CORINFO_CLASS_HANDLE structHnd, unsigned curLevel)
{
    assert(varTypeIsStruct(dest->GetType()));

    while (dest->OperIs(GT_COMMA))
    {
        assert(varTypeIsStruct(dest->AsOp()->GetOp(1)));
        impAppendTree(dest->AsOp()->GetOp(0), curLevel, impCurStmtOffs);
        dest = dest->AsOp()->GetOp(1);
    }

    // Return a NOP if this is a self-assignment.
    if (dest->OperIs(GT_LCL_VAR) && src->OperIs(GT_LCL_VAR) &&
        (src->AsLclVar()->GetLclNum() == dest->AsLclVar()->GetLclNum()))
    {
        return gtNewNothingNode();
    }

    // TODO-1stClassStructs: Avoid creating an address if it is not needed,
    // or re-creating an indir node if it is.
    GenTree* destAddr;

    if (dest->OperIs(GT_IND, GT_OBJ))
    {
        destAddr = dest->AsIndir()->GetAddr();
    }
    else
    {
        assert(dest->OperIs(GT_LCL_VAR, GT_FIELD, GT_INDEX));

        destAddr = gtNewAddrNode(dest);
    }

    return impAssignStructPtr(destAddr, src, structHnd, curLevel);
}

//------------------------------------------------------------------------
// impAssignStructPtr: Assign (copy) the structure from 'src' to 'destAddr'.
//
// Arguments:
//    destAddr     - address of the destination of the assignment
//    src          - source of the assignment
//    structHnd    - handle representing the struct type
//    curLevel     - stack level for which a spill may be being done
//
// Return Value:
//    The tree that should be appended to the statement list that represents the assignment.
//
// Notes:
//    Temp assignments may be appended to impStmtList if spilling is necessary.

GenTree* Compiler::impAssignStructPtr(GenTree*             destAddr,
                                      GenTree*             src,
                                      CORINFO_CLASS_HANDLE structHnd,
                                      unsigned             curLevel)
{
    assert(src->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_FIELD, GT_IND, GT_OBJ, GT_CALL, GT_MKREFANY, GT_RET_EXPR, GT_COMMA) ||
           (!src->TypeIs(TYP_STRUCT) && src->OperIsSimdOrHWintrinsic()));

    if (src->OperIs(GT_COMMA))
    {
        // TODO-MIKE-Cleanup: Is this really needed? fgMorphCopyBlock already handles COMMAs
        // and it does it correctly. This extracts COMMA's side effect into a new statement
        // without checking if the side effect doesn't interfere with the destination address.
        // It also does this inconsistently - if the block already contains a statement it
        // extracts the side effect, otherwise it sinks the assignment below the comma (and
        // again incorrectly reorders side effects).
        // It looks like the importer generates structs COMMAs only for static fields which
        // probably makes such reordering unlikely to be significant (e.g. a.b.c = sfield
        // could generate a TypeInitializationException instead of a NullReferenceException
        // but it's unlikely that anyone would notice or care about that).

        GenTree* sideEffect = src->AsOp()->GetOp(0);
        GenTree* value      = src->AsOp()->GetOp(1);

        assert(varTypeIsStruct(value->GetType()) || value->TypeIs(TYP_BYREF));

        if (impLastStmt != nullptr)
        {
            impAppendTree(sideEffect, curLevel, impCurStmtOffs);

            return impAssignStructPtr(destAddr, value, structHnd, curLevel);
        }
        else
        {
            // If there's no previous statement put the assignment under COMMA.
            // Why? No idea. Probably because this code was sometimes called from
            // outside the importer. Doesn't matter, one way or another is still
            // incorrectly reorders side effects as mentioned above.

            src->AsOp()->SetOp(1, impAssignStructPtr(destAddr, value, structHnd, curLevel));

            return src;
        }
    }

    // Handle calls that return structs by reference - the destination address
    // is passed to the call as the return buffer address and no assignment is
    // generated.

    if (src->OperIs(GT_CALL))
    {
        GenTreeCall* call = src->AsCall();

        if (call->TreatAsHasRetBufArg())
        {
#if defined(TARGET_WINDOWS) && !defined(TARGET_ARM)
            if (call->IsUnmanaged())
            {
                if (callConvIsInstanceMethodCallConv(call->GetUnmanagedCallConv()))
                {
                    gtInsertNewCallArgAfter(destAddr, call->gtCallArgs);
                }
                else
                {
#ifndef TARGET_X86
                    call->gtCallArgs = gtPrependNewCallArg(destAddr, call->gtCallArgs);
#else
                    // The argument list has already been reversed.
                    // Insert the return buffer as the last node so it will be pushed on to the stack last
                    // as required by the native ABI.
                    GenTreeCall::Use* lastArg = call->gtCallArgs;
                    if (lastArg == nullptr)
                    {
                        call->gtCallArgs = gtPrependNewCallArg(destAddr, call->gtCallArgs);
                    }
                    else
                    {
                        for (; lastArg->GetNext() != nullptr; lastArg = lastArg->GetNext())
                            ;
                        gtInsertNewCallArgAfter(destAddr, lastArg);
                    }
#endif
                }
            }
            else
#endif // defined(TARGET_WINDOWS) && !defined(TARGET_ARM)
            {
                call->gtCallArgs = gtPrependNewCallArg(destAddr, call->gtCallArgs);
            }

            call->SetType(TYP_VOID);

            return src;
        }
    }
    else if (GenTreeRetExpr* retExpr = src->IsRetExpr())
    {
        GenTreeCall* call = retExpr->GetCall();

        assert(retExpr->GetRetExpr() == call);

        if (call->TreatAsHasRetBufArg())
        {
            call->gtCallArgs = gtPrependNewCallArg(destAddr, call->gtCallArgs);
            call->SetType(TYP_VOID);
            src->SetType(TYP_VOID);

            return src;
        }
    }

    // Assigning a MKREFANY generates 2 assignments, one for each field of the struct.
    // One assignment is appended and the other is returned to the caller.

    if (src->OperIs(GT_MKREFANY))
    {
        assert(destAddr->TypeIs(TYP_I_IMPL, TYP_BYREF));

        GenTree* destAddrClone;
        destAddr = impCloneExpr(destAddr, &destAddrClone, structHnd, curLevel DEBUGARG("MKREFANY assignment"));

        FieldSeqNode* valFieldSeq = GetFieldSeqStore()->CreateSingleton(GetRefanyDataField());
        assert(OFFSETOF__CORINFO_TypedReference__dataPtr == 0);
        fgAddFieldSeqForZeroOffset(destAddr, valFieldSeq);
        GenTree* valField = gtNewOperNode(GT_IND, TYP_I_IMPL, destAddr);
        impAppendTree(gtNewAssignNode(valField, src->AsOp()->GetOp(0)), curLevel, impCurStmtOffs);

        FieldSeqNode* typeFieldSeq    = GetFieldSeqStore()->CreateSingleton(GetRefanyTypeField());
        GenTree*      typeFieldOffset = gtNewIconNode(OFFSETOF__CORINFO_TypedReference__type, typeFieldSeq);
        GenTree*      typeFieldAddr   = gtNewOperNode(GT_ADD, destAddr->GetType(), destAddrClone, typeFieldOffset);
        GenTree*      typeField       = gtNewOperNode(GT_IND, TYP_I_IMPL, typeFieldAddr);

        return gtNewAssignNode(typeField, src->AsOp()->GetOp(1));
    }

    // In all other cases we create and return a struct assignment node.

    GenTree* dest = nullptr;

    if (src->OperIs(GT_CALL))
    {
        if (destAddr->OperIs(GT_ADDR) && destAddr->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR))
        {
            dest = destAddr->AsUnOp()->GetOp(0)->AsLclVar();
        }
    }
    else if (src->OperIs(GT_RET_EXPR))
    {
    }
    else if (src->OperIs(GT_OBJ))
    {
        assert(src->GetType() == typGetStructType(structHnd));
        assert((src->AsObj()->GetLayout()->GetClassHandle() == structHnd) || varTypeIsSIMD(src->GetType()));
    }
    else if (src->OperIs(GT_INDEX))
    {
        assert(src->GetType() == typGetStructType(structHnd));
        assert(src->AsIndex()->GetLayout()->GetClassHandle() == structHnd);
    }
    else if (src->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
    }
    else
    {
        assert(src->OperIs(GT_IND, GT_FIELD) || src->OperIsSimdOrHWintrinsic());
        assert((structHnd == NO_CLASS_HANDLE) || (src->GetType() == typGetStructType(structHnd)));
    }

    var_types srcType = src->GetType();

    if ((dest == nullptr) && destAddr->OperIs(GT_ADDR))
    {
        GenTree* destLocation = destAddr->AsUnOp()->GetOp(0);

        if (destLocation->GetType() == srcType)
        {
            if (destLocation->OperIs(GT_LCL_VAR, GT_INDEX, GT_OBJ))
            {
                if (varTypeIsSIMD(srcType))
                {
                    dest = destLocation;
                }
                else
                {
                    ClassLayout* layout;

                    switch (destLocation->GetOper())
                    {
                        case GT_LCL_VAR:
                            layout = lvaGetDesc(destLocation->AsLclVar())->GetLayout();
                            break;
                        case GT_INDEX:
                            layout = destLocation->AsIndex()->GetLayout();
                            break;
                        default:
                            layout = destLocation->AsObj()->GetLayout();
                            break;
                    }

                    if (layout->GetClassHandle() == structHnd)
                    {
                        dest = destLocation;
                    }
                }
            }
            else if (destLocation->OperIs(GT_FIELD))
            {
                // SIMD typed FIELDs can be used directly, STRUCT typed fields need to be wrapped into
                // an OBJ to avoid the struct type getting lost due to single field struct promotion.

                if (varTypeIsSIMD(srcType))
                {
                    dest = destLocation;
                }
            }
        }
    }

    if (dest == nullptr)
    {
        if (srcType == TYP_STRUCT)
        {
            dest = gtNewObjNode(structHnd, destAddr);
        }
        else
        {
            dest = gtNewOperNode(GT_IND, srcType, destAddr);
        }
    }
    else if (dest->OperIs(GT_LCL_VAR))
    {
        LclVarDsc* lcl = lvaGetDesc(dest->AsLclVar());

#if FEATURE_MULTIREG_RET
#ifdef UNIX_AMD64_ABI
        if (src->OperIs(GT_CALL))
#else
        if (src->OperIs(GT_CALL) && src->AsCall()->HasMultiRegRetVal())
#endif
        {
            // If the struct is returned in multiple registers we need to set lvIsMultiRegRet
            // on the destination local variable.

            // TODO-1stClassStructs: Eliminate this pessimization when we can more generally
            // handle multireg returns.

            // TODO-MIKE-Cleanup: Why is lvIsMultiRegRet set unconditionally
            // for UNIX_AMD64_ABI?!
            // Well, because MultiRegRet doesn't really have much to do with
            // multiple registers. The problem is that returning a struct in
            // one or multiple registers results in dependent promotion if
            // registers and promoted fields do not match. So it makes sense
            // to block promotion if the struct is returned in a single reg
            // but it has more than one field.
            // At the same time, this shouldn't be needed if the struct is
            // returned in a single register and has a single field.
            // But what about ARMARCH?!
            // Oh well, the usual mess.

            lcl->lvIsMultiRegRet = true;
        }
#endif
    }

    GenTreeOp* asgNode = gtNewAssignNode(dest, src);
    gtInitStructCopyAsg(asgNode);
    return asgNode;
}

GenTree* Compiler::impGetStructAddr(GenTree*             value,
                                    CORINFO_CLASS_HANDLE structHnd,
                                    unsigned             curLevel,
                                    bool                 willDereference)
{
    assert(varTypeIsStruct(value->GetType()) || info.compCompHnd->isValueClass(structHnd));

    if (value->OperIs(GT_COMMA))
    {
        assert(value->AsOp()->GetOp(1)->GetType() == value->GetType());

        Statement* oldLastStmt = impLastStmt;
        value->AsOp()->SetOp(1, impGetStructAddr(value->AsOp()->GetOp(1), structHnd, curLevel, willDereference));
        value->SetType(TYP_BYREF);

        if (oldLastStmt != impLastStmt)
        {
            // Some temp assignment statement was placed on the statement list
            // for Op2, but that would be out of order with op1, so we need to
            // spill op1 onto the statement list after whatever was last
            // before we recursed on Op2 (i.e. before whatever Op2 appended).
            Statement* beforeStmt;
            if (oldLastStmt == nullptr)
            {
                // The op1 stmt should be the first in the list.
                beforeStmt = impStmtList;
            }
            else
            {
                // Insert after the oldLastStmt before the first inserted for op2.
                beforeStmt = oldLastStmt->GetNextStmt();
            }

            impInsertTreeBefore(value->AsOp()->GetOp(0), impCurStmtOffs, beforeStmt);
            value->AsOp()->SetOp(0, gtNewNothingNode());
        }

        return value;
    }

    if (value->OperIs(GT_OBJ) && willDereference)
    {
        assert(value->AsObj()->GetLayout()->GetClassHandle() == structHnd);
        return value->AsObj()->GetAddr();
    }

    if (value->OperIs(GT_CALL, GT_RET_EXPR, GT_OBJ, GT_MKREFANY) || value->OperIsSimdOrHWintrinsic())
    {
        unsigned tmpNum = lvaNewTemp(structHnd, true DEBUGARG("struct address temp"));
        GenTree* tmp    = gtNewLclvNode(tmpNum, lvaGetDesc(tmpNum)->GetType());
        GenTree* asg    = impAssignStruct(tmp, value, structHnd, curLevel);
        impAppendTree(asg, curLevel, impCurStmtOffs);
        return gtNewAddrNode(gtNewLclvNode(tmpNum, lvaGetDesc(tmpNum)->GetType()));
    }

    assert(value->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_FIELD));

    return gtNewAddrNode(value);
}

GenTree* Compiler::impCanonicalizeStructCallArg(GenTree* arg, ClassLayout* argLayout, unsigned curLevel)
{
    assert(arg->GetType() == typGetStructType(argLayout));

    unsigned argLclNum = BAD_VAR_NUM;
    bool     isCanonical;

    switch (arg->GetOper())
    {
        case GT_CALL:
        case GT_RET_EXPR:
            // TODO-MIKE-Cleanup: We do need a local temp for calls that return structs via
            // a return buffer. Do we also need a temp if structs are returned in registers?
            {
                argLclNum          = lvaNewTemp(argLayout, true DEBUGARG("struct arg temp"));
                GenTree* argLclVar = gtNewLclvNode(argLclNum, lvaGetDesc(argLclNum)->GetType());
                GenTree* asg       = impAssignStruct(argLclVar, arg, argLayout->GetClassHandle(), curLevel);

                impAppendTree(asg, curLevel, impCurStmtOffs);

                arg = gtNewLclvNode(argLclNum, lvaGetDesc(argLclNum)->GetType());
            }

            isCanonical = false;
            break;

        case GT_LCL_VAR:
        case GT_LCL_FLD:
            argLclNum = arg->AsLclVarCommon()->GetLclNum();
            arg       = gtNewObjNode(argLayout, gtNewAddrNode(arg));
            assert(arg->GetType() == lvaGetDesc(argLclNum)->GetType());
            isCanonical = true;
            break;

        case GT_FIELD:
            // FIELDs need to be wrapped in OBJs because FIELD morphing code produces INDs
            // instead of OBJs so we lose the struct type. They can also be turned into
            // primitive type LCL_VARs due to single field struct promotion.
            isCanonical = false;
            break;

        case GT_IND:
            arg         = gtNewObjNode(argLayout, arg->AsIndir()->GetAddr());
            isCanonical = true;
            break;

#if defined(FEATURE_SIMD) || defined(FEATURE_HW_INTRINSICS)
#ifdef FEATURE_SIMD
        case GT_SIMD:
#endif
#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
#endif
            assert(varTypeIsSIMD(arg->GetType()));
            FALLTHROUGH;
#endif
        case GT_MKREFANY:
        case GT_INDEX:
        case GT_OBJ:
            isCanonical = true;
            break;

        case GT_COMMA:
        {
            GenTree* lastComma  = arg;
            GenTree* commaValue = arg->AsOp()->GetOp(1);

            while (commaValue->OperIs(GT_COMMA))
            {
                assert(commaValue->GetType() == arg->GetType());

                lastComma  = commaValue;
                commaValue = commaValue->AsOp()->GetOp(1);
            }

            assert(commaValue->GetType() == arg->GetType());

            if (commaValue->OperIs(GT_FIELD))
            {
                commaValue = gtNewObjNode(argLayout, gtNewAddrNode(commaValue));
            }

#ifdef FEATURE_SIMD
            if (commaValue->OperIsSimdOrHWintrinsic())
            {
                lastComma->AsOp()->SetOp(1, impCanonicalizeStructCallArg(commaValue, argLayout, curLevel));
            }
            else
#endif
            {
                noway_assert(commaValue->OperIs(GT_OBJ));

                // Hoist the block node above the COMMA so we don't have to deal with struct typed COMMAs:
                //   COMMA(x, OBJ(addr)) => OBJ(COMMA(x, addr))

                // TODO-MIKE-Fix: Huh, this doesn't handle multiple COMMAs even though the code above does.
                // Though it looks like struct COMMAs are rare in the importer - only produced by static
                // field access - and aren't nested. And the static field import code could probably be
                // changed to produce OBJ(COMMA(...)) rather than COMMA(OBJ(...)).

                GenTree* addr = commaValue->AsObj()->GetAddr();

                lastComma->SetType(addr->GetType());
                lastComma->AsOp()->SetOp(1, addr);

                commaValue->AsObj()->SetAddr(lastComma);

                if (lastComma == arg)
                {
                    arg = commaValue;
                }
            }

            isCanonical = true;
        }
        break;

        default:
            unreached();
    }

    if (!isCanonical && arg->TypeIs(TYP_STRUCT) && !arg->OperIs(GT_OBJ))
    {
        arg = gtNewObjNode(argLayout, gtNewAddrNode(arg));
    }

    if (arg->OperIs(GT_OBJ))
    {
        if (argLclNum != BAD_VAR_NUM)
        {
            // A OBJ on a ADDR(LCL_VAR) can never raise an exception so we don't set GTF_EXCEPT here.
            if (!lvaGetDesc(argLclNum)->IsImplicitByRefParam())
            {
                arg->gtFlags &= ~GTF_GLOB_REF;
            }
        }
        else
        {
            // In general a OBJ is an indirection and could raise an exception.
            arg->gtFlags |= GTF_EXCEPT;
        }
    }

    return arg;
}

/******************************************************************************/
// Given a type token, generate code that will evaluate to the correct
// handle representation of that token (type handle, field handle, or method handle)
//
// For most cases, the handle is determined at compile-time, and the code
// generated is simply an embedded handle.
//
// Run-time lookup is required if the enclosing method is shared between instantiations
// and the token refers to formal type parameters whose instantiation is not known
// at compile-time.
//
GenTree* Compiler::impTokenToHandle(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                    BOOL*                   pRuntimeLookup /* = NULL */,
                                    BOOL                    mustRestoreHandle /* = FALSE */,
                                    BOOL                    importParent /* = FALSE */)
{
    assert(!fgGlobalMorph);

    CORINFO_GENERICHANDLE_RESULT embedInfo;
    info.compCompHnd->embedGenericHandle(pResolvedToken, importParent, &embedInfo);

    if (pRuntimeLookup)
    {
        *pRuntimeLookup = embedInfo.lookup.lookupKind.needsRuntimeLookup;
    }

    if (mustRestoreHandle && !embedInfo.lookup.lookupKind.needsRuntimeLookup)
    {
        switch (embedInfo.handleType)
        {
            case CORINFO_HANDLETYPE_CLASS:
                info.compCompHnd->classMustBeLoadedBeforeCodeIsRun((CORINFO_CLASS_HANDLE)embedInfo.compileTimeHandle);
                break;

            case CORINFO_HANDLETYPE_METHOD:
                info.compCompHnd->methodMustBeLoadedBeforeCodeIsRun((CORINFO_METHOD_HANDLE)embedInfo.compileTimeHandle);
                break;

            case CORINFO_HANDLETYPE_FIELD:
                info.compCompHnd->classMustBeLoadedBeforeCodeIsRun(
                    info.compCompHnd->getFieldClass((CORINFO_FIELD_HANDLE)embedInfo.compileTimeHandle));
                break;

            default:
                break;
        }
    }

    // Generate the full lookup tree. May be null if we're abandoning an inline attempt.
    GenTree* result = impLookupToTree(pResolvedToken, &embedInfo.lookup, gtTokenToIconFlags(pResolvedToken->token),
                                      embedInfo.compileTimeHandle);

    // If we have a result and it requires runtime lookup, wrap it in a runtime lookup node.
    if ((result != nullptr) && embedInfo.lookup.lookupKind.needsRuntimeLookup)
    {
        result = gtNewRuntimeLookup(embedInfo.compileTimeHandle, embedInfo.handleType, result);
    }

    return result;
}

GenTree* Compiler::impLookupToTree(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                   CORINFO_LOOKUP*         pLookup,
                                   unsigned                handleFlags,
                                   void*                   compileTimeHandle)
{
    if (!pLookup->lookupKind.needsRuntimeLookup)
    {
        // No runtime lookup is required.
        // Access is direct or memory-indirect (of a fixed address) reference

        CORINFO_GENERIC_HANDLE handle       = nullptr;
        void*                  pIndirection = nullptr;
        assert(pLookup->constLookup.accessType != IAT_PPVALUE && pLookup->constLookup.accessType != IAT_RELPVALUE);

        if (pLookup->constLookup.accessType == IAT_VALUE)
        {
            handle = pLookup->constLookup.handle;
        }
        else if (pLookup->constLookup.accessType == IAT_PVALUE)
        {
            pIndirection = pLookup->constLookup.addr;
        }
        GenTree* addr = gtNewIconEmbHndNode(handle, pIndirection, handleFlags, compileTimeHandle);

#ifdef DEBUG
        size_t handleToTrack;
        if (handleFlags == GTF_ICON_TOKEN_HDL)
        {
            handleToTrack = 0;
        }
        else
        {
            handleToTrack = reinterpret_cast<size_t>(compileTimeHandle);
        }

        if (handle != nullptr)
        {
            addr->AsIntCon()->gtTargetHandle = handleToTrack;
        }
        else
        {
            addr->AsIndir()->GetAddr()->AsIntCon()->gtTargetHandle = handleToTrack;
        }
#endif
        return addr;
    }

    if (pLookup->lookupKind.runtimeLookupKind == CORINFO_LOOKUP_NOT_SUPPORTED)
    {
        // Runtime does not support inlining of all shapes of runtime lookups
        // Inlining has to be aborted in such a case
        assert(compIsForInlining());
        compInlineResult->NoteFatal(InlineObservation::CALLSITE_GENERIC_DICTIONARY_LOOKUP);
        return nullptr;
    }

    // Need to use dictionary-based access which depends on the typeContext
    // which is only available at runtime, not at compile-time.
    return impRuntimeLookupToTree(pResolvedToken, pLookup, compileTimeHandle);
}

#ifdef FEATURE_READYTORUN_COMPILER
GenTree* Compiler::impReadyToRunLookupToTree(CORINFO_CONST_LOOKUP* pLookup,
                                             unsigned              handleFlags,
                                             void*                 compileTimeHandle)
{
    CORINFO_GENERIC_HANDLE handle       = nullptr;
    void*                  pIndirection = nullptr;
    assert(pLookup->accessType != IAT_PPVALUE && pLookup->accessType != IAT_RELPVALUE);

    if (pLookup->accessType == IAT_VALUE)
    {
        handle = pLookup->handle;
    }
    else if (pLookup->accessType == IAT_PVALUE)
    {
        pIndirection = pLookup->addr;
    }
    GenTree* addr = gtNewIconEmbHndNode(handle, pIndirection, handleFlags, compileTimeHandle);
#ifdef DEBUG
    assert((handleFlags == GTF_ICON_CLASS_HDL) || (handleFlags == GTF_ICON_METHOD_HDL));
    if (handle != nullptr)
    {
        addr->AsIntCon()->gtTargetHandle = (size_t)compileTimeHandle;
    }
    else
    {
        addr->gtGetOp1()->AsIntCon()->gtTargetHandle = (size_t)compileTimeHandle;
    }
#endif //  DEBUG
    return addr;
}

GenTreeCall* Compiler::impReadyToRunHelperToTree(
    CORINFO_RESOLVED_TOKEN* pResolvedToken,
    CorInfoHelpFunc         helper,
    var_types               type,
    GenTreeCall::Use*       args /* = nullptr */,
    CORINFO_LOOKUP_KIND*    pGenericLookupKind /* =NULL. Only used with generics */)
{
    CORINFO_CONST_LOOKUP lookup;
    if (!info.compCompHnd->getReadyToRunHelper(pResolvedToken, pGenericLookupKind, helper, &lookup))
    {
        return nullptr;
    }

    GenTreeCall* op1 = gtNewHelperCallNode(helper, type, args);

    op1->setEntryPoint(lookup);

    return op1;
}
#endif

GenTree* Compiler::impMethodPointer(CORINFO_RESOLVED_TOKEN* pResolvedToken, CORINFO_CALL_INFO* pCallInfo)
{
    GenTree* op1 = nullptr;

    switch (pCallInfo->kind)
    {
        case CORINFO_CALL:
            op1 = new (this, GT_FTN_ADDR) GenTreeFptrVal(TYP_I_IMPL, pCallInfo->hMethod);

#ifdef FEATURE_READYTORUN_COMPILER
            if (opts.IsReadyToRun())
            {
                op1->AsFptrVal()->gtEntryPoint = pCallInfo->codePointerLookup.constLookup;
            }
#endif
            break;

        case CORINFO_CALL_CODE_POINTER:
            op1 = impLookupToTree(pResolvedToken, &pCallInfo->codePointerLookup, GTF_ICON_FTN_ADDR, pCallInfo->hMethod);
            break;

        default:
            noway_assert(!"unknown call kind");
            break;
    }

    return op1;
}

//------------------------------------------------------------------------
// getRuntimeContextTree: find pointer to context for runtime lookup.
//
// Arguments:
//    kind - lookup kind.
//
// Return Value:
//    Return GenTree pointer to generic shared context.
//
// Notes:
//    Reports about generic context using.

GenTree* Compiler::getRuntimeContextTree(CORINFO_RUNTIME_LOOKUP_KIND kind)
{
    GenTree* ctxTree = nullptr;

    // Collectible types requires that for shared generic code, if we use the generic context parameter
    // that we report it. (This is a conservative approach, we could detect some cases particularly when the
    // context parameter is this that we don't need the eager reporting logic.)
    lvaGenericsContextInUse = true;

    Compiler* pRoot = impInlineRoot();

    if (kind == CORINFO_LOOKUP_THISOBJ)
    {
        // this Object
        ctxTree = gtNewLclvNode(pRoot->info.compThisArg, TYP_REF);
        ctxTree->gtFlags |= GTF_VAR_CONTEXT;

        // context is the method table pointer of the this object
        ctxTree = gtNewMethodTableLookup(ctxTree);
    }
    else
    {
        assert(kind == CORINFO_LOOKUP_METHODPARAM || kind == CORINFO_LOOKUP_CLASSPARAM);

        // Exact method descriptor as passed in
        ctxTree = gtNewLclvNode(pRoot->info.compTypeCtxtArg, TYP_I_IMPL);
        ctxTree->gtFlags |= GTF_VAR_CONTEXT;
    }
    return ctxTree;
}

/*****************************************************************************/
/* Import a dictionary lookup to access a handle in code shared between
   generic instantiations.
   The lookup depends on the typeContext which is only available at
   runtime, and not at compile-time.
   pLookup->token1 and pLookup->token2 specify the handle that is needed.
   The cases are:

   1. pLookup->indirections == CORINFO_USEHELPER : Call a helper passing it the
      instantiation-specific handle, and the tokens to lookup the handle.
   2. pLookup->indirections != CORINFO_USEHELPER :
      2a. pLookup->testForNull == false : Dereference the instantiation-specific handle
          to get the handle.
      2b. pLookup->testForNull == true : Dereference the instantiation-specific handle.
          If it is non-NULL, it is the handle required. Else, call a helper
          to lookup the handle.
 */

GenTree* Compiler::impRuntimeLookupToTree(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                          CORINFO_LOOKUP*         pLookup,
                                          void*                   compileTimeHandle)
{
    GenTree* ctxTree = getRuntimeContextTree(pLookup->lookupKind.runtimeLookupKind);
#if 0
    ctxTree->gtFlags |= GTF_DONT_CSE;   // ToDo Remove this
#endif
    CORINFO_RUNTIME_LOOKUP* pRuntimeLookup = &pLookup->runtimeLookup;
    // It's available only via the run-time helper function
    if (pRuntimeLookup->indirections == CORINFO_USEHELPER)
    {
#ifdef FEATURE_READYTORUN_COMPILER
        if (opts.IsReadyToRun())
        {
            return impReadyToRunHelperToTree(pResolvedToken, CORINFO_HELP_READYTORUN_GENERIC_HANDLE, TYP_I_IMPL,
                                             gtNewCallArgs(ctxTree), &pLookup->lookupKind);
        }
#endif
        return gtNewRuntimeLookupHelperCallNode(pRuntimeLookup, ctxTree, compileTimeHandle);
    }

    // Slot pointer
    GenTree* slotPtrTree = ctxTree;

    if (pRuntimeLookup->testForNull)
    {
        slotPtrTree =
            impCloneExpr(ctxTree, &ctxTree, NO_CLASS_HANDLE, CHECK_SPILL_ALL DEBUGARG("impRuntimeLookup slot"));
    }

    GenTree* indOffTree    = nullptr;
    GenTree* lastIndOfTree = nullptr;

    // Applied repeated indirections
    for (uint16_t i = 0; i < pRuntimeLookup->indirections; i++)
    {
        if ((i == 1 && pRuntimeLookup->indirectFirstOffset) || (i == 2 && pRuntimeLookup->indirectSecondOffset))
        {
            indOffTree = impCloneExpr(slotPtrTree, &slotPtrTree, NO_CLASS_HANDLE,
                                      CHECK_SPILL_ALL DEBUGARG("impRuntimeLookup indirectOffset"));
        }

        // The last indirection could be subject to a size check (dynamic dictionary expansion)
        bool isLastIndirectionWithSizeCheck =
            ((i == pRuntimeLookup->indirections - 1) && (pRuntimeLookup->sizeOffset != CORINFO_NO_SIZE_CHECK));

        if (i != 0)
        {
            slotPtrTree = gtNewOperNode(GT_IND, TYP_I_IMPL, slotPtrTree);
            slotPtrTree->gtFlags |= GTF_IND_NONFAULTING;
            if (!isLastIndirectionWithSizeCheck)
            {
                slotPtrTree->gtFlags |= GTF_IND_INVARIANT;
            }
        }

        if ((i == 1 && pRuntimeLookup->indirectFirstOffset) || (i == 2 && pRuntimeLookup->indirectSecondOffset))
        {
            slotPtrTree = gtNewOperNode(GT_ADD, TYP_I_IMPL, indOffTree, slotPtrTree);
        }

        if (pRuntimeLookup->offsets[i] != 0)
        {
            if (isLastIndirectionWithSizeCheck)
            {
                lastIndOfTree = impCloneExpr(slotPtrTree, &slotPtrTree, NO_CLASS_HANDLE,
                                             CHECK_SPILL_ALL DEBUGARG("impRuntimeLookup indirectOffset"));
            }

            slotPtrTree =
                gtNewOperNode(GT_ADD, TYP_I_IMPL, slotPtrTree, gtNewIconNode(pRuntimeLookup->offsets[i], TYP_I_IMPL));
        }
    }

    // No null test required
    if (!pRuntimeLookup->testForNull)
    {
        if (pRuntimeLookup->indirections == 0)
        {
            return slotPtrTree;
        }

        slotPtrTree = gtNewOperNode(GT_IND, TYP_I_IMPL, slotPtrTree);
        slotPtrTree->gtFlags |= GTF_IND_NONFAULTING;

        if (!pRuntimeLookup->testForFixup)
        {
            return slotPtrTree;
        }

        impSpillSideEffects(true, CHECK_SPILL_ALL DEBUGARG("bubbling QMark0"));

        unsigned slotLclNum = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("impRuntimeLookup test"));
        GenTree* asg        = gtNewAssignNode(gtNewLclvNode(slotLclNum, TYP_I_IMPL), slotPtrTree);
        impAppendTree(asg, CHECK_SPILL_ALL, impCurStmtOffs);

        GenTree* slot = gtNewLclvNode(slotLclNum, TYP_I_IMPL);
#ifdef TARGET_64BIT
        slot = gtNewCastNode(TYP_INT, slot, false, TYP_INT);
#endif
        // Use a GT_AND to check for the lowest bit and indirect if it is set
        GenTree* test  = gtNewOperNode(GT_AND, TYP_INT, slot, gtNewIconNode(1));
        GenTree* relop = gtNewOperNode(GT_EQ, TYP_INT, test, gtNewIconNode(0));

        // slot = GT_IND(slot - 1)
        slot           = gtNewLclvNode(slotLclNum, TYP_I_IMPL);
        GenTree* add   = gtNewOperNode(GT_ADD, TYP_I_IMPL, slot, gtNewIconNode(-1, TYP_I_IMPL));
        GenTree* indir = gtNewOperNode(GT_IND, TYP_I_IMPL, add);
        indir->gtFlags |= GTF_IND_NONFAULTING;
        indir->gtFlags |= GTF_IND_INVARIANT;
        asg = gtNewAssignNode(gtNewLclvNode(slotLclNum, TYP_I_IMPL), indir);

        GenTree* qmark = gtNewQmarkNode(TYP_VOID, relop, gtNewNothingNode(), asg);
        impAppendTree(qmark, CHECK_SPILL_NONE, impCurStmtOffs);

        return gtNewLclvNode(slotLclNum, TYP_I_IMPL);
    }

    assert(pRuntimeLookup->indirections != 0);

    impSpillSideEffects(true, CHECK_SPILL_ALL DEBUGARG("bubbling QMark1"));

    // Extract the handle
    GenTree* handleForNullCheck = gtNewOperNode(GT_IND, TYP_I_IMPL, slotPtrTree);
    handleForNullCheck->gtFlags |= GTF_IND_NONFAULTING;

    // Call the helper
    // - Setup argNode with the pointer to the signature returned by the lookup
    GenTree* argNode = gtNewIconEmbHndNode(pRuntimeLookup->signature, nullptr, GTF_ICON_GLOBAL_PTR, compileTimeHandle);

    GenTreeCall::Use* helperArgs = gtNewCallArgs(ctxTree, argNode);
    GenTreeCall*      helperCall = gtNewHelperCallNode(pRuntimeLookup->helper, TYP_I_IMPL, helperArgs);

    // Check for null and possibly call helper
    GenTree* nullCheck       = gtNewOperNode(GT_NE, TYP_INT, handleForNullCheck, gtNewIconNode(0, TYP_I_IMPL));
    GenTree* handleForResult = gtCloneExpr(handleForNullCheck);

    GenTree* result = nullptr;

    if (pRuntimeLookup->sizeOffset != CORINFO_NO_SIZE_CHECK)
    {
        // Dynamic dictionary expansion support

        assert((lastIndOfTree != nullptr) && (pRuntimeLookup->indirections > 0));

        // sizeValue = dictionary[pRuntimeLookup->sizeOffset]
        GenTreeIntCon* sizeOffset      = gtNewIconNode(pRuntimeLookup->sizeOffset, TYP_I_IMPL);
        GenTree*       sizeValueOffset = gtNewOperNode(GT_ADD, TYP_I_IMPL, lastIndOfTree, sizeOffset);
        GenTree*       sizeValue       = gtNewOperNode(GT_IND, TYP_I_IMPL, sizeValueOffset);
        sizeValue->gtFlags |= GTF_IND_NONFAULTING;

        // sizeCheck fails if sizeValue < pRuntimeLookup->offsets[i]
        GenTree* offsetValue = gtNewIconNode(pRuntimeLookup->offsets[pRuntimeLookup->indirections - 1], TYP_I_IMPL);
        GenTree* sizeCheck   = gtNewOperNode(GT_LE, TYP_INT, sizeValue, offsetValue);

        // revert null check condition.
        nullCheck->ChangeOperUnchecked(GT_EQ);

        // ((sizeCheck fails || nullCheck fails))) ? (helperCall : handle).
        // Add checks and the handle as call arguments, indirect call transformer will handle this.
        helperCall->gtCallArgs = gtPrependNewCallArg(handleForResult, helperCall->gtCallArgs);
        helperCall->gtCallArgs = gtPrependNewCallArg(sizeCheck, helperCall->gtCallArgs);
        helperCall->gtCallArgs = gtPrependNewCallArg(nullCheck, helperCall->gtCallArgs);
        result                 = helperCall;
        addExpRuntimeLookupCandidate(helperCall);
    }
    else
    {
        result = gtNewQmarkNode(TYP_I_IMPL, nullCheck, handleForResult, helperCall);
    }

    unsigned tmp = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("spilling Runtime Lookup tree"));
    GenTree* asg = gtNewAssignNode(gtNewLclvNode(tmp, TYP_I_IMPL), result);
    impAppendTree(asg, CHECK_SPILL_NONE, impCurStmtOffs);
    return gtNewLclvNode(tmp, TYP_I_IMPL);
}

// Spills the stack at verCurrentState.esStack[level] and replaces it with a temp.
void Compiler::impSpillStackEntry(unsigned level DEBUGARG(const char* reason))
{
    GenTree* tree = verCurrentState.esStack[level].val;
    unsigned tnum = lvaGrabTemp(true DEBUGARG(reason));

    impAssignTempGen(tnum, tree, verCurrentState.esStack[level].seTypeInfo.GetClassHandle(), level);

    // If temp is newly introduced and a ref type, grab what type info we can.
    if (lvaTable[tnum].lvType == TYP_REF)
    {
        assert(lvaTable[tnum].lvSingleDef == 0);
        lvaTable[tnum].lvSingleDef = 1;
        JITDUMP("Marked V%02u as a single def temp\n", tnum);
        CORINFO_CLASS_HANDLE stkHnd = verCurrentState.esStack[level].seTypeInfo.GetClassHandle();
        lvaSetClass(tnum, tree, stkHnd);

        // If we're assigning a GT_RET_EXPR, note the temp over on the call,
        // so the inliner can use it in case it needs a return spill temp.
        if (GenTreeRetExpr* retExpr = tree->IsRetExpr())
        {
            JITDUMP("\n*** see V%02u = GT_RET_EXPR, noting temp\n", tnum);

            assert(retExpr->GetRetExpr() == retExpr->GetCall());
            retExpr->GetCall()->gtInlineCandidateInfo->preexistingSpillTemp = tnum;
        }
    }

    // The tree type may be modified by impAssignTempGen, so use the type of the lclVar.
    var_types type                     = genActualType(lvaTable[tnum].TypeGet());
    GenTree*  temp                     = gtNewLclvNode(tnum, type);
    verCurrentState.esStack[level].val = temp;
}

/*****************************************************************************
 *
 *  Ensure that the stack has only spilled values
 */

void Compiler::impSpillStackEnsure(bool spillLeaves)
{
    assert(!spillLeaves || opts.compDbgCode);

    for (unsigned level = 0; level < verCurrentState.esStackDepth; level++)
    {
        GenTree* tree = verCurrentState.esStack[level].val;

        // TODO-MIKE-Review: The use of OperIsLeaf here is kind of risky.
        // It makes sense to ignore such trees when breaking up trees that
        // are too deep but this fails to account for side effects leafs
        // may have (e.g. GT_CLS_VAR). There are all sort of other leafs
        // (e.g. MEMORYBARRIER) and the only reason why this happens to
        // work is that such opers don't appear during import or as part
        // of other trees (MEMORYBARRIER returns VOID). This is also
        // problematic to test because large trees are not that common.

        if (!spillLeaves && tree->OperIsLeaf() && !tree->OperIs(GT_CLS_VAR))
        {
            continue;
        }

        // Temps introduced by the importer itself don't need to be spilled

        bool isTempLcl =
            (tree->OperGet() == GT_LCL_VAR) && (tree->AsLclVarCommon()->GetLclNum() >= info.compLocalsCount);

        if (isTempLcl)
        {
            continue;
        }

        impSpillStackEntry(level DEBUGARG("impSpillStackEnsure"));
    }
}

void Compiler::impSpillEvalStack()
{
    for (unsigned level = 0; level < verCurrentState.esStackDepth; level++)
    {
        impSpillStackEntry(level DEBUGARG("impSpillEvalStack"));
    }
}

/*****************************************************************************
 *
 *  If the stack contains any trees with side effects in them, assign those
 *  trees to temps and append the assignments to the statement list.
 *  On return the stack is guaranteed to be empty.
 */

inline void Compiler::impEvalSideEffects()
{
    impSpillSideEffects(false, (unsigned)CHECK_SPILL_ALL DEBUGARG("impEvalSideEffects"));
    verCurrentState.esStackDepth = 0;
}

/*****************************************************************************
 *
 *  If the stack contains any trees with side effects in them, assign those
 *  trees to temps and replace them on the stack with refs to their temps.
 *  [0..chkLevel) is the portion of the stack which will be checked and spilled.
 */

inline void Compiler::impSpillSideEffects(bool spillGlobEffects, unsigned chkLevel DEBUGARG(const char* reason))
{
    assert(chkLevel != CHECK_SPILL_NONE);

    // Before we make any appends to the tree list we must spill the
    // "special" side effects (GTF_ORDER_SIDEEFF on a GT_CATCH_ARG)

    impSpillSpecialSideEff();

    if (chkLevel == CHECK_SPILL_ALL)
    {
        chkLevel = verCurrentState.esStackDepth;
    }

    assert(chkLevel <= verCurrentState.esStackDepth);

    unsigned spillSideEffects = spillGlobEffects ? GTF_GLOB_EFFECT : GTF_SIDE_EFFECT;

    for (unsigned i = 0; i < chkLevel; i++)
    {
        GenTree* tree = verCurrentState.esStack[i].val;

        if (impIsAddressInLocal(tree))
        {
            // Trees that represent local addresses may have spurios GLOB_REF
            // side effects but they never need need to be spilled.
            continue;
        }

        if ((tree->GetSideEffects() & spillSideEffects) == 0)
        {
            // We haven't yet determined which local variables are address exposed
            // so we cannot rely on GTF_GLOB_REF being present in trees that use
            // such variables. Conservatively assume that address taken variables
            // will be address exposed and get GTF_GLOB_REF.

            if (!spillGlobEffects || !gtHasAddressTakenLocals(tree))
            {
                continue;
            }
        }

        impSpillStackEntry(i DEBUGARG(reason));
    }
}

/*****************************************************************************
 *
 *  If the stack contains any trees with special side effects in them, assign
 *  those trees to temps and replace them on the stack with refs to their temps.
 */

inline void Compiler::impSpillSpecialSideEff()
{
    // Only exception objects need to be carefully handled

    if (!compCurBB->bbCatchTyp)
    {
        return;
    }

    for (unsigned level = 0; level < verCurrentState.esStackDepth; level++)
    {
        GenTree* tree = verCurrentState.esStack[level].val;
        // Make sure if we have an exception object in the sub tree we spill ourselves.
        if (gtHasCatchArg(tree))
        {
            impSpillStackEntry(level DEBUGARG("impSpillSpecialSideEff"));
        }
    }
}

/*****************************************************************************
 *
 *  Spill all stack references to value classes (TYP_STRUCT nodes)
 */

void Compiler::impSpillValueClasses()
{
    for (unsigned level = 0; level < verCurrentState.esStackDepth; level++)
    {
        GenTree* tree = verCurrentState.esStack[level].val;

        if (fgWalkTreePre(&tree, impFindValueClasses) == WALK_ABORT)
        {
            // Tree walk was aborted, which means that we found a
            // value class on the stack.  Need to spill that
            // stack entry.

            impSpillStackEntry(level DEBUGARG("impSpillValueClasses"));
        }
    }
}

/*****************************************************************************
 *
 *  Callback that checks if a tree node is TYP_STRUCT
 */

Compiler::fgWalkResult Compiler::impFindValueClasses(GenTree** pTree, fgWalkData* data)
{
    fgWalkResult walkResult = WALK_CONTINUE;

    if ((*pTree)->gtType == TYP_STRUCT)
    {
        // Abort the walk and indicate that we found a value class

        walkResult = WALK_ABORT;
    }

    return walkResult;
}

/*****************************************************************************
 *
 *  If the stack contains any trees with references to local #lclNum, assign
 *  those trees to temps and replace their place on the stack with refs to
 *  their temps.
 */

void Compiler::impSpillLclRefs(ssize_t lclNum)
{
    /* Before we make any appends to the tree list we must spill the
     * "special" side effects (GTF_ORDER_SIDEEFF) - GT_CATCH_ARG */

    impSpillSpecialSideEff();

    for (unsigned level = 0; level < verCurrentState.esStackDepth; level++)
    {
        GenTree* tree = verCurrentState.esStack[level].val;

        /* If the tree may throw an exception, and the block has a handler,
           then we need to spill assignments to the local if the local is
           live on entry to the handler.
           Just spill 'em all without considering the liveness */

        bool xcptnCaught = ehBlockHasExnFlowDsc(compCurBB) && (tree->gtFlags & (GTF_CALL | GTF_EXCEPT));

        /* Skip the tree if it doesn't have an affected reference,
           unless xcptnCaught */

        if (xcptnCaught || gtHasRef(tree, lclNum))
        {
            impSpillStackEntry(level DEBUGARG("impSpillLclRefs"));
        }
    }
}

/*****************************************************************************
 *
 *  Push catch arg onto the stack.
 *  If there are jumps to the beginning of the handler, insert basic block
 *  and spill catch arg to a temp. Update the handler block if necessary.
 *
 *  Returns the basic block of the actual handler.
 */

BasicBlock* Compiler::impPushCatchArgOnStack(BasicBlock* hndBlk, CORINFO_CLASS_HANDLE clsHnd, bool isSingleBlockFilter)
{
    // Do not inject the basic block twice on reimport. This should be
    // hit only under JIT stress. See if the block is the one we injected.
    // Note that EH canonicalization can inject internal blocks here. We might
    // be able to re-use such a block (but we don't, right now).
    if ((hndBlk->bbFlags & (BBF_IMPORTED | BBF_INTERNAL | BBF_DONT_REMOVE | BBF_HAS_LABEL | BBF_JMP_TARGET)) ==
        (BBF_IMPORTED | BBF_INTERNAL | BBF_DONT_REMOVE | BBF_HAS_LABEL | BBF_JMP_TARGET))
    {
        Statement* stmt = hndBlk->firstStmt();

        if (stmt != nullptr)
        {
            GenTree* tree = stmt->GetRootNode();
            assert(tree != nullptr);

            if ((tree->gtOper == GT_ASG) && (tree->AsOp()->gtOp1->gtOper == GT_LCL_VAR) &&
                (tree->AsOp()->gtOp2->gtOper == GT_CATCH_ARG))
            {
                tree = gtNewLclvNode(tree->AsOp()->gtOp1->AsLclVarCommon()->GetLclNum(), TYP_REF);

                assert(hndBlk->bbEntryState->HasCatchArg());

                return hndBlk->bbNext;
            }
        }

        // If we get here, it must have been some other kind of internal block. It's possible that
        // someone prepended something to our injected block, but that's unlikely.
    }

#if defined(JIT32_GCENCODER)
    const bool forceInsertNewBlock = isSingleBlockFilter || compStressCompile(STRESS_CATCH_ARG, 5);
#else
    const bool forceInsertNewBlock = compStressCompile(STRESS_CATCH_ARG, 5);
#endif // defined(JIT32_GCENCODER)

    // Spill GT_CATCH_ARG to a temp if there are jumps to the beginning of the handler
    if ((hndBlk->bbRefs <= 1) && !forceInsertNewBlock)
    {
        hndBlk->bbEntryState = new (this, CMK_ImpStack) ImportSpillCliqueState(clsHnd);
    }
    else
    {
        if (hndBlk->bbRefs == 1)
        {
            hndBlk->bbRefs++;
        }

        /* Create extra basic block for the spill */
        BasicBlock* newBlk = fgNewBBbefore(BBJ_NONE, hndBlk, /* extendRegion */ true);
        newBlk->bbFlags |= BBF_IMPORTED | BBF_DONT_REMOVE | BBF_HAS_LABEL | BBF_JMP_TARGET;
        newBlk->setBBWeight(hndBlk->bbWeight);
        newBlk->bbCodeOffs   = hndBlk->bbCodeOffs;
        newBlk->bbEntryState = new (this, CMK_ImpStack) ImportSpillCliqueState(clsHnd);

        /* Account for the new link we are about to create */
        hndBlk->bbRefs++;

        // Spill into a temp.
        unsigned   tempNum = lvaNewTemp(TYP_REF, false DEBUGARG("CATCH_ARG spill temp"));
        GenTree*   argAsg  = gtNewAssignNode(gtNewLclvNode(tempNum, TYP_REF), impNewCatchArg());
        Statement* argStmt;

        if (info.compStmtOffsetsImplicit & ICorDebugInfo::CALL_SITE_BOUNDARIES)
        {
            // Report the debug info. impImportBlockCode won't treat the actual handler as exception block and thus
            // won't do it for us.
            impCurStmtOffs = newBlk->bbCodeOffs | IL_OFFSETX_STKBIT;
            argStmt        = gtNewStmt(argAsg, impCurStmtOffs);
        }
        else
        {
            argStmt = gtNewStmt(argAsg);
        }

        fgInsertStmtAtEnd(newBlk, argStmt);

        if (fgCheapPredsValid)
        {
            fgAddCheapPred(hndBlk, newBlk);
        }

        impSetSpillCliqueState(newBlk, new (this, CMK_ImpStack) ImportSpillCliqueState(tempNum, 1));
    }

    return hndBlk;
}

GenTree* Compiler::impNewCatchArg()
{
    GenTree* arg = new (this, GT_CATCH_ARG) GenTree(GT_CATCH_ARG, TYP_REF);
    // GT_CATCH_ARG cannot be moved around since it uses a fixed register on x86 (EAX).
    arg->gtFlags |= GTF_ORDER_SIDEEFF;
    return arg;
}

// Given a tree, clone it. *pClone is set to the cloned tree.
// Returns the original tree if the cloning was easy,
// else returns the temp to which the tree had to be spilled to.
// If the tree has side-effects, it will be spilled to a temp.
GenTree* Compiler::impCloneExpr(GenTree*     tree,
                                GenTree**    clone,
                                ClassLayout* layout,
                                unsigned spillCheckLevel DEBUGARG(const char* reason))
{
    return impCloneExpr(tree, clone, layout == nullptr ? NO_CLASS_HANDLE : layout->GetClassHandle(),
                        spillCheckLevel DEBUGARG(reason));
}

GenTree* Compiler::impCloneExpr(GenTree*             tree,
                                GenTree**            clone,
                                CORINFO_CLASS_HANDLE structHnd,
                                unsigned spillCheckLevel DEBUGARG(const char* reason))
{
    if ((tree->gtFlags & GTF_GLOB_EFFECT) == 0)
    {
        *clone = gtClone(tree, true);

        if (*clone != nullptr)
        {
            return tree;
        }
    }

    unsigned lclNum = lvaGrabTemp(true DEBUGARG(reason));
    impAssignTempGen(lclNum, tree, structHnd, spillCheckLevel);
    var_types type = varActualType(lvaGetDesc(lclNum)->GetType());

    *clone = gtNewLclvNode(lclNum, type);
    return gtNewLclvNode(lclNum, type);
}

void Compiler::impMakeMultiUse(GenTree*     tree,
                               unsigned     useCount,
                               GenTree**    uses,
                               ClassLayout* layout,
                               unsigned spillCheckLevel DEBUGARG(const char* reason))
{
    assert(useCount > 1);

    if ((tree->gtFlags & GTF_GLOB_EFFECT) == 0)
    {
        uses[0] = tree;
        uses[1] = gtClone(tree, true);

        if (uses[1] != nullptr)
        {
            for (unsigned i = 2; i < useCount; i++)
            {
                uses[i] = gtClone(tree, true);
            }
            return;
        }
    }

    unsigned lclNum = lvaGrabTemp(true DEBUGARG(reason));
    impAssignTempGen(lclNum, tree, layout, spillCheckLevel);
    var_types type = varActualType(lvaGetDesc(lclNum)->GetType());

    for (unsigned i = 0; i < useCount; i++)
    {
        uses[i] = gtNewLclvNode(lclNum, type);
    }
}

/*****************************************************************************
 * Remember the IL offset (including stack-empty info) for the trees we will
 * generate now.
 */

inline void Compiler::impCurStmtOffsSet(IL_OFFSET offs)
{
    if (compIsForInlining())
    {
        Statement* callStmt = impInlineInfo->iciStmt;
        impCurStmtOffs      = callStmt->GetILOffsetX();
    }
    else
    {
        assert(offs == BAD_IL_OFFSET || (offs & IL_OFFSETX_BITS) == 0);
        IL_OFFSETX stkBit = (verCurrentState.esStackDepth > 0) ? IL_OFFSETX_STKBIT : 0;
        impCurStmtOffs    = offs | stkBit;
    }
}

/*****************************************************************************
 * Returns current IL offset with stack-empty and call-instruction info incorporated
 */
inline IL_OFFSETX Compiler::impCurILOffset(IL_OFFSET offs, bool callInstruction)
{
    if (compIsForInlining())
    {
        return BAD_IL_OFFSET;
    }
    else
    {
        assert(offs == BAD_IL_OFFSET || (offs & IL_OFFSETX_BITS) == 0);
        IL_OFFSETX stkBit             = (verCurrentState.esStackDepth > 0) ? IL_OFFSETX_STKBIT : 0;
        IL_OFFSETX callInstructionBit = callInstruction ? IL_OFFSETX_CALLINSTRUCTIONBIT : 0;
        return offs | stkBit | callInstructionBit;
    }
}

//------------------------------------------------------------------------
// impCanSpillNow: check is it possible to spill all values from eeStack to local variables.
//
// Arguments:
//    prevOpcode - last importer opcode
//
// Return Value:
//    true if it is legal, false if it could be a sequence that we do not want to divide.
bool Compiler::impCanSpillNow(OPCODE prevOpcode)
{
    // Don't spill after ldtoken, newarr and newobj, because it could be a part of the InitializeArray sequence.
    // Avoid breaking up to guarantee that impInitializeArrayIntrinsic can succeed.
    return (prevOpcode != CEE_LDTOKEN) && (prevOpcode != CEE_NEWARR) && (prevOpcode != CEE_NEWOBJ);
}

/*****************************************************************************
 *
 *  Remember the instr offset for the statements
 *
 *  When we do impAppendTree(tree), we can't set stmt->SetLastILOffset(impCurOpcOffs),
 *  if the append was done because of a partial stack spill,
 *  as some of the trees corresponding to code up to impCurOpcOffs might
 *  still be sitting on the stack.
 *  So we delay calling of SetLastILOffset() until impNoteLastILoffs().
 *  This should be called when an opcode finally/explicitly causes
 *  impAppendTree(tree) to be called (as opposed to being called because of
 *  a spill caused by the opcode)
 */

#ifdef DEBUG

void Compiler::impNoteLastILoffs()
{
    if (impLastILoffsStmt == nullptr)
    {
        // We should have added a statement for the current basic block
        // Is this assert correct ?

        assert(impLastStmt);

        impLastStmt->SetLastILOffset(compIsForInlining() ? BAD_IL_OFFSET : impCurOpcOffs);
    }
    else
    {
        impLastILoffsStmt->SetLastILOffset(compIsForInlining() ? BAD_IL_OFFSET : impCurOpcOffs);
        impLastILoffsStmt = nullptr;
    }
}

#endif // DEBUG

/*****************************************************************************
 * We don't create any GenTree (excluding spills) for a branch.
 * For debugging info, we need a placeholder so that we can note
 * the IL offset in gtStmt.gtStmtOffs. So append an empty statement.
 */

void Compiler::impNoteBranchOffs()
{
    if (opts.compDbgCode)
    {
        impAppendTree(gtNewNothingNode(), (unsigned)CHECK_SPILL_NONE, impCurStmtOffs);
    }
}

/*****************************************************************************
 * Locate the next stmt boundary for which we need to record info.
 * We will have to spill the stack at such boundaries if it is not
 * already empty.
 * Returns the next stmt boundary (after the start of the block)
 */

unsigned Compiler::impInitBlockLineInfo()
{
    /* Assume the block does not correspond with any IL offset. This prevents
       us from reporting extra offsets. Extra mappings can cause confusing
       stepping, especially if the extra mapping is a jump-target, and the
       debugger does not ignore extra mappings, but instead rewinds to the
       nearest known offset */

    impCurStmtOffsSet(BAD_IL_OFFSET);

    if (compIsForInlining())
    {
        return ~0;
    }

    IL_OFFSET blockOffs = compCurBB->bbCodeOffs;

    if ((verCurrentState.esStackDepth == 0) && (info.compStmtOffsetsImplicit & ICorDebugInfo::STACK_EMPTY_BOUNDARIES))
    {
        impCurStmtOffsSet(blockOffs);
    }

    if (false && (info.compStmtOffsetsImplicit & ICorDebugInfo::CALL_SITE_BOUNDARIES))
    {
        impCurStmtOffsSet(blockOffs);
    }

    /* Always report IL offset 0 or some tests get confused.
       Probably a good idea anyways */

    if (blockOffs == 0)
    {
        impCurStmtOffsSet(blockOffs);
    }

    if (!info.compStmtOffsetsCount)
    {
        return ~0;
    }

    /* Find the lowest explicit stmt boundary within the block */

    /* Start looking at an entry that is based on our instr offset */

    unsigned index = (info.compStmtOffsetsCount * blockOffs) / info.compILCodeSize;

    if (index >= info.compStmtOffsetsCount)
    {
        index = info.compStmtOffsetsCount - 1;
    }

    /* If we've guessed too far, back up */

    while (index > 0 && info.compStmtOffsets[index - 1] >= blockOffs)
    {
        index--;
    }

    /* If we guessed short, advance ahead */

    while (info.compStmtOffsets[index] < blockOffs)
    {
        index++;

        if (index == info.compStmtOffsetsCount)
        {
            return info.compStmtOffsetsCount;
        }
    }

    assert(index < info.compStmtOffsetsCount);

    if (info.compStmtOffsets[index] == blockOffs)
    {
        /* There is an explicit boundary for the start of this basic block.
           So we will start with bbCodeOffs. Else we will wait until we
           get to the next explicit boundary */

        impCurStmtOffsSet(blockOffs);

        index++;
    }

    return index;
}

/*****************************************************************************/

static inline bool impOpcodeIsCallOpcode(OPCODE opcode)
{
    switch (opcode)
    {
        case CEE_CALL:
        case CEE_CALLI:
        case CEE_CALLVIRT:
            return true;

        default:
            return false;
    }
}

/*****************************************************************************/

static inline bool impOpcodeIsCallSiteBoundary(OPCODE opcode)
{
    switch (opcode)
    {
        case CEE_CALL:
        case CEE_CALLI:
        case CEE_CALLVIRT:
        case CEE_JMP:
        case CEE_NEWOBJ:
        case CEE_NEWARR:
            return true;

        default:
            return false;
    }
}

/*****************************************************************************/

// One might think it is worth caching these values, but results indicate
// that it isn't.
// In addition, caching them causes SuperPMI to be unable to completely
// encapsulate an individual method context.
CORINFO_CLASS_HANDLE Compiler::impGetRefAnyClass()
{
    CORINFO_CLASS_HANDLE refAnyClass = info.compCompHnd->getBuiltinClass(CLASSID_TYPED_BYREF);
    assert(refAnyClass != (CORINFO_CLASS_HANDLE) nullptr);
    return refAnyClass;
}

CORINFO_CLASS_HANDLE Compiler::impGetTypeHandleClass()
{
    CORINFO_CLASS_HANDLE typeHandleClass = info.compCompHnd->getBuiltinClass(CLASSID_TYPE_HANDLE);
    assert(typeHandleClass != (CORINFO_CLASS_HANDLE) nullptr);
    return typeHandleClass;
}

CORINFO_CLASS_HANDLE Compiler::impGetRuntimeArgumentHandle()
{
    CORINFO_CLASS_HANDLE argIteratorClass = info.compCompHnd->getBuiltinClass(CLASSID_ARGUMENT_HANDLE);
    assert(argIteratorClass != (CORINFO_CLASS_HANDLE) nullptr);
    return argIteratorClass;
}

CORINFO_CLASS_HANDLE Compiler::impGetStringClass()
{
    CORINFO_CLASS_HANDLE stringClass = info.compCompHnd->getBuiltinClass(CLASSID_STRING);
    assert(stringClass != (CORINFO_CLASS_HANDLE) nullptr);
    return stringClass;
}

CORINFO_CLASS_HANDLE Compiler::impGetObjectClass()
{
    CORINFO_CLASS_HANDLE objectClass = info.compCompHnd->getBuiltinClass(CLASSID_SYSTEM_OBJECT);
    assert(objectClass != (CORINFO_CLASS_HANDLE) nullptr);
    return objectClass;
}

/* static */
void Compiler::impBashVarAddrsToI(GenTree* tree1, GenTree* tree2 /* = nullptr */)
{
    // "&local" can be used either as TYP_BYREF or TYP_I_IMPL, but we
    // set its type to TYP_BYREF when we create it. We know if it can
    // be changed to TYP_I_IMPL only at the point where we use it.

    if (tree1->TypeIs(TYP_BYREF) && (tree1->IsLocalAddrExpr() != nullptr))
    {
        tree1->SetType(TYP_I_IMPL);
    }

    if ((tree2 != nullptr) && tree2->TypeIs(TYP_BYREF) && (tree2->IsLocalAddrExpr() != nullptr))
    {
        tree2->SetType(TYP_I_IMPL);
    }
}

/*****************************************************************************
 *  TYP_INT and TYP_I_IMPL can be used almost interchangeably, but we want
 *  to make that an explicit cast in our trees, so any implicit casts that
 *  exist in the IL (at least on 64-bit where TYP_I_IMPL != TYP_INT) are
 *  turned into explicit casts here.
 *  We also allow an implicit conversion of a ldnull into a TYP_I_IMPL(0)
 */

GenTree* Compiler::impImplicitIorI4Cast(GenTree* tree, var_types dstTyp)
{
    var_types currType   = genActualType(tree->gtType);
    var_types wantedType = genActualType(dstTyp);

    if (wantedType != currType)
    {
        // Automatic upcast for a GT_CNS_INT into TYP_I_IMPL
        if ((tree->OperGet() == GT_CNS_INT) && varTypeIsI(dstTyp))
        {
            if (!varTypeIsI(tree->gtType) || ((tree->gtType == TYP_REF) && (tree->AsIntCon()->gtIconVal == 0)))
            {
                tree->gtType = TYP_I_IMPL;
            }
        }
#ifdef TARGET_64BIT
        else if (varTypeIsI(wantedType) && (currType == TYP_INT))
        {
            // Note that this allows TYP_INT to be cast to a TYP_I_IMPL when wantedType is a TYP_BYREF or TYP_REF
            tree = gtNewCastNode(TYP_I_IMPL, tree, false, TYP_I_IMPL);
        }
        else if ((wantedType == TYP_INT) && varTypeIsI(currType))
        {
            // Note that this allows TYP_BYREF or TYP_REF to be cast to a TYP_INT
            tree = gtNewCastNode(TYP_INT, tree, false, TYP_INT);
        }
#endif // TARGET_64BIT
    }

    return tree;
}

/*****************************************************************************
 *  TYP_FLOAT and TYP_DOUBLE can be used almost interchangeably in some cases,
 *  but we want to make that an explicit cast in our trees, so any implicit casts
 *  that exist in the IL are turned into explicit casts here.
 */

GenTree* Compiler::impImplicitR4orR8Cast(GenTree* tree, var_types dstTyp)
{
    if (varTypeIsFloating(tree) && varTypeIsFloating(dstTyp) && (dstTyp != tree->gtType))
    {
        tree = gtNewCastNode(dstTyp, tree, false, dstTyp);
    }

    return tree;
}

//------------------------------------------------------------------------
// impInitializeArrayIntrinsic: Attempts to replace a call to InitializeArray
//    with a GT_COPYBLK node.
//
// Arguments:
//    sig - The InitializeArray signature.
//
// Return Value:
//    A pointer to the newly created GT_COPYBLK node if the replacement succeeds or
//    nullptr otherwise.
//
// Notes:
//    The function recognizes the following IL pattern:
//      ldc <length> or a list of ldc <lower bound>/<length>
//      newarr or newobj
//      dup
//      ldtoken <field handle>
//      call InitializeArray
//    The lower bounds need not be constant except when the array rank is 1.
//    The function recognizes all kinds of arrays thus enabling a small runtime
//    such as CoreRT to skip providing an implementation for InitializeArray.

GenTree* Compiler::impInitializeArrayIntrinsic(CORINFO_SIG_INFO* sig)
{
    assert(sig->numArgs == 2);

    GenTree* fieldTokenNode = impStackTop(0).val;
    GenTree* arrayLocalNode = impStackTop(1).val;

    //
    // Verify that the field token is known and valid.  Note that It's also
    // possible for the token to come from reflection, in which case we cannot do
    // the optimization and must therefore revert to calling the helper.  You can
    // see an example of this in bvt\DynIL\initarray2.exe (in Main).
    //

    // Check to see if the ldtoken helper call is what we see here.
    if (fieldTokenNode->gtOper != GT_CALL || (fieldTokenNode->AsCall()->gtCallType != CT_HELPER) ||
        (fieldTokenNode->AsCall()->gtCallMethHnd != eeFindHelper(CORINFO_HELP_FIELDDESC_TO_STUBRUNTIMEFIELD)))
    {
        return nullptr;
    }

    // Strip helper call away
    fieldTokenNode = fieldTokenNode->AsCall()->gtCallArgs->GetNode();

    if (fieldTokenNode->gtOper == GT_IND)
    {
        fieldTokenNode = fieldTokenNode->AsOp()->gtOp1;
    }

    // Check for constant
    if (fieldTokenNode->gtOper != GT_CNS_INT)
    {
        return nullptr;
    }

    CORINFO_FIELD_HANDLE fieldToken = (CORINFO_FIELD_HANDLE)fieldTokenNode->AsIntCon()->gtCompileTimeHandle;
    if (!fieldTokenNode->IsIconHandle(GTF_ICON_FIELD_HDL) || (fieldToken == nullptr))
    {
        return nullptr;
    }

    //
    // We need to get the number of elements in the array and the size of each element.
    // We verify that the newarr statement is exactly what we expect it to be.
    // If it's not then we just return NULL and we don't optimize this call
    //

    // It is possible the we don't have any statements in the block yet.
    if (impLastStmt == nullptr)
    {
        return nullptr;
    }

    //
    // We start by looking at the last statement, making sure it's an assignment, and
    // that the target of the assignment is the array passed to InitializeArray.
    //
    GenTree* arrayAssignment = impLastStmt->GetRootNode();
    if ((arrayAssignment->gtOper != GT_ASG) || (arrayAssignment->AsOp()->gtOp1->gtOper != GT_LCL_VAR) ||
        (arrayLocalNode->gtOper != GT_LCL_VAR) || (arrayAssignment->AsOp()->gtOp1->AsLclVarCommon()->GetLclNum() !=
                                                   arrayLocalNode->AsLclVarCommon()->GetLclNum()))
    {
        return nullptr;
    }

    //
    // Make sure that the object being assigned is a helper call.
    //

    GenTree* newArrayCall = arrayAssignment->AsOp()->gtOp2;
    if ((newArrayCall->gtOper != GT_CALL) || (newArrayCall->AsCall()->gtCallType != CT_HELPER))
    {
        return nullptr;
    }

    //
    // Verify that it is one of the new array helpers.
    //

    bool isMDArray = false;

    if (newArrayCall->AsCall()->gtCallMethHnd != eeFindHelper(CORINFO_HELP_NEWARR_1_DIRECT) &&
        newArrayCall->AsCall()->gtCallMethHnd != eeFindHelper(CORINFO_HELP_NEWARR_1_OBJ) &&
        newArrayCall->AsCall()->gtCallMethHnd != eeFindHelper(CORINFO_HELP_NEWARR_1_VC) &&
        newArrayCall->AsCall()->gtCallMethHnd != eeFindHelper(CORINFO_HELP_NEWARR_1_ALIGN8)
#ifdef FEATURE_READYTORUN_COMPILER
        && newArrayCall->AsCall()->gtCallMethHnd != eeFindHelper(CORINFO_HELP_READYTORUN_NEWARR_1)
#endif
            )
    {
        if (newArrayCall->AsCall()->gtCallMethHnd != eeFindHelper(CORINFO_HELP_NEW_MDARR_NONVARARG))
        {
            return nullptr;
        }

        isMDArray = true;
    }

    CORINFO_CLASS_HANDLE arrayClsHnd = (CORINFO_CLASS_HANDLE)newArrayCall->AsCall()->compileTimeHelperArgumentHandle;

    //
    // Make sure we found a compile time handle to the array
    //

    if (!arrayClsHnd)
    {
        return nullptr;
    }

    unsigned rank = 0;
    S_UINT32 numElements;

    if (isMDArray)
    {
        rank = info.compCompHnd->getArrayRank(arrayClsHnd);

        if (rank == 0)
        {
            return nullptr;
        }

        GenTreeCall::Use* tokenArg = newArrayCall->AsCall()->gtCallArgs;
        assert(tokenArg != nullptr);
        GenTreeCall::Use* numArgsArg = tokenArg->GetNext();
        assert(numArgsArg != nullptr);
        GenTreeCall::Use* argsArg = numArgsArg->GetNext();
        assert(argsArg != nullptr);

        //
        // The number of arguments should be a constant between 1 and 64. The rank can't be 0
        // so at least one length must be present and the rank can't exceed 32 so there can
        // be at most 64 arguments - 32 lengths and 32 lower bounds.
        //

        if ((!numArgsArg->GetNode()->IsCnsIntOrI()) || (numArgsArg->GetNode()->AsIntCon()->IconValue() < 1) ||
            (numArgsArg->GetNode()->AsIntCon()->IconValue() > 64))
        {
            return nullptr;
        }

        unsigned numArgs = static_cast<unsigned>(numArgsArg->GetNode()->AsIntCon()->IconValue());
        bool     lowerBoundsSpecified;

        if (numArgs == rank * 2)
        {
            lowerBoundsSpecified = true;
        }
        else if (numArgs == rank)
        {
            lowerBoundsSpecified = false;

            //
            // If the rank is 1 and a lower bound isn't specified then the runtime creates
            // a SDArray. Note that even if a lower bound is specified it can be 0 and then
            // we get a SDArray as well, see the for loop below.
            //

            if (rank == 1)
            {
                isMDArray = false;
            }
        }
        else
        {
            return nullptr;
        }

        //
        // The rank is known to be at least 1 so we can start with numElements being 1
        // to avoid the need to special case the first dimension.
        //

        numElements = S_UINT32(1);

        struct Match
        {
            static bool IsArgsFieldInit(GenTree* tree, unsigned index, unsigned lvaNewObjArrayArgs)
            {
                return (tree->OperGet() == GT_ASG) && IsArgsFieldIndir(tree->gtGetOp1(), index, lvaNewObjArrayArgs) &&
                       IsArgsAddr(tree->gtGetOp1()->gtGetOp1()->gtGetOp1(), lvaNewObjArrayArgs);
            }

            static bool IsArgsFieldIndir(GenTree* tree, unsigned index, unsigned lvaNewObjArrayArgs)
            {
                return (tree->OperGet() == GT_IND) && (tree->gtGetOp1()->OperGet() == GT_ADD) &&
                       (tree->gtGetOp1()->gtGetOp2()->IsIntegralConst(sizeof(INT32) * index)) &&
                       IsArgsAddr(tree->gtGetOp1()->gtGetOp1(), lvaNewObjArrayArgs);
            }

            static bool IsArgsAddr(GenTree* tree, unsigned lvaNewObjArrayArgs)
            {
                return (tree->OperGet() == GT_ADDR) && (tree->gtGetOp1()->OperGet() == GT_LCL_VAR) &&
                       (tree->gtGetOp1()->AsLclVar()->GetLclNum() == lvaNewObjArrayArgs);
            }

            static bool IsComma(GenTree* tree)
            {
                return (tree != nullptr) && (tree->OperGet() == GT_COMMA);
            }
        };

        unsigned argIndex = 0;
        GenTree* comma;

        for (comma = argsArg->GetNode(); Match::IsComma(comma); comma = comma->gtGetOp2())
        {
            if (lowerBoundsSpecified)
            {
                //
                // In general lower bounds can be ignored because they're not needed to
                // calculate the total number of elements. But for single dimensional arrays
                // we need to know if the lower bound is 0 because in this case the runtime
                // creates a SDArray and this affects the way the array data offset is calculated.
                //

                if (rank == 1)
                {
                    GenTree* lowerBoundAssign = comma->gtGetOp1();
                    assert(Match::IsArgsFieldInit(lowerBoundAssign, argIndex, lvaNewObjArrayArgs));
                    GenTree* lowerBoundNode = lowerBoundAssign->gtGetOp2();

                    if (lowerBoundNode->IsIntegralConst(0))
                    {
                        isMDArray = false;
                    }
                }

                comma = comma->gtGetOp2();
                argIndex++;
            }

            GenTree* lengthNodeAssign = comma->gtGetOp1();
            assert(Match::IsArgsFieldInit(lengthNodeAssign, argIndex, lvaNewObjArrayArgs));
            GenTree* lengthNode = lengthNodeAssign->gtGetOp2();

            if (!lengthNode->IsCnsIntOrI())
            {
                return nullptr;
            }

            numElements *= S_SIZE_T(lengthNode->AsIntCon()->IconValue());
            argIndex++;
        }

        assert((comma != nullptr) && Match::IsArgsAddr(comma, lvaNewObjArrayArgs));

        if (argIndex != numArgs)
        {
            return nullptr;
        }
    }
    else
    {
        //
        // Make sure there are exactly two arguments:  the array class and
        // the number of elements.
        //

        GenTree* arrayLengthNode;

        GenTreeCall::Use* args = newArrayCall->AsCall()->gtCallArgs;
#ifdef FEATURE_READYTORUN_COMPILER
        if (newArrayCall->AsCall()->gtCallMethHnd == eeFindHelper(CORINFO_HELP_READYTORUN_NEWARR_1))
        {
            // Array length is 1st argument for readytorun helper
            arrayLengthNode = args->GetNode();
        }
        else
#endif
        {
            // Array length is 2nd argument for regular helper
            arrayLengthNode = args->GetNext()->GetNode();
        }

        //
        // This optimization is only valid for a constant array size.
        //
        if (arrayLengthNode->gtOper != GT_CNS_INT)
        {
            return nullptr;
        }

        numElements = S_SIZE_T(arrayLengthNode->AsIntCon()->gtIconVal);

        if (!info.compCompHnd->isSDArray(arrayClsHnd))
        {
            return nullptr;
        }
    }

    CORINFO_CLASS_HANDLE elemClsHnd;
    var_types            elementType = JITtype2varType(info.compCompHnd->getChildType(arrayClsHnd, &elemClsHnd));

    // Note that genTypeSize will return zero for non primitive types, which is exactly
    // what we want (size will then be 0, and we will catch this in the conditional below).

    S_UINT32 elemSize(genTypeSize(elementType));
    S_UINT32 size = elemSize * S_UINT32(numElements);

    if (size.IsOverflow())
    {
        return nullptr;
    }

    if ((size.Value() == 0) || (varTypeIsGC(elementType)))
    {
        return nullptr;
    }

    void* initData = info.compCompHnd->getArrayInitializationData(fieldToken, size.Value());
    if (initData == nullptr)
    {
        return nullptr;
    }

    //
    // At this point we are ready to commit to implementing the InitializeArray
    // intrinsic using a struct assignment.  Pop the arguments from the stack and
    // return the struct assignment node.
    //

    impPopStack();
    impPopStack();

    const unsigned blkSize = size.Value();
    unsigned       dataOffset;

    if (isMDArray)
    {
        dataOffset = eeGetMDArrayDataOffset(elementType, rank);
    }
    else
    {
        dataOffset = eeGetArrayDataOffset(elementType);
    }

    GenTree*    dstAddr = gtNewOperNode(GT_ADD, TYP_BYREF, arrayLocalNode, gtNewIconNode(dataOffset, TYP_I_IMPL));
    GenTreeBlk* dst     = new (this, GT_BLK) GenTreeBlk(dstAddr, typGetBlkLayout(blkSize));
    GenTree*    srcAddr = gtNewIconHandleNode(reinterpret_cast<size_t>(initData), GTF_ICON_CONST_PTR);
    GenTreeBlk* src     = new (this, GT_BLK) GenTreeBlk(srcAddr, dst->GetLayout());

    dst->gtFlags |= GTF_IND_NONFAULTING;
    src->gtFlags |= GTF_IND_NONFAULTING | GTF_IND_INVARIANT;

    INDEBUG(srcAddr->AsIntCon()->gtTargetHandle = THT_IntializeArrayIntrinsics;)

    return gtNewAssignNode(dst, src);
}

//------------------------------------------------------------------------
// impIntrinsic: possibly expand intrinsic call into alternate IR sequence
//
// Arguments:
//    newobjThis - for constructor calls, the tree for the newly allocated object
//    clsHnd - handle for the intrinsic method's class
//    method - handle for the intrinsic method
//    sig    - signature of the intrinsic method
//    methodFlags - CORINFO_FLG_XXX flags of the intrinsic method
//    memberRef - the token for the intrinsic method
//    readonlyCall - true if call has a readonly prefix
//    tailCall - true if call is in tail position
//    pConstrainedResolvedToken -- resolved token for constrained call, or nullptr
//       if call is not constrained
//    constraintCallThisTransform -- this transform to apply for a constrained call
//    pIntrinsicID [OUT] -- intrinsic ID (see enumeration in corinfo.h)
//       for "traditional" jit intrinsics
//    isSpecialIntrinsic [OUT] -- set true if intrinsic expansion is a call
//       that is amenable to special downstream optimization opportunities
//
// Returns:
//    IR tree to use in place of the call, or nullptr if the jit should treat
//    the intrinsic call like a normal call.
//
//    pIntrinsicID set to non-illegal value if the call is recognized as a
//    traditional jit intrinsic, even if the intrinsic is not expaned.
//
//    isSpecial set true if the expansion is subject to special
//    optimizations later in the jit processing
//
// Notes:
//    On success the IR tree may be a call to a different method or an inline
//    sequence. If it is a call, then the intrinsic processing here is responsible
//    for handling all the special cases, as upon return to impImportCall
//    expanded intrinsics bypass most of the normal call processing.
//
//    Intrinsics are generally not recognized in minopts and debug codegen.
//
//    However, certain traditional intrinsics are identifed as "must expand"
//    if there is no fallback implmentation to invoke; these must be handled
//    in all codegen modes.
//
//    New style intrinsics (where the fallback implementation is in IL) are
//    identified as "must expand" if they are invoked from within their
//    own method bodies.
//

GenTree* Compiler::impIntrinsic(GenTree*                newobjThis,
                                CORINFO_CLASS_HANDLE    clsHnd,
                                CORINFO_METHOD_HANDLE   method,
                                CORINFO_SIG_INFO*       sig,
                                unsigned                methodFlags,
                                int                     memberRef,
                                bool                    readonlyCall,
                                bool                    tailCall,
                                CORINFO_RESOLVED_TOKEN* pConstrainedResolvedToken,
                                CORINFO_THIS_TRANSFORM  constraintCallThisTransform,
                                CorInfoIntrinsics*      pIntrinsicID,
                                bool*                   isSpecialIntrinsic)
{
    assert((methodFlags & (CORINFO_FLG_INTRINSIC | CORINFO_FLG_JIT_INTRINSIC)) != 0);

    bool              mustExpand  = false;
    bool              isSpecial   = false;
    CorInfoIntrinsics intrinsicID = CORINFO_INTRINSIC_Illegal;
    NamedIntrinsic    ni          = NI_Illegal;

    if ((methodFlags & CORINFO_FLG_INTRINSIC) != 0)
    {
        intrinsicID = info.compCompHnd->getIntrinsicID(method, &mustExpand);
    }

    if ((methodFlags & CORINFO_FLG_JIT_INTRINSIC) != 0)
    {
        // The recursive non-virtual calls to Jit intrinsics are must-expand by convention.
        mustExpand = mustExpand || (gtIsRecursiveCall(method) && !(methodFlags & CORINFO_FLG_VIRTUAL));

        if (intrinsicID == CORINFO_INTRINSIC_Illegal)
        {
            ni = lookupNamedIntrinsic(method);

            // We specially support the following on all platforms to allow for dead
            // code optimization and to more generally support recursive intrinsics.

            if (ni == NI_IsSupported_True)
            {
                assert(sig->numArgs == 0);
                return gtNewIconNode(true);
            }

            if (ni == NI_IsSupported_False)
            {
                assert(sig->numArgs == 0);
                return gtNewIconNode(false);
            }

            if (ni == NI_Throw_PlatformNotSupportedException)
            {
                return impUnsupportedNamedIntrinsic(CORINFO_HELP_THROW_PLATFORM_NOT_SUPPORTED, method, sig, mustExpand);
            }

#ifdef FEATURE_HW_INTRINSICS
            if ((ni > NI_HW_INTRINSIC_START) && (ni < NI_HW_INTRINSIC_END))
            {
                GenTree* hwintrinsic = impHWIntrinsic(ni, clsHnd, method, sig, mustExpand);

                if (mustExpand && (hwintrinsic == nullptr))
                {
                    return impUnsupportedNamedIntrinsic(CORINFO_HELP_THROW_NOT_IMPLEMENTED, method, sig, mustExpand);
                }

                return hwintrinsic;
            }

            if ((ni > NI_SIMD_AS_HWINTRINSIC_START) && (ni < NI_SIMD_AS_HWINTRINSIC_END))
            {
                // These intrinsics aren't defined recursively and so they will never be mustExpand
                // Instead, they provide software fallbacks that will be executed instead.

                assert(!mustExpand);
                return impSimdAsHWIntrinsic(ni, clsHnd, method, sig, newobjThis);
            }
#endif // FEATURE_HW_INTRINSICS
        }
    }

    *pIntrinsicID = intrinsicID;

#ifndef TARGET_ARM
    genTreeOps interlockedOperator;
#endif

    if (intrinsicID == CORINFO_INTRINSIC_StubHelpers_GetStubContext)
    {
        // must be done regardless of DbgCode and MinOpts
        return gtNewLclvNode(lvaStubArgumentVar, TYP_I_IMPL);
    }
#ifdef TARGET_64BIT
    if (intrinsicID == CORINFO_INTRINSIC_StubHelpers_GetStubContextAddr)
    {
        // must be done regardless of DbgCode and MinOpts
        return gtNewAddrNode(gtNewLclvNode(lvaStubArgumentVar, TYP_I_IMPL), TYP_I_IMPL);
    }
#else
    assert(intrinsicID != CORINFO_INTRINSIC_StubHelpers_GetStubContextAddr);
#endif

    if (intrinsicID == CORINFO_INTRINSIC_StubHelpers_NextCallReturnAddress)
    {
        // For now we just avoid inlining anything into these methods since
        // this intrinsic is only rarely used. We could do this better if we
        // wanted to by trying to match which call is the one we need to get
        // the return address of.
        info.compHasNextCallRetAddr = true;
        return new (this, GT_LABEL) GenTree(GT_LABEL, TYP_I_IMPL);
    }

    GenTree* retNode = nullptr;

    // Under debug and minopts, only expand what is required.
    // NextCallReturnAddress intrinsic returns the return address of the next call.
    // If that call is an intrinsic and is expanded, codegen for NextCallReturnAddress will fail.
    // To avoid that we conservatively expand only required intrinsics in methods that call
    // the NextCallReturnAddress intrinsic.
    if (!mustExpand && (opts.OptimizationDisabled() || info.compHasNextCallRetAddr))
    {
        *pIntrinsicID = CORINFO_INTRINSIC_Illegal;
        return retNode;
    }

    var_types callType = JITtype2varType(sig->retType);

    /* First do the intrinsics which are always smaller than a call */

    switch (intrinsicID)
    {
        GenTree* op1;
        GenTree* op2;

#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
        // TODO-ARM-CQ: reenable treating Interlocked operation as intrinsic

        // Note that CORINFO_INTRINSIC_InterlockedAdd32/64 are not actually used.
        // Anyway, we can import them as XADD and leave it to lowering/codegen to perform
        // whatever optimizations may arise from the fact that result value is not used.
        case CORINFO_INTRINSIC_InterlockedAdd32:
        case CORINFO_INTRINSIC_InterlockedXAdd32:
            interlockedOperator = GT_XADD;
            goto InterlockedBinOpCommon;
        case CORINFO_INTRINSIC_InterlockedXchg32:
            interlockedOperator = GT_XCHG;
            goto InterlockedBinOpCommon;

#ifdef TARGET_64BIT
        case CORINFO_INTRINSIC_InterlockedAdd64:
        case CORINFO_INTRINSIC_InterlockedXAdd64:
            interlockedOperator = GT_XADD;
            goto InterlockedBinOpCommon;
        case CORINFO_INTRINSIC_InterlockedXchg64:
            interlockedOperator = GT_XCHG;
            goto InterlockedBinOpCommon;
#endif // TARGET_AMD64

        InterlockedBinOpCommon:
            assert(callType != TYP_STRUCT);
            assert(sig->numArgs == 2);

            op2 = impPopStack().val;
            op1 = impPopStack().val;

            // This creates:
            //   val
            // XAdd
            //   addr
            //     field (for example)
            //
            // In the case where the first argument is the address of a local, we might
            // want to make this *not* make the var address-taken -- but atomic instructions
            // on a local are probably pretty useless anyway, so we probably don't care.

            op1 = gtNewOperNode(interlockedOperator, genActualType(callType), op1, op2);
            op1->gtFlags |= GTF_GLOB_REF | GTF_ASG;
            retNode = op1;
            break;
#endif // defined(TARGET_XARCH) || defined(TARGET_ARM64)

        case CORINFO_INTRINSIC_MemoryBarrier:
        case CORINFO_INTRINSIC_MemoryBarrierLoad:

            assert(sig->numArgs == 0);

            op1 = new (this, GT_MEMORYBARRIER) GenTree(GT_MEMORYBARRIER, TYP_VOID);
            op1->gtFlags |= GTF_GLOB_REF | GTF_ASG;

            // On XARCH `CORINFO_INTRINSIC_MemoryBarrierLoad` fences need not be emitted.
            // However, we still need to capture the effect on reordering.
            if (intrinsicID == CORINFO_INTRINSIC_MemoryBarrierLoad)
            {
                op1->gtFlags |= GTF_MEMORYBARRIER_LOAD;
            }

            retNode = op1;
            break;

#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
        // TODO-ARM-CQ: reenable treating InterlockedCmpXchg32 operation as intrinsic
        case CORINFO_INTRINSIC_InterlockedCmpXchg32:
#ifdef TARGET_64BIT
        case CORINFO_INTRINSIC_InterlockedCmpXchg64:
#endif
        {
            assert(callType != TYP_STRUCT);
            assert(sig->numArgs == 3);
            GenTree* op3;

            op3 = impPopStack().val; // comparand
            op2 = impPopStack().val; // value
            op1 = impPopStack().val; // location

            GenTree* node = new (this, GT_CMPXCHG) GenTreeCmpXchg(genActualType(callType), op1, op2, op3);

            node->AsCmpXchg()->gtOpLocation->gtFlags |= GTF_DONT_CSE;
            retNode = node;
            break;
        }
#endif // defined(TARGET_XARCH) || defined(TARGET_ARM64)

        case CORINFO_INTRINSIC_StringLength:
            op1 = impPopStack().val;
            if (opts.OptimizationEnabled())
            {
                if (op1->OperIs(GT_CNS_STR))
                {
                    // Optimize `ldstr + String::get_Length()` to CNS_INT
                    // e.g. "Hello".Length => 5
                    int             length = -1;
                    const char16_t* str    = info.compCompHnd->getStringLiteral(op1->AsStrCon()->gtScpHnd,
                                                                             op1->AsStrCon()->gtSconCPX, &length);
                    if (length >= 0)
                    {
                        retNode = gtNewIconNode(length);
                        if (str != nullptr) // can be NULL for dynamic context
                        {
                            JITDUMP("Optimizing '\"%ws\".Length' to just '%d'\n", str, length);
                        }
                        else
                        {
                            JITDUMP("Optimizing 'CNS_STR.Length' to just '%d'\n", length);
                        }
                        break;
                    }
                }

                op1 = gtNewArrLen(op1, OFFSETOF__CORINFO_String__stringLen, compCurBB);
            }
            else
            {
                /* Create the expression "*(str_addr + stringLengthOffset)" */
                op1 = gtNewOperNode(GT_ADD, TYP_BYREF, op1,
                                    gtNewIconNode(OFFSETOF__CORINFO_String__stringLen, TYP_I_IMPL));
                op1 = gtNewOperNode(GT_IND, TYP_INT, op1);
            }

            // Getting the length of a null string should throw
            op1->gtFlags |= GTF_EXCEPT;

            retNode = op1;
            break;

        case CORINFO_INTRINSIC_StringGetChar:
            op2     = impPopStack().val;
            op1     = impPopStack().val;
            op1     = gtNewStringIndex(op1, op2);
            retNode = op1;
            break;

        case CORINFO_INTRINSIC_InitializeArray:
            retNode = impInitializeArrayIntrinsic(sig);
            break;

        case CORINFO_INTRINSIC_Array_Address:
        case CORINFO_INTRINSIC_Array_Get:
        case CORINFO_INTRINSIC_Array_Set:
            retNode = impArrayAccessIntrinsic(clsHnd, sig, memberRef, readonlyCall, intrinsicID);
            break;

        case CORINFO_INTRINSIC_GetTypeFromHandle:
            op1 = impStackTop(0).val;
            CorInfoHelpFunc typeHandleHelper;
            if (op1->gtOper == GT_CALL && (op1->AsCall()->gtCallType == CT_HELPER) &&
                gtIsTypeHandleToRuntimeTypeHandleHelper(op1->AsCall(), &typeHandleHelper))
            {
                op1 = impPopStack().val;
                // Replace helper with a more specialized helper that returns RuntimeType
                if (typeHandleHelper == CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE)
                {
                    typeHandleHelper = CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE;
                }
                else
                {
                    assert(typeHandleHelper == CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE_MAYBENULL);
                    typeHandleHelper = CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE_MAYBENULL;
                }
                assert(op1->AsCall()->gtCallArgs->GetNext() == nullptr);
                op1         = gtNewHelperCallNode(typeHandleHelper, TYP_REF, op1->AsCall()->gtCallArgs);
                op1->gtType = TYP_REF;
                retNode     = op1;
            }
            // Call the regular function.
            break;

        case CORINFO_INTRINSIC_RTH_GetValueInternal:
            op1 = impStackTop(0).val;
            if (op1->gtOper == GT_CALL && (op1->AsCall()->gtCallType == CT_HELPER) &&
                gtIsTypeHandleToRuntimeTypeHandleHelper(op1->AsCall()))
            {
                // Old tree
                // Helper-RuntimeTypeHandle -> TreeToGetNativeTypeHandle
                //
                // New tree
                // TreeToGetNativeTypeHandle

                // Remove call to helper and return the native TypeHandle pointer that was the parameter
                // to that helper.

                op1 = impPopStack().val;

                // Get native TypeHandle argument to old helper
                GenTreeCall::Use* arg = op1->AsCall()->gtCallArgs;
                assert(arg->GetNext() == nullptr);
                op1     = arg->GetNode();
                retNode = op1;
            }
            // Call the regular function.
            break;

        case CORINFO_INTRINSIC_Object_GetType:
        {
            JITDUMP("\n impIntrinsic: call to Object.GetType\n");
            op1 = impStackTop(0).val;

            // If we're calling GetType on a boxed value, just get the type directly.
            if (op1->IsBox())
            {
                JITDUMP("Attempting to optimize box(...).getType() to direct type construction\n");

                // Try and clean up the box. Obtain the handle we
                // were going to pass to the newobj.
                GenTree* boxTypeHandle = gtTryRemoveBoxUpstreamEffects(op1, BR_REMOVE_AND_NARROW_WANT_TYPE_HANDLE);

                if (boxTypeHandle != nullptr)
                {
                    // Note we don't need to play the TYP_STRUCT games here like
                    // do for LDTOKEN since the return value of this operator is Type,
                    // not RuntimeTypeHandle.
                    impPopStack();
                    GenTreeCall::Use* helperArgs = gtNewCallArgs(boxTypeHandle);
                    GenTree*          runtimeType =
                        gtNewHelperCallNode(CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE, TYP_REF, helperArgs);
                    retNode = runtimeType;
                }
            }

            // If we have a constrained callvirt with a "box this" transform
            // we know we have a value class and hence an exact type.
            //
            // If so, instead of boxing and then extracting the type, just
            // construct the type directly.
            if ((retNode == nullptr) && (pConstrainedResolvedToken != nullptr) &&
                (constraintCallThisTransform == CORINFO_BOX_THIS))
            {
                // Ensure this is one of the is simple box cases (in particular, rule out nullables).
                const CorInfoHelpFunc boxHelper = info.compCompHnd->getBoxHelper(pConstrainedResolvedToken->hClass);
                const bool            isSafeToOptimize = (boxHelper == CORINFO_HELP_BOX);

                if (isSafeToOptimize)
                {
                    JITDUMP("Optimizing constrained box-this obj.getType() to direct type construction\n");
                    impPopStack();
                    GenTree* typeHandleOp =
                        impTokenToHandle(pConstrainedResolvedToken, nullptr, TRUE /* mustRestoreHandle */);
                    if (typeHandleOp == nullptr)
                    {
                        assert(compDonotInline());
                        return nullptr;
                    }
                    GenTreeCall::Use* helperArgs = gtNewCallArgs(typeHandleOp);
                    GenTree*          runtimeType =
                        gtNewHelperCallNode(CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE, TYP_REF, helperArgs);
                    retNode = runtimeType;
                }
            }

#ifdef DEBUG
            if (retNode != nullptr)
            {
                JITDUMP("Optimized result for call to GetType is\n");
                if (verbose)
                {
                    gtDispTree(retNode);
                }
            }
#endif

            // Else expand as an intrinsic, unless the call is constrained,
            // in which case we defer expansion to allow impImportCall do the
            // special constraint processing.
            if ((retNode == nullptr) && (pConstrainedResolvedToken == nullptr))
            {
                JITDUMP("Expanding as special intrinsic\n");
                impPopStack();
                op1 = new (this, GT_INTRINSIC) GenTreeIntrinsic(genActualType(callType), op1, intrinsicID, ni, method);

                // Set the CALL flag to indicate that the operator is implemented by a call.
                // Set also the EXCEPTION flag because the native implementation of
                // CORINFO_INTRINSIC_Object_GetType intrinsic can throw NullReferenceException.
                op1->gtFlags |= (GTF_CALL | GTF_EXCEPT);
                retNode = op1;
                // Might be further optimizable, so arrange to leave a mark behind
                isSpecial = true;
            }

            if (retNode == nullptr)
            {
                JITDUMP("Leaving as normal call\n");
                // Might be further optimizable, so arrange to leave a mark behind
                isSpecial = true;
            }

            break;
        }

        // Implement ByReference Ctor.  This wraps the assignment of the ref into a byref-like field
        // in a value type.  The canonical example of this is Span<T>. In effect this is just a
        // substitution.  The parameter byref will be assigned into the newly allocated object.
        case CORINFO_INTRINSIC_ByReference_Ctor:
        {
            // Remove call to constructor and directly assign the byref passed
            // to the call to the first slot of the ByReference struct.
            op1                                    = impPopStack().val;
            GenTree*             thisptr           = newobjThis;
            CORINFO_FIELD_HANDLE fldHnd            = info.compCompHnd->getFieldInClass(clsHnd, 0);
            GenTree*             field             = gtNewFieldRef(TYP_BYREF, fldHnd, thisptr, 0);
            GenTree*             assign            = gtNewAssignNode(field, op1);
            GenTree*             byReferenceStruct = gtCloneExpr(thisptr->gtGetOp1());
            assert(byReferenceStruct != nullptr);
            impPushOnStack(byReferenceStruct, typeInfo(TI_STRUCT, clsHnd));
            retNode = assign;
            break;
        }
        // Implement ptr value getter for ByReference struct.
        case CORINFO_INTRINSIC_ByReference_Value:
        {
            op1                         = impPopStack().val;
            CORINFO_FIELD_HANDLE fldHnd = info.compCompHnd->getFieldInClass(clsHnd, 0);
            GenTree*             field  = gtNewFieldRef(TYP_BYREF, fldHnd, op1, 0);
            retNode                     = field;
            break;
        }
        case CORINFO_INTRINSIC_Span_GetItem:
        case CORINFO_INTRINSIC_ReadOnlySpan_GetItem:
        {
            // Have index, stack pointer-to Span<T> s on the stack. Expand to:
            //
            // For Span<T>
            //   Comma
            //     BoundsCheck(index, s->_length)
            //     s->_pointer + index * sizeof(T)
            //
            // For ReadOnlySpan<T> -- same expansion, as it now returns a readonly ref
            //
            // Signature should show one class type parameter, which
            // we need to examine.
            assert(sig->sigInst.classInstCount == 1);
            CORINFO_CLASS_HANDLE spanElemHnd = sig->sigInst.classInst[0];
            const unsigned       elemSize    = info.compCompHnd->getClassSize(spanElemHnd);
            assert(elemSize > 0);

            const bool isReadOnly = (intrinsicID == CORINFO_INTRINSIC_ReadOnlySpan_GetItem);

            JITDUMP("\nimpIntrinsic: Expanding %sSpan<T>.get_Item, T=%s, sizeof(T)=%u\n", isReadOnly ? "ReadOnly" : "",
                    info.compCompHnd->getClassName(spanElemHnd), elemSize);

            GenTree* index          = impPopStack().val;
            GenTree* ptrToSpan      = impPopStack().val;
            GenTree* indexClone     = nullptr;
            GenTree* ptrToSpanClone = nullptr;
            assert(varTypeIsIntegral(index));
            assert(ptrToSpan->TypeGet() == TYP_BYREF);

#if defined(DEBUG)
            if (verbose)
            {
                printf("with ptr-to-span\n");
                gtDispTree(ptrToSpan);
                printf("and index\n");
                gtDispTree(index);
            }
#endif // defined(DEBUG)

            // We need to use both index and ptr-to-span twice, so clone or spill.
            index = impCloneExpr(index, &indexClone, NO_CLASS_HANDLE, CHECK_SPILL_ALL DEBUGARG("Span.get_Item index"));
            ptrToSpan = impCloneExpr(ptrToSpan, &ptrToSpanClone, NO_CLASS_HANDLE,
                                     CHECK_SPILL_ALL DEBUGARG("Span.get_Item ptrToSpan"));

            // Bounds check
            CORINFO_FIELD_HANDLE lengthHnd    = info.compCompHnd->getFieldInClass(clsHnd, 1);
            const unsigned       lengthOffset = info.compCompHnd->getFieldOffset(lengthHnd);
            GenTree*             length       = gtNewFieldRef(TYP_INT, lengthHnd, ptrToSpan, lengthOffset);
            GenTree*             boundsCheck  = gtNewArrBoundsChk(index, length, SCK_RNGCHK_FAIL);

            // Element access
            GenTree*             indexIntPtr = impImplicitIorI4Cast(indexClone, TYP_I_IMPL);
            GenTree*             sizeofNode  = gtNewIconNode(elemSize, TYP_I_IMPL);
            GenTree*             mulNode     = gtNewOperNode(GT_MUL, TYP_I_IMPL, indexIntPtr, sizeofNode);
            CORINFO_FIELD_HANDLE ptrHnd      = info.compCompHnd->getFieldInClass(clsHnd, 0);
            const unsigned       ptrOffset   = info.compCompHnd->getFieldOffset(ptrHnd);
            GenTree*             data        = gtNewFieldRef(TYP_BYREF, ptrHnd, ptrToSpanClone, ptrOffset);
            GenTree*             result      = gtNewOperNode(GT_ADD, TYP_BYREF, data, mulNode);

            // Prepare result
            var_types resultType = JITtype2varType(sig->retType);
            assert(resultType == result->TypeGet());
            retNode = gtNewOperNode(GT_COMMA, resultType, boundsCheck, result);

            break;
        }

        case CORINFO_INTRINSIC_GetRawHandle:
        {
            noway_assert(IsTargetAbi(CORINFO_CORERT_ABI)); // Only CoreRT supports it.
            CORINFO_RESOLVED_TOKEN resolvedToken;
            resolvedToken.tokenContext = impTokenLookupContextHandle;
            resolvedToken.tokenScope   = info.compScopeHnd;
            resolvedToken.token        = memberRef;
            resolvedToken.tokenType    = CORINFO_TOKENKIND_Method;

            CORINFO_GENERICHANDLE_RESULT embedInfo;
            info.compCompHnd->expandRawHandleIntrinsic(&resolvedToken, &embedInfo);

            GenTree* rawHandle = impLookupToTree(&resolvedToken, &embedInfo.lookup, gtTokenToIconFlags(memberRef),
                                                 embedInfo.compileTimeHandle);
            if (rawHandle == nullptr)
            {
                return nullptr;
            }

            noway_assert(rawHandle->TypeIs(TYP_I_IMPL));

            unsigned rawHandleSlot = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("rawHandle"));
            GenTree* asg           = gtNewAssignNode(gtNewLclvNode(rawHandleSlot, TYP_I_IMPL), rawHandle);
            impAppendTree(asg, CHECK_SPILL_NONE, impCurStmtOffs);
            GenTree* lclVarAddr = gtNewAddrNode(gtNewLclvNode(rawHandleSlot, TYP_I_IMPL), TYP_I_IMPL);

            retNode = gtNewOperNode(GT_IND, JITtype2varType(sig->retType), lclVarAddr);

            break;
        }

        case CORINFO_INTRINSIC_TypeEQ:
        case CORINFO_INTRINSIC_TypeNEQ:
        {
            JITDUMP("Importing Type.op_*Equality intrinsic\n");
            op1              = impStackTop(1).val;
            op2              = impStackTop(0).val;
            GenTree* optTree = gtFoldTypeEqualityCall(intrinsicID, op1, op2);
            if (optTree != nullptr)
            {
                // Success, clean up the evaluation stack.
                impPopStack();
                impPopStack();

                // See if we can optimize even further, to a handle compare.
                optTree = gtFoldTypeCompare(optTree);

                // See if we can now fold a handle compare to a constant.
                optTree = gtFoldExpr(optTree);

                retNode = optTree;
            }
            else
            {
                // Retry optimizing these later
                isSpecial = true;
            }
            break;
        }

        default:
            /* Unknown intrinsic */
            intrinsicID = CORINFO_INTRINSIC_Illegal;
            break;
    }

    // Look for new-style jit intrinsics by name
    if (ni != NI_Illegal)
    {
        assert(retNode == nullptr);
        switch (ni)
        {
            case NI_System_Enum_HasFlag:
            {
                GenTree* thisOp  = impStackTop(1).val;
                GenTree* flagOp  = impStackTop(0).val;
                GenTree* optTree = gtOptimizeEnumHasFlag(thisOp, flagOp);

                if (optTree != nullptr)
                {
                    // Optimization successful. Pop the stack for real.
                    impPopStack();
                    impPopStack();
                    retNode = optTree;
                }
                else
                {
                    // Retry optimizing this during morph.
                    isSpecial = true;
                }

                break;
            }

            case NI_System_Type_IsAssignableFrom:
            {
                GenTree* typeTo   = impStackTop(1).val;
                GenTree* typeFrom = impStackTop(0).val;

                retNode = impTypeIsAssignable(typeTo, typeFrom);
                break;
            }

            case NI_System_Type_IsAssignableTo:
            {
                GenTree* typeTo   = impStackTop(0).val;
                GenTree* typeFrom = impStackTop(1).val;

                retNode = impTypeIsAssignable(typeTo, typeFrom);
                break;
            }

            case NI_System_Type_get_IsValueType:
            {
                // Optimize
                //
                //   call Type.GetTypeFromHandle (which is replaced with CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE)
                //   call Type.IsValueType
                //
                // to `true` or `false`
                // e.g. `typeof(int).IsValueType` => `true`
                if (impStackTop().val->IsCall())
                {
                    GenTreeCall* call = impStackTop().val->AsCall();
                    if (call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE))
                    {
                        CORINFO_CLASS_HANDLE hClass = gtGetHelperArgClassHandle(call->gtCallArgs->GetNode());
                        if (hClass != NO_CLASS_HANDLE)
                        {
                            retNode =
                                gtNewIconNode((eeIsValueClass(hClass) &&
                                               // pointers are not value types (e.g. typeof(int*).IsValueType is false)
                                               info.compCompHnd->asCorInfoType(hClass) != CORINFO_TYPE_PTR)
                                                  ? 1
                                                  : 0);
                            impPopStack(); // drop CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE call
                        }
                    }
                }
                break;
            }

            case NI_System_Threading_Thread_get_ManagedThreadId:
                if (opts.OptimizationEnabled())
                {
                    if (GenTreeRetExpr* retExpr = impStackTop().val->IsRetExpr())
                    {
                        GenTreeCall* call = retExpr->GetCall();
                        assert(retExpr->GetRetExpr() == call);

                        if (((call->gtFlags & CORINFO_FLG_JIT_INTRINSIC) != 0) &&
                            (lookupNamedIntrinsic(call->gtCallMethHnd) == NI_System_Threading_Thread_get_CurrentThread))
                        {
                            // drop get_CurrentThread() call
                            impPopStack();
                            call->ReplaceWith(gtNewNothingNode(), this);
                            retNode = gtNewHelperCallNode(CORINFO_HELP_GETCURRENTMANAGEDTHREADID, TYP_INT);
                        }
                    }
                }
                break;

#ifdef TARGET_ARM64
            // Intrinsify Interlocked.Or and Interlocked.And only for arm64-v8.1 (and newer)
            // TODO-CQ: Implement for XArch (https://github.com/dotnet/runtime/issues/32239).
            case NI_System_Threading_Interlocked_Or:
            case NI_System_Threading_Interlocked_And:
            {
                if (opts.OptimizationEnabled() && compOpportunisticallyDependsOn(InstructionSet_Atomics))
                {
                    assert(sig->numArgs == 2);
                    GenTree*   op2 = impPopStack().val;
                    GenTree*   op1 = impPopStack().val;
                    genTreeOps op  = (ni == NI_System_Threading_Interlocked_Or) ? GT_XORR : GT_XAND;
                    retNode        = gtNewOperNode(op, genActualType(callType), op1, op2);
                    retNode->gtFlags |= GTF_GLOB_REF | GTF_ASG;
                }
                break;
            }
#endif // TARGET_ARM64

#ifdef FEATURE_HW_INTRINSICS
            case NI_System_Math_FusedMultiplyAdd:
            {
#ifdef TARGET_XARCH
                if (compExactlyDependsOn(InstructionSet_FMA) && supportSIMDTypes())
                {
                    assert(varTypeIsFloating(callType));

                    // We are constructing a chain of intrinsics similar to:
                    //    return FMA.MultiplyAddScalar(
                    //        Vector128.CreateScalarUnsafe(x),
                    //        Vector128.CreateScalarUnsafe(y),
                    //        Vector128.CreateScalarUnsafe(z)
                    //    ).ToScalar();

                    GenTree* op3 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, callType, 16,
                                                            impPopStack().val);
                    GenTree* op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, callType, 16,
                                                            impPopStack().val);
                    GenTree* op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, callType, 16,
                                                            impPopStack().val);
                    GenTree* res =
                        gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_FMA_MultiplyAddScalar, callType, 16, op1, op2, op3);

                    retNode = gtNewSimdHWIntrinsicNode(callType, NI_Vector128_ToScalar, callType, 16, res);
                    break;
                }
#elif defined(TARGET_ARM64)
                if (compExactlyDependsOn(InstructionSet_AdvSimd))
                {
                    assert(varTypeIsFloating(callType));

                    // We are constructing a chain of intrinsics similar to:
                    //    return AdvSimd.FusedMultiplyAddScalar(
                    //        Vector64.Create{ScalarUnsafe}(z),
                    //        Vector64.Create{ScalarUnsafe}(y),
                    //        Vector64.Create{ScalarUnsafe}(x)
                    //    ).ToScalar();

                    NamedIntrinsic createVector64 =
                        (callType == TYP_DOUBLE) ? NI_Vector64_Create : NI_Vector64_CreateScalarUnsafe;

                    constexpr unsigned int simdSize = 8;

                    GenTree* op3 =
                        gtNewSimdHWIntrinsicNode(TYP_SIMD8, createVector64, callType, simdSize, impPopStack().val);
                    GenTree* op2 =
                        gtNewSimdHWIntrinsicNode(TYP_SIMD8, createVector64, callType, simdSize, impPopStack().val);
                    GenTree* op1 =
                        gtNewSimdHWIntrinsicNode(TYP_SIMD8, createVector64, callType, simdSize, impPopStack().val);

                    // Note that AdvSimd.FusedMultiplyAddScalar(op1,op2,op3) corresponds to op1 + op2 * op3
                    // while Math{F}.FusedMultiplyAddScalar(op1,op2,op3) corresponds to op1 * op2 + op3
                    retNode = gtNewSimdHWIntrinsicNode(TYP_SIMD8, NI_AdvSimd_FusedMultiplyAddScalar, callType, simdSize,
                                                       op3, op2, op1);

                    retNode = gtNewSimdHWIntrinsicNode(callType, NI_Vector64_ToScalar, callType, simdSize, retNode);
                    break;
                }
#endif

                // TODO-CQ-XArch: Ideally we would create a GT_INTRINSIC node for fma, however, that currently
                // requires more extensive changes to valuenum to support methods with 3 operands

                // We want to generate a GT_INTRINSIC node in the case the call can't be treated as
                // a target intrinsic so that we can still benefit from CSE and constant folding.

                break;
            }
#endif // FEATURE_HW_INTRINSICS

            case NI_System_Math_Abs:
            case NI_System_Math_Acos:
            case NI_System_Math_Acosh:
            case NI_System_Math_Asin:
            case NI_System_Math_Asinh:
            case NI_System_Math_Atan:
            case NI_System_Math_Atanh:
            case NI_System_Math_Atan2:
            case NI_System_Math_Cbrt:
            case NI_System_Math_Ceiling:
            case NI_System_Math_Cos:
            case NI_System_Math_Cosh:
            case NI_System_Math_Exp:
            case NI_System_Math_Floor:
            case NI_System_Math_FMod:
            case NI_System_Math_ILogB:
            case NI_System_Math_Log:
            case NI_System_Math_Log2:
            case NI_System_Math_Log10:
            case NI_System_Math_Pow:
            case NI_System_Math_Round:
            case NI_System_Math_Sin:
            case NI_System_Math_Sinh:
            case NI_System_Math_Sqrt:
            case NI_System_Math_Tan:
            case NI_System_Math_Tanh:
            {
                retNode = impMathIntrinsic(method, sig, callType, ni, tailCall);
                break;
            }

            case NI_System_Collections_Generic_Comparer_get_Default:
            case NI_System_Collections_Generic_EqualityComparer_get_Default:
            {
                // Flag for later handling during devirtualization.
                isSpecial = true;
                break;
            }

            case NI_System_Buffers_Binary_BinaryPrimitives_ReverseEndianness:
            {
                assert(sig->numArgs == 1);

                // We expect the return type of the ReverseEndianness routine to match the type of the
                // one and only argument to the method. We use a special instruction for 16-bit
                // BSWAPs since on x86 processors this is implemented as ROR <16-bit reg>, 8. Additionally,
                // we only emit 64-bit BSWAP instructions on 64-bit archs; if we're asked to perform a
                // 64-bit byte swap on a 32-bit arch, we'll fall to the default case in the switch block below.

                switch (sig->retType)
                {
                    case CorInfoType::CORINFO_TYPE_SHORT:
                    case CorInfoType::CORINFO_TYPE_USHORT:
                        retNode = gtNewCastNode(TYP_INT, gtNewOperNode(GT_BSWAP16, TYP_INT, impPopStack().val), false,
                                                callType);
                        break;

                    case CorInfoType::CORINFO_TYPE_INT:
                    case CorInfoType::CORINFO_TYPE_UINT:
#ifdef TARGET_64BIT
                    case CorInfoType::CORINFO_TYPE_LONG:
                    case CorInfoType::CORINFO_TYPE_ULONG:
#endif // TARGET_64BIT
                        retNode = gtNewOperNode(GT_BSWAP, callType, impPopStack().val);
                        break;

                    default:
                        // This default case gets hit on 32-bit archs when a call to a 64-bit overload
                        // of ReverseEndianness is encountered. In that case we'll let JIT treat this as a standard
                        // method call, where the implementation decomposes the operation into two 32-bit
                        // bswap routines. If the input to the 64-bit function is a constant, then we rely
                        // on inlining + constant folding of 32-bit bswaps to effectively constant fold
                        // the 64-bit call site.
                        break;
                }

                break;
            }

            // Fold PopCount for constant input
            case NI_System_Numerics_BitOperations_PopCount:
                assert(sig->numArgs == 1);
                if (impStackTop().val->IsIntegralConst())
                {
                    CORINFO_CLASS_HANDLE argClass;
                    CorInfoType          corArgType = strip(info.compCompHnd->getArgType(sig, sig->args, &argClass));

                    var_types argType = JITtype2varType(corArgType);
                    ssize_t   cns     = impPopStack().val->AsIntConCommon()->IconValue();
                    if (argType == TYP_LONG)
                    {
                        retNode = gtNewIconNode(genCountBits(cns), callType);
                    }
                    else
                    {
                        assert(argType == TYP_INT);
                        retNode = gtNewIconNode(genCountBits(static_cast<unsigned>(cns)), callType);
                    }
                }
                break;

            case NI_System_GC_KeepAlive:
            {
                retNode = gtNewOperNode(GT_KEEPALIVE, TYP_VOID, impPopStack().val);

                // Prevent both reordering and removal. Invalid optimizations of GC.KeepAlive are
                // very subtle and hard to observe. Thus we are conservatively marking it with both
                // GTF_CALL and GTF_GLOB_REF side-effects even though it may be more than strictly
                // necessary. The conservative side-effects are unlikely to have negative impact
                // on code quality in this case.
                retNode->gtFlags |= (GTF_CALL | GTF_GLOB_REF);

                break;
            }

            default:
                break;
        }
    }

    if (mustExpand && (retNode == nullptr))
    {
        assert(!"Unhandled must expand intrinsic, throwing PlatformNotSupportedException");
        return impUnsupportedNamedIntrinsic(CORINFO_HELP_THROW_PLATFORM_NOT_SUPPORTED, method, sig, mustExpand);
    }

    // Optionally report if this intrinsic is special
    // (that is, potentially re-optimizable during morph).
    if (isSpecialIntrinsic != nullptr)
    {
        *isSpecialIntrinsic = isSpecial;
    }

    return retNode;
}

GenTree* Compiler::impTypeIsAssignable(GenTree* typeTo, GenTree* typeFrom)
{
    // Optimize patterns like:
    //
    //   typeof(TTo).IsAssignableFrom(typeof(TTFrom))
    //   valueTypeVar.GetType().IsAssignableFrom(typeof(TTFrom))
    //   typeof(TTFrom).IsAssignableTo(typeof(TTo))
    //   typeof(TTFrom).IsAssignableTo(valueTypeVar.GetType())
    //
    // to true/false

    if (typeTo->IsCall() && typeFrom->IsCall())
    {
        // make sure both arguments are `typeof()`
        CORINFO_METHOD_HANDLE hTypeof = eeFindHelper(CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE);
        if ((typeTo->AsCall()->gtCallMethHnd == hTypeof) && (typeFrom->AsCall()->gtCallMethHnd == hTypeof))
        {
            CORINFO_CLASS_HANDLE hClassTo   = gtGetHelperArgClassHandle(typeTo->AsCall()->gtCallArgs->GetNode());
            CORINFO_CLASS_HANDLE hClassFrom = gtGetHelperArgClassHandle(typeFrom->AsCall()->gtCallArgs->GetNode());

            if (hClassTo == NO_CLASS_HANDLE || hClassFrom == NO_CLASS_HANDLE)
            {
                return nullptr;
            }

            TypeCompareState castResult = info.compCompHnd->compareTypesForCast(hClassFrom, hClassTo);
            if (castResult == TypeCompareState::May)
            {
                // requires runtime check
                // e.g. __Canon, COMObjects, Nullable
                return nullptr;
            }

            GenTreeIntCon* retNode = gtNewIconNode((castResult == TypeCompareState::Must) ? 1 : 0);
            impPopStack(); // drop both CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE calls
            impPopStack();

            return retNode;
        }
    }

    return nullptr;
}

GenTree* Compiler::impMathIntrinsic(CORINFO_METHOD_HANDLE method,
                                    CORINFO_SIG_INFO*     sig,
                                    var_types             callType,
                                    NamedIntrinsic        intrinsicName,
                                    bool                  tailCall)
{
    GenTree* op1;
    GenTree* op2;

    assert(callType != TYP_STRUCT);
    assert(IsMathIntrinsic(intrinsicName));

    op1 = nullptr;

#if !defined(TARGET_X86)
    // Intrinsics that are not implemented directly by target instructions will
    // be re-materialized as users calls in rationalizer. For prefixed tail calls,
    // don't do this optimization, because
    //  a) For back compatibility reasons on desktop .NET Framework 4.6 / 4.6.1
    //  b) It will be non-trivial task or too late to re-materialize a surviving
    //     tail prefixed GT_INTRINSIC as tail call in rationalizer.
    if (!IsIntrinsicImplementedByUserCall(intrinsicName) || !tailCall)
#else
    // On x86 RyuJIT, importing intrinsics that are implemented as user calls can cause incorrect calculation
    // of the depth of the stack if these intrinsics are used as arguments to another call. This causes bad
    // code generation for certain EH constructs.
    if (!IsIntrinsicImplementedByUserCall(intrinsicName))
#endif
    {
        CORINFO_CLASS_HANDLE    tmpClass;
        CORINFO_ARG_LIST_HANDLE arg;
        var_types               op1Type;
        var_types               op2Type;

        switch (sig->numArgs)
        {
            case 1:
                op1 = impPopStack().val;

                arg     = sig->args;
                op1Type = JITtype2varType(strip(info.compCompHnd->getArgType(sig, arg, &tmpClass)));

                if (op1->TypeGet() != genActualType(op1Type))
                {
                    assert(varTypeIsFloating(op1));
                    op1 = gtNewCastNode(callType, op1, false, callType);
                }

                op1 = new (this, GT_INTRINSIC)
                    GenTreeIntrinsic(genActualType(callType), op1, CORINFO_INTRINSIC_Illegal, intrinsicName, method);
                break;

            case 2:
                op2 = impPopStack().val;
                op1 = impPopStack().val;

                arg     = sig->args;
                op1Type = JITtype2varType(strip(info.compCompHnd->getArgType(sig, arg, &tmpClass)));

                if (op1->TypeGet() != genActualType(op1Type))
                {
                    assert(varTypeIsFloating(op1));
                    op1 = gtNewCastNode(callType, op1, false, callType);
                }

                arg     = info.compCompHnd->getArgNext(arg);
                op2Type = JITtype2varType(strip(info.compCompHnd->getArgType(sig, arg, &tmpClass)));

                if (op2->TypeGet() != genActualType(op2Type))
                {
                    assert(varTypeIsFloating(op2));
                    op2 = gtNewCastNode(callType, op2, false, callType);
                }

                op1 = new (this, GT_INTRINSIC) GenTreeIntrinsic(genActualType(callType), op1, op2,
                                                                CORINFO_INTRINSIC_Illegal, intrinsicName, method);
                break;

            default:
                NO_WAY("Unsupported number of args for Math Intrinsic");
        }

        if (IsIntrinsicImplementedByUserCall(intrinsicName))
        {
            op1->gtFlags |= GTF_CALL;
        }
    }

    return op1;
}

//------------------------------------------------------------------------
// lookupNamedIntrinsic: map method to jit named intrinsic value
//
// Arguments:
//    method -- method handle for method
//
// Return Value:
//    Id for the named intrinsic, or Illegal if none.
//
// Notes:
//    method should have CORINFO_FLG_JIT_INTRINSIC set in its attributes,
//    otherwise it is not a named jit intrinsic.
//

NamedIntrinsic Compiler::lookupNamedIntrinsic(CORINFO_METHOD_HANDLE method)
{
    const char* className          = nullptr;
    const char* namespaceName      = nullptr;
    const char* enclosingClassName = nullptr;
    const char* methodName =
        info.compCompHnd->getMethodNameFromMetadata(method, &className, &namespaceName, &enclosingClassName);

    JITDUMP("Named Intrinsic ");

    if (namespaceName != nullptr)
    {
        JITDUMP("%s.", namespaceName);
    }
    if (enclosingClassName != nullptr)
    {
        JITDUMP("%s.", enclosingClassName);
    }
    if (className != nullptr)
    {
        JITDUMP("%s.", className);
    }
    if (methodName != nullptr)
    {
        JITDUMP("%s", methodName);
    }
    JITDUMP(": ");

    if ((namespaceName == nullptr) || (className == nullptr) || (methodName == nullptr))
    {
        JITDUMP("Not recognized, not enough metadata\n");
        return NI_Illegal;
    }

    NamedIntrinsic result = NI_Illegal;

    if (strcmp(namespaceName, "System") == 0)
    {
        if ((strcmp(className, "Enum") == 0) && (strcmp(methodName, "HasFlag") == 0))
        {
            result = NI_System_Enum_HasFlag;
        }
        else if (strcmp(className, "Math") == 0 || strcmp(className, "MathF") == 0)
        {
            if (strcmp(methodName, "Abs") == 0)
            {
                result = NI_System_Math_Abs;
            }
            else if (strcmp(methodName, "Acos") == 0)
            {
                result = NI_System_Math_Acos;
            }
            else if (strcmp(methodName, "Acosh") == 0)
            {
                result = NI_System_Math_Acosh;
            }
            else if (strcmp(methodName, "Asin") == 0)
            {
                result = NI_System_Math_Asin;
            }
            else if (strcmp(methodName, "Asinh") == 0)
            {
                result = NI_System_Math_Asinh;
            }
            else if (strcmp(methodName, "Atan") == 0)
            {
                result = NI_System_Math_Atan;
            }
            else if (strcmp(methodName, "Atanh") == 0)
            {
                result = NI_System_Math_Atanh;
            }
            else if (strcmp(methodName, "Atan2") == 0)
            {
                result = NI_System_Math_Atan2;
            }
            else if (strcmp(methodName, "Cbrt") == 0)
            {
                result = NI_System_Math_Cbrt;
            }
            else if (strcmp(methodName, "Ceiling") == 0)
            {
                result = NI_System_Math_Ceiling;
            }
            else if (strcmp(methodName, "Cos") == 0)
            {
                result = NI_System_Math_Cos;
            }
            else if (strcmp(methodName, "Cosh") == 0)
            {
                result = NI_System_Math_Cosh;
            }
            else if (strcmp(methodName, "Exp") == 0)
            {
                result = NI_System_Math_Exp;
            }
            else if (strcmp(methodName, "Floor") == 0)
            {
                result = NI_System_Math_Floor;
            }
            else if (strcmp(methodName, "FMod") == 0)
            {
                result = NI_System_Math_FMod;
            }
            else if (strcmp(methodName, "FusedMultiplyAdd") == 0)
            {
                result = NI_System_Math_FusedMultiplyAdd;
            }
            else if (strcmp(methodName, "ILogB") == 0)
            {
                result = NI_System_Math_ILogB;
            }
            else if (strcmp(methodName, "Log") == 0)
            {
                result = NI_System_Math_Log;
            }
            else if (strcmp(methodName, "Log2") == 0)
            {
                result = NI_System_Math_Log2;
            }
            else if (strcmp(methodName, "Log10") == 0)
            {
                result = NI_System_Math_Log10;
            }
            else if (strcmp(methodName, "Pow") == 0)
            {
                result = NI_System_Math_Pow;
            }
            else if (strcmp(methodName, "Round") == 0)
            {
                result = NI_System_Math_Round;
            }
            else if (strcmp(methodName, "Sin") == 0)
            {
                result = NI_System_Math_Sin;
            }
            else if (strcmp(methodName, "Sinh") == 0)
            {
                result = NI_System_Math_Sinh;
            }
            else if (strcmp(methodName, "Sqrt") == 0)
            {
                result = NI_System_Math_Sqrt;
            }
            else if (strcmp(methodName, "Tan") == 0)
            {
                result = NI_System_Math_Tan;
            }
            else if (strcmp(methodName, "Tanh") == 0)
            {
                result = NI_System_Math_Tanh;
            }
        }
        else if (strcmp(className, "GC") == 0)
        {
            if (strcmp(methodName, "KeepAlive") == 0)
            {
                result = NI_System_GC_KeepAlive;
            }
        }
        else if (strcmp(className, "Array") == 0)
        {
            if (strcmp(methodName, "Clone") == 0)
            {
                result = NI_System_Array_Clone;
            }
        }
        else if (strcmp(className, "Object") == 0)
        {
            if (strcmp(methodName, "MemberwiseClone") == 0)
            {
                result = NI_System_Object_MemberwiseClone;
            }
        }
        else if (strcmp(className, "Type") == 0)
        {
            if (strcmp(methodName, "get_IsValueType") == 0)
            {
                result = NI_System_Type_get_IsValueType;
            }
            else if (strcmp(methodName, "IsAssignableFrom") == 0)
            {
                result = NI_System_Type_IsAssignableFrom;
            }
            else if (strcmp(methodName, "IsAssignableTo") == 0)
            {
                result = NI_System_Type_IsAssignableTo;
            }
        }
    }
    else if (strcmp(namespaceName, "System.Threading") == 0)
    {
        if (strcmp(className, "Thread") == 0)
        {
            if (strcmp(methodName, "get_CurrentThread") == 0)
            {
                result = NI_System_Threading_Thread_get_CurrentThread;
            }
            else if (strcmp(methodName, "get_ManagedThreadId") == 0)
            {
                result = NI_System_Threading_Thread_get_ManagedThreadId;
            }
        }
#ifndef TARGET_ARM64
        // TODO-CQ: Implement for XArch (https://github.com/dotnet/runtime/issues/32239).
        else if (strcmp(className, "Interlocked") == 0)
        {
            if (strcmp(methodName, "And") == 0)
            {
                result = NI_System_Threading_Interlocked_And;
            }
            else if (strcmp(methodName, "Or") == 0)
            {
                result = NI_System_Threading_Interlocked_Or;
            }
        }
#endif
    }
#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
    else if (strcmp(namespaceName, "System.Buffers.Binary") == 0)
    {
        if ((strcmp(className, "BinaryPrimitives") == 0) && (strcmp(methodName, "ReverseEndianness") == 0))
        {
            result = NI_System_Buffers_Binary_BinaryPrimitives_ReverseEndianness;
        }
    }
#endif // defined(TARGET_XARCH) || defined(TARGET_ARM64)
    else if (strcmp(namespaceName, "System.Collections.Generic") == 0)
    {
        if ((strcmp(className, "EqualityComparer`1") == 0) && (strcmp(methodName, "get_Default") == 0))
        {
            result = NI_System_Collections_Generic_EqualityComparer_get_Default;
        }
        else if ((strcmp(className, "Comparer`1") == 0) && (strcmp(methodName, "get_Default") == 0))
        {
            result = NI_System_Collections_Generic_Comparer_get_Default;
        }
    }
    else if ((strcmp(namespaceName, "System.Numerics") == 0) && (strcmp(className, "BitOperations") == 0))
    {
        if (strcmp(methodName, "PopCount") == 0)
        {
            result = NI_System_Numerics_BitOperations_PopCount;
        }
    }
#ifdef FEATURE_HW_INTRINSICS
    else if (strcmp(namespaceName, "System.Numerics") == 0)
    {
        CORINFO_SIG_INFO sig;
        info.compCompHnd->getMethodSig(method, &sig);

        int sizeOfVectorT = getSIMDVectorRegisterByteLength();

        result = SimdAsHWIntrinsicInfo::lookupId(&sig, className, methodName, enclosingClassName, sizeOfVectorT);
    }
#endif // FEATURE_HW_INTRINSICS
    else if (strncmp(namespaceName, "System.Runtime.Intrinsics", 25) == 0)
    {
        // We go down this path even when FEATURE_HW_INTRINSICS isn't enabled
        // so we can specially handle IsSupported and recursive calls.

        // This is required to appropriately handle the intrinsics on platforms
        // which don't support them. On such a platform methods like Vector64.Create
        // will be seen as `Intrinsic` and `mustExpand` due to having a code path
        // which is recursive. When such a path is hit we expect it to be handled by
        // the importer and we fire an assert if it wasn't and in previous versions
        // of the JIT would fail fast. This was changed to throw a PNSE instead but
        // we still assert as most intrinsics should have been recognized/handled.

        // In order to avoid the assert, we specially handle the IsSupported checks
        // (to better allow dead-code optimizations) and we explicitly throw a PNSE
        // as we know that is the desired behavior for the HWIntrinsics when not
        // supported. For cases like Vector64.Create, this is fine because it will
        // be behind a relevant IsSupported check and will never be hit and the
        // software fallback will be executed instead.

        CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef FEATURE_HW_INTRINSICS
        namespaceName += 25;
        const char* platformNamespaceName;

#if defined(TARGET_XARCH)
        platformNamespaceName = ".X86";
#elif defined(TARGET_ARM64)
        platformNamespaceName = ".Arm";
#else
#error Unsupported platform
#endif

        if ((namespaceName[0] == '\0') || (strcmp(namespaceName, platformNamespaceName) == 0))
        {
            CORINFO_SIG_INFO sig;
            info.compCompHnd->getMethodSig(method, &sig);

            result = HWIntrinsicInfo::lookupId(this, &sig, className, methodName, enclosingClassName);
        }
#endif // FEATURE_HW_INTRINSICS

        if (result == NI_Illegal)
        {
            if (strcmp(methodName, "get_IsSupported") == 0)
            {
                // This allows the relevant code paths to be dropped as dead code even
                // on platforms where FEATURE_HW_INTRINSICS is not supported.

                result = NI_IsSupported_False;
            }
            else if (gtIsRecursiveCall(method))
            {
                // For the framework itself, any recursive intrinsics will either be
                // only supported on a single platform or will be guarded by a relevant
                // IsSupported check so the throw PNSE will be valid or dropped.

                result = NI_Throw_PlatformNotSupportedException;
            }
        }
    }

    if (result == NI_Illegal)
    {
        JITDUMP("Not recognized\n");
    }
    else if (result == NI_IsSupported_False)
    {
        JITDUMP("Unsupported - return false");
    }
    else if (result == NI_Throw_PlatformNotSupportedException)
    {
        JITDUMP("Unsupported - throw PlatformNotSupportedException");
    }
    else
    {
        JITDUMP("Recognized\n");
    }
    return result;
}

//------------------------------------------------------------------------
// impUnsupportedNamedIntrinsic: Throws an exception for an unsupported named intrinsic
//
// Arguments:
//    helper     - JIT helper ID for the exception to be thrown
//    method     - method handle of the intrinsic function.
//    sig        - signature of the intrinsic call
//    mustExpand - true if the intrinsic must return a GenTree*; otherwise, false
//
// Return Value:
//    a gtNewMustThrowException if mustExpand is true; otherwise, nullptr
//
GenTree* Compiler::impUnsupportedNamedIntrinsic(unsigned              helper,
                                                CORINFO_METHOD_HANDLE method,
                                                CORINFO_SIG_INFO*     sig,
                                                bool                  mustExpand)
{
    // We've hit some error case and may need to return a node for the given error.
    //
    // When `mustExpand=false`, we are attempting to inline the intrinsic directly into another method. In this
    // scenario, we need to return `nullptr` so that a GT_CALL to the intrinsic is emitted instead. This is to
    // ensure that everything continues to behave correctly when optimizations are enabled (e.g. things like the
    // inliner may expect the node we return to have a certain signature, and the `MustThrowException` node won't
    // match that).
    //
    // When `mustExpand=true`, we are in a GT_CALL to the intrinsic and are attempting to JIT it. This will generally
    // be in response to an indirect call (e.g. done via reflection) or in response to an earlier attempt returning
    // `nullptr` (under `mustExpand=false`). In that scenario, we are safe to return the `MustThrowException` node.

    if (mustExpand)
    {
        for (unsigned i = 0; i < sig->numArgs; i++)
        {
            impPopStack();
        }

        return gtNewMustThrowException(helper, JITtype2varType(sig->retType), sig->retTypeClass);
    }
    else
    {
        return nullptr;
    }
}

/*****************************************************************************/

GenTree* Compiler::impArrayAccessIntrinsic(
    CORINFO_CLASS_HANDLE clsHnd, CORINFO_SIG_INFO* sig, int memberRef, bool readonlyCall, CorInfoIntrinsics intrinsicID)
{
    /* If we are generating SMALL_CODE, we don't want to use intrinsics for
       the following, as it generates fatter code.
    */

    if (compCodeOpt() == SMALL_CODE)
    {
        return nullptr;
    }

    /* These intrinsics generate fatter (but faster) code and are only
       done if we don't need SMALL_CODE */

    unsigned rank = (intrinsicID == CORINFO_INTRINSIC_Array_Set) ? (sig->numArgs - 1) : sig->numArgs;

    // The rank 1 case is special because it has to handle two array formats
    // we will simply not do that case
    if (rank > GT_ARR_MAX_RANK || rank <= 1)
    {
        return nullptr;
    }

    CORINFO_CLASS_HANDLE elemClsHnd = nullptr;
    var_types            elemType   = JITtype2varType(info.compCompHnd->getChildType(clsHnd, &elemClsHnd));

    // For the ref case, we will only be able to inline if the types match
    // and the type is final (so we don't need to do the cast).
    if ((intrinsicID != CORINFO_INTRINSIC_Array_Get) && !readonlyCall && (elemType == TYP_REF))
    {
        CORINFO_SIG_INFO callSig;
        eeGetCallSiteSig(memberRef, info.compScopeHnd, impTokenLookupContextHandle, &callSig);
        assert(callSig.hasThis());

        CORINFO_CLASS_HANDLE accessClsHnd;

        if (intrinsicID == CORINFO_INTRINSIC_Array_Set)
        {
            // Fetch the last argument, the one that indicates the type we are setting.
            CORINFO_ARG_LIST_HANDLE arg = callSig.args;
            for (unsigned r = 0; r < rank; r++)
            {
                arg = info.compCompHnd->getArgNext(arg);
            }

            assert(strip(info.compCompHnd->getArgType(&callSig, arg, &accessClsHnd)) == CORINFO_TYPE_CLASS);

            accessClsHnd = info.compCompHnd->getArgClass(&callSig, arg);
        }
        else
        {
            assert(intrinsicID == CORINFO_INTRINSIC_Array_Address);
            assert(callSig.retType == CORINFO_TYPE_BYREF);

            info.compCompHnd->getChildType(callSig.retTypeClass, &accessClsHnd);
        }

        // if it's not final, we can't do the optimization
        if ((info.compCompHnd->getClassAttribs(accessClsHnd) & CORINFO_FLG_FINAL) == 0)
        {
            return nullptr;
        }
    }

    unsigned elemSize;
    if (elemType == TYP_STRUCT)
    {
        assert(elemClsHnd != NO_CLASS_HANDLE);

        elemSize = info.compCompHnd->getClassSize(elemClsHnd);
    }
    else
    {
        elemSize = genTypeSize(elemType);
    }

    if (static_cast<unsigned char>(elemSize) != elemSize)
    {
        // elemSize would be truncated as an unsigned char.
        // This means the array element is too large. Don't do the optimization.
        return nullptr;
    }

    GenTree* val = nullptr;

    if (intrinsicID == CORINFO_INTRINSIC_Array_Set)
    {
        // Assignment of a struct is more work, and there are more gets than sets.
        if (elemType == TYP_STRUCT)
        {
            return nullptr;
        }

        val = impPopStack().val;
        assert(genActualType(elemType) == genActualType(val->gtType) ||
               (elemType == TYP_FLOAT && val->gtType == TYP_DOUBLE) ||
               (elemType == TYP_INT && val->gtType == TYP_BYREF) ||
               (elemType == TYP_DOUBLE && val->gtType == TYP_FLOAT));
    }

    noway_assert((unsigned char)GT_ARR_MAX_RANK == GT_ARR_MAX_RANK);

    GenTree* inds[GT_ARR_MAX_RANK];
    for (unsigned k = rank; k > 0; k--)
    {
        inds[k - 1] = impPopStack().val;
    }

    GenTree* arr = impPopStack().val;
    assert(arr->gtType == TYP_REF);

    GenTree* arrElem = new (this, GT_ARR_ELEM) GenTreeArrElem(TYP_BYREF, arr, static_cast<unsigned char>(rank),
                                                              static_cast<unsigned char>(elemSize), elemType, &inds[0]);

    if (intrinsicID != CORINFO_INTRINSIC_Array_Address)
    {
        if (varTypeIsStruct(elemType))
        {
            arrElem = gtNewObjNode(sig->retTypeClass, arrElem);
        }
        else
        {
            arrElem = gtNewOperNode(GT_IND, elemType, arrElem);
        }
    }

    if (intrinsicID == CORINFO_INTRINSIC_Array_Set)
    {
        assert(val != nullptr);
        return gtNewAssignNode(arrElem, val);
    }
    else
    {
        return arrElem;
    }
}

#ifdef DEBUG

bool Compiler::verCheckTailCallConstraint(OPCODE                  opcode,
                                          CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                          CORINFO_RESOLVED_TOKEN* pConstrainedResolvedToken // Is this a "constrained."
                                                                                            // call on a type parameter?
                                          )
{
    DWORD            mflags;
    CORINFO_SIG_INFO sig;
    unsigned int     popCount = 0; // we can't pop the stack since impImportCall needs it, so
                                   // this counter is used to keep track of how many items have been
                                   // virtually popped

    CORINFO_METHOD_HANDLE methodHnd       = nullptr;
    CORINFO_CLASS_HANDLE  methodClassHnd  = nullptr;
    unsigned              methodClassFlgs = 0;

    assert(impOpcodeIsCallOpcode(opcode));

    if (compIsForInlining())
    {
        return false;
    }

    // for calli, VerifyOrReturn that this is not a virtual method
    if (opcode == CEE_CALLI)
    {
        /* Get the call sig */
        eeGetSig(pResolvedToken->token, pResolvedToken->tokenScope, pResolvedToken->tokenContext, &sig);

        // We don't know the target method, so we have to infer the flags, or
        // assume the worst-case.
        mflags = (sig.callConv & CORINFO_CALLCONV_HASTHIS) ? 0 : CORINFO_FLG_STATIC;
    }
    else
    {
        methodHnd = pResolvedToken->hMethod;

        mflags = info.compCompHnd->getMethodAttribs(methodHnd);

        // When verifying generic code we pair the method handle with its
        // owning class to get the exact method signature.
        methodClassHnd = pResolvedToken->hClass;
        assert(methodClassHnd);

        eeGetMethodSig(methodHnd, &sig, methodClassHnd);

        // opcode specific check
        methodClassFlgs = info.compCompHnd->getClassAttribs(methodClassHnd);
    }

    // We must have got the methodClassHnd if opcode is not CEE_CALLI
    assert((methodHnd != nullptr && methodClassHnd != nullptr) || opcode == CEE_CALLI);

    if ((sig.callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_VARARG)
    {
        eeGetCallSiteSig(pResolvedToken->token, pResolvedToken->tokenScope, pResolvedToken->tokenContext, &sig);
    }

    // check compatibility of the arguments
    CORINFO_ARG_LIST_HANDLE args = sig.args;

    for (unsigned i = 0; i < sig.numArgs; i++, args = info.compCompHnd->getArgNext(args))
    {
        // For unsafe code, we might have parameters containing pointer to the stack location.
        // Disallow the tailcall for this kind.
        CORINFO_CLASS_HANDLE argClass;
        CorInfoType          argCorType = strip(info.compCompHnd->getArgType(&sig, args, &argClass));

        if ((argCorType == CORINFO_TYPE_PTR) || (argCorType == CORINFO_TYPE_BYREF) ||
            ((argCorType == CORINFO_TYPE_VALUECLASS) &&
             ((info.compCompHnd->getClassAttribs(argClass) & CORINFO_FLG_CONTAINS_STACK_PTR) != 0)))
        {
            return false;
        }
    }

    // update popCount
    popCount += sig.numArgs;

    // check for 'this' which is on non-static methods, not called via NEWOBJ
    if ((mflags & CORINFO_FLG_STATIC) == 0)
    {
        typeInfo tiThis;

        if (opcode == CEE_CALLI)
        {
            // For CALLI, we don't know the methodClassHnd. Therefore, let's check the "this" arg on
            // the stack. If it's not REF then it must be BYREF/I_IMPL and could point to a local.
            if (!impStackTop(popCount).val->TypeIs(TYP_REF))
            {
                return false;
            }
        }
        else
        {
            // If it's a value class then it may be a local so we can't tailcall.
            if (info.compCompHnd->isValueClass(methodClassHnd))
            {
                return false;
            }
        }

        popCount++;
    }

    // Tail calls on constrained calls should be illegal too:
    // when instantiated at a value type, a constrained call may pass the address of a stack allocated value
    if (pConstrainedResolvedToken)
    {
        return false;
    }

    // void return type gets morphed into the error type, so we have to treat them specially here
    if (sig.retType == CORINFO_TYPE_VOID)
    {
        if (info.compMethodInfo->args.retType != CORINFO_TYPE_VOID)
        {
            return false;
        }
    }
    else
    {
        // Get the exact view of the signature for an array method
        if ((methodClassFlgs & CORINFO_FLG_ARRAY) != 0)
        {
            assert(opcode != CEE_CALLI);
            eeGetCallSiteSig(pResolvedToken->token, pResolvedToken->tokenScope, pResolvedToken->tokenContext, &sig);
        }

        typeInfo tiCalleeRetType = verMakeTypeInfo(sig.retType, sig.retTypeClass);
        typeInfo tiCallerRetType =
            verMakeTypeInfo(info.compMethodInfo->args.retType, info.compMethodInfo->args.retTypeClass);

        if (!tiCompatibleWith(tiCalleeRetType, tiCallerRetType))
        {
            return false;
        }
    }

    // for tailcall, stack must be empty
    return verCurrentState.esStackDepth == popCount;
}
#endif // DEBUG

GenTree* Compiler::impImportLdvirtftn(GenTree*                thisPtr,
                                      CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                      CORINFO_CALL_INFO*      pCallInfo)
{
    if ((pCallInfo->methodFlags & CORINFO_FLG_EnC) && !(pCallInfo->classFlags & CORINFO_FLG_INTERFACE))
    {
        NO_WAY("Virtual call to a function added via EnC is not supported");
    }

    // CoreRT generic virtual method
    if ((pCallInfo->sig.sigInst.methInstCount != 0) && IsTargetAbi(CORINFO_CORERT_ABI))
    {
        GenTree* runtimeMethodHandle =
            impLookupToTree(pResolvedToken, &pCallInfo->codePointerLookup, GTF_ICON_METHOD_HDL, pCallInfo->hMethod);
        return gtNewHelperCallNode(CORINFO_HELP_GVMLOOKUP_FOR_SLOT, TYP_I_IMPL,
                                   gtNewCallArgs(thisPtr, runtimeMethodHandle));
    }

#ifdef FEATURE_READYTORUN_COMPILER
    if (opts.IsReadyToRun())
    {
        if (!pCallInfo->exactContextNeedsRuntimeLookup)
        {
            GenTreeCall* call =
                gtNewHelperCallNode(CORINFO_HELP_READYTORUN_VIRTUAL_FUNC_PTR, TYP_I_IMPL, gtNewCallArgs(thisPtr));

            call->setEntryPoint(pCallInfo->codePointerLookup.constLookup);

            return call;
        }

        // We need a runtime lookup. CoreRT has a ReadyToRun helper for that too.
        if (IsTargetAbi(CORINFO_CORERT_ABI))
        {
            GenTree* ctxTree = getRuntimeContextTree(pCallInfo->codePointerLookup.lookupKind.runtimeLookupKind);

            return impReadyToRunHelperToTree(pResolvedToken, CORINFO_HELP_READYTORUN_GENERIC_HANDLE, TYP_I_IMPL,
                                             gtNewCallArgs(ctxTree), &pCallInfo->codePointerLookup.lookupKind);
        }
    }
#endif

    // Get the exact descriptor for the static callsite
    GenTree* exactTypeDesc = impParentClassTokenToHandle(pResolvedToken);
    if (exactTypeDesc == nullptr)
    { // compDonotInline()
        return nullptr;
    }

    GenTree* exactMethodDesc = impTokenToHandle(pResolvedToken);
    if (exactMethodDesc == nullptr)
    { // compDonotInline()
        return nullptr;
    }

    GenTreeCall::Use* helpArgs = gtNewCallArgs(exactMethodDesc);

    helpArgs = gtPrependNewCallArg(exactTypeDesc, helpArgs);

    helpArgs = gtPrependNewCallArg(thisPtr, helpArgs);

    // Call helper function.  This gets the target address of the final destination callsite.

    return gtNewHelperCallNode(CORINFO_HELP_VIRTUAL_FUNC_PTR, TYP_I_IMPL, helpArgs);
}

//------------------------------------------------------------------------
// impBoxPatternMatch: match and import common box idioms
//
// Arguments:
//   pResolvedToken - resolved token from the box operation
//   codeAddr - position in IL stream after the box instruction
//   codeEndp - end of IL stream
//
// Return Value:
//   Number of IL bytes matched and imported, -1 otherwise
//
// Notes:
//   pResolvedToken is known to be a value type; ref type boxing
//   is handled in the CEE_BOX clause.

int Compiler::impBoxPatternMatch(CORINFO_RESOLVED_TOKEN* pResolvedToken, const BYTE* codeAddr, const BYTE* codeEndp)
{
    if (codeAddr >= codeEndp)
    {
        return -1;
    }

    switch (codeAddr[0])
    {
        case CEE_UNBOX_ANY:
            // box + unbox.any
            if (codeAddr + 1 + sizeof(mdToken) <= codeEndp)
            {
                CORINFO_RESOLVED_TOKEN unboxResolvedToken;

                impResolveToken(codeAddr + 1, &unboxResolvedToken, CORINFO_TOKENKIND_Class);

                // See if the resolved tokens describe types that are equal.
                const TypeCompareState compare =
                    info.compCompHnd->compareTypesForEquality(unboxResolvedToken.hClass, pResolvedToken->hClass);

                // If so, box/unbox.any is a nop.
                if (compare == TypeCompareState::Must)
                {
                    JITDUMP("\n Importing BOX; UNBOX.ANY as NOP\n");
                    // Skip the next unbox.any instruction
                    return 1 + sizeof(mdToken);
                }
            }
            break;

        case CEE_BRTRUE:
        case CEE_BRTRUE_S:
        case CEE_BRFALSE:
        case CEE_BRFALSE_S:
            // box + br_true/false
            if ((codeAddr + ((codeAddr[0] >= CEE_BRFALSE) ? 5 : 2)) <= codeEndp)
            {
                if (info.compCompHnd->getBoxHelper(pResolvedToken->hClass) == CORINFO_HELP_BOX)
                {
                    GenTree* sideEffects = impImportPop(compCurBB);

                    if (sideEffects != nullptr)
                    {
                        impAppendTree(sideEffects, CHECK_SPILL_ALL, impCurStmtOffs);
                    }

                    impPushOnStack(gtNewIconNode(1), typeInfo());
                    return 0;
                }
            }
            break;

        case CEE_ISINST:
            if (codeAddr + 1 + sizeof(mdToken) + 1 <= codeEndp)
            {
                const BYTE* nextCodeAddr = codeAddr + 1 + sizeof(mdToken);

                switch (nextCodeAddr[0])
                {
                    // box + isinst + br_true/false
                    case CEE_BRTRUE:
                    case CEE_BRTRUE_S:
                    case CEE_BRFALSE:
                    case CEE_BRFALSE_S:
                        if ((nextCodeAddr + ((nextCodeAddr[0] >= CEE_BRFALSE) ? 5 : 2)) <= codeEndp)
                        {
                            if (!(impStackTop().val->gtFlags & GTF_SIDE_EFFECT))
                            {
                                CorInfoHelpFunc boxHelper = info.compCompHnd->getBoxHelper(pResolvedToken->hClass);
                                if (boxHelper == CORINFO_HELP_BOX)
                                {
                                    CORINFO_RESOLVED_TOKEN isInstResolvedToken;

                                    impResolveToken(codeAddr + 1, &isInstResolvedToken, CORINFO_TOKENKIND_Casting);

                                    TypeCompareState castResult =
                                        info.compCompHnd->compareTypesForCast(pResolvedToken->hClass,
                                                                              isInstResolvedToken.hClass);
                                    if (castResult != TypeCompareState::May)
                                    {
                                        JITDUMP("\n Importing BOX; ISINST; BR_TRUE/FALSE as constant\n");
                                        impPopStack();

                                        impPushOnStack(gtNewIconNode((castResult == TypeCompareState::Must) ? 1 : 0),
                                                       typeInfo());

                                        // Skip the next isinst instruction
                                        return 1 + sizeof(mdToken);
                                    }
                                }
                            }
                        }
                        break;

                    // box + isinst + unbox.any
                    case CEE_UNBOX_ANY:
                        if ((nextCodeAddr + 1 + sizeof(mdToken)) <= codeEndp)
                        {
                            // See if the resolved tokens in box, isinst and unbox.any describe types that are equal.
                            CORINFO_RESOLVED_TOKEN isinstResolvedToken = {};
                            impResolveToken(codeAddr + 1, &isinstResolvedToken, CORINFO_TOKENKIND_Class);

                            if (info.compCompHnd->compareTypesForEquality(isinstResolvedToken.hClass,
                                                                          pResolvedToken->hClass) ==
                                TypeCompareState::Must)
                            {
                                CORINFO_RESOLVED_TOKEN unboxResolvedToken = {};
                                impResolveToken(nextCodeAddr + 1, &unboxResolvedToken, CORINFO_TOKENKIND_Class);

                                // If so, box + isinst + unbox.any is a nop.
                                if (info.compCompHnd->compareTypesForEquality(unboxResolvedToken.hClass,
                                                                              pResolvedToken->hClass) ==
                                    TypeCompareState::Must)
                                {
                                    JITDUMP("\n Importing BOX; ISINST, UNBOX.ANY as NOP\n");
                                    return 2 + sizeof(mdToken) * 2;
                                }
                            }
                        }
                        break;
                }
            }
            break;

        default:
            break;
    }

    return -1;
}

//------------------------------------------------------------------------
// impImportAndPushBox: build and import a value-type box
//
// Arguments:
//   pResolvedToken - resolved token from the box operation
//
// Return Value:
//   None.
//
// Side Effects:
//   The value to be boxed is popped from the stack, and a tree for
//   the boxed value is pushed. This method may create upstream
//   statements, spill side effecting trees, and create new temps.
//
//   If importing an inlinee, we may also discover the inline must
//   fail. If so there is no new value pushed on the stack. Callers
//   should use CompDoNotInline after calling this method to see if
//   ongoing importation should be aborted.
//
// Notes:
//   Boxing of ref classes results in the same value as the value on
//   the top of the stack, so is handled inline in impImportBlockCode
//   for the CEE_BOX case. Only value or primitive type boxes make it
//   here.
//
//   Boxing for nullable types is done via a helper call; boxing
//   of other value types is expanded inline or handled via helper
//   call, depending on the jit's codegen mode.
//
//   When the jit is operating in size and time constrained modes,
//   using a helper call here can save jit time and code size. But it
//   also may inhibit cleanup optimizations that could have also had a
//   even greater benefit effect on code size and jit time. An optimal
//   strategy may need to peek ahead and see if it is easy to tell how
//   the box is being used. For now, we defer.

void Compiler::impImportAndPushBox(CORINFO_RESOLVED_TOKEN* pResolvedToken)
{
    // Spill any special side effects
    impSpillSpecialSideEff();

    // Get get the expression to box from the stack.
    GenTree*             op1       = nullptr;
    GenTree*             op2       = nullptr;
    StackEntry           se        = impPopStack();
    CORINFO_CLASS_HANDLE operCls   = se.seTypeInfo.GetClassHandle();
    GenTree*             exprToBox = se.val;

    // Look at what helper we should use.
    CorInfoHelpFunc boxHelper = info.compCompHnd->getBoxHelper(pResolvedToken->hClass);

    // Determine what expansion to prefer.
    //
    // In size/time/debuggable constrained modes, the helper call
    // expansion for box is generally smaller and is preferred, unless
    // the value to box is a struct that comes from a call. In that
    // case the call can construct its return value directly into the
    // box payload, saving possibly some up-front zeroing.
    //
    // Currently primitive type boxes always get inline expanded. We may
    // want to do the same for small structs if they don't come from
    // calls and don't have GC pointers, since explicitly copying such
    // structs is cheap.
    JITDUMP("\nCompiler::impImportAndPushBox -- handling BOX(value class) via");
    bool canExpandInline = (boxHelper == CORINFO_HELP_BOX);
    bool optForSize      = !exprToBox->IsCall() && (operCls != nullptr) && opts.OptimizationDisabled();
    bool expandInline    = canExpandInline && !optForSize;

    if (expandInline)
    {
        JITDUMP(" inline allocate/copy sequence\n");

        // we are doing 'normal' boxing.  This means that we can inline the box operation
        // Box(expr) gets morphed into
        // temp = new(clsHnd)
        // cpobj(temp+4, expr, clsHnd)
        // push temp
        // The code paths differ slightly below for structs and primitives because
        // "cpobj" differs in these cases.  In one case you get
        //    impAssignStructPtr(temp+4, expr, clsHnd)
        // and the other you get
        //    *(temp+4) = expr

        if (opts.OptimizationDisabled())
        {
            // For minopts/debug code, try and minimize the total number
            // of box temps by reusing an existing temp when possible.
            if (impBoxTempInUse || (impBoxTemp == BAD_VAR_NUM))
            {
                impBoxTemp = lvaNewTemp(TYP_REF, true DEBUGARG("Reusable Box Helper"));
            }
        }
        else
        {
            // When optimizing, use a new temp for each box operation
            // since we then know the exact class of the box temp.
            impBoxTemp                          = lvaNewTemp(TYP_REF, true DEBUGARG("Single-def Box Helper"));
            lvaGetDesc(impBoxTemp)->lvSingleDef = 1;
            JITDUMP("Marking V%02u as a single def local\n", impBoxTemp);
            lvaSetClass(impBoxTemp, pResolvedToken->hClass, /* isExact */ true);
        }

        // needs to stay in use until this box expression is appended
        // some other node.  We approximate this by keeping it alive until
        // the opcode stack becomes empty
        impBoxTempInUse = true;

        op1 = gtNewAllocObjNode(pResolvedToken, /* useParent */ false);
        if (op1 == nullptr)
        {
            return;
        }

        // Remember that this basic block contains 'new' of an object, and so does this method
        compCurBB->bbFlags |= BBF_HAS_NEWOBJ;
        optMethodFlags |= OMF_HAS_NEWOBJ;

        GenTree*   asg     = gtNewAssignNode(gtNewLclvNode(impBoxTemp, TYP_REF), op1);
        Statement* asgStmt = impAppendTree(asg, CHECK_SPILL_NONE, impCurStmtOffs);

        op1 = gtNewLclvNode(impBoxTemp, TYP_REF);
        op2 = gtNewIconNode(TARGET_POINTER_SIZE, TYP_I_IMPL);
        op1 = gtNewOperNode(GT_ADD, TYP_BYREF, op1, op2);

        if (varTypeIsStruct(exprToBox))
        {
            assert(info.compCompHnd->getClassSize(pResolvedToken->hClass) == info.compCompHnd->getClassSize(operCls));
            op1 = impAssignStructPtr(op1, exprToBox, operCls, (unsigned)CHECK_SPILL_ALL);
        }
        else
        {
            var_types lclTyp = exprToBox->TypeGet();
            if (lclTyp == TYP_BYREF)
            {
                lclTyp = TYP_I_IMPL;
            }
            CorInfoType jitType = info.compCompHnd->asCorInfoType(pResolvedToken->hClass);
            if (impIsPrimitive(jitType))
            {
                lclTyp = JITtype2varType(jitType);
            }
            assert(genActualType(exprToBox->TypeGet()) == genActualType(lclTyp) ||
                   varTypeIsFloating(lclTyp) == varTypeIsFloating(exprToBox->TypeGet()));
            var_types srcTyp = exprToBox->TypeGet();
            var_types dstTyp = lclTyp;

            if (srcTyp != dstTyp)
            {
                assert((varTypeIsFloating(srcTyp) && varTypeIsFloating(dstTyp)) ||
                       (varTypeIsIntegral(srcTyp) && varTypeIsIntegral(dstTyp)));
                exprToBox = gtNewCastNode(dstTyp, exprToBox, false, dstTyp);
            }
            op1 = gtNewAssignNode(gtNewOperNode(GT_IND, lclTyp, op1), exprToBox);
        }

        // Spill eval stack to flush out any pending side effects.
        impSpillSideEffects(true, (unsigned)CHECK_SPILL_ALL DEBUGARG("impImportAndPushBox"));

        // Set up this copy as a second assignment.
        Statement* copyStmt = impAppendTree(op1, (unsigned)CHECK_SPILL_NONE, impCurStmtOffs);

        op1 = gtNewLclvNode(impBoxTemp, TYP_REF);

        // Record that this is a "box" node and keep track of the matching parts.
        op1 = new (this, GT_BOX) GenTreeBox(TYP_REF, op1, asgStmt, copyStmt);

        // If it is a value class, mark the "box" node.  We can use this information
        // to optimise several cases:
        //    "box(x) == null" --> false
        //    "(box(x)).CallAnInterfaceMethod(...)" --> "(&x).CallAValueTypeMethod"
        //    "(box(x)).CallAnObjectMethod(...)" --> "(&x).CallAValueTypeMethod"

        assert(asg->gtOper == GT_ASG);
    }
    else
    {
        // Don't optimize, just call the helper and be done with it.
        JITDUMP(" helper call because: %s\n", canExpandInline ? "optimizing for size" : "nullable");
        assert(operCls != nullptr);

        // Ensure that the value class is restored
        op2 = impTokenToHandle(pResolvedToken, nullptr, TRUE /* mustRestoreHandle */);
        if (op2 == nullptr)
        {
            // We must be backing out of an inline.
            assert(compDonotInline());
            return;
        }

        GenTree* addr = impGetStructAddr(exprToBox, operCls, CHECK_SPILL_ALL, true);

        op1 = gtNewHelperCallNode(boxHelper, TYP_REF, gtNewCallArgs(op2, addr));
    }

    // Push the result back on the stack, even if clsHnd is a value class we want the TI_REF
    impPushOnStack(op1, typeInfo(TI_REF, info.compCompHnd->getTypeForBox(pResolvedToken->hClass)));
}

//------------------------------------------------------------------------
// impImportNewObjArray: Build and import `new` of multi-dimmensional array
//
// Arguments:
//    pResolvedToken - The CORINFO_RESOLVED_TOKEN that has been initialized
//                     by a call to CEEInfo::resolveToken().
//    pCallInfo - The CORINFO_CALL_INFO that has been initialized
//                by a call to CEEInfo::getCallInfo().
//
// Assumptions:
//    The multi-dimensional array constructor arguments (array dimensions) are
//    pushed on the IL stack on entry to this method.
//
// Notes:
//    Multi-dimensional array constructors are imported as calls to a JIT
//    helper, not as regular calls.

void Compiler::impImportNewObjArray(CORINFO_RESOLVED_TOKEN* pResolvedToken, CORINFO_CALL_INFO* pCallInfo)
{
    GenTree* classHandle = impParentClassTokenToHandle(pResolvedToken);
    if (classHandle == nullptr)
    { // compDonotInline()
        return;
    }

    assert(pCallInfo->sig.numArgs);

    GenTree* node;

    //
    // There are two different JIT helpers that can be used to allocate
    // multi-dimensional arrays:
    //
    // - CORINFO_HELP_NEW_MDARR - takes the array dimensions as varargs.
    //      This variant is deprecated. It should be eventually removed.
    //
    // - CORINFO_HELP_NEW_MDARR_NONVARARG - takes the array dimensions as
    //      pointer to block of int32s. This variant is more portable.
    //
    // The non-varargs helper is enabled for CoreRT only for now. Enabling this
    // unconditionally would require ReadyToRun version bump.
    //
    CLANG_FORMAT_COMMENT_ANCHOR;

    if (!opts.IsReadyToRun() || IsTargetAbi(CORINFO_CORERT_ABI))
    {
        // Reuse the temp used to pass the array dimensions to avoid bloating
        // the stack frame in case there are multiple calls to multi-dim array
        // constructors within a single method.

        LclVarDsc* argsLcl;

        if (lvaNewObjArrayArgs == BAD_VAR_NUM)
        {
            lvaNewObjArrayArgs = lvaGrabTemp(false DEBUGARG("NewObjArrayArgs"));

            argsLcl = lvaGetDesc(lvaNewObjArrayArgs);
            argsLcl->SetBlockType(0);
        }
        else
        {
            argsLcl = lvaGetDesc(lvaNewObjArrayArgs);
        }

        // Increase size of lvaNewObjArrayArgs to be the largest size needed to hold 'numArgs' integers
        // for our call to CORINFO_HELP_NEW_MDARR_NONVARARG.
        argsLcl->SetBlockType(max(argsLcl->GetBlockSize(), pCallInfo->sig.numArgs * sizeof(int32_t)));

        // The side-effects may include allocation of more multi-dimensional arrays. Spill all side-effects
        // to ensure that the shared lvaNewObjArrayArgs local variable is only ever used to pass arguments
        // to one allocation at a time.
        impSpillSideEffects(true, CHECK_SPILL_ALL DEBUGARG("impImportNewObjArray"));

        //
        // The arguments of the CORINFO_HELP_NEW_MDARR_NONVARARG helper are:
        //  - Array class handle
        //  - Number of dimension arguments
        //  - Pointer to block of int32 dimensions - address  of lvaNewObjArrayArgs temp.
        //

        node = gtNewAddrNode(gtNewLclvNode(lvaNewObjArrayArgs, TYP_BLK), TYP_I_IMPL);

        // Pop dimension arguments from the stack one at a time and store it
        // into lvaNewObjArrayArgs temp.
        for (int i = pCallInfo->sig.numArgs - 1; i >= 0; i--)
        {
            GenTree* arg = impImplicitIorI4Cast(impPopStack().val, TYP_INT);

            GenTree* dest = gtNewAddrNode(gtNewLclvNode(lvaNewObjArrayArgs, TYP_BLK), TYP_I_IMPL);
            dest          = gtNewOperNode(GT_ADD, TYP_I_IMPL, dest, gtNewIconNode(sizeof(INT32) * i, TYP_I_IMPL));
            dest          = gtNewOperNode(GT_IND, TYP_INT, dest);
            node          = gtNewOperNode(GT_COMMA, node->TypeGet(), gtNewAssignNode(dest, arg), node);
        }

        GenTreeCall::Use* args = gtNewCallArgs(node);

        // pass number of arguments to the helper
        args = gtPrependNewCallArg(gtNewIconNode(pCallInfo->sig.numArgs), args);

        args = gtPrependNewCallArg(classHandle, args);

        node = gtNewHelperCallNode(CORINFO_HELP_NEW_MDARR_NONVARARG, TYP_REF, args);
    }
    else
    {
        //
        // The varargs helper needs the type and method handles as last
        // and  last-1 param (this is a cdecl call, so args will be
        // pushed in reverse order on the CPU stack)
        //

        GenTreeCall::Use* args = gtNewCallArgs(classHandle);

        // pass number of arguments to the helper
        args = gtPrependNewCallArg(gtNewIconNode(pCallInfo->sig.numArgs), args);

        unsigned argFlags = 0;
        args              = impPopCallArgs(pCallInfo->sig.numArgs, &pCallInfo->sig, args);

        node = gtNewHelperCallNode(CORINFO_HELP_NEW_MDARR, TYP_REF, args);

        // varargs, so we pop the arguments
        node->gtFlags |= GTF_CALL_POP_ARGS;

#ifdef DEBUG
        // At the present time we don't track Caller pop arguments
        // that have GC references in them
        for (GenTreeCall::Use& use : GenTreeCall::UseList(args))
        {
            assert(use.GetNode()->TypeGet() != TYP_REF);
        }
#endif
    }

    for (GenTreeCall::Use& use : node->AsCall()->Args())
    {
        node->gtFlags |= use.GetNode()->gtFlags & GTF_GLOB_EFFECT;
    }

    node->AsCall()->compileTimeHelperArgumentHandle = (CORINFO_GENERIC_HANDLE)pResolvedToken->hClass;

    // Remember that this basic block contains 'new' of a md array
    compCurBB->bbFlags |= BBF_HAS_NEWARRAY;

    impPushOnStack(node, typeInfo(TI_REF, pResolvedToken->hClass));
}

GenTree* Compiler::impTransformThis(GenTree*                thisPtr,
                                    CORINFO_RESOLVED_TOKEN* pConstrainedResolvedToken,
                                    CORINFO_THIS_TRANSFORM  transform)
{
    switch (transform)
    {
        case CORINFO_DEREF_THIS:
        {
            GenTree* obj = thisPtr;

            // This does a LDIND on the obj, which should be a byref. pointing to a ref
            impBashVarAddrsToI(obj);
            assert(genActualType(obj->gtType) == TYP_I_IMPL || obj->gtType == TYP_BYREF);
            CorInfoType constraintTyp = info.compCompHnd->asCorInfoType(pConstrainedResolvedToken->hClass);

            obj = gtNewOperNode(GT_IND, JITtype2varType(constraintTyp), obj);
            obj->gtFlags |= GTF_EXCEPT | GTF_GLOB_REF;

            return obj;
        }

        case CORINFO_BOX_THIS:
        {
            // Constraint calls where there might be no
            // unboxed entry point require us to implement the call via helper.
            // These only occur when a possible target of the call
            // may have inherited an implementation of an interface
            // method from System.Object or System.ValueType.  The EE does not provide us with
            // "unboxed" versions of these methods.

            assert(thisPtr->TypeIs(TYP_BYREF, TYP_I_IMPL));

            var_types type = JITtype2varType(info.compCompHnd->asCorInfoType(pConstrainedResolvedToken->hClass));
            GenTree*  indir;

            if (type == TYP_STRUCT)
            {
                indir = gtNewObjNode(pConstrainedResolvedToken->hClass, thisPtr);
            }
            else
            {
                indir = gtNewOperNode(GT_IND, type, thisPtr);
            }

            indir->gtFlags |= GTF_EXCEPT;

            if ((type == TYP_STRUCT) || (info.compCompHnd->getTypeForPrimitiveValueClass(
                                             pConstrainedResolvedToken->hClass) == CORINFO_TYPE_UNDEF))
            {
                impPushOnStack(indir, typeInfo(TI_STRUCT, pConstrainedResolvedToken->hClass));
            }
            else
            {
                impPushOnStack(indir, typeInfo());
            }

            // This pops off the byref-to-a-value-type remaining on the stack and
            // replaces it with a boxed object.
            // This is then used as the object to the virtual call immediately below.
            impImportAndPushBox(pConstrainedResolvedToken);
            if (compDonotInline())
            {
                return nullptr;
            }

            return impPopStack().val;
        }
        case CORINFO_NO_THIS_TRANSFORM:
        default:
            return thisPtr;
    }
}

//------------------------------------------------------------------------
// impCanPInvokeInline: check whether PInvoke inlining should enabled in current method.
//
// Return Value:
//    true if PInvoke inlining should be enabled in current method, false otherwise
//
// Notes:
//    Checks a number of ambient conditions where we could pinvoke but choose not to

bool Compiler::impCanPInvokeInline()
{
    return getInlinePInvokeEnabled() && (!opts.compDbgCode) && (compCodeOpt() != SMALL_CODE) &&
           (!opts.compNoPInvokeInlineCB) // profiler is preventing inline pinvoke
        ;
}

//------------------------------------------------------------------------
// impCanPInvokeInlineCallSite: basic legality checks using information
// from a call to see if the call qualifies as an inline pinvoke.
//
// Arguments:
//    block      - block contaning the call, or for inlinees, block
//                 containing the call being inlined
//
// Return Value:
//    true if this call can legally qualify as an inline pinvoke, false otherwise
//
// Notes:
//    For runtimes that support exception handling interop there are
//    restrictions on using inline pinvoke in handler regions.
//
//    * We have to disable pinvoke inlining inside of filters because
//    in case the main execution (i.e. in the try block) is inside
//    unmanaged code, we cannot reuse the inlined stub (we still need
//    the original state until we are in the catch handler)
//
//    * We disable pinvoke inlining inside handlers since the GSCookie
//    is in the inlined Frame (see
//    CORINFO_EE_INFO::InlinedCallFrameInfo::offsetOfGSCookie), but
//    this would not protect framelets/return-address of handlers.
//
//    These restrictions are currently also in place for CoreCLR but
//    can be relaxed when coreclr/#8459 is addressed.

bool Compiler::impCanPInvokeInlineCallSite(BasicBlock* block)
{
    if (block->hasHndIndex())
    {
        return false;
    }

    // The remaining limitations do not apply to CoreRT
    if (IsTargetAbi(CORINFO_CORERT_ABI))
    {
        return true;
    }

#ifdef TARGET_64BIT
    // On 64-bit platforms, we disable pinvoke inlining inside of try regions.
    // Note that this could be needed on other architectures too, but we
    // haven't done enough investigation to know for sure at this point.
    //
    // Here is the comment from JIT64 explaining why:
    //   [VSWhidbey: 611015] - because the jitted code links in the
    //   Frame (instead of the stub) we rely on the Frame not being
    //   'active' until inside the stub.  This normally happens by the
    //   stub setting the return address pointer in the Frame object
    //   inside the stub.  On a normal return, the return address
    //   pointer is zeroed out so the Frame can be safely re-used, but
    //   if an exception occurs, nobody zeros out the return address
    //   pointer.  Thus if we re-used the Frame object, it would go
    //   'active' as soon as we link it into the Frame chain.
    //
    //   Technically we only need to disable PInvoke inlining if we're
    //   in a handler or if we're in a try body with a catch or
    //   filter/except where other non-handler code in this method
    //   might run and try to re-use the dirty Frame object.
    //
    //   A desktop test case where this seems to matter is
    //   jit\jit64\ebvts\mcpp\sources2\ijw\__clrcall\vector_ctor_dtor.02\deldtor_clr.exe
    if (block->hasTryIndex())
    {
        // This does not apply to the raw pinvoke call that is inside the pinvoke
        // ILStub. In this case, we have to inline the raw pinvoke call into the stub,
        // otherwise we would end up with a stub that recursively calls itself, and end
        // up with a stack overflow.
        if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_IL_STUB) && opts.ShouldUsePInvokeHelpers())
        {
            return true;
        }

        return false;
    }
#endif // TARGET_64BIT

    return true;
}

//------------------------------------------------------------------------
// impCheckForPInvokeCall examine call to see if it is a pinvoke and if so
// if it can be expressed as an inline pinvoke.
//
// Arguments:
//    call       - tree for the call
//    methHnd    - handle for the method being called (may be null)
//    sig        - signature of the method being called
//    mflags     - method flags for the method being called
//    block      - block contaning the call, or for inlinees, block
//                 containing the call being inlined
//
// Notes:
//   Sets GTF_CALL_M_PINVOKE on the call for pinvokes.
//
//   Also sets GTF_CALL_UNMANAGED on call for inline pinvokes if the
//   call passes a combination of legality and profitabilty checks.
//
//   If GTF_CALL_UNMANAGED is set, increments info.compUnmanagedCallCountWithGCTransition

void Compiler::impCheckForPInvokeCall(
    GenTreeCall* call, CORINFO_METHOD_HANDLE methHnd, CORINFO_SIG_INFO* sig, unsigned mflags, BasicBlock* block)
{
    CorInfoCallConvExtension unmanagedCallConv;

    // If VM flagged it as Pinvoke, flag the call node accordingly
    if ((mflags & CORINFO_FLG_PINVOKE) != 0)
    {
        call->gtCallMoreFlags |= GTF_CALL_M_PINVOKE;
    }

    bool suppressGCTransition = false;
    if (methHnd)
    {
        if ((mflags & CORINFO_FLG_PINVOKE) == 0)
        {
            return;
        }

        unmanagedCallConv = info.compCompHnd->getUnmanagedCallConv(methHnd, nullptr, &suppressGCTransition);
    }
    else
    {
        if (sig->getCallConv() == CORINFO_CALLCONV_DEFAULT || sig->getCallConv() == CORINFO_CALLCONV_VARARG)
        {
            return;
        }

        unmanagedCallConv = info.compCompHnd->getUnmanagedCallConv(nullptr, sig, &suppressGCTransition);

        assert(!call->gtCallCookie);
    }

    if (suppressGCTransition)
    {
        call->gtCallMoreFlags |= GTF_CALL_M_SUPPRESS_GC_TRANSITION;
    }

    if (unmanagedCallConv != CorInfoCallConvExtension::C && unmanagedCallConv != CorInfoCallConvExtension::Stdcall &&
        unmanagedCallConv != CorInfoCallConvExtension::Thiscall)
    {
        return;
    }
    optNativeCallCount++;

    if (methHnd == nullptr && (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_IL_STUB) || IsTargetAbi(CORINFO_CORERT_ABI)))
    {
        // PInvoke in CoreRT ABI must be always inlined. Non-inlineable CALLI cases have been
        // converted to regular method calls earlier using convertPInvokeCalliToCall.

        // PInvoke CALLI in IL stubs must be inlined
    }
    else
    {
        // Check legality
        if (!impCanPInvokeInlineCallSite(block))
        {
            return;
        }

        // Legal PInvoke CALL in PInvoke IL stubs must be inlined to avoid infinite recursive
        // inlining in CoreRT. Skip the ambient conditions checks and profitability checks.
        if (!IsTargetAbi(CORINFO_CORERT_ABI) || (info.compFlags & CORINFO_FLG_PINVOKE) == 0)
        {
            if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_IL_STUB) && opts.ShouldUsePInvokeHelpers())
            {
                // Raw PInvoke call in PInvoke IL stub generated must be inlined to avoid infinite
                // recursive calls to the stub.
            }
            else
            {
                if (!impCanPInvokeInline())
                {
                    return;
                }

                // Size-speed tradeoff: don't use inline pinvoke at rarely
                // executed call sites.  The non-inline version is more
                // compact.
                if (block->isRunRarely())
                {
                    return;
                }
            }
        }

        // The expensive check should be last
        if (info.compCompHnd->pInvokeMarshalingRequired(methHnd, sig))
        {
            return;
        }
    }

    JITLOG((LL_INFO1000000, "\nInline a CALLI PINVOKE call from method %s", info.compFullName));

    call->gtFlags |= GTF_CALL_UNMANAGED;
    call->unmgdCallConv = unmanagedCallConv;
    if (!call->IsSuppressGCTransition())
    {
        info.compUnmanagedCallCountWithGCTransition++;
    }

    // AMD64 convention is same for native and managed
    if (unmanagedCallConv == CorInfoCallConvExtension::C)
    {
        call->gtFlags |= GTF_CALL_POP_ARGS;
    }

    if (unmanagedCallConv == CorInfoCallConvExtension::Thiscall)
    {
        call->gtCallMoreFlags |= GTF_CALL_M_UNMGD_THISCALL;
    }
}

GenTreeCall* Compiler::impImportIndirectCall(CORINFO_SIG_INFO* sig, IL_OFFSETX ilOffset)
{
    var_types callRetTyp = JITtype2varType(sig->retType);

    /* The function pointer is on top of the stack - It may be a
     * complex expression. As it is evaluated after the args,
     * it may cause registered args to be spilled. Simply spill it.
     */

    // Ignore this trivial case.
    if (impStackTop().val->gtOper != GT_LCL_VAR)
    {
        impSpillStackEntry(verCurrentState.esStackDepth - 1 DEBUGARG("impImportIndirectCall"));
    }

    /* Get the function pointer */

    GenTree* fptr = impPopStack().val;

    // The function pointer is typically a sized to match the target pointer size
    // However, stubgen IL optimization can change LDC.I8 to LDC.I4
    // See ILCodeStream::LowerOpcode
    assert(genActualType(fptr->gtType) == TYP_I_IMPL || genActualType(fptr->gtType) == TYP_INT);

#ifdef DEBUG
    // This temporary must never be converted to a double in stress mode,
    // because that can introduce a call to the cast helper after the
    // arguments have already been evaluated.

    if (fptr->OperGet() == GT_LCL_VAR)
    {
        lvaTable[fptr->AsLclVarCommon()->GetLclNum()].lvKeepType = 1;
    }
#endif

    /* Create the call node */

    GenTreeCall* call = gtNewIndCallNode(fptr, callRetTyp, nullptr, ilOffset);

    call->gtFlags |= GTF_EXCEPT | (fptr->gtFlags & GTF_GLOB_EFFECT);

    return call;
}

/*****************************************************************************/

void Compiler::impPopArgsForUnmanagedCall(GenTree* call, CORINFO_SIG_INFO* sig)
{
    assert(call->gtFlags & GTF_CALL_UNMANAGED);

    /* Since we push the arguments in reverse order (i.e. right -> left)
     * spill any side effects from the stack
     *
     * OBS: If there is only one side effect we do not need to spill it
     *      thus we have to spill all side-effects except last one
     */

    unsigned lastLevelWithSideEffects = UINT_MAX;

    unsigned argsToReverse = sig->numArgs;

    // For "thiscall", the first argument goes in a register. Since its
    // order does not need to be changed, we do not need to spill it

    if (call->AsCall()->gtCallMoreFlags & GTF_CALL_M_UNMGD_THISCALL)
    {
        assert(argsToReverse);
        argsToReverse--;
    }

#ifndef TARGET_X86
    // Don't reverse args on ARM or x64 - first four args always placed in regs in order
    argsToReverse = 0;
#endif

    for (unsigned level = verCurrentState.esStackDepth - argsToReverse; level < verCurrentState.esStackDepth; level++)
    {
        if (verCurrentState.esStack[level].val->gtFlags & GTF_ORDER_SIDEEFF)
        {
            assert(lastLevelWithSideEffects == UINT_MAX);

            impSpillStackEntry(level DEBUGARG("impPopArgsForUnmanagedCall - other side effect"));
        }
        else if (verCurrentState.esStack[level].val->gtFlags & GTF_SIDE_EFFECT)
        {
            if (lastLevelWithSideEffects != UINT_MAX)
            {
                /* We had a previous side effect - must spill it */
                impSpillStackEntry(lastLevelWithSideEffects DEBUGARG("impPopArgsForUnmanagedCall - side effect"));

                /* Record the level for the current side effect in case we will spill it */
                lastLevelWithSideEffects = level;
            }
            else
            {
                /* This is the first side effect encountered - record its level */

                lastLevelWithSideEffects = level;
            }
        }
    }

    /* The argument list is now "clean" - no out-of-order side effects
     * Pop the argument list in reverse order */

    GenTreeCall::Use* args     = impPopReverseCallArgs(sig->numArgs, sig, sig->numArgs - argsToReverse);
    call->AsCall()->gtCallArgs = args;

    if (call->AsCall()->gtCallMoreFlags & GTF_CALL_M_UNMGD_THISCALL)
    {
        GenTree* thisPtr = args->GetNode();
        impBashVarAddrsToI(thisPtr);
        assert(thisPtr->TypeGet() == TYP_I_IMPL || thisPtr->TypeGet() == TYP_BYREF);
    }

    for (GenTreeCall::Use& argUse : GenTreeCall::UseList(args))
    {
        GenTree* arg = argUse.GetNode();
        call->gtFlags |= arg->gtFlags & GTF_GLOB_EFFECT;

        // We should not be passing gc typed args to an unmanaged call.
        if (varTypeIsGC(arg->TypeGet()))
        {
            // Tolerate byrefs by retyping to native int.
            //
            // This is needed or we'll generate inconsistent GC info
            // for this arg at the call site (gc info says byref,
            // pinvoke sig says native int).
            //
            if (arg->TypeGet() == TYP_BYREF)
            {
                arg->ChangeType(TYP_I_IMPL);
            }
            else
            {
                assert(!"*** invalid IL: gc ref passed to unmanaged call");
            }
        }
    }
}

//------------------------------------------------------------------------
// impInitClass: Build a node to initialize the class before accessing the
//               field if necessary
//
// Arguments:
//    pResolvedToken - The CORINFO_RESOLVED_TOKEN that has been initialized
//                     by a call to CEEInfo::resolveToken().
//
// Return Value: If needed, a pointer to the node that will perform the class
//               initializtion.  Otherwise, nullptr.
//

GenTree* Compiler::impInitClass(CORINFO_RESOLVED_TOKEN* pResolvedToken)
{
    CorInfoInitClassResult initClassResult =
        info.compCompHnd->initClass(pResolvedToken->hField, info.compMethodHnd, impTokenLookupContextHandle);

    if ((initClassResult & CORINFO_INITCLASS_USE_HELPER) == 0)
    {
        return nullptr;
    }
    BOOL runtimeLookup;

    GenTree* node = impParentClassTokenToHandle(pResolvedToken, &runtimeLookup);

    if (node == nullptr)
    {
        assert(compDonotInline());
        return nullptr;
    }

    if (runtimeLookup)
    {
        node = gtNewHelperCallNode(CORINFO_HELP_INITCLASS, TYP_VOID, gtNewCallArgs(node));
    }
    else
    {
        // Call the shared non gc static helper, as its the fastest
        node = fgGetSharedCCtor(pResolvedToken->hClass);
    }

    return node;
}

GenTree* Compiler::impImportStaticReadOnlyField(void* addr, var_types type)
{
    switch (type)
    {
        int32_t ival;

        case TYP_BYTE:
            ival = *static_cast<int8_t*>(addr);
            goto IVAL_COMMON;
        case TYP_BOOL:
        case TYP_UBYTE:
            ival = *static_cast<uint8_t*>(addr);
            goto IVAL_COMMON;
        case TYP_SHORT:
            ival = *static_cast<int16_t*>(addr);
            goto IVAL_COMMON;
        case TYP_USHORT:
            ival = *static_cast<uint16_t*>(addr);
            goto IVAL_COMMON;
        case TYP_UINT:
        case TYP_INT:
            ival = *static_cast<int32_t*>(addr);
        IVAL_COMMON:
            return gtNewIconNode(ival);

        case TYP_LONG:
        case TYP_ULONG:
            return gtNewLconNode(*static_cast<int64_t*>(addr));

        case TYP_FLOAT:
            return gtNewDconNode(*static_cast<float*>(addr), TYP_FLOAT);

        case TYP_DOUBLE:
            return gtNewDconNode(*static_cast<double*>(addr), TYP_DOUBLE);

        default:
            assert(!"Unexpected type");
            return nullptr;
    }
}

GenTree* Compiler::impImportFieldAccess(GenTree*                  objPtr,
                                        CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                        const CORINFO_FIELD_INFO& fieldInfo,
                                        CORINFO_ACCESS_FLAGS      accessFlags,
                                        var_types                 type,
                                        CORINFO_CLASS_HANDLE      structType)
{
    assert((fieldInfo.fieldAccessor == CORINFO_FIELD_INSTANCE_ADDR_HELPER) &&
           (fieldInfo.helper == CORINFO_HELP_GETFIELDADDR));
    assert(objPtr != nullptr);

    GenTree* fieldHnd = impTokenToHandle(resolvedToken);
    GenTree* addr     = gtNewHelperCallNode(fieldInfo.helper, TYP_BYREF, gtNewCallArgs(objPtr, fieldHnd));

    if ((accessFlags & CORINFO_ACCESS_ADDRESS) != 0)
    {
        return addr;
    }

    GenTree* indir;

    if (varTypeIsStruct(type))
    {
        indir = gtNewObjNode(structType, addr);
    }
    else
    {
        indir = gtNewOperNode(GT_IND, type, addr);
    }

    indir->gtFlags |= GTF_GLOB_REF | GTF_EXCEPT;
    return indir;
}

GenTree* Compiler::impImportStaticFieldAddressHelper(CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                                     const CORINFO_FIELD_INFO& fieldInfo,
                                                     CORINFO_ACCESS_FLAGS      accessFlags)
{
    GenTree* addr;

    switch (fieldInfo.fieldAccessor)
    {
        case CORINFO_FIELD_STATIC_GENERICS_STATIC_HELPER:
        {
            assert(!compIsForInlining());
            addr = impParentClassTokenToHandle(resolvedToken);
            // compIsForInlining() is false so we should not get NULL here
            assert(addr != nullptr);

            var_types type = TYP_BYREF;

            switch (fieldInfo.helper)
            {
                case CORINFO_HELP_GETGENERICS_NONGCTHREADSTATIC_BASE:
                    type = TYP_I_IMPL;
                    break;
                case CORINFO_HELP_GETGENERICS_GCSTATIC_BASE:
                case CORINFO_HELP_GETGENERICS_NONGCSTATIC_BASE:
                case CORINFO_HELP_GETGENERICS_GCTHREADSTATIC_BASE:
                    break;
                default:
                    assert(!"unknown generic statics helper");
                    break;
            }

            addr = gtNewHelperCallNode(fieldInfo.helper, type, gtNewCallArgs(addr));
        }
        break;

        case CORINFO_FIELD_STATIC_SHARED_STATIC_HELPER:
        {
#ifdef FEATURE_READYTORUN_COMPILER
            if (opts.IsReadyToRun())
            {
                uint32_t classAttribs = info.compCompHnd->getClassAttribs(resolvedToken->hClass);

                addr = gtNewHelperCallNode(CORINFO_HELP_READYTORUN_STATIC_BASE, TYP_BYREF);
                addr->AsCall()->setEntryPoint(fieldInfo.fieldLookup);

                if ((classAttribs & CORINFO_FLG_BEFOREFIELDINIT) != 0)
                {
                    addr->gtFlags |= GTF_CALL_HOISTABLE;
                }
            }
            else
#endif
            {
                addr = fgGetStaticsCCtorHelper(resolvedToken->hClass, fieldInfo.helper);
            }
            break;
        }

#ifdef FEATURE_READYTORUN_COMPILER
        case CORINFO_FIELD_STATIC_READYTORUN_HELPER:
        {
            assert(opts.IsReadyToRun());
            assert(!compIsForInlining());

            CORINFO_LOOKUP_KIND kind;
            info.compCompHnd->getLocationOfThisType(info.compMethodHnd, &kind);
            assert(kind.needsRuntimeLookup);

            uint32_t classAttribs = info.compCompHnd->getClassAttribs(resolvedToken->hClass);

            GenTree* ctxTree = getRuntimeContextTree(kind.runtimeLookupKind);
            addr = gtNewHelperCallNode(CORINFO_HELP_READYTORUN_GENERIC_STATIC_BASE, TYP_BYREF, gtNewCallArgs(ctxTree));
            addr->AsCall()->setEntryPoint(fieldInfo.fieldLookup);

            if ((classAttribs & CORINFO_FLG_BEFOREFIELDINIT) != 0)
            {
                addr->gtFlags |= GTF_CALL_HOISTABLE;
            }
        }
        break;
#endif

        default:
            unreached();
    }

    FieldSeqNode* fieldSeq = GetFieldSeqStore()->CreateSingleton(resolvedToken->hField);
    addr                   = gtNewOperNode(GT_ADD, addr->GetType(), addr, gtNewIconNode(fieldInfo.offset, fieldSeq));

    if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC_IN_HEAP) != 0)
    {
        addr     = gtNewOperNode(GT_IND, TYP_REF, addr);
        fieldSeq = GetFieldSeqStore()->CreateSingleton(FieldSeqStore::BoxedValuePseudoFieldHandle);
        addr     = gtNewOperNode(GT_ADD, TYP_BYREF, addr, gtNewIconNode(TARGET_POINTER_SIZE, fieldSeq));
    }

    return addr;
}

GenTree* Compiler::impImportStaticFieldAccess(CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                              const CORINFO_FIELD_INFO& fieldInfo,
                                              CORINFO_ACCESS_FLAGS      accessFlags,
                                              var_types                 type)
{
    if ((fieldInfo.fieldAccessor == CORINFO_FIELD_STATIC_GENERICS_STATIC_HELPER) ||
        (fieldInfo.fieldAccessor == CORINFO_FIELD_STATIC_SHARED_STATIC_HELPER) ||
        (fieldInfo.fieldAccessor == CORINFO_FIELD_STATIC_READYTORUN_HELPER))
    {
        GenTree* addr = impImportStaticFieldAddressHelper(resolvedToken, fieldInfo, accessFlags);

        if ((accessFlags & CORINFO_ACCESS_ADDRESS) != 0)
        {
            return addr;
        }

        GenTree* indir;

        if (varTypeIsStruct(type))
        {
            indir = gtNewObjNode(fieldInfo.structType, addr);
        }
        else
        {
            indir = gtNewOperNode(GT_IND, type, addr);
        }

        indir->gtFlags |= GTF_GLOB_REF;

        if (indir->TypeIs(TYP_REF) && addr->TypeIs(TYP_BYREF))
        {
            // Storing an object reference into a static field requires a write barrier.
            // But what kind of barrier? GCInfo::GetWriteBarrierForm has trouble
            // figuring it out because the address is a byref that comes from a helper
            // call, rather than being derived from an object reference.
            //
            // Set GTF_IND_TGT_HEAP to tell GetWriteBarrierForm that this is really
            // a GC heap store so an unchecked write barrier can be used.

            // TODO-MIKE-Review: Are the checked barriers significantly slower than the
            // unchecked barriers to worth this trouble?

            indir->gtFlags |= GTF_IND_TGT_HEAP;
        }

        return indir;
    }

    assert((fieldInfo.fieldAccessor == CORINFO_FIELD_STATIC_ADDRESS) ||
           (fieldInfo.fieldAccessor == CORINFO_FIELD_STATIC_RVA_ADDRESS));

    void* pFldAddr = nullptr;
    void* fldAddr  = info.compCompHnd->getFieldAddress(resolvedToken->hField, &pFldAddr);
    // We should always be able to access this static's address directly
    assert(pFldAddr == nullptr);

    // Replace static read-only fields with constant if possible
    if (((accessFlags & CORINFO_ACCESS_GET) != 0) && ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_FINAL) != 0) &&
        ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC_IN_HEAP) == 0) &&
        (varTypeIsIntegral(type) || varTypeIsFloating(type)))
    {
        CorInfoInitClassResult initClassResult =
            info.compCompHnd->initClass(resolvedToken->hField, info.compMethodHnd, impTokenLookupContextHandle);

        if ((initClassResult & CORINFO_INITCLASS_INITIALIZED) != 0)
        {
            return impImportStaticReadOnlyField(fldAddr, type);
        }
    }

    FieldSeqNode* fieldSeq = GetFieldSeqStore()->CreateSingleton(resolvedToken->hField);
    GenTree*      addr     = nullptr;

    if (((accessFlags & CORINFO_ACCESS_ADDRESS) != 0)
#ifdef TARGET_64BIT
        || (eeGetRelocTypeHint(fldAddr) != IMAGE_REL_BASED_REL32)
#endif
            )
    {
        addr = gtNewIconHandleNode(reinterpret_cast<size_t>(fldAddr), GTF_ICON_STATIC_HDL, fieldSeq);
        INDEBUG(addr->AsIntCon()->gtTargetHandle = addr->AsIntCon()->GetValue();)

        if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_INITCLASS) != 0)
        {
            addr->gtFlags |= GTF_ICON_INITCLASS;
        }
    }

    if ((accessFlags & CORINFO_ACCESS_ADDRESS) != 0)
    {
        if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC_IN_HEAP) != 0)
        {
            addr     = gtNewOperNode(GT_IND, TYP_REF, addr);
            fieldSeq = GetFieldSeqStore()->CreateSingleton(FieldSeqStore::BoxedValuePseudoFieldHandle);
            addr     = gtNewOperNode(GT_ADD, TYP_BYREF, addr, gtNewIconNode(TARGET_POINTER_SIZE, fieldSeq));
        }

        return addr;
    }

    GenTree* indir;

    if (addr != nullptr)
    {
        indir = gtNewOperNode(GT_IND, type, addr);
        indir->gtFlags |= GTF_IND_NONFAULTING;
    }
    else
    {
        indir = new (this, GT_CLS_VAR) GenTreeClsVar(GT_CLS_VAR, type, resolvedToken->hField, fieldSeq);

        if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_INITCLASS) != 0)
        {
            indir->gtFlags |= GTF_CLS_VAR_INITCLASS;
        }
    }

    indir->gtFlags |= GTF_GLOB_REF;

    if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC_IN_HEAP) != 0)
    {
        indir->SetType(TYP_REF);
        addr = indir;

        fieldSeq = GetFieldSeqStore()->CreateSingleton(FieldSeqStore::BoxedValuePseudoFieldHandle);
        addr     = gtNewOperNode(GT_ADD, TYP_BYREF, addr, gtNewIconNode(TARGET_POINTER_SIZE, fieldSeq));

        if (varTypeIsStruct(type))
        {
            indir = gtNewObjNode(fieldInfo.structType, addr);
        }
        else
        {
            indir = gtNewOperNode(GT_IND, type, addr);
            indir->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;
        }
    }

    return indir;
}

void Compiler::impHandleAccessAllowed(CorInfoIsAccessAllowedResult result, CORINFO_HELPER_DESC* helperCall)
{
    if (result != CORINFO_ACCESS_ALLOWED)
    {
        impHandleAccessAllowedInternal(result, helperCall);
    }
}

void Compiler::impHandleAccessAllowedInternal(CorInfoIsAccessAllowedResult result, CORINFO_HELPER_DESC* helperCall)
{
    switch (result)
    {
        case CORINFO_ACCESS_ALLOWED:
            break;
        case CORINFO_ACCESS_ILLEGAL:
            impInsertHelperCall(helperCall);
            break;
    }
}

void Compiler::impInsertHelperCall(CORINFO_HELPER_DESC* helperInfo)
{
    // Construct the argument list
    GenTreeCall::Use* args = nullptr;
    assert(helperInfo->helperNum != CORINFO_HELP_UNDEF);
    for (unsigned i = helperInfo->numArgs; i > 0; --i)
    {
        const CORINFO_HELPER_ARG& helperArg  = helperInfo->args[i - 1];
        GenTree*                  currentArg = nullptr;
        switch (helperArg.argType)
        {
            case CORINFO_HELPER_ARG_TYPE_Field:
                info.compCompHnd->classMustBeLoadedBeforeCodeIsRun(
                    info.compCompHnd->getFieldClass(helperArg.fieldHandle));
                currentArg = gtNewIconEmbFldHndNode(helperArg.fieldHandle);
                break;
            case CORINFO_HELPER_ARG_TYPE_Method:
                info.compCompHnd->methodMustBeLoadedBeforeCodeIsRun(helperArg.methodHandle);
                currentArg = gtNewIconEmbMethHndNode(helperArg.methodHandle);
                break;
            case CORINFO_HELPER_ARG_TYPE_Class:
                info.compCompHnd->classMustBeLoadedBeforeCodeIsRun(helperArg.classHandle);
                currentArg = gtNewIconEmbClsHndNode(helperArg.classHandle);
                break;
            case CORINFO_HELPER_ARG_TYPE_Module:
                currentArg = gtNewIconEmbScpHndNode(helperArg.moduleHandle);
                break;
            case CORINFO_HELPER_ARG_TYPE_Const:
                currentArg = gtNewIconNode(helperArg.constant);
                break;
            default:
                NO_WAY("Illegal helper arg type");
        }
        args = gtPrependNewCallArg(currentArg, args);
    }

    /* TODO-Review:
     * Mark as CSE'able, and hoistable.  Consider marking hoistable unless you're in the inlinee.
     * Also, consider sticking this in the first basic block.
     */
    GenTree* callout = gtNewHelperCallNode(helperInfo->helperNum, TYP_VOID, args);
    impAppendTree(callout, (unsigned)CHECK_SPILL_NONE, impCurStmtOffs);
}

// Checks whether the return types of caller and callee are compatible
// so that callee can be tail called. Note that here we don't check
// compatibility in IL Verifier sense, but on the lines of return type
// sizes are equal and get returned in the same return register(s).
bool Compiler::impTailCallRetTypeCompatible(GenTreeCall* call)
{
    if (!varTypeIsStruct(call->GetRetSigType()))
    {
        // Note that we can not relax this condition with genActualType() as the
        // calling convention dictates that the caller of a function with a small
        // typed return value is responsible for normalizing the return val.

        return call->GetRetSigType() == info.GetRetSigType();
    }

    if (call->HasRetBufArg())
    {
        return info.compRetBuffArg != BAD_VAR_NUM;
    }

    if (call->GetRegCount() != info.retDesc.GetRegCount())
    {
        return false;
    }

    for (unsigned i = 0; i < call->GetRegCount(); i++)
    {
        if (call->GetRegType(i) != info.retDesc.GetRegType(i))
        {
            return false;
        }
    }

    return true;
}

// For prefixFlags
enum
{
    PREFIX_TAILCALL_EXPLICIT = 0x00000001, // call has "tail" IL prefix
    PREFIX_TAILCALL_IMPLICIT =
        0x00000010, // call is treated as having "tail" prefix even though there is no "tail" IL prefix
    PREFIX_TAILCALL_STRESS =
        0x00000100, // call doesn't "tail" IL prefix but is treated as explicit because of tail call stress
    PREFIX_TAILCALL    = (PREFIX_TAILCALL_EXPLICIT | PREFIX_TAILCALL_IMPLICIT | PREFIX_TAILCALL_STRESS),
    PREFIX_VOLATILE    = 0x00001000,
    PREFIX_UNALIGNED   = 0x00010000,
    PREFIX_CONSTRAINED = 0x00100000,
    PREFIX_READONLY    = 0x01000000
};

/********************************************************************************
 *
 * Returns true if the current opcode and and the opcodes following it correspond
 * to a supported tail call IL pattern.
 *
 */
bool Compiler::impIsTailCallILPattern(
    bool tailPrefixed, OPCODE curOpcode, const BYTE* codeAddrOfNextOpcode, const BYTE* codeEnd, bool isRecursive)
{
    // Bail out if the current opcode is not a call.
    if (!impOpcodeIsCallOpcode(curOpcode))
    {
        return false;
    }

#if !FEATURE_TAILCALL_OPT_SHARED_RETURN
    // If shared ret tail opt is not enabled, we will enable
    // it for recursive methods.
    if (isRecursive)
#endif
    {
        // we can actually handle if the ret is in a fallthrough block, as long as that is the only part of the
        // sequence. Make sure we don't go past the end of the IL however.
        codeEnd = min(codeEnd + 1, info.compCode + info.compILCodeSize);
    }

    // Bail out if there is no next opcode after call
    if (codeAddrOfNextOpcode >= codeEnd)
    {
        return false;
    }

    OPCODE nextOpcode = (OPCODE)getU1LittleEndian(codeAddrOfNextOpcode);

    return (nextOpcode == CEE_RET);
}

/*****************************************************************************
 *
 * Determine whether the call could be converted to an implicit tail call
 *
 */
bool Compiler::impIsImplicitTailCallCandidate(
    OPCODE opcode, const BYTE* codeAddrOfNextOpcode, const BYTE* codeEnd, int prefixFlags, bool isRecursive)
{

#if FEATURE_TAILCALL_OPT
    if (!opts.compTailCallOpt)
    {
        return false;
    }

    if (opts.OptimizationDisabled())
    {
        return false;
    }

    // must not be tail prefixed
    if (prefixFlags & PREFIX_TAILCALL_EXPLICIT)
    {
        return false;
    }

#if !FEATURE_TAILCALL_OPT_SHARED_RETURN
    // the block containing call is marked as BBJ_RETURN
    // We allow shared ret tail call optimization on recursive calls even under
    // !FEATURE_TAILCALL_OPT_SHARED_RETURN.
    if (!isRecursive && (compCurBB->bbJumpKind != BBJ_RETURN))
        return false;
#endif // !FEATURE_TAILCALL_OPT_SHARED_RETURN

    // must be call+ret or call+pop+ret
    if (!impIsTailCallILPattern(false, opcode, codeAddrOfNextOpcode, codeEnd, isRecursive))
    {
        return false;
    }

    return true;
#else
    return false;
#endif // FEATURE_TAILCALL_OPT
}

//------------------------------------------------------------------------
// impImportCall: import a call-inspiring opcode
//
// Arguments:
//    opcode                    - opcode that inspires the call
//    pResolvedToken            - resolved token for the call target
//    pConstrainedResolvedToken - resolved constraint token (or nullptr)
//    newObjThis                - tree for this pointer or uninitalized newobj temp (or nullptr)
//    prefixFlags               - IL prefix flags for the call
//    callInfo                  - EE supplied info for the call
//    rawILOffset               - IL offset of the opcode
//
// Returns:
//    Type of the call's return value.
//    If we're importing an inlinee and have realized the inline must fail, the call return type should be TYP_UNDEF.
//    However we can't assert for this here yet because there are cases we miss. See issue #13272.
//
//
// Notes:
//    opcode can be CEE_CALL, CEE_CALLI, CEE_CALLVIRT, or CEE_NEWOBJ.
//
//    For CEE_NEWOBJ, newobjThis should be the temp grabbed for the allocated
//    uninitalized object.

#ifdef _PREFAST_
#pragma warning(push)
#pragma warning(disable : 21000) // Suppress PREFast warning about overly large function
#endif

var_types Compiler::impImportCall(OPCODE                  opcode,
                                  CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                  CORINFO_RESOLVED_TOKEN* pConstrainedResolvedToken,
                                  GenTree*                newobjThis,
                                  int                     prefixFlags,
                                  CORINFO_CALL_INFO*      callInfo,
                                  IL_OFFSET               rawILOffset)
{
    assert(opcode == CEE_CALL || opcode == CEE_CALLVIRT || opcode == CEE_NEWOBJ || opcode == CEE_CALLI);

    IL_OFFSETX             ilOffset                       = impCurILOffset(rawILOffset, true);
    var_types              callRetTyp                     = TYP_COUNT;
    CORINFO_SIG_INFO*      sig                            = nullptr;
    CORINFO_METHOD_HANDLE  methHnd                        = nullptr;
    CORINFO_CLASS_HANDLE   clsHnd                         = nullptr;
    unsigned               clsFlags                       = 0;
    unsigned               mflags                         = 0;
    unsigned               argFlags                       = 0;
    GenTree*               call                           = nullptr;
    GenTreeCall::Use*      args                           = nullptr;
    CORINFO_THIS_TRANSFORM constraintCallThisTransform    = CORINFO_NO_THIS_TRANSFORM;
    CORINFO_CONTEXT_HANDLE exactContextHnd                = nullptr;
    bool                   exactContextNeedsRuntimeLookup = false;
    bool                   canTailCall                    = true;
    const char*            szCanTailCallFailReason        = nullptr;
    const int              tailCallFlags                  = (prefixFlags & PREFIX_TAILCALL);
    const bool             isReadonlyCall                 = (prefixFlags & PREFIX_READONLY) != 0;

    CORINFO_RESOLVED_TOKEN* ldftnToken = nullptr;

    bool             hasCallSiteSig = false;
    CORINFO_SIG_INFO callSiteSig;

    // Synchronized methods need to call CORINFO_HELP_MON_EXIT at the end. We could
    // do that before tailcalls, but that is probably not the intended
    // semantic. So just disallow tailcalls from synchronized methods.
    // Also, popping arguments in a varargs function is more work and NYI
    // If we have a security object, we have to keep our frame around for callers
    // to see any imperative security.
    // Reverse P/Invokes need a call to CORINFO_HELP_JIT_REVERSE_PINVOKE_EXIT
    // at the end, so tailcalls should be disabled.
    if (info.compFlags & CORINFO_FLG_SYNCH)
    {
        canTailCall             = false;
        szCanTailCallFailReason = "Caller is synchronized";
    }
    else if (opts.IsReversePInvoke())
    {
        canTailCall             = false;
        szCanTailCallFailReason = "Caller is Reverse P/Invoke";
    }
#if !FEATURE_FIXED_OUT_ARGS
    else if (info.compIsVarArgs)
    {
        canTailCall             = false;
        szCanTailCallFailReason = "Caller is varargs";
    }
#endif // FEATURE_FIXED_OUT_ARGS

    // We only need to cast the return value of pinvoke inlined calls that return small types

    // TODO-AMD64-Cleanup: Remove this when we stop interoperating with JIT64, or if we decide to stop
    // widening everything! CoreCLR does not support JIT64 interoperation so no need to widen there.
    // The existing x64 JIT doesn't bother widening all types to int, so we have to assume for
    // the time being that the callee might be compiled by the other JIT and thus the return
    // value will need to be widened by us (or not widened at all...)

    // ReadyToRun code sticks with default calling convention that does not widen small return types.

    bool checkForSmallType  = opts.IsReadyToRun();
    bool bIntrinsicImported = false;

    CORINFO_SIG_INFO  calliSig;
    GenTreeCall::Use* extraArg = nullptr;

    /*-------------------------------------------------------------------------
     * First create the call node
     */

    if (opcode == CEE_CALLI)
    {
        if (IsTargetAbi(CORINFO_CORERT_ABI))
        {
            // See comment in impCheckForPInvokeCall
            BasicBlock* block = compIsForInlining() ? impInlineInfo->iciBlock : compCurBB;
            if (info.compCompHnd->convertPInvokeCalliToCall(pResolvedToken, !impCanPInvokeInlineCallSite(block)))
            {
                eeGetCallInfo(pResolvedToken, nullptr, CORINFO_CALLINFO_ALLOWINSTPARAM, callInfo);
                return impImportCall(CEE_CALL, pResolvedToken, nullptr, nullptr, prefixFlags, callInfo, rawILOffset);
            }
        }

        /* Get the call site sig */
        eeGetSig(pResolvedToken->token, pResolvedToken->tokenScope, pResolvedToken->tokenContext, &calliSig);

        callRetTyp = JITtype2varType(calliSig.retType);

        call = impImportIndirectCall(&calliSig, ilOffset);

        // We don't know the target method, so we have to infer the flags, or
        // assume the worst-case.
        mflags = (calliSig.callConv & CORINFO_CALLCONV_HASTHIS) ? 0 : CORINFO_FLG_STATIC;

#ifdef DEBUG
        if (verbose)
        {
            unsigned structSize =
                (callRetTyp == TYP_STRUCT) ? info.compCompHnd->getClassSize(calliSig.retTypeSigClass) : 0;
            printf("\nIn Compiler::impImportCall: opcode is %s, kind=%d, callRetType is %s, structSize is %d\n",
                   opcodeNames[opcode], callInfo->kind, varTypeName(callRetTyp), structSize);
        }
#endif
        // This should be checked in impImportBlockCode.
        assert(!compIsForInlining() || !(impInlineInfo->inlineCandidateInfo->dwRestrictions & INLINE_RESPECT_BOUNDARY));

        sig = &calliSig;
    }
    else // (opcode != CEE_CALLI)
    {
        CorInfoIntrinsics intrinsicID = CORINFO_INTRINSIC_Count;

        // Passing CORINFO_CALLINFO_ALLOWINSTPARAM indicates that this JIT is prepared to
        // supply the instantiation parameters necessary to make direct calls to underlying
        // shared generic code, rather than calling through instantiating stubs.  If the
        // returned signature has CORINFO_CALLCONV_PARAMTYPE then this indicates that the JIT
        // must indeed pass an instantiation parameter.

        methHnd = callInfo->hMethod;

        sig        = &(callInfo->sig);
        callRetTyp = JITtype2varType(sig->retType);

        mflags = callInfo->methodFlags;

#ifdef DEBUG
        if (verbose)
        {
            unsigned structSize = (callRetTyp == TYP_STRUCT) ? info.compCompHnd->getClassSize(sig->retTypeSigClass) : 0;
            printf("\nIn Compiler::impImportCall: opcode is %s, kind=%d, callRetType is %s, structSize is %d\n",
                   opcodeNames[opcode], callInfo->kind, varTypeName(callRetTyp), structSize);
        }
#endif
        if (compIsForInlining())
        {
            /* Does this call site have security boundary restrictions? */

            if (impInlineInfo->inlineCandidateInfo->dwRestrictions & INLINE_RESPECT_BOUNDARY)
            {
                compInlineResult->NoteFatal(InlineObservation::CALLSITE_CROSS_BOUNDARY_SECURITY);
                return TYP_UNDEF;
            }

            /* Does the inlinee use StackCrawlMark */

            if (mflags & CORINFO_FLG_DONT_INLINE_CALLER)
            {
                compInlineResult->NoteFatal(InlineObservation::CALLEE_STACK_CRAWL_MARK);
                return TYP_UNDEF;
            }

            /* For now ignore varargs */
            if ((sig->callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_NATIVEVARARG)
            {
                compInlineResult->NoteFatal(InlineObservation::CALLEE_HAS_NATIVE_VARARGS);
                return TYP_UNDEF;
            }

            if ((sig->callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_VARARG)
            {
                compInlineResult->NoteFatal(InlineObservation::CALLEE_HAS_MANAGED_VARARGS);
                return TYP_UNDEF;
            }

            if ((mflags & CORINFO_FLG_VIRTUAL) && (sig->sigInst.methInstCount != 0) && (opcode == CEE_CALLVIRT))
            {
                compInlineResult->NoteFatal(InlineObservation::CALLEE_IS_GENERIC_VIRTUAL);
                return TYP_UNDEF;
            }
        }

        clsHnd = pResolvedToken->hClass;

        clsFlags = callInfo->classFlags;

        // <NICE> Factor this into getCallInfo </NICE>
        bool isSpecialIntrinsic = false;
        if ((mflags & (CORINFO_FLG_INTRINSIC | CORINFO_FLG_JIT_INTRINSIC)) != 0)
        {
            const bool isTailCall = canTailCall && (tailCallFlags != 0);

            call = impIntrinsic(newobjThis, clsHnd, methHnd, sig, mflags, pResolvedToken->token, isReadonlyCall,
                                isTailCall, pConstrainedResolvedToken, callInfo->thisTransform, &intrinsicID,
                                &isSpecialIntrinsic);

            if (compDonotInline())
            {
                return TYP_UNDEF;
            }

            if (call != nullptr)
            {
#ifdef FEATURE_READYTORUN_COMPILER
                if (call->OperGet() == GT_INTRINSIC)
                {
                    if (opts.IsReadyToRun())
                    {
                        noway_assert(callInfo->kind == CORINFO_CALL);
                        call->AsIntrinsic()->gtEntryPoint = callInfo->codePointerLookup.constLookup;
                    }
                    else
                    {
                        call->AsIntrinsic()->gtEntryPoint.addr       = nullptr;
                        call->AsIntrinsic()->gtEntryPoint.accessType = IAT_VALUE;
                    }
                }
#endif

                bIntrinsicImported = true;
                goto DONE_INTRINSIC;
            }
        }

#ifdef FEATURE_SIMD
        if (featureSIMD)
        {
            call = impSIMDIntrinsic(opcode, newobjThis, clsHnd, methHnd, sig, mflags, pResolvedToken->token);
            if (call != nullptr)
            {
                bIntrinsicImported = true;
                goto DONE_INTRINSIC;
            }
        }
#endif // FEATURE_SIMD

        if ((mflags & CORINFO_FLG_VIRTUAL) && (mflags & CORINFO_FLG_EnC) && (opcode == CEE_CALLVIRT))
        {
            NO_WAY("Virtual call to a function added via EnC is not supported");
        }

        if ((sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_DEFAULT &&
            (sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_VARARG &&
            (sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_NATIVEVARARG)
        {
            BADCODE("Bad calling convention");
        }

        //-------------------------------------------------------------------------
        //  Construct the call node
        //
        // Work out what sort of call we're making.
        // Dispense with virtual calls implemented via LDVIRTFTN immediately.

        constraintCallThisTransform    = callInfo->thisTransform;
        exactContextHnd                = callInfo->contextHandle;
        exactContextNeedsRuntimeLookup = callInfo->exactContextNeedsRuntimeLookup == TRUE;

        switch (callInfo->kind)
        {

            case CORINFO_VIRTUALCALL_STUB:
            {
                assert(!(mflags & CORINFO_FLG_STATIC)); // can't call a static method
                assert(!(clsFlags & CORINFO_FLG_VALUECLASS));
                if (callInfo->stubLookup.lookupKind.needsRuntimeLookup)
                {
                    if (callInfo->stubLookup.lookupKind.runtimeLookupKind == CORINFO_LOOKUP_NOT_SUPPORTED)
                    {
                        // Runtime does not support inlining of all shapes of runtime lookups
                        // Inlining has to be aborted in such a case
                        compInlineResult->NoteFatal(InlineObservation::CALLSITE_HAS_COMPLEX_HANDLE);
                        return TYP_UNDEF;
                    }

                    GenTree* stubAddr = impRuntimeLookupToTree(pResolvedToken, &callInfo->stubLookup, methHnd);
                    assert(!compDonotInline());
                    assert(stubAddr->TypeIs(TYP_I_IMPL));

                    // The stubAddr may be a
                    // complex expression. As it is evaluated after the args,
                    // it may cause registered args to be spilled. Simply spill it.

                    unsigned lclNum = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("VirtualCall with runtime lookup"));
                    GenTree* asg    = gtNewAssignNode(gtNewLclvNode(lclNum, TYP_I_IMPL), stubAddr);
                    impAppendTree(asg, CHECK_SPILL_NONE, impCurStmtOffs);
                    stubAddr = gtNewLclvNode(lclNum, TYP_I_IMPL);

                    // Create the actual call node

                    assert((sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_VARARG &&
                           (sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_NATIVEVARARG);

                    call = gtNewIndCallNode(stubAddr, callRetTyp, nullptr);

                    call->gtFlags |= GTF_EXCEPT | (stubAddr->gtFlags & GTF_GLOB_EFFECT);
                    call->gtFlags |= GTF_CALL_VIRT_STUB;

#ifdef TARGET_X86
                    // No tailcalls allowed for these yet...
                    canTailCall             = false;
                    szCanTailCallFailReason = "VirtualCall with runtime lookup";
#endif
                }
                else
                {
                    // The stub address is known at compile time
                    call = gtNewCallNode(CT_USER_FUNC, callInfo->hMethod, callRetTyp, nullptr, ilOffset);
                    call->AsCall()->gtStubCallStubAddr = callInfo->stubLookup.constLookup.addr;
                    call->gtFlags |= GTF_CALL_VIRT_STUB;
                    assert(callInfo->stubLookup.constLookup.accessType != IAT_PPVALUE &&
                           callInfo->stubLookup.constLookup.accessType != IAT_RELPVALUE);
                    if (callInfo->stubLookup.constLookup.accessType == IAT_PVALUE)
                    {
                        call->AsCall()->gtCallMoreFlags |= GTF_CALL_M_VIRTSTUB_REL_INDIRECT;
                    }
                }

#ifdef FEATURE_READYTORUN_COMPILER
                if (opts.IsReadyToRun())
                {
                    // Null check is sometimes needed for ready to run to handle
                    // non-virtual <-> virtual changes between versions
                    if (callInfo->nullInstanceCheck)
                    {
                        call->gtFlags |= GTF_CALL_NULLCHECK;
                    }
                }
#endif

                break;
            }

            case CORINFO_VIRTUALCALL_VTABLE:
            {
                assert(!(mflags & CORINFO_FLG_STATIC)); // can't call a static method
                assert(!(clsFlags & CORINFO_FLG_VALUECLASS));
                call = gtNewCallNode(CT_USER_FUNC, callInfo->hMethod, callRetTyp, nullptr, ilOffset);
                call->gtFlags |= GTF_CALL_VIRT_VTABLE;

                // Should we expand virtual call targets early for this method?
                //
                if (opts.compExpandCallsEarly)
                {
                    // Mark this method to expand the virtual call target early in fgMorpgCall
                    call->AsCall()->SetExpandedEarly();
                }
                break;
            }

            case CORINFO_VIRTUALCALL_LDVIRTFTN:
            {
                if (compIsForInlining())
                {
                    compInlineResult->NoteFatal(InlineObservation::CALLSITE_HAS_CALL_VIA_LDVIRTFTN);
                    return TYP_UNDEF;
                }

                assert(!(mflags & CORINFO_FLG_STATIC)); // can't call a static method
                assert(!(clsFlags & CORINFO_FLG_VALUECLASS));
                // OK, We've been told to call via LDVIRTFTN, so just
                // take the call now....

                GenTreeCall::Use* args = impPopCallArgs(sig->numArgs, sig);

                GenTree* thisPtr = impPopStack().val;
                thisPtr          = impTransformThis(thisPtr, pConstrainedResolvedToken, callInfo->thisTransform);
                assert(thisPtr != nullptr);

                // Clone the (possibly transformed) "this" pointer
                GenTree* thisPtrCopy;
                thisPtr = impCloneExpr(thisPtr, &thisPtrCopy, NO_CLASS_HANDLE,
                                       CHECK_SPILL_ALL DEBUGARG("LDVIRTFTN this pointer"));

                GenTree* fptr = impImportLdvirtftn(thisPtr, pResolvedToken, callInfo);
                assert(fptr->TypeIs(TYP_I_IMPL));

                thisPtr = nullptr; // can't reuse it

                // Now make an indirect call through the function pointer

                unsigned lclNum = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("VirtualCall through function pointer"));
                GenTree* asg    = gtNewAssignNode(gtNewLclvNode(lclNum, TYP_I_IMPL), fptr);
                impAppendTree(asg, CHECK_SPILL_ALL, impCurStmtOffs);
                fptr = gtNewLclvNode(lclNum, TYP_I_IMPL);

                // Create the actual call node

                call                          = gtNewIndCallNode(fptr, callRetTyp, args, ilOffset);
                call->AsCall()->gtCallThisArg = gtNewCallArgs(thisPtrCopy);
                call->gtFlags |= GTF_EXCEPT | (fptr->gtFlags & GTF_GLOB_EFFECT);

                if ((sig->sigInst.methInstCount != 0) && IsTargetAbi(CORINFO_CORERT_ABI))
                {
                    // CoreRT generic virtual method: need to handle potential fat function pointers
                    addFatPointerCandidate(call->AsCall());
                }
#ifdef FEATURE_READYTORUN_COMPILER
                if (opts.IsReadyToRun())
                {
                    // Null check is needed for ready to run to handle
                    // non-virtual <-> virtual changes between versions
                    call->gtFlags |= GTF_CALL_NULLCHECK;
                }
#endif

                // Sine we are jumping over some code, check that its OK to skip that code
                assert((sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_VARARG &&
                       (sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_NATIVEVARARG);
                goto DONE;
            }

            case CORINFO_CALL:
            {
                // This is for a non-virtual, non-interface etc. call
                call = gtNewCallNode(CT_USER_FUNC, callInfo->hMethod, callRetTyp, nullptr, ilOffset);

                // We remove the nullcheck for the GetType call intrinsic.
                // TODO-CQ: JIT64 does not introduce the null check for many more helper calls
                // and intrinsics.
                if (callInfo->nullInstanceCheck &&
                    !((mflags & CORINFO_FLG_INTRINSIC) != 0 && (intrinsicID == CORINFO_INTRINSIC_Object_GetType)))
                {
                    call->gtFlags |= GTF_CALL_NULLCHECK;
                }

#ifdef FEATURE_READYTORUN_COMPILER
                if (opts.IsReadyToRun())
                {
                    call->AsCall()->setEntryPoint(callInfo->codePointerLookup.constLookup);
                }
#endif
                break;
            }

            case CORINFO_CALL_CODE_POINTER:
            {
                // The EE has asked us to call by computing a code pointer and then doing an
                // indirect call.  This is because a runtime lookup is required to get the code entry point.

                // These calls always follow a uniform calling convention, i.e. no extra hidden params
                assert((sig->callConv & CORINFO_CALLCONV_PARAMTYPE) == 0);

                assert((sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_VARARG);
                assert((sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_NATIVEVARARG);

                GenTree* fptr =
                    impLookupToTree(pResolvedToken, &callInfo->codePointerLookup, GTF_ICON_FTN_ADDR, callInfo->hMethod);

                if (compDonotInline())
                {
                    return TYP_UNDEF;
                }

                // Now make an indirect call through the function pointer

                assert(fptr->TypeIs(TYP_I_IMPL));

                unsigned lclNum = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("Indirect call through function pointer"));
                GenTree* asg    = gtNewAssignNode(gtNewLclvNode(lclNum, TYP_I_IMPL), fptr);
                impAppendTree(asg, CHECK_SPILL_ALL, impCurStmtOffs);
                fptr = gtNewLclvNode(lclNum, TYP_I_IMPL);

                call = gtNewIndCallNode(fptr, callRetTyp, nullptr, ilOffset);
                call->gtFlags |= GTF_EXCEPT | (fptr->gtFlags & GTF_GLOB_EFFECT);
                if (callInfo->nullInstanceCheck)
                {
                    call->gtFlags |= GTF_CALL_NULLCHECK;
                }

                break;
            }

            default:
                assert(!"unknown call kind");
                break;
        }

        //-------------------------------------------------------------------------
        // Set more flags

        PREFIX_ASSUME(call != nullptr);

        if (mflags & CORINFO_FLG_NOGCCHECK)
        {
            call->AsCall()->gtCallMoreFlags |= GTF_CALL_M_NOGCCHECK;
        }

        // Mark call if it's one of the ones we will maybe treat as an intrinsic
        if (isSpecialIntrinsic)
        {
            call->AsCall()->gtCallMoreFlags |= GTF_CALL_M_SPECIAL_INTRINSIC;
        }
    }

    assert(sig != nullptr);
    assert((clsHnd != NO_CLASS_HANDLE) || (opcode == CEE_CALLI));

    /* Some sanity checks */

    // CALL_VIRT and NEWOBJ must have a THIS pointer
    assert((opcode != CEE_CALLVIRT && opcode != CEE_NEWOBJ) || (sig->callConv & CORINFO_CALLCONV_HASTHIS));
    // static bit and hasThis are negations of one another
    assert(((mflags & CORINFO_FLG_STATIC) != 0) == ((sig->callConv & CORINFO_CALLCONV_HASTHIS) == 0));
    assert(call != nullptr);

    /*-------------------------------------------------------------------------
     * Check special-cases etc
     */

    /* Special case - Check if it is a call to Delegate.Invoke(). */

    if (mflags & CORINFO_FLG_DELEGATE_INVOKE)
    {
        assert(!(mflags & CORINFO_FLG_STATIC)); // can't call a static method
        assert(mflags & CORINFO_FLG_FINAL);

        /* Set the delegate flag */
        call->AsCall()->gtCallMoreFlags |= GTF_CALL_M_DELEGATE_INV;

        if (callInfo->wrapperDelegateInvoke)
        {
            call->AsCall()->gtCallMoreFlags |= GTF_CALL_M_WRAPPER_DELEGATE_INV;
        }

        if (opcode == CEE_CALLVIRT)
        {
            assert(mflags & CORINFO_FLG_FINAL);

            /* It should have the GTF_CALL_NULLCHECK flag set. Reset it */
            assert(call->gtFlags & GTF_CALL_NULLCHECK);
            call->gtFlags &= ~GTF_CALL_NULLCHECK;
        }
    }

    CORINFO_CLASS_HANDLE actualMethodRetTypeSigClass;
    actualMethodRetTypeSigClass = sig->retTypeSigClass;

#if !FEATURE_VARARG
    /* Check for varargs */
    if ((sig->callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_VARARG ||
        (sig->callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_NATIVEVARARG)
    {
        BADCODE("Varargs not supported.");
    }
#endif // !FEATURE_VARARG

    if ((sig->callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_VARARG ||
        (sig->callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_NATIVEVARARG)
    {
        assert(!compIsForInlining());

        /* Set the right flags */

        call->gtFlags |= GTF_CALL_POP_ARGS;
        call->AsCall()->gtCallMoreFlags |= GTF_CALL_M_VARARGS;

        /* Can't allow tailcall for varargs as it is caller-pop. The caller
           will be expecting to pop a certain number of arguments, but if we
           tailcall to a function with a different number of arguments, we
           are hosed. There are ways around this (caller remembers esp value,
           varargs is not caller-pop, etc), but not worth it. */
        CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef TARGET_X86
        if (canTailCall)
        {
            canTailCall             = false;
            szCanTailCallFailReason = "Callee is varargs";
        }
#endif

        /* Get the total number of arguments - this is already correct
         * for CALLI - for methods we have to get it from the call site */

        if (opcode != CEE_CALLI)
        {
#ifdef DEBUG
            unsigned numArgsDef = sig->numArgs;
#endif
            eeGetCallSiteSig(pResolvedToken->token, pResolvedToken->tokenScope, pResolvedToken->tokenContext, sig);

            // For vararg calls we must be sure to load the return type of the
            // method actually being called, as well as the return types of the
            // specified in the vararg signature. With type equivalency, these types
            // may not be the same.
            if (sig->retTypeSigClass != actualMethodRetTypeSigClass)
            {
                if (actualMethodRetTypeSigClass != nullptr && sig->retType != CORINFO_TYPE_CLASS &&
                    sig->retType != CORINFO_TYPE_BYREF && sig->retType != CORINFO_TYPE_PTR &&
                    sig->retType != CORINFO_TYPE_VAR)
                {
                    // Make sure that all valuetypes (including enums) that we push are loaded.
                    // This is to guarantee that if a GC is triggerred from the prestub of this methods,
                    // all valuetypes in the method signature are already loaded.
                    // We need to be able to find the size of the valuetypes, but we cannot
                    // do a class-load from within GC.
                    info.compCompHnd->classMustBeLoadedBeforeCodeIsRun(actualMethodRetTypeSigClass);
                }
            }

            assert(numArgsDef <= sig->numArgs);
        }

        /* We will have "cookie" as the last argument but we cannot push
         * it on the operand stack because we may overflow, so we append it
         * to the arg list next after we pop them */
    }

    //--------------------------- Inline NDirect ------------------------------

    // For inline cases we technically should look at both the current
    // block and the call site block (or just the latter if we've
    // fused the EH trees). However the block-related checks pertain to
    // EH and we currently won't inline a method with EH. So for
    // inlinees, just checking the call site block is sufficient.
    {
        // New lexical block here to avoid compilation errors because of GOTOs.
        BasicBlock* block = compIsForInlining() ? impInlineInfo->iciBlock : compCurBB;
        impCheckForPInvokeCall(call->AsCall(), methHnd, sig, mflags, block);
    }

#ifdef UNIX_X86_ABI
    // On Unix x86 we use caller-cleaned convention.
    if ((call->gtFlags & GTF_CALL_UNMANAGED) == 0)
        call->gtFlags |= GTF_CALL_POP_ARGS;
#endif // UNIX_X86_ABI

    if (call->gtFlags & GTF_CALL_UNMANAGED)
    {
        // We set up the unmanaged call by linking the frame, disabling GC, etc
        // This needs to be cleaned up on return
        if (canTailCall)
        {
            canTailCall             = false;
            szCanTailCallFailReason = "Callee is native";
        }

        checkForSmallType = true;

        impPopArgsForUnmanagedCall(call, sig);

        goto DONE;
    }
    else if ((opcode == CEE_CALLI) && ((sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_DEFAULT) &&
             ((sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_VARARG))
    {
        if (!info.compCompHnd->canGetCookieForPInvokeCalliSig(sig))
        {
            // Normally this only happens with inlining.
            // However, a generic method (or type) being NGENd into another module
            // can run into this issue as well.  There's not an easy fall-back for NGEN
            // so instead we fallback to JIT.
            if (compIsForInlining())
            {
                compInlineResult->NoteFatal(InlineObservation::CALLSITE_CANT_EMBED_PINVOKE_COOKIE);
            }
            else
            {
                IMPL_LIMITATION("Can't get PInvoke cookie (cross module generics)");
            }

            return TYP_UNDEF;
        }

        GenTree* cookie = eeGetPInvokeCookie(sig);

        // This cookie is required to be either a simple GT_CNS_INT or
        // an indirection of a GT_CNS_INT
        //
        GenTree* cookieConst = cookie;
        if (cookie->gtOper == GT_IND)
        {
            cookieConst = cookie->AsOp()->gtOp1;
        }
        assert(cookieConst->gtOper == GT_CNS_INT);

        // Setting GTF_DONT_CSE on the GT_CNS_INT as well as on the GT_IND (if it exists) will ensure that
        // we won't allow this tree to participate in any CSE logic
        //
        cookie->gtFlags |= GTF_DONT_CSE;
        cookieConst->gtFlags |= GTF_DONT_CSE;

        call->AsCall()->gtCallCookie = cookie;

        if (canTailCall)
        {
            canTailCall             = false;
            szCanTailCallFailReason = "PInvoke calli";
        }
    }

    /*-------------------------------------------------------------------------
     * Create the argument list
     */

    //-------------------------------------------------------------------------
    // Special case - for varargs we have an implicit last argument

    if ((sig->callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_VARARG)
    {
        assert(!compIsForInlining());

        void *varCookie, *pVarCookie;
        if (!info.compCompHnd->canGetVarArgsHandle(sig))
        {
            compInlineResult->NoteFatal(InlineObservation::CALLSITE_CANT_EMBED_VARARGS_COOKIE);
            return TYP_UNDEF;
        }

        varCookie = info.compCompHnd->getVarArgsHandle(sig, &pVarCookie);
        assert((!varCookie) != (!pVarCookie));
        GenTree* cookie = gtNewIconEmbHndNode(varCookie, pVarCookie, GTF_ICON_VARG_HDL, sig);

        assert(extraArg == nullptr);
        extraArg = gtNewCallArgs(cookie);
    }

    //-------------------------------------------------------------------------
    // Extra arg for shared generic code and array methods
    //
    // Extra argument containing instantiation information is passed in the
    // following circumstances:
    // (a) To the "Address" method on array classes; the extra parameter is
    //     the array's type handle (a TypeDesc)
    // (b) To shared-code instance methods in generic structs; the extra parameter
    //     is the struct's type handle (a vtable ptr)
    // (c) To shared-code per-instantiation non-generic static methods in generic
    //     classes and structs; the extra parameter is the type handle
    // (d) To shared-code generic methods; the extra parameter is an
    //     exact-instantiation MethodDesc
    //
    // We also set the exact type context associated with the call so we can
    // inline the call correctly later on.

    if (sig->callConv & CORINFO_CALLCONV_PARAMTYPE)
    {
        assert(call->AsCall()->gtCallType == CT_USER_FUNC);
        if (clsHnd == nullptr)
        {
            NO_WAY("CALLI on parameterized type");
        }

        assert(opcode != CEE_CALLI);

        GenTree* instParam;
        BOOL     runtimeLookup;

        // Instantiated generic method
        if (((SIZE_T)exactContextHnd & CORINFO_CONTEXTFLAGS_MASK) == CORINFO_CONTEXTFLAGS_METHOD)
        {
            assert(exactContextHnd != METHOD_BEING_COMPILED_CONTEXT());

            CORINFO_METHOD_HANDLE exactMethodHandle =
                (CORINFO_METHOD_HANDLE)((SIZE_T)exactContextHnd & ~CORINFO_CONTEXTFLAGS_MASK);

            if (!exactContextNeedsRuntimeLookup)
            {
#ifdef FEATURE_READYTORUN_COMPILER
                if (opts.IsReadyToRun())
                {
                    instParam =
                        impReadyToRunLookupToTree(&callInfo->instParamLookup, GTF_ICON_METHOD_HDL, exactMethodHandle);
                    if (instParam == nullptr)
                    {
                        assert(compDonotInline());
                        return TYP_UNDEF;
                    }
                }
                else
#endif
                {
                    instParam = gtNewIconEmbMethHndNode(exactMethodHandle);
                    info.compCompHnd->methodMustBeLoadedBeforeCodeIsRun(exactMethodHandle);
                }
            }
            else
            {
                instParam = impTokenToHandle(pResolvedToken, &runtimeLookup, TRUE /*mustRestoreHandle*/);
                if (instParam == nullptr)
                {
                    assert(compDonotInline());
                    return TYP_UNDEF;
                }
            }
        }

        // otherwise must be an instance method in a generic struct,
        // a static method in a generic type, or a runtime-generated array method
        else
        {
            assert(((SIZE_T)exactContextHnd & CORINFO_CONTEXTFLAGS_MASK) == CORINFO_CONTEXTFLAGS_CLASS);
            CORINFO_CLASS_HANDLE exactClassHandle = eeGetClassFromContext(exactContextHnd);

            if (compIsForInlining() && (clsFlags & CORINFO_FLG_ARRAY) != 0)
            {
                compInlineResult->NoteFatal(InlineObservation::CALLEE_IS_ARRAY_METHOD);
                return TYP_UNDEF;
            }

            if ((clsFlags & CORINFO_FLG_ARRAY) && isReadonlyCall)
            {
                // We indicate "readonly" to the Address operation by using a null
                // instParam.
                instParam = gtNewIconNode(0, TYP_REF);
            }
            else if (!exactContextNeedsRuntimeLookup)
            {
#ifdef FEATURE_READYTORUN_COMPILER
                if (opts.IsReadyToRun())
                {
                    instParam =
                        impReadyToRunLookupToTree(&callInfo->instParamLookup, GTF_ICON_CLASS_HDL, exactClassHandle);
                    if (instParam == nullptr)
                    {
                        assert(compDonotInline());
                        return TYP_UNDEF;
                    }
                }
                else
#endif
                {
                    instParam = gtNewIconEmbClsHndNode(exactClassHandle);
                    info.compCompHnd->classMustBeLoadedBeforeCodeIsRun(exactClassHandle);
                }
            }
            else
            {
                // If the EE was able to resolve a constrained call, the instantiating parameter to use is the type
                // by which the call was constrained with. We embed pConstrainedResolvedToken as the extra argument
                // because pResolvedToken is an interface method and interface types make a poor generic context.
                if (pConstrainedResolvedToken)
                {
                    instParam = impTokenToHandle(pConstrainedResolvedToken, &runtimeLookup, TRUE /*mustRestoreHandle*/,
                                                 FALSE /* importParent */);
                }
                else
                {
                    instParam = impParentClassTokenToHandle(pResolvedToken, &runtimeLookup, TRUE /*mustRestoreHandle*/);
                }

                if (instParam == nullptr)
                {
                    assert(compDonotInline());
                    return TYP_UNDEF;
                }
            }
        }

        assert(extraArg == nullptr);
        extraArg = gtNewCallArgs(instParam);
    }

    if ((opcode == CEE_NEWOBJ) && ((clsFlags & CORINFO_FLG_DELEGATE) != 0))
    {
        // Only verifiable cases are supported.
        // dup; ldvirtftn; newobj; or ldftn; newobj.
        // IL test could contain unverifiable sequence, in this case optimization should not be done.
        if (impStackHeight() > 0)
        {
            typeInfo delegateTypeInfo = impStackTop().seTypeInfo;
            if (delegateTypeInfo.IsToken())
            {
                ldftnToken = delegateTypeInfo.GetToken();
            }
        }
    }

    //-------------------------------------------------------------------------
    // The main group of arguments

    args                       = impPopCallArgs(sig->numArgs, sig, extraArg);
    call->AsCall()->gtCallArgs = args;

    for (GenTreeCall::Use& use : call->AsCall()->Args())
    {
        call->gtFlags |= use.GetNode()->gtFlags & GTF_GLOB_EFFECT;
    }

    //-------------------------------------------------------------------------
    // The "this" pointer

    if (((mflags & CORINFO_FLG_STATIC) == 0) && ((sig->callConv & CORINFO_CALLCONV_EXPLICITTHIS) == 0) &&
        !((opcode == CEE_NEWOBJ) && (newobjThis == nullptr)))
    {
        GenTree* obj;

        if (opcode == CEE_NEWOBJ)
        {
            obj = newobjThis;
        }
        else
        {
            obj = impPopStack().val;
            obj = impTransformThis(obj, pConstrainedResolvedToken, constraintCallThisTransform);
            if (compDonotInline())
            {
                return TYP_UNDEF;
            }
        }

        // Store the "this" value in the call
        call->gtFlags |= obj->gtFlags & GTF_GLOB_EFFECT;
        call->AsCall()->gtCallThisArg = gtNewCallArgs(obj);

        // Is this a virtual or interface call?
        if (call->AsCall()->IsVirtual())
        {
            // only true object pointers can be virtual
            assert(obj->gtType == TYP_REF);

            // See if we can devirtualize.

            const bool isExplicitTailCall     = (tailCallFlags & PREFIX_TAILCALL_EXPLICIT) != 0;
            const bool isLateDevirtualization = false;
            impDevirtualizeCall(call->AsCall(), &callInfo->hMethod, &callInfo->methodFlags, &callInfo->contextHandle,
                                &exactContextHnd, isLateDevirtualization, isExplicitTailCall, rawILOffset);
        }

        if (impIsThis(obj))
        {
            call->AsCall()->gtCallMoreFlags |= GTF_CALL_M_NONVIRT_SAME_THIS;
        }
    }

    //-------------------------------------------------------------------------
    // The "this" pointer for "newobj"

    if (opcode == CEE_NEWOBJ)
    {
        if (clsFlags & CORINFO_FLG_VAROBJSIZE)
        {
            assert(!(clsFlags & CORINFO_FLG_ARRAY)); // arrays handled separately
            // This is a 'new' of a variable sized object, wher
            // the constructor is to return the object.  In this case
            // the constructor claims to return VOID but we know it
            // actually returns the new object
            assert(callRetTyp == TYP_VOID);
            callRetTyp = TYP_REF;
            call->SetType(TYP_REF);
            call->AsCall()->SetRetSigType(TYP_REF);
            impSpillSpecialSideEff();

            impPushOnStack(call, typeInfo(TI_REF, clsHnd));
        }
        else
        {
            if (clsFlags & CORINFO_FLG_DELEGATE)
            {
                // New inliner morph it in impImportCall.
                // This will allow us to inline the call to the delegate constructor.
                call = fgOptimizeDelegateConstructor(call->AsCall(), &exactContextHnd, ldftnToken);
            }

#if defined(DEBUG) || defined(INLINE_DATA)
            // Keep track of the raw IL offset of the call
            call->AsCall()->gtRawILOffset = rawILOffset;
#endif // defined(DEBUG) || defined(INLINE_DATA)

            // Is it an inline candidate?
            impMarkInlineCandidate(call->AsCall(), exactContextHnd, exactContextNeedsRuntimeLookup, callInfo);

            // append the call node.
            impAppendTree(call, (unsigned)CHECK_SPILL_ALL, impCurStmtOffs);

            // Now push the value of the 'new onto the stack

            // This is a 'new' of a non-variable sized object.
            // Append the new node (op1) to the statement list,
            // and then push the local holding the value of this
            // new instruction on the stack.

            if ((clsFlags & CORINFO_FLG_VALUECLASS) != 0)
            {
                assert(newobjThis->OperIs(GT_ADDR) && newobjThis->AsUnOp()->GetOp(0)->OperIs(GT_LCL_VAR));

                unsigned tmp = newobjThis->AsUnOp()->GetOp(0)->AsLclVar()->GetLclNum();
                impPushOnStack(gtNewLclvNode(tmp, lvaGetDesc(tmp)->GetType()), typeInfo(TI_STRUCT, clsHnd));
            }
            else
            {
                if (newobjThis->gtOper == GT_COMMA)
                {
                    // We must have inserted the callout. Get the real newobj.
                    newobjThis = newobjThis->AsOp()->gtOp2;
                }

                assert(newobjThis->gtOper == GT_LCL_VAR);
                impPushOnStack(gtNewLclvNode(newobjThis->AsLclVarCommon()->GetLclNum(), TYP_REF),
                               typeInfo(TI_REF, clsHnd));
            }
        }
        return callRetTyp;
    }

DONE:

#ifdef DEBUG
    // In debug we want to be able to register callsites with the EE.
    assert(call->AsCall()->callSig == nullptr);
    call->AsCall()->callSig  = new (this, CMK_Generic) CORINFO_SIG_INFO;
    *call->AsCall()->callSig = *sig;
#endif

    if (call->TypeIs(TYP_STRUCT))
    {
        if ((clsFlags & CORINFO_FLG_ARRAY) != 0)
        {
            eeGetCallSiteSig(pResolvedToken->token, pResolvedToken->tokenScope, pResolvedToken->tokenContext,
                             &callSiteSig);
            hasCallSiteSig = true;
            impInitializeStructCall(call->AsCall(), callSiteSig.retTypeClass);
        }
        else
        {
            impInitializeStructCall(call->AsCall(), sig->retTypeClass);
        }
    }

    // Final importer checks for calls flagged as tail calls.
    //
    if (tailCallFlags != 0)
    {
        const bool isExplicitTailCall = (tailCallFlags & PREFIX_TAILCALL_EXPLICIT) != 0;
        const bool isImplicitTailCall = (tailCallFlags & PREFIX_TAILCALL_IMPLICIT) != 0;
        const bool isStressTailCall   = (tailCallFlags & PREFIX_TAILCALL_STRESS) != 0;

        // Exactly one of these should be true.
        assert(isExplicitTailCall != isImplicitTailCall);

        // This check cannot be performed for implicit tail calls for the reason
        // that impIsImplicitTailCallCandidate() is not checking whether return
        // types are compatible before marking a call node with PREFIX_TAILCALL_IMPLICIT.
        // As a result it is possible that in the following case, we find that
        // the type stack is non-empty if Callee() is considered for implicit
        // tail calling.
        //      int Caller(..) { .... void Callee(); ret val; ... }
        //
        // Note that we cannot check return type compatibility before ImpImportCall()
        // as we don't have required info or need to duplicate some of the logic of
        // ImpImportCall().
        //
        // For implicit tail calls, we perform this check after return types are
        // known to be compatible.
        if (isExplicitTailCall && (verCurrentState.esStackDepth != 0))
        {
            BADCODE("Stack should be empty after tailcall");
        }

        if (canTailCall && !impTailCallRetTypeCompatible(call->AsCall()))
        {
            canTailCall             = false;
            szCanTailCallFailReason = "Return types are not tail call compatible";
        }

        // Stack empty check for implicit tail calls.
        if (canTailCall && isImplicitTailCall && (verCurrentState.esStackDepth != 0))
        {
            BADCODE("Stack should be empty after tailcall");
        }

        // assert(compCurBB is not a catch, finally or filter block);
        // assert(compCurBB is not a try block protected by a finally block);
        assert(!isExplicitTailCall || compCurBB->bbJumpKind == BBJ_RETURN);

        // Ask VM for permission to tailcall
        if (canTailCall)
        {
            // True virtual or indirect calls, shouldn't pass in a callee handle.
            CORINFO_METHOD_HANDLE exactCalleeHnd =
                ((call->AsCall()->gtCallType != CT_USER_FUNC) || call->AsCall()->IsVirtual()) ? nullptr : methHnd;

            if (info.compCompHnd->canTailCall(info.compMethodHnd, methHnd, exactCalleeHnd, isExplicitTailCall))
            {
                if (isExplicitTailCall)
                {
                    // In case of explicit tail calls, mark it so that it is not considered
                    // for in-lining.
                    call->AsCall()->gtCallMoreFlags |= GTF_CALL_M_EXPLICIT_TAILCALL;
                    JITDUMP("\nGTF_CALL_M_EXPLICIT_TAILCALL set for call [%06u]\n", dspTreeID(call));

                    if (isStressTailCall)
                    {
                        call->AsCall()->gtCallMoreFlags |= GTF_CALL_M_STRESS_TAILCALL;
                        JITDUMP("\nGTF_CALL_M_STRESS_TAILCALL set for call [%06u]\n", dspTreeID(call));
                    }
                }
                else
                {
#if FEATURE_TAILCALL_OPT
                    // Must be an implicit tail call.
                    assert(isImplicitTailCall);

                    // It is possible that a call node is both an inline candidate and marked
                    // for opportunistic tail calling.  In-lining happens before morhphing of
                    // trees.  If in-lining of an in-line candidate gets aborted for whatever
                    // reason, it will survive to the morphing stage at which point it will be
                    // transformed into a tail call after performing additional checks.

                    call->AsCall()->gtCallMoreFlags |= GTF_CALL_M_IMPLICIT_TAILCALL;
                    JITDUMP("\nGTF_CALL_M_IMPLICIT_TAILCALL set for call [%06u]\n", dspTreeID(call));

#else //! FEATURE_TAILCALL_OPT
                    NYI("Implicit tail call prefix on a target which doesn't support opportunistic tail calls");

#endif // FEATURE_TAILCALL_OPT
                }

                // This might or might not turn into a tailcall. We do more
                // checks in morph. For explicit tailcalls we need more
                // information in morph in case it turns out to be a
                // helper-based tailcall.
                if (isExplicitTailCall)
                {
                    assert(call->AsCall()->tailCallInfo == nullptr);
                    call->AsCall()->tailCallInfo = new (this, CMK_CorTailCallInfo) TailCallSiteInfo;
                    switch (opcode)
                    {
                        case CEE_CALLI:
                            call->AsCall()->tailCallInfo->SetCalli(sig);
                            break;
                        case CEE_CALLVIRT:
                            call->AsCall()->tailCallInfo->SetCallvirt(sig, pResolvedToken);
                            break;
                        default:
                            call->AsCall()->tailCallInfo->SetCall(sig, pResolvedToken);
                            break;
                    }
                }
            }
            else
            {
                // canTailCall reported its reasons already
                canTailCall = false;
                JITDUMP("\ninfo.compCompHnd->canTailCall returned false for call [%06u]\n", dspTreeID(call));
            }
        }
        else
        {
            // If this assert fires it means that canTailCall was set to false without setting a reason!
            assert(szCanTailCallFailReason != nullptr);
            JITDUMP("\nRejecting %splicit tail call for  [%06u]\n", isExplicitTailCall ? "ex" : "im", dspTreeID(call),
                    szCanTailCallFailReason);
            info.compCompHnd->reportTailCallDecision(info.compMethodHnd, methHnd, isExplicitTailCall, TAILCALL_FAIL,
                                                     szCanTailCallFailReason);
        }
    }

    // A tail recursive call is a potential loop from the current block to the start of the method.
    if ((tailCallFlags != 0) && canTailCall && gtIsRecursiveCall(methHnd))
    {
        assert(verCurrentState.esStackDepth == 0);
        BasicBlock* loopHead = nullptr;
        if (opts.IsOSR())
        {
            // We might not have been planning on importing the method
            // entry block, but now we must.

            // We should have remembered the real method entry block.
            assert(fgEntryBB != nullptr);

            JITDUMP("\nOSR: found tail recursive call in the method, scheduling " FMT_BB " for importation\n",
                    fgEntryBB->bbNum);
            impImportBlockPending(fgEntryBB);
            loopHead = fgEntryBB;
        }
        else
        {
            // For normal jitting we'll branch back to the firstBB; this
            // should already be imported.
            loopHead = fgFirstBB;
        }

        JITDUMP("\nFound tail recursive call in the method. Mark " FMT_BB " to " FMT_BB
                " as having a backward branch.\n",
                loopHead->bbNum, compCurBB->bbNum);
        fgMarkBackwardJump(loopHead, compCurBB);
    }

    // Note: we assume that small return types are already normalized by the managed callee
    // or by the pinvoke stub for calls to unmanaged code.

    if (compIsForInlining() && opcode == CEE_CALLVIRT)
    {
        GenTree* callObj = call->AsCall()->gtCallThisArg->GetNode();

        if ((call->AsCall()->IsVirtual() || (call->gtFlags & GTF_CALL_NULLCHECK)) &&
            impInlineIsGuaranteedThisDerefBeforeAnySideEffects(nullptr, call->AsCall()->gtCallArgs, callObj))
        {
            impInlineInfo->thisDereferencedFirst = true;
        }
    }

#if defined(DEBUG) || defined(INLINE_DATA)
    // Keep track of the raw IL offset of the call
    call->AsCall()->gtRawILOffset = rawILOffset;
#endif // defined(DEBUG) || defined(INLINE_DATA)

    // Is it an inline candidate?
    impMarkInlineCandidate(call->AsCall(), exactContextHnd, exactContextNeedsRuntimeLookup, callInfo);

    if ((sig->flags & CORINFO_SIGFLAG_FAT_CALL) != 0)
    {
        assert(opcode == CEE_CALLI);
        addFatPointerCandidate(call->AsCall());
    }

DONE_INTRINSIC:
    // Push or append the result of the call
    if (callRetTyp == TYP_VOID)
    {
        if (opcode == CEE_NEWOBJ)
        {
            // we actually did push something, so don't spill the thing we just pushed.
            assert(verCurrentState.esStackDepth > 0);
            impAppendTree(call, verCurrentState.esStackDepth - 1, impCurStmtOffs);
        }
        else
        {
            impAppendTree(call, (unsigned)CHECK_SPILL_ALL, impCurStmtOffs);
        }
    }
    else
    {
        impSpillSpecialSideEff();

        if ((clsFlags & CORINFO_FLG_ARRAY) != 0)
        {
            if (!hasCallSiteSig)
            {
                eeGetCallSiteSig(pResolvedToken->token, pResolvedToken->tokenScope, pResolvedToken->tokenContext,
                                 &callSiteSig);
            }

            sig = &callSiteSig;
        }

        if (GenTreeCall* origCall = call->IsCall())
        {
            // Sometimes "call" is not a GT_CALL (if we imported an intrinsic that didn't turn into a call)

            const bool isFatPointerCandidate              = origCall->IsFatPointerCandidate();
            const bool isInlineCandidate                  = origCall->IsInlineCandidate();
            const bool isGuardedDevirtualizationCandidate = origCall->IsGuardedDevirtualizationCandidate();

            if (varTypeIsStruct(origCall->GetType()))
            {
#if FEATURE_MULTIREG_RET
                if ((origCall->GetRegCount() > 1) && !origCall->CanTailCall() && !isInlineCandidate)
                {
                    call = impCanonicalizeMultiRegCall(origCall);
                }
#endif
            }

            // TODO: consider handling fatcalli cases this way too...?
            if (isInlineCandidate || isGuardedDevirtualizationCandidate)
            {
                // We should not have made any adjustments in impCanonicalizeMultiRegCall
                // as we defer those until we know the fate of the call.

                // TODO-MIKE-Review: This seems broken. impCanonicalizeMultiRegCall is not
                // called for inline candidates but it is called for guarded devirtualization
                // candidates.

                noway_assert(call == origCall);

                assert(opts.OptEnabled(CLFLG_INLINING));
                assert(!isFatPointerCandidate); // We should not try to inline calli.

                // Make the call its own tree (spill the stack if needed).
                impAppendTree(origCall, CHECK_SPILL_ALL, impCurStmtOffs);

                // TODO: Still using the widened type.
                GenTreeRetExpr* retExpr = gtNewRetExpr(origCall, origCall->GetType());

                // Link the retExpr to the call so if necessary we can manipulate it later.
                origCall->gtInlineCandidateInfo->retExprPlaceholder = retExpr;

                // Propagate retExpr as the placeholder for the call.
                call = retExpr;
            }
            else
            {
                if (isFatPointerCandidate)
                {
                    // fatPointer candidates should be in statements of the form call() or var = call().
                    // Such form allows to find statements with fat calls without walking through whole trees
                    // and removes problems with cutting trees.
                    assert(!bIntrinsicImported);
                    assert(IsTargetAbi(CORINFO_CORERT_ABI));
                    if (call->OperGet() != GT_LCL_VAR) // can be already converted by impCanonicalizeMultiRegCall.
                    {
                        unsigned calliTempLclNum = lvaGrabTemp(true DEBUGARG("calli"));
                        impAssignTempGen(calliTempLclNum, call, sig->retTypeClass, CHECK_SPILL_NONE);
                        call = gtNewLclvNode(calliTempLclNum, varActualType(lvaGetDesc(calliTempLclNum)->GetType()));
                    }
                }

                // For non-candidates we must also spill, since we
                // might have locals live on the eval stack that this
                // call can modify.
                //
                // Suppress this for certain well-known call targets
                // that we know won't modify locals, eg calls that are
                // recognized in gtCanOptimizeTypeEquality. Otherwise
                // we may break key fragile pattern matches later on.
                bool spillStack = true;
                if (call->IsCall())
                {
                    GenTreeCall* callNode = call->AsCall();
                    if ((callNode->gtCallType == CT_HELPER) && (gtIsTypeHandleToRuntimeTypeHelper(callNode) ||
                                                                gtIsTypeHandleToRuntimeTypeHandleHelper(callNode)))
                    {
                        spillStack = false;
                    }
                    else if ((callNode->gtCallMoreFlags & GTF_CALL_M_SPECIAL_INTRINSIC) != 0)
                    {
                        spillStack = false;
                    }
                }

                if (spillStack)
                {
                    impSpillSideEffects(true, CHECK_SPILL_ALL DEBUGARG("non-inline candidate call"));
                }
            }
        }

        if (!bIntrinsicImported)
        {
            //-------------------------------------------------------------------------
            //
            /* If the call is of a small type and the callee is managed, the callee will normalize the result
                before returning.
                However, we need to normalize small type values returned by unmanaged
                functions (pinvoke). The pinvoke stub does the normalization, but we need to do it here
                if we use the shorter inlined pinvoke stub. */

            if (checkForSmallType && varTypeIsIntegral(callRetTyp) && genTypeSize(callRetTyp) < genTypeSize(TYP_INT))
            {
                call = gtNewCastNode(genActualType(callRetTyp), call, false, callRetTyp);
            }
        }

        if (sig->retTypeClass != NO_CLASS_HANDLE)
        {
            impPushOnStack(call, impMakeTypeInfo(sig->retType, sig->retTypeClass));
        }
        else
        {
            impPushOnStack(call, typeInfo());
        }
    }

    // VSD functions get a new call target each time we getCallInfo, so clear the cache.
    // Also, the call info cache for CALLI instructions is largely incomplete, so clear it out.
    // if ( (opcode == CEE_CALLI) || (callInfoCache.fetchCallInfo().kind == CORINFO_VIRTUALCALL_STUB))
    //  callInfoCache.uncacheCallInfo();

    return callRetTyp;
}
#ifdef _PREFAST_
#pragma warning(pop)
#endif

void Compiler::impInitializeStructCall(GenTreeCall* call, CORINFO_CLASS_HANDLE retClass)
{
    assert(call->GetType() == TYP_STRUCT);
    assert(call->GetRetSigType() == TYP_STRUCT);
    assert(call->GetRetLayout() == nullptr);

    ClassLayout* layout = typGetObjLayout(retClass);
    var_types    type   = typGetStructType(layout);

    call->SetType(type);
    call->SetRetSigType(type);
    call->SetRetLayout(layout);

    StructPassing   retKind = abiGetStructReturnType(layout, call->GetUnmanagedCallConv());
    ReturnTypeDesc* retDesc = call->GetRetDesc();

    if (retKind.kind == SPK_PrimitiveType)
    {
        retDesc->InitializePrimitive(retKind.type);
    }
#if FEATURE_MULTIREG_RET
    else if ((retKind.kind == SPK_ByValue) || (retKind.kind == SPK_ByValueAsHfa))
    {
        retDesc->InitializeStruct(this, layout, retKind.kind, retKind.type);
    }
#endif
    else
    {
        assert(retKind.kind == SPK_ByReference);

        // JIT generated code does return the return buffer arg if required
        // by the ABI but it does not use the return value so calls have no
        // return registers in this case.
        assert(retDesc->GetRegCount() == 0);

        call->gtCallMoreFlags |= GTF_CALL_M_RETBUFFARG;
    }
}

#if FEATURE_MULTIREG_RET

GenTree* Compiler::impCanonicalizeMultiRegCall(GenTreeCall* call)
{
    // Multireg return calls have limited support in IR - basically they can only
    // be assigned to locals or "returned" if they're tail calls.
    // For inline candidate calls this transform is deferred to the inliner.

    assert(varTypeIsStruct(call->GetType()));
    assert((call->GetRegCount() > 1) && !call->CanTailCall() && !call->IsInlineCandidate());

    unsigned tempLclNum = lvaNewTemp(call->GetRetLayout(), true DEBUGARG("multireg return call temp"));
    // Make sure that this local doesn't get promoted.
    lvaGetDesc(tempLclNum)->lvIsMultiRegRet = true;

    GenTree* temp = gtNewLclvNode(tempLclNum, lvaGetDesc(tempLclNum)->GetType());
    GenTree* asg  = impAssignStruct(temp, call, call->GetRetLayout(), CHECK_SPILL_ALL);
    impAppendTree(asg, CHECK_SPILL_ALL, impCurStmtOffs);

    temp = gtNewLclvNode(tempLclNum, temp->GetType());
    // TODO-1stClassStructs: Handle constant propagation and CSE-ing of multireg returns.
    temp->gtFlags |= GTF_DONT_CSE;

    return temp;
}

GenTree* Compiler::impCanonicalizeMultiRegReturnValue(GenTree* value, CORINFO_CLASS_HANDLE retClass)
{
    assert(varTypeIsStruct(info.compRetType));
    assert(info.compRetBuffArg == BAD_VAR_NUM);
    assert(info.retDesc.GetRegCount() > 1);

#ifndef FEATURE_MULTIREG_RET
    unreached();
#else
    // In case of multi-reg struct return, we force IR to be one of the following:
    // RETURN(LCL_VAR) or RETURN(CALL). If op is anything other than a LCL_VAR or
    // a CALL, it is assigned to a temp that is then returned.

    if (GenTreeCall* call = value->IsCall())
    {
#ifndef TARGET_ARMARCH
        return value;
#else
        if (!call->IsVarargs())
        {
            return value;
        }

        // We cannot tail call because control needs to return to fixup the calling
        // convention for result return.
        call->gtCallMoreFlags &= ~GTF_CALL_M_TAILCALL;
        call->gtCallMoreFlags &= ~GTF_CALL_M_EXPLICIT_TAILCALL;
#endif
    }

    LclVarDsc* lcl = nullptr;

    if (value->OperIs(GT_LCL_VAR))
    {
        lcl = lvaGetDesc(value->AsLclVar());

        if (lcl->IsImplicitByRefParam())
        {
            // Implicit byref params will be transformed into indirs so
            // we need a temp even if now they're LCL_VARs.

            lcl = nullptr;
        }
    }

    if (lcl == nullptr)
    {
        unsigned tempLclNum = lvaNewTemp(retClass, true DEBUGARG("multireg return temp"));
        GenTree* temp       = gtNewLclvNode(tempLclNum, lvaGetDesc(tempLclNum)->GetType());
        GenTree* asg        = impAssignStruct(temp, value, retClass, CHECK_SPILL_ALL);
        impAppendTree(asg, CHECK_SPILL_ALL, impCurStmtOffs);

        lcl   = lvaGetDesc(tempLclNum);
        value = gtNewLclvNode(tempLclNum, lcl->GetType());
    }

    // Make sure that this struct stays in memory and doesn't get promoted.
    lcl->lvIsMultiRegRet = true;

    // TODO-1stClassStructs: Handle constant propagation and CSE-ing of multireg returns.
    value->gtFlags |= GTF_DONT_CSE;

    return value;
#endif
}

#endif // FEATURE_MULTIREG_RET

GenTree* Compiler::impSpillPseudoReturnBufferCall(GenTree* value, CORINFO_CLASS_HANDLE retClass)
{
    assert(value->IsCall() && value->AsCall()->TreatAsHasRetBufArg());

    // This must be one of those 'special' helpers that don't
    // really have a return buffer, but instead use it as a way
    // to keep the trees cleaner with fewer address-taken temps.
    //
    // Well now we have to materialize the the return buffer as
    // an address-taken temp. Then we can return the temp.
    //
    // NOTE: this code assumes that since the call directly
    // feeds the return, then the call must be returning the
    // same structure/class/type.

    unsigned tmpNum = lvaGrabTemp(true DEBUGARG("pseudo return buffer"));

    // No need to spill anything as we're about to return.
    impAssignTempGen(tmpNum, value, retClass, CHECK_SPILL_NONE);
    return gtNewLclvNode(tmpNum, info.compRetType);
}

/*****************************************************************************
   CEE_LEAVE may be jumping out of a protected block, viz, a catch or a
   finally-protected try. We find the finally blocks protecting the current
   offset (in order) by walking over the complete exception table and
   finding enclosing clauses. This assumes that the table is sorted.
   This will create a series of BBJ_CALLFINALLY -> BBJ_CALLFINALLY ... -> BBJ_ALWAYS.

   If we are leaving a catch handler, we need to attach the
   CPX_ENDCATCHes to the correct BBJ_CALLFINALLY blocks.

   After this function, the BBJ_LEAVE block has been converted to a different type.
 */

#if !defined(FEATURE_EH_FUNCLETS)

void Compiler::impImportLeave(BasicBlock* block)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("\nBefore import CEE_LEAVE:\n");
        fgDispBasicBlocks();
        fgDispHandlerTab();
    }
#endif // DEBUG

    bool        invalidatePreds = false; // If we create new blocks, invalidate the predecessor lists (if created)
    unsigned    blkAddr         = block->bbCodeOffs;
    BasicBlock* leaveTarget     = block->bbJumpDest;
    unsigned    jmpAddr         = leaveTarget->bbCodeOffs;

    // LEAVE clears the stack, spill side effects, and set stack to 0

    impSpillSideEffects(true, (unsigned)CHECK_SPILL_ALL DEBUGARG("impImportLeave"));
    verCurrentState.esStackDepth = 0;

    assert(block->bbJumpKind == BBJ_LEAVE);
    assert(fgBBs == (BasicBlock**)0xCDCD || fgLookupBB(jmpAddr) != NULL); // should be a BB boundary

    BasicBlock* step         = DUMMY_INIT(NULL);
    unsigned    encFinallies = 0; // Number of enclosing finallies.
    GenTree*    endCatches   = NULL;
    Statement*  endLFinStmt  = NULL; // The statement tree to indicate the end of locally-invoked finally.

    unsigned  XTnum;
    EHblkDsc* HBtab;

    for (XTnum = 0, HBtab = compHndBBtab; XTnum < compHndBBtabCount; XTnum++, HBtab++)
    {
        // Grab the handler offsets

        IL_OFFSET tryBeg = HBtab->ebdTryBegOffs();
        IL_OFFSET tryEnd = HBtab->ebdTryEndOffs();
        IL_OFFSET hndBeg = HBtab->ebdHndBegOffs();
        IL_OFFSET hndEnd = HBtab->ebdHndEndOffs();

        /* Is this a catch-handler we are CEE_LEAVEing out of?
         * If so, we need to call CORINFO_HELP_ENDCATCH.
         */

        if (jitIsBetween(blkAddr, hndBeg, hndEnd) && !jitIsBetween(jmpAddr, hndBeg, hndEnd))
        {
            // Can't CEE_LEAVE out of a finally/fault handler
            if (HBtab->HasFinallyOrFaultHandler())
                BADCODE("leave out of fault/finally block");

            // Create the call to CORINFO_HELP_ENDCATCH
            GenTree* endCatch = gtNewHelperCallNode(CORINFO_HELP_ENDCATCH, TYP_VOID);

            // Make a list of all the currently pending endCatches
            if (endCatches)
                endCatches = gtNewOperNode(GT_COMMA, TYP_VOID, endCatches, endCatch);
            else
                endCatches = endCatch;

#ifdef DEBUG
            if (verbose)
            {
                printf("impImportLeave - " FMT_BB " jumping out of catch handler EH#%u, adding call to "
                       "CORINFO_HELP_ENDCATCH\n",
                       block->bbNum, XTnum);
            }
#endif
        }
        else if (HBtab->HasFinallyHandler() && jitIsBetween(blkAddr, tryBeg, tryEnd) &&
                 !jitIsBetween(jmpAddr, tryBeg, tryEnd))
        {
            /* This is a finally-protected try we are jumping out of */

            /* If there are any pending endCatches, and we have already
               jumped out of a finally-protected try, then the endCatches
               have to be put in a block in an outer try for async
               exceptions to work correctly.
               Else, just use append to the original block */

            BasicBlock* callBlock;

            assert(!encFinallies ==
                   !endLFinStmt); // if we have finallies, we better have an endLFin tree, and vice-versa

            if (encFinallies == 0)
            {
                assert(step == DUMMY_INIT(NULL));
                callBlock             = block;
                callBlock->bbJumpKind = BBJ_CALLFINALLY; // convert the BBJ_LEAVE to BBJ_CALLFINALLY

                if (endCatches)
                    impAppendTree(endCatches, (unsigned)CHECK_SPILL_NONE, impCurStmtOffs);

#ifdef DEBUG
                if (verbose)
                {
                    printf("impImportLeave - jumping out of a finally-protected try, convert block to BBJ_CALLFINALLY "
                           "block %s\n",
                           callBlock->dspToString());
                }
#endif
            }
            else
            {
                assert(step != DUMMY_INIT(NULL));

                /* Calling the finally block */
                callBlock = fgNewBBinRegion(BBJ_CALLFINALLY, XTnum + 1, 0, step);
                assert(step->bbJumpKind == BBJ_ALWAYS);
                step->bbJumpDest = callBlock; // the previous call to a finally returns to this call (to the next
                                              // finally in the chain)
                step->bbJumpDest->bbRefs++;

                /* The new block will inherit this block's weight */
                callBlock->setBBWeight(block->bbWeight);
                callBlock->bbFlags |= block->bbFlags & BBF_RUN_RARELY;

#ifdef DEBUG
                if (verbose)
                {
                    printf("impImportLeave - jumping out of a finally-protected try, new BBJ_CALLFINALLY block %s\n",
                           callBlock->dspToString());
                }
#endif

                Statement* lastStmt;

                if (endCatches)
                {
                    lastStmt = gtNewStmt(endCatches);
                    endLFinStmt->SetNextStmt(lastStmt);
                    lastStmt->SetPrevStmt(endLFinStmt);
                }
                else
                {
                    lastStmt = endLFinStmt;
                }

                // note that this sets BBF_IMPORTED on the block
                impEndTreeList(callBlock, endLFinStmt, lastStmt);
            }

            step = fgNewBBafter(BBJ_ALWAYS, callBlock, true);
            /* The new block will inherit this block's weight */
            step->setBBWeight(block->bbWeight);
            step->bbFlags |= (block->bbFlags & BBF_RUN_RARELY) | BBF_IMPORTED | BBF_KEEP_BBJ_ALWAYS;

#ifdef DEBUG
            if (verbose)
            {
                printf("impImportLeave - jumping out of a finally-protected try, created step (BBJ_ALWAYS) block %s\n",
                       step->dspToString());
            }
#endif

            unsigned finallyNesting = compHndBBtab[XTnum].ebdHandlerNestingLevel;
            assert(finallyNesting <= compHndBBtabCount);

            callBlock->bbJumpDest = HBtab->ebdHndBeg; // This callBlock will call the "finally" handler.
            GenTree* endLFin      = new (this, GT_END_LFIN) GenTreeVal(GT_END_LFIN, TYP_VOID, finallyNesting);
            endLFinStmt           = gtNewStmt(endLFin);
            endCatches            = NULL;

            encFinallies++;

            invalidatePreds = true;
        }
    }

    /* Append any remaining endCatches, if any */

    assert(!encFinallies == !endLFinStmt);

    if (encFinallies == 0)
    {
        assert(step == DUMMY_INIT(NULL));
        block->bbJumpKind = BBJ_ALWAYS; // convert the BBJ_LEAVE to a BBJ_ALWAYS

        if (endCatches)
            impAppendTree(endCatches, (unsigned)CHECK_SPILL_NONE, impCurStmtOffs);

#ifdef DEBUG
        if (verbose)
        {
            printf("impImportLeave - no enclosing finally-protected try blocks; convert CEE_LEAVE block to BBJ_ALWAYS "
                   "block %s\n",
                   block->dspToString());
        }
#endif
    }
    else
    {
        // If leaveTarget is the start of another try block, we want to make sure that
        // we do not insert finalStep into that try block. Hence, we find the enclosing
        // try block.
        unsigned tryIndex = bbFindInnermostCommonTryRegion(step, leaveTarget);

        // Insert a new BB either in the try region indicated by tryIndex or
        // the handler region indicated by leaveTarget->bbHndIndex,
        // depending on which is the inner region.
        BasicBlock* finalStep = fgNewBBinRegion(BBJ_ALWAYS, tryIndex, leaveTarget->bbHndIndex, step);
        finalStep->bbFlags |= BBF_KEEP_BBJ_ALWAYS;
        step->bbJumpDest = finalStep;

        /* The new block will inherit this block's weight */
        finalStep->setBBWeight(block->bbWeight);
        finalStep->bbFlags |= block->bbFlags & BBF_RUN_RARELY;

#ifdef DEBUG
        if (verbose)
        {
            printf("impImportLeave - finalStep block required (encFinallies(%d) > 0), new block %s\n", encFinallies,
                   finalStep->dspToString());
        }
#endif

        Statement* lastStmt;

        if (endCatches)
        {
            lastStmt = gtNewStmt(endCatches);
            endLFinStmt->SetNextStmt(lastStmt);
            lastStmt->SetPrevStmt(endLFinStmt);
        }
        else
        {
            lastStmt = endLFinStmt;
        }

        impEndTreeList(finalStep, endLFinStmt, lastStmt);

        finalStep->bbJumpDest = leaveTarget; // this is the ultimate destination of the LEAVE

        // Queue up the jump target for importing

        impImportBlockPending(leaveTarget);

        invalidatePreds = true;
    }

    if (invalidatePreds && fgComputePredsDone)
    {
        JITDUMP("\n**** impImportLeave - Removing preds after creating new blocks\n");
        fgRemovePreds();
    }

#ifdef DEBUG
    fgVerifyHandlerTab();

    if (verbose)
    {
        printf("\nAfter import CEE_LEAVE:\n");
        fgDispBasicBlocks();
        fgDispHandlerTab();
    }
#endif // DEBUG
}

#else // FEATURE_EH_FUNCLETS

void Compiler::impImportLeave(BasicBlock* block)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("\nBefore import CEE_LEAVE in " FMT_BB " (targetting " FMT_BB "):\n", block->bbNum,
               block->bbJumpDest->bbNum);
        fgDispBasicBlocks();
        fgDispHandlerTab();
    }
#endif // DEBUG

    bool        invalidatePreds = false; // If we create new blocks, invalidate the predecessor lists (if created)
    unsigned    blkAddr         = block->bbCodeOffs;
    BasicBlock* leaveTarget     = block->bbJumpDest;
    unsigned    jmpAddr         = leaveTarget->bbCodeOffs;

    // LEAVE clears the stack, spill side effects, and set stack to 0

    impSpillSideEffects(true, (unsigned)CHECK_SPILL_ALL DEBUGARG("impImportLeave"));
    verCurrentState.esStackDepth = 0;

    assert(block->bbJumpKind == BBJ_LEAVE);
    assert(fgBBs == (BasicBlock**)0xCDCD || fgLookupBB(jmpAddr) != nullptr); // should be a BB boundary

    BasicBlock* step = nullptr;

    enum StepType
    {
        // No step type; step == NULL.
        ST_None,

        // Is the step block the BBJ_ALWAYS block of a BBJ_CALLFINALLY/BBJ_ALWAYS pair?
        // That is, is step->bbJumpDest where a finally will return to?
        ST_FinallyReturn,

        // The step block is a catch return.
        ST_Catch,

        // The step block is in a "try", created as the target for a finally return or the target for a catch return.
        ST_Try
    };
    StepType stepType = ST_None;

    unsigned  XTnum;
    EHblkDsc* HBtab;

    for (XTnum = 0, HBtab = compHndBBtab; XTnum < compHndBBtabCount; XTnum++, HBtab++)
    {
        // Grab the handler offsets

        IL_OFFSET tryBeg = HBtab->ebdTryBegOffs();
        IL_OFFSET tryEnd = HBtab->ebdTryEndOffs();
        IL_OFFSET hndBeg = HBtab->ebdHndBegOffs();
        IL_OFFSET hndEnd = HBtab->ebdHndEndOffs();

        /* Is this a catch-handler we are CEE_LEAVEing out of?
         */

        if (jitIsBetween(blkAddr, hndBeg, hndEnd) && !jitIsBetween(jmpAddr, hndBeg, hndEnd))
        {
            // Can't CEE_LEAVE out of a finally/fault handler
            if (HBtab->HasFinallyOrFaultHandler())
            {
                BADCODE("leave out of fault/finally block");
            }

            /* We are jumping out of a catch */

            if (step == nullptr)
            {
                step             = block;
                step->bbJumpKind = BBJ_EHCATCHRET; // convert the BBJ_LEAVE to BBJ_EHCATCHRET
                stepType         = ST_Catch;

#ifdef DEBUG
                if (verbose)
                {
                    printf("impImportLeave - jumping out of a catch (EH#%u), convert block " FMT_BB
                           " to BBJ_EHCATCHRET "
                           "block\n",
                           XTnum, step->bbNum);
                }
#endif
            }
            else
            {
                BasicBlock* exitBlock;

                /* Create a new catch exit block in the catch region for the existing step block to jump to in this
                 * scope */
                exitBlock = fgNewBBinRegion(BBJ_EHCATCHRET, 0, XTnum + 1, step);

                assert(step->bbJumpKind == BBJ_ALWAYS || step->bbJumpKind == BBJ_EHCATCHRET);
                step->bbJumpDest = exitBlock; // the previous step (maybe a call to a nested finally, or a nested catch
                                              // exit) returns to this block
                step->bbJumpDest->bbRefs++;

#if defined(TARGET_ARM)
                if (stepType == ST_FinallyReturn)
                {
                    assert(step->bbJumpKind == BBJ_ALWAYS);
                    // Mark the target of a finally return
                    step->bbJumpDest->bbFlags |= BBF_FINALLY_TARGET;
                }
#endif // defined(TARGET_ARM)

                /* The new block will inherit this block's weight */
                exitBlock->setBBWeight(block->bbWeight);
                exitBlock->bbFlags |= (block->bbFlags & BBF_RUN_RARELY) | BBF_IMPORTED;

                /* This exit block is the new step */
                step     = exitBlock;
                stepType = ST_Catch;

                invalidatePreds = true;

#ifdef DEBUG
                if (verbose)
                {
                    printf("impImportLeave - jumping out of a catch (EH#%u), new BBJ_EHCATCHRET block " FMT_BB "\n",
                           XTnum, exitBlock->bbNum);
                }
#endif
            }
        }
        else if (HBtab->HasFinallyHandler() && jitIsBetween(blkAddr, tryBeg, tryEnd) &&
                 !jitIsBetween(jmpAddr, tryBeg, tryEnd))
        {
            /* We are jumping out of a finally-protected try */

            BasicBlock* callBlock;

            if (step == nullptr)
            {
#if FEATURE_EH_CALLFINALLY_THUNKS

                // Put the call to the finally in the enclosing region.
                unsigned callFinallyTryIndex =
                    (HBtab->ebdEnclosingTryIndex == EHblkDsc::NO_ENCLOSING_INDEX) ? 0 : HBtab->ebdEnclosingTryIndex + 1;
                unsigned callFinallyHndIndex =
                    (HBtab->ebdEnclosingHndIndex == EHblkDsc::NO_ENCLOSING_INDEX) ? 0 : HBtab->ebdEnclosingHndIndex + 1;
                callBlock = fgNewBBinRegion(BBJ_CALLFINALLY, callFinallyTryIndex, callFinallyHndIndex, block);

                // Convert the BBJ_LEAVE to BBJ_ALWAYS, jumping to the new BBJ_CALLFINALLY. This is because
                // the new BBJ_CALLFINALLY is in a different EH region, thus it can't just replace the BBJ_LEAVE,
                // which might be in the middle of the "try". In most cases, the BBJ_ALWAYS will jump to the
                // next block, and flow optimizations will remove it.
                block->bbJumpKind = BBJ_ALWAYS;
                block->bbJumpDest = callBlock;
                block->bbJumpDest->bbRefs++;

                /* The new block will inherit this block's weight */
                callBlock->setBBWeight(block->bbWeight);
                callBlock->bbFlags |= (block->bbFlags & BBF_RUN_RARELY) | BBF_IMPORTED;

#ifdef DEBUG
                if (verbose)
                {
                    printf("impImportLeave - jumping out of a finally-protected try (EH#%u), convert block " FMT_BB
                           " to "
                           "BBJ_ALWAYS, add BBJ_CALLFINALLY block " FMT_BB "\n",
                           XTnum, block->bbNum, callBlock->bbNum);
                }
#endif

#else // !FEATURE_EH_CALLFINALLY_THUNKS

                callBlock             = block;
                callBlock->bbJumpKind = BBJ_CALLFINALLY; // convert the BBJ_LEAVE to BBJ_CALLFINALLY

#ifdef DEBUG
                if (verbose)
                {
                    printf("impImportLeave - jumping out of a finally-protected try (EH#%u), convert block " FMT_BB
                           " to "
                           "BBJ_CALLFINALLY block\n",
                           XTnum, callBlock->bbNum);
                }
#endif

#endif // !FEATURE_EH_CALLFINALLY_THUNKS
            }
            else
            {
                // Calling the finally block. We already have a step block that is either the call-to-finally from a
                // more nested try/finally (thus we are jumping out of multiple nested 'try' blocks, each protected by
                // a 'finally'), or the step block is the return from a catch.
                //
                // Due to ThreadAbortException, we can't have the catch return target the call-to-finally block
                // directly. Note that if a 'catch' ends without resetting the ThreadAbortException, the VM will
                // automatically re-raise the exception, using the return address of the catch (that is, the target
                // block of the BBJ_EHCATCHRET) as the re-raise address. If this address is in a finally, the VM will
                // refuse to do the re-raise, and the ThreadAbortException will get eaten (and lost). On AMD64/ARM64,
                // we put the call-to-finally thunk in a special "cloned finally" EH region that does look like a
                // finally clause to the VM. Thus, on these platforms, we can't have BBJ_EHCATCHRET target a
                // BBJ_CALLFINALLY directly. (Note that on ARM32, we don't mark the thunk specially -- it lives directly
                // within the 'try' region protected by the finally, since we generate code in such a way that execution
                // never returns to the call-to-finally call, and the finally-protected 'try' region doesn't appear on
                // stack walks.)

                assert(step->bbJumpKind == BBJ_ALWAYS || step->bbJumpKind == BBJ_EHCATCHRET);

#if FEATURE_EH_CALLFINALLY_THUNKS
                if (step->bbJumpKind == BBJ_EHCATCHRET)
                {
                    // Need to create another step block in the 'try' region that will actually branch to the
                    // call-to-finally thunk.
                    BasicBlock* step2 = fgNewBBinRegion(BBJ_ALWAYS, XTnum + 1, 0, step);
                    step->bbJumpDest  = step2;
                    step->bbJumpDest->bbRefs++;
                    step2->setBBWeight(block->bbWeight);
                    step2->bbFlags |= (block->bbFlags & BBF_RUN_RARELY) | BBF_IMPORTED;

#ifdef DEBUG
                    if (verbose)
                    {
                        printf("impImportLeave - jumping out of a finally-protected try (EH#%u), step block is "
                               "BBJ_EHCATCHRET (" FMT_BB "), new BBJ_ALWAYS step-step block " FMT_BB "\n",
                               XTnum, step->bbNum, step2->bbNum);
                    }
#endif

                    step = step2;
                    assert(stepType == ST_Catch); // Leave it as catch type for now.
                }
#endif // FEATURE_EH_CALLFINALLY_THUNKS

#if FEATURE_EH_CALLFINALLY_THUNKS
                unsigned callFinallyTryIndex =
                    (HBtab->ebdEnclosingTryIndex == EHblkDsc::NO_ENCLOSING_INDEX) ? 0 : HBtab->ebdEnclosingTryIndex + 1;
                unsigned callFinallyHndIndex =
                    (HBtab->ebdEnclosingHndIndex == EHblkDsc::NO_ENCLOSING_INDEX) ? 0 : HBtab->ebdEnclosingHndIndex + 1;
#else  // !FEATURE_EH_CALLFINALLY_THUNKS
                unsigned callFinallyTryIndex = XTnum + 1;
                unsigned callFinallyHndIndex = 0; // don't care
#endif // !FEATURE_EH_CALLFINALLY_THUNKS

                callBlock        = fgNewBBinRegion(BBJ_CALLFINALLY, callFinallyTryIndex, callFinallyHndIndex, step);
                step->bbJumpDest = callBlock; // the previous call to a finally returns to this call (to the next
                                              // finally in the chain)
                step->bbJumpDest->bbRefs++;

#if defined(TARGET_ARM)
                if (stepType == ST_FinallyReturn)
                {
                    assert(step->bbJumpKind == BBJ_ALWAYS);
                    // Mark the target of a finally return
                    step->bbJumpDest->bbFlags |= BBF_FINALLY_TARGET;
                }
#endif // defined(TARGET_ARM)

                /* The new block will inherit this block's weight */
                callBlock->setBBWeight(block->bbWeight);
                callBlock->bbFlags |= (block->bbFlags & BBF_RUN_RARELY) | BBF_IMPORTED;

#ifdef DEBUG
                if (verbose)
                {
                    printf("impImportLeave - jumping out of a finally-protected try (EH#%u), new BBJ_CALLFINALLY "
                           "block " FMT_BB "\n",
                           XTnum, callBlock->bbNum);
                }
#endif
            }

            step     = fgNewBBafter(BBJ_ALWAYS, callBlock, true);
            stepType = ST_FinallyReturn;

            /* The new block will inherit this block's weight */
            step->setBBWeight(block->bbWeight);
            step->bbFlags |= (block->bbFlags & BBF_RUN_RARELY) | BBF_IMPORTED | BBF_KEEP_BBJ_ALWAYS;

#ifdef DEBUG
            if (verbose)
            {
                printf("impImportLeave - jumping out of a finally-protected try (EH#%u), created step (BBJ_ALWAYS) "
                       "block " FMT_BB "\n",
                       XTnum, step->bbNum);
            }
#endif

            callBlock->bbJumpDest = HBtab->ebdHndBeg; // This callBlock will call the "finally" handler.

            invalidatePreds = true;
        }
        else if (HBtab->HasCatchHandler() && jitIsBetween(blkAddr, tryBeg, tryEnd) &&
                 !jitIsBetween(jmpAddr, tryBeg, tryEnd))
        {
            // We are jumping out of a catch-protected try.
            //
            // If we are returning from a call to a finally, then we must have a step block within a try
            // that is protected by a catch. This is so when unwinding from that finally (e.g., if code within the
            // finally raises an exception), the VM will find this step block, notice that it is in a protected region,
            // and invoke the appropriate catch.
            //
            // We also need to handle a special case with the handling of ThreadAbortException. If a try/catch
            // catches a ThreadAbortException (which might be because it catches a parent, e.g. System.Exception),
            // and the catch doesn't call System.Threading.Thread::ResetAbort(), then when the catch returns to the VM,
            // the VM will automatically re-raise the ThreadAbortException. When it does this, it uses the target
            // address of the catch return as the new exception address. That is, the re-raised exception appears to
            // occur at the catch return address. If this exception return address skips an enclosing try/catch that
            // catches ThreadAbortException, then the enclosing try/catch will not catch the exception, as it should.
            // For example:
            //
            // try {
            //    try {
            //       // something here raises ThreadAbortException
            //       LEAVE LABEL_1; // no need to stop at LABEL_2
            //    } catch (Exception) {
            //       // This catches ThreadAbortException, but doesn't call System.Threading.Thread::ResetAbort(), so
            //       // ThreadAbortException is re-raised by the VM at the address specified by the LEAVE opcode.
            //       // This is bad, since it means the outer try/catch won't get a chance to catch the re-raised
            //       // ThreadAbortException. So, instead, create step block LABEL_2 and LEAVE to that. We only
            //       // need to do this transformation if the current EH block is a try/catch that catches
            //       // ThreadAbortException (or one of its parents), however we might not be able to find that
            //       // information, so currently we do it for all catch types.
            //       LEAVE LABEL_1; // Convert this to LEAVE LABEL2;
            //    }
            //    LABEL_2: LEAVE LABEL_1; // inserted by this step creation code
            // } catch (ThreadAbortException) {
            // }
            // LABEL_1:
            //
            // Note that this pattern isn't theoretical: it occurs in ASP.NET, in IL code generated by the Roslyn C#
            // compiler.

            if ((stepType == ST_FinallyReturn) || (stepType == ST_Catch))
            {
                BasicBlock* catchStep;

                assert(step);

                if (stepType == ST_FinallyReturn)
                {
                    assert(step->bbJumpKind == BBJ_ALWAYS);
                }
                else
                {
                    assert(stepType == ST_Catch);
                    assert(step->bbJumpKind == BBJ_EHCATCHRET);
                }

                /* Create a new exit block in the try region for the existing step block to jump to in this scope */
                catchStep        = fgNewBBinRegion(BBJ_ALWAYS, XTnum + 1, 0, step);
                step->bbJumpDest = catchStep;
                step->bbJumpDest->bbRefs++;

#if defined(TARGET_ARM)
                if (stepType == ST_FinallyReturn)
                {
                    // Mark the target of a finally return
                    step->bbJumpDest->bbFlags |= BBF_FINALLY_TARGET;
                }
#endif // defined(TARGET_ARM)

                /* The new block will inherit this block's weight */
                catchStep->setBBWeight(block->bbWeight);
                catchStep->bbFlags |= (block->bbFlags & BBF_RUN_RARELY) | BBF_IMPORTED;

#ifdef DEBUG
                if (verbose)
                {
                    if (stepType == ST_FinallyReturn)
                    {
                        printf("impImportLeave - return from finally jumping out of a catch-protected try (EH#%u), new "
                               "BBJ_ALWAYS block " FMT_BB "\n",
                               XTnum, catchStep->bbNum);
                    }
                    else
                    {
                        assert(stepType == ST_Catch);
                        printf("impImportLeave - return from catch jumping out of a catch-protected try (EH#%u), new "
                               "BBJ_ALWAYS block " FMT_BB "\n",
                               XTnum, catchStep->bbNum);
                    }
                }
#endif // DEBUG

                /* This block is the new step */
                step     = catchStep;
                stepType = ST_Try;

                invalidatePreds = true;
            }
        }
    }

    if (step == nullptr)
    {
        block->bbJumpKind = BBJ_ALWAYS; // convert the BBJ_LEAVE to a BBJ_ALWAYS

#ifdef DEBUG
        if (verbose)
        {
            printf("impImportLeave - no enclosing finally-protected try blocks or catch handlers; convert CEE_LEAVE "
                   "block " FMT_BB " to BBJ_ALWAYS\n",
                   block->bbNum);
        }
#endif
    }
    else
    {
        step->bbJumpDest = leaveTarget; // this is the ultimate destination of the LEAVE

#if defined(TARGET_ARM)
        if (stepType == ST_FinallyReturn)
        {
            assert(step->bbJumpKind == BBJ_ALWAYS);
            // Mark the target of a finally return
            step->bbJumpDest->bbFlags |= BBF_FINALLY_TARGET;
        }
#endif // defined(TARGET_ARM)

#ifdef DEBUG
        if (verbose)
        {
            printf("impImportLeave - final destination of step blocks set to " FMT_BB "\n", leaveTarget->bbNum);
        }
#endif

        // Queue up the jump target for importing

        impImportBlockPending(leaveTarget);
    }

    if (invalidatePreds && fgComputePredsDone)
    {
        JITDUMP("\n**** impImportLeave - Removing preds after creating new blocks\n");
        fgRemovePreds();
    }

#ifdef DEBUG
    fgVerifyHandlerTab();

    if (verbose)
    {
        printf("\nAfter import CEE_LEAVE:\n");
        fgDispBasicBlocks();
        fgDispHandlerTab();
    }
#endif // DEBUG
}

#endif // FEATURE_EH_FUNCLETS

/*****************************************************************************/
// This is called when reimporting a leave block. It resets the JumpKind,
// JumpDest, and bbNext to the original values

void Compiler::impResetLeaveBlock(BasicBlock* block, unsigned jmpAddr)
{
#if defined(FEATURE_EH_FUNCLETS)
    // With EH Funclets, while importing leave opcode we create another block ending with BBJ_ALWAYS (call it B1)
    // and the block containing leave (say B0) is marked as BBJ_CALLFINALLY.   Say for some reason we reimport B0,
    // it is reset (in this routine) by marking as ending with BBJ_LEAVE and further down when B0 is reimported, we
    // create another BBJ_ALWAYS (call it B2). In this process B1 gets orphaned and any blocks to which B1 is the
    // only predecessor are also considered orphans and attempted to be deleted.
    //
    //  try  {
    //     ....
    //     try
    //     {
    //         ....
    //         leave OUTSIDE;  // B0 is the block containing this leave, following this would be B1
    //     } finally { }
    //  } finally { }
    //  OUTSIDE:
    //
    // In the above nested try-finally example, we create a step block (call it Bstep) which in branches to a block
    // where a finally would branch to (and such block is marked as finally target).  Block B1 branches to step block.
    // Because of re-import of B0, Bstep is also orphaned. Since Bstep is a finally target it cannot be removed.  To
    // work around this we will duplicate B0 (call it B0Dup) before reseting. B0Dup is marked as BBJ_CALLFINALLY and
    // only serves to pair up with B1 (BBJ_ALWAYS) that got orphaned. Now during orphan block deletion B0Dup and B1
    // will be treated as pair and handled correctly.
    if (block->bbJumpKind == BBJ_CALLFINALLY)
    {
        BasicBlock* dupBlock = bbNewBasicBlock(block->bbJumpKind);
        dupBlock->bbFlags    = block->bbFlags;
        dupBlock->bbJumpDest = block->bbJumpDest;
        dupBlock->copyEHRegion(block);
        dupBlock->bbCatchTyp = block->bbCatchTyp;

        // Mark this block as
        //  a) not referenced by any other block to make sure that it gets deleted
        //  b) weight zero
        //  c) prevent from being imported
        //  d) as internal
        //  e) as rarely run
        dupBlock->bbRefs   = 0;
        dupBlock->bbWeight = 0;
        dupBlock->bbFlags |= BBF_IMPORTED | BBF_INTERNAL | BBF_RUN_RARELY;

        // Insert the block right after the block which is getting reset so that BBJ_CALLFINALLY and BBJ_ALWAYS
        // will be next to each other.
        fgInsertBBafter(block, dupBlock);

#ifdef DEBUG
        if (verbose)
        {
            printf("New Basic Block " FMT_BB " duplicate of " FMT_BB " created.\n", dupBlock->bbNum, block->bbNum);
        }
#endif
    }
#endif // FEATURE_EH_FUNCLETS

    block->bbJumpKind = BBJ_LEAVE;
    fgInitBBLookup();
    block->bbJumpDest = fgLookupBB(jmpAddr);

    // We will leave the BBJ_ALWAYS block we introduced. When it's reimported
    // the BBJ_ALWAYS block will be unreachable, and will be removed after. The
    // reason we don't want to remove the block at this point is that if we call
    // fgInitBBLookup() again we will do it wrong as the BBJ_ALWAYS block won't be
    // added and the linked list length will be different than fgBBcount.
}

/*****************************************************************************/
// Get the first non-prefix opcode. Used for verification of valid combinations
// of prefixes and actual opcodes.

static OPCODE impGetNonPrefixOpcode(const BYTE* codeAddr, const BYTE* codeEndp)
{
    while (codeAddr < codeEndp)
    {
        OPCODE opcode = (OPCODE)getU1LittleEndian(codeAddr);
        codeAddr += sizeof(__int8);

        if (opcode == CEE_PREFIX1)
        {
            if (codeAddr >= codeEndp)
            {
                break;
            }
            opcode = (OPCODE)(getU1LittleEndian(codeAddr) + 256);
            codeAddr += sizeof(__int8);
        }

        switch (opcode)
        {
            case CEE_UNALIGNED:
            case CEE_VOLATILE:
            case CEE_TAILCALL:
            case CEE_CONSTRAINED:
            case CEE_READONLY:
                break;
            default:
                return opcode;
        }

        codeAddr += opcodeSizes[opcode];
    }

    return CEE_ILLEGAL;
}

/*****************************************************************************/
// Checks whether the opcode is a valid opcode for volatile. and unaligned. prefixes

static void impValidateMemoryAccessOpcode(const BYTE* codeAddr, const BYTE* codeEndp, bool volatilePrefix)
{
    OPCODE opcode = impGetNonPrefixOpcode(codeAddr, codeEndp);

    if (!(
            // Opcode of all ldind and stdind happen to be in continuous, except stind.i.
            ((CEE_LDIND_I1 <= opcode) && (opcode <= CEE_STIND_R8)) || (opcode == CEE_STIND_I) ||
            (opcode == CEE_LDFLD) || (opcode == CEE_STFLD) || (opcode == CEE_LDOBJ) || (opcode == CEE_STOBJ) ||
            (opcode == CEE_INITBLK) || (opcode == CEE_CPBLK) ||
            // volatile. prefix is allowed with the ldsfld and stsfld
            (volatilePrefix && ((opcode == CEE_LDSFLD) || (opcode == CEE_STSFLD)))))
    {
        BADCODE("Invalid opcode for unaligned. or volatile. prefix");
    }
}

var_types Compiler::impGetNumericBinaryOpType(genTreeOps oper, bool fUnsigned, GenTree** pOp1, GenTree** pOp2)
{
    GenTree* op1 = *pOp1;
    GenTree* op2 = *pOp2;

    impBashVarAddrsToI(op1, op2);

#ifdef TARGET_64BIT
    auto WidenToNativeInt = [this](GenTree* op, bool fromUnsigned) -> GenTree* {
        if (GenTreeIntCon* con = op->IsIntCon())
        {
            // There are no IL instructions that load a native int constant so the C# compiler
            // emits ldc.i4 and takes advantage of the implicit int32 - native int widening.

            assert(con->TypeIs(TYP_INT));
            con->SetType(TYP_LONG);

            if (fromUnsigned)
            {
                con->SetValue(con->GetUInt32Value());
            }
            else
            {
                con->SetValue(con->GetInt32Value());
            }

            return con;
        }
        else
        {
            assert(varTypeIsIntegralOrI(op->GetType()));

            return gtNewCastNode(TYP_LONG, op, fromUnsigned, TYP_LONG);
        }
    };
#endif

    assert(!op1->TypeIs(TYP_REF) && !op2->TypeIs(TYP_REF));

    if (op1->TypeIs(TYP_BYREF) || op2->TypeIs(TYP_BYREF))
    {
        if (oper == GT_SUB)
        {
            if (op1->TypeIs(TYP_BYREF) && op2->TypeIs(TYP_BYREF))
            {
                // byref - byref = native int

                return TYP_I_IMPL;
            }

            if (varActualTypeIsIntOrI(op1->GetType()) && op2->TypeIs(TYP_BYREF))
            {
#ifdef TARGET_64BIT
                if (!op1->TypeIs(TYP_LONG))
                {
                    *pOp1 = WidenToNativeInt(op1, fUnsigned);
                }
#endif

                // [native] int - byref = native int.
                // This isn't valid but apparently VC++ produced such IL.

                return TYP_I_IMPL;
            }

            // byref - [native] int = byref

            assert(op1->TypeIs(TYP_BYREF) && varActualTypeIsIntOrI(op2->GetType()));

#ifdef TARGET_64BIT
            if (!op2->TypeIs(TYP_LONG))
            {
                *pOp2 = WidenToNativeInt(op2, fUnsigned);
            }
#endif

            return TYP_BYREF;
        }

        // byref + [native] int = byref
        // [native] int + byref = byref

        assert(oper == GT_ADD);
        assert(!op1->TypeIs(TYP_BYREF) || !op2->TypeIs(TYP_BYREF));
        assert(varActualTypeIsIntOrI(op1->GetType()) || varActualTypeIsIntOrI(op2->GetType()));

#ifdef TARGET_64BIT
        if (op2->TypeIs(TYP_BYREF))
        {
            if (!op1->TypeIs(TYP_LONG))
            {
                *pOp1 = WidenToNativeInt(op1, fUnsigned);
            }
        }
        else
        {
            if (!op2->TypeIs(TYP_LONG))
            {
                *pOp2 = WidenToNativeInt(op2, fUnsigned);
            }
        }
#endif

        return TYP_BYREF;
    }

    if (op1->TypeIs(TYP_LONG) || op2->TypeIs(TYP_LONG))
    {
#ifndef TARGET_64BIT
        // TODO-MIKE-Cleanup: Both operands should be LONG but
        // JIT\Methodical\Boxing\morph\sin3double\sin3double.cmd
        // contains invalid IL - it adds native int and int64.
        assert(varTypeIsIntegral(op1->GetType()) && varTypeIsIntegral(op2->GetType()));
#else
        // int32 + native int = native int
        // native int + int32 = native int
        // On 64 bit targets the JIT doesn't distinguish between native int and int64
        // so this is extended to int32 + int64 = int64 which is invalid in ECMA-335.

        if (!op1->TypeIs(TYP_LONG))
        {
            *pOp1 = WidenToNativeInt(op1, fUnsigned);
        }
        else if (!op2->TypeIs(TYP_LONG))
        {
            *pOp2 = WidenToNativeInt(op2, fUnsigned);
        }
#endif

        return TYP_LONG;
    }

    if (op1->TypeIs(TYP_FLOAT) && !op2->TypeIs(TYP_FLOAT))
    {
        // float + double = double

        assert(op2->TypeIs(TYP_DOUBLE));
        *pOp1 = gtNewCastNode(TYP_DOUBLE, op1, false, TYP_DOUBLE);
        return TYP_DOUBLE;
    }

    if (op1->TypeIs(TYP_DOUBLE) && !op2->TypeIs(TYP_DOUBLE))
    {
        // double + float = double

        assert(op2->TypeIs(TYP_FLOAT));
        *pOp2 = gtNewCastNode(TYP_DOUBLE, op2, false, TYP_DOUBLE);
        return TYP_DOUBLE;
    }

    // int + int = int
    // float + float = float
    // double + double = double

    assert(varActualType(op1->GetType()) == varActualType(op2->GetType()));

    return varActualType(op1->GetType());
}

//------------------------------------------------------------------------
// impOptimizeCastClassOrIsInst: attempt to resolve a cast when jitting
//
// Arguments:
//   op1 - value to cast
//   pResolvedToken - resolved token for type to cast to
//   isCastClass - true if this is a castclass, false if isinst
//
// Return Value:
//   tree representing optimized cast, or null if no optimization possible

GenTree* Compiler::impOptimizeCastClassOrIsInst(GenTree* op1, CORINFO_RESOLVED_TOKEN* pResolvedToken, bool isCastClass)
{
    assert(op1->TypeGet() == TYP_REF);

    // Don't optimize for minopts or debug codegen.
    if (opts.OptimizationDisabled())
    {
        return nullptr;
    }

    // See what we know about the type of the object being cast.
    bool                 isExact   = false;
    bool                 isNonNull = false;
    CORINFO_CLASS_HANDLE fromClass = gtGetClassHandle(op1, &isExact, &isNonNull);
    GenTree*             optResult = nullptr;

    if (fromClass != nullptr)
    {
        CORINFO_CLASS_HANDLE toClass = pResolvedToken->hClass;
        JITDUMP("\nConsidering optimization of %s from %s%p (%s) to %p (%s)\n", isCastClass ? "castclass" : "isinst",
                isExact ? "exact " : "", dspPtr(fromClass), info.compCompHnd->getClassName(fromClass), dspPtr(toClass),
                info.compCompHnd->getClassName(toClass));

        // Perhaps we know if the cast will succeed or fail.
        TypeCompareState castResult = info.compCompHnd->compareTypesForCast(fromClass, toClass);

        if (castResult == TypeCompareState::Must)
        {
            // Cast will succeed, result is simply op1.
            JITDUMP("Cast will succeed, optimizing to simply return input\n");
            return op1;
        }
        else if (castResult == TypeCompareState::MustNot)
        {
            // See if we can sharpen exactness by looking for final classes
            if (!isExact)
            {
                isExact = impIsClassExact(fromClass);
            }

            // Cast to exact type will fail. Handle case where we have
            // an exact type (that is, fromClass is not a subtype)
            // and we're not going to throw on failure.
            if (isExact && !isCastClass)
            {
                JITDUMP("Cast will fail, optimizing to return null\n");
                GenTree* result = gtNewIconNode(0, TYP_REF);

                // If the cast was fed by a box, we can remove that too.
                if (op1->IsBox())
                {
                    JITDUMP("Also removing upstream box\n");
                    gtTryRemoveBoxUpstreamEffects(op1);
                }

                return result;
            }
            else if (isExact)
            {
                JITDUMP("Not optimizing failing castclass (yet)\n");
            }
            else
            {
                JITDUMP("Can't optimize since fromClass is inexact\n");
            }
        }
        else
        {
            JITDUMP("Result of cast unknown, must generate runtime test\n");
        }
    }
    else
    {
        JITDUMP("\nCan't optimize since fromClass is unknown\n");
    }

    return nullptr;
}

//------------------------------------------------------------------------
// impCastClassOrIsInstToTree: build and import castclass/isinst
//
// Arguments:
//   op1 - value to cast
//   op2 - type handle for type to cast to
//   pResolvedToken - resolved token from the cast operation
//   isCastClass - true if this is castclass, false means isinst
//
// Return Value:
//   Tree representing the cast
//
// Notes:
//   May expand into a series of runtime checks or a helper call.

GenTree* Compiler::impCastClassOrIsInstToTree(GenTree*                op1,
                                              GenTree*                op2,
                                              CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                              bool                    isCastClass)
{
    assert(op1->TypeGet() == TYP_REF);

    // Optimistically assume the jit should expand this as an inline test
    bool shouldExpandInline = true;

    // Profitability check.
    //
    // Don't bother with inline expansion when jit is trying to
    // generate code quickly, or the cast is in code that won't run very
    // often, or the method already is pretty big.
    if (compCurBB->isRunRarely() || opts.OptimizationDisabled())
    {
        // not worth the code expansion if jitting fast or in a rarely run block
        shouldExpandInline = false;
    }
    else if ((op1->gtFlags & GTF_GLOB_EFFECT) && lvaHaveManyLocals())
    {
        // not worth creating an untracked local variable
        shouldExpandInline = false;
    }

    // Pessimistically assume the jit cannot expand this as an inline test
    bool                  canExpandInline = false;
    const CorInfoHelpFunc helper          = info.compCompHnd->getCastingHelper(pResolvedToken, isCastClass);

    // Legality check.
    //
    // Not all classclass/isinst operations can be inline expanded.
    // Check legality only if an inline expansion is desirable.
    if (shouldExpandInline)
    {
        if (isCastClass)
        {
            // Jit can only inline expand the normal CHKCASTCLASS helper.
            canExpandInline = (helper == CORINFO_HELP_CHKCASTCLASS);
        }
        else
        {
            if (helper == CORINFO_HELP_ISINSTANCEOFCLASS)
            {
                // If the class is exact, the jit can expand the IsInst check inline.
                canExpandInline = impIsClassExact(pResolvedToken->hClass);
            }
        }
    }

    const bool expandInline = canExpandInline && shouldExpandInline;

    if (!expandInline)
    {
        JITDUMP("\nExpanding %s as call because %s\n", isCastClass ? "castclass" : "isinst",
                canExpandInline ? "want smaller code or faster jitting" : "inline expansion not legal");

        // If we CSE this class handle we prevent assertionProp from making SubType assertions
        // so instead we force the CSE logic to not consider CSE-ing this class handle.
        //
        op2->gtFlags |= GTF_DONT_CSE;

        return gtNewHelperCallNode(helper, TYP_REF, gtNewCallArgs(op2, op1));
    }

    JITDUMP("\nExpanding %s inline\n", isCastClass ? "castclass" : "isinst");

    impSpillSideEffects(true, CHECK_SPILL_ALL DEBUGARG("bubbling QMark2"));

    GenTree* temp;
    GenTree* condMT;
    //
    // expand the methodtable match:
    //
    //  condMT ==>   GT_NE
    //               /    \.
    //           GT_IND   op2 (typically CNS_INT)
    //              |
    //           op1Copy
    //

    // This can replace op1 with a GT_COMMA that evaluates op1 into a local
    //
    op1 = impCloneExpr(op1, &temp, NO_CLASS_HANDLE, CHECK_SPILL_ALL DEBUGARG("CASTCLASS eval op1"));
    //
    // op1 is now known to be a non-complex tree
    // thus we can use gtClone(op1) from now on
    //

    GenTree* op2Var = op2;
    if (isCastClass)
    {
        op2Var                                                  = fgInsertCommaFormTemp(&op2);
        lvaTable[op2Var->AsLclVarCommon()->GetLclNum()].lvIsCSE = true;
    }
    temp   = gtNewMethodTableLookup(temp);
    condMT = gtNewOperNode(GT_NE, TYP_INT, temp, op2);

    GenTree* condNull;
    //
    // expand the null check:
    //
    //  condNull ==>   GT_EQ
    //                 /    \.
    //             op1Copy CNS_INT
    //                      null
    //
    condNull = gtNewOperNode(GT_EQ, TYP_INT, gtClone(op1), gtNewIconNode(0, TYP_REF));

    //
    // expand the true and false trees for the condMT
    //
    GenTree* condFalse = gtClone(op1);
    GenTree* condTrue;
    if (isCastClass)
    {
        //
        // use the special helper that skips the cases checked by our inlined cast
        //
        const CorInfoHelpFunc specialHelper = CORINFO_HELP_CHKCASTCLASS_SPECIAL;

        condTrue = gtNewHelperCallNode(specialHelper, TYP_REF, gtNewCallArgs(op2Var, gtClone(op1)));
    }
    else
    {
        condTrue = gtNewIconNode(0, TYP_REF);
    }

    GenTree* qmarkMT   = gtNewQmarkNode(TYP_REF, condMT, condTrue, condFalse);
    GenTree* qmarkNull = gtNewQmarkNode(TYP_REF, condNull, gtClone(op1), qmarkMT);
    qmarkNull->gtFlags |= GTF_QMARK_CAST_INSTOF;

    // Make QMark node a top level node by spilling it.
    unsigned tmp = lvaNewTemp(TYP_REF, true DEBUGARG("spilling QMark2"));
    GenTree* asg = gtNewAssignNode(gtNewLclvNode(tmp, TYP_REF), qmarkNull);
    impAppendTree(asg, CHECK_SPILL_NONE, impCurStmtOffs);

    // TODO-CQ: Is it possible op1 has a better type?
    //
    // See also gtGetHelperCallClassHandle where we make the same
    // determination for the helper call variants.
    LclVarDsc* lclDsc = lvaGetDesc(tmp);
    assert(lclDsc->lvSingleDef == 0);
    lclDsc->lvSingleDef = 1;
    JITDUMP("Marked V%02u as a single def temp\n", tmp);
    lvaSetClass(tmp, pResolvedToken->hClass);
    return gtNewLclvNode(tmp, TYP_REF);
}

#ifndef DEBUG
#define assertImp(cond) ((void)0)
#else
#define assertImp(cond)                                                                                                \
    do                                                                                                                 \
    {                                                                                                                  \
        if (!(cond))                                                                                                   \
        {                                                                                                              \
            const int cchAssertImpBuf = 600;                                                                           \
            char*     assertImpBuf    = (char*)alloca(cchAssertImpBuf);                                                \
            _snprintf_s(assertImpBuf, cchAssertImpBuf, cchAssertImpBuf - 1,                                            \
                        "%s : Possibly bad IL with CEE_%s at offset %04Xh (op1=%s op2=%s stkDepth=%d)", #cond,         \
                        impCurOpcName, impCurOpcOffs, op1 ? varTypeName(op1->TypeGet()) : "NULL",                      \
                        op2 ? varTypeName(op2->TypeGet()) : "NULL", verCurrentState.esStackDepth);                     \
            assertAbort(assertImpBuf, __FILE__, __LINE__);                                                             \
        }                                                                                                              \
    } while (0)
#endif // DEBUG

//------------------------------------------------------------------------
// impBlockIsInALoop: check if a block might be in a loop
//
// Arguments:
//    block - block to check
//
// Returns:
//    true if the block might be in a loop.
//
// Notes:
//    Conservatively correct; may return true for some blocks that are
//    not actually in loops.
//
bool Compiler::impBlockIsInALoop(BasicBlock* block)
{
    return (compIsForInlining() && ((impInlineInfo->iciBlock->bbFlags & BBF_BACKWARD_JUMP) != 0)) ||
           ((block->bbFlags & BBF_BACKWARD_JUMP) != 0);
}

#ifdef _PREFAST_
#pragma warning(push)
#pragma warning(disable : 21000) // Suppress PREFast warning about overly large function
#endif
/*****************************************************************************
 *  Import the instr for the given basic block
 */
void Compiler::impImportBlockCode(BasicBlock* block)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("\nImporting " FMT_BB " (PC=%03u) of '%s'", block->bbNum, block->bbCodeOffs, info.compFullName);
    }
#endif

    unsigned                     nxtStmtIndex = impInitBlockLineInfo();
    IL_OFFSET                    nxtStmtOffs;
    CorInfoHelpFunc              helper;
    CorInfoIsAccessAllowedResult accessAllowedResult;
    CORINFO_HELPER_DESC          calloutHelper;
    const BYTE*                  lastLoadToken = nullptr;

    /* Get the tree list started */

    impBeginTreeList();

#ifdef FEATURE_ON_STACK_REPLACEMENT

    // Are there any places in the method where we might add a patchpoint?
    if (compHasBackwardJump)
    {
        // Are patchpoints enabled?
        if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_TIER0) && (JitConfig.TC_OnStackReplacement() > 0))
        {
            // We don't inline at Tier0, if we do, we may need rethink our approach.
            // Could probably support inlines that don't introduce flow.
            assert(!compIsForInlining());

            // Is the start of this block a suitable patchpoint?
            // Current strategy is blocks that are stack-empty and backwards branch targets
            if (block->bbFlags & BBF_BACKWARD_JUMP_TARGET && (verCurrentState.esStackDepth == 0))
            {
                block->bbFlags |= BBF_PATCHPOINT;
                setMethodHasPatchpoint();
            }
        }
    }
    else
    {
        // Should not see backward branch targets w/o backwards branches
        assert((block->bbFlags & BBF_BACKWARD_JUMP_TARGET) == 0);
    }

#endif // FEATURE_ON_STACK_REPLACEMENT

#ifdef FEATURE_SIMD
    m_impSIMDCoalescingBuffer.Clear();
#endif

    /* Walk the opcodes that comprise the basic block */

    const BYTE* codeAddr = info.compCode + block->bbCodeOffs;
    const BYTE* codeEndp = info.compCode + block->bbCodeOffsEnd;

    IL_OFFSET opcodeOffs    = block->bbCodeOffs;
    IL_OFFSET lastSpillOffs = opcodeOffs;

    signed jmpDist;

    int  prefixFlags = 0;
    bool explicitTailCall, constraintCall, readonlyCall;

    var_types callTyp    = TYP_COUNT;
    OPCODE    prevOpcode = CEE_ILLEGAL;

    if (block->bbCatchTyp)
    {
        if (info.compStmtOffsetsImplicit & ICorDebugInfo::CALL_SITE_BOUNDARIES)
        {
            impCurStmtOffsSet(block->bbCodeOffs);
        }

        // We will spill the GT_CATCH_ARG and the input of the BB_QMARK block
        // to a temp. This is a trade off for code simplicity
        impSpillSpecialSideEff();
    }

    while (codeAddr < codeEndp)
    {
        bool                   usingReadyToRunHelper = false;
        CORINFO_RESOLVED_TOKEN resolvedToken;
        CORINFO_RESOLVED_TOKEN constrainedResolvedToken;
        CORINFO_CALL_INFO      callInfo;

        //---------------------------------------------------------------------

        /* We need to restrict the max tree depth as many of the Compiler
           functions are recursive. We do this by spilling the stack */

        if (verCurrentState.esStackDepth)
        {
            /* Has it been a while since we last saw a non-empty stack (which
               guarantees that the tree depth isnt accumulating. */

            if ((opcodeOffs - lastSpillOffs) > MAX_TREE_SIZE && impCanSpillNow(prevOpcode))
            {
                impSpillStackEnsure();
                lastSpillOffs = opcodeOffs;
            }
        }
        else
        {
            lastSpillOffs   = opcodeOffs;
            impBoxTempInUse = false; // nothing on the stack, box temp OK to use again
        }

        /* Compute the current instr offset */

        opcodeOffs = (IL_OFFSET)(codeAddr - info.compCode);

#ifndef DEBUG
        if (opts.compDbgInfo)
#endif
        {
            if (!compIsForInlining())
            {
                nxtStmtOffs =
                    (nxtStmtIndex < info.compStmtOffsetsCount) ? info.compStmtOffsets[nxtStmtIndex] : BAD_IL_OFFSET;

                /* Have we reached the next stmt boundary ? */

                if (nxtStmtOffs != BAD_IL_OFFSET && opcodeOffs >= nxtStmtOffs)
                {
                    assert(nxtStmtOffs == info.compStmtOffsets[nxtStmtIndex]);

                    if (verCurrentState.esStackDepth != 0 && opts.compDbgCode)
                    {
                        /* We need to provide accurate IP-mapping at this point.
                           So spill anything on the stack so that it will form
                           gtStmts with the correct stmt offset noted */

                        impSpillStackEnsure(true);
                    }

                    // Has impCurStmtOffs been reported in any tree?

                    if (impCurStmtOffs != BAD_IL_OFFSET && opts.compDbgCode)
                    {
                        GenTree* placeHolder = new (this, GT_NO_OP) GenTree(GT_NO_OP, TYP_VOID);
                        impAppendTree(placeHolder, (unsigned)CHECK_SPILL_NONE, impCurStmtOffs);

                        assert(impCurStmtOffs == BAD_IL_OFFSET);
                    }

                    if (impCurStmtOffs == BAD_IL_OFFSET)
                    {
                        /* Make sure that nxtStmtIndex is in sync with opcodeOffs.
                           If opcodeOffs has gone past nxtStmtIndex, catch up */

                        while ((nxtStmtIndex + 1) < info.compStmtOffsetsCount &&
                               info.compStmtOffsets[nxtStmtIndex + 1] <= opcodeOffs)
                        {
                            nxtStmtIndex++;
                        }

                        /* Go to the new stmt */

                        impCurStmtOffsSet(info.compStmtOffsets[nxtStmtIndex]);

                        /* Update the stmt boundary index */

                        nxtStmtIndex++;
                        assert(nxtStmtIndex <= info.compStmtOffsetsCount);

                        /* Are there any more line# entries after this one? */

                        if (nxtStmtIndex < info.compStmtOffsetsCount)
                        {
                            /* Remember where the next line# starts */

                            nxtStmtOffs = info.compStmtOffsets[nxtStmtIndex];
                        }
                        else
                        {
                            /* No more line# entries */

                            nxtStmtOffs = BAD_IL_OFFSET;
                        }
                    }
                }
                else if ((info.compStmtOffsetsImplicit & ICorDebugInfo::STACK_EMPTY_BOUNDARIES) &&
                         (verCurrentState.esStackDepth == 0))
                {
                    /* At stack-empty locations, we have already added the tree to
                       the stmt list with the last offset. We just need to update
                       impCurStmtOffs
                     */

                    impCurStmtOffsSet(opcodeOffs);
                }
                else if ((info.compStmtOffsetsImplicit & ICorDebugInfo::CALL_SITE_BOUNDARIES) &&
                         impOpcodeIsCallSiteBoundary(prevOpcode))
                {
                    /* Make sure we have a type cached */
                    assert(callTyp != TYP_COUNT);

                    if (callTyp == TYP_VOID)
                    {
                        impCurStmtOffsSet(opcodeOffs);
                    }
                    else if (opts.compDbgCode)
                    {
                        impSpillStackEnsure(true);
                        impCurStmtOffsSet(opcodeOffs);
                    }
                }
                else if ((info.compStmtOffsetsImplicit & ICorDebugInfo::NOP_BOUNDARIES) && (prevOpcode == CEE_NOP))
                {
                    if (opts.compDbgCode)
                    {
                        impSpillStackEnsure(true);
                    }

                    impCurStmtOffsSet(opcodeOffs);
                }

                assert(impCurStmtOffs == BAD_IL_OFFSET || nxtStmtOffs == BAD_IL_OFFSET ||
                       jitGetILoffs(impCurStmtOffs) <= nxtStmtOffs);
            }
        }

        CORINFO_CLASS_HANDLE clsHnd = DUMMY_INIT(NULL);

        var_types lclTyp        = TYP_UNKNOWN;
        GenTree*  op1           = DUMMY_INIT(NULL);
        GenTree*  op2           = DUMMY_INIT(NULL);
        GenTree*  newObjThisPtr = DUMMY_INIT(NULL);
        bool      uns           = DUMMY_INIT(false);
        bool      isLocal       = false;

        /* Get the next opcode and the size of its parameters */

        OPCODE opcode = (OPCODE)getU1LittleEndian(codeAddr);
        codeAddr += sizeof(__int8);

#ifdef DEBUG
        impCurOpcOffs = (IL_OFFSET)(codeAddr - info.compCode - 1);
        JITDUMP("\n    [%2u] %3u (0x%03x) ", verCurrentState.esStackDepth, impCurOpcOffs, impCurOpcOffs);
#endif

    DECODE_OPCODE:

        // Return if any previous code has caused inline to fail.
        if (compDonotInline())
        {
            return;
        }

        /* Get the size of additional parameters */

        signed int sz = opcodeSizes[opcode];

#ifdef DEBUG
        clsHnd  = NO_CLASS_HANDLE;
        lclTyp  = TYP_COUNT;
        callTyp = TYP_COUNT;

        impCurOpcOffs = (IL_OFFSET)(codeAddr - info.compCode - 1);
        impCurOpcName = opcodeNames[opcode];

        if (verbose && (opcode != CEE_PREFIX1))
        {
            printf("%s", impCurOpcName);
        }

        /* Use assertImp() to display the opcode */

        op1 = op2 = nullptr;
#endif

        /* See what kind of an opcode we have, then */

        unsigned mflags   = 0;
        unsigned clsFlags = 0;

        switch (opcode)
        {
            unsigned  lclNum;
            var_types type;

            GenTree*   op3;
            genTreeOps oper;

            int val;

            CORINFO_SIG_INFO     sig;
            IL_OFFSET            jmpAddr;
            bool                 ovfl, unordered, callNode;
            CORINFO_CLASS_HANDLE tokenType;

            union {
                int     intVal;
                float   fltVal;
                __int64 lngVal;
                double  dblVal;
            } cval;

            case CEE_PREFIX1:
                opcode     = (OPCODE)(getU1LittleEndian(codeAddr) + 256);
                opcodeOffs = (IL_OFFSET)(codeAddr - info.compCode);
                codeAddr += sizeof(__int8);
                goto DECODE_OPCODE;

            SPILL_APPEND:
                // We need to call impSpillLclRefs() for a struct type lclVar.
                // This is because there may be loads of that lclVar on the evaluation stack, and
                // we need to ensure that those loads are completed before we modify it.
                if (op1->OperIs(GT_ASG) && varTypeIsStruct(op1->AsOp()->GetOp(0)->GetType()))
                {
                    GenTree*       lhs    = op1->AsOp()->GetOp(0);
                    GenTreeLclVar* lclVar = nullptr;

                    if (lhs->OperIs(GT_LCL_VAR))
                    {
                        lclVar = lhs->AsLclVar();
                    }
                    else if (lhs->OperIsBlk())
                    {
                        // Check if LHS address is within some struct local, to catch
                        // cases where we're updating the struct by something other than a stfld
                        lclVar = impIsAddressInLocal(lhs->AsBlk()->GetAddr());
                    }

                    if (lclVar != nullptr)
                    {
                        impSpillLclRefs(lclVar->GetLclNum());
                    }
                }

            SPILL_ALL_APPEND:
                impAppendTree(op1, CHECK_SPILL_ALL, impCurStmtOffs);
                goto DONE_APPEND;

            APPEND:
                impAppendTree(op1, CHECK_SPILL_NONE, impCurStmtOffs);
            DONE_APPEND:
                // Remember at which BC offset the tree was finished
                INDEBUG(impNoteLastILoffs();)
                break;

            case CEE_LDNULL:
                impPushOnStack(gtNewIconNode(0, TYP_REF), typeInfo());
                break;

            case CEE_LDC_I4_M1:
            case CEE_LDC_I4_0:
            case CEE_LDC_I4_1:
            case CEE_LDC_I4_2:
            case CEE_LDC_I4_3:
            case CEE_LDC_I4_4:
            case CEE_LDC_I4_5:
            case CEE_LDC_I4_6:
            case CEE_LDC_I4_7:
            case CEE_LDC_I4_8:
                cval.intVal = (opcode - CEE_LDC_I4_0);
                assert(-1 <= cval.intVal && cval.intVal <= 8);
                goto PUSH_I4CON;

            case CEE_LDC_I4_S:
                cval.intVal = getI1LittleEndian(codeAddr);
                goto PUSH_I4CON;
            case CEE_LDC_I4:
                cval.intVal = getI4LittleEndian(codeAddr);
                goto PUSH_I4CON;
            PUSH_I4CON:
                JITDUMP(" %d", cval.intVal);
                impPushOnStack(gtNewIconNode(cval.intVal), typeInfo());
                break;

            case CEE_LDC_I8:
                cval.lngVal = getI8LittleEndian(codeAddr);
                JITDUMP(" 0x%016llx", cval.lngVal);
                impPushOnStack(gtNewLconNode(cval.lngVal), typeInfo());
                break;

            case CEE_LDC_R8:
                cval.dblVal = getR8LittleEndian(codeAddr);
                JITDUMP(" %#.17g", cval.dblVal);
                impPushOnStack(gtNewDconNode(cval.dblVal), typeInfo());
                break;

            case CEE_LDC_R4:
                cval.dblVal = getR4LittleEndian(codeAddr);
                JITDUMP(" %#.17g", cval.dblVal);
                impPushOnStack(gtNewDconNode(cval.dblVal, TYP_FLOAT), typeInfo());
                break;

            case CEE_LDSTR:
                if (compIsForInlining())
                {
                    if (impInlineInfo->inlineCandidateInfo->dwRestrictions & INLINE_NO_CALLEE_LDSTR)
                    {
                        compInlineResult->NoteFatal(InlineObservation::CALLSITE_HAS_LDSTR_RESTRICTION);
                        return;
                    }
                }

                val = getU4LittleEndian(codeAddr);
                JITDUMP(" %08X", val);
                impPushOnStack(gtNewSconNode(val, info.compScopeHnd), typeInfo());
                break;

            case CEE_LDARG:
                lclNum = getU2LittleEndian(codeAddr);
                JITDUMP(" %u", lclNum);
                impLoadArg(lclNum, opcodeOffs + sz + 1);
                break;

            case CEE_LDARG_S:
                lclNum = getU1LittleEndian(codeAddr);
                JITDUMP(" %u", lclNum);
                impLoadArg(lclNum, opcodeOffs + sz + 1);
                break;

            case CEE_LDARG_0:
            case CEE_LDARG_1:
            case CEE_LDARG_2:
            case CEE_LDARG_3:
                lclNum = (opcode - CEE_LDARG_0);
                impLoadArg(lclNum, opcodeOffs + sz + 1);
                break;

            case CEE_LDLOC:
                lclNum = getU2LittleEndian(codeAddr);
                JITDUMP(" %u", lclNum);
                impLoadLoc(lclNum, opcodeOffs + sz + 1);
                break;

            case CEE_LDLOC_S:
                lclNum = getU1LittleEndian(codeAddr);
                JITDUMP(" %u", lclNum);
                impLoadLoc(lclNum, opcodeOffs + sz + 1);
                break;

            case CEE_LDLOC_0:
            case CEE_LDLOC_1:
            case CEE_LDLOC_2:
            case CEE_LDLOC_3:
                lclNum = (opcode - CEE_LDLOC_0);
                assert(lclNum >= 0 && lclNum < 4);
                impLoadLoc(lclNum, opcodeOffs + sz + 1);
                break;

            case CEE_STARG:
                lclNum = getU2LittleEndian(codeAddr);
                goto STARG;
            case CEE_STARG_S:
                lclNum = getU1LittleEndian(codeAddr);
            STARG:
                JITDUMP(" %u", lclNum);

                if (compIsForInlining())
                {
                    if (lclNum >= impInlineInfo->ilArgCount)
                    {
                        compInlineResult->NoteFatal(InlineObservation::CALLEE_BAD_ARGUMENT_NUMBER);
                        return;
                    }

                    op1 = inlUseArg(impInlineInfo, lclNum);
                    noway_assert(op1->OperIs(GT_LCL_VAR));
                    lclNum = op1->AsLclVar()->GetLclNum();
                }
                else
                {
                    if (lclNum >= info.compILargsCount)
                    {
                        BADCODE("Bad IL arg num");
                    }

                    lclNum = compMapILargNum(lclNum);

                    if (lclNum == info.compThisArg)
                    {
                        lclNum = lvaArg0Var;
                    }

                    assert(lvaGetDesc(lclNum)->lvHasILStoreOp);
                }

                goto ST_ARG;

            case CEE_STLOC:
                lclNum  = getU2LittleEndian(codeAddr);
                isLocal = true;
                JITDUMP(" %u", lclNum);
                goto ST_LOC;
            case CEE_STLOC_S:
                lclNum  = getU1LittleEndian(codeAddr);
                isLocal = true;
                JITDUMP(" %u", lclNum);
                goto ST_LOC;
            case CEE_STLOC_0:
            case CEE_STLOC_1:
            case CEE_STLOC_2:
            case CEE_STLOC_3:
                isLocal = true;
                lclNum  = (opcode - CEE_STLOC_0);
            ST_LOC:
                if (compIsForInlining())
                {
                    if (lclNum >= impInlineInfo->ilLocCount)
                    {
                        impInlineInfo->inlineResult->NoteFatal(InlineObservation::CALLEE_BAD_LOCAL_NUMBER);
                        return;
                    }

                    lclNum = inlGetInlineeLocal(impInlineInfo, lclNum);
                    lclTyp = lvaGetDesc(lclNum)->GetType();
                }
                else
                {
                    if (lclNum >= info.compMethodInfo->locals.numArgs)
                    {
                        BADCODE("Bad IL loc num");
                    }

                    lclNum += info.compArgsCount;

                ST_ARG:
                    LclVarDsc* lcl = lvaGetDesc(lclNum);
                    lclTyp         = lcl->GetType();

                    if (!lcl->lvNormalizeOnLoad())
                    {
                        lclTyp = varActualType(lclTyp);
                    }
                }

                {
                    StackEntry se = impPopStack();
                    clsHnd        = se.seTypeInfo.GetClassHandle();
                    op1           = se.val;
                }

#ifdef FEATURE_SIMD
                if (varTypeIsSIMD(lclTyp) && (lclTyp != op1->TypeGet()))
                {
                    assert(op1->TypeGet() == TYP_STRUCT);
                    op1->gtType = lclTyp;
                }
#endif // FEATURE_SIMD

                op1 = impImplicitIorI4Cast(op1, lclTyp);

#ifdef TARGET_64BIT
                // Downcast the TYP_I_IMPL into a 32-bit Int for x86 JIT compatiblity
                if (varTypeIsI(op1->TypeGet()) && (genActualType(lclTyp) == TYP_INT))
                {
                    op1 = gtNewCastNode(TYP_INT, op1, false, TYP_INT);
                }
#endif // TARGET_64BIT

                // We had better assign it a value of the correct type
                assertImp(
                    genActualType(lclTyp) == genActualType(op1->gtType) ||
                    (genActualType(lclTyp) == TYP_I_IMPL && op1->IsLocalAddrExpr() != nullptr) ||
                    (genActualType(lclTyp) == TYP_I_IMPL && (op1->gtType == TYP_BYREF || op1->gtType == TYP_REF)) ||
                    (genActualType(op1->gtType) == TYP_I_IMPL && lclTyp == TYP_BYREF) ||
                    (varTypeIsFloating(lclTyp) && varTypeIsFloating(op1->TypeGet())) ||
                    ((genActualType(lclTyp) == TYP_BYREF) && genActualType(op1->TypeGet()) == TYP_REF));

                /* If op1 is "&var" then its type is the transient "*" and it can
                   be used either as TYP_BYREF or TYP_I_IMPL */

                if (op1->IsLocalAddrExpr() != nullptr)
                {
                    assertImp(genActualType(lclTyp) == TYP_I_IMPL || lclTyp == TYP_BYREF);

                    /* When "&var" is created, we assume it is a byref. If it is
                       being assigned to a TYP_I_IMPL var, change the type to
                       prevent unnecessary GC info */

                    if (genActualType(lclTyp) == TYP_I_IMPL)
                    {
                        op1->gtType = TYP_I_IMPL;
                    }
                }

                // If this is a local and the local is a ref type, see
                // if we can improve type information based on the
                // value being assigned.
                if (isLocal && (lclTyp == TYP_REF))
                {
                    // We should have seen a stloc in our IL prescan.
                    assert(lvaTable[lclNum].lvHasILStoreOp);

                    // Is there just one place this local is defined?
                    const bool isSingleDefLocal = lvaTable[lclNum].lvSingleDef;

                    // TODO-MIKE-Cleanup: This check is probably no longer needed. It used to be the case
                    // that ref class handles were propagated from predecessors without merging, resulting
                    // in incorrect devirtualization.

                    // Conservative check that there is just one
                    // definition that reaches this store.
                    const bool hasSingleReachingDef = (block->bbStackDepthOnEntry() == 0);

                    if (isSingleDefLocal && hasSingleReachingDef)
                    {
                        lvaUpdateClass(lclNum, op1, clsHnd);
                    }
                }

                /* Filter out simple assignments to itself */

                if (op1->gtOper == GT_LCL_VAR && lclNum == op1->AsLclVarCommon()->GetLclNum())
                {
                    if (opts.compDbgCode)
                    {
                        op1 = gtNewNothingNode();
                        goto SPILL_ALL_APPEND;
                    }
                    else
                    {
                        break;
                    }
                }

                /* Create the assignment node */

                op2 = gtNewLclvNode(lclNum, lclTyp DEBUGARG(opcodeOffs + sz + 1));

                /* If the local is aliased or pinned, we need to spill calls and
                   indirections from the stack. */

                if ((lvaTable[lclNum].lvAddrExposed || lvaTable[lclNum].lvHasLdAddrOp || lvaTable[lclNum].lvPinned) &&
                    (verCurrentState.esStackDepth > 0))
                {
                    impSpillSideEffects(false,
                                        (unsigned)CHECK_SPILL_ALL DEBUGARG("Local could be aliased or is pinned"));
                }

                /* Spill any refs to the local from the stack */

                impSpillLclRefs(lclNum);

                // We can generate an assignment to a TYP_FLOAT from a TYP_DOUBLE
                // We insert a cast to the dest 'op2' type
                //
                if ((op1->TypeGet() != op2->TypeGet()) && varTypeIsFloating(op1->gtType) &&
                    varTypeIsFloating(op2->gtType))
                {
                    op1 = gtNewCastNode(op2->TypeGet(), op1, false, op2->TypeGet());
                }

                if (varTypeIsStruct(lclTyp))
                {
                    op1 = impAssignStruct(op2, op1, clsHnd, (unsigned)CHECK_SPILL_ALL);
                }
                else
                {
                    // The code generator generates GC tracking information
                    // based on the RHS of the assignment.  Later the LHS (which is
                    // is a BYREF) gets used and the emitter checks that that variable
                    // is being tracked.  It is not (since the RHS was an int and did
                    // not need tracking).  To keep this assert happy, we change the RHS
                    if (lclTyp == TYP_BYREF && !varTypeIsGC(op1->gtType))
                    {
                        op1->gtType = TYP_BYREF;
                    }
                    op1 = gtNewAssignNode(op2, op1);
                }

                goto SPILL_APPEND;

            case CEE_LDLOCA:
                lclNum = getU2LittleEndian(codeAddr);
                goto LDLOCA;
            case CEE_LDLOCA_S:
                lclNum = getU1LittleEndian(codeAddr);
            LDLOCA:
                JITDUMP(" %u", lclNum);

                if (compIsForInlining())
                {
                    if (lclNum >= impInlineInfo->ilLocCount)
                    {
                        compInlineResult->NoteFatal(InlineObservation::CALLEE_BAD_LOCAL_NUMBER);
                        return;
                    }

                    lclNum = inlGetInlineeLocal(impInlineInfo, lclNum);
                    op1    = gtNewLclvNode(lclNum, varActualType(lvaGetDesc(lclNum)->GetType()));
                    goto PUSH_ADRVAR;
                }

                if (lclNum >= info.compMethodInfo->locals.numArgs)
                {
                    BADCODE("Bad IL loc num");
                }

                lclNum += info.compArgsCount;
                goto ADRVAR;

            case CEE_LDARGA:
                lclNum = getU2LittleEndian(codeAddr);
                goto LDARGA;
            case CEE_LDARGA_S:
                lclNum = getU1LittleEndian(codeAddr);
            LDARGA:
                JITDUMP(" %u", lclNum);

                if (compIsForInlining())
                {
                    if (lclNum >= impInlineInfo->ilArgCount)
                    {
                        compInlineResult->NoteFatal(InlineObservation::CALLEE_BAD_ARGUMENT_NUMBER);
                        return;
                    }

                    op1 = inlUseArg(impInlineInfo, lclNum);
                    noway_assert(op1->OperIs(GT_LCL_VAR));
                    goto PUSH_ADRVAR;
                }

                if (lclNum >= info.compILargsCount)
                {
                    BADCODE("Bad IL arg num");
                }

                lclNum = compMapILargNum(lclNum); // account for possible hidden param

                if (lclNum == info.compThisArg)
                {
                    lclNum = lvaArg0Var;
                }

            ADRVAR:
                op1 = gtNewLclvNode(lclNum, varActualType(lvaGetDesc(lclNum)->GetType()) DEBUGARG(opcodeOffs + sz + 1));

            PUSH_ADRVAR:
                assert(op1->gtOper == GT_LCL_VAR);

                // Note that this is supposed to create the transient type "*"
                // which may be used as a TYP_I_IMPL. However we catch places
                // where it is used as a TYP_I_IMPL and change the node if needed.
                // Thus we are pessimistic and may report byrefs in the GC info
                // where it was not absolutely needed, but it is safer this way.
                op1 = gtNewAddrNode(op1);

                // &aliasedVar doesnt need GTF_GLOB_REF, though alisasedVar does
                assert((op1->gtFlags & GTF_GLOB_REF) == 0);

                // TODO-MIKE-Cleanup: This is weird, lvImpTypeInfo is pushed on the stack for what really
                // is the address of the local. Only when verification was enabled the pushed typeInfo
                // was transformed into a byref.
                //
                // In general we do not need typeInfo for non-struct values but LDFLD import code depends
                // on this because of the "normed type" mess. LDFLD accepts pretty much all sorts of types
                // as source - REF, I_IMPL, STRUCT - and the generated IR is different for STRUCT because
                // in that case we really need the address of the struct value.
                // But with the "normed type" thing we can end up with INT/LONG instead of STRUCT on the
                // stack and then the LDFLD import code can no longer figure out if it needs the address.
                // So it checks if lvImpTypeInfo contains a handle, set by lvaInitVarDsc and others.
                //
                // In addition to this being confusing, it also seems to be a small CQ issue because some
                // other importer code sees that handle, thinks that the value is a struct and spills the
                // stack even if there's no need for that.

                impPushOnStack(op1, lvaTable[lclNum].lvImpTypeInfo);
                break;

            case CEE_ARGLIST:
                if (!info.compIsVarArgs)
                {
                    BADCODE("arglist in non-vararg method");
                }

                assertImp((info.compMethodInfo->args.callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_VARARG);

                // The ARGLIST cookie is a hidden 'last' parameter, we have already
                // adjusted the arg count cos this is like fetching the last param
                assertImp(info.compArgsCount > 0);
                assert(lvaGetDesc(lvaVarargsHandleArg)->lvAddrExposed);

                lclNum = lvaVarargsHandleArg;
                op1    = gtNewLclvNode(lclNum, TYP_I_IMPL DEBUGARG(opcodeOffs + sz + 1));
                op1    = gtNewAddrNode(op1);
                impPushOnStack(op1, typeInfo());
                break;

            case CEE_ENDFINALLY:

                if (compIsForInlining())
                {
                    assert(!"Shouldn't have exception handlers in the inliner!");
                    compInlineResult->NoteFatal(InlineObservation::CALLEE_HAS_ENDFINALLY);
                    return;
                }

                if (verCurrentState.esStackDepth > 0)
                {
                    impEvalSideEffects();
                }

                if (info.compXcptnsCount == 0)
                {
                    BADCODE("endfinally outside finally");
                }

                assert(verCurrentState.esStackDepth == 0);

                op1 = gtNewOperNode(GT_RETFILT, TYP_VOID, nullptr);
                goto APPEND;

            case CEE_ENDFILTER:

                if (compIsForInlining())
                {
                    assert(!"Shouldn't have exception handlers in the inliner!");
                    compInlineResult->NoteFatal(InlineObservation::CALLEE_HAS_ENDFILTER);
                    return;
                }

                block->bbSetRunRarely(); // filters are rare

                if (info.compXcptnsCount == 0)
                {
                    BADCODE("endfilter outside filter");
                }

                op1 = impPopStack().val;
                assertImp(op1->gtType == TYP_INT);
                if (!bbInFilterILRange(block))
                {
                    BADCODE("EndFilter outside a filter handler");
                }

                /* Mark current bb as end of filter */

                assert(compCurBB->bbFlags & BBF_DONT_REMOVE);
                assert(compCurBB->bbJumpKind == BBJ_EHFILTERRET);

                /* Mark catch handler as successor */

                op1 = gtNewOperNode(GT_RETFILT, op1->TypeGet(), op1);
                if (verCurrentState.esStackDepth != 0)
                {
                    BADCODE("stack must be 1 on end of filter");
                }
                goto APPEND;

            case CEE_RET:
                prefixFlags &= ~PREFIX_TAILCALL; // ret without call before it

                if (compIsForInlining())
                {
                    if (!impInlineReturnInstruction())
                    {
                        return;
                    }

                    break;
                }

                impReturnInstruction(prefixFlags, &opcode);
                break;

            case CEE_JMP:
                assert(!compIsForInlining());

                if ((info.compFlags & CORINFO_FLG_SYNCH) || block->hasTryIndex() || block->hasHndIndex())
                {
                    /* CEE_JMP does not make sense in some "protected" regions. */

                    BADCODE("Jmp not allowed in protected region");
                }

                if (opts.IsReversePInvoke())
                {
                    BADCODE("Jmp not allowed in reverse P/Invoke");
                }

                if (verCurrentState.esStackDepth != 0)
                {
                    BADCODE("Stack must be empty after CEE_JMPs");
                }

                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Method);

                JITDUMP(" %08X", resolvedToken.token);

                /* The signature of the target has to be identical to ours.
                   At least check that argCnt and returnType match */

                eeGetMethodSig(resolvedToken.hMethod, &sig);
                if (sig.numArgs != info.compMethodInfo->args.numArgs ||
                    sig.retType != info.compMethodInfo->args.retType ||
                    sig.callConv != info.compMethodInfo->args.callConv)
                {
                    BADCODE("Incompatible target for CEE_JMPs");
                }

                op1 = new (this, GT_JMP) GenTreeVal(GT_JMP, TYP_VOID, (size_t)resolvedToken.hMethod);

                /* Mark the basic block as being a JUMP instead of RETURN */

                block->bbFlags |= BBF_HAS_JMP;

                /* Set this flag to make sure register arguments have a location assigned
                 * even if we don't use them inside the method */

                compJmpOpUsed = true;

                fgNoStructPromotion = true;

                goto APPEND;

            case CEE_LDELEMA:
                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);
                clsHnd = resolvedToken.hClass;

                // If it's a value class array we just do a simple address-of
                if (info.compCompHnd->isValueClass(resolvedToken.hClass))
                {
                    lclTyp = JITtype2varType(info.compCompHnd->asCorInfoType(clsHnd));
                    goto ARR_LD;
                }

                // Similarly, if its a readonly access, we can do a simple address-of
                // without doing a runtime type-check
                if ((prefixFlags & PREFIX_READONLY) != 0)
                {
                    lclTyp = TYP_REF;
                    goto ARR_LD;
                }

                // Otherwise we need the full helper function with run-time type check
                op1 = impTokenToHandle(&resolvedToken);
                if (op1 == nullptr)
                {
                    return;
                }

                {
                    GenTreeCall::Use* args = gtNewCallArgs(op1);                           // Type
                    args                   = gtPrependNewCallArg(impPopStack().val, args); // index
                    args                   = gtPrependNewCallArg(impPopStack().val, args); // array
                    op1                    = gtNewHelperCallNode(CORINFO_HELP_LDELEMA_REF, TYP_BYREF, args);
                }

                impPushOnStack(op1, typeInfo());
                break;

            case CEE_LDELEM:
                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);
                clsHnd = resolvedToken.hClass;

                if (info.compCompHnd->isValueClass(resolvedToken.hClass))
                {
                    lclTyp = JITtype2varType(info.compCompHnd->asCorInfoType(clsHnd));
                    goto ARR_LD;
                }

                opcode = CEE_LDELEM_REF;
                __fallthrough;
            case CEE_LDELEM_REF:
                lclTyp = TYP_REF;
                goto ARR_LD;
            case CEE_LDELEM_I1:
                lclTyp = TYP_BYTE;
                goto ARR_LD;
            case CEE_LDELEM_I2:
                lclTyp = TYP_SHORT;
                goto ARR_LD;
            case CEE_LDELEM_I:
                lclTyp = TYP_I_IMPL;
                goto ARR_LD;
            case CEE_LDELEM_U4:
                lclTyp = TYP_INT;
                goto ARR_LD;
            case CEE_LDELEM_I4:
                lclTyp = TYP_INT;
                goto ARR_LD;
            case CEE_LDELEM_I8:
                lclTyp = TYP_LONG;
                goto ARR_LD;
            case CEE_LDELEM_R4:
                lclTyp = TYP_FLOAT;
                goto ARR_LD;
            case CEE_LDELEM_R8:
                lclTyp = TYP_DOUBLE;
                goto ARR_LD;
            case CEE_LDELEM_U1:
                lclTyp = TYP_UBYTE;
                goto ARR_LD;
            case CEE_LDELEM_U2:
                lclTyp = TYP_USHORT;
            ARR_LD:
                op2 = impPopStack().val; // Index
                op1 = impPopStack().val; // Array reference
                assertImp(op1->gtType == TYP_REF);

                /* Check for null pointer - in the inliner case we simply abort */

                if (compIsForInlining())
                {
                    if (op1->gtOper == GT_CNS_INT)
                    {
                        compInlineResult->NoteFatal(InlineObservation::CALLEE_HAS_NULL_FOR_LDELEM);
                        return;
                    }
                }

                op1 = impCheckForNullPointer(op1);

                /* Mark the block as containing an index expression */

                if (op1->gtOper == GT_LCL_VAR)
                {
                    if (op2->gtOper == GT_LCL_VAR || op2->gtOper == GT_CNS_INT || op2->gtOper == GT_ADD)
                    {
                        block->bbFlags |= BBF_HAS_IDX_LEN;
                        optMethodFlags |= OMF_HAS_ARRAYREF;
                    }
                }

                op1 = gtNewArrayIndex(lclTyp, op1, op2);

                if (lclTyp == TYP_STRUCT)
                {
                    assert((opcode == CEE_LDELEM) || (opcode == CEE_LDELEMA));

                    ClassLayout* layout = typGetObjLayout(clsHnd);
                    op1->SetType(typGetStructType(layout));
                    op1->AsIndex()->SetLayout(layout);
                    op1->AsIndex()->SetElemSize(layout->GetSize());
                }

                if ((opcode == CEE_LDELEMA) || (lclTyp == TYP_STRUCT))
                {
                    op1 = gtNewAddrNode(op1);
                }

                if (opcode == CEE_LDELEM)
                {
                    if (lclTyp == TYP_STRUCT)
                    {
                        op1 = gtNewObjNode(clsHnd, op1);
                        op1->gtFlags |= GTF_EXCEPT;
                    }

                    if (varTypeUsesFloatReg(op1->GetType()))
                    {
                        compFloatingPointUsed = true;
                    }
                }

                if ((opcode == CEE_LDELEM) &&
                    ((lclTyp == TYP_STRUCT) ||
                     (info.compCompHnd->getTypeForPrimitiveValueClass(clsHnd) == CORINFO_TYPE_UNDEF)))
                {
                    impPushOnStack(op1, typeInfo(TI_STRUCT, clsHnd));
                }
                else
                {
                    impPushOnStack(op1, typeInfo());
                }
                break;

            // stelem for reference and value types
            case CEE_STELEM:
                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);
                clsHnd = resolvedToken.hClass;

                if (eeIsValueClass(clsHnd))
                {
                    lclTyp = JITtype2varType(info.compCompHnd->asCorInfoType(clsHnd));
                    goto ARR_ST;
                }

                // If it's a reference type just behave as though it's a stelem.ref instruction
                FALLTHROUGH;

            case CEE_STELEM_REF:
                if (opts.OptimizationEnabled())
                {
                    GenTree* array = impStackTop(2).val;
                    GenTree* value = impStackTop().val;

                    // Is this a case where we can skip the covariant store check?
                    if (impCanSkipCovariantStoreCheck(value, array))
                    {
                        lclTyp = TYP_REF;
                        goto ARR_ST;
                    }
                }

                // Else call a helper function to do the assignment
                op1 = gtNewHelperCallNode(CORINFO_HELP_ARRADDR_ST, TYP_VOID, impPopCallArgs(3, nullptr));
                goto SPILL_ALL_APPEND;

            case CEE_STELEM_I1:
                lclTyp = TYP_BYTE;
                goto ARR_ST;
            case CEE_STELEM_I2:
                lclTyp = TYP_SHORT;
                goto ARR_ST;
            case CEE_STELEM_I:
                lclTyp = TYP_I_IMPL;
                goto ARR_ST;
            case CEE_STELEM_I4:
                lclTyp = TYP_INT;
                goto ARR_ST;
            case CEE_STELEM_I8:
                lclTyp = TYP_LONG;
                goto ARR_ST;
            case CEE_STELEM_R4:
                lclTyp = TYP_FLOAT;
                goto ARR_ST;
            case CEE_STELEM_R8:
                lclTyp = TYP_DOUBLE;
                goto ARR_ST;

            ARR_ST:
                /* The strict order of evaluation is LHS-operands, RHS-operands,
                   range-check, and then assignment. However, codegen currently
                   does the range-check before evaluation the RHS-operands. So to
                   maintain strict ordering, we spill the stack. */

                if (impStackTop().val->gtFlags & GTF_SIDE_EFFECT)
                {
                    impSpillSideEffects(false, (unsigned)CHECK_SPILL_ALL DEBUGARG(
                                                   "Strict ordering of exceptions for Array store"));
                }

                /* Pull the new value from the stack */
                op2 = impPopStack().val;

                /* Pull the index value */
                op1 = impPopStack().val;

                /* Pull the array address */
                op3 = impPopStack().val;

                assertImp(op3->gtType == TYP_REF);
                if (op2->IsLocalAddrExpr() != nullptr)
                {
                    op2->gtType = TYP_I_IMPL;
                }

                op3 = impCheckForNullPointer(op3);

                // Mark the block as containing an index expression

                if (op3->gtOper == GT_LCL_VAR)
                {
                    if (op1->gtOper == GT_LCL_VAR || op1->gtOper == GT_CNS_INT || op1->gtOper == GT_ADD)
                    {
                        block->bbFlags |= BBF_HAS_IDX_LEN;
                        optMethodFlags |= OMF_HAS_ARRAYREF;
                    }
                }

                op1 = gtNewArrayIndex(lclTyp, op3, op1);

                if (lclTyp == TYP_STRUCT)
                {
                    ClassLayout* layout = typGetObjLayout(clsHnd);
                    op1->SetType(typGetStructType(layout));
                    op1->AsIndex()->SetLayout(layout);
                    op1->AsIndex()->SetElemSize(layout->GetSize());

                    op1 = impAssignStruct(op1, op2, clsHnd, CHECK_SPILL_ALL);
                }
                else
                {
                    op2 = impImplicitR4orR8Cast(op2, lclTyp);
                    op2 = impImplicitIorI4Cast(op2, lclTyp);

                    op1 = gtNewAssignNode(op1, op2);
                }

                if (varTypeUsesFloatReg(op1->GetType()))
                {
                    compFloatingPointUsed = true;
                }

                goto SPILL_APPEND;

            case CEE_ADD:
                oper = GT_ADD;
                goto MATH_OP2;
            case CEE_ADD_OVF:
                uns = false;
                goto ADD_OVF;
            case CEE_ADD_OVF_UN:
                uns = true;
            ADD_OVF:
                ovfl     = true;
                callNode = false;
                oper     = GT_ADD;
                goto MATH_OP2_FLAGS;

            case CEE_SUB:
                oper = GT_SUB;
                goto MATH_OP2;
            case CEE_SUB_OVF:
                uns = false;
                goto SUB_OVF;
            case CEE_SUB_OVF_UN:
                uns = true;
            SUB_OVF:
                ovfl     = true;
                callNode = false;
                oper     = GT_SUB;
                goto MATH_OP2_FLAGS;

            case CEE_MUL:
                oper = GT_MUL;
                goto MATH_MAYBE_CALL_NO_OVF;
            case CEE_MUL_OVF:
                uns = false;
                goto MUL_OVF;
            case CEE_MUL_OVF_UN:
                uns = true;
            MUL_OVF:
                ovfl = true;
                oper = GT_MUL;
                goto MATH_MAYBE_CALL_OVF;

            // Other binary math operations

            case CEE_DIV:
                oper = GT_DIV;
                goto MATH_MAYBE_CALL_NO_OVF;
            case CEE_DIV_UN:
                oper = GT_UDIV;
                goto MATH_MAYBE_CALL_NO_OVF;
            case CEE_REM:
                oper = GT_MOD;
                goto MATH_MAYBE_CALL_NO_OVF;
            case CEE_REM_UN:
                oper = GT_UMOD;
            MATH_MAYBE_CALL_NO_OVF:
                ovfl = false;
            MATH_MAYBE_CALL_OVF:
                // Morpher has some complex logic about when to turn different
                // typed nodes on different platforms into helper calls. We
                // need to either duplicate that logic here, or just
                // pessimistically make all the nodes large enough to become
                // call nodes.  Since call nodes aren't that much larger and
                // these opcodes are infrequent enough I chose the latter.
                callNode = true;
                goto MATH_OP2_FLAGS;

            case CEE_AND:
                oper = GT_AND;
                goto MATH_OP2;
            case CEE_OR:
                oper = GT_OR;
                goto MATH_OP2;
            case CEE_XOR:
                oper = GT_XOR;
            MATH_OP2: // For default values of 'ovfl' and 'callNode'
                ovfl     = false;
                callNode = false;

            MATH_OP2_FLAGS: // If 'ovfl' and 'callNode' have already been set
                op2 = impPopStack().val;
                op1 = impPopStack().val;

                type = impGetNumericBinaryOpType(oper, uns, &op1, &op2);

                assert(!ovfl || !varTypeIsFloating(type));

                if ((op2->IsIntegralConst(0) && (oper == GT_ADD || oper == GT_SUB)) ||
                    (op2->IsIntegralConst(1) && (oper == GT_MUL || oper == GT_DIV)))
                {
                    impPushOnStack(op1, typeInfo());
                    break;
                }

                if (callNode)
                {
                    /* These operators can later be transformed into 'GT_CALL' */

                    assert(GenTree::s_gtNodeSizes[GT_CALL] > GenTree::s_gtNodeSizes[GT_MUL]);
#ifndef TARGET_ARM
                    assert(GenTree::s_gtNodeSizes[GT_CALL] > GenTree::s_gtNodeSizes[GT_DIV]);
                    assert(GenTree::s_gtNodeSizes[GT_CALL] > GenTree::s_gtNodeSizes[GT_UDIV]);
                    assert(GenTree::s_gtNodeSizes[GT_CALL] > GenTree::s_gtNodeSizes[GT_MOD]);
                    assert(GenTree::s_gtNodeSizes[GT_CALL] > GenTree::s_gtNodeSizes[GT_UMOD]);
#endif
                    // It's tempting to use LargeOpOpcode() here, but this logic is *not* saying
                    // that we'll need to transform into a general large node, but rather specifically
                    // to a call: by doing it this way, things keep working if there are multiple sizes,
                    // and a CALL is no longer the largest.
                    // That said, as of now it *is* a large node, so we'll do this with an assert rather
                    // than an "if".
                    assert(GenTree::s_gtNodeSizes[GT_CALL] == TREE_NODE_SZ_LARGE);
                    op1 = new (this, GT_CALL) GenTreeOp(oper, type, op1, op2 DEBUGARG(/*largeNode*/ true));
                }
                else
                {
                    op1 = gtNewOperNode(oper, type, op1, op2);
                }

                if (ovfl)
                {
                    assert(op1->OperIs(GT_ADD, GT_SUB, GT_MUL));

                    op1->gtFlags |= (GTF_EXCEPT | GTF_OVERFLOW);

                    if (uns)
                    {
                        op1->gtFlags |= GTF_UNSIGNED;
                    }
                }
                else if (varTypeIsIntegral(op1->GetType()) && op1->OperIs(GT_DIV, GT_UDIV, GT_MOD, GT_UMOD))
                {
                    if (op1->OperMayThrow(this))
                    {
                        op1->gtFlags |= GTF_EXCEPT;
                    }
                }
                else
                {
                    assert(!op1->OperMayThrow(this));
                }

                impPushOnStack(op1, typeInfo());
                break;

            case CEE_SHL:
                oper = GT_LSH;
                goto CEE_SH_OP2;
            case CEE_SHR:
                oper = GT_RSH;
                goto CEE_SH_OP2;
            case CEE_SHR_UN:
                oper = GT_RSZ;
            CEE_SH_OP2:
                op2 = impPopStack().val;
                op1 = impPopStack().val; // operand to be shifted
                impBashVarAddrsToI(op1, op2);
                type = genActualType(op1->TypeGet());
                op1  = gtNewOperNode(oper, type, op1, op2);
                impPushOnStack(op1, typeInfo());
                break;

            case CEE_NOT:
                op1 = impPopStack().val;
                impBashVarAddrsToI(op1, nullptr);
                type = genActualType(op1->TypeGet());
                impPushOnStack(gtNewOperNode(GT_NOT, type, op1), typeInfo());
                break;

            case CEE_CKFINITE:
                op1  = impPopStack().val;
                type = op1->TypeGet();
                op1  = gtNewOperNode(GT_CKFINITE, type, op1);
                op1->gtFlags |= GTF_EXCEPT;
                impPushOnStack(op1, typeInfo());
                break;

            case CEE_LEAVE:
                val     = getI4LittleEndian(codeAddr); // jump distance
                jmpAddr = (IL_OFFSET)((codeAddr - info.compCode + sizeof(__int32)) + val);
                goto LEAVE;

            case CEE_LEAVE_S:
                val     = getI1LittleEndian(codeAddr); // jump distance
                jmpAddr = (IL_OFFSET)((codeAddr - info.compCode + sizeof(__int8)) + val);

            LEAVE:
                if (compIsForInlining())
                {
                    compInlineResult->NoteFatal(InlineObservation::CALLEE_HAS_LEAVE);
                    return;
                }

                JITDUMP(" %04X", jmpAddr);
                if (block->bbJumpKind != BBJ_LEAVE)
                {
                    impResetLeaveBlock(block, jmpAddr);
                }

                assert(jmpAddr == block->bbJumpDest->bbCodeOffs);
                impImportLeave(block);
                impNoteBranchOffs();
                break;

            case CEE_BR:
            case CEE_BR_S:
                jmpDist = (sz == 1) ? getI1LittleEndian(codeAddr) : getI4LittleEndian(codeAddr);

                if (compIsForInlining() && jmpDist == 0)
                {
                    break; /* NOP */
                }

                impNoteBranchOffs();
                break;

            case CEE_BRTRUE:
            case CEE_BRTRUE_S:
            case CEE_BRFALSE:
            case CEE_BRFALSE_S:
                op1  = impPopStack().val;
                type = op1->TypeGet();

                // Per Ecma-355, brfalse and brtrue are only specified for nint, ref, and byref.
                //
                // We've historically been a bit more permissive, so here we allow
                // any type that gtNewZeroConNode can handle.
                if (!varTypeIsArithmetic(type) && !varTypeIsGC(type))
                {
                    BADCODE("invalid type for brtrue/brfalse");
                }

                if (opts.OptimizationEnabled() && (block->bbJumpDest == block->bbNext))
                {
                    block->bbJumpKind = BBJ_NONE;

                    if ((op1->gtFlags & GTF_GLOB_EFFECT) != 0)
                    {
                        op1 = gtUnusedValNode(op1);
                        goto SPILL_ALL_APPEND;
                    }

                    break;
                }

                if (op1->OperIsCompare())
                {
                    if (opcode == CEE_BRFALSE || opcode == CEE_BRFALSE_S)
                    {
                        // Flip the sense of the compare

                        op1 = gtReverseCond(op1);
                    }
                }
                else
                {
                    // We'll compare against an equally-sized integer 0
                    // For small types, we always compare against int
                    op2 = gtNewZeroConNode(genActualType(op1->gtType));

                    // Create the comparison operator and try to fold it
                    oper = (opcode == CEE_BRTRUE || opcode == CEE_BRTRUE_S) ? GT_NE : GT_EQ;
                    op1  = gtNewOperNode(oper, TYP_INT, op1, op2);
                }

            // fall through

            COND_JUMP:

                /* Fold comparison if we can */

                op1 = gtFoldExpr(op1);

                /* Try to fold the really simple cases like 'iconst *, ifne/ifeq'*/
                /* Don't make any blocks unreachable in import only mode */

                if (op1->OperIs(GT_CNS_INT))
                {
                    /* gtFoldExpr() should prevent this as we don't want to make any blocks
                       unreachable under compDbgCode */
                    assert(!opts.compDbgCode);

                    BBjumpKinds foldedJumpKind = (BBjumpKinds)(op1->AsIntCon()->gtIconVal ? BBJ_ALWAYS : BBJ_NONE);
                    assertImp((block->bbJumpKind == BBJ_COND)            // normal case
                              || (block->bbJumpKind == foldedJumpKind)); // this can happen if we are reimporting the
                                                                         // block for the second time

                    block->bbJumpKind = foldedJumpKind;
#ifdef DEBUG
                    if (verbose)
                    {
                        if (op1->AsIntCon()->gtIconVal)
                        {
                            printf("\nThe conditional jump becomes an unconditional jump to " FMT_BB "\n",
                                   block->bbJumpDest->bbNum);
                        }
                        else
                        {
                            printf("\nThe block falls through into the next " FMT_BB "\n", block->bbNext->bbNum);
                        }
                    }
#endif
                    break;
                }

                op1 = gtNewOperNode(GT_JTRUE, TYP_VOID, op1);

                // GT_JTRUE is handled specially for non-empty stacks. See 'addStmt'
                // in impImportBlock(block). For correct line numbers, spill stack. */

                if (opts.compDbgCode && impCurStmtOffs != BAD_IL_OFFSET)
                {
                    impSpillStackEnsure(true);
                }

                goto SPILL_ALL_APPEND;

            case CEE_CEQ:
                oper = GT_EQ;
                uns  = false;
                goto CMP_2_OPs;
            case CEE_CGT_UN:
                oper = GT_GT;
                uns  = true;
                goto CMP_2_OPs;
            case CEE_CGT:
                oper = GT_GT;
                uns  = false;
                goto CMP_2_OPs;
            case CEE_CLT_UN:
                oper = GT_LT;
                uns  = true;
                goto CMP_2_OPs;
            case CEE_CLT:
                oper = GT_LT;
                uns  = false;
            CMP_2_OPs:
                op2 = impPopStack().val;
                op1 = impPopStack().val;

#ifdef TARGET_64BIT
                if (varTypeIsI(op1->TypeGet()) && (genActualType(op2->TypeGet()) == TYP_INT))
                {
                    op2 = gtNewCastNode(TYP_I_IMPL, op2, uns, uns ? TYP_U_IMPL : TYP_I_IMPL);
                }
                else if (varTypeIsI(op2->TypeGet()) && (genActualType(op1->TypeGet()) == TYP_INT))
                {
                    op1 = gtNewCastNode(TYP_I_IMPL, op1, uns, uns ? TYP_U_IMPL : TYP_I_IMPL);
                }
#endif // TARGET_64BIT

                assertImp(genActualType(op1->TypeGet()) == genActualType(op2->TypeGet()) ||
                          (varTypeIsI(op1->TypeGet()) && varTypeIsI(op2->TypeGet())) ||
                          (varTypeIsFloating(op1->gtType) && varTypeIsFloating(op2->gtType)));

                /* Create the comparison node */

                op1 = gtNewOperNode(oper, TYP_INT, op1, op2);

                /* TODO: setting both flags when only one is appropriate */
                if (opcode == CEE_CGT_UN || opcode == CEE_CLT_UN)
                {
                    op1->gtFlags |= GTF_RELOP_NAN_UN | GTF_UNSIGNED;
                }

                // Fold result, if possible.
                op1 = gtFoldExpr(op1);

                impPushOnStack(op1, typeInfo());
                break;

            case CEE_BEQ_S:
            case CEE_BEQ:
                oper = GT_EQ;
                goto CMP_2_OPs_AND_BR;

            case CEE_BGE_S:
            case CEE_BGE:
                oper = GT_GE;
                goto CMP_2_OPs_AND_BR;

            case CEE_BGE_UN_S:
            case CEE_BGE_UN:
                oper = GT_GE;
                goto CMP_2_OPs_AND_BR_UN;

            case CEE_BGT_S:
            case CEE_BGT:
                oper = GT_GT;
                goto CMP_2_OPs_AND_BR;

            case CEE_BGT_UN_S:
            case CEE_BGT_UN:
                oper = GT_GT;
                goto CMP_2_OPs_AND_BR_UN;

            case CEE_BLE_S:
            case CEE_BLE:
                oper = GT_LE;
                goto CMP_2_OPs_AND_BR;

            case CEE_BLE_UN_S:
            case CEE_BLE_UN:
                oper = GT_LE;
                goto CMP_2_OPs_AND_BR_UN;

            case CEE_BLT_S:
            case CEE_BLT:
                oper = GT_LT;
                goto CMP_2_OPs_AND_BR;

            case CEE_BLT_UN_S:
            case CEE_BLT_UN:
                oper = GT_LT;
                goto CMP_2_OPs_AND_BR_UN;

            case CEE_BNE_UN_S:
            case CEE_BNE_UN:
                oper = GT_NE;
                goto CMP_2_OPs_AND_BR_UN;

            CMP_2_OPs_AND_BR_UN:
                uns       = true;
                unordered = true;
                goto CMP_2_OPs_AND_BR_ALL;
            CMP_2_OPs_AND_BR:
                uns       = false;
                unordered = false;
                goto CMP_2_OPs_AND_BR_ALL;
            CMP_2_OPs_AND_BR_ALL:
                op2 = impPopStack().val;
                op1 = impPopStack().val;

#ifdef TARGET_64BIT
                if ((op1->TypeGet() == TYP_I_IMPL) && (genActualType(op2->TypeGet()) == TYP_INT))
                {
                    op2 = gtNewCastNode(TYP_I_IMPL, op2, uns, uns ? TYP_U_IMPL : TYP_I_IMPL);
                }
                else if ((op2->TypeGet() == TYP_I_IMPL) && (genActualType(op1->TypeGet()) == TYP_INT))
                {
                    op1 = gtNewCastNode(TYP_I_IMPL, op1, uns, uns ? TYP_U_IMPL : TYP_I_IMPL);
                }
#endif // TARGET_64BIT

                assertImp(genActualType(op1->TypeGet()) == genActualType(op2->TypeGet()) ||
                          (varTypeIsI(op1->TypeGet()) && varTypeIsI(op2->TypeGet())) ||
                          (varTypeIsFloating(op1->gtType) && varTypeIsFloating(op2->gtType)));

                if (opts.OptimizationEnabled() && (block->bbJumpDest == block->bbNext))
                {
                    block->bbJumpKind = BBJ_NONE;

                    if (op1->gtFlags & GTF_GLOB_EFFECT)
                    {
                        impSpillSideEffects(false, (unsigned)CHECK_SPILL_ALL DEBUGARG(
                                                       "Branch to next Optimization, op1 side effect"));
                        impAppendTree(gtUnusedValNode(op1), (unsigned)CHECK_SPILL_NONE, impCurStmtOffs);
                    }
                    if (op2->gtFlags & GTF_GLOB_EFFECT)
                    {
                        impSpillSideEffects(false, (unsigned)CHECK_SPILL_ALL DEBUGARG(
                                                       "Branch to next Optimization, op2 side effect"));
                        impAppendTree(gtUnusedValNode(op2), (unsigned)CHECK_SPILL_NONE, impCurStmtOffs);
                    }

#ifdef DEBUG
                    if ((op1->gtFlags | op2->gtFlags) & GTF_GLOB_EFFECT)
                    {
                        impNoteLastILoffs();
                    }
#endif
                    break;
                }

                // We can generate an compare of different sized floating point op1 and op2
                // We insert a cast
                //
                if (varTypeIsFloating(op1->TypeGet()))
                {
                    if (op1->TypeGet() != op2->TypeGet())
                    {
                        assert(varTypeIsFloating(op2->TypeGet()));

                        // say op1=double, op2=float. To avoid loss of precision
                        // while comparing, op2 is converted to double and double
                        // comparison is done.
                        if (op1->TypeGet() == TYP_DOUBLE)
                        {
                            // We insert a cast of op2 to TYP_DOUBLE
                            op2 = gtNewCastNode(TYP_DOUBLE, op2, false, TYP_DOUBLE);
                        }
                        else if (op2->TypeGet() == TYP_DOUBLE)
                        {
                            // We insert a cast of op1 to TYP_DOUBLE
                            op1 = gtNewCastNode(TYP_DOUBLE, op1, false, TYP_DOUBLE);
                        }
                    }
                }

                /* Create and append the operator */

                op1 = gtNewOperNode(oper, TYP_INT, op1, op2);

                if (uns)
                {
                    op1->gtFlags |= GTF_UNSIGNED;
                }

                if (unordered)
                {
                    op1->gtFlags |= GTF_RELOP_NAN_UN;
                }

                goto COND_JUMP;

            case CEE_SWITCH:
                assert(!compIsForInlining());

                op1 = impPopStack().val;
                assertImp(genActualTypeIsIntOrI(op1->TypeGet()));

                /* We can create a switch node */

                op1 = gtNewOperNode(GT_SWITCH, TYP_VOID, op1);

                val = (int)getU4LittleEndian(codeAddr);
                codeAddr += 4 + val * 4; // skip over the switch-table

                goto SPILL_ALL_APPEND;

            /************************** Casting OPCODES ***************************/

            case CEE_CONV_OVF_I1:
                lclTyp = TYP_BYTE;
                goto CONV_OVF;
            case CEE_CONV_OVF_I2:
                lclTyp = TYP_SHORT;
                goto CONV_OVF;
            case CEE_CONV_OVF_I:
                lclTyp = TYP_I_IMPL;
                goto CONV_OVF;
            case CEE_CONV_OVF_I4:
                lclTyp = TYP_INT;
                goto CONV_OVF;
            case CEE_CONV_OVF_I8:
                lclTyp = TYP_LONG;
                goto CONV_OVF;

            case CEE_CONV_OVF_U1:
                lclTyp = TYP_UBYTE;
                goto CONV_OVF;
            case CEE_CONV_OVF_U2:
                lclTyp = TYP_USHORT;
                goto CONV_OVF;
            case CEE_CONV_OVF_U:
                lclTyp = TYP_U_IMPL;
                goto CONV_OVF;
            case CEE_CONV_OVF_U4:
                lclTyp = TYP_UINT;
                goto CONV_OVF;
            case CEE_CONV_OVF_U8:
                lclTyp = TYP_ULONG;
                goto CONV_OVF;

            case CEE_CONV_OVF_I1_UN:
                lclTyp = TYP_BYTE;
                goto CONV_OVF_UN;
            case CEE_CONV_OVF_I2_UN:
                lclTyp = TYP_SHORT;
                goto CONV_OVF_UN;
            case CEE_CONV_OVF_I_UN:
                lclTyp = TYP_I_IMPL;
                goto CONV_OVF_UN;
            case CEE_CONV_OVF_I4_UN:
                lclTyp = TYP_INT;
                goto CONV_OVF_UN;
            case CEE_CONV_OVF_I8_UN:
                lclTyp = TYP_LONG;
                goto CONV_OVF_UN;

            case CEE_CONV_OVF_U1_UN:
                lclTyp = TYP_UBYTE;
                goto CONV_OVF_UN;
            case CEE_CONV_OVF_U2_UN:
                lclTyp = TYP_USHORT;
                goto CONV_OVF_UN;
            case CEE_CONV_OVF_U_UN:
                lclTyp = TYP_U_IMPL;
                goto CONV_OVF_UN;
            case CEE_CONV_OVF_U4_UN:
                lclTyp = TYP_UINT;
                goto CONV_OVF_UN;
            case CEE_CONV_OVF_U8_UN:
                lclTyp = TYP_ULONG;
                goto CONV_OVF_UN;

            CONV_OVF_UN:
                uns = true;
                goto CONV_OVF_COMMON;
            CONV_OVF:
                uns = false;
                goto CONV_OVF_COMMON;

            CONV_OVF_COMMON:
                ovfl = true;
                goto _CONV;

            case CEE_CONV_I1:
                lclTyp = TYP_BYTE;
                goto CONV;
            case CEE_CONV_I2:
                lclTyp = TYP_SHORT;
                goto CONV;
            case CEE_CONV_I:
                lclTyp = TYP_I_IMPL;
                goto CONV;
            case CEE_CONV_I4:
                lclTyp = TYP_INT;
                goto CONV;
            case CEE_CONV_I8:
                lclTyp = TYP_LONG;
                goto CONV;

            case CEE_CONV_U1:
                lclTyp = TYP_UBYTE;
                goto CONV;
            case CEE_CONV_U2:
                lclTyp = TYP_USHORT;
                goto CONV;
#if (REGSIZE_BYTES == 8)
            case CEE_CONV_U:
                lclTyp = TYP_U_IMPL;
                goto CONV_UN;
#else
            case CEE_CONV_U:
                lclTyp = TYP_U_IMPL;
                goto CONV;
#endif
            case CEE_CONV_U4:
                lclTyp = TYP_UINT;
                goto CONV;
            case CEE_CONV_U8:
                lclTyp = TYP_ULONG;
                goto CONV_UN;

            case CEE_CONV_R4:
                lclTyp = TYP_FLOAT;
                goto CONV;
            case CEE_CONV_R8:
                lclTyp = TYP_DOUBLE;
                goto CONV;

            case CEE_CONV_R_UN:
                lclTyp = TYP_DOUBLE;
                goto CONV_UN;

            CONV_UN:
                uns  = true;
                ovfl = false;
                goto _CONV;

            CONV:
                uns  = false;
                ovfl = false;
                goto _CONV;

            _CONV:
                op1 = impPopStack().val;
                impBashVarAddrsToI(op1);

                if (varTypeIsSmall(lclTyp) && !ovfl && op1->gtType == TYP_INT && op1->gtOper == GT_AND)
                {
                    op2 = op1->AsOp()->gtOp2;

                    if (op2->gtOper == GT_CNS_INT)
                    {
                        ssize_t ival = op2->AsIntCon()->gtIconVal;
                        ssize_t mask, umask;

                        switch (lclTyp)
                        {
                            case TYP_BYTE:
                            case TYP_UBYTE:
                                mask  = 0x00FF;
                                umask = 0x007F;
                                break;
                            case TYP_USHORT:
                            case TYP_SHORT:
                                mask  = 0xFFFF;
                                umask = 0x7FFF;
                                break;

                            default:
                                assert(!"unexpected type");
                                return;
                        }

                        if (((ival & umask) == ival) || ((ival & mask) == ival && uns))
                        {
                            /* Toss the cast, it's a waste of time */

                            impPushOnStack(op1, typeInfo());
                            break;
                        }
                        else if (ival == mask)
                        {
                            /* Toss the masking, it's a waste of time, since
                               we sign-extend from the small value anyways */

                            op1 = op1->AsOp()->gtOp1;
                        }
                    }
                }

                /*  The 'op2' sub-operand of a cast is the 'real' type number,
                    since the result of a cast to one of the 'small' integer
                    types is an integer.
                 */

                type = genActualType(lclTyp);

                // If this is a no-op cast, just use op1.
                if (!ovfl && (type == op1->TypeGet()) && (genTypeSize(type) == genTypeSize(lclTyp)))
                {
                    // Nothing needs to change
                }
                // Work is evidently required, add cast node
                else
                {
                    op1 = gtNewCastNode(type, op1, uns, lclTyp);

                    if (ovfl)
                    {
                        op1->gtFlags |= (GTF_OVERFLOW | GTF_EXCEPT);
                    }

                    if (op1->gtGetOp1()->OperIsConst() && opts.OptimizationEnabled())
                    {
                        // Try and fold the introduced cast
                        op1 = gtFoldExprConst(op1);
                    }
                }

                impPushOnStack(op1, typeInfo());
                break;

            case CEE_NEG:
                op1 = impPopStack().val;
                impBashVarAddrsToI(op1, nullptr);
                impPushOnStack(gtNewOperNode(GT_NEG, genActualType(op1->gtType), op1), typeInfo());
                break;

            case CEE_POP:
                op1 = impImportPop(block);

                if (op1 != nullptr)
                {
                    goto SPILL_APPEND;
                }

                break;

            case CEE_DUP:
            {
                // If the expression to dup is simple, just clone it.
                // Otherwise spill it to a temp, and reload the temp
                // twice.
                StackEntry se   = impPopStack();
                GenTree*   tree = se.val;
                op1             = tree;

                if (!opts.compDbgCode && !op1->IsIntegralConst(0) && !op1->IsDblConPositiveZero() && !op1->IsLocal())
                {
                    const unsigned tmpNum = lvaGrabTemp(true DEBUGARG("dup spill"));
                    impAssignTempGen(tmpNum, op1, se.seTypeInfo.GetClassHandle(), (unsigned)CHECK_SPILL_ALL);
                    var_types type = genActualType(lvaTable[tmpNum].TypeGet());
                    op1            = gtNewLclvNode(tmpNum, type);

                    // Propagate type info to the temp from the stack and the original tree
                    if (type == TYP_REF)
                    {
                        assert(lvaTable[tmpNum].lvSingleDef == 0);
                        lvaTable[tmpNum].lvSingleDef = 1;
                        JITDUMP("Marked V%02u as a single def local\n", tmpNum);
                        lvaSetClass(tmpNum, tree, se.seTypeInfo.GetClassHandle());
                    }
                }

                op1 = impCloneExpr(op1, &op2, se.seTypeInfo.GetClassHandle(),
                                   CHECK_SPILL_ALL DEBUGARG("DUP instruction"));

                assert(!(op1->gtFlags & GTF_GLOB_EFFECT) && !(op2->gtFlags & GTF_GLOB_EFFECT));
                impPushOnStack(op1, se.seTypeInfo);
                impPushOnStack(op2, se.seTypeInfo);
            }
            break;

            case CEE_STIND_I1:
                lclTyp = TYP_BYTE;
                goto STIND;
            case CEE_STIND_I2:
                lclTyp = TYP_SHORT;
                goto STIND;
            case CEE_STIND_I4:
                lclTyp = TYP_INT;
                goto STIND;
            case CEE_STIND_I8:
                lclTyp = TYP_LONG;
                goto STIND;
            case CEE_STIND_I:
                lclTyp = TYP_I_IMPL;
                goto STIND;
            case CEE_STIND_REF:
                lclTyp = TYP_REF;
                goto STIND;
            case CEE_STIND_R4:
                lclTyp = TYP_FLOAT;
                goto STIND;
            case CEE_STIND_R8:
                lclTyp = TYP_DOUBLE;
                goto STIND;
            STIND:
                op2 = impPopStack().val; // value to store
                op1 = impPopStack().val; // address to store to

                // you can indirect off of a TYP_I_IMPL (if we are in C) or a BYREF
                assertImp(op1->TypeIs(TYP_I_IMPL, TYP_BYREF));

                impBashVarAddrsToI(op1, op2);

                op2 = impImplicitR4orR8Cast(op2, lclTyp);
                op2 = impImplicitIorI4Cast(op2, lclTyp);

                if ((lclTyp == TYP_REF) && !op2->TypeIs(TYP_REF))
                {
                    // STIND_REF can be used to store TYP_INT, TYP_I_IMPL, TYP_REF, or TYP_BYREF.
                    assertImp(op2->TypeIs(TYP_INT, TYP_I_IMPL, TYP_BYREF));
                    lclTyp = op2->GetType();
                }

// Check target type.
#ifdef DEBUG
                if (op2->gtType == TYP_BYREF || lclTyp == TYP_BYREF)
                {
                    if (op2->gtType == TYP_BYREF)
                    {
                        assertImp(lclTyp == TYP_BYREF || lclTyp == TYP_I_IMPL);
                    }
                    else if (lclTyp == TYP_BYREF)
                    {
                        assertImp(op2->gtType == TYP_BYREF || varTypeIsIntOrI(op2->gtType));
                    }
                }
                else
                {
                    assertImp(genActualType(op2->gtType) == genActualType(lclTyp) ||
                              ((lclTyp == TYP_I_IMPL) && (genActualType(op2->gtType) == TYP_INT)) ||
                              (varTypeIsFloating(op2->gtType) && varTypeIsFloating(lclTyp)));
                }
#endif

            // For CPOBJ op2 always has type lclType so we can skip all the type
            // compatibility checks above.
            STIND_CPOBJ:
                op1 = gtNewOperNode(GT_IND, lclTyp, op1);

                if ((prefixFlags & PREFIX_VOLATILE) != 0)
                {
                    op1->gtFlags |= GTF_DONT_CSE;      // Can't CSE a volatile
                    op1->gtFlags |= GTF_ORDER_SIDEEFF; // Prevent this from being reordered
                    op1->gtFlags |= GTF_IND_VOLATILE;
                }

                if (((prefixFlags & PREFIX_UNALIGNED) != 0) && !varTypeIsByte(lclTyp))
                {
                    op1->gtFlags |= GTF_IND_UNALIGNED;
                }

                op1 = gtNewAssignNode(op1, op2);
                op1->gtFlags |= GTF_EXCEPT | GTF_GLOB_REF;

                // Spill side-effects AND global-data-accesses
                if (verCurrentState.esStackDepth > 0)
                {
                    impSpillSideEffects(true, (unsigned)CHECK_SPILL_ALL DEBUGARG("spill side effects before STIND"));
                }

                goto APPEND;

            case CEE_LDIND_I1:
                lclTyp = TYP_BYTE;
                goto LDIND;
            case CEE_LDIND_I2:
                lclTyp = TYP_SHORT;
                goto LDIND;
            case CEE_LDIND_U4:
            case CEE_LDIND_I4:
                lclTyp = TYP_INT;
                goto LDIND;
            case CEE_LDIND_I8:
                lclTyp = TYP_LONG;
                goto LDIND;
            case CEE_LDIND_REF:
                lclTyp = TYP_REF;
                goto LDIND;
            case CEE_LDIND_I:
                lclTyp = TYP_I_IMPL;
                goto LDIND;
            case CEE_LDIND_R4:
                lclTyp = TYP_FLOAT;
                goto LDIND;
            case CEE_LDIND_R8:
                lclTyp = TYP_DOUBLE;
                goto LDIND;
            case CEE_LDIND_U1:
                lclTyp = TYP_UBYTE;
                goto LDIND;
            case CEE_LDIND_U2:
                lclTyp = TYP_USHORT;
                goto LDIND;
            LDIND:
                op1 = impPopStack().val; // address to load from
                impBashVarAddrsToI(op1);

#ifdef TARGET_64BIT
                // Allow an upcast of op1 from a 32-bit Int into TYP_I_IMPL for x86 JIT compatiblity
                if (genActualType(op1->gtType) == TYP_INT)
                {
                    op1 = gtNewCastNode(TYP_I_IMPL, op1, false, TYP_I_IMPL);
                }
#endif

                assertImp(genActualType(op1->gtType) == TYP_I_IMPL || op1->gtType == TYP_BYREF);

                op1 = gtNewOperNode(GT_IND, lclTyp, op1);

                op1->gtFlags |= GTF_EXCEPT | GTF_GLOB_REF;

                if (prefixFlags & PREFIX_VOLATILE)
                {
                    assert(op1->OperGet() == GT_IND);
                    op1->gtFlags |= GTF_DONT_CSE;      // Can't CSE a volatile
                    op1->gtFlags |= GTF_ORDER_SIDEEFF; // Prevent this from being reordered
                    op1->gtFlags |= GTF_IND_VOLATILE;
                }

                if ((prefixFlags & PREFIX_UNALIGNED) && !varTypeIsByte(lclTyp))
                {
                    assert(op1->OperGet() == GT_IND);
                    op1->gtFlags |= GTF_IND_UNALIGNED;
                }

                impPushOnStack(op1, typeInfo());
                break;

            case CEE_UNALIGNED:
                assert(sz == 1);
                val = getU1LittleEndian(codeAddr);
                ++codeAddr;
                JITDUMP(" %u", val);
                if ((val != 1) && (val != 2) && (val != 4))
                {
                    BADCODE("Alignment unaligned. must be 1, 2, or 4");
                }

                if ((prefixFlags & PREFIX_UNALIGNED) != 0)
                {
                    BADCODE("Multiple unaligned. prefixes");
                }

                prefixFlags |= PREFIX_UNALIGNED;
                impValidateMemoryAccessOpcode(codeAddr, codeEndp, false);

            PREFIX:
                opcode     = (OPCODE)getU1LittleEndian(codeAddr);
                opcodeOffs = (IL_OFFSET)(codeAddr - info.compCode);
                codeAddr += sizeof(__int8);
                goto DECODE_OPCODE;

            case CEE_VOLATILE:
                if ((prefixFlags & PREFIX_VOLATILE) != 0)
                {
                    BADCODE("Multiple volatile. prefixes");
                }

                prefixFlags |= PREFIX_VOLATILE;

                impValidateMemoryAccessOpcode(codeAddr, codeEndp, true);

                assert(sz == 0);
                goto PREFIX;

            case CEE_LDFTN:
            {
                // Need to do a lookup here so that we perform an access check
                // and do a NOWAY if protections are violated
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Method);
                JITDUMP(" %08X", resolvedToken.token);

                eeGetCallInfo(&resolvedToken, nullptr /* constraint typeRef*/,
                              combine(CORINFO_CALLINFO_SECURITYCHECKS, CORINFO_CALLINFO_LDFTN), &callInfo);

                // This check really only applies to intrinsic Array.Address methods
                if (callInfo.sig.callConv & CORINFO_CALLCONV_PARAMTYPE)
                {
                    NO_WAY("Currently do not support LDFTN of Parameterized functions");
                }

                // Do this before DO_LDFTN since CEE_LDVIRTFN does it on its own.
                impHandleAccessAllowed(callInfo.accessAllowed, &callInfo.callsiteCalloutHelper);

            DO_LDFTN:
                op1 = impMethodPointer(&resolvedToken, &callInfo);

                if (compDonotInline())
                {
                    return;
                }

                // Call info may have more precise information about the function than
                // the resolved token.
                CORINFO_RESOLVED_TOKEN* heapToken = impAllocateToken(resolvedToken);
                assert(callInfo.hMethod != nullptr);
                heapToken->hMethod = callInfo.hMethod;
                impPushOnStack(op1, typeInfo(heapToken));

                break;
            }

            case CEE_LDVIRTFTN:
            {
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Method);
                JITDUMP(" %08X", resolvedToken.token);

                eeGetCallInfo(&resolvedToken, nullptr /* constraint typeRef */,
                              combine(combine(CORINFO_CALLINFO_SECURITYCHECKS, CORINFO_CALLINFO_LDFTN),
                                      CORINFO_CALLINFO_CALLVIRT),
                              &callInfo);

                // This check really only applies to intrinsic Array.Address methods
                if (callInfo.sig.callConv & CORINFO_CALLCONV_PARAMTYPE)
                {
                    NO_WAY("Currently do not support LDFTN of Parameterized functions");
                }

                mflags = callInfo.methodFlags;

                impHandleAccessAllowed(callInfo.accessAllowed, &callInfo.callsiteCalloutHelper);

                if (compIsForInlining())
                {
                    if (mflags & (CORINFO_FLG_FINAL | CORINFO_FLG_STATIC) || !(mflags & CORINFO_FLG_VIRTUAL))
                    {
                        compInlineResult->NoteFatal(InlineObservation::CALLSITE_LDVIRTFN_ON_NON_VIRTUAL);
                        return;
                    }
                }

                CORINFO_SIG_INFO& ftnSig = callInfo.sig;

                op1 = impPopStack().val;
                assertImp(op1->gtType == TYP_REF);

                if (opts.IsReadyToRun())
                {
                    if (callInfo.kind != CORINFO_VIRTUALCALL_LDVIRTFTN)
                    {
                        if (op1->gtFlags & GTF_SIDE_EFFECT)
                        {
                            op1 = gtUnusedValNode(op1);
                            impAppendTree(op1, (unsigned)CHECK_SPILL_ALL, impCurStmtOffs);
                        }
                        goto DO_LDFTN;
                    }
                }
                else if (mflags & (CORINFO_FLG_FINAL | CORINFO_FLG_STATIC) || !(mflags & CORINFO_FLG_VIRTUAL))
                {
                    if (op1->gtFlags & GTF_SIDE_EFFECT)
                    {
                        op1 = gtUnusedValNode(op1);
                        impAppendTree(op1, (unsigned)CHECK_SPILL_ALL, impCurStmtOffs);
                    }
                    goto DO_LDFTN;
                }

                GenTree* fptr = impImportLdvirtftn(op1, &resolvedToken, &callInfo);
                if (compDonotInline())
                {
                    return;
                }

                CORINFO_RESOLVED_TOKEN* heapToken = impAllocateToken(resolvedToken);

                assert(heapToken->tokenType == CORINFO_TOKENKIND_Method);
                assert(callInfo.hMethod != nullptr);

                heapToken->tokenType = CORINFO_TOKENKIND_Ldvirtftn;
                heapToken->hMethod   = callInfo.hMethod;
                impPushOnStack(fptr, typeInfo(heapToken));

                break;
            }

            case CEE_CONSTRAINED:
                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &constrainedResolvedToken, CORINFO_TOKENKIND_Constrained);
                codeAddr += sizeof(unsigned); // prefix instructions must increment codeAddr manually
                JITDUMP(" (%08X) ", constrainedResolvedToken.token);

                if ((prefixFlags & PREFIX_CONSTRAINED) != 0)
                {
                    BADCODE("Multiple constrained. prefixes");
                }

                prefixFlags |= PREFIX_CONSTRAINED;

                {
                    OPCODE actualOpcode = impGetNonPrefixOpcode(codeAddr, codeEndp);
                    if (actualOpcode != CEE_CALLVIRT)
                    {
                        BADCODE("constrained. has to be followed by callvirt");
                    }
                }

                goto PREFIX;

            case CEE_READONLY:
                JITDUMP(" readonly.");

                if ((prefixFlags & PREFIX_READONLY) != 0)
                {
                    BADCODE("Multiple readonly. prefixes");
                }

                prefixFlags |= PREFIX_READONLY;

                {
                    OPCODE actualOpcode = impGetNonPrefixOpcode(codeAddr, codeEndp);
                    if (actualOpcode != CEE_LDELEMA && !impOpcodeIsCallOpcode(actualOpcode))
                    {
                        BADCODE("readonly. has to be followed by ldelema or call");
                    }
                }

                assert(sz == 0);
                goto PREFIX;

            case CEE_TAILCALL:
                JITDUMP(" tail.");

                if ((prefixFlags & PREFIX_TAILCALL_EXPLICIT) != 0)
                {
                    BADCODE("Multiple tailcall. prefixes");
                }

                prefixFlags |= PREFIX_TAILCALL_EXPLICIT;

                {
                    OPCODE actualOpcode = impGetNonPrefixOpcode(codeAddr, codeEndp);
                    if (!impOpcodeIsCallOpcode(actualOpcode))
                    {
                        BADCODE("tailcall. has to be followed by call, callvirt or calli");
                    }
                }
                assert(sz == 0);
                goto PREFIX;

            case CEE_NEWOBJ:

                /* Since we will implicitly insert newObjThisPtr at the start of the
                   argument list, spill any GTF_ORDER_SIDEEFF */
                impSpillSpecialSideEff();

                /* NEWOBJ does not respond to TAIL */
                prefixFlags &= ~PREFIX_TAILCALL_EXPLICIT;

                /* NEWOBJ does not respond to CONSTRAINED */
                prefixFlags &= ~PREFIX_CONSTRAINED;

                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_NewObj);

                eeGetCallInfo(&resolvedToken, nullptr /* constraint typeRef*/,
                              combine(CORINFO_CALLINFO_SECURITYCHECKS, CORINFO_CALLINFO_ALLOWINSTPARAM), &callInfo);

                if (compIsForInlining())
                {
                    if (impInlineInfo->inlineCandidateInfo->dwRestrictions & INLINE_RESPECT_BOUNDARY)
                    {
                        // Check to see if this call violates the boundary.
                        compInlineResult->NoteFatal(InlineObservation::CALLSITE_CROSS_BOUNDARY_SECURITY);
                        return;
                    }
                }

                mflags = callInfo.methodFlags;

                if ((mflags & (CORINFO_FLG_STATIC | CORINFO_FLG_ABSTRACT)) != 0)
                {
                    BADCODE("newobj on static or abstract method");
                }

                // Insert the security callout before any actual code is generated
                impHandleAccessAllowed(callInfo.accessAllowed, &callInfo.callsiteCalloutHelper);

                // There are three different cases for new
                // Object size is variable (depends on arguments)
                //      1) Object is an array (arrays treated specially by the EE)
                //      2) Object is some other variable sized object (e.g. String)
                //      3) Class Size can be determined beforehand (normal case)
                // In the first case, we need to call a NEWOBJ helper (multinewarray)
                // in the second case we call the constructor with a '0' this pointer
                // In the third case we alloc the memory, then call the constuctor

                clsFlags = callInfo.classFlags;
                if (clsFlags & CORINFO_FLG_ARRAY)
                {
                    // Arrays need to call the NEWOBJ helper.
                    assertImp(clsFlags & CORINFO_FLG_VAROBJSIZE);

                    impImportNewObjArray(&resolvedToken, &callInfo);
                    if (compDonotInline())
                    {
                        return;
                    }

                    callTyp = TYP_REF;
                    break;
                }
                // At present this can only be String
                else if (clsFlags & CORINFO_FLG_VAROBJSIZE)
                {
                    if (IsTargetAbi(CORINFO_CORERT_ABI))
                    {
                        // The dummy argument does not exist in CoreRT
                        newObjThisPtr = nullptr;
                    }
                    else
                    {
                        // This is the case for variable-sized objects that are not
                        // arrays.  In this case, call the constructor with a null 'this'
                        // pointer
                        newObjThisPtr = gtNewIconNode(0, TYP_REF);
                    }

                    /* Remember that this basic block contains 'new' of an object */
                    block->bbFlags |= BBF_HAS_NEWOBJ;
                    optMethodFlags |= OMF_HAS_NEWOBJ;
                }
                else
                {
                    // This is the normal case where the size of the object is
                    // fixed.  Allocate the memory and call the constructor.

                    // Note: We cannot add a peep to avoid use of temp here
                    // becase we don't have enough interference info to detect when
                    // sources and destination interfere, example: s = new S(ref);

                    // TODO: We find the correct place to introduce a general
                    // reverse copy prop for struct return values from newobj or
                    // any function returning structs.

                    /* get a temporary for the new object */
                    lclNum = lvaGrabTemp(true DEBUGARG("NewObj constructor temp"));
                    if (compDonotInline())
                    {
                        // Fail fast if lvaGrabTemp fails with CALLSITE_TOO_MANY_LOCALS.
                        assert(compInlineResult->GetObservation() == InlineObservation::CALLSITE_TOO_MANY_LOCALS);
                        return;
                    }

                    // In the value class case we only need clsHnd for size calcs.
                    //
                    // The lookup of the code pointer will be handled by CALL in this case
                    if (clsFlags & CORINFO_FLG_VALUECLASS)
                    {
                        if (compIsForInlining())
                        {
                            // If value class has GC fields, inform the inliner. It may choose to
                            // bail out on the inline.
                            DWORD typeFlags = info.compCompHnd->getClassAttribs(resolvedToken.hClass);
                            if ((typeFlags & CORINFO_FLG_CONTAINS_GC_PTR) != 0)
                            {
                                compInlineResult->Note(InlineObservation::CALLEE_HAS_GC_STRUCT);
                                if (compInlineResult->IsFailure())
                                {
                                    return;
                                }

                                // Do further notification in the case where the call site is rare;
                                // some policies do not track the relative hotness of call sites for
                                // "always" inline cases.
                                if (impInlineInfo->iciBlock->isRunRarely())
                                {
                                    compInlineResult->Note(InlineObservation::CALLSITE_RARE_GC_STRUCT);
                                    if (compInlineResult->IsFailure())
                                    {
                                        return;
                                    }
                                }
                            }
                        }

                        CorInfoType jitTyp = info.compCompHnd->asCorInfoType(resolvedToken.hClass);

                        if (impIsPrimitive(jitTyp))
                        {
                            lvaGetDesc(lclNum)->SetType(JITtype2varType(jitTyp));
                        }
                        else
                        {
                            // The local variable itself is the allocated space.
                            // Here we need unsafe value cls check, since the address of struct is taken for further use
                            // and potentially exploitable.
                            lvaSetStruct(lclNum, resolvedToken.hClass, /* checkUnsafeBuffer */ true);
                        }

                        bool bbInALoop  = impBlockIsInALoop(block);
                        bool bbIsReturn = (block->bbJumpKind == BBJ_RETURN) &&
                                          (!compIsForInlining() || (impInlineInfo->iciBlock->bbJumpKind == BBJ_RETURN));
                        LclVarDsc* const lcl = lvaGetDesc(lclNum);
                        if (fgVarNeedsExplicitZeroInit(lclNum, bbInALoop, bbIsReturn))
                        {
                            GenTree* init = gtNewAssignNode(gtNewLclvNode(lclNum, lcl->GetType()), gtNewIconNode(0));
                            impAppendTree(init, CHECK_SPILL_NONE, impCurStmtOffs);
                        }
                        else
                        {
                            JITDUMP("\nSuppressing zero-init for V%02u -- expect to zero in prolog\n", lclNum);
                            lcl->lvSuppressedZeroInit = 1;
                            compSuppressedZeroInit    = true;
                        }

                        newObjThisPtr = gtNewAddrNode(gtNewLclvNode(lclNum, lcl->GetType()));
                    }
                    else
                    {
                        op1 = gtNewAllocObjNode(&resolvedToken, /* useParent */ true);
                        if (op1 == nullptr)
                        {
                            return;
                        }

                        // Remember that this basic block contains 'new' of an object
                        block->bbFlags |= BBF_HAS_NEWOBJ;
                        optMethodFlags |= OMF_HAS_NEWOBJ;

                        LclVarDsc* lcl = lvaGetDesc(lclNum);
                        lcl->SetType(TYP_REF);
                        assert(lcl->lvSingleDef == 0);
                        lcl->lvSingleDef = 1;
                        JITDUMP("Marked V%02u as a single def local\n", lclNum);
                        lvaSetClass(lclNum, resolvedToken.hClass, /* isExact */ true);

                        // Append the assignment to the temp/local. Dont need to spill
                        // at all as we are just calling an EE-Jit helper which can only
                        // cause an (async) OutOfMemoryException.

                        // We assign the newly allocated object (by a GT_ALLOCOBJ node)
                        // to a temp. Note that the pattern "temp = allocObj" is required
                        // by ObjectAllocator phase to be able to determine GT_ALLOCOBJ nodes
                        // without exhaustive walk over all expressions.

                        GenTree* asg = gtNewAssignNode(gtNewLclvNode(lclNum, TYP_REF), op1);
                        impAppendTree(asg, CHECK_SPILL_NONE, impCurStmtOffs);
                        newObjThisPtr = gtNewLclvNode(lclNum, TYP_REF);
                    }
                }
                goto CALL;

            case CEE_CALLI:

                /* CALLI does not respond to CONSTRAINED */
                prefixFlags &= ~PREFIX_CONSTRAINED;

                if (compIsForInlining())
                {
                    // CALLI doesn't have a method handle, so assume the worst.
                    if (impInlineInfo->inlineCandidateInfo->dwRestrictions & INLINE_RESPECT_BOUNDARY)
                    {
                        compInlineResult->NoteFatal(InlineObservation::CALLSITE_CROSS_BOUNDARY_CALLI);
                        return;
                    }
                }

                FALLTHROUGH;

            case CEE_CALLVIRT:
            case CEE_CALL:

                // We can't call getCallInfo on the token from a CALLI, but we need it in
                // many other places.  We unfortunately embed that knowledge here.
                if (opcode != CEE_CALLI)
                {
                    impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Method);

                    eeGetCallInfo(&resolvedToken,
                                  (prefixFlags & PREFIX_CONSTRAINED) ? &constrainedResolvedToken : nullptr,
                                  // this is how impImportCall invokes getCallInfo

                                  combine(combine(CORINFO_CALLINFO_ALLOWINSTPARAM, CORINFO_CALLINFO_SECURITYCHECKS),
                                          (opcode == CEE_CALLVIRT) ? CORINFO_CALLINFO_CALLVIRT : CORINFO_CALLINFO_NONE),
                                  &callInfo);
                }
                else
                {
                    // Suppress uninitialized use warning.
                    memset(&resolvedToken, 0, sizeof(resolvedToken));
                    memset(&callInfo, 0, sizeof(callInfo));

                    resolvedToken.token        = getU4LittleEndian(codeAddr);
                    resolvedToken.tokenContext = impTokenLookupContextHandle;
                    resolvedToken.tokenScope   = info.compScopeHnd;
                }

            CALL: // memberRef should be set.
                // newObjThisPtr should be set for CEE_NEWOBJ

                JITDUMP(" %08X", resolvedToken.token);
                constraintCall = (prefixFlags & PREFIX_CONSTRAINED) != 0;

                bool newBBcreatedForTailcallStress;
                bool passedStressModeValidation;

                newBBcreatedForTailcallStress = false;
                passedStressModeValidation    = true;

                if (compIsForInlining())
                {
                    if (compDonotInline())
                    {
                        return;
                    }
                    // We rule out inlinees with explicit tail calls in fgMakeBasicBlocks.
                    assert((prefixFlags & PREFIX_TAILCALL_EXPLICIT) == 0);
                }
                else
                {
#ifdef DEBUG
                    if (compTailCallStress())
                    {
                        // Have we created a new BB after the "call" instruction in fgMakeBasicBlocks()?
                        // Tail call stress only recognizes call+ret patterns and forces them to be
                        // explicit tail prefixed calls.  Also fgMakeBasicBlocks() under tail call stress
                        // doesn't import 'ret' opcode following the call into the basic block containing
                        // the call instead imports it to a new basic block.  Note that fgMakeBasicBlocks()
                        // is already checking that there is an opcode following call and hence it is
                        // safe here to read next opcode without bounds check.
                        newBBcreatedForTailcallStress =
                            impOpcodeIsCallOpcode(opcode) && // Current opcode is a CALL, (not a CEE_NEWOBJ). So, don't
                                                             // make it jump to RET.
                            (OPCODE)getU1LittleEndian(codeAddr + sz) == CEE_RET; // Next opcode is a CEE_RET

                        bool hasTailPrefix = (prefixFlags & PREFIX_TAILCALL_EXPLICIT);
                        if (newBBcreatedForTailcallStress && !hasTailPrefix)
                        {
                            // Do a more detailed evaluation of legality
                            const bool passedConstraintCheck =
                                verCheckTailCallConstraint(opcode, &resolvedToken,
                                                           constraintCall ? &constrainedResolvedToken : nullptr);

                            if (passedConstraintCheck)
                            {
                                // Now check with the runtime
                                CORINFO_METHOD_HANDLE declaredCalleeHnd = callInfo.hMethod;
                                bool                  isVirtual         = (callInfo.kind == CORINFO_VIRTUALCALL_STUB) ||
                                                 (callInfo.kind == CORINFO_VIRTUALCALL_VTABLE);
                                CORINFO_METHOD_HANDLE exactCalleeHnd = isVirtual ? nullptr : declaredCalleeHnd;
                                if (info.compCompHnd->canTailCall(info.compMethodHnd, declaredCalleeHnd, exactCalleeHnd,
                                                                  hasTailPrefix)) // Is it legal to do tailcall?
                                {
                                    // Stress the tailcall.
                                    JITDUMP(" (Tailcall stress: prefixFlags |= PREFIX_TAILCALL_EXPLICIT)");
                                    prefixFlags |= PREFIX_TAILCALL_EXPLICIT;
                                    prefixFlags |= PREFIX_TAILCALL_STRESS;
                                }
                                else
                                {
                                    // Runtime disallows this tail call
                                    JITDUMP(" (Tailcall stress: runtime preventing tailcall)");
                                    passedStressModeValidation = false;
                                }
                            }
                            else
                            {
                                // Constraints disallow this tail call
                                JITDUMP(" (Tailcall stress: constraint check failed)");
                                passedStressModeValidation = false;
                            }
                        }
                    }
#endif // DEBUG
                }

                // This is split up to avoid goto flow warnings.
                bool isRecursive;
                isRecursive = !compIsForInlining() && (callInfo.hMethod == info.compMethodHnd);

                // If we've already disqualified this call as a tail call under tail call stress,
                // don't consider it for implicit tail calling either.
                //
                // When not running under tail call stress, we may mark this call as an implicit
                // tail call candidate. We'll do an "equivalent" validation during impImportCall.
                //
                // Note that when running under tail call stress, a call marked as explicit
                // tail prefixed will not be considered for implicit tail calling.
                if (passedStressModeValidation &&
                    impIsImplicitTailCallCandidate(opcode, codeAddr + sz, codeEndp, prefixFlags, isRecursive))
                {
                    if (compIsForInlining())
                    {
#if FEATURE_TAILCALL_OPT_SHARED_RETURN
                        // Are we inlining at an implicit tail call site? If so the we can flag
                        // implicit tail call sites in the inline body. These call sites
                        // often end up in non BBJ_RETURN blocks, so only flag them when
                        // we're able to handle shared returns.
                        if (impInlineInfo->iciCall->IsImplicitTailCall())
                        {
                            JITDUMP(" (Inline Implicit Tail call: prefixFlags |= PREFIX_TAILCALL_IMPLICIT)");
                            prefixFlags |= PREFIX_TAILCALL_IMPLICIT;
                        }
#endif // FEATURE_TAILCALL_OPT_SHARED_RETURN
                    }
                    else
                    {
                        JITDUMP(" (Implicit Tail call: prefixFlags |= PREFIX_TAILCALL_IMPLICIT)");
                        prefixFlags |= PREFIX_TAILCALL_IMPLICIT;
                    }
                }

                // Treat this call as tail call for verification only if "tail" prefixed (i.e. explicit tail call).
                explicitTailCall = (prefixFlags & PREFIX_TAILCALL_EXPLICIT) != 0;
                readonlyCall     = (prefixFlags & PREFIX_READONLY) != 0;

                if (opcode != CEE_CALLI && opcode != CEE_NEWOBJ)
                {
                    // All calls and delegates need a security callout.
                    // For delegates, this is the call to the delegate constructor, not the access check on the
                    // LD(virt)FTN.
                    impHandleAccessAllowed(callInfo.accessAllowed, &callInfo.callsiteCalloutHelper);
                }

                callTyp = impImportCall(opcode, &resolvedToken, constraintCall ? &constrainedResolvedToken : nullptr,
                                        newObjThisPtr, prefixFlags, &callInfo, opcodeOffs);

                if (compDonotInline())
                {
                    // We do not check fails after lvaGrabTemp. It is covered with CoreCLR_13272 issue.
                    assert((callTyp == TYP_UNDEF) ||
                           (compInlineResult->GetObservation() == InlineObservation::CALLSITE_TOO_MANY_LOCALS));

                    return;
                }

                if (explicitTailCall || newBBcreatedForTailcallStress)
                {
                    // If newBBcreatedForTailcallStress is true, we have created a new BB after the "call"
                    // instruction in fgMakeBasicBlocks(). So we need to jump to RET regardless.

                    impReturnInstruction(prefixFlags, &opcode);
                }

                break;

            case CEE_LDFLD:
            case CEE_LDSFLD:
            case CEE_LDFLDA:
            case CEE_LDSFLDA:
            {
                const bool isLoadAddress = (opcode == CEE_LDFLDA || opcode == CEE_LDSFLDA);
                const bool isLoadStatic  = (opcode == CEE_LDSFLD || opcode == CEE_LDSFLDA);

                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Field);
                JITDUMP(" %08X", resolvedToken.token);

                CORINFO_ACCESS_FLAGS accessFlags = isLoadAddress ? CORINFO_ACCESS_ADDRESS : CORINFO_ACCESS_GET;

                GenTree* obj = nullptr;
                typeInfo tiObj;

                if ((opcode == CEE_LDFLD) || (opcode == CEE_LDFLDA))
                {
                    tiObj = impStackTop().seTypeInfo;
                    obj   = impPopStack().val;

                    if (impIsThis(obj))
                    {
                        accessFlags = static_cast<CORINFO_ACCESS_FLAGS>(accessFlags | CORINFO_ACCESS_THIS);
                    }
                }

                CORINFO_FIELD_INFO fieldInfo;
                eeGetFieldInfo(&resolvedToken, accessFlags, &fieldInfo);

                lclTyp = JITtype2varType(fieldInfo.fieldType);

                if (compIsForInlining())
                {
                    switch (fieldInfo.fieldAccessor)
                    {
                        case CORINFO_FIELD_INSTANCE_HELPER:
                        case CORINFO_FIELD_INSTANCE_ADDR_HELPER:
                        case CORINFO_FIELD_STATIC_ADDR_HELPER:
                        case CORINFO_FIELD_STATIC_TLS:
                            compInlineResult->NoteFatal(InlineObservation::CALLEE_LDFLD_NEEDS_HELPER);
                            return;

                        case CORINFO_FIELD_STATIC_GENERICS_STATIC_HELPER:
                        case CORINFO_FIELD_STATIC_READYTORUN_HELPER:
                            /* We may be able to inline the field accessors in specific instantiations of generic
                             * methods */
                            compInlineResult->NoteFatal(InlineObservation::CALLSITE_LDFLD_NEEDS_HELPER);
                            return;

                        default:
                            break;
                    }

                    if (!isLoadAddress && ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC) != 0) &&
                        (lclTyp == TYP_STRUCT) && (fieldInfo.structType != NO_CLASS_HANDLE))
                    {
                        if ((info.compCompHnd->getTypeForPrimitiveValueClass(fieldInfo.structType) ==
                             CORINFO_TYPE_UNDEF) &&
                            ((info.compFlags & CORINFO_FLG_FORCEINLINE) == 0))
                        {
                            // Loading a static valuetype field usually will cause a JitHelper to be called
                            // for the static base. This will bloat the code.
                            compInlineResult->Note(InlineObservation::CALLEE_LDFLD_STATIC_VALUECLASS);

                            if (compInlineResult->IsFailure())
                            {
                                return;
                            }
                        }
                    }
                }

                impHandleAccessAllowed(fieldInfo.accessAllowed, &fieldInfo.accessCalloutHelper);

                // Raise InvalidProgramException if static load accesses non-static field
                if (isLoadStatic && ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC) == 0))
                {
                    BADCODE("static access on an instance field");
                }

                // We are using ldfld/a on a static field. We allow it, but need to get side-effect from obj.
                if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC) && obj != nullptr)
                {
                    if (obj->gtFlags & GTF_SIDE_EFFECT)
                    {
                        obj = gtUnusedValNode(obj);
                        impAppendTree(obj, (unsigned)CHECK_SPILL_ALL, impCurStmtOffs);
                    }
                    obj = nullptr;
                }

                switch (fieldInfo.fieldAccessor)
                {
                    case CORINFO_FIELD_INSTANCE:
#ifdef FEATURE_READYTORUN_COMPILER
                    case CORINFO_FIELD_INSTANCE_WITH_BASE:
#endif
                    {
                        if (!varTypeGCtype(obj->GetType()) && tiObj.IsType(TI_STRUCT))
                        {
                            // If the object is a struct, what we really want is
                            // for the field to operate on the address of the struct.

                            assert((opcode == CEE_LDFLD) && (tiObj.GetClassHandle() != NO_CLASS_HANDLE));

                            obj = impGetStructAddr(obj, tiObj.GetClassHandle(), CHECK_SPILL_ALL, true);
                        }

                        obj = impCheckForNullPointer(obj);

                        op1 = gtNewFieldRef(lclTyp, resolvedToken.hField, obj, fieldInfo.offset);

#ifdef FEATURE_READYTORUN_COMPILER
                        if (fieldInfo.fieldAccessor == CORINFO_FIELD_INSTANCE_WITH_BASE)
                        {
                            noway_assert(fieldInfo.fieldLookup.accessType == IAT_PVALUE);
                            op1->AsField()->SetR2RFieldLookupAddr(fieldInfo.fieldLookup.addr);
                        }
#endif

                        if (fgAddrCouldBeNull(obj))
                        {
                            op1->gtFlags |= GTF_EXCEPT;
                        }

                        if (StructHasOverlappingFields(info.compCompHnd->getClassAttribs(resolvedToken.hClass)))
                        {
                            op1->AsField()->gtFldMayOverlap = true;
                        }

                        // wrap it in a address of operator if necessary
                        if (isLoadAddress)
                        {
                            op1 = gtNewAddrNode(op1, varTypeIsGC(obj->GetType()) ? TYP_BYREF : TYP_I_IMPL);
                        }
                        else
                        {
                            if (compIsForInlining() &&
                                impInlineIsGuaranteedThisDerefBeforeAnySideEffects(nullptr, nullptr, obj))
                            {
                                impInlineInfo->thisDereferencedFirst = true;
                            }
                        }
                    }
                    break;

                    case CORINFO_FIELD_INSTANCE_ADDR_HELPER:
                        op1 = impImportFieldAccess(obj, &resolvedToken, fieldInfo, accessFlags, lclTyp,
                                                   fieldInfo.structType);
                        break;

                    case CORINFO_FIELD_STATIC_TLS:
                        op1 = impImportTlsFieldAccess(&resolvedToken, fieldInfo, accessFlags, lclTyp);
                        break;

                    case CORINFO_FIELD_STATIC_ADDRESS:
                    case CORINFO_FIELD_STATIC_RVA_ADDRESS:
                    case CORINFO_FIELD_STATIC_SHARED_STATIC_HELPER:
                    case CORINFO_FIELD_STATIC_GENERICS_STATIC_HELPER:
                    case CORINFO_FIELD_STATIC_READYTORUN_HELPER:
                        op1 = impImportStaticFieldAccess(&resolvedToken, fieldInfo, accessFlags, lclTyp);
                        if (((accessFlags & CORINFO_ACCESS_GET) != 0) && op1->OperIsConst())
                        {
                            goto FIELD_DONE;
                        }
                        break;

                    case CORINFO_FIELD_INTRINSIC_ZERO:
                        assert(accessFlags & CORINFO_ACCESS_GET);
                        op1 = gtNewIconNode(0, varActualType(lclTyp));
                        goto FIELD_DONE;
                        break;

                    case CORINFO_FIELD_INTRINSIC_EMPTY_STRING:
                    {
                        assert(accessFlags & CORINFO_ACCESS_GET);

                        void*          pValue;
                        InfoAccessType iat = info.compCompHnd->emptyStringLiteral(&pValue);
                        op1                = gtNewStringLiteralNode(iat, pValue);
                        goto FIELD_DONE;
                    }

                    case CORINFO_FIELD_INTRINSIC_ISLITTLEENDIAN:
                        assert(accessFlags & CORINFO_ACCESS_GET);
#if BIGENDIAN
                        op1 = gtNewIconNode(0, varActualType(lclTyp));
#else
                        op1 = gtNewIconNode(1, varActualType(lclTyp));
#endif
                        goto FIELD_DONE;

                    default:
                        assert(!"Unexpected fieldAccessor");
                }

                if (!isLoadAddress)
                {
                    if ((prefixFlags & PREFIX_VOLATILE) != 0)
                    {
                        op1->gtFlags |= GTF_DONT_CSE;      // Can't CSE a volatile
                        op1->gtFlags |= GTF_ORDER_SIDEEFF; // Prevent this from being reordered

                        assert(op1->OperIs(GT_FIELD, GT_IND, GT_OBJ, GT_CLS_VAR));
                        op1->gtFlags |= GTF_IND_VOLATILE;
                    }

                    if (((prefixFlags & PREFIX_UNALIGNED) != 0) && !varTypeIsByte(lclTyp) && (obj == nullptr))
                    {
                        assert(op1->OperIs(GT_FIELD, GT_IND, GT_OBJ));
                        op1->gtFlags |= GTF_IND_UNALIGNED;
                    }
                }

                /* Check if the class needs explicit initialization */

                if (fieldInfo.fieldFlags & CORINFO_FLG_FIELD_INITCLASS)
                {
                    GenTree* helperNode = impInitClass(&resolvedToken);
                    if (compDonotInline())
                    {
                        return;
                    }
                    if (helperNode != nullptr)
                    {
                        // TDOO-MIKE-Cleanup: This appears to be the only case where the importer creates
                        // a struct typed COMMA. Subsequently this is transformed into OBJ(COMMA(...))
                        // (see impCanonicalizeStructCallArg and impAssignStructPtr).
                        // We could do that here but let's keep it as is for now for testing purposes.

                        op1 = gtNewOperNode(GT_COMMA, op1->TypeGet(), helperNode, op1);
                    }
                }

                if (!isLoadAddress && (fieldInfo.structType != NO_CLASS_HANDLE))
                {
                    impPushOnStack(op1, impMakeTypeInfo(fieldInfo.fieldType, fieldInfo.structType));
                    break;
                }

            FIELD_DONE:
                impPushOnStack(op1, typeInfo());
            }
            break;

            case CEE_STFLD:
            case CEE_STSFLD:
            {
                BOOL isStoreStatic = (opcode == CEE_STSFLD);

                CORINFO_CLASS_HANDLE fieldClsHnd; // class of the field (if it's a ref type)

                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Field);
                JITDUMP(" %08X", resolvedToken.token);

                CORINFO_ACCESS_FLAGS accessFlags = CORINFO_ACCESS_SET;
                GenTree*             obj         = nullptr;
                typeInfo*            tiObj       = nullptr;
                typeInfo             tiVal;

                /* Pull the value from the stack */
                StackEntry se = impPopStack();
                op2           = se.val;
                tiVal         = se.seTypeInfo;
                clsHnd        = tiVal.GetClassHandle();

                if (opcode == CEE_STFLD)
                {
                    tiObj = &impStackTop().seTypeInfo;
                    obj   = impPopStack().val;

                    if (impIsThis(obj))
                    {
                        accessFlags = static_cast<CORINFO_ACCESS_FLAGS>(accessFlags | CORINFO_ACCESS_THIS);
                    }
                }

                CORINFO_FIELD_INFO fieldInfo;
                eeGetFieldInfo(&resolvedToken, accessFlags, &fieldInfo);

                // Figure out the type of the member.  We always call canAccessField, so you always need this
                // handle
                CorInfoType ciType = fieldInfo.fieldType;
                fieldClsHnd        = fieldInfo.structType;

                lclTyp = JITtype2varType(ciType);

                if (compIsForInlining())
                {
                    /* Is this a 'special' (COM) field? or a TLS ref static field?, field stored int GC heap? or
                     * per-inst static? */

                    switch (fieldInfo.fieldAccessor)
                    {
                        case CORINFO_FIELD_INSTANCE_HELPER:
                        case CORINFO_FIELD_INSTANCE_ADDR_HELPER:
                        case CORINFO_FIELD_STATIC_ADDR_HELPER:
                        case CORINFO_FIELD_STATIC_TLS:
                            compInlineResult->NoteFatal(InlineObservation::CALLEE_STFLD_NEEDS_HELPER);
                            return;

                        case CORINFO_FIELD_STATIC_GENERICS_STATIC_HELPER:
                        case CORINFO_FIELD_STATIC_READYTORUN_HELPER:
                            /* We may be able to inline the field accessors in specific instantiations of generic
                             * methods */
                            compInlineResult->NoteFatal(InlineObservation::CALLSITE_STFLD_NEEDS_HELPER);
                            return;

                        default:
                            break;
                    }
                }

                impHandleAccessAllowed(fieldInfo.accessAllowed, &fieldInfo.accessCalloutHelper);

                if (isStoreStatic && ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC) == 0))
                {
                    BADCODE("static access on an instance field");
                }

                // We are using stfld on a static field.
                // We allow it, but need to eval any side-effects for obj
                if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC) && obj != nullptr)
                {
                    if (obj->gtFlags & GTF_SIDE_EFFECT)
                    {
                        obj = gtUnusedValNode(obj);
                        impAppendTree(obj, (unsigned)CHECK_SPILL_ALL, impCurStmtOffs);
                    }
                    obj = nullptr;
                }

                switch (fieldInfo.fieldAccessor)
                {
                    case CORINFO_FIELD_INSTANCE:
#ifdef FEATURE_READYTORUN_COMPILER
                    case CORINFO_FIELD_INSTANCE_WITH_BASE:
#endif
                    {
                        obj = impCheckForNullPointer(obj);

                        op1 = gtNewFieldRef(lclTyp, resolvedToken.hField, obj, fieldInfo.offset);

                        if (StructHasOverlappingFields(info.compCompHnd->getClassAttribs(resolvedToken.hClass)))
                        {
                            op1->AsField()->gtFldMayOverlap = true;
                        }

#ifdef FEATURE_READYTORUN_COMPILER
                        if (fieldInfo.fieldAccessor == CORINFO_FIELD_INSTANCE_WITH_BASE)
                        {
                            noway_assert(fieldInfo.fieldLookup.accessType == IAT_PVALUE);
                            op1->AsField()->SetR2RFieldLookupAddr(fieldInfo.fieldLookup.addr);
                        }
#endif

                        if (fgAddrCouldBeNull(obj))
                        {
                            op1->gtFlags |= GTF_EXCEPT;
                        }

                        if (compIsForInlining() &&
                            impInlineIsGuaranteedThisDerefBeforeAnySideEffects(op2, nullptr, obj))
                        {
                            impInlineInfo->thisDereferencedFirst = true;
                        }
                    }
                    break;

                    case CORINFO_FIELD_INSTANCE_ADDR_HELPER:
                        op1 = impImportFieldAccess(obj, &resolvedToken, fieldInfo, accessFlags, lclTyp, clsHnd);
                        break;

                    case CORINFO_FIELD_STATIC_TLS:
                        op1 = impImportTlsFieldAccess(&resolvedToken, fieldInfo, accessFlags, lclTyp);
                        break;

                    case CORINFO_FIELD_STATIC_ADDRESS:
                    case CORINFO_FIELD_STATIC_RVA_ADDRESS:
                    case CORINFO_FIELD_STATIC_SHARED_STATIC_HELPER:
                    case CORINFO_FIELD_STATIC_GENERICS_STATIC_HELPER:
                    case CORINFO_FIELD_STATIC_READYTORUN_HELPER:
                        op1 = impImportStaticFieldAccess(&resolvedToken, fieldInfo, accessFlags, lclTyp);
                        break;

                    default:
                        assert(!"Unexpected fieldAccessor");
                }

                // Create the member assignment, unless we have a TYP_STRUCT.
                bool deferStructAssign = (lclTyp == TYP_STRUCT);

                if (!deferStructAssign)
                {
                    if (prefixFlags & PREFIX_VOLATILE)
                    {
                        assert(op1->OperIs(GT_FIELD, GT_IND, GT_CLS_VAR));
                        op1->gtFlags |= GTF_DONT_CSE;      // Can't CSE a volatile
                        op1->gtFlags |= GTF_ORDER_SIDEEFF; // Prevent this from being reordered
                        op1->gtFlags |= GTF_IND_VOLATILE;
                    }

                    if ((prefixFlags & PREFIX_UNALIGNED) && !varTypeIsByte(lclTyp) && (obj == nullptr))
                    {
                        assert(op1->OperIs(GT_FIELD, GT_IND));
                        op1->gtFlags |= GTF_IND_UNALIGNED;
                    }

                    /* V4.0 allows assignment of i4 constant values to i8 type vars when IL verifier is bypassed (full
                       trust apps). The reason this works is that JIT stores an i4 constant in Gentree union during
                       importation and reads from the union as if it were a long during code generation. Though this
                       can potentially read garbage, one can get lucky to have this working correctly.

                       This code pattern is generated by Dev10 MC++ compiler while storing to fields when compiled with
                       /O2 switch (default when compiling retail configs in Dev10) and a customer app has taken a
                       dependency on it. To be backward compatible, we will explicitly add an upward cast here so that
                       it works correctly always.

                       Note that this is limited to x86 alone as there is no back compat to be addressed for Arm JIT
                       for V4.0.
                    */
                    CLANG_FORMAT_COMMENT_ANCHOR;

#ifndef TARGET_64BIT
                    // In UWP6.0 and beyond (post-.NET Core 2.0), we decided to let this cast from int to long be
                    // generated for ARM as well as x86, so the following IR will be accepted:
                    // STMTx (IL 0x... ???)
                    //   *  ASG long
                    //   +--*  CLS_VAR   long
                    //   \--*  CNS_INT   int    2

                    if ((op1->TypeGet() != op2->TypeGet()) && op2->OperIsConst() && varTypeIsIntOrI(op2->TypeGet()) &&
                        varTypeIsLong(op1->TypeGet()))
                    {
                        op2 = gtNewCastNode(op1->TypeGet(), op2, false, op1->TypeGet());
                    }
#endif

#ifdef TARGET_64BIT
                    // Automatic upcast for a GT_CNS_INT into TYP_I_IMPL
                    if ((op2->OperGet() == GT_CNS_INT) && varTypeIsI(lclTyp) && !varTypeIsI(op2->gtType))
                    {
                        op2->gtType = TYP_I_IMPL;
                    }
                    else
                    {
                        // Allow a downcast of op2 from TYP_I_IMPL into a 32-bit Int for x86 JIT compatiblity
                        //
                        if (varTypeIsI(op2->gtType) && (genActualType(lclTyp) == TYP_INT))
                        {
                            op2 = gtNewCastNode(TYP_INT, op2, false, TYP_INT);
                        }
                        // Allow an upcast of op2 from a 32-bit Int into TYP_I_IMPL for x86 JIT compatiblity
                        //
                        if (varTypeIsI(lclTyp) && (genActualType(op2->gtType) == TYP_INT))
                        {
                            op2 = gtNewCastNode(TYP_I_IMPL, op2, false, TYP_I_IMPL);
                        }
                    }
#endif

                    // We can generate an assignment to a TYP_FLOAT from a TYP_DOUBLE
                    // We insert a cast to the dest 'op1' type
                    //
                    if ((op1->TypeGet() != op2->TypeGet()) && varTypeIsFloating(op1->gtType) &&
                        varTypeIsFloating(op2->gtType))
                    {
                        op2 = gtNewCastNode(op1->TypeGet(), op2, false, op1->TypeGet());
                    }

                    op1 = gtNewAssignNode(op1, op2);
                }

                /* Check if the class needs explicit initialization */

                if (fieldInfo.fieldFlags & CORINFO_FLG_FIELD_INITCLASS)
                {
                    GenTree* helperNode = impInitClass(&resolvedToken);
                    if (compDonotInline())
                    {
                        return;
                    }
                    if (helperNode != nullptr)
                    {
                        op1 = gtNewOperNode(GT_COMMA, op1->TypeGet(), helperNode, op1);
                    }
                }

                // stfld can interfere with value classes (consider the sequence
                // ldloc, ldloca, ..., stfld, stloc).  We will be conservative and
                // spill all value class references from the stack.

                if ((obj != nullptr) && obj->TypeIs(TYP_BYREF, TYP_I_IMPL))
                {
                    if (tiObj->IsType(TI_STRUCT))
                    {
                        impSpillEvalStack();
                    }
                    else
                    {
                        impSpillValueClasses();
                    }
                }

                /* Spill any refs to the same member from the stack */

                impSpillLclRefs((ssize_t)resolvedToken.hField);

                /* stsfld also interferes with indirect accesses (for aliased
                   statics) and calls. But don't need to spill other statics
                   as we have explicitly spilled this particular static field. */

                impSpillSideEffects(false, (unsigned)CHECK_SPILL_ALL DEBUGARG("spill side effects before STFLD"));

                if (deferStructAssign)
                {
                    op1 = impAssignStruct(op1, op2, clsHnd, (unsigned)CHECK_SPILL_ALL);
                }
            }
                goto APPEND;

            case CEE_NEWARR:
            {
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Newarr);
                JITDUMP(" %08X", resolvedToken.token);

                if (!opts.IsReadyToRun())
                {
                    // Need to restore array classes before creating array objects on the heap
                    op1 = impTokenToHandle(&resolvedToken, nullptr, TRUE /*mustRestoreHandle*/);
                    if (op1 == nullptr)
                    { // compDonotInline()
                        return;
                    }
                }

                accessAllowedResult =
                    info.compCompHnd->canAccessClass(&resolvedToken, info.compMethodHnd, &calloutHelper);
                impHandleAccessAllowed(accessAllowedResult, &calloutHelper);

                /* Form the arglist: array class handle, size */
                op2 = impPopStack().val;
                assertImp(genActualTypeIsIntOrI(op2->gtType));

#ifdef TARGET_64BIT
                // The array helper takes a native int for array length.
                // So if we have an int, explicitly extend it to be a native int.
                if (genActualType(op2->TypeGet()) != TYP_I_IMPL)
                {
                    if (op2->IsIntegralConst())
                    {
                        op2->gtType = TYP_I_IMPL;
                    }
                    else
                    {
                        bool isUnsigned = false;
                        op2             = gtNewCastNode(TYP_I_IMPL, op2, isUnsigned, TYP_I_IMPL);
                    }
                }
#endif // TARGET_64BIT

#ifdef FEATURE_READYTORUN_COMPILER
                if (opts.IsReadyToRun())
                {
                    op1 = impReadyToRunHelperToTree(&resolvedToken, CORINFO_HELP_READYTORUN_NEWARR_1, TYP_REF,
                                                    gtNewCallArgs(op2));
                    usingReadyToRunHelper = (op1 != nullptr);

                    if (!usingReadyToRunHelper)
                    {
                        // TODO: ReadyToRun: When generic dictionary lookups are necessary, replace the lookup call
                        // and the newarr call with a single call to a dynamic R2R cell that will:
                        //      1) Load the context
                        //      2) Perform the generic dictionary lookup and caching, and generate the appropriate stub
                        //      3) Allocate the new array
                        // Reason: performance (today, we'll always use the slow helper for the R2R generics case)

                        // Need to restore array classes before creating array objects on the heap
                        op1 = impTokenToHandle(&resolvedToken, nullptr, TRUE /*mustRestoreHandle*/);
                        if (op1 == nullptr)
                        { // compDonotInline()
                            return;
                        }
                    }
                }

                if (!usingReadyToRunHelper)
#endif
                {
                    GenTreeCall::Use* args = gtNewCallArgs(op1, op2);

                    /* Create a call to 'new' */

                    // Note that this only works for shared generic code because the same helper is used for all
                    // reference array types
                    op1 = gtNewHelperCallNode(info.compCompHnd->getNewArrHelper(resolvedToken.hClass), TYP_REF, args);
                }

                op1->AsCall()->compileTimeHelperArgumentHandle = (CORINFO_GENERIC_HANDLE)resolvedToken.hClass;

                /* Remember that this basic block contains 'new' of an sd array */

                block->bbFlags |= BBF_HAS_NEWARRAY;
                optMethodFlags |= OMF_HAS_NEWARRAY;

                impPushOnStack(op1, typeInfo(TI_REF, resolvedToken.hClass));

                callTyp = TYP_REF;
            }
            break;

            case CEE_LOCALLOC:
                // We don't allow locallocs inside handlers
                if (block->hasHndIndex())
                {
                    BADCODE("Localloc can't be inside handler");
                }

                // Get the size to allocate

                op2 = impPopStack().val;
                assertImp(genActualTypeIsIntOrI(op2->gtType));

                if (verCurrentState.esStackDepth != 0)
                {
                    BADCODE("Localloc can only be used when the stack is empty");
                }

                // If the localloc is not in a loop and its size is a small constant,
                // create a new local var of TYP_BLK and return its address.
                {
                    bool convertedToLocal = false;

                    // Need to aggressively fold here, as even fixed-size locallocs
                    // will have casts in the way.
                    op2 = gtFoldExpr(op2);

                    if (GenTreeIntCon* icon = op2->IsIntCon())
                    {
                        const ssize_t allocSize = icon->GetValue();

                        if (allocSize == 0)
                        {
                            // Result is nullptr
                            JITDUMP("Converting stackalloc of 0 bytes to push null unmanaged pointer\n");
                            op1 = gtNewIconNode(0, TYP_I_IMPL);

                            convertedToLocal = true;
                        }
                        else if ((allocSize > 0) && !impBlockIsInALoop(block))
                        {
                            ssize_t maxSize = DEFAULT_MAX_LOCALLOC_TO_LOCAL_SIZE;
                            INDEBUG(maxSize = JitConfig.JitStackAllocToLocalSize();)

                            if (allocSize <= maxSize)
                            {
                                const unsigned lclNum = lvaGrabTemp(false DEBUGARG("small stackalloc temp"));
                                JITDUMP("Converting stackalloc of %lld bytes to new local V%02u\n", allocSize, lclNum);

                                LclVarDsc* lcl = lvaGetDesc(lclNum);
                                lcl->SetBlockType(static_cast<unsigned>(allocSize));
                                lcl->lvIsUnsafeBuffer = true;

                                op1 = gtNewAddrNode(gtNewLclvNode(lclNum, TYP_BLK), TYP_I_IMPL);

                                if (!opts.compDbgEnC)
                                {
                                    // Ensure we have stack security for this method.
                                    // Reorder layout since the converted localloc is treated as an unsafe buffer.
                                    setNeedsGSSecurityCookie();
                                    compGSReorderStackLayout = true;
                                }

                                convertedToLocal = true;
                            }
                        }
                    }

                    if (!convertedToLocal)
                    {
                        // Bail out if inlining and the localloc was not converted.
                        //
                        // Note we might consider allowing the inline, if the call
                        // site is not in a loop.
                        if (compIsForInlining())
                        {
                            InlineObservation obs = op2->IsIntegralConst()
                                                        ? InlineObservation::CALLEE_LOCALLOC_TOO_LARGE
                                                        : InlineObservation::CALLSITE_LOCALLOC_SIZE_UNKNOWN;
                            compInlineResult->NoteFatal(obs);
                            return;
                        }

                        op1 = gtNewOperNode(GT_LCLHEAP, TYP_I_IMPL, op2);
                        // May throw a stack overflow exception. Obviously, we don't want locallocs to be CSE'd.
                        op1->gtFlags |= (GTF_EXCEPT | GTF_DONT_CSE);

                        // Ensure we have stack security for this method.
                        setNeedsGSSecurityCookie();

                        /* The FP register may not be back to the original value at the end
                           of the method, even if the frame size is 0, as localloc may
                           have modified it. So we will HAVE to reset it */
                        compLocallocUsed = true;
                    }
                    else
                    {
                        compLocallocOptimized = true;
                    }
                }

                impPushOnStack(op1, typeInfo());
                break;

            case CEE_ISINST:
            {
                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Casting);
                JITDUMP(" %08X", resolvedToken.token);

                if (!opts.IsReadyToRun())
                {
                    op2 = impTokenToHandle(&resolvedToken, nullptr, FALSE);
                    if (op2 == nullptr)
                    {
                        return;
                    }
                }

                accessAllowedResult =
                    info.compCompHnd->canAccessClass(&resolvedToken, info.compMethodHnd, &calloutHelper);
                impHandleAccessAllowed(accessAllowedResult, &calloutHelper);

                op1 = impPopStack().val;

                GenTree* optTree = impOptimizeCastClassOrIsInst(op1, &resolvedToken, false);

                if (optTree != nullptr)
                {
                    impPushOnStack(optTree, typeInfo());
                    break;
                }

#ifdef FEATURE_READYTORUN_COMPILER
                if (opts.IsReadyToRun())
                {
                    GenTreeCall* opLookup =
                        impReadyToRunHelperToTree(&resolvedToken, CORINFO_HELP_READYTORUN_ISINSTANCEOF, TYP_REF,
                                                  gtNewCallArgs(op1));
                    usingReadyToRunHelper = (opLookup != nullptr);
                    op1                   = (usingReadyToRunHelper ? opLookup : op1);

                    if (!usingReadyToRunHelper)
                    {
                        // TODO: ReadyToRun: When generic dictionary lookups are necessary, replace the lookup call
                        // and the isinstanceof_any call with a single call to a dynamic R2R cell that will:
                        //      1) Load the context
                        //      2) Perform the generic dictionary lookup and caching, and generate the appropriate
                        //      stub
                        //      3) Perform the 'is instance' check on the input object
                        // Reason: performance (today, we'll always use the slow helper for the R2R generics case)

                        op2 = impTokenToHandle(&resolvedToken, nullptr, FALSE);
                        if (op2 == nullptr)
                        {
                            return;
                        }
                    }
                }

                if (!usingReadyToRunHelper)
#endif
                {
                    op1 = impCastClassOrIsInstToTree(op1, op2, &resolvedToken, false);
                }
                if (compDonotInline())
                {
                    return;
                }

                impPushOnStack(op1, typeInfo());
                break;
            }

            case CEE_REFANYVAL:
                if (impStackTop().seTypeInfo.GetClassHandleForValueClass() != impGetRefAnyClass())
                {
                    BADCODE("typedref expected");
                }

                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);

                op2 = impTokenToHandle(&resolvedToken);
                if (op2 == nullptr)
                {
                    assert(compIsForInlining() && compDonotInline());
                    return;
                }

                op1 = impPopStack().val;

                if (op1->OperIs(GT_CALL, GT_RET_EXPR))
                {
                    ClassLayout* layout = typGetObjLayout(impGetRefAnyClass());
                    unsigned     tmpNum = lvaNewTemp(layout, true DEBUGARG("refanyval temp"));
                    GenTree*     asg = impAssignStruct(gtNewLclvNode(tmpNum, TYP_STRUCT), op1, layout, CHECK_SPILL_ALL);
                    impAppendTree(asg, CHECK_SPILL_ALL, impCurStmtOffs);
                    op1 = gtNewLclvNode(tmpNum, TYP_STRUCT);
                }

                {
                    GenTreeCall::Use* arg1 = gtNewCallArgs(op2);
                    GenTreeCall::Use* arg2 = gtNewCallArgs(op1);
                    arg2->SetSigTypeNum(typGetObjLayoutNum(impGetRefAnyClass()));
                    arg1->SetNext(arg2);

                    op1 = gtNewHelperCallNode(CORINFO_HELP_GETREFANY, TYP_BYREF, arg1);
                }

                impPushOnStack(op1, typeInfo());
                break;

            case CEE_REFANYTYPE:
                if (impStackTop().seTypeInfo.GetClassHandleForValueClass() != impGetRefAnyClass())
                {
                    BADCODE("typedref expected");
                }

                op1 = impPopStack().val;

                if (!op1->OperIs(GT_LCL_VAR, GT_MKREFANY))
                {
                    ClassLayout* layout = typGetObjLayout(impGetRefAnyClass());
                    unsigned     tmpNum = lvaNewTemp(layout, true DEBUGARG("refanytype temp"));
                    GenTree*     asg = impAssignStruct(gtNewLclvNode(tmpNum, TYP_STRUCT), op1, layout, CHECK_SPILL_ALL);
                    impAppendTree(asg, CHECK_SPILL_ALL, impCurStmtOffs);
                    op1 = gtNewLclvNode(tmpNum, TYP_STRUCT);
                }

                if (op1->OperIs(GT_LCL_VAR))
                {
                    op1 = gtNewLclFldNode(op1->AsLclVar()->GetLclNum(), TYP_BYREF,
                                          OFFSETOF__CORINFO_TypedReference__type);
                    op1->AsLclFld()->SetFieldSeq(GetFieldSeqStore()->CreateSingleton(GetRefanyTypeField()));
                }
                else
                {
                    assertImp(op1->gtOper == GT_MKREFANY);

                    // The pointer may have side-effects
                    if (op1->AsOp()->gtOp1->gtFlags & GTF_SIDE_EFFECT)
                    {
                        impAppendTree(op1->AsOp()->gtOp1, (unsigned)CHECK_SPILL_ALL, impCurStmtOffs);
#ifdef DEBUG
                        impNoteLastILoffs();
#endif
                    }

                    // We already have the class handle
                    op1 = op1->AsOp()->gtOp2;
                }

                // convert native TypeHandle to RuntimeTypeHandle
                op1 = gtNewHelperCallNode(CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE_MAYBENULL, TYP_STRUCT,
                                          gtNewCallArgs(op1));
                op1->AsCall()->SetRetLayout(typGetObjLayout(impGetTypeHandleClass()));
                op1->AsCall()->GetRetDesc()->InitializePrimitive(GetRuntimeHandleUnderlyingType());

                impPushOnStack(op1, typeInfo(TI_STRUCT, op1->AsCall()->GetRetLayout()->GetClassHandle()));
                break;

            case CEE_LDTOKEN:
                assertImp(sz == sizeof(unsigned));
                lastLoadToken = codeAddr;
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Ldtoken);
                tokenType = info.compCompHnd->getTokenTypeAsHandle(&resolvedToken);

                op1 = impTokenToHandle(&resolvedToken, nullptr, TRUE);
                if (op1 == nullptr)
                { // compDonotInline()
                    return;
                }

                assert(resolvedToken.hClass != nullptr);

                if (resolvedToken.hMethod != nullptr)
                {
                    helper = CORINFO_HELP_METHODDESC_TO_STUBRUNTIMEMETHOD;
                }
                else if (resolvedToken.hField != nullptr)
                {
                    helper = CORINFO_HELP_FIELDDESC_TO_STUBRUNTIMEFIELD;
                }
                else
                {
                    helper = CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE;
                }

                op1 = gtNewHelperCallNode(helper, TYP_STRUCT, gtNewCallArgs(op1));
                op1->AsCall()->GetRetDesc()->InitializePrimitive(GetRuntimeHandleUnderlyingType());
                op1->AsCall()->SetRetLayout(typGetObjLayout(tokenType));

                impPushOnStack(op1, typeInfo(TI_STRUCT, tokenType));
                break;

            case CEE_UNBOX:
            case CEE_UNBOX_ANY:
            {
                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);

                BOOL runtimeLookup;
                op2 = impTokenToHandle(&resolvedToken, &runtimeLookup);
                if (op2 == nullptr)
                {
                    assert(compDonotInline());
                    return;
                }

                accessAllowedResult =
                    info.compCompHnd->canAccessClass(&resolvedToken, info.compMethodHnd, &calloutHelper);
                impHandleAccessAllowed(accessAllowedResult, &calloutHelper);

                if ((opcode == CEE_UNBOX_ANY) && !info.compCompHnd->isValueClass(resolvedToken.hClass))
                {
                    JITDUMP("\n Importing UNBOX.ANY(refClass) as CASTCLASS\n");
                    op1 = impPopStack().val;
                    goto CASTCLASS;
                }

                // Pop the object and create the unbox helper call

                op1 = impPopStack().val;
                assertImp(op1->gtType == TYP_REF);

                helper = info.compCompHnd->getUnBoxHelper(resolvedToken.hClass);
                assert(helper == CORINFO_HELP_UNBOX || helper == CORINFO_HELP_UNBOX_NULLABLE);

                // Check legality and profitability of inline expansion for unboxing.
                const bool canExpandInline    = (helper == CORINFO_HELP_UNBOX);
                const bool shouldExpandInline = !compCurBB->isRunRarely() && opts.OptimizationEnabled();

                if (canExpandInline && shouldExpandInline)
                {
                    // See if we know anything about the type of op1, the object being unboxed.
                    bool                 isExact   = false;
                    bool                 isNonNull = false;
                    CORINFO_CLASS_HANDLE clsHnd    = gtGetClassHandle(op1, &isExact, &isNonNull);

                    // We can skip the "exact" bit here as we are comparing to a value class.
                    // compareTypesForEquality should bail on comparisions for shared value classes.
                    if (clsHnd != NO_CLASS_HANDLE)
                    {
                        const TypeCompareState compare =
                            info.compCompHnd->compareTypesForEquality(resolvedToken.hClass, clsHnd);

                        if (compare == TypeCompareState::Must)
                        {
                            JITDUMP("\nOptimizing %s (%s) -- type test will succeed\n",
                                    opcode == CEE_UNBOX ? "UNBOX" : "UNBOX.ANY", eeGetClassName(clsHnd));

                            // For UNBOX, null check (if necessary), and then leave the box payload byref on the stack.
                            if (opcode == CEE_UNBOX)
                            {
                                GenTree* cloneOperand;
                                op1 = impCloneExpr(op1, &cloneOperand, NO_CLASS_HANDLE,
                                                   CHECK_SPILL_ALL DEBUGARG("optimized unbox clone"));

                                GenTree* boxPayloadOffset = gtNewIconNode(TARGET_POINTER_SIZE, TYP_I_IMPL);
                                GenTree* boxPayloadAddress =
                                    gtNewOperNode(GT_ADD, TYP_BYREF, cloneOperand, boxPayloadOffset);
                                GenTree* nullcheck = gtNewNullCheck(op1, block);
                                GenTree* result    = gtNewOperNode(GT_COMMA, TYP_BYREF, nullcheck, boxPayloadAddress);
                                impPushOnStack(result, typeInfo());
                                break;
                            }

                            // For UNBOX.ANY load the struct from the box payload byref (the load will nullcheck)
                            assert(opcode == CEE_UNBOX_ANY);
                            GenTree* boxPayloadOffset = gtNewIconNode(TARGET_POINTER_SIZE, TYP_I_IMPL);
                            op1                       = gtNewOperNode(GT_ADD, TYP_BYREF, op1, boxPayloadOffset);
                            goto LDOBJ;
                        }
                        else
                        {
                            JITDUMP("\nUnable to optimize %s -- can't resolve type comparison\n",
                                    opcode == CEE_UNBOX ? "UNBOX" : "UNBOX.ANY");
                        }
                    }
                    else
                    {
                        JITDUMP("\nUnable to optimize %s -- class for [%06u] not known\n",
                                opcode == CEE_UNBOX ? "UNBOX" : "UNBOX.ANY", dspTreeID(op1));
                    }

                    JITDUMP("\n Importing %s as inline sequence\n", opcode == CEE_UNBOX ? "UNBOX" : "UNBOX.ANY");
                    // we are doing normal unboxing
                    // inline the common case of the unbox helper
                    // UNBOX(exp) morphs into
                    // clone = pop(exp);
                    // ((*clone == typeToken) ? nop : helper(clone, typeToken));
                    // push(clone + TARGET_POINTER_SIZE)
                    //
                    GenTree* cloneOperand;
                    op1 = impCloneExpr(op1, &cloneOperand, NO_CLASS_HANDLE,
                                       CHECK_SPILL_ALL DEBUGARG("inline UNBOX clone1"));
                    op1 = gtNewMethodTableLookup(op1);

                    GenTree* condBox = gtNewOperNode(GT_EQ, TYP_INT, op1, op2);

                    op1 = impCloneExpr(cloneOperand, &cloneOperand, NO_CLASS_HANDLE,
                                       CHECK_SPILL_ALL DEBUGARG("inline UNBOX clone2"));
                    op2 = impTokenToHandle(&resolvedToken);
                    if (op2 == nullptr)
                    { // compDonotInline()
                        return;
                    }
                    op1 = gtNewHelperCallNode(helper, TYP_VOID, gtNewCallArgs(op2, op1));
                    op1 = gtNewQmarkNode(TYP_VOID, condBox, gtNewNothingNode(), op1);

                    // QMARK nodes cannot reside on the evaluation stack. Because there
                    // may be other trees on the evaluation stack that side-effect the
                    // sources of the UNBOX operation we must spill the stack.

                    impAppendTree(op1, CHECK_SPILL_ALL, impCurStmtOffs);

                    // Create the address-expression to reference past the object header
                    // to the beginning of the value-type. Today this means adjusting
                    // past the base of the objects vtable field which is pointer sized.

                    op2 = gtNewIconNode(TARGET_POINTER_SIZE, TYP_I_IMPL);
                    op1 = gtNewOperNode(GT_ADD, TYP_BYREF, cloneOperand, op2);
                }
                else
                {
                    JITDUMP("\n Importing %s as helper call because %s\n", opcode == CEE_UNBOX ? "UNBOX" : "UNBOX.ANY",
                            canExpandInline ? "want smaller code or faster jitting" : "inline expansion not legal");

                    // Don't optimize, just call the helper and be done with it
                    if (helper == CORINFO_HELP_UNBOX)
                    {
                        op1 = gtNewHelperCallNode(helper, TYP_BYREF, gtNewCallArgs(op2, op1));
                    }
                    else
                    {
                        assert(helper == CORINFO_HELP_UNBOX_NULLABLE);

                        op1 = gtNewHelperCallNode(helper, TYP_STRUCT, gtNewCallArgs(op2, op1));
                        op1->AsCall()->SetRetLayout(typGetObjLayout(resolvedToken.hClass));

                        // This helper always returns the nullable struct via an "out" parameter,
                        // similar to a return buffer. We do not need to initialize reg types.
                        assert(op1->AsCall()->TreatAsHasRetBufArg());
                    }
                }

                assert(((helper == CORINFO_HELP_UNBOX) && op1->TypeIs(TYP_BYREF)) ||
                       ((helper == CORINFO_HELP_UNBOX_NULLABLE) && op1->TypeIs(TYP_STRUCT)));

                if (opcode == CEE_UNBOX)
                {
                    if (helper == CORINFO_HELP_UNBOX_NULLABLE)
                    {
                        // Unbox nullable helper returns a struct type.
                        // We need to spill it to a temp so than can take the address of it.
                        // Here we need unsafe value cls check, since the address of struct is taken to be used
                        // further along and potetially be exploitable.

                        unsigned tmp = lvaGrabTemp(true DEBUGARG("UNBOXing a nullable"));
                        lvaSetStruct(tmp, resolvedToken.hClass, /* checkUnsafeBuffer */ true);

                        op2 = gtNewLclvNode(tmp, TYP_STRUCT);
                        op1 = impAssignStruct(op2, op1, resolvedToken.hClass, CHECK_SPILL_ALL);
                        assert(op1->gtType == TYP_VOID); // We must be assigning the return struct to the temp.

                        op2 = gtNewAddrNode(gtNewLclvNode(tmp, TYP_STRUCT));
                        op1 = gtNewOperNode(GT_COMMA, TYP_BYREF, op1, op2);
                    }

                    assert(op1->TypeIs(TYP_BYREF));

                    impPushOnStack(op1, typeInfo());
                    break;
                }

                assert(opcode == CEE_UNBOX_ANY);

                if (helper == CORINFO_HELP_UNBOX)
                {
                    // Normal unbox helper returns a TYP_BYREF.
                    goto LDOBJ;
                }

                assert(helper == CORINFO_HELP_UNBOX_NULLABLE);
                assert(op1->TypeIs(TYP_STRUCT));

#if FEATURE_MULTIREG_RET
                // TODO-MIKE-Cleanup: This has nothing to do with multireg returns.
                // No matter what the struct type is the helper returns the struct value
                // via an "out" parameter, there's no way to return it in registers because
                // the same helper is used for all struct types.
                // Doing this here is bad for CQ when the destination is a memory location,
                // because we introduce a temp instead of just passing in the address of
                // that location. impAssignStructPtr (TreatAsHasRetBufArg) already handles
                // this case so there's no real need to do this here.
                // Adding a temp when the destination is a promotable struct local might
                // be useful because it avoids dependent promotion. But's probably something
                // that impAssignStructPtr could handle as well.

                ClassLayout*  layout  = typGetObjLayout(resolvedToken.hClass);
                StructPassing retKind = abiGetStructReturnType(layout, CorInfoCallConvExtension::Managed);

                if ((retKind.kind == SPK_ByValue) || (retKind.kind == SPK_ByValueAsHfa))
                {
                    // Unbox nullable helper returns a TYP_STRUCT.
                    // For the multi-reg case we need to spill it to a temp so that
                    // we can pass the address to the unbox_nullable jit helper.

                    unsigned tmp = lvaGrabTemp(true DEBUGARG("UNBOXing a register returnable nullable"));
                    lvaTable[tmp].lvIsMultiRegArg = true;
                    lvaSetStruct(tmp, layout, /* checkUnsafeBuffer */ true);

                    op2 = gtNewLclvNode(tmp, TYP_STRUCT);
                    op1 = impAssignStruct(op2, op1, resolvedToken.hClass, CHECK_SPILL_ALL);
                    assert(op1->gtType == TYP_VOID); // We must be assigning the return struct to the temp.

                    op2 = gtNewAddrNode(gtNewLclvNode(tmp, TYP_STRUCT));
                    op1 = gtNewOperNode(GT_COMMA, TYP_BYREF, op1, op2);

                    goto LDOBJ;
                }
#endif // !FEATURE_MULTIREG_RET

                impPushOnStack(op1, typeInfo(TI_STRUCT, resolvedToken.hClass));
            }
            break;

            case CEE_BOX:
            {
                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Box);
                JITDUMP(" %08X", resolvedToken.token);

                accessAllowedResult =
                    info.compCompHnd->canAccessClass(&resolvedToken, info.compMethodHnd, &calloutHelper);
                impHandleAccessAllowed(accessAllowedResult, &calloutHelper);

                if (!info.compCompHnd->isValueClass(resolvedToken.hClass))
                {
                    // Boxing a reference type has no effect.
                    break;
                }

                // Look ahead for box idioms
                int matched = impBoxPatternMatch(&resolvedToken, codeAddr + sz, codeEndp);
                if (matched >= 0)
                {
                    // Skip the matched IL instructions
                    sz += matched;
                    break;
                }

                impImportAndPushBox(&resolvedToken);
                if (compDonotInline())
                {
                    return;
                }
            }
            break;

            case CEE_SIZEOF:
                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);

                op1 = gtNewIconNode(info.compCompHnd->getClassSize(resolvedToken.hClass));
                impPushOnStack(op1, typeInfo());
                break;

            case CEE_CASTCLASS:
                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Casting);
                JITDUMP(" %08X", resolvedToken.token);

                if (!opts.IsReadyToRun())
                {
                    op2 = impTokenToHandle(&resolvedToken, nullptr, FALSE);
                    if (op2 == nullptr)
                    { // compDonotInline()
                        return;
                    }
                }

                accessAllowedResult =
                    info.compCompHnd->canAccessClass(&resolvedToken, info.compMethodHnd, &calloutHelper);
                impHandleAccessAllowed(accessAllowedResult, &calloutHelper);

                op1 = impPopStack().val;

            /* Pop the address and create the 'checked cast' helper call */

            // At this point we expect typeRef to contain the token, op1 to contain the value being cast,
            // and op2 to contain code that creates the type handle corresponding to typeRef
            CASTCLASS:
            {
                GenTree* optTree = impOptimizeCastClassOrIsInst(op1, &resolvedToken, true);

                if (optTree != nullptr)
                {
                    impPushOnStack(optTree, typeInfo());
                }
                else
                {

#ifdef FEATURE_READYTORUN_COMPILER
                    if (opts.IsReadyToRun())
                    {
                        GenTreeCall* opLookup =
                            impReadyToRunHelperToTree(&resolvedToken, CORINFO_HELP_READYTORUN_CHKCAST, TYP_REF,
                                                      gtNewCallArgs(op1));
                        usingReadyToRunHelper = (opLookup != nullptr);
                        op1                   = (usingReadyToRunHelper ? opLookup : op1);

                        if (!usingReadyToRunHelper)
                        {
                            // TODO: ReadyToRun: When generic dictionary lookups are necessary, replace the lookup call
                            // and the chkcastany call with a single call to a dynamic R2R cell that will:
                            //      1) Load the context
                            //      2) Perform the generic dictionary lookup and caching, and generate the appropriate
                            //      stub
                            //      3) Check the object on the stack for the type-cast
                            // Reason: performance (today, we'll always use the slow helper for the R2R generics case)

                            op2 = impTokenToHandle(&resolvedToken, nullptr, FALSE);
                            if (op2 == nullptr)
                            {
                                return;
                            }
                        }
                    }

                    if (!usingReadyToRunHelper)
#endif
                    {
                        op1 = impCastClassOrIsInstToTree(op1, op2, &resolvedToken, true);
                    }
                    if (compDonotInline())
                    {
                        return;
                    }

                    impPushOnStack(op1, typeInfo());
                }
            }
            break;

            case CEE_THROW:
                // Any block with a throw is rarely executed.
                block->bbSetRunRarely();

                // Pop the exception object and create the 'throw' helper call
                op1 = gtNewHelperCallNode(CORINFO_HELP_THROW, TYP_VOID, gtNewCallArgs(impPopStack().val));

            // Fall through to clear out the eval stack.

            EVAL_APPEND:
                if (verCurrentState.esStackDepth > 0)
                {
                    impEvalSideEffects();
                }

                assert(verCurrentState.esStackDepth == 0);

                goto APPEND;

            case CEE_RETHROW:

                assert(!compIsForInlining());

                if (info.compXcptnsCount == 0)
                {
                    BADCODE("rethrow outside catch");
                }

                op1 = gtNewHelperCallNode(CORINFO_HELP_RETHROW, TYP_VOID);

                goto EVAL_APPEND;

            case CEE_INITBLK:
                op3 = impPopStack().val; // Size
                op2 = impPopStack().val; // Value
                op1 = impPopStack().val; // Dest

                op1 = impImportInitBlk(op1, op2, op3, (prefixFlags & PREFIX_VOLATILE) != 0);
                goto SPILL_APPEND;

            case CEE_CPBLK:
                op3 = impPopStack().val; // Size
                op2 = impPopStack().val; // Src
                op1 = impPopStack().val; // Dest

                op1 = impImportCpBlk(op1, op2, op3, (prefixFlags & PREFIX_VOLATILE) != 0);
                goto SPILL_APPEND;

            case CEE_INITOBJ:
                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);

                if (info.compCompHnd->isValueClass(resolvedToken.hClass))
                {
                    lclTyp = JITtype2varType(info.compCompHnd->asCorInfoType(resolvedToken.hClass));
                }
                else
                {
                    lclTyp = TYP_REF;
                }

                op1 = impPopStack().val; // Destination address

                assertImp(op1->TypeIs(TYP_I_IMPL, TYP_BYREF));

                impBashVarAddrsToI(op1);

                if (lclTyp != TYP_STRUCT)
                {
                    op2 = gtNewZeroConNode(varActualType(lclTyp));

                    goto STIND_CPOBJ;
                }

                op1 = impImportInitObj(op1, resolvedToken.hClass);
                goto SPILL_APPEND;

            case CEE_CPOBJ:
                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);

                if (info.compCompHnd->isValueClass(resolvedToken.hClass))
                {
                    lclTyp = JITtype2varType(info.compCompHnd->asCorInfoType(resolvedToken.hClass));
                }
                else
                {
                    lclTyp = TYP_REF;
                }

                op2 = impPopStack().val; // Source address
                op1 = impPopStack().val; // Destination address

                assertImp(op1->TypeIs(TYP_I_IMPL, TYP_BYREF));
                assertImp(op2->TypeIs(TYP_I_IMPL, TYP_BYREF));

                impBashVarAddrsToI(op1, op2);

                if (lclTyp != TYP_STRUCT)
                {
                    op2 = gtNewOperNode(GT_IND, lclTyp, op2);
                    op2->gtFlags |= GTF_EXCEPT | GTF_GLOB_REF;

                    goto STIND_CPOBJ;
                }

                op1 = impImportCpObj(op1, op2, resolvedToken.hClass);
                goto SPILL_APPEND;

            case CEE_STOBJ:
                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);

                if (info.compCompHnd->isValueClass(resolvedToken.hClass))
                {
                    lclTyp = JITtype2varType(info.compCompHnd->asCorInfoType(resolvedToken.hClass));
                }
                else
                {
                    lclTyp = TYP_REF;
                }

                if (lclTyp != TYP_STRUCT)
                {
                    goto STIND;
                }

                op2 = impPopStack().val; // Value
                op1 = impPopStack().val; // Ptr

                assertImp(varTypeIsStruct(op2));

                op1 = impAssignStructPtr(op1, op2, resolvedToken.hClass, CHECK_SPILL_ALL);

                if ((prefixFlags & PREFIX_UNALIGNED) != 0)
                {
                    if (op1->OperIs(GT_ASG))
                    {
                        // If the store value is MKREFANY impAssignStructPtr will append another indir,
                        // we don't set unaligned on that. It isn't necessary since the JIT doesn't do
                        // anything special with unaligned if the indir type is integral.

                        if (GenTreeIndir* indir = op1->AsOp()->GetOp(0)->IsIndir())
                        {
                            indir->SetUnaligned();
                        }
                    }
                    else
                    {
                        // It's possible that impAssignStructPtr returned a CALL node (struct returned
                        // via return buffer). We're ignoring the unaligned prefix in this case.

                        // TODO-MIKE-Consider: We should probably introduce a temp, pass that as
                        // return buffer and then assign the temp to the actual STOBJ destination.

                        assert(op1->OperIs(GT_CALL) && op1->TypeIs(TYP_VOID));
                    }
                }
                goto SPILL_APPEND;

            case CEE_MKREFANY:
                assert(!compIsForInlining());

                // Being lazy here. Refanys are tricky in terms of gc tracking.
                // Since it is uncommon, just don't perform struct promotion in any method that contains mkrefany.

                JITDUMP("disabling struct promotion because of mkrefany\n");
                fgNoStructPromotion = true;

                oper = GT_MKREFANY;
                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);

                op2 = impTokenToHandle(&resolvedToken, nullptr, TRUE);
                if (op2 == nullptr)
                {
                    return;
                }

                accessAllowedResult =
                    info.compCompHnd->canAccessClass(&resolvedToken, info.compMethodHnd, &calloutHelper);
                impHandleAccessAllowed(accessAllowedResult, &calloutHelper);

                op1 = impPopStack().val;

                // @SPECVIOLATION: TYP_INT should not be allowed here by a strict reading of the spec.
                // But JIT32 allowed it, so we continue to allow it.
                assertImp(op1->TypeGet() == TYP_BYREF || op1->TypeGet() == TYP_I_IMPL || op1->TypeGet() == TYP_INT);

                // MKREFANY returns a struct.  op2 is the class token.
                op1 = gtNewOperNode(oper, TYP_STRUCT, op1, op2);

                impPushOnStack(op1, typeInfo(TI_STRUCT, impGetRefAnyClass()));
                break;

            case CEE_LDOBJ:
                assertImp(sz == sizeof(unsigned));
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);

                if (!info.compCompHnd->isValueClass(resolvedToken.hClass))
                {
                    lclTyp = TYP_REF;
                    opcode = CEE_LDIND_REF;

                    // TODO-MIKE-Cleanup: It's convenient to reuse the LDIND.REF import code but in doing so
                    // we are losing the class handle. The code below already handles primitive types (and
                    // cannot easily reuse the LDIND import code due to pesky normed types) and it should be
                    // pretty easy to adapt it to also handle object references.
                    // Though it's unlikely to be very useful to do this, such LDOBJs are probably only
                    // appearing in generic code and only when byrefs are involved (e.g. a method argument of
                    // type `ref SomeClass`).

                    goto LDIND;
                }

                op1 = impPopStack().val;
                assertImp(op1->TypeIs(TYP_BYREF, TYP_I_IMPL));

            LDOBJ:
                lclTyp = JITtype2varType(info.compCompHnd->asCorInfoType(resolvedToken.hClass));

                if (lclTyp == TYP_STRUCT)
                {
                    op1 = gtNewObjNode(resolvedToken.hClass, op1);
                }
                else
                {
                    assertImp(varTypeIsArithmetic(lclTyp));

                    op1 = gtNewOperNode(GT_IND, lclTyp, op1);
                    op1->gtFlags |= GTF_GLOB_REF;
                }

                op1->gtFlags |= GTF_EXCEPT;

                if ((prefixFlags & PREFIX_UNALIGNED) != 0)
                {
                    op1->gtFlags |= GTF_IND_UNALIGNED;
                }

                // TODO-MIKE-Fix: This doesn't check for volatile. prefix...

                if ((lclTyp == TYP_STRUCT) ||
                    (info.compCompHnd->getTypeForPrimitiveValueClass(resolvedToken.hClass) == CORINFO_TYPE_UNDEF))
                {
                    impPushOnStack(op1, typeInfo(TI_STRUCT, resolvedToken.hClass));
                }
                else
                {
                    impPushOnStack(op1, typeInfo());
                }
                break;

            case CEE_LDLEN:
                op1 = impPopStack().val;
                if (opts.OptimizationEnabled())
                {
                    op1 = gtNewArrLen(op1, OFFSETOF__CORINFO_Array__length, block);
                }
                else
                {
                    op1 = gtNewOperNode(GT_ADD, TYP_BYREF, op1,
                                        gtNewIconNode(OFFSETOF__CORINFO_Array__length, TYP_I_IMPL));
                    op1 = gtNewIndir(TYP_INT, op1);
                }

                impPushOnStack(op1, typeInfo());
                break;

            case CEE_BREAK:
                op1 = gtNewHelperCallNode(CORINFO_HELP_USER_BREAKPOINT, TYP_VOID);
                goto SPILL_ALL_APPEND;

            case CEE_NOP:
                if (opts.compDbgCode)
                {
                    op1 = new (this, GT_NO_OP) GenTree(GT_NO_OP, TYP_VOID);
                    goto SPILL_ALL_APPEND;
                }
                break;

            /******************************** NYI *******************************/

            case 0xCC:
                OutputDebugStringA("CLR: Invalid x86 breakpoint in IL stream\n");
                FALLTHROUGH;

            case CEE_ILLEGAL:
            case CEE_MACRO_END:

            default:
                if (compIsForInlining())
                {
                    compInlineResult->NoteFatal(InlineObservation::CALLEE_COMPILATION_ERROR);
                    return;
                }

                BADCODE3("unknown opcode", ": %02X", (int)opcode);
        }

        codeAddr += sz;
        prevOpcode = opcode;

        prefixFlags = 0;
    }
}
#ifdef _PREFAST_
#pragma warning(pop)
#endif

// Load an argument on the operand stack
// Shared by the various CEE_LDARG opcodes
// ilArgNum is the argument index as specified in IL.
// It will be mapped to the correct lvaTable index
void Compiler::impLoadArg(unsigned ilArgNum, IL_OFFSET offset)
{
    if (compIsForInlining())
    {
        if (ilArgNum >= impInlineInfo->ilArgCount)
        {
            compInlineResult->NoteFatal(InlineObservation::CALLEE_BAD_ARGUMENT_NUMBER);
            return;
        }

        impPushOnStack(inlUseArg(impInlineInfo, ilArgNum), impInlineInfo->GetParamTypeInfo(ilArgNum));
    }
    else
    {
        if (ilArgNum >= info.compILargsCount)
        {
            BADCODE("Bad IL arg num");
        }

        unsigned lclNum = compMapILargNum(ilArgNum); // account for possible hidden param

        if (lclNum == info.compThisArg)
        {
            lclNum = lvaArg0Var;
        }

        impPushLclVar(lclNum, offset);
    }
}

// Load a local on the operand stack
// Shared by the various CEE_LDLOC opcodes
// ilLocNum is the local index as specified in IL.
// It will be mapped to the correct lvaTable index
void Compiler::impLoadLoc(unsigned ilLocNum, IL_OFFSET offset)
{
    unsigned lclNum;

    if (compIsForInlining())
    {
        if (ilLocNum >= impInlineInfo->ilLocCount)
        {
            compInlineResult->NoteFatal(InlineObservation::CALLEE_BAD_LOCAL_NUMBER);
            return;
        }

        lclNum = inlGetInlineeLocal(impInlineInfo, ilLocNum);
        offset = BAD_IL_OFFSET;
    }
    else
    {
        if (ilLocNum >= info.compMethodInfo->locals.numArgs)
        {
            BADCODE("Bad IL loc num");
        }

        lclNum = info.compArgsCount + ilLocNum;
    }

    impPushLclVar(lclNum, offset);
}

// Load a local/argument on the operand stack
// lclNum is an index into lvaTable *NOT* the arg/lcl index in the IL
void Compiler::impPushLclVar(unsigned lclNum, IL_OFFSET offset)
{
    LclVarDsc* lcl  = lvaGetDesc(lclNum);
    var_types  type = lcl->GetType();

    if (!lcl->lvNormalizeOnLoad())
    {
        type = varActualType(type);
    }

    impPushOnStack(gtNewLclvNode(lclNum, type DEBUGARG(offset)), lcl->lvImpTypeInfo);
}

//------------------------------------------------------------------------
// impInlineReturnInstruction: import a return during inlining
//
// Returns:
//     True if import was successful (may fail for some inlinees)
//
bool Compiler::impInlineReturnInstruction()
{
    assert(compIsForInlining());

    if (info.compRetType == TYP_VOID)
    {
        return true;
    }

    StackEntry           se        = impPopStack();
    CORINFO_CLASS_HANDLE retClsHnd = se.seTypeInfo.GetClassHandle();
    GenTree*             op2       = se.val;

    // inlinee's stack should be empty now.
    assert(verCurrentState.esStackDepth == 0);

    return inlImportReturn(impInlineInfo, op2, retClsHnd);
}

void Compiler::impReturnInstruction(int prefixFlags, OPCODE* opcode)
{
    assert(!compIsForInlining());

    GenTree* ret;

    if (info.compRetType == TYP_VOID)
    {
        assert(info.retDesc.GetRegCount() == 0);

        ret = new (this, GT_RETURN) GenTreeOp(GT_RETURN, TYP_VOID);
    }
    else
    {
        StackEntry se    = impPopStack();
        GenTree*   value = se.val;

        impBashVarAddrsToI(value);
        value = impImplicitIorI4Cast(value, info.compRetType);
        value = impImplicitR4orR8Cast(value, info.compRetType);

#ifdef DEBUG
        {
            GenTree*  op1     = value;
            GenTree*  op2     = nullptr;
            var_types retType = info.compRetType;
            var_types valType = value->GetType();
            assertImp((varActualType(valType) == varActualType(retType)) ||
                      ((valType == TYP_I_IMPL) && (retType == TYP_BYREF)) ||
                      ((valType == TYP_BYREF) && (retType == TYP_I_IMPL)) ||
                      (varTypeIsFloating(valType) && varTypeIsFloating(retType)) ||
                      (varTypeIsStruct(valType) && varTypeIsStruct(retType)));
        }

        if (((prefixFlags & PREFIX_TAILCALL) == 0) && opts.compGcChecks && (info.compRetType == TYP_REF))
        {
            // DDB 3483  : JIT Stress: early termination of GC ref's life time in exception code path
            // VSW 440513: Incorrect gcinfo on the return value under COMPlus_JitGCChecks=1 for methods with
            // one-return BB.

            assert(value->TypeIs(TYP_REF));

            // confirm that the argument is a GC pointer (for debugging (GC stress))
            value = gtNewHelperCallNode(CORINFO_HELP_CHECK_OBJ, TYP_REF, gtNewCallArgs(value));

            if (verbose)
            {
                printf("\ncompGcChecks tree:\n");
                gtDispTree(value);
            }
        }
#endif

        if (info.compRetBuffArg != BAD_VAR_NUM)
        {
            GenTree* retBuffAddr = gtNewLclvNode(info.compRetBuffArg, TYP_BYREF DEBUGARG(impCurStmtOffs));
            value = impAssignStructPtr(retBuffAddr, value, se.seTypeInfo.GetClassHandle(), CHECK_SPILL_ALL);
            impAppendTree(value, CHECK_SPILL_NONE, impCurStmtOffs);

            if (info.retDesc.GetRegCount() == 0)
            {
                ret = new (this, GT_RETURN) GenTreeOp(GT_RETURN, TYP_VOID);
            }
            else
            {
                // There are cases where the address of the implicit RetBuf should be returned explicitly.

                assert(info.retDesc.GetRegCount() == 1);
                assert(info.retDesc.GetRegType(0) == TYP_BYREF);

                ret = gtNewOperNode(GT_RETURN, TYP_BYREF, gtNewLclvNode(info.compRetBuffArg, TYP_BYREF));
            }
        }
        else
        {
            assert(info.retDesc.GetRegCount() >= 1);

            if (value->IsCall() && value->AsCall()->TreatAsHasRetBufArg())
            {
                value = impSpillPseudoReturnBufferCall(value, se.seTypeInfo.GetClassHandle());
            }

#if FEATURE_MULTIREG_RET
            if (varTypeIsStruct(info.compRetType) && (info.retDesc.GetRegCount() > 1))
            {
                value = impCanonicalizeMultiRegReturnValue(value, se.seTypeInfo.GetClassHandle());
            }
#endif

            ret = gtNewOperNode(GT_RETURN, varActualType(info.compRetType), value);
        }
    }

    // We must have imported a tailcall and jumped to RET
    if ((prefixFlags & PREFIX_TAILCALL) != 0)
    {
        assert((verCurrentState.esStackDepth == 0) && impOpcodeIsCallOpcode(*opcode));

        *opcode = CEE_RET; // To prevent trying to spill if CALL_SITE_BOUNDARIES

        // impImportCall() would have already appended TYP_VOID calls
        if (info.compRetType == TYP_VOID)
        {
            return;
        }
    }

    impAppendTree(ret, CHECK_SPILL_NONE, impCurStmtOffs);

    // Remember at which IL offset the tree was finished
    INDEBUG(impNoteLastILoffs();)
}

void Compiler::impAddPendingEHSuccessors(BasicBlock* block)
{
    assert(!compIsForInlining());

    unsigned  tryIndex = block->getTryIndex();
    EHblkDsc* ehDesc   = ehGetDsc(tryIndex);

    while (ehDesc != nullptr)
    {
        // Recursively process the handler block, if we haven't already done so.
        BasicBlock* hndBegBB = ehDesc->ebdHndBeg;

        if (((hndBegBB->bbFlags & BBF_IMPORTED) == 0) && !impIsPendingBlockMember(hndBegBB))
        {
            if (handlerGetsXcptnObj(hndBegBB->bbCatchTyp))
            {
                CORINFO_CLASS_HANDLE clsHnd;

                if (ehDesc->HasFilter())
                {
                    clsHnd = impGetObjectClass();
                }
                else
                {
                    CORINFO_RESOLVED_TOKEN resolvedToken;
                    resolvedToken.tokenContext = impTokenLookupContextHandle;
                    resolvedToken.tokenScope   = info.compScopeHnd;
                    resolvedToken.token        = ehDesc->ebdTyp;
                    resolvedToken.tokenType    = CORINFO_TOKENKIND_Class;
                    info.compCompHnd->resolveToken(&resolvedToken);

                    clsHnd = resolvedToken.hClass;
                }

                // push catch arg the stack, spill to a temp if necessary
                // Note: can update HBtab->ebdHndBeg!
                hndBegBB = impPushCatchArgOnStack(hndBegBB, clsHnd, false);
            }

            impImportBlockPending(hndBegBB);
        }

        // Process the filter block, if we haven't already done so.
        if (ehDesc->HasFilter())
        {
            BasicBlock* filterBB = ehDesc->ebdFilter;

            if (((filterBB->bbFlags & BBF_IMPORTED) == 0) && !impIsPendingBlockMember(filterBB))
            {
                // push catch arg the stack, spill to a temp if necessary
                // Note: can update HBtab->ebdFilter!
                const bool isSingleBlockFilter = (filterBB->bbNext == hndBegBB);
                filterBB = impPushCatchArgOnStack(filterBB, impGetObjectClass(), isSingleBlockFilter);

                impImportBlockPending(filterBB);
            }
        }

        // Now process our enclosing try index (if any)
        tryIndex = ehDesc->ebdEnclosingTryIndex;
        if (tryIndex == EHblkDsc::NO_ENCLOSING_INDEX)
        {
            ehDesc = nullptr;
        }
        else
        {
            ehDesc = ehGetDsc(tryIndex);
        }
    }
}

void Compiler::impImportBlock(BasicBlock* block)
{
    // BBF_INTERNAL blocks only exist during importation due to EH canonicalization. We need to
    // handle them specially. In particular, there is no IL to import for them, but we do need
    // to mark them as imported and put their successors on the pending import list.
    if ((block->bbFlags & BBF_INTERNAL) != 0)
    {
        JITDUMP("Marking BBF_INTERNAL block " FMT_BB " as BBF_IMPORTED\n", block->bbNum);
        block->bbFlags |= BBF_IMPORTED;

        // Since there's no IL to import the exit state is the same as the entry state.
        impSetSpillCliqueState(block, block->bbEntryState);

        const unsigned numSuccs = block->NumSucc();
        for (unsigned i = 0; i < numSuccs; i++)
        {
            impImportBlockPending(block->GetSucc(i));
        }

        return;
    }

    impSetCurrentState(block);

    compCurBB = block;

    INDEBUG(impCurOpcName = "unknown";)
    INDEBUG(impCurOpcOffs = block->bbCodeOffs;)

    if (((block->bbFlags & BBF_TRY_BEG) != 0) && (verCurrentState.esStackDepth != 0))
    {
        BADCODE("Evaluation stack must be empty on entry into a try block");
    }

    if (block->hasTryIndex())
    {
        impAddPendingEHSuccessors(block);
    }

    impImportBlockCode(block);

    if (compDonotInline())
    {
        return;
    }

    bool reimportSpillClique = false;

    if (verCurrentState.esStackDepth != 0)
    {
        reimportSpillClique = impSpillStackAtBlockEnd(block);
    }

    // Some of the append/spill logic works on compCurBB

    assert(compCurBB == block);

    /* Save the tree list in the block */
    impEndTreeList(block);

    // impEndTreeList sets BBF_IMPORTED on the block
    // We do *NOT* want to set it later than this because
    // impReimportSpillClique might clear it if this block is both a
    // predecessor and successor in the current spill clique
    assert((block->bbFlags & BBF_IMPORTED) != 0);

    // If we had a int/native int, or float/double collision, we need to re-import
    if (reimportSpillClique)
    {
        impReimportSpillClique(block);
    }

    const unsigned numSuccs = block->NumSucc();
    for (unsigned i = 0; i < numSuccs; i++)
    {
        impImportBlockPending(block->GetSucc(i));
    }
}

bool Compiler::impSpillStackAtBlockEnd(BasicBlock* block)
{
    JITDUMP("\nSpilling %u stack entries at the end of " FMT_BB "\n", verCurrentState.esStackDepth, block->bbNum);

    switch (block->bbJumpKind)
    {
        case BBJ_CALLFINALLY:
        case BBJ_EHCATCHRET:
        case BBJ_RETURN:
        case BBJ_EHFINALLYRET:
        case BBJ_EHFILTERRET:
        case BBJ_THROW:
            BADCODE("can't have 'unreached' end of BB with non-empty stack");
            break;
        default:
            break;
    }

    // If a box temp is used in a block that leaves something on the stack, its lifetime
    // is hard to determine, simply don't reuse such temps.
    impBoxTemp = BAD_VAR_NUM;

    // Remove the branch statement at the end of the block (if any) because spilling must
    // be done before it. It will need to be added back once spilling is done.
    Statement* branchStmt = nullptr;

    if (block->bbJumpKind == BBJ_COND)
    {
        branchStmt = impExtractLastStmt();
        assert(branchStmt->GetRootNode()->OperIs(GT_JTRUE));
    }
    else if (block->bbJumpKind == BBJ_SWITCH)
    {
        branchStmt = impExtractLastStmt();
        assert(branchStmt->GetRootNode()->OperIs(GT_SWITCH));
    }

    ImportSpillCliqueState* state               = block->bbExitState;
    bool                    reimportSpillClique = false;

    if (state == nullptr)
    {
        unsigned spillTempBaseLclNum = lvaGrabTemps(verCurrentState.esStackDepth DEBUGARG("spill clique temp"));

        state = new (this, CMK_ImpStack) ImportSpillCliqueState(spillTempBaseLclNum, verCurrentState.esStackDepth);

        impSetSpillCliqueState(block, state);

        for (unsigned level = 0; level < verCurrentState.esStackDepth; level++)
        {
            unsigned   spillTempLclNum = spillTempBaseLclNum + level;
            LclVarDsc* spillTempLcl    = lvaGetDesc(spillTempLclNum);
            GenTree*   tree            = verCurrentState.esStack[level].val;
            typeInfo   stackType       = verCurrentState.esStack[level].seTypeInfo;

            JITDUMPTREE(tree, "Stack entry %u:\n", level);

            impAssignTempGen(spillTempLclNum, tree, stackType.GetClassHandle(), CHECK_SPILL_NONE);

            if (stackType.IsType(TI_STRUCT))
            {
                // We must propagate stack type info if it's TI_STRUCT, at least because
                // LDFLD import code needs it to recognize "normed types".
                // We must NOT propagate the type info if it's TI_REF because we don't
                // have any checks to ensure that all predecessors produce the same type,
                // nor do we attempt to determine a common base type if they don't.
                spillTempLcl->lvImpTypeInfo = stackType;
            }
        }
    }
    else
    {
        if (state->GetSpillTempCount() != verCurrentState.esStackDepth)
        {
            BADCODE("Same spill clique blocks have different stack depths at end.");
        }

        for (unsigned level = 0; level < verCurrentState.esStackDepth; level++)
        {
            unsigned   spillTempLclNum = state->GetSpillTempBaseLclNum() + level;
            LclVarDsc* spillTempLcl    = lvaGetDesc(spillTempLclNum);
            GenTree*   tree            = verCurrentState.esStack[level].val;

            JITDUMPTREE(tree, "Stack entry %u:\n", level);

            if (tree->TypeIs(TYP_BYREF) && (spillTempLcl->GetType() == TYP_I_IMPL))
            {
                // VC generates code where it pushes a byref from one branch, and an int (ldc.i4 0) from
                // the other.
                // However, if the branch which leaves the TYP_I_IMPL on the stack is imported first, the
                // successor would be imported assuming there was a TYP_I_IMPL on the stack. Thus the value
                // would not get GC-tracked. Hence, change the temp to TYP_BYREF and reimport the successors.

                spillTempLcl->SetType(TYP_BYREF);
                reimportSpillClique = true;
            }
#ifdef TARGET_64BIT
            else if (tree->TypeIs(TYP_LONG) && (spillTempLcl->GetType() == TYP_INT))
            {
                // Some other block in the spill clique set this to "int", but now we have "native int".
                // Change the type and go back to re-import any blocks that used the wrong type.
                spillTempLcl->SetType(TYP_LONG);
                reimportSpillClique = true;
            }
            else if ((varActualType(tree->GetType()) == TYP_INT) && (spillTempLcl->GetType() == TYP_LONG))
            {
                // Spill clique has decided this should be "native int", but this block only pushes an "int".
                // Insert a sign-extension to "native int" so we match the clique.
                tree = gtNewCastNode(TYP_LONG, tree, false, TYP_LONG);
            }
            // Consider the case where one branch left a 'byref' on the stack and the other leaves
            // an 'int'. On 32-bit, this is allowed since they are the same size. JIT64 managed to
            // make this work on 64-bit. For compatibility, we support JIT64 behavior instead of
            // asserting and then generating bad code (where we save/restore the low 32 bits of a
            // byref pointer to an 'int' sized local). If the 'int' side has been imported already,
            // we need to change the type of the local and reimport the spill clique. If the 'byref'
            // side has imported, we insert a cast from int to 'native int' to match the 'byref' size.
            else if (tree->TypeIs(TYP_BYREF) && (spillTempLcl->GetType() == TYP_INT))
            {
                // Some other block in the spill clique set this to "int", but now we have "byref".
                // Change the type and go back to re-import any blocks that used the wrong type.
                spillTempLcl->SetType(TYP_BYREF);
                reimportSpillClique = true;
            }
            else if ((varActualType(tree->GetType()) == TYP_INT) && (spillTempLcl->GetType() == TYP_BYREF))
            {
                // Spill clique has decided this should be "byref", but this block only pushes an "int".
                // Insert a sign-extension to "native int" so we match the clique size.
                tree = gtNewCastNode(TYP_LONG, tree, false, TYP_LONG);
            }
#endif // TARGET_64BIT
            else if (tree->TypeIs(TYP_DOUBLE) && (spillTempLcl->GetType() == TYP_FLOAT))
            {
                // Some other block in the spill clique set this to "float", but now we have "double".
                // Change the type and go back to re-import any blocks that used the wrong type.
                spillTempLcl->SetType(TYP_DOUBLE);
                reimportSpillClique = true;
            }
            else if (tree->TypeIs(TYP_FLOAT) && (spillTempLcl->GetType() == TYP_DOUBLE))
            {
                // Spill clique has decided this should be "double", but this block only pushes a "float".
                // Insert a cast to "double" so we match the clique.
                tree = gtNewCastNode(TYP_DOUBLE, tree, false, TYP_DOUBLE);
            }

            // If branchStmt has references to spillTempLclNum (can only happen if we are spilling to
            // the temps already used by a previous block), we need to spill such references because
            // spilling the current stack will modify the spill temp.

            if (branchStmt != nullptr)
            {
                GenTreeUnOp* branch = branchStmt->GetRootNode()->AsUnOp();

                if (branch->OperIs(GT_JTRUE))
                {
                    GenTreeOp* relOp = branch->GetOp(0)->AsOp();

                    if (gtHasRef(relOp->GetOp(0), spillTempLclNum))
                    {
                        unsigned temp = lvaGrabTemp(true DEBUGARG("branch spill temp"));
                        impAssignTempGen(temp, relOp->GetOp(0), level);
                        relOp->SetOp(0, gtNewLclvNode(temp, lvaGetDesc(temp)->GetType()));
                    }

                    if (gtHasRef(relOp->GetOp(1), spillTempLclNum))
                    {
                        unsigned temp = lvaGrabTemp(true DEBUGARG("branch spill temp"));
                        impAssignTempGen(temp, relOp->GetOp(1), level);
                        relOp->SetOp(1, gtNewLclvNode(temp, lvaGetDesc(temp)->GetType()));
                    }
                }
                else
                {
                    assert(branch->OperIs(GT_SWITCH));

                    if (gtHasRef(branch->GetOp(0), spillTempLclNum))
                    {
                        unsigned temp = lvaGrabTemp(true DEBUGARG("branch spill temp"));
                        impAssignTempGen(temp, branch->GetOp(0), level);
                        branch->SetOp(0, gtNewLclvNode(temp, lvaGetDesc(temp)->GetType()));
                    }
                }
            }

            impAssignTempGen(spillTempLclNum, tree, verCurrentState.esStack[level].seTypeInfo.GetClassHandle(),
                             CHECK_SPILL_NONE);
        }
    }

    if (branchStmt != nullptr)
    {
        impAppendStmt(branchStmt, CHECK_SPILL_NONE);
    }

    return reimportSpillClique;
}

void Compiler::impImportBlockPending(BasicBlock* block)
{
    if (((block->bbFlags & BBF_IMPORTED) == 0) && !impIsPendingBlockMember(block))
    {
        JITDUMP(FMT_BB " pending import\n", block->bbNum);

        impPushPendingBlock(block);
    }
}

void Compiler::impPushPendingBlock(BasicBlock* block)
{
    assert((block->bbFlags & BBF_IMPORTED) == 0);
    assert(!impIsPendingBlockMember(block));

    BlockListNode* dsc   = new (this) BlockListNode(block, impPendingBlockStack);
    impPendingBlockStack = dsc;
    impSetPendingBlockMember(block, true);
}

BasicBlock* Compiler::impPopPendingBlock()
{
    BlockListNode* node = impPendingBlockStack;

    if (node == nullptr)
    {
        return nullptr;
    }

    BasicBlock* block = node->m_blk;
    impSetPendingBlockMember(block, false);
    impPendingBlockStack = node->m_next;
    FreeBlockListNode(node);
    return block;
}

void* Compiler::BlockListNode::operator new(size_t sz, Compiler* comp)
{
    if (comp->impBlockListNodeFreeList == nullptr)
    {
        return comp->getAllocator(CMK_BasicBlock).allocate<BlockListNode>(1);
    }
    else
    {
        BlockListNode* res             = comp->impBlockListNodeFreeList;
        comp->impBlockListNodeFreeList = res->m_next;
        return res;
    }
}

void Compiler::FreeBlockListNode(Compiler::BlockListNode* node)
{
    node->m_next             = impBlockListNodeFreeList;
    impBlockListNodeFreeList = node;
}

void Compiler::impWalkSpillCliqueFromPred(BasicBlock* block, SpillCliqueWalker* callback)
{
    bool toDo = true;

    noway_assert(!fgComputePredsDone);
    if (!fgCheapPredsValid)
    {
        fgComputeCheapPreds();
    }

    BlockListNode* succCliqueToDo = nullptr;
    BlockListNode* predCliqueToDo = new (this) BlockListNode(block);
    while (toDo)
    {
        toDo = false;
        // Look at the successors of every member of the predecessor to-do list.
        while (predCliqueToDo != nullptr)
        {
            BlockListNode* node = predCliqueToDo;
            predCliqueToDo      = node->m_next;
            BasicBlock* blk     = node->m_blk;
            FreeBlockListNode(node);

            const unsigned numSuccs = blk->NumSucc();
            for (unsigned succNum = 0; succNum < numSuccs; succNum++)
            {
                BasicBlock* succ = blk->GetSucc(succNum);
                // If it's not already in the clique, add it, and also add it
                // as a member of the successor "toDo" set.
                if (impAddSpillCliqueMember(SpillCliqueSucc, succ))
                {
                    callback->Visit(SpillCliqueSucc, succ);
                    succCliqueToDo = new (this) BlockListNode(succ, succCliqueToDo);
                    toDo           = true;
                }
            }
        }
        // Look at the predecessors of every member of the successor to-do list.
        while (succCliqueToDo != nullptr)
        {
            BlockListNode* node = succCliqueToDo;
            succCliqueToDo      = node->m_next;
            BasicBlock* blk     = node->m_blk;
            FreeBlockListNode(node);

            for (BasicBlockList* pred = blk->bbCheapPreds; pred != nullptr; pred = pred->next)
            {
                BasicBlock* predBlock = pred->block;
                // If it's not already in the clique, add it, and also add it
                // as a member of the predecessor "toDo" set.
                if (impAddSpillCliqueMember(SpillCliquePred, predBlock))
                {
                    callback->Visit(SpillCliquePred, predBlock);
                    predCliqueToDo = new (this) BlockListNode(predBlock, predCliqueToDo);
                    toDo           = true;
                }
            }
        }
    }

    // If this fails, it means we didn't walk the spill clique properly and somehow managed
    // miss walking back to include the predecessor we started from.
    // This most likely cause: missing or out of date bbPreds
    assert(impIsSpillCliqueMember(SpillCliquePred, block));
}

void Compiler::impSetSpillCliqueState(BasicBlock* block, ImportSpillCliqueState* state)
{
    class SetSpillCliqueState : public SpillCliqueWalker
    {
        ImportSpillCliqueState* const m_state;

    public:
        SetSpillCliqueState(ImportSpillCliqueState* state) : m_state(state)
        {
        }

        void Visit(SpillCliqueDir dir, BasicBlock* block) override
        {
            if (dir == SpillCliqueSucc)
            {
                assert(block->bbEntryState == nullptr);

                block->bbEntryState = m_state;
            }
            else
            {
                assert(dir == SpillCliquePred);
                assert(block->bbExitState == nullptr);

                block->bbExitState = m_state;
            }
        }
    } callback(state);

    // We do *NOT* need to reset impSpillCliqueMembers because a block can only be the predecessor
    // to one spill clique, and similarly can only be the sucessor to one spill clique.
    impWalkSpillCliqueFromPred(block, &callback);
}

void Compiler::impReimportSpillClique(BasicBlock* block)
{
    class ReimportSpillClique : public SpillCliqueWalker
    {
        Compiler*   m_compiler;
        BasicBlock* m_currentBlock;

    public:
        ReimportSpillClique(Compiler* compiler, BasicBlock* currentBlock)
            : m_compiler(compiler), m_currentBlock(currentBlock)
        {
        }

        void Visit(SpillCliqueDir dir, BasicBlock* block) override
        {
            if ((block->bbFlags & BBF_IMPORTED) == 0)
            {
                // The block isn't yet imported so there's no need to re-import.
                return;
            }

            // If it's already imported it cannot be pending.
            assert(!m_compiler->impIsPendingBlockMember(block));

            if ((block == m_currentBlock) && (dir == SpillCliquePred))
            {
                // The current block, which triggered re-importing, does not need
                // to be re-imported unless it is a successor (e.g. current block
                // is reached with a FLOAT on the stack but then it pushes DOUBLE
                // and loops back to itself).
                return;
            }

            JITDUMP(FMT_BB " will be reimported\n", block->bbNum);

            block->bbFlags &= ~BBF_IMPORTED;
            m_compiler->impPushPendingBlock(block);
        }
    } callback(this, block);

    // If we get here, it is because this block is already part of a spill clique
    // and one predecessor had an outgoing live stack slot of type int, and this
    // block has an outgoing live stack slot of type native int.
    // We need to reset these before traversal because they have already been set
    // by the previous walk to determine all the members of the spill clique.
    impInlineRoot()->impSpillCliqueMembers.Reset();

    impWalkSpillCliqueFromPred(block, &callback);
}

// Set the current state to the state at the start of the basic block
void Compiler::impSetCurrentState(BasicBlock* block)
{
    if (block->bbEntryState == nullptr)
    {
        verCurrentState.esStackDepth = 0;
        return;
    }

    if (block->bbEntryState->HasCatchArg())
    {
        verCurrentState.esStackDepth          = 1;
        verCurrentState.esStack[0].val        = impNewCatchArg();
        verCurrentState.esStack[0].seTypeInfo = typeInfo(TI_REF, block->bbEntryState->GetCatchArgType());
        return;
    }

    verCurrentState.esStackDepth = block->bbEntryState->GetSpillTempCount();

    for (unsigned i = 0; i < verCurrentState.esStackDepth; i++)
    {
        unsigned   lclNum = block->bbEntryState->GetSpillTempBaseLclNum() + i;
        LclVarDsc* lcl    = lvaGetDesc(lclNum);

        verCurrentState.esStack[i].val        = gtNewLclvNode(lclNum, lcl->GetType());
        verCurrentState.esStack[i].seTypeInfo = lcl->lvImpTypeInfo;
    }
}

unsigned BasicBlock::bbStackDepthOnEntry()
{
    return (bbEntryState == nullptr) ? 0 : bbEntryState->GetStackDepth();
}

Compiler* Compiler::impInlineRoot()
{
    return (impInlineInfo == nullptr) ? this : impInlineInfo->InlinerCompiler;
}

bool Compiler::impIsSpillCliqueMember(SpillCliqueDir dir, BasicBlock* block)
{
    uint8_t state = impInlineRoot()->impSpillCliqueMembers.Get(block->bbInd());
    uint8_t bit   = 1 << dir;

    return (state & bit) != 0;
}

bool Compiler::impAddSpillCliqueMember(SpillCliqueDir dir, BasicBlock* block)
{
    uint8_t& state = impInlineRoot()->impSpillCliqueMembers.GetRef(block->bbInd());
    uint8_t  bit   = 1 << dir;

    if ((state & bit) != 0)
    {
        return false;
    }

    state |= bit;
    return true;
}

/*****************************************************************************
 *
 *  Convert the instrs ("import") into our internal format (trees). The
 *  basic flowgraph has already been constructed and is passed in.
 */

void Compiler::impImport()
{
#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In impImport() for %s\n", info.compFullName);
    }
#endif

    Compiler* inlineRoot = impInlineRoot();

    if (info.compMaxStack <= SMALL_STACK_SIZE)
    {
        impStkSize = SMALL_STACK_SIZE;
    }
    else
    {
        impStkSize = info.compMaxStack;
    }

    verCurrentState.esStackDepth = 0;

    if (this == inlineRoot)
    {
        // Allocate the stack contents
        verCurrentState.esStack = new (this, CMK_ImpStack) StackEntry[impStkSize];
    }
    else
    {
        // This is the inlinee compiler, steal the stack from the inliner compiler
        // (after ensuring that it is large enough).
        if (inlineRoot->impStkSize < impStkSize)
        {
            inlineRoot->impStkSize              = impStkSize;
            inlineRoot->verCurrentState.esStack = new (this, CMK_ImpStack) StackEntry[impStkSize];
        }

        verCurrentState.esStack = inlineRoot->verCurrentState.esStack;
    }

    assert(fgFirstBB->bbEntryState == nullptr);

    // Initialize stuff related to figuring "spill cliques" (see spec comment for impGetSpillTmpBase).
    if (this == inlineRoot) // These are only used on the root of the inlining tree.
    {
        // We have initialized these previously, but to size 0.  Make them larger.
        impPendingBlockMembers.Init(getAllocator(), fgBBNumMax * 2);
        impSpillCliqueMembers.Init(getAllocator(), fgBBNumMax * 2);
    }
    inlineRoot->impPendingBlockMembers.Reset(fgBBNumMax * 2);
    inlineRoot->impSpillCliqueMembers.Reset(fgBBNumMax * 2);
    impBlockListNodeFreeList = nullptr;
    INDEBUG(impLastILoffsStmt = nullptr;)
    impBoxTemp           = BAD_VAR_NUM;
    impPendingBlockStack = nullptr;

    // Skip leading internal blocks.
    // These can arise from needing a leading scratch BB, from EH normalization, and from OSR entry redirects.
    //
    // We expect a linear flow to the first non-internal block. But not necessarily straght-line flow.
    BasicBlock* entryBlock = fgFirstBB;

    while (entryBlock->bbFlags & BBF_INTERNAL)
    {
        JITDUMP("Marking leading BBF_INTERNAL block " FMT_BB " as BBF_IMPORTED\n", entryBlock->bbNum);
        entryBlock->bbFlags |= BBF_IMPORTED;

        if (entryBlock->bbJumpKind == BBJ_NONE)
        {
            entryBlock = entryBlock->bbNext;
        }
        else if (entryBlock->bbJumpKind == BBJ_ALWAYS)
        {
            // Only expected for OSR
            assert(opts.IsOSR());
            entryBlock = entryBlock->bbJumpDest;
        }
        else
        {
            assert(!"unexpected bbJumpKind in entry sequence");
        }
    }

    // Note for OSR we'd like to be able to verify this block must be
    // stack empty, but won't know that until we've imported...so instead
    // we'll BADCODE out if we mess up.
    //
    // (the concern here is that the runtime asks us to OSR a
    // different IL version than the one that matched the method that
    // triggered OSR).  This should not happen but I might have the
    // IL versioning stuff wrong.
    //
    // TODO: we also currently expect this block to be a join point,
    // which we should verify over when we find jump targets.
    impImportBlockPending(entryBlock);

    while (BasicBlock* block = impPopPendingBlock())
    {
        impImportBlock(block);

        if (compDonotInline())
        {
            return;
        }
    }

#ifdef DEBUG
    if (verbose && info.compXcptnsCount)
    {
        printf("\nAfter impImport() added block for try,catch,finally");
        fgDispBasicBlocks();
        printf("\n");
    }
#endif
}

GenTreeLclVar* Compiler::impIsAddressInLocal(GenTree* tree)
{
    if (!tree->TypeIs(TYP_BYREF, TYP_I_IMPL))
    {
        return nullptr;
    }

    while (tree->OperIs(GT_ADD, GT_SUB))
    {
        GenTree* op1 = tree->AsOp()->GetOp(0);
        GenTree* op2 = tree->AsOp()->GetOp(1);

        if (op2->IsIntCon())
        {
            tree = op1;
        }
        else if (op1->IsIntCon())
        {
            tree = op2;
        }
        else
        {
            return nullptr;
        }
    }

    if (!tree->OperIs(GT_ADDR))
    {
        return nullptr;
    }

    GenTree* location = tree->AsUnOp()->GetOp(0);

    while (GenTreeField* field = location->IsField())
    {
        GenTree* addr = field->GetAddr();

        if ((addr == nullptr) || !addr->OperIs(GT_ADDR))
        {
            return nullptr;
        }

        location = addr->AsUnOp()->GetOp(0);
    }

    return location->OperIs(GT_LCL_VAR) ? location->AsLclVar() : nullptr;
}

//------------------------------------------------------------------------
// impMakeDiscretionaryInlineObservations: make observations that help
// determine the profitability of a discretionary inline
//
// Arguments:
//    pInlineInfo -- InlineInfo for the inline, or null for the prejit root
//    inlineResult -- InlineResult accumulating information about this inline
//
// Notes:
//    If inlining or prejitting the root, this method also makes
//    various observations about the method that factor into inline
//    decisions. It sets `compNativeSizeEstimate` as a side effect.

void Compiler::impMakeDiscretionaryInlineObservations(InlineInfo* pInlineInfo, InlineResult* inlineResult)
{
    assert((pInlineInfo != nullptr && compIsForInlining()) || // Perform the actual inlining.
           (pInlineInfo == nullptr && !compIsForInlining())   // Calculate the static inlining hint for ngen.
           );

    // If we're really inlining, we should just have one result in play.
    assert((pInlineInfo == nullptr) || (inlineResult == pInlineInfo->inlineResult));

    // If this is a "forceinline" method, the JIT probably shouldn't have gone
    // to the trouble of estimating the native code size. Even if it did, it
    // shouldn't be relying on the result of this method.
    assert(inlineResult->GetObservation() == InlineObservation::CALLEE_IS_DISCRETIONARY_INLINE);

    // Note if the caller contains NEWOBJ or NEWARR.
    Compiler* rootCompiler = impInlineRoot();

    if ((rootCompiler->optMethodFlags & OMF_HAS_NEWARRAY) != 0)
    {
        inlineResult->Note(InlineObservation::CALLER_HAS_NEWARRAY);
    }

    if ((rootCompiler->optMethodFlags & OMF_HAS_NEWOBJ) != 0)
    {
        inlineResult->Note(InlineObservation::CALLER_HAS_NEWOBJ);
    }

    bool calleeIsStatic  = (info.compFlags & CORINFO_FLG_STATIC) != 0;
    bool isSpecialMethod = (info.compFlags & CORINFO_FLG_CONSTRUCTOR) != 0;

    if (isSpecialMethod)
    {
        if (calleeIsStatic)
        {
            inlineResult->Note(InlineObservation::CALLEE_IS_CLASS_CTOR);
        }
        else
        {
            inlineResult->Note(InlineObservation::CALLEE_IS_INSTANCE_CTOR);
        }
    }
    else if (!calleeIsStatic)
    {
        // Callee is an instance method.
        //
        // Check if the callee has the same 'this' as the root.
        if (pInlineInfo != nullptr)
        {
            GenTree* thisArg = pInlineInfo->iciCall->AsCall()->gtCallThisArg->GetNode();
            assert(thisArg);
            bool isSameThis = impIsThis(thisArg);
            inlineResult->NoteBool(InlineObservation::CALLSITE_IS_SAME_THIS, isSameThis);
        }
    }

    // Note if the callee's class is a promotable struct
    if ((info.compClassAttr & CORINFO_FLG_VALUECLASS) != 0)
    {
        assert(structPromotionHelper != nullptr);
        if (structPromotionHelper->CanPromoteStructType(info.compClassHnd))
        {
            inlineResult->Note(InlineObservation::CALLEE_CLASS_PROMOTABLE);
        }
    }

#ifdef FEATURE_SIMD

    // Note if this method is has SIMD args or return value
    if (pInlineInfo != nullptr && pInlineInfo->hasSIMDTypeArgLocalOrReturn)
    {
        inlineResult->Note(InlineObservation::CALLEE_HAS_SIMD);
    }

#endif // FEATURE_SIMD

    // Roughly classify callsite frequency.
    InlineCallsiteFrequency frequency = InlineCallsiteFrequency::UNUSED;

    // If this is a prejit root, or a maximally hot block...
    if ((pInlineInfo == nullptr) || (pInlineInfo->iciBlock->bbWeight >= BB_MAX_WEIGHT))
    {
        frequency = InlineCallsiteFrequency::HOT;
    }
    // No training data.  Look for loop-like things.
    // We consider a recursive call loop-like.  Do not give the inlining boost to the method itself.
    // However, give it to things nearby.
    else if ((pInlineInfo->iciBlock->bbFlags & BBF_BACKWARD_JUMP) &&
             (info.compMethodHnd != pInlineInfo->inlineCandidateInfo->ilCallerHandle))
    {
        frequency = InlineCallsiteFrequency::LOOP;
    }
    else if (pInlineInfo->iciBlock->hasProfileWeight() && (pInlineInfo->iciBlock->bbWeight > BB_ZERO_WEIGHT))
    {
        frequency = InlineCallsiteFrequency::WARM;
    }
    // Now modify the multiplier based on where we're called from.
    else if (pInlineInfo->iciBlock->isRunRarely() || ((info.compFlags & FLG_CCTOR) == FLG_CCTOR))
    {
        frequency = InlineCallsiteFrequency::RARE;
    }
    else
    {
        frequency = InlineCallsiteFrequency::BORING;
    }

    // Also capture the block weight of the call site.
    //
    // In the prejit root case, assume at runtime there might be a hot call site
    // for this method, so we won't prematurely conclude this method should never
    // be inlined.
    //
    BasicBlock::weight_t weight = 0;

    if (pInlineInfo != nullptr)
    {
        weight = pInlineInfo->iciBlock->bbWeight;
    }
    else
    {
        const float prejitHotCallerWeight = 1000000.0f;
        weight                            = prejitHotCallerWeight;
    }

    inlineResult->NoteInt(InlineObservation::CALLSITE_FREQUENCY, static_cast<int>(frequency));
    inlineResult->NoteInt(InlineObservation::CALLSITE_WEIGHT, (int)(weight));

    // If the call site has profile data, report the relative frequency of the site.
    //
    if ((pInlineInfo != nullptr) && pInlineInfo->iciBlock->hasProfileWeight())
    {
        double callSiteWeight = (double)pInlineInfo->iciBlock->bbWeight;
        double entryWeight    = (double)impInlineRoot()->fgFirstBB->bbWeight;

        assert(callSiteWeight >= 0);
        assert(entryWeight >= 0);

        if (entryWeight != 0)
        {
            inlineResult->NoteBool(InlineObservation::CALLSITE_HAS_PROFILE, true);

            double frequency = callSiteWeight / entryWeight;
            inlineResult->NoteDouble(InlineObservation::CALLSITE_PROFILE_FREQUENCY, frequency);
        }
    }
}

/*****************************************************************************
 This method makes STATIC inlining decision based on the IL code.
 It should not make any inlining decision based on the context.
 If forceInline is true, then the inlining decision should not depend on
 performance heuristics (code size, etc.).
 */

void Compiler::impCanInlineIL(CORINFO_METHOD_HANDLE fncHandle,
                              CORINFO_METHOD_INFO*  methInfo,
                              bool                  forceInline,
                              InlineResult*         inlineResult)
{
    unsigned codeSize = methInfo->ILCodeSize;

    // We shouldn't have made up our minds yet...
    assert(!inlineResult->IsDecided());

    if (methInfo->EHcount)
    {
        inlineResult->NoteFatal(InlineObservation::CALLEE_HAS_EH);
        return;
    }

    if ((methInfo->ILCode == nullptr) || (codeSize == 0))
    {
        inlineResult->NoteFatal(InlineObservation::CALLEE_HAS_NO_BODY);
        return;
    }

    // For now we don't inline varargs (import code can't handle it)

    if (methInfo->args.isVarArg())
    {
        inlineResult->NoteFatal(InlineObservation::CALLEE_HAS_MANAGED_VARARGS);
        return;
    }

    // Reject if it has too many locals.
    // This is currently an implementation limit due to fixed-size arrays in the
    // inline info, rather than a performance heuristic.

    inlineResult->NoteInt(InlineObservation::CALLEE_NUMBER_OF_LOCALS, methInfo->locals.numArgs);

    if (methInfo->locals.numArgs > MAX_INL_LCLS)
    {
        inlineResult->NoteFatal(InlineObservation::CALLEE_TOO_MANY_LOCALS);
        return;
    }

    // Make sure there aren't too many arguments.
    // This is currently an implementation limit due to fixed-size arrays in the
    // inline info, rather than a performance heuristic.

    inlineResult->NoteInt(InlineObservation::CALLEE_NUMBER_OF_ARGUMENTS, methInfo->args.numArgs);

    if (methInfo->args.numArgs > MAX_INL_ARGS)
    {
        inlineResult->NoteFatal(InlineObservation::CALLEE_TOO_MANY_ARGUMENTS);
        return;
    }

    // Note force inline state

    inlineResult->NoteBool(InlineObservation::CALLEE_IS_FORCE_INLINE, forceInline);

    // Note IL code size

    inlineResult->NoteInt(InlineObservation::CALLEE_IL_CODE_SIZE, codeSize);

    if (inlineResult->IsFailure())
    {
        return;
    }

    // Make sure maxstack is not too big

    inlineResult->NoteInt(InlineObservation::CALLEE_MAXSTACK, methInfo->maxStack);

    if (inlineResult->IsFailure())
    {
        return;
    }
}

/*****************************************************************************
 */

void Compiler::impCheckCanInline(GenTreeCall*           call,
                                 CORINFO_METHOD_HANDLE  fncHandle,
                                 unsigned               methAttr,
                                 CORINFO_CONTEXT_HANDLE exactContextHnd,
                                 InlineCandidateInfo**  ppInlineCandidateInfo,
                                 InlineResult*          inlineResult)
{
    // Either EE or JIT might throw exceptions below.
    // If that happens, just don't inline the method.

    struct Param
    {
        Compiler*              pThis;
        GenTreeCall*           call;
        CORINFO_METHOD_HANDLE  fncHandle;
        unsigned               methAttr;
        CORINFO_CONTEXT_HANDLE exactContextHnd;
        InlineResult*          result;
        InlineCandidateInfo**  ppInlineCandidateInfo;
    } param;
    memset(&param, 0, sizeof(param));

    param.pThis                 = this;
    param.call                  = call;
    param.fncHandle             = fncHandle;
    param.methAttr              = methAttr;
    param.exactContextHnd       = (exactContextHnd != nullptr) ? exactContextHnd : MAKE_METHODCONTEXT(fncHandle);
    param.result                = inlineResult;
    param.ppInlineCandidateInfo = ppInlineCandidateInfo;

    bool success = eeRunWithErrorTrap<Param>(
        [](Param* pParam) {
            uint32_t               dwRestrictions = 0;
            CorInfoInitClassResult initClassResult;

#ifdef DEBUG
            const char* methodName;
            const char* className;
            methodName = pParam->pThis->eeGetMethodName(pParam->fncHandle, &className);

            if (JitConfig.JitNoInline())
            {
                pParam->result->NoteFatal(InlineObservation::CALLEE_IS_JIT_NOINLINE);
                return;
            }
#endif

            /* Try to get the code address/size for the method */

            CORINFO_METHOD_INFO methInfo;
            if (!pParam->pThis->info.compCompHnd->getMethodInfo(pParam->fncHandle, &methInfo))
            {
                pParam->result->NoteFatal(InlineObservation::CALLEE_NO_METHOD_INFO);
                return;
            }

            bool forceInline;
            forceInline = !!(pParam->methAttr & CORINFO_FLG_FORCEINLINE);

            pParam->pThis->impCanInlineIL(pParam->fncHandle, &methInfo, forceInline, pParam->result);

            if (pParam->result->IsFailure())
            {
                assert(pParam->result->IsNever());
                return;
            }

            // Speculatively check if initClass() can be done.
            // If it can be done, we will try to inline the method.
            initClassResult =
                pParam->pThis->info.compCompHnd->initClass(nullptr /* field */, pParam->fncHandle /* method */,
                                                           pParam->exactContextHnd /* context */);

            if (initClassResult & CORINFO_INITCLASS_DONT_INLINE)
            {
                pParam->result->NoteFatal(InlineObservation::CALLSITE_CANT_CLASS_INIT);
                return;
            }

            CorInfoInline vmResult;
            vmResult = pParam->pThis->info.compCompHnd->canInline(pParam->pThis->info.compMethodHnd, pParam->fncHandle,
                                                                  &dwRestrictions);

            if (vmResult == INLINE_FAIL)
            {
                pParam->result->NoteFatal(InlineObservation::CALLSITE_IS_VM_NOINLINE);
            }
            else if (vmResult == INLINE_NEVER)
            {
                pParam->result->NoteFatal(InlineObservation::CALLEE_IS_VM_NOINLINE);
            }

            if (pParam->result->IsFailure())
            {
                // Make sure not to report this one.  It was already reported by the VM.
                pParam->result->SetReported();
                return;
            }

            // check for unsupported inlining restrictions
            assert((dwRestrictions & ~(INLINE_RESPECT_BOUNDARY | INLINE_NO_CALLEE_LDSTR | INLINE_SAME_THIS)) == 0);

            if (dwRestrictions & INLINE_SAME_THIS)
            {
                GenTree* thisArg = pParam->call->gtCallThisArg->GetNode();
                assert(thisArg);

                if (!pParam->pThis->impIsThis(thisArg))
                {
                    pParam->result->NoteFatal(InlineObservation::CALLSITE_REQUIRES_SAME_THIS);
                    return;
                }
            }

            /* Get the method properties */

            CORINFO_CLASS_HANDLE clsHandle;
            clsHandle = pParam->pThis->info.compCompHnd->getMethodClass(pParam->fncHandle);
            unsigned clsAttr;
            clsAttr = pParam->pThis->info.compCompHnd->getClassAttribs(clsHandle);

#ifdef DEBUG
            var_types fncRetType     = pParam->call->GetType();
            var_types fncRealRetType = JITtype2varType(methInfo.args.retType);

            assert((genActualType(fncRealRetType) == genActualType(fncRetType)) ||
                   // <BUGNUM> VSW 288602 </BUGNUM>
                   // In case of IJW, we allow to assign a native pointer to a BYREF.
                   (fncRetType == TYP_BYREF && methInfo.args.retType == CORINFO_TYPE_PTR) ||
                   (varTypeIsStruct(fncRetType) && (fncRealRetType == TYP_STRUCT)));
#endif

            // Allocate an InlineCandidateInfo structure,
            //
            // Or, reuse the existing GuardedDevirtualizationCandidateInfo,
            // which was pre-allocated to have extra room.
            //
            InlineCandidateInfo* pInfo;

            if (pParam->call->IsGuardedDevirtualizationCandidate())
            {
                pInfo = pParam->call->gtInlineCandidateInfo;
            }
            else
            {
                pInfo = new (pParam->pThis, CMK_Inlining) InlineCandidateInfo;

                // Null out bits we don't use when we're just inlining
                pInfo->guardedClassHandle  = nullptr;
                pInfo->guardedMethodHandle = nullptr;
                pInfo->stubAddr            = nullptr;
            }

            pInfo->methInfo                       = methInfo;
            pInfo->ilCallerHandle                 = pParam->pThis->info.compMethodHnd;
            pInfo->clsHandle                      = clsHandle;
            pInfo->exactContextHnd                = pParam->exactContextHnd;
            pInfo->retExprPlaceholder             = nullptr;
            pInfo->dwRestrictions                 = dwRestrictions;
            pInfo->preexistingSpillTemp           = BAD_VAR_NUM;
            pInfo->clsAttr                        = clsAttr;
            pInfo->methAttr                       = pParam->methAttr;
            pInfo->initClassResult                = initClassResult;
            pInfo->exactContextNeedsRuntimeLookup = false;

            // Note exactContextNeedsRuntimeLookup is reset later on,
            // over in impMarkInlineCandidate.

            *(pParam->ppInlineCandidateInfo) = pInfo;
        },
        &param);
    if (!success)
    {
        param.result->NoteFatal(InlineObservation::CALLSITE_COMPILATION_ERROR);
    }
}

//-----------------------------------------------------------------------------
// impInlineIsGuaranteedThisDerefBeforeAnySideEffects: Check if a dereference in
// the inlinee can guarantee that the "this" pointer is non-NULL.
//
// Arguments:
//    additionalTree - a tree to check for side effects
//    additionalCallArgs - a list of call args to check for side effects
//    dereferencedAddress - address expression being dereferenced
//
// Notes:
//    If we haven't hit a branch or a side effect, and we are dereferencing
//    from 'this' to access a field or make GTF_CALL_NULLCHECK call,
//    then we can avoid a separate null pointer check.
//
//    The importer stack and current statement list are searched for side effects.
//    Trees that have been popped of the stack but haven't been appended to the
//    statement list and have to be checked for side effects may be provided via
//    additionalTree and additionalCallArgs.
//
bool Compiler::impInlineIsGuaranteedThisDerefBeforeAnySideEffects(GenTree*          additionalTree,
                                                                  GenTreeCall::Use* additionalCallArgs,
                                                                  GenTree*          dereferencedAddress)
{
    assert(compIsForInlining());
    assert(opts.OptEnabled(CLFLG_INLINING));

    BasicBlock* block = compCurBB;

    if (block != fgFirstBB)
    {
        return false;
    }

    if (!impInlineInfo->IsThisParam(dereferencedAddress))
    {
        return false;
    }

    if ((additionalTree != nullptr) && GTF_GLOBALLY_VISIBLE_SIDE_EFFECTS(additionalTree->gtFlags))
    {
        return false;
    }

    for (GenTreeCall::Use& use : GenTreeCall::UseList(additionalCallArgs))
    {
        if (GTF_GLOBALLY_VISIBLE_SIDE_EFFECTS(use.GetNode()->gtFlags))
        {
            return false;
        }
    }

    for (Statement* stmt : StatementList(impStmtList))
    {
        GenTree* expr = stmt->GetRootNode();
        if (GTF_GLOBALLY_VISIBLE_SIDE_EFFECTS(expr->gtFlags))
        {
            return false;
        }
    }

    for (unsigned level = 0; level < verCurrentState.esStackDepth; level++)
    {
        unsigned stackTreeFlags = verCurrentState.esStack[level].val->gtFlags;
        if (GTF_GLOBALLY_VISIBLE_SIDE_EFFECTS(stackTreeFlags))
        {
            return false;
        }
    }

    return true;
}

//------------------------------------------------------------------------
// impMarkInlineCandidate: determine if this call can be subsequently inlined
//
// Arguments:
//    call -- call under scrutiny
//    exactContextHnd -- context handle for inlining
//    exactContextNeedsRuntimeLookup -- true if context required runtime lookup
//    callInfo -- call info from VM
//
// Notes:
//    Mostly a wrapper for impMarkInlineCandidateHelper that also undoes
//    guarded devirtualization for virtual calls where the method we'd
//    devirtualize to cannot be inlined.

void Compiler::impMarkInlineCandidate(GenTreeCall*           call,
                                      CORINFO_CONTEXT_HANDLE exactContextHnd,
                                      bool                   exactContextNeedsRuntimeLookup,
                                      CORINFO_CALL_INFO*     callInfo)
{
    // Do the actual evaluation
    impMarkInlineCandidateHelper(call, exactContextHnd, exactContextNeedsRuntimeLookup, callInfo);

    // If this call is an inline candidate or is not a guarded devirtualization
    // candidate, we're done.
    if (call->IsInlineCandidate() || !call->IsGuardedDevirtualizationCandidate())
    {
        return;
    }

    // If we can't inline the call we'd guardedly devirtualize to,
    // we undo the guarded devirtualization, as the benefit from
    // just guarded devirtualization alone is likely not worth the
    // extra jit time and code size.
    //
    // TODO: it is possibly interesting to allow this, but requires
    // fixes elsewhere too...
    JITDUMP("Revoking guarded devirtualization candidacy for call [%06u]: target method can't be inlined\n",
            dspTreeID(call));

    call->ClearGuardedDevirtualizationCandidate();

    // If we have a stub address, restore it back into the union that it shares
    // with the candidate info.
    if (call->IsVirtualStub())
    {
        JITDUMP("Restoring stub addr %p from guarded devirt candidate info\n",
                dspPtr(call->gtGuardedDevirtualizationCandidateInfo->stubAddr));
        call->gtStubCallStubAddr = call->gtGuardedDevirtualizationCandidateInfo->stubAddr;
    }
}

//------------------------------------------------------------------------
// impMarkInlineCandidateHelper: determine if this call can be subsequently
//     inlined
//
// Arguments:
//    callNode -- call under scrutiny
//    exactContextHnd -- context handle for inlining
//    exactContextNeedsRuntimeLookup -- true if context required runtime lookup
//    callInfo -- call info from VM
//
// Notes:
//    If callNode is an inline candidate, this method sets the flag
//    GTF_CALL_INLINE_CANDIDATE, and ensures that helper methods have
//    filled in the associated InlineCandidateInfo.
//
//    If callNode is not an inline candidate, and the reason is
//    something that is inherent to the method being called, the
//    method may be marked as "noinline" to short-circuit any
//    future assessments of calls to this method.

void Compiler::impMarkInlineCandidateHelper(GenTreeCall*           call,
                                            CORINFO_CONTEXT_HANDLE exactContextHnd,
                                            bool                   exactContextNeedsRuntimeLookup,
                                            CORINFO_CALL_INFO*     callInfo)
{
    // Let the strategy know there's another call
    impInlineRoot()->m_inlineStrategy->NoteCall();

    if (!opts.OptEnabled(CLFLG_INLINING))
    {
        /* XXX Mon 8/18/2008
         * This assert is misleading.  The caller does not ensure that we have CLFLG_INLINING set before
         * calling impMarkInlineCandidate.  However, if this assert trips it means that we're an inlinee and
         * CLFLG_MINOPT is set.  That doesn't make a lot of sense.  If you hit this assert, work back and
         * figure out why we did not set MAXOPT for this compile.
         */
        assert(!compIsForInlining());
        return;
    }

    InlineResult inlineResult(this, call, nullptr, "impMarkInlineCandidate");

    // Don't inline if not optimizing root method
    if (opts.compDbgCode)
    {
        inlineResult.NoteFatal(InlineObservation::CALLER_DEBUG_CODEGEN);
        return;
    }

    // Don't inline if inlining into this method is disabled.
    if (impInlineRoot()->m_inlineStrategy->IsInliningDisabled())
    {
        inlineResult.NoteFatal(InlineObservation::CALLER_IS_JIT_NOINLINE);
        return;
    }

    // Don't inline into callers that use the NextCallReturnAddress intrinsic.
    if (info.compHasNextCallRetAddr)
    {
        inlineResult.NoteFatal(InlineObservation::CALLER_USES_NEXT_CALL_RET_ADDR);
        return;
    }

    // Inlining candidate determination needs to honor only IL tail prefix.
    // Inlining takes precedence over implicit tail call optimization (if the call is not directly recursive).
    if (call->IsTailPrefixedCall())
    {
        inlineResult.NoteFatal(InlineObservation::CALLSITE_EXPLICIT_TAIL_PREFIX);
        return;
    }

    // Tail recursion elimination takes precedence over inlining.
    // TODO: We may want to do some of the additional checks from fgMorphCall
    // here to reduce the chance we don't inline a call that won't be optimized
    // as a fast tail call or turned into a loop.
    if (gtIsRecursiveCall(call) && call->IsImplicitTailCall())
    {
        inlineResult.NoteFatal(InlineObservation::CALLSITE_IMPLICIT_REC_TAIL_CALL);
        return;
    }

    if (call->IsVirtual())
    {
        // Allow guarded devirt calls to be treated as inline candidates,
        // but reject all other virtual calls.
        if (!call->IsGuardedDevirtualizationCandidate())
        {
            inlineResult.NoteFatal(InlineObservation::CALLSITE_IS_NOT_DIRECT);
            return;
        }
    }

    /* Ignore helper calls */

    if (call->gtCallType == CT_HELPER)
    {
        assert(!call->IsGuardedDevirtualizationCandidate());
        inlineResult.NoteFatal(InlineObservation::CALLSITE_IS_CALL_TO_HELPER);
        return;
    }

    /* Ignore indirect calls */
    if (call->gtCallType == CT_INDIRECT)
    {
        inlineResult.NoteFatal(InlineObservation::CALLSITE_IS_NOT_DIRECT_MANAGED);
        return;
    }

    /* I removed the check for BBJ_THROW.  BBJ_THROW is usually marked as rarely run.  This more or less
     * restricts the inliner to non-expanding inlines.  I removed the check to allow for non-expanding
     * inlining in throw blocks.  I should consider the same thing for catch and filter regions. */

    CORINFO_METHOD_HANDLE fncHandle;
    unsigned              methAttr;

    if (call->IsGuardedDevirtualizationCandidate())
    {
        fncHandle = call->gtGuardedDevirtualizationCandidateInfo->guardedMethodHandle;
        methAttr  = info.compCompHnd->getMethodAttribs(fncHandle);
    }
    else
    {
        fncHandle = call->gtCallMethHnd;

        // Reuse method flags from the original callInfo if possible
        if (fncHandle == callInfo->hMethod)
        {
            methAttr = callInfo->methodFlags;
        }
        else
        {
            methAttr = info.compCompHnd->getMethodAttribs(fncHandle);
        }
    }

#ifdef DEBUG
    if (compStressCompile(STRESS_FORCE_INLINE, 0))
    {
        methAttr |= CORINFO_FLG_FORCEINLINE;
    }
#endif

    // Check for COMPlus_AggressiveInlining
    if (compDoAggressiveInlining)
    {
        methAttr |= CORINFO_FLG_FORCEINLINE;
    }

    if (!(methAttr & CORINFO_FLG_FORCEINLINE))
    {
        /* Don't bother inline blocks that are in the filter region */
        if (bbInCatchHandlerILRange(compCurBB))
        {
#ifdef DEBUG
            if (verbose)
            {
                printf("\nWill not inline blocks that are in the catch handler region\n");
            }

#endif

            inlineResult.NoteFatal(InlineObservation::CALLSITE_IS_WITHIN_CATCH);
            return;
        }

        if (bbInFilterILRange(compCurBB))
        {
#ifdef DEBUG
            if (verbose)
            {
                printf("\nWill not inline blocks that are in the filter region\n");
            }
#endif

            inlineResult.NoteFatal(InlineObservation::CALLSITE_IS_WITHIN_FILTER);
            return;
        }
    }

    /* Check if we tried to inline this method before */

    if (methAttr & CORINFO_FLG_DONT_INLINE)
    {
        inlineResult.NoteFatal(InlineObservation::CALLEE_IS_NOINLINE);
        return;
    }

    /* Cannot inline synchronized methods */

    if (methAttr & CORINFO_FLG_SYNCH)
    {
        inlineResult.NoteFatal(InlineObservation::CALLEE_IS_SYNCHRONIZED);
        return;
    }

    /* Check legality of PInvoke callsite (for inlining of marshalling code) */

    if (methAttr & CORINFO_FLG_PINVOKE)
    {
        // See comment in impCheckForPInvokeCall
        BasicBlock* block = compIsForInlining() ? impInlineInfo->iciBlock : compCurBB;
        if (!impCanPInvokeInlineCallSite(block))
        {
            inlineResult.NoteFatal(InlineObservation::CALLSITE_PINVOKE_EH);
            return;
        }
    }

    InlineCandidateInfo* inlineCandidateInfo = nullptr;
    impCheckCanInline(call, fncHandle, methAttr, exactContextHnd, &inlineCandidateInfo, &inlineResult);

    if (inlineResult.IsFailure())
    {
        return;
    }

    // The old value should be null OR this call should be a guarded devirtualization candidate.
    assert((call->gtInlineCandidateInfo == nullptr) || call->IsGuardedDevirtualizationCandidate());

    // The new value should not be null.
    assert(inlineCandidateInfo != nullptr);
    inlineCandidateInfo->exactContextNeedsRuntimeLookup = exactContextNeedsRuntimeLookup;
    call->gtInlineCandidateInfo                         = inlineCandidateInfo;

    // Mark the call node as inline candidate.
    call->gtFlags |= GTF_CALL_INLINE_CANDIDATE;

    // Let the strategy know there's another candidate.
    impInlineRoot()->m_inlineStrategy->NoteCandidate();

    // Since we're not actually inlining yet, and this call site is
    // still just an inline candidate, there's nothing to report.
    inlineResult.SetReported();
}

/******************************************************************************/
// Returns true if the given intrinsic will be implemented by target-specific
// instructions

bool Compiler::IsTargetIntrinsic(NamedIntrinsic intrinsicName)
{
#if defined(TARGET_XARCH)
    switch (intrinsicName)
    {
        // AMD64/x86 has SSE2 instructions to directly compute sqrt/abs and SSE4.1
        // instructions to directly compute round/ceiling/floor.

        case NI_System_Math_Abs:
        case NI_System_Math_Sqrt:
            return true;

        case NI_System_Math_Ceiling:
        case NI_System_Math_Floor:
        case NI_System_Math_Round:
            return compOpportunisticallyDependsOn(InstructionSet_SSE41);

        case NI_System_Math_FusedMultiplyAdd:
        {
            // AMD64/x86 has FMA3 instructions to directly compute fma. However, in
            // the scenario where it is supported we should have generated GT_HWINTRINSIC
            // nodes in place of the GT_INTRINSIC node.

            assert(!compIsaSupportedDebugOnly(InstructionSet_FMA));
            return false;
        }

        default:
            return false;
    }
#elif defined(TARGET_ARM64)
    switch (intrinsicName)
    {
        case NI_System_Math_Abs:
        case NI_System_Math_Ceiling:
        case NI_System_Math_Floor:
        case NI_System_Math_Round:
        case NI_System_Math_Sqrt:
            return true;

        case NI_System_Math_FusedMultiplyAdd:
        {
            // ARM64 has AdvSimd instructions to directly compute fma. However, in
            // the scenario where it is supported we should have generated GT_HWINTRINSIC
            // nodes in place of the GT_INTRINSIC node.

            assert(!compIsaSupportedDebugOnly(InstructionSet_AdvSimd));
            return false;
        }

        default:
            return false;
    }
#elif defined(TARGET_ARM)
    switch (intrinsicName)
    {
        case NI_System_Math_Abs:
        case NI_System_Math_Round:
        case NI_System_Math_Sqrt:
            return true;

        default:
            return false;
    }
#else
    // TODO: This portion of logic is not implemented for other arch.
    // The reason for returning true is that on all other arch the only intrinsic
    // enabled are target intrinsics.
    return true;
#endif
}

/******************************************************************************/
// Returns true if the given intrinsic will be implemented by calling System.Math
// methods.

bool Compiler::IsIntrinsicImplementedByUserCall(NamedIntrinsic intrinsicName)
{
    // Currently, if a math intrinsic is not implemented by target-specific
    // instructions, it will be implemented by a System.Math call. In the
    // future, if we turn to implementing some of them with helper calls,
    // this predicate needs to be revisited.
    return !IsTargetIntrinsic(intrinsicName);
}

bool Compiler::IsMathIntrinsic(NamedIntrinsic intrinsicName)
{
    switch (intrinsicName)
    {
        case NI_System_Math_Abs:
        case NI_System_Math_Acos:
        case NI_System_Math_Acosh:
        case NI_System_Math_Asin:
        case NI_System_Math_Asinh:
        case NI_System_Math_Atan:
        case NI_System_Math_Atanh:
        case NI_System_Math_Atan2:
        case NI_System_Math_Cbrt:
        case NI_System_Math_Ceiling:
        case NI_System_Math_Cos:
        case NI_System_Math_Cosh:
        case NI_System_Math_Exp:
        case NI_System_Math_Floor:
        case NI_System_Math_FMod:
        case NI_System_Math_FusedMultiplyAdd:
        case NI_System_Math_ILogB:
        case NI_System_Math_Log:
        case NI_System_Math_Log2:
        case NI_System_Math_Log10:
        case NI_System_Math_Pow:
        case NI_System_Math_Round:
        case NI_System_Math_Sin:
        case NI_System_Math_Sinh:
        case NI_System_Math_Sqrt:
        case NI_System_Math_Tan:
        case NI_System_Math_Tanh:
        {
            assert((intrinsicName > NI_SYSTEM_MATH_START) && (intrinsicName < NI_SYSTEM_MATH_END));
            return true;
        }

        default:
        {
            assert((intrinsicName < NI_SYSTEM_MATH_START) || (intrinsicName > NI_SYSTEM_MATH_END));
            return false;
        }
    }
}

bool Compiler::IsMathIntrinsic(GenTree* tree)
{
    return (tree->OperGet() == GT_INTRINSIC) && IsMathIntrinsic(tree->AsIntrinsic()->gtIntrinsicName);
}

//------------------------------------------------------------------------
// impDevirtualizeCall: Attempt to change a virtual vtable call into a
//   normal call
//
// Arguments:
//     call -- the call node to examine/modify
//     method   -- [IN/OUT] the method handle for call. Updated iff call devirtualized.
//     methodFlags -- [IN/OUT] flags for the method to call. Updated iff call devirtualized.
//     pContextHandle -- [IN/OUT] context handle for the call. Updated iff call devirtualized.
//     pExactContextHandle -- [OUT] updated context handle iff call devirtualized
//     isLateDevirtualization -- if devirtualization is happening after importation
//     isExplicitTailCalll -- [IN] true if we plan on using an explicit tail call
//     ilOffset -- IL offset of the call
//
// Notes:
//     Virtual calls in IL will always "invoke" the base class method.
//
//     This transformation looks for evidence that the type of 'this'
//     in the call is exactly known, is a final class or would invoke
//     a final method, and if that and other safety checks pan out,
//     modifies the call and the call info to create a direct call.
//
//     This transformation is initially done in the importer and not
//     in some subsequent optimization pass because we want it to be
//     upstream of inline candidate identification.
//
//     However, later phases may supply improved type information that
//     can enable further devirtualization. We currently reinvoke this
//     code after inlining, if the return value of the inlined call is
//     the 'this obj' of a subsequent virtual call.
//
//     If devirtualization succeeds and the call's this object is the
//     result of a box, the jit will ask the EE for the unboxed entry
//     point. If this exists, the jit will see if it can rework the box
//     to instead make a local copy. If that is doable, the call is
//     updated to invoke the unboxed entry on the local copy.
//
//     When guarded devirtualization is enabled, this method will mark
//     calls as guarded devirtualization candidates, if the type of `this`
//     is not exactly known, and there is a plausible guess for the type.

void Compiler::impDevirtualizeCall(GenTreeCall*            call,
                                   CORINFO_METHOD_HANDLE*  method,
                                   unsigned*               methodFlags,
                                   CORINFO_CONTEXT_HANDLE* pContextHandle,
                                   CORINFO_CONTEXT_HANDLE* pExactContextHandle,
                                   bool                    isLateDevirtualization,
                                   bool                    isExplicitTailCall,
                                   IL_OFFSETX              ilOffset)
{
    assert(call != nullptr);
    assert(method != nullptr);
    assert(methodFlags != nullptr);
    assert(pContextHandle != nullptr);

    // This should be a virtual vtable or virtual stub call.
    assert(call->IsVirtual());

    // Possibly instrument, if not optimizing.
    //
    if (opts.OptimizationDisabled() && (call->gtCallType != CT_INDIRECT))
    {
        // During importation, optionally flag this block as one that
        // contains calls requiring class profiling. Ideally perhaps
        // we'd just keep track of the calls themselves, so we don't
        // have to search for them later.
        //
        if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_BBINSTR) && !opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT) &&
            (JitConfig.JitClassProfiling() > 0) && !isLateDevirtualization)
        {
            JITDUMP("\n ... marking [%06u] in " FMT_BB " for class profile instrumentation\n", dspTreeID(call),
                    compCurBB->bbNum);
            ClassProfileCandidateInfo* pInfo = new (this, CMK_Inlining) ClassProfileCandidateInfo;

            // Record some info needed for the class profiling probe.
            //
            pInfo->ilOffset   = ilOffset;
            pInfo->probeIndex = info.compClassProbeCount++;
            pInfo->stubAddr   = call->gtStubCallStubAddr;

            // note this overwrites gtCallStubAddr, so it needs to be undone
            // during the instrumentation phase, or we won't generate proper
            // code for vsd calls.
            //
            call->gtClassProfileCandidateInfo = pInfo;

            // Flag block as needing scrutiny
            //
            compCurBB->bbFlags |= BBF_HAS_CLASS_PROFILE;
        }

        return;
    }

#if defined(DEBUG)
    // Bail if devirt is disabled.
    if (JitConfig.JitEnableDevirtualization() == 0)
    {
        return;
    }

    // Optionally, print info on devirtualization
    Compiler* const rootCompiler = impInlineRoot();
    const bool      doPrint      = JitConfig.JitPrintDevirtualizedMethods().contains(rootCompiler->info.compMethodName,
                                                                           rootCompiler->info.compClassName,
                                                                           &rootCompiler->info.compMethodInfo->args);

#endif // DEBUG

    // Fetch information about the virtual method we're calling.
    CORINFO_METHOD_HANDLE baseMethod        = *method;
    unsigned              baseMethodAttribs = *methodFlags;

    if (baseMethodAttribs == 0)
    {
        // For late devirt we may not have method attributes, so fetch them.
        baseMethodAttribs = info.compCompHnd->getMethodAttribs(baseMethod);
    }
    else
    {
#if defined(DEBUG)
        // Validate that callInfo has up to date method flags
        const DWORD freshBaseMethodAttribs = info.compCompHnd->getMethodAttribs(baseMethod);

        // All the base method attributes should agree, save that
        // CORINFO_FLG_DONT_INLINE may have changed from 0 to 1
        // because of concurrent jitting activity.
        //
        // Note we don't look at this particular flag bit below, and
        // later on (if we do try and inline) we will rediscover why
        // the method can't be inlined, so there's no danger here in
        // seeing this particular flag bit in different states between
        // the cached and fresh values.
        if ((freshBaseMethodAttribs & ~CORINFO_FLG_DONT_INLINE) != (baseMethodAttribs & ~CORINFO_FLG_DONT_INLINE))
        {
            assert(!"mismatched method attributes");
        }
#endif // DEBUG
    }

    // In R2R mode, we might see virtual stub calls to
    // non-virtuals. For instance cases where the non-virtual method
    // is in a different assembly but is called via CALLVIRT. For
    // verison resilience we must allow for the fact that the method
    // might become virtual in some update.
    //
    // In non-R2R modes CALLVIRT <nonvirtual> will be turned into a
    // regular call+nullcheck upstream, so we won't reach this
    // point.
    if ((baseMethodAttribs & CORINFO_FLG_VIRTUAL) == 0)
    {
        assert(call->IsVirtualStub());
        assert(opts.IsReadyToRun());
        JITDUMP("\nimpDevirtualizeCall: [R2R] base method not virtual, sorry\n");
        return;
    }

    // See what we know about the type of 'this' in the call.
    GenTree*             thisObj       = call->gtCallThisArg->GetNode()->gtEffectiveVal(false);
    GenTree*             actualThisObj = nullptr;
    bool                 isExact       = false;
    bool                 objIsNonNull  = false;
    CORINFO_CLASS_HANDLE objClass      = gtGetClassHandle(thisObj, &isExact, &objIsNonNull);

    // Bail if we know nothing.
    if (objClass == nullptr)
    {
        JITDUMP("\nimpDevirtualizeCall: no type available (op=%s)\n", GenTree::OpName(thisObj->OperGet()));
        return;
    }

    // Fetch information about the class that introduced the virtual method.
    CORINFO_CLASS_HANDLE baseClass        = info.compCompHnd->getMethodClass(baseMethod);
    const DWORD          baseClassAttribs = info.compCompHnd->getClassAttribs(baseClass);

    // Is the call an interface call?
    const bool isInterface = (baseClassAttribs & CORINFO_FLG_INTERFACE) != 0;

    // If the objClass is sealed (final), then we may be able to devirtualize.
    const DWORD objClassAttribs = info.compCompHnd->getClassAttribs(objClass);
    const bool  objClassIsFinal = (objClassAttribs & CORINFO_FLG_FINAL) != 0;

#if defined(DEBUG)
    const char* callKind       = isInterface ? "interface" : "virtual";
    const char* objClassNote   = "[?]";
    const char* objClassName   = "?objClass";
    const char* baseClassName  = "?baseClass";
    const char* baseMethodName = "?baseMethod";

    if (verbose || doPrint)
    {
        objClassNote   = isExact ? " [exact]" : objClassIsFinal ? " [final]" : "";
        objClassName   = info.compCompHnd->getClassName(objClass);
        baseClassName  = info.compCompHnd->getClassName(baseClass);
        baseMethodName = eeGetMethodName(baseMethod, nullptr);

        if (verbose)
        {
            printf("\nimpDevirtualizeCall: Trying to devirtualize %s call:\n"
                   "    class for 'this' is %s%s (attrib %08x)\n"
                   "    base method is %s::%s\n",
                   callKind, objClassName, objClassNote, objClassAttribs, baseClassName, baseMethodName);
        }
    }
#endif // defined(DEBUG)

    // See if the jit's best type for `obj` is an interface.
    // See for instance System.ValueTuple`8::GetHashCode, where lcl 0 is System.IValueTupleInternal
    //   IL_021d:  ldloc.0
    //   IL_021e:  callvirt   instance int32 System.Object::GetHashCode()
    //
    // If so, we can't devirtualize, but we may be able to do guarded devirtualization.
    //
    if ((objClassAttribs & CORINFO_FLG_INTERFACE) != 0)
    {
        // Don't try guarded devirtualiztion when we're doing late devirtualization.
        //
        if (isLateDevirtualization)
        {
            JITDUMP("No guarded devirt during late devirtualization\n");
            return;
        }

        considerGuardedDevirtualization(call, ilOffset, isInterface, baseMethod, baseClass,
                                        pContextHandle DEBUGARG(objClass) DEBUGARG(objClassName));
        return;
    }

    // If we get this far, the jit has a lower bound class type for the `this` object being used for dispatch.
    // It may or may not know enough to devirtualize...
    if (isInterface)
    {
        assert(call->IsVirtualStub());
        JITDUMP("--- base class is interface\n");
    }

    // Fetch the method that would be called based on the declared type of 'this',
    // and prepare to fetch the method attributes.
    //
    CORINFO_DEVIRTUALIZATION_INFO dvInfo;
    dvInfo.virtualMethod = baseMethod;
    dvInfo.objClass      = objClass;
    dvInfo.context       = *pContextHandle;

    info.compCompHnd->resolveVirtualMethod(&dvInfo);

    CORINFO_METHOD_HANDLE  derivedMethod = dvInfo.devirtualizedMethod;
    CORINFO_CONTEXT_HANDLE exactContext  = dvInfo.exactContext;
    CORINFO_CLASS_HANDLE   derivedClass  = NO_CLASS_HANDLE;

    if (exactContext != nullptr)
    {
        // We currently expect the context to always be a class context.
        assert(((size_t)exactContext & CORINFO_CONTEXTFLAGS_MASK) == CORINFO_CONTEXTFLAGS_CLASS);
        derivedClass = (CORINFO_CLASS_HANDLE)((size_t)exactContext & ~CORINFO_CONTEXTFLAGS_MASK);
    }

    DWORD derivedMethodAttribs = 0;
    bool  derivedMethodIsFinal = false;
    bool  canDevirtualize      = false;

#if defined(DEBUG)
    const char* derivedClassName  = "?derivedClass";
    const char* derivedMethodName = "?derivedMethod";
    const char* note              = "inexact or not final";
#endif

    // If we failed to get a method handle, we can't directly devirtualize.
    //
    // This can happen when prejitting, if the devirtualization crosses
    // servicing bubble boundaries, or if objClass is a shared class.
    //
    if (derivedMethod == nullptr)
    {
        JITDUMP("--- no derived method\n");
    }
    else
    {
        // Fetch method attributes to see if method is marked final.
        derivedMethodAttribs = info.compCompHnd->getMethodAttribs(derivedMethod);
        derivedMethodIsFinal = ((derivedMethodAttribs & CORINFO_FLG_FINAL) != 0);

#if defined(DEBUG)
        if (isExact)
        {
            note = "exact";
        }
        else if (objClassIsFinal)
        {
            note = "final class";
        }
        else if (derivedMethodIsFinal)
        {
            note = "final method";
        }

        if (verbose || doPrint)
        {
            derivedMethodName = eeGetMethodName(derivedMethod, nullptr);
            derivedClassName  = eeGetClassName(derivedClass);
            if (verbose)
            {
                printf("    devirt to %s::%s -- %s\n", derivedClassName, derivedMethodName, note);
                gtDispTree(call);
            }
        }
#endif // defined(DEBUG)

        canDevirtualize = isExact || objClassIsFinal || (!isInterface && derivedMethodIsFinal);
    }

    // We still might be able to do a guarded devirtualization.
    // Note the call might be an interface call or a virtual call.
    //
    if (!canDevirtualize)
    {
        JITDUMP("    Class not final or exact%s\n", isInterface ? "" : ", and method not final");

        // Don't try guarded devirtualiztion if we're doing late devirtualization.
        //
        if (isLateDevirtualization)
        {
            JITDUMP("No guarded devirt during late devirtualization\n");
            return;
        }

        considerGuardedDevirtualization(call, ilOffset, isInterface, baseMethod, baseClass,
                                        pContextHandle DEBUGARG(objClass) DEBUGARG(objClassName));
        return;
    }

    // All checks done. Time to transform the call.
    assert(canDevirtualize);

    JITDUMP("    %s; can devirtualize\n", note);

    // See if the method we're devirtualizing to is an intrinsic.
    //
    if (derivedMethodAttribs & (CORINFO_FLG_JIT_INTRINSIC | CORINFO_FLG_INTRINSIC))
    {
        JITDUMP("!!! Devirt to intrinsic in %s, calling %s::%s\n", impInlineRoot()->info.compFullName, derivedClassName,
                derivedMethodName);
    }

    // Make the updates.
    call->gtFlags &= ~GTF_CALL_VIRT_VTABLE;
    call->gtFlags &= ~GTF_CALL_VIRT_STUB;
    call->gtCallMethHnd = derivedMethod;
    call->gtCallType    = CT_USER_FUNC;
    call->gtCallMoreFlags |= GTF_CALL_M_DEVIRTUALIZED;

    // Virtual calls include an implicit null check, which we may
    // now need to make explicit.
    if (!objIsNonNull)
    {
        call->gtFlags |= GTF_CALL_NULLCHECK;
    }

    // Clear the inline candidate info (may be non-null since
    // it's a union field used for other things by virtual
    // stubs)
    call->gtInlineCandidateInfo = nullptr;

#if defined(DEBUG)
    if (verbose)
    {
        printf("... after devirt...\n");
        gtDispTree(call);
    }

    if (doPrint)
    {
        printf("Devirtualized %s call to %s:%s; now direct call to %s:%s [%s]\n", callKind, baseClassName,
               baseMethodName, derivedClassName, derivedMethodName, note);
    }
#endif // defined(DEBUG)

    // If the 'this' object is a box, see if we can find the unboxed entry point for the call.
    if (thisObj->IsBox())
    {
        JITDUMP("Now have direct call to boxed entry point, looking for unboxed entry point\n");

        if (isExplicitTailCall)
        {
            JITDUMP("Call is an explicit tail call, we cannot perform an unbox\n");
            return;
        }

        // Note for some shared methods the unboxed entry point requires an extra parameter.
        bool                  requiresInstMethodTableArg = false;
        CORINFO_METHOD_HANDLE unboxedEntryMethod =
            info.compCompHnd->getUnboxedEntry(derivedMethod, &requiresInstMethodTableArg);

        if (unboxedEntryMethod != nullptr)
        {
            // Since the call is the only consumer of the box, we know the box can't escape
            // since it is being passed an interior pointer.
            //
            // So, revise the box to simply create a local copy, use the address of that copy
            // as the this pointer, and update the entry point to the unboxed entry.
            //
            // Ideally, we then inline the boxed method and and if it turns out not to modify
            // the copy, we can undo the copy too.
            if (requiresInstMethodTableArg)
            {
                // Perform a trial box removal and ask for the type handle tree.
                JITDUMP("Unboxed entry needs method table arg...\n");
                GenTree* methodTableArg = gtTryRemoveBoxUpstreamEffects(thisObj, BR_DONT_REMOVE_WANT_TYPE_HANDLE);

                if (methodTableArg != nullptr)
                {
                    // If that worked, turn the box into a copy to a local var
                    JITDUMP("Found suitable method table arg tree [%06u]\n", dspTreeID(methodTableArg));
                    GenTree* localCopyThis = gtTryRemoveBoxUpstreamEffects(thisObj, BR_MAKE_LOCAL_COPY);

                    if (localCopyThis != nullptr)
                    {
                        // Pass the local var as this and the type handle as a new arg
                        JITDUMP("Success! invoking unboxed entry point on local copy, and passing method table arg\n");
                        call->gtCallThisArg = gtNewCallArgs(localCopyThis);
                        call->gtCallMoreFlags |= GTF_CALL_M_UNBOXED;

                        // Prepend for R2L arg passing or empty L2R passing
                        if ((Target::g_tgtArgOrder == Target::ARG_ORDER_R2L) || (call->gtCallArgs == nullptr))
                        {
                            call->gtCallArgs = gtPrependNewCallArg(methodTableArg, call->gtCallArgs);
                        }
                        // Append for non-empty L2R
                        else
                        {
                            GenTreeCall::Use* beforeArg = call->gtCallArgs;
                            while (beforeArg->GetNext() != nullptr)
                            {
                                beforeArg = beforeArg->GetNext();
                            }

                            beforeArg->SetNext(gtNewCallArgs(methodTableArg));
                        }

                        call->gtCallMethHnd = unboxedEntryMethod;
                        derivedMethod       = unboxedEntryMethod;

                        // Method attributes will differ because unboxed entry point is shared
                        const DWORD unboxedMethodAttribs = info.compCompHnd->getMethodAttribs(unboxedEntryMethod);
                        JITDUMP("Updating method attribs from 0x%08x to 0x%08x\n", derivedMethodAttribs,
                                unboxedMethodAttribs);
                        derivedMethodAttribs = unboxedMethodAttribs;
                    }
                    else
                    {
                        JITDUMP("Sorry, failed to undo the box -- can't convert to local copy\n");
                    }
                }
                else
                {
                    JITDUMP("Sorry, failed to undo the box -- can't find method table arg\n");
                }
            }
            else
            {
                JITDUMP("Found unboxed entry point, trying to simplify box to a local copy\n");
                GenTree* localCopyThis = gtTryRemoveBoxUpstreamEffects(thisObj, BR_MAKE_LOCAL_COPY);

                if (localCopyThis != nullptr)
                {
                    JITDUMP("Success! invoking unboxed entry point on local copy\n");
                    call->gtCallThisArg = gtNewCallArgs(localCopyThis);
                    call->gtCallMethHnd = unboxedEntryMethod;
                    call->gtCallMoreFlags |= GTF_CALL_M_UNBOXED;
                    derivedMethod = unboxedEntryMethod;

#if FEATURE_TAILCALL_OPT
                    if (call->IsImplicitTailCall())
                    {
                        JITDUMP("Clearing the implicit tail call flag\n");

                        // If set, we clear the implicit tail call flag
                        // as we just introduced a new address taken local variable
                        //
                        call->gtCallMoreFlags &= ~GTF_CALL_M_IMPLICIT_TAILCALL;
                    }
#endif // FEATURE_TAILCALL_OPT
                }
                else
                {
                    JITDUMP("Sorry, failed to undo the box\n");
                }
            }
        }
        else
        {
            // Many of the low-level methods on value classes won't have unboxed entries,
            // as they need access to the type of the object.
            //
            // Note this may be a cue for us to stack allocate the boxed object, since
            // we probably know that these objects don't escape.
            JITDUMP("Sorry, failed to find unboxed entry point\n");
        }
    }

    // Need to update call info too.
    //
    *method      = derivedMethod;
    *methodFlags = derivedMethodAttribs;

    // Update context handle
    //
    *pContextHandle = MAKE_METHODCONTEXT(derivedMethod);

    // Update exact context handle.
    //
    if (pExactContextHandle != nullptr)
    {
        *pExactContextHandle = MAKE_CLASSCONTEXT(derivedClass);
    }

#ifdef FEATURE_READYTORUN_COMPILER
    if (opts.IsReadyToRun())
    {
        // For R2R, getCallInfo triggers bookkeeping on the zap
        // side so we need to call it here.
        //
        // First, cons up a suitable resolved token.
        CORINFO_RESOLVED_TOKEN derivedResolvedToken = {};

        derivedResolvedToken.tokenScope   = info.compCompHnd->getMethodModule(derivedMethod);
        derivedResolvedToken.tokenContext = *pContextHandle;
        derivedResolvedToken.token        = info.compCompHnd->getMethodDefFromMethod(derivedMethod);
        derivedResolvedToken.tokenType    = CORINFO_TOKENKIND_Method;
        derivedResolvedToken.hClass       = derivedClass;
        derivedResolvedToken.hMethod      = derivedMethod;

        // Look up the new call info.
        CORINFO_CALL_INFO derivedCallInfo;
        eeGetCallInfo(&derivedResolvedToken, nullptr, CORINFO_CALLINFO_ALLOWINSTPARAM, &derivedCallInfo);

        // Update the call.
        call->gtCallMoreFlags &= ~GTF_CALL_M_VIRTSTUB_REL_INDIRECT;
        call->gtCallMoreFlags &= ~GTF_CALL_M_R2R_REL_INDIRECT;
        call->setEntryPoint(derivedCallInfo.codePointerLookup.constLookup);
    }
#endif // FEATURE_READYTORUN_COMPILER
}

void Compiler::impLateDevirtualizeCall(GenTreeCall* call)
{
    JITDUMPTREE(call, "**** Late devirt opportunity\n");

    CORINFO_METHOD_HANDLE  method                 = call->gtCallMethHnd;
    unsigned               methodFlags            = 0;
    CORINFO_CONTEXT_HANDLE context                = nullptr;
    const bool             isLateDevirtualization = true;
    bool                   explicitTailCall       = (call->gtCallMoreFlags & GTF_CALL_M_EXPLICIT_TAILCALL) != 0;

    impDevirtualizeCall(call, &method, &methodFlags, &context, nullptr, isLateDevirtualization, explicitTailCall);
}

//------------------------------------------------------------------------
// impGetSpecialIntrinsicExactReturnType: Look for special cases where a call
//   to an intrinsic returns an exact type
//
// Arguments:
//     methodHnd -- handle for the special intrinsic method
//
// Returns:
//     Exact class handle returned by the intrinsic call, if known.
//     Nullptr if not known, or not likely to lead to beneficial optimization.

CORINFO_CLASS_HANDLE Compiler::impGetSpecialIntrinsicExactReturnType(CORINFO_METHOD_HANDLE methodHnd)
{
    JITDUMP("Special intrinsic: looking for exact type returned by %s\n", eeGetMethodFullName(methodHnd));

    CORINFO_CLASS_HANDLE result = nullptr;

    // See what intrinisc we have...
    const NamedIntrinsic ni = lookupNamedIntrinsic(methodHnd);
    switch (ni)
    {
        case NI_System_Collections_Generic_Comparer_get_Default:
        case NI_System_Collections_Generic_EqualityComparer_get_Default:
        {
            // Expect one class generic parameter; figure out which it is.
            CORINFO_SIG_INFO sig;
            info.compCompHnd->getMethodSig(methodHnd, &sig);
            assert(sig.sigInst.classInstCount == 1);
            CORINFO_CLASS_HANDLE typeHnd = sig.sigInst.classInst[0];
            assert(typeHnd != nullptr);

            // Lookup can incorrect when we have __Canon as it won't appear
            // to implement any interface types.
            //
            // And if we do not have a final type, devirt & inlining is
            // unlikely to result in much simplification.
            //
            // We can use CORINFO_FLG_FINAL to screen out both of these cases.
            const DWORD typeAttribs = info.compCompHnd->getClassAttribs(typeHnd);
            const bool  isFinalType = ((typeAttribs & CORINFO_FLG_FINAL) != 0);

            if (isFinalType)
            {
                if (ni == NI_System_Collections_Generic_EqualityComparer_get_Default)
                {
                    result = info.compCompHnd->getDefaultEqualityComparerClass(typeHnd);
                }
                else
                {
                    assert(ni == NI_System_Collections_Generic_Comparer_get_Default);
                    result = info.compCompHnd->getDefaultComparerClass(typeHnd);
                }
                JITDUMP("Special intrinsic for type %s: return type is %s\n", eeGetClassName(typeHnd),
                        result != nullptr ? eeGetClassName(result) : "unknown");
            }
            else
            {
                JITDUMP("Special intrinsic for type %s: type not final, so deferring opt\n", eeGetClassName(typeHnd));
            }

            break;
        }

        default:
        {
            JITDUMP("This special intrinsic not handled, sorry...\n");
            break;
        }
    }

    return result;
}

//------------------------------------------------------------------------
// impAllocateToken: create CORINFO_RESOLVED_TOKEN into jit-allocated memory and init it.
//
// Arguments:
//    token - init value for the allocated token.
//
// Return Value:
//    pointer to token into jit-allocated memory.
CORINFO_RESOLVED_TOKEN* Compiler::impAllocateToken(const CORINFO_RESOLVED_TOKEN& token)
{
    CORINFO_RESOLVED_TOKEN* memory = getAllocator(CMK_Unknown).allocate<CORINFO_RESOLVED_TOKEN>(1);
    *memory                        = token;
    return memory;
}

//------------------------------------------------------------------------
// SpillRetExprHelper: iterate through arguments tree and spill ret_expr to local variables.
//
class SpillRetExprHelper
{
public:
    SpillRetExprHelper(Compiler* comp) : comp(comp)
    {
    }

    void StoreRetExprResultsInArgs(GenTreeCall* call)
    {
        for (GenTreeCall::Use& use : call->Args())
        {
            comp->fgWalkTreePre(&use.NodeRef(), SpillRetExprVisitor, this);
        }

        if (call->gtCallThisArg != nullptr)
        {
            comp->fgWalkTreePre(&call->gtCallThisArg->NodeRef(), SpillRetExprVisitor, this);
        }
    }

private:
    static Compiler::fgWalkResult SpillRetExprVisitor(GenTree** pTree, Compiler::fgWalkData* fgWalkPre)
    {
        assert((pTree != nullptr) && (*pTree != nullptr));
        GenTree* tree = *pTree;
        if ((tree->gtFlags & GTF_CALL) == 0)
        {
            // Trees with ret_expr are marked as GTF_CALL.
            return Compiler::WALK_SKIP_SUBTREES;
        }
        if (tree->OperGet() == GT_RET_EXPR)
        {
            SpillRetExprHelper* walker = static_cast<SpillRetExprHelper*>(fgWalkPre->pCallbackData);
            walker->StoreRetExprAsLocalVar(pTree);
        }
        return Compiler::WALK_CONTINUE;
    }

    void StoreRetExprAsLocalVar(GenTree** pRetExpr)
    {
        GenTree* retExpr = *pRetExpr;
        assert(retExpr->OperGet() == GT_RET_EXPR);
        const unsigned tmp = comp->lvaGrabTemp(true DEBUGARG("spilling ret_expr"));
        JITDUMP("Storing return expression [%06u] to a local var V%02u.\n", comp->dspTreeID(retExpr), tmp);
        comp->impAssignTempGen(tmp, retExpr, Compiler::CHECK_SPILL_NONE);
        *pRetExpr = comp->gtNewLclvNode(tmp, retExpr->TypeGet());

        if (retExpr->TypeGet() == TYP_REF)
        {
            assert(comp->lvaTable[tmp].lvSingleDef == 0);
            comp->lvaTable[tmp].lvSingleDef = 1;
            JITDUMP("Marked V%02u as a single def temp\n", tmp);

            bool                 isExact   = false;
            bool                 isNonNull = false;
            CORINFO_CLASS_HANDLE retClsHnd = comp->gtGetClassHandle(retExpr, &isExact, &isNonNull);
            if (retClsHnd != nullptr)
            {
                comp->lvaSetClass(tmp, retClsHnd, isExact);
            }
        }
    }

private:
    Compiler* comp;
};

//------------------------------------------------------------------------
// addFatPointerCandidate: mark the call and the method, that they have a fat pointer candidate.
//                         Spill ret_expr in the call node, because they can't be cloned.
//
// Arguments:
//    call - fat calli candidate
//
void Compiler::addFatPointerCandidate(GenTreeCall* call)
{
    JITDUMP("Marking call [%06u] as fat pointer candidate\n", dspTreeID(call));
    setMethodHasFatPointer();
    call->SetFatPointerCandidate();
    SpillRetExprHelper helper(this);
    helper.StoreRetExprResultsInArgs(call);
}

//------------------------------------------------------------------------
// considerGuardedDevirtualization: see if we can profitably guess at the
//    class involved in an interface or virtual call.
//
// Arguments:
//
//    call - potential guarded devirtualization candidate
//    ilOffset - IL offset of the call instruction
//    isInterface - true if this is an interface call
//    baseMethod - target method of the call
//    baseClass - class that introduced the target method
//    pContextHandle - context handle for the call
//    objClass - class of 'this' in the call
//    objClassName - name of the obj Class
//
// Notes:
//    Consults with VM to see if there's a likely class at runtime,
//    if so, adds a candidate for guarded devirtualization.
//
void Compiler::considerGuardedDevirtualization(
    GenTreeCall*            call,
    IL_OFFSETX              ilOffset,
    bool                    isInterface,
    CORINFO_METHOD_HANDLE   baseMethod,
    CORINFO_CLASS_HANDLE    baseClass,
    CORINFO_CONTEXT_HANDLE* pContextHandle DEBUGARG(CORINFO_CLASS_HANDLE objClass) DEBUGARG(const char* objClassName))
{
#if defined(DEBUG)
    const char* callKind = isInterface ? "interface" : "virtual";
#endif

    JITDUMP("Considering guarded devirtualization\n");

    // See if there's a likely guess for the class.
    //
    const unsigned       likelihoodThreshold = isInterface ? 25 : 30;
    unsigned             likelihood          = 0;
    unsigned             numberOfClasses     = 0;
    CORINFO_CLASS_HANDLE likelyClass =
        info.compCompHnd->getLikelyClass(info.compMethodHnd, baseClass, ilOffset, &likelihood, &numberOfClasses);

    if (likelyClass == NO_CLASS_HANDLE)
    {
        JITDUMP("No likely class, sorry\n");
        return;
    }

    JITDUMP("Likely class for %p (%s) is %p (%s) [likelihood:%u classes seen:%u]\n", dspPtr(objClass), objClassName,
            likelyClass, eeGetClassName(likelyClass), likelihood, numberOfClasses);

    // Todo: a more advanced heuristic using likelihood, number of
    // classes, and the profile count for this block.
    //
    // For now we will guess if the likelihood is at least 25%/30% (intfc/virt), as studies
    // have shown this transformation should pay off even if we guess wrong sometimes.
    //
    if (likelihood < likelihoodThreshold)
    {
        JITDUMP("Not guessing for class; likelihood is below %s call threshold %u\n", callKind, likelihoodThreshold);
        return;
    }

    // Figure out which method will be called.
    //
    CORINFO_DEVIRTUALIZATION_INFO dvInfo;
    dvInfo.virtualMethod = baseMethod;
    dvInfo.objClass      = likelyClass;
    dvInfo.context       = *pContextHandle;

    const bool canResolve = info.compCompHnd->resolveVirtualMethod(&dvInfo);

    if (!canResolve)
    {
        JITDUMP("Can't figure out which method would be invoked, sorry\n");
        return;
    }

    CORINFO_METHOD_HANDLE likelyMethod = dvInfo.devirtualizedMethod;
    JITDUMP("%s call would invoke method %s\n", callKind, eeGetMethodName(likelyMethod, nullptr));

    // Add this as a potential candidate.
    //
    uint32_t const likelyMethodAttribs = info.compCompHnd->getMethodAttribs(likelyMethod);
    uint32_t const likelyClassAttribs  = info.compCompHnd->getClassAttribs(likelyClass);
    addGuardedDevirtualizationCandidate(call, likelyMethod, likelyClass, likelyMethodAttribs, likelyClassAttribs,
                                        likelihood);
}

//------------------------------------------------------------------------
// addGuardedDevirtualizationCandidate: potentially mark the call as a guarded
//    devirtualization candidate
//
// Notes:
//
// Call sites in rare or unoptimized code, and calls that require cookies are
// not marked as candidates.
//
// As part of marking the candidate, the code spills GT_RET_EXPRs anywhere in any
// child tree, because and we need to clone all these trees when we clone the call
// as part of guarded devirtualization, and these IR nodes can't be cloned.
//
// Arguments:
//    call - potential guarded devirtualization candidate
//    methodHandle - method that will be invoked if the class test succeeds
//    classHandle - class that will be tested for at runtime
//    methodAttr - attributes of the method
//    classAttr - attributes of the class
//    likelihood - odds that this class is the class seen at runtime
//
void Compiler::addGuardedDevirtualizationCandidate(GenTreeCall*          call,
                                                   CORINFO_METHOD_HANDLE methodHandle,
                                                   CORINFO_CLASS_HANDLE  classHandle,
                                                   unsigned              methodAttr,
                                                   unsigned              classAttr,
                                                   unsigned              likelihood)
{
    // This transformation only makes sense for virtual calls
    assert(call->IsVirtual());

    // Only mark calls if the feature is enabled.
    const bool isEnabled = JitConfig.JitEnableGuardedDevirtualization() > 0;

    if (!isEnabled)
    {
        JITDUMP("NOT Marking call [%06u] as guarded devirtualization candidate -- disabled by jit config\n",
                dspTreeID(call));
        return;
    }

    // Bail if not optimizing or the call site is very likely cold
    if (compCurBB->isRunRarely() || opts.OptimizationDisabled())
    {
        JITDUMP("NOT Marking call [%06u] as guarded devirtualization candidate -- rare / dbg / minopts\n",
                dspTreeID(call));
        return;
    }

    // CT_INDIRECT calls may use the cookie, bail if so...
    //
    // If transforming these provides a benefit, we could save this off in the same way
    // we save the stub address below.
    if ((call->gtCallType == CT_INDIRECT) && (call->AsCall()->gtCallCookie != nullptr))
    {
        JITDUMP("NOT Marking call [%06u] as guarded devirtualization candidate -- CT_INDIRECT with cookie\n",
                dspTreeID(call));
        return;
    }

#ifdef DEBUG

    // See if disabled by range
    //
    static ConfigMethodRange JitGuardedDevirtualizationRange;
    JitGuardedDevirtualizationRange.EnsureInit(JitConfig.JitGuardedDevirtualizationRange());
    assert(!JitGuardedDevirtualizationRange.Error());
    if (!JitGuardedDevirtualizationRange.Contains(impInlineRoot()->info.compMethodHash()))
    {
        JITDUMP("NOT Marking call [%06u] as guarded devirtualization candidate -- excluded by "
                "JitGuardedDevirtualizationRange",
                dspTreeID(call));
        return;
    }

#endif

    // We're all set, proceed with candidate creation.
    //
    JITDUMP("Marking call [%06u] as guarded devirtualization candidate; will guess for class %s\n", dspTreeID(call),
            eeGetClassName(classHandle));
    setMethodHasGuardedDevirtualization();
    call->SetGuardedDevirtualizationCandidate();

    // Spill off any GT_RET_EXPR subtrees so we can clone the call.
    //
    SpillRetExprHelper helper(this);
    helper.StoreRetExprResultsInArgs(call);

    // Gather some information for later. Note we actually allocate InlineCandidateInfo
    // here, as the devirtualized half of this call will likely become an inline candidate.
    //
    GuardedDevirtualizationCandidateInfo* pInfo = new (this, CMK_Inlining) InlineCandidateInfo;

    pInfo->guardedMethodHandle = methodHandle;
    pInfo->guardedClassHandle  = classHandle;
    pInfo->likelihood          = likelihood;

    // Save off the stub address since it shares a union with the candidate info.
    //
    if (call->IsVirtualStub())
    {
        JITDUMP("Saving stub addr %p in candidate info\n", dspPtr(call->gtStubCallStubAddr));
        pInfo->stubAddr = call->gtStubCallStubAddr;
    }
    else
    {
        pInfo->stubAddr = nullptr;
    }

    call->gtGuardedDevirtualizationCandidateInfo = pInfo;
}

void Compiler::addExpRuntimeLookupCandidate(GenTreeCall* call)
{
    setMethodHasExpRuntimeLookup();
    call->SetExpRuntimeLookup();
}

//------------------------------------------------------------------------
// impIsClassExact: check if a class handle can only describe values
//    of exactly one class.
//
// Arguments:
//    classHnd - handle for class in question
//
// Returns:
//    true if class is final and not subject to special casting from
//    variance or similar.
//
// Note:
//    We are conservative on arrays of primitive types here.

bool Compiler::impIsClassExact(CORINFO_CLASS_HANDLE classHnd)
{
    DWORD flags     = info.compCompHnd->getClassAttribs(classHnd);
    DWORD flagsMask = CORINFO_FLG_FINAL | CORINFO_FLG_VARIANCE | CORINFO_FLG_ARRAY;

    if ((flags & flagsMask) == CORINFO_FLG_FINAL)
    {
        return true;
    }
    if ((flags & flagsMask) == (CORINFO_FLG_FINAL | CORINFO_FLG_ARRAY))
    {
        CORINFO_CLASS_HANDLE arrayElementHandle = nullptr;
        CorInfoType          type               = info.compCompHnd->getChildType(classHnd, &arrayElementHandle);

        if ((type == CORINFO_TYPE_CLASS) || (type == CORINFO_TYPE_VALUECLASS))
        {
            return impIsClassExact(arrayElementHandle);
        }
    }
    return false;
}

//------------------------------------------------------------------------
// impCanSkipCovariantStoreCheck: see if storing a ref type value to an array
//    can skip the array store covariance check.
//
// Arguments:
//    value -- tree producing the value to store
//    array -- tree representing the array to store to
//
// Returns:
//    true if the store does not require a covariance check.
//
bool Compiler::impCanSkipCovariantStoreCheck(GenTree* value, GenTree* array)
{
    // We should only call this when optimizing.
    assert(opts.OptimizationEnabled());

    // Check for assignment to same array, ie. arrLcl[i] = arrLcl[j]
    if (value->OperIs(GT_INDEX) && array->OperIs(GT_LCL_VAR))
    {
        GenTree* valueIndex = value->AsIndex()->GetArray();
        if (valueIndex->OperIs(GT_LCL_VAR))
        {
            unsigned valueLcl = valueIndex->AsLclVar()->GetLclNum();
            unsigned arrayLcl = array->AsLclVar()->GetLclNum();
            if ((valueLcl == arrayLcl) && !lvaGetDesc(arrayLcl)->lvAddrExposed)
            {
                JITDUMP("\nstelem of ref from same array: skipping covariant store check\n");
                return true;
            }
        }
    }

    // Check for assignment of NULL.
    if (value->OperIs(GT_CNS_INT))
    {
        assert(value->gtType == TYP_REF);
        if (value->AsIntCon()->gtIconVal == 0)
        {
            JITDUMP("\nstelem of null: skipping covariant store check\n");
            return true;
        }
        // Non-0 const refs can only occur with frozen objects
        assert(value->IsIconHandle(GTF_ICON_STR_HDL));
        assert(doesMethodHaveFrozenString() ||
               (compIsForInlining() && impInlineInfo->InlinerCompiler->doesMethodHaveFrozenString()));
    }

    // Try and get a class handle for the array
    if (value->gtType != TYP_REF)
    {
        return false;
    }

    bool                 arrayIsExact   = false;
    bool                 arrayIsNonNull = false;
    CORINFO_CLASS_HANDLE arrayHandle    = gtGetClassHandle(array, &arrayIsExact, &arrayIsNonNull);

    if (arrayHandle == NO_CLASS_HANDLE)
    {
        return false;
    }

    // There are some methods in corelib where we're storing to an array but the IL
    // doesn't reflect this (see SZArrayHelper). Avoid.
    DWORD attribs = info.compCompHnd->getClassAttribs(arrayHandle);
    if ((attribs & CORINFO_FLG_ARRAY) == 0)
    {
        return false;
    }

    CORINFO_CLASS_HANDLE arrayElementHandle = nullptr;
    CorInfoType          arrayElemType      = info.compCompHnd->getChildType(arrayHandle, &arrayElementHandle);

    // Verify array type handle is really an array of ref type
    assert(arrayElemType == CORINFO_TYPE_CLASS);

    // Check for exactly object[]
    if (arrayIsExact && (arrayElementHandle == impGetObjectClass()))
    {
        JITDUMP("\nstelem to (exact) object[]: skipping covariant store check\n");
        return true;
    }

    // Check for T[] with T exact.
    if (!impIsClassExact(arrayElementHandle))
    {
        return false;
    }

    bool                 valueIsExact   = false;
    bool                 valueIsNonNull = false;
    CORINFO_CLASS_HANDLE valueHandle    = gtGetClassHandle(value, &valueIsExact, &valueIsNonNull);

    if (valueHandle == arrayElementHandle)
    {
        JITDUMP("\nstelem to T[] with T exact: skipping covariant store check\n");
        return true;
    }

    return false;
}

GenTree* Compiler::impImportInitObj(GenTree* dstAddr, CORINFO_CLASS_HANDLE classHandle)
{
    ClassLayout* layout = typGetObjLayout(classHandle);
    GenTree*     dst    = nullptr;

    if (dstAddr->OperIs(GT_ADDR))
    {
        GenTree* location = dstAddr->AsUnOp()->GetOp(0);

        if (location->OperIs(GT_LCL_VAR) && varTypeIsStruct(location->GetType()))
        {
            LclVarDsc* lcl = lvaGetDesc(location->AsLclVar());

            if (layout->GetSize() >= lcl->GetLayout()->GetSize())
            {
                layout = lcl->GetLayout();
                dst    = location;
            }
        }
    }

    if (dst == nullptr)
    {
        dst = gtNewObjNode(layout, dstAddr);
    }

    GenTree* initValue;

#ifdef FEATURE_SIMD
    if (layout->IsVector())
    {
        // TODO-MIKE-Cleanup: This should probably use gtGetSIMDZero. But NI_Vector128_get_Zero & co.
        // are a bunch of nonsense, how many "zero" nodes does it take to change a light bulb!?!
        initValue = gtNewSIMDVectorZero(layout->GetSIMDType(), layout->GetElementType(), layout->GetSize());
    }
    else
#endif
    {
        initValue = gtNewIconNode(0);
    }

    return gtNewAssignNode(dst, initValue);
}

GenTree* Compiler::impImportCpObj(GenTree* dstAddr, GenTree* srcAddr, CORINFO_CLASS_HANDLE classHandle)
{
    GenTree* dst = nullptr;

    if (dstAddr->OperIs(GT_ADDR))
    {
        GenTree* location = dstAddr->AsUnOp()->GetOp(0);

        if (location->OperIs(GT_LCL_VAR))
        {
            LclVarDsc* lcl = lvaGetDesc(location->AsLclVar());

            if (varTypeIsStruct(lcl->GetType()) && !lcl->IsImplicitByRefParam() &&
                (lcl->GetLayout()->GetClassHandle() == classHandle))
            {
                dst = location;
            }
        }
    }

    if (dst == nullptr)
    {
        dst = gtNewObjNode(classHandle, dstAddr);
    }

    GenTree* src = nullptr;

    if (srcAddr->OperIs(GT_ADDR))
    {
        src = srcAddr->AsUnOp()->GetOp(0);
    }
    else
    {
        src = gtNewObjNode(classHandle, srcAddr);
    }

    // TODO-MIKE-CQ: This should probably be removed, it's here only because
    // a previous implementation (gtNewBlkOpNode) was setting it. And it
    // probably blocks SIMD tree CSEing.
    src->gtFlags |= GTF_DONT_CSE;

    GenTreeOp* asg = gtNewAssignNode(dst, src);
    gtInitStructCopyAsg(asg);
    return asg;
}

GenTree* Compiler::impImportInitBlk(GenTree* dstAddr, GenTree* initValue, GenTree* size, bool isVolatile)
{
    ClassLayout*  layout = nullptr;
    GenTreeIndir* dst;

    if (GenTreeIntCon* sizeIntCon = size->IsIntCon())
    {
        layout = typGetBlkLayout(static_cast<unsigned>(sizeIntCon->GetValue()));
        dst    = new (this, GT_BLK) GenTreeBlk(dstAddr, layout);
    }
    else
    {
        dst = new (this, GT_DYN_BLK) GenTreeDynBlk(dstAddr, size);
    }

    if (isVolatile)
    {
        dst->SetVolatile();
    }

    // TODO-MIKE-Review: Currently INITBLK ignores the unaligned prefix.

    if (!initValue->IsIntegralConst(0))
    {
        initValue = gtNewOperNode(GT_INIT_VAL, TYP_INT, initValue);
    }

    return gtNewAssignNode(dst, initValue);
}

GenTree* Compiler::impImportCpBlk(GenTree* dstAddr, GenTree* srcAddr, GenTree* size, bool isVolatile)
{
    ClassLayout* layout = nullptr;
    GenTreeBlk*  dst;

    if (GenTreeIntCon* sizeIntCon = size->IsIntCon())
    {
        layout = typGetBlkLayout(static_cast<unsigned>(sizeIntCon->GetValue()));
        dst    = new (this, GT_BLK) GenTreeBlk(dstAddr, layout);
    }
    else
    {
        dst = new (this, GT_DYN_BLK) GenTreeDynBlk(dstAddr, size);
    }

    if (isVolatile)
    {
        dst->SetVolatile();
    }

    // TODO-MIKE-Review: Currently CPBLK ignores the unaligned prefix.

    GenTreeIndir* src;

    if (layout != nullptr)
    {
        src = new (this, GT_BLK) GenTreeBlk(srcAddr, layout);
    }
    else
    {
        // STRUCT typed IND aren't normally used, we'll use it here as a special case, to denote
        // a "load" of unknown size. Maybe using DYN_BLK as source would make more sense.
        // We'd need to spill the size tree so it can have multiple uses but such copies are
        // rare so getting an extra local shouldn't be a problem.

        // TODO-MIKE-Consider: Replace GT_DYN_BLK with GT_COPY_BLK. Using load/store semantics
        // for untyped, arbitrary sized copies is kind of nonsense.

        src = new (this, GT_IND) GenTreeIndir(GT_IND, TYP_STRUCT, srcAddr);
    }

    if (isVolatile)
    {
        src->SetVolatile();
    }

    return gtNewAssignNode(dst, src);
}

GenTree* Compiler::impImportPop(BasicBlock* block)
{
    StackEntry           se     = impPopStack();
    CORINFO_CLASS_HANDLE clsHnd = se.seTypeInfo.GetClassHandle();
    GenTree*             op1    = se.val;

    if (((op1->gtFlags & GTF_SIDE_EFFECT) == 0) && !opts.compDbgCode)
    {
        return nullptr;
    }

    if (varTypeIsStruct(op1->GetType()))
    {
        JITDUMPTREE(op1, "\n ... CEE_POP struct ...\n");

        // If the value being produced comes from loading
        // via an underlying address, just null check the address.

        // TODO-MIKE-Cleanup: If the tree is an INDEX then this will assign it to
        // a newly allocated temp. We don't need it, we only need the range check
        // side effect. This could probably be achieved by changing the tree to
        // ADDR(INDEX(...)) and treating the address as an unused value below or
        // perhaps by simply replacing INDEX with ARR_BOUNDS_CHECK here instead of
        // deferring it to morph.

        if (op1->OperIs(GT_FIELD, GT_IND, GT_OBJ))
        {
            gtChangeOperToNullCheck(op1, block);
        }
        else
        {
            op1 = impGetStructAddr(op1, clsHnd, CHECK_SPILL_ALL, false);
        }

        JITDUMPTREE(op1, "\n ... optimized to ...\n");
    }

    // If op1 is non-overflow cast, throw it away since it is useless.
    // Another reason for throwing away the useless cast is in the context of
    // implicit tail calls when the operand of pop is GT_CAST(GT_CALL(..)).
    // The cast gets added as part of importing GT_CALL, which gets in the way
    // of fgMorphCall() on the forms of tail call nodes that we assert.
    if (op1->IsCast() && !op1->gtOverflow())
    {
        op1 = op1->AsCast()->GetOp(0);
    }

    if (!op1->IsCall())
    {
        if ((op1->gtFlags & GTF_SIDE_EFFECT) != 0)
        {
            op1 = gtUnusedValNode(op1);
        }
        else
        {
            // Can't bash to NOP here because op1 can be referenced from the spill
            // clique, if we ever need to reimport we need a valid LCL_VAR on it.
            op1 = gtNewNothingNode();
        }
    }

    return op1;
}

GenTree* Compiler::impImportTlsFieldAccess(CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                           const CORINFO_FIELD_INFO& fieldInfo,
                                           CORINFO_ACCESS_FLAGS      accessFlags,
                                           var_types                 type)
{
#if !defined(TARGET_X86) || !defined(TARGET_WINDOWS)
    // Legacy TLS access is implemented as intrinsic on x86 only
    assert(fieldInfo.helper == CORINFO_HELP_GETSTATICFIELDADDR_TLS);

    GenTree* fieldHnd = impTokenToHandle(resolvedToken);
    GenTree* addr     = gtNewHelperCallNode(fieldInfo.helper, TYP_BYREF, gtNewCallArgs(fieldHnd));
#else
    void**   pIdAddr = nullptr;
    unsigned IdValue = info.compCompHnd->getFieldThreadLocalStoreID(resolvedToken->hField, (void**)&pIdAddr);

    // If we can we access the TLS DLL index ID value directly
    // then pIdAddr will be NULL and
    //      IdValue will be the actual TLS DLL index ID

    GenTree* dllRef = nullptr;
    if (pIdAddr == nullptr)
    {
        if (IdValue != 0)
        {
            dllRef = gtNewIconNode(IdValue * 4, TYP_I_IMPL);
        }
    }
    else
    {
        dllRef = gtNewIndOfIconHandleNode(TYP_I_IMPL, reinterpret_cast<size_t>(pIdAddr), GTF_ICON_CONST_PTR, true);

        // Next we multiply by 4
        dllRef = gtNewOperNode(GT_MUL, TYP_I_IMPL, dllRef, gtNewIconNode(4, TYP_I_IMPL));
    }

    constexpr size_t WIN32_TLS_SLOTS = 0x2C; // Offset from fs:[0] where the pointer to the slots resides

    // Mark this ICON as a TLS_HDL, codegen will use FS:[cns]

    GenTree* addr = gtNewIconHandleNode(WIN32_TLS_SLOTS, GTF_ICON_TLS_HDL);

    if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_INITCLASS) != 0)
    {
        addr->gtFlags |= GTF_ICON_INITCLASS;
    }

    addr = gtNewOperNode(GT_IND, TYP_I_IMPL, addr);
    addr->gtFlags |= GTF_IND_NONFAULTING | GTF_IND_INVARIANT;

    if (dllRef != nullptr)
    {
        addr = gtNewOperNode(GT_ADD, TYP_I_IMPL, addr, dllRef);
    }

    addr = gtNewOperNode(GT_IND, TYP_I_IMPL, addr);

    if (fieldInfo.offset != 0)
    {
        // Add the TLS static field offset. Don't bother recording a field sequence
        // for the field offset as it won't be recognized during value numbering.
        addr = gtNewOperNode(GT_ADD, TYP_I_IMPL, addr, gtNewIconNode(fieldInfo.offset, TYP_I_IMPL));
    }
#endif // TARGET_X86 && TARGET_WINDOWS

    if ((accessFlags & CORINFO_ACCESS_ADDRESS) != 0)
    {
        return addr;
    }

    GenTree* indir;

    if (varTypeIsStruct(type))
    {
        indir = gtNewObjNode(fieldInfo.structType, addr);
    }
    else
    {
        indir = gtNewOperNode(GT_IND, type, addr);
    }

    indir->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;
    return indir;
}
