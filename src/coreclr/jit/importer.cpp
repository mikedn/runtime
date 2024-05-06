// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

void Importer::impPushOnStack(GenTree* tree, typeInfo ti)
{
    if (verCurrentState.esStackDepth >= verCurrentState.maxStack)
    {
        BADCODE("stack overflow");
    }

    // If we are pushing a struct, make certain we know the precise type!
    assert(!tree->TypeIs(TYP_STRUCT) || (ti.GetClassHandleForValueClass() != NO_CLASS_HANDLE));

    auto& entry      = verCurrentState.esStack[verCurrentState.esStackDepth++];
    entry.seTypeInfo = ti;
    entry.val        = tree;

    if (((tree->gtType == TYP_FLOAT) || (tree->gtType == TYP_DOUBLE)) && !comp->compFloatingPointUsed)
    {
        comp->compFloatingPointUsed = true;
    }
}

typeInfo Importer::impMakeTypeInfo(CorInfoType type, CORINFO_CLASS_HANDLE classHandle)
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

void Compiler::impResolveToken(const BYTE* addr, CORINFO_RESOLVED_TOKEN* resolvedToken, CorInfoTokenKind kind)
{
    resolvedToken->tokenContext = impTokenLookupContextHandle;
    resolvedToken->tokenScope   = info.compScopeHnd;
    resolvedToken->token        = getU4LittleEndian(addr);
    resolvedToken->tokenType    = kind;

    info.compCompHnd->resolveToken(resolvedToken);
}

Importer::StackEntry Importer::impPopStack()
{
    if (verCurrentState.esStackDepth == 0)
    {
        BADCODE("stack underflow");
    }

    return verCurrentState.esStack[--verCurrentState.esStackDepth];
}

GenTree* Importer::impPopStackCoerceArg(var_types signatureType)
{
    // Not currently supported for structs (it would need to check the struct handle).
    assert(!varTypeIsStruct(signatureType));
    // Not currently supported for small int (it's not clear if truncation has to be done or not).
    assert(!varTypeIsSmall(signatureType));

    // TODO-MIKE-Cleanup: PopCallArgs has some similar logic...

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
            tree = gtNewCastNode(tree, false, signatureType);
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

GenTree* Importer::impSIMDPopStack(var_types type)
{
    assert(varTypeIsSIMD(type));

    GenTree* tree = impPopStack().val;

    if (tree->OperIs(GT_RET_EXPR, GT_CALL))
    {
        // TODO-MIKE-Cleanup: This is probably not needed when the SIMD type is returned in a register.

        ClassLayout* layout = tree->IsRetExpr() ? tree->AsRetExpr()->GetLayout() : tree->AsCall()->GetRetLayout();

        LclVarDsc* tmpLcl = lvaAllocTemp(true DEBUGARG("struct address for call/obj"));
        impAppendTempStore(tmpLcl, tree, layout, CHECK_SPILL_ALL);
        tree = gtNewLclvNode(tmpLcl, tmpLcl->GetType());
    }

    assert(tree->GetType() == type);

    return tree;
}

#endif // FEATURE_SIMD

Importer::StackEntry& Importer::impStackTop(unsigned n)
{
    if (verCurrentState.esStackDepth <= n)
    {
        BADCODE("stack underflow");
    }

    return verCurrentState.esStack[verCurrentState.esStackDepth - n - 1];
}

unsigned Importer::impStackHeight()
{
    return verCurrentState.esStackDepth;
}

void Importer::impStmtListAppend(Statement* stmt)
{
    if (impStmtList == nullptr)
    {
        impStmtList = stmt;
    }
    else
    {
        impLastStmt->SetNextStmt(stmt);
        stmt->SetPrevStmt(impLastStmt);
    }

    impLastStmt = stmt;
}

Statement* Importer::impStmtListRemoveLast()
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

void Importer::impStmtListInsertBefore(Statement* stmt, Statement* stmtBefore)
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

void Importer::impStmtListEnd(BasicBlock* block)
{
    impSetBlockStmtList(block, impStmtList, impLastStmt);

    impStmtList = nullptr;
    impLastStmt = nullptr;
}

void Importer::impSetBlockStmtList(BasicBlock* block, Statement* firstStmt, Statement* lastStmt)
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

#ifdef DEBUG

void Importer::AppendStmtCheck(GenTree* tree, unsigned chkLevel)
{
    assert((verCurrentState.esStackDepth != 0) && (chkLevel != 0));

    if (chkLevel == CHECK_SPILL_ALL)
    {
        chkLevel = verCurrentState.esStackDepth;
    }

    // Calls can only be appended if there are no GTF_GLOB_EFFECT on the stack

    if (tree->HasAnySideEffect(GTF_CALL))
    {
        for (unsigned level = 0; level < chkLevel; level++)
        {
            GenTree* tree = verCurrentState.esStack[level].val;
            assert(!tree->HasAnySideEffect(GTF_GLOB_EFFECT) || impIsAddressInLocal(tree));
        }
    }

    if (tree->OperIs(GT_STORE_LCL_VAR))
    {
        // For a store to a local variable, all references to that local have
        // to be spilled. If the local is address taken, all calls and indirect
        // accesses have to be spilled.

        LclVarDsc* lcl = tree->AsLclVar()->GetLcl();

        for (unsigned level = 0; level < chkLevel; level++)
        {
            GenTree* val = verCurrentState.esStack[level].val;

            assert(!impHasLclRef(val, lcl) || impIsAddressInLocal(val));

            // TODO-MIKE-Cleanup: Checking IsAddressExposed here is nonsense,
            // it's rarely set during import.

            assert(!lcl->IsAddressExposed() || !val->HasAnySideEffect(GTF_SIDE_EFFECT));
        }
    }
    else if (tree->OperIs(GT_STOREIND, GT_STORE_OBJ, GT_STORE_BLK))
    {
        // For indirect stores, all side effects have to be spilled.
        // TODO-MIKE-Review: Comment says "all side effects" but code
        // checks only for GLOB_REF. This should probably check for
        // EXCEPT as well.

        for (unsigned level = 0; level < chkLevel; level++)
        {
            assert(!verCurrentState.esStack[level].val->HasAnySideEffect(GTF_GLOB_REF));
        }
    }
}

#endif // DEBUG

void Importer::SpillStackCheck(GenTree* tree, unsigned spillDepth)
{
    if ((spillDepth != 0) && (verCurrentState.esStackDepth != 0))
    {
        SpillStack(tree, spillDepth);
    }
}

// Spill stack trees that interfere with stmtExpr.
// [0..spillDepth) is the portion of the stack which we will check
// for interference with stmt and spill if needed.
void Importer::SpillStack(GenTree* stmtExpr, unsigned spillDepth)
{
    assert((spillDepth != 0) && (verCurrentState.esStackDepth != 0));

    // If the statement being appended has any side-effects, check the stack
    // to see if anything needs to be spilled to preserve correct ordering.

    unsigned stmtSideEffects = stmtExpr->GetSideEffects();

    // Assignment to (unaliased) locals don't count as a side-effect as we
    // handle them specially using impSpillLclReferences. Temp locals
    // should be fine too.

    // TODO-MIKE-Review: Using GTF_GLOB_REF for locals here is dubious. It's
    // basically never set at import time, in part due to the fact we do not
    // know yet which variables are address exposed. What we probably need to
    // do here is to check for address taken locals.
    // Oddly enough, impSpillSideEffects does that already.

    if (stmtExpr->OperIs(GT_STORE_LCL_VAR) && !impHasAddressTakenLocals(stmtExpr->AsLclVar()->GetOp(0)))
    {
        GenTreeFlags srcSideEffects = stmtExpr->AsLclVar()->GetOp(0)->GetSideEffects();
        assert(stmtSideEffects == (srcSideEffects | GTF_ASG));
        stmtSideEffects = srcSideEffects;
    }

    // TODO-MIKE-Review: It's not clear why GTF_ORDER_SIDEEFF keeps getting ignored.
    // If the statement we're appending contains volatile indirs then we should probably
    // spill pretty much everything. Now, such indir have GLOB_REF most of the time,
    // except when they're generated from volatile local field access which are rather
    // rare and unusual.

    if ((stmtSideEffects & GTF_GLOB_EFFECT) == 0)
    {
        return;
    }

    GenTreeFlags spillSideEffects = GTF_SIDE_EFFECT;

    if ((stmtSideEffects & GTF_CALL) != 0)
    {
        // If there is a call, we have to spill global refs
        spillSideEffects |= GTF_GLOB_REF;
    }
    else if (stmtExpr->OperIs(GT_STORE_LCL_VAR))
    {
        if (stmtExpr->AsLclVar()->GetOp(0)->HasAnySideEffect(GTF_ASG))
        {
            // The value has a store side effect. Since we don't know
            // where it stores to, we need to spill global refs.
            spillSideEffects |= GTF_GLOB_REF;
        }

        // TODO-MIKE-Review: Should this check for stores to an address taken local?
        // The ASG version checked for GTF_GLOB_REF on destination but GLOB_REF is
        // usually not set on locals during import.
    }
    else if ((stmtSideEffects & GTF_ASG) != 0)
    {
        // The expression is not a store but it has a store side effect, it must be an
        // atomic op, HW intrinsic or some other kind of node that stores to memory.
        // Since we don't know where it stores to, we need to spill global refs.
        spillSideEffects |= GTF_GLOB_REF;
    }

    impSpillSideEffects(spillSideEffects, spillDepth DEBUGARG("append statement spill temp"));
}

Statement* Importer::impAppendTree(GenTree* tree, unsigned spillDepth)
{
    assert(tree != nullptr);

    if ((spillDepth != 0) && (verCurrentState.esStackDepth != 0))
    {
        SpillStack(tree, spillDepth);
        INDEBUG(AppendStmtCheck(tree, spillDepth);)
    }

    Statement* stmt = gtNewStmt(tree, impCurStmtOffs);
    impStmtListAppend(stmt);

#ifdef FEATURE_SIMD
    if (opts.OptimizationEnabled() && comp->featureSIMD)
    {
        m_impSIMDCoalescingBuffer.Mark(comp, stmt);
    }
#endif

    // Once we set impCurStmtOffs in an appended tree, we are ready
    // to report the following offsets. So reset impCurStmtOffs.
    impCurStmtOffs = BAD_IL_OFFSET;

#ifdef DEBUG
    if (verbose)
    {
        printf("\n\n");
        gtDispStmt(stmt);
    }
#endif

    return stmt;
}

Statement* Importer::impSpillAllAppendTree(GenTree* op1)
{
    return impAppendTree(op1, CHECK_SPILL_ALL);
}

Statement* Importer::impSpillNoneAppendTree(GenTree* op1)
{
    return impAppendTree(op1, CHECK_SPILL_NONE);
}

// Append a store of the given value to a temp to the current tree list.
// curLevel is the stack level for which the spill to the temp is being done.
void Importer::impAppendTempStore(LclVarDsc* lcl, GenTree* val, unsigned curLevel)
{
    assert(lcl->GetType() == TYP_UNDEF);

    var_types type = varActualType(val->GetType());
    lcl->SetType(type);

    GenTree* asg = comp->gtNewLclStore(lcl, type, val);
    impAppendTree(asg, curLevel);
}

void Importer::impAppendTempStore(LclVarDsc* lcl, GenTree* val, ClassLayout* layout, unsigned curLevel)
{
    if (!varTypeIsStruct(val->GetType()))
    {
        impAppendTempStore(lcl, val, curLevel);
        return;
    }

    assert(layout->IsValueClass());
    assert(lcl->GetType() == TYP_UNDEF);

    comp->lvaSetStruct(lcl, layout, false);

    GenTree* dest = gtNewLclvNode(lcl, lcl->GetType());
    GenTree* asg  = impAssignStruct(dest, val, curLevel);
    impAppendTree(asg, curLevel);
}

void Importer::impAppendTempStore(LclVarDsc* lcl, GenTree* val, CORINFO_CLASS_HANDLE structType, unsigned curLevel)
{
    if (!varTypeIsStruct(val->GetType()))
    {
        impAppendTempStore(lcl, val, curLevel);
        return;
    }

    impAppendTempStore(lcl, val, typGetObjLayout(structType), curLevel);
}

GenTreeCall::Use* Importer::PopCallArgs(CORINFO_SIG_INFO* sig, GenTree* extraArg)
{
    assert(sig->retType != CORINFO_TYPE_VAR);

    GenTreeCall::Use* args = nullptr;

#ifdef TARGET_X86
    if (extraArg != nullptr)
    {
        args = gtNewCallArgs(extraArg);
    }
#endif

    for (unsigned i = 0, paramCount = sig->numArgs; i < paramCount; i++)
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

        args = gtPrependNewCallArg(arg, args);
    }

    CORINFO_ARG_LIST_HANDLE param = sig->args;
    GenTreeCall::Use*       arg   = args;

    for (unsigned i = 0, paramCount = sig->numArgs; i < paramCount; i++)
    {
        CORINFO_CLASS_HANDLE paramClass;
        CorInfoType          paramCorType = strip(info.compCompHnd->getArgType(sig, param, &paramClass));
        var_types            paramType    = varActualType(CorTypeToVarType(paramCorType));

        if (paramType != varActualType(arg->GetNode()->GetType()))
        {
            arg->SetNode(CoerceCallArg(paramType, arg->GetNode()));
        }

        if ((paramCorType == CORINFO_TYPE_VALUECLASS) || (paramCorType == CORINFO_TYPE_REFANY))
        {
            assert(paramClass != NO_CLASS_HANDLE);
            arg->SetSigTypeNum(typGetObjLayoutNum(paramClass));
        }
        else
        {
            arg->SetSigTypeNum(static_cast<unsigned>(CorTypeToVarType(paramCorType)));
        }

        if ((paramCorType != CORINFO_TYPE_CLASS) && (paramCorType != CORINFO_TYPE_BYREF) &&
            (paramCorType != CORINFO_TYPE_PTR))
        {
            CORINFO_CLASS_HANDLE realParamClass = info.compCompHnd->getArgClass(sig, param);

            if (realParamClass != nullptr)
            {
                // Make sure that all valuetypes (including enums) that we push are loaded.
                // This is to guarantee that if a GC is triggered from the prestub of this
                // methods, all valuetypes in the method signature are already loaded.
                // We need to be able to find the size of the valuetypes, but we cannot
                // do a class-load from within GC.
                info.compCompHnd->classMustBeLoadedBeforeCodeIsRun(realParamClass);
            }
        }

        if (i + 1 < paramCount)
        {
            param = info.compCompHnd->getArgNext(param);
            arg   = arg->GetNext();
        }
    }

    if ((sig->retTypeSigClass != nullptr) && (sig->retType != CORINFO_TYPE_CLASS) &&
        (sig->retType != CORINFO_TYPE_BYREF) && (sig->retType != CORINFO_TYPE_PTR))
    {
        // Make sure that all valuetypes (including enums) that we push are loaded.
        // This is to guarantee that if a GC is triggerred from the prestub of this
        // methods, all valuetypes in the method signature are already loaded.
        // We need to be able to find the size of the valuetypes, but we cannot
        // do a class-load from within GC.
        info.compCompHnd->classMustBeLoadedBeforeCodeIsRun(sig->retTypeSigClass);
    }

#ifndef TARGET_X86
    if (extraArg != nullptr)
    {
        args = gtPrependNewCallArg(extraArg, args);
    }
#endif

    return args;
}

GenTree* Importer::CoerceCallArg(var_types paramType, GenTree* arg)
{
    var_types argType  = varActualType(arg->GetType());
    var_types castType = TYP_UNDEF;

    assert(paramType != argType);

    switch (paramType)
    {
#ifdef TARGET_64BIT
        case TYP_INT:
            // We allow implicit int64 to int32 truncation that ECMA does not allow,
            // JIT's type system doesn't distinguish between "native int" and int64.
            castType = (argType == TYP_LONG) ? paramType : TYP_UNDEF;
            break;

        case TYP_LONG:
            // We allow implicit int32 to int64 extension that ECMA does not allow,
            // JIT's type system doesn't distinguish between "native int" and int32.
            if (argType == TYP_INT)
            {
                // TODO-MIKE-Fix: This gets it wrong when the arg is int32 and the param
                // is native uint, the spec requires zero extension but we do sign extension.
                // Probably it doesn't really matter, at least the C# compiler has the habit
                // of inserting its own casts.
                castType = paramType;
                break;
            }

            // We allow BYREF to LONG conversion that ECMA does not allow but that
            // appears in real code (e.g. passing a ldloca/ldarga value as native int).
            castType = (argType == TYP_BYREF) ? argType : TYP_UNDEF;
            break;
#else
        case TYP_INT:
            // We allow BYREF to INT conversion that ECMA does not allow but that
            // appears in real code (e.g. passing a ldloca/ldarga value as native int).
            castType = argType == TYP_BYREF ? argType : TYP_UNDEF;
            break;
#endif

        case TYP_FLOAT:
            castType = (argType == TYP_DOUBLE) ? paramType : TYP_UNDEF;
            break;

        case TYP_DOUBLE:
            castType = (argType == TYP_FLOAT) ? paramType : TYP_UNDEF;
            break;

        case TYP_BYREF:
            // We allow REF to BYREF conversion that ECMA does not allow but that
            // sometimes appears in real code (e.g. to get the method table from
            // an object reference or to create a null BYREF by using ldnull, like
            // IL stubs using GetDelegateTarget do).
            castType = ((argType == TYP_I_IMPL) || (argType == TYP_REF)) ? argType : TYP_UNDEF;
            break;

        case TYP_STRUCT:
            castType = varTypeIsSIMD(argType) ? argType : TYP_UNDEF;
            break;

        default:
            break;
    }

    if (castType == TYP_UNDEF)
    {
        BADCODE("the call argument has a type that can't be implicitly converted to the parameter type");
    }

    return castType == argType ? arg : gtNewCastNode(arg, false, castType);
}

#ifdef TARGET_X86
GenTreeCall::Use* Importer::ReverseCallArgs(GenTreeCall::Use* args, bool skipFirst)
{
    GenTreeCall::Use* reversedArgs = nullptr;
    GenTreeCall::Use* arg          = args;

    if (skipFirst)
    {
        arg = arg->GetNext();
    }

    if (arg == nullptr)
    {
        return args;
    }

    do
    {
        GenTreeCall::Use* next = arg->GetNext();
        arg->SetNext(reversedArgs);
        reversedArgs = arg;
        arg          = next;
    } while (arg != nullptr);

    if (skipFirst)
    {
        args->SetNext(reversedArgs);
        reversedArgs = args;
    }

    return reversedArgs;
}
#endif // TARGET_X86

void Compiler::impAssignCallWithRetBuf(GenTree* dest, GenTreeCall* call)
{
    assert(varTypeIsStruct(dest->GetType()) && dest->OperIs(GT_LCL_VAR, GT_OBJ, GT_IND));
    assert(call->TreatAsHasRetBufArg());

    GenTree* retBufAddr;

    if (dest->OperIs(GT_LCL_VAR))
    {
        retBufAddr = dest->ChangeToLclAddr(TYP_I_IMPL, dest->AsLclVar()->GetLcl());
    }
    else
    {
        retBufAddr = dest->AsIndir()->GetAddr();
    }

#if defined(TARGET_WINDOWS) && !defined(TARGET_ARM)
    if (call->IsUnmanaged())
    {
        if (callConvIsInstanceMethodCallConv(call->GetUnmanagedCallConv()))
        {
#ifdef TARGET_X86
            // The argument list has already been reversed.
            // Insert the return buffer as the second-to-last node
            // so it will be pushed on to the stack after the user args but before the native this arg
            // as required by the native ABI.
            GenTreeCall::Use* lastArg = call->gtCallArgs;
            if (lastArg == nullptr)
            {
                call->gtCallArgs = gtPrependNewCallArg(retBufAddr, call->gtCallArgs);
            }
            else if (call->GetUnmanagedCallConv() == CorInfoCallConvExtension::Thiscall)
            {
                // For thiscall, the "this" parameter is not included in the argument list reversal,
                // so we need to put the return buffer as the last parameter.
                for (; lastArg->GetNext() != nullptr; lastArg = lastArg->GetNext())
                    ;
                gtInsertNewCallArgAfter(retBufAddr, lastArg);
            }
            else if (lastArg->GetNext() == nullptr)
            {
                call->gtCallArgs = gtPrependNewCallArg(retBufAddr, lastArg);
            }
            else
            {
                assert(lastArg != nullptr && lastArg->GetNext() != nullptr);
                GenTreeCall::Use* secondLastArg = lastArg;
                lastArg                         = lastArg->GetNext();
                for (; lastArg->GetNext() != nullptr; secondLastArg = lastArg, lastArg = lastArg->GetNext())
                    ;
                assert(secondLastArg->GetNext() != nullptr);
                gtInsertNewCallArgAfter(retBufAddr, secondLastArg);
            }
#else
            gtInsertNewCallArgAfter(retBufAddr, call->gtCallArgs);
#endif
        }
        else
        {
#ifndef TARGET_X86
            call->gtCallArgs = gtPrependNewCallArg(retBufAddr, call->gtCallArgs);
#else
            // The argument list has already been reversed.
            // Insert the return buffer as the last node so it will be pushed on to the stack last
            // as required by the native ABI.
            GenTreeCall::Use* lastArg = call->gtCallArgs;
            if (lastArg == nullptr)
            {
                call->gtCallArgs = gtPrependNewCallArg(retBufAddr, call->gtCallArgs);
            }
            else
            {
                for (; lastArg->GetNext() != nullptr; lastArg = lastArg->GetNext())
                    ;
                gtInsertNewCallArgAfter(retBufAddr, lastArg);
            }
#endif
        }
    }
    else
#endif // defined(TARGET_WINDOWS) && !defined(TARGET_ARM)
    {
        call->gtCallArgs = gtPrependNewCallArg(retBufAddr, call->gtCallArgs);
    }

    call->SetType(TYP_VOID);
}

GenTree* Importer::impAssignMkRefAny(GenTree* dest, GenTreeOp* mkRefAny, unsigned curLevel)
{
    assert(dest->TypeIs(TYP_STRUCT) && dest->OperIs(GT_LCL_VAR, GT_OBJ));
    assert(mkRefAny->OperIs(GT_MKREFANY));

    GenTree* destAddr;

    if (dest->OperIs(GT_LCL_VAR))
    {
        // TODO-MIKE-Cleanup: This should generate LCL_FLD stores...
        destAddr = dest->ChangeToLclAddr(TYP_I_IMPL, dest->AsLclVar()->GetLcl());
    }
    else
    {
        destAddr = dest->AsObj()->GetAddr();
    }

    GenTree* destAddrUses[2];
    impMakeMultiUse(destAddr, 2, destAddrUses, curLevel DEBUGARG("MKREFANY store"));

    GenTreeFieldAddr* valueAddr =
        gtNewFieldAddr(destAddrUses[0], GetRefanyValueField(), OFFSETOF__CORINFO_TypedReference__dataPtr);
    GenTreeIndir*     valueStore = gtNewFieldIndStore(TYP_BYREF, valueAddr, mkRefAny->GetOp(0));
    GenTreeFieldAddr* typeAddr =
        gtNewFieldAddr(destAddrUses[1], GetRefanyTypeField(), OFFSETOF__CORINFO_TypedReference__type);
    GenTreeIndir* typeStore = gtNewFieldIndStore(TYP_I_IMPL, typeAddr, mkRefAny->GetOp(1));

    impAppendTree(valueStore, curLevel);
    return typeStore;
}

GenTree* Importer::impAssignStruct(GenTree* dest, GenTree* src, unsigned curLevel)
{
    assert(dest->OperIs(GT_LCL_VAR, GT_OBJ));
    assert(
        (src->TypeIs(TYP_STRUCT) && src->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_OBJ, GT_CALL, GT_MKREFANY, GT_RET_EXPR)) ||
        varTypeIsSIMD(src->GetType()));

    // Storing a MKREFANY generates 2 stores, one for each field of the struct.
    // One store is appended and the other is returned to the caller.

    if (src->OperIs(GT_MKREFANY))
    {
        return impAssignMkRefAny(dest, src->AsOp(), curLevel);
    }

    // Handle calls that return structs by reference - the destination address
    // is passed to the call as the return buffer address and no store is
    // generated.

    if (GenTreeCall* call = src->IsCall())
    {
        if (call->TreatAsHasRetBufArg())
        {
            comp->impAssignCallWithRetBuf(dest, call);

            return call;
        }

#if FEATURE_MULTIREG_RET
        if (dest->OperIs(GT_LCL_VAR))
        {
#ifndef UNIX_AMD64_ABI
            if (call->HasMultiRegRetVal())
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

                dest->AsLclVar()->GetLcl()->lvIsMultiRegRet = true;
            }
        }
#endif
    }
    else if (GenTreeRetExpr* retExpr = src->IsRetExpr())
    {
        GenTreeCall* call = retExpr->GetCall();

        assert(retExpr->GetRetExpr() == call);

        if (call->TreatAsHasRetBufArg())
        {
            comp->impAssignCallWithRetBuf(dest, call);
            retExpr->SetType(TYP_VOID);

            return retExpr;
        }
    }

    // In all other cases we create and return a struct store node.

    if (dest->OperIs(GT_LCL_VAR))
    {
        GenTreeLclVar* store = comp->gtNewLclStore(dest->AsLclVar()->GetLcl(), dest->GetType(), src);
        store->AddSideEffects(dest->GetSideEffects());
        gtInitStructLclStore(store, src);

        return store;
    }

    if (varTypeIsSIMD(dest->GetType()))
    {
        // TODO-MIKE-Cleanup: There doesn't seem to be any good reason to do this here,
        // except for VN being weird and failing on SIMD OBJs and old code doing it here.
        dest->SetOper(GT_IND);
    }

    gtInitStructIndStore(dest->AsIndir(), src);
    return dest;
}

void Importer::gtInitStructIndStore(GenTreeIndir* store, GenTree* value)
{
    assert(store->OperIs(GT_IND, GT_OBJ) && varTypeIsStruct(store->GetType()));
    assert(varTypeIsStruct(value->GetType()) || value->IsIntCon(0));

    store->SetOper(store->OperIs(GT_IND) ? GT_STOREIND : GT_STORE_OBJ);
    store->SetValue(value);
    store->AddSideEffects(GTF_ASG | value->GetSideEffects());

    if (value->OperIs(GT_INIT_VAL, GT_CNS_INT))
    {
        return;
    }

    // In the case of a block copy, we want to avoid generating nodes where the source
    // and destination are the same because of two reasons, first, is useless, second
    // it introduces issues in liveness and also copying memory from an overlapping
    // memory location is undefined both as per the ECMA standard and also the memcpy
    // semantics specify that.
    //
    // NOTE: In this case we'll only detect the case for addr of a local and a local
    // itself, any other complex expressions won't be caught.
    //
    // TODO-Cleanup: though having this logic is goodness (i.e. avoids self-copying
    // of struct vars very early), it was added because fgInterBlockLocalVarLiveness()
    // isn't handling self-copying of struct variables correctly. This issue may not
    // surface if struct promotion is ON (which is the case on x86/arm). But still the
    // fundamental issue exists that needs to be addressed.

    LclVarDsc* srcLcl     = nullptr;
    unsigned   srcLclOffs = 0;
    LclVarDsc* dstLcl     = nullptr;
    unsigned   dstLclOffs = 0;

    if (store->GetAddr()->OperIs(GT_LCL_ADDR))
    {
        dstLcl     = store->GetAddr()->AsLclAddr()->GetLcl();
        dstLclOffs = store->GetAddr()->AsLclAddr()->GetLclOffs();
    }

    if (dstLcl == nullptr)
    {
        return;
    }

    if (value->IsIndir() && value->AsIndir()->GetAddr()->OperIs(GT_LCL_ADDR))
    {
        srcLcl     = value->AsIndir()->GetAddr()->AsLclAddr()->GetLcl();
        srcLclOffs = value->AsIndir()->GetAddr()->AsLclAddr()->GetLclOffs();
    }
    else if (value->OperIs(GT_LCL_VAR))
    {
        srcLcl = value->AsLclVar()->GetLcl();
    }

    if ((srcLcl == dstLcl) && (srcLclOffs == dstLclOffs))
    {
        store->ChangeToNothingNode();

        return;
    }

#ifdef FEATURE_SIMD
    if ((dstLclOffs == 0) && varTypeIsSIMD(dstLcl->GetType()))
    {
        if (GenTreeHWIntrinsic* hwi = value->IsHWIntrinsic())
        {
            lvaRecordSimdIntrinsicDef(dstLcl, hwi);
        }
    }
#endif
}

void Importer::gtInitStructLclStore(GenTreeLclVar* store, GenTree* value)
{
    assert(store->OperIs(GT_STORE_LCL_VAR) && varTypeIsStruct(store->GetType()));

    if (value->OperIs(GT_INIT_VAL, GT_CNS_INT))
    {
        return;
    }

    // In the case of a block copy, we want to avoid generating nodes where the source
    // and destination are the same because of two reasons, first, is useless, second
    // it introduces issues in liveness and also copying memory from an overlapping
    // memory location is undefined both as per the ECMA standard and also the memcpy
    // semantics specify that.
    //
    // NOTE: In this case we'll only detect the case for addr of a local and a local
    // itself, any other complex expressions won't be caught.
    //
    // TODO-Cleanup: though having this logic is goodness (i.e. avoids self-copying
    // of struct vars very early), it was added because fgInterBlockLocalVarLiveness()
    // isn't handling self-copying of struct variables correctly. This issue may not
    // surface if struct promotion is ON (which is the case on x86/arm). But still the
    // fundamental issue exists that needs to be addressed.

    LclVarDsc* dstLcl     = store->GetLcl();
    unsigned   dstLclOffs = 0;
    LclVarDsc* srcLcl     = nullptr;
    unsigned   srcLclOffs = 0;

    if (value->IsIndir() && value->AsIndir()->GetAddr()->OperIs(GT_LCL_ADDR))
    {
        srcLcl     = value->AsIndir()->GetAddr()->AsLclAddr()->GetLcl();
        srcLclOffs = value->AsIndir()->GetAddr()->AsLclAddr()->GetLclOffs();
    }
    else if (value->OperIs(GT_LCL_VAR))
    {
        srcLcl = value->AsLclVar()->GetLcl();
    }

    if ((srcLcl == dstLcl) && (srcLclOffs == dstLclOffs))
    {
        store->ChangeToNothingNode();

        return;
    }

#ifdef FEATURE_SIMD
    if ((dstLclOffs == 0) && varTypeIsSIMD(dstLcl->GetType()))
    {
        if (GenTreeHWIntrinsic* hwi = value->IsHWIntrinsic())
        {
            lvaRecordSimdIntrinsicDef(dstLcl, hwi);
        }
    }
#endif
}

GenTree* Importer::impGetStructAddr(GenTree*             value,
                                    CORINFO_CLASS_HANDLE structHnd,
                                    unsigned             curLevel,
                                    bool                 willDereference)
{
    assert(varTypeIsStruct(value->GetType()) || info.compCompHnd->isValueClass(structHnd));

    if (value->OperIs(GT_LCL_VAR))
    {
        return value->ChangeToLclAddr(TYP_BYREF, value->AsLclVar()->GetLcl());
    }

    if (value->OperIs(GT_LCL_FLD))
    {
        return value->ChangeToLclAddr(TYP_BYREF, value->AsLclFld()->GetLcl(), value->AsLclFld()->GetLclOffs(),
                                      value->AsLclFld()->GetFieldSeq());
    }

    if (value->OperIs(GT_OBJ) && willDereference)
    {
        assert(value->AsObj()->GetLayout()->GetClassHandle() == structHnd);
        return value->AsObj()->GetAddr();
    }

    LclVarDsc* tmpLcl = lvaAllocTemp(true DEBUGARG("struct address temp"));
    impAppendTempStore(tmpLcl, value, structHnd, curLevel);
    return gtNewLclVarAddrNode(tmpLcl, TYP_BYREF);
}

GenTree* Importer::impCanonicalizeStructCallArg(GenTree* arg, ClassLayout* argLayout, unsigned curLevel)
{
    assert(arg->GetType() == typGetStructType(argLayout));

    bool spillToTemp = false;

    switch (arg->GetOper())
    {
        case GT_CALL:
            // TODO-MIKE-CQ: We should not need a temp for single reg return calls either.
            spillToTemp = arg->AsCall()->GetRegCount() <= 1;
            break;
        case GT_RET_EXPR:
            spillToTemp = arg->AsRetExpr()->GetCall()->GetRegCount() <= 1;
            break;
        case GT_LCL_VAR:
            assert(arg->GetType() == arg->AsLclVar()->GetLcl()->GetType());
            break;
#ifdef FEATURE_SIMD
        case GT_IND:
#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
#endif
            assert(varTypeIsSIMD(arg->GetType()));
            FALLTHROUGH;
#endif
        case GT_MKREFANY:
        case GT_LCL_FLD:
        case GT_OBJ:
            break;
        default:
            unreached();
    }

    if (spillToTemp)
    {
        LclVarDsc* argLcl = lvaAllocTemp(true DEBUGARG("struct arg temp"));
        impAppendTempStore(argLcl, arg, argLayout, curLevel);
        arg = comp->gtNewLclLoad(argLcl, argLcl->GetType());
    }

    return arg;
}

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
GenTree* Importer::impTokenToHandle(CORINFO_RESOLVED_TOKEN* resolvedToken,
                                    bool                    mustRestoreHandle,
                                    bool                    importParent,
                                    bool*                   runtimeLookup)
{
    CORINFO_GENERICHANDLE_RESULT embedInfo;
    info.compCompHnd->embedGenericHandle(resolvedToken, importParent, &embedInfo);

    if (runtimeLookup != nullptr)
    {
        *runtimeLookup = embedInfo.lookup.lookupKind.needsRuntimeLookup;
    }

    if (!embedInfo.lookup.lookupKind.needsRuntimeLookup)
    {
        if (mustRestoreHandle)
        {
            switch (embedInfo.handleType)
            {
                case CORINFO_HANDLETYPE_CLASS:
                    info.compCompHnd->classMustBeLoadedBeforeCodeIsRun(
                        reinterpret_cast<CORINFO_CLASS_HANDLE>(embedInfo.compileTimeHandle));
                    break;
                case CORINFO_HANDLETYPE_FIELD:
                    info.compCompHnd->classMustBeLoadedBeforeCodeIsRun(info.compCompHnd->getFieldClass(
                        reinterpret_cast<CORINFO_FIELD_HANDLE>(embedInfo.compileTimeHandle)));
                    break;
                case CORINFO_HANDLETYPE_METHOD:
                    info.compCompHnd->methodMustBeLoadedBeforeCodeIsRun(
                        reinterpret_cast<CORINFO_METHOD_HANDLE>(embedInfo.compileTimeHandle));
                    break;
                default:
                    break;
            }
        }

        return comp->gtNewConstLookupTree(embedInfo.lookup.constLookup, TokenToHandleKind(resolvedToken->token),
                                          embedInfo.compileTimeHandle DEBUGARG(embedInfo.compileTimeHandle));
    }

    // Generate the full lookup tree. May be null if we're abandoning an inline attempt.
    GenTree* result = impLookupToTree(resolvedToken, &embedInfo.lookup, TokenToHandleKind(resolvedToken->token),
                                      embedInfo.compileTimeHandle);

    if (result != nullptr)
    {
        result = comp->gtNewRuntimeLookup(embedInfo.compileTimeHandle, embedInfo.handleType, result);
    }

    return result;
}

GenTree* Importer::impLookupToTree(CORINFO_RESOLVED_TOKEN* resolvedToken,
                                   CORINFO_LOOKUP*         lookup,
                                   HandleKind              handleKind,
                                   void*                   compileTimeHandle)
{
    if (!lookup->lookupKind.needsRuntimeLookup)
    {
        return comp->gtNewConstLookupTree(lookup->constLookup, handleKind,
                                          compileTimeHandle DEBUGARG(compileTimeHandle));
    }

    if (lookup->lookupKind.runtimeLookupKind != CORINFO_LOOKUP_NOT_SUPPORTED)
    {
        // Need to use dictionary-based access which depends on the typeContext
        // which is only available at runtime, not at compile-time.
        return impRuntimeLookupToTree(resolvedToken, lookup, compileTimeHandle);
    }

    // Runtime does not support inlining of all shapes of runtime lookups
    // Inlining has to be aborted in such a case
    assert(compIsForInlining());
    compInlineResult->NoteFatal(InlineObservation::CALLSITE_GENERIC_DICTIONARY_LOOKUP);
    return nullptr;
}

GenTree* Importer::impMethodPointer(CORINFO_RESOLVED_TOKEN& resolvedToken, CORINFO_CALL_INFO& callInfo)
{
    if (callInfo.kind == CORINFO_CALL)
    {
        GenTreeMethodAddr* addr = new (comp, GT_METHOD_ADDR) GenTreeMethodAddr(callInfo.hMethod);
#ifdef FEATURE_READYTORUN_COMPILER
        if (opts.IsReadyToRun())
        {
            addr->SetEntryPoint(callInfo.codePointerLookup.constLookup);
        }
#endif
        return addr;
    }

    noway_assert(callInfo.kind == CORINFO_CALL_CODE_POINTER);
    return impLookupToTree(&resolvedToken, &callInfo.codePointerLookup, HandleKind::MethodAddr, callInfo.hMethod);
}

// Import a dictionary lookup to access a handle in code shared between
// generic instantiations.
// The lookup depends on the typeContext which is only available at
// runtime, and not at compile-time.
// pLookup->token1 and pLookup->token2 specify the handle that is needed.
// The cases are:
//
// 1. pLookup->indirections == CORINFO_USEHELPER : Call a helper passing it the
//    instantiation-specific handle, and the tokens to lookup the handle.
// 2. pLookup->indirections != CORINFO_USEHELPER :
//    2a. pLookup->testForNull == false : Dereference the instantiation-specific handle
//        to get the handle.
//    2b. pLookup->testForNull == true : Dereference the instantiation-specific handle.
//        If it is non-NULL, it is the handle required. Else, call a helper
//        to lookup the handle.
GenTree* Importer::impRuntimeLookupToTree(CORINFO_RESOLVED_TOKEN* resolvedToken,
                                          CORINFO_LOOKUP*         lookup,
                                          void*                   compileTimeHandle)
{
    GenTree* ctxTree = gtNewRuntimeContextTree(lookup->lookupKind.runtimeLookupKind);

    CORINFO_RUNTIME_LOOKUP& runtimeLookup = lookup->runtimeLookup;

    // It's available only via the run-time helper function
    if (runtimeLookup.indirections == CORINFO_USEHELPER)
    {
#ifdef FEATURE_READYTORUN_COMPILER
        if (opts.IsReadyToRun())
        {
            return gtNewReadyToRunHelperCallNode(resolvedToken, CORINFO_HELP_READYTORUN_GENERIC_HANDLE, TYP_I_IMPL,
                                                 gtNewCallArgs(ctxTree), &lookup->lookupKind);
        }
#endif
        return gtNewRuntimeLookupHelperCallNode(&runtimeLookup, ctxTree, compileTimeHandle);
    }

    if (!runtimeLookup.testForNull && (runtimeLookup.indirections == 0))
    {
        return ctxTree;
    }

    GenTree* slotPtrTree = ctxTree;

    if (runtimeLookup.testForNull)
    {
        slotPtrTree = impCloneExpr(ctxTree, &ctxTree, CHECK_SPILL_ALL DEBUGARG("impRuntimeLookup slot"));
    }

    GenTree* indOffTree    = nullptr;
    GenTree* lastIndOfTree = nullptr;

    for (uint16_t i = 0; i < runtimeLookup.indirections; i++)
    {
        if ((i == 1 && runtimeLookup.indirectFirstOffset) || (i == 2 && runtimeLookup.indirectSecondOffset))
        {
            indOffTree =
                impCloneExpr(slotPtrTree, &slotPtrTree, CHECK_SPILL_ALL DEBUGARG("impRuntimeLookup indirectOffset"));
        }

        // The last indirection could be subject to a size check (dynamic dictionary expansion)
        bool isLastIndirectionWithSizeCheck =
            ((i == runtimeLookup.indirections - 1) && (runtimeLookup.sizeOffset != CORINFO_NO_SIZE_CHECK));

        if (i != 0)
        {
            slotPtrTree = gtNewIndir(TYP_I_IMPL, slotPtrTree);
            slotPtrTree->gtFlags |= GTF_IND_NONFAULTING;

            if (!isLastIndirectionWithSizeCheck)
            {
                slotPtrTree->gtFlags |= GTF_IND_INVARIANT;
            }
        }

        if ((i == 1 && runtimeLookup.indirectFirstOffset) || (i == 2 && runtimeLookup.indirectSecondOffset))
        {
            slotPtrTree = gtNewOperNode(GT_ADD, TYP_I_IMPL, indOffTree, slotPtrTree);
        }

        if (runtimeLookup.offsets[i] != 0)
        {
            if (isLastIndirectionWithSizeCheck)
            {
                lastIndOfTree = impCloneExpr(slotPtrTree, &slotPtrTree,
                                             CHECK_SPILL_ALL DEBUGARG("impRuntimeLookup indirectOffset"));
            }

            slotPtrTree =
                gtNewOperNode(GT_ADD, TYP_I_IMPL, slotPtrTree, gtNewIconNode(runtimeLookup.offsets[i], TYP_I_IMPL));
        }
    }

    if (!runtimeLookup.testForNull)
    {
        slotPtrTree = gtNewIndir(TYP_I_IMPL, slotPtrTree);
        slotPtrTree->gtFlags |= GTF_IND_NONFAULTING;

        if (!runtimeLookup.testForFixup)
        {
            return slotPtrTree;
        }

        impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("bubbling QMark0"));

        LclVarDsc* slotLcl = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("impRuntimeLookup test"));
        GenTree*   asg     = comp->gtNewLclStore(slotLcl, TYP_I_IMPL, slotPtrTree);
        impSpillNoneAppendTree(asg);

        GenTree* slot = comp->gtNewLclLoad(slotLcl, TYP_I_IMPL);
#ifdef TARGET_64BIT
        slot = gtNewCastNode(slot, false, TYP_INT);
#endif
        // Use a GT_AND to check for the lowest bit and indirect if it is set
        GenTree* test  = gtNewOperNode(GT_AND, TYP_INT, slot, gtNewIconNode(1));
        GenTree* relop = gtNewOperNode(GT_EQ, TYP_INT, test, gtNewIconNode(0));

        // slot = GT_IND(slot - 1)
        slot           = comp->gtNewLclLoad(slotLcl, TYP_I_IMPL);
        GenTree* add   = gtNewOperNode(GT_ADD, TYP_I_IMPL, slot, gtNewIconNode(-1, TYP_I_IMPL));
        GenTree* indir = gtNewIndir(TYP_I_IMPL, add);
        indir->gtFlags |= GTF_IND_NONFAULTING | GTF_IND_INVARIANT;

        asg = comp->gtNewLclStore(slotLcl, TYP_I_IMPL, indir);

        GenTree* qmark = gtNewQmarkNode(TYP_VOID, relop, gtNewNothingNode(), asg);
        impSpillNoneAppendTree(qmark);

        return comp->gtNewLclLoad(slotLcl, TYP_I_IMPL);
    }

    assert(runtimeLookup.indirections != 0);

    impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("bubbling QMark1"));

    // Extract the handle
    GenTree* handleForNullCheck = gtNewIndir(TYP_I_IMPL, slotPtrTree);
    handleForNullCheck->gtFlags |= GTF_IND_NONFAULTING;

    // Call the helper
    // - Setup argValue with the pointer to the signature returned by the lookup
    GenTree* argNode = gtNewIconHandleNode(runtimeLookup.signature, HandleKind::MutableData);
    argNode->AsIntCon()->SetCompileTimeHandle(compileTimeHandle);

    GenTreeCall::Use* helperArgs = gtNewCallArgs(ctxTree, argNode);
    GenTreeCall*      helperCall = gtNewHelperCallNode(runtimeLookup.helper, TYP_I_IMPL, helperArgs);

    // Check for null and possibly call helper
    GenTree* nullCheck       = gtNewOperNode(GT_NE, TYP_INT, handleForNullCheck, gtNewIconNode(0, TYP_I_IMPL));
    GenTree* handleForResult = gtCloneExpr(handleForNullCheck);

    GenTree* result = nullptr;

    if (runtimeLookup.sizeOffset != CORINFO_NO_SIZE_CHECK)
    {
        // Dynamic dictionary expansion support

        assert((lastIndOfTree != nullptr) && (runtimeLookup.indirections > 0));

        // sizeValue = dictionary[pRuntimeLookup->sizeOffset]
        GenTreeIntCon* sizeOffset      = gtNewIconNode(runtimeLookup.sizeOffset, TYP_I_IMPL);
        GenTree*       sizeValueOffset = gtNewOperNode(GT_ADD, TYP_I_IMPL, lastIndOfTree, sizeOffset);
        GenTree*       sizeValue       = gtNewIndir(TYP_I_IMPL, sizeValueOffset);
        sizeValue->gtFlags |= GTF_IND_NONFAULTING;

        // sizeCheck fails if sizeValue < pRuntimeLookup->offsets[i]
        GenTree* offsetValue = gtNewIconNode(runtimeLookup.offsets[runtimeLookup.indirections - 1], TYP_I_IMPL);
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

    LclVarDsc* tmpLcl = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("spilling Runtime Lookup tree"));
    GenTree*   asg    = comp->gtNewLclStore(tmpLcl, TYP_I_IMPL, result);
    impSpillNoneAppendTree(asg);
    return comp->gtNewLclLoad(tmpLcl, TYP_I_IMPL);
}

void Importer::impImportDup()
{
    // If the expression to dup is simple, just clone it.
    // Otherwise spill it to a temp, and reload the temp twice.

    StackEntry se  = impPopStack();
    GenTree*   op1 = se.val;
    GenTree*   op2 = nullptr;

    if (impIsAddressInLocal(op1))
    {
        // Always clone local addresses, otherwise the local will end up being address exposed.
        op2 = gtCloneExpr(op1);
    }
    else if (op1->IsIntegralConst(0) || op1->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        if ((op1->gtFlags & GTF_GLOB_EFFECT) == 0)
        {
            op2 = gtClone(op1, true);
        }
    }

    if (op2 == nullptr)
    {
        // TODO-MIKE-Cleanup: This should just use impSpillStackEntry. But it looks like
        // impSpillStackEntry's special RET_EXPR handling hurts CQ...

        LclVarDsc* lcl = lvaAllocTemp(true DEBUGARG("dup spill"));
        impAppendTempStore(lcl, op1, se.seTypeInfo.GetClassHandle(), CHECK_SPILL_ALL);

        if (!opts.compDbgCode && lcl->TypeIs(TYP_REF))
        {
            assert(lcl->lvSingleDef == 0);
            lcl->lvSingleDef = 1;
            JITDUMP("Marked V%02u as a single def local\n", lcl->GetLclNum());
            comp->lvaSetClass(lcl, op1, se.seTypeInfo.GetClassHandle());
        }

        op1 = gtNewLclvNode(lcl, varActualType(lcl->GetType()));
        op2 = gtNewLclvNode(lcl, varActualType(lcl->GetType()));
    }

    impPushOnStack(op1, se.seTypeInfo);
    impPushOnStack(op2, se.seTypeInfo);
}

// Spills the stack at verCurrentState.esStack[level] and replaces it with a temp.
void Importer::impSpillStackEntry(unsigned level DEBUGARG(const char* reason))
{
    StackEntry& se   = verCurrentState.esStack[level];
    GenTree*    tree = se.val;

    LclVarDsc* lcl = lvaAllocTemp(true DEBUGARG(reason));
    impAppendTempStore(lcl, tree, se.seTypeInfo.GetClassHandle(), level);

    if (lcl->TypeIs(TYP_REF))
    {
        assert(lcl->lvSingleDef == 0);
        lcl->lvSingleDef = 1;
        JITDUMP("Marked V%02u as a single def temp\n", lcl->GetLclNum());
        comp->lvaSetClass(lcl, tree, se.seTypeInfo.GetClassHandle());

        // If we're assigning a GT_RET_EXPR, note the temp over on the call,
        // so the inliner can use it in case it needs a return spill temp.
        if (GenTreeRetExpr* retExpr = tree->IsRetExpr())
        {
            JITDUMP("\n*** see V%02u = GT_RET_EXPR, noting temp\n", lcl->GetLclNum());

            assert(retExpr->GetRetExpr() == retExpr->GetCall());
            retExpr->GetCall()->gtInlineCandidateInfo->preexistingSpillTemp = lcl;
        }
    }

    se.val = gtNewLclvNode(lcl, varActualType(lcl->GetType()));
}

void Importer::EnsureStackSpilled(bool ignoreLeaves DEBUGARG(const char* reason))
{
    for (unsigned level = 0; level < verCurrentState.esStackDepth; level++)
    {
        GenTree* tree = verCurrentState.esStack[level].val;

        if (ignoreLeaves && tree->OperIsLeaf())
        {
            continue;
        }

        // Temps introduced by the importer itself don't need to be spilled.
        if (tree->OperIs(GT_LCL_VAR) && (tree->AsLclVar()->GetLcl()->GetLclNum() >= info.compLocalsCount))
        {
            continue;
        }

        impSpillStackEntry(level DEBUGARG(reason));
    }
}

// If the stack contains any trees with side effects in them, assign those trees
// to temps and replace them on the stack with LCL_VARs referencing those temps.
// [0..spillDepth) is the portion of the stack which will be checked and spilled.
void Importer::impSpillSideEffects(GenTreeFlags spillSideEffects, unsigned spillDepth DEBUGARG(const char* reason))
{
    spillDepth = min(spillDepth, verCurrentState.esStackDepth);

    for (unsigned i = 0; i < spillDepth; i++)
    {
        GenTree* tree = verCurrentState.esStack[i].val;

        if (impIsAddressInLocal(tree))
        {
            // Trees that represent local addresses may have spurious GLOB_REF
            // side effects but they never need to be spilled.
            continue;
        }

        GenTreeFlags treeSideEffects = tree->GetSideEffects();

        // We haven't yet determined which local variables are address exposed
        // so we cannot rely on GTF_GLOB_REF being present in trees that use
        // such locals. Conservatively assume that address taken locals will be
        // address exposed and add GTF_GLOB_REF.
        if (((spillSideEffects & GTF_GLOB_REF) != 0) && impHasAddressTakenLocals(tree))
        {
            treeSideEffects |= GTF_GLOB_REF;
        }

        if ((treeSideEffects & spillSideEffects) != 0)
        {
            impSpillStackEntry(i DEBUGARG(reason));
        }
    }
}

void Importer::SpillCatchArg()
{
    assert(handlerGetsXcptnObj(currentBlock->bbCatchTyp));
    assert(verCurrentState.esStackDepth == 1);
    assert(impStackTop().val->OperIs(GT_CATCH_ARG));

    StackEntry& se = verCurrentState.esStack[0];

    LclVarDsc* lcl   = lvaNewTemp(TYP_REF, true DEBUGARG("catch arg spill temp"));
    lcl->lvSingleDef = true;
    JITDUMP("Marked V%02u as a single def temp\n", lcl->GetLclNum());
    comp->lvaSetClass(lcl, se.val, se.seTypeInfo.GetClassHandle());

    GenTree* store = comp->gtNewLclStore(lcl, TYP_REF, se.val);
    impSpillNoneAppendTree(store);
    se.val = comp->gtNewLclLoad(lcl, TYP_REF);
}

// Spill all trees containing references to the specified local.
void Importer::impSpillLclReferences(LclVarDsc* lcl)
{
    for (unsigned level = 0; level < verCurrentState.esStackDepth; level++)
    {
        GenTree* tree = verCurrentState.esStack[level].val;

        // These never need to be spilled, they're basically constants. However, if they are
        // used by indirections then those indirections need spilling. impHasLclRef does not
        // check for indirections so we have to spill any tree that contains these. At least
        // avoid spilling when the tree is just a local address node.
        if (tree->OperIs(GT_LCL_ADDR))
        {
            continue;
        }

        // If the tree may throw an exception, and the block has a handler, then we need
        // to spill assignments to the local if the local is live on entry to the handler.
        // We don't have liveness during import so we simply spill them all.

        bool xcptnCaught = ((tree->gtFlags & (GTF_CALL | GTF_EXCEPT)) != 0) && ehBlockHasExnFlowDsc(currentBlock);

        if (xcptnCaught || impHasLclRef(tree, lcl))
        {
            impSpillStackEntry(level DEBUGARG("STLOC stack spill temp"));
        }
    }
}

class ImportSpillCliqueState
{
    unsigned const hasCatchArg : 1;
    unsigned const spillTempCount : 31;
    union {
        CORINFO_CLASS_HANDLE const catchArgType;
        LclVarDsc* const           spillTemps;
    };

public:
    ImportSpillCliqueState(CORINFO_CLASS_HANDLE catchArgType)
        : hasCatchArg(1), spillTempCount(0), catchArgType(catchArgType)
    {
    }

    ImportSpillCliqueState(LclVarDsc* spillTemps, unsigned spillTempCount)
        : hasCatchArg(0), spillTempCount(spillTempCount), spillTemps(spillTemps)
    {
    }

    bool HasCatchArg() const
    {
        return hasCatchArg;
    }

    CORINFO_CLASS_HANDLE GetCatchArgType() const
    {
        assert(hasCatchArg);
        return catchArgType;
    }

    unsigned GetSpillTempCount() const
    {
        return spillTempCount;
    }

    LclVarDsc* GetSpillTemps() const
    {
        assert(!hasCatchArg);
        return spillTemps;
    }

    unsigned GetStackDepth() const
    {
        return hasCatchArg ? 1 : spillTempCount;
    }
};

// Push catch arg onto the stack.
// If there are jumps to the beginning of the handler, insert basic block
// and spill catch arg to a temp. Update the handler block if necessary.
// Returns the basic block of the actual handler.
BasicBlock* Importer::impPushCatchArgOnStack(BasicBlock* hndBlk, CORINFO_CLASS_HANDLE clsHnd, bool isSingleBlockFilter)
{
    // Do not inject the basic block twice on reimport. This should be
    // hit only under JIT stress. See if the block is the one we injected.
    // Note that EH canonicalization can inject internal blocks here. We might
    // be able to re-use such a block (but we don't, right now).
    if ((hndBlk->bbFlags & (BBF_IMPORTED | BBF_INTERNAL | BBF_DONT_REMOVE)) ==
        (BBF_IMPORTED | BBF_INTERNAL | BBF_DONT_REMOVE))
    {
        Statement* stmt = hndBlk->firstStmt();

        if (stmt != nullptr)
        {
            GenTree* tree = stmt->GetRootNode();
            assert(tree != nullptr);

            if (tree->OperIs(GT_STORE_LCL_VAR) && tree->AsLclVar()->GetOp(0)->OperIs(GT_CATCH_ARG))
            {
                tree = comp->gtNewLclLoad(tree->AsLclVar()->GetLcl(), TYP_REF);

                assert(hndBlk->bbEntryState->HasCatchArg());

                return hndBlk->bbNext;
            }
        }

        // If we get here, it must have been some other kind of internal block. It's possible that
        // someone prepended something to our injected block, but that's unlikely.
    }

#ifdef JIT32_GCENCODER
    const bool forceInsertNewBlock = isSingleBlockFilter || comp->compStressCompile(Compiler::STRESS_CATCH_ARG, 5);
#else
    const bool forceInsertNewBlock = comp->compStressCompile(Compiler::STRESS_CATCH_ARG, 5);
#endif

    // Spill GT_CATCH_ARG to a temp if there are jumps to the beginning of the handler
    if ((hndBlk->bbRefs <= 1) && !forceInsertNewBlock)
    {
        hndBlk->bbEntryState = new (comp, CMK_Importer) ImportSpillCliqueState(clsHnd);
    }
    else
    {
        if (hndBlk->bbRefs == 1)
        {
            hndBlk->bbRefs++;
        }

        // Create extra basic block for the spill
        BasicBlock* newBlk = fgNewBBbefore(BBJ_NONE, hndBlk, /* extendRegion */ true);
        newBlk->bbFlags |= BBF_IMPORTED | BBF_DONT_REMOVE;
        newBlk->inheritWeight(hndBlk);
        newBlk->bbCodeOffs   = hndBlk->bbCodeOffs;
        newBlk->bbEntryState = new (comp, CMK_Importer) ImportSpillCliqueState(clsHnd);

        // Account for the new link we are about to create
        hndBlk->bbRefs++;

        // Spill into a temp.
        LclVarDsc* tempLcl = lvaNewTemp(TYP_REF, false DEBUGARG("CATCH_ARG spill temp"));
        GenTree*   argAsg  = comp->gtNewLclStore(tempLcl, TYP_REF, impNewCatchArg());
        Statement* argStmt;

        if ((compStmtOffsetsImplicit & ICorDebugInfo::CALL_SITE_BOUNDARIES) != 0)
        {
            // Report the debug info. impImportBlockCode won't treat the actual handler
            // as exception block and thus won't do it for us.
            impCurStmtOffs = newBlk->bbCodeOffs | IL_OFFSETX_STKBIT;
            argStmt        = gtNewStmt(argAsg, impCurStmtOffs);
        }
        else
        {
            argStmt = gtNewStmt(argAsg);
        }

        fgInsertStmtAtEnd(newBlk, argStmt);

        if (comp->fgCheapPredsValid)
        {
            fgAddCheapPred(hndBlk, newBlk);
        }

        impSetSpillCliqueState(newBlk, new (comp, CMK_Importer) ImportSpillCliqueState(tempLcl, 1));
    }

    return hndBlk;
}

GenTree* Importer::impNewCatchArg()
{
    GenTree* arg = new (comp, GT_CATCH_ARG) GenTree(GT_CATCH_ARG, TYP_REF);
    // GT_CATCH_ARG cannot be moved around since it uses a fixed register on x86 (EAX).
    arg->gtFlags |= GTF_ORDER_SIDEEFF;
    return arg;
}

// Given a tree, clone it. *pClone is set to the cloned tree.
// Returns the original tree if the cloning was easy,
// else returns the temp to which the tree had to be spilled to.
// If the tree has side-effects, it will be spilled to a temp.
GenTree* Importer::impCloneExpr(GenTree* tree, GenTree** clone, unsigned spillCheckLevel DEBUGARG(const char* reason))
{
    GenTree* uses[2];
    impMakeMultiUse(tree, 2, uses, spillCheckLevel DEBUGARG(reason));
    *clone = uses[1];
    return uses[0];
}

GenTree* Importer::impCloneExpr(GenTree*     tree,
                                GenTree**    clone,
                                ClassLayout* layout,
                                unsigned spillCheckLevel DEBUGARG(const char* reason))
{
    GenTree* uses[2];
    impMakeMultiUse(tree, 2, uses, layout, spillCheckLevel DEBUGARG(reason));
    *clone = uses[1];
    return uses[0];
}

void Importer::impMakeMultiUse(GenTree*  tree,
                               unsigned  useCount,
                               GenTree** uses,
                               unsigned spillCheckLevel DEBUGARG(const char* reason))
{
    assert(!varTypeIsStruct(tree->GetType()));
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

    SpillStackCheck(tree, spillCheckLevel);
    var_types  type = varActualType(tree->GetType());
    LclVarDsc* lcl  = lvaNewTemp(type, true DEBUGARG(reason));
    impSpillNoneAppendTree(comp->gtNewLclStore(lcl, type, tree));

    for (unsigned i = 0; i < useCount; i++)
    {
        uses[i] = comp->gtNewLclLoad(lcl, type);
    }
}

void Importer::impMakeMultiUse(GenTree*     tree,
                               unsigned     useCount,
                               GenTree**    uses,
                               ClassLayout* layout,
                               unsigned spillCheckLevel DEBUGARG(const char* reason))
{
    assert(varTypeIsStruct(tree->GetType()) && (layout != nullptr));
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

    LclVarDsc* lcl = lvaAllocTemp(true DEBUGARG(reason));
    impAppendTempStore(lcl, tree, layout, spillCheckLevel);

    var_types type = varActualType(lcl->GetType());

    for (unsigned i = 0; i < useCount; i++)
    {
        uses[i] = gtNewLclvNode(lcl, type);
    }
}

NOINLINE unsigned Importer::AdvanceStmtOffset(unsigned nextStmtIndex, unsigned opcodeOffs)
{
    if (opts.compDbgCode)
    {
        if (verCurrentState.esStackDepth != 0)
        {
            EnsureStackSpilled(false DEBUGARG("debug info spill"));
        }
        else if (impCurStmtOffs != BAD_IL_OFFSET)
        {
            // Somehow we did not generate any statements for the current offset,
            // add a nop statement so that the current offset gets reported.
            impSpillNoneAppendTree(new (comp, GT_NO_OP) GenTree(GT_NO_OP, TYP_VOID));
        }

        assert(impCurStmtOffs == BAD_IL_OFFSET);
    }

    if (impCurStmtOffs == BAD_IL_OFFSET)
    {
        // Make sure that nextStmtIndex is in sync with opcodeOffs.
        while ((nextStmtIndex + 1 < compStmtOffsetsCount) && (compStmtOffsets[nextStmtIndex + 1] <= opcodeOffs))
        {
            nextStmtIndex++;
        }

        impCurStmtOffsSet(compStmtOffsets[nextStmtIndex]);

        nextStmtIndex++;
        assert(nextStmtIndex <= compStmtOffsetsCount);
    }

    return nextStmtIndex;
}

// Remember the IL offset (including stack-empty info) for the trees we will generate now.
void Importer::impCurStmtOffsSet(IL_OFFSET offs)
{
    assert(!compIsForInlining());
    assert((offs != BAD_IL_OFFSET) && ((offs & IL_OFFSETX_BITS) == 0));

    IL_OFFSETX stkBit = (verCurrentState.esStackDepth > 0) ? IL_OFFSETX_STKBIT : 0;
    impCurStmtOffs    = offs | stkBit;
}

IL_OFFSETX Importer::GetCallILOffsetX(IL_OFFSET offs)
{
    assert((offs != BAD_IL_OFFSET) && ((offs & IL_OFFSETX_BITS) == 0));

    if (compIsForInlining())
    {
        return BAD_IL_OFFSET;
    }

    IL_OFFSETX stkBit = (verCurrentState.esStackDepth > 0) ? IL_OFFSETX_STKBIT : 0;
    return offs | IL_OFFSETX_CALLINSTRUCTIONBIT | stkBit;
}

//------------------------------------------------------------------------
// impCanSpillNow: check is it possible to spill all values from eeStack to local variables.
//
// Arguments:
//    prevOpcode - last importer opcode
//
// Return Value:
//    true if it is legal, false if it could be a sequence that we do not want to divide.
bool Importer::impCanSpillNow(OPCODE prevOpcode)
{
    // Don't spill after ldtoken, newarr and newobj, because it could be a part of the InitializeArray sequence.
    // Avoid breaking up to guarantee that impInitializeArrayIntrinsic can succeed.
    return (prevOpcode != CEE_LDTOKEN) && (prevOpcode != CEE_NEWARR) && (prevOpcode != CEE_NEWOBJ);
}

// We don't create any GenTree (excluding spills) for a branch.
// For debugging info, we need a placeholder so that we can note
// the IL offset in gtStmt.gtStmtOffs. So append an empty statement.
void Importer::impNoteBranchOffs()
{
    if (opts.compDbgCode)
    {
        impSpillNoneAppendTree(gtNewNothingNode());
    }
}

// Locate the next stmt boundary for which we need to record info.
// We will have to spill the stack at such boundaries if it is not
// already empty.
// Returns the next stmt boundary (after the start of the block)
unsigned Importer::impInitBlockLineInfo(BasicBlock* block)
{
    if (compIsForInlining())
    {
        // TODO-MIKE-Review: This is dubious. We're reporting an offset that may
        // have already been reported by the inliner due to CALL_SITE_BOUNDARIES.
        impCurStmtOffs = impInlineInfo->iciStmt->GetILOffsetX();

        return UINT32_MAX;
    }

    // Assume the block does not correspond with any IL offset. This prevents
    // us from reporting extra offsets. Extra mappings can cause confusing
    // stepping, especially if the extra mapping is a jump-target, and the
    // debugger does not ignore extra mappings, but instead rewinds to the
    // nearest known offset.

    impCurStmtOffs = BAD_IL_OFFSET;

    IL_OFFSET blockOffs = block->bbCodeOffs;

    if ((verCurrentState.esStackDepth == 0) && (compStmtOffsetsImplicit & ICorDebugInfo::STACK_EMPTY_BOUNDARIES))
    {
        impCurStmtOffsSet(blockOffs);
    }

    // Always report IL offset 0 or some tests get confused.
    if (blockOffs == 0)
    {
        impCurStmtOffsSet(blockOffs);
    }

    if (compStmtOffsetsCount == 0)
    {
        return UINT32_MAX;
    }

    // Find the lowest explicit stmt boundary within the block.

    // Guess a starting index to avoid searching the entire offset array.
    unsigned index = (compStmtOffsetsCount * blockOffs) / info.compILCodeSize;

    if (index >= compStmtOffsetsCount)
    {
        index = compStmtOffsetsCount - 1;
    }

    while ((index > 0) && (compStmtOffsets[index - 1] >= blockOffs))
    {
        index--;
    }

    while (compStmtOffsets[index] < blockOffs)
    {
        index++;

        if (index == compStmtOffsetsCount)
        {
            return index;
        }
    }

    assert(index < compStmtOffsetsCount);

    if (compStmtOffsets[index] == blockOffs)
    {
        impCurStmtOffsSet(blockOffs);
        index++;
    }

    return index;
}

bool Importer::impOpcodeIsCallOpcode(OPCODE opcode)
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

static bool impOpcodeIsCallSiteBoundary(OPCODE opcode)
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

CORINFO_CLASS_HANDLE Compiler::impGetStringClass()
{
    CORINFO_CLASS_HANDLE stringClass = info.compCompHnd->getBuiltinClass(CLASSID_STRING);
    assert(stringClass != nullptr);
    return stringClass;
}

CORINFO_CLASS_HANDLE Compiler::impGetObjectClass()
{
    CORINFO_CLASS_HANDLE objectClass = info.compCompHnd->getBuiltinClass(CLASSID_SYSTEM_OBJECT);
    assert(objectClass != nullptr);
    return objectClass;
}

void Importer::impBashVarAddrsToI(GenTree* tree1, GenTree* tree2)
{
    // "&local" can be used either as TYP_BYREF or TYP_I_IMPL, but we
    // set its type to TYP_BYREF when we create it. We know if it can
    // be changed to TYP_I_IMPL only at the point where we use it.

    if (tree1->TypeIs(TYP_BYREF) && impIsLocalAddrExpr(tree1))
    {
        tree1->SetType(TYP_I_IMPL);
    }

    if ((tree2 != nullptr) && tree2->TypeIs(TYP_BYREF) && impIsLocalAddrExpr(tree2))
    {
        tree2->SetType(TYP_I_IMPL);
    }
}

// INT and I_IMPL can be used almost interchangeably, but we want
// to make that an explicit cast in our trees, so any implicit casts
// that exist in the IL (at least on 64-bit where I_IMPL != INT) are
// turned into explicit casts here.
// We also allow an implicit conversion of a ldnull into a I_IMPL(0)
GenTree* Importer::impImplicitIorI4Cast(GenTree* tree, var_types dstTyp)
{
    var_types currType   = varActualType(tree->GetType());
    var_types wantedType = varActualType(dstTyp);

    if (wantedType != currType)
    {
        // Automatic upcast for a GT_CNS_INT into TYP_I_IMPL
        if (tree->IsIntCon() && varTypeIsI(dstTyp))
        {
            if (!varTypeIsI(tree->GetType()) || (tree->TypeIs(TYP_REF) && (tree->AsIntCon()->GetValue() == 0)))
            {
                tree->SetType(TYP_I_IMPL);
            }
        }
#ifdef TARGET_64BIT
        else if (varTypeIsI(wantedType) && (currType == TYP_INT))
        {
            // Note that this allows TYP_INT to be cast to a TYP_I_IMPL when wantedType is a TYP_BYREF or TYP_REF
            tree = gtNewCastNode(tree, false, TYP_LONG);
        }
        else if ((wantedType == TYP_INT) && varTypeIsI(currType))
        {
            // Note that this allows TYP_BYREF or TYP_REF to be cast to a TYP_INT
            tree = gtNewCastNode(tree, false, TYP_INT);
        }
#endif // TARGET_64BIT
    }

    return tree;
}

// FLOAT and DOUBLE can be used almost interchangeably in some cases,
// but we want to make that an explicit cast in our trees, so any implicit
// casts that exist in the IL are turned into explicit casts here.
GenTree* Importer::impImplicitR4orR8Cast(GenTree* tree, var_types dstTyp)
{
    if (varTypeIsFloating(tree->GetType()) && varTypeIsFloating(dstTyp) && (dstTyp != tree->GetType()))
    {
        tree = gtNewCastNode(tree, false, dstTyp);
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

GenTree* Importer::impInitializeArrayIntrinsic(CORINFO_SIG_INFO* sig)
{
    assert(sig->numArgs == 2);

    GenTreeCall* fieldTokenCall = impStackTop(0).val->IsCall();
    GenTree*     arrayLocalNode = impStackTop(1).val;

    // Verify that the field token is known and valid. Note that It's also
    // possible for the token to come from reflection, in which case we cannot do
    // the optimization and must therefore revert to calling the helper. You can
    // see an example of this in bvt\DynIL\initarray2.exe (in Main).

    if ((fieldTokenCall == nullptr) || !fieldTokenCall->IsHelperCall() ||
        (Compiler::eeGetHelperNum(fieldTokenCall->GetMethodHandle()) != CORINFO_HELP_FIELDDESC_TO_STUBRUNTIMEFIELD))
    {
        return nullptr;
    }

    GenTree* fieldTokenNode = fieldTokenCall->gtCallArgs->GetNode();

    if (fieldTokenNode->OperIs(GT_IND))
    {
        fieldTokenNode = fieldTokenNode->AsIndir()->GetAddr();
    }

    if (!fieldTokenNode->IsIntCon(HandleKind::Field))
    {
        return nullptr;
    }

    CORINFO_FIELD_HANDLE fieldHandle = fieldTokenNode->AsIntCon()->GetCompileTimeFieldHandle();
    if (fieldHandle == nullptr)
    {
        return nullptr;
    }

    // We need to get the number of elements in the array and the size of each element.
    // We verify that the newarr statement is exactly what we expect it to be.
    // If it's not then we just return NULL and we don't optimize this call

    // It is possible the we don't have any statements in the block yet.
    if (impLastStmt == nullptr)
    {
        return nullptr;
    }

    // We start by looking at the last statement, making sure it's a store, and
    // that the target of the store is the array passed to InitializeArray.
    GenTree* arrayStore = impLastStmt->GetRootNode();

    if (!arrayStore->OperIs(GT_STORE_LCL_VAR) || !arrayLocalNode->OperIs(GT_LCL_VAR) ||
        (arrayStore->AsLclVar()->GetLcl() != arrayLocalNode->AsLclVar()->GetLcl()))
    {
        return nullptr;
    }

    // Make sure that the object being assigned is a helper call.

    GenTreeCall* newArrayCall = arrayStore->AsLclVar()->GetOp(0)->IsCall();

    if ((newArrayCall == nullptr) || !newArrayCall->IsHelperCall())
    {
        return nullptr;
    }

    // Verify that it is one of the new array helpers.

    bool            isMDArray = false;
    CorInfoHelpFunc helper    = Compiler::eeGetHelperNum(newArrayCall->GetMethodHandle());

    if ((helper != CORINFO_HELP_NEWARR_1_DIRECT) && (helper != CORINFO_HELP_NEWARR_1_OBJ) &&
        (helper != CORINFO_HELP_NEWARR_1_VC) && (helper != CORINFO_HELP_NEWARR_1_ALIGN8)
#ifdef FEATURE_READYTORUN_COMPILER
        && (helper != CORINFO_HELP_READYTORUN_NEWARR_1)
#endif
            )
    {
        if (helper != CORINFO_HELP_NEW_MDARR_NONVARARG)
        {
            return nullptr;
        }

        isMDArray = true;
    }

    CORINFO_CLASS_HANDLE arrayClsHnd =
        reinterpret_cast<CORINFO_CLASS_HANDLE>(newArrayCall->compileTimeHelperArgumentHandle);

    if (arrayClsHnd == nullptr)
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

        // The number of arguments should be a constant between 1 and 64. The rank can't be 0
        // so at least one length must be present and the rank can't exceed 32 so there can
        // be at most 64 arguments - 32 lengths and 32 lower bounds.

        if ((!numArgsArg->GetNode()->IsIntCon()) || (numArgsArg->GetNode()->AsIntCon()->GetValue() < 1) ||
            (numArgsArg->GetNode()->AsIntCon()->GetValue() > 64))
        {
            return nullptr;
        }

        unsigned numArgs = numArgsArg->GetNode()->AsIntCon()->GetUInt32Value();
        bool     lowerBoundsSpecified;

        if (numArgs == rank * 2)
        {
            lowerBoundsSpecified = true;
        }
        else if (numArgs == rank)
        {
            lowerBoundsSpecified = false;

            // If the rank is 1 and a lower bound isn't specified then the runtime creates
            // a SDArray. Note that even if a lower bound is specified it can be 0 and then
            // we get a SDArray as well, see the for loop below.

            if (rank == 1)
            {
                isMDArray = false;
            }
        }
        else
        {
            return nullptr;
        }

        // The rank is known to be at least 1 so we can start with numElements being 1
        // to avoid the need to special case the first dimension.

        numElements = S_UINT32(1);

        struct Match
        {
            static bool IsArgsFieldInit(GenTree* tree, unsigned index, LclVarDsc* argLcl)
            {
                return tree->OperIs(GT_STORE_LCL_FLD) && (tree->AsLclFld()->GetLclOffs() == 4 * index) &&
                       (tree->AsLclFld()->GetLcl() == argLcl);
            }

            static bool IsArgsAddr(GenTree* tree, LclVarDsc* argLcl)
            {
                return tree->OperIs(GT_LCL_ADDR) && (tree->AsLclAddr()->GetLcl() == argLcl) &&
                       (tree->AsLclAddr()->GetLclOffs() == 0);
            }

            static bool IsComma(GenTree* tree)
            {
                return (tree != nullptr) && tree->OperIs(GT_COMMA);
            }
        };

        INDEBUG(LclVarDsc* newObjArrayArgsLcl = comp->lvaGetDesc(comp->lvaNewObjArrayArgs);)
        unsigned argIndex = 0;
        GenTree* comma;

        for (comma = argsArg->GetNode(); Match::IsComma(comma); comma = comma->gtGetOp2())
        {
            if (lowerBoundsSpecified)
            {
                // In general lower bounds can be ignored because they're not needed to
                // calculate the total number of elements. But for single dimensional arrays
                // we need to know if the lower bound is 0 because in this case the runtime
                // creates a SDArray and this affects the way the array data offset is calculated.

                if (rank == 1)
                {
                    GenTree* lowerBoundStore = comma->AsOp()->GetOp(0);
                    assert(Match::IsArgsFieldInit(lowerBoundStore, argIndex, newObjArrayArgsLcl));
                    GenTree* lowerBoundNode = lowerBoundStore->AsLclFld()->GetOp(0);

                    if (lowerBoundNode->IsIntegralConst(0))
                    {
                        isMDArray = false;
                    }
                }

                comma = comma->AsOp()->GetOp(1);
                argIndex++;
            }

            GenTree* lengthNodeStore = comma->AsOp()->GetOp(0);
            assert(Match::IsArgsFieldInit(lengthNodeStore, argIndex, newObjArrayArgsLcl));
            GenTree* lengthNode = lengthNodeStore->AsLclFld()->GetOp(0);

            if (!lengthNode->IsIntCon())
            {
                return nullptr;
            }

            numElements *= S_SIZE_T(lengthNode->AsIntCon()->GetValue());
            argIndex++;
        }

        assert((comma != nullptr) && Match::IsArgsAddr(comma, newObjArrayArgsLcl));

        if (argIndex != numArgs)
        {
            return nullptr;
        }
    }
    else
    {
        // Make sure there are exactly two arguments: the array class and
        // the number of elements.

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

        // This optimization is only valid for a constant array size.
        if (!arrayLengthNode->IsIntCon())
        {
            return nullptr;
        }

        numElements = S_SIZE_T(arrayLengthNode->AsIntCon()->GetValue());

        if (!info.compCompHnd->isSDArray(arrayClsHnd))
        {
            return nullptr;
        }
    }

    CORINFO_CLASS_HANDLE elemClsHnd;
    var_types            elementType = CorTypeToVarType(info.compCompHnd->getChildType(arrayClsHnd, &elemClsHnd));

    // Note that genTypeSize will return zero for non primitive types, which is exactly
    // what we want (size will then be 0, and we will catch this in the conditional below).

    S_UINT32 elemSize(varTypeSize(elementType));
    S_UINT32 size = elemSize * S_UINT32(numElements);

    if (size.IsOverflow())
    {
        return nullptr;
    }

    if ((size.Value() == 0) || varTypeIsGC(elementType))
    {
        return nullptr;
    }

    void* initData = info.compCompHnd->getArrayInitializationData(fieldHandle, size.Value());
    if (initData == nullptr)
    {
        return nullptr;
    }

    // At this point we are ready to commit to implementing the InitializeArray
    // intrinsic using a struct store. Pop the arguments from the stack and
    // return the struct store node.

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

    GenTree*    srcAddr = gtNewIconHandleNode(initData, HandleKind::ConstData);
    GenTreeBlk* load    = new (comp, GT_BLK) GenTreeBlk(srcAddr, typGetBlkLayout(blkSize));
    GenTree*    dstAddr = gtNewOperNode(GT_ADD, TYP_BYREF, arrayLocalNode, gtNewIconNode(dataOffset, TYP_I_IMPL));
    GenTreeBlk* store   = new (comp, GT_STORE_BLK) GenTreeBlk(dstAddr, load, load->GetLayout());

    load->gtFlags &= ~GTF_EXCEPT;
    load->gtFlags |= GTF_IND_NONFAULTING | GTF_IND_INVARIANT;
    store->gtFlags &= ~GTF_EXCEPT;
    store->gtFlags |= GTF_IND_NONFAULTING;

    INDEBUG(srcAddr->AsIntCon()->SetDumpHandle(reinterpret_cast<void*>(THT_IntializeArrayIntrinsics)));

    return store;
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

GenTree* Importer::impIntrinsic(GenTree*                newobjThis,
                                CORINFO_SIG_INFO*       sig,
                                unsigned                methodFlags,
                                CORINFO_RESOLVED_TOKEN* resolvedToken,
                                bool                    readonlyCall,
                                bool                    tailCall,
                                CORINFO_RESOLVED_TOKEN* constrainedResolvedToken,
                                CORINFO_CALL_INFO*      callInfo,
                                CorInfoIntrinsics*      pIntrinsicId,
                                bool*                   isSpecialIntrinsic)
{
    assert((methodFlags & (CORINFO_FLG_INTRINSIC | CORINFO_FLG_JIT_INTRINSIC)) != 0);

    CORINFO_CLASS_HANDLE  clsHnd      = resolvedToken->hClass;
    CORINFO_METHOD_HANDLE method      = callInfo->hMethod;
    bool                  mustExpand  = false;
    CorInfoIntrinsics     intrinsicId = CORINFO_INTRINSIC_Illegal;

    if ((methodFlags & CORINFO_FLG_INTRINSIC) != 0)
    {
        intrinsicId = info.compCompHnd->getIntrinsicID(method, &mustExpand);
        assert((intrinsicId == CORINFO_INTRINSIC_Illegal) || (intrinsicId < CORINFO_INTRINSIC_Count));

        // These intrinsics must be supported regardless of DbgCode and MinOpts.

        switch (intrinsicId)
        {
            case CORINFO_INTRINSIC_StubHelpers_NextCallReturnAddress:
                // For now we just avoid inlining anything into these methods since
                // this intrinsic is only rarely used. We could do this better if we
                // wanted to by trying to match which call is the one we need to get
                // the return address of.
                info.compHasNextCallRetAddr = true;
                return new (comp, GT_LABEL) GenTree(GT_LABEL, TYP_I_IMPL);
            case CORINFO_INTRINSIC_StubHelpers_GetStubContext:
                noway_assert(comp->lvaStubArgumentVar != BAD_VAR_NUM);
                return gtNewLclvNode(comp->lvaGetDesc(comp->lvaStubArgumentVar), TYP_I_IMPL);
#ifdef TARGET_64BIT
            case CORINFO_INTRINSIC_StubHelpers_GetStubContextAddr:
                noway_assert(comp->lvaStubArgumentVar != BAD_VAR_NUM);
                lvaSetAddressExposed(comp->lvaGetDesc(comp->lvaStubArgumentVar));
                return gtNewLclVarAddrNode(comp->lvaGetDesc(comp->lvaStubArgumentVar), TYP_I_IMPL);
#endif
            default:
                assert(intrinsicId != CORINFO_INTRINSIC_StubHelpers_GetStubContextAddr);
                break;
        }
    }

    NamedIntrinsic ni = NI_Illegal;

    if ((methodFlags & CORINFO_FLG_JIT_INTRINSIC) != 0)
    {
        // The recursive non-virtual calls to Jit intrinsics are must-expand by convention.
        mustExpand = mustExpand || (gtIsRecursiveCall(method) && !(methodFlags & CORINFO_FLG_VIRTUAL));

        if (intrinsicId == CORINFO_INTRINSIC_Illegal)
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
                return impImportSysNumSimdIntrinsic(ni, clsHnd, method, sig, false);
            }
#endif // FEATURE_HW_INTRINSICS
        }
    }

    // Under debug and minopts, only expand what is required.
    // NextCallReturnAddress intrinsic returns the return address of the next call.
    // If that call is an intrinsic and is expanded, codegen for NextCallReturnAddress will fail.
    // To avoid that we conservatively expand only required intrinsics in methods that call
    // the NextCallReturnAddress intrinsic.
    if (!mustExpand && (opts.OptimizationDisabled() || info.compHasNextCallRetAddr))
    {
        *pIntrinsicId = CORINFO_INTRINSIC_Illegal;
        return nullptr;
    }

    *pIntrinsicId = intrinsicId;

    if (intrinsicId != CORINFO_INTRINSIC_Illegal)
    {
        assert(ni == NI_Illegal);
        static_assert_no_msg(CORINFO_INTRINSIC_Array_Get == 0);
        static_assert_no_msg(NI_CORINFO_INTRINSIC_END - NI_CORINFO_INTRINSIC_START - 1 == CORINFO_INTRINSIC_Count);

        ni = static_cast<NamedIntrinsic>(intrinsicId + NI_CORINFO_INTRINSIC_Array_Get);
    }

    var_types callType  = CorTypeToVarType(sig->retType);
    GenTree*  retNode   = nullptr;
    bool      isSpecial = false;
#ifndef TARGET_ARM
    genTreeOps interlockedOperator;
#endif

    switch (ni)
    {
        GenTree* op1;
        GenTree* op2;

#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
        // TODO-ARM-CQ: reenable treating Interlocked operation as intrinsic

        // Note that CORINFO_INTRINSIC_InterlockedAdd32/64 are not actually used.
        // Anyway, we can import them as XADD and leave it to lowering/codegen to perform
        // whatever optimizations may arise from the fact that result value is not used.
        case NI_CORINFO_INTRINSIC_InterlockedAdd32:
        case NI_CORINFO_INTRINSIC_InterlockedXAdd32:
#ifdef TARGET_64BIT
        case NI_CORINFO_INTRINSIC_InterlockedAdd64:
        case NI_CORINFO_INTRINSIC_InterlockedXAdd64:
#endif
            interlockedOperator = GT_XADD;
            goto InterlockedBinOpCommon;

        case NI_CORINFO_INTRINSIC_InterlockedXchg32:
#ifdef TARGET_64BIT
        case NI_CORINFO_INTRINSIC_InterlockedXchg64:
#endif
            interlockedOperator = GT_XCHG;
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

        case NI_CORINFO_INTRINSIC_InterlockedCmpXchg32:
#ifdef TARGET_64BIT
        case NI_CORINFO_INTRINSIC_InterlockedCmpXchg64:
#endif
        {
            assert(callType != TYP_STRUCT);
            assert(sig->numArgs == 3);
            GenTree* op2;
            GenTree* op3;

            op3 = impPopStack().val; // comparand
            op2 = impPopStack().val; // value
            op1 = impPopStack().val; // location address

            retNode = new (comp, GT_CMPXCHG) GenTreeCmpXchg(genActualType(callType), op1, op2, op3);
            break;
        }
#endif // defined(TARGET_XARCH) || defined(TARGET_ARM64)

        case NI_CORINFO_INTRINSIC_MemoryBarrier:
        case NI_CORINFO_INTRINSIC_MemoryBarrierLoad:

            assert(sig->numArgs == 0);

            op1 = new (comp, GT_MEMORYBARRIER) GenTree(GT_MEMORYBARRIER, TYP_VOID);
            op1->gtFlags |= GTF_GLOB_REF | GTF_ASG;

            // On XARCH `CORINFO_INTRINSIC_MemoryBarrierLoad` fences need not be emitted.
            // However, we still need to capture the effect on reordering.
            if (ni == NI_CORINFO_INTRINSIC_MemoryBarrierLoad)
            {
                op1->gtFlags |= GTF_MEMORYBARRIER_LOAD;
            }

            retNode = op1;
            break;

        case NI_CORINFO_INTRINSIC_InitializeArray:
            retNode = impInitializeArrayIntrinsic(sig);
            break;

        case NI_CORINFO_INTRINSIC_Array_Address:
        case NI_CORINFO_INTRINSIC_Array_Get:
        case NI_CORINFO_INTRINSIC_Array_Set:
            retNode = impArrayAccessIntrinsic(clsHnd, sig, resolvedToken->token, readonlyCall, ni);
            break;

        case NI_CORINFO_INTRINSIC_RTH_GetValueInternal:
            op1 = impStackTop(0).val;
            if (op1->IsHelperCall() && op1->AsCall()->IsTypeHandleToRuntimeTypeHandleHelperCall())
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

        case NI_CORINFO_INTRINSIC_Object_GetType:
        {
            JITDUMP("\n impIntrinsic: call to Object.GetType\n");
            op1 = impStackTop(0).val;

            // If we're calling GetType on a boxed value, just get the type directly.
            if (GenTreeBox* box = op1->IsBox())
            {
                JITDUMP("Attempting to optimize box(...).getType() to direct type construction\n");

                // Try and clean up the box. Obtain the handle we
                // were going to pass to the newobj.
                GenTree* boxTypeHandle =
                    comp->gtTryRemoveBoxUpstreamEffects(box, Compiler::BR_REMOVE_AND_NARROW_WANT_TYPE_HANDLE);

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
            if ((retNode == nullptr) && (constrainedResolvedToken != nullptr) &&
                (callInfo->thisTransform == CORINFO_BOX_THIS))
            {
                // Ensure this is one of the is simple box cases (in particular, rule out nullables).
                const CorInfoHelpFunc boxHelper = info.compCompHnd->getBoxHelper(constrainedResolvedToken->hClass);
                const bool            isSafeToOptimize = (boxHelper == CORINFO_HELP_BOX);

                if (isSafeToOptimize)
                {
                    JITDUMP("Optimizing constrained box-this obj.getType() to direct type construction\n");
                    impPopStack();
                    GenTree* typeHandleOp = impTokenToHandle(constrainedResolvedToken, /* mustRestoreHandle */ true);
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

            if (retNode != nullptr)
            {
                JITDUMPTREE(retNode, "Optimized field for call to GetType is\n");
            }

            // Else expand as an intrinsic, unless the call is constrained,
            // in which case we defer expansion to allow impImportCall do the
            // special constraint processing.
            if ((retNode == nullptr) && (constrainedResolvedToken == nullptr))
            {
                JITDUMP("Expanding as special intrinsic\n");
                impPopStack();
                op1 = new (comp, GT_INTRINSIC) GenTreeIntrinsic(TYP_REF, op1, ni, method);

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

        // Implement ByReference Ctor. This wraps the store of the ref into a byref-like field
        // in a value type. The canonical example of this is Span<T>. In effect this is just
        // a substitution. The parameter byref will be assigned into the newly allocated object.
        case NI_CORINFO_INTRINSIC_ByReference_Ctor:
        {
            assert(newobjThis->AsLclAddr()->GetLclOffs() == 0);

            // Remove call to constructor and directly assign the byref passed
            // to the call to the first slot of the ByReference struct.
            op1                         = impPopStack().val;
            CORINFO_FIELD_HANDLE fldHnd = info.compCompHnd->getFieldInClass(clsHnd, 0);
            GenTreeIndir*        store  = gtNewFieldIndStore(TYP_BYREF, gtNewFieldAddr(newobjThis, fldHnd, 0), op1);
            GenTree*             byReferenceStruct = gtCloneExpr(newobjThis);
            assert(byReferenceStruct != nullptr);
            byReferenceStruct->SetOper(GT_LCL_VAR);
            byReferenceStruct->SetType(TYP_STRUCT);
            // TODO-MIKE-Cleanup: This isn't needed, it's here only because previously we had
            // ADDR(LCL_VAR) and returned only the LCL_VAR node, without clearing GTF_DONT_CSE.
            byReferenceStruct->SetDoNotCSE();
            impPushOnStack(byReferenceStruct, typeInfo(TI_STRUCT, clsHnd));
            retNode = store;
            break;
        }
        // Implement ptr value getter for ByReference struct.
        case NI_CORINFO_INTRINSIC_ByReference_Value:
        {
            op1                         = impPopStack().val;
            CORINFO_FIELD_HANDLE fldHnd = info.compCompHnd->getFieldInClass(clsHnd, 0);
            GenTree*             field  = gtNewFieldIndir(TYP_BYREF, gtNewFieldAddr(op1, fldHnd, 0));
            retNode                     = field;
            break;
        }

        case NI_CORINFO_INTRINSIC_GetRawHandle:
        {
            noway_assert(IsTargetAbi(CORINFO_CORERT_ABI)); // Only CoreRT supports it.

            // TODO-MIKE-Review: Can't we just use the existing resolvedToken?
            CORINFO_RESOLVED_TOKEN rt;
            rt.tokenContext = impTokenLookupContextHandle;
            rt.tokenScope   = info.compScopeHnd;
            rt.token        = resolvedToken->token;
            rt.tokenType    = CORINFO_TOKENKIND_Method;

            CORINFO_GENERICHANDLE_RESULT embedInfo;
            info.compCompHnd->expandRawHandleIntrinsic(&rt, &embedInfo);

            GenTree* rawHandle =
                impLookupToTree(&rt, &embedInfo.lookup, TokenToHandleKind(rt.token), embedInfo.compileTimeHandle);
            if (rawHandle == nullptr)
            {
                return nullptr;
            }

            noway_assert(rawHandle->TypeIs(TYP_I_IMPL));

            LclVarDsc* rawHandleSlotLcl = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("rawHandle"));
            GenTree*   store            = comp->gtNewLclStore(rawHandleSlotLcl, TYP_I_IMPL, rawHandle);
            store->AddSideEffects(GTF_GLOB_REF);
            impSpillNoneAppendTree(store);
            GenTree* lclVarAddr = gtNewLclVarAddrNode(rawHandleSlotLcl, TYP_I_IMPL);

            retNode = gtNewIndir(CorTypeToVarType(sig->retType), lclVarAddr);

            break;
        }

        case NI_System_String_get_Chars:
        {
            GenTree* op2 = impPopStack().val;
            GenTree* op1 = impPopStack().val;
            retNode      = gtNewIndexIndir(TYP_USHORT, gtNewStringIndexAddr(op1, op2));
            break;
        }

        case NI_System_String_get_Length:
            op1 = impPopStack().val;

            if (opts.OptimizationEnabled())
            {
                GenTreeFlags flags = GTF_EXCEPT;

                if (GenTreeStrCon* constStr = op1->IsStrCon())
                {
                    if (GenTreeIntCon* constLength = gtNewStringLiteralLength(constStr))
                    {
                        retNode = constLength;
                        break;
                    }

                    flags = GTF_IND_NONFAULTING;
                }

                op1 = comp->gtNewArrLen(op1, OFFSETOF__CORINFO_String__stringLen, flags);
            }
            else
            {
                op2 = gtNewIconNode(OFFSETOF__CORINFO_String__stringLen, TYP_I_IMPL);
                op1 = gtNewOperNode(GT_ADD, TYP_BYREF, op1, op2);
                op1 = gtNewIndir(TYP_INT, op1);
                op1->gtFlags |= GTF_EXCEPT;
            }

            retNode = op1;
            break;

        case NI_System_Span_get_Item:
        case NI_System_ReadOnlySpan_get_Item:
        {
            // Have index, stack pointer-to Span<T> s on the stack. Expand to:
            //
            // For Span<T>
            //   Comma
            //     BoundsCheck(index, s->_length)
            //     s->_pointer + index * sizeof(T)
            //
            // For ReadOnlySpan<T> -- same expansion, as it now returns a readonly ref

            assert(sig->retType == CORINFO_TYPE_BYREF);
            assert(sig->sigInst.classInstCount == 1);
            assert(sig->numArgs == 1);

            CORINFO_CLASS_HANDLE spanElemHnd = sig->sigInst.classInst[0];
            const unsigned       elemSize    = info.compCompHnd->getClassSize(spanElemHnd);
            assert(elemSize > 0);

            const bool isReadOnly = (ni == NI_System_ReadOnlySpan_get_Item);

            JITDUMP("\nimpIntrinsic: Expanding %sSpan<T>.get_Item, T=%s, sizeof(T)=%u\n", isReadOnly ? "ReadOnly" : "",
                    info.compCompHnd->getClassName(spanElemHnd), elemSize);

            GenTree* index    = impPopStack().val;
            GenTree* spanAddr = impPopStack().val;

            JITDUMPTREE(index, "Span index:\n");
            JITDUMPTREE(spanAddr, "Span address:\n");
            assert(varTypeIsIntegral(index->GetType()));
            assert(spanAddr->TypeIs(TYP_BYREF));

            GenTree* indexUses[2];
            GenTree* spanAddrUses[2];

            impMakeMultiUse(index, 2, indexUses, CHECK_SPILL_ALL DEBUGARG("span index temp"));
            impMakeMultiUse(spanAddr, 2, spanAddrUses, CHECK_SPILL_ALL DEBUGARG("span addr temp"));

            // Bounds check
            CORINFO_FIELD_HANDLE lengthHnd    = info.compCompHnd->getFieldInClass(clsHnd, 1);
            const unsigned       lengthOffset = info.compCompHnd->getFieldOffset(lengthHnd);
            GenTree* length      = gtNewFieldIndir(TYP_INT, gtNewFieldAddr(spanAddrUses[0], lengthHnd, lengthOffset));
            GenTree* boundsCheck = gtNewBoundsChk(indexUses[0], length, ThrowHelperKind::IndexOutOfRange);
            GenTree* indexOffset;

            if (GenTreeIntCon* indexConst = index->IsIntCon())
            {
                indexOffset =
                    gtNewIconNode(static_cast<target_ssize_t>(elemSize) * indexConst->GetInt32Value(), TYP_I_IMPL);
            }
            else
            {
                GenTree* indexIntPtr = impImplicitIorI4Cast(indexUses[1], TYP_I_IMPL);
                GenTree* sizeofNode  = gtNewIconNode(elemSize, TYP_I_IMPL);
                indexOffset          = gtNewOperNode(GT_MUL, TYP_I_IMPL, indexIntPtr, sizeofNode);
            }

            CORINFO_FIELD_HANDLE ptrHnd    = info.compCompHnd->getFieldInClass(clsHnd, 0);
            const unsigned       ptrOffset = info.compCompHnd->getFieldOffset(ptrHnd);
            FieldSeqNode*        ptrField  = GetByReferenceValueField(ptrHnd);

            GenTree* pointer = gtNewFieldIndir(TYP_BYREF, gtNewFieldAddr(spanAddrUses[1], ptrField, ptrOffset));
            GenTree* result  = gtNewOperNode(GT_ADD, TYP_BYREF, pointer, indexOffset);

            retNode = gtNewCommaNode(boundsCheck, result);

            break;
        }

        case NI_System_Type_GetTypeFromHandle:
        {
            GenTree* op1 = impStackTop(0).val;
            if (op1->IsHelperCall() && op1->AsCall()->IsTypeHandleToRuntimeTypeHandleHelperCall())
            {
                assert(op1->AsCall()->gtCallArgs->GetNext() == nullptr);

                impPopStack();

                // Replace helper with a more specialized helper that returns RuntimeType
                CorInfoHelpFunc helper = Compiler::eeGetHelperNum(op1->AsCall()->GetMethodHandle());

                if (helper == CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE)
                {
                    helper = CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE;
                }
                else
                {
                    assert(helper == CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE_MAYBENULL);
                    helper = CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE_MAYBENULL;
                }

                retNode = gtNewHelperCallNode(helper, TYP_REF, op1->AsCall()->gtCallArgs);
            }
            break;
        }

        case NI_System_Type_op_Equality:
        case NI_System_Type_op_Inequality:
        {
            JITDUMP("Importing Type.op_*Equality intrinsic\n");
            GenTree* op1     = impStackTop(1).val;
            GenTree* op2     = impStackTop(0).val;
            GenTree* optTree = gtFoldTypeEqualityCall(ni == NI_System_Type_op_Equality, op1, op2);
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
                            gtNewIconNode((info.compCompHnd->isValueClass(hClass) &&
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

                    if (((call->gtFlags & GTF_CALL_M_SPECIAL_INTRINSIC) != 0) &&
                        (lookupNamedIntrinsic(call->gtCallMethHnd) == NI_System_Threading_Thread_get_CurrentThread))
                    {
                        // drop get_CurrentThread() call
                        impPopStack();
                        call->ReplaceWith(gtNewNothingNode(), comp);
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
#ifdef FEATURE_HW_INTRINSICS
        case NI_System_Math_FusedMultiplyAdd:
#endif
            retNode = impMathIntrinsic(method, sig, callType, ni, tailCall);
            break;

        case NI_System_Array_Clone:
        case NI_System_Collections_Generic_Comparer_get_Default:
        case NI_System_Collections_Generic_EqualityComparer_get_Default:
        case NI_System_Object_MemberwiseClone:
        case NI_System_Threading_Thread_get_CurrentThread:
        {
            // Flag for later handling.
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
                    retNode = gtNewOperNode(GT_BSWAP16, TYP_INT, impPopStack().val);
                    retNode = gtNewCastNode(retNode, false, callType);
                    break;

                case CorInfoType::CORINFO_TYPE_INT:
                case CorInfoType::CORINFO_TYPE_UINT:
#ifdef TARGET_64BIT
                case CorInfoType::CORINFO_TYPE_LONG:
                case CorInfoType::CORINFO_TYPE_ULONG:
#endif
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
            if (GenTreeIntConCommon* intCon = impStackTop().val->IsIntConCommon())
            {
                impPopStack();

                CORINFO_CLASS_HANDLE argClass;
                var_types argType = CorTypeToVarType(strip(info.compCompHnd->getArgType(sig, sig->args, &argClass)));
                unsigned  popCount;

                if (argType == TYP_LONG)
                {
                    popCount = genCountBits(intCon->GetValue());
                }
                else
                {
                    assert(argType == TYP_INT);
                    popCount = genCountBits(intCon->AsIntCon()->GetUInt32Value());
                }

                retNode = gtNewIconNode(popCount, callType);
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

GenTree* Importer::impTypeIsAssignable(GenTree* typeTo, GenTree* typeFrom)
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

GenTree* Importer::impMathIntrinsic(CORINFO_METHOD_HANDLE method,
                                    CORINFO_SIG_INFO*     sig,
                                    var_types             callType,
                                    NamedIntrinsic        intrinsicName,
                                    bool                  tailCall)
{
    assert(callType != TYP_STRUCT);
    assert(IsMathIntrinsic(intrinsicName));

#ifdef FEATURE_HW_INTRINSICS
    if (intrinsicName == NI_System_Math_FusedMultiplyAdd)
    {
        assert(sig->numArgs == 3);
        assert(varTypeIsFloating(callType));

#ifdef TARGET_XARCH
        if (compExactlyDependsOn(InstructionSet_FMA) && supportSIMDTypes())
        {
            // We are constructing a chain of intrinsics similar to:
            //    return FMA.MultiplyAddScalar(
            //        Vector128.CreateScalarUnsafe(x),
            //        Vector128.CreateScalarUnsafe(y),
            //        Vector128.CreateScalarUnsafe(z)
            //    ).ToScalar();

            GenTree* op3 =
                gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, callType, 16, impPopStack().val);
            GenTree* op2 =
                gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, callType, 16, impPopStack().val);
            GenTree* op1 =
                gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector128_CreateScalarUnsafe, callType, 16, impPopStack().val);
            op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_FMA_MultiplyAddScalar, callType, 16, op1, op2, op3);

            return gtNewSimdHWIntrinsicNode(callType, NI_Vector128_GetElement, callType, 16, op1, gtNewIconNode(0));
        }
#elif defined(TARGET_ARM64)
        if (compExactlyDependsOn(InstructionSet_AdvSimd))
        {
            // We are constructing a chain of intrinsics similar to:
            //    return AdvSimd.FusedMultiplyAddScalar(
            //        Vector64.Create{ScalarUnsafe}(z),
            //        Vector64.Create{ScalarUnsafe}(y),
            //        Vector64.Create{ScalarUnsafe}(x)
            //    ).ToScalar();

            NamedIntrinsic createVector64 =
                (callType == TYP_DOUBLE) ? NI_Vector64_Create : NI_Vector64_CreateScalarUnsafe;

            constexpr unsigned int simdSize = 8;

            GenTree* op3 = gtNewSimdHWIntrinsicNode(TYP_SIMD8, createVector64, callType, simdSize, impPopStack().val);
            GenTree* op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD8, createVector64, callType, simdSize, impPopStack().val);
            GenTree* op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD8, createVector64, callType, simdSize, impPopStack().val);

            // Note that AdvSimd.FusedMultiplyAddScalar(op1,op2,op3) corresponds to op1 + op2 * op3
            // while Math{F}.FusedMultiplyAddScalar(op1,op2,op3) corresponds to op1 * op2 + op3
            op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD8, NI_AdvSimd_FusedMultiplyAddScalar, callType, simdSize, op3, op2,
                                           op1);

            return gtNewSimdHWIntrinsicNode(callType, NI_Vector64_GetElement, callType, simdSize, op1,
                                            gtNewIconNode(0));
        }
#endif

        // TODO-CQ-XArch: Ideally we would create a GT_INTRINSIC node for fma, however, that currently
        // requires more extensive changes to valuenum to support methods with 3 operands

        // We want to generate a GT_INTRINSIC node in the case the call can't be treated as
        // a target intrinsic so that we can still benefit from CSE and constant folding.

        return nullptr;
    }
#endif // FEATURE_HW_INTRINSICS

    GenTree* op1 = nullptr;
    GenTree* op2 = nullptr;

#ifndef TARGET_X86
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
                    // TODO-MIKE-Review: This is messed up, it casts to the method's
                    // return type instead of casting to the parameter type. This
                    // would probably blow in some cases involving ILogB.
                    op1 = gtNewCastNode(op1, false, callType);
                }

                break;

            case 2:
                op2 = impPopStack().val;
                op1 = impPopStack().val;

                arg     = sig->args;
                op1Type = JITtype2varType(strip(info.compCompHnd->getArgType(sig, arg, &tmpClass)));

                if (op1->TypeGet() != genActualType(op1Type))
                {
                    assert(varTypeIsFloating(op1));
                    op1 = gtNewCastNode(op1, false, callType);
                }

                arg     = info.compCompHnd->getArgNext(arg);
                op2Type = JITtype2varType(strip(info.compCompHnd->getArgType(sig, arg, &tmpClass)));

                if (op2->TypeGet() != genActualType(op2Type))
                {
                    assert(varTypeIsFloating(op2));
                    op2 = gtNewCastNode(op2, false, callType);
                }

                break;

            default:
                NO_WAY("Unsupported number of args for Math Intrinsic");
        }

        op1 = new (comp, GT_INTRINSIC) GenTreeIntrinsic(varActualType(callType), op1, op2, intrinsicName, method);

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
            else if (strcmp(methodName, "op_Equality") == 0)
            {
                result = NI_System_Type_op_Equality;
            }
            else if (strcmp(methodName, "op_Inequality") == 0)
            {
                result = NI_System_Type_op_Inequality;
            }
            else if (strcmp(methodName, "GetTypeFromHandle") == 0)
            {
                result = NI_System_Type_GetTypeFromHandle;
            }
        }
        else if (strcmp(className, "String") == 0)
        {
            if (strcmp(methodName, "get_Chars") == 0)
            {
                result = NI_System_String_get_Chars;
            }
            else if (strcmp(methodName, "get_Length") == 0)
            {
                result = NI_System_String_get_Length;
            }
        }
        else if (strcmp(className, "Span`1") == 0)
        {
            if (strcmp(methodName, "get_Item") == 0)
            {
                result = NI_System_Span_get_Item;
            }
        }
        else if (strcmp(className, "ReadOnlySpan`1") == 0)
        {
            if (strcmp(methodName, "get_Item") == 0)
            {
                result = NI_System_ReadOnlySpan_get_Item;
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
        result = impFindSysNumSimdIntrinsic(method, className, methodName, enclosingClassName);
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
GenTree* Importer::impUnsupportedNamedIntrinsic(CorInfoHelpFunc       helper,
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

    if (!mustExpand)
    {
        return nullptr;
    }

    for (unsigned i = 0; i < sig->numArgs; i++)
    {
        GenTree* arg = impPopStack().val;
        // These are expected to be the intrinsic method's own parameters so
        // they should not have side effects and can simply be discarded.
        assert(arg->GetSideEffects() == 0);
    }

    GenTreeCall* call = gtNewHelperCallNode(helper, TYP_VOID);
    call->gtCallMoreFlags |= GTF_CALL_M_DOES_NOT_RETURN;
    impSpillAllAppendTree(call);

    var_types retType = JITtype2varType(sig->retType);

    if (retType == TYP_VOID)
    {
        return gtNewNothingNode();
    }

    if (retType != TYP_STRUCT)
    {
        return gtNewZeroConNode(retType);
    }

    ClassLayout* layout = typGetObjLayout(sig->retTypeClass);

#ifdef FEATURE_SIMD
    if (varTypeIsSIMD(typGetStructType(layout)))
    {
        return gtNewZeroSimdHWIntrinsicNode(layout);
    }
#endif

    LclVarDsc* tempLcl = lvaAllocTemp(true DEBUGARG("unsupported named intrinsic temp"));
    comp->lvaSetStruct(tempLcl, layout, false);
    return gtNewLclvNode(tempLcl, TYP_STRUCT);
}

GenTree* Importer::impArrayAccessIntrinsic(
    CORINFO_CLASS_HANDLE clsHnd, CORINFO_SIG_INFO* sig, int memberRef, bool readonlyCall, NamedIntrinsic name)
{
    // If we are generating SMALL_CODE, we don't want to use intrinsics for
    // the following, as it generates fatter code.
    if (compCodeOpt() == SMALL_CODE)
    {
        return nullptr;
    }

    // These intrinsics generate fatter (but faster) code and are only
    // done if we don't need SMALL_CODE

    unsigned rank = (name == NI_CORINFO_INTRINSIC_Array_Set) ? (sig->numArgs - 1) : sig->numArgs;

    // The rank 1 case is special because it has to handle two array formats
    // we will simply not do that case
    if ((rank > GenTreeArrElem::MaxRank) || (rank <= 1))
    {
        return nullptr;
    }

    CORINFO_CLASS_HANDLE elemClsHnd = nullptr;
    var_types            elemType   = JITtype2varType(info.compCompHnd->getChildType(clsHnd, &elemClsHnd));

    // For the ref case, we will only be able to inline if the types match
    // and the type is final (so we don't need to do the cast).
    if ((name != NI_CORINFO_INTRINSIC_Array_Get) && !readonlyCall && (elemType == TYP_REF))
    {
        CORINFO_SIG_INFO callSig;
        eeGetCallSiteSig(memberRef, info.compScopeHnd, impTokenLookupContextHandle, &callSig);
        assert(callSig.hasThis());

        CORINFO_CLASS_HANDLE accessClsHnd;

        if (name == NI_CORINFO_INTRINSIC_Array_Set)
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
            assert(name == NI_CORINFO_INTRINSIC_Array_Address);
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

    if (elemSize > GenTreeArrElem::MaxElemSize)
    {
        return nullptr;
    }

    GenTree* val = nullptr;

    if (name == NI_CORINFO_INTRINSIC_Array_Set)
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

    GenTree* inds[GenTreeArrElem::MaxRank];
    for (unsigned k = rank; k > 0; k--)
    {
        inds[k - 1] = impPopStack().val;
    }

    GenTree* arr = impPopStack().val;
    assert(arr->gtType == TYP_REF);

    GenTree* elemAddr = new (comp, GT_ARR_ELEM) GenTreeArrElem(TYP_BYREF, arr, rank, elemSize, elemType, inds);

    if (name == NI_CORINFO_INTRINSIC_Array_Address)
    {
        return elemAddr;
    }

    GenTreeIndir* elem;

    if (varTypeIsStruct(elemType))
    {
        elem = gtNewObjNode(typGetObjLayout(sig->retTypeClass), elemAddr);
    }
    else
    {
        elem = gtNewIndir(elemType, elemAddr);
    }

    if (name == NI_CORINFO_INTRINSIC_Array_Set)
    {
        // TODO-MIKE-Cleanup: It would be better to generate stores from the get go
        elem->SetOper(elem->OperIs(GT_OBJ) ? GT_STORE_OBJ : GT_STOREIND);
        elem->SetValue(val);
        elem->AddSideEffects(GTF_ASG | GTF_GLOB_REF | val->GetSideEffects());
    }

    return elem;
}

#ifdef DEBUG

bool Importer::verCheckTailCallConstraint(OPCODE                  opcode,
                                          CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                          CORINFO_RESOLVED_TOKEN* pConstrainedResolvedToken // Is this a "constrained."
                                                                                            // call on a type parameter?
                                          )
{
    assert(impOpcodeIsCallOpcode(opcode));
    assert(!compIsForInlining());

    DWORD            mflags;
    CORINFO_SIG_INFO sig;
    unsigned int     popCount = 0; // we can't pop the stack since impImportCall needs it, so
                                   // this counter is used to keep track of how many items have been
                                   // virtually popped

    CORINFO_METHOD_HANDLE methodHnd       = nullptr;
    CORINFO_CLASS_HANDLE  methodClassHnd  = nullptr;
    unsigned              methodClassFlgs = 0;

    // for calli, VerifyOrReturn that this is not a virtual method
    if (opcode == CEE_CALLI)
    {
        eeGetSig(pResolvedToken->token, pResolvedToken->tokenScope, pResolvedToken->tokenContext, &sig);

        // We don't know the target method, so we have to infer the flags, or assume the worst-case.
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
        CORINFO_CLASS_HANDLE paramClass;
        CorInfoType          argCorType = strip(info.compCompHnd->getArgType(&sig, args, &paramClass));

        if ((argCorType == CORINFO_TYPE_PTR) || (argCorType == CORINFO_TYPE_BYREF) ||
            ((argCorType == CORINFO_TYPE_VALUECLASS) &&
             ((info.compCompHnd->getClassAttribs(paramClass) & CORINFO_FLG_CONTAINS_STACK_PTR) != 0)))
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

GenTree* Importer::impImportLdvirtftn(GenTree*                thisPtr,
                                      CORINFO_RESOLVED_TOKEN* resolvedToken,
                                      CORINFO_CALL_INFO*      callInfo)
{
    if ((callInfo->methodFlags & CORINFO_FLG_EnC) && !(callInfo->classFlags & CORINFO_FLG_INTERFACE))
    {
        NO_WAY("Virtual call to a function added via EnC is not supported");
    }

    // CoreRT generic virtual method
    if ((callInfo->sig.sigInst.methInstCount != 0) && IsTargetAbi(CORINFO_CORERT_ABI))
    {
        GenTree* runtimeMethodHandle =
            impLookupToTree(resolvedToken, &callInfo->codePointerLookup, HandleKind::Method, callInfo->hMethod);
        return gtNewHelperCallNode(CORINFO_HELP_GVMLOOKUP_FOR_SLOT, TYP_I_IMPL,
                                   gtNewCallArgs(thisPtr, runtimeMethodHandle));
    }

#ifdef FEATURE_READYTORUN_COMPILER
    if (opts.IsReadyToRun())
    {
        if (!callInfo->exactContextNeedsRuntimeLookup)
        {
            GenTreeCall* call =
                gtNewHelperCallNode(CORINFO_HELP_READYTORUN_VIRTUAL_FUNC_PTR, TYP_I_IMPL, gtNewCallArgs(thisPtr));

            call->setEntryPoint(callInfo->codePointerLookup.constLookup);

            return call;
        }

        // We need a runtime lookup. CoreRT has a ReadyToRun helper for that too.
        if (IsTargetAbi(CORINFO_CORERT_ABI))
        {
            GenTree* ctxTree = gtNewRuntimeContextTree(callInfo->codePointerLookup.lookupKind.runtimeLookupKind);

            return gtNewReadyToRunHelperCallNode(resolvedToken, CORINFO_HELP_READYTORUN_GENERIC_HANDLE, TYP_I_IMPL,
                                                 gtNewCallArgs(ctxTree), &callInfo->codePointerLookup.lookupKind);
        }
    }
#endif

    // Get the exact descriptor for the static callsite
    GenTree* exactTypeDesc = impParentClassTokenToHandle(resolvedToken);
    if (exactTypeDesc == nullptr)
    { // compDonotInline()
        return nullptr;
    }

    GenTree* exactMethodDesc = impTokenToHandle(resolvedToken);
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

BoxPattern Compiler::impBoxPatternMatch(const BYTE* codeAddr, const BYTE* codeEnd, unsigned* patternSize)
{
    if (codeAddr >= codeEnd)
    {
        return BoxPattern::None;
    }

    switch (codeAddr[0])
    {
        case CEE_UNBOX_ANY:
            if (codeAddr + 1 + sizeof(mdToken) <= codeEnd)
            {
                *patternSize = 1 + sizeof(mdToken);
                return BoxPattern::BoxUnbox;
            }
            break;

        case CEE_BRTRUE:
        case CEE_BRTRUE_S:
        case CEE_BRFALSE:
        case CEE_BRFALSE_S:
            if ((codeAddr + ((codeAddr[0] >= CEE_BRFALSE) ? 5 : 2)) <= codeEnd)
            {
                *patternSize = 0;
                return BoxPattern::BoxBranch;
            }
            break;

        case CEE_ISINST:
            if (codeAddr + 1 + sizeof(mdToken) + 1 <= codeEnd)
            {
                const BYTE* nextCodeAddr = codeAddr + 1 + sizeof(mdToken);

                switch (nextCodeAddr[0])
                {
                    case CEE_BRTRUE:
                    case CEE_BRTRUE_S:
                    case CEE_BRFALSE:
                    case CEE_BRFALSE_S:
                        if ((nextCodeAddr + ((nextCodeAddr[0] >= CEE_BRFALSE) ? 5 : 2)) <= codeEnd)
                        {
                            *patternSize = 1 + sizeof(mdToken);
                            return BoxPattern::BoxCastBranch;
                        }
                        break;

                    case CEE_UNBOX_ANY:
                        if ((nextCodeAddr + 1 + sizeof(mdToken)) <= codeEnd)
                        {
                            *patternSize = 2 + sizeof(mdToken) * 2;
                            return BoxPattern::BoxCastUnbox;
                        }
                        break;
                }
            }
            break;

        default:
            break;
    }

    return BoxPattern::None;
}

bool Importer::impImportBoxPattern(BoxPattern              pattern,
                                   CORINFO_RESOLVED_TOKEN* resolvedToken,
                                   const BYTE* codeAddr DEBUGARG(const BYTE* codeEnd))
{
    assert(pattern != BoxPattern::None);
    assert(codeAddr < codeEnd);

    CORINFO_CLASS_HANDLE boxClass = resolvedToken->hClass;
    ICorJitInfo*         jitInfo  = info.compCompHnd;

    switch (pattern)
    {
        const BYTE*          nextCodeAddr;
        CorInfoHelpFunc      boxHelper;
        GenTree*             sideEffects;
        CORINFO_CLASS_HANDLE unboxClass;
        CORINFO_CLASS_HANDLE isinstClass;

        case BoxPattern::BoxUnbox:
            assert(codeAddr + 1 + sizeof(mdToken) <= codeEnd);

            unboxClass = impResolveClassToken(codeAddr + 1);

            if (jitInfo->compareTypesForEquality(unboxClass, boxClass) != TypeCompareState::Must)
            {
                return false;
            }

            JITDUMP("\n Importing BOX; UNBOX.ANY as NOP\n");

            return true;

        case BoxPattern::BoxBranch:
            assert((codeAddr + ((codeAddr[0] >= CEE_BRFALSE) ? 5 : 2)) <= codeEnd);

            if (jitInfo->getBoxHelper(boxClass) != CORINFO_HELP_BOX)
            {
                return false;
            }

            sideEffects = impImportPop(currentBlock);

            if (sideEffects != nullptr)
            {
                impSpillAllAppendTree(sideEffects);
            }

            JITDUMP("\n Importing BOX; BRTRUE/FALSE as constant branch\n");

            impPushOnStack(gtNewIconNode(1));

            return true;

        case BoxPattern::BoxCastBranch:
            assert(codeAddr + 1 + sizeof(mdToken) + 1 <= codeEnd);
            nextCodeAddr = codeAddr + 1 + sizeof(mdToken);
            assert((nextCodeAddr + ((nextCodeAddr[0] >= CEE_BRFALSE) ? 5 : 2)) <= codeEnd);

            if ((impStackTop().val->gtFlags & GTF_SIDE_EFFECT) != 0)
            {
                return false;
            }

            boxHelper = jitInfo->getBoxHelper(boxClass);

            if (boxHelper == CORINFO_HELP_BOX)
            {
                isinstClass = impResolveClassToken(codeAddr + 1, CORINFO_TOKENKIND_Casting);

                TypeCompareState castResult = jitInfo->compareTypesForCast(boxClass, isinstClass);

                if (castResult == TypeCompareState::May)
                {
                    return false;
                }

                JITDUMP("\n Importing BOX; ISINST; BRTRUE/FALSE as constant branch\n");

                impPopStack();
                impPushOnStack(gtNewIconNode((castResult == TypeCompareState::Must) ? 1 : 0));

                return true;
            }

            if (boxHelper == CORINFO_HELP_BOX_NULLABLE)
            {
                // For nullable we're going to fold it to "ldfld hasValue + brtrue/brfalse" or
                // "ldc.i4.0 + brtrue/brfalse" in case if the underlying type is not castable to
                // the target type.

                isinstClass = impResolveClassToken(codeAddr + 1, CORINFO_TOKENKIND_Casting);

                CORINFO_CLASS_HANDLE underlyingCls = info.compCompHnd->getTypeForBox(boxClass);

                TypeCompareState castResult = jitInfo->compareTypesForCast(underlyingCls, isinstClass);

                if (castResult == TypeCompareState::Must)
                {
                    const CORINFO_FIELD_HANDLE hasValueField = jitInfo->getFieldInClass(boxClass, 0);

                    assert(jitInfo->getFieldOffset(hasValueField) == 0);
                    assert(!strcmp(jitInfo->getFieldName(hasValueField, nullptr), "hasValue"));

                    GenTree* objToBox = impPopStack().val;

                    // Spill struct to get its address (to access hasValue field)
                    objToBox = impGetStructAddr(objToBox, boxClass, CHECK_SPILL_ALL, true);

                    impPushOnStack(gtNewFieldIndir(TYP_BOOL, gtNewFieldAddr(objToBox, hasValueField, 0)));

                    JITDUMP("\n Importing BOX; ISINST; BR_TRUE/FALSE as Nullable.hasValue\n");

                    return true;
                }

                if (castResult == TypeCompareState::MustNot)
                {
                    impPopStack();
                    impPushOnStack(gtNewIconNode(0));

                    JITDUMP("\n Importing BOX; ISINST; BR_TRUE/FALSE as constant (false)\n");

                    return true;
                }
            }

            return false;

        case BoxPattern::BoxCastUnbox:
            assert(codeAddr + 1 + sizeof(mdToken) + 1 <= codeEnd);
            nextCodeAddr = codeAddr + 1 + sizeof(mdToken);
            assert((nextCodeAddr + 1 + sizeof(mdToken)) <= codeEnd);

            isinstClass = impResolveClassToken(codeAddr + 1);

            if (jitInfo->compareTypesForEquality(isinstClass, boxClass) != TypeCompareState::Must)
            {
                return false;
            }

            unboxClass = impResolveClassToken(nextCodeAddr + 1);

            if (jitInfo->compareTypesForEquality(unboxClass, boxClass) != TypeCompareState::Must)
            {
                return false;
            }

            JITDUMP("\n Importing BOX; ISINST, UNBOX.ANY as NOP\n");

            return true;

        default:
            return false;
    }
}

// Build and import a value-type box.
//
// The value to be boxed is popped from the stack, and a tree for
// the boxed value is pushed. This method may create upstream
// statements, spill side effecting trees, and create new temps.
//
// If importing an inlinee, we may also discover the inline must
// fail. If so there is no new value pushed on the stack. Callers
// should use CompDoNotInline after calling this method to see if
// ongoing importation should be aborted.
//
// Boxing of ref classes results in the same value as the value on
// the top of the stack, so is handled inline in impImportBlockCode
// for the CEE_BOX case. Only value or primitive type boxes make it
// here.
//
// Boxing for nullable types is done via a helper call; boxing
// of other value types is expanded inline or handled via helper
// call, depending on the JIT's codegen mode.
//
// When the jit is operating in size and time constrained modes,
// using a helper call here can save jit time and code size. But it
// also may inhibit cleanup optimizations that could have also had a
// even greater benefit effect on code size and jit time. An optimal
// strategy may need to peek ahead and see if it is easy to tell how
// the box is being used. For now, we defer.

void Importer::impImportAndPushBox(CORINFO_RESOLVED_TOKEN* resolvedToken)
{
    StackEntry           se         = impPopStack();
    CORINFO_CLASS_HANDLE valueClass = se.seTypeInfo.GetClassHandle();
    GenTree*             value      = se.val;
    CorInfoHelpFunc      boxHelper  = info.compCompHnd->getBoxHelper(resolvedToken->hClass);

    // Determine what expansion to prefer.
    //
    // In size/time/debuggable constrained modes, the helper call expansion for box
    // is generally smaller and is preferred, unless the value to box is a struct
    // that comes from a call. In that case the call can construct its return value
    // directly into the box payload, saving possibly some up-front zeroing.
    //
    // Currently primitive type boxes always get inline expanded. We may want to do
    // the same for small structs if they don't come from calls and don't have GC
    // pointers, since explicitly copying such structs is cheap.

    bool canExpandInline = boxHelper == CORINFO_HELP_BOX;
    bool optForSize      = !value->IsCall() && (valueClass != nullptr) && opts.OptimizationDisabled();
    bool expandInline    = canExpandInline && !optForSize;

    JITDUMP("\nCompiler::impImportAndPushBox -- handling BOX(value class) via");

    GenTree* boxed;

    if (expandInline)
    {
        JITDUMP(" inline allocate/copy sequence\n");

        // We are doing 'normal' boxing. This means that we can inline the box operation
        // by allocating an object on the heap and storing the value in it.
        //
        // For minopts/debug code, try and minimize the total number of box temps by
        // reusing an existing temp when possible.
        // In minopts we don't inline so there's no point in sharing the temp between
        // inliner and inlinees.
        //
        // When optimizing, use a new temp for each box operation
        // since we then know the exact class of the box temp.

        if (opts.OptimizationDisabled())
        {
            if (impBoxTempInUse || (impBoxTempLcl == nullptr))
            {
                impBoxTempLcl = lvaNewTemp(TYP_REF, true DEBUGARG("Reusable Box Helper"));
            }
        }
        else
        {
            LclVarDsc* boxTempLcl   = lvaNewTemp(TYP_REF, true DEBUGARG("Single-def Box Helper"));
            boxTempLcl->lvSingleDef = true;
            JITDUMP("Marking " FMT_LCL " as a single def local\n", boxTempLcl->GetLclNum());
            comp->lvaSetClass(boxTempLcl, resolvedToken->hClass, /* isExact */ true);
            impBoxTempLcl = boxTempLcl;
        }

        // Needs to stay in use until this box expression is appended some other node.
        // We approximate this by keeping it alive until the opcode stack becomes empty.
        impBoxTempInUse = true;

        GenTree* alloc = gtNewAllocObjNode(resolvedToken, /* useParent */ false);

        if (alloc == nullptr)
        {
            assert(compDonotInline());
            return;
        }

        currentBlock->bbFlags |= BBF_HAS_NEWOBJ;
        comp->optMethodFlags |= OMF_HAS_NEWOBJ;

        GenTree*   allocStore = comp->gtNewLclStore(impBoxTempLcl, TYP_REF, alloc);
        Statement* allocStmt  = impSpillNoneAppendTree(allocStore);

        GenTree* addr = comp->gtNewLclLoad(impBoxTempLcl, TYP_REF);
        addr          = gtNewOperNode(GT_ADD, TYP_BYREF, addr, gtNewIconNode(TARGET_POINTER_SIZE, TYP_I_IMPL));

        GenTree* store;

        if (varTypeIsStruct(value->GetType()))
        {
            // Workaround for GitHub issue 53549.
            //
            // If the struct being boxed is returned via hidden buffer and comes from an inline/GDV candidate,
            // the IR we produce after importation is out of order:
            //
            //    call (&(box-temp + 8), ....)
            //    box-temp = newobj
            //    ret-val from call (void)
            //        ... box-temp (on stack)
            //
            // For inline candidates this bad ordering gets fixed up during inlining, but for GDV candidates
            // the GDV expansion is such that the newobj follows the call as in the above.
            //
            // This is nontrivial to fix in GDV, so in these (rare) cases we simply disable GDV.

            if (GenTreeRetExpr* retExpr = value->IsRetExpr())
            {
                GenTreeCall* call = retExpr->GetCall();

                if (call->IsGuardedDevirtualizationCandidate() && call->HasRetBufArg())
                {
                    JITDUMP("Disabling GDV for [%06u] because of in-box struct return\n", call->GetID());

                    call->ClearGuardedDevirtualizationCandidate();

                    if (call->IsVirtualStub())
                    {
                        JITDUMP("Restoring stub addr %p from guarded devirt candidate info\n",
                                dspPtr(call->gtGuardedDevirtualizationCandidateInfo->stubAddr));

                        call->gtStubCallStubAddr = call->gtGuardedDevirtualizationCandidateInfo->stubAddr;
                    }
                }
            }

            store = gtNewObjNode(typGetObjLayout(valueClass), addr);
            store = impAssignStruct(store, value, CHECK_SPILL_ALL);
        }
        else
        {
            var_types type = value->GetType();

            if (type == TYP_BYREF)
            {
                type = TYP_I_IMPL;
            }

            CorInfoType jitType = info.compCompHnd->asCorInfoType(resolvedToken->hClass);

            if (impIsPrimitive(jitType))
            {
                type = JITtype2varType(jitType);
            }

            assert((varActualType(value->GetType()) == varActualType(type)) ||
                   (varTypeIsFloating(type) == varTypeIsFloating(value->GetType())));

            var_types srcTyp = value->GetType();
            var_types dstTyp = type;

            if (srcTyp != dstTyp)
            {
                assert((varTypeIsFloating(srcTyp) && varTypeIsFloating(dstTyp)) ||
                       (varTypeIsIntegral(srcTyp) && varTypeIsIntegral(dstTyp)));

                value = gtNewCastNode(value, false, dstTyp);
            }

            store = comp->gtNewIndStore(type, addr, value);
            // TODO-MIKE-Review: Does this need a GLOB_REF? On one hand boxes are immutable,
            // so after this store the value cannot change again. But what would prevent a
            // load of the boxed value to move before this store? Magic?
        }

        impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("impImportAndPushBox"));

        Statement* storeStmt = impSpillNoneAppendTree(store);

        boxed = gtNewLclvNode(impBoxTempLcl, TYP_REF);
        // Record that this is a "box" node and keep track of the matching parts.
        // We can use this information to optimize several cases:
        //    "box(x) == null" --> false
        //    "(box(x)).CallAnInterfaceMethod(...)" --> "(&x).CallAValueTypeMethod"
        //    "(box(x)).CallAnObjectMethod(...)" --> "(&x).CallAValueTypeMethod"
        boxed = new (comp, GT_BOX) GenTreeBox(boxed, allocStmt, storeStmt);
    }
    else
    {
        JITDUMP(" helper call because: %s\n", canExpandInline ? "optimizing for size" : "nullable");
        assert(valueClass != nullptr);

        GenTree* handle = impTokenToHandle(resolvedToken, /* mustRestoreHandle */ true);

        if (handle == nullptr)
        {
            assert(compDonotInline());
            return;
        }

        GenTree* addr = impGetStructAddr(value, valueClass, CHECK_SPILL_ALL, true);

        boxed = gtNewHelperCallNode(boxHelper, TYP_REF, gtNewCallArgs(handle, addr));
    }

    impPushOnStack(boxed, typeInfo(TI_REF, info.compCompHnd->getTypeForBox(resolvedToken->hClass)));
}

//------------------------------------------------------------------------
// impImportNewObjArray: Build and import `new` of multi-dimmensional array
//
// Arguments:
//    resolvedToken - The CORINFO_RESOLVED_TOKEN that has been initialized
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

void Importer::impImportNewObjArray(CORINFO_RESOLVED_TOKEN* pResolvedToken, CORINFO_CALL_INFO* pCallInfo)
{
    assert(pCallInfo->sig.numArgs != 0);

    GenTree* classHandle = impParentClassTokenToHandle(pResolvedToken);
    if (classHandle == nullptr)
    {
        assert(!compDonotInline());
        return;
    }

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

        // TODO-MIKE-Cleanup: When inlining this should use the inliner compiler
        // to share the temp between the inliner and all inlinees.

        if (comp->lvaNewObjArrayArgs == BAD_VAR_NUM)
        {
            argsLcl = lvaAllocTemp(false DEBUGARG("NewObjArrayArgs"));
            argsLcl->SetBlockType(0);
            comp->lvaSetAddressExposed(argsLcl);

            comp->lvaNewObjArrayArgs = argsLcl->GetLclNum();
        }
        else
        {
            argsLcl = comp->lvaGetDesc(comp->lvaNewObjArrayArgs);
        }

        // Increase size of lvaNewObjArrayArgs to be the largest size needed to hold 'numArgs' integers
        // for our call to CORINFO_HELP_NEW_MDARR_NONVARARG.
        argsLcl->SetBlockType(max(argsLcl->GetBlockSize(), pCallInfo->sig.numArgs * sizeof(int32_t)));

        // The side-effects may include allocation of more multi-dimensional arrays. Spill all side-effects
        // to ensure that the shared lvaNewObjArrayArgs local variable is only ever used to pass arguments
        // to one allocation at a time.
        impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("impImportNewObjArray"));

        //
        // The arguments of the CORINFO_HELP_NEW_MDARR_NONVARARG helper are:
        //  - Array class handle
        //  - Number of dimension arguments
        //  - Pointer to block of int32 dimensions - address  of lvaNewObjArrayArgs temp.
        //

        node = gtNewLclVarAddrNode(argsLcl, TYP_I_IMPL);

        // Pop dimension arguments from the stack one at a time and store it
        // into lvaNewObjArrayArgs temp.
        for (int i = pCallInfo->sig.numArgs - 1; i >= 0; i--)
        {
            GenTree* arg   = impImplicitIorI4Cast(impPopStack().val, TYP_INT);
            GenTree* store = comp->gtNewLclStoreFld(TYP_INT, argsLcl, 4 * i, arg);
            store->AddSideEffects(GTF_GLOB_REF);
            node = gtNewCommaNode(store, node);
        }

        GenTreeCall::Use* args = gtNewCallArgs(node);

        args = gtPrependNewCallArg(gtNewIconNode(pCallInfo->sig.numArgs), args);
        args = gtPrependNewCallArg(classHandle, args);
        node = gtNewHelperCallNode(CORINFO_HELP_NEW_MDARR_NONVARARG, TYP_REF, args);
    }
    else
    {
        GenTreeCall::Use* args    = nullptr;
        GenTreeCall::Use* lastArg = nullptr;

        for (int i = pCallInfo->sig.numArgs - 1; i >= 0; i--)
        {
            GenTree* dim = impImplicitIorI4Cast(impPopStack().val, TYP_INT);
            args         = gtPrependNewCallArg(dim, args);

            if (lastArg == nullptr)
            {
                lastArg = args;
            }
        }

        GenTreeIntCon* numArgsNode = gtNewIconNode(pCallInfo->sig.numArgs);

#ifdef TARGET_X86
        lastArg = gtInsertNewCallArgAfter(numArgsNode, lastArg);
        lastArg = gtInsertNewCallArgAfter(classHandle, lastArg);
#else
        args = gtPrependNewCallArg(numArgsNode, args);
        args = gtPrependNewCallArg(classHandle, args);
#endif

        node = gtNewHelperCallNode(CORINFO_HELP_NEW_MDARR, TYP_REF, args);

#ifdef TARGET_X86
        node->gtFlags |= GTF_CALL_POP_ARGS;
#endif
    }

    for (GenTreeCall::Use& use : node->AsCall()->Args())
    {
        node->gtFlags |= use.GetNode()->gtFlags & GTF_GLOB_EFFECT;
    }

    node->AsCall()->compileTimeHelperArgumentHandle = (CORINFO_GENERIC_HANDLE)pResolvedToken->hClass;

    // Remember that this basic block contains 'new' of a md array
    currentBlock->bbFlags |= BBF_HAS_NEWARRAY;

    impPushOnStack(node, typeInfo(TI_REF, pResolvedToken->hClass));
}

GenTree* Importer::impTransformThis(GenTree*                thisPtr,
                                    CORINFO_RESOLVED_TOKEN* constrainedResolvedToken,
                                    CORINFO_THIS_TRANSFORM  transform)
{
    if (transform == CORINFO_DEREF_THIS)
    {
        var_types type = CorTypeToVarType(info.compCompHnd->asCorInfoType(constrainedResolvedToken->hClass));
        impBashVarAddrsToI(thisPtr);
        GenTree* load = gtNewIndir(type, thisPtr);
        load->AddSideEffects(GTF_EXCEPT | GTF_GLOB_REF);

        return load;
    }

    if (transform == CORINFO_BOX_THIS)
    {
        // Constraint calls where there might be no unboxed entry point require us to
        // implement the call via helper. These only occur when a possible target of
        // the call may have inherited an implementation of an interface method from
        // System.Object or System.ValueType. The EE does not provide us with unboxed
        // versions of these methods.

        var_types type = CorTypeToVarType(info.compCompHnd->asCorInfoType(constrainedResolvedToken->hClass));
        GenTree*  indir;

        if (type == TYP_STRUCT)
        {
            indir = gtNewObjNode(typGetObjLayout(constrainedResolvedToken->hClass), thisPtr);
        }
        else
        {
            indir = gtNewIndir(type, thisPtr);
        }

        indir->gtFlags |= GTF_EXCEPT;

        if ((type == TYP_STRUCT) ||
            (info.compCompHnd->getTypeForPrimitiveValueClass(constrainedResolvedToken->hClass) == CORINFO_TYPE_UNDEF))
        {
            impPushOnStack(indir, typeInfo(TI_STRUCT, constrainedResolvedToken->hClass));
        }
        else
        {
            impPushOnStack(indir);
        }

        // This pops off the byref-to-a-value-type remaining on the stack and
        // replaces it with a boxed object.
        // This is then used as the object to the virtual call immediately below.
        impImportAndPushBox(constrainedResolvedToken);
        if (compDonotInline())
        {
            return nullptr;
        }

        return impPopStack().val;
    }

    assert(transform == CORINFO_NO_THIS_TRANSFORM);
    return thisPtr;
}

//------------------------------------------------------------------------
// impCanPInvokeInline: check whether PInvoke inlining should enabled in current method.
//
// Return Value:
//    true if PInvoke inlining should be enabled in current method, false otherwise
//
// Notes:
//    Checks a number of ambient conditions where we could pinvoke but choose not to

bool Importer::impCanPInvokeInline()
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

bool Importer::impCanPInvokeInlineCallSite(BasicBlock* block)
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

void Importer::impCheckForPInvokeCall(
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

    // If we can't get the unmanaged calling convention or the calling convention is unsupported in the JIT,
    // return here without inlining the native call.
    if (unmanagedCallConv == CorInfoCallConvExtension::Managed ||
        unmanagedCallConv == CorInfoCallConvExtension::Fastcall ||
        unmanagedCallConv == CorInfoCallConvExtension::FastcallMemberFunction)
    {
        return;
    }
    comp->optNativeCallCount++;

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

#ifdef TARGET_X86
    if (unmanagedCallConv == CorInfoCallConvExtension::C ||
        unmanagedCallConv == CorInfoCallConvExtension::CMemberFunction)
    {
        call->gtFlags |= GTF_CALL_POP_ARGS;
    }
#endif

    if (unmanagedCallConv == CorInfoCallConvExtension::Thiscall)
    {
        if (sig->numArgs == 0)
        {
            BADCODE("Instance method without 'this' param");
        }

        call->gtCallMoreFlags |= GTF_CALL_M_UNMGD_THISCALL;
    }
}

GenTreeCall* Importer::impImportIndirectCall(CORINFO_SIG_INFO* sig, IL_OFFSETX ilOffset)
{
    // The function pointer is on top of the stack - it may be a
    // complex expression. As it is evaluated after the args,
    // it may cause registered args to be spilled. Simply spill it.

    // Ignore this trivial case.
    if (!impStackTop().val->OperIs(GT_LCL_VAR))
    {
        impSpillStackEntry(verCurrentState.esStackDepth - 1 DEBUGARG("impImportIndirectCall"));
    }

    GenTree* addr = impPopStack().val;

    // The function pointer should have type TYP_I_IMPL. However, stubgen IL
    // optimization can change LDC.I8 to LDC.I4, see ILCodeStream::LowerOpcode.
    // TODO-MIKE-Review: If this really happens we should change the constant
    // type to TYP_I_IMPL here. But then the above code spills anything other
    // than LCL_VAR, which would be stupid if the addr is ever a constant.
    assert(addr->TypeIs(TYP_I_IMPL, TYP_INT));

    return gtNewIndCallNode(addr, CorTypeToVarType(sig->retType), nullptr, ilOffset);
}

void Importer::PopUnmanagedCallArgs(GenTreeCall* call, CORINFO_SIG_INFO* sig)
{
    assert(call->IsUnmanaged());

#ifdef TARGET_X86
    // Since we push the arguments in reverse order (i.e. right -> left)
    // spill any side effects from the stack.
    // If there is only one side effect we do not need to spill it thus
    // we have to spill all side-effects except last one.

    unsigned argsToReverse = sig->numArgs;

    // For "thiscall", the first argument goes in a register. Since its
    // order does not need to be changed, we do not need to spill it

    if ((call->gtCallMoreFlags & GTF_CALL_M_UNMGD_THISCALL) != 0)
    {
        assert(argsToReverse >= 1);
        argsToReverse--;
    }

    unsigned lastLevelWithSideEffects = UINT_MAX;

    for (unsigned level = verCurrentState.esStackDepth - argsToReverse; level < verCurrentState.esStackDepth; level++)
    {
        if ((verCurrentState.esStack[level].val->gtFlags & GTF_ORDER_SIDEEFF) != 0)
        {
            assert(lastLevelWithSideEffects == UINT_MAX);

            impSpillStackEntry(level DEBUGARG("PopUnmanagedCallArgs - other side effect"));
        }
        else if ((verCurrentState.esStack[level].val->gtFlags & GTF_SIDE_EFFECT) != 0)
        {
            if (lastLevelWithSideEffects != UINT_MAX)
            {
                // We had a previous side effect - must spill it.
                impSpillStackEntry(lastLevelWithSideEffects DEBUGARG("PopUnmanagedCallArgs - side effect"));

                // Record the level for the current side effect in case we will spill it.
                lastLevelWithSideEffects = level;
            }
            else
            {
                // This is the first side effect encountered - record its level.
                lastLevelWithSideEffects = level;
            }
        }
    }
#endif // TARGET_X86

    GenTreeCall::Use* args = PopCallArgs(sig);

#ifdef TARGET_X86
    args = ReverseCallArgs(args, (call->gtCallMoreFlags & GTF_CALL_M_UNMGD_THISCALL) != 0);
#endif

    call->gtCallArgs = args;

    if ((call->gtCallMoreFlags & GTF_CALL_M_UNMGD_THISCALL) != 0)
    {
        GenTree* thisArg = args->GetNode();
        assert(thisArg->TypeIs(TYP_I_IMPL, TYP_BYREF));
        impBashVarAddrsToI(thisArg);
    }

    for (GenTreeCall::Use& argUse : call->Args())
    {
        GenTree* arg = argUse.GetNode();
        call->gtFlags |= arg->gtFlags & GTF_GLOB_EFFECT;

        // We should not pass GC typed args to an unmanaged call.
        // We tolerate BYREF args by retyping to native int.
        // Otherwise we'll generate inconsistent GC info for this arg at
        // the call site (GC info says byref, PInvoke sig says native int).
        if (arg->TypeIs(TYP_BYREF))
        {
            arg->ChangeType(TYP_I_IMPL);
        }
        else
        {
            assert(!varTypeIsGC(arg->GetType()));
        }
    }
}

GenTree* Importer::CreateClassInitTree(CORINFO_RESOLVED_TOKEN* resolvedToken)
{
    bool     runtimeLookup;
    GenTree* node = impParentClassTokenToHandle(resolvedToken, /* mustRestoreHandle */ false, &runtimeLookup);

    if (node == nullptr)
    {
        assert(compDonotInline());
        return nullptr;
    }

    if (runtimeLookup)
    {
        return gtNewHelperCallNode(CORINFO_HELP_INITCLASS, TYP_VOID, gtNewCallArgs(node));
    }

    // Call the shared non gc static helper, as its the fastest
    return gtNewSharedCctorHelperCall(resolvedToken->hClass);
}

GenTree* Importer::impImportStaticReadOnlyField(void* addr, var_types type)
{
#ifdef DEBUG
    // If we're replaying under SuperPMI, we're going to read the data stored by SuperPMI and use it
    // for optimization. Unfortunately, SuperPMI doesn't implement a guarantee on the alignment of
    // this data, so for some platforms which don't allow unaligned access (e.g., Linux arm32),
    // this can fault. We should fix SuperPMI to guarantee alignment, but that is a big change.
    // Instead, simply fix up the data here for future use.

    // This variable should be the largest size element, with the largest alignment requirement,
    // and the native C++ compiler should guarantee sufficient alignment.
    alignas(8) char alignedBuffer[8];

    if ((info.compMethodSuperPMIIndex != -1) && (varTypeSize(type) > 1))
    {
        assert(varTypeSize(type) <= sizeof(alignedBuffer));
        memcpy(alignedBuffer, addr, varTypeSize(type));
        addr = alignedBuffer;
    }
#endif // DEBUG

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

GenTreeFieldAddr* Importer::impImportFieldAddr(GenTree*                      addr,
                                               const CORINFO_RESOLVED_TOKEN& resolvedToken,
                                               const CORINFO_FIELD_INFO&     fieldInfo)
{
    GenTreeFieldAddr* field = addr->IsFieldAddr();

    if ((field != nullptr)
#ifdef FEATURE_READYTORUN_COMPILER
        && (fieldInfo.fieldAccessor != CORINFO_FIELD_INSTANCE_WITH_BASE)
#endif
        && (field->GetOffset() + fieldInfo.offset >= field->GetOffset()))
    {
        unsigned      offset   = field->GetOffset() + fieldInfo.offset;
        FieldSeqNode* fieldSeq = GetFieldSeqStore()->Append(field->GetFieldSeq(), resolvedToken.hField);

        field->SetOffset(offset, fieldSeq);
    }
    else
    {
        field = gtNewFieldAddr(addr, resolvedToken.hField, fieldInfo.offset);

#ifdef FEATURE_READYTORUN_COMPILER
        if (fieldInfo.fieldAccessor == CORINFO_FIELD_INSTANCE_WITH_BASE)
        {
            noway_assert(fieldInfo.fieldLookup.accessType == IAT_PVALUE);
            field->SetR2RFieldLookupAddr(fieldInfo.fieldLookup.addr);
        }
#endif

        if (fgAddrCouldBeNull(addr))
        {
            field->gtFlags |= GTF_EXCEPT;
        }
    }

    if (CorTypeToVarType(fieldInfo.fieldType) == TYP_STRUCT)
    {
        field->SetLayoutNum(typGetObjLayoutNum(fieldInfo.structType));
    }
    else
    {
        field->SetLayoutNum(0);
    }

    if ((info.compCompHnd->getClassAttribs(resolvedToken.hClass) & CORINFO_FLG_OVERLAPPING_FIELDS) != 0)
    {
        field->SetMayOverlap();
    }

    return field;
}

GenTree* Importer::impImportFieldInstanceAddrHelper(OPCODE                    opcode,
                                                    GenTree*                  objPtr,
                                                    CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                                    const CORINFO_FIELD_INFO& fieldInfo,
                                                    var_types                 type,
                                                    CORINFO_CLASS_HANDLE      structType)
{
    assert((opcode == CEE_LDFLD) || (opcode == CEE_STFLD) || (opcode == CEE_LDFLDA));
    assert((fieldInfo.fieldAccessor == CORINFO_FIELD_INSTANCE_ADDR_HELPER) &&
           (fieldInfo.helper == CORINFO_HELP_GETFIELDADDR));
    assert(objPtr != nullptr);

    GenTree* fieldHnd = impTokenToHandle(resolvedToken);
    GenTree* addr     = gtNewHelperCallNode(fieldInfo.helper, TYP_BYREF, gtNewCallArgs(objPtr, fieldHnd));

    if (opcode == CEE_LDFLDA)
    {
        return addr;
    }

    GenTree* indir;

    if (varTypeIsStruct(type))
    {
        indir = gtNewObjNode(typGetObjLayout(structType), addr);
    }
    else
    {
        indir = gtNewIndir(type, addr);
    }

    // The helper checks for null so the indir cannot fault.
    indir->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;

    return indir;
}

GenTree* Importer::impImportStaticFieldAddressHelper(OPCODE                    opcode,
                                                     CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                                     const CORINFO_FIELD_INFO& fieldInfo)
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
                addr = gtNewSharedStaticsCctorHelperCall(resolvedToken->hClass, fieldInfo.helper);
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

            GenTree* ctxTree = gtNewRuntimeContextTree(kind.runtimeLookupKind);
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

    // For static struct fields we may get either the address of a reference to a boxed struct
    // or a byref to the struct value itself, the helper does the unboxing. Add a boxed field
    // to the field sequence in both case to keep things consistent.

    if (((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC_IN_HEAP) == 0) &&
        (CorTypeToVarType(fieldInfo.fieldType) == TYP_STRUCT))
    {
        fieldSeq = GetFieldSeqStore()->Append(fieldSeq, GetFieldSeqStore()->GetBoxedValuePseudoField());
    }

    addr = new (comp, GT_FIELD_ADDR) GenTreeFieldAddr(addr, fieldSeq, fieldInfo.offset);

    if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC_IN_HEAP) != 0)
    {
        addr = gtNewIndir(TYP_REF, addr);
        addr->gtFlags |= GTF_IND_NONFAULTING;
        fieldSeq = GetFieldSeqStore()->GetBoxedValuePseudoField();
        addr     = new (comp, GT_FIELD_ADDR) GenTreeFieldAddr(addr, fieldSeq, TARGET_POINTER_SIZE);
    }

    return addr;
}

static bool IsStaticFieldHelperAccess(const CORINFO_FIELD_INFO& fieldInfo)
{
    return (fieldInfo.fieldAccessor == CORINFO_FIELD_STATIC_GENERICS_STATIC_HELPER) ||
           (fieldInfo.fieldAccessor == CORINFO_FIELD_STATIC_SHARED_STATIC_HELPER) ||
           (fieldInfo.fieldAccessor == CORINFO_FIELD_STATIC_READYTORUN_HELPER);
}

GenTree* Importer::impImportLdSFld(OPCODE                    opcode,
                                   CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                   const CORINFO_FIELD_INFO& fieldInfo,
                                   unsigned                  prefixFlags)
{
    var_types fieldType = CorTypeToVarType(fieldInfo.fieldType);

    switch (fieldInfo.fieldAccessor)
    {
        case CORINFO_FIELD_INTRINSIC_ZERO:
            assert(opcode == CEE_LDSFLD);
            return gtNewIconNode(0, varActualType(fieldType));

        case CORINFO_FIELD_INTRINSIC_EMPTY_STRING:
            assert(opcode == CEE_LDSFLD);
            {
                void*          pValue;
                InfoAccessType iat = info.compCompHnd->emptyStringLiteral(&pValue);
                return gtNewStringLiteralNode(iat, pValue);
            }

        case CORINFO_FIELD_INTRINSIC_ISLITTLEENDIAN:
            assert(opcode == CEE_LDSFLD);
#if BIGENDIAN
            return gtNewIconNode(0, varActualType(lclTyp));
#else
            return gtNewIconNode(1, varActualType(fieldType));
#endif
        default:
            break;
    }

    if (compIsForInlining())
    {
        switch (fieldInfo.fieldAccessor)
        {
            case CORINFO_FIELD_STATIC_ADDR_HELPER:
            case CORINFO_FIELD_STATIC_TLS:
                compInlineResult->NoteFatal(InlineObservation::CALLEE_LDFLD_NEEDS_HELPER);
                return nullptr;
            case CORINFO_FIELD_STATIC_GENERICS_STATIC_HELPER:
            case CORINFO_FIELD_STATIC_READYTORUN_HELPER:
                // We may be able to inline the field accessors in specific instantiations of generic methods.
                compInlineResult->NoteFatal(InlineObservation::CALLSITE_LDFLD_NEEDS_HELPER);
                return nullptr;
            default:
                break;
        }

        if ((opcode == CEE_LDSFLD) && (fieldType == TYP_STRUCT) && (fieldInfo.structType != NO_CLASS_HANDLE))
        {
            if ((info.compCompHnd->getTypeForPrimitiveValueClass(fieldInfo.structType) == CORINFO_TYPE_UNDEF) &&
                ((info.compFlags & CORINFO_FLG_FORCEINLINE) == 0))
            {
                // TODO-MIKE-Review: This is suspect. Struct static fields don't always need
                // a helper call and non-struct static fields may need helper calls too.

                // Loading a static valuetype field usually will cause a JitHelper to be called
                // for the static base. This will bloat the code.
                compInlineResult->Note(InlineObservation::CALLEE_LDFLD_STATIC_VALUECLASS);

                if (compInlineResult->IsFailure())
                {
                    return nullptr;
                }
            }
        }
    }

    impHandleAccessAllowed(fieldInfo.accessAllowed, fieldInfo.accessCalloutHelper);

    GenTree* field = nullptr;

    if (fieldInfo.fieldAccessor == CORINFO_FIELD_STATIC_TLS)
    {
        field = CreateStaticFieldTlsAccess(opcode, resolvedToken, fieldInfo);
    }
    else if (IsStaticFieldHelperAccess(fieldInfo))
    {
        field = CreateStaticFieldHelperAccess(opcode, resolvedToken, fieldInfo);
    }
    else
    {
        field = CreateStaticFieldAddressAccess(opcode, resolvedToken, fieldInfo);

        if ((opcode == CEE_LDSFLD) && field->OperIsConst())
        {
            return field;
        }
    }

    if (opcode == CEE_LDSFLD)
    {
        assert(fieldType == TYP_STRUCT ? field->OperIs(GT_OBJ) : field->OperIs(GT_IND));

        if ((prefixFlags & PREFIX_VOLATILE) != 0)
        {
            field->AsIndir()->SetVolatile();
        }
    }

    if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_INITCLASS) == 0)
    {
        return field;
    }

    CorInfoInitClassResult initClass =
        info.compCompHnd->initClass(resolvedToken->hField, info.compMethodHnd, impTokenLookupContextHandle);

    if ((initClass & CORINFO_INITCLASS_USE_HELPER) == 0)
    {
        return field;
    }

    GenTree* helperNode = CreateClassInitTree(resolvedToken);

    if (helperNode == nullptr)
    {
        assert(compDonotInline());
        return nullptr;
    }

    // Avoid creating struct COMMA nodes by adding the COMMA on top of the indirection's
    // address (we always get an IND/OBJ for a static struct field load). They would be
    // later transformed by fgMorphStructComma anyway.
    //
    // Extracting the helper call to a separate statement does have some advantages:
    //   - Avoids "poisoning" the entire tree with side effects from the helper call.
    //     This was only done for assignments and these are typically top level during
    //     import so it doesn't really matter.
    //   - Avoids poor register allocation due to a call appearing inside the tree.
    //     PMI diff does show a few diffs caused by register allocation changes.
    // However, loop hoisting depends on the type initialization helper call being
    // present in the tree, if it's in a separate statement it doesn't know if it's
    // safe to hoist the load.

    // TODO-MIKE-CQ: If the type has BeforeFieldInit initialization semantics we could
    // extract the helper call to a separate statement without worrying about side effect
    // ordering. We could even insert it at the start of the block and avoid any stack
    // spilling. But we still need to deal with the loop hoisting issue...

    // TODO-MIKE-Cleanup: SIMD COMMAs should not need this and previous implementation
    // actually preserved them in some cases (e.g. when the resulting tree was used
    // by a SIMD/HWINTRINSIC node rather than a store or call). Doesn't seem to
    // matter and anyway there are many other places that insist on transforming SIMD
    // COMMAs for no reason. Actually we could simply preserve all struct COMMAs but
    // there seems to be little advantage in doing that and requires a bit of work.

    if (GenTreeFieldAddr* fieldAddr = field->IsFieldAddr())
    {
        fieldAddr->SetAddr(gtNewCommaNode(helperNode, fieldAddr->GetAddr()));
        fieldAddr->AddSideEffects(helperNode->GetSideEffects());

        return fieldAddr;
    }

    if (varTypeIsStruct(field->GetType()))
    {
        GenTree* addr = field->AsIndir()->GetAddr();

        if (GenTreeFieldAddr* fieldAddr = addr->IsFieldAddr())
        {
            fieldAddr->SetAddr(gtNewCommaNode(helperNode, fieldAddr->GetAddr()));
            fieldAddr->AddSideEffects(helperNode->GetSideEffects());
        }
        else
        {
            addr = gtNewCommaNode(helperNode, addr);
        }

        field->AsIndir()->SetAddr(addr);
        field->AddSideEffects(addr->GetSideEffects());

        return field;
    }

    return gtNewCommaNode(helperNode, field);
}

GenTree* Importer::impImportStSFld(GenTree*                  value,
                                   CORINFO_CLASS_HANDLE      valueStructType,
                                   CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                   const CORINFO_FIELD_INFO& fieldInfo,
                                   unsigned                  prefixFlags)
{
    if (compIsForInlining())
    {
        switch (fieldInfo.fieldAccessor)
        {
            case CORINFO_FIELD_STATIC_ADDR_HELPER:
            case CORINFO_FIELD_STATIC_TLS:
                compInlineResult->NoteFatal(InlineObservation::CALLEE_STFLD_NEEDS_HELPER);
                return nullptr;
            case CORINFO_FIELD_STATIC_GENERICS_STATIC_HELPER:
            case CORINFO_FIELD_STATIC_READYTORUN_HELPER:
                // We may be able to inline the field accessors in specific instantiations of generic methods.
                compInlineResult->NoteFatal(InlineObservation::CALLSITE_STFLD_NEEDS_HELPER);
                return nullptr;
            default:
                break;
        }
    }

    impHandleAccessAllowed(fieldInfo.accessAllowed, fieldInfo.accessCalloutHelper);

    GenTree* field;

    if (fieldInfo.fieldAccessor == CORINFO_FIELD_STATIC_TLS)
    {
        field = CreateStaticFieldTlsAccess(CEE_STSFLD, resolvedToken, fieldInfo);
    }
    else if (IsStaticFieldHelperAccess(fieldInfo))
    {
        field = CreateStaticFieldHelperAccess(CEE_STSFLD, resolvedToken, fieldInfo);
    }
    else
    {
        field = CreateStaticFieldAddressAccess(CEE_STSFLD, resolvedToken, fieldInfo);
    }

    var_types fieldType = CorTypeToVarType(fieldInfo.fieldType);

    assert((fieldType == TYP_STRUCT) ? field->OperIs(GT_OBJ) : field->OperIs(GT_IND));

    if ((prefixFlags & PREFIX_VOLATILE) != 0)
    {
        field->AsIndir()->SetVolatile();
    }

    GenTree* helperNode = nullptr;

    if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_INITCLASS) != 0)
    {
        CorInfoInitClassResult initClass =
            info.compCompHnd->initClass(resolvedToken->hField, info.compMethodHnd, impTokenLookupContextHandle);

        if ((initClass & CORINFO_INITCLASS_USE_HELPER) != 0)
        {
            helperNode = CreateClassInitTree(resolvedToken);

            if (helperNode == nullptr)
            {
                assert(compDonotInline());
                return nullptr;
            }
        }
    }

    // We have to spill GLOB_REFs for static field stores since such fields
    // may be accessed via byrefs.
    impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("STSFLD stack spill temp"));

    if (fieldType == TYP_STRUCT)
    {
        if (helperNode != nullptr)
        {
            // TODO-MIKE-Review: We've already popped the value tree from the stack and
            // now we're appending the class initialization helper call, such that class
            // initialization will happen before whatever side effects the value tree may
            // have. This doesn't seem quite right when the type initializer doesn't have
            // BeforeFieldInit sematic.
            impSpillNoneAppendTree(helperNode);
        }

        field = impAssignStruct(field, value, CHECK_SPILL_NONE);
    }
    else
    {
        value = impConvertFieldStoreValue(field->GetType(), value);

        // TODO-MIKE-Cleanup: It would be better to generate stores from the get go
        field->SetOper(field->OperIs(GT_OBJ) ? GT_STORE_OBJ : GT_STOREIND);
        field->AsIndir()->SetValue(value);
        field->AddSideEffects(GTF_ASG | GTF_GLOB_REF | value->GetSideEffects());

        if (helperNode != nullptr)
        {
            field = gtNewCommaNode(helperNode, field);
        }
    }

    return field;
}

GenTree* Importer::CreateStaticFieldHelperAccess(OPCODE                    opcode,
                                                 CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                                 const CORINFO_FIELD_INFO& fieldInfo)
{
    assert((opcode == CEE_LDSFLD) || (opcode == CEE_STSFLD) || (opcode == CEE_LDSFLDA));
    assert(IsStaticFieldHelperAccess(fieldInfo));

    var_types    type   = CorTypeToVarType(fieldInfo.fieldType);
    ClassLayout* layout = type != TYP_STRUCT ? nullptr : typGetObjLayout(fieldInfo.structType);

    GenTree* addr = impImportStaticFieldAddressHelper(opcode, resolvedToken, fieldInfo);

    if ((layout != nullptr) && addr->IsFieldAddr())
    {
        addr->AsFieldAddr()->SetLayoutNum(typGetLayoutNum(layout));
    }

    if (opcode == CEE_LDSFLDA)
    {
        return addr;
    }

    GenTree* indir;

    if (type == TYP_STRUCT)
    {
        indir = gtNewObjNode(layout, addr);
    }
    else
    {
        indir = gtNewIndir(type, addr);
    }

    indir->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;

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

GenTree* Importer::CreateStaticFieldAddressAccess(OPCODE                    opcode,
                                                  CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                                  const CORINFO_FIELD_INFO& fieldInfo)
{
    assert((opcode == CEE_LDSFLD) || (opcode == CEE_STSFLD) || (opcode == CEE_LDSFLDA));
    assert(!IsStaticFieldHelperAccess(fieldInfo));
    assert((fieldInfo.fieldAccessor == CORINFO_FIELD_STATIC_ADDRESS) ||
           (fieldInfo.fieldAccessor == CORINFO_FIELD_STATIC_RVA_ADDRESS));

    var_types    type   = CorTypeToVarType(fieldInfo.fieldType);
    ClassLayout* layout = type != TYP_STRUCT ? nullptr : typGetObjLayout(fieldInfo.structType);

    void* pFldAddr = nullptr;
    void* fldAddr  = info.compCompHnd->getFieldAddress(resolvedToken->hField, &pFldAddr);
    // We should always be able to access this static's address directly
    assert(pFldAddr == nullptr);

    // Replace static read-only fields with constant if possible
    if ((opcode == CEE_LDSFLD) && ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_FINAL) != 0) &&
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

#ifdef TARGET_64BIT
    bool isStaticReadOnlyInited = false;
    bool isSpeculative          = true;
    if (info.compCompHnd->getStaticFieldCurrentClass(resolvedToken->hField, &isSpeculative) != NO_CLASS_HANDLE)
    {
        isStaticReadOnlyInited = !isSpeculative;

        if (isStaticReadOnlyInited)
        {
            JITDUMP("Initialized static read-only field '%s' is invariant.\n", eeGetFieldName(resolvedToken->hField));
        }
    }
#endif

    FieldSeqNode* fieldSeq = GetFieldSeqStore()->CreateSingleton(resolvedToken->hField);
    GenTree*      addr     = nullptr;

#if defined(TARGET_AMD64) || defined(TARGET_X86) || defined(TARGET_ARM)
    // TODO-MIKE-Cleanup: CLS_VAR_ADDR is almost useless, only VN still needs it.
    // VN treats it differently from CNS_INT and removing it causes some diffs.
    if ((opcode == CEE_LDSFLDA) ||
        opts.OptimizationDisabled() AMD64_ONLY(|| !comp->eeIsRIPRelativeAddress(fldAddr) || isStaticReadOnlyInited))
#endif
    {
        addr = gtNewIconHandleNode(fldAddr, HandleKind::Static, fieldSeq);

#ifdef TARGET_64BIT
        if (isStaticReadOnlyInited)
        {
            addr->AsIntCon()->SetHandleKind(HandleKind::ConstData);
        }
#endif

        if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_INITCLASS) != 0)
        {
            addr->gtFlags |= GTF_ICON_INITCLASS;
        }
    }

    if (opcode == CEE_LDSFLDA)
    {
        if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC_IN_HEAP) != 0)
        {
            addr = gtNewIndir(TYP_REF, addr);
            addr->gtFlags |= GTF_IND_NONFAULTING;

#ifdef TARGET_64BIT
            if (isStaticReadOnlyInited)
            {
                addr->gtFlags |= GTF_IND_INVARIANT | GTF_IND_NONNULL;
            }
#endif

            fieldSeq = GetFieldSeqStore()->GetBoxedValuePseudoField();
            addr     = new (comp, GT_FIELD_ADDR) GenTreeFieldAddr(addr, fieldSeq, TARGET_POINTER_SIZE);

            if (layout != nullptr)
            {
                addr->AsFieldAddr()->SetLayoutNum(typGetLayoutNum(layout));
            }
        }

        return addr;
    }

    GenTree* indir;

#ifndef TARGET_ARM64
    if (addr == nullptr)
    {
        addr = new (comp, GT_CLS_VAR_ADDR) GenTreeClsVar(fldAddr, fieldSeq);

        if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_INITCLASS) != 0)
        {
            addr->gtFlags |= GTF_CLS_VAR_INITCLASS;
        }

        indir = gtNewIndir(type, addr);

        // TODO-MIKE-CQ: Should GTF_IND_INVARIANT be set here? CLS_VAR did not have such a thing.
    }
    else
#endif // TARGET_ARM64
    {
        indir = gtNewIndir(type, addr);

#ifdef TARGET_64BIT
        if (isStaticReadOnlyInited)
        {
            indir->gtFlags |= GTF_IND_INVARIANT;
        }
#endif
    }

    indir->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;

    if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC_IN_HEAP) != 0)
    {
        indir->SetType(TYP_REF);
        addr = indir;

        fieldSeq = GetFieldSeqStore()->GetBoxedValuePseudoField();
        addr     = new (comp, GT_FIELD_ADDR) GenTreeFieldAddr(addr, fieldSeq, TARGET_POINTER_SIZE);

        if (layout != nullptr)
        {
            addr->AsFieldAddr()->SetLayoutNum(typGetLayoutNum(layout));
        }

        if (type == TYP_STRUCT)
        {
            indir = gtNewObjNode(layout, addr);
        }
        else
        {
            indir = gtNewIndir(type, addr);
        }

        indir->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;
    }

    return indir;
}

GenTree* Importer::impConvertFieldStoreValue(var_types storeType, GenTree* value)
{
    if (varActualType(storeType) == varActualType(value->GetType()))
    {
        return value;
    }

#ifndef TARGET_64BIT
    // V4.0 allows storing of i4 constant values to i8 type vars when IL verifier is bypassed (full
    // trust apps). The reason this works is that JIT stores an i4 constant in Gentree union during
    // importation and reads from the union as if it were a long during code generation. Though this
    // can potentially read garbage, one can get lucky to have this working correctly.
    //
    // This code pattern is generated by Dev10 MC++ compiler while storing to fields when compiled with
    // /O2 switch (default when compiling retail configs in Dev10) and a customer app has taken a
    // dependency on it. To be backward compatible, we will explicitly add an upward cast here so that
    // it works correctly always.
    //
    // Note that this is limited to x86 alone as there is no back compat to be addressed for Arm JIT
    // for V4.0.
    //
    // In UWP6.0 and beyond (post-.NET Core 2.0), we decided to let this cast from int to long be
    // generated for ARM as well as x86, so the following IL will be accepted:
    //     ldc.i4 2
    //     stsfld int64 foo::bar

    if (value->TypeIs(TYP_INT) && value->IsIntCon() && varTypeIsLong(storeType))
    {
        value->ChangeToLngCon(value->AsIntCon()->GetInt32Value());
    }
#else
    // Implicit narrowing from LONG to INT for x86 JIT compatiblity.
    if (varTypeIsI(value->GetType()) && varActualTypeIsInt(storeType))
    {
        value = gtNewCastNode(value, false, TYP_INT);
    }
    // Implicit widening from INT to LONG for x86 JIT compatiblity.
    else if (varActualTypeIsInt(value->GetType()) && varTypeIsI(storeType))
    {
        if (GenTreeIntCon* con = value->IsIntCon())
        {
            con->SetValue(TYP_LONG, con->GetInt32Value());
        }
        else
        {
            value = gtNewCastNode(value, false, TYP_LONG);
        }
    }
#endif
    // FLOAT/DOUBLE implicit conversions.
    else if (varTypeIsFloating(value->GetType()) && varTypeIsFloating(storeType))
    {
        value = gtNewCastNode(value, false, storeType);
    }

    return value;
}

void Importer::impHandleAccessAllowed(CorInfoIsAccessAllowedResult result, const CORINFO_HELPER_DESC& helperCall)
{
    if (result == CORINFO_ACCESS_ILLEGAL)
    {
        impInsertHelperCall(helperCall);
    }
}

void Importer::impInsertHelperCall(const CORINFO_HELPER_DESC& helperInfo)
{
    // Construct the argument list
    GenTreeCall::Use* args = nullptr;
    assert(helperInfo.helperNum != CORINFO_HELP_UNDEF);
    for (unsigned i = helperInfo.numArgs; i > 0; --i)
    {
        const CORINFO_HELPER_ARG& helperArg  = helperInfo.args[i - 1];
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
                currentArg = gtNewIconEmbModHndNode(helperArg.moduleHandle);
                break;
            case CORINFO_HELPER_ARG_TYPE_Const:
                currentArg = gtNewIconNode(helperArg.constant);
                break;
            default:
                NO_WAY("Illegal helper arg type");
        }
        args = gtPrependNewCallArg(currentArg, args);
    }

    // TODO-Review: Mark as CSE'able, and hoistable. Consider marking hoistable unless
    // you're in the inlinee. Also, consider sticking this in the first basic block.
    GenTree* callout = gtNewHelperCallNode(helperInfo.helperNum, TYP_VOID, args);
    impSpillNoneAppendTree(callout);
}

// Checks whether the return types of caller and callee are compatible
// so that callee can be tail called. Note that here we don't check
// compatibility in IL Verifier sense, but on the lines of return type
// sizes are equal and get returned in the same return register(s).
bool Compiler::impTailCallRetTypeCompatible(GenTreeCall* call, bool allowWidening)
{
    if (!varTypeIsStruct(call->GetRetSigType()))
    {
        var_types callerRetType = info.GetRetSigType();
        var_types calleeRetType = call->GetRetSigType();

        // Note that we can not relax this condition with genActualType() as the
        // calling convention dictates that the caller of a function with a small
        // typed return value is responsible for normalizing the return val.

        if (callerRetType == calleeRetType)
        {
            return true;
        }

        if (!varTypeIsIntegral(callerRetType) || !varTypeIsIntegral(calleeRetType))
        {
            return false;
        }

        // For integral types the managed calling convention dictates that callee
        // will widen the return value to 4 bytes, so we can allow implicit widening
        // in managed to managed tailcalls when dealing with <= 4 bytes.

        return allowWidening && (call->GetUnmanagedCallConv() == CorInfoCallConvExtension::Managed) &&
               (info.compCallConv == CorInfoCallConvExtension::Managed) && (varTypeSize(callerRetType) <= 4) &&
               (varTypeSize(calleeRetType) <= varTypeSize(callerRetType));
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

//------------------------------------------------------------------------
// impImportCall: import a call-inspiring opcode
//
// Arguments:
//    opcode                    - opcode that inspires the call
//    resolvedToken            - resolved token for the call target
//    pConstrainedResolvedToken - resolved constraint token (or nullptr)
//    newObjThis                - tree for this pointer or uninitalized newobj temp (or nullptr)
//    prefixFlags               - IL prefix flags for the call
//    callInfo                  - EE supplied info for the call
//    rawILOffset               - IL offset of the opcode
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
GenTreeCall* Importer::impImportCall(OPCODE                  opcode,
                                     CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                     CORINFO_RESOLVED_TOKEN* pConstrainedResolvedToken,
                                     GenTree*                newobjThis,
                                     int                     prefixFlags,
                                     CORINFO_CALL_INFO*      callInfo,
                                     const uint8_t*          ilAddr)
{
    assert(opcode == CEE_CALL || opcode == CEE_CALLVIRT || opcode == CEE_NEWOBJ || opcode == CEE_CALLI);

    IL_OFFSET              rawILOffset                    = static_cast<IL_OFFSET>(ilAddr - info.compCode);
    IL_OFFSETX             ilOffset                       = GetCallILOffsetX(rawILOffset);
    CORINFO_THIS_TRANSFORM constraintCallThisTransform    = CORINFO_NO_THIS_TRANSFORM;
    CORINFO_CONTEXT_HANDLE exactContextHnd                = nullptr;
    bool                   exactContextNeedsRuntimeLookup = false;
    const char*            tailCallFailReason             = nullptr;
    const bool             isReadonlyCall                 = (prefixFlags & PREFIX_READONLY) != 0;

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
        tailCallFailReason = "Caller is synchronized";
    }
    else if (opts.IsReversePInvoke())
    {
        tailCallFailReason = "Caller is Reverse P/Invoke";
    }
#if !FEATURE_FIXED_OUT_ARGS
    else if (info.compIsVarArgs)
    {
        tailCallFailReason = "Caller is varargs";
    }
#endif // FEATURE_FIXED_OUT_ARGS

    CORINFO_SIG_INFO*     sig;
    CORINFO_METHOD_HANDLE methHnd;
    CORINFO_SIG_INFO      calliSig;
    CORINFO_SIG_INFO      callSiteSig;
    CORINFO_SIG_INFO*     retTypeSig;
    unsigned              mflags;
    CORINFO_CLASS_HANDLE  clsHnd;
    unsigned              clsFlags;

    if (opcode == CEE_CALLI)
    {
        eeGetSig(pResolvedToken->token, pResolvedToken->tokenScope, pResolvedToken->tokenContext, &calliSig);

        sig        = &calliSig;
        methHnd    = nullptr;
        mflags     = ((sig->callConv & CORINFO_CALLCONV_HASTHIS) != 0) ? 0 : CORINFO_FLG_STATIC;
        clsHnd     = nullptr;
        clsFlags   = 0;
        retTypeSig = sig;
    }
    else
    {
        sig      = &callInfo->sig;
        methHnd  = callInfo->hMethod;
        mflags   = callInfo->methodFlags;
        clsHnd   = pResolvedToken->hClass;
        clsFlags = callInfo->classFlags;

        if ((((clsFlags & CORINFO_FLG_ARRAY) != 0) && (sig->retType != CORINFO_TYPE_VOID)) ||
            ((sig->callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_VARARG) ||
            ((sig->callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_NATIVEVARARG))
        {
            eeGetCallSiteSig(pResolvedToken->token, pResolvedToken->tokenScope, pResolvedToken->tokenContext,
                             &callSiteSig);

            retTypeSig = &callSiteSig;
        }
        else
        {
            retTypeSig = sig;
        }
    }

    var_types callRetTyp = CorTypeToVarType(sig->retType);

    JITDUMP("\nimpImportCall: opcode %s, kind %d, retType %s, retStructSize %u\n", opcodeNames[opcode], callInfo->kind,
            varTypeName(callRetTyp),
            (callRetTyp == TYP_STRUCT) ? info.compCompHnd->getClassSize(sig->retTypeSigClass) : 0);

    GenTreeCall* call  = nullptr;
    GenTree*     value = nullptr;

    if (opcode == CEE_CALLI)
    {
        call = impImportIndirectCall(&calliSig, ilOffset);

        // This should be checked in impImportBlockCode.
        assert(!compIsForInlining() || !(impInlineInfo->inlineCandidateInfo->dwRestrictions & INLINE_RESPECT_BOUNDARY));
    }
    else // (opcode != CEE_CALLI)
    {
        CorInfoIntrinsics intrinsicID = CORINFO_INTRINSIC_Count;

        if (compIsForInlining())
        {
            // Does this call site have security boundary restrictions?
            if (impInlineInfo->inlineCandidateInfo->dwRestrictions & INLINE_RESPECT_BOUNDARY)
            {
                compInlineResult->NoteFatal(InlineObservation::CALLSITE_CROSS_BOUNDARY_SECURITY);
                return nullptr;
            }

            // Does the inlinee use StackCrawlMark
            if (mflags & CORINFO_FLG_DONT_INLINE_CALLER)
            {
                compInlineResult->NoteFatal(InlineObservation::CALLEE_STACK_CRAWL_MARK);
                return nullptr;
            }

            // For now ignore varargs
            if ((sig->callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_NATIVEVARARG)
            {
                compInlineResult->NoteFatal(InlineObservation::CALLEE_HAS_NATIVE_VARARGS);
                return nullptr;
            }

            if ((sig->callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_VARARG)
            {
                compInlineResult->NoteFatal(InlineObservation::CALLEE_HAS_MANAGED_VARARGS);
                return nullptr;
            }

            if ((mflags & CORINFO_FLG_VIRTUAL) && (sig->sigInst.methInstCount != 0) && (opcode == CEE_CALLVIRT))
            {
                compInlineResult->NoteFatal(InlineObservation::CALLEE_IS_GENERIC_VIRTUAL);
                return nullptr;
            }
        }

        // <NICE> Factor this into getCallInfo </NICE>
        bool isSpecialIntrinsic = false;
        if ((mflags & (CORINFO_FLG_INTRINSIC | CORINFO_FLG_JIT_INTRINSIC)) != 0)
        {
            const bool isTailCall = (tailCallFailReason == nullptr) && ((prefixFlags & PREFIX_TAILCALL) != 0);

            value = impIntrinsic(newobjThis, sig, mflags, pResolvedToken, isReadonlyCall, isTailCall,
                                 pConstrainedResolvedToken, callInfo, &intrinsicID, &isSpecialIntrinsic);

            if (compDonotInline())
            {
                return nullptr;
            }

            if (value != nullptr)
            {
#ifdef FEATURE_READYTORUN_COMPILER
                if (GenTreeIntrinsic* intrinsic = value->IsIntrinsic())
                {
                    if (opts.IsReadyToRun())
                    {
                        noway_assert(callInfo->kind == CORINFO_CALL);
                        intrinsic->gtEntryPoint = callInfo->codePointerLookup.constLookup;
                    }
                    else
                    {
                        intrinsic->gtEntryPoint.addr       = nullptr;
                        intrinsic->gtEntryPoint.accessType = IAT_VALUE;
                    }
                }
#endif

                if (callRetTyp == TYP_VOID)
                {
                    unsigned spillDepth = verCurrentState.esStackDepth;

                    // TODO-MIKE-Review: This is needed for NI_CORINFO_INTRINSIC_ByReference_Ctor,
                    // that needs to be handled in ImportNewObj instead.
                    if (opcode == CEE_NEWOBJ)
                    {
                        // We actually did push something, so don't spill the thing we just pushed.
                        assert(spillDepth > 0);
                        spillDepth--;
                    }

                    impAppendTree(value, spillDepth);

                    return nullptr;
                }

                call = value->IsCall();

                if (call != nullptr)
                {
                    goto PUSH_CALL;
                }

                goto PUSH_VALUE;
            }
        }

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
        exactContextNeedsRuntimeLookup = callInfo->exactContextNeedsRuntimeLookup;

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
                        return nullptr;
                    }

                    GenTree* stubAddr = impRuntimeLookupToTree(pResolvedToken, &callInfo->stubLookup, methHnd);
                    assert(!compDonotInline());
                    assert(stubAddr->TypeIs(TYP_I_IMPL));

                    // The stubAddr may be a
                    // complex expression. As it is evaluated after the args,
                    // it may cause registered args to be spilled. Simply spill it.

                    LclVarDsc* lcl = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("VirtualCall with runtime lookup"));
                    GenTree*   asg = comp->gtNewLclStore(lcl, TYP_I_IMPL, stubAddr);
                    impSpillNoneAppendTree(asg);
                    stubAddr = comp->gtNewLclLoad(lcl, TYP_I_IMPL);

                    // Create the actual call node

                    assert((sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_VARARG &&
                           (sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_NATIVEVARARG);

                    call = gtNewIndCallNode(stubAddr, callRetTyp, nullptr);
                    call->gtFlags |= GTF_CALL_VIRT_STUB;

                    X86_ONLY(tailCallFailReason = "VirtualCall with runtime lookup");
                }
                else
                {
                    // The stub address is known at compile time
                    call                     = gtNewUserCallNode(callInfo->hMethod, callRetTyp, nullptr, ilOffset);
                    call->gtStubCallStubAddr = callInfo->stubLookup.constLookup.addr;
                    call->gtFlags |= GTF_CALL_VIRT_STUB;
                    assert(callInfo->stubLookup.constLookup.accessType != IAT_PPVALUE &&
                           callInfo->stubLookup.constLookup.accessType != IAT_RELPVALUE);
                    if (callInfo->stubLookup.constLookup.accessType == IAT_PVALUE)
                    {
                        call->gtCallMoreFlags |= GTF_CALL_M_VIRTSTUB_REL_INDIRECT;
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
                call = gtNewUserCallNode(callInfo->hMethod, callRetTyp, nullptr, ilOffset);
                call->gtFlags |= GTF_CALL_VIRT_VTABLE;

                // Should we expand virtual call targets early for this method?
                //
                if (opts.compExpandCallsEarly)
                {
                    // Mark this method to expand the virtual call target early in fgMorpgCall
                    call->SetExpandedEarly();
                }
                break;
            }

            case CORINFO_VIRTUALCALL_LDVIRTFTN:
            {
                if (compIsForInlining())
                {
                    compInlineResult->NoteFatal(InlineObservation::CALLSITE_HAS_CALL_VIA_LDVIRTFTN);
                    return nullptr;
                }

                assert(!(mflags & CORINFO_FLG_STATIC)); // can't call a static method
                assert(!(clsFlags & CORINFO_FLG_VALUECLASS));
                // OK, We've been told to call via LDVIRTFTN, so just
                // take the call now....

                GenTreeCall::Use* args = PopCallArgs(sig);

                GenTree* thisPtr = impPopStack().val;
                thisPtr          = impTransformThis(thisPtr, pConstrainedResolvedToken, callInfo->thisTransform);
                assert(thisPtr != nullptr);

                GenTree* thisPtrUses[2];
                impMakeMultiUse(thisPtr, 2, thisPtrUses, CHECK_SPILL_ALL DEBUGARG("LDVIRTFTN this pointer"));

                GenTree* fptr = impImportLdvirtftn(thisPtrUses[0], pResolvedToken, callInfo);
                assert(fptr->TypeIs(TYP_I_IMPL));

                // Now make an indirect call through the function pointer

                SpillStackCheck(fptr, CHECK_SPILL_ALL); // TODO-MIKE-Review: Can fptr really interfere with anything?
                LclVarDsc* lcl = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("VirtualCall through function pointer"));
                impSpillNoneAppendTree(comp->gtNewLclStore(lcl, TYP_I_IMPL, fptr));
                fptr = comp->gtNewLclLoad(lcl, TYP_I_IMPL);

                // Create the actual call node

                call                = gtNewIndCallNode(fptr, callRetTyp, args, ilOffset);
                call->gtCallThisArg = gtNewCallArgs(thisPtrUses[1]);

                if ((sig->sigInst.methInstCount != 0) && IsTargetAbi(CORINFO_CORERT_ABI))
                {
                    // CoreRT generic virtual method: need to handle potential fat function pointers
                    addFatPointerCandidate(call);
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
                call = gtNewUserCallNode(callInfo->hMethod, callRetTyp, nullptr, ilOffset);

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
                    call->setEntryPoint(callInfo->codePointerLookup.constLookup);
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

                GenTree* fptr = impLookupToTree(pResolvedToken, &callInfo->codePointerLookup, HandleKind::MethodAddr,
                                                callInfo->hMethod);

                if (compDonotInline())
                {
                    return nullptr;
                }

                // Now make an indirect call through the function pointer

                assert(fptr->TypeIs(TYP_I_IMPL));

                SpillStackCheck(fptr, CHECK_SPILL_ALL); // TODO-MIKE-Review: Can fptr really interfere with anything?
                LclVarDsc* lcl = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("Indirect call through function pointer"));
                impSpillNoneAppendTree(comp->gtNewLclStore(lcl, TYP_I_IMPL, fptr));
                fptr = comp->gtNewLclLoad(lcl, TYP_I_IMPL);

                call = gtNewIndCallNode(fptr, callRetTyp, nullptr, ilOffset);

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

        if ((mflags & CORINFO_FLG_NOGCCHECK) != 0)
        {
            call->gtCallMoreFlags |= GTF_CALL_M_NOGCCHECK;
        }

        if (isSpecialIntrinsic)
        {
            call->gtCallMoreFlags |= GTF_CALL_M_SPECIAL_INTRINSIC;
        }

        if ((mflags & CORINFO_FLG_DELEGATE_INVOKE) != 0)
        {
            assert(!(mflags & CORINFO_FLG_STATIC)); // can't call a static method
            assert(mflags & CORINFO_FLG_FINAL);

            // Set the delegate flag
            call->gtCallMoreFlags |= GTF_CALL_M_DELEGATE_INV;

            if (callInfo->wrapperDelegateInvoke)
            {
                call->gtCallMoreFlags |= GTF_CALL_M_WRAPPER_DELEGATE_INV;
            }

            if (opcode == CEE_CALLVIRT)
            {
                assert(mflags & CORINFO_FLG_FINAL);

                // It should have the GTF_CALL_NULLCHECK flag set. Reset it.
                assert(call->gtFlags & GTF_CALL_NULLCHECK);
                call->gtFlags &= ~GTF_CALL_NULLCHECK;
            }
        }
    }

    assert(sig != nullptr);
    assert((clsHnd != NO_CLASS_HANDLE) || (opcode == CEE_CALLI));

    // CALL_VIRT and NEWOBJ must have a THIS pointer
    assert((opcode != CEE_CALLVIRT && opcode != CEE_NEWOBJ) || (sig->callConv & CORINFO_CALLCONV_HASTHIS));
    // static bit and hasThis are negations of one another
    assert(((mflags & CORINFO_FLG_STATIC) != 0) == ((sig->callConv & CORINFO_CALLCONV_HASTHIS) == 0));
    assert(call != nullptr);

    if ((sig->callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_VARARG ||
        (sig->callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_NATIVEVARARG)
    {
#if !FEATURE_VARARG
        BADCODE("Varargs not supported.");
#else
        assert(!compIsForInlining());

        call->gtCallMoreFlags |= GTF_CALL_M_VARARGS;

#ifdef TARGET_X86
        call->gtFlags |= GTF_CALL_POP_ARGS;

        // Can't allow tailcall for varargs as it is caller-pop. The caller
        // will be expecting to pop a certain number of arguments, but if we
        // tailcall to a function with a different number of arguments, we
        // are hosed. There are ways around this (caller remembers esp value,
        // varargs is not caller-pop, etc), but not worth it.
        if (tailCallFailReason == nullptr)
        {
            tailCallFailReason = "Callee is varargs";
        }
#endif

        // CALLI already has the correct signature, for other opcodes we need to switch to
        // the call site signature to get the correct number of args.

        if (opcode != CEE_CALLI)
        {
            assert(sig->numArgs <= retTypeSig->numArgs);

            if ((sig->retTypeSigClass != NO_CLASS_HANDLE) && (sig->retTypeSigClass != retTypeSig->retTypeSigClass) &&
                (sig->retType != CORINFO_TYPE_CLASS) && (sig->retType != CORINFO_TYPE_BYREF) &&
                (sig->retType != CORINFO_TYPE_PTR) && (sig->retType != CORINFO_TYPE_VAR))
            {
                // Make sure that all valuetypes (including enums) that we push are loaded.
                // This is to guarantee that if a GC is triggerred from the prestub of this
                // methods, all valuetypes in the method signature are already loaded.
                // We need to be able to find the size of the valuetypes, but we cannot
                // do a class-load from within GC.
                // PopCallArgs does this for all types in the signature but we need to
                // use the call site signature and due to type equivalence the return type
                // of the method's signature may be different, so we handle it here.
                // TODO-MIKE-Review: What about param types, can't those be different too?
                info.compCompHnd->classMustBeLoadedBeforeCodeIsRun(sig->retTypeSigClass);
            }

            sig = retTypeSig;
        }

// We will have "cookie" as the last argument but we cannot push
// it on the operand stack because we may overflow, so we append it
// to the arg list next after we pop them
#endif // FEATURE_VARARG
    }

    //--------------------------- Inline NDirect ------------------------------

    // For inline cases we technically should look at both the current
    // block and the call site block (or just the latter if we've
    // fused the EH trees). However the block-related checks pertain to
    // EH and we currently won't inline a method with EH. So for
    // inlinees, just checking the call site block is sufficient.
    {
        // New lexical block here to avoid compilation errors because of GOTOs.
        BasicBlock* block = compIsForInlining() ? impInlineInfo->iciBlock : currentBlock;
        impCheckForPInvokeCall(call, methHnd, sig, mflags, block);
    }

    if ((call->gtFlags & GTF_CALL_UNMANAGED) != 0)
    {
        // We set up the unmanaged call by linking the frame, disabling GC, etc
        // This needs to be cleaned up on return.
        // In addition, native calls have different normalization rules than managed code
        // (managed calling convention always widens return values in the callee)
        if (tailCallFailReason == nullptr)
        {
            tailCallFailReason = "Callee is native";
        }

        PopUnmanagedCallArgs(call, sig);
    }
    else
    {
#ifdef UNIX_X86_ABI
        call->gtFlags |= GTF_CALL_POP_ARGS;
#endif

        if ((opcode == CEE_CALLI) && ((sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_DEFAULT) &&
            ((sig->callConv & CORINFO_CALLCONV_MASK) != CORINFO_CALLCONV_VARARG))
        {
            if (CreateCallICookie(call, sig) == nullptr)
            {
                assert(compDonotInline());

                return nullptr;
            }

            if (tailCallFailReason == nullptr)
            {
                tailCallFailReason = "PInvoke calli";
            }
        }

        // Create the argument list

        GenTree* extraArg = nullptr;

        if ((sig->callConv & CORINFO_CALLCONV_MASK) == CORINFO_CALLCONV_VARARG)
        {
            extraArg = CreateVarargsCallArgHandle(call, sig);

            if (extraArg == nullptr)
            {
                assert(compDonotInline());

                return nullptr;
            }
        }

        if ((sig->callConv & CORINFO_CALLCONV_PARAMTYPE) != 0)
        {
            assert(opcode != CEE_CALLI);
            assert(extraArg == nullptr);

            extraArg =
                CreateGenericCallTypeArg(call, callInfo, pResolvedToken, pConstrainedResolvedToken, isReadonlyCall);

            if (extraArg == nullptr)
            {
                assert(compDonotInline());

                return nullptr;
            }
        }

        call->gtCallArgs = PopCallArgs(sig, extraArg);

        for (GenTreeCall::Use& use : call->Args())
        {
            call->gtFlags |= use.GetNode()->gtFlags & GTF_GLOB_EFFECT;
        }

        if (opcode == CEE_NEWOBJ)
        {
            return call;
        }

        //-------------------------------------------------------------------------
        // The "this" pointer

        if (((mflags & CORINFO_FLG_STATIC) == 0) && ((sig->callConv & CORINFO_CALLCONV_EXPLICITTHIS) == 0))
        {
            GenTree* obj = impPopStack().val;

            obj = impTransformThis(obj, pConstrainedResolvedToken, constraintCallThisTransform);
            if (compDonotInline())
            {
                return nullptr;
            }

            // Store the "this" value in the call
            call->gtFlags |= obj->gtFlags & GTF_GLOB_EFFECT;
            call->gtCallThisArg = gtNewCallArgs(obj);

            // Is this a virtual or interface call?
            if (call->IsVirtual())
            {
                // only true object pointers can be virtual
                assert(obj->gtType == TYP_REF);

                // See if we can devirtualize.

                const bool isExplicitTailCall = (prefixFlags & PREFIX_TAILCALL_EXPLICIT) != 0;
                impDevirtualizeCall(call, pResolvedToken, &callInfo->hMethod, &callInfo->methodFlags,
                                    &callInfo->contextHandle, &exactContextHnd, isExplicitTailCall, rawILOffset);
            }
        }
    }

DONE:
    // In debug we want to be able to register callsites with the EE.
    INDEBUG(call->callSig = new (comp, CMK_DebugOnly) CORINFO_SIG_INFO(*sig));

    if (call->TypeIs(TYP_STRUCT))
    {
        impInitializeStructCall(call, retTypeSig->retTypeClass);
    }

    if ((prefixFlags & PREFIX_TAILCALL) != 0)
    {
        SetupTailCall(call, opcode, prefixFlags, sig, pResolvedToken, methHnd, tailCallFailReason);
    }

    // Note: we assume that small return types are already normalized by the managed callee
    // or by the pinvoke stub for calls to unmanaged code.

    if (compIsForInlining() && opcode == CEE_CALLVIRT)
    {
        GenTree* callObj = call->gtCallThisArg->GetNode();

        if ((call->IsVirtual() || (call->gtFlags & GTF_CALL_NULLCHECK)) &&
            impInlineIsGuaranteedThisDerefBeforeAnySideEffects(nullptr, call->gtCallArgs, callObj))
        {
            impInlineInfo->thisDereferencedFirst = true;
        }
    }

#if defined(DEBUG) || defined(INLINE_DATA)
    // Keep track of the raw IL offset of the call
    call->gtRawILOffset = rawILOffset;
#endif

    // Is it an inline candidate?
    impMarkInlineCandidate(call, exactContextHnd, exactContextNeedsRuntimeLookup, callInfo);

    if ((sig->flags & CORINFO_SIGFLAG_FAT_CALL) != 0)
    {
        assert(opcode == CEE_CALLI);
        addFatPointerCandidate(call);
    }

    assert(opcode != CEE_NEWOBJ);

    if (callRetTyp == TYP_VOID)
    {
        impSpillAllAppendTree(call);

        return nullptr;
    }

PUSH_CALL:
    // TODO: consider handling fatcalli cases this way too...?
    if (call->IsInlineCandidate() || call->IsGuardedDevirtualizationCandidate())
    {
        assert(opts.OptEnabled(CLFLG_INLINING));
        assert(!call->IsFatPointerCandidate()); // We should not try to inline calli.

        // Make the call its own tree (spill the stack if needed).
        impSpillAllAppendTree(call);

        // TODO: Still using the widened type.
        GenTreeRetExpr* retExpr                         = gtNewRetExpr(call);
        call->gtInlineCandidateInfo->retExprPlaceholder = retExpr;

        value = retExpr;
    }
    else
    {
        // For non-candidates we must also spill, since we might have locals live on the eval
        // stack that this call can modify.
        //
        // Suppress this for certain well-known call targets that we know won't modify locals,
        // eg calls that are recognized in gtCanOptimizeTypeEquality. Otherwise we may break key
        // fragile pattern matches later on.
        bool spillStack = true;

        if (call->IsFatPointerCandidate())
        {
            // fatPointer candidates should be in statements of the form call() or var = call().
            // Such form allows to find statements with fat calls without walking through whole trees
            // and removes problems with cutting trees.
            assert(IsTargetAbi(CORINFO_CORERT_ABI));

            LclVarDsc* calliTempLcl = lvaAllocTemp(true DEBUGARG("calli fat pointer temp"));
            // TODO-MIKE-Review: CHECK_SPILL_NONE followed by CHECK_SPILL_ALL below, this seems
            // bogus. We'll end up with something like tmp = CALL, other stack contents, tmp use
            // instead of other stack contents, tmp = CALL, tmp use.
            impAppendTempStore(calliTempLcl, call, call->GetRetLayout(), CHECK_SPILL_NONE);

            value = gtNewLclvNode(calliTempLcl, varActualType(calliTempLcl->GetType()));
        }
        else
        {
            if (call->IsHelperCall() &&
                (call->IsTypeHandleToRuntimeTypeHelperCall() || call->IsTypeHandleToRuntimeTypeHandleHelperCall()))
            {
                spillStack = false;
            }
            else if ((call->gtCallMoreFlags & GTF_CALL_M_SPECIAL_INTRINSIC) != 0)
            {
                spillStack = false;
            }

            value = call;
        }

        if (spillStack)
        {
            impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("non-inline candidate call"));
        }
    }

    // Usual native calling conventions do not automatically widen returned small int values to
    // INT so we have to do it in the caller. R2R also follows the native calling and widens in
    // the caller, even if the callee is a managed method that automatically widens itself.

    if (varTypeIsSmall(callRetTyp) && (opts.IsReadyToRun() || call->IsUnmanaged()))
    {
        value = gtNewCastNode(value, false, callRetTyp);
    }

PUSH_VALUE:
    if (retTypeSig->retTypeClass != NO_CLASS_HANDLE)
    {
        impPushOnStack(value, impMakeTypeInfo(retTypeSig->retType, retTypeSig->retTypeClass));
    }
    else
    {
        impPushOnStack(value);
    }

    return nullptr;
}
#ifdef _PREFAST_
#pragma warning(pop)
#endif

GenTree* Importer::CreateCallICookie(GenTreeCall* call, CORINFO_SIG_INFO* sig)
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
            return nullptr;
        }

        IMPL_LIMITATION("Can't get PInvoke cookie (cross module generics)");
    }

    void* valueAddr;
    void* value = info.compCompHnd->GetCookieForPInvokeCalliSig(sig, &valueAddr);

    GenTree* cookie = gtNewConstLookupTree(value, valueAddr, HandleKind::ConstData, nullptr);
    cookie->SetDoNotCSE();

    if (cookie->OperIs(GT_IND))
    {
        cookie->AsIndir()->GetAddr()->AsIntCon()->SetDoNotCSE();
    }
    else
    {
        assert(cookie->IsIntCon());
    }

    call->gtCallCookie = cookie;

    return cookie;
}

GenTree* Importer::CreateVarargsCallArgHandle(GenTreeCall* call, CORINFO_SIG_INFO* sig)
{
    assert(!compIsForInlining());

    if (!info.compCompHnd->canGetVarArgsHandle(sig))
    {
        compInlineResult->NoteFatal(InlineObservation::CALLSITE_CANT_EMBED_VARARGS_COOKIE);
        return nullptr;
    }

    void* handleAddr;
    void* handle = info.compCompHnd->getVarArgsHandle(sig, &handleAddr);
    return gtNewConstLookupTree(handle, handleAddr, HandleKind::ConstData, nullptr);
}

GenTree* Importer::CreateGenericCallTypeArg(GenTreeCall*            call,
                                            CORINFO_CALL_INFO*      callInfo,
                                            CORINFO_RESOLVED_TOKEN* resolvedToken,
                                            CORINFO_RESOLVED_TOKEN* constrainedResolvedToken,
                                            bool                    isReadonlyCall)
{
    assert(call->IsUserCall());

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

    CORINFO_CLASS_HANDLE clsHnd                         = resolvedToken->hClass;
    auto                 exactContextHnd                = callInfo->contextHandle;
    bool                 exactContextNeedsRuntimeLookup = callInfo->exactContextNeedsRuntimeLookup;
    auto                 clsFlags                       = callInfo->classFlags;

    if (clsHnd == nullptr)
    {
        NO_WAY("CALLI on parameterized type");
    }

    GenTree* instParam;

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
                instParam = comp->gtNewConstLookupTree(callInfo->instParamLookup, HandleKind::Method,
                                                       exactMethodHandle DEBUGARG(exactMethodHandle));
            }
            else
#endif
            {
                info.compCompHnd->methodMustBeLoadedBeforeCodeIsRun(exactMethodHandle);
                instParam = gtNewIconEmbMethHndNode(exactMethodHandle);
            }
        }
        else
        {
            instParam = impTokenToHandle(resolvedToken, /* mustRestoreHandle */ true);
            if (instParam == nullptr)
            {
                assert(compDonotInline());
                return nullptr;
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
            return nullptr;
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
                instParam = comp->gtNewConstLookupTree(callInfo->instParamLookup, HandleKind::Class,
                                                       exactClassHandle DEBUGARG(exactClassHandle));
            }
            else
#endif
            {
                info.compCompHnd->classMustBeLoadedBeforeCodeIsRun(exactClassHandle);
                instParam = gtNewIconEmbClsHndNode(exactClassHandle);
            }
        }
        else
        {
            // If the EE was able to resolve a constrained call, the instantiating parameter to use is the type
            // by which the call was constrained with. We embed pConstrainedResolvedToken as the extra argument
            // because resolvedToken is an interface method and interface types make a poor generic context.
            if (constrainedResolvedToken != nullptr)
            {
                instParam = impTokenToHandle(constrainedResolvedToken, /* mustRestoreHandle */ true);
            }
            else
            {
                instParam = impParentClassTokenToHandle(resolvedToken, /* mustRestoreHandle */ true);
            }

            if (instParam == nullptr)
            {
                assert(compDonotInline());
                return nullptr;
            }
        }
    }

    return instParam;
}

void Importer::SetupTailCall(GenTreeCall*            call,
                             OPCODE                  opcode,
                             int                     prefixFlags,
                             CORINFO_SIG_INFO*       sig,
                             CORINFO_RESOLVED_TOKEN* resolvedToken,
                             CORINFO_METHOD_HANDLE   methodHandle,
                             const char*             tailCallFailReason)
{
    const bool isExplicitTailCall = (prefixFlags & PREFIX_TAILCALL_EXPLICIT) != 0;
    const bool isImplicitTailCall = (prefixFlags & PREFIX_TAILCALL_IMPLICIT) != 0;

    assert(isExplicitTailCall != isImplicitTailCall);

    // This check cannot be performed for implicit tail calls because impIsImplicitTailCallCandidate
    // is not checking whether return types types are compatible before marking a call node with
    // PREFIX_TAILCALL_IMPLICIT.
    // As a result it is possible that in the following case, we find that the type stack is not
    // empty if Callee() is considered for implicit tail calling.
    //
    //      int Caller(..) { .... void Callee(); ret val; ... }
    //
    // Note that we cannot check return type compatibility before impImportCall as we don't have
    // required info or need to duplicate some of the logic of impImportCall.
    //
    // For implicit tail calls, we perform this check after return types are known to be compatible.

    if (isExplicitTailCall && (verCurrentState.esStackDepth != 0))
    {
        BADCODE("Stack should be empty after tailcall");
    }

    // For opportunistic tailcalls we allow implicit widening, i.e. tailcalls from int32 -> int16,
    // since the managed calling convention dictates that the callee widens the value. For explicit
    // tailcalls we don't want to require this detail of the calling convention to bubble up to the
    // tailcall helpers.

    if ((tailCallFailReason == nullptr) && !comp->impTailCallRetTypeCompatible(call, isImplicitTailCall))
    {
        tailCallFailReason = "Return types are not tail call compatible";
    }

    if ((tailCallFailReason == nullptr) && isImplicitTailCall && (verCurrentState.esStackDepth != 0))
    {
        BADCODE("Stack should be empty after tailcall");
    }

    assert(!isExplicitTailCall || (currentBlock->bbJumpKind == BBJ_RETURN));

    // Ask VM for permission to tailcall
    if (tailCallFailReason != nullptr)
    {
        JITDUMP("\nRejecting %splicit tail call for [%06u], reason: '%s'\n", isExplicitTailCall ? "ex" : "im",
                call->GetID(), tailCallFailReason);

        info.compCompHnd->reportTailCallDecision(info.compMethodHnd, methodHandle, isExplicitTailCall, TAILCALL_FAIL,
                                                 tailCallFailReason);

        return;
    }

    // True virtual or indirect calls, shouldn't pass in a callee handle.
    CORINFO_METHOD_HANDLE exactCalleeHnd = (!call->IsUserCall() || call->IsVirtual()) ? nullptr : methodHandle;

    if (!info.compCompHnd->canTailCall(info.compMethodHnd, methodHandle, exactCalleeHnd, isExplicitTailCall))
    {
        JITDUMP("\ninfo.compCompHnd->canTailCall returned false for call [%06u]\n", call->GetID());

        return;
    }

    if (isExplicitTailCall)
    {
        // In case of explicit tail calls, mark it so that it is not considered for inlining.
        call->gtCallMoreFlags |= GTF_CALL_M_EXPLICIT_TAILCALL;

        JITDUMP("\nGTF_CALL_M_EXPLICIT_TAILCALL set for call [%06u]\n", call->GetID());

        if ((prefixFlags & PREFIX_TAILCALL_STRESS) != 0)
        {
            call->gtCallMoreFlags |= GTF_CALL_M_STRESS_TAILCALL;

            JITDUMP("\nGTF_CALL_M_STRESS_TAILCALL set for call [%06u]\n", call->GetID());
        }
    }
    else
    {
#if !FEATURE_TAILCALL_OPT
        NYI("Implicit tail call prefix on a target which doesn't support opportunistic tail calls");
#else
        // Must be an implicit tail call.
        assert(isImplicitTailCall);

        // It is possible that a call node is both an inline candidate and marked
        // for opportunistic tail calling. Inlining happens before morhphing of
        // trees. If inlining of an inline candidate gets aborted for whatever
        // reason, it will survive to the morphing stage at which point it will
        // be transformed into a tail call after performing additional checks.
        call->gtCallMoreFlags |= GTF_CALL_M_IMPLICIT_TAILCALL;

        JITDUMP("\nGTF_CALL_M_IMPLICIT_TAILCALL set for call [%06u]\n", call->GetID());
#endif
    }

    // This might or might not turn into a tailcall. We do more checks in morph.
    // For explicit tailcalls we need more information in morph in case it turns
    // out to be a helper-based tailcall.
    if (isExplicitTailCall)
    {
        call->tailCallInfo = new (comp, CMK_CorTailCallInfo) TailCallSiteInfo(sig);

        switch (opcode)
        {
            case CEE_CALLI:
                call->tailCallInfo->SetCalli();
                break;
            case CEE_CALLVIRT:
                call->tailCallInfo->SetCallvirt(resolvedToken);
                break;
            default:
                call->tailCallInfo->SetCall(resolvedToken);
                break;
        }
    }

    // A tail recursive call is a potential loop from the current block to the start of the method.
    if (gtIsRecursiveCall(methodHandle))
    {
        assert(verCurrentState.esStackDepth == 0);

        BasicBlock* loopHead = nullptr;

        if (opts.IsOSR())
        {
            // OSR doesn't import the entry block by default,
            // we have to do it now for recursive tail calls.

            assert(comp->fgEntryBB != nullptr);

            JITDUMP("\nOSR: found tail recursive call in the method, scheduling " FMT_BB " for importation\n",
                    comp->fgEntryBB->bbNum);

            impImportBlockPending(comp->fgEntryBB);
            loopHead = comp->fgEntryBB;
        }
        else
        {
            // For normal jitting we'll branch back to the firstBB,
            // this should already be imported.

            loopHead = comp->fgFirstBB;
        }

        JITDUMP("\nFound recursive tail call. Mark " FMT_BB " to " FMT_BB " as having a backward branch.\n",
                loopHead->bbNum, currentBlock->bbNum);

        comp->fgMarkBackwardJump(loopHead, currentBlock);
    }
}

void Importer::impInitializeStructCall(GenTreeCall* call, CORINFO_CLASS_HANDLE retClass)
{
    assert(call->GetType() == TYP_STRUCT);
    assert(call->GetRetSigType() == TYP_STRUCT);
    assert(call->GetRetLayout() == nullptr);

    ClassLayout* layout = typGetObjLayout(retClass);
    var_types    type   = typGetStructType(layout);

    call->SetType(type);
    call->SetRetSigType(type);
    call->SetRetLayout(layout);

    StructPassing   retKind = abiGetStructReturnType(layout, call->GetUnmanagedCallConv(), call->IsVarargs());
    ReturnTypeDesc* retDesc = call->GetRetDesc();

    if (retKind.kind == SPK_PrimitiveType)
    {
        retDesc->InitializePrimitive(retKind.type);
    }
#if FEATURE_MULTIREG_RET
    else if (retKind.kind == SPK_ByValue)
    {
        assert(retKind.type == TYP_STRUCT);

        retDesc->InitializeStruct(comp, layout);
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

GenTree* Importer::impCanonicalizeMultiRegReturnValue(GenTree* value, CORINFO_CLASS_HANDLE retClass)
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
        // TODO-MIKE-Cleanup: This is dubious. Existing code probably tried to ensure
        // that the callee returns the value in the same registers as the caller and
        // it happens so that on ARM varargs affects the return ABI (no HFA for either
        // parameters or returns, unlike win-arm64 ABI where varargs only blocks HFA
        // parameters). It may be better to explicitly check the return registers to
        // ensure that we don't have any surprises with new calling conventions (e.g.
        // native win-x86 ABI returns Vector2 in RAX/RDX but __vectorcall should use
        // XMM0/XMM1 instead).

        // We don't support varargs on ARM so just assert.
        ARM_ONLY(noway_assert(!call->IsVarargs());)

        return value;
    }

    if (varTypeIsSIMD(value->GetType()) || info.GetRetLayout()->IsHfa())
    {
        return value;
    }

    LclVarDsc* lcl = nullptr;

    if (value->OperIs(GT_LCL_VAR))
    {
        lcl = value->AsLclVar()->GetLcl();

        if (lcl->IsImplicitByRefParam())
        {
            // Implicit byref params will be transformed into indirs so
            // we need a temp even if now they're LCL_VARs.

            lcl = nullptr;
        }
    }

    if ((lcl != nullptr) && (info.GetRetSigType() == TYP_STRUCT))
    {
        // Make sure that this struct stays in memory and doesn't get promoted.
        lcl->lvIsMultiRegRet = true;
    }

    return value;
#endif
}

#endif // FEATURE_MULTIREG_RET

GenTree* Importer::impSpillPseudoReturnBufferCall(GenTreeCall* call)
{
    assert(call->TreatAsHasRetBufArg());
    assert(call->TypeIs(TYP_STRUCT));

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

    LclVarDsc* tmpLcl = lvaAllocTemp(true DEBUGARG("pseudo return buffer"));
    impAppendTempStore(tmpLcl, call, call->GetRetLayout(), CHECK_SPILL_ALL);
    return gtNewLclvNode(tmpLcl, info.compRetType);
}

// CEE_LEAVE may be jumping out of a protected block, viz, a catch or a
// finally-protected try. We find the finally blocks protecting the current
// offset (in order) by walking over the complete exception table and
// finding enclosing clauses. This assumes that the table is sorted.
// This will create a series of BBJ_CALLFINALLY -> BBJ_CALLFINALLY ... -> BBJ_ALWAYS.
//
// If we are leaving a catch handler, we need to attach the
// CPX_ENDCATCHes to the correct BBJ_CALLFINALLY blocks.
//
// After this function, the BBJ_LEAVE block has been converted to a different type.
#ifndef FEATURE_EH_FUNCLETS

void Importer::impImportLeave(BasicBlock* block)
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

    impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("impImportLeave"));
    verCurrentState.esStackDepth = 0;

    assert(block->bbJumpKind == BBJ_LEAVE);
    assert(comp->fgBBs == (BasicBlock**)0xCDCD || fgLookupBB(jmpAddr) != NULL); // should be a BB boundary

    BasicBlock* step         = nullptr;
    unsigned    encFinallies = 0; // Number of enclosing finallies.
    GenTree*    endCatches   = nullptr;
    Statement*  endLFinStmt  = nullptr; // The statement tree to indicate the end of locally-invoked finally.

    unsigned  XTnum;
    EHblkDsc* HBtab;

    for (XTnum = 0, HBtab = comp->compHndBBtab; XTnum < comp->compHndBBtabCount; XTnum++, HBtab++)
    {
        // Grab the handler offsets

        IL_OFFSET tryBeg = HBtab->ebdTryBegOffs();
        IL_OFFSET tryEnd = HBtab->ebdTryEndOffs();
        IL_OFFSET hndBeg = HBtab->ebdHndBegOffs();
        IL_OFFSET hndEnd = HBtab->ebdHndEndOffs();

        // Is this a catch-handler we are CEE_LEAVEing out of?
        // If so, we need to call CORINFO_HELP_ENDCATCH.

        if (jitIsBetween(blkAddr, hndBeg, hndEnd) && !jitIsBetween(jmpAddr, hndBeg, hndEnd))
        {
            // Can't CEE_LEAVE out of a finally/fault handler
            if (HBtab->HasFinallyOrFaultHandler())
                BADCODE("leave out of fault/finally block");

            // Create the call to CORINFO_HELP_ENDCATCH
            GenTree* endCatch = gtNewHelperCallNode(CORINFO_HELP_ENDCATCH, TYP_VOID);

            // Make a list of all the currently pending endCatches
            if (endCatches != nullptr)
            {
                endCatches = gtNewCommaNode(endCatches, endCatch);
            }
            else
            {
                endCatches = endCatch;
            }

            JITDUMP("impImportLeave - " FMT_BB " jumping out of catch handler EH#%u, adding call to "
                    "CORINFO_HELP_ENDCATCH\n",
                    block->bbNum, XTnum);
        }
        else if (HBtab->HasFinallyHandler() && jitIsBetween(blkAddr, tryBeg, tryEnd) &&
                 !jitIsBetween(jmpAddr, tryBeg, tryEnd))
        {
            // This is a finally-protected try we are jumping out of
            //
            // If there are any pending endCatches, and we have already
            // jumped out of a finally-protected try, then the endCatches
            // have to be put in a block in an outer try for async
            // exceptions to work correctly.
            // Else, just use append to the original block

            BasicBlock* callBlock;

            assert(!encFinallies ==
                   !endLFinStmt); // if we have finallies, we better have an endLFin tree, and vice-versa

            if (encFinallies == 0)
            {
                assert(step == nullptr);
                callBlock             = block;
                callBlock->bbJumpKind = BBJ_CALLFINALLY; // convert the BBJ_LEAVE to BBJ_CALLFINALLY

                if (endCatches)
                {
                    impSpillNoneAppendTree(endCatches);
                }

                JITDUMP("impImportLeave - jumping out of a finally-protected try, convert block to BBJ_CALLFINALLY "
                        "block %s\n",
                        callBlock->dspToString());
            }
            else
            {
                assert(step != nullptr);

                // Calling the finally block
                callBlock = fgNewBBinRegion(BBJ_CALLFINALLY, XTnum + 1, 0, step);
                assert(step->bbJumpKind == BBJ_ALWAYS);
                step->bbJumpDest = callBlock; // the previous call to a finally returns to this call (to the next
                                              // finally in the chain)
                step->bbJumpDest->bbRefs++;

                // The new block will inherit this block's weight
                callBlock->inheritWeight(block);

                JITDUMP("impImportLeave - jumping out of a finally-protected try, new BBJ_CALLFINALLY block %s\n",
                        callBlock->dspToString());

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
                impSetBlockStmtList(callBlock, endLFinStmt, lastStmt);
            }

            step = fgNewBBafter(BBJ_ALWAYS, callBlock, true);
            // The new block will inherit this block's weight
            step->inheritWeight(block);
            step->bbFlags |= BBF_IMPORTED | BBF_KEEP_BBJ_ALWAYS;

            JITDUMP("impImportLeave - jumping out of a finally-protected try, created step (BBJ_ALWAYS) block %s\n",
                    step->dspToString());

            unsigned finallyNesting = comp->compHndBBtab[XTnum].ebdHandlerNestingLevel;
            assert(finallyNesting <= comp->compHndBBtabCount);

            callBlock->bbJumpDest = HBtab->ebdHndBeg; // This callBlock will call the "finally" handler.
            GenTree* endLFin      = new (comp, GT_END_LFIN) GenTreeEndLFin(finallyNesting);
            endLFinStmt           = gtNewStmt(endLFin);
            endCatches            = NULL;

            encFinallies++;

            invalidatePreds = true;
        }
    }

    // Append any remaining endCatches, if any

    assert(!encFinallies == !endLFinStmt);

    if (encFinallies == 0)
    {
        assert(step == nullptr);
        block->bbJumpKind = BBJ_ALWAYS; // convert the BBJ_LEAVE to a BBJ_ALWAYS

        if (endCatches)
        {
            impSpillNoneAppendTree(endCatches);
        }

        JITDUMP("impImportLeave - no enclosing finally-protected try blocks; convert CEE_LEAVE block to BBJ_ALWAYS "
                "block %s\n",
                block->dspToString());
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

        // The new block will inherit this block's weight
        finalStep->inheritWeight(block);

        JITDUMP("impImportLeave - finalStep block required (encFinallies(%d) > 0), new block %s\n", encFinallies,
                finalStep->dspToString());

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

        impSetBlockStmtList(finalStep, endLFinStmt, lastStmt);

        finalStep->bbJumpDest = leaveTarget; // this is the ultimate destination of the LEAVE

        // Queue up the jump target for importing

        impImportBlockPending(leaveTarget);

        invalidatePreds = true;
    }

    if (invalidatePreds && comp->fgComputePredsDone)
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

void Importer::impImportLeave(BasicBlock* block)
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

    impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("impImportLeave"));
    verCurrentState.esStackDepth = 0;

    assert(block->bbJumpKind == BBJ_LEAVE);
    assert(comp->fgBBs == (BasicBlock**)0xCDCD || fgLookupBB(jmpAddr) != nullptr); // should be a BB boundary

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

    for (XTnum = 0, HBtab = comp->compHndBBtab; XTnum < comp->compHndBBtabCount; XTnum++, HBtab++)
    {
        // Grab the handler offsets

        IL_OFFSET tryBeg = HBtab->ebdTryBegOffs();
        IL_OFFSET tryEnd = HBtab->ebdTryEndOffs();
        IL_OFFSET hndBeg = HBtab->ebdHndBegOffs();
        IL_OFFSET hndEnd = HBtab->ebdHndEndOffs();

        // Is this a catch-handler we are CEE_LEAVEing out of?

        if (jitIsBetween(blkAddr, hndBeg, hndEnd) && !jitIsBetween(jmpAddr, hndBeg, hndEnd))
        {
            // Can't CEE_LEAVE out of a finally/fault handler
            if (HBtab->HasFinallyOrFaultHandler())
            {
                BADCODE("leave out of fault/finally block");
            }

            // We are jumping out of a catch

            if (step == nullptr)
            {
                step             = block;
                step->bbJumpKind = BBJ_EHCATCHRET; // convert the BBJ_LEAVE to BBJ_EHCATCHRET
                stepType         = ST_Catch;

                JITDUMP("impImportLeave - jumping out of a catch (EH#%u), convert block " FMT_BB " to BBJ_EHCATCHRET "
                        "block\n",
                        XTnum, step->bbNum);
            }
            else
            {
                BasicBlock* exitBlock;

                // Create a new catch exit block in the catch region for the existing step block
                // to jump to in this scope
                exitBlock = fgNewBBinRegion(BBJ_EHCATCHRET, 0, XTnum + 1, step);

                assert(step->bbJumpKind == BBJ_ALWAYS || step->bbJumpKind == BBJ_EHCATCHRET);
                step->bbJumpDest = exitBlock; // the previous step (maybe a call to a nested finally, or a nested catch
                                              // exit) returns to this block
                step->bbJumpDest->bbRefs++;

#ifdef TARGET_ARM
                if (stepType == ST_FinallyReturn)
                {
                    assert(step->bbJumpKind == BBJ_ALWAYS);
                    // Mark the target of a finally return
                    step->bbJumpDest->bbFlags |= BBF_FINALLY_TARGET;
                }
#endif

                // The new block will inherit this block's weight
                exitBlock->inheritWeight(block);
                exitBlock->bbFlags |= BBF_IMPORTED;

                // This exit block is the new step
                step     = exitBlock;
                stepType = ST_Catch;

                invalidatePreds = true;

                JITDUMP("impImportLeave - jumping out of a catch (EH#%u), new BBJ_EHCATCHRET block " FMT_BB "\n", XTnum,
                        exitBlock->bbNum);
            }
        }
        else if (HBtab->HasFinallyHandler() && jitIsBetween(blkAddr, tryBeg, tryEnd) &&
                 !jitIsBetween(jmpAddr, tryBeg, tryEnd))
        {
            // We are jumping out of a finally-protected try

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

                // The new block will inherit this block's weight
                callBlock->inheritWeight(block);
                callBlock->bbFlags |= BBF_IMPORTED;

                JITDUMP("impImportLeave - jumping out of a finally-protected try (EH#%u), convert block " FMT_BB " to "
                        "BBJ_ALWAYS, add BBJ_CALLFINALLY block " FMT_BB "\n",
                        XTnum, block->bbNum, callBlock->bbNum);
#else  // !FEATURE_EH_CALLFINALLY_THUNKS
                callBlock             = block;
                callBlock->bbJumpKind = BBJ_CALLFINALLY; // convert the BBJ_LEAVE to BBJ_CALLFINALLY

                JITDUMP("impImportLeave - jumping out of a finally-protected try (EH#%u), convert block " FMT_BB " to "
                        "BBJ_CALLFINALLY block\n",
                        XTnum, callBlock->bbNum);
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
                    step2->inheritWeight(block);
                    step2->bbFlags |= (block->bbFlags & BBF_RUN_RARELY) | BBF_IMPORTED;

                    JITDUMP("impImportLeave - jumping out of a finally-protected try (EH#%u), step block is "
                            "BBJ_EHCATCHRET (" FMT_BB "), new BBJ_ALWAYS step-step block " FMT_BB "\n",
                            XTnum, step->bbNum, step2->bbNum);

                    step = step2;
                    assert(stepType == ST_Catch); // Leave it as catch type for now.
                }
#endif // FEATURE_EH_CALLFINALLY_THUNKS

#if FEATURE_EH_CALLFINALLY_THUNKS
                unsigned callFinallyTryIndex =
                    (HBtab->ebdEnclosingTryIndex == EHblkDsc::NO_ENCLOSING_INDEX) ? 0 : HBtab->ebdEnclosingTryIndex + 1;
                unsigned callFinallyHndIndex =
                    (HBtab->ebdEnclosingHndIndex == EHblkDsc::NO_ENCLOSING_INDEX) ? 0 : HBtab->ebdEnclosingHndIndex + 1;
#else
                unsigned callFinallyTryIndex = XTnum + 1;
                unsigned callFinallyHndIndex = 0; // don't care
#endif

                callBlock        = fgNewBBinRegion(BBJ_CALLFINALLY, callFinallyTryIndex, callFinallyHndIndex, step);
                step->bbJumpDest = callBlock; // the previous call to a finally returns to this call (to the next
                                              // finally in the chain)
                step->bbJumpDest->bbRefs++;

#ifdef TARGET_ARM
                if (stepType == ST_FinallyReturn)
                {
                    assert(step->bbJumpKind == BBJ_ALWAYS);
                    // Mark the target of a finally return
                    step->bbJumpDest->bbFlags |= BBF_FINALLY_TARGET;
                }
#endif

                // The new block will inherit this block's weight
                callBlock->inheritWeight(block);
                callBlock->bbFlags |= BBF_IMPORTED;

                JITDUMP("impImportLeave - jumping out of a finally-protected try (EH#%u), new BBJ_CALLFINALLY "
                        "block " FMT_BB "\n",
                        XTnum, callBlock->bbNum);
            }

            step     = fgNewBBafter(BBJ_ALWAYS, callBlock, true);
            stepType = ST_FinallyReturn;

            // The new block will inherit this block's weight
            step->inheritWeight(block);
            step->bbFlags |= BBF_IMPORTED | BBF_KEEP_BBJ_ALWAYS;

            JITDUMP("impImportLeave - jumping out of a finally-protected try (EH#%u), created step (BBJ_ALWAYS) "
                    "block " FMT_BB "\n",
                    XTnum, step->bbNum);

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

                // Create a new exit block in the try region for the existing step block to jump to in this scope
                catchStep        = fgNewBBinRegion(BBJ_ALWAYS, XTnum + 1, 0, step);
                step->bbJumpDest = catchStep;
                step->bbJumpDest->bbRefs++;

#ifdef TARGET_ARM
                if (stepType == ST_FinallyReturn)
                {
                    // Mark the target of a finally return
                    step->bbJumpDest->bbFlags |= BBF_FINALLY_TARGET;
                }
#endif

                // The new block will inherit this block's weight
                catchStep->inheritWeight(block);
                catchStep->bbFlags |= BBF_IMPORTED;

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

                // This block is the new step
                step     = catchStep;
                stepType = ST_Try;

                invalidatePreds = true;
            }
        }
    }

    if (step == nullptr)
    {
        block->bbJumpKind = BBJ_ALWAYS; // convert the BBJ_LEAVE to a BBJ_ALWAYS

        JITDUMP("impImportLeave - no enclosing finally-protected try blocks or catch handlers; convert CEE_LEAVE "
                "block " FMT_BB " to BBJ_ALWAYS\n",
                block->bbNum);
    }
    else
    {
        step->bbJumpDest = leaveTarget; // this is the ultimate destination of the LEAVE

#ifdef TARGET_ARM
        if (stepType == ST_FinallyReturn)
        {
            assert(step->bbJumpKind == BBJ_ALWAYS);
            // Mark the target of a finally return
            step->bbJumpDest->bbFlags |= BBF_FINALLY_TARGET;
        }
#endif

        JITDUMP("impImportLeave - final destination of step blocks set to " FMT_BB "\n", leaveTarget->bbNum);

        // Queue up the jump target for importing

        impImportBlockPending(leaveTarget);
    }

    if (invalidatePreds && comp->fgComputePredsDone)
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

// This is called when reimporting a leave block. It resets the JumpKind,
// JumpDest, and bbNext to the original values
void Importer::impResetLeaveBlock(BasicBlock* block, IL_OFFSET leaveOffset)
{
#ifdef FEATURE_EH_FUNCLETS
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
        dupBlock->bbWeight = BB_ZERO_WEIGHT;
        dupBlock->bbFlags |= BBF_IMPORTED | BBF_INTERNAL | BBF_RUN_RARELY;

        // Insert the block right after the block which is getting reset so that BBJ_CALLFINALLY and BBJ_ALWAYS
        // will be next to each other.
        fgInsertBBafter(block, dupBlock);

        JITDUMP("New Basic Block " FMT_BB " duplicate of " FMT_BB " created.\n", dupBlock->bbNum, block->bbNum);
    }
#endif // FEATURE_EH_FUNCLETS

    block->bbJumpKind = BBJ_LEAVE;
    fgInitBBLookup();
    block->bbJumpDest = fgLookupBB(leaveOffset);

    // We will leave the BBJ_ALWAYS block we introduced. When it's reimported
    // the BBJ_ALWAYS block will be unreachable, and will be removed after. The
    // reason we don't want to remove the block at this point is that if we call
    // fgInitBBLookup() again we will do it wrong as the BBJ_ALWAYS block won't be
    // added and the linked list length will be different than fgBBcount.
}

// Get the first non-prefix opcode. Used for verification of valid combinations
// of prefixes and actual opcodes.
OPCODE Importer::impGetNonPrefixOpcode(const BYTE* codeAddr, const BYTE* codeEndp)
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

// Checks whether the opcode is a valid opcode for volatile. and unaligned. prefixes
void Importer::impValidateMemoryAccessOpcode(OPCODE opcode, bool volatilePrefix)
{
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

var_types Importer::impGetNumericBinaryOpType(genTreeOps oper, bool fUnsigned, GenTree** pOp1, GenTree** pOp2)
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

            return gtNewCastNode(op, fromUnsigned, TYP_LONG);
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
        assert(op2->TypeIs(TYP_DOUBLE));
        *pOp1 = gtNewCastNode(op1, false, TYP_DOUBLE);
        return TYP_DOUBLE;
    }

    if (op1->TypeIs(TYP_DOUBLE) && !op2->TypeIs(TYP_DOUBLE))
    {
        assert(op2->TypeIs(TYP_FLOAT));
        *pOp2 = gtNewCastNode(op2, false, TYP_DOUBLE);
        return TYP_DOUBLE;
    }

    // int + int = int
    // float + float = float
    // double + double = double

    assert(varActualType(op1->GetType()) == varActualType(op2->GetType()));

    return varActualType(op1->GetType());
}

void Importer::impAddCompareOpImplicitCasts(bool isUnsigned, GenTree*& op1, GenTree*& op2)
{
    if (varTypeIsFloating(op1->GetType()))
    {
        assert(varTypeIsFloating(op2->TypeGet()));

        if (op1->TypeIs(TYP_DOUBLE))
        {
            op2 = gtNewCastNode(op2, false, TYP_DOUBLE);
        }
        else if (op2->TypeIs(TYP_DOUBLE))
        {
            op1 = gtNewCastNode(op1, false, TYP_DOUBLE);
        }
    }
#ifdef TARGET_64BIT
    else if (varTypeIsI(op1->GetType()) && varActualTypeIsInt(op2->GetType()))
    {
        op2 = gtNewCastNode(op2, isUnsigned, TYP_LONG);
    }
    else if (varTypeIsI(op2->GetType()) && varActualTypeIsInt(op1->GetType()))
    {
        op1 = gtNewCastNode(op1, isUnsigned, TYP_LONG);
    }
#endif
}

void Importer::impBranchToNextBlock(BasicBlock* block, GenTree* op1, GenTree* op2)
{
    assert(opts.OptimizationEnabled() && (block->bbJumpDest == block->bbNext));

    block->bbJumpKind = BBJ_NONE;

    if (op1->gtFlags & GTF_GLOB_EFFECT)
    {
        impSpillSideEffects(GTF_SIDE_EFFECT, CHECK_SPILL_ALL DEBUGARG("Branch to next Optimization, op1 side effect"));
        impSpillNoneAppendTree(gtUnusedValNode(op1));
    }
    if (op2->gtFlags & GTF_GLOB_EFFECT)
    {
        impSpillSideEffects(GTF_SIDE_EFFECT, CHECK_SPILL_ALL DEBUGARG("Branch to next Optimization, op2 side effect"));
        impSpillNoneAppendTree(gtUnusedValNode(op2));
    }
}

//------------------------------------------------------------------------
// impOptimizeCastClassOrIsInst: attempt to resolve a cast when jitting
//
// Arguments:
//   op1 - value to cast
//   resolvedToken - resolved token for type to cast to
//   isCastClass - true if this is a castclass, false if isinst
//
// Return Value:
//   tree representing optimized cast, or null if no optimization possible

GenTree* Importer::impOptimizeCastClassOrIsInst(GenTree* op1, CORINFO_RESOLVED_TOKEN* pResolvedToken, bool isCastClass)
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
                if (GenTreeBox* box = op1->IsBox())
                {
                    JITDUMP("Also removing upstream box\n");
                    comp->gtTryRemoveBoxUpstreamEffects(box, Compiler::BR_REMOVE_AND_NARROW);
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
//   resolvedToken - resolved token from the cast operation
//   isCastClass - true if this is castclass, false means isinst
//
// Return Value:
//   Tree representing the cast
//
// Notes:
//   May expand into a series of runtime checks or a helper call.

GenTree* Importer::impCastClassOrIsInstToTree(GenTree*                op1,
                                              GenTree*                op2,
                                              CORINFO_RESOLVED_TOKEN* resolvedToken,
                                              bool                    isCastClass)
{
    assert(op1->TypeIs(TYP_REF));
    assert(op2->TypeIs(TYP_I_IMPL));

    // Optimistically assume the jit should expand this as an inline test
    bool shouldExpandInline = true;

    // Profitability check.
    //
    // Don't bother with inline expansion when jit is trying to
    // generate code quickly, or the cast is in code that won't run very
    // often, or the method already is pretty big.
    if (currentBlock->isRunRarely() || opts.OptimizationDisabled())
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
    const CorInfoHelpFunc helper          = info.compCompHnd->getCastingHelper(resolvedToken, isCastClass);

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
                canExpandInline = impIsClassExact(resolvedToken->hClass);
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
        op2->gtFlags |= GTF_DONT_CSE;

        return gtNewHelperCallNode(helper, TYP_REF, gtNewCallArgs(op2, op1));
    }

    JITDUMP("\nExpanding %s inline\n", isCastClass ? "castclass" : "isinst");

    impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("castclass qmark temp"));

    GenTree* op1Uses[5];
    impMakeMultiUse(op1, 4 + isCastClass, op1Uses, CHECK_SPILL_ALL DEBUGARG("castclass obj temp"));

    GenTree* op2Use = op2;

    if (isCastClass)
    {
        LclVarDsc* lcl = lvaNewTemp(TYP_I_IMPL, true DEBUGARG("castclass class handle temp"));
        lcl->lvIsCSE   = true;

        GenTree* store = comp->gtNewLclStore(lcl, TYP_I_IMPL, op2);

        op2    = gtNewCommaNode(store, comp->gtNewLclLoad(lcl, TYP_I_IMPL), TYP_I_IMPL);
        op2Use = comp->gtNewLclLoad(lcl, TYP_I_IMPL);
    }

    GenTree* condMT    = gtNewOperNode(GT_NE, TYP_INT, gtNewMethodTableLookup(op1Uses[0]), op2);
    GenTree* condNull  = gtNewOperNode(GT_EQ, TYP_INT, op1Uses[1], gtNewIconNode(0, TYP_REF));
    GenTree* condFalse = op1Uses[2];
    GenTree* condTrue;

    if (isCastClass)
    {
        condTrue = gtNewHelperCallNode(CORINFO_HELP_CHKCASTCLASS_SPECIAL, TYP_REF, gtNewCallArgs(op2Use, op1Uses[4]));

        if (impIsClassExact(resolvedToken->hClass))
        {
            // The helper is used only for throwing InvalidCastException in case of casting to an exact class.
            condTrue->AsCall()->gtCallMoreFlags |= GTF_CALL_M_DOES_NOT_RETURN;
        }
    }
    else
    {
        condTrue = gtNewIconNode(0, TYP_REF);
    }

    GenTree* qmarkMT   = gtNewQmarkNode(TYP_REF, condMT, condTrue, condFalse);
    GenTree* qmarkNull = gtNewQmarkNode(TYP_REF, condNull, op1Uses[3], qmarkMT);
    qmarkNull->gtFlags |= GTF_QMARK_CAST_INSTOF;

    LclVarDsc* lcl = lvaNewTemp(TYP_REF, true DEBUGARG("castclass null qmark temp"));
    impSpillNoneAppendTree(comp->gtNewLclStore(lcl, TYP_REF, qmarkNull));

    // TODO-CQ: Is it possible op1 has a better type?
    // See also gtGetHelperCallClassHandle where we make the same
    // determination for the helper call variants.
    assert(!lcl->lvSingleDef);
    lcl->lvSingleDef = true;
    JITDUMP("Marked V%02u as a single def temp\n", lcl->GetLclNum());
    comp->lvaSetClass(lcl, resolvedToken->hClass);

    return gtNewLclvNode(lcl, TYP_REF);
}

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
bool Importer::impBlockIsInALoop(BasicBlock* block)
{
    return (compIsForInlining() && ((impInlineInfo->iciBlock->bbFlags & BBF_BACKWARD_JUMP) != 0)) ||
           ((block->bbFlags & BBF_BACKWARD_JUMP) != 0);
}

#ifdef _PREFAST_
#pragma warning(push)
#pragma warning(disable : 21000) // Suppress PREFast warning about overly large function
#endif
void Importer::impImportBlockCode(BasicBlock* block)
{
    JITDUMP("\nImporting " FMT_BB " (PC=%03u) of '%s'", block->bbNum, block->bbCodeOffs, info.compFullName);

    assert((impStmtList == nullptr) && (impLastStmt == nullptr));

#ifdef FEATURE_SIMD
    m_impSIMDCoalescingBuffer.Clear();
#endif

    unsigned nextStmtIndex = impInitBlockLineInfo(block);

    if (block->bbCatchTyp != 0)
    {
        if ((compStmtOffsetsImplicit & ICorDebugInfo::CALL_SITE_BOUNDARIES) != 0)
        {
            impCurStmtOffsSet(block->bbCodeOffs);
        }

        if (handlerGetsXcptnObj(block->bbCatchTyp))
        {
            SpillCatchArg();
        }
    }

    IL_OFFSET      opcodeOffs    = block->bbCodeOffs;
    IL_OFFSET      lastSpillOffs = opcodeOffs;
    const uint8_t* codeAddr      = info.compCode + opcodeOffs;
    const uint8_t* codeEndp      = info.compCode + block->bbCodeOffsEnd;
    OPCODE         prevOpcode    = CEE_ILLEGAL;

    while (codeAddr < codeEndp)
    {
        if (verCurrentState.esStackDepth != 0)
        {
            // We need to restrict the tree depth as traversal facilities (e.g. GenTreeVisitor)
            // are recursive. We do this by spilling the stack. For simplicity, we don't track
            // the actual tree depth, we track the amount of IL that has been imported while the
            // stack is not empty. This may result in unnecessary spilling - 200 bytes of IL may
            // result in 50 trees, each with a depth of just 3.

            if ((opcodeOffs - lastSpillOffs > MAX_TREE_SIZE) && impCanSpillNow(prevOpcode))
            {
                EnsureStackSpilled(true DEBUGARG("deep tree spill"));
                lastSpillOffs = opcodeOffs;
            }
        }
        else
        {
            lastSpillOffs   = opcodeOffs;
            impBoxTempInUse = false; // The stack is empty now, the box temp can be reused.
        }

        if (compDonotInline())
        {
            return;
        }

        opcodeOffs = static_cast<IL_OFFSET>(codeAddr - info.compCode);

        if ((nextStmtIndex < compStmtOffsetsCount) && (opcodeOffs >= compStmtOffsets[nextStmtIndex]))
        {
            nextStmtIndex = AdvanceStmtOffset(nextStmtIndex, opcodeOffs);
        }
        else if (compStmtOffsetsImplicit != ICorDebugInfo::NO_BOUNDARIES)
        {
            if ((compStmtOffsetsImplicit & ICorDebugInfo::STACK_EMPTY_BOUNDARIES) &&
                (verCurrentState.esStackDepth == 0))
            {
                impCurStmtOffsSet(opcodeOffs);
            }
            else if ((compStmtOffsetsImplicit & ICorDebugInfo::CALL_SITE_BOUNDARIES) &&
                     impOpcodeIsCallSiteBoundary(prevOpcode))
            {
                if (opts.compDbgCode)
                {
                    EnsureStackSpilled(false DEBUGARG("debug info spill"));
                    impCurStmtOffsSet(opcodeOffs);
                }
                else if (verCurrentState.esStackDepth == 0)
                {
                    impCurStmtOffsSet(opcodeOffs);
                }
            }
            else if ((compStmtOffsetsImplicit & ICorDebugInfo::NOP_BOUNDARIES) && (prevOpcode == CEE_NOP))
            {
                if (opts.compDbgCode)
                {
                    EnsureStackSpilled(false DEBUGARG("debug info spill"));
                }

                impCurStmtOffsSet(opcodeOffs);
            }
        }

        JITDUMP("\n    [%2u] %3u (0x%03x) ", verCurrentState.esStackDepth, opcodeOffs, opcodeOffs);

        int    prefixFlags = 0;
        OPCODE opcode      = static_cast<OPCODE>(*codeAddr++);

    DECODE_OPCODE:
        DBEXEC(verbose && (opcode != CEE_PREFIX1), printf("%s", opcodeNames[opcode]))

        unsigned opcodeSize = opcodeSizes[opcode];

        switch (opcode)
        {
            unsigned               ilLocNum;
            unsigned               ilArgNum;
            LclVarDsc*             lcl;
            var_types              lclTyp;
            var_types              type;
            GenTree*               op1;
            GenTree*               op2;
            GenTree*               op3;
            genTreeOps             oper;
            int                    val;
            bool                   uns;
            bool                   ovfl;
            bool                   isLocal;
            CORINFO_CLASS_HANDLE   clsHnd;
            CORINFO_RESOLVED_TOKEN resolvedToken;
            CORINFO_RESOLVED_TOKEN constrainedResolvedToken;
            CORINFO_FIELD_INFO     fieldInfo;

            case CEE_PREFIX1:
                opcodeOffs = static_cast<IL_OFFSET>(codeAddr - info.compCode);
                opcode     = static_cast<OPCODE>(*codeAddr++ + 256);
                goto DECODE_OPCODE;

            case CEE_UNALIGNED:
                val = *codeAddr++;
                JITDUMP(" %u", val);

                if ((val != 1) && (val != 2) && (val != 4))
                {
                    BADCODE("Alignment unaligned. must be 1, 2, or 4");
                }

                impValidateMemoryAccessOpcode(impGetNonPrefixOpcode(codeAddr, codeEndp), false);

                if ((prefixFlags & PREFIX_UNALIGNED) != 0)
                {
                    BADCODE("Multiple unaligned. prefixes");
                }

                prefixFlags |= PREFIX_UNALIGNED;
                goto PREFIX;

            case CEE_VOLATILE:
                impValidateMemoryAccessOpcode(impGetNonPrefixOpcode(codeAddr, codeEndp), true);

                if ((prefixFlags & PREFIX_VOLATILE) != 0)
                {
                    BADCODE("Multiple volatile. prefixes");
                }

                prefixFlags |= PREFIX_VOLATILE;
                goto PREFIX;

            case CEE_READONLY:
                JITDUMP(" readonly.");

                opcode = impGetNonPrefixOpcode(codeAddr, codeEndp);
                if ((opcode != CEE_LDELEMA) && !impOpcodeIsCallOpcode(opcode))
                {
                    BADCODE("readonly. has to be followed by ldelema or call");
                }

                if ((prefixFlags & PREFIX_READONLY) != 0)
                {
                    BADCODE("Multiple readonly. prefixes");
                }

                prefixFlags |= PREFIX_READONLY;
                goto PREFIX;

            case CEE_TAILCALL:
                JITDUMP(" tail.");

                if (!impOpcodeIsCallOpcode(impGetNonPrefixOpcode(codeAddr, codeEndp)))
                {
                    BADCODE("tailcall. has to be followed by call, callvirt or calli");
                }

                if ((prefixFlags & PREFIX_TAILCALL_EXPLICIT) != 0)
                {
                    BADCODE("Multiple tailcall. prefixes");
                }

                prefixFlags |= PREFIX_TAILCALL_EXPLICIT;
                goto PREFIX;

            case CEE_CONSTRAINED:
                impResolveToken(codeAddr, &constrainedResolvedToken, CORINFO_TOKENKIND_Constrained);
                codeAddr += 4;
                JITDUMP(" (%08X) ", constrainedResolvedToken.token);

                opcode = impGetNonPrefixOpcode(codeAddr, codeEndp);
                if ((opcode != CEE_CALLVIRT) && (opcode != CEE_CALL) && (opcode != CEE_LDFTN))
                {
                    BADCODE("constrained. has to be followed by callvirt, call or ldftn");
                }

                if ((prefixFlags & PREFIX_CONSTRAINED) != 0)
                {
                    BADCODE("Multiple constrained. prefixes");
                }

                prefixFlags |= PREFIX_CONSTRAINED;
            PREFIX:
                opcodeOffs = static_cast<IL_OFFSET>(codeAddr - info.compCode);
                opcode     = static_cast<OPCODE>(*codeAddr++);
                goto DECODE_OPCODE;

            case CEE_LDNULL:
                impPushOnStack(gtNewIconNode(0, TYP_REF));
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
                val = (opcode - CEE_LDC_I4_0);
                assert(-1 <= val && val <= 8);
                goto PUSH_I4CON;
            case CEE_LDC_I4_S:
                val = getI1LittleEndian(codeAddr);
                goto PUSH_I4CON;
            case CEE_LDC_I4:
                val = getI4LittleEndian(codeAddr);
                goto PUSH_I4CON;
            PUSH_I4CON:
                JITDUMP(" %d", val);
                impPushOnStack(gtNewIconNode(val));
                break;

            case CEE_LDC_I8:
            {
                int64_t value = getI8LittleEndian(codeAddr);
                JITDUMP(" 0x%016llx", value);
                impPushOnStack(gtNewLconNode(value));
                break;
            }

            case CEE_LDC_R8:
            {
                double value = getR8LittleEndian(codeAddr);
                JITDUMP(" %#.17g", value);
                impPushOnStack(gtNewDconNode(value, TYP_DOUBLE));
                break;
            }

            case CEE_LDC_R4:
            {
                float value = getR4LittleEndian(codeAddr);
                JITDUMP(" %#.17g", value);
                impPushOnStack(gtNewDconNode(value, TYP_FLOAT));
                break;
            }

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
                impPushOnStack(gtNewSconNode(info.compScopeHnd, static_cast<mdToken>(val)));
                break;

            case CEE_LDARG:
                ilArgNum = getU2LittleEndian(codeAddr);
                JITDUMP(" %u", ilArgNum);
                impLoadArg(ilArgNum);
                break;

            case CEE_LDARG_S:
                ilArgNum = getU1LittleEndian(codeAddr);
                JITDUMP(" %u", ilArgNum);
                impLoadArg(ilArgNum);
                break;

            case CEE_LDARG_0:
            case CEE_LDARG_1:
            case CEE_LDARG_2:
            case CEE_LDARG_3:
                ilArgNum = opcode - CEE_LDARG_0;
                impLoadArg(ilArgNum);
                break;

            case CEE_LDLOC:
                ilLocNum = getU2LittleEndian(codeAddr);
                JITDUMP(" %u", ilLocNum);
                impLoadLoc(ilLocNum);
                break;

            case CEE_LDLOC_S:
                ilLocNum = getU1LittleEndian(codeAddr);
                JITDUMP(" %u", ilLocNum);
                impLoadLoc(ilLocNum);
                break;

            case CEE_LDLOC_0:
            case CEE_LDLOC_1:
            case CEE_LDLOC_2:
            case CEE_LDLOC_3:
                ilLocNum = opcode - CEE_LDLOC_0;
                assert((0 <= ilLocNum) && (ilLocNum < 4));
                impLoadLoc(ilLocNum);
                break;

            case CEE_STARG:
                ilArgNum = getU2LittleEndian(codeAddr);
                goto STARG;
            case CEE_STARG_S:
                ilArgNum = getU1LittleEndian(codeAddr);
            STARG:
                JITDUMP(" %u", ilArgNum);

                if (compIsForInlining())
                {
                    if (ilArgNum >= impInlineInfo->ilArgCount)
                    {
                        compInlineResult->NoteFatal(InlineObservation::CALLEE_BAD_ARGUMENT_NUMBER);
                        return;
                    }

                    op1 = inlUseArg(impInlineInfo, ilArgNum);
                    noway_assert(op1->OperIs(GT_LCL_VAR));
                    lcl = op1->AsLclVar()->GetLcl();
                }
                else
                {
                    if (ilArgNum >= info.GetILArgCount())
                    {
                        BADCODE("Bad IL arg num");
                    }

                    unsigned lclNum = comp->lvaMapILArgNumToLclNum(ilArgNum);

                    if (lclNum == info.GetThisParamLclNum())
                    {
                        lclNum = comp->lvaThisLclNum;
                    }

                    lcl = comp->lvaGetDesc(lclNum);
                    assert(lcl->lvHasILStoreOp);
                }

                isLocal = false;
                goto STLCL;

            case CEE_STLOC:
                ilLocNum = getU2LittleEndian(codeAddr);
                JITDUMP(" %u", ilLocNum);
                goto STLOC;
            case CEE_STLOC_S:
                ilLocNum = getU1LittleEndian(codeAddr);
                JITDUMP(" %u", ilLocNum);
                goto STLOC;
            case CEE_STLOC_0:
            case CEE_STLOC_1:
            case CEE_STLOC_2:
            case CEE_STLOC_3:
                ilLocNum = opcode - CEE_STLOC_0;
            STLOC:
                isLocal = true;

                if (compIsForInlining())
                {
                    if (ilLocNum >= impInlineInfo->ilLocCount)
                    {
                        impInlineInfo->inlineResult->NoteFatal(InlineObservation::CALLEE_BAD_LOCAL_NUMBER);
                        return;
                    }

                    lcl    = inlGetInlineeLocal(impInlineInfo, ilLocNum);
                    lclTyp = lcl->GetType();
                }
                else
                {
                    if (ilLocNum >= info.GetILLocCount())
                    {
                        BADCODE("Bad IL loc num");
                    }

                    lcl = comp->lvaGetDesc(info.GetParamCount() + ilLocNum);

                STLCL:
                    lclTyp = lcl->GetType();

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

                // Filter out simple assignments to itself
                if (op1->OperIs(GT_LCL_VAR) && (op1->AsLclVar()->GetLcl() == lcl))
                {
                    if (opts.compDbgCode)
                    {
                        impSpillAllAppendTree(gtNewNothingNode());
                    }

                    break;
                }

                if (verCurrentState.esStackDepth > 0)
                {
                    GenTreeFlags spillSideEffects = GTF_EMPTY;

                    if (lcl->IsPinning())
                    {
                        // Don't move anything past pinning stores, it's potentially unsafe.
                        // Load/stores and calls obviously can't be moved, as they depend on
                        // native pointers within the pinned object. Pure expressions that
                        // don't depend on such native pointers can be moved but it's not
                        // easy to detect those. Pure expressions that do depend on native
                        // pointers should be safe but it's not clear if there's aren't odd
                        // cases (e.g. expressions computing an intermediary address outside
                        // of the pinned object).
                        //
                        // TODO-MIKE-Review: Is there anything that prevents other JIT
                        // transforms doing this kind of code motion? Pinning locals aren't
                        // included in liveness but they're not address exposed so it's not
                        // clear what would prevent a load/store from moving, probably just
                        // the fact that the JIT doesn't do such optimizations.
                        // CSE is one interesting case. Every time a managed pointer is
                        // converted to a native pointer we basically obtain a NEW value,
                        // since the managed object might have moved and the native pointer
                        // is not updated. Interestingly, the C# generates code that gets
                        // the native pointer out of the pinning local, which is ignored by
                        // SSA and always gets a new VN so any CSEs issues are avoided.
                        // But the IL spec doesn't seem to say anywhere that you transform
                        // the original managed pointer into a native pointer using conv.u.
                        //
                        // TODO-MIKE-Review: It also looks like there's a bug in Roslyn,
                        // see pin-roslyn-bug.cs. There's probably nothing the JIT can do
                        // to avoid that.

                        EnsureStackSpilled(true DEBUGARG("Pinning store"));
                    }
                    else if (lcl->lvHasLdAddrOp)
                    {
                        spillSideEffects = GTF_GLOB_EFFECT;
                    }
                    else
                    {
                        GenTreeFlags valueSideEffects = op1->GetSideEffects() & GTF_GLOB_EFFECT;

                        if ((valueSideEffects & (GTF_CALL | GTF_ASG)) != 0)
                        {
                            spillSideEffects = GTF_GLOB_EFFECT;
                        }
                        else if (((valueSideEffects & GTF_GLOB_EFFECT) != 0) || impHasAddressTakenLocals(op1))
                        {
                            spillSideEffects = GTF_SIDE_EFFECT;
                        }

                        impSpillLclReferences(lcl);
                    }

                    if (spillSideEffects != GTF_EMPTY)
                    {
                        impSpillSideEffects(spillSideEffects, CHECK_SPILL_ALL DEBUGARG("STLOC stack spill temp"));
                    }
                }

                // Create and append the store statement

                if (varTypeIsStruct(lclTyp))
                {
                    op1 = impAssignStruct(gtNewLclvNode(lcl, lclTyp), op1, CHECK_SPILL_ALL);
                }
                else
                {
                    op1 = impImplicitIorI4Cast(op1, lclTyp);

#ifdef TARGET_64BIT
                    // Downcast the TYP_I_IMPL into a 32-bit Int for x86 JIT compatiblity
                    if (varTypeIsI(op1->GetType()) && (varActualType(lclTyp) == TYP_INT))
                    {
                        op1 = gtNewCastNode(op1, false, TYP_INT);
                    }
#endif

                    // TODO-MIKE-Review: This should be BADCODE.
                    assert((varActualType(lclTyp) == varActualType(op1->GetType())) ||
                           ((lclTyp == TYP_I_IMPL) && op1->TypeIs(TYP_BYREF, TYP_REF)) ||
                           ((lclTyp == TYP_BYREF) && op1->TypeIs(TYP_I_IMPL)) ||
                           ((lclTyp == TYP_BYREF) && op1->TypeIs(TYP_REF)) ||
                           (varTypeIsFloating(lclTyp) && varTypeIsFloating(op1->GetType())));

                    if (lclTyp == TYP_I_IMPL)
                    {
                        // When "&var" is created, we assume it is a byref. If it is being assigned
                        // to a TYP_I_IMPL var, change the type to prevent unnecessary GC info.
                        if (op1->TypeIs(TYP_BYREF) && impIsLocalAddrExpr(op1))
                        {
                            op1->SetType(TYP_I_IMPL);
                        }
                    }
                    else if (lclTyp == TYP_REF)
                    {
                        // If this is a local and the local is a ref type, see
                        // if we can improve type information based on the
                        // value being assigned.
                        if (isLocal)
                        {
                            // We should have seen a stloc in our IL prescan.
                            assert(lcl->lvHasILStoreOp);

                            // Is there just one place this local is defined?
                            const bool isSingleDefLocal = lcl->lvSingleDef;

                            // TODO-MIKE-Cleanup: This check is probably no longer needed. It used to be the case
                            // that ref class handles were propagated from predecessors without merging, resulting
                            // in incorrect devirtualization.

                            // Conservative check that there is just one
                            // definition that reaches this store.
                            const bool hasSingleReachingDef = (block->bbStackDepthOnEntry() == 0);

                            if (isSingleDefLocal && hasSingleReachingDef)
                            {
                                comp->lvaUpdateClass(lcl, op1, clsHnd);
                            }
                        }
                    }
                    else if (varTypeIsFloating(lclTyp) && varTypeIsFloating(op1->GetType()) &&
                             (lclTyp != op1->GetType()))
                    {
                        op1 = gtNewCastNode(op1, false, lclTyp);
                    }

                    op1 = comp->gtNewLclStore(lcl, lclTyp, op1);
                }

                impSpillNoneAppendTree(op1);
                break;

            case CEE_LDLOCA:
                ilLocNum = getU2LittleEndian(codeAddr);
                goto LDLOCA;
            case CEE_LDLOCA_S:
                ilLocNum = getU1LittleEndian(codeAddr);
            LDLOCA:
                JITDUMP(" %u", ilLocNum);

                if (compIsForInlining())
                {
                    if (ilLocNum >= impInlineInfo->ilLocCount)
                    {
                        compInlineResult->NoteFatal(InlineObservation::CALLEE_BAD_LOCAL_NUMBER);
                        return;
                    }

                    LclVarDsc* lcl = inlGetInlineeLocal(impInlineInfo, ilLocNum);

                    op1 = gtNewLclVarAddrNode(lcl, TYP_BYREF);
                    goto PUSH_ADRVAR;
                }

                if (ilLocNum >= info.GetILLocCount())
                {
                    BADCODE("Bad IL loc num");
                }

                lcl = comp->lvaGetDesc(info.GetParamCount() + ilLocNum);
                goto ADRVAR;

            case CEE_LDARGA:
                ilArgNum = getU2LittleEndian(codeAddr);
                goto LDARGA;
            case CEE_LDARGA_S:
                ilArgNum = getU1LittleEndian(codeAddr);
            LDARGA:
                JITDUMP(" %u", ilArgNum);

                if (compIsForInlining())
                {
                    if (ilArgNum >= impInlineInfo->ilArgCount)
                    {
                        compInlineResult->NoteFatal(InlineObservation::CALLEE_BAD_ARGUMENT_NUMBER);
                        return;
                    }

                    // TODO-MIKE-Cleanup: It would make more sense to have inlUseParamAddr
                    // instead of getting a LCL_VAR and changing it to LCL_ADDR.
                    op1 = inlUseArg(impInlineInfo, ilArgNum);
                    noway_assert(op1->OperIs(GT_LCL_VAR));
                    op1 = op1->ChangeToLclAddr(TYP_BYREF, op1->AsLclVar()->GetLcl());

                    goto PUSH_ADRVAR;
                }

                if (ilArgNum >= info.GetILArgCount())
                {
                    BADCODE("Bad IL arg num");
                }

                {
                    unsigned lclNum = comp->lvaMapILArgNumToLclNum(ilArgNum);

                    if (lclNum == info.GetThisParamLclNum())
                    {
                        lclNum = comp->lvaThisLclNum;
                    }

                    lcl = comp->lvaGetDesc(lclNum);
                }

            ADRVAR:
                op1 = gtNewLclVarAddrNode(lcl, TYP_BYREF);

            PUSH_ADRVAR:
                assert(op1->AsLclAddr()->GetLclOffs() == 0);
                impPushOnStack(op1);
                break;

            case CEE_ARGLIST:
                ImportArgList();
                break;

            case CEE_ENDFINALLY:
                if (compIsForInlining())
                {
                    assert(!"Shouldn't have exception handlers in the inliner!");
                    compInlineResult->NoteFatal(InlineObservation::CALLEE_HAS_ENDFINALLY);
                    return;
                }

                if (info.compXcptnsCount == 0)
                {
                    BADCODE("endfinally outside finally");
                }

                if (verCurrentState.esStackDepth > 0)
                {
                    impSpillSideEffects(GTF_SIDE_EFFECT, CHECK_SPILL_ALL DEBUGARG("endfinally"));
                    verCurrentState.esStackDepth = 0;
                }

                impSpillNoneAppendTree(gtNewOperNode(GT_RETFILT, TYP_VOID, nullptr));
                break;

            case CEE_ENDFILTER:
                if (compIsForInlining())
                {
                    assert(!"Shouldn't have exception handlers in the inliner!");
                    compInlineResult->NoteFatal(InlineObservation::CALLEE_HAS_ENDFILTER);
                    return;
                }

                if (info.compXcptnsCount == 0)
                {
                    BADCODE("endfilter outside filter");
                }

                if (!bbInFilterILRange(block))
                {
                    BADCODE("EndFilter outside a filter handler");
                }

                assert(block->bbFlags & BBF_DONT_REMOVE);
                assert(block->bbJumpKind == BBJ_EHFILTERRET);

                if (verCurrentState.esStackDepth != 1)
                {
                    BADCODE("stack must be 1 on end of filter");
                }

                block->bbSetRunRarely(); // filters are rare

                op1 = impPopStack().val;

                if (varActualType(op1->GetType()) != TYP_INT)
                {
                    // TODO-MIKE-Review: This should be BADCODE. Old code only asserted and
                    // there's a pretty good chance that LONG values worked fine by accident.
                    op1 = gtNewCastNode(op1, false, TYP_INT);
                    assert(!"Bad endfilter value type");
                }

                impSpillNoneAppendTree(gtNewOperNode(GT_RETFILT, TYP_INT, op1));
                break;

            case CEE_RET:
                if (compIsForInlining())
                {
                    if (!impInlineReturnInstruction())
                    {
                        return;
                    }
                }
                else
                {
                    impReturnInstruction();
                }
                break;

            case CEE_JMP:
                ImportJmp(codeAddr, block);
                break;

            case CEE_LDELEMA:
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);
                clsHnd = resolvedToken.hClass;

                // If it's a value class array we just do a simple address-of
                if (info.compCompHnd->isValueClass(resolvedToken.hClass))
                {
                    lclTyp = JITtype2varType(info.compCompHnd->asCorInfoType(clsHnd));
                    goto LDELEM;
                }

                // Similarly, if its a readonly access, we can do a simple address-of
                // without doing a runtime type-check
                if ((prefixFlags & PREFIX_READONLY) != 0)
                {
                    lclTyp = TYP_REF;
                    goto LDELEM;
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

                impPushOnStack(op1);
                break;

            case CEE_LDELEM:
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);
                clsHnd = resolvedToken.hClass;

                if (info.compCompHnd->isValueClass(resolvedToken.hClass))
                {
                    lclTyp = JITtype2varType(info.compCompHnd->asCorInfoType(clsHnd));
                    goto LDELEM;
                }

                opcode = CEE_LDELEM_REF;
                FALLTHROUGH;
            case CEE_LDELEM_REF:
                lclTyp = TYP_REF;
                goto LDELEM_T;
            case CEE_LDELEM_I1:
                lclTyp = TYP_BYTE;
                goto LDELEM_T;
            case CEE_LDELEM_I2:
                lclTyp = TYP_SHORT;
                goto LDELEM_T;
            case CEE_LDELEM_I:
                lclTyp = TYP_I_IMPL;
                goto LDELEM_T;
            case CEE_LDELEM_U4:
                lclTyp = TYP_INT;
                goto LDELEM_T;
            case CEE_LDELEM_I4:
                lclTyp = TYP_INT;
                goto LDELEM_T;
            case CEE_LDELEM_I8:
                lclTyp = TYP_LONG;
                goto LDELEM_T;
            case CEE_LDELEM_R4:
                lclTyp = TYP_FLOAT;
                goto LDELEM_T;
            case CEE_LDELEM_R8:
                lclTyp = TYP_DOUBLE;
                goto LDELEM_T;
            case CEE_LDELEM_U1:
                lclTyp = TYP_UBYTE;
                goto LDELEM_T;
            case CEE_LDELEM_U2:
                lclTyp = TYP_USHORT;
            LDELEM_T:
                clsHnd = NO_CLASS_HANDLE;
            LDELEM:
                op2 = impPopStack().val; // Index
                op1 = impPopStack().val; // Array reference

                // TODO-MIKE-Review: This should be BADCODE.
                assert(op1->TypeIs(TYP_REF));

                // Check for null pointer - in the inliner case we simply abort

                if (compIsForInlining())
                {
                    if (op1->gtOper == GT_CNS_INT)
                    {
                        compInlineResult->NoteFatal(InlineObservation::CALLEE_HAS_NULL_FOR_LDELEM);
                        return;
                    }
                }

                op1 = impCheckForNullPointer(op1);
                op1 = gtNewArrayIndexAddr(op1, op2, lclTyp);

                if (lclTyp == TYP_STRUCT)
                {
                    assert((opcode == CEE_LDELEM) || (opcode == CEE_LDELEMA));

                    unsigned     layoutNum = typGetObjLayoutNum(clsHnd);
                    ClassLayout* layout    = typGetLayoutByNum(layoutNum);

                    op1->AsIndexAddr()->SetElemSize(layout->GetSize());
                    op1->AsIndexAddr()->SetElemTypeNum(layoutNum);
                }

                if (opcode != CEE_LDELEMA)
                {
                    op1 = gtNewIndexIndir(lclTyp, op1->AsIndexAddr());

                    if (varTypeUsesFloatReg(op1->GetType()))
                    {
                        comp->compFloatingPointUsed = true;
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
                    impPushOnStack(op1);
                }
                break;

            // stelem for reference and value types
            case CEE_STELEM:
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);
                clsHnd = resolvedToken.hClass;

                if (info.compCompHnd->isValueClass(clsHnd))
                {
                    lclTyp = JITtype2varType(info.compCompHnd->asCorInfoType(clsHnd));
                    goto STELEM;
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
                        goto STELEM_T;
                    }

                    // Else call a helper function to do the store
                }

                {
                    GenTree* value = impPopStack().val;
                    GenTree* index = impPopStack().val;
                    GenTree* array = impPopStack().val;

                    // TODO-MIKE-Review: This should be BADCODE.
                    assert(array->TypeIs(TYP_REF));
                    assert(value->TypeIs(TYP_REF));
                    assert(varTypeIsIntegral(index->GetType()));

                    impSpillAllAppendTree(
                        gtNewHelperCallNode(CORINFO_HELP_ARRADDR_ST, TYP_VOID, gtNewCallArgs(array, index, value)));
                }
                break;

            case CEE_STELEM_I1:
                lclTyp = TYP_BYTE;
                goto STELEM_T;
            case CEE_STELEM_I2:
                lclTyp = TYP_SHORT;
                goto STELEM_T;
            case CEE_STELEM_I:
                lclTyp = TYP_I_IMPL;
                goto STELEM_T;
            case CEE_STELEM_I4:
                lclTyp = TYP_INT;
                goto STELEM_T;
            case CEE_STELEM_I8:
                lclTyp = TYP_LONG;
                goto STELEM_T;
            case CEE_STELEM_R4:
                lclTyp = TYP_FLOAT;
                goto STELEM_T;
            case CEE_STELEM_R8:
                lclTyp = TYP_DOUBLE;
            STELEM_T:
                clsHnd = NO_CLASS_HANDLE;
            STELEM:
                // We need to evaluate array, index, value and then perform a range check.
                // However, the IR we build is STOREIND(INDEX_ADDR(array, index)), value),
                // with INDEX_ADDR performing the range check, before value is evaluated.
                // We don't have much of a choice but to spill the stack to ensure correct
                // side effect ordering.

                if ((impStackTop().val->gtFlags & GTF_SIDE_EFFECT) != 0)
                {
                    impSpillSideEffects(GTF_SIDE_EFFECT, CHECK_SPILL_ALL DEBUGARG("STELEM ordering spill temp"));
                }

                op2 = impPopStack().val; // value
                op1 = impPopStack().val; // index
                op3 = impPopStack().val; // array

                // TODO-MIKE-Review: This should be BADCODE.
                assert(op3->TypeIs(TYP_REF));

                op3 = impCheckForNullPointer(op3);
                op1 = gtNewArrayIndexAddr(op3, op1, lclTyp);

                if (lclTyp == TYP_STRUCT)
                {
                    unsigned     layoutNum = typGetObjLayoutNum(clsHnd);
                    ClassLayout* layout    = typGetLayoutByNum(layoutNum);

                    op1->AsIndexAddr()->SetElemSize(layout->GetSize());
                    op1->AsIndexAddr()->SetElemTypeNum(layoutNum);

                    op1 = gtNewIndexIndir(lclTyp, op1->AsIndexAddr());
                    op1 = impAssignStruct(op1, op2, CHECK_SPILL_ALL);
                }
                else
                {
                    op2 = impImplicitR4orR8Cast(op2, lclTyp);
                    op2 = impImplicitIorI4Cast(op2, lclTyp);
                    op1 = gtNewIndexIndStore(lclTyp, op1->AsIndexAddr(), op2);
                }

                if (varTypeUsesFloatReg(op1->GetType()))
                {
                    comp->compFloatingPointUsed = true;
                }

                impSpillAllAppendTree(op1);
                break;

            case CEE_ADD_OVF:
                uns = false;
                goto ADD_OVF;
            case CEE_ADD_OVF_UN:
                uns = true;
            ADD_OVF:
                ovfl = true;
                oper = GT_ADD;
                goto MATH_OP2;

            case CEE_SUB_OVF:
                uns = false;
                goto SUB_OVF;
            case CEE_SUB_OVF_UN:
                uns = true;
            SUB_OVF:
                ovfl = true;
                oper = GT_SUB;
                goto MATH_OP2;

            case CEE_MUL_OVF:
                uns = false;
                goto MUL_OVF;
            case CEE_MUL_OVF_UN:
                uns = true;
            MUL_OVF:
                ovfl = true;
                oper = GT_MUL;
                goto MATH_OP2;

            case CEE_ADD:
                oper = GT_ADD;
                goto MATH_OP2_NO_OVF;
            case CEE_SUB:
                oper = GT_SUB;
                goto MATH_OP2_NO_OVF;
            case CEE_MUL:
                oper = GT_MUL;
                goto MATH_OP2_NO_OVF;
            case CEE_DIV:
                oper = GT_DIV;
                goto MATH_OP2_NO_OVF;
            case CEE_DIV_UN:
                oper = GT_UDIV;
                goto MATH_OP2_NO_OVF;
            case CEE_REM:
                oper = GT_MOD;
                goto MATH_OP2_NO_OVF;
            case CEE_REM_UN:
                oper = GT_UMOD;
                goto MATH_OP2_NO_OVF;
            case CEE_AND:
                oper = GT_AND;
                goto MATH_OP2_NO_OVF;
            case CEE_OR:
                oper = GT_OR;
                goto MATH_OP2_NO_OVF;
            case CEE_XOR:
                oper = GT_XOR;
            MATH_OP2_NO_OVF:
                ovfl = false;
                uns  = false;

            MATH_OP2:
                op2 = impPopStack().val;
                op1 = impPopStack().val;

                type = impGetNumericBinaryOpType(oper, uns, &op1, &op2);

                if (varTypeIsFloating(type))
                {
                    oper = static_cast<genTreeOps>(oper - (GT_ADD - GT_FADD));

                    op1 = new (comp, oper == GT_FMOD ? GT_CALL : oper)
                        GenTreeOp(oper, type, op1, op2 DEBUGARG(/*largeNode*/ true));
                }
                else if ((op2->IsIntegralConst(0) && (oper == GT_ADD || oper == GT_SUB)) ||
                         (op2->IsIntegralConst(1) && (oper == GT_MUL || oper == GT_DIV)))
                {
                    // just push op1
                }
                else
                {
                    if ((oper == GT_MUL) && !ovfl && ((type == TYP_INT) || (type == TYP_LONG)))
                    {
                        GenTreeIntCon* i1 = op1->IsIntCon();
                        GenTreeIntCon* i2 = op2->IsIntCon();

                        // In general IL generated by Rosly (and hopefully any other sane compiler)
                        // doesn't generate constant expressions. However, it does happen sometimes,
                        // due to the use of sizeof or when stackalloc is used, for unclear reasons.
                        // Fold it now to avoid interfering with local address expression recognition.

                        if ((i1 != nullptr) && (i2 != nullptr))
                        {
                            assert(i1->TypeIs(TYP_INT, TYP_LONG));
                            assert(i2->TypeIs(TYP_INT, TYP_LONG));

                            i1->SetValue(type, i1->GetValue() * i2->GetValue());

                            impPushOnStack(i1);
                            break;
                        }
                    }

#ifndef TARGET_64BIT
                    if ((type == TYP_LONG) && ((oper == GT_MUL) || (oper == GT_DIV) || (oper == GT_UDIV) ||
                                               (oper == GT_MOD) || (oper == GT_UMOD)))
                    {
                        // LONG multiplication/division usually requires helper calls on 32 bit targets.
                        op1 = new (comp, GT_CALL) GenTreeOp(oper, type, op1, op2 DEBUGARG(/*largeNode*/ true));
                    }
                    else
#endif
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
                    else if (op1->OperIs(GT_DIV, GT_UDIV, GT_MOD, GT_UMOD))
                    {
                        if (op1->DivModMayThrow(comp))
                        {
                            op1->gtFlags |= GTF_EXCEPT;
                        }
                    }
                    else
                    {
                        assert(!op1->OperMayThrow(comp));
                    }
                }

                impPushOnStack(op1);
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
                impPushOnStack(op1);
                break;

            case CEE_NOT:
                op1 = impPopStack().val;
                impBashVarAddrsToI(op1, nullptr);
                type = genActualType(op1->TypeGet());
                impPushOnStack(gtNewOperNode(GT_NOT, type, op1));
                break;

            case CEE_CKFINITE:
                op1  = impPopStack().val;
                type = op1->TypeGet();
                op1  = gtNewOperNode(GT_CKFINITE, type, op1);
                op1->gtFlags |= GTF_EXCEPT;
                impPushOnStack(op1);
                break;

            case CEE_LEAVE:
                val = 4 + getI4LittleEndian(codeAddr);
                goto LEAVE;
            case CEE_LEAVE_S:
                val = 1 + getI1LittleEndian(codeAddr);
            LEAVE:
                if (compIsForInlining())
                {
                    compInlineResult->NoteFatal(InlineObservation::CALLEE_HAS_LEAVE);
                    return;
                }

                {
                    IL_OFFSET offset = static_cast<IL_OFFSET>((codeAddr - info.compCode) + val);
                    JITDUMP(" %04X", offset);

                    if (block->bbJumpKind != BBJ_LEAVE)
                    {
                        impResetLeaveBlock(block, offset);
                    }

                    assert(offset == block->bbJumpDest->bbCodeOffs);
                }

                impImportLeave(block);
                impNoteBranchOffs();
                break;

            case CEE_BR:
            case CEE_BR_S:
                if (compIsForInlining() &&
                    ((opcode == CEE_BR_S ? getI1LittleEndian(codeAddr) : getI4LittleEndian(codeAddr)) == 0))
                {
                    break; // NOP
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
                        impSpillAllAppendTree(gtUnusedValNode(op1));
                    }

                    break;
                }

                if (op1->OperIsCompare())
                {
                    if (opcode == CEE_BRFALSE || opcode == CEE_BRFALSE_S)
                    {
                        Compiler::gtReverseRelop(op1->AsOp());
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

                // Fold comparison if we can

                op1 = gtFoldExpr(op1);

                // Try to fold the really simple cases like 'iconst *, ifne/ifeq'
                // Don't make any blocks unreachable in import only mode

                if (op1->OperIs(GT_CNS_INT))
                {
                    // gtFoldExpr() should prevent this as we don't want to make any blocks
                    // unreachable under compDbgCode
                    assert(!opts.compDbgCode);

                    BBjumpKinds foldedJumpKind = (BBjumpKinds)(op1->AsIntCon()->gtIconVal ? BBJ_ALWAYS : BBJ_NONE);

                    assert((block->bbJumpKind == BBJ_COND)            // normal case
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

                // GT_JTRUE is handled specially for non-empty stacks. See 'addStmt'
                // in impImportBlock(block). For correct line numbers, spill stack.

                if (opts.compDbgCode && impCurStmtOffs != BAD_IL_OFFSET)
                {
                    EnsureStackSpilled(false DEBUGARG("debug info spill"));
                }

                impSpillAllAppendTree(gtNewOperNode(GT_JTRUE, TYP_VOID, op1));
                break;

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

                // Recognize the IL idiom of CGT_UN(op1, 0) and normalize
                // it so that downstream optimizations don't have to.
                if ((opcode == CEE_CGT_UN) && op2->IsIntegralConst(0))
                {
                    oper = GT_NE;
                    uns  = false;
                }

                if (op1->IsNumericConst() && !op2->IsNumericConst())
                {
                    oper = GenTree::SwapRelop(oper);
                    std::swap(op1, op2);
                }

                if (varActualType(op1->GetType()) != varActualType(op2->GetType()))
                {
                    impAddCompareOpImplicitCasts(uns, op1, op2);

                    // TODO-MIKE-Review: This should be BADCODE.
                    assert((varActualType(op1->GetType()) == varActualType(op2->GetType())) ||
                           (varTypeIsI(op1->GetType()) == varTypeIsI(op2->GetType())));
                }

                op1 = gtNewOperNode(oper, TYP_INT, op1, op2);

                // TODO: setting both flags when only one is appropriate.
                if (uns)
                {
                    op1->gtFlags |= GTF_RELOP_NAN_UN | GTF_UNSIGNED;
                }

                // Fold result, if possible.
                op1 = gtFoldExpr(op1);

                impPushOnStack(op1);
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
                uns = true;
                goto CMP_2_OPs_AND_BR_ALL;
            CMP_2_OPs_AND_BR:
                uns = false;
                goto CMP_2_OPs_AND_BR_ALL;

            CMP_2_OPs_AND_BR_ALL:
                op2 = impPopStack().val;
                op1 = impPopStack().val;

                if (opts.OptimizationEnabled() && (block->bbJumpDest == block->bbNext))
                {
                    impBranchToNextBlock(block, op1, op2);
                    break;
                }

                if (op1->IsNumericConst() && !op2->IsNumericConst())
                {
                    oper = GenTree::SwapRelop(oper);
                    std::swap(op1, op2);
                }

                if (varActualType(op1->GetType()) != varActualType(op2->GetType()))
                {
                    impAddCompareOpImplicitCasts(uns, op1, op2);

                    // TODO-MIKE-Review: This should be BADCODE.
                    assert((varActualType(op1->GetType()) == varActualType(op2->GetType())) ||
                           (varTypeIsI(op1->GetType()) == varTypeIsI(op2->GetType())));
                }

                op1 = gtNewOperNode(oper, TYP_INT, op1, op2);

                // TODO: setting both flags when only one is appropriate.
                if (uns)
                {
                    op1->gtFlags |= GTF_RELOP_NAN_UN | GTF_UNSIGNED;
                }

                goto COND_JUMP;

            case CEE_SWITCH:
                // skip over the switch-table
                codeAddr += 4 + getU4LittleEndian(codeAddr) * 4;

                op1 = impPopStack().val;

                // TODO-MIKE-Review: This should be BADCODE.
                assert(varActualTypeIsIntOrI(op1->GetType()));

                impSpillAllAppendTree(gtNewOperNode(GT_SWITCH, TYP_VOID, op1));
                break;

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
            case CEE_CONV_U:
                lclTyp = TYP_U_IMPL;
#ifdef TARGET_64BIT
                goto CONV_UN;
#else
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

                // Casts from floating point types must not have GTF_UNSIGNED set.
                if (varTypeIsFloating(op1))
                {
                    uns = false;
                }

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
                            // Toss the cast, it's a waste of time

                            impPushOnStack(op1);
                            break;
                        }
                        else if (ival == mask)
                        {
                            // Toss the masking, it's a waste of time, since
                            // we sign-extend from the small value anyways

                            op1 = op1->AsOp()->gtOp1;
                        }
                    }
                }

                // The 'op2' sub-operand of a cast is the 'real' type number,
                // since the result of a cast to one of the 'small' integer
                // types is an integer.

                type = varActualType(lclTyp);

                // If this is a no-op cast, just use op1.
                if (!ovfl && (type == op1->GetType()) && (varTypeSize(type) == varTypeSize(lclTyp)))
                {
                    // Nothing needs to change
                }
                // Work is evidently required, add cast node
                else
                {
                    op1 = gtNewCastNode(op1, uns, lclTyp);

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

                impPushOnStack(op1);
                break;

            case CEE_NEG:
                op1  = impPopStack().val;
                type = op1->GetType();
                if (varTypeIsFloating(type))
                {
                    oper = GT_FNEG;
                }
                else
                {
                    impBashVarAddrsToI(op1, nullptr);
                    type = varActualType(type);
                    oper = GT_NEG;
                }
                impPushOnStack(gtNewOperNode(oper, type, op1));
                break;

            case CEE_POP:
                op1 = impImportPop(block);

                if (op1 != nullptr)
                {
                    impSpillAllAppendTree(op1);
                }

                break;

            case CEE_DUP:
                impImportDup();
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

                // TODO-MIKE-Review: This should be BADCODE.
                assert(op1->TypeIs(TYP_I_IMPL, TYP_BYREF));

                impBashVarAddrsToI(op1, op2);

                op2 = impImplicitR4orR8Cast(op2, lclTyp);
                op2 = impImplicitIorI4Cast(op2, lclTyp);

                if ((lclTyp == TYP_REF) && !op2->TypeIs(TYP_REF))
                {
                    // TODO-MIKE-Review: This should be BADCODE. Not clear why this allows
                    // I_IMPL, BYREF, much less INT. Typical nonsense...
                    assert(op2->TypeIs(TYP_INT, TYP_I_IMPL, TYP_BYREF));
                    lclTyp = op2->GetType();
                }

#ifdef DEBUG
                // TODO-MIKE-Review: This should be BADCODE. And it's a complete mess anyway.
                if (op2->TypeIs(TYP_BYREF) || (lclTyp == TYP_BYREF))
                {
                    if (op2->TypeIs(TYP_BYREF))
                    {
                        assert((lclTyp == TYP_BYREF) || (lclTyp == TYP_I_IMPL));
                    }
                    else if (lclTyp == TYP_BYREF)
                    {
                        assert(op2->TypeIs(TYP_BYREF) || varTypeIsIntOrI(op2->GetType()));
                    }
                }
                else
                {
                    assert((varActualType(op2->GetType()) == varActualType(lclTyp)) ||
                           ((lclTyp == TYP_I_IMPL) && (varActualType(op2->GetType()) == TYP_INT)) ||
                           (varTypeIsFloating(op2->GetType()) && varTypeIsFloating(lclTyp)));
                }
#endif

            // For CPOBJ op2 always has type lclType so we can skip all the type
            // compatibility checks above.
            STIND_CPOBJ:
                op1 = comp->gtNewIndStore(lclTyp, op1, op2);

                if ((prefixFlags & PREFIX_VOLATILE) != 0)
                {
                    op1->AsIndir()->SetVolatile();
                }

                if (((prefixFlags & PREFIX_UNALIGNED) != 0) && !varTypeIsByte(lclTyp))
                {
                    op1->AsIndir()->SetUnaligned();
                }

                op1->AddSideEffects(GTF_EXCEPT | GTF_GLOB_REF);

                // Spill side-effects AND global-data-accesses
                if (verCurrentState.esStackDepth > 0)
                {
                    impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("spill side effects before STIND"));
                }

                impSpillNoneAppendTree(op1);
                break;

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
                if (varActualTypeIsInt(op1->GetType()))
                {
                    op1 = gtNewCastNode(op1, false, TYP_LONG);
                }
#endif

                // TODO-MIKE-Review: This should be BADCODE. Might need to tolerate REF too.
                assert(op1->TypeIs(TYP_I_IMPL, TYP_BYREF));

                op1 = gtNewIndir(lclTyp, op1);

                op1->gtFlags |= GTF_EXCEPT | GTF_GLOB_REF;

                if ((prefixFlags & PREFIX_VOLATILE) != 0)
                {
                    op1->AsIndir()->SetVolatile();
                }

                if ((prefixFlags & PREFIX_UNALIGNED) && !varTypeIsByte(lclTyp))
                {
                    op1->AsIndir()->SetUnaligned();
                }

                impPushOnStack(op1);
                break;

            case CEE_LDFTN:
                ImportLdFtn(codeAddr, constrainedResolvedToken, prefixFlags);
                break;
            case CEE_LDVIRTFTN:
                ImportLdVirtFtn(codeAddr);
                break;
            case CEE_NEWOBJ:
                ImportNewObj(codeAddr, prefixFlags, block);
                break;
            case CEE_CALLI:
                ImportCallI(codeAddr, prefixFlags);
                break;
            case CEE_CALLVIRT:
            case CEE_CALL:
                ImportCall(codeAddr, opcode, &constrainedResolvedToken, prefixFlags);
                break;

            case CEE_LDFLD:
            case CEE_LDFLDA:
            {
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Field);
                JITDUMP(" %08X", resolvedToken.token);
                eeGetFieldInfo(&resolvedToken, (opcode == CEE_LDFLDA) ? CORINFO_ACCESS_ADDRESS : CORINFO_ACCESS_GET,
                               &fieldInfo);

                typeInfo tiObj = impStackTop().seTypeInfo;
                GenTree* obj   = impPopStack().val;

                // LDFLD(A) can be used with static fields. The address is ignored but side effects must be
                // preserved.
                if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC) != 0)
                {
                    if ((obj->gtFlags & GTF_SIDE_EFFECT) != 0)
                    {
                        impSpillAllAppendTree(gtUnusedValNode(obj));
                    }

                    opcode = opcode == CEE_LDFLD ? CEE_LDSFLD : CEE_LDSFLDA;
                    goto LDSFLD;
                }

                assert((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_INITCLASS) == 0);

                if (compIsForInlining())
                {
                    switch (fieldInfo.fieldAccessor)
                    {
                        case CORINFO_FIELD_INSTANCE_ADDR_HELPER:
                            compInlineResult->NoteFatal(InlineObservation::CALLEE_LDFLD_NEEDS_HELPER);
                            return;
                        case CORINFO_FIELD_INSTANCE_HELPER:
                        case CORINFO_FIELD_STATIC_ADDR_HELPER:
                        case CORINFO_FIELD_STATIC_TLS:
                        case CORINFO_FIELD_STATIC_GENERICS_STATIC_HELPER:
                        case CORINFO_FIELD_STATIC_READYTORUN_HELPER:
                            compInlineResult->NoteFatal(InlineObservation::CALLEE_COMPILATION_ERROR);
                            return;
                        default:
                            break;
                    }
                }

                impHandleAccessAllowed(fieldInfo.accessAllowed, fieldInfo.accessCalloutHelper);

                lclTyp = CorTypeToVarType(fieldInfo.fieldType);

                if (fieldInfo.fieldAccessor == CORINFO_FIELD_INSTANCE_ADDR_HELPER)
                {
                    op1 = impImportFieldInstanceAddrHelper(opcode, obj, &resolvedToken, fieldInfo, lclTyp,
                                                           fieldInfo.structType);
                }
                else
                {
                    assert((fieldInfo.fieldAccessor == CORINFO_FIELD_INSTANCE) ||
                           (fieldInfo.fieldAccessor == CORINFO_FIELD_INSTANCE_WITH_BASE));

                    if (tiObj.IsStruct())
                    {
                        // If the object is a struct, what we really want is
                        // for the field to operate on the address of the struct.

                        assert((opcode == CEE_LDFLD) && (tiObj.GetClassHandle() != NO_CLASS_HANDLE));

                        obj = impGetStructAddr(obj, tiObj.GetClassHandle(), CHECK_SPILL_ALL, true);
                    }

                    // TODO-MIKE-Review: It seems like this should apply to LDFLDA too,
                    // for some reason old code only did this for LCLFLD.
                    if (compIsForInlining() && (opcode == CEE_LDFLD) &&
                        impInlineIsGuaranteedThisDerefBeforeAnySideEffects(nullptr, nullptr, obj))
                    {
                        impInlineInfo->thisDereferencedFirst = true;
                    }

                    obj = impCheckForNullPointer(obj);

                    // Handle the weird case of fields belonging to primitive types. Such fields
                    // exist in IL/C# but the C# compiler usually does not use them, loading the
                    // m_value field of Int32 is done using ldind.i4 instead of ldfld for example.
                    // However, the C# compiler does not perform this transform when the address
                    // of the field is taken - it does emit ldflda m_value. Roslyn bug?
                    // Also, the C# compiler does not perform any transform in (U)IntPtr, as if
                    // these are normal structs. But the runtime does report them as primitives
                    // to the JIT so we can end up with an INT/LONG value and a field sequence
                    // for a field that doesn't exist as far as the JIT is concerned.
                    if (varTypeIsArithmetic(lclTyp) &&
                        (lclTyp == CorTypeToVarType(info.compCompHnd->asCorInfoType(resolvedToken.hClass))))
                    {
                        if (opcode == CEE_LDFLDA)
                        {
                            // TODO-MIKE-Fix: This likely needs a NULLCHECK but it's not worth the trouble
                            // now given the very specific cases that hit this - float/double.GetHashCode,
                            // where the address is immediately dereferenced.
                            op1 = obj;
                        }
                        else
                        {
                            op1 = gtNewIndir(lclTyp, obj);
                        }

                        fieldInfo.structType = NO_CLASS_HANDLE;
                    }
                    else
                    {
                        GenTreeFieldAddr* addr = impImportFieldAddr(obj, resolvedToken, fieldInfo);

                        if (opcode == CEE_LDFLDA)
                        {
                            op1 = addr;
                        }
                        else
                        {
                            op1 = gtNewFieldIndir(lclTyp, addr->GetLayoutNum(), addr);
                        }
                    }
                }

                if (opcode == CEE_LDFLD)
                {
                    assert(lclTyp == TYP_STRUCT ? op1->OperIs(GT_OBJ) : op1->OperIs(GT_IND));

                    if ((prefixFlags & PREFIX_VOLATILE) != 0)
                    {
                        op1->AsIndir()->SetVolatile();
                    }

                    if (((prefixFlags & PREFIX_UNALIGNED) != 0) && !varTypeIsByte(lclTyp))
                    {
                        op1->AsIndir()->SetUnaligned();
                    }

                    if (fieldInfo.structType != NO_CLASS_HANDLE)
                    {
                        impPushOnStack(op1, impMakeTypeInfo(fieldInfo.fieldType, fieldInfo.structType));
                        break;
                    }
                }

                impPushOnStack(op1);
            }
            break;

            case CEE_STFLD:
            {
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Field);
                JITDUMP(" %08X", resolvedToken.token);
                eeGetFieldInfo(&resolvedToken, CORINFO_ACCESS_SET, &fieldInfo);

                // TODO-MIKE-Review: This code uses both the value class handle and the field class handle,
                // in would make more sense to use only the field class handle. In theory they should be
                // identical but due to the A<Canon>/A<C> mess it might matter which one is used.
                clsHnd       = impStackTop().seTypeInfo.GetClassHandle();
                op2          = impPopStack().val;
                GenTree* obj = impPopStack().val;

                // STFLD can be used with static fields. The address is ignored but side effects must be preserved.
                if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC) != 0)
                {
                    if ((obj->gtFlags & GTF_SIDE_EFFECT) != 0)
                    {
                        impSpillAllAppendTree(gtUnusedValNode(obj));
                    }

                    opcode = CEE_STSFLD;
                    goto STSFLD;
                }

                assert((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_INITCLASS) == 0);

                if (compIsForInlining())
                {
                    switch (fieldInfo.fieldAccessor)
                    {
                        case CORINFO_FIELD_INSTANCE_ADDR_HELPER:
                            compInlineResult->NoteFatal(InlineObservation::CALLEE_STFLD_NEEDS_HELPER);
                            return;
                        case CORINFO_FIELD_INSTANCE_HELPER:
                        case CORINFO_FIELD_STATIC_ADDR_HELPER:
                        case CORINFO_FIELD_STATIC_TLS:
                        case CORINFO_FIELD_STATIC_GENERICS_STATIC_HELPER:
                        case CORINFO_FIELD_STATIC_READYTORUN_HELPER:
                            compInlineResult->NoteFatal(InlineObservation::CALLEE_COMPILATION_ERROR);
                            return;
                        default:
                            break;
                    }
                }

                impHandleAccessAllowed(fieldInfo.accessAllowed, fieldInfo.accessCalloutHelper);

                lclTyp = JITtype2varType(fieldInfo.fieldType);

                if (fieldInfo.fieldAccessor == CORINFO_FIELD_INSTANCE_ADDR_HELPER)
                {
                    op1 = impImportFieldInstanceAddrHelper(opcode, obj, &resolvedToken, fieldInfo, lclTyp, clsHnd);
                }
                else
                {
                    assert((fieldInfo.fieldAccessor == CORINFO_FIELD_INSTANCE) ||
                           (fieldInfo.fieldAccessor == CORINFO_FIELD_INSTANCE_WITH_BASE));

                    obj = impCheckForNullPointer(obj);

                    GenTreeFieldAddr* addr = impImportFieldAddr(obj, resolvedToken, fieldInfo);

                    if (compIsForInlining() && impInlineIsGuaranteedThisDerefBeforeAnySideEffects(op2, nullptr, obj))
                    {
                        impInlineInfo->thisDereferencedFirst = true;
                    }

                    op1 = gtNewFieldIndir(lclTyp, addr->GetLayoutNum(), addr);
                }

                // We have to spill GLOB_REFs for heap field stores since such fields may be
                // accessed via byrefs. We don't need to spill when the field belongs to an
                // unaliased local but in the importer aliased = "address taken" and stfld on
                // a local field implies "address taken". So we spill GLOB_REFs for all locals too.
                impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("STFLD stack spill temp"));

                assert((lclTyp == TYP_STRUCT) ? op1->OperIs(GT_OBJ) : op1->OperIs(GT_IND));

                if ((prefixFlags & PREFIX_VOLATILE) != 0)
                {
                    op1->AsIndir()->SetVolatile();
                }

                if (((prefixFlags & PREFIX_UNALIGNED) != 0) && !varTypeIsByte(lclTyp))
                {
                    op1->AsIndir()->SetUnaligned();
                }

                if (lclTyp == TYP_STRUCT)
                {
                    op1 = impAssignStruct(op1, op2, CHECK_SPILL_NONE);
                }
                else
                {
                    op2 = impConvertFieldStoreValue(op1->GetType(), op2);

                    // TODO-MIKE-Cleanup: It would be better to generate stores from the get go
                    op1->SetOper(op1->OperIs(GT_OBJ) ? GT_STORE_OBJ : GT_STOREIND);
                    op1->AsIndir()->SetValue(op2);
                    // We're expecting gtNewFieldIndir to add GLOB_REF as needed.
                    op1->AddSideEffects(GTF_ASG | op2->GetSideEffects());
                }

                impSpillNoneAppendTree(op1);
                break;
            }

            case CEE_LDSFLD:
            case CEE_LDSFLDA:
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Field);
                JITDUMP(" %08X", resolvedToken.token);
                eeGetFieldInfo(&resolvedToken, (opcode == CEE_LDSFLDA) ? CORINFO_ACCESS_ADDRESS : CORINFO_ACCESS_GET,
                               &fieldInfo);

                // Raise InvalidProgramException if static load accesses non-static field
                if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC) == 0)
                {
                    BADCODE("static access on an instance field");
                }

            LDSFLD:
                op1 = impImportLdSFld(opcode, &resolvedToken, fieldInfo, prefixFlags);

                if (op1 == nullptr)
                {
                    return;
                }

                if ((opcode == CEE_LDSFLD) && (fieldInfo.structType != NO_CLASS_HANDLE))
                {
                    impPushOnStack(op1, impMakeTypeInfo(fieldInfo.fieldType, fieldInfo.structType));
                }
                else
                {
                    impPushOnStack(op1);
                }
                break;

            case CEE_STSFLD:
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Field);
                JITDUMP(" %08X", resolvedToken.token);
                eeGetFieldInfo(&resolvedToken, CORINFO_ACCESS_SET, &fieldInfo);

                if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_STATIC) == 0)
                {
                    BADCODE("static access on an instance field");
                }

                clsHnd = impStackTop().seTypeInfo.GetClassHandle();
                op2    = impPopStack().val;

            STSFLD:
                op1 = impImportStSFld(op2, clsHnd, &resolvedToken, fieldInfo, prefixFlags);

                if (op1 == nullptr)
                {
                    return;
                }

                impSpillNoneAppendTree(op1);
                break;

            case CEE_NEWARR:
                ImportNewArr(codeAddr, block);
                break;
            case CEE_LOCALLOC:
                ImportLocAlloc(block);
                break;
            case CEE_ISINST:
                ImportIsInst(codeAddr);
                break;
            case CEE_MKREFANY:
                ImportMkRefAny(codeAddr);
                break;
            case CEE_REFANYVAL:
                ImportRefAnyVal(codeAddr);
                break;
            case CEE_REFANYTYPE:
                ImportRefAnyType();
                break;
            case CEE_LDTOKEN:
                ImportLdToken(codeAddr);
                break;

            case CEE_UNBOX:
            case CEE_UNBOX_ANY:
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);

                if ((opcode == CEE_UNBOX_ANY) && !info.compCompHnd->isValueClass(resolvedToken.hClass))
                {
                    JITDUMP("\n Importing UNBOX.ANY(refClass) as CASTCLASS\n");
                    ImportCastClass(resolvedToken, true);
                }
                else
                {
                    ImportUnbox(resolvedToken, opcode == CEE_UNBOX_ANY);
                }
                break;

            case CEE_BOX:
                codeAddr += ImportBox(codeAddr, codeEndp);
                break;

            case CEE_SIZEOF:
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);

                impPushOnStack(gtNewIconNode(info.compCompHnd->getClassSize(resolvedToken.hClass)));
                break;

            case CEE_CASTCLASS:
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Casting);
                JITDUMP(" %08X", resolvedToken.token);

                ImportCastClass(resolvedToken, false);
                break;

            case CEE_THROW:
                // Any block with a throw is rarely executed.
                block->bbSetRunRarely();

                op1 = impPopStack().val;
                op1 = gtNewHelperCallNode(CORINFO_HELP_THROW, TYP_VOID, gtNewCallArgs(op1));
                goto POP_APPEND;

            case CEE_RETHROW:
                assert(!compIsForInlining());

                if (info.compXcptnsCount == 0)
                {
                    BADCODE("rethrow outside catch");
                }

                op1 = gtNewHelperCallNode(CORINFO_HELP_RETHROW, TYP_VOID);
            POP_APPEND:
                if (verCurrentState.esStackDepth > 0)
                {
                    impSpillSideEffects(GTF_SIDE_EFFECT, CHECK_SPILL_ALL DEBUGARG("throw"));
                    verCurrentState.esStackDepth = 0;
                }

                impSpillNoneAppendTree(op1);
                break;

            case CEE_INITBLK:
                impImportInitBlk(prefixFlags);
                break;

            case CEE_CPBLK:
                impImportCpBlk(prefixFlags);
                break;

            case CEE_INITOBJ:
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

                // TODO-MIKE-Review: This should be BADCODE.
                assert(op1->TypeIs(TYP_I_IMPL, TYP_BYREF));

                impBashVarAddrsToI(op1);

                if (lclTyp != TYP_STRUCT)
                {
                    op2 = gtNewZeroConNode(varActualType(lclTyp));

                    goto STIND_CPOBJ;
                }

                impImportInitObj(op1, typGetObjLayout(resolvedToken.hClass));
                break;

            case CEE_CPOBJ:
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

                // TODO-MIKE-Review: This should be BADCODE.
                assert(op1->TypeIs(TYP_I_IMPL, TYP_BYREF));
                assert(op2->TypeIs(TYP_I_IMPL, TYP_BYREF));

                impBashVarAddrsToI(op1, op2);

                if (lclTyp != TYP_STRUCT)
                {
                    op2 = gtNewIndir(lclTyp, op2);
                    op2->gtFlags |= GTF_EXCEPT | GTF_GLOB_REF;

                    goto STIND_CPOBJ;
                }

                impImportCpObj(op1, op2, typGetObjLayout(resolvedToken.hClass));
                break;

            case CEE_STOBJ:
                impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
                JITDUMP(" %08X", resolvedToken.token);

                if (info.compCompHnd->isValueClass(resolvedToken.hClass))
                {
                    lclTyp = CorTypeToVarType(info.compCompHnd->asCorInfoType(resolvedToken.hClass));
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

                // TODO-MIKE-Review: This should be BADCODE.
                assert(varTypeIsStruct(op2->GetType()));

                op1 = gtNewObjNode(typGetObjLayout(resolvedToken.hClass), op1);
                op1 = impAssignStruct(op1, op2, CHECK_SPILL_ALL);

                if ((prefixFlags & PREFIX_UNALIGNED) != 0)
                {
                    if (op1->OperIs(GT_STORE_OBJ, GT_STOREIND))
                    {
                        // If the store value is MKREFANY impAssignStruct will append another indir,
                        // we don't set unaligned on that. It isn't necessary since the JIT doesn't
                        // do anything special with unaligned if the indir type is integral.

                        op1->AsIndir()->SetUnaligned();
                    }
                    else
                    {
                        // It's possible that impAssignStruct returned a CALL node (struct returned
                        // via return buffer). We're ignoring the unaligned prefix in this case.

                        // TODO-MIKE-Consider: We should probably introduce a temp, pass that as
                        // return buffer and then assign the temp to the actual STOBJ destination.

                        assert(op1->OperIs(GT_CALL) && op1->TypeIs(TYP_VOID));
                    }
                }

                // We have to spill GLOB_REFs even if the destination is a local,
                // we've got an address so the local is "address taken".
                impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("STOBJ stack spill temp"));
                impSpillNoneAppendTree(op1);
                break;

            case CEE_LDOBJ:
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

                // TODO-MIKE-Review: This should be BADCODE.
                assert(op1->TypeIs(TYP_BYREF, TYP_I_IMPL));

                lclTyp = JITtype2varType(info.compCompHnd->asCorInfoType(resolvedToken.hClass));
                op2    = op1;

                if (lclTyp == TYP_STRUCT)
                {
                    op1 = gtNewObjNode(typGetObjLayout(resolvedToken.hClass), op1);
                }
                else
                {
                    assert(varTypeIsArithmetic(lclTyp));

                    op1 = gtNewIndir(lclTyp, op1);
                    op1->gtFlags |= GTF_GLOB_REF;
                }

                if (op2->IsFieldAddr() && op2->AsFieldAddr()->GetFieldSeq()->IsBoxedValueField())
                {
                    op1->gtFlags |= GTF_IND_NONFAULTING;
                }
                else
                {
                    op1->gtFlags |= GTF_EXCEPT;
                }

                if ((prefixFlags & PREFIX_UNALIGNED) != 0)
                {
                    op1->AsIndir()->SetUnaligned();
                }

                // TODO-MIKE-Fix: This doesn't check for volatile. prefix...

                if ((lclTyp == TYP_STRUCT) ||
                    (info.compCompHnd->getTypeForPrimitiveValueClass(resolvedToken.hClass) == CORINFO_TYPE_UNDEF))
                {
                    impPushOnStack(op1, typeInfo(TI_STRUCT, resolvedToken.hClass));
                }
                else
                {
                    impPushOnStack(op1);
                }
                break;

            case CEE_LDLEN:
                op1 = impPopStack().val;

                if (opts.OptimizationEnabled())
                {
                    op1 = comp->gtNewArrLen(op1, OFFSETOF__CORINFO_Array__length);
                }
                else
                {
                    op2 = gtNewIconNode(OFFSETOF__CORINFO_Array__length, TYP_I_IMPL);
                    op1 = gtNewOperNode(GT_ADD, TYP_BYREF, op1, op2);
                    op1 = gtNewIndir(TYP_INT, op1);
                    op1->gtFlags |= GTF_EXCEPT;
                }

                impPushOnStack(op1);
                break;

            case CEE_BREAK:
                impSpillAllAppendTree(gtNewHelperCallNode(CORINFO_HELP_USER_BREAKPOINT, TYP_VOID));
                break;

            case CEE_NOP:
                if (opts.compDbgCode)
                {
                    impSpillAllAppendTree(new (comp, GT_NO_OP) GenTree(GT_NO_OP, TYP_VOID));
                }
                break;

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

        codeAddr += opcodeSize;
        prevOpcode = opcode;
    }
}
#ifdef _PREFAST_
#pragma warning(pop)
#endif

void Importer::ImportArgList()
{
    if (!info.compIsVarArgs)
    {
        BADCODE("arglist in non-vararg method");
    }

    assert(info.compMethodInfo->args.getCallConv() == CORINFO_CALLCONV_VARARG);

    LclVarDsc* varargsHandleParam = comp->lvaGetDesc(comp->info.compVarargsHandleArg);
    assert(varargsHandleParam->IsAddressExposed());

    impPushOnStack(gtNewLclVarAddrNode(varargsHandleParam, TYP_I_IMPL));
}

void Importer::ImportMkRefAny(const BYTE* codeAddr)
{
    assert(!compIsForInlining());

    // Being lazy here. Refanys are tricky in terms of gc tracking.
    // Since it is uncommon, just don't perform struct promotion in any method that contains mkrefany.
    // TODO-MIKE-Review: What the heck is this comment talking about?
    JITDUMP("disabling struct promotion because of mkrefany\n");
    comp->fgNoStructPromotion = true;

    CORINFO_RESOLVED_TOKEN resolvedToken;
    impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
    JITDUMP(" %08X", resolvedToken.token);

    GenTree* op2 = impTokenToHandle(&resolvedToken, /* mustRestoreHandle */ true);
    if (op2 == nullptr)
    {
        return;
    }

    CORINFO_HELPER_DESC          calloutHelper;
    CorInfoIsAccessAllowedResult accessAllowedResult =
        info.compCompHnd->canAccessClass(&resolvedToken, info.compMethodHnd, &calloutHelper);
    impHandleAccessAllowed(accessAllowedResult, calloutHelper);

    GenTree* op1 = impPopStack().val;

    // TODO-MIKE-Review: This should be BADCODE.
    assert(op1->TypeIs(TYP_BYREF, TYP_I_IMPL));

    // MKREFANY returns a struct.  op2 is the class token.
    op1 = gtNewOperNode(GT_MKREFANY, TYP_STRUCT, op1, op2);

    impPushOnStack(op1, typeInfo(TI_STRUCT, impGetRefAnyClass()));
}

void Importer::ImportRefAnyType()
{
    if (impStackTop().seTypeInfo.GetClassHandleForValueClass() != impGetRefAnyClass())
    {
        BADCODE("typedref expected");
    }

    GenTree* op1 = impPopStack().val;

    if (!op1->OperIs(GT_LCL_LOAD, GT_MKREFANY))
    {
        LclVarDsc* tmpLcl = lvaAllocTemp(true DEBUGARG("refanytype temp"));
        impAppendTempStore(tmpLcl, op1, impGetRefAnyClass(), CHECK_SPILL_ALL);
        op1 = gtNewLclvNode(tmpLcl, TYP_STRUCT);
    }

    if (GenTreeLclLoad* load = op1->IsLclLoad())
    {
        op1 = comp->gtNewLclLoadFld(TYP_I_IMPL, load->GetLcl(), OFFSETOF__CORINFO_TypedReference__type);
        op1->AsLclFld()->SetFieldSeq(GetRefanyTypeField());
    }
    else
    {
        // The pointer may have side-effects
        if (op1->AsOp()->gtOp1->gtFlags & GTF_SIDE_EFFECT)
        {
            impSpillAllAppendTree(op1->AsOp()->gtOp1);
        }

        // We already have the class handle
        op1 = op1->AsOp()->gtOp2;
    }

    // convert native TypeHandle to RuntimeTypeHandle
    op1 = gtNewHelperCallNode(CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE_MAYBENULL, TYP_STRUCT, gtNewCallArgs(op1));
    op1->AsCall()->SetRetLayout(typGetObjLayout(impGetTypeHandleClass()));
    op1->AsCall()->GetRetDesc()->InitializePrimitive(GetRuntimeHandleUnderlyingType());

    impPushOnStack(op1, typeInfo(TI_STRUCT, op1->AsCall()->GetRetLayout()->GetClassHandle()));
}

void Importer::ImportRefAnyVal(const BYTE* codeAddr)
{
    if (impStackTop().seTypeInfo.GetClassHandleForValueClass() != impGetRefAnyClass())
    {
        BADCODE("typedref expected");
    }

    CORINFO_RESOLVED_TOKEN resolvedToken;
    impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Class);
    JITDUMP(" %08X", resolvedToken.token);

    GenTree* op2 = impTokenToHandle(&resolvedToken);
    if (op2 == nullptr)
    {
        assert(compIsForInlining() && compDonotInline());
        return;
    }

    GenTree* op1 = impPopStack().val;

    if (op1->OperIs(GT_CALL, GT_RET_EXPR))
    {
        LclVarDsc* tmpLcl = lvaAllocTemp(true DEBUGARG("refanyval temp"));
        impAppendTempStore(tmpLcl, op1, impGetRefAnyClass(), CHECK_SPILL_ALL);
        op1 = gtNewLclvNode(tmpLcl, TYP_STRUCT);
    }

    {
        GenTreeCall::Use* arg1 = gtNewCallArgs(op2);
        GenTreeCall::Use* arg2 = gtNewCallArgs(op1);
        arg2->SetSigTypeNum(typGetObjLayoutNum(impGetRefAnyClass()));
        arg1->SetNext(arg2);

        op1 = gtNewHelperCallNode(CORINFO_HELP_GETREFANY, TYP_BYREF, arg1);
    }

    impPushOnStack(op1);
}

void Importer::ImportLocAlloc(BasicBlock* block)
{
    // We don't allow locallocs inside handlers
    if (block->hasHndIndex())
    {
        BADCODE("Localloc can't be inside handler");
    }

    if (verCurrentState.esStackDepth != 1)
    {
        BADCODE("Localloc can only be used when the stack is empty");
    }

    // Get the size to allocate
    GenTree* op2 = impPopStack().val;

    // TODO-MIKE-Review: This should be BADCODE.
    assert(genActualTypeIsIntOrI(op2->GetType()));

    // If the localloc is not in a loop and its size is a small constant,
    // create a new local var of TYP_BLK and return its address.
    // Need to aggressively fold here, as even fixed-size locallocs
    // will have casts in the way.
    op2 = gtFoldExpr(op2);

    GenTree* op1 = nullptr;

    if (GenTreeIntCon* icon = op2->IsIntCon())
    {
        const ssize_t allocSize = icon->GetValue();

        if (allocSize == 0)
        {
            // Result is nullptr
            JITDUMP("Converting stackalloc of 0 bytes to push null unmanaged pointer\n");
            op1 = gtNewIconNode(0, TYP_I_IMPL);
        }
        else if ((allocSize > 0) && !impBlockIsInALoop(block))
        {
            ssize_t maxSize = DEFAULT_MAX_LOCALLOC_TO_LOCAL_SIZE;
            INDEBUG(maxSize = JitConfig.JitStackAllocToLocalSize();)

            if (allocSize <= maxSize)
            {
                LclVarDsc* lcl = lvaAllocTemp(false DEBUGARG("small stackalloc temp"));
                lcl->SetBlockType(static_cast<unsigned>(allocSize));
                lcl->lvIsUnsafeBuffer = true;

                JITDUMP("Converting stackalloc of %zd bytes to new local V%02u\n", allocSize, lcl->GetLclNum());

                op1 = gtNewLclVarAddrNode(lcl, TYP_I_IMPL);

                if (!opts.compDbgEnC)
                {
                    // Ensure we have stack security for this method.
                    // Reorder layout since the converted localloc is treated as an unsafe buffer.
                    setNeedsGSSecurityCookie();
                    comp->compGSReorderStackLayout = true;
                }
            }
        }
    }

    if (op1 == nullptr)
    {
        // Bail out if inlining and the localloc was not converted.
        //
        // Note we might consider allowing the inline, if the call
        // site is not in a loop.
        if (compIsForInlining())
        {
            InlineObservation obs = op2->IsIntegralConst() ? InlineObservation::CALLEE_LOCALLOC_TOO_LARGE
                                                           : InlineObservation::CALLSITE_LOCALLOC_SIZE_UNKNOWN;
            compInlineResult->NoteFatal(obs);
            return;
        }

        op1 = gtNewOperNode(GT_LCLHEAP, TYP_I_IMPL, op2);
        // May throw a stack overflow exception. Obviously, we don't want locallocs to be CSE'd.
        op1->gtFlags |= (GTF_EXCEPT | GTF_DONT_CSE);

        // Ensure we have stack security for this method.
        setNeedsGSSecurityCookie();

        // The FP register may not be back to the original value at the end
        // of the method, even if the frame size is 0, as localloc may
        // have modified it. So we will HAVE to reset it
        comp->compLocallocUsed = true;
    }

    impPushOnStack(op1);
}

void Importer::ImportIsInst(const BYTE* codeAddr)
{
    CORINFO_RESOLVED_TOKEN resolvedToken;
    impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Casting);
    JITDUMP(" %08X", resolvedToken.token);

    GenTree* op2 = nullptr;

    if (!opts.IsReadyToRun())
    {
        op2 = impTokenToHandle(&resolvedToken);
        if (op2 == nullptr)
        {
            assert(compDonotInline());
            return;
        }
    }

    CORINFO_HELPER_DESC          calloutHelper;
    CorInfoIsAccessAllowedResult accessAllowedResult =
        info.compCompHnd->canAccessClass(&resolvedToken, info.compMethodHnd, &calloutHelper);
    impHandleAccessAllowed(accessAllowedResult, calloutHelper);

    GenTree* op1 = impPopStack().val;

    GenTree* optTree = impOptimizeCastClassOrIsInst(op1, &resolvedToken, false);

    if (optTree != nullptr)
    {
        impPushOnStack(optTree);
        return;
    }

    bool usingReadyToRunHelper = false;

#ifdef FEATURE_READYTORUN_COMPILER
    if (opts.IsReadyToRun())
    {
        GenTreeCall* opLookup = gtNewReadyToRunHelperCallNode(&resolvedToken, CORINFO_HELP_READYTORUN_ISINSTANCEOF,
                                                              TYP_REF, gtNewCallArgs(op1));
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

            op2 = impTokenToHandle(&resolvedToken);
            if (op2 == nullptr)
            {
                assert(compDonotInline());
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

    impPushOnStack(op1);
}

void Importer::ImportCastClass(CORINFO_RESOLVED_TOKEN& resolvedToken, bool isUnboxAny)
{
    GenTree* op2 = nullptr;

    if (!opts.IsReadyToRun() || isUnboxAny)
    {
        op2 = impTokenToHandle(&resolvedToken);

        if (op2 == nullptr)
        {
            assert(compDonotInline());
            return;
        }
    }

    CORINFO_HELPER_DESC          calloutHelper;
    CorInfoIsAccessAllowedResult accessAllowedResult =
        info.compCompHnd->canAccessClass(&resolvedToken, info.compMethodHnd, &calloutHelper);
    impHandleAccessAllowed(accessAllowedResult, calloutHelper);

    GenTree* op1     = impPopStack().val;
    GenTree* optTree = impOptimizeCastClassOrIsInst(op1, &resolvedToken, true);

    if (optTree != nullptr)
    {
        op1 = optTree;
    }
    else
    {
#ifdef FEATURE_READYTORUN_COMPILER
        bool usingReadyToRunHelper = false;

        if (opts.IsReadyToRun())
        {
            GenTreeCall* opLookup = gtNewReadyToRunHelperCallNode(&resolvedToken, CORINFO_HELP_READYTORUN_CHKCAST,
                                                                  TYP_REF, gtNewCallArgs(op1));
            usingReadyToRunHelper = (opLookup != nullptr);
            op1                   = (usingReadyToRunHelper ? opLookup : op1);

            if (!usingReadyToRunHelper)
            {
                // TODO: ReadyToRun: When generic dictionary lookups are necessary, replace the lookup call
                // and the chkcastany call with a single call to a dynamic R2R cell that will:
                //   1) Load the context
                //   2) Perform the generic dictionary lookup and caching, and generate the appropriate stub
                //   3) Check the object on the stack for the type-cast
                // Reason: performance (today, we'll always use the slow helper for the R2R generics case)

                op2 = impTokenToHandle(&resolvedToken);
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
    }

    impPushOnStack(op1);
}

void Importer::ImportUnbox(CORINFO_RESOLVED_TOKEN& resolvedToken, bool isUnboxAny)
{
    GenTree* op2 = impTokenToHandle(&resolvedToken);
    if (op2 == nullptr)
    {
        assert(compDonotInline());
        return;
    }

    CORINFO_HELPER_DESC          calloutHelper;
    CorInfoIsAccessAllowedResult accessAllowedResult =
        info.compCompHnd->canAccessClass(&resolvedToken, info.compMethodHnd, &calloutHelper);
    impHandleAccessAllowed(accessAllowedResult, calloutHelper);

    GenTree* op1 = impPopStack().val;

    // TODO-MIKE-Review: This should be BADCODE.
    assert(op1->TypeIs(TYP_REF));

    CorInfoHelpFunc helper = info.compCompHnd->getUnBoxHelper(resolvedToken.hClass);
    assert(helper == CORINFO_HELP_UNBOX || helper == CORINFO_HELP_UNBOX_NULLABLE);

    // Check legality and profitability of inline expansion for unboxing.
    const bool canExpandInline    = (helper == CORINFO_HELP_UNBOX);
    const bool shouldExpandInline = !currentBlock->isRunRarely() && opts.OptimizationEnabled();

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
            const TypeCompareState compare = info.compCompHnd->compareTypesForEquality(resolvedToken.hClass, clsHnd);

            if (compare == TypeCompareState::Must)
            {
                JITDUMP("\nOptimizing %s (%s) -- type test will succeed\n", isUnboxAny ? "UNBOX.ANY" : "UNBOX",
                        eeGetClassName(clsHnd));

                // For UNBOX, null check (if necessary), and then leave the box payload byref on the stack.
                if (!isUnboxAny)
                {
                    GenTree* op1Uses[2];
                    impMakeMultiUse(op1, 2, op1Uses, CHECK_SPILL_ALL DEBUGARG("optimized unbox clone"));

                    GenTree* boxPayloadOffset  = gtNewIconNode(TARGET_POINTER_SIZE, TYP_I_IMPL);
                    GenTree* boxPayloadAddress = gtNewOperNode(GT_ADD, TYP_BYREF, op1Uses[0], boxPayloadOffset);
                    GenTree* nullcheck         = gtNewNullCheck(op1Uses[1]);
                    impPushOnStack(gtNewCommaNode(nullcheck, boxPayloadAddress));

                    return;
                }

                // For UNBOX.ANY load the struct from the box payload byref (the load will nullcheck)
                assert(isUnboxAny);
                GenTree* boxPayloadOffset = gtNewIconNode(TARGET_POINTER_SIZE, TYP_I_IMPL);
                op1                       = gtNewOperNode(GT_ADD, TYP_BYREF, op1, boxPayloadOffset);

                goto LOAD_VALUE;
            }
            else
            {
                JITDUMP("\nUnable to optimize %s -- can't resolve type comparison\n",
                        isUnboxAny ? "UNBOX.ANY" : "UNBOX");
            }
        }
        else
        {
            JITDUMP("\nUnable to optimize %s -- class for [%06u] not known\n", isUnboxAny ? "UNBOX.ANY" : "UNBOX",
                    dspTreeID(op1));
        }

        JITDUMP("\n Importing %s as inline sequence\n", isUnboxAny ? "UNBOX.ANY" : "UNBOX");

        // we are doing normal unboxing
        // inline the common case of the unbox helper
        // UNBOX(exp) morphs into
        // clone = pop(exp);
        // ((*clone == typeToken) ? nop : helper(clone, typeToken));
        // push(clone + TARGET_POINTER_SIZE)

        GenTree* op1Uses[3];
        impMakeMultiUse(op1, 3, op1Uses, CHECK_SPILL_ALL DEBUGARG("inline unbox temp"));

        GenTree* condBox = gtNewOperNode(GT_EQ, TYP_INT, gtNewMethodTableLookup(op1Uses[0]), op2);

        op2 = impTokenToHandle(&resolvedToken);
        if (op2 == nullptr)
        {
            assert(!compDonotInline());
            return;
        }

        op1 = gtNewHelperCallNode(helper, TYP_VOID, gtNewCallArgs(op2, op1Uses[1]));
        op1 = gtNewQmarkNode(TYP_VOID, condBox, gtNewNothingNode(), op1);

        // QMARK nodes cannot reside on the evaluation stack. Because there
        // may be other trees on the evaluation stack that side-effect the
        // sources of the UNBOX operation we must spill the stack.

        impSpillAllAppendTree(op1);

        // Create the address-expression to reference past the object header
        // to the beginning of the value-type. Today this means adjusting
        // past the base of the objects vtable field which is pointer sized.

        op2 = gtNewIconNode(TARGET_POINTER_SIZE, TYP_I_IMPL);
        op1 = gtNewOperNode(GT_ADD, TYP_BYREF, op1Uses[2], op2);
    }
    else
    {
        JITDUMP("\n Importing %s as helper call because %s\n", isUnboxAny ? "UNBOX.ANY" : "UNBOX",
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

    if (!isUnboxAny)
    {
        if (helper == CORINFO_HELP_UNBOX_NULLABLE)
        {
            // Unbox nullable helper returns a struct type.
            // We need to spill it to a temp so than can take the address of it.
            // Here we need unsafe value cls check, since the address of struct
            // is taken to be used further along and potentially be exploitable.
            LclVarDsc* lcl = lvaAllocTemp(true DEBUGARG("unbox nullable temp"));
            comp->lvaSetStruct(lcl, typGetObjLayout(resolvedToken.hClass), /* checkUnsafeBuffer */ true);

            op2 = gtNewLclvNode(lcl, TYP_STRUCT);
            op1 = impAssignStruct(op2, op1, CHECK_SPILL_ALL);
            op2 = gtNewLclVarAddrNode(lcl, TYP_BYREF);
            op1 = gtNewCommaNode(op1, op2);
        }

        assert(op1->TypeIs(TYP_BYREF));

        impPushOnStack(op1);
        return;
    }

    assert(isUnboxAny);

    // Normal unbox helper returns a TYP_BYREF.
    if (helper != CORINFO_HELP_UNBOX)
    {
        assert(helper == CORINFO_HELP_UNBOX_NULLABLE);
        assert(op1->TypeIs(TYP_STRUCT));

#if FEATURE_MULTIREG_RET
        // TODO-MIKE-Cleanup: This has nothing to do with multireg returns.
        // No matter what the struct type is the helper returns the struct value
        // via an "out" parameter, there's no way to return it in registers because
        // the same helper is used for all struct types.
        // Doing this here is bad for CQ when the destination is a memory location,
        // because we introduce a temp instead of just passing in the address of
        // that location. impAssignStruct (TreatAsHasRetBufArg) already handles
        // this case so there's no real need to do this here.
        // Adding a temp when the destination is a promotable struct local might
        // be useful because it avoids dependent promotion. But's probably something
        // that impAssignStruct could handle as well.

        ClassLayout*  layout  = typGetObjLayout(resolvedToken.hClass);
        StructPassing retKind = abiGetStructReturnType(layout, CorInfoCallConvExtension::Managed, false);

        if (retKind.kind == SPK_ByValue)
        {
            // Unbox nullable helper returns a TYP_STRUCT.
            // For the multi-reg case we need to spill it to a temp so that
            // we can pass the address to the unbox_nullable jit helper.

            LclVarDsc* tmpLcl = lvaAllocTemp(true DEBUGARG("unbox nullable multireg temp"));

            tmpLcl->lvIsMultiRegArg = true;
            comp->lvaSetStruct(tmpLcl, layout, /* checkUnsafeBuffer */ true);

            op2 = gtNewLclvNode(tmpLcl, TYP_STRUCT);
            op1 = impAssignStruct(op2, op1, CHECK_SPILL_ALL);
            op2 = gtNewLclVarAddrNode(tmpLcl, TYP_BYREF);
            op1 = gtNewCommaNode(op1, op2);
        }
        else
#endif // !FEATURE_MULTIREG_RET
        {
            impPushOnStack(op1, typeInfo(TI_STRUCT, resolvedToken.hClass));
            return;
        }
    }

LOAD_VALUE:
    var_types lclTyp = CorTypeToVarType(info.compCompHnd->asCorInfoType(resolvedToken.hClass));
    op2              = op1;

    if (lclTyp == TYP_STRUCT)
    {
        op1 = gtNewObjNode(typGetObjLayout(resolvedToken.hClass), op1);
    }
    else
    {
        assert(varTypeIsArithmetic(lclTyp));

        op1 = gtNewIndir(lclTyp, op1);
        op1->gtFlags |= GTF_GLOB_REF;
    }

    if (op2->IsFieldAddr() && op2->AsFieldAddr()->GetFieldSeq()->IsBoxedValueField())
    {
        op1->gtFlags |= GTF_IND_NONFAULTING;
    }
    else
    {
        op1->gtFlags |= GTF_EXCEPT;
    }

    if ((lclTyp == TYP_STRUCT) ||
        (info.compCompHnd->getTypeForPrimitiveValueClass(resolvedToken.hClass) == CORINFO_TYPE_UNDEF))
    {
        impPushOnStack(op1, typeInfo(TI_STRUCT, resolvedToken.hClass));
    }
    else
    {
        impPushOnStack(op1);
    }
}

int Importer::ImportBox(const BYTE* codeAddr, const BYTE* codeEnd)
{
    CORINFO_RESOLVED_TOKEN resolvedToken;
    impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Box);
    JITDUMP(" %08X", resolvedToken.token);

    CORINFO_HELPER_DESC          calloutHelper;
    CorInfoIsAccessAllowedResult accessAllowedResult =
        info.compCompHnd->canAccessClass(&resolvedToken, info.compMethodHnd, &calloutHelper);
    impHandleAccessAllowed(accessAllowedResult, calloutHelper);

    if (!info.compCompHnd->isValueClass(resolvedToken.hClass))
    {
        // Boxing a reference type has no effect.
        return 0;
    }

    unsigned   patternSize;
    BoxPattern pattern = comp->impBoxPatternMatch(codeAddr + 4, codeEnd, &patternSize);

    if ((pattern != BoxPattern::None) && impImportBoxPattern(pattern, &resolvedToken, codeAddr + 4 DEBUGARG(codeEnd)))
    {
        return patternSize;
    }

    impImportAndPushBox(&resolvedToken);

    return 0;
}

void Importer::ImportLdToken(const BYTE* codeAddr)
{
    CORINFO_RESOLVED_TOKEN resolvedToken;
    impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Ldtoken);
    JITDUMP(" %08X", resolvedToken.token);
    assert(resolvedToken.hClass != nullptr);

    CORINFO_CLASS_HANDLE tokenType = info.compCompHnd->getTokenTypeAsHandle(&resolvedToken);
    GenTree*             token     = impTokenToHandle(&resolvedToken, /* mustRestoreHandle */ true);

    if (token == nullptr)
    {
        assert(compDonotInline());
        return;
    }

    CorInfoHelpFunc helper;

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

    GenTreeCall* call = gtNewHelperCallNode(helper, TYP_STRUCT, gtNewCallArgs(token));
    call->GetRetDesc()->InitializePrimitive(GetRuntimeHandleUnderlyingType());
    call->SetRetLayout(typGetObjLayout(tokenType));

    impPushOnStack(call, typeInfo(TI_STRUCT, tokenType));
}

void Importer::ImportJmp(const BYTE* codeAddr, BasicBlock* block)
{
    assert(!compIsForInlining());

    if ((info.compFlags & CORINFO_FLG_SYNCH) || block->hasTryIndex() || block->hasHndIndex())
    {
        // CEE_JMP does not make sense in some "protected" regions.
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

    CORINFO_RESOLVED_TOKEN resolvedToken;
    impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Method);

    JITDUMP(" %08X", resolvedToken.token);

    // The signature of the target has to be identical to ours.
    // At least check that argCnt and returnType match

    CORINFO_SIG_INFO sig;
    eeGetMethodSig(resolvedToken.hMethod, &sig);
    if (sig.numArgs != info.compMethodInfo->args.numArgs || sig.retType != info.compMethodInfo->args.retType ||
        sig.callConv != info.compMethodInfo->args.callConv)
    {
        BADCODE("Incompatible target for CEE_JMPs");
    }

    // Mark the basic block as being a JUMP instead of RETURN
    block->bbFlags |= BBF_HAS_JMP;
    // Set this flag to make sure register arguments have a location assigned
    // even if we don't use them inside the method
    comp->compJmpOpUsed = true;
    // TODO-MIKE-Review: What does struct promotion have to do with JMP?
    // Probably they messed up arg passing...
    comp->fgNoStructPromotion = true;

    impSpillNoneAppendTree(new (comp, GT_JMP) GenTreeJmp(resolvedToken.hMethod));
}

void Importer::ImportLdFtn(const BYTE* codeAddr, CORINFO_RESOLVED_TOKEN& constrainedResolvedToken, int prefixFlags)
{
    CORINFO_RESOLVED_TOKEN resolvedToken;
    impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Method);
    JITDUMP(" %08X", resolvedToken.token);

    CORINFO_CALL_INFO callInfo;
    eeGetCallInfo(&resolvedToken, (prefixFlags & PREFIX_CONSTRAINED) ? &constrainedResolvedToken : nullptr,
                  CORINFO_CALLINFO_SECURITYCHECKS | CORINFO_CALLINFO_LDFTN, &callInfo);

    // This check really only applies to intrinsic Array.Address methods
    if ((callInfo.sig.callConv & CORINFO_CALLCONV_PARAMTYPE) != 0)
    {
        NO_WAY("Currently do not support LDFTN of parameterized functions");
    }

    impHandleAccessAllowed(callInfo.accessAllowed, callInfo.callsiteCalloutHelper);

    GenTree* result = impMethodPointer(resolvedToken, callInfo);

    if (compDonotInline())
    {
        return;
    }

    CORINFO_RESOLVED_TOKEN* token = new (comp, CMK_Importer) CORINFO_RESOLVED_TOKEN(resolvedToken);
    // Call info may have more precise information about the function than the resolved token.
    assert(callInfo.hMethod != nullptr);
    token->hMethod = callInfo.hMethod;
    impPushOnStack(result, typeInfo(token));
}

void Importer::ImportLdVirtFtn(const BYTE* codeAddr)
{
    CORINFO_RESOLVED_TOKEN resolvedToken;
    impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Method);
    JITDUMP(" %08X", resolvedToken.token);

    CORINFO_CALL_INFO callInfo;
    eeGetCallInfo(&resolvedToken, nullptr,
                  CORINFO_CALLINFO_SECURITYCHECKS | CORINFO_CALLINFO_LDFTN | CORINFO_CALLINFO_CALLVIRT, &callInfo);

    // This check really only applies to intrinsic Array.Address methods
    if ((callInfo.sig.callConv & CORINFO_CALLCONV_PARAMTYPE) != 0)
    {
        NO_WAY("Currently do not support LDFTN of parameterized functions");
    }

    impHandleAccessAllowed(callInfo.accessAllowed, callInfo.callsiteCalloutHelper);

    bool isNonVirtual = callInfo.methodFlags & (CORINFO_FLG_FINAL | CORINFO_FLG_STATIC) ||
                        !(callInfo.methodFlags & CORINFO_FLG_VIRTUAL);

    if (compIsForInlining() && isNonVirtual)
    {
        compInlineResult->NoteFatal(InlineObservation::CALLSITE_LDVIRTFN_ON_NON_VIRTUAL);

        return;
    }

    GenTree* obj = impPopStack().val;
    assert(obj->TypeIs(TYP_REF));
    GenTree* result;

    if (opts.IsReadyToRun() ? (callInfo.kind != CORINFO_VIRTUALCALL_LDVIRTFTN) : isNonVirtual)
    {
        if ((obj->gtFlags & GTF_SIDE_EFFECT) != 0)
        {
            impSpillAllAppendTree(gtUnusedValNode(obj));
        }

        result = impMethodPointer(resolvedToken, callInfo);
    }
    else
    {
        result                  = impImportLdvirtftn(obj, &resolvedToken, &callInfo);
        resolvedToken.tokenType = CORINFO_TOKENKIND_Ldvirtftn;
    }

    if (compDonotInline())
    {
        return;
    }

    CORINFO_RESOLVED_TOKEN* token = new (comp, CMK_Importer) CORINFO_RESOLVED_TOKEN(resolvedToken);
    // Call info may have more precise information about the function than the resolved token.
    assert(callInfo.hMethod != nullptr);
    token->hMethod = callInfo.hMethod;
    impPushOnStack(result, typeInfo(token));
}

void Importer::ImportNewArr(const BYTE* codeAddr, BasicBlock* block)
{
    CORINFO_RESOLVED_TOKEN resolvedToken;
    impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Newarr);
    JITDUMP(" %08X", resolvedToken.token);

    GenTree* op1 = nullptr;
    GenTree* op2 = nullptr;

    if (!opts.IsReadyToRun())
    {
        // Need to restore array classes before creating array objects on the heap
        op1 = impTokenToHandle(&resolvedToken, /* mustRestoreHandle */ true);
        if (op1 == nullptr)
        {
            assert(compDonotInline());
            return;
        }
    }

    CORINFO_HELPER_DESC          calloutHelper;
    CorInfoIsAccessAllowedResult accessAllowedResult =
        info.compCompHnd->canAccessClass(&resolvedToken, info.compMethodHnd, &calloutHelper);
    impHandleAccessAllowed(accessAllowedResult, calloutHelper);

    // Form the arglist: array class handle, size
    op2 = impPopStack().val;

    // TODO-MIKE-Review: This should be BADCODE.
    assert(genActualTypeIsIntOrI(op2->GetType()));

#ifdef TARGET_64BIT
    // The array helper takes a native int for array length.
    // So if we have an int, explicitly extend it to be a native int.
    if (!varTypeIsLong(op2->GetType()))
    {
        if (op2->IsIntCon())
        {
            op2->SetType(TYP_LONG);
        }
        else
        {
            op2 = gtNewCastNode(op2, false, TYP_LONG);
        }
    }
#endif // TARGET_64BIT

    bool usingReadyToRunHelper = false;

#ifdef FEATURE_READYTORUN_COMPILER
    if (opts.IsReadyToRun())
    {
        op1 = gtNewReadyToRunHelperCallNode(&resolvedToken, CORINFO_HELP_READYTORUN_NEWARR_1, TYP_REF,
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
            op1 = impTokenToHandle(&resolvedToken, /* mustRestoreHandle */ true);
            if (op1 == nullptr)
            {
                assert(compDonotInline());
                return;
            }
        }
    }

    if (!usingReadyToRunHelper)
#endif
    {
        GenTreeCall::Use* args = gtNewCallArgs(op1, op2);

        // Create a call to 'new'

        // Note that this only works for shared generic code because the same helper is used for all
        // reference array types
        op1 = gtNewHelperCallNode(info.compCompHnd->getNewArrHelper(resolvedToken.hClass), TYP_REF, args);
    }

    op1->AsCall()->compileTimeHelperArgumentHandle = (CORINFO_GENERIC_HANDLE)resolvedToken.hClass;

    // Remember that this basic block contains 'new' of an sd array

    block->bbFlags |= BBF_HAS_NEWARRAY;
    comp->optMethodFlags |= OMF_HAS_NEWARRAY;

    impPushOnStack(op1, typeInfo(TI_REF, resolvedToken.hClass));
}

void Importer::ImportNewObj(const uint8_t* codeAddr, int prefixFlags, BasicBlock* block)
{
    if (compIsForInlining())
    {
        if ((impInlineInfo->inlineCandidateInfo->dwRestrictions & INLINE_RESPECT_BOUNDARY) != 0)
        {
            compInlineResult->NoteFatal(InlineObservation::CALLSITE_CROSS_BOUNDARY_SECURITY);

            return;
        }
    }

    CORINFO_RESOLVED_TOKEN resolvedToken;
    impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_NewObj);
    JITDUMP(" %08X", resolvedToken.token);

    CORINFO_CALL_INFO callInfo;
    eeGetCallInfo(&resolvedToken, nullptr, CORINFO_CALLINFO_SECURITYCHECKS | CORINFO_CALLINFO_ALLOWINSTPARAM,
                  &callInfo);

    if ((callInfo.methodFlags & (CORINFO_FLG_STATIC | CORINFO_FLG_ABSTRACT)) != 0)
    {
        BADCODE("newobj on static or abstract method");
    }

    // TODO-MIKE-Review: This should probably be BADCODE
    prefixFlags &= ~(PREFIX_TAILCALL_EXPLICIT | PREFIX_CONSTRAINED);

    // Insert the security callout before any actual code is generated
    impHandleAccessAllowed(callInfo.accessAllowed, callInfo.callsiteCalloutHelper);

    CORINFO_CLASS_HANDLE classHandle = resolvedToken.hClass;
    unsigned             classFlags  = callInfo.classFlags;

    // There are three different cases for new
    // Object size is variable (depends on arguments)
    //      1) Object is an array (arrays treated specially by the EE)
    //      2) Object is some other variable sized object (e.g. String)
    //      3) Class Size can be determined beforehand (normal case)
    // In the first case, we need to call a NEWOBJ helper (multinewarray).
    // In the second case we call the constructor with a null this pointer.
    // In the third case we alloc the memory, then call the constuctor.

    if ((classFlags & CORINFO_FLG_ARRAY) != 0)
    {
        assert((classFlags & CORINFO_FLG_VAROBJSIZE) != 0);

        impImportNewObjArray(&resolvedToken, &callInfo);

        return;
    }

    GenTree*   newObjThis = nullptr;
    LclVarDsc* lcl        = nullptr;

    // At present this can only be String
    if ((classFlags & CORINFO_FLG_VAROBJSIZE) != 0)
    {
        if (IsTargetAbi(CORINFO_CORERT_ABI))
        {
            // The dummy argument does not exist in CoreRT
            newObjThis = nullptr;
        }
        else
        {
            // This is the case for variable-sized objects that are not arrays.
            // In this case, call the constructor with a null 'this' pointer.
            newObjThis = gtNewIconNode(0, TYP_REF);
        }

        block->bbFlags |= BBF_HAS_NEWOBJ;
        comp->optMethodFlags |= OMF_HAS_NEWOBJ;
    }
    // This is the normal case where the size of the object is fixed.
    // Allocate the memory and call the constructor.
    // Note: We cannot add a peep to avoid use of temp here becase we
    // don't have enough interference info to detect when sources and
    // destination interfere, example: s = new S(ref);
    //
    // TODO: We find the correct place to introduce a general reverse
    // copy prop for struct return values from newobj or any function
    // returning structs.
    //
    // In the value class case we only need clsHnd for size calcs.
    // The lookup of the code pointer will be handled by CALL in this case.
    else if ((classFlags & CORINFO_FLG_VALUECLASS) != 0)
    {
        CorInfoType  corType = info.compCompHnd->asCorInfoType(classHandle);
        ClassLayout* layout  = impIsPrimitive(corType) ? nullptr : typGetObjLayout(classHandle);

#ifdef FEATURE_SIMD
        if ((layout != nullptr) && layout->IsVector() && ((callInfo.methodFlags & CORINFO_FLG_JIT_INTRINSIC) != 0))
        {
            // Only System.Numerics vectors have intrinsic constructors.
            assert((layout->GetVectorKind() == VectorKind::Vector234) ||
                   (layout->GetVectorKind() == VectorKind::VectorT));

            CORINFO_METHOD_HANDLE methodHandle       = callInfo.hMethod;
            const char*           className          = nullptr;
            const char*           namespaceName      = nullptr;
            const char*           enclosingClassName = nullptr;
            const char*           methodName = info.compCompHnd->getMethodNameFromMetadata(methodHandle, &className,
                                                                                 &namespaceName, &enclosingClassName);

            NamedIntrinsic ni = impFindSysNumSimdIntrinsic(methodHandle, className, methodName, enclosingClassName);

            if (ni != NI_Illegal)
            {
                GenTree* intrinsic = impImportSysNumSimdIntrinsic(ni, classHandle, methodHandle, &callInfo.sig, true);

                // TODO-MIKE-Cleanup: This should probably be an assert. impFindSysNumSimdIntrinsic is
                // dumb and returns an intrinsic even if intrinsics are disabled or if the relevant
                // ISAs aren't available. Otherwise there's no reason for intrinsic import to fail.
                if (intrinsic != nullptr)
                {
                    // Set the call type for ICorDebugInfo::CALL_SITE_BOUNDARIES, even if we treated
                    // this call as an intrinsic. These are constructors so the type is always VOID.
                    impPushOnStack(intrinsic, typeInfo(TI_STRUCT, classHandle));

                    return;
                }
            }
        }
#endif // FEATURE_SIMD

        lcl = lvaAllocTemp(true DEBUGARG("newobj temp"));

        if (compIsForInlining())
        {
            if (compInlineResult->IsFailure())
            {
                return;
            }

            // If value class has GC fields, inform the inliner. It may choose to bail out on the inline.
            if ((classFlags & CORINFO_FLG_CONTAINS_GC_PTR) != 0)
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

        if (layout == nullptr)
        {
            lcl->SetType(CorTypeToVarType(corType));
        }
        else
        {
            comp->lvaSetStruct(lcl, layout, /* checkUnsafeBuffer */ true);
        }

        bool bbInALoop  = impBlockIsInALoop(block);
        bool bbIsReturn = (block->bbJumpKind == BBJ_RETURN) &&
                          (!compIsForInlining() || (impInlineInfo->iciBlock->bbJumpKind == BBJ_RETURN));

        if (fgVarNeedsExplicitZeroInit(lcl, bbInALoop, bbIsReturn))
        {
            impSpillNoneAppendTree(comp->gtNewLclStore(lcl, lcl->GetType(), gtNewIconNode(0)));
        }
        else
        {
            JITDUMP("\nSuppressing zero-init for V%02u -- expect to zero in prolog\n", lcl->GetLclNum());

            lcl->lvSuppressedZeroInit    = true;
            comp->compSuppressedZeroInit = true;
        }

        newObjThis = gtNewLclVarAddrNode(lcl, TYP_BYREF);
    }
    else
    {
        bool            hasSideEffects = false;
        CorInfoHelpFunc newHelper = info.compCompHnd->getNewHelper(&resolvedToken, info.compMethodHnd, &hasSideEffects);

        if (hasSideEffects)
        {
            JITDUMP("\nSpilling stack for finalizable newobj\n");

            impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("finalizable newobj spill"));
        }

        lcl = lvaNewTemp(TYP_REF, true DEBUGARG("newobj temp"));

        if (compDonotInline())
        {
            return;
        }

        GenTree* alloc = gtNewAllocObjNode(&resolvedToken, /* useParent */ true);

        if (alloc == nullptr)
        {
            assert(compDonotInline());
            return;
        }

        block->bbFlags |= BBF_HAS_NEWOBJ;
        comp->optMethodFlags |= OMF_HAS_NEWOBJ;

        assert(!lcl->lvSingleDef);
        lcl->lvSingleDef = true;
        JITDUMP("Marked " FMT_LCL " as a single def local\n", lcl->GetLclNum());
        comp->lvaSetClass(lcl, classHandle, /* isExact */ true);

        // Append the store to the temp/local. We don't need to spill the stack as
        // we are just calling a JIT helper which can only throw OutOfMemoryException.
        // We assign the newly allocated object (by a ALLOCOBJ node) to a temp. Note that
        // the pattern "temp = alloc" is required by ObjectAllocator phase to be able
        // to determine ALLOCOBJ nodes without exhaustive walk over all expressions.

        impSpillNoneAppendTree(comp->gtNewLclStore(lcl, TYP_REF, alloc));

        newObjThis = comp->gtNewLclLoad(lcl, TYP_REF);
    }

    if (compDonotInline())
    {
        return;
    }

    CORINFO_RESOLVED_TOKEN* ldftnToken = nullptr;

    if ((classFlags & CORINFO_FLG_DELEGATE) != 0)
    {
        // Only verifiable cases are supported.
        // dup; ldvirtftn; newobj; or ldftn; newobj.
        // IL test could contain unverifiable sequence, in this case optimization should not be done.

        if (impStackHeight() > 0)
        {
            typeInfo delegateTypeInfo = impStackTop().seTypeInfo;
            if (delegateTypeInfo.IsMethod())
            {
                ldftnToken = delegateTypeInfo.GetToken();
            }
        }
    }

    GenTreeCall* call =
        impImportCall(CEE_NEWOBJ, &resolvedToken, nullptr, newObjThis, prefixFlags, &callInfo, codeAddr);

    if (call == nullptr)
    {
        return;
    }

    if (((callInfo.sig.callConv & CORINFO_CALLCONV_EXPLICITTHIS) == 0) && (newObjThis != nullptr))
    {
        assert(!call->IsVirtual());

        call->gtCallThisArg = gtNewCallArgs(newObjThis);
        call->AddSideEffects(newObjThis->GetSideEffects());
    }

    if ((classFlags & CORINFO_FLG_VAROBJSIZE) != 0)
    {
        // This is a 'new' of a variable sized object, where the constructor is to return the object.
        // In this case the signature has VOID return but we know that the object is actually returned.

        call->SetType(TYP_REF);
        call->SetRetSigType(TYP_REF);

        impPushOnStack(call, typeInfo(TI_REF, classHandle));

        return;
    }

#if defined(DEBUG) || defined(INLINE_DATA)
    call->gtRawILOffset = static_cast<IL_OFFSET>(codeAddr - info.compCode);
#endif

    CORINFO_CONTEXT_HANDLE exactContextHnd = callInfo.contextHandle;

    if ((classFlags & CORINFO_FLG_DELEGATE) != 0)
    {
        call = fgOptimizeDelegateConstructor(call, &exactContextHnd, ldftnToken);
    }

    impMarkInlineCandidate(call, exactContextHnd, callInfo.exactContextNeedsRuntimeLookup, &callInfo);
    impSpillAllAppendTree(call);

    if ((classFlags & CORINFO_FLG_VALUECLASS) != 0)
    {
        impPushOnStack(gtNewLclvNode(lcl, lcl->GetType()), typeInfo(TI_STRUCT, classHandle));
    }
    else
    {
        impPushOnStack(gtNewLclvNode(lcl, TYP_REF), typeInfo(TI_REF, classHandle));
    }
}

void Importer::ImportCallI(const uint8_t* codeAddr, int prefixFlags)
{
    // TODO-MIKE-Review: This should probably be BADCODE
    prefixFlags &= ~PREFIX_CONSTRAINED;

    if (compIsForInlining())
    {
        // CALLI doesn't have a method handle, so assume the worst.
        if ((impInlineInfo->inlineCandidateInfo->dwRestrictions & INLINE_RESPECT_BOUNDARY) != 0)
        {
            compInlineResult->NoteFatal(InlineObservation::CALLSITE_CROSS_BOUNDARY_CALLI);

            return;
        }
    }

    CORINFO_RESOLVED_TOKEN resolvedToken{};
    CORINFO_CALL_INFO      callInfo{};

    resolvedToken.token        = getU4LittleEndian(codeAddr);
    resolvedToken.tokenContext = impTokenLookupContextHandle;
    resolvedToken.tokenScope   = info.compScopeHnd;
    JITDUMP(" %08X", resolvedToken.token);

    ImportCall(codeAddr, CEE_CALLI, resolvedToken, nullptr, callInfo, prefixFlags);
}

void Importer::ImportCall(const uint8_t*          codeAddr,
                          OPCODE                  opcode,
                          CORINFO_RESOLVED_TOKEN* constrainedResolvedToken,
                          int                     prefixFlags)
{
    assert((opcode == CEE_CALL) || (opcode == CEE_CALLVIRT));

    CORINFO_RESOLVED_TOKEN resolvedToken;
    impResolveToken(codeAddr, &resolvedToken, CORINFO_TOKENKIND_Method);
    JITDUMP(" %08X", resolvedToken.token);

    CORINFO_CALL_INFO callInfo;
    eeGetCallInfo(&resolvedToken, (prefixFlags & PREFIX_CONSTRAINED) ? constrainedResolvedToken : nullptr,
                  CORINFO_CALLINFO_ALLOWINSTPARAM | CORINFO_CALLINFO_SECURITYCHECKS |
                      ((opcode == CEE_CALLVIRT) ? CORINFO_CALLINFO_CALLVIRT : CORINFO_CALLINFO_NONE),
                  &callInfo);

    impHandleAccessAllowed(callInfo.accessAllowed, callInfo.callsiteCalloutHelper);

    ImportCall(codeAddr, opcode, resolvedToken, constrainedResolvedToken, callInfo, prefixFlags);
}

void Importer::ImportCall(const uint8_t*          codeAddr,
                          OPCODE                  opcode,
                          CORINFO_RESOLVED_TOKEN& resolvedToken,
                          CORINFO_RESOLVED_TOKEN* constrainedResolvedToken,
                          CORINFO_CALL_INFO&      callInfo,
                          int                     prefixFlags)
{
    assert(impOpcodeIsCallOpcode(opcode));

    bool           isConstrained         = (prefixFlags & PREFIX_CONSTRAINED) != 0;
    bool           allowImplicitTailcall = (prefixFlags & PREFIX_TAILCALL_EXPLICIT) == 0;
    const uint8_t* nextOpcodeAddr        = codeAddr + 4;
    INDEBUG(bool isInTailcallReturnBlock = false);

    if (compIsForInlining())
    {
        assert(!compDonotInline());
        // We rule out inlinees with explicit tail calls in fgMakeBasicBlocks.
        assert((prefixFlags & PREFIX_TAILCALL_EXPLICIT) == 0);
    }
#ifdef DEBUG
    else if (compTailCallStress())
    {
        // In tail call stress mode we always create a RETURN block for a call followed
        // by a ret. This block does not include the ret instruction (since the ret could
        // be a jump target) so even if we decide not to synthesize an explicit tail call
        // we still need to create a RETURN node out of thin air.
        isInTailcallReturnBlock = static_cast<OPCODE>(*nextOpcodeAddr) == CEE_RET;

        if (isInTailcallReturnBlock && allowImplicitTailcall)
        {
            if (verCheckTailCallConstraint(opcode, &resolvedToken, isConstrained ? constrainedResolvedToken : nullptr))
            {
                CORINFO_METHOD_HANDLE exactMethod =
                    ((callInfo.kind == CORINFO_VIRTUALCALL_STUB) || (callInfo.kind == CORINFO_VIRTUALCALL_VTABLE))
                        ? nullptr
                        : callInfo.hMethod;

                if (info.compCompHnd->canTailCall(info.compMethodHnd, callInfo.hMethod, exactMethod, false))
                {
                    JITDUMP(" (Tailcall stress: prefixFlags |= PREFIX_TAILCALL_EXPLICIT)");

                    prefixFlags |= PREFIX_TAILCALL_EXPLICIT | PREFIX_TAILCALL_STRESS;
                }
                else
                {
                    JITDUMP(" (Tailcall stress: runtime preventing tailcall)");
                }
            }
            else
            {
                JITDUMP(" (Tailcall stress: constraint check failed)");
            }

            allowImplicitTailcall = false;
        }
    }
#endif // DEBUG

#if FEATURE_TAILCALL_OPT
    if (allowImplicitTailcall && (JitConfig.TailCallOpt() != 0) && opts.OptimizationEnabled()
        // Note that we don't care if the RET is in a different block, if we do tail
        // call then the call's block will eventually be converted to a RETURN block.
        && (nextOpcodeAddr < info.compCode + info.compILCodeSize) && (static_cast<OPCODE>(*nextOpcodeAddr) == CEE_RET)
        // When inlining the inliner call has to be an implicit tail call as well,
        // to ensure that the inlinee's call ends up in a RETRUN block eventually.
        && (!compIsForInlining() || impInlineInfo->iciCall->IsImplicitTailCall()))
    {
        JITDUMP("\n (Implicit Tail call: prefixFlags |= PREFIX_TAILCALL_IMPLICIT)");

        prefixFlags |= PREFIX_TAILCALL_IMPLICIT;
    }
#endif

    // TODO-MIKE-Review: Can this be moved to ImportCallI?
    if ((opcode == CEE_CALLI) && IsTargetAbi(CORINFO_CORERT_ABI))
    {
        assert(constrainedResolvedToken == nullptr);

        // See comment in impCheckForPInvokeCall
        BasicBlock* block = compIsForInlining() ? impInlineInfo->iciBlock : currentBlock;

        if (info.compCompHnd->convertPInvokeCalliToCall(&resolvedToken, !impCanPInvokeInlineCallSite(block)))
        {
            eeGetCallInfo(&resolvedToken, nullptr, CORINFO_CALLINFO_ALLOWINSTPARAM, &callInfo);
            opcode = CEE_CALL;
        }
    }

    impImportCall(opcode, &resolvedToken, isConstrained ? constrainedResolvedToken : nullptr, nullptr, prefixFlags,
                  &callInfo, codeAddr);

    if (compDonotInline())
    {
        return;
    }

    if ((prefixFlags & PREFIX_TAILCALL_EXPLICIT) != 0)
    {
        // An explicit tail call always gets its own RETURN block that does not
        // include the following ret instruction, if the call returns a value we
        // need to generate a RETURN node now.
        if (info.compRetType != TYP_VOID)
        {
            impReturnInstruction(INDEBUG(true));
        }
    }
#ifdef DEBUG
    else if (isInTailcallReturnBlock)
    {
        impReturnInstruction();
    }
#endif
}

// Load an argument on the operand stack
// Shared by the various CEE_LDARG opcodes
// ilArgNum is the argument index as specified in IL.
// It will be mapped to the correct lclNum
void Importer::impLoadArg(unsigned ilArgNum)
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
        if (ilArgNum >= info.GetILArgCount())
        {
            BADCODE("Bad IL arg num");
        }

        unsigned lclNum = comp->lvaMapILArgNumToLclNum(ilArgNum);

        if (lclNum == info.GetThisParamLclNum())
        {
            lclNum = comp->lvaThisLclNum;
        }

        impPushLclVar(comp->lvaGetDesc(lclNum));
    }
}

// Load a local on the operand stack
// Shared by the various CEE_LDLOC opcodes
// ilLocNum is the local index as specified in IL.
// It will be mapped to the correct lclNum
void Importer::impLoadLoc(unsigned ilLocNum)
{
    LclVarDsc* lcl;

    if (compIsForInlining())
    {
        if (ilLocNum >= impInlineInfo->ilLocCount)
        {
            compInlineResult->NoteFatal(InlineObservation::CALLEE_BAD_LOCAL_NUMBER);
            return;
        }

        lcl = inlGetInlineeLocal(impInlineInfo, ilLocNum);
    }
    else
    {
        if (ilLocNum >= info.GetILLocCount())
        {
            BADCODE("Bad IL loc num");
        }

        lcl = comp->lvaGetDesc(info.GetParamCount() + ilLocNum);
    }

    impPushLclVar(lcl);
}

// Load a local/argument on the operand stack
void Importer::impPushLclVar(LclVarDsc* lcl)
{
    var_types type = lcl->GetType();

    if (!lcl->lvNormalizeOnLoad())
    {
        type = varActualType(type);
    }

    impPushOnStack(gtNewLclvNode(lcl, type), lcl->lvImpTypeInfo);
}

//------------------------------------------------------------------------
// impInlineReturnInstruction: import a return during inlining
//
// Returns:
//     True if import was successful (may fail for some inlinees)
//
bool Importer::impInlineReturnInstruction()
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

void Importer::impReturnInstruction(INDEBUG(bool isTailcall))
{
    assert(!compIsForInlining());
    assert(currentBlock->bbJumpKind == BBJ_RETURN);

    GenTree* ret;

    if (info.compRetType == TYP_VOID)
    {
        assert(info.retDesc.GetRegCount() == 0);

        ret = new (comp, GT_RETURN) GenTreeOp(GT_RETURN, TYP_VOID);
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
            var_types retType = varActualType(info.compRetType);
            var_types valType = varActualType(value->GetType());

            // TODO-MIKE-Review: This should be BADCODE.
            assert((valType == retType) || ((valType == TYP_I_IMPL) && (retType == TYP_BYREF)) ||
                   (varTypeIsGC(valType) && (retType == TYP_I_IMPL)) ||
                   (varTypeIsFloating(valType) && varTypeIsFloating(retType)) ||
                   (varTypeIsStruct(valType) && varTypeIsStruct(retType)));
        }

        if (!isTailcall && opts.compGcChecks && (info.compRetType == TYP_REF))
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
            LclVarDsc* retBuffLcl   = comp->lvaGetDesc(info.compRetBuffArg);
            GenTree*   retBuffAddr  = gtNewLclvNode(retBuffLcl, TYP_BYREF);
            GenTree*   retBuffIndir = gtNewObjNode(info.GetRetLayout(), retBuffAddr);
            value                   = impAssignStruct(retBuffIndir, value, CHECK_SPILL_ALL);

            impSpillNoneAppendTree(value);

            if (info.retDesc.GetRegCount() == 0)
            {
                ret = new (comp, GT_RETURN) GenTreeOp(GT_RETURN, TYP_VOID);
            }
            else
            {
                // There are cases where the address of the implicit RetBuf should be returned explicitly.

                assert(info.retDesc.GetRegCount() == 1);
                assert(info.retDesc.GetRegType(0) == TYP_BYREF);

                ret = gtNewOperNode(GT_RETURN, TYP_BYREF, gtNewLclvNode(retBuffLcl, TYP_BYREF));
            }
        }
        else
        {
            assert(info.retDesc.GetRegCount() >= 1);

            if (value->IsCall() && value->AsCall()->TreatAsHasRetBufArg())
            {
                value = impSpillPseudoReturnBufferCall(value->AsCall());
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

    impSpillNoneAppendTree(ret);
}

void Importer::impAddPendingEHSuccessors(BasicBlock* block)
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

void Importer::ImportSingleBlockMethod(BasicBlock* block)
{
    assert((block == comp->fgFirstBB) && (block->bbNext == nullptr) && ((block->bbFlags & BBF_INTERNAL) == 0));

    currentBlock = block;
    impImportBlockCode(block);
    impStmtListEnd(block);

    if (compIsForInlining() && (block->firstStmt() != nullptr))
    {
        compInlineResult->SetImportedILSize(block->bbCodeOffsEnd - block->bbCodeOffs);
    }

    if (comp->fgCheapPredsValid)
    {
        fgRemovePreds();
    }
}

void Importer::impImportBlock(BasicBlock* block)
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

        for (BasicBlock* const succBlock : block->Succs())
        {
            impImportBlockPending(succBlock);
        }

        return;
    }

    impSetCurrentState(block);

    currentBlock = block;

    if (((block->bbFlags & BBF_TRY_BEG) != 0) && (verCurrentState.esStackDepth != 0))
    {
        BADCODE("Evaluation stack must be empty on entry into a try block");
    }

    if (block->hasTryIndex())
    {
        impAddPendingEHSuccessors(block);
    }

#ifdef FEATURE_ON_STACK_REPLACEMENT
    // Are there any places in the method where we might add a patchpoint?
    if (comp->compHasBackwardJump)
    {
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
                comp->setMethodHasPatchpoint();
            }
        }
    }
    else
    {
        // Should not see backward branch targets w/o backwards branches
        assert((block->bbFlags & BBF_BACKWARD_JUMP_TARGET) == 0);
    }
#endif // FEATURE_ON_STACK_REPLACEMENT

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

    // Some of the append/spill logic works on currentBlock

    assert(currentBlock == block);

    // Save the tree list in the block
    impStmtListEnd(block);

    // impStmtListEnd sets BBF_IMPORTED on the block
    // We do *NOT* want to set it later than this because
    // impReimportSpillClique might clear it if this block is both a
    // predecessor and successor in the current spill clique
    assert((block->bbFlags & BBF_IMPORTED) != 0);

    // If we had a int/native int, or float/double collision, we need to re-import
    if (reimportSpillClique)
    {
        impReimportSpillClique(block);
    }

    for (BasicBlock* const succBlock : block->Succs())
    {
        impImportBlockPending(succBlock);
    }
}

bool Importer::impSpillStackAtBlockEnd(BasicBlock* block)
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
    impBoxTempLcl = nullptr;

    // Remove the branch statement at the end of the block (if any) because spilling must
    // be done before it. It will need to be added back once spilling is done.
    Statement* branchStmt = nullptr;

    if (block->bbJumpKind == BBJ_COND)
    {
        branchStmt = impStmtListRemoveLast();
        assert(branchStmt->GetRootNode()->OperIs(GT_JTRUE));
    }
    else if (block->bbJumpKind == BBJ_SWITCH)
    {
        branchStmt = impStmtListRemoveLast();
        assert(branchStmt->GetRootNode()->OperIs(GT_SWITCH));
    }

    ImportSpillCliqueState* state               = block->bbExitState;
    bool                    reimportSpillClique = false;

    if (state == nullptr)
    {
        LclVarDsc* spillTemps = lvaAllocTemps(verCurrentState.esStackDepth DEBUGARG("spill clique temp"));

        state = new (comp, CMK_Importer) ImportSpillCliqueState(spillTemps, verCurrentState.esStackDepth);

        impSetSpillCliqueState(block, state);

        for (unsigned level = 0; level < verCurrentState.esStackDepth; level++)
        {
            LclVarDsc* spillTempLcl = &spillTemps[level];
            GenTree*   tree         = verCurrentState.esStack[level].val;
            typeInfo   stackType    = verCurrentState.esStack[level].seTypeInfo;

            JITDUMPTREE(tree, "Stack entry %u:\n", level);

            impAppendTempStore(spillTempLcl, tree, stackType.GetClassHandle(), CHECK_SPILL_NONE);

            if (stackType.IsStruct())
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

        LclVarDsc* spillTemps = state->GetSpillTemps();

        for (unsigned level = 0; level < verCurrentState.esStackDepth; level++)
        {
            LclVarDsc* spillTempLcl = &spillTemps[level];
            GenTree*   tree         = verCurrentState.esStack[level].val;

            JITDUMPTREE(tree, "Stack entry %u:\n", level);

            if (tree->OperIs(GT_LCL_VAR) && (tree->AsLclVar()->GetLcl() == spillTempLcl))
            {
                assert(tree->GetType() == spillTempLcl->GetType());
                continue;
            }

            if (tree->TypeIs(TYP_BYREF) && spillTempLcl->TypeIs(TYP_I_IMPL))
            {
                // VC generates code where it pushes a byref from one branch, and an int (ldc.i4 0) from
                // the other.
                // However, if the branch which leaves the TYP_I_IMPL on the stack is imported first, the
                // successor would be imported assuming there was a TYP_I_IMPL on the stack. Thus the value
                // would not get GC-tracked. Hence, change the temp to TYP_BYREF and reimport the successors.
                // We don't need to do this is the tree represents a local address, these do not need to
                // be reported to the GC as byrefs.

                if (impIsAddressInLocal(tree))
                {
                    tree->SetType(TYP_I_IMPL);
                }
                else
                {
                    spillTempLcl->SetType(TYP_BYREF);
                    reimportSpillClique = true;
                }
            }
#ifdef TARGET_64BIT
            else if (tree->TypeIs(TYP_LONG) && spillTempLcl->TypeIs(TYP_INT))
            {
                // Some other block in the spill clique set this to "int", but now we have "native int".
                // Change the type and go back to re-import any blocks that used the wrong type.
                spillTempLcl->SetType(TYP_LONG);
                reimportSpillClique = true;
            }
            else if (varActualTypeIsInt(tree->GetType()) && spillTempLcl->TypeIs(TYP_LONG))
            {
                // Spill clique has decided this should be "native int", but this block only pushes an "int".
                // Insert a sign-extension to "native int" so we match the clique.
                tree = gtNewCastNode(tree, false, TYP_LONG);
            }
            // Consider the case where one branch left a 'byref' on the stack and the other leaves
            // an 'int'. On 32-bit, this is allowed since they are the same size. JIT64 managed to
            // make this work on 64-bit. For compatibility, we support JIT64 behavior instead of
            // asserting and then generating bad code (where we save/restore the low 32 bits of a
            // byref pointer to an 'int' sized local). If the 'int' side has been imported already,
            // we need to change the type of the local and reimport the spill clique. If the 'byref'
            // side has imported, we insert a cast from int to 'native int' to match the 'byref' size.
            else if (tree->TypeIs(TYP_BYREF) && spillTempLcl->TypeIs(TYP_INT))
            {
                // Some other block in the spill clique set this to "int", but now we have "byref".
                // Change the type and go back to re-import any blocks that used the wrong type.
                spillTempLcl->SetType(TYP_BYREF);
                reimportSpillClique = true;
            }
            else if (varActualTypeIsInt(tree->GetType()) && spillTempLcl->TypeIs(TYP_BYREF))
            {
                // Spill clique has decided this should be "byref", but this block only pushes an "int".
                // Insert a sign-extension to "native int" so we match the clique size.
                tree = gtNewCastNode(tree, false, TYP_LONG);
            }
#endif // TARGET_64BIT
            else if (tree->TypeIs(TYP_DOUBLE) && spillTempLcl->TypeIs(TYP_FLOAT))
            {
                // Some other block in the spill clique set this to "float", but now we have "double".
                // Change the type and go back to re-import any blocks that used the wrong type.
                spillTempLcl->SetType(TYP_DOUBLE);
                reimportSpillClique = true;
            }
            else if (tree->TypeIs(TYP_FLOAT) && spillTempLcl->TypeIs(TYP_DOUBLE))
            {
                // Spill clique has decided this should be "double", but this block only pushes a "float".
                // Insert a cast to "double" so we match the clique.
                tree = gtNewCastNode(tree, false, TYP_DOUBLE);
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

                    if (impHasLclRef(relOp->GetOp(0), spillTempLcl))
                    {
                        LclVarDsc* tempLcl = lvaAllocTemp(true DEBUGARG("branch spill temp"));
                        impAppendTempStore(tempLcl, relOp->GetOp(0), level);
                        relOp->SetOp(0, gtNewLclvNode(tempLcl, tempLcl->GetType()));
                    }

                    if (impHasLclRef(relOp->GetOp(1), spillTempLcl))
                    {
                        LclVarDsc* tempLcl = lvaAllocTemp(true DEBUGARG("branch spill temp"));
                        impAppendTempStore(tempLcl, relOp->GetOp(1), level);
                        relOp->SetOp(1, gtNewLclvNode(tempLcl, tempLcl->GetType()));
                    }
                }
                else
                {
                    assert(branch->OperIs(GT_SWITCH));

                    if (impHasLclRef(branch->GetOp(0), spillTempLcl))
                    {
                        LclVarDsc* tempLcl = lvaAllocTemp(true DEBUGARG("branch spill temp"));
                        impAppendTempStore(tempLcl, branch->GetOp(0), level);
                        branch->SetOp(0, gtNewLclvNode(tempLcl, tempLcl->GetType()));
                    }
                }
            }

            GenTree* store;

            if (varTypeIsStruct(spillTempLcl->GetType()))
            {
                GenTree* dst = gtNewLclvNode(spillTempLcl, spillTempLcl->GetType());
                store        = impAssignStruct(dst, tree, CHECK_SPILL_NONE);
                assert(!store->IsNothingNode());
            }
            else
            {
                store = comp->gtNewLclStore(spillTempLcl, spillTempLcl->GetType(), tree);
            }

            impSpillNoneAppendTree(store);
        }
    }

    if (branchStmt != nullptr)
    {
        impStmtListAppend(branchStmt);
    }

    return reimportSpillClique;
}

void Importer::impImportBlockPending(BasicBlock* block)
{
    if (((block->bbFlags & BBF_IMPORTED) == 0) && !impIsPendingBlockMember(block))
    {
        JITDUMP(FMT_BB " pending import\n", block->bbNum);

        impPushPendingBlock(block);
    }
}

void Importer::impPushPendingBlock(BasicBlock* block)
{
    assert((block->bbFlags & BBF_IMPORTED) == 0);
    assert(!impIsPendingBlockMember(block));

    BlockListNode* dsc   = new (this) BlockListNode(block, impPendingBlockStack);
    impPendingBlockStack = dsc;
    impSetPendingBlockMember(block, true);
}

bool Importer::impIsPendingBlockMember(BasicBlock* block)
{
    return (block->bbFlags & BBF_MARKED) != 0;
}

void Importer::impSetPendingBlockMember(BasicBlock* block, bool pending)
{
    if (pending)
    {
        block->bbFlags |= BBF_MARKED;
    }
    else
    {
        block->bbFlags &= ~BBF_MARKED;
    }
}

BasicBlock* Importer::impPopPendingBlock()
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

void* Importer::BlockListNode::operator new(size_t sz, Importer* importer)
{
    if (importer->impBlockListNodeFreeList == nullptr)
    {
        return importer->comp->getAllocator(CMK_Importer).allocate<BlockListNode>(1);
    }
    else
    {
        BlockListNode* res                 = importer->impBlockListNodeFreeList;
        importer->impBlockListNodeFreeList = res->m_next;
        return res;
    }
}

void Importer::FreeBlockListNode(Importer::BlockListNode* node)
{
    node->m_next             = impBlockListNodeFreeList;
    impBlockListNodeFreeList = node;
}

void Importer::impWalkSpillCliqueFromPred(BasicBlock* block, SpillCliqueWalker* callback)
{
    if (!comp->fgCheapPredsValid)
    {
        fgComputeCheapPreds();
    }

    BlockListNode* succCliqueToDo = nullptr;
    BlockListNode* predCliqueToDo = new (this) BlockListNode(block);

    for (bool toDo = true; toDo;)
    {
        toDo = false;

        // Look at the successors of every member of the predecessor to-do list.
        while (predCliqueToDo != nullptr)
        {
            BlockListNode* node = predCliqueToDo;
            predCliqueToDo      = node->m_next;
            BasicBlock* blk     = node->m_blk;
            FreeBlockListNode(node);

            for (BasicBlock* const succ : blk->Succs())
            {
                if (impAddSpillCliqueSuccMember(succ))
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

                if (impAddSpillCliquePredMember(predBlock))
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
    assert(impIsSpillCliquePredMember(block));
}

void Importer::impSetSpillCliqueState(BasicBlock* block, ImportSpillCliqueState* state)
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

void Importer::impReimportSpillClique(BasicBlock* block)
{
    class ReimportSpillClique : public SpillCliqueWalker
    {
        Importer*   importer;
        BasicBlock* currentBlock;

    public:
        ReimportSpillClique(Importer* importer, BasicBlock* currentBlock)
            : importer(importer), currentBlock(currentBlock)
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
            assert(!importer->impIsPendingBlockMember(block));

            if ((block == currentBlock) && (dir == SpillCliquePred))
            {
                // The current block, which triggered re-importing, does not need
                // to be re-imported unless it is a successor (e.g. current block
                // is reached with a FLOAT on the stack but then it pushes DOUBLE
                // and loops back to itself).
                return;
            }

            JITDUMP(FMT_BB " will be reimported\n", block->bbNum);

            block->bbFlags &= ~BBF_IMPORTED;
            importer->impPushPendingBlock(block);
        }
    } callback(this, block);

    // If we get here, it is because this block is already part of a spill clique
    // and one predecessor had an outgoing live stack slot of type int, and this
    // block has an outgoing live stack slot of type native int.
    // We need to reset these before traversal because they have already been set
    // by the previous walk to determine all the members of the spill clique.

    for (BasicBlock* block : comp->Blocks())
    {
        block->spillCliquePredMember = false;
        block->spillCliqueSuccMember = false;
    }

    impWalkSpillCliqueFromPred(block, &callback);
}

// Set the current state to the state at the start of the basic block
void Importer::impSetCurrentState(BasicBlock* block)
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

    LclVarDsc* spillTemps = block->bbEntryState->GetSpillTemps();

    for (unsigned i = 0; i < verCurrentState.esStackDepth; i++)
    {
        LclVarDsc* lcl = &spillTemps[i];

        verCurrentState.esStack[i].val        = gtNewLclvNode(lcl, lcl->GetType());
        verCurrentState.esStack[i].seTypeInfo = lcl->lvImpTypeInfo;
    }
}

unsigned BasicBlock::bbStackDepthOnEntry() const
{
    return (bbEntryState == nullptr) ? 0 : bbEntryState->GetStackDepth();
}

Compiler* Compiler::impInlineRoot()
{
    return (impInlineInfo == nullptr) ? this : impInlineInfo->InlinerCompiler;
}

#ifdef DEBUG
bool Importer::impIsSpillCliquePredMember(BasicBlock* block)
{
    return block->spillCliquePredMember;
}
#endif

bool Importer::impAddSpillCliqueSuccMember(BasicBlock* block)
{
    if (block->spillCliqueSuccMember)
    {
        return false;
    }

    block->spillCliqueSuccMember = true;
    return true;
}

bool Importer::impAddSpillCliquePredMember(BasicBlock* block)
{
    if (block->spillCliquePredMember)
    {
        return false;
    }

    block->spillCliquePredMember = true;
    return true;
}

// Convert the instrs ("import") into our internal format (trees).
// The basic flowgraph has already been constructed and is passed in.
void Importer::Import()
{
    JITDUMP("*************** In impImport() for %s\n", info.compFullName);

    assert(comp->fgFirstBB->bbEntryState == nullptr);

    InitDebuggingInfo();

    BasicBlock* entryBlock = comp->fgFirstBB;

    if (entryBlock->bbNext == nullptr)
    {
        ImportSingleBlockMethod(entryBlock);

        return;
    }

    // Skip leading internal blocks.
    // These can arise from needing a leading scratch BB, from EH normalization, and from OSR entry redirects.
    //
    // We expect a linear flow to the first non-internal block. But not necessarily straght-line flow.

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
    if (verbose && (info.compXcptnsCount != 0))
    {
        printf("\nAfter impImport() added block for try,catch,finally");
        fgDispBasicBlocks();
        printf("\n");
    }
#endif

    // Estimate how much of method IL was actually imported.
    //
    // Note this includes (to some extent) the impact of importer folded
    // branches, provided the folded tree covered the entire block's IL.
    unsigned importedILSize = 0;

    if (compIsForInlining() INDEBUG(|| true))
    {
        for (BasicBlock* const block : comp->Blocks())
        {
            if ((block->bbFlags & BBF_IMPORTED) != 0)
            {
                // Assume if we generate any IR for the block we generate IR for the entire block.
                if (block->firstStmt() != nullptr)
                {
                    // TODO-MIKE-Review: Some EH code generates blocks where bbCodeOffs is valid
                    // and bbCodeOffsEnd is BAD_IL_OFFSET, that doesn't make a lot of sense.
                    if ((block->bbCodeOffs < block->bbCodeOffsEnd) && (block->bbCodeOffsEnd <= MAX_IL_OFFSET))
                    {
                        importedILSize += block->bbCodeOffsEnd - block->bbCodeOffs;
                    }
                }
            }
        }
    }

    if (importedILSize != info.compILCodeSize)
    {
        // Could be tripped up if we ever duplicate blocks
        assert(importedILSize <= info.compILCodeSize);

        JITDUMP("\n** Note: %s IL was partially imported -- imported %u of %u bytes of method IL\n",
                compIsForInlining() ? "inlinee" : "root method", importedILSize, info.compILCodeSize);
    }

    if (compIsForInlining())
    {
        compInlineResult->SetImportedILSize(importedILSize);
    }

    if (comp->fgCheapPredsValid)
    {
        fgRemovePreds();
    }
}

GenTreeLclAddr* Compiler::impIsAddressInLocal(GenTree* tree)
{
    if (!tree->TypeIs(TYP_BYREF, TYP_I_IMPL))
    {
        return nullptr;
    }

    while (true)
    {
        if (tree->OperIs(GT_ADD, GT_SUB))
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
        else if (GenTreeFieldAddr* field = tree->IsFieldAddr())
        {
            tree = field->GetAddr();
        }
        else
        {
            break;
        }
    }

    return tree->OperIs(GT_LCL_ADDR) ? tree->AsLclAddr() : nullptr;
}

// TODO-MIKE-Cleanup: This should be merged with impIsAddressInLocal
GenTreeLclAddr* Compiler::impIsLocalAddrExpr(GenTree* node)
{
    while (node->OperIs(GT_ADD))
    {
        GenTree* op1 = node->AsOp()->GetOp(0);
        GenTree* op2 = node->AsOp()->GetOp(1);

        if (op1->OperIs(GT_CNS_INT))
        {
            std::swap(op1, op2);
        }

        if (!op2->OperIs(GT_CNS_INT))
        {
            return nullptr;
        }

        node = op1;
    }

    return node->OperIs(GT_LCL_ADDR) ? node->AsLclAddr() : nullptr;
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

    bool callsiteIsGeneric = (rootCompiler->info.compMethodInfo->args.sigInst.methInstCount != 0) ||
                             (rootCompiler->info.compMethodInfo->args.sigInst.classInstCount != 0);

    bool calleeIsGeneric = (info.compMethodInfo->args.sigInst.methInstCount != 0) ||
                           (info.compMethodInfo->args.sigInst.classInstCount != 0);

    if (!callsiteIsGeneric && calleeIsGeneric)
    {
        inlineResult->Note(InlineObservation::CALLSITE_NONGENERIC_CALLS_GENERIC);
    }

    // Inspect callee's arguments (and the actual values at the callsite for them)
    CORINFO_SIG_INFO        sig    = info.compMethodInfo->args;
    CORINFO_ARG_LIST_HANDLE sigArg = sig.args;

    GenTreeCall::Use* argUse = pInlineInfo == nullptr ? nullptr : pInlineInfo->iciCall->AsCall()->gtCallArgs;

    for (unsigned i = 0; i < info.compMethodInfo->args.numArgs; i++)
    {
        CORINFO_CLASS_HANDLE sigClass;
        CorInfoType          corType = strip(info.compCompHnd->getArgType(&sig, sigArg, &sigClass));
        GenTree*             argNode = argUse == nullptr ? nullptr : argUse->GetNode();

        if (corType == CORINFO_TYPE_CLASS)
        {
            sigClass = info.compCompHnd->getArgClass(&sig, sigArg);
        }
        else if (corType == CORINFO_TYPE_VALUECLASS)
        {
            inlineResult->Note(InlineObservation::CALLEE_ARG_STRUCT);
        }
        else if (corType == CORINFO_TYPE_BYREF)
        {
            sigClass = info.compCompHnd->getArgClass(&sig, sigArg);
            corType  = info.compCompHnd->getChildType(sigClass, &sigClass);
        }

        if (argNode != nullptr)
        {
            bool                 isExact   = false;
            bool                 isNonNull = false;
            CORINFO_CLASS_HANDLE argCls    = gtGetClassHandle(argNode, &isExact, &isNonNull);
            if (argCls != nullptr)
            {
                const bool isArgValueType = info.compCompHnd->isValueClass(argCls);
                // Exact class of the arg is known
                if (isExact && !isArgValueType)
                {
                    inlineResult->Note(InlineObservation::CALLSITE_ARG_EXACT_CLS);
                    if ((argCls != sigClass) && (sigClass != nullptr))
                    {
                        // .. but the signature accepts a less concrete type.
                        inlineResult->Note(InlineObservation::CALLSITE_ARG_EXACT_CLS_SIG_IS_NOT);
                    }
                }
                // Arg is a reference type in the signature and a boxed value type was passed.
                else if (isArgValueType && (corType == CORINFO_TYPE_CLASS))
                {
                    inlineResult->Note(InlineObservation::CALLSITE_ARG_BOXED);
                }
            }

            if (argNode->OperIsConst())
            {
                inlineResult->Note(InlineObservation::CALLSITE_ARG_CONST);
            }
            argUse = argUse->GetNext();
        }
        sigArg = info.compCompHnd->getArgNext(sigArg);
    }

    // Note if the callee's return type is a value type
    if (info.compMethodInfo->args.retType == CORINFO_TYPE_VALUECLASS)
    {
        inlineResult->Note(InlineObservation::CALLEE_RETURNS_STRUCT);
    }

    // Note if the callee's class is a promotable struct
    if ((info.compClassAttr & CORINFO_FLG_VALUECLASS) != 0)
    {
        CORINFO_CLASS_HANDLE* cache = impPromotableStructTypeCache;

        if (pInlineInfo != nullptr)
        {
            cache = pInlineInfo->InlinerCompiler->impPromotableStructTypeCache;
        }

        bool promotable;

        if (info.compClassHnd == cache[0])
        {
            promotable = false;
        }
        else if (info.compClassHnd == cache[1])
        {
            promotable = true;
        }
        else
        {
            StructPromotionHelper helper(this);
            promotable        = helper.CanPromoteStructType(info.compClassHnd);
            cache[promotable] = info.compClassHnd;
        }

        if (promotable)
        {
            inlineResult->Note(InlineObservation::CALLEE_CLASS_PROMOTABLE);
        }
        inlineResult->Note(InlineObservation::CALLEE_CLASS_VALUETYPE);
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
    if ((pInlineInfo == nullptr) || (pInlineInfo->iciBlock->isMaxBBWeight()))
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

    bool   hasProfile  = false;
    double profileFreq = 0.0;

    // If the call site has profile data, report the relative frequency of the site.
    //
    if ((pInlineInfo != nullptr) && rootCompiler->fgHaveSufficientProfileData())
    {
        const BasicBlock::weight_t callSiteWeight = pInlineInfo->iciBlock->bbWeight;
        const BasicBlock::weight_t entryWeight    = rootCompiler->fgFirstBB->bbWeight;
        profileFreq                               = entryWeight == 0.0f ? 0.0 : callSiteWeight / entryWeight;
        hasProfile                                = true;

        assert(callSiteWeight >= 0);
        assert(entryWeight >= 0);
    }
    else if (pInlineInfo == nullptr)
    {
        // Simulate a hot callsite for PrejitRoot mode.
        hasProfile  = true;
        profileFreq = 1.0;
    }

    inlineResult->NoteBool(InlineObservation::CALLSITE_HAS_PROFILE, hasProfile);
    inlineResult->NoteDouble(InlineObservation::CALLSITE_PROFILE_FREQUENCY, profileFreq);
}

// This method makes STATIC inlining decision based on the IL code.
// It should not make any inlining decision based on the context.
// If forceInline is true, then the inlining decision should not depend on
// performance heuristics (code size, etc.).
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

            // Try to get the code address/size for the method

            CORINFO_METHOD_INFO methInfo;
            if (!pParam->pThis->info.compCompHnd->getMethodInfo(pParam->fncHandle, &methInfo))
            {
                pParam->result->NoteFatal(InlineObservation::CALLEE_NO_METHOD_INFO);
                return;
            }

            // Profile data allows us to avoid early "too many IL bytes" outs.
            pParam->result->NoteBool(InlineObservation::CALLSITE_HAS_PROFILE,
                                     pParam->pThis->fgHaveSufficientProfileData());

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

            // Get the method properties

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
                pInfo->guardedClassHandle              = nullptr;
                pInfo->guardedMethodHandle             = nullptr;
                pInfo->guardedMethodUnboxedEntryHandle = nullptr;
                pInfo->stubAddr                        = nullptr;
                pInfo->likelihood                      = 0;
                pInfo->requiresInstMethodTableArg      = false;
            }

            pInfo->methInfo                       = methInfo;
            pInfo->ilCallerHandle                 = pParam->pThis->info.compMethodHnd;
            pInfo->clsHandle                      = clsHandle;
            pInfo->exactContextHnd                = pParam->exactContextHnd;
            pInfo->retExprPlaceholder             = nullptr;
            pInfo->dwRestrictions                 = dwRestrictions;
            pInfo->preexistingSpillTemp           = nullptr;
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
bool Importer::impInlineIsGuaranteedThisDerefBeforeAnySideEffects(GenTree*          additionalTree,
                                                                  GenTreeCall::Use* additionalCallArgs,
                                                                  GenTree*          dereferencedAddress)
{
    assert(compIsForInlining());
    assert(opts.OptEnabled(CLFLG_INLINING));

    BasicBlock* block = currentBlock;

    if (block != comp->fgFirstBB)
    {
        return false;
    }

    if (!impInlineInfo->IsThisParam(dereferencedAddress))
    {
        return false;
    }

    auto HasGloballyVisibleSideEffects = [](GenTree* tree) {
        return tree->HasAnySideEffect(GTF_CALL | GTF_EXCEPT) || tree->HasAllSideEffects(GTF_ASG | GTF_GLOB_REF);
    };

    if ((additionalTree != nullptr) && HasGloballyVisibleSideEffects(additionalTree))
    {
        return false;
    }

    for (GenTreeCall::Use& use : GenTreeCall::UseList(additionalCallArgs))
    {
        if (HasGloballyVisibleSideEffects(use.GetNode()))
        {
            return false;
        }
    }

    for (Statement* stmt : StatementList(impStmtList))
    {
        if (HasGloballyVisibleSideEffects(stmt->GetRootNode()))
        {
            return false;
        }
    }

    for (unsigned level = 0; level < verCurrentState.esStackDepth; level++)
    {
        if (HasGloballyVisibleSideEffects(verCurrentState.esStack[level].val))
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

void Importer::impMarkInlineCandidate(GenTreeCall*           call,
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

void Importer::impMarkInlineCandidateHelper(GenTreeCall*           call,
                                            CORINFO_CONTEXT_HANDLE exactContextHnd,
                                            bool                   exactContextNeedsRuntimeLookup,
                                            CORINFO_CALL_INFO*     callInfo)
{
    // Let the strategy know there's another call
    impInlineRoot()->m_inlineStrategy->NoteCall();

    if (!opts.OptEnabled(CLFLG_INLINING))
    {
        // This assert is misleading. The caller does not ensure that we have CLFLG_INLINING set before
        // calling impMarkInlineCandidate. However, if this assert trips it means that we're an inlinee
        // and CLFLG_MINOPT is set. That doesn't make a lot of sense. If you hit this assert, work back
        // and figure out why we did not set MAXOPT for this compile.
        assert(!compIsForInlining());
        return;
    }

    InlineResult inlineResult(comp, call, nullptr, "impMarkInlineCandidate");

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

    // Ignore helper calls
    if (call->gtCallType == CT_HELPER)
    {
        assert(!call->IsGuardedDevirtualizationCandidate());
        inlineResult.NoteFatal(InlineObservation::CALLSITE_IS_CALL_TO_HELPER);
        return;
    }

    // Ignore indirect calls
    if (call->gtCallType == CT_INDIRECT)
    {
        inlineResult.NoteFatal(InlineObservation::CALLSITE_IS_NOT_DIRECT_MANAGED);
        return;
    }

    // I removed the check for BBJ_THROW. BBJ_THROW is usually marked as rarely run. This more or less
    // restricts the inliner to non-expanding inlines. I removed the check to allow for non-expanding
    // inlining in throw blocks. I should consider the same thing for catch and filter regions.

    CORINFO_METHOD_HANDLE fncHandle;
    unsigned              methAttr;

    if (call->IsGuardedDevirtualizationCandidate())
    {
        if (call->gtGuardedDevirtualizationCandidateInfo->guardedMethodUnboxedEntryHandle != nullptr)
        {
            fncHandle = call->gtGuardedDevirtualizationCandidateInfo->guardedMethodUnboxedEntryHandle;
        }
        else
        {
            fncHandle = call->gtGuardedDevirtualizationCandidateInfo->guardedMethodHandle;
        }
        methAttr = info.compCompHnd->getMethodAttribs(fncHandle);
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
    if (comp->compStressCompile(Compiler::STRESS_FORCE_INLINE, 0))
    {
        methAttr |= CORINFO_FLG_FORCEINLINE;
    }
#endif

    // Check for COMPlus_AggressiveInlining
    if (comp->compDoAggressiveInlining)
    {
        methAttr |= CORINFO_FLG_FORCEINLINE;
    }

    if (!(methAttr & CORINFO_FLG_FORCEINLINE))
    {
        // Don't bother inline blocks that are in the filter region
        if (bbInCatchHandlerILRange(currentBlock))
        {
            JITDUMP("\nWill not inline blocks that are in the catch handler region\n");

            inlineResult.NoteFatal(InlineObservation::CALLSITE_IS_WITHIN_CATCH);
            return;
        }

        if (bbInFilterILRange(currentBlock))
        {
            JITDUMP("\nWill not inline blocks that are in the filter region\n");

            inlineResult.NoteFatal(InlineObservation::CALLSITE_IS_WITHIN_FILTER);
            return;
        }
    }

    // Check if we tried to inline this method before

    if (methAttr & CORINFO_FLG_DONT_INLINE)
    {
        inlineResult.NoteFatal(InlineObservation::CALLEE_IS_NOINLINE);
        return;
    }

    // Cannot inline synchronized methods

    if (methAttr & CORINFO_FLG_SYNCH)
    {
        inlineResult.NoteFatal(InlineObservation::CALLEE_IS_SYNCHRONIZED);
        return;
    }

    // Check legality of PInvoke callsite (for inlining of marshalling code)

    if (methAttr & CORINFO_FLG_PINVOKE)
    {
        // See comment in impCheckForPInvokeCall
        BasicBlock* block = compIsForInlining() ? impInlineInfo->iciBlock : currentBlock;
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

    // If we're in an inlinee compiler, and have a return spill temp, and this inline candidate
    // is also a tail call candidate, it can use the same return spill temp.
    //
    if (compIsForInlining() && call->CanTailCall() &&
        (impInlineInfo->inlineCandidateInfo->preexistingSpillTemp != nullptr))
    {
        inlineCandidateInfo->preexistingSpillTemp = impInlineInfo->inlineCandidateInfo->preexistingSpillTemp;
        JITDUMP("Inline candidate [%06u] can share spill temp V%02u\n", dspTreeID(call),
                inlineCandidateInfo->preexistingSpillTemp);
    }

    // Mark the call node as inline candidate.
    call->gtFlags |= GTF_CALL_INLINE_CANDIDATE;

    // Let the strategy know there's another candidate.
    impInlineRoot()->m_inlineStrategy->NoteCandidate();

    // Since we're not actually inlining yet, and this call site is
    // still just an inline candidate, there's nothing to report.
    inlineResult.SetReported();
}

// Returns true if the given intrinsic will be implemented by target-specific instructions.
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
            return compOpportunisticallyDependsOn(InstructionSet_FMA);

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
            return compOpportunisticallyDependsOn(InstructionSet_AdvSimd);

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

// Returns true if the given intrinsic will be implemented by calling System.Math methods.
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
    return tree->IsIntrinsic() && IsMathIntrinsic(tree->AsIntrinsic()->GetIntrinsic());
}

//------------------------------------------------------------------------
// impDevirtualizeCall: Attempt to change a virtual vtable call into a
//   normal call
//
// Arguments:
//     call -- the call node to examine/modify
//     resolvedToken -- [IN] the resolved token used to create the call. Used for R2R.
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
//     If devirtualization succeeds and the call's this object is a
//     (boxed) value type, the jit will ask the EE for the unboxed entry
//     point. If this exists, the jit will invoke the unboxed entry
//     on the box payload. In addition if the boxing operation is
//     visible to the jit and the call is the only consmer of the box,
//     the jit will try analyze the box to see if the call can be instead
//     instead made on a local copy. If that is doable, the call is
//     updated to invoke the unboxed entry on the local copy and the
//     boxing operation is removed.
//
//     When guarded devirtualization is enabled, this method will mark
//     calls as guarded devirtualization candidates, if the type of `this`
//     is not exactly known, and there is a plausible guess for the type.
void Compiler::impDevirtualizeCall(GenTreeCall*            call,
                                   CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                   CORINFO_METHOD_HANDLE*  method,
                                   unsigned*               methodFlags,
                                   CORINFO_CONTEXT_HANDLE* pContextHandle,
                                   CORINFO_CONTEXT_HANDLE* pExactContextHandle,
                                   Importer*               importer,
                                   bool                    isExplicitTailCall,
                                   IL_OFFSETX              ilOffset)
{
    assert(call != nullptr);
    assert(method != nullptr);
    assert(methodFlags != nullptr);
    assert(pContextHandle != nullptr);

    // This should be a virtual vtable or virtual stub call.
    //
    assert(call->IsVirtual());

    // GDV can be done only while importing, it may need to append new statements and requires
    // the IndirectCallTransformer phase that runs only once when importing is complete.
    bool isLateDevirtualization = importer == nullptr;

    // Possibly instrument, if not optimizing.
    //
    if (opts.OptimizationDisabled())
    {
        // During importation, optionally flag this block as one that
        // contains calls requiring class profiling. Ideally perhaps
        // we'd just keep track of the calls themselves, so we don't
        // have to search for them later.
        //
        if ((call->gtCallType != CT_INDIRECT) && opts.jitFlags->IsSet(JitFlags::JIT_FLAG_BBINSTR) &&
            !opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT) && (JitConfig.JitClassProfiling() > 0) &&
            !isLateDevirtualization)
        {
            JITDUMP("\n ... marking [%06u] in " FMT_BB " for class profile instrumentation\n", dspTreeID(call),
                    importer->currentBlock->bbNum);
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
            importer->currentBlock->bbFlags |= BBF_HAS_CLASS_PROFILE;
        }

        return;
    }

#ifdef DEBUG
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
#ifdef DEBUG
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

    // Fetch information about the class that introduced the virtual method.
    CORINFO_CLASS_HANDLE baseClass        = info.compCompHnd->getMethodClass(baseMethod);
    const DWORD          baseClassAttribs = info.compCompHnd->getClassAttribs(baseClass);

    // Is the call an interface call?
    const bool isInterface = (baseClassAttribs & CORINFO_FLG_INTERFACE) != 0;

    // See what we know about the type of 'this' in the call.
    GenTree*             thisObj      = call->gtCallThisArg->GetNode()->gtEffectiveVal();
    bool                 isExact      = false;
    bool                 objIsNonNull = false;
    CORINFO_CLASS_HANDLE objClass     = gtGetClassHandle(thisObj, &isExact, &objIsNonNull);

    // Bail if we know nothing.
    if (objClass == NO_CLASS_HANDLE)
    {
        JITDUMP("\nimpDevirtualizeCall: no type available (op=%s)\n", GenTree::OpName(thisObj->OperGet()));

        // Don't try guarded devirtualiztion when we're doing late devirtualization.
        //
        if (isLateDevirtualization)
        {
            JITDUMP("No guarded devirt during late devirtualization\n");
            return;
        }

        importer->considerGuardedDevirtualization(call, ilOffset, isInterface, baseMethod, baseClass,
                                                  pContextHandle DEBUGARG(objClass) DEBUGARG("unknown"));

        return;
    }

    // If the objClass is sealed (final), then we may be able to devirtualize.
    const DWORD objClassAttribs = info.compCompHnd->getClassAttribs(objClass);
    const bool  objClassIsFinal = (objClassAttribs & CORINFO_FLG_FINAL) != 0;

#ifdef DEBUG
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

        JITDUMP("\nimpDevirtualizeCall: Trying to devirtualize %s call:\n"
                "    class for 'this' is %s%s (attrib %08x)\n"
                "    base method is %s::%s\n",
                callKind, objClassName, objClassNote, objClassAttribs, baseClassName, baseMethodName);
    }
#endif // DEBUG

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

        importer->considerGuardedDevirtualization(call, ilOffset, isInterface, baseMethod, baseClass,
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
    dvInfo.virtualMethod               = baseMethod;
    dvInfo.objClass                    = objClass;
    dvInfo.context                     = *pContextHandle;
    dvInfo.detail                      = CORINFO_DEVIRTUALIZATION_UNKNOWN;
    dvInfo.pResolvedTokenVirtualMethod = pResolvedToken;

    info.compCompHnd->resolveVirtualMethod(&dvInfo);

    CORINFO_METHOD_HANDLE   derivedMethod         = dvInfo.devirtualizedMethod;
    CORINFO_CONTEXT_HANDLE  exactContext          = dvInfo.exactContext;
    CORINFO_CLASS_HANDLE    derivedClass          = NO_CLASS_HANDLE;
    CORINFO_RESOLVED_TOKEN* pDerivedResolvedToken = &dvInfo.resolvedTokenDevirtualizedMethod;

    if (derivedMethod != nullptr)
    {
        assert(exactContext != nullptr);
        assert(((size_t)exactContext & CORINFO_CONTEXTFLAGS_MASK) == CORINFO_CONTEXTFLAGS_CLASS);
        derivedClass = (CORINFO_CLASS_HANDLE)((size_t)exactContext & ~CORINFO_CONTEXTFLAGS_MASK);
    }

    DWORD derivedMethodAttribs = 0;
    bool  derivedMethodIsFinal = false;
    bool  canDevirtualize      = false;

#ifdef DEBUG
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
        JITDUMP("--- no derived method: %s\n", devirtualizationDetailToString(dvInfo.detail));
    }
    else
    {
        // Fetch method attributes to see if method is marked final.
        derivedMethodAttribs = info.compCompHnd->getMethodAttribs(derivedMethod);
        derivedMethodIsFinal = ((derivedMethodAttribs & CORINFO_FLG_FINAL) != 0);

#ifdef DEBUG
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
            JITDUMPTREE(call, "    devirt to %s::%s -- %s\n", derivedClassName, derivedMethodName, note);
        }
#endif // DEBUG

        canDevirtualize = isExact || objClassIsFinal || (!isInterface && derivedMethodIsFinal);
    }

    // We still might be able to do a guarded devirtualization.
    // Note the call might be an interface call or a virtual call.
    //
    if (!canDevirtualize)
    {
        JITDUMP("    Class not final or exact%s\n", isInterface ? "" : ", and method not final");

#ifdef DEBUG
        // If we know the object type exactly, we generally expect we can devirtualize.
        // (don't when doing late devirt as we won't have an owner type (yet))
        //
        if (!isLateDevirtualization && (isExact || objClassIsFinal) && JitConfig.JitNoteFailedExactDevirtualization())
        {
            printf("@@@ Exact/Final devirt failure in %s at [%06u] $ %s\n", info.compFullName, dspTreeID(call),
                   devirtualizationDetailToString(dvInfo.detail));
        }
#endif

        // Don't try guarded devirtualiztion if we're doing late devirtualization.
        //
        if (isLateDevirtualization)
        {
            JITDUMP("No guarded devirt during late devirtualization\n");
            return;
        }

        importer->considerGuardedDevirtualization(call, ilOffset, isInterface, baseMethod, baseClass,
                                                  pContextHandle DEBUGARG(objClass) DEBUGARG(objClassName));
        return;
    }

    // All checks done. Time to transform the call.
    //
    // We should always have an exact class context.
    //
    // Note that wouldnt' be true if the runtime side supported array interface devirt,
    // the resulting method would be a generic method of the non-generic SZArrayHelper class.
    //
    assert(canDevirtualize);

    JITDUMP("    %s; can devirtualize\n", note);

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

#ifdef DEBUG
    JITDUMPTREE(call, "... after devirt...\n");

    if (doPrint)
    {
        printf("Devirtualized %s call to %s:%s; now direct call to %s:%s [%s]\n", callKind, baseClassName,
               baseMethodName, derivedClassName, derivedMethodName, note);
    }

    // If we successfully devirtualized based on an exact or final class,
    // and we have dynamic PGO data describing the likely class, make sure they agree.
    //
    // If pgo source is not dynamic we may see likely classes from other versions of this code
    // where types had different properties.
    //
    // If method is an inlinee we may be specializing to a class that wasn't seen at runtime.
    //
    const bool canSensiblyCheck =
        (isExact || objClassIsFinal) && (fgPgoSource == ICorJitInfo::PgoSource::Dynamic) && !compIsForInlining();
    if (JitConfig.JitCrossCheckDevirtualizationAndPGO() && canSensiblyCheck)
    {
        unsigned likelihood      = 0;
        unsigned numberOfClasses = 0;

        CORINFO_CLASS_HANDLE likelyClass =
            getLikelyClass(fgPgoSchema, fgPgoSchemaCount, fgPgoData, ilOffset, &likelihood, &numberOfClasses);

        if (likelyClass != NO_CLASS_HANDLE)
        {
            // PGO had better agree the class we devirtualized to is plausible.
            //
            if (likelyClass != derivedClass)
            {
                // Managed type system may report different addresses for a class handle
                // at different times....?
                //
                // Also, AOT may have a more nuanced notion of class equality.
                //
                if (!opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT))
                {
                    bool mismatch = true;

                    // derivedClass will be the introducer of derived method, so it's possible
                    // likelyClass is a non-overriding subclass. Check up the hierarchy.
                    //
                    CORINFO_CLASS_HANDLE parentClass = likelyClass;
                    while (parentClass != NO_CLASS_HANDLE)
                    {
                        if (parentClass == derivedClass)
                        {
                            mismatch = false;
                            break;
                        }

                        parentClass = info.compCompHnd->getParentType(parentClass);
                    }

                    if (mismatch || (numberOfClasses != 1) || (likelihood != 100))
                    {
                        printf("@@@ Likely %p (%s) != Derived %p (%s) [n=%u, l=%u, il=%u] in %s \n", likelyClass,
                               eeGetClassName(likelyClass), derivedClass, eeGetClassName(derivedClass), numberOfClasses,
                               likelihood, ilOffset, info.compFullName);
                    }

                    assert(!(mismatch || (numberOfClasses != 1) || (likelihood != 100)));
                }
            }
        }
    }
#endif // DEBUG

    // If the 'this' object is a value class, see if we can rework the call to invoke the
    // unboxed entry. This effectively inlines the normally un-inlineable wrapper stub
    // and exposes the potentially inlinable unboxed entry method.
    //
    // We won't optimize explicit tail calls, as ensuring we get the right tail call info
    // is tricky (we'd need to pass an updated sig and resolved token back to some callers).
    //
    // Note we may not have a derived class in some cases (eg interface call on an array)
    //
    if (info.compCompHnd->isValueClass(derivedClass))
    {
        if (isExplicitTailCall)
        {
            JITDUMP("Have a direct explicit tail call to boxed entry point; can't optimize further\n");
        }
        else
        {
            JITDUMP("Have a direct call to boxed entry point. Trying to optimize to call an unboxed entry point\n");

            // Note for some shared methods the unboxed entry point requires an extra parameter.
            bool                  requiresInstMethodTableArg = false;
            CORINFO_METHOD_HANDLE unboxedEntryMethod =
                info.compCompHnd->getUnboxedEntry(derivedMethod, &requiresInstMethodTableArg);

            if (unboxedEntryMethod != nullptr)
            {
                bool optimizedTheBox = false;

                // If the 'this' object is a local box, see if we can revise things
                // to not require boxing.
                //
                if (thisObj->IsBox() && !isExplicitTailCall)
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
                        // Perform a trial box removal and ask for the type handle tree that fed the box.
                        //
                        JITDUMP("Unboxed entry needs method table arg...\n");
                        GenTree* methodTableArg =
                            gtTryRemoveBoxUpstreamEffects(thisObj->AsBox(), BR_DONT_REMOVE_WANT_TYPE_HANDLE);

                        if (methodTableArg != nullptr)
                        {
                            // If that worked, turn the box into a copy to a local var
                            //
                            JITDUMP("Found suitable method table arg tree [%06u]\n", dspTreeID(methodTableArg));
                            GenTree* localCopyThis =
                                gtTryRemoveBoxUpstreamEffects(thisObj->AsBox(), BR_MAKE_LOCAL_COPY);

                            if (localCopyThis != nullptr)
                            {
                                // Pass the local var as this and the type handle as a new arg
                                //
                                JITDUMP("Success! invoking unboxed entry point on local copy, and passing method table "
                                        "arg\n");
                                call->gtCallThisArg = gtNewCallArgs(localCopyThis);
                                call->gtCallMoreFlags |= GTF_CALL_M_UNBOXED;

                                if (call->gtCallArgs == nullptr)
                                {
                                    call->gtCallArgs = gtNewCallArgs(methodTableArg);
                                }
                                else
                                {
#ifdef TARGET_X86
                                    GenTreeCall::Use* beforeArg = call->gtCallArgs;

                                    while (beforeArg->GetNext() != nullptr)
                                    {
                                        beforeArg = beforeArg->GetNext();
                                    }

                                    beforeArg->SetNext(gtNewCallArgs(methodTableArg));
#else
                                    // If there's a ret buf, the method table is the second arg.
                                    if (call->HasRetBufArg())
                                    {
                                        gtInsertNewCallArgAfter(methodTableArg, call->gtCallArgs);
                                    }
                                    else
                                    {
                                        call->gtCallArgs = gtPrependNewCallArg(methodTableArg, call->gtCallArgs);
                                    }
#endif
                                }

                                call->gtCallMethHnd   = unboxedEntryMethod;
                                derivedMethod         = unboxedEntryMethod;
                                pDerivedResolvedToken = &dvInfo.resolvedTokenDevirtualizedUnboxedMethod;

                                // Method attributes will differ because unboxed entry point is shared
                                //
                                const DWORD unboxedMethodAttribs =
                                    info.compCompHnd->getMethodAttribs(unboxedEntryMethod);
                                JITDUMP("Updating method attribs from 0x%08x to 0x%08x\n", derivedMethodAttribs,
                                        unboxedMethodAttribs);
                                derivedMethodAttribs = unboxedMethodAttribs;
                                optimizedTheBox      = true;
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
                        GenTree* localCopyThis = gtTryRemoveBoxUpstreamEffects(thisObj->AsBox(), BR_MAKE_LOCAL_COPY);

                        if (localCopyThis != nullptr)
                        {
                            JITDUMP("Success! invoking unboxed entry point on local copy\n");
                            call->gtCallThisArg = gtNewCallArgs(localCopyThis);
                            call->gtCallMethHnd = unboxedEntryMethod;
                            call->gtCallMoreFlags |= GTF_CALL_M_UNBOXED;
                            derivedMethod         = unboxedEntryMethod;
                            pDerivedResolvedToken = &dvInfo.resolvedTokenDevirtualizedUnboxedMethod;

                            optimizedTheBox = true;
                        }
                        else
                        {
                            JITDUMP("Sorry, failed to undo the box\n");
                        }
                    }

                    if (optimizedTheBox)
                    {

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
                }

                if (!optimizedTheBox)
                {
                    // If we get here, we have a boxed value class that either wasn't boxed
                    // locally, or was boxed locally but we were unable to remove the box for
                    // various reasons.
                    //
                    // We can still update the call to invoke the unboxed entry, if the
                    // boxed value is simple.
                    //
                    if (requiresInstMethodTableArg)
                    {
                        // Get the method table from the boxed object.
                        //
                        GenTree* const thisArg       = call->gtCallThisArg->GetNode();
                        GenTree* const clonedThisArg = gtClone(thisArg);

                        if (clonedThisArg == nullptr)
                        {
                            JITDUMP(
                                "unboxed entry needs MT arg, but `this` was too complex to clone. Deferring update.\n");
                        }
                        else
                        {
                            JITDUMP("revising call to invoke unboxed entry with additional method table arg\n");

                            GenTree* const methodTableArg = gtNewMethodTableLookup(clonedThisArg);

                            // Update the 'this' pointer to refer to the box payload
                            //
                            GenTree* const payloadOffset = gtNewIconNode(TARGET_POINTER_SIZE, TYP_I_IMPL);
                            GenTree* const boxPayload    = gtNewOperNode(GT_ADD, TYP_BYREF, thisArg, payloadOffset);

                            call->gtCallThisArg = gtNewCallArgs(boxPayload);
                            call->gtCallMethHnd = unboxedEntryMethod;
                            call->gtCallMoreFlags |= GTF_CALL_M_UNBOXED;

                            // Method attributes will differ because unboxed entry point is shared
                            //
                            const DWORD unboxedMethodAttribs = info.compCompHnd->getMethodAttribs(unboxedEntryMethod);
                            JITDUMP("Updating method attribs from 0x%08x to 0x%08x\n", derivedMethodAttribs,
                                    unboxedMethodAttribs);
                            derivedMethod         = unboxedEntryMethod;
                            pDerivedResolvedToken = &dvInfo.resolvedTokenDevirtualizedUnboxedMethod;
                            derivedMethodAttribs  = unboxedMethodAttribs;

                            if (call->gtCallArgs == nullptr)
                            {
                                call->gtCallArgs = gtNewCallArgs(methodTableArg);
                            }
                            else
                            {
#ifdef TARGET_X86
                                GenTreeCall::Use* beforeArg = call->gtCallArgs;

                                while (beforeArg->GetNext() != nullptr)
                                {
                                    beforeArg = beforeArg->GetNext();
                                }

                                beforeArg->SetNext(gtNewCallArgs(methodTableArg));
#else
                                // If there's a ret buf, the method table is the second arg.
                                if (call->HasRetBufArg())
                                {
                                    gtInsertNewCallArgAfter(methodTableArg, call->gtCallArgs);
                                }
                                else
                                {
                                    call->gtCallArgs = gtPrependNewCallArg(methodTableArg, call->gtCallArgs);
                                }
#endif
                            }
                        }
                    }
                    else
                    {
                        JITDUMP("revising call to invoke unboxed entry\n");

                        GenTree* const thisArg       = call->gtCallThisArg->GetNode();
                        GenTree* const payloadOffset = gtNewIconNode(TARGET_POINTER_SIZE, TYP_I_IMPL);
                        GenTree* const boxPayload    = gtNewOperNode(GT_ADD, TYP_BYREF, thisArg, payloadOffset);

                        call->gtCallThisArg = gtNewCallArgs(boxPayload);
                        call->gtCallMethHnd = unboxedEntryMethod;
                        call->gtCallMoreFlags |= GTF_CALL_M_UNBOXED;
                        derivedMethod         = unboxedEntryMethod;
                        pDerivedResolvedToken = &dvInfo.resolvedTokenDevirtualizedUnboxedMethod;
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
        // side and acquires the actual symbol to call so we need to call it here.

        // Look up the new call info.
        CORINFO_CALL_INFO derivedCallInfo;
        eeGetCallInfo(pDerivedResolvedToken, nullptr, CORINFO_CALLINFO_ALLOWINSTPARAM, &derivedCallInfo);

        call->gtCallMoreFlags &= ~GTF_CALL_M_VIRTSTUB_REL_INDIRECT;
#ifdef TARGET_ARMARCH
        call->gtCallMoreFlags &= ~GTF_CALL_M_R2R_REL_INDIRECT;
#endif
        call->setEntryPoint(derivedCallInfo.codePointerLookup.constLookup);
    }
#endif // FEATURE_READYTORUN_COMPILER
}

void Compiler::impLateDevirtualizeCall(GenTreeCall* call)
{
    JITDUMPTREE(call, "**** Late devirt opportunity\n");

    CORINFO_METHOD_HANDLE  method           = call->GetMethodHandle();
    unsigned               methodFlags      = 0;
    CORINFO_CONTEXT_HANDLE context          = nullptr;
    bool                   explicitTailCall = (call->gtCallMoreFlags & GTF_CALL_M_EXPLICIT_TAILCALL) != 0;

    impDevirtualizeCall(call, nullptr, &method, &methodFlags, &context, nullptr, nullptr, explicitTailCall);
}

void Compiler::impLateDevirtualizeCall(GenTreeCall*            call,
                                       InlineCandidateInfo*    inlineInfo,
                                       CORINFO_METHOD_HANDLE*  methodHnd,
                                       CORINFO_CONTEXT_HANDLE* context)
{
    *methodHnd                  = call->GetMethodHandle();
    unsigned methodFlags        = info.compCompHnd->getMethodAttribs(*methodHnd);
    *context                    = inlineInfo->exactContextHnd;
    const bool explicitTailCall = (call->gtCallMoreFlags & GTF_CALL_M_EXPLICIT_TAILCALL) != 0;

    impDevirtualizeCall(call, nullptr, methodHnd, &methodFlags, context, nullptr, nullptr, explicitTailCall);
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

// Iterate through call arguments and spill RET_EXPR to local variables.
class SpillRetExprHelper final : public GenTreeVisitor<SpillRetExprHelper>
{
    Importer* importer;

public:
    enum
    {
        DoPreOrder = true
    };

    SpillRetExprHelper(Importer* importer) : GenTreeVisitor<SpillRetExprHelper>(importer->comp), importer(importer)
    {
    }

    void StoreRetExprResultsInArgs(GenTreeCall* call)
    {
        // TODO-MIKE-Review: This seems to mess up arg evaluation order...

        for (GenTreeCall::Use& use : call->Args())
        {
            WalkTree(&use.NodeRef(), call);
        }

        if (call->gtCallThisArg != nullptr)
        {
            WalkTree(&call->gtCallThisArg->NodeRef(), call);
        }
    }

    fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
    {
        GenTree* tree = *use;

        if ((tree->gtFlags & GTF_CALL) == 0)
        {
            // Trees with ret_expr are marked as GTF_CALL.
            return Compiler::WALK_SKIP_SUBTREES;
        }

        if (tree->IsRetExpr())
        {
            StoreRetExprAsLocalVar(use);
        }

        return Compiler::WALK_CONTINUE;
    }

    void StoreRetExprAsLocalVar(GenTree** use)
    {
        GenTreeRetExpr* retExpr = (*use)->AsRetExpr();

        LclVarDsc* lcl = m_compiler->lvaAllocTemp(true DEBUGARG("RET_EXPR temp"));
        JITDUMP("Storing return expression [%06u] to a local var V%02u.\n", retExpr->GetID(), lcl->GetLclNum());
        importer->impAppendTempStore(lcl, retExpr, retExpr->GetLayout(), Importer::CHECK_SPILL_NONE);
        *use = m_compiler->gtNewLclvNode(lcl, retExpr->GetType());

        if (retExpr->TypeIs(TYP_REF))
        {
            JITDUMP("Marked V%02u as a single def temp\n", lcl->GetLclNum());

            assert(!lcl->lvSingleDef);
            lcl->lvSingleDef = true;

            bool isExact   = false;
            bool isNonNull = false;
            if (CORINFO_CLASS_HANDLE retClsHnd = m_compiler->gtGetClassHandle(retExpr, &isExact, &isNonNull))
            {
                m_compiler->lvaSetClass(lcl, retClsHnd, isExact);
            }
        }
    }
};

//------------------------------------------------------------------------
// addFatPointerCandidate: mark the call and the method, that they have a fat pointer candidate.
//                         Spill ret_expr in the call node, because they can't be cloned.
//
// Arguments:
//    call - fat calli candidate
//
void Importer::addFatPointerCandidate(GenTreeCall* call)
{
    JITDUMP("Marking call [%06u] as fat pointer candidate\n", dspTreeID(call));
    comp->setMethodHasFatPointer();
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
void Importer::considerGuardedDevirtualization(
    GenTreeCall*            call,
    IL_OFFSETX              ilOffset,
    bool                    isInterface,
    CORINFO_METHOD_HANDLE   baseMethod,
    CORINFO_CLASS_HANDLE    baseClass,
    CORINFO_CONTEXT_HANDLE* pContextHandle DEBUGARG(CORINFO_CLASS_HANDLE objClass) DEBUGARG(const char* objClassName))
{
    INDEBUG(const char* callKind = isInterface ? "interface" : "virtual");
    JITDUMP("Considering guarded devirtualization at IL offset %u (0x%x)\n", ilOffset, ilOffset);

    // We currently only get likely class guesses when there is PGO data
    // with class profiles.
    //
    if (comp->fgPgoClassProfiles == 0)
    {
        JITDUMP("Not guessing for class: no class profile pgo data, or pgo disabled\n");
        return;
    }

    // See if there's a likely guess for the class.
    //
    const unsigned likelihoodThreshold = isInterface ? 25 : 30;
    unsigned       likelihood          = 0;
    unsigned       numberOfClasses     = 0;

    CORINFO_CLASS_HANDLE likelyClass = NO_CLASS_HANDLE;

    bool doRandomDevirt = false;

#ifdef DEBUG
    // Optional stress mode to pick a random known class, rather than
    // the most likely known class.
    //
    doRandomDevirt = JitConfig.JitRandomGuardedDevirtualization() != 0;

    if (doRandomDevirt)
    {
        // Reuse the random inliner's random state.
        //
        CLRRandom* const random =
            impInlineRoot()->m_inlineStrategy->GetRandom(JitConfig.JitRandomGuardedDevirtualization());
        likelihood      = 100;
        numberOfClasses = 1;
        likelyClass =
            Compiler::getRandomClass(comp->fgPgoSchema, comp->fgPgoSchemaCount, comp->fgPgoData, ilOffset, random);
    }
    else
#endif
    {
        likelyClass = getLikelyClass(comp->fgPgoSchema, comp->fgPgoSchemaCount, comp->fgPgoData, ilOffset, &likelihood,
                                     &numberOfClasses);
    }

    if (likelyClass == NO_CLASS_HANDLE)
    {
        JITDUMP("No likely class, sorry\n");
        return;
    }

    JITDUMP("%s class for %p (%s) is %p (%s) [likelihood:%u classes seen:%u]\n", doRandomDevirt ? "Random" : "Likely",
            dspPtr(objClass), objClassName, likelyClass, eeGetClassName(likelyClass), likelihood, numberOfClasses);

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
    dvInfo.virtualMethod               = baseMethod;
    dvInfo.objClass                    = likelyClass;
    dvInfo.context                     = *pContextHandle;
    dvInfo.exactContext                = *pContextHandle;
    dvInfo.pResolvedTokenVirtualMethod = nullptr;

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
void Importer::addGuardedDevirtualizationCandidate(GenTreeCall*          call,
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
    if (currentBlock->isRunRarely() || opts.OptimizationDisabled())
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
    GuardedDevirtualizationCandidateInfo* pInfo = new (comp, CMK_Inlining) InlineCandidateInfo;

    pInfo->guardedMethodHandle             = methodHandle;
    pInfo->guardedMethodUnboxedEntryHandle = nullptr;
    pInfo->guardedClassHandle              = classHandle;
    pInfo->likelihood                      = likelihood;
    pInfo->requiresInstMethodTableArg      = false;

    // If the guarded class is a value class, look for an unboxed entry point.
    //
    if ((classAttr & CORINFO_FLG_VALUECLASS) != 0)
    {
        JITDUMP("    ... class is a value class, looking for unboxed entry\n");
        bool                  requiresInstMethodTableArg = false;
        CORINFO_METHOD_HANDLE unboxedEntryMethodHandle =
            info.compCompHnd->getUnboxedEntry(methodHandle, &requiresInstMethodTableArg);

        if (unboxedEntryMethodHandle != nullptr)
        {
            JITDUMP("    ... updating GDV candidate with unboxed entry info\n");
            pInfo->guardedMethodUnboxedEntryHandle = unboxedEntryMethodHandle;
            pInfo->requiresInstMethodTableArg      = requiresInstMethodTableArg;
        }
    }

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

void Importer::addExpRuntimeLookupCandidate(GenTreeCall* call)
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
bool Importer::impCanSkipCovariantStoreCheck(GenTree* value, GenTree* array)
{
    // We should only call this when optimizing.
    assert(opts.OptimizationEnabled());

    // Check for storing to the same array, ie. arrLcl[i] = arrLcl[j]
    if (value->OperIs(GT_IND) && value->AsIndir()->GetAddr()->IsIndexAddr() && array->OperIs(GT_LCL_VAR))
    {
        GenTree* valueIndex = value->AsIndir()->GetAddr()->AsIndexAddr()->GetArray();
        if (valueIndex->OperIs(GT_LCL_VAR))
        {
            LclVarDsc* valueLcl = valueIndex->AsLclVar()->GetLcl();
            LclVarDsc* arrayLcl = array->AsLclVar()->GetLcl();

            // TODO-MIKE-Cleanup: Checking IsAddressExposed here is nonsense, it's rarely set
            // during import. Besides, the check is probably overly conservative, there's a
            // reasonable good chance that index trees do not interfere with AX locals. During
            // import stores typically end up in their own statements rather than being hidden
            // under COMMAs so the only source of interference are probably calls. Then we
            // could check for GTF_CALL and lvHasLdAddrOp.

            if ((valueLcl == arrayLcl) && !arrayLcl->IsAddressExposed())
            {
                JITDUMP("\nstelem of ref from same array: skipping covariant store check\n");
                return true;
            }
        }
    }

    // Check for storing of null.
    if (value->OperIs(GT_CNS_INT))
    {
        assert(value->TypeIs(TYP_REF));

        if (value->AsIntCon()->GetValue() == 0)
        {
            JITDUMP("\nstelem of null: skipping covariant store check\n");
            return true;
        }

        // Non-0 const refs can only occur with frozen objects
        assert(value->IsIntCon(HandleKind::String));
        assert(comp->doesMethodHaveFrozenString() ||
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

void Importer::impImportInitObj(GenTree* dstAddr, ClassLayout* layout)
{
    // We have to spill GLOB_REFs even if the destination is a local,
    // we've got an address so the local is "address taken".
    impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("INITOBJ stack spill temp"));

    GenTree* initValue;

#ifdef FEATURE_SIMD
    if (layout->IsVector())
    {
        initValue = gtNewZeroSimdHWIntrinsicNode(layout);
    }
    else
#endif
    {
        initValue = gtNewIconNode(0);
    }

    if (dstAddr->OperIs(GT_LCL_ADDR))
    {
        // Currently the importer doesn't generate local field addresses.
        assert(dstAddr->AsLclAddr()->GetLclOffs() == 0);

        LclVarDsc* lcl = dstAddr->AsLclAddr()->GetLcl();

        if (varTypeIsStruct(lcl->GetType()) && (layout->GetSize() >= lcl->GetLayout()->GetSize()))
        {
            impSpillNoneAppendTree(comp->gtNewLclStore(lcl, lcl->GetType(), initValue));

            return;
        }
    }

    GenTreeIndir* store = gtNewObjNode(layout, dstAddr);
    gtInitStructIndStore(store, initValue);
    impSpillNoneAppendTree(store);
}

void Importer::impImportCpObj(GenTree* dstAddr, GenTree* srcAddr, ClassLayout* layout)
{
    // We have to spill GLOB_REFs even if the destination is a local,
    // we've got an address so the local is "address taken".
    impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("CPOBJ stack spill temp"));

    GenTree* src = nullptr;

    if (srcAddr->OperIs(GT_LCL_ADDR))
    {
        // Currently the importer doesn't generate local field addresses.
        assert(srcAddr->AsLclAddr()->GetLclOffs() == 0);

        LclVarDsc* lcl = srcAddr->AsLclAddr()->GetLcl();

        src = srcAddr;
        src->SetOper(GT_LCL_VAR);
        src->AsLclVar()->SetLcl(lcl);
        src->SetType(lcl->GetType());
    }
    else
    {
        src = gtNewObjNode(layout, srcAddr);
    }

    GenTree* dst = nullptr;

    if (dstAddr->OperIs(GT_LCL_ADDR))
    {
        // Currently the importer doesn't generate local field addresses.
        assert(dstAddr->AsLclAddr()->GetLclOffs() == 0);

        LclVarDsc* lcl = dstAddr->AsLclAddr()->GetLcl();

        if (varTypeIsStruct(lcl->GetType()) && !lcl->IsImplicitByRefParam() && (lcl->GetLayout() == layout))
        {
            impSpillNoneAppendTree(comp->gtNewLclStore(lcl, lcl->GetType(), src));

            return;
        }
    }

    // TODO-MIKE-CQ: This should probably be removed, it's here only because
    // a previous implementation (gtNewBlkOpNode) was setting it. And it
    // probably blocks SIMD tree CSEing.
    src->gtFlags |= GTF_DONT_CSE;

    GenTreeIndir* store = gtNewObjNode(layout, dstAddr);
    gtInitStructIndStore(store, src);
    impSpillNoneAppendTree(store);
}

void Importer::impImportInitBlk(unsigned prefixFlags)
{
    // TODO-MIKE-Review: Currently INITBLK ignores the unaligned prefix.

    GenTree* size      = impPopStack().val;
    GenTree* initValue = impPopStack().val;
    GenTree* dstAddr   = impPopStack().val;
    GenTree* init;

    GenTreeIntCon* sizeIntCon = size->IsIntCon();

    if ((sizeIntCon == nullptr) || (sizeIntCon->GetUInt32Value() == 0))
    {
        init = new (comp, GT_INIT_BLK) GenTreeDynBlk(GT_INIT_BLK, dstAddr, initValue, size);

        if ((prefixFlags & PREFIX_VOLATILE) != 0)
        {
            init->AsDynBlk()->SetVolatile();
        }
    }
    else
    {
        if (!initValue->IsIntegralConst(0))
        {
            initValue = gtNewOperNode(GT_INIT_VAL, TYP_STRUCT, initValue);
        }

        ClassLayout* layout = typGetBlkLayout(sizeIntCon->GetUInt32Value());
        GenTreeBlk*  store  = new (comp, GT_STORE_BLK) GenTreeBlk(dstAddr, initValue, layout);

        if ((prefixFlags & PREFIX_VOLATILE) != 0)
        {
            store->SetVolatile();
        }

        init = store;
    }

    impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("INITBLK stack spill temp"));
    impSpillNoneAppendTree(init);
}

void Importer::impImportCpBlk(unsigned prefixFlags)
{
    // TODO-MIKE-Review: Currently CPBLK ignores the unaligned prefix.

    GenTree* size    = impPopStack().val;
    GenTree* srcAddr = impPopStack().val;
    GenTree* dstAddr = impPopStack().val;
    GenTree* copy;

    GenTreeIntCon* sizeIntCon = size->IsIntCon();

    if ((sizeIntCon == nullptr) || (sizeIntCon->GetUInt32Value() == 0))
    {
        copy = new (comp, GT_COPY_BLK) GenTreeDynBlk(GT_COPY_BLK, dstAddr, srcAddr, size);

        if ((prefixFlags & PREFIX_VOLATILE) != 0)
        {
            copy->AsDynBlk()->SetVolatile();
        }
    }
    else
    {
        ClassLayout* layout = typGetBlkLayout(sizeIntCon->GetUInt32Value());
        GenTreeBlk*  load   = new (comp, GT_BLK) GenTreeBlk(srcAddr, layout);
        GenTreeBlk*  store  = new (comp, GT_STORE_BLK) GenTreeBlk(dstAddr, load, layout);

        if ((prefixFlags & PREFIX_VOLATILE) != 0)
        {
            store->SetVolatile();
            load->SetVolatile();
        }

        copy = store;
    }

    impSpillSideEffects(GTF_GLOB_EFFECT, CHECK_SPILL_ALL DEBUGARG("CPBLK stack spill temp"));
    impSpillNoneAppendTree(copy);
}

GenTree* Importer::impImportPop(BasicBlock* block)
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

        if (op1->OperIs(GT_IND, GT_OBJ) && !op1->AsIndir()->IsVolatile())
        {
            GenTree* addr = op1->AsIndir()->GetAddr();

            while (GenTreeFieldAddr* field = addr->IsFieldAddr())
            {
                addr = field->GetAddr();
            }

            // Don't create NULLCHECK(INDEX_ADDR), INDEX_ADDR already checks for null.
            if (addr->OperIs(GT_INDEX_ADDR))
            {
                op1 = addr;
            }
            else
            {
                gtChangeOperToNullCheck(op1);
                op1->AsIndir()->SetAddr(addr);
            }
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

GenTree* Importer::CreateStaticFieldTlsAccess(OPCODE                    opcode,
                                              CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                              const CORINFO_FIELD_INFO& fieldInfo)
{
    assert((opcode == CEE_LDSFLD) || (opcode == CEE_STSFLD) || (opcode == CEE_LDSFLDA));

#ifndef WINDOWS_X86_ABI
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
        dllRef = gtNewIndOfIconHandleNode(TYP_I_IMPL, reinterpret_cast<size_t>(pIdAddr), HandleKind::ConstData, true);

        // Next we multiply by 4
        dllRef = gtNewOperNode(GT_MUL, TYP_I_IMPL, dllRef, gtNewIconNode(4, TYP_I_IMPL));
    }

    constexpr size_t WIN32_TLS_SLOTS = 0x2C; // Offset from fs:[0] where the pointer to the slots resides

    // Mark this ICON as a TLS_HDL, codegen will use FS:[cns]

    GenTree* addr = gtNewIconHandleNode(WIN32_TLS_SLOTS, HandleKind::TLS);

    if ((fieldInfo.fieldFlags & CORINFO_FLG_FIELD_INITCLASS) != 0)
    {
        addr->gtFlags |= GTF_ICON_INITCLASS;
    }

    addr = gtNewIndir(TYP_I_IMPL, addr);
    addr->gtFlags |= GTF_IND_NONFAULTING | GTF_IND_INVARIANT;

    if (dllRef != nullptr)
    {
        addr = gtNewOperNode(GT_ADD, TYP_I_IMPL, addr, dllRef);
    }

    addr = gtNewIndir(TYP_I_IMPL, addr);

    if (fieldInfo.offset != 0)
    {
        // Add the TLS static field offset. Don't bother recording a field sequence
        // for the field offset as it won't be recognized during value numbering.
        addr = gtNewOperNode(GT_ADD, TYP_I_IMPL, addr, gtNewIconNode(fieldInfo.offset, TYP_I_IMPL));
    }
#endif // WINDOWS_X86_ABI

    if (opcode == CEE_LDSFLDA)
    {
        return addr;
    }

    var_types type = CorTypeToVarType(fieldInfo.fieldType);
    GenTree*  indir;

    if (varTypeIsStruct(type))
    {
        indir = gtNewObjNode(typGetObjLayout(fieldInfo.structType), addr);
    }
    else
    {
        indir = gtNewIndir(type, addr);
    }

    indir->gtFlags |= GTF_GLOB_REF | GTF_IND_NONFAULTING;
    return indir;
}

// Returns true if the given tree contains a use of a local
bool Compiler::impHasLclRef(GenTree* tree, LclVarDsc* lcl)
{
    while (tree->OperIsUnary() || (tree->OperIsBinary() && (tree->AsOp()->gtOp2 == nullptr)))
    {
        tree = tree->AsUnOp()->gtOp1;

        if (tree == nullptr)
        {
            return false;
        }
    }

    if (tree->OperIsBinary())
    {
        GenTreeOp* op = tree->AsOp();

        return ((op->gtOp1 != nullptr) && impHasLclRef(op->gtOp1, lcl)) || impHasLclRef(op->gtOp2, lcl);
    }

    if (tree->OperIsLeaf())
    {
        // TODO-MIKE-Review: This is dubious, LCL_ADDR isn't really an "use"
        // of a local variable. Though it depends on what exactly "use" means...
        if (tree->OperIs(GT_LCL_ADDR))
        {
            return tree->AsLclAddr()->GetLcl() == lcl;
        }

        if (tree->OperIs(GT_LCL_VAR, GT_LCL_FLD))
        {
            return tree->AsLclVarCommon()->GetLcl() == lcl;
        }

        if (tree->OperIs(GT_RET_EXPR))
        {
            return impHasLclRef(tree->AsRetExpr()->GetRetExpr(), lcl);
        }

        return false;
    }

    if (GenTreeBoundsChk* boundsChk = tree->IsBoundsChk())
    {
        return impHasLclRef(boundsChk->GetIndex(), lcl) || impHasLclRef(boundsChk->GetLength(), lcl);
    }

    if (GenTreeTernaryOp* ternary = tree->IsTernaryOp())
    {
        for (unsigned i = 0; i < 3; i++)
        {
            if (impHasLclRef(ternary->GetOp(i), lcl))
            {
                return true;
            }
        }

        return false;
    }

    if (GenTreeCall* call = tree->IsCall())
    {
        // We haven't morphed calls yet.
        assert(call->gtCallLateArgs == nullptr);
        assert(call->gtControlExpr == nullptr);

        if (call->gtCallThisArg != nullptr)
        {
            if (impHasLclRef(call->gtCallThisArg->GetNode(), lcl))
            {
                return true;
            }
        }

        for (GenTreeCall::Use& use : call->Args())
        {
            if (impHasLclRef(use.GetNode(), lcl))
            {
                return true;
            }
        }

        if (call->IsIndirectCall())
        {
            GenTree* cookie = call->gtCallCookie;

            // PInvoke-calli cookie is a constant, or constant address indirection.
            assert((cookie == nullptr) || cookie->IsIntCon() ||
                   (cookie->OperIs(GT_IND) && cookie->AsIndir()->GetAddr()->IsIntCon()));

            return impHasLclRef(call->gtCallAddr, lcl);
        }

        return false;
    }

    if (GenTreeArrElem* arrElem = tree->IsArrElem())
    {
        for (unsigned i = 0; i < arrElem->gtArrRank; i++)
        {
            if (impHasLclRef(arrElem->gtArrInds[i], lcl))
            {
                return true;
            }
        }

        return impHasLclRef(arrElem->gtArrObj, lcl);
    }

#ifdef FEATURE_HW_INTRINSICS
    if (GenTreeHWIntrinsic* intrinsic = tree->IsHWIntrinsic())
    {
        for (GenTreeHWIntrinsic::Use& use : intrinsic->Uses())
        {
            if (impHasLclRef(use.GetNode(), lcl))
            {
                return true;
            }
        }

        return false;
    }
#endif

    INDEBUG(gtDispTree(tree);)
    unreached();
}

// Check if the tree references any address taken locals.
//
// Note that "address taken" is far more conservative than "address exposed"
// and as such this should only be used before we determine which locals are
// address exposed - typically during IL import and inlining. Beyond that,
// GTF_GLOB_REF should be used instead as it is set on any tree that uses
// address exposed locals.
//
bool Compiler::impHasAddressTakenLocals(GenTree* tree)
{
    auto visitor = [](GenTree** use, fgWalkData* data) {
        GenTree* node = *use;

        // TODO-MIKE-Review: This is dubious, what we really want is the equivalent
        // of GLOB_REF for locals. A local address by itself isn't GLOB_REF but
        // a treat it as such in case it is used by an indir. Maybe we can detect
        // some obvious cases where no indir is present (field addresses likely).

        if (node->OperIs(GT_LCL_ADDR))
        {
            LclVarDsc* lcl = node->AsLclAddr()->GetLcl();

            if (lcl->lvHasLdAddrOp)
            {
                return WALK_ABORT;
            }
        }
        else if (node->OperIs(GT_LCL_VAR, GT_LCL_FLD))
        {
            LclVarDsc* lcl = node->AsLclVarCommon()->GetLcl();

            if (lcl->lvHasLdAddrOp)
            {
                return WALK_ABORT;
            }
        }

        return WALK_CONTINUE;
    };

    return fgWalkTreePre(&tree, visitor) == WALK_ABORT;
}

// Check for the special case where an address is the constant 0.
// As we can't even fold the tree (null + field offset), we are left with
// op1 and op2 both being a constant. This causes lots of problems.
// We simply grab a temp and assign 0 to it and use it in place of the NULL.
// TODO-MIKE-Review: This looks like a bunch of idiotic nonsense.
// null + field offset = null, load/store(null) -> NullReferenceException.
GenTree* Importer::impCheckForNullPointer(GenTree* addr)
{
    // If it is not a GC type, we will be able to fold it.
    // So don't need to do anything.

    if (!varTypeIsGC(addr->GetType()) || !addr->OperIs(GT_CNS_INT))
    {
        return addr;
    }

    if (addr->AsIntCon()->GetValue() != 0)
    {
#ifdef DEBUG
        if (!addr->TypeIs(TYP_BYREF))
        {
            // We can see non-zero byrefs for RVA statics or for frozen strings.

            assert(addr->TypeIs(TYP_REF));
            assert(addr->IsIntCon(HandleKind::String));
            assert(comp->doesMethodHaveFrozenString() ||
                   (compIsForInlining() && impInlineInfo->InlinerCompiler->doesMethodHaveFrozenString()));
        }
#endif

        return addr;
    }

    LclVarDsc* lcl = lvaNewTemp(addr->GetType(), true DEBUGARG("CheckForNullPointer"));
    GenTree*   asg = comp->gtNewLclStore(lcl, addr->GetType(), addr);
    impSpillNoneAppendTree(asg);
    return comp->gtNewLclLoad(lcl, addr->GetType());
}

// Check for the special case where the object is the methods original 'this' pointer.
// Note that, the original 'this' pointer is always local var 0 for non-static method,
// even if we might have created the copy of 'this' pointer in lvaThisLclNum.
bool Compiler::impIsThis(GenTree* obj)
{
    return (obj != nullptr) && obj->OperIs(GT_LCL_VAR) &&
           impInlineRoot()->lvaIsOriginalThisParam(obj->AsLclVar()->GetLcl()->GetLclNum());
}

bool Importer::impIsPrimitive(CorInfoType jitType)
{
    return ((CORINFO_TYPE_BOOL <= jitType && jitType <= CORINFO_TYPE_DOUBLE) || jitType == CORINFO_TYPE_PTR);
}

Importer::StackEntry* Compiler::impAllocStack(unsigned size)
{
    if (impInlineInfo != nullptr)
    {
        return impInlineInfo->InlinerCompiler->impAllocStack(size);
    }

    if (size > impSharedStackSize)
    {
        impSharedStackSize = max(size, Importer::Stack::MinSize);
        impSharedStack     = new (this, CMK_Importer) Importer::StackEntry[impSharedStackSize];
    }

    return impSharedStack;
}

Importer::Stack::Stack(Compiler* compiler)
    : maxStack(compiler->info.compMaxStack), esStackDepth(0), esStack(compiler->impAllocStack(maxStack))
{
}

Importer::Importer(Compiler* comp)
    : comp(comp)
    , impTokenLookupContextHandle(comp->impTokenLookupContextHandle)
    , impInlineInfo(comp->impInlineInfo)
    , compInlineResult(comp->compInlineResult)
#ifdef DEBUG
    , verbose(comp->verbose)
#endif
    , opts(comp->opts)
    , info(comp->info)
    , verCurrentState(comp)
{
}

CompAllocator Importer::getAllocator(CompMemKind kind)
{
    return comp->getAllocator(kind);
}

codeOptimize Importer::compCodeOpt()
{
    return comp->compCodeOpt();
}

bool Importer::IsTargetAbi(CORINFO_RUNTIME_ABI abi)
{
    return comp->IsTargetAbi(abi);
}

bool Importer::compIsForInlining()
{
    return impInlineInfo != nullptr;
}

bool Importer::compDonotInline()
{
    return compIsForInlining() ? compInlineResult->IsFailure() : false;
}

Compiler* Importer::impInlineRoot()
{
    return comp->impInlineRoot();
}

#ifdef FEATURE_SIMD
bool Importer::supportSIMDTypes()
{
    return comp->supportSIMDTypes();
}
#endif

bool Importer::IsBaselineSimdIsaSupported()
{
    return comp->IsBaselineSimdIsaSupported();
}

bool Importer::compExactlyDependsOn(CORINFO_InstructionSet isa)
{
    return comp->compExactlyDependsOn(isa);
}

bool Importer::compOpportunisticallyDependsOn(CORINFO_InstructionSet isa)
{
    return comp->compOpportunisticallyDependsOn(isa);
}

bool Importer::IsIntrinsicImplementedByUserCall(NamedIntrinsic intrinsicName)
{
    return comp->IsIntrinsicImplementedByUserCall(intrinsicName);
}

bool Importer::IsMathIntrinsic(NamedIntrinsic intrinsicName)
{
    return comp->IsMathIntrinsic(intrinsicName);
}

bool Importer::IsMathIntrinsic(GenTree* tree)
{
    return comp->IsMathIntrinsic(tree);
}

void Importer::setMethodHasExpRuntimeLookup()
{
    comp->setMethodHasExpRuntimeLookup();
}

void Importer::setMethodHasGuardedDevirtualization()
{
    comp->setMethodHasGuardedDevirtualization();
}

#ifdef DEBUG
bool Importer::compTailCallStress()
{
    return comp->compTailCallStress();
}
#endif

NamedIntrinsic Importer::lookupNamedIntrinsic(CORINFO_METHOD_HANDLE method)
{
    return comp->lookupNamedIntrinsic(method);
}

#ifdef FEATURE_HW_INTRINSICS
NamedIntrinsic Importer::impFindSysNumSimdIntrinsic(CORINFO_METHOD_HANDLE method,
                                                    const char*           className,
                                                    const char*           methodName,
                                                    const char*           enclosingClassName)
{
    return comp->impFindSysNumSimdIntrinsic(method, className, methodName, enclosingClassName);
}
#endif

void Importer::fgInitBBLookup()
{
    comp->fgInitBBLookup();
}

BasicBlock* Importer::fgLookupBB(unsigned addr)
{
    return comp->fgLookupBB(addr);
}

BasicBlock* Importer::bbNewBasicBlock(BBjumpKinds jumpKind)
{
    return comp->bbNewBasicBlock(jumpKind);
}

BasicBlock* Importer::fgNewBBbefore(BBjumpKinds jumpKind, BasicBlock* block, bool extendRegion)
{
    return comp->fgNewBBbefore(jumpKind, block, extendRegion);
}

BasicBlock* Importer::fgNewBBafter(BBjumpKinds jumpKind, BasicBlock* block, bool extendRegion)
{
    return comp->fgNewBBafter(jumpKind, block, extendRegion);
}

BasicBlock* Importer::fgNewBBinRegion(BBjumpKinds jumpKind,
                                      unsigned    tryIndex,
                                      unsigned    hndIndex,
                                      BasicBlock* nearBlk,
                                      bool        putInFilter,
                                      bool        runRarely,
                                      bool        insertAtEnd)
{
    return comp->fgNewBBinRegion(jumpKind, tryIndex, hndIndex, nearBlk, putInFilter, runRarely, insertAtEnd);
}

BasicBlock* Importer::fgNewBBinRegion(BBjumpKinds jumpKind, BasicBlock* srcBlk, bool runRarely, bool insertAtEnd)
{
    return comp->fgNewBBinRegion(jumpKind, srcBlk, runRarely, insertAtEnd);
}

BasicBlock* Importer::fgNewBBinRegion(BBjumpKinds jumpKind)
{
    return comp->fgNewBBinRegion(jumpKind);
}

void Importer::fgInsertBBafter(BasicBlock* insertAfter, BasicBlock* block)
{
    comp->fgInsertBBafter(insertAfter, block);
}

void Importer::fgAddCheapPred(BasicBlock* block, BasicBlock* blockPred)
{
    comp->fgAddCheapPred(block, blockPred);
}

void Importer::fgInsertStmtAtEnd(BasicBlock* block, Statement* stmt)
{
    comp->fgInsertStmtAtEnd(block, stmt);
}

bool Importer::ehBlockHasExnFlowDsc(BasicBlock* block)
{
    return comp->ehBlockHasExnFlowDsc(block);
}

EHblkDsc* Importer::ehGetDsc(unsigned regionIndex)
{
    return comp->ehGetDsc(regionIndex);
}

bool Importer::bbInCatchHandlerILRange(BasicBlock* block)
{
    return comp->bbInCatchHandlerILRange(block);
}

bool Importer::bbInFilterILRange(BasicBlock* block)
{
    return comp->bbInFilterILRange(block);
}

uint16_t Importer::bbFindInnermostCommonTryRegion(BasicBlock* block1, BasicBlock* block2)
{
    return comp->bbFindInnermostCommonTryRegion(block1, block2);
}

#ifdef DEBUG
void Importer::fgVerifyHandlerTab()
{
    comp->fgVerifyHandlerTab();
}
#endif

void Importer::fgComputeCheapPreds()
{
    comp->fgComputeCheapPreds();
}

void Importer::fgRemovePreds()
{
    comp->fgRemovePreds();
}

void Importer::setNeedsGSSecurityCookie()
{
    comp->setNeedsGSSecurityCookie();
}

FieldSeqStore* Importer::GetFieldSeqStore()
{
    return comp->GetFieldSeqStore();
}

FieldSeqNode* Importer::GetRefanyTypeField()
{
    return comp->GetRefanyTypeField();
}

FieldSeqNode* Importer::GetRefanyValueField()
{
    return comp->GetRefanyValueField();
}

FieldSeqNode* Importer::GetByReferenceValueField(CORINFO_FIELD_HANDLE byRefFieldHandle)
{
    return comp->GetByReferenceValueField(byRefFieldHandle);
}

bool Importer::jitIsBetween(unsigned value, unsigned start, unsigned end)
{
    return Compiler::jitIsBetween(value, start, end);
}

#ifdef DEBUG

void Importer::fgDispBasicBlocks(BasicBlock* firstBlock, BasicBlock* lastBlock, bool dumpTrees)
{
    comp->fgDispBasicBlocks(firstBlock, lastBlock, dumpTrees);
}

void Importer::fgDispBasicBlocks(bool dumpTrees)
{
    comp->fgDispBasicBlocks(dumpTrees);
}

void Importer::fgDispHandlerTab()
{
    comp->fgDispHandlerTab();
}

void Importer::gtDispStmt(Statement* stmt)
{
    comp->gtDispStmt(stmt);
}

void Importer::gtDispTree(GenTree* tree)
{
    return comp->gtDispTree(tree);
}

int Importer::dspTreeID(GenTree* tree)
{
    return Compiler::dspTreeID(tree);
}

void Importer::JitLogEE(unsigned level, const char* fmt, ...)
{
    va_list args;

    if (verbose)
    {
        va_start(args, fmt);
        vflogf(jitstdout, fmt, args);
        va_end(args);
    }

    va_start(args, fmt);
    vlogf(level, fmt, args);
    va_end(args);
}

#endif // DEBUG

void Importer::eeGetCallInfo(CORINFO_RESOLVED_TOKEN* resolvedToken,
                             CORINFO_RESOLVED_TOKEN* constrainedToken,
                             CORINFO_CALLINFO_FLAGS  flags,
                             CORINFO_CALL_INFO*      result)
{
    return comp->eeGetCallInfo(resolvedToken, constrainedToken, flags, result);
}

void Importer::eeGetSig(unsigned               sigTok,
                        CORINFO_MODULE_HANDLE  scope,
                        CORINFO_CONTEXT_HANDLE context,
                        CORINFO_SIG_INFO*      retSig)
{
    comp->eeGetSig(sigTok, scope, context, retSig);
}

void Importer::eeGetCallSiteSig(unsigned               sigTok,
                                CORINFO_MODULE_HANDLE  scope,
                                CORINFO_CONTEXT_HANDLE context,
                                CORINFO_SIG_INFO*      retSig)
{
    comp->eeGetCallSiteSig(sigTok, scope, context, retSig);
}

void Importer::eeGetMethodSig(CORINFO_METHOD_HANDLE methHnd, CORINFO_SIG_INFO* retSig, CORINFO_CLASS_HANDLE owner)
{
    comp->eeGetMethodSig(methHnd, retSig, owner);
}

void Importer::eeGetFieldInfo(CORINFO_RESOLVED_TOKEN* resolvedToken,
                              CORINFO_ACCESS_FLAGS    flags,
                              CORINFO_FIELD_INFO*     result)
{
    return comp->eeGetFieldInfo(resolvedToken, flags, result);
}

const char* Importer::eeGetFieldName(CORINFO_FIELD_HANDLE field, const char** className)
{
    return comp->eeGetFieldName(field, className);
}

const char* Importer::eeGetClassName(CORINFO_CLASS_HANDLE clsHnd)
{
    return comp->eeGetClassName(clsHnd);
}

const char* Importer::eeGetMethodName(CORINFO_METHOD_HANDLE method, const char** className)
{
    return comp->eeGetMethodName(method, className);
}

CORINFO_CLASS_HANDLE Importer::eeGetClassFromContext(CORINFO_CONTEXT_HANDLE context)
{
    return comp->eeGetClassFromContext(context);
}

CORINFO_METHOD_HANDLE Importer::eeFindHelper(unsigned helper)
{
    return Compiler::eeFindHelper(helper);
}

unsigned Importer::eeGetArrayDataOffset(var_types type)
{
    return Compiler::eeGetArrayDataOffset(type);
}

unsigned Importer::eeGetMDArrayDataOffset(var_types type, unsigned rank)
{
    return Compiler::eeGetMDArrayDataOffset(type, rank);
}

bool Importer::impIsClassExact(CORINFO_CLASS_HANDLE classHnd)
{
    return comp->impIsClassExact(classHnd);
}

GenTree* Importer::impParentClassTokenToHandle(CORINFO_RESOLVED_TOKEN* resolvedToken,
                                               bool                    mustRestoreHandle,
                                               bool*                   runtimeLookup)
{
    return impTokenToHandle(resolvedToken, mustRestoreHandle, /* importParent */ true, runtimeLookup);
}

#ifdef FEATURE_READYTORUN_COMPILER
GenTreeCall* Importer::gtNewReadyToRunHelperCallNode(CORINFO_RESOLVED_TOKEN* resolvedToken,
                                                     CorInfoHelpFunc         helper,
                                                     var_types               type,
                                                     GenTreeCall::Use*       args,
                                                     CORINFO_LOOKUP_KIND*    genericLookupKind)
{
    return comp->gtNewReadyToRunHelperCallNode(resolvedToken, helper, type, args, genericLookupKind);
}
#endif

GenTree* Importer::gtNewRuntimeContextTree(CORINFO_RUNTIME_LOOKUP_KIND kind)
{
    return comp->gtNewRuntimeContextTree(kind);
}

GenTreeCall* Importer::gtNewSharedStaticsCctorHelperCall(CORINFO_CLASS_HANDLE cls, CorInfoHelpFunc helper)
{
    return comp->gtNewSharedStaticsCctorHelperCall(cls, helper);
}

GenTreeCall* Importer::gtNewSharedCctorHelperCall(CORINFO_CLASS_HANDLE cls)
{
    return comp->gtNewSharedCctorHelperCall(cls);
}

CORINFO_CLASS_HANDLE Importer::impGetRefAnyClass()
{
    return comp->impGetRefAnyClass();
}

CORINFO_CLASS_HANDLE Importer::impGetObjectClass()
{
    return comp->impGetObjectClass();
}

CORINFO_CLASS_HANDLE Importer::impGetTypeHandleClass()
{
    CORINFO_CLASS_HANDLE typeHandleClass = info.compCompHnd->getBuiltinClass(CLASSID_TYPE_HANDLE);
    assert(typeHandleClass != (CORINFO_CLASS_HANDLE) nullptr);
    return typeHandleClass;
}

var_types Importer::GetRuntimeHandleUnderlyingType()
{
    // RuntimeTypeHandle is backed by raw pointer on CoreRT and by object reference on other runtimes
    return IsTargetAbi(CORINFO_CORERT_ABI) ? TYP_I_IMPL : TYP_REF;
}

CORINFO_CLASS_HANDLE Importer::gtGetClassHandle(GenTree* tree, bool* isExact, bool* isNonNull)
{
    return comp->gtGetClassHandle(tree, isExact, isNonNull);
}

ClassLayout* Importer::typGetObjLayout(CORINFO_CLASS_HANDLE classHandle)
{
    return comp->typGetObjLayout(classHandle);
}

unsigned Importer::typGetObjLayoutNum(CORINFO_CLASS_HANDLE classHandle)
{
    return comp->typGetObjLayoutNum(classHandle);
}

var_types Importer::typGetStructType(ClassLayout* layout)
{
    return comp->typGetStructType(layout);
}

var_types Importer::typGetStructType(CORINFO_CLASS_HANDLE classHandle, var_types* elementType)
{
    return comp->typGetStructType(classHandle, elementType);
}

ClassLayout* Importer::typGetBlkLayout(unsigned blockSize)
{
    return comp->typGetBlkLayout(blockSize);
}

unsigned Importer::typGetLayoutNum(ClassLayout* layout)
{
    return comp->typGetLayoutNum(layout);
}

ClassLayout* Importer::typGetLayoutByNum(unsigned layoutNum)
{
    return comp->typGetLayoutByNum(layoutNum);
}

StructPassing Importer::abiGetStructReturnType(ClassLayout* layout, CorInfoCallConvExtension callConv, bool isVarArgs)
{
    return comp->abiGetStructReturnType(layout, callConv, isVarArgs);
}

LclVarDsc* Importer::lvaAllocTemp(bool shortLifetime DEBUGARG(const char* reason))
{
    return comp->lvaAllocTemp(shortLifetime DEBUGARG(reason));
}

LclVarDsc* Importer::lvaAllocTemps(unsigned count DEBUGARG(const char* reason))
{
    return comp->lvaAllocTemps(count DEBUGARG(reason));
}

LclVarDsc* Importer::lvaNewTemp(var_types type, bool shortLifetime DEBUGARG(const char* reason))
{
    return comp->lvaNewTemp(type, shortLifetime DEBUGARG(reason));
}

LclVarDsc* Importer::lvaNewTemp(ClassLayout* layout, bool shortLifetime DEBUGARG(const char* reason))
{
    return comp->lvaNewTemp(layout, shortLifetime DEBUGARG(reason));
}

LclVarDsc* Importer::lvaNewTemp(CORINFO_CLASS_HANDLE classHandle, bool shortLifetime DEBUGARG(const char* reason))
{
    return comp->lvaNewTemp(classHandle, shortLifetime DEBUGARG(reason));
}

LclVarDsc* Importer::lvaNewTemp(GenTree* tree, bool shortLifetime DEBUGARG(const char* reason))
{
    return comp->lvaNewTemp(tree, shortLifetime DEBUGARG(reason));
}

void Importer::lvaSetAddressExposed(LclVarDsc* lcl)
{
    comp->lvaSetAddressExposed(lcl);
}

bool Importer::lvaHaveManyLocals()
{
    return comp->lvaHaveManyLocals();
}

bool Importer::fgVarNeedsExplicitZeroInit(LclVarDsc* lcl, bool blockIsInLoop, bool blockIsReturn)
{
    return comp->fgVarNeedsExplicitZeroInit(lcl, blockIsInLoop, blockIsReturn);
}

Statement* Importer::gtNewStmt(GenTree* expr, IL_OFFSETX offset)
{
    return comp->gtNewStmt(expr, offset);
}

GenTreeLclVar* Importer::gtNewLclvNode(LclVarDsc* lcl, var_types type)
{
    return comp->gtNewLclvNode(lcl, type);
}

GenTreeLclAddr* Importer::gtNewLclVarAddrNode(LclVarDsc* lcl, var_types type)
{
    return comp->gtNewLclVarAddrNode(lcl, type);
}

GenTreeIntCon* Importer::gtNewIconNode(ssize_t value, var_types type)
{
    return comp->gtNewIconNode(value, type);
}

GenTreeIntCon* Importer::gtNewIconNode(unsigned fieldOffset, FieldSeqNode* fieldSeq)
{
    return comp->gtNewIconNode(fieldOffset, fieldSeq);
}

GenTree* Importer::gtNewLconNode(int64_t value)
{
    return comp->gtNewLconNode(value);
}

GenTreeIntCon* Importer::gtNewIconHandleNode(void* value, HandleKind kind, FieldSeqNode* fieldSeq)
{
    return comp->gtNewIconHandleNode(value, kind, fieldSeq);
}

GenTreeIntCon* Importer::gtNewIconHandleNode(size_t value, HandleKind kind, FieldSeqNode* fieldSeq)
{
    return comp->gtNewIconHandleNode(value, kind, fieldSeq);
}

GenTree* Importer::gtNewConstLookupTree(void* value, void* pValue, HandleKind kind, void* compileTimeHandle)
{
    return comp->gtNewConstLookupTree(value, pValue, kind, compileTimeHandle);
}

GenTree* Importer::gtNewIconEmbModHndNode(CORINFO_MODULE_HANDLE modHnd)
{
    return comp->gtNewIconEmbModHndNode(modHnd);
}

GenTree* Importer::gtNewIconEmbClsHndNode(CORINFO_CLASS_HANDLE clsHnd)
{
    return comp->gtNewIconEmbClsHndNode(clsHnd);
}

GenTree* Importer::gtNewIconEmbMethHndNode(CORINFO_METHOD_HANDLE methHnd)
{
    return comp->gtNewIconEmbMethHndNode(methHnd);
}

GenTree* Importer::gtNewIconEmbFldHndNode(CORINFO_FIELD_HANDLE fldHnd)
{
    return comp->gtNewIconEmbFldHndNode(fldHnd);
}

GenTree* Importer::gtNewIndOfIconHandleNode(var_types type, size_t value, HandleKind kind, bool invariant)
{
    return comp->gtNewIndOfIconHandleNode(type, value, kind, invariant);
}

GenTree* Importer::gtNewZeroConNode(var_types type)
{
    return comp->gtNewZeroConNode(type);
}

GenTree* Importer::gtNewOneConNode(var_types type)
{
    return comp->gtNewOneConNode(type);
}

GenTree* Importer::gtNewDconNode(double value, var_types type)
{
    return comp->gtNewDconNode(value, type);
}

GenTreeStrCon* Importer::gtNewSconNode(CORINFO_MODULE_HANDLE module, mdToken token)
{
    return comp->gtNewSconNode(module, token);
}

GenTree* Importer::gtNewNothingNode()
{
    return comp->gtNewNothingNode();
}

GenTree* Importer::gtUnusedValNode(GenTree* expr)
{
    return comp->gtUnusedValNode(expr);
}

GenTreeRetExpr* Importer::gtNewRetExpr(GenTreeCall* call)
{
    return comp->gtNewRetExpr(call);
}

GenTreeUnOp* Importer::gtNewOperNode(genTreeOps oper, var_types type, GenTree* op1)
{
    return comp->gtNewOperNode(oper, type, op1);
}

GenTree* Importer::gtNewNullCheck(GenTree* addr)
{
    return comp->gtNewNullCheck(addr);
}

GenTreeCast* Importer::gtNewCastNode(GenTree* op1, bool fromUnsigned, var_types toType)
{
    return comp->gtNewCastNode(op1, fromUnsigned, toType);
}

GenTreeIndir* Importer::gtNewIndir(var_types type, GenTree* addr)
{
    return comp->gtNewIndir(type, addr);
}

GenTreeFieldAddr* Importer::gtNewFieldAddr(GenTree* addr, CORINFO_FIELD_HANDLE handle, unsigned offset)
{
    return comp->gtNewFieldAddr(addr, handle, offset);
}

GenTreeFieldAddr* Importer::gtNewFieldAddr(GenTree* addr, FieldSeqNode* fieldSeq, unsigned offset)
{
    return comp->gtNewFieldAddr(addr, fieldSeq, offset);
}

GenTreeIndir* Importer::gtNewFieldIndir(var_types type, GenTreeFieldAddr* fieldAddr)
{
    return comp->gtNewFieldIndir(type, fieldAddr);
}

GenTreeIndir* Importer::gtNewFieldIndir(var_types type, unsigned layoutNum, GenTreeFieldAddr* fieldAddr)
{
    return comp->gtNewFieldIndir(type, layoutNum, fieldAddr);
}

GenTreeIndir* Importer::gtNewFieldIndStore(var_types type, GenTreeFieldAddr* fieldAddr, GenTree* value)
{
    GenTreeIndir* store = gtNewFieldIndir(type, fieldAddr);
    store->SetOper(GT_STOREIND);
    store->SetValue(value);
    store->AddSideEffects(GTF_ASG | value->GetSideEffects());
    return store;
}

GenTreeObj* Importer::gtNewObjNode(ClassLayout* layout, GenTree* addr)
{
    return comp->gtNewObjNode(layout, addr);
}

GenTreeObj* Importer::gtNewObjNode(var_types type, ClassLayout* layout, GenTree* addr)
{
    return comp->gtNewObjNode(type, layout, addr);
}

GenTree* Importer::gtNewStringLiteralNode(InfoAccessType iat, void* value)
{
    return comp->gtNewStringLiteralNode(iat, value);
}

GenTreeIntCon* Importer::gtNewStringLiteralLength(GenTreeStrCon* node)
{
    return comp->gtNewStringLiteralLength(node);
}

GenTreeIndir* Importer::gtNewMethodTableLookup(GenTree* obj)
{
    return comp->gtNewMethodTableLookup(obj);
}

GenTreeOp* Importer::gtNewOperNode(genTreeOps oper, var_types type, GenTree* op1, GenTree* op2)
{
    return comp->gtNewOperNode(oper, type, op1, op2);
}

GenTreeOp* Importer::gtNewCommaNode(GenTree* op1, GenTree* op2, var_types type)
{
    return comp->gtNewCommaNode(op1, op2, type);
}

GenTreeQmark* Importer::gtNewQmarkNode(var_types type, GenTree* cond, GenTree* op1, GenTree* op2)
{
    return comp->gtNewQmarkNode(type, cond, op1, op2);
}

GenTreeBoundsChk* Importer::gtNewBoundsChk(GenTree* index, GenTree* length, ThrowHelperKind kind)
{
    return comp->gtNewBoundsChk(index, length, kind);
}

GenTreeIndexAddr* Importer::gtNewArrayIndexAddr(GenTree* arr, GenTree* ind, var_types elemType)
{
    return comp->gtNewArrayIndexAddr(arr, ind, elemType);
}

GenTreeIndexAddr* Importer::gtNewStringIndexAddr(GenTree* arr, GenTree* ind)
{
    return comp->gtNewStringIndexAddr(arr, ind);
}

GenTreeIndir* Importer::gtNewIndexIndir(var_types type, GenTreeIndexAddr* indexAddr)
{
    return comp->gtNewIndexIndir(type, indexAddr);
}

GenTreeIndir* Importer::gtNewIndexIndStore(var_types type, GenTreeIndexAddr* indexAddr, GenTree* value)
{
    GenTreeIndir* store = gtNewIndexIndir(type, indexAddr);
    store->SetOper(store->OperIs(GT_IND) ? GT_STOREIND : GT_STORE_OBJ);
    store->SetValue(value);
    store->AddSideEffects(GTF_ASG | GTF_GLOB_REF | value->GetSideEffects());
    return store;
}

GenTreeCall::Use* Importer::gtNewCallArgs(GenTree* node)
{
    return comp->gtNewCallArgs(node);
}

GenTreeCall::Use* Importer::gtNewCallArgs(GenTree* node1, GenTree* node2)
{
    return comp->gtNewCallArgs(node1, node2);
}

GenTreeCall::Use* Importer::gtNewCallArgs(GenTree* node1, GenTree* node2, GenTree* node3)
{
    return comp->gtNewCallArgs(node1, node2, node3);
}

GenTreeCall::Use* Importer::gtNewCallArgs(GenTree* node1, GenTree* node2, GenTree* node3, GenTree* node4)
{
    return comp->gtNewCallArgs(node1, node2, node3, node4);
}

GenTreeCall::Use* Importer::gtPrependNewCallArg(GenTree* node, GenTreeCall::Use* args)
{
    return comp->gtPrependNewCallArg(node, args);
}

GenTreeCall::Use* Importer::gtInsertNewCallArgAfter(GenTree* node, GenTreeCall::Use* after)
{
    return comp->gtInsertNewCallArgAfter(node, after);
}

GenTreeCall* Importer::gtNewHelperCallNode(CorInfoHelpFunc helper, var_types type, GenTreeCall::Use* args)
{
    return comp->gtNewHelperCallNode(helper, type, args);
}

GenTreeCall* Importer::gtNewRuntimeLookupHelperCallNode(CORINFO_RUNTIME_LOOKUP* runtimeLookup,
                                                        GenTree*                context,
                                                        void*                   compileTimeHandle)
{
    return comp->gtNewRuntimeLookupHelperCallNode(runtimeLookup, context, compileTimeHandle);
}

GenTreeCall* Importer::gtNewUserCallNode(CORINFO_METHOD_HANDLE handle,
                                         var_types             type,
                                         GenTreeCall::Use*     args,
                                         IL_OFFSETX            ilOffset)
{
    return comp->gtNewUserCallNode(handle, type, args, ilOffset);
}

GenTreeCall* Importer::gtNewIndCallNode(GenTree* addr, var_types type, GenTreeCall::Use* args, IL_OFFSETX ilOffset)
{
    return comp->gtNewIndCallNode(addr, type, args, ilOffset);
}

#ifdef FEATURE_HW_INTRINSICS

GenTreeHWIntrinsic* Importer::gtNewSimdHWIntrinsicNode(var_types      type,
                                                       NamedIntrinsic hwIntrinsicID,
                                                       var_types      baseType,
                                                       unsigned       size)
{
    return comp->gtNewSimdHWIntrinsicNode(type, hwIntrinsicID, baseType, size);
}

GenTreeHWIntrinsic* Importer::gtNewSimdHWIntrinsicNode(
    var_types type, NamedIntrinsic hwIntrinsicID, var_types baseType, unsigned size, GenTree* op1)
{
    return comp->gtNewSimdHWIntrinsicNode(type, hwIntrinsicID, baseType, size, op1);
}

GenTreeHWIntrinsic* Importer::gtNewSimdHWIntrinsicNode(
    var_types type, NamedIntrinsic hwIntrinsicID, var_types baseType, unsigned size, GenTree* op1, GenTree* op2)
{
    return comp->gtNewSimdHWIntrinsicNode(type, hwIntrinsicID, baseType, size, op1, op2);
}

GenTreeHWIntrinsic* Importer::gtNewSimdHWIntrinsicNode(var_types      type,
                                                       NamedIntrinsic hwIntrinsicID,
                                                       var_types      baseType,
                                                       unsigned       size,
                                                       GenTree*       op1,
                                                       GenTree*       op2,
                                                       GenTree*       op3)
{
    return comp->gtNewSimdHWIntrinsicNode(type, hwIntrinsicID, baseType, size, op1, op2, op3);
}

GenTreeHWIntrinsic* Importer::gtNewSimdHWIntrinsicNode(var_types      type,
                                                       NamedIntrinsic hwIntrinsicID,
                                                       var_types      baseType,
                                                       unsigned       size,
                                                       GenTree*       op1,
                                                       GenTree*       op2,
                                                       GenTree*       op3,
                                                       GenTree*       op4)
{
    return comp->gtNewSimdHWIntrinsicNode(type, hwIntrinsicID, baseType, size, op1, op2, op3, op4);
}

GenTreeHWIntrinsic* Importer::gtNewSimdHWIntrinsicNode(var_types      type,
                                                       NamedIntrinsic hwIntrinsicID,
                                                       var_types      baseType,
                                                       unsigned       size,
                                                       GenTree*       op1,
                                                       GenTree*       op2,
                                                       GenTree*       op3,
                                                       GenTree*       op4,
                                                       GenTree*       op5)
{
    return comp->gtNewSimdHWIntrinsicNode(type, hwIntrinsicID, baseType, size, op1, op2, op3, op4, op5);
}

GenTreeHWIntrinsic* Importer::gtNewSimdHWIntrinsicNode(
    var_types type, NamedIntrinsic hwIntrinsicID, var_types baseType, unsigned size, unsigned numOps, GenTree** ops)
{
    return comp->gtNewSimdHWIntrinsicNode(type, hwIntrinsicID, baseType, size, numOps, ops);
}

GenTreeHWIntrinsic* Importer::gtNewZeroSimdHWIntrinsicNode(ClassLayout* layout)
{
    return comp->gtNewZeroSimdHWIntrinsicNode(layout);
}

GenTreeHWIntrinsic* Importer::gtNewZeroSimdHWIntrinsicNode(var_types type, var_types baseType)
{
    return comp->gtNewZeroSimdHWIntrinsicNode(type, baseType);
}

GenTreeHWIntrinsic* Importer::gtNewSimdGetElementNode(var_types simdType,
                                                      var_types elementType,
                                                      GenTree*  value,
                                                      GenTree*  index)
{
    return comp->gtNewSimdGetElementNode(simdType, elementType, value, index);
}

GenTreeHWIntrinsic* Importer::gtNewSimdWithElementNode(
    var_types type, var_types eltType, GenTree* vec, GenTreeIntCon* idx, GenTree* elt)
{
    return comp->gtNewSimdWithElementNode(type, eltType, vec, idx, elt);
}

GenTreeHWIntrinsic* Importer::gtNewScalarHWIntrinsicNode(var_types type, NamedIntrinsic hwIntrinsicID, GenTree* op1)
{
    return comp->gtNewScalarHWIntrinsicNode(type, hwIntrinsicID, op1);
}

GenTreeHWIntrinsic* Importer::gtNewScalarHWIntrinsicNode(var_types      type,
                                                         NamedIntrinsic hwIntrinsicID,
                                                         GenTree*       op1,
                                                         GenTree*       op2)
{
    return comp->gtNewScalarHWIntrinsicNode(type, hwIntrinsicID, op1, op2);
}

GenTreeHWIntrinsic* Importer::gtNewScalarHWIntrinsicNode(
    var_types type, NamedIntrinsic hwIntrinsicID, GenTree* op1, GenTree* op2, GenTree* op3)
{
    return comp->gtNewScalarHWIntrinsicNode(type, hwIntrinsicID, op1, op2, op3);
}

void Importer::lvaRecordSimdIntrinsicUse(GenTree* op)
{
    comp->lvaRecordSimdIntrinsicUse(op);
}

void Importer::lvaRecordSimdIntrinsicUse(GenTreeLclVar* lclVar)
{
    comp->lvaRecordSimdIntrinsicUse(lclVar);
}

void Importer::lvaRecordSimdIntrinsicUse(LclVarDsc* lcl)
{
    comp->lvaRecordSimdIntrinsicUse(lcl);
}

void Importer::lvaRecordSimdIntrinsicDef(GenTreeLclVar* lclVar, GenTreeHWIntrinsic* src)
{
    comp->lvaRecordSimdIntrinsicDef(lclVar, src);
}

void Importer::lvaRecordSimdIntrinsicDef(LclVarDsc* lcl, GenTreeHWIntrinsic* src)
{
    comp->lvaRecordSimdIntrinsicDef(lcl, src);
}

#endif // FEATURE_HW_INTRINSICS

GenTreeLclAddr* Importer::impIsAddressInLocal(GenTree* tree)
{
    return Compiler::impIsAddressInLocal(tree);
}

GenTreeLclAddr* Importer::impIsLocalAddrExpr(GenTree* node)
{
    return Compiler::impIsLocalAddrExpr(node);
}

bool Importer::impHasLclRef(GenTree* tree, LclVarDsc* lcl)
{
    return comp->impHasLclRef(tree, lcl);
}

bool Importer::impHasAddressTakenLocals(GenTree* tree)
{
    return comp->impHasAddressTakenLocals(tree);
}

void Importer::impDevirtualizeCall(GenTreeCall*            call,
                                   CORINFO_RESOLVED_TOKEN* resolvedToken,
                                   CORINFO_METHOD_HANDLE*  method,
                                   unsigned*               methodFlags,
                                   CORINFO_CONTEXT_HANDLE* contextHandle,
                                   CORINFO_CONTEXT_HANDLE* exactContextHandle,
                                   bool                    isExplicitTailCall,
                                   IL_OFFSETX              ilOffset)
{
    comp->impDevirtualizeCall(call, resolvedToken, method, methodFlags, contextHandle, exactContextHandle, this,
                              isExplicitTailCall, ilOffset);
}

GenTree* Importer::gtClone(GenTree* tree, bool complexOK)
{
    return comp->gtClone(tree, complexOK);
}

GenTree* Importer::gtCloneExpr(GenTree* tree)
{
    return comp->gtCloneExpr(tree);
}

bool Importer::gtCanSwapOrder(GenTree* op1, GenTree* op2)
{
    return comp->gtCanSwapOrder(op1, op2);
}

GenTree* Importer::gtFoldExpr(GenTree* tree)
{
    return comp->gtFoldExpr(tree);
}

GenTree* Importer::gtFoldExprConst(GenTree* tree)
{
    return comp->gtFoldExprConst(tree);
}

void Importer::gtChangeOperToNullCheck(GenTree* tree)
{
    comp->gtChangeOperToNullCheck(tree);
}

bool Importer::gtIsRecursiveCall(GenTreeCall* call)
{
    return comp->gtIsRecursiveCall(call);
}

bool Importer::gtIsRecursiveCall(CORINFO_METHOD_HANDLE callMethodHandle)
{
    return comp->gtIsRecursiveCall(callMethodHandle);
}

GenTree* Importer::gtFoldTypeCompare(GenTree* tree)
{
    return comp->gtFoldTypeCompare(tree);
}

GenTree* Importer::gtFoldTypeEqualityCall(bool isEq, GenTree* op1, GenTree* op2)
{
    return comp->gtFoldTypeEqualityCall(isEq, op1, op2);
}

GenTree* Importer::gtOptimizeEnumHasFlag(GenTree* thisOp, GenTree* flagOp)
{
    return comp->gtOptimizeEnumHasFlag(thisOp, flagOp);
}

CORINFO_CLASS_HANDLE Importer::gtGetHelperArgClassHandle(GenTree* array)
{
    return comp->gtGetHelperArgClassHandle(array);
}

bool Importer::fgAddrCouldBeNull(GenTree* addr)
{
    return comp->fgAddrCouldBeNull(addr);
}

void Importer::impCheckCanInline(GenTreeCall*           call,
                                 CORINFO_METHOD_HANDLE  methodHandle,
                                 unsigned               methodAttrs,
                                 CORINFO_CONTEXT_HANDLE exactContextHnd,
                                 InlineCandidateInfo**  inlineCandidateInfo,
                                 InlineResult*          inlineResult)
{
    comp->impCheckCanInline(call, methodHandle, methodAttrs, exactContextHnd, inlineCandidateInfo, inlineResult);
}

LclVarDsc* Importer::inlGetInlineeLocal(InlineInfo* inlineInfo, unsigned ilLocNum)
{
    return comp->inlGetInlineeLocal(inlineInfo, ilLocNum);
}

GenTree* Importer::inlUseArg(InlineInfo* inlineInfo, unsigned ilArgNum)
{
    return comp->inlUseArg(inlineInfo, ilArgNum);
}

bool Importer::inlImportReturn(InlineInfo* inlineInfo, GenTree* op, CORINFO_CLASS_HANDLE retClsHnd)
{
    return comp->inlImportReturn(*this, inlineInfo, op, retClsHnd);
}

void Importer::impResolveToken(const BYTE* addr, CORINFO_RESOLVED_TOKEN* resolvedToken, CorInfoTokenKind kind)
{
    comp->impResolveToken(addr, resolvedToken, kind);
}

CORINFO_CLASS_HANDLE Importer::impResolveClassToken(const BYTE* addr, CorInfoTokenKind kind)
{
    assert((kind == CORINFO_TOKENKIND_Class) || (kind == CORINFO_TOKENKIND_Casting));

    CORINFO_RESOLVED_TOKEN token{};
    comp->impResolveToken(addr, &token, kind);
    return token.hClass;
}
