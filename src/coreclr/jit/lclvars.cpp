// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "register_arg_convention.h"
#include "jitstd/algorithm.h"
#include "patchpointinfo.h"

bool Compiler::lvaInitRetType()
{
    var_types retType       = CorTypeToVarType(info.compMethodInfo->args.retType);
    bool      hasRetBuffArg = false;

    if (retType != TYP_STRUCT)
    {
        info.retDesc.InitializePrimitive(retType);
        info.compRetType = retType;
    }
    else
    {
        ClassLayout*  retLayout = typGetObjLayout(info.compMethodInfo->args.retTypeClass);
        StructPassing retKind   = abiGetStructReturnType(retLayout, info.compCallConv, info.compIsVarArgs);

        info.compRetType = typGetStructType(retLayout);
        info.retLayout   = retLayout;

        if (retKind.kind == SPK_PrimitiveType)
        {
            info.retDesc.InitializePrimitive(retKind.type);

            if (varTypeIsFloating(retKind.type) && !compFloatingPointUsed)
            {
                compFloatingPointUsed = true;
            }
        }
#if FEATURE_MULTIREG_RET
        else if (retKind.kind == SPK_ByValue)
        {
            assert(retKind.type == TYP_STRUCT);

            info.retDesc.InitializeStruct(this, retLayout);
        }
#endif
        else
        {
            assert(retKind.kind == SPK_ByReference);

            hasRetBuffArg = true;
            retKind.type  = TYP_VOID;

            if (!compIsForInlining()
#ifndef TARGET_AMD64
                && (compIsProfilerHookNeeded()
#if defined(TARGET_WINDOWS) && defined(TARGET_ARM64)
                    || callConvIsInstanceMethodCallConv(info.compCallConv)
#elif defined(TARGET_X86)
                    || (info.compCallConv != CorInfoCallConvExtension::Managed)
#endif
                        )
#endif
                    )
            {
                retKind.type = TYP_BYREF;
            }

            info.retDesc.InitializePrimitive(retKind.type);
        }
    }

    return hasRetBuffArg;
}

bool Compiler::lvaInitLocalsCount()
{
    bool hasRetBuffArg = lvaInitRetType();

    info.compArgsCount = info.compMethodInfo->args.numArgs;

    if (!info.compIsStatic)
    {
        info.compArgsCount++;
    }
    else
    {
        info.compThisArg = BAD_VAR_NUM;
    }

    info.compILargsCount = info.compArgsCount;

    if (hasRetBuffArg)
    {
        info.compArgsCount++;
    }
    else
    {
        info.compRetBuffArg = BAD_VAR_NUM;
    }

    if (info.compIsVarArgs)
    {
        info.compArgsCount++;
    }

    if (info.compMethodInfo->args.callConv & CORINFO_CALLCONV_PARAMTYPE)
    {
        info.compArgsCount++;
    }
    else
    {
        info.compTypeCtxtArg = BAD_VAR_NUM;
    }

    info.compLocalsCount = info.compArgsCount + info.compMethodInfo->locals.numArgs;

    return hasRetBuffArg;
}

void Compiler::lvaInitInline()
{
    assert(compIsForInlining());

    lvaInitLocalsCount();

    Compiler* inlinerCompiler = impInlineInfo->InlinerCompiler;

    lvaTable           = inlinerCompiler->lvaTable;
    lvaCount           = inlinerCompiler->lvaCount;
    lvaTableSize       = inlinerCompiler->lvaTableSize;
    lvaStubArgumentVar = inlinerCompiler->lvaStubArgumentVar;
}

void Compiler::lvaInitTable()
{
    assert(!compIsForInlining());

#ifdef TARGET_UNIX
    if (info.compIsVarArgs)
    {
        // Currently native varargs is not implemented on non windows targets.
        //
        // Note that some targets like Arm64 Unix should not need much work as
        // the ABI is the same. While other targets may only need small changes
        // such as amd64 Unix, which just expects RAX to pass numFPArguments.
        NYI("InitUserArgs for Vararg callee is not yet implemented on non Windows targets.");
    }
#endif

    bool hasRetBuffArg = lvaInitLocalsCount();

    lvaCount     = info.compLocalsCount;
    lvaTableSize = max(16, lvaCount * 2);
    lvaTable     = getAllocator(CMK_LvaTable).allocate<LclVarDsc>(lvaTableSize);
    memset(lvaTable, 0, lvaTableSize * sizeof(lvaTable[0]));
    for (unsigned i = 0; i < lvaCount; i++)
    {
        new (&lvaTable[i]) LclVarDsc();
    }

    lvaInitParams(hasRetBuffArg);
    lvaInitLocals();
}

void Compiler::lvaInitLocals()
{
    CORINFO_ARG_LIST_HANDLE local      = info.compMethodInfo->locals.args;
    unsigned                localCount = info.compMethodInfo->locals.numArgs;

    for (unsigned i = 0; i < localCount; i++, local = info.compCompHnd->getArgNext(local))
    {
        CORINFO_CLASS_HANDLE typeHnd    = NO_CLASS_HANDLE;
        CorInfoTypeWithMod   corTypeMod = info.compCompHnd->getArgType(&info.compMethodInfo->locals, local, &typeHnd);
        CorInfoType          corType    = strip(corTypeMod);

        unsigned   lclNum = info.compArgsCount + i;
        LclVarDsc* lcl    = lvaGetDesc(lclNum);

        lvaInitVarDsc(lcl, corType, typeHnd);

        if ((corTypeMod & CORINFO_TYPE_MOD_PINNED) != 0)
        {
            if ((corType == CORINFO_TYPE_CLASS) || (corType == CORINFO_TYPE_BYREF))
            {
                JITDUMP("Setting lvPinned for V%02u\n", lclNum);
                lcl->lvPinned = true;
            }
            else
            {
                JITDUMP("Ignoring pin for non-GC type V%02u\n", lclNum);
            }
        }

        if (corType == CORINFO_TYPE_CLASS)
        {
            lvaSetClass(lclNum, info.compCompHnd->getArgClass(&info.compMethodInfo->locals, local));
        }

        if (opts.IsOSR() && info.compPatchpointInfo->IsExposed(lclNum))
        {
            JITDUMP("-- V%02u is OSR exposed\n", lclNum);
            lcl->lvHasLdAddrOp = true;

            if (!lcl->TypeIs(TYP_STRUCT))
            {
                lvaSetVarAddrExposed(lclNum);
            }
        }
    }

#ifdef DEBUG
    if (compStressCompile(STRESS_UNSAFE_BUFFER_CHECKS, 25) &&
        // If there already exist unsafe buffers, don't mark more structs as unsafe
        // as that will cause them to be placed along with the real unsafe buffers,
        // unnecessarily exposing them to overruns. This can affect GS tests which
        // intentionally do buffer-overruns.
        !getNeedsGSSecurityCookie() &&
        // GS checks require the stack to be re-ordered, which can't be done with EnC
        !opts.compDbgEnC)
    {
        setNeedsGSSecurityCookie();
        compGSReorderStackLayout = true;

        for (unsigned i = 0; i < lvaCount; i++)
        {
            if (lvaGetDesc(i)->TypeIs(TYP_STRUCT) && compStressCompile(STRESS_GENERIC_VARN, 60))
            {
                lvaGetDesc(i)->lvIsUnsafeBuffer = true;
            }
        }
    }
#endif

    if (getNeedsGSSecurityCookie())
    {
        // Ensure that there will be at least one stack variable since
        // we require that the GSCookie does not have a 0 stack offset.

        // TODO-MIKE-Cleanup: This is a bunch of crap. It mainly exists due to the stress
        // code above, which blindly introduces GC cookies in methods that have no locals.
        // Normally we'd need a GS cookie only if the method has a local with an unsafe
        // buffer or if it uses localloc, and in the later case it would be difficult for
        // the method to do anything useful if it doesn't also have some other locals.
        // Funnily enough, this is done so early that it doesn't catch the localloc case.
        // And it's added unconditionally, even if other stack locals are already present.
        // Note that the inliner does something similar, but not quite the the same. It
        // adds it after import so it does detect localloc. It's not even clear why is this
        // done here, when the actual cookie is added later, after global morph. Go figure.
        // This could probably be added just before register allocation when we could
        // check if it's actually needed. But it would be even better to not need this
        // dummy and have frame allocation take care of this.
        // Removing this causes a few diffs so keep it for now.

        unsigned lclNum = lvaNewTemp(TYP_INT, false DEBUGARG("GSCookie dummy"));
        lvaSetImplicitlyReferenced(lclNum);
    }

#if FEATURE_FIXED_OUT_ARGS
    // Allocate the lvaOutgoingArgSpaceVar now because we can run into problems in the
    // emitter when the varNum is greater that 32767 (see emitLclVarAddr::initLclVarAddr)
    lvaOutgoingArgSpaceVar = lvaGrabTemp(false DEBUGARG("outgoing args area"));
    lvaGetDesc(lvaOutgoingArgSpaceVar)->SetBlockType(0);
    lvaSetImplicitlyReferenced(lvaOutgoingArgSpaceVar);
#endif

    if (info.compPublishStubParam)
    {
        lvaStubArgumentVar = lvaNewTemp(TYP_I_IMPL, false DEBUGARG("StubParam"));
        lvaSetImplicitlyReferenced(lvaStubArgumentVar);
    }

    DBEXEC(verbose, lvaTableDump());
}

void Compiler::lvaInitParams(bool hasRetBufParam)
{
#if defined(TARGET_ARM) && defined(PROFILING_SUPPORTED)
    // Prespill all argument regs on to stack in case of Arm when under profiler.
    if (compIsProfilerHookNeeded())
    {
        codeGen->preSpillParamRegs |= RBM_ARG_REGS;
    }
#endif

    unsigned intRegCount   = MAX_REG_ARG;
    unsigned floatRegCount = MAX_FLOAT_REG_ARG;

#ifdef TARGET_X86
    assert(floatRegCount == 0);

    if (info.compIsVarArgs)
    {
        intRegCount = 0;

        // Native varargs doesn't use any registers but the managed version
        // uses up to 2 for special params - this and the return buffer.
        assert(!callConvIsInstanceMethodCallConv(info.compCallConv));

        intRegCount += info.compIsStatic ? 0 : 1;
        intRegCount += hasRetBufParam ? 1 : 0;
    }
    else
    {
        switch (info.compCallConv)
        {
            case CorInfoCallConvExtension::Thiscall:
                intRegCount = 1;
                break;
            case CorInfoCallConvExtension::C:
            case CorInfoCallConvExtension::Stdcall:
            case CorInfoCallConvExtension::CMemberFunction:
            case CorInfoCallConvExtension::StdcallMemberFunction:
                intRegCount = 0;
                break;
            case CorInfoCallConvExtension::Managed:
            case CorInfoCallConvExtension::Fastcall:
            case CorInfoCallConvExtension::FastcallMemberFunction:
                break;
            default:
                assert(!"Unknown calling convention");
                break;
        }
    }
#endif

    ParamAllocInfo paramInfo(intRegCount, floatRegCount);

    // x86 params look something like this:
    //  [this] [struct return buffer] [user params] [generic context] [varargs handle]
    //
    // x64, arm and arm64 place the generic context and varargs handle before user params:
    //  [this] [struct return buffer] [generic context] [varargs handle] [user params]

    lvaInitThisParam(paramInfo);

    bool skipFirstParam = false;
    bool useFixedRetBufReg;

#if defined(TARGET_WINDOWS) && !defined(TARGET_ARM)
    if (callConvIsInstanceMethodCallConv(info.compCallConv))
    {
        if (info.compMethodInfo->args.numArgs == 0)
        {
            BADCODE("Instance method without 'this' param");
        }

        lvaInitUserParam(paramInfo, info.compMethodInfo->args.args);

        paramInfo.lclNum++;
        skipFirstParam    = true;
        useFixedRetBufReg = false;
    }
    else
#endif
    {
        useFixedRetBufReg = true;
    }

    if (hasRetBufParam)
    {
        lvaInitRetBufParam(paramInfo, useFixedRetBufReg);
    }

#ifndef TARGET_X86
    lvaInitGenericsContextParam(paramInfo);
    lvaInitVarargsHandleParam(paramInfo);
#endif

    lvaInitUserParams(paramInfo, skipFirstParam);

    ARM_ONLY(lvaAlignPreSpillParams(paramInfo.doubleAlignMask));
    paramInfo.stackOffset = roundUp(paramInfo.stackOffset, REGSIZE_BYTES);

#ifdef TARGET_X86
    lvaInitGenericsContextParam(paramInfo);
    lvaInitVarargsHandleParam(paramInfo);
#endif

    noway_assert(paramInfo.lclNum == info.compArgsCount);
    assert(paramInfo.intRegIndex <= MAX_REG_ARG);

    codeGen->paramsStackSize             = paramInfo.stackOffset;
    codeGen->paramRegState.intRegCount   = paramInfo.intRegIndex;
    codeGen->paramRegState.floatRegCount = paramInfo.floatRegIndex;

#ifdef TARGET_X86
    // The x86 ret instruction has a 16 bit immediate so we cannot easily pop more than
    // 2^16 bytes of stack arguments. Could be handled correctly but it will be very
    // difficult for fully interruptible code
    if (paramInfo.stackOffset >= (1u << 16))
    {
        IMPL_LIMITATION("Too many arguments for the \"ret\" instruction to pop");
    }

    // The managed x86 calling convention pushes the arguments from left to right
    // so the last arguments end up having offset 0. We would need to traverse the
    // params from right to left to assign correct offsets but then we also need
    // to assign registers and that require left to right traversal. So we process
    // from left to right and now we fix up the offsets.
    // TODO-MIKE-Cleanup: We have to traverse the params twice anyway, it may be
    // better to just compute the correct offsets in the second traversal, while
    // the first traversal only assigns registers.
    if (info.compCallConv == CorInfoCallConvExtension::Managed)
    {
        unsigned oldOffset = paramInfo.stackOffset;
        unsigned newOffset = 0;

        for (unsigned i = info.compArgsCount - 1; i != UINT32_MAX; i--)
        {
            LclVarDsc* lcl = lvaGetDesc(i);

            assert(lcl->IsParam());

            if (!lcl->IsRegParam())
            {
                unsigned offset = static_cast<unsigned>(lcl->GetStackOffset());
                unsigned size   = oldOffset - offset;

                oldOffset = offset;
                lcl->SetStackOffset(newOffset);
                newOffset += size;
            }
        }

        assert(newOffset == paramInfo.stackOffset);
    }
#endif
}

void Compiler::lvaInitThisParam(ParamAllocInfo& paramInfo)
{
    if (info.compIsStatic)
    {
        return;
    }

    assert(paramInfo.lclNum == 0);
    assert(paramInfo.intRegIndex == 0);
    assert(paramInfo.stackOffset == 0);

    lvaArg0Var       = 0;
    info.compThisArg = 0;

    LclVarDsc* lcl = lvaGetDesc(paramInfo.lclNum);

    if ((info.compClassAttr & CORINFO_FLG_VALUECLASS) != 0)
    {
        lcl->SetType(TYP_BYREF);
    }
    else
    {
        lcl->SetType(TYP_REF);
        lvaSetClass(paramInfo.lclNum, info.compClassHnd);
    }

    lcl->lvIsParam = true;
    lcl->lvIsPtr   = true;

    lcl->SetParamRegs(paramInfo.AllocReg(TYP_INT));

    JITDUMP("'this' passed in register %s\n", getRegName(lcl->GetParamReg()));

#ifdef WINDOWS_AMD64_ABI
    lcl->SetStackOffset(paramInfo.stackOffset);
    paramInfo.stackOffset += REGSIZE_BYTES;
#endif
    paramInfo.lclNum++;
}

void Compiler::lvaInitRetBufParam(ParamAllocInfo& paramInfo, bool useFixedRetBufReg)
{
#ifdef DEBUG
    if (info.compRetType == TYP_STRUCT)
    {
        CORINFO_SIG_INFO sigInfo;
        info.compCompHnd->getMethodSig(info.compMethodHnd, &sigInfo);

        assert(CorTypeToVarType(sigInfo.retType) == TYP_STRUCT);

        // The VM has disabled this optimization a long time ago.
        assert(!info.compCompHnd->isStructRequiringStackAllocRetBuf(sigInfo.retTypeClass));
    }
#endif

    info.compRetBuffArg = paramInfo.lclNum;

    LclVarDsc* lcl = lvaGetDesc(paramInfo.lclNum);

    lcl->SetType(TYP_BYREF);
    lcl->lvIsParam = true;

#ifdef TARGET_ARM64
    if (useFixedRetBufReg)
    {
        lcl->SetParamRegs(REG_ARG_RET_BUFF);
    }
    else
#endif
        if (paramInfo.CanEnregister(TYP_INT))
    {
        lcl->SetParamRegs(paramInfo.AllocReg(TYP_INT));

        JITDUMP("'__retBuf' passed in register %s\n", getRegName(lcl->GetParamReg()));

#ifdef WINDOWS_AMD64_ABI
        lcl->SetStackOffset(paramInfo.stackOffset);
        paramInfo.stackOffset += REGSIZE_BYTES;
#endif
    }
    else
    {
        lcl->SetStackOffset(paramInfo.stackOffset);
        paramInfo.stackOffset += REGSIZE_BYTES;
    }

    paramInfo.lclNum++;
}

void Compiler::lvaInitGenericsContextParam(ParamAllocInfo& paramInfo)
{
    if ((info.compMethodInfo->args.callConv & CORINFO_CALLCONV_PARAMTYPE) == 0)
    {
        return;
    }

    noway_assert(!info.compIsVarArgs);

    info.compTypeCtxtArg = paramInfo.lclNum;

    LclVarDsc* lcl = lvaGetDesc(paramInfo.lclNum);

    lcl->SetType(TYP_I_IMPL);
    lcl->lvIsParam = true;

    if (paramInfo.CanEnregister(TYP_INT))
    {
        lcl->SetParamRegs(paramInfo.AllocReg(TYP_INT));

        JITDUMP("'GenCtxt' passed in register %s\n", getRegName(lcl->GetParamReg()));

#ifdef WINDOWS_AMD64_ABI
        lcl->SetStackOffset(paramInfo.stackOffset);
        paramInfo.stackOffset += REGSIZE_BYTES;
#endif
    }
    else
    {
        assert(paramInfo.stackOffset % REGSIZE_BYTES == 0);

        lcl->SetStackOffset(paramInfo.stackOffset);
        paramInfo.stackOffset += REGSIZE_BYTES;
    }

    paramInfo.lclNum++;
}

void Compiler::lvaInitVarargsHandleParam(ParamAllocInfo& paramInfo)
{
    if (!info.compIsVarArgs)
    {
        return;
    }

    lvaVarargsHandleArg = paramInfo.lclNum;

    LclVarDsc* lcl = lvaGetDesc(paramInfo.lclNum);

    lcl->SetType(TYP_I_IMPL);
    lcl->lvIsParam = true;

    // Make sure this lives in the stack, address may be reported to the VM.
    // TODO-CQ: This should probably be only DNER but that causes problems,
    // so, for expedience, I switched back to this heavyweight hammer.
    // But I think it should be possible to switch; it may just work now
    // that other problems are fixed.
    lvaSetAddressExposed(lcl);

    if (paramInfo.CanEnregister(TYP_INT))
    {
        lcl->SetParamRegs(paramInfo.AllocReg(TYP_INT));

        JITDUMP("'VarArgHnd' passed in register %s\n", getRegName(lcl->GetParamReg()));

#ifdef WINDOWS_AMD64_ABI
        lcl->SetStackOffset(paramInfo.stackOffset);
        paramInfo.stackOffset += REGSIZE_BYTES;
#endif
    }
    else
    {
        assert(paramInfo.stackOffset % REGSIZE_BYTES == 0);

        lcl->SetStackOffset(paramInfo.stackOffset);
        paramInfo.stackOffset += REGSIZE_BYTES;
    }

    paramInfo.lclNum++;

#ifdef TARGET_X86
    lvaVarargsBaseOfStkArgs = lvaNewTemp(TYP_I_IMPL, false DEBUGARG("Varargs BaseOfStkArgs"));
#endif

#ifdef TARGET_ARM
    // We have to pre-spill all the register params explicitly because we only
    // have have symbols for the declared ones, not any potential variadic ones.
    codeGen->preSpillParamRegs |= RBM_ARG_REGS;
#endif
}

void Compiler::lvaInitUserParams(ParamAllocInfo& paramInfo, bool skipFirstParam)
{
    assert(!skipFirstParam || (info.compMethodInfo->args.numArgs != 0));

#ifdef WINDOWS_AMD64_ABI
    assert(paramInfo.floatRegIndex == paramInfo.intRegIndex);
#endif

    CORINFO_ARG_LIST_HANDLE param      = info.compMethodInfo->args.args;
    unsigned                paramCount = info.compMethodInfo->args.numArgs;

    if (skipFirstParam)
    {
        param = info.compCompHnd->getArgNext(param);
        paramCount--;
    }

    for (unsigned i = 0; i < paramCount; i++, paramInfo.lclNum++, param = info.compCompHnd->getArgNext(param))
    {
        lvaInitUserParam(paramInfo, param);
    }
}

void Compiler::lvaInitUserParam(ParamAllocInfo& paramInfo, CORINFO_ARG_LIST_HANDLE param)
{
    CORINFO_CLASS_HANDLE typeHnd = NO_CLASS_HANDLE;
    CorInfoType          corType = strip(info.compCompHnd->getArgType(&info.compMethodInfo->args, param, &typeHnd));

    LclVarDsc* lcl = lvaGetDesc(paramInfo.lclNum);

    lcl->lvIsParam = true;
    lvaInitVarDsc(lcl, corType, typeHnd);
    lvaAllocUserParam(paramInfo, lcl);

    if (opts.IsOSR() && info.compPatchpointInfo->IsExposed(paramInfo.lclNum))
    {
        JITDUMP("-- V%02u is OSR exposed\n", paramInfo.lclNum);

        lcl->lvHasLdAddrOp = true;
        lvaSetVarAddrExposed(paramInfo.lclNum);
    }

    if (corType == CORINFO_TYPE_CLASS)
    {
        lvaSetClass(paramInfo.lclNum, info.compCompHnd->getArgClass(&info.compMethodInfo->args, param));
    }
}

#ifdef UNIX_AMD64_ABI

void Compiler::lvaAllocUserParam(ParamAllocInfo& paramInfo, LclVarDsc* lcl)
{
    regNumber reg0 = REG_NA;
    regNumber reg1 = REG_NA;

    if (!varTypeIsStruct(lcl->GetType()))
    {
        assert(varTypeSize(lcl->GetType()) <= REGSIZE_BYTES);

        if (paramInfo.CanEnregister(lcl->GetType()))
        {
            reg0 = paramInfo.AllocReg(lcl->GetType());
        }
    }
    else
    {
        lcl->GetLayout()->EnsureSysVAmd64AbiInfo(this);

        if (lcl->GetLayout()->GetSysVAmd64AbiRegCount() != 0)
        {
            unsigned intRegCount   = 0;
            unsigned floatRegCount = 0;

            for (unsigned i = 0; i < lcl->GetLayout()->GetSysVAmd64AbiRegCount(); i++)
            {
                if (!varTypeUsesFloatReg(lcl->GetLayout()->GetSysVAmd64AbiRegType(i)))
                {
                    intRegCount++;
                }
                else
                {
                    floatRegCount++;
                }
            }

            if (((intRegCount == 0) || paramInfo.CanEnregister(TYP_INT, intRegCount)) &&
                ((floatRegCount == 0) || paramInfo.CanEnregister(TYP_FLOAT, floatRegCount)))
            {
                reg0 = paramInfo.AllocReg(lcl->GetLayout()->GetSysVAmd64AbiRegType(0));

                if (lcl->GetLayout()->GetSysVAmd64AbiRegCount() >= 2)
                {
                    reg1 = paramInfo.AllocReg(lcl->GetLayout()->GetSysVAmd64AbiRegType(1));
                }
            }
        }
    }

    if (reg0 != REG_NA)
    {
        lcl->SetParamRegs(reg0, reg1);
        lcl->lvIsMultiRegArg = reg1 != REG_NA;

        JITDUMP("Param V%02u registers: %s%s%s\n", paramInfo.lclNum, getRegName(reg0), reg1 == REG_NA ? "" : ", ",
                reg1 == REG_NA ? "" : getRegName(reg1));
    }
    else
    {
        unsigned offset = paramInfo.stackOffset;
        unsigned size   = lvaGetParamAllocSize(lcl);

        assert(offset % REGSIZE_BYTES == 0);
        assert(size % REGSIZE_BYTES == 0);

        lcl->SetStackOffset(offset);
        paramInfo.stackOffset = offset + size;

        JITDUMP("Param V%02u offset: %u\n", paramInfo.lclNum, lcl->GetStackOffset());
    }

    if (info.compIsVarArgs)
    {
        // TODO-CQ: We shouldn't have to go as far as to declare these AX, DNER should suffice.
        lvaSetAddressExposed(lcl);
    }
}

#elif defined(WINDOWS_AMD64_ABI)

void Compiler::lvaAllocUserParam(ParamAllocInfo& paramInfo, LclVarDsc* lcl)
{
    var_types regType = lcl->GetType();

    if (varTypeIsStruct(regType))
    {
        regType = abiGetStructIntegerRegisterType(lcl->GetLayout());

        if (regType == TYP_UNDEF)
        {
            lcl->lvIsImplicitByRef = true;
            regType                = TYP_INT;
        }
    }

    if (paramInfo.CanEnregister(regType))
    {
        lcl->SetParamRegs(paramInfo.AllocReg(regType));

        JITDUMP("Param V%02u register: %s\n", paramInfo.lclNum, getRegName(lcl->GetParamReg()));
    }

    assert(paramInfo.stackOffset % REGSIZE_BYTES == 0);
    lcl->SetStackOffset(paramInfo.stackOffset);
    paramInfo.stackOffset += REGSIZE_BYTES;

    JITDUMP("Param V%02u offset: %u\n", paramInfo.lclNum, lcl->GetStackOffset());

    if (info.compIsVarArgs)
    {
        // TODO-CQ: We shouldn't have to go as far as to declare these AX, DNER should suffice.
        // TODO-MIKE-Review: DNER shouldn't be needed either, someone must have been really
        // confused about what varargs is and how it works. Or perhaps the runtime requires
        // this for some reason?
        lvaSetAddressExposed(lcl);
    }
}

#elif defined(TARGET_ARM64)

void Compiler::lvaAllocUserParam(ParamAllocInfo& paramInfo, LclVarDsc* lcl)
{
    if (varTypeIsStruct(lcl->GetType()) &&
        abiGetStructParamType(lcl->GetLayout(), info.compIsVarArgs).kind == SPK_ByReference)
    {
        lcl->lvIsImplicitByRef = true;
    }

    unsigned  paramSize = lvaGetParamAllocSize(lcl);
    var_types regType   = lcl->GetType();
    unsigned  regCount;
    unsigned  minRegCount;

#ifdef TARGET_WINDOWS
    // win-arm64 varargs does not use HFAs and can split a STRUCT arg between the last
    // integer reg arg (x7) and the first stack arg slot.
    if (info.compIsVarArgs)
    {
        assert(paramSize <= 2 * REGSIZE_BYTES);

        regType     = TYP_INT;
        regCount    = (paramSize > REGSIZE_BYTES) && paramInfo.CanEnregister(TYP_INT, 2) ? 2 : 1;
        minRegCount = 1;
    }
    else
#endif
        if (!varTypeIsStruct(regType))
    {
        assert(paramSize <= REGSIZE_BYTES);

        regCount    = 1;
        minRegCount = 1;
    }
    else if (lcl->GetLayout()->IsHfa())
    {
        lcl->SetIsHfa();

        regType     = lcl->GetLayout()->GetHfaElementType();
        regCount    = lcl->GetLayout()->GetHfaElementCount();
        minRegCount = regCount;
    }
    else
    {
        assert(paramSize <= 2 * REGSIZE_BYTES);

        regType     = TYP_INT;
        regCount    = paramSize > REGSIZE_BYTES ? 2 : 1;
        minRegCount = regCount;
    }

    if (paramInfo.CanEnregister(regType, minRegCount))
    {
        lcl->lvIsMultiRegArg = regCount > 1;
        lcl->SetParamRegs(paramInfo.AllocRegs(regType, regCount), regCount);

#ifdef TARGET_WINDOWS
        bool isSplit = info.compIsVarArgs && (regCount * REGSIZE_BYTES < paramSize);

        if (isSplit)
        {
            paramInfo.stackOffset += REGSIZE_BYTES;
        }
#endif

#ifdef DEBUG
        if (verbose)
        {
            printf("Param V%02u registers: ", paramInfo.lclNum);

            for (unsigned i = 0; i < regCount; i++)
            {
                printf("%s%s", i > 0 ? ", " : "", getRegName(lcl->GetParamReg(i)));
            }

#ifdef TARGET_WINDOWS
            if (isSplit)
            {
                printf("+ 1 stack slot");
            }
#endif

            printf("\n");
        }
#endif // DEBUG
    }
    else
    {
        paramInfo.SetHasStackParam(regType);

        unsigned offset    = paramInfo.stackOffset;
        unsigned alignment = lvaGetParamAlignment(lcl->GetType(), (regType == TYP_FLOAT));

#ifdef OSX_ARM64_ABI
        // TODO-MIKE-Review: Note that lvaGetParamAlignment appears to be returning wrong
        // alignment for vector params, if that's the case then this code should be enabled
        // for all ARM64 targets, not just OSX.
        offset = roundUp(offset, alignment);
#endif

        assert(paramSize % alignment == 0);
        assert(offset % alignment == 0);

        lcl->SetStackOffset(offset);
        paramInfo.stackOffset = offset + paramSize;

        JITDUMP("Param V%02u offset: %u\n", paramInfo.lclNum, lcl->GetStackOffset());
    }

    if (info.compIsVarArgs)
    {
        // TODO-CQ: We shouldn't have to go as far as to declare these AX, DNER should suffice.
        lvaSetAddressExposed(lcl);
    }
}

#elif defined(TARGET_X86)

void Compiler::lvaAllocUserParam(ParamAllocInfo& paramInfo, LclVarDsc* lcl)
{
    var_types regType = lcl->GetType();

    if ((regType != TYP_STRUCT ? varTypeIsI(varActualType(regType)) : isTrivialPointerSizedStruct(lcl->GetLayout())) &&
        paramInfo.CanEnregister(TYP_INT))
    {
        lcl->SetParamRegs(paramInfo.AllocReg(TYP_INT));

        JITDUMP("Param V%02u register: %s\n", paramInfo.lclNum, getRegName(lcl->GetParamReg()));
    }
    else
    {
        unsigned paramSize = lvaGetParamAllocSize(lcl);

        // Note that the x86 managed calling convention pushes the args from left to
        // right and since we also traverse the params from left to right the offset
        // we compute here is incorrect. We'll fix it up afterwards.

        assert(paramInfo.stackOffset % REGSIZE_BYTES == 0);

        lcl->SetStackOffset(paramInfo.stackOffset);
        paramInfo.stackOffset += paramSize;

        JITDUMP("Param V%02u offset: %u\n", paramInfo.lclNum, lcl->GetStackOffset());
    }
}

#elif defined(TARGET_ARM)

void Compiler::lvaAllocUserParam(ParamAllocInfo& paramInfo, LclVarDsc* lcl)
{
    unsigned paramSize = lvaGetParamAllocSize(lcl);
    unsigned alignment = REGSIZE_BYTES;

    assert(paramSize % alignment == 0);
    assert(paramInfo.stackOffset % alignment == 0);

    var_types regType        = varActualType(lcl->GetType());
    unsigned  regCount       = paramSize / REGSIZE_BYTES;
    unsigned  minRegCount    = regCount;
    bool      isHfa          = false;
    bool      softFPPreSpill = false;

    assert(!varTypeIsSIMD(regType));

    if (varTypeIsFloating(regType))
    {
        if (regType == TYP_DOUBLE)
        {
            alignment = 2 * REGSIZE_BYTES;
        }

        if (opts.UseSoftFP() || info.compIsVarArgs)
        {
            regType        = regType == TYP_FLOAT ? TYP_INT : TYP_LONG;
            softFPPreSpill = true;
        }
    }
    else if (regType == TYP_LONG)
    {
        alignment = 2 * REGSIZE_BYTES;
        regType   = TYP_INT;
    }
    else if (regType == TYP_STRUCT)
    {
        if (lcl->lvStructDoubleAlign)
        {
            alignment = 2 * REGSIZE_BYTES;
        }

        if (lcl->GetLayout()->IsHfa() && !info.compIsVarArgs)
        {
            assert(!opts.UseSoftFP());

            lcl->SetIsHfa();

            regType     = lcl->GetLayout()->GetHfaElementType();
            regCount    = lcl->GetLayout()->GetHfaRegCount();
            isHfa       = true;
            minRegCount = regCount;

            assert((alignment == 2 * REGSIZE_BYTES) == (regType == TYP_DOUBLE));
        }
        else
        {
            regType = TYP_INT;

            if (!paramInfo.CanEnregister(TYP_INT, regCount) && paramInfo.CanEnregister(TYP_INT) &&
                (paramInfo.stackOffset == 0))
            {
                minRegCount = 1;
            }
        }
    }

    paramInfo.AlignReg(regType, alignment / REGSIZE_BYTES);

    if (paramInfo.CanEnregister(regType, minRegCount))
    {
        bool preSpill = softFPPreSpill;
        bool isSplit  = false;

        if (lcl->TypeIs(TYP_STRUCT))
        {
            if (isHfa)
            {
                lcl->lvIsMultiRegArg = lcl->GetLayout()->GetHfaElementCount() > 1;
            }
            else
            {
                assert(regType == TYP_INT);

                if (regCount > paramInfo.GetAvailableRegCount(TYP_INT))
                {
                    regCount = paramInfo.GetAvailableRegCount(TYP_INT);
                    isSplit  = true;
                }

                preSpill = true;
            }
        }

        if (preSpill)
        {
            regMaskTP regMask = RBM_NONE;

            for (unsigned i = 0; i < regCount; i++)
            {
                regMask |= genMapIntRegArgNumToRegMask(paramInfo.GetRegIndex(TYP_INT) + i);
            }

            if (alignment == 2 * REGSIZE_BYTES)
            {
                paramInfo.doubleAlignMask |= regMask;
            }

            codeGen->preSpillParamRegs |= regMask;
        }

        lcl->SetParamRegs(paramInfo.AllocRegs(regType, regCount), regCount);

        if (isSplit)
        {
            paramInfo.stackOffset += paramSize - regCount * REGSIZE_BYTES;
        }

#ifdef DEBUG
        if (verbose)
        {
            printf("Param V%02u registers: ", paramInfo.lclNum);

            for (unsigned i = 0; i < regCount; i++)
            {
                if (i > 0)
                {
                    printf(", ");
                }

                regNumber reg = lcl->GetParamReg(i);

                if (regType == TYP_DOUBLE)
                {
                    printf("%s/%s", getRegName(reg), getRegName(REG_NEXT(reg)));

                    assert(i + 1 < regCount);
                    ++i;
                }
                else
                {
                    printf("%s", getRegName(reg));
                }
            }

            if (isSplit)
            {
                printf(" + %u stack slots", paramSize / REGSIZE_BYTES - regCount);
            }

            printf("\n");
        }
#endif // DEBUG
    }
    else
    {
        paramInfo.SetHasStackParam(regType);

        unsigned offset = roundUp(paramInfo.stackOffset, alignment);
        lcl->SetStackOffset(offset);
        paramInfo.stackOffset = offset + paramSize;

        JITDUMP("Param V%02u offset: %u\n", paramInfo.lclNum, lcl->GetStackOffset());
    }

    if (info.compIsVarArgs || softFPPreSpill)
    {
        // TODO-CQ: We shouldn't have to go as far as to declare these AX, DNER should suffice.
        lvaSetAddressExposed(lcl);
    }
}

void Compiler::lvaAlignPreSpillParams(regMaskTP doubleAlignMask)
{
    if ((doubleAlignMask == RBM_NONE) || (doubleAlignMask == RBM_ARG_REGS))
    {
        return;
    }

    // Double aligned params can begin only at r0 or r2 and we always expect at least
    // two registers to be used. Note that in rare cases, we can have double aligned
    // params of 12 bytes (if specified explicitly with StructLayout attribute).
    assert((doubleAlignMask == (RBM_R0 | RBM_R1)) || (doubleAlignMask == (RBM_R2 | RBM_R3)) ||
           (doubleAlignMask == (RBM_R0 | RBM_R1 | RBM_R2)));

    // Now if doubleAlignMask is xyz1 i.e., the struct starts in r0, and we prespill r2 or r3
    // but not both, then the stack would be misaligned for r0. So spill both
    // r2 and r3.
    //
    // ; +0 --- caller SP double aligned ----
    // ; -4 r2    r3
    // ; -8 r1    r1
    // ; -c r0    r0   <-- misaligned.
    // ; callee saved regs
    bool startsAtR0 = (doubleAlignMask & RBM_R0) == RBM_R0;
    bool r2XorR3    = ((codeGen->preSpillParamRegs & RBM_R2) == 0) != ((codeGen->preSpillParamRegs & RBM_R3) == 0);

    if (startsAtR0 && r2XorR3)
    {
        codeGen->preSpillAlignRegs = (~codeGen->preSpillParamRegs & ~doubleAlignMask) & RBM_ARG_REGS;
    }
}
#endif // TARGET_ARM

void Compiler::lvaInitVarDsc(LclVarDsc* lcl, CorInfoType corType, CORINFO_CLASS_HANDLE typeHnd)
{
    switch (corType)
    {
        // Mark types that looks like a pointer for doing shadow-copying of
        // parameters if we have an unsafe buffer.
        // Note that this does not handle structs with pointer fields. Instead,
        // we rely on using the assign-groups/equivalence-groups in
        // gsFindVulnerableParams() to determine if a buffer-struct contains a
        // pointer. We could do better by having the EE determine this for us.
        // Note that we want to keep buffers without pointers at lower memory
        // addresses than buffers with pointers.
        case CORINFO_TYPE_PTR:
        case CORINFO_TYPE_BYREF:
        case CORINFO_TYPE_CLASS:
        case CORINFO_TYPE_STRING:
        case CORINFO_TYPE_VAR:
        case CORINFO_TYPE_REFANY:
            lcl->lvIsPtr = true;
            break;
        default:
            break;
    }

    var_types type = CorTypeToVarType(corType);

    if (varTypeIsStruct(type))
    {
        lvaSetStruct(lcl, typGetObjLayout(typeHnd), true);
    }
    else
    {
        lcl->SetType(type);

        if (varTypeIsFloating(type))
        {
            compFloatingPointUsed = true;
        }
#if OPT_BOOL_OPS
        else if (type == TYP_BOOL)
        {
            lcl->lvIsBoolean = true;
        }
#endif

        if ((typeHnd != NO_CLASS_HANDLE) && info.compCompHnd->isValueClass(typeHnd))
        {
            // This is a "normed type" - a struct that contains a single primitive type field.
            // In general this is just a primtive type as far as the JIT is concerned but there
            // are 2 exceptions:
            //   - ldfld import code needs to know that the value on the stack is really a
            //     struct object, otherwise it could think it's the address of the object.
            //   - The inliner state machine assigns special weights to LDLOCA/LDARGA used
            //     with normed type locals.
            //
            // Note: inlUseArg and inlFetchInlineeLocal have similar code.

            assert(info.compCompHnd->getTypeForPrimitiveValueClass(typeHnd) == CORINFO_TYPE_UNDEF);

            lcl->lvImpTypeInfo = typeInfo(TI_STRUCT, typeHnd);
        }
    }

    INDEBUG(lcl->SetStackOffset(BAD_STK_OFFS);)
}

/*****************************************************************************
 * Returns the IL variable number given our internal varNum.
 * Special return values are VARG_ILNUM, RETBUF_ILNUM, TYPECTXT_ILNUM.
 *
 * Returns UNKNOWN_ILNUM if it can't be mapped.
 */

unsigned Compiler::compMap2ILvarNum(unsigned varNum) const
{
    if (compIsForInlining())
    {
        return impInlineInfo->InlinerCompiler->compMap2ILvarNum(varNum);
    }

    noway_assert(varNum < lvaCount);

    if (varNum == info.compRetBuffArg)
    {
        return (unsigned)ICorDebugInfo::RETBUF_ILNUM;
    }

    // Is this a varargs function?
    if (info.compIsVarArgs && varNum == lvaVarargsHandleArg)
    {
        return (unsigned)ICorDebugInfo::VARARGS_HND_ILNUM;
    }

    // We create an extra argument for the type context parameter
    // needed for shared generic code.
    if ((info.compMethodInfo->args.callConv & CORINFO_CALLCONV_PARAMTYPE) && (varNum == info.compTypeCtxtArg))
    {
        return (unsigned)ICorDebugInfo::TYPECTXT_ILNUM;
    }

#if FEATURE_FIXED_OUT_ARGS
    if (varNum == lvaOutgoingArgSpaceVar)
    {
        return (unsigned)ICorDebugInfo::UNKNOWN_ILNUM; // Cannot be mapped
    }
#endif // FEATURE_FIXED_OUT_ARGS

    // Now mutate varNum to remove extra parameters from the count.
    if ((info.compMethodInfo->args.callConv & CORINFO_CALLCONV_PARAMTYPE) && (varNum > info.compTypeCtxtArg))
    {
        varNum--;
    }

    if (info.compIsVarArgs && varNum > lvaVarargsHandleArg)
    {
        varNum--;
    }

    /* Is there a hidden argument for the return buffer.
       Note that this code works because if the RetBuffArg is not present,
       compRetBuffArg will be BAD_VAR_NUM */
    if (info.compRetBuffArg != BAD_VAR_NUM && varNum > info.compRetBuffArg)
    {
        varNum--;
    }

    if (varNum >= info.compLocalsCount)
    {
        return (unsigned)ICorDebugInfo::UNKNOWN_ILNUM; // Cannot be mapped
    }

    return varNum;
}

void Compiler::lvaResizeTable(unsigned newSize)
{
    // Check for overflow
    if (newSize <= lvaCount)
    {
        IMPL_LIMITATION("too many locals");
    }

    LclVarDsc* newTable = getAllocator(CMK_LvaTable).allocate<LclVarDsc>(newSize);
    memcpy(newTable, lvaTable, lvaCount * sizeof(lvaTable[0]));
    memset(newTable + lvaCount, 0, (static_cast<size_t>(newSize) - lvaCount) * sizeof(lvaTable[0]));

    // Fill the old table with junk to detect accidental use through cached LclVarDsc pointers.
    INDEBUG(memset(lvaTable, JitConfig.JitDefaultFill(), lvaCount * sizeof(lvaTable[0]));)

    lvaTableSize = newSize;
    lvaTable     = newTable;
}

bool LclVarDsc::IsDependentPromotedField(Compiler* compiler) const
{
    return lvIsStructField && !compiler->lvaGetDesc(lvParentLcl)->IsIndependentPromoted();
}

void Compiler::lvaSetImplicitlyReferenced(unsigned lclNum)
{
    LclVarDsc* lcl = lvaGetDesc(lclNum);

    lcl->lvImplicitlyReferenced = true;
    lvaSetDoNotEnregister(lcl DEBUGARG(DNER_HasImplicitRefs));

    // Currently this is only used before ref counting starts
    // so there's no need to bother with setting ref counts.
    assert(!lvaLocalVarRefCounted());
}

void Compiler::lvaSetAddressExposed(unsigned lclNum)
{
    lvaSetAddressExposed(lvaGetDesc(lclNum));
}

void Compiler::lvaSetAddressExposed(LclVarDsc* lcl)
{
    lcl->lvAddrExposed = 1;
    lvaSetDoNotEnregister(lcl DEBUGARG(DNER_AddrExposed));

    // For promoted locals we make all fields address exposed. However, if the local
    // is a promoted field we don't make the parent nor other fields address exposed.
    // It is assumed that the caller specifically wants only the specified field to
    // be address exposed, otherwise it would just make the parent address exposed.

    if (lcl->IsPromoted())
    {
        for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
        {
            LclVarDsc* fieldLcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

            fieldLcl->lvAddrExposed = 1;
            lvaSetDoNotEnregister(fieldLcl DEBUGARG(DNER_AddrExposed));
        }
    }
}

void Compiler::lvaSetDoNotEnregister(unsigned lclNum DEBUGARG(DoNotEnregisterReason reason))
{
    lvaSetDoNotEnregister(lvaGetDesc(lclNum) DEBUGARG(reason));
}

void Compiler::lvaSetDoNotEnregister(LclVarDsc* lcl DEBUGARG(DoNotEnregisterReason reason))
{
    lcl->lvDoNotEnregister = 1;

// TODO-MIKE-Review: Shouldn't this make promoted fields DNER too?

#ifdef DEBUG
    if (verbose)
    {
        const char* message;

        switch (reason)
        {
            case DNER_AddrExposed:
                assert(lcl->IsAddressExposed());
                message = "it is address exposed";
                break;
            case DNER_IsStruct:
                assert(varTypeIsStruct(lcl->GetType()));
                message = "it is a struct";
                break;
            case DNER_IsStructArg:
                assert(varTypeIsStruct(lcl->GetType()));
                message = "it is a struct arg";
                break;
            case DNER_BlockOp:
                lcl->lvLclBlockOpAddr = 1;
                message               = "written in a block op";
                break;
            case DNER_LocalField:
                lcl->lvLclFieldExpr = 1;
                message             = "was accessed as a local field";
                break;
            case DNER_LiveInOutOfHandler:
                message = "live in/out of a handler";
                break;
            case DNER_DepField:
                assert(lcl->IsDependentPromotedField(this));
                message = "field of a dependently promoted struct";
                break;
            case DNER_NoRegVars:
                assert(!compEnregLocals());
                message = "opts.compFlags & CLFLG_REGVAR is not set";
                break;
#ifdef JIT32_GCENCODER
            case DNER_PinningRef:
                assert(lcl->IsPinning());
                message = "pinning ref";
                break;
#endif
#ifndef TARGET_64BIT
            case DNER_LongParamField:
                message = "it is a decomposed field of a long parameter";
                break;
            case DNER_LongUnpromoted:
                message = "it is unpromoted LONG";
                break;
#endif
            case DNER_HasImplicitRefs:
                message = "it has implicit references";
                break;
            default:
                message = "???";
                break;
        }

        printf("\nLocal V%02u should not be enregistered: %s\n", lcl - lvaTable, message);
    }
#endif
}

void Compiler::lvSetMinOptsDoNotEnreg()
{
    JITDUMP("compEnregLocals() is false, setting doNotEnreg flag for all locals.");
    assert(!compEnregLocals());

    for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
    {
        lvaSetDoNotEnregister(lclNum DEBUGARG(Compiler::DNER_NoRegVars));
    }
}

void Compiler::lvaSetLiveInOutOfHandler(unsigned lclNum)
{
    LclVarDsc* lcl = lvaGetDesc(lclNum);

    lcl->lvLiveInOutOfHndlr = true;

    // For now, only enregister an EH Var if it is a single def and whose refCount > 1.
    if (!lvaEnregEHVars || !lcl->lvSingleDefRegCandidate || (lcl->GetRefCount() <= 1))
    {
        lvaSetDoNotEnregister(lcl DEBUGARG(DNER_LiveInOutOfHandler));
    }
#ifdef JIT32_GCENCODER
    else if (lvaKeepAliveAndReportThis() && (lclNum == info.compThisArg))
    {
        // For the JIT32_GCENCODER, when lvaKeepAliveAndReportThis is true, we must either keep the "this" pointer
        // in the same register for the entire method, or keep it on the stack. If it is EH-exposed, we can't ever
        // keep it in a register, since it must also be live on the stack. Therefore, we won't attempt to allocate it.
        lvaSetDoNotEnregister(lcl DEBUGARG(DNER_LiveInOutOfHandler));
    }
#endif

    if (lcl->IsPromoted())
    {
        for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
        {
            LclVarDsc* fieldLcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

            fieldLcl->lvLiveInOutOfHndlr = 1;

            // For now, only enregister an EH Var if it is a single def and whose refCount > 1.
            if (!lvaEnregEHVars || !fieldLcl->lvSingleDefRegCandidate || (fieldLcl->GetRefCount() <= 1))
            {
                lvaSetDoNotEnregister(fieldLcl DEBUGARG(DNER_LiveInOutOfHandler));
            }
        }
    }
}

void Compiler::lvaSetStruct(unsigned lclNum, CORINFO_CLASS_HANDLE classHandle, bool checkUnsafeBuffer)
{
    lvaSetStruct(lclNum, typGetObjLayout(classHandle), checkUnsafeBuffer);
}

void Compiler::lvaSetStruct(unsigned lclNum, ClassLayout* layout, bool checkUnsafeBuffer)
{
    noway_assert(lclNum < lvaCount);

    lvaSetStruct(lvaGetDesc(lclNum), layout, checkUnsafeBuffer);
}

void Compiler::lvaSetStruct(LclVarDsc* lcl, ClassLayout* layout, bool checkUnsafeBuffer)
{
    assert(!layout->IsBlockLayout());

    if (lcl->lvExactSize != 0)
    {
        // TODO-MIKE-Cleanup: Normally we should not attemp to call lvaSetStruct on a local that
        // already has struct type. Some trivial cases have been fixed but there are a at least
        // 2 more:
        //   - Spill clique temps may be assigned multiple times via impAssignTempGen.
        //   - inlPrependStatements initializes inlinee parameters also using impAssignTempGen.
        //     Inlinee parameters (temps really) have already been created and assigned a type when
        //     inlining started.
        //
        // impAssignTempGen insists on calling lvaSetStruct itself, which makes sense in most cases
        // (it's typically called immediately after creating a temp). It may be better to add a
        // new function that assigns without attempting to set the temp's type.
        //
        // To make things more complicated, in the inlining case the struct types can be different.
        // If a generic method is inlined then the inlinee parameter may have type A<Canon> and
        // the inliner value may have type A<SomeRefClass>. These 2 types are equivalent for most
        // purposes but they have different class handles which complicates things. Probably it
        // would be better to keep the inliner type (A<SomeRefClass>) because it's more precise.
        // On the other hand, all the inlined code uses A<Canon> so it may be better to keep that,
        // unless there's a way to import the inlined code using A<SomeRefClass>.
        //
        // This means that we can end up with "A<SomeRefClass> = A<Canon>" struct assignments but
        // that's OK, fgMorphCopyStruct and codegen support that. It may be that such mismatches
        // have some CQ consequences (block copy prop/CSE due to different VNs?).
        // In FX there aren't many such type mismatch cases but Microsoft.CodeAnalysis.CSharp.dll
        // has a lot more.
        //
        // In theory we can also have different class handles in the spill clique case but that
        // would be caused by invalid IL so it's probably something that can be ignored.
        // Ideally, such IL would just result in InvalidProgramException.
        //
        // For now at least assert that the existing type is the same type we would get from the
        // provided class handle. This catches attempts to change between STRUCT and SIMD types
        // that would leave LclVarDsc in a weird state.

        assert(lcl->GetType() == typGetStructType(layout));
        assert(lcl->lvExactSize == layout->GetSize());
    }
    else
    {
        lcl->lvType = TYP_STRUCT;
        lcl->SetLayout(layout);
        lcl->lvExactSize   = layout->GetSize();
        lcl->lvImpTypeInfo = typeInfo(TI_STRUCT, layout->GetClassHandle());

        if (layout->IsValueClass())
        {
#if FEATURE_SIMD
            var_types simdType = typGetStructType(layout);

            if (simdType != TYP_STRUCT)
            {
                assert(varTypeIsSIMD(simdType));
                lcl->lvType = simdType;
            }
#endif

            // TODO-MIKE-Cleanup: This should be in lvaAllocUserParam but
            // there may be a few places that rely on this being here.
            layout->EnsureHfaInfo(this);
        }
    }

    if (!varTypeIsSIMD(lcl->GetType()))
    {
        // TODO-MIKE-Throughput: ClassLayout already queries class attributes, it should store
        // "overlapping fields" and "unsafe value class" bits so we don't have to do it again.

        unsigned classAttribs = info.compCompHnd->getClassAttribs(layout->GetClassHandle());

        lcl->lvOverlappingFields = (classAttribs & CORINFO_FLG_OVERLAPPING_FIELDS) != 0;

        // Check whether this local is an unsafe value type and requires GS cookie protection.
        // GS checks require the stack to be re-ordered, which can't be done with EnC.
        if (checkUnsafeBuffer && ((classAttribs & CORINFO_FLG_UNSAFE_VALUECLASS) != 0) && !opts.compDbgEnC)
        {
            setNeedsGSSecurityCookie();
            compGSReorderStackLayout = true;
            lcl->lvIsUnsafeBuffer    = true;
        }
    }

#ifndef TARGET_64BIT
    bool doubleAlignHint = false;
#ifdef TARGET_X86
    doubleAlignHint = true;
#endif
    if (info.compCompHnd->getClassAlignmentRequirement(layout->GetClassHandle(), doubleAlignHint) == 8)
    {
        lcl->lvStructDoubleAlign = 1;
    }
#endif

#ifdef DEBUG
    if (JitConfig.EnableExtraSuperPmiQueries())
    {
        makeExtraStructQueries(layout->GetClassHandle(), 2);
    }
#endif
}

#ifdef DEBUG
//------------------------------------------------------------------------
// makeExtraStructQueries: Query the information for the given struct handle.
//
// Arguments:
//    structHandle -- The handle for the struct type we're querying.
//    level        -- How many more levels to recurse.
//
void Compiler::makeExtraStructQueries(CORINFO_CLASS_HANDLE structHandle, int level)
{
    if (level <= 0)
    {
        return;
    }

    ClassLayout* layout = typGetObjLayout(structHandle);

    uint32_t typeFlags = info.compCompHnd->getClassAttribs(structHandle);
    if ((typeFlags & CORINFO_FLG_DONT_PROMOTE) != 0)
    {
        // In AOT ReadyToRun compilation, don't query fields of types
        // outside of the current version bubble.
        return;
    }
    unsigned fieldCnt = info.compCompHnd->getClassNumInstanceFields(structHandle);
    typGetStructType(structHandle);
#ifdef TARGET_ARMARCH
    layout->EnsureHfaInfo(this);
#endif
    for (unsigned int i = 0; i < fieldCnt; i++)
    {
        CORINFO_FIELD_HANDLE fieldHandle      = info.compCompHnd->getFieldInClass(structHandle, i);
        unsigned             fldOffset        = info.compCompHnd->getFieldOffset(fieldHandle);
        CORINFO_CLASS_HANDLE fieldClassHandle = NO_CLASS_HANDLE;
        CorInfoType          fieldCorType     = info.compCompHnd->getFieldType(fieldHandle, &fieldClassHandle);
        var_types            fieldVarType     = JITtype2varType(fieldCorType);
        if (fieldClassHandle != NO_CLASS_HANDLE)
        {
            if (varTypeIsStruct(fieldVarType))
            {
                makeExtraStructQueries(fieldClassHandle, level - 1);
            }
        }
    }
}
#endif // DEBUG

//------------------------------------------------------------------------
// lvaSetClass: set class information for a local var.
//
// Arguments:
//    varNum -- number of the variable
//    clsHnd -- class handle to use in set or update
//    isExact -- true if class is known exactly
//
// Notes:
//    varNum must not already have a ref class handle.

void Compiler::lvaSetClass(unsigned varNum, CORINFO_CLASS_HANDLE clsHnd, bool isExact)
{
    assert(clsHnd != nullptr);

    LclVarDsc* varDsc = lvaGetDesc(varNum);
    assert(varDsc->TypeIs(TYP_REF));

    // We shoud not have any ref type information for this var.
    assert(varDsc->lvClassHnd == NO_CLASS_HANDLE);
    assert(!varDsc->lvClassIsExact);

    JITDUMP("\nlvaSetClass: setting class for V%02i to (%p) %s %s\n", varNum, dspPtr(clsHnd),
            info.compCompHnd->getClassName(clsHnd), isExact ? " [exact]" : "");

    varDsc->lvClassHnd     = clsHnd;
    varDsc->lvClassIsExact = isExact;
}

//------------------------------------------------------------------------
// lvaSetClass: set class information for a local var from a tree or stack type
//
// Arguments:
//    varNum -- number of the variable. Must be a single def local
//    tree  -- tree establishing the variable's value
//    stackHnd -- handle for the type from the evaluation stack
//
// Notes:
//    Preferentially uses the tree's type, when available. Since not all
//    tree kinds can track ref types, the stack type is used as a
//    fallback. If there is no stack type, then the class is set to object.

void Compiler::lvaSetClass(unsigned varNum, GenTree* tree, CORINFO_CLASS_HANDLE stackHnd)
{
    bool                 isExact   = false;
    bool                 isNonNull = false;
    CORINFO_CLASS_HANDLE clsHnd    = gtGetClassHandle(tree, &isExact, &isNonNull);

    if (clsHnd != nullptr)
    {
        lvaSetClass(varNum, clsHnd, isExact);
    }
    else if (stackHnd != nullptr)
    {
        lvaSetClass(varNum, stackHnd);
    }
    else
    {
        lvaSetClass(varNum, impGetObjectClass());
    }
}

//------------------------------------------------------------------------
// lvaUpdateClass: update class information for a local var.
//
// Arguments:
//    varNum -- number of the variable
//    clsHnd -- class handle to use in set or update
//    isExact -- true if class is known exactly
//
// Notes:
//
//    This method models the type update rule for an assignment.
//
//    Updates currently should only happen for single-def user args or
//    locals, when we are processing the expression actually being
//    used to initialize the local (or inlined arg). The update will
//    change the local from the declared type to the type of the
//    initial value.
//
//    These updates should always *improve* what we know about the
//    type, that is making an inexact type exact, or changing a type
//    to some subtype. However the jit lacks precise type information
//    for shared code, so ensuring this is so is currently not
//    possible.

void Compiler::lvaUpdateClass(unsigned varNum, CORINFO_CLASS_HANDLE clsHnd, bool isExact)
{
    assert(varNum < lvaCount);

    // Else we should have a class handle to consider
    assert(clsHnd != nullptr);

    LclVarDsc* varDsc = lvaGetDesc(varNum);
    assert(varDsc->TypeIs(TYP_REF));

    // We should already have a class
    assert(varDsc->lvClassHnd != NO_CLASS_HANDLE);

    // We should only be updating classes for single-def locals.
    assert(varDsc->lvSingleDef);

    // Now see if we should update.
    //
    // New information may not always be "better" so do some
    // simple analysis to decide if the update is worthwhile.
    const bool isNewClass   = (clsHnd != varDsc->lvClassHnd);
    bool       shouldUpdate = false;

    // Are we attempting to update the class? Only check this when we have
    // an new type and the existing class is inexact... we should not be
    // updating exact classes.
    if (!varDsc->lvClassIsExact && isNewClass)
    {
        shouldUpdate = !!info.compCompHnd->isMoreSpecificType(varDsc->lvClassHnd, clsHnd);
    }
    // Else are we attempting to update exactness?
    else if (isExact && !varDsc->lvClassIsExact && !isNewClass)
    {
        shouldUpdate = true;
    }

#if DEBUG
    if (isNewClass || (isExact != varDsc->lvClassIsExact))
    {
        JITDUMP("\nlvaUpdateClass:%s Updating class for V%02u", shouldUpdate ? "" : " NOT", varNum);
        JITDUMP(" from (%p) %s%s", dspPtr(varDsc->lvClassHnd), info.compCompHnd->getClassName(varDsc->lvClassHnd),
                varDsc->lvClassIsExact ? " [exact]" : "");
        JITDUMP(" to (%p) %s%s\n", dspPtr(clsHnd), info.compCompHnd->getClassName(clsHnd), isExact ? " [exact]" : "");
    }
#endif // DEBUG

    if (shouldUpdate)
    {
        varDsc->lvClassHnd     = clsHnd;
        varDsc->lvClassIsExact = isExact;

#if DEBUG
        // Note we've modified the type...
        varDsc->lvClassInfoUpdated = true;
#endif // DEBUG
    }

    return;
}

//------------------------------------------------------------------------
// lvaUpdateClass: Uupdate class information for a local var from a tree
//  or stack type
//
// Arguments:
//    varNum -- number of the variable. Must be a single def local
//    tree  -- tree establishing the variable's value
//    stackHnd -- handle for the type from the evaluation stack
//
// Notes:
//    Preferentially uses the tree's type, when available. Since not all
//    tree kinds can track ref types, the stack type is used as a
//    fallback.

void Compiler::lvaUpdateClass(unsigned varNum, GenTree* tree, CORINFO_CLASS_HANDLE stackHnd)
{
    bool                 isExact   = false;
    bool                 isNonNull = false;
    CORINFO_CLASS_HANDLE clsHnd    = gtGetClassHandle(tree, &isExact, &isNonNull);

    if (clsHnd != nullptr)
    {
        lvaUpdateClass(varNum, clsHnd, isExact);
    }
    else if (stackHnd != nullptr)
    {
        lvaUpdateClass(varNum, stackHnd);
    }
}

// Returns the number of bytes needed on the frame for the local variable lclNum
unsigned LclVarDsc::GetFrameSize() const
{
    switch (lvType)
    {
        case TYP_BLK:
            return roundUp(lvExactSize, REGSIZE_BYTES);

        case TYP_STRUCT:
        {
            unsigned size = m_layout->GetSize();

            if (lvIsParam)
            {
                bool     isFloatHfa = IsHfaParam() && (m_layout->GetHfaElementType() == TYP_FLOAT);
                unsigned alignment  = Compiler::lvaGetParamAlignment(lvType, isFloatHfa);

                return roundUp(size, alignment);
            }

            return roundUp(size, REGSIZE_BYTES);
        }

#ifdef FEATURE_SIMD
        case TYP_SIMD12:
            // TODO-MIKE-Fix: This is messed up in some cases (e.g. on x86 SIMD12 params
            // have only 12 bytes). lvSize() dealt with this but for some reason it was
            // only called for structs. CanWidenSimd12ToSimd16 is broken due to this.
            return 16;
#endif

        default:
#ifdef TARGET_64BIT
            if (lvQuirkToLong)
            {
                noway_assert(varActualTypeIsInt(lvType) && lvAddrExposed);
                return 8;
            }
#endif
            return varTypeSize(varActualType(lvType));
    }
}

// getCalledCount -- get the value used to normalized weights for this method
//  if we don't have profile data then getCalledCount will return BB_UNITY_WEIGHT (100)
//  otherwise it returns the number of times that profile data says the method was called.
//
// static
BasicBlock::weight_t BasicBlock::getCalledCount(Compiler* comp)
{
    // when we don't have profile data then fgCalledCount will be BB_UNITY_WEIGHT (100)
    BasicBlock::weight_t calledCount = comp->fgCalledCount;

    // If we haven't yet reach the place where we setup fgCalledCount it could still be zero
    // so return a reasonable value to use until we set it.
    //
    if (calledCount == 0)
    {
        if (comp->fgIsUsingProfileWeights())
        {
            // When we use profile data block counts we have exact counts,
            // not multiples of BB_UNITY_WEIGHT (100)
            calledCount = 1;
        }
        else
        {
            calledCount = comp->fgFirstBB->bbWeight;

            if (calledCount == 0)
            {
                calledCount = BB_UNITY_WEIGHT;
            }
        }
    }
    return calledCount;
}

// getBBWeight -- get the normalized weight of this block
BasicBlock::weight_t BasicBlock::getBBWeight(Compiler* comp)
{
    if (this->bbWeight == BB_ZERO_WEIGHT)
    {
        return BB_ZERO_WEIGHT;
    }
    else
    {
        weight_t calledCount = getCalledCount(comp);

        // Normalize the bbWeights by multiplying by BB_UNITY_WEIGHT and dividing by the calledCount.
        //
        weight_t fullResult = this->bbWeight * BB_UNITY_WEIGHT / calledCount;

        return fullResult;
    }
}

// LclVarDsc "less" comparer used to compare the weight of two locals, when optimizing for small code.
class LclVarDsc_SmallCode_Less
{
    const LclVarDsc* m_lvaTable;
    INDEBUG(unsigned m_lvaCount;)

public:
    LclVarDsc_SmallCode_Less(const LclVarDsc* lvaTable DEBUGARG(unsigned lvaCount))
        : m_lvaTable(lvaTable)
#ifdef DEBUG
        , m_lvaCount(lvaCount)
#endif
    {
    }

    bool operator()(unsigned n1, unsigned n2)
    {
        assert(n1 < m_lvaCount);
        assert(n2 < m_lvaCount);

        const LclVarDsc* dsc1 = &m_lvaTable[n1];
        const LclVarDsc* dsc2 = &m_lvaTable[n2];

        // We should not be sorting untracked variables
        assert(dsc1->lvTracked);
        assert(dsc2->lvTracked);
        // We should not be sorting after registers have been allocated
        assert(!dsc1->lvRegister);
        assert(!dsc2->lvRegister);

        unsigned weight1 = dsc1->lvRefCnt();
        unsigned weight2 = dsc2->lvRefCnt();

#ifndef TARGET_ARM
        // ARM-TODO: this was disabled for ARM under !FEATURE_FP_REGALLOC; it was probably a left-over from
        // legacy backend. It should be enabled and verified.

        // Force integer candidates to sort above float candidates.
        const bool isFloat1 = varTypeUsesFloatReg(dsc1->GetType());
        const bool isFloat2 = varTypeUsesFloatReg(dsc2->GetType());

        if (isFloat1 != isFloat2)
        {
            if ((weight2 != 0) && isFloat1)
            {
                return false;
            }

            if ((weight1 != 0) && isFloat2)
            {
                return true;
            }
        }
#endif

        if (weight1 != weight2)
        {
            return weight1 > weight2;
        }

        // If the weighted ref counts are different then use their difference.
        if (dsc1->lvRefCntWtd() != dsc2->lvRefCntWtd())
        {
            return dsc1->lvRefCntWtd() > dsc2->lvRefCntWtd();
        }

        // We have equal ref counts and weighted ref counts.
        // Break the tie by:
        //   - Increasing the weight by 2   if we are a register arg.
        //   - Increasing the weight by 0.5 if we are a GC type.
        //
        // Review: seems odd that this is mixing counts and weights.

        if (weight1 != 0)
        {
            if (dsc1->IsRegParam())
            {
                weight1 += 2 * BB_UNITY_WEIGHT_UNSIGNED;
            }

            if (varTypeIsGC(dsc1->TypeGet()))
            {
                weight1 += BB_UNITY_WEIGHT_UNSIGNED / 2;
            }
        }

        if (weight2 != 0)
        {
            if (dsc2->IsRegParam())
            {
                weight2 += 2 * BB_UNITY_WEIGHT_UNSIGNED;
            }

            if (varTypeIsGC(dsc2->TypeGet()))
            {
                weight2 += BB_UNITY_WEIGHT_UNSIGNED / 2;
            }
        }

        if (weight1 != weight2)
        {
            return weight1 > weight2;
        }

        // To achieve a stable sort we use the LclNum (by way of the pointer address).
        return dsc1 < dsc2;
    }
};

// LclVarDsc "less" comparer used to compare the weight of two locals, when optimizing for blended code.
class LclVarDsc_BlendedCode_Less
{
    const LclVarDsc* m_lvaTable;
    INDEBUG(unsigned m_lvaCount;)

public:
    LclVarDsc_BlendedCode_Less(const LclVarDsc* lvaTable DEBUGARG(unsigned lvaCount))
        : m_lvaTable(lvaTable)
#ifdef DEBUG
        , m_lvaCount(lvaCount)
#endif
    {
    }

    bool operator()(unsigned n1, unsigned n2)
    {
        assert(n1 < m_lvaCount);
        assert(n2 < m_lvaCount);

        const LclVarDsc* dsc1 = &m_lvaTable[n1];
        const LclVarDsc* dsc2 = &m_lvaTable[n2];

        // We should not be sorting untracked variables
        assert(dsc1->lvTracked);
        assert(dsc2->lvTracked);
        // We should not be sorting after registers have been allocated
        assert(!dsc1->lvRegister);
        assert(!dsc2->lvRegister);

        BasicBlock::weight_t weight1 = dsc1->lvRefCntWtd();
        BasicBlock::weight_t weight2 = dsc2->lvRefCntWtd();

#ifndef TARGET_ARM
        // ARM-TODO: this was disabled for ARM under !FEATURE_FP_REGALLOC; it was probably a left-over from
        // legacy backend. It should be enabled and verified.

        // Force integer candidates to sort above float candidates.
        const bool isFloat1 = varTypeUsesFloatReg(dsc1->GetType());
        const bool isFloat2 = varTypeUsesFloatReg(dsc2->GetType());

        if (isFloat1 != isFloat2)
        {
            if ((weight2 != 0) && isFloat1)
            {
                return false;
            }

            if ((weight1 != 0) && isFloat2)
            {
                return true;
            }
        }
#endif

        if ((weight1 != 0) && dsc1->IsRegParam())
        {
            weight1 += 2 * BB_UNITY_WEIGHT;
        }

        if ((weight2 != 0) && dsc2->IsRegParam())
        {
            weight2 += 2 * BB_UNITY_WEIGHT;
        }

        if (weight1 != weight2)
        {
            return weight1 > weight2;
        }

        // If the weighted ref counts are different then try the unweighted ref counts.
        if (dsc1->lvRefCnt() != dsc2->lvRefCnt())
        {
            return dsc1->lvRefCnt() > dsc2->lvRefCnt();
        }

        // If one is a GC type and the other is not the GC type wins.
        if (varTypeIsGC(dsc1->TypeGet()) != varTypeIsGC(dsc2->TypeGet()))
        {
            return varTypeIsGC(dsc1->TypeGet());
        }

        // To achieve a stable sort we use the LclNum (by way of the pointer address).
        return dsc1 < dsc2;
    }
};

void Compiler::lvaMarkLivenessTrackedLocals()
{
    assert(opts.OptimizationEnabled() && compEnregLocals());

    lvaTrackedCount             = 0;
    lvaTrackedCountInSizeTUnits = 0;

    if (lvaCount == 0)
    {
        return;
    }

    if (lvaTrackedToVarNumSize < lvaCount)
    {
        lvaTrackedToVarNumSize = lvaCount;
        lvaTrackedToVarNum     = new (getAllocator(CMK_LvaTable)) unsigned[lvaTrackedToVarNumSize];
    }

    unsigned  trackedCount = 0;
    unsigned* tracked      = lvaTrackedToVarNum;

    for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum);

        // Start by assuming that the variable will be tracked.
        lcl->lvTracked = 1;

        if (lcl->GetRefCount() == 0)
        {
            assert(lcl->GetRefWeight() == 0);

            lcl->lvTracked = 0;
        }

        if (lcl->IsPromoted())
        {
            lcl->lvTracked = 0;
        }

        if (lcl->IsAddressExposed())
        {
            assert(lcl->lvDoNotEnregister);

            lcl->lvTracked = 0;
        }

#if defined(JIT32_GCENCODER) && defined(FEATURE_EH_FUNCLETS)
        if (lvaIsOriginalThisArg(lclNum) && (info.compMethodInfo->options & CORINFO_GENERICS_CTXT_FROM_THIS) != 0)
        {
            // For x86/Linux, we need to track "this". However we cannot have it in tracked locals,
            // so we make "this" pointer always untracked.
            lcl->lvTracked = 0;
        }
#endif

        if (lcl->TypeIs(TYP_BLK))
        {
            // BLK locals are rare and rather special (e.g. outgoing args area), it's not worth tracking them.
            // LONG locals are never enregistered on 32 bit targets (if they're promoted their fields may be).
            lcl->lvTracked = 0;
        }

        if (lcl->IsPinning())
        {
            // Pinning locals may not be tracked (a condition of the GCInfo representation)
            // or enregistered, on x86 -- it is believed that we can enregister pinning
            // references when using the general GC encoding.
            lcl->lvTracked = 0;
#ifdef JIT32_GCENCODER
            lvaSetDoNotEnregister(lcl DEBUGARG(DNER_PinningRef));
#endif
        }

        // TODO-MIKE-Cleanup: Implicitly referenced locals should not be tracked. Most have
        // no explicit references in IR so tracking achieves nothing, except eating into the
        // tracking count limit. And lvaStubArgumentVar is weird in that it does have uses
        // in IR but the definition is implicit. This makes it live in and the start of the
        // method so we risk treating it as zero iniitialized in VN if compInitMem is set.
        // Luckily that doesn't happen because generated stubs do not use .localsinit now.

        if (lcl->lvTracked)
        {
            tracked[trackedCount++] = lclNum;
        }

        if (compJmpOpUsed && lcl->IsRegParam())
        {
            // If we have JMP, reg args must be put on the stack
            lvaSetDoNotEnregister(lcl DEBUGARG(DNER_BlockOp));
        }
        else if (lcl->IsDependentPromotedField(this))
        {
            lvaSetDoNotEnregister(lcl DEBUGARG(DNER_DepField));
        }
        else if (lcl->TypeIs(TYP_STRUCT) && !lcl->IsPromoted())
        {
            if (!compEnregStructLocals())
            {
                lvaSetDoNotEnregister(lcl DEBUGARG(DNER_IsStruct));
            }
            else if (lcl->GetRegisterType() == TYP_UNDEF)
            {
                lvaSetDoNotEnregister(lcl DEBUGARG(DNER_IsStruct));
            }
            else if (lcl->HasGCPtr())
            {
                // TODO-1stClassStructs: support vars with GC pointers. The issue is that such
                // vars will have `lvMustInit` set, because emitter has poor support for struct
                // liveness, but if the variable is tracked the prolog generator would expect it
                // to be in liveIn set, so an assert in `genFnProlog` will fire.
                lvaSetDoNotEnregister(lcl DEBUGARG(DNER_IsStruct));
            }
            else if (lcl->lvIsMultiRegArg || lcl->lvIsMultiRegRet)
            {
                // Prolog and return generators do not support vector - integer register moves.
                lvaSetDoNotEnregister(lcl DEBUGARG(DNER_IsStructArg));
            }
#if defined(TARGET_ARM) || defined(TARGET_X86)
            else if (lcl->IsParam())
            {
                // On ARM we prespill all struct args.
                // TODO-ARM-CQ: Keep them in registers, it will need a fix to
                // "On the ARM we will spill any incoming struct args" logic in codegencommon.
                // TODO-MIKE-CQ: This also affects x86, not clear how come main
                // doesn't have this problem. Probably because they still can't generate sane IR
                // to begin with and end up DNERing structs anyway...
                lvaSetDoNotEnregister(lcl DEBUGARG(DNER_IsStructArg));
            }
#endif
        }
    }

    if (compCodeOpt() == SMALL_CODE)
    {
        jitstd::sort(tracked, tracked + trackedCount, LclVarDsc_SmallCode_Less(lvaTable DEBUGARG(lvaCount)));
    }
    else
    {
        jitstd::sort(tracked, tracked + trackedCount, LclVarDsc_BlendedCode_Less(lvaTable DEBUGARG(lvaCount)));
    }

    lvaTrackedCount = min(static_cast<unsigned>(JitConfig.JitMaxLocalsToTrack()), trackedCount);

    JITDUMP("Tracked local (%u out of %u) table:\n", lvaTrackedCount, lvaCount);

    for (unsigned trackedIndex = 0; trackedIndex < lvaTrackedCount; trackedIndex++)
    {
        LclVarDsc* lcl = lvaGetDesc(tracked[trackedIndex]);

        assert(lcl->lvTracked);
        lcl->lvVarIndex = static_cast<uint16_t>(trackedIndex);

        DBEXEC(verbose, gtDispLclVar(tracked[trackedIndex]))
        JITDUMP("Tracked V%02u: refCnt = %4u, refCntWtd = %6s\n", tracked[trackedIndex], lcl->lvRefCnt(),
                refCntWtd2str(lcl->lvRefCntWtd()));
    }

    // If we have too many tracked locals mark the rest as untracked. This does not remove
    // them from lvaTrackedToVarNum but lvaTrackedCount reflects the actual tracked count.
    for (unsigned trackedIndex = lvaTrackedCount; trackedIndex < trackedCount; trackedIndex++)
    {
        LclVarDsc* lcl = lvaGetDesc(tracked[trackedIndex]);

        assert(lcl->lvTracked);
        lcl->lvTracked = 0;

        DBEXEC(verbose, gtDispLclVar(tracked[trackedIndex]))
        JITDUMP("Untracked V%02u: refCnt = %4u, refCntWtd = %6s\n", tracked[trackedIndex], lcl->lvRefCnt(),
                refCntWtd2str(lcl->lvRefCntWtd()));
    }

    JITDUMP("\n");

    lvaCurEpoch++;
    lvaTrackedCountInSizeTUnits =
        roundUp(lvaTrackedCount, static_cast<unsigned>(sizeof(size_t) * 8)) / static_cast<unsigned>(sizeof(size_t) * 8);
}

//------------------------------------------------------------------------
// GetRegisterType: Determine register type for this local var.
//
// Arguments:
//    tree - node that uses the local, its type is checked first.
//
// Return Value:
//    TYP_UNDEF if the layout is not enregistrable, the register type otherwise.
//
var_types LclVarDsc::GetRegisterType(const GenTreeLclVarCommon* tree) const
{
    var_types targetType = tree->gtType;
    var_types lclVarType = TypeGet();

    if (targetType == TYP_STRUCT)
    {
        if (lclVarType == TYP_STRUCT)
        {
            assert(!tree->IsLclFld() && "do not expect struct local fields.");
            lclVarType = GetLayout()->GetRegisterType();
        }
        targetType = lclVarType;
    }

#ifdef DEBUG
    if ((targetType != TYP_UNDEF) && tree->OperIs(GT_STORE_LCL_VAR) && lvNormalizeOnStore())
    {
        const bool phiStore = (tree->gtGetOp1()->OperIsNonPhiLocal() == false);
        // Ensure that the lclVar node is typed correctly,
        // does not apply to phi-stores because they do not produce code in the merge block.
        assert(phiStore || targetType == genActualType(lclVarType));
    }
#endif
    return targetType;
}

//------------------------------------------------------------------------
// GetRegisterType: Determine register type for this local var.
//
// Return Value:
//    TYP_UNDEF if the layout is not enregistrable, the register type otherwise.
//
var_types LclVarDsc::GetRegisterType() const
{
    if (TypeGet() != TYP_STRUCT)
    {
#if !defined(TARGET_64BIT)
        if (TypeGet() == TYP_LONG)
        {
            return TYP_UNDEF;
        }
#endif
        return TypeGet();
    }
    assert(m_layout != nullptr);
    return m_layout->GetRegisterType();
}

//------------------------------------------------------------------------
// GetActualRegisterType: Determine an actual register type for this local var.
//
// Return Value:
//    TYP_UNDEF if the layout is not enregistrable, the register type otherwise.
//
var_types LclVarDsc::GetActualRegisterType() const
{
    return genActualType(GetRegisterType());
}

void Compiler::lvaAddRef(LclVarDsc* lcl, BasicBlock::weight_t weight, bool propagate)
{
    assert(opts.OptimizationEnabled());
    assert(lvaRefCountState == RCS_NORMAL);

    if (!lcl->TypeIs(TYP_STRUCT) || !lcl->IsIndependentPromoted())
    {
        lcl->SetRefCount(lcl->GetRefCount() + 1);

        if (weight != 0)
        {
            // We double the weight of internal temps
            bool doubleWeight = lcl->lvIsTemp;
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
            // and, for the time being, implicit byref params
            doubleWeight |= lcl->lvIsImplicitByRef;
#endif

            if (doubleWeight && (weight * 2 > weight))
            {
                weight *= 2;
            }

            BasicBlock::weight_t newWeight = lcl->GetRefWeight() + weight;
            assert(newWeight >= lcl->GetRefWeight());
            lcl->SetRefWeight(newWeight);
        }
    }

    if (propagate)
    {
        if (lcl->IsPromotedField())
        {
            LclVarDsc* parentLcl = lvaGetDesc(lcl->GetPromotedFieldParentLclNum());

            if (parentLcl->IsDependentPromoted())
            {
                lvaAddRef(parentLcl, weight, false);
            }
        }
        else if (lcl->IsPromoted() && varTypeIsStruct(lcl->GetType()))
        {
            for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
            {
                lvaAddRef(lvaGetDesc(lcl->GetPromotedFieldLclNum(i)), weight, false);
            }
        }
    }

    JITDUMP("New refCnts for V%02u: refCnt = %2u, refCntWtd = %s\n", static_cast<unsigned>(lcl - lvaTable),
            lcl->lvRefCnt(), refCntWtd2str(lcl->lvRefCntWtd()));
}

//------------------------------------------------------------------------
// IsDominatedByExceptionalEntry: Check is the block dominated by an exception entry block.
//
// Arguments:
//    block - the checking block.
//
bool Compiler::IsDominatedByExceptionalEntry(BasicBlock* block)
{
    assert(fgDomsComputed);
    return block->IsDominatedByExceptionalEntryFlag();
}

void Compiler::lvaComputeRefCountsHIR()
{
    class MarkLocalVarsVisitor final : public GenTreeVisitor<MarkLocalVarsVisitor>
    {
        BasicBlock*          m_block;
        BasicBlock::weight_t m_weight;
        Statement*           m_stmt;

    public:
        enum
        {
            DoPreOrder = true,
        };

        MarkLocalVarsVisitor(Compiler* compiler) : GenTreeVisitor<MarkLocalVarsVisitor>(compiler)
        {
        }

        void Visit()
        {
            for (BasicBlock* block : m_compiler->Blocks())
            {
                m_block  = block;
                m_weight = block->getBBWeight(m_compiler);

                JITDUMP("Marking local variables in block " FMT_BB " (weight %s)\n", block->bbNum,
                        refCntWtd2str(m_weight));

                for (Statement* stmt : block->NonPhiStatements())
                {
                    m_stmt = stmt;

                    DISPSTMT(stmt);
                    WalkTree(stmt->GetRootNodePointer(), nullptr);
                }
            }
        }

        Compiler::fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
        {
            GenTree* node = *use;

            switch (node->GetOper())
            {
#if OPT_BOOL_OPS
                case GT_ASG:
                {
                    GenTree* op1 = node->AsOp()->GetOp(0);
                    GenTree* op2 = node->AsOp()->GetOp(1);

                    if (op1->OperIs(GT_LCL_VAR) && !op2->TypeIs(TYP_BOOL) && !op2->OperIsCompare() &&
                        !op2->IsIntegralConst(0) && !op2->IsIntegralConst(1))
                    {
                        m_compiler->lvaGetDesc(op1->AsLclVar())->lvIsBoolean = false;
                    }
                }
                break;
#endif

                case GT_LCL_VAR_ADDR:
                case GT_LCL_FLD_ADDR:
                {
                    LclVarDsc* lcl = m_compiler->lvaGetDesc(node->AsLclVarCommon());
                    assert(lcl->IsAddressExposed());
#if ASSERTION_PROP
                    DisqualifyAddCopy(lcl);
#endif
                    m_compiler->lvaAddRef(lcl, 0);
                }
                break;

                case GT_LCL_VAR:
                case GT_LCL_FLD:
                    MarkLclRefs(node->AsLclVarCommon(), user);
                    break;

                default:
                    break;
            }

            return WALK_CONTINUE;
        }

        void MarkLclRefs(GenTreeLclVarCommon* node, GenTree* user)
        {
            unsigned   lclNum = node->GetLclNum();
            LclVarDsc* lcl    = m_compiler->lvaGetDesc(lclNum);

            m_compiler->lvaAddRef(lcl, m_weight);

            if (lcl->IsAddressExposed() || node->OperIs(GT_LCL_FLD))
            {
                lcl->lvIsBoolean = false;
#if ASSERTION_PROP
                DisqualifyAddCopy(lcl);
#endif
            }

            if (node->OperIs(GT_LCL_FLD))
            {
                assert((node->gtFlags & GTF_VAR_CONTEXT) == 0);

                return;
            }

            if (((node->gtFlags & GTF_VAR_CONTEXT) != 0) && !m_compiler->lvaGenericsContextInUse)
            {
                JITDUMP("Generic context in use at [%06u]\n", node->GetID());
                m_compiler->lvaGenericsContextInUse = true;
            }

            noway_assert((node->GetType() == lcl->GetType()) ||
                         (node->TypeIs(TYP_INT) && varTypeIsSmall(lcl->GetType())) ||
                         (node->TypeIs(TYP_UBYTE) && lcl->TypeIs(TYP_BOOL)) ||
                         (node->TypeIs(TYP_BYREF) && lcl->TypeIs(TYP_I_IMPL)) ||
                         (node->TypeIs(TYP_I_IMPL) && lcl->TypeIs(TYP_BYREF)) ||
                         (node->TypeIs(TYP_INT) && lcl->TypeIs(TYP_LONG) && (node->gtFlags & GTF_VAR_DEF) == 0));

            if (m_compiler->fgDomsComputed && m_compiler->IsDominatedByExceptionalEntry(m_block))
            {
                lcl->lvEHLive = true;
            }

#if ASSERTION_PROP
            if (!lcl->lvDisqualifyAddCopy)
            {
                if ((node->gtFlags & GTF_VAR_DEF) != 0)
                {
                    // TODO-MIKE-Consider: "single def" doesn't apply to address exposed locals.
                    // There's a pretty good chance that a local that's not AX will be in SSA,
                    // can we simply check the SSA def count instead?

                    if (lcl->lvSingleDef)
                    {
                        // It's already single-def so this must be a second def.
                        DisqualifyAddCopy(lcl);
                    }
                    else
                    {
                        // It's neither single-def nor disqualified, this must be the first def.
                        lcl->lvSingleDef = true;
                        lcl->lvDefStmt   = m_stmt;
                    }
                }
                else
                {
                    if (BlockSetOps::MayBeUninit(lcl->lvUseBlocks))
                    {
                        lcl->lvUseBlocks = BlockSetOps::MakeEmpty(m_compiler);
                    }

                    BlockSetOps::AddElemD(m_compiler, lcl->lvUseBlocks, m_block->bbNum);
                }
            }
#endif // ASSERTION_PROP

            if (!lcl->lvDisqualifySingleDefRegCandidate && ((node->gtFlags & GTF_VAR_DEF) != 0))
            {
                bool bbInALoop  = (m_block->bbFlags & BBF_BACKWARD_JUMP) != 0;
                bool bbIsReturn = m_block->bbJumpKind == BBJ_RETURN;

                // TODO: Zero-inits in LSRA are created with below condition. Try to use
                // similar condition here as well.
                // if (compiler->info.compInitMem || varTypeIsGC(lcl->TypeGet()))

                bool needsExplicitZeroInit = m_compiler->fgVarNeedsExplicitZeroInit(lclNum, bbInALoop, bbIsReturn);

                // TODO-MIKE-Review: Disabling single def reg stuff for lvIsMultiRegRet, it seems
                // broken. For a multireg store lvSingleDefRegCandidate probably needs to be set
                // on the fields of the local.

                if (lcl->lvSingleDefRegCandidate || needsExplicitZeroInit || lcl->lvIsMultiRegRet)
                {
                    JITDUMP("V%02u %s. Disqualified as a single-def register candidate.\n", lclNum,
                            needsExplicitZeroInit ? "needs explicit zero init" : "has multiple definitions");

                    INDEBUG(lcl->lvSingleDefDisqualifyReason = needsExplicitZeroInit ? 'Z' : 'M');
                    lcl->lvSingleDefRegCandidate           = false;
                    lcl->lvDisqualifySingleDefRegCandidate = true;
                }
                else
                {
#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
                    // TODO-CQ: If the varType needs partial callee save, conservatively do not enregister
                    // such variable. In future, need to enable enregisteration for such variables.
                    if (!varTypeNeedsPartialCalleeSave(lcl->GetRegisterType()))
#endif
                    {
                        lcl->lvSingleDefRegCandidate = true;

                        JITDUMP("Marking EH V%02u as a single-def register candidate.\n", lclNum);
                    }
                }
            }
        }

        void DisqualifyAddCopy(LclVarDsc* lcl)
        {
#if ASSERTION_PROP
            lcl->lvDisqualifyAddCopy = true;
            lcl->lvSingleDef         = false;
            lcl->lvDefStmt           = nullptr;
#endif
        }
    };

    MarkLocalVarsVisitor visitor(this);
    visitor.Visit();
}

void Compiler::lvaComputeRefCountsLIR()
{
    for (BasicBlock* block : Blocks())
    {
        const BasicBlock::weight_t weight = block->getBBWeight(this);

        for (GenTree* node : LIR::AsRange(block))
        {
            auto       refWeight = weight;
            LclVarDsc* lcl       = nullptr;

            switch (node->GetOper())
            {
                case GT_LCL_VAR_ADDR:
                case GT_LCL_FLD_ADDR:
                    refWeight = 0;
                    lcl       = lvaGetDesc(node->AsLclVarCommon());
                    break;

                case GT_LCL_VAR:
                case GT_LCL_FLD:
                    if ((node->gtFlags & GTF_VAR_CONTEXT) != 0)
                    {
                        assert(node->OperIs(GT_LCL_VAR));
                        lvaGenericsContextInUse = true;
                    }

                    lcl = lvaGetDesc(node->AsLclVarCommon());
                    break;

                case GT_STORE_LCL_VAR:
                case GT_STORE_LCL_FLD:
                    assert((node->gtFlags & GTF_VAR_CONTEXT) == 0);
                    lcl = lvaGetDesc(node->AsLclVarCommon());

                    // If this is an EH var, use a zero weight for defs, so that we don't
                    // count those in our heuristic for register allocation, since they always
                    // must be stored, so there's no value in enregistering them at defs; only
                    // if there are enough uses to justify it.
                    if (lcl->lvLiveInOutOfHndlr && !lcl->lvDoNotEnregister)
                    {
                        refWeight = 0;
                    }
                    break;

                default:
                    break;
            }

            if (lcl != nullptr)
            {
                lvaAddRef(lcl, refWeight);
            }
        }
    }
}

//------------------------------------------------------------------------
// lvaMarkLocalVars: enable normal ref counting, compute initial counts, sort locals table
//
// Notes:
//    Now behaves differently in minopts / debug. Instead of actually inspecting
//    the IR and counting references, the jit assumes all locals are referenced
//    and does not sort the locals table.
//
//    Also, when optimizing, lays the groundwork for assertion prop and more.
//    See details in lvaMarkLclRefs.

void Compiler::lvaMarkLocalVars()
{
#if !defined(FEATURE_EH_FUNCLETS)

    // Grab space for exception handling

    if (ehNeedsShadowSPslots())
    {
        // The first slot is reserved for ICodeManager::FixContext(ppEndRegion)
        // ie. the offset of the end-of-last-executed-filter
        unsigned slotsNeeded = 1;

        unsigned handlerNestingLevel = ehMaxHndNestingCount;

        if (opts.compDbgEnC && (handlerNestingLevel < (unsigned)MAX_EnC_HANDLER_NESTING_LEVEL))
            handlerNestingLevel = (unsigned)MAX_EnC_HANDLER_NESTING_LEVEL;

        slotsNeeded += handlerNestingLevel;

        // For a filter (which can be active at the same time as a catch/finally handler)
        slotsNeeded++;
        // For zero-termination of the shadow-Stack-pointer chain
        slotsNeeded++;

        lvaShadowSPslotsVar = lvaGrabTemp(false DEBUGARG("ShadowSPslots"));
        lvaGetDesc(lvaShadowSPslotsVar)->SetBlockType(slotsNeeded * REGSIZE_BYTES);
        lvaSetImplicitlyReferenced(lvaShadowSPslotsVar);
    }

#endif // !FEATURE_EH_FUNCLETS

    // PSPSym and LocAllocSPvar are not used by the CoreRT ABI
    if (!IsTargetAbi(CORINFO_CORERT_ABI))
    {
#if defined(FEATURE_EH_FUNCLETS)
        if (ehNeedsPSPSym())
        {
            lvaPSPSym = lvaNewTemp(TYP_I_IMPL, false DEBUGARG("PSPSym"));
            lvaSetImplicitlyReferenced(lvaPSPSym);
        }
#endif // FEATURE_EH_FUNCLETS

#ifdef JIT32_GCENCODER
        // LocAllocSPvar is only required by the implicit frame layout expected by the VM on x86. Whether
        // a function contains a Localloc is conveyed in the GC information, in the InfoHdrSmall.localloc
        // field. The function must have an EBP frame. Then, the VM finds the LocAllocSP slot by assuming
        // the following stack layout:
        //
        //      -- higher addresses --
        //      saved EBP                       <-- EBP points here
        //      other callee-saved registers    // InfoHdrSmall.savedRegsCountExclFP specifies this size
        //      optional GS cookie              // InfoHdrSmall.security is 1 if this exists
        //      LocAllocSP slot
        //      -- lower addresses --
        //
        // See also eetwain.cpp::GetLocallocSPOffset() and its callers.
        if (compLocallocUsed)
        {
            lvaLocAllocSPvar = lvaNewTemp(TYP_I_IMPL, false DEBUGARG("LocAllocSP"));
            lvaSetImplicitlyReferenced(lvaLocAllocSPvar);
        }
#endif // JIT32_GCENCODER
    }

    // Ref counting is now enabled normally.
    lvaRefCountState = RCS_NORMAL;

    if (opts.OptimizationDisabled())
    {
        // If we don't optimize we make all locals implicitly referenced.
        lvaSetImplictlyReferenced();
        return;
    }

    lvaComputeLclRefCounts();

    const bool reportParamTypeArg = lvaReportParamTypeArg();
    LclVarDsc* paramTypeLcl       = nullptr;

    if (lvaKeepAliveAndReportThis())
    {
        paramTypeLcl = lvaGetDesc(0u);
    }
    else if (reportParamTypeArg)
    {
        paramTypeLcl = lvaGetDesc(info.compTypeCtxtArg);
    }

    if (paramTypeLcl != nullptr)
    {
        // TODO-MIKE-Review: There's something dubious going on here, for lvaKeepAliveAndReportThis
        // it sets "implicitly referenced" based on lvaReportParamTypeArg, which could be false.
        // But lvImplicitlyReferenced wasn't previously set on `this` so doing this has no effect.
        // Was the intention to always set lvImplicitlyReferenced to `true` perhaps?

        paramTypeLcl->lvImplicitlyReferenced = reportParamTypeArg;

        if (reportParamTypeArg && (paramTypeLcl->GetRefCount() == 0))
        {
            paramTypeLcl->SetRefCount(1);
            paramTypeLcl->SetRefWeight(BB_UNITY_WEIGHT);
        }
    }
}

// Check for a stack passed parameter in a X86 varargs method.
// Such parameters are not accessed directly, they're accessed via an indirection
// based on the address passed in lvaVarargsHandleArg and they cannot be tracked
// by GC (their offsets in the stack are not known at compile time).
bool Compiler::lvaIsX86VarargsStackParam(unsigned lclNum)
{
#ifdef TARGET_X86
    LclVarDsc* lcl = lvaGetDesc(lclNum);
    return info.compIsVarArgs && lcl->IsParam() && !lcl->IsRegParam() && (lclNum != lvaVarargsHandleArg);
#else
    return false;
#endif
}

void Compiler::lvaSetImplictlyReferenced()
{
    assert(opts.OptimizationDisabled());
    assert(!compRationalIRForm);

    for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum);

        noway_assert(varTypeIsValidLclType(lcl->GetType()));
        assert(!lcl->lvTracked);

        // X86 varargs stack params must remain unreferenced.
        if (lvaIsX86VarargsStackParam(lclNum))
        {
            assert(!lcl->lvImplicitlyReferenced);
            assert(lcl->GetRefCount() == 0);
            assert(lcl->GetRefWeight() == 0);

            continue;
        }

        lcl->lvDoNotEnregister      = true;
        lcl->lvImplicitlyReferenced = true;
        lcl->SetRefCount(1);
        lcl->SetRefWeight(BB_UNITY_WEIGHT);
    }
}

void Compiler::lvaComputeLclRefCounts()
{
    JITDUMP("\n*** lvaComputeLclRefCounts ***\n");

    assert(opts.OptimizationEnabled());

    // First, reset all explicit ref counts and weights.

    for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum);

        noway_assert(varTypeIsValidLclType(lcl->GetType()));

        if (lcl->lvImplicitlyReferenced && !lvaIsX86VarargsStackParam(lclNum))
        {
            lcl->SetRefCount(1);
            lcl->SetRefWeight(BB_UNITY_WEIGHT);
        }
        else
        {
            lcl->SetRefCount(0);
            lcl->SetRefWeight(BB_ZERO_WEIGHT);
        }

        // TODO-MIKE-Review: lvSingleDef isn't used in LIR so we might as well set it and be done with it.
        // lvSingleDefRegCandidate is bizarre. It's mainly a LSRA thing yet we're computing it before LIR.
        // It can influence DNER so computing it early may be a good thing but it's still a bit odd that
        // it is not recomputed. Are we guaranteed that there's transform that introduces new defs? Loop
        // cloning/unrolling could do that, but it looks like this loop defs aren't candidates.
        if (!compRationalIRForm)
        {
            lcl->lvSingleDef             = lcl->IsParam() || info.compInitMem;
            lcl->lvSingleDefRegCandidate = lcl->IsParam();
        }
    }

    // Second, count all explicit local variable references. This will also set
    // lvaGenericsContextInUse again if the generics context is still used.

    INDEBUG(const bool oldGenericsContextInUse = lvaGenericsContextInUse);
    lvaGenericsContextInUse = false;

    if (compRationalIRForm)
    {
        lvaComputeRefCountsLIR();
    }
    else
    {
        lvaComputeRefCountsHIR();
    }

#ifdef DEBUG
    if (oldGenericsContextInUse && !lvaGenericsContextInUse)
    {
        JITDUMP("\n** Generics context no longer in use\n");
    }
    else if (lvaGenericsContextInUse && !oldGenericsContextInUse)
    {
        assert(!"New generic context use pulled ouf of a hat");
    }
#endif

    // Third, bump ref counts for some implicit prolog references

    for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum);

        // TODO-MIKE-Review: It seems like nobody knows why is this done...
        if (lcl->IsRegParam())
        {
            if ((lclNum < info.compArgsCount) && (lcl->GetRefCount() > 0))
            {
                lvaAddRef(lcl, BB_UNITY_WEIGHT);
                lvaAddRef(lcl, BB_UNITY_WEIGHT);
            }

            if (lcl->IsPromotedField())
            {
                lvaAddRef(lcl, BB_UNITY_WEIGHT);
            }
        }

        if (compJmpOpUsed && lcl->IsParam())
        {
            // If we have JMP, all parameters must have a location even if we don't use them
            // inside the method. Except when we have varargs and the argument is passed on
            // the stack. In that case, it's important for the ref count to be zero, so that
            // we don't attempt to track them for GC info (which is not possible since we
            // don't know their offset in the stack). See the assert at the end of raMarkStkVars.
            if ((lcl->GetRefCount() == 0) && !lvaIsX86VarargsStackParam(lclNum))
            {
                lcl->lvImplicitlyReferenced = true;
                lcl->SetRefCount(1);
                lcl->SetRefWeight(BB_UNITY_WEIGHT);
            }
        }

#ifndef TARGET_64BIT
        if (compRationalIRForm && lcl->TypeIs(TYP_LONG) && !lcl->IsPromoted())
        {
            lvaSetDoNotEnregister(lcl DEBUGARG(Compiler::DNER_LongUnpromoted));
        }
#endif
    }
}

void Compiler::lvaIncrementFrameSize(unsigned size)
{
    // Limit frames size to 1GB. The maximum is 2GB in theory - make
    // it intentionally smaller to avoid bugs from borderline cases.
    constexpr unsigned MaxFrameSize = 0x3FFFFFFF;

    if ((size > MaxFrameSize) || (codeGen->lclFrameSize + size > MaxFrameSize))
    {
        BADCODE("Frame size overflow");
    }

    JITDUMP("lclFrameSize: %u + %u\n", codeGen->lclFrameSize, size);

    codeGen->lclFrameSize += size;
}

/****************************************************************************
*
*  Return true if absolute offsets of temps are larger than vars, or in other
*  words, did we allocate temps before of after vars.  The /GS buffer overrun
*  checks want temps to be at low stack addresses than buffers
*/
bool Compiler::lvaTempsHaveLargerOffsetThanVars()
{
#ifdef TARGET_ARM
    // We never want to place the temps with larger offsets for ARM
    return false;
#else
    if (compGSReorderStackLayout)
    {
        return codeGen->isFramePointerUsed();
    }
    else
    {
        return true;
    }
#endif
}

// Returns the number of bytes required for the given parameter.
// Usually this is just the param type size, rounded up to the register size
// but there are special case like implicit by ref params and osx-arm64 weird
// parameter packing.
unsigned Compiler::lvaGetParamAllocSize(LclVarDsc* lcl)
{
    assert(lcl->IsParam());

#ifndef TARGET_64BIT
    if (lcl->TypeIs(TYP_LONG, TYP_DOUBLE))
    {
        return 2 * REGSIZE_BYTES;
    }
#endif

#ifndef WINDOWS_AMD64_ABI
    if (varTypeIsStruct(lcl->GetType()))
    {
        ClassLayout* layout = lcl->GetLayout();
        unsigned     size   = layout->GetSize();

#ifdef TARGET_ARM64
        if (size <= MAX_PASS_MULTIREG_BYTES)
        {
            layout->EnsureHfaInfo(this);

#ifdef TARGET_WINDOWS
            if (layout->IsHfa() && !info.compIsVarArgs)
#else
            if (layout->IsHfa())
#endif
            {
#ifndef OSX_ARM64_ABI
                size = roundUp(size, REGSIZE_BYTES);
#endif
                return size;
            }
        }

        if (size > 2 * REGSIZE_BYTES)
        {
            assert(lcl->IsImplicitByRefParam());

            return REGSIZE_BYTES;
        }
#endif // TARGET_ARM64

        return roundUp(size, REGSIZE_BYTES);
    }
#endif // WINDOWS_AMD64_ABI

    assert((lcl->GetTypeSize() <= REGSIZE_BYTES) || lcl->IsImplicitByRefParam());

    return REGSIZE_BYTES;
}

// Return alignment for a parameter of the given type.
// It currently doesn't return smaller than required alignment for arm32 (4 bytes for double and int64)
// but it does not lead to issues because its alignment requirements are satisfied in other code parts.
// TODO: fix this function and delete the other code that is handling this.
unsigned Compiler::lvaGetParamAlignment(var_types type, bool isFloatHfa)
{
#ifdef OSX_ARM64_ABI
    if (isFloatHfa)
    {
        assert(varTypeIsStruct(type));
        return 4;
    }

    if (!varTypeIsStruct(type))
    {
        unsigned size = varTypeSize(type);
        assert((0 < size) && (size <= REGSIZE_BYTES));
        return size;
    }
#endif

    // TODO-MIKE-Review: Looks like vector params have incorrect alignment on ARM64.

    return REGSIZE_BYTES;
}

// Check whether the variable is never zero initialized in the prolog.
bool Compiler::lvaIsNeverZeroInitializedInProlog(unsigned lclNum)
{
    if ((lclNum == lvaGSSecurityCookie) ||
#if FEATURE_FIXED_OUT_ARGS
        (lclNum == lvaOutgoingArgSpaceVar) ||
#endif
#ifdef FEATURE_EH_FUNCLETS
        (lclNum == lvaPSPSym) ||
#endif
        (lclNum == lvaInlinedPInvokeFrameVar) || (lclNum == lvaStubArgumentVar) || (lclNum == lvaRetAddrVar) ||
        lvaIsOSRLocal(lclNum))
    {
        return true;
    }

    return lvaGetDesc(lclNum)->IsParam();
}

// clang-format off
/*****************************************************************************
 *
 *  Compute stack frame offsets for arguments, locals and optionally temps.
 *
 *  The frame is laid out as follows for x86:
 *
 *              ESP frames
 *
 *      |                       |
 *      |-----------------------|
 *      |       incoming        |
 *      |       arguments       |
 *      |-----------------------| <---- Virtual '0'
 *      |    return address     |
 *      +=======================+
 *      |Callee saved registers |
 *      |-----------------------|
 *      |       Temps           |
 *      |-----------------------|
 *      |       Variables       |
 *      |-----------------------| <---- Ambient ESP
 *      |   Arguments for the   |
 *      ~    next function      ~
 *      |                       |
 *      |       |               |
 *      |       | Stack grows   |
 *              | downward
 *              V
 *
 *
 *              EBP frames
 *
 *      |                       |
 *      |-----------------------|
 *      |       incoming        |
 *      |       arguments       |
 *      |-----------------------| <---- Virtual '0'
 *      |    return address     |
 *      +=======================+
 *      |    incoming EBP       |
 *      |-----------------------| <---- EBP
 *      |Callee saved registers |
 *      |-----------------------|
 *      |   security object     |
 *      |-----------------------|
 *      |     ParamTypeArg      |
 *      |-----------------------|
 *      |  Last-executed-filter |
 *      |-----------------------|
 *      |                       |
 *      ~      Shadow SPs       ~
 *      |                       |
 *      |-----------------------|
 *      |                       |
 *      ~      Variables        ~
 *      |                       |
 *      ~-----------------------|
 *      |       Temps           |
 *      |-----------------------|
 *      |       localloc        |
 *      |-----------------------| <---- Ambient ESP
 *      |   Arguments for the   |
 *      |    next function      ~
 *      |                       |
 *      |       |               |
 *      |       | Stack grows   |
 *              | downward
 *              V
 *
 *
 *  The frame is laid out as follows for x64:
 *
 *              RSP frames
 *      |                       |
 *      |-----------------------|
 *      |       incoming        |
 *      |       arguments       |
 *      |-----------------------|
 *      |   4 fixed incoming    |
 *      |    argument slots     |
 *      |-----------------------| <---- Caller's SP & Virtual '0'
 *      |    return address     |
 *      +=======================+
 *      | Callee saved Int regs |
 *      -------------------------
 *      |        Padding        | <---- this padding (0 or 8 bytes) is to ensure flt registers are saved at a mem location aligned at 16-bytes
 *      |                       |       so that we can save 128-bit callee saved xmm regs using performant "movaps" instruction instead of "movups"
 *      -------------------------
 *      | Callee saved Flt regs | <----- entire 128-bits of callee saved xmm registers are stored here
 *      |-----------------------|
 *      |         Temps         |
 *      |-----------------------|
 *      |       Variables       |
 *      |-----------------------|
 *      |   Arguments for the   |
 *      ~    next function      ~
 *      |                       |
 *      |-----------------------|
 *      |   4 fixed outgoing    |
 *      |    argument slots     |
 *      |-----------------------| <---- Ambient RSP
 *      |       |               |
 *      ~       | Stack grows   ~
 *      |       | downward      |
 *              V
 *
 *
 *              RBP frames
 *      |                       |
 *      |-----------------------|
 *      |       incoming        |
 *      |       arguments       |
 *      |-----------------------|
 *      |   4 fixed incoming    |
 *      |    argument slots     |
 *      |-----------------------| <---- Caller's SP & Virtual '0'
 *      |    return address     |
 *      +=======================+
 *      | Callee saved Int regs |
 *      -------------------------
 *      |        Padding        |
 *      -------------------------
 *      | Callee saved Flt regs |
 *      |-----------------------|
 *      |   security object     |
 *      |-----------------------|
 *      |     ParamTypeArg      |
 *      |-----------------------|
 *      |                       |
 *      |                       |
 *      ~       Variables       ~
 *      |                       |
 *      |                       |
 *      |-----------------------|
 *      |        Temps          |
 *      |-----------------------|
 *      |                       |
 *      ~       localloc        ~   // not in frames with EH
 *      |                       |
 *      |-----------------------|
 *      |        PSPSym         |   // only in frames with EH (thus no localloc)
 *      |                       |
 *      |-----------------------| <---- RBP in localloc frames (max 240 bytes from Initial-SP)
 *      |   Arguments for the   |
 *      ~    next function      ~
 *      |                       |
 *      |-----------------------|
 *      |   4 fixed outgoing    |
 *      |    argument slots     |
 *      |-----------------------| <---- Ambient RSP (before localloc, this is Initial-SP)
 *      |       |               |
 *      ~       | Stack grows   ~
 *      |       | downward      |
 *              V
 *
 *
 *  The frame is laid out as follows for ARM (this is a general picture; details may differ for different conditions):
 *
 *              SP frames
 *      |                       |
 *      |-----------------------|
 *      |       incoming        |
 *      |       arguments       |
 *      +=======================+ <---- Caller's SP
 *      |  Pre-spill registers  |
 *      |-----------------------| <---- Virtual '0'
 *      |Callee saved registers |
 *      |-----------------------|
 *      ~ possible double align ~
 *      |-----------------------|
 *      |   security object     |
 *      |-----------------------|
 *      |     ParamTypeArg      |
 *      |-----------------------|
 *      |  possible GS cookie   |
 *      |-----------------------|
 *      |       Variables       |
 *      |-----------------------|
 *      |  possible GS cookie   |
 *      |-----------------------|
 *      |        Temps          |
 *      |-----------------------|
 *      |   Stub Argument Var   |
 *      |-----------------------|
 *      |Inlined PInvoke Frame V|
 *      |-----------------------|
 *      ~ possible double align ~
 *      |-----------------------|
 *      |   Arguments for the   |
 *      ~    next function      ~
 *      |                       |
 *      |-----------------------| <---- Ambient SP
 *      |       |               |
 *      ~       | Stack grows   ~
 *      |       | downward      |
 *              V
 *
 *
 *              FP / R11 frames
 *      |                       |
 *      |-----------------------|
 *      |       incoming        |
 *      |       arguments       |
 *      +=======================+ <---- Caller's SP
 *      |  Pre-spill registers  |
 *      |-----------------------| <---- Virtual '0'
 *      |Callee saved registers |
 *      |-----------------------|
 *      |        PSPSym         |   // Only for frames with EH, which means FP-based frames
 *      |-----------------------|
 *      ~ possible double align ~
 *      |-----------------------|
 *      |   security object     |
 *      |-----------------------|
 *      |     ParamTypeArg      |
 *      |-----------------------|
 *      |  possible GS cookie   |
 *      |-----------------------|
 *      |       Variables       |
 *      |-----------------------|
 *      |  possible GS cookie   |
 *      |-----------------------|
 *      |        Temps          |
 *      |-----------------------|
 *      |   Stub Argument Var   |
 *      |-----------------------|
 *      |Inlined PInvoke Frame V|
 *      |-----------------------|
 *      ~ possible double align ~
 *      |-----------------------|
 *      |       localloc        |
 *      |-----------------------|
 *      |   Arguments for the   |
 *      ~    next function      ~
 *      |                       |
 *      |-----------------------| <---- Ambient SP
 *      |       |               |
 *      ~       | Stack grows   ~
 *      |       | downward      |
 *              V
 *
 *
 *  The frame is laid out as follows for ARM64 (this is a general picture; details may differ for different conditions):
 *  NOTE: SP must be 16-byte aligned, so there may be alignment slots in the frame.
 *  We will often save and establish a frame pointer to create better ETW stack walks.
 *
 *              SP frames
 *      |                       |
 *      |-----------------------|
 *      |       incoming        |
 *      |       arguments       |
 *      +=======================+ <---- Caller's SP
 *      |         homed         | // this is only needed if reg argument need to be homed, e.g., for varargs
 *      |   register arguments  |
 *      |-----------------------| <---- Virtual '0'
 *      |Callee saved registers |
 *      |   except fp/lr        |
 *      |-----------------------|
 *      |   security object     |
 *      |-----------------------|
 *      |     ParamTypeArg      |
 *      |-----------------------|
 *      |  possible GS cookie   |
 *      |-----------------------|
 *      |       Variables       |
 *      |-----------------------|
 *      |  possible GS cookie   |
 *      |-----------------------|
 *      |        Temps          |
 *      |-----------------------|
 *      |   Stub Argument Var   |
 *      |-----------------------|
 *      |Inlined PInvoke Frame V|
 *      |-----------------------|
 *      |      Saved LR         |
 *      |-----------------------|
 *      |      Saved FP         | <---- Frame pointer
 *      |-----------------------|
 *      |  Stack arguments for  |
 *      |   the next function   |
 *      |-----------------------| <---- SP
 *      |       |               |
 *      ~       | Stack grows   ~
 *      |       | downward      |
 *              V
 *
 *
 *              FP (R29 / x29) frames
 *      |                       |
 *      |-----------------------|
 *      |       incoming        |
 *      |       arguments       |
 *      +=======================+ <---- Caller's SP
 *      |     optional homed    | // this is only needed if reg argument need to be homed, e.g., for varargs
 *      |   register arguments  |
 *      |-----------------------| <---- Virtual '0'
 *      |Callee saved registers |
 *      |   except fp/lr        |
 *      |-----------------------|
 *      |        PSPSym         | // Only for frames with EH, which requires FP-based frames
 *      |-----------------------|
 *      |   security object     |
 *      |-----------------------|
 *      |     ParamTypeArg      |
 *      |-----------------------|
 *      |  possible GS cookie   |
 *      |-----------------------|
 *      |       Variables       |
 *      |-----------------------|
 *      |  possible GS cookie   |
 *      |-----------------------|
 *      |        Temps          |
 *      |-----------------------|
 *      |   Stub Argument Var   |
 *      |-----------------------|
 *      |Inlined PInvoke Frame V|
 *      |-----------------------|
 *      |      Saved LR         |
 *      |-----------------------|
 *      |      Saved FP         | <---- Frame pointer
 *      |-----------------------|
 *      ~       localloc        ~
 *      |-----------------------|
 *      |  Stack arguments for  |
 *      |   the next function   |
 *      |-----------------------| <---- Ambient SP
 *      |       |               |
 *      ~       | Stack grows   ~
 *      |       | downward      |
 *              V
 *
 *
 *              FP (R29 / x29) frames where FP/LR are stored at the top of the frame (frames requiring GS that have localloc)
 *      |                       |
 *      |-----------------------|
 *      |       incoming        |
 *      |       arguments       |
 *      +=======================+ <---- Caller's SP
 *      |     optional homed    | // this is only needed if reg argument need to be homed, e.g., for varargs
 *      |   register arguments  |
 *      |-----------------------| <---- Virtual '0'
 *      |      Saved LR         |
 *      |-----------------------|
 *      |      Saved FP         | <---- Frame pointer
 *      |-----------------------|
 *      |Callee saved registers |
 *      |-----------------------|
 *      |        PSPSym         | // Only for frames with EH, which requires FP-based frames
 *      |-----------------------|
 *      |   security object     |
 *      |-----------------------|
 *      |     ParamTypeArg      |
 *      |-----------------------|
 *      |  possible GS cookie   |
 *      |-----------------------|
 *      |       Variables       |
 *      |-----------------------|
 *      |  possible GS cookie   |
 *      |-----------------------|
 *      |        Temps          |
 *      |-----------------------|
 *      |   Stub Argument Var   |
 *      |-----------------------|
 *      |Inlined PInvoke Frame V|
 *      |-----------------------|
 *      ~       localloc        ~
 *      |-----------------------|
 *      |  Stack arguments for  |
 *      |   the next function   |
 *      |-----------------------| <---- Ambient SP
 *      |       |               |
 *      ~       | Stack grows   ~
 *      |       | downward      |
 *              V
 *
 *
 *  Doing this all in one pass is 'hard'.  So instead we do it in 2 basic passes:
 *    1. Assign all the offsets relative to the Virtual '0'. Offsets above (the
 *      incoming arguments) are positive. Offsets below (everything else) are
 *      negative.  This pass also calcuates the total frame size (between Caller's
 *      SP/return address and the Ambient SP).
 *    2. Figure out where to place the frame pointer, and then adjust the offsets
 *      as needed for the final stack size and whether the offset is frame pointer
 *      relative or stack pointer relative.
 *
 */
// clang-format on

void Compiler::lvaAssignFrameOffsets(FrameLayoutState curState)
{
#ifdef TARGET_ARMARCH
    assert((curState == REGALLOC_FRAME_LAYOUT) || (curState == FINAL_FRAME_LAYOUT));
#else
    assert(curState == FINAL_FRAME_LAYOUT);
#endif
    assert(curState > lvaDoneFrameLayout);

    lvaDoneFrameLayout = curState;

#ifdef TARGET_ARMARCH
    JITDUMP("*************** In lvaAssignFrameOffsets (%s)\n",
            curState == FINAL_FRAME_LAYOUT ? "FINAL_FRAME_LAYOUT" : "REGALLOC_FRAME_LAYOUT");
#else
    JITDUMP("*************** In lvaAssignFrameOffsets (FINAL_FRAME_LAYOUT)\n");
#endif

#if FEATURE_FIXED_OUT_ARGS
    assert(lvaOutgoingArgSpaceVar != BAD_VAR_NUM);
#endif

#ifdef TARGET_ARMARCH
    // Param frame offsets are not affected by register allocation so
    // they can be assigned only once, during frame size estimation.
    if (curState == REGALLOC_FRAME_LAYOUT)
    {
        lvaAssignParamsVirtualFrameOffsets();
    }
#endif

    lvaAssignLocalsVirtualFrameOffsets();
    lvaAlignFrame();

#ifdef TARGET_ARMARCH
    // Frame size estimation does not need real offsets.
    if (curState == REGALLOC_FRAME_LAYOUT)
    {
        return;
    }
#endif

    lvaFixVirtualFrameOffsets();
    lvaAssignPromotedFieldsVirtualFrameOffsets();
}

// Now that everything has a virtual offset, determine the final value for
// the frame pointer (if needed) and then adjust all the offsets appropriately.
// This routine fixes virtual offset to be relative to frame pointer or SP
// based on whether lcl->lvFramePointerBased is true or false respectively.
void Compiler::lvaFixVirtualFrameOffsets()
{
#ifdef TARGET_AMD64
    if (lvaPSPSym != BAD_VAR_NUM)
    {
        // We need to fix the offset of the PSPSym so there is no padding between it and the outgoing
        // argument space. Without this code, lvaAlignFrame might have put the padding lower than the
        // PSPSym, which would be between the PSPSym and the outgoing argument space.
        LclVarDsc* lcl = lvaGetDesc(lvaPSPSym);

        assert(lcl->lvFramePointerBased && !lcl->lvMustInit);

        lcl->SetStackOffset(codeGen->genCallerSPtoInitialSPdelta() + codeGen->outgoingArgSpaceSize);

        // With OSR the new frame RBP points at the base of the new frame, but the virtual offsets
        // are from the base of the old frame. Adjust.
        if (opts.IsOSR())
        {
            lcl->SetStackOffset(lcl->GetStackOffset() - info.compPatchpointInfo->FpToSpDelta());
        }
    }
#endif

    // The delta to be added to virtual offset to adjust it relative to frame pointer or SP
    int delta = 0;

#ifdef TARGET_XARCH
    delta += REGSIZE_BYTES; // pushed return address for x86/x64

    JITDUMP("--- delta bump %d for RA\n", REGSIZE_BYTES);

    if (codeGen->doubleAlignOrFramePointerUsed())
    {
        JITDUMP("--- delta bump %d for FP\n", REGSIZE_BYTES);

        delta += REGSIZE_BYTES; // pushed EBP (frame pointer)
    }
#endif

    if (!codeGen->isFramePointerUsed())
    {
        // pushed registers, return address, and padding
        JITDUMP("--- delta bump %d for RSP frame\n", codeGen->genTotalFrameSize());

        delta += codeGen->genTotalFrameSize();
    }
#if defined(TARGET_ARM)
    else
    {
        // We set FP to be after LR, FP
        delta += 2 * REGSIZE_BYTES;
    }
#elif defined(TARGET_AMD64) || defined(TARGET_ARM64)
    else
    {
        // FP is used.
        JITDUMP("--- delta bump %d for RBP frame\n", codeGen->genTotalFrameSize() - codeGen->genSPtoFPdelta());

        delta += codeGen->genTotalFrameSize() - codeGen->genSPtoFPdelta();
    }
#endif // TARGET_AMD64

    // For OSR, update the delta to reflect the current policy that
    // RBP points at the base of the new frame, and RSP is relative to that RBP.
    if (opts.IsOSR())
    {
        JITDUMP("--- delta bump %d for OSR\n", info.compPatchpointInfo->FpToSpDelta());

        delta += info.compPatchpointInfo->FpToSpDelta();
    }

    JITDUMP("--- virtual stack offset to actual stack offset delta is %d\n", delta);

    for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum);

        // Can't be relative to EBP unless we have an EBP
        noway_assert(!lcl->lvFramePointerBased || codeGen->doubleAlignOrFramePointerUsed());

        if (lcl->IsDependentPromotedField(this))
        {
            continue; // Assigned later in lvaAssignPromotedFieldsVirtualFrameOffsets
        }

        if (!lcl->lvOnFrame && (!lcl->IsParam()
#ifndef TARGET_AMD64
                                || (lcl->IsRegParam()
#if defined(TARGET_ARM) && defined(PROFILING_SUPPORTED)
                                    && compIsProfilerHookNeeded() &&
                                    // We need assign stack offsets for prespilled arguments
                                    !lcl->IsPreSpilledRegParam(codeGen->preSpillParamRegs)
#endif
                                        )
#endif // !TARGET_AMD64
                                    ))
        {
            continue;
        }

#ifdef DEBUG
        if (lcl->GetStackOffset() == BAD_STK_OFFS)
        {
            assert(lcl->IsRegParam());

            // TODO-MIKE-Cleanup: Unused reg params not have a stack offset assigned but
            // the logic below doesn't filter out them properly and we end up computing
            // a bogus offset.
            continue;
        }
#endif

        JITDUMP("-- V%02u was %d, now %d\n", lclNum, lcl->GetStackOffset(), lcl->GetStackOffset() + delta);

        lcl->SetStackOffset(lcl->GetStackOffset() + delta);

#if defined(TARGET_X86) && DOUBLE_ALIGN
        if (codeGen->doDoubleAlign() && !codeGen->isFramePointerUsed() && lcl->lvFramePointerBased)
        {
            lcl->SetStackOffset(lcl->GetStackOffset() - delta);

            // We need to re-adjust the offsets of the parameters so they
            // are EBP relative rather than stack/frame pointer relative.

            lcl->SetStackOffset(lcl->GetStackOffset() + 2 * REGSIZE_BYTES); // return address and pushed EBP

            noway_assert(lcl->GetStackOffset() >= FIRST_ARG_STACK_OFFS);
        }
#endif

// TODO-MIKE-Review: See if this assert can be re-enabled for ARMARCH.
// The above filtering code is messed up and we try to fix the offset
// of locals that either haven't been assigned a stack offset (and do
// not need one) or have been assigned an offset during frame size
// estimation, which is no longer correct during final frame layout.

#ifndef TARGET_ARMARCH
        // For normal methods only frame pointer relative references can have negative offsets.
        assert(codeGen->isFramePointerUsed() || (lcl->GetStackOffset() >= 0));
#endif
    }

    assert(codeGen->regSet.tmpAllFree());

    for (TempDsc* temp = codeGen->regSet.tmpListBeg(); temp != nullptr; temp = codeGen->regSet.tmpListNxt(temp))
    {
        temp->tdAdjustTempOffs(delta);
    }

    codeGen->cachedGenericContextArgOffset += delta;

#if FEATURE_FIXED_OUT_ARGS
    if (lvaOutgoingArgSpaceVar != BAD_VAR_NUM)
    {
        LclVarDsc* lcl = lvaGetDesc(lvaOutgoingArgSpaceVar);

        lcl->SetStackOffset(0);
        lcl->lvFramePointerBased = false;
        lcl->lvMustInit          = false;
    }
#endif

#ifdef TARGET_ARM64
    // We normally add alignment below the locals between them and the outgoing
    // arg space area. When we store fp/lr at the bottom, however, this will be
    // below the alignment. So we should not apply the alignment adjustment to
    // them. On ARM64 it turns out we always store these at +0 and +8 of the FP,
    // so instead of dealing with skipping adjustment just for them we just set
    // them here always.
    assert(codeGen->isFramePointerUsed());

    if (lvaRetAddrVar != BAD_VAR_NUM)
    {
        lvaGetDesc(lvaRetAddrVar)->SetStackOffset(REGSIZE_BYTES);
    }
#endif
}

// Assign offsets to fields within a promoted struct (worker for lvaAssignFrameOffsets).
void Compiler::lvaAssignPromotedFieldsVirtualFrameOffsets()
{
    for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum);

        if (!lcl->IsPromotedField())
        {
            continue;
        }

        LclVarDsc* parentLcl               = lvaGetDesc(lcl->GetPromotedFieldParentLclNum());
        bool       hasDependentStackOffset = parentLcl->IsDependentPromoted();

        // On win-x64 all parameters are allocated on the caller's frame, including
        // parameters that are passed in registers. Since we have no control over
        // how the caller stores parameter values, we have to treat them as dependent
        // promoted for frame allocation purposes (we can't widen small int fields
        // to INT, we can't reorder fields etc.).
        // On all other targets, parameters passed in registers are allocated on
        // callee's frame and thus can be treated as independent locals.
        // There are special cases like ARM64 varargs and pre-spilled ARM params
        // but then promotion is disabled. These would probably require dependent
        // promotion too if enabled, or the pre-spilling code needs to be adjusted.

        hasDependentStackOffset |= parentLcl->IsParam()
#ifndef WINDOWS_AMD64_ABI
                                   && !parentLcl->IsRegParam()
#endif
            ;

#ifdef TARGET_ARM
        assert(!parentLcl->IsParam());
#endif
#ifdef TARGET_ARM64
        assert(!info.compIsVarArgs);
#endif

        if (hasDependentStackOffset)
        {
            lcl->SetStackOffset(parentLcl->GetStackOffset() + lcl->GetPromotedFieldOffset());
        }
    }
}

#ifdef TARGET_ARM64

void Compiler::lvaAssignParamsVirtualFrameOffsets()
{
    // We only need to assign offsets to reg parameters of varargs methods, which are
    // "pre-spilled" right below stack parameters so they all form a contiguous area.
    // Stack parameters have already been assigned offsets during the initial import.

    if (info.compIsVarArgs)
    {
        for (unsigned i = 0; i < info.compArgsCount; i++)
        {
            LclVarDsc* lcl = lvaGetDesc(i);

            if (lcl->IsRegParam() && (lcl->GetParamReg() != REG_ARG_RET_BUFF))
            {
                assert(genIsValidIntReg(lcl->GetParamReg()));

                lcl->SetStackOffset(((lcl->GetParamReg() - REG_R0) - MAX_REG_ARG) * REGSIZE_BYTES);
            }
        }
    }
}

#elif defined(TARGET_ARM)

void Compiler::lvaAssignParamsVirtualFrameOffsets()
{
    regMaskTP preSpillRegs = codeGen->GetPreSpillRegs();
    int       preSpillSize = codeGen->GetPreSpillSize();

    for (unsigned i = 0; i < info.compArgsCount; i++)
    {
        LclVarDsc* lcl = lvaGetDesc(i);

        if (lcl->IsPreSpilledRegParam(preSpillRegs))
        {
            regMaskTP regsBelow = genRegMask(lcl->GetParamReg()) - 1;
            unsigned  regOffset = genCountBits(preSpillRegs & regsBelow) * REGSIZE_BYTES;

            lcl->SetStackOffset(regOffset);
        }
        else if (!lcl->IsRegParam())
        {
            lcl->SetStackOffset(lcl->GetStackOffset() + preSpillSize);
        }
    }
}

#endif // TARGET_ARM

// Assign virtual stack offsets to locals, temps, and anything else.
// These will all be negative offsets (stack grows down) relative to the virtual '0' address.
void Compiler::lvaAssignLocalsVirtualFrameOffsets()
{
    codeGen->lclFrameSize = 0;

    int stkOffs              = 0;
    int originalFrameStkOffs = 0;
    int originalFrameSize    = 0;

#ifdef TARGET_XARCH
    // On x86/amd64, the return address has already been pushed by the call instruction in the caller.
    stkOffs -= REGSIZE_BYTES;

    if (lvaRetAddrVar != BAD_VAR_NUM)
    {
        lvaGetDesc(lvaRetAddrVar)->SetStackOffset(stkOffs);
    }

    // If we are an OSR method, we "inherit" the frame of the original method, and the stack
    // is already double aligned on entry (since the return address push and any special
    // alignment push happened "before").
    if (opts.IsOSR())
    {
        originalFrameSize    = info.compPatchpointInfo->FpToSpDelta();
        originalFrameStkOffs = stkOffs;
        stkOffs -= originalFrameSize;
    }

    // TODO-AMD64-CQ: for X64 eventually this should be pushed with all the other callee regs.
    // When you fix this, you'll also need to fix the assert at the bottom of this method.
    if (codeGen->doubleAlignOrFramePointerUsed())
    {
        stkOffs -= REGSIZE_BYTES;
    }

    stkOffs -= codeGen->calleeRegsPushed * REGSIZE_BYTES;

#ifdef WINDOWS_AMD64_ABI
    // In case of AMD64 compCalleeRegsPushed includes float regs (XMM6-XMM15) that
    // need to be pushed. But AMD64 doesn't support push/pop of XMM registers.
    // Instead we need to allocate space for them on the stack and save them in prolog.
    // Therefore, we consider XMM registers being saved while computing stack offsets
    // but space for XMM registers is considered part of compLclFrameSize.
    // Notes
    //  1) We need to save the entire 128-bits of XMM register to stack, since AMD64
    //     prolog unwind codes allow encoding of an instruction that stores the entire
    //     XMM reg at an offset relative to SP.
    //  2) We adjust frame size so that SP is aligned at 16-bytes after pushing integer registers.
    //     This means while saving the first XMM register to its allocated stack location we might
    //     have to skip 8-bytes. The reason for padding is to use efficient MOVAPS to save/restore
    //     XMM registers to/from stack to match JIT64 codegen. Without the aligning on 16-byte
    //     boundary we would have to use MOVUPS when offset turns out unaligned. MOVAPS is more
    //     performant than MOVUPS.
    const unsigned calleeFPRegsSavedSize =
        genCountBits(codeGen->calleeSavedModifiedRegs & RBM_ALLFLOAT) * XMM_REGSIZE_BYTES;

    // For OSR the alignment pad computation should not take the original frame into account.
    // Original frame size includes the pseudo-saved RA and so is always = 8 mod 16.
    const int offsetForAlign = -(stkOffs + originalFrameSize);

    if ((calleeFPRegsSavedSize > 0) && ((offsetForAlign % XMM_REGSIZE_BYTES) != 0))
    {
        int alignPad = static_cast<int>(AlignmentPad(static_cast<unsigned>(offsetForAlign), XMM_REGSIZE_BYTES));
        assert(alignPad != 0);

        lvaIncrementFrameSize(alignPad);
        stkOffs -= alignPad;
    }

    lvaIncrementFrameSize(calleeFPRegsSavedSize);
    stkOffs -= calleeFPRegsSavedSize;
#elif defined(TARGET_X86)
    const int preSpillSize    = 0;
    bool      mustDoubleAlign = false;

#if DOUBLE_ALIGN
    if (codeGen->doDoubleAlign())
    {
        mustDoubleAlign = true;

        if ((stkOffs + preSpillSize) % (2 * REGSIZE_BYTES) != 0)
        {
            lvaIncrementFrameSize(REGSIZE_BYTES);
            stkOffs -= REGSIZE_BYTES;

            assert((stkOffs + preSpillSize) % (2 * REGSIZE_BYTES) == 0);
        }
    }
#endif // DOUBLE_ALIGN
#endif // TARGET_X86

#elif defined(TARGET_ARMARCH)

#ifdef TARGET_ARM64
    // Decide where to save FP and LR registers. We store FP/LR registers at the bottom of the frame if there is
    // a frame pointer used (so we get positive offsets from the frame pointer to access locals), but not if we
    // need a GS cookie AND localloc is used, since we need the GS cookie to protect the saved return value,
    // and also the saved frame pointer. See CodeGen::PrologPushCalleeSavedRegisters() for more details about the
    // frame types. Since saving FP/LR at high addresses is a relatively rare case, force using it during stress.
    // (It should be legal to use these frame types for every frame).
    switch (opts.compJitSaveFpLrWithCalleeSavedRegisters)
    {
        default:
            // Default configuration
            codeGen->SetSaveFpLrWithAllCalleeSavedRegisters((getNeedsGSSecurityCookie() && compLocallocUsed) ||
                                                            compStressCompile(STRESS_GENERIC_VARN, 20));
            break;
        case 1:
            // Disable using new frames
            codeGen->SetSaveFpLrWithAllCalleeSavedRegisters(false);
            break;
        case 2:
            // Force using new frames
            codeGen->SetSaveFpLrWithAllCalleeSavedRegisters(true);
            break;
    }

    // For varargs we always save all of the integer register arguments
    // so that they are contiguous with the incoming stack arguments.
    if (info.compIsVarArgs)
    {
        stkOffs -= MAX_REG_ARG * REGSIZE_BYTES;
    }

    // If the frame pointer is used, then we'll save FP/LR at the bottom of the stack.
    // Otherwise, we won't store FP, and we'll store LR at the top, with the other
    // callee-save registers (if any).

    if (codeGen->IsSaveFpLrWithAllCalleeSavedRegisters() ||
        !codeGen->isFramePointerUsed()) // Note that currently we always have a frame pointer
    {
        stkOffs -= codeGen->calleeRegsPushed * REGSIZE_BYTES;
    }
    else
    {
        // Subtract off FP and LR.
        assert(codeGen->calleeRegsPushed >= 2);

        stkOffs -= (codeGen->calleeRegsPushed - 2) * REGSIZE_BYTES;
    }
#else  // TARGET_ARM
    // On ARM32 LR is part of the pushed registers and is always stored at the top.
    if (lvaRetAddrVar != BAD_VAR_NUM)
    {
        lvaGetDesc(lvaRetAddrVar)->SetStackOffset(stkOffs - REGSIZE_BYTES);
    }

    stkOffs -= codeGen->calleeRegsPushed * REGSIZE_BYTES;
#endif // TARGET_ARM

    if (lvaPSPSym != BAD_VAR_NUM)
    {
        // On ARM/ARM64, if we need a PSPSym, allocate it first, before anything else, including
        // padding (so we can avoid computing the same padding in the funclet frame). Note that
        // there is no special padding requirement for the PSPSym.

        noway_assert(codeGen->isFramePointerUsed());

        stkOffs = lvaAllocLocalAndSetVirtualOffset(lvaPSPSym, REGSIZE_BYTES, stkOffs);
    }

#ifdef TARGET_ARM
    const int  preSpillSize    = codeGen->GetPreSpillSize();
    const bool mustDoubleAlign = true;

    if (lvaDoneFrameLayout != FINAL_FRAME_LAYOUT)
    {
        lvaIncrementFrameSize(REGSIZE_BYTES);
        stkOffs -= REGSIZE_BYTES;

        // If we have any LONG, DOUBLE or double aligned structs then we need
        // to allocate a second pointer sized stack slot, since we may need to
        // double align that LclVar when we see it in the loop below. We will
        // just always do this so that the offsets that we calculate for the
        // stack frame will always be greater (or equal) to what they can be
        // in the final layout.

        lvaIncrementFrameSize(REGSIZE_BYTES);
        stkOffs -= REGSIZE_BYTES;
    }
    else
    {
        if ((stkOffs + preSpillSize) % (2 * REGSIZE_BYTES) != 0)
        {
            lvaIncrementFrameSize(REGSIZE_BYTES);
            stkOffs -= REGSIZE_BYTES;

            assert((stkOffs + preSpillSize) % (2 * REGSIZE_BYTES) == 0);
        }
    }
#endif // TARGET_ARM
#endif // TARGET_ARMARCH

    if (lvaMonAcquired != BAD_VAR_NUM)
    {
        // This var must go first, in what is called the 'frame header' for EnC so that it is
        // preserved when remapping occurs.  See vm\eetwain.cpp for detailed comment specifying frame
        // layout requirements for EnC to work.
        // TODO-MIKE-Review: lvaMonAcquired is INT so in theory it needs only 4 bytes, even on
        // 64 bit targets. But lvQuirkToLong used to (unnecessarily) affect this so we allocate
        // 8 bytes on 64 targets.
        stkOffs = lvaAllocLocalAndSetVirtualOffset(lvaMonAcquired, REGSIZE_BYTES, stkOffs);
    }

#ifdef JIT32_GCENCODER
    if (lvaLocAllocSPvar != BAD_VAR_NUM)
    {
        noway_assert(codeGen->isFramePointerUsed());

        stkOffs = lvaAllocLocalAndSetVirtualOffset(lvaLocAllocSPvar, REGSIZE_BYTES, stkOffs);
    }
#endif // JIT32_GCENCODER

    // For OSR methods, param type args are always reportable via the root method frame slot.
    // (see gcInfoBlockHdrSave) and so do not need a new slot on the frame.
    //
    // OSR methods may also be able to use the root frame kept alive this, if the root
    // method needed to report this.
    //
    // Inlining done under OSR may introduce new reporting, in which case the OSR frame
    // must allocate a slot.
    if (lvaReportParamTypeArg())
    {
#ifdef JIT32_GCENCODER
        noway_assert(codeGen->isFramePointerUsed());
#endif
        if (opts.IsOSR())
        {
            PatchpointInfo* ppInfo = info.compPatchpointInfo;
            assert(ppInfo->HasGenericContextArgOffset());
            codeGen->cachedGenericContextArgOffset = originalFrameStkOffs + ppInfo->GenericContextArgOffset();
        }
        else
        {
            // For CORINFO_CALLCONV_PARAMTYPE (if needed)
            lvaIncrementFrameSize(REGSIZE_BYTES);
            stkOffs -= REGSIZE_BYTES;

            codeGen->cachedGenericContextArgOffset = stkOffs;
        }
    }
#ifndef JIT32_GCENCODER
    else if (lvaKeepAliveAndReportThis())
    {
        bool canUseExistingSlot = false;

        if (opts.IsOSR())
        {
            PatchpointInfo* ppInfo = info.compPatchpointInfo;

            if (ppInfo->HasKeptAliveThis())
            {
                codeGen->cachedGenericContextArgOffset = originalFrameStkOffs + ppInfo->KeptAliveThisOffset();
                canUseExistingSlot                     = true;
            }
        }

        if (!canUseExistingSlot)
        {
            // When "this" is also used as generic context arg.
            lvaIncrementFrameSize(REGSIZE_BYTES);
            stkOffs -= REGSIZE_BYTES;

            codeGen->cachedGenericContextArgOffset = stkOffs;
        }
    }
#endif

#ifndef FEATURE_EH_FUNCLETS
    // If we need space for slots for shadow SP, reserve it now.
    if (ehNeedsShadowSPslots())
    {
        noway_assert(codeGen->isFramePointerUsed());

        if (!lvaReportParamTypeArg())
        {
#ifndef JIT32_GCENCODER
            if (!lvaKeepAliveAndReportThis())
#endif
            {
                // In order to keep the GC info encoding smaller, the VM assumes that all methods
                // with EH have also saved space for a ParamTypeArg, so we need to do that here.
                lvaIncrementFrameSize(REGSIZE_BYTES);
                stkOffs -= REGSIZE_BYTES;
            }
        }

        stkOffs = lvaAllocLocalAndSetVirtualOffset(lvaShadowSPslotsVar, lvaGetDesc(lvaShadowSPslotsVar)->GetBlockSize(),
                                                   stkOffs);
    }
#endif // !FEATURE_EH_FUNCLETS

    if (compGSReorderStackLayout)
    {
        assert(getNeedsGSSecurityCookie());

        if (!opts.IsOSR() || !info.compPatchpointInfo->HasSecurityCookie())
        {
            stkOffs = lvaAllocLocalAndSetVirtualOffset(lvaGSSecurityCookie, REGSIZE_BYTES, stkOffs);
        }
    }

    // If we're supposed to track lifetimes of pointer temps, we'll
    // assign frame offsets in the following order:
    //
    // non-pointer local variables (also untracked pointer variables)
    //     pointer local variables
    //     pointer temps
    // non-pointer temps

    enum
    {
        ALLOC_NON_PTRS                 = 0x1,
        ALLOC_PTRS                     = 0x2,
        ALLOC_UNSAFE_BUFFERS           = 0x4,
        ALLOC_UNSAFE_BUFFERS_WITH_PTRS = 0x8
    };

    unsigned allocOrder[4];
    unsigned allocCount = 0;

    if (opts.compDbgEnC)
    {
        assert(!compGSReorderStackLayout);

        // We will use just one pass, and assign offsets to all variables.
        allocOrder[allocCount++] = ALLOC_NON_PTRS | ALLOC_PTRS;
    }
    else if (!compGSReorderStackLayout)
    {
        allocOrder[allocCount++] = ALLOC_NON_PTRS;
        allocOrder[allocCount++] = ALLOC_PTRS;
    }
    else
    {
        noway_assert(getNeedsGSSecurityCookie());

        if (codeGen->isFramePointerUsed())
        {
            allocOrder[allocCount++] = ALLOC_UNSAFE_BUFFERS;
            allocOrder[allocCount++] = ALLOC_UNSAFE_BUFFERS_WITH_PTRS;
            allocOrder[allocCount++] = ALLOC_NON_PTRS;
            allocOrder[allocCount++] = ALLOC_PTRS;
        }
        else
        {
            allocOrder[allocCount++] = ALLOC_NON_PTRS;
            allocOrder[allocCount++] = ALLOC_PTRS;
            allocOrder[allocCount++] = ALLOC_UNSAFE_BUFFERS_WITH_PTRS;
            allocOrder[allocCount++] = ALLOC_UNSAFE_BUFFERS;
        }
    }

    bool tempsAllocated = false;

    if (lvaTempsHaveLargerOffsetThanVars() && !codeGen->isFramePointerUsed())
    {
        // Because we want the temps to have a larger offset than locals
        // and we're not using a frame pointer, we have to place the temps
        // above the vars.  Otherwise we place them after the vars (at the
        // bottom of the frame).

        stkOffs = lvaAllocateTemps(stkOffs
#ifndef TARGET_64BIT
                                   ,
                                   mustDoubleAlign
#endif
                                   );
        tempsAllocated = true;
    }

    // Force first pass to happen
    unsigned assignNext         = ALLOC_NON_PTRS | ALLOC_PTRS | ALLOC_UNSAFE_BUFFERS | ALLOC_UNSAFE_BUFFERS_WITH_PTRS;
    bool     haveLclDoubleAlign = false;

    for (unsigned allocStep = 0; allocStep < allocCount; allocStep++)
    {
        if ((assignNext & allocOrder[allocStep]) == 0)
        {
            continue;
        }

        assignNext = 0;

        for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
        {
            LclVarDsc* lcl = lvaGetDesc(lclNum);

            if (lcl->IsDependentPromotedField(this))
            {
                continue;
            }

#if FEATURE_FIXED_OUT_ARGS
            if (lclNum == lvaOutgoingArgSpaceVar)
            {
                continue;
            }
#endif

            // For OSR args and locals, we use the slots on the original frame.
            //
            // Note we must do this even for "non frame" locals, as we sometimes
            // will refer to their memory homes.
            if (lvaIsOSRLocal(lclNum))
            {
                // TODO-CQ: enable struct promotion for OSR locals; when that happens, figure out
                // how to properly refer to the original frame slots for the promoted fields.
                assert(!lcl->IsPromotedField());

                // Add frampointer-relative offset of this OSR live local in the original frame
                // to the offset of original frame in our new frame.
                int originalOffset = info.compPatchpointInfo->Offset(lclNum);
                int offset         = originalFrameStkOffs + originalOffset;

                JITDUMP("OSR: V%02u (on old frame) old rbp offset %d old frame offset %d new virt offset %d\n", lclNum,
                        originalOffset, originalFrameStkOffs, offset);

                lcl->SetStackOffset(offset);

                continue;
            }

            // Ignore variables that are not on the stack frame.

            if (!lcl->lvOnFrame)
            {
                // For EnC, all variables have to be allocated space on the
                // stack, even though they may actually be enregistered. This
                // way, the frame layout can be directly inferred from the
                // locals-sig.

                if (!opts.compDbgEnC)
                {
                    continue;
                }

                if (lclNum >= info.compLocalsCount)
                {
                    // ignore temps for EnC
                    continue;
                }
            }
            else if ((lvaGSSecurityCookie == lclNum) && getNeedsGSSecurityCookie())
            {
                // Special case for OSR. If the original method had a cookie,
                // we use its slot on the original frame.

                if (opts.IsOSR() && info.compPatchpointInfo->HasSecurityCookie())
                {
                    int originalOffset = info.compPatchpointInfo->SecurityCookieOffset();
                    int offset         = originalFrameStkOffs + originalOffset;

                    JITDUMP("OSR: V%02u (on old frame, security cookie) old rbp offset %d old frame offset %d new "
                            "virt offset %d\n",
                            lclNum, originalOffset, originalFrameStkOffs, offset);

                    lcl->SetStackOffset(offset);
                }

                continue;
            }

            // These need to be located as the very first variables (highest memory address)
            // and so they have already been assigned an offset

            if (lclNum == lvaMonAcquired ||
#ifdef FEATURE_EH_FUNCLETS
                lclNum == lvaPSPSym ||
#else
                lclNum == lvaShadowSPslotsVar ||
#endif
#ifdef JIT32_GCENCODER
                lclNum == lvaLocAllocSPvar ||
#endif
                lclNum == lvaRetAddrVar)
            {
                assert(lcl->GetStackOffset() != BAD_STK_OFFS);

                continue;
            }

            // This should be low on the stack. Hence, it will be assigned later.
            if (lclNum == lvaStubArgumentVar)
            {
#ifdef JIT32_GCENCODER
                noway_assert(codeGen->isFramePointerUsed());
#endif
                continue;
            }

            // This should be low on the stack. Hence, it will be assigned later.
            if (lclNum == lvaInlinedPInvokeFrameVar)
            {
                noway_assert(codeGen->isFramePointerUsed());

                continue;
            }

            // On win-x64 reg params have homes in the callers frame so they have
            // already been assigned an offset. On ARM targets, reg params may be
            // "pre-spilled" and then they also have offsets already assigned.
            if (lcl->IsParam()
#ifndef WINDOWS_AMD64_ABI
                && (!lcl->IsRegParam() ARM64_ONLY(|| (info.compIsVarArgs && (lcl->GetParamReg() != RET_BUFF_ARGNUM)))
                         ARM_ONLY(|| lcl->IsPreSpilledRegParam(codeGen->preSpillParamRegs)))
#endif
                    )
            {
                assert(lcl->GetStackOffset() != BAD_STK_OFFS);

                continue;
            }

            if (lcl->lvIsUnsafeBuffer && compGSReorderStackLayout)
            {
                if (lcl->lvIsPtr)
                {
                    if ((allocOrder[allocStep] & ALLOC_UNSAFE_BUFFERS_WITH_PTRS) == 0)
                    {
                        assignNext |= ALLOC_UNSAFE_BUFFERS_WITH_PTRS;

                        continue;
                    }
                }
                else
                {
                    if ((allocOrder[allocStep] & ALLOC_UNSAFE_BUFFERS) == 0)
                    {
                        assignNext |= ALLOC_UNSAFE_BUFFERS;

                        continue;
                    }
                }
            }
            else if (varTypeIsGC(lcl->GetType()) && lcl->HasLiveness())
            {
                if ((allocOrder[allocStep] & ALLOC_PTRS) == 0)
                {
                    assignNext |= ALLOC_PTRS;

                    continue;
                }
            }
            else
            {
                if ((allocOrder[allocStep] & ALLOC_NON_PTRS) == 0)
                {
                    assignNext |= ALLOC_NON_PTRS;

                    continue;
                }
            }

#ifndef TARGET_64BIT
            if (mustDoubleAlign &&
                (lcl->TypeIs(TYP_DOUBLE) ARM_ONLY(|| lcl->TypeIs(TYP_LONG)) || lcl->lvStructDoubleAlign))
            {
                noway_assert((codeGen->lclFrameSize % REGSIZE_BYTES) == 0);

#ifdef TARGET_ARM
                if ((lvaDoneFrameLayout != FINAL_FRAME_LAYOUT) && !haveLclDoubleAlign)
                {
                    // If this is the first LONG, DOUBLE or double aligned struct that
                    // we have seen in this loop then we allocate a pointer sized stack
                    // slot since we may need to double align this local for the final
                    // frame layout.

                    lvaIncrementFrameSize(REGSIZE_BYTES);
                    stkOffs -= REGSIZE_BYTES;
                }
                else
#endif
                {
                    if ((stkOffs + preSpillSize) % (2 * REGSIZE_BYTES) != 0)
                    {
                        lvaIncrementFrameSize(REGSIZE_BYTES);
                        stkOffs -= REGSIZE_BYTES;
                    }

                    assert((stkOffs + preSpillSize) % (2 * REGSIZE_BYTES) == 0);
                }

                haveLclDoubleAlign = true;
            }
#endif // !TARGET_64BIT

            stkOffs = lvaAllocLocalAndSetVirtualOffset(lclNum, lcl->GetFrameSize(), stkOffs);
        }
    }

    if (getNeedsGSSecurityCookie() && !compGSReorderStackLayout)
    {
        if (!opts.IsOSR() || !info.compPatchpointInfo->HasSecurityCookie())
        {
            // LOCALLOC used, but we have no unsafe buffer. Allocated cookie last, close to localloc buffer.
            stkOffs = lvaAllocLocalAndSetVirtualOffset(lvaGSSecurityCookie, REGSIZE_BYTES, stkOffs);
        }
    }

    if (!tempsAllocated)
    {
        stkOffs = lvaAllocateTemps(stkOffs
#ifndef TARGET_64BIT
                                   ,
                                   mustDoubleAlign
#endif
                                   );
    }

    // lvaInlinedPInvokeFrameVar and lvaStubArgumentVar need to be assigned last
    // Important: The stack walker depends on lvaStubArgumentVar immediately
    // following lvaInlinedPInvokeFrameVar in the frame.

    if (lvaStubArgumentVar != BAD_VAR_NUM)
    {
#ifdef JIT32_GCENCODER
        noway_assert(codeGen->isFramePointerUsed());
#endif

        stkOffs = lvaAllocLocalAndSetVirtualOffset(lvaStubArgumentVar, REGSIZE_BYTES, stkOffs);
    }

    if (lvaInlinedPInvokeFrameVar != BAD_VAR_NUM)
    {
        noway_assert(codeGen->isFramePointerUsed());

        stkOffs = lvaAllocLocalAndSetVirtualOffset(lvaInlinedPInvokeFrameVar,
                                                   lvaGetDesc(lvaInlinedPInvokeFrameVar)->GetBlockSize(), stkOffs);
    }

#ifndef TARGET_64BIT
    if (mustDoubleAlign)
    {
#ifdef TARGET_ARM
        if (lvaDoneFrameLayout != FINAL_FRAME_LAYOUT)
        {
            // Allocate a pointer sized stack slot, since we may need to double align here.

            lvaIncrementFrameSize(REGSIZE_BYTES);
            stkOffs -= REGSIZE_BYTES;

            if (haveLclDoubleAlign)
            {
                // If we have any LONG, DOUBLE or double aligned structs then we need to
                // allocate a second pointer sized stack slot, since we may need to double
                // align the last LclVar that we saw in the loop above. We do this so that
                // the offsets that we calculate for the stack frame are always greater
                // than they will be in the final layout.

                lvaIncrementFrameSize(REGSIZE_BYTES);
                stkOffs -= REGSIZE_BYTES;
            }
        }
        else
#endif
        {
            if ((stkOffs + preSpillSize) % (2 * REGSIZE_BYTES) != 0)
            {
                lvaIncrementFrameSize(REGSIZE_BYTES);
                stkOffs -= REGSIZE_BYTES;
            }

            assert((stkOffs + preSpillSize) % (2 * REGSIZE_BYTES) == 0);
        }
    }
#endif // !TARGET_64BIT

#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_AMD64)
    if (lvaPSPSym != BAD_VAR_NUM)
    {
        // On AMD64, if we need a PSPSym, allocate it last, immediately above the outgoing argument
        // space. Any padding will be higher on the stack than this (including the padding added by
        // lvaAlignFrame).
        noway_assert(codeGen->isFramePointerUsed()); // We need an explicit frame pointer

        stkOffs = lvaAllocLocalAndSetVirtualOffset(lvaPSPSym, REGSIZE_BYTES, stkOffs);
    }
#endif // FEATURE_EH_FUNCLETS && defined(TARGET_AMD64)

#ifdef TARGET_ARM64
    // Note that currently we always have a frame pointer
    if (!codeGen->IsSaveFpLrWithAllCalleeSavedRegisters() && codeGen->isFramePointerUsed())
    {
        // Create space for saving FP and LR.
        stkOffs -= 2 * REGSIZE_BYTES;
    }
#endif // TARGET_ARM64

#if FEATURE_FIXED_OUT_ARGS
    if (codeGen->outgoingArgSpaceSize > 0)
    {
        noway_assert(codeGen->outgoingArgSpaceSize % REGSIZE_BYTES == 0);
#ifdef WINDOWS_AMD64_ABI
        noway_assert(codeGen->outgoingArgSpaceSize >= 4 * REGSIZE_BYTES);
#endif

        // Give it a value so we can avoid asserts in CHK builds.
        // Since this will always use an SP relative offset of zero
        // at the end of lvaFixVirtualFrameOffsets, it will be set to absolute '0'

        stkOffs = lvaAllocLocalAndSetVirtualOffset(lvaOutgoingArgSpaceVar, codeGen->outgoingArgSpaceSize, stkOffs);
    }
#endif // FEATURE_FIXED_OUT_ARGS

    // Now -stkOffs should be equal to the frame size - the space allocated for locals (lclFrameSize),
    // the original frame size for OSR compilation and the space allocated for call preserved registers.
    // Note that this does not include padding required to maintain frame alignment, that will be added
    // later to the space allocated for locals.
    int pushedCount = codeGen->calleeRegsPushed;

#ifdef TARGET_ARM64
    if (info.compIsVarArgs)
    {
        pushedCount += MAX_REG_ARG;
    }
#endif

#ifdef TARGET_XARCH
    if (codeGen->doubleAlignOrFramePointerUsed())
    {
        pushedCount++; // pushed EBP (frame pointer)
    }

    pushedCount++; // pushed PC (return address)
#endif

    noway_assert(codeGen->lclFrameSize + originalFrameSize + pushedCount * REGSIZE_BYTES ==
                 static_cast<unsigned>(-stkOffs));
}

int Compiler::lvaAllocLocalAndSetVirtualOffset(unsigned lclNum, unsigned size, int stkOffs)
{
    noway_assert(lclNum != BAD_VAR_NUM);
    assert((size != 0) && (size % 4 == 0));
    assert(stkOffs <= 0);

    LclVarDsc* lcl = lvaGetDesc(lclNum);

    assert(size >= lcl->GetTypeSize());

#ifdef TARGET_64BIT
    if (size >= 8)
    {
        // Before final frame layout, assume the worst case, that every >=8 byte local will need
        // maximum padding to be aligned. This is because we generate code based on the stack offset
        // computed during tentative frame layout. These offsets cannot get bigger during final
        // frame layout, as that would possibly require different code generation (for example,
        // using a 4-byte offset instead of a 1-byte offset in an instruction). The offsets can get
        // smaller. It is possible there is different alignment at the point locals are allocated
        // between tentative and final frame layout which would introduce padding between locals
        // and thus increase the offset (from the stack pointer) of one of the locals. Hence the
        // need to assume the worst alignment before final frame layout.
        // We could probably improve this by sorting all the objects by alignment, such that all
        // 8 byte objects are together, 4 byte objects are together, etc., which would require at
        // most one alignment padding per group.

        if ((stkOffs % 8 != 0)
#ifdef TARGET_ARMARCH
            || (lvaDoneFrameLayout != FINAL_FRAME_LAYOUT)
#endif
#ifdef FEATURE_SIMD
            || varTypeIsSIMD(lcl->GetType())
#endif
                )
        {
            unsigned pad = 0;

#ifdef FEATURE_SIMD
            if (varTypeIsSIMD(lcl->GetType()))
            {
                int alignment = lvaGetSimdTypedLocalPreferredAlignment(lcl);

                if (stkOffs % alignment != 0)
                {
#ifdef TARGET_ARMARCH
                    if (lvaDoneFrameLayout != FINAL_FRAME_LAYOUT)
                    {
                        // Note that all the objects will probably be misaligned, but we'll fix that in final layout.
                        pad = alignment - 1;
                    }
                    else
#endif
                    {
                        pad = alignment + (stkOffs % alignment); // +1 to +(alignment-1) bytes
                    }
                }
            }
            else
#endif // FEATURE_SIMD
#ifdef TARGET_ARMARCH
                if (lvaDoneFrameLayout != FINAL_FRAME_LAYOUT)
            {
                // Note that all the objects will probably be misaligned, but we'll fix that in final layout.
                pad = 7;
            }
            else
#endif
            {
                pad = 8 + stkOffs % 8; // +1 to +7 bytes
            }

            lvaIncrementFrameSize(pad);
            stkOffs -= pad;

            JITDUMP("Pad %u V%02u stack offset %c0x%x (size %u)", pad, lclNum, stkOffs < 0 ? '-' : '+',
                    stkOffs < 0 ? -stkOffs : stkOffs, size);
        }
    }
#endif // TARGET_64BIT

    lvaIncrementFrameSize(size);
    stkOffs -= size;
    lcl->SetStackOffset(stkOffs);

    JITDUMP("Assign V%02u stack offset %c0x%x (size %u)\n", lclNum, stkOffs < 0 ? '-' : '+',
            stkOffs < 0 ? -stkOffs : stkOffs, size);

    return stkOffs;
}

#ifdef TARGET_AMD64
// Returns true if the number of integer registers pushed onto stack
// is even including RBP if used as frame pointer
//
// Note that this excludes return address (PC) pushed by caller. To know whether
// the SP offset after pushing integer registers is aligned, we need to take
// negation of this routine.
bool Compiler::lvaIsCalleeSavedIntRegCountEven()
{
    unsigned regsPushed = codeGen->calleeRegsPushed + (codeGen->isFramePointerUsed() ? 1 : 0);
    return (regsPushed % (16 / REGSIZE_BYTES)) == 0;
}
#endif // TARGET_AMD64

// After allocating everything on the frame, reserve any
// extra space needed to keep the frame aligned.
void Compiler::lvaAlignFrame()
{
#if defined(TARGET_AMD64)
    // On x64 the stack must be 16 byte aligned.

    // Leaf frames do not need full alignment, but the unwind info is smaller if we
    // are at least 8 byte aligned (and we assert as much)
    if (codeGen->lclFrameSize % 8 != 0)
    {
        lvaIncrementFrameSize(8 - codeGen->lclFrameSize % 8);
        assert(codeGen->lclFrameSize % 8 == 0);
    }

    bool stackNeedsAlignment = codeGen->lclFrameSize != 0;
#ifdef UNIX_AMD64_ABI
    // The needToAlignFrame flag is indicating if there is a need to align the frame.
    // On AMD64-Windows, if there are calls, 4 slots for the outgoing ars are allocated,
    // except for FastTailCall. This slots makes the frame size non-zero, so alignment logic
    // will be called. On AMD64-Unix, there are no such slots. There is a possibility to have
    // calls in the method with frame size of 0. The frame alignment logic won't kick in.
    // This flags takes care of the AMD64-Unix case by remembering that there are calls and
    // making sure the frame alignment logic is executed.
    stackNeedsAlignment |= codeGen->needToAlignFrame;
#endif

    if (stackNeedsAlignment)
    {
        unsigned pushCount = 1 + (codeGen->isFramePointerUsed() ? 1 : 0) + codeGen->calleeRegsPushed;
        unsigned frameSize = codeGen->lclFrameSize + pushCount * REGSIZE_BYTES;

        if (frameSize % 16 != 0)
        {
            lvaIncrementFrameSize(8);
            assert((codeGen->lclFrameSize + pushCount * REGSIZE_BYTES) % 16 == 0);
        }
    }
#elif defined(TARGET_ARM64)
    // On arm64 the stack must be 16 byte aligned.

    if (lvaDoneFrameLayout == REGALLOC_FRAME_LAYOUT)
    {
        // When we estimate the frame size we don't know the exact value of lclFrameSize
        // and thus do not know how much we will need to add in order to be aligned. Just
        // assume the worst and add 16.
        lvaIncrementFrameSize(16);

        return;
    }

    // First, align up to 8 (lclFrameSize may be only 4 byte aligned and we need 8 for
    // callee pushed regs).
    if (codeGen->lclFrameSize % 8 != 0)
    {
        lvaIncrementFrameSize(8 - codeGen->lclFrameSize % 8);
        assert(codeGen->lclFrameSize % 8 == 0);
    }

    unsigned pushCount = codeGen->calleeRegsPushed;
    unsigned frameSize = codeGen->lclFrameSize + pushCount * REGSIZE_BYTES;

    if (frameSize % 16 != 0)
    {
        lvaIncrementFrameSize(8);
        assert((codeGen->lclFrameSize + pushCount * REGSIZE_BYTES) % 16 == 0);
    }

#elif defined(TARGET_ARM)
    // On arm the stack must be 8 byte aligned.

    // TODO-MIKE-Review: What about REGALLOC_FRAME_LAYOUT?

    unsigned pushCount = codeGen->calleeRegsPushed + codeGen->GetPreSpillRegCount();
    unsigned frameSize = codeGen->lclFrameSize + pushCount * REGSIZE_BYTES;

    if (frameSize % 8 != 0)
    {
        lvaIncrementFrameSize(4);
        assert((codeGen->lclFrameSize + pushCount * REGSIZE_BYTES) % 8 == 0);
    }

#elif defined(TARGET_X86)
    // On win-x86 the stack must be 4 byte aligned and it should already be,
    // we allocate in 4 byte slots.

    assert(codeGen->lclFrameSize % 4 == 0);

#if DOUBLE_ALIGN
    if (codeGen->doDoubleAlign())
    {
        // Double Frame Alignment for x86 is handled in Compiler::lvaAssignLocalsVirtualFrameOffsets()

        if (codeGen->lclFrameSize == 0)
        {
            // This can only happen with JitStress=1 or JitDoubleAlign=2
            lvaIncrementFrameSize(4);
        }
    }
#endif

#ifdef UNIX_X86_ABI
    // On unix-x86 the stack must be 16 byte alignment.

    int pushCount = 1 + (codeGen->doubleAlignOrFramePointerUsed() ? 1 : 0) + codeGen->calleeRegsPushed;
    int frameSize = codeGen->lclFrameSize + (pushCount * REGSIZE_BYTES) % 16;

    if (frameSize % 16 != 0)
    {
        lvaIncrementFrameSize(16 - frameSize % 16);
        assert((codeGen->lclFrameSize + pushCount * REGSIZE_BYTES) % 16 == 0);
    }
#endif

#else
    NYI("TARGET specific lvaAlignFrame");
#endif // !TARGET_AMD64
}

// Assign virtual offsets to temps (always negative).
int Compiler::lvaAllocateTemps(int stkOffs
#ifndef TARGET_64BIT
                               ,
                               bool mustDoubleAlign
#endif
                               )
{
    assert(stkOffs <= 0);

#ifdef TARGET_ARMARCH
    if (lvaDoneFrameLayout == REGALLOC_FRAME_LAYOUT)
    {
        // There are no temps when estimating frame size.
        return stkOffs;
    }
#endif

    assert(lvaDoneFrameLayout == FINAL_FRAME_LAYOUT);

#ifdef TARGET_ARM
    unsigned spillTempSize = 0;
    int      preSpillSize  = codeGen->GetPreSpillSize();
#else
    int preSpillSize = 0;
#endif

    assert(codeGen->regSet.tmpAllFree());

    for (TempDsc* temp = codeGen->regSet.tmpListBeg(); temp != nullptr; temp = codeGen->regSet.tmpListNxt(temp))
    {
        var_types type = temp->tdTempType();
        unsigned  size = temp->tdTempSize();

#ifdef TARGET_64BIT
        if (varTypeIsGC(type) && ((stkOffs % REGSIZE_BYTES) != 0))
        {
            unsigned alignPad = static_cast<unsigned>(-stkOffs);
            alignPad          = roundUp(alignPad, REGSIZE_BYTES) - alignPad;

            lvaIncrementFrameSize(alignPad);
            stkOffs -= alignPad;

            noway_assert((stkOffs % REGSIZE_BYTES) == 0);
        }
#else
        if (mustDoubleAlign && (type == TYP_DOUBLE)) // Align doubles for x86 and ARM
        {
            noway_assert(codeGen->lclFrameSize % REGSIZE_BYTES == 0);

            if ((stkOffs + preSpillSize) % (2 * REGSIZE_BYTES) != 0)
            {
                lvaIncrementFrameSize(REGSIZE_BYTES);
                stkOffs -= REGSIZE_BYTES;
#ifdef TARGET_ARM
                spillTempSize += REGSIZE_BYTES;
#endif
            }

            noway_assert((stkOffs + preSpillSize) % (2 * REGSIZE_BYTES) == 0);
        }
#endif

        lvaIncrementFrameSize(size);
        stkOffs -= size;
#ifdef TARGET_ARM
        spillTempSize += size;
#endif

        temp->tdSetTempOffs(stkOffs);
    }

    return stkOffs;
}

// Determine the stack frame offset of the given local,
// and how to generate an address to that local's stack location.
int Compiler::lvaFrameAddress(int varNum, bool* pFPbased)
{
    assert(lvaDoneFrameLayout == FINAL_FRAME_LAYOUT);

    bool fpBased;
    int  varOffset;

    if (varNum >= 0)
    {
        LclVarDsc* lcl = lvaGetDesc(static_cast<unsigned>(varNum));

        fpBased   = lcl->lvFramePointerBased;
        varOffset = lcl->GetStackOffset();

#ifdef DEBUG
        bool isPrespilledArg = false;
#if defined(TARGET_ARM) && defined(PROFILING_SUPPORTED)
        isPrespilledArg =
            lcl->IsParam() && compIsProfilerHookNeeded() && lcl->IsPreSpilledRegParam(codeGen->preSpillParamRegs);
#endif

        if (!lcl->lvOnFrame)
        {
#ifdef WINDOWS_AMD64_ABI
            assert(lcl->IsParam());
#endif
#ifndef TARGET_AMD64
            assert((lcl->IsParam() && !lcl->IsRegParam()) || isPrespilledArg);
#endif
        }

#if FEATURE_FIXED_OUT_ARGS
        if (static_cast<unsigned>(varNum) == lvaOutgoingArgSpaceVar)
        {
            assert(!fpBased);
        }
        else
#endif
        {
#ifdef TARGET_X86
#if DOUBLE_ALIGN
            assert(fpBased == (codeGen->isFramePointerUsed() ||
                               (codeGen->doDoubleAlign() && lcl->IsParam() && !lcl->IsRegParam())));
#else
            assert(fpBased == codeGen->isFramePointerUsed());
#endif
#endif
        }
#endif // DEBUG
    }
    else
    {
        TempDsc* tmpDsc = codeGen->regSet.tmpFindNum(varNum);

        // The temp might be in use, since this might be during code generation.
        if (tmpDsc == nullptr)
        {
            tmpDsc = codeGen->regSet.tmpFindNum(varNum, RegSet::TEMP_USAGE_USED);
            assert(tmpDsc != nullptr);
        }

        fpBased   = codeGen->isFramePointerUsed();
        varOffset = tmpDsc->tdTempOffs();
    }

    *pFPbased = fpBased;
    return varOffset;
}

#ifdef TARGET_ARMARCH

// Returns true if the REG_OPT_RSVD should be reserved so that it
// can be used to form large offsets when accessing stack based
// LclVar including both incoming and out going argument areas.
// The method advances the frame layout state to curState by calling
// lvaEstimateFrameSize().
bool Compiler::lvaHasLargeFrameOffset()
{
    // Always do the layout even if returning early. Callers might
    // depend on us to do the layout.
    unsigned frameSize = lvaEstimateFrameSize();

#ifdef TARGET_ARM64
    // TODO-ARM64-CQ: update this!
    JITDUMP("Returning true (ARM64)\n\n");
    return true; // just always assume we'll need it, for now
#else
    // TODO-MIKE-Fix: paramsStackSize does not include prespilled params.
    // Also note that the old paramsSize was bogus anyway, as it included
    // alignment registers and float param registers, which are never pre
    // spilled.
    unsigned paramsSize = codeGen->paramsStackSize;
    JITDUMP("\ncompRsvdRegCheck - frame size = %u, compArgSize = %u\n", frameSize, paramsSize);

    if (opts.MinOpts())
    {
        // Have a recovery path in case we fail to reserve REG_OPT_RSVD and go
        // over the limit of SP and FP offset ranges due to large
        // temps.
        JITDUMP("Returning true (MinOpts)\n\n");
        return true;
    }

    // frame layout:
    //
    //         ... high addresses ...
    //                         frame contents       size
    //                         -------------------  ------------------------
    //                         inArgs               compArgSize (includes prespill)
    //  caller SP --->
    //                         prespill
    //                         LR                   REGSIZE_BYTES
    //  R11    --->            R11                  REGSIZE_BYTES
    //                         callee saved regs    CALLEE_SAVED_REG_MAXSZ   (32 bytes)
    //                     optional saved fp regs   CALLEE_SAVED_FLOAT_MAXSZ (64 bytes)
    //                         lclSize
    //                             incl. TEMPS      MAX_SPILL_TEMP_SIZE
    //                             incl. outArgs
    //  SP     --->
    //          ... low addresses ...
    //
    // When codeGen->isFramePointerRequired is true, R11 will be established as a frame pointer.
    // We can then use R11 to access incoming args with positive offsets, and LclVars with
    // negative offsets.
    //
    // In functions with EH, in the non-funclet (or main) region, even though we will have a
    // frame pointer, we can use SP with positive offsets to access any or all locals or arguments
    // that we can reach with SP-relative encodings. The funclet region might require the reserved
    // register, since it must use offsets from R11 to access the parent frame.

    unsigned maxR11PositiveEncodingOffset = compFloatingPointUsed ? 0x03FC : 0x0FFF;
    JITDUMP("  maxR11PositiveEncodingOffset     = %6d\n", maxR11PositiveEncodingOffset);

    // Floating point load/store instructions (VLDR/VSTR) can address up to -0x3FC from R11, but we
    // don't know if there are either no integer locals, or if we don't need large negative offsets
    // for the integer locals, so we must use the integer max negative offset, which is a
    // smaller (absolute value) number.
    unsigned maxR11NegativeEncodingOffset = 0x00FF; // This is a negative offset from R11.
    JITDUMP("  maxR11NegativeEncodingOffset     = %6d\n", maxR11NegativeEncodingOffset);

    // -1 because otherwise we are computing the address just beyond the last argument, which we don't need to do.
    unsigned maxR11PositiveOffset = paramsSize + (2 * REGSIZE_BYTES) - 1;
    JITDUMP("  maxR11PositiveOffset             = %6d\n", maxR11PositiveOffset);

    // The value is positive, but represents a negative offset from R11.
    // frameSize includes callee-saved space for R11 and LR, which are at non-negative offsets from R11
    // (+0 and +4, respectively), so don't include those in the max possible negative offset.
    assert(frameSize >= (2 * REGSIZE_BYTES));
    unsigned maxR11NegativeOffset = frameSize - (2 * REGSIZE_BYTES);
    JITDUMP("  maxR11NegativeOffset             = %6d\n", maxR11NegativeOffset);

    if (codeGen->isFramePointerRequired())
    {
        if (maxR11NegativeOffset > maxR11NegativeEncodingOffset)
        {
            JITDUMP(" Returning true (frame required and maxR11NegativeOffset)\n\n");
            return true;
        }
        if (maxR11PositiveOffset > maxR11PositiveEncodingOffset)
        {
            JITDUMP(" Returning true (frame required and maxR11PositiveOffset)\n\n");
            return true;
        }
    }

    // Now consider the SP based frame case. Note that we will use SP based offsets to access the stack in R11 based
    // frames in the non-funclet main code area.

    unsigned maxSPPositiveEncodingOffset = compFloatingPointUsed ? 0x03FC : 0x0FFF;
    JITDUMP("  maxSPPositiveEncodingOffset      = %6d\n", maxSPPositiveEncodingOffset);

    // -1 because otherwise we are computing the address just beyond the last argument, which we don't need to do.
    assert(paramsSize + frameSize > 0);
    unsigned maxSPPositiveOffset = paramsSize + frameSize - 1;

    if (codeGen->isFramePointerUsed())
    {
        // We have a frame pointer, so we can use it to access part of the stack, even if SP can't reach those parts.
        // We will still generate SP-relative offsets if SP can reach.

        // First, check that the stack between R11 and SP can be fully reached, either via negative offset from FP
        // or positive offset from SP. Don't count stored R11 or LR, which are reached from positive offsets from FP.

        unsigned maxSPLocalsCombinedOffset = frameSize - (2 * REGSIZE_BYTES) - 1;
        JITDUMP("  maxSPLocalsCombinedOffset        = %6d\n", maxSPLocalsCombinedOffset);

        if (maxSPLocalsCombinedOffset > maxSPPositiveEncodingOffset)
        {
            // Can R11 help?
            unsigned maxRemainingLocalsCombinedOffset = maxSPLocalsCombinedOffset - maxSPPositiveEncodingOffset;
            JITDUMP("  maxRemainingLocalsCombinedOffset = %6d\n", maxRemainingLocalsCombinedOffset);

            if (maxRemainingLocalsCombinedOffset > maxR11NegativeEncodingOffset)
            {
                JITDUMP(" Returning true (frame pointer exists; R11 and SP can't reach entire stack between them)\n\n");
                return true;
            }

            // Otherwise, yes, we can address the remaining parts of the locals frame with negative offsets from R11.
        }

        // Check whether either R11 or SP can access the arguments.
        if ((maxR11PositiveOffset > maxR11PositiveEncodingOffset) &&
            (maxSPPositiveOffset > maxSPPositiveEncodingOffset))
        {
            JITDUMP(" Returning true (frame pointer exists; R11 and SP can't reach all arguments)\n\n");
            return true;
        }
    }
    else
    {
        if (maxSPPositiveOffset > maxSPPositiveEncodingOffset)
        {
            JITDUMP(" Returning true (no frame pointer exists; SP can't reach all of frame)\n\n");
            return true;
        }
    }

    // We won't need to reserve REG_OPT_RSVD.
    //
    JITDUMP(" Returning false\n\n");
    return false;
#endif // TARGET_ARM
}

// Returns the (conservative, that is, overly large) estimated size of the frame,
// including the callee-saved registers. This is only used by LSRA to check if a
// temporary register needs to be reserved for frame offsets that are to large to
// be encoded in a load/store instruction.
unsigned Compiler::lvaEstimateFrameSize()
{
    assert(lvaDoneFrameLayout == NO_FRAME_LAYOUT);

    // Layout the stack frame conservatively.
    // Assume all callee-saved registers are spilled to stack.

    codeGen->calleeRegsPushed = CNT_CALLEE_SAVED;

    if (compFloatingPointUsed)
    {
        codeGen->calleeRegsPushed += CNT_CALLEE_SAVED_FLOAT;
    }

    codeGen->calleeRegsPushed++; // We always push LR, see PrologPushCalleeSavedRegisters.

    lvaAssignFrameOffsets(REGALLOC_FRAME_LAYOUT);
    DBEXEC(verbose, lvaTableDump());

    unsigned frameSize = codeGen->lclFrameSize;

    frameSize += CALLEE_SAVED_REG_MAXSZ;

    if (compFloatingPointUsed)
    {
        frameSize += CALLEE_SAVED_FLOAT_MAXSZ;
    }

    frameSize += REGSIZE_BYTES; // We always push LR, see PrologPushCalleeSavedRegisters.

    return frameSize;
}

#endif // TARGET_ARMARCH

// Return the caller-SP-relative stack offset of a local/parameter.
int Compiler::lvaGetCallerSPRelativeOffset(unsigned lclNum)
{
    assert(lvaDoneFrameLayout == FINAL_FRAME_LAYOUT);

    LclVarDsc* lcl = lvaGetDesc(lclNum);
    assert(lcl->lvOnFrame);
    return lvaToCallerSPRelativeOffset(lcl->GetStackOffset(), lcl->lvFramePointerBased);
}

// Translate a frame offset into an offset from the caller's stack pointer.
int Compiler::lvaToCallerSPRelativeOffset(int offset, bool isFpBased, bool forRootFrame) const
{
    assert(lvaDoneFrameLayout == FINAL_FRAME_LAYOUT);

    if (isFpBased)
    {
        offset += codeGen->genCallerSPtoFPdelta();
    }
    else
    {
        offset += codeGen->genCallerSPtoInitialSPdelta();
    }

#ifndef TARGET_AMD64
    // OSR NYI for other targets.
    assert(!opts.IsOSR());
#else
    if (forRootFrame && opts.IsOSR())
    {
        // The offset computed above already includes the OSR frame adjustment, plus the
        // pop of the "pseudo return address" from the OSR frame.
        //
        // To get to root method caller-SP, we need to subtract off the original frame
        // size and the pushed return address and RBP for that frame (which we know is an
        // RPB frame).
        //
        // ppInfo's FpToSpDelta also accounts for the popped pseudo return address
        // between the original method frame and the OSR frame. So the net adjustment
        // is simply FpToSpDelta plus one register.

        offset -= info.compPatchpointInfo->FpToSpDelta() + REGSIZE_BYTES;
    }
#endif

    return offset;
}

#ifdef TARGET_AMD64
int Compiler::lvaGetPSPSymInitialSPRelativeOffset()
{
    assert(lvaDoneFrameLayout == FINAL_FRAME_LAYOUT);

    LclVarDsc* lcl = lvaGetDesc(lvaPSPSym);
    assert(lcl->lvOnFrame);

    if (lcl->lvFramePointerBased)
    {
        // Currently, the frame starts by pushing ebp, ebp points to the saved ebp
        // (so we have ebp pointer chaining). Add the fixed-size frame size plus the
        // size of the callee-saved regs (not including ebp itself) to find Initial-SP.

        assert(codeGen->isFramePointerUsed());

        return lcl->GetStackOffset() + codeGen->genSPtoFPdelta();
    }
    else
    {
        return lcl->GetStackOffset();
    }
}
#endif // TARGET_AMD64

#ifdef FEATURE_SIMD
// Get the preferred alignment of SIMD typed local for better performance.
int Compiler::lvaGetSimdTypedLocalPreferredAlignment(LclVarDsc* lcl)
{
    assert(varTypeIsSIMD(lcl->GetType()));

    if (lcl->GetType() == TYP_SIMD12)
    {
        return 16;
    }

    return varTypeSize(lcl->GetType());
}
#endif // FEATURE_SIMD

#ifdef DEBUG

// Dump the register a local is in right now. It is only the current location, since the location
// changes and it is updated throughout code generation based on LSRA register assignments.
void Compiler::lvaDumpRegLocation(unsigned lclNum)
{
    LclVarDsc* varDsc = lvaTable + lclNum;

#ifdef TARGET_ARM
    if (varDsc->TypeGet() == TYP_DOUBLE)
    {
        printf("%3s:%-3s    ", getRegName(varDsc->GetRegNum()), getRegName(REG_NEXT(varDsc->GetRegNum())));
    }
    else
#endif
    {
        printf("%3s        ", getRegName(varDsc->GetRegNum()));
    }
}

// Dump the frame location assigned to a local.
// It's the home location, even though the variable doesn't always live in its home location.
void Compiler::lvaDumpFrameLocation(unsigned lclNum)
{
    bool      fpBased;
    int       offset  = lvaFrameAddress(lclNum, &fpBased);
    regNumber baseReg = fpBased ? REG_FPBASE : REG_SPBASE;

#ifdef TARGET_ARM64
    printf("[%s,#%d]  ", getRegName(baseReg), offset);
#else
    printf("[%2s%1s%02XH]  ", getRegName(baseReg), (offset < 0 ? "-" : "+"), (offset < 0 ? -offset : offset));
#endif
}

void Compiler::lvaDumpEntry(unsigned lclNum, size_t refCntWtdWidth)
{
    LclVarDsc* varDsc = lvaGetDesc(lclNum);
    var_types  type   = varDsc->TypeGet();

    printf("; ");

    gtDispLclVar(lclNum);

    if (lvaTrackedCount != 0)
    {
        if (varDsc->HasLiveness())
        {
            printf("[L%02u]", varDsc->GetLivenessBitIndex());
        }
        else
        {
            printf("[   ]");
        }
    }

    if (lvaRefCountState == RCS_NORMAL)
    {
        printf(" (%3u,%*s)", varDsc->GetRefCount(), (int)refCntWtdWidth, refCntWtd2str(varDsc->GetRefWeight()));
    }
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
    else if (lvaRefCountState == RCS_MORPH)
    {
        printf(" (%3u,%3u)", varDsc->GetImplicitByRefParamAnyRefCount(), varDsc->GetImplicitByRefParamCallRefCount());
    }
#endif

    printf(" %-6s", varTypeName(type));

    if (lvaDoneFrameLayout != FINAL_FRAME_LAYOUT)
    {
        if (type == TYP_STRUCT)
        {
            ClassLayout* layout = varDsc->GetLayout();
            assert(layout != nullptr);
            gtDispClassLayout(layout, type);
        }
#if FEATURE_FIXED_OUT_ARGS
        else if (lclNum == lvaOutgoingArgSpaceVar)
        {
            if (codeGen->outgoingArgSpaceSize.HasFinalValue())
            {
                printf("<%u>", codeGen->outgoingArgSpaceSize.GetValue());
            }
            else
            {
                printf("<na>");
            }
        }
#endif
    }
    else
    {
        if (varTypeIsStruct(type) || (type == TYP_BLK))
        {
            printf("<%2u>  ", varDsc->GetFrameSize());
        }
        else
        {
            printf("      ");
        }

        if ((lvaRefCountState == RCS_NORMAL) && (varDsc->GetRefCount() == 0))
        {
            printf("           ");
        }
        else if (varDsc->lvRegister)
        {
            lvaDumpRegLocation(lclNum);
        }
        else if (!varDsc->lvOnFrame)
        {
            printf("registers  ");
        }
        else
        {
            lvaDumpFrameLocation(lclNum);
        }
    }

    if (varDsc->lvDoNotEnregister)
    {
        printf(" do-not-enreg[");
        if (varDsc->lvAddrExposed)
        {
            printf("X");
        }
        if (varTypeIsStruct(varDsc))
        {
            printf("S");
        }
        if (lvaEnregEHVars && varDsc->lvLiveInOutOfHndlr)
        {
            printf("%c", varDsc->lvSingleDefDisqualifyReason);
        }
        if (varDsc->lvLclFieldExpr)
        {
            printf("F");
        }
        if (varDsc->lvLclBlockOpAddr)
        {
            printf("B");
        }
        if (varDsc->lvIsMultiRegArg)
        {
            printf("A");
        }
        if (varDsc->lvIsMultiRegRet)
        {
            printf("R");
        }
#ifdef JIT32_GCENCODER
        if (varDsc->lvPinned)
        {
            printf("P");
        }
#endif
        printf("]");
    }

    if (varDsc->lvIsMultiRegArg)
    {
        printf(" multireg-arg");
    }
    if (varDsc->lvIsMultiRegRet)
    {
        printf(" multireg-ret");
    }
    if (varDsc->lvMustInit)
    {
        printf(" must-init");
    }
    if (varDsc->lvAddrExposed)
    {
        printf(" addr-exposed");
    }
    if (varDsc->lvHasLdAddrOp)
    {
        printf(" ld-addr-op");
    }
    if (varDsc->lvPinned)
    {
        printf(" pinned");
    }
    if (varDsc->lvClassHnd != NO_CLASS_HANDLE)
    {
        printf(" class-hnd");
    }
    if (varDsc->lvClassIsExact)
    {
        printf(" exact");
    }
    if (varDsc->lvLiveInOutOfHndlr)
    {
        printf(" EH-live");
    }
    if (varDsc->lvSpillAtSingleDef)
    {
        printf(" spill-single-def");
    }
    else if (varDsc->lvSingleDefRegCandidate)
    {
        printf(" single-def");
    }
#ifndef TARGET_64BIT
    if (varDsc->lvStructDoubleAlign)
    {
        printf(" double-align");
    }
#endif
    if (varDsc->lvOverlappingFields)
    {
        printf(" overlapping-fields");
    }

    if (compGSReorderStackLayout && !varDsc->lvRegister)
    {
        if (varDsc->lvIsPtr)
        {
            printf(" ptr");
        }
        if (varDsc->lvIsUnsafeBuffer)
        {
            printf(" unsafe-buffer");
        }
    }

    if (varDsc->IsPromotedField())
    {
        LclVarDsc* parentLcl = lvaGetDesc(varDsc->GetPromotedFieldParentLclNum());
        printf(" %s", parentLcl->IsIndependentPromoted() ? "P-INDEP" : "P-DEP");
        printf(" V%02u@%u", varDsc->GetPromotedFieldParentLclNum(), varDsc->GetPromotedFieldOffset());

        if (varDsc->GetPromotedFieldSeq() != nullptr)
        {
            printf(" ");
            dmpFieldSeqFields(varDsc->GetPromotedFieldSeq());
        }
    }

    if (varDsc->lvReason != nullptr)
    {
        printf(" \"%s\"", varDsc->lvReason);
    }

    printf("\n");
}

void Compiler::lvaTableDump()
{
    if (lvaDoneFrameLayout == FINAL_FRAME_LAYOUT)
    {
        printf("; Final");
    }
#ifdef TARGET_ARMARCH
    else if (lvaDoneFrameLayout == REGALLOC_FRAME_LAYOUT)
    {
        printf("; RegAlloc");
    }
#endif
    else
    {
        printf("; Initial");
    }

    printf(" local variable assignments\n;\n");

    unsigned   lclNum;
    LclVarDsc* varDsc;

    // Figure out some sizes, to help line things up

    size_t refCntWtdWidth = 6; // Use 6 as the minimum width

    if (lvaRefCountState == RCS_NORMAL)
    {
        for (lclNum = 0, varDsc = lvaTable; lclNum < lvaCount; lclNum++, varDsc++)
        {
            size_t width = strlen(refCntWtd2str(varDsc->GetRefWeight()));
            if (width > refCntWtdWidth)
            {
                refCntWtdWidth = width;
            }
        }
    }

    for (lclNum = 0, varDsc = lvaTable; lclNum < lvaCount; lclNum++, varDsc++)
    {
        lvaDumpEntry(lclNum, refCntWtdWidth);
    }

    for (TempDsc* temp = codeGen->regSet.tmpListBeg(); temp != nullptr; temp = codeGen->regSet.tmpListNxt(temp))
    {
        printf("; T%02u %25s%*s%7s     ", -temp->tdTempNum(), " ", refCntWtdWidth, " ",
               varTypeName(temp->tdTempType()));
        int offset = temp->tdTempOffs();
        printf(" [%2s%1s%02XH]\n", codeGen->isFramePointerUsed() ? STR_FPBASE : STR_SPBASE, offset < 0 ? "-" : "+",
               abs(offset));
    }

    if (lvaDoneFrameLayout == FINAL_FRAME_LAYOUT)
    {
        printf(";\n");
        printf("; Lcl frame size = %d\n", codeGen->lclFrameSize);
    }
}

void Compiler::lvaDispVarSet(VARSET_VALARG_TP set)
{
    VARSET_TP allVars(VarSetOps::MakeEmpty(this));
    lvaDispVarSet(set, allVars);
}

void Compiler::lvaDispVarSet(VARSET_VALARG_TP set, VARSET_VALARG_TP allVars)
{
    printf("{");

    bool needSpace = false;

    for (unsigned index = 0; index < lvaTrackedCount; index++)
    {
        if (VarSetOps::IsMember(this, set, index))
        {
            unsigned   lclNum;
            LclVarDsc* varDsc;

            /* Look for the matching variable */

            for (lclNum = 0, varDsc = lvaTable; lclNum < lvaCount; lclNum++, varDsc++)
            {
                if ((varDsc->lvVarIndex == index) && varDsc->lvTracked)
                {
                    break;
                }
            }

            if (needSpace)
            {
                printf(" ");
            }
            else
            {
                needSpace = true;
            }

            printf("V%02u", lclNum);
        }
        else if (VarSetOps::IsMember(this, allVars, index))
        {
            if (needSpace)
            {
                printf(" ");
            }
            else
            {
                needSpace = true;
            }

            printf("   ");
        }
    }

    printf("}");
}

#endif // DEBUG
