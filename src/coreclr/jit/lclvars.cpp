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
    CORINFO_ARG_LIST_HANDLE localsSig = info.compMethodInfo->locals.args;
    unsigned                varNum    = info.compArgsCount;

    for (unsigned i = 0; i < info.compMethodInfo->locals.numArgs;
         i++, varNum++, localsSig = info.compCompHnd->getArgNext(localsSig))
    {
        LclVarDsc* varDsc = lvaGetDesc(varNum);

        CORINFO_CLASS_HANDLE typeHnd;
        CorInfoTypeWithMod   corInfoTypeWithMod =
            info.compCompHnd->getArgType(&info.compMethodInfo->locals, localsSig, &typeHnd);
        CorInfoType corInfoType = strip(corInfoTypeWithMod);

        lvaInitVarDsc(varDsc, varNum, corInfoType, typeHnd);

        if ((corInfoTypeWithMod & CORINFO_TYPE_MOD_PINNED) != 0)
        {
            if ((corInfoType == CORINFO_TYPE_CLASS) || (corInfoType == CORINFO_TYPE_BYREF))
            {
                JITDUMP("Setting lvPinned for V%02u\n", varNum);
                varDsc->lvPinned = true;
            }
            else
            {
                JITDUMP("Ignoring pin for non-GC type V%02u\n", varNum);
            }
        }

        varDsc->lvOnFrame = true;

        if (corInfoType == CORINFO_TYPE_CLASS)
        {
            lvaSetClass(varNum, info.compCompHnd->getArgClass(&info.compMethodInfo->locals, localsSig));
        }

        if (opts.IsOSR() && info.compPatchpointInfo->IsExposed(varNum))
        {
            JITDUMP("-- V%02u is OSR exposed\n", varNum);
            varDsc->lvHasLdAddrOp = true;

            if (!varDsc->TypeIs(TYP_STRUCT))
            {
                lvaSetVarAddrExposed(varNum);
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
        codeGen->regSet.rsMaskPreSpillRegArg |= RBM_ARG_REGS;
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

    InitVarDscInfo paramInfo(intRegCount, floatRegCount);
    compArgSize = 0;

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

        paramInfo.varNum++;
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
    compArgSize = GetOutgoingArgByteSize(compArgSize);

#ifdef TARGET_X86
    lvaInitGenericsContextParam(paramInfo);
    lvaInitVarargsHandleParam(paramInfo);
#endif

    noway_assert(paramInfo.varNum == info.compArgsCount);
    assert(paramInfo.intRegArgNum <= MAX_REG_ARG);

    codeGen->intRegState.rsCalleeRegArgCount   = paramInfo.intRegArgNum;
    codeGen->floatRegState.rsCalleeRegArgCount = paramInfo.floatRegArgNum;

#if FEATURE_FASTTAILCALL
    info.compArgStackSize = paramInfo.stackArgSize;
#endif

    // The total argument size must be aligned.
    noway_assert((compArgSize % REGSIZE_BYTES) == 0);

#ifdef TARGET_X86
    // The x86 ret instruction has a 16 bit immediate so we cannot easily pop more than
    // 2^16 bytes of stack arguments. Could be handled correctly but it will be very
    // difficult for fully interruptible code
    if (compArgSize >= (1u << 16))
    {
        IMPL_LIMITATION("Too many arguments for the \"ret\" instruction to pop");
    }
#endif
}

void Compiler::lvaInitThisParam(InitVarDscInfo& paramInfo)
{
    if (info.compIsStatic)
    {
        return;
    }

    assert(paramInfo.varNum == 0);
    assert(paramInfo.intRegArgNum == 0);

    lvaArg0Var       = 0;
    info.compThisArg = 0;

    LclVarDsc* lcl = lvaGetDesc(paramInfo.varNum);

    if ((info.compClassAttr & CORINFO_FLG_VALUECLASS) != 0)
    {
        lcl->SetType(TYP_BYREF);
    }
    else
    {
        lcl->SetType(TYP_REF);
        lvaSetClass(paramInfo.varNum, info.compClassHnd);
    }

    lcl->lvIsParam  = true;
    lcl->lvIsPtr    = true;
    lcl->lvIsRegArg = true;
    lcl->lvOnFrame  = true;

    lcl->SetArgReg(genMapIntRegArgNumToRegNum(paramInfo.allocRegArg(TYP_INT)));
#if FEATURE_MULTIREG_ARGS
    lcl->SetOtherArgReg(REG_NA);
#endif

    JITDUMP("'this' passed in register %s\n", getRegName(lcl->GetArgReg()));

    compArgSize += REGSIZE_BYTES;
    paramInfo.varNum++;
}

void Compiler::lvaInitRetBufParam(InitVarDscInfo& paramInfo, bool useFixedRetBufReg)
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

    info.compRetBuffArg = paramInfo.varNum;

    LclVarDsc* lcl = lvaGetDesc(paramInfo.varNum);

    lcl->SetType(TYP_BYREF);
    lcl->lvIsParam = true;
    lcl->lvOnFrame = true;

    if (useFixedRetBufReg && hasFixedRetBuffReg())
    {
        lcl->lvIsRegArg = true;
        lcl->SetArgReg(theFixedRetBuffReg());
    }
    else if (paramInfo.canEnreg(TYP_INT))
    {
        lcl->lvIsRegArg = true;
        lcl->SetArgReg(genMapIntRegArgNumToRegNum(paramInfo.allocRegArg(TYP_INT)));
    }

#if FEATURE_MULTIREG_ARGS
    lcl->SetOtherArgReg(REG_NA);
#endif

    assert(!lcl->lvIsRegArg || isValidIntArgReg(lcl->GetArgReg()));

    if (lcl->lvIsRegArg)
    {
        JITDUMP("'__retBuf' passed in register %s\n", getRegName(lcl->GetArgReg()));
    }

    compArgSize += REGSIZE_BYTES;
    paramInfo.varNum++;
}

void Compiler::lvaInitGenericsContextParam(InitVarDscInfo& paramInfo)
{
    if ((info.compMethodInfo->args.callConv & CORINFO_CALLCONV_PARAMTYPE) == 0)
    {
        return;
    }

    info.compTypeCtxtArg = paramInfo.varNum;

    LclVarDsc* lcl = lvaGetDesc(paramInfo.varNum);

    lcl->SetType(TYP_I_IMPL);
    lcl->lvIsParam = true;
    lcl->lvOnFrame = true;

    if (paramInfo.canEnreg(TYP_I_IMPL))
    {
        lcl->lvIsRegArg = true;
        lcl->SetArgReg(genMapIntRegArgNumToRegNum(paramInfo.allocRegArg(TYP_I_IMPL)));
#if FEATURE_MULTIREG_ARGS
        lcl->SetOtherArgReg(REG_NA);
#endif

        JITDUMP("'GenCtxt' passed in register %s\n", getRegName(lcl->GetArgReg()));
    }
    else
    {
#if FEATURE_FASTTAILCALL
        lcl->SetStackOffset(paramInfo.stackArgSize);
        paramInfo.stackArgSize += REGSIZE_BYTES;
#endif
    }

    compArgSize += REGSIZE_BYTES;
    paramInfo.varNum++;

#ifdef TARGET_X86
    if (info.compIsVarArgs)
    {
        lcl->SetStackOffset(compArgSize);
    }
#endif
}

void Compiler::lvaInitVarargsHandleParam(InitVarDscInfo& paramInfo)
{
    if (!info.compIsVarArgs)
    {
        return;
    }

    lvaVarargsHandleArg = paramInfo.varNum;

    LclVarDsc* lcl = lvaGetDesc(paramInfo.varNum);

    lcl->SetType(TYP_I_IMPL);
    lcl->lvIsParam = true;
    lcl->lvOnFrame = true;

    // Make sure this lives in the stack, address may be reported to the VM.
    // TODO-CQ: This should probably be only DNER but that causes problems,
    // so, for expedience, I switched back to this heavyweight hammer.
    // But I think it should be possible to switch; it may just work now
    // that other problems are fixed.
    lvaSetAddressExposed(lcl);

    if (paramInfo.canEnreg(TYP_I_IMPL))
    {
        unsigned regIndex = paramInfo.allocRegArg(TYP_I_IMPL);

        lcl->lvIsRegArg = true;
        lcl->SetArgReg(genMapIntRegArgNumToRegNum(regIndex));
#if FEATURE_MULTIREG_ARGS
        lcl->SetOtherArgReg(REG_NA);
#endif

#ifdef TARGET_ARM
        // This has to be spilled right in front of the user params and we have
        // to pre-spill all the register params explicitly because we only have
        // have symbols for the declared ones, not any potential variadic ones.
        for (unsigned ix = regIndex; ix < ArrLen(intArgMasks); ix++)
        {
            codeGen->regSet.rsMaskPreSpillRegArg |= intArgMasks[ix];
        }
#endif // TARGET_ARM

        JITDUMP("'VarArgHnd' passed in register %s\n", getRegName(lcl->GetArgReg()));
    }
    else
    {
#if FEATURE_FASTTAILCALL
        lcl->SetStackOffset(paramInfo.stackArgSize);
        paramInfo.stackArgSize += REGSIZE_BYTES;
#endif
    }

    compArgSize += REGSIZE_BYTES;
    paramInfo.varNum++;

#ifdef TARGET_X86
    lcl->SetStackOffset(compArgSize);
    lvaVarargsBaseOfStkArgs = lvaNewTemp(TYP_I_IMPL, false DEBUGARG("Varargs BaseOfStkArgs"));
#endif
}

void Compiler::lvaInitUserParams(InitVarDscInfo& paramInfo, bool skipFirstParam)
{
    assert(!skipFirstParam || (info.compMethodInfo->args.numArgs != 0));

#ifdef WINDOWS_AMD64_ABI
    assert(paramInfo.floatRegArgNum == paramInfo.intRegArgNum);
#endif

    CORINFO_ARG_LIST_HANDLE param      = info.compMethodInfo->args.args;
    unsigned                paramCount = info.compMethodInfo->args.numArgs;

    if (skipFirstParam)
    {
        param = info.compCompHnd->getArgNext(param);
        paramCount--;
    }

    for (unsigned i = 0; i < paramCount; i++, paramInfo.varNum++, param = info.compCompHnd->getArgNext(param))
    {
        lvaInitUserParam(paramInfo, param);
    }
}

void Compiler::lvaInitUserParam(InitVarDscInfo& paramInfo, CORINFO_ARG_LIST_HANDLE param)
{
    CORINFO_CLASS_HANDLE typeHnd = nullptr;
    CorInfoType          corType = strip(info.compCompHnd->getArgType(&info.compMethodInfo->args, param, &typeHnd));

    LclVarDsc* lcl = lvaGetDesc(paramInfo.varNum);

    lcl->lvIsParam = true;
    lcl->lvOnFrame = true;
    lvaInitVarDsc(lcl, paramInfo.varNum, corType, typeHnd);

    if (opts.IsOSR() && info.compPatchpointInfo->IsExposed(paramInfo.varNum))
    {
        JITDUMP("-- V%02u is OSR exposed\n", paramInfo.varNum);

        lcl->lvHasLdAddrOp = true;
        lvaSetVarAddrExposed(paramInfo.varNum);
    }

    if (corType == CORINFO_TYPE_CLASS)
    {
        lvaSetClass(paramInfo.varNum, info.compCompHnd->getArgClass(&info.compMethodInfo->args, param));
    }
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
    else if (varTypeIsStruct(lcl->GetType()) &&
             abiGetStructParamType(lcl->GetLayout(), info.compIsVarArgs).kind == SPK_ByReference)
    {
        JITDUMP("Marking V%02u as a byref parameter\n", paramInfo.varNum);
        lcl->lvIsImplicitByRef = true;

        // TODO-MIKE-Cleanup: If it's implicit-by-ref we know that it needs a single integer
        // register or stack slot and can skip a lot of the mess below, like HFA checks.
    }
#endif

    lvaAllocUserParam(paramInfo, param, lcl);
}

void Compiler::lvaAllocUserParam(InitVarDscInfo& paramInfo, CORINFO_ARG_LIST_HANDLE param, LclVarDsc* lcl)
{
    unsigned  paramSize = lvaGetParamAllocSize(param, &info.compMethodInfo->args);
    unsigned  slots     = (paramSize + REGSIZE_BYTES - 1) / REGSIZE_BYTES;
    unsigned  regCount  = slots;
    var_types regType   = lcl->GetType();
    var_types hfaType   = TYP_UNDEF;

    if (varTypeIsStruct(lcl->GetType()))
    {
#if defined(TARGET_ARM64) && defined(TARGET_WINDOWS)
        // win-arm64 varargs does not use HFAs and can split a STRUCT arg between the last
        // integer reg arg (x7) and the first stack arg slot.
        if (info.compIsVarArgs)
        {
            if (paramInfo.canEnreg(TYP_INT, 1) && !paramInfo.canEnreg(TYP_INT, slots))
            {
                // Make sure this is always treated as STRUCT even if it's a SIMD type.
                // The code below assigns x8 to "OtherArgReg", that's complete nonsense but
                // AssignVirtualFrameOffsetToArg uses that to recognize this split param.
                regType  = TYP_STRUCT;
                regCount = 1;
            }
        }
        else
#endif
            if (lcl->GetLayout()->IsHfa())
        {
            slots    = lcl->GetLayout()->GetHfaRegCount();
            hfaType  = lcl->GetLayout()->GetHfaElementType();
            regType  = hfaType;
            regCount = slots;
        }
    }
    else
    {
#ifdef TARGET_ARMARCH
        regType = mangleVarArgsType(regType);
#endif
    }

    bool canPassArgInRegisters = false;

#ifdef TARGET_ARM
    const bool isSoftFPPreSpill = opts.UseSoftFP() && varTypeIsFloating(lcl->GetType());
    // On ARM we pass the first 4 words of integer params and non-HFA structs in registers.
    // But we pre-spill user params in varargs methods and structs.
    bool     preSpill = info.compIsVarArgs || isSoftFPPreSpill;
    unsigned regAlign = 1;

    if (lcl->TypeIs(TYP_LONG, TYP_DOUBLE))
    {
        regAlign = 2;
    }
    else if (lcl->TypeIs(TYP_STRUCT))
    {
        assert(lcl->lvSize() == paramSize);

        if (lcl->lvStructDoubleAlign)
        {
            regAlign = 2;
        }

        if (regType == TYP_STRUCT)
        {
            // TODO-Arm32-Windows: vararg struct should be forced to split like ARM64 above.

            // Are we going to split the struct between registers and stack? We can do that as long as
            // no floating-point params have been put on the stack.
            //
            // From the ARM Procedure Call Standard:
            // Rule C.5: "If the NCRN is less than r4 **and** the NSAA is equal to the SP,"
            // then split the argument between registers and stack. Implication: if something
            // has already been spilled to the stack, then anything that would normally be
            // split between the core registers and the stack will be put on the stack.
            // Anything that follows will also be on the stack. However, if something from
            // floating point regs has been spilled to the stack, we can still use r0-r3 until they are full.

            regCount = 1;
            preSpill = true;

            if (paramInfo.canEnreg(TYP_INT, 1) && !paramInfo.canEnreg(TYP_INT, slots) &&
                paramInfo.existAnyFloatStackArgs())
            {
                // Prevent all future use of integer registers
                paramInfo.setAllRegArgUsed(TYP_INT);
                // This struct won't be prespilled, since it will go on the stack
                preSpill = false;
            }
        }
    }

    if (isRegParamType(regType))
    {
        compArgSize += paramInfo.alignReg(regType, regAlign) * REGSIZE_BYTES;
    }

    if (preSpill)
    {
        regMaskTP paramRegMask = RBM_NONE;

        for (unsigned i = 0; i < slots; i++)
        {
            if (!paramInfo.canEnreg(TYP_INT, i + 1))
            {
                break;
            }

            paramRegMask |= genMapArgNumToRegMask(paramInfo.regArgNum(TYP_INT) + i, TYP_INT);
        }

        if (regAlign == 2)
        {
            paramInfo.doubleAlignMask |= paramRegMask;
        }

        codeGen->regSet.rsMaskPreSpillRegArg |= paramRegMask;
    }
#endif // TARGET_ARM

#if defined(UNIX_AMD64_ABI)
    if (varTypeIsStruct(regType))
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

            if (((intRegCount == 0) || paramInfo.canEnreg(TYP_INT, intRegCount)) &&
                ((floatRegCount == 0) || paramInfo.canEnreg(TYP_FLOAT, floatRegCount)))
            {
                canPassArgInRegisters = true;
            }
        }
    }
    else
#elif defined(TARGET_X86)
    if (varTypeIsStruct(regType))
    {
        canPassArgInRegisters = isTrivialPointerSizedStruct(lcl->GetLayout()) && paramInfo.canEnreg(TYP_INT, regCount);
    }
    else
#endif
    {
        canPassArgInRegisters = paramInfo.canEnreg(regType, regCount);
    }

    if (canPassArgInRegisters)
    {
        lcl->lvIsRegArg = true;

        if ((hfaType != TYP_UNDEF) && (lcl->GetLayout()->GetHfaElementCount() > 1))
        {
            lcl->lvIsMultiRegArg = true;
        }

#if defined(UNIX_AMD64_ABI)
        if (varTypeIsStruct(regType))
        {
            var_types regType0     = lcl->GetLayout()->GetSysVAmd64AbiRegType(0);
            unsigned  regParamNum0 = paramInfo.allocRegArg(regType0);
            lcl->SetParamReg(0, genMapRegArgNumToRegNum(regParamNum0, regType0));

            if (lcl->GetLayout()->GetSysVAmd64AbiRegCount() >= 2)
            {
                var_types regType1     = lcl->GetLayout()->GetSysVAmd64AbiRegType(1);
                unsigned  regParamNum1 = paramInfo.allocRegArg(regType1);
                lcl->SetParamReg(1, genMapRegArgNumToRegNum(regParamNum1, regType1));
                lcl->lvIsMultiRegArg = true;
            }
        }
        else
#elif defined(TARGET_ARM64)
        if (regType == TYP_STRUCT)
        {
            unsigned regParamNum = paramInfo.allocRegArg(TYP_INT, slots);

            lcl->SetArgReg(genMapIntRegArgNumToRegNum(regParamNum));

            if (slots == 2)
            {
                lcl->SetOtherArgReg(genMapIntRegArgNumToRegNum(regParamNum + 1));
                lcl->lvIsMultiRegArg = true;
            }
        }
        else
#elif defined(TARGET_ARM)
        if (lcl->TypeIs(TYP_LONG))
        {
            unsigned regParamNum = paramInfo.allocRegArg(TYP_INT, 2);

            lcl->SetArgReg(genMapIntRegArgNumToRegNum(regParamNum));
        }
        else if (varTypeIsStruct(regType))
        {
            unsigned regParamNum = paramInfo.allocRegArg(TYP_INT, slots);

            lcl->SetArgReg(genMapIntRegArgNumToRegNum(regParamNum));
        }
        else
#endif
        {
            unsigned regParamNum = paramInfo.allocRegArg(regType, slots);

            lcl->SetArgReg(genMapRegArgNumToRegNum(regParamNum, regType));
        }

        JITDUMP("Param V%02u registers: ", paramInfo.varNum);

#ifdef DEBUG
        if (verbose)
        {
#ifdef UNIX_AMD64_ABI
            printf("%s", getRegName(lcl->GetParamReg(0)));

            if (lcl->GetParamReg(1) != REG_NA)
            {
                printf(", %s", getRegName(lcl->GetParamReg(1)));
            }
#else // !UNIX_AMD64_ABI
            bool     isFloat   = varTypeUsesFloatReg(regType);
            unsigned regArgNum = genMapRegNumToRegArgNum(lcl->GetArgReg(), regType);

            for (unsigned ix = 0; ix < slots; ix++, regArgNum++)
            {
                if (ix > 0)
                {
                    printf(",");
                }

                if (!isFloat && (regArgNum >= paramInfo.maxIntRegArgNum))
                {
                    printf(" stack slots:%d", slots - ix);
                    break;
                }

#ifdef TARGET_ARM
                if (isFloat)
                {
                    if (regType == TYP_DOUBLE)
                    {
                        printf("%s/%s", getRegName(genMapFloatRegArgNumToRegNum(regArgNum)),
                               getRegName(genMapFloatRegArgNumToRegNum(regArgNum + 1)));

                        assert(ix + 1 < slots);
                        ++ix;
                        ++regArgNum;
                    }
                    else
                    {
                        printf("%s", getRegName(genMapFloatRegArgNumToRegNum(regArgNum)));
                    }
                }
                else
#endif // TARGET_ARM
                {
                    printf("%s", getRegName(genMapIntRegArgNumToRegNum(regArgNum)));
                }
            }
#endif // !UNIX_AMD64_ABI

            printf("\n");
        }
#endif // DEBUG
    }
    else
    {
#ifdef TARGET_ARMARCH
        paramInfo.setAllRegArgUsed(regType);

#ifdef TARGET_ARM
        if (varTypeUsesFloatReg(regType))
        {
            paramInfo.setAnyFloatStackArgs();
        }
#endif
#endif

#if FEATURE_FASTTAILCALL
        const unsigned argAlignment = lvaGetParamAlignment(lcl->GetType(), (hfaType == TYP_FLOAT));

#ifdef OSX_ARM64_ABI
        paramInfo.stackArgSize = roundUp(paramInfo.stackArgSize, argAlignment);
#endif

        assert((paramSize % argAlignment) == 0);
        assert((paramInfo.stackArgSize % argAlignment) == 0);
        JITDUMP("set user arg V%02u offset to %u\n", paramInfo.varNum, paramInfo.stackArgSize);

        lcl->SetStackOffset(paramInfo.stackArgSize);
        paramInfo.stackArgSize += paramSize;
#endif // FEATURE_FASTTAILCALL
    }

    compArgSize += paramSize;

    if (info.compIsVarArgs ARM_ONLY(|| isSoftFPPreSpill))
    {
#ifdef TARGET_X86
        lcl->SetStackOffset(compArgSize);
#else
        // TODO-CQ: We shouldn't have to go as far as to declare these AX, DNER should suffice.
        lvaSetAddressExposed(lcl);
#endif
    }
}

#ifdef TARGET_ARM
void Compiler::lvaAlignPreSpillParams(regMaskTP doubleAlignMask)
{
    if (doubleAlignMask != RBM_NONE)
    {
        assert(RBM_ARG_REGS == 0xF);
        assert((doubleAlignMask & RBM_ARG_REGS) == doubleAlignMask);

        if (doubleAlignMask != RBM_NONE && doubleAlignMask != RBM_ARG_REGS)
        {
            // 'double aligned types' can begin only at r0 or r2 and we always expect at least two registers to be used
            // Note that in rare cases, we can have double-aligned structs of 12 bytes (if specified explicitly with
            // attributes)
            assert((doubleAlignMask == 0b0011) || (doubleAlignMask == 0b1100) ||
                   (doubleAlignMask == 0b0111) /* || 0b1111 is if'ed out */);

            // Now if doubleAlignMask is xyz1 i.e., the struct starts in r0, and we prespill r2 or r3
            // but not both, then the stack would be misaligned for r0. So spill both
            // r2 and r3.
            //
            // ; +0 --- caller SP double aligned ----
            // ; -4 r2    r3
            // ; -8 r1    r1
            // ; -c r0    r0   <-- misaligned.
            // ; callee saved regs
            bool startsAtR0 = (doubleAlignMask & 1) == 1;
            bool r2XorR3    = ((codeGen->regSet.rsMaskPreSpillRegArg & RBM_R2) == 0) !=
                           ((codeGen->regSet.rsMaskPreSpillRegArg & RBM_R3) == 0);
            if (startsAtR0 && r2XorR3)
            {
                codeGen->regSet.rsMaskPreSpillAlign =
                    (~codeGen->regSet.rsMaskPreSpillRegArg & ~doubleAlignMask) & RBM_ARG_REGS;
            }
        }
    }
}
#endif // TARGET_ARM

void Compiler::lvaInitVarDsc(LclVarDsc* varDsc, unsigned varNum, CorInfoType corInfoType, CORINFO_CLASS_HANDLE typeHnd)
{
    noway_assert(varDsc == &lvaTable[varNum]);

    switch (corInfoType)
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
            varDsc->lvIsPtr = true;
            break;
        default:
            break;
    }

    var_types type = JITtype2varType(corInfoType);

    if (varTypeIsStruct(type))
    {
        lvaSetStruct(varNum, typeHnd, true);

#if defined(TARGET_WINDOWS) && defined(TARGET_ARM64)
        if (info.compIsVarArgs)
        {
            varDsc->SetIsHfa(false);
        }
#endif
    }
    else
    {
        varDsc->SetType(type);

        if (varTypeIsFloating(type))
        {
            compFloatingPointUsed = true;
        }
#if OPT_BOOL_OPS
        else if (type == TYP_BOOL)
        {
            varDsc->lvIsBoolean = true;
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

            varDsc->lvImpTypeInfo = typeInfo(TI_STRUCT, typeHnd);
        }
    }

    INDEBUG(varDsc->SetStackOffset(BAD_STK_OFFS);)

#if FEATURE_MULTIREG_ARGS
    varDsc->SetOtherArgReg(REG_NA);
#endif
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

bool Compiler::lvaIsMultiRegStructParam(LclVarDsc* lcl)
{
    assert(lcl->IsParam());

    if (!varTypeIsStruct(lcl->GetType()))
    {
        return false;
    }

    // TODO-MIKE-Throughput: Isn't there enough information in LclVarDsc
    // to avoid calling abiGetStructParamType again?

    switch (abiGetStructParamType(lcl->GetLayout(), info.compIsVarArgs).kind)
    {
#ifdef FEATURE_HFA
        case SPK_ByValueAsHfa:
            return true;
#endif
#if defined(UNIX_AMD64_ABI) || defined(TARGET_ARM64)
        case SPK_ByValue:
            // TODO-MIKE-Review: Why is this excluded on ARM? And how exactly does SPK_ByValue
            // imply multireg? Such params can be passed on stack on UNIX_AMD64_ABI. Maybe
            // it's not that ARM is excluded, maybe it's that UNIX_AMD64_ABI shouldn't be
            // included because only on ARM64 SPK_ByValue implies multireg. Though the same
            // is true about HFA, SPK_ByValueAsHfa doesn't actually mean that the parameter
            // is passed in registers.
            return true;
#endif
        default:
            return false;
    }
}

void Compiler::lvaSetStruct(unsigned lclNum, CORINFO_CLASS_HANDLE classHandle, bool checkUnsafeBuffer)
{
    lvaSetStruct(lclNum, typGetObjLayout(classHandle), checkUnsafeBuffer);
}

void Compiler::lvaSetStruct(unsigned lclNum, ClassLayout* layout, bool checkUnsafeBuffer)
{
    assert(!layout->IsBlockLayout());

    noway_assert(lclNum < lvaCount);

    LclVarDsc* varDsc = lvaGetDesc(lclNum);

    if (varDsc->lvExactSize != 0)
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

        assert(varDsc->GetType() == typGetStructType(layout));
        assert(varDsc->lvExactSize == layout->GetSize());
    }
    else
    {
        varDsc->lvType = TYP_STRUCT;
        varDsc->SetLayout(layout);
        varDsc->lvExactSize   = layout->GetSize();
        varDsc->lvImpTypeInfo = typeInfo(TI_STRUCT, layout->GetClassHandle());

        if (layout->IsValueClass())
        {
#if FEATURE_SIMD
            var_types simdType = typGetStructType(layout);

            if (simdType != TYP_STRUCT)
            {
                assert(varTypeIsSIMD(simdType));
                varDsc->lvType = simdType;
            }
#endif

            // TODO-MIKE-Cleanup: This should only be needed on params and only if HFAs are
            // available (i.e. not in varargs methods on win-arm64).
            layout->EnsureHfaInfo(this);
            varDsc->SetIsHfa(layout->IsHfa());
        }
    }

    if (!varTypeIsSIMD(varDsc->GetType()))
    {
        // TODO-MIKE-Throughput: ClassLayout already queries class attributes, it should store
        // "overlapping fields" and "unsafe value class" bits so we don't have to do it again.

        unsigned classAttribs = info.compCompHnd->getClassAttribs(layout->GetClassHandle());

        varDsc->lvOverlappingFields = StructHasOverlappingFields(classAttribs);

        // Check whether this local is an unsafe value type and requires GS cookie protection.
        // GS checks require the stack to be re-ordered, which can't be done with EnC.
        if (checkUnsafeBuffer && ((classAttribs & CORINFO_FLG_UNSAFE_VALUECLASS) != 0) && !opts.compDbgEnC)
        {
            setNeedsGSSecurityCookie();
            compGSReorderStackLayout = true;
            varDsc->lvIsUnsafeBuffer = true;
        }
    }

#ifndef TARGET_64BIT
    bool doubleAlignHint = false;
#ifdef TARGET_X86
    doubleAlignHint = true;
#endif
    if (info.compCompHnd->getClassAlignmentRequirement(layout->GetClassHandle(), doubleAlignHint) == 8)
    {
        JITDUMP("Marking struct in V%02i with double align flag\n", lclNum);
        varDsc->lvStructDoubleAlign = 1;
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
    if (StructHasNoPromotionFlagSet(typeFlags))
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
unsigned Compiler::lvaLclSize(unsigned lclNum)
{
    LclVarDsc* lcl = lvaGetDesc(lclNum);

    switch (lcl->GetType())
    {
        case TYP_BLK:
#if FEATURE_FIXED_OUT_ARGS
            if (lclNum == lvaOutgoingArgSpaceVar)
            {
                return lvaOutgoingArgSpaceSize.GetValue();
            }
#endif
            FALLTHROUGH;
        case TYP_STRUCT:
            return lcl->lvSize();

        default:
#ifdef TARGET_64BIT
            if (lcl->lvQuirkToLong)
            {
                noway_assert(lcl->lvAddrExposed);
                return genTypeStSz(TYP_LONG) * sizeof(int); // return 8  (2 * 4)
            }
#endif
            return genTypeStSz(lcl->GetType()) * sizeof(int);
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
            if (dsc1->lvIsRegArg)
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
            if (dsc2->lvIsRegArg)
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

        if ((weight1 != 0) && dsc1->lvIsRegArg)
        {
            weight1 += 2 * BB_UNITY_WEIGHT;
        }

        if ((weight2 != 0) && dsc2->lvIsRegArg)
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

        if (lcl->lvRefCnt() == 0)
        {
            assert(lcl->lvRefCntWtd() == 0);

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

unsigned LclVarDsc::lvSize() const // Size needed for storage representation. Only used for structs or TYP_BLK.
{
    // TODO-Review: Sometimes we get called on ARM with HFA struct variables that have been promoted,
    // where the struct itself is no longer used because all access is via its member fields.
    // When that happens, the struct is marked as unused and its type has been changed to
    // TYP_INT (to keep the GC tracking code from looking at it).
    // See Compiler::raAssignVars() for details. For example:
    //      N002 (  4,  3) [00EA067C] -------------               return    struct $346
    //      N001 (  3,  2) [00EA0628] -------------                  lclVar    struct(U) V03 loc2
    //                                                                        float  V03.f1 (offs=0x00) -> V12 tmp7
    //                                                                        f8 (last use) (last use) $345
    // Here, the "struct(U)" shows that the "V03 loc2" variable is unused. Not shown is that V03
    // is now TYP_INT in the local variable table. It's not really unused, because it's in the tree.

    assert(varTypeIsStruct(lvType) || (lvType == TYP_BLK));

    unsigned size = GetSize();

    if (lvIsParam)
    {
        assert(varTypeIsStruct(lvType));
        bool     isFloatHfa   = lvIsHfa() && (m_layout->GetHfaElementType() == TYP_FLOAT);
        unsigned argAlignment = Compiler::lvaGetParamAlignment(lvType, isFloatHfa);
        return roundUp(size, argAlignment);
    }

#if defined(FEATURE_SIMD) && !defined(TARGET_64BIT)
    // For 32-bit architectures, we make local variable SIMD12 types 16 bytes instead of just 12. We can't do
    // this for arguments, which must be passed according the defined ABI. We don't want to do this for
    // dependently promoted struct fields, but we don't know that here. See lvaMapSimd12ToSimd16().
    // (Note that for 64-bits, we are already rounding up to 16.)
    if (lvType == TYP_SIMD12)
    {
        assert(size == 12);
        return 16;
    }
#endif // defined(FEATURE_SIMD) && !defined(TARGET_64BIT)

    return roundUp(size, TARGET_POINTER_SIZE);
}

/**********************************************************************************
* Get type of a variable when passed as an argument.
*/
var_types LclVarDsc::lvaArgType()
{
    var_types type = lvType;

#ifdef TARGET_AMD64
#ifdef UNIX_AMD64_ABI
    if (type == TYP_STRUCT)
    {
        NYI("lvaArgType");
    }
#else  //! UNIX_AMD64_ABI
    if (type == TYP_STRUCT)
    {
        switch (m_layout->GetSize())
        {
            case 1:
                type = TYP_BYTE;
                break;
            case 2:
                type = TYP_SHORT;
                break;
            case 4:
                type = TYP_INT;
                break;
            case 8:
                type = m_layout->GetGCPtrType(0);
                break;
            default:
                type = TYP_BYREF;
                break;
        }
    }
#endif // !UNIX_AMD64_ABI
#elif defined(TARGET_ARM64)
    if (type == TYP_STRUCT)
    {
        NYI("lvaArgType");
    }
#elif defined(TARGET_X86)
// Nothing to do; use the type as is.
#else
    NYI("lvaArgType");
#endif // TARGET_AMD64

    return type;
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

    if ((size > MaxFrameSize) || (compLclFrameSize + size > MaxFrameSize))
    {
        BADCODE("Frame size overflow");
    }

    compLclFrameSize += size;
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

unsigned Compiler::lvaGetMaxSpillTempSize()
{
    assert(lvaDoneFrameLayout != NO_FRAME_LAYOUT);

    return codeGen->regSet.tmpGetTotalSize();
}

// Returns the number of bytes required for the given parameter.
// Usually this is just the param type size, rounded up to the register size
// but there are special case like implicit by ref params and osx-arm64 weird
// parameter packing.
unsigned Compiler::lvaGetParamAllocSize(CORINFO_ARG_LIST_HANDLE param, CORINFO_SIG_INFO* sig)
{
#ifdef WINDOWS_AMD64_ABI
    return REGSIZE_BYTES;
#else
    CORINFO_CLASS_HANDLE paramClass;
    var_types            paramType = CorTypeToVarType(strip(info.compCompHnd->getArgType(sig, param, &paramClass)));
    unsigned             paramSize;
#ifdef TARGET_ARM64
    var_types            hfaType = TYP_UNDEF;
#endif

    if (!varTypeIsStruct(paramType))
    {
        paramSize = varTypeSize(paramType);
    }
    else
    {
        ClassLayout* layout = typGetObjLayout(paramClass);

        paramSize = layout->GetSize();

#ifdef TARGET_ARM64
        if (paramSize > MAX_PASS_MULTIREG_BYTES)
        {
            return REGSIZE_BYTES;
        }

        layout->EnsureHfaInfo(this);

#ifdef TARGET_WINDOWS
        if (layout->IsHfa() && !info.compIsVarArgs)
#else
        if (layout->IsHfa())
#endif
        {
            hfaType = layout->GetHfaElementType();
        }

        if ((paramSize > 2 * REGSIZE_BYTES) && (hfaType == TYP_UNDEF))
        {
            return REGSIZE_BYTES;
        }
#endif // TARGET_ARM64
    }

#ifdef OSX_ARM64_ABI
    return roundUp(paramSize, lvaGetParamAlignment(paramType, (hfaType == TYP_FLOAT)));
#else
    return roundUp(paramSize, REGSIZE_BYTES);
#endif
#endif // !WINDOWS_AMD64_ABI
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

    lvaAssignVirtualFrameOffsetsToArgs();
    lvaAssignVirtualFrameOffsetsToLocals();
    lvaAlignFrame();
    lvaFixVirtualFrameOffsets();
    lvaAssignFrameOffsetsToPromotedStructs();
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

        lcl->SetStackOffset(codeGen->genCallerSPtoInitialSPdelta() + lvaOutgoingArgSpaceSize);

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
            continue; // Assigned later in lvaAssignFrameOffsetsToPromotedStructs
        }

        if (!lcl->lvOnFrame && (!lcl->IsParam()
#ifndef TARGET_AMD64
                                || (lcl->IsRegParam()
#if defined(TARGET_ARM) && defined(PROFILING_SUPPORTED)
                                    && compIsProfilerHookNeeded() &&
                                    // We need assign stack offsets for prespilled arguments
                                    !lcl->IsPreSpilledRegParam(codeGen->regSet.rsMaskPreSpillRegs(false))
#endif
                                        )
#endif // !TARGET_AMD64
                                    ))
        {
            continue;
        }

        JITDUMP("-- V%02u was %d, now %d\n", lclNum, lcl->GetStackOffset(), lcl->GetStackOffset() + delta);

        lcl->SetStackOffset(lcl->GetStackOffset() + delta);

#if defined(TARGET_X86) && DOUBLE_ALIGN
        if (genDoubleAlign() && !codeGen->isFramePointerUsed() && lcl->lvFramePointerBased)
        {
            lcl->SetStackOffset(lcl->GetStackOffset() - delta);

            // We need to re-adjust the offsets of the parameters so they
            // are EBP relative rather than stack/frame pointer relative.

            lcl->SetStackOffset(lcl->GetStackOffset() + 2 * REGSIZE_BYTES); // return address and pushed EBP

            noway_assert(lcl->GetStackOffset() >= FIRST_ARG_STACK_OFFS);
        }
#endif

        // On System V environments the stack offset could be 0 for params passed in registers.
        // For normal methods only EBP relative references can have negative offsets.
        assert(codeGen->isFramePointerUsed() || (lcl->GetStackOffset() >= 0));
    }

    assert(codeGen->regSet.tmpAllFree());

    for (TempDsc* temp = codeGen->regSet.tmpListBeg(); temp != nullptr; temp = codeGen->regSet.tmpListNxt(temp))
    {
        temp->tdAdjustTempOffs(delta);
    }

    lvaCachedGenericContextArgOffs += delta;

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
void Compiler::lvaAssignFrameOffsetsToPromotedStructs()
{
    for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum);

        if (lcl->IsPromotedField())
        {
            LclVarDsc* parentLcl = lvaGetDesc(lcl->GetPromotedFieldParentLclNum());

            if (parentLcl->IsIndependentPromoted())
            {
                // The stack offset for these field locals must have been calculated
                // by the normal frame offset assignment.
                continue;
            }

            noway_assert(lcl->lvOnFrame);

            lcl->SetStackOffset(parentLcl->GetStackOffset() + lcl->GetPromotedFieldOffset());
        }
    }
}

// Assign virtual stack offsets to the arguments, and implicit arguments
// (this ptr, return buffer, generics, and varargs).
void Compiler::lvaAssignVirtualFrameOffsetsToArgs()
{
    noway_assert(codeGen->intRegState.rsCalleeRegArgCount <= MAX_REG_ARG);
#ifndef OSX_ARM64_ABI
    noway_assert(compArgSize >= codeGen->intRegState.rsCalleeRegArgCount * REGSIZE_BYTES);
#endif

    // Assign stack offsets to arguments (in reverse order of passing).
    //
    // This means that if we pass arguments left->right, we start at
    // the end of the list and work backwards, for right->left we start
    // with the first argument and move forward.
    //
    // This is all relative to our Virtual '0'

    int argOffs = 0;

#ifdef TARGET_X86
    if (info.compCallConv == CorInfoCallConvExtension::Managed)
    {
        argOffs = compArgSize;

        // Update the argOffs to reflect arguments that are passed in registers.
        argOffs -= codeGen->intRegState.rsCalleeRegArgCount * REGSIZE_BYTES;
    }
#endif

    unsigned                lclNum    = 0;
    CORINFO_ARG_LIST_HANDLE argLst    = info.compMethodInfo->args.args;
    unsigned                argSigLen = info.compMethodInfo->args.numArgs;
#ifdef UNIX_AMD64_ABI
    int callerArgOffset = 0;
#endif

    if (!info.compIsStatic)
    {
        noway_assert(lclNum == info.compThisArg);

#ifndef TARGET_X86
        argOffs =
            lvaAssignParamVirtualFrameOffset(lclNum, REGSIZE_BYTES, argOffs UNIX_AMD64_ABI_ONLY_ARG(&callerArgOffset));
#endif

        lclNum++;
    }

#if defined(TARGET_WINDOWS) && !defined(TARGET_ARM)
    // In the native instance method calling convention on Windows,
    // the this parameter comes before the hidden return buffer parameter.
    if (callConvIsInstanceMethodCallConv(info.compCallConv))
    {
#ifdef TARGET_X86
        if (!lvaGetDesc(lclNum)->IsRegParam())
#endif
        {
            argOffs = lvaAssignParamVirtualFrameOffset(lclNum, REGSIZE_BYTES, argOffs);
        }

        lclNum++;
        argLst = info.compCompHnd->getArgNext(argLst);
        argSigLen--;
    }
#endif

    if (info.compRetBuffArg != BAD_VAR_NUM)
    {
        noway_assert(lclNum == info.compRetBuffArg);

        argOffs = lvaAssignParamVirtualFrameOffset(lclNum++, REGSIZE_BYTES,
                                                   argOffs UNIX_AMD64_ABI_ONLY_ARG(&callerArgOffset));
    }

#ifndef TARGET_X86
    if (info.compMethodInfo->args.hasTypeArg())
    {
        noway_assert(lclNum == info.compTypeCtxtArg);

        argOffs = lvaAssignParamVirtualFrameOffset(lclNum++, REGSIZE_BYTES,
                                                   argOffs UNIX_AMD64_ABI_ONLY_ARG(&callerArgOffset));
    }

    if (info.compIsVarArgs)
    {
        argOffs = lvaAssignParamVirtualFrameOffset(lclNum++, REGSIZE_BYTES,
                                                   argOffs UNIX_AMD64_ABI_ONLY_ARG(&callerArgOffset));
    }
#endif // !TARGET_X86

#ifdef TARGET_ARM
    // struct_n { int; int; ... n times };
    //
    // Consider signature:
    //
    // Foo (float a,double b,float c,double d,float e,double f,float g,double h,
    //      float i,double j,float k,double l,struct_3 m) { }
    //
    // Basically the signature is: (all float regs full, 1 double, struct_3);
    //
    // The double argument occurs before pre spill in the argument iteration and
    // computes an argOffset of 0. struct_3 offset becomes 8. This is wrong.
    // Because struct_3 is prespilled and double occurs after prespill.
    // The correct offsets are double = 16 (aligned stk), struct_3 = 0..12,
    // Offset 12 will be skipped for double alignment of double.
    //
    // Another example is (struct_2, all float regs full, double, struct_2);
    // Here, notice the order is similarly messed up because of 2 pre-spilled
    // struct_2.
    //
    // Succinctly,
    // ARG_INDEX(i) > ARG_INDEX(j) DOES NOT IMPLY |ARG_OFFSET(i)| > |ARG_OFFSET(j)|
    //
    // Therefore, we'll do a two pass offset calculation, one that considers pre-spill
    // and the next, stack args.

    // Take care of pre spill registers first.
    regMaskTP preSpillMask = codeGen->regSet.rsMaskPreSpillRegs(false);
    regMaskTP tempMask     = RBM_NONE;

    for (unsigned i = 0; i < argSigLen; ++i, argLst = info.compCompHnd->getArgNext(argLst))
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum + i);

        if (lcl->IsPreSpilledRegParam(preSpillMask))
        {
            unsigned argSize = lvaGetParamAllocSize(argLst, &info.compMethodInfo->args);
            argOffs          = lvaAssignParamVirtualFrameOffset(lclNum + i, argSize, argOffs);

            // Early out if we can. If size is 8 and base reg is 2, then the mask is 0x1100
            tempMask |= (((1 << (roundUp(argSize, REGSIZE_BYTES) / REGSIZE_BYTES))) - 1) << lcl->GetArgReg();

            if (tempMask == preSpillMask)
            {
                // We won't encounter more pre-spilled registers, so don't bother iterating further.
                break;
            }
        }
    }

    // Take care of non pre-spilled stack arguments.
    argLst = info.compMethodInfo->args.args;

    for (unsigned i = 0; i < argSigLen; ++i, argLst = info.compCompHnd->getArgNext(argLst))
    {
        if (!lvaGetDesc(lclNum + i)->IsPreSpilledRegParam(preSpillMask))
        {
            unsigned argSize = lvaGetParamAllocSize(argLst, &info.compMethodInfo->args);
            argOffs          = lvaAssignParamVirtualFrameOffset(lclNum + i, argSize, argOffs);
        }
    }
#else // !TARGET_ARM
    for (unsigned i = 0; i < argSigLen; i++, argLst = info.compCompHnd->getArgNext(argLst))
    {
        unsigned argSize = lvaGetParamAllocSize(argLst, &info.compMethodInfo->args);

#ifndef OSX_ARM64_ABI
        assert(argSize % REGSIZE_BYTES == 0);
#endif

        argOffs =
            lvaAssignParamVirtualFrameOffset(lclNum++, argSize, argOffs UNIX_AMD64_ABI_ONLY_ARG(&callerArgOffset));
    }

#ifdef TARGET_X86
    if (info.compMethodInfo->args.hasTypeArg())
    {
        noway_assert(lclNum == info.compTypeCtxtArg);

        argOffs = lvaAssignParamVirtualFrameOffset(lclNum++, REGSIZE_BYTES, argOffs);
    }

    if (info.compIsVarArgs)
    {
        argOffs = lvaAssignParamVirtualFrameOffset(lclNum++, REGSIZE_BYTES, argOffs);
    }

#endif // TARGET_X86
#endif // !TARGET_ARM
}

#ifdef UNIX_AMD64_ABI

int Compiler::lvaAssignVirtualFrameOffsetToArg(LclVarDsc* varDsc, unsigned argSize, int argOffs, int* callerArgOffset)
{
    if (varDsc->IsRegParam())
    {
        // Argument is passed in a register, don't count it, when updating the current offset on the stack.
        // The offset for args needs to be set only for the stack homed arguments for System V.

        varDsc->SetStackOffset(varDsc->lvOnFrame ? argOffs : 0);

        return argOffs;
    }

    // On System V platforms, if the RA decides to home a register passed arg on the stack, it creates a stack
    // location on the callee stack (like any other local var.) In such a case, the register passed, stack homed
    // arguments are accessed using negative offsets and the stack passed arguments are accessed using positive
    // offset (from the caller's stack.)
    // For System V platforms if there is no frame pointer the caller stack parameter offset should include the
    // callee allocated space. If frame register is used, the callee allocated space should not be included for
    // accessing the caller stack parameters. The last two requirements are met in lvaFixVirtualFrameOffsets
    // method, which fixes the offsets, based on frame pointer existence, existence of alloca instructions, ret
    // address pushed, ets.

    varDsc->SetStackOffset(*callerArgOffset);
    *callerArgOffset += static_cast<int>(roundUp(argSize, REGSIZE_BYTES));

    return argOffs + argSize;
}

#elif defined(WINDOWS_AMD64_ABI)

int Compiler::lvaAssignVirtualFrameOffsetToArg(LclVarDsc* lcl, unsigned argSize, int argOffs)
{
    assert(argSize == REGSIZE_BYTES);
    assert((argOffs % REGSIZE_BYTES) == 0);

    lcl->SetStackOffset(argOffs);
    argOffs += REGSIZE_BYTES;

    return argOffs;
}

#elif defined(TARGET_ARM64)

int Compiler::lvaAssignVirtualFrameOffsetToArg(LclVarDsc* lcl, unsigned argSize, int argOffs)
{
    if (lcl->IsRegParam())
    {
#if TARGET_WINDOWS
        // Register arguments on ARM64 only take stack space when they have a frame home.
        // Unless on Windows and in a vararg method where we can have a struct param that
        // is split between the last integer param register and a stack slot.
        if (info.compIsVarArgs && lcl->TypeIs(TYP_STRUCT) && (lcl->GetOtherArgReg() >= MAX_REG_ARG) &&
            (lcl->GetOtherArgReg() != REG_NA))
        {
            lcl->SetStackOffset(lcl->GetStackOffset() + REGSIZE_BYTES);
            argOffs += REGSIZE_BYTES;
        }
#endif // TARGET_WINDOWS

        return argOffs;
    }

#ifdef OSX_ARM64_ABI
    unsigned align =
        lvaGetParamAlignment(lcl->GetType(), lcl->lvIsHfa() && (lcl->GetLayout()->GetHfaElementType() == TYP_FLOAT));

    argOffs = roundUp(argOffs, align);
#else
    unsigned align = REGSIZE_BYTES;
#endif

    assert((argSize % align) == 0);
    assert((argOffs % align) == 0);

    lcl->SetStackOffset(argOffs);

    return argOffs + argSize;
}

#elif defined(TARGET_X86)

int Compiler::lvaAssignVirtualFrameOffsetToArg(LclVarDsc* lcl, unsigned argSize, int argOffs)
{
    assert((argSize % REGSIZE_BYTES) == 0);
    assert((argOffs % REGSIZE_BYTES) == 0);

    if (lcl->IsRegParam())
    {
        assert(argSize == REGSIZE_BYTES);

        return argOffs;
    }

    if (info.compCallConv == CorInfoCallConvExtension::Managed)
    {
        argOffs -= argSize;
    }

    lcl->SetStackOffset(argOffs);

    if (info.compCallConv != CorInfoCallConvExtension::Managed)
    {
        argOffs += argSize;
    }

    return argOffs;
}

#elif defined(TARGET_ARM)

int Compiler::lvaAssignVirtualFrameOffsetToArg(LclVarDsc* lcl, unsigned argSize, int argOffs)
{
    bool align8Byte = lcl->TypeIs(TYP_DOUBLE, TYP_LONG) || (lcl->TypeIs(TYP_STRUCT) && lcl->lvStructDoubleAlign);

    if (lcl->IsRegParam())
    {
        // On ARM we spill the registers in codeGen->regSet.rsMaskPreSpillRegArg
        // in the prolog, so we have to assign them frame offsets.

        regMaskTP regMask = genRegMask(lcl->GetArgReg());

        if ((codeGen->regSet.rsMaskPreSpillRegArg & regMask) == 0)
        {
            return argOffs;
        }

        // Signature: void foo(struct_8, int, struct_4)
        // ------- CALLER SP -------
        // r3 struct_4
        // r2 int - not prespilled, but added for alignment. argOffs should skip this.
        // r1 struct_8
        // r0 struct_8
        // -------------------------
        // If we added alignment we need to fix argOffs for all registers above alignment.

        if (codeGen->regSet.rsMaskPreSpillAlign != RBM_NONE)
        {
            assert(genCountBits(codeGen->regSet.rsMaskPreSpillAlign) == 1);

            // Is register beyond the alignment pos?
            if (regMask > codeGen->regSet.rsMaskPreSpillAlign)
            {
                // Increment argOffs just once for the _first_ register after alignment pos
                // in the prespill mask.
                if (!BitsBetween(codeGen->regSet.rsMaskPreSpillRegArg, regMask, codeGen->regSet.rsMaskPreSpillAlign))
                {
                    argOffs += TARGET_POINTER_SIZE;
                }
            }
        }

        if (align8Byte)
        {
            // Let's assign offsets to arg1, a double in r2. argOffs has to be 4 not 8.
            //
            // ------- CALLER SP -------
            // r3
            // r2 double   -- argOffs = 4, but it doesn't need to be skipped, because there is no skipping.
            // r1 VACookie -- argOffs = 0
            // -------------------------
            //
            // Consider argOffs as if it accounts for number of prespilled registers before the current
            // register. In the above example, for r2, it is r1 that is prespilled, but since r1 is
            // accounted for by argOffs being 4, there should have been no skipping. Instead, if we didn't
            // assign r1 to any variable, then argOffs would still be 0 which implies it is not accounting
            // for r1, equivalently r1 is skipped.
            //
            // If prevRegsSize is unaccounted for by a corresponding argOffs, we must have skipped a register.

            int prevRegsSize = genCountBits(codeGen->regSet.rsMaskPreSpillRegArg & (regMask - 1)) * REGSIZE_BYTES;

            if (argOffs < prevRegsSize)
            {
                // We must align up the argOffset to a multiple of 8 to account for skipped registers.
                argOffs = roundUp(static_cast<unsigned>(argOffs), 2 * REGSIZE_BYTES);
            }

            // We should've skipped only a single register.
            assert(argOffs == prevRegsSize);
        }

        lcl->SetStackOffset(argOffs);

        return argOffs + argSize;
    }

    // Dev11 Bug 42817: incorrect codegen for DrawFlatCheckBox causes A/V in WinForms
    //
    // Here we have method with a signature (int a1, struct a2, struct a3, int a4, int a5).
    // Struct parameter 'a2' is 16-bytes with no alignment requirements;
    //  it uses r1,r2,r3 and [OutArg+0] when passed.
    // Struct parameter 'a3' is 16-bytes that is required to be double aligned;
    //  the caller skips [OutArg+4] and starts the argument at [OutArg+8].
    // Thus the caller generates the correct code to pass the arguments.
    // When generating code to receive the arguments we set codeGen->regSet.rsMaskPreSpillRegArg to [r1,r2,r3]
    //  and spill these three registers as the first instruction in the prolog.
    // Then when we layout the arguments' stack offsets we have an argOffs 0 which
    //  points at the location that we spilled r1 into the stack.  For this first
    //  struct we take the lvIsRegArg path above with "codeGen->regSet.rsMaskPreSpillRegArg &" matching.
    // Next when we calculate the argOffs for the second 16-byte struct we have an argOffs
    //  of 16, which appears to be aligned properly so we don't skip a stack slot.
    //
    // To fix this we must recover the actual OutArg offset by subtracting off the
    //  sizeof of the PreSpill register args.
    // Then we align this offset to a multiple of 8 and add back the sizeof
    //  of the PreSpill register args.
    //
    // Dev11 Bug 71767: failure of assert(sizeofPreSpillRegArgs <= argOffs)
    //
    // We have a method with 'this' passed in r0, RetBuf arg in r1, VarArgs cookie
    // in r2. The first user arg is a 144 byte struct with double alignment required,
    // r3 is skipped, and the struct is passed on the stack. However, 'r3' is added
    // to the codeGen->regSet.rsMaskPreSpillRegArg mask by the VarArgs cookie code, since we need to
    // home all the potential varargs arguments in registers, even if we don't have
    // signature type information for the variadic arguments. However, due to alignment,
    // we have skipped a register that doesn't have a corresponding symbol. Make up
    // for that by increasing argOffs here.

    int sizeofPreSpillRegArgs = genCountBits(codeGen->regSet.rsMaskPreSpillRegs(true)) * REGSIZE_BYTES;

    if (argOffs < sizeofPreSpillRegArgs)
    {
        // This can only happen if we skipped the last register spot because current stk arg
        // is a struct requiring alignment or a pre-spill alignment was required because the
        // first reg arg needed alignment.
        //
        // Example 1: First Stk Argument requiring alignment in vararg case (same as above comment.)
        //            Signature (int a0, int a1, int a2, struct {long} a3, ...)
        //
        // stk arg    a3             --> argOffs here will be 12 (r0-r2) but pre-spill will be 16.
        // ---- Caller SP ----
        // r3                        --> Stack slot is skipped in this case.
        // r2    int  a2
        // r1    int  a1
        // r0    int  a0
        //
        // Example 2: First Reg Argument requiring alignment in no-vararg case.
        //            Signature (struct {long} a0, struct {int} a1, int a2, int a3)
        //
        // stk arg                  --> argOffs here will be 12 {r0-r2} but pre-spill will be 16.
        // ---- Caller SP ----
        // r3    int             a2 --> pushed (not pre-spilled) for alignment of a0 by lvaInitUserParams.
        // r2    struct { int }  a1
        // r0-r1 struct { long } a0
        CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef PROFILING_SUPPORTED
        // On Arm under profiler, r0-r3 are always prespilled on stack.
        // It is possible to have methods that accept only HFAs as parameters e.g. Signature(struct hfa1, struct
        // hfa2), in which case hfa1 and hfa2 will be en-registered in co-processor registers and will have an
        // argument offset less than size of preSpill.
        //
        // For this reason the following conditions are asserted when not under profiler.
        if (!compIsProfilerHookNeeded())
#endif
        {
            noway_assert(((info.compIsVarArgs || opts.compUseSoftFP) && align8Byte) ||
                         // Did first reg arg require alignment?
                         (codeGen->regSet.rsMaskPreSpillAlign & genRegMask(REG_ARG_LAST)));

            // At most one register of alignment.
            noway_assert(sizeofPreSpillRegArgs <= argOffs + REGSIZE_BYTES);
        }

        argOffs = sizeofPreSpillRegArgs;
    }

    noway_assert(argOffs >= sizeofPreSpillRegArgs);

    int argOffsWithoutPreSpillRegArgs = argOffs - sizeofPreSpillRegArgs;

    if (align8Byte)
    {
        argOffs =
            roundUp(static_cast<unsigned>(argOffsWithoutPreSpillRegArgs), 2 * REGSIZE_BYTES) + sizeofPreSpillRegArgs;
    }

    assert((argSize % REGSIZE_BYTES) == 0);
    assert((argOffs % REGSIZE_BYTES) == 0);

    lcl->SetStackOffset(argOffs);

    return argOffs + argSize;
}

#endif

int Compiler::lvaAssignParamVirtualFrameOffset(unsigned lclNum,
                                               unsigned argSize,
                                               int argOffs UNIX_AMD64_ABI_ONLY_ARG(int* callerArgOffset))
{
    assert(lclNum < info.compArgsCount);
    assert(argSize != 0);

    LclVarDsc* lcl = lvaGetDesc(lclNum);

    assert(lcl->IsParam() && !lcl->IsPromotedField());

    argOffs = lvaAssignVirtualFrameOffsetToArg(lcl, argSize, argOffs UNIX_AMD64_ABI_ONLY_ARG(callerArgOffset));

    if (lcl->IsPromoted())
    {
        for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); i++)
        {
            LclVarDsc* fieldLcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(i));
            fieldLcl->SetStackOffset(lcl->GetStackOffset() + fieldLcl->GetPromotedFieldOffset());
        }
    }

    return argOffs;
}

// Assign virtual stack offsets to locals, temps, and anything else.
// These will all be negative offsets (stack grows down) relative to the virtual '0' address.
void Compiler::lvaAssignVirtualFrameOffsetsToLocals()
{
#ifdef TARGET_ARM64
    // Decide where to save FP and LR registers. We store FP/LR registers at the bottom of the frame if there is
    // a frame pointer used (so we get positive offsets from the frame pointer to access locals), but not if we
    // need a GS cookie AND localloc is used, since we need the GS cookie to protect the saved return value,
    // and also the saved frame pointer. See CodeGen::genPushCalleeSavedRegisters() for more details about the
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
#endif // TARGET_ARM64

    int stkOffs              = 0;
    int originalFrameStkOffs = 0;
    int originalFrameSize    = 0;

#ifdef TARGET_XARCH
    // On x86/amd64, the return address has already been pushed by the call instruction in the caller.
    stkOffs -= REGSIZE_BYTES; // return address;

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
#endif // TARGET_XARCH

#ifdef TARGET_ARM
    int  preSpillSize    = genCountBits(codeGen->regSet.rsMaskPreSpillRegs(true)) * REGSIZE_BYTES;
    bool mustDoubleAlign = true;
#elif defined(TARGET_X86)
    int  preSpillSize    = 0;
    bool mustDoubleAlign = false;
#if DOUBLE_ALIGN
    if (genDoubleAlign())
    {
        mustDoubleAlign = true;
    }
#endif
#endif

#ifdef TARGET_ARM64
    // If the frame pointer is used, then we'll save FP/LR at the bottom of the stack.
    // Otherwise, we won't store FP, and we'll store LR at the top, with the other callee-save
    // registers (if any).

    int initialStkOffs = 0;

    if (info.compIsVarArgs)
    {
        // For varargs we always save all of the integer register arguments
        // so that they are contiguous with the incoming stack arguments.
        initialStkOffs = MAX_REG_ARG * REGSIZE_BYTES;
        stkOffs -= initialStkOffs;
    }

    if (codeGen->IsSaveFpLrWithAllCalleeSavedRegisters() ||
        !isFramePointerUsed()) // Note that currently we always have a frame pointer
    {
        stkOffs -= compCalleeRegsPushed * REGSIZE_BYTES;
    }
    else
    {
        // Subtract off FP and LR.
        assert(compCalleeRegsPushed >= 2);

        stkOffs -= (compCalleeRegsPushed - 2) * REGSIZE_BYTES;
    }
#else // !TARGET_ARM64
#ifdef TARGET_ARM
    // On ARM32 LR is part of the pushed registers and is always stored at the top.
    if (lvaRetAddrVar != BAD_VAR_NUM)
    {
        lvaGetDesc(lvaRetAddrVar)->SetStackOffset(stkOffs - REGSIZE_BYTES);
    }
#endif

    stkOffs -= compCalleeRegsPushed * REGSIZE_BYTES;
#endif // !TARGET_ARM64

    compLclFrameSize = 0;

#ifdef TARGET_AMD64
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
    unsigned calleeFPRegsSavedSize = genCountBits(compCalleeFPRegsSavedMask) * XMM_REGSIZE_BYTES;

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
#endif // TARGET_AMD64

#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARMARCH)
    if (lvaPSPSym != BAD_VAR_NUM)
    {
        // On ARM/ARM64, if we need a PSPSym, allocate it first, before anything else, including
        // padding (so we can avoid computing the same padding in the funclet frame). Note that
        // there is no special padding requirement for the PSPSym.

        noway_assert(codeGen->isFramePointerUsed());

        stkOffs = lvaAllocLocalAndSetVirtualOffset(lvaPSPSym, REGSIZE_BYTES, stkOffs);
    }
#endif // FEATURE_EH_FUNCLETS && defined(TARGET_ARMARCH)

#ifndef TARGET_64BIT
    if (mustDoubleAlign)
    {
#ifdef TARGET_ARM
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

    if (lvaMonAcquired != BAD_VAR_NUM)
    {
        // This var must go first, in what is called the 'frame header' for EnC so that it is
        // preserved when remapping occurs.  See vm\eetwain.cpp for detailed comment specifying frame
        // layout requirements for EnC to work.
        stkOffs = lvaAllocLocalAndSetVirtualOffset(lvaMonAcquired, lvaLclSize(lvaMonAcquired), stkOffs);
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
            lvaCachedGenericContextArgOffs = originalFrameStkOffs + ppInfo->GenericContextArgOffset();
        }
        else
        {
            // For CORINFO_CALLCONV_PARAMTYPE (if needed)
            lvaIncrementFrameSize(REGSIZE_BYTES);
            stkOffs -= REGSIZE_BYTES;

            lvaCachedGenericContextArgOffs = stkOffs;
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
                lvaCachedGenericContextArgOffs = originalFrameStkOffs + ppInfo->KeptAliveThisOffset();
                canUseExistingSlot             = true;
            }
        }

        if (!canUseExistingSlot)
        {
            // When "this" is also used as generic context arg.
            lvaIncrementFrameSize(REGSIZE_BYTES);
            stkOffs -= REGSIZE_BYTES;

            lvaCachedGenericContextArgOffs = stkOffs;
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

        stkOffs = lvaAllocLocalAndSetVirtualOffset(lvaShadowSPslotsVar, lvaLclSize(lvaShadowSPslotsVar), stkOffs);
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

            if (
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

            if (lclNum == lvaMonAcquired)
            {
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

            if (lcl->IsParam())
            {
#ifdef WINDOWS_AMD64_ABI
                // On Windows AMD64 we can use the caller-reserved stack area that is already setup
                assert(lcl->GetStackOffset() != BAD_STK_OFFS);

                continue;
#else
                if (!lcl->IsRegParam())
                {
                    continue;
                }

#ifdef TARGET_ARM64
                if (info.compIsVarArgs && (lcl->GetArgReg() != theFixedRetBuffArgNum()))
                {
                    // Stack offset to varargs (parameters) should point to home area which will be preallocated.
                    unsigned regNum = genMapIntRegNumToRegArgNum(lcl->GetArgReg());
                    lcl->SetStackOffset(-initialStkOffs + regNum * REGSIZE_BYTES);

                    continue;
                }
#elif defined(TARGET_ARM)
                // On ARM we spill the registers in codeGen->regSet.rsMaskPreSpillRegArg
                // in the prolog, thus they don't need stack frame space.
                if ((codeGen->regSet.rsMaskPreSpillRegs(false) & genRegMask(lcl->GetArgReg())) != 0)
                {
                    assert(lcl->GetStackOffset() != BAD_STK_OFFS);

                    continue;
                }
#endif
#endif // !WINDOWS_AMD64_ABI
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
                noway_assert((compLclFrameSize % REGSIZE_BYTES) == 0);

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

            stkOffs = lvaAllocLocalAndSetVirtualOffset(lclNum, lvaLclSize(lclNum), stkOffs);

#ifdef TARGET_ARMARCH
            if (lcl->IsPromoted() && lcl->IsRegParam())
            {
                for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); i++)
                {
                    LclVarDsc* fieldLcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(i));
                    fieldLcl->SetStackOffset(lcl->GetStackOffset() + fieldLcl->GetPromotedFieldOffset());
                }
            }
#endif // TARGET_ARMARCH
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

        stkOffs =
            lvaAllocLocalAndSetVirtualOffset(lvaInlinedPInvokeFrameVar, lvaLclSize(lvaInlinedPInvokeFrameVar), stkOffs);
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
    if (!codeGen->IsSaveFpLrWithAllCalleeSavedRegisters() && isFramePointerUsed())
    {
        // Create space for saving FP and LR.
        stkOffs -= 2 * REGSIZE_BYTES;
    }
#endif // TARGET_ARM64

#if FEATURE_FIXED_OUT_ARGS
    if (lvaOutgoingArgSpaceSize > 0)
    {
        noway_assert(lvaOutgoingArgSpaceSize % REGSIZE_BYTES == 0);
#ifdef WINDOWS_AMD64_ABI
        noway_assert(lvaOutgoingArgSpaceSize >= 4 * REGSIZE_BYTES);
#endif

        // Give it a value so we can avoid asserts in CHK builds.
        // Since this will always use an SP relative offset of zero
        // at the end of lvaFixVirtualFrameOffsets, it will be set to absolute '0'

        stkOffs = lvaAllocLocalAndSetVirtualOffset(lvaOutgoingArgSpaceVar, lvaOutgoingArgSpaceSize, stkOffs);
    }
#endif // FEATURE_FIXED_OUT_ARGS

    // compLclFrameSize equals our negated virtual stack offset minus the pushed registers and return address
    // and the pushed frame pointer register which for some strange reason isn't part of 'compCalleeRegsPushed'.
    int pushedCount = compCalleeRegsPushed;

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

    noway_assert(compLclFrameSize + originalFrameSize ==
                 static_cast<unsigned>(-(stkOffs + pushedCount * REGSIZE_BYTES)));
}

int Compiler::lvaAllocLocalAndSetVirtualOffset(unsigned lclNum, unsigned size, int stkOffs)
{
    noway_assert(lclNum != BAD_VAR_NUM);
    assert(size != 0);
    assert(stkOffs <= 0);

    LclVarDsc* lcl = lvaGetDesc(lclNum);

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
    unsigned regsPushed = compCalleeRegsPushed + (codeGen->isFramePointerUsed() ? 1 : 0);
    return (regsPushed % (16 / REGSIZE_BYTES)) == 0;
}
#endif // TARGET_AMD64

// After allocating everything on the frame, reserve any
// extra space needed to keep the frame aligned.
void Compiler::lvaAlignFrame()
{
#if defined(TARGET_AMD64)
    // Leaf frames do not need full alignment, but the unwind info is smaller if we
    // are at least 8 byte aligned (and we assert as much)
    if ((compLclFrameSize % 8) != 0)
    {
        lvaIncrementFrameSize(8 - (compLclFrameSize % 8));
    }

    assert((compLclFrameSize % 8) == 0);

    // Ensure that the stack is always 16-byte aligned by grabbing an unused QWORD
    // if needed, but off by 8 because of the return value.
    // And don't forget that compCalleeRegsPused does *not* include RBP if we are
    // using it as the frame pointer.
    bool regPushedCountAligned = lvaIsCalleeSavedIntRegCountEven();
    bool lclFrameSizeAligned   = (compLclFrameSize % 16) == 0;

    // If this isn't the final frame layout, assume we have to push an extra QWORD
    // Just so the offsets are true upper limits.
    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef UNIX_AMD64_ABI
    // The compNeedToAlignFrame flag  is indicating if there is a need to align the frame.
    // On AMD64-Windows, if there are calls, 4 slots for the outgoing ars are allocated, except for
    // FastTailCall. This slots makes the frame size non-zero, so alignment logic will be called.
    // On AMD64-Unix, there are no such slots. There is a possibility to have calls in the method with frame size of 0.
    // The frame alignment logic won't kick in. This flags takes care of the AMD64-Unix case by remembering that there
    // are calls and making sure the frame alignment logic is executed.
    bool stackNeedsAlignment = (compLclFrameSize != 0 || opts.compNeedToAlignFrame);
#else
    bool stackNeedsAlignment = compLclFrameSize != 0;
#endif

    if ((!codeGen->isFramePointerUsed() && (lvaDoneFrameLayout != FINAL_FRAME_LAYOUT)) ||
        (stackNeedsAlignment && (regPushedCountAligned == lclFrameSizeAligned)))
    {
        lvaIncrementFrameSize(REGSIZE_BYTES);
    }

#elif defined(TARGET_ARM64)
    // The stack on ARM64 must be 16 byte aligned.

    // First, align up to 8.
    if ((compLclFrameSize % 8) != 0)
    {
        lvaIncrementFrameSize(8 - (compLclFrameSize % 8));
    }
    else if (lvaDoneFrameLayout != FINAL_FRAME_LAYOUT)
    {
        // If we are not doing final layout, we don't know the exact value of compLclFrameSize
        // and thus do not know how much we will need to add in order to be aligned.
        // We add 8 so compLclFrameSize is still a multiple of 8.
        lvaIncrementFrameSize(8);
    }
    assert((compLclFrameSize % 8) == 0);

    // Ensure that the stack is always 16-byte aligned by grabbing an unused QWORD
    // if needed.
    bool regPushedCountAligned = (compCalleeRegsPushed % (16 / REGSIZE_BYTES)) == 0;
    bool lclFrameSizeAligned   = (compLclFrameSize % 16) == 0;

    // If this isn't the final frame layout, assume we have to push an extra QWORD
    // Just so the offsets are true upper limits.
    if ((lvaDoneFrameLayout != FINAL_FRAME_LAYOUT) || (regPushedCountAligned != lclFrameSizeAligned))
    {
        lvaIncrementFrameSize(REGSIZE_BYTES);
    }

#elif defined(TARGET_ARM)

    // Ensure that stack offsets will be double-aligned by grabbing an unused DWORD if needed.
    bool lclFrameSizeAligned   = (compLclFrameSize % sizeof(double)) == 0;
    bool regPushedCountAligned = ((compCalleeRegsPushed + genCountBits(codeGen->regSet.rsMaskPreSpillRegs(true))) %
                                  (sizeof(double) / TARGET_POINTER_SIZE)) == 0;

    if (regPushedCountAligned != lclFrameSizeAligned)
    {
        lvaIncrementFrameSize(TARGET_POINTER_SIZE);
    }

#elif defined(TARGET_X86)

#if DOUBLE_ALIGN
    if (genDoubleAlign())
    {
        // Double Frame Alignment for x86 is handled in Compiler::lvaAssignVirtualFrameOffsetsToLocals()

        if (compLclFrameSize == 0)
        {
            // This can only happen with JitStress=1 or JitDoubleAlign=2
            lvaIncrementFrameSize(TARGET_POINTER_SIZE);
        }
    }
#endif

    if (STACK_ALIGN > REGSIZE_BYTES)
    {
        // Align the stack with STACK_ALIGN value.
        int  adjustFrameSize = compLclFrameSize;
#if defined(UNIX_X86_ABI)
        bool isEbpPushed     = codeGen->isFramePointerUsed();
#if DOUBLE_ALIGN
        isEbpPushed |= genDoubleAlign();
#endif
        // we need to consider spilled register(s) plus return address and/or EBP
        int adjustCount = compCalleeRegsPushed + 1 + (isEbpPushed ? 1 : 0);
        adjustFrameSize += (adjustCount * REGSIZE_BYTES) % STACK_ALIGN;
#endif
        if ((adjustFrameSize % STACK_ALIGN) != 0)
        {
            lvaIncrementFrameSize(STACK_ALIGN - (adjustFrameSize % STACK_ALIGN));
        }
    }

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
    int      preSpillSize  = genCountBits(codeGen->regSet.rsMaskPreSpillRegs(true)) * REGSIZE_BYTES;
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
            noway_assert(compLclFrameSize % REGSIZE_BYTES == 0);

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

#ifdef TARGET_ARM
    // Only required for the ARM platform that we have an accurate estimate for the spillTempSize
    noway_assert(spillTempSize <= lvaGetMaxSpillTempSize());
#endif

    return stkOffs;
}

// Determine the stack frame offset of the given local,
// and how to generate an address to that local's stack location.
int Compiler::lvaFrameAddress(int varNum,
#ifdef TARGET_ARM
                              bool       mustBeFPBased,
                              int        addrModeOffset,
                              bool       isFloatUsage,
                              regNumber* baseReg
#else
                              bool* pFPbased
#endif
                              )
{
    assert(lvaDoneFrameLayout == FINAL_FRAME_LAYOUT);

    int  varOffset;
    bool fpBased;

    if (varNum >= 0)
    {
        LclVarDsc* lcl = lvaGetDesc(static_cast<unsigned>(varNum));

        bool isPrespilledArg = false;
#if defined(TARGET_ARM) && defined(PROFILING_SUPPORTED)
        isPrespilledArg = lcl->IsParam() && compIsProfilerHookNeeded() &&
                          lcl->IsPreSpilledRegParam(codeGen->regSet.rsMaskPreSpillRegs(false));
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

        fpBased = lcl->lvFramePointerBased;

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
            assert(fpBased == (isFramePointerUsed() || (genDoubleAlign() && lcl->IsParam() && !lcl->IsRegParam())));
#else
            assert(fpBased == isFramePointerUsed());
#endif
#endif
        }

        varOffset = lcl->GetStackOffset();
    }
    else // Its a spill-temp
    {
        fpBased = isFramePointerUsed();

        TempDsc* tmpDsc = codeGen->regSet.tmpFindNum(varNum);

        // The temp might be in use, since this might be during code generation.
        if (tmpDsc == nullptr)
        {
            tmpDsc = codeGen->regSet.tmpFindNum(varNum, RegSet::TEMP_USAGE_USED);
            assert(tmpDsc != nullptr);
        }

        varOffset = tmpDsc->tdTempOffs();
    }

#ifndef TARGET_ARM
    *pFPbased = fpBased;
#else
    if (!fpBased)
    {
        *baseReg = REG_SPBASE;
    }
    else if (mustBeFPBased)
    {
        *baseReg = REG_FPBASE;
    }
    else
    {
        // Change the Frame Pointer (R11)-based addressing to the SP-based addressing when
        // possible because it generates smaller code on ARM. See frame picture above for
        // the math.

        // If it is the final frame layout phase, we don't have a choice, we should stick
        // to either FP based or SP based that we decided in the earlier phase. Because
        // we have already selected the instruction. MinOpts will always reserve R10, so
        // for MinOpts always use SP-based offsets, using R10 as necessary, for simplicity.

        int spVarOffset        = varOffset + codeGen->genSPtoFPdelta();
        int actualSPOffset     = spVarOffset + addrModeOffset;
        int actualFPOffset     = varOffset + addrModeOffset;
        int encodingLimitUpper = isFloatUsage ? 0x3FC : 0xFFF;
        int encodingLimitLower = isFloatUsage ? -0x3FC : -0xFF;

        // Use SP-based encoding. During encoding, we'll pick the best encoding for the actual offset we have.
        if (opts.MinOpts() || (actualSPOffset <= encodingLimitUpper))
        {
            *baseReg = compLocallocUsed ? REG_SAVED_LOCALLOC_SP : REG_SPBASE;

            varOffset = spVarOffset;
        }
        // Use Frame Pointer (R11)-based encoding.
        else if ((encodingLimitLower <= actualFPOffset) && (actualFPOffset <= encodingLimitUpper))
        {
            *baseReg = REG_FPBASE;
        }
        // Otherwise, use SP-based encoding. This is either (1) a small positive offset using a single movw,
        // (2) a large offset using movw/movt. In either case, we must have already reserved
        // the "reserved register", which will get used during encoding.
        else
        {
            *baseReg = compLocallocUsed ? REG_SAVED_LOCALLOC_SP : REG_SPBASE;

            varOffset = spVarOffset;
        }
    }
#endif

    return varOffset;
}

#ifdef TARGET_ARMARCH

// Function compRsvdRegCheck:
//  given a curState to use for calculating the total frame size
//  it will return true if the REG_OPT_RSVD should be reserved so
//  that it can be use to form large offsets when accessing stack
//  based LclVar including both incoming and out going argument areas.
//
//  The method advances the frame layout state to curState by calling
//  lvaFrameSize(curState).
//
bool Compiler::compRsvdRegCheck()
{
    // Always do the layout even if returning early. Callers might
    // depend on us to do the layout.
    unsigned frameSize = lvaFrameSize();

    JITDUMP("\ncompRsvdRegCheck - frame size = %u, compArgSize = %u\n", frameSize, compArgSize);

    if (opts.MinOpts())
    {
        // Have a recovery path in case we fail to reserve REG_OPT_RSVD and go
        // over the limit of SP and FP offset ranges due to large
        // temps.
        JITDUMP("Returning true (MinOpts)\n\n");
        return true;
    }

#ifdef TARGET_ARM64
    // TODO-ARM64-CQ: update this!
    JITDUMP("Returning true (ARM64)\n\n");
    return true; // just always assume we'll need it, for now
#else
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
    unsigned maxR11PositiveOffset = compArgSize + (2 * REGSIZE_BYTES) - 1;
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
    assert(compArgSize + frameSize > 0);
    unsigned maxSPPositiveOffset = compArgSize + frameSize - 1;

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
unsigned Compiler::lvaFrameSize()
{
    assert(lvaDoneFrameLayout < FINAL_FRAME_LAYOUT);

    // Layout the stack frame conservatively.
    // Assume all callee-saved registers are spilled to stack.

    compCalleeRegsPushed = CNT_CALLEE_SAVED;

    if (compFloatingPointUsed)
    {
        compCalleeRegsPushed += CNT_CALLEE_SAVED_FLOAT;
    }

    compCalleeRegsPushed++; // We always push LR, see genPushCalleeSavedRegisters.

    lvaAssignFrameOffsets(REGALLOC_FRAME_LAYOUT);
    DBEXEC(verbose, lvaTableDump());

    unsigned frameSize = compLclFrameSize;

    frameSize += CALLEE_SAVED_REG_MAXSZ;

    if (compFloatingPointUsed)
    {
        frameSize += CALLEE_SAVED_FLOAT_MAXSZ;
    }

    frameSize += REGSIZE_BYTES; // We always push LR, see genPushCalleeSavedRegisters.

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
// Return the Initial-SP-relative stack offset of a local/parameter.
int Compiler::lvaGetInitialSPRelativeOffset(unsigned lclNum)
{
    assert(lvaDoneFrameLayout == FINAL_FRAME_LAYOUT);

    LclVarDsc* lcl = lvaGetDesc(lclNum);
    assert(lcl->lvOnFrame);
    return lvaToInitialSPRelativeOffset(lcl->GetStackOffset(), lcl->lvFramePointerBased);
}

// Given a local variable offset, and whether that offset is frame-pointer based,
// return its offset from Initial-SP. This is used, for example, to figure out
// the offset of the frame pointer from Initial-SP.
int Compiler::lvaToInitialSPRelativeOffset(unsigned offset, bool isFpBased)
{
    assert(lvaDoneFrameLayout == FINAL_FRAME_LAYOUT);

    if (isFpBased)
    {
        // Currently, the frame starts by pushing ebp, ebp points to the saved ebp
        // (so we have ebp pointer chaining). Add the fixed-size frame size plus the
        // size of the callee-saved regs (not including ebp itself) to find Initial-SP.

        assert(codeGen->isFramePointerUsed());

        offset += codeGen->genSPtoFPdelta();
    }
    else
    {
        // The offset is correct already!
    }

    return offset;
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

#ifdef FEATURE_SIMD
// Mark locals used by SIMD intrinsics to prevent struct promotion.
void Compiler::lvaRecordSimdIntrinsicUse(GenTree* op)
{
    if (op->OperIs(GT_OBJ, GT_IND))
    {
        GenTree* addr = op->AsIndir()->GetAddr();

        if (addr->OperIs(GT_LCL_VAR_ADDR))
        {
            lvaRecordSimdIntrinsicUse(addr->AsLclVar());
        }
    }
    else if (op->OperIs(GT_LCL_VAR))
    {
        lvaRecordSimdIntrinsicUse(op->AsLclVar());
    }
}

void Compiler::lvaRecordSimdIntrinsicUse(GenTreeLclVar* lclVar)
{
    lvaRecordSimdIntrinsicUse(lclVar->GetLclNum());
}

void Compiler::lvaRecordSimdIntrinsicUse(unsigned lclNum)
{
    lvaGetDesc(lclNum)->lvUsedInSIMDIntrinsic = true;
}

void Compiler::lvaRecordSimdIntrinsicDef(GenTreeLclVar* lclVar, GenTreeHWIntrinsic* src)
{
    lvaRecordSimdIntrinsicDef(lclVar->GetLclNum(), src);
}

void Compiler::lvaRecordSimdIntrinsicDef(unsigned lclNum, GenTreeHWIntrinsic* src)
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

    lvaGetDesc(lclNum)->lvUsedInSIMDIntrinsic = true;
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
    int       offset;
    regNumber baseReg;

#ifdef TARGET_ARM
    offset = lvaFrameAddress(lclNum, compLocallocUsed, 0, /* isFloatUsage */ false, &baseReg);
#else
    bool EBPbased;
    offset  = lvaFrameAddress(lclNum, &EBPbased);
    baseReg = EBPbased ? REG_FPBASE : REG_SPBASE;
#endif

    printf("[%2s%1s%02XH]  ", getRegName(baseReg), (offset < 0 ? "-" : "+"), (offset < 0 ? -offset : offset));
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
            if (lvaOutgoingArgSpaceSize.HasFinalValue())
            {
                printf("<%u>", lvaOutgoingArgSpaceSize.GetValue());
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
            printf("<%2u>  ", lvaLclSize(lclNum));
        }
        else
        {
            printf("      ");
        }

        if ((lvaRefCountState == RCS_NORMAL) && (varDsc->GetRefCount() == 0))
        {
            printf("           ");
        }
        else if (varDsc->lvRegister != 0)
        {
            // It's always a register, and always in the same register.
            lvaDumpRegLocation(lclNum);
        }
        else if (varDsc->lvOnFrame == 0)
        {
            printf("registers  ");
        }
        else
        {
            // For RyuJIT backend, it might be in a register part of the time, but it will
            // definitely have a stack home location. Otherwise, it's always on the stack.
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
        printf(" [%2s%1s%02XH]\n", isFramePointerUsed() ? STR_FPBASE : STR_SPBASE, offset < 0 ? "-" : "+", abs(offset));
    }

    if (lvaDoneFrameLayout == FINAL_FRAME_LAYOUT)
    {
        printf(";\n");
        printf("; Lcl frame size = %d\n", compLclFrameSize);
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
