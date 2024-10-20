// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*****************************************************************************/
#ifndef CompMemKindMacro
#error Define CompMemKindMacro before including this file.
#endif

// This list of macro invocations should be used to define the CompMemKind enumeration,
// and the corresponding array of string names for these enum members.

// clang-format off
CompMemKindMacro(AssertionProp)
CompMemKindMacro(ASTNode)
CompMemKindMacro(InstDesc)
CompMemKindMacro(Importer)
CompMemKindMacro(BasicBlock)
CompMemKindMacro(CallInfo)
CompMemKindMacro(FlowList)
CompMemKindMacro(DominatorMemory)
CompMemKindMacro(LSRA)
CompMemKindMacro(LSRA_Interval)
CompMemKindMacro(LSRA_RefPosition)
CompMemKindMacro(Reachability)
CompMemKindMacro(SSA)
CompMemKindMacro(ValueNumber)
CompMemKindMacro(LvaTable)
CompMemKindMacro(LvaTracked)
CompMemKindMacro(LclVarDsc)
CompMemKindMacro(UnwindInfo)
CompMemKindMacro(hashBv)
CompMemKindMacro(bitset)
CompMemKindMacro(DataFlow)
CompMemKindMacro(SwitchDedup)
CompMemKindMacro(EHOpts)
CompMemKindMacro(LocalAddressVisitor)
CompMemKindMacro(FieldSeqStore)
CompMemKindMacro(ZeroOffsetFieldMap)
CompMemKindMacro(MemoryPhiArg)
CompMemKindMacro(CSE)
CompMemKindMacro(GC)
CompMemKindMacro(CorTailCallInfo)
CompMemKindMacro(Inlining)
CompMemKindMacro(ArrayStack)
CompMemKindMacro(DebugInfo)
CompMemKindMacro(DebugOnly)
CompMemKindMacro(Codegen)
CompMemKindMacro(LoopOpt)
CompMemKindMacro(LoopClone)
CompMemKindMacro(LoopUnroll)
CompMemKindMacro(LoopHoist)
CompMemKindMacro(GS)
CompMemKindMacro(ReorderBlocks)
CompMemKindMacro(RangeCheck)
CompMemKindMacro(CopyProp)
CompMemKindMacro(SideEffects)
CompMemKindMacro(ObjectAllocator)
CompMemKindMacro(VariableLiveRanges)
CompMemKindMacro(ClassLayout)
CompMemKindMacro(TailMergeThrows)
CompMemKindMacro(EarlyProp)
CompMemKindMacro(ZeroInit)
CompMemKindMacro(Pgo)
CompMemKindMacro(MorphAssertion)
CompMemKindMacro(SpillTemp)
CompMemKindMacro(ThrowHelperBlock)
//clang-format on

#undef CompMemKindMacro
