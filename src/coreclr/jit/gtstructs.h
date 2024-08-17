// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

// clang-format off

#ifndef GTSTRUCT_0
#error  Define GTSTRUCT_0 before including this file.
#endif

#ifndef GTSTRUCT_1
#error  Define GTSTRUCT_1 before including this file.
#endif

#ifndef GTSTRUCT_2
#error  Define GTSTRUCT_2 before including this file.
#endif

#ifndef GTSTRUCT_3
#error  Define GTSTRUCT_3 before including this file.
#endif

#ifndef GTSTRUCT_4
#error  Define GTSTRUCT_4 before including this file.
#endif

#ifndef GTSTRUCT_N
#error  Define GTSTRUCT_N before including this file.
#endif

#ifndef GTSTRUCT_2_SPECIAL
#error  Define GTSTRUCT_2_SPECIAL before including this file.
#endif

#ifndef GTSTRUCT_3_SPECIAL
#error  Define GTSTRUCT_3_SPECIAL before including this file.
#endif

//
//       Field name    , Allowed node enum(s)
//
// The "SPECIAL" variants indicate that some or all of the allowed opers exist elsewhere. This is
// used in the DEBUGGABLE_GENTREE implementation when determining which vtable pointer to use for
// a given oper. For example, IntConCommon (for the GenTreeIntConCommon type) allows opers
// for all its subtypes. The "SPECIAL" version is attached to the supertypes. "N" is always
// considered "special".

GTSTRUCT_0(UnOp        , GT_OP)
GTSTRUCT_0(Op          , GT_OP)

#ifndef FEATURE_EH_FUNCLETS
GTSTRUCT_1(EndLFin     , GT_END_LFIN)
#endif

GTSTRUCT_1(Jmp         , GT_JMP)

#ifdef TARGET_64BIT
GTSTRUCT_N(IntConCommon, GT_CNS_INT)
GTSTRUCT_1(IntCon      , GT_CNS_INT)
#else
GTSTRUCT_2_SPECIAL(IntConCommon, GT_CNS_INT, GT_CNS_LNG)
GTSTRUCT_1(IntCon      , GT_CNS_INT)
GTSTRUCT_1(LngCon      , GT_CNS_LNG)
#endif

GTSTRUCT_1(DblCon      , GT_CNS_DBL)
GTSTRUCT_1(StrCon      , GT_CNS_STR)
GTSTRUCT_N(LclVarCommon, GT_LCL_LOAD, GT_LCL_STORE, GT_LCL_LOAD_FLD, GT_LCL_STORE_FLD, GT_LCL_ADDR)
GTSTRUCT_N(LclVar      , GT_LCL_LOAD, GT_LCL_STORE)
GTSTRUCT_1(LclLoad     , GT_LCL_LOAD)
GTSTRUCT_1(LclStore    , GT_LCL_STORE)
GTSTRUCT_N(LclFld      , GT_LCL_LOAD_FLD, GT_LCL_STORE_FLD)
GTSTRUCT_1(LclLoadFld  , GT_LCL_LOAD_FLD)
GTSTRUCT_1(LclStoreFld , GT_LCL_STORE_FLD)
GTSTRUCT_1(LclAddr     , GT_LCL_ADDR)
GTSTRUCT_1(LclDef      , GT_LCL_DEF)
GTSTRUCT_1(LclUse      , GT_LCL_USE)
GTSTRUCT_1(Phi         , GT_PHI)
GTSTRUCT_1(Insert      , GT_INSERT)
GTSTRUCT_1(Extract     , GT_EXTRACT)
GTSTRUCT_1(NullCheck   , GT_NULLCHECK)
GTSTRUCT_1(IndLoad     , GT_IND_LOAD)
GTSTRUCT_1(IndLoadBlk  , GT_IND_LOAD_BLK)
GTSTRUCT_1(IndLoadObj  , GT_IND_LOAD_OBJ)
GTSTRUCT_1(IndStore    , GT_IND_STORE)
GTSTRUCT_1(IndStoreBlk , GT_IND_STORE_BLK)
GTSTRUCT_1(IndStoreObj , GT_IND_STORE_OBJ)
GTSTRUCT_N(Blk         , GT_IND_LOAD_BLK, GT_IND_STORE_BLK, GT_IND_LOAD_OBJ, GT_IND_STORE_OBJ)
GTSTRUCT_N(Indir       , GT_IND_STORE, GT_IND_LOAD, GT_IND_LOAD_BLK, GT_IND_STORE_BLK, GT_IND_LOAD_OBJ, GT_IND_STORE_OBJ, GT_NULLCHECK)
GTSTRUCT_1(Box         , GT_BOX)
GTSTRUCT_1(FieldAddr   , GT_FIELD_ADDR)
GTSTRUCT_1(Call        , GT_CALL)
GTSTRUCT_1(FieldList   , GT_FIELD_LIST)
GTSTRUCT_1(MethodAddr  , GT_METHOD_ADDR)
GTSTRUCT_1(Intrinsic   , GT_INTRINSIC)
GTSTRUCT_1(IndexAddr   , GT_INDEX_ADDR)
GTSTRUCT_1(BoundsChk   , GT_BOUNDS_CHECK)
GTSTRUCT_1(ArrLen      , GT_ARR_LENGTH)
GTSTRUCT_1(ArrElem     , GT_ARR_ELEM)
GTSTRUCT_1(ArrOffs     , GT_ARR_OFFSET)
GTSTRUCT_1(ArrIndex    , GT_ARR_INDEX)
GTSTRUCT_1(RetExpr     , GT_RET_EXPR)
GTSTRUCT_1(ILOffset    , GT_IL_OFFSET)
GTSTRUCT_2(CopyOrReload, GT_COPY, GT_RELOAD)
GTSTRUCT_1(ClsVar      , GT_CLS_VAR_ADDR)
GTSTRUCT_1(ConstAddr   , GT_CONST_ADDR)
GTSTRUCT_1(CmpXchg     , GT_CMPXCHG)
GTSTRUCT_1(AddrMode    , GT_LEA)
GTSTRUCT_2(DynBlk      , GT_COPY_BLK, GT_INIT_BLK)
GTSTRUCT_1(Qmark       , GT_QMARK)

#if FEATURE_ARG_SPLIT
GTSTRUCT_2_SPECIAL(PutArgStk, GT_PUTARG_STK, GT_PUTARG_SPLIT)
GTSTRUCT_1(PutArgSplit , GT_PUTARG_SPLIT)
#else 
GTSTRUCT_1(PutArgStk   , GT_PUTARG_STK)
#endif

GTSTRUCT_1(PhysReg     , GT_PHYSREG)

#ifdef FEATURE_HW_INTRINSICS
GTSTRUCT_1(HWIntrinsic , GT_HWINTRINSIC)
#endif

GTSTRUCT_1(AllocObj    , GT_ALLOCOBJ)
GTSTRUCT_1(RuntimeLookup, GT_RUNTIMELOOKUP)
GTSTRUCT_2(CC          , GT_JCC, GT_SETCC)
GTSTRUCT_1(Instr       , GT_INSTR)
GTSTRUCT_N(TernaryOp   , GT_ARR_OFFSET, GT_CMPXCHG, GT_COPY_BLK, GT_INIT_BLK, GT_QMARK)

#undef  GTSTRUCT_0
#undef  GTSTRUCT_1
#undef  GTSTRUCT_2
#undef  GTSTRUCT_3
#undef  GTSTRUCT_4
#undef  GTSTRUCT_N
#undef  GTSTRUCT_2_SPECIAL
#undef  GTSTRUCT_3_SPECIAL
// clang-format on
