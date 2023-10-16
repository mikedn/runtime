// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef GTNODE
#error Define GTNODE before including this file.
#endif

// clang-format off
//     name             , struct              , kind
GTNODE(NONE             , GenTree             , GTK_SPECIAL)

//-----------------------------------------------------------------------------
//  Leaf nodes (i.e. these nodes have no sub-operands):
//-----------------------------------------------------------------------------

GTNODE(STORE_LCL_VAR    , GenTreeLclVar       , GTK_UNOP|GTK_NOVALUE) // store to local variable
GTNODE(STORE_LCL_FLD    , GenTreeLclFld       , GTK_UNOP|GTK_NOVALUE) // store to field in a non-primitive variable
GTNODE(LCL_VAR          , GenTreeLclVar       , GTK_LEAF)             // local variable
GTNODE(LCL_FLD          , GenTreeLclFld       , GTK_LEAF)             // field in a non-primitive variable
GTNODE(LCL_DEF          , GenTreeLclDef       , GTK_UNOP)             // local variable SSA def
GTNODE(LCL_USE          , GenTreeLclUse       , GTK_LEAF)             // local variable SSA use
GTNODE(LCL_ADDR         , GenTreeLclAddr      , GTK_LEAF)             // address of a local variable
GTNODE(CATCH_ARG        , GenTree             , GTK_LEAF)             // Exception object in a catch block
GTNODE(LABEL            , GenTree             , GTK_LEAF)             // Jump-target
GTNODE(FTN_ADDR         , GenTreeFptrVal      , GTK_LEAF)             // Address of a function
GTNODE(RET_EXPR         , GenTreeRetExpr      , GTK_LEAF|GTK_NOTLIR)  // Place holder for the return expression from an inline candidate

//-----------------------------------------------------------------------------
//  Constant nodes:
//-----------------------------------------------------------------------------

GTNODE(CNS_INT          , GenTreeIntCon       , GTK_LEAF)
GTNODE(CNS_LNG          , GenTreeLngCon       , GTK_LEAF)
GTNODE(CNS_DBL          , GenTreeDblCon       , GTK_LEAF)
GTNODE(CNS_STR          , GenTreeStrCon       , GTK_LEAF)

//-----------------------------------------------------------------------------
//  Unary  operators
//-----------------------------------------------------------------------------

GTNODE(NOT              , GenTreeOp           , GTK_UNOP|GTK_VN)
GTNODE(NOP              , GenTree             , GTK_UNOP|GTK_NOCONTAIN)
GTNODE(NEG              , GenTreeOp           , GTK_UNOP|GTK_VN)
GTNODE(FNEG             , GenTreeOp           , GTK_UNOP|GTK_VN)

GTNODE(COPY             , GenTreeCopyOrReload , GTK_UNOP) // Copies a variable from its current location to a register that satisfies
                                                          // code generation constraints. The child is the actual lclVar node.
GTNODE(RELOAD           , GenTreeCopyOrReload , GTK_UNOP)

GTNODE(INTRINSIC        , GenTreeIntrinsic    , GTK_BINOP|GTK_EXOP)     

GTNODE(LOCKADD          , GenTreeOp           , GTK_BINOP|GTK_NOVALUE)
GTNODE(XAND             , GenTreeOp           , GTK_BINOP)
GTNODE(XORR             , GenTreeOp           , GTK_BINOP)
GTNODE(XADD             , GenTreeOp           , GTK_BINOP)
GTNODE(XCHG             , GenTreeOp           , GTK_BINOP)
GTNODE(CMPXCHG          , GenTreeCmpXchg      , GTK_SPECIAL)
GTNODE(MEMORYBARRIER    , GenTree             , GTK_LEAF|GTK_NOVALUE)

GTNODE(KEEPALIVE        , GenTree             , GTK_UNOP|GTK_NOVALUE)   // keep operand alive, generate no code, produce no result
GTNODE(CAST             , GenTreeCast         , GTK_UNOP|GTK_EXOP)      // conversion to another type
GTNODE(BITCAST          , GenTreeOp           , GTK_UNOP)               // reinterpretation of bits as another type
GTNODE(CKFINITE         , GenTreeOp           , GTK_UNOP|GTK_NOCONTAIN) // Check for NaN
GTNODE(LCLHEAP          , GenTreeOp           , GTK_UNOP|GTK_NOCONTAIN) // alloca()
GTNODE(JMP              , GenTreeVal          , GTK_LEAF|GTK_NOVALUE)   // Jump to another function

GTNODE(ARR_LENGTH       , GenTreeArrLen       , GTK_UNOP|GTK_EXOP|GTK_VN) // array-length
GTNODE(NULLCHECK        , GenTreeIndir        , GTK_UNOP|GTK_NOVALUE)     // null checks the source
GTNODE(IND              , GenTreeIndir        , GTK_UNOP)                 // load indirection
GTNODE(STOREIND         , GenTreeStoreInd     , GTK_BINOP|GTK_NOVALUE)    // store indirection
GTNODE(OBJ              , GenTreeObj          , GTK_UNOP|GTK_EXOP)              // Object that MAY have gc pointers, and thus includes the relevant gc layout info.
GTNODE(STORE_OBJ        , GenTreeObj          , GTK_BINOP|GTK_EXOP|GTK_NOVALUE) // Object that MAY have gc pointers, and thus includes the relevant gc layout info.
GTNODE(BLK              , GenTreeBlk          , GTK_UNOP|GTK_EXOP)              // Block/object with no gc pointers, and with a known size (e.g. a struct with no gc fields)
GTNODE(STORE_BLK        , GenTreeBlk          , GTK_BINOP|GTK_EXOP|GTK_NOVALUE) // Block/object with no gc pointers, and with a known size (e.g. a struct with no gc fields)

GTNODE(COPY_BLK         , GenTreeDynBlk       , GTK_SPECIAL|GTK_NOVALUE) // Dynamically sized block copy
GTNODE(INIT_BLK         , GenTreeDynBlk       , GTK_SPECIAL|GTK_NOVALUE) // Dynamically sized block init

GTNODE(BOX              , GenTreeBox          , GTK_UNOP|GTK_EXOP|GTK_NOTLIR)
GTNODE(INSERT           , GenTreeInsert       , GTK_BINOP|GTK_EXOP|GTK_NOTLIR)
GTNODE(EXTRACT          , GenTreeExtract      , GTK_UNOP|GTK_EXOP|GTK_NOTLIR)
GTNODE(BOUNDS_CHECK     , GenTreeBoundsChk    , GTK_BINOP|GTK_EXOP|GTK_NOVALUE)
GTNODE(ALLOCOBJ         , GenTreeAllocObj     , GTK_UNOP|GTK_EXOP) // object allocator
GTNODE(INIT_VAL         , GenTreeOp           , GTK_UNOP|GTK_VN)   // Initialization value for an initBlk
GTNODE(RUNTIMELOOKUP    , GenTreeRuntimeLookup, GTK_UNOP|GTK_EXOP) // Runtime handle lookup
GTNODE(BSWAP            , GenTreeOp           , GTK_UNOP|GTK_VN)   // Byte swap (32-bit or 64-bit)
GTNODE(BSWAP16          , GenTreeOp           , GTK_UNOP|GTK_VN)   // Byte swap (16-bit)
GTNODE(INC_SATURATE     , GenTreeOp           , GTK_UNOP)          // saturating increment, used in division by a constant (LowerUnsignedDivOrMod)

//-----------------------------------------------------------------------------
//  Binary operators
//-----------------------------------------------------------------------------

GTNODE(FADD             , GenTreeOp           , GTK_BINOP|GTK_COMMUTE|GTK_VN)
GTNODE(FSUB             , GenTreeOp           , GTK_BINOP|GTK_VN)
GTNODE(FMUL             , GenTreeOp           , GTK_BINOP|GTK_COMMUTE|GTK_VN)
GTNODE(FDIV             , GenTreeOp           , GTK_BINOP|GTK_VN)
GTNODE(FMOD             , GenTreeOp           , GTK_BINOP|GTK_VN)

GTNODE(ADD              , GenTreeOp           , GTK_BINOP|GTK_COMMUTE|GTK_VN)
GTNODE(SUB              , GenTreeOp           , GTK_BINOP|GTK_VN)
GTNODE(MUL              , GenTreeOp           , GTK_BINOP|GTK_COMMUTE|GTK_VN)
GTNODE(DIV              , GenTreeOp           , GTK_BINOP|GTK_VN)
GTNODE(MOD              , GenTreeOp           , GTK_BINOP|GTK_VN)
GTNODE(UDIV             , GenTreeOp           , GTK_BINOP|GTK_VN)
GTNODE(UMOD             , GenTreeOp           , GTK_BINOP|GTK_VN)
GTNODE(OR               , GenTreeOp           , GTK_BINOP|GTK_COMMUTE|GTK_VN)
GTNODE(XOR              , GenTreeOp           , GTK_BINOP|GTK_COMMUTE|GTK_VN)
GTNODE(AND              , GenTreeOp           , GTK_BINOP|GTK_COMMUTE|GTK_VN)
GTNODE(LSH              , GenTreeOp           , GTK_BINOP|GTK_VN)
GTNODE(RSH              , GenTreeOp           , GTK_BINOP|GTK_VN)
GTNODE(RSZ              , GenTreeOp           , GTK_BINOP|GTK_VN)
GTNODE(ROL              , GenTreeOp           , GTK_BINOP|GTK_VN)
GTNODE(ROR              , GenTreeOp           , GTK_BINOP|GTK_VN)

// GT_MULHI is used in division by a constant (fgMorphDivByConst). We turn
// the DIV into a MULHI + some adjustments. In codegen, we only use the
// results of the high register, and we drop the low results.
GTNODE(MULHI            , GenTreeOp           , GTK_BINOP|GTK_COMMUTE) 

GTNODE(ASG              , GenTreeOp           , GTK_BINOP|GTK_NOTLIR)

GTNODE(EQ               , GenTreeOp           , GTK_BINOP|GTK_NOCONTAIN|GTK_VN)
GTNODE(NE               , GenTreeOp           , GTK_BINOP|GTK_NOCONTAIN|GTK_VN)
GTNODE(LT               , GenTreeOp           , GTK_BINOP|GTK_NOCONTAIN|GTK_VN)
GTNODE(LE               , GenTreeOp           , GTK_BINOP|GTK_NOCONTAIN|GTK_VN)
GTNODE(GE               , GenTreeOp           , GTK_BINOP|GTK_NOCONTAIN|GTK_VN)
GTNODE(GT               , GenTreeOp           , GTK_BINOP|GTK_NOCONTAIN|GTK_VN)

// These are similar to GT_EQ/GT_NE but they generate "test" instead of "cmp" instructions.
// Currently these are generated during lowering for code like ((x & y) eq|ne 0) only on
// XArch but ARM could too use these for the same purpose as there is a "tst" instruction.
// Note that the general case of comparing a register against 0 is handled directly by
// codegen which emits a "test reg, reg" instruction, that would be more difficult to do
// during lowering because the source operand is used twice so it has to be a lclvar.
// Because of this there is no need to also add GT_TEST_LT/LE/GE/GT opers.
GTNODE(TEST_EQ          , GenTreeOp           , GTK_BINOP|GTK_NOCONTAIN)
GTNODE(TEST_NE          , GenTreeOp           , GTK_BINOP|GTK_NOCONTAIN)

GTNODE(COMMA            , GenTreeOp           , GTK_BINOP|GTK_NOTLIR)
GTNODE(QMARK            , GenTreeQmark        , GTK_SPECIAL|GTK_NOTLIR)
GTNODE(INDEX_ADDR       , GenTreeIndexAddr    , GTK_BINOP|GTK_EXOP|GTK_VN) // addr of SZ-array-element;
GTNODE(MKREFANY         , GenTreeOp           , GTK_BINOP|GTK_NOTLIR)
GTNODE(LEA              , GenTreeAddrMode     , GTK_BINOP|GTK_EXOP)

#ifndef TARGET_64BIT
// A GT_LONG node simply represents the long value produced by the concatenation
// of its two (lower and upper half) operands.  Some GT_LONG nodes are transient,
// during the decomposing of longs; others are handled by codegen as operands of
// nodes such as calls, returns and stores of long lclVars.
GTNODE(LONG             , GenTreeOp           , GTK_BINOP)

// The following are nodes representing x86/arm32 specific long operators, including
// high operators of a 64-bit operations that requires a carry/borrow, which are
// named GT_XXX_HI for consistency, low operators of 64-bit operations that need
// to not be modified in phases post-decompose, and operators that return 64-bit
// results in one instruction.
GTNODE(ADD_LO           , GenTreeOp           , GTK_BINOP|GTK_COMMUTE)
GTNODE(ADD_HI           , GenTreeOp           , GTK_BINOP|GTK_COMMUTE)
GTNODE(SUB_LO           , GenTreeOp           , GTK_BINOP)
GTNODE(SUB_HI           , GenTreeOp           , GTK_BINOP)

// A mul that returns the 2N bit result of an NxN multiply. This op is used for
// multiplies that take two ints and return a long result. All other multiplies
// with long results are morphed into helper calls. It is similar to GT_MULHI,
// the difference being that GT_MULHI drops the lo part of the result, whereas
// GT_MUL_LONG keeps both parts of the result.
GTNODE(MUL_LONG         , GenTreeOp           , GTK_BINOP|GTK_COMMUTE)

// The following are nodes that specify shifts that take a GT_LONG op1. The GT_LONG
// contains the hi and lo parts of three operand shift form where one op will be
// shifted into the other op as part of the operation (LSH_HI will shift
// the high bits of the lo operand into the high operand as it shifts left. RSH_LO
// will shift the lo bits of the high operand into the lo operand). LSH_HI
// represents the high operation of a 64-bit left shift by a constant int, and
// RSH_LO represents the lo operation of a 64-bit right shift by a constant int.
GTNODE(LSH_HI           , GenTreeOp           , GTK_BINOP)
GTNODE(RSH_LO           , GenTreeOp           , GTK_BINOP)
#endif // !TARGET_64BIT

#ifdef FEATURE_HW_INTRINSICS
GTNODE(HWINTRINSIC      , GenTreeHWIntrinsic  , GTK_SPECIAL)
#endif 

//-----------------------------------------------------------------------------
//  LIR specific compare and conditional branch/set nodes:
//-----------------------------------------------------------------------------

GTNODE(CMP              , GenTreeOp           , GTK_BINOP|GTK_NOVALUE) // Sets the condition flags according to the compare result.
                                                                       // N.B. Not a relop, it does not produce a value and it cannot be reversed.
GTNODE(JCC              , GenTreeCC           , GTK_LEAF|GTK_NOVALUE)  // Checks the condition flags and branch if the condition specified
                                                                       // by GenTreeCC::Condition is true.
GTNODE(SETCC            , GenTreeCC           , GTK_LEAF)              // Checks the condition flags and produces 1 if the condition specified
                                                                       // by GenTreeCC::Condition is true and 0 otherwise.
#ifdef TARGET_ARM64
GTNODE(JCMP             , GenTreeOp           , GTK_BINOP|GTK_NOVALUE) // Makes a comparison and jump if the condition specified. Does not set flags
#endif
#ifdef TARGET_XARCH
GTNODE(BT               , GenTreeOp           , GTK_BINOP|GTK_NOVALUE) // The XARCH BT instruction. Like CMP, this sets the condition flags
                                                                       // and does not produce a value.
#endif

//-----------------------------------------------------------------------------
//  Other nodes that look like unary/binary operators:
//-----------------------------------------------------------------------------

GTNODE(JTRUE            , GenTreeOp           , GTK_UNOP|GTK_NOVALUE)

//-----------------------------------------------------------------------------
//  Other nodes that have special structure:
//-----------------------------------------------------------------------------

GTNODE(FIELD_ADDR       , GenTreeFieldAddr    , GTK_UNOP|GTK_EXOP|GTK_NOTLIR) // Member-field address
GTNODE(ARR_ELEM         , GenTreeArrElem      , GTK_SPECIAL)            // Multi-dimensional array-element address
GTNODE(ARR_INDEX        , GenTreeArrIndex     , GTK_BINOP|GTK_EXOP)     // Effective, bounds-checked index for one dimension of a multi-dimensional array element
GTNODE(ARR_OFFSET       , GenTreeArrOffs      , GTK_SPECIAL)            // Flattened offset of multi-dimensional array element
GTNODE(CALL             , GenTreeCall         , GTK_SPECIAL|GTK_NOCONTAIN)
GTNODE(FIELD_LIST       , GenTreeFieldList    , GTK_SPECIAL)            // List of fields of a struct, when passed as an argument
GTNODE(RETURN           , GenTreeOp           , GTK_UNOP|GTK_NOVALUE)   // return from current function
GTNODE(SWITCH           , GenTreeOp           , GTK_UNOP|GTK_NOVALUE)   // switch
GTNODE(NO_OP            , GenTree             , GTK_LEAF|GTK_NOVALUE)   // nop!
GTNODE(START_NONGC      , GenTree             , GTK_LEAF|GTK_NOVALUE)   // starts a new instruction group that will be non-gc interruptible
GTNODE(START_PREEMPTGC  , GenTree             , GTK_LEAF|GTK_NOVALUE)   // starts a new instruction group where preemptive GC is enabled
GTNODE(PROF_HOOK        , GenTree             , GTK_LEAF|GTK_NOVALUE)   // profiler Enter/Leave/TailCall hook
GTNODE(RETFILT          , GenTreeOp           , GTK_UNOP|GTK_NOVALUE)   // end filter with TYP_I_IMPL return value

#ifndef FEATURE_EH_FUNCLETS
GTNODE(END_LFIN         , GenTreeVal          , GTK_LEAF|GTK_NOVALUE)   // end locally-invoked finally
#endif 

//-----------------------------------------------------------------------------
//  Nodes used for optimizations.
//-----------------------------------------------------------------------------

GTNODE(PHI              , GenTreePhi          , GTK_SPECIAL)

//-----------------------------------------------------------------------------
//  Nodes used by Lower to generate a closer CPU representation of other nodes
//-----------------------------------------------------------------------------

GTNODE(JMPTABLE         , GenTree             , GTK_LEAF|GTK_NOCONTAIN) // Generates the jump table for switches
GTNODE(SWITCH_TABLE     , GenTreeOp           , GTK_BINOP|GTK_NOVALUE)  // Jump Table based switch construct

//-----------------------------------------------------------------------------
//  Nodes used only within the code generator:
//-----------------------------------------------------------------------------

GTNODE(CLS_VAR_ADDR     , GenTreeClsVar       , GTK_LEAF)                        // static data member address
GTNODE(ARGPLACE         , GenTree             , GTK_LEAF|GTK_NOVALUE|GTK_NOTLIR) // placeholder for a register arg
GTNODE(PHYSREG          , GenTreePhysReg      , GTK_LEAF)                        // read from a physical register
GTNODE(EMITNOP          , GenTree             , GTK_LEAF|GTK_NOVALUE)            // emitter-placed nop
GTNODE(PINVOKE_PROLOG   , GenTree             , GTK_LEAF|GTK_NOVALUE)            // pinvoke prolog seq
GTNODE(PINVOKE_EPILOG   , GenTree             , GTK_LEAF|GTK_NOVALUE)            // pinvoke epilog seq
GTNODE(PUTARG_REG       , GenTreeOp           , GTK_UNOP)                        // operator that places outgoing arg in register
GTNODE(PUTARG_STK       , GenTreePutArgStk    , GTK_UNOP|GTK_NOVALUE)            // operator that places outgoing arg in stack

#if FEATURE_ARG_SPLIT                          
GTNODE(PUTARG_SPLIT     , GenTreePutArgSplit  , GTK_UNOP)                        // operator that places outgoing arg in registers with stack (split struct in ARM32)
#endif

GTNODE(RETURNTRAP       , GenTreeOp           , GTK_UNOP|GTK_NOVALUE)            // a conditional call to wait on gc

#ifdef TARGET_XARCH                            
GTNODE(SWAP             , GenTreeOp           , GTK_BINOP|GTK_NOVALUE)           // op1 and op2 swap (registers)
#endif

GTNODE(IL_OFFSET        , GenTreeILOffset     , GTK_LEAF|GTK_NOVALUE)            // marks an IL offset for debugging purposes
GTNODE(INSTR            , GenTreeInstr        , GTK_SPECIAL)

#ifdef FEATURE_SIMD
GTNODE(SIMD_UPPER_SPILL  ,GenTreeUnOp         , GTK_UNOP)
GTNODE(SIMD_UPPER_UNSPILL,GenTreeUnOp         , GTK_UNOP)
#endif
// clang-format on

#undef GTNODE
