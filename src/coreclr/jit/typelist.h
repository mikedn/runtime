// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_64BIT
#define VTF_I32 0
#define VTF_I64 VTF_I
#define PS 8
#else
#define VTF_I32 VTF_I
#define VTF_I64 0
#define PS 4
#endif

// tn  - TYP_name
// nm  - name string
// jitType - The jit compresses types that are 'equivalent', this is the jit type varActualType()
// sz  - size in bytes (genTypeSize(t))
// sze - size in bytes for the emitter (GC types are encoded) (emitTypeSize(t))
// asze- size in bytes for the emitter (GC types are encoded) (emitActualTypeSize(t))
// al  - alignment
// tf  - flags

// DEF_TP(tn   ,nm        , jitType,    sz, sze,        asze,       al, tf)

// clang-format off
DEF_TP(UNDEF,   "undef",   TYP_UNDEF,   0, EA_UNKNOWN, EA_UNKNOWN,  0,  VTF_ANY)
DEF_TP(VOID,    "void",    TYP_VOID,    0, EA_UNKNOWN, EA_UNKNOWN,  0,  VTF_ANY)
                                                                        
DEF_TP(BOOL,    "bool",    TYP_INT,     1, EA_1BYTE,   EA_4BYTE,    1,  VTF_INT|VTF_UNS)
DEF_TP(BYTE,    "byte",    TYP_INT,     1, EA_1BYTE,   EA_4BYTE,    1,  VTF_INT)
DEF_TP(UBYTE,   "ubyte",   TYP_INT,     1, EA_1BYTE,   EA_4BYTE,    1,  VTF_INT|VTF_UNS)
                                                                        
DEF_TP(SHORT,   "short",   TYP_INT,     2, EA_2BYTE,   EA_4BYTE,    2,  VTF_INT)
DEF_TP(USHORT,  "ushort",  TYP_INT,     2, EA_2BYTE,   EA_4BYTE,    2,  VTF_INT|VTF_UNS)
                                                                        
DEF_TP(INT,     "int",     TYP_INT,     4, EA_4BYTE,   EA_4BYTE,    4,  VTF_INT|VTF_I32)
DEF_TP(UINT,    "uint",    TYP_INT,     4, EA_4BYTE,   EA_4BYTE,    4,  VTF_INT|VTF_UNS|VTF_I32) // Only used in GT_CAST nodes
                                                                        
DEF_TP(LONG,    "long",    TYP_LONG,    8, EA_PTRSIZE, EA_PTRSIZE,  8,  VTF_INT|VTF_I64)
DEF_TP(ULONG,   "ulong",   TYP_LONG,    8, EA_PTRSIZE, EA_PTRSIZE,  8,  VTF_INT|VTF_UNS|VTF_I64) // Only used in GT_CAST nodes
                                                                       
DEF_TP(FLOAT,   "float",   TYP_FLOAT,   4, EA_4BYTE,   EA_4BYTE,    4,  VTF_FLT)
DEF_TP(DOUBLE,  "double",  TYP_DOUBLE,  8, EA_8BYTE,   EA_8BYTE,    8,  VTF_FLT)
                                                                      
DEF_TP(REF,     "ref",     TYP_REF,    PS, EA_GCREF,   EA_GCREF,   PS,  VTF_ANY|VTF_GCR|VTF_I)
DEF_TP(BYREF,   "byref",   TYP_BYREF,  PS, EA_BYREF,   EA_BYREF,   PS,  VTF_ANY|VTF_BYR|VTF_I)
DEF_TP(STRUCT,  "struct",  TYP_STRUCT,  0, EA_UNKNOWN, EA_UNKNOWN,  4,  VTF_S)
                 
DEF_TP(BLK,     "blk",     TYP_BLK,     0, EA_UNKNOWN, EA_UNKNOWN,  4,  VTF_ANY) // blob of memory

#ifdef FEATURE_SIMD
DEF_TP(SIMD8,   "simd8" ,  TYP_SIMD8,   8, EA_8BYTE,   EA_8BYTE,    8,  VTF_S)
DEF_TP(SIMD12,  "simd12",  TYP_SIMD12, 12, EA_16BYTE,  EA_16BYTE,  16,  VTF_S)
DEF_TP(SIMD16,  "simd16",  TYP_SIMD16, 16, EA_16BYTE,  EA_16BYTE,  16,  VTF_S)
DEF_TP(SIMD32,  "simd32",  TYP_SIMD32, 32, EA_32BYTE,  EA_32BYTE,  16,  VTF_S)
#endif                                                                       

DEF_TP(UNKNOWN, "unknown", TYP_UNKNOWN, 0, EA_UNKNOWN, EA_UNKNOWN,  0,  VTF_ANY)
// clang-format on

#undef PS
#undef VTF_I32
#undef VTF_I64
#undef DEF_TP
