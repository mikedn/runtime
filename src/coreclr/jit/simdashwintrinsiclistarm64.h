// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef SIMD_AS_HWINTRINSIC
#error Define SIMD_AS_HWINTRINSIC before including this file
#endif

#pragma push_macro("ID")
#pragma push_macro("IDF")
#pragma push_macro("NM")
#pragma push_macro("NMF")
#pragma push_macro("NONE")
#pragma push_macro("SPECIAL")

#define NONE NI_Illegal
#define SPECIAL NI_SIMD_AS_HWINTRINSIC_START

// Defines a SimdAsHWIntrinsic where the name is implicitly taken from the id
#define ID(classId, id, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, flag)                                         \
    SIMD_AS_HWINTRINSIC(classId, id, #id, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, flag)

#define IDF(classId, id, numarg, t, flag)                                                                              \
    SIMD_AS_HWINTRINSIC(classId, id, #id, numarg, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, t, NONE, flag)

// Defines a SimdAsHWIntrinsic where the name is explicit
#define NM(classId, id, name, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, flag)                                   \
    SIMD_AS_HWINTRINSIC(classId, id, name, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, flag)

#define NMF(classId, id, name, numarg, t, flag)                                                                        \
    SIMD_AS_HWINTRINSIC(classId, id, name, numarg, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, t, NONE, flag)

// clang-format off

// ISA       ID               Name      NumArg TYP_FLOAT            Flags
// -----------------------------------------------------------------------------------------------------
IDF(Vector2, Abs,                       1, NI_AdvSimd_Abs,          SysNumSimdIntrinsicFlag::None)
IDF(Vector2, CopyTo,                    2, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector2, CopyToAt,        "CopyTo", 3, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector2, CreateBroadcast, ".ctor",  2, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector2, Create,          ".ctor",  3, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
IDF(Vector2, Dot,                       2, SPECIAL,                 SysNumSimdIntrinsicFlag::None)
IDF(Vector2, Equals,                    2, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
IDF(Vector2, get_One,                   0, SPECIAL,                 SysNumSimdIntrinsicFlag::None)
IDF(Vector2, get_Zero,                  0, NI_Vector64_get_Zero,    SysNumSimdIntrinsicFlag::None)
IDF(Vector2, Max,                       2, NI_AdvSimd_Max,          SysNumSimdIntrinsicFlag::None)
IDF(Vector2, Min,                       2, NI_AdvSimd_Min,          SysNumSimdIntrinsicFlag::None)
IDF(Vector2, op_Addition,               2, NI_AdvSimd_Add,          SysNumSimdIntrinsicFlag::None)
IDF(Vector2, op_Division,               2, NI_AdvSimd_Arm64_Divide, SysNumSimdIntrinsicFlag::None)
IDF(Vector2, op_Equality,               2, SPECIAL,                 SysNumSimdIntrinsicFlag::None)
IDF(Vector2, op_Inequality,             2, SPECIAL,                 SysNumSimdIntrinsicFlag::None)
IDF(Vector2, op_Multiply,               2, NI_AdvSimd_Multiply,     SysNumSimdIntrinsicFlag::None)
IDF(Vector2, op_Subtraction,            2, NI_AdvSimd_Subtract,     SysNumSimdIntrinsicFlag::None)
IDF(Vector2, SquareRoot,                1, NI_AdvSimd_Arm64_Sqrt,   SysNumSimdIntrinsicFlag::None)

// ISA       ID               Name      NumArg TYP_FLOAT            Flags
// -----------------------------------------------------------------------------------------------------
IDF(Vector3, Abs,                       1, NI_AdvSimd_Abs,          SysNumSimdIntrinsicFlag::None)
IDF(Vector3, CopyTo,                    2, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector3, CopyToAt,        "CopyTo", 3, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector3, CreateBroadcast, ".ctor",  2, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector3, CreateExtend1,   ".ctor",  3, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector3, Create,          ".ctor",  4, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
IDF(Vector3, Dot,                       2, SPECIAL,                 SysNumSimdIntrinsicFlag::None)
IDF(Vector3, Equals,                    2, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
IDF(Vector3, get_One,                   0, SPECIAL,                 SysNumSimdIntrinsicFlag::None)
IDF(Vector3, get_Zero,                  0, NI_Vector128_get_Zero,   SysNumSimdIntrinsicFlag::None)
IDF(Vector3, Max,                       2, NI_AdvSimd_Max,          SysNumSimdIntrinsicFlag::None)
IDF(Vector3, Min,                       2, NI_AdvSimd_Min,          SysNumSimdIntrinsicFlag::None)
IDF(Vector3, op_Addition,               2, NI_AdvSimd_Add,          SysNumSimdIntrinsicFlag::None)
IDF(Vector3, op_Division,               2, NI_AdvSimd_Arm64_Divide, SysNumSimdIntrinsicFlag::None)
IDF(Vector3, op_Equality,               2, SPECIAL,                 SysNumSimdIntrinsicFlag::None)
IDF(Vector3, op_Inequality,             2, SPECIAL,                 SysNumSimdIntrinsicFlag::None)
IDF(Vector3, op_Multiply,               2, NI_AdvSimd_Multiply,     SysNumSimdIntrinsicFlag::None)
IDF(Vector3, op_Subtraction,            2, NI_AdvSimd_Subtract,     SysNumSimdIntrinsicFlag::None)
IDF(Vector3, SquareRoot,                1, NI_AdvSimd_Arm64_Sqrt,   SysNumSimdIntrinsicFlag::None)

// ISA       ID               Name      NumArg TYP_FLOAT            Flags
// -----------------------------------------------------------------------------------------------------
IDF(Vector4, Abs,                       1, NI_AdvSimd_Abs,          SysNumSimdIntrinsicFlag::None)
IDF(Vector4, CopyTo,                    2, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector4, CopyToAt,        "CopyTo", 3, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector4, CreateBroadcast, ".ctor",  2, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector4, CreateExtend1,   ".ctor",  3, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector4, CreateExtend2,   ".ctor",  4, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector4, Create,          ".ctor",  5, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
IDF(Vector4, Dot,                       2, SPECIAL,                 SysNumSimdIntrinsicFlag::None)
IDF(Vector4, Equals,                    2, SPECIAL,                 SysNumSimdIntrinsicFlag::HasThis)
IDF(Vector4, get_One,                   0, SPECIAL,                 SysNumSimdIntrinsicFlag::None)
IDF(Vector4, get_Zero,                  0, NI_Vector128_get_Zero,   SysNumSimdIntrinsicFlag::None)
IDF(Vector4, Max,                       2, NI_AdvSimd_Max,          SysNumSimdIntrinsicFlag::None)
IDF(Vector4, Min,                       2, NI_AdvSimd_Min,          SysNumSimdIntrinsicFlag::None)
IDF(Vector4, op_Addition,               2, NI_AdvSimd_Add,          SysNumSimdIntrinsicFlag::None)
IDF(Vector4, op_Division,               2, NI_AdvSimd_Arm64_Divide, SysNumSimdIntrinsicFlag::None)
IDF(Vector4, op_Equality,               2, SPECIAL,                 SysNumSimdIntrinsicFlag::None)
IDF(Vector4, op_Inequality,             2, SPECIAL,                 SysNumSimdIntrinsicFlag::None)
IDF(Vector4, op_Multiply,               2, NI_AdvSimd_Multiply,     SysNumSimdIntrinsicFlag::None)
IDF(Vector4, op_Subtraction,            2, NI_AdvSimd_Subtract,     SysNumSimdIntrinsicFlag::None)
IDF(Vector4, SquareRoot,                1, NI_AdvSimd_Arm64_Sqrt,   SysNumSimdIntrinsicFlag::None)

// ISA         ID                  Name      NumArg TYP_BYTE                          TYP_UBYTE                             TYP_SHORT                             TYP_USHORT                            TYP_INT                               TYP_UINT                              TYP_LONG                                    TYP_ULONG                                   TYP_FLOAT                              TYP_DOUBLE                                   Flags
// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ID(VectorT128, Abs,                          1, NI_AdvSimd_Abs,                       SPECIAL,                              NI_AdvSimd_Abs,                       SPECIAL,                              NI_AdvSimd_Abs,                       SPECIAL,                              NI_AdvSimd_Arm64_Abs,                       SPECIAL,                                    NI_AdvSimd_Abs,                        NI_AdvSimd_Arm64_Abs,                        SysNumSimdIntrinsicFlag::None)
ID(VectorT128, AndNot,                       2, NI_AdvSimd_BitwiseClear,              NI_AdvSimd_BitwiseClear,              NI_AdvSimd_BitwiseClear,              NI_AdvSimd_BitwiseClear,              NI_AdvSimd_BitwiseClear,              NI_AdvSimd_BitwiseClear,              NI_AdvSimd_BitwiseClear,                    NI_AdvSimd_BitwiseClear,                    NI_AdvSimd_BitwiseClear,               NI_AdvSimd_BitwiseClear,                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, As,                           1, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, Ceiling,                      1, NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                       NONE,                                       NI_AdvSimd_Ceiling,                    NI_AdvSimd_Arm64_Ceiling,                    SysNumSimdIntrinsicFlag::None)
ID(VectorT128, ConditionalSelect,            3, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, ConvertToInt32,               1, NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                       NONE,                                       NI_AdvSimd_ConvertToInt32RoundToZero,  NONE,                                        SysNumSimdIntrinsicFlag::None)
ID(VectorT128, ConvertToUInt32,              1, NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                       NONE,                                       NI_AdvSimd_ConvertToUInt32RoundToZero, NONE,                                        SysNumSimdIntrinsicFlag::None)
ID(VectorT128, ConvertToInt64,               1, NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                       NONE,                                       NONE,                                  NI_AdvSimd_Arm64_ConvertToInt64RoundToZero,  SysNumSimdIntrinsicFlag::None)
ID(VectorT128, ConvertToUInt64,              1, NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                       NONE,                                       NONE,                                  NI_AdvSimd_Arm64_ConvertToUInt64RoundToZero, SysNumSimdIntrinsicFlag::None)
ID(VectorT128, ConvertToSingle,              1, NONE,                                 NONE,                                 NONE,                                 NONE,                                 NI_AdvSimd_ConvertToSingle,           NI_AdvSimd_ConvertToSingle,           NONE,                                       NONE,                                       NONE,                                  NONE,                                        SysNumSimdIntrinsicFlag::None)
ID(VectorT128, ConvertToDouble,              1, NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NI_AdvSimd_Arm64_ConvertToDouble,           NI_AdvSimd_Arm64_ConvertToDouble,           NONE,                                  NONE,                                        SysNumSimdIntrinsicFlag::None)
ID(VectorT128, CopyTo,                       2, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::HasThis)
NM(VectorT128, CopyToAt,           "CopyTo", 3, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::HasThis)
NM(VectorT128, CreateBroadcast,    ".ctor",  2, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::HasThis)
NM(VectorT128, FromArray,          ".ctor",  3, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::HasThis)
ID(VectorT128, Dot,                          2, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, Equals,                       2, NI_AdvSimd_CompareEqual,              NI_AdvSimd_CompareEqual,              NI_AdvSimd_CompareEqual,              NI_AdvSimd_CompareEqual,              NI_AdvSimd_CompareEqual,              NI_AdvSimd_CompareEqual,              NI_AdvSimd_Arm64_CompareEqual,              NI_AdvSimd_Arm64_CompareEqual,              NI_AdvSimd_CompareEqual,               NI_AdvSimd_Arm64_CompareEqual,               SysNumSimdIntrinsicFlag::None)
NM(VectorT128, EqualsInstance,     "Equals", 2, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::HasThis)
ID(VectorT128, Floor,                        1, NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                       NONE,                                       NI_AdvSimd_Floor,                      NI_AdvSimd_Arm64_Floor,                      SysNumSimdIntrinsicFlag::None)
ID(VectorT128, get_AllBitsSet,               0, NI_Vector128_get_AllBitsSet,          NI_Vector128_get_AllBitsSet,          NI_Vector128_get_AllBitsSet,          NI_Vector128_get_AllBitsSet,          NI_Vector128_get_AllBitsSet,          NI_Vector128_get_AllBitsSet,          NI_Vector128_get_AllBitsSet,                NI_Vector128_get_AllBitsSet,                NI_Vector128_get_AllBitsSet,           NI_Vector128_get_AllBitsSet,                 SysNumSimdIntrinsicFlag::None)
ID(VectorT128, get_Count,                    0, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, get_IsHardwareAccelerated,    0, NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                       NONE,                                       NONE,                                  NONE,                                        SysNumSimdIntrinsicFlag::None)
ID(VectorT128, get_Item,                     2, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::HasThis)
ID(VectorT128, get_One,                      0, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, get_Zero,                     0, NI_Vector128_get_Zero,                NI_Vector128_get_Zero,                NI_Vector128_get_Zero,                NI_Vector128_get_Zero,                NI_Vector128_get_Zero,                NI_Vector128_get_Zero,                NI_Vector128_get_Zero,                      NI_Vector128_get_Zero,                      NI_Vector128_get_Zero,                 NI_Vector128_get_Zero,                       SysNumSimdIntrinsicFlag::None)
ID(VectorT128, GreaterThan,                  2, NI_AdvSimd_CompareGreaterThan,        NI_AdvSimd_CompareGreaterThan,        NI_AdvSimd_CompareGreaterThan,        NI_AdvSimd_CompareGreaterThan,        NI_AdvSimd_CompareGreaterThan,        NI_AdvSimd_CompareGreaterThan,        NI_AdvSimd_Arm64_CompareGreaterThan,        NI_AdvSimd_Arm64_CompareGreaterThan,        NI_AdvSimd_CompareGreaterThan,         NI_AdvSimd_Arm64_CompareGreaterThan,         SysNumSimdIntrinsicFlag::None)
ID(VectorT128, GreaterThanOrEqual,           2, NI_AdvSimd_CompareGreaterThanOrEqual, NI_AdvSimd_CompareGreaterThanOrEqual, NI_AdvSimd_CompareGreaterThanOrEqual, NI_AdvSimd_CompareGreaterThanOrEqual, NI_AdvSimd_CompareGreaterThanOrEqual, NI_AdvSimd_CompareGreaterThanOrEqual, NI_AdvSimd_Arm64_CompareGreaterThanOrEqual, NI_AdvSimd_Arm64_CompareGreaterThanOrEqual, NI_AdvSimd_CompareGreaterThanOrEqual,  NI_AdvSimd_Arm64_CompareGreaterThanOrEqual,  SysNumSimdIntrinsicFlag::None)
ID(VectorT128, LessThan,                     2, NI_AdvSimd_CompareLessThan,           NI_AdvSimd_CompareLessThan,           NI_AdvSimd_CompareLessThan,           NI_AdvSimd_CompareLessThan,           NI_AdvSimd_CompareLessThan,           NI_AdvSimd_CompareLessThan,           NI_AdvSimd_Arm64_CompareLessThan,           NI_AdvSimd_Arm64_CompareLessThan,           NI_AdvSimd_CompareLessThan,            NI_AdvSimd_Arm64_CompareLessThan,            SysNumSimdIntrinsicFlag::None)
ID(VectorT128, LessThanOrEqual,              2, NI_AdvSimd_CompareLessThanOrEqual,    NI_AdvSimd_CompareLessThanOrEqual,    NI_AdvSimd_CompareLessThanOrEqual,    NI_AdvSimd_CompareLessThanOrEqual,    NI_AdvSimd_CompareLessThanOrEqual,    NI_AdvSimd_CompareLessThanOrEqual,    NI_AdvSimd_Arm64_CompareLessThanOrEqual,    NI_AdvSimd_Arm64_CompareLessThanOrEqual,    NI_AdvSimd_CompareLessThanOrEqual,     NI_AdvSimd_Arm64_CompareLessThanOrEqual,     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, Max,                          2, NI_AdvSimd_Max,                       NI_AdvSimd_Max,                       NI_AdvSimd_Max,                       NI_AdvSimd_Max,                       NI_AdvSimd_Max,                       NI_AdvSimd_Max,                       SPECIAL,                                    SPECIAL,                                    NI_AdvSimd_Max,                        NI_AdvSimd_Arm64_Max,                        SysNumSimdIntrinsicFlag::None)
ID(VectorT128, Min,                          2, NI_AdvSimd_Min,                       NI_AdvSimd_Min,                       NI_AdvSimd_Min,                       NI_AdvSimd_Min,                       NI_AdvSimd_Min,                       NI_AdvSimd_Min,                       SPECIAL,                                    SPECIAL,                                    NI_AdvSimd_Min,                        NI_AdvSimd_Arm64_Min,                        SysNumSimdIntrinsicFlag::None)
ID(VectorT128, Narrow,                       2, NONE,                                 NONE,                                 SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    NONE,                                  SPECIAL,                                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_Addition,                  2, NI_AdvSimd_Add,                       NI_AdvSimd_Add,                       NI_AdvSimd_Add,                       NI_AdvSimd_Add,                       NI_AdvSimd_Add,                       NI_AdvSimd_Add,                       NI_AdvSimd_Add,                             NI_AdvSimd_Add,                             NI_AdvSimd_Add,                        NI_AdvSimd_Arm64_Add,                        SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_BitwiseAnd,                2, NI_AdvSimd_And,                       NI_AdvSimd_And,                       NI_AdvSimd_And,                       NI_AdvSimd_And,                       NI_AdvSimd_And,                       NI_AdvSimd_And,                       NI_AdvSimd_And,                             NI_AdvSimd_And,                             NI_AdvSimd_And,                        NI_AdvSimd_And,                              SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_BitwiseOr,                 2, NI_AdvSimd_Or,                        NI_AdvSimd_Or,                        NI_AdvSimd_Or,                        NI_AdvSimd_Or,                        NI_AdvSimd_Or,                        NI_AdvSimd_Or,                        NI_AdvSimd_Or,                              NI_AdvSimd_Or,                              NI_AdvSimd_Or,                         NI_AdvSimd_Or,                               SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_Division,                  2, NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                       NONE,                                       NI_AdvSimd_Arm64_Divide,               NI_AdvSimd_Arm64_Divide,                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_Equality,                  2, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_ExclusiveOr,               2, NI_AdvSimd_Xor,                       NI_AdvSimd_Xor,                       NI_AdvSimd_Xor,                       NI_AdvSimd_Xor,                       NI_AdvSimd_Xor,                       NI_AdvSimd_Xor,                       NI_AdvSimd_Xor,                             NI_AdvSimd_Xor,                             NI_AdvSimd_Xor,                        NI_AdvSimd_Xor,                              SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_Explicit,                  1, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_Inequality,                2, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                                    SPECIAL,                                    SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_Multiply,                  2, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              NONE,                                       NONE,                                       SPECIAL,                               SPECIAL,                                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_Subtraction,               2, NI_AdvSimd_Subtract,                  NI_AdvSimd_Subtract,                  NI_AdvSimd_Subtract,                  NI_AdvSimd_Subtract,                  NI_AdvSimd_Subtract,                  NI_AdvSimd_Subtract,                  NI_AdvSimd_Subtract,                        NI_AdvSimd_Subtract,                        NI_AdvSimd_Subtract,                   NI_AdvSimd_Arm64_Subtract,                   SysNumSimdIntrinsicFlag::None)
ID(VectorT128, SquareRoot,                   1, NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                 NONE,                                       NONE,                                       NI_AdvSimd_Arm64_Sqrt,                 NI_AdvSimd_Arm64_Sqrt,                       SysNumSimdIntrinsicFlag::None)
ID(VectorT128, Widen,                        3, SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              SPECIAL,                              NONE,                                       NONE,                                       SPECIAL,                               NONE,                                        SysNumSimdIntrinsicFlag::None)

// clang-format on

#pragma pop_macro("ID")
#pragma pop_macro("IDF")
#pragma pop_macro("NM")
#pragma pop_macro("NMF")
#pragma pop_macro("NONE")
#pragma pop_macro("SPECIAL")

#undef SIMD_AS_HWINTRINSIC
