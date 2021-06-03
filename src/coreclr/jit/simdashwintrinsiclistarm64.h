// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef SIMD_AS_HWINTRINSIC
#error Define SIMD_AS_HWINTRINSIC before including this file
#endif

#pragma push_macro("ID")
#pragma push_macro("IDF")
#pragma push_macro("NM")
#pragma push_macro("NMF")

// Defines a SimdAsHWIntrinsic where the name is implicitly taken from the id
#define ID(classId, id, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, flag)                                         \
    SIMD_AS_HWINTRINSIC(classId, id, #id, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, flag)

#define IDF(classId, id, numarg, t, flag)                                                                              \
    SIMD_AS_HWINTRINSIC(classId, id, #id, numarg, NI_Illegal, NI_Illegal, NI_Illegal, NI_Illegal, NI_Illegal,          \
                        NI_Illegal, NI_Illegal, NI_Illegal, t, NI_Illegal, flag)

// Defines a SimdAsHWIntrinsic where the name is explicit
#define NM(classId, id, name, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, flag)                                   \
    SIMD_AS_HWINTRINSIC(classId, id, name, numarg, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, flag)

#define NMF(classId, id, name, numarg, t, flag)                                                                        \
    SIMD_AS_HWINTRINSIC(classId, id, name, numarg, NI_Illegal, NI_Illegal, NI_Illegal, NI_Illegal, NI_Illegal,         \
                        NI_Illegal, NI_Illegal, NI_Illegal, t, NI_Illegal, flag)

// NI_Illegal is used to represent an unsupported vector element type
// Using the same intrinsic as the represented entry is used to indicate special handling is required

// clang-format off

// ISA       ID               Name      NumArg TYP_FLOAT               Flags
// -----------------------------------------------------------------------------------------------------
IDF(Vector2, Abs,                       1, NI_AdvSimd_Abs,             SysNumSimdIntrinsicFlag::None)
IDF(Vector2, CopyTo,                    2, NI_Vector2_CopyTo,          SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector2, CopyToAt,        "CopyTo", 3, NI_Vector2_CopyToAt,        SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector2, CreateBroadcast, ".ctor",  2, NI_Vector2_CreateBroadcast, SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector2, Create,          ".ctor",  3, NI_Vector2_Create,          SysNumSimdIntrinsicFlag::HasThis)
IDF(Vector2, Dot,                       2, NI_Vector64_Dot,            SysNumSimdIntrinsicFlag::None)
NMF(Vector2, EqualsInstance,  "Equals", 2, NI_Vector64_op_Equality,    SysNumSimdIntrinsicFlag::HasThis)
IDF(Vector2, get_One,                   0, NI_Vector2_get_One,         SysNumSimdIntrinsicFlag::None)
IDF(Vector2, get_Zero,                  0, NI_Vector64_get_Zero,       SysNumSimdIntrinsicFlag::None)
IDF(Vector2, Max,                       2, NI_AdvSimd_Max,             SysNumSimdIntrinsicFlag::None)
IDF(Vector2, Min,                       2, NI_AdvSimd_Min,             SysNumSimdIntrinsicFlag::None)
IDF(Vector2, op_Addition,               2, NI_AdvSimd_Add,             SysNumSimdIntrinsicFlag::None)
IDF(Vector2, op_Division,               2, NI_AdvSimd_Arm64_Divide,    SysNumSimdIntrinsicFlag::None)
IDF(Vector2, op_Equality,               2, NI_Vector64_op_Equality,    SysNumSimdIntrinsicFlag::None)
IDF(Vector2, op_Inequality,             2, NI_Vector64_op_Inequality,  SysNumSimdIntrinsicFlag::None)
IDF(Vector2, op_Multiply,               2, NI_AdvSimd_Multiply,        SysNumSimdIntrinsicFlag::None)
IDF(Vector2, op_Subtraction,            2, NI_AdvSimd_Subtract,        SysNumSimdIntrinsicFlag::None)
IDF(Vector2, SquareRoot,                1, NI_AdvSimd_Arm64_Sqrt,      SysNumSimdIntrinsicFlag::None)

// ISA       ID               Name      NumArg TYP_FLOAT               Flags
// -----------------------------------------------------------------------------------------------------
IDF(Vector3, Abs,                       1, NI_AdvSimd_Abs,             SysNumSimdIntrinsicFlag::None)
IDF(Vector3, CopyTo,                    2, NI_Vector3_CopyTo,          SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector3, CopyToAt,        "CopyTo", 3, NI_Vector3_CopyToAt,        SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector3, CreateBroadcast, ".ctor",  2, NI_Vector3_CreateBroadcast, SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector3, CreateExtend1,   ".ctor",  3, NI_Vector3_CreateExtend1,   SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector3, Create,          ".ctor",  4, NI_Vector3_Create,          SysNumSimdIntrinsicFlag::HasThis)
IDF(Vector3, Dot,                       2, NI_Vector128_Dot,           SysNumSimdIntrinsicFlag::None)
NMF(Vector3, EqualsInstance,  "Equals", 2, NI_Vector128_op_Equality,   SysNumSimdIntrinsicFlag::HasThis)
IDF(Vector3, get_One,                   0, NI_Vector3_get_One,         SysNumSimdIntrinsicFlag::None)
IDF(Vector3, get_Zero,                  0, NI_Vector128_get_Zero,      SysNumSimdIntrinsicFlag::None)
IDF(Vector3, Max,                       2, NI_AdvSimd_Max,             SysNumSimdIntrinsicFlag::None)
IDF(Vector3, Min,                       2, NI_AdvSimd_Min,             SysNumSimdIntrinsicFlag::None)
IDF(Vector3, op_Addition,               2, NI_AdvSimd_Add,             SysNumSimdIntrinsicFlag::None)
IDF(Vector3, op_Division,               2, NI_AdvSimd_Arm64_Divide,    SysNumSimdIntrinsicFlag::None)
IDF(Vector3, op_Equality,               2, NI_Vector128_op_Equality,   SysNumSimdIntrinsicFlag::None)
IDF(Vector3, op_Inequality,             2, NI_Vector128_op_Inequality, SysNumSimdIntrinsicFlag::None)
IDF(Vector3, op_Multiply,               2, NI_AdvSimd_Multiply,        SysNumSimdIntrinsicFlag::None)
IDF(Vector3, op_Subtraction,            2, NI_AdvSimd_Subtract,        SysNumSimdIntrinsicFlag::None)
IDF(Vector3, SquareRoot,                1, NI_AdvSimd_Arm64_Sqrt,      SysNumSimdIntrinsicFlag::None)

// ISA       ID               Name      NumArg TYP_FLOAT               Flags
// -----------------------------------------------------------------------------------------------------
IDF(Vector4, Abs,                       1, NI_AdvSimd_Abs,             SysNumSimdIntrinsicFlag::None)
IDF(Vector4, CopyTo,                    2, NI_Vector4_CopyTo,          SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector4, CopyToAt,        "CopyTo", 3, NI_Vector4_CopyToAt,        SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector4, CreateBroadcast, ".ctor",  2, NI_Vector4_CreateBroadcast, SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector4, CreateExtend1,   ".ctor",  3, NI_Vector4_CreateExtend1,   SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector4, CreateExtend2,   ".ctor",  4, NI_Vector4_CreateExtend2,   SysNumSimdIntrinsicFlag::HasThis)
NMF(Vector4, Create,          ".ctor",  5, NI_Vector4_Create,          SysNumSimdIntrinsicFlag::HasThis)
IDF(Vector4, Dot,                       2, NI_Vector128_Dot,           SysNumSimdIntrinsicFlag::None)
NMF(Vector4, EqualsInstance,  "Equals", 2, NI_Vector128_op_Equality,   SysNumSimdIntrinsicFlag::HasThis)
IDF(Vector4, get_One,                   0, NI_Vector4_get_One,         SysNumSimdIntrinsicFlag::None)
IDF(Vector4, get_Zero,                  0, NI_Vector128_get_Zero,      SysNumSimdIntrinsicFlag::None)
IDF(Vector4, Max,                       2, NI_AdvSimd_Max,             SysNumSimdIntrinsicFlag::None)
IDF(Vector4, Min,                       2, NI_AdvSimd_Min,             SysNumSimdIntrinsicFlag::None)
IDF(Vector4, op_Addition,               2, NI_AdvSimd_Add,             SysNumSimdIntrinsicFlag::None)
IDF(Vector4, op_Division,               2, NI_AdvSimd_Arm64_Divide,    SysNumSimdIntrinsicFlag::None)
IDF(Vector4, op_Equality,               2, NI_Vector128_op_Equality,   SysNumSimdIntrinsicFlag::None)
IDF(Vector4, op_Inequality,             2, NI_Vector128_op_Inequality, SysNumSimdIntrinsicFlag::None)
IDF(Vector4, op_Multiply,               2, NI_AdvSimd_Multiply,        SysNumSimdIntrinsicFlag::None)
IDF(Vector4, op_Subtraction,            2, NI_AdvSimd_Subtract,        SysNumSimdIntrinsicFlag::None)
IDF(Vector4, SquareRoot,                1, NI_AdvSimd_Arm64_Sqrt,      SysNumSimdIntrinsicFlag::None)

// ISA         ID                  Name      NumArg TYP_BYTE                          TYP_UBYTE                             TYP_SHORT                             TYP_USHORT                            TYP_INT                               TYP_UINT                              TYP_LONG                                    TYP_ULONG                                   TYP_FLOAT                              TYP_DOUBLE                                   Flags
// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ID(VectorT128, Abs,                          1, NI_AdvSimd_Abs,                       NI_VectorT128_Abs,                    NI_AdvSimd_Abs,                       NI_VectorT128_Abs,                    NI_AdvSimd_Abs,                       NI_VectorT128_Abs,                    NI_AdvSimd_Arm64_Abs,                       NI_VectorT128_Abs,                          NI_AdvSimd_Abs,                        NI_AdvSimd_Arm64_Abs,                        SysNumSimdIntrinsicFlag::None)
ID(VectorT128, AndNot,                       2, NI_AdvSimd_BitwiseClear,              NI_AdvSimd_BitwiseClear,              NI_AdvSimd_BitwiseClear,              NI_AdvSimd_BitwiseClear,              NI_AdvSimd_BitwiseClear,              NI_AdvSimd_BitwiseClear,              NI_AdvSimd_BitwiseClear,                    NI_AdvSimd_BitwiseClear,                    NI_AdvSimd_BitwiseClear,               NI_AdvSimd_BitwiseClear,                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, As,                           1, NI_VectorT128_As,                     NI_VectorT128_As,                     NI_VectorT128_As,                     NI_VectorT128_As,                     NI_VectorT128_As,                     NI_VectorT128_As,                     NI_VectorT128_As,                           NI_VectorT128_As,                           NI_VectorT128_As,                      NI_VectorT128_As,                            SysNumSimdIntrinsicFlag::None)
ID(VectorT128, Ceiling,                      1, NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                                 NI_Illegal,                                 NI_AdvSimd_Ceiling,                    NI_AdvSimd_Arm64_Ceiling,                    SysNumSimdIntrinsicFlag::None)
ID(VectorT128, ConditionalSelect,            3, NI_VectorT128_ConditionalSelect,      NI_VectorT128_ConditionalSelect,      NI_VectorT128_ConditionalSelect,      NI_VectorT128_ConditionalSelect,      NI_VectorT128_ConditionalSelect,      NI_VectorT128_ConditionalSelect,      NI_VectorT128_ConditionalSelect,            NI_VectorT128_ConditionalSelect,            NI_VectorT128_ConditionalSelect,       NI_VectorT128_ConditionalSelect,             SysNumSimdIntrinsicFlag::None)
ID(VectorT128, ConvertToInt32,               1, NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                                 NI_Illegal,                                 NI_AdvSimd_ConvertToInt32RoundToZero,  NI_Illegal,                                  SysNumSimdIntrinsicFlag::None)
ID(VectorT128, ConvertToUInt32,              1, NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                                 NI_Illegal,                                 NI_AdvSimd_ConvertToUInt32RoundToZero, NI_Illegal,                                  SysNumSimdIntrinsicFlag::None)
ID(VectorT128, ConvertToInt64,               1, NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                                 NI_Illegal,                                 NI_Illegal,                            NI_AdvSimd_Arm64_ConvertToInt64RoundToZero,  SysNumSimdIntrinsicFlag::None)
ID(VectorT128, ConvertToUInt64,              1, NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                                 NI_Illegal,                                 NI_Illegal,                            NI_AdvSimd_Arm64_ConvertToUInt64RoundToZero, SysNumSimdIntrinsicFlag::None)
ID(VectorT128, ConvertToSingle,              1, NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_AdvSimd_ConvertToSingle,           NI_AdvSimd_ConvertToSingle,           NI_Illegal,                                 NI_Illegal,                                 NI_Illegal,                            NI_Illegal,                                  SysNumSimdIntrinsicFlag::None)
ID(VectorT128, ConvertToDouble,              1, NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_AdvSimd_Arm64_ConvertToDouble,           NI_AdvSimd_Arm64_ConvertToDouble,           NI_Illegal,                            NI_Illegal,                                  SysNumSimdIntrinsicFlag::None)
ID(VectorT128, CopyTo,                       2, NI_VectorT128_CopyTo,                 NI_VectorT128_CopyTo,                 NI_VectorT128_CopyTo,                 NI_VectorT128_CopyTo,                 NI_VectorT128_CopyTo,                 NI_VectorT128_CopyTo,                 NI_VectorT128_CopyTo,                       NI_VectorT128_CopyTo,                       NI_VectorT128_CopyTo,                  NI_VectorT128_CopyTo,                        SysNumSimdIntrinsicFlag::HasThis)
NM(VectorT128, CopyToAt,           "CopyTo", 3, NI_VectorT128_CopyToAt,               NI_VectorT128_CopyToAt,               NI_VectorT128_CopyToAt,               NI_VectorT128_CopyToAt,               NI_VectorT128_CopyToAt,               NI_VectorT128_CopyToAt,               NI_VectorT128_CopyToAt,                     NI_VectorT128_CopyToAt,                     NI_VectorT128_CopyToAt,                NI_VectorT128_CopyToAt,                      SysNumSimdIntrinsicFlag::HasThis)
NM(VectorT128, CreateBroadcast,    ".ctor",  2, NI_VectorT128_CreateBroadcast,        NI_VectorT128_CreateBroadcast,        NI_VectorT128_CreateBroadcast,        NI_VectorT128_CreateBroadcast,        NI_VectorT128_CreateBroadcast,        NI_VectorT128_CreateBroadcast,        NI_VectorT128_CreateBroadcast,              NI_VectorT128_CreateBroadcast,              NI_VectorT128_CreateBroadcast,         NI_VectorT128_CreateBroadcast,               SysNumSimdIntrinsicFlag::HasThis)
NM(VectorT128, FromArray,          ".ctor",  3, NI_VectorT128_FromArray,              NI_VectorT128_FromArray,              NI_VectorT128_FromArray,              NI_VectorT128_FromArray,              NI_VectorT128_FromArray,              NI_VectorT128_FromArray,              NI_VectorT128_FromArray,                    NI_VectorT128_FromArray,                    NI_VectorT128_FromArray,               NI_VectorT128_FromArray,                     SysNumSimdIntrinsicFlag::HasThis)
ID(VectorT128, Dot,                          2, NI_Vector128_Dot,                     NI_Vector128_Dot,                     NI_Vector128_Dot,                     NI_Vector128_Dot,                     NI_Vector128_Dot,                     NI_Vector128_Dot,                     NI_Illegal,                                 NI_Illegal,                                 NI_Vector128_Dot,                      NI_Vector128_Dot,                            SysNumSimdIntrinsicFlag::None)
ID(VectorT128, Equals,                       2, NI_AdvSimd_CompareEqual,              NI_AdvSimd_CompareEqual,              NI_AdvSimd_CompareEqual,              NI_AdvSimd_CompareEqual,              NI_AdvSimd_CompareEqual,              NI_AdvSimd_CompareEqual,              NI_AdvSimd_Arm64_CompareEqual,              NI_AdvSimd_Arm64_CompareEqual,              NI_AdvSimd_CompareEqual,               NI_AdvSimd_Arm64_CompareEqual,               SysNumSimdIntrinsicFlag::None)
NM(VectorT128, EqualsInstance,     "Equals", 2, NI_Vector128_op_Equality,             NI_Vector128_op_Equality,             NI_Vector128_op_Equality,             NI_Vector128_op_Equality,             NI_Vector128_op_Equality,             NI_Vector128_op_Equality,             NI_Vector128_op_Equality,                   NI_Vector128_op_Equality,                   NI_Vector128_op_Equality,              NI_Vector128_op_Equality,                    SysNumSimdIntrinsicFlag::HasThis)
ID(VectorT128, Floor,                        1, NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                                 NI_Illegal,                                 NI_AdvSimd_Floor,                      NI_AdvSimd_Arm64_Floor,                      SysNumSimdIntrinsicFlag::None)
ID(VectorT128, get_AllBitsSet,               0, NI_Vector128_get_AllBitsSet,          NI_Vector128_get_AllBitsSet,          NI_Vector128_get_AllBitsSet,          NI_Vector128_get_AllBitsSet,          NI_Vector128_get_AllBitsSet,          NI_Vector128_get_AllBitsSet,          NI_Vector128_get_AllBitsSet,                NI_Vector128_get_AllBitsSet,                NI_Vector128_get_AllBitsSet,           NI_Vector128_get_AllBitsSet,                 SysNumSimdIntrinsicFlag::None)
ID(VectorT128, get_Count,                    0, NI_VectorT128_get_Count,              NI_VectorT128_get_Count,              NI_VectorT128_get_Count,              NI_VectorT128_get_Count,              NI_VectorT128_get_Count,              NI_VectorT128_get_Count,              NI_VectorT128_get_Count,                    NI_VectorT128_get_Count,                    NI_VectorT128_get_Count,               NI_VectorT128_get_Count,                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, get_IsHardwareAccelerated,    0, NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                                 NI_Illegal,                                 NI_Illegal,                            NI_Illegal,                                  SysNumSimdIntrinsicFlag::None)
ID(VectorT128, get_Item,                     2, NI_VectorT128_get_Item,               NI_VectorT128_get_Item,               NI_VectorT128_get_Item,               NI_VectorT128_get_Item,               NI_VectorT128_get_Item,               NI_VectorT128_get_Item,               NI_VectorT128_get_Item,                     NI_VectorT128_get_Item,                     NI_VectorT128_get_Item,                NI_VectorT128_get_Item,                      SysNumSimdIntrinsicFlag::HasThis)
ID(VectorT128, get_One,                      0, NI_VectorT128_get_One,                NI_VectorT128_get_One,                NI_VectorT128_get_One,                NI_VectorT128_get_One,                NI_VectorT128_get_One,                NI_VectorT128_get_One,                NI_VectorT128_get_One,                      NI_VectorT128_get_One,                      NI_VectorT128_get_One,                 NI_VectorT128_get_One,                       SysNumSimdIntrinsicFlag::None)
ID(VectorT128, get_Zero,                     0, NI_Vector128_get_Zero,                NI_Vector128_get_Zero,                NI_Vector128_get_Zero,                NI_Vector128_get_Zero,                NI_Vector128_get_Zero,                NI_Vector128_get_Zero,                NI_Vector128_get_Zero,                      NI_Vector128_get_Zero,                      NI_Vector128_get_Zero,                 NI_Vector128_get_Zero,                       SysNumSimdIntrinsicFlag::None)
ID(VectorT128, GreaterThan,                  2, NI_AdvSimd_CompareGreaterThan,        NI_AdvSimd_CompareGreaterThan,        NI_AdvSimd_CompareGreaterThan,        NI_AdvSimd_CompareGreaterThan,        NI_AdvSimd_CompareGreaterThan,        NI_AdvSimd_CompareGreaterThan,        NI_AdvSimd_Arm64_CompareGreaterThan,        NI_AdvSimd_Arm64_CompareGreaterThan,        NI_AdvSimd_CompareGreaterThan,         NI_AdvSimd_Arm64_CompareGreaterThan,         SysNumSimdIntrinsicFlag::None)
ID(VectorT128, GreaterThanOrEqual,           2, NI_AdvSimd_CompareGreaterThanOrEqual, NI_AdvSimd_CompareGreaterThanOrEqual, NI_AdvSimd_CompareGreaterThanOrEqual, NI_AdvSimd_CompareGreaterThanOrEqual, NI_AdvSimd_CompareGreaterThanOrEqual, NI_AdvSimd_CompareGreaterThanOrEqual, NI_AdvSimd_Arm64_CompareGreaterThanOrEqual, NI_AdvSimd_Arm64_CompareGreaterThanOrEqual, NI_AdvSimd_CompareGreaterThanOrEqual,  NI_AdvSimd_Arm64_CompareGreaterThanOrEqual,  SysNumSimdIntrinsicFlag::None)
ID(VectorT128, LessThan,                     2, NI_AdvSimd_CompareLessThan,           NI_AdvSimd_CompareLessThan,           NI_AdvSimd_CompareLessThan,           NI_AdvSimd_CompareLessThan,           NI_AdvSimd_CompareLessThan,           NI_AdvSimd_CompareLessThan,           NI_AdvSimd_Arm64_CompareLessThan,           NI_AdvSimd_Arm64_CompareLessThan,           NI_AdvSimd_CompareLessThan,            NI_AdvSimd_Arm64_CompareLessThan,            SysNumSimdIntrinsicFlag::None)
ID(VectorT128, LessThanOrEqual,              2, NI_AdvSimd_CompareLessThanOrEqual,    NI_AdvSimd_CompareLessThanOrEqual,    NI_AdvSimd_CompareLessThanOrEqual,    NI_AdvSimd_CompareLessThanOrEqual,    NI_AdvSimd_CompareLessThanOrEqual,    NI_AdvSimd_CompareLessThanOrEqual,    NI_AdvSimd_Arm64_CompareLessThanOrEqual,    NI_AdvSimd_Arm64_CompareLessThanOrEqual,    NI_AdvSimd_CompareLessThanOrEqual,     NI_AdvSimd_Arm64_CompareLessThanOrEqual,     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, Max,                          2, NI_AdvSimd_Max,                       NI_AdvSimd_Max,                       NI_AdvSimd_Max,                       NI_AdvSimd_Max,                       NI_AdvSimd_Max,                       NI_AdvSimd_Max,                       NI_VectorT128_Max,                          NI_VectorT128_Max,                          NI_AdvSimd_Max,                        NI_AdvSimd_Arm64_Max,                        SysNumSimdIntrinsicFlag::None)
ID(VectorT128, Min,                          2, NI_AdvSimd_Min,                       NI_AdvSimd_Min,                       NI_AdvSimd_Min,                       NI_AdvSimd_Min,                       NI_AdvSimd_Min,                       NI_AdvSimd_Min,                       NI_VectorT128_Min,                          NI_VectorT128_Min,                          NI_AdvSimd_Min,                        NI_AdvSimd_Arm64_Min,                        SysNumSimdIntrinsicFlag::None)
ID(VectorT128, Narrow,                       2, NI_Illegal,                           NI_Illegal,                           NI_VectorT128_Narrow,                 NI_VectorT128_Narrow,                 NI_VectorT128_Narrow,                 NI_VectorT128_Narrow,                 NI_VectorT128_Narrow,                       NI_VectorT128_Narrow,                       NI_Illegal,                            NI_VectorT128_Narrow,                        SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_Addition,                  2, NI_AdvSimd_Add,                       NI_AdvSimd_Add,                       NI_AdvSimd_Add,                       NI_AdvSimd_Add,                       NI_AdvSimd_Add,                       NI_AdvSimd_Add,                       NI_AdvSimd_Add,                             NI_AdvSimd_Add,                             NI_AdvSimd_Add,                        NI_AdvSimd_Arm64_Add,                        SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_BitwiseAnd,                2, NI_AdvSimd_And,                       NI_AdvSimd_And,                       NI_AdvSimd_And,                       NI_AdvSimd_And,                       NI_AdvSimd_And,                       NI_AdvSimd_And,                       NI_AdvSimd_And,                             NI_AdvSimd_And,                             NI_AdvSimd_And,                        NI_AdvSimd_And,                              SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_BitwiseOr,                 2, NI_AdvSimd_Or,                        NI_AdvSimd_Or,                        NI_AdvSimd_Or,                        NI_AdvSimd_Or,                        NI_AdvSimd_Or,                        NI_AdvSimd_Or,                        NI_AdvSimd_Or,                              NI_AdvSimd_Or,                              NI_AdvSimd_Or,                         NI_AdvSimd_Or,                               SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_Division,                  2, NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                                 NI_Illegal,                                 NI_AdvSimd_Arm64_Divide,               NI_AdvSimd_Arm64_Divide,                     SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_Equality,                  2, NI_Vector128_op_Equality,             NI_Vector128_op_Equality,             NI_Vector128_op_Equality,             NI_Vector128_op_Equality,             NI_Vector128_op_Equality,             NI_Vector128_op_Equality,             NI_Vector128_op_Equality,                   NI_Vector128_op_Equality,                   NI_Vector128_op_Equality,              NI_Vector128_op_Equality,                    SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_ExclusiveOr,               2, NI_AdvSimd_Xor,                       NI_AdvSimd_Xor,                       NI_AdvSimd_Xor,                       NI_AdvSimd_Xor,                       NI_AdvSimd_Xor,                       NI_AdvSimd_Xor,                       NI_AdvSimd_Xor,                             NI_AdvSimd_Xor,                             NI_AdvSimd_Xor,                        NI_AdvSimd_Xor,                              SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_Explicit,                  1, NI_VectorT128_op_Explicit,            NI_VectorT128_op_Explicit,            NI_VectorT128_op_Explicit,            NI_VectorT128_op_Explicit,            NI_VectorT128_op_Explicit,            NI_VectorT128_op_Explicit,            NI_VectorT128_op_Explicit,                  NI_VectorT128_op_Explicit,                  NI_VectorT128_op_Explicit,             NI_VectorT128_op_Explicit,                   SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_Inequality,                2, NI_Vector128_op_Inequality,           NI_Vector128_op_Inequality,           NI_Vector128_op_Inequality,           NI_Vector128_op_Inequality,           NI_Vector128_op_Inequality,           NI_Vector128_op_Inequality,           NI_Vector128_op_Inequality,                 NI_Vector128_op_Inequality,                 NI_Vector128_op_Inequality,            NI_Vector128_op_Inequality,                  SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_Multiply,                  2, NI_VectorT128_op_Multiply,            NI_VectorT128_op_Multiply,            NI_VectorT128_op_Multiply,            NI_VectorT128_op_Multiply,            NI_VectorT128_op_Multiply,            NI_VectorT128_op_Multiply,            NI_Illegal,                                 NI_Illegal,                                 NI_VectorT128_op_Multiply,             NI_VectorT128_op_Multiply,                   SysNumSimdIntrinsicFlag::None)
ID(VectorT128, op_Subtraction,               2, NI_AdvSimd_Subtract,                  NI_AdvSimd_Subtract,                  NI_AdvSimd_Subtract,                  NI_AdvSimd_Subtract,                  NI_AdvSimd_Subtract,                  NI_AdvSimd_Subtract,                  NI_AdvSimd_Subtract,                        NI_AdvSimd_Subtract,                        NI_AdvSimd_Subtract,                   NI_AdvSimd_Arm64_Subtract,                   SysNumSimdIntrinsicFlag::None)
ID(VectorT128, SquareRoot,                   1, NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                           NI_Illegal,                                 NI_Illegal,                                 NI_AdvSimd_Arm64_Sqrt,                 NI_AdvSimd_Arm64_Sqrt,                       SysNumSimdIntrinsicFlag::None)
ID(VectorT128, Widen,                        3, NI_VectorT128_Widen,                  NI_VectorT128_Widen,                  NI_VectorT128_Widen,                  NI_VectorT128_Widen,                  NI_VectorT128_Widen,                  NI_VectorT128_Widen,                  NI_Illegal,                                 NI_Illegal,                                 NI_VectorT128_Widen,                   NI_Illegal,                                  SysNumSimdIntrinsicFlag::None)

// clang-format on

#pragma pop_macro("ID")
#pragma pop_macro("IDF")
#pragma pop_macro("NM")
#pragma pop_macro("NMF")

#undef SIMD_AS_HWINTRINSIC
