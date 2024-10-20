// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//   Automatically generated code. DO NOT MODIFY!
//   To generate this file, do
//        "WeightsArrayGen.pl matrix.txt results.txt > SMWeights.cpp"
//
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#include "jitpch.h"
#include "sm.h"

#define DEFAULT_WEIGHT_VALUE 65 // This is the average of all the weights.

#define NA 9999

const short g_StateWeights[] = {
    NA,                   // state 0
    NA,                   // state 1
    DEFAULT_WEIGHT_VALUE, // state 2 [noshow]
    10,                   // state 3 [ldarg.0]
    16,                   // state 4 [ldarg.1]
    35,                   // state 5 [ldarg.2]
    28,                   // state 6 [ldarg.3]
    12,                   // state 7 [ldloc.0]
    9,                    // state 8 [ldloc.1]
    22,                   // state 9 [ldloc.2]
    24,                   // state 10 [ldloc.3]
    6,                    // state 11 [stloc.0]
    34,                   // state 12 [stloc.1]
    4,                    // state 13 [stloc.2]
    49,                   // state 14 [stloc.3]
    32,                   // state 15 [ldarg.s]
    77,                   // state 16 [ldarga.s]
    21,                   // state 17 [starg.s]
    32,                   // state 18 [ldloc.s]
    61,                   // state 19 [ldloca.s]
    -45,                  // state 20 [stloc.s]
    7,                    // state 21 [ldnull]
    22,                   // state 22 [ldc.i4.m1]
    15,                   // state 23 [ldc.i4.0]
    28,                   // state 24 [ldc.i4.1]
    34,                   // state 25 [ldc.i4.2]
    -6,                   // state 26 [ldc.i4.3]
    20,                   // state 27 [ldc.i4.4]
    4,                    // state 28 [ldc.i4.5]
    10,                   // state 29 [ldc.i4.6]
    56,                   // state 30 [ldc.i4.7]
    42,                   // state 31 [ldc.i4.8]
    41,                   // state 32 [ldc.i4.s]
    38,                   // state 33 [ldc.i4]
    160,                  // state 34 [ldc.i8]
    33,                   // state 35 [ldc.r4]
    113,                  // state 36 [ldc.r8]
    DEFAULT_WEIGHT_VALUE, // state 37 [unused]
    11,                   // state 38 [dup]
    -24,                  // state 39 [pop]
    79,                   // state 40 [call]
    DEFAULT_WEIGHT_VALUE, // state 41 [calli]
    19,                   // state 42 [ret]
    44,                   // state 43 [br.s]
    27,                   // state 44 [brfalse.s]
    25,                   // state 45 [brtrue.s]
    6,                    // state 46 [beq.s]
    20,                   // state 47 [bge.s]
    33,                   // state 48 [bgt.s]
    53,                   // state 49 [ble.s]
    28,                   // state 50 [blt.s]
    12,                   // state 51 [bne.un.s]
    85,                   // state 52 [bge.un.s]
    -52,                  // state 53 [bgt.un.s]
    147,                  // state 54 [ble.un.s]
    -63,                  // state 55 [blt.un.s]
    DEFAULT_WEIGHT_VALUE, // state 56 [long.branch]
    116,                  // state 57 [switch]
    -19,                  // state 58 [ldind.i1]
    17,                   // state 59 [ldind.u1]
    -18,                  // state 60 [ldind.i2]
    10,                   // state 61 [ldind.u2]
    -11,                  // state 62 [ldind.i4]
    -33,                  // state 63 [ldind.u4]
    41,                   // state 64 [ldind.i8]
    -110,                 // state 65 [ldind.i]
    31,                   // state 66 [ldind.r4]
    45,                   // state 67 [ldind.r8]
    1,                    // state 68 [ldind.ref]
    60,                   // state 69 [stind.ref]
    36,                   // state 70 [stind.i1]
    40,                   // state 71 [stind.i2]
    11,                   // state 72 [stind.i4]
    84,                   // state 73 [stind.i8]
    50,                   // state 74 [stind.r4]
    73,                   // state 75 [stind.r8]
    -12,                  // state 76 [add]
    -15,                  // state 77 [sub]
    -9,                   // state 78 [mul]
    35,                   // state 79 [div]
    89,                   // state 80 [div.un]
    89,                   // state 81 [rem]
    82,                   // state 82 [rem.un]
    -5,                   // state 83 [and]
    -7,                   // state 84 [or]
    35,                   // state 85 [xor]
    0,                    // state 86 [shl]
    17,                   // state 87 [shr]
    27,                   // state 88 [shr.un]
    58,                   // state 89 [neg]
    19,                   // state 90 [not]
    78,                   // state 91 [conv.i1]
    54,                   // state 92 [conv.i2]
    2,                    // state 93 [conv.i4]
    99,                   // state 94 [conv.i8]
    273,                  // state 95 [conv.r4]
    197,                  // state 96 [conv.r8]
    45,                   // state 97 [conv.u4]
    55,                   // state 98 [conv.u8]
    83,                   // state 99 [callvirt]
    DEFAULT_WEIGHT_VALUE, // state 100 [cpobj]
    29,                   // state 101 [ldobj]
    66,                   // state 102 [ldstr]
    227,                  // state 103 [newobj]
    261,                  // state 104 [castclass]
    166,                  // state 105 [isinst]
    209,                  // state 106 [conv.r.un]
    DEFAULT_WEIGHT_VALUE, // state 107 [unbox]
    210,                  // state 108 [throw]
    18,                   // state 109 [ldfld]
    17,                   // state 110 [ldflda]
    31,                   // state 111 [stfld]
    159,                  // state 112 [ldsfld]
    177,                  // state 113 [ldsflda]
    125,                  // state 114 [stsfld]
    36,                   // state 115 [stobj]
    148,                  // state 116 [ovf.notype.un]
    247,                  // state 117 [box]
    152,                  // state 118 [newarr]
    7,                    // state 119 [ldlen]
    145,                  // state 120 [ldelema]
    103,                  // state 121 [ldelem.i1]
    91,                   // state 122 [ldelem.u1]
    267,                  // state 123 [ldelem.i2]
    148,                  // state 124 [ldelem.u2]
    92,                   // state 125 [ldelem.i4]
    213,                  // state 126 [ldelem.u4]
    223,                  // state 127 [ldelem.i8]
    DEFAULT_WEIGHT_VALUE, // state 128 [ldelem.i]
    DEFAULT_WEIGHT_VALUE, // state 129 [ldelem.r4]
    549,                  // state 130 [ldelem.r8]
    81,                   // state 131 [ldelem.ref]
    DEFAULT_WEIGHT_VALUE, // state 132 [stelem.i]
    14,                   // state 133 [stelem.i1]
    23,                   // state 134 [stelem.i2]
    66,                   // state 135 [stelem.i4]
    254,                  // state 136 [stelem.i8]
    DEFAULT_WEIGHT_VALUE, // state 137 [stelem.r4]
    DEFAULT_WEIGHT_VALUE, // state 138 [stelem.r8]
    94,                   // state 139 [stelem.ref]
    DEFAULT_WEIGHT_VALUE, // state 140 [ldelem]
    DEFAULT_WEIGHT_VALUE, // state 141 [stelem]
    274,                  // state 142 [unbox.any]
    DEFAULT_WEIGHT_VALUE, // state 143 [conv.ovf.i1]
    DEFAULT_WEIGHT_VALUE, // state 144 [conv.ovf.u1]
    DEFAULT_WEIGHT_VALUE, // state 145 [conv.ovf.i2]
    DEFAULT_WEIGHT_VALUE, // state 146 [conv.ovf.u2]
    242,                  // state 147 [conv.ovf.i4]
    DEFAULT_WEIGHT_VALUE, // state 148 [conv.ovf.u4]
    293,                  // state 149 [conv.ovf.i8]
    293,                  // state 150 [conv.ovf.u8]
    DEFAULT_WEIGHT_VALUE, // state 151 [refanyval]
    DEFAULT_WEIGHT_VALUE, // state 152 [ckfinite]
    -17,                  // state 153 [mkrefany]
    32,                   // state 154 [ldtoken]
    25,                   // state 155 [conv.u2]
    50,                   // state 156 [conv.u1]
    -0,                   // state 157 [conv.i]
    178,                  // state 158 [conv.ovf.i]
    DEFAULT_WEIGHT_VALUE, // state 159 [conv.ovf.u]
    DEFAULT_WEIGHT_VALUE, // state 160 [add.ovf]
    DEFAULT_WEIGHT_VALUE, // state 161 [mul.ovf]
    DEFAULT_WEIGHT_VALUE, // state 162 [sub.ovf]
    -17,                  // state 163 [leave.s]
    182,                  // state 164 [stind.i]
    -36,                  // state 165 [conv.u]
    DEFAULT_WEIGHT_VALUE, // state 166 [prefix.n]
    120,                  // state 167 [arglist]
    20,                   // state 168 [ceq]
    -1,                   // state 169 [cgt]
    47,                   // state 170 [cgt.un]
    26,                   // state 171 [clt]
    85,                   // state 172 [clt.un]
    102,                  // state 173 [ldftn]
    234,                  // state 174 [ldvirtftn]
    DEFAULT_WEIGHT_VALUE, // state 175 [long.loc.arg]
    347,                  // state 176 [localloc]
    DEFAULT_WEIGHT_VALUE, // state 177 [unaligned]
    -44,                  // state 178 [volatile]
    DEFAULT_WEIGHT_VALUE, // state 179 [tailcall]
    55,                   // state 180 [initobj]
    DEFAULT_WEIGHT_VALUE, // state 181 [constrained]
    DEFAULT_WEIGHT_VALUE, // state 182 [cpblk]
    DEFAULT_WEIGHT_VALUE, // state 183 [initblk]
    DEFAULT_WEIGHT_VALUE, // state 184 [rethrow]
    38,                   // state 185 [sizeof]
    -68,                  // state 186 [refanytype]
    DEFAULT_WEIGHT_VALUE, // state 187 [readonly]
    55,                   // state 188 [ldarga.s.normed]
    35,                   // state 189 [ldloca.s.normed]
    161,                  // state 190 [constrained -> callvirt]
    31,                   // state 191 [ldarg.0 -> ldfld]
    29,                   // state 192 [ldarg.1 -> ldfld]
    22,                   // state 193 [ldarg.2 -> ldfld]
    321,                  // state 194 [ldarg.3 -> ldfld]
    46,                   // state 195 [ldarga.s -> ldfld]
    8,                    // state 196 [ldloca.s -> ldfld]
    19,                   // state 197 [ldarga.s.normed -> ldfld]
    -35,                  // state 198 [ldloca.s.normed -> ldfld]
    20,                   // state 199 [stloc.0 -> ldloc.0]
    -7,                   // state 200 [stloc.1 -> ldloc.1]
    -10,                  // state 201 [stloc.2 -> ldloc.2]
    -4,                   // state 202 [stloc.3 -> ldloc.3]
    DEFAULT_WEIGHT_VALUE, // state 203 [ldc.r4 -> add]
    DEFAULT_WEIGHT_VALUE, // state 204 [ldc.r4 -> sub]
    DEFAULT_WEIGHT_VALUE, // state 205 [ldc.r4 -> mul]
    DEFAULT_WEIGHT_VALUE, // state 206 [ldc.r4 -> div]
    52,                   // state 207 [ldc.r8 -> add]
    DEFAULT_WEIGHT_VALUE, // state 208 [ldc.r8 -> sub]
    -169,                 // state 209 [ldc.r8 -> mul]
    -17,                  // state 210 [ldc.r8 -> div]
    DEFAULT_WEIGHT_VALUE, // state 211 [conv.r4 -> add]
    DEFAULT_WEIGHT_VALUE, // state 212 [conv.r4 -> sub]
    DEFAULT_WEIGHT_VALUE, // state 213 [conv.r4 -> mul]
    DEFAULT_WEIGHT_VALUE, // state 214 [conv.r4 -> div]
    358,                  // state 215 [conv.r8 -> mul]
    DEFAULT_WEIGHT_VALUE, // state 216 [conv.r8 -> div]
    NA,                   // state 217
    32,                   // state 218 [ldarg.0 -> ldc.i4.0 -> stfld]
    NA,                   // state 219
    DEFAULT_WEIGHT_VALUE, // state 220 [ldarg.0 -> ldc.r4 -> stfld]
    NA,                   // state 221
    38,                   // state 222 [ldarg.0 -> ldc.r8 -> stfld]
    NA,                   // state 223
    NA,                   // state 224
    64,                   // state 225 [ldarg.0 -> ldarg.1 -> ldfld -> stfld]
    69,                   // state 226 [ldarg.0 -> ldarg.1 -> stfld]
    NA,                   // state 227
    98,                   // state 228 [ldarg.0 -> ldarg.2 -> stfld]
    NA,                   // state 229
    97,                   // state 230 [ldarg.0 -> ldarg.3 -> stfld]
    NA,                   // state 231
    NA,                   // state 232
    NA,                   // state 233
    NA,                   // state 234
    34,                   // state 235 [ldarg.0 -> dup -> ldfld -> ldarg.1 -> add -> stfld]
    NA,                   // state 236
    -10,                  // state 237 [ldarg.0 -> dup -> ldfld -> ldarg.1 -> sub -> stfld]
    NA,                   // state 238
    DEFAULT_WEIGHT_VALUE, // state 239 [ldarg.0 -> dup -> ldfld -> ldarg.1 -> mul -> stfld]
    NA,                   // state 240
    DEFAULT_WEIGHT_VALUE, // state 241 [ldarg.0 -> dup -> ldfld -> ldarg.1 -> div -> stfld]
    NA,                   // state 242
    NA,                   // state 243
    DEFAULT_WEIGHT_VALUE, // state 244 [ldarg.0 -> ldfld -> ldarg.1 -> ldfld -> add]
    DEFAULT_WEIGHT_VALUE, // state 245 [ldarg.0 -> ldfld -> ldarg.1 -> ldfld -> sub]
    NA,                   // state 246
    NA,                   // state 247
    DEFAULT_WEIGHT_VALUE, // state 248 [ldarga.s -> ldfld -> ldarga.s -> ldfld -> add]
    DEFAULT_WEIGHT_VALUE, // state 249 [ldarga.s -> ldfld -> ldarga.s -> ldfld -> sub]
};

static_assert_no_msg(NUM_SM_STATES == _countof(g_StateWeights));

const short* gp_StateWeights = g_StateWeights;
