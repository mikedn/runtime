// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef _SIMD_H_
#define _SIMD_H_

#ifdef FEATURE_SIMD
#ifdef TARGET_XARCH
// SSE2 Shuffle control byte to shuffle vector <W, Z, Y, X>
// These correspond to shuffle immediate byte in shufps SSE2 instruction.
#define SHUFFLE_XXXX 0x00 // 00 00 00 00
#define SHUFFLE_XXZX 0x08 // 00 00 10 00
#define SHUFFLE_XXWW 0x0F // 00 00 11 11
#define SHUFFLE_XYZW 0x1B // 00 01 10 11
#define SHUFFLE_YXYX 0x44 // 01 00 01 00
#define SHUFFLE_YWXZ 0x72 // 01 11 00 10
#define SHUFFLE_YYZZ 0x5A // 01 01 10 10
#define SHUFFLE_ZXXX 0x80 // 10 00 00 00
#define SHUFFLE_ZXXY 0x81 // 10 00 00 01
#define SHUFFLE_ZWXY 0xB1 // 10 11 00 01
#define SHUFFLE_WYZX 0xD8 // 11 01 10 00
#define SHUFFLE_WWYY 0xF5 // 11 11 01 01
#define SHUFFLE_ZZXX 0xA0 // 10 10 00 00
#endif

#endif // FEATURE_SIMD
#endif //_SIMD_H_
