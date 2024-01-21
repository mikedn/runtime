// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef CC_DEF
#error Must define CC_DEF macro before including this file
#endif

// clang-format off
#ifdef TARGET_XARCH
//     cc  reverse
CC_DEF(o , no)
CC_DEF(no, o )
CC_DEF(b , ae)
CC_DEF(ae, b )
CC_DEF(e , ne)
CC_DEF(ne, e )
CC_DEF(be, a )
CC_DEF(a , be)
CC_DEF(s , ns)
CC_DEF(ns, s )
CC_DEF(p , np)
CC_DEF(np, p )
CC_DEF(l , ge)
CC_DEF(ge, l )
CC_DEF(le, g )
CC_DEF(g , le)
#endif

#ifdef TARGET_ARMARCH
//     cc  reverse condcode
CC_DEF(eq, ne, EQ)
CC_DEF(ne, eq, NE)
CC_DEF(hs, lo, HS)
CC_DEF(lo, hs, LO)
CC_DEF(mi, pl, MI)
CC_DEF(pl, mi, PL)
CC_DEF(vs, vc, VS)
CC_DEF(vc, vs, VC)
CC_DEF(hi, ls, HI)
CC_DEF(ls, hi, LS)
CC_DEF(ge, lt, GE)
CC_DEF(lt, ge, LT)
CC_DEF(gt, le, GT)
CC_DEF(le, gt, LE)
#endif
// clang-format on

#undef CC_DEF
