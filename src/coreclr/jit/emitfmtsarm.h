// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_ARM

#ifndef IF_DEF
#error Must define IF_DEF macro before including this file
#endif

// insFormat  instruction  ID_OPS
//            scheduling
//            (unused)

// clang-format off
IF_DEF(NONE,     IS_NONE, NONE)

IF_DEF(GC_REG,   IS_NONE, NONE) // GC reg update
IF_DEF(LARGEJMP, IS_NONE, JMP)  // large conditional branch pseudo-op

/////////////////////////////////////////////////////////////////////////////////////////////////////////

IF_DEF(T1_A,     IS_NONE, NONE) // ................   
IF_DEF(T1_B,     IS_NONE, NONE) // ........cccc....               cond
IF_DEF(T1_C,     IS_NONE, NONE) // .....iiiiimmmddd   R1  R2      imm5
IF_DEF(T1_D0,    IS_NONE, NONE) // ........Dmmmmddd   R1* R2*
IF_DEF(T1_D1,    IS_NONE, SPEC) // .........mmmm...   R1*
IF_DEF(T1_D2,    IS_NONE, SPEC) // .........mmmm...           R3*
IF_DEF(T1_E,     IS_NONE, NONE) // ..........mmmddd   R1  R2
IF_DEF(T1_F,     IS_NONE, NONE) // .........iiiiiii   SP          imm7
IF_DEF(T1_G,     IS_NONE, NONE) // .......iiinnnddd   R1  R2      imm3
IF_DEF(T1_H,     IS_NONE, NONE) // .......mmmnnnddd   R1  R2  R3
IF_DEF(T1_I,     IS_NONE, JMP ) // ......i.iiiiinnn   R1          imm6
IF_DEF(T1_J0,    IS_NONE, NONE) // .....dddiiiiiiii   R1          imm8
IF_DEF(T1_J1,    IS_NONE, NONE) // .....dddiiiiiiii   R1          <regmask8>
IF_DEF(T1_J2,    IS_NONE, NONE) // .....dddiiiiiiii   R1  SP      imm8
IF_DEF(T1_J3,    IS_NONE, NONE) // .....dddiiiiiiii   R1  PC      imm8
IF_DEF(T1_K,     IS_NONE, JMP ) // ....cccciiiiiiii   Branch      imm8, cond4
IF_DEF(T1_L0,    IS_NONE, NONE) // ........iiiiiiii               imm8
IF_DEF(T1_L1,    IS_NONE, NONE) // ........rrrrrrrr               <regmask8>
IF_DEF(T1_M,     IS_NONE, JMP ) // .....iiiiiiiiiii   Branch      imm11

IF_DEF(T2_A,     IS_NONE, NONE) // ................ ................
IF_DEF(T2_B,     IS_NONE, NONE) // ................ ............iiii                      imm4
IF_DEF(T2_C0,    IS_NONE, NONE) // ...........Snnnn .iiiddddiishmmmm   R1  R2  R3      S, imm5, sh
IF_DEF(T2_C1,    IS_NONE, NONE) // ...........S.... .iiiddddiishmmmm   R1  R2          S, imm5, sh
IF_DEF(T2_C2,    IS_NONE, NONE) // ...........S.... .iiiddddii..mmmm   R1  R2          S, imm5
IF_DEF(T2_C3,    IS_NONE, NONE) // ...........S.... ....dddd....mmmm   R1  R2          S
IF_DEF(T2_C4,    IS_NONE, NONE) // ...........Snnnn ....dddd....mmmm   R1  R2  R3      S
IF_DEF(T2_C5,    IS_NONE, NONE) // ............nnnn ....dddd....mmmm   R1  R2  R3
IF_DEF(T2_C6,    IS_NONE, NONE) // ................ ....dddd..iimmmm   R1  R2                   imm2
IF_DEF(T2_C7,    IS_NONE, NONE) // ............nnnn ..........shmmmm   R1  R2                   imm2
IF_DEF(T2_C8,    IS_NONE, NONE) // ............nnnn .iii....iishmmmm   R1  R2             imm5, sh
IF_DEF(T2_C9,    IS_NONE, NONE) // ............nnnn ............mmmm   R1  R2
IF_DEF(T2_C10,   IS_NONE, NONE) // ............mmmm ....dddd....mmmm   R1  R2
IF_DEF(T2_D0,    IS_NONE, NONE) // ............nnnn .iiiddddii.wwwww   R1  R2             imm5, imm5
IF_DEF(T2_D1,    IS_NONE, NONE) // ................ .iiiddddii.wwwww   R1                 imm5, imm5
IF_DEF(T2_E0,    IS_NONE, NONE) // ............nnnn tttt......shmmmm   R1  R2  R3               imm2
IF_DEF(T2_E1,    IS_NONE, NONE) // ............nnnn tttt............   R1  R2
IF_DEF(T2_E2,    IS_NONE, NONE) // ................ tttt............   R1
IF_DEF(T2_F1,    IS_NONE, NONE) // ............nnnn ttttdddd....mmmm   R1  R2  R3  R4
IF_DEF(T2_F2,    IS_NONE, NONE) // ............nnnn aaaadddd....mmmm   R1  R2  R3  R4
IF_DEF(T2_G0,    IS_NONE, NONE) // .......PU.W.nnnn ttttTTTTiiiiiiii   R1  R2  R3         imm8, PUW
IF_DEF(T2_G1,    IS_NONE, NONE) // ............nnnn ttttTTTT........   R1  R2  R3
IF_DEF(T2_H0,    IS_NONE, NONE) // ............nnnn tttt.PUWiiiiiiii   R1  R2             imm8, PUW
IF_DEF(T2_H1,    IS_NONE, NONE) // ............nnnn tttt....iiiiiiii   R1  R2             imm8
IF_DEF(T2_H2,    IS_NONE, NONE) // ............nnnn ........iiiiiiii   R1                 imm8
IF_DEF(T2_I0,    IS_NONE, NONE) // ..........W.nnnn rrrrrrrrrrrrrrrr   R1              W, imm16
IF_DEF(T2_I1,    IS_NONE, NONE) // ................ rrrrrrrrrrrrrrrr                      imm16
IF_DEF(T2_J1,    IS_NONE, JMP ) // .....Scccciiiiii ..j.jiiiiiiiiiii   Branch             imm20, cond4
IF_DEF(T2_J2,    IS_NONE, JMP ) // .....Siiiiiiiiii ..j.jiiiiiiiiii.   Branch             imm24
IF_DEF(T2_J3,    IS_NONE, CALL) // .....Siiiiiiiiii ..j.jiiiiiiiiii.   Call               imm24
IF_DEF(T2_K1,    IS_NONE, NONE) // ............nnnn ttttiiiiiiiiiiii   R1  R2             imm12
IF_DEF(T2_K2,    IS_NONE, NONE) // ............nnnn ....iiiiiiiiiiii   R1                 imm12
IF_DEF(T2_K3,    IS_NONE, NONE) // ........U....... ....iiiiiiiiiiii   PC              U, imm12
IF_DEF(T2_K4,    IS_NONE, NONE) // ........U....... ttttiiiiiiiiiiii   R1  PC          U, imm12
IF_DEF(T2_L0,    IS_NONE, NONE) // .....i.....Snnnn .iiiddddiiiiiiii   R1  R2          S, imm8<<imm4
IF_DEF(T2_L1,    IS_NONE, NONE) // .....i.....S.... .iiiddddiiiiiiii   R1              S, imm8<<imm4
IF_DEF(T2_L2,    IS_NONE, NONE) // .....i......nnnn .iii....iiiiiiii   R1                 imm8<<imm4
IF_DEF(T2_M0,    IS_NONE, NONE) // .....i......nnnn .iiiddddiiiiiiii   R1  R2             imm12
IF_DEF(T2_M1,    IS_NONE, NONE) // .....i.......... .iiiddddiiiiiiii   R1  PC             imm12
IF_DEF(T2_N,     IS_NONE, NONE) // .....i......iiii .iiiddddiiiiiiii   R1                 imm16    ; movw/movt
IF_DEF(T2_N1,    IS_NONE, JMP)  // .....i......iiii .iiiddddiiiiiiii   R1                 imm16    ; movw/movt of a code address
IF_DEF(T2_N2,    IS_NONE, NONE) // .....i......iiii .iiiddddiiiiiiii   R1                 imm16    ; movw/movt of a data address
IF_DEF(T2_N3,    IS_NONE, NONE) // .....i......iiii .iiiddddiiiiiiii   R1                 imm16    ; movw/movt (relocatable imm)
IF_DEF(T2_VLDST, IS_NONE, NONE) // 11101101UD0Lnnnn dddd101Ziiiiiiii   D1  R2             imm(+-1020)
IF_DEF(T2_VFP2,  IS_NONE, NONE) // 111011101D110--- dddd101Z--M0mmmm   D1  D2
IF_DEF(T2_VFP3,  IS_NONE, NONE) // 11101110-D--nnnn dddd101ZN-M0mmmm   D1  D2  D3
IF_DEF(T2_VMOVS, IS_NONE, NONE)
IF_DEF(T2_VMOVD, IS_NONE, NONE)
// clang-format on

#undef IF_DEF

#endif // TARGET_ARM
