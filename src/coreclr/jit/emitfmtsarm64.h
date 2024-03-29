// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_ARM64

#ifndef IF_DEF
#error Must define IF_DEF macro before including this file
#endif

// insFormat  instruction  ID_OPS
//            scheduling
//            (unused)

// clang-format off
IF_DEF(NONE, IS_NONE, NONE) //

IF_DEF(GC_REG, IS_NONE, NONE)  // GC reg update

IF_DEF(NOP_JMP,  IS_NONE, JMP) // nop branch (distance = 0)
IF_DEF(LARGEJMP, IS_NONE, JMP) // large conditional branch pseudo-op (cond branch + uncond branch)
IF_DEF(LARGEADR, IS_NONE, JMP) // large address pseudo-op (adrp + add)
IF_DEF(LARGELDC, IS_NONE, JMP) // large constant pseudo-op (adrp + ldr)
IF_DEF(SMALLADR, IS_NONE, JMP) // small address pseudo-op (adr)

// Key for insFormat names:
//
// Above (Specifies multiple encodings)
//
//   EN#? ::  (count of the number of encodings)
//            (? is a unique letter A,B,C...)
//
// Below  (Specifies an exact instruction encoding)
//
//       -- the first two characters are
//
//   DI  :: Data Processing - Immediate
//   DR  :: Data Processing - Register
//   DV  :: Data Processing - Vector Register
//   LS  :: Loads and Stores
//   BI  :: Branches - Immediate
//   BR  :: Branches - Register
//   SN  :: System - No Registers or Immediates
//   SI  :: System - Immediate
//   SR  :: System - Register
//
//   _   :: a separator char '_'
//
//       -- the next two characters are
//
//   #   :: number of registers in the encoding
//   ?   :: A unique letter A,B,C,...
//       -- optional third character
//   I   :: by element immediate

IF_DEF(BI_0A, IS_NONE, JMP)  // ......iiiiiiiiii iiiiiiiiiiiiiiii           simm26:00   b
IF_DEF(BI_0B, IS_NONE, JMP)  // ......iiiiiiiiii iiiiiiiiiii.....           simm19:00   b<cond>
IF_DEF(BI_0C, IS_NONE, CALL) // ......iiiiiiiiii iiiiiiiiiiiiiiii           simm26:00   bl
IF_DEF(BI_1A, IS_NONE, JMP)  // X.......iiiiiiii iiiiiiiiiiittttt  Rt       simm19:00   cbz cbnz
IF_DEF(BI_1B, IS_NONE, JMP)  // B.......bbbbbiii iiiiiiiiiiittttt  Rt imm6  simm14:00   tbz tbnz
IF_DEF(BR_1A, IS_NONE, CALL) // ................ ......nnnnn.....     Rn                ret
IF_DEF(BR_1B, IS_NONE, CALL) // ................ ......nnnnn.....     Rn                br blr

IF_DEF(LS_1A, IS_NONE, JMP)  // XX...V..iiiiiiii iiiiiiiiiiittttt  Rt    PC imm(1MB)
IF_DEF(LS_2A, IS_NONE, NONE) // .X.......X...... ......nnnnnttttt  Rt Rn
IF_DEF(LS_2B, IS_NONE, NONE) // .X.......Xiiiiii iiiiiinnnnnttttt  Rt Rn    imm(0-4095)
IF_DEF(LS_2C, IS_NONE, NONE) // .X.......X.iiiii iiiiP.nnnnnttttt  Rt Rn    imm(-256..+255) pre/post inc
IF_DEF(LS_2D, IS_NONE, NONE) // .Q.............. ....ssnnnnnttttt  Vt Rn    Load/Store multiple structures       base register
                             //                                             Load single structure and replicate  base register
IF_DEF(LS_2E, IS_NONE, NONE) // .Q.............. ....ssnnnnnttttt  Vt Rn    Load/Store multiple structures       post-indexed by an immediate
                             //                                             Load single structure and replicate  post-indexed by an immediate
IF_DEF(LS_2F, IS_NONE, NONE) // .Q.............. xx.Sssnnnnnttttt  Vt[] Rn  Load/Store single structure          base register
IF_DEF(LS_2G, IS_NONE, NONE) // .Q.............. xx.Sssnnnnnttttt  Vt[] Rn  Load/Store single structure          post-indexed by an immediate
IF_DEF(LS_3A, IS_NONE, NONE) // .X.......X.mmmmm xxxS..nnnnnttttt  Rt Rn Rm ext(Rm) LSL {}
IF_DEF(LS_3B, IS_NONE, NONE) // X............... .aaaaannnnnddddd  Rd Ra Rn
IF_DEF(LS_3C, IS_NONE, NONE) // X.........iiiiii iaaaaannnnnddddd  Rd Ra Rn imm(im7,sh)
IF_DEF(LS_3D, IS_NONE, NONE) // .X.......X.mmmmm ......nnnnnttttt  Wm Rt Rn
IF_DEF(LS_3E, IS_NONE, NONE) // .X.........mmmmm ......nnnnnttttt  Rm Rt Rn ARMv8.1 LSE Atomics
IF_DEF(LS_3F, IS_NONE, NONE) // .Q.........mmmmm ....ssnnnnnttttt  Vt Rn Rm   Load/Store multiple structures       post-indexed by a register
                             //                                               Load single structure and replicate  post-indexed by a register
IF_DEF(LS_3G, IS_NONE, NONE) // .Q.........mmmmm ...Sssnnnnnttttt  Vt[] Rn Rm Load/Store single structure          post-indexed by a register

IF_DEF(DI_1A, IS_NONE, NONE) // X.......shiiiiii iiiiiinnnnn.....     Rn    imm(i12,sh)
IF_DEF(DI_1B, IS_NONE, NONE) // X........hwiiiii iiiiiiiiiiiddddd  Rd       imm(i16,hw)
IF_DEF(DI_1C, IS_NONE, NONE) // X........Nrrrrrr ssssssnnnnn.....     Rn    imm(N,r,s)
IF_DEF(DI_1D, IS_NONE, NONE) // X........Nrrrrrr ssssss.....ddddd  Rd       imm(N,r,s)
IF_DEF(DI_1E, IS_NONE, NONE) // .ii.....iiiiiiii iiiiiiiiiiiddddd  Rd       simm21
IF_DEF(DI_1F, IS_NONE, NONE) // X..........iiiii cccc..nnnnn.nzcv  Rn imm5  nzcv cond

IF_DEF(DI_2A, IS_NONE, NONE) // X.......shiiiiii iiiiiinnnnnddddd  Rd Rn    imm(i12,sh)
IF_DEF(DI_2B, IS_NONE, NONE) // X.........Xnnnnn ssssssnnnnnddddd  Rd Rn    imm(0-63)
IF_DEF(DI_2C, IS_NONE, NONE) // X........Nrrrrrr ssssssnnnnnddddd  Rd Rn    imm(N,r,s)
IF_DEF(DI_2D, IS_NONE, NONE) // X........Nrrrrrr ssssssnnnnnddddd  Rd Rn    imr, imms   (N,r,s)

IF_DEF(DR_1D, IS_NONE, NONE) // X............... cccc.......ddddd  Rd       cond

IF_DEF(DR_2A, IS_NONE, NONE) // X..........mmmmm ......nnnnn.....     Rn Rm
IF_DEF(DR_2B, IS_NONE, NONE) // X.......sh.mmmmm ssssssnnnnn.....     Rn Rm {LSL,LSR,ASR} imm(0-63)
IF_DEF(DR_2C, IS_NONE, NONE) // X..........mmmmm xxxsssnnnnn.....     Rn Rm ext(Rm) LSL imm(0-4)
IF_DEF(DR_2D, IS_NONE, NONE) // X..........nnnnn cccc..nnnnnddddd  Rd Rn    cond
IF_DEF(DR_2E, IS_NONE, NONE) // X..........mmmmm ...........ddddd  Rd    Rm
IF_DEF(DR_2F, IS_NONE, NONE) // X.......sh.mmmmm ssssss.....ddddd  Rd    Rm {LSL,LSR,ASR} imm(0-63)
IF_DEF(DR_2G, IS_NONE, NONE) // X............... ......nnnnnddddd  Rd Rn
IF_DEF(DR_2H, IS_NONE, NONE) // X........X...... ......nnnnnddddd  Rd Rn
IF_DEF(DR_2I, IS_NONE, NONE) // X..........mmmmm cccc..nnnnn.nzcv  Rn Rm    nzcv cond

IF_DEF(DR_3A, IS_NONE, NONE) // X..........mmmmm ......nnnnnddddd  Rd Rn Rm
IF_DEF(DR_3B, IS_NONE, NONE) // X.......sh.mmmmm ssssssnnnnnddddd  Rd Rn Rm {LSL,LSR,ASR} imm(0-63)
IF_DEF(DR_3C, IS_NONE, NONE) // X..........mmmmm xxxsssnnnnnddddd  Rd Rn Rm ext(Rm) LSL imm(0-4)
IF_DEF(DR_3D, IS_NONE, NONE) // X..........mmmmm cccc..nnnnnddddd  Rd Rn Rm cond
IF_DEF(DR_3E, IS_NONE, NONE) // X........X.mmmmm ssssssnnnnnddddd  Rd Rn Rm imm(0-63)

IF_DEF(DR_4A, IS_NONE, NONE) // X..........mmmmm .aaaaannnnnddddd  Rd Rn Rm Ra

IF_DEF(DV_1A, IS_NONE, NONE) // .........X.iiiii iii........ddddd  Vd imm8    (fmov - immediate scalar)
IF_DEF(DV_1B, IS_NONE, NONE) // .QX..........iii jjjj..iiiiiddddd  Vd imm8    (fmov/movi - immediate vector)
IF_DEF(DV_1C, IS_NONE, NONE) // .........X...... ......nnnnn.....  Vn #0.0    (fcmp - with zero)

IF_DEF(DV_2A, IS_NONE, NONE) // .Q.......X...... ......nnnnnddddd  Vd Vn      (fabs, fcvtXX - vector)
IF_DEF(DV_2B, IS_NONE, NONE) // .Q.........iiiii ......nnnnnddddd  Rd Vn[]    (umov/smov    - to general)
IF_DEF(DV_2C, IS_NONE, NONE) // .Q.........iiiii ......nnnnnddddd  Vd Rn      (dup/ins - vector from general)
IF_DEF(DV_2D, IS_NONE, NONE) // .Q.........iiiii ......nnnnnddddd  Vd Vn[]    (dup - vector)
IF_DEF(DV_2E, IS_NONE, NONE) // ...........iiiii ......nnnnnddddd  Vd Vn[]    (dup - scalar)
IF_DEF(DV_2F, IS_NONE, NONE) // ...........iiiii .jjjj.nnnnnddddd  Vd[] Vn[]  (ins - element)
IF_DEF(DV_2G, IS_NONE, NONE) // .........X...... ......nnnnnddddd  Vd Vn      (fmov, fcvtXX - register)
IF_DEF(DV_2H, IS_NONE, NONE) // X........X...... ......nnnnnddddd  Rd Vn      (fmov, fcvtXX - to general)
IF_DEF(DV_2I, IS_NONE, NONE) // X........X...... ......nnnnnddddd  Vd Rn      (fmov, fcvtXX - from general)
IF_DEF(DV_2J, IS_NONE, NONE) // .........d...... D.....nnnnnddddd  Vd Vn      (fcvt)
IF_DEF(DV_2K, IS_NONE, NONE) // .........X.mmmmm ......nnnnn.....  Vn Vm      (fcmp)
IF_DEF(DV_2L, IS_NONE, NONE) // ........XX...... ......nnnnnddddd  Vd Vn      (abs, neg - scalar)
IF_DEF(DV_2M, IS_NONE, NONE) // .Q......XX...... ......nnnnnddddd  Vd Vn      (abs, neg - vector)
IF_DEF(DV_2N, IS_NONE, NONE) // .........iiiiiii ......nnnnnddddd  Vd Vn imm  (shift - scalar)
IF_DEF(DV_2O, IS_NONE, NONE) // .Q.......iiiiiii ......nnnnnddddd  Vd Vn imm  (shift - vector)
IF_DEF(DV_2P, IS_NONE, NONE) // ................ ......nnnnnddddd  Vd Vn      (Vd used as both source and destination)
IF_DEF(DV_2Q, IS_NONE, NONE) // .........X...... ......nnnnnddddd  Sd Vn      (faddp, fmaxnmp, fmaxp, fminnmp, fminp - scalar)
IF_DEF(DV_2R, IS_NONE, NONE) // .Q.......X...... ......nnnnnddddd  Sd Vn      (fmaxnmv, fmaxv, fminnmv, fminv)
IF_DEF(DV_2S, IS_NONE, NONE) // ........XX...... ......nnnnnddddd  Sd Vn      (addp - scalar)
IF_DEF(DV_2T, IS_NONE, NONE) // .Q......XX...... ......nnnnnddddd  Sd Vn      (addv, saddlv, smaxv, sminv, uaddlv, umaxv, uminv)
IF_DEF(DV_2U, IS_NONE, NONE) // ................ ......nnnnnddddd  Sd Sn      (sha1h)

IF_DEF(DV_3A,  IS_NONE, NONE) //.Q......XX.mmmmm ......nnnnnddddd  Vd Vn Vm   (vector)
IF_DEF(DV_3AI, IS_NONE, NONE) //.Q......XXLMmmmm ....H.nnnnnddddd  Vd Vn Vm[] (vector by element)
IF_DEF(DV_3B,  IS_NONE, NONE) //.Q.......X.mmmmm ......nnnnnddddd  Vd Vn Vm   (vector)
IF_DEF(DV_3BI, IS_NONE, NONE) //.Q.......XLmmmmm ....H.nnnnnddddd  Vd Vn Vm[] (vector by element)
IF_DEF(DV_3C,  IS_NONE, NONE) //.Q.........mmmmm ......nnnnnddddd  Vd Vn Vm   (vector)
IF_DEF(DV_3D,  IS_NONE, NONE) //.........X.mmmmm ......nnnnnddddd  Vd Vn Vm   (scalar)
IF_DEF(DV_3DI, IS_NONE, NONE) //.........XLmmmmm ....H.nnnnnddddd  Vd Vn Vm[] (scalar by element)
IF_DEF(DV_3E,  IS_NONE, NONE) //........XX.mmmmm ......nnnnnddddd  Vd Vn Vm   (scalar)
IF_DEF(DV_3EI, IS_NONE, NONE) //........XXLMmmmm ....H.nnnnnddddd  Vd Vn Vm[] (scalar by element)
IF_DEF(DV_3F,  IS_NONE, NONE) //...........mmmmm ......nnnnnddddd  Qd Sn Vm   (Qd used as both source and destination)
IF_DEF(DV_3G,  IS_NONE, NONE) // .Q.........mmmmm .iiii.nnnnnddddd Vd Vn Vm imm (vector)

IF_DEF(DV_4A,  IS_NONE, NONE) //.........X.mmmmm .aaaaannnnnddddd  Vd Vn Vm Va (scalar)

IF_DEF(SN_0A, IS_NONE, NONE) // ................ ................
IF_DEF(SI_0A, IS_NONE, NONE) // ...........iiiii iiiiiiiiiii.....           imm16
IF_DEF(SI_0B, IS_NONE, NONE) // ................ ....bbbb........           imm4 - barrier

IF_DEF(SR_1A, IS_NONE, NONE) // ................ ...........ttttt  Rt       (dc zva)
// clang-format on

#undef IF_DEF

#endif // TARGET_ARM64
