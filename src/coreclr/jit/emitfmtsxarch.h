// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_XARCH

#ifndef IF_DEF
#error Must define IF_DEF macro before including this file
#endif

//////////////////////////////////////////////////////////////////////////////
//
// A note on the naming convention for instruction forms (IF_xxxxx).
// For 3-character code XYY, generally we have:
//      X =
//          R - register
//          M - memory
//          S - stack
//          A - address mode
//      YY =
//          RD - read
//          WR - write
//          RW - read / write
//
// The following sequences don't follow this pattern:
//      XYY =
//          CNS - constant
//          SHF - shift-constant
//
// For IF_XXX_YYY, the first operand is XXX, the second operand is YYY.
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// enum insFormat   instruction            enum ID_OPS
//                  scheduling
//                  (unused)
//////////////////////////////////////////////////////////////////////////////

// clang-format off
IF_DEF(NONE,        IS_NONE,                    NONE)     // no operands
IF_DEF(GC_REG,      IS_NONE,                    NONE)     // GC reg update
IF_DEF(LABEL,       IS_NONE,                    JMP )     // label
IF_DEF(RWR_LABEL,   IS_R1_WR,                   JMP )     // write label to register

IF_DEF(METHOD,      IS_NONE,                    CALL)     // method
IF_DEF(METHPTR,     IS_NONE,                    CALL)     // method ptr (glbl)

IF_DEF(CNS,         IS_NONE,                    CNS )     // const

//----------------------------------------------------------------------------
// NOTE: The order of the "RD/WR/RW" varieties must match that of the "insUpdateModes" enum.
//----------------------------------------------------------------------------

IF_DEF(RRD,         IS_R1_RD,                   NONE)     // read   reg
IF_DEF(RWR,         IS_R1_WR,                   NONE)     // write  reg
IF_DEF(RRW,         IS_R1_RW,                   NONE)     // r/w    reg

IF_DEF(RRD_CNS,     IS_R1_RD,                   CNS )     // read   reg , const
IF_DEF(RWR_CNS,     IS_R1_WR,                   CNS )     // write  reg , const
IF_DEF(RRW_CNS,     IS_R1_RW,                   CNS )     // r/w    reg , const

IF_DEF(RRD_RRD,     IS_R1_RD|IS_R2_RD,          NONE)     // read   reg , read reg2
IF_DEF(RWR_RRD,     IS_R1_WR|IS_R2_RD,          NONE)     // write  reg , read reg2
IF_DEF(RRW_RRD,     IS_R1_RW|IS_R2_RD,          NONE)     // r/w    reg , read reg2
IF_DEF(RRW_RRW,     IS_R1_RW|IS_R2_RW,          NONE)     // r/w    reg , r/w reg2 - for XCHG reg, reg2
IF_DEF(RRW_RRD_CNS, IS_R1_RW|IS_R2_RD,          CNS )     // r/w    reg , read reg2 , const

IF_DEF(RWR_RRD_RRD, IS_R1_WR|IS_R2_RD|IS_R3_RD, NONE)     // write  reg , read reg2 , read reg3
IF_DEF(RWR_RRD_RRD_CNS, IS_R1_WR|IS_R2_RD|IS_R3_RD, CNS)  // write  reg , read reg2 , read reg3, const

IF_DEF(RWR_RRD_RRD_RRD, IS_R1_WR|IS_R2_RD|IS_R3_RD|IS_R4_RD, CNS)     // write  reg , read reg2 , read reg3 , read reg4

//----------------------------------------------------------------------------
// The following formats are used for direct addresses (e.g. static data members)
//----------------------------------------------------------------------------

IF_DEF(MRD,         IS_GM_RD,                   DSP)      // read  [mem]
IF_DEF(MWR,         IS_GM_WR,                   DSP)      // write [mem]
IF_DEF(MRW,         IS_GM_RW,                   DSP)      // r/w   [mem]

IF_DEF(RRD_MRD,     IS_GM_RD|IS_R1_RD,          DSP)      // read   reg , read [mem]
IF_DEF(RWR_MRD,     IS_GM_RD|IS_R1_WR,          DSP)      // write  reg , read [mem]
IF_DEF(RRW_MRD,     IS_GM_RD|IS_R1_RW,          DSP)      // r/w    reg , read [mem]
IF_DEF(RRW_MRD_CNS, IS_GM_RD|IS_R1_RW,          DSP_CNS)  // r/w    reg , read [mem], const

IF_DEF(RWR_RRD_MRD, IS_GM_RD|IS_R1_WR|IS_R2_RD, DSP)      // write  reg , read reg2 , read [mem]
IF_DEF(RWR_MRD_CNS, IS_GM_RD|IS_R1_WR,          DSP_CNS)  // write  reg , read [mem], const
IF_DEF(RWR_RRD_MRD_CNS, IS_GM_RD|IS_R1_WR|IS_R2_RD, DSP_CNS) // write  reg , read reg2 , read [mem], const
IF_DEF(RWR_RRD_MRD_RRD, IS_GM_RD|IS_R1_WR|IS_R2_RD|IS_R3_RD, DSP_CNS) // write  reg , read reg2 , read [mem], read reg3

IF_DEF(MRD_RRD,     IS_GM_RD|IS_R1_RD,          DSP)      // read  [mem], read  reg
IF_DEF(MWR_RRD,     IS_GM_WR|IS_R1_RD,          DSP)      // write [mem], read  reg
IF_DEF(MRW_RRD,     IS_GM_RW|IS_R1_RD,          DSP)      // r/w   [mem], read  reg

IF_DEF(MRD_CNS,     IS_GM_RD,                   DSP_CNS)  // read  [mem], const
IF_DEF(MWR_CNS,     IS_GM_WR,                   DSP_CNS)  // write [mem], const
IF_DEF(MRW_CNS,     IS_GM_RW,                   DSP_CNS)  // r/w   [mem], const

IF_DEF(MWR_RRD_CNS, IS_GM_WR|IS_R1_RD,          DSP_CNS)  // write [mem], read reg, const

//----------------------------------------------------------------------------
// The following formats are used for stack frame refs
//----------------------------------------------------------------------------

IF_DEF(SRD,         IS_SF_RD,                   NONE)     // read  [stk]
IF_DEF(SWR,         IS_SF_WR,                   NONE)     // write [stk]
IF_DEF(SRW,         IS_SF_RW,                   NONE)     // r/w   [stk]

IF_DEF(RRD_SRD,     IS_SF_RD|IS_R1_RD,          NONE)     // read   reg , read [stk]
IF_DEF(RWR_SRD,     IS_SF_RD|IS_R1_WR,          NONE)     // write  reg , read [stk]
IF_DEF(RRW_SRD,     IS_SF_RD|IS_R1_RW,          NONE)     // r/w    reg , read [stk]
IF_DEF(RRW_SRD_CNS, IS_SF_RD|IS_R1_RW,          CNS )     // r/w    reg , read [stk], const

IF_DEF(RWR_RRD_SRD, IS_SF_RD|IS_R1_WR|IS_R2_RD, NONE)     // write  reg , read  reg2, read [stk]
IF_DEF(RWR_SRD_CNS, IS_SF_RD|IS_R1_WR,          CNS )     // write  reg , read [stk], const
IF_DEF(RWR_RRD_SRD_CNS, IS_SF_RD|IS_R1_WR|IS_R2_RD, CNS ) // write  reg , read  reg2, read [stk], const
IF_DEF(RWR_RRD_SRD_RRD, IS_SF_RD|IS_R1_WR|IS_R2_RD|IS_R3_RD, CNS ) // write  reg , read  reg2, read [stk], read reg3

IF_DEF(SRD_RRD,     IS_SF_RD|IS_R1_RD,          NONE)     // read  [stk], read  reg
IF_DEF(SWR_RRD,     IS_SF_WR|IS_R1_RD,          NONE)     // write [stk], read  reg
IF_DEF(SRW_RRD,     IS_SF_RW|IS_R1_RD,          NONE)     // r/w   [stk], read  reg

IF_DEF(SRD_CNS,     IS_SF_RD,                   CNS )     // read  [stk], const
IF_DEF(SWR_CNS,     IS_SF_WR,                   CNS )     // write [stk], const
IF_DEF(SRW_CNS,     IS_SF_RW,                   CNS )     // r/w   [stk], const

IF_DEF(SWR_RRD_CNS, IS_AM_WR|IS_R1_RD,          CNS )     // write [stk], read reg, const

//----------------------------------------------------------------------------
// The following formats are used for indirect address modes
//----------------------------------------------------------------------------

IF_DEF(ARD,         IS_AM_RD,                   AMD )     // read  [adr]
IF_DEF(AWR,         IS_AM_WR,                   AMD )     // write [adr]
IF_DEF(ARW,         IS_AM_RW,                   AMD )     // r/w   [adr]

IF_DEF(RRD_ARD,     IS_AM_RD|IS_R1_RD,          AMD )     // read   reg , read [adr]
IF_DEF(RWR_ARD,     IS_AM_RD|IS_R1_WR,          AMD )     // write  reg , read [adr]
IF_DEF(RRW_ARD,     IS_AM_RD|IS_R1_RW,          AMD )     // r/w    reg , read [adr]
IF_DEF(RRW_ARD_CNS, IS_AM_RD|IS_R1_RW,          AMD_CNS)  // r/w    reg , read [adr], const

IF_DEF(RWR_RRD_ARD, IS_AM_RD|IS_R1_WR|IS_R2_RD, AMD )     // write  reg , read  reg2, read [adr]
IF_DEF(RWR_ARD_CNS, IS_AM_RD|IS_R1_WR,          AMD_CNS)  // write  reg , read [adr], const
IF_DEF(RWR_ARD_RRD, IS_AM_RD|IS_R1_WR|IS_R2_RD, AMD)      // write  reg , read [adr], read reg2
IF_DEF(RWR_RRD_ARD_CNS, IS_AM_RD|IS_R1_WR|IS_R2_RD, AMD_CNS) // write  reg , read  reg2, read [adr], const
IF_DEF(RWR_RRD_ARD_RRD, IS_AM_RD|IS_R1_WR|IS_R2_RD|IS_R3_RD, AMD_CNS) // write  reg , read  reg2, read [adr], read reg3

IF_DEF(ARD_RRD,     IS_AM_RD|IS_R1_RD,          AMD )     // read  [adr], read  reg
IF_DEF(AWR_RRD,     IS_AM_WR|IS_R1_RD,          AMD )     // write [adr], read  reg
IF_DEF(ARW_RRD,     IS_AM_RW|IS_R1_RD,          AMD )     // r/w   [adr], read  reg

IF_DEF(AWR_RRD_RRD, IS_AM_WR|IS_R1_RD|IS_R2_RD, AMD )     // write  [adr], read  reg, read  reg

IF_DEF(ARD_CNS,     IS_AM_RD,                   AMD_CNS)  // read  [adr], const
IF_DEF(AWR_CNS,     IS_AM_WR,                   AMD_CNS)  // write [adr], const
IF_DEF(ARW_CNS,     IS_AM_RW,                   AMD_CNS)  // r/w   [adr], const

IF_DEF(AWR_RRD_CNS, IS_AM_WR|IS_R1_RD,          AMD_CNS)  // write [adr], read reg, const
// clang-format on

#undef IF_DEF

#endif // TARGET_XARCH
