// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_XARCH

#ifndef IF_DEF
#error Must define IF_DEF macro before including this file
#endif

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
//
// For IF_XXX_YYY, the first operand is XXX, the second operand is YYY.

// name                 instruction                          ID_OPS  Memory operand type
//                      scheduling
//                      (unused)

// clang-format off
IF_DEF(NONE,            IS_NONE,                             NONE, NONE)
IF_DEF(GC_REG,          IS_NONE,                             NONE, NONE) // GC reg update
IF_DEF(LABEL,           IS_NONE,                             JMP,  NONE) // label
IF_DEF(RWR_LABEL,       IS_R1_WR,                            JMP,  NONE) // write label address to register

IF_DEF(METHOD,          IS_NONE,                             CALL, NONE) // method call
IF_DEF(METHPTR,         IS_NONE,                             CALL, MRD)  // indirect method call

IF_DEF(CNS,             IS_NONE,                             CNS,  NONE)

//----------------------------------------------------------------------------
// NOTE: The order of the "RD/WR/RW" varieties must match that of the "insUpdateModes" enum.
//----------------------------------------------------------------------------

IF_DEF(RRD,             IS_R1_RD,                            NONE, NONE)
IF_DEF(RWR,             IS_R1_WR,                            NONE, NONE)
IF_DEF(RRW,             IS_R1_RW,                            NONE, NONE)

IF_DEF(RRD_CNS,         IS_R1_RD,                            CNS,  NONE)
IF_DEF(RWR_CNS,         IS_R1_WR,                            CNS,  NONE)
IF_DEF(RRW_CNS,         IS_R1_RW,                            CNS,  NONE)

IF_DEF(RRD_RRD,         IS_R1_RD|IS_R2_RD,                   NONE, NONE)
IF_DEF(RWR_RRD,         IS_R1_WR|IS_R2_RD,                   NONE, NONE)
IF_DEF(RRW_RRD,         IS_R1_RW|IS_R2_RD,                   NONE, NONE)
IF_DEF(RRW_RRW,         IS_R1_RW|IS_R2_RW,                   NONE, NONE) // XCHG reg, reg2
IF_DEF(RRW_RRD_CNS,     IS_R1_RW|IS_R2_RD,                   CNS,  NONE)

IF_DEF(RWR_RRD_RRD,     IS_R1_WR|IS_R2_RD|IS_R3_RD,          NONE, NONE)
IF_DEF(RWR_RRD_RRD_CNS, IS_R1_WR|IS_R2_RD|IS_R3_RD,          CNS,  NONE)

IF_DEF(RWR_RRD_RRD_RRD, IS_R1_WR|IS_R2_RD|IS_R3_RD|IS_R4_RD, CNS,  NONE)

//----------------------------------------------------------------------------
// The following formats are used for direct addresses (e.g. static data members)
//----------------------------------------------------------------------------

IF_DEF(MRD,             IS_GM_RD,                            DSP,     MRD)
IF_DEF(MWR,             IS_GM_WR,                            DSP,     MWR)
IF_DEF(MRW,             IS_GM_RW,                            DSP,     MRW)

IF_DEF(RRD_MRD,         IS_GM_RD|IS_R1_RD,                   DSP,     MRD)
IF_DEF(RWR_MRD,         IS_GM_RD|IS_R1_WR,                   DSP,     MRD)
IF_DEF(RRW_MRD,         IS_GM_RD|IS_R1_RW,                   DSP,     MRD)
IF_DEF(RRW_MRD_CNS,     IS_GM_RD|IS_R1_RW,                   DSP_CNS, MRD)

IF_DEF(RWR_RRD_MRD,     IS_GM_RD|IS_R1_WR|IS_R2_RD,          DSP,     MRD)
IF_DEF(RWR_MRD_CNS,     IS_GM_RD|IS_R1_WR,                   DSP_CNS, MRD)
IF_DEF(RWR_RRD_MRD_CNS, IS_GM_RD|IS_R1_WR|IS_R2_RD,          DSP_CNS, MRD)
IF_DEF(RWR_RRD_MRD_RRD, IS_GM_RD|IS_R1_WR|IS_R2_RD|IS_R3_RD, DSP_CNS, MRD)

IF_DEF(MRD_RRD,         IS_GM_RD|IS_R1_RD,                   DSP,     MRD)
IF_DEF(MWR_RRD,         IS_GM_WR|IS_R1_RD,                   DSP,     MWR)
IF_DEF(MRW_RRD,         IS_GM_RW|IS_R1_RD,                   DSP,     MRW)

IF_DEF(MRD_CNS,         IS_GM_RD,                            DSP_CNS, MRD)
IF_DEF(MWR_CNS,         IS_GM_WR,                            DSP_CNS, MWR)
IF_DEF(MRW_CNS,         IS_GM_RW,                            DSP_CNS, MRW)

IF_DEF(MWR_RRD_CNS,     IS_GM_WR|IS_R1_RD,                   DSP_CNS, MWR)

//----------------------------------------------------------------------------
// The following formats are used for stack frame refs
//----------------------------------------------------------------------------

IF_DEF(SRD,             IS_SF_RD,                            NONE, SRD)
IF_DEF(SWR,             IS_SF_WR,                            NONE, SWR)
IF_DEF(SRW,             IS_SF_RW,                            NONE, SRW)

IF_DEF(RRD_SRD,         IS_SF_RD|IS_R1_RD,                   NONE, SRD)
IF_DEF(RWR_SRD,         IS_SF_RD|IS_R1_WR,                   NONE, SRD)
IF_DEF(RRW_SRD,         IS_SF_RD|IS_R1_RW,                   NONE, SRD)
IF_DEF(RRW_SRD_CNS,     IS_SF_RD|IS_R1_RW,                   CNS,  SRD)

IF_DEF(RWR_RRD_SRD,     IS_SF_RD|IS_R1_WR|IS_R2_RD,          NONE, SRD)
IF_DEF(RWR_SRD_CNS,     IS_SF_RD|IS_R1_WR,                   CNS,  SRD)
IF_DEF(RWR_RRD_SRD_CNS, IS_SF_RD|IS_R1_WR|IS_R2_RD,          CNS,  SRD)
IF_DEF(RWR_RRD_SRD_RRD, IS_SF_RD|IS_R1_WR|IS_R2_RD|IS_R3_RD, CNS,  SRD)

IF_DEF(SRD_RRD,         IS_SF_RD|IS_R1_RD,                   NONE, SRD)
IF_DEF(SWR_RRD,         IS_SF_WR|IS_R1_RD,                   NONE, SWR)
IF_DEF(SRW_RRD,         IS_SF_RW|IS_R1_RD,                   NONE, SRW)

IF_DEF(SRD_CNS,         IS_SF_RD,                            CNS,  SRD)
IF_DEF(SWR_CNS,         IS_SF_WR,                            CNS,  SWR)
IF_DEF(SRW_CNS,         IS_SF_RW,                            CNS,  SRW)

IF_DEF(SWR_RRD_CNS,     IS_AM_WR|IS_R1_RD,                   CNS,  SWR)

//----------------------------------------------------------------------------
// The following formats are used for indirect address modes
//----------------------------------------------------------------------------

IF_DEF(ARD,             IS_AM_RD,                            AMD,     ARD)
IF_DEF(AWR,             IS_AM_WR,                            AMD,     AWR)
IF_DEF(ARW,             IS_AM_RW,                            AMD,     ARW)
                                                                      
IF_DEF(RRD_ARD,         IS_AM_RD|IS_R1_RD,                   AMD,     ARD)
IF_DEF(RWR_ARD,         IS_AM_RD|IS_R1_WR,                   AMD,     ARD)
IF_DEF(RRW_ARD,         IS_AM_RD|IS_R1_RW,                   AMD,     ARD)
IF_DEF(RRW_ARD_CNS,     IS_AM_RD|IS_R1_RW,                   AMD_CNS, ARD)

IF_DEF(RWR_RRD_ARD,     IS_AM_RD|IS_R1_WR|IS_R2_RD,          AMD,     ARD)
IF_DEF(RWR_ARD_CNS,     IS_AM_RD|IS_R1_WR,                   AMD_CNS, ARD)
IF_DEF(RWR_ARD_RRD,     IS_AM_RD|IS_R1_WR|IS_R2_RD,          AMD,     ARD)
IF_DEF(RWR_RRD_ARD_CNS, IS_AM_RD|IS_R1_WR|IS_R2_RD,          AMD_CNS, ARD)
IF_DEF(RWR_RRD_ARD_RRD, IS_AM_RD|IS_R1_WR|IS_R2_RD|IS_R3_RD, AMD_CNS, ARD)

IF_DEF(ARD_RRD,         IS_AM_RD|IS_R1_RD,                   AMD,     ARD)
IF_DEF(AWR_RRD,         IS_AM_WR|IS_R1_RD,                   AMD,     AWR)
IF_DEF(ARW_RRD,         IS_AM_RW|IS_R1_RD,                   AMD,     ARW)

IF_DEF(AWR_RRD_RRD,     IS_AM_WR|IS_R1_RD|IS_R2_RD,          AMD,     AWR)

IF_DEF(ARD_CNS,         IS_AM_RD,                            AMD_CNS, ARD)
IF_DEF(AWR_CNS,         IS_AM_WR,                            AMD_CNS, AWR)
IF_DEF(ARW_CNS,         IS_AM_RW,                            AMD_CNS, ARW)

IF_DEF(AWR_RRD_CNS,     IS_AM_WR|IS_R1_RD,                   AMD_CNS, AWR)
// clang-format on

#undef IF_DEF

#endif // TARGET_XARCH
