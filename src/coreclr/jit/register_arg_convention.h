// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef __register_arg_convention__
#define __register_arg_convention__

struct InitVarDscInfo
{
    unsigned size           = 0;
    unsigned varNum         = 0;
    unsigned intRegArgNum   = 0;
    unsigned floatRegArgNum = 0;
    unsigned maxIntRegArgNum;
    unsigned maxFloatRegArgNum;

#ifdef TARGET_ARM
    regMaskTP doubleAlignMask      = RBM_NONE;
    regMaskTP fltArgSkippedRegMask = RBM_NONE;
    bool      anyFloatStackArgs    = false;
#endif

#if FEATURE_FASTTAILCALL
    // It is used to calculate argument stack size information in byte
    unsigned stackArgSize       = 0;
    bool     hasMultiSlotStruct = false;
#endif

public:
    InitVarDscInfo(unsigned maxIntRegArgNum, unsigned maxFloatRegArgNum)
        : maxIntRegArgNum(maxIntRegArgNum), maxFloatRegArgNum(maxFloatRegArgNum)
    {
    }

    // return ref to current register arg for this type
    unsigned& regArgNum(var_types type)
    {
        return varTypeUsesFloatArgReg(type) ? floatRegArgNum : intRegArgNum;
    }

    unsigned regArgNum(var_types type) const
    {
        return varTypeUsesFloatArgReg(type) ? floatRegArgNum : intRegArgNum;
    }

    // Allocate a set of contiguous argument registers. "type" is either an integer
    // type, indicating to use the integer registers, or a floating-point type, indicating
    // to use the floating-point registers. The actual type (TYP_FLOAT vs. TYP_DOUBLE) is
    // ignored. "numRegs" is the number of registers to allocate. Thus, on ARM, to allocate
    // a double-precision floating-point register, you need to pass numRegs=2. For an HFA,
    // pass the number of slots/registers needed.
    // This routine handles floating-point register back-filling on ARM.
    // Returns the first argument register of the allocated set.
    unsigned allocRegArg(var_types type, unsigned numRegs = 1);

    regNumber AllocReg(var_types type)
    {
        return genMapRegArgNumToRegNum(allocRegArg(type, 1), type);
    }

    regNumber AllocRegs(var_types type, unsigned count)
    {
        return genMapRegArgNumToRegNum(allocRegArg(type, count), type);
    }

#ifdef TARGET_ARM
    // We are aligning the register to an ABI-required boundary, such as putting
    // double-precision floats in even-numbered registers, by skipping one register.
    // "requiredRegAlignment" is the amount to align to: 1 for no alignment (everything
    // is 1-aligned), 2 for "double" alignment.
    // Returns the number of registers skipped.
    unsigned alignReg(var_types type, unsigned requiredRegAlignment);
#endif // TARGET_ARM

    // Return true if it is an enregisterable type and there is room.
    // Note that for "type", we only care if it is float or not. In particular,
    // "numRegs" must be "2" to allocate an ARM double-precision floating-point register.
    bool canEnreg(var_types type, unsigned numRegs = 1);

#ifdef TARGET_ARMARCH
    void SetAllRegsUsed(var_types type)
    {
        regArgNum(type) = maxRegArgNum(type);
    }

#ifdef TARGET_ARM
    void SetHasFloatStackParams()
    {
        anyFloatStackArgs = true;
    }

    bool HasFloatStackParams()
    {
        return anyFloatStackArgs;
    }
#endif // TARGET_ARM
#endif // TARGET_ARMARCH

    unsigned GetAvailableRegCount(var_types type) const
    {
        return maxRegArgNum(type) - regArgNum(type);
    }

private:
    // return max register arg for this type
    unsigned maxRegArgNum(var_types type) const
    {
        return varTypeUsesFloatArgReg(type) ? maxFloatRegArgNum : maxIntRegArgNum;
    }

    bool enoughAvailRegs(var_types type, unsigned numRegs = 1);

    void nextReg(var_types type, unsigned numRegs = 1)
    {
        regArgNum(type) = min(regArgNum(type) + numRegs, maxRegArgNum(type));
    }
};

#endif // __register_arg_convention__
