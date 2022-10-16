// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

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
    unsigned stackArgSize = 0;
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
    unsigned allocRegArg(var_types type, unsigned numRegs = 1)
    {
        assert(numRegs > 0);

        unsigned resultArgNum = regArgNum(type);

#ifdef TARGET_ARM
        // Check for back-filling
        if (varTypeIsFloating(type) && !anyFloatStackArgs && (numRegs == 1) && (fltArgSkippedRegMask != RBM_NONE))
        {
            // We will never back-fill something greater than a single register
            // (TYP_FLOAT, or TYP_STRUCT HFA with a single float). This is because
            // we don't have any types that require > 2 register alignment, so we
            // can't create a > 1 register alignment hole to back-fill.

            // Back-fill the register
            regMaskTP backFillBitMask = genFindLowestBit(fltArgSkippedRegMask);
            fltArgSkippedRegMask &= ~backFillBitMask;
            resultArgNum = genMapFloatRegNumToRegArgNum(genRegNumFromMask(backFillBitMask));
            assert(resultArgNum < MAX_FLOAT_REG_ARG);

            return resultArgNum;
        }
#endif

#ifdef WINDOWS_AMD64_ABI
        nextReg(TYP_INT, numRegs);
        nextReg(TYP_FLOAT, numRegs);
#else
        nextReg(type, numRegs);
#endif

        return resultArgNum;
    }

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
    unsigned alignReg(var_types type, unsigned requiredRegAlignment)
    {
        assert(requiredRegAlignment > 0);
        if (requiredRegAlignment == 1)
        {
            return 0; // Everything is always "1" aligned
        }

        assert(requiredRegAlignment == 2); // we don't expect anything else right now

        int alignMask = regArgNum(type) & (requiredRegAlignment - 1);
        if (alignMask == 0)
        {
            return 0; // We're already aligned
        }

        unsigned cAlignSkipped = requiredRegAlignment - alignMask;
        assert(cAlignSkipped == 1); // Alignment is currently only 1 or 2, so misalignment can only be 1.

        if (varTypeIsFloating(type))
        {
            fltArgSkippedRegMask |= genMapFloatRegArgNumToRegMask(floatRegArgNum);
        }

        // if equal, then we aligned the last slot, and the arg can't be enregistered
        assert(regArgNum(type) + cAlignSkipped <= maxRegArgNum(type));
        regArgNum(type) += cAlignSkipped;

        return cAlignSkipped;
    }
#endif // TARGET_ARM

    // Return true if it is an enregisterable type and there is room.
    // Note that for "type", we only care if it is float or not. In particular,
    // "numRegs" must be "2" to allocate an ARM double-precision floating-point register.
    bool canEnreg(var_types type, unsigned numRegs = 1)
    {
        return isRegParamType(type) && enoughAvailRegs(type, numRegs);
    }

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

    bool enoughAvailRegs(var_types type, unsigned numRegs = 1)
    {
        assert(numRegs > 0);

#ifdef TARGET_ARM
        // Check for back-filling
        if (varTypeIsFloating(type) && !anyFloatStackArgs && (numRegs == 1) && (fltArgSkippedRegMask != RBM_NONE))
        {
            return regArgNum(type) <= maxRegArgNum(type);
        }
#endif

        return regArgNum(type) + numRegs <= maxRegArgNum(type);
    }

    void nextReg(var_types type, unsigned numRegs = 1)
    {
        regArgNum(type) = min(regArgNum(type) + numRegs, maxRegArgNum(type));
    }
};
