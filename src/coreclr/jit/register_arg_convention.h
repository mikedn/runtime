// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

struct ParamAllocInfo
{
    unsigned lclNum = 0;

    unsigned size = 0;
#if FEATURE_FASTTAILCALL
    unsigned stackSize = 0;
#endif

    unsigned intRegIndex   = 0;
    unsigned floatRegIndex = 0;
    unsigned intRegCount;
    unsigned floatRegCount;

#ifdef TARGET_ARM
    regMaskTP doubleAlignMask     = RBM_NONE;
    regMaskTP floatAlignPadMask   = RBM_NONE;
    bool      hasFloatStackParams = false;
#endif

public:
    ParamAllocInfo(unsigned intRegCount, unsigned floatRegCount)
        : intRegCount(intRegCount), floatRegCount(floatRegCount)
    {
    }

    unsigned GetRegCount(var_types type) const
    {
        return varTypeUsesFloatArgReg(type) ? floatRegCount : intRegCount;
    }

    unsigned GetAvailableRegCount(var_types type) const
    {
        return GetRegCount(type) - GetRegIndex(type);
    }

    unsigned GetRegIndex(var_types type) const
    {
        return varTypeUsesFloatArgReg(type) ? floatRegIndex : intRegIndex;
    }

    bool CanEnregister(var_types type, unsigned count = 1) const
    {
        return isRegParamType(type) && AreRegsAvailable(type, count);
    }

    regNumber AllocReg(var_types type)
    {
        return genMapRegArgNumToRegNum(AllocRegIndex(type), type);
    }

    regNumber AllocRegs(var_types type, unsigned count)
    {
        return genMapRegArgNumToRegNum(AllocRegIndex(type, count), type);
    }

    unsigned AllocRegIndex(var_types type, unsigned count = 1)
    {
        assert(count > 0);

#ifdef TARGET_ARM
        if (varTypeIsFloating(type) && (count == 1) && (floatAlignPadMask != RBM_NONE))
        {
            regMaskTP regMask = genFindLowestBit(floatAlignPadMask);
            floatAlignPadMask &= ~regMask;
            unsigned regIndex = genMapFloatRegNumToRegArgNum(genRegNumFromMask(regMask));
            assert(regIndex < MAX_FLOAT_REG_ARG);

            return regIndex;
        }
#endif

        unsigned regIndex = GetRegIndex(type);

#ifdef WINDOWS_AMD64_ABI
        NextReg(TYP_INT, count);
        NextReg(TYP_FLOAT, count);
#else
        NextReg(type, count);
#endif

        return regIndex;
    }

#ifdef TARGET_ARM
    unsigned AlignReg(var_types type, unsigned align)
    {
        assert((align == 1) || (align == 2));

        if ((align == 1) || ((GetRegIndex(type) & 1) == 0))
        {
            return 0;
        }

        if (varTypeIsFloating(type))
        {
            floatAlignPadMask |= genMapFloatRegArgNumToRegMask(floatRegIndex);
        }

        assert(GetRegIndex(type) + 1 <= GetRegCount(type));
        RegIndex(type) += 1;

        return 1;
    }

    bool HasFloatStackParams() const
    {
        return hasFloatStackParams;
    }
#endif // TARGET_ARM

#ifdef TARGET_ARMARCH
    void SetHasStackParam(var_types type)
    {
        RegIndex(type) = GetRegCount(type);

#ifdef TARGET_ARM
        if (varTypeIsFloating(type))
        {
            hasFloatStackParams = true;
            floatAlignPadMask   = RBM_NONE;
        }
#endif
    }
#endif // TARGET_ARMARCH

private:
    bool AreRegsAvailable(var_types type, unsigned count = 1) const
    {
        assert(count > 0);

#ifdef TARGET_ARM
        if (varTypeIsFloating(type) && (count == 1) && (floatAlignPadMask != RBM_NONE))
        {
            return GetRegIndex(type) <= GetRegCount(type);
        }
#endif

        return GetRegIndex(type) + count <= GetRegCount(type);
    }

    void NextReg(var_types type, unsigned count = 1)
    {
        RegIndex(type) = min(GetRegIndex(type) + count, GetRegCount(type));
    }

    unsigned& RegIndex(var_types type)
    {
        return varTypeUsesFloatArgReg(type) ? floatRegIndex : intRegIndex;
    }
};
