// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

class JitFlags
{
    uint64_t                    m_jitFlags;
    CORINFO_InstructionSetFlags m_instructionSetFlags;

public:
#define DEF_FLAG(n) JIT_FLAG_##n = CORJIT_FLAGS::CORJIT_FLAG_##n
    enum JitFlag
    {
        DEF_FLAG(SPEED_OPT),
        DEF_FLAG(SIZE_OPT),
        DEF_FLAG(DEBUG_CODE), // generate "debuggable" code (no code-mangling optimizations)
        DEF_FLAG(DEBUG_EnC),  // We are in Edit-n-Continue mode
        DEF_FLAG(DEBUG_INFO), // generate line and local-var info
        DEF_FLAG(MIN_OPT),    // disable all jit optimizations (not necessarily debuggable code)
        DEF_FLAG(OSR),        // Generate alternate version for On Stack Replacement
        DEF_FLAG(TIER0),      // Initial tier for tiered compilation which should generate code as quickly as possible
        DEF_FLAG(TIER1),      // Final tier (for now) for tiered compilation which should generate high quality code
        DEF_FLAG(NO_INLINING),

        DEF_FLAG(ALT_JIT),                // JIT should consider itself an ALT_JIT
        DEF_FLAG(READYTORUN),             // Use version-resilient code generation
        DEF_FLAG(PROF_ENTERLEAVE),        // Instrument prologues/epilogues
        DEF_FLAG(PROF_NO_PINVOKE_INLINE), // Disables PInvoke inlining
        DEF_FLAG(SKIP_VERIFICATION),      // (lazy) skip verification - determined without doing a full resolve.
        DEF_FLAG(PREJIT),                 // jit or prejit is the execution engine.
        DEF_FLAG(RELOC),                  // Generate relocatable code
        DEF_FLAG(IMPORT_ONLY),            // Only import the function
        DEF_FLAG(IL_STUB),                // method is an IL stub
        DEF_FLAG(PROCSPLIT),              // JIT should separate code into hot and cold sections
        DEF_FLAG(BBINSTR),                // Collect basic block profile information
        DEF_FLAG(BBOPT),                  // Optimize method based on profile information
        DEF_FLAG(FRAMED),                 // All methods have an EBP frame
        DEF_FLAG(PUBLISH_SECRET_PARAM),   // JIT must place stub secret param into local 0 (used by IL stubs).
        DEF_FLAG(USE_PINVOKE_HELPERS),    // Use the PINVOKE_{BEGIN,END} helpers instead of emitting inline transitions
        DEF_FLAG(REVERSE_PINVOKE),        // Insert REVERSE_PINVOKE_{ENTER,EXIT} helpers into method prolog/epilog
        DEF_FLAG(TRACK_TRANSITIONS),      // Insert the helper variants that track transitions.

#if defined(TARGET_X86) || defined(TARGET_AMD64) || defined(TARGET_ARM64)
        DEF_FLAG(FEATURE_SIMD),
#endif

#ifdef TARGET_X86
        DEF_FLAG(PINVOKE_RESTORE_ESP), // Restore ESP after returning from inlined PInvoke
        DEF_FLAG(USE_CMOV),            // Generated code may use cmov instruction
#endif

#ifdef TARGET_ARM
        DEF_FLAG(RELATIVE_CODE_RELOCS), // Generate PC-relative address computations instead of EE relocation records
        DEF_FLAG(SOFTFP_ABI),           // On ARM should enable armel calling convention
#endif
    };
#undef DEF_FLAG

    JitFlags(uint64_t flags) : m_jitFlags(flags)
    {
    }

    JitFlags(CORJIT_FLAGS flags) : m_jitFlags(flags.GetFlagsRaw())
    {
        m_instructionSetFlags.SetFromFlagsRaw(flags.GetInstructionSetFlagsRaw());
    }

    CORINFO_InstructionSetFlags GetInstructionSetFlags() const
    {
        return m_instructionSetFlags;
    }

    void SetInstructionSetFlags(CORINFO_InstructionSetFlags instructionSetFlags)
    {
        m_instructionSetFlags = instructionSetFlags;
    }

    void Set(JitFlag flag)
    {
        m_jitFlags |= 1ULL << flag;
    }

    void Clear(JitFlag flag)
    {
        m_jitFlags &= ~(1ULL << flag);
    }

    bool IsSet(JitFlag flag) const
    {
        return (m_jitFlags & (1ULL << flag)) != 0;
    }

    uint64_t GetFlagRaw() const
    {
        return m_jitFlags;
    }
};
