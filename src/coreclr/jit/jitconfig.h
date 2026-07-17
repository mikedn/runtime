// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "switches.h"

struct CORINFO_SIG_INFO;
class ICorJitHost;

class JitConfigValues
{
public:
    class MethodSet
    {
    private:
        struct MethodName
        {
            MethodName* m_next;
            int         m_methodNameStart;
            int         m_methodNameLen;
            bool        m_methodNameWildcardAtEnd;
            int         m_classNameStart;
            int         m_classNameLen;
            bool        m_classNameWildcardAtEnd;
            int         m_numArgs;
        };

        char*       m_list;
        MethodName* m_names;

    public:
        MethodSet()                  = default;
        MethodSet(MethodSet&& other) = delete;

        const char* list() const
        {
            return m_list;
        }

        void initialize(const WCHAR* list, ICorJitHost* host);
        void destroy(ICorJitHost* host);

        bool isEmpty() const
        {
            return m_names == nullptr;
        }

        bool contains(const char* methodName, const char* className, CORINFO_SIG_INFO* sigInfo) const;
    };

private:
#define CONFIG_BOOL(name, ...) bool           m_##name;
#define CONFIG_INT(name, ...) int             m_##name;
#define CONFIG_UNSIGNED(name, ...) unsigned   m_##name;
#define CONFIG_DOUBLE(name, ...) double       m_##name;
#define CONFIG_STRING(name, ...) const WCHAR* m_##name;
#define CONFIG_METHODSET(name, ...) MethodSet m_##name;
#include "jitconfigvalues.h"

public:
#define CONFIG_BOOL(name, ...)                                                                                         \
    bool name() const                                                                                                  \
    {                                                                                                                  \
        return m_##name;                                                                                               \
    }
#define CONFIG_INT(name, ...)                                                                                          \
    int name() const                                                                                                   \
    {                                                                                                                  \
        return m_##name;                                                                                               \
    }
#define CONFIG_UNSIGNED(name, ...)                                                                                     \
    unsigned name() const                                                                                              \
    {                                                                                                                  \
        return m_##name;                                                                                               \
    }
#define CONFIG_DOUBLE(name, ...)                                                                                       \
    double name() const                                                                                                \
    {                                                                                                                  \
        return m_##name;                                                                                               \
    }
#define CONFIG_STRING(name, ...)                                                                                       \
    const WCHAR* name() const                                                                                          \
    {                                                                                                                  \
        return m_##name;                                                                                               \
    }
#define CONFIG_METHODSET(name, ...)                                                                                    \
    const MethodSet& name() const                                                                                      \
    {                                                                                                                  \
        return m_##name;                                                                                               \
    }

#define CONFIG_DECL
#include "jitconfigvalues.h"
#undef CONFIG_DECL

private:
    bool m_isInitialized;

public:
    JitConfigValues()                        = default;
    JitConfigValues(JitConfigValues&& other) = delete;

    bool isInitialized() const
    {
        return m_isInitialized != 0;
    }

    void initialize(ICorJitHost* host);
    void destroy(ICorJitHost* host);
};

extern JitConfigValues JitConfig;

#ifdef DEBUG
unsigned ReinterpretHexAsDecimal(unsigned in);
#endif
