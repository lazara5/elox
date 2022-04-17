#ifndef SLOX_COMMON_H
#define SLOX_COMMON_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

//#define DEBUG_PRINT_CODE
//#define DEBUG_TRACE_EXECUTION
//#define DEBUG_STRESS_GC
//#define DEBUG_LOG_GC

//#define ENABLE_DYNAMIC_STACK
//#define ENABLE_NAN_BOXING
//#define ENABLE_COMPUTED_GOTO

#define UINT8_COUNT  (UINT8_MAX + 1)
#define UINT16_COUNT (UINT16_MAX + 1)

#if !defined(SLOX_PRINTF)
#if defined(__GNUC__)
#define SLOX_PRINTF(n, m) __attribute__ ((format(printf, n, m)))
#else
#define SLOX_PRINTF(n, m)
#endif // __GNUC__
#endif // SLOX_PRINTF

#if !defined(SLOX_UNUSED)
#if defined(__GNUC__)
#define SLOX_UNUSED __attribute__((__unused__))
#else
#define SLOX_UNUSED
#endif // __GNUC__
#endif // SLOX_UNUSED

#if !defined(SLOX_UNLIKELY)
#if defined(__GNUC__)
#define SLOX_UNLIKELY(x) __builtin_expect((x), 0)
#else
#define SLOX_UNLIKELY(x) (x)
#endif // __GNUC__
#endif // SLOX_UNLIKELY

#if !defined(SLOX_LIKELY)
#if defined(__GNUC__)
#define SLOX_LIKELY(x) __builtin_expect((x), 1)
#else
#define SLOX_LIKELY(x) (x)
#endif // __GNUC__
#endif // SLOX_LIKELY

#endif //SLOX_COMMON_H
