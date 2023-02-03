#ifndef ELOX_UTIL_H
#define ELOX_UTIL_H

#include "elox.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define UINT8_COUNT  (UINT8_MAX + 1)
#define UINT16_COUNT (UINT16_MAX + 1)

#define JOIN(A, B) JOIN_(A, B)
#define JOIN_(A, B) A##B

// FOREACH macro
// based on https://stackoverflow.com/a/14735113

#define EXPAND(x) x
#define FOR_EACH_1(what, x, ...) what(x)
#define FOR_EACH_2(what, x, ...) what(x) EXPAND(FOR_EACH_1(what, __VA_ARGS__))
#define FOR_EACH_3(what, x, ...) what(x) EXPAND(FOR_EACH_2(what, __VA_ARGS__))
#define FOR_EACH_4(what, x, ...) what(x) EXPAND(FOR_EACH_3(what, __VA_ARGS__))
#define FOR_EACH_5(what, x, ...) what(x) EXPAND(FOR_EACH_4(what, __VA_ARGS__))
#define FOR_EACH_6(what, x, ...) what(x) EXPAND(FOR_EACH_5(what, __VA_ARGS__))
#define FOR_EACH_7(what, x, ...) what(x) EXPAND(FOR_EACH_6(what, __VA_ARGS__))
#define FOR_EACH_8(what, x, ...) what(x) EXPAND(FOR_EACH_7(what, __VA_ARGS__))
#define FOR_EACH_9(what, x, ...) what(x) EXPAND(FOR_EACH_8(what, __VA_ARGS__))
#define FOR_EACH_10(what, x, ...) what(x) EXPAND(FOR_EACH_9(what, __VA_ARGS__))
#define FOR_EACH_11(what, x, ...) what(x) EXPAND(FOR_EACH_10(what, __VA_ARGS__))
#define FOR_EACH_12(what, x, ...) what(x) EXPAND(FOR_EACH_11(what, __VA_ARGS__))
#define FOR_EACH_13(what, x, ...) what(x) EXPAND(FOR_EACH_12(what, __VA_ARGS__))
#define FOR_EACH_14(what, x, ...) what(x) EXPAND(FOR_EACH_13(what, __VA_ARGS__))
#define FOR_EACH_15(what, x, ...) what(x) EXPAND(FOR_EACH_14(what, __VA_ARGS__))
#define FOR_EACH_16(what, x, ...) what(x) EXPAND(FOR_EACH_15(what, __VA_ARGS__))
#define FOR_EACH_17(what, x, ...) what(x) EXPAND(FOR_EACH_16(what, __VA_ARGS__))
#define FOR_EACH_18(what, x, ...) what(x) EXPAND(FOR_EACH_17(what, __VA_ARGS__))
#define FOR_EACH_19(what, x, ...) what(x) EXPAND(FOR_EACH_18(what, __VA_ARGS__))
#define FOR_EACH_20(what, x, ...) what(x) EXPAND(FOR_EACH_19(what, __VA_ARGS__))
#define FOR_EACH_NARG(...) FOR_EACH_NARG_(__VA_ARGS__, FOR_EACH_RSEQ_N())
#define FOR_EACH_NARG_(...) EXPAND(FOR_EACH_ARG_N(__VA_ARGS__))
#define FOR_EACH_ARG_N(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, \
	_13, _14, _15, _16, _17, _18, _19, _20, N, ...) N
#define FOR_EACH_RSEQ_N() 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0
#define FOR_EACH_(N, what, ...) EXPAND(JOIN_(FOR_EACH_, N)(what, __VA_ARGS__))
#define FOR_EACH(what, ...) FOR_EACH_(FOR_EACH_NARG(__VA_ARGS__), what, __VA_ARGS__)

#if !defined(ELOX_PRINTF)
#if defined(__GNUC__)
#define ELOX_PRINTF(n, m) __attribute__ ((format(printf, n, m)))
#else
#define ELOX_PRINTF(n, m)
#endif // __GNUC__
#endif // ELOX_PRINTF

#if !defined(ELOX_UNUSED)
#if defined(__GNUC__)
#define ELOX_UNUSED __attribute__((__unused__))
#else
#define ELOX_UNUSED
#endif // __GNUC__
#endif // ELOX_UNUSED

#if !defined(ELOX_UNLIKELY)
#if defined(__GNUC__)
#define ELOX_UNLIKELY(x) __builtin_expect((x), 0)
#else
#define ELOX_UNLIKELY(x) (x)
#endif // __GNUC__
#endif // ELOX_UNLIKELY

#if !defined(ELOX_LIKELY)
#if defined(__GNUC__)
#define ELOX_LIKELY(x) __builtin_expect((x), 1)
#else
#define ELOX_LIKELY(x) (x)
#endif // __GNUC__
#endif // ELOX_LIKELY

#if !defined(ELOX_UNREACHABLE)
#if defined (__GNUC__) // GCC 4.8+, Clang, Intel and other compilers compatible with GCC (-std=c++0x or above)
#define ELOX_UNREACHABLE() __builtin_unreachable()
#elif defined(_MSC_VER) // MSVC
#define ELOX_UNREACHABLE() __assume(false)
#else // ???
#define ELOX_UNREACHABLE() ;
#endif // __GNUC__
#endif // ELOX_UNREACHABLE

#if !defined(ELOX_FORCE_INLINE)
#if defined(__GNUC__)
#define ELOX_FORCE_INLINE __attribute__((always_inline)) inline
#elif defined(_MSC_VER_)
#define ELOX_FORCE_INLINE __forceinline
#else
#define ELOX_FORCE_INLINE
#endif // __GNUC__
#endif // ELOX_FORCE_INLINE

#if !defined(ELOX_PACKED)
#if defined(__GNUC__)
#define ELOX_PACKED __attribute__((__packed__))
#else
#define ELOX_PACKED
#endif // __GNUC__
#endif // ELOX_PACKED

#define ELOX_UNCONST(ptr) ((void *)(uintptr_t)(const void *)(ptr))

#define ELOX_ARRAY_SIZE(arr) ((sizeof(arr)/sizeof(0[arr])) / ((size_t)(!(sizeof(arr) % sizeof(0[arr])))))

#define U8(string_literal) \
	(const uint8_t *)("" string_literal "")

#define ELOX_STATIC_STRLEN(string_literal) \
	(sizeof("" string_literal "") - 1)

#define ELOX_STR_AND_LEN(string_literal) \
	("" string_literal ""), (sizeof("" string_literal "") - 1)

#define ELOX_USTR_AND_LEN(string_literal) \
	(const uint8_t *)("" string_literal ""), (sizeof("" string_literal "") - 1)

typedef struct {
	const uint8_t *chars;
	int32_t length;
} String;

#define STRING_INITIALIZER(string_literal) \
	{ .chars = (uint8_t *)"" string_literal "", .length = (sizeof("" string_literal "") - 1) }

bool stringEquals(const String *a, const String *b);

EloxInterpretResult eloxRunFile(EloxVM *vmCtx, const char *path);

#endif
