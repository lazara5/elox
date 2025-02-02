// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_UTIL_H
#define ELOX_UTIL_H

#ifdef _MSC_VER
#include <windows.h>
// whoever thought it would be a good idea to define this ?
#undef IN
#include <intrin.h>
#endif

#include "elox.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdatomic.h>

#define ELOX_MIN(a,b) (((a)<(b))?(a):(b))
#define ELOX_MAX(a,b) (((a)>(b))?(a):(b))

#define UINT8_COUNT  (UINT8_MAX + 1)
#define UINT16_COUNT (UINT16_MAX + 1)

typedef int32_t suint16_t;

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
#elif defined(_MSC_VER)
#define ELOX_FORCE_INLINE __forceinline
#else
#define ELOX_FORCE_INLINE
#endif // __GNUC__
#endif // ELOX_FORCE_INLINE

#if !defined(ELOX_EXPORT)
#if defined(__GNUC__)
#define ELOX_EXPORT
#elif defined(_MSC_VER)
#define ELOX_EXPORT __declspec(dllexport)
#else
#define ELOX_EXPORT
#endif // __GNUC__
#endif // ELOX_EXPORT

#if !defined(ELOX_PACKED)
#if defined(__GNUC__)
#define ELOX_PACKED __attribute__((__packed__))
#else
#define ELOX_PACKED
#endif // __GNUC__
#endif // ELOX_PACKED

#if !defined(ELOX_CTZ)
#if defined (__GNUC__)
#define ELOX_CTZ(X) __builtin_ctz(X)
#elif defined(_MSC_VER)
static inline uint32_t ELOX_MSVC_CTZ(uint32_t x) {
	DWORD trailingZero;
	_BitScanForward(&trailingZero, x);
	return trailingZero;
}
#define ELOX_CTZ(X) ELOX_MSVC_CTZ(X)
#else
#error("no CTZ implementation available")
#endif // __GNUC__
#endif // ELOX_CTZ

#if !defined(ELOX_ALIGN)
#if defined(__GNUC__)
#define ELOX_ALIGN(X) __asm volatile (".align " #X)
#else
#define ELOX_ALIGN(X)
#endif // __GNUC__
#endif // ELOX_ALIGN

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

#define ELOX_STRING(string_literal) \
	{ .chars = (uint8_t *)"" string_literal "", .length = (sizeof("" string_literal "") - 1) }

#define TOKEN_INITIALIZER(string_literal) \
	{ .string.chars = (uint8_t *)"" string_literal "", .string.length = (sizeof("" string_literal "") - 1) }

// Generic Fibonacci hashing, see
// https://probablydance.com/2018/06/16/fibonacci-hashing-the-optimization-that-the-world-forgot-or-a-better-alternative-to-integer-modulo/
static inline uint32_t tableIndexFor(uint32_t hash, uint32_t shift) {
	return (hash * 2654435769u) >> shift;
}

bool stringEquals(const EloxString *a, const EloxString *b);

EloxString eloxBasename(const char *path);

EloxInterpretResult eloxRunFile(EloxRunCtxHandle *runHandle, const char *path);

EloxInterpretResult eloxInterpret(EloxRunCtxHandle *runHandle, uint8_t *source,
								  const EloxString *fileName, const EloxString *moduleName);

#endif
