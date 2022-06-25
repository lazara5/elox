#ifndef ELOX_COMMON_H
#define ELOX_COMMON_H

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

#define ELOX_UNCONST(ptr) ((void *)(uintptr_t)(const void *)(ptr))

#define ELOX_STATIC_STRLEN(string_literal) \
	(sizeof("" string_literal "") - 1)

#define ELOX_STR_AND_LEN(string_literal) \
	("" string_literal ""), (sizeof("" string_literal "") - 1)

typedef struct {
	const char *chars;
	int length;
} String;

bool stringEquals(const String *a, const String *b);

#define STRING_INITIALIZER(string_literal) \
	{ .chars = "" string_literal "", .length = (sizeof("" string_literal "") - 1) }

#endif //ELOX_COMMON_H
