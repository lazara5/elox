#ifndef ELOX_BUILTINS_STRING_H
#define ELOX_BUILTINS_STRING_H

#include <elox/state.h>

extern const uint8_t eloxCTable[256];

#include "ctypeInit.h"

static inline bool isAlpha(uint8_t c) {
	return (eloxCTable[c] & ALPHA_MASK) != 0;
}

static inline bool isDigit(uint8_t c) {
	return (eloxCTable[c] & DIGIT_MASK) != 0;
}

static inline bool isHex(uint8_t c) {
	return (eloxCTable[c] & HEX_MASK) != 0;
}

static inline bool isWhitespace(uint8_t c) {
	return (eloxCTable[c] & WS_MASK) != 0;
}

#include "ctypeCleanup.h"

Value stringFmt(Args *args);
Value printFmt(Args *args);
Value stringFind(Args *args);
Value stringFindMatch(Args *args);
Value stringMatch(Args *args);
Value stringGsub(Args *args);
Value gmatchIteratorHasNext(Args *args);
Value gmatchIteratorNext(Args *args);
Value stringGmatch(Args *args);
Value stringStartsWith(Args *args);
Value stringEndsWith(Args *args);
Value stringUpper(Args *args);
Value stringLower(Args *args);
Value stringTrim(Args *args);
Value stringAtSafe(VMCtx *vmCtx, ObjString *str, int32_t index);

#endif
