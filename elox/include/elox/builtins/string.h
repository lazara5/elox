// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_BUILTINS_STRING_H
#define ELOX_BUILTINS_STRING_H

#include <elox/state.h>

extern const uint8_t eloxCTable[256];
extern const uint8_t upperLookup[256];
extern const uint8_t lowerLookup[256];

#include "ctypeInit.h"

/// Alphabetic or _
static inline bool isAlpha(uint8_t c) {
	return (eloxCTable[c] & ALPHA_MASK) != 0;
}

static inline bool isAlnum(uint8_t c) {
	return (eloxCTable[c] & (ALPHA_MASK | DIGIT_MASK)) != 0;
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
bool stringContains(const ObjString *seq, const ObjString *needle);
Value stringSlice(RunCtx *runCtx, ObjString *str, Value start, Value end);
Value stringAtSafe(RunCtx *runCtx, ObjString *str, int32_t index);

#endif
