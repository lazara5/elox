// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/vm.h>
#include <elox/state.h>
#include <elox/builtins/string.h>

#include <limits.h>
#include <math.h>
#include <float.h>
#include <stdio.h>
#include <string.h>

typedef struct FmtState {
	RunCtx *runCtx;
	const uint8_t *ptr;
	const uint8_t *end;
	int autoIdx;
	Args *args;
	int maxArg;
	Value arg;
	char zeroPadding;
	HeapCString *output;
} FmtState;

typedef struct FmtSpec {
	char fill;
	char align;
	char sign;
	char alternate;
	char zero;
	char grouping;
	int width;
	int precision;
	char type;
} FmtSpec;

static int getAutoIdx(FmtState *state, EloxError *error) {
	ELOX_CHECK_THROW_RET_VAL(state->autoIdx != -1, error,
							 RTERR(state->runCtx, "Cannot mix auto and specific field numbering"), -1);
	ELOX_CHECK_THROW_RET_VAL(state->autoIdx < state->maxArg, error,
							 RTERR(state->runCtx, "Auto index out of range"), -1);
	state->autoIdx++;
	return state->autoIdx;
}

static int getSpecificIdx(int idx, FmtState *state, EloxError *error) {
	ELOX_CHECK_THROW_RET_VAL(state->autoIdx <= 0, error,
							 RTERR(state->runCtx, "Cannot mix auto and specific field numbering"), -1);
	state->autoIdx = -1;
	return idx;
}

static bool parseUInt(int *val, FmtState *state, EloxError *error) {
	const uint8_t *ptr = state->ptr;
	unsigned int base = 0;
	while ((ptr < state->end) && isDigit(*ptr)) {
		int digit = *ptr++ - '0';
		bool validDigit = (base < INT_MAX / 10) ||
						  (base == INT_MAX / 10 && digit <= INT_MAX % 10);
		ELOX_CHECK_THROW_RET_VAL(validDigit,
								 error, RTERR(state->runCtx, "Too many decimal digits"), false);
		base = base * 10 + digit;
	}
	if (ptr == state->ptr)
		return false;
	if (val)
		*val = base;
	state->ptr = ptr;
	return true;
}

static Value getProperty(Value object, String *key, FmtState *state, EloxError *error) {
	RunCtx *runCtx = state->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	ELOX_CHECK_THROW_RET_VAL(IS_HASHMAP(object), error, RTERR(runCtx, "Argument is not a map"), NIL_VAL);
	ObjHashMap *map = AS_HASHMAP(object);

	ObjString *keyString = copyString(runCtx, key->chars, key->length);
	ELOX_CHECK_THROW_RET_VAL(keyString != NULL, error, OOM(runCtx), NIL_VAL);
	push(fiber, OBJ_VAL(keyString));

	Value val;
	bool found = valueTableGet(runCtx, &map->items, OBJ_VAL(keyString), &val, error);
	if (ELOX_UNLIKELY(error->raised))
		return EXCEPTION_VAL;

	ELOX_CHECK_THROW_RET_VAL(found, error,
							 RTERR(runCtx, "Undefined property %.*s", key->length, key->chars), EXCEPTION_VAL);

	pop(fiber); // key
	return val;
}

static Value getIndex(RunCtx *runCtx, Value object, int index, EloxError *error) {
	ELOX_CHECK_THROW_RET_VAL(IS_ARRAY(object), error,
							 RTERR(runCtx, "Argument is not an array"), NIL_VAL);
	ObjArray *array = AS_ARRAY(object);
	return arrayAt(array, index);
}

static bool getIdentifier(FmtState *state, String *str) {
	const uint8_t *ptr = state->ptr;

	if (isAlpha(*ptr))
		while (++ptr < state->end && isAlnum(*ptr));

	if (ptr == state->ptr)
		return false;

	str->chars = state->ptr;
	str->length = ptr - state->ptr;
	state->ptr = ptr;
	return true;
}

static Value access(Value val, FmtState *state, EloxError *error) {
	RunCtx *runCtx = state->runCtx;

	Value crtVal = val;
	while ((*state->ptr == '.') || (*state->ptr == '[')) {
		if (ELOX_UNLIKELY(error->raised))
			return NIL_VAL;

		int idx;

		state->ptr++;
		const uint8_t *ptr = state->ptr;
		if (ptr[-1] == '.') {
			String name;
			if (getIdentifier(state, &name)) {
				crtVal = getProperty(crtVal, &name, state, error);
				if (ELOX_UNLIKELY(error->raised))
					return NIL_VAL;
			} else
				ELOX_THROW_RET_VAL(error, RTERR(runCtx, "Invalid identifier after '.'"), NIL_VAL);
		} else if (parseUInt(&idx, state, error)) {
			if (*state->ptr != ']') {
				ELOX_THROW_RET_VAL(error, RTERR(runCtx, "Unexpected '%c' in field name", *state->ptr), NIL_VAL);
			}
			state->ptr++;
			crtVal = getIndex(runCtx, crtVal, idx, error);
		} else {
			if (ELOX_UNLIKELY(error->raised))
				return NIL_VAL;
			String name;
			if (getIdentifier(state, &name)) {
				ELOX_CHECK_THROW_RET_VAL(*state->ptr == ']', error,
										 RTERR(runCtx, "Unexpected '%c' in field name", *state->ptr), NIL_VAL);
				state->ptr++;
				crtVal = getProperty(crtVal, &name, state, error);
				if (ELOX_UNLIKELY(error->raised))
					return NIL_VAL;
			} else
				ELOX_THROW_RET_VAL(error, RTERR(runCtx, "Invalid identifier in '[]'"), NIL_VAL);
		}
	}
	return crtVal;
}

static Value getArg(FmtState *state, EloxError *error) {
	RunCtx *runCtx = state->runCtx;

	ELOX_CHECK_THROW_RET_VAL(state->ptr < state->end, error, RTERR(runCtx, "'}' expected"), NIL_VAL);

	int idx = 0;
	Value val;

	if ((*state->ptr == ':') || (*state->ptr == '}')) {
		int argIdx = getAutoIdx(state, error);
		val = getValueArg(state->args, argIdx);
	}
	else if (parseUInt(&idx, state, error)) {
		int argIdx = getSpecificIdx(idx, state, error);
		ELOX_CHECK_THROW_RET_VAL((argIdx >= 1) && (argIdx < state->maxArg), error,
								 RTERR(runCtx, "Argument index out of range: %d", argIdx), NIL_VAL);
		val = getValueArg(state->args, argIdx);
	} else {
		getSpecificIdx(0, state, error);
		String name;
		ELOX_CHECK_THROW_RET_VAL(getIdentifier(state, &name), error,
								 RTERR(runCtx, "Unexpected '%c' in field name", *state->ptr), NIL_VAL);
		val = getProperty(getValueArg(state->args,1), &name, state, error);
	}

	if (ELOX_UNLIKELY(error->raised))
		return NIL_VAL;

	return access(val, state, error);
}

static char readChar(FmtState *state, EloxError *error) {
	char ch = *state->ptr;
	state->ptr++;
	ELOX_CHECK_THROW_RET_VAL(state->ptr < state->end, error,
							 RTERR(state->runCtx, "Unterminated format spec"), -1);
	return ch;
}

static int toInteger(RunCtx *runCtx, const Value val, EloxError *error) {
	ELOX_CHECK_THROW_RET_VAL(IS_NUMBER(val), error, RTERR(runCtx, "Integer expected"), 0);
	double dVal = AS_NUMBER(val);
	double iVal = trunc(dVal);
	if (iVal == dVal)
		return (int)iVal;
	ELOX_THROW_RET_VAL(error, RTERR(runCtx, "Integer expected, got double"), 0);
}

static int readUInt(FmtState *state, bool required, const char *label, EloxError *error) {
	RunCtx *runCtx = state->runCtx;

	int val = 0;

	if (*state->ptr != '{') {
		bool isInt = parseUInt(&val, state, error);
		if (ELOX_UNLIKELY(error->raised))
			return 0;
		ELOX_CHECK_THROW_RET_VAL(isInt || (!required), error,
								 RTERR(runCtx, "Missing %s in format specifier", label), 0);
		ELOX_CHECK_THROW_RET_VAL(state->ptr < state->end, error,
								 RTERR(runCtx, "Unterminated format spec"), 0);
	} else {
		state->ptr++;
		Value arg = getArg(state, error);
		if (ELOX_UNLIKELY(error->raised))
			return 0;
		ELOX_CHECK_THROW_RET_VAL(*state->ptr == '}', error,
								 RTERR(runCtx, "Unexpected character '%c' in format spec", *state->ptr), 0);
		state->ptr++;
		val = toInteger(runCtx, arg, error);
		if (ELOX_UNLIKELY(error->raised))
			return 0;
	}

	return val;
}

static void parseSpec(FmtState *state, FmtSpec *spec, EloxError *error) {
	if ((state->ptr[1] == '<') || (state->ptr[1] == '>') || (state->ptr[1] == '^')) {
		spec->fill  = readChar(state, error);
		spec->align = readChar(state, error);
	} else if ((*state->ptr == '<') || (*state->ptr == '>') || (*state->ptr == '^'))
		spec->align = readChar(state, error);
	if (*state->ptr == ' ' || *state->ptr == '+' || *state->ptr == '-')
		spec->sign = readChar(state, error);
	if (*state->ptr == '#')
		spec->alternate = readChar(state, error);
	if (*state->ptr == '0')
		spec->zero  = readChar(state, error);
	if (ELOX_UNLIKELY(error->raised))
		return;

	spec->width = readUInt(state, false, "width", error);
	if (ELOX_UNLIKELY(error->raised))
		return;

	if (*state->ptr == ',')
		spec->grouping = readChar(state, error);
	if (ELOX_UNLIKELY(error->raised))
		return;

	if (*state->ptr == '.') {
		state->ptr++;
		spec->precision = readUInt(state, true, "precision", error);
		if (ELOX_UNLIKELY(error->raised))
			return;
	}

	if (*state->ptr != '}') {
		const uint8_t *ptr = state->ptr++;
		spec->type = *ptr;
		if (*state->ptr != '}') {
			while ((state->ptr < state->end) && (*state->ptr != '}'))
				state->ptr++;
			ELOX_CHECK_THROW_RET(state->ptr < state->end, error,
								 RTERR(state->runCtx, "Unterminated format spec"));
			ELOX_THROW_RET(error, RTERR(state->runCtx, "Invalid format specifier: '%s'", ptr));
		}
	}
}

static void parse(FmtState *state, FmtSpec *spec, EloxError *error) {
	state->arg = getArg(state, error);
	if (error->raised)
		return;
	if ((*state->ptr == ':') && (state->ptr + 1 < state->end)) {
		state->ptr++;
		parseSpec(state, spec, error);
		if (error->raised)
			return;
	}
	ELOX_CHECK_THROW_RET((state->ptr < state->end) && (*state->ptr == '}'),
						 error, RTERR(state->runCtx, "'}' expected"));
	state->ptr++;
}

static void addPadding(FmtState *state, char ch, int len) {
	if (ch == 0)
		ch = ' ';
	// TODO: check
	uint8_t *padding = reserveHeapString(state->runCtx, state->output, len);
	memset(padding, ch, len);
}

#define FMT_GROUPING_WIDTH 3
#define FMT_FULL_GROUP_WIDTH (FMT_GROUPING_WIDTH + 1)

static void addZeroPadding(FmtState *state, FmtSpec *spec, int len) {
	// TODO: check
	uint8_t *padding = reserveHeapString(state->runCtx, state->output, len);
	if (len > state->zeroPadding) {
		int prefix = (len - state->zeroPadding) % FMT_FULL_GROUP_WIDTH;
		if (prefix > 2)
			*padding++ = '0';
		if (prefix > 0) {
			*padding++ = '0';
			*padding++ = spec->grouping;
		}
		len -= prefix;
		while (len > FMT_FULL_GROUP_WIDTH) {
			padding[0] = padding[1] = padding[2] = '0';
			padding[3] = spec->grouping;
			padding += FMT_FULL_GROUP_WIDTH;
			len -= FMT_FULL_GROUP_WIDTH;
		}
	}
	memset(padding, '0', len);
}

static void addString(String *str, FmtState *state, FmtSpec *spec, bool shrink, int width) {
	RunCtx *runCtx = state->runCtx;

	const uint8_t *s = str->chars;
	int len = str->length;

	if (shrink && spec->precision)
		len = (len > spec->precision) ? spec->precision : len;

	if (len > width) {
		// TODO: check
		heapStringAddString(runCtx, state->output, s, len);
		return;
	}

	int padLen = width - len;
	switch (spec->align) {
		case 0:
		case '>':
			if ((!spec->zero) || (spec->grouping == 0))
				addPadding(state, (spec->fill) ? spec->fill : spec->zero, padLen);
			else
				addZeroPadding(state, spec, padLen);
			heapStringAddString(runCtx, state->output, s, len);
			break;
		case '<':
			heapStringAddString(runCtx, state->output, s, len);
			addPadding(state, spec->fill, padLen);
			break;
		case '^':
			addPadding(state, spec->fill, padLen / 2);
			heapStringAddString(runCtx, state->output, s, len);
			addPadding(state, spec->fill, padLen - padLen / 2);
			break;
	}
}

static const char *lowerHex = "0123456789abcdef";
static const char *upperHex = "0123456789ABCDEF";

#define INT_FMT_BUFFER_SIZE 10

static char writeInt(uint8_t **pPtr, int64_t val, FmtSpec *spec) {
	int radix = 10;
	char zeroPadding;
	uint8_t *ptr = *pPtr;
	const char *alphabet = lowerHex;

	switch (spec->type) {
		case 'X':
			alphabet = upperHex;
			radix = 16;
			break;
		case 'x':
			alphabet = lowerHex;
			radix = 16;
			break;
		case 'o':
		case 'O':
			radix = 8;
			break;
		case 'b':
		case 'B':
			radix = 2;
			break;
	}

	zeroPadding = spec->grouping ? FMT_GROUPING_WIDTH : 0;

	while (*--ptr = alphabet[val % radix], val /= radix, --zeroPadding, val != 0) {
		if (zeroPadding == 0) {
			zeroPadding = FMT_GROUPING_WIDTH;
			*--ptr = spec->grouping;
		}
	}
	*pPtr = ptr;

	return zeroPadding;
}

static void dumpChar(int64_t codepoint, FmtState *state, FmtSpec *spec, EloxError *error) {
	RunCtx *runCtx = state->runCtx;

	ELOX_CHECK_THROW_RET(spec->sign == '\0', error, RTERR(runCtx, "Sign not allowed for char format"));
	ELOX_CHECK_THROW_RET(spec->grouping == '\0', error, RTERR(runCtx, "Grouping not allowed for char format"));
	ELOX_CHECK_THROW_RET(spec->zero == '\0', error, RTERR(runCtx, "Zero padding not allowed for char format"));
	ELOX_CHECK_THROW_RET(spec->alternate == '\0', error, RTERR(runCtx, "Alternate form not allowed for char format"));
	ELOX_CHECK_THROW_RET((codepoint >= 0 && codepoint <= INT32_MAX), error,
						RTERR(runCtx, "Char codepoint out of range"));

	// The string encoding will eventually be utf8...
	char ch[4];
	String str = { .chars = (const uint8_t *)ch };

	if (codepoint <= 0x7F) {
		// plain ASCII
		ch[0] = (char)codepoint;
		str.length = 1;
	} else if (codepoint <= 0x07FF) {
		// 2-byte utf8
		ch[0] = (char)(((codepoint >> 6) & 0x1F) | 0xC0);
		ch[1] = (char)(((codepoint >> 0) & 0x3F) | 0x80);
		str.length = 2;
	} else if (codepoint <= 0xFFFF) {
		// 3-byte utf8
		ch[0] = (char)(((codepoint >> 12) & 0x0F) | 0xE0);
		ch[1] = (char)(((codepoint >>  6) & 0x3F) | 0x80);
		ch[2] = (char)(((codepoint >>  0) & 0x3F) | 0x80);
		str.length = 3;
	} else if (codepoint <= 0x10FFFF) {
		// 4-byte utf8
		ch[0] = (char)(((codepoint >> 18) & 0x07) | 0xF0);
		ch[1] = (char)(((codepoint >> 12) & 0x3F) | 0x80);
		ch[2] = (char)(((codepoint >>  6) & 0x3F) | 0x80);
		ch[3] = (char)(((codepoint >>  0) & 0x3F) | 0x80);
		str.length = 4;
	} else {
		// error, use replacement character
		ch[0] = (char)0xEF;
		ch[1] = (char)0xBF;
		ch[2] = (char)0xBD;
		str.length = 3;
	}

	addString(&str, state, spec, false, spec->width);
}

static char getSign(bool positive, char dsign) {
	switch (dsign) {
		case '+':
			return positive ? '+' : '-';
		case ' ':
			return positive ? ' ' : '-';
		default:
			return positive ?  0  : '-';
	}
}

static void dumpInt(int64_t val, FmtState *state, FmtSpec *spec) {
	uint8_t buffer[INT_FMT_BUFFER_SIZE];
	uint8_t *ptr = buffer + INT_FMT_BUFFER_SIZE;

	bool positive = (val >= 0);
	int width = spec->width;
	if (!positive)
		val = -val;
	state->zeroPadding = writeInt(&ptr, val, spec);
	uint8_t *dp = ptr;
	if (spec->alternate && (spec->type != 0) && (spec->type != 'd')) {
		*--ptr = spec->type;
		*--ptr = '0';
	}
	if ((ptr[-1] = getSign(positive, spec->sign)) != 0)
		ptr--;
	if (spec->zero && (spec->width > (INT_FMT_BUFFER_SIZE - (ptr - buffer)))) {
		if (dp > ptr)
			heapStringAddString(state->runCtx, state->output, ptr, dp - ptr);
		width -= (dp - ptr);
		ptr = dp;
	}
	String str = { .chars = ptr, .length = INT_FMT_BUFFER_SIZE - (ptr - buffer) };
	addString(&str, state, spec, false, width);
}

// oversized to prevent truncation warnings
#define DBL_FMT_LEN 20

static int writeDouble(double val, uint8_t *buffer, int size, FmtSpec *spec) {
	char fmt[DBL_FMT_LEN];
	int type = spec->type ? spec->type : 'g';
	const char *percent = "";
	const char *alternate = "";

	if (spec->type == '%') {
		type = 'f';
		val *= 100.0;
		percent = "%%";
	}
	if (spec->alternate)
		alternate = "#";

	// kind of cheating, but it's hard to replace printf for floating point...
	if (spec->precision) {
			snprintf(fmt, DBL_FMT_LEN, "%%%s.%d%c%s",
					 alternate, spec->precision, type, percent);
		} else if (trunc(val) == val)
			snprintf(fmt, DBL_FMT_LEN, "%%.1f%s", percent);
		else {
			snprintf(fmt, DBL_FMT_LEN, "%%%s%c%s",
					 alternate, type, percent);
		}
		return snprintf((char *)buffer, size, fmt, val);
}

// 2 digits + decimal point
#define DBL_FMT_MAX_PREC 100
// 10 should cover possible extra characters, such as sign or exponent
#define DBL_FMT_BUFFER_SIZE (DBL_FMT_MAX_PREC + DBL_MAX_10_EXP + 10)

static void dumpDouble(double val, FmtState *state, FmtSpec *spec, EloxError *error) {
	RunCtx *runCtx = state->runCtx;

	uint8_t buffer[DBL_FMT_BUFFER_SIZE];
	uint8_t *ptr = buffer;

	ELOX_CHECK_THROW_RET(spec->precision < DBL_FMT_MAX_PREC, error,
						 RTERR(runCtx, "Maximum precision exceeded"));
	ELOX_CHECK_THROW_RET(spec->grouping == 0, error,
						 RTERR(runCtx, "Grouping not allowed for floating point formats"));

	bool positive = (val >= 0);
	int width = spec->width;

	if (!positive)
		val = -val;
	uint8_t *dp = ptr;
	if ((*dp = getSign(positive, spec->sign)) != 0)
		dp++;
	int len = writeDouble(val, dp, DBL_FMT_BUFFER_SIZE - (dp - buffer), spec);
	if (spec->zero && (width > len)) {
		if (dp > ptr)
			heapStringAddString(state->runCtx, state->output, buffer, dp - ptr);
		width -= (dp - ptr);
		ptr = dp;
	}
	String str = { .chars = ptr, .length = len };
	addString(&str, state, spec, false, width);
}

static void dumpNumber(double val, FmtState *state, FmtSpec *spec, EloxError *error) {
	char type = spec->type;
	if (type == 0)
		type = (trunc(val) == val) ? 'd' : 'g';
	switch (type) {
		case 'c':
			dumpChar((int64_t)val, state, spec, error);
			break;
		case 'd':
		case 'b':
		case 'B':
		case 'o':
		case 'O':
		case 'x':
		case 'X':
			dumpInt((int64_t)val, state, spec);
			break;
		case 'e':
		case 'E':
		case 'f':
		case 'F':
		case 'g':
		case 'G':
		case '%':
			dumpDouble(val, state, spec, error);
			break;
		default:
			ELOX_THROW_RET(error, RTERR(state->runCtx, "Unknown format '%c' for number argument", type));
			break;
	}
}

static void dumpString(ObjString *str, FmtState *state, FmtSpec *spec, EloxError *error) {
	ELOX_CHECK_THROW_RET((!spec->type) || (spec->type == 's'),
						 error, RTERR(state->runCtx, "Unknown format code '%c' for string argument", spec->type));
	addString(&str->string, state, spec, true, spec->width);
}

static void dump(FmtState *state, FmtSpec *spec, EloxError *error) {
	RunCtx *runCtx = state->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	Value arg = state->arg;
	if (IS_NUMBER(arg)) {
		dumpNumber(AS_NUMBER(arg), state, spec, error);
		return;
	}

	Value strVal = toString(runCtx, arg, error);
	if (ELOX_UNLIKELY(error->raised))
		return;

	push(fiber, strVal);

	DBG_PRINT_STACK("DBG0", runCtx);

	dumpString(AS_STRING(strVal), state, spec, error);
	if (ELOX_UNLIKELY(error->raised))
		return;

	pop(fiber);
}

static bool format(Args *args, HeapCString *output) {
	RunCtx *runCtx = args->runCtx;

	ObjString *str = AS_STRING(getValueArg(args, 0));

	FmtState state = {
		.runCtx = runCtx,
		.ptr = str->string.chars,
		.end = str->string.chars + str->string.length,
		.args = args,
		.maxArg = args->count - 1,
		.autoIdx = 0,
		.output = output
	};

	initHeapStringWithSize(runCtx, output, str->string.length + 1);

	while (state.ptr < state.end) {
		const uint8_t *ptr = state.ptr;

		while ((ptr < state.end) && (*ptr != '{') && (*ptr != '}'))
			ptr++;
		heapStringAddString(runCtx, output, state.ptr, ptr - state.ptr);
		state.ptr = ptr;

		if (state.ptr >= state.end)
			break;
		if (state.ptr[0] == state.ptr[1]) {
			// escaped bracket
			heapStringAddChar(runCtx, output, state.ptr[0]);
			state.ptr += 2;
		} else {
			if ((*state.ptr++ == '}') || (state.ptr >= state.end)) {
				runtimeError(runCtx, "Single '%c' in format string", *(state.ptr - 1));
				return false;
			}

			EloxError error = ELOX_ERROR_INITIALIZER;

			FmtSpec spec = { 0 };
			parse(&state, &spec, &error);
			if (ELOX_UNLIKELY(error.raised))
				return false;
			dump(&state, &spec, &error);
			if (ELOX_UNLIKELY(error.raised))
				return false;
		}
	}

	return true;
}

Value stringFmt(Args *args) {
	RunCtx *runCtx = args->runCtx;

	HeapCString output;

	if (!format(args, &output)) {
		freeHeapString(runCtx, &output);
		return EXCEPTION_VAL;
	}

	ObjString *str = takeString(runCtx, output.chars, output.length, output.capacity);
	if (ELOX_UNLIKELY(str == NULL))
		return oomError(runCtx);
	return OBJ_VAL(str);
}

Value printFmt(Args *args) {
	RunCtx *runCtx = args->runCtx;

	ObjString *fmt;
	ELOX_GET_STRING_ARG_ELSE_RET(&fmt, args, 0);

	HeapCString output;

	if (!format(args, &output)) {
		freeHeapString(runCtx, &output);
		return EXCEPTION_VAL;
	}

	// TODO: UTF8
	runCtx->vmEnv->write(ELOX_IO_OUT, (const char *)output.chars, output.length);

	freeHeapString(runCtx, &output);
	return NIL_VAL;
}
