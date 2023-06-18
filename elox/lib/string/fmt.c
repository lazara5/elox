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
	VMCtx *vmCtx;
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

static int getAutoIdx(FmtState *state, Error *error) {
	if (ELOX_UNLIKELY(state->autoIdx == -1))
		ELOX_RAISE_RET_VAL(-1, error, "Cannot mix auto and specific field numbering");
	if (ELOX_UNLIKELY(state->autoIdx == state->maxArg))
		ELOX_RAISE_RET_VAL(-1, error, "Auto index out of range");
	state->autoIdx++;
	return state->autoIdx;
}

static int getSpecificIdx(int idx, FmtState *state, Error *error) {
	if (ELOX_UNLIKELY(state->autoIdx > 0))
		ELOX_RAISE_RET_VAL(-1, error, "Cannot mix auto and specific field numbering");
	state->autoIdx = -1;
	return idx;
}

static bool parseUInt(int *val, FmtState *state, Error *error) {
	const uint8_t *ptr = state->ptr;
	unsigned int base = 0;
	while ((ptr < state->end) && isDigit(*ptr)) {
		int digit = *ptr++ - '0';
		bool validDigit = (base < INT_MAX / 10) ||
						  (base == INT_MAX / 10 && digit <= INT_MAX % 10);
		if (ELOX_UNLIKELY(!validDigit)) {
			ELOX_RAISE(error, "Too many decimal digits");
			return false;
		}
		base = base * 10 + digit;
	}
	if (ptr == state->ptr)
		return false;
	if (val)
		*val = base;
	state->ptr = ptr;
	return true;
}

static Value getProperty(Value object, String *key, FmtState *state, Error *error) {
	VMCtx *vmCtx = state->vmCtx;
	VM *vm = &vmCtx->vm;

	if (!IS_MAP(object)) {
		ELOX_RAISE(error, "Argument is not a map");
		return NIL_VAL;
	}
	ObjMap *map = AS_MAP(object);

	ObjString *keyString = copyString(vmCtx, key->chars, key->length);
	push(vm, OBJ_VAL(keyString));

	Value val;
	bool found = valueTableGet(&map->items, OBJ_VAL(keyString), &val, error);
	if (ELOX_UNLIKELY(error->raised))
		return EXCEPTION_VAL;

	if (!found)
		ELOX_RAISE_RET_EXC(error, "Undefined property %.*s", key->length, key->chars);

	pop(vm); // key
	return val;
}

static Value getIndex(Value object, int index, Error *error) {
	if (!IS_ARRAY(object)) {
		ELOX_RAISE(error, "Argument is not an array");
		return NIL_VAL;
	}
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

static Value access(Value val, FmtState *state, Error *error) {
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
			} else {
				ELOX_RAISE(error, "Invalid identifier after '.'");
				return NIL_VAL;
			}
		} else if (parseUInt(&idx, state, error)) {
			if (*state->ptr != ']') {
				ELOX_RAISE(error, "Unexpected '%c' in field name", *state->ptr);
				return NIL_VAL;
			}
			state->ptr++;
			crtVal = getIndex(crtVal, idx, error);
		} else {
			if (ELOX_UNLIKELY(error->raised))
				return NIL_VAL;
			String name;
			if (getIdentifier(state, &name)) {
				if (*state->ptr != ']')
					ELOX_RAISE_RET_VAL(NIL_VAL, error, "Unexpected '%c' in field name", *state->ptr);
				state->ptr++;
				crtVal = getProperty(crtVal, &name, state, error);
				if (ELOX_UNLIKELY(error->raised))
					return NIL_VAL;
			} else
				ELOX_RAISE_RET_VAL(NIL_VAL, error, "Invalid identifier in '[]'");
		}
	}
	return crtVal;
}

static Value getArg(FmtState *state, Error *error) {
	if (state->ptr >= state->end)
		ELOX_RAISE_RET_VAL(NIL_VAL, error, "'}' expected");

	int idx = 0;
	Value val;

	if ((*state->ptr == ':') || (*state->ptr == '}')) {
		int argIdx = getAutoIdx(state, error);
		val = getValueArg(state->args, argIdx);
	}
	else if (parseUInt(&idx, state, error)) {
		int argIdx = getSpecificIdx(idx, state, error);
		if ((argIdx < 1) || (argIdx > state->maxArg))
			ELOX_RAISE_RET_VAL(NIL_VAL, error, "Argument index out of range: %d", argIdx);
		val = getValueArg(state->args, argIdx);
	} else {
		getSpecificIdx(0, state, error);
		String name;
		if (!getIdentifier(state, &name))
			ELOX_RAISE_RET_VAL(NIL_VAL, error, "Unexpected '%c' in field name", *state->ptr);
		val = getProperty(getValueArg(state->args,1), &name, state, error);
	}

	if (ELOX_UNLIKELY(error->raised))
		return NIL_VAL;

	return access(val, state, error);
}

static char readChar(FmtState *state, Error *error) {
	char ch = *state->ptr;
	state->ptr++;
	if (state->ptr >= state->end) {
		ELOX_RAISE(error, "Unterminated format spec");
		return -1;
	}
	return ch;
}

static int toInteger(const Value val, Error *error) {
	if (!IS_NUMBER(val))
		ELOX_RAISE_RET_VAL(0, error, "Integer expected");
	double dVal = AS_NUMBER(val);
	double iVal = trunc(dVal);
	if (iVal == dVal)
		return (int)iVal;
	ELOX_RAISE_RET_VAL(0, error, "Integer expected, got double");
}

static int readUInt(FmtState *state, bool required, const char *label, Error *error) {
	int val = 0;

	if (*state->ptr != '{') {
		bool isInt = parseUInt(&val, state, error);
		if (ELOX_UNLIKELY(error->raised))
			return 0;
		if ((!isInt) && required)
			ELOX_RAISE_RET_VAL(0, error, "Missing %s in format specifier", label);
		if (state->ptr >= state->end)
			ELOX_RAISE_RET_VAL(0, error, "Unterminated format spec");
	} else {
		state->ptr++;
		Value arg = getArg(state, error);
		if (ELOX_UNLIKELY(error->raised))
			return 0;
		if (*state->ptr != '}')
			ELOX_RAISE_RET_VAL(0, error, "Unexpected character '%c' in format spec", *state->ptr);
		state->ptr++;
		val = toInteger(arg, error);
		if (ELOX_UNLIKELY(error->raised))
			return 0;
	}

	return val;
}

static void parseSpec(FmtState *state, FmtSpec *spec, Error *error) {
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
			if (state->ptr >= state->end)
				ELOX_RAISE_RET(error, "Unterminated format spec");
			ELOX_RAISE_RET(error, "Invalid format specifier: '%s'", ptr);
		}
	}
}

static void parse(FmtState *state, FmtSpec *spec, Error *error) {
	state->arg = getArg(state, error);
	if (error->raised)
		return;
	if ((*state->ptr == ':') && (state->ptr + 1 < state->end)) {
		state->ptr++;
		parseSpec(state, spec, error);
		if (error->raised)
			return;
	}
	if ((state->ptr >= state->end) || (*state->ptr != '}'))
		ELOX_RAISE_RET(error, "'}' expected");
	state->ptr++;
}

static void addPadding(FmtState *state, char ch, int len) {
	if (ch == 0)
		ch = ' ';
	uint8_t *padding = reserveHeapString(state->vmCtx, state->output, len);
	memset(padding, ch, len);
}

#define FMT_GROUPING_WIDTH 3
#define FMT_FULL_GROUP_WIDTH (FMT_GROUPING_WIDTH + 1)

static void addZeroPadding(FmtState *state, FmtSpec *spec, int len) {
	uint8_t *padding = reserveHeapString(state->vmCtx, state->output, len);
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
	const uint8_t *s = str->chars;
	int len = str->length;

	if (shrink && spec->precision)
		len = (len > spec->precision) ? spec->precision : len;

	if (len > width) {
		heapStringAddString(state->vmCtx, state->output, s, len);
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
			heapStringAddString(state->vmCtx, state->output, s, len);
			break;
		case '<':
			heapStringAddString(state->vmCtx, state->output, s, len);
			addPadding(state, spec->fill, padLen);
			break;
		case '^':
			addPadding(state, spec->fill, padLen / 2);
			heapStringAddString(state->vmCtx, state->output, s, len);
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

static void dumpChar(int64_t codepoint, FmtState *state, FmtSpec *spec, Error *error) {
	if (ELOX_UNLIKELY(spec->sign != '\0'))
		ELOX_RAISE_RET(error, "Sign not allowed for char format");
	if (ELOX_UNLIKELY(spec->grouping != '\0'))
		ELOX_RAISE_RET(error, "Grouping not allowed for char format");
	if (ELOX_UNLIKELY(spec->zero != '\0'))
		ELOX_RAISE_RET(error, "Zero padding not allowed for char format")
	if (ELOX_UNLIKELY(spec->alternate != '\0'))
		ELOX_RAISE_RET(error, "Alternate form not allowed for char format");
	if (ELOX_UNLIKELY(codepoint < 0 || codepoint > INT32_MAX))
		ELOX_RAISE_RET(error, "Char codepoint out of range");

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
			heapStringAddString(state->vmCtx, state->output, ptr, dp - ptr);
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

static void dumpDouble(double val, FmtState *state, FmtSpec *spec, Error *error) {
	uint8_t buffer[DBL_FMT_BUFFER_SIZE];
	uint8_t *ptr = buffer;

	if (spec->precision >= DBL_FMT_MAX_PREC)
		ELOX_RAISE_RET(error, "Maximum precision exceeded");
	if (spec->grouping)
		ELOX_RAISE_RET(error, "Grouping not allowed for floating point formats");

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
			heapStringAddString(state->vmCtx, state->output, buffer, dp - ptr);
		width -= (dp - ptr);
		ptr = dp;
	}
	String str = { .chars = ptr, .length = len };
	addString(&str, state, spec, false, width);
}

static void dumpNumber(double val, FmtState *state, FmtSpec *spec, Error *error) {
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
			ELOX_RAISE(error, "Unknown format '%c' for number argument", type);
			break;
	}
}

static void dumpString(ObjString *str, FmtState *state, FmtSpec *spec, Error *error) {
	if (spec->type && (spec->type != 's'))
		ELOX_RAISE_RET(error, "Unknown format code '%c' for string argument", spec->type);
	addString(&str->string, state, spec, true, spec->width);
}

static void dump(FmtState *state, FmtSpec *spec, Error *error) {
	VMCtx *vmCtx = state->vmCtx;
	VM *vm = &vmCtx->vm;

	Value arg = state->arg;
	if (IS_NUMBER(arg)) {
		dumpNumber(AS_NUMBER(arg), state, spec, error);
		return;
	}

	Value strVal = toString(arg, error);
	if (ELOX_UNLIKELY(error->raised))
		return;

	push(vm, strVal);

	DBG_PRINT_STACK("DBG0", vmCtx);

	dumpString(AS_STRING(strVal), state, spec, error);
	if (ELOX_UNLIKELY(error->raised))
		return;

	pop(vm);
}

static bool format(Args *args, HeapCString *output) {
	VMCtx *vmCtx = args->vmCtx;

	ObjString *str = AS_STRING(getValueArg(args, 0));

	FmtState state = {
		.vmCtx = vmCtx,
		.ptr = str->string.chars,
		.end = str->string.chars + str->string.length,
		.args = args,
		.maxArg = args->count - 1,
		.autoIdx = 0,
		.output = output
	};

	initHeapStringWithSize(vmCtx, output, str->string.length + 1);

	while (state.ptr < state.end) {
		const uint8_t *ptr = state.ptr;

		while ((ptr < state.end) && (*ptr != '{') && (*ptr != '}'))
			ptr++;
		heapStringAddString(vmCtx, output, state.ptr, ptr - state.ptr);
		state.ptr = ptr;

		if (state.ptr >= state.end)
			break;
		if (state.ptr[0] == state.ptr[1]) {
			// escaped bracket
			heapStringAddChar(vmCtx, output, state.ptr[0]);
			state.ptr += 2;
		} else {
			if ((*state.ptr++ == '}') || (state.ptr >= state.end)) {
				runtimeError(vmCtx, "Single '%c' in format string", *(state.ptr - 1));
				return false;
			}

			Error error = { .vmCtx = vmCtx };

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
	VMCtx *vmCtx = args->vmCtx;

	HeapCString output;

	if (!format(args, &output)) {
		freeHeapString(vmCtx, &output);
		return EXCEPTION_VAL;
	}

	return OBJ_VAL(takeString(vmCtx, output.chars, output.length, output.capacity));
}

Value printFmt(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	ObjString *fmt;
	ELOX_GET_STRING_ARG_ELSE_RET(&fmt, args, 0);

	HeapCString output;

	if (!format(args, &output)) {
		freeHeapString(vmCtx, &output);
		return EXCEPTION_VAL;
	}

	// TODO: UTF8
	vmCtx->write(ELOX_IO_OUT, (const char *)output.chars, output.length);

	freeHeapString(vmCtx, &output);
	return NIL_VAL;
}
