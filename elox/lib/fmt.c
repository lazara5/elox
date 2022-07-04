#include "elox/vm.h"
#include "elox/state.h"

#include <limits.h>
#include <math.h>
#include <float.h>
#include <stdio.h>

typedef struct FmtState {
	VMCtx *vmCtx;
	const char *ptr;
	const char *end;
	int autoIdx;
	Value *args;
	int maxArg;
	int idx;
	char zeroPadding;
	HeapCString output;
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

typedef struct Error {
	VMCtx *vmCtx;
	bool raised;
	Value errorVal;
} Error;

#define RAISE(error, fmt, ...) \
	if (!error->raised) { \
		error->raised = true; \
		runtimeError(error->vmCtx, fmt, ## __VA_ARGS__); \
		error->errorVal = peek(&(error->vmCtx->vm), 0); \
	}

#define ERROR(error, val) \
	if (!error->raised) { \
		error->raised = true; \
		error->errorVal = val; \
	}

static inline bool isDigit(char ch) {
	return (ch >= '0') && (ch <= '9');
}

static int getAutoIdx(FmtState *state, Error *error) {
	if (ELOX_UNLIKELY(state->autoIdx == -1)) {
		RAISE(error, "Cannot mix auto and specific field numbering");
		return -1;
	}
	if (ELOX_UNLIKELY(state->autoIdx == state->maxArg)) {
		RAISE(error, "Auto index out of range");
		return -1;
	}
	state->autoIdx++;
	return state->autoIdx;
}

static int getSpecificIdx(FmtState *state, int idx, Error *error) {
	if (ELOX_UNLIKELY(state->autoIdx > 0)) {
		RAISE(error, "Cannot mix auto and specific field numbering");
		return -1;
	}
	state->autoIdx = -1;
	return idx;
}

static bool parseUInt(FmtState *state, int *val, Error *error) {
	const char *ptr = state->ptr;
	unsigned int base = 0;
	while ((ptr < state->end) && isDigit(*ptr)) {
		int digit = *ptr++ - '0';
		bool validDigit = (base < INT_MAX / 10) ||
						  (base == INT_MAX / 10 && digit <= INT_MAX % 10);
		if (ELOX_UNLIKELY(!validDigit)) {
			RAISE(error, "Too many decimal digits");
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

static int getArgId(FmtState *state, Error *error) {
	if (state->ptr >= state->end) {
		RAISE(error, "'}' expected");
		return -1;
	}
	int idx = 0;
	if ((*state->ptr == ':') || (*state->ptr == '}'))
		return getAutoIdx(state, error);
	else if (parseUInt(state, &idx, error)) {
		int argIdx = getSpecificIdx(state, idx, error);
		if ((argIdx < 1) || (argIdx > state->maxArg)) {
			RAISE(error, "Argument index out of range: %d", argIdx);
			return -1;
		}
		return argIdx;
	}

	RAISE(error, "Invalid argument index");
	return -1;
}

static char readChar(FmtState *state, Error *error) {
	char ch = *state->ptr;
	state->ptr++;
	if (state->ptr >= state->end) {
		RAISE(error, "Unterminated format spec");
		return -1;
	}
	return ch;
}

static int toInteger(const Value val, Error *error) {
	if (!IS_NUMBER(val)) {
		RAISE(error, "Integer expected");
		return 0;
	}
	double dVal = AS_NUMBER(val);
	double iVal = trunc(dVal);
	if (iVal == dVal)
		return (int)iVal;
	RAISE(error, "Integer expected, got double");
	return 0;
}

static int readUInt(FmtState *state, bool required, const char *label, Error *error) {
	int val = 0;

	if (*state->ptr != '{') {
		bool isInt = parseUInt(state, &val, error);
		if (ELOX_UNLIKELY(error->raised))
			return 0;
		if ((!isInt) && required) {
			RAISE(error, "Missing %s in format specifier", label);
			return 0;
		}
		if (state->ptr >= state->end) {
			RAISE(error, "Unterminated format spec");
			return 0;
		}
	} else {
		state->ptr++;
		int argIdx = getArgId(state, error);
		if (ELOX_UNLIKELY(error->raised))
			return 0;
		if (*state->ptr != '}') {
			RAISE(error, "Unexpected character '%c' in format spec", *state->ptr);
			return 0;
		}
		state->ptr++;
		val = toInteger(state->args[argIdx], error);
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
		const char *ptr = state->ptr++;
		spec->type = *ptr;
		if (*state->ptr != '}') {
			while ((state->ptr < state->end) && (*state->ptr != '}'))
				state->ptr++;
			if (state->ptr >= state->end) {
				RAISE(error, "Unterminated format spec");
				return;
			}
			RAISE(error, "Invalid format specifier: '%s'", ptr);
			return;
		}
	}
}

static void parse(FmtState *state, FmtSpec *spec, Error *error) {
	state->idx = getArgId(state, error);
	if (error->raised)
		return;
	if ((*state->ptr == ':') && (state->ptr + 1 < state->end)) {
		state->ptr++;
		parseSpec(state, spec, error);
		if (error->raised)
			return;
	}
	if ((state->ptr >= state->end) || (*state->ptr != '}')) {
		RAISE(error, "'}' expected");
		return;
	}
	state->ptr++;
}

static void addPadding(FmtState *state, char ch, int len) {
	if (ch == 0)
		ch = ' ';
	char *padding = reserveHeapString(state->vmCtx, &state->output, len);
	memset(padding, ch, len);
}

#define FMT_GROUPING_WIDTH 3
#define FMT_FULL_GROUP_WIDTH (FMT_GROUPING_WIDTH + 1)

static void addZeroPadding(FmtState *state, FmtSpec *spec, int len) {
	char *padding = reserveHeapString(state->vmCtx, &state->output, len);
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
	const char *s = str->chars;
	int len = str->length;

	if (shrink && spec->precision)
		len = (len > spec->precision) ? spec->precision : len;

	if (len > width) {
		addHeapString(state->vmCtx, &state->output, s, len);
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
			addHeapString(state->vmCtx, &state->output, s, len);
			break;
		case '<':
			addHeapString(state->vmCtx, &state->output, s, len);
			addPadding(state, spec->fill, padLen);
			break;
		case '^':
			addPadding(state, spec->fill, padLen/2);
			addHeapString(state->vmCtx, &state->output, s, len);
			addPadding(state, spec->fill, padLen - padLen/2);
			break;
	}
}

static const char *lowerHex = "0123456789abcdef";
static const char *upperHex = "0123456789ABCDEF";

#define INT_FMT_BUFFER_SIZE 10

static char writeInt(char **pPtr, int64_t val, FmtSpec *spec) {
	int radix = 10;
	char zeroPadding;
	char *ptr = *pPtr;
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

static void dumpChar() {

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
	char buffer[INT_FMT_BUFFER_SIZE];
	char *ptr = buffer + INT_FMT_BUFFER_SIZE;

	bool positive = (val >= 0);
	int width = spec->width;
	if (!positive)
		val = -val;
	state->zeroPadding = writeInt(&ptr, val, spec);
	char *dp = ptr;
	if (spec->alternate && (spec->type != 0) && (spec->type != 'd')) {
		*--ptr = spec->type;
		*--ptr = '0';
	}
	if ((ptr[-1] = getSign(positive, spec->sign)) != 0)
		ptr--;
	if (spec->zero && (spec->width > (INT_FMT_BUFFER_SIZE - (ptr - buffer)))) {
		if (dp > ptr)
			addHeapString(state->vmCtx, &state->output, ptr, dp - ptr);
		width -= (dp - ptr);
		ptr = dp;
	}
	String str = { .chars = ptr, .length = INT_FMT_BUFFER_SIZE - (ptr - buffer) };
	addString(&str, state, spec, false, width);
}

// oversized to prevent truncation warnings
#define DBL_FMT_LEN 20

static int writeDouble(double val, char *buffer, int size, FmtSpec *spec) {
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
		return snprintf(buffer, size, fmt, val);
}

// 2 digits + decimal point
#define DBL_FMT_MAX_PREC 100
// 10 should cover possible extra characters, such as sign or exponent
#define DBL_FMT_BUFFER_SIZE (DBL_FMT_MAX_PREC + DBL_MAX_10_EXP + 10)

static void dumpDouble(double val, FmtState *state, FmtSpec *spec, Error *error) {
	char buffer[DBL_FMT_BUFFER_SIZE];
	char *ptr = buffer;

	if (spec->precision >= DBL_FMT_MAX_PREC) {
		RAISE(error, "Maximum precision exceeded");
		return;
	}
	if (spec->grouping) {
		RAISE(error, "Grouping not allowed for floating point formats")
		return;
	}

	bool positive = (val >= 0);
	int width = spec->width;

	if (!positive)
		val = -val;
	char *dp = ptr;
	if ((*dp = getSign(positive, spec->sign)) != 0)
		dp++;
	int len = writeDouble(val, dp, DBL_FMT_BUFFER_SIZE - (dp - buffer), spec);
	if (spec->zero && (width > len)) {
		if (dp > ptr)
			addHeapString(state->vmCtx, &state->output, buffer, dp - ptr);
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
			dumpChar();
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
			RAISE(error, "Unknown format '%c' for number argument", type);
			break;
	}
}

static void dumpString(ObjString *str, FmtState *state, FmtSpec *spec, Error *error) {
	if (spec->type && (spec->type != 's')) {
		RAISE(error, "Unknown format code '%c' for string argument", spec->type);
		return;
	}
	addString(&str->string, state, spec, true, spec->width);
}

static void dump(FmtState *state, FmtSpec *spec, Error *error) {
	VMCtx *vmCtx = state->vmCtx;
	VM *vm = &vmCtx->vm;

	int argIdx = state->idx;
	if (IS_NUMBER(state->args[argIdx])) {
		dumpNumber(AS_NUMBER(state->args[argIdx]), state, spec, error);
		return;
	}

	ExecContext execCtx = EXEC_CTX_INITIALIZER(vmCtx);
	Value strVal = toString(&execCtx, state->args[argIdx]);
	if (ELOX_UNLIKELY(execCtx.error)) {
		ERROR(error, strVal);
		return;
	}
	push(vmCtx, strVal);

	DBG_PRINT_STACK("DBG0", vm);

	dumpString(AS_STRING(strVal), state, spec, error);
	if (ELOX_UNLIKELY(error->raised)) {
		popn(vm, 2);
		push(vmCtx, error->errorVal);
		return;
	}

	pop(vm);
}

Value stringFmt(VMCtx *vmCtx, int argCount, Value *args) {
	ObjString *inst = AS_STRING(args[0]);

	FmtState state = {
		.vmCtx = vmCtx,
		.ptr = inst->string.chars,
		.end = inst->string.chars + inst->string.length,
		.args = args,
		.maxArg = argCount - 1,
		.autoIdx = 0
	};

	initHeapStringWithSize(vmCtx, &state.output, inst->string.length + 1);

	while (state.ptr < state.end) {
		const char *ptr = state.ptr;

		while ((ptr < state.end) && (*ptr != '{') && (*ptr != '}'))
			ptr++;
		addHeapString(vmCtx, &state.output, state.ptr, ptr - state.ptr);
		state.ptr = ptr;

		if (state.ptr >= state.end)
			break;
		if (state.ptr[0] == state.ptr[1]) {
			// escaped bracket
			addHeapString(vmCtx, &state.output, state.ptr, 1);
			state.ptr += 2;
		} else {
			if ((*state.ptr++ == '}') || (state.ptr >= state.end)) {
				freeHeapString(vmCtx, &state.output);
				return runtimeError(vmCtx, "Single '%c' in format string", *(state.ptr - 1));
			}

			Error error = { .vmCtx = vmCtx };

			FmtSpec spec = { 0 };
			parse(&state, &spec, &error);
			if (ELOX_UNLIKELY(error.raised)) {
				freeHeapString(vmCtx, &state.output);
				return EXCEPTION_VAL;
			}
			dump(&state, &spec, &error);
			if (ELOX_UNLIKELY(error.raised)) {
				freeHeapString(vmCtx, &state.output);
				return EXCEPTION_VAL;
			}
		}
	}

	HeapCString *output = &state.output;
	return OBJ_VAL(takeString(vmCtx, output->chars, output->length, output->capacity));
}
