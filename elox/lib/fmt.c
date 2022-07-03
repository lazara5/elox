#include "elox/vm.h"
#include "elox/state.h"

#include <limits.h>
#include <math.h>

typedef struct FmtState {
	VMCtx *vmCtx;
	const char *ptr;
	const char *end;
	int autoIdx;
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
		error->errorVal = runtimeError(error->vmCtx, fmt, ## __VA_ARGS__); \
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
	state->autoIdx++;
	return state->autoIdx;
}

static int getSpecificIdx(FmtState *state, int idx, Error *error) {
	if (state->autoIdx > 1) {
		RAISE(error, "Cannot mix auto and specific field numbering");
		return -1;
	}
	return idx;
}

static bool parseUInt(FmtState *state, int *val, Error *error) {
	const char *ptr = state->ptr;
	unsigned int base = 0;
	while ((ptr < state->end) && isDigit(*ptr)) {
		int digit = *ptr++ - '0';
		bool validDigit = (base < INT_MAX / 10) ||
						  (base == INT_MAX / 10 && digit <= INT_MAX % 10);
		if (!validDigit) {
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
		// TODO
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

static void addZeroPadding(FmtState *state, FmtSpec *spec, int len) {
	char *padding = reserveHeapString(state->vmCtx, &state->output, len);
	if (len > state->zeroPadding) {
		int prefix = (len - state->zeroPadding) % 4;
		if (prefix > 2)
			*padding++ = '0';
		if (prefix > 0) {
			*padding++ = '0';
			*padding++ = spec->grouping;
		}
		len -= prefix;
		while (len > 4) {
			padding[0] = padding[1] = padding[2] = '0';
			padding[3] = spec->grouping;
			padding += 4;
			len -= 4;
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

#define INT_FMT_BUFFER_SIZE 100
#define FMT_GROUPING_WIDTH 3

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
		ptr= dp;
	}
	String str = { .chars = ptr, .length = INT_FMT_BUFFER_SIZE - (ptr - buffer)};
	addString(&str, state, spec, false, width);
}

static void dumpDouble() {

}

static void dumpNumber(double val, FmtState *state, FmtSpec *spec, Error *error) {
	char type = spec->type;
	if (type == 0)
		type = (trunc(val) == val) ? 'd' : 'g';
	switch (type) {
		case 'd':
		case 'b':
		case 'B':
		case 'o':
		case 'O':
		case 'x':
		case 'X':
			dumpInt((int64_t)val, state, spec);
			break;
		case 'g':
			dumpDouble();
			break;
		default:
			RAISE(error, "Unknown format '%c' for number argument", type);
			break;
	}
}

static void dumpString(ObjString *str, FmtState *state, FmtSpec *spec, Error *error) {
	addString(&str->string, state, spec, true, spec->width);
}

static void dump(FmtState *state, FmtSpec *spec, Value *args, Error *error) {
	VMCtx *vmCtx = state->vmCtx;
	VM *vm = &vmCtx->vm;

	int argIdx = state->idx;
	if (IS_NUMBER(args[argIdx])) {
		dumpNumber(AS_NUMBER(args[argIdx]), state, spec, error);
		return;
	}

	ExecContext execCtx = EXEC_CTX_INITIALIZER(vmCtx);
	Value strVal = toString(&execCtx, args[argIdx]);
	if (ELOX_UNLIKELY(execCtx.error)) {
		ERROR(error, strVal);
		return;
	}
	push(vmCtx, strVal);

	dumpString(AS_STRING(strVal), state, spec, error);

	pop(vm);
}

Value stringFmt(VMCtx *vmCtx, int argCount, Value *args) {
	ObjString *inst = AS_STRING(args[0]);

	FmtState state = {
		.vmCtx = vmCtx,
		.ptr = inst->string.chars,
		.end = inst->string.chars + inst->string.length,
		.maxArg = argCount - 1
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
				return error.errorVal;
			}
			dump(&state, &spec, args, &error);
			if (ELOX_UNLIKELY(error.raised)) {
				freeHeapString(vmCtx, &state.output);
				return error.errorVal;
			}
		}
	}

	HeapCString *output = &state.output;
	return OBJ_VAL(takeString(vmCtx, output->chars, output->length, output->capacity));
}
