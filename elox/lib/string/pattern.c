/******************************************************************************
* Copyright (C) 1994-2015 Lua.org, PUC-Rio.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************/

// Heavily based on the Lua regular expression matching library written by Roberto Ierusalimschy

#include "elox/vm.h"
#include "elox/state.h"

#include <ctype.h>

// Maximum of captures we can return.
#define MAX_CAPTURES 32

#define MAXDEPTH 200

// Pattern escape"character
#define PATTERN_ESC		'%'

#define QL(x)       "'" x "'"

#define uchar(c)        ((unsigned char)(c))

#define CAP_UNFINISHED	(-1)
#define CAP_POSITION	(-2)

#define RAISE(error, fmt, ...) \
	if (!(error)->raised) { \
		(error)->raised = true; \
		runtimeError((error)->vmCtx, fmt, ## __VA_ARGS__); \
		(error)->errorVal = peek(&((error)->vmCtx->vm), 0); \
	}

typedef struct MatchState {
	VMCtx *vmCtx;
	int matchdepth;
	const char *src_init;
	const char *src_end;
	const char *p_end;
	int level; // total number of captures (finished or unfinished)
	struct {
		const char *init;
		ptrdiff_t len;
	} capture[MAX_CAPTURES];
	HeapCString output;
} MatchState;

typedef struct Error {
	VMCtx *vmCtx;
	bool raised;
	Value errorVal;
} Error;

static int check_capture(MatchState *ms, int l, Error *error) {
	l -= '1';
	if (l < 0 || l >= ms->level || ms->capture[l].len == CAP_UNFINISHED) {
		RAISE(error, "invalid capture index %%%d", l + 1);
		return -1;
	}
	return l;
}

static int capture_to_close(MatchState *ms, Error *error) {
	int level = ms->level;
	for (level--; level>=0; level--)
		if (ms->capture[level].len == CAP_UNFINISHED)
			return level;
	RAISE(error, "invalid pattern capture");
	return -1;
}

static const char *classend(MatchState *ms, const char *p, Error *error) {
	switch (*p++) {
		case PATTERN_ESC: {
			if (p == ms->p_end) {
				RAISE(error, "malformed pattern (ends with " QL("%%") ")");
				return NULL;
			}
			return p+1;
		}
		case '[': {
			if (*p == '^')
				p++;
			do {  /* look for a `]' */
				if (p == ms->p_end) {
					RAISE(error, "malformed pattern (missing " QL("]") ")");
					return NULL;
				}
				if (*(p++) == PATTERN_ESC && p < ms->p_end)
					p++;  /* skip escapes (e.g. `%]') */
			} while (*p != ']');
			return p + 1;
		}
		default: {
			return p;
		}
	}
}

static int match_class (int c, int cl) {
	int res;
	switch (tolower(cl)) {
		case 'a' :
			res = isalpha(c);
			break;
		case 'c' :
			res = iscntrl(c);
			break;
		case 'd' :
			res = isdigit(c);
			break;
		case 'g' :
			res = isgraph(c);
			break;
		case 'l' :
			res = islower(c);
			break;
	case 'p' : res = ispunct(c); break;
	case 's' : res = isspace(c); break;
	case 'u' : res = isupper(c); break;
	case 'w' : res = isalnum(c); break;
	case 'x' : res = isxdigit(c); break;
	case 'z' : res = (c == 0); break;  /* deprecated option */
	default: return (cl == c);
  }
  return (islower(cl) ? res : !res);
}


static int matchbracketclass(int c, const char *p, const char *ec) {
	int sig = 1;
	if (*(p + 1) == '^') {
		sig = 0;
		p++;  /* skip the `^' */
	}
	while (++p < ec) {
		if (*p == PATTERN_ESC) {
			p++;
			if (match_class(c, uchar(*p)))
				return sig;
		} else if ((*(p + 1) == '-') && (p + 2 < ec)) {
			p += 2;
			if (uchar(*(p - 2)) <= c && c <= uchar(*p))
				return sig;
		} else if (uchar(*p) == c)
			return sig;
	}
	return !sig;
}

static int singlematch(MatchState *ms, const char *s, const char *p, const char *ep) {
	if (s >= ms->src_end)
		return 0;
	else {
		int c = uchar(*s);
		switch (*p) {
			case '.':
				return 1;  /* matches any char */
			case PATTERN_ESC:
				return match_class(c, uchar(*(p + 1)));
			case '[':
				return matchbracketclass(c, p, ep - 1);
			default:
				return (uchar(*p) == c);
		}
	}
}

static const char *matchbalance(MatchState *ms, const char *s, const char *p, Error *error) {
	if (p >= ms->p_end - 1) {
		RAISE(error, "malformed pattern (missing arguments to " QL("%%b") ")");
		return NULL;
	}
	if (*s != *p)
		return NULL;
	else {
		int b = *p;
		int e = *(p+1);
		int cont = 1;
		while (++s < ms->src_end) {
			if (*s == e) {
				if (--cont == 0)
					return s+1;
			} else if (*s == b)
				cont++;
		}
	}
	return NULL;  /* string ends out of balance */
}

static const char *match(MatchState *ms, const char *s, const char *p, Error *error);

static const char *max_expand(MatchState *ms, const char *s, const char *p, const char *ep, Error *error) {
	ptrdiff_t i = 0;  /* counts maximum expand for item */
	while (singlematch(ms, s + i, p, ep))
		i++;
	/* keeps trying to match with the maximum repetitions */
	while (i>=0) {
		const char *res = match(ms, (s + i), ep + 1, error);
		if (ELOX_UNLIKELY(error->raised))
			return NULL;
		if (res)
			return res;
		i--;  /* else didn't match; reduce 1 repetition to try again */
	}
	return NULL;
}

static const char *min_expand(MatchState *ms, const char *s, const char *p, const char *ep, Error * error) {
	for (;;) {
		const char *res = match(ms, s, ep + 1, error);
		if (ELOX_UNLIKELY(error->raised))
			return NULL;
		if (res != NULL)
			return res;
		else if (singlematch(ms, s, p, ep))
			s++;  /* try with one more repetition */
		else
			return NULL;
	}
}

static const char *start_capture(MatchState *ms, const char *s, const char *p, int what, Error *error) {
	const char *res;
	int level = ms->level;
	if (level >= MAX_CAPTURES) {
		RAISE(error, "too many captures");
		return NULL;
	}
	ms->capture[level].init = s;
	ms->capture[level].len = what;
	ms->level = level + 1;
	res = match(ms, s, p, error);
	if (ELOX_UNLIKELY(error->raised))
		return NULL;
	if (res == NULL)  /* match failed? */
		ms->level--;  /* undo capture */
	return res;
}

static const char *end_capture(MatchState *ms, const char *s, const char *p, Error *error) {
	int l = capture_to_close(ms, error);
	if (ELOX_UNLIKELY(error->raised))
		return NULL;
	const char *res;
	ms->capture[l].len = s - ms->capture[l].init;  /* close capture */
	res = match(ms, s, p, error);
	if (ELOX_UNLIKELY(error->raised))
		return NULL;
	if (res == NULL)  /* match failed? */
		ms->capture[l].len = CAP_UNFINISHED;  /* undo capture */
	return res;
}

static const char *match_capture(MatchState *ms, const char *s, int l, Error *error) {
	size_t len;
	l = check_capture(ms, l, error);
	if (ELOX_UNLIKELY(error->raised))
		return NULL;
	len = ms->capture[l].len;
	if ((size_t)(ms->src_end - s) >= len && memcmp(ms->capture[l].init, s, len) == 0)
		return s + len;
	else
		return NULL;
}

static const char *match (MatchState *ms, const char *s, const char *p, Error *error) {
	if (ms->matchdepth-- == 0) {
		RAISE(error, "pattern too complex");
		return NULL;
	}
init: /* using goto's to optimize tail recursion */
	if (p != ms->p_end) {  /* end of pattern? */
		switch (*p) {
			case '(': {  /* start capture */
				if (*(p + 1) == ')')  /* position capture? */
					s = start_capture(ms, s, p + 2, CAP_POSITION, error);
				else
					s = start_capture(ms, s, p + 1, CAP_UNFINISHED, error);
				if (ELOX_UNLIKELY(error->raised))
					return NULL;
				break;
			}
			case ')': {  /* end capture */
				s = end_capture(ms, s, p + 1, error);
				if (ELOX_UNLIKELY(error->raised))
					return NULL;
				break;
			}
			case '$': {
				if ((p + 1) != ms->p_end)  /* is the `$' the last char in pattern? */
					goto dflt;  /* no; go to default */
				s = (s == ms->src_end) ? s : NULL;  /* check end of string */
				break;
			}
			case PATTERN_ESC: {  /* escaped sequences not in the format class[*+?-]? */
				switch (*(p + 1)) {
					case 'b': {  /* balanced string? */
						s = matchbalance(ms, s, p + 2, error);
						if (ELOX_UNLIKELY(error->raised))
							return NULL;
						if (s != NULL) {
							p += 4;
							goto init;  /* return match(ms, s, p + 4); */
						}  /* else fail (s == NULL) */
						break;
					}
					case 'f': {  /* frontier? */
						const char *ep;
						char previous;
						p += 2;
						if (*p != '[') {
							RAISE(error, "missing " QL("[") " after " QL("%%f") " in pattern");
							return NULL;
						}
						ep = classend(ms, p, error);  /* points to what is next */
						if (ELOX_UNLIKELY(error->raised))
							return NULL;
						previous = (s == ms->src_init) ? '\0' : *(s - 1);
						if (!matchbracketclass(uchar(previous), p, ep - 1) && matchbracketclass(uchar(*s), p, ep - 1)) {
							p = ep;
							goto init;  /* return match(ms, s, ep); */
						}
						s = NULL;  /* match failed */
						break;
					}
					case '0': case '1': case '2': case '3':
					case '4': case '5': case '6': case '7':
					case '8': case '9': {  /* capture results (%0-%9)? */
						s = match_capture(ms, s, uchar(*(p + 1)), error);
						if (ELOX_UNLIKELY(error->raised))
							return NULL;
						if (s != NULL) {
							p += 2;
							goto init;  /* return match(ms, s, p + 2) */
						}
						break;
					}
					default:
						goto dflt;
				}
				break;
			}
			default:
dflt:		{  /* pattern class plus optional suffix */
				const char *ep = classend(ms, p, error);  /* points to optional suffix */
				if (ELOX_UNLIKELY(error->raised))
					return NULL;
				/* does not match at least once? */
				if (!singlematch(ms, s, p, ep)) {
					if (*ep == '*' || *ep == '?' || *ep == '-') {  /* accept empty? */
						p = ep + 1;
						goto init;  /* return match(ms, s, ep + 1); */
					} else  /* '+' or no suffix */
						s = NULL;  /* fail */
				} else {  /* matched once */
					switch (*ep) {  /* handle optional suffix */
						case '?': {  /* optional */
							const char *res;
							res = match(ms, s + 1, ep + 1, error);
							if (ELOX_UNLIKELY(error->raised))
								return NULL;
							if (res != NULL)
								s = res;
							else {
								p = ep + 1;
								goto init;  /* else return match(ms, s, ep + 1); */
							}
							break;
						}
						case '+':  /* 1 or more repetitions */
							s++;  /* 1 match already done */
							// FALLTHROUGH
						case '*':  /* 0 or more repetitions */
							s = max_expand(ms, s, p, ep, error);
							if (ELOX_UNLIKELY(error->raised))
								return NULL;
							break;
						case '-':  /* 0 or more repetitions (minimum) */
							s = min_expand(ms, s, p, ep, error);
							if (ELOX_UNLIKELY(error->raised))
								return NULL;
							break;
						default:  /* no suffix */
							s++;
							p = ep;
							goto init;  /* return match(ms, s + 1, ep); */
					}
				}
				break;
			}
		}
	}
	ms->matchdepth++;
	return s;
}

/* translate a relative string position: negative means back from end */
static ptrdiff_t posrelat (ptrdiff_t pos, size_t len) {
	if (pos >= 0)
		return (size_t)pos;
	else if (0u - (size_t)pos > len)
		return -1;
	else
		return len - ((size_t)-pos);
}

static Value getCapture(MatchState *ms, int i, const char *s, const char *e, Error *error) {
	VMCtx *vmCtx = ms->vmCtx;

	if (i > ms->level) {
		if (i == 0)  /* ms->level == 0, too */
			return OBJ_VAL(copyString(vmCtx, s, e - s)); /* add whole match */
		else {
			RAISE(error, "invalid capture index");
			return NIL_VAL;
		}
	} else {
		ptrdiff_t l = ms->capture[i].len;
			if (l == CAP_UNFINISHED) {
				RAISE(error, "unfinished capture");
				return NIL_VAL;
			}
			if (l == CAP_POSITION)
				return NUMBER_VAL(ms->capture[i].init - ms->src_init);
			else
				return OBJ_VAL(copyString(vmCtx, ms->capture[i].init, l));
	}
}

static Value getCaptures(MatchState *ms, const char *s, const char *e, Error *error) {
	VMCtx *vmCtx = ms->vmCtx;
	VM *vm = &vmCtx->vm;

	int nlevels = (ms->level == 0 && s) ? 1 : ms->level;
	ObjArray *ret = newArray(vmCtx, 2, OBJ_TUPLE);
	push(vm, OBJ_VAL(ret));
	for (int i = 0; i < nlevels; i++) {
		Value cap = getCapture(ms, i, s, e, error);
		if (ELOX_UNLIKELY(error->raised)) {
			pop(vm);
			return NIL_VAL;
		}
		push(vm, cap);
		appendToArray(vmCtx, ret, cap);
		pop(vm);
	}
	pop(vm);
	return OBJ_VAL(ret);
}

Value stringMatch(VMCtx *vmCtx, int argCount ELOX_UNUSED, Args *args) {
	ObjString *inst = AS_STRING(getValueArg(args, 0));
	ObjString *pattern = AS_STRING(getValueArg(args, 1));

	const char *s = inst->string.chars;
	int ls = inst->string.length;
	const char *p = pattern->string.chars;
	int lp = pattern->string.length;

	Value initVal = getValueArg(args, 2);
	ptrdiff_t init = IS_NIL(initVal) ? 0 : AS_NUMBER(initVal);
	init = posrelat(init, inst->string.length);
	if (init < 0)
		init = 0;

	MatchState state = {
		.vmCtx = vmCtx,
		.matchdepth = MAXDEPTH,
		.src_init = s,
		.src_end = s + ls,
		.p_end = p + lp
	};

	const char *s1 = s + init;
	bool anchor = (pattern->string.length > 0) && (*p == '^');
	if (anchor) {
		p++;
		lp--;
	}

	Error error = { .vmCtx = vmCtx };

	do {
		const char *res;
		state.level = 0;
		if (ELOX_UNLIKELY(state.matchdepth != MAXDEPTH)) {
			RAISE(&error, "state.matchdepth != MAXDEPTH");
			return EXCEPTION_VAL;
		}
		res = (match(&state, s1, p, &error));
		if (ELOX_UNLIKELY(error.raised)) {
			return EXCEPTION_VAL;
		}
		if (res != NULL) {
			Value ret = getCaptures(&state, s1, res, &error);
			if (ELOX_UNLIKELY(error.raised)) {
				return EXCEPTION_VAL;
			}
			return ret;
		}
	} while (s1++ < state.src_end && !anchor);

	return NIL_VAL;
}

/*Value stringGsub(VMCtx *vmCtx, int argCount, Args *args) {
	ObjString *inst = AS_STRING(getValueArg(args, 0));
	ObjString *pattern = AS_STRING(getValueArg(args, 1));
	Value repl = getValueArg(args, 2);

	Value maxCountVal = getValueArg(args, 3);
	unsigned int maxCount = IS_NIL(maxCountVal) ? 0 : AS_NUMBER(maxCountVal);

	MatchState state = {
		.vmCtx = vmCtx,
		.src = inst->string.chars,
		.src_len = inst->string.length
	};

	initHeapStringWithSize(vmCtx, &state.output, inst->string.length + 1);

	Error error = { .vmCtx = vmCtx };

	unsigned int count = 0;

	if (ELOX_UNLIKELY(error.raised)) {
		freeHeapString(vmCtx, &state.output);
		return EXCEPTION_VAL;
	}

	HeapCString *output = &state.output;
	return OBJ_VAL(takeString(vmCtx, output->chars, output->length, output->capacity));
}*/
