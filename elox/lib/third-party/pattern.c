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
#include <string.h>

// Maximum of captures we can return.
#define MAX_CAPTURES 32

#define MAXDEPTH 200

// Pattern escape"character
#define PATTERN_ESC		'%'

#define QL(x)       "'" x "'"

#define uchar(c)    ((unsigned char)(c))

#define CAP_UNFINISHED	(-1)
#define CAP_POSITION	(-2)

typedef enum {
	REPL_STRING,
	REPL_CALLABLE
} ReplType;

typedef struct MatchState {
	RunCtx *runCtx;
	int matchdepth;
	const char *src_init;
	const char *src_end;
	const char *p_end;
	int16_t level; // total number of captures (finished or unfinished)
	struct {
		const char *init;
		ptrdiff_t len;
	} capture[MAX_CAPTURES];
	Value repl;
	ReplType replType;
} MatchState;

static int check_capture(MatchState *ms, int l, EloxError *error) {
	l -= '1';
	ELOX_CHECK_THROW_RET_VAL((l >= 0) && (l < ms->level) && (ms->capture[l].len != CAP_UNFINISHED),
							 error, RTERR(ms->runCtx, "invalid capture index %%%d", l + 1), -1);
	return l;
}

static int capture_to_close(MatchState *ms, EloxError *error) {
	int level = ms->level;
	for (level--; level>=0; level--)
		if (ms->capture[level].len == CAP_UNFINISHED)
			return level;
	ELOX_THROW_RET_VAL(error, RTERR(ms->runCtx, "invalid pattern capture"), -1);
}

static const char *classend(MatchState *ms, const char *p, EloxError *error) {
	RunCtx *runCtx = ms->runCtx;

	switch (*p++) {
		case PATTERN_ESC: {
			ELOX_CHECK_THROW_RET_VAL(p != ms->p_end, error,
									 RTERR(runCtx, "malformed pattern (ends with " QL("%%") ")"), NULL);
			return p+1;
		}
		case '[': {
			if (*p == '^')
				p++;
			do {  // look for a `]'
				ELOX_CHECK_THROW_RET_VAL(p != ms->p_end, error,
										 RTERR(runCtx, "malformed pattern (missing " QL("]") ")"), NULL);
				if (*(p++) == PATTERN_ESC && p < ms->p_end)
					p++;  // skip escapes (e.g. `%]')
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
		p++;  // skip the `^'
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
				return 1;  // matches any char
			case PATTERN_ESC:
				return match_class(c, uchar(*(p + 1)));
			case '[':
				return matchbracketclass(c, p, ep - 1);
			default:
				return (uchar(*p) == c);
		}
	}
}

static const char *matchbalance(MatchState *ms, const char *s, const char *p, EloxError *error) {
	ELOX_CHECK_THROW_RET_VAL(p < ms->p_end - 1, error,
							 RTERR(ms->runCtx, "malformed pattern (missing arguments to " QL("%%b") ")"), NULL);
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

static const char *match(MatchState *ms, const char *s, const char *p, EloxError *error);

static const char *max_expand(MatchState *ms, const char *s, const char *p, const char *ep,
							  EloxError *error) {
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

static const char *min_expand(MatchState *ms, const char *s, const char *p, const char *ep,
							  EloxError * error) {
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

static const char *start_capture(MatchState *ms, const char *s, const char *p, int what,
								 EloxError *error) {
	const char *res;
	int level = ms->level;

	ELOX_CHECK_THROW_RET_VAL(level < MAX_CAPTURES, error, RTERR(ms->runCtx, "too many captures"), NULL);
	ms->capture[level].init = s;
	ms->capture[level].len = what;
	ms->level = level + 1;
	res = match(ms, s, p, error);
	if (ELOX_UNLIKELY(error->raised))
		return NULL;
	if (res == NULL)  // match failed?
		ms->level--;  // undo capture
	return res;
}

static const char *end_capture(MatchState *ms, const char *s, const char *p, EloxError *error) {
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

static const char *match_capture(MatchState *ms, const char *s, int l, EloxError *error) {
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

static const char *match(MatchState *ms, const char *s, const char *p, EloxError *error) {
	RunCtx *runCtx = ms->runCtx;

	ELOX_CHECK_THROW_RET_VAL(ms->matchdepth-- != 0, error, RTERR(runCtx, "pattern too complex"), NULL);
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
						ELOX_CHECK_THROW_RET_VAL(*p == '[', error,
												 RTERR(runCtx, "missing " QL("[") " after " QL("%%f") " in pattern"), NULL);
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

/// Translates a relative string position: negative means back from end
static ptrdiff_t posrelat(ptrdiff_t pos, size_t len) {
	if (pos >= 0)
		return (size_t)pos;
	else if (0u - (size_t)pos > len)
		return -1;
	else
		return len - ((size_t)-pos);
}

static Value getCapture(MatchState *ms, int i, const char *s, const char *e, EloxError *error) {
	RunCtx *runCtx = ms->runCtx;

	if (i >= ms->level) { // TODO ??? >=
		if (i == 0) {  /* ms->level == 0, too */
			ObjString *str = copyString(runCtx, (const uint8_t *)s, e - s); /* add whole match */
			ELOX_CHECK_THROW_RET_VAL(str != NULL, error, OOM(runCtx), NIL_VAL);
			return OBJ_VAL(str);
		} else
			ELOX_THROW_RET_VAL(error, RTERR(runCtx, "invalid capture index"), NIL_VAL);
	} else {
		ptrdiff_t l = ms->capture[i].len;
			ELOX_CHECK_THROW_RET_VAL(l != CAP_UNFINISHED, error, RTERR(runCtx, "unfinished capture"), NIL_VAL);
			if (l == CAP_POSITION)
				return NUMBER_VAL(ms->capture[i].init - ms->src_init);
			else {
				ObjString *str = copyString(runCtx, (const uint8_t *)ms->capture[i].init, l);
				ELOX_CHECK_THROW_RET_VAL(str != NULL, error, OOM(runCtx), NIL_VAL);
				return OBJ_VAL(str);
			}
	}
}

static int16_t getNumCaptures(MatchState *ms, const char *s) {
	return (ms->level == 0 && s) ? 1 : ms->level;
}

static void addCapturesToTuple(MatchState *ms, ObjArray *tuple, const char *s, const char *e,
							   EloxError *error) {
	RunCtx *runCtx = ms->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	int nLevels = getNumCaptures(ms, s);
	for (int i = 0; i < nLevels; i++) {
		Value cap = getCapture(ms, i, s, e, error);
		if (ELOX_UNLIKELY(error->raised))
			return;
		push(fiber, cap);
		bool res = appendToArray(runCtx, tuple, cap);
		ELOX_CHECK_THROW_RET(res, error, OOM(runCtx));
		pop(fiber);
	}
}

static const char *memFind (const char *s1, size_t l1, const char *s2, size_t l2) {
	if (l2 == 0)
		return s1;  /* empty strings are everywhere */
	else if (l2 > l1)
		return NULL;  /* avoids a negative `l1' */
	else {
		const char *init;  /* to search for a `*s2' inside `s1' */
		l2--;  /* 1st char will be checked by `memchr' */
		l1 = l1-l2;  /* `s2' cannot be found after that */
		while (l1 > 0 && (init = (const char *)memchr(s1, *s2, l1)) != NULL) {
			init++;   /* 1st char is already checked */
			if (memcmp(init, s2 + 1, l2) == 0)
				return init - 1;
			else {  /* correct `l1' and `s1' to try again */
				l1 -= init - s1;
				s1 = init;
			}
		}
		return NULL;
	}
}

static Value doMatch(Args *args, bool plain, bool retPos) {
	RunCtx *runCtx = args->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	ObjString *inst = AS_STRING(getValueArg(args, 0));
	ObjString *pattern = AS_STRING(getValueArg(args, 1));

	const char *s = (const char *)inst->string.chars;
	int ls = inst->string.length;
	const char *p = (const char *)pattern->string.chars;
	int lp = pattern->string.length;

	Value initVal = getValueArg(args, 2);
	ptrdiff_t init = IS_NIL(initVal) ? 0 : AS_NUMBER(initVal);
	init = posrelat(init, inst->string.length);

	if (init < 0)
		init = 0;
	else if (init > ls)
		return NIL_VAL;

	EloxError error = ELOX_ERROR_INITIALIZER;

	if (plain) {
		const char *s2 = memFind(s + init, ls - init + 1, p, lp);
		if (s2) {
				ObjArray *ret = newArray(runCtx, 2, OBJ_TUPLE);
				ELOX_CHECK_THROW_RET_VAL(ret != NULL, &error, OOM(runCtx), EXCEPTION_VAL);
				push(fiber, OBJ_VAL(ret));
				bool res;
				res = appendToArray(runCtx, ret, NUMBER_VAL(s2 - s));
				ELOX_CHECK_THROW_RET_VAL(res, &error, OOM(runCtx), EXCEPTION_VAL);
				res = appendToArray(runCtx, ret, NUMBER_VAL(s2 - s + lp - 1));
				ELOX_CHECK_THROW_RET_VAL(res, &error, OOM(runCtx), EXCEPTION_VAL);
				pop(fiber);
				return OBJ_VAL(ret);
		}
	} else {
		MatchState state = {
			.runCtx = runCtx,
			.matchdepth = MAXDEPTH,
			.src_init = s,
			.src_end = s + ls,
			.p_end = p + lp
		};

		const char *s1 = s + init;
		bool anchor = (lp > 0) && (*p == '^');
		if (anchor) {
			p++;
			lp--;
		}

		do {
			const char *res;
			state.level = 0;
			ELOX_CHECK_THROW_RET_VAL(state.matchdepth == MAXDEPTH, &error,
									 RTERR(runCtx, "state.matchdepth != MAXDEPTH"), EXCEPTION_VAL);
			res = (match(&state, s1, p, &error));
			if (ELOX_UNLIKELY(error.raised))
				return EXCEPTION_VAL;
			if (res != NULL) {
				int16_t numCaptures = getNumCaptures(&state, s1);
				ObjArray *ret = newArray(runCtx, numCaptures + 2 * (int)retPos, OBJ_TUPLE);
				ELOX_CHECK_THROW_RET_VAL(ret != NULL, &error, OOM(runCtx), EXCEPTION_VAL);
				push(fiber, OBJ_VAL(ret));
				if (retPos) {
					bool r;
					r = appendToArray(runCtx, ret, NUMBER_VAL(s1 - s));
					ELOX_CHECK_THROW_RET_VAL(r, &error, OOM(runCtx), EXCEPTION_VAL);
					r = appendToArray(runCtx, ret, NUMBER_VAL(res - s - 1));
					ELOX_CHECK_THROW_RET_VAL(r, &error, OOM(runCtx), EXCEPTION_VAL);
				}
				addCapturesToTuple(&state, ret, s1, res, &error);
				if (ELOX_UNLIKELY(error.raised))
					return EXCEPTION_VAL;
				pop(fiber);
				return OBJ_VAL(ret);
			}
		} while (s1++ < state.src_end && !anchor);
	}

	return NIL_VAL;
}

Value stringMatch(Args *args) {
	return doMatch(args, false, false);
}

Value stringFind(Args *args) {
	return doMatch(args, true, true);
}

Value stringFindMatch(Args *args) {
	return doMatch(args, false, true);
}

static void addValue(RunCtx *runCtx, HeapCString *b, Value val) {
	FiberCtx *fiber = runCtx->activeFiber;

	if (IS_STRING(val)) {
		ObjString *str = AS_STRING(val);
		push(fiber, val);
		heapStringAddString(runCtx, b, str->string.chars, str->string.length);
		pop(fiber);
	} else { // integer
		int num = AS_NUMBER(val);
		heapStringAddFmt(runCtx, b, "%d", num);
	}
}

static void add_s(MatchState *ms, HeapCString *b, const char *s, const char *e, EloxError *error) {
	RunCtx *runCtx = ms->runCtx;

	size_t i;
	ObjString *repl = AS_STRING(ms->repl);
	const char *news = (const char *)repl->string.chars;
	size_t l = repl->string.length;
	for (i = 0; i < l; i++) {
		if (news[i] != PATTERN_ESC)
			heapStringAddChar(runCtx, b, news[i]);
		else {
			i++;  // skip ESC
			if (!isdigit(uchar(news[i]))) {
				ELOX_CHECK_THROW_RET(news[i] == PATTERN_ESC, error,
									 RTERR(runCtx, "invalid use of " QL("%c") " in replacement string", PATTERN_ESC));
				heapStringAddChar(runCtx, b, news[i]);
			} else if (news[i] == '0')
				heapStringAddString(runCtx, b, (const uint8_t *)s, e - s);
			else {
				Value cap = getCapture(ms, news[i] - '1', s, e, error);
				if (ELOX_UNLIKELY(error->raised))
					return;
				addValue(runCtx, b, cap); // add capture to accumulated result
			}
		}
	}
}

static void add_value(MatchState *ms, HeapCString *b, const char *s, const char *e, EloxError *error) {
	RunCtx *runCtx = ms->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	Value repl;
	switch (ms->replType) {
		case REPL_CALLABLE: {
			push(fiber, ms->repl);
			int n = getNumCaptures(ms, s);
			for (int i = 0; i < n; i++) {
				push(fiber, getCapture(ms, i, s, e, error));
				if (ELOX_UNLIKELY(error->raised))
					return;
			}
			repl = runCall(runCtx, n);
			if (ELOX_UNLIKELY(IS_EXCEPTION(repl)))
				return;
			pop(fiber);
			break;
		}
		default: { // number or string
			add_s(ms, b, s, e, error);
			return;
		}
	}
	if (IS_NIL(repl) || (IS_BOOL(repl) && (AS_BOOL(repl) == false))) {
		heapStringAddString(runCtx, b, (const uint8_t *)s, e - s);  // keep original text
		return;
	} else if (!IS_STRING(repl))
		ELOX_THROW_RET(error, RTERR(runCtx, "invalid replacement value"));
	push(fiber, repl);
	ObjString *str = AS_STRING(repl);
	heapStringAddString(runCtx, b, str->string.chars, str->string.length); // add result to accumulator
	pop(fiber);
}

Value stringGsub(Args *args) {
	RunCtx *runCtx = args->runCtx;

	ObjString *inst = AS_STRING(getValueArg(args, 0));
	ObjString *pattern = AS_STRING(getValueArg(args, 1));

	Value repl = getValueArg(args, 2);
	ReplType replType;
	if (isCallable(repl))
		replType = REPL_CALLABLE;
	else if (IS_STRING(repl))
		replType = REPL_STRING;
	else
		return runtimeError(runCtx, "Invalid replacement type");

	Value maxSVal = getValueArg(args, 3);
	unsigned int max_s = IS_NIL(maxSVal)
						 ? inst->string.length + 1
						 : AS_NUMBER(maxSVal);

	const char *src = (const char *)inst->string.chars;
	int srcl = inst->string.length;
	const char *p = (const char *)pattern->string.chars;
	int lp = pattern->string.length;

	bool anchor = (lp > 0) && (*p == '^');
	if (anchor) { // skip anchor character
		p++;
		lp--;
	}

	MatchState state = {
		.runCtx = runCtx,
		.matchdepth = MAXDEPTH,
		.src_init = src,
		.src_end = src + srcl,
		.p_end = p + lp,
		.repl = repl,
		.replType = replType
	};

	HeapCString output;
	initHeapStringWithSize(runCtx, &output, srcl + 1);

	EloxError error = ELOX_ERROR_INITIALIZER;

	size_t n = 0;
	while (n < max_s) {
		const char *e;
		state.level = 0;
		ELOX_CHECK_THROW_GOTO(state.matchdepth == MAXDEPTH, &error,
							  RTERR(runCtx, "state.matchdepth != MAXDEPTH"), error);
		e = match(&state, src, p, &error);
		if (ELOX_UNLIKELY(error.raised))
			goto error;
		if (e) {
			n++;
			add_value(&state, &output, src, e, &error);
			if (ELOX_UNLIKELY(error.raised))
				goto error;
		}
		if (e && e > src) /* non empty match? */
			src = e;  /* skip it */
		else if (src < state.src_end)
			heapStringAddChar(runCtx, &output, *src++);
		else
			break;
		if (anchor)
			break;
	}

	heapStringAddString(runCtx, &output, (const uint8_t *)src, state.src_end - src);

	ObjString *str = takeString(runCtx, output.chars, output.length, output.capacity);
	if (ELOX_UNLIKELY(str == NULL)) {
		oomError(runCtx);
		error.raised = true;
		goto error;
	}
	return OBJ_VAL(str);

error:
	freeHeapString(runCtx, &output);
	return EXCEPTION_VAL;
}

enum {
	GMATCH_DONE = -1,
	GMATCH_ERROR = -2
};

static Value gmatchGetNext(RunCtx *runCtx, ObjInstance *inst, int32_t offset, EloxError *error) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	struct BIGmatchIterator *gi = &vm->builtins.biGmatchIterator;

	ObjString *string = AS_STRING(inst->fields.values[gi->_string]);
	ObjString *pattern = AS_STRING(inst->fields.values[gi->_pattern]);

	const char *s = (const char *)string->string.chars;
	size_t ls = string->string.length;
	const char *p = (const char *)pattern->string.chars;
	size_t lp = pattern->string.length;

	MatchState state = {
		.runCtx = runCtx,
		.matchdepth = MAXDEPTH,
		.src_init = s,
		.src_end = s + ls,
		.p_end = p + lp
	};

	for (const char *src = s + offset; src <= state.src_end; src++) {
		const char *e;
		state.level = 0;
		ELOX_CHECK_THROW_RET_VAL(state.matchdepth == MAXDEPTH, error,
								 RTERR(runCtx, "state.matchdepth != MAXDEPTH"), EXCEPTION_VAL);
		if ((e = match(&state, src, p, error)) != NULL) {
			if (ELOX_UNLIKELY(error->raised))
				return EXCEPTION_VAL;
			int32_t newStart = e - s;
			if (e == src)
				newStart++;  // empty match? advance at least one position
			inst->fields.values[gi->_offset] = NUMBER_VAL(newStart);
			int16_t numCaptures = getNumCaptures(&state, src);
			ObjArray *ret = newArray(runCtx, numCaptures, OBJ_TUPLE);
			ELOX_CHECK_THROW_RET_VAL(ret != NULL, error, OOM(runCtx), EXCEPTION_VAL);
			push(fiber, OBJ_VAL(ret));
			addCapturesToTuple(&state, ret, src, e, error);
			if (ELOX_UNLIKELY(error->raised))
				return EXCEPTION_VAL;
			pop(fiber);
			return OBJ_VAL(ret);
		}
	}

	inst->fields.values[gi->_offset] = NUMBER_VAL(GMATCH_DONE);
	return NIL_VAL;
}

Value gmatchIteratorHasNext(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;

	struct BIGmatchIterator *gi = &vm->builtins.biGmatchIterator;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));

	int32_t offset = AS_NUMBER(inst->fields.values[gi->_offset]);
	if (offset < 0)
		return BOOL_VAL(false);

	Value cachedNext = inst->fields.values[gi->_cachedNext];
	if (!IS_NIL(cachedNext))
		return BOOL_VAL(true);

	EloxError error = ELOX_ERROR_INITIALIZER;
	inst->fields.values[gi->_cachedNext] = gmatchGetNext(runCtx, inst, offset, &error);
	if (ELOX_UNLIKELY(error.raised)) {
		inst->fields.values[gi->_offset] = NUMBER_VAL(GMATCH_ERROR);
		return EXCEPTION_VAL;
	}

	offset = AS_NUMBER(inst->fields.values[gi->_offset]);
	return BOOL_VAL(offset >= 0);
}

Value gmatchIteratorNext(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;

	struct BIGmatchIterator *gi = &vm->builtins.biGmatchIterator;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));

	int32_t offset = AS_NUMBER(inst->fields.values[gi->_offset]);
	if (offset < 0) {
		switch(offset) {
			case GMATCH_DONE:
				return runtimeError(runCtx, "Gmatch already completed");
			case GMATCH_ERROR:
				return runtimeError(runCtx, "Error already raised during gmatch");
		}
	}

	Value cachedNext = inst->fields.values[gi->_cachedNext];
	if (!IS_NIL(cachedNext)) {
		inst->fields.values[gi->_cachedNext] = NIL_VAL;
		return cachedNext;
	}

	EloxError error = ELOX_ERROR_INITIALIZER;
	Value next = gmatchGetNext(runCtx, inst, offset, &error);
	if (ELOX_UNLIKELY(error.raised)) {
		inst->fields.values[gi->_offset] = NUMBER_VAL(GMATCH_ERROR);
		return EXCEPTION_VAL;
	}

	offset = AS_NUMBER(inst->fields.values[gi->_offset]);
	if (offset < 0)
		return runtimeError(runCtx, "Gmatch already completed");

	return next;
}

Value stringGmatch(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;

	struct BIGmatchIterator *gi = &vm->builtins.biGmatchIterator;

	ObjString *inst = AS_STRING(getValueArg(args, 0));
	ObjString *pattern = AS_STRING(getValueArg(args, 1));

	ObjInstance *iter = newInstance(runCtx, gi->_class);
	if (ELOX_UNLIKELY(iter == NULL))
		return oomError(runCtx);
	iter->fields.values[gi->_string] = OBJ_VAL(inst);
	iter->fields.values[gi->_pattern] = OBJ_VAL(pattern);
	iter->fields.values[gi->_offset] = NUMBER_VAL(0);
	iter->fields.values[gi->_cachedNext] = NIL_VAL;
	return OBJ_VAL(iter);
}
