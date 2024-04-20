// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/scanner.h>
#include <elox/state.h>
#include <elox/builtins/string.h>
#include <elox/third-party/utf8decoder.h>

#include <string.h>

void initScanner(CCtx *cCtx, uint8_t *source) {
	Scanner *scanner = &cCtx->scanner;
	scanner->start = source;
	scanner->current = source;
	scanner->line = 1;
}

bool isAtEnd(Scanner *scanner) {
	return *scanner->current == '\0';
}

static char scanLookBack(Scanner *scanner) {
	return scanner->current[-1];
}

static char scanPeek(Scanner *scanner) {
	return *scanner->current;
}

static char scanPeekNext(Scanner *scanner) {
	if (isAtEnd(scanner))
		return '\0';
	return scanner->current[1];
}

static char advance(Scanner *scanner) {
	scanner->current++;
	return scanner->current[-1];
}

#define INVALID_UTF8 0xFFFFFFFF

static uint32_t utf8_advance(Scanner *scanner) {
	uint32_t codepoint = 0;
	uint32_t state = 0;

	uint8_t ch;
	do {
		scanner->current++;
		ch = scanner->current[-1];
		switch (elox_utf8_decode(&state, &codepoint, ch)) {
			case ELOX_UTF8_ACCEPT:
				return codepoint;
			case ELOX_UTF8_REJECT:
				return INVALID_UTF8;
			default:
				break;
		}
	} while (ch > 0);

	return codepoint;
}

static uint8_t *getPos(Scanner *scanner) {
	return scanner->current;
}

static bool advanceIfMatch(Scanner *scanner, char expected) {
	if (isAtEnd(scanner))
		return false;
	if (*scanner->current != expected)
		return false;
	scanner->current++;
	return true;
}

static bool advanceIfMatch2(Scanner *scanner, char xa, char xb) {
	if (isAtEnd(scanner))
		return false;
	if (*scanner->current != xa)
		return false;
	scanner->current++;
	if (isAtEnd(scanner)) {
		scanner->current--;
		return false;
	}
	if (*scanner->current != xb) {
		scanner->current--;
		return false;
	}
	scanner->current++;
	return true;
}

static Token makeToken(Scanner *scanner, EloxTokenType type) {
	Token token;
	token.type = type;
	token.string.chars = scanner->start;
	token.string.length = (int)(scanner->current - scanner->start);
	token.line = scanner->line;
	return token;
}

static Token makeTrimmedToken(Scanner *scanner, EloxTokenType type, int len) {
	Token token;
	token.type = type;
	token.string.chars = scanner->start;
	token.string.length = len;
	token.line = scanner->line;
	return token;
}

// TODO: combine
static Token errorToken(Scanner *scanner, const char *message) {
	Token token;
	token.type = TOKEN_ERROR;
	token.string.chars = (const uint8_t *)message;
	token.string.length = (int)strlen(message);
	token.line = scanner->line;
	return token;
}

typedef enum {
	WSS_SCAN,
	WSS_STAR,
	WSS_DONE
} WSSState;

static bool skipWhitespace(Scanner *scanner) {
	for (;;) {
		char c = scanPeek(scanner);
		switch (c) {
			case ' ':
			case '\r':
			case '\t':
				advance(scanner);
				break;
			case '\n':
				scanner->line++;
				advance(scanner);
				break;
			case '#':
				if (scanPeekNext(scanner) == '*') {
					// multi-line comment
					advance(scanner);
					WSSState state = WSS_SCAN;
					while (!isAtEnd(scanner)) {
						char ch = advance(scanner);
						switch (state) {
							case WSS_SCAN:
								switch (ch) {
									case '*':
										state = WSS_STAR;
										break;
									case '\n':
										scanner->line++;
										break;
								}
								break;
							case WSS_STAR:
								switch (ch) {
									case '#':
										state = WSS_DONE;
										break;
									case '\n':
										scanner->line++;
										// FALLTHROUGH
									default:
										state = WSS_SCAN;
										break;
								}
								break;
							case WSS_DONE:
								// Unreachable
								break;
						}
						if (state == WSS_DONE)
							break;
					}
					if (state != WSS_DONE)
						return false;
				} else {
					// A comment goes until the end of the line.
					while (scanPeek(scanner) != '\n' && !isAtEnd(scanner))
						advance(scanner);
				}
				break;
			default:
				return true;
		}
	}
}

static EloxTokenType checkKeyword(Scanner *scanner, int start, int length, const char *rest,
								  EloxTokenType type) {
	if (scanner->current - scanner->start == start + length &&
			memcmp(scanner->start + start, rest, (size_t)length) == 0) {
		return type;
	}

	return TOKEN_IDENTIFIER;
}

static EloxTokenType identifierType(Scanner *scanner) {
	switch (scanner->start[0]) {
		case 'a':
			return checkKeyword(scanner, 1, 2, "nd", TOKEN_AND);
		case 'b':
			return checkKeyword(scanner, 1, 4, "reak", TOKEN_BREAK);
		case 'c':
			if (scanner->current - scanner->start > 1) {
				switch (scanner->start[1]) {
					case 'a':
						return checkKeyword(scanner, 2, 3, "tch", TOKEN_CATCH);
					case 'l':
						return checkKeyword(scanner, 2, 3, "ass", TOKEN_CLASS);
					case 'o':
						return checkKeyword(scanner, 2, 6, "ntinue", TOKEN_CONTINUE);
				}
			}
			break;
		case 'e':
			return checkKeyword(scanner, 1, 3, "lse", TOKEN_ELSE);
		case 'f':
			if (scanner->current - scanner->start > 1) {
				switch (scanner->start[1]) {
					case 'a':
						return checkKeyword(scanner, 2, 3, "lse", TOKEN_FALSE);
					case 'i':
						return checkKeyword(scanner, 2, 5, "nally", TOKEN_FINALLY);
					case 'o':
						if (scanner->current - scanner->start > 2) {
							switch (scanner->start[2]) {
								case 'r':
									if (scanner->current - scanner->start > 3) {
										if (scanner->start[3] == 'e')
											return checkKeyword(scanner, 3, 4, "each", TOKEN_FOREACH);
										else
											return checkKeyword(scanner, 3, 0, "", TOKEN_FOR);
									} else
										return checkKeyword(scanner, 3, 0, "", TOKEN_FOR);
							}
						}
						return checkKeyword(scanner, 2, 1, "r", TOKEN_FOR);
					case 'r':
						return checkKeyword(scanner, 2, 2, "om", TOKEN_FROM);
					case 'u':
						return checkKeyword(scanner, 2, 6, "nction", TOKEN_FUNCTION);
				}
			}
			break;
		case 'g':
			return checkKeyword(scanner, 1, 5, "lobal", TOKEN_GLOBAL);
		case 'i':
			if (scanner->current - scanner->start > 1) {
				switch (scanner->start[1]) {
					case 'f':
						return checkKeyword(scanner, 2, 0, "", TOKEN_IF);
					case 'm':
						return checkKeyword(scanner, 2, 4, "port", TOKEN_IMPORT);
					case 'n':
						if (scanner->current - scanner->start > 2) {
							if (scanner->start[2] == 's')
								return checkKeyword(scanner, 2, 8, "stanceof", TOKEN_INSTANCEOF);
							else
								return checkKeyword(scanner, 2, 0, "", TOKEN_IN);
						} else
							return checkKeyword(scanner, 2, 0, "", TOKEN_IN);
				}
			}
			break;
		case 'l':
			return checkKeyword(scanner, 1, 4, "ocal", TOKEN_LOCAL);
		case 'n':
			return checkKeyword(scanner, 1, 2, "il", TOKEN_NIL);
		case 'o':
			return checkKeyword(scanner, 1, 1, "r", TOKEN_OR);
		case 'r':
			return checkKeyword(scanner, 1, 5, "eturn", TOKEN_RETURN);
		case 's':
			return checkKeyword(scanner, 1, 4, "uper", TOKEN_SUPER);
		case 't':
			if (scanner->current - scanner->start > 1) {
				switch (scanner->start[1]) {
					case 'h':
						if (scanner->current - scanner->start > 2) {
							switch (scanner->start[2]) {
								case 'i':
									return checkKeyword(scanner, 3, 1, "s", TOKEN_THIS);
								case 'r':
									return checkKeyword(scanner, 3, 2, "ow", TOKEN_THROW);
							}
						}
						break;
					case 'r':
						if (scanner->current - scanner->start > 2) {
							switch (scanner->start[2]) {
								case 'u':
									return checkKeyword(scanner, 3, 1, "e", TOKEN_TRUE);
								case 'y':
									return checkKeyword(scanner, 3, 0, "", TOKEN_TRY);
							}
						}
						break;
				}
			}
			break;
		case 'w':
			return checkKeyword(scanner, 1, 4, "hile", TOKEN_WHILE);
	}

	return TOKEN_IDENTIFIER;
}

static Token identifier(Scanner *scanner) {
	while (isAlpha(scanPeek(scanner)) || isDigit(scanPeek(scanner)))
		advance(scanner);
	return makeToken(scanner, identifierType(scanner));
}

static Token number(Scanner *scanner) {
	if ((scanLookBack(scanner) == '0') &&
		(scanPeek(scanner) == 'x' || scanPeek(scanner) == 'X')) {
		advance(scanner);

		while (isHex(scanPeek(scanner)))
			advance(scanner);
	} else {
		while (isDigit(scanPeek(scanner)))
			advance(scanner);

		// Look for a fractional part.
		if (scanPeek(scanner) == '.' && isDigit(scanPeekNext(scanner))) {
			// Consume the "."
			advance(scanner);

			while (isDigit(scanPeek(scanner)))
				advance(scanner);
		}
	}

	return makeToken(scanner, TOKEN_NUMBER);
}

static bool outputUtf8(uint32_t codepoint, uint8_t **output) {
	uint8_t *out = *output;
	if (codepoint <= 0x7F) {
		// plain ASCII
		out[0] = (char)codepoint;
		*output += 1;
	} else if (codepoint <= 0x07FF) {
		// 2-byte utf8
		out[0] = (char)(((codepoint >> 6) & 0x1F) | 0xC0);
		out[1] = (char)(((codepoint >> 0) & 0x3F) | 0x80);
		*output += 2;
	} else if (codepoint <= 0xFFFF) {
		// 3-byte utf8
		out[0] = (char)(((codepoint >> 12) & 0x0F) | 0xE0);
		out[1] = (char)(((codepoint >>  6) & 0x3F) | 0x80);
		out[2] = (char)(((codepoint >>  0) & 0x3F) | 0x80);
		*output += 3;
	} else if (codepoint <= 0x10FFFF) {
		// 4-byte utf8
		out[0] = (char)(((codepoint >> 18) & 0x07) | 0xF0);
		out[1] = (char)(((codepoint >> 12) & 0x3F) | 0x80);
		out[2] = (char)(((codepoint >>  6) & 0x3F) | 0x80);
		out[3] = (char)(((codepoint >>  0) & 0x3F) | 0x80);
		*output += 4;
	} else {
		return false;
	}
	return true;
}

static Token string(Scanner *scanner, uint8_t delimiter, bool fString, bool fStringStart) {
	typedef enum {
		SCAN, ESCAPE, UNICODE
	} SSMODE;

	bool complete = false;
	uint8_t *start = getPos(scanner);
	uint8_t *output = start;

	SSMODE mode = SCAN;
	int numUChars = 0;
	uint32_t codepoint = 0;
	bool endFString = false;

	do {
		if (isAtEnd(scanner))
			return errorToken(scanner, "Unterminated string");
		uint32_t cp = utf8_advance(scanner);
		switch (mode) {
			case SCAN: {
				if (cp == delimiter) {
					outputUtf8(cp, &output);
					complete = true;
					endFString = true;
				} else if (cp == '\\')
					mode = ESCAPE;
				else if (cp == '\0')
					return errorToken(scanner, "Unterminated string");
				else if (cp == INVALID_UTF8)
					return errorToken(scanner, "Invalid UTF-8 character");
				else if (fString && (cp == '{')) {
					outputUtf8(cp, &output);
					FString *crtFString = &scanner->fStrings[scanner->openFStrings - 1];
					crtFString->openBraces++;
					crtFString->hasExpr = true;
					complete = true;
				} else
					outputUtf8(cp, &output);
				break;
			}
			case ESCAPE: {
				switch (cp) {
					case '\'':
					case '\\':
					case '"':
						outputUtf8(cp, &output);
						mode = SCAN;
						break;
					case 'n':
						*(output++) = '\n';
						mode = SCAN;
						break;
					case 'r':
						*(output++) = '\r';
						mode = SCAN;
						break;
					case 't':
						*(output++) = '\t';
						mode = SCAN;
						break;
					case 'u':
						mode = UNICODE;
						numUChars = 0;
						codepoint = 0;
						break;
					case '\0':
						return errorToken(scanner, "Unterminated string");
					case INVALID_UTF8:
						return errorToken(scanner, "Invalid UTF-8 character");
					default:
						return errorToken(scanner, "Invalid escape sequence");
				}

				break;
			}
			case UNICODE: {
				if (cp >= '0' && cp <= '9')
					codepoint = (codepoint << 4) | (cp - '0');
				else if ((cp >= 'a') && (cp <= 'f'))
					codepoint = (codepoint << 4) | (cp - 'a' + 10);
				else if ((cp >= 'A') && (cp <= 'F'))
					codepoint = (codepoint << 4) | (cp - 'A' + 10);
				else
					return errorToken(scanner, "Invalid character in Unicode escape");
				numUChars++;
				if (numUChars == 4) {
					if (!outputUtf8(codepoint, &output))
						return errorToken(scanner, "Invalid UTF-8 unicode escape");
					mode = SCAN;
				}
			}
		}
	} while (!complete);
	int len = output - start;

	EloxTokenType tokenType;
	if (fString) {
		if (endFString) {
			FString *crtFString = &scanner->fStrings[scanner->openFStrings - 1];
			scanner->openFStrings--;
			tokenType = crtFString->hasExpr ? TOKEN_FSTRING_END : TOKEN_STRING;
		} else
			tokenType = fStringStart ? TOKEN_FSTRING_START : TOKEN_FSTRING;
	} else
		tokenType = TOKEN_STRING;

	return makeTrimmedToken(scanner, tokenType, len + 1);
}

static Token rawString(Scanner *scanner, char delimiter) {
	do {
		if (isAtEnd(scanner))
			return errorToken(scanner, "Unterminated string");
		char ch = advance(scanner);
		if (ch == delimiter)
			break;
	} while (true);
	return makeToken(scanner, TOKEN_STRING);
}

static Token fString(Scanner *scanner, char delimiter) {
	if (scanner->openFStrings >= MAX_INTERP_DEPTH)
		return errorToken(scanner, "Interpolation nesting limit exceeded");

	FString *crtFString = &scanner->fStrings[scanner->openFStrings];
	crtFString->delim = delimiter;
	crtFString->hasExpr = false;
	crtFString->openBraces = 0;
	scanner->openFStrings++;

	return string(scanner, delimiter, true, true);
}

Token scanToken(CCtx *cCtx) {
	Scanner *scanner = &cCtx->scanner;

	if (!skipWhitespace(scanner))
		return errorToken(scanner, "Unterminated comment");

	scanner->start = scanner->current;

	if (isAtEnd(scanner))
		return makeToken(scanner, TOKEN_EOF);

	char c = advance(scanner);
	if (isAlpha(c)) {
		switch (c) {
			case 'r':
				if (advanceIfMatch(scanner, '"')) {
					scanner->start++; // discard 'r'
					return rawString(scanner, '"');
				} else if (advanceIfMatch(scanner, '\'')) {
					scanner->start++; // discard 'r'
					return rawString(scanner, '\'');
				}
				break;
			case 'f':
				if (advanceIfMatch(scanner, '"')) {
					scanner->start++; // discard 'f'
					return fString(scanner, '"');
				} else if (advanceIfMatch(scanner, '\'')) {
					scanner->start++; // discard 'f'
					return fString(scanner, '\'');
				}
				break;
			default:
				break;
		}
		return identifier(scanner);
	}
	if (isDigit(c))
		return number(scanner);

	switch (c) {
		case '(':
			return makeToken(scanner, TOKEN_LEFT_PAREN);
		case ')':
			return makeToken(scanner, TOKEN_RIGHT_PAREN);
		case '{':
			return makeToken(scanner, TOKEN_LEFT_BRACE);
		case '}': {
			if (scanner->openFStrings > 0) {
				FString *crtFString = &scanner->fStrings[scanner->openFStrings - 1];
				if (crtFString->openBraces == 1) {
					crtFString->openBraces = 0;
					return string(scanner, crtFString->delim, true, false);
				} else {
					crtFString->openBraces--;
					return makeToken(scanner, TOKEN_RIGHT_BRACE);
				}
			} else
				return makeToken(scanner, TOKEN_RIGHT_BRACE);
		}
		case '[':
			return makeToken(scanner, TOKEN_LEFT_BRACKET);
		case ']':
			return makeToken(scanner, TOKEN_RIGHT_BRACKET);
		case ':':
			if (advanceIfMatch(scanner, ':'))
				return makeToken(scanner, TOKEN_DOUBLE_COLON);
			else if (advanceIfMatch(scanner, '='))
				return makeToken(scanner, TOKEN_COLON_EQUAL);
			else
				return makeToken(scanner, TOKEN_COLON);
		case ';':
			return makeToken(scanner, TOKEN_SEMICOLON);
		case ',':
			return makeToken(scanner, TOKEN_COMMA);
		case '.':
			if (advanceIfMatch2(scanner, '.', '.'))
				return makeToken(scanner, TOKEN_ELLIPSIS);
			else
				return makeToken(scanner, advanceIfMatch(scanner, '.') ? TOKEN_DOT_DOT : TOKEN_DOT);
		case '-':
			return makeToken(scanner, advanceIfMatch(scanner, '=') ? TOKEN_MINUS_EQUAL : TOKEN_MINUS);
		case '+':
			return makeToken(scanner, advanceIfMatch(scanner, '=') ? TOKEN_PLUS_EQUAL : TOKEN_PLUS);
		case '/':
			return makeToken(scanner, advanceIfMatch(scanner, '=') ? TOKEN_SLASH_EQUAL : TOKEN_SLASH);
		case '%':
			return makeToken(scanner, advanceIfMatch(scanner, '=') ? TOKEN_PERCENT_EQUAL : TOKEN_PERCENT);
		case '*':
			return makeToken(scanner, advanceIfMatch(scanner, '=') ? TOKEN_STAR_EQUAL : TOKEN_STAR);
		case '!':
			return makeToken(scanner, advanceIfMatch(scanner, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
		case '=':
			return makeToken(scanner, advanceIfMatch(scanner, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
		case '<':
			return makeToken(scanner, advanceIfMatch(scanner, '=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
		case '>':
			return makeToken(scanner, advanceIfMatch(scanner, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
		case '"': return string(scanner, '"', false, false);
		case '\'': return string(scanner, '\'', false, false);
	}

	return errorToken(scanner, "Unexpected character.");
}
