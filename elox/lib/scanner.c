#include <stdio.h>
#include <string.h>

#include "elox/common.h"
#include "elox/scanner.h"
#include "elox/state.h"

void initScanner(CCtx *cCtx, char *source) {
	Scanner *scanner = &cCtx->scanner;
	scanner->start = source;
	scanner->current = source;
	scanner->line = 1;
}

static bool isAlpha(char c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static bool isDigit(char c) {
	return c >= '0' && c <= '9';
}

bool isAtEnd(Scanner *scanner) {
	return *scanner->current == '\0';
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

static char *getPos(Scanner *scanner) {
	return scanner->current;
}

static bool match(Scanner *scanner, char expected) {
	if (isAtEnd(scanner))
		return false;
	if (*scanner->current != expected)
		return false;
	scanner->current++;
	return true;
}

static bool match2(Scanner *scanner, char xa, char xb) {
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

static Token makeToken(Scanner *scanner, TokenType type) {
	Token token;
	token.type = type;
	token.string.chars = scanner->start;
	token.string.length = (int)(scanner->current - scanner->start);
	token.line = scanner->line;
	return token;
}

static Token makeTrimmedToken(Scanner *scanner, TokenType type, int len) {
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
	token.string.chars = message;
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
			case '/':
				if (scanPeekNext(scanner) == '/') {
					// A comment goes until the end of the line.
					while (scanPeek(scanner) != '\n' && !isAtEnd(scanner))
						advance(scanner);
				} else if (scanPeekNext(scanner) == '*') {
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
									case '/':
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
				} else
					return true;
				break;
			default:
				return true;
		}
	}
}

static TokenType checkKeyword(Scanner *scanner, int start, int length, const char *rest,
							  TokenType type) {
	if (scanner->current - scanner->start == start + length &&
			memcmp(scanner->start + start, rest, (size_t)length) == 0) {
		return type;
	}

	return TOKEN_IDENTIFIER;
}

static TokenType identifierType(Scanner *scanner) {
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
	while (isDigit(scanPeek(scanner)))
		advance(scanner);

	// Look for a fractional part.
	if (scanPeek(scanner) == '.' && isDigit(scanPeekNext(scanner))) {
		// Consume the "."
		advance(scanner);

		while (isDigit(scanPeek(scanner)))
			advance(scanner);
	}

	return makeToken(scanner, TOKEN_NUMBER);
}

static Token string(Scanner *scanner, char delimiter) {
	typedef enum {
		SCAN, ESCAPE
	} SSMODE;

	bool complete = false;
	char *start = getPos(scanner);
	char *output = start;
	SSMODE mode = SCAN;
	do {
		if (isAtEnd(scanner))
			return errorToken(scanner, "Unterminated string");
		char ch = advance(scanner);
		switch (mode) {
			case SCAN: {
				if (ch == delimiter) {
					*(output++) = ch;
					complete = true;
				} else if (ch == '\\')
					mode = ESCAPE;
				else if (ch == '\0')
					return errorToken(scanner, "Unterminated string");
				else
					*(output++) = ch;
				break;
			}
			case ESCAPE: {
				switch (ch) {
					case '\'':
					case '\\':
					case '"':
						*(output++) = ch;
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
					case '\0':
						return errorToken(scanner, "Unterminated string");
					default:
						return errorToken(scanner, "Invalid escape sequence");
				}

				break;
			}
		}
	} while (!complete);
	int len = output - start;
	return makeTrimmedToken(scanner, TOKEN_STRING, len + 1);
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

Token scanToken(Scanner *scanner) {
	if (!skipWhitespace(scanner))
		return errorToken(scanner, "Unterminated comment");

	scanner->start = scanner->current;

	if (isAtEnd(scanner))
		return makeToken(scanner, TOKEN_EOF);

	char c = advance(scanner);
	if (isAlpha(c)) {
		switch (c) {
			case 'r':
				if (match(scanner, '"')) {
					scanner->start++; // discard 'r'
					return rawString(scanner, '"');
				} else if (match(scanner, '\'')) {
					scanner->start++; // discard 'r'
					return rawString(scanner, '\'');
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
		case '}':
			return makeToken(scanner, TOKEN_RIGHT_BRACE);
		case '[':
			return makeToken(scanner, TOKEN_LEFT_BRACKET);
		case ']':
			return makeToken(scanner, TOKEN_RIGHT_BRACKET);
		case ':':
			return makeToken(scanner, match(scanner, ':') ? TOKEN_DOUBLE_COLON : TOKEN_COLON);
		case ';':
			return makeToken(scanner, TOKEN_SEMICOLON);
		case ',':
			return makeToken(scanner, TOKEN_COMMA);
		case '.':
			return makeToken(scanner, match2(scanner, '.', '.') ? TOKEN_ELLIPSIS : TOKEN_DOT);
		case '-':
			return makeToken(scanner, match(scanner, '=') ? TOKEN_MINUS_EQUAL : TOKEN_MINUS);
		case '+':
			return makeToken(scanner, match(scanner, '=') ? TOKEN_PLUS_EQUAL : TOKEN_PLUS);
		case '/':
			return makeToken(scanner, match(scanner, '=') ? TOKEN_SLASH_EQUAL : TOKEN_SLASH);
		case '%':
			return makeToken(scanner, match(scanner, '=') ? TOKEN_PERCENT_EQUAL : TOKEN_PERCENT);
		case '*':
			return makeToken(scanner, match(scanner, '=') ? TOKEN_STAR_EQUAL : TOKEN_STAR);
		case '!':
			return makeToken(scanner, match(scanner, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
		case '=':
			return makeToken(scanner, match(scanner, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
		case '<':
			return makeToken(scanner, match(scanner, '=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
		case '>':
			return makeToken(scanner, match(scanner, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
		case '"': return string(scanner, '"');
		case '\'': return string(scanner, '\'');
	}

	return errorToken(scanner, "Unexpected character.");
}
