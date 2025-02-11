// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_SCANNER_H
#define ELOX_SCANNER_H

#include "elox/util.h"

#include <stdbool.h>

typedef enum {
	// Single-character tokens
	TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
	TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
	TOKEN_LEFT_BRACKET, TOKEN_RIGHT_BRACKET,
	TOKEN_COLON, TOKEN_DOUBLE_COLON, TOKEN_COMMA, TOKEN_DOT, TOKEN_DOT_DOT, TOKEN_ELLIPSIS,
	TOKEN_MINUS, TOKEN_PERCENT, TOKEN_PLUS, TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,
	// One or two character tokens
	TOKEN_BANG, TOKEN_BANG_EQUAL,
	TOKEN_EQUAL, TOKEN_EQUAL_EQUAL, TOKEN_COLON_EQUAL,
	TOKEN_GREATER, TOKEN_GREATER_EQUAL,
	TOKEN_LESS, TOKEN_LESS_EQUAL,
	TOKEN_PLUS_EQUAL, TOKEN_MINUS_EQUAL, TOKEN_SLASH_EQUAL, TOKEN_STAR_EQUAL, TOKEN_PERCENT_EQUAL,
	// Literals
	TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,
	// FString
	TOKEN_FSTRING_START, TOKEN_FSTRING, TOKEN_FSTRING_END,
	// Keywords
	TOKEN_AND, TOKEN_BREAK, TOKEN_CATCH, TOKEN_CONTINUE,
	TOKEN_CLASS, TOKEN_ELSE, TOKEN_EXTENDS, TOKEN_FALSE, TOKEN_FINALLY, TOKEN_FOR, TOKEN_FOREACH,
	TOKEN_FROM, TOKEN_FUNCTION, TOKEN_IF, TOKEN_IMPLEMENTS, TOKEN_IMPORT, TOKEN_IN,
	TOKEN_INSTANCEOF, TOKEN_INTERFACE, TOKEN_NIL, TOKEN_OR,
	TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS, TOKEN_THROW, TOKEN_TRUE,
	TOKEN_TRY, TOKEN_WHILE,
	// Qualifiers
	TOKEN_ABSTRACT, TOKEN_GLOBAL, TOKEN_LOCAL,
	// Special tokens
	TOKEN_ERROR, TOKEN_EOF
} ELOX_PACKED EloxTokenType;

typedef EloxString String;

typedef struct {
	String string;
	EloxTokenType type;
	int line;
} Token;

#define MAX_INTERP_DEPTH 8

typedef struct {
	char delim;
	bool hasExpr;
	uint16_t openBraces;
} FString;

typedef struct {
	const uint8_t *start;
	uint8_t *current;
	int line;
	uint8_t openFStrings;
	FString fStrings[MAX_INTERP_DEPTH];
} Scanner;

typedef struct CCtx CCtx;

void initScanner(CCtx *cCtx, uint8_t *source);
Token scanToken(CCtx *cCtx);

bool isAtEnd(Scanner *scanner);

#endif // ELOX_SCANNER_H
