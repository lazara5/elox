// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/util.h"
#include "elox/compiler.h"
#include "elox/memory.h"
#include "elox/scanner.h"
#include "elox/state.h"
#include <elox/Class.h>
#include "elox/builtins.h"

#if defined(ELOX_DEBUG_PRINT_CODE) || defined(ELOX_DEBUG_TRACE_SCANNER)
#include "elox/debug.h"
#endif

#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>

#pragma GCC diagnostic ignored "-Wswitch-enum"

#define CHECK_RAISE_PARSE_ERR_RET(cond, msg) \
{ \
	if (ELOX_UNLIKELY(cond)) { \
		errorAtCurrent(cCtx, msg); \
		return; \
	} \
}

#define CHECK_RAISE_PARSE_ERR_RET_VAL(cond, msg, val) \
{ \
	if (ELOX_UNLIKELY(cond)) { \
		errorAtCurrent(cCtx, msg); \
		return (val); \
	} \
}

#define IF_RAISED_RESTORE_RAISE_PARSE_ERR_RET_VAL(ERROR, FIBER, STACK, CCTX, MSG, VAL) \
{ \
	if (ELOX_UNLIKELY((ERROR)->raised)) { \
		restoreStack((FIBER), (STACK)); \
		errorAtCurrent((CCTX), ("" MSG "")); \
		return (VAL); \
	} \
}

typedef enum {
	PREC_NONE,
	PREC_ASSIGNMENT,  // =
	PREC_OR,          // or
	PREC_AND,         // and
	PREC_EQUALITY,    // == !=
	PREC_COMPARISON,  // < > <= >=
	PREC_TERM,        // + -
	PREC_FACTOR,      // * / %
	PREC_UNARY,       // ! -
	PREC_CALL,        // . : ()
	PREC_SUBSCRIPT,   // []
	PREC_PRIMARY
} Precedence;

typedef enum {
	ETYPE_NORMAL,
	ETYPE_EXPAND
} ELOX_PACKED ExpressionType;

typedef ExpressionType (*ParseFn)(CCtx *cCtx, bool canAssign, bool canExpand, bool firstExpansion);

typedef struct {
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

bool initCompilerContext(CCtx *cCtx, RunCtx *runCtx, const String *fileName, const String *moduleName) {
	cCtx->runCtx = runCtx;
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;

	compilerState->currentFunctionCompiler = NULL;
	compilerState->currentKlass = NULL;
	compilerState->innermostLoop.start = -1;
	compilerState->innermostLoop.scopeDepth = 0;
	compilerState->innermostLoop.catchStackDepth = 0;
	compilerState->innermostLoop.finallyDepth = 0;
	compilerState->breakJumps = NULL;
	compilerState->lambdaCount = 0;
	compilerState->fileName = copyString(runCtx, fileName->chars, fileName->length);
	if (ELOX_UNLIKELY(compilerState->fileName == NULL))
		return false;

	cCtx->moduleName = *moduleName;

	return true;
}

static Chunk *currentChunk(FunctionCompiler *current) {
	return &current->function->chunk;
}

static void errorAt(CCtx *cCtx, Token *token, const char *message) {
	RunCtx *runCtx = cCtx->runCtx;
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	if (parser->panicMode)
		return;
	parser->panicMode = true;
	eloxPrintf(runCtx, ELOX_IO_ERR, "[line %d] Error", token->line);

	if (token->type == TOKEN_EOF)
		eloxPrintf(runCtx, ELOX_IO_ERR, " at end");
	else if (token->type == TOKEN_ERROR) {
		// Nothing
	} else
		eloxPrintf(runCtx, ELOX_IO_ERR, " at '%.*s'", token->string.length, token->string.chars);

	eloxPrintf(runCtx, ELOX_IO_ERR, ": %s\n", message);
	parser->hadError = true;
}

void compileError(CCtx *cCtx, const char *message) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	errorAt(cCtx, &parser->previous, message);
}

static void errorAtCurrent(CCtx *cCtx, const char *message) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	errorAt(cCtx, &parser->current, message);
}

static void advance(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	parser->beforePrevious = parser->previous;
	parser->previous = parser->current;

	for (;;) {
		if (parser->hasNext) {
			parser->current = parser->next;
			parser->hasNext = false;
		} else
			parser->current = scanToken(cCtx);
		if (parser->current.type != TOKEN_ERROR)
			break;

		// TODO: error message
		errorAtCurrent(cCtx, (const char *)parser->current.string.chars);
	}
#ifdef ELOX_DEBUG_TRACE_SCANNER
	RunCtx *runCtx = cCtx->runCtx;
	eloxPrintf(runCtx, ELOX_IO_DEBUG, ">% 5d ", parser->current.line);
	printToken(runCtx, &parser->current);
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "\n");
#endif
}

static bool consume(CCtx *cCtx, EloxTokenType type, const char *message) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	if (parser->current.type == type) {
		advance(cCtx);
		return true;
	}

	errorAtCurrent(cCtx, message);
	return false;
}

typedef struct {
	struct {
		const uint8_t *start;
		uint8_t *current;
		int line;
		uint8_t openFStrings;
	} scanner;
	Parser parser;
} CCtxState;

static void saveCCtxState(CCtx *cCtx, CCtxState *state) {
	state->scanner.start = cCtx->scanner.start;
	state->scanner.current = cCtx->scanner.current;
	state->scanner.line = cCtx->scanner.line;
	state->scanner.openFStrings = cCtx->scanner.openFStrings;

	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	state->parser = compilerState->parser;
}

static void restoreCCtxState(CCtx *cCtx, const CCtxState *state) {
	cCtx->scanner.start = state->scanner.start;
	cCtx->scanner.current = state->scanner.current;
	cCtx->scanner.line = state->scanner.line;
	cCtx->scanner.openFStrings = state->scanner.openFStrings;

	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	compilerState->parser = state->parser;
}

static bool check(CCtx *cCtx, EloxTokenType type) {

	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	return parser->current.type == type;
}

static EloxTokenType getCrtType(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	return parser->current.type;
}

static bool checkNext(CCtx *cCtx, EloxTokenType type) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;
	Scanner *scanner = &cCtx->scanner;

	if (isAtEnd(scanner))
		return false;

	if (!parser->hasNext) {
		parser->next = scanToken(cCtx);
		parser->hasNext = true;
	}

	return parser->next.type == type;
}

static bool consumeIfMatch(CCtx *cCtx, EloxTokenType type) {
	if (!check(cCtx, type))
		return false;
	advance(cCtx);
	return true;
}

static int emitByte(CCtx *cCtx, uint8_t byte) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;
	Parser *parser = &compilerState->parser;

	writeChunk(cCtx, currentChunk(current), &byte, 1, parser->previous.line);
	return currentChunk(current)->count - 1;
}

static void patchByte(CCtx *cCtx, int offset, uint8_t value) {
	if (ELOX_UNLIKELY(offset < 0))
		return;

	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	currentChunk(current)->code[offset] = value;
}

static void emitBytes(CCtx *cCtx, uint8_t byte1, uint8_t byte2) {
	emitByte(cCtx, byte1);
	emitByte(cCtx, byte2);
}

static void emitPop(CCtx *cCtx, uint8_t n) {
	if (n == 0)
		return;
	if (n == 1)
		emitByte(cCtx, OP_POP);
	else
		emitBytes(cCtx, OP_POPN, n);
}

static int emitUShort(CCtx *cCtx, uint16_t val) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;
	Parser *parser = &compilerState->parser;

	writeChunk(cCtx, currentChunk(current), (uint8_t *)&val, 2, parser->previous.line);
	return currentChunk(current)->count - 2;
}

static int emitInt(CCtx *cCtx, int32_t val) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;
	Parser *parser = &compilerState->parser;

	writeChunk(cCtx, currentChunk(current), (uint8_t *)&val, 4, parser->previous.line);
	return currentChunk(current)->count - 4;
}

static void patchUShort(CCtx *cCtx, int offset, uint16_t val) {
	if (ELOX_UNLIKELY(offset < 0))
		return;

	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	memcpy(currentChunk(current)->code + offset, &val, sizeof(uint16_t));
}

void chunkPatchUShort(Chunk *chunk, int offset, uint16_t val) {
	if (ELOX_UNLIKELY(offset < 0))
		return;

	memcpy(chunk->code + offset, &val, sizeof(uint16_t));
}

static void emitLoop(CCtx *cCtx, int loopStart) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	emitByte(cCtx, OP_LOOP);

	int offset = currentChunk(current)->count - loopStart + 2;
	if (offset > UINT16_MAX)
		compileError(cCtx, "Loop body too large");

	emitUShort(cCtx, (uint16_t)offset);
}

static int emitJump(CCtx *cCtx, uint8_t instruction) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	emitByte(cCtx, instruction);
	emitUShort(cCtx, 0);
	return currentChunk(current)->count - 2;
}

static int emitAddress(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	emitByte(cCtx, 0xff);
	emitByte(cCtx, 0xff);
	return currentChunk(current)->count - 2;
}

static void patchAddress(FunctionCompiler *current, int offset) {
	if (ELOX_UNLIKELY(offset < 0))
		return;

	uint16_t address = currentChunk(current)->count;
	memcpy(currentChunk(current)->code + offset, &address, sizeof(uint16_t));
}

static void emitReturn(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	if (current->type == FTYPE_INITIALIZER) {
		emitBytes(cCtx, OP_GET_LOCAL, 0);
		emitByte(cCtx, (uint8_t)false);
	} else
		emitByte(cCtx, OP_NIL);
	emitByte(cCtx, OP_RETURN);
}

static suint16_t makeConstant(CCtx *cCtx, Value value) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	int constant = addConstant(cCtx->runCtx, currentChunk(current), value);
	if (ELOX_UNLIKELY(constant < 0)) {
		compileError(cCtx, "Out of memory");
		return -1;
	}
	if (constant > UINT16_MAX) {
		compileError(cCtx, "Too many constants in one chunk");
		return -1;
	}

	return (suint16_t)constant;
}

static void emitConstantOp(CCtx *cCtx, uint16_t constantIndex) {
	if (constantIndex < 256) {
		emitByte(cCtx, OP_CONST8);
		emitByte(cCtx, constantIndex);
	} else {
		emitByte(cCtx, OP_CONST16);
		emitUShort(cCtx, constantIndex);
	}
}

static void emitConstant(CCtx *cCtx, Value value) {
	if (IS_NUMBER(value)) {
		double val = AS_NUMBER(value);
		if (trunc(val) == val) {
			if ((val >= INT32_MIN) && (val <= INT32_MAX)) {
				emitByte(cCtx, OP_IMMI);
				emitInt(cCtx, val);
				return;
			}
		}
	}
	suint16_t constantIndex = makeConstant(cCtx, value);
	if (ELOX_UNLIKELY(constantIndex < 0))
		errorAtCurrent(cCtx, "Out of memory");
	emitConstantOp(cCtx, constantIndex);
}

static void patchJump(CCtx *cCtx, int offset) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	// -2 to adjust for the bytecode for the jump offset itself
	int jump = currentChunk(current)->count - offset - 2;

	if (jump > UINT16_MAX)
		compileError(cCtx, "Too much code to jump over");

	uint16_t ushortJmp = (uint16_t)jump;
	memcpy(currentChunk(current)->code + offset, &ushortJmp, sizeof(uint16_t));
}

static void patchBreakJumps(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;

	while (compilerState->breakJumps != NULL) {
		if (compilerState->breakJumps->scopeDepth >= compilerState->innermostLoop.scopeDepth) {
			// Patch break jump
			patchJump(cCtx, compilerState->breakJumps->offset);

			BreakJump *temp = compilerState->breakJumps;
			compilerState->breakJumps = compilerState->breakJumps->next;
			FREE(cCtx->runCtx, BreakJump, temp);
		} else
			break;
	}
}

static VarScope parseQuals(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;
	Qualifiers *quals = &compilerState->currentFunctionCompiler->quals;

	bool inSpec = true;
	do {
		switch (parser->current.type) {
			case TOKEN_ABSTRACT:
				if (quals->attrs & QUAL_ABSTRACT)
					errorAtCurrent(cCtx, "Duplicated specifier 'abstract'");
				quals->attrs |= QUAL_ABSTRACT;
				quals->pending |= QUAL_PENDING_CLASS;
				break;
			case TOKEN_GLOBAL:
				if (quals->attrs & QUAL_GLOBAL)
					errorAtCurrent(cCtx, "Duplicated specifier 'global'");
				if (quals->attrs & QUAL_LOCAL)
					errorAtCurrent(cCtx, "'global' cannot follow 'local'");
				quals->attrs |= QUAL_GLOBAL;
				quals->pending |= QUAL_PENDING_SCOPE;
				break;
			case TOKEN_LOCAL:
				if (quals->attrs & QUAL_LOCAL)
					errorAtCurrent(cCtx, "Duplicated specifier 'local'");
				if (quals->attrs & QUAL_GLOBAL)
					errorAtCurrent(cCtx, "'local' cannot follow 'global'");
				quals->attrs |= QUAL_LOCAL;
				quals->pending |= QUAL_PENDING_SCOPE;
				break;
			default:
				inSpec = false;
				break;
		}

		if (inSpec) {
			quals->pending = true;
			advance(cCtx);
		}
	} while (inSpec);

	return (quals->attrs & QUAL_GLOBAL) ? VAR_GLOBAL : VAR_LOCAL;
}

static void resetQuals(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Qualifiers *quals = &compilerState->currentFunctionCompiler->quals;

	quals->attrs = 0;
	quals->pending = 0;
}

static FunctionCompiler *initFunctionCompiler(CCtx *cCtx, FunctionCompiler *functionCompiler,
											  MethodCompiler *methodCompiler, FunctionType type,
											  const Token *nameToken) {
	static int compilerId = 0;

	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;
	RunCtx *runCtx = cCtx->runCtx;

	ObjFunction *function = newFunction(runCtx, compilerState->fileName);
	if (ELOX_UNLIKELY(function == NULL))
		return NULL;

	functionCompiler->id = compilerId++;
	functionCompiler->enclosing = current;
	functionCompiler->methodCompiler = methodCompiler;
	functionCompiler->function = NULL;
	functionCompiler->type = type;
	functionCompiler->localCount = 0;
	functionCompiler->postArgs = false;
	functionCompiler->hasVarargs = false;
	functionCompiler->scopeDepth = 0;
	functionCompiler->catchStackDepth = 0;
	functionCompiler->catchDepth = 0;
	functionCompiler->finallyDepth = 0;
	functionCompiler->numArgs = 0;
	functionCompiler->function = function;
	initTable(&functionCompiler->stringConstants);

	static const Token anonName = TOKEN_INITIALIZER("$init");

	compilerState->currentFunctionCompiler = current = functionCompiler;
	resetQuals(cCtx);
	switch (type) {
		case FTYPE_SCRIPT:
			current->function->name = NULL;
			break;
		case FTYPE_LAMBDA: {
			uint8_t lambdaBuffer[64];
			int len = sprintf((char *)lambdaBuffer, "<lambda_%d>", compilerState->lambdaCount++);
			current->function->name = copyString(runCtx, lambdaBuffer, len);
			if (ELOX_UNLIKELY(current->function->name == NULL))
				return false;
			break;
		}
		case FTYPE_INITIALIZER:
			if (nameToken == NULL)
				nameToken = &anonName;
			// FALLTHROUGH
		case FTYPE_FUNCTION:
		case FTYPE_METHOD:
		case FTYPE_DEFAULT:
			current->function->name = copyString(runCtx,
												 nameToken->string.chars, nameToken->string.length);
			if (ELOX_UNLIKELY(current->function->name == NULL))
				return false;
			break;
	}

	Local *local = &current->locals[current->localCount++];
	local->depth = 0;
	local->isCaptured = false;
	local->postArgs = false;
	if (type != FTYPE_FUNCTION) {
		local->name.string.chars = U8("this");
		local->name.string.length = 4;
	} else {
		local->name.string.chars = U8("");
		local->name.string.length = 0;
	}
#ifdef ELOX_DEBUG_PRINT_CODE
	eloxPrintf(cCtx->runCtx, ELOX_IO_DEBUG, ">>>Local[%u][%d] <- %.*s\n",
			   current->id, current->localCount - 1, local->name.string.length, local->name.string.chars);
#endif

	return current;
}

static ObjFunction *endCompiler(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	emitReturn(cCtx);
	ObjFunction* function = current->function;

#ifdef ELOX_DEBUG_PRINT_CODE
	Parser *parser = &compilerState->parser;
	RunCtx *runCtx = cCtx->runCtx;
	if (!parser->hadError) {
		disassembleChunk(runCtx, currentChunk(current),
						 function->name != NULL ? (const char *)function->name->string.chars : "<script>");
	}
#endif

	freeTable(cCtx->runCtx, &current->stringConstants);

	compilerState->currentFunctionCompiler = current->enclosing;

	return function;
}

static void beginScope(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;
	current->scopeDepth++;
}

static void endScope(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	current->scopeDepth--;

	int numPendingPop = 0;
	while ((current->localCount > 0) &&
		   (current->locals[current->localCount - 1].depth > current->scopeDepth)) {
		if (current->locals[current->localCount - 1].isCaptured) {
			if (numPendingPop > 0) {
				emitPop(cCtx, numPendingPop);
				numPendingPop = 0;
			}
			emitByte(cCtx, OP_CLOSE_UPVALUE);
		} else
			numPendingPop++;
		current->localCount--;
	}

	emitPop(cCtx, numPendingPop);
}

static void statement(CCtx *cCtx);
static void declaration(CCtx *cCtx);
static ParseRule *getRule(EloxTokenType type);
static ExpressionType and_(CCtx *cCtx, bool canAssign, bool canExpand, bool firstExpansion);

typedef enum {
	EXPR_ALLOW_EXPAND = 1 << 0,
	EXPR_ALLOW_TUPLE  = 1 << 1
} ELOX_PACKED ExpressionFlags;

static ExpressionType expression(CCtx *cCtx, Precedence precedence,
								 uint8_t flags, bool firstExpansion) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	advance(cCtx);
	ParseFn prefixRule = getRule(parser->previous.type)->prefix;
	if (prefixRule == NULL) {
		compileError(cCtx, "Expect expression");
		return ETYPE_NORMAL;
	}

	bool canAssign = (precedence <= PREC_ASSIGNMENT);
	ExpressionType type = prefixRule(cCtx, canAssign, flags & EXPR_ALLOW_EXPAND, firstExpansion);
	if ((!(flags & EXPR_ALLOW_EXPAND)) && (type == ETYPE_EXPAND))
		compileError(cCtx, "Expansion not allowed in this context");

	while (precedence <= getRule(parser->current.type)->precedence) {
		if (type == ETYPE_EXPAND)
			compileError(cCtx, "Expansions can only be used as stand-alone expressions");
		advance(cCtx);
		ParseFn infixRule = getRule(parser->previous.type)->infix;
		infixRule(cCtx, canAssign, flags & EXPR_ALLOW_EXPAND, firstExpansion);
	}

	if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL))
		compileError(cCtx, "Invalid assignment target");

	if ((flags & EXPR_ALLOW_TUPLE) && (consumeIfMatch(cCtx, TOKEN_COMMA))) {
		int itemCount = 1;
		do {
			if (check(cCtx, TOKEN_SEMICOLON) || check(cCtx, TOKEN_RIGHT_PAREN)) {
				// trailing comma
				break;
			}

			expression(cCtx, PREC_ASSIGNMENT, 0, false);
			if (itemCount == UINT16_COUNT)
				compileError(cCtx, "Cannot have more than 16384 items in a tuple literal");
			itemCount++;
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));

		emitByte(cCtx, OP_NEW_TUPLE);
		emitUShort(cCtx, (uint16_t)itemCount);
	}

	return type;
}

static ExpressionType binary(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							 bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	EloxTokenType operatorType = parser->previous.type;
	ParseRule *rule = getRule(operatorType);

	if ((operatorType == TOKEN_IN) && (consumeIfMatch(cCtx, TOKEN_ELLIPSIS)))
		emitByte(cCtx, OP_GET_VARARGS);
	else
		expression(cCtx, (Precedence)(rule->precedence + 1), 0, false);

	switch (operatorType) {
		case TOKEN_BANG_EQUAL:
			emitBytes(cCtx, OP_EQUAL, OP_NOT);
			break;
		case TOKEN_EQUAL_EQUAL:
			emitByte(cCtx, OP_EQUAL);
			break;
		case TOKEN_GREATER:
			emitByte(cCtx, OP_GREATER);
			break;
		case TOKEN_GREATER_EQUAL:
			emitBytes(cCtx, OP_LESS, OP_NOT);
			break;
		case TOKEN_LESS:
			emitByte(cCtx, OP_LESS);
			break;
		case TOKEN_LESS_EQUAL:
			emitBytes(cCtx, OP_GREATER, OP_NOT);
			break;
		case TOKEN_PLUS:
			emitByte(cCtx, OP_ADD);
			break;
		case TOKEN_MINUS:
			emitByte(cCtx, OP_SUBTRACT);
			break;
		case TOKEN_STAR:
			emitByte(cCtx, OP_MULTIPLY);
			break;
		case TOKEN_SLASH:
			emitByte(cCtx, OP_DIVIDE);
			break;
		case TOKEN_PERCENT:
			emitByte(cCtx, OP_MODULO);
			break;
		case TOKEN_INSTANCEOF:
			emitByte(cCtx, OP_INSTANCEOF);
			break;
		case TOKEN_IN:
			emitByte(cCtx, OP_IN);
			break;
		default:
			ELOX_UNREACHABLE();
	}

	return ETYPE_NORMAL;
}

static ExpressionType unary(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;
	EloxTokenType operatorType = parser->previous.type;

	// Compile the operand
	expression(cCtx, PREC_UNARY, 0, false);

	// Emit the operator instruction
	switch (operatorType) {
		case TOKEN_BANG:
			emitByte(cCtx, OP_NOT);
			break;
		case TOKEN_MINUS:
			emitByte(cCtx, OP_NEGATE);
			break;
		default:
			ELOX_UNREACHABLE();
	}

	return ETYPE_NORMAL;
}

static uint8_t argumentList(CCtx *cCtx, bool *hasExpansions) {
	uint8_t argCount = 0;
	*hasExpansions = false;
	if (!check(cCtx, TOKEN_RIGHT_PAREN)) {
		do {
			ExpressionType argType = expression(cCtx, PREC_ASSIGNMENT,
												EXPR_ALLOW_EXPAND, !(*hasExpansions));
			if (argType == ETYPE_EXPAND) {
				*hasExpansions = true;
			} else {
				if (argCount == UINT8_MAX)
					compileError(cCtx, "Can't have more than 255 arguments");
				argCount++;
				if (*hasExpansions)
					emitByte(cCtx, OP_SWAP);
			}
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	}
	consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after function arguments");
	return argCount;
}

static ExpressionType call(CCtx *cCtx, bool canAssign ELOX_UNUSED,
						   bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	bool hasExpansions;
	uint8_t argCount = argumentList(cCtx, &hasExpansions);
	emitByte(cCtx, OP_CALL);
	emitBytes(cCtx, argCount, hasExpansions);
	return ETYPE_NORMAL;
}

suint16_t identifierConstant(CCtx *cCtx, const String *name) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;
	RunCtx *runCtx = cCtx->runCtx;
	ObjFiber *fiber = runCtx->activeFiber;

	// See if we already have it
	ObjString *string = copyString(runCtx, name->chars, name->length);
	if (ELOX_UNLIKELY(string == NULL))
		return -1;
	Value indexValue;
	if (tableGet(&current->stringConstants, string, &indexValue)) {
		// We do
		return (uint16_t)AS_NUMBER(indexValue);
	}

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	PUSH_TEMP(temps, protectedString, OBJ_VAL(string));
	suint16_t index = makeConstant(cCtx, OBJ_VAL(string));
	releaseTemps(&temps);
	if (ELOX_UNLIKELY(index < 0))
		return -1;
	EloxError error = ELOX_ERROR_INITIALIZER;
	size_t savedStack = saveStack(fiber);
	tableSet(runCtx, &current->stringConstants, string, NUMBER_VAL((double)index), &error);
	if (ELOX_UNLIKELY(error.raised)) {
		error.discardException(fiber, savedStack);
		return -1;
	}
#ifdef ELOX_DEBUG_TRACE_EXECUTION
	eloxPrintf(cCtx->runCtx, ELOX_IO_DEBUG, "(%s:%p) %d <-> %.*s\n",
			   ((current->function->name) ? (char *)current->function->name->string.chars : ""),
			   currentChunk(current), index, name->length, name->chars);
#endif
	return index;
}

suint16_t globalIdentifierConstant(RunCtx *runCtx, const String *name, const String *moduleName) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	suint16_t ret = -1;
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	VMTemp protectedIdentifier = TEMP_INITIALIZER;

	// See if we already have it
	ObjStringPair *identifier = copyStrings(runCtx,
											name->chars, name->length,
											moduleName->chars, moduleName->length);
	if (ELOX_UNLIKELY(identifier == NULL))
		goto cleanup;
	pushTempVal(temps, &protectedIdentifier, OBJ_VAL(identifier));
	Value indexValue;
	EloxError error = ELOX_ERROR_INITIALIZER;
	size_t savedStack = saveStack(fiber);
	if (valueTableGet(runCtx, &vm->globalNames, OBJ_VAL(identifier), &indexValue, &error)) {
		// We do
		ret = (suint16_t)AS_NUMBER(indexValue);
		goto done;
	}
	if (ELOX_UNLIKELY(error.raised)) {
		error.discardException(fiber, savedStack);
		goto cleanup;
	}

	uint16_t newIndex = (uint16_t)vm->globalValues.count;
	savedStack = saveStack(fiber);
	bool res = valueArrayPush(runCtx, &vm->globalValues, UNDEFINED_VAL);
	if (ELOX_UNLIKELY(!res))
		goto cleanup;
	valueTableSet(runCtx, &vm->globalNames, OBJ_VAL(identifier), NUMBER_VAL((double)newIndex), &error);
	if (ELOX_UNLIKELY(error.raised)) {
		error.discardException(fiber, savedStack);
		goto cleanup;
	}

#ifdef ELOX_DEBUG_PRINT_CODE
	eloxPrintf(runCtx, ELOX_IO_DEBUG, ">>>Global[%5u] (%.*s:%.*s)\n", newIndex,
			   moduleName->length, moduleName->chars,
			   name->length, name->chars);
#endif

	ret = newIndex;

done:
cleanup:
	releaseTemps(&temps);
	return ret;
}

static void emitPendingRef(CCtx *cCtx, uint16_t nameHandle, uint64_t mask, RefTarget target,
						   EloxError *error) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	MethodCompiler *mc = compilerState->currentFunctionCompiler->methodCompiler;

	if (mc != NULL) {
		int offset = emitUShort(cCtx, 0);
		uint64_t val =
			(offset & 0xFFFFFF) | mask | ((uint64_t)nameHandle << 32) | ((uint64_t)target << 48);
		if (ELOX_UNLIKELY(!valueArrayPush(cCtx->runCtx, &mc->pendingRefs, NUMBER_VAL(val))))
			ELOX_RAISE_RET(error, OOM(cCtx->runCtx));
	} else
		emitUShort(cCtx, 0);
}

static ExpressionType colon(CCtx *cCtx, bool canAssign,
							bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;
	RunCtx *runCtx = cCtx->runCtx;
	ObjFiber *fiber = runCtx->activeFiber;

	bool isThisRef = (parser->beforePrevious.type == TOKEN_THIS);
	consume(cCtx, TOKEN_IDENTIFIER, "Expect property name after ':'");
	String *propName = &parser->previous.string;
	suint16_t nameHandle = identifierConstant(cCtx, propName);
	CHECK_RAISE_PARSE_ERR_RET_VAL((nameHandle < 0), "Out of memory", ETYPE_NORMAL);

	if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL)) {
		expression(cCtx, PREC_ASSIGNMENT, 0, false);
		if (isThisRef) {
			EloxError error = ELOX_ERROR_INITIALIZER;
			size_t savedStack = saveStack(fiber);
			emitByte(cCtx, OP_SET_REF);
			emitPendingRef(cCtx, nameHandle, MEMBER_FIELD_MASK, REF_THIS, &error);
			IF_RAISED_RESTORE_RAISE_PARSE_ERR_RET_VAL(&error, fiber, savedStack,
													  cCtx, "Out of memory", ETYPE_NORMAL);
		} else {
			emitByte(cCtx, OP_SET_PROP);
			emitUShort(cCtx, nameHandle);
		}
	} else if (consumeIfMatch(cCtx, TOKEN_LEFT_PAREN)) {
		bool hasExpansions;
		uint8_t argCount = argumentList(cCtx, &hasExpansions);
		if (isThisRef) {
			EloxError error = ELOX_ERROR_INITIALIZER;
			size_t savedStack = saveStack(fiber);
			emitByte(cCtx, OP_INVOKE_REF);
			emitPendingRef(cCtx, nameHandle, MEMBER_ANY_MASK, REF_THIS, &error);
			IF_RAISED_RESTORE_RAISE_PARSE_ERR_RET_VAL(&error, fiber, savedStack,
													  cCtx, "Out of memory", ETYPE_NORMAL);
			emitBytes(cCtx, argCount, hasExpansions);
		} else {
			emitByte(cCtx, OP_INVOKE);
			emitUShort(cCtx, nameHandle);
			emitBytes(cCtx, argCount, hasExpansions);
		}
	} else {
		if (isThisRef) {
			EloxError error = ELOX_ERROR_INITIALIZER;
			size_t savedStack = saveStack(fiber);
			emitByte(cCtx, OP_GET_REF);
			emitPendingRef(cCtx, nameHandle, MEMBER_ANY_MASK, REF_THIS, &error);
			IF_RAISED_RESTORE_RAISE_PARSE_ERR_RET_VAL(&error, fiber, savedStack,
													  cCtx, "Out of memory", ETYPE_NORMAL);
		} else {
			emitByte(cCtx, OP_GET_PROP);
			emitUShort(cCtx, nameHandle);
		}
	}

	return ETYPE_NORMAL;
}

static ExpressionType dot(CCtx *cCtx, bool canAssign,
						  bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	consume(cCtx, TOKEN_IDENTIFIER, "Expect property name after '.'");
	Token *propName = &parser->previous;
	suint16_t name = identifierConstant(cCtx, &propName->string);
	CHECK_RAISE_PARSE_ERR_RET_VAL((name < 0), "Out of memory", ETYPE_NORMAL);

	if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL)) {
		expression(cCtx, PREC_ASSIGNMENT, 0, false);
		emitByte(cCtx, OP_MAP_SET);
		emitUShort(cCtx, name);
	} else {
		emitByte(cCtx, OP_MAP_GET);
		emitUShort(cCtx, name);
	}

	return ETYPE_NORMAL;
}

static ExpressionType literal(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							  bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	switch (parser->previous.type) {
		case TOKEN_FALSE:
			emitByte(cCtx, OP_FALSE);
			break;
		case TOKEN_NIL:
			emitByte(cCtx, OP_NIL);
			break;
		case TOKEN_TRUE:
			emitByte(cCtx, OP_TRUE);
			break;
		default:
			ELOX_UNREACHABLE();
	}

	return ETYPE_NORMAL;
}

static ExpressionType grouping(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							   bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	expression(cCtx, PREC_ASSIGNMENT, EXPR_ALLOW_TUPLE, false);
	consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after expression");
	return ETYPE_NORMAL;
}

static ExpressionType array(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	int itemCount = 0;
	if (!check(cCtx, TOKEN_RIGHT_BRACKET)) {
		do {
			if (check(cCtx, TOKEN_RIGHT_BRACKET)) {
				// Let's support a trailing comma
				break;
			}

			expression(cCtx, PREC_ASSIGNMENT, 0, false);

			if (itemCount == UINT16_COUNT)
				compileError(cCtx, "Cannot have more than 16384 items in an array literal");
			itemCount++;
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	}

	consume(cCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after array literal");

	emitByte(cCtx, OP_NEW_ARRAY);
	emitUShort(cCtx, (uint16_t)itemCount);
	return ETYPE_NORMAL;
}

static ExpressionType index_(CCtx *cCtx, bool canAssign,
							 bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	bool isSlice = false;

	if (consumeIfMatch(cCtx, TOKEN_DOT_DOT)) {
		emitByte(cCtx, OP_NIL);
		isSlice = true;
	} else
		expression(cCtx, PREC_ASSIGNMENT, 0, false);

	if (isSlice || consumeIfMatch(cCtx, TOKEN_DOT_DOT)) {
		// slice
		if (consumeIfMatch(cCtx, TOKEN_RIGHT_BRACKET))
			emitByte(cCtx, OP_NIL);
		else {
			expression(cCtx, PREC_ASSIGNMENT, 0, false);
			consume(cCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after slice");
		}
		emitByte(cCtx, OP_SLICE);
	} else {
		// index
		consume(cCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after index");

		if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL)) {
			expression(cCtx, PREC_ASSIGNMENT, 0, false);
			emitByte(cCtx, OP_INDEX_STORE);
		} else
			emitByte(cCtx, OP_INDEX);
	}

	return ETYPE_NORMAL;
}

static ExpressionType map(CCtx *cCtx, bool canAssign ELOX_UNUSED,
						  bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	int itemCount = 0;

	if (!check(cCtx, TOKEN_RIGHT_BRACE)) {
		do {
			if (check(cCtx, TOKEN_RIGHT_BRACE)) {
				// Let's support a trailing comma
				break;
			}

			if (consumeIfMatch(cCtx, TOKEN_IDENTIFIER)) {
				suint16_t key = identifierConstant(cCtx, &parser->previous.string);
				CHECK_RAISE_PARSE_ERR_RET_VAL((key < 0), "Out of memory", ETYPE_NORMAL);
				emitConstantOp(cCtx, key);
			} else {
				consume(cCtx, TOKEN_LEFT_BRACKET, "Expecting identifier or index expression as key");
				expression(cCtx, PREC_ASSIGNMENT, 0, false);
				consume(cCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after index");
			}
			consume(cCtx, TOKEN_EQUAL, "Expect '=' between key and value pair");
			expression(cCtx, PREC_ASSIGNMENT, 0, false);

			if (itemCount == UINT16_COUNT)
				compileError(cCtx,  "No more than 65536 items allowed in a map constructor");
			itemCount++;
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	}
	consume(cCtx, TOKEN_RIGHT_BRACE, "Expect '}' after map elements");

	emitByte(cCtx, OP_NEW_MAP);
	emitUShort(cCtx, itemCount);

	return ETYPE_NORMAL;
}

static bool identifiersEqual(Token *a, Token *b) {
	if (a->string.length != b->string.length)
		return false;
	return memcmp(a->string.chars, b->string.chars, a->string.length) == 0;
}

static Local *addLocal(CCtx *cCtx, Token name, uint8_t *handle) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	if (current->localCount == UINT8_COUNT) {
		compileError(cCtx, "Too many local variables in function");
		*handle = 0;
		return NULL;
	}

	Local *local = &current->locals[current->localCount++];
	local->name = name;
	local->depth = -1;
	local->postArgs = current->postArgs;
	local->isCaptured = false;
#ifdef ELOX_DEBUG_PRINT_CODE
	eloxPrintf(cCtx->runCtx, ELOX_IO_DEBUG, ">>>Local[%u][%d] <- %.*s\n",
			   current->id, current->localCount-1, name.string.length, name.string.chars);
#endif

	*handle = current->localCount - 1;

	return local;
}

static int declareVariable(CCtx *cCtx, VarScope varType) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;
	Parser *parser = &compilerState->parser;

	if (varType == VAR_GLOBAL)
		return 0;

	Token *name = &parser->previous;
	for (int i = current->localCount - 1; i >= 0; i--) {
		Local *local = &current->locals[i];
		if (local->depth != -1 && local->depth < current->scopeDepth)
			break;

		if (identifiersEqual(name, &local->name)) {
			compileError(cCtx, "Duplicated variable in this scope");
			return -1;
		}
	}

	uint8_t handle;
	addLocal(cCtx, *name, &handle);
	return handle;
}

static uint16_t parseVariable(CCtx *cCtx, VarScope varType, const char *errorMessage) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	consume(cCtx, TOKEN_IDENTIFIER, errorMessage);

	declareVariable(cCtx, varType);
	if (varType == VAR_LOCAL)
		return 0;

	return globalIdentifierConstant(cCtx->runCtx, &parser->previous.string, &cCtx->moduleName);
}

static void markInitialized(FunctionCompiler *current, VarScope varType) {
	if (varType == VAR_GLOBAL)
		return;
	current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(CCtx *cCtx, uint16_t nameGlobal, VarScope varType) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	switch (varType) {
		case VAR_LOCAL:
			markInitialized(current, VAR_LOCAL);
			break;
		case VAR_GLOBAL:
			emitByte(cCtx, OP_DEFINE_GLOBAL);
			emitUShort(cCtx, nameGlobal);
			break;
		default:
			assert(false);
	}
}

static void block(CCtx *cCtx) {
	while (!check(cCtx, TOKEN_RIGHT_BRACE) && !check(cCtx, TOKEN_EOF))
		declaration(cCtx);

	consume(cCtx, TOKEN_RIGHT_BRACE, "Expect '}' after block");
}

static int resolveLocal(CCtx *cCtx, FunctionCompiler *compiler, Token *name, bool *postArgs) {
	for (int i = compiler->localCount - 1; i >= 0; i--) {
		Local *local = &compiler->locals[i];
		if (identifiersEqual(name, &local->name)) {
			if (local->depth == -1)
				compileError(cCtx, "Can't read local variable in its own initializer");
			*postArgs = local->postArgs;
#ifdef ELOX_DEBUG_PRINT_CODE
			eloxPrintf(cCtx->runCtx, ELOX_IO_DEBUG, "???Local[%u][%d]    %.*s\n",
					   compiler->id, i, local->name.string.length, local->name.string.chars);
#endif
			return i;
		}
	}

	return -1;
}

static int addUpvalue(CCtx *cCtx, FunctionCompiler *compiler,
					  uint8_t index, bool postArgs, bool isLocal) {

	int upvalueCount = compiler->function->upvalueCount;

	for (int i = 0; i < upvalueCount; i++) {
		Upvalue *upvalue = &compiler->upvalues[i];
		if (upvalue->index == index && upvalue->isLocal == isLocal)
			return i;
	}

	if (upvalueCount == UINT8_COUNT) {
		compileError(cCtx, "Too many closure variables in function");
		return 0;
	}

	compiler->upvalues[upvalueCount].isLocal = isLocal;
	compiler->upvalues[upvalueCount].index = index;
	compiler->upvalues[upvalueCount].postArgs = postArgs;
	return compiler->function->upvalueCount++;
}

static int resolveUpvalue(CCtx *cCtx, FunctionCompiler *compiler, Token *name) {
	if (compiler->enclosing == NULL)
		return -1;

	bool postArgs;
	int local = resolveLocal(cCtx, compiler->enclosing, name, &postArgs);
	if (local != -1) {
			compiler->enclosing->locals[local].isCaptured = true;
			int uval = addUpvalue(cCtx, compiler, (uint8_t)local, postArgs, true);
#ifdef ELOX_DEBUG_PRINT_CODE
			eloxPrintf(cCtx->runCtx, ELOX_IO_DEBUG, ">>>UpVal[%d@%d] (%.*s)\n", uval, local,
					   name->string.length, name->string.chars);
#endif
			return uval;
	}

	int upvalue = resolveUpvalue(cCtx, compiler->enclosing, name);
	if (upvalue != -1) {
		int uval = addUpvalue(cCtx, compiler, (uint8_t)upvalue, false, false);
#ifdef ELOX_DEBUG_PRINT_CODE
		eloxPrintf(cCtx->runCtx, ELOX_IO_DEBUG, ">>>UpVal[%d@%d] (%.*s)\n", uval, upvalue,
				   name->string.length, name->string.chars);
#endif
		return uval;
	}

	return -1;
}

typedef struct {
	int handle;
	bool postArgs;
	bool isShort;
	bool isLocal;
} ArgDesc;

static void emitLoad(CCtx *cCtx, ArgDesc *arg, uint8_t getOp) {
	emitByte(cCtx, getOp);
	if (arg->isShort)
		emitUShort(cCtx, (uint16_t)arg->handle);
	else {
		emitByte(cCtx, (uint8_t)arg->handle);
		if (arg->isLocal)
			emitByte(cCtx, (uint8_t)arg->postArgs);
	}
}

static void emitStore(CCtx *cCtx, ArgDesc *arg, uint8_t setOp) {
	emitByte(cCtx, setOp);
	if (arg->isShort)
		emitUShort(cCtx, (uint16_t)arg->handle);
	else {
		emitByte(cCtx, (uint8_t)arg->handle);
		if (arg->isLocal)
			emitByte(cCtx, (uint8_t)arg->postArgs);
	}
}

static void emitShorthandAssign(CCtx *cCtx, ArgDesc *arg,
								uint8_t getOp, uint8_t setOp, uint8_t op) {
	emitLoad(cCtx, arg, getOp);
	expression(cCtx, PREC_ASSIGNMENT, 0, false);
	emitByte(cCtx, op);
	emitStore(cCtx, arg, setOp);
}

static void emitLoadOrAssignVariable(CCtx *cCtx, Token name, bool canAssign) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;
	Parser *parser = &compilerState->parser;
	VM *vm = cCtx->runCtx->vm;

	uint8_t getOp, setOp;
	ArgDesc arg = { .postArgs = false, .isShort = false, .isLocal = false };
	arg.handle = resolveLocal(cCtx, current, &name, &arg.postArgs);
	if (arg.handle != -1) {
		getOp = OP_GET_LOCAL;
		setOp = OP_SET_LOCAL;
		arg.isLocal = true;
	} else if ((arg.handle = resolveUpvalue(cCtx, current, &name)) != -1) {
		getOp = OP_GET_UPVALUE;
		setOp = OP_SET_UPVALUE;
	} else {
		const String *varName = &name.string;
		const String *moduleName = NULL;
		if (consumeIfMatch(cCtx, TOKEN_DOUBLE_COLON)) {
			consume(cCtx, TOKEN_IDENTIFIER, "Var name expected after ::");
			moduleName = &name.string;
			varName = &parser->previous.string;
		}

		bool isBuiltin = false;
		uint16_t builtinIndex;
		if (moduleName == NULL) {
			uint32_t nameHash = hashString(name.string.chars, name.string.length);
			Value indexVal;
			isBuiltin = tableGetString(&vm->builtinSymbols,
									   name.string.chars, name.string.length, nameHash, &indexVal);
			if (isBuiltin)
				builtinIndex = AS_NUMBER(indexVal);
			else
				moduleName = &cCtx->moduleName;
		}

		if (isBuiltin) {
			getOp = OP_GET_BUILTIN;
			setOp = OP_INVALID;
			arg.handle = builtinIndex;
		} else {
			arg.handle = globalIdentifierConstant(cCtx->runCtx, varName, moduleName);
			getOp = OP_GET_GLOBAL;
			setOp = OP_SET_GLOBAL;
		}
		arg.isShort = true;
	}

	if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL)) {
		expression(cCtx, PREC_ASSIGNMENT, 0, false);
		emitStore(cCtx, &arg, setOp);
	} else if (canAssign && consumeIfMatch(cCtx, TOKEN_PLUS_EQUAL))
		emitShorthandAssign(cCtx, &arg, getOp, setOp, OP_ADD);
	else if (canAssign && consumeIfMatch(cCtx, TOKEN_MINUS_EQUAL))
		emitShorthandAssign(cCtx, &arg, getOp, setOp, OP_SUBTRACT);
	else if (canAssign && consumeIfMatch(cCtx, TOKEN_STAR_EQUAL))
		emitShorthandAssign(cCtx, &arg, getOp, setOp, OP_MULTIPLY);
	else if (canAssign && consumeIfMatch(cCtx, TOKEN_SLASH_EQUAL))
		emitShorthandAssign(cCtx, &arg, getOp, setOp, OP_DIVIDE);
	else if (canAssign && consumeIfMatch(cCtx, TOKEN_PERCENT_EQUAL))
		emitShorthandAssign(cCtx, &arg, getOp, setOp, OP_MODULO);
	else
		emitLoad(cCtx, &arg, getOp);
}

Token syntheticToken(const uint8_t *text) {
	Token token;
	token.string.chars = text;
	token.string.length = (int)strlen((const char *)text);
	return token;
}

static Value parseConstant(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;
	RunCtx *runCtx = cCtx->runCtx;

	if (consumeIfMatch(cCtx, TOKEN_NIL))
		return NIL_VAL;
	else if (consumeIfMatch(cCtx, TOKEN_FALSE))
		return BOOL_VAL(false);
	else if (consumeIfMatch(cCtx, TOKEN_TRUE))
		return BOOL_VAL(true);
	else if (consumeIfMatch(cCtx, TOKEN_NUMBER)) {
		double value = strtod((const char *)parser->previous.string.chars, NULL);
		return NUMBER_VAL(value);
	} else if (consumeIfMatch(cCtx, TOKEN_STRING)) {
		ObjString *str = copyString(runCtx,
									parser->previous.string.chars + 1,
									parser->previous.string.length - 2);
		CHECK_RAISE_PARSE_ERR_RET_VAL((str == NULL), "Out of memory", NIL_VAL);
		return OBJ_VAL(str);
	} else
		errorAtCurrent(cCtx, "Constant expected");

	return NIL_VAL;
}

static void parsePrototype(CCtx *cCtx, Prototype *proto, FunctionCompiler *compiler) {
	consume(cCtx, TOKEN_LEFT_PAREN, "Expect '(' after function name");
	if (!check(cCtx, TOKEN_RIGHT_PAREN)) {
		do {
			proto->arity++;
			if (proto->arity > 255)
				errorAtCurrent(cCtx, "Can't have more than 255 parameters");
			if (consumeIfMatch(cCtx, TOKEN_ELLIPSIS)) {
				proto->arity--;
				proto->hasVarargs = true;
				if (!check(cCtx, TOKEN_RIGHT_PAREN))
					errorAtCurrent(cCtx, "Expected ) after ...");
			} else {
				if (compiler == NULL)
					consume(cCtx, TOKEN_IDENTIFIER, "Expect parameter name");
				else {
					uint16_t constant = parseVariable(cCtx, VAR_LOCAL, "Expect parameter name");
					defineVariable(cCtx, constant, VAR_LOCAL);
					if (consumeIfMatch(cCtx, TOKEN_EQUAL))
						compiler->defaultArgs[compiler->numArgs++] = parseConstant(cCtx);
					else
						compiler->defaultArgs[compiler->numArgs++] = NIL_VAL;
				}
			}
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	}
	consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after parameters");
}

static suint16_t function(CCtx *cCtx, FunctionType type, Prototype *proto,
						  MethodCompiler *methodCompiler) {
	RunCtx *runCtx = cCtx->runCtx;
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	FunctionCompiler functionCompiler;
	FunctionCompiler *current = initFunctionCompiler(cCtx, &functionCompiler, methodCompiler,
													 type, &parser->previous);
	ObjFunction *currentFunction = current->function;
	beginScope(cCtx);

	parsePrototype(cCtx, proto, current);
	currentFunction->arity = proto->arity;

	if ((type == FTYPE_METHOD) || (type == FTYPE_DEFAULT)) {
		currentFunction->arity++; // for this
		currentFunction->isMethod = true;
	}
	current->hasVarargs = proto->hasVarargs;

	currentFunction->maxArgs = current->hasVarargs ? ELOX_MAX_ARGS : currentFunction->arity;
	current->postArgs = true;

	if (currentFunction->arity > 0) {
		currentFunction->defaultArgs = ALLOCATE(runCtx, Value, currentFunction->arity);
		CHECK_RAISE_PARSE_ERR_RET_VAL((currentFunction->defaultArgs == NULL), "Out of memory", -1);
		memcpy(currentFunction->defaultArgs, current->defaultArgs,
			   currentFunction->arity * sizeof(Value));
	}

	uint8_t argCount = 0;
	bool hasExpansions = false;
	if (type == FTYPE_INITIALIZER) {
		emitLoadOrAssignVariable(cCtx, syntheticToken(U8("this")), false);
		compilerState->currentKlass->hasExplicitInitializer = true;
	}
	if (consumeIfMatch(cCtx, TOKEN_COLON)) {
		if (type != FTYPE_INITIALIZER)
			errorAtCurrent(cCtx, "Only initializers can be chained");
		consume(cCtx, TOKEN_SUPER, "Expect 'super'");
		consume(cCtx, TOKEN_LEFT_PAREN, "Expect super argument list");
		argCount = argumentList(cCtx, &hasExpansions);
	}
	if (type == FTYPE_INITIALIZER) {
		emitLoadOrAssignVariable(cCtx, syntheticToken(U8("super")), false);
		emitByte(cCtx, OP_SUPER_INIT);
		emitBytes(cCtx, argCount, hasExpansions);
		emitByte(cCtx, OP_POP); // discard super init result
	}

	consume(cCtx, TOKEN_LEFT_BRACE, "Expect '{' before function body");
	block(cCtx);

	ObjFunction *function = endCompiler(cCtx);

	suint16_t functionConstant = makeConstant(cCtx, OBJ_VAL(function));
	if (type == FTYPE_DEFAULT)
		return functionConstant;
	if (function->upvalueCount > 0) {
		emitByte(cCtx, OP_CLOSURE);
		emitUShort(cCtx, functionConstant);

		// Emit arguments for each upvalue to know whether to capture a local or an upvalue
		for (int i = 0; i < function->upvalueCount; i++) {
			emitByte(cCtx, functionCompiler.upvalues[i].isLocal ? 1 : 0);
			emitByte(cCtx, functionCompiler.upvalues[i].index);
		}
	} else {
		// No need to create a closure
		emitConstantOp(cCtx, functionConstant);
	}

	return functionConstant;
}

static int declareLocal(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	int localHandle = declareVariable(cCtx, VAR_LOCAL);
	if (localHandle >= 0)
		markInitialized(current, VAR_LOCAL);
	return localHandle;
}

typedef enum {
	IMPORT_MODULE,
	IMPORT_SYMBOLS
} ImportType;

static void importStatement(CCtx *cCtx, ImportType importType) {
	RunCtx *runCtx = cCtx->runCtx;
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	emitByte(cCtx, OP_IMPORT);

	consume(cCtx, TOKEN_IDENTIFIER, "Expect module name");
	String moduleName = parser->previous.string;
	suint16_t moduleConstant = identifierConstant(cCtx, &moduleName);
	CHECK_RAISE_PARSE_ERR_RET((moduleConstant < 0), "Out of memory");
	emitUShort(cCtx, moduleConstant);
	int symOffset = emitUShort(cCtx, 0);

	if (importType == IMPORT_SYMBOLS) {
		consume(cCtx, TOKEN_IMPORT, "Expect 'import'");
		uint16_t numSymbols = 0;
		do {
			consume(cCtx, TOKEN_IDENTIFIER, "Expect symbol name");
			declareLocal(cCtx);
			uint16_t symbol = globalIdentifierConstant(runCtx, &parser->previous.string, &moduleName);
			emitUShort(cCtx, symbol);
			numSymbols++;
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));

		patchUShort(cCtx, symOffset, numSymbols);
	}

	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after import");
}

static ExpressionType lambda(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							 bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	Prototype proto = { 0 };
	function(cCtx, FTYPE_LAMBDA, &proto, NULL);
	return ETYPE_NORMAL;
}

static ExpressionType number(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							 bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;
	double value = strtod((const char *)parser->previous.string.chars, NULL);
	emitConstant(cCtx, NUMBER_VAL(value));
	return ETYPE_NORMAL;
}

static ExpressionType fString(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							  bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	RunCtx *runCtx = cCtx->runCtx;
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	EloxTokenType incomingType = TOKEN_FSTRING_START;
	bool first = true;
	bool end = false;

	static const String toStringName = ELOX_STRING("toString");
	suint16_t toStringConst = identifierConstant(cCtx, &toStringName);
	CHECK_RAISE_PARSE_ERR_RET_VAL((toStringConst < 0), "Out of memory", ETYPE_NORMAL);

	bool started = false;

	while (true) {
		bool emitted = false;
		bool isStr = false;
		bool isExpr = false;


		switch (incomingType) {
			case TOKEN_FSTRING_START:
				if (started)
					isExpr = true;
				else {
					isStr = true;
					started = true;
				}
				break;
			case TOKEN_FSTRING_END:
				isStr = true;
				end = true;
				break;
			case TOKEN_FSTRING:
				isStr = true;
				break;
			case TOKEN_EOF:
				errorAtCurrent(cCtx, "Unterminated FString");
				end = true;
				break;
			default:
				isExpr = true;
				break;
		}

		if (isStr) {
			Token token = first ? parser->previous : parser->current;
			if (token.string.length > 2) {
				ObjString *str = copyString(runCtx,
											token.string.chars + 1,
											token.string.length - 2);
				CHECK_RAISE_PARSE_ERR_RET_VAL((str == NULL), "Out of memory", ETYPE_NORMAL);
				emitConstant(cCtx, OBJ_VAL(str));
				emitted = true;
			}
			// consume token; START already consumed
			if (!first)
				advance(cCtx);
		}

		if (isExpr) {
			expression(cCtx, PREC_ASSIGNMENT, 0, false);
			emitByte(cCtx, OP_INVOKE);
			emitUShort(cCtx, toStringConst);
			emitBytes(cCtx, 0, 0);
			emitted = true;
		}

		if (emitted && (!first))
			emitByte(cCtx, OP_ADD);
		if (emitted)
			first = false;

		if (end)
			break;

		incomingType = getCrtType(cCtx);
	}

	return ETYPE_NORMAL;
}

static ExpressionType or_(CCtx *cCtx, bool canAssign ELOX_UNUSED,
						  bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	int elseJump = emitJump(cCtx, OP_JUMP_IF_FALSE);
	int endJump = emitJump(cCtx, OP_JUMP);

	patchJump(cCtx, elseJump);
	emitByte(cCtx, OP_POP);

	expression(cCtx, PREC_OR, 0, false);
	patchJump(cCtx, endJump);
	return ETYPE_NORMAL;
}

static ExpressionType string(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							 bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	RunCtx *runCtx = cCtx->runCtx;
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	if (!check(cCtx, TOKEN_STRING)) {
		ObjString *str = copyString(runCtx,
									parser->previous.string.chars + 1,
									parser->previous.string.length - 2);
		CHECK_RAISE_PARSE_ERR_RET_VAL((str == NULL), "Out of memory", ETYPE_NORMAL);
		emitConstant(cCtx, OBJ_VAL(str));
	} else {
		HeapCString hStr;
		bool res = initHeapStringWithSize(runCtx, &hStr, parser->previous.string.length);
		CHECK_RAISE_PARSE_ERR_RET_VAL((!res), "Out of memory", ETYPE_NORMAL);
		heapStringAddString(runCtx, &hStr,
							parser->previous.string.chars + 1,
							parser->previous.string.length - 2);
		while (consumeIfMatch(cCtx, TOKEN_STRING)) {
			heapStringAddString(runCtx, &hStr,
								parser->previous.string.chars + 1,
								parser->previous.string.length - 2);
		}
		ObjString *str = takeString(runCtx, hStr.chars, hStr.length, hStr.capacity);
		CHECK_RAISE_PARSE_ERR_RET_VAL((str == NULL), "Out of memory", ETYPE_NORMAL);
		emitConstant(cCtx, OBJ_VAL(str));
	}

	return ETYPE_NORMAL;
}

typedef struct {
	VarScope scope;
	uint16_t handle; //slot for local or upvalue, name index for global
	bool postArgs; // only for locals
} VarRef;

static VarRef resolveVar(CCtx *cCtx, Token name) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;
	Parser *parser = &compilerState->parser;
	VM *vm = cCtx->runCtx->vm;

	bool postArgs;
	int slot = resolveLocal(cCtx, current, &name, &postArgs);
	if (slot >= 0)
		return (VarRef){ .scope = VAR_LOCAL, .handle = slot, .postArgs = postArgs };
	else if ((slot = resolveUpvalue(cCtx, current, &name)) >= 0)
		return (VarRef){ .scope = VAR_UPVALUE, .handle = slot };

	const String *symbolName = &name.string;
	const String *moduleName = NULL;
	if (consumeIfMatch(cCtx, TOKEN_DOUBLE_COLON)) {
		consume(cCtx, TOKEN_IDENTIFIER, "Var name expected after ::");
		moduleName = &name.string;
		symbolName = &parser->previous.string;
	}
	bool isBuiltin = false;
	if (moduleName == NULL) {
		uint32_t nameHash = hashString(name.string.chars, name.string.length);
		isBuiltin = tableFindString(&vm->builtinSymbols,
										 name.string.chars, name.string.length, nameHash);
		if (!isBuiltin)
			moduleName = &cCtx->moduleName;
	}

	if (isBuiltin) {
		return (VarRef){ .scope = VAR_BUILTIN,
						 .handle = builtinConstant(cCtx->runCtx, symbolName) };
	} else {
		return (VarRef){ .scope = VAR_GLOBAL,
						 .handle = globalIdentifierConstant(cCtx->runCtx, symbolName, moduleName) };
	}
}

static void emitUnpack(CCtx *cCtx, uint8_t numVal, VarRef *slots) {
	emitByte(cCtx, OP_UNPACK);
	emitByte(cCtx, numVal);
	for (int i = 0; i < numVal; i++) {
		emitByte(cCtx, slots[i].scope);
		switch(slots[i].scope) {
			case VAR_LOCAL:
				emitBytes(cCtx, slots[i].handle, slots[i].postArgs);
				break;
			case VAR_GLOBAL:
				emitUShort(cCtx, slots[i].handle);
				break;
			case VAR_UPVALUE:
				emitByte(cCtx, slots[i].handle);
				break;
			case VAR_BUILTIN:
				emitByte(cCtx, slots[i].handle);
				errorAtCurrent(cCtx, "Cannot override builtins");
				break;
		}
	}
}

static ExpressionType variable(CCtx *cCtx, bool canAssign,
							   bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;
	emitLoadOrAssignVariable(cCtx, parser->previous, canAssign);
	return ETYPE_NORMAL;
}

static ExpressionType ellipsis(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							   bool canExpand, bool firstExpansion) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;
	Parser *parser = &compilerState->parser;

	if (!current->hasVarargs) {
		errorAtCurrent(cCtx, "Function does not have varargs");
		return ETYPE_NORMAL;
	}

	ExpressionType eType = ETYPE_NORMAL;

	if (consumeIfMatch(cCtx, TOKEN_LEFT_BRACKET)) {
		emitByte(cCtx, OP_GET_VARARGS);

		expression(cCtx, PREC_ASSIGNMENT, 0, false);
		consume(cCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after index");

		if (consumeIfMatch(cCtx, TOKEN_EQUAL))
			errorAtCurrent(cCtx, "Cannot assign to vararg");
		else
			emitByte(cCtx, OP_INDEX);
	} else if (consumeIfMatch(cCtx, TOKEN_COLON)) {
		consume(cCtx, TOKEN_IDENTIFIER, "Expect property name after ':'");
		Token *propName = &parser->previous;
		suint16_t nameHandle = identifierConstant(cCtx, &propName->string);
		CHECK_RAISE_PARSE_ERR_RET_VAL((nameHandle < 0), "Out of memory", ETYPE_NORMAL);

		emitByte(cCtx, OP_GET_VARARGS);

		consume(cCtx, TOKEN_LEFT_PAREN, "Expect '(' after method name");
		bool hasExpansions;
		uint8_t argCount = argumentList(cCtx, &hasExpansions);
		emitByte(cCtx, OP_INVOKE);
		emitUShort(cCtx, nameHandle);
		emitBytes(cCtx, argCount, hasExpansions);
	} else {
		if (!canExpand)
			errorAtCurrent(cCtx, "... used in a context that doesn't allow expansion");
		emitByte(cCtx, OP_GET_VARARGS);
		if (!firstExpansion)
			emitByte(cCtx, OP_SWAP);
		emitBytes(cCtx, OP_EXPAND, firstExpansion);

		eType = ETYPE_EXPAND;
	}

	return eType;
}

static ExpressionType expand(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							 bool canExpand, bool firstExpansion) {
	if (!canExpand)
		errorAtCurrent(cCtx, ".. used in a context that doesn't allow expansion");

	expression(cCtx, PREC_ASSIGNMENT, 0, false);
	if (!firstExpansion)
		emitByte(cCtx, OP_SWAP);
	emitBytes(cCtx, OP_EXPAND, firstExpansion);

	return ETYPE_EXPAND;
}

static ExpressionType this_(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	KlassCompiler *currentKlass = compilerState->currentKlass;

	if (currentKlass == NULL) {
		compileError(cCtx, "Can't use 'this' outside of a class");
		return ETYPE_NORMAL;
	}

	variable(cCtx, false, false, false);
	return ETYPE_NORMAL;
}

static ExpressionType super_(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							 bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;
	KlassCompiler *currentKlass = compilerState->currentKlass;
	RunCtx *runCtx = cCtx->runCtx;
	ObjFiber *fiber = runCtx->activeFiber;

	if (currentKlass == NULL)
		compileError(cCtx, "Can't use 'super' outside of a class");

	consume(cCtx, TOKEN_COLON, "Expect ':' after 'super'");
	consume(cCtx, TOKEN_IDENTIFIER, "Expect superclass method name");

	String *name = &parser->previous.string;
	suint16_t nameHandle = identifierConstant(cCtx, name);
	CHECK_RAISE_PARSE_ERR_RET_VAL((nameHandle < 0), "Out of memory", ETYPE_NORMAL);

	emitLoadOrAssignVariable(cCtx, syntheticToken(U8("this")), false);
	if (consumeIfMatch(cCtx, TOKEN_LEFT_PAREN)) {
		bool hasExpansions;
		uint8_t argCount = argumentList(cCtx, &hasExpansions);
		EloxError error = ELOX_ERROR_INITIALIZER;
		size_t savedStack = saveStack(fiber);
		emitByte(cCtx, OP_INVOKE_REF);
		emitPendingRef(cCtx, nameHandle, MEMBER_METHOD_MASK, REF_SUPER, &error);
		IF_RAISED_RESTORE_RAISE_PARSE_ERR_RET_VAL(&error, fiber, savedStack,
												  cCtx, "Out of memory", ETYPE_NORMAL);
		emitBytes(cCtx, argCount, hasExpansions);
	} else {
		EloxError error = ELOX_ERROR_INITIALIZER;
		size_t savedStack = saveStack(fiber);
		emitByte(cCtx, OP_GET_REF);
		emitPendingRef(cCtx, nameHandle, MEMBER_ANY_MASK, REF_THIS, &error);
		IF_RAISED_RESTORE_RAISE_PARSE_ERR_RET_VAL(&error, fiber, savedStack,
												  cCtx, "Out of memory", ETYPE_NORMAL);
	}

	return ETYPE_NORMAL;
}

static ExpressionType anonIntf(CCtx *cCtx, bool canAssign, bool canExpand, bool firstExpansion);
static ExpressionType anonClass(CCtx *cCtx, bool canAssign, bool canExpand, bool firstExpansion);
static ExpressionType abstract(CCtx *cCtx, bool canAssign, bool canExpand, bool firstExpansion);

static ParseRule parseRules[] = {
	[TOKEN_LEFT_PAREN]    = {grouping,  call,   PREC_CALL},
	[TOKEN_RIGHT_PAREN]   = {NULL,      NULL,   PREC_NONE},
	[TOKEN_LEFT_BRACE]    = {map,       NULL,   PREC_NONE},
	[TOKEN_RIGHT_BRACE]   = {NULL,      NULL,   PREC_NONE},
	[TOKEN_LEFT_BRACKET]  = {array,     index_, PREC_SUBSCRIPT},
	[TOKEN_RIGHT_BRACKET] = {NULL,      NULL,   PREC_NONE},
	[TOKEN_COMMA]         = {NULL,      NULL,   PREC_NONE},
	[TOKEN_DOT]           = {NULL,      dot,    PREC_CALL},
	[TOKEN_MINUS]         = {unary,     binary, PREC_TERM},
	[TOKEN_PERCENT]       = {NULL,      binary, PREC_FACTOR},
	[TOKEN_PLUS]          = {NULL,      binary, PREC_TERM},
	[TOKEN_COLON]         = {NULL,      colon,  PREC_CALL},
	[TOKEN_SEMICOLON]     = {NULL,      NULL,   PREC_NONE},
	[TOKEN_SLASH]         = {NULL,      binary, PREC_FACTOR},
	[TOKEN_STAR]          = {NULL,      binary, PREC_FACTOR},
	[TOKEN_BANG]          = {unary,     NULL,   PREC_NONE},
	[TOKEN_BANG_EQUAL]    = {NULL,      binary, PREC_EQUALITY},
	[TOKEN_EQUAL]         = {NULL,      NULL,   PREC_NONE},
	[TOKEN_EQUAL_EQUAL]   = {NULL,      binary, PREC_EQUALITY},
	[TOKEN_GREATER]       = {NULL,      binary, PREC_COMPARISON},
	[TOKEN_GREATER_EQUAL] = {NULL,      binary, PREC_COMPARISON},
	[TOKEN_LESS]          = {NULL,      binary, PREC_COMPARISON},
	[TOKEN_LESS_EQUAL]    = {NULL,      binary, PREC_COMPARISON},
	[TOKEN_PLUS_EQUAL]    = {NULL,      binary, PREC_NONE},
	[TOKEN_MINUS_EQUAL]   = {NULL,      binary, PREC_NONE},
	[TOKEN_STAR_EQUAL]    = {NULL,      binary, PREC_NONE},
	[TOKEN_SLASH_EQUAL]   = {NULL,      binary, PREC_NONE},
	[TOKEN_PERCENT_EQUAL] = {NULL,      binary, PREC_NONE},
	[TOKEN_INSTANCEOF]    = {NULL,      binary, PREC_COMPARISON},
	[TOKEN_IN]            = {NULL,      binary, PREC_COMPARISON},
	[TOKEN_IMPORT]        = {NULL,      NULL,   PREC_NONE},
	[TOKEN_IDENTIFIER]    = {variable,  NULL,   PREC_NONE},
	[TOKEN_ELLIPSIS]      = {ellipsis,  NULL,   PREC_NONE},
	[TOKEN_DOT_DOT]       = {expand,    NULL,   PREC_NONE},
	[TOKEN_STRING]        = {string,    NULL,   PREC_NONE},
	[TOKEN_NUMBER]        = {number,    NULL,   PREC_NONE},
	[TOKEN_FSTRING_START] = {fString,   NULL,   PREC_NONE},
	[TOKEN_AND]           = {NULL,      and_,   PREC_AND},
	[TOKEN_BREAK]         = {NULL,      NULL,   PREC_NONE},
	[TOKEN_CATCH]         = {NULL,      NULL,   PREC_NONE},
	[TOKEN_ABSTRACT]      = {abstract,  NULL,   PREC_NONE},
	[TOKEN_INTERFACE]     = {anonIntf,  NULL,   PREC_NONE},
	[TOKEN_CLASS]         = {anonClass, NULL,   PREC_NONE},
	[TOKEN_CONTINUE]      = {NULL,      NULL,   PREC_NONE},
	[TOKEN_ELSE]          = {NULL,      NULL,   PREC_NONE},
	[TOKEN_FALSE]         = {literal,   NULL,   PREC_NONE},
	[TOKEN_FOR]           = {NULL,      NULL,   PREC_NONE},
	[TOKEN_FUNCTION]      = {lambda,    NULL,   PREC_NONE},
	[TOKEN_IF]            = {NULL,      NULL,   PREC_NONE},
	[TOKEN_NIL]           = {literal,   NULL,   PREC_NONE},
	[TOKEN_OR]            = {NULL,      or_,    PREC_OR},
	[TOKEN_RETURN]        = {NULL,      NULL,   PREC_NONE},
	[TOKEN_SUPER]         = {super_,    NULL,   PREC_NONE},
	[TOKEN_THIS]          = {this_,     NULL,   PREC_NONE},
	[TOKEN_THROW]         = {NULL,      NULL,   PREC_NONE},
	[TOKEN_TRUE]          = {literal,   NULL,   PREC_NONE},
	[TOKEN_LOCAL]         = {NULL,      NULL,   PREC_NONE},
	[TOKEN_GLOBAL]        = {NULL,      NULL,   PREC_NONE},
	[TOKEN_WHILE]         = {NULL,      NULL,   PREC_NONE},
	[TOKEN_ERROR]         = {NULL,      NULL,   PREC_NONE},
	[TOKEN_EOF]           = {NULL,      NULL,   PREC_NONE},
};

static ExpressionType and_(CCtx *cCtx, bool canAssign ELOX_UNUSED,
						   bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	int endJump = emitJump(cCtx, OP_JUMP_IF_FALSE);

	emitByte(cCtx, OP_POP);
	expression(cCtx, PREC_AND, 0, false);

	patchJump(cCtx, endJump);
	return ETYPE_NORMAL;
}

static ParseRule *getRule(EloxTokenType type) {
	return &parseRules[type];
}

static void emitField(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	consume(cCtx, TOKEN_IDENTIFIER, "Expect field name");
	suint16_t constant = identifierConstant(cCtx, &parser->previous.string);
	CHECK_RAISE_PARSE_ERR_RET((constant < 0), "Out of memory");
	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after field declaration");

	emitByte(cCtx, OP_FIELD);
	emitUShort(cCtx, constant);
}

MethodCompiler *initMethodCompiler(MethodCompiler *mc) {
	if (mc == NULL)
		return NULL;
	initValueArray(&mc->pendingRefs);
	return mc;
}

void freeMethodCompiler(RunCtx *runCtx, MethodCompiler *mc) {
	freeValueArray(runCtx, &mc->pendingRefs);
}

static void emitPendingRefs(CCtx *cCtx, MethodCompiler *mc) {
	ValueArray *pendingRefs = &mc->pendingRefs;
	emitUShort(cCtx, pendingRefs->count);
	for (uint32_t i = 0; i < pendingRefs->count; i++) {
		uint64_t val = AS_NUMBER(pendingRefs->values[i]);
		uint32_t offset = val & 0xFFFFFF;
		uint8_t memberType = (val & MEMBER_ANY_MASK) >> 30;
		uint16_t nameHandle = (val >> 32) & 0xFFFF;
		uint8_t refType = (val >> 48) & 0xF;

		int8_t slotType = refType | memberType << 1;
		emitInt(cCtx, offset);
		emitByte(cCtx, slotType);
		emitUShort(cCtx, nameHandle);
	}
}

static void emitMethod(CCtx *cCtx, Token *className, FunctionType type) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	MethodCompiler methodCompiler;

	static const Token anonToken = TOKEN_INITIALIZER("$init");
	suint16_t nameConstant = 0;
	bool anonInit = false;
	if (check(cCtx, TOKEN_LEFT_PAREN)) {
		nameConstant = identifierConstant(cCtx, &anonToken.string);
		anonInit = true;
	} else {
		consume(cCtx, TOKEN_IDENTIFIER, "Expect method name");
		nameConstant = identifierConstant(cCtx, &parser->previous.string);
	}
	CHECK_RAISE_PARSE_ERR_RET((nameConstant < 0), "Out of memory");

	if (className != NULL) {
		if (parser->previous.string.length == className->string.length &&
			memcmp(parser->previous.string.chars, className->string.chars, className->string.length) == 0) {
			type = FTYPE_INITIALIZER;
		}
	} else if (anonInit)
		type = FTYPE_INITIALIZER;

	Prototype proto = { 0 };
	suint16_t functionHandle = function(cCtx, type, &proto, initMethodCompiler(&methodCompiler));
	CHECK_RAISE_PARSE_ERR_RET((functionHandle < 0), "Out of memory");

	emitByte(cCtx, type == FTYPE_DEFAULT ? OP_DEFAULT_METHOD : OP_METHOD);
	emitUShort(cCtx, nameConstant);

	// DEBUG needs function handle to display refs
	emitUShort(cCtx, functionHandle);

	emitPendingRefs(cCtx, &methodCompiler);
	freeMethodCompiler(cCtx->runCtx, &methodCompiler);
}

static void emitAbstractMethod(CCtx *cCtx, uint8_t parentOffset) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	consume(cCtx, TOKEN_IDENTIFIER, "Expect method name");
	suint16_t nameConstant = identifierConstant(cCtx, &parser->previous.string);
	CHECK_RAISE_PARSE_ERR_RET((nameConstant < 0), "Out of memory");

	Prototype proto = { 0 };
	parsePrototype(cCtx, &proto, NULL);

	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after abstract method declaration");
	emitByte(cCtx, OP_ABS_METHOD);
	emitUShort(cCtx, nameConstant);
	emitByte(cCtx, parentOffset);
	emitBytes(cCtx, proto.arity, proto.hasVarargs);
}

static void _class(CCtx *cCtx, Token *className);

static void emitStaticClass(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;
	Qualifiers *quals = &compilerState->currentFunctionCompiler->quals;

	consume(cCtx, TOKEN_IDENTIFIER, "Expect class name");
	Token className = parser->previous;
	suint16_t nameConstant = identifierConstant(cCtx, &className.string);
	CHECK_RAISE_PARSE_ERR_RET((nameConstant < 0), "Out of memory");

	bool abstract = (quals->attrs & QUAL_ABSTRACT) != 0;

	beginScope(cCtx); // for temp class local
	uint8_t localHandle;
	Local *local = addLocal(cCtx, syntheticToken(U8("")), &localHandle);
	if (local != NULL) {
		emitByte(cCtx, OP_CLASS);
		emitUShort(cCtx, nameConstant);
		emitByte(cCtx, abstract);
		defineVariable(cCtx, 0, VAR_LOCAL);
		_class(cCtx, &className);
		emitByte(cCtx, OP_STATIC);
		emitUShort(cCtx, nameConstant);
	}
	endScope(cCtx);
}

static void interface(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	KlassCompiler klassCompiler;
	klassCompiler.enclosing = NULL;
	compilerState->currentKlass = &klassCompiler;

	consume(cCtx, TOKEN_LEFT_BRACE, "Expect '{' before interface body");
	while (!check(cCtx, TOKEN_RIGHT_BRACE) && !check(cCtx, TOKEN_EOF)) {
		CCtxState state;
		saveCCtxState(cCtx, &state);
		bool isAbstract = consumeIfMatch(cCtx, TOKEN_ABSTRACT);
		consume(cCtx, TOKEN_IDENTIFIER, "Expect method name");
		suint16_t methodNameConstant = identifierConstant(cCtx, &parser->previous.string);
		CHECK_RAISE_PARSE_ERR_RET((methodNameConstant < 0), "Out of memory");

		Prototype proto = { 0 };
		parsePrototype(cCtx, &proto, NULL);

		if (check(cCtx, TOKEN_LEFT_BRACE)) {
			// default method
			if (ELOX_UNLIKELY(isAbstract)) {
				errorAtCurrent(cCtx, "Abstract method cannot have a body");
			} else {
				restoreCCtxState(cCtx, &state);
				emitMethod(cCtx, NULL, FTYPE_DEFAULT);
			}
		} else {
			consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after method");
			emitByte(cCtx, OP_ABS_METHOD);
			emitUShort(cCtx, methodNameConstant);
			emitByte(cCtx, 0); // no local super in interfaces
			emitBytes(cCtx, proto.arity, proto.hasVarargs);
		}
	}
	consume(cCtx, TOKEN_RIGHT_BRACE, "Expect '}' after interface body");

	compilerState->currentKlass = NULL;
}

static void interfaceDeclaration(CCtx *cCtx, VarScope varType) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	uint16_t intfGlobal = parseVariable(cCtx, varType, "Expect interface name");
	suint16_t nameConstant = identifierConstant(cCtx, &parser->previous.string);
	CHECK_RAISE_PARSE_ERR_RET((nameConstant < 0), "Out of memory");

	emitByte(cCtx, OP_INTF);
	emitUShort(cCtx, nameConstant);
	interface(cCtx);

	defineVariable(cCtx, intfGlobal, varType);
}

static ExpressionType anonIntf(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							   bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	static const String emptyStr = ELOX_STRING("");
	suint16_t nameConstant = identifierConstant(cCtx, &emptyStr);
	CHECK_RAISE_PARSE_ERR_RET_VAL((nameConstant < 0), "Out of memory", ETYPE_NORMAL);

	emitByte(cCtx, OP_INTF);
	emitUShort(cCtx, nameConstant);
	interface(cCtx);

	return ETYPE_NORMAL;
}

typedef struct {
	Token *className;
	uint8_t localSlot;
} ClassContext;

static void _class(CCtx *cCtx, Token *className) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	KlassCompiler *currentKlass = compilerState->currentKlass;
	RunCtx *runCtx = cCtx->runCtx;
	ObjFiber *fiber = runCtx->activeFiber;

	KlassCompiler klassCompiler;
	klassCompiler.enclosing = currentKlass;
	klassCompiler.hasExplicitInitializer = false;
	compilerState->currentKlass = currentKlass = &klassCompiler;

	if (consumeIfMatch(cCtx, TOKEN_EXTENDS)) {
		expression(cCtx, PREC_ASSIGNMENT, 0, false);

		//if (identifiersEqual(&className, &parser->previous))
		//	error(parser, "A class can't inherit from itself");
	} else {
		String rootObjName = ELOX_STRING("Object");
		// this should newer fail
		suint16_t objNameConstant = builtinConstant(runCtx, &rootObjName);
		emitByte(cCtx, OP_GET_BUILTIN);
		emitUShort(cCtx, objNameConstant);
	}

	uint8_t numSuper = 1;

	if (consumeIfMatch(cCtx, TOKEN_IMPLEMENTS)) {
		do {
			expression(cCtx, PREC_ASSIGNMENT, 0, false);
			numSuper++;
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	}

	beginScope(cCtx);
	uint8_t handle;
	addLocal(cCtx, syntheticToken(U8("super")), &handle);
	defineVariable(cCtx, 0, VAR_LOCAL);

	emitBytes(cCtx, OP_PEEK, 1);
	emitBytes(cCtx, OP_INHERIT, numSuper);

	consume(cCtx, TOKEN_LEFT_BRACE, "Expect '{' before class body");
	while (!check(cCtx, TOKEN_RIGHT_BRACE) && !check(cCtx, TOKEN_EOF)) {
		if (consumeIfMatch(cCtx, TOKEN_LOCAL))
			emitField(cCtx);
		else if (consumeIfMatch(cCtx, TOKEN_CLASS))
			emitStaticClass(cCtx);
		else if (consumeIfMatch(cCtx, TOKEN_ABSTRACT))
			emitAbstractMethod(cCtx, 1); // skip over local super
		else
			emitMethod(cCtx, className, FTYPE_METHOD);
	}
	consume(cCtx, TOKEN_RIGHT_BRACE, "Expect '}' after class body");

	if (!klassCompiler.hasExplicitInitializer) {
		// emit implicit initializer
		FunctionCompiler localCompiler;
		// TODO: methodCompiler?
		FunctionCompiler *fc = initFunctionCompiler(cCtx, &localCompiler, NULL, FTYPE_INITIALIZER, className);
		ObjFunction *function = fc->function;
		// generate initializer code
		beginScope(cCtx);
		function->maxArgs = 0;
		emitLoadOrAssignVariable(cCtx, syntheticToken(U8("this")), false);
		emitLoadOrAssignVariable(cCtx, syntheticToken(U8("super")), false);
		emitByte(cCtx, OP_SUPER_INIT);
		emitBytes(cCtx, 0, false);
		emitByte(cCtx, OP_POP); // discard super init result
		function = endCompiler(cCtx);
		TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
		PUSH_TEMP(temps, protectedFunction, OBJ_VAL(function));
		suint16_t nameConstant = identifierConstant(cCtx, &function->name->string);
		if (ELOX_UNLIKELY(nameConstant < 0))
			errorAtCurrent(cCtx, "Out of memory");
		suint16_t functionConstant = makeConstant(cCtx, OBJ_VAL(function));
		if (ELOX_UNLIKELY(functionConstant < 0))
			errorAtCurrent(cCtx, "Out of memory");
		// we know it has upvalues
		emitByte(cCtx, OP_CLOSURE);
		emitUShort(cCtx, functionConstant);
		for (int i = 0; i < function->upvalueCount; i++) {
			emitByte(cCtx, localCompiler.upvalues[i].isLocal ? 1 : 0);
			emitByte(cCtx, localCompiler.upvalues[i].index);
		}
		emitByte(cCtx, OP_METHOD);
		emitUShort(cCtx, nameConstant);
		emitUShort(cCtx, functionConstant);
		emitUShort(cCtx, 0);
		releaseTemps(&temps);
	}

	emitByte(cCtx, OP_CLOSE_CLASS);

	endScope(cCtx);

	compilerState->currentKlass = currentKlass->enclosing;
}

static void classDeclaration(CCtx *cCtx, VarScope varType) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;
	Qualifiers *quals = &compilerState->currentFunctionCompiler->quals;

	uint16_t classGlobal = parseVariable(cCtx, varType, "Expect class name");
	Token className = parser->previous;
	suint16_t nameConstant = identifierConstant(cCtx, &parser->previous.string);
	CHECK_RAISE_PARSE_ERR_RET((nameConstant < 0), "Out of memory");

	bool abstract = (quals->attrs & QUAL_ABSTRACT) != 0;

	emitByte(cCtx, OP_CLASS);
	emitUShort(cCtx, nameConstant);
	emitByte(cCtx, abstract);

	defineVariable(cCtx, classGlobal, varType);

	beginScope(cCtx);
	_class(cCtx, &className);
	endScope(cCtx);
}

static ExpressionType anonClass(CCtx *cCtx, bool canAssign ELOX_UNUSED,
								bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Qualifiers *quals = &compilerState->currentFunctionCompiler->quals;

	static const String emptyStr = ELOX_STRING("");
	suint16_t nameConstant = identifierConstant(cCtx, &emptyStr);
	CHECK_RAISE_PARSE_ERR_RET_VAL((nameConstant < 0), "Out of memory", ETYPE_NORMAL);

	bool abstract = (quals->attrs & QUAL_ABSTRACT) != 0;

	resetQuals(cCtx);

	emitByte(cCtx, OP_CLASS);
	emitUShort(cCtx, nameConstant);
	emitByte(cCtx, abstract);

	beginScope(cCtx);
	_class(cCtx, NULL);
	endScope(cCtx);

	return ETYPE_NORMAL;
}

static ExpressionType abstract(CCtx *cCtx, bool canAssign, bool canExpand, bool firstExpansion) {
	if (!consumeIfMatch(cCtx, TOKEN_CLASS))
		errorAtCurrent(cCtx, "Expect 'class'");

	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Qualifiers *quals = &compilerState->currentFunctionCompiler->quals;
	quals->attrs |= QUAL_ABSTRACT;

	return anonClass(cCtx, canAssign, canExpand, firstExpansion);
}

static void functionDeclaration(CCtx *cCtx, VarScope varType) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	uint16_t nameGlobal = parseVariable(cCtx, varType, "Expect function name");
	markInitialized(current, varType);
	Prototype proto = { 0 };
	function(cCtx, FTYPE_FUNCTION, &proto, NULL);
	defineVariable(cCtx, nameGlobal, varType);
}

static void varDeclaration(CCtx *cCtx, VarScope varType) {
	uint16_t nameGlobal = parseVariable(cCtx, varType, "Expect variable name");

	if (consumeIfMatch(cCtx, TOKEN_EQUAL))
		expression(cCtx, PREC_ASSIGNMENT, 0, false);
	else
		emitByte(cCtx, OP_NIL);

	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after variable declaration");

	defineVariable(cCtx, nameGlobal, varType);
}

static void expressionStatement(CCtx *cCtx) {
	expression(cCtx, PREC_ASSIGNMENT, EXPR_ALLOW_TUPLE, false);
	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after expression");
	emitByte(cCtx, OP_POP);
}

static void unpackStatement(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	VarRef unpackVars[16];
	int numVars = 0;
	do {
		consume(cCtx, TOKEN_IDENTIFIER, "Identifier expected in unpack statement");
		unpackVars[numVars] = resolveVar(cCtx, parser->previous);
		numVars++;
	} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	consume(cCtx, TOKEN_COLON_EQUAL, "Expect ':=' after unpack values");

	expression(cCtx, PREC_ASSIGNMENT, 0, false);
	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after unpack statement");

	emitUnpack(cCtx, numVars, unpackVars);
}

static void breakStatement(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	if (compilerState->innermostLoop.start == -1)
		compileError(cCtx, "Cannot use 'break' outside of a loop");
	if (current->finallyDepth != compilerState->innermostLoop.finallyDepth)
		compileError(cCtx, "Cannot break outside a finally block");

	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after 'break'");

	// Discard any locals created inside the loop.
	int numLocals = 0;
	for (int i = current->localCount - 1;
		i >= 0 && current->locals[i].depth > compilerState->innermostLoop.scopeDepth;
		i--) {
			numLocals++;
	}
	emitPop(cCtx, numLocals);

	if (current->catchStackDepth > compilerState->innermostLoop.catchStackDepth) {
		emitByte(cCtx, OP_UNROLL_EXH);
		emitBytes(cCtx, compilerState->innermostLoop.catchStackDepth, 0);
	}
	// Jump to the end of the loop
	// This needs to be patched when loop block is exited
	int jmpOffset = emitJump(cCtx, OP_JUMP);

	// Record jump for later patching
	BreakJump *breakJump = ALLOCATE(cCtx->runCtx, BreakJump, 1);
	CHECK_RAISE_PARSE_ERR_RET((breakJump == NULL), "Out of memory");
	breakJump->scopeDepth = compilerState->innermostLoop.scopeDepth;
	breakJump->offset = jmpOffset;
	breakJump->next = compilerState->breakJumps;
	compilerState->breakJumps = breakJump;
}

static void continueStatement(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	if (compilerState->innermostLoop.start == -1)
		compileError(cCtx, "Can't use 'continue' outside of a loop");
	if (current->finallyDepth != compilerState->innermostLoop.finallyDepth)
		compileError(cCtx, "Cannot continue outside a finally block");

	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after 'continue'");

	// Discard any locals created inside the loop
	int numLocals = 0;
	for (int i = current->localCount - 1;
		 i >= 0 && current->locals[i].depth > compilerState->innermostLoop.scopeDepth;
		 i--) {
		numLocals++;
	}
	emitPop(cCtx, numLocals);

	if (current->catchStackDepth > compilerState->innermostLoop.catchStackDepth) {
		emitByte(cCtx, OP_UNROLL_EXH);
		emitBytes(cCtx, compilerState->innermostLoop.catchStackDepth, 0);
	}

	// Jump to top of current innermost loop
	emitLoop(cCtx, compilerState->innermostLoop.start);
}

static void forStatement(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	beginScope(cCtx);
	consume(cCtx, TOKEN_LEFT_PAREN, "Expect '(' after 'for'");

	if (consumeIfMatch(cCtx, TOKEN_SEMICOLON)) {
		// No initializer
	} else if (consumeIfMatch(cCtx, TOKEN_LOCAL))
		varDeclaration(cCtx, VAR_LOCAL);
	else
		expressionStatement(cCtx);

	LoopCtx surroundingLoop = compilerState->innermostLoop;
	compilerState->innermostLoop.start = currentChunk(current)->count;
	compilerState->innermostLoop.scopeDepth = current->scopeDepth;
	compilerState->innermostLoop.catchStackDepth = current->catchStackDepth;
	compilerState->innermostLoop.finallyDepth = current->finallyDepth;

	int exitJump = -1;
	if (!consumeIfMatch(cCtx, TOKEN_SEMICOLON)) {
		expression(cCtx, PREC_ASSIGNMENT, 0, false);
		consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after loop condition");

		// Jump out of the loop if the condition is false.
		exitJump = emitJump(cCtx, OP_JUMP_IF_FALSE);
		emitByte(cCtx, OP_POP); // Condition.
	}

	if (!consumeIfMatch(cCtx, TOKEN_RIGHT_PAREN)) {
		int bodyJump = emitJump(cCtx, OP_JUMP);
		int incrementStart = currentChunk(current)->count;
		expression(cCtx, PREC_ASSIGNMENT, 0, false);
		emitByte(cCtx, OP_POP);
		consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after for clauses");

		emitLoop(cCtx, compilerState->innermostLoop.start);
		compilerState->innermostLoop.start = incrementStart;
		patchJump(cCtx, bodyJump);
	}

	statement(cCtx);
	emitLoop(cCtx, compilerState->innermostLoop.start);

	if (exitJump != -1) {
		patchJump(cCtx, exitJump);
		emitByte(cCtx, OP_POP); // Condition.
	}

	patchBreakJumps(cCtx);

	compilerState->innermostLoop = surroundingLoop;

	endScope(cCtx);
}

static void forEachStatement(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;
	Parser *parser = &compilerState->parser;

	beginScope(cCtx);
	consume(cCtx, TOKEN_LEFT_PAREN, "Expect '(' after 'foreach'");

	VarRef foreachVars[16];
	int numVars = 0;
	do {
		if (consumeIfMatch(cCtx, TOKEN_LOCAL)) {
			consume(cCtx, TOKEN_IDENTIFIER, "Var name expected in foreach");
			uint8_t handle;
			addLocal(cCtx, parser->previous, &handle);
			defineVariable(cCtx, 0, VAR_LOCAL);
			emitByte(cCtx, OP_NIL);
		} else
			consume(cCtx, TOKEN_IDENTIFIER, "Var name expected in foreach");

		foreachVars[numVars] = resolveVar(cCtx, parser->previous);
		numVars++;
	} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	consume (cCtx, TOKEN_IN, "Expect 'in' after foreach variables");

	uint8_t hasNextSlot = 0;
	Local *hasNextVar = addLocal(cCtx, syntheticToken(U8("")), &hasNextSlot);
	emitByte(cCtx, OP_NIL);
	defineVariable(cCtx, 0, VAR_LOCAL);

	uint8_t nextSlot = 0;
	Local *nextVar = addLocal(cCtx, syntheticToken(U8("")), &nextSlot);
	emitByte(cCtx, OP_NIL);
	defineVariable(cCtx, 0, VAR_LOCAL);

	// iterator
	if (consumeIfMatch(cCtx, TOKEN_ELLIPSIS))
		emitByte(cCtx, OP_GET_VARARGS);
	else
		expression(cCtx, PREC_ASSIGNMENT, 0, false);
	consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after foreach iterator");

	emitByte(cCtx, OP_FOREACH_INIT);
	emitBytes(cCtx, hasNextSlot, hasNextVar->postArgs);
	emitBytes(cCtx, nextSlot, nextVar->postArgs);

	LoopCtx surroundingLoop = compilerState->innermostLoop;
	compilerState->innermostLoop.start = currentChunk(current)->count;
	compilerState->innermostLoop.scopeDepth = current->scopeDepth;
	compilerState->innermostLoop.catchStackDepth = current->catchStackDepth;
	compilerState->innermostLoop.finallyDepth = current->finallyDepth;

	emitBytes(cCtx, OP_GET_LOCAL, hasNextSlot);
	emitByte(cCtx, (uint8_t)hasNextVar->postArgs);
	emitByte(cCtx, OP_CALL);
	emitBytes(cCtx, 0, false);

	int exitJump = emitJump(cCtx, OP_JUMP_IF_FALSE);
	emitByte(cCtx, OP_POP); // condition

	emitBytes(cCtx, OP_GET_LOCAL, nextSlot);
	emitByte(cCtx, (uint8_t)nextVar->postArgs);
	emitByte(cCtx, OP_CALL);
	emitBytes(cCtx, 0, false);

	emitUnpack(cCtx, numVars, foreachVars);

	statement(cCtx);
	emitLoop(cCtx, compilerState->innermostLoop.start);

	patchJump(cCtx, exitJump);
	emitByte(cCtx, OP_POP); // condition

	patchBreakJumps(cCtx);

	compilerState->innermostLoop = surroundingLoop;

	endScope(cCtx);
}

typedef enum {
	LOOP_STYLE_NONE,
	LOOP_STYLE_FOR,
	LOOP_STYLE_FOREACH,
} LoopStyle;

static LoopStyle guessLoopStyle(CCtx *cCtx) {
	CCtxState state;

	LoopStyle ret = LOOP_STYLE_FOR;

	saveCCtxState(cCtx, &state);

	if (!consumeIfMatch(cCtx, TOKEN_LEFT_PAREN)) {
		errorAtCurrent(cCtx, "Expect '(' after 'for'");
		return LOOP_STYLE_NONE;
	}

	do {
		if (consumeIfMatch(cCtx, TOKEN_LOCAL)) {
			if (!consumeIfMatch(cCtx, TOKEN_IDENTIFIER))
				goto cleanup;
		} else {
			if (!consumeIfMatch(cCtx, TOKEN_IDENTIFIER))
				goto cleanup;
		}
	} while (consumeIfMatch(cCtx, TOKEN_COMMA));

	if (consumeIfMatch(cCtx, TOKEN_IN))
		ret = LOOP_STYLE_FOREACH;
cleanup:
	restoreCCtxState(cCtx, &state);
	return ret;
}

static void forLoopStatement(CCtx *cCtx) {
	LoopStyle loopStyle = guessLoopStyle(cCtx);

	switch (loopStyle) {
		case LOOP_STYLE_FOR:
			forStatement(cCtx);
			break;
		case LOOP_STYLE_FOREACH:
			forEachStatement(cCtx);
			break;
		default:
			break;
	}
}

static void ifStatement(CCtx *vmCtx) {
	consume(vmCtx, TOKEN_LEFT_PAREN, "Expect '(' after 'if'");
	expression(vmCtx, PREC_ASSIGNMENT, 0, false);
	consume(vmCtx, TOKEN_RIGHT_PAREN, "Expect ')' after condition");

	int thenJump = emitJump(vmCtx, OP_JUMP_IF_FALSE);
	emitByte(vmCtx, OP_POP);
	statement(vmCtx);

	int elseJump = emitJump(vmCtx, OP_JUMP);

	patchJump(vmCtx, thenJump);
	emitByte(vmCtx, OP_POP);

	if (consumeIfMatch(vmCtx, TOKEN_ELSE))
		statement(vmCtx);
	patchJump(vmCtx, elseJump);
}

static void returnStatement(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	if (current->type == FTYPE_SCRIPT)
		compileError(cCtx, "Can't return from top-level code");

	if (consumeIfMatch(cCtx, TOKEN_SEMICOLON)) {
		if (current->catchStackDepth > 0) {
			// return from inside try block
			emitByte(cCtx, OP_UNROLL_EXH);
			emitBytes(cCtx, 0, 0);
		}
		emitReturn(cCtx);
	} else {
		if (current->type == FTYPE_INITIALIZER)
			compileError(cCtx, "Can't return a value from an initializer");
		expression(cCtx, PREC_ASSIGNMENT, EXPR_ALLOW_TUPLE, false);
		if (current->catchStackDepth > 0) {
			// return from inside try block
			emitByte(cCtx, OP_UNROLL_EXH),
			emitBytes(cCtx, 0, 1);
		}
		consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after return value");
		emitByte(cCtx, OP_RETURN);
	}
}

static void whileStatement(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;

	LoopCtx surroundingLoop = compilerState->innermostLoop;
	compilerState->innermostLoop.start = currentChunk(current)->count;
	compilerState->innermostLoop.scopeDepth = current->scopeDepth;
	compilerState->innermostLoop.catchStackDepth = current->catchStackDepth;
	compilerState->innermostLoop.finallyDepth = current->finallyDepth;

	consume(cCtx, TOKEN_LEFT_PAREN, "Expect '(' after 'while'");
	expression(cCtx, PREC_ASSIGNMENT, 0, false);
	consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after condition");

	int exitJump = emitJump(cCtx, OP_JUMP_IF_FALSE);
	emitByte(cCtx, OP_POP);
	statement(cCtx);

	emitLoop(cCtx, compilerState->innermostLoop.start);

	patchJump(cCtx, exitJump);
	emitByte(cCtx, OP_POP);

	patchBreakJumps(cCtx);

	compilerState->innermostLoop = surroundingLoop;
}

static void throwStatement(CCtx *cCtx) {
	expression(cCtx, PREC_ASSIGNMENT, 0, false);
	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after value");
	emitByte(cCtx, OP_THROW);
}

typedef struct {
	uint16_t address;
	VarRef typeVar;
	int handlerJump;
} CatchHandler;

static void tryCatchStatement(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	FunctionCompiler *current = compilerState->currentFunctionCompiler;
	Parser *parser = &compilerState->parser;

	int currentCatchStack = current->catchStackDepth;
	current->catchStackDepth++;

	emitByte(cCtx, OP_PUSH_EXH);
	int handlerData = emitAddress(cCtx);

	statement(cCtx);

	emitByte(cCtx, OP_UNROLL_EXH);
	emitBytes(cCtx, currentCatchStack, 0);

	int successJump = emitJump(cCtx, OP_JUMP);

	CatchHandler handlers[32];
	int numCatchClauses = 0;
	uint16_t finallyAddress = 0;

	while (consumeIfMatch(cCtx, TOKEN_CATCH)) {
		beginScope(cCtx);
		consume(cCtx, TOKEN_LEFT_PAREN, "Expect '(' after catch");
		consume(cCtx, TOKEN_IDENTIFIER, "Expect type name to catch");
		Token typeName = parser->previous;

		handlers[numCatchClauses].typeVar = resolveVar(cCtx, typeName);
		handlers[numCatchClauses].address = currentChunk(current)->count;

		if (!consumeIfMatch(cCtx, TOKEN_RIGHT_PAREN)) {
			consume(cCtx, TOKEN_IDENTIFIER, "Expect identifier for exception instance");
			uint8_t handle;
			addLocal(cCtx, parser->previous, &handle);
			markInitialized(current, VAR_LOCAL);
			bool postArgs = false;
			uint8_t ex_var = resolveLocal(cCtx, current, &parser->previous, &postArgs);
			emitBytes(cCtx, OP_SET_LOCAL, ex_var);
			emitByte(cCtx, postArgs);
			consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after catch statement");
		} else {
			// still have to discard it from the stack
			// at the end of the handler
			uint8_t handle;
			addLocal(cCtx, syntheticToken(U8("")), &handle);
			markInitialized(current, VAR_LOCAL);
		}

		current->catchDepth++;
		statement(cCtx);
		current->catchDepth--;
		emitByte(cCtx, OP_UNROLL_EXH);
		emitBytes(cCtx, currentCatchStack, 0);

		endScope(cCtx);
		handlers[numCatchClauses].handlerJump = emitJump(cCtx, OP_JUMP);
		numCatchClauses++;
	}

	if (consumeIfMatch(cCtx, TOKEN_FINALLY)) {
		finallyAddress = currentChunk(current)->count;
		current->finallyDepth++;
		statement(cCtx);
		current->finallyDepth--;
		emitByte(cCtx, OP_END);
	}

	current->catchStackDepth--;

	// Catch table
	emitByte(cCtx, OP_DATA);
	patchAddress(current, handlerData);
	emitByte(cCtx, 2 + (6 * numCatchClauses));
	emitUShort(cCtx, finallyAddress);
	for (int i = 0; i < numCatchClauses; i++) {
		CatchHandler *handler = &handlers[i];
		emitByte(cCtx, handler->typeVar.scope);
		emitByte(cCtx, handler->typeVar.postArgs);
		emitUShort(cCtx, handler->typeVar.handle);
		emitUShort(cCtx, handler->address);
	}

	for (int i = 0; i < numCatchClauses; i++)
		patchJump(cCtx, handlers[i].handlerJump);
	patchJump(cCtx, successJump);
}

static void synchronize(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	parser->panicMode = false;

	while (parser->current.type != TOKEN_EOF) {
		if (parser->previous.type == TOKEN_SEMICOLON)
			return;
		switch (parser->current.type) {
			case TOKEN_BREAK:
			case TOKEN_INTERFACE:
			case TOKEN_CLASS:
			case TOKEN_CONTINUE:
			case TOKEN_FUNCTION:
			case TOKEN_GLOBAL:
			case TOKEN_LOCAL:
			case TOKEN_FOR:
			case TOKEN_IF:
			case TOKEN_WHILE:
			case TOKEN_RETURN:
			case TOKEN_THROW:
			case TOKEN_TRY:
			case TOKEN_IMPORT:
				return;
			default:
				; // Do nothing.
		}

		advance(cCtx);
	}
}

static void statement(CCtx *cCtx) {
	if (consumeIfMatch(cCtx, TOKEN_BREAK))
		breakStatement(cCtx);
	else if (consumeIfMatch(cCtx, TOKEN_CONTINUE))
		continueStatement(cCtx);
	else if (consumeIfMatch(cCtx, TOKEN_FOR))
		forLoopStatement(cCtx);
	else if (consumeIfMatch(cCtx, TOKEN_IF))
		ifStatement(cCtx);
	else if (consumeIfMatch(cCtx, TOKEN_RETURN))
		returnStatement(cCtx);
	else if (consumeIfMatch(cCtx, TOKEN_WHILE))
		whileStatement(cCtx);
	else if (consumeIfMatch(cCtx, TOKEN_LEFT_BRACE)) {
		beginScope(cCtx);
		block(cCtx);
		endScope(cCtx);
	} else if (consumeIfMatch(cCtx, TOKEN_THROW))
		throwStatement(cCtx);
	else if (consumeIfMatch(cCtx, TOKEN_TRY))
		tryCatchStatement(cCtx);
	else if (consumeIfMatch(cCtx, TOKEN_IMPORT))
		importStatement(cCtx, IMPORT_MODULE);
	else if (consumeIfMatch(cCtx, TOKEN_FROM))
		importStatement(cCtx, IMPORT_SYMBOLS);
	else if (check(cCtx, TOKEN_IDENTIFIER) &&
			 (checkNext(cCtx, TOKEN_COMMA) || checkNext(cCtx, TOKEN_COLON_EQUAL)))
		unpackStatement(cCtx);
	else
		expressionStatement(cCtx);
}

static void declaration(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;
	Qualifiers *quals = &compilerState->currentFunctionCompiler->quals;

	VarScope varType = parseQuals(cCtx);

	if (quals->pending & QUAL_PENDING_CLASS) {
		if (consumeIfMatch(cCtx, TOKEN_CLASS))
			classDeclaration(cCtx, varType);
		else if (consumeIfMatch(cCtx, TOKEN_INTERFACE))
			interfaceDeclaration(cCtx, varType);
		else
			errorAtCurrent(cCtx, "class or interface expected");
	} else {
		bool expectVar = quals->pending & QUAL_PENDING_SCOPE;

		if (consumeIfMatch(cCtx, TOKEN_CLASS))
			classDeclaration(cCtx, varType);
		else if (consumeIfMatch(cCtx, TOKEN_INTERFACE))
			interfaceDeclaration(cCtx, varType);
		else if (check(cCtx, TOKEN_FUNCTION) && (checkNext(cCtx, TOKEN_IDENTIFIER))) {
			consume(cCtx, TOKEN_FUNCTION, NULL);
			functionDeclaration(cCtx, true);
		} else if (expectVar)
			varDeclaration(cCtx, varType);
		else
			statement(cCtx);
	}

	resetQuals(cCtx);

	if (parser->panicMode)
		synchronize(cCtx);
}

ObjFunction *compile(RunCtx *runCtx, uint8_t *source, const String *fileName,
					 const String *moduleName) {
	ObjFunction *ret = NULL;
	CCtx cCtx;

	cCtx.compilerHandle = getCompiler(runCtx);
	if (ELOX_UNLIKELY(cCtx.compilerHandle == NULL)) {
		eloxPrintf(runCtx, ELOX_IO_ERR, "Compile error: Out of memory\n");
		goto cleanup;
	}

	FunctionCompiler functionCompiler;
	CompilerState *compilerState = &cCtx.compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	if (ELOX_UNLIKELY(!initCompilerContext(&cCtx, runCtx, fileName, moduleName))) {
		eloxPrintf(runCtx, ELOX_IO_ERR, "Compile error: Out of memory\n");
		goto cleanup;
	}

	initScanner(&cCtx, source);

	initFunctionCompiler(&cCtx, &functionCompiler, NULL, FTYPE_SCRIPT, &parser->previous);

	parser->hadError = false;
	parser->panicMode = false;
	parser->hasNext = false;

	advance(&cCtx);

	while (!consumeIfMatch(&cCtx, TOKEN_EOF))
		declaration(&cCtx);

	ObjFunction *function = endCompiler(&cCtx);

	ret = parser->hadError ? NULL : function;

cleanup:
	eloxReleaseHandle((EloxHandle *)cCtx.compilerHandle);

	return ret;
}

static ObjFunction *compileInlineFunction(CCtx *cCtx, FunctionType type, Token *nameToken,
										  MethodCompiler *mc, EloxError *error) {
	RunCtx *runCtx = cCtx->runCtx;
	ObjFiber *fiber = runCtx->activeFiber;
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;

	FunctionCompiler functionCompiler;

	FunctionCompiler *current = initFunctionCompiler(cCtx, &functionCompiler, mc, type, nameToken);
	ObjFunction *currentFunction = current->function;
	beginScope(cCtx);

	Prototype proto = { 0 };
	parsePrototype(cCtx, &proto, current);
	currentFunction->arity = proto.arity;
	if ((type == FTYPE_METHOD) || (type == FTYPE_DEFAULT)) {
		currentFunction->arity++; // for this
		currentFunction->isMethod = true;
	}
	current->hasVarargs = proto.hasVarargs;

	currentFunction->maxArgs = current->hasVarargs ? ELOX_MAX_ARGS : currentFunction->arity;
	current->postArgs = true;

	if (currentFunction->arity > 0) {
		currentFunction->defaultArgs = ALLOCATE(runCtx, Value, currentFunction->arity);
		ELOX_CHECK_RAISE_RET_VAL((currentFunction->defaultArgs != NULL), error, OOM(runCtx), NULL);
		memcpy(currentFunction->defaultArgs, current->defaultArgs,
			   currentFunction->arity * sizeof(Value));
	}

	uint8_t argCount = 0;
	bool hasExpansions = false;
	if (type == FTYPE_INITIALIZER) {
		emitLoadOrAssignVariable(cCtx, syntheticToken(U8("this")), false);
		compilerState->currentKlass->hasExplicitInitializer = true;
	}
	if (consumeIfMatch(cCtx, TOKEN_COLON)) {
		if (type != FTYPE_INITIALIZER)
			errorAtCurrent(cCtx, "Only initializers can be chained");
		consume(cCtx, TOKEN_SUPER, "Expect 'super'");
		consume(cCtx, TOKEN_LEFT_PAREN, "Expect super argument list");
		argCount = argumentList(cCtx, &hasExpansions);
	}
	if (type == FTYPE_INITIALIZER) {
		emitLoadOrAssignVariable(cCtx, syntheticToken(U8("super")), false);
		emitByte(cCtx, OP_SUPER_INIT);
		emitBytes(cCtx, argCount, hasExpansions);
		emitByte(cCtx, OP_POP); // discard super init result
	}

	consume(cCtx, TOKEN_LEFT_BRACE, "Expect '{' before function body");
	block(cCtx);

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	ObjFunction *ret = NULL;

	ObjFunction *function = endCompiler(cCtx);

	PUSH_TEMP(temps, protectedFunction, OBJ_VAL(function));
	if (function->upvalueCount > 0) {
		// we don't support closures in inline functions
		ELOX_RAISE_GOTO(error, RTERR(runCtx, "Inline functions cannot be closures"), cleanup)
	} else
		ret = function;

cleanup:
	releaseTemps(&temps);
	return ret;
}

Obj *compileFunction(RunCtx *runCtx, CCtx *cCtx, MethodCompiler *mc, ObjKlass *parentKlass,
					 uint8_t *source, EloxError *error) {
	ObjFiber *fiber = runCtx->activeFiber;
	FunctionCompiler functionCompiler;
	CompilerState *compilerState = &cCtx->compilerHandle->compilerState;
	Parser *parser = &compilerState->parser;

	Obj *ret = NULL;

	// duplicate input in case it is a string literal (scanner can modify input in place)
	size_t srcLen = strlen((const char *)source);
	uint8_t *srcCopy = ALLOCATE(runCtx, uint8_t, srcLen + 1);
	ELOX_CHECK_RAISE_RET_VAL((srcCopy != NULL), error, OOM(runCtx), NULL);
	memcpy(srcCopy, source, srcLen + 1);

	initScanner(cCtx, srcCopy);

	FunctionType type;
	if (parentKlass == NULL)
		type = FTYPE_LAMBDA;
	else {
		switch (parentKlass->obj.type) {
			case OBJ_CLASS:
				type = FTYPE_METHOD;
				break;
			case OBJ_INTERFACE:
				type = FTYPE_DEFAULT;
				break;
			default:
				ELOX_RAISE_GOTO(error, RTERR(runCtx, "Parent is not a class or interface"), cleanup);
		}
	}

	parser->hadError = false;
	parser->panicMode = false;
	parser->hasNext = false;

	advance(cCtx);

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);

	if (type == FTYPE_LAMBDA) {
		consume(cCtx, TOKEN_FUNCTION, "'function' expected");

		initFunctionCompiler(cCtx, &functionCompiler, NULL, type, &parser->previous);

		ret = (Obj *)function;
	} else {
		consume(cCtx, TOKEN_IDENTIFIER, "Method name expected");
		Token nameToken = parser->previous;

		ObjFunction *function = compileInlineFunction(cCtx, type, &nameToken, mc, error);
		if (ELOX_UNLIKELY(error->raised))
			goto cleanup;

		PUSH_TEMP(temps, protectedFunction, OBJ_VAL(function));

		if (type == FTYPE_METHOD) {
			ObjClass *parentClass = (ObjClass *)parentKlass;
			function->parentClass = parentClass;
			ObjClass *super = parentClass->super;
			function->refOffset = super->numRefs;

			ObjMethod *method = newMethod(runCtx, (ObjKlass *)parentClass, (Obj *)function);
			ELOX_CHECK_RAISE_GOTO((method != NULL), error, OOM(runCtx), cleanup);

			ret = (Obj *)method;
		} else {
			ObjDefaultMethod *method = newDefaultMethod(runCtx, function);
			ELOX_CHECK_RAISE_GOTO((method != NULL), error, OOM(runCtx), cleanup);

			ret = (Obj *)method;
		}
	}

cleanup:
	FREE(runCtx, uint8_t, srcCopy);
	releaseTemps(&temps);
	return ret;
}

void markCompilerHandle(EloxHandle *handle) {
	EloxCompilerHandle *hnd = (EloxCompilerHandle *)handle;
	CompilerState *compilerState = &hnd->compilerState;

	RunCtx *runCtx = hnd->base.runCtx;

	markObject(runCtx, (Obj *)compilerState->fileName);
	FunctionCompiler *compiler = compilerState->currentFunctionCompiler;
	while (compiler != NULL) {
		markObject(runCtx, (Obj *)compiler->function);
		for (int j = 0; j < compiler->numArgs; j++)
			markValue(runCtx, compiler->defaultArgs[j]);
		compiler = compiler->enclosing;
	}
}

