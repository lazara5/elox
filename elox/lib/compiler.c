// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/util.h"
#include "elox/compiler.h"
#include "elox/memory.h"
#include "elox/scanner.h"
#include "elox/state.h"
#include "elox/builtins.h"

#ifdef ELOX_DEBUG_PRINT_CODE
#include "elox/debug.h"
#endif

#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>

#pragma GCC diagnostic ignored "-Wswitch-enum"

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

void initCompilerContext(CCtx *cCtx, VMCtx *vmCtx, const String *moduleName) {
	cCtx->vmCtx = vmCtx;
	CompilerState *state = &cCtx->compilerState;
	state->current = NULL;
	state->currentClass = NULL;
	state->innermostLoopStart = -1;
	state->innermostLoopScopeDepth = 0;
	state->breakJumps = NULL;
	state->lambdaCount = 0;
	cCtx->moduleName = *moduleName;
}

static Chunk *currentChunk(Compiler *current) {
	return &current->function->chunk;
}

static void errorAt(CCtx *cCtx, Token *token, const char *message) {
	VMCtx *vmCtx = cCtx->vmCtx;
	Parser *parser = &cCtx->compilerState.parser;

	if (parser->panicMode)
		return;
	parser->panicMode = true;
	eloxPrintf(vmCtx, ELOX_IO_ERR, "[line %d] Error", token->line);

	if (token->type == TOKEN_EOF)
		eloxPrintf(vmCtx, ELOX_IO_ERR, " at end");
	else if (token->type == TOKEN_ERROR) {
		// Nothing.
	} else
		eloxPrintf(vmCtx, ELOX_IO_ERR, " at '%.*s'", token->string.length, token->string.chars);

	eloxPrintf(vmCtx, ELOX_IO_ERR, ": %s\n", message);
	parser->hadError = true;
}

static void error(CCtx *cCtx, const char *message) {
	Parser *parser = &cCtx->compilerState.parser;

	errorAt(cCtx, &parser->previous, message);
}

static void errorAtCurrent(CCtx *cCtx, const char *message) {
	Parser *parser = &cCtx->compilerState.parser;

	errorAt(cCtx, &parser->current, message);
}

static void advance(CCtx *cCtx) {
	Parser *parser = &cCtx->compilerState.parser;

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
}

static bool consume(CCtx *cCtx, TokenType type, const char *message) {
	Parser *parser = &cCtx->compilerState.parser;

	if (parser->current.type == type) {
		advance(cCtx);
		return true;
	}

	errorAtCurrent(cCtx, message);
	return false;
}

static bool check(CCtx *cCtx, TokenType type) {
	Parser *parser = &cCtx->compilerState.parser;

	return parser->current.type == type;
}

static bool checkNext(CCtx *cCtx, TokenType type) {
	Parser *parser = &cCtx->compilerState.parser;
	Scanner *scanner = &cCtx->scanner;

	if (isAtEnd(scanner))
		return false;

	if (!parser->hasNext) {
		parser->next = scanToken(cCtx);
		parser->hasNext = true;
	}

	return parser->next.type == type;
}

static TokenType getType(CCtx *cCtx) {
	Parser *parser = &cCtx->compilerState.parser;

	return parser->current.type;
}

static bool consumeIfMatch(CCtx *cCtx, TokenType type) {
	if (!check(cCtx, type))
		return false;
	advance(cCtx);
	return true;
}

static int emitByte(CCtx *cCtx, uint8_t byte) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;

	writeChunk(cCtx->vmCtx, currentChunk(current), &byte, 1, parser->previous.line);
	return currentChunk(current)->count - 1;
}

static void patchByte(Compiler *current, int where, uint8_t value) {
	currentChunk(current)->code[where] = value;
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
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;

	writeChunk(cCtx->vmCtx, currentChunk(current), (uint8_t *)&val, 2, parser->previous.line);
	return currentChunk(current)->count - 2;
}

static void patchUShort(CCtx *cCtx, uint16_t offset, uint16_t val) {
	Compiler *current = cCtx->compilerState.current;

	memcpy(currentChunk(current)->code + offset, &val, sizeof(uint16_t));
}

static void emitLoop(CCtx *cCtx, int loopStart) {
	Compiler *current = cCtx->compilerState.current;

	emitByte(cCtx, OP_LOOP);

	int offset = currentChunk(current)->count - loopStart + 2;
	if (offset > UINT16_MAX)
		error(cCtx, "Loop body too large");

	emitUShort(cCtx, (uint16_t)offset);
}

static int emitJump(CCtx *cCtx, uint8_t instruction) {
	Compiler *current = cCtx->compilerState.current;

	emitByte(cCtx, instruction);
	emitByte(cCtx, 0xff);
	emitByte(cCtx, 0xff);
	return currentChunk(current)->count - 2;
}

static int emitAddress(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;

	emitByte(cCtx, 0xff);
	emitByte(cCtx, 0xff);
	return currentChunk(current)->count - 2;
}

static void patchAddress(Compiler *current, uint16_t offset) {
	uint16_t address = currentChunk(current)->count;
	memcpy(currentChunk(current)->code + offset, &address, sizeof(uint16_t));
}

static void emitReturn(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;

	if (current->type == TYPE_INITIALIZER) {
		emitBytes(cCtx, OP_GET_LOCAL, 0);
		emitByte(cCtx, (uint8_t)false);
	} else
		emitByte(cCtx, OP_NIL);
	emitByte(cCtx, OP_RETURN);
}

static uint16_t makeConstant(CCtx *cCtx, Value value) {
	Compiler *current = cCtx->compilerState.current;

	int constant = addConstant(cCtx->vmCtx, currentChunk(current), value);
	if (constant > UINT16_MAX) {
		error(cCtx, "Too many constants in one chunk");
		return 0;
	}

	return (uint16_t)constant;
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
			if ((val >= 0) && (val <= UINT16_MAX)) {
				emitByte(cCtx, OP_IMMI);
				emitUShort(cCtx, val);
				return;
			}
		}
	}
	uint16_t constantIndex = makeConstant(cCtx, value);
	emitConstantOp(cCtx, constantIndex);
}

static void patchJump(CCtx *cCtx, int offset) {
	Compiler *current = cCtx->compilerState.current;

	// -2 to adjust for the bytecode for the jump offset itself
	int jump = currentChunk(current)->count - offset - 2;

	if (jump > UINT16_MAX)
		error(cCtx, "Too much code to jump over");

	uint16_t ushortJmp = (uint16_t)jump;
	memcpy(currentChunk(current)->code + offset, &ushortJmp, sizeof(uint16_t));
}

static void patchBreakJumps(CCtx *cCtx) {
	CompilerState *compilerState = &cCtx->compilerState;

	while (compilerState->breakJumps != NULL) {
		if (compilerState->breakJumps->scopeDepth >= compilerState->innermostLoopScopeDepth) {
			// Patch break jump
			patchJump(cCtx, compilerState->breakJumps->offset);

			BreakJump *temp = compilerState->breakJumps;
			compilerState->breakJumps = compilerState->breakJumps->next;
			FREE(cCtx->vmCtx, BreakJump, temp);
		} else
			break;
	}
}

static Compiler *initCompiler(CCtx *cCtx, Compiler *compiler, FunctionType type) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;
	VMCtx *vmCtx = cCtx->vmCtx;

	compiler->enclosing = current;
	compiler->function = NULL;
	compiler->type = type;
	compiler->localCount = 0;
	compiler->postArgs = false;
	compiler->hasVarargs = false;
	compiler->scopeDepth = 0;
	compiler->catchStackDepth = 0;
	compiler->numArgs = 0;
	compiler->function = newFunction(vmCtx);
	initTable(&compiler->stringConstants);

	cCtx->compilerState.current = current = compiler;
	if (type == TYPE_SCRIPT)
		current->function->name = NULL;
	else if (type == TYPE_LAMBDA) {
		uint8_t lambdaBuffer[64];
		int len = sprintf((char *)lambdaBuffer, "<lambda_%d>", cCtx->compilerState.lambdaCount++);
		current->function->name = copyString(vmCtx, lambdaBuffer, len);
	} else {
		current->function->name = copyString(vmCtx,
											 parser->previous.string.chars,
											 parser->previous.string.length);
	}

	Local *local = &current->locals[current->localCount++];
	local->depth = 0;
	local->isCaptured = false;
	local->postArgs = false;
	if (type != TYPE_FUNCTION) {
		local->name.string.chars = U8("this");
		local->name.string.length = 4;
	} else {
		local->name.string.chars = U8("");
		local->name.string.length = 0;
	}

	return current;
}

static ObjFunction *endCompiler(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;

	emitReturn(cCtx);
	ObjFunction* function = current->function;

#ifdef ELOX_DEBUG_PRINT_CODE
	Parser *parser = &cCtx->compilerState.parser;
	VMCtx *vmCtx = cCtx->vmCtx;
	if (!parser->hadError) {
		disassembleChunk(vmCtx, currentChunk(current),
						 function->name != NULL ? (const char *)function->name->string.chars : "<script>");
	}
#endif

	freeTable(cCtx->vmCtx, &current->stringConstants);

	cCtx->compilerState.current = current->enclosing;

	return function;
}

static void beginScope(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;
	current->scopeDepth++;
}

static void endScope(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;

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
static ParseRule *getRule(TokenType type);
static ExpressionType and_(CCtx *cCtx, bool canAssign, bool canExpand, bool firstExpansion);

static ExpressionType parsePrecedence(CCtx *cCtx, Precedence precedence,
									  bool canExpand, bool firstExpansion) {
	Parser *parser = &cCtx->compilerState.parser;

	advance(cCtx);
	ParseFn prefixRule = getRule(parser->previous.type)->prefix;
	if (prefixRule == NULL) {
		error(cCtx, "Expect expression");
		return ETYPE_NORMAL;
	}

	bool canAssign = (precedence <= PREC_ASSIGNMENT);
	ExpressionType type = prefixRule(cCtx, canAssign, canExpand, firstExpansion);
	if ((!canExpand) && (type == ETYPE_EXPAND))
		error(cCtx, "Expansion not allowed in this context");

	while (precedence <= getRule(parser->current.type)->precedence) {
		if (type == ETYPE_EXPAND)
			error(cCtx, "Expansions can only be used as stand-alone expressions");
		advance(cCtx);
		ParseFn infixRule = getRule(parser->previous.type)->infix;
		infixRule(cCtx, canAssign, canExpand, firstExpansion);
	}

	if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL))
		error(cCtx, "Invalid assignment target");

	return type;
}

static ExpressionType expression(CCtx *cCtx, bool canExpand, bool firstExpansion) {
	return parsePrecedence(cCtx, PREC_ASSIGNMENT, canExpand, firstExpansion);
}

static ExpressionType binary(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							 bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;

	TokenType operatorType = parser->previous.type;
	ParseRule *rule = getRule(operatorType);
	parsePrecedence(cCtx, (Precedence)(rule->precedence + 1), false, false);

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

static uint8_t argumentList(CCtx *cCtx, bool *hasExpansions) {
	uint8_t argCount = 0;
	*hasExpansions = false;
	if (!check(cCtx, TOKEN_RIGHT_PAREN)) {
		do {
			ExpressionType argType = expression(cCtx, true, !(*hasExpansions));
			if (argType == ETYPE_EXPAND) {
				*hasExpansions = true;
			} else {
				if (argCount == UINT8_MAX)
					error(cCtx, "Can't have more than 255 arguments");
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

uint16_t identifierConstant(CCtx *cCtx, Token *name) {
	Compiler *current = cCtx->compilerState.current;

	// See if we already have it.
	ObjString *string = copyString(cCtx->vmCtx, name->string.chars, name->string.length);
	Value indexValue;
	if (tableGet(&current->stringConstants, string, &indexValue)) {
		// We do.
		return (uint16_t)AS_NUMBER(indexValue);
	}

	uint16_t index = makeConstant(cCtx, OBJ_VAL(string));
	tableSet(cCtx->vmCtx, &current->stringConstants, string, NUMBER_VAL((double)index));
	return index;
}

uint16_t globalIdentifierConstant(VMCtx *vmCtx, const String *name, const String *moduleName) {
	VM *vm = &vmCtx->vm;

	// See if we already have it
	ObjStringPair *identifier = copyStrings(vmCtx,
											name->chars, name->length,
											moduleName->chars, moduleName->length);
	push(vm, OBJ_VAL(identifier));
	Value indexValue;
	Error error = ERROR_INITIALIZER(vmCtx);
	if (closeTableGet(&vm->globalNames, OBJ_VAL(identifier), &indexValue, &error)) {
		// We do
		pop(vm);
		return (uint16_t)AS_NUMBER(indexValue);
	}

	// TODO: error handling

	uint16_t newIndex = (uint16_t)vm->globalValues.count;
	valueArrayPush(vmCtx, &vm->globalValues, UNDEFINED_VAL);
	closeTableSet(&vm->globalNames, OBJ_VAL(identifier), NUMBER_VAL((double)newIndex), &error);
	pop(vm);

#ifdef ELOX_DEBUG_PRINT_CODE
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, ">>>Global[%5u] (%.*s:%.*s)\n", newIndex,
			   moduleName->length, moduleName->chars,
			   name->length, name->chars);
#endif

	return newIndex;
}

static uint16_t stringConstantId(CCtx *cCtx, ObjString *str) {
	Token nameToken = { .string.chars = str->string.chars, .string.length = str->string.length };
	return identifierConstant(cCtx, &nameToken);
}

#define MEMBER_FIELD_MASK  0x40000000
#define MEMBER_METHOD_MASK 0x80000000
#define MEMBER_ANY_MASK    0xC0000000

static int addPendingProperty(VMCtx *vmCtx, CompilerState *compiler, uint16_t nameHandle,
							  uint64_t mask, bool isThis) {
	Table *pendingThis = &compiler->currentClass->pendingThisProperties;
	Table *pendingSuper = &compiler->currentClass->pendingSuperProperties;
	int slot = pendingThis->count + pendingSuper->count;
	ObjString *name = AS_STRING(currentChunk(compiler->current)->constants.values[nameHandle]);
	Table *table = isThis ? pendingThis : pendingSuper;
	uint64_t actualSlot = AS_NUMBER(tableSetIfMissing(vmCtx, table, name, NUMBER_VAL(slot | mask)));
	actualSlot &= 0xFFFF;
	return actualSlot;
}

static ExpressionType colon(CCtx *cCtx, bool canAssign,
							bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;
	VMCtx *vmCtx = cCtx->vmCtx;

	bool isThisRef = (parser->beforePrevious.type == TOKEN_THIS);
	consume(cCtx, TOKEN_IDENTIFIER, "Expect property name after ':'");
	Token *propName = &parser->previous;
	uint16_t name = identifierConstant(cCtx, propName);

	if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL)) {
		expression(cCtx, false, false);
		if (isThisRef) {
			int propSlot = addPendingProperty(vmCtx, &cCtx->compilerState, name,
											  MEMBER_FIELD_MASK, true);
			emitByte(cCtx, OP_SET_MEMBER_PROP);
			emitUShort(cCtx, propSlot);
		} else {
			emitByte(cCtx, OP_SET_PROP);
			emitUShort(cCtx, name);
		}
	} else if (consumeIfMatch(cCtx, TOKEN_LEFT_PAREN)) {
		bool hasExpansions;
		uint8_t argCount = argumentList(cCtx, &hasExpansions);
		if (isThisRef) {
			int propSlot = addPendingProperty(vmCtx, &cCtx->compilerState, name,
											  MEMBER_ANY_MASK, true);
			emitByte(cCtx, OP_MEMBER_INVOKE);
			emitUShort(cCtx, propSlot);
			emitBytes(cCtx, argCount, hasExpansions);
		} else {
			emitByte(cCtx, OP_INVOKE);
			emitUShort(cCtx, name);
			emitBytes(cCtx, argCount, hasExpansions);
		}
	} else {
		if (isThisRef) {
			int propSlot = addPendingProperty(vmCtx, &cCtx->compilerState, name,
											  MEMBER_ANY_MASK, true);
			emitByte(cCtx, OP_GET_MEMBER_PROP);
			emitUShort(cCtx, propSlot);
		} else {
			emitByte(cCtx, OP_GET_PROP);
			emitUShort(cCtx, name);
		}
	}

	return ETYPE_NORMAL;
}

static ExpressionType dot(CCtx *cCtx, bool canAssign,
						  bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;

	consume(cCtx, TOKEN_IDENTIFIER, "Expect property name after '.'");
	Token *propName = &parser->previous;
	uint16_t name = identifierConstant(cCtx, propName);

	if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL)) {
		expression(cCtx, false, false);
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
	Parser *parser = &cCtx->compilerState.parser;

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
	expression(cCtx, false, false);
	consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after expression");
	return ETYPE_NORMAL;
}

static void parseArray(CCtx *cCtx, ObjType objType) {
	int itemCount = 0;
	if (!check(cCtx, TOKEN_RIGHT_BRACKET)) {
		do {
			if (check(cCtx, TOKEN_RIGHT_BRACKET)) {
				// Let's support a trailing comma
				break;
			}

			expression(cCtx, false, false);

			if (itemCount == UINT16_COUNT)
				error(cCtx, "Cannot have more than 16384 items in an array literal");
			itemCount++;
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	}

	consume(cCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after array literal");

	emitBytes(cCtx, OP_ARRAY_BUILD, objType);
	emitUShort(cCtx, (uint16_t)itemCount);
	return;
}

static ExpressionType array(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	parseArray(cCtx, OBJ_ARRAY);
	return ETYPE_NORMAL;
}

static ExpressionType tuple(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	consume(cCtx, TOKEN_LEFT_BRACKET, "");
	parseArray(cCtx, OBJ_TUPLE);
	return ETYPE_NORMAL;
}

static ExpressionType index_(CCtx *cCtx, bool canAssign,
							 bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	bool isSlice = false;

	if (consumeIfMatch(cCtx, TOKEN_DOT_DOT)) {
		emitByte(cCtx, OP_NIL);
		isSlice = true;
	} else
		expression(cCtx, false, false);

	if (isSlice || consumeIfMatch(cCtx, TOKEN_DOT_DOT)) {
		// slice
		if (consumeIfMatch(cCtx, TOKEN_RIGHT_BRACKET))
			emitByte(cCtx, OP_NIL);
		else {
			expression(cCtx, false, false);
			consume(cCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after slice");
		}
		emitByte(cCtx, OP_SLICE);
	} else {
		// index
		consume(cCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after index");

		if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL)) {
			expression(cCtx, false, false);
			emitByte(cCtx, OP_INDEX_STORE);
		} else
			emitByte(cCtx, OP_INDEX);
	}

	return ETYPE_NORMAL;
}

static ExpressionType map(CCtx *cCtx, bool canAssign ELOX_UNUSED,
						  bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;

	int itemCount = 0;

	if (!check(cCtx, TOKEN_RIGHT_BRACE)) {
		do {
			if (check(cCtx, TOKEN_RIGHT_BRACE)) {
				// Let's support a trailing comma
				break;
			}

			if (consumeIfMatch(cCtx, TOKEN_IDENTIFIER)) {
				uint16_t key = identifierConstant(cCtx, &parser->previous);
				emitConstantOp(cCtx, key);
			} else {
				consume(cCtx, TOKEN_LEFT_BRACKET, "Expecting identifier or index expression as key");
				expression(cCtx, false, false);
				consume(cCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after index");
			}
			consume(cCtx, TOKEN_EQUAL, "Expect '=' between key and value pair");
			expression(cCtx, false, false);

			if (itemCount == UINT16_COUNT)
				error(cCtx,  "No more than 65536 items allowed in a map constructor");
			itemCount++;
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	}
	consume(cCtx, TOKEN_RIGHT_BRACE, "Expect '}' after map elements");

	emitByte(cCtx, OP_MAP_BUILD);
	emitUShort(cCtx, itemCount);

	return ETYPE_NORMAL;
}

static bool identifiersEqual(Token *a, Token *b) {
	if (a->string.length != b->string.length)
		return false;
	return memcmp(a->string.chars, b->string.chars, a->string.length) == 0;
}

static Local *addLocal(CCtx *cCtx, Token name, uint8_t *handle) {
	Compiler *current = cCtx->compilerState.current;

	if (current->localCount == UINT8_COUNT) {
		error(cCtx, "Too many local variables in function");
		*handle = 0;
		return NULL;
	}

	Local *local = &current->locals[current->localCount++];
	local->name = name;
	local->depth = -1;
	local->postArgs = current->postArgs;
	local->isCaptured = false;

	*handle = current->localCount - 1;

	return local;
}

static int declareVariable(CCtx *cCtx, VarType varType) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;

	if (varType == VAR_GLOBAL)
		return 0;

	Token *name = &parser->previous;
	for (int i = current->localCount - 1; i >= 0; i--) {
		Local *local = &current->locals[i];
		if (local->depth != -1 && local->depth < current->scopeDepth)
			break;

		if (identifiersEqual(name, &local->name)) {
			error(cCtx, "Duplicated variable in this scope");
			return -1;
		}
	}

	uint8_t handle;
	addLocal(cCtx, *name, &handle);
	return handle;
}

static uint16_t parseVariable(CCtx *cCtx, VarType varType, const char *errorMessage) {
	Parser *parser = &cCtx->compilerState.parser;

	consume(cCtx, TOKEN_IDENTIFIER, errorMessage);

	declareVariable(cCtx, varType);
	if (varType == VAR_LOCAL)
		return 0;

	return globalIdentifierConstant(cCtx->vmCtx, &parser->previous.string, &cCtx->moduleName);
}

static void markInitialized(Compiler *current, VarType varType) {
	if (varType == VAR_GLOBAL)
		return;
	current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(CCtx *cCtx, uint16_t nameGlobal, VarType varType) {
	Compiler *current = cCtx->compilerState.current;

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

static int resolveLocal(CCtx *cCtx, Compiler *compiler, Token *name, bool *postArgs) {
	for (int i = compiler->localCount - 1; i >= 0; i--) {
		Local *local = &compiler->locals[i];
		if (identifiersEqual(name, &local->name)) {
			if (local->depth == -1)
				error(cCtx, "Can't read local variable in its own initializer");
			*postArgs = local->postArgs;
			return i;
		}
	}

	return -1;
}

static int addUpvalue(CCtx *cCtx, Compiler *compiler,
					  uint8_t index, bool postArgs, bool isLocal) {

	int upvalueCount = compiler->function->upvalueCount;

	for (int i = 0; i < upvalueCount; i++) {
		Upvalue *upvalue = &compiler->upvalues[i];
		if (upvalue->index == index && upvalue->isLocal == isLocal)
			return i;
	}

	if (upvalueCount == UINT8_COUNT) {
		error(cCtx, "Too many closure variables in function");
		return 0;
	}

	compiler->upvalues[upvalueCount].isLocal = isLocal;
	compiler->upvalues[upvalueCount].index = index;
	compiler->upvalues[upvalueCount].postArgs = postArgs;
	return compiler->function->upvalueCount++;
}

static int resolveUpvalue(CCtx *cCtx, Compiler *compiler, Token *name) {
	if (compiler->enclosing == NULL)
		return -1;

	bool postArgs;
	int local = resolveLocal(cCtx, compiler->enclosing, name, &postArgs);
	if (local != -1) {
			compiler->enclosing->locals[local].isCaptured = true;
			return addUpvalue(cCtx, compiler, (uint8_t)local, postArgs, true);
	}

	int upvalue = resolveUpvalue(cCtx, compiler->enclosing, name);
	if (upvalue != -1)
		return addUpvalue(cCtx, compiler, (uint8_t)upvalue, false, false);

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
	expression(cCtx, false, false);
	emitByte(cCtx, op);
	emitStore(cCtx, arg, setOp);
}

static void emitLoadOrAssignVariable(CCtx *cCtx, Token name, bool canAssign) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;
	VM *vm = &cCtx->vmCtx->vm;

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
		if (moduleName == NULL) {
			uint32_t nameHash = hashString(name.string.chars, name.string.length);
			bool isBuiltin = tableFindString(&vm->builtinSymbols,
											 name.string.chars, name.string.length, nameHash);
			moduleName = isBuiltin ? &eloxBuiltinModule : &cCtx->moduleName;
		}
		arg.handle = globalIdentifierConstant(cCtx->vmCtx, varName, moduleName);
		getOp = OP_GET_GLOBAL;
		setOp = OP_SET_GLOBAL;
		arg.isShort = true;
	}

	if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL)) {
		expression(cCtx, false, false);
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
	Parser *parser = &cCtx->compilerState.parser;
	VMCtx *vmCtx = cCtx->vmCtx;

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
		return OBJ_VAL(copyString(vmCtx,
								  parser->previous.string.chars + 1,
								  parser->previous.string.length - 2));
	} else
		errorAtCurrent(cCtx, "Constant expected");

	return NIL_VAL;
}

static void function(CCtx *cCtx, FunctionType type) {
	VMCtx *vmCtx = cCtx->vmCtx;

	Compiler compiler;
	Compiler *current = initCompiler(cCtx, &compiler, type);
	ObjFunction *currentFunction = current->function;
	beginScope(cCtx);

	consume(cCtx, TOKEN_LEFT_PAREN, "Expect '(' after function name");
	if (!check(cCtx, TOKEN_RIGHT_PAREN)) {
		do {
			currentFunction->arity++;
			if (currentFunction->arity > 255)
				errorAtCurrent(cCtx, "Can't have more than 255 parameters");
			if (consumeIfMatch(cCtx, TOKEN_ELLIPSIS)) {
				currentFunction->arity--;
				current->hasVarargs = true;
				if (!check(cCtx, TOKEN_RIGHT_PAREN))
					errorAtCurrent(cCtx, "Expected ) after ...");
			} else {
				uint16_t constant = parseVariable(cCtx, VAR_LOCAL, "Expect parameter name");
				defineVariable(cCtx, constant, VAR_LOCAL);
				if (consumeIfMatch(cCtx, TOKEN_EQUAL))
					current->defaultArgs[current->numArgs++] = parseConstant(cCtx);
				else
					current->defaultArgs[current->numArgs++] = NIL_VAL;
			}
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	}
	consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after parameters");
	currentFunction->maxArgs = current->hasVarargs ? 255 : currentFunction->arity;
	current->postArgs = true;

	if (currentFunction->arity > 0) {
		currentFunction->defaultArgs = ALLOCATE(vmCtx, Value, currentFunction->arity);
		memcpy(currentFunction->defaultArgs, current->defaultArgs,
			   currentFunction->arity * sizeof(Value));
	}

	uint8_t argCount = 0;
	bool hasExpansions = false;
	if (type == TYPE_INITIALIZER)
		emitLoadOrAssignVariable(cCtx, syntheticToken(U8("this")), false); // TODO: ???
	if (consumeIfMatch(cCtx, TOKEN_COLON)) {
		if (type != TYPE_INITIALIZER)
			errorAtCurrent(cCtx, "Only initializers can be chained");
		consume(cCtx, TOKEN_SUPER, "Expect 'super'");
		consume(cCtx, TOKEN_LEFT_PAREN, "Expect super argument list");
		argCount = argumentList(cCtx, &hasExpansions);
	}
	if (type == TYPE_INITIALIZER) {
		emitLoadOrAssignVariable(cCtx, syntheticToken(U8("super")), false);
		emitByte(cCtx, OP_SUPER_INIT);
		emitBytes(cCtx, argCount, hasExpansions);
	}

	consume(cCtx, TOKEN_LEFT_BRACE, "Expect '{' before function body");
	block(cCtx);

	ObjFunction *function = endCompiler(cCtx);
	uint16_t functionConstant = makeConstant(cCtx, OBJ_VAL(function));
	if (function->upvalueCount > 0) {
		emitByte(cCtx, OP_CLOSURE);
		emitUShort(cCtx, functionConstant);

		// Emit arguments for each upvalue to know whether to capture a local or an upvalue
		for (int i = 0; i < function->upvalueCount; i++) {
			emitByte(cCtx, compiler.upvalues[i].isLocal ? 1 : 0);
			emitByte(cCtx, compiler.upvalues[i].index);
		}
	} else {
		// No need to create a closure
		emitConstantOp(cCtx, functionConstant);
	}
}

static int declareLocal(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;

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
	VMCtx *vmCtx = cCtx->vmCtx;
	Parser *parser = &cCtx->compilerState.parser;

	emitByte(cCtx, OP_IMPORT);

	consume(cCtx, TOKEN_IDENTIFIER, "Expect module name");
	String moduleName = parser->previous.string;
	uint16_t moduleConstant = identifierConstant(cCtx, &parser->previous);
	emitUShort(cCtx, moduleConstant);
	int symOffset = emitUShort(cCtx, 0);

	if (importType == IMPORT_SYMBOLS) {
		consume(cCtx, TOKEN_IMPORT, "Expect 'import'");
		uint16_t numSymbols = 0;
		do {
			consume(cCtx, TOKEN_IDENTIFIER, "Expect symbol name");
			declareLocal(cCtx);
			uint16_t symbol = globalIdentifierConstant(vmCtx, &parser->previous.string, &moduleName);
			emitUShort(cCtx, symbol);
			numSymbols++;
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));

		patchUShort(cCtx, symOffset, numSymbols);
	}

	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after import");
}

static ExpressionType lambda(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							 bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	function(cCtx, TYPE_LAMBDA);
	return ETYPE_NORMAL;
}

static ExpressionType number(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							 bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;
	double value = strtod((const char *)parser->previous.string.chars, NULL);
	emitConstant(cCtx, NUMBER_VAL(value));
	return ETYPE_NORMAL;
}

static ExpressionType or_(CCtx *cCtx, bool canAssign ELOX_UNUSED,
						  bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	int elseJump = emitJump(cCtx, OP_JUMP_IF_FALSE);
	int endJump = emitJump(cCtx, OP_JUMP);

	patchJump(cCtx, elseJump);
	emitByte(cCtx, OP_POP);

	parsePrecedence(cCtx, PREC_OR, false, false);
	patchJump(cCtx, endJump);
	return ETYPE_NORMAL;
}

static ExpressionType string(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							 bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	VMCtx *vmCtx = cCtx->vmCtx;
	Parser *parser = &cCtx->compilerState.parser;

	if (!check(cCtx, TOKEN_STRING)) {
		emitConstant(cCtx, OBJ_VAL(copyString(vmCtx,
											  parser->previous.string.chars + 1,
											  parser->previous.string.length - 2)));
	} else {
		HeapCString str;
		initHeapStringWithSize(vmCtx, &str, parser->previous.string.length);
		heapStringAddString(vmCtx, &str,
							parser->previous.string.chars + 1,
							parser->previous.string.length - 2);
		while (consumeIfMatch(cCtx, TOKEN_STRING)) {
			heapStringAddString(vmCtx, &str,
								parser->previous.string.chars + 1,
								parser->previous.string.length - 2);
		}
		emitConstant(cCtx, OBJ_VAL(takeString(vmCtx, str.chars, str.length, str.capacity)));
	}

	return ETYPE_NORMAL;
}

typedef struct {
	VarType type;
	uint16_t handle; //slot for local or upvalue, name index for global
	bool postArgs; // only for locals
} VarRef;

static VarRef resolveVar(CCtx *cCtx, Token name) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;
	VM *vm = &cCtx->vmCtx->vm;

	bool postArgs;
	int slot = resolveLocal(cCtx, current, &name, &postArgs);
	if (slot >= 0)
		return (VarRef){ .type = VAR_LOCAL, .handle = slot, .postArgs = postArgs };
	else if ((slot = resolveUpvalue(cCtx, current, &name)) >= 0)
		return (VarRef){ .type = VAR_UPVALUE, .handle = slot };

	const String *symbolName = &name.string;
	const String *moduleName = NULL;
	if (consumeIfMatch(cCtx, TOKEN_DOUBLE_COLON)) {
		consume(cCtx, TOKEN_IDENTIFIER, "Var name expected after ::");
		moduleName = &name.string;
		symbolName = &parser->previous.string;
	}
	if (moduleName == NULL) {
		uint32_t nameHash = hashString(name.string.chars, name.string.length);
		bool isBuiltin = tableFindString(&vm->builtinSymbols,
										 name.string.chars, name.string.length, nameHash);
		moduleName = isBuiltin ? &eloxBuiltinModule : &cCtx->moduleName;
	}

	return (VarRef){ .type = VAR_GLOBAL,
					 .handle = globalIdentifierConstant(cCtx->vmCtx, symbolName, moduleName) };
}

static void emitUnpack(CCtx *cCtx, uint8_t numVal, VarRef *slots) {
	emitByte(cCtx, OP_UNPACK);
	emitByte(cCtx, numVal);
	for (int i = 0; i < numVal; i++) {
		emitByte(cCtx, slots[i].type);
		switch(slots[i].type) {
			case VAR_LOCAL:
				emitBytes(cCtx, slots[i].handle, slots[i].postArgs);
				break;
			case VAR_GLOBAL:
				emitUShort(cCtx, slots[i].handle);
				break;
			case VAR_UPVALUE:
				emitByte(cCtx, slots[i].handle);
		}
	}
}

static ExpressionType variable(CCtx *cCtx, bool canAssign,
							   bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;
	emitLoadOrAssignVariable(cCtx, parser->previous, canAssign);
	return ETYPE_NORMAL;
}

static String ellipsisLength = STRING_INITIALIZER("length");

static ExpressionType ellipsis(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							   bool canExpand, bool firstExpansion) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;

	if (!current->hasVarargs) {
		errorAtCurrent(cCtx, "Function does not have varargs");
		return ETYPE_NORMAL;
	}

	ExpressionType eType = ETYPE_NORMAL;

	if (consumeIfMatch(cCtx, TOKEN_LEFT_BRACKET)) {
		expression(cCtx, false, false);
		consume(cCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after index");

		if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL)) {
			expression(cCtx, false, false);
			emitByte(cCtx, OP_SET_VARARG);
		} else
			emitByte(cCtx, OP_GET_VARARG);
	} else if (consumeIfMatch(cCtx, TOKEN_COLON)) {
		consume(cCtx, TOKEN_IDENTIFIER, "Expect property name after ':'");
		Token *propName = &parser->previous;
		if (stringEquals(&propName->string, &ellipsisLength)) {
			consume(cCtx, TOKEN_LEFT_PAREN, "Expect '(' after function name");
			consume(cCtx, TOKEN_RIGHT_PAREN, "Function takes no arguments");
			emitByte(cCtx, OP_NUM_VARARGS);
		} else
			errorAtCurrent(cCtx, "Unknown property name for ...");
	} else {
		if (!canExpand)
			errorAtCurrent(cCtx, "... used in a context that doesn't allow expansion");
		emitBytes(cCtx, OP_EXPAND_VARARGS, firstExpansion);
		eType = ETYPE_EXPAND;
	}

	return eType;
}

static ExpressionType expand(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							 bool canExpand, bool firstExpansion) {
	if (!canExpand)
		errorAtCurrent(cCtx, ".. used in a context that doesn't allow expansion");

	expression(cCtx, false, false);
	if (!firstExpansion)
		emitByte(cCtx, OP_SWAP);
	emitBytes(cCtx, OP_EXPAND, firstExpansion);

	return ETYPE_EXPAND;
}

static ExpressionType this_(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	ClassCompiler *currentClass = cCtx->compilerState.currentClass;

	if (currentClass == NULL) {
		error(cCtx, "Can't use 'this' outside of a class");
		return ETYPE_NORMAL;
	}

	variable(cCtx, false, false, false);
	return ETYPE_NORMAL;
}

static ExpressionType super_(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							 bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;
	ClassCompiler *currentClass = cCtx->compilerState.currentClass;
	VMCtx *vmCtx = cCtx->vmCtx;

	if (currentClass == NULL)
		error(cCtx, "Can't use 'super' outside of a class");

	consume(cCtx, TOKEN_COLON, "Expect ':' after 'super'");
	consume(cCtx, TOKEN_IDENTIFIER, "Expect superclass method name");
	uint16_t name = identifierConstant(cCtx, &parser->previous);

	emitLoadOrAssignVariable(cCtx, syntheticToken(U8("this")), false);
	if (consumeIfMatch(cCtx, TOKEN_LEFT_PAREN)) {
		bool hasExpansions;
		uint8_t argCount = argumentList(cCtx, &hasExpansions);
		int propSlot = addPendingProperty(vmCtx, &cCtx->compilerState, name,
										  MEMBER_METHOD_MASK, false);
		emitByte(cCtx, OP_SUPER_INVOKE);
		emitUShort(cCtx, propSlot);
		emitBytes(cCtx, argCount, hasExpansions);
	} else {
		int propSlot = addPendingProperty(vmCtx, &cCtx->compilerState, name,
										  MEMBER_METHOD_MASK, false);
		emitByte(cCtx, OP_GET_SUPER);
		emitUShort(cCtx, propSlot);
	}

	return ETYPE_NORMAL;
}

static ExpressionType unary(CCtx *cCtx, bool canAssign ELOX_UNUSED,
							bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;
	TokenType operatorType = parser->previous.type;

	// Compile the operand.
	parsePrecedence(cCtx, PREC_UNARY, false, false);

	// Emit the operator instruction.
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

static ExpressionType anonClass(CCtx *cCtx, bool canAssign, bool canExpand, bool firstExpansion);

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
	[TOKEN_COLON]         = {tuple,     colon,  PREC_CALL},
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
	[TOKEN_AND]           = {NULL,      and_,   PREC_AND},
	[TOKEN_BREAK]         = {NULL,      NULL,   PREC_NONE},
	[TOKEN_CATCH]         = {NULL,      NULL,   PREC_NONE},
	[TOKEN_CLASS]         = {anonClass, NULL,   PREC_NONE},
	[TOKEN_CONTINUE]      = {NULL,      NULL,   PREC_NONE},
	[TOKEN_ELSE]          = {NULL,      NULL,   PREC_NONE},
	[TOKEN_FALSE]         = {literal,   NULL,   PREC_NONE},
	[TOKEN_FOR]           = {NULL,      NULL,   PREC_NONE},
	[TOKEN_FOREACH]       = {NULL,      NULL,   PREC_NONE},
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
	parsePrecedence(cCtx, PREC_AND, false, false);

	patchJump(cCtx, endJump);
	return ETYPE_NORMAL;
}

static ParseRule *getRule(TokenType type) {
	return &parseRules[type];
}

static void emitField(CCtx *cCtx) {
	Parser *parser = &cCtx->compilerState.parser;

	consume(cCtx, TOKEN_IDENTIFIER, "Expect field name");
	uint16_t constant = identifierConstant(cCtx, &parser->previous);
	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after field declaration");

	emitByte(cCtx, OP_FIELD);
	emitUShort(cCtx, constant);
}

static void emitMethod(CCtx *cCtx, Token *className) {
	Parser *parser = &cCtx->compilerState.parser;

	uint16_t nameConstant = 0;
	bool anonInit = false;
	if (check(cCtx, TOKEN_LEFT_PAREN)) {
		Token initToken = syntheticToken(U8("$init"));
		nameConstant = identifierConstant(cCtx, &initToken);
		anonInit = true;
	} else {
		consume(cCtx, TOKEN_IDENTIFIER, "Expect method name");
		nameConstant = identifierConstant(cCtx, &parser->previous);
	}
	FunctionType type = TYPE_METHOD;

	if (className != NULL) {
		if (parser->previous.string.length == className->string.length &&
			memcmp(parser->previous.string.chars, className->string.chars, className->string.length) == 0) {
			type = TYPE_INITIALIZER;
		}
	} else if (anonInit)
		type = TYPE_INITIALIZER;

	function(cCtx, type);
	emitByte(cCtx, OP_METHOD);
	emitUShort(cCtx, nameConstant);
}

static void _class(CCtx *cCtx, Token *className);

static void emitStaticClass(CCtx *cCtx) {
	Parser *parser = &cCtx->compilerState.parser;

	consume(cCtx, TOKEN_IDENTIFIER, "Expect class name");
	Token className = parser->previous;
	uint16_t nameConstant = identifierConstant(cCtx, &className);

	beginScope(cCtx); // for temp class local
	uint8_t localHandle;
	Local *local = addLocal(cCtx, syntheticToken(U8("")), &localHandle);
	if (local != NULL) {
		emitByte(cCtx, OP_CLASS);
		emitUShort(cCtx, nameConstant);
		defineVariable(cCtx, 0, VAR_LOCAL);
		_class(cCtx, &className);
		emitByte(cCtx, OP_STATIC);
		emitUShort(cCtx, nameConstant);
	}
	endScope(cCtx);
}

static uint8_t getSlotType(uint32_t slot, bool isSuper) {
	uint32_t memberType = (slot & MEMBER_ANY_MASK) >> 30;
	return (uint8_t)isSuper | memberType << 1;
}

typedef struct {
	Token *className;
	uint8_t localSlot;
} ClassContext;

static void _class(CCtx *cCtx, Token *className) {
	ClassCompiler *currentClass = cCtx->compilerState.currentClass;
	VMCtx *vmCtx = cCtx->vmCtx;

	ClassCompiler classCompiler;
	initTable(&classCompiler.pendingThisProperties);
	initTable(&classCompiler.pendingSuperProperties);
	classCompiler.enclosing = currentClass;
	cCtx->compilerState.currentClass = currentClass = &classCompiler;

	if (consumeIfMatch(cCtx, TOKEN_COLON)) {
		expression(cCtx, false, false);

		//if (identifiersEqual(&className, &parser->previous))
		//	error(parser, "A class can't inherit from itself");
	} else {
		String rootObjName = STRING_INITIALIZER("Object");
		uint16_t objNameConstant = globalIdentifierConstant(vmCtx, &rootObjName, &eloxBuiltinModule);
		emitByte(cCtx, OP_GET_GLOBAL);
		emitUShort(cCtx, objNameConstant);
	}

	beginScope(cCtx);
	uint8_t handle;
	addLocal(cCtx, syntheticToken(U8("super")), &handle);
	defineVariable(cCtx, 0, VAR_LOCAL);

	emitBytes(cCtx, OP_PEEK, 1);
	emitByte(cCtx, OP_INHERIT);

	consume(cCtx, TOKEN_LEFT_BRACE, "Expect '{' before class body");
	while (!check(cCtx, TOKEN_RIGHT_BRACE) && !check(cCtx, TOKEN_EOF)) {
		if (consumeIfMatch(cCtx, TOKEN_LOCAL))
			emitField(cCtx);
		else if (consumeIfMatch(cCtx, TOKEN_CLASS))
			emitStaticClass(cCtx);
		else
			emitMethod(cCtx, className);
	}
	consume(cCtx, TOKEN_RIGHT_BRACE, "Expect '}' after class body");

	Table *pendingThis = &classCompiler.pendingThisProperties;
	Table *pendingSuper = &classCompiler.pendingSuperProperties;
	if (pendingThis->count + pendingSuper->count > 0) {
		if (pendingThis->count + pendingSuper->count > UINT16_MAX)
			error(cCtx, "Can't have more than 65535 this/super references in a method");
		else {
			emitByte(cCtx, OP_RESOLVE_MEMBERS);
			emitUShort(cCtx, pendingThis->count + pendingSuper->count);
			for (int i = 0; i < pendingThis->capacity; i++) {
				Entry *entry = &pendingThis->entries[i];
				if (entry->key != NULL) {
					uint32_t slot = AS_NUMBER(entry->value);
					emitByte(cCtx, getSlotType(slot, false));
					emitUShort(cCtx, stringConstantId(cCtx, entry->key));
					emitUShort(cCtx, slot & 0xFFFF);
				}
			}
			for (int i = 0; i < pendingSuper->capacity; i++) {
				Entry *entry = &pendingSuper->entries[i];
				if (entry->key != NULL) {
					uint32_t slot = AS_NUMBER(entry->value);
					emitByte(cCtx, getSlotType(slot, true));
					emitUShort(cCtx, stringConstantId(cCtx, entry->key));
					emitUShort(cCtx, slot & 0xFFFF);
				}
			}
		}
	}

	freeTable(vmCtx, pendingThis);
	freeTable(vmCtx, pendingSuper);

	endScope(cCtx);

	cCtx->compilerState.currentClass = currentClass->enclosing;
}

static void classDeclaration(CCtx *cCtx, VarType varType) {
	Parser *parser = &cCtx->compilerState.parser;

	uint16_t classGlobal = parseVariable(cCtx, varType, "Expect class name");
	Token className = parser->previous;
	uint16_t nameConstant = identifierConstant(cCtx, &parser->previous);

	emitByte(cCtx, OP_CLASS);
	emitUShort(cCtx, nameConstant);

	defineVariable(cCtx, classGlobal, varType);

	beginScope(cCtx);
	_class(cCtx, &className);
	endScope(cCtx);
}

static ExpressionType anonClass(CCtx *cCtx, bool canAssign ELOX_UNUSED,
								bool canExpand ELOX_UNUSED, bool firstExpansion ELOX_UNUSED) {
	emitByte(cCtx, OP_ANON_CLASS);
	beginScope(cCtx);
	_class(cCtx, NULL);
	endScope(cCtx);
	return ETYPE_NORMAL;
}

static void functionDeclaration(CCtx *cCtx, VarType varType) {
	Compiler *current = cCtx->compilerState.current;

	uint16_t nameGlobal = parseVariable(cCtx, varType, "Expect function name");
	markInitialized(current, varType);
	function(cCtx, TYPE_FUNCTION);
	defineVariable(cCtx, nameGlobal, varType);
}

static void varDeclaration(CCtx *cCtx, VarType varType) {
	uint16_t nameGlobal = parseVariable(cCtx, varType, "Expect variable name");

	if (consumeIfMatch(cCtx, TOKEN_EQUAL))
		expression(cCtx, false, false);
	else
		emitByte(cCtx, OP_NIL);

	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after variable declaration");

	defineVariable(cCtx, nameGlobal, varType);
}

static void expressionStatement(CCtx *cCtx) {
	expression(cCtx, false, false);
	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after expression");
	emitByte(cCtx, OP_POP);
}

static void unpackStatement(CCtx *cCtx) {
	Parser *parser = &cCtx->compilerState.parser;

	VarRef unpackVars[16];
	int numVars = 0;
	do {
		consume(cCtx, TOKEN_IDENTIFIER, "Identifier expected in unpack statement");
		unpackVars[numVars] = resolveVar(cCtx, parser->previous);
		numVars++;
	} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	consume(cCtx, TOKEN_COLON_EQUAL, "Expect ':=' after unpack values");

	expression(cCtx, false, false);
	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after unpack statement");

	emitUnpack(cCtx, numVars, unpackVars);
}

static void breakStatement(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;
	CompilerState *compilerState = &cCtx->compilerState;

	if (compilerState->innermostLoopStart == -1)
		error(cCtx, "Cannot use 'break' outside of a loop");

	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after 'break'");

	// Discard any locals created inside the loop.
	int numLocals = 0;
	for (int i = current->localCount - 1;
		i >= 0 && current->locals[i].depth > compilerState->innermostLoopScopeDepth;
		i--) {
			numLocals++;
	}
	emitPop(cCtx, numLocals);

	// Jump to the end of the loop
	// This needs to be patched when loop block is exited
	int jmpOffset = emitJump(cCtx, OP_JUMP);

	// Record jump for later patching
	BreakJump *breakJump = ALLOCATE(cCtx->vmCtx, BreakJump, 1);
	breakJump->scopeDepth = compilerState->innermostLoopScopeDepth;
	breakJump->offset = jmpOffset;
	breakJump->next = compilerState->breakJumps;
	compilerState->breakJumps = breakJump;
}

static void continueStatement(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;
	CompilerState *compilerState = &cCtx->compilerState;

	if (compilerState->innermostLoopStart == -1)
		error(cCtx, "Can't use 'continue' outside of a loop");

	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after 'continue'");

	// Discard any locals created inside the loop
	int numLocals = 0;
	for (int i = current->localCount - 1;
		 i >= 0 && current->locals[i].depth > compilerState->innermostLoopScopeDepth;
		 i--) {
		numLocals++;
	}
	emitPop(cCtx, numLocals);

	// Jump to top of current innermost loop
	emitLoop(cCtx, compilerState->innermostLoopStart);
}

static void forStatement(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;
	CompilerState *compilerState = &cCtx->compilerState;

	beginScope(cCtx);
	consume(cCtx, TOKEN_LEFT_PAREN, "Expect '(' after 'for'");

	if (consumeIfMatch(cCtx, TOKEN_SEMICOLON)) {
		// No initializer
	} else if (consumeIfMatch(cCtx, TOKEN_LOCAL))
		varDeclaration(cCtx, VAR_LOCAL);
	else
		expressionStatement(cCtx);

	int surroundingLoopStart = compilerState->innermostLoopStart;
	int surroundingLoopScopeDepth = compilerState->innermostLoopScopeDepth;
	compilerState->innermostLoopStart = currentChunk(current)->count;
	compilerState->innermostLoopScopeDepth = current->scopeDepth;

	int exitJump = -1;
	if (!consumeIfMatch(cCtx, TOKEN_SEMICOLON)) {
		expression(cCtx, false, false);
		consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after loop condition");

		// Jump out of the loop if the condition is false.
		exitJump = emitJump(cCtx, OP_JUMP_IF_FALSE);
		emitByte(cCtx, OP_POP); // Condition.
	}

	if (!consumeIfMatch(cCtx, TOKEN_RIGHT_PAREN)) {
		int bodyJump = emitJump(cCtx, OP_JUMP);
		int incrementStart = currentChunk(current)->count;
		expression(cCtx, false, false);
		emitByte(cCtx, OP_POP);
		consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after for clauses");

		emitLoop(cCtx, compilerState->innermostLoopStart);
		compilerState->innermostLoopStart = incrementStart;
		patchJump(cCtx, bodyJump);
	}

	statement(cCtx);
	emitLoop(cCtx, compilerState->innermostLoopStart);

	if (exitJump != -1) {
		patchJump(cCtx, exitJump);
		emitByte(cCtx, OP_POP); // Condition.
	}

	patchBreakJumps(cCtx);

	compilerState->innermostLoopStart = surroundingLoopStart;
	compilerState->innermostLoopScopeDepth = surroundingLoopScopeDepth;

	endScope(cCtx);
}

static void forEachStatement(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;
	CompilerState *compilerState = &cCtx->compilerState;

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
	expression(cCtx, false, false);
	consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after foreach iterator");

	emitByte(cCtx, OP_FOREACH_INIT);
	emitBytes(cCtx, hasNextSlot, hasNextVar->postArgs);
	emitBytes(cCtx, nextSlot, nextVar->postArgs);

	int surroundingLoopStart = compilerState->innermostLoopStart;
	int surroundingLoopScopeDepth = compilerState->innermostLoopScopeDepth;
	compilerState->innermostLoopStart = currentChunk(current)->count;
	compilerState->innermostLoopScopeDepth = current->scopeDepth;

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
	emitLoop(cCtx, compilerState->innermostLoopStart);

	patchJump(cCtx, exitJump);
	emitByte(cCtx, OP_POP); // condition

	patchBreakJumps(cCtx);

	compilerState->innermostLoopStart = surroundingLoopStart;
	compilerState->innermostLoopScopeDepth = surroundingLoopScopeDepth;

	endScope(cCtx);
}

static void ifStatement(CCtx *vmCtx) {
	consume(vmCtx, TOKEN_LEFT_PAREN, "Expect '(' after 'if'");
	expression(vmCtx, false, false);
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
	Compiler *current = cCtx->compilerState.current;

	if (current->type == TYPE_SCRIPT)
		error(cCtx, "Can't return from top-level code");

	if (consumeIfMatch(cCtx, TOKEN_SEMICOLON))
		emitReturn(cCtx);
	else {
		if (current->type == TYPE_INITIALIZER)
			error(cCtx, "Can't return a value from an initializer");
		expression(cCtx, false, false);
		consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after return value");
		emitByte(cCtx, OP_RETURN);
	}
}

static void whileStatement(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;
	CompilerState *compilerState = &cCtx->compilerState;

	int surroundingLoopStart = compilerState->innermostLoopStart;
	int surroundingLoopScopeDepth = compilerState->innermostLoopScopeDepth;
	compilerState->innermostLoopStart = currentChunk(current)->count;
	compilerState->innermostLoopScopeDepth = current->scopeDepth;

	consume(cCtx, TOKEN_LEFT_PAREN, "Expect '(' after 'while'");
	expression(cCtx, false, false);
	consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after condition");

	int exitJump = emitJump(cCtx, OP_JUMP_IF_FALSE);
	emitByte(cCtx, OP_POP);
	statement(cCtx);

	emitLoop(cCtx, compilerState->innermostLoopStart);

	patchJump(cCtx, exitJump);
	emitByte(cCtx, OP_POP);

	patchBreakJumps(cCtx);

	compilerState->innermostLoopStart = surroundingLoopStart;
	compilerState->innermostLoopScopeDepth = surroundingLoopScopeDepth;
}

static void throwStatement(CCtx *cCtx) {
	expression(cCtx, false, false);
	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after value");
	emitByte(cCtx, OP_THROW);
}

typedef struct {
	uint16_t address;
	VarRef typeVar;
	int handlerJump;
} CatchHandler;

static void tryCatchStatement(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;

	int currentCatchStack = current->catchStackDepth;
	current->catchStackDepth++;

	emitByte(cCtx, OP_PUSH_EXCEPTION_HANDLER);
	emitByte(cCtx, currentCatchStack);
	int handlerData = emitAddress(cCtx);

	statement(cCtx);
	current->catchStackDepth--;

	emitByte(cCtx, OP_POP_EXCEPTION_HANDLER);
	emitByte(cCtx, currentCatchStack);

	int successJump = emitJump(cCtx, OP_JUMP);

	CatchHandler handlers[32];
	int numCatchClauses = 0;
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
		}

		emitByte(cCtx, OP_POP_EXCEPTION_HANDLER);
		emitByte(cCtx, currentCatchStack);
		statement(cCtx);
		endScope(cCtx);
		handlers[numCatchClauses].handlerJump = emitJump(cCtx, OP_JUMP);
		numCatchClauses++;
	}

	// Catch table
	emitByte(cCtx, OP_DATA);
	patchAddress(current, handlerData);
	emitByte(cCtx, 5 * numCatchClauses);
	for (int i = 0; i < numCatchClauses; i++) {
		// TODO: local postArgs
		emitByte(cCtx, handlers[i].typeVar.type);
		emitUShort(cCtx, handlers[i].typeVar.handle);
		emitUShort(cCtx, handlers[i].address);
	}

	for (int i = 0; i < numCatchClauses; i++)
		patchJump(cCtx, handlers[i].handlerJump);
	patchJump(cCtx, successJump);
}

static void synchronize(CCtx *cCtx) {
	Parser *parser = &cCtx->compilerState.parser;

	parser->panicMode = false;

	while (parser->current.type != TOKEN_EOF) {
		if (parser->previous.type == TOKEN_SEMICOLON)
			return;
		switch (parser->current.type) {
			case TOKEN_BREAK:
			case TOKEN_CLASS:
			case TOKEN_CONTINUE:
			case TOKEN_FUNCTION:
			case TOKEN_GLOBAL:
			case TOKEN_LOCAL:
			case TOKEN_FOR:
			case TOKEN_FOREACH:
			case TOKEN_IF:
			case TOKEN_WHILE:
			case TOKEN_RETURN:
			case TOKEN_THROW:
			case TOKEN_TRY:
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
		forStatement(cCtx);
	else if (consumeIfMatch(cCtx, TOKEN_FOREACH))
		forEachStatement(cCtx);
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
	Parser *parser = &cCtx->compilerState.parser;

	VarType varType = VAR_LOCAL;
	bool expectVar = false;

	if (consumeIfMatch(cCtx, TOKEN_GLOBAL)) {
		expectVar = true;
		varType = VAR_GLOBAL;
	} else if (consumeIfMatch(cCtx, TOKEN_LOCAL))
		expectVar = true;

	if (consumeIfMatch(cCtx, TOKEN_CLASS))
		classDeclaration(cCtx, varType);
	else if (check(cCtx, TOKEN_FUNCTION) && (checkNext(cCtx, TOKEN_IDENTIFIER))) {
		consume(cCtx, TOKEN_FUNCTION, NULL);
		functionDeclaration(cCtx, true);
	} else if (expectVar)
		varDeclaration(cCtx, varType);
	else
		statement(cCtx);

	if (parser->panicMode)
		synchronize(cCtx);
}

ObjFunction *compile(VMCtx *vmCtx, uint8_t *source, const String *moduleName) {
	CCtx cCtx;
	Compiler compiler;
	Parser *parser = &cCtx.compilerState.parser;

	initCompilerContext(&cCtx, vmCtx, moduleName);
	initScanner(&cCtx, source);
	initCompiler(&cCtx, &compiler, TYPE_SCRIPT);

	pushCompilerState(vmCtx, &cCtx.compilerState);

	parser->hadError = false;
	parser->panicMode = false;
	parser->hasNext = false;

	advance(&cCtx);

	while (!consumeIfMatch(&cCtx, TOKEN_EOF))
		declaration(&cCtx);

	ObjFunction *function = endCompiler(&cCtx);

	popCompilerState(vmCtx);

	return parser->hadError ? NULL : function;
}

void markCompilerRoots(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	for (int i = 0; i < vm->compilerCount; i++) {
		Compiler *compiler = vm->compilerStack[i]->current;
		while (compiler != NULL) {
			markObject(vmCtx, (Obj *)compiler->function);
			for (int j = 0; j < compiler->numArgs; j++)
				markValue(vmCtx, compiler->defaultArgs[j]);
			compiler = compiler->enclosing;
		}
	}
}
