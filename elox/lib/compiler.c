#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "elox/common.h"
#include "elox/compiler.h"
#include "elox/memory.h"
#include "elox/scanner.h"
#include "elox/state.h"
#include "elox/builtins.h"

#ifdef ELOX_DEBUG_PRINT_CODE
#include "elox/debug.h"
#endif

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
	PREC_CALL,        // . ()
	PREC_SUBSCRIPT,   // []
	PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(CCtx *cCtx, bool canAssign);

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

static void errorAt(Parser *parser, Token *token, const char *message) {
	if (parser->panicMode)
		return;
	parser->panicMode = true;
	fprintf(stderr, "[line %d] Error", token->line);

	if (token->type == TOKEN_EOF)
		fprintf(stderr, " at end");
	else if (token->type == TOKEN_ERROR) {
		// Nothing.
	} else
		fprintf(stderr, " at '%.*s'", token->string.length, token->string.chars);

	fprintf(stderr, ": %s\n", message);
	parser->hadError = true;
}

static void error(Parser *parser, const char *message) {
	errorAt(parser, &parser->previous, message);
}

static void errorAtCurrent(Parser *parser, const char *message) {
	errorAt(parser, &parser->current, message);
}

static void advance(CCtx *cCtx) {
	Parser *parser = &cCtx->compilerState.parser;
	Scanner *scanner = &cCtx->scanner;

	parser->beforePrevious = parser->previous;
	parser->previous = parser->current;

	for (;;) {
		if (parser->hasNext) {
			parser->current = parser->next;
			parser->hasNext = false;
		} else
			parser->current = scanToken(scanner);
		if (parser->current.type != TOKEN_ERROR)
			break;

		errorAtCurrent(parser, parser->current.string.chars);
	}
}

static void consume(CCtx *cCtx, TokenType type, const char *message) {
	Parser *parser = &cCtx->compilerState.parser;

	if (parser->current.type == type) {
		advance(cCtx);
		return;
	}

	errorAtCurrent(parser, message);
}

static bool check(Parser *parser, TokenType type) {
	return parser->current.type == type;
}

static bool checkNext(CCtx *cCtx, TokenType type) {
	Parser *parser = &cCtx->compilerState.parser;
	Scanner *scanner = &cCtx->scanner;

	if (isAtEnd(scanner))
		return false;

	parser->next = scanToken(scanner);
	parser->hasNext = true;

	return parser->next.type == type;
}

static bool consumeIfMatch(CCtx *cCtx, TokenType type) {
	Parser *parser = &cCtx->compilerState.parser;

	if (!check(parser, type))
		return false;
	advance(cCtx);
	return true;
}

static int emitByte(CCtx *cCtx, uint8_t byte) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;

	writeChunk(cCtx->vmCtx, currentChunk(current), byte, parser->previous.line);
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

static void emitUShort(CCtx *cCtx, uint16_t val) {
	emitByte(cCtx, (val >> 8) & 0xff);
	emitByte(cCtx, val & 0xff);
}

static void emitLoop(CCtx *cCtx, int loopStart) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;

	emitByte(cCtx, OP_LOOP);

	int offset = currentChunk(current)->count - loopStart + 2;
	if (offset > UINT16_MAX)
		error(parser, "Loop body too large");

	emitByte(cCtx, (offset >> 8) & 0xff);
	emitByte(cCtx, offset & 0xff);
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
	int address = currentChunk(current)->count;
	currentChunk(current)->code[offset] = (address >> 8) & 0xff;
	currentChunk(current)->code[offset + 1] = address & 0xff;
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
	Parser *parser = &cCtx->compilerState.parser;

	int constant = addConstant(cCtx->vmCtx, currentChunk(current), value);
	if (constant > UINT16_MAX) {
		error(parser, "Too many constants in one chunk");
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
				if (val < 256)
					emitBytes(cCtx, OP_IMM8, val);
				else {
					emitByte(cCtx, OP_IMM16);
					emitUShort(cCtx, val);
				}
				return;
			}
		}
	}
	uint16_t constantIndex = makeConstant(cCtx, value);
	emitConstantOp(cCtx, constantIndex);
}

static void patchJump(CCtx *cCtx, int offset) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;

	// -2 to adjust for the bytecode for the jump offset itself
	int jump = currentChunk(current)->count - offset - 2;

	if (jump > UINT16_MAX)
		error(parser, "Too much code to jump over");

	currentChunk(current)->code[offset] = (jump >> 8) & 0xff;
	currentChunk(current)->code[offset + 1] = jump & 0xff;
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
	compiler->scopeDepth = 0;
	compiler->catchStackDepth = 0;
	compiler->function = newFunction(vmCtx);
	initTable(&compiler->stringConstants);

	cCtx->compilerState.current = current = compiler;
	if (type == TYPE_SCRIPT)
		current->function->name = NULL;
	else if (type == TYPE_LAMBDA) {
		char lambdaBuffer[64];
		int len = sprintf(lambdaBuffer, "<lambda_%d>", cCtx->compilerState.lambdaCount++);
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
		local->name.string.chars = "this";
		local->name.string.length = 4;
	} else {
		local->name.string.chars = "";
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
	if (!parser->hadError) {
		disassembleChunk(currentChunk(current),
						 function->name != NULL ? function->name->string.chars : "<script>");
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
static void declaration();
static ParseRule *getRule(TokenType type);
static void and_(CCtx *cCtx, bool canAssign);

static void parsePrecedence(CCtx *cCtx, Precedence precedence) {
	Parser *parser = &cCtx->compilerState.parser;

	advance(cCtx);
	ParseFn prefixRule = getRule(parser->previous.type)->prefix;
	if (prefixRule == NULL) {
		error(parser, "Expect expression");
		return;
	}

	bool canAssign = (precedence <= PREC_ASSIGNMENT);
	prefixRule(cCtx, canAssign);

	while (precedence <= getRule(parser->current.type)->precedence) {
		advance(cCtx);
		ParseFn infixRule = getRule(parser->previous.type)->infix;
		infixRule(cCtx, canAssign);
	}

	if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL))
		error(parser, "Invalid assignment target");
}

static void expression(CCtx *cCtx) {
	parsePrecedence(cCtx, PREC_ASSIGNMENT);
}

static void binary(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;

	TokenType operatorType = parser->previous.type;
	ParseRule *rule = getRule(operatorType);
	parsePrecedence(cCtx, (Precedence)(rule->precedence + 1));

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
		default: return;
			// Unreachable.
	}
}

static uint8_t argumentList(CCtx *cCtx) {
	Parser *parser = &cCtx->compilerState.parser;

	uint8_t argCount = 0;
	if (!check(parser, TOKEN_RIGHT_PAREN)) {
		do {
			expression(cCtx);
			if (argCount == 255)
				error(parser, "Can't have more than 255 arguments");
			argCount++;
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	}
	consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after function arguments");
	return argCount;
}

static void call(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	uint8_t argCount = argumentList(cCtx);
	emitBytes(cCtx, OP_CALL, argCount);
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
	ExecContext execCtx = EXEC_CTX_INITIALIZER(vmCtx);
	if (valueTableGet(&execCtx, &vm->globalNames, OBJ_VAL(identifier), &indexValue)) {
		// We do
		pop(vm);
		return (uint16_t)AS_NUMBER(indexValue);
	}

	uint16_t newIndex = (uint16_t)vm->globalValues.count;
	writeValueArray(vmCtx, &vm->globalValues, UNDEFINED_VAL);
	valueTableSet(&execCtx, &vm->globalNames, OBJ_VAL(identifier), NUMBER_VAL((double)newIndex));
	pop(vm);

#ifdef ELOX_DEBUG_PRINT_CODE
	printf(">>>Global[%5u] (%.*s:%.*s)\n", newIndex,
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

static void colon(CCtx *cCtx, bool canAssign) {
	Parser *parser = &cCtx->compilerState.parser;
	VMCtx *vmCtx = cCtx->vmCtx;

	bool isThisRef = (parser->beforePrevious.type == TOKEN_THIS);
	consume(cCtx, TOKEN_IDENTIFIER, "Expect property name after '.'");
	Token *propName = &parser->previous;
	uint16_t name = identifierConstant(cCtx, propName);

	if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL)) {
		expression(cCtx);
		if (isThisRef) {
			int propSlot = addPendingProperty(vmCtx, &cCtx->compilerState, name,
											  MEMBER_FIELD_MASK, true);
			emitByte(cCtx, OP_SET_MEMBER_PROPERTY);
			emitUShort(cCtx, propSlot);
		} else {
			emitByte(cCtx, OP_SET_PROPERTY);
			emitUShort(cCtx, name);
		}
	} else if (consumeIfMatch(cCtx, TOKEN_LEFT_PAREN)) {
		uint8_t argCount = argumentList(cCtx);
		if (isThisRef) {
			int propSlot = addPendingProperty(vmCtx, &cCtx->compilerState, name,
											  MEMBER_ANY_MASK, true);
			emitByte(cCtx, OP_MEMBER_INVOKE);
			emitUShort(cCtx, propSlot);
			emitByte(cCtx, argCount);
		} else {
			emitByte(cCtx, OP_INVOKE);
			emitUShort(cCtx, name);
			emitByte(cCtx, argCount);
		}
	} else {
		if (isThisRef) {
			int propSlot = addPendingProperty(vmCtx, &cCtx->compilerState, name,
											  MEMBER_ANY_MASK, true);
			emitByte(cCtx, OP_GET_MEMBER_PROPERTY);
			emitUShort(cCtx, propSlot);
		} else {
			emitBytes(cCtx, OP_GET_PROPERTY, false);
			emitUShort(cCtx, name);
		}
	}
}

static void dot(CCtx *cCtx, bool canAssign) {
	Parser *parser = &cCtx->compilerState.parser;

	consume(cCtx, TOKEN_IDENTIFIER, "Expect property name after '.'");
	Token *propName = &parser->previous;
	uint16_t name = identifierConstant(cCtx, propName);

	if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL)) {
		expression(cCtx);
		emitByte(cCtx, OP_MAP_SET);
		emitUShort(cCtx, name);
	} else {
		emitByte(cCtx, OP_MAP_GET);
		emitUShort(cCtx, name);
	}
}

static void literal(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
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
			return; // Unreachable.
	}
}

static void grouping(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	expression(cCtx);
	consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after expression");
}

static void parseArray(CCtx *cCtx, ObjType objType) {
	Parser *parser = &cCtx->compilerState.parser;

	int itemCount = 0;
	if (!check(parser, TOKEN_RIGHT_BRACKET)) {
		do {
			if (check(parser, TOKEN_RIGHT_BRACKET)) {
				// Let's support a trailing comma
				break;
			}

			parsePrecedence(cCtx, PREC_OR);

			if (itemCount == UINT16_COUNT)
				error(parser, "Cannot have more than 16384 items in an array literal");
			itemCount++;
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	}

	consume(cCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after array literal");

	emitBytes(cCtx, OP_ARRAY_BUILD, objType);
	emitByte(cCtx, (itemCount >> 8) & 0xff);
	emitByte(cCtx, itemCount & 0xff);
	return;
}

static void array(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	parseArray(cCtx, OBJ_ARRAY);
}

static void tuple(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	consume(cCtx, TOKEN_LEFT_BRACKET, "");
	parseArray(cCtx, OBJ_TUPLE);
}

static void index_(CCtx *cCtx, bool canAssign) {
	parsePrecedence(cCtx, PREC_OR);
	consume(cCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after index");

	if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL)) {
		expression(cCtx);
		emitByte(cCtx, OP_INDEX_STORE);
	} else
		emitByte(cCtx, OP_INDEX);
	return;
}

static void map(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;

	int itemCount = 0;

	if (!check(parser, TOKEN_RIGHT_BRACE)) {
		do {
			if (check(parser, TOKEN_RIGHT_BRACE)) {
				// Let's support a trailing comma
				break;
			}

			if (consumeIfMatch(cCtx, TOKEN_IDENTIFIER)) {
				uint16_t key = identifierConstant(cCtx, &parser->previous);
				emitConstantOp(cCtx, key);
			} else {
				consume(cCtx, TOKEN_LEFT_BRACKET, "Expecting identifier or index expression as key");
				parsePrecedence(cCtx, PREC_OR);
				consume(cCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after index");
			}
			consume(cCtx, TOKEN_EQUAL, "Expect '=' between key and value pair");
			parsePrecedence(cCtx, PREC_OR);

			if (itemCount == UINT16_COUNT) {
				error(parser, "Maximum 65536 items allowed in a map constructor");
			}
			itemCount++;
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	}
	consume(cCtx, TOKEN_RIGHT_BRACE, "Expect '}' after map elements");

	emitByte(cCtx, OP_MAP_BUILD);
	emitUShort(cCtx, itemCount);
}

static bool identifiersEqual(Token *a, Token *b) {
	if (a->string.length != b->string.length)
		return false;
	return memcmp(a->string.chars, b->string.chars, a->string.length) == 0;
}

static void addLocal(CCtx *cCtx, Token name) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;

	if (current->localCount == UINT8_COUNT) {
		error(parser, "Too many local variables in function");
		return;
	}

	Local *local = &current->locals[current->localCount++];
	local->name = name;
	local->depth = -1;
	local->postArgs = current->postArgs;
	local->isCaptured = false;
}

static void declareVariable(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;

	if (current->scopeDepth == 0)
		return;

	Token *name = &parser->previous;
	for (int i = current->localCount - 1; i >= 0; i--) {
		Local *local = &current->locals[i];
		if (local->depth != -1 && local->depth < current->scopeDepth)
			break;

		if (identifiersEqual(name, &local->name))
			error(parser, "Duplicated variable in this scope");
	}

	addLocal(cCtx, *name);
}

static uint16_t parseVariable(CCtx *cCtx, const char *errorMessage) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;

	consume(cCtx, TOKEN_IDENTIFIER, errorMessage);

	declareVariable(cCtx);
	if (current->scopeDepth > 0)
		return 0;

	return globalIdentifierConstant(cCtx->vmCtx, &parser->previous.string, &cCtx->moduleName);
}

static void markInitialized(Compiler *current) {
	if (current->scopeDepth == 0)
		return;
	current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(CCtx *cCtx, uint16_t global) {
	Compiler *current = cCtx->compilerState.current;

	if (current->scopeDepth > 0) {
		markInitialized(current);
		return;
	}

	emitByte(cCtx, OP_DEFINE_GLOBAL);
	emitUShort(cCtx, global);
}

static void block(CCtx *cCtx) {
	Parser *parser = &cCtx->compilerState.parser;

	while (!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF))
		declaration(cCtx);

	consume(cCtx, TOKEN_RIGHT_BRACE, "Expect '}' after block");
}

static int resolveLocal(CCtx *cCtx, Compiler *compiler, Token *name, bool *postArgs) {
	Parser *parser = &cCtx->compilerState.parser;

	for (int i = compiler->localCount - 1; i >= 0; i--) {
		Local *local = &compiler->locals[i];
		if (identifiersEqual(name, &local->name)) {
			if (local->depth == -1)
				error(parser, "Can't read local variable in its own initializer");
			*postArgs = local->postArgs;
			return i;
		}
	}

	return -1;
}

static int addUpvalue(CCtx *cCtx, Compiler *compiler,
					  uint8_t index, bool postArgs, bool isLocal) {
	Parser *parser = &cCtx->compilerState.parser;

	int upvalueCount = compiler->function->upvalueCount;

	for (int i = 0; i < upvalueCount; i++) {
		Upvalue *upvalue = &compiler->upvalues[i];
		if (upvalue->index == index && upvalue->isLocal == isLocal)
			return i;
	}

	if (upvalueCount == UINT8_COUNT) {
		error(parser, "Too many closure variables in function");
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

static void loadOrAssignVariable(CCtx *cCtx, Token name, bool canAssign) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;
	VM *vm = &cCtx->vmCtx->vm;

	uint8_t getOp, setOp;
	bool postArgs = false;
	int arg = resolveLocal(cCtx, current, &name, &postArgs);
	bool shortArg = false;
	bool local = false;
	if (arg != -1) {
		getOp = OP_GET_LOCAL;
		setOp = OP_SET_LOCAL;
		local = true;
	} else if ((arg = resolveUpvalue(cCtx, current, &name)) != -1) {
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
		arg = globalIdentifierConstant(cCtx->vmCtx, varName, moduleName);
		getOp = OP_GET_GLOBAL;
		setOp = OP_SET_GLOBAL;
		shortArg = true;
	}

	if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL)) {
		expression(cCtx);
		emitByte(cCtx, setOp);
		if (shortArg)
			emitUShort(cCtx, (uint16_t)arg);
		else {
			emitByte(cCtx, (uint8_t)arg);
			if (local)
				emitByte(cCtx, (uint8_t)postArgs);
		}
	} else {
		emitByte(cCtx, getOp);
		if (shortArg)
			emitUShort(cCtx, (uint16_t)arg);
		else {
			emitByte(cCtx, (uint8_t)arg);
			if (local)
				emitByte(cCtx, (uint8_t)postArgs);
		}
	}
}

Token syntheticToken(const char *text) {
	Token token;
	token.string.chars = text;
	token.string.length = (int)strlen(text);
	return token;
}

static void function(CCtx *cCtx, FunctionType type) {
	Parser *parser = &cCtx->compilerState.parser;

	Compiler compiler;
	Compiler *current = initCompiler(cCtx, &compiler, type);
	beginScope(cCtx);

	bool varArg = false;
	consume(cCtx, TOKEN_LEFT_PAREN, "Expect '(' after function name");
	if (!check(parser, TOKEN_RIGHT_PAREN)) {
		do {
			current->function->arity++;
			if (current->function->arity > 255)
				errorAtCurrent(parser, "Can't have more than 255 parameters");
			if (consumeIfMatch(cCtx, TOKEN_ELLIPSIS)) {
				current->function->arity--;
				varArg = true;
				if (!check(parser, TOKEN_RIGHT_PAREN))
					errorAtCurrent(parser, "Expected ) after ...");
			} else {
				uint16_t constant = parseVariable(cCtx, "Expect parameter name");
				defineVariable(cCtx, constant);
			}
		} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	}
	consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after parameters");
	current->function->maxArgs = varArg ? 255 : current->function->arity;
	current->postArgs = true;

	uint8_t argCount = 0;

//	if (type == TYPE_INITIALIZER)
//		loadOrAssignVariable(cCtx, syntheticToken("this"), false);
	if (consumeIfMatch(cCtx, TOKEN_COLON)) {
		if (type != TYPE_INITIALIZER)
			errorAtCurrent(parser, "Only initializers can be chained");
		consume(cCtx, TOKEN_SUPER, "Expect 'super'");
		consume(cCtx, TOKEN_LEFT_PAREN, "Expect super argument list");
		argCount = argumentList(cCtx);
	}
	if (type == TYPE_INITIALIZER) {
		loadOrAssignVariable(cCtx, syntheticToken("super"), false);
		emitBytes(cCtx, OP_SUPER_INIT, argCount);
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

static void importStatement(CCtx *cCtx) {
	Parser *parser = &cCtx->compilerState.parser;

	consume(cCtx, TOKEN_IDENTIFIER, "Expect module name");
	uint16_t moduleName = identifierConstant(cCtx, &parser->previous);
	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after import");

	emitByte(cCtx, OP_IMPORT);
	emitUShort(cCtx, moduleName);
}

static void lambda(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	function(cCtx, TYPE_LAMBDA);
}

static void number(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;
	double value = strtod(parser->previous.string.chars, NULL);
	emitConstant(cCtx, NUMBER_VAL(value));
}

static void or_(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	int elseJump = emitJump(cCtx, OP_JUMP_IF_FALSE);
	int endJump = emitJump(cCtx, OP_JUMP);

	patchJump(cCtx, elseJump);
	emitByte(cCtx, OP_POP);

	parsePrecedence(cCtx, PREC_OR);
	patchJump(cCtx, endJump);
}

static void string(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;

	emitConstant(cCtx, OBJ_VAL(copyString(cCtx->vmCtx,
										  parser->previous.string.chars + 1,
										  parser->previous.string.length - 2)));
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
		return (VarRef){.type = VAR_LOCAL, .handle = slot, .postArgs = postArgs};
	else if ((slot = resolveUpvalue(cCtx, current, &name)) >= 0)
		return (VarRef){.type = VAR_UPVALUE, .handle = slot};

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
		if (slots[i].type == VAR_GLOBAL)
			emitUShort(cCtx, slots[i].handle);
		else
			emitByte(cCtx, slots[i].handle);
	}
}

static void variable(CCtx *cCtx, bool canAssign) {
	Parser *parser = &cCtx->compilerState.parser;
	loadOrAssignVariable(cCtx, parser->previous, canAssign);
}

static String ellipsisLength = STRING_INITIALIZER("length");

static void ellipsis(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;

	if (consumeIfMatch(cCtx, TOKEN_LEFT_BRACKET)) {
		parsePrecedence(cCtx, PREC_OR);
		consume(cCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after index");

		if (canAssign && consumeIfMatch(cCtx, TOKEN_EQUAL)) {
			expression(cCtx);
			emitByte(cCtx, OP_SET_VARARG);
		} else {
			emitByte(cCtx, OP_GET_VARARG);
		}
	} else if (consumeIfMatch(cCtx, TOKEN_COLON)) {
		consume(cCtx, TOKEN_IDENTIFIER, "Expect property name after ':'");
		Token *propName = &parser->previous;
		if (stringEquals(&propName->string, &ellipsisLength)) {
			consume(cCtx, TOKEN_LEFT_PAREN, "Expect '(' after function name");
			consume(cCtx, TOKEN_RIGHT_PAREN, "Function takes no arguments");
			emitByte(cCtx, OP_NUM_VARARGS);
		} else
			errorAtCurrent(parser, "Unknown property name for ...");
	}
}

static void this_(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;
	ClassCompiler *currentClass = cCtx->compilerState.currentClass;

	if (currentClass == NULL) {
		error(parser, "Can't use 'this' outside of a class");
		return;
	}

	variable(cCtx, false);
}

static void super_(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;
	ClassCompiler *currentClass = cCtx->compilerState.currentClass;
	VMCtx *vmCtx = cCtx->vmCtx;

	if (currentClass == NULL)
		error(parser, "Can't use 'super' outside of a class");
	else if (!currentClass->hasSuperclass)
		error(parser, "Can't use 'super' in a class with no superclass");

	consume(cCtx, TOKEN_COLON, "Expect ':' after 'super'");
	consume(cCtx, TOKEN_IDENTIFIER, "Expect superclass method name");
	uint16_t name = identifierConstant(cCtx, &parser->previous);

	loadOrAssignVariable(cCtx, syntheticToken("this"), false);
	if (consumeIfMatch(cCtx, TOKEN_LEFT_PAREN)) {
		uint8_t argCount = argumentList(cCtx);
		int propSlot = addPendingProperty(vmCtx, &cCtx->compilerState, name,
										  MEMBER_METHOD_MASK, false);
		emitByte(cCtx, OP_SUPER_INVOKE);
		emitUShort(cCtx, propSlot);
		emitByte(cCtx, argCount);
	} else {
		int propSlot = addPendingProperty(vmCtx, &cCtx->compilerState, name,
										  MEMBER_METHOD_MASK, false);
		emitByte(cCtx, OP_GET_SUPER);
		emitUShort(cCtx, propSlot);
	}
}

static void unary(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	Parser *parser = &cCtx->compilerState.parser;
	TokenType operatorType = parser->previous.type;

	// Compile the operand.
	parsePrecedence(cCtx, PREC_UNARY);

	// Emit the operator instruction.
	switch (operatorType) {
		case TOKEN_BANG:
			emitByte(cCtx, OP_NOT);
			break;
		case TOKEN_MINUS:
			emitByte(cCtx, OP_NEGATE);
			break;
		default:
			return; // Unreachable.
	}
}

static ParseRule parseRules[] = {
	[TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
	[TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
	[TOKEN_LEFT_BRACE]    = {map,      NULL,   PREC_NONE},
	[TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
	[TOKEN_LEFT_BRACKET]  = {array,    index_, PREC_SUBSCRIPT},
	[TOKEN_RIGHT_BRACKET] = {NULL,     NULL,   PREC_NONE},
	[TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
	[TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
	[TOKEN_PERCENT]       = {NULL,     binary, PREC_FACTOR},
	[TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
	[TOKEN_COLON]         = {tuple,    colon,  PREC_CALL},
	[TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
	[TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
	[TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
	[TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
	[TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
	[TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
	[TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
	[TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
	[TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
	[TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
	[TOKEN_INSTANCEOF]    = {NULL,     binary, PREC_COMPARISON},
	[TOKEN_IMPORT]        = {NULL,     NULL,   PREC_NONE},
	[TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
	[TOKEN_ELLIPSIS]      = {ellipsis, NULL,   PREC_NONE},
	[TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
	[TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
	[TOKEN_AND]           = {NULL,     and_,   PREC_AND},
	[TOKEN_BREAK]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_CATCH]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_CONTINUE]      = {NULL,     NULL,   PREC_NONE},
	[TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
	[TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
	[TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
	[TOKEN_FOREACH]       = {NULL,     NULL,   PREC_NONE},
	[TOKEN_FUNCTION]      = {lambda,   NULL,   PREC_NONE},
	[TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
	[TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
	[TOKEN_OR]            = {NULL,     or_,    PREC_OR},
	[TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
	[TOKEN_SUPER]         = {super_,   NULL,   PREC_NONE},
	[TOKEN_THIS]          = {this_,    NULL,   PREC_NONE},
	[TOKEN_THROW]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
	[TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
	[TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

static void and_(CCtx *cCtx, bool canAssign ELOX_UNUSED) {
	int endJump = emitJump(cCtx, OP_JUMP_IF_FALSE);

	emitByte(cCtx, OP_POP);
	parsePrecedence(cCtx, PREC_AND);

	patchJump(cCtx, endJump);
}

static ParseRule *getRule(TokenType type) {
	return &parseRules[type];
}

static void field(CCtx *cCtx) {
	Parser *parser = &cCtx->compilerState.parser;

	consume(cCtx, TOKEN_IDENTIFIER, "Expect field name");
	uint16_t constant = identifierConstant(cCtx, &parser->previous);
	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after field declaration");

	emitByte(cCtx, OP_FIELD);
	emitUShort(cCtx, constant);
}

static void method(CCtx *cCtx, Token className) {
	Parser *parser = &cCtx->compilerState.parser;

	consume(cCtx, TOKEN_IDENTIFIER, "Expect method name");
	uint16_t constant = identifierConstant(cCtx, &parser->previous);
	FunctionType type = TYPE_METHOD;

	if (parser->previous.string.length == className.string.length &&
		memcmp(parser->previous.string.chars, className.string.chars, className.string.length) == 0) {
		type = TYPE_INITIALIZER;
	}

	function(cCtx, type);
	emitByte(cCtx, OP_METHOD);
	emitUShort(cCtx, constant);
}

static uint8_t getSlotType(uint32_t slot, bool isSuper) {
	uint32_t memberType = (slot & MEMBER_ANY_MASK) >> 30;
	return (uint8_t)isSuper | memberType << 1;
}

static void classDeclaration(CCtx *cCtx) {
	Parser *parser = &cCtx->compilerState.parser;
	ClassCompiler *currentClass = cCtx->compilerState.currentClass;
	VMCtx *vmCtx = cCtx->vmCtx;

	uint16_t classGlobal = parseVariable(cCtx, "Expect class name");
	Token className = parser->previous;
	uint16_t nameConstant = identifierConstant(cCtx, &parser->previous);

	declareVariable(cCtx);

	emitByte(cCtx, OP_CLASS);
	emitUShort(cCtx, nameConstant);

	defineVariable(cCtx, classGlobal);

	ClassCompiler classCompiler;
	classCompiler.hasSuperclass = false;
	initTable(&classCompiler.pendingThisProperties);
	initTable(&classCompiler.pendingSuperProperties);
	classCompiler.enclosing = currentClass;
	cCtx->compilerState.currentClass = currentClass = &classCompiler;

	if (consumeIfMatch(cCtx, TOKEN_COLON)) {
		consume(cCtx, TOKEN_IDENTIFIER, "Expect superclass name");
		variable(cCtx, false);

		if (identifiersEqual(&className, &parser->previous))
			error(parser, "A class can't inherit from itself");
	} else {
		String rootObjName = STRING_INITIALIZER("Object");
		uint16_t objNameConstant = globalIdentifierConstant(vmCtx, &rootObjName, &eloxBuiltinModule);
		emitByte(cCtx, OP_GET_GLOBAL);
		emitUShort(cCtx, objNameConstant);
	}

	beginScope(cCtx);
	addLocal(cCtx, syntheticToken("super"));
	defineVariable(cCtx, 0);

	loadOrAssignVariable(cCtx, className, false);
	emitByte(cCtx, OP_INHERIT);
	classCompiler.hasSuperclass = true;

	loadOrAssignVariable(cCtx, className, false);

	consume(cCtx, TOKEN_LEFT_BRACE, "Expect '{' before class body");
	while (!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF)) {
		if (consumeIfMatch(cCtx, TOKEN_VAR))
			field(cCtx);
		else
			method(cCtx, className);
	}
	consume(cCtx, TOKEN_RIGHT_BRACE, "Expect '}' after class body");

	Table *pendingThis = &classCompiler.pendingThisProperties;
	Table *pendingSuper = &classCompiler.pendingSuperProperties;
	if (pendingThis->count + pendingSuper->count > 0) {
		emitBytes(cCtx, OP_RESOLVE_MEMBERS, pendingThis->count + pendingSuper->count);
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

	freeTable(vmCtx, pendingThis);
	freeTable(vmCtx, pendingSuper);

	emitByte(cCtx, OP_POP); // pop class

	if (classCompiler.hasSuperclass)
		endScope(cCtx);

	cCtx->compilerState.currentClass = currentClass->enclosing;
}

static void functionDeclaration(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;

	uint16_t global = parseVariable(cCtx, "Expect function name");
	markInitialized(current);
	function(cCtx, TYPE_FUNCTION);
	defineVariable(cCtx, global);
}

static void varDeclaration(CCtx *cCtx) {
	uint16_t global = parseVariable(cCtx, "Expect variable name");

	if (consumeIfMatch(cCtx, TOKEN_EQUAL))
		expression(cCtx);
	else
		emitByte(cCtx, OP_NIL);

	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after variable declaration");

	defineVariable(cCtx, global);
}

static void expressionStatement(CCtx *cCtx) {
	expression(cCtx);
	consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after expression");
	emitByte(cCtx, OP_POP);
}

static void breakStatement(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;
	CompilerState *compilerState = &cCtx->compilerState;

	if (compilerState->innermostLoopStart == -1)
		error(parser, "Cannot use 'break' outside of a loop");

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
	int jmp = emitJump(cCtx, OP_JUMP);

	// Record jump for later patching
	BreakJump *breakJump = ALLOCATE(cCtx->vmCtx, BreakJump, 1);
	breakJump->scopeDepth = compilerState->innermostLoopScopeDepth;
	breakJump->offset = jmp;
	breakJump->next = compilerState->breakJumps;
	compilerState->breakJumps = breakJump;
}

static void continueStatement(CCtx *cCtx) {
	Compiler *current = cCtx->compilerState.current;
	Parser *parser = &cCtx->compilerState.parser;
	CompilerState *compilerState = &cCtx->compilerState;

	if (compilerState->innermostLoopStart == -1)
		error(parser, "Can't use 'continue' outside of a loop");

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
	} else if (consumeIfMatch(cCtx, TOKEN_VAR))
		varDeclaration(cCtx);
	else
		expressionStatement(cCtx);

	int surroundingLoopStart = compilerState->innermostLoopStart;
	int surroundingLoopScopeDepth = compilerState->innermostLoopScopeDepth;
	compilerState->innermostLoopStart = currentChunk(current)->count;
	compilerState->innermostLoopScopeDepth = current->scopeDepth;

	int exitJump = -1;
	if (!consumeIfMatch(cCtx, TOKEN_SEMICOLON)) {
		expression(cCtx);
		consume(cCtx, TOKEN_SEMICOLON, "Expect ';' after loop condition");

		// Jump out of the loop if the condition is false.
		exitJump = emitJump(cCtx, OP_JUMP_IF_FALSE);
		emitByte(cCtx, OP_POP); // Condition.
	}

	if (!consumeIfMatch(cCtx, TOKEN_RIGHT_PAREN)) {
		int bodyJump = emitJump(cCtx, OP_JUMP);
		int incrementStart = currentChunk(current)->count;
		expression(cCtx);
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
		if (consumeIfMatch(cCtx, TOKEN_VAR)) {
			consume(cCtx, TOKEN_IDENTIFIER, "Var name expected in foreach");
			addLocal(cCtx, parser->previous);
			markInitialized(current);
			emitByte(cCtx, OP_NIL);
		} else
			consume(cCtx, TOKEN_IDENTIFIER, "Var name expected in foreach");

		foreachVars[numVars] = resolveVar(cCtx, parser->previous);
		numVars++;
	} while (consumeIfMatch(cCtx, TOKEN_COMMA));
	consume (cCtx, TOKEN_COLON, "Expect ':' after foreach variables");

	Token iterName = syntheticToken("$iter");
	Token stateName = syntheticToken("$state");
	Token varName = syntheticToken("$var");
	addLocal(cCtx, iterName);
	emitByte(cCtx, OP_NIL);
	markInitialized(current);
	bool iterPostArgs = false;
	uint8_t iterSlot = resolveLocal(cCtx, current, &iterName, &iterPostArgs);
	addLocal(cCtx, stateName);
	emitByte(cCtx, OP_NIL);
	markInitialized(current);
	bool statePostArgs = false;
	uint8_t stateSlot = resolveLocal(cCtx, current, &stateName, &statePostArgs);
	addLocal(cCtx, varName);
	emitByte(cCtx, OP_NIL);
	markInitialized(current);
	bool varPostArgs = false;
	uint8_t varSlot = resolveLocal(cCtx, current, &varName, &varPostArgs);

	// iterator
	expression(cCtx);
	consume(cCtx, TOKEN_RIGHT_PAREN, "Expect ')' after foreach iterator");

	emitByte(cCtx, OP_FOREACH_INIT);
	emitBytes(cCtx, iterSlot, (uint8_t)iterPostArgs);
	emitBytes(cCtx, stateSlot, (uint8_t)statePostArgs);
	emitBytes(cCtx, varSlot, (uint8_t)varPostArgs);

	int initJump = emitJump(cCtx, OP_JUMP_IF_FALSE);
	emitBytes(cCtx, OP_CALL, 0);
	// no state and control variable
	emitBytes(cCtx, OP_SET_LOCAL, iterSlot);
	emitByte(cCtx, (uint8_t)iterPostArgs);
	emitByte(cCtx, OP_POP);
	// will also match the temporary 'false'
	emitByte(cCtx, OP_NIL);
	emitBytes(cCtx, OP_SET_LOCAL, stateSlot);
	emitByte(cCtx, (uint8_t)statePostArgs);
	emitBytes(cCtx, OP_SET_LOCAL, varSlot);
	emitByte(cCtx, (uint8_t)varPostArgs);

	patchJump(cCtx, initJump);

	// discard temporary marker
	emitByte(cCtx, OP_POP);

	int surroundingLoopStart = compilerState->innermostLoopStart;
	int surroundingLoopScopeDepth = compilerState->innermostLoopScopeDepth;
	compilerState->innermostLoopStart = currentChunk(current)->count;
	compilerState->innermostLoopScopeDepth = current->scopeDepth;

	emitBytes(cCtx, OP_GET_LOCAL, iterSlot);
	emitByte(cCtx, (uint8_t)iterPostArgs);
	emitBytes(cCtx, OP_GET_LOCAL, stateSlot);
	emitByte(cCtx, (uint8_t)statePostArgs);
	emitBytes(cCtx, OP_GET_LOCAL, varSlot);
	emitByte(cCtx, (uint8_t)varPostArgs);
	emitBytes(cCtx, OP_CALL, 2);

	emitUnpack(cCtx, numVars, foreachVars);

	switch (foreachVars[0].type) {
		case VAR_LOCAL:
			emitBytes(cCtx, OP_GET_LOCAL, foreachVars[0].handle);
			emitByte(cCtx, foreachVars[0].postArgs);
			break;
		case VAR_UPVALUE:
			emitBytes(cCtx, OP_GET_UPVALUE, foreachVars[0].handle);
			break;
		case VAR_GLOBAL:
			emitByte(cCtx, OP_GET_GLOBAL);
			emitUShort(cCtx, foreachVars[0].handle);
			break;
	}
	emitBytes(cCtx, OP_SET_LOCAL, varSlot);
	emitByte(cCtx, (uint8_t)varPostArgs);
	emitByte(cCtx, OP_NIL);
	emitBytes(cCtx, OP_EQUAL, OP_NOT);
	int exitJump = emitJump(cCtx, OP_JUMP_IF_FALSE);
	emitByte(cCtx, OP_POP); // condition

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
	expression(vmCtx);
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
	Parser *parser = &cCtx->compilerState.parser;

	if (current->type == TYPE_SCRIPT)
		error(parser, "Can't return from top-level code");

	if (consumeIfMatch(cCtx, TOKEN_SEMICOLON))
		emitReturn(cCtx);
	else {
		if (current->type == TYPE_INITIALIZER)
			error(parser, "Can't return a value from an initializer");
		expression(cCtx);
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
	expression(cCtx);
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
	expression(cCtx);
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
			addLocal(cCtx, parser->previous);
			markInitialized(current);
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
			case TOKEN_VAR:
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
		importStatement(cCtx);
	else
		expressionStatement(cCtx);
}

static void declaration(CCtx *cCtx) {
	Parser *parser = &cCtx->compilerState.parser;

	if (consumeIfMatch(cCtx, TOKEN_CLASS))
		classDeclaration(cCtx);
	else if (check(parser, TOKEN_FUNCTION) && (checkNext(cCtx, TOKEN_IDENTIFIER))) {
		consume(cCtx, TOKEN_FUNCTION, NULL);
		functionDeclaration(cCtx);
	} else if (consumeIfMatch(cCtx, TOKEN_VAR))
		varDeclaration(cCtx);
	else
		statement(cCtx);

	if (parser->panicMode)
		synchronize(cCtx);
}

ObjFunction *compile(VMCtx *vmCtx, char *source, const String *moduleName) {
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
			compiler = compiler->enclosing;
		}
	}
}
