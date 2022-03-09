#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "slox/common.h"
#include "slox/compiler.h"
#include "slox/memory.h"
#include "slox/scanner.h"
#include "slox/state.h"

#ifdef DEBUG_PRINT_CODE
#include "slox/debug.h"
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

typedef void (*ParseFn)(VMCtx *vmCtx, bool canAssign);

typedef struct {
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

void initCompilerState(VMCtx *vmCtx) {
	CompilerState *state = &vmCtx->compiler;
	state->current = NULL;
	state->currentClass = NULL;
	state->innermostLoopStart = -1;
	state->innermostLoopScopeDepth = 0;
	state->breakJumps = NULL;
	state->lambdaCount = 0;
}

static Chunk *currentChunk(Compiler *current) {
	return &current->function->chunk;
}

static void errorAt(Parser *parser, Token *token, const char *message) {
	if (parser->panicMode)
		return;
	parser->panicMode = true;
	fprintf(stderr, "[line %d] Error", token->line);

	if (token->type == TOKEN_EOF) {
		fprintf(stderr, " at end");
	} else if (token->type == TOKEN_ERROR) {
		// Nothing.
	} else {
		fprintf(stderr, " at '%.*s'", token->length, token->start);
	}

	fprintf(stderr, ": %s\n", message);
	parser->hadError = true;
}

static void error(Parser *parser, const char *message) {
	errorAt(parser, &parser->previous, message);
}

static void errorAtCurrent(Parser *parser, const char *message) {
	errorAt(parser, &parser->current, message);
}

static void advance(VMCtx *vmCtx) {
	Parser *parser = &vmCtx->compiler.parser;
	Scanner *scanner = &vmCtx->scanner;

	parser->previous = parser->current;

	for (;;) {
		if (parser->hasNext) {
			parser->current = parser->next;
			parser->hasNext = false;
		} else
			parser->current = scanToken(scanner);
		if (parser->current.type != TOKEN_ERROR)
			break;

		errorAtCurrent(parser, parser->current.start);
	}
}

static void consume(VMCtx *vmCtx, TokenType type, const char *message) {
	Parser *parser = &vmCtx->compiler.parser;

	if (parser->current.type == type) {
		advance(vmCtx);
		return;
	}

	errorAtCurrent(parser, message);
}

static bool check(Parser *parser, TokenType type) {
	return parser->current.type == type;
}

static bool checkNext(VMCtx *vmCtx, TokenType type) {
	Parser *parser = &vmCtx->compiler.parser;
	Scanner *scanner = &vmCtx->scanner;

	if (isAtEnd(scanner))
		return false;

	parser->next = scanToken(scanner);
	parser->hasNext = true;

	return parser->next.type == type;
}

static bool match(VMCtx *vmCtx, TokenType type) {
	Parser *parser = &vmCtx->compiler.parser;

	if (!check(parser, type))
		return false;
	advance(vmCtx);
	return true;
}

static void emitByte(VMCtx *vmCtx, uint8_t byte) {
	Compiler *current = vmCtx->compiler.current;
	Parser *parser = &vmCtx->compiler.parser;

	writeChunk(vmCtx, currentChunk(current), byte, parser->previous.line);
}

static void emitBytes(VMCtx *vmCtx, uint8_t byte1, uint8_t byte2) {
	emitByte(vmCtx, byte1);
	emitByte(vmCtx, byte2);
}

static void emitPop(VMCtx *vmCtx, uint8_t n) {
	if (n == 0)
		return;
	if (n == 1)
		emitByte(vmCtx, OP_POP);
	else
		emitBytes(vmCtx, OP_POPN, n);
}

static void emitUShort(VMCtx *vmCtx, uint16_t val) {
	emitByte(vmCtx, (val >> 8) & 0xff);
	emitByte(vmCtx, val & 0xff);
}

static void emitLoop(VMCtx *vmCtx, int loopStart) {
	Compiler *current = vmCtx->compiler.current;
	Parser *parser = &vmCtx->compiler.parser;

	emitByte(vmCtx, OP_LOOP);

	int offset = currentChunk(current)->count - loopStart + 2;
	if (offset > UINT16_MAX)
		error(parser, "Loop body too large");

	emitByte(vmCtx, (offset >> 8) & 0xff);
	emitByte(vmCtx, offset & 0xff);
}

static int emitJump(VMCtx *vmCtx, uint8_t instruction) {
	Compiler *current = vmCtx->compiler.current;

	emitByte(vmCtx, instruction);
	emitByte(vmCtx, 0xff);
	emitByte(vmCtx, 0xff);
	return currentChunk(current)->count - 2;
}

static int emitAddress(VMCtx *vmCtx) {
	Compiler *current = vmCtx->compiler.current;

	emitByte(vmCtx, 0xff);
	emitByte(vmCtx, 0xff);
	return currentChunk(current)->count - 2;
}

static void patchAddress(Compiler *current, uint16_t offset) {
	int address = currentChunk(current)->count;
	currentChunk(current)->code[offset] = (address >> 8) & 0xff;
	currentChunk(current)->code[offset + 1] = address & 0xff;
}

static void emitReturn(VMCtx *vmCtx) {
	Compiler *current = vmCtx->compiler.current;

	if (current->type == TYPE_INITIALIZER) {
		emitBytes(vmCtx, OP_GET_LOCAL, 0);
	} else {
		emitByte(vmCtx, OP_NIL);
	}
	emitByte(vmCtx, OP_RETURN);
}

static uint16_t makeConstant(VMCtx *vmCtx, Value value) {
	Compiler *current = vmCtx->compiler.current;
	Parser *parser = &vmCtx->compiler.parser;

	int constant = addConstant(vmCtx, currentChunk(current), value);
	if (constant > UINT16_MAX) {
		error(parser, "Too many constants in one chunk");
		return 0;
	}

	return (uint16_t)constant;
}

static void emitConstant(VMCtx *vmCtx, Value value) {
	uint16_t constantIndex = makeConstant(vmCtx, value);
	emitByte(vmCtx, OP_CONSTANT);
	emitUShort(vmCtx, constantIndex);
}

static void patchJump(VMCtx *vmCtx, int offset) {
	Compiler *current = vmCtx->compiler.current;
	Parser *parser = &vmCtx->compiler.parser;

	// -2 to adjust for the bytecode for the jump offset itself
	int jump = currentChunk(current)->count - offset - 2;

	if (jump > UINT16_MAX) {
		error(parser, "Too much code to jump over");
	}

	currentChunk(current)->code[offset] = (jump >> 8) & 0xff;
	currentChunk(current)->code[offset + 1] = jump & 0xff;
}

static void patchBreakJumps(VMCtx *vmCtx) {
	CompilerState *compilerState = &vmCtx->compiler;

	while (compilerState->breakJumps != NULL) {
		if (compilerState->breakJumps->scopeDepth >= compilerState->innermostLoopScopeDepth) {
			// Patch break jump
			patchJump(vmCtx, compilerState->breakJumps->offset);

			BreakJump *temp = compilerState->breakJumps;
			compilerState->breakJumps = compilerState->breakJumps->next;
			FREE(vmCtx, BreakJump, temp);
		} else {
			break;
		}
	}
}

static Compiler *initCompiler(VMCtx *vmCtx, Compiler *compiler, FunctionType type) {
	Compiler *current = vmCtx->compiler.current;
	Parser *parser = &vmCtx->compiler.parser;

	compiler->enclosing = current;
	compiler->function = NULL;
	compiler->type = type;
	compiler->localCount = 0;
	compiler->scopeDepth = 0;
	compiler->catchStackDepth = 0;
	compiler->function = newFunction(vmCtx);
	initTable(&compiler->stringConstants);

	vmCtx->compiler.current = current = compiler;
	if (type == TYPE_SCRIPT)
		current->function->name = NULL;
	else if (type == TYPE_LAMBDA) {
		char lambdaBuffer[64];
		int len = sprintf(lambdaBuffer, "<lambda_%d>", vmCtx->compiler.lambdaCount++);
		current->function->name = copyString(vmCtx, lambdaBuffer, len);
	} else {
		current->function->name = copyString(vmCtx,
											 parser->previous.start,
											 parser->previous.length);
	}

	Local *local = &current->locals[current->localCount++];
	local->depth = 0;
	local->isCaptured = false;
	if (type != TYPE_FUNCTION) {
		local->name.start = "this";
		local->name.length = 4;
	} else {
		local->name.start = "";
		local->name.length = 0;
	}

	return current;
}

static ObjFunction *endCompiler(VMCtx *vmCtx) {
	Compiler *current = vmCtx->compiler.current;

	emitReturn(vmCtx);
	ObjFunction* function = current->function;

#ifdef DEBUG_PRINT_CODE
	Parser *parser = &vmCtx->compiler.parser;
	if (!parser->hadError) {
		disassembleChunk(currentChunk(current),
						 function->name != NULL ? function->name->chars : "<script>");
	}
#endif

	freeTable(vmCtx, &current->stringConstants);

	vmCtx->compiler.current = current->enclosing;

	return function;
}

static void beginScope(VMCtx *vmCtx) {
	Compiler *current = vmCtx->compiler.current;
	current->scopeDepth++;
}

static void endScope(VMCtx *vmCtx) {
	Compiler *current = vmCtx->compiler.current;

	current->scopeDepth--;

	int numPendingPop = 0;
	while ((current->localCount > 0) &&
		   (current->locals[current->localCount - 1].depth > current->scopeDepth)) {
		if (current->locals[current->localCount - 1].isCaptured) {
			if (numPendingPop > 0) {
				emitPop(vmCtx, numPendingPop);
				numPendingPop = 0;
			}
			emitByte(vmCtx, OP_CLOSE_UPVALUE);
		} else {
			numPendingPop++;
		}
		current->localCount--;
	}

	emitPop(vmCtx, numPendingPop);
}

static void statement(VMCtx *vmCtx);
static void declaration();
static ParseRule *getRule(TokenType type);
static void and_(VMCtx *vmCtx, bool canAssign);

static void parsePrecedence(VMCtx *vmCtx, Precedence precedence) {
	Parser *parser = &vmCtx->compiler.parser;

	advance(vmCtx);
	ParseFn prefixRule = getRule(parser->previous.type)->prefix;
	if (prefixRule == NULL) {
		error(parser, "Expect expression");
		return;
	}

	bool canAssign = (precedence <= PREC_ASSIGNMENT);
	prefixRule(vmCtx, canAssign);

	while (precedence <= getRule(parser->current.type)->precedence) {
		advance(vmCtx);
		ParseFn infixRule = getRule(parser->previous.type)->infix;
		infixRule(vmCtx, canAssign);
	}

	if (canAssign && match(vmCtx, TOKEN_EQUAL)) {
		error(parser, "Invalid assignment target");
	}
}

static void expression(VMCtx *vmCtx) {
	parsePrecedence(vmCtx, PREC_ASSIGNMENT);
}

static void binary(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	Parser *parser = &vmCtx->compiler.parser;

	TokenType operatorType = parser->previous.type;
	ParseRule *rule = getRule(operatorType);
	parsePrecedence(vmCtx, (Precedence)(rule->precedence + 1));

	switch (operatorType) {
		case TOKEN_BANG_EQUAL:
			emitBytes(vmCtx, OP_EQUAL, OP_NOT);
			break;
		case TOKEN_EQUAL_EQUAL:
			emitByte(vmCtx, OP_EQUAL);
			break;
		case TOKEN_GREATER:
			emitByte(vmCtx, OP_GREATER);
			break;
		case TOKEN_GREATER_EQUAL:
			emitBytes(vmCtx, OP_LESS, OP_NOT);
			break;
		case TOKEN_LESS:
			emitByte(vmCtx, OP_LESS);
			break;
		case TOKEN_LESS_EQUAL:
			emitBytes(vmCtx, OP_GREATER, OP_NOT);
			break;
		case TOKEN_PLUS:
			emitByte(vmCtx, OP_ADD);
			break;
		case TOKEN_MINUS:
			emitByte(vmCtx, OP_SUBTRACT);
			break;
		case TOKEN_STAR:
			emitByte(vmCtx, OP_MULTIPLY);
			break;
		case TOKEN_SLASH:
			emitByte(vmCtx, OP_DIVIDE);
			break;
		case TOKEN_PERCENT:
			emitByte(vmCtx, OP_MODULO);
			break;
		default: return;
			// Unreachable.
	}
}

static uint8_t argumentList(VMCtx *vmCtx) {
	Parser *parser = &vmCtx->compiler.parser;

	uint8_t argCount = 0;
	if (!check(parser, TOKEN_RIGHT_PAREN)) {
		do {
			expression(vmCtx);
			if (argCount == 255) {
				error(parser, "Can't have more than 255 arguments");
			}
			argCount++;
		} while (match(vmCtx, TOKEN_COMMA));
	}
	consume(vmCtx, TOKEN_RIGHT_PAREN, "Expect ')' after function arguments");
	return argCount;
}

static void call(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	uint8_t argCount = argumentList(vmCtx);
	emitBytes(vmCtx, OP_CALL, argCount);
}

static uint16_t identifierConstant(VMCtx *vmCtx, Token *name) {
	Compiler *current = vmCtx->compiler.current;

	// See if we already have it.
	ObjString *string = copyString(vmCtx, name->start, name->length);
	Value indexValue;
	if (tableGet(&current->stringConstants, string, &indexValue)) {
		// We do.
		return (uint16_t)AS_NUMBER(indexValue);
	}

	uint16_t index = makeConstant(vmCtx, OBJ_VAL(string));
	tableSet(vmCtx, &current->stringConstants, string, NUMBER_VAL((double)index));
	return index;
}

static void dot(VMCtx *vmCtx, bool canAssign) {
	Parser *parser = &vmCtx->compiler.parser;

	consume(vmCtx, TOKEN_IDENTIFIER, "Expect property name after '.'");
	uint16_t name = identifierConstant(vmCtx, &parser->previous);

	if (canAssign && match(vmCtx, TOKEN_EQUAL)) {
		expression(vmCtx);
		emitByte(vmCtx, OP_SET_PROPERTY);
		emitUShort(vmCtx, name);
	} else if (match(vmCtx, TOKEN_LEFT_PAREN)) {
		uint8_t argCount = argumentList(vmCtx);
		emitByte(vmCtx, OP_INVOKE);
		emitUShort(vmCtx, name);
		emitByte(vmCtx, argCount);
	} else {
		emitBytes(vmCtx, OP_GET_PROPERTY, false);
		emitUShort(vmCtx, name);
	}
}

static void colon(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	Parser *parser = &vmCtx->compiler.parser;

	consume(vmCtx, TOKEN_IDENTIFIER, "Expect property name after ':'");
	uint16_t name = identifierConstant(vmCtx, &parser->previous);

	if (match(vmCtx, TOKEN_LEFT_PAREN)) {
		uint8_t argCount = argumentList(vmCtx);
		emitByte(vmCtx, OP_INVOKE);
		emitUShort(vmCtx, name);
		emitByte(vmCtx, argCount);
	} else {
		emitBytes(vmCtx, OP_GET_PROPERTY, true);
		emitUShort(vmCtx, name);
	}
}

static void literal(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	Parser *parser = &vmCtx->compiler.parser;

	switch (parser->previous.type) {
		case TOKEN_FALSE:
			emitByte(vmCtx, OP_FALSE);
			break;
		case TOKEN_NIL:
			emitByte(vmCtx, OP_NIL);
			break;
		case TOKEN_TRUE:
			emitByte(vmCtx, OP_TRUE);
			break;
		default:
			return; // Unreachable.
	}
}

static void grouping(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	expression(vmCtx);
	consume(vmCtx, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void parseArray(VMCtx *vmCtx, ObjType objType) {
	Parser *parser = &vmCtx->compiler.parser;

	int itemCount = 0;
	if (!check(parser, TOKEN_RIGHT_BRACKET)) {
		do {
			if (check(parser, TOKEN_RIGHT_BRACKET)) {
				// Let's support a trailing comma
				break;
			}

			parsePrecedence(vmCtx, PREC_OR);

			if (itemCount == UINT16_COUNT) {
				error(parser, "Cannot have more than 16384 items in an array literal.");
			}
			itemCount++;
		} while (match(vmCtx, TOKEN_COMMA));
	}

	consume(vmCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after array literal.");

	emitBytes(vmCtx, OP_ARRAY_BUILD, objType);
	emitByte(vmCtx, (itemCount >> 8) & 0xff);
	emitByte(vmCtx, itemCount & 0xff);
	return;
}

static void array(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	parseArray(vmCtx, OBJ_ARRAY);
}

static void tuple(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	consume(vmCtx, TOKEN_LEFT_BRACKET, "");
	parseArray(vmCtx, OBJ_TUPLE);
}

static void index_(VMCtx *vmCtx, bool canAssign) {
	parsePrecedence(vmCtx, PREC_OR);
	consume(vmCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after index.");

	if (canAssign && match(vmCtx, TOKEN_EQUAL)) {
		expression(vmCtx);
		emitByte(vmCtx, OP_INDEX_STORE);
	} else {
		emitByte(vmCtx, OP_INDEX);
	}
	return;
}

static void map(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	Parser *parser = &vmCtx->compiler.parser;

	int itemCount = 0;

	if (!check(parser, TOKEN_RIGHT_BRACE)) {
		do {
			if (check(parser, TOKEN_RIGHT_BRACE)) {
				// Let's support a trailing comma
				break;
			}

			if (match(vmCtx, TOKEN_IDENTIFIER)) {
				uint16_t key = identifierConstant(vmCtx, &parser->previous);
				emitByte(vmCtx, OP_CONSTANT);
				emitUShort(vmCtx, key);
			} else {
				consume(vmCtx, TOKEN_LEFT_BRACKET, "Expecting identifier or index expression as key");
				parsePrecedence(vmCtx, PREC_OR);
				consume(vmCtx, TOKEN_RIGHT_BRACKET, "Expect ']' after index");
			}
			consume(vmCtx, TOKEN_EQUAL, "Expect '=' between key and value pair");
			parsePrecedence(vmCtx, PREC_OR);

			if (itemCount == UINT16_COUNT) {
				error(parser, "Maximum 65536 items allowed in a map constructor");
			}
			itemCount++;
		} while (match(vmCtx, TOKEN_COMMA));
	}
	consume(vmCtx, TOKEN_RIGHT_BRACE, "Expect '}' after map elements");

	emitByte(vmCtx, OP_MAP_BUILD);
	emitUShort(vmCtx, itemCount);
}

static bool identifiersEqual(Token *a, Token *b) {
	if (a->length != b->length)
		return false;
	return memcmp(a->start, b->start, a->length) == 0;
}

static void addLocal(VMCtx *vmCtx, Token name) {
	Compiler *current = vmCtx->compiler.current;
	Parser *parser = &vmCtx->compiler.parser;

	if (current->localCount == UINT8_COUNT) {
		error(parser, "Too many local variables in function.");
		return;
	}

	Local *local = &current->locals[current->localCount++];
	local->name = name;
	local->depth = -1;
	local->isCaptured = false;
}

static void declareVariable(VMCtx *vmCtx) {
	Compiler *current = vmCtx->compiler.current;
	Parser *parser = &vmCtx->compiler.parser;

	if (current->scopeDepth == 0)
		return;

	Token *name = &parser->previous;
	for (int i = current->localCount - 1; i >= 0; i--) {
		Local *local = &current->locals[i];
		if (local->depth != -1 && local->depth < current->scopeDepth) {
			break;
		}

		if (identifiersEqual(name, &local->name)) {
			error(parser, "Already a variable with this name in this scope");
		}
	}

	addLocal(vmCtx, *name);
}

static uint16_t parseVariable(VMCtx *vmCtx, const char *errorMessage) {
	Compiler *current = vmCtx->compiler.current;
	Parser *parser = &vmCtx->compiler.parser;

	consume(vmCtx, TOKEN_IDENTIFIER, errorMessage);

	declareVariable(vmCtx);
	if (current->scopeDepth > 0)
		return 0;

	return identifierConstant(vmCtx, &parser->previous);
}

static void markInitialized(Compiler *current) {
	if (current->scopeDepth == 0)
		return;
	current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(VMCtx *vmCtx, uint16_t global) {
	Compiler *current = vmCtx->compiler.current;

	if (current->scopeDepth > 0) {
		markInitialized(current);
		return;
	}

	emitByte(vmCtx, OP_DEFINE_GLOBAL);
	emitUShort(vmCtx, global);
}

static void block(VMCtx *vmCtx) {
	Parser *parser = &vmCtx->compiler.parser;

	while (!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF)) {
		declaration(vmCtx);
	}

	consume(vmCtx, TOKEN_RIGHT_BRACE, "Expect '}' after block");
}

static int resolveLocal(VMCtx *vmCtx, Compiler *compiler, Token *name) {
	Parser *parser = &vmCtx->compiler.parser;

	for (int i = compiler->localCount - 1; i >= 0; i--) {
		Local *local = &compiler->locals[i];
		if (identifiersEqual(name, &local->name)) {
			if (local->depth == -1) {
				error(parser, "Can't read local variable in its own initializer");
			}
			return i;
		}
	}

	return -1;
}

static int addUpvalue(VMCtx *vmCtx, Compiler *compiler, uint8_t index, bool isLocal) {
	Parser *parser = &vmCtx->compiler.parser;

	int upvalueCount = compiler->function->upvalueCount;

	for (int i = 0; i < upvalueCount; i++) {
		Upvalue *upvalue = &compiler->upvalues[i];
		if (upvalue->index == index && upvalue->isLocal == isLocal) {
			return i;
		}
	}

	if (upvalueCount == UINT8_COUNT) {
		error(parser, "Too many closure variables in function");
		return 0;
	}

	compiler->upvalues[upvalueCount].isLocal = isLocal;
	compiler->upvalues[upvalueCount].index = index;
	return compiler->function->upvalueCount++;
}

static int resolveUpvalue(VMCtx *vmCtx, Compiler *compiler, Token *name) {
	if (compiler->enclosing == NULL)
		return -1;

	int local = resolveLocal(vmCtx, compiler->enclosing, name);
	if (local != -1) {
			compiler->enclosing->locals[local].isCaptured = true;
			return addUpvalue(vmCtx, compiler, (uint8_t)local, true);
	}

	int upvalue = resolveUpvalue(vmCtx, compiler->enclosing, name);
	if (upvalue != -1) {
		return addUpvalue(vmCtx, compiler, (uint8_t)upvalue, false);
	}

	return -1;
}

static void namedVariable(VMCtx *vmCtx, Token name, bool canAssign) {
	Compiler *current = vmCtx->compiler.current;

	uint8_t getOp, setOp;
	int arg = resolveLocal(vmCtx, current, &name);
	bool shortArg = false;
	if (arg != -1) {
		getOp = OP_GET_LOCAL;
		setOp = OP_SET_LOCAL;
	} else if ((arg = resolveUpvalue(vmCtx, current, &name)) != -1) {
		getOp = OP_GET_UPVALUE;
		setOp = OP_SET_UPVALUE;
	} else {
		arg = identifierConstant(vmCtx, &name);
		getOp = OP_GET_GLOBAL;
		setOp = OP_SET_GLOBAL;
		shortArg = true;
	}

	if (canAssign && match(vmCtx, TOKEN_EQUAL)) {
		expression(vmCtx);
		emitByte(vmCtx, setOp);
		if (shortArg)
			emitUShort(vmCtx, (uint16_t)arg);
		else
			emitByte(vmCtx, (uint8_t)arg);
	} else {
		emitByte(vmCtx, getOp);
		if (shortArg)
			emitUShort(vmCtx, (uint16_t)arg);
		else
			emitByte(vmCtx, (uint8_t)arg);
	}
}

static Token syntheticToken(const char *text) {
	Token token;
	token.start = text;
	token.length = (int)strlen(text);
	return token;
}

static void function(VMCtx *vmCtx, FunctionType type) {
	Parser *parser = &vmCtx->compiler.parser;

	Compiler compiler;
	Compiler *current = initCompiler(vmCtx, &compiler, type);
	beginScope(vmCtx);

	consume(vmCtx, TOKEN_LEFT_PAREN, "Expect '(' after function name");
	if (!check(parser, TOKEN_RIGHT_PAREN)) {
		do {
			current->function->arity++;
			if (current->function->arity > 255) {
				errorAtCurrent(parser, "Can't have more than 255 parameters");
			}
			uint8_t constant = parseVariable(vmCtx, "Expect parameter name");
			defineVariable(vmCtx, constant);
		} while (match(vmCtx, TOKEN_COMMA));
	}
	consume(vmCtx, TOKEN_RIGHT_PAREN, "Expect ')' after parameters");

	uint8_t argCount = 0;
	namedVariable(vmCtx, syntheticToken("this"), false);
	if (match(vmCtx, TOKEN_COLON)) {
		if (type != TYPE_INITIALIZER) {
			errorAtCurrent(parser, "Only initializers can be chained");
		}
		consume(vmCtx, TOKEN_SUPER, "Expect 'super'");
		consume(vmCtx, TOKEN_LEFT_PAREN, "Expect super argument list");
		argCount = argumentList(vmCtx);
	}
	namedVariable(vmCtx, syntheticToken("super"), false);
	emitBytes(vmCtx, OP_SUPER_INIT, argCount);

	consume(vmCtx, TOKEN_LEFT_BRACE, "Expect '{' before function body");
	block(vmCtx);

	ObjFunction *function = endCompiler(vmCtx);
	uint16_t functionConstant = makeConstant(vmCtx, OBJ_VAL(function));
	if (function->upvalueCount > 0) {
		emitByte(vmCtx, OP_CLOSURE);
		emitUShort(vmCtx, functionConstant);

		// Emit arguments for each upvalue to know whether to capture a local or an upvalue
		for (int i = 0; i < function->upvalueCount; i++) {
			emitByte(vmCtx, compiler.upvalues[i].isLocal ? 1 : 0);
			emitByte(vmCtx, compiler.upvalues[i].index);
		}
	} else {
		// No need to create a closure
		emitByte(vmCtx, OP_CONSTANT);
		emitUShort(vmCtx, functionConstant);
	}
}

static void lambda(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	function(vmCtx, TYPE_LAMBDA);
}

static void number(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	Parser *parser = &vmCtx->compiler.parser;
	double value = strtod(parser->previous.start, NULL);
	emitConstant(vmCtx, NUMBER_VAL(value));
}

static void or_(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	int elseJump = emitJump(vmCtx, OP_JUMP_IF_FALSE);
	int endJump = emitJump(vmCtx, OP_JUMP);

	patchJump(vmCtx, elseJump);
	emitByte(vmCtx, OP_POP);

	parsePrecedence(vmCtx, PREC_OR);
	patchJump(vmCtx, endJump);
}

static void string(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	Parser *parser = &vmCtx->compiler.parser;

	emitConstant(vmCtx, OBJ_VAL(copyString(vmCtx,
										   parser->previous.start + 1,
										   parser->previous.length - 2)));
}

typedef struct {
	VarType type;
	uint16_t handle; //slot for local or upvalue, name index for global
} VarRef;

static VarRef resolveVar(VMCtx *vmCtx, Token name) {
	Compiler *current = vmCtx->compiler.current;

	int slot = resolveLocal(vmCtx, current, &name);
	if (slot >= 0)
		return (VarRef){.type = VAR_LOCAL, .handle = slot};
	else if ((slot = resolveUpvalue(vmCtx, current, &name)) >= 0)
		return (VarRef){.type = VAR_UPVALUE, .handle = slot};

	return (VarRef){.type = VAR_GLOBAL, .handle = identifierConstant(vmCtx, &name)};
}

static void emitUnpack(VMCtx *vmCtx, uint8_t numVal, VarRef *slots) {
	emitByte(vmCtx, OP_UNPACK);
	emitByte(vmCtx, numVal);
	for (int i = 0; i < numVal; i++) {
		emitByte(vmCtx, slots[i].type);
		if (slots[i].type == VAR_GLOBAL)
			emitUShort(vmCtx, slots[i].handle);
		else
			emitByte(vmCtx, slots[i].handle);
	}
}

static void variable(VMCtx *vmCtx, bool canAssign) {
	Parser *parser = &vmCtx->compiler.parser;
	namedVariable(vmCtx, parser->previous, canAssign);
}

static void this_(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	Parser *parser = &vmCtx->compiler.parser;
	ClassCompiler *currentClass = vmCtx->compiler.currentClass;

	if (currentClass == NULL) {
		error(parser, "Can't use 'this' outside of a class.");
		return;
	}

	variable(vmCtx, false);
}

static void super_(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	Parser *parser = &vmCtx->compiler.parser;
	ClassCompiler *currentClass = vmCtx->compiler.currentClass;

	if (currentClass == NULL) {
		error(parser, "Can't use 'super' outside of a class");
	} else if (!currentClass->hasSuperclass) {
		error(parser, "Can't use 'super' in a class with no superclass");
	}

	consume(vmCtx, TOKEN_DOT, "Expect '.' after 'super'");
	consume(vmCtx, TOKEN_IDENTIFIER, "Expect superclass method name");
	uint16_t name = identifierConstant(vmCtx, &parser->previous);

	namedVariable(vmCtx, syntheticToken("this"), false);
	if (match(vmCtx, TOKEN_LEFT_PAREN)) {
		uint8_t argCount = argumentList(vmCtx);
		namedVariable(vmCtx, syntheticToken("super"), false);
		emitByte(vmCtx, OP_SUPER_INVOKE);
		emitUShort(vmCtx, name);
		emitByte(vmCtx, argCount);
	} else {
		namedVariable(vmCtx, syntheticToken("super"), false);
		emitByte(vmCtx, OP_GET_SUPER);
		emitUShort(vmCtx, name);
	}
}

static void unary(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	Parser *parser = &vmCtx->compiler.parser;
	TokenType operatorType = parser->previous.type;

	// Compile the operand.
	parsePrecedence(vmCtx, PREC_UNARY);

	// Emit the operator instruction.
	switch (operatorType) {
		case TOKEN_BANG:
			emitByte(vmCtx, OP_NOT);
			break;
		case TOKEN_MINUS:
			emitByte(vmCtx, OP_NEGATE);
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
	[TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
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
	[TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
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

static void and_(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	int endJump = emitJump(vmCtx, OP_JUMP_IF_FALSE);

	emitByte(vmCtx, OP_POP);
	parsePrecedence(vmCtx, PREC_AND);

	patchJump(vmCtx, endJump);
}

static ParseRule* getRule(TokenType type) {
	return &parseRules[type];
}

static void method(VMCtx *vmCtx, Token className) {
	Parser *parser = &vmCtx->compiler.parser;

	consume(vmCtx, TOKEN_IDENTIFIER, "Expect method name");
	uint16_t constant = identifierConstant(vmCtx, &parser->previous);
	FunctionType type = TYPE_METHOD;

	if (parser->previous.length == className.length &&
		memcmp(parser->previous.start, className.start, className.length) == 0) {
		type = TYPE_INITIALIZER;
	}

	function(vmCtx, type);
	emitByte(vmCtx, OP_METHOD);
	emitUShort(vmCtx, constant);
}

static void classDeclaration(VMCtx *vmCtx) {
	Parser *parser = &vmCtx->compiler.parser;
	ClassCompiler *currentClass = vmCtx->compiler.currentClass;

	consume(vmCtx, TOKEN_IDENTIFIER, "Expect class name");
	Token className = parser->previous;
	uint16_t nameConstant = identifierConstant(vmCtx, &parser->previous);
	declareVariable(vmCtx);

	emitByte(vmCtx, OP_CLASS);
	emitUShort(vmCtx, nameConstant);
	defineVariable(vmCtx, nameConstant);

	ClassCompiler classCompiler;
	classCompiler.hasSuperclass = false;
	classCompiler.enclosing = currentClass;
	vmCtx->compiler.currentClass = currentClass = &classCompiler;

	if (match(vmCtx, TOKEN_COLON)) {
		consume(vmCtx, TOKEN_IDENTIFIER, "Expect superclass name.");
		variable(vmCtx, false);

		if (identifiersEqual(&className, &parser->previous)) {
			error(parser, "A class can't inherit from itself.");
		}
	} else {
		Token rootObjName = syntheticToken("Object");
		uint16_t objNameConstant = identifierConstant(vmCtx, &rootObjName);
		emitByte(vmCtx, OP_GET_GLOBAL);
		emitUShort(vmCtx, objNameConstant);
	}

	beginScope(vmCtx);
	addLocal(vmCtx, syntheticToken("super"));
	defineVariable(vmCtx, 0);

	namedVariable(vmCtx, className, false);
	emitByte(vmCtx, OP_INHERIT);
	classCompiler.hasSuperclass = true;

	namedVariable(vmCtx, className, false);

	consume(vmCtx, TOKEN_LEFT_BRACE, "Expect '{' before class body.");
	while (!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF)) {
		method(vmCtx, className);
	}
	consume(vmCtx, TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
	emitByte(vmCtx, OP_POP);

	if (classCompiler.hasSuperclass) {
		endScope(vmCtx);
	}

	vmCtx->compiler.currentClass = currentClass->enclosing;
}

static void functionDeclaration(VMCtx *vmCtx) {
	Compiler *current = vmCtx->compiler.current;

	uint8_t global = parseVariable(vmCtx, "Expect function name.");
	markInitialized(current);
	function(vmCtx, TYPE_FUNCTION);
	defineVariable(vmCtx, global);
}

static void varDeclaration(VMCtx *vmCtx) {
	uint8_t global = parseVariable(vmCtx, "Expect variable name.");

	if (match(vmCtx, TOKEN_EQUAL)) {
		expression(vmCtx);
	} else {
		emitByte(vmCtx, OP_NIL);
	}
	consume(vmCtx, TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

	defineVariable(vmCtx, global);
}

static void expressionStatement(VMCtx *vmCtx) {
	expression(vmCtx);
	consume(vmCtx, TOKEN_SEMICOLON, "Expect ';' after expression.");
	emitByte(vmCtx, OP_POP);
}

static void breakStatement(VMCtx *vmCtx) {
	Compiler *current = vmCtx->compiler.current;
	Parser *parser = &vmCtx->compiler.parser;
	CompilerState *compilerState = &vmCtx->compiler;

	if (compilerState->innermostLoopStart == -1) {
		error(parser, "Cannot use 'break' outside of a loop.");
	}

	consume(vmCtx, TOKEN_SEMICOLON, "Expect ';' after 'break'.");

	// Discard any locals created inside the loop.
	int numLocals = 0;
	for (int i = current->localCount - 1;
		i >= 0 && current->locals[i].depth > compilerState->innermostLoopScopeDepth;
		i--) {
			numLocals++;
	}
	emitPop(vmCtx, numLocals);

	// Jump to the end of the loop
	// This needs to be patched when loop block is exited
	int jmp = emitJump(vmCtx, OP_JUMP);

	// Record jump for later patching
	BreakJump *breakJump = ALLOCATE(vmCtx, BreakJump, 1);
	breakJump->scopeDepth = compilerState->innermostLoopScopeDepth;
	breakJump->offset = jmp;
	breakJump->next = compilerState->breakJumps;
	compilerState->breakJumps = breakJump;
}

static void continueStatement(VMCtx *vmCtx) {
	Compiler *current = vmCtx->compiler.current;
	Parser *parser = &vmCtx->compiler.parser;
	CompilerState *compilerState = &vmCtx->compiler;

	if (compilerState->innermostLoopStart == -1) {
		error(parser, "Can't use 'continue' outside of a loop.");
	}

	consume(vmCtx, TOKEN_SEMICOLON, "Expect ';' after 'continue'.");

	// Discard any locals created inside the loop.
	int numLocals = 0;
	for (int i = current->localCount - 1;
		 i >= 0 && current->locals[i].depth > compilerState->innermostLoopScopeDepth;
		 i--) {
		numLocals++;
	}
	emitPop(vmCtx, numLocals);

	// Jump to top of current innermost loop.
	emitLoop(vmCtx, compilerState->innermostLoopStart);
}

static void forStatement(VMCtx *vmCtx) {
	Compiler *current = vmCtx->compiler.current;
	CompilerState *compilerState = &vmCtx->compiler;

	beginScope(vmCtx);
	consume(vmCtx, TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

	if (match(vmCtx, TOKEN_SEMICOLON)) {
		// No initializer.
	} else if (match(vmCtx, TOKEN_VAR)) {
		varDeclaration(vmCtx);
	} else {
		expressionStatement(vmCtx);
	}

	int surroundingLoopStart = compilerState->innermostLoopStart;
	int surroundingLoopScopeDepth = compilerState->innermostLoopScopeDepth;
	compilerState->innermostLoopStart = currentChunk(current)->count;
	compilerState->innermostLoopScopeDepth = current->scopeDepth;

	int exitJump = -1;
	if (!match(vmCtx, TOKEN_SEMICOLON)) {
		expression(vmCtx);
		consume(vmCtx, TOKEN_SEMICOLON, "Expect ';' after loop condition");

		// Jump out of the loop if the condition is false.
		exitJump = emitJump(vmCtx, OP_JUMP_IF_FALSE);
		emitByte(vmCtx, OP_POP); // Condition.
	}

	if (!match(vmCtx, TOKEN_RIGHT_PAREN)) {
		int bodyJump = emitJump(vmCtx, OP_JUMP);
		int incrementStart = currentChunk(current)->count;
		expression(vmCtx);
		emitByte(vmCtx, OP_POP);
		consume(vmCtx, TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

		emitLoop(vmCtx, compilerState->innermostLoopStart);
		compilerState->innermostLoopStart = incrementStart;
		patchJump(vmCtx, bodyJump);
	}

	statement(vmCtx);
	emitLoop(vmCtx, compilerState->innermostLoopStart);

	if (exitJump != -1) {
		patchJump(vmCtx, exitJump);
		emitByte(vmCtx, OP_POP); // Condition.
	}

	patchBreakJumps(vmCtx);

	compilerState->innermostLoopStart = surroundingLoopStart;
	compilerState->innermostLoopScopeDepth = surroundingLoopScopeDepth;

	endScope(vmCtx);
}

static void forEachStatement(VMCtx *vmCtx) {
	Compiler *current = vmCtx->compiler.current;
	Parser *parser = &vmCtx->compiler.parser;
	CompilerState *compilerState = &vmCtx->compiler;

	beginScope(vmCtx);
	consume(vmCtx, TOKEN_LEFT_PAREN, "Expect '(' after 'foreach'");

	VarRef foreachVars[16];

	int numVars = 0;
	do {
		if (match(vmCtx, TOKEN_VAR)) {
			consume(vmCtx, TOKEN_IDENTIFIER, "Var name expected in foreach");
			addLocal(vmCtx, parser->previous);
			markInitialized(current);
			emitByte(vmCtx, OP_NIL);
		} else {
			consume(vmCtx, TOKEN_IDENTIFIER, "Var name expected in foreach");
		}

		foreachVars[numVars] = resolveVar(vmCtx, parser->previous);
		numVars++;
	} while (match(vmCtx, TOKEN_COMMA));
	consume (vmCtx, TOKEN_COLON, "Expect ':' after foreach variables");

	Token iterName = syntheticToken("$iter");
	Token stateName = syntheticToken("$state");
	Token varName = syntheticToken("$var");
	addLocal(vmCtx, iterName);
	emitByte(vmCtx, OP_NIL);
	markInitialized(current);
	uint8_t iterSlot = resolveLocal(vmCtx, current, &iterName);
	addLocal(vmCtx, stateName);
	emitByte(vmCtx, OP_NIL);
	markInitialized(current);
	uint8_t stateSlot = resolveLocal(vmCtx, current, &stateName);
	addLocal(vmCtx, varName);
	emitByte(vmCtx, OP_NIL);
	markInitialized(current);
	uint8_t varSlot = resolveLocal(vmCtx, current, &varName);

	// iterator
	expression(vmCtx);
	consume(vmCtx, TOKEN_RIGHT_PAREN, "Expect ')' after foreach iterator");

	emitByte(vmCtx, OP_FOREACH_INIT);
	emitByte(vmCtx, iterSlot);
	emitByte(vmCtx, stateSlot);
	emitByte(vmCtx, varSlot);

	int initJump = emitJump(vmCtx, OP_JUMP_IF_FALSE);
	emitBytes(vmCtx, OP_CALL, 0);
	// no state and control variable
	emitBytes(vmCtx, OP_SET_LOCAL, iterSlot);
	emitByte(vmCtx, OP_POP);
	// will also match the temporary 'false'
	emitByte(vmCtx, OP_NIL);
	emitBytes(vmCtx, OP_SET_LOCAL, stateSlot);
	emitBytes(vmCtx, OP_SET_LOCAL, varSlot);

	patchJump(vmCtx, initJump);

	// discard temporary marker
	emitByte(vmCtx, OP_POP);

	int surroundingLoopStart = compilerState->innermostLoopStart;
	int surroundingLoopScopeDepth = compilerState->innermostLoopScopeDepth;
	compilerState->innermostLoopStart = currentChunk(current)->count;
	compilerState->innermostLoopScopeDepth = current->scopeDepth;

	emitBytes(vmCtx, OP_GET_LOCAL, iterSlot);
	emitBytes(vmCtx, OP_GET_LOCAL, stateSlot);
	emitBytes(vmCtx, OP_GET_LOCAL, varSlot);
	emitBytes(vmCtx, OP_CALL, 2);

	emitUnpack(vmCtx, numVars, foreachVars);

	switch (foreachVars[0].type) {
		case VAR_LOCAL:
			emitBytes(vmCtx, OP_GET_LOCAL, foreachVars[0].handle);
			break;
		case VAR_UPVALUE:
			emitBytes(vmCtx, OP_GET_UPVALUE, foreachVars[0].handle);
			break;
		case VAR_GLOBAL:
			emitByte(vmCtx, OP_GET_GLOBAL);
			emitUShort(vmCtx, foreachVars[0].handle);
			break;
	}
	emitBytes(vmCtx, OP_SET_LOCAL, varSlot);
	emitByte(vmCtx, OP_NIL);
	emitBytes(vmCtx, OP_EQUAL, OP_NOT);
	int exitJump = emitJump(vmCtx, OP_JUMP_IF_FALSE);
	emitByte(vmCtx, OP_POP); // condition

	statement(vmCtx);
	emitLoop(vmCtx, compilerState->innermostLoopStart);

	patchJump(vmCtx, exitJump);
	emitByte(vmCtx, OP_POP); // condition

	patchBreakJumps(vmCtx);

	compilerState->innermostLoopStart = surroundingLoopStart;
	compilerState->innermostLoopScopeDepth = surroundingLoopScopeDepth;

	endScope(vmCtx);
}

static void ifStatement(VMCtx *vmCtx) {
	consume(vmCtx, TOKEN_LEFT_PAREN, "Expect '(' after 'if'");
	expression(vmCtx);
	consume(vmCtx, TOKEN_RIGHT_PAREN, "Expect ')' after condition");

	int thenJump = emitJump(vmCtx, OP_JUMP_IF_FALSE);
	emitByte(vmCtx, OP_POP);
	statement(vmCtx);

	int elseJump = emitJump(vmCtx, OP_JUMP);

	patchJump(vmCtx, thenJump);
	emitByte(vmCtx, OP_POP);

	if (match(vmCtx, TOKEN_ELSE))
		statement(vmCtx);
	patchJump(vmCtx, elseJump);
}

static void printStatement(VMCtx *vmCtx) {
	expression(vmCtx);
	consume(vmCtx, TOKEN_SEMICOLON, "Expect ';' after value.");
	emitByte(vmCtx, OP_PRINT);
}

static void returnStatement(VMCtx *vmCtx) {
	Compiler *current = vmCtx->compiler.current;
	Parser *parser = &vmCtx->compiler.parser;

	if (current->type == TYPE_SCRIPT) {
		error(parser, "Can't return from top-level code.");
	}

	if (match(vmCtx, TOKEN_SEMICOLON)) {
		emitReturn(vmCtx);
	} else {
		if (current->type == TYPE_INITIALIZER) {
			error(parser, "Can't return a value from an initializer.");
		}
		expression(vmCtx);
		consume(vmCtx, TOKEN_SEMICOLON, "Expect ';' after return value.");
		emitByte(vmCtx, OP_RETURN);
	}
}

static void whileStatement(VMCtx *vmCtx) {
	Compiler *current = vmCtx->compiler.current;
	CompilerState *compilerState = &vmCtx->compiler;

	int surroundingLoopStart = compilerState->innermostLoopStart;
	int surroundingLoopScopeDepth = compilerState->innermostLoopScopeDepth;
	compilerState->innermostLoopStart = currentChunk(current)->count;
	compilerState->innermostLoopScopeDepth = current->scopeDepth;

	consume(vmCtx, TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
	expression(vmCtx);
	consume(vmCtx, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

	int exitJump = emitJump(vmCtx, OP_JUMP_IF_FALSE);
	emitByte(vmCtx, OP_POP);
	statement(vmCtx);

	emitLoop(vmCtx, compilerState->innermostLoopStart);

	patchJump(vmCtx, exitJump);
	emitByte(vmCtx, OP_POP);

	patchBreakJumps(vmCtx);

	compilerState->innermostLoopStart = surroundingLoopStart;
	compilerState->innermostLoopScopeDepth = surroundingLoopScopeDepth;
}

static void throwStatement(VMCtx *vmCtx) {
	expression(vmCtx);
	consume(vmCtx, TOKEN_SEMICOLON, "Expect ';' after value.");
	emitByte(vmCtx, OP_THROW);
}

typedef struct {
	uint16_t address;
	//uint16_t class;
	VarRef typeVar;
	int handlerJump;
} CatchHandler;

static void tryCatchStatement(VMCtx *vmCtx) {
	Compiler *current = vmCtx->compiler.current;
	Parser *parser = &vmCtx->compiler.parser;

	int currentCatchStack = current->catchStackDepth;
	current->catchStackDepth++;

	emitByte(vmCtx, OP_PUSH_EXCEPTION_HANDLER);
	emitByte(vmCtx, currentCatchStack);
	int handlerData = emitAddress(vmCtx);

	statement(vmCtx);
	current->catchStackDepth--;

	emitByte(vmCtx, OP_POP_EXCEPTION_HANDLER);
	emitByte(vmCtx, currentCatchStack);

	int successJump = emitJump(vmCtx, OP_JUMP);

	CatchHandler handlers[32];
	int numCatchClauses = 0;
	while (match(vmCtx, TOKEN_CATCH)) {
		beginScope(vmCtx);
		consume(vmCtx, TOKEN_LEFT_PAREN, "Expect '(' after catch");
		consume(vmCtx, TOKEN_IDENTIFIER, "Expect type name to catch");
		Token typeName = parser->previous;
		//uint16_t name = identifierConstant(vmCtx, &parser->previous);

		handlers[numCatchClauses].typeVar = resolveVar(vmCtx, typeName);
		//handlers[numCatchClauses].class = name;
		handlers[numCatchClauses].address = currentChunk(current)->count;

		if (!match(vmCtx, TOKEN_RIGHT_PAREN)) {
			consume(vmCtx, TOKEN_IDENTIFIER, "Expect identifier for exception instance");
			addLocal(vmCtx, parser->previous);
			markInitialized(current);
			uint8_t ex_var = resolveLocal(vmCtx, current, &parser->previous);
			emitBytes(vmCtx, OP_SET_LOCAL, ex_var);
			consume(vmCtx, TOKEN_RIGHT_PAREN, "Expect ')' after catch statement");
		}

		emitByte(vmCtx, OP_POP_EXCEPTION_HANDLER);
		emitByte(vmCtx, currentCatchStack);
		statement(vmCtx);
		endScope(vmCtx);
		handlers[numCatchClauses].handlerJump = emitJump(vmCtx, OP_JUMP);
		numCatchClauses++;
	}

	// Catch table
	emitByte(vmCtx, OP_DATA);
	patchAddress(current, handlerData);
	emitByte(vmCtx, 5 * numCatchClauses);
	for (int i = 0; i < numCatchClauses; i++) {
		emitByte(vmCtx, handlers[i].typeVar.type);
		emitUShort(vmCtx, handlers[i].typeVar.handle);
		emitUShort(vmCtx, handlers[i].address);
	}

	for (int i = 0; i < numCatchClauses; i++) {
		patchJump(vmCtx, handlers[i].handlerJump);
	}
	patchJump(vmCtx, successJump);
}

static void synchronize(VMCtx *vmCtx) {
	Parser *parser = &vmCtx->compiler.parser;

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
			case TOKEN_IF:
			case TOKEN_WHILE:
			case TOKEN_PRINT:
			case TOKEN_RETURN:
			case TOKEN_THROW:
				return;
			default:
				; // Do nothing.
		}

		advance(vmCtx);
	}
}

static void statement(VMCtx *vmCtx) {
	if (match(vmCtx, TOKEN_BREAK)) {
		breakStatement(vmCtx);
	} else if (match(vmCtx, TOKEN_CONTINUE)) {
		continueStatement(vmCtx);
	} else if (match(vmCtx, TOKEN_PRINT)) {
		printStatement(vmCtx);
	} else if (match(vmCtx, TOKEN_FOR)) {
		forStatement(vmCtx);
	} else if (match(vmCtx, TOKEN_FOREACH)) {
		forEachStatement(vmCtx);
	} else if (match(vmCtx, TOKEN_IF)) {
		ifStatement(vmCtx);
	} else if (match(vmCtx, TOKEN_RETURN)) {
		returnStatement(vmCtx);
	} else if (match(vmCtx, TOKEN_WHILE)) {
		whileStatement(vmCtx);
	} else if (match(vmCtx, TOKEN_LEFT_BRACE)) {
		beginScope(vmCtx);
		block(vmCtx);
		endScope(vmCtx);
	} else if (match(vmCtx, TOKEN_THROW)) {
		throwStatement(vmCtx);
	} else if (match(vmCtx, TOKEN_TRY)) {
		tryCatchStatement(vmCtx);
	} else {
		expressionStatement(vmCtx);
	}
}

static void declaration(VMCtx *vmCtx) {
	Parser *parser = &vmCtx->compiler.parser;

	if (match(vmCtx, TOKEN_CLASS)) {
		classDeclaration(vmCtx);
	} else if (check(parser, TOKEN_FUNCTION) && (checkNext(vmCtx, TOKEN_IDENTIFIER))) {
		consume(vmCtx, TOKEN_FUNCTION, NULL);
		functionDeclaration(vmCtx);
	} else if (match(vmCtx, TOKEN_VAR)) {
		varDeclaration(vmCtx);
	} else {
		statement(vmCtx);
	}

	if (parser->panicMode)
		synchronize(vmCtx);
}

ObjFunction *compile(VMCtx *vmCtx, char *source) {
	Parser *parser = &vmCtx->compiler.parser;

	initScanner(vmCtx, source);

	Compiler compiler;
	initCompiler(vmCtx, &compiler, TYPE_SCRIPT);

	parser->hadError = false;
	parser->panicMode = false;
	parser->hasNext = false;

	advance(vmCtx);

	while (!match(vmCtx, TOKEN_EOF)) {
		declaration(vmCtx);
	}

	ObjFunction* function = endCompiler(vmCtx);

	return parser->hadError ? NULL : function;
}

void markCompilerRoots(VMCtx *vmCtx) {
	Compiler *compiler = vmCtx->compiler.current;
	while (compiler != NULL) {
		markObject(vmCtx, (Obj *)compiler->function);
		compiler = compiler->enclosing;
	}
}
