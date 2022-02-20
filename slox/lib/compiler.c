#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "slox/common.h"
#include "slox/compiler.h"
#include "slox/memory.h"
#include "slox/scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "slox/debug.h"
#endif

#pragma GCC diagnostic ignored "-Wswitch-enum"

typedef struct {
	Token current;
	Token previous;
	bool hadError;
	bool panicMode;
} Parser;

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

typedef struct {
	Token name;
	int depth;
	bool isCaptured;
} Local;

typedef struct {
	uint8_t index;
	bool isLocal;
} Upvalue;

typedef enum {
	TYPE_FUNCTION,
	TYPE_INITIALIZER,
	TYPE_METHOD,
	TYPE_SCRIPT
} FunctionType;

typedef struct Compiler {
	struct Compiler *enclosing;
	ObjFunction *function;
	FunctionType type;

	Local locals[UINT8_COUNT];
	int localCount;
	Upvalue upvalues[UINT8_COUNT];
	int scopeDepth;

	Table stringConstants;
} Compiler;

typedef struct ClassCompiler {
	struct ClassCompiler *enclosing;
	bool hasSuperclass;
} ClassCompiler;

typedef struct BreakJump {
	int scopeDepth;
	int offset;
	struct BreakJump *next;
} BreakJump;

static Parser parser;
static Compiler *current = NULL;
static ClassCompiler *currentClass = NULL;

static int innermostLoopStart = -1;
static int innermostLoopScopeDepth = 0;
static BreakJump *breakJumps = NULL;

static Chunk *currentChunk() {
	return &current->function->chunk;
}

static void errorAt(Token *token, const char *message) {
	if (parser.panicMode)
		return;
	parser.panicMode = true;
	fprintf(stderr, "[line %d] Error", token->line);

	if (token->type == TOKEN_EOF) {
		fprintf(stderr, " at end");
	} else if (token->type == TOKEN_ERROR) {
		// Nothing.
	} else {
		fprintf(stderr, " at '%.*s'", token->length, token->start);
	}

	fprintf(stderr, ": %s\n", message);
	parser.hadError = true;
}

static void error(const char *message) {
	errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
	errorAt(&parser.current, message);
}

static void advance() {
	parser.previous = parser.current;

	for (;;) {
		parser.current = scanToken();
		if (parser.current.type != TOKEN_ERROR)
			break;

		errorAtCurrent(parser.current.start);
	}
}

static void consume(TokenType type, const char *message) {
	if (parser.current.type == type) {
		advance();
		return;
	}

	errorAtCurrent(message);
}

static bool check(TokenType type) {
	return parser.current.type == type;
}

static bool match(TokenType type) {
	if (!check(type))
		return false;
	advance();
	return true;
}

static void emitByte(VMCtx *vmCtx, uint8_t byte) {
	writeChunk(vmCtx, currentChunk(), byte, parser.previous.line);
}

static void emitBytes(VMCtx *vmCtx, uint8_t byte1, uint8_t byte2) {
	emitByte(vmCtx, byte1);
	emitByte(vmCtx, byte2);
}

static void emitLoop(VMCtx *vmCtx, int loopStart) {
	emitByte(vmCtx, OP_LOOP);

	int offset = currentChunk()->count - loopStart + 2;
	if (offset > UINT16_MAX)
		error("Loop body too large.");

	emitByte(vmCtx, (offset >> 8) & 0xff);
	emitByte(vmCtx, offset & 0xff);
}

static int emitJump(VMCtx *vmCtx, uint8_t instruction) {
	emitByte(vmCtx, instruction);
	emitByte(vmCtx, 0xff);
	emitByte(vmCtx, 0xff);
	return currentChunk()->count - 2;
}

static void emitReturn(VMCtx *vmCtx) {
	if (current->type == TYPE_INITIALIZER) {
		emitBytes(vmCtx, OP_GET_LOCAL, 0);
	} else {
		emitByte(vmCtx, OP_NIL);
	}
	emitByte(vmCtx, OP_RETURN);
}

static uint8_t makeConstant(VMCtx *vmCtx, Value value) {
	int constant = addConstant(vmCtx, currentChunk(), value);
	if (constant > UINT8_MAX) {
		error("Too many constants in one chunk.");
		return 0;
	}

	return (uint8_t)constant;
}

static void emitConstant(VMCtx *vmCtx, Value value) {
	emitBytes(vmCtx, OP_CONSTANT, makeConstant(vmCtx, value));
}

static void patchJump(int offset) {
	// -2 to adjust for the bytecode for the jump offset itself.
	int jump = currentChunk()->count - offset - 2;

	if (jump > UINT16_MAX) {
		error("Too much code to jump over.");
	}

	currentChunk()->code[offset] = (jump >> 8) & 0xff;
	currentChunk()->code[offset + 1] = jump & 0xff;
}

static void patchBreakJumps(VMCtx *vmCtx) {
	while (breakJumps != NULL) {
		if (breakJumps->scopeDepth >= innermostLoopScopeDepth) {
			// Patch break jump
			patchJump(breakJumps->offset);

			BreakJump *temp = breakJumps;
			breakJumps = breakJumps->next;
			FREE(vmCtx, BreakJump, temp);
		} else {
			break;
		}
	}
}

static void initCompiler(VMCtx *vmCtx, Compiler *compiler, FunctionType type) {
	compiler->enclosing = current;
	compiler->function = NULL;
	compiler->type = type;
	compiler->localCount = 0;
	compiler->scopeDepth = 0;
	compiler->function = newFunction(vmCtx);
	initTable(&compiler->stringConstants);

	current = compiler;
	if (type != TYPE_SCRIPT) {
		current->function->name = copyString(vmCtx,
											 parser.previous.start,
											 parser.previous.length);
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
}

static ObjFunction *endCompiler(VMCtx *vmCtx) {
	emitReturn(vmCtx);
	ObjFunction* function = current->function;

#ifdef DEBUG_PRINT_CODE
	if (!parser.hadError) {
		disassembleChunk(currentChunk(),
						 function->name != NULL ? function->name->chars : "<script>");
	}
#endif

	freeTable(vmCtx, &current->stringConstants);

	current = current->enclosing;

	return function;
}

static void beginScope() {
	current->scopeDepth++;
}

static void endScope(VMCtx *vmCtx) {
	current->scopeDepth--;

	while ((current->localCount > 0) &&
		   (current->locals[current->localCount - 1].depth > current->scopeDepth)) {
		if (current->locals[current->localCount - 1].isCaptured) {
			emitByte(vmCtx, OP_CLOSE_UPVALUE);
		} else {
			emitByte(vmCtx, OP_POP);
		}
		current->localCount--;
	}
}

static void expression(VMCtx *vmCtx);
static void statement(VMCtx *vmCtx);
static void declaration();
static ParseRule *getRule(TokenType type);
static void parsePrecedence(VMCtx *vmCtx, Precedence precedence);
static uint8_t identifierConstant(VMCtx *vmCtx, Token *name);
static int resolveLocal(Compiler *compiler, Token *name);
static void and_(VMCtx *vmCtx, bool canAssign);
static uint8_t argumentList(VMCtx *vmCtx);
static int resolveUpvalue(Compiler *compiler, Token *name);

static void binary(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	TokenType operatorType = parser.previous.type;
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

static void call(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	uint8_t argCount = argumentList(vmCtx);
	emitBytes(vmCtx, OP_CALL, argCount);
}

static void dot(VMCtx *vmCtx, bool canAssign) {
	consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
	uint8_t name = identifierConstant(vmCtx, &parser.previous);

	if (canAssign && match(TOKEN_EQUAL)) {
		expression(vmCtx);
		emitBytes(vmCtx, OP_SET_PROPERTY, name);
	} else if (match(TOKEN_LEFT_PAREN)) {
		uint8_t argCount = argumentList(vmCtx);
		emitBytes(vmCtx, OP_INVOKE, name);
		emitByte(vmCtx, argCount);
	} else {
		emitBytes(vmCtx, OP_GET_PROPERTY, name);
	}
}

static void literal(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	switch (parser.previous.type) {
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
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void array(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	int itemCount = 0;
	if (!check(TOKEN_RIGHT_BRACKET)) {
		do {
			if (check(TOKEN_RIGHT_BRACKET)) {
				// Trailing comma case
				break;
			}

			parsePrecedence(vmCtx, PREC_OR);

			if (itemCount == UINT16_COUNT) {
				error("Cannot have more than 16384 items in an array literal.");
			}
			itemCount++;
		} while (match(TOKEN_COMMA));
	}

	consume(TOKEN_RIGHT_BRACKET, "Expect ']' after array literal.");

	emitByte(vmCtx, OP_ARRAY_BUILD);
	emitByte(vmCtx, (itemCount >> 8) & 0xff);
	emitByte(vmCtx, itemCount & 0xff);
	return;
}

static void index_(VMCtx *vmCtx, bool canAssign) {
	parsePrecedence(vmCtx, PREC_OR);
	consume(TOKEN_RIGHT_BRACKET, "Expect ']' after index.");

	if (canAssign && match(TOKEN_EQUAL)) {
		expression(vmCtx);
		emitByte(vmCtx, OP_ARRAY_STORE);
	} else {
		emitByte(vmCtx, OP_ARRAY_INDEX);
	}
	return;
}

static void number(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	double value = strtod(parser.previous.start, NULL);
	emitConstant(vmCtx, NUMBER_VAL(value));
}

static void or_(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	int elseJump = emitJump(vmCtx, OP_JUMP_IF_FALSE);
	int endJump = emitJump(vmCtx, OP_JUMP);

	patchJump(elseJump);
	emitByte(vmCtx, OP_POP);

	parsePrecedence(vmCtx, PREC_OR);
	patchJump(endJump);
}

static void string(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	emitConstant(vmCtx, OBJ_VAL(copyString(vmCtx,
										   parser.previous.start + 1, parser.previous.length - 2)));
}

static void namedVariable(VMCtx *vmCtx, Token name, bool canAssign) {
	uint8_t getOp, setOp;
	int arg = resolveLocal(current, &name);
	if (arg != -1) {
		getOp = OP_GET_LOCAL;
		setOp = OP_SET_LOCAL;
	} else if ((arg = resolveUpvalue(current, &name)) != -1) {
		getOp = OP_GET_UPVALUE;
		setOp = OP_SET_UPVALUE;
	} else {
		arg = identifierConstant(vmCtx, &name);
		getOp = OP_GET_GLOBAL;
		setOp = OP_SET_GLOBAL;
	}

	if (canAssign && match(TOKEN_EQUAL)) {
		expression(vmCtx);
		emitBytes(vmCtx, setOp, (uint8_t)arg);
	} else {
		emitBytes(vmCtx, getOp, (uint8_t)arg);
	}
}

static void variable(VMCtx *vmCtx, bool canAssign) {
	namedVariable(vmCtx, parser.previous, canAssign);
}

static void this_(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	if (currentClass == NULL) {
		error("Can't use 'this' outside of a class.");
		return;
	}

	variable(vmCtx, false);
}

static Token syntheticToken(const char *text) {
	Token token;
	token.start = text;
	token.length = (int)strlen(text);
	return token;
}

static void super_(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	if (currentClass == NULL) {
		error("Can't use 'super' outside of a class.");
	} else if (!currentClass->hasSuperclass) {
		error("Can't use 'super' in a class with no superclass.");
	}

	consume(TOKEN_DOT, "Expect '.' after 'super'.");
	consume(TOKEN_IDENTIFIER, "Expect superclass method name.");
	uint8_t name = identifierConstant(vmCtx, &parser.previous);

	namedVariable(vmCtx, syntheticToken("this"), false);
	if (match(TOKEN_LEFT_PAREN)) {
		uint8_t argCount = argumentList(vmCtx);
		namedVariable(vmCtx, syntheticToken("super"), false);
		emitBytes(vmCtx, OP_SUPER_INVOKE, name);
		emitByte(vmCtx, argCount);
	} else {
		namedVariable(vmCtx, syntheticToken("super"), false);
		emitBytes(vmCtx, OP_GET_SUPER, name);
	}
}

static void unary(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	TokenType operatorType = parser.previous.type;

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
	[TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
	[TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
	[TOKEN_LEFT_BRACKET]  = {array,    index_, PREC_SUBSCRIPT},
	[TOKEN_RIGHT_BRACKET] = {NULL,     NULL,   PREC_NONE},
	[TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
	[TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
	[TOKEN_PERCENT]       = {NULL,     binary, PREC_FACTOR},
	[TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
	[TOKEN_COLON]         = {NULL,     NULL,   PREC_NONE},
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
	[TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_CONTINUE]      = {NULL,     NULL,   PREC_NONE},
	[TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
	[TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
	[TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
	[TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
	[TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
	[TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
	[TOKEN_OR]            = {NULL,     or_,    PREC_OR},
	[TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
	[TOKEN_SUPER]         = {super_,   NULL,   PREC_NONE},
	[TOKEN_THIS]          = {this_,    NULL,   PREC_NONE},
	[TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
	[TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
	[TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
	[TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

static void parsePrecedence(VMCtx *vmCtx, Precedence precedence) {
	advance();
	ParseFn prefixRule = getRule(parser.previous.type)->prefix;
	if (prefixRule == NULL) {
		error("Expect expression.");
		return;
	}

	bool canAssign = (precedence <= PREC_ASSIGNMENT);
	prefixRule(vmCtx, canAssign);

	while (precedence <= getRule(parser.current.type)->precedence) {
		advance();
		ParseFn infixRule = getRule(parser.previous.type)->infix;
		infixRule(vmCtx, canAssign);
	}

	if (canAssign && match(TOKEN_EQUAL)) {
		error("Invalid assignment target.");
	}
}

static uint8_t identifierConstant(VMCtx *vmCtx, Token *name) {
	// See if we already have it.
	ObjString *string = copyString(vmCtx, name->start, name->length);
	Value indexValue;
	if (tableGet(&current->stringConstants, string, &indexValue)) {
		// We do.
		return (uint8_t)AS_NUMBER(indexValue);
	}

	uint8_t index = makeConstant(vmCtx, OBJ_VAL(string));
	tableSet(vmCtx, &current->stringConstants, string, NUMBER_VAL((double)index));
	return index;
}

static bool identifiersEqual(Token *a, Token *b) {
	if (a->length != b->length)
		return false;
	return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler *compiler, Token *name) {
	for (int i = compiler->localCount - 1; i >= 0; i--) {
		Local *local = &compiler->locals[i];
		if (identifiersEqual(name, &local->name)) {
			if (local->depth == -1) {
				error("Can't read local variable in its own initializer.");
			}
			return i;
		}
	}

	return -1;
}

static int addUpvalue(Compiler *compiler, uint8_t index, bool isLocal) {
	int upvalueCount = compiler->function->upvalueCount;

	for (int i = 0; i < upvalueCount; i++) {
		Upvalue *upvalue = &compiler->upvalues[i];
		if (upvalue->index == index && upvalue->isLocal == isLocal) {
			return i;
		}
	}

	if (upvalueCount == UINT8_COUNT) {
		error("Too many closure variables in function.");
		return 0;
	}

	compiler->upvalues[upvalueCount].isLocal = isLocal;
	compiler->upvalues[upvalueCount].index = index;
	return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Compiler *compiler, Token *name) {
	if (compiler->enclosing == NULL)
		return -1;

	int local = resolveLocal(compiler->enclosing, name);
	if (local != -1) {
			compiler->enclosing->locals[local].isCaptured = true;
			return addUpvalue(compiler, (uint8_t)local, true);
	}

	int upvalue = resolveUpvalue(compiler->enclosing, name);
	if (upvalue != -1) {
		return addUpvalue(compiler, (uint8_t)upvalue, false);
	}

	return -1;
}

static void addLocal(Token name) {
	if (current->localCount == UINT8_COUNT) {
		error("Too many local variables in function.");
		return;
	}

	Local* local = &current->locals[current->localCount++];
	local->name = name;
	local->depth = -1;
	local->isCaptured = false;
}

static void declareVariable() {
	if (current->scopeDepth == 0)
		return;

	Token *name = &parser.previous;
	for (int i = current->localCount - 1; i >= 0; i--) {
		Local *local = &current->locals[i];
		if (local->depth != -1 && local->depth < current->scopeDepth) {
			break;
		}

		if (identifiersEqual(name, &local->name)) {
			error("Already a variable with this name in this scope.");
		}
	}

	addLocal(*name);
}

static uint8_t parseVariable(VMCtx *vmCtx, const char *errorMessage) {
	consume(TOKEN_IDENTIFIER, errorMessage);

	declareVariable();
	if (current->scopeDepth > 0)
		return 0;

	return identifierConstant(vmCtx, &parser.previous);
}

static void markInitialized() {
	if (current->scopeDepth == 0)
		return;
	current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(VMCtx *vmCtx, uint8_t global) {
	if (current->scopeDepth > 0) {
		markInitialized();
		return;
	}

	emitBytes(vmCtx, OP_DEFINE_GLOBAL, global);
}

static uint8_t argumentList(VMCtx *vmCtx) {
	uint8_t argCount = 0;
	if (!check(TOKEN_RIGHT_PAREN)) {
		do {
			expression(vmCtx);
			if (argCount == 255) {
				error("Can't have more than 255 arguments.");
			}
			argCount++;
		} while (match(TOKEN_COMMA));
	}
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
	return argCount;
}

static void and_(VMCtx *vmCtx, bool canAssign SLOX_UNUSED) {
	int endJump = emitJump(vmCtx, OP_JUMP_IF_FALSE);

	emitByte(vmCtx, OP_POP);
	parsePrecedence(vmCtx, PREC_AND);

	patchJump(endJump);
}

static ParseRule* getRule(TokenType type) {
	return &parseRules[type];
}

static void expression(VMCtx *vmCtx) {
	parsePrecedence(vmCtx, PREC_ASSIGNMENT);
}

static void block(VMCtx *vmCtx) {
	while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
		declaration(vmCtx);
	}

	consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(VMCtx *vmCtx, FunctionType type) {
	Compiler compiler;
	initCompiler(vmCtx, &compiler, type);
	beginScope();

	consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
	if (!check(TOKEN_RIGHT_PAREN)) {
		do {
			current->function->arity++;
			if (current->function->arity > 255) {
				errorAtCurrent("Can't have more than 255 parameters.");
			}
			uint8_t constant = parseVariable(vmCtx, "Expect parameter name.");
			defineVariable(vmCtx, constant);
		} while (match(TOKEN_COMMA));
	}
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");

	consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
	block(vmCtx);

	ObjFunction *function = endCompiler(vmCtx);
	uint8_t functionConstant = makeConstant(vmCtx, OBJ_VAL(function));
	if (function->upvalueCount > 0) {
		emitBytes(vmCtx, OP_CLOSURE, functionConstant);

		// Emit arguments for each upvalue to know whether to capture a local or an upvalue.
		for (int i = 0; i < function->upvalueCount; i++) {
			emitByte(vmCtx, compiler.upvalues[i].isLocal ? 1 : 0);
			emitByte(vmCtx, compiler.upvalues[i].index);
		}
	} else {
		// No need to create a closure.
		emitBytes(vmCtx, OP_CONSTANT, functionConstant);
	}
}

static void method(VMCtx *vmCtx) {
	consume(TOKEN_IDENTIFIER, "Expect method name.");
	uint8_t constant = identifierConstant(vmCtx, &parser.previous);
	FunctionType type = TYPE_METHOD;
	if (parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0) {
		type = TYPE_INITIALIZER;
	}

	function(vmCtx, type);
	emitBytes(vmCtx, OP_METHOD, constant);
}

static void classDeclaration(VMCtx *vmCtx) {
	consume(TOKEN_IDENTIFIER, "Expect class name.");
	Token className = parser.previous;
	uint8_t nameConstant = identifierConstant(vmCtx, &parser.previous);
	declareVariable();

	emitBytes(vmCtx, OP_CLASS, nameConstant);
	defineVariable(vmCtx, nameConstant);

	ClassCompiler classCompiler;
	classCompiler.hasSuperclass = false;
	classCompiler.enclosing = currentClass;
	currentClass = &classCompiler;

	if (match(TOKEN_COLON)) {
		consume(TOKEN_IDENTIFIER, "Expect superclass name.");
		variable(vmCtx, false);

		if (identifiersEqual(&className, &parser.previous)) {
			error("A class can't inherit from itself.");
		}
	} else {
		Token rootObjName = syntheticToken("Object");
		emitBytes(vmCtx, OP_GET_GLOBAL, identifierConstant(vmCtx, &rootObjName));
	}

	beginScope();
	addLocal(syntheticToken("super"));
	defineVariable(vmCtx, 0);

	namedVariable(vmCtx, className, false);
	emitByte(vmCtx, OP_INHERIT);
	classCompiler.hasSuperclass = true;

	namedVariable(vmCtx, className, false);

	consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
	while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
		method(vmCtx);
	}
	consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
	emitByte(vmCtx, OP_POP);

	if (classCompiler.hasSuperclass) {
		endScope(vmCtx);
	}

	currentClass = currentClass->enclosing;
}

static void funDeclaration(VMCtx *vmCtx) {
	uint8_t global = parseVariable(vmCtx, "Expect function name.");
	markInitialized();
	function(vmCtx, TYPE_FUNCTION);
	defineVariable(vmCtx, global);
}

static void varDeclaration(VMCtx *vmCtx) {
	uint8_t global = parseVariable(vmCtx, "Expect variable name.");

	if (match(TOKEN_EQUAL)) {
		expression(vmCtx);
	} else {
		emitByte(vmCtx, OP_NIL);
	}
	consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

	defineVariable(vmCtx, global);
}

static void expressionStatement(VMCtx *vmCtx) {
	expression(vmCtx);
	consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
	emitByte(vmCtx, OP_POP);
}

static void breakStatement(VMCtx *vmCtx) {
	if (innermostLoopStart == -1) {
		error("Cannot use 'break' outside of a loop.");
	}

	consume(TOKEN_SEMICOLON, "Expect ';' after 'break'.");

	// Discard any locals created inside the loop.
	for (int i = current->localCount - 1;
		i >= 0 && current->locals[i].depth > innermostLoopScopeDepth;
		i--) {
			emitByte(vmCtx, OP_POP);
	}

	// Jump to the end of the loop
	// This needs to be patched when loop block is exited
	int jmp = emitJump(vmCtx, OP_JUMP);

	// Record jump for later patching
	BreakJump *breakJump = ALLOCATE(vmCtx, BreakJump, 1);
	breakJump->scopeDepth = innermostLoopScopeDepth;
	breakJump->offset = jmp;
	breakJump->next = breakJumps;
	breakJumps = breakJump;
}

static void continueStatement(VMCtx *vmCtx) {
	if (innermostLoopStart == -1) {
		error("Can't use 'continue' outside of a loop.");
	}

	consume(TOKEN_SEMICOLON, "Expect ';' after 'continue'.");

	// Discard any locals created inside the loop.
	for (int i = current->localCount - 1;
		 i >= 0 && current->locals[i].depth > innermostLoopScopeDepth;
		 i--) {
		emitByte(vmCtx, OP_POP);
	}

	// Jump to top of current innermost loop.
	emitLoop(vmCtx, innermostLoopStart);
}

static void forStatement(VMCtx *vmCtx) {
	beginScope();
	consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

	if (match(TOKEN_SEMICOLON)) {
		// No initializer.
	} else if (match(TOKEN_VAR)) {
		varDeclaration(vmCtx);
	} else {
		expressionStatement(vmCtx);
	}

	int surroundingLoopStart = innermostLoopStart;
	int surroundingLoopScopeDepth = innermostLoopScopeDepth;
	innermostLoopStart = currentChunk()->count;
	innermostLoopScopeDepth = current->scopeDepth;

	int exitJump = -1;
	if (!match(TOKEN_SEMICOLON)) {
		expression(vmCtx);
		consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

		// Jump out of the loop if the condition is false.
		exitJump = emitJump(vmCtx, OP_JUMP_IF_FALSE);
		emitByte(vmCtx, OP_POP); // Condition.
	}

	if (!match(TOKEN_RIGHT_PAREN)) {
		int bodyJump = emitJump(vmCtx, OP_JUMP);
		int incrementStart = currentChunk()->count;
		expression(vmCtx);
		emitByte(vmCtx, OP_POP);
		consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

		emitLoop(vmCtx, innermostLoopStart);
		innermostLoopStart = incrementStart;
		patchJump(bodyJump);
	}

	statement(vmCtx);
	emitLoop(vmCtx, innermostLoopStart);

	if (exitJump != -1) {
		patchJump(exitJump);
		emitByte(vmCtx, OP_POP); // Condition.
	}

	patchBreakJumps(vmCtx);

	innermostLoopStart = surroundingLoopStart;
	innermostLoopScopeDepth = surroundingLoopScopeDepth;

	endScope(vmCtx);
}

static void ifStatement(VMCtx *vmCtx) {
	consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
	expression(vmCtx);
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

	int thenJump = emitJump(vmCtx, OP_JUMP_IF_FALSE);
	emitByte(vmCtx, OP_POP);
	statement(vmCtx);

	int elseJump = emitJump(vmCtx, OP_JUMP);

	patchJump(thenJump);
	emitByte(vmCtx, OP_POP);

	if (match(TOKEN_ELSE))
		statement(vmCtx);
	patchJump(elseJump);
}

static void printStatement(VMCtx *vmCtx) {
	expression(vmCtx);
	consume(TOKEN_SEMICOLON, "Expect ';' after value.");
	emitByte(vmCtx, OP_PRINT);
}

static void returnStatement(VMCtx *vmCtx) {
	if (current->type == TYPE_SCRIPT) {
		error("Can't return from top-level code.");
	}

	if (match(TOKEN_SEMICOLON)) {
		emitReturn(vmCtx);
	} else {
		if (current->type == TYPE_INITIALIZER) {
			error("Can't return a value from an initializer.");
		}
		expression(vmCtx);
		consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
		emitByte(vmCtx, OP_RETURN);
	}
}

static void whileStatement(VMCtx *vmCtx) {
	int surroundingLoopStart = innermostLoopStart;
	int surroundingLoopScopeDepth = innermostLoopScopeDepth;
	innermostLoopStart = currentChunk()->count;
	innermostLoopScopeDepth = current->scopeDepth;

	consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
	expression(vmCtx);
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

	int exitJump = emitJump(vmCtx, OP_JUMP_IF_FALSE);
	emitByte(vmCtx, OP_POP);
	statement(vmCtx);

	emitLoop(vmCtx, innermostLoopStart);

	patchJump(exitJump);
	emitByte(vmCtx, OP_POP);

	patchBreakJumps(vmCtx);

	innermostLoopStart = surroundingLoopStart;
	innermostLoopScopeDepth = surroundingLoopScopeDepth;
}

static void synchronize() {
	parser.panicMode = false;

	while (parser.current.type != TOKEN_EOF) {
		if (parser.previous.type == TOKEN_SEMICOLON)
			return;
		switch (parser.current.type) {
			case TOKEN_CLASS:
			case TOKEN_FUN:
			case TOKEN_VAR:
			case TOKEN_FOR:
			case TOKEN_IF:
			case TOKEN_WHILE:
			case TOKEN_PRINT:
			case TOKEN_RETURN:
				return;
			default:
				; // Do nothing.
		}

		advance();
	}
}

static void declaration(VMCtx *vmCtx) {
	if (match(TOKEN_CLASS)) {
		classDeclaration(vmCtx);
	} else if (match(TOKEN_FUN)) {
		funDeclaration(vmCtx);
	} else if (match(TOKEN_VAR)) {
		varDeclaration(vmCtx);
	} else {
		statement(vmCtx);
	}

	if (parser.panicMode)
		synchronize();
}

static void statement(VMCtx *vmCtx) {
	if (match(TOKEN_BREAK)) {
		breakStatement(vmCtx);
	} else if (match(TOKEN_CONTINUE)) {
		continueStatement(vmCtx);
	} else if (match(TOKEN_PRINT)) {
		printStatement(vmCtx);
	} else if (match(TOKEN_FOR)) {
		forStatement(vmCtx);
	} else if (match(TOKEN_IF)) {
		ifStatement(vmCtx);
	} else if (match(TOKEN_RETURN)) {
		returnStatement(vmCtx);
	} else if (match(TOKEN_WHILE)) {
		whileStatement(vmCtx);
	} else if (match(TOKEN_LEFT_BRACE)) {
		beginScope();
		block(vmCtx);
		endScope(vmCtx);
	} else {
		expressionStatement(vmCtx);
	}
}

ObjFunction *compile(VMCtx *vmCtx, const char *source) {
	initScanner(source);

	Compiler compiler;
	initCompiler(vmCtx, &compiler, TYPE_SCRIPT);

	parser.hadError = false;
	parser.panicMode = false;

	advance();

	while (!match(TOKEN_EOF)) {
		declaration(vmCtx);
	}

	ObjFunction* function = endCompiler(vmCtx);

	return parser.hadError ? NULL : function;
}

void markCompilerRoots(VMCtx *vmCtx) {
	Compiler *compiler = current;
	while (compiler != NULL) {
		markObject(vmCtx, (Obj *)compiler->function);
		compiler = compiler->enclosing;
	}
}
