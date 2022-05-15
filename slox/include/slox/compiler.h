#ifndef SLOX_COMPILER_H
#define SLOX_COMPILER_H

#include "slox/object.h"
#include "slox/scanner.h"

typedef struct {
	Token current;
	Token previous;
	Token beforePrevious;
	Token next;
	bool hasNext;
	bool hadError;
	bool panicMode;
} Parser;

typedef enum {
	TYPE_FUNCTION,
	TYPE_INITIALIZER,
	TYPE_METHOD,
	TYPE_LAMBDA,
	TYPE_SCRIPT
} FunctionType;

typedef enum {
	VAR_LOCAL,
	VAR_GLOBAL,
	VAR_UPVALUE
} VarType;

typedef struct {
	Token name;
	int depth;
	bool isCaptured;
} Local;

typedef struct {
	uint8_t index;
	bool isLocal;
} Upvalue;

typedef struct Compiler {
	struct Compiler *enclosing;
	ObjFunction *function;
	FunctionType type;

	Local locals[UINT8_COUNT];
	int localCount;
	Upvalue upvalues[UINT8_COUNT];
	int scopeDepth;

	Table stringConstants;

	int catchStackDepth;
} Compiler;

typedef struct ClassCompiler {
	struct ClassCompiler *enclosing;
	bool hasSuperclass;
	Table pendingThisProperties;
	Table pendingSuperProperties;
} ClassCompiler;

#define MEMBER_FIELD  0x1
#define MEMBER_METHOD 0x2
#define MEMBER_ANY    0x3

typedef struct BreakJump {
	int scopeDepth;
	int offset;
	struct BreakJump *next;
} BreakJump;

typedef struct {
	Parser parser;
	Compiler *current;
	ClassCompiler *currentClass;
	int innermostLoopStart;
	int innermostLoopScopeDepth;
	BreakJump *breakJumps;
	int lambdaCount;
} CompilerState;

void initCompilerState(VMCtx *vmCtx);
ObjFunction *compile(VMCtx *vmCtx, char *source);
void markCompilerRoots(VMCtx *vmCtx);

Token syntheticToken(const char *text);
uint16_t identifierConstant(VMCtx *vmCtx, Token *name);
uint16_t globalIdentifierConstant(VMCtx *vmCtx, Token *name);

#endif // SLOX_COMPILER_H
