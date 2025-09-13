#ifndef ELOX_COMPILER_H
#define ELOX_COMPILER_H

#include "elox/object.h"
#include "elox/scanner.h"
#include <elox/table.h>

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
	VAR_LOCAL,
	VAR_GLOBAL,
	VAR_BUILTIN,
	VAR_UPVALUE,
	VAR_TUPLE // for use inside unpack
} VarScope;

typedef struct {
	Token name;
	int16_t depth;
	bool postArgs;
	bool isCaptured;
} Local;

typedef struct {
	uint8_t index;
	bool postArgs;
	bool isLocal;
} Upvalue;

typedef enum {
	QUAL_ABSTRACT = 1 << 0,
	QUAL_GLOBAL = 1 << 1,
	QUAL_LOCAL = 1 << 2
} QualifierType;

typedef enum {
	QUAL_PENDING_SCOPE = 1 << 0,
	QUAL_PENDING_CLASS = 1 << 1
} QualPendingType;

typedef struct {
	uint32_t attrs;
	uint32_t pending;
} Qualifiers;

typedef struct MethodCompiler MethodCompiler;

typedef struct FunctionCompiler {
	struct FunctionCompiler *enclosing;
	MethodCompiler *methodCompiler;
	ObjFunction *function;
	FunctionType type;

	bool postArgs;
	bool hasVarargs;
	Local locals[UINT8_COUNT];
	int localCount;
	Upvalue upvalues[UINT8_COUNT];
	int16_t scopeDepth;
	Value defaultArgs[UINT8_COUNT];
	uint16_t numArgs;
	Qualifiers quals;

	uint32_t id;

	Table stringConstants;

	int catchStackDepth;
	int catchDepth;
	int finallyDepth;
} FunctionCompiler;

typedef struct KlassCompiler {
	struct KlassCompiler *enclosing;
	//Table pendingThisProperties;
	//Table pendingSuperProperties;
	bool hasExplicitInitializer;
} KlassCompiler;

#define MEMBER_FIELD  0x1
#define MEMBER_METHOD 0x2
#define MEMBER_ANY    0x3

typedef struct BreakJump {
	int scopeDepth;
	int offset;
	struct BreakJump *next;
} BreakJump;

typedef struct LoopCtx {
	int start;
	int16_t scopeDepth;
	int16_t catchStackDepth;
	int16_t finallyDepth;
} LoopCtx;

typedef struct MethodCompiler {
	ValueArray pendingRefs;
} MethodCompiler;

MethodCompiler *initMethodCompiler(MethodCompiler *mc);
void freeMethodCompiler(VMCtx *vmCtx, MethodCompiler *mc);

typedef struct CompilerState {
	ObjString *fileName;
	Parser parser;
	FunctionCompiler *currentFunctionCompiler;
	KlassCompiler *currentKlass;
	LoopCtx innermostLoop;
	BreakJump *breakJumps;
	int lambdaCount;
} CompilerState;

typedef struct ObjKlass ObjKlass;

bool initCompilerContext(CCtx *cCtx, RunCtx *runCtx, const String *fileName, const String *moduleName);
ObjFunction *compile(RunCtx *runCtx, uint8_t *source, const String *fileName, const String *moduleName);
Obj *compileFunction(RunCtx *runCtx, CCtx *cCtx, MethodCompiler *mc, ObjKlass *parentKlass, uint8_t *source, EloxError *error);
void markCompilerHandle(EloxHandle *handle);

Token syntheticToken(const uint8_t *text);
suint16_t identifierConstant(CCtx *cCtx, const String *name);
suint16_t globalIdentifierConstant(RunCtx *runCtx, const String *name, const String *moduleName);

void compileError(CCtx *cCtx, const char *message);

void chunkPatchUShort(Chunk *chunk, int offset, uint16_t val);

#endif // ELOX_COMPILER_H
