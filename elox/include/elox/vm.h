#ifndef ELOX_VM_H
#define ELOX_VM_H

#include "elox/memory.h"
#include "elox/chunk.h"
#include "elox/object.h"
#include "elox/table.h"
#include "elox/value.h"
#include "elox/rand.h"

#define FRAMES_MAX 64
#ifdef ENABLE_DYNAMIC_STACK
	#define MIN_STACK (4096)
#else
	#define MIN_STACK (FRAMES_MAX * UINT8_COUNT)
#endif
#define MAX_CATCH_HANDLER_FRAMES 16

typedef struct {
	uint16_t handlerTableOffset;
	uint16_t stackOffset;
} TryBlock;

typedef struct {
	Obj *function;
	uint8_t *ip;
	Value *slots;
	uint8_t handlerCount;
	TryBlock handlerStack[MAX_CATCH_HANDLER_FRAMES];
} CallFrame;

typedef struct CompilerState CompilerState;

typedef struct {
	Chunk *chunk;
	uint8_t *ip;
	CallFrame frames[FRAMES_MAX];
	int frameCount;

	Value *stack;
	Value *stackTop;
	Value *dummy[7];
	Value *stackTopMax;
	int handlingException;

	Table strings;
	ObjUpvalue *openUpvalues;
	stc64_t prng;
// globals
	ValueTable globalNames;
	ValueArray globalValues;
// modules
	Table modules;
	Table builtinSymbols;
// builtins
	ObjString *iteratorString;
	ObjString *hashCodeString;
	ObjString *equalsString;
	ObjString *toStringString;
	ObjClass *stringClass;
	ObjClass *numberClass;
	ObjClass *exceptionClass;
	ObjClass *runtimeExceptionClass;
	ObjClass *arrayClass;
	ObjClass *mapClass;
// compilers
	int compilerCount;
	int compilerCapacity;
	CompilerState **compilerStack;
// for GC
	size_t bytesAllocated;
	size_t nextGC;
	Obj *objects;
	int grayCount;
	int grayCapacity;
	Obj **grayStack;
} VM;

typedef enum {
	INTERPRET_OK,
	INTERPRET_COMPILE_ERROR,
	INTERPRET_RUNTIME_ERROR
} InterpretResult;

typedef struct VMCtx VMCtx;

void initVM(VMCtx *vmCtx);
void freeVM(VMCtx *vmCtx);
void pushCompilerState(VMCtx *vmCtx, CompilerState *compilerState);
void popCompilerState(VMCtx *vmCtx);
InterpretResult interpret(VMCtx *vmCtx, char *source, const String *moduleName);
void push(VMCtx *vmCtx, Value value);
Value pop(VM *vm);
void popn(VM *vm, uint8_t n);
Value peek(VM *vm, int distance);

#ifdef DEBUG_TRACE_EXECUTION
void printStack(VM *vm);
#define DBG_PRINT_STACK(label, vm) \
	printf("[" label "]"); printStack(vm);
#else
#define DBG_PRINT_STACK(label, vm)
#endif

void registerNativeFunction(VMCtx *vmCtx, const String *name, const String *moduleName,
							NativeFn function);
Value runtimeError(VMCtx *vmCtx, const char *format, ...) ELOX_PRINTF(2, 3);

bool setInstanceField(ObjInstance *instance, ObjString *name, Value value);

typedef struct ExecContext {
	VMCtx *vmCtx;
	bool error;
} ExecContext;

#define EXEC_CTX_INITIALIZER(VMCTX) { \
	.vmCtx = (VMCTX), \
	.error = false \
}

InterpretResult run(VMCtx *vmCtx, int exitFrame);
Value doCall(VMCtx *vmCtx, int argCount);
bool isFalsey(Value value);
Value toString(ExecContext *execCtx, Value value);

#endif // ELOX_VM_H
