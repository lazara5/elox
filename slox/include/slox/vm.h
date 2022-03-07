#ifndef SLOX_VM_H
#define SLOX_VM_H

#include "slox/memory.h"
#include "slox/chunk.h"
#include "slox/object.h"
#include "slox/table.h"
#include "slox/value.h"
#include "slox/rand.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)
#define MAX_CATCH_HANDLER_FRAMES 16

typedef struct {
	Obj *function;
	uint8_t *ip;
	Value *slots;
	uint8_t handlerCount;
	uint16_t handlerStack[MAX_CATCH_HANDLER_FRAMES];
} CallFrame;

typedef struct {
	Chunk *chunk;
	uint8_t *ip;
	CallFrame frames[FRAMES_MAX];
	int frameCount;
	Value stack[STACK_MAX];
	Value *stackTop;
	Value *savedStackTop;
	bool handlingException;
	Table globals;
	Table strings;
	ObjUpvalue *openUpvalues;
	stc64_t prng;
// builtins
	ObjString *iteratorString;
	ObjString *hashCodeString;
	ObjClass *stringClass;
	ObjClass *exceptionClass;
	ObjClass *runtimeExceptionClass;
	ObjClass *arrayClass;
	ObjClass *mapClass;
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
InterpretResult interpret(VMCtx *vmCtx, char *source);
void push(VM *vm, Value value);
Value pop(VM *vm);
void popn(VM *vm, uint8_t n);

void defineNative(VMCtx *vmCtx, const char *name, NativeFn function);
Value runtimeError(VMCtx *vmCtx, const char *format, ...);

typedef struct ExecContext {
	bool error;
} ExecContext;

#define EXEC_CTX_INITIALIZER { \
	.error = false \
}

InterpretResult run(VMCtx *vmCtx, int exitFrame);
Value doCall(VMCtx *vmCtx, int argCount);

#endif // SLOX_VM_H
