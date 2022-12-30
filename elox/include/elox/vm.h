#ifndef ELOX_VM_H
#define ELOX_VM_H

#include "elox.h"
#include "elox/memory.h"
#include "elox/chunk.h"
#include "elox/object.h"
#include "elox/table.h"
#include "elox/handleSet.h"
#include "elox/function.h"
#include "elox/rand.h"
#include "elox/primegen.h"

typedef struct CompilerState CompilerState;

typedef struct {
	Chunk *chunk;
	uint8_t *ip;
	CallFrame frames[FRAMES_MAX];
	int frameCount;

	Value *stack;
	Value *stackTop;
	Value *stackTopMax;
	int stackCapacity;
	int handlingException;

	Table strings;
	ObjUpvalue *openUpvalues;
	stc64_t prng;
	PrimeGen primeGen;
// globals
	CloseTable globalNames;
	ValueArray globalValues;
// modules
	Table modules;
	Table builtinSymbols;
// builtins
	ObjString *iteratorString;
	ObjString *hasNextString;
	ObjString *nextString;

	ObjString *hashCodeString;
	ObjString *equalsString;
	ObjString *toStringString;
	ObjClass *stringClass;
	ObjClass *numberClass;
	ObjClass *exceptionClass;
	ObjClass *runtimeExceptionClass;
	ObjClass *arrayIteratorClass;
	uint16_t arrayIteratorArrayIndex;
	uint16_t arrayIteratorCurrentIndex;
	ObjClass *arrayClass;
	ObjClass *mapIteratorClass;
	uint16_t mapIteratorMapIndex;
	uint16_t mapIteratorCurrentIndex;
	uint16_t mapIteratorModCountIndex;
	ObjClass *mapClass;
	ObjClass *iteratorClass;
// handles
	HandleSet handles;
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

typedef struct VMCtx VMCtx;

void initVM(VMCtx *vmCtx);
void destroyVMCtx(VMCtx *vmCtx);
void pushCompilerState(VMCtx *vmCtx, CompilerState *compilerState);
void popCompilerState(VMCtx *vmCtx);
EloxInterpretResult interpret(VMCtx *vmCtx, char *source, const String *moduleName);
void push(VM *vm, Value value);
Value pop(VM *vm);
void popn(VM *vm, uint8_t n);
void pushn(VM *vm, uint8_t n);
Value peek(VM *vm, int distance);

#ifdef ELOX_DEBUG_TRACE_EXECUTION
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

EloxInterpretResult run(VMCtx *vmCtx, int exitFrame);
Value doCall(VMCtx *vmCtx, int argCount);
bool isCallable(Value val);
bool isFalsey(Value value);
Value toString(ExecContext *execCtx, Value value);

#endif // ELOX_VM_H
