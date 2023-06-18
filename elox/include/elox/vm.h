// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_VM_H
#define ELOX_VM_H

#include <elox-config.h>
#include <elox.h>
#include "elox/memory.h"
#include "elox/chunk.h"
#include "elox/object.h"
#include "elox/table.h"
#include "elox/handleSet.h"
#include "elox/function.h"
#include <elox/third-party/rand.h>
#include <elox/third-party/primegen.h>

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
	ValueArray tmpStack;
	int handlingException;

	Table strings;
	ObjUpvalue *openUpvalues;
	stc64_t prng;
	PrimeGen primeGen;
// globals
	ValueTable globalNames;
	ValueArray globalValues;
// modules
	Table modules;
	Table builtinSymbols;

	struct {
		ObjString *anonInitString;

		ObjString *iteratorString;
		ObjString *hasNextString;
		ObjString *nextString;

		ObjString *hashCodeString;
		ObjString *equalsString;
		ObjString *toStringString;

		ObjClass *iteratorClass;

		ObjClass *stringClass;
		ObjNative *stringGsub;
		struct GmatchIterator {
			ObjClass *_class;
			uint16_t _string;
			uint16_t _pattern;
			uint16_t _offset;
			uint16_t _cachedNext;
		} gmatchIterator;

		ObjClass *numberClass;
		ObjClass *boolClass;
		ObjString *trueString;
		ObjString *falseString;
		ObjClass *instanceClass;
		ObjClass *classClass;
		ObjClass *exceptionClass;
		ObjClass *runtimeExceptionClass;

		struct ArrayIterator {
			ObjClass *_class;
			uint16_t _array;
			uint16_t _cursor; // next element to return
			uint16_t _lastRet; // last element returned
			uint16_t _modCount;
		} arrayIterator;
		ObjClass *arrayClass;

		ObjClass *tupleClass;

		struct MapIterator {
			ObjClass *_class;
			uint16_t _map;
			uint16_t _current;
			uint16_t _modCount;
		} mapIterator;
		ObjClass *mapClass;
	} builtins;
	ObjClass *classes[VTYPE_MAX];
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

//typedef struct VMCtx VMCtx;

void initVM(VMCtx *vmCtx);
void destroyVMCtx(VMCtx *vmCtx);
void pushCompilerState(VMCtx *vmCtx, CompilerState *compilerState);
void popCompilerState(VMCtx *vmCtx);
EloxInterpretResult interpret(VMCtx *vmCtx, uint8_t *source, const String *moduleName);

#define INLINE_STACK

#ifndef INLINE_STACK

void push(VM *vm, Value value);
Value pop(VM *vm);
void popn(VM *vm, uint8_t n);
void pushn(VM *vm, uint8_t n);
Value peek(VM *vm, int distance);

#else

static inline void push(VM *vm, Value value) {
	*vm->stackTop = value;
	vm->stackTop++;
}

static inline Value pop(VM *vm) {
	vm->stackTop--;
	return *vm->stackTop;
}

static inline void popn(VM *vm, uint8_t n) {
	vm->stackTop -= n;
}

static inline void pushn(VM *vm, uint8_t n) {
	vm->stackTop += n;
}

static inline Value peek(VM *vm, int distance) {
	return vm->stackTop[-1 - distance];
}

#endif // INLINE_STACK

#ifdef ELOX_DEBUG_TRACE_EXECUTION
void printStack(VMCtx *vmCtx);
#define DBG_PRINT_STACK(label, vmCtx) \
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "[" label "]"); printStack(vmCtx);
#else
#define DBG_PRINT_STACK(label, vm)
#endif

void registerNativeFunction(VMCtx *vmCtx, const String *name, const String *moduleName,
							NativeFn function, uint16_t arity, bool hasVarargs);

// Error handling

Value runtimeError(VMCtx *vmCtx, const char *format, ...) ELOX_PRINTF(2, 3);

typedef EloxError Error;

#define ERROR_INITIALIZER(VMCTX) { \
	.vmCtx = (VMCTX), \
	.raised = false \
}

#define ___ELOX_RAISE(error, fmt, ...) \
	if (!(error)->raised) { \
		(error)->raised = true; \
		runtimeError((error)->vmCtx, fmt, ## __VA_ARGS__); \
	}

#define ELOX_RAISE(error, fmt, ...) \
{ \
	___ELOX_RAISE(error, fmt, ## __VA_ARGS__) \
}

#define ELOX_RAISE_RET(error, fmt, ...) \
{ \
	___ELOX_RAISE(error, fmt, ## __VA_ARGS__) \
	return; \
}

#define ELOX_RAISE_RET_EXC(error, fmt, ...) \
{ \
	___ELOX_RAISE(error, fmt, ## __VA_ARGS__) \
	return EXCEPTION_VAL; \
}

#define ELOX_RAISE_RET_VAL(val, error, fmt, ...) \
{ \
	___ELOX_RAISE(error, fmt, ## __VA_ARGS__) \
	return (val); \
}

#define ELOX_RAISE_GOTO(label, error, fmt, ...) \
{ \
	ELOX_RAISE(error, fmt, ## __VA_ARGS__) \
	goto label; \
}

#define ELOX_IF_RAISED_RET_VAL(error, val) \
{ \
	if (ELOX_UNLIKELY(error->raised)) \
		return (val); \
}

#define ___ON_ERROR_RETURN return _error

#define ___ELOX_GET_ARG(var, args, idx, IS, AS, TYPE, ON_ERROR) \
	{ \
		Value val = getValueArg(args, idx); \
		if (ELOX_LIKELY(IS(val))) { \
			*(var) = AS(val); \
		} else { \
			Value _error = runtimeError(args->vmCtx, "Invalid argument type, expecting " #TYPE); \
			ON_ERROR; \
		} \
	}

#define ELOX_GET_STRING_ARG_ELSE_RET(var, args, idx) \
	___ELOX_GET_ARG(var, args, idx, IS_STRING, AS_STRING, string, ___ON_ERROR_RETURN)

#define ELOX_GET_NUMBER_ARG_ELSE_RET(var, args, idx) \
	___ELOX_GET_ARG(var, args, idx, IS_NUMBER, AS_NUMBER, number, ___ON_ERROR_RETURN)

int eloxPrintf(VMCtx *vmCtx, EloxIOStream stream, const char *format, ...) ELOX_PRINTF(3, 4);

int eloxVPrintf(VMCtx *vmCtx, EloxIOStream stream, const char *format, va_list args);

#define ELOX_WRITE(vmCtx, stream, string_literal) \
	(vmCtx)->write(stream, ELOX_STR_AND_LEN(string_literal))

bool setInstanceField(ObjInstance *instance, ObjString *name, Value value);

EloxInterpretResult run(VMCtx *vmCtx, int exitFrame);
Value runCall(VMCtx *vmCtx, int argCount);
bool runChunk(VMCtx *vmCtx);
bool isCallable(Value val);
bool isFalsey(Value value);
Value toString(Value value, Error *error);

#endif // ELOX_VM_H
