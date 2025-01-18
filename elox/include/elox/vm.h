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

typedef struct CompilerState CompilerState;

typedef struct {
	Obj *objects;
	uint8_t initialMarkers;
} VMHeap;

typedef struct VMTemp {
	struct VMTemp *next;
	Value val;
#ifndef NDEBUG
	bool pushed;
#endif
} VMTemp;

typedef struct FiberCtx {
	CallFrame *activeFrame;
	uint32_t callDepth;
	Value *stack;
	_Alignas(64) Value *stackTop;
	Value *stackTopMax;
	int stackCapacity;

	ObjUpvalue *openUpvalues;

	VMTemp *temps;
} FiberCtx;

typedef struct VM {
	uint8_t *ip;

	int handlingException;

	Table strings;

	stc64_t prng;

	FiberCtx *initFiber;

	CallFrame *freeFrames;
// globals
	ValueTable globalNames;
	ValueArray globalValues;
// builtins
	Table builtinSymbols;
	ValueArray builtinValues;

	struct Builtins {
		ObjString *anonInitString;

		ObjString *equalsString;

		ObjString *scriptString;

		struct BIObject {
			ObjString *_nameStr;
			ObjClass *_class;
			ObjString *toStringStr;
			ObjString *hashCodeStr;
		} biObject;

		struct BIIterable {
			ObjString *_nameStr;
			ObjInterface *_intf;
			ObjString *iteratorStr;
		} biIterable;

		struct BIIterator {
			ObjString *_nameStr;
			ObjInterface *_intf;
			ObjString *nextStr;
			ObjString *hasNextStr;
			ObjString *removeStr;
		} biIterator;

		struct BIString {
			ObjString *_nameStr;
			ObjClass *_class;
			ObjString *lengthStr;
			ObjString *fmtStr;
			ObjString *findStr;
			ObjString *findMatchStr;
			ObjString *matchStr;
			ObjString *gmatchStr;
			ObjString *gsubStr;
			ObjString *startsWithStr;
			ObjString *endsWithStr;
			ObjString *upperStr;
			ObjString *lowerStr;
			ObjString *trimStr;
			ObjNative *_gsub;
		} biString;

		struct BIGmatchIterator {
			ObjString *_nameStr;
			ObjClass *_class;
			ObjString *stringStr;
			ObjString *patternStr;
			ObjString *offsetStr;
			ObjString *cachedNextStr;
			uint16_t _string;
			uint16_t _pattern;
			uint16_t _offset;
			uint16_t _cachedNext;
		} biGmatchIterator;

		struct BINumber {
			ObjString *_nameStr;
			ObjClass *_class;
		} biNumber;

		ObjString *trueStr;
		ObjString *falseStr;

		struct BIBool {
			ObjString *_nameStr;
			ObjClass *_class;
		} biBool;

		struct BIInstance {
			ObjString *_nameStr;
			ObjClass *_class;
		} biInstance;

		struct BIClass {
			ObjString *_nameStr;
			ObjClass *_class;
		} biClass;

		struct BIStackTraceElement {
			ObjString *_nameStr;
			ObjClass *_class;
			ObjString *fileNameStr;
			ObjString *lineNumberStr;
			ObjString *functionNameStr;
			uint16_t _fileName;
			uint16_t _lineNumber;
			uint16_t _functionName;
		} biStackTraceElement;

		struct BIThrowable {
			ObjString *_nameStr;
			ObjClass *_class;
			ObjString *messageStr;
		} biThrowable;

		struct BIException {
			ObjString *_nameStr;
			ObjClass *_class;
			ObjString *stacktraceStr;
			ObjString *printStackTraceString;
			ObjNative *_printStackTrace;
		} biException;

		struct BIRuntimeException {
			ObjString *_nameStr;
			ObjClass *_class;
		} biRuntimeException;;

		struct BIError {
			ObjString *_nameStr;
			ObjClass *_class;
		} biError;

		ObjString *oomErrorMsg;
		ObjInstance *oomError;

		struct BIArrayIterator {
			ObjString *_nameStr;
			ObjClass *_class;
			ObjString *arrayStr;
			ObjString *cursorStr;
			ObjString *lastRetStr;
			ObjString *modCountStr;
			uint16_t _array;
			uint16_t _cursor; // next element to return
			uint16_t _lastRet; // last element returned
			uint16_t _modCount;
		} biArrayIterator;

		struct BIArray {
			ObjString *_nameStr;
			ObjClass *_class;
			ObjString *lengthStr;
			ObjString *addStr;
			ObjString *removeAtStr;
		} biArray;

		struct BITuple {
			ObjString *_nameStr;
			ObjClass *_class;
			ObjString *lengthStr;
		} biTuple;

		struct BIMap {
			ObjString *_nameStr;
			ObjInterface *_intf;
			ObjString *sizeStr;
			ObjString *putStr;
			ObjString *removeStr;
		} biMap;

		struct BIHashMapIterator {
			ObjString *_nameStr;
			ObjClass *_class;
			ObjString *mapStr;
			ObjString *currentStr;
			ObjString *modCountStr;
			uint16_t _map;
			uint16_t _current;
			uint16_t _modCount;
		} biHashMapIterator;

		struct BIHashMap {
			ObjString *_nameStr;
			ObjClass *_class;

		} biHashMap;
	} builtins;
// modules
	Table modules;

	ObjClass *classes[VTYPE_MAX];
// handles
	HandleSet handles;
// compilers
	CompilerState *currentCompilerState;
// for GC
	VMHeap mainHeap;
	VMHeap permHeap;
	VMHeap *heap;
	size_t bytesAllocated;
	size_t nextGC;
	bool grayOverflow;
	int grayCount;
	int grayCapacity;
	Obj **grayStack;
} VM;

FiberCtx *newFiberCtx(RunCtx *runCtx);
void markFiberCtx(RunCtx *runCtx, FiberCtx *fiberCtx);
void destroyFiberCtx(RunCtx *runCtx, FiberCtx *fiberCtx);

void pushCompilerState(RunCtx *runCtx, CompilerState *compilerState);
void popCompilerState(RunCtx *runCtx);
EloxInterpretResult interpret(RunCtx *runCtx, uint8_t *source, const String *fileName,
							  const String *moduleName);

static inline void push(FiberCtx *fiberCtx, Value value) {
	*fiberCtx->stackTop = value;
	fiberCtx->stackTop++;
}

static inline Value pop(FiberCtx *fiberCtx) {
	fiberCtx->stackTop--;
	return *fiberCtx->stackTop;
}

static inline void popn(FiberCtx *fiberCtx, uint8_t n) {
	fiberCtx->stackTop -= n;
}

static inline void pushn(FiberCtx *fiberCtx, uint8_t n) {
	fiberCtx->stackTop += n;
}

static inline Value peek(FiberCtx *fiberCtx, int distance) {
	return fiberCtx->stackTop[-1 - distance];
}

static inline size_t saveStack(FiberCtx *fiberCtx) {
	return fiberCtx->stackTop - fiberCtx->stack;
}

static void restoreStack(FiberCtx *fiberCtx, size_t saved) {
	fiberCtx->stackTop = fiberCtx->stack + saved;
}

#ifdef ELOX_DEBUG_TRACE_EXECUTION
void printStack(RunCtx *runCtx);
#define DBG_PRINT_STACK(label, runCtx) \
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "[" label "]"); printStack(runCtx);
#else
#define DBG_PRINT_STACK(label, runCtx)
#endif

ObjNative *registerNativeFunction(RunCtx *runCtx, const String *name, const String *moduleName,
								  NativeFn function, uint16_t arity, bool hasVarargs);

// Error handling

Value runtimeError(RunCtx *runCtx, const char *format, ...) ELOX_PRINTF(2, 3);
Value oomError(RunCtx *runCtx);

#define ___ON_ERROR_RETURN return _error

#define ___ELOX_GET_ARG(var, args, idx, IS, AS, TYPE, ON_ERROR) \
	{ \
		Value val = getValueArg(args, idx); \
		if (ELOX_LIKELY(IS(val))) { \
			*(var) = AS(val); \
		} else { \
			Value _error = runtimeError(args->runCtx, "Invalid argument type, expecting " #TYPE); \
			ON_ERROR; \
		} \
	}

#define ELOX_GET_STRING_ARG_ELSE_RET(var, args, idx) \
	___ELOX_GET_ARG(var, args, idx, IS_STRING, AS_STRING, string, ___ON_ERROR_RETURN)

#define ELOX_GET_NUMBER_ARG_ELSE_RET(var, args, idx) \
	___ELOX_GET_ARG(var, args, idx, IS_NUMBER, AS_NUMBER, number, ___ON_ERROR_RETURN)

int eloxPrintf(RunCtx *runCtx, EloxIOStream stream, const char *format, ...) ELOX_PRINTF(3, 4);

int eloxVPrintf(RunCtx *runCtx, EloxIOStream stream, const char *format, va_list args);

#define ELOX_WRITE(runCtx, stream, string_literal) \
	(runCtx)->vmEnv->write(stream, ELOX_STR_AND_LEN(string_literal))

static inline bool getInstanceValue(ObjInstance *instance, ObjString *name, Value *value) {
	ObjClass *clazz = instance->clazz;
	Value valueIndex;
	if (tableGet(&clazz->fields, name, &valueIndex)) {
		int valueOffset = AS_NUMBER(valueIndex);
		*value = instance->fields.values[valueOffset];
		return true;
	}
	return false;
};

bool setInstanceField(ObjInstance *instance, ObjString *name, Value value);

EloxInterpretResult run(RunCtx *runCtx);
Value runCall(RunCtx *runCtx, int argCount);
bool runChunk(RunCtx *runCtx);
bool callMethod(RunCtx *runCtx, Obj *callable, int argCount, uint8_t argOffset, bool *wasNative);
bool isCallable(Value val);
bool isFalsey(Value value);
Value toString(RunCtx *runCtx, Value value, EloxError *error);

#endif // ELOX_VM_H
