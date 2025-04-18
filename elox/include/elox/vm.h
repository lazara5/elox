// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_VM_H
#define ELOX_VM_H

#include <elox-config.h>
#include <elox.h>
#include "elox/memory.h"
#include "elox/chunk.h"
#include <elox/object.h>
#include "elox/table.h"
#include "elox/handleSet.h"
#include <elox/third-party/rand.h>

typedef struct CompilerState CompilerState;
typedef struct ObjInterface ObjInterface;
typedef struct ObjInstance ObjInstance;

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

typedef enum {
	ELOX_FIBER_DETACHED,
	ELOX_FIBER_IDLE,
	ELOX_FIBER_RUNNING,
	ELOX_FIBER_WAITING,
	ELOX_FIBER_SUSPENDED,
	ELOX_FIBER_TERMINATED
} ELOX_PACKED FiberState;

extern const char *fiberStateNames[];

typedef struct ObjFiber {
	Obj obj;
#ifdef ELOX_SUPPORTS_PACKED
	FiberState state;
#else
	FiberState state : 8;
#endif
	ObjFunction *function;
	ObjClosure *closure;
	ObjCallFrame *activeFrame;
	uint32_t callDepth;
	Value *stack;
	_Alignas(64) Value *stackTop;
	Value *stackTopMax;
	int stackCapacity;

	ObjUpvalue *openUpvalues;

	VMTemp *temps;
	struct ObjFiber *parent;
	struct ObjFiber *prevSuspended;
	struct ObjFiber *nextSuspended;
} ObjFiber;

typedef struct VM {
	uint8_t *ip;

	int handlingException;

	Table strings;

	stc64_t prng;

	ObjFiber *initFiber;

	ObjCallFrame *freeFrames;
	TryBlock *freeTryBlocks;
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
			ObjClass *class_;
			struct {
				ObjString *name;
				ObjString *toString;
				ObjString *hashCode;
			} strings;
		} biObject;

		struct BIIterable {
			ObjInterface *intf;
			struct {
				ObjString *name;
				ObjString *iterator;
			} strings;
		} biIterable;

		struct BIIterator {
			ObjClass *class_;
			struct {
				ObjString *name;
				ObjString *next;
				ObjString *hasNext;
				ObjString *remove;
			} strings;
		} biIterator;

		struct BIString {
			ObjClass *class_;
			struct {
				ObjString *name;
				ObjString *length;
				ObjString *fmt;
				ObjString *find;
				ObjString *findMatch;
				ObjString *match;
				ObjString *gmatch;
				ObjString *gsub;
				ObjString *startsWith;
				ObjString *endsWith;
				ObjString *upper;
				ObjString *lower;
				ObjString *trim;
			} strings;
			struct {
				ObjNative *gsub;
			} methods;
		} biString;

		struct BIGmatchIterator {
			ObjClass *class_;
			struct {
				ObjString *name;
				ObjString *string;
				ObjString *pattern;
				ObjString *offset;
				ObjString *cachedNext;
			} strings;
			struct {
				uint16_t string;
				uint16_t pattern;
				uint16_t offset;
				uint16_t cachedNext;
			} fields;
		} biGmatchIterator;

		struct BINumber {
			ObjClass *class_;
			struct {
				ObjString *name;
			} strings;
		} biNumber;

		struct BIBool {
			ObjClass *class_;
			struct {
				ObjString *name;
			} strings;
		} biBool;

		struct BIInstance {
			ObjClass *class_;
			struct {
				ObjString *name;
			} strings;
		} biInstance;

		struct BIClass {
			ObjClass *class_;
			struct {
				ObjString *name;
			} strings;
		} biClass;

		struct BIStackTraceElement {
			ObjClass *class_;
			struct {
				ObjString *name;
				ObjString *fileName;
				ObjString *lineNumber;
				ObjString *functionName;
			} strings;
			struct {
				uint16_t fileName;
				uint16_t lineNumber;
				uint16_t functionName;
			} fields;
		} biStackTraceElement;

		struct BIThrowable {
			ObjClass *class_;
			struct {
				ObjString *name;
				ObjString *message;
			} strings;
		} biThrowable;

		struct BIException {
			ObjClass *class_;
			struct {
				ObjString *name;
				ObjString *stacktrace;
				ObjString *printStackTrace;
			} strings;
			struct {
				ObjNative *printStackTrace;
			} methods;
		} biException;

		struct BIRuntimeException {
			ObjClass *class_;
			struct {
				ObjString *name;
			} strings;
		} biRuntimeException;;

		struct BIError {
			ObjClass *class_;
			struct {
				ObjString *name;
			} strings;
		} biError;

		ObjInstance *oomError;
		ObjInstance *terminateError;

		struct BIArrayIterator {
			ObjClass *class_;
			struct {
				ObjString *name;
				ObjString *array;
				ObjString *cursor;
				ObjString *lastRet;
				ObjString *modCount;
			} strings;
			struct {
				uint16_t array;
				uint16_t cursor; // next element to return
				uint16_t lastRet; // last element returned
				uint16_t modCount;
			} fields;
		} biArrayIterator;

		struct BIArray {
			ObjClass *class_;
			struct {
				ObjString *name;
				ObjString *length;
				ObjString *add;
				ObjString *removeAt;
			} strings;
		} biArray;

		struct BITuple {
			ObjClass *class_;
			struct {
				ObjString *name;
				ObjString *length;
			} strings;
		} biTuple;

		struct BIMap {
			ObjInterface *intf;
			struct {
				ObjString *name;
				ObjString *size;
				ObjString *put;
				ObjString *remove;
			} strings;
		} biMap;

		struct BIHashMapIterator {
			ObjClass *class_;
			struct {
				ObjString *name;
				ObjString *map;
				ObjString *current;
				ObjString *modCount;
			} strings;
			struct {
				uint16_t map;
				uint16_t current;
				uint16_t modCount;
			} fields;
		} biHashMapIterator;

		struct BIHashMap {
			ObjClass *class_;
			struct {
				ObjString *name;
			} strings;
		} biHashMap;

		struct BIVarargsIterator {
			ObjClass *class_;
			struct {
				ObjString *name;
				ObjString *frame;
				ObjString *cursor;
			} strings;
			struct {
				uint16_t frame;
				uint16_t cursor; // next element to return
			} fields;
		} biVarargsIterator;

		struct BIVarargs {
			ObjClass *class_;
			struct {
				ObjString *name;
				ObjString *length;
			} strings;
		} biVarargs;

		struct BIFiber {
			ObjClass *class_;
			struct {
				ObjString *name;
				ObjString *resume;
				ObjString *resumeThrow;
				ObjString *yield;
				ObjString *getCurrent;
			} strings;
		} biFiber;

		struct {
			ObjString *true_;
			ObjString *false_;
		} strings;
	} builtins;
// modules
	Table modules;

	ObjClass *classes[VTYPE_MAX];
// handles
	HandleSet handles;
// compilers
	CompilerState *currentCompilerState;
// suspended fibers
	ObjFiber *suspendedHead;
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

ObjFiber *newFiber(RunCtx *runCtx, Value callable, EloxError *error);
void releaseFiberStack(RunCtx *runCtx, ObjFiber *fiber);
void destroyFiber(RunCtx *runCtx, ObjFiber *fiber);
void resumeFiber(RunCtx *runCtx, ObjFiber *fiber, ValueArray args, EloxError *error);
void resumeThrow(RunCtx *runCtx, ObjFiber *fiber, ObjInstance *throwable, EloxError *error);
void yieldFiber(RunCtx *runCtx, ValueArray args, EloxError *error);

EloxCompilerHandle *getCompiler(RunCtx *runCtx);
EloxInterpretResult interpret(RunCtx *runCtx, uint8_t *source, const String *fileName,
							  const String *moduleName);

static inline void push(ObjFiber *fiber, Value value) {
	*fiber->stackTop = value;
	fiber->stackTop++;
}

static inline Value pop(ObjFiber *fiber) {
	fiber->stackTop--;
	return *fiber->stackTop;
}

static inline void popn(ObjFiber *fiber, uint8_t n) {
	fiber->stackTop -= n;
}

static inline void pushn(ObjFiber *fiber, uint8_t n) {
	fiber->stackTop += n;
}

static inline Value peek(ObjFiber *fiber, int distance) {
	return fiber->stackTop[-1 - distance];
}

static inline size_t saveStack(ObjFiber *fiber) {
	return fiber->stackTop - fiber->stack;
}

static void restoreStack(ObjFiber *fiber, size_t saved) {
	fiber->stackTop = fiber->stack + saved;
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

Value runtimeError(RunCtx *runCtx, EloxError *error, const char *format, ...) ELOX_PRINTF(3, 4);
Value oomError(RunCtx *runCtx, EloxError *error);

#define ___ON_ERROR_RETURN return _error

#define ___ELOX_GET_ARG(var, args, idx, IS, AS, TYPE, ON_ERROR) \
	{ \
		Value val = getValueArg(args, idx); \
		if (ELOX_LIKELY(IS(val))) { \
			*(var) = AS(val); \
		} else { \
			Value _error = runtimeError(args->runCtx, NULL, "Argument %d: Invalid type, " #TYPE " expected" , idx); \
			ON_ERROR; \
		} \
	}

#define ELOX_GET_STRING_ARG_THROW_RET(var, args, idx) \
	___ELOX_GET_ARG(var, args, idx, IS_STRING, AS_STRING, string, ___ON_ERROR_RETURN)

#define ELOX_GET_NUMBER_ARG_THROW_RET(var, args, idx) \
	___ELOX_GET_ARG(var, args, idx, IS_NUMBER, AS_NUMBER, number, ___ON_ERROR_RETURN)

int eloxPrintf(RunCtx *runCtx, EloxIOStream stream, const char *format, ...) ELOX_PRINTF(3, 4);

int eloxVPrintf(RunCtx *runCtx, EloxIOStream stream, const char *format, va_list args);

#define ELOX_WRITE(runCtx, stream, string_literal) \
	(runCtx)->vmEnv->write(stream, ELOX_STR_AND_LEN(string_literal))

bool getInstanceValue(ObjInstance *instance, ObjString *name, Value *value);

bool setInstanceField(ObjInstance *instance, ObjString *name, Value value);

typedef struct {
	bool wasNative;
	bool result;
} ELOX_PACKED CallResult;

EloxInterpretResult run(RunCtx *runCtx);
Value runCall(RunCtx *runCtx, int argCount);
bool runChunk(RunCtx *runCtx);

ObjCallFrame *propagateException(RunCtx *runCtx);

CallResult callMethod(RunCtx *runCtx, Obj *callable, int argCount, uint8_t argOffset);
bool isCallable(Value val);
bool prototypeMatches(Obj *o1, Obj *o2);
bool isFalsey(Value value);
Value toString(RunCtx *runCtx, Value value, EloxError *error);

#endif // ELOX_VM_H
