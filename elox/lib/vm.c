// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/util.h"
#include "elox/compiler.h"
#include "elox/object.h"
#include "elox/memory.h"
#include "elox/state.h"
#include <elox/Class.h>
#include "elox/builtins.h"
#include <elox/debug.h>
#include <elox/builtins/string.h>
#include <elox.h>

#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <unistd.h>

const char *fiberStateNames[] = {
	[ELOX_FIBER_DETACHED] = "detached",
	[ELOX_FIBER_IDLE] = "idle",
	[ELOX_FIBER_RUNNING] = "running",
	[ELOX_FIBER_SUSPENDED] = "suspended",
	[ELOX_FIBER_TERMINATED] = "terminated"
};

static inline ObjFunction *getValueFunction(Value value) {
	assert(IS_OBJ(value));
	Obj *objVal = AS_OBJ(value);
	switch (getObjType(objVal)) {
		case OBJ_FUNCTION:
			return (ObjFunction *)AS_OBJ(value);
		case OBJ_CLOSURE:
			return ((ObjClosure *)AS_OBJ(value))->function;
		default:
			return NULL;
	}
}

static ELOX_FORCE_INLINE
ObjCallFrame *allocCallFrame(RunCtx *runCtx, ObjFiber *fiber) {
	VM *vm = runCtx->vm;

	ObjCallFrame *frame = vm->freeFrames;
	if (ELOX_LIKELY(frame != NULL))
		vm->freeFrames = (ObjCallFrame *)getObjNext(&frame->obj);
	else {
		frame = ALLOCATE(runCtx, ObjCallFrame, 1);
		if (ELOX_UNLIKELY(frame == NULL))
			return NULL;
		setObjType(&frame->obj, OBJ_FRAME);
	}

	frame->tryDepth = 0;
	frame->tryStack = NULL;

	setObjNext(&frame->obj, (Obj *)fiber->activeFrame);
	fiber->activeFrame = frame;
	fiber->callDepth++;

	return frame;
}

static ELOX_FORCE_INLINE
void releaseCallFrame(RunCtx *runCtx, ObjFiber *fiber) {
	VM *vm = runCtx->vm;

	assert(fiber->activeFrame != NULL);
	ObjCallFrame *frame = fiber->activeFrame;
	fiber->activeFrame = (ObjCallFrame *)getObjNext(&frame->obj);
	fiber->callDepth--;
	setObjNext(&frame->obj, (Obj *)vm->freeFrames);
	vm->freeFrames = frame;
}

ELOX_FORCE_INLINE
TryBlock *allocTryBlock(RunCtx *runCtx, ObjCallFrame *frame) {
	VM *vm = runCtx->vm;

	TryBlock *block = vm->freeTryBlocks;
	if (ELOX_LIKELY(block != NULL))
		vm->freeTryBlocks = block->prev;
	else {
		block = ALLOCATE(runCtx, TryBlock, 1);
		if (ELOX_UNLIKELY(block == NULL))
			return NULL;
	}

	block->prev = frame->tryStack;
	frame->tryStack = block;
	frame->tryDepth++;
	return block;
}

ELOX_FORCE_INLINE
void releaseTryBlock(RunCtx *runCtx, ObjCallFrame *frame) {
	VM *vm = runCtx->vm;

	assert(frame->tryStack != NULL);
	TryBlock *block = frame->tryStack;
	frame->tryStack = block->prev;
	frame->tryDepth--;
	block->prev = vm->freeTryBlocks;
	vm->freeTryBlocks = block;
}

ELOX_FORCE_INLINE
static int adjustArgs(ObjFiber *fiber, Value *defaultValues,
					  int argCount, uint16_t arity, uint16_t maxArgs,
					  int *missingArgs) {
	int stackArgs = argCount;

	if (argCount != arity) {
		if (argCount < arity) {
			*missingArgs = arity - argCount;
			for (int i = argCount; i < arity; i++) {
				push(fiber, defaultValues[i]);
				stackArgs++;
			}
		} else {
			if (argCount > maxArgs) {
				int extraArgs = argCount - maxArgs;
				stackArgs -= extraArgs;
				popn(fiber, extraArgs);
			}
		}
	}

	return stackArgs;
}

ELOX_FORCE_INLINE
static ObjCallFrame *setupStackFrame(RunCtx *runCtx, ObjFiber *fiber, Value *defaultValues,
							int argCount, uint16_t arity, uint16_t maxArgs, uint8_t argOffset) {
	ObjCallFrame *frame = allocCallFrame(runCtx, fiber);
	if (ELOX_UNLIKELY(frame == NULL))
		return NULL;

	int missingArgs = 0;
	int stackArgs = adjustArgs(fiber, defaultValues, argCount - argOffset, arity, maxArgs, &missingArgs);

	frame->slots = fiber->stackTop - stackArgs - 1;
	frame->fixedArgs = arity;
	frame->varArgs = argCount - argOffset + missingArgs - arity;
	frame->argOffset = argOffset;

	return frame;
}

ELOX_FORCE_INLINE
static ObjCallFrame *setupNativeStackFrame(RunCtx *runCtx, ObjFiber *fiber, Value *defaultValues,
										   int argCount, uint16_t arity, uint16_t maxArgs, uint8_t argOffset) {
	ObjCallFrame *frame = allocCallFrame(runCtx, fiber);
	if (ELOX_UNLIKELY(frame == NULL))
		return NULL;

	int missingArgs = 0;
	int stackArgs = adjustArgs(fiber, defaultValues, argCount, arity, maxArgs, &missingArgs);

	frame->slots = fiber->stackTop - stackArgs + argOffset;
	frame->argOffset = argOffset;
	frame->stackArgs = stackArgs;

	return frame;
}

static inline bool call(RunCtx *runCtx, ObjClosure *closure, ObjFunction *function,
						int argCount, uint8_t argOffset) {
	ObjFiber *fiber = runCtx->activeFiber;

DBG_PRINT_STACK("bsstk", runCtx);
	ObjCallFrame *frame = setupStackFrame(runCtx, fiber, function->defaultArgs, argCount,
										  function->arity - (function->isMethod ? 1 : 0),
										  function->maxArgs, argOffset);
	if (ELOX_UNLIKELY(frame == NULL)) {
		oomError(runCtx, NULL);
		return false;
	}
DBG_PRINT_STACK("asstk", runCtx);
	frame->type = ELOX_FT_INTER;
	frame->closure = closure;
	frame->function = function;
	frame->ip = function->chunk.code;

	return true;
}

static bool callClosure(RunCtx *runCtx, ObjClosure *closure, int argCount, uint8_t argOffset) {
	return call(runCtx, closure, closure->function, argCount, argOffset);
}

static bool callFunction(RunCtx *runCtx, ObjFunction *function, int argCount, uint8_t argOffset) {
	return call(runCtx, NULL, function, argCount, argOffset);
}

static bool callNative(RunCtx *runCtx, ObjNative *native,
					   int argCount, uint8_t argOffset, bool method) {
	ObjFiber *fiber = runCtx->activeFiber;

	// for native methods include 'this'
	ObjCallFrame *frame = setupNativeStackFrame(runCtx, fiber, native->defaultArgs,
												argCount + (uint16_t)method,
												native->arity, native->maxArgs, argOffset);
	if (ELOX_UNLIKELY(frame == NULL)) {
		oomError(runCtx, NULL);
		return false;
	}
	frame->type = ELOX_FT_INTER;
	frame->closure = NULL;
	frame->function = NULL;

#ifdef ELOX_DEBUG_TRACE_EXECUTION
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "<native>( %p --->", native);
	printStack(runCtx);
#endif

	uint16_t stackArgs = frame->stackArgs;

	Args args = { .runCtx = runCtx, .count = stackArgs, .frame = frame };
	Value result = native->function(&args);

	if (ELOX_LIKELY(!IS_EXCEPTION(result))) {
		releaseCallFrame(runCtx, fiber);
		fiber->stackTop -= (stackArgs + ((int)!method));
		push(fiber, result);
#ifdef ELOX_DEBUG_TRACE_EXECUTION
		ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "<native><---");
		printStack(runCtx);
#endif
		return true;
	}
#ifdef ELOX_DEBUG_TRACE_EXECUTION
		ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "<native><--- Exception!");
		printStack(runCtx);
#endif

	releaseCallFrame(runCtx, fiber);
	return false;
}

static bool callNativeClosure(RunCtx *runCtx, ObjNativeClosure *closure,
							  int argCount, uint8_t argOffset, bool method) {
	ObjFiber *fiber = runCtx->activeFiber;

	// for native methods include 'this'
	ObjCallFrame *frame = setupNativeStackFrame(runCtx, fiber, closure->defaultArgs,
												argCount + (uint16_t)method,
												closure->arity, closure->maxArgs, argOffset);
	if (ELOX_UNLIKELY(frame == NULL)) {
		oomError(runCtx, NULL);
		return false;
	}
	frame->type = ELOX_FT_INTER;

#ifdef ELOX_DEBUG_TRACE_EXECUTION
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "#native#--->");
	printStack(runCtx);
#endif

	uint16_t stackArgs = frame->stackArgs;

	NativeClosureFn native = closure->function;
	Args args = { .runCtx = runCtx, .count = stackArgs, .frame = frame };
	Value result = native(&args, closure->upvalueCount, closure->upvalues);
	if (ELOX_LIKELY(!IS_EXCEPTION(result))) {
		releaseCallFrame(runCtx, fiber);
		fiber->stackTop -= (stackArgs + ((int)!method));
		push(fiber, result);
#ifdef ELOX_DEBUG_TRACE_EXECUTION
		ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "#native#<---");
		printStack(runCtx);
#endif
		return true;
	}
#ifdef ELOX_DEBUG_TRACE_EXECUTION
		ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "#native#<--- Exception!");
		printStack(runCtx);
#endif

	releaseCallFrame(runCtx, fiber);
	return false;
}

CallResult callMethod(RunCtx *runCtx, Obj *callable, int argCount, uint8_t argOffset) {
	switch(getObjType(callable)) {
		case OBJ_FUNCTION:
			return (CallResult){ false,
				callFunction(runCtx, (ObjFunction *)callable, argCount, argOffset) };
		case OBJ_CLOSURE:
			return (CallResult){ false,
				callClosure(runCtx, (ObjClosure *)callable, argCount, argOffset) };
		case OBJ_NATIVE_CLOSURE:
			return (CallResult){ true,
				callNativeClosure(runCtx, (ObjNativeClosure *)callable, argCount, argOffset, true) };
		case OBJ_NATIVE:
			return (CallResult){ true,
				callNative(runCtx, ((ObjNative *)callable), argCount, argOffset, true) };
		default:
			runtimeError(runCtx, NULL, "Can only call functions and classes");
			return (CallResult){ false, false };
	}

	ELOX_UNREACHABLE();
}

static void printStackTrace(RunCtx *runCtx, EloxIOStream stream) {
	ObjFiber *fiber = runCtx->activeFiber;

	int frameNo = 0;
	for (ObjCallFrame *frame = fiber->activeFrame; frame != NULL;
		 frame = (ObjCallFrame *)getObjNext(&frame->obj)) {
		ObjFunction *function = frame->function;
		// -1 because the IP is sitting on the next instruction to be executed.
		size_t instruction = frame->ip - function->chunk.code - 1;
		uint32_t lineno = getLine(&function->chunk, instruction);
		eloxPrintf(runCtx, stream, "#%d [line %d] in %s()\n",
				   frameNo, lineno,
				   function->name == NULL ? "script" : (const char *)function->name->string.chars);
		frameNo++;
	}
}

Value oomError(RunCtx *runCtx, EloxError *error ELOX_UNUSED) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	push(fiber, OBJ_VAL(vm->builtins.oomError));
	return EXCEPTION_VAL;
}

void discardException(EloxFiber *fiber, size_t saved) {
	restoreStack(fiber, saved);
}

Value msgOomError(RunCtx *runCtx ELOX_UNUSED, EloxError *error) {
	EloxMsgError *msgError = (EloxMsgError *)error;
	strcpy(msgError->msg, "Out of memory");
	return EXCEPTION_VAL;
}

void msgDiscardException(EloxFiber *fiber ELOX_UNUSED, size_t saved ELOX_UNUSED) {

}

Value runtimeError(RunCtx *runCtx, EloxError *error ELOX_UNUSED, const char *format, ...) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	if (vm->handlingException) {
		eloxPrintf(runCtx, ELOX_IO_ERR, "Exception raised while handling exception: ");
		va_list args;
		va_start(args, format);
		eloxVPrintf(runCtx, ELOX_IO_ERR, format, args);
		va_end(args);
		ELOX_WRITE(runCtx, ELOX_IO_ERR, "\n\n");

		printStackTrace(runCtx, ELOX_IO_ERR);
		exit(1);
	}

	vm->handlingException++;

	HeapCString msg;
	initHeapStringWithSize(runCtx, &msg, 16);
	va_list args;
	va_start(args, format);
	heapStringAddVFmt(runCtx, &msg, format, args);
	va_end(args);

	Obj *errorInst = newInstance(runCtx, vm->builtins.biRuntimeException.class_);
	// TODO: check
	push(fiber, OBJ_VAL(errorInst));
	ObjString *msgObj = takeString(runCtx, msg.chars, msg.length, msg.capacity);
	// TODO: check
	push(fiber, OBJ_VAL(msgObj));
	callMethod(runCtx, AS_OBJ(vm->builtins.biRuntimeException.class_->initializer), 1, 0);
	pop(fiber);
	push(fiber, OBJ_VAL(errorInst));

	vm->handlingException--;
	return EXCEPTION_VAL;
}

Value msgRuntimeError(RunCtx *runCtx ELOX_UNUSED, EloxError *error, const char *format, ...) {
	EloxMsgError *msgError = (EloxMsgError *)error;

	va_list args;
	va_start(args, format);
	vsnprintf(msgError->msg, sizeof(msgError->msg), format, args);
	va_end(args);

	return EXCEPTION_VAL;
}

void ensureStack(RunCtx *runCtx, ObjFiber *fiber, int required) {
	if (ELOX_UNLIKELY(required > fiber->stackCapacity)) {
		int oldCapacity = fiber->stackTopMax - fiber->stack + 1;
		int newCapacity = GROW_CAPACITY(oldCapacity);
		Value *oldStack = fiber->stack;

		fiber->stack = GROW_ARRAY(runCtx, Value, fiber->stack, oldCapacity, newCapacity);
		if (ELOX_UNLIKELY(fiber->stack == NULL)) {
			// TODO: ?
			exit(1);
		}
		fiber->stackTop = fiber->stack + oldCapacity - 1;
		fiber->stackTopMax = fiber->stack + newCapacity -1;
		fiber->stackCapacity = newCapacity;

		if (oldStack != fiber->stack) {
			// the stack moved, recalculate all pointers that point to the old stack

			for (ObjCallFrame *frame = fiber->activeFrame; frame != NULL;
				 frame = (ObjCallFrame *)getObjNext(&frame->obj))
				frame->slots = fiber->stack + (frame->slots - oldStack);

			for (ObjUpvalue *upvalue = fiber->openUpvalues; upvalue != NULL; upvalue = upvalue->next)
				upvalue->location = fiber->stack + (upvalue->location - oldStack);
		}
	}
}

static bool extractPrototype(Obj *obj, Prototype *proto) {
	switch (getObjType(obj)) {
		case OBJ_FUNCTION: {
			ObjFunction *f = (ObjFunction *)obj;
			proto->arity = f->arity;
			proto->hasVarargs = (f->maxArgs == ELOX_MAX_ARGS);
			return true;
		}
		case OBJ_CLOSURE: {
			ObjClosure *c = (ObjClosure *)obj;
			proto->arity = c->function->arity;
			proto->hasVarargs = (c->function->maxArgs == ELOX_MAX_ARGS);
			return true;
		}
		case OBJ_NATIVE_CLOSURE: {
			ObjNativeClosure *nc = (ObjNativeClosure *)obj;
			proto->arity = nc->arity;
			proto->hasVarargs = (nc->maxArgs == ELOX_MAX_ARGS);
			return true;
		}
		case OBJ_NATIVE: {
			ObjNative *n = (ObjNative *)obj;
			proto->arity = n->arity;
			proto->hasVarargs = (n->maxArgs == ELOX_MAX_ARGS);
			return true;
		}
		case OBJ_ABSTRACT_METHOD: {
			ObjMethod *m = (ObjMethod *)obj;
			*proto = m->abstract.proto;
			return true;
		}
		case OBJ_DEFAULT_METHOD: {
			ObjDefaultMethod *m = (ObjDefaultMethod *)obj;
			proto->arity = m->function->arity;
			proto->hasVarargs = (m->function->maxArgs == ELOX_MAX_ARGS);
			return true;
		}
		case OBJ_METHOD: {
			ObjMethod *m = (ObjMethod *)obj;
			return extractPrototype(m->method.callable, proto);
		}
		default:
			return false;
	}
}

bool prototypeMatches(Obj *o1, Obj *o2) {
	Prototype o1Proto;
	Prototype o2Proto;

	if (!extractPrototype(o1, &o1Proto))
		return false;
	if (!extractPrototype(o2, &o2Proto))
		return false;

	return (o1Proto.arity == o2Proto.arity) && (o1Proto.hasVarargs == o2Proto.hasVarargs);
}

static uint16_t _chkreadu16tmp;
static int32_t _chkreadi32tmp;
#define CHUNK_READ_BYTE(PTR) (*PTR++)
#define CHUNK_READ_USHORT(PTR) \
	(memcpy(&_chkreadu16tmp, PTR, sizeof(uint16_t)), \
	 PTR += sizeof(uint16_t), \
	 _chkreadu16tmp)
#define CHUNK_READ_INT32(PTR) \
	(memcpy(&_chkreadi32tmp, PTR, sizeof(int32_t)), \
	 PTR += sizeof(int32_t), \
	 _chkreadi32tmp)
#define CHUNK_READ_STRING16(PTR, FRAME) \
	(memcpy(&_chkreadu16tmp, PTR, sizeof(uint16_t)), \
	 PTR += sizeof(uint16_t), \
	 AS_STRING(FRAME->function->chunk.constants.values[_chkreadu16tmp]))
#define CHUNK_READ_CONST16(PTR, FRAME) \
	(memcpy(&_chkreadu16tmp, PTR, sizeof(uint16_t)), \
	 PTR += sizeof(uint16_t), \
	 FRAME->function->chunk.constants.values[_chkreadu16tmp])

static unsigned int inherit(RunCtx *runCtx, uint8_t *ip, EloxError *error) {
	ObjFiber *fiber = runCtx->activeFiber;

	uint8_t *ptr = ip;

	uint8_t numIntf = CHUNK_READ_BYTE(ptr) - 1;

	Value superclassVal = peek(fiber, numIntf + 1);
	ELOX_CHECK_RAISE_RET_VAL(isObjType(superclassVal, OBJ_CLASS), error,
							 RTERR(runCtx, "Superclass must be a class"), ptr - ip);

	Obj *supertypes[ELOX_MAX_SUPERTYPES];
	uint16_t numSupertypes = 0;

	ObjClass *clazz = (ObjClass *)AS_OBJ(peek(fiber, numIntf + 2));
	ObjClass *super = (ObjClass *)AS_OBJ(superclassVal);

	for (int i = 0; i < super->props.capacity; i++) {
		PropEntry *entry = &super->props.entries[i];
		if (entry->key != NULL) {
			if (entry->value.type == ELOX_PROP_FIELD) {
				propTableSet(runCtx, &clazz->props, entry->key, entry->value, error);
				if (ELOX_UNLIKELY(error->raised))
					return (ptr - ip);
				clazz->numFields++;
			} else {
				bool set = valueArraySet(runCtx, &clazz->classData, entry->value.index,
										 super->classData.values[entry->value.index]);
				ELOX_CHECK_RAISE_RET_VAL(set, error, OOM(runCtx), ptr - ip);
				clazz->tables[ELOX_DT_CLASS] = clazz->classData.values;
				propTableSet(runCtx, &clazz->props, entry->key, entry->value, error);
				if (ELOX_UNLIKELY(error->raised))
					return (ptr - ip);
			}
		}
	}

	clazz->super = (ObjClass *)AS_OBJ(superclassVal);
	clazz->tables[ELOX_DT_SUPER] = super->classData.values;

	uint8_t typeDepth = super->typeInfo.depth + 1;
	clazz->typeInfo.depth = typeDepth;
	uint8_t superDisplaySize = ELOX_MAX(typeDepth, ELOX_CLASS_DISPLAY_SIZE);
	memcpy(clazz->typeInfo.rptDisplay, super->typeInfo.rptDisplay,
		   superDisplaySize * sizeof(Obj *));
	bool restricted = typeDepth >= ELOX_CLASS_DISPLAY_SIZE;
	bool superRestricted = super->typeInfo.depth >= ELOX_CLASS_DISPLAY_SIZE;
	if (!restricted) {
		clazz->typeInfo.rptDisplay[typeDepth] = (Obj *)clazz;
		clazz->typeCheckOffset = typeDepth;
	} else
		clazz->typeCheckOffset = ELOX_CLASS_DISPLAY_SIZE;
	if (superRestricted) {
		supertypes[numSupertypes] = (Obj *)super;
		numSupertypes++;
	}
	for (uint16_t s = 0; s < super->typeInfo.numRss; s++) {
		supertypes[numSupertypes] = super->typeInfo.rssList[s];
		numSupertypes++;
		// TODO: check
	}

	for (uint8_t i = 0; i < numIntf; i++) {
		Value intfVal = peek(fiber, numIntf - i);
		if (ELOX_UNLIKELY(!isObjType(intfVal, OBJ_INTERFACE)))
			ELOX_RAISE_RET_VAL(error, RTERR(runCtx, "Can only implement interfaces"), ptr - ip);

		ObjInterface *intf = (ObjInterface *)AS_OBJ(intfVal);

		bool duplicate = false;
		for (uint16_t s = 0; s < numSupertypes; s++) {
			if ((Obj *)intf == supertypes[s]) {
				duplicate = true;
				break;
			}
		}

		if (!duplicate) {
			supertypes[numSupertypes] = (Obj *)intf;
			numSupertypes++;

			for (int im = 0; im < intf->methods.capacity; im++) {
				Entry *entry = &intf->methods.entries[im];
				ObjString *methodName = entry->key;
				if (methodName != NULL) {
					Obj *intfMethodObj = AS_OBJ(entry->value);
					PropInfo propInfo = propTableGetAny(&clazz->props, methodName);
					int methodIndex = -1;
					ObjMethod *classMethod = NULL;
					ObjMethod *methodToAdd = NULL;

					if (propInfo.type != ELOX_PROP_NONE) {
						if (ELOX_UNLIKELY(propInfo.type != ELOX_PROP_METHOD)) {
							ELOX_RAISE_RET_VAL(error, RTERR(runCtx, "Property %.*s shadows interface method",
											   methodName->string.length, methodName->string.chars),
											   ptr - ip);
						}
						Value methodVal = clazz->classData.values[propInfo.index];
						classMethod = (ObjMethod *)AS_OBJ(methodVal);

						if (!prototypeMatches((Obj *)classMethod, intfMethodObj)) {
							ELOX_RAISE_RET_VAL(error, RTERR(runCtx, "Interface Method %.*s mismatch",
											   methodName->string.length, methodName->string.chars),
											   ptr - ip);
						}

						if (getObjType(intfMethodObj) == OBJ_DEFAULT_METHOD) {
							ObjDefaultMethod *intfDefaultMethod = (ObjDefaultMethod *)intfMethodObj;
							switch (getObjType(&classMethod->obj)) {
								case OBJ_METHOD: {
									if ((classMethod->method.fromDefault != NULL) &&
										(classMethod->method.fromDefault != intfDefaultMethod))
										classMethod->isConflicted = true;
									break;
								}
								case OBJ_ABSTRACT_METHOD: {
									methodToAdd = newPendingMethod(runCtx, (ObjDefaultMethod *)intfMethodObj);
									ELOX_CHECK_RAISE_RET_VAL(methodToAdd != NULL, error, OOM(runCtx), ptr - ip);
									methodIndex = propInfo.index;
									break;
								}
								default:
									ELOX_UNREACHABLE();
									assert(false);
							}
						}
					} else {
						if (getObjType(intfMethodObj) == OBJ_ABSTRACT_METHOD)
							methodToAdd = (ObjMethod *)intfMethodObj;
						else {
							methodToAdd = newPendingMethod(runCtx, (ObjDefaultMethod *)intfMethodObj);
							ELOX_CHECK_RAISE_RET_VAL(methodToAdd != NULL, error, OOM(runCtx), ptr - ip);
						}
					}

					if (methodIndex < 0)
						methodIndex = clazz->classData.count;

					if (methodToAdd != NULL) {
						TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
						Value methodVal = OBJ_VAL(methodToAdd);
						PUSH_TEMP(temps, protectedMethod, methodVal);
						methodIndex = setClassData(runCtx, clazz, methodIndex, methodVal);
						releaseTemps(&temps);
						ELOX_CHECK_RAISE_RET_VAL(methodIndex >= 0, error, OOM(runCtx), ptr - ip);
						propTableSet(runCtx, &clazz->props, methodName,
									 (PropInfo){ methodIndex, ELOX_PROP_METHOD, ELOX_PROP_METHOD_MASK }, error);
						if (ELOX_UNLIKELY(error->raised))
							return (ptr - ip);
					}
				}
			}
		}
	}

	if (numSupertypes > 0) {
		clazz->typeInfo.rssList = ALLOCATE(runCtx, Obj *, numSupertypes);
		ELOX_CHECK_RAISE_RET_VAL((clazz->typeInfo.rssList != NULL), error, OOM(runCtx), ptr - ip);
		clazz->typeInfo.numRss = numSupertypes;
		memcpy(clazz->typeInfo.rssList, supertypes, numSupertypes * sizeof(Obj *));
	}

	popn(fiber, numIntf + 1); // interfaces and subclass

	return (ptr - ip);
}

static unsigned int defineDefaultMethod(RunCtx *runCtx, ObjCallFrame *frame, EloxError *error) {
	ObjFiber *fiber = runCtx->activeFiber;
	uint8_t *ip = frame->ip;

	uint8_t *ptr = ip;

	ObjString *name = CHUNK_READ_STRING16(ptr, frame);
	ObjFunction *function = (ObjFunction *)AS_OBJ(CHUNK_READ_CONST16(ptr, frame));

	ObjInterface *intf = (ObjInterface *)AS_OBJ(peek(fiber, 0));

	ObjDefaultMethod *method = newDefaultMethod(runCtx, function);
	ELOX_CHECK_RAISE_RET_VAL((method != NULL), error, OOM(runCtx), ptr - ip);
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	PUSH_TEMP(temps, protectedMethod, OBJ_VAL(method));

	Value existingMethod;
	bool methodExists = tableGet(&intf->methods, name, &existingMethod);
	if (ELOX_UNLIKELY(methodExists)) {
		ELOX_RAISE_GOTO(error, RTERR(runCtx, "Method %.*s already defined",
									 name->string.length, name->string.chars),
						cleanup);
	}

	uint16_t numRefs = CHUNK_READ_USHORT(ptr);
	method->refs = ALLOCATE(runCtx, RefBindDesc, numRefs);
	ELOX_CHECK_RAISE_GOTO((method->refs != NULL), error, OOM(runCtx), cleanup);
	method->numRefs = numRefs;
	for (int i = 0; i < numRefs; i++) {
		int32_t offset = CHUNK_READ_INT32(ptr);
		uint8_t slotType = CHUNK_READ_BYTE(ptr);
		uint16_t nameHandle = CHUNK_READ_USHORT(ptr);
		method->refs[i] = (RefBindDesc){ offset, slotType, nameHandle };
	}

	// no need to check for now, we return after this anyway
	tableSet(runCtx, &intf->methods, name, OBJ_VAL(method), error);

cleanup:
	releaseTemps(&temps);

	return ptr - ip;
}

static unsigned int defineMethod(RunCtx *runCtx, ObjCallFrame *frame, EloxError *error) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;
	uint8_t *ip = frame->ip;

	uint8_t *ptr = ip;

	ObjString *methodName = CHUNK_READ_STRING16(ptr, frame);
	uint16_t functionHandle ELOX_UNUSED = CHUNK_READ_USHORT(ptr); // function constant, ignore

	Value methodCallable = peek(fiber, 0);
	ObjClass *clazz = (ObjClass *)AS_OBJ(peek(fiber, 2));
	ObjFunction *methodFunction = getValueFunction(methodCallable);
	methodFunction->parentClass = clazz;
	ObjClass *super = clazz->super;
	methodFunction->refOffset = super->numRefs;

	if ((methodName == clazz->name) || (methodName == vm->builtins.anonInitString))
		clazz->initializer = methodCallable;
	else {
		ObjMethod *method = newMethod(runCtx, (ObjKlass *)clazz, AS_OBJ(methodCallable));
		ELOX_CHECK_RAISE_RET_VAL((method != NULL), error, OOM(runCtx), ptr - ip);
		TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
		PUSH_TEMP(temps, protectedMethod, OBJ_VAL(method));
		PropInfo existingPropInfo = propTableGetAny(&clazz->props, methodName);
		Value existingMethod;
		bool methodExists = false;
		if (existingPropInfo.type != ELOX_PROP_NONE) {
			methodExists = true;
			if (existingPropInfo.type != ELOX_PROP_METHOD) {
				releaseTemps(&temps);
				ELOX_RAISE_RET_VAL(error, RTERR(runCtx, "Method %.*s shadows existing property",
												methodName->string.length, methodName->string.chars),
								   ptr - ip);
			} else
				existingMethod = clazz->classData.values[existingPropInfo.index];
		}
		if (methodExists) {
			if (!prototypeMatches(method->method.callable, AS_OBJ(existingMethod))) {
				releaseTemps(&temps);
				ELOX_RAISE_RET_VAL(error, RTERR(runCtx, "Method %.*s overrides incompatible method",
												methodName->string.length, methodName->string.chars),
							   ptr - ip);
			}
		}
		if (methodExists) {
			uint32_t methodIndex = existingPropInfo.index;
			clazz->classData.values[methodIndex] = OBJ_VAL(method);
			releaseTemps(&temps);
		} else {
			int methodIndex = pushClassData(runCtx, clazz, OBJ_VAL(method));
			if (ELOX_UNLIKELY(methodIndex < 0)) {
				releaseTemps(&temps);
				ELOX_RAISE_RET_VAL(error, OOM(runCtx), ptr - ip);
			}
			propTableSet(runCtx, &clazz->props, methodName,
						 (PropInfo){methodIndex, ELOX_PROP_METHOD, ELOX_PROP_METHOD_MASK}, error);
			releaseTemps(&temps);
			if (ELOX_UNLIKELY(error->raised))
				return ptr - ip;
		}

		if (methodName == vm->builtins.biObject.strings.hashCode)
			clazz->hashCode = method;
		else if (methodName == vm->builtins.equalsString)
			clazz->equals = method;
	}

	OpenKlass *openKlass = clazz->openKlass;

	uint16_t numRefs = CHUNK_READ_USHORT(ptr);
	for (int i = 0; i < numRefs; i++) {
		int32_t offset = CHUNK_READ_INT32(ptr);
		uint8_t slotType = CHUNK_READ_BYTE(ptr);
		uint16_t nameHandle = CHUNK_READ_USHORT(ptr);

		ObjString *refName = AS_STRING(methodFunction->chunk.constants.values[nameHandle]);

		bool isSuper = slotType & 0x1;
		uint8_t propType = (slotType & 0x6) >> 1;

		Table *table = isSuper ? &openKlass->pendingSuper : &openKlass->pendingThis;
		int slot = openKlass->numRefs;
		uint64_t actualSlot = AS_NUMBER(tableSetIfMissing(runCtx, table, refName,
														  NUMBER_VAL(slot | propType << 24), error));
		actualSlot &= 0xFFFFFF;
		if (actualSlot + 1 > openKlass->numRefs)
			openKlass->numRefs = actualSlot + 1;

		chunkPatchUShort(&methodFunction->chunk, offset, actualSlot);
	}

//#ifdef ELOX_DEBUG_TRACE_EXECUTION
#if 0
	if (numRefs > 0) {
		disassembleChunk(runCtx, &methodFunction->chunk,
						 (const char *)methodName->string.chars);
	}
#endif

	pop(fiber);

	return ptr - ip;
}

static void defineField(RunCtx *runCtx, ObjString *name, EloxError *error) {
	ObjFiber *fiber = runCtx->activeFiber;

	ObjClass *clazz = (ObjClass *)AS_OBJ(peek(fiber, 1));
	int index = clazz->numFields;
	// no need to check for now, we return after this anyway
	bool isNewField = propTableSet(runCtx, &clazz->props, name,
								   (PropInfo){index, ELOX_PROP_FIELD, ELOX_PROP_FIELD_MASK }, error);
	if (isNewField)
		clazz->numFields++;
}

static void defineStatic(RunCtx *runCtx, ObjString *name, EloxError *error) {
	ObjFiber *fiber = runCtx->activeFiber;

	ObjClass *clazz = (ObjClass *)AS_OBJ(peek(fiber, 2));

	PropInfo propInfo = propTableGetAny(&clazz->props, name);
	if (propInfo.type != ELOX_PROP_NONE) {
		if (ELOX_UNLIKELY(propInfo.type != ELOX_PROP_STATIC))
			ELOX_RAISE_RET(error, RTERR(runCtx, "Static %.*s shadows existing property",
										name->string.length, name->string.chars));
	}
	if (propInfo.type == ELOX_PROP_NONE) {
		int staticIndex = pushClassData(runCtx, clazz, peek(fiber, 0));
		ELOX_CHECK_RAISE_RET(staticIndex >= 0, error, OOM(runCtx));
		propTableSet(runCtx, &clazz->props, name,
					 (PropInfo){staticIndex, ELOX_PROP_STATIC, ELOX_PROP_STATIC_MASK }, error);
		if (ELOX_UNLIKELY(error->raised))
			return;
	} else {
		uint32_t staticIndex = propInfo.index;
		clazz->classData.values[staticIndex] = peek(fiber, 0);
	}

	// do not pop the static from the stack, it is saved into a local and
	// will be automatically discarded at the end of the scope
}

ObjNative *registerNativeFunction(RunCtx *runCtx,
								  const String *name, const String *moduleName,
								  NativeFn function, uint16_t arity, bool hasVarargs) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	ObjNative *ret = NULL;

	bool isBuiltin = stringEquals(moduleName, &eloxBuiltinModule);
	ObjNative *native = newNative(runCtx, function, arity);
	ELOX_CHECK_RET_VAL(native != NULL, NULL);

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	PUSH_TEMP(temps, protectedNative, OBJ_VAL(native));

	if (isBuiltin) {
		suint16_t builtinIdx = builtinConstant(runCtx, name);
		ELOX_CHECK_GOTO(builtinIdx >= 0, cleanup);
		vm->builtinValues.values[builtinIdx] = OBJ_VAL(native);
	} else {
		suint16_t globalIdx = globalIdentifierConstant(runCtx, name, moduleName);
		ELOX_CHECK_GOTO(globalIdx >= 0, cleanup);
		vm->globalValues.values[globalIdx] = OBJ_VAL(native);
	}

	native->arity = arity;
	native->maxArgs = hasVarargs ? ELOX_MAX_ARGS : arity;

	ret = native;

cleanup:
	releaseTemps(&temps);

	return ret;
}

static ObjClass *classOf(VM *vm, Value val) {
	ValueTypeId typeId = valueTypeId(val);
	return vm->classes[typeId];
}

static ObjClass *classOfFollowInstance(VM *vm, Value val) {
	ObjClass *clazz = classOf(vm, val);
	if (clazz == vm->builtins.biInstance.class_)
		return ((ObjInstance *)AS_OBJ(val))->class_;
	return clazz;
}

static bool instanceOf(ObjKlass *T, ObjClass *S) {
	uint8_t checkOffset = T->typeCheckOffset;
	if ((Obj *)T == S->typeInfo.rptDisplay[checkOffset])
		return true;
	if (checkOffset != ELOX_CLASS_DISPLAY_SIZE)
		return false;
	if ((ObjKlass *)S == T)
		return true;
	for (uint16_t s = 0; s < S->typeInfo.numRss; s++) {
		if ((Obj *)T == S->typeInfo.rssList[s]) {
			S->typeInfo.rptDisplay[ELOX_CLASS_DISPLAY_SIZE] = (Obj *)T;
			return true;
		}
	}
	return false;
}

ObjFiber *newFiber(RunCtx *runCtx, Value callable, EloxError *error) {
	Value *stack = GROW_ARRAY(runCtx, Value, NULL, 0, MIN_STACK);
	ELOX_CHECK_RET_VAL(stack != NULL, NULL);

	ObjFunction *function = NULL;
	ObjClosure *closure = NULL;
	if (!IS_NIL(callable)) {
		if (IS_OBJ(callable)) {
			Obj *callableObj = AS_OBJ(callable);
			ObjType callableType = getObjType(callableObj);
			if (callableType == OBJ_FUNCTION)
				function = (ObjFunction *)callableObj;
			else if (callableType == OBJ_CLOSURE) {
				closure = (ObjClosure *) callableObj;
				function = closure->function;
			}
		}
		if (ELOX_UNLIKELY(function == NULL))
			ELOX_RAISE_RET_VAL(error, RTERR(runCtx, "Fiber callable must be a function or closure)"), NULL);
	}

	ObjFiber *fiber = ALLOCATE_OBJ(runCtx, ObjFiber, OBJ_FIBER);
	if (ELOX_UNLIKELY(fiber == NULL)) {
		FREE_ARRAY(runCtx, Value, stack, MIN_STACK);
		ELOX_RAISE_RET_VAL(error, OOM(runCtx), NULL);
	}

	fiber->function = function;
	fiber->closure = closure;

	fiber->stack = stack;
	fiber->stackTopMax = fiber->stack + MIN_STACK - 1;
	fiber->stackCapacity = fiber->stackTopMax - fiber->stack + 1;

	fiber->stackTop = fiber->stack;
	fiber->state = IS_NIL(callable) ? ELOX_FIBER_DETACHED : ELOX_FIBER_IDLE;
	fiber->activeFrame = NULL;
	fiber->callDepth = 0;
	fiber->openUpvalues = NULL;
	fiber->temps = NULL;
	fiber->parent = NULL;
	fiber->prevSuspended = NULL;
	fiber->nextSuspended = NULL;

	return fiber;
}

void releaseFiberStack(RunCtx *runCtx, ObjFiber *fiber) {
	FREE_ARRAY(runCtx, Value, fiber->stack, fiber->stackCapacity);
	fiber->stack = NULL;
	fiber->stackTop = NULL;
	fiber->stackCapacity = 0;
}

void destroyFiber(RunCtx *runCtx, ObjFiber *fiber) {
	if (fiber != NULL) {
		ObjCallFrame *frame = fiber->activeFrame;
		while (frame != NULL) {
			releaseCallFrame(runCtx, fiber);
			frame = fiber->activeFrame;
		}

		FREE_ARRAY(runCtx, Value, fiber->stack, fiber->stackCapacity);
		FREE(runCtx, ObjFiber, fiber);
	}
}

void resumeFiber(RunCtx *runCtx, ObjFiber *fiber, ValueArray args, EloxError *error) {
	switch (fiber->state) {
		case ELOX_FIBER_IDLE: {
			ObjCallFrame *fiberFrame = allocCallFrame(runCtx, fiber);
			ELOX_CHECK_RAISE_RET(fiberFrame != NULL, error, OOM(runCtx));

			int missingArgs = 0;
			uint32_t argCount = args.count;
			uint16_t arity = fiber->function->arity;
			uint16_t maxArgs = fiber->function->maxArgs;
			uint16_t toCopy = ELOX_MIN(argCount, maxArgs);
			int stackArgs = toCopy;
			push(fiber, OBJ_VAL(fiber->function));
			for (int i = 0; i < toCopy; i++)
				push(fiber, args.values[i]);
			if (argCount < arity) {
				missingArgs = arity - argCount;
				Value *defaultValues = fiber->function->defaultArgs;
				for (int i = argCount; i < arity; i++) {
					push(fiber, defaultValues[i]);
					stackArgs++;
				}
			}

			fiberFrame->slots = fiber->stackTop - stackArgs - 1;
			fiberFrame->fixedArgs = arity;
			fiberFrame->varArgs = argCount + missingArgs - arity;
			fiberFrame->argOffset = 0;

			fiberFrame->type = ELOX_FT_FIBER_START;
			fiberFrame->function = fiber->function;
			fiberFrame->closure = fiber->closure;
			fiberFrame->ip = fiber->function->chunk.code;

			fiber->parent = runCtx->activeFiber;
			fiber->state = ELOX_FIBER_RUNNING;

			runCtx->activeFiber->state = ELOX_FIBER_WAITING;
			runCtx->activeFiber = fiber;
			break;
		}
		case ELOX_FIBER_SUSPENDED: {
			Value resumeVal = NIL_VAL;
			uint32_t argCount = args.count;
			if (args.count > 0) {
				if (argCount == 1)
					resumeVal = args.values[0];
				else {
					ObjArray *res = newArrayFrom(runCtx, args, OBJ_TUPLE);
					ELOX_CHECK_RAISE_RET(res != NULL, error, OOM(runCtx));
					resumeVal = OBJ_VAL(res);
				}
			}

			pop(fiber); // result from native invocation
			push(fiber, resumeVal);
			fiber->parent = runCtx->activeFiber;

			fiber->nextSuspended->prevSuspended = fiber->prevSuspended;
			fiber->prevSuspended->nextSuspended = fiber->nextSuspended;
			fiber->nextSuspended = NULL;
			fiber->prevSuspended = NULL;

			fiber->state = ELOX_FIBER_RUNNING;
			runCtx->activeFiber->state = ELOX_FIBER_WAITING;

			runCtx->activeFiber = fiber;
			break;
		}
		case ELOX_FIBER_WAITING:
		case ELOX_FIBER_DETACHED:
		case ELOX_FIBER_RUNNING:
		case ELOX_FIBER_TERMINATED:
			ELOX_RAISE_RET(error, RTERR(runCtx, "Cannot resume %s fiber",
										fiberStateNames[fiber->state]));
	}
}

void resumeThrow(RunCtx *runCtx, ObjFiber *fiber, ObjInstance *throwable, EloxError *error) {
	VM *vm = runCtx->vm;

	ELOX_CHECK_RAISE_RET(instanceOf((ObjKlass *)vm->builtins.biThrowable.class_, throwable->class_),
						 error, RTERR(runCtx, "Can only throw Throwable instances"));
	ELOX_CHECK_RAISE_RET(fiber->state == ELOX_FIBER_SUSPENDED, error,
						 RTERR(runCtx, "Cannot resume %s fiber with error",
							   fiberStateNames[fiber->state]));

	pop(fiber); // result from native invocation
	push(fiber, OBJ_VAL(throwable));
	fiber->parent = runCtx->activeFiber;

	fiber->nextSuspended->prevSuspended = fiber->prevSuspended;
	fiber->prevSuspended->nextSuspended = fiber->nextSuspended;
	fiber->nextSuspended = NULL;
	fiber->prevSuspended = NULL;

	fiber->state = ELOX_FIBER_RUNNING;
	runCtx->activeFiber->state = ELOX_FIBER_WAITING;

	runCtx->activeFiber = fiber;
}

void yieldFiber(RunCtx *runCtx, ValueArray args, EloxError *error) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	if (fiber->state != ELOX_FIBER_RUNNING)
		ELOX_RAISE_RET(error, RTERR(runCtx, "Cannot yield from %s fiber",
									fiberStateNames[fiber->state]));

	Value retVal = NIL_VAL;
	uint32_t argCount = args.count;
	if (args.count > 0) {
		if (argCount == 1)
			retVal = args.values[0];
		else {
			ObjArray *res = newArrayFrom(runCtx, args, OBJ_TUPLE);
			ELOX_CHECK_RAISE_RET(res != NULL, error, OOM(runCtx));
			retVal = OBJ_VAL(res);
		}
	}

	ObjFiber *parent = fiber->parent;
	pop(parent); // result from native invocation
	push(parent, retVal);

	fiber->state = ELOX_FIBER_SUSPENDED;
	fiber->nextSuspended = vm->suspendedHead->nextSuspended;
	fiber->prevSuspended = vm->suspendedHead;
	vm->suspendedHead->nextSuspended->prevSuspended = fiber;
	vm->suspendedHead->nextSuspended = fiber;

	fiber->parent = NULL;
	parent->state = (parent->function == NULL) ?
					ELOX_FIBER_DETACHED : ELOX_FIBER_RUNNING;
	runCtx->activeFiber = parent;
}

static Value getStackTrace(RunCtx *runCtx) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);

	ObjArray *arr = newArray(runCtx, fiber->callDepth, OBJ_ARRAY);
	if (ELOX_UNLIKELY(arr == NULL))
		return NIL_VAL;
	Value ret = OBJ_VAL(arr);
	PUSH_TEMP(temps, protectedRet, ret);

	const struct BIStackTraceElement *biSTE = &vm->builtins.biStackTraceElement;

	for (ObjCallFrame *frame = fiber->activeFrame; frame != NULL;
		 frame = (ObjCallFrame *)getObjType(&frame->obj)) {
		ObjFunction *function = frame->function;
		// -1 because the IP is sitting on the next instruction to be executed
		size_t instruction = frame->ip - function->chunk.code - 1;
		uint32_t lineNo = getLine(&function->chunk, instruction);

		ObjInstance *elem = (ObjInstance *)newInstance(runCtx, biSTE->class_);
		if (ELOX_UNLIKELY(elem == NULL))
			goto cleanup;
		elem->fields[biSTE->fields.fileName] = OBJ_VAL(function->chunk.fileName);
		elem->fields[biSTE->fields.lineNumber] = NUMBER_VAL(lineNo);
		if (function->name == NULL)
			elem->fields[biSTE->fields.functionName] = OBJ_VAL(vm->builtins.scriptString);
		else
			elem->fields[biSTE->fields.functionName] = OBJ_VAL(function->name);

		appendToArray(runCtx, arr, OBJ_VAL(elem));
	}

cleanup:
	releaseTemps(&temps);

	return ret;
}

bool setInstanceField(ObjInstance *instance, ObjString *name, Value value) {
	ObjClass *class_ = instance->class_;
	PropInfo valueInfo = propTableGet(&class_->props, name, ELOX_PROP_FIELD_MASK);
	if (valueInfo.type != ELOX_PROP_NONE) {
		instance->fields[valueInfo.index] = value;
		return false;
	}
	return true;
}

ObjCallFrame *propagateException(RunCtx *runCtx) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	ObjInstance *exception = (ObjInstance *)AS_OBJ(peek(fiber, 0));
	bool exceptionHandled = false;
	while (true) {
		ObjCallFrame *frame = fiber->activeFrame;
		TryBlock *tryBlock = frame->tryStack;

		while (tryBlock != NULL) {
			uint16_t handlerDataOffset = tryBlock->handlerDataOffset;
			ObjFunction *frameFunction = frame->function;
			uint8_t *handlerData = frameFunction->chunk.code + handlerDataOffset;
			uint8_t handlerTableSize = handlerData[0];
			uint16_t finallyAddress;
			memcpy(&finallyAddress, handlerData + 1, sizeof(uint16_t));
			uint8_t numHandlers = handlerTableSize / 6;
			if (!tryBlock->caught) {
				for (int i = 0; i < numHandlers; i++) {
					uint8_t *handlerRecord = handlerData + 1 + 2 + (6 * i);
					VarScope typeVarType = handlerRecord[0];
					bool postArgs = handlerRecord[1];
					uint16_t typeHandle;
					memcpy(&typeHandle, handlerRecord + 2, sizeof(uint16_t));
					Value klassVal = NIL_VAL;
					switch (typeVarType) {
						case VAR_LOCAL:
							klassVal = frame->slots[typeHandle + (postArgs * frame->varArgs)];
							break;
						case VAR_UPVALUE:
							klassVal = *frame->closure->upvalues[typeHandle]->location;
							break;
						case VAR_GLOBAL: {
							klassVal = vm->globalValues.values[typeHandle];
							if (ELOX_UNLIKELY(IS_UNDEFINED(klassVal))) {
								// replace exception
								exception = (ObjInstance *)AS_OBJ(peek(fiber, 0));
								goto replace; // TODO: ???
							}
							break;
						}
						case VAR_BUILTIN:
							klassVal = vm->builtinValues.values[typeHandle];
							break;
						case VAR_TUPLE:
							ELOX_UNREACHABLE();
							assert(false);
					}
					if (ELOX_UNLIKELY(!IS_KLASS(klassVal))) {
						runtimeError(runCtx, NULL, "[catch] Not a catcheable type");
						// replace exception
						exception = (ObjInstance *)AS_OBJ(peek(fiber, 0));
						goto replace; // TODO: ???
					}

					ObjKlass *handlerKlass = (ObjKlass *)AS_OBJ(klassVal);
					if (instanceOf(handlerKlass, exception->class_)) {
						tryBlock->caught = true;
						exceptionHandled = true;
						uint16_t handlerAddress;
						memcpy(&handlerAddress, handlerRecord + 4, sizeof(uint16_t));
						frame->ip = &frameFunction->chunk.code[handlerAddress];
						Value exception = pop(fiber);
						fiber->stackTop = frame->slots + tryBlock->stackOffset;
						push(fiber, exception);
						return NULL; // TODO: ???
					}
				}
			}

replace:
			if (finallyAddress > 0) {
				frame->ip = &frameFunction->chunk.code[finallyAddress];
				fiber->stackTop = frame->slots + tryBlock->stackOffset;
				TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
				PUSH_TEMP(temps, protectedException, OBJ_VAL(exception));
				bool finallyOk = runChunk(runCtx);
				releaseTemps(&temps);
				if (finallyOk) {
					if (!exceptionHandled) {
						// restore original exception
						push(fiber, OBJ_VAL(exception));
					}
				} else {
					// really should not throw exceptions from finally, but...
					// replace exception
					exception = (ObjInstance *)AS_OBJ(peek(fiber, 0));
					//frame->handlerCount--; // TODO: ???
				}
			}

			tryBlock = tryBlock->prev;
			releaseTryBlock(runCtx, frame);
		}

		switch (frame->type) {
			case ELOX_FT_INTER:
				releaseCallFrame(runCtx, fiber);
				break;
			case ELOX_FT_INTERNAL_CALL_START:
				DBG_PRINT_STACK("DBGExc", runCtx);
				return fiber->activeFrame;
			case ELOX_FT_FIBER_START:
				if (fiber->parent != NULL) {
					ObjFiber *parent = fiber->parent;
					fiber->state = ELOX_FIBER_TERMINATED;
					fiber->parent = NULL;
					releaseCallFrame(runCtx, fiber);
					pop(parent); // result from native resume()invocation
					push(parent, OBJ_VAL(exception));
					parent->state = (parent->function == NULL) ?
									ELOX_FIBER_DETACHED : ELOX_FIBER_RUNNING;
					runCtx->activeFiber = parent;
					fiber = parent;
				} else
					// TODO: ???
					return NULL;
				break;
		}
	}

	return fiber->activeFrame;
}

static bool unrollExceptionHandlerStack(RunCtx *runCtx, uint8_t targetLevel, bool restore) {
	ObjFiber *fiber = runCtx->activeFiber;

	Value savedTop = NIL_VAL;
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	VMTemp protectedTop;
	if (restore) {
		savedTop = pop(fiber);
		pushTempVal(temps, &protectedTop, savedTop);
	}

	ObjCallFrame *frame = fiber->activeFrame;
	uint8_t tryDepth = frame->tryDepth;
	while (tryDepth > targetLevel) {
		TryBlock *tryBlock = frame->tryStack;
		uint16_t handlerDataOffset = tryBlock->handlerDataOffset;
		ObjFunction *frameFunction = frame->function;
		uint8_t *handlerData = frameFunction->chunk.code + handlerDataOffset;
		uint16_t finallyAddress;
		memcpy(&finallyAddress, handlerData + 1, sizeof(uint16_t));

		if (finallyAddress > 0) {
			frame->ip = &frameFunction->chunk.code[finallyAddress];
			fiber->stackTop = frame->slots + tryBlock->stackOffset;
			bool finallyStatus = runChunk(runCtx);
		}

		releaseTryBlock(runCtx, frame);
		tryDepth = frame->tryDepth;
	}

	if (restore) {
		push(fiber, savedTop);
		releaseTemps(&temps);
	}

	return true;
}

static bool pushExceptionHandler(RunCtx *runCtx, uint16_t handlerTableAddress) {
	ObjFiber *fiber = runCtx->activeFiber;

	ObjCallFrame *frame = fiber->activeFrame;

	TryBlock *tryBlock = allocTryBlock(runCtx, frame);
	if (ELOX_UNLIKELY(tryBlock == NULL)) {
		oomError(runCtx, NULL);
		return false;
	}

	tryBlock->handlerDataOffset = handlerTableAddress;
	tryBlock->stackOffset = fiber->stackTop - frame->slots;
	tryBlock->caught = false;
	return true;
}

static CallResult callValue(RunCtx *runCtx, Value callee, int argCount) {
#ifdef ELOX_ENABLE_COMPUTED_GOTO
#define ELOX_OBJTAGS_INLINE

	static void *objTagDispatchTable[] = {
		#define INITTAG(name, init) && objTagDispatch_##name,
		#define TAG(name) && objTagDispatch_##name,
		#include <elox/objTags.h>
	};

#undef TAG
#undef INITTAG
#undef ELOX_OBJTAGS_INLINE
#endif // ELOX_ENABLE_COMPUTED_GOTO

	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	if (IS_OBJ(callee)) {
		ObjType objType = getObjType(AS_OBJ(callee));

		#include <elox/objTagsDispatchStart.h>

		OBJ_TAG_DISPATCH_START(objType)
			OBJ_TAG_DISPATCH_CASE(BOUND_METHOD): {
				ObjBoundMethod *bound = (ObjBoundMethod *)AS_OBJ(callee);

				fiber->stackTop[-argCount - 1] = bound->receiver;
				return callMethod(runCtx, bound->method, argCount, 0);
			}
			OBJ_TAG_DISPATCH_CASE(METHOD): {
				ObjMethod *method = (ObjMethod *)AS_OBJ(callee);
				if (ELOX_UNLIKELY(argCount < 1)) {
					runtimeError(runCtx, NULL, "Need to pass instance when calling method");
					return (CallResult){ false, false };
				}
				if (ELOX_UNLIKELY(!instanceOf(method->method.klass,
											  classOfFollowInstance(vm, fiber->stackTop[-argCount])))) {
					runtimeError(runCtx, NULL, "Method invoked on wrong instance type");
					return (CallResult){ false, false };
				}
				return callMethod(runCtx, method->method.callable, argCount, 1);
			}
			OBJ_TAG_DISPATCH_CASE(INTERFACE):
				runtimeError(runCtx, NULL, "Cannot call interfaces");
				return (CallResult){ false, false };
			OBJ_TAG_DISPATCH_CASE(CLASS): {
				ObjClass *class_ = (ObjClass *)AS_OBJ(callee);
				if ((class_->flags & CLASS_INIT_RETURNS_INST) != 0) {
					if (ELOX_UNLIKELY(IS_NIL(class_->initializer))) {
						runtimeError(runCtx, NULL, "Missing initializer for class '%s'",
									 class_->name->string.chars);
						return (CallResult){ false, false };
					}
					return callMethod(runCtx, AS_OBJ(class_->initializer), argCount, 0);
				} else {
					Obj *inst = newInstance(runCtx, class_);
					if (ELOX_UNLIKELY(inst == NULL)) {
						oomError(runCtx, NULL);
						return (CallResult){ false, false };
					}
					fiber->stackTop[-argCount - 1] = OBJ_VAL(inst);
					if (!IS_NIL(class_->initializer)) {
#ifdef ELOX_DEBUG_TRACE_EXECUTION
					eloxPrintf(runCtx, ELOX_IO_DEBUG, "===>%s init\n", class_->name->string.chars);
#endif
						return callMethod(runCtx, AS_OBJ(class_->initializer), argCount, 0);
					} else if (argCount != 0) {
						runtimeError(runCtx, NULL, "Expected 0 arguments but got %d", argCount);
						return (CallResult){ false, false };
					}
				}
				return (CallResult){ false, true };
			}
			OBJ_TAG_DISPATCH_CASE(CLOSURE):
				return (CallResult){ false,
									 callClosure(runCtx, (ObjClosure *)AS_OBJ(callee), argCount, 0) };
			OBJ_TAG_DISPATCH_CASE(NATIVE_CLOSURE):
				return (CallResult){ true,
									 callNativeClosure(runCtx, (ObjNativeClosure *)AS_OBJ(callee), argCount, 0, false) };
			OBJ_TAG_DISPATCH_CASE(FUNCTION):
				return (CallResult){ false,
									 callFunction(runCtx, (ObjFunction *)AS_OBJ(callee), argCount, 0) };
			OBJ_TAG_DISPATCH_CASE(NATIVE):
				return (CallResult){ true,
									 callNative(runCtx, (ObjNative *)AS_OBJ(callee), argCount, 0, false) };
			OBJ_TAG_DISPATCH_CASE(STRING):
			OBJ_TAG_DISPATCH_CASE(PENDING_METHOD):
			OBJ_TAG_DISPATCH_CASE(DEFAULT_METHOD):
			OBJ_TAG_DISPATCH_CASE(ABSTRACT_METHOD):
			OBJ_TAG_DISPATCH_CASE(INSTANCE):
			OBJ_TAG_DISPATCH_CASE(STRINGPAIR):
			OBJ_TAG_DISPATCH_CASE(UPVALUE):
			OBJ_TAG_DISPATCH_CASE(ARRAY):
			OBJ_TAG_DISPATCH_CASE(TUPLE):
			OBJ_TAG_DISPATCH_CASE(HASHMAP):
			OBJ_TAG_DISPATCH_CASE(FRAME):
			OBJ_TAG_DISPATCH_CASE(FIBER):
				OBJ_TAG_DISPATCH_BREAK; // Non-callable object type
		OBJ_TAG_DISPATCH_END

		#include <elox/objTagsDispatchEnd.h>
	}
	runtimeError(runCtx, NULL, "Can only call functions and classes");
	return (CallResult){ false, false };
}

static bool invoke(RunCtx *runCtx, ObjString *name, int argCount) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	Value receiver = peek(fiber, argCount);

	ObjClass *class_ = classOf(vm, receiver);
	if (ELOX_UNLIKELY(class_ == NULL)) {
		runtimeError(runCtx, NULL, "Value has no methods");
		return false;
	}

	uint32_t allow = ELOX_PROP_METHOD_MASK;

	if (class_ == vm->builtins.biClass.class_) {
		class_ = (ObjClass *)AS_OBJ(receiver);
		allow = ELOX_PROP_METHOD_MASK | ELOX_PROP_STATIC_MASK;
	} else if (class_ == vm->builtins.biInstance.class_) {
		class_ = ((ObjInstance *)AS_OBJ(receiver))->class_;
		allow = ELOX_PROP_METHOD_MASK | ELOX_PROP_FIELD_MASK;
	}

#ifdef ELOX_ENABLE_COMPUTED_GOTO
#define ELOX_PROP_TYPES_INLINE

	static void *propTypeDispatchTable[] = {
		#define PROP_TYPE(name) && propTypeDispatch_##name,
		#include <elox/propTypes.h>
	};

#undef PROP_TYPE
#undef ELOX_OBJTAGS_INLINE
#endif // ELOX_ENABLE_COMPUTED_GOTO

	PropInfo propInfo = propTableGet(&class_->props, name, allow);

	#include <elox/propTypeDispatchStart.h>

	PROP_TYPE_DISPATCH_START(propInfo.type)
		PROP_TYPE_DISPATCH_CASE(FIELD):
		PROP_TYPE_DISPATCH_CASE(STATIC):
			return callValue(runCtx, class_->classData.values[propInfo.index], argCount).result;
		PROP_TYPE_DISPATCH_CASE(METHOD):
			return callMethod(runCtx,
							  ((ObjMethod *)AS_OBJ(class_->classData.values[propInfo.index]))->method.callable,
							  argCount, 0).result;
		PROP_TYPE_DISPATCH_CASE(NONE):
			PROP_TYPE_DISPATCH_BREAK;
	PROP_TYPE_DISPATCH_END

	#include <elox/propTypeDispatchEnd.h>

	runtimeError(runCtx, NULL, "Undefined property '%s'", name->string.chars);
	return false;
}

static bool invoke1(RunCtx *runCtx, ObjString *name, int argCount) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	Value receiver = peek(fiber, argCount);

	ObjClass *clazz = classOf(vm, receiver);
	if (ELOX_UNLIKELY(clazz == NULL)) {
		runtimeError(runCtx, NULL, "This value has no methods");
		return false;
	}

	if (clazz == vm->builtins.biClass.class_) {
		clazz = (ObjClass *)AS_OBJ(receiver);
		PropInfo propInfo = propTableGetAny(&clazz->props, name);
		if (propInfo.type != ELOX_PROP_NONE) {
			if (ELOX_LIKELY(propInfo.type < ELOX_PROP_FIELD)) {
				Value namedVal = clazz->classData.values[propInfo.index];
				return callValue(runCtx, namedVal, argCount).result;
			}
		}
		runtimeError(runCtx, NULL, "Undefined method or static member '%s'", name->string.chars);
		return false;
	} else if (clazz == vm->builtins.biInstance.class_) {
		ObjInstance *instance = (ObjInstance *)AS_OBJ(receiver);
		clazz = instance->class_;
		PropInfo propInfo = propTableGetAny(&clazz->props, name);
		if (propInfo.type != ELOX_PROP_NONE) {
			switch (propInfo.type) {
				case ELOX_PROP_FIELD:
					return callValue(runCtx, instance->fields[propInfo.index], argCount).result;
				case ELOX_PROP_METHOD:
					return callMethod(runCtx,
									  ((ObjMethod *)AS_OBJ(clazz->classData.values[propInfo.index]))->method.callable,
									  argCount, 0).result;
				default:
					break;
			}
		}
		runtimeError(runCtx, NULL, "Undefined method or field '%s'", name->string.chars);
		return false;
	}

	PropInfo methodInfo = propTableGet(&clazz->props, name, ELOX_PROP_METHOD_MASK);
	if (ELOX_UNLIKELY(methodInfo.type == ELOX_PROP_NONE)) {
		runtimeError(runCtx, NULL, "Undefined method '%s'", name->string.chars);
		return false;
	}
	return callMethod(runCtx,
					  ((ObjMethod *)AS_OBJ(clazz->classData.values[methodInfo.index]))->method.callable,
					  argCount, 0).result;
}

static bool invokeMember(RunCtx *runCtx, Value *member, bool isMethod, int argCount) {
	ObjFiber *fiber = runCtx->activeFiber;

	if (!isMethod) {
		fiber->stackTop[-argCount - 1] = *member;
		return callValue(runCtx, *member, argCount).result;
	} else
		return callMethod(runCtx, ((ObjMethod *)AS_OBJ(*member))->method.callable, argCount, 0).result;
}

static void bindMethod(RunCtx *runCtx, ObjClass *clazz, ObjString *name, EloxError *error) {
	ObjFiber *fiber = runCtx->activeFiber;

	PropInfo methodInfo = propTableGet(&clazz->props, name, ELOX_PROP_METHOD_MASK);
	if (ELOX_UNLIKELY(methodInfo.type == ELOX_PROP_NONE))
		ELOX_RAISE_RET(error, RTERR(runCtx, "Undefined property '%s'", name->string.chars));

	ObjBoundMethod *bound = newBoundMethod(runCtx, peek(fiber, 0),
										   (ObjMethod *)AS_OBJ(clazz->classData.values[methodInfo.index]));
	if (ELOX_UNLIKELY(bound == NULL))
		ELOX_RAISE_RET(error, OOM(runCtx));

	pop(fiber);
	push(fiber, OBJ_VAL(bound));
}

static ObjUpvalue *captureUpvalue(RunCtx *runCtx, Value *local) {
	ObjFiber *fiber = runCtx->activeFiber;

	ObjUpvalue *prevUpvalue = NULL;
	ObjUpvalue *upvalue = fiber->openUpvalues;
	while (upvalue != NULL && upvalue->location > local) {
		prevUpvalue = upvalue;
		upvalue = upvalue->next;
	}

	if (upvalue != NULL && upvalue->location == local)
		return upvalue;

	ObjUpvalue *createdUpvalue = newUpvalue(runCtx, local);
	if (ELOX_UNLIKELY(createdUpvalue == NULL))
		return NULL;
	createdUpvalue->next = upvalue;

	if (prevUpvalue == NULL)
		fiber->openUpvalues = createdUpvalue;
	else
		prevUpvalue->next = createdUpvalue;

	return createdUpvalue;
}

static void closeUpvalues(RunCtx *runCtx, Value *last) {
	ObjFiber *fiber = runCtx->activeFiber;

	while ((fiber->openUpvalues != NULL) && (fiber->openUpvalues->location >= last)) {
		ObjUpvalue *upvalue = fiber->openUpvalues;
		upvalue->closed = *upvalue->location;
#ifdef ELOX_DEBUG_TRACE_EXECUTION
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%p >>>  (", upvalue);
	printValue(runCtx, ELOX_IO_DEBUG, upvalue->closed);
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, ")\n");
#endif
		upvalue->location = &upvalue->closed;
		fiber->openUpvalues = upvalue->next;
	}
}

bool isCallable(Value val) {
	if (!IS_OBJ(val))
		return false;
	switch (getObjType(AS_OBJ(val))) {
		case OBJ_BOUND_METHOD:
		case OBJ_METHOD:
		case OBJ_CLOSURE:
		case OBJ_FUNCTION:
		case OBJ_NATIVE:
			return true;
		default:
			return false;
	}
}

bool isFalsey(Value value) {
	return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

Value toString(RunCtx *runCtx, Value value, EloxError *error) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	ObjClass *clazz = classOfFollowInstance(vm, value);
	ELOX_CHECK_RAISE_RET_VAL((clazz !=  NULL), error,
							  RTERR(runCtx, "No string representation available"), EXCEPTION_VAL);

	PropInfo methodInfo =
		propTableGet(&clazz->props, vm->builtins.biObject.strings.toString, ELOX_PROP_METHOD_MASK);
	if (ELOX_UNLIKELY(methodInfo.type == ELOX_PROP_NONE))
		ELOX_RAISE_RET_VAL(error, RTERR(runCtx, "No string representation available"), EXCEPTION_VAL);

	ObjBoundMethod *boundToString = newBoundMethod(runCtx, value,
												   (ObjMethod *)AS_OBJ(clazz->classData.values[methodInfo.index]));
	if (ELOX_UNLIKELY(boundToString == NULL)) {
		push(fiber, OBJ_VAL(vm->builtins.oomError));
		error->raised = true;
		return EXCEPTION_VAL;
	}
	push(fiber, OBJ_VAL(boundToString));
	Value strVal = runCall(runCtx, 0);

	if (ELOX_UNLIKELY(IS_EXCEPTION(strVal))) {
		error->raised = true;
		return EXCEPTION_VAL;
	}

	pop(fiber);
	return strVal;
}

static bool buildMap(RunCtx *runCtx, uint16_t itemCount) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	ObjHashMap *map = newHashMap(runCtx);
	if (ELOX_UNLIKELY(map == NULL)) {
		push(fiber, OBJ_VAL(vm->builtins.oomError));
		return false;
	}

	push(fiber, OBJ_VAL(map));
	int i = 2 * itemCount;
	EloxError error = ELOX_ERROR_INITIALIZER;
	while (i > 0) {
		Value key = peek(fiber, i--);
		Value value = peek(fiber, i--);
		valueTableSet(runCtx, &map->items, key, value, &error);
		if (ELOX_UNLIKELY(error.raised))
			return false;
	}
	pop(fiber);

	// pop constructor arguments from the stack
	popn(fiber, 2 * itemCount);

	push(fiber, OBJ_VAL(map));

	return true;
}

static bool indexValue(RunCtx *runCtx) {
	ObjFiber *fiber = runCtx->activeFiber;

	Value indexVal = peek(fiber, 0);
	Value indexable = peek(fiber, 1);
	Value result;

	ValueTypeId indexableType = valueTypeId(indexable);

	switch (indexableType) {
		case VTYPE_OBJ_ARRAY:
		case VTYPE_OBJ_TUPLE: {
			ObjArray *array = (ObjArray *)AS_OBJ(indexable);
			if (ELOX_UNLIKELY(!IS_NUMBER(indexVal))) {
				runtimeError(runCtx, NULL, "Array index is not a number");
				return false;
			}
			int32_t index = AS_NUMBER(indexVal);
			result = arrayAtSafe(runCtx, array, index);
			if (ELOX_UNLIKELY(IS_EXCEPTION(result)))
				return false;
			break;
		}
		case VTYPE_OBJ_HASHMAP: {
			ObjHashMap *map = (ObjHashMap *)AS_OBJ(indexable);
			EloxError error = ELOX_ERROR_INITIALIZER;
			bool found = valueTableGet(runCtx, &map->items, indexVal, &result, &error);
			if (ELOX_UNLIKELY(error.raised))
				return false;
			if (!found)
				result = NIL_VAL;
			break;
		}
		case VTYPE_OBJ_STRING: {
			ObjString *str = AS_STRING(indexable);
			if (ELOX_UNLIKELY(!IS_NUMBER(indexVal))) {
				runtimeError(runCtx, NULL, "String index is not a number");
				return false;
			}
			int32_t index = AS_NUMBER(indexVal);
			result = stringAtSafe(runCtx, str, index);
			if (ELOX_UNLIKELY(IS_EXCEPTION(result)))
				return false;
			break;
		}
		case VTYPE_OBJ_FRAME: {
			ObjCallFrame *frame = (ObjCallFrame *)AS_OBJ(indexable);
			if (ELOX_UNLIKELY(!IS_NUMBER(indexVal))) {
				runtimeError(runCtx, NULL, "Arg index is not a number");
				return false;
			}
			uint32_t index = AS_NUMBER(indexVal);
			result = frame->slots[frame->fixedArgs + index + 1];
			break;
		}
		default:
			runtimeError(runCtx, NULL, "Invalid type to index into");
			return false;
	}

	popn(fiber, 2);
	push(fiber, result);

	return true;
}

static bool indexStore(RunCtx *runCtx) {
	ObjFiber *fiber = runCtx->activeFiber;

	Value item = peek(fiber, 0);
	Value indexVal = peek(fiber, 1);
	Value indexable = peek(fiber, 2);

	ValueTypeId indexableType = valueTypeId(indexable);

	switch (indexableType) {
		case VTYPE_OBJ_ARRAY: {
			ObjArray *array = (ObjArray *)AS_OBJ(indexable);

			if (!IS_NUMBER(indexVal)) {
				runtimeError(runCtx, NULL, "Array index is not a number");
				return false;
			}

			int index = AS_NUMBER(indexVal);
			if (ELOX_UNLIKELY(!isValidArrayIndex(array, index))) {
				runtimeError(runCtx, NULL, "Array index out of range");
				return false;
			}

			arraySet(array, index, item);
			break;
		}
		case VTYPE_OBJ_HASHMAP: {
			ObjHashMap *map = (ObjHashMap *)AS_OBJ(indexable);

			EloxError error = ELOX_ERROR_INITIALIZER;
			valueTableSet(runCtx, &map->items, indexVal, item, &error);
			if (ELOX_UNLIKELY(error.raised))
				return false;
			break;
		}
		default:
			runtimeError(runCtx, NULL, "Destination is not an array or map");
			return false;
	}

	popn(fiber, 3);
	push(fiber, item);

	return true;
}

static bool sliceValue(RunCtx *runCtx) {
	ObjFiber *fiber = runCtx->activeFiber;

	Value sliceEnd = peek(fiber, 0);
	Value sliceStart = peek(fiber, 1);
	Value sliceable = peek(fiber, 2);
	Value result;

	ValueTypeId sliceableType = valueTypeId(sliceable);
	switch(sliceableType) {
		case VTYPE_OBJ_ARRAY:
		case VTYPE_OBJ_TUPLE: {
			ObjType type = getObjType(AS_OBJ(sliceable));
			ObjArray *array = (ObjArray *)AS_OBJ(sliceable);
			result = arraySlice(runCtx, array, type, sliceStart, sliceEnd);
			if (ELOX_UNLIKELY(IS_EXCEPTION(result)))
				return false;
			break;
		}
		case VTYPE_OBJ_STRING: {
			ObjString *str = AS_STRING(sliceable);
			result = stringSlice(runCtx, str, sliceStart, sliceEnd);
			if (ELOX_UNLIKELY(IS_EXCEPTION(result)))
				return false;
			break;
		}
		default:
			runtimeError(runCtx, NULL, "Invalid type to slice");
			return false;
	}

	popn(fiber, 3);
	push(fiber, result);

	return true;
}

typedef enum {
#define ELOX_OPCODES_INLINE
#define OPCODE(name) IN_OP_##name,
#include "ops/inOps.h"
#undef OPCODE
#undef ELOX_OPCODES_INLINE
} ELOX_PACKED InOps;

#define ANY BOOL, NIL, NUMBER, OBJ_STRING, OBJ_BOUND_METHOD, OBJ_CLASS, OBJ_CLOSURE, \
	OBJ_NATIVE_CLOSURE, OBJ_FUNCTION, OBJ_INSTANCE, OBJ_NATIVE, OBJ_ARRAY, OBJ_TUPLE, OBJ_HASHMAP

#define IN_ARRAY(T) \
	[VTYPE_ ## T][VTYPE_OBJ_ARRAY] = IN_OP_VALUE_ARRAY,
#define IN_TUPLE(T) \
	[VTYPE_ ## T][VTYPE_OBJ_TUPLE] = IN_OP_VALUE_ARRAY,
#define IN_HASHMAP(T) \
	[VTYPE_ ## T][VTYPE_OBJ_HASHMAP] = IN_OP_VALUE_HASHMAP,
#define IN_VARARGS(T) \
	[VTYPE_ ## T][VTYPE_OBJ_FRAME] = IN_OP_VALUE_VARARGS,

static const InOps inTable[VTYPE_MAX][VTYPE_MAX] = {
	[VTYPE_OBJ_STRING][VTYPE_OBJ_STRING] = IN_OP_STRING_STRING,
	FOR_EACH(IN_ARRAY, ANY)
	FOR_EACH(IN_TUPLE, ANY)
	FOR_EACH(IN_HASHMAP, ANY)
	FOR_EACH(IN_VARARGS, ANY)
};

static bool valIn(RunCtx *runCtx, EloxError *error) {
	ObjFiber *fiber = runCtx->activeFiber;

	bool ret = false;

#ifdef ELOX_ENABLE_COMPUTED_GOTO
#define ELOX_OPCODES_INLINE

static void *INDispatchTable[] = {
	#define OPCODE(name) &&IN_opcode_##name,
	#include "ops/inOps.h"
};

#undef ELOX_OPCODES_INLINE
#undef OPCODE
#endif // ELOX_ENABLE_COMPUTED_GOTO

	uint32_t seqType = valueTypeId(peek(fiber, 0));
	uint32_t valType = valueTypeId(peek(fiber, 1));

	InOps op = inTable[valType][seqType];

	#define OPNAME IN
	#include "ops/opsInit.h"

	OP_DISPATCH_START(op)
		OP_DISPATCH_CASE(STRING_STRING): {
			ObjString *seq = AS_STRING(pop(fiber));
			ObjString *val = AS_STRING(pop(fiber));
			ret = stringContains(seq, val);
			OP_DISPATCH_BREAK;
		}
		OP_DISPATCH_CASE(VALUE_ARRAY): {
			EloxError error = ELOX_ERROR_INITIALIZER;
			bool res = arrayContains(runCtx, (ObjArray *)AS_OBJ(peek(fiber, 0)), peek(fiber, 1), &error);
			if (ELOX_UNLIKELY(error.raised))
				return false;
			popn(fiber, 2);
			ret = res;
			OP_DISPATCH_BREAK;
		}
		OP_DISPATCH_CASE(VALUE_HASHMAP): {
			ObjHashMap *map = (ObjHashMap *)AS_OBJ(peek(fiber, 0));
			Value val = peek(fiber, 1);
			bool found = valueTableContains(runCtx, &map->items, val, error);
			if (ELOX_UNLIKELY(error->raised))
				return false;
			popn(fiber, 2);
			ret = found;
			OP_DISPATCH_BREAK;
		}
		OP_DISPATCH_CASE(VALUE_VARARGS): {
			ObjCallFrame *frame = (ObjCallFrame *)AS_OBJ(peek(fiber, 0));
			Value val = peek(fiber, 1);

			uint32_t numVarargs = frame->varArgs;
			for (uint32_t i = 0; i < numVarargs; i++) {
				Value argVal = frame->slots[frame->fixedArgs + i + 1];
				if (valuesEquals(runCtx, val, argVal, error)) {
					ret = true;
					break;
				}
				if (ELOX_UNLIKELY(error->raised))
					return false;
			}

			popn(fiber, 2);
			OP_DISPATCH_BREAK;
		}
		OP_DISPATCH_CASE(UNDEFINED):
			ELOX_RAISE_RET_VAL(error, RTERR(runCtx, "Invalid operands for 'in'"), false);
			OP_DISPATCH_BREAK;
	OP_DISPATCH_END

	#include "ops/opsCleanup.h"

	return ret;
}

static unsigned int closeClass(RunCtx *runCtx, ObjCallFrame *frame, EloxError *error) {
	ObjFiber *fiber = runCtx->activeFiber;
	uint8_t *ip = frame->ip;

	uint8_t *ptr = ip;

	ObjClass *class_ = (ObjClass *)AS_OBJ(peek(fiber, 1));

	closeOpenKlass(runCtx, (ObjKlass *)class_, error);
	if (ELOX_UNLIKELY(error->raised))
		return (ptr - ip);

	bool abstract = (class_->flags & CLASS_ABSTRACT) != 0;

	bool hasAbstractMethods = false;
	for (int m = 0; m < class_->props.capacity; m++) {
		PropEntry *entry = &class_->props.entries[m];
		ObjString *methodName = entry->key;
		if ((methodName != NULL) && (entry->value.type == ELOX_PROP_METHOD)) {
			Obj *method = AS_OBJ(class_->classData.values[entry->value.index]);
			if (getObjType(method) == OBJ_ABSTRACT_METHOD) {
				if (!abstract) {
					ELOX_RAISE_RET_VAL(error, RTERR(runCtx, "Unimplemented method %.*s",
													methodName->string.length, methodName->string.chars),
									   ptr - ip);
				} else {
					hasAbstractMethods = true;
					break;
				}
			}
		}
	}
	if (abstract && (!hasAbstractMethods)) {
		ELOX_RAISE_RET_VAL(error, RTERR(runCtx, "Abstract class has no abstract methods"), ptr - ip);
	}

	return (ptr - ip);
}

static unsigned int buildArray(RunCtx *runCtx, uint8_t *ip, ObjType objType, EloxError *error) {
	ObjFiber *fiber = runCtx->activeFiber;

	uint8_t *ptr = ip;

	uint16_t itemCount = CHUNK_READ_USHORT(ptr);
	ObjArray *array = newArray(runCtx, itemCount, objType);
	ELOX_CHECK_RAISE_RET_VAL(array != NULL, error, OOM(runCtx), ptr - ip);

	push(fiber, OBJ_VAL(array));
	for (int i = itemCount; i > 0; i--) {
		bool ret = appendToArray(runCtx, array, peek(fiber, i));
		ELOX_CHECK_RAISE_RET_VAL(ret, error, OOM(runCtx), ptr - ip);
	}
	pop(fiber);

	// pop constructor arguments from the stack
	popn(fiber, itemCount);

	push(fiber, OBJ_VAL(array));

	return (ptr - ip);
}

static unsigned int foreachInit(RunCtx *runCtx, ObjCallFrame *frame, EloxError *error) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;
	uint8_t *ip = frame->ip;

	uint8_t *ptr = ip;

	uint8_t hasNextSlot = CHUNK_READ_BYTE(ptr);
	bool hasNextPostArgs = CHUNK_READ_BYTE(ptr);
	uint8_t nextSlot = CHUNK_READ_BYTE(ptr);
	bool nextPostArgs = CHUNK_READ_BYTE(ptr);
	Value iterableVal = peek(fiber, 0);

	ObjInstance *iterator = NULL;
	if (isObjType(iterableVal, OBJ_INSTANCE) &&
		instanceOf((ObjKlass *)vm->builtins.biIterator.class_,
				   ((ObjInstance *)AS_OBJ(iterableVal))->class_))
		iterator = (ObjInstance *)AS_OBJ(pop(fiber));
	else {
		ObjClass *clazz = classOfFollowInstance(vm, iterableVal);
		if (clazz != NULL) {
			if (instanceOf((ObjKlass *)vm->builtins.biIterable.intf, clazz)) {
				bindMethod(runCtx, clazz, vm->builtins.biIterable.strings.iterator, error);
				if (ELOX_UNLIKELY(error->raised))
					return (ptr - ip);

				Value iteratorVal = runCall(runCtx, 0);
				if (ELOX_UNLIKELY(IS_EXCEPTION(iteratorVal))) {
					error->raised = true;
					return (ptr - ip);
				}

				pop(fiber);

				if (isObjType(iteratorVal, OBJ_INSTANCE) &&
					instanceOf((ObjKlass *)vm->builtins.biIterator.class_,
							   ((ObjInstance *)AS_OBJ(iteratorVal))->class_))
					iterator = (ObjInstance *)AS_OBJ(iteratorVal);
			}
		}
	}

	if (ELOX_UNLIKELY(iterator == NULL))
		ELOX_RAISE_RET_VAL(error, RTERR(runCtx, "Attempt to iterate non-iterable value"), ptr - ip);

	ObjClass *iteratorClass = iterator->class_;

	push(fiber, OBJ_VAL(iterator));
	bindMethod(runCtx, iteratorClass, vm->builtins.biIterator.strings.hasNext, error);
	if (ELOX_UNLIKELY(error->raised))
		return (ptr - ip);
	frame->slots[hasNextSlot + (hasNextPostArgs * frame->varArgs)] = pop(fiber);

	push(fiber, OBJ_VAL(iterator));
	bindMethod(runCtx, iteratorClass, vm->builtins.biIterator.strings.next, error);
	if (ELOX_UNLIKELY(error->raised))
		return (ptr - ip);
	frame->slots[nextSlot + (nextPostArgs * frame->varArgs)] = pop(fiber);

	return (ptr - ip);
}

static bool import(RunCtx *runCtx, ObjString *moduleName,
				   uint16_t numSymbols, uint8_t *args, Value *consts ELOX_UNUSED) {
	VM *vm = runCtx->vm;
	VMEnv *env = runCtx->vmEnv;
	ObjFiber *fiber = runCtx->activeFiber;

	bool loaded = false;
	if (tableFindString(&vm->modules,
						moduleName->string.chars, moduleName->string.length,
						moduleName->hash) != NULL) {
		// already loaded
		loaded = true;
	}

	if (!loaded) {
		EloxError error = ELOX_ERROR_INITIALIZER;
		String *strModuleName = &moduleName->string;
		bool found = false;

		for (EloxModuleLoader *mLoader = env->loaders; mLoader != NULL; mLoader++) {
			if (mLoader->loader == NULL)
				break;

			Value callable = mLoader->loader(runCtx, strModuleName, mLoader->options, &error);
			if (ELOX_UNLIKELY(error.raised))
				return false;

			if (!IS_NIL(callable)) {
				push(fiber, callable);

				tableSet(runCtx, &vm->modules, moduleName, BOOL_VAL(true), &error);
				if (ELOX_UNLIKELY(error.raised))
					return false;

				Value res = runCall(runCtx, 0);
				if (ELOX_UNLIKELY(IS_EXCEPTION(res)))
					return false;

				pop(fiber); // discard module result
				found = true;
				break;
			}
		}

		if (!found) {
			runtimeError(runCtx, NULL, "Could not find module '%s'", moduleName->string.chars);
			return false;
		}
	}

	uint8_t *sym = args;
	for (uint16_t i = 0; i < numSymbols; i++) {
		uint16_t symbol;
		memcpy(&symbol, sym, sizeof(uint16_t));
		sym += sizeof(uint16_t);
		Value value = vm->globalValues.values[symbol];
		if (ELOX_UNLIKELY(IS_UNDEFINED(value)))
			push(fiber, NIL_VAL);
		else
			push(fiber, value);
	}

	return true;
}

typedef enum {
	UPK_OP_READ_VAR,
	UPK_OP_INIT_TUPLE,
	UPK_OP_GET_NEXT,
	UPK_OP_SET_NEXT,
	UPK_OP_DONE
} UnpackOps;

typedef enum {
	UPK_VALUE,
	UPK_TUPLE,
	UPK_VARARGS,
	UPK_ITERATOR
} UnpackType;

typedef struct {
	UnpackOps op;
	struct {
		union {
			struct {
				ObjArray *tuple;
				int index;
			} tState;
			struct {
				ObjCallFrame *frame;
				int index;
			} vState;
			struct {
				Value hasNext;
				Value next;
			} iState;
		};
		bool hasNext;
		UnpackType type;
	} in;
	struct {
		VarScope type;
		union {
			struct {
				uint8_t slot;
				uint8_t postArgs;
			} local;
			struct {
				uint8_t slot;
			} upvalue;
			struct {
				uint16_t index;
			} global;
		};
		ObjArray *tuple;
		Value crtVal;
		unsigned int numRead;
	} out;
} UnpackState;

static void expand(RunCtx *runCtx, bool firstExpansion, EloxError *error) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	double prevVarArgs = 0;

	if (!firstExpansion)
		prevVarArgs = AS_NUMBER(pop(fiber));

	const Value expandable = pop(fiber);
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	PUSH_TEMP(temps, protectedExpandable, expandable);

	UnpackState state = {
		.in = {
			.type = UPK_VALUE,
			.hasNext = false
		}
	};
	unsigned int numExpanded = 0;

	VMTemp protectedHasNext = TEMP_INITIALIZER;
	VMTemp protectedNext = TEMP_INITIALIZER;

	if (isObjType(expandable, OBJ_TUPLE)) {
		state.in.type = UPK_TUPLE;
		state.in.tState.tuple = (ObjArray *)AS_OBJ(expandable);
		state.in.hasNext = state.in.tState.tuple->size > 0;
		state.in.tState.index = 0;
	} else if (isObjType(expandable, OBJ_FRAME)) {
		state.in.type = UPK_VARARGS;
		ObjCallFrame *frame = state.in.vState.frame = (ObjCallFrame *)AS_OBJ(expandable);
		state.in.hasNext = frame->varArgs > 0;
		state.in.vState.index = 0;
	} else if (isObjType(expandable, OBJ_INSTANCE) &&
			   instanceOf((ObjKlass *)vm->builtins.biIterator.class_,
						  ((ObjInstance *)AS_OBJ(expandable))->class_)) {
		state.in.type = UPK_ITERATOR;
		ObjInstance *iterator = (ObjInstance *)AS_OBJ(expandable);
		ObjClass *iteratorClass = iterator->class_;

		push(fiber, OBJ_VAL(iterator));
		bindMethod(runCtx, iteratorClass, vm->builtins.biIterator.strings.hasNext, error);
		if (ELOX_UNLIKELY(error->raised))
			goto cleanup;
		state.in.iState.hasNext = pop(fiber);
		pushTempVal(temps, &protectedHasNext, state.in.iState.hasNext);

		push(fiber, OBJ_VAL(iterator));
		bindMethod(runCtx, iteratorClass, vm->builtins.biIterator.strings.next, error);
		if (ELOX_UNLIKELY(error->raised))
			goto cleanup;
		state.in.iState.next = pop(fiber);
		pushTempVal(temps, &protectedNext, state.in.iState.next);

		push(fiber, state.in.iState.hasNext);
		Value hasNext = runCall(runCtx, 0);
		if (ELOX_UNLIKELY(IS_EXCEPTION(hasNext)))
			goto cleanup;
		pop(fiber);
		state.in.hasNext = AS_BOOL(hasNext);
	} else {
		// just a single value
		state.in.hasNext = true;
	}

	while (state.in.hasNext) {
		switch(state.in.type) {
			case UPK_VALUE:
				push(fiber, expandable);
				numExpanded++;
				state.in.hasNext = false;
				break;
			case UPK_TUPLE:
				push(fiber, arrayAt(state.in.tState.tuple, state.in.tState.index++));
				numExpanded++;
				state.in.hasNext = state.in.tState.index < state.in.tState.tuple->size;
				break;
			case UPK_VARARGS: {
				ObjCallFrame *frame = state.in.vState.frame;
				push(fiber, frame->slots[frame->fixedArgs + state.in.vState.index + 1]);
				state.in.vState.index++;
				numExpanded++;
				state.in.hasNext = state.in.vState.index < frame->varArgs;
				break;
			}
			case UPK_ITERATOR:
				push(fiber, state.in.iState.next);
				Value next = runCall(runCtx, 0);
				if (ELOX_UNLIKELY(IS_EXCEPTION(next)))
					goto cleanup;
				pop(fiber);

				push(fiber, next);
				numExpanded++;

				push(fiber, state.in.iState.hasNext);
				Value hasNext = runCall(runCtx, 0);
				if (ELOX_UNLIKELY(IS_EXCEPTION(hasNext)))
					goto cleanup;
				pop(fiber);
				state.in.hasNext = AS_BOOL(hasNext);

				break;
		}
	}

	push(fiber, NUMBER_VAL(prevVarArgs + numExpanded));

cleanup:
	releaseTemps(&temps);
}

bool getInstanceValue(ObjInstance *instance, ObjString *name, Value *value) {
	ObjClass *class_ = instance->class_;
	PropInfo fieldInfo = propTableGet(&class_->props, name, ELOX_PROP_FIELD_MASK);
	if (fieldInfo.type != ELOX_PROP_NONE) {
		*value = instance->fields[fieldInfo.index];
		return true;
	}
	return false;
};

static void getProperty(RunCtx * runCtx, ObjString *name, EloxError *error) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	Value targetVal = peek(fiber, 0);

	if (isObjType(targetVal, OBJ_INSTANCE)) {
		ObjInstance *instance = (ObjInstance *)AS_OBJ(targetVal);

		Value value;
		if (getInstanceValue(instance, name, &value)) {
			pop(fiber); // Instance
			push(fiber, value);
		} else {
			bindMethod(runCtx, instance->class_, name, error);
			if (ELOX_UNLIKELY(error->raised))
				return;
		}
	} else if (isObjType(targetVal, OBJ_CLASS)) {
		ObjClass *clazz = (ObjClass *)AS_OBJ(targetVal);
		PropInfo methodInfo = propTableGet(&clazz->props, name, ELOX_PROP_METHOD_MASK);
		if (methodInfo.type != ELOX_PROP_NONE) {
			pop(fiber); // class
			push(fiber, clazz->classData.values[methodInfo.index]);
			return;
		}
	} else {
		ObjClass *clazz = classOfFollowInstance(vm, targetVal);
		if (ELOX_LIKELY(clazz != NULL)) {
			bindMethod(runCtx, clazz, name, error);
			if (ELOX_UNLIKELY(error->raised))
				return;
		} else
			ELOX_RAISE_RET(error, RTERR(runCtx, "This value doesn't have properties"));
	}
}

static unsigned int doUnpack(RunCtx *runCtx, ObjCallFrame *frame, EloxError *error) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;
	uint8_t *ip = frame->ip;

	uint8_t *ptr = ip;

	uint8_t numVars = CHUNK_READ_BYTE(ptr);
	Value val = peek(fiber, 0);

	UnpackState state = {
		.op = UPK_OP_READ_VAR,
		.in = {
			.type = UPK_VALUE,
			.hasNext = false
		}
	};

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	VMTemp protectedHasNext = TEMP_INITIALIZER;
	VMTemp protectedNext = TEMP_INITIALIZER;
	VMTemp protectedTuple = TEMP_INITIALIZER;

	if (isObjType(val, OBJ_TUPLE)) {
		state.in.type = UPK_TUPLE;
		state.in.tState.tuple = (ObjArray *)AS_OBJ(val);
		state.in.hasNext = state.in.tState.tuple->size > 0;
		state.in.tState.index = 0;
	} else if (isObjType(val, OBJ_INSTANCE) &&
			   instanceOf((ObjKlass *)vm->builtins.biIterator.class_,
						  ((ObjInstance *)AS_OBJ(val))->class_)) {
		state.in.type = UPK_ITERATOR;
		ObjInstance *iterator = (ObjInstance *)AS_OBJ(val);
		ObjClass *iteratorClass = iterator->class_;

		push(fiber, OBJ_VAL(iterator));
		bindMethod(runCtx, iteratorClass, vm->builtins.biIterator.strings.hasNext, error);
		if (ELOX_UNLIKELY(error->raised))
			goto cleanup;
		state.in.iState.hasNext = pop(fiber);
		pushTempVal(temps, &protectedHasNext, state.in.iState.hasNext);

		push(fiber, OBJ_VAL(iterator));
		bindMethod(runCtx, iteratorClass, vm->builtins.biIterator.strings.next, error);
		if (ELOX_UNLIKELY(error->raised))
			goto cleanup;
		state.in.iState.next = pop(fiber);
		pushTempVal(temps, &protectedNext, state.in.iState.next);

		push(fiber, state.in.iState.hasNext);
		Value hasNext = runCall(runCtx, 0);
		if (ELOX_UNLIKELY(IS_EXCEPTION(hasNext))) {
			error->raised = true;
			goto cleanup;
		}
		pop(fiber);
		state.in.hasNext = AS_BOOL(hasNext);
	} else {
		// just a single value
		state.in.hasNext = true;
	}

	while (state.op != UPK_OP_DONE) {
		switch (state.op) {
			case UPK_OP_READ_VAR: {
				if (state.out.numRead >= numVars) {
					state.op = UPK_OP_DONE;
					break;
				}

				uint8_t type = CHUNK_READ_BYTE(ptr);
				state.out.type = type & 0xF;
				bool isRest = (type & 0xF0) != 0;
				switch (state.out.type) {
					case VAR_LOCAL:
						state.out.local.slot = CHUNK_READ_BYTE(ptr);
						state.out.local.postArgs = CHUNK_READ_BYTE(ptr);
						break;
					case VAR_UPVALUE:
						state.out.upvalue.slot = CHUNK_READ_BYTE(ptr);
						break;
					case VAR_GLOBAL:
						state.out.global.index = CHUNK_READ_USHORT(ptr);
						break;
					case VAR_BUILTIN:
						ELOX_RAISE_GOTO(error, RTERR(runCtx, "Cannot override builtins"), cleanup);
						break;
					case VAR_TUPLE:
						ELOX_UNREACHABLE();
						assert(false);
				}
				state.out.numRead++;
				state.op = isRest ? UPK_OP_INIT_TUPLE : UPK_OP_GET_NEXT;
				break;
			}
			case UPK_OP_INIT_TUPLE: {
				state.out.tuple = newArray(runCtx, 0, OBJ_TUPLE);
				ELOX_CHECK_RAISE_GOTO(state.out.tuple != NULL, error, OOM(runCtx), cleanup);
				pushTempVal(temps, &protectedTuple, OBJ_VAL(state.out.tuple));
				state.out.crtVal = OBJ_VAL(state.out.tuple);
				state.op = UPK_OP_SET_NEXT;
				break;
			}
			case UPK_OP_GET_NEXT: {
				bool stop = false;
				if (state.in.hasNext) {
					switch(state.in.type) {
						case UPK_VALUE:
							state.out.crtVal = val;
							state.in.hasNext = false;
							break;
						case UPK_TUPLE:
							state.out.crtVal = arrayAt(state.in.tState.tuple, state.in.tState.index++);
							state.in.hasNext = state.in.tState.index < state.in.tState.tuple->size;
							break;
						case UPK_ITERATOR:
							push(fiber, state.in.iState.next);
							Value next = runCall(runCtx, 0);
							if (ELOX_UNLIKELY(IS_EXCEPTION(next))) {
								error->raised = true;
								goto cleanup;
							}
							pop(fiber);
							state.out.crtVal = next;

							push(fiber, state.in.iState.hasNext);
							Value hasNext = runCall(runCtx, 0);
							if (ELOX_UNLIKELY(IS_EXCEPTION(hasNext))) {
								error->raised = true;
								goto cleanup;
							}
							pop(fiber);
							state.in.hasNext = AS_BOOL(hasNext);

							break;
						case UPK_VARARGS:
							ELOX_UNREACHABLE();
							assert(false);
					}
				} else {
					if (state.out.tuple != NULL) {
						stop = true;
					} else {
						state.out.crtVal = NIL_VAL;
					}
				}
				state.op = stop ? UPK_OP_DONE : UPK_OP_SET_NEXT;
				break;
			}
			case UPK_OP_SET_NEXT: {
				switch (state.out.type) {
					case VAR_LOCAL:
						frame->slots[state.out.local.slot + (state.out.local.postArgs * frame->varArgs)] =
							state.out.crtVal;
						break;
					case VAR_UPVALUE:
						*frame->closure->upvalues[state.out.upvalue.slot]->location = state.out.crtVal;
						break;
					case VAR_GLOBAL:
						vm->globalValues.values[state.out.global.index] = state.out.crtVal;
						break;
					case VAR_BUILTIN:
						ELOX_UNREACHABLE();
						assert(false);
					case VAR_TUPLE: {
						bool ret = appendToArray(runCtx, state.out.tuple, state.out.crtVal);
						ELOX_CHECK_RAISE_GOTO(ret, error, OOM(runCtx), cleanup);
						break;
					}
				}
				if (state.out.tuple == NULL)
					state.op = UPK_OP_READ_VAR;
				else {
					state.op = UPK_OP_GET_NEXT;
					state.out.type = VAR_TUPLE;
				}
				break;
			}
			case UPK_OP_DONE:
				ELOX_UNREACHABLE();
				assert(false);
		}
	}

cleanup:

	releaseTemps(&temps);

	return ptr - ip;
}

#ifdef ELOX_DEBUG_TRACE_EXECUTION
void printStack(RunCtx *runCtx) {
	ObjFiber *fiber = runCtx->activeFiber;

	eloxPrintf(runCtx, ELOX_IO_DEBUG, "          %p>", fiber);
	ObjCallFrame *frame = fiber->activeFrame;
	for (Value *slot = fiber->stack; slot < fiber->stackTop; slot++) {
		if (frame && (slot == frame->slots))
			ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "|");
		ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "[ ");
		printValue(runCtx, ELOX_IO_DEBUG, *slot);
		ELOX_WRITE(runCtx, ELOX_IO_DEBUG, " ]");
	}
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "\n");
}
#endif

Value runCall(RunCtx *runCtx, int argCount) {
	ObjFiber *fiber = runCtx->activeFiber;

	Value callable = peek(fiber, argCount);
#ifdef ELOX_DEBUG_TRACE_EXECUTION
	static uint32_t callIndex = 0;
	uint32_t callId = callIndex++;
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%08x--->", callId);
	printValue(runCtx, ELOX_IO_DEBUG, callable);
	printStack(runCtx);
#endif

	CallResult ret = callValue(runCtx, callable, argCount);
	if (ELOX_UNLIKELY(!ret.result)) {
#ifdef ELOX_DEBUG_TRACE_EXECUTION
		eloxPrintf(runCtx, ELOX_IO_DEBUG, "%08x<---\n", callId);
		printStack(runCtx);
#endif
		return EXCEPTION_VAL;
	}
	if (ret.wasNative) {
		// Native function already returned
#ifdef ELOX_DEBUG_TRACE_EXECUTION
		eloxPrintf(runCtx, ELOX_IO_DEBUG, "%08x<---\n", callId);
		printStack(runCtx);
#endif
		return peek(fiber, 0);
	}
	ObjCallFrame *activeFrame = fiber->activeFrame;
	activeFrame->type = ELOX_FT_INTERNAL_CALL_START;
	EloxInterpretResult res = run(runCtx);
#ifdef ELOX_DEBUG_TRACE_EXECUTION
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%08x<---\n", callId);
	printStack(runCtx);
#endif
	if (ELOX_UNLIKELY(res == ELOX_INTERPRET_RUNTIME_ERROR))
		return EXCEPTION_VAL;

	return peek(fiber, 0);
}

bool runChunk(RunCtx *runCtx) {
	EloxInterpretResult res = run(runCtx);
	return res == ELOX_INTERPRET_OK;
}

typedef enum {
#define ELOX_OPCODES_INLINE
#define OPCODE(name) ADD_OP_##name,
#include "ops/addOps.h"
#undef OPCODE
#undef ELOX_OPCODES_INLINE
} ELOX_PACKED AddOps;

static bool concatenate(RunCtx *runCtx) {
	ObjFiber *fiber = runCtx->activeFiber;

	ObjString *b = AS_STRING(peek(fiber, 0));
	ObjString *a = AS_STRING(peek(fiber, 1));

	int length = a->string.length + b->string.length;
	uint8_t *chars = ALLOCATE(runCtx, uint8_t, length + 1);
	if (ELOX_UNLIKELY(chars == NULL)) {
		oomError(runCtx, NULL);
		return false;
	}
	memcpy(chars, a->string.chars, a->string.length);
	memcpy(chars + a->string.length, b->string.chars, b->string.length);
	chars[length] = '\0';

	ObjString *result = takeString(runCtx, chars, length, length + 1);
	if (ELOX_UNLIKELY(result == NULL)) {
		FREE(runCtx, uint8_t, chars);
		oomError(runCtx, NULL);
		return false;
	}
	popn(fiber, 2);
	push(fiber, OBJ_VAL(result));

	return true;
}

#ifdef ELOX_ENABLE_COMPUTED_GOTO

#define DISPATCH_START(instruction)      goto *dispatchTable[instruction];
#define DISPATCH_CASE(name)              opcode_##name
#define DISPATCH_BREAK                   goto dispatchLoop
#define DISPATCH_END

#else

#define DISPATCH_START(instruction) switch(instruction) {
#define DISPATCH_CASE(name)         case OP_##name
#define DISPATCH_BREAK              break
#define DISPATCH_END }

#endif // ELOX_ENABLE_COMPUTED_GOTO

EloxInterpretResult run(RunCtx *runCtx) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;
	ObjCallFrame *frame = fiber->activeFrame;
	EloxError error = ELOX_ERROR_INITIALIZER;
	register uint8_t *ip = frame->ip;

#ifdef ELOX_ENABLE_COMPUTED_GOTO
	#define ELOX_OPCODES_INLINE

	static void *dispatchTable[] = {
		#define OPCODE(name) &&opcode_##name,
		#include "elox/opcodes.h"
		#undef OPCODE
	};

	static void *ADDDispatchTable[] = {
		#define OPCODE(name) &&ADD_opcode_##name,
		#include "ops/addOps.h"
	};

	#undef ELOX_OPCODES_INLINE
	#undef OPCODE
#endif // ELOX_ENABLE_COMPUTED_GOTO

#define READ_BYTE() (*ip++)
	uint16_t _readu16tmp;
	int32_t _readi32tmp;
#define READ_USHORT() \
	(memcpy(&_readu16tmp, ip, sizeof(uint16_t)), ip += sizeof(uint16_t), _readu16tmp)
#define READ_INT() \
	(memcpy(&_readi32tmp, ip, sizeof(int32_t)), ip += sizeof(int32_t), _readi32tmp)
#define READ_CONST8() \
	(frame->function->chunk.constants.values[READ_BYTE()])
#define READ_CONST16(tmp) \
	(frame->function->chunk.constants.values[READ_USHORT(tmp)])
#define READ_ARRAY(n, s) \
	(ip += (n) * (s), ip - (n) * (s) )
#define READ_STRING16(tmp) AS_STRING(READ_CONST16(tmp))
#define BINARY_OP(valueType, op) \
	do { \
		if (ELOX_UNLIKELY(!IS_NUMBER(peek(fiber, 0)) || !IS_NUMBER(peek(fiber, 1)))) { \
			frame->ip = ip; \
			runtimeError(runCtx, NULL, "Operands must be numbers"); \
			goto throwException; \
		} \
		double b = AS_NUMBER(pop(fiber)); \
		double a = AS_NUMBER(pop(fiber)); \
		push(fiber, valueType(a op b)); \
	} while (false)

	ELOX_ALIGN(32);
	for (;;) {
#ifdef ELOX_ENABLE_COMPUTED_GOTO
dispatchLoop: ;
#endif

#ifdef ELOX_DEBUG_TRACE_EXECUTION
		printStack(runCtx);

		disassembleInstruction(runCtx, &frame->function->chunk,
							   (int)(ip - frame->function->chunk.code));
#endif
		//usleep(100000);
		uint8_t instruction = READ_BYTE();
		DISPATCH_START(instruction)
			DISPATCH_CASE(CONST8): {
				Value constant = READ_CONST8();
				push(fiber, constant);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CONST16): {
				Value constant = READ_CONST16();
				push(fiber, constant);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(IMMI): {
				push(fiber, NUMBER_VAL(READ_INT()));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(NIL):
				push(fiber, NIL_VAL);
				DISPATCH_BREAK;
			DISPATCH_CASE(TRUE):
				push(fiber, BOOL_VAL(true));
				DISPATCH_BREAK;
			DISPATCH_CASE(FALSE):
				push(fiber, BOOL_VAL(false));
				DISPATCH_BREAK;
			DISPATCH_CASE(POP):
				pop(fiber);
				DISPATCH_BREAK;
			DISPATCH_CASE(POPN): {
				uint8_t n = READ_BYTE();
				popn(fiber, n);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SWAP): {
				Value b = pop(fiber);
				Value a = pop(fiber);
				push(fiber, b);
				push(fiber, a);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(EXPAND): {
				bool firstExpansion = READ_BYTE();
				frame->ip = ip;
				expand(runCtx, firstExpansion, &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(PEEK): {
				uint8_t offset = READ_BYTE();
				push(fiber, peek(fiber, offset));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_LOCAL): {
				uint8_t slot = READ_BYTE();
				uint8_t postArgs = READ_BYTE();
				push(fiber, frame->slots[slot + (postArgs * frame->varArgs)]);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_VARARGS): {
				push(fiber, OBJ_VAL(frame));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_GLOBAL): {
				Value value = vm->globalValues.values[READ_USHORT()];
				if (ELOX_UNLIKELY(IS_UNDEFINED(value))) {
					frame->ip = ip;
					runtimeError(runCtx, NULL, "Undefined global variable");
					goto throwException;
				}
				push(fiber, value);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_BUILTIN): {
				Value value = vm->builtinValues.values[READ_USHORT()];
				push(fiber, value);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(DEFINE_GLOBAL): {
				vm->globalValues.values[READ_USHORT()] = pop(fiber);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_LOCAL): {
				uint8_t slot = READ_BYTE();
				uint8_t postArgs = READ_BYTE();
				frame->slots[slot + (postArgs * frame->varArgs)] = peek(fiber, 0);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_GLOBAL): {
				uint16_t index = READ_USHORT();
				if (ELOX_UNLIKELY(IS_UNDEFINED(vm->globalValues.values[index]))) {
					frame->ip = ip;
					runtimeError(runCtx, NULL, "Undefined global variable");
					goto throwException;
				}
				vm->globalValues.values[index] = peek(fiber, 0);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_UPVALUE): {
				uint8_t slot = READ_BYTE();
				push(fiber, *frame->closure->upvalues[slot]->location);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_UPVALUE): {
				uint8_t slot = READ_BYTE();
				*frame->closure->upvalues[slot]->location = peek(fiber, 0);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_PROP): {
				ObjString *name = READ_STRING16();
				frame->ip = ip;
				getProperty(runCtx, name, &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_REF): {
				uint16_t propRef = READ_USHORT();
				ObjInstance *instance = (ObjInstance *)AS_OBJ(peek(fiber, 0));
				ObjFunction *frameFunction = frame->function;
				ObjClass *parentClass = frameFunction->parentClass;
				Ref *ref = &parentClass->refs[propRef + frameFunction->refOffset];
				Value result;
				if (ref->isMethod) {
					Value method = instance->tables[ref->tableIndex][ref->propIndex];
					ObjBoundMethod *bound = newBoundMethod(runCtx, peek(fiber, 0),
														   (ObjMethod *)AS_OBJ(method));
					if (ELOX_UNLIKELY(bound == NULL)) {
						push(fiber, OBJ_VAL(vm->builtins.oomError));
						goto throwException;
					}
					result = OBJ_VAL(bound);
				} else {
					result = instance->tables[ref->tableIndex][ref->propIndex];
#ifdef ELOX_DEBUG_TRACE_EXECUTION
				eloxPrintf(runCtx, ELOX_IO_DEBUG, "%p[%s][%u]->",
						   instance, DataTableNames[ref->tableIndex], ref->propIndex);
				printValue(runCtx, ELOX_IO_DEBUG, result);
				eloxPrintf(runCtx, ELOX_IO_DEBUG, "\n");
#endif
				}
				pop(fiber); // Instance
				push(fiber, result);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(MAP_GET): {
				Value instanceVal = peek(fiber, 0);

				ObjString *name = READ_STRING16();

				if (ELOX_LIKELY(isObjType(instanceVal, OBJ_HASHMAP))) {
					ObjHashMap *map = (ObjHashMap *)AS_OBJ(instanceVal);

					Value value;
					frame->ip = ip;
					bool found = valueTableGet(runCtx, &map->items, OBJ_VAL(name), &value, &error);
					if (ELOX_UNLIKELY(error.raised))
						goto throwException;
					if (!found)
						value = NIL_VAL;
					pop(fiber); // map
					push(fiber, value);
				} else {
					frame->ip = ip;
					runtimeError(runCtx, NULL, "Argument is not a map");
					goto throwException;
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_PROP): {
				Value instanceVal = peek(fiber, 1);

				if (ELOX_LIKELY(isObjType(instanceVal, OBJ_INSTANCE))) {
					ObjInstance *instance = (ObjInstance *)AS_OBJ(instanceVal);
					ObjString *fieldName = READ_STRING16();
					if (ELOX_UNLIKELY(!setInstanceField(instance, fieldName, peek(fiber, 0)))) {
						frame->ip = ip;
						runtimeError(runCtx, NULL, "Undefined field '%s'", fieldName->string.chars);
						goto throwException;
					}
					Value value = pop(fiber);
					pop(fiber);
					push(fiber, value);
				} else {
					frame->ip = ip;
					runtimeError(runCtx, NULL, "Only instances have fields");
					goto throwException;
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_REF): {
				uint16_t propRef = READ_USHORT();
				ObjInstance *instance = (ObjInstance *)AS_OBJ(peek(fiber, 1));
				ObjFunction *frameFunction = frame->function;
				ObjClass *parentClass = frameFunction->parentClass;
				Ref *ref = &parentClass->refs[propRef + frameFunction->refOffset];
				instance->tables[ref->tableIndex][ref->propIndex] = peek(fiber, 0);
#ifdef ELOX_DEBUG_TRACE_EXECUTION
				eloxPrintf(runCtx, ELOX_IO_DEBUG, "%p[%s][%u]<-",
						   instance, DataTableNames[ref->tableIndex], ref->propIndex);
				printValue(runCtx, ELOX_IO_DEBUG, peek(fiber, 0));
				eloxPrintf(runCtx, ELOX_IO_DEBUG, "\n");
#endif
				Value value = pop(fiber);
				pop(fiber);
				push(fiber, value);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(MAP_SET): {
				Value instanceVal = peek(fiber, 1);
				if (ELOX_LIKELY(isObjType(instanceVal, OBJ_HASHMAP))) {
					ObjHashMap *map = (ObjHashMap *)AS_OBJ(instanceVal);
					ObjString *index = READ_STRING16();
					Value value = peek(fiber, 0);
					frame->ip = ip;
					valueTableSet(runCtx, &map->items, OBJ_VAL(index), value, &error);
					if (ELOX_UNLIKELY(error.raised))
						goto throwException;
					value = pop(fiber);
					pop(fiber);
					push(fiber, value);
				} else {
					frame->ip = ip;
					runtimeError(runCtx, NULL, "Argument is not a map");
					goto throwException;
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(EQUAL): {
				Value b = pop(fiber);
				Value a = pop(fiber);
				bool eq = valuesEquals(runCtx, a, b, &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				push(fiber, BOOL_VAL(eq));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GREATER):
				BINARY_OP(BOOL_VAL, >);
				DISPATCH_BREAK;
			DISPATCH_CASE(LESS):
				BINARY_OP(BOOL_VAL, <);
				DISPATCH_BREAK;
			DISPATCH_CASE(ADD): {
				static const AddOps addTable[VTYPE_MAX][VTYPE_MAX] = {
					[VTYPE_NUMBER][VTYPE_NUMBER] = ADD_OP_NUMBER_NUMBER,
					[VTYPE_OBJ_STRING][VTYPE_OBJ_STRING] = ADD_OP_STRING_STRING
				};

				uint32_t aType = valueTypeId(peek(fiber, 0));
				uint32_t bType = valueTypeId(peek(fiber, 1));

				AddOps op = addTable[aType][bType];

				#define OPNAME ADD
				#include "ops/opsInit.h"

				OP_DISPATCH_START(op)
					OP_DISPATCH_CASE(NUMBER_NUMBER): {
						double b = AS_NUMBER(pop(fiber));
						double a = AS_NUMBER(pop(fiber));
						push(fiber, NUMBER_VAL(a + b));
						OP_DISPATCH_BREAK;
					}
					OP_DISPATCH_CASE(STRING_STRING):
						if (ELOX_UNLIKELY(!concatenate(runCtx)))
							goto throwException;
						OP_DISPATCH_BREAK;
					OP_DISPATCH_CASE(UNDEFINED):
						frame->ip = ip;
						runtimeError(runCtx, NULL, "Operands must be two numbers or two strings");
						goto throwException;
						OP_DISPATCH_BREAK;
				OP_DISPATCH_END

				#include "ops/opsCleanup.h"

				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SUBTRACT):
				BINARY_OP(NUMBER_VAL, -);
				DISPATCH_BREAK;
			DISPATCH_CASE(MULTIPLY):
				BINARY_OP(NUMBER_VAL, *);
				DISPATCH_BREAK;
			DISPATCH_CASE(DIVIDE):
				BINARY_OP(NUMBER_VAL, /);
				DISPATCH_BREAK;
			DISPATCH_CASE(MODULO): {
				if (!IS_NUMBER(peek(fiber, 0)) || !IS_NUMBER(peek(fiber, 1))) {
					frame->ip = ip;
					runtimeError(runCtx, NULL, "Operands must be numbers");
					goto throwException;
				}
				double b = AS_NUMBER(pop(fiber));
				double a = AS_NUMBER(pop(fiber));
				push(fiber, NUMBER_VAL(fmod(a, b)));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INSTANCEOF): {
				if (ELOX_UNLIKELY(!IS_KLASS(peek(fiber, 0)))) {
					frame->ip = ip;
					runtimeError(runCtx, NULL, "Right-hand operand must be a class or interface");
					goto throwException;
				}
				ObjKlass *klass = (ObjKlass *)AS_OBJ(pop(fiber));
				ObjClass *instClass = classOfFollowInstance(vm, pop(fiber));
				if (instClass != NULL)
					push(fiber, BOOL_VAL(instanceOf(klass, instClass)));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(IN):
				frame->ip = ip;
				bool in = valIn(runCtx, &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				push(fiber, BOOL_VAL(in));

				DISPATCH_BREAK;
			DISPATCH_CASE(NOT):
				push(fiber, BOOL_VAL(isFalsey(pop(fiber))));
				DISPATCH_BREAK;
			DISPATCH_CASE(NEGATE):
				if (ELOX_UNLIKELY(!IS_NUMBER(peek(fiber, 0)))) {
					frame->ip = ip;
					runtimeError(runCtx, NULL, "Operand must be a number");
					goto throwException;
				}
				push(fiber, NUMBER_VAL(-AS_NUMBER(pop(fiber))));
				DISPATCH_BREAK;
			DISPATCH_CASE(JUMP): {
				uint16_t offset = READ_USHORT();
				ip += offset;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(JUMP_IF_FALSE): {
				uint16_t offset = READ_USHORT();
				if (isFalsey(peek(fiber, 0)))
					ip += offset;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(LOOP): {
				uint16_t offset = READ_USHORT();
				ip -= offset;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CALL): {
				int argCount = READ_BYTE();
				bool hasExpansions = READ_BYTE();
				if (hasExpansions)
					argCount += AS_NUMBER(pop(fiber));
				frame->ip = ip;
				if (ELOX_UNLIKELY(!callValue(runCtx, peek(fiber, argCount), argCount).result))
					goto throwException;
				frame = fiber->activeFrame;
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INVOKE): {
				ObjString *methodName = READ_STRING16();
				int argCount = READ_BYTE();
				bool hasExpansions = READ_BYTE();
				if (hasExpansions)
					argCount += AS_NUMBER(pop(fiber));
				frame->ip = ip;
				if (ELOX_UNLIKELY(!invoke(runCtx, methodName, argCount)))
					goto throwException;
				fiber = runCtx->activeFiber;
				frame = fiber->activeFrame;
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INVOKE_REF): {
				uint16_t propRef = READ_USHORT();
				int argCount = READ_BYTE();
				bool hasExpansions = READ_BYTE();
				if (hasExpansions)
					argCount += AS_NUMBER(pop(fiber));
				ObjInstance *instance = (ObjInstance *)AS_OBJ(peek(fiber, argCount));
				ObjFunction *frameFunction = frame->function;
				Ref *ref = &instance->class_->refs[propRef + frameFunction->refOffset];
				bool isMethod = ref->isMethod;
				frame->ip = ip;
				if (ELOX_UNLIKELY(!invokeMember(runCtx,
												&instance->tables[ref->tableIndex][ref->propIndex],
												isMethod, argCount)))
					goto throwException;
				fiber = runCtx->activeFiber;
				frame = fiber->activeFrame;
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SUPER_INIT): {
				int argCount = READ_BYTE();
				bool hasExpansions = READ_BYTE();
				ObjClass *superclass = (ObjClass *)AS_OBJ(pop(fiber));
				Value init = superclass->initializer;
				if (!IS_NIL(init)) {
#ifdef ELOX_DEBUG_TRACE_EXECUTION
					eloxPrintf(runCtx, ELOX_IO_DEBUG, "===>%s init\n", superclass->name->string.chars);
#endif
					if (hasExpansions)
						argCount += AS_NUMBER(pop(fiber));
					frame->ip = ip;
					if (!callMethod(runCtx, AS_OBJ(init), argCount, 0).result)
						goto throwException;
					frame = fiber->activeFrame;
					ip = frame->ip;
					pop(fiber); // this
				} else {
					// no return, discard arguments
					//vm->stackTop = frame->slots;
					// if there is no initializer, the compiler-generated POP
					// will discard 'this'
				}
				//pop(fiber); // this
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CLOSURE): {
				ObjFunction *function = (ObjFunction *)AS_OBJ(READ_CONST16());
				ObjClosure *closure = newClosure(runCtx, function);
				if (ELOX_UNLIKELY(closure == NULL)) {
					push(fiber, OBJ_VAL(vm->builtins.oomError));
					goto throwException;
				}
				push(fiber, OBJ_VAL(closure));
				for (int i = 0; i < closure->upvalueCount; i++) {
					uint8_t isLocal = READ_BYTE();
					uint8_t index = READ_BYTE();
					if (isLocal) {
						closure->upvalues[i] = captureUpvalue(runCtx, frame->slots + index);
						if (ELOX_UNLIKELY(closure->upvalues[i] == NULL)) {
							push(fiber, OBJ_VAL(vm->builtins.oomError));
							goto throwException;
						}
#ifdef ELOX_DEBUG_TRACE_EXECUTION
						eloxPrintf(runCtx, ELOX_IO_DEBUG, "<<< %d (", i);
						printValue(runCtx, ELOX_IO_DEBUG, frame->slots[index]);
						ELOX_WRITE(runCtx, ELOX_IO_DEBUG, ")\n");
#endif
					} else
						closure->upvalues[i] = frame->closure->upvalues[index];
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CLOSE_UPVALUE):
				closeUpvalues(runCtx, fiber->stackTop - 1);
				pop(fiber);
				DISPATCH_BREAK;
			DISPATCH_CASE(RETURN): {
				Value result = peek(fiber, 0);
				closeUpvalues(runCtx, frame->slots);
				ObjCallFrame *activeFrame = fiber->activeFrame;
				releaseCallFrame(runCtx, fiber);

				fiber->stackTop = frame->slots - frame->argOffset;
				push(fiber, result);

				switch (activeFrame->type) {
					case ELOX_FT_INTER:
						frame = fiber->activeFrame;
						ip = frame->ip;
						break;
					case ELOX_FT_INTERNAL_CALL_START:
						return ELOX_INTERPRET_OK;
					case ELOX_FT_FIBER_START: {
						ObjFiber *parent = fiber->parent;
						// resume() is native, so parent frame already released
						pop(parent); // result from native invocation
						push(parent, peek(fiber, 0));
						fiber->state = ELOX_FIBER_TERMINATED;
						fiber->parent = NULL;
						parent->state = (parent->function == NULL) ?
										ELOX_FIBER_DETACHED : ELOX_FIBER_RUNNING;
						runCtx->activeFiber = parent;
						fiber = parent;
						frame = fiber->activeFrame;
						ip = frame->ip;
						break;
					}
				}

				DISPATCH_BREAK;
			}
			DISPATCH_CASE(END):
				return ELOX_INTERPRET_OK;
				DISPATCH_BREAK;
			DISPATCH_CASE(INTF): {
				ObjString *name = READ_STRING16();
				ObjInterface *intf = newInterface(runCtx, name);
				if (ELOX_UNLIKELY(intf == NULL)) {
					push(fiber, OBJ_VAL(vm->builtins.oomError));
					goto throwException;
				}
				push(fiber, OBJ_VAL(intf));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CLASS): {
				ObjString *name = READ_STRING16();
				bool abstract = READ_BYTE();
				ObjClass *clazz = newClass(runCtx, name, abstract);
				if (ELOX_UNLIKELY(clazz == NULL)) {
					push(fiber, OBJ_VAL(vm->builtins.oomError));
					goto throwException;
				}
				push(fiber, OBJ_VAL(clazz));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INHERIT):
				frame->ip = ip;
				ip += inherit(runCtx, ip, &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			DISPATCH_CASE(ABS_METHOD): {
				frame->ip = ip;
				ObjString *methodName = READ_STRING16();
				uint8_t parentOffset = READ_BYTE();
				uint8_t arity = READ_BYTE();
				uint8_t hasVarargs = READ_BYTE();
				Obj *parent = AS_OBJ(peek(fiber, parentOffset));
				addAbstractMethod(runCtx, parent, methodName, arity, hasVarargs, &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(DEFAULT_METHOD):
				frame->ip = ip;
				ip += defineDefaultMethod(runCtx, frame, &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			DISPATCH_CASE(METHOD):
				frame->ip = ip;
				ip += defineMethod(runCtx, frame, &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			DISPATCH_CASE(FIELD):
				frame->ip = ip;
				defineField(runCtx, READ_STRING16(), &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			DISPATCH_CASE(STATIC):
				frame->ip = ip;
				defineStatic(runCtx, READ_STRING16(), &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			DISPATCH_CASE(CLOSE_CLASS):
				frame->ip = ip;
				ip += closeClass(runCtx, frame, &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			DISPATCH_CASE(NEW_ARRAY):
				frame->ip = ip;
				ip += buildArray(runCtx, ip, OBJ_ARRAY, &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			DISPATCH_CASE(NEW_TUPLE):
				frame->ip = ip;
				ip += buildArray(runCtx, ip, OBJ_TUPLE, &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			DISPATCH_CASE(INDEX):
				frame->ip = ip;
				if (ELOX_UNLIKELY(!indexValue(runCtx)))
					goto throwException;
				DISPATCH_BREAK;
			DISPATCH_CASE(INDEX_STORE):
				frame->ip = ip;
				if (ELOX_UNLIKELY(!indexStore(runCtx)))
					goto throwException;
				DISPATCH_BREAK;
			DISPATCH_CASE(SLICE):
				frame->ip = ip;
				if (ELOX_UNLIKELY(!sliceValue(runCtx)))
					goto throwException;
				DISPATCH_BREAK;
			DISPATCH_CASE(NEW_MAP): {
				uint16_t itemCount = READ_USHORT();

				frame->ip = ip;
				if (ELOX_UNLIKELY(!buildMap(runCtx, itemCount)))
					goto throwException;

				DISPATCH_BREAK;
			}
			DISPATCH_CASE(THROW): {
throwException:
				frame->ip = ip;
				Value stacktrace = getStackTrace(runCtx);

				DBG_PRINT_STACK("EXC", runCtx);

				ObjInstance *instance = (ObjInstance *)AS_OBJ(peek(fiber, 0));
				push(fiber, stacktrace);
				// TODO: check copyString
				ObjString *stacktraceName = copyString(runCtx, ELOX_USTR_AND_LEN("stacktrace"));
				push(fiber, OBJ_VAL(stacktraceName));
				setInstanceField(instance, stacktraceName, stacktrace);
				popn(fiber, 2);
				vm->handlingException++;
				ObjCallFrame *startFrame = propagateException(runCtx);
				fiber = runCtx->activeFiber;
				if (startFrame == NULL) {
					// exception handled
					vm->handlingException--;
					frame = fiber->activeFrame;
					ip = frame->ip;
					error.raised = false;
					DISPATCH_BREAK;
				}
				vm->handlingException--;

				// unroll call stack
				// TODO: check if propagateException guarantees below
				//fiber->frameCount = exitFrame;
				ObjCallFrame *retFrame = fiber->activeFrame;
				fiber->stackTop = retFrame->slots + 1;
				releaseCallFrame(runCtx, fiber);
				// set exception as result
				push(fiber, OBJ_VAL(instance));

				return ELOX_INTERPRET_RUNTIME_ERROR;
			}
			DISPATCH_CASE(PUSH_EXH): {
				uint16_t handlerTableAddress = READ_USHORT();
				frame->ip = ip;
				if (ELOX_UNLIKELY(!pushExceptionHandler(runCtx, handlerTableAddress)))
					goto throwException;
#ifdef ELOX_DEBUG_TRACE_EXECUTION
				eloxPrintf(runCtx, ELOX_IO_DEBUG, "EXH -> %d\n", frame->tryDepth);
#endif
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(UNROLL_EXH): {
				uint8_t newHandlerCount = READ_BYTE();
				uint8_t preserve = READ_BYTE();
				unrollExceptionHandlerStack(runCtx, newHandlerCount, (bool)preserve);
#ifdef ELOX_DEBUG_TRACE_EXECUTION
				eloxPrintf(runCtx, ELOX_IO_DEBUG, "EXH <- %d\n", frame->tryDepth);
#endif
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(FOREACH_INIT): {
				frame->ip = ip;
				ip += foreachInit(runCtx, frame, &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(UNPACK): {
				frame->ip = ip;
				ip += doUnpack(runCtx, frame, &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(IMPORT): {
				ObjString *moduleName = READ_STRING16();
				uint16_t numArgs = READ_USHORT();
				uint8_t *args = READ_ARRAY(numArgs, 2);
				frame->ip = ip;
				if (ELOX_UNLIKELY(!import(runCtx, moduleName, numArgs, args,
										  frame->function->chunk.constants.values))) {
					goto throwException;
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(DATA): {
				frame->ip = ip;
				runtimeError(runCtx, NULL, "Attempted to execute data section");
				goto throwException;
			}
		DISPATCH_END
	}

#undef READ_BYTE
#undef READ_I8
#undef READ_USHORT
#undef READ_CONST16
#undef READ_CONST8
#undef READ_STRING16
#undef READ_ARRAY
#undef BINARY_OP
}

EloxCompilerHandle *getCompiler(RunCtx *runCtx) {
	VM *vm = runCtx->vm;

	EloxCompilerHandle *handle = ALLOCATE(runCtx, EloxCompilerHandle, 1);
	if (ELOX_UNLIKELY(handle == NULL))
		return NULL;

	handle->base.runCtx = runCtx;
	handle->base.type = COMPILER_HANDLE;

	CompilerState *compilerState = &handle->compilerState;
	compilerState->fileName = NULL;
	compilerState->currentFunctionCompiler = NULL;

	handleSetAdd(&vm->handles, (EloxHandle *)handle);

	return handle;
}

void eloxPrintException(RunCtx *runCtx) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	Value ex = peek(fiber, 0);
	size_t savedStack = saveStack(fiber);
	ObjClass *exClass = classOfFollowInstance(vm, ex);
	if (instanceOf((ObjKlass *)vm->builtins.biException.class_, exClass)) {
		push(fiber, OBJ_VAL(vm->builtins.biException.methods.printStackTrace));
		push(fiber, ex);
		Value res = runCall(runCtx, 1);
		if (ELOX_UNLIKELY(IS_EXCEPTION(res))) {
			// discard exception
			restoreStack(fiber, savedStack);
			eloxPrintf(runCtx, ELOX_IO_ERR, "Error printing exception\n");
			return;
		}
		pop(fiber);
	} else {
		EloxError error = ELOX_ERROR_INITIALIZER;
		Value exStrVal = toString(runCtx, ex, &error);
		if (ELOX_UNLIKELY(error.raised)) {
			error.discardException(fiber, savedStack);
			eloxPrintf(runCtx, ELOX_IO_ERR, "Error printing exception\n");
			return;
		}
		ObjString *exStr = AS_STRING(exStrVal);
		eloxPrintf(runCtx, ELOX_IO_ERR, "%.*s\n", exStr->string.length, exStr->string.chars);
	}
}

EloxInterpretResult interpret(RunCtx *runCtx, uint8_t *source, const String *fileName,
							  const String *moduleName) {
	ObjFiber *fiber = runCtx->activeFiber;

	ObjFunction *function = compile(runCtx, source, fileName, moduleName);
	if (function == NULL)
		return ELOX_INTERPRET_COMPILE_ERROR;

	push(fiber, OBJ_VAL(function));
	callFunction(runCtx, function, 0, 0);

	DBG_PRINT_STACK("DBGa", runCtx);

	ObjCallFrame *activeFrame = fiber->activeFrame;
	activeFrame->type = ELOX_FT_INTERNAL_CALL_START;
	EloxInterpretResult res = run(runCtx);
	if (res == ELOX_INTERPRET_RUNTIME_ERROR) {
		eloxPrintf(runCtx, ELOX_IO_ERR, "Unhandled exception: ");
		eloxPrintException(runCtx);
#if 0
		ObjInstance *exception = AS_INSTANCE(peek(fiber, 0));

		eloxPrintf(runCtx, ELOX_IO_ERR, "Unhandled exception %s", exception->clazz->name->string.chars);

		Value message;
		// TODO: check copyString
		if (getInstanceValue(exception, copyString(runCtx, ELOX_USTR_AND_LEN("message")), &message))
			eloxPrintf(runCtx, ELOX_IO_ERR, ": %s\n", AS_CSTRING(message));
		else
			ELOX_WRITE(runCtx, ELOX_IO_ERR, "\n");
		/*Value stacktrace;
		if (getInstanceValue(exception, copyString(runCtx, ELOX_USTR_AND_LEN("stacktrace")), &stacktrace))
			eloxPrintf(runCtx, ELOX_IO_ERR, "%s", AS_CSTRING(stacktrace));*/
#endif
	}
	pop(fiber);

	return res;
}
