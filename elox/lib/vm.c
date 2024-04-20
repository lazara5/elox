// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/util.h"
#include "elox/compiler.h"
#include "elox/object.h"
#include "elox/memory.h"
#include "elox/state.h"
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

static inline ObjFunction *getFrameFunction(CallFrame *frame) {
	switch (frame->function->type) {
		case OBJ_FUNCTION:
			return (ObjFunction *)frame->function;
		case OBJ_CLOSURE:
			return ((ObjClosure *)frame->function)->function;
		default:
			return NULL;
	}
}

static inline ObjFunction *getValueFunction(Value value) {
	assert(IS_OBJ(value));
	Obj *objVal = AS_OBJ(value);
	switch (objVal->type) {
		case OBJ_FUNCTION:
			return AS_FUNCTION(value);
		case OBJ_CLOSURE:
			return AS_CLOSURE(value)->function;
		default:
			return NULL;
	}
}

static inline ObjClosure *getFrameClosure(CallFrame *frame) {
	assert(frame->function->type != OBJ_FUNCTION);
	return ((ObjClosure *)frame->function);
}

ELOX_FORCE_INLINE
static int adjustArgs(FiberCtx *fiberCtx, Value *defaultValues,
					  int argCount, uint16_t arity, uint16_t maxArgs,
					  int *missingArgs) {
	int stackArgs = argCount;

	if (argCount != arity) {
		if (argCount < arity) {
			*missingArgs = arity - argCount;
			for (int i = argCount; i < arity; i++) {
				push(fiberCtx, defaultValues[i]);
				stackArgs++;
			}
		} else {
			if (argCount > maxArgs) {
				int extraArgs = argCount - maxArgs;
				stackArgs -= extraArgs;
				popn(fiberCtx, extraArgs);
			}
		}
	}

	return stackArgs;
}

ELOX_FORCE_INLINE
static void setupStackFrame(FiberCtx *fiberCtx, Value *defaultValues, CallFrame *frame,
							int argCount, uint16_t arity, uint16_t maxArgs, uint8_t argOffset) {
	int missingArgs = 0;
	int stackArgs = adjustArgs(fiberCtx, defaultValues, argCount - argOffset, arity, maxArgs, &missingArgs);

	frame->slots = fiberCtx->stackTop - stackArgs - 1;
	frame->fixedArgs = arity;
	frame->varArgs = argCount - argOffset + missingArgs - arity;
	frame->argOffset = argOffset;
}

ELOX_FORCE_INLINE
static int setupNativeStackFrame(FiberCtx *fiberCtx, Value *defaultValues, CallFrame *frame,
								 int argCount, uint16_t arity, uint16_t maxArgs, uint8_t argOffset) {
	int missingArgs = 0;
	int stackArgs = adjustArgs(fiberCtx, defaultValues, argCount, arity, maxArgs, &missingArgs);

	frame->slots = fiberCtx->stackTop - stackArgs + argOffset;
	frame->argOffset = argOffset;

	return stackArgs;
}

static bool call(RunCtx *runCtx, Obj *callee, ObjFunction *function,
				 int argCount, uint8_t argOffset) {
	FiberCtx *fiber = runCtx->activeFiber;

	if (ELOX_UNLIKELY(fiber->frameCount == FRAMES_MAX)) {
		runtimeError(runCtx, "Call stack overflow");
		return false;
	}

	CallFrame *frame = &fiber->frames[fiber->frameCount++];
DBG_PRINT_STACK("call1", runCtx);
	setupStackFrame(fiber, function->defaultArgs, frame, argCount,
					function->arity, function->maxArgs, argOffset);
DBG_PRINT_STACK("call2", runCtx);
	frame->function = (Obj *)callee;
	frame->ip = function->chunk.code;
	frame->handlerCount = 0;

	return true;
}

static bool callClosure(RunCtx *runCtx, ObjClosure *closure, int argCount, uint8_t argOffset) {
	return call(runCtx, (Obj *)closure, closure->function, argCount, argOffset);
}

static bool callFunction(RunCtx *runCtx, ObjFunction *function, int argCount, uint8_t argOffset) {
	return call(runCtx, (Obj *)function, function, argCount, argOffset);
}

static bool callNative(RunCtx *runCtx, ObjNative *native,
					   int argCount, uint8_t argOffset, bool method) {
	FiberCtx *fiber = runCtx->activeFiber;

	CallFrame *frame = &fiber->frames[fiber->frameCount++];
	frame->function = NULL;
	// for native methods include 'this'
	int stackArgs = setupNativeStackFrame(fiber, native->defaultArgs, frame,
										  argCount + (uint16_t)method,
										  native->arity, native->maxArgs, argOffset);
	//frame->slots = vm->stackTop - argCount - (int)method;

#ifdef ELOX_DEBUG_TRACE_EXECUTION
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "<native>( %p --->", native);
	printStack(runCtx);
#endif

	Args args = { .runCtx = runCtx, .count = stackArgs, .frame = frame };
	Value result = native->function(&args);

	if (ELOX_LIKELY(!IS_EXCEPTION(result))) {
/*#ifdef ELOX_DEBUG_TRACE_EXECUTION
		ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "<nativ1><---");
		printStack(vmCtx);
#endif*/
		fiber->frameCount--;
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

	fiber->frameCount--;
	return false;
}

static bool callNativeClosure(RunCtx *runCtx, ObjNativeClosure *closure,
							  int argCount, uint8_t argOffset, bool method) {
	FiberCtx *fiber = runCtx->activeFiber;

	CallFrame *frame = &fiber->frames[fiber->frameCount++];
	// for native methods include 'this'
	int stackArgs = setupNativeStackFrame(fiber, closure->defaultArgs, frame,
										  argCount + (uint16_t)method,
										  closure->arity, closure->maxArgs, argOffset);
	//frame->slots = vm->stackTop - argCount - (int)method;

#ifdef ELOX_DEBUG_TRACE_EXECUTION
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "#native#--->");
	printStack(runCtx);
#endif

	NativeClosureFn native = closure->function;
	Args args = { .runCtx = runCtx, .count = stackArgs, .frame = frame };
	Value result = native(&args, closure->upvalueCount, closure->upvalues);
	if (ELOX_LIKELY(!IS_EXCEPTION(result))) {
		fiber->frameCount--;
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

	fiber->frameCount--;
	return false;
}

bool callMethod(RunCtx *runCtx, Obj *callable,
				int argCount, uint8_t argOffset, bool *wasNative) {
	switch (callable->type) {
		case OBJ_FUNCTION:
			return callFunction(runCtx, (ObjFunction *)callable, argCount, argOffset);
		case OBJ_CLOSURE:
			return callClosure(runCtx, (ObjClosure *)callable, argCount, argOffset);
		case OBJ_NATIVE_CLOSURE:
			*wasNative = true;
			return callNativeClosure(runCtx, (ObjNativeClosure *)callable, argCount, argOffset, true);
		case OBJ_NATIVE:
			*wasNative = true;
			return callNative(runCtx, ((ObjNative *)callable), argCount, argOffset, true);
		default:
			runtimeError(runCtx, "Can only call functions and classes");
			break;
	}
	return false;
}

static void printStackTrace(RunCtx *runCtx, EloxIOStream stream) {
	FiberCtx *fiber = runCtx->activeFiber;

	int frameNo = 0;
	for (int i = fiber->frameCount - 1; i >= 0; i--) {
		CallFrame *frame = &fiber->frames[i];
		ObjFunction *function = getFrameFunction(frame);
		// -1 because the IP is sitting on the next instruction to be executed.
		size_t instruction = frame->ip - function->chunk.code - 1;
		uint32_t lineno = getLine(&function->chunk, instruction);
		eloxPrintf(runCtx, stream, "#%d [line %d] in %s()\n",
				   frameNo, lineno,
				   function->name == NULL ? "script" : (const char *)function->name->string.chars);
		frameNo++;
	}
}

Value oomError(RunCtx *runCtx) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	push(fiber, OBJ_VAL(vm->builtins.oomError));
	return EXCEPTION_VAL;
}

Value runtimeError(RunCtx *runCtx, const char *format, ...) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

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

	ObjInstance *errorInst = newInstance(runCtx, vm->builtins.runtimeExceptionClass);
	// TODO: check
	push(fiber, OBJ_VAL(errorInst));
	ObjString *msgObj = takeString(runCtx, msg.chars, msg.length, msg.capacity);
	// TODO: check
	push(fiber, OBJ_VAL(msgObj));
	bool wasNative;
	callMethod(runCtx, AS_OBJ(vm->builtins.runtimeExceptionClass->initializer), 1, 0, &wasNative);
	pop(fiber);
	push(fiber, OBJ_VAL(errorInst));

	vm->handlingException--;
	return EXCEPTION_VAL;
}

void ensureStack(RunCtx *runCtx, FiberCtx *fiberCtx, int required) {
	if (ELOX_UNLIKELY(required > fiberCtx->stackCapacity)) {
		int oldCapacity = fiberCtx->stackTopMax - fiberCtx->stack + 1;
		int newCapacity = GROW_CAPACITY(oldCapacity);
		Value *oldStack = fiberCtx->stack;

		fiberCtx->stack = GROW_ARRAY(runCtx, Value, fiberCtx->stack, oldCapacity, newCapacity);
		if (ELOX_UNLIKELY(fiberCtx->stack == NULL)) {
			// TODO: ?
			exit(1);
		}
		fiberCtx->stackTop = fiberCtx->stack + oldCapacity - 1;
		fiberCtx->stackTopMax = fiberCtx->stack + newCapacity -1;
		fiberCtx->stackCapacity = newCapacity;

		if (oldStack != fiberCtx->stack) {
			// the stack moved, recalculate all pointers that point to the old stack

			for (int i = 0; i < fiberCtx->frameCount; i++) {
				CallFrame *frame = &fiberCtx->frames[i];
				frame->slots = fiberCtx->stack + (frame->slots - oldStack);
			}

			for (ObjUpvalue *upvalue = fiberCtx->openUpvalues; upvalue != NULL; upvalue = upvalue->next)
				upvalue->location = fiberCtx->stack + (upvalue->location - oldStack);
		}
	}
}

#ifndef INLINE_STACK

void push(VM *vm, Value value) {
	*vm->stackTop = value;
	vm->stackTop++;
}

Value pop(VM *vm) {
	vm->stackTop--;
	return *vm->stackTop;
}

void popn(VM *vm, uint8_t n) {
	vm->stackTop -= n;
}

void pushn(VM *vm, uint8_t n) {
	vm->stackTop += n;
}

Value peek(VM *vm, int distance) {
	return vm->stackTop[-1 - distance];
}

#endif // INLINE_STACK

static void inherit(Error *error) {
	RunCtx *runCtx = error->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	Value superclassVal = peek(fiber, 1);
	ELOX_COND_RAISE_RET((!IS_CLASS(superclassVal)), error, RTERR("Superclass must be a class"));

	ObjClass *subclass = AS_CLASS(peek(fiber, 0));
	ObjClass *superclass = AS_CLASS(superclassVal);
	subclass->classId = subclass->baseId * superclass->classId;
	for (int i = 0; i < superclass->fields.capacity; i++) {
		Entry *entry = &superclass->fields.entries[i];
		if (entry->key != NULL) {
			bool isNewKey = tableSet(&subclass->fields, entry->key, entry->value, error);
			if (ELOX_UNLIKELY(error->raised))
				return;
			ELOX_COND_RAISE_RET((!isNewKey), error,
								RTERR("Field '%s' shadows field from superclass", entry->key->string.chars));
		}
	}
	tableAddAll(&superclass->methods, &subclass->methods, error);
	if (ELOX_UNLIKELY(error->raised))
		return;
	subclass->super = superclassVal;
	pop(fiber); // Subclass
}

static void defineMethod(ObjString *name, Error *error) {
	RunCtx *runCtx = error->runCtx;
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	Value methodCallable = peek(fiber, 0);
	ObjClass *clazz = AS_CLASS(peek(fiber, 2));
	ObjFunction *methodFunction = getValueFunction(methodCallable);
	methodFunction->parentClass = clazz;

	if ((name == clazz->name) || (name == vm->builtins.anonInitString))
		clazz->initializer = methodCallable;
	else {
		ObjMethod *method = newMethod(runCtx, clazz, AS_OBJ(methodCallable));
		ELOX_COND_RAISE_RET((method == NULL), error, OOM());
		TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
		PUSH_TEMP(temps, protectedMethod, OBJ_VAL(method));
		tableSet(&clazz->methods, name, OBJ_VAL(method), error);
		releaseTemps(&temps);
		if (ELOX_UNLIKELY(error->raised))
			return;
		if (name == vm->builtins.hashCodeString)
			clazz->hashCode = method;
		else if (name == vm->builtins.equalsString)
			clazz->equals = method;
	}
	pop(fiber);
}

static void defineField(ObjString *name, Error *error) {
	RunCtx *runCtx = error->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	ObjClass *clazz = AS_CLASS(peek(fiber, 1));
	int index = clazz->fields.count;
	tableSet(&clazz->fields, name, NUMBER_VAL(index), error);
}

static void defineStatic(ObjString *name, Error *error) {
	RunCtx *runCtx = error->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	ObjClass *clazz = AS_CLASS(peek(fiber, 2));
	int index;
	Value indexVal;
	if (!tableGet(&clazz->statics, name, &indexVal)) {
		index = clazz->statics.count;
		bool res = valueArrayPush(runCtx, &clazz->staticValues, peek(fiber, 0));
		ELOX_COND_RAISE_RET((!res), error, OOM());
		tableSet(&clazz->statics, name, NUMBER_VAL(index), error);
		if (ELOX_UNLIKELY(error->raised))
			return;
	} else {
		index = AS_NUMBER(indexVal);
		clazz->staticValues.values[index] = peek(fiber, 0);
	}

	// do not pop the static from the stack, it is saved into a local and
	// will be automatically discarded at the end of te scope
}

ObjNative *registerNativeFunction(RunCtx *runCtx,
								  const String *name, const String *moduleName,
								  NativeFn function, uint16_t arity, bool hasVarargs) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	ObjNative *ret = NULL;

	bool isBuiltin = stringEquals(moduleName, &eloxBuiltinModule);
	ObjNative *native = newNative(runCtx, function, arity);
	if (ELOX_UNLIKELY(native == NULL))
		return NULL;
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	PUSH_TEMP(temps, protectedNative, OBJ_VAL(native));

	if (isBuiltin) {
		suint16_t builtinIdx = builtinConstant(runCtx, name);
		if (ELOX_UNLIKELY(builtinIdx < 0))
			goto cleanup;
		vm->builtinValues.values[builtinIdx] = OBJ_VAL(native);
	} else {
		suint16_t globalIdx = globalIdentifierConstant(runCtx, name, moduleName);
		if (ELOX_UNLIKELY(globalIdx < 0))
			goto cleanup;
		vm->globalValues.values[globalIdx] = OBJ_VAL(native);
	}

	native->arity = arity;
	native->maxArgs = hasVarargs ? 255 : arity;

	ret = native;

cleanup:
	releaseTemps(&temps);

	return ret;
}

FiberCtx *newFiberCtx(RunCtx *runCtx) {
	Value *stack = GROW_ARRAY(runCtx, Value, NULL, 0, MIN_STACK);
	if (ELOX_UNLIKELY(stack == NULL))
		return NULL;

	FiberCtx *fiber = ALLOCATE(runCtx, FiberCtx, 1);
	if (ELOX_UNLIKELY(fiber == NULL)) {
		FREE_ARRAY(runCtx, Value, stack, MIN_STACK);
		return NULL;
	}

	fiber->stack = stack;
	fiber->stackTopMax = fiber->stack + MIN_STACK - 1;
	fiber->stackCapacity = fiber->stackTopMax - fiber->stack + 1;

	fiber->stackTop = fiber->stack;
	fiber->frameCount = 0;
	fiber->openUpvalues = NULL;
	fiber->temps = NULL;

	return fiber;
}

void markFiberCtx(RunCtx *runCtx, FiberCtx *fiberCtx) {
	if (ELOX_UNLIKELY(fiberCtx == NULL))
		return;

	for (Value *slot = fiberCtx->stack; slot < fiberCtx->stackTop; slot++)
		markValue(runCtx, *slot);

	for (int i = 0; i < fiberCtx->frameCount; i++)
		markObject(runCtx, fiberCtx->frames[i].function);

	for (ObjUpvalue *upvalue = fiberCtx->openUpvalues; upvalue != NULL; upvalue = upvalue->next)
		markObject(runCtx, (Obj *)upvalue);

	VMTemp *temp = fiberCtx->temps;
	while (temp != NULL) {
		markValue(runCtx, temp->val);
		temp = temp->next;
	}
}

bool initVM(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vmInstance;
	bool ret = false;

	vm->compilerCount = 0;
	vm->compilerCapacity = 0;
	vm->compilerStack = NULL;

	vm->handles.head = NULL;

	vm->grayOverflow = false;
	vm->grayCount = 0;
	vm->grayCapacity = 0;
	vm->grayStack = NULL;

	initTable(&vm->strings);

	vm->mainHeap.objects = NULL;
	vm->mainHeap.initialMarkers = 0;
	vm->permHeap.objects = NULL;
	vm->permHeap.initialMarkers = MARKER_BLACK;
	vm->heap = &vm->mainHeap;
	vm->bytesAllocated = 0;
	vm->nextGC = 1024 * 1024;

	vm->initFiber = NULL;
	initValueTable(&vm->globalNames);
	initValueArray(&vm->globalValues);

	RunCtx runCtx = {
		.vm = vm,
		.vmEnv = &vmCtx->env
	};

	vm->initFiber = newFiberCtx(&runCtx);
	if (ELOX_UNLIKELY(vm->initFiber == NULL))
		goto cleanup;
	runCtx.activeFiber = vm->initFiber;

	vm->handlingException = 0;
	stc64_init(&vm->prng, 64);
	initPrimeGen(&vm->primeGen, 0);

	initValueArray(&vm->builtinValues);

	initTable(&vm->modules);
	initTable(&vm->builtinSymbols);

	clearBuiltins(vm);

	vm->heap = &vm->permHeap;
	bool ok = registerBuiltins(&runCtx);
	vm->heap = &vm->mainHeap;
	if (!ok)
		goto cleanup;

	memset(vm->classes, 0, sizeof(vm->classes));
	vm->classes[VTYPE_BOOL] = vm->builtins.boolClass;
	vm->classes[VTYPE_NUMBER] = vm->builtins.numberClass;
	vm->classes[VTYPE_OBJ_STRING] = vm->builtins.stringClass;
	vm->classes[VTYPE_OBJ_CLASS] = vm->builtins.classClass;
	vm->classes[VTYPE_OBJ_INSTANCE] = vm->builtins.instanceClass;
	vm->classes[VTYPE_OBJ_ARRAY] = vm->builtins.arrayClass;
	vm->classes[VTYPE_OBJ_TUPLE] = vm->builtins.tupleClass;
	vm->classes[VTYPE_OBJ_MAP] = vm->builtins.mapClass;

	ok = initHandleSet(&runCtx, &vm->handles);
	if (!ok)
		goto cleanup;

	ret = true;

cleanup:
	vm->initFiber = NULL;

	return ret;
}

static Value getStackTrace(RunCtx *runCtx) {
	FiberCtx *fiber = runCtx->activeFiber;

#define MAX_LINE_LENGTH 512

	int maxStackTraceLength = fiber->frameCount * MAX_LINE_LENGTH;
	uint8_t *stacktrace = ALLOCATE(runCtx, uint8_t, maxStackTraceLength);
	uint16_t index = 0;
	int frameNo = 0;
	for (int i = fiber->frameCount - 1; i >= 0; i--) {
		CallFrame *frame = &fiber->frames[i];
		ObjFunction *function = getFrameFunction(frame);
		// -1 because the IP is sitting on the next instruction to be executed
		size_t instruction = frame->ip - function->chunk.code - 1;
		uint32_t lineno = getLine(&function->chunk, instruction);
		index += snprintf((char *)&stacktrace[index], MAX_LINE_LENGTH, "#%d [line %d] in %s()\n",
						  frameNo, lineno,
						  function->name == NULL ? "script" : (const char *)function->name->string.chars);
		frameNo++;
	}
	stacktrace = GROW_ARRAY(runCtx, uint8_t, stacktrace, maxStackTraceLength, index + 1);
	return OBJ_VAL(takeString(runCtx, stacktrace, index, index + 1));
	// TODO: check

#undef MAX_LINE_LENGTH
}

static bool getInstanceValue(ObjInstance *instance, ObjString *name, Value *value) {
	ObjClass *clazz = instance->clazz;
	Value valueIndex;
	if (tableGet(&clazz->fields, name, &valueIndex)) {
		int valueOffset = AS_NUMBER(valueIndex);
		*value = instance->fields.values[valueOffset];
		return true;
	}
	return false;
}

bool setInstanceField(ObjInstance *instance, ObjString *name, Value value) {
	ObjClass *clazz = instance->clazz;
	Value valueIndex;
	if (tableGet(&clazz->fields, name, &valueIndex)) {
		int valueOffset = AS_NUMBER(valueIndex);
		instance->fields.values[valueOffset] = value;
		return false;
	}
	return true;
}

static bool instanceOf(ObjClass *clazz, ObjClass *instanceClass) {
	return ((instanceClass->classId % clazz->classId) == 0);
}

static bool propagateException(RunCtx *runCtx, int exitFrame) {
	VM *vm = runCtx->vm;
	VMEnv *env = runCtx->vmEnv;
	FiberCtx *fiber = runCtx->activeFiber;

	ObjInstance *exception = AS_INSTANCE(peek(fiber, 0));
	bool exceptionHandled = false;

	while (fiber->frameCount > exitFrame) {
		CallFrame *frame = &fiber->frames[fiber->frameCount - 1];
		for (int handlerStack = frame->handlerCount; handlerStack > 0; handlerStack--) {
			TryBlock *tryBlock = &frame->handlerStack[handlerStack - 1];
			uint16_t handlerDataOffset = tryBlock->handlerDataOffset;
			ObjFunction *frameFunction = getFrameFunction(frame);
			uint8_t *handlerData = frameFunction->chunk.code + handlerDataOffset;
			uint8_t handlerTableSize = handlerData[0];
			uint16_t finallyAddress;
			memcpy(&finallyAddress, handlerData + 1, sizeof(uint16_t));
			uint8_t numHandlers = handlerTableSize / 6;
			if (!tryBlock->caught) {
				for (int i = 0; i < numHandlers; i++) {
					uint8_t *handlerRecord = handlerData + 1 + 2 + (6 * i);
					VarType typeVarType = handlerRecord[0];
					bool postArgs = handlerRecord[1];
					uint16_t typeHandle;
					memcpy(&typeHandle, handlerRecord + 2, sizeof(uint16_t));
					Value classVal = NIL_VAL;
					switch (typeVarType) {
						case VAR_LOCAL:
							classVal = frame->slots[typeHandle + (postArgs * frame->varArgs)];
							break;
						case VAR_UPVALUE:
							classVal = *getFrameClosure(frame)->upvalues[typeHandle]->location;
							break;
						case VAR_GLOBAL: {
							classVal = vm->globalValues.values[typeHandle];
							if (ELOX_UNLIKELY(IS_UNDEFINED(classVal))) {
								runtimeError(runCtx, "Undefined global variable");
								return false;
							}
							if (ELOX_UNLIKELY(!IS_CLASS(classVal))) {
								runtimeError(runCtx, "Not a type to catch");
								return false;
							}
							break;
						}
					}

					ObjClass *handlerClass = AS_CLASS(classVal);
					if (instanceOf(handlerClass, exception->clazz)) {
						tryBlock->caught = true;
						exceptionHandled = true;
						uint16_t handlerAddress;
						memcpy(&handlerAddress, handlerRecord + 4, sizeof(uint16_t));
						frame->ip = &frameFunction->chunk.code[handlerAddress];
						Value exception = pop(fiber);
						fiber->stackTop = frame->slots + tryBlock->stackOffset;
						push(fiber, exception);
						return true;
					}
				}
			}

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
					exception = AS_INSTANCE(peek(fiber, 0));
					frame->handlerCount--; // TODO: ??
				}
			}
		}
		fiber->frameCount--;
	}

	DBG_PRINT_STACK("DBGExc", runCtx);

	// Do not print the exception here if we are inside an internal call
	if (exitFrame == 0) {
		eloxPrintf(runCtx, ELOX_IO_ERR, "Unhandled exception %s", exception->clazz->name->string.chars);
		Value message;
		// TODO: check copyString
		if (getInstanceValue(exception, copyString(runCtx, ELOX_USTR_AND_LEN("message")), &message))
			eloxPrintf(runCtx, ELOX_IO_ERR, ": %s\n", AS_CSTRING(message));
		else
			ELOX_WRITE(runCtx, ELOX_IO_ERR, "\n");
		Value stacktrace;
		if (getInstanceValue(exception, copyString(runCtx, ELOX_USTR_AND_LEN("stacktrace")), &stacktrace))
			eloxPrintf(runCtx, ELOX_IO_ERR, "%s", AS_CSTRING(stacktrace));
	}
	return false;
}

static bool unrollExceptionHandlerStack(RunCtx *runCtx, uint8_t targetLevel, bool restore) {
	FiberCtx *fiber = runCtx->activeFiber;

	Value savedTop = NIL_VAL;
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	VMTemp protectedTop;
	if (restore) {
		savedTop = pop(fiber);
		pushTempVal(temps, &protectedTop, savedTop);
	}

	CallFrame *frame = &fiber->frames[fiber->frameCount - 1];
	for (int i = frame->handlerCount - 1; i >= targetLevel; i--) {
		TryBlock *tryBlock = &frame->handlerStack[i];
		uint16_t handlerDataOffset = tryBlock->handlerDataOffset;
		ObjFunction *frameFunction = getFrameFunction(frame);
		uint8_t *handlerData = frameFunction->chunk.code + handlerDataOffset;
		uint16_t finallyAddress;
		memcpy(&finallyAddress, handlerData + 1, sizeof(uint16_t));

		if (finallyAddress > 0) {
			frame->ip = &frameFunction->chunk.code[finallyAddress];
			fiber->stackTop = frame->slots + tryBlock->stackOffset;
			bool finallyStatus = runChunk(runCtx);
		}

		frame->handlerCount--;
	}

	if (restore) {
		push(fiber, savedTop);
		releaseTemps(&temps);
	}

	return true;
}

static bool pushExceptionHandler(RunCtx *runCtx, uint8_t stackLevel, uint16_t handlerTableAddress) {
	FiberCtx *fiber = runCtx->activeFiber;

	CallFrame *frame = &fiber->frames[fiber->frameCount - 1];
	if (ELOX_UNLIKELY(frame->handlerCount == MAX_CATCH_HANDLER_FRAMES)) {
		runtimeError(runCtx, "Too many nested exception handlers in one function");
		return false;
	}

	TryBlock *tryBlock = &frame->handlerStack[stackLevel];
	if (stackLevel >= frame->handlerCount)
		frame->handlerCount = stackLevel + 1;

	tryBlock->handlerDataOffset = handlerTableAddress;
	tryBlock->stackOffset = fiber->stackTop - frame->slots;
	tryBlock->caught = false;
	return true;
}

static ObjClass *classOf(VM *vm, Value val) {
	ValueTypeId typeId = valueTypeId(val);
	return vm->classes[typeId];
}

static ObjClass *classOfFollowInstance(VM *vm, Value val) {
	ObjClass *clazz = classOf(vm, val);
	if (clazz == vm->builtins.instanceClass)
		return ((ObjInstance *)AS_OBJ(val))->clazz;
	return clazz;
}

static bool callValue(RunCtx *runCtx, Value callee, int argCount, bool *wasNative) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	if (IS_OBJ(callee)) {
		switch (OBJ_TYPE(callee)) {
			case OBJ_BOUND_METHOD: {
				ObjBoundMethod *bound = AS_BOUND_METHOD(callee);

				fiber->stackTop[-argCount - 1] = bound->receiver;
				return callMethod(runCtx, bound->method, argCount, 0, wasNative);
			}
			case OBJ_METHOD: {
				ObjMethod *method = AS_METHOD(callee);
				if (ELOX_UNLIKELY(argCount < 1)) {
					runtimeError(runCtx, "Need to pass instance when calling method");
					return false;
				}
				if (ELOX_UNLIKELY(!instanceOf(method->clazz, classOfFollowInstance(vm, fiber->stackTop[-argCount])))) {
					runtimeError(runCtx, "Method invoked on wrong instance type");
					return false;
				}
				return callMethod(runCtx, method->callable, argCount, 1, wasNative);
			}
			case OBJ_CLASS: {
				ObjClass *clazz = AS_CLASS(callee);
				ObjInstance *inst = newInstance(runCtx, clazz);
				if (ELOX_UNLIKELY(inst == NULL)) {
					oomError(runCtx);
					return false;
				}
				fiber->stackTop[-argCount - 1] = OBJ_VAL(inst);
				if (!IS_NIL(clazz->initializer)) {
#ifdef ELOX_DEBUG_TRACE_EXECUTION
				eloxPrintf(runCtx, ELOX_IO_DEBUG, "===>%s init\n", clazz->name->string.chars);
#endif
					return callMethod(runCtx, AS_OBJ(clazz->initializer), argCount, 0, wasNative);
				} else if (argCount != 0) {
					runtimeError(runCtx, "Expected 0 arguments but got %d", argCount);
					return false;
				}
				return true;
			}
			case OBJ_CLOSURE:
				return callClosure(runCtx, AS_CLOSURE(callee), argCount, 0);
			case OBJ_NATIVE_CLOSURE:
				*wasNative = true;
				return callNativeClosure(runCtx, AS_NATIVE_CLOSURE(callee), argCount, 0, false);
			case OBJ_FUNCTION:
				return callFunction(runCtx, AS_FUNCTION(callee), argCount, 0);
			case OBJ_NATIVE:
				*wasNative = true;
				return callNative(runCtx, AS_NATIVE(callee), argCount, 0, false);
			default:
				break; // Non-callable object type
		}
	}
	runtimeError(runCtx, "Can only call functions and classes");
	return false;
}

static bool invoke(RunCtx *runCtx, ObjString *name, int argCount) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	Value receiver = peek(fiber, argCount);

	ObjClass *clazz = classOf(vm, receiver);
	if (ELOX_UNLIKELY(clazz == NULL)) {
		runtimeError(runCtx, "This value has no methods");
		return false;
	}

	if (clazz == vm->builtins.classClass) {
		clazz = (ObjClass *)AS_OBJ(receiver);
		Value namedVal;
		if (tableGet(&clazz->statics, name, &namedVal)) {
			int index = AS_NUMBER(namedVal);
			bool wasNative;
			return callValue(runCtx, clazz->staticValues.values[index], argCount, &wasNative);
		} else if (tableGet(&clazz->methods, name, &namedVal)) {
			bool wasNative;
			return callValue(runCtx, namedVal, argCount, &wasNative);
		}
		runtimeError(runCtx, "Undefined method or static property '%s'", name->string.chars);
		return false;
	} else if (clazz == vm->builtins.instanceClass) {
		clazz = ((ObjInstance *)AS_OBJ(receiver))->clazz;

		ObjInstance *instance = AS_INSTANCE(receiver);
		Value value;
		if (getInstanceValue(instance, name, &value)) {
			fiber->stackTop[-argCount - 1] = value;
			bool wasNative;
			return callValue(runCtx, value, argCount, &wasNative);
		}
	}

	Value method;
	if (ELOX_UNLIKELY(!tableGet(&clazz->methods, name, &method))) {
		runtimeError(runCtx, "Undefined property '%s'", name->string.chars);
		return false;
	}
	bool wasNative;
	return callMethod(runCtx, AS_METHOD(method)->callable, argCount, 0, &wasNative);
}

static bool invokeMember(RunCtx *runCtx, Value *member, bool isMember, int argCount) {
	FiberCtx *fiber = runCtx->activeFiber;
	bool wasNative;

	if (!isMember) {
		fiber->stackTop[-argCount - 1] = *member;
		return callValue(runCtx, *member, argCount, &wasNative);
	} else
		return callMethod(runCtx, AS_OBJ(*member), argCount, 0, &wasNative);
}

static bool bindMethod(RunCtx *runCtx, ObjClass *clazz, ObjString *name) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	Value method;
	if (!tableGet(&clazz->methods, name, &method)) {
		runtimeError(runCtx, "Undefined property '%s'", name->string.chars);
		return false;
	}

	ObjBoundMethod *bound = newBoundMethod(runCtx, peek(fiber, 0), AS_METHOD(method));
	if (ELOX_UNLIKELY(bound == NULL)) {
		push(fiber, OBJ_VAL(vm->builtins.oomError));
		return false;
	}
	pop(fiber);
	push(fiber, OBJ_VAL(bound));
	return true;
}

static ObjUpvalue *captureUpvalue(RunCtx *runCtx, Value *local) {
	FiberCtx *fiber = runCtx->activeFiber;

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
	FiberCtx *fiber = runCtx->activeFiber;

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
	switch (OBJ_TYPE(val)) {
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

Value toString(Value value, Error *error) {
	RunCtx *runCtx = error->runCtx;
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	ObjClass *clazz = classOfFollowInstance(vm, value);
	ELOX_COND_RAISE_RET_VAL((clazz ==  NULL), error,
							RTERR("No string representation available"), EXCEPTION_VAL);

	Value method;
	if (ELOX_UNLIKELY(!tableGet(&clazz->methods, vm->builtins.toStringString, &method)))
		ELOX_RAISE_RET_VAL(error, RTERR("No string representation available"), EXCEPTION_VAL);

	ObjBoundMethod *boundToString = newBoundMethod(runCtx, value, AS_METHOD(method));
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

static Value *resolveRef(MemberRef *ref, ObjInstance *inst) {
	if (ref->refType == REFTYPE_CLASS_MEMBER)
		return ref->data.value;
	return &inst->fields.values[ref->data.propIndex];
}

static bool buildMap(RunCtx *runCtx, uint16_t itemCount) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	ObjMap *map = newMap(runCtx);
	if (ELOX_UNLIKELY(map == NULL)) {
		push(fiber, OBJ_VAL(vm->builtins.oomError));
		return false;
	}

	push(fiber, OBJ_VAL(map));
	int i = 2 * itemCount;
	Error error = ERROR_INITIALIZER(runCtx);
	while (i > 0) {
		Value key = peek(fiber, i--);
		Value value = peek(fiber, i--);
		valueTableSet(&map->items, key, value, &error);
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
	FiberCtx *fiber = runCtx->activeFiber;

	Value indexVal = peek(fiber, 0);
	Value indexable = peek(fiber, 1);
	Value result;

	ValueTypeId indexableType = valueTypeId(indexable);

	switch (indexableType) {
		case VTYPE_OBJ_ARRAY:
		case VTYPE_OBJ_TUPLE: {
			ObjArray *array = AS_ARRAY(indexable);
			if (ELOX_UNLIKELY(!IS_NUMBER(indexVal))) {
				runtimeError(runCtx, "Array index is not a number");
				return false;
			}
			int32_t index = AS_NUMBER(indexVal);
			result = arrayAtSafe(runCtx, array, index);
			if (ELOX_UNLIKELY(IS_EXCEPTION(result)))
				return false;
			break;
		}
		case VTYPE_OBJ_MAP: {
			ObjMap *map = AS_MAP(indexable);
			Error error = ERROR_INITIALIZER(runCtx);
			bool found = valueTableGet(&map->items, indexVal, &result, &error);
			if (ELOX_UNLIKELY(error.raised))
				return false;
			if (!found)
				result = NIL_VAL;
			break;
		}
		case VTYPE_OBJ_STRING: {
			ObjString *str = AS_STRING(indexable);
			if (ELOX_UNLIKELY(!IS_NUMBER(indexVal))) {
				runtimeError(runCtx, "String index is not a number");
				return false;
			}
			int32_t index = AS_NUMBER(indexVal);
			result = stringAtSafe(runCtx, str, index);
			if (ELOX_UNLIKELY(IS_EXCEPTION(result)))
				return false;
			break;
		}
		default:
			runtimeError(runCtx, "Invalid type to index into");
			return false;
	}

	popn(fiber, 2);
	push(fiber, result);

	return true;
}

static bool indexStore(RunCtx *runCtx) {
	FiberCtx *fiber = runCtx->activeFiber;

	Value item = peek(fiber, 0);
	Value indexVal = peek(fiber, 1);
	Value indexable = peek(fiber, 2);

	ValueTypeId indexableType = valueTypeId(indexable);

	switch (indexableType) {
		case VTYPE_OBJ_ARRAY: {
			ObjArray *array = AS_ARRAY(indexable);

			if (!IS_NUMBER(indexVal)) {
				runtimeError(runCtx, "Array index is not a number");
				return false;
			}

			int index = AS_NUMBER(indexVal);
			if (ELOX_UNLIKELY(!isValidArrayIndex(array, index))) {
				runtimeError(runCtx, "Array index out of range");
				return false;
			}

			arraySet(array, index, item);
			break;
		}
		case VTYPE_OBJ_MAP: {
			ObjMap *map = AS_MAP(indexable);

			Error error = ERROR_INITIALIZER(runCtx);
			valueTableSet(&map->items, indexVal, item, &error);
			if (ELOX_UNLIKELY(error.raised))
				return false;
			break;
		}
		default:
			runtimeError(runCtx, "Destination is not an array or map");
			return false;
	}

	popn(fiber, 3);
	push(fiber, item);

	return true;
}

static bool sliceValue(RunCtx *runCtx) {
	FiberCtx *fiber = runCtx->activeFiber;

	Value sliceEnd = peek(fiber, 0);
	Value sliceStart = peek(fiber, 1);
	Value sliceable = peek(fiber, 2);
	Value result;

	ValueTypeId sliceableType = valueTypeId(sliceable);
	switch(sliceableType) {
		case VTYPE_OBJ_ARRAY:
		case VTYPE_OBJ_TUPLE: {
			ObjType type = AS_OBJ(sliceable)->type;
			ObjArray *array = AS_ARRAY(sliceable);
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
			runtimeError(runCtx, "Invalid type to slice");
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
	OBJ_NATIVE_CLOSURE, OBJ_FUNCTION, OBJ_INSTANCE, OBJ_NATIVE, OBJ_ARRAY, OBJ_TUPLE, OBJ_MAP

#define IN_ARRAY(T) \
	[VTYPE_ ## T][VTYPE_OBJ_ARRAY] = IN_OP_VALUE_ARRAY,
#define IN_TUPLE(T) \
	[VTYPE_ ## T][VTYPE_OBJ_TUPLE] = IN_OP_VALUE_ARRAY,
#define IN_MAP(T) \
	[VTYPE_ ## T][VTYPE_OBJ_MAP] = IN_OP_VALUE_MAP,

static const InOps inTable[VTYPE_MAX][VTYPE_MAX] = {
	[VTYPE_OBJ_STRING][VTYPE_OBJ_STRING] = IN_OP_STRING_STRING,
	FOR_EACH(IN_ARRAY, ANY)
	FOR_EACH(IN_TUPLE, ANY)
	FOR_EACH(IN_MAP, ANY)
};

static bool in(RunCtx *runCtx) {
	FiberCtx *fiber = runCtx->activeFiber;

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
			push(fiber, BOOL_VAL(stringContains(seq, val)));
			OP_DISPATCH_BREAK;
		}
		OP_DISPATCH_CASE(VALUE_ARRAY): {
			Error error = ERROR_INITIALIZER(runCtx);
			bool res = arrayContains(AS_ARRAY(peek(fiber, 0)), peek(fiber, 1), &error);
			if (ELOX_UNLIKELY(error.raised))
				return false;
			popn(fiber, 2);
			push(fiber, BOOL_VAL(res));
			OP_DISPATCH_BREAK;
		}
		OP_DISPATCH_CASE(VALUE_MAP): {
			ObjMap *map = AS_MAP(peek(fiber, 0));
			Value val = peek(fiber, 1);
			Error error = ERROR_INITIALIZER(runCtx);
			bool found = valueTableContains(&map->items, val, &error);
			if (ELOX_UNLIKELY(error.raised))
				return false;
			popn(fiber, 2);
			push(fiber, BOOL_VAL(found));
			OP_DISPATCH_BREAK;
		}
		OP_DISPATCH_CASE(UNDEFINED):
			runtimeError(runCtx, "Invalid operands for 'in'");
			return false;
			OP_DISPATCH_BREAK;
	OP_DISPATCH_END

	#include "ops/opsCleanup.h"

	return true;
}

static bool resolveMember(RunCtx *runCtx, ObjClass *clazz, uint8_t slotType,
						  ObjString *propName, uint16_t slot) {
	bool super = slotType & 0x1;
	uint8_t propType = (slotType & 0x6) >> 1;

	if (super) {
		ObjClass *superClass = AS_CLASS(clazz->super);
		int propIndex = tableGetIndex(&superClass->methods, propName);
		if (ELOX_UNLIKELY(propIndex < 0)) {
			runtimeError(runCtx, "Undefined property '%s'", propName->string.chars);
			return false;
		}
		clazz->memberRefs[slot] = (MemberRef){
			.refType = REFTYPE_CLASS_MEMBER,
			.data.value = &superClass->methods.entries[propIndex].value
		};
	} else {
		int propIndex = -1;
		bool isField = false;

		if (propType & MEMBER_FIELD) {
			Value index;
			if (tableGet(&clazz->fields, propName, &index)) {
				propIndex = AS_NUMBER(index);
				isField = true;
			}
		}
		if ((propIndex) < 0 && (propType & MEMBER_METHOD))
			propIndex = tableGetIndex(&clazz->methods, propName);

		if (ELOX_UNLIKELY(propIndex < 0)) {
			runtimeError(runCtx, "Undefined property '%s'", propName->string.chars);
			return false;
		}
		if (isField) {
			clazz->memberRefs[slot] = (MemberRef) {
				.refType = REF_TYPE_INST_FIELD,
				.data.propIndex = propIndex
			};
		} else {
			clazz->memberRefs[slot] = (MemberRef) {
				.refType = REFTYPE_CLASS_MEMBER,
				.data.value = &clazz->methods.entries[propIndex].value
			};
		}
	}

	return true;
}

static bool import(RunCtx *runCtx, ObjString *moduleName,
				   uint16_t numSymbols, uint8_t *args, Value *consts ELOX_UNUSED) {
	VM *vm = runCtx->vm;
	VMEnv *env = runCtx->vmEnv;
	FiberCtx *fiber = runCtx->activeFiber;

	bool loaded = false;
	if (tableFindString(&vm->modules,
						moduleName->string.chars, moduleName->string.length,
						moduleName->hash) != NULL) {
		// already loaded
		loaded = true;
	}

	if (!loaded) {
		Error error = ERROR_INITIALIZER(runCtx);
		String *strModuleName = &moduleName->string;
		bool found = false;

		for (EloxModuleLoader *mLoader = env->loaders; mLoader != NULL; mLoader++) {
			if (mLoader->loader == NULL)
				break;

			Value callable = mLoader->loader(strModuleName, mLoader->options, &error);
			if (ELOX_UNLIKELY(error.raised))
				return false;

			if (!IS_NIL(callable)) {
				push(fiber, callable);

				tableSet(&vm->modules, moduleName, BOOL_VAL(true), &error);
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
			runtimeError(runCtx, "Could not find module '%s'", moduleName->string.chars);
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

static void expandVarArgs(FiberCtx *fiberCtx, CallFrame *frame, bool firstExpansion) {
	uint8_t numVarArgs = frame->varArgs;
	double prevVarArgs = 0;

	if (!firstExpansion)
		prevVarArgs = AS_NUMBER(pop(fiberCtx));

	for (uint32_t i = 0; i < numVarArgs; i++)
		push(fiberCtx, frame->slots[frame->fixedArgs + i + 1]);
	push(fiberCtx, NUMBER_VAL(prevVarArgs + numVarArgs));
}

typedef enum {
	UPK_VALUE,
	UPK_TUPLE,
	UPK_ITERATOR
} UnpackType;

typedef struct {
	bool hasNext;
	union {
		struct {
			ObjArray *tuple;
			int index;
		} tState;
		struct {
			Value hasNext;
			Value next;
		} iState;
	};
} UnpackState;

static bool expand(RunCtx *runCtx, bool firstExpansion) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	bool ret = false;
	double prevVarArgs = 0;

	if (!firstExpansion)
		prevVarArgs = AS_NUMBER(pop(fiber));

	const Value expandable = pop(fiber);
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	PUSH_TEMP(temps, protectedExpandable, expandable);

	UnpackType unpackType = UPK_VALUE;
	UnpackState state = {
		.hasNext = false
	};
	unsigned int numExpanded = 0;

	VMTemp protectedHasNext = TEMP_INITIALIZER;
	VMTemp protectedNext = TEMP_INITIALIZER;

	if (IS_TUPLE(expandable)) {
		unpackType = UPK_TUPLE;
		state.tState.tuple = AS_TUPLE(expandable);
		state.hasNext = state.tState.tuple->size > 0;
		state.tState.index = 0;
	} else if (IS_INSTANCE(expandable) &&
			   instanceOf(vm->builtins.iteratorClass, AS_INSTANCE(expandable)->clazz)) {
		unpackType = UPK_ITERATOR;
		ObjInstance *iterator = AS_INSTANCE(expandable);
		ObjClass *iteratorClass = iterator->clazz;

		push(fiber, OBJ_VAL(iterator));
		bindMethod(runCtx, iteratorClass, vm->builtins.hasNextString);
		state.iState.hasNext = pop(fiber);
		pushTempVal(temps, &protectedHasNext, state.iState.hasNext);

		push(fiber, OBJ_VAL(iterator));
		bindMethod(runCtx, iteratorClass, vm->builtins.nextString);
		state.iState.next = pop(fiber);
		pushTempVal(temps, &protectedNext, state.iState.next);

		push(fiber, state.iState.hasNext);
		Value hasNext = runCall(runCtx, 0);
		if (ELOX_UNLIKELY(IS_EXCEPTION(hasNext)))
			goto cleanup;
		pop(fiber);
		state.hasNext = AS_BOOL(hasNext);
	} else {
		// just a single value
		state.hasNext = true;
	}

	while (state.hasNext) {
		switch(unpackType) {
			case UPK_VALUE:
				push(fiber, expandable);
				numExpanded++;
				state.hasNext = false;
				break;
			case UPK_TUPLE:
				push(fiber, arrayAt(state.tState.tuple, state.tState.index++));
				numExpanded++;
				state.hasNext = state.tState.index < state.tState.tuple->size;
				break;
			case UPK_ITERATOR:
				push(fiber, state.iState.next);
				Value next = runCall(runCtx, 0);
				if (ELOX_UNLIKELY(IS_EXCEPTION(next)))
					goto cleanup;
				pop(fiber);

				push(fiber, next);
				numExpanded++;

				push(fiber, state.iState.hasNext);
				Value hasNext = runCall(runCtx, 0);
				if (ELOX_UNLIKELY(IS_EXCEPTION(hasNext)))
					goto cleanup;
				pop(fiber);
				state.hasNext = AS_BOOL(hasNext);

				break;
		}
	}

	push(fiber, NUMBER_VAL(prevVarArgs + numExpanded));

	ret = true;

cleanup:
	releaseTemps(&temps);

	return ret;
}

static bool getProperty(RunCtx *runCtx, ObjString *name) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	Value targetVal = peek(fiber, 0);

	if (IS_INSTANCE(targetVal)) {
		ObjInstance *instance = AS_INSTANCE(targetVal);

		Value value;
		if (getInstanceValue(instance, name, &value)) {
			pop(fiber); // Instance
			push(fiber, value);
		} else {
			if (ELOX_UNLIKELY(!bindMethod(runCtx, instance->clazz, name)))
				return false;
		}
	} else if (IS_CLASS(targetVal)) {
		ObjClass *clazz = AS_CLASS(targetVal);
		Value methodVal;
		if (tableGet(&clazz->methods, name, &methodVal)) {
			pop(fiber); // class
			push(fiber, methodVal);
			return true;
		}
	} else {
		ObjClass *clazz = classOfFollowInstance(vm, targetVal);
		if (ELOX_LIKELY(clazz != NULL)) {
			if (ELOX_UNLIKELY(!bindMethod(runCtx, clazz, name)))
				return false;
		} else {
			runtimeError(runCtx, "This value doesn't have properties");
			return false;
		}
	}

	return true;
}

static uint16_t _chkreadu16tmp;
#define CHUNK_READ_BYTE(PTR) (*PTR++)
#define CHUNK_READ_USHORT(PTR) \
	(memcpy(&_chkreadu16tmp, PTR, sizeof(uint16_t)), PTR += sizeof(uint16_t), _chkreadu16tmp )

static unsigned int doUnpack(RunCtx *runCtx, CallFrame *frame, uint8_t *chunk, bool *error) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	unsigned int ret = 0;
	uint8_t *ptr = chunk;

	uint8_t numVars = CHUNK_READ_BYTE(ptr);
	Value val = peek(fiber, 0);

	UnpackType unpackType = UPK_VALUE;
	UnpackState state = {
		.hasNext = false
	};

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	VMTemp protectedHasNext = TEMP_INITIALIZER;
	VMTemp protectedNext = TEMP_INITIALIZER;

	if (IS_TUPLE(val)) {
		unpackType = UPK_TUPLE;
		state.tState.tuple = AS_TUPLE(val);
		state.hasNext = state.tState.tuple->size > 0;
		state.tState.index = 0;
	} else if (IS_INSTANCE(val) &&
			   instanceOf(vm->builtins.iteratorClass, AS_INSTANCE(val)->clazz)) {
		unpackType = UPK_ITERATOR;
		ObjInstance *iterator = AS_INSTANCE(val);
		ObjClass *iteratorClass = iterator->clazz;

		push(fiber, OBJ_VAL(iterator));
		bindMethod(runCtx, iteratorClass, vm->builtins.hasNextString);
		state.iState.hasNext = pop(fiber);
		pushTempVal(temps, &protectedHasNext, state.iState.hasNext);

		push(fiber, OBJ_VAL(iterator));
		bindMethod(runCtx, iteratorClass, vm->builtins.nextString);
		state.iState.next = pop(fiber);
		pushTempVal(temps, &protectedNext, state.iState.next);

		push(fiber, state.iState.hasNext);
		Value hasNext = runCall(runCtx, 0);
		if (ELOX_UNLIKELY(IS_EXCEPTION(hasNext))) {
			*error = true;
			ret = ptr - chunk;
			goto cleanup;
		}
		pop(fiber);
		state.hasNext = AS_BOOL(hasNext);
	} else {
		// just a single value
		state.hasNext = true;
	}

	for (int i = 0; i < numVars; i++) {
		Value crtVal;
		if (state.hasNext) {
			switch(unpackType) {
				case UPK_VALUE:
					crtVal = val;
					state.hasNext = false;
					break;
				case UPK_TUPLE:
					crtVal = arrayAt(state.tState.tuple, state.tState.index++);
					state.hasNext = state.tState.index < state.tState.tuple->size;
					break;
				case UPK_ITERATOR:
					push(fiber, state.iState.next);
					Value next = runCall(runCtx, 0);
					if (ELOX_UNLIKELY(IS_EXCEPTION(next))) {
						*error = true;
						ret = ptr - chunk;
						goto cleanup;
					}
					pop(fiber);
					crtVal = next;

					push(fiber, state.iState.hasNext);
					Value hasNext = runCall(runCtx, 0);
					if (ELOX_UNLIKELY(IS_EXCEPTION(hasNext))) {
						*error = true;
						ret = ptr - chunk;
						goto cleanup;
					}
					pop(fiber);
					state.hasNext = AS_BOOL(hasNext);

					break;
			}
		} else
			crtVal = NIL_VAL;

		VarType varType = CHUNK_READ_BYTE(ptr);
		switch (varType) {
			case VAR_LOCAL: {
				uint8_t slot = CHUNK_READ_BYTE(ptr);
				uint8_t postArgs = CHUNK_READ_BYTE(ptr);
				frame->slots[slot + (postArgs * frame->varArgs)] = crtVal;
				break;
			}
			case VAR_UPVALUE: {
				uint8_t slot = CHUNK_READ_BYTE(ptr);
				*getFrameClosure(frame)->upvalues[slot]->location = crtVal;
				break;
			}
			case VAR_GLOBAL: {
				uint16_t globalIdx = CHUNK_READ_USHORT(ptr);
				vm->globalValues.values[globalIdx] = crtVal;
				break;
			}
		}
	}
	pop(fiber);

	ret = ptr - chunk;

cleanup:
	releaseTemps(&temps);

	return ret;
}

#ifdef ELOX_DEBUG_TRACE_EXECUTION
void printStack(RunCtx *runCtx) {
	FiberCtx *fiber = runCtx->activeFiber;

	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "          ");
	CallFrame *frame = (fiber->frameCount > 0) ? &fiber->frames[fiber->frameCount - 1] : NULL;
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
	FiberCtx *fiber = runCtx->activeFiber;

	int exitFrame = fiber->frameCount;
	Value callable = peek(fiber, argCount);
#ifdef ELOX_DEBUG_TRACE_EXECUTION
	static uint32_t callIndex = 0;
	uint32_t callId = callIndex++;
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%08x--->", callId);
	printValue(runCtx, ELOX_IO_DEBUG, callable);
	printStack(runCtx);
#endif
	bool wasNative = false;
	bool ret = callValue(runCtx, callable, argCount, &wasNative);
	if (ELOX_UNLIKELY(!ret)) {
#ifdef ELOX_DEBUG_TRACE_EXECUTION
		eloxPrintf(runCtx, ELOX_IO_DEBUG, "%08x<---\n", callId);
		printStack(runCtx);
#endif
		return EXCEPTION_VAL;
	}
	if (wasNative) {
		// Native function already returned
#ifdef ELOX_DEBUG_TRACE_EXECUTION
		eloxPrintf(runCtx, ELOX_IO_DEBUG, "%08x<---\n", callId);
		printStack(runCtx);
#endif
		return peek(fiber, 0);
	}
	EloxInterpretResult res = run(runCtx, exitFrame);
#ifdef ELOX_DEBUG_TRACE_EXECUTION
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%08x<---\n", callId);
	printStack(runCtx);
#endif
	if (ELOX_UNLIKELY(res == ELOX_INTERPRET_RUNTIME_ERROR))
		return EXCEPTION_VAL;

	return peek(fiber, 0);
}

bool runChunk(RunCtx *runCtx) {
	FiberCtx *fiber = runCtx->activeFiber;

	EloxInterpretResult res = run(runCtx, fiber->frameCount);
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
	FiberCtx *fiber = runCtx->activeFiber;

	ObjString *b = AS_STRING(peek(fiber, 0));
	ObjString *a = AS_STRING(peek(fiber, 1));

	int length = a->string.length + b->string.length;
	uint8_t *chars = ALLOCATE(runCtx, uint8_t, length + 1);
	if (ELOX_UNLIKELY(chars == NULL)) {
		oomError(runCtx);
		return false;
	}
	memcpy(chars, a->string.chars, a->string.length);
	memcpy(chars + a->string.length, b->string.chars, b->string.length);
	chars[length] = '\0';

	ObjString *result = takeString(runCtx, chars, length, length + 1);
	if (ELOX_UNLIKELY(result == NULL)) {
		FREE(runCtx, uint8_t, chars);
		oomError(runCtx);
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

EloxInterpretResult run(RunCtx *runCtx, int exitFrame) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;
	CallFrame *frame = &fiber->frames[fiber->frameCount - 1];
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
	(getFrameFunction(frame)->chunk.constants.values[READ_BYTE()])
#define READ_CONST16(tmp) \
	(getFrameFunction(frame)->chunk.constants.values[READ_USHORT(tmp)])
#define READ_ARRAY(n, s) \
	(ip += (n) * (s), ip - (n) * (s) )
#define READ_STRING16(tmp) AS_STRING(READ_CONST16(tmp))
#define BINARY_OP(valueType, op) \
	do { \
		if (ELOX_UNLIKELY(!IS_NUMBER(peek(fiber, 0)) || !IS_NUMBER(peek(fiber, 1)))) { \
			frame->ip = ip; \
			runtimeError(runCtx, "Operands must be numbers"); \
			goto throwException; \
		} \
		double b = AS_NUMBER(pop(fiber)); \
		double a = AS_NUMBER(pop(fiber)); \
		push(fiber, valueType(a op b)); \
	} while (false)

	for (;;) {
#ifdef ELOX_ENABLE_COMPUTED_GOTO
dispatchLoop: ;
#endif

#ifdef ELOX_DEBUG_TRACE_EXECUTION
		printStack(runCtx);

		disassembleInstruction(runCtx, &getFrameFunction(frame)->chunk,
							   (int)(ip - getFrameFunction(frame)->chunk.code));
#endif
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
			DISPATCH_CASE(NUM_VARARGS): {
				push(fiber, NUMBER_VAL(frame->varArgs));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(EXPAND_VARARGS): {
				bool firstExpansion = READ_BYTE();
				expandVarArgs(fiber, frame, firstExpansion);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(EXPAND): {
				bool firstExpansion = READ_BYTE();
				expand(runCtx, firstExpansion);
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
			DISPATCH_CASE(GET_VARARG): {
				Value indexVal = pop(fiber);
				if (ELOX_UNLIKELY(!IS_NUMBER(indexVal))) {
					frame->ip = ip;
					runtimeError(runCtx, "Arg index is not a number");
					goto throwException;
				}
				int index = AS_NUMBER(indexVal);
				push(fiber, frame->slots[frame->fixedArgs + index + 1]);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_GLOBAL): {
				Value value = vm->globalValues.values[READ_USHORT()];
				if (ELOX_UNLIKELY(IS_UNDEFINED(value))) {
					frame->ip = ip;
					runtimeError(runCtx, "Undefined global variable");
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
			DISPATCH_CASE(SET_VARARG): {
				Value indexVal = peek(fiber, 1);
				if (ELOX_UNLIKELY(!IS_NUMBER(indexVal))) {
					frame->ip = ip;
					runtimeError(runCtx, "Arg index is not a number");
					goto throwException;
				}
				Value val = pop(fiber);
				int index = AS_NUMBER(indexVal);
				frame->slots[frame->fixedArgs + index + 1] = val;
				pop(fiber);
				push(fiber, val);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_GLOBAL): {
				uint16_t index = READ_USHORT();
				if (ELOX_UNLIKELY(IS_UNDEFINED(vm->globalValues.values[index]))) {
					frame->ip = ip;
					runtimeError(runCtx, "Undefined global variable");
					goto throwException;
				}
				vm->globalValues.values[index] = peek(fiber, 0);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_UPVALUE): {
				uint8_t slot = READ_BYTE();
				push(fiber, *getFrameClosure(frame)->upvalues[slot]->location);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_UPVALUE): {
				uint8_t slot = READ_BYTE();
				*getFrameClosure(frame)->upvalues[slot]->location = peek(fiber, 0);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_PROP): {
				ObjString *name = READ_STRING16();
				frame->ip = ip;
				if (ELOX_UNLIKELY(!getProperty(runCtx, name)))
					goto throwException;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_MEMBER_PROP): {
				uint16_t propRef = READ_USHORT();
				ObjInstance *instance = AS_INSTANCE(peek(fiber, 0));
				ObjClass *parentClass = getFrameFunction(frame)->parentClass;
				MemberRef *ref = &parentClass->memberRefs[propRef];
				Value *prop = resolveRef(ref, instance);
				pop(fiber); // Instance
				push(fiber, *prop);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(MAP_GET): {
				Value instanceVal = peek(fiber, 0);

				ObjString *name = READ_STRING16();

				if (ELOX_LIKELY(IS_MAP(instanceVal))) {
					ObjMap *map = AS_MAP(instanceVal);

					Value value;
					frame->ip = ip;
					Error error = ERROR_INITIALIZER(runCtx);
					bool found = valueTableGet(&map->items, OBJ_VAL(name), &value, &error);
					if (ELOX_UNLIKELY(error.raised))
						goto throwException;
					if (!found)
						value = NIL_VAL;
					pop(fiber); // map
					push(fiber, value);
				} else {
					frame->ip = ip;
					runtimeError(runCtx, "Argument is not a map");
					goto throwException;
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_PROP): {
				Value instanceVal = peek(fiber, 1);

				if (ELOX_LIKELY(IS_INSTANCE(instanceVal))) {
					ObjInstance *instance = AS_INSTANCE(instanceVal);
					ObjString *fieldName = READ_STRING16();
					if (ELOX_UNLIKELY(!setInstanceField(instance, fieldName, peek(fiber, 0)))) {
						frame->ip = ip;
						runtimeError(runCtx, "Undefined field '%s'", fieldName->string.chars);
						goto throwException;
					}
					Value value = pop(fiber);
					pop(fiber);
					push(fiber, value);
				} else {
					frame->ip = ip;
					runtimeError(runCtx, "Only instances have fields");
					goto throwException;
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_MEMBER_PROP): {
				uint16_t propRef = READ_USHORT();
				ObjInstance *instance = AS_INSTANCE(peek(fiber, 1));
				ObjClass *parentClass = getFrameFunction(frame)->parentClass;
				MemberRef *ref = &parentClass->memberRefs[propRef];
				Value *prop = resolveRef(ref, instance);
				*prop = peek(fiber, 0);
				Value value = pop(fiber);
				pop(fiber);
				push(fiber, value);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(MAP_SET): {
				Value instanceVal = peek(fiber, 1);
				if (ELOX_LIKELY(IS_MAP(instanceVal))) {
					ObjMap *map = AS_MAP(instanceVal);
					ObjString *index = READ_STRING16();
					Value value = peek(fiber, 0);
					frame->ip = ip;
					Error error = ERROR_INITIALIZER(runCtx);
					valueTableSet(&map->items, OBJ_VAL(index), value, &error);
					if (ELOX_UNLIKELY(error.raised))
						goto throwException;
					value = pop(fiber);
					pop(fiber);
					push(fiber, value);
				} else {
					frame->ip = ip;
					runtimeError(runCtx, "Argument is not a map");
					goto throwException;
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_SUPER): {
				uint16_t propRef = READ_USHORT();
				ObjInstance *instance = AS_INSTANCE(peek(fiber, 0));
				MemberRef *ref = &instance->clazz->memberRefs[propRef];
				Value method = *resolveRef(ref, instance);
				ObjBoundMethod *bound = newBoundMethod(runCtx, peek(fiber, 0), AS_METHOD(method));
				if (ELOX_UNLIKELY(bound == NULL)) {
					push(fiber, OBJ_VAL(vm->builtins.oomError));
					goto throwException;
				}
				pop(fiber);
				push(fiber, OBJ_VAL(bound));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(EQUAL): {
				Value b = pop(fiber);
				Value a = pop(fiber);
				Error error = ERROR_INITIALIZER(runCtx);
				bool eq = valuesEquals(a, b, &error);
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
						runtimeError(runCtx, "Operands must be two numbers or two strings");
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
					runtimeError(runCtx, "Operands must be numbers");
					goto throwException;
				}
				double b = AS_NUMBER(pop(fiber));
				double a = AS_NUMBER(pop(fiber));
				push(fiber, NUMBER_VAL(fmod(a, b)));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INSTANCEOF): {
				if (ELOX_UNLIKELY(!IS_CLASS(peek(fiber, 0)))) {
					frame->ip = ip;
					runtimeError(runCtx, "Right-hand operand must be a class");
					goto throwException;
				}
				ObjClass *clazz = AS_CLASS(pop(fiber));
				ObjClass *instClass = classOfFollowInstance(vm, pop(fiber));
				if (instClass != NULL)
					push(fiber, BOOL_VAL(instanceOf(clazz, instClass)));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(IN): {
				frame->ip = ip;
				if (ELOX_UNLIKELY(!in(runCtx)))
					goto throwException;

				DISPATCH_BREAK;
			}
			DISPATCH_CASE(NOT):
				push(fiber, BOOL_VAL(isFalsey(pop(fiber))));
				DISPATCH_BREAK;
			DISPATCH_CASE(NEGATE):
				if (ELOX_UNLIKELY(!IS_NUMBER(peek(fiber, 0)))) {
					frame->ip = ip;
					runtimeError(runCtx, "Operand must be a number");
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
				bool wasNative;
				if (ELOX_UNLIKELY(!callValue(runCtx, peek(fiber, argCount), argCount, &wasNative)))
					goto throwException;
				frame = &fiber->frames[fiber->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INVOKE): {
				ObjString *method = READ_STRING16();
				int argCount = READ_BYTE();
				bool hasExpansions = READ_BYTE();
				if (hasExpansions)
					argCount += AS_NUMBER(pop(fiber));
				frame->ip = ip;
				if (ELOX_UNLIKELY(!invoke(runCtx, method, argCount)))
					goto throwException;
				frame = &fiber->frames[fiber->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(MEMBER_INVOKE): {
				uint16_t propRef = READ_USHORT();
				int argCount = READ_BYTE();
				bool hasExpansions = READ_BYTE();
				if (hasExpansions)
					argCount += AS_NUMBER(pop(fiber));
				ObjInstance *instance = AS_INSTANCE(peek(fiber, argCount));
				MemberRef *ref = &instance->clazz->memberRefs[propRef];
				Value *method = resolveRef(ref, instance);
				bool isMember = (ref->refType == REFTYPE_CLASS_MEMBER);
				frame->ip = ip;
				if (ELOX_UNLIKELY(!invokeMember(runCtx, method, isMember, argCount)))
					goto throwException;
				frame = &fiber->frames[fiber->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SUPER_INVOKE): {
				uint16_t propRef = READ_USHORT();
				int argCount = READ_BYTE();
				bool hasExpansions = READ_BYTE();
				if (hasExpansions)
					argCount += AS_NUMBER(pop(fiber));
				ObjInstance *instance = AS_INSTANCE(peek(fiber, argCount));
				MemberRef *ref = &instance->clazz->memberRefs[propRef];
				Value *method = resolveRef(ref, NULL);
				frame->ip = ip;
				if (ELOX_UNLIKELY(!invokeMember(runCtx, method, true, argCount)))
					goto throwException;
				frame = &fiber->frames[fiber->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SUPER_INIT): {
				int argCount = READ_BYTE();
				bool hasExpansions = READ_BYTE();
				ObjClass *superclass = AS_CLASS(pop(fiber));
				Value init = superclass->initializer;
				if (!IS_NIL(init)) {
#ifdef ELOX_DEBUG_TRACE_EXECUTION
					eloxPrintf(runCtx, ELOX_IO_DEBUG, "===>%s init\n", superclass->name->string.chars);
#endif
					if (hasExpansions)
						argCount += AS_NUMBER(pop(fiber));
					frame->ip = ip;
					bool wasNative;
					if (!callMethod(runCtx, AS_OBJ(init), argCount, 0, &wasNative))
						goto throwException;
					frame = &fiber->frames[fiber->frameCount - 1];
					ip = frame->ip;
				} else {
					// no return, discard arguments
					//vm->stackTop = frame->slots;
				}
				pop(fiber); // this
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CLOSURE): {
				ObjFunction *function = AS_FUNCTION(READ_CONST16());
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
					} else
						closure->upvalues[i] = getFrameClosure(frame)->upvalues[index];
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
				fiber->frameCount--;

				fiber->stackTop = frame->slots - frame->argOffset;
				push(fiber, result);
				if (fiber->frameCount == exitFrame)
					return ELOX_INTERPRET_OK;
				frame = &fiber->frames[fiber->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(END): {
				return ELOX_INTERPRET_OK;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CLASS): {
				ObjClass *clazz = newClass(runCtx, READ_STRING16());
				if (ELOX_UNLIKELY(clazz == NULL)) {
					push(fiber, OBJ_VAL(vm->builtins.oomError));
					goto throwException;
				}
				push(fiber, OBJ_VAL(clazz));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(ANON_CLASS): {
				ObjClass *clazz = newClass(runCtx, NULL);
				if (ELOX_UNLIKELY(clazz == NULL)) {
					push(fiber, OBJ_VAL(vm->builtins.oomError));
					goto throwException;
				}
				push(fiber, OBJ_VAL(clazz));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INHERIT): {
				frame->ip = ip;
				Error error = ERROR_INITIALIZER(runCtx);
				inherit(&error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(METHOD): {
				frame->ip = ip;
				Error error = ERROR_INITIALIZER(runCtx);
				defineMethod(READ_STRING16(), &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(FIELD): {
				frame->ip = ip;
				Error error = ERROR_INITIALIZER(runCtx);
				defineField(READ_STRING16(), &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(STATIC): {
				frame->ip = ip;
				Error error = ERROR_INITIALIZER(runCtx);
				defineStatic(READ_STRING16(), &error);
				if (ELOX_UNLIKELY(error.raised))
					goto throwException;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(RESOLVE_MEMBERS): {
				uint16_t numSlots = READ_USHORT();
				ObjClass *clazz = AS_CLASS(peek(fiber, 1));
				clazz->memberRefs = ALLOCATE(runCtx, MemberRef, numSlots);
				if (ELOX_UNLIKELY(clazz->memberRefs == NULL)) {
					push(fiber, OBJ_VAL(vm->builtins.oomError));
					goto throwException;
				}
				clazz->memberRefCount = numSlots;

				for (int i = 0; i < numSlots; i++) {
					uint8_t slotType = READ_BYTE();
					ObjString *propName = READ_STRING16();
					uint16_t slot = READ_USHORT();

					frame->ip = ip;
					if (ELOX_UNLIKELY(!resolveMember(runCtx, clazz, slotType, propName, slot)))
						goto throwException;
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(ARRAY_BUILD): {
				ObjType objType = READ_BYTE();
				uint16_t itemCount = READ_USHORT();
				ObjArray *array = newArray(runCtx, itemCount, objType);
				if (ELOX_UNLIKELY(array == NULL)) {
					push(fiber, OBJ_VAL(vm->builtins.oomError));
					goto throwException;
				}

				push(fiber, OBJ_VAL(array));
				for (int i = itemCount; i > 0; i--) {
					bool ret = appendToArray(runCtx, array, peek(fiber, i));
					if (ELOX_UNLIKELY(!ret)) {
						push(fiber, OBJ_VAL(vm->builtins.oomError));
						goto throwException;
					}
				}
				pop(fiber);

				// pop constructor arguments from the stack
				popn(fiber, itemCount);

				push(fiber, OBJ_VAL(array));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INDEX): {
				frame->ip = ip;
				if (ELOX_UNLIKELY(!indexValue(runCtx)))
					goto throwException;

				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INDEX_STORE): {
				frame->ip = ip;
				if (ELOX_UNLIKELY(!indexStore(runCtx)))
					goto throwException;

				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SLICE): {
				frame->ip = ip;
				if (ELOX_UNLIKELY(!sliceValue(runCtx)))
					goto throwException;

				DISPATCH_BREAK;
			}
			DISPATCH_CASE(MAP_BUILD): {
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

				ObjInstance *instance = AS_INSTANCE(peek(fiber, 0));
				push(fiber, stacktrace);
				// TODO: check copyString
				ObjString *stacktraceName = copyString(runCtx, ELOX_USTR_AND_LEN("stacktrace"));
				push(fiber, OBJ_VAL(stacktraceName));
				setInstanceField(instance, stacktraceName, stacktrace);
				popn(fiber, 2);
				vm->handlingException++;
				if (propagateException(runCtx, exitFrame)) {
					vm->handlingException--;
					frame = &fiber->frames[fiber->frameCount - 1];
					ip = frame->ip;
					DISPATCH_BREAK;
				}
				vm->handlingException--;

				// unroll call stack
				fiber->frameCount = exitFrame;
				CallFrame *retFrame = &fiber->frames[fiber->frameCount];
				fiber->stackTop = retFrame->slots + 1;
				// set exception as result
				push(fiber, OBJ_VAL(instance));

				return ELOX_INTERPRET_RUNTIME_ERROR;
			}
			DISPATCH_CASE(PUSH_EXH): {
				uint8_t stackLevel = READ_BYTE();
				uint16_t handlerTableAddress = READ_USHORT();
				frame->ip = ip;
				if (ELOX_UNLIKELY(!pushExceptionHandler(runCtx, stackLevel, handlerTableAddress)))
					goto throwException;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(UNROLL_EXH): {
				uint8_t newHandlerCount = READ_BYTE();
				unrollExceptionHandlerStack(runCtx, newHandlerCount, false);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(UNROLL_EXH_R): {
				uint8_t newHandlerCount = READ_BYTE();
				unrollExceptionHandlerStack(runCtx, newHandlerCount, true);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(UNROLL_EXH_F): {
				uint8_t newHandlerCount = READ_BYTE();
				frame->handlerCount = newHandlerCount;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(FOREACH_INIT): {
				uint8_t hasNextSlot = READ_BYTE();
				bool hasNextPostArgs = READ_BYTE();
				uint8_t nextSlot = READ_BYTE();
				bool nextPostArgs = READ_BYTE();
				Value iterableVal = peek(fiber, 0);

				ObjInstance *iterator = NULL;
				if (IS_INSTANCE(iterableVal) &&
					instanceOf(vm->builtins.iteratorClass, AS_INSTANCE(iterableVal)->clazz))
					iterator = AS_INSTANCE(pop(fiber));
				else {
					bool hasIterator = false;
					ObjClass *clazz = classOfFollowInstance(vm, iterableVal);
					if (clazz != NULL) {
						if (bindMethod(runCtx, clazz, vm->builtins.iteratorString))
							hasIterator = true;
					}
					if (hasIterator) {
						frame->ip = ip;
						Value iteratorVal = runCall(runCtx, 0);
						if (ELOX_UNLIKELY(IS_EXCEPTION(iteratorVal)))
							goto throwException;

						pop(fiber);

						if (IS_INSTANCE(iteratorVal) &&
							instanceOf(vm->builtins.iteratorClass, AS_INSTANCE(iteratorVal)->clazz))
							iterator = AS_INSTANCE(iteratorVal);
					}
				}

				if (iterator == NULL) {
					frame->ip = ip;
					runtimeError(runCtx, "Attempt to iterate non-iterable value");
					goto throwException;
				}

				ObjClass *iteratorClass = iterator->clazz;

				push(fiber, OBJ_VAL(iterator));
				bindMethod(runCtx, iteratorClass, vm->builtins.hasNextString);
				frame->slots[hasNextSlot + (hasNextPostArgs * frame->varArgs)] = pop(fiber);

				push(fiber, OBJ_VAL(iterator));
				bindMethod(runCtx, iteratorClass, vm->builtins.nextString);
				frame->slots[nextSlot + (nextPostArgs * frame->varArgs)] = pop(fiber);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(UNPACK): {
				bool error = false;
				ip += doUnpack(runCtx, frame, ip, &error);
				if (ELOX_UNLIKELY(error))
					goto throwException;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(IMPORT): {
				ObjString *moduleName = READ_STRING16();
				uint16_t numArgs = READ_USHORT();
				uint8_t *args = READ_ARRAY(numArgs, 2);
				frame->ip = ip;
				if (ELOX_UNLIKELY(!import(runCtx, moduleName, numArgs, args,
										  getFrameFunction(frame)->chunk.constants.values))) {
					goto throwException;
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(DATA): {
				frame->ip = ip;
				runtimeError(runCtx, "Attempted to execute data section");
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

void pushCompilerState(RunCtx *runCtx, CompilerState *compilerState) {
	VM *vm = runCtx->vm;
	VMEnv *env = runCtx->vmEnv;

	if (vm->compilerCapacity < vm->compilerCount + 1) {
		vm->compilerCapacity = GROW_CAPACITY(vm->compilerCapacity);
		vm->compilerStack =
			(CompilerState **)env->realloc(vm->compilerStack,
										   sizeof(CompilerState *) * vm->compilerCapacity,
										   env->allocatorUserData);
		if (vm->compilerStack == NULL)
			exit(1);
	}

	vm->compilerStack[vm->compilerCount++] = compilerState;
}

void popCompilerState(RunCtx *runCtx) {
	VM *vm = runCtx->vm;

	vm->compilerCount--;
}

EloxInterpretResult interpret(RunCtx *runCtx, uint8_t *source, const String *moduleName) {
	FiberCtx *fiber = runCtx->activeFiber;

	ObjFunction *function = compile(runCtx, source, moduleName);
	if (function == NULL)
		return ELOX_INTERPRET_COMPILE_ERROR;

	push(fiber, OBJ_VAL(function));
	callFunction(runCtx, function, 0, 0);

	DBG_PRINT_STACK("DBGa", runCtx);

	EloxInterpretResult res = run(runCtx, 0);
	DBG_PRINT_STACK("DBGb1", runCtx);
	popn(fiber, 1);

	DBG_PRINT_STACK("DBGb", runCtx);

	return res;
}
