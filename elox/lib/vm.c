#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <math.h>

#include "elox/common.h"
#include "elox/compiler.h"
#include "elox/debug.h"
#include "elox/object.h"
#include "elox/memory.h"
#include "elox/state.h"
#include "elox/builtins.h"
#include "elox.h"

static void resetStack(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	vm->stackTop = vm->stack;
	vm->frameCount = 0;
	vm->openUpvalues = NULL;
}

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

static bool call(VMCtx *vmCtx, Obj *callee, ObjFunction *function, int argCount) {
	VM *vm = &vmCtx->vm;

	int missingArgs = 0;
	int stackArgs = argCount;
	if (argCount != function->arity) {
		if (argCount < function->arity) {
			missingArgs = function->arity - argCount;
			for (int i = 0; i < missingArgs; i++)
				push(vm, NIL_VAL);
		} else {
			if (argCount > function->maxArgs) {
				int extraArgs = argCount - function->maxArgs;
				stackArgs -= extraArgs;
				popn(vm, extraArgs);
			}
		}
	}

	if (ELOX_UNLIKELY(vm->frameCount == FRAMES_MAX)) {
		runtimeError(vmCtx, "Stack overflow");
		return false;
	}

	CallFrame *frame = &vm->frames[vm->frameCount++];
	frame->function = (Obj *)callee;
	frame->ip = function->chunk.code;
	frame->handlerCount = 0;

	frame->slots = vm->stackTop - stackArgs - 1;
	frame->fixedArgs = function->arity;
	frame->varArgs = argCount + missingArgs - function->arity;
	return true;
}

static bool callClosure(VMCtx *vmCtx, ObjClosure *closure, int argCount) {
	return call(vmCtx, (Obj *)closure, closure->function, argCount);
}

static bool callNativeClosure(VMCtx *vmCtx, ObjNativeClosure *closure, int argCount, bool method) {
	VM *vm = &vmCtx->vm;

	CallFrame *frame = &vm->frames[vm->frameCount++];
	// for native methods include 'this'
	frame->slots = vm->stackTop - argCount - (int)method;

	NativeClosureFn native = closure->nativeFunction;
	Args args = { .frame = frame };
	Value result = native(vmCtx,
						  argCount + (int)method, &args,
						  closure->upvalueCount, closure->upvalues);
	if (ELOX_LIKELY(!IS_EXCEPTION(result))) {
		vm->stackTop -= argCount + 1;
		push(vm, result);
		return true;
	}
	// TODO: is this right for exceptions?
	vm->frameCount--;
	return false;
}

static bool callFunction(VMCtx *vmCtx, ObjFunction *function, int argCount) {
	return call(vmCtx, (Obj *)function, function, argCount);
}

static bool callNative(VMCtx *vmCtx, NativeFn native, int argCount, bool method) {
	VM *vm = &vmCtx->vm;

	CallFrame *frame = &vm->frames[vm->frameCount++];
	// for native methods include 'this'
	frame->slots = vm->stackTop - argCount - (int)method;

	Args args = { .frame = frame };
	Value result = native(vmCtx, argCount + (int)method, &args);

	if (ELOX_LIKELY(!IS_EXCEPTION(result))) {
		vm->frameCount--;
		vm->stackTop -= argCount + 1;
		push(vm, result);
		return true;
	}
	// TODO: is this right for exceptions?
	vm->frameCount--;
	return false;
}

static bool callMethod(VMCtx *vmCtx, Obj *function, int argCount, bool *wasNative) {
	switch (function->type) {
		case OBJ_FUNCTION:
			return callFunction(vmCtx, (ObjFunction *)function, argCount);
		case OBJ_CLOSURE:
			return callClosure(vmCtx, (ObjClosure *)function, argCount);
		case OBJ_NATIVE_CLOSURE:
			*wasNative = true;
			return callNativeClosure(vmCtx, (ObjNativeClosure *)function, argCount, true);
		case OBJ_NATIVE:
			*wasNative = true;
			return callNative(vmCtx, ((ObjNative *)function)->function, argCount, true);
		default:
			runtimeError(vmCtx, "Can only call functions and classes");
			break;
	}
	return false;
}

static void printStackTrace(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	int frameNo = 0;
	for (int i = vm->frameCount - 1; i >= 0; i--) {
		CallFrame *frame = &vm->frames[i];
		ObjFunction *function = getFrameFunction(frame);
		// -1 because the IP is sitting on the next instruction to be executed.
		size_t instruction = frame->ip - function->chunk.code - 1;
		uint32_t lineno = getLine(&function->chunk, instruction);
		fprintf(stderr, "#%d [line %d] in %s()\n",
				frameNo,
				lineno,
				function->name == NULL ? "script" : function->name->string.chars);
		frameNo++;
	}
}

Value runtimeError(VMCtx *vmCtx, const char *format, ...) {
	VM *vm = &vmCtx->vm;

	if (vm->handlingException) {
		fprintf(stderr, "Exception raised while handling exception: ");
		va_list args;
		va_start(args, format);
		vfprintf(stderr, format, args);
		va_end(args);
		fputs("\n\n", stderr);

		printStackTrace(vmCtx);
		exit(1);
	}

	vm->handlingException++;

	HeapCString msg;
	initHeapStringWithSize(vmCtx, &msg, 16);
	va_list args;
	va_start(args, format);
	addHeapStringVFmt(vmCtx, &msg, format, args);
	va_end(args);

	ObjInstance *errorInst = newInstance(vmCtx, vm->runtimeExceptionClass);
	push(vm, OBJ_VAL(errorInst));
	ObjString *msgObj = takeString(vmCtx, msg.chars, msg.length, msg.capacity);
	push(vm, OBJ_VAL(msgObj));
	bool wasNative;
	callMethod(vmCtx, AS_OBJ(vm->runtimeExceptionClass->initializer), 1, &wasNative);
	pop(vm);
	push(vm, OBJ_VAL(errorInst));

	vm->handlingException--;
	return EXCEPTION_VAL;
}

void ensureStack(VMCtx *vmCtx, int required) {
	VM *vm = &vmCtx->vm;

	if (ELOX_UNLIKELY(required > vm->stackCapacity)) {
		int oldCapacity = vm->stackTopMax - vm->stack + 1;
		int newCapacity = GROW_CAPACITY(oldCapacity);
		Value *oldStack = vm->stack;

		vm->stack = GROW_ARRAY(vmCtx, Value, vm->stack, oldCapacity, newCapacity);
		vm->stackTop = vm->stack + oldCapacity - 1;
		vm->stackTopMax = vm->stack + newCapacity -1;
		vm->stackCapacity = newCapacity;

		if (oldStack != vm->stack) {
			// the stack moved, recalculate all pointers that point to the old stack

			for (int i = 0; i < vm->frameCount; i++) {
				CallFrame *frame = &vm->frames[i];
				frame->slots = vm->stack + (frame->slots - oldStack);
			}

			for (ObjUpvalue *upvalue = vm->openUpvalues; upvalue != NULL; upvalue = upvalue->next) {
				upvalue->location = vm->stack + (upvalue->location - oldStack);
			}
		}
	}
}

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

static void defineMethod(VMCtx *vmCtx, ObjString *name) {
	VM *vm = &vmCtx->vm;

	Value method = peek(vm, 0);
	ObjClass *clazz = AS_CLASS(peek(vm, 1));
	ObjFunction *methodFunction = getValueFunction(method);
	methodFunction->parentClass = clazz;

	if (name == clazz->name)
		clazz->initializer = method;
	else {
		tableSet(vmCtx, &clazz->methods, name, method);
		if (name == vm->hashCodeString)
			clazz->hashCode = method;
		else if (name == vm->equalsString)
			clazz->equals = method;
	}
	pop(vm);
}

static void defineField(VMCtx *vmCtx, ObjString *name) {
	VM *vm = &vmCtx->vm;
	ObjClass *clazz = AS_CLASS(peek(vm, 0));
	int index = clazz->fields.count;
	tableSet(vmCtx, &clazz->fields, name, NUMBER_VAL(index));
}

void registerNativeFunction(VMCtx *vmCtx,
							const String *name, const String *moduleName,
							NativeFn function) {
	VM *vm = &vmCtx->vm;
	ObjNative *native = newNative(vmCtx, function);
	push(vm, OBJ_VAL(native));
	uint16_t globalIdx = globalIdentifierConstant(vmCtx, name, moduleName);
	vm->globalValues.values[globalIdx] = peek(vm, 0);
	pop(vm);

	if (stringEquals(moduleName, &eloxBuiltinModule)) {
		// already interned and referenced in global table
		ObjString *nameStr = copyString(vmCtx, name->chars, name->length);
		tableSet(vmCtx, &vm->builtinSymbols, nameStr, OBJ_VAL(native));
	}
}

void initVM(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	vm->stack = NULL;
	vm->stack = GROW_ARRAY(vmCtx, Value, vm->stack, 0, MIN_STACK);
	vm->stackTopMax = vm->stack + MIN_STACK - 1;
	vm->stackCapacity = vm->stackTopMax - vm->stack + 1;
	resetStack(vmCtx);

	vm->handlingException = 0;
	stc64_init(&vm->prng, 64);
	initPrimeGen(&vm->primeGen, 0);
	vm->objects = NULL;
	vm->bytesAllocated = 0;
	vm->nextGC = 1024 * 1024;

	vm->compilerCount = 0;
	vm->compilerCapacity = 0;
	vm->compilerStack = NULL;

	vm->grayCount = 0;
	vm->grayCapacity = 0;
	vm->grayStack = NULL;

	initValueTable(&vm->globalNames);
	initValueArray(&vm->globalValues);

	initTable(&vm->modules);
	initTable(&vm->builtinSymbols);

	initHandleSet(&vm->handles);

	initTable(&vm->strings);

	registerBuiltins(vmCtx);
}

void destroyVMCtx(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	freeValueTable(vmCtx, &vm->globalNames);
	freeValueArray(vmCtx, &vm->globalValues);
	freeTable(vmCtx, &vm->builtinSymbols);
	freeTable(vmCtx, &vm->modules);
	freeHandleSet(vmCtx, &vm->handles);
	freeTable(vmCtx, &vm->strings);

	clearBuiltins(vm);
	freeObjects(vmCtx);

	vmCtx->free(vm->compilerStack, vmCtx->allocatorUserdata);

	FREE_ARRAY(vmCtx, Value, vm->stack, vm->stackTopMax - vm->stack + 1);
}

static Value getStackTrace(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

#define MAX_LINE_LENGTH 512

	int maxStackTraceLength = vm->frameCount * MAX_LINE_LENGTH;
	char *stacktrace = ALLOCATE(vmCtx, char, maxStackTraceLength);
	uint16_t index = 0;
	int frameNo = 0;
	for (int i = vm->frameCount - 1; i >= 0; i--) {
		CallFrame *frame = &vm->frames[i];
		ObjFunction *function = getFrameFunction(frame);
		// -1 because the IP is sitting on the next instruction to be executed.
		size_t instruction = frame->ip - function->chunk.code - 1;
		uint32_t lineno = getLine(&function->chunk, instruction);
		index += snprintf(&stacktrace[index], MAX_LINE_LENGTH, "#%d [line %d] in %s()\n",
						  frameNo,
						  lineno,
						  function->name == NULL ? "script" : function->name->string.chars);
		frameNo++;
	}
	stacktrace = GROW_ARRAY(vmCtx, char, stacktrace, maxStackTraceLength, index + 1);
	return OBJ_VAL(takeString(vmCtx, stacktrace, index, index + 1));

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

// TODO: optimize
/*static bool instanceof(ObjClass *clazz, ObjInstance *instance) {
	ObjClass *instanceClass = instance->clazz;
	return ((instanceClass->classId % clazz->classId) == 0);
	for (ObjClass *c = instance->clazz; c != NULL;
		 c = (IS_NIL(c->super)) ? NULL : AS_CLASS(c->super)) {
		if (c == clazz)
			return true;
	}
	return false;
}*/

static bool instanceOf(ObjClass *clazz, ObjClass *instanceClass) {
	return ((instanceClass->classId % clazz->classId) == 0);
}

static bool propagateException(VMCtx *vmCtx, int exitFrame) {
	VM *vm = &vmCtx->vm;

	ObjInstance *exception = AS_INSTANCE(peek(vm, 0));

	while (vm->frameCount > exitFrame) {
		CallFrame *frame = &vm->frames[vm->frameCount - 1];
		for (int handlerStack = frame->handlerCount; handlerStack > 0; handlerStack--) {
			TryBlock *tryBlock = &frame->handlerStack[handlerStack - 1];
			uint16_t handlerTableOffset = tryBlock->handlerTableOffset;
			ObjFunction *frameFunction = getFrameFunction(frame);
			uint8_t *handlerTable = frameFunction->chunk.code + handlerTableOffset;
			uint8_t numHandlers = handlerTable[0] / 5;
			for (int i = 0; i < numHandlers; i++) {
				uint8_t *handlerRecord = handlerTable + 1 + (5 * i);
				VarType typeVarType = handlerRecord[0];
				uint16_t typeHandle = (uint16_t)(handlerRecord[1] << 8);
				typeHandle |= handlerRecord[2];
				Value classVal = NIL_VAL;
				switch (typeVarType) {
					case VAR_LOCAL:
						classVal = frame->slots[typeHandle];
						break;
					case VAR_UPVALUE:
						classVal = *getFrameClosure(frame)->upvalues[typeHandle]->location;
						break;
					case VAR_GLOBAL: {
						classVal = vm->globalValues.values[typeHandle];
						if (ELOX_UNLIKELY(IS_UNDEFINED(classVal))) {
							runtimeError(vmCtx, "Undefined global variable");
							return false;
						}
						if (ELOX_UNLIKELY(!IS_CLASS(classVal))) {
							runtimeError(vmCtx, "Not a type to catch");
							return false;
						}
						break;
					}

				}
				ObjClass *handlerClass = AS_CLASS(classVal);
				if (instanceOf(handlerClass, exception->clazz)) {
					uint16_t handlerAddress = (uint16_t)(handlerRecord[3] << 8);
					handlerAddress |= handlerRecord[4];
					frame->ip = &frameFunction->chunk.code[handlerAddress];
					Value exception = pop(vm);
					vm->stackTop = frame->slots + tryBlock->stackOffset;
					push(vm, exception);
					return true;
				}
			}
		}
		vm->frameCount--;
	}

	DBG_PRINT_STACK("DBGExc", vm);

	// Do not print the exception here if we are inside an internal call
	if (exitFrame == 0) {
		fprintf(stderr, "Unhandled exception %s", exception->clazz->name->string.chars);
		Value message;
		if (getInstanceValue(exception, copyString(vmCtx, ELOX_STR_AND_LEN("message")), &message))
			fprintf(stderr, ": %s\n", AS_CSTRING(message));
		else
			fprintf(stderr, "\n");
		fflush(stderr);
		Value stacktrace;
		if (getInstanceValue(exception, copyString(vmCtx, ELOX_STR_AND_LEN("stacktrace")), &stacktrace)) {
			fprintf(stderr, "%s", AS_CSTRING(stacktrace));
			fflush(stderr);
		}
	}
	return false;
}

static bool pushExceptionHandler(VMCtx *vmCtx, uint8_t stackLevel, uint16_t handlerTableAddress) {
	VM *vm = &vmCtx->vm;

	CallFrame *frame = &vm->frames[vm->frameCount - 1];
	if (frame->handlerCount == MAX_CATCH_HANDLER_FRAMES) {
		runtimeError(vmCtx, "Too many nested exception handlers in one function");
		return false;
	}

	TryBlock *tryBlock = &frame->handlerStack[stackLevel];
	if (stackLevel >= frame->handlerCount)
		frame->handlerCount = stackLevel + 1;

	tryBlock->handlerTableOffset = handlerTableAddress;
	tryBlock->stackOffset = vm->stackTop - frame->slots;
	return true;
}

static bool callValue(VMCtx *vmCtx, Value callee, int argCount, bool *wasNative) {
	VM *vm = &vmCtx->vm;

	if (IS_OBJ(callee)) {
		switch (OBJ_TYPE(callee)) {
			case OBJ_BOUND_METHOD: {
				ObjBoundMethod *bound = AS_BOUND_METHOD(callee);

				vm->stackTop[-argCount - 1] = bound->receiver;
				return callMethod(vmCtx, bound->method, argCount, wasNative);
			}
			case OBJ_CLASS: {
				ObjClass *clazz = AS_CLASS(callee);
				vm->stackTop[-argCount - 1] = OBJ_VAL(newInstance(vmCtx, clazz));
				if (!IS_NIL(clazz->initializer)) {
#ifdef ELOX_DEBUG_TRACE_EXECUTION
				printf("--->%s init\n", clazz->name->string.chars);
#endif
					return callMethod(vmCtx, AS_OBJ(clazz->initializer), argCount, wasNative);
				} else if (argCount != 0) {
					runtimeError(vmCtx, "Expected 0 arguments but got %d", argCount);
					return false;
				}
				return true;
			}
			case OBJ_CLOSURE:
				return callClosure(vmCtx, AS_CLOSURE(callee), argCount);
			case OBJ_NATIVE_CLOSURE:
				*wasNative = true;
				return callNativeClosure(vmCtx, AS_NATIVE_CLOSURE(callee), argCount, false);
			case OBJ_FUNCTION:
				return callFunction(vmCtx, AS_FUNCTION(callee), argCount);
			case OBJ_NATIVE:
				*wasNative = true;
				return callNative(vmCtx, AS_NATIVE(callee), argCount, false);
			default:
				break; // Non-callable object type
		}
	}
	runtimeError(vmCtx, "Can only call functions and classes");
	return false;
}

static bool invokeFromClass(VMCtx *vmCtx, ObjClass *clazz, ObjString *name, int argCount) {
	Value method;
	if (ELOX_UNLIKELY(!tableGet(&clazz->methods, name, &method))) {
		runtimeError(vmCtx, "Undefined property '%s'", name->string.chars);
		return false;
	}
	bool wasNative;
	return callMethod(vmCtx, AS_OBJ(method), argCount, &wasNative);
}

static inline ObjClass *classOf(VM *vm, const Obj *obj, ObjInstance **instance) {
	switch (obj->type) {
		case OBJ_INSTANCE:
			*instance = (ObjInstance *)obj;
			return ((ObjInstance *)obj)->clazz;
		case OBJ_STRING:
			return vm->stringClass;
		case OBJ_ARRAY:
			return vm->arrayClass;
		case OBJ_MAP:
			return vm->mapClass;
		default:
			break;
	}

	*instance = NULL;
	return NULL;
}

static ObjClass *classOfValue(VM *vm, Value val, ObjInstance **instance) {
	if (!IS_OBJ(val)) {
		if (IS_NUMBER(val))
			return vm->numberClass;
		return NULL;
	}
	return classOf(vm, AS_OBJ(val), instance);
}

static bool invoke(VMCtx *vmCtx, ObjString *name, int argCount) {
	VM *vm = &vmCtx->vm;

	Value receiver = peek(vm, argCount);

	ObjInstance *instance = NULL;
	ObjClass *clazz = classOfValue(vm, receiver, &instance);
	if (ELOX_UNLIKELY(clazz == NULL)) {
		runtimeError(vmCtx, "Only instances have methods");
		return false;
	}

	if (instance != NULL) {
		Value value;
		if (getInstanceValue(instance, name, &value)) {
			vm->stackTop[-argCount - 1] = value;
			bool wasNative;
			return callValue(vmCtx, value, argCount, &wasNative);
		}
	}

	return invokeFromClass(vmCtx, clazz, name, argCount);
}

static bool invokeMember(VMCtx *vmCtx, Value *member, bool isMember, int argCount) {
	VM *vm = &vmCtx->vm;
	bool wasNative;
	if (!isMember) {
		vm->stackTop[-argCount - 1] = *member;
		return callValue(vmCtx, *member, argCount, &wasNative);
	} else
		return callMethod(vmCtx, AS_OBJ(*member), argCount, &wasNative);
}

static bool bindMethod(VMCtx *vmCtx, ObjClass *clazz, ObjString *name) {
	VM *vm = &vmCtx->vm;

	Value method;
	if (!tableGet(&clazz->methods, name, &method)) {
		runtimeError(vmCtx, "Undefined property '%s'", name->string.chars);
		return false;
	}

	ObjBoundMethod *bound = newBoundMethod(vmCtx, peek(vm, 0), AS_OBJ(method));
	pop(vm);
	push(vm, OBJ_VAL(bound));
	return true;
}

static ObjUpvalue *captureUpvalue(VMCtx *vmCtx, Value *local) {
	VM *vm = &vmCtx->vm;
	ObjUpvalue *prevUpvalue = NULL;
	ObjUpvalue *upvalue = vm->openUpvalues;
	while (upvalue != NULL && upvalue->location > local) {
		prevUpvalue = upvalue;
		upvalue = upvalue->next;
	}

	if (upvalue != NULL && upvalue->location == local)
		return upvalue;

	ObjUpvalue *createdUpvalue = newUpvalue(vmCtx, local);
	createdUpvalue->next = upvalue;

	if (prevUpvalue == NULL)
		vm->openUpvalues = createdUpvalue;
	else
		prevUpvalue->next = createdUpvalue;

	return createdUpvalue;
}

static void closeUpvalues(VM *vm, Value *last) {
	while (vm->openUpvalues != NULL && vm->openUpvalues->location >= last) {
		ObjUpvalue *upvalue = vm->openUpvalues;
		upvalue->closed = *upvalue->location;
		upvalue->location = &upvalue->closed;
		vm->openUpvalues = upvalue->next;
	}
}

static bool isCallable(Value val) {
	if (!IS_OBJ(val))
		return false;
	switch (OBJ_TYPE(val)) {
		case OBJ_BOUND_METHOD:
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

static void concatenate(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	ObjString *b = AS_STRING(peek(vm, 0));
	ObjString *a = AS_STRING(peek(vm, 1));

	int length = a->string.length + b->string.length;
	char *chars = ALLOCATE(vmCtx, char, length + 1);
	memcpy(chars, a->string.chars, a->string.length);
	memcpy(chars + a->string.length, b->string.chars, b->string.length);
	chars[length] = '\0';

	ObjString *result = takeString(vmCtx, chars, length, length + 1);
	popn(vm, 2);
	push(vm, OBJ_VAL(result));
}

Value toString(ExecContext *execCtx, Value value) {
	VMCtx *vmCtx = execCtx->vmCtx;
	VM *vm = &vmCtx->vm;

	ObjInstance *instance = NULL;
	ObjClass *clazz = classOfValue(vm, value, &instance);
	if (ELOX_UNLIKELY(clazz == NULL)) {
		execCtx->error = true;
		return runtimeError(vmCtx, "No string representation available");
	}
	Value method;
	if (ELOX_UNLIKELY(!tableGet(&clazz->methods, vm->toStringString, &method))) {
		execCtx->error = true;
		return runtimeError(vmCtx, "No string representation available");
	}
	ObjBoundMethod *boundToString = newBoundMethod(vmCtx, value, AS_OBJ(method));
	push(vm, OBJ_VAL(boundToString));
	Value strVal = doCall(vmCtx, 0);
	if (ELOX_LIKELY(!IS_EXCEPTION(strVal))) {
		pop(vm);
		return strVal;
	}
	execCtx->error = true;
	return strVal;
}

static Value *getClassMemberRef(RefData *refData, ObjInstance *instance ELOX_UNUSED) {
	return refData->value;
}

static Value *getClassInstFieldRef(RefData *refData, ObjInstance *instance) {
	intptr_t propIndex = refData->offset;
	return &instance->fields.values[propIndex];
}

static char *loadFile(VMCtx *vmCtx, const char *path) {
	FILE *file = fopen(path, "rb");
	if (file == NULL) {
		runtimeError(vmCtx, "Could not open file '%s'", path);
		return NULL;
	}

	fseek(file, 0L, SEEK_END);
	size_t fileSize = ftell(file);
	rewind(file);

	char *buffer = ALLOCATE(vmCtx, char, fileSize + 1);
	size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
	if (bytesRead < fileSize) {
		FREE(vmCtx, char, buffer);
		fclose(file);
		runtimeError(vmCtx, "Could not read file '%s'", path);
		return NULL;
	}

	buffer[bytesRead] = '\0';

	fclose(file);
	return buffer;
}

#define MAX_PATH 4096

static bool import(VMCtx *vmCtx, ObjString *moduleName) {
	VM *vm = &vmCtx->vm;

	bool ret = false;
	char *source = NULL;

	if (tableFindString(&vm->modules,
						moduleName->string.chars, moduleName->string.length,
						moduleName->hash) != NULL) {
		// already loaded
		return true;
	}

	char moduleFileName[MAX_PATH];
	snprintf(moduleFileName, MAX_PATH, "tests/%.*s.elox",
			 moduleName->string.length, moduleName->string.chars);

	source = loadFile(vmCtx, moduleFileName);
	if (ELOX_UNLIKELY(source == NULL))
		goto cleanup;

	ObjFunction *function = compile(vmCtx, source, &moduleName->string);
	if (function == NULL) {
		runtimeError(vmCtx, "Could not compile module '%s'", moduleName->string.chars);
		goto cleanup;
	}

	push(vm, OBJ_VAL(function));
	Value moduleRet = doCall(vmCtx, 0);
	if (ELOX_UNLIKELY(IS_EXCEPTION(moduleRet))) {
		pop(vm);
		push(vm, moduleRet);
		goto cleanup;
	}
	pop(vm);
	ret = true;

cleanup:
	if (source != NULL)
		FREE(vmCtx, char, source);

	return ret;
}

#ifdef ELOX_DEBUG_TRACE_EXECUTION
void printStack(VM *vm) {
	printf("          ");
	CallFrame *frame = (vm->frameCount > 0) ? &vm->frames[vm->frameCount - 1] : NULL;
	for (Value *slot = vm->stack; slot < vm->stackTop; slot++) {
		if (frame && (slot == frame->slots))
			printf("|");
		printf("[ ");
		printValue(*slot);
		printf(" ]");
	}
	printf("\n");
}
#endif

Value doCall(VMCtx *vmCtx, int argCount) {
	VM *vm = &vmCtx->vm;

	int exitFrame = vm->frameCount;
	Value callable = peek(vm, argCount);
#ifdef ELOX_DEBUG_TRACE_EXECUTION
	printf("--->");
	printValue(callable);
	printStack(vm);
#endif
	bool wasNative = false;
	bool ret = callValue(vmCtx, callable, argCount, &wasNative);
	if (!ret) {
#ifdef ELOX_DEBUG_TRACE_EXECUTION
		printf("<---");
		printValue(callable);
		printStack(vm);
#endif
		return EXCEPTION_VAL;
	}
	if (wasNative) {
		// Native function already returned
#ifdef ELOX_DEBUG_TRACE_EXECUTION
		printf("<---");
		printValue(callable);
		printStack(vm);
#endif
		return peek(vm, 0);
	}
	EloxInterpretResult res = run(vmCtx, exitFrame);
#ifdef ELOX_DEBUG_TRACE_EXECUTION
	printf("<---");
	printValue(callable);
	printStack(vm);
#endif
	if (res == ELOX_INTERPRET_RUNTIME_ERROR)
		return EXCEPTION_VAL;
	return peek(vm, 0);
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

EloxInterpretResult run(VMCtx *vmCtx, int exitFrame) {
	VM *vm = &vmCtx->vm;
	CallFrame *frame = &vm->frames[vm->frameCount - 1];
	register uint8_t *ip = frame->ip;

#ifdef ELOX_ENABLE_COMPUTED_GOTO
	static void *dispatchTable[] = {
		#define OPCODE(name) &&opcode_##name,
		#define ELOX_OPCODES_INLINE
		#include "elox/opcodes.h"
		#undef ELOX_OPCODES_INLINE
		#undef OPCODE
	};
#endif // ELOX_ENABLE_COMPUTED_GOTO

#define READ_BYTE() (*ip++)
#define READ_USHORT() \
	(ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))
#define READ_CONST8() \
	(getFrameFunction(frame)->chunk.constants.values[READ_BYTE()])
#define READ_CONST16() \
	(getFrameFunction(frame)->chunk.constants.values[READ_USHORT()])
#define READ_STRING16() AS_STRING(READ_CONST16())
#define BINARY_OP(valueType, op) \
	do { \
		if (ELOX_UNLIKELY(!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1)))) { \
			frame->ip = ip; \
			runtimeError(vmCtx, "Operands must be numbers"); \
			goto throwException; \
		} \
		double b = AS_NUMBER(pop(vm)); \
		double a = AS_NUMBER(pop(vm)); \
		push(vm, valueType(a op b)); \
	} while (false)

	for (;;) {
#ifdef ELOX_ENABLE_COMPUTED_GOTO
dispatchLoop: ;
#endif

#ifdef ELOX_DEBUG_TRACE_EXECUTION
		printStack(vm);

		disassembleInstruction(&getFrameFunction(frame)->chunk,
							   (int)(ip - getFrameFunction(frame)->chunk.code));
#endif
		uint8_t instruction = READ_BYTE();
		DISPATCH_START(instruction)
			DISPATCH_CASE(CONST8): {
				Value constant = READ_CONST8();
				push(vm, constant);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CONST16): {
				Value constant = READ_CONST16();
				push(vm, constant);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(IMM8): {
				push(vm, NUMBER_VAL(READ_BYTE()));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(IMM16): {
				push(vm, NUMBER_VAL(READ_USHORT()));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(NIL):
				push(vm, NIL_VAL);
				DISPATCH_BREAK;
			DISPATCH_CASE(TRUE):
				push(vm, BOOL_VAL(true));
				DISPATCH_BREAK;
			DISPATCH_CASE(FALSE):
				push(vm, BOOL_VAL(false));
				DISPATCH_BREAK;
			DISPATCH_CASE(POP):
				pop(vm);
				DISPATCH_BREAK;
			DISPATCH_CASE(POPN): {
				uint8_t n = READ_BYTE();
				popn(vm, n);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(NUM_VARARGS): {
				push(vm, NUMBER_VAL(frame->varArgs));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_LOCAL): {
				uint8_t slot = READ_BYTE();
				uint8_t postArgs = READ_BYTE();
				push(vm, frame->slots[slot + (postArgs * frame->varArgs)]);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_VARARG): {
				Value indexVal = pop(vm);
				if (ELOX_UNLIKELY(!IS_NUMBER(indexVal))) {
					frame->ip = ip;
					runtimeError(vmCtx, "Arg index is not a number");
					goto throwException;
				}
				int index = AS_NUMBER(indexVal);
				push(vm, frame->slots[frame->fixedArgs + index + 1]);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_GLOBAL): {
				Value value = vm->globalValues.values[READ_USHORT()];
				if (ELOX_UNLIKELY(IS_UNDEFINED(value))) {
					frame->ip = ip;
					runtimeError(vmCtx, "Undefined global variable");
					goto throwException;
				}
				push(vm, value);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(DEFINE_GLOBAL): {
				vm->globalValues.values[READ_USHORT()] = pop(vm);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_LOCAL): {
				uint8_t slot = READ_BYTE();
				uint8_t postArgs = READ_BYTE();
				frame->slots[slot + (postArgs * frame->varArgs)] = peek(vm, 0);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_VARARG): {
				Value indexVal = peek(vm, 1);
				if (ELOX_UNLIKELY(!IS_NUMBER(indexVal))) {
					frame->ip = ip;
					runtimeError(vmCtx, "Arg index is not a number");
					goto throwException;
				}
				Value val = pop(vm);
				int index = AS_NUMBER(indexVal);
				frame->slots[frame->fixedArgs + index + 1] = val;
				pop(vm);
				push(vm, val);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_GLOBAL): {
				uint16_t index = READ_USHORT();
				if (ELOX_UNLIKELY(IS_UNDEFINED(vm->globalValues.values[index]))) {
					frame->ip = ip;
					runtimeError(vmCtx, "Undefined global variable");
					goto throwException;
				}
				vm->globalValues.values[index] = peek(vm, 0);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_UPVALUE): {
				uint8_t slot = READ_BYTE();
				push(vm, *getFrameClosure(frame)->upvalues[slot]->location);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_UPVALUE): {
				uint8_t slot = READ_BYTE();
				*getFrameClosure(frame)->upvalues[slot]->location = peek(vm, 0);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_PROPERTY): {
				Value instanceVal = peek(vm, 0);

				bool methodsOnly = READ_BYTE();
				ObjString *name = READ_STRING16();

				if (IS_INSTANCE(instanceVal)) {
					ObjInstance *instance = AS_INSTANCE(instanceVal);

					if (!methodsOnly) {
						Value value;
						if (getInstanceValue(instance, name, &value)) {
							pop(vm); // Instance
							push(vm, value);
							DISPATCH_BREAK;
						}
					}

					frame->ip = ip;
					if (ELOX_UNLIKELY(!bindMethod(vmCtx, instance->clazz, name)))
						goto throwException;
				} else if (IS_MAP(instanceVal)) {
					ObjMap *map = AS_MAP(instanceVal);

					if (methodsOnly) {
						frame->ip = ip;
						if (ELOX_UNLIKELY(!bindMethod(vmCtx, vm->mapClass, name)))
							goto throwException;
					} else {
						Value value;
						frame->ip = ip;
						ExecContext execCtx = EXEC_CTX_INITIALIZER(vmCtx);
						bool found = valueTableGet(&execCtx, &map->items, OBJ_VAL(name), &value);
						if (ELOX_UNLIKELY(execCtx.error))
							goto throwException;
						if (!found)
							value = NIL_VAL;
						pop(vm); // map
						push(vm, value);
					}
				} else {
					ObjInstance *instance;
					ObjClass *clazz = classOfValue(vm, instanceVal, &instance);
					if (ELOX_LIKELY(clazz != NULL)) {
						frame->ip = ip;
						if (ELOX_UNLIKELY(!bindMethod(vmCtx, clazz, name)))
							goto throwException;
					} else {
						frame->ip = ip;
						runtimeError(vmCtx, "This value doesn't have properties");
						goto throwException;
					}
				}

				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_MEMBER_PROPERTY): {
				uint16_t propRef = READ_USHORT();
				ObjInstance *instance = AS_INSTANCE(peek(vm, 0));
				ObjClass *parentClass = getFrameFunction(frame)->parentClass;
				MemberRef *ref = &parentClass->memberRefs[propRef];
				Value *prop = ref->getMemberRef(&ref->refData, instance);
				pop(vm); // Instance
				push(vm, *prop);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_PROPERTY): {
				Value instanceVal = peek(vm, 1);

				if (IS_INSTANCE(instanceVal)) {
					ObjInstance *instance = AS_INSTANCE(instanceVal);
					ObjString *fieldName = READ_STRING16();
					if (ELOX_UNLIKELY(!setInstanceField(instance, fieldName, peek(vm, 0)))) {
						frame->ip = ip;
						runtimeError(vmCtx, "Undefined field '%s'", fieldName->string.chars);
						goto throwException;
					}
					Value value = pop(vm);
					pop(vm);
					push(vm, value);
				} else if (IS_MAP(instanceVal)) {
					ObjMap *map = AS_MAP(instanceVal);
					ObjString *index = READ_STRING16();
					Value value = peek(vm, 0);
					frame->ip = ip;
					ExecContext execCtx = EXEC_CTX_INITIALIZER(vmCtx);
					valueTableSet(&execCtx, &map->items, OBJ_VAL(index), value);
					if (ELOX_UNLIKELY(execCtx.error))
						goto throwException;
					value = pop(vm);
					pop(vm);
					push(vm, value);
				} else {
					frame->ip = ip;
					runtimeError(vmCtx, "Only instances have fields");
					goto throwException;
				}

				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_MEMBER_PROPERTY): {
				uint16_t propRef = READ_USHORT();
				ObjInstance *instance = AS_INSTANCE(peek(vm, 1));
				ObjClass *parentClass = getFrameFunction(frame)->parentClass;
				MemberRef *ref = &parentClass->memberRefs[propRef];
				Value *prop = ref->getMemberRef(&ref->refData, instance);
				*prop = peek(vm, 0);
				Value value = pop(vm);
				pop(vm);
				push(vm, value);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_SUPER): {
				uint16_t propRef = READ_USHORT();
				ObjInstance *instance = AS_INSTANCE(peek(vm, 0));
				MemberRef *ref = &instance->clazz->memberRefs[propRef];
				Value method = *ref->getMemberRef(&ref->refData, instance);
				ObjBoundMethod *bound = newBoundMethod(vmCtx, peek(vm, 0), AS_OBJ(method));
				pop(vm);
				push(vm, OBJ_VAL(bound));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(EQUAL): {
				Value b = pop(vm);
				Value a = pop(vm);
				push(vm, BOOL_VAL(valuesEqual(a, b)));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GREATER):
				BINARY_OP(BOOL_VAL, >);
				DISPATCH_BREAK;
			DISPATCH_CASE(LESS):
				BINARY_OP(BOOL_VAL, <);
				DISPATCH_BREAK;
			DISPATCH_CASE(ADD): {
				if (IS_STRING(peek(vm, 0)) && IS_STRING(peek(vm, 1))) {
					concatenate(vmCtx);
				} else if (IS_NUMBER(peek(vm, 0)) && IS_NUMBER(peek(vm, 1))) {
					double b = AS_NUMBER(pop(vm));
					double a = AS_NUMBER(pop(vm));
					push(vm, NUMBER_VAL(a + b));
				} else {
					frame->ip = ip;
					runtimeError(vmCtx, "Operands must be two numbers or two strings");
					goto throwException;
				}
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
				if (!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1))) {
					frame->ip = ip;
					runtimeError(vmCtx, "Operands must be numbers");
					goto throwException;
				}
				double b = AS_NUMBER(pop(vm));
				double a = AS_NUMBER(pop(vm));
				push(vm, NUMBER_VAL(fmod(a, b)));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INSTANCEOF): {
				if (ELOX_UNLIKELY(!IS_CLASS(peek(vm, 0)))) {
					frame->ip = ip;
					runtimeError(vmCtx, "Righthand operand must be a class");
					goto throwException;
				}
				ObjClass *clazz = AS_CLASS(pop(vm));
				ObjInstance *valueInstance  = NULL;
				ObjClass *instClass = classOfValue(vm, pop(vm), &valueInstance);
				if (instClass != NULL)
					push(vm, BOOL_VAL(instanceOf(clazz, instClass)));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(NOT):
				push(vm, BOOL_VAL(isFalsey(pop(vm))));
				DISPATCH_BREAK;
			DISPATCH_CASE(NEGATE):
				if (ELOX_UNLIKELY(!IS_NUMBER(peek(vm, 0)))) {
					frame->ip = ip;
					runtimeError(vmCtx, "Operand must be a number");
					goto throwException;
				}
				push(vm, NUMBER_VAL(-AS_NUMBER(pop(vm))));
				DISPATCH_BREAK;
			DISPATCH_CASE(JUMP): {
				uint16_t offset = READ_USHORT();
				ip += offset;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(JUMP_IF_FALSE): {
				uint16_t offset = READ_USHORT();
				if (isFalsey(peek(vm, 0)))
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
				frame->ip = ip;
				bool wasNative;
				if (ELOX_UNLIKELY(!callValue(vmCtx, peek(vm, argCount), argCount, &wasNative)))
					goto throwException;
				frame = &vm->frames[vm->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INVOKE): {
				ObjString *method = READ_STRING16();
				int argCount = READ_BYTE();
				frame->ip = ip;
				if (ELOX_UNLIKELY(!invoke(vmCtx, method, argCount)))
					goto throwException;
				frame = &vm->frames[vm->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(MEMBER_INVOKE): {
				uint16_t propRef = READ_USHORT();
				int argCount = READ_BYTE();
				ObjInstance *instance = AS_INSTANCE(peek(vm, argCount));
				MemberRef *ref = &instance->clazz->memberRefs[propRef];
				Value *method = ref->getMemberRef(&ref->refData, instance);
				bool isMember = (ref->getMemberRef == getClassMemberRef);
				frame->ip = ip;
				if (ELOX_UNLIKELY(!invokeMember(vmCtx, method, isMember, argCount)))
					goto throwException;
				frame = &vm->frames[vm->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SUPER_INVOKE): {
				uint16_t propRef = READ_USHORT();
				int argCount = READ_BYTE();
				ObjInstance *instance = AS_INSTANCE(peek(vm, argCount));
				MemberRef *ref = &instance->clazz->memberRefs[propRef];
				Value *method = ref->getMemberRef(&ref->refData, NULL);
				frame->ip = ip;
				if (ELOX_UNLIKELY(!invokeMember(vmCtx, method, true, argCount)))
					goto throwException;
				frame = &vm->frames[vm->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SUPER_INIT): {
				int argCount = READ_BYTE();
				//ObjInstance *instance = AS_INSTANCE(frame->slots[0]);
				ObjClass *superclass = AS_CLASS(pop(vm));
				/*ObjInstance *instance = AS_INSTANCE(peek(vm, argCount));
				ObjClass *superclass = AS_CLASS(instance->clazz->super);*/
				Value init = superclass->initializer;
				if (!IS_NIL(init)) {
					frame->ip = ip;
					//vm->stackTop[-argCount - 1] = OBJ_VAL(instance); //!!!
#ifdef ELOX_DEBUG_TRACE_EXECUTION
				printf("--->%s init\n", superclass->name->string.chars);
#endif
					bool wasNative;
					if (!callMethod(vmCtx, AS_OBJ(init), argCount, &wasNative))
						goto throwException;
					frame = &vm->frames[vm->frameCount - 1];
					ip = frame->ip;
				} else {
					// no return, discard arguments
					//vm->stackTop = frame->slots;
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CLOSURE): {
				ObjFunction *function = AS_FUNCTION(READ_CONST16());
				ObjClosure *closure = newClosure(vmCtx, function);
				push(vm, OBJ_VAL(closure));
				for (int i = 0; i < closure->upvalueCount; i++) {
					uint8_t isLocal = READ_BYTE();
					uint8_t index = READ_BYTE();
					if (isLocal)
						closure->upvalues[i] = captureUpvalue(vmCtx, frame->slots + index);
					else
						closure->upvalues[i] = getFrameClosure(frame)->upvalues[index];
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CLOSE_UPVALUE):
				closeUpvalues(vm, vm->stackTop - 1);
				pop(vm);
				DISPATCH_BREAK;
			DISPATCH_CASE(RETURN): {
				//Value result = pop(vm);
				Value result = peek(vm, 0);
				closeUpvalues(vm, frame->slots);
				vm->frameCount--;
				/*if (vm->frameCount == 0) {
					pop(vm);
					return INTERPRET_OK;
				} else if (vm->frameCount == exitFrame) {
					push(vm, result);
					return INTERPRET_OK;
				}*/

				//if (vm->frameCount == exitFrame)
					//return ELOX_INTERPRET_OK;

				vm->stackTop = frame->slots;
				push(vm, result);
				if (vm->frameCount == exitFrame)
					return ELOX_INTERPRET_OK;
				frame = &vm->frames[vm->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CLASS):
				push(vm, OBJ_VAL(newClass(vmCtx, READ_STRING16())));
				DISPATCH_BREAK;
			DISPATCH_CASE(INHERIT): {
				Value superclassVal = peek(vm, 1);
				if (ELOX_UNLIKELY(!IS_CLASS(superclassVal))) {
					frame->ip = ip;
					runtimeError(vmCtx, "Superclass must be a class");
					goto throwException;
				}
				ObjClass *subclass = AS_CLASS(peek(vm, 0));
				ObjClass *superclass = AS_CLASS(superclassVal);
				subclass->classId = subclass->baseId * superclass->classId;
				for (int i = 0; i < superclass->fields.capacity; i++) {
					Entry *entry = &superclass->fields.entries[i];
					if (entry->key != NULL) {
						if (ELOX_UNLIKELY(!tableSet(vmCtx, &subclass->fields, entry->key, entry->value))) {
							frame->ip = ip;
							runtimeError(vmCtx, "Field '%s' shadows field from superclass",
										 entry->key->string.chars);
							goto throwException;
						}
					}
				}
				tableAddAll(vmCtx, &superclass->methods, &subclass->methods);
				subclass->super = superclassVal;
				pop(vm); // Subclass
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(METHOD):
				defineMethod(vmCtx, READ_STRING16());
				DISPATCH_BREAK;
			DISPATCH_CASE(FIELD):
				defineField(vmCtx, READ_STRING16());
				DISPATCH_BREAK;
			DISPATCH_CASE(RESOLVE_MEMBERS): {
				uint8_t numSlots = READ_BYTE();
				ObjClass *clazz = AS_CLASS(peek(vm, 0));
				clazz->memberRefs = ALLOCATE(vmCtx, MemberRef, numSlots);
				clazz->memberRefCount = numSlots;
				for (int i = 0; i < numSlots; i++) {
					uint8_t slotType = READ_BYTE();
					bool super = slotType & 0x1;
					uint8_t propType = (slotType & 0x6) >> 1;
					ObjString *propName = READ_STRING16();
					uint16_t slot = READ_USHORT();

					if (super) {
						ObjClass *superClass = AS_CLASS(clazz->super);
						int propIndex = tableGetIndex(&superClass->methods, propName);
						if (ELOX_UNLIKELY(propIndex < 0)) {
							frame->ip = ip;
							runtimeError(vmCtx, "Undefined property '%s'", propName->string.chars);
							goto throwException;
						}
						clazz->memberRefs[slot] = (MemberRef) {
							.getMemberRef = getClassMemberRef,
							.refData.value = &superClass->methods.entries[propIndex].value
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
							frame->ip = ip;
							runtimeError(vmCtx, "Undefined property '%s'", propName->string.chars);
							goto throwException;
						}
						if (isField) {
							clazz->memberRefs[slot] = (MemberRef) {
								.getMemberRef = getClassInstFieldRef,
								.refData.offset = propIndex
							};
						} else {
							clazz->memberRefs[slot] = (MemberRef) {
								.getMemberRef = getClassMemberRef,
								.refData.value = &clazz->methods.entries[propIndex].value
							};
						}
					}
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(ARRAY_BUILD): {
				ObjType objType = READ_BYTE();
				uint16_t itemCount = READ_USHORT();
				ObjArray *array = newArray(vmCtx, itemCount, objType);

				push(vm, OBJ_VAL(array));
				for (int i = itemCount; i > 0; i--)
					appendToArray(vmCtx, array, peek(vm, i));
				pop(vm);

				popn(vm, itemCount);

				push(vm, OBJ_VAL(array));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INDEX): {
				Value indexVal = pop(vm);
				Value indexableVal = pop(vm);
				Value result;

				if (IS_ARRAY(indexableVal)) {
					ObjArray *array = AS_ARRAY(indexableVal);

					if (ELOX_UNLIKELY(!IS_NUMBER(indexVal))) {
						frame->ip = ip;
						runtimeError(vmCtx, "Array index is not a number");
						goto throwException;
					}
					int index = AS_NUMBER(indexVal);

					if (ELOX_UNLIKELY(!isValidArrayIndex(array, index))) {
						frame->ip = ip;
						runtimeError(vmCtx, "Array index out of range");
						goto throwException;
					}

					result = arrayAt(array, index);
				} else if (IS_MAP(indexableVal)) {
					ObjMap *map = AS_MAP(indexableVal);
					frame->ip = ip;
					ExecContext execCtx = EXEC_CTX_INITIALIZER(vmCtx);
					bool found = valueTableGet(&execCtx, &map->items, indexVal, &result);
					if (ELOX_UNLIKELY(execCtx.error))
						goto throwException;
					if (!found)
						result = NIL_VAL;
				} else {
					frame->ip = ip;
					runtimeError(vmCtx, "Invalid type to index into");
					goto throwException;
				}

				push(vm, result);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INDEX_STORE): {
				Value item = peek(vm, 0);
				Value indexVal = peek(vm, 1);
				Value indexableVal = peek(vm, 2);

				if (IS_ARRAY(indexableVal)) {
					ObjArray *array = AS_ARRAY(indexableVal);

					if (!IS_NUMBER(indexVal)) {
						frame->ip = ip;
						runtimeError(vmCtx, "Array index is not a number");
						goto throwException;
					}
					int index = AS_NUMBER(indexVal);

					if (ELOX_UNLIKELY(!isValidArrayIndex(array, index))) {
						frame->ip = ip;
						runtimeError(vmCtx, "Array index out of range");
						goto throwException;
					}

					arraySet(array, index, item);
				} else if (IS_MAP(indexableVal)) {
					ObjMap *map = AS_MAP(indexableVal);

					frame->ip = ip;
					ExecContext execCtx = EXEC_CTX_INITIALIZER(vmCtx);
					valueTableSet(&execCtx, &map->items, indexVal, item);
					if (ELOX_UNLIKELY(execCtx.error))
						goto throwException;
				} else {
					frame->ip = ip;
					runtimeError(vmCtx, "Destination is not an array or map");
					goto throwException;
				}

				popn(vm, 3);
				push(vm, item);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(MAP_BUILD): {
				ObjMap *map = newMap(vmCtx);
				uint16_t itemCount = READ_USHORT();

				push(vm, OBJ_VAL(map));
				int i = 2 * itemCount;
				frame->ip = ip;
				ExecContext execCtx = EXEC_CTX_INITIALIZER(vmCtx);
				while (i > 0) {
					Value key = peek(vm, i--);
					Value value = peek(vm, i--);

					valueTableSet(&execCtx, &map->items, key, value);
					if (ELOX_UNLIKELY(execCtx.error))
						goto throwException;
				}
				pop(vm);

				// pop constructor items from the stack
				popn(vm, 2 * itemCount);

				push(vm, OBJ_VAL(map));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(THROW): {
throwException:
				frame->ip = ip;
				Value stacktrace = getStackTrace(vmCtx);

				DBG_PRINT_STACK("EXC", &vmCtx->vm);

				ObjInstance *instance = AS_INSTANCE(peek(vm, 0));
				push(vm, stacktrace);
				ObjString *stacktraceName = copyString(vmCtx, ELOX_STR_AND_LEN("stacktrace"));
				push(vm, OBJ_VAL(stacktraceName));
				setInstanceField(instance, stacktraceName, stacktrace);
				popn(vm, 2);
				vm->handlingException++;
				if (propagateException(vmCtx, exitFrame)) {
					vm->handlingException--;
					frame = &vm->frames[vm->frameCount - 1];
					ip = frame->ip;
					DISPATCH_BREAK;
				}
				vm->handlingException--;

				// unroll call stack
				vm->frameCount = exitFrame;
				CallFrame *retFrame = &vm->frames[vm->frameCount];
				vm->stackTop = retFrame->slots + 1;
				// set exception as result
				push(vm, OBJ_VAL(instance));

				return ELOX_INTERPRET_RUNTIME_ERROR;
			}
			DISPATCH_CASE(PUSH_EXCEPTION_HANDLER): {
				uint8_t stackLevel = READ_BYTE();
				uint16_t handlerTableAddress = READ_USHORT();
				frame->ip = ip;
				if (!pushExceptionHandler(vmCtx, stackLevel, handlerTableAddress))
					goto throwException;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(POP_EXCEPTION_HANDLER): {
				uint8_t newHandlerCount = READ_BYTE();
				frame->handlerCount = newHandlerCount;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(FOREACH_INIT): {
				uint8_t iterSlot = READ_BYTE();
				bool iterPostArgs = READ_BYTE();
				uint8_t stateSlot = READ_BYTE();
				bool statePostArgs = READ_BYTE();
				uint8_t varSlot = READ_BYTE();
				bool varPostArgs = READ_BYTE();
				Value iterableVal = peek(vm, 0);
				if (!isCallable(iterableVal)) {
					if (IS_TUPLE(iterableVal)) {
						ObjArray *tuple = AS_TUPLE(iterableVal);

						Value iterator = arrayAtSafe(tuple, 0);
						if (!isCallable(iterator)) {
							frame->ip = ip;
							runtimeError(vmCtx, "Attempt to iterate non-iterable value");
							goto throwException;
						}

						frame->slots[iterSlot] = iterator;
						frame->slots[stateSlot] = arrayAtSafe(tuple, 1);
						frame->slots[varSlot] = arrayAtSafe(tuple, 2);

						pop(vm);
						push(vm, BOOL_VAL(false));
					} else {
						bool hasIterator = false;
						ObjInstance *instance;
						ObjClass *clazz = classOfValue(vm, iterableVal, &instance);
						if (clazz != NULL) {
							if (bindMethod(vmCtx, clazz, vm->iteratorString))
								hasIterator = true;
						}
						if (!hasIterator) {
							frame->ip = ip;
							runtimeError(vmCtx, "Attempt to iterate non-iterable value");
							goto throwException;
						}
					}
				} else {
					frame->slots[iterSlot] = pop(vm);
					// no state and control variable
					frame->slots[stateSlot] = NIL_VAL;
					frame->slots[varSlot] = NIL_VAL;
					push(vm, BOOL_VAL(false));
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(UNPACK): {
				uint8_t numVars = READ_BYTE();
				Value val = peek(vm, 0);
				int numItems = 1;
				int tIndex = 0;
				ObjArray *tuple = NULL;
				if (IS_TUPLE(val)) {
					tuple = AS_TUPLE(val);
					numItems = tuple->size;
				}
				for (int i = 0; i < numVars; i++) {
					Value crtVal;
					if (i < numItems) {
						if (tuple == NULL)
							crtVal = val;
						else {
							crtVal = (tIndex < tuple->size ? arrayAt(tuple, tIndex) : NIL_VAL);
							tIndex++;
						}
					} else
						crtVal = NIL_VAL;

					VarType varType = READ_BYTE();
					switch (varType) {
						case VAR_LOCAL: {
							uint8_t slot = READ_BYTE();
							frame->slots[slot] = crtVal;
							break;
						}
						case VAR_UPVALUE: {
							uint8_t slot = READ_BYTE();
							*getFrameClosure(frame)->upvalues[slot]->location = crtVal;
							break;
						}
						case VAR_GLOBAL: {
							uint16_t globalIdx = READ_USHORT();
							vm->globalValues.values[globalIdx] = crtVal;
							break;
						}
					}
				}
				pop(vm);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(IMPORT): {
				ObjString *moduleName = READ_STRING16();
				frame->ip = ip;
				if (ELOX_UNLIKELY(!import(vmCtx, moduleName)))
					goto throwException;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(DATA): {
				frame->ip = ip;
				runtimeError(vmCtx, "Attempted to execute data section");
				goto throwException;
			}
		DISPATCH_END
	}

#undef READ_BYTE
#undef READ_USHORT
#undef READ_CONST16
#undef READ_CONST8
#undef READ_STRING16
#undef BINARY_OP
}

void pushCompilerState(VMCtx *vmCtx, CompilerState *compilerState) {
	VM *vm = &vmCtx->vm;

	if (vm->compilerCapacity < vm->compilerCount + 1) {
		vm->compilerCapacity = GROW_CAPACITY(vm->compilerCapacity);
		vm->compilerStack =
			(CompilerState **)vmCtx->realloc(vm->compilerStack,
											 sizeof(CompilerState *) * vm->compilerCapacity,
											 vmCtx->allocatorUserdata);
		if (vm->compilerStack == NULL)
			exit(1);
	}

	vm->compilerStack[vm->compilerCount++] = compilerState;
}

void popCompilerState(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	vm->compilerCount--;
}

EloxInterpretResult interpret(VMCtx *vmCtx, char *source, const String *moduleName) {
	VM *vm = &vmCtx->vm;


	ObjFunction *function = compile(vmCtx, source, moduleName);
	if (function == NULL)
		return ELOX_INTERPRET_COMPILE_ERROR;

	push(vm, OBJ_VAL(function));
	callFunction(vmCtx, function, 0);

	DBG_PRINT_STACK("DBGa", vm);

	EloxInterpretResult res = run(vmCtx, 0);
	DBG_PRINT_STACK("DBGb1", vm);
	popn(vm, 1);

	DBG_PRINT_STACK("DBGb", vm);

	return res;
}
