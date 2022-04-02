#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <math.h>

#include "slox/common.h"
#include "slox/compiler.h"
#include "slox/debug.h"
#include "slox/object.h"
#include "slox/memory.h"
#include "slox/state.h"
#include "slox/builtins.h"

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

static inline ObjFunction *getValueFunction(Value *value) {
	assert(IS_OBJ(*value));
	Obj *objVal = AS_OBJ(*value);
	switch (objVal->type) {
		case OBJ_FUNCTION:
			return AS_FUNCTION(*value);
		case OBJ_CLOSURE:
			return AS_CLOSURE(*value)->function;
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

	if (argCount != function->arity) {
		if (argCount < function->arity) {
			int missingArgs = function->arity - argCount;
			for (int i = 0; i < missingArgs; i++)
				push(vm, NIL_VAL);
		} else {
			int extraArgs = argCount - function->arity;
			popn(vm, extraArgs);
		}
	}

	if (SLOX_UNLIKELY(vm->frameCount == FRAMES_MAX)) {
		runtimeError(vmCtx, "Stack overflow");
		return false;
	}

	CallFrame *frame = &vm->frames[vm->frameCount++];
	frame->function = (Obj *)callee;
	frame->ip = function->chunk.code;
	frame->handlerCount = 0;

	frame->slots = vm->stackTop - function->arity - 1;
	return true;
}

static bool callClosure(VMCtx *vmCtx, ObjClosure *closure, int argCount) {
	return call(vmCtx, (Obj *)closure, closure->function, argCount);
}

static bool callNativeClosure(VMCtx *vmCtx, ObjNativeClosure *closure, int argCount, bool method) {
	VM *vm = &vmCtx->vm;

	NativeClosureFn native = closure->nativeFunction;
	// for native methods include 'this'
	Value result = native(vmCtx,
						  argCount + (int)method, vm->stackTop - argCount - (int)method,
						  closure->upvalueCount, closure->upvalues);
	if (SLOX_LIKELY(!IS_EXCEPTION(result))) {
		vm->stackTop -= argCount + 1;
		push(vm, result);
		return true;
	}
	return false;
}

static bool callFunction(VMCtx *vmCtx, ObjFunction *function, int argCount) {
	return call(vmCtx, (Obj *)function, function, argCount);
}

static bool callNative(VMCtx *vmCtx, NativeFn native, int argCount, bool method) {
	VM *vm = &vmCtx->vm;

	// for native methods include 'this'
	Value result = native(vmCtx, argCount + (int)method, vm->stackTop - argCount - (int)method);

	if (SLOX_LIKELY(!IS_EXCEPTION(result))) {
		vm->stackTop -= argCount + 1;
		push(vm, result);
		return true;
	}
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

/*static void runtimeError(VMCtx *vmCtx, const char *format, ...) {
	VM *vm = &vmCtx->vm;

	va_list args;
	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	fputs("\n", stderr);

	for (int i = vm->frameCount - 1; i >= 0; i--) {
		CallFrame* frame = &vm->frames[i];
		ObjFunction* function = getFrameFunction(frame);
		size_t instruction = frame->ip - function->chunk.code - 1;
		fprintf(stderr, "[line %d] in ",
				getLine(&function->chunk, instruction));
		if (function->name == NULL) {
			fprintf(stderr, "script\n");
		} else {
			fprintf(stderr, "%s()\n", function->name->chars);
		}
	}
	resetStack(vmCtx);
}*/

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
				function->name == NULL ? "script" : function->name->chars);
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
	addStringVFmt(vmCtx, &msg, format, args);
	va_end(args);

	ObjInstance *errorInst = newInstance(vmCtx, vm->runtimeExceptionClass);
	push(vm, OBJ_VAL(errorInst));
	ObjString *msgObj = takeString(vmCtx, msg.chars, msg.length, msg.capacity);
	push(vm, OBJ_VAL(msgObj));
	bool wasNative;
	callMethod(vmCtx, AS_OBJ(vm->runtimeExceptionClass->initializer), 1, &wasNative);
	popn(vm, 2);
	push(vm, OBJ_VAL(errorInst));

	vm->handlingException--;
	return EXCEPTION_VAL;
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

static Value peek(VM *vm, int distance) {
	return vm->stackTop[-1 - distance];
}

static void defineMethod(VMCtx *vmCtx, ObjString *name) {
	VM *vm = &vmCtx->vm;

	Value method = peek(vm, 0);
	ObjClass *clazz = AS_CLASS(peek(vm, 1));
	ObjFunction *methodFunction = getValueFunction(&method);
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

void defineNative(VMCtx *vmCtx, const char *name, NativeFn function) {
	VM *vm = &vmCtx->vm;
	push(vm, OBJ_VAL(copyString(vmCtx, name, (int)strlen(name))));
	push(vm, OBJ_VAL(newNative(vmCtx, function)));
	tableSet(vmCtx, &vm->globals, AS_STRING(vm->stack[0]), vm->stack[1]);
	popn(vm, 2);
}

void initVM(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	resetStack(vmCtx);
	vm->handlingException = 0;
	stc64_init(&vm->prng, 64);
	vm->objects = NULL;
	vm->bytesAllocated = 0;
	vm->nextGC = 1024 * 1024;

	vm->grayCount = 0;
	vm->grayCapacity = 0;
	vm->grayStack = NULL;

	initTable(&vm->globals);
	initTable(&vm->strings);

	registerBuiltins(vmCtx);
}

void freeVM(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	freeTable(vmCtx, &vm->globals);
	freeTable(vmCtx, &vm->strings);

	clearBuiltins(vm);
	freeObjects(vmCtx);
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
						  function->name == NULL ? "script" : function->name->chars);
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
	}
	return false;
}

// TODO: optimize
static bool instanceof(ObjClass *clazz, ObjInstance *instance) {
	for (ObjClass *c = instance->clazz; c != NULL;
		 c = (IS_NIL(c->super)) ? NULL : AS_CLASS(c->super)) {
		if (c == clazz)
			return true;
	}
	return false;
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
				Value classVal;
				switch (typeVarType) {
					case VAR_LOCAL:
						classVal = frame->slots[typeHandle];
						break;
					case VAR_UPVALUE:
						classVal = *getFrameClosure(frame)->upvalues[typeHandle]->location;
						break;
					case VAR_GLOBAL: {
						ObjString *className = AS_STRING(getFrameFunction(frame)->chunk.constants.values[typeHandle]);
						if (!tableGet(&vm->globals, className, &classVal) || !IS_CLASS(classVal)) {
							runtimeError(vmCtx, "'%s' is not a type to catch", className->chars);
							return false;
						}
						break;
					}

				}
				ObjClass *handlerClass = AS_CLASS(classVal);
				if (instanceof(handlerClass, exception)) {
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

	// Do not print the exception here if we are inside
	// an internal call
	if (exitFrame == 0) {
		fprintf(stderr, "Unhandled exception %s\n", exception->clazz->name->chars);
		Value stacktrace;
		if (getInstanceValue(exception, copyString(vmCtx, STR_AND_LEN("stacktrace")), &stacktrace)) {
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
	if (SLOX_UNLIKELY(!tableGet(&clazz->methods, name, &method))) {
		runtimeError(vmCtx, "Undefined property '%s'", name->chars);
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
	if (SLOX_UNLIKELY(clazz == NULL)) {
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
		runtimeError(vmCtx, "Undefined property '%s'", name->chars);
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

	int length = a->length + b->length;
	char *chars = ALLOCATE(vmCtx, char, length + 1);
	memcpy(chars, a->chars, a->length);
	memcpy(chars + a->length, b->chars, b->length);
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
	if (SLOX_UNLIKELY(clazz == NULL)) {
		execCtx->error = true;
		return runtimeError(vmCtx, "No string representation available");
	}
	Value method;
	if (SLOX_UNLIKELY(!tableGet(&clazz->methods, vm->toStringString, &method))) {
		execCtx->error = true;
		return runtimeError(vmCtx, "No string representation available");
	}
	ObjBoundMethod *boundHashCode = newBoundMethod(vmCtx, value, AS_OBJ(method));
	push(vm, OBJ_VAL(boundHashCode));
	Value strVal = doCall(vmCtx, 0);
	if (SLOX_LIKELY(!IS_EXCEPTION(strVal))) {
		popn(vm, 2);
		return strVal;
	}
	execCtx->error = true;
	return strVal;
}

static Value *getClassMemberRef(RefData *refData, ObjInstance *instance SLOX_UNUSED) {
	return refData->value;
}

static Value *getClassInstFieldRef(RefData *refData, ObjInstance *instance) {
	intptr_t propIndex = refData->offset;
	return &instance->fields.values[propIndex];
}

#ifdef DEBUG_TRACE_EXECUTION
static void printStack(VM *vm) {
	printf("          ");
	CallFrame *frame = &vm->frames[vm->frameCount - 1];
	for (Value *slot = vm->stack; slot < vm->stackTop; slot++) {
		if (slot == frame->slots)
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
#ifdef DEBUG_TRACE_EXECUTION
	printValue(callable);
	printf("--->");
	printStack(vm);
#endif
	bool wasNative = false;
	bool ret = callValue(vmCtx, callable, argCount, &wasNative);
	if (!ret) {
#ifdef DEBUG_TRACE_EXECUTION
		printValue(callable);
		printf("<---");
		printStack(vm);
#endif
		return EXCEPTION_VAL;
	}
	if (wasNative) {
		// Native function already returned
#ifdef DEBUG_TRACE_EXECUTION
		printValue(callable);
		printf("<---");
		printStack(vm);
#endif
		return peek(vm, 0);
	}
	InterpretResult res = run(vmCtx, exitFrame);
#ifdef DEBUG_TRACE_EXECUTION
	printValue(callable);
	printf("<---");
	printStack(vm);
#endif
	if (res == INTERPRET_RUNTIME_ERROR)
		return EXCEPTION_VAL;
	return peek(vm, 0);
}

#ifdef ENABLE_COMPUTED_GOTO

#define DISPATCH_START(instruction)      goto *dispatchTable[instruction];
#define DISPATCH_CASE(name)              opcode_##name
#define DISPATCH_BREAK                   goto dispatchLoop
#define DISPATCH_END

#else

#define DISPATCH_START(instruction) switch(instruction) {
#define DISPATCH_CASE(name)         case OP_##name
#define DISPATCH_BREAK              break
#define DISPATCH_END }

#endif // ENABLE_COMPUTED_GOTO

InterpretResult run(VMCtx *vmCtx, int exitFrame) {
	VM *vm = &vmCtx->vm;
	CallFrame *frame = &vm->frames[vm->frameCount - 1];
	register uint8_t *ip = frame->ip;

#ifdef ENABLE_COMPUTED_GOTO
	static void *dispatchTable[] = {
		#define OPCODE(name) &&opcode_##name,
		#define SLOX_OPCODES_INLINE
		#include "slox/opcodes.h"
		#undef SLOX_OPCODES_INLINE
		#undef OPCODE
	};
#endif // ENABLE_COMPUTED_GOTO

#define READ_BYTE() (*ip++)
#define READ_USHORT() \
	(ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))
#define READ_CONSTANT() \
	(getFrameFunction(frame)->chunk.constants.values[READ_USHORT()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op) \
	do { \
		if (SLOX_UNLIKELY(!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1)))) { \
			frame->ip = ip; \
			runtimeError(vmCtx, "Operands must be numbers."); \
			goto throwException; \
		} \
		double b = AS_NUMBER(pop(vm)); \
		double a = AS_NUMBER(pop(vm)); \
		push(vm, valueType(a op b)); \
	} while (false)

	for (;;) {
#ifdef ENABLE_COMPUTED_GOTO
dispatchLoop: ;
#endif

#ifdef DEBUG_TRACE_EXECUTION
		printStack(vm);

		disassembleInstruction(&getFrameFunction(frame)->chunk,
							   (int)(ip - getFrameFunction(frame)->chunk.code));
#endif
		uint8_t instruction = READ_BYTE();
		DISPATCH_START(instruction)
			DISPATCH_CASE(CONSTANT): {
				Value constant = READ_CONSTANT();
				push(vm, constant);
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
			DISPATCH_CASE(GET_LOCAL): {
				uint8_t slot = READ_BYTE();
				push(vm, frame->slots[slot]);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_GLOBAL): {
				ObjString *name = READ_STRING();
				Value value;
				if (SLOX_UNLIKELY(!tableGet(&vm->globals, name, &value))) {
					frame->ip = ip;
					runtimeError(vmCtx, "Undefined variable '%s'", name->chars);
					goto throwException;
				}
				push(vm, value);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(DEFINE_GLOBAL): {
				ObjString *name = READ_STRING();
				tableSet(vmCtx, &vm->globals, name, peek(vm, 0));
				pop(vm);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_LOCAL): {
				uint8_t slot = READ_BYTE();
				frame->slots[slot] = peek(vm, 0);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_GLOBAL): {
				ObjString *name = READ_STRING();
				if (SLOX_UNLIKELY(tableSet(vmCtx, &vm->globals, name, peek(vm, 0)))) {
					tableDelete(&vm->globals, name);
					frame->ip = ip;
					runtimeError(vmCtx, "Undefined variable '%s'", name->chars);
					goto throwException;
				}
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
				ObjString *name = READ_STRING();

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
					if (SLOX_UNLIKELY(!bindMethod(vmCtx, instance->clazz, name)))
						goto throwException;
				} else if (IS_MAP(instanceVal)) {
					ObjMap *map = AS_MAP(instanceVal);

					if (methodsOnly) {
						frame->ip = ip;
						if (SLOX_UNLIKELY(!bindMethod(vmCtx, vm->mapClass, name)))
							goto throwException;
					} else {
						Value value;
						frame->ip = ip;
						ExecContext execCtx = EXEC_CTX_INITIALIZER(vmCtx);
						bool found = valueTableGet(&execCtx, &map->items, OBJ_VAL(name), &value);
						if (SLOX_UNLIKELY(execCtx.error))
							goto throwException;
						if (!found)
							value = NIL_VAL;
						pop(vm); // map
						push(vm, value);
					}
				} else {
					ObjInstance *instance;
					ObjClass *clazz = classOfValue(vm, instanceVal, &instance);
					if (SLOX_LIKELY(clazz != NULL)) {
						frame->ip = ip;
						if (SLOX_UNLIKELY(!bindMethod(vmCtx, clazz, name)))
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
					ObjString *fieldName = READ_STRING();
					if (setInstanceField(instance, fieldName, peek(vm, 0))) {
						frame->ip = ip;
						runtimeError(vmCtx, "Undefined field '%s'", fieldName->chars);
						goto throwException;
					}
					Value value = pop(vm);
					pop(vm);
					push(vm, value);
				} else if (IS_MAP(instanceVal)) {
					ObjMap *map = AS_MAP(instanceVal);
					ObjString *index = READ_STRING();
					Value value = peek(vm, 0);
					frame->ip = ip;
					ExecContext execCtx = EXEC_CTX_INITIALIZER(vmCtx);
					valueTableSet(&execCtx, &map->items, OBJ_VAL(index), value);
					if (SLOX_UNLIKELY(execCtx.error))
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
					runtimeError(vmCtx, "Operands must be numbers.");
					goto throwException;
				}
				double b = AS_NUMBER(pop(vm));
				double a = AS_NUMBER(pop(vm));
				push(vm, NUMBER_VAL(fmod(a, b)));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(NOT):
				push(vm, BOOL_VAL(isFalsey(pop(vm))));
				DISPATCH_BREAK;
			DISPATCH_CASE(NEGATE):
				if (SLOX_UNLIKELY(!IS_NUMBER(peek(vm, 0)))) {
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
				if (SLOX_UNLIKELY(!callValue(vmCtx, peek(vm, argCount), argCount, &wasNative)))
					goto throwException;
				frame = &vm->frames[vm->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INVOKE): {
				ObjString *method = READ_STRING();
				int argCount = READ_BYTE();
				frame->ip = ip;
				if (SLOX_UNLIKELY(!invoke(vmCtx, method, argCount)))
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
				if (SLOX_UNLIKELY(!invokeMember(vmCtx, method, isMember, argCount)))
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
				if (SLOX_UNLIKELY(!invokeMember(vmCtx, method, true, argCount)))
					goto throwException;
				frame = &vm->frames[vm->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SUPER_INIT): {
				int argCount = READ_BYTE();
				ObjClass *superclass = AS_CLASS(pop(vm));
				Value init = superclass->initializer;
				if (!IS_NIL(init)) {
					frame->ip = ip;
					bool wasNative;
					if (!callMethod(vmCtx, AS_OBJ(init), argCount, &wasNative))
						goto throwException;
					frame = &vm->frames[vm->frameCount - 1];
					ip = frame->ip;
				}
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CLOSURE): {
				ObjFunction *function = AS_FUNCTION(READ_CONSTANT());
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
				Value result = pop(vm);
				closeUpvalues(vm, frame->slots);
				vm->frameCount--;
				if (vm->frameCount == 0) {
					pop(vm);
					return INTERPRET_OK;
				} else if (vm->frameCount == exitFrame) {
					push(vm, result);
					return INTERPRET_OK;
				}

				vm->stackTop = frame->slots;
				push(vm, result);
				frame = &vm->frames[vm->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CLASS):
				push(vm, OBJ_VAL(newClass(vmCtx, READ_STRING())));
				DISPATCH_BREAK;
			DISPATCH_CASE(INHERIT): {
				Value superclassVal = peek(vm, 1);
				if (SLOX_UNLIKELY(!IS_CLASS(superclassVal))) {
					frame->ip = ip;
					runtimeError(vmCtx, "Superclass must be a class");
					goto throwException;
				}
				ObjClass *subclass = AS_CLASS(peek(vm, 0));
				ObjClass *superclass = AS_CLASS(superclassVal);
				for (int i = 0; i < superclass->fields.capacity; i++) {
					Entry *entry = &superclass->fields.entries[i];
					if (entry->key != NULL) {
						if (SLOX_UNLIKELY(!tableSet(vmCtx, &subclass->fields, entry->key, entry->value))) {
							frame->ip = ip;
							runtimeError(vmCtx, "Field '%s' shadows field from superclass",
										 entry->key->chars);
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
				defineMethod(vmCtx, READ_STRING());
				DISPATCH_BREAK;
			DISPATCH_CASE(FIELD):
				defineField(vmCtx, READ_STRING());
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
					ObjString *propName = READ_STRING();
					uint16_t slot = READ_USHORT();

					if (super) {
						ObjClass *superClass = AS_CLASS(clazz->super);
						int propIndex = tableGetIndex(&superClass->methods, propName);
						if (SLOX_UNLIKELY(propIndex < 0)) {
							frame->ip = ip;
							runtimeError(vmCtx, "Undefined property '%s'", propName->chars);
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

						if (SLOX_UNLIKELY(propIndex < 0)) {
							frame->ip = ip;
							runtimeError(vmCtx, "Undefined property '%s'", propName->chars);
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

					if (SLOX_UNLIKELY(!IS_NUMBER(indexVal))) {
						frame->ip = ip;
						runtimeError(vmCtx, "Array index is not a number");
						goto throwException;
					}
					int index = AS_NUMBER(indexVal);

					if (SLOX_UNLIKELY(!isValidArrayIndex(array, index))) {
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
					if (SLOX_UNLIKELY(execCtx.error))
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

					if (SLOX_UNLIKELY(!isValidArrayIndex(array, index))) {
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
					if (SLOX_UNLIKELY(execCtx.error))
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
					if (SLOX_UNLIKELY(execCtx.error))
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
				ObjInstance *instance = AS_INSTANCE(peek(vm, 0));
				push(vm, stacktrace);
				ObjString *stacktraceName = copyString(vmCtx, STR_AND_LEN("stacktrace"));
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
				return INTERPRET_RUNTIME_ERROR;
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
				uint8_t stateSlot = READ_BYTE();
				uint8_t varSlot = READ_BYTE();
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
							ObjString *name = READ_STRING();
							if (tableSet(vmCtx, &vm->globals, name, crtVal)) {
								tableDelete(&vm->globals, name);
								frame->ip = ip;
								runtimeError(vmCtx, "Undefined variable '%s'", name->chars);
								goto throwException;
							}
							break;
						}
					}
				}
				pop(vm);
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
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(VMCtx *vmCtx, char *source) {
	VM *vm = &vmCtx->vm;

	ObjFunction *function = compile(vmCtx, source);
	if (function == NULL)
		return INTERPRET_COMPILE_ERROR;

	push(vm, OBJ_VAL(function));
	ObjClosure *closure = newClosure(vmCtx, function);
	pop(vm);
	push(vm, OBJ_VAL(closure));
	callClosure(vmCtx, closure, 0);

	return run(vmCtx, 0);
}
