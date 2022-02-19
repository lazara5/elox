#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include <stdlib.h>

#include "slox/common.h"
#include "slox/compiler.h"
#include "slox/debug.h"
#include "slox/object.h"
#include "slox/memory.h"
#include "slox/vm.h"

//VM vm;

static Value clockNative(VMCtx *vmCtx SLOX_UNUSED,
						 int argCount SLOX_UNUSED, Value *args SLOX_UNUSED) {
	return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static void resetStack(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	vm->stackTop = vm->stack;
	vm->frameCount = 0;
	vm->openUpvalues = NULL;
}

static inline ObjFunction *getFrameFunction(CallFrame *frame) {
	if (frame->function->type == OBJ_FUNCTION) {
		return (ObjFunction *)frame->function;
	} else if (frame->function->type == OBJ_CLOSURE) {
		return ((ObjClosure *)frame->function)->function;
	}
	return NULL;
}

static inline ObjClosure *getFrameClosure(CallFrame *frame) {
	assert(frame->function->type != OBJ_FUNCTION);
	return ((ObjClosure *)frame->function);
}

static void runtimeError(VMCtx *vmCtx, const char *format, ...) {
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
}

void push(VM *vm, Value value) {
	*vm->stackTop = value;
	vm->stackTop++;
}

Value pop(VM *vm) {
	vm->stackTop--;
	return *vm->stackTop;
}

static Value peek(VM *vm, int distance) {
	return vm->stackTop[-1 - distance];
}

static void defineMethod(VMCtx *vmCtx, ObjString *name) {
	VM *vm = &vmCtx->vm;
	Value method = peek(vm, 0);
	ObjClass *clazz = AS_CLASS(peek(vm, 1));
	tableSet(vmCtx, &clazz->methods, name, method);
	pop(vm);
}

static void defineNative(VMCtx *vmCtx, const char *name, NativeFn function) {
	VM *vm = &vmCtx->vm;
	push(vm, OBJ_VAL(copyString(vmCtx, name, (int)strlen(name))));
	push(vm, OBJ_VAL(newNative(vmCtx, function)));
	tableSet(vmCtx, &vm->globals, AS_STRING(vm->stack[0]), vm->stack[1]);
	pop(vm);
	pop(vm);
}

static Value objectToString(VMCtx *vmCtx, int argCount SLOX_UNUSED, Value *args) {
	HeapCString ret;
	initHeapStringSize(vmCtx, &ret, 16);
	ObjInstance *inst = AS_INSTANCE(args[0]);
	addStringFmt(vmCtx, &ret, "%s@%u", inst->clazz->name->chars, inst->identityHash);
	return(OBJ_VAL(takeString(vmCtx, ret.chars, ret.length, ret.capacity)));
}

static Value objectHashCode(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	ObjInstance *inst = AS_INSTANCE(args[0]);
	return(NUMBER_VAL(inst->identityHash));
}

static ObjClass *defineGlobalClass(VMCtx *vmCtx, const char *name) {
	VM *vm = &vmCtx->vm;
	ObjString *className = copyString(vmCtx, name, strlen(name));
	push(vm, OBJ_VAL(className));
	ObjClass *clazz = newClass(vmCtx, className);
	push(vm, OBJ_VAL(clazz));
	tableSet(vmCtx, &vm->globals, className, OBJ_VAL(clazz));
	pop(vm);
	pop(vm);
	return clazz;
}

static void initVM(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	resetStack(vmCtx);
	stc64_init(&vm->prng, 64);
	vm->objects = NULL;
	vm->bytesAllocated = 0;
	vm->nextGC = 1024 * 1024;

	vm->grayCount = 0;
	vm->grayCapacity = 0;
	vm->grayStack = NULL;

	initTable(&vm->globals);
	initTable(&vm->strings);

	vm->initString = NULL;
	vm->initString = copyString(vmCtx, "init", 4);

	ObjClass *rootClass = defineGlobalClass(vmCtx, "Object");
	addNativeMethod(vmCtx, rootClass, "toString", objectToString);
	addNativeMethod(vmCtx, rootClass, "hashCode", objectHashCode);

	defineNative(vmCtx, "clock", clockNative);
}

static void *defaultRealloc(void *oldPtr, size_t newSize, void *userData SLOX_UNUSED) {
	return realloc(oldPtr, newSize);
}

static void defaultFree(void *ptr, void *userData SLOX_UNUSED) {
	free(ptr);
}

void initVMCtx(VMCtx *vmCtx) {
	initVM(vmCtx);
	vmCtx->realloc = defaultRealloc;
	vmCtx->free = defaultFree;
	vmCtx->allocatorUserdata = NULL;
}


void freeVM(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	freeTable(vmCtx, &vm->globals);
	freeTable(vmCtx, &vm->strings);
	vm->initString = NULL;
	freeObjects(vmCtx);
}

static bool call(VMCtx *vmCtx, Obj *callee, ObjFunction *function, int argCount) {
	VM *vm = &vmCtx->vm;

	if (argCount != function->arity) {
		runtimeError(vmCtx, "Expected %d arguments but got %d.", function->arity, argCount);
		return false;
	}

	if (vm->frameCount == FRAMES_MAX) {
		runtimeError(vmCtx, "Stack overflow.");
		return false;
	}

	CallFrame *frame = &vm->frames[vm->frameCount++];
	frame->function = (Obj *)callee;
	frame->ip = function->chunk.code;

	frame->slots = vm->stackTop - argCount - 1;
	return true;
}

static bool callClosure(VMCtx *vmCtx, ObjClosure *closure, int argCount) {
	return call(vmCtx, (Obj *)closure, closure->function, argCount);
}

static bool callFunction(VMCtx *vmCtx, ObjFunction *function, int argCount) {
	return call(vmCtx, (Obj *)function, function, argCount);
}

static bool callNative(VMCtx *vmCtx, NativeFn native, int argCount, bool method) {
	VM *vm = &vmCtx->vm;

	// for native methods include 'this'
	Value result = native(vmCtx, argCount, vm->stackTop - argCount - (int)method);
	vm->stackTop -= argCount + 1;
	push(vm, result);
	return true;
}

static bool callMethod(VMCtx *vmCtx, Obj *function, int argCount) {
	switch (function->type) {
		case OBJ_FUNCTION:
			return callFunction(vmCtx, (ObjFunction *)function, argCount);
		case OBJ_CLOSURE:
			return callClosure(vmCtx, (ObjClosure *)function, argCount);
		case OBJ_NATIVE:
			return callNative(vmCtx, ((ObjNative *)function)->function, argCount, true);
		default:
			runtimeError(vmCtx, "Can only call functions and classes.");
			break;
	}
	return false;
}

static bool callValue(VMCtx *vmCtx, Value callee, int argCount) {
	VM *vm = &vmCtx->vm;

	if (IS_OBJ(callee)) {
		switch (OBJ_TYPE(callee)) {
			case OBJ_BOUND_METHOD: {
				ObjBoundMethod *bound = AS_BOUND_METHOD(callee);

				vm->stackTop[-argCount - 1] = bound->receiver;
				return callMethod(vmCtx, bound->method, argCount);
			}
			case OBJ_CLASS: {
				ObjClass *clazz = AS_CLASS(callee);
				vm->stackTop[-argCount - 1] = OBJ_VAL(newInstance(vmCtx, clazz));
				Value initializer;
				if (tableGet(&clazz->methods, vm->initString, &initializer)) {
					return callMethod(vmCtx, AS_OBJ(initializer), argCount);
				} else if (argCount != 0) {
					runtimeError(vmCtx, "Expected 0 arguments but got %d.", argCount);
					return false;
				}
				return true;
			}
			case OBJ_CLOSURE:
				return callClosure(vmCtx, AS_CLOSURE(callee), argCount);
			case OBJ_FUNCTION:
				return callFunction(vmCtx, AS_FUNCTION(callee), argCount);
			case OBJ_NATIVE:
				return callNative(vmCtx, AS_NATIVE(callee), argCount, false);
			default:
				break; // Non-callable object type.
		}
	}
	runtimeError(vmCtx, "Can only call functions and classes.");
	return false;
}

static bool invokeFromClass(VMCtx *vmCtx, ObjClass *clazz, ObjString *name, int argCount) {
	Value method;
	if (!tableGet(&clazz->methods, name, &method)) {
		runtimeError(vmCtx, "Undefined property '%s'.", name->chars);
		return false;
	}
	return callMethod(vmCtx, AS_OBJ(method), argCount);
}

static bool invoke(VMCtx *vmCtx, ObjString *name, int argCount) {
	VM *vm = &vmCtx->vm;

	Value receiver = peek(vm, argCount);

	if (!IS_INSTANCE(receiver)) {
		runtimeError(vmCtx, "Only instances have methods.");
		return false;
	}

	ObjInstance *instance = AS_INSTANCE(receiver);

	Value value;
	if (tableGet(&instance->fields, name, &value)) {
		vm->stackTop[-argCount - 1] = value;
		return callValue(vmCtx, value, argCount);
	}

	return invokeFromClass(vmCtx, instance->clazz, name, argCount);
}

static bool bindMethod(VMCtx *vmCtx, ObjClass *clazz, ObjString *name) {
	VM *vm = &vmCtx->vm;

	Value method;
	if (!tableGet(&clazz->methods, name, &method)) {
		runtimeError(vmCtx, "Undefined property '%s'.", name->chars);
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

	if (upvalue != NULL && upvalue->location == local) {
		return upvalue;
	}

	ObjUpvalue *createdUpvalue = newUpvalue(vmCtx, local);
	createdUpvalue->next = upvalue;

	if (prevUpvalue == NULL) {
		vm->openUpvalues = createdUpvalue;
	} else {
		prevUpvalue->next = createdUpvalue;
	}

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

static bool isFalsey(Value value) {
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
	pop(vm);
	pop(vm);
	push(vm, OBJ_VAL(result));
}

#ifdef DEBUG_TRACE_EXECUTION
static void printStack() {
	printf("          ");
	for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
		printf("[ ");
		printValue(*slot);
		printf(" ]");
	}
	printf("\n");

}
#endif

static InterpretResult run(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;
	CallFrame* frame = &vm->frames[vm->frameCount - 1];

#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() \
	(frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_CONSTANT() \
	(getFrameFunction(frame)->chunk.constants.values[READ_BYTE()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op) \
	do { \
		if (!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1))) { \
			runtimeError(vmCtx, "Operands must be numbers."); \
			return INTERPRET_RUNTIME_ERROR; \
		} \
		double b = AS_NUMBER(pop(vm)); \
		double a = AS_NUMBER(pop(vm)); \
		push(vm, valueType(a op b)); \
	} while (false)

	for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
		printStack();

		disassembleInstruction(&getFrameFunction(frame)->chunk,
							   (int)(frame->ip - getFrameFunction(frame)->chunk.code));
#endif
		uint8_t instruction;
		switch (instruction = READ_BYTE()) {
			case OP_CONSTANT: {
				Value constant = READ_CONSTANT();
				push(vm, constant);
				break;
			}
			case OP_NIL:
				push(vm, NIL_VAL);
				break;
			case OP_TRUE:
				push(vm, BOOL_VAL(true));
				break;
			case OP_FALSE:
				push(vm, BOOL_VAL(false));
				break;
			case OP_POP:
				pop(vm);
				break;
			case OP_GET_LOCAL: {
				uint8_t slot = READ_BYTE();
				push(vm, frame->slots[slot]);
				break;
			}
			case OP_GET_GLOBAL: {
				ObjString *name = READ_STRING();
				Value value;
				if (!tableGet(&vm->globals, name, &value)) {
					runtimeError(vmCtx, "Undefined variable '%s'.", name->chars);
					return INTERPRET_RUNTIME_ERROR;
				}
				push(vm, value);
				break;
			}
			case OP_DEFINE_GLOBAL: {
				ObjString *name = READ_STRING();
				tableSet(vmCtx, &vm->globals, name, peek(vm, 0));
				pop(vm);
				break;
			}
			case OP_SET_LOCAL: {
				uint8_t slot = READ_BYTE();
				frame->slots[slot] = peek(vm, 0);
				break;
			}
			case OP_SET_GLOBAL: {
				ObjString* name = READ_STRING();
				if (tableSet(vmCtx, &vm->globals, name, peek(vm, 0))) {
					tableDelete(&vm->globals, name);
					runtimeError(vmCtx, "Undefined variable '%s'.", name->chars);
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}
			case OP_GET_UPVALUE: {
				uint8_t slot = READ_BYTE();
				push(vm, *getFrameClosure(frame)->upvalues[slot]->location);
				break;
			}
			case OP_SET_UPVALUE: {
				uint8_t slot = READ_BYTE();
				*getFrameClosure(frame)->upvalues[slot]->location = peek(vm, 0);
				break;
			}
			case OP_GET_PROPERTY: {
				if (!IS_INSTANCE(peek(vm, 0))) {
					runtimeError(vmCtx, "Only instances have properties.");
					return INTERPRET_RUNTIME_ERROR;
				}

				ObjInstance *instance = AS_INSTANCE(peek(vm, 0));
				ObjString *name = READ_STRING();

				Value value;
				if (tableGet(&instance->fields, name, &value)) {
					pop(vm); // Instance.
					push(vm, value);
					break;
				}

				if (!bindMethod(vmCtx, instance->clazz, name)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}
			case OP_SET_PROPERTY: {
				if (!IS_INSTANCE(peek(vm, 1))) {
					runtimeError(vmCtx, "Only instances have fields.");
					return INTERPRET_RUNTIME_ERROR;
				}
				ObjInstance *instance = AS_INSTANCE(peek(vm, 1));
				tableSet(vmCtx, &instance->fields, READ_STRING(), peek(vm, 0));
				Value value = pop(vm);
				pop(vm);
				push(vm, value);
				break;
			}
			case OP_GET_SUPER: {
				ObjString *name = READ_STRING();
				ObjClass *superclass = AS_CLASS(pop(vm));

				if (!bindMethod(vmCtx, superclass, name)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}
			case OP_EQUAL: {
				Value b = pop(vm);
				Value a = pop(vm);
				push(vm, BOOL_VAL(valuesEqual(a, b)));
				break;
			}
			case OP_GREATER:
				BINARY_OP(BOOL_VAL, >);
				break;
			case OP_LESS:
				BINARY_OP(BOOL_VAL, <);
				break;
			case OP_ADD: {
				if (IS_STRING(peek(vm, 0)) && IS_STRING(peek(vm, 1))) {
					concatenate(vmCtx);
				} else if (IS_NUMBER(peek(vm, 0)) && IS_NUMBER(peek(vm, 1))) {
					double b = AS_NUMBER(pop(vm));
					double a = AS_NUMBER(pop(vm));
					push(vm, NUMBER_VAL(a + b));
				} else {
					runtimeError(vmCtx, "Operands must be two numbers or two strings.");
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}
			case OP_SUBTRACT:
				BINARY_OP(NUMBER_VAL, -);
				break;
			case OP_MULTIPLY:
				BINARY_OP(NUMBER_VAL, *);
				break;
			case OP_DIVIDE:
				BINARY_OP(NUMBER_VAL, /);
				break;
			case OP_NOT:
				push(vm, BOOL_VAL(isFalsey(pop(vm))));
				break;
			case OP_NEGATE:
				if (!IS_NUMBER(peek(vm, 0))) {
					runtimeError(vmCtx, "Operand must be a number.");
					return INTERPRET_RUNTIME_ERROR;
				}
				push(vm, NUMBER_VAL(-AS_NUMBER(pop(vm))));
				break;
			case OP_PRINT: {
				printValue(pop(vm));
				printf("\n");
				break;
			}
			case OP_JUMP: {
				uint16_t offset = READ_SHORT();
				frame->ip += offset;
				break;
			}
			case OP_JUMP_IF_FALSE: {
				uint16_t offset = READ_SHORT();
				if (isFalsey(peek(vm, 0)))
					frame->ip += offset;
				break;
			}
			case OP_LOOP: {
				uint16_t offset = READ_SHORT();
				frame->ip -= offset;
				break;
			}
			case OP_CALL: {
				int argCount = READ_BYTE();
				if (!callValue(vmCtx, peek(vm, argCount), argCount)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm->frames[vm->frameCount - 1];
				break;
			}
			case OP_INVOKE: {
				ObjString *method = READ_STRING();
				int argCount = READ_BYTE();
				if (!invoke(vmCtx, method, argCount)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm->frames[vm->frameCount - 1];
				break;
			}
			case OP_SUPER_INVOKE: {
				ObjString *method = READ_STRING();
				int argCount = READ_BYTE();
				ObjClass *superclass = AS_CLASS(pop(vm));
				if (!invokeFromClass(vmCtx, superclass, method, argCount)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm->frames[vm->frameCount - 1];
				break;
			}
			case OP_CLOSURE: {
				ObjFunction *function = AS_FUNCTION(READ_CONSTANT());
				ObjClosure *closure = newClosure(vmCtx, function);
				push(vm, OBJ_VAL(closure));
				for (int i = 0; i < closure->upvalueCount; i++) {
					uint8_t isLocal = READ_BYTE();
					uint8_t index = READ_BYTE();
					if (isLocal) {
						closure->upvalues[i] = captureUpvalue(vmCtx, frame->slots + index);
					} else {
						closure->upvalues[i] = getFrameClosure(frame)->upvalues[index];
					}
				}

				break;
			}
			case OP_CLOSE_UPVALUE:
				closeUpvalues(vm, vm->stackTop - 1);
				pop(vm);
				break;
			case OP_RETURN: {
				Value result = pop(vm);
				closeUpvalues(vm, frame->slots);
				vm->frameCount--;
				if (vm->frameCount == 0) {
					pop(vm);
					return INTERPRET_OK;
				}

				vm->stackTop = frame->slots;
				push(vm, result);
				frame = &vm->frames[vm->frameCount - 1];
				break;
			}
			case OP_CLASS:
				push(vm, OBJ_VAL(newClass(vmCtx, READ_STRING())));
				break;
			case OP_INHERIT: {
				Value superclass = peek(vm, 1);
				if (!IS_CLASS(superclass)) {
					runtimeError(vmCtx, "Superclass must be a class.");
					return INTERPRET_RUNTIME_ERROR;
				}
				ObjClass *subclass = AS_CLASS(peek(vm, 0));
				tableAddAll(vmCtx, &AS_CLASS(superclass)->methods, &subclass->methods);
				pop(vm); // Subclass.
				break;
			}
			case OP_METHOD:
				defineMethod(vmCtx, READ_STRING());
				break;
		}
	}

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(VMCtx *vmCtx, const char *source) {
	VM *vm = &vmCtx->vm;

	ObjFunction *function = compile(vmCtx, source);
	if (function == NULL)
		return INTERPRET_COMPILE_ERROR;

	push(vm, OBJ_VAL(function));
	ObjClosure *closure = newClosure(vmCtx, function);
	pop(vm);
	push(vm, OBJ_VAL(closure));
	callClosure(vmCtx, closure, 0);

	return run(vmCtx);
}
