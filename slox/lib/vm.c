#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

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
	tableSet(vmCtx, &clazz->methods, name, method);
	pop(vm);
}

void defineNative(VMCtx *vmCtx, const char *name, NativeFn function) {
	VM *vm = &vmCtx->vm;
	push(vm, OBJ_VAL(copyString(vmCtx, name, (int)strlen(name))));
	push(vm, OBJ_VAL(newNative(vmCtx, function)));
	tableSet(vmCtx, &vm->globals, AS_STRING(vm->stack[0]), vm->stack[1]);
	pop(vm);
	pop(vm);
}

void initVM(VMCtx *vmCtx) {
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

static bool instanceof(ObjClass *clazz, ObjInstance *instance) {
	return instance->clazz == clazz;
}

static bool propagateException(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	ObjInstance *exception = AS_INSTANCE(peek(vm, 0));

	while (vm->frameCount > 0) {
		CallFrame *frame = &vm->frames[vm->frameCount - 1];
		for (int handlerStack = frame->handlerCount; handlerStack > 0; handlerStack--) {
			uint16_t handlerTableOffset = frame->handlerStack[handlerStack - 1];
			ObjFunction *frameFunction = getFrameFunction(frame);
			uint8_t *handlerTable = frameFunction->chunk.code + handlerTableOffset;
			uint8_t numHandlers = handlerTable[0] / 4;
			for (int i = 0; i < numHandlers; i++) {
				uint8_t *handlerRecord = handlerTable + 1 + (4 * i);
				uint16_t type = (uint16_t)(handlerRecord[0] << 8);
				type |= handlerRecord[1];
				Value typeNameVal = frameFunction->chunk.constants.values[type];
				if (!IS_STRING(typeNameVal)) {
					runtimeError(vmCtx, "Type name expected in catch handler");
					return false;
				}
				ObjString *typeName = AS_STRING(typeNameVal);
				Value classVal;
				if (!tableGet(&vm->globals, typeName, &classVal) || !IS_CLASS(classVal)) {
					runtimeError(vmCtx, "'%s' is not a type to catch", typeName->chars);
					return false;
				}
				ObjClass *handlerClass = AS_CLASS(classVal);
				if (instanceof(handlerClass, exception)) {
					uint16_t handlerAddress = (uint16_t)(handlerRecord[2] << 8);
					handlerAddress |= handlerRecord[3];
					frame->ip = &frameFunction->chunk.code[handlerAddress];
					return true;
				}
			}
		}
		vm->frameCount--;
	}

	fprintf(stderr, "Unhandled exception %s\n", exception->clazz->name->chars);
	Value stacktrace;
	if (tableGet(&exception->fields, copyString(vmCtx, STR_AND_LEN("stacktrace")), &stacktrace)) {
		fprintf(stderr, "%s", AS_CSTRING(stacktrace));
		fflush(stderr);
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

	uint16_t *handlerTable = &frame->handlerStack[stackLevel];
	if (stackLevel >= frame->handlerCount)
		frame->handlerCount = stackLevel + 1;

	*handlerTable = handlerTableAddress;
	return true;
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
			for (int i = 0; i < extraArgs; i++)
				pop(vm);
		}
	}

	if (vm->frameCount == FRAMES_MAX) {
		runtimeError(vmCtx, "Stack overflow.");
		return false;
	}

	CallFrame *frame = &vm->frames[vm->frameCount++];
	frame->function = (Obj *)callee;
	frame->ip = function->chunk.code;

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
						  argCount + (int) method, vm->stackTop - argCount - (int)method,
						  closure->upvalueCount, closure->upvalues);
	vm->stackTop -= argCount + 1;
	push(vm, result);
	return true;
}

static bool callFunction(VMCtx *vmCtx, ObjFunction *function, int argCount) {
	return call(vmCtx, (Obj *)function, function, argCount);
}

static bool callNative(VMCtx *vmCtx, NativeFn native, int argCount, bool method) {
	VM *vm = &vmCtx->vm;

	// for native methods include 'this'
	Value result = native(vmCtx, argCount + (int)method, vm->stackTop - argCount - (int)method);
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
		case OBJ_NATIVE_CLOSURE:
			return callNativeClosure(vmCtx, (ObjNativeClosure *)function, argCount, true);
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
			case OBJ_NATIVE_CLOSURE:
				return callNativeClosure(vmCtx, AS_NATIVE_CLOSURE(callee), argCount, false);
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

static ObjClass *classOf(VM *vm, const Obj *obj, ObjInstance **instance) {
	*instance = NULL;
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

	return NULL;
}

static ObjClass *classOfValue(VM *vm, Value val, ObjInstance **instance) {
	if (!IS_OBJ(val))
		return NULL;
	return classOf(vm, AS_OBJ(val), instance);
}

static bool invoke(VMCtx *vmCtx, ObjString *name, int argCount) {
	VM *vm = &vmCtx->vm;

	Value receiver = peek(vm, argCount);

	ObjInstance *instance = NULL;
	ObjClass *clazz = NULL;
	if (IS_OBJ(receiver))
		clazz = classOf(vm, AS_OBJ(receiver), &instance);
	if (clazz == NULL) {
		runtimeError(vmCtx, "Only instances have methods.");
		return false;
	}

	if (instance != NULL) {
		Value value;
		if (tableGet(&instance->fields, name, &value)) {
			vm->stackTop[-argCount - 1] = value;
			return callValue(vmCtx, value, argCount);
		}
	}

	return invokeFromClass(vmCtx, clazz, name, argCount);
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
static void printStack(VM *vm) {
	printf("          ");
	for (Value* slot = vm->stack; slot < vm->stackTop; slot++) {
		printf("[ ");
		printValue(*slot);
		printf(" ]");
	}
	printf("\n");

}
#endif

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

static InterpretResult run(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;
	CallFrame* frame = &vm->frames[vm->frameCount - 1];
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
#define READ_SHORT() \
	(ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))
#define READ_CONSTANT() \
	(getFrameFunction(frame)->chunk.constants.values[READ_SHORT()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op) \
	do { \
		if (!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1))) { \
			frame->ip = ip; \
			runtimeError(vmCtx, "Operands must be numbers."); \
			return INTERPRET_RUNTIME_ERROR; \
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
				if (!tableGet(&vm->globals, name, &value)) {
					frame->ip = ip;
					runtimeError(vmCtx, "Undefined variable '%s'.", name->chars);
					return INTERPRET_RUNTIME_ERROR;
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
				if (tableSet(vmCtx, &vm->globals, name, peek(vm, 0))) {
					tableDelete(&vm->globals, name);
					frame->ip = ip;
					runtimeError(vmCtx, "Undefined variable '%s'.", name->chars);
					return INTERPRET_RUNTIME_ERROR;
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
						if (tableGet(&instance->fields, name, &value)) {
							pop(vm); // Instance.
							push(vm, value);
							DISPATCH_BREAK;
						}
					}

					frame->ip = ip;
					if (!bindMethod(vmCtx, instance->clazz, name)) {
						return INTERPRET_RUNTIME_ERROR;
					}
				} else if (IS_MAP(instanceVal)) {
					ObjMap *map = AS_MAP(instanceVal);

					if (methodsOnly) {
						frame->ip = ip;
						if (!bindMethod(vmCtx, vm->mapClass, name)) {
							return INTERPRET_RUNTIME_ERROR;
						}
					} else {
						Value value;
						bool found = valueTableGet(&map->items, OBJ_VAL(name), &value);
						if (!found)
							value = NIL_VAL;
						pop(vm); // map
						push(vm, value);
					}
				} else {
					ObjInstance *instance;
					ObjClass *clazz = classOfValue(vm, instanceVal, &instance);
					if (clazz != NULL) {
						frame->ip = ip;
						if (!bindMethod(vmCtx, clazz, name)) {
							return INTERPRET_RUNTIME_ERROR;
						}
					} else {
						frame->ip = ip;
						runtimeError(vmCtx, "This value doesn't have properties");
						return INTERPRET_RUNTIME_ERROR;
					}
				}

				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SET_PROPERTY): {
				Value instanceVal = peek(vm, 1);

				if (IS_INSTANCE(instanceVal)) {
					ObjInstance *instance = AS_INSTANCE(instanceVal);
					tableSet(vmCtx, &instance->fields, READ_STRING(), peek(vm, 0));
					Value value = pop(vm);
					pop(vm);
					push(vm, value);
				} else if (IS_MAP(instanceVal)) {
					ObjMap *map = AS_MAP(instanceVal);
					ObjString *index = READ_STRING();
					Value value = peek(vm, 0);
					valueTableSet(vmCtx, &map->items, OBJ_VAL(index), value);
					value = pop(vm);
					pop(vm);
					push(vm, value);
				} else {
					frame->ip = ip;
					runtimeError(vmCtx, "Only instances have fields");
					return INTERPRET_RUNTIME_ERROR;
				}

				DISPATCH_BREAK;
			}
			DISPATCH_CASE(GET_SUPER): {
				ObjString *name = READ_STRING();
				ObjClass *superclass = AS_CLASS(pop(vm));

				frame->ip = ip;
				if (!bindMethod(vmCtx, superclass, name)) {
					return INTERPRET_RUNTIME_ERROR;
				}
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
					runtimeError(vmCtx, "Operands must be two numbers or two strings.");
					return INTERPRET_RUNTIME_ERROR;
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
					return INTERPRET_RUNTIME_ERROR;
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
				if (!IS_NUMBER(peek(vm, 0))) {
					frame->ip = ip;
					runtimeError(vmCtx, "Operand must be a number.");
					return INTERPRET_RUNTIME_ERROR;
				}
				push(vm, NUMBER_VAL(-AS_NUMBER(pop(vm))));
				DISPATCH_BREAK;
			DISPATCH_CASE(PRINT): {
				printValue(pop(vm));
				printf("\n");
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(JUMP): {
				uint16_t offset = READ_SHORT();
				ip += offset;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(JUMP_IF_FALSE): {
				uint16_t offset = READ_SHORT();
				if (isFalsey(peek(vm, 0)))
					ip += offset;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(LOOP): {
				uint16_t offset = READ_SHORT();
				ip -= offset;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CALL): {
				int argCount = READ_BYTE();
				frame->ip = ip;
				if (!callValue(vmCtx, peek(vm, argCount), argCount)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm->frames[vm->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INVOKE): {
				ObjString *method = READ_STRING();
				int argCount = READ_BYTE();
				frame->ip = ip;
				if (!invoke(vmCtx, method, argCount)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm->frames[vm->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(SUPER_INVOKE): {
				ObjString *method = READ_STRING();
				int argCount = READ_BYTE();
				ObjClass *superclass = AS_CLASS(pop(vm));
				frame->ip = ip;
				if (!invokeFromClass(vmCtx, superclass, method, argCount)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm->frames[vm->frameCount - 1];
				ip = frame->ip;
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(CLOSURE): {
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
				Value superclass = peek(vm, 1);
				if (!IS_CLASS(superclass)) {
					frame->ip = ip;
					runtimeError(vmCtx, "Superclass must be a class.");
					return INTERPRET_RUNTIME_ERROR;
				}
				ObjClass *subclass = AS_CLASS(peek(vm, 0));
				tableAddAll(vmCtx, &AS_CLASS(superclass)->methods, &subclass->methods);
				pop(vm); // Subclass.
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(METHOD):
				defineMethod(vmCtx, READ_STRING());
				DISPATCH_BREAK;
			DISPATCH_CASE(ARRAY_BUILD): {
				ObjType objType = READ_BYTE();
				uint16_t itemCount = READ_SHORT();
				ObjArray *array = newArray(vmCtx, itemCount, objType);

				push(vm, OBJ_VAL(array));
				for (int i = itemCount; i > 0; i--) {
					appendToArray(vmCtx, array, peek(vm, i));
				}
				pop(vm);

				while (itemCount-- > 0) {
					pop(vm);
				}

				push(vm, OBJ_VAL(array));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INDEX): {
				Value indexVal = pop(vm);
				Value indexableVal = pop(vm);
				Value result;

				if (IS_ARRAY(indexableVal)) {
					ObjArray *array = AS_ARRAY(indexableVal);

					if (!IS_NUMBER(indexVal)) {
						runtimeError(vmCtx, "Array index is not a number");
						return INTERPRET_RUNTIME_ERROR;
					}
					int index = AS_NUMBER(indexVal);

					if (!isValidArrayIndex(array, index)) {
						frame->ip = ip;
						runtimeError(vmCtx, "Array index out of range");
						return INTERPRET_RUNTIME_ERROR;
					}

					result = arrayAt(array, index);
				} else if (IS_MAP(indexableVal)) {
					ObjMap *map = AS_MAP(indexableVal);

					bool found = valueTableGet(&map->items, indexVal, &result);
					if (!found)
						result = NIL_VAL;
				} else {
					frame->ip = ip;
					runtimeError(vmCtx, "Invalid type to index into");
					return INTERPRET_RUNTIME_ERROR;
				}

				push(vm, result);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(INDEX_STORE): {
				Value item = pop(vm);
				Value indexVal = pop(vm);
				Value indexableVal = pop(vm);

				if (IS_ARRAY(indexableVal)) {
					ObjArray *array = AS_ARRAY(indexableVal);

					if (!IS_NUMBER(indexVal)) {
						frame->ip = ip;
						runtimeError(vmCtx, "Array index is not a number");
						return INTERPRET_RUNTIME_ERROR;
					}
					int index = AS_NUMBER(indexVal);

					if (!isValidArrayIndex(array, index)) {
						frame->ip = ip;
						runtimeError(vmCtx, "Array index out of range");
						return INTERPRET_RUNTIME_ERROR;
					}

					arraySet(array, index, item);
				} else if (IS_MAP(indexableVal)) {
					ObjMap *map = AS_MAP(indexableVal);

					valueTableSet(vmCtx, &map->items, indexVal, item);
				} else {
					frame->ip = ip;
					runtimeError(vmCtx, "Destination is not an array or map");
					return INTERPRET_RUNTIME_ERROR;
				}

				push(vm, item);
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(MAP_BUILD): {
				ObjMap *map = newMap(vmCtx);
				uint16_t itemCount = READ_SHORT();

				push(vm, OBJ_VAL(map));
				int i = 2 * itemCount;
				while (i > 0) {
					Value key = peek(vm, i--);
					Value value = peek(vm, i--);

					valueTableSet(vmCtx, &map->items, key, value);
				}
				pop(vm);

				// pop constructor items from the stack
				for (i = 0; i < itemCount; i++) {
					pop(vm);
					pop(vm);
				}

				push(vm, OBJ_VAL(map));
				DISPATCH_BREAK;
			}
			DISPATCH_CASE(THROW): {
				frame->ip = ip;
				Value stacktrace = getStackTrace(vmCtx);
				ObjInstance *instance = AS_INSTANCE(peek(vm, 0));
				push(vm, stacktrace);
				ObjString *stacktraceName = copyString(vmCtx, STR_AND_LEN("stacktrace"));
				push(vm, OBJ_VAL(stacktraceName));
				tableSet(vmCtx, &instance->fields, stacktraceName, stacktrace);
				pop(vm);
				pop(vm);
				if (propagateException(vmCtx)) {
					frame = &vm->frames[vm->frameCount - 1];
					ip = frame->ip;
					DISPATCH_BREAK;
				}
				return INTERPRET_RUNTIME_ERROR;
			}
			DISPATCH_CASE(PUSH_EXCEPTION_HANDLER): {
				uint8_t stackLevel = READ_BYTE();
				uint16_t handlerTableAddress = READ_SHORT();
				frame->ip = ip;
				if (!pushExceptionHandler(vmCtx, stackLevel, handlerTableAddress))
					return INTERPRET_RUNTIME_ERROR;
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
							return INTERPRET_RUNTIME_ERROR;
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
							if (bindMethod(vmCtx, clazz, vm->iteratorString)) {
								hasIterator = true;
							}
						}
						if (!hasIterator) {
							frame->ip = ip;
							runtimeError(vmCtx, "Attempt to iterate non-iterable value");
							return INTERPRET_RUNTIME_ERROR;
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
								runtimeError(vmCtx, "Undefined variable '%s'.", name->chars);
								return INTERPRET_RUNTIME_ERROR;
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
				runtimeError(vmCtx, "Attempted to execute data section.");
				return INTERPRET_RUNTIME_ERROR;
			}
		DISPATCH_END
	}

#undef READ_BYTE
#undef READ_SHORT
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

	return run(vmCtx);
}
