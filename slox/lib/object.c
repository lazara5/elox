#include <stdio.h>
#include <string.h>

#include "slox/memory.h"
#include "slox/object.h"
#include "slox/table.h"
#include "slox/value.h"
#include "slox/vm.h"

#define ALLOCATE_OBJ(vmctx, type, objectType) \
	(type *)allocateObject(vmctx, sizeof(type), objectType)

static Obj *allocateObject(VMCtx *vmCtx, size_t size, ObjType type) {
	VM *vm = &vmCtx->vm;

	Obj *object = (Obj *)reallocate(vmCtx, NULL, 0, size);
	object->type = type;
	object->isMarked = false;
	object->next = vm->objects;
	vm->objects = object;

#ifdef DEBUG_LOG_GC
	printf("%p allocate %zu for %d\n", (void*)object, size, type);
#endif

	return object;
}

ObjBoundMethod *newBoundMethod(VMCtx *vmCtx,Value receiver, Obj *method) {
	ObjBoundMethod *bound = ALLOCATE_OBJ(vmCtx, ObjBoundMethod, OBJ_BOUND_METHOD);
	bound->receiver = receiver;
	bound->method = method;
	return bound;
}

ObjClass *newClass(VMCtx *vmCtx, ObjString *name) {
	ObjClass *clazz = ALLOCATE_OBJ(vmCtx, ObjClass, OBJ_CLASS);
	clazz->name = name;
	initTable(&clazz->methods);
	return clazz;
}

ObjClosure *newClosure(VMCtx *vmCtx, ObjFunction *function) {
	ObjUpvalue **upvalues = ALLOCATE(vmCtx, ObjUpvalue *, function->upvalueCount);
	for (int i = 0; i < function->upvalueCount; i++) {
		upvalues[i] = NULL;
	}

	ObjClosure *closure = ALLOCATE_OBJ(vmCtx, ObjClosure, OBJ_CLOSURE);
	closure->function = function;
	closure->upvalues = upvalues;
	closure->upvalueCount = function->upvalueCount;
	return closure;
}

ObjFunction *newFunction(VMCtx *vmCtx) {
	ObjFunction *function = ALLOCATE_OBJ(vmCtx, ObjFunction, OBJ_FUNCTION);
	function->arity = 0;
	function->upvalueCount = 0;
	function->name = NULL;
	initChunk(&function->chunk);
	return function;
}

ObjInstance *newInstance(VMCtx *vmCtx, ObjClass* clazz) {
	VM *vm = &vmCtx->vm;
	ObjInstance *instance = ALLOCATE_OBJ(vmCtx, ObjInstance, OBJ_INSTANCE);
	instance->clazz = clazz;
	initTable(&instance->fields);
	instance->identityHash = stc64_rand(&vm->prng) & 0xFFFFFFFF;
	return instance;
}

ObjNative *newNative(VMCtx *vmCtx, NativeFn function) {
	ObjNative *native = ALLOCATE_OBJ(vmCtx, ObjNative, OBJ_NATIVE);
	native->function = function;
	return native;
}

ObjNative *addNativeMethod(VMCtx *vmCtx, ObjClass *clazz, const char *name, NativeFn method) {
	VM *vm = &vmCtx->vm;
	ObjString *methodName = copyString(vmCtx, name, strlen(name));
	push(vm, OBJ_VAL(methodName));
	ObjNative *nativeObj = newNative(vmCtx, method);
	push(vm, OBJ_VAL(nativeObj));
	tableSet(vmCtx, &clazz->methods, methodName, OBJ_VAL(nativeObj));
	pop(vm);
	pop(vm);
	return nativeObj;
}

static ObjString *allocateString(VMCtx *vmCtx, char *chars, int length, uint32_t hash) {
	VM *vm = &vmCtx->vm;
	ObjString *string = ALLOCATE_OBJ(vmCtx, ObjString, OBJ_STRING);
	string->length = length;
	string->capacity = length + 1;
	string->chars = chars;
	string->hash = hash;
	push(vm, OBJ_VAL(string));
	tableSet(vmCtx, &vm->strings, string, NIL_VAL);
	pop(vm);
	return string;
}

static uint32_t hashString(const char *key, int length) {
	uint32_t hash = 2166136261u;
	for (int i = 0; i < length; i++) {
		hash ^= (uint8_t)key[i];
		hash *= 16777619;
	}
	return hash;
}

ObjString *takeString(VMCtx *vmCtx, char *chars, int length, int capacity) {
	VM *vm = &vmCtx->vm;
	uint32_t hash = hashString(chars, length);
	ObjString *interned = tableFindString(&vm->strings, chars, length, hash);
	if (interned != NULL) {
		FREE_ARRAY(vmCtx, char, chars, capacity);
		return interned;
	}
	return allocateString(vmCtx, chars, length, hash);
}

ObjString *copyString(VMCtx *vmCtx, const char *chars, int length) {
	VM *vm = &vmCtx->vm;
	uint32_t hash = hashString(chars, length);
	ObjString *interned = tableFindString(&vm->strings, chars, length, hash);
	if (interned != NULL)
		return interned;
	char *heapChars = ALLOCATE(vmCtx, char, length + 1);
	memcpy(heapChars, chars, length);
	heapChars[length] = '\0';
	return allocateString(vmCtx, heapChars, length, hash);
}

void initHeapString(VMCtx *vmCtx, HeapCString *str) {
	initHeapStringSize(vmCtx, str, 8);
}

void initHeapStringSize(VMCtx *vmCtx, HeapCString *str, int initialCapacity) {
	str->chars = ALLOCATE(vmCtx, char, initialCapacity);
	str->chars[0] = '\0';
	str->length = 0;
	str->capacity = initialCapacity;
}

void addStringFmt(VMCtx *vmCtx, HeapCString *string, const char *format, ...) {
	va_list args;
	va_start(args, format);
	addStringVFmt(vmCtx, string, format, args);
	va_end(args);
}

void addStringVFmt(VMCtx *vmCtx, HeapCString *string, const char *format, va_list ap) {
	int available = string->capacity - string->length - 1;
	va_list ap1;
	va_copy(ap1, ap);

	int required = vsnprintf(string->chars + string->length, available, format, ap1);
	va_end(ap1);

	if (required <= available) {
		string->length += required;
		return;
	}

	int requiredCapacity = string->length + required + 1;
	int newCapacity = GROW_CAPACITY(string->capacity);
	newCapacity = (newCapacity < requiredCapacity) ?  requiredCapacity : newCapacity;
	string->chars = GROW_ARRAY(vmCtx, char, string->chars, string->capacity, newCapacity);
	string->capacity = newCapacity;

	available = string->capacity - string->length - 1;
	required = vsnprintf(string->chars + string->length, available, format, ap);
	string->length += required;
}

ObjUpvalue *newUpvalue(VMCtx *vmCtx, Value *slot) {
	ObjUpvalue *upvalue = ALLOCATE_OBJ(vmCtx, ObjUpvalue, OBJ_UPVALUE);
	upvalue->closed = NIL_VAL;
	upvalue->location = slot;
	upvalue->next = NULL;
	return upvalue;
}

static void printFunction(ObjFunction *function) {
	if (function->name == NULL) {
		printf("<script>");
		return;
	}
	 printf("<fn %s>", function->name->chars);
}

static void printMethod(Obj *method) {
	switch (method->type) {
		case OBJ_CLOSURE:
			printFunction(((ObjClosure *)method)->function);
			break;
		case OBJ_FUNCTION:
			printFunction((ObjFunction *)method);
			break;
		case OBJ_NATIVE:
			printf("<native fn>");
			break;
		default:
			break;
	}
}

void printObject(Value value) {
	switch (OBJ_TYPE(value)) {
		case OBJ_BOUND_METHOD:
			printMethod(AS_BOUND_METHOD(value)->method);
			break;
		case OBJ_CLASS:
			printf("%s", AS_CLASS(value)->name->chars);
			break;
		case OBJ_CLOSURE:
			printFunction(AS_CLOSURE(value)->function);
			break;
		case OBJ_FUNCTION:
			printFunction(AS_FUNCTION(value));
			break;
		case OBJ_INSTANCE:
			printf("%s instance", AS_INSTANCE(value)->clazz->name->chars);
			break;
		case OBJ_NATIVE:
			printf("<native fn>");
			break;
		case OBJ_STRING:
			printf("'%s'", AS_CSTRING(value));
			break;
		case OBJ_UPVALUE:
			printf("upvalue");
			break;
	}
}
