#include <stdio.h>
#include <string.h>

#include "slox/memory.h"
#include "slox/object.h"
#include "slox/table.h"
#include "slox/value.h"
#include "slox/vm.h"

#define ALLOCATE_OBJ(type, objectType) \
	(type*)allocateObject(sizeof(type), objectType)

static Obj *allocateObject(size_t size, ObjType type) {
	Obj *object = (Obj *)reallocate(NULL, 0, size);
	object->type = type;
	object->isMarked = false;
	object->next = vm.objects;
	vm.objects = object;

#ifdef DEBUG_LOG_GC
	printf("%p allocate %zu for %d\n", (void*)object, size, type);
#endif

	return object;
}

ObjBoundMethod *newBoundMethod(Value receiver, Obj *method) {
	ObjBoundMethod *bound = ALLOCATE_OBJ(ObjBoundMethod, OBJ_BOUND_METHOD);
	bound->receiver = receiver;
	bound->method = method;
	return bound;
}

ObjClass *newClass(ObjString *name) {
	ObjClass *clazz = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
	clazz->name = name;
	initTable(&clazz->methods);
	return clazz;
}

ObjClosure *newClosure(ObjFunction *function) {
	ObjUpvalue **upvalues = ALLOCATE(ObjUpvalue *, function->upvalueCount);
	for (int i = 0; i < function->upvalueCount; i++) {
		upvalues[i] = NULL;
	}

	ObjClosure *closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
	closure->function = function;
	closure->upvalues = upvalues;
	closure->upvalueCount = function->upvalueCount;
	return closure;
}

ObjFunction *newFunction() {
	ObjFunction *function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
	function->arity = 0;
	function->upvalueCount = 0;
	function->name = NULL;
	initChunk(&function->chunk);
	return function;
}

ObjInstance *newInstance(ObjClass* clazz) {
	ObjInstance *instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
	instance->clazz = clazz;
	initTable(&instance->fields);
	return instance;
}

ObjNative *newNative(NativeFn function) {
	ObjNative *native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
	native->function = function;
	return native;
}

ObjNative *addNativeMethod(ObjClass *clazz, const char *name, NativeFn method) {
	ObjString *methodName = copyString(name, strlen(name));
	push(OBJ_VAL(methodName));
	ObjNative *nativeObj = newNative(method);
	push(OBJ_VAL(nativeObj));
	tableSet(&clazz->methods, methodName, OBJ_VAL(nativeObj));
	pop();
	pop();
	return nativeObj;
}

static ObjString *allocateString(char *chars, int length, uint32_t hash) {
	ObjString *string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
	string->length = length;
	string->capacity = length + 1;
	string->chars = chars;
	string->hash = hash;
	push(OBJ_VAL(string));
	tableSet(&vm.strings, string, NIL_VAL);
	pop();
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

ObjString *takeString(char *chars, int length, int capacity) {
	uint32_t hash = hashString(chars, length);
	ObjString *interned = tableFindString(&vm.strings, chars, length, hash);
	if (interned != NULL) {
		FREE_ARRAY(char, chars, capacity);
		return interned;
	}
	return allocateString(chars, length, hash);
}

ObjString *copyString(const char *chars, int length) {
	uint32_t hash = hashString(chars, length);
	ObjString *interned = tableFindString(&vm.strings, chars, length, hash);
	if (interned != NULL)
		return interned;
	char *heapChars = ALLOCATE(char, length + 1);
	memcpy(heapChars, chars, length);
	heapChars[length] = '\0';
	return allocateString(heapChars, length, hash);
}

void initHeapString(HeapCString *str) {
	initHeapStringSize(str, 8);
}

void initHeapStringSize(HeapCString *str, int initialCapacity) {
	str->chars = ALLOCATE(char, initialCapacity);
	str->chars[0] = '\0';
	str->length = 0;
	str->capacity = initialCapacity;
}

void addStringFmt(HeapCString *string, const char *format, ...) {
	va_list args;
	va_start(args, format);
	addStringVFmt(string, format, args);
	va_end(args);
}

void addStringVFmt(HeapCString *string, const char *format, va_list ap) {
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
	string->chars = GROW_ARRAY(char, string->chars, string->capacity, newCapacity);
	string->capacity = newCapacity;

	available = string->capacity - string->length - 1;
	required = vsnprintf(string->chars + string->length, available, format, ap);
	string->length += required;
}

ObjUpvalue *newUpvalue(Value *slot) {
	ObjUpvalue *upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
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
