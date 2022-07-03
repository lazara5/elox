#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "elox/memory.h"
#include "elox/object.h"
#include "elox/table.h"
#include "elox/value.h"
#include "elox/state.h"

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
	clazz->initializer = NIL_VAL;
	clazz->hashCode = NIL_VAL;
	clazz->equals = NIL_VAL;
	clazz->super = NIL_VAL;
	initTable(&clazz->fields);
	initTable(&clazz->methods);
	clazz->memberRefs = NULL;
	clazz->memberRefCount = 0;
	return clazz;
}

ObjClosure *newClosure(VMCtx *vmCtx, ObjFunction *function) {
	ObjUpvalue **upvalues = ALLOCATE(vmCtx, ObjUpvalue *, function->upvalueCount);
	for (int i = 0; i < function->upvalueCount; i++)
		upvalues[i] = NULL;

	ObjClosure *closure = ALLOCATE_OBJ(vmCtx, ObjClosure, OBJ_CLOSURE);
	closure->function = function;
	closure->upvalues = upvalues;
	closure->upvalueCount = function->upvalueCount;
	return closure;
}

ObjNativeClosure *newNativeClosure(VMCtx *vmCtx, NativeClosureFn function, uint8_t numUpvalues) {
	Value *upvalues = ALLOCATE(vmCtx, Value, numUpvalues);
	for (int i = 0; i < numUpvalues; i++)
		upvalues[i] = NIL_VAL;

	ObjNativeClosure *closure = ALLOCATE_OBJ(vmCtx, ObjNativeClosure, OBJ_NATIVE_CLOSURE);
	closure->nativeFunction = function;
	closure->upvalues = upvalues;
	closure->upvalueCount = numUpvalues;
	return closure;
}

ObjFunction *newFunction(VMCtx *vmCtx) {
	ObjFunction *function = ALLOCATE_OBJ(vmCtx, ObjFunction, OBJ_FUNCTION);
	function->arity = 0;
	function->upvalueCount = 0;
	function->name = NULL;
	function->parentClass = NULL;
	initChunk(&function->chunk);
	return function;
}

ObjInstance *newInstance(VMCtx *vmCtx, ObjClass *clazz) {
	VM *vm = &vmCtx->vm;
	ObjInstance *instance = ALLOCATE_OBJ(vmCtx, ObjInstance, OBJ_INSTANCE);
	push(vmCtx, OBJ_VAL(instance));
	instance->clazz = clazz;
	initSizedValueArray(vmCtx, &instance->fields, clazz->fields.count);
	pop(vm);
	instance->identityHash = stc64_rand(&vm->prng) & 0xFFFFFFFF;
	instance->flags =
			INST_HAS_HASHCODE * (!IS_NIL(clazz->hashCode)) |
			INST_HAS_EQUALS * (!IS_NIL(clazz->equals));
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
	push(vmCtx, OBJ_VAL(methodName));
	ObjNative *nativeObj = newNative(vmCtx, method);
	push(vmCtx, OBJ_VAL(nativeObj));
	if (methodName == clazz->name)
		clazz->initializer = OBJ_VAL(nativeObj);
	else {
		tableSet(vmCtx, &clazz->methods, methodName, OBJ_VAL(nativeObj));
		if (methodName == vm->hashCodeString)
			clazz->hashCode = OBJ_VAL(nativeObj);
		else if (methodName == vm->equalsString)
			clazz->equals = OBJ_VAL(nativeObj);
	}
	popn(vm, 2);
	return nativeObj;
}

void addClassField(VMCtx *vmCtx, ObjClass *clazz, const char *name) {
	VM *vm = &vmCtx->vm;
	ObjString *fieldName = copyString(vmCtx, name, strlen(name));
	push(vmCtx, OBJ_VAL(fieldName));
	int index = clazz->fields.count;
	tableSet(vmCtx, &clazz->fields, fieldName, NUMBER_VAL(index));
	pop(vm);
}

static ObjString *allocateString(VMCtx *vmCtx, char *chars, int length, uint32_t hash) {
	VM *vm = &vmCtx->vm;
	ObjString *string = ALLOCATE_OBJ(vmCtx, ObjString, OBJ_STRING);
	string->string.length = length;
	string->string.chars = chars;
	string->hash = hash;
	push(vmCtx, OBJ_VAL(string));
	tableSet(vmCtx, &vm->strings, string, NIL_VAL);
	pop(vm);
	return string;
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

ObjStringPair *copyStrings(VMCtx *vmCtx,
						   const char *chars1, int len1, const char *chars2, int len2) {
	VM *vm = &vmCtx->vm;
	ObjStringPair *pair = ALLOCATE_OBJ(vmCtx, ObjStringPair, OBJ_STRINGPAIR);
	push(vmCtx, OBJ_VAL(pair));
	pair->str1 = copyString(vmCtx, chars1, len1);
	pair->str2 = copyString(vmCtx, chars2, len2);
	pair->hash = pair->str1->hash + pair->str2->hash;
	pop(vm);
	return pair;
}

void initHeapString(VMCtx *vmCtx, HeapCString *str) {
	initHeapStringWithSize(vmCtx, str, 8);
}

void initHeapStringWithSize(VMCtx *vmCtx, HeapCString *str, int initialCapacity) {
	str->chars = ALLOCATE(vmCtx, char, initialCapacity);
	str->chars[0] = '\0';
	str->length = 0;
	str->capacity = initialCapacity;
}

void freeHeapString(VMCtx *vmCtx, HeapCString *str) {
	FREE(vmCtx, char, str->chars);
	str->chars = NULL;
}

void addHeapStringFmt(VMCtx *vmCtx, HeapCString *string, const char *format, ...) {
	va_list args;
	va_start(args, format);
	addHeapStringVFmt(vmCtx, string, format, args);
	va_end(args);
}

void addHeapStringVFmt(VMCtx *vmCtx, HeapCString *string, const char *format, va_list ap) {
	int available = string->capacity - string->length - 1;
	va_list apCopy;
	va_copy(apCopy, ap);

	int required = vsnprintf(string->chars + string->length, available, format, apCopy);
	va_end(apCopy);

	if (required <= available) {
		string->length += required;
		return;
	}

	int requiredCapacity = string->length + required + 1;
	int newCapacity = GROW_CAPACITY(string->capacity);
	newCapacity = (newCapacity < requiredCapacity) ?  requiredCapacity : newCapacity;
	string->chars = GROW_ARRAY(vmCtx, char, string->chars, string->capacity, newCapacity);
	string->capacity = newCapacity;

	available = string->capacity - string->length;
	va_copy(apCopy, ap);
	required = vsnprintf(string->chars + string->length, available, format, apCopy);
	va_end(apCopy);
	string->length += required;
}

char *reserveHeapString(VMCtx *vmCtx, HeapCString *string, int len) {
	int available = string->capacity - string->length - 1;
	int required = len + 1;

	if (required > available) {
		int requiredCapacity = string->length + required + 1;
		int newCapacity = GROW_CAPACITY(string->capacity);
		newCapacity = (newCapacity < requiredCapacity) ?  requiredCapacity : newCapacity;
		string->chars = GROW_ARRAY(vmCtx, char, string->chars, string->capacity, newCapacity);
		string->capacity = newCapacity;
	}

	int oldLen =string->length;
	string->length += len;
	string->chars[string->length] = '\0';
	return string->chars + oldLen;
}

void addHeapString(VMCtx *vmCtx, HeapCString *string, const char *str, int len) {
	char *buffer = reserveHeapString(vmCtx, string, len);
	memcpy(buffer, str, len);
	return;
}

ObjUpvalue *newUpvalue(VMCtx *vmCtx, Value *slot) {
	ObjUpvalue *upvalue = ALLOCATE_OBJ(vmCtx, ObjUpvalue, OBJ_UPVALUE);
	upvalue->closed = NIL_VAL;
	upvalue->location = slot;
	upvalue->next = NULL;
	return upvalue;
}

ObjArray *newArray(VMCtx *vmCtx, int initialSize, ObjType objType) {
	assert((objType == OBJ_ARRAY) || (objType == OBJ_TUPLE));
	VM *vm = &vmCtx->vm;

	ObjArray *array = ALLOCATE_OBJ(vmCtx, ObjArray, objType);
	if (initialSize <= 0) {
		array->items = NULL;
		array->capacity = 0;
	} else {
		push(vmCtx, OBJ_VAL(array));
		array->items = GROW_ARRAY(vmCtx, Value, NULL, 0, initialSize);
		pop(vm);
		array->capacity = initialSize;
	}
	array->size = 0;
	return array;
}

void appendToArray(VMCtx *vmCtx, ObjArray *array, Value value) {
	if (array->capacity < array->size + 1) {
		int oldCapacity = array->capacity;
		array->capacity = GROW_CAPACITY(oldCapacity);
		array->items = GROW_ARRAY(vmCtx, Value, array->items, oldCapacity, array->capacity);
	}
	array->items[array->size] = value;
	array->size++;
	return;
}

bool isValidArrayIndex(ObjArray *array, int index) {
	return !((index < 0) || (index > array->size - 1));
}

Value arrayAt(ObjArray *array, int index) {
	return array->items[index];
}

Value arrayAtSafe(ObjArray *array, int index) {
	if (isValidArrayIndex(array, index))
		return array->items[index];
	return NIL_VAL;
}

void arraySet(ObjArray *array, int index, Value value) {
	array->items[index] = value;
}

ObjMap *newMap(VMCtx *vmCtx) {
	ObjMap *map = ALLOCATE_OBJ(vmCtx, ObjMap, OBJ_MAP);
	initValueTable(&map->items);
	return map;
}

static void printFunction(ObjFunction *function, const char *wb, const char *we) {
	if (function->name == NULL) {
		printf("%sscript%s", wb, we);
		return;
	}
	printf("%sfn %s%s", wb, function->name->string.chars, we);
}

static void printMethod(Obj *method) {
	switch (method->type) {
		case OBJ_CLOSURE:
			printFunction(((ObjClosure *)method)->function, "<<", ">>");
			break;
		case OBJ_NATIVE_CLOSURE:
			printf("<<native fn>>");
			break;
		case OBJ_FUNCTION:
			printFunction((ObjFunction *)method, "<", ">");
			break;
		case OBJ_NATIVE:
			printf("<native fn>");
			break;
		default:
			break;
	}
}

static void printArray(ObjArray *array, const char *b, const char *e) {
	printf("%s", b);
	for (int i = 0; i < array->size - 1; i++) {
		printValue(array->items[i]);
		printf(", ");
	}
	if (array->size != 0)
		printValue(array->items[array->size - 1]);
	printf("%s", e);
}

static void printMap(ObjMap *map) {
	bool first = true;
	printf("{");
	for (int i = 0; i < map->items.capacity; i++) {
		if (!IS_NIL(map->items.entries[i].key)) {
			if (!first)
				printf(", ");
			first = false;
			printValue(map->items.entries[i].key);
			printf(" = ");
			printValue(map->items.entries[i].value);
		}
	}
	printf("}");
}

void printObject(Value value) {
	switch (OBJ_TYPE(value)) {
		case OBJ_MAP:
			printMap(AS_MAP(value));
			break;
		case OBJ_ARRAY:
			printArray(AS_ARRAY(value), "[", "]");
			break;
		case OBJ_TUPLE:
			printArray(AS_ARRAY(value), "<", ">");
			break;
		case OBJ_BOUND_METHOD:
			printMethod(AS_BOUND_METHOD(value)->method);
			break;
		case OBJ_CLASS:
			printf("%s", AS_CLASS(value)->name->string.chars);
			break;
		case OBJ_CLOSURE:
			printFunction(AS_CLOSURE(value)->function, "<<", ">>");
			break;
		case OBJ_NATIVE_CLOSURE:
			printf("<<native fn>>");
			break;
		case OBJ_FUNCTION:
			printFunction(AS_FUNCTION(value), "<", ">");
			break;
		case OBJ_INSTANCE:
			printf("%s instance", AS_INSTANCE(value)->clazz->name->string.chars);
			break;
		case OBJ_NATIVE:
			printf("<native fn>");
			break;
		case OBJ_STRING:
			printf("'%s'", AS_CSTRING(value));
			break;
		case OBJ_STRINGPAIR:
			printf("'%s', '%s'",
				   AS_STRINGPAIR(value)->str1->string.chars,
				   AS_STRINGPAIR(value)->str2->string.chars);
			break;
		case OBJ_UPVALUE:
			printf("upvalue");
			break;
	}
}
