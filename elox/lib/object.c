// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/memory.h"
#include "elox/object.h"
#include "elox/table.h"
#include "elox/value.h"
#include "elox/state.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>

#define ALLOCATE_OBJ(vmctx, type, objectType) \
	(type *)allocateObject(vmctx, sizeof(type), objectType)

static Obj *allocateObject(VMCtx *vmCtx, size_t size, ObjType type) {
	VM *vm = &vmCtx->vm;

	Obj *object = (Obj *)reallocate(vmCtx, NULL, 0, size);
	object->type = type;
	object->isMarked = false;
	object->next = vm->objects;
	vm->objects = object;

#ifdef ELOX_DEBUG_LOG_GC
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%p allocate %zu for %d\n", (void*)object, size, type);
#endif

	return object;
}

ObjBoundMethod *newBoundMethod(VMCtx *vmCtx,Value receiver, ObjMethod *method) {
	ObjBoundMethod *bound = ALLOCATE_OBJ(vmCtx, ObjBoundMethod, OBJ_BOUND_METHOD);
	bound->receiver = receiver;
	bound->method = method->callable;
	return bound;
}

ObjMethod *newMethod(VMCtx *vmCtx, ObjClass *clazz, Obj *callable) {
	ObjMethod *method = ALLOCATE_OBJ(vmCtx, ObjMethod, OBJ_METHOD);
	method->clazz = clazz;
	method->callable = callable;
	return method;
}

ObjClass *newClass(VMCtx *vmCtx, ObjString *name) {
	VM *vm = &vmCtx->vm;

	ObjString *className = name;

	if (name == NULL) {
		HeapCString ret;
		initHeapStringWithSize(vmCtx, &ret, 16);
		heapStringAddFmt(vmCtx, &ret, "Class_%lu", stc64_rand(&vm->prng) & 0xFFFFFFFF);
		className = takeString(vmCtx, ret.chars, ret.length, ret.capacity);
		push(vm, OBJ_VAL(className));
	}

	ObjClass *clazz = ALLOCATE_OBJ(vmCtx, ObjClass, OBJ_CLASS);
	clazz->baseId = nextPrime(&vm->primeGen);
	clazz->name = className;
	if (name == NULL)
		pop(vm);
	clazz->initializer = NIL_VAL;
	clazz->hashCode = NULL;
	clazz->equals = NULL;
	clazz->super = NIL_VAL;
	initTable(&clazz->fields);
	initTable(&clazz->methods);
	initTable(&clazz->statics);
	initValueArray(&clazz->staticValues);
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

ObjNativeClosure *newNativeClosure(VMCtx *vmCtx, NativeClosureFn function,
								   uint16_t arity, uint8_t numUpvalues) {
	Value *upvalues = ALLOCATE(vmCtx, Value, numUpvalues);
	for (int i = 0; i < numUpvalues; i++)
		upvalues[i] = NIL_VAL;

	ObjNativeClosure *closure = ALLOCATE_OBJ(vmCtx, ObjNativeClosure, OBJ_NATIVE_CLOSURE);
	closure->function = function;
	closure->upvalues = upvalues;
	closure->upvalueCount = numUpvalues;
	closure->defaultArgs = NULL;
	closure->arity = arity;
	if (arity > 0) {
		closure->defaultArgs = ALLOCATE(vmCtx, Value, arity);
		for (uint16_t i = 0; i < arity; i++)
			closure->defaultArgs[i] = NIL_VAL;
	}
	return closure;
}

ObjFunction *newFunction(VMCtx *vmCtx) {
	ObjFunction *function = ALLOCATE_OBJ(vmCtx, ObjFunction, OBJ_FUNCTION);
	function->arity = 0;
	function->maxArgs = 0;
	function->defaultArgs = NULL;
	function->upvalueCount = 0;
	function->name = NULL;
	function->parentClass = NULL;
	initChunk(&function->chunk);
	return function;
}

ObjInstance *newInstance(VMCtx *vmCtx, ObjClass *clazz) {
	VM *vm = &vmCtx->vm;
	ObjInstance *instance = ALLOCATE_OBJ(vmCtx, ObjInstance, OBJ_INSTANCE);
	push(vm, OBJ_VAL(instance));
	instance->clazz = clazz;
	initEmptyValueArray(vmCtx, &instance->fields, clazz->fields.count);
	pop(vm);
	instance->identityHash = stc64_rand(&vm->prng) & 0xFFFFFFFF;
	instance->flags =
			INST_HAS_HASHCODE * (clazz->hashCode != NULL) |
			INST_HAS_EQUALS * (clazz->equals != NULL);
	return instance;
}

ObjNative *newNative(VMCtx *vmCtx, NativeFn function, uint16_t arity) {
	VM *vm = &vmCtx->vm;

	ObjNative *native = ALLOCATE_OBJ(vmCtx, ObjNative, OBJ_NATIVE);
	native->function = function;
	native->arity = arity;
	native->defaultArgs = NULL;
	if (arity > 0) {
		push(vm, OBJ_VAL(native));
		native->defaultArgs = ALLOCATE(vmCtx, Value, arity);
		pop(vm);
		for (uint16_t i = 0; i < arity; i++)
			native->defaultArgs[i] = NIL_VAL;
	}
	return native;
}

ObjNative *addNativeMethod(VMCtx *vmCtx, ObjClass *clazz, const char *name,
						   NativeFn method, uint16_t arity, bool hasVarargs) {
	VM *vm = &vmCtx->vm;
	ObjString *methodName = copyString(vmCtx, (const uint8_t *)name, strlen(name));
	pushTemp(vmCtx, OBJ_VAL(methodName));
	ObjNative *nativeObj = newNative(vmCtx, method, arity);
	pushTemp(vmCtx, OBJ_VAL(nativeObj));
	if (methodName == clazz->name)
		clazz->initializer = OBJ_VAL(nativeObj);
	else {
		ObjMethod *method = newMethod(vmCtx, clazz, (Obj *)nativeObj);
		pushTemp(vmCtx, OBJ_VAL(method));
		tableSet(vmCtx, &clazz->methods, methodName, OBJ_VAL(method));
		popTemp(vmCtx);
		if (methodName == vm->builtins.hashCodeString)
			clazz->hashCode = method;
		else if (methodName == vm->builtins.equalsString)
			clazz->equals = method;
	}
	popTempN(vmCtx, 2);
	nativeObj->arity = arity;
	nativeObj->maxArgs = hasVarargs ? 255 : arity;
	return nativeObj;
}

int addClassField(VMCtx *vmCtx, ObjClass *clazz, const char *name) {
	VM *vm = &vmCtx->vm;
	ObjString *fieldName = copyString(vmCtx, (const uint8_t*)name, strlen(name));
	push(vm, OBJ_VAL(fieldName));
	int index = clazz->fields.count;
	tableSet(vmCtx, &clazz->fields, fieldName, NUMBER_VAL(index));
	pop(vm);
	return index;
}

static ObjString *allocateString(VMCtx *vmCtx, uint8_t *chars, int length, uint32_t hash) {
	VM *vm = &vmCtx->vm;
	ObjString *string = ALLOCATE_OBJ(vmCtx, ObjString, OBJ_STRING);
	string->string.length = length;
	string->string.chars = chars;
	string->hash = hash;
	push(vm, OBJ_VAL(string));
	tableSet(vmCtx, &vm->strings, string, NIL_VAL);
	pop(vm);
	return string;
}

ObjString *takeString(VMCtx *vmCtx, uint8_t *chars, int length, int capacity) {
	VM *vm = &vmCtx->vm;

	uint32_t hash = hashString(chars, length);
	ObjString *interned = tableFindString(&vm->strings, chars, length, hash);
	if (interned != NULL) {
		FREE_ARRAY(vmCtx, char, chars, capacity);
		return interned;
	}
	return allocateString(vmCtx, chars, length, hash);
}

ObjString *copyString(VMCtx *vmCtx, const uint8_t *chars, int32_t length) {
	VM *vm = &vmCtx->vm;
	uint32_t hash = hashString(chars, length);
	ObjString *interned = tableFindString(&vm->strings, chars, length, hash);
	if (interned != NULL)
		return interned;
	uint8_t *heapChars = ALLOCATE(vmCtx, uint8_t, length + 1);
	memcpy(heapChars, chars, length);
	heapChars[length] = '\0';
	return allocateString(vmCtx, heapChars, length, hash);
}

ObjStringPair *copyStrings(VMCtx *vmCtx,
						   const uint8_t *chars1, int len1, const uint8_t *chars2, int len2) {
	VM *vm = &vmCtx->vm;
	ObjStringPair *pair = ALLOCATE_OBJ(vmCtx, ObjStringPair, OBJ_STRINGPAIR);
	pair->str1 = NULL;
	pair->str2 = NULL;
	push(vm, OBJ_VAL(pair));
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
	str->chars = ALLOCATE(vmCtx, uint8_t, initialCapacity);
	str->chars[0] = '\0';
	str->length = 0;
	str->capacity = initialCapacity;
}

void freeHeapString(VMCtx *vmCtx, HeapCString *str) {
	FREE(vmCtx, char, str->chars);
	str->chars = NULL;
}

void heapStringAddFmt(VMCtx *vmCtx, HeapCString *string, const char *format, ...) {
	va_list args;
	va_start(args, format);
	heapStringAddVFmt(vmCtx, string, format, args);
	va_end(args);
}

void heapStringAddVFmt(VMCtx *vmCtx, HeapCString *string, const char *format, va_list ap) {
	int available = string->capacity - string->length - 1;
	va_list apCopy;
	va_copy(apCopy, ap);

	int required = vsnprintf((char *)string->chars + string->length, available, format, apCopy);
	va_end(apCopy);

	if (required <= available) {
		string->length += required;
		return;
	}

	int requiredCapacity = string->length + required + 1;
	int newCapacity = GROW_CAPACITY(string->capacity);
	newCapacity = (newCapacity < requiredCapacity) ?  requiredCapacity : newCapacity;
	string->chars = GROW_ARRAY(vmCtx, uint8_t, string->chars, string->capacity, newCapacity);
	string->capacity = newCapacity;

	available = string->capacity - string->length;
	va_copy(apCopy, ap);
	required = vsnprintf((char *)string->chars + string->length, available, format, apCopy);
	va_end(apCopy);
	string->length += required;
}

uint8_t *reserveHeapString(VMCtx *vmCtx, HeapCString *string, int len) {
	int available = string->capacity - string->length - 1;
	int required = len + 1;

	if (required > available) {
		int requiredCapacity = string->length + required + 1;
		int newCapacity = GROW_CAPACITY(string->capacity);
		newCapacity = (newCapacity < requiredCapacity) ?  requiredCapacity : newCapacity;
		string->chars = GROW_ARRAY(vmCtx, uint8_t, string->chars, string->capacity, newCapacity);
		string->capacity = newCapacity;
	}

	int oldLen =string->length;
	string->length += len;
	string->chars[string->length] = '\0';
	return string->chars + oldLen;
}

void heapStringAddString(VMCtx *vmCtx, HeapCString *string, const uint8_t *str, int len) {
	uint8_t *buffer = reserveHeapString(vmCtx, string, len);
	memcpy(buffer, str, len);
}

void heapStringAddChar(VMCtx *vmCtx, HeapCString *string, uint8_t ch) {
	uint8_t *buffer = reserveHeapString(vmCtx, string, 1);
	*buffer = ch;
}

ObjUpvalue *newUpvalue(VMCtx *vmCtx, Value *slot) {
	ObjUpvalue *upvalue = ALLOCATE_OBJ(vmCtx, ObjUpvalue, OBJ_UPVALUE);
	upvalue->closed = NIL_VAL;
	upvalue->location = slot;
#ifdef ELOX_DEBUG_TRACE_EXECUTION
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%p <<<  (", upvalue);
	printValue(vmCtx, ELOX_IO_DEBUG, *slot);
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, ")\n");
#endif
	upvalue->next = NULL;
	return upvalue;
}

ObjArray *newArray(VMCtx *vmCtx, int initialSize, ObjType objType) {
	assert((objType == OBJ_ARRAY) || (objType == OBJ_TUPLE));
	VM *vm = &vmCtx->vm;

	ObjArray *array = ALLOCATE_OBJ(vmCtx, ObjArray, objType);
	array->size = 0;
	array->modCount = 0;
	if (initialSize <= 0) {
		array->items = NULL;
		array->capacity = 0;
	} else {
		push(vm, OBJ_VAL(array));
		array->items = GROW_ARRAY(vmCtx, Value, NULL, 0, initialSize);
		pop(vm);
		array->capacity = initialSize;
	}
	return array;
}

void appendToArray(VMCtx *vmCtx, ObjArray *array, Value value) {
	array->modCount++;
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

Value arrayAtSafe(VMCtx *vmCtx, ObjArray *array, int32_t index) {
	int32_t realIndex = (index < 0) ? array->size + index : index;

	if (ELOX_UNLIKELY((realIndex < 0) || (realIndex > array->size - 1)))
		return runtimeError(vmCtx, "Array index out of range");

	return array->items[realIndex];
}

void arraySet(ObjArray *array, int index, Value value) {
	array->items[index] = value;
}

ObjMap *newMap(VMCtx *vmCtx) {
	ObjMap *map = ALLOCATE_OBJ(vmCtx, ObjMap, OBJ_MAP);
	initValueTable(&map->items);
	return map;
}

static void printFunction(VMCtx *vmCtx, EloxIOStream stream,
						  ObjFunction *function, const char *wb, const char *we) {
	if (function->name == NULL) {
		eloxPrintf(vmCtx, stream, "%sscript%s", wb, we);
		return;
	}
	eloxPrintf(vmCtx, stream, "%sfn %s%s", wb, function->name->string.chars, we);
}

static void printMethod(VMCtx *vmCtx, EloxIOStream stream, Obj *method) {
	switch (method->type) {
		case OBJ_CLOSURE:
			printFunction(vmCtx, stream, ((ObjClosure *)method)->function, "<<", ">>");
			break;
		case OBJ_NATIVE_CLOSURE:
			ELOX_WRITE(vmCtx, stream, "<<native fn>>");
			break;
		case OBJ_FUNCTION:
			printFunction(vmCtx, stream, (ObjFunction *)method, "<", ">");
			break;
		case OBJ_NATIVE:
			eloxPrintf(vmCtx, stream, "<native fn %p>", ((ObjNative *)method)->function);
			break;
		default:
			break;
	}
}

static void printArray(VMCtx *vmCtx, EloxIOStream stream, ObjArray *array, const char *b, const char *e) {
	eloxPrintf(vmCtx, stream, "%s", b);
	for (int i = 0; i < array->size - 1; i++) {
		printValue(vmCtx, stream, array->items[i]);
		ELOX_WRITE(vmCtx, stream, ", ");
	}
	if (array->size != 0)
		printValue(vmCtx, stream, array->items[array->size - 1]);
	eloxPrintf(vmCtx, stream, "%s", e);
}

static void printMap(VMCtx *vmCtx, EloxIOStream stream, ObjMap *map) {
	bool first = true;
	ELOX_WRITE(vmCtx, stream, "{");
	for (int i = 0; i < map->items.entriesCount; i++) {
		if (!IS_UNDEFINED(map->items.entries[i].key)) {
			if (!first)
				ELOX_WRITE(vmCtx, stream, ", ");
			first = false;
			printValue(vmCtx, stream, map->items.entries[i].key);
			ELOX_WRITE(vmCtx, stream, " = ");
			printValue(vmCtx, stream, map->items.entries[i].value);
		}
	}
	ELOX_WRITE(vmCtx, stream, "}");
}

void printValueObject(VMCtx *vmCtx, EloxIOStream stream, Value value) {
	printObject(vmCtx, stream, AS_OBJ(value));
}

void printObject(VMCtx *vmCtx, EloxIOStream stream, Obj *obj) {
	switch (obj->type) {
		case OBJ_MAP:
			printMap(vmCtx, stream, OBJ_AS_MAP(obj));
			break;
		case OBJ_ARRAY:
			printArray(vmCtx, stream, OBJ_AS_ARRAY(obj), "[", "]");
			break;
		case OBJ_TUPLE:
			printArray(vmCtx, stream, OBJ_AS_ARRAY(obj), "<", ">");
			break;
		case OBJ_BOUND_METHOD:
			printMethod(vmCtx, stream, OBJ_AS_BOUND_METHOD(obj)->method);
			break;
		case OBJ_METHOD:
			// TODO: print class
			ELOX_WRITE(vmCtx, stream, "M");
			printMethod(vmCtx, stream, OBJ_AS_METHOD(obj)->callable);
			break;
		case OBJ_CLASS:
			eloxPrintf(vmCtx, stream, "class %s", OBJ_AS_CLASS(obj)->name->string.chars);
			break;
		case OBJ_CLOSURE:
			printFunction(vmCtx, stream, OBJ_AS_CLOSURE(obj)->function, "#", "#");
			break;
		case OBJ_NATIVE_CLOSURE:
			ELOX_WRITE(vmCtx, stream, "#<native fn>#");
			break;
		case OBJ_FUNCTION:
			printFunction(vmCtx, stream, OBJ_AS_FUNCTION(obj), "<", ">");
			break;
		case OBJ_INSTANCE:
			eloxPrintf(vmCtx, stream, "%s instance",
					   OBJ_AS_INSTANCE(obj)->clazz->name->string.chars);
			break;
		case OBJ_NATIVE:
			eloxPrintf(vmCtx, stream, "<native fn %p>", OBJ_AS_NATIVE(obj)->function);
			break;
		case OBJ_STRING:
			eloxPrintf(vmCtx, stream, "'%s'", OBJ_AS_CSTRING(obj));
			break;
		case OBJ_STRINGPAIR: {
			ObjStringPair *pair = OBJ_AS_STRINGPAIR(obj);
			eloxPrintf(vmCtx, stream, "'%s', '%s'",
					   pair->str1 ? (const char *)pair->str1->string.chars : "<null>",
					   pair->str2 ? (const char *)pair->str2->string.chars : "<null>");
			break;
		}
		case OBJ_UPVALUE:
			ELOX_WRITE(vmCtx, stream, "upvalue");
			break;
	}
}
