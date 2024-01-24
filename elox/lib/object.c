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
	VMHeap *heap = vm->heap;

	Obj *object = (Obj *)reallocate(vmCtx, NULL, 0, size);
	if (ELOX_UNLIKELY(object == NULL))
		return NULL;
	object->type = type;
	object->markers = heap->initialMarkers;
	object->next = heap->objects;
	heap->objects = object;

#ifdef ELOX_DEBUG_LOG_GC
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%p allocate %zu for %d\n", (void*)object, size, type);
#endif

	return object;
}

ObjBoundMethod *newBoundMethod(VMCtx *vmCtx,Value receiver, ObjMethod *method) {
	ObjBoundMethod *bound = ALLOCATE_OBJ(vmCtx, ObjBoundMethod, OBJ_BOUND_METHOD);
	if (ELOX_UNLIKELY(bound == NULL))
		return NULL;
	bound->receiver = receiver;
	bound->method = method->callable;
	return bound;
}

ObjMethod *newMethod(VMCtx *vmCtx, ObjClass *clazz, Obj *callable) {
	ObjMethod *method = ALLOCATE_OBJ(vmCtx, ObjMethod, OBJ_METHOD);
	if (ELOX_UNLIKELY(method == NULL))
		return NULL;
	method->clazz = clazz;
	method->callable = callable;
	return method;
}

ObjClass *newClass(VMCtx *vmCtx, ObjString *name) {
	VM *vm = &vmCtx->vm;

	ObjString *className = name;

	if (name == NULL) {
		HeapCString ret;
		bool res = initHeapStringWithSize(vmCtx, &ret, 16);
		if (ELOX_UNLIKELY(!res))
			return NULL;
		heapStringAddFmt(vmCtx, &ret, "Class_%lu", stc64_rand(&vm->prng) & 0xFFFFFFFF);
		className = takeString(vmCtx, ret.chars, ret.length, ret.capacity);
		if (ELOX_UNLIKELY(className == NULL))
			return NULL;
		push(vm, OBJ_VAL(className));
	}

	ObjClass *clazz = ALLOCATE_OBJ(vmCtx, ObjClass, OBJ_CLASS);
	if (ELOX_UNLIKELY(clazz == NULL))
		return NULL;
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
	if (ELOX_UNLIKELY(upvalues == NULL))
		return NULL;
	for (int i = 0; i < function->upvalueCount; i++)
		upvalues[i] = NULL;

	ObjClosure *closure = ALLOCATE_OBJ(vmCtx, ObjClosure, OBJ_CLOSURE);
	if (ELOX_UNLIKELY(closure == NULL)) {
		FREE_ARRAY(vmCtx, ObjUpvalue *, upvalues, function->upvalueCount);
		return NULL;
	}
	closure->function = function;
	closure->upvalues = upvalues;
	closure->upvalueCount = function->upvalueCount;
	return closure;
}

ObjNativeClosure *newNativeClosure(VMCtx *vmCtx, NativeClosureFn function,
								   uint16_t arity, uint8_t numUpvalues) {
	ObjNativeClosure *closure = NULL;
	Value *upvalues = ALLOCATE(vmCtx, Value, numUpvalues);
	if (ELOX_UNLIKELY(upvalues == NULL))
		goto cleanup;
	for (int i = 0; i < numUpvalues; i++)
		upvalues[i] = NIL_VAL;

	closure = ALLOCATE_OBJ(vmCtx, ObjNativeClosure, OBJ_NATIVE_CLOSURE);
	if (ELOX_UNLIKELY(closure == NULL))
		goto cleanup;
	closure->function = function;
	closure->upvalues = upvalues;
	closure->upvalueCount = numUpvalues;
	closure->defaultArgs = NULL;
	closure->arity = arity;
	if (arity > 0) {
		closure->defaultArgs = ALLOCATE(vmCtx, Value, arity);
		if (ELOX_UNLIKELY(closure->defaultArgs == NULL))
			goto cleanup;
		for (uint16_t i = 0; i < arity; i++)
			closure->defaultArgs[i] = NIL_VAL;
	}
	return closure;

cleanup:
	if (upvalues != NULL)
		FREE_ARRAY(vmCtx, Value, upvalues, numUpvalues);
	if (closure != NULL)
		closure->upvalues = NULL;

	return NULL;
}

ObjFunction *newFunction(VMCtx *vmCtx) {
	ObjFunction *function = ALLOCATE_OBJ(vmCtx, ObjFunction, OBJ_FUNCTION);
	if (ELOX_UNLIKELY(function == NULL))
		return NULL;
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

	ObjInstance *ret = NULL;
	PHandle protectedInstance = PHANDLE_INITIALIZER;

	ObjInstance *instance = ALLOCATE_OBJ(vmCtx, ObjInstance, OBJ_INSTANCE);
	if (ELOX_UNLIKELY(instance == NULL))
		return NULL;
	protectedInstance = protectObj((Obj *)instance);
	instance->clazz = clazz;
	bool init = initEmptyValueArray(vmCtx, &instance->fields, clazz->fields.count);
	if (ELOX_UNLIKELY(!init))
		goto cleanup;

	instance->identityHash = stc64_rand(&vm->prng) & 0xFFFFFFFF;
	instance->flags =
			INST_HAS_HASHCODE * (clazz->hashCode != NULL) |
			INST_HAS_EQUALS * (clazz->equals != NULL);

	ret = instance;

cleanup:
	unprotectObj(protectedInstance);

	return ret;
}

ObjNative *newNative(VMCtx *vmCtx, NativeFn function, uint16_t arity) {
	PHandle protectedNative = PHANDLE_INITIALIZER;

	ObjNative *native = ALLOCATE_OBJ(vmCtx, ObjNative, OBJ_NATIVE);
	if (ELOX_UNLIKELY(native == NULL))
		return NULL;
	native->function = function;
	native->arity = arity;
	native->defaultArgs = NULL;
	if (arity > 0) {
		protectedNative = protectObj((Obj *)native);
		native->defaultArgs = ALLOCATE(vmCtx, Value, arity);
		if (ELOX_UNLIKELY(native->defaultArgs == NULL))
			goto cleanup;
		for (uint16_t i = 0; i < arity; i++)
			native->defaultArgs[i] = NIL_VAL;
	}
	return native;

cleanup:
	unprotectObj(protectedNative);

	return NULL;
}

ObjNative *addNativeMethod(VMCtx *vmCtx, ObjClass *clazz, const char *name,
						   NativeFn method, uint16_t arity, bool hasVarargs,
						   ErrorMsg *errorMsg) {
	VM *vm = &vmCtx->vm;

	ObjNative *ret = NULL;
	PHandle protectedMethodName = PHANDLE_INITIALIZER;
	PHandle protectedNative = PHANDLE_INITIALIZER;
	PHandle protectedMethod = PHANDLE_INITIALIZER;

	ObjString *methodName = copyString(vmCtx, (const uint8_t *)name, strlen(name));
	ELOX_IF_COND_RAISE_MSG_GOTO((methodName == NULL), errorMsg, "Out of memory", cleanup);
	protectedMethodName = protectObj((Obj *)methodName);
	ObjNative *nativeObj = newNative(vmCtx, method, arity);
	ELOX_IF_COND_RAISE_MSG_GOTO((nativeObj == NULL), errorMsg, "Out of memory", cleanup);
	protectedNative = protectObj((Obj *)nativeObj);
	if (methodName == clazz->name)
		clazz->initializer = OBJ_VAL(nativeObj);
	else {
		ObjMethod *method = newMethod(vmCtx, clazz, (Obj *)nativeObj);
		ELOX_IF_COND_RAISE_MSG_GOTO((method == NULL), errorMsg, "Out of memory", cleanup);
		protectedMethod = protectObj((Obj *)method);
		Error error = ERROR_INITIALIZER(vmCtx);
		tableSet(&clazz->methods, methodName, OBJ_VAL(method), &error);
		if (ELOX_UNLIKELY(error.raised)) {
			pop(vm); // discard error
			ELOX_RAISE_MSG(errorMsg, "Out of memory");
			goto cleanup;
		}
		if (methodName == vm->builtins.hashCodeString)
			clazz->hashCode = method;
		else if (methodName == vm->builtins.equalsString)
			clazz->equals = method;
	}
	nativeObj->arity = arity;
	nativeObj->maxArgs = hasVarargs ? 255 : arity;

	ret = nativeObj;

cleanup:
	unprotectObj(protectedMethod);
	unprotectObj(protectedMethodName);
	unprotectObj(protectedNative);
	return ret;
}

int addClassField(VMCtx *vmCtx, ObjClass *clazz, const char *name, ErrorMsg *errorMsg) {
	VM *vm = &vmCtx->vm;

	int ret = -1;
	PHandle protectedName = PHANDLE_INITIALIZER;

	ObjString *fieldName = copyString(vmCtx, (const uint8_t*)name, strlen(name));
	if (ELOX_UNLIKELY(fieldName == NULL)) {
		ELOX_RAISE_MSG(errorMsg, "Out of memory");
		goto cleanup;
	}
	protectedName = protectObj((Obj *)fieldName);
	int index = clazz->fields.count;
	Error error = ERROR_INITIALIZER(vmCtx);
	tableSet(&clazz->fields, fieldName, NUMBER_VAL(index), &error);
	if (ELOX_UNLIKELY(error.raised)) {
		pop(vm); // discard error
		ELOX_RAISE_MSG(errorMsg, "Out of memory");
		goto cleanup;
	}
	ret = index;

cleanup:
	unprotectObj(protectedName);

	return ret;
}

static ObjString *allocateString(VMCtx *vmCtx, uint8_t *chars, int length, uint32_t hash) {
	VM *vm = &vmCtx->vm;

	ObjString *ret = NULL;
	PHandle protectedString = PHANDLE_INITIALIZER;

	ObjString *string = ALLOCATE_OBJ(vmCtx, ObjString, OBJ_STRING);
	if (ELOX_UNLIKELY(string == NULL))
		return NULL;
	string->string.length = length;
	string->string.chars = chars;
	string->hash = hash;

	protectedString = protectObj((Obj *)string);
	Error error = ERROR_INITIALIZER(vmCtx);
	tableSet(&vm->strings, string, NIL_VAL, &error);
	if (ELOX_UNLIKELY(error.raised)) {
		pop(vm); // discard error
		goto cleanup;
	}

	ret = string;

cleanup:
	unprotectObj(protectedString);

	return ret;
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
	if (ELOX_UNLIKELY(heapChars == NULL))
		return NULL;
	memcpy(heapChars, chars, length);
	heapChars[length] = '\0';
	return allocateString(vmCtx, heapChars, length, hash);
}

ObjStringPair *copyStrings(VMCtx *vmCtx,
						   const uint8_t *chars1, int len1, const uint8_t *chars2, int len2) {
	VM *vm = &vmCtx->vm;
	ObjStringPair *pair = ALLOCATE_OBJ(vmCtx, ObjStringPair, OBJ_STRINGPAIR);
	if (ELOX_UNLIKELY(pair == NULL))
		return NULL;
	pair->str1 = NULL;
	pair->str2 = NULL;
	push(vm, OBJ_VAL(pair));
	pair->str1 = copyString(vmCtx, chars1, len1);
	// TODO: fix error handling
	if (ELOX_UNLIKELY(pair->str1 == NULL))
		return NULL;
	pair->str2 = copyString(vmCtx, chars2, len2);
	if (ELOX_UNLIKELY(pair->str2 == NULL))
		return NULL;
	pair->hash = pair->str1->hash + pair->str2->hash;
	pop(vm);
	return pair;
}

bool initHeapString(VMCtx *vmCtx, HeapCString *str) {
	return initHeapStringWithSize(vmCtx, str, 8);
}

bool initHeapStringWithSize(VMCtx *vmCtx, HeapCString *str, int initialCapacity) {
	str->chars = ALLOCATE(vmCtx, uint8_t, initialCapacity);
	if (ELOX_UNLIKELY(str->chars == NULL))
		return false;
	str->chars[0] = '\0';
	str->length = 0;
	str->capacity = initialCapacity;
	return true;
}

void freeHeapString(VMCtx *vmCtx, HeapCString *str) {
	FREE(vmCtx, char, str->chars);
	str->chars = NULL;
}

bool heapStringAddFmt(VMCtx *vmCtx, HeapCString *string, const char *format, ...) {
	va_list args;
	va_start(args, format);
	bool ret = heapStringAddVFmt(vmCtx, string, format, args);
	va_end(args);
	return ret;
}

bool heapStringAddVFmt(VMCtx *vmCtx, HeapCString *string, const char *format, va_list ap) {
	int available = string->capacity - string->length - 1;
	va_list apCopy;
	va_copy(apCopy, ap);

	int required = vsnprintf((char *)string->chars + string->length, available, format, apCopy);
	va_end(apCopy);

	if (required <= available) {
		string->length += required;
		return true;
	}

	int requiredCapacity = string->length + required + 1;
	int newCapacity = GROW_CAPACITY(string->capacity);
	newCapacity = (newCapacity < requiredCapacity) ?  requiredCapacity : newCapacity;
	uint8_t *oldChars = string->chars;
	string->chars = GROW_ARRAY(vmCtx, uint8_t, string->chars, string->capacity, newCapacity);
	if (ELOX_UNLIKELY(string->chars == NULL)) {
		string->chars = oldChars;
		return false;
	}
	string->capacity = newCapacity;

	available = string->capacity - string->length;
	va_copy(apCopy, ap);
	required = vsnprintf((char *)string->chars + string->length, available, format, apCopy);
	va_end(apCopy);
	string->length += required;

	return true;
}

uint8_t *reserveHeapString(VMCtx *vmCtx, HeapCString *string, int len) {
	int available = string->capacity - string->length - 1;
	int required = len + 1;

	if (required > available) {
		int requiredCapacity = string->length + required + 1;
		int newCapacity = GROW_CAPACITY(string->capacity);
		newCapacity = (newCapacity < requiredCapacity) ?  requiredCapacity : newCapacity;
		uint8_t *oldChars = string->chars;
		string->chars = GROW_ARRAY(vmCtx, uint8_t, string->chars, string->capacity, newCapacity);
		if (ELOX_UNLIKELY(string->chars == NULL)) {
			string->chars = oldChars;
			return NULL;
		}
		string->capacity = newCapacity;
	}

	int oldLen =string->length;
	string->length += len;
	string->chars[string->length] = '\0';
	return string->chars + oldLen;
}

bool heapStringAddString(VMCtx *vmCtx, HeapCString *string, const uint8_t *str, int len) {
	uint8_t *buffer = reserveHeapString(vmCtx, string, len);
	if (ELOX_UNLIKELY(buffer == NULL))
		return false;
	memcpy(buffer, str, len);
	return true;
}

bool heapStringAddChar(VMCtx *vmCtx, HeapCString *string, uint8_t ch) {
	uint8_t *buffer = reserveHeapString(vmCtx, string, 1);
	if (ELOX_UNLIKELY(buffer == NULL))
		return false;
	*buffer = ch;
	return true;
}

ObjUpvalue *newUpvalue(VMCtx *vmCtx, Value *slot) {
	ObjUpvalue *upvalue = ALLOCATE_OBJ(vmCtx, ObjUpvalue, OBJ_UPVALUE);
	if (ELOX_UNLIKELY(upvalue == NULL))
		return NULL;
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
	if (ELOX_UNLIKELY(array == NULL))
		return NULL;
	array->size = 0;
	array->modCount = 0;
	if (initialSize <= 0) {
		array->items = NULL;
		array->capacity = 0;
	} else {
		push(vm, OBJ_VAL(array));
		array->items = GROW_ARRAY(vmCtx, Value, NULL, 0, initialSize);
		if (ELOX_UNLIKELY(array->items == NULL))
			return NULL;
		pop(vm);
		array->capacity = initialSize;
	}
	return array;
}

bool appendToArray(VMCtx *vmCtx, ObjArray *array, Value value) {
	array->modCount++;
	if (array->capacity < array->size + 1) {
		int oldCapacity = array->capacity;
		int newCapacity = GROW_CAPACITY(oldCapacity);
		Value *oldItems = array->items;
		array->items = GROW_ARRAY(vmCtx, Value, array->items, oldCapacity, array->capacity);
		if (ELOX_UNLIKELY(array->items == NULL)) {
			array->items = oldItems;
			return false;
		}
		array->capacity = newCapacity;
	}
	array->items[array->size] = value;
	array->size++;
	return true;
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
	if (ELOX_UNLIKELY(map == NULL))
		return NULL;
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
	for (int i = 0; i < map->items.fullCount; i++) {
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
