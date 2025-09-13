// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/memory.h"
#include "elox/object.h"
#include "elox/table.h"
#include "elox/value.h"
#include <elox/state.h>
#include <elox/Class.h>

#include <assert.h>
#include <string.h>
#include <stdio.h>

Obj *allocateObject(RunCtx *runCtx, size_t size, ObjType type) {
	VM *vm = runCtx->vmCtx->vm;
	VMHeap *heap = vm->heap;

	Obj *object = (Obj *)vmRealloc(runCtx, NULL, 0, size);
	if (ELOX_UNLIKELY(object == NULL))
		return NULL;
	setObjType(object, type);
	setObjMarkers(object, heap->initialMarkers);
	setObjNext(object, heap->objects);
	heap->objects = object;

#ifdef ELOX_DEBUG_LOG_GC
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%p allocate %zu for %d\n", (void*)object, size, type);
#endif

	return object;
}

ObjClosure *newClosure(RunCtx *runCtx, ObjFunction *function) {
	VMCtx * vmCtx = runCtx->vmCtx;

	ObjUpvalue **upvalues = ALLOCATE(runCtx, ObjUpvalue *, function->upvalueCount);
	if (ELOX_UNLIKELY(upvalues == NULL))
		return NULL;
	for (int i = 0; i < function->upvalueCount; i++)
		upvalues[i] = NULL;

	ObjClosure *closure = ALLOCATE_OBJ(runCtx, ObjClosure, OBJ_CLOSURE);
	if (ELOX_UNLIKELY(closure == NULL)) {
		FREE_ARRAY(vmCtx, ObjUpvalue *, upvalues, function->upvalueCount);
		return NULL;
	}
	closure->function = function;
	closure->upvalues = upvalues;
	closure->upvalueCount = function->upvalueCount;
	return closure;
}

ObjNativeClosure *newNativeClosure(RunCtx *runCtx, NativeClosureFn function,
								   uint16_t arity, uint8_t numUpvalues) {
	VMCtx * vmCtx = runCtx->vmCtx;

	ObjNativeClosure *closure = NULL;
	Value *upvalues = ALLOCATE(runCtx, Value, numUpvalues);
	if (ELOX_UNLIKELY(upvalues == NULL))
		goto cleanup;
	for (int i = 0; i < numUpvalues; i++)
		upvalues[i] = NIL_VAL;

	closure = ALLOCATE_OBJ(runCtx, ObjNativeClosure, OBJ_NATIVE_CLOSURE);
	if (ELOX_UNLIKELY(closure == NULL))
		goto cleanup;
	closure->function = function;
	closure->upvalues = upvalues;
	closure->upvalueCount = numUpvalues;
	closure->defaultArgs = NULL;
	closure->arity = arity;
	if (arity > 0) {
		closure->defaultArgs = ALLOCATE(runCtx, Value, arity);
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

ObjFunction *newFunction(RunCtx *runCtx, ObjString *fileName) {
	ObjFunction *function = ALLOCATE_OBJ(runCtx, ObjFunction, OBJ_FUNCTION);
	if (ELOX_UNLIKELY(function == NULL))
		return NULL;
	function->isMethod = false;
	function->arity = 0;
	function->maxArgs = 0;
	function->defaultArgs = NULL;
	function->upvalueCount = 0;
	function->name = NULL;
	function->parentClass = NULL;
	initChunk(&function->chunk, fileName);
	return function;
}

ObjNative *newNative(RunCtx *runCtx, NativeFn function, uint16_t arity) {
	ObjFiber *fiber = runCtx->activeFiber;

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	VMTemp protectedNative = TEMP_INITIALIZER;
	ObjNative *ret = NULL;

	ObjNative *native = ALLOCATE_OBJ(runCtx, ObjNative, OBJ_NATIVE);
	ELOX_CHECK_RET_VAL(native != NULL, NULL);

	native->function = function;
	native->arity = arity;
	native->defaultArgs = NULL;
	if (arity > 0) {
		pushTempVal(temps, &protectedNative, OBJ_VAL(native));
		native->defaultArgs = ALLOCATE(runCtx, Value, arity);
		ELOX_CHECK_GOTO(native->defaultArgs != NULL, cleanup);
		for (uint16_t i = 0; i < arity; i++)
			native->defaultArgs[i] = NIL_VAL;
	}

	ret = native;

cleanup:
	releaseTemps(&temps);

	return ret;
}

ObjString *internString(RunCtx *runCtx, const uint8_t *chars, int32_t length, EloxError *error) {
	if (ELOX_UNLIKELY(error->raised))
		return NULL;

	ObjString *str = copyString(runCtx, chars, length);
	if (ELOX_UNLIKELY(str == NULL))
		ELOX_RAISE(error, OOM(runCtx));
	return str;
}

static ObjString *allocateString(RunCtx *runCtx, uint8_t *chars, int length, uint32_t hash) {
	VM *vm = runCtx->vmCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	ObjString *ret = NULL;
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);

	ObjString *string = ALLOCATE_OBJ(runCtx, ObjString, OBJ_STRING);
	if (ELOX_UNLIKELY(string == NULL))
		return NULL;
	string->string.length = length;
	string->string.chars = chars;
	string->hash = hash;

	PUSH_TEMP(temps, protectedString, OBJ_VAL(string));
	EloxError error = ELOX_ERROR_INITIALIZER;
	size_t savedStack = saveStack(fiber);
	tableSet(runCtx, &vm->strings, string, NIL_VAL, &error);
	if (ELOX_UNLIKELY(error.raised)) {
		error.discardException(fiber, savedStack);
		goto cleanup;
	}

	ret = string;

cleanup:
	releaseTemps(&temps);

	return ret;
}

ObjString *takeString(RunCtx *runCtx, uint8_t *chars, int length, int capacity) {
	VMCtx *vmCtx = runCtx->vmCtx;
	VM *vm = vmCtx->vm;

	uint32_t hash = hashString(chars, length);
	ObjString *interned = tableFindString(&vm->strings, chars, length, hash);
	if (interned != NULL) {
		FREE_ARRAY(vmCtx, char, chars, capacity);
		return interned;
	}
	return allocateString(runCtx, chars, length, hash);
}

ObjString *copyString(RunCtx *runCtx, const uint8_t *chars, int32_t length) {
	VM *vm = runCtx->vmCtx->vm;

	uint32_t hash = hashString(chars, length);
	ObjString *interned = tableFindString(&vm->strings, chars, length, hash);
	if (interned != NULL)
		return interned;
	uint8_t *heapChars = ALLOCATE(runCtx, uint8_t, length + 1);
	if (ELOX_UNLIKELY(heapChars == NULL))
		return NULL;
	memcpy(heapChars, chars, length);
	heapChars[length] = '\0';
	return allocateString(runCtx, heapChars, length, hash);
}

ObjStringPair *copyStrings(RunCtx *runCtx,
						   const uint8_t *chars1, int len1, const uint8_t *chars2, int len2) {
	ObjFiber *fiber = runCtx->activeFiber;

	ObjStringPair *pair = ALLOCATE_OBJ(runCtx, ObjStringPair, OBJ_STRINGPAIR);
	if (ELOX_UNLIKELY(pair == NULL))
		return NULL;
	pair->str1 = NULL;
	pair->str2 = NULL;
	push(fiber, OBJ_VAL(pair));
	pair->str1 = copyString(runCtx, chars1, len1);
	// TODO: fix error handling
	if (ELOX_UNLIKELY(pair->str1 == NULL))
		return NULL;
	pair->str2 = copyString(runCtx, chars2, len2);
	if (ELOX_UNLIKELY(pair->str2 == NULL))
		return NULL;
	pair->hash = pair->str1->hash + pair->str2->hash;
	pop(fiber);
	return pair;
}

bool initHeapString(RunCtx *runCtx, HeapCString *str) {
	return initHeapStringWithSize(runCtx, str, 8);
}

bool initHeapStringWithSize(RunCtx *runCtx, HeapCString *str, int initialCapacity) {
	str->chars = ALLOCATE(runCtx, uint8_t, initialCapacity);
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

bool heapStringAddFmt(RunCtx *runCtx, HeapCString *string, const char *format, ...) {
	va_list args;
	va_start(args, format);
	bool ret = heapStringAddVFmt(runCtx, string, format, args);
	va_end(args);
	return ret;
}

bool heapStringAddVFmt(RunCtx *runCtx, HeapCString *string, const char *format, va_list ap) {
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
	string->chars = GROW_ARRAY(runCtx, uint8_t, string->chars, string->capacity, newCapacity);
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

uint8_t *reserveHeapString(RunCtx *runCtx, HeapCString *string, int len) {
	int available = string->capacity - string->length - 1;
	int required = len + 1;

	if (required > available) {
		int requiredCapacity = string->length + required + 1;
		int newCapacity = GROW_CAPACITY(string->capacity);
		newCapacity = (newCapacity < requiredCapacity) ?  requiredCapacity : newCapacity;
		uint8_t *oldChars = string->chars;
		string->chars = GROW_ARRAY(runCtx, uint8_t, string->chars, string->capacity, newCapacity);
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

bool heapStringAddString(RunCtx *runCtx, HeapCString *string, const uint8_t *str, int len) {
	uint8_t *buffer = reserveHeapString(runCtx, string, len);
	if (ELOX_UNLIKELY(buffer == NULL))
		return false;
	memcpy(buffer, str, len);
	return true;
}

bool heapStringAddChar(RunCtx *runCtx, HeapCString *string, uint8_t ch) {
	uint8_t *buffer = reserveHeapString(runCtx, string, 1);
	if (ELOX_UNLIKELY(buffer == NULL))
		return false;
	*buffer = ch;
	return true;
}

ObjUpvalue *newUpvalue(RunCtx *runCtx, Value *slot) {
	ObjUpvalue *upvalue = ALLOCATE_OBJ(runCtx, ObjUpvalue, OBJ_UPVALUE);
	if (ELOX_UNLIKELY(upvalue == NULL))
		return NULL;
	upvalue->closed = NIL_VAL;
	upvalue->location = slot;
	upvalue->next = NULL;
	return upvalue;
}

ObjArray *newArray(RunCtx *runCtx, int initialSize, ObjType objType) {
	assert((objType == OBJ_ARRAY) || (objType == OBJ_TUPLE));
	ObjFiber *fiber = runCtx->activeFiber;

	ObjArray *array = ALLOCATE_OBJ(runCtx, ObjArray, objType);
	if (ELOX_UNLIKELY(array == NULL))
		return NULL;
	array->size = 0;
	array->modCount = 0;
	if (initialSize <= 0) {
		array->items = NULL;
		array->capacity = 0;
	} else {
		push(fiber, OBJ_VAL(array));
		array->items = GROW_ARRAY(runCtx, Value, NULL, 0, initialSize);
		if (ELOX_UNLIKELY(array->items == NULL)) {
			pop(fiber);
			return NULL;
		}
		pop(fiber);
		array->capacity = initialSize;
	}
	return array;
}

ObjArray *newArrayFrom(RunCtx *runCtx, ValueArray from, ObjType objType) {
	assert((objType == OBJ_ARRAY) || (objType == OBJ_TUPLE));
	ObjFiber *fiber = runCtx->activeFiber;

	uint32_t numVal = from.count;
	ObjArray *array = ALLOCATE_OBJ(runCtx, ObjArray, objType);
	if (ELOX_UNLIKELY(array == NULL))
		return NULL;
	array->size = 0;
	array->modCount = 0;
	if (numVal == 0) {
		array->items = NULL;
		array->capacity = 0;
	} else {
		push(fiber, OBJ_VAL(array));
		array->items = GROW_ARRAY(runCtx, Value, NULL, 0, numVal);
		if (ELOX_UNLIKELY(array->items == NULL)) {
			pop(fiber);
			return NULL;
		}
		array->size = numVal;
		pop(fiber);
		memcpy(array->items, from.values, numVal * sizeof(Value));
		array->capacity = numVal;
	}
	return array;
}

bool appendToArray(RunCtx *runCtx, ObjArray *array, Value value) {
	array->modCount++;
	if (array->capacity < array->size + 1) {
		int oldCapacity = array->capacity;
		int newCapacity = GROW_CAPACITY(oldCapacity);
		Value *oldItems = array->items;
		array->items = GROW_ARRAY(runCtx, Value, array->items, oldCapacity, newCapacity);
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

Value arrayAtSafe(RunCtx *runCtx, ObjArray *array, int32_t index) {
	int32_t realIndex = (index < 0) ? array->size + index : index;

	if (ELOX_UNLIKELY((realIndex < 0) || (realIndex > array->size - 1)))
		return runtimeError(runCtx, NULL, "Array index out of range");

	return array->items[realIndex];
}

void arraySet(ObjArray *array, int index, Value value) {
	array->items[index] = value;
}

ObjHashMap *newHashMap(RunCtx *runCtx) {
	ObjHashMap *map = ALLOCATE_OBJ(runCtx, ObjHashMap, OBJ_HASHMAP);
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
	switch (getObjType(method)) {
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

static void printHashMap(VMCtx *vmCtx, EloxIOStream stream, ObjHashMap *map) {
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
	switch (getObjType(obj)) {
		case OBJ_HASHMAP:
			printHashMap(vmCtx, stream, (ObjHashMap *)obj);
			break;
		case OBJ_ARRAY:
			printArray(vmCtx, stream, (ObjArray *)obj, "[", "]");
			break;
		case OBJ_TUPLE:
			printArray(vmCtx, stream, (ObjArray *)obj, "<", ">");
			break;
		case OBJ_BOUND_METHOD:
			printMethod(vmCtx, stream, ((ObjBoundMethod *)obj)->method);
			break;
		case OBJ_METHOD:
			// TODO: print class
			ELOX_WRITE(vmCtx, stream, "M");
			printMethod(vmCtx, stream, ((ObjMethod *)obj)->method.callable);
			break;
		case OBJ_PENDING_METHOD:
			eloxPrintf(vmCtx, stream, "pendingDefault");
			break;
		case OBJ_DEFAULT_METHOD:
			eloxPrintf(vmCtx, stream, "defaultMethod");
			break;
		case OBJ_ABSTRACT_METHOD:
			eloxPrintf(vmCtx, stream, "abstractMethod");
			break;
		case OBJ_INTERFACE:
			eloxPrintf(vmCtx, stream, "interface %s", ((ObjInterface *)obj)->name->string.chars);
			break;
		case OBJ_CLASS:
			eloxPrintf(vmCtx, stream, "class %s", ((ObjClass *)obj)->name->string.chars);
			break;
		case OBJ_CLOSURE:
			printFunction(vmCtx, stream, ((ObjClosure *)obj)->function, "#", "#");
			break;
		case OBJ_NATIVE_CLOSURE:
			ELOX_WRITE(vmCtx, stream, "#<native fn>#");
			break;
		case OBJ_FUNCTION:
			printFunction(vmCtx, stream, ((ObjFunction *)obj), "<", ">");
			break;
		case OBJ_INSTANCE:
			eloxPrintf(vmCtx, stream, "%s instance",
					   ((ObjInstance *)obj)->class_->name->string.chars);
			break;
		case OBJ_NATIVE:
			eloxPrintf(vmCtx, stream, "<native fn %p>", ((ObjNative *)obj)->function);
			break;
		case OBJ_STRING:
			eloxPrintf(vmCtx, stream, "'%s'", OBJ_AS_CSTRING(obj));
			break;
		case OBJ_STRINGPAIR: {
			ObjStringPair *pair = (ObjStringPair *)obj;
			eloxPrintf(vmCtx, stream, "'%s', '%s'",
					   pair->str1 ? (const char *)pair->str1->string.chars : "<null>",
					   pair->str2 ? (const char *)pair->str2->string.chars : "<null>");
			break;
		}
		case OBJ_UPVALUE:
			ELOX_WRITE(vmCtx, stream, "upvalue");
			break;
		case OBJ_FRAME:
			ELOX_WRITE(vmCtx, stream, "frame");
			break;
		case OBJ_FIBER:
			eloxPrintf(vmCtx, stream, "fiber<%p>", obj);
			break;
	}
}
