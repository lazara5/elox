#include <time.h>
#include <math.h>
#include <inttypes.h>
#include <stdio.h>

#include "elox/builtins.h"

static Value clockNative(Args *args ELOX_UNUSED) {
	return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static Value printNative(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	for (int i = 0; i < args->count; i++) {
		printValue(vmCtx, ELOX_IO_OUT, getValueArg(args, i));
		ELOX_WRITE(vmCtx, ELOX_IO_OUT, " ");
	}
	ELOX_WRITE(vmCtx, ELOX_IO_OUT, "\n");
	return NIL_VAL;
}

static Value assertNative(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;

	if (args->count > 0) {
		if (isFalsey(getValueArg(args, 0))) {
			if (args->count < 2)
				return runtimeError(vmCtx, "Assertion failed");
			else {
				ExecContext execCtx = EXEC_CTX_INITIALIZER(vmCtx);
				Value strVal = toString(&execCtx, getValueArg(args, 1));
				if (ELOX_UNLIKELY(execCtx.error))
					return strVal;
				const char *str = AS_CSTRING(strVal);
				push(vm, strVal);
				Value error = runtimeError(vmCtx, "Assertion failed: %s", str);
				Value exception = pop(vm);
				pop(vm);
				push(vm, exception);
				return error;
			}
		}
	}
	return NIL_VAL;
}

//--- Object --------------------

static Value objectToString(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	HeapCString ret;
	initHeapStringWithSize(vmCtx, &ret, 16);
	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	heapStringAddFmt(vmCtx, &ret, "%s@%u", inst->clazz->name->string.chars, inst->identityHash);
	return OBJ_VAL(takeString(vmCtx, ret.chars, ret.length, ret.capacity));
}

static Value objectHashCode(Args *args) {
	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	return NUMBER_VAL(inst->identityHash);
}

//--- String --------------------

static Value stringToString(Args *args) {
	ObjString *inst = AS_STRING(getValueArg(args, 0));
	return OBJ_VAL(inst);
}

static Value stringHashCode(Args *args) {
	ObjString *inst = AS_STRING(getValueArg(args, 0));
	return NUMBER_VAL(inst->hash);
}

static Value stringLength(Args *args) {
	ObjString *inst = AS_STRING(getValueArg(args, 0));
	return NUMBER_VAL(inst->string.length);
}

//--- Number --------------------

static Value numberToString(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	double n = AS_NUMBER(getValueArg(args, 0));
	HeapCString ret;
	initHeapString(vmCtx, &ret);
	if (trunc(n) == n)
		heapStringAddFmt(vmCtx, &ret, "%" PRId64, (int64_t)n);
	else
		heapStringAddFmt(vmCtx, &ret, "%g", n);
	return OBJ_VAL(takeString(vmCtx, ret.chars, ret.length, ret.capacity));
}

//--- Bool ----------------------

static Value boolToString(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;

	bool b = AS_BOOL(getValueArg(args, 0));
	return b ? OBJ_VAL(vm->trueString) : OBJ_VAL(vm->falseString);
}

//--- Exception -----------------

static Value exceptionInit(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjString *msg = AS_STRING(getValueArg(args, 1));
	ObjString *msgName = copyString(vmCtx, ELOX_STR_AND_LEN("message"));
	push(vm, OBJ_VAL(msgName));
	setInstanceField(inst, msgName, OBJ_VAL(msg));
	pop(vm);
	return OBJ_VAL(inst);
}

//--- Array ---------------------

static Value arrayIteratorHasNext(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjArray *array = AS_ARRAY(inst->fields.values[vm->arrayIteratorArrayIndex]);
	int current = AS_NUMBER(inst->fields.values[vm->arrayIteratorCurrentIndex]);

	return BOOL_VAL(current < array->size);
}

static Value arrayIteratorNext(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjArray *array = AS_ARRAY(inst->fields.values[vm->arrayIteratorArrayIndex]);
	int current = AS_NUMBER(inst->fields.values[vm->arrayIteratorCurrentIndex]);
	uint32_t modCount = AS_NUMBER(inst->fields.values[vm->arrayIteratorModCountIndex]);

	if (ELOX_UNLIKELY(modCount != array->modCount))
		return runtimeError(vmCtx, "Array modified during iteration");

	inst->fields.values[vm->arrayIteratorCurrentIndex] = NUMBER_VAL(current + 1);
	return array->items[current];
}

static Value arrayLength(Args *args) {
	ObjArray *inst = AS_ARRAY(getValueArg(args, 0));
	return NUMBER_VAL(inst->size);
}

static Value arrayIterator(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;

	ObjArray *inst = AS_ARRAY(getValueArg(args, 0));

	ObjInstance *iter = newInstance(vmCtx, vm->arrayIteratorClass);
	iter->fields.values[vm->arrayIteratorArrayIndex] = OBJ_VAL(inst);
	iter->fields.values[vm->arrayIteratorCurrentIndex] = NUMBER_VAL(0);
	iter->fields.values[vm->arrayIteratorModCountIndex] = NUMBER_VAL(inst->modCount);
	return OBJ_VAL(iter);
}

static Value arrayAdd(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	ObjArray *inst = AS_ARRAY(getValueArg(args, 0));
	Value val = getValueArg(args, 1);
	appendToArray(vmCtx, inst, val);

	return NIL_VAL;
}

#define ARRAY_CHECK_INDEX_RET(vmCtx, array, index) \
	if (ELOX_UNLIKELY(((index) < 0) || (index > array->size - 1))) \
		return runtimeError(vmCtx, "Array index out of bounds");

static Value arrayRemoveAt(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	ObjArray *inst = AS_ARRAY(getValueArg(args, 0));
	double indexArg;
	ELOX_GET_NUMBER_ARG_ELSE_RET(&indexArg, args, 1);

	int index = indexArg;
	ARRAY_CHECK_INDEX_RET(vmCtx, inst, index);
	inst->modCount++;
	memmove(inst->items + index, inst->items + index + 1,
			(inst->size - index - 1) * sizeof(Value));
	inst->size--;

	return NIL_VAL;
}

//--- Map -----------------------

static Value mapIteratorHasNext(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjMap *map = AS_MAP(inst->fields.values[vm->mapIteratorMapIndex]);
	int current = AS_NUMBER(inst->fields.values[vm->mapIteratorCurrentIndex]);

	TableEntry *entry;
	int32_t nextIndex = closeTableGetNext(&map->items, current, &entry);

	return BOOL_VAL(nextIndex >= 0);
}

static Value mapIteratorNext(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjMap *map = AS_MAP(inst->fields.values[vm->mapIteratorMapIndex]);
	int current = AS_NUMBER(inst->fields.values[vm->mapIteratorCurrentIndex]);
	uint32_t modCount = AS_NUMBER(inst->fields.values[vm->mapIteratorModCountIndex]);

	if (ELOX_UNLIKELY(modCount != map->items.modCount))
		return runtimeError(vmCtx, "Map modified during iteration");

	TableEntry *entry;
	int nextIndex = closeTableGetNext(&map->items, current, &entry);

	inst->fields.values[vm->arrayIteratorCurrentIndex] = NUMBER_VAL(nextIndex);

	ObjArray *ret = newArray(vmCtx, 2, OBJ_TUPLE);
	push(vm, OBJ_VAL(ret));
	appendToArray(vmCtx, ret, entry->key);
	appendToArray(vmCtx, ret, entry->value);
	pop(vm);
	return OBJ_VAL(ret);
}

static Value mapSize(Args *args) {
	ObjMap *inst = AS_MAP(getValueArg(args, 0));
	return NUMBER_VAL(inst->items.count);
}

static Value mapIterator(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;

	ObjMap *inst = AS_MAP(getValueArg(args, 0));

	ObjInstance *iter = newInstance(vmCtx, vm->mapIteratorClass);
	iter->fields.values[vm->mapIteratorMapIndex] = OBJ_VAL(inst);
	iter->fields.values[vm->mapIteratorCurrentIndex] = NUMBER_VAL(0);
	iter->fields.values[vm->mapIteratorModCountIndex] = NUMBER_VAL(inst->items.modCount);
	return OBJ_VAL(iter);
}

static Value notImplementedMethod(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	return runtimeError(vmCtx, "Not implemented");
}

static ObjClass *registerStaticClass(VMCtx *vmCtx, const String *name, const String *moduleName,
									 ObjClass *super) {
	VM *vm = &vmCtx->vm;
	ObjString *className = copyString(vmCtx, name->chars, name->length);
	push(vm, OBJ_VAL(className));
	ObjClass *clazz = newClass(vmCtx, className);
	push(vm, OBJ_VAL(clazz));
	uint16_t globalIdx = globalIdentifierConstant(vmCtx, name, moduleName);
	vm->globalValues.values[globalIdx] = peek(vm, 0);
	popn(vm, 2);
	if (super != NULL) {
		clazz->super = OBJ_VAL(super);
		clazz->classId = clazz->baseId * super->classId;
		for (int i = 0; i < super->fields.capacity; i++) {
			Entry *entry = &super->fields.entries[i];
			if (entry->key != NULL)
				tableSet(vmCtx, &clazz->fields, entry->key, entry->value);
		}
		tableAddAll(vmCtx, &super->methods, &clazz->methods);
		clazz->initializer = super->initializer;
	} else
		clazz->classId = clazz->baseId;

	if (stringEquals(moduleName, &eloxBuiltinModule)) {
		// already interned and referenced in global table
		ObjString *nameStr = copyString(vmCtx, name->chars, name->length);
		tableSet(vmCtx, &vm->builtinSymbols, nameStr, OBJ_VAL(clazz));
	}

	return clazz;
}

void registerBuiltins(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	clearBuiltins(vm);

	vm->iteratorString = copyString(vmCtx, ELOX_STR_AND_LEN("iterator"));
	vm->hasNextString = copyString(vmCtx, ELOX_STR_AND_LEN("hasNext"));
	vm->nextString = copyString(vmCtx, ELOX_STR_AND_LEN("next"));

	vm->hashCodeString = copyString(vmCtx, ELOX_STR_AND_LEN("hashCode"));
	vm->equalsString = copyString(vmCtx, ELOX_STR_AND_LEN("equals"));
	vm->toStringString = copyString(vmCtx, ELOX_STR_AND_LEN("toString"));

	const String objectName = STRING_INITIALIZER("Object");
	ObjClass *objectClass = registerStaticClass(vmCtx, &objectName, &eloxBuiltinModule, NULL);
	addNativeMethod(vmCtx, objectClass, "toString", objectToString, 1, false);
	addNativeMethod(vmCtx, objectClass, "hashCode", objectHashCode, 1, false);

	const String stringName = STRING_INITIALIZER("String");
	ObjClass *stringClass = registerStaticClass(vmCtx, &stringName, &eloxBuiltinModule, objectClass);
	addNativeMethod(vmCtx, stringClass, "toString", stringToString, 1, false);
	addNativeMethod(vmCtx, stringClass, "hashCode", stringHashCode, 1, false);
	addNativeMethod(vmCtx, stringClass, "length", stringLength, 1, false);
	addNativeMethod(vmCtx, stringClass, "fmt", stringFmt, 1, true);
	addNativeMethod(vmCtx, stringClass, "match", stringMatch, 3, false);
	addNativeMethod(vmCtx, stringClass, "gsub", stringGsub, 4, false);
	addNativeMethod(vmCtx, stringClass, "startsWith", stringStartsWith, 2, false);
	addNativeMethod(vmCtx, stringClass, "endsWith", stringEndsWith, 2, false);

	vm->stringClass = stringClass;

	const String numberName = STRING_INITIALIZER("Number");
	ObjClass *numberClass = registerStaticClass(vmCtx, &numberName, &eloxBuiltinModule, objectClass);
	addNativeMethod(vmCtx, numberClass, "toString", numberToString, 1, false);
	vm->numberClass = numberClass;

	vm->trueString = copyString(vmCtx, ELOX_STR_AND_LEN("true"));
	vm->falseString = copyString(vmCtx, ELOX_STR_AND_LEN("false"));

	const String boolName = STRING_INITIALIZER("Bool");
	ObjClass *boolClass = registerStaticClass(vmCtx, &boolName, &eloxBuiltinModule, objectClass);
	addNativeMethod(vmCtx, boolClass, "toString", boolToString, 1, false);
	vm->boolClass = boolClass;

	const String exceptionName = STRING_INITIALIZER("Exception");
	ObjClass *exceptionClass = registerStaticClass(vmCtx, &exceptionName, &eloxBuiltinModule, objectClass);
	addClassField(vmCtx, exceptionClass, "message");
	addClassField(vmCtx, exceptionClass, "stacktrace");
	addNativeMethod(vmCtx, exceptionClass, "Exception", exceptionInit, 2, false);
	vm->exceptionClass = exceptionClass;

	const String runtimeExceptionName = STRING_INITIALIZER("RuntimeException");
	ObjClass *runtimeExceptionClass = registerStaticClass(vmCtx, &runtimeExceptionName, &eloxBuiltinModule, exceptionClass);
	vm->runtimeExceptionClass = runtimeExceptionClass;

	const String iteratorName = STRING_INITIALIZER("Iterator");
	ObjClass *iteratorClass = registerStaticClass(vmCtx, &iteratorName, &eloxBuiltinModule, objectClass);
	addNativeMethod(vmCtx, iteratorClass, "hasNext", notImplementedMethod, 1, false);
	addNativeMethod(vmCtx, iteratorClass, "next", notImplementedMethod, 1, false);
	addNativeMethod(vmCtx, iteratorClass, "remove", notImplementedMethod, 1, false);
	vm->iteratorClass = iteratorClass;

	const String arrayIteratorName = STRING_INITIALIZER("$ArrayIterator");
	ObjClass *arrayIteratorClass = registerStaticClass(vmCtx, &arrayIteratorName, &eloxBuiltinModule, iteratorClass);
	vm->arrayIteratorArrayIndex = addClassField(vmCtx, arrayIteratorClass, "array");
	vm->arrayIteratorCurrentIndex = addClassField(vmCtx, arrayIteratorClass, "current");
	vm->arrayIteratorModCountIndex = addClassField(vmCtx, arrayIteratorClass, "modCount");
	addNativeMethod(vmCtx, arrayIteratorClass, "hasNext", arrayIteratorHasNext, 1, false);
	addNativeMethod(vmCtx, arrayIteratorClass, "next", arrayIteratorNext, 1, false);
	vm->arrayIteratorClass = arrayIteratorClass;

	const String arrayName = STRING_INITIALIZER("Array");
	ObjClass *arrayClass = registerStaticClass(vmCtx, &arrayName, &eloxBuiltinModule, objectClass);
	addNativeMethod(vmCtx, arrayClass, "length", arrayLength, 1, false);
	addNativeMethod(vmCtx, arrayClass, "add", arrayAdd, 2, false);
	addNativeMethod(vmCtx, arrayClass, "removeAt", arrayRemoveAt, 2, false);
	addNativeMethod(vmCtx, arrayClass, "iterator", arrayIterator, 1, false);
	vm->arrayClass = arrayClass;

	const String mapIteratorName = STRING_INITIALIZER("$MapIterator");
	ObjClass *mapIteratorClass = registerStaticClass(vmCtx, &mapIteratorName, &eloxBuiltinModule, iteratorClass);
	vm->mapIteratorMapIndex = addClassField(vmCtx, mapIteratorClass, "map");
	vm->mapIteratorCurrentIndex = addClassField(vmCtx, mapIteratorClass, "current");
	vm->mapIteratorModCountIndex = addClassField(vmCtx, mapIteratorClass, "modCount");
	addNativeMethod(vmCtx, mapIteratorClass, "hasNext", mapIteratorHasNext, 1, false);
	addNativeMethod(vmCtx, mapIteratorClass, "next", mapIteratorNext, 1, false);
	vm->mapIteratorClass = mapIteratorClass;

	const String mapName = STRING_INITIALIZER("Map");
	ObjClass *mapClass = registerStaticClass(vmCtx, &mapName, &eloxBuiltinModule, objectClass);
	addNativeMethod(vmCtx, mapClass, "size", mapSize, 1, false);
	addNativeMethod(vmCtx, mapClass, "iterator", mapIterator, 1, false);
	vm->mapClass = mapClass;

	const String printName = STRING_INITIALIZER("print");
	registerNativeFunction(vmCtx, &printName, &eloxBuiltinModule, printNative, 0, true);

	const String printfName = STRING_INITIALIZER("printf");
	registerNativeFunction(vmCtx, &printfName, &eloxBuiltinModule, printFmt, 1, true);

	const String assertName = STRING_INITIALIZER("assert");
	registerNativeFunction(vmCtx, &assertName, &eloxBuiltinModule, assertNative, 0, true);

	const String clockName = STRING_INITIALIZER("clock");
	registerNativeFunction(vmCtx, &clockName, &eloxBuiltinModule, clockNative, 0, false);
}

void markBuiltins(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	markTable(vmCtx, &vm->builtinSymbols);

	markObject(vmCtx, (Obj *)vm->iteratorString);
	markObject(vmCtx, (Obj *)vm->hasNextString);
	markObject(vmCtx, (Obj *)vm->nextString);

	markObject(vmCtx, (Obj *)vm->hashCodeString);
	markObject(vmCtx, (Obj *)vm->equalsString);
	markObject(vmCtx, (Obj *)vm->toStringString);

	markObject(vmCtx, (Obj *)vm->stringClass);
	markObject(vmCtx, (Obj *)vm->numberClass);

	markObject(vmCtx, (Obj *)vm->boolClass);
	markObject(vmCtx, (Obj *)vm->trueString);
	markObject(vmCtx, (Obj *)vm->falseString);

	markObject(vmCtx, (Obj *)vm->exceptionClass);
	markObject(vmCtx, (Obj *)vm->runtimeExceptionClass);
	markObject(vmCtx, (Obj *)vm->arrayIteratorClass);
	markObject(vmCtx, (Obj *)vm->arrayClass);
	markObject(vmCtx, (Obj *)vm->mapIteratorClass);
	markObject(vmCtx, (Obj *)vm->mapClass);
	markObject(vmCtx, (Obj *)vm->iteratorClass);
}

void clearBuiltins(VM *vm) {
	vm->iteratorString = NULL;
	vm->hasNextString = NULL;
	vm->nextString = NULL;

	vm->hashCodeString = NULL;
	vm->equalsString = NULL;
	vm->toStringString = NULL;

	vm->stringClass = NULL;
	vm->numberClass = NULL;

	vm->boolClass = NULL;
	vm->trueString = NULL;
	vm->falseString = NULL;

	vm->exceptionClass = NULL;
	vm->runtimeExceptionClass = NULL;
	vm->arrayIteratorClass = NULL;
	vm->arrayClass = NULL;
	vm->mapIteratorClass = NULL;
	vm->mapClass = NULL;
	vm->iteratorClass = NULL;
}
