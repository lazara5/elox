// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <time.h>
#include <math.h>
#include <inttypes.h>
#include <stdio.h>
#include <assert.h>

#include <elox/builtins.h>
#include <elox/builtins/string.h>
#include <elox/builtins/array.h>

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
				Error error = ERROR_INITIALIZER(vmCtx);
				Value strVal = toString(getValueArg(args, 1), &error);
				if (ELOX_UNLIKELY(error.raised))
					return strVal;
				const char *str = AS_CSTRING(strVal);
				push(vm, strVal);
				Value errorVal = runtimeError(vmCtx, "Assertion failed: %s", str);
				Value exception = pop(vm);
				pop(vm);
				push(vm, exception);
				return errorVal;
			}
		}
	}
	return NIL_VAL;
}

//--- Object --------------------

static Value objectToString(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	HeapCString ret;
	if (ELOX_UNLIKELY(!initHeapStringWithSize(vmCtx, &ret, 16)))
		return oomError(vmCtx);
	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	heapStringAddFmt(vmCtx, &ret, "%s@%u", inst->clazz->name->string.chars, inst->identityHash);
	ObjString *str = takeString(vmCtx, ret.chars, ret.length, ret.capacity);
	if (ELOX_UNLIKELY(str == NULL))
		return oomError(vmCtx);
	return OBJ_VAL(str);
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
	if (ELOX_UNLIKELY(!initHeapString(vmCtx, &ret)))
		return oomError(vmCtx);
	if (trunc(n) == n)
		heapStringAddFmt(vmCtx, &ret, "%" PRId64, (int64_t)n);
	else
		heapStringAddFmt(vmCtx, &ret, "%g", n);
	ObjString *str = takeString(vmCtx, ret.chars, ret.length, ret.capacity);
	if (ELOX_UNLIKELY(str == NULL))
		return oomError(vmCtx);
	return OBJ_VAL(str);
}

//--- Bool ----------------------

static Value boolToString(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;

	bool b = AS_BOOL(getValueArg(args, 0));
	return b ? OBJ_VAL(vm->builtins.trueString) : OBJ_VAL(vm->builtins.falseString);
}

//--- Throwable -----------------

static Value throwableInit(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjString *msg = AS_STRING(getValueArg(args, 1));

	ObjString *msgName = copyString(vmCtx, ELOX_USTR_AND_LEN("message"));
	if (ELOX_UNLIKELY(msgName == NULL))
		return oomError(vmCtx);
	TmpScope temps = TMP_SCOPE_INITIALIZER(vm);
	PUSH_TEMP(temps, protectedName, OBJ_VAL(msgName));
	setInstanceField(inst, msgName, OBJ_VAL(msg));
	releaseTemps(&temps);
	return OBJ_VAL(inst);
}

//--- Exception -----------------

static Value exceptionInit(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjString *msg = AS_STRING(getValueArg(args, 1));

	ObjString *msgName = copyString(vmCtx, ELOX_USTR_AND_LEN("message"));
	if (ELOX_UNLIKELY(msgName == NULL))
		return oomError(vmCtx);
	TmpScope temps = TMP_SCOPE_INITIALIZER(vm);
	PUSH_TEMP(temps, protectedName, OBJ_VAL(msgName));
	setInstanceField(inst, msgName, OBJ_VAL(msg));
	releaseTemps(&temps);
	return OBJ_VAL(inst);
}

//--- Error ---------------------

static Value errorInit(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjString *msg = AS_STRING(getValueArg(args, 1));

	Value superInit = AS_CLASS(inst->clazz->super)->initializer;
	if (!IS_NIL(superInit)) {
		push(vm, OBJ_VAL(inst));
		push(vm, OBJ_VAL(msg));
		bool wasNative;
		callMethod(vmCtx, AS_OBJ(superInit), 1, 0, &wasNative);
		pop(vm);
	}

	return OBJ_VAL(inst);
}

//--- Map -----------------------

static Value mapIteratorHasNext(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;
	struct MapIterator *mi = &vm->builtins.mapIterator;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjMap *map = AS_MAP(inst->fields.values[mi->_map]);
	int current = AS_NUMBER(inst->fields.values[mi->_current]);

	TableEntry *entry;
	int32_t nextIndex = valueTableGetNext(&map->items, current, &entry);

	return BOOL_VAL(nextIndex >= 0);
}

static Value mapIteratorNext(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;
	struct MapIterator *mi = &vm->builtins.mapIterator;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjMap *map = AS_MAP(inst->fields.values[mi->_map]);
	int current = AS_NUMBER(inst->fields.values[mi->_current]);
	uint32_t modCount = AS_NUMBER(inst->fields.values[mi->_modCount]);

	if (ELOX_UNLIKELY(modCount != map->items.modCount))
		return runtimeError(vmCtx, "Map modified during iteration");

	TableEntry *entry;
	int nextIndex = valueTableGetNext(&map->items, current, &entry);

	inst->fields.values[mi->_current] = NUMBER_VAL(nextIndex);

	ObjArray *ret = newArray(vmCtx, 2, OBJ_TUPLE);
	if (ELOX_UNLIKELY(ret == NULL))
		return oomError(vmCtx);
	TmpScope temps = TMP_SCOPE_INITIALIZER(vm);
	PUSH_TEMP(temps, protectedRet, OBJ_VAL(ret));
	// array pre-allocated, won't fail
	appendToArray(vmCtx, ret, entry->key);
	appendToArray(vmCtx, ret, entry->value);
	releaseTemps(&temps);
	return OBJ_VAL(ret);
}

static Value mapSize(Args *args) {
	ObjMap *inst = AS_MAP(getValueArg(args, 0));
	return NUMBER_VAL(inst->items.liveCount);
}

static Value mapPut(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	ObjMap *inst = AS_MAP(getValueArg(args, 0));
	Value key = getValueArg(args, 1);
	Value val = getValueArg(args, 2);

	Error error = ERROR_INITIALIZER(vmCtx);
	valueTableSet(&inst->items, key, val, &error);
	if (ELOX_UNLIKELY(error.raised))
		return EXCEPTION_VAL;

	return NIL_VAL;
}

static Value mapRemove(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	ObjMap *inst = AS_MAP(getValueArg(args, 0));
	Value key = getValueArg(args, 1);

	Error error = ERROR_INITIALIZER(vmCtx);
	bool deleted = valueTableDelete(&inst->items, key, &error);
	if (ELOX_UNLIKELY(error.raised))
		return EXCEPTION_VAL;

	return BOOL_VAL(deleted);
}

static Value mapIterator(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;
	struct MapIterator *mi = &vm->builtins.mapIterator;

	ObjMap *inst = AS_MAP(getValueArg(args, 0));

	ObjInstance *iter = newInstance(vmCtx, mi->_class);
	if (ELOX_UNLIKELY(iter == NULL))
		return oomError(vmCtx);
	iter->fields.values[mi->_map] = OBJ_VAL(inst);
	iter->fields.values[mi->_current] = NUMBER_VAL(0);
	iter->fields.values[mi->_modCount] = NUMBER_VAL(inst->items.modCount);
	return OBJ_VAL(iter);
}

static Value notImplementedMethod(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	return runtimeError(vmCtx, "Not implemented");
}

suint16_t builtinConstant(VMCtx *vmCtx, const String *name) {
	VM *vm = &vmCtx->vm;

	suint16_t ret = -1;
	TmpScope temps = TMP_SCOPE_INITIALIZER(vm);

	ObjString *nameString = copyString(vmCtx, name->chars, name->length);
	if (ELOX_UNLIKELY(nameString == NULL))
		goto cleanup;
	PUSH_TEMP(temps, protectedName, OBJ_VAL(nameString));

	Value indexValue;
	if (tableGet(&vm->builtinSymbols, nameString, &indexValue)) {
		// already present
		ret = (suint16_t)AS_NUMBER(indexValue);
		goto cleanup;
	}

	assert(vm->heap == &vm->permHeap);

	uint16_t newIndex = (uint16_t)vm->builtinValues.count;
	bool res = valueArrayPush(vmCtx, &vm->builtinValues, UNDEFINED_VAL);
	if (ELOX_UNLIKELY(!res))
		goto cleanup;

	Error error = ERROR_INITIALIZER(vmCtx);
	tableSet(&vm->builtinSymbols, nameString, NUMBER_VAL((double)newIndex), &error);
	if (ELOX_UNLIKELY(error.raised))
		goto cleanup;

#ifdef ELOX_DEBUG_PRINT_CODE
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, ">>>Builtin[%5u] (%.*s)\n", newIndex,
			   name->length, name->chars);
#endif

	ret = newIndex;

cleanup:
	releaseTemps(&temps);

	return ret;
}

static ObjClass *registerStaticClass(VMCtx *vmCtx, const String *name, const String *moduleName,
									 ObjClass *super) {
	VM *vm = &vmCtx->vm;

	bool isBuiltin = stringEquals(moduleName, &eloxBuiltinModule);
	ObjString *className = copyString(vmCtx, name->chars, name->length);
	if (ELOX_UNLIKELY(className == NULL))
		return NULL;
	push(vm, OBJ_VAL(className));
	ObjClass *clazz = newClass(vmCtx, className);
	if (ELOX_UNLIKELY(clazz == NULL))
		return NULL;
	push(vm, OBJ_VAL(clazz));

	if (isBuiltin) {
		suint16_t builtinIdx = builtinConstant(vmCtx, name);
		if (ELOX_UNLIKELY(builtinIdx < 0))
			return NULL;
		vm->builtinValues.values[builtinIdx] = peek(vm, 0);
	} else {
		uint16_t globalIdx = globalIdentifierConstant(vmCtx, name, moduleName);
		vm->globalValues.values[globalIdx] = peek(vm, 0);
	}

	popn(vm, 2);
	Error error = ERROR_INITIALIZER(vmCtx);
	if (super != NULL) {
		clazz->super = OBJ_VAL(super);
		clazz->classId = clazz->baseId * super->classId;
		for (int i = 0; i < super->fields.capacity; i++) {
			Entry *entry = &super->fields.entries[i];
			if (entry->key != NULL) {
				tableSet(&clazz->fields, entry->key, entry->value, &error);
				if (ELOX_UNLIKELY(error.raised)) {
					pop(vm); // discard error
					return NULL;
				}
			}
		}
		tableAddAll(&super->methods, &clazz->methods, &error);
		if (ELOX_UNLIKELY(error.raised)) {
			pop(vm); // discard error
			return NULL;
		}

		clazz->initializer = super->initializer;
	} else
		clazz->classId = clazz->baseId;

	return clazz;
}

#define RET_IF_OOM(ptr) \
{ \
	if (ELOX_UNLIKELY(ptr == NULL)) \
		return false; \
}

#define RET_IF_RAISED(err) \
{ \
	if (ELOX_UNLIKELY(err.raised)) \
		return false; \
}

bool registerBuiltins(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	ErrorMsg errMsg = ERROR_MSG_INITIALIZER;

	vm->builtins.anonInitString = copyString(vmCtx, ELOX_USTR_AND_LEN("$init"));
	RET_IF_OOM(vm->builtins.anonInitString);

	vm->builtins.iteratorString = copyString(vmCtx, ELOX_USTR_AND_LEN("iterator"));
	RET_IF_OOM(vm->builtins.iteratorString);
	vm->builtins.hasNextString = copyString(vmCtx, ELOX_USTR_AND_LEN("hasNext"));
	RET_IF_OOM(vm->builtins.hasNextString);
	vm->builtins.nextString = copyString(vmCtx, ELOX_USTR_AND_LEN("next"));
	RET_IF_OOM(vm->builtins.nextString);

	vm->builtins.hashCodeString = copyString(vmCtx, ELOX_USTR_AND_LEN("hashCode"));
	RET_IF_OOM(vm->builtins.hashCodeString);
	vm->builtins.equalsString = copyString(vmCtx, ELOX_USTR_AND_LEN("equals"));
	RET_IF_OOM(vm->builtins.equalsString);
	vm->builtins.toStringString = copyString(vmCtx, ELOX_USTR_AND_LEN("toString"));
	RET_IF_OOM(vm->builtins.toStringString);

	const String objectName = STRING_INITIALIZER("Object");
	ObjClass *objectClass = registerStaticClass(vmCtx, &objectName, &eloxBuiltinModule, NULL);
	RET_IF_OOM(objectClass);
	addNativeMethod(vmCtx, objectClass, "toString", objectToString, 1, false, &errMsg);
	addNativeMethod(vmCtx, objectClass, "hashCode", objectHashCode, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);

	const String iteratorName = STRING_INITIALIZER("Iterator");
	ObjClass *iteratorClass = registerStaticClass(vmCtx, &iteratorName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(iteratorClass);
	addNativeMethod(vmCtx, iteratorClass, "hasNext", notImplementedMethod, 1, false, &errMsg);
	addNativeMethod(vmCtx, iteratorClass, "next", notImplementedMethod, 1, false, &errMsg);
	addNativeMethod(vmCtx, iteratorClass, "remove", notImplementedMethod, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.iteratorClass = iteratorClass;

	const String gmatchIteratorName = STRING_INITIALIZER("$GmatchIterator");
	ObjClass *gmatchIteratorClass = registerStaticClass(vmCtx, &gmatchIteratorName, &eloxBuiltinModule, iteratorClass);
	RET_IF_OOM(gmatchIteratorClass);
	vm->builtins.gmatchIterator = (struct GmatchIterator){
		._string = addClassField(vmCtx, gmatchIteratorClass, "string", &errMsg),
		._pattern = addClassField(vmCtx, gmatchIteratorClass, "pattern", &errMsg),
		._offset = addClassField(vmCtx, gmatchIteratorClass, "offset", &errMsg),
		._cachedNext = addClassField(vmCtx, gmatchIteratorClass, "cachedNext", &errMsg),
		._class = gmatchIteratorClass
	};
	addNativeMethod(vmCtx, gmatchIteratorClass, "hasNext", gmatchIteratorHasNext, 1, false, &errMsg);
	addNativeMethod(vmCtx, gmatchIteratorClass, "next", gmatchIteratorNext, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);

	const String stringName = STRING_INITIALIZER("String");
	ObjClass *stringClass = registerStaticClass(vmCtx, &stringName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(stringClass);
	addNativeMethod(vmCtx, stringClass, "toString", stringToString, 1, false, &errMsg);
	addNativeMethod(vmCtx, stringClass, "hashCode", stringHashCode, 1, false, &errMsg);
	addNativeMethod(vmCtx, stringClass, "length", stringLength, 1, false, &errMsg);
	addNativeMethod(vmCtx, stringClass, "fmt", stringFmt, 1, true, &errMsg);
	addNativeMethod(vmCtx, stringClass, "find", stringFind, 3, false, &errMsg);
	addNativeMethod(vmCtx, stringClass, "findMatch", stringFindMatch, 3, false, &errMsg);
	addNativeMethod(vmCtx, stringClass, "match", stringMatch, 3, false, &errMsg);
	addNativeMethod(vmCtx, stringClass, "gmatch", stringGmatch, 2, false, &errMsg);
	vm->builtins.stringGsub = addNativeMethod(vmCtx, stringClass, "gsub", stringGsub, 4, false, &errMsg);
	addNativeMethod(vmCtx, stringClass, "startsWith", stringStartsWith, 2, false, &errMsg);
	addNativeMethod(vmCtx, stringClass, "endsWith", stringEndsWith, 2, false, &errMsg);
	addNativeMethod(vmCtx, stringClass, "upper", stringUpper, 1, false, &errMsg);
	addNativeMethod(vmCtx, stringClass, "lower", stringLower, 1, false, &errMsg);
	addNativeMethod(vmCtx, stringClass, "trim", stringTrim, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);

	vm->builtins.stringClass = stringClass;

	const String numberName = STRING_INITIALIZER("Number");
	ObjClass *numberClass = registerStaticClass(vmCtx, &numberName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(numberClass);
	addNativeMethod(vmCtx, numberClass, "toString", numberToString, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.numberClass = numberClass;

	vm->builtins.trueString = copyString(vmCtx, ELOX_USTR_AND_LEN("true"));
	RET_IF_OOM(vm->builtins.trueString);
	vm->builtins.falseString = copyString(vmCtx, ELOX_USTR_AND_LEN("false"));
	RET_IF_OOM(vm->builtins.falseString);

	const String boolName = STRING_INITIALIZER("Bool");
	ObjClass *boolClass = registerStaticClass(vmCtx, &boolName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(boolClass);
	addNativeMethod(vmCtx, boolClass, "toString", boolToString, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.boolClass = boolClass;

	const String instanceName = STRING_INITIALIZER("$Instance");
	ObjClass *instanceClass = registerStaticClass(vmCtx, &instanceName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(instanceClass);
	vm->builtins.instanceClass = instanceClass;

	const String className = STRING_INITIALIZER("Class");
	ObjClass *classClass = registerStaticClass(vmCtx, &className, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(classClass);
	vm->builtins.classClass = classClass;

	const String throwableName = STRING_INITIALIZER("Throwable");
	ObjClass *throwableClass = registerStaticClass(vmCtx, &throwableName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(throwableClass);
	addClassField(vmCtx, throwableClass, "message", &errMsg);
	addNativeMethod(vmCtx, throwableClass, "Throwable", throwableInit, 2, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.throwableClass = throwableClass;

	const String exceptionName = STRING_INITIALIZER("Exception");
	ObjClass *exceptionClass = registerStaticClass(vmCtx, &exceptionName, &eloxBuiltinModule, throwableClass);
	RET_IF_OOM(exceptionClass);
	addClassField(vmCtx, exceptionClass, "stacktrace", &errMsg);
	addNativeMethod(vmCtx, exceptionClass, "Exception", exceptionInit, 2, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.exceptionClass = exceptionClass;

	const String runtimeExceptionName = STRING_INITIALIZER("RuntimeException");
	ObjClass *runtimeExceptionClass = registerStaticClass(vmCtx, &runtimeExceptionName, &eloxBuiltinModule, exceptionClass);
	RET_IF_OOM(runtimeExceptionClass);
	vm->builtins.runtimeExceptionClass = runtimeExceptionClass;

	const String errorName = STRING_INITIALIZER("Error");
	ObjClass *errorClass = registerStaticClass(vmCtx, &errorName, &eloxBuiltinModule, throwableClass);
	RET_IF_OOM(errorClass);
	addNativeMethod(vmCtx, errorClass, "Error", errorInit, 2, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.errorClass = errorClass;

	ObjInstance *oomErrorInst = newInstance(vmCtx, errorClass);
	RET_IF_OOM(oomErrorInst);
	push(vm, OBJ_VAL(oomErrorInst));
	ObjString *oomErrorMsg = copyString(vmCtx, ELOX_USTR_AND_LEN("Out of memory"));
	RET_IF_OOM(oomErrorMsg);
	push(vm, OBJ_VAL(oomErrorMsg));
	bool wasNative;
	callMethod(vmCtx, AS_OBJ(errorClass->initializer), 1, 0, &wasNative);
	pop(vm);
	vm->builtins.oomError = oomErrorInst;

	const String arrayIteratorName = STRING_INITIALIZER("$ArrayIterator");
	ObjClass *arrayIteratorClass = registerStaticClass(vmCtx, &arrayIteratorName, &eloxBuiltinModule, iteratorClass);
	RET_IF_OOM(arrayIteratorClass);
	vm->builtins.arrayIterator = (struct ArrayIterator){
		._array = addClassField(vmCtx, arrayIteratorClass, "array", &errMsg),
		._cursor = addClassField(vmCtx, arrayIteratorClass, "cursor", &errMsg),
		._lastRet = addClassField(vmCtx, arrayIteratorClass, "lastRet", &errMsg),
		._modCount = addClassField(vmCtx, arrayIteratorClass, "modCount", &errMsg),
		._class = arrayIteratorClass
	};
	addNativeMethod(vmCtx, arrayIteratorClass, "hasNext", arrayIteratorHasNext, 1, false, &errMsg);
	addNativeMethod(vmCtx, arrayIteratorClass, "next", arrayIteratorNext, 1, false, &errMsg);
	addNativeMethod(vmCtx, arrayIteratorClass, "remove", arrayIteratorRemove, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);

	const String arrayName = STRING_INITIALIZER("Array");
	ObjClass *arrayClass = registerStaticClass(vmCtx, &arrayName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(arrayClass);
	addNativeMethod(vmCtx, arrayClass, "length", arrayLength, 1, false, &errMsg);
	addNativeMethod(vmCtx, arrayClass, "add", arrayAdd, 2, false, &errMsg);
	addNativeMethod(vmCtx, arrayClass, "removeAt", arrayRemoveAt, 2, false, &errMsg);
	addNativeMethod(vmCtx, arrayClass, "iterator", arrayIterator, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.arrayClass = arrayClass;

	const String tupleName = STRING_INITIALIZER("Tuple");
	ObjClass *tupleClass = registerStaticClass(vmCtx, &tupleName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(tupleClass);
	addNativeMethod(vmCtx, tupleClass, "length", arrayLength, 1, false, &errMsg);
	addNativeMethod(vmCtx, tupleClass, "iterator", arrayIterator, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.tupleClass = tupleClass;

	const String mapIteratorName = STRING_INITIALIZER("$MapIterator");
	ObjClass *mapIteratorClass = registerStaticClass(vmCtx, &mapIteratorName, &eloxBuiltinModule, iteratorClass);
	RET_IF_OOM(mapIteratorClass);
	vm->builtins.mapIterator = (struct MapIterator){
		._map = addClassField(vmCtx, mapIteratorClass, "map", &errMsg),
		._current = addClassField(vmCtx, mapIteratorClass, "current", &errMsg),
		._modCount = addClassField(vmCtx, mapIteratorClass, "modCount", &errMsg),
		._class = mapIteratorClass
	};
	addNativeMethod(vmCtx, mapIteratorClass, "hasNext", mapIteratorHasNext, 1, false, &errMsg);
	addNativeMethod(vmCtx, mapIteratorClass, "next", mapIteratorNext, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);

	const String mapName = STRING_INITIALIZER("Map");
	ObjClass *mapClass = registerStaticClass(vmCtx, &mapName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(mapClass);
	addNativeMethod(vmCtx, mapClass, "size", mapSize, 1, false, &errMsg);
	addNativeMethod(vmCtx, mapClass, "put", mapPut, 3, false, &errMsg);
	addNativeMethod(vmCtx, mapClass, "remove", mapRemove, 2, false, &errMsg);
	addNativeMethod(vmCtx, mapClass, "iterator", mapIterator, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.mapClass = mapClass;

	const String printName = STRING_INITIALIZER("print");
	ObjNative *printFn = registerNativeFunction(vmCtx, &printName,
												&eloxBuiltinModule, printNative, 0, true);
	RET_IF_OOM(printFn);

	const String printfName = STRING_INITIALIZER("printf");
	ObjNative *printfFn = registerNativeFunction(vmCtx, &printfName,
												 &eloxBuiltinModule, printFmt, 1, true);
	RET_IF_OOM(printfFn);

	const String assertName = STRING_INITIALIZER("assert");
	ObjNative *assertFn = registerNativeFunction(vmCtx, &assertName,
												 &eloxBuiltinModule, assertNative, 0, true);
	RET_IF_OOM(assertFn);

	return true;
}

void clearBuiltins(VM *vm) {
	vm->builtins.anonInitString = NULL;

	vm->builtins.iteratorString = NULL;
	vm->builtins.hasNextString = NULL;
	vm->builtins.nextString = NULL;

	vm->builtins.hashCodeString = NULL;
	vm->builtins.equalsString = NULL;
	vm->builtins.toStringString = NULL;

	vm->builtins.stringClass = NULL;
	vm->builtins.gmatchIterator._class = NULL;

	vm->builtins.numberClass = NULL;

	vm->builtins.boolClass = NULL;
	vm->builtins.trueString = NULL;
	vm->builtins.falseString = NULL;
	vm->builtins.instanceClass = NULL;
	vm->builtins.classClass = NULL;

	vm->builtins.oomError = NULL;
	vm->builtins.errorClass = NULL;
	vm->builtins.runtimeExceptionClass = NULL;
	vm->builtins.exceptionClass = NULL;
	vm->builtins.throwableClass = NULL;
	vm->builtins.arrayIterator._class = NULL;
	vm->builtins.arrayClass = NULL;
	vm->builtins.tupleClass = NULL;
	vm->builtins.mapIterator._class = NULL;
	vm->builtins.mapClass = NULL;
	vm->builtins.iteratorClass = NULL;
}
