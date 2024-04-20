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
	RunCtx *runCtx = args->runCtx;

	for (int i = 0; i < args->count; i++) {
		printValue(runCtx, ELOX_IO_OUT, getValueArg(args, i));
		ELOX_WRITE(runCtx, ELOX_IO_OUT, " ");
	}
	ELOX_WRITE(runCtx, ELOX_IO_OUT, "\n");
	return NIL_VAL;
}

static Value assertNative(Args *args) {
	RunCtx *runCtx = args->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	if (args->count > 0) {
		if (isFalsey(getValueArg(args, 0))) {
			if (args->count < 2)
				return runtimeError(runCtx, "Assertion failed");
			else {
				Error error = ERROR_INITIALIZER(runCtx);
				Value strVal = toString(getValueArg(args, 1), &error);
				if (ELOX_UNLIKELY(error.raised))
					return strVal;
				const char *str = AS_CSTRING(strVal);
				push(fiber, strVal);
				Value errorVal = runtimeError(runCtx, "Assertion failed: %s", str);
				Value exception = pop(fiber);
				pop(fiber);
				push(fiber, exception);
				return errorVal;
			}
		}
	}
	return NIL_VAL;
}

//--- Object --------------------

static Value objectToString(Args *args) {
	RunCtx *runCtx = args->runCtx;

	HeapCString ret;
	if (ELOX_UNLIKELY(!initHeapStringWithSize(runCtx, &ret, 16)))
		return oomError(runCtx);
	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	heapStringAddFmt(runCtx, &ret, "%s@%u", inst->clazz->name->string.chars, inst->identityHash);
	ObjString *str = takeString(runCtx, ret.chars, ret.length, ret.capacity);
	if (ELOX_UNLIKELY(str == NULL))
		return oomError(runCtx);
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
	RunCtx *runCtx = args->runCtx;

	double n = AS_NUMBER(getValueArg(args, 0));
	HeapCString ret;
	if (ELOX_UNLIKELY(!initHeapString(runCtx, &ret)))
		return oomError(runCtx);
	if (trunc(n) == n)
		heapStringAddFmt(runCtx, &ret, "%" PRId64, (int64_t)n);
	else
		heapStringAddFmt(runCtx, &ret, "%g", n);
	ObjString *str = takeString(runCtx, ret.chars, ret.length, ret.capacity);
	if (ELOX_UNLIKELY(str == NULL))
		return oomError(runCtx);
	return OBJ_VAL(str);
}

//--- Bool ----------------------

static Value boolToString(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;

	bool b = AS_BOOL(getValueArg(args, 0));
	return b ? OBJ_VAL(vm->builtins.trueString) : OBJ_VAL(vm->builtins.falseString);
}

//--- Throwable -----------------

static Value throwableInit(Args *args) {
	RunCtx *runCtx = args->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjString *msg = AS_STRING(getValueArg(args, 1));

	ObjString *msgName = copyString(runCtx, ELOX_USTR_AND_LEN("message"));
	if (ELOX_UNLIKELY(msgName == NULL))
		return oomError(runCtx);
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	PUSH_TEMP(temps, protectedName, OBJ_VAL(msgName));
	setInstanceField(inst, msgName, OBJ_VAL(msg));
	releaseTemps(&temps);
	return OBJ_VAL(inst);
}

//--- Exception -----------------

static Value exceptionInit(Args *args) {
	RunCtx *runCtx = args->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjString *msg = AS_STRING(getValueArg(args, 1));

	ObjString *msgName = copyString(runCtx, ELOX_USTR_AND_LEN("message"));
	if (ELOX_UNLIKELY(msgName == NULL))
		return oomError(runCtx);
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	PUSH_TEMP(temps, protectedName, OBJ_VAL(msgName));
	setInstanceField(inst, msgName, OBJ_VAL(msg));
	releaseTemps(&temps);
	return OBJ_VAL(inst);
}

//--- Error ---------------------

static Value errorInit(Args *args) {
	RunCtx *runCtx = args->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjString *msg = AS_STRING(getValueArg(args, 1));

	Value superInit = AS_CLASS(inst->clazz->super)->initializer;
	if (!IS_NIL(superInit)) {
		push(fiber, OBJ_VAL(inst));
		push(fiber, OBJ_VAL(msg));
		bool wasNative;
		callMethod(runCtx, AS_OBJ(superInit), 1, 0, &wasNative);
		pop(fiber);
	}

	return OBJ_VAL(inst);
}

//--- Map -----------------------

static Value mapIteratorHasNext(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;

	struct MapIterator *mi = &vm->builtins.mapIterator;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjMap *map = AS_MAP(inst->fields.values[mi->_map]);
	int current = AS_NUMBER(inst->fields.values[mi->_current]);

	TableEntry *entry;
	int32_t nextIndex = valueTableGetNext(&map->items, current, &entry);

	return BOOL_VAL(nextIndex >= 0);
}

static Value mapIteratorNext(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	struct MapIterator *mi = &vm->builtins.mapIterator;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjMap *map = AS_MAP(inst->fields.values[mi->_map]);
	int current = AS_NUMBER(inst->fields.values[mi->_current]);
	uint32_t modCount = AS_NUMBER(inst->fields.values[mi->_modCount]);

	if (ELOX_UNLIKELY(modCount != map->items.modCount))
		return runtimeError(runCtx, "Map modified during iteration");

	TableEntry *entry;
	int nextIndex = valueTableGetNext(&map->items, current, &entry);

	inst->fields.values[mi->_current] = NUMBER_VAL(nextIndex);

	ObjArray *ret = newArray(runCtx, 2, OBJ_TUPLE);
	if (ELOX_UNLIKELY(ret == NULL))
		return oomError(runCtx);
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	PUSH_TEMP(temps, protectedRet, OBJ_VAL(ret));
	// array pre-allocated, won't fail
	appendToArray(runCtx, ret, entry->key);
	appendToArray(runCtx, ret, entry->value);
	releaseTemps(&temps);
	return OBJ_VAL(ret);
}

static Value mapSize(Args *args) {
	ObjMap *inst = AS_MAP(getValueArg(args, 0));
	return NUMBER_VAL(inst->items.liveCount);
}

static Value mapPut(Args *args) {
	RunCtx *runCtx = args->runCtx;

	ObjMap *inst = AS_MAP(getValueArg(args, 0));
	Value key = getValueArg(args, 1);
	Value val = getValueArg(args, 2);

	Error error = ERROR_INITIALIZER(runCtx);
	valueTableSet(&inst->items, key, val, &error);
	if (ELOX_UNLIKELY(error.raised))
		return EXCEPTION_VAL;

	return NIL_VAL;
}

static Value mapRemove(Args *args) {
	RunCtx *runCtx = args->runCtx;

	ObjMap *inst = AS_MAP(getValueArg(args, 0));
	Value key = getValueArg(args, 1);

	Error error = ERROR_INITIALIZER(runCtx);
	bool deleted = valueTableDelete(&inst->items, key, &error);
	if (ELOX_UNLIKELY(error.raised))
		return EXCEPTION_VAL;

	return BOOL_VAL(deleted);
}

static Value mapIterator(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;
	struct MapIterator *mi = &vm->builtins.mapIterator;

	ObjMap *inst = AS_MAP(getValueArg(args, 0));

	ObjInstance *iter = newInstance(runCtx, mi->_class);
	if (ELOX_UNLIKELY(iter == NULL))
		return oomError(runCtx);
	iter->fields.values[mi->_map] = OBJ_VAL(inst);
	iter->fields.values[mi->_current] = NUMBER_VAL(0);
	iter->fields.values[mi->_modCount] = NUMBER_VAL(inst->items.modCount);
	return OBJ_VAL(iter);
}

static Value notImplementedMethod(Args *args) {
	RunCtx *runCtx = args->runCtx;

	return runtimeError(runCtx, "Not implemented");
}

suint16_t builtinConstant(RunCtx *runCtx, const String *name) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	suint16_t ret = -1;
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);

	ObjString *nameString = copyString(runCtx, name->chars, name->length);
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
	bool res = valueArrayPush(runCtx, &vm->builtinValues, UNDEFINED_VAL);
	if (ELOX_UNLIKELY(!res))
		goto cleanup;

	Error error = ERROR_INITIALIZER(runCtx);
	tableSet(&vm->builtinSymbols, nameString, NUMBER_VAL((double)newIndex), &error);
	if (ELOX_UNLIKELY(error.raised))
		goto cleanup;

#ifdef ELOX_DEBUG_PRINT_CODE
	eloxPrintf(runCtx, ELOX_IO_DEBUG, ">>>Builtin[%5u] (%.*s)\n", newIndex,
			   name->length, name->chars);
#endif

	ret = newIndex;

cleanup:
	releaseTemps(&temps);

	return ret;
}

static ObjClass *registerStaticClass(RunCtx *runCtx, const String *name, const String *moduleName,
									 ObjClass *super) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	bool isBuiltin = stringEquals(moduleName, &eloxBuiltinModule);
	ObjString *className = copyString(runCtx, name->chars, name->length);
	if (ELOX_UNLIKELY(className == NULL))
		return NULL;
	push(fiber, OBJ_VAL(className));
	ObjClass *clazz = newClass(runCtx, className);
	if (ELOX_UNLIKELY(clazz == NULL))
		return NULL;
	push(fiber, OBJ_VAL(clazz));

	if (isBuiltin) {
		suint16_t builtinIdx = builtinConstant(runCtx, name);
		if (ELOX_UNLIKELY(builtinIdx < 0))
			return NULL;
		vm->builtinValues.values[builtinIdx] = peek(fiber, 0);
	} else {
		uint16_t globalIdx = globalIdentifierConstant(runCtx, name, moduleName);
		vm->globalValues.values[globalIdx] = peek(fiber, 0);
	}

	popn(fiber, 2);
	Error error = ERROR_INITIALIZER(runCtx);
	if (super != NULL) {
		clazz->super = OBJ_VAL(super);
		clazz->classId = clazz->baseId * super->classId;
		for (int i = 0; i < super->fields.capacity; i++) {
			Entry *entry = &super->fields.entries[i];
			if (entry->key != NULL) {
				tableSet(&clazz->fields, entry->key, entry->value, &error);
				if (ELOX_UNLIKELY(error.raised)) {
					pop(fiber); // discard error
					return NULL;
				}
			}
		}
		tableAddAll(&super->methods, &clazz->methods, &error);
		if (ELOX_UNLIKELY(error.raised)) {
			pop(fiber); // discard error
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

bool registerBuiltins(RunCtx *runCtx) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	ErrorMsg errMsg = ERROR_MSG_INITIALIZER;

	vm->builtins.anonInitString = copyString(runCtx, ELOX_USTR_AND_LEN("$init"));
	RET_IF_OOM(vm->builtins.anonInitString);

	vm->builtins.iteratorString = copyString(runCtx, ELOX_USTR_AND_LEN("iterator"));
	RET_IF_OOM(vm->builtins.iteratorString);
	vm->builtins.hasNextString = copyString(runCtx, ELOX_USTR_AND_LEN("hasNext"));
	RET_IF_OOM(vm->builtins.hasNextString);
	vm->builtins.nextString = copyString(runCtx, ELOX_USTR_AND_LEN("next"));
	RET_IF_OOM(vm->builtins.nextString);

	vm->builtins.hashCodeString = copyString(runCtx, ELOX_USTR_AND_LEN("hashCode"));
	RET_IF_OOM(vm->builtins.hashCodeString);
	vm->builtins.equalsString = copyString(runCtx, ELOX_USTR_AND_LEN("equals"));
	RET_IF_OOM(vm->builtins.equalsString);
	vm->builtins.toStringString = copyString(runCtx, ELOX_USTR_AND_LEN("toString"));
	RET_IF_OOM(vm->builtins.toStringString);

	const String objectName = ELOX_STRING("Object");
	ObjClass *objectClass = registerStaticClass(runCtx, &objectName, &eloxBuiltinModule, NULL);
	RET_IF_OOM(objectClass);
	addNativeMethod(runCtx, objectClass, "toString", objectToString, 1, false, &errMsg);
	addNativeMethod(runCtx, objectClass, "hashCode", objectHashCode, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);

	const String iteratorName = ELOX_STRING("Iterator");
	ObjClass *iteratorClass = registerStaticClass(runCtx, &iteratorName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(iteratorClass);
	addNativeMethod(runCtx, iteratorClass, "hasNext", notImplementedMethod, 1, false, &errMsg);
	addNativeMethod(runCtx, iteratorClass, "next", notImplementedMethod, 1, false, &errMsg);
	addNativeMethod(runCtx, iteratorClass, "remove", notImplementedMethod, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.iteratorClass = iteratorClass;

	const String gmatchIteratorName = ELOX_STRING("$GmatchIterator");
	ObjClass *gmatchIteratorClass = registerStaticClass(runCtx, &gmatchIteratorName, &eloxBuiltinModule, iteratorClass);
	RET_IF_OOM(gmatchIteratorClass);
	vm->builtins.gmatchIterator = (struct GmatchIterator){
		._string = addClassField(runCtx, gmatchIteratorClass, "string", &errMsg),
		._pattern = addClassField(runCtx, gmatchIteratorClass, "pattern", &errMsg),
		._offset = addClassField(runCtx, gmatchIteratorClass, "offset", &errMsg),
		._cachedNext = addClassField(runCtx, gmatchIteratorClass, "cachedNext", &errMsg),
		._class = gmatchIteratorClass
	};
	addNativeMethod(runCtx, gmatchIteratorClass, "hasNext", gmatchIteratorHasNext, 1, false, &errMsg);
	addNativeMethod(runCtx, gmatchIteratorClass, "next", gmatchIteratorNext, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);

	const String stringName = ELOX_STRING("String");
	ObjClass *stringClass = registerStaticClass(runCtx, &stringName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(stringClass);
	addNativeMethod(runCtx, stringClass, "toString", stringToString, 1, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "hashCode", stringHashCode, 1, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "length", stringLength, 1, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "fmt", stringFmt, 1, true, &errMsg);
	addNativeMethod(runCtx, stringClass, "find", stringFind, 3, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "findMatch", stringFindMatch, 3, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "match", stringMatch, 3, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "gmatch", stringGmatch, 2, false, &errMsg);
	vm->builtins.stringGsub = addNativeMethod(runCtx, stringClass, "gsub", stringGsub, 4, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "startsWith", stringStartsWith, 2, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "endsWith", stringEndsWith, 2, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "upper", stringUpper, 1, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "lower", stringLower, 1, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "trim", stringTrim, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);

	vm->builtins.stringClass = stringClass;

	const String numberName = ELOX_STRING("Number");
	ObjClass *numberClass = registerStaticClass(runCtx, &numberName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(numberClass);
	addNativeMethod(runCtx, numberClass, "toString", numberToString, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.numberClass = numberClass;

	vm->builtins.trueString = copyString(runCtx, ELOX_USTR_AND_LEN("true"));
	RET_IF_OOM(vm->builtins.trueString);
	vm->builtins.falseString = copyString(runCtx, ELOX_USTR_AND_LEN("false"));
	RET_IF_OOM(vm->builtins.falseString);

	const String boolName = ELOX_STRING("Bool");
	ObjClass *boolClass = registerStaticClass(runCtx, &boolName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(boolClass);
	addNativeMethod(runCtx, boolClass, "toString", boolToString, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.boolClass = boolClass;

	const String instanceName = ELOX_STRING("$Instance");
	ObjClass *instanceClass = registerStaticClass(runCtx, &instanceName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(instanceClass);
	vm->builtins.instanceClass = instanceClass;

	const String className = ELOX_STRING("Class");
	ObjClass *classClass = registerStaticClass(runCtx, &className, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(classClass);
	vm->builtins.classClass = classClass;

	const String throwableName = ELOX_STRING("Throwable");
	ObjClass *throwableClass = registerStaticClass(runCtx, &throwableName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(throwableClass);
	addClassField(runCtx, throwableClass, "message", &errMsg);
	addNativeMethod(runCtx, throwableClass, "Throwable", throwableInit, 2, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.throwableClass = throwableClass;

	const String exceptionName = ELOX_STRING("Exception");
	ObjClass *exceptionClass = registerStaticClass(runCtx, &exceptionName, &eloxBuiltinModule, throwableClass);
	RET_IF_OOM(exceptionClass);
	addClassField(runCtx, exceptionClass, "stacktrace", &errMsg);
	addNativeMethod(runCtx, exceptionClass, "Exception", exceptionInit, 2, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.exceptionClass = exceptionClass;

	const String runtimeExceptionName = ELOX_STRING("RuntimeException");
	ObjClass *runtimeExceptionClass = registerStaticClass(runCtx, &runtimeExceptionName, &eloxBuiltinModule, exceptionClass);
	RET_IF_OOM(runtimeExceptionClass);
	vm->builtins.runtimeExceptionClass = runtimeExceptionClass;

	const String errorName = ELOX_STRING("Error");
	ObjClass *errorClass = registerStaticClass(runCtx, &errorName, &eloxBuiltinModule, throwableClass);
	RET_IF_OOM(errorClass);
	addNativeMethod(runCtx, errorClass, "Error", errorInit, 2, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.errorClass = errorClass;

	ObjInstance *oomErrorInst = newInstance(runCtx, errorClass);
	RET_IF_OOM(oomErrorInst);
	push(fiber, OBJ_VAL(oomErrorInst));
	ObjString *oomErrorMsg = copyString(runCtx, ELOX_USTR_AND_LEN("Out of memory"));
	RET_IF_OOM(oomErrorMsg);
	push(fiber, OBJ_VAL(oomErrorMsg));
	bool wasNative;
	callMethod(runCtx, AS_OBJ(errorClass->initializer), 1, 0, &wasNative);
	pop(fiber);
	vm->builtins.oomError = oomErrorInst;

	const String arrayIteratorName = ELOX_STRING("$ArrayIterator");
	ObjClass *arrayIteratorClass = registerStaticClass(runCtx, &arrayIteratorName, &eloxBuiltinModule, iteratorClass);
	RET_IF_OOM(arrayIteratorClass);
	vm->builtins.arrayIterator = (struct ArrayIterator){
		._array = addClassField(runCtx, arrayIteratorClass, "array", &errMsg),
		._cursor = addClassField(runCtx, arrayIteratorClass, "cursor", &errMsg),
		._lastRet = addClassField(runCtx, arrayIteratorClass, "lastRet", &errMsg),
		._modCount = addClassField(runCtx, arrayIteratorClass, "modCount", &errMsg),
		._class = arrayIteratorClass
	};
	addNativeMethod(runCtx, arrayIteratorClass, "hasNext", arrayIteratorHasNext, 1, false, &errMsg);
	addNativeMethod(runCtx, arrayIteratorClass, "next", arrayIteratorNext, 1, false, &errMsg);
	addNativeMethod(runCtx, arrayIteratorClass, "remove", arrayIteratorRemove, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);

	const String arrayName = ELOX_STRING("Array");
	ObjClass *arrayClass = registerStaticClass(runCtx, &arrayName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(arrayClass);
	addNativeMethod(runCtx, arrayClass, "length", arrayLength, 1, false, &errMsg);
	addNativeMethod(runCtx, arrayClass, "add", arrayAdd, 2, false, &errMsg);
	addNativeMethod(runCtx, arrayClass, "removeAt", arrayRemoveAt, 2, false, &errMsg);
	addNativeMethod(runCtx, arrayClass, "iterator", arrayIterator, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.arrayClass = arrayClass;

	const String tupleName = ELOX_STRING("Tuple");
	ObjClass *tupleClass = registerStaticClass(runCtx, &tupleName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(tupleClass);
	addNativeMethod(runCtx, tupleClass, "length", arrayLength, 1, false, &errMsg);
	addNativeMethod(runCtx, tupleClass, "iterator", arrayIterator, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.tupleClass = tupleClass;

	const String mapIteratorName = ELOX_STRING("$MapIterator");
	ObjClass *mapIteratorClass = registerStaticClass(runCtx, &mapIteratorName, &eloxBuiltinModule, iteratorClass);
	RET_IF_OOM(mapIteratorClass);
	vm->builtins.mapIterator = (struct MapIterator){
		._map = addClassField(runCtx, mapIteratorClass, "map", &errMsg),
		._current = addClassField(runCtx, mapIteratorClass, "current", &errMsg),
		._modCount = addClassField(runCtx, mapIteratorClass, "modCount", &errMsg),
		._class = mapIteratorClass
	};
	addNativeMethod(runCtx, mapIteratorClass, "hasNext", mapIteratorHasNext, 1, false, &errMsg);
	addNativeMethod(runCtx, mapIteratorClass, "next", mapIteratorNext, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);

	const String mapName = ELOX_STRING("Map");
	ObjClass *mapClass = registerStaticClass(runCtx, &mapName, &eloxBuiltinModule, objectClass);
	RET_IF_OOM(mapClass);
	addNativeMethod(runCtx, mapClass, "size", mapSize, 1, false, &errMsg);
	addNativeMethod(runCtx, mapClass, "put", mapPut, 3, false, &errMsg);
	addNativeMethod(runCtx, mapClass, "remove", mapRemove, 2, false, &errMsg);
	addNativeMethod(runCtx, mapClass, "iterator", mapIterator, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.mapClass = mapClass;

	const String printName = ELOX_STRING("print");
	ObjNative *printFn = registerNativeFunction(runCtx, &printName,
												&eloxBuiltinModule, printNative, 0, true);
	RET_IF_OOM(printFn);

	const String printfName = ELOX_STRING("printf");
	ObjNative *printfFn = registerNativeFunction(runCtx, &printfName,
												 &eloxBuiltinModule, printFmt, 1, true);
	RET_IF_OOM(printfFn);

	const String assertName = ELOX_STRING("assert");
	ObjNative *assertFn = registerNativeFunction(runCtx, &assertName,
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
