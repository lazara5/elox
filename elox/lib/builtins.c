// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <time.h>
#include <math.h>
#include <inttypes.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

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

//--- HashMap -----------------------

static Value hashMapIteratorHasNext(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;

	struct HashMapIterator *mi = &vm->builtins.hashMapIterator;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjHashMap *map = AS_HASHMAP(inst->fields.values[mi->_map]);
	int current = AS_NUMBER(inst->fields.values[mi->_current]);

	TableEntry *entry;
	int32_t nextIndex = valueTableGetNext(&map->items, current, &entry);

	return BOOL_VAL(nextIndex >= 0);
}

static Value hashMapIteratorNext(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	struct HashMapIterator *mi = &vm->builtins.hashMapIterator;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjHashMap *map = AS_HASHMAP(inst->fields.values[mi->_map]);
	int current = AS_NUMBER(inst->fields.values[mi->_current]);
	uint32_t modCount = AS_NUMBER(inst->fields.values[mi->_modCount]);

	if (ELOX_UNLIKELY(modCount != map->items.modCount))
		return runtimeError(runCtx, "HashMap modified during iteration");

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

static Value hashMapSize(Args *args) {
	ObjHashMap *inst = AS_HASHMAP(getValueArg(args, 0));
	return NUMBER_VAL(inst->items.liveCount);
}

static Value hashMapPut(Args *args) {
	RunCtx *runCtx = args->runCtx;

	ObjHashMap *inst = AS_HASHMAP(getValueArg(args, 0));
	Value key = getValueArg(args, 1);
	Value val = getValueArg(args, 2);

	Error error = ERROR_INITIALIZER(runCtx);
	valueTableSet(&inst->items, key, val, &error);
	if (ELOX_UNLIKELY(error.raised))
		return EXCEPTION_VAL;

	return NIL_VAL;
}

static Value hashMapRemove(Args *args) {
	RunCtx *runCtx = args->runCtx;

	ObjHashMap *inst = AS_HASHMAP(getValueArg(args, 0));
	Value key = getValueArg(args, 1);

	Error error = ERROR_INITIALIZER(runCtx);
	bool deleted = valueTableDelete(&inst->items, key, &error);
	if (ELOX_UNLIKELY(error.raised))
		return EXCEPTION_VAL;

	return BOOL_VAL(deleted);
}

static Value hashMapIterator(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;
	struct HashMapIterator *mi = &vm->builtins.hashMapIterator;

	ObjHashMap *inst = AS_HASHMAP(getValueArg(args, 0));

	ObjInstance *iter = newInstance(runCtx, mi->_class);
	if (ELOX_UNLIKELY(iter == NULL))
		return oomError(runCtx);
	iter->fields.values[mi->_map] = OBJ_VAL(inst);
	iter->fields.values[mi->_current] = NUMBER_VAL(0);
	iter->fields.values[mi->_modCount] = NUMBER_VAL(inst->items.modCount);
	return OBJ_VAL(iter);
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

static ObjInterface *registerStaticInterface(RunCtx *runCtx, const String *name,
											 const String *moduleName) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	bool isBuiltin = stringEquals(moduleName, &eloxBuiltinModule);
	ObjString *intfName = copyString(runCtx, name->chars, name->length);
	if (ELOX_UNLIKELY(intfName == NULL))
		return NULL;
	push(fiber, OBJ_VAL(intfName));
	ObjInterface *intf = newInterface(runCtx, intfName);
	if (ELOX_UNLIKELY(intf == NULL))
		return NULL;
	push(fiber, OBJ_VAL(intf));

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

	return intf;
}

#define REGISTER_STATIC_CLASS(runCtx, name, moduleName, ...) \
	registerStaticClass(runCtx, name, moduleName, __VA_ARGS__, NULL)

static ObjClass *registerStaticClass(RunCtx *runCtx, bool abstract,
									 const String *name, const String *moduleName, ...) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	bool isBuiltin = stringEquals(moduleName, &eloxBuiltinModule);
	ObjString *className = copyString(runCtx, name->chars, name->length);
	if (ELOX_UNLIKELY(className == NULL))
		return NULL;
	push(fiber, OBJ_VAL(className));
	ObjClass *clazz = newClass(runCtx, className, abstract);
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

	va_list va;
	va_start(va, moduleName);
	ObjClass *ret = NULL;

	ObjClass *super;
	Obj *superObj = va_arg(va, Obj *);
	if (superObj == NULL)
		super = NULL;
	else {
		if (ELOX_UNLIKELY(!OBJ_IS_CLASS(superObj))) {
			// TODO: better error reporting
			goto cleanup;
		}
		super = (ObjClass *)superObj;
	}

	Obj *supertypes[ELOX_MAX_SUPERTYPES];
	uint16_t numSupertypes = 0;

	Error error = ERROR_INITIALIZER(runCtx);
	if (super != NULL) {
		clazz->super = OBJ_VAL(super);
		uint8_t typeDepth = super->typeInfo.depth + 1;
		clazz->typeInfo.depth = typeDepth;
		uint8_t superDisplaySize = ELOX_MAX(typeDepth, ELOX_CLASS_DISPLAY_SIZE);
		memcpy(clazz->typeInfo.rptDisplay, super->typeInfo.rptDisplay,
			   superDisplaySize * sizeof(Obj *));
		bool restricted = typeDepth >= ELOX_CLASS_DISPLAY_SIZE;
		bool superRestricted = super->typeInfo.depth >= ELOX_CLASS_DISPLAY_SIZE;
		if (!restricted) {
			clazz->typeInfo.rptDisplay[typeDepth] = (Obj *)clazz;
			clazz->typeCheckOffset = typeDepth;
		} else
			clazz->typeCheckOffset = ELOX_CLASS_DISPLAY_SIZE;
		if (superRestricted) {
			supertypes[numSupertypes] = (Obj *)super;
			numSupertypes++;
		}
		for (uint16_t s = 0; s < super->typeInfo.numRss; s++) {
			supertypes[numSupertypes] = super->typeInfo.rssList[s];
			numSupertypes++;
			// TODO: check
		}
		Obj *intfObj = va_arg(va, Obj *);
		while (intfObj != NULL) {
			if (ELOX_UNLIKELY(!OBJ_IS_INTERFACE(intfObj))) {
				// TODO: better error reporting
				goto cleanup;
			}

			bool duplicate = false;
			for (uint16_t s = 0; s < numSupertypes; s++) {
				if (intfObj == supertypes[s]) {
					duplicate = true;
					break;
				}
			}

			if (!duplicate) {
				supertypes[numSupertypes] = intfObj;
				numSupertypes++;
			}

			intfObj = va_arg(va, Obj *);
		}
		if (numSupertypes > 0) {
			clazz->typeInfo.rssList = ALLOCATE(runCtx, Obj *, numSupertypes);
			if (ELOX_UNLIKELY(clazz->typeInfo.rssList == NULL))
				goto cleanup;
			clazz->typeInfo.numRss = numSupertypes;
			memcpy(clazz->typeInfo.rssList, supertypes, numSupertypes * sizeof(Obj *));
		}
		for (int i = 0; i < super->fields.capacity; i++) {
			Entry *entry = &super->fields.entries[i];
			if (entry->key != NULL) {
				tableSet(&clazz->fields, entry->key, entry->value, &error);
				if (ELOX_UNLIKELY(error.raised)) {
					pop(fiber); // discard error
					goto cleanup;
				}
			}
		}
		tableAddAll(&super->methods, &clazz->methods, &error);
		if (ELOX_UNLIKELY(error.raised)) {
			pop(fiber); // discard error
			goto cleanup;
		}

		clazz->initializer = super->initializer;
	} else {
		clazz->typeInfo.depth = 0;
		clazz->typeInfo.rptDisplay[0] = (Obj *)clazz;
	}

	ret = clazz;

cleanup:
	va_end(va);
	return ret;
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
	vm->builtins.iteratorString = copyString(runCtx, ELOX_USTR_AND_LEN("iterator"));
	RET_IF_OOM(vm->builtins.iteratorString);

	const String objectName = ELOX_STRING("Object");
	ObjClass *objectClass = REGISTER_STATIC_CLASS(runCtx, false, &objectName, &eloxBuiltinModule);
	RET_IF_OOM(objectClass);
	addNativeMethod(runCtx, objectClass, "toString", objectToString, 0, false, &errMsg);
	addNativeMethod(runCtx, objectClass, "hashCode", objectHashCode, 0, false, &errMsg);
	RET_IF_RAISED(errMsg);

	const String iterableName = ELOX_STRING("Iterable");
	ObjInterface *iterableIntf = registerStaticInterface(runCtx, &iterableName, &eloxBuiltinModule);
	RET_IF_OOM(iterableIntf);
	addMethod(runCtx, iterableIntf, "iterator", 0, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.iterableIntf = iterableIntf;

	const String iteratorName = ELOX_STRING("Iterator");
	ObjInterface *iteratorIntf = registerStaticInterface(runCtx, &iteratorName, &eloxBuiltinModule);
	RET_IF_OOM(iteratorIntf);
	addMethod(runCtx, iteratorIntf, "hasNext", 0, false, &errMsg);
	addMethod(runCtx, iteratorIntf, "next", 0, false, &errMsg);
	addMethod(runCtx, iteratorIntf, "remove", 0, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.iteratorIntf = iteratorIntf;

	const String gmatchIteratorName = ELOX_STRING("$GmatchIterator");
	ObjClass *gmatchIteratorClass = REGISTER_STATIC_CLASS(runCtx, false, &gmatchIteratorName, &eloxBuiltinModule,
														  objectClass, iteratorIntf);
	RET_IF_OOM(gmatchIteratorClass);
	vm->builtins.gmatchIterator = (struct GmatchIterator){
		._string = addClassField(runCtx, gmatchIteratorClass, "string", &errMsg),
		._pattern = addClassField(runCtx, gmatchIteratorClass, "pattern", &errMsg),
		._offset = addClassField(runCtx, gmatchIteratorClass, "offset", &errMsg),
		._cachedNext = addClassField(runCtx, gmatchIteratorClass, "cachedNext", &errMsg),
		._class = gmatchIteratorClass
	};
	addNativeMethod(runCtx, gmatchIteratorClass, "hasNext", gmatchIteratorHasNext, 0, false, &errMsg);
	addNativeMethod(runCtx, gmatchIteratorClass, "next", gmatchIteratorNext, 0, false, &errMsg);
	RET_IF_RAISED(errMsg);

	const String stringName = ELOX_STRING("String");
	ObjClass *stringClass = REGISTER_STATIC_CLASS(runCtx, false, &stringName, &eloxBuiltinModule,
												  objectClass);
	RET_IF_OOM(stringClass);
	addNativeMethod(runCtx, stringClass, "toString", stringToString, 0, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "hashCode", stringHashCode, 0, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "length", stringLength, 0, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "fmt", stringFmt, 0, true, &errMsg);
	addNativeMethod(runCtx, stringClass, "find", stringFind, 2, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "findMatch", stringFindMatch, 2, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "match", stringMatch, 2, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "gmatch", stringGmatch, 1, false, &errMsg);
	vm->builtins.stringGsub = addNativeMethod(runCtx, stringClass, "gsub", stringGsub, 3, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "startsWith", stringStartsWith, 1, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "endsWith", stringEndsWith, 1, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "upper", stringUpper, 0, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "lower", stringLower, 0, false, &errMsg);
	addNativeMethod(runCtx, stringClass, "trim", stringTrim, 0, false, &errMsg);
	RET_IF_RAISED(errMsg);

	vm->builtins.stringClass = stringClass;

	const String numberName = ELOX_STRING("Number");
	ObjClass *numberClass = REGISTER_STATIC_CLASS(runCtx, false, &numberName, &eloxBuiltinModule,
												  objectClass);
	RET_IF_OOM(numberClass);
	addNativeMethod(runCtx, numberClass, "toString", numberToString, 0, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.numberClass = numberClass;

	vm->builtins.trueString = copyString(runCtx, ELOX_USTR_AND_LEN("true"));
	RET_IF_OOM(vm->builtins.trueString);
	vm->builtins.falseString = copyString(runCtx, ELOX_USTR_AND_LEN("false"));
	RET_IF_OOM(vm->builtins.falseString);

	const String boolName = ELOX_STRING("Bool");
	ObjClass *boolClass = REGISTER_STATIC_CLASS(runCtx, false, &boolName, &eloxBuiltinModule,
												objectClass);
	RET_IF_OOM(boolClass);
	addNativeMethod(runCtx, boolClass, "toString", boolToString, 0, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.boolClass = boolClass;

	const String instanceName = ELOX_STRING("$Instance");
	ObjClass *instanceClass = REGISTER_STATIC_CLASS(runCtx, false, &instanceName, &eloxBuiltinModule,
													objectClass);
	RET_IF_OOM(instanceClass);
	vm->builtins.instanceClass = instanceClass;

	const String className = ELOX_STRING("Class");
	ObjClass *classClass = REGISTER_STATIC_CLASS(runCtx, false, &className, &eloxBuiltinModule,
												 objectClass);
	RET_IF_OOM(classClass);
	vm->builtins.classClass = classClass;

	const String throwableName = ELOX_STRING("Throwable");
	ObjClass *throwableClass = REGISTER_STATIC_CLASS(runCtx, false, &throwableName, &eloxBuiltinModule,
													 objectClass);
	RET_IF_OOM(throwableClass);
	addClassField(runCtx, throwableClass, "message", &errMsg);
	addNativeMethod(runCtx, throwableClass, "Throwable", throwableInit, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.throwableClass = throwableClass;

	const String exceptionName = ELOX_STRING("Exception");
	ObjClass *exceptionClass = REGISTER_STATIC_CLASS(runCtx, false, &exceptionName, &eloxBuiltinModule,
													 throwableClass);
	RET_IF_OOM(exceptionClass);
	addClassField(runCtx, exceptionClass, "stacktrace", &errMsg);
	addNativeMethod(runCtx, exceptionClass, "Exception", exceptionInit, 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.exceptionClass = exceptionClass;

	const String runtimeExceptionName = ELOX_STRING("RuntimeException");
	ObjClass *runtimeExceptionClass = REGISTER_STATIC_CLASS(runCtx, false, &runtimeExceptionName, &eloxBuiltinModule,
															exceptionClass);
	RET_IF_OOM(runtimeExceptionClass);
	vm->builtins.runtimeExceptionClass = runtimeExceptionClass;

	const String errorName = ELOX_STRING("Error");
	ObjClass *errorClass = REGISTER_STATIC_CLASS(runCtx, false, &errorName, &eloxBuiltinModule,
												 throwableClass);
	RET_IF_OOM(errorClass);
	addNativeMethod(runCtx, errorClass, "Error", errorInit, 1, false, &errMsg);
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
	ObjClass *arrayIteratorClass = REGISTER_STATIC_CLASS(runCtx, false, &arrayIteratorName, &eloxBuiltinModule,
														 objectClass, iteratorIntf);
	RET_IF_OOM(arrayIteratorClass);
	vm->builtins.arrayIterator = (struct ArrayIterator){
		._array = addClassField(runCtx, arrayIteratorClass, "array", &errMsg),
		._cursor = addClassField(runCtx, arrayIteratorClass, "cursor", &errMsg),
		._lastRet = addClassField(runCtx, arrayIteratorClass, "lastRet", &errMsg),
		._modCount = addClassField(runCtx, arrayIteratorClass, "modCount", &errMsg),
		._class = arrayIteratorClass
	};
	addNativeMethod(runCtx, arrayIteratorClass, "hasNext", arrayIteratorHasNext, 0, false, &errMsg);
	addNativeMethod(runCtx, arrayIteratorClass, "next", arrayIteratorNext, 0, false, &errMsg);
	addNativeMethod(runCtx, arrayIteratorClass, "remove", arrayIteratorRemove, 0, false, &errMsg);
	RET_IF_RAISED(errMsg);

	const String arrayName = ELOX_STRING("Array");
	ObjClass *arrayClass = REGISTER_STATIC_CLASS(runCtx, false, &arrayName, &eloxBuiltinModule,
												 objectClass);
	RET_IF_OOM(arrayClass);
	addNativeMethod(runCtx, arrayClass, "length", arrayLength, 0, false, &errMsg);
	addNativeMethod(runCtx, arrayClass, "add", arrayAdd, 1, false, &errMsg);
	addNativeMethod(runCtx, arrayClass, "removeAt", arrayRemoveAt, 1, false, &errMsg);
	addNativeMethod(runCtx, arrayClass, "iterator", arrayIterator, 0, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.arrayClass = arrayClass;

	const String tupleName = ELOX_STRING("Tuple");
	ObjClass *tupleClass = REGISTER_STATIC_CLASS(runCtx, false, &tupleName, &eloxBuiltinModule,
												 objectClass);
	RET_IF_OOM(tupleClass);
	addNativeMethod(runCtx, tupleClass, "length", arrayLength, 0, false, &errMsg);
	addNativeMethod(runCtx, tupleClass, "iterator", arrayIterator, 0, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.tupleClass = tupleClass;

	const String hashMapIteratorName = ELOX_STRING("$HashMapIterator");
	ObjClass *hashMapIteratorClass = REGISTER_STATIC_CLASS(runCtx, false, &hashMapIteratorName, &eloxBuiltinModule,
														   objectClass, iteratorIntf);
	RET_IF_OOM(hashMapIteratorClass);
	vm->builtins.hashMapIterator = (struct HashMapIterator){
		._map = addClassField(runCtx, hashMapIteratorClass, "map", &errMsg),
		._current = addClassField(runCtx, hashMapIteratorClass, "current", &errMsg),
		._modCount = addClassField(runCtx, hashMapIteratorClass, "modCount", &errMsg),
		._class = hashMapIteratorClass
	};
	addNativeMethod(runCtx, hashMapIteratorClass, "hasNext", hashMapIteratorHasNext, 0, false, &errMsg);
	addNativeMethod(runCtx, hashMapIteratorClass, "next", hashMapIteratorNext, 0, false, &errMsg);
	RET_IF_RAISED(errMsg);

	const String mapName = ELOX_STRING("Map");
	ObjInterface *mapIntf = registerStaticInterface(runCtx, &mapName, &eloxBuiltinModule);
	RET_IF_OOM(mapIntf);
	addMethod(runCtx, mapIntf, "size", 0, false, &errMsg);
	addMethod(runCtx, mapIntf, "put", 2, false, &errMsg);
	addMethod(runCtx, mapIntf, "remove", 1, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.mapIntf = mapIntf;

	const String hashMapName = ELOX_STRING("HashMap");
	ObjClass *hashMapClass = REGISTER_STATIC_CLASS(runCtx, false, &hashMapName, &eloxBuiltinModule,
												   objectClass, mapIntf);
	RET_IF_OOM(hashMapClass);
	addNativeMethod(runCtx, hashMapClass, "size", hashMapSize, 0, false, &errMsg);
	addNativeMethod(runCtx, hashMapClass, "put", hashMapPut, 2, false, &errMsg);
	addNativeMethod(runCtx, hashMapClass, "remove", hashMapRemove, 1, false, &errMsg);
	addNativeMethod(runCtx, hashMapClass, "iterator", hashMapIterator, 0, false, &errMsg);
	RET_IF_RAISED(errMsg);
	vm->builtins.hashMapClass = hashMapClass;

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

	vm->builtins.hasNextString = NULL;
	vm->builtins.nextString = NULL;

	vm->builtins.hashCodeString = NULL;
	vm->builtins.equalsString = NULL;
	vm->builtins.toStringString = NULL;
	vm->builtins.iteratorString = NULL;

	vm->builtins.iterableIntf = NULL;
	vm->builtins.iteratorIntf = NULL;

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
	vm->builtins.hashMapIterator._class = NULL;
	vm->builtins.mapIntf = NULL;
	vm->builtins.hashMapClass = NULL;
}
