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
	return b ? OBJ_VAL(vm->builtins.trueStr) : OBJ_VAL(vm->builtins.falseStr);
}

//--- StackTraceElement ---------

static Value stackTraceElementInit(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjString *fileName = AS_STRING(getValueArg(args, 1));
	double lineNumber = AS_NUMBER(getValueArg(args, 2));
	ObjString *functionName = AS_STRING(getValueArg(args, 3));

	inst->fields.values[vm->builtins.biStackTraceElement._fileName] = OBJ_VAL(fileName);
	inst->fields.values[vm->builtins.biStackTraceElement._lineNumber] = NUMBER_VAL(lineNumber);
	inst->fields.values[vm->builtins.biStackTraceElement._functionName] = OBJ_VAL(functionName);

	return OBJ_VAL(inst);
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

static Value throwableToString(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));

	HeapCString ret;
	if (ELOX_UNLIKELY(!initHeapString(runCtx, &ret)))
		return oomError(runCtx);
	ObjClass *clazz = inst->clazz;
	heapStringAddFmt(runCtx, &ret, "%.*s", clazz->name->string.length, clazz->name->string.chars);
	Value msg;
	if (getInstanceValue(inst, vm->builtins.biThrowable.messageStr, &msg)) {
		if (!IS_NIL(msg)) {
			Error error = ERROR_INITIALIZER(runCtx);
			size_t crtStack = saveStack(fiber);
			Value msgStrVal = toString(msg, &error);
			if (ELOX_UNLIKELY(error.raised)) {
				restoreStack(fiber, crtStack);
				heapStringAddFmt(runCtx, &ret, ": Error retrieving error message");
			} else {
				ObjString *exStr = AS_STRING(msgStrVal);
				heapStringAddFmt(runCtx, &ret, ": %.*s", exStr->string.length, exStr->string.chars);
			}
		}
	}
	ObjString *str = takeString(runCtx, ret.chars, ret.length, ret.capacity);
	if (ELOX_UNLIKELY(str == NULL))
		return oomError(runCtx);
	return OBJ_VAL(str);
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

static Value exceptionPrintStackTrace(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;

	Value instVal = getValueArg(args, 0);

	Error error = ERROR_INITIALIZER(runCtx);
	Value exStrVal = toString(instVal, &error);
	ELOX_IF_RAISED_RET_VAL(&error, EXCEPTION_VAL);
	ObjString *exStr = AS_STRING(exStrVal);
	eloxPrintf(runCtx, ELOX_IO_ERR, "%.*s\n", exStr->string.length, exStr->string.chars);

	ObjInstance *inst = AS_INSTANCE(instVal);
	Value stVal;
	if (getInstanceValue(inst, vm->builtins.biException.stacktraceStr, &stVal)) {
		if (IS_ARRAY(stVal)) {
			ObjArray *st = AS_ARRAY(stVal);
			for (int32_t i = 0; i < st->size; i++) {
				ObjInstance *elem = AS_INSTANCE(st->items[i]);
				eloxPrintf(runCtx, ELOX_IO_ERR, "\tat ");
				ObjString *functionName = AS_STRING(elem->fields.values[vm->builtins.biStackTraceElement._functionName]);
				eloxPrintf(runCtx, ELOX_IO_ERR, "%.*s", functionName->string.length, functionName->string.chars);
				ObjString *fileName = AS_STRING(elem->fields.values[vm->builtins.biStackTraceElement._fileName]);
				int lineNumber = (int)AS_NUMBER(elem->fields.values[vm->builtins.biStackTraceElement._lineNumber]);
				eloxPrintf(runCtx, ELOX_IO_ERR, " (%.*s:%d)\n",
						   fileName->string.length, fileName->string.chars, lineNumber);
			}
		}
	}

	return NIL_VAL;
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

	struct BIHashMapIterator *mi = &vm->builtins.biHashMapIterator;

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

	struct BIHashMapIterator *mi = &vm->builtins.biHashMapIterator;

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
	struct BIHashMapIterator *mi = &vm->builtins.biHashMapIterator;

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

static ObjInterface *registerStaticInterface(RunCtx *runCtx, ObjString *intfName,
											 const String *moduleName, ErrorMsg *errorMsg) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	if (ELOX_UNLIKELY(errorMsg->raised))
		return NULL;

	bool isBuiltin = stringEquals(moduleName, &eloxBuiltinModule);

	ObjInterface *intf = newInterface(runCtx, intfName);
	if (ELOX_UNLIKELY(intf == NULL))
		ELOX_RAISE_MSG_RET_VAL(errorMsg, "Out of memory", NULL);
	push(fiber, OBJ_VAL(intf));

	if (isBuiltin) {
		suint16_t builtinIdx = builtinConstant(runCtx, &intfName->string);
		if (ELOX_UNLIKELY(builtinIdx < 0)) {
			pop(fiber);
			ELOX_RAISE_MSG_RET_VAL(errorMsg, "Out of memory", NULL);
		}
		vm->builtinValues.values[builtinIdx] = peek(fiber, 0);
	} else {
		uint16_t globalIdx = globalIdentifierConstant(runCtx, &intfName->string, moduleName);
		vm->globalValues.values[globalIdx] = peek(fiber, 0);
	}

	pop(fiber);

	return intf;
}

#define REGISTER_STATIC_CLASS(runCtx, name, moduleName, errorMsg, ...) \
	registerStaticClass(runCtx, name, moduleName, errorMsg, __VA_ARGS__, NULL)

static const char *rscNoErr = "";
static const char *rscErrSuper = "Super is not a class";
static const char *rscErrIntf = "Interface argument is not an interface";
static const char *rscErrOOM = "Out of memory";

static ObjClass *registerStaticClass(RunCtx *runCtx, bool abstract,
									 ObjString *className, const String *moduleName,
									 ErrorMsg *errorMsg, ...) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	if (ELOX_UNLIKELY(errorMsg->raised))
		return NULL;

	bool isBuiltin = stringEquals(moduleName, &eloxBuiltinModule);

	ObjClass *clazz = newClass(runCtx, className, abstract);
	if (ELOX_UNLIKELY(clazz == NULL))
		ELOX_RAISE_MSG_RET_VAL(errorMsg, "Out of memory", NULL);

	push(fiber, OBJ_VAL(clazz));

	if (isBuiltin) {
		suint16_t builtinIdx = builtinConstant(runCtx, &className->string);
		if (ELOX_UNLIKELY(builtinIdx < 0))
			ELOX_RAISE_MSG_RET_VAL(errorMsg, "Out of memory", NULL);
		vm->builtinValues.values[builtinIdx] = peek(fiber, 0);
	} else {
		uint16_t globalIdx = globalIdentifierConstant(runCtx, &className->string, moduleName);
		vm->globalValues.values[globalIdx] = peek(fiber, 0);
	}

	pop(fiber);

	va_list va;
	va_start(va, errorMsg);
	ObjClass *ret = NULL;
	const char *rscErr = rscNoErr;

	ObjClass *super;
	Obj *superObj = va_arg(va, Obj *);
	if (superObj == NULL)
		super = NULL;
	else {
		if (ELOX_UNLIKELY(superObj->type != OBJ_CLASS)) {
			rscErr = rscErrSuper;
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
			if (ELOX_UNLIKELY(intfObj->type != OBJ_INTERFACE)) {
				rscErr = rscErrIntf;
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
			if (ELOX_UNLIKELY(clazz->typeInfo.rssList == NULL)) {
				rscErr = rscErrOOM;
				goto cleanup;
			}
			clazz->typeInfo.numRss = numSupertypes;
			memcpy(clazz->typeInfo.rssList, supertypes, numSupertypes * sizeof(Obj *));
		}
		for (int i = 0; i < super->fields.capacity; i++) {
			Entry *entry = &super->fields.entries[i];
			if (entry->key != NULL) {
				tableSet(&clazz->fields, entry->key, entry->value, &error);
				if (ELOX_UNLIKELY(error.raised)) {
					pop(fiber); // discard error
					rscErr = rscErrOOM;
					goto cleanup;
				}
			}
		}
		tableAddAll(&super->methods, &clazz->methods, &error);
		if (ELOX_UNLIKELY(error.raised)) {
			pop(fiber); // discard error
			rscErr = rscErrOOM;
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
	if (ret == NULL)
		ELOX_RAISE_STRMSG(errorMsg, rscErr);
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

	struct Builtins *bi = &vm->builtins;

	bi->anonInitString = copyString(runCtx, ELOX_USTR_AND_LEN("$init"));
	RET_IF_OOM(bi->anonInitString);

	bi->scriptString = copyString(runCtx, ELOX_USTR_AND_LEN("<script>"));
	RET_IF_OOM(bi->scriptString);

	ObjClass *objectClass;
	bi->biObject._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Object"), &errMsg);
	bi->biObject._class = objectClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biObject._nameStr, &eloxBuiltinModule, &errMsg);
	bi->biObject.toStringStr = internString(runCtx, ELOX_USTR_AND_LEN("toString"), &errMsg);
	bi->biObject.hashCodeStr = internString(runCtx, ELOX_USTR_AND_LEN("hashCode"), &errMsg);
	addNativeMethod(runCtx, objectClass, bi->biObject.toStringStr, objectToString, 0, false, &errMsg);
	addNativeMethod(runCtx, objectClass, bi->biObject.hashCodeStr, objectHashCode, 0, false, &errMsg);

	ObjInterface *iterableIntf;
	bi->biIterable._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Iterable"), &errMsg);
	bi->biIterable._intf = iterableIntf =
		registerStaticInterface(runCtx, bi->biIterable._nameStr, &eloxBuiltinModule, &errMsg);
	bi->biIterable.iteratorStr = internString(runCtx, ELOX_USTR_AND_LEN("iterator"), &errMsg),
	addMethod(runCtx, iterableIntf, bi->biIterable.iteratorStr, 0, false, &errMsg);

	ObjInterface *iteratorIntf;
	bi->biIterator._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Iterator"), &errMsg);
	bi->biIterator._intf = iteratorIntf =
		registerStaticInterface(runCtx, bi->biIterator._nameStr, &eloxBuiltinModule, &errMsg);
	bi->biIterator.hasNextStr = internString(runCtx, ELOX_USTR_AND_LEN("hasNext"), &errMsg);
	bi->biIterator.nextStr = internString(runCtx, ELOX_USTR_AND_LEN("next"), &errMsg);
	bi->biIterator.removeStr = internString(runCtx, ELOX_USTR_AND_LEN("remove"), &errMsg);
	addMethod(runCtx, iteratorIntf, bi->biIterator.hasNextStr, 0, false, &errMsg);
	addMethod(runCtx, iteratorIntf, bi->biIterator.nextStr, 0, false, &errMsg);
	addMethod(runCtx, iteratorIntf, bi->biIterator.removeStr, 0, false, &errMsg);

	ObjClass *gmatchIteratorClass;
	bi->biGmatchIterator._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("$GmatchIterator"), &errMsg);
	bi->biGmatchIterator._class = gmatchIteratorClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biGmatchIterator._nameStr, &eloxBuiltinModule, &errMsg,
							  objectClass, iteratorIntf);
	bi->biGmatchIterator.stringStr = internString(runCtx, ELOX_USTR_AND_LEN("string"), &errMsg);
	bi->biGmatchIterator.patternStr = internString(runCtx, ELOX_USTR_AND_LEN("pattern"), &errMsg);
	bi->biGmatchIterator.offsetStr = internString(runCtx, ELOX_USTR_AND_LEN("offset"), &errMsg);
	bi->biGmatchIterator.cachedNextStr = internString(runCtx, ELOX_USTR_AND_LEN("cachedNext"), &errMsg);
	bi->biGmatchIterator._string = addClassField(runCtx, gmatchIteratorClass,
												 bi->biGmatchIterator.stringStr, &errMsg),
	bi->biGmatchIterator._pattern = addClassField(runCtx, gmatchIteratorClass,
												  bi->biGmatchIterator.patternStr, &errMsg),
	bi->biGmatchIterator._offset = addClassField(runCtx, gmatchIteratorClass,
												 bi->biGmatchIterator.offsetStr, &errMsg),
	bi->biGmatchIterator._cachedNext = addClassField(runCtx, gmatchIteratorClass,
													 bi->biGmatchIterator.cachedNextStr, &errMsg),
	addNativeMethod(runCtx, gmatchIteratorClass, bi->biIterator.hasNextStr, gmatchIteratorHasNext, 0, false, &errMsg);
	addNativeMethod(runCtx, gmatchIteratorClass, bi->biIterator.nextStr, gmatchIteratorNext, 0, false, &errMsg);

	ObjClass *stringClass;
	bi->biString._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("String"), &errMsg);
	bi->biString._class = stringClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biString._nameStr, &eloxBuiltinModule, &errMsg,
							  objectClass);
	bi->biString.lengthStr = internString(runCtx, ELOX_USTR_AND_LEN("length"), &errMsg);
	bi->biString.fmtStr = internString(runCtx, ELOX_USTR_AND_LEN("fmt"), &errMsg);
	bi->biString.findStr = internString(runCtx, ELOX_USTR_AND_LEN("find"), &errMsg);
	bi->biString.findMatchStr = internString(runCtx, ELOX_USTR_AND_LEN("findMatch"), &errMsg);
	bi->biString.matchStr = internString(runCtx, ELOX_USTR_AND_LEN("match"), &errMsg);
	bi->biString.gmatchStr = internString(runCtx, ELOX_USTR_AND_LEN("gmatch"), &errMsg);
	bi->biString.gsubStr = internString(runCtx, ELOX_USTR_AND_LEN("gsub"), &errMsg);
	bi->biString.startsWithStr = internString(runCtx, ELOX_USTR_AND_LEN("startsWith"), &errMsg);
	bi->biString.endsWithStr = internString(runCtx, ELOX_USTR_AND_LEN("endsWith"), &errMsg);
	bi->biString.upperStr = internString(runCtx, ELOX_USTR_AND_LEN("upper"), &errMsg);
	bi->biString.lowerStr = internString(runCtx, ELOX_USTR_AND_LEN("lower"), &errMsg);
	bi->biString.trimStr = internString(runCtx, ELOX_USTR_AND_LEN("trim"), &errMsg);
	addNativeMethod(runCtx, stringClass, bi->biObject.toStringStr, stringToString, 0, false, &errMsg);
	addNativeMethod(runCtx, stringClass, bi->biObject.hashCodeStr, stringHashCode, 0, false, &errMsg);
	addNativeMethod(runCtx, stringClass, bi->biString.lengthStr, stringLength, 0, false, &errMsg);
	addNativeMethod(runCtx, stringClass, bi->biString.fmtStr, stringFmt, 0, true, &errMsg);
	addNativeMethod(runCtx, stringClass, bi->biString.findStr, stringFind, 2, false, &errMsg);
	addNativeMethod(runCtx, stringClass, bi->biString.findMatchStr, stringFindMatch, 2, false, &errMsg);
	addNativeMethod(runCtx, stringClass, bi->biString.matchStr, stringMatch, 2, false, &errMsg);
	addNativeMethod(runCtx, stringClass, bi->biString.gmatchStr, stringGmatch, 1, false, &errMsg);
	bi->biString._gsub =
		addNativeMethod(runCtx, stringClass, bi->biString.gsubStr, stringGsub, 3, false, &errMsg),
	addNativeMethod(runCtx, stringClass, bi->biString.startsWithStr, stringStartsWith, 1, false, &errMsg);
	addNativeMethod(runCtx, stringClass, bi->biString.endsWithStr, stringEndsWith, 1, false, &errMsg);
	addNativeMethod(runCtx, stringClass, bi->biString.upperStr, stringUpper, 0, false, &errMsg);
	addNativeMethod(runCtx, stringClass, bi->biString.lowerStr, stringLower, 0, false, &errMsg);
	addNativeMethod(runCtx, stringClass, bi->biString.trimStr, stringTrim, 0, false, &errMsg);

	ObjClass *numberClass;
	bi->biNumber._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Number"), &errMsg);
	bi->biNumber._class = numberClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biNumber._nameStr, &eloxBuiltinModule, &errMsg,
							  objectClass);
	addNativeMethod(runCtx, numberClass, bi->biObject.toStringStr, numberToString, 0, false, &errMsg);

	vm->builtins.trueStr = internString(runCtx, ELOX_USTR_AND_LEN("true"), &errMsg);
	vm->builtins.falseStr = internString(runCtx, ELOX_USTR_AND_LEN("false"), &errMsg);

	ObjClass *boolClass;
	bi->biBool._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Bool"), &errMsg);
	bi->biBool._class = boolClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biBool._nameStr, &eloxBuiltinModule, &errMsg,
							  objectClass);
	addNativeMethod(runCtx, boolClass, bi->biObject.toStringStr, boolToString, 0, false, &errMsg);

	ObjClass *instanceClass;
	bi->biInstance._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("$Instance"), &errMsg);
	bi->biInstance._class = instanceClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biInstance._nameStr, &eloxBuiltinModule, &errMsg,
							  objectClass);

	ObjClass *classClass;
	bi->biClass._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Class"), &errMsg);
	bi->biClass._class = classClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biClass._nameStr, &eloxBuiltinModule, &errMsg,
							  objectClass);

	ObjClass *stackTraceElementClass;
	bi->biStackTraceElement._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("StackTraceElement"), &errMsg);
	bi->biStackTraceElement._class = stackTraceElementClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biStackTraceElement._nameStr, &eloxBuiltinModule, &errMsg,
							  objectClass);
	bi->biStackTraceElement.fileNameStr = internString(runCtx, ELOX_USTR_AND_LEN("fileName"), &errMsg);
	bi->biStackTraceElement.lineNumberStr = internString(runCtx, ELOX_USTR_AND_LEN("lineNumber"), &errMsg);
	bi->biStackTraceElement.functionNameStr = internString(runCtx, ELOX_USTR_AND_LEN("functionName"), &errMsg);
	bi->biStackTraceElement._fileName = addClassField(runCtx, stackTraceElementClass,
													  bi->biStackTraceElement.fileNameStr, &errMsg);
	bi->biStackTraceElement._lineNumber = addClassField(runCtx, stackTraceElementClass,
														bi->biStackTraceElement.lineNumberStr, &errMsg);
	bi->biStackTraceElement._functionName = addClassField(runCtx, stackTraceElementClass,
														  bi->biStackTraceElement.functionNameStr, &errMsg);
	addNativeMethod(runCtx, stackTraceElementClass, bi->biStackTraceElement._nameStr, stackTraceElementInit,
					3, false, &errMsg);

	ObjClass *throwableClass;
	bi->biThrowable._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Throwable"), &errMsg);
	bi->biThrowable._class = throwableClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biThrowable._nameStr, &eloxBuiltinModule, &errMsg,
							  objectClass);
	bi->biThrowable.messageStr = internString(runCtx, ELOX_USTR_AND_LEN("message"), &errMsg);
	addClassField(runCtx, throwableClass, bi->biThrowable.messageStr, &errMsg);
	addNativeMethod(runCtx, throwableClass, bi->biThrowable._nameStr, throwableInit, 1, false, &errMsg);
	addNativeMethod(runCtx, throwableClass, bi->biObject.toStringStr, throwableToString, 0, false, &errMsg);

	ObjClass *exceptionClass;
	bi->biException._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Exception"), &errMsg);
	bi->biException._class = exceptionClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biException._nameStr, &eloxBuiltinModule, &errMsg,
							  throwableClass);
	bi->biException.stacktraceStr = internString(runCtx, ELOX_USTR_AND_LEN("stacktrace"), &errMsg);
	bi->biException.printStackTraceString = internString(runCtx, ELOX_USTR_AND_LEN("printStackTrace"), &errMsg),
	addClassField(runCtx, exceptionClass, bi->biException.stacktraceStr, &errMsg);
	addNativeMethod(runCtx, exceptionClass, bi->biException._nameStr, exceptionInit, 1, false, &errMsg);
	bi->biException._printStackTrace =
		addNativeMethod(runCtx, exceptionClass, bi->biException.printStackTraceString, exceptionPrintStackTrace, 0, false, &errMsg);

	ObjClass *runtimeExceptionClass;
	bi->biRuntimeException._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("RuntimeException"), &errMsg);
	bi->biRuntimeException._class = runtimeExceptionClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biRuntimeException._nameStr, &eloxBuiltinModule, &errMsg,
							  exceptionClass);

	ObjClass *errorClass;
	bi->biError._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Error"), &errMsg);
	bi->biError._class = errorClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biError._nameStr, &eloxBuiltinModule, &errMsg,
							  throwableClass);
	addNativeMethod(runCtx, errorClass, bi->biError._nameStr, errorInit, 1, false, &errMsg);

	RET_IF_RAISED(errMsg);
	bi->oomErrorMsg = internString(runCtx, ELOX_USTR_AND_LEN("Out of memory"), &errMsg);
	RET_IF_RAISED(errMsg);
	ObjInstance *oomErrorInst = newInstance(runCtx, errorClass);
	RET_IF_OOM(oomErrorInst);
	push(fiber, OBJ_VAL(oomErrorInst));
	push(fiber, OBJ_VAL(bi->oomErrorMsg));
	bool wasNative;
	callMethod(runCtx, AS_OBJ(errorClass->initializer), 1, 0, &wasNative);
	pop(fiber);
	vm->builtins.oomError = oomErrorInst;

	ObjClass *arrayIteratorClass;
	bi->biArrayIterator._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("$ArrayIterator"), &errMsg);
	bi->biArrayIterator._class = arrayIteratorClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biArrayIterator._nameStr, &eloxBuiltinModule, &errMsg,
							  objectClass, iteratorIntf);
	bi->biArrayIterator.arrayStr = internString(runCtx, ELOX_USTR_AND_LEN("array"), &errMsg);
	bi->biArrayIterator.cursorStr = internString(runCtx, ELOX_USTR_AND_LEN("cursor"), &errMsg);
	bi->biArrayIterator.lastRetStr = internString(runCtx, ELOX_USTR_AND_LEN("lastRet"), &errMsg);
	bi->biArrayIterator.modCountStr = internString(runCtx, ELOX_USTR_AND_LEN("modCount"), &errMsg);
	bi->biArrayIterator._array = addClassField(runCtx, arrayIteratorClass,
											   bi->biArrayIterator.arrayStr, &errMsg);
	bi->biArrayIterator._cursor = addClassField(runCtx, arrayIteratorClass,
												bi->biArrayIterator.cursorStr, &errMsg);
	bi->biArrayIterator._lastRet = addClassField(runCtx, arrayIteratorClass,
												 bi->biArrayIterator.lastRetStr, &errMsg);
	bi->biArrayIterator._modCount = addClassField(runCtx, arrayIteratorClass,
												  bi->biArrayIterator.modCountStr, &errMsg);
	addNativeMethod(runCtx, arrayIteratorClass, bi->biIterator.hasNextStr, arrayIteratorHasNext, 0, false, &errMsg);
	addNativeMethod(runCtx, arrayIteratorClass, bi->biIterator.nextStr, arrayIteratorNext, 0, false, &errMsg);
	addNativeMethod(runCtx, arrayIteratorClass, bi->biIterator.removeStr, arrayIteratorRemove, 0, false, &errMsg);

	ObjClass *arrayClass;
	bi->biArray._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Array"), &errMsg);
	bi->biArray._class = arrayClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biArray._nameStr, &eloxBuiltinModule, &errMsg,
							  objectClass);
	bi->biArray.lengthStr = internString(runCtx, ELOX_USTR_AND_LEN("length"), &errMsg);
	bi->biArray.addStr = internString(runCtx, ELOX_USTR_AND_LEN("add"), &errMsg);
	bi->biArray.removeAtStr = internString(runCtx, ELOX_USTR_AND_LEN("removeAt"), &errMsg);
	addNativeMethod(runCtx, arrayClass, bi->biArray.lengthStr, arrayLength, 0, false, &errMsg);
	addNativeMethod(runCtx, arrayClass, bi->biArray.addStr, arrayAdd, 1, false, &errMsg);
	addNativeMethod(runCtx, arrayClass, bi->biArray.removeAtStr, arrayRemoveAt, 1, false, &errMsg);
	addNativeMethod(runCtx, arrayClass, bi->biIterable.iteratorStr, arrayIterator, 0, false, &errMsg);

	ObjClass *tupleClass;
	bi->biTuple._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Tuple"), &errMsg);
	bi->biTuple._class = tupleClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biTuple._nameStr, &eloxBuiltinModule, &errMsg,
							  objectClass);
	bi->biTuple.lengthStr = internString(runCtx, ELOX_USTR_AND_LEN("length"), &errMsg);
	addNativeMethod(runCtx, tupleClass, bi->biTuple.lengthStr, arrayLength, 0, false, &errMsg);
	addNativeMethod(runCtx, tupleClass, bi->biIterable.iteratorStr, arrayIterator, 0, false, &errMsg);

	ObjClass *hashMapIteratorClass;
	bi->biHashMapIterator._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("$HashMapIterator"), &errMsg);
	bi->biHashMapIterator._class = hashMapIteratorClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biHashMapIterator._nameStr, &eloxBuiltinModule, &errMsg,
							  objectClass, iteratorIntf);
	bi->biHashMapIterator.mapStr = internString(runCtx, ELOX_USTR_AND_LEN("map"), &errMsg);
	bi->biHashMapIterator.currentStr = internString(runCtx, ELOX_USTR_AND_LEN("current"), &errMsg);
	bi->biHashMapIterator.modCountStr = internString(runCtx, ELOX_USTR_AND_LEN("modCount"), &errMsg);
	bi->biHashMapIterator._map = addClassField(runCtx, hashMapIteratorClass,
											   bi->biHashMapIterator.mapStr, &errMsg);
	bi->biHashMapIterator._current = addClassField(runCtx, hashMapIteratorClass,
												   bi->biHashMapIterator.currentStr, &errMsg);
	bi->biHashMapIterator._modCount = addClassField(runCtx, hashMapIteratorClass,
													bi->biHashMapIterator.modCountStr, &errMsg);
	bi->biHashMapIterator._class = hashMapIteratorClass;
	addNativeMethod(runCtx, hashMapIteratorClass, bi->biIterator.hasNextStr, hashMapIteratorHasNext, 0, false, &errMsg);
	addNativeMethod(runCtx, hashMapIteratorClass, bi->biIterator.nextStr, hashMapIteratorNext, 0, false, &errMsg);

	ObjInterface *mapIntf;
	bi->biMap._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Map"), &errMsg);
	bi->biMap._intf = mapIntf =
			registerStaticInterface(runCtx, bi->biMap._nameStr, &eloxBuiltinModule, &errMsg);
	bi->biMap.sizeStr = internString(runCtx, ELOX_USTR_AND_LEN("size"), &errMsg);
	bi->biMap.putStr = internString(runCtx, ELOX_USTR_AND_LEN("put"), &errMsg);
	bi->biMap.removeStr = internString(runCtx, ELOX_USTR_AND_LEN("remove"), &errMsg);
	addMethod(runCtx, mapIntf, bi->biMap.sizeStr, 0, false, &errMsg);
	addMethod(runCtx, mapIntf, bi->biMap.putStr, 2, false, &errMsg);
	addMethod(runCtx, mapIntf, bi->biMap.removeStr, 1, false, &errMsg);

	ObjClass *hashMapClass;
	bi->biHashMap._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("HashMap"), &errMsg);
	bi->biHashMap._class = hashMapClass =
		REGISTER_STATIC_CLASS(runCtx, false, bi->biHashMap._nameStr, &eloxBuiltinModule, &errMsg,
							  objectClass, mapIntf);
	addNativeMethod(runCtx, hashMapClass, bi->biMap.sizeStr, hashMapSize, 0, false, &errMsg);
	addNativeMethod(runCtx, hashMapClass, bi->biMap.putStr, hashMapPut, 2, false, &errMsg);
	addNativeMethod(runCtx, hashMapClass, bi->biMap.removeStr, hashMapRemove, 1, false, &errMsg);
	addNativeMethod(runCtx, hashMapClass, bi->biIterable.iteratorStr, hashMapIterator, 0, false, &errMsg);

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

	RET_IF_RAISED(errMsg);

	return true;
}

void clearBuiltins(VM *vm) {
	memset(&vm->builtins, 0, sizeof(vm->builtins));
}
