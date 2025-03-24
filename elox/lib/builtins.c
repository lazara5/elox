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
#include <elox/Class.h>

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
				return runtimeError(runCtx, NULL, "Assertion failed");
			else {
				EloxError error = ELOX_ERROR_INITIALIZER;
				Value strVal = toString(runCtx, getValueArg(args, 1), &error);
				if (ELOX_UNLIKELY(error.raised))
					return strVal;
				const char *str = AS_CSTRING(strVal);
				push(fiber, strVal);
				Value errorVal = runtimeError(runCtx, NULL, "Assertion failed: %s", str);
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
		return oomError(runCtx, NULL);
	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	heapStringAddFmt(runCtx, &ret, "%s@%u", inst->clazz->name->string.chars, inst->identityHash);
	ObjString *str = takeString(runCtx, ret.chars, ret.length, ret.capacity);
	if (ELOX_UNLIKELY(str == NULL))
		return oomError(runCtx, NULL);
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
		return oomError(runCtx, NULL);
	if (trunc(n) == n)
		heapStringAddFmt(runCtx, &ret, "%" PRId64, (int64_t)n);
	else
		heapStringAddFmt(runCtx, &ret, "%g", n);
	ObjString *str = takeString(runCtx, ret.chars, ret.length, ret.capacity);
	if (ELOX_UNLIKELY(str == NULL))
		return oomError(runCtx, NULL);
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

	inst->fields[vm->builtins.biStackTraceElement._fileName] = OBJ_VAL(fileName);
	inst->fields[vm->builtins.biStackTraceElement._lineNumber] = NUMBER_VAL(lineNumber);
	inst->fields[vm->builtins.biStackTraceElement._functionName] = OBJ_VAL(functionName);

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
		return oomError(runCtx, NULL);
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
		return oomError(runCtx, NULL);
	ObjClass *clazz = inst->clazz;
	heapStringAddFmt(runCtx, &ret, "%.*s", clazz->name->string.length, clazz->name->string.chars);
	Value msg;
	if (getInstanceValue(inst, vm->builtins.biThrowable.messageStr, &msg)) {
		if (!IS_NIL(msg)) {
			EloxError error = ELOX_ERROR_INITIALIZER;
			size_t savedStack = saveStack(fiber);
			Value msgStrVal = toString(runCtx, msg, &error);
			if (ELOX_UNLIKELY(error.raised)) {
				restoreStack(fiber, savedStack);
				heapStringAddFmt(runCtx, &ret, ": Error retrieving error message");
			} else {
				ObjString *exStr = AS_STRING(msgStrVal);
				heapStringAddFmt(runCtx, &ret, ": %.*s", exStr->string.length, exStr->string.chars);
			}
		}
	}
	ObjString *str = takeString(runCtx, ret.chars, ret.length, ret.capacity);
	if (ELOX_UNLIKELY(str == NULL))
		return oomError(runCtx, NULL);
	return OBJ_VAL(str);
}

//--- Exception -----------------

static Value exceptionInit(Args *args) {
	RunCtx *runCtx = args->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjString *msg;
	ELOX_GET_STRING_ARG_THROW_RET(&msg, args, 1);

	ObjString *msgName = copyString(runCtx, ELOX_USTR_AND_LEN("message"));
	if (ELOX_UNLIKELY(msgName == NULL))
		return oomError(runCtx, NULL);
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

	EloxError error = ELOX_ERROR_INITIALIZER;
	Value exStrVal = toString(runCtx, instVal, &error);
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
				ObjString *functionName = AS_STRING(elem->fields[vm->builtins.biStackTraceElement._functionName]);
				eloxPrintf(runCtx, ELOX_IO_ERR, "%.*s", functionName->string.length, functionName->string.chars);
				ObjString *fileName = AS_STRING(elem->fields[vm->builtins.biStackTraceElement._fileName]);
				int lineNumber = (int)AS_NUMBER(elem->fields[vm->builtins.biStackTraceElement._lineNumber]);
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

	Value superInit = inst->clazz->super->initializer;
	if (!IS_NIL(superInit)) {
		push(fiber, OBJ_VAL(inst));
		push(fiber, OBJ_VAL(msg));
		callMethod(runCtx, AS_OBJ(superInit), 1, 0);
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
	ObjHashMap *map = AS_HASHMAP(inst->fields[mi->_map]);
	int current = AS_NUMBER(inst->fields[mi->_current]);

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
	ObjHashMap *map = AS_HASHMAP(inst->fields[mi->_map]);
	int current = AS_NUMBER(inst->fields[mi->_current]);
	uint32_t modCount = AS_NUMBER(inst->fields[mi->_modCount]);

	if (ELOX_UNLIKELY(modCount != map->items.modCount))
		return runtimeError(runCtx, NULL, "HashMap modified during iteration");

	TableEntry *entry;
	int nextIndex = valueTableGetNext(&map->items, current, &entry);

	inst->fields[mi->_current] = NUMBER_VAL(nextIndex);

	ObjArray *ret = newArray(runCtx, 2, OBJ_TUPLE);
	if (ELOX_UNLIKELY(ret == NULL))
		return oomError(runCtx, NULL);
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

	EloxError error = ELOX_ERROR_INITIALIZER;
	valueTableSet(runCtx, &inst->items, key, val, &error);
	if (ELOX_UNLIKELY(error.raised))
		return EXCEPTION_VAL;

	return NIL_VAL;
}

static Value hashMapRemove(Args *args) {
	RunCtx *runCtx = args->runCtx;

	ObjHashMap *inst = AS_HASHMAP(getValueArg(args, 0));
	Value key = getValueArg(args, 1);

	EloxError error = ELOX_ERROR_INITIALIZER;
	bool deleted = valueTableDelete(runCtx, &inst->items, key, &error);
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
		return oomError(runCtx, NULL);
	iter->fields[mi->_map] = OBJ_VAL(inst);
	iter->fields[mi->_current] = NUMBER_VAL(0);
	iter->fields[mi->_modCount] = NUMBER_VAL(inst->items.modCount);
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

	EloxError error = ELOX_ERROR_INITIALIZER;
	tableSet(runCtx, &vm->builtinSymbols, nameString, NUMBER_VAL((double)newIndex), &error);
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

static EloxKlassHandle *openInterface(RunCtx *runCtx, ObjString *intfName, EloxError *error) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	if (ELOX_UNLIKELY(error->raised))
		return NULL;

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);

	ObjInterface *intf = newInterface(runCtx, intfName);
	ELOX_CHECK_RAISE_RET_VAL(intf != NULL, error, OOM(runCtx), NULL);

	PUSH_TEMP(temps, protectedIntf, OBJ_VAL(intf));

	EloxKlassHandle *ret = NULL;

	intf->openKlass = newOpenKlass(runCtx, (ObjKlass *)intf);
	ELOX_CHECK_RAISE_GOTO(intf->openKlass != NULL, error, OOM(runCtx), cleanup);
	intf->openKlass->error = error;

	EloxKlassHandle *handle = ALLOCATE(runCtx, EloxKlassHandle, 1);
	ELOX_CHECK_RAISE_GOTO(handle, error, OOM(runCtx), cleanup);

	handle->base.runCtx = runCtx;
	handle->base.type = KLASS_HANDLE;
	handle->klass = (ObjKlass *)intf;

	handleSetAdd(&vm->handles, (EloxHandle *)handle);

	ret = handle;

cleanup:
	releaseTemps(&temps);

	return ret;
}

// Intentionally omitting mandatory super to avoid empty vararg list issues
#define REGISTER_GLOBAL_CLASS(runCtx, abstract, name, moduleName, error,  ...) \
	registerGlobalClass(runCtx, abstract, name, moduleName, error, __VA_ARGS__, NULL)

static ObjKlass *klassCloseAndRegister(EloxKlassHandle *okh,
									   ObjString *name, const String *moduleName) {
	if (ELOX_UNLIKELY(okh == NULL))
		return NULL;

	RunCtx *runCtx = okh->base.runCtx;
	VM *vm = runCtx->vm;

	ObjKlass *klass = okh->klass;
	EloxError *error = klass->openKlass->error;
	ObjKlass *ret = NULL;

	closeOpenKlass(runCtx, klass, error);
	if (ELOX_UNLIKELY(error->raised))
		goto cleanup;

	bool isBuiltin = stringEquals(moduleName, &eloxBuiltinModule);
	if (isBuiltin) {
		suint16_t builtinIdx = builtinConstant(runCtx, &name->string);
		ELOX_CHECK_RAISE_GOTO(builtinIdx >= 0, error, OOM(runCtx), cleanup);
		vm->builtinValues.values[builtinIdx] = OBJ_VAL(klass);
	} else {
		suint16_t globalIdx = globalIdentifierConstant(runCtx, &name->string, moduleName);
		ELOX_CHECK_RAISE_GOTO(globalIdx >= 0, error, OOM(runCtx), cleanup);
		vm->globalValues.values[globalIdx] = OBJ_VAL(klass);
	}

	ret = klass;

cleanup:
	eloxReleaseHandle((EloxHandle *)okh);
	return ret;
}

// Intentionally omitting mandatory super to avoid empty vararg list issues
#define OPEN_CLASS(runCtx, abstract, name, error,  ...) \
	openClass(runCtx, abstract, name, error, __VA_ARGS__, NULL)


static EloxKlassHandle *openClass(RunCtx *runCtx, bool abstract,
								  ObjString *className, EloxError *error, ObjClass *super, ...) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	if (ELOX_UNLIKELY(error->raised))
		return NULL;

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);

	ObjClass *clazz = newClass(runCtx, className, abstract);
	ELOX_CHECK_RAISE_RET_VAL(clazz != NULL, error, OOM(runCtx), NULL);\
	clazz->openKlass->error = error;

	PUSH_TEMP(temps, protectedClass, OBJ_VAL(clazz));

	va_list va;
	va_start(va, super);
	EloxKlassHandle *ret = NULL;

	Obj *supertypes[ELOX_MAX_SUPERTYPES];
	uint16_t numSupertypes = 0;

	EloxError localError = ELOX_ERROR_INITIALIZER;
	if (super != NULL) {
		clazz->super = super;
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
			if (ELOX_UNLIKELY(numSupertypes > ELOX_MAX_SUPERTYPES - 1))
				ELOX_RAISE_GOTO(error, RTERR(runCtx, "Cannot have more than %u supertypes",
											 ELOX_MAX_SUPERTYPES), cleanup);
			supertypes[numSupertypes] = super->typeInfo.rssList[s];
			numSupertypes++;
		}
		Obj *intfObj = va_arg(va, Obj *);
		while (intfObj != NULL) {
			ELOX_CHECK_RAISE_GOTO(intfObj->type == OBJ_INTERFACE, error,
								  RTERR(runCtx, "Interface argument is not an interface"), cleanup);

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
			ELOX_CHECK_RAISE_GOTO(clazz->typeInfo.rssList != NULL, error, OOM(runCtx), cleanup);
			clazz->typeInfo.numRss = numSupertypes;
			memcpy(clazz->typeInfo.rssList, supertypes, numSupertypes * sizeof(Obj *));
		}
		for (int i = 0; i < super->props.capacity; i++) {
			PropEntry *entry = &super->props.entries[i];
			if (entry->key != NULL) {
				if (entry->value.type == ELOX_PROP_FIELD) {
					size_t savedStack = saveStack(fiber);
					propTableSet(runCtx, &clazz->props,
								 entry->key, entry->value, &localError);
					if (ELOX_UNLIKELY(localError.raised)) {
						localError.discardException(fiber, savedStack);
						ELOX_RAISE_GOTO(error, OOM(runCtx), cleanup);
					}
					clazz->numFields++;
				} else {
					bool set = valueArraySet(runCtx, &clazz->classData, entry->value.index,
											 super->classData.values[entry->value.index]);
					ELOX_CHECK_RAISE_GOTO(set, error, OOM(runCtx), cleanup);
					clazz->tables[ELOX_DT_CLASS] = clazz->classData.values;
					size_t savedStack = saveStack(fiber);
					propTableSet(runCtx, &clazz->props, entry->key, entry->value, &localError);
					if (ELOX_UNLIKELY(localError.raised)) {
						localError.discardException(fiber, savedStack);
						ELOX_RAISE_GOTO(error, OOM(runCtx), cleanup);
					}
				}
			}
		}

		clazz->initializer = super->initializer;
	} else {
		clazz->typeInfo.depth = 0;
		clazz->typeInfo.rptDisplay[0] = (Obj *)clazz;
	}

	EloxKlassHandle *handle = ALLOCATE(runCtx, EloxKlassHandle, 1);
	ELOX_CHECK_RAISE_GOTO(handle, error, OOM(runCtx), cleanup);

	handle->base.runCtx = runCtx;
	handle->base.type = KLASS_HANDLE;
	handle->klass = (ObjKlass *)clazz;

	handleSetAdd(&vm->handles, (EloxHandle *)handle);

	ret = handle;

cleanup:
	releaseTemps(&temps);

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
	if (ELOX_UNLIKELY((err)->raised)) \
		return false; \
}

bool registerBuiltins(RunCtx *runCtx, EloxMsgError *errorMsg) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	EloxError *error = (EloxError *)errorMsg;

	struct Builtins *bi = &vm->builtins;

	bi->anonInitString = copyString(runCtx, ELOX_USTR_AND_LEN("$init"));
	RET_IF_OOM(bi->anonInitString);

	bi->scriptString = copyString(runCtx, ELOX_USTR_AND_LEN("<script>"));
	RET_IF_OOM(bi->scriptString);

	ObjClass *objectClass;
	bi->biObject._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Object"), error);
	EloxKlassHandle *openObject = OPEN_CLASS(runCtx, false, bi->biObject._nameStr, error, NULL);
	bi->biObject.toStringStr = internString(runCtx, ELOX_USTR_AND_LEN("toString"), error);
	bi->biObject.hashCodeStr = internString(runCtx, ELOX_USTR_AND_LEN("hashCode"), error);
	klassAddNativeMethod(openObject, bi->biObject.toStringStr, objectToString, 0, false);
	klassAddNativeMethod(openObject, bi->biObject.hashCodeStr, objectHashCode, 0, false);
	bi->biObject._class = objectClass =
		(ObjClass *)klassCloseAndRegister(openObject, bi->biObject._nameStr, &eloxBuiltinModule);

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

	static String fn = ELOX_STRING("builtins.c");

	ObjInterface *iterableIntf;
	bi->biIterable._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Iterable"), error);
	EloxKlassHandle *openIterable = openInterface(runCtx, bi->biIterable._nameStr, error);
	bi->biIterable.iteratorStr = internString(runCtx, ELOX_USTR_AND_LEN("iterator"), error),
	klassAddAbstractMethod(openIterable, bi->biIterable.iteratorStr, 0, false);
	klassAddCompiledMethod(openIterable, (uint8_t *)
		"count() {"
			"local cnt = 0;"
			"local i = this:iterator();"
			"while (i:hasNext()) {"
				"cnt += 1;"
				"i:next();"
			"}"
			"return cnt;"
		"}",
		&fn, &eloxBuiltinModule);
	klassAddCompiledMethod(openIterable, (uint8_t *)
		"isEmpty() {"
			"local i = this:iterator();"
			"return i:hasNext() == false;"
		"}",
		&fn, &eloxBuiltinModule);
	bi->biIterable._intf = iterableIntf =
		(ObjInterface *)klassCloseAndRegister(openIterable, bi->biIterable._nameStr, &eloxBuiltinModule);

	ObjClass *iteratorClass;
	bi->biIterator._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Iterator"), error);
	EloxKlassHandle *openIterator = OPEN_CLASS(runCtx, true, bi->biIterator._nameStr, error, objectClass);
	bi->biIterator.hasNextStr = internString(runCtx, ELOX_USTR_AND_LEN("hasNext"), error);
	bi->biIterator.nextStr = internString(runCtx, ELOX_USTR_AND_LEN("next"), error);
	bi->biIterator.removeStr = internString(runCtx, ELOX_USTR_AND_LEN("remove"), error);
	klassAddAbstractMethod(openIterator, bi->biIterator.hasNextStr, 0, false);
	klassAddAbstractMethod(openIterator, bi->biIterator.nextStr, 0, false);
	klassAddAbstractMethod(openIterator, bi->biIterator.removeStr, 0, false);
	bi->biIterator._class = iteratorClass =
		(ObjClass *)klassCloseAndRegister(openIterator, bi->biIterator._nameStr, &eloxBuiltinModule);

	ObjClass *gmatchIteratorClass;
	bi->biGmatchIterator._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("$GmatchIterator"), error);
	EloxKlassHandle *openGMatchIterator = OPEN_CLASS(runCtx, false, bi->biGmatchIterator._nameStr, error,
													 iteratorClass);
	bi->biGmatchIterator.stringStr = internString(runCtx, ELOX_USTR_AND_LEN("string"), error);
	bi->biGmatchIterator.patternStr = internString(runCtx, ELOX_USTR_AND_LEN("pattern"), error);
	bi->biGmatchIterator.offsetStr = internString(runCtx, ELOX_USTR_AND_LEN("offset"), error);
	bi->biGmatchIterator.cachedNextStr = internString(runCtx, ELOX_USTR_AND_LEN("cachedNext"), error);
	bi->biGmatchIterator._string = klassAddField(openGMatchIterator, bi->biGmatchIterator.stringStr);
	bi->biGmatchIterator._pattern = klassAddField(openGMatchIterator, bi->biGmatchIterator.patternStr);
	bi->biGmatchIterator._offset = klassAddField(openGMatchIterator, bi->biGmatchIterator.offsetStr);
	bi->biGmatchIterator._cachedNext = klassAddField(openGMatchIterator, bi->biGmatchIterator.cachedNextStr),
	klassAddNativeMethod(openGMatchIterator, bi->biIterator.hasNextStr, gmatchIteratorHasNext, 0, false);
	klassAddNativeMethod(openGMatchIterator, bi->biIterator.nextStr, gmatchIteratorNext, 0, false);
	bi->biGmatchIterator._class = gmatchIteratorClass =
		(ObjClass *)klassCloseAndRegister(openGMatchIterator, bi->biGmatchIterator._nameStr, &eloxBuiltinModule);

	ObjClass *stringClass;
	bi->biString._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("String"), error);
	EloxKlassHandle *openString = OPEN_CLASS(runCtx, false, bi->biString._nameStr, error,
											 objectClass);
	bi->biString.lengthStr = internString(runCtx, ELOX_USTR_AND_LEN("length"), error);
	bi->biString.fmtStr = internString(runCtx, ELOX_USTR_AND_LEN("fmt"), error);
	bi->biString.findStr = internString(runCtx, ELOX_USTR_AND_LEN("find"), error);
	bi->biString.findMatchStr = internString(runCtx, ELOX_USTR_AND_LEN("findMatch"), error);
	bi->biString.matchStr = internString(runCtx, ELOX_USTR_AND_LEN("match"), error);
	bi->biString.gmatchStr = internString(runCtx, ELOX_USTR_AND_LEN("gmatch"), error);
	bi->biString.gsubStr = internString(runCtx, ELOX_USTR_AND_LEN("gsub"), error);
	bi->biString.startsWithStr = internString(runCtx, ELOX_USTR_AND_LEN("startsWith"), error);
	bi->biString.endsWithStr = internString(runCtx, ELOX_USTR_AND_LEN("endsWith"), error);
	bi->biString.upperStr = internString(runCtx, ELOX_USTR_AND_LEN("upper"), error);
	bi->biString.lowerStr = internString(runCtx, ELOX_USTR_AND_LEN("lower"), error);
	bi->biString.trimStr = internString(runCtx, ELOX_USTR_AND_LEN("trim"), error);
	klassAddNativeMethod(openString, bi->biObject.toStringStr, stringToString, 0, false);
	klassAddNativeMethod(openString, bi->biObject.hashCodeStr, stringHashCode, 0, false);
	klassAddNativeMethod(openString, bi->biString.lengthStr, stringLength, 0, false);
	klassAddNativeMethod(openString, bi->biString.fmtStr, stringFmt, 0, true);
	klassAddNativeMethod(openString, bi->biString.findStr, stringFind, 2, false);
	klassAddNativeMethod(openString, bi->biString.findMatchStr, stringFindMatch, 2, false);
	klassAddNativeMethod(openString, bi->biString.matchStr, stringMatch, 2, false);
	klassAddNativeMethod(openString, bi->biString.gmatchStr, stringGmatch, 1, false);
	bi->biString._gsub = klassAddNativeMethod(openString, bi->biString.gsubStr, stringGsub, 3, false),
	klassAddNativeMethod(openString, bi->biString.startsWithStr, stringStartsWith, 1, false);
	klassAddNativeMethod(openString, bi->biString.endsWithStr, stringEndsWith, 1, false);
	klassAddNativeMethod(openString, bi->biString.upperStr, stringUpper, 0, false);
	klassAddNativeMethod(openString, bi->biString.lowerStr, stringLower, 0, false);
	klassAddNativeMethod(openString, bi->biString.trimStr, stringTrim, 0, false);
	bi->biString._class = stringClass =
		(ObjClass *)klassCloseAndRegister(openString, bi->biString._nameStr, &eloxBuiltinModule);


	ObjClass *numberClass;
	bi->biNumber._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Number"), error);
	EloxKlassHandle *openNumber = OPEN_CLASS(runCtx, false, bi->biNumber._nameStr, error,
											 objectClass);
	klassAddNativeMethod(openNumber, bi->biObject.toStringStr, numberToString, 0, false);
	bi->biNumber._class = numberClass =
		(ObjClass *)klassCloseAndRegister(openNumber, bi->biNumber._nameStr, &eloxBuiltinModule);

	vm->builtins.trueStr = internString(runCtx, ELOX_USTR_AND_LEN("true"), error);
	vm->builtins.falseStr = internString(runCtx, ELOX_USTR_AND_LEN("false"), error);

	ObjClass *boolClass;
	bi->biBool._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Bool"), error);
	EloxKlassHandle *openBool = OPEN_CLASS(runCtx, false, bi->biBool._nameStr, error,
										   objectClass);
	klassAddNativeMethod(openBool, bi->biObject.toStringStr, boolToString, 0, false);
	bi->biBool._class = boolClass =
		(ObjClass *)klassCloseAndRegister(openBool, bi->biBool._nameStr, &eloxBuiltinModule);

	ObjClass *instanceClass;
	bi->biInstance._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("$Instance"), error);
	EloxKlassHandle *openInstance = OPEN_CLASS(runCtx, false, bi->biInstance._nameStr, error,
											   objectClass);
	bi->biInstance._class = instanceClass =
		(ObjClass *)klassCloseAndRegister(openInstance, bi->biInstance._nameStr, &eloxBuiltinModule);

	ObjClass *classClass;
	bi->biClass._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Class"), error);
	EloxKlassHandle *_openClass = OPEN_CLASS(runCtx, false, bi->biClass._nameStr, error,
											 objectClass);
	bi->biClass._class = classClass =
		(ObjClass *)klassCloseAndRegister(_openClass, bi->biClass._nameStr, &eloxBuiltinModule);

	ObjClass *stackTraceElementClass;
	bi->biStackTraceElement._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("StackTraceElement"), error);
	EloxKlassHandle *openStackTraceElement = OPEN_CLASS(runCtx, false, bi->biStackTraceElement._nameStr, error,
														objectClass);
	bi->biStackTraceElement.fileNameStr = internString(runCtx, ELOX_USTR_AND_LEN("fileName"), error);
	bi->biStackTraceElement.lineNumberStr = internString(runCtx, ELOX_USTR_AND_LEN("lineNumber"), error);
	bi->biStackTraceElement.functionNameStr = internString(runCtx, ELOX_USTR_AND_LEN("functionName"), error);
	bi->biStackTraceElement._fileName = klassAddField(openStackTraceElement, bi->biStackTraceElement.fileNameStr);
	bi->biStackTraceElement._lineNumber = klassAddField(openStackTraceElement, bi->biStackTraceElement.lineNumberStr);
	bi->biStackTraceElement._functionName = klassAddField(openStackTraceElement, bi->biStackTraceElement.functionNameStr);
	klassAddNativeMethod(openStackTraceElement, bi->biStackTraceElement._nameStr, stackTraceElementInit,
						 3, false);
	bi->biStackTraceElement._class = stackTraceElementClass =
		(ObjClass *)klassCloseAndRegister(openStackTraceElement,
										  bi->biStackTraceElement._nameStr, &eloxBuiltinModule);

	ObjClass *throwableClass;
	bi->biThrowable._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Throwable"), error);
	EloxKlassHandle *openThrowable = OPEN_CLASS(runCtx, false, bi->biThrowable._nameStr, error,
												objectClass);
	bi->biThrowable.messageStr = internString(runCtx, ELOX_USTR_AND_LEN("message"), error);
	klassAddField(openThrowable, bi->biThrowable.messageStr);
	klassAddNativeMethod(openThrowable, bi->biThrowable._nameStr, throwableInit, 1, false);
	klassAddNativeMethod(openThrowable, bi->biObject.toStringStr, throwableToString, 0, false);
	bi->biThrowable._class = throwableClass =
		(ObjClass *)klassCloseAndRegister(openThrowable, bi->biThrowable._nameStr, &eloxBuiltinModule);

	ObjClass *exceptionClass;
	bi->biException._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Exception"), error);
	EloxKlassHandle *openException = OPEN_CLASS(runCtx, false, bi->biException._nameStr, error,
												throwableClass);
	bi->biException.stacktraceStr = internString(runCtx, ELOX_USTR_AND_LEN("stacktrace"), error);
	bi->biException.printStackTraceString = internString(runCtx, ELOX_USTR_AND_LEN("printStackTrace"), error),
	klassAddField(openException, bi->biException.stacktraceStr);
	klassAddNativeMethod(openException, bi->biException._nameStr, exceptionInit, 1, false);
	bi->biException._printStackTrace =
		klassAddNativeMethod(openException, bi->biException.printStackTraceString, exceptionPrintStackTrace, 0, false);
	bi->biException._class = exceptionClass =
		(ObjClass *)klassCloseAndRegister(openException, bi->biException._nameStr, &eloxBuiltinModule);

	ObjClass *runtimeExceptionClass;
	bi->biRuntimeException._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("RuntimeException"), error);
	EloxKlassHandle *openRuntimeException = OPEN_CLASS(runCtx, false, bi->biRuntimeException._nameStr, error,
													   exceptionClass);
	bi->biRuntimeException._class = runtimeExceptionClass =
		(ObjClass *)klassCloseAndRegister(openRuntimeException,
										  bi->biRuntimeException._nameStr, &eloxBuiltinModule);

	ObjClass *errorClass;
	bi->biError._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Error"), error);
	EloxKlassHandle *openError = OPEN_CLASS(runCtx, false, bi->biError._nameStr, error,
											throwableClass);
	klassAddNativeMethod(openError, bi->biError._nameStr, errorInit, 1, false);
	bi->biError._class = errorClass =
		(ObjClass *)klassCloseAndRegister(openError, bi->biError._nameStr, &eloxBuiltinModule);

	RET_IF_RAISED(error);
	bi->oomErrorMsg = internString(runCtx, ELOX_USTR_AND_LEN("Out of memory"), error);
	RET_IF_RAISED(error);
	ObjInstance *oomErrorInst = newInstance(runCtx, errorClass);
	RET_IF_OOM(oomErrorInst);
	push(fiber, OBJ_VAL(oomErrorInst));
	push(fiber, OBJ_VAL(bi->oomErrorMsg));
	callMethod(runCtx, AS_OBJ(errorClass->initializer), 1, 0);
	pop(fiber);
	vm->builtins.oomError = oomErrorInst;

	ObjClass *arrayIteratorClass;
	bi->biArrayIterator._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("$ArrayIterator"), error);
	EloxKlassHandle *openArrayIterator = OPEN_CLASS(runCtx, false, bi->biArrayIterator._nameStr, error,
													iteratorClass);
	bi->biArrayIterator.arrayStr = internString(runCtx, ELOX_USTR_AND_LEN("array"), error);
	bi->biArrayIterator.cursorStr = internString(runCtx, ELOX_USTR_AND_LEN("cursor"), error);
	bi->biArrayIterator.lastRetStr = internString(runCtx, ELOX_USTR_AND_LEN("lastRet"), error);
	bi->biArrayIterator.modCountStr = internString(runCtx, ELOX_USTR_AND_LEN("modCount"), error);
	bi->biArrayIterator._array = klassAddField(openArrayIterator, bi->biArrayIterator.arrayStr);
	bi->biArrayIterator._cursor = klassAddField(openArrayIterator, bi->biArrayIterator.cursorStr);
	bi->biArrayIterator._lastRet = klassAddField(openArrayIterator, bi->biArrayIterator.lastRetStr);
	bi->biArrayIterator._modCount = klassAddField(openArrayIterator, bi->biArrayIterator.modCountStr);
	klassAddNativeMethod(openArrayIterator, bi->biIterator.hasNextStr, arrayIteratorHasNext, 0, false);
	klassAddNativeMethod(openArrayIterator, bi->biIterator.nextStr, arrayIteratorNext, 0, false);
	klassAddNativeMethod(openArrayIterator, bi->biIterator.removeStr, arrayIteratorRemove, 0, false);
	bi->biArrayIterator._class = arrayIteratorClass =
		(ObjClass *)klassCloseAndRegister(openArrayIterator, bi->biArrayIterator._nameStr, &eloxBuiltinModule);

	ObjClass *arrayClass;
	bi->biArray._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Array"), error);
	EloxKlassHandle *openArray = OPEN_CLASS(runCtx, false, bi->biArray._nameStr, error,
											objectClass);
	bi->biArray.lengthStr = internString(runCtx, ELOX_USTR_AND_LEN("length"), error);
	bi->biArray.addStr = internString(runCtx, ELOX_USTR_AND_LEN("add"), error);
	bi->biArray.removeAtStr = internString(runCtx, ELOX_USTR_AND_LEN("removeAt"), error);
	klassAddNativeMethod(openArray, bi->biArray.lengthStr, arrayLength, 0, false);
	klassAddNativeMethod(openArray, bi->biArray.addStr, arrayAdd, 1, false);
	klassAddNativeMethod(openArray, bi->biArray.removeAtStr, arrayRemoveAt, 1, false);
	klassAddNativeMethod(openArray, bi->biIterable.iteratorStr, arrayIterator, 0, false);
	bi->biArray._class = arrayClass =
		(ObjClass *)klassCloseAndRegister(openArray, bi->biArray._nameStr, &eloxBuiltinModule);

	ObjClass *tupleClass;
	bi->biTuple._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Tuple"), error);
	EloxKlassHandle *openTuple = OPEN_CLASS(runCtx, false, bi->biTuple._nameStr, error,
											objectClass);
	bi->biTuple.lengthStr = internString(runCtx, ELOX_USTR_AND_LEN("length"), error);
	klassAddNativeMethod(openTuple, bi->biTuple.lengthStr, arrayLength, 0, false);
	klassAddNativeMethod(openTuple, bi->biIterable.iteratorStr, arrayIterator, 0, false);
	bi->biTuple._class = tupleClass =
		(ObjClass *)klassCloseAndRegister(openTuple, bi->biTuple._nameStr, &eloxBuiltinModule);

	ObjClass *hashMapIteratorClass;
	bi->biHashMapIterator._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("$HashMapIterator"), error);
	EloxKlassHandle *openHashMapIterator = OPEN_CLASS(runCtx, false, bi->biHashMapIterator._nameStr, error,
													  iteratorClass);
	bi->biHashMapIterator.mapStr = internString(runCtx, ELOX_USTR_AND_LEN("map"), error);
	bi->biHashMapIterator.currentStr = internString(runCtx, ELOX_USTR_AND_LEN("current"), error);
	bi->biHashMapIterator.modCountStr = internString(runCtx, ELOX_USTR_AND_LEN("modCount"), error);
	bi->biHashMapIterator._map = klassAddField(openHashMapIterator, bi->biHashMapIterator.mapStr);
	bi->biHashMapIterator._current = klassAddField(openHashMapIterator, bi->biHashMapIterator.currentStr);
	bi->biHashMapIterator._modCount = klassAddField(openHashMapIterator, bi->biHashMapIterator.modCountStr);
	klassAddNativeMethod(openHashMapIterator, bi->biIterator.hasNextStr, hashMapIteratorHasNext, 0, false);
	klassAddNativeMethod(openHashMapIterator, bi->biIterator.nextStr, hashMapIteratorNext, 0, false);
	bi->biHashMapIterator._class = hashMapIteratorClass =
		(ObjClass *)klassCloseAndRegister(openHashMapIterator, bi->biHashMapIterator._nameStr, &eloxBuiltinModule);

	ObjInterface *mapIntf;
	bi->biMap._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("Map"), error);
	EloxKlassHandle *openMap = openInterface(runCtx, bi->biMap._nameStr, error);
	bi->biMap.sizeStr = internString(runCtx, ELOX_USTR_AND_LEN("size"), error);
	bi->biMap.putStr = internString(runCtx, ELOX_USTR_AND_LEN("put"), error);
	bi->biMap.removeStr = internString(runCtx, ELOX_USTR_AND_LEN("remove"), error);
	klassAddAbstractMethod(openMap, bi->biMap.sizeStr, 0, false);
	klassAddAbstractMethod(openMap, bi->biMap.putStr, 2, false);
	klassAddAbstractMethod(openMap, bi->biMap.removeStr, 1, false);
	bi->biMap._intf = mapIntf =
		(ObjInterface *)klassCloseAndRegister(openMap, bi->biMap._nameStr, &eloxBuiltinModule);

	ObjClass *hashMapClass;
	bi->biHashMap._nameStr = internString(runCtx, ELOX_USTR_AND_LEN("HashMap"), error);
	EloxKlassHandle *openHashMap = OPEN_CLASS(runCtx, false, bi->biHashMap._nameStr, error,
											  objectClass, mapIntf, iterableIntf);
	klassAddNativeMethod(openHashMap, bi->biMap.sizeStr, hashMapSize, 0, false);
	klassAddNativeMethod(openHashMap, bi->biMap.putStr, hashMapPut, 2, false);
	klassAddNativeMethod(openHashMap, bi->biMap.removeStr, hashMapRemove, 1, false);
	klassAddNativeMethod(openHashMap, bi->biIterable.iteratorStr, hashMapIterator, 0, false);
	bi->biHashMap._class = hashMapClass =
		(ObjClass *)klassCloseAndRegister(openHashMap, bi->biHashMap._nameStr, &eloxBuiltinModule);

	RET_IF_RAISED(error);

	return true;
}

void clearBuiltins(VM *vm) {
	memset(&vm->builtins, 0, sizeof(vm->builtins));
}
