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
	ObjFiber *fiber = runCtx->activeFiber;

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
	ObjInstance *inst = (ObjInstance *)AS_OBJ(getValueArg(args, 0));
	heapStringAddFmt(runCtx, &ret, "%s@%u", inst->class_->name->string.chars, inst->identityHash);
	ObjString *str = takeString(runCtx, ret.chars, ret.length, ret.capacity);
	if (ELOX_UNLIKELY(str == NULL))
		return oomError(runCtx, NULL);
	return OBJ_VAL(str);
}

static Value objectHashCode(Args *args) {
	ObjInstance *inst = (ObjInstance *)AS_OBJ(getValueArg(args, 0));
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
	return b ? OBJ_VAL(vm->builtins.strings.true_) : OBJ_VAL(vm->builtins.strings.false_);
}

//--- StackTraceElement ---------

static Value stackTraceElementInit(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;

	ObjInstance *inst = (ObjInstance *)AS_OBJ(getValueArg(args, 0));
	ObjString *fileName = AS_STRING(getValueArg(args, 1));
	double lineNumber = AS_NUMBER(getValueArg(args, 2));
	ObjString *functionName = AS_STRING(getValueArg(args, 3));

	inst->fields[vm->builtins.biStackTraceElement.fields.fileName] = OBJ_VAL(fileName);
	inst->fields[vm->builtins.biStackTraceElement.fields.lineNumber] = NUMBER_VAL(lineNumber);
	inst->fields[vm->builtins.biStackTraceElement.fields.functionName] = OBJ_VAL(functionName);

	return OBJ_VAL(inst);
}

//--- Throwable -----------------

static Value throwableInit(Args *args) {
	RunCtx *runCtx = args->runCtx;
	ObjFiber *fiber = runCtx->activeFiber;

	ObjInstance *inst = (ObjInstance *)AS_OBJ(getValueArg(args, 0));
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
	ObjFiber *fiber = runCtx->activeFiber;

	ObjInstance *inst = (ObjInstance *)AS_OBJ(getValueArg(args, 0));

	HeapCString ret;
	if (ELOX_UNLIKELY(!initHeapString(runCtx, &ret)))
		return oomError(runCtx, NULL);
	ObjClass *class_ = inst->class_;
	heapStringAddFmt(runCtx, &ret, "%.*s", class_->name->string.length, class_->name->string.chars);
	Value msg;
	if (getInstanceValue(inst, vm->builtins.biThrowable.strings.message, &msg)) {
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
	ObjFiber *fiber = runCtx->activeFiber;

	ObjInstance *inst = (ObjInstance *)AS_OBJ(getValueArg(args, 0));
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

	ObjInstance *inst = (ObjInstance *)AS_OBJ(instVal);
	Value stVal;
	if (getInstanceValue(inst, vm->builtins.biException.strings.stacktrace, &stVal)) {
		if (isObjType(stVal, OBJ_ARRAY)) {
			ObjArray *st = (ObjArray *)AS_OBJ(stVal);
			for (int32_t i = 0; i < st->size; i++) {
				ObjInstance *elem = (ObjInstance *)AS_OBJ(st->items[i]);
				eloxPrintf(runCtx, ELOX_IO_ERR, "\tat ");
				ObjString *functionName = AS_STRING(elem->fields[vm->builtins.biStackTraceElement.fields.functionName]);
				eloxPrintf(runCtx, ELOX_IO_ERR, "%.*s", functionName->string.length, functionName->string.chars);
				ObjString *fileName = AS_STRING(elem->fields[vm->builtins.biStackTraceElement.fields.fileName]);
				int lineNumber = (int)AS_NUMBER(elem->fields[vm->builtins.biStackTraceElement.fields.lineNumber]);
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
	ObjFiber *fiber = runCtx->activeFiber;

	ObjInstance *inst = (ObjInstance *)AS_OBJ(getValueArg(args, 0));
	ObjString *msg = AS_STRING(getValueArg(args, 1));

	Value superInit = inst->class_->super->initializer;
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

	struct BIHashMapIterator *biHMI = &vm->builtins.biHashMapIterator;

	ObjInstance *inst = (ObjInstance *)AS_OBJ(getValueArg(args, 0));
	ObjHashMap *map = (ObjHashMap *)AS_OBJ(inst->fields[biHMI->fields.map]);
	int current = AS_NUMBER(inst->fields[biHMI->fields.current]);

	TableEntry *entry;
	int32_t nextIndex = valueTableGetNext(&map->items, current, &entry);

	return BOOL_VAL(nextIndex >= 0);
}

static Value hashMapIteratorNext(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	struct BIHashMapIterator *biHMI = &vm->builtins.biHashMapIterator;

	ObjInstance *inst = (ObjInstance *)AS_OBJ(getValueArg(args, 0));
	ObjHashMap *map = (ObjHashMap *)AS_OBJ(inst->fields[biHMI->fields.map]);
	int current = AS_NUMBER(inst->fields[biHMI->fields.current]);
	uint32_t modCount = AS_NUMBER(inst->fields[biHMI->fields.modCount]);

	if (ELOX_UNLIKELY(modCount != map->items.modCount))
		return runtimeError(runCtx, NULL, "HashMap modified during iteration");

	TableEntry *entry;
	int nextIndex = valueTableGetNext(&map->items, current, &entry);

	inst->fields[biHMI->fields.current] = NUMBER_VAL(nextIndex);

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
	ObjHashMap *inst = (ObjHashMap *)AS_OBJ(getValueArg(args, 0));
	return NUMBER_VAL(inst->items.liveCount);
}

static Value hashMapPut(Args *args) {
	RunCtx *runCtx = args->runCtx;

	ObjHashMap *inst = (ObjHashMap *)AS_OBJ(getValueArg(args, 0));
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

	ObjHashMap *inst = (ObjHashMap *)AS_OBJ(getValueArg(args, 0));
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
	struct BIHashMapIterator *biHMI = &vm->builtins.biHashMapIterator;

	ObjHashMap *inst = (ObjHashMap *)AS_OBJ(getValueArg(args, 0));

	ObjInstance *iter = (ObjInstance *)newInstance(runCtx, biHMI->class_);
	if (ELOX_UNLIKELY(iter == NULL))
		return oomError(runCtx, NULL);
	iter->fields[biHMI->fields.map] = OBJ_VAL(inst);
	iter->fields[biHMI->fields.current] = NUMBER_VAL(0);
	iter->fields[biHMI->fields.modCount] = NUMBER_VAL(inst->items.modCount);
	return OBJ_VAL(iter);
}

//--- $VarargsIterator --------------

Value varargsIteratorHasNext(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;

	struct BIVarargsIterator *biVI = &vm->builtins.biVarargsIterator;

	ObjInstance *inst = (ObjInstance *)AS_OBJ(getValueArg(args, 0));
	ObjCallFrame *frame = (ObjCallFrame *)AS_OBJ(inst->fields[biVI->fields.frame]);
	int32_t cursor = AS_NUMBER(inst->fields[biVI->fields.cursor]);

	return BOOL_VAL(cursor < frame->varArgs);
}

Value varargsIteratorNext(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;

	struct BIVarargsIterator *biVI = &vm->builtins.biVarargsIterator;

	ObjInstance *inst = (ObjInstance *)AS_OBJ(getValueArg(args, 0));
	ObjCallFrame *frame = (ObjCallFrame *)AS_OBJ(inst->fields[biVI->fields.frame]);
	int32_t i = AS_NUMBER(inst->fields[biVI->fields.cursor]);

	inst->fields[biVI->fields.cursor] = NUMBER_VAL(i + 1);
	return frame->slots[frame->fixedArgs + i + 1];
}

//--- Varargs -----------------------

static Value varargsLength(Args *args) {
	ObjCallFrame *inst = (ObjCallFrame *)AS_OBJ(getValueArg(args, 0));
	return NUMBER_VAL(inst->varArgs);
}

static Value varargsIterator(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;
	struct BIVarargsIterator *biVI = &vm->builtins.biVarargsIterator;

	ObjCallFrame *inst = (ObjCallFrame *)AS_OBJ(getValueArg(args, 0));

	ObjInstance *iter = (ObjInstance *)newInstance(runCtx, biVI->class_);
	if (ELOX_UNLIKELY(iter == NULL))
		return oomError(runCtx, NULL);
	iter->fields[biVI->fields.frame] = OBJ_VAL(inst);
	iter->fields[biVI->fields.cursor] = NUMBER_VAL(0);
	return OBJ_VAL(iter);
}

//--- Fiber ---------------------

static Value fiberInit(Args *args) {
	RunCtx *runCtx = args->runCtx;

	Value callable = getValueArg(args, 1);

	EloxError error = ELOX_ERROR_INITIALIZER;
	ObjFiber *fiber = newFiber(runCtx, callable, &error);
	if (ELOX_UNLIKELY(error.raised))
		return EXCEPTION_VAL;

	return OBJ_VAL(fiber);
}

static Value fiberResume(Args *args) {
	RunCtx *runCtx = args->runCtx;

	ObjFiber *inst = (ObjFiber *)AS_OBJ(getValueArg(args, 0));

	EloxError error = ELOX_ERROR_INITIALIZER;
	resumeFiber(runCtx, inst, getArgsFrom(args, 1), &error);
	if (ELOX_UNLIKELY(error.raised))
		return EXCEPTION_VAL;

	return NIL_VAL;
}

static Value fiberResumeThrow(Args *args) {
	RunCtx *runCtx = args->runCtx;

	ObjFiber *inst = (ObjFiber *)AS_OBJ(getValueArg(args, 0));
	Value throwable = getValueArg(args, 1);

	EloxError error = ELOX_ERROR_INITIALIZER;
	ELOX_CHECK_RAISE_RET_VAL(IS_INSTANCE(throwable),
							 &error, RTERR(runCtx, "Can only throw Throwable instances"),
							 EXCEPTION_VAL);

	resumeThrow(runCtx, inst, (ObjInstance *)AS_OBJ(throwable), &error);

	// one way or another it's still an eception
	return EXCEPTION_VAL;
}

static Value fiberYield(Args *args) {
	RunCtx *runCtx = args->runCtx;

	EloxError error = ELOX_ERROR_INITIALIZER;
	yieldFiber(runCtx, getArgsFrom(args, 0), &error);
	if (ELOX_UNLIKELY(error.raised))
		return EXCEPTION_VAL;

	return NIL_VAL;
}

static Value fiberGetCurrent(Args *args) {
	RunCtx *runCtx = args->runCtx;

	return OBJ_VAL(runCtx->activeFiber);
}

suint16_t builtinConstant(RunCtx *runCtx, const String *name) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

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
	ObjFiber *fiber = runCtx->activeFiber;

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

static EloxKlassHandle *openClass(RunCtx *runCtx, uint8_t flags,
								  ObjString *className, EloxError *error, ObjClass *super, ...) {
	VM *vm = runCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	if (ELOX_UNLIKELY(error->raised))
		return NULL;

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);

	ObjClass *clazz = newClass(runCtx, className, flags);
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
	ObjFiber *fiber = runCtx->activeFiber;

	EloxError *error = (EloxError *)errorMsg;

	struct Builtins *bi = &vm->builtins;

	bi->anonInitString = copyString(runCtx, ELOX_USTR_AND_LEN("$init"));
	RET_IF_OOM(bi->anonInitString);

	bi->scriptString = copyString(runCtx, ELOX_USTR_AND_LEN("<script>"));
	RET_IF_OOM(bi->scriptString);

	ObjClass *objectClass;
	bi->biObject.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("Object"), error);
	EloxKlassHandle *openObject = OPEN_CLASS(runCtx, 0, bi->biObject.strings.name, error, NULL);
	bi->biObject.strings.toString = internString(runCtx, ELOX_USTR_AND_LEN("toString"), error);
	bi->biObject.strings.hashCode = internString(runCtx, ELOX_USTR_AND_LEN("hashCode"), error);
	klassAddNativeMethod(openObject, bi->biObject.strings.toString, objectToString, 0, false);
	klassAddNativeMethod(openObject, bi->biObject.strings.hashCode, objectHashCode, 0, false);
	bi->biObject.class_ = objectClass =
		(ObjClass *)klassCloseAndRegister(openObject, bi->biObject.strings.name, &eloxBuiltinModule);

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
	bi->biIterable.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("Iterable"), error);
	EloxKlassHandle *openIterable = openInterface(runCtx, bi->biIterable.strings.name, error);
	bi->biIterable.strings.iterator = internString(runCtx, ELOX_USTR_AND_LEN("iterator"), error),
	klassAddAbstractMethod(openIterable, bi->biIterable.strings.iterator, 0, false);
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
	bi->biIterable.intf = iterableIntf =
		(ObjInterface *)klassCloseAndRegister(openIterable, bi->biIterable.strings.name, &eloxBuiltinModule);

	ObjClass *iteratorClass;
	bi->biIterator.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("Iterator"), error);
	EloxKlassHandle *openIterator = OPEN_CLASS(runCtx, CLASS_ABSTRACT, bi->biIterator.strings.name, error,
											   objectClass);
	bi->biIterator.strings.hasNext = internString(runCtx, ELOX_USTR_AND_LEN("hasNext"), error);
	bi->biIterator.strings.next = internString(runCtx, ELOX_USTR_AND_LEN("next"), error);
	bi->biIterator.strings.remove = internString(runCtx, ELOX_USTR_AND_LEN("remove"), error);
	klassAddAbstractMethod(openIterator, bi->biIterator.strings.hasNext, 0, false);
	klassAddAbstractMethod(openIterator, bi->biIterator.strings.next, 0, false);
	klassAddCompiledMethod(openIterator, (uint8_t *)
		"remove() {"
			"throw Exception('Iterator does not support removal');"
		"}",
		&fn, &eloxBuiltinModule);
	bi->biIterator.class_ = iteratorClass =
		(ObjClass *)klassCloseAndRegister(openIterator, bi->biIterator.strings.name, &eloxBuiltinModule);

	ObjClass *gmatchIteratorClass;
	bi->biGmatchIterator.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("$GmatchIterator"), error);
	EloxKlassHandle *openGMatchIterator = OPEN_CLASS(runCtx, 0, bi->biGmatchIterator.strings.name, error,
													 iteratorClass);
	bi->biGmatchIterator.strings.string = internString(runCtx, ELOX_USTR_AND_LEN("string"), error);
	bi->biGmatchIterator.strings.pattern = internString(runCtx, ELOX_USTR_AND_LEN("pattern"), error);
	bi->biGmatchIterator.strings.offset = internString(runCtx, ELOX_USTR_AND_LEN("offset"), error);
	bi->biGmatchIterator.strings.cachedNext = internString(runCtx, ELOX_USTR_AND_LEN("cachedNext"), error);
	bi->biGmatchIterator.fields.string = klassAddField(openGMatchIterator, bi->biGmatchIterator.strings.string);
	bi->biGmatchIterator.fields.pattern = klassAddField(openGMatchIterator, bi->biGmatchIterator.strings.pattern);
	bi->biGmatchIterator.fields.offset = klassAddField(openGMatchIterator, bi->biGmatchIterator.strings.offset);
	bi->biGmatchIterator.fields.cachedNext = klassAddField(openGMatchIterator, bi->biGmatchIterator.strings.cachedNext),
	klassAddNativeMethod(openGMatchIterator, bi->biIterator.strings.hasNext, gmatchIteratorHasNext, 0, false);
	klassAddNativeMethod(openGMatchIterator, bi->biIterator.strings.next, gmatchIteratorNext, 0, false);
	bi->biGmatchIterator.class_ = gmatchIteratorClass =
		(ObjClass *)klassCloseAndRegister(openGMatchIterator, bi->biGmatchIterator.strings.name, &eloxBuiltinModule);

	ObjClass *stringClass;
	bi->biString.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("String"), error);
	EloxKlassHandle *openString = OPEN_CLASS(runCtx, 0, bi->biString.strings.name, error,
											 objectClass);
	bi->biString.strings.length = internString(runCtx, ELOX_USTR_AND_LEN("length"), error);
	bi->biString.strings.fmt = internString(runCtx, ELOX_USTR_AND_LEN("fmt"), error);
	bi->biString.strings.find = internString(runCtx, ELOX_USTR_AND_LEN("find"), error);
	bi->biString.strings.findMatch = internString(runCtx, ELOX_USTR_AND_LEN("findMatch"), error);
	bi->biString.strings.match = internString(runCtx, ELOX_USTR_AND_LEN("match"), error);
	bi->biString.strings.gmatch = internString(runCtx, ELOX_USTR_AND_LEN("gmatch"), error);
	bi->biString.strings.gsub = internString(runCtx, ELOX_USTR_AND_LEN("gsub"), error);
	bi->biString.strings.startsWith = internString(runCtx, ELOX_USTR_AND_LEN("startsWith"), error);
	bi->biString.strings.endsWith = internString(runCtx, ELOX_USTR_AND_LEN("endsWith"), error);
	bi->biString.strings.upper = internString(runCtx, ELOX_USTR_AND_LEN("upper"), error);
	bi->biString.strings.lower = internString(runCtx, ELOX_USTR_AND_LEN("lower"), error);
	bi->biString.strings.trim = internString(runCtx, ELOX_USTR_AND_LEN("trim"), error);
	klassAddNativeMethod(openString, bi->biObject.strings.toString, stringToString, 0, false);
	klassAddNativeMethod(openString, bi->biObject.strings.hashCode, stringHashCode, 0, false);
	klassAddNativeMethod(openString, bi->biString.strings.length, stringLength, 0, false);
	klassAddNativeMethod(openString, bi->biString.strings.fmt, stringFmt, 0, true);
	klassAddNativeMethod(openString, bi->biString.strings.find, stringFind, 2, false);
	klassAddNativeMethod(openString, bi->biString.strings.findMatch, stringFindMatch, 2, false);
	klassAddNativeMethod(openString, bi->biString.strings.match, stringMatch, 2, false);
	klassAddNativeMethod(openString, bi->biString.strings.gmatch, stringGmatch, 1, false);
	bi->biString.methods.gsub = klassAddNativeMethod(openString, bi->biString.strings.gsub, stringGsub, 3, false),
	klassAddNativeMethod(openString, bi->biString.strings.startsWith, stringStartsWith, 1, false);
	klassAddNativeMethod(openString, bi->biString.strings.endsWith, stringEndsWith, 1, false);
	klassAddNativeMethod(openString, bi->biString.strings.upper, stringUpper, 0, false);
	klassAddNativeMethod(openString, bi->biString.strings.lower, stringLower, 0, false);
	klassAddNativeMethod(openString, bi->biString.strings.trim, stringTrim, 0, false);
	bi->biString.class_ = stringClass =
		(ObjClass *)klassCloseAndRegister(openString, bi->biString.strings.name, &eloxBuiltinModule);

	ObjClass *numberClass;
	bi->biNumber.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("Number"), error);
	EloxKlassHandle *openNumber = OPEN_CLASS(runCtx, 0, bi->biNumber.strings.name, error,
											 objectClass);
	klassAddNativeMethod(openNumber, bi->biObject.strings.toString, numberToString, 0, false);
	bi->biNumber.class_ = numberClass =
		(ObjClass *)klassCloseAndRegister(openNumber, bi->biNumber.strings.name, &eloxBuiltinModule);

	vm->builtins.strings.true_ = internString(runCtx, ELOX_USTR_AND_LEN("true"), error);
	vm->builtins.strings.false_ = internString(runCtx, ELOX_USTR_AND_LEN("false"), error);

	ObjClass *boolClass;
	bi->biBool.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("Bool"), error);
	EloxKlassHandle *openBool = OPEN_CLASS(runCtx, 0, bi->biBool.strings.name, error,
										   objectClass);
	klassAddNativeMethod(openBool, bi->biObject.strings.toString, boolToString, 0, false);
	bi->biBool.class_ = boolClass =
		(ObjClass *)klassCloseAndRegister(openBool, bi->biBool.strings.name, &eloxBuiltinModule);

	ObjClass *instanceClass;
	bi->biInstance.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("$Instance"), error);
	EloxKlassHandle *openInstance = OPEN_CLASS(runCtx, 0, bi->biInstance.strings.name, error,
											   objectClass);
	bi->biInstance.class_ = instanceClass =
		(ObjClass *)klassCloseAndRegister(openInstance, bi->biInstance.strings.name, &eloxBuiltinModule);

	ObjClass *classClass;
	bi->biClass.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("Class"), error);
	EloxKlassHandle *_openClass = OPEN_CLASS(runCtx, 0, bi->biClass.strings.name, error,
											 objectClass);
	bi->biClass.class_ = classClass =
		(ObjClass *)klassCloseAndRegister(_openClass, bi->biClass.strings.name, &eloxBuiltinModule);

	ObjClass *stackTraceElementClass;
	bi->biStackTraceElement.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("StackTraceElement"), error);
	EloxKlassHandle *openStackTraceElement = OPEN_CLASS(runCtx, 0, bi->biStackTraceElement.strings.name, error,
														objectClass);
	bi->biStackTraceElement.strings.fileName = internString(runCtx, ELOX_USTR_AND_LEN("fileName"), error);
	bi->biStackTraceElement.strings.lineNumber = internString(runCtx, ELOX_USTR_AND_LEN("lineNumber"), error);
	bi->biStackTraceElement.strings.functionName = internString(runCtx, ELOX_USTR_AND_LEN("functionName"), error);
	bi->biStackTraceElement.fields.fileName = klassAddField(openStackTraceElement, bi->biStackTraceElement.strings.fileName);
	bi->biStackTraceElement.fields.lineNumber = klassAddField(openStackTraceElement, bi->biStackTraceElement.strings.lineNumber);
	bi->biStackTraceElement.fields.functionName = klassAddField(openStackTraceElement, bi->biStackTraceElement.strings.functionName);
	klassAddNativeMethod(openStackTraceElement, bi->biStackTraceElement.strings.name, stackTraceElementInit,
						 3, false);
	bi->biStackTraceElement.class_ = stackTraceElementClass =
		(ObjClass *)klassCloseAndRegister(openStackTraceElement,
										  bi->biStackTraceElement.strings.name, &eloxBuiltinModule);

	ObjClass *throwableClass;
	bi->biThrowable.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("Throwable"), error);
	EloxKlassHandle *openThrowable = OPEN_CLASS(runCtx, 0, bi->biThrowable.strings.name, error,
												objectClass);
	bi->biThrowable.strings.message = internString(runCtx, ELOX_USTR_AND_LEN("message"), error);
	klassAddField(openThrowable, bi->biThrowable.strings.message);
	klassAddNativeMethod(openThrowable, bi->biThrowable.strings.name, throwableInit, 1, false);
	klassAddNativeMethod(openThrowable, bi->biObject.strings.toString, throwableToString, 0, false);
	bi->biThrowable.class_ = throwableClass =
		(ObjClass *)klassCloseAndRegister(openThrowable, bi->biThrowable.strings.name, &eloxBuiltinModule);

	ObjClass *exceptionClass;
	bi->biException.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("Exception"), error);
	EloxKlassHandle *openException = OPEN_CLASS(runCtx, 0, bi->biException.strings.name, error,
												throwableClass);
	bi->biException.strings.stacktrace = internString(runCtx, ELOX_USTR_AND_LEN("stacktrace"), error);
	bi->biException.strings.printStackTrace = internString(runCtx, ELOX_USTR_AND_LEN("printStackTrace"), error),
	klassAddField(openException, bi->biException.strings.stacktrace);
	klassAddNativeMethod(openException, bi->biException.strings.name, exceptionInit, 1, false);
	bi->biException.methods.printStackTrace =
		klassAddNativeMethod(openException, bi->biException.strings.printStackTrace, exceptionPrintStackTrace, 0, false);
	bi->biException.class_ = exceptionClass =
		(ObjClass *)klassCloseAndRegister(openException, bi->biException.strings.name,
										  &eloxBuiltinModule);

	ObjClass *runtimeExceptionClass;
	bi->biRuntimeException.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("RuntimeException"), error);
	EloxKlassHandle *openRuntimeException = OPEN_CLASS(runCtx, 0, bi->biRuntimeException.strings.name, error,
													   exceptionClass);
	bi->biRuntimeException.class_ = runtimeExceptionClass =
		(ObjClass *)klassCloseAndRegister(openRuntimeException,
										  bi->biRuntimeException.strings.name, &eloxBuiltinModule);

	ObjClass *errorClass;
	bi->biError.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("Error"), error);
	EloxKlassHandle *openError = OPEN_CLASS(runCtx, 0, bi->biError.strings.name, error,
											throwableClass);
	klassAddNativeMethod(openError, bi->biError.strings.name, errorInit, 1, false);
	bi->biError.class_ = errorClass =
		(ObjClass *)klassCloseAndRegister(openError, bi->biError.strings.name, &eloxBuiltinModule);

	ObjString *oomErrorMsg = internString(runCtx, ELOX_USTR_AND_LEN("Out of memory"), error);
	RET_IF_OOM(oomErrorMsg);
	ObjInstance *oomErrorInst = (ObjInstance *)newInstance(runCtx, errorClass);
	RET_IF_OOM(oomErrorInst);
	push(fiber, OBJ_VAL(oomErrorInst));
	push(fiber, OBJ_VAL(oomErrorMsg));
	callMethod(runCtx, AS_OBJ(errorClass->initializer), 1, 0);
	pop(fiber);
	vm->builtins.oomError = oomErrorInst;

	ObjString *terminateErrorMsg = internString(runCtx, ELOX_USTR_AND_LEN("Terminate"), error);
	RET_IF_OOM(terminateErrorMsg);
	ObjInstance *terminateErrorInst = (ObjInstance *)newInstance(runCtx, errorClass);
	RET_IF_OOM(terminateErrorInst);
	push(fiber, OBJ_VAL(terminateErrorInst));
	push(fiber, OBJ_VAL(terminateErrorMsg));
	callMethod(runCtx, AS_OBJ(errorClass->initializer), 1, 0);
	pop(fiber);
	vm->builtins.terminateError = terminateErrorInst;

	ObjClass *arrayIteratorClass;
	bi->biArrayIterator.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("$ArrayIterator"), error);
	EloxKlassHandle *openArrayIterator = OPEN_CLASS(runCtx, 0, bi->biArrayIterator.strings.name, error,
													iteratorClass);
	bi->biArrayIterator.strings.array = internString(runCtx, ELOX_USTR_AND_LEN("array"), error);
	bi->biArrayIterator.strings.cursor = internString(runCtx, ELOX_USTR_AND_LEN("cursor"), error);
	bi->biArrayIterator.strings.lastRet = internString(runCtx, ELOX_USTR_AND_LEN("lastRet"), error);
	bi->biArrayIterator.strings.modCount = internString(runCtx, ELOX_USTR_AND_LEN("modCount"), error);
	bi->biArrayIterator.fields.array = klassAddField(openArrayIterator, bi->biArrayIterator.strings.array);
	bi->biArrayIterator.fields.cursor = klassAddField(openArrayIterator, bi->biArrayIterator.strings.cursor);
	bi->biArrayIterator.fields.lastRet = klassAddField(openArrayIterator, bi->biArrayIterator.strings.lastRet);
	bi->biArrayIterator.fields.modCount = klassAddField(openArrayIterator, bi->biArrayIterator.strings.modCount);
	klassAddNativeMethod(openArrayIterator, bi->biIterator.strings.hasNext, arrayIteratorHasNext, 0, false);
	klassAddNativeMethod(openArrayIterator, bi->biIterator.strings.next, arrayIteratorNext, 0, false);
	klassAddNativeMethod(openArrayIterator, bi->biIterator.strings.remove, arrayIteratorRemove, 0, false);
	bi->biArrayIterator.class_ = arrayIteratorClass =
		(ObjClass *)klassCloseAndRegister(openArrayIterator, bi->biArrayIterator.strings.name,
										  &eloxBuiltinModule);

	ObjClass *arrayClass;
	bi->biArray.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("Array"), error);
	EloxKlassHandle *openArray = OPEN_CLASS(runCtx, 0, bi->biArray.strings.name, error,
											objectClass, iterableIntf);
	bi->biArray.strings.length = internString(runCtx, ELOX_USTR_AND_LEN("length"), error);
	bi->biArray.strings.add = internString(runCtx, ELOX_USTR_AND_LEN("add"), error);
	bi->biArray.strings.removeAt = internString(runCtx, ELOX_USTR_AND_LEN("removeAt"), error);
	klassAddNativeMethod(openArray, bi->biArray.strings.length, arrayLength, 0, false);
	klassAddNativeMethod(openArray, bi->biArray.strings.add, arrayAdd, 1, false);
	klassAddNativeMethod(openArray, bi->biArray.strings.removeAt, arrayRemoveAt, 1, false);
	klassAddNativeMethod(openArray, bi->biIterable.strings.iterator, arrayIterator, 0, false);
	bi->biArray.class_ = arrayClass =
		(ObjClass *)klassCloseAndRegister(openArray, bi->biArray.strings.name, &eloxBuiltinModule);

	ObjClass *tupleClass;
	bi->biTuple.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("Tuple"), error);
	EloxKlassHandle *openTuple = OPEN_CLASS(runCtx, 0, bi->biTuple.strings.name, error,
											objectClass);
	bi->biTuple.strings.length = internString(runCtx, ELOX_USTR_AND_LEN("length"), error);
	klassAddNativeMethod(openTuple, bi->biTuple.strings.length, arrayLength, 0, false);
	klassAddNativeMethod(openTuple, bi->biIterable.strings.iterator, arrayIterator, 0, false);
	bi->biTuple.class_ = tupleClass =
		(ObjClass *)klassCloseAndRegister(openTuple, bi->biTuple.strings.name, &eloxBuiltinModule);

	ObjClass *hashMapIteratorClass;
	bi->biHashMapIterator.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("$HashMapIterator"), error);
	EloxKlassHandle *openHashMapIterator = OPEN_CLASS(runCtx, 0, bi->biHashMapIterator.strings.name, error,
													  iteratorClass);
	bi->biHashMapIterator.strings.map = internString(runCtx, ELOX_USTR_AND_LEN("map"), error);
	bi->biHashMapIterator.strings.current = internString(runCtx, ELOX_USTR_AND_LEN("current"), error);
	bi->biHashMapIterator.strings.modCount = internString(runCtx, ELOX_USTR_AND_LEN("modCount"), error);
	bi->biHashMapIterator.fields.map = klassAddField(openHashMapIterator, bi->biHashMapIterator.strings.map);
	bi->biHashMapIterator.fields.current = klassAddField(openHashMapIterator, bi->biHashMapIterator.strings.current);
	bi->biHashMapIterator.fields.modCount = klassAddField(openHashMapIterator, bi->biHashMapIterator.strings.modCount);
	klassAddNativeMethod(openHashMapIterator, bi->biIterator.strings.hasNext, hashMapIteratorHasNext, 0, false);
	klassAddNativeMethod(openHashMapIterator, bi->biIterator.strings.next, hashMapIteratorNext, 0, false);
	bi->biHashMapIterator.class_ = hashMapIteratorClass =
		(ObjClass *)klassCloseAndRegister(openHashMapIterator, bi->biHashMapIterator.strings.name, &eloxBuiltinModule);

	ObjInterface *mapIntf;
	bi->biMap.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("Map"), error);
	EloxKlassHandle *openMap = openInterface(runCtx, bi->biMap.strings.name, error);
	bi->biMap.strings.size = internString(runCtx, ELOX_USTR_AND_LEN("size"), error);
	bi->biMap.strings.put = internString(runCtx, ELOX_USTR_AND_LEN("put"), error);
	bi->biMap.strings.remove = internString(runCtx, ELOX_USTR_AND_LEN("remove"), error);
	klassAddAbstractMethod(openMap, bi->biMap.strings.size, 0, false);
	klassAddAbstractMethod(openMap, bi->biMap.strings.put, 2, false);
	klassAddAbstractMethod(openMap, bi->biMap.strings.remove, 1, false);
	bi->biMap.intf = mapIntf =
		(ObjInterface *)klassCloseAndRegister(openMap, bi->biMap.strings.name, &eloxBuiltinModule);

	ObjClass *hashMapClass;
	bi->biHashMap.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("HashMap"), error);
	EloxKlassHandle *openHashMap = OPEN_CLASS(runCtx, 0, bi->biHashMap.strings.name, error,
											  objectClass, mapIntf, iterableIntf);
	klassAddNativeMethod(openHashMap, bi->biMap.strings.size, hashMapSize, 0, false);
	klassAddNativeMethod(openHashMap, bi->biMap.strings.put, hashMapPut, 2, false);
	klassAddNativeMethod(openHashMap, bi->biMap.strings.remove, hashMapRemove, 1, false);
	klassAddNativeMethod(openHashMap, bi->biIterable.strings.iterator, hashMapIterator, 0, false);
	bi->biHashMap.class_ = hashMapClass =
		(ObjClass *)klassCloseAndRegister(openHashMap, bi->biHashMap.strings.name, &eloxBuiltinModule);

	ObjClass *varargsIteratorClass;
	bi->biVarargsIterator.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("$VarargsIterator"), error);
	EloxKlassHandle *openVarargsIterator = OPEN_CLASS(runCtx, 0, bi->biVarargsIterator.strings.name, error,
													  iteratorClass);
	bi->biVarargsIterator.strings.frame = internString(runCtx, ELOX_USTR_AND_LEN("frame"), error);
	bi->biVarargsIterator.strings.cursor = internString(runCtx, ELOX_USTR_AND_LEN("cursor"), error);
	bi->biVarargsIterator.fields.frame = klassAddField(openVarargsIterator, bi->biVarargsIterator.strings.frame);
	bi->biVarargsIterator.fields.cursor = klassAddField(openVarargsIterator, bi->biVarargsIterator.strings.cursor);
	klassAddNativeMethod(openVarargsIterator, bi->biIterator.strings.hasNext, varargsIteratorHasNext, 0, false);
	klassAddNativeMethod(openVarargsIterator, bi->biIterator.strings.next, varargsIteratorNext, 0, false);
	bi->biVarargsIterator.class_ = varargsIteratorClass =
		(ObjClass *)klassCloseAndRegister(openVarargsIterator, bi->biVarargsIterator.strings.name, &eloxBuiltinModule);

	ObjClass *varargsClass;
	bi->biVarargs.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("$Varargs"), error);
	EloxKlassHandle *openVarargs = OPEN_CLASS(runCtx, 0, bi->biVarargs.strings.name, error,
											  objectClass, iterableIntf);
	bi->biVarargs.strings.length = internString(runCtx, ELOX_USTR_AND_LEN("length"), error);
	klassAddNativeMethod(openVarargs, bi->biVarargs.strings.length, varargsLength, 0, false);
	klassAddNativeMethod(openVarargs, bi->biIterable.strings.iterator, varargsIterator, 0, false);
	bi->biVarargs.class_ = varargsClass =
		(ObjClass *)klassCloseAndRegister(openVarargs, bi->biVarargs.strings.name, &eloxBuiltinModule);

	ObjClass *fiberClass;
	bi->biFiber.strings.name = internString(runCtx, ELOX_USTR_AND_LEN("Fiber"), error);
	EloxKlassHandle *openFiber = OPEN_CLASS(runCtx, CLASS_INIT_RETURNS_INST, bi->biFiber.strings.name, error,
											objectClass);
	bi->biFiber.strings.resume = internString(runCtx, ELOX_USTR_AND_LEN("resume"), error);
	bi->biFiber.strings.resumeThrow = internString(runCtx, ELOX_USTR_AND_LEN("resumeThrow"), error);
	bi->biFiber.strings.yield = internString(runCtx, ELOX_USTR_AND_LEN("yield"), error);
	bi->biFiber.strings.getCurrent = internString(runCtx, ELOX_USTR_AND_LEN("getCurrent"), error);
	klassAddNativeMethod(openFiber, bi->biFiber.strings.name, fiberInit, 1, false);
	klassAddNativeMethod(openFiber, bi->biFiber.strings.resume, fiberResume, 0, true);
	klassAddNativeMethod(openFiber, bi->biFiber.strings.resumeThrow, fiberResumeThrow, 1, false);
	klassAddStaticNativeMethod(openFiber, bi->biFiber.strings.yield, fiberYield, 0, true);
	klassAddStaticNativeMethod(openFiber, bi->biFiber.strings.getCurrent, fiberGetCurrent, 0, false);
	bi->biFiber.class_ = fiberClass =
		(ObjClass *)klassCloseAndRegister(openFiber, bi->biFiber.strings.name, &eloxBuiltinModule);

	RET_IF_RAISED(error);

	return true;
}

void clearBuiltins(VM *vm) {
	memset(&vm->builtins, 0, sizeof(vm->builtins));
}
