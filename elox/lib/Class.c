// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/Class.h>
#include <elox/state.h>

#include <string.h>

ObjInterface *newInterface(RunCtx *runCtx, ObjString *name) {
	ObjInterface *intf = ALLOCATE_OBJ(runCtx, ObjInterface, OBJ_INTERFACE);
	if (ELOX_UNLIKELY(intf == NULL))
		return NULL;
	intf->name = name;
	intf->typeCheckOffset = ELOX_CLASS_DISPLAY_SIZE;
	initTable(&intf->methods);
	return intf;
}

ObjClass *newClass(RunCtx *runCtx, ObjString *name, bool abstract) {
	VM *vm = runCtx->vm;
	FiberCtx * fiber = runCtx->activeFiber;

	ObjString *className = name;

	if (name->string.length == 0) {
		HeapCString ret;
		bool res = initHeapStringWithSize(runCtx, &ret, 16);
		if (ELOX_UNLIKELY(!res))
			return NULL;
		heapStringAddFmt(runCtx, &ret, "Class_%lu", stc64_rand(&vm->prng) & 0xFFFFFFFF);
		className = takeString(runCtx, ret.chars, ret.length, ret.capacity);
		if (ELOX_UNLIKELY(className == NULL))
			return NULL;
		push(fiber, OBJ_VAL(className));
	}

	ObjClass *clazz = ALLOCATE_OBJ(runCtx, ObjClass, OBJ_CLASS);
	if (ELOX_UNLIKELY(clazz == NULL))
		return NULL;
	memset(&clazz->typeInfo, 0, sizeof(TypeInfo));
	clazz->name = className;
	if (name->string.length == 0)
		pop(fiber);
	clazz->initializer = NIL_VAL;
	clazz->hashCode = NULL;
	clazz->equals = NULL;
	clazz->super = NIL_VAL;
	initPropTable(&clazz->props);
	initValueArray(&clazz->classData);
	clazz->numFields = 0;
	clazz->refs = NULL;
	clazz->numRefs = 0;
	clazz->abstract = abstract;
	return clazz;
}

ObjInstance *newInstance(RunCtx *runCtx, ObjClass *clazz) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	ObjInstance *ret = NULL;
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	VMTemp protectedInstance = TEMP_INITIALIZER;

	ObjInstance *instance = ALLOCATE_OBJ(runCtx, ObjInstance, OBJ_INSTANCE);
	if (ELOX_UNLIKELY(instance == NULL))
		return NULL;
	pushTempVal(temps, &protectedInstance, OBJ_VAL(instance));
	instance->clazz = clazz;
	instance->numFields = 0;
	instance->fields = ALLOCATE(runCtx, Value, clazz->numFields);
	if (ELOX_UNLIKELY((clazz->numFields > 0) && (instance->fields == NULL)))
		goto cleanup;
	instance->numFields = clazz->numFields;
	for (uint16_t i = 0; i < instance->numFields; i++)
		instance->fields[i] = NIL_VAL;

	instance->tables[ELOX_DT_SUPER] = clazz->tables[ELOX_DT_SUPER];
	instance->tables[ELOX_DT_CLASS] = clazz->tables[ELOX_DT_CLASS];
	instance->tables[ELOX_DT_INST] = instance->fields;

	instance->identityHash = stc64_rand(&vm->prng) & 0xFFFFFFFF;
	instance->flags =
		INST_HAS_HASHCODE * (clazz->hashCode != NULL) |
		INST_HAS_EQUALS * (clazz->equals != NULL);

	ret = instance;

cleanup:
	releaseTemps(&temps);

	return ret;
}

ObjBoundMethod *newBoundMethod(RunCtx *runCtx,Value receiver, ObjMethod *method) {
	ObjBoundMethod *bound = ALLOCATE_OBJ(runCtx, ObjBoundMethod, OBJ_BOUND_METHOD);
	if (ELOX_UNLIKELY(bound == NULL))
		return NULL;
	bound->receiver = receiver;
	bound->method = method->callable;
	return bound;
}

ObjMethod *newMethod(RunCtx *runCtx, ObjClass *clazz, Obj *callable) {
	ObjMethod *method = ALLOCATE_OBJ(runCtx, ObjMethod, OBJ_METHOD);
	if (ELOX_UNLIKELY(method == NULL))
		return NULL;
	method->clazz = clazz;
	method->callable = callable;
	return method;
}

ObjMethodDesc *newMethodDesc(RunCtx *runCtx, uint8_t arity, bool hasVarargs) {
	ObjMethodDesc *methodDesc = ALLOCATE_OBJ(runCtx, ObjMethodDesc, OBJ_METHOD_DESC);
	if (ELOX_UNLIKELY(methodDesc == NULL))
		return NULL;
	// +1 for this
	methodDesc->arity = arity + 1;
	methodDesc->hasVarargs = hasVarargs;
	return methodDesc;
}

void addMethod(RunCtx *runCtx, ObjInterface *intf, ObjString *methodName,
			   uint16_t arity, bool hasVarargs, EloxError *error) {
	FiberCtx *fiber = runCtx->activeFiber;

	if (ELOX_UNLIKELY(error->raised))
		return;

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);

	ObjMethodDesc *methodDesc = newMethodDesc(runCtx, arity, hasVarargs);
	ELOX_CHECK_RAISE_GOTO(methodDesc != NULL, error, "Out of memory", cleanup);
	PUSH_TEMP(temps, protectedMethod, OBJ_VAL(methodDesc));

	EloxError tableError = ELOX_ERROR_INITIALIZER;
	tableSet(runCtx, &intf->methods, methodName, OBJ_VAL(methodDesc), &tableError);
	if (ELOX_UNLIKELY(tableError.raised)) {
		pop(fiber); // discard error
		ELOX_RAISE(error, "Out of memory");
		goto cleanup;
	}

cleanup:
	releaseTemps(&temps);
}

ObjNative *addNativeMethod(RunCtx *runCtx, ObjClass *clazz, ObjString *methodName,
						   NativeFn method, uint16_t arity, bool hasVarargs,
						   EloxError *error) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	if (ELOX_UNLIKELY(error->raised))
		return NULL;

	arity += 1; // this

	ObjNative *ret = NULL;
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);

	VMTemp protectedNative = TEMP_INITIALIZER;
	VMTemp protectedMethod = TEMP_INITIALIZER;

	ObjNative *nativeObj = newNative(runCtx, method, arity);
	ELOX_CHECK_RAISE_GOTO(nativeObj != NULL, error, "Out of memory", cleanup);
	pushTempVal(temps, &protectedNative, OBJ_VAL(nativeObj));
	if (methodName == clazz->name)
		clazz->initializer = OBJ_VAL(nativeObj);
	else {
		ObjMethod *method = newMethod(runCtx, clazz, (Obj *)nativeObj);
		ELOX_CHECK_RAISE_GOTO(method != NULL, error, "Out of memory", cleanup);
		pushTempVal(temps, &protectedMethod, OBJ_VAL(method));
		EloxError tableError = ELOX_ERROR_INITIALIZER;
		bool pushed = pushClassData(runCtx, clazz, OBJ_VAL(method));
		if (ELOX_UNLIKELY(!pushed)) {
			ELOX_RAISE(error, "Out of memory");
			goto cleanup;
		}
		clazz->tables[ELOX_DT_CLASS] = clazz->classData.values;
		uint32_t methodIndex = clazz->classData.count - 1;
		propTableSet(runCtx, &clazz->props, methodName,
					 (PropInfo){methodIndex, ELOX_PROP_METHOD, ELOX_PROP_METHOD_MASK }, &tableError);
		if (ELOX_UNLIKELY(tableError.raised)) {
			pop(fiber); // discard error
			ELOX_RAISE(error, "Out of memory");
			goto cleanup;
		}
		if (methodName == vm->builtins.biObject.hashCodeStr)
			clazz->hashCode = method;
		else if (methodName == vm->builtins.equalsString)
			clazz->equals = method;
	}
	nativeObj->arity = arity;
	nativeObj->maxArgs = hasVarargs ? ELOX_MAX_ARGS : arity;

	ret = nativeObj;

cleanup:
	releaseTemps(&temps);

	return ret;
}

int addClassField(RunCtx *runCtx, ObjClass *clazz, ObjString *fieldName, EloxError *error) {
	FiberCtx *fiber = runCtx->activeFiber;

	if (ELOX_UNLIKELY(error->raised))
		return -1;

	int index = clazz->numFields;

	EloxError tableError = ELOX_ERROR_INITIALIZER;
	propTableSet(runCtx, &clazz->props, fieldName,
				 (PropInfo){index, ELOX_PROP_FIELD, ELOX_PROP_FIELD_MASK }, &tableError);
	if (ELOX_UNLIKELY(tableError.raised)) {
		pop(fiber); // discard error
		ELOX_RAISE(error, "Out of memory");
		return -1;
	}
	clazz->numFields++;

	return index;
}

