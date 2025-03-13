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

void addAbstractMethod(RunCtx *runCtx, Obj* parent, ObjString *methodName,
					   uint16_t arity, bool hasVarargs, EloxError *error) {
	FiberCtx *fiber = runCtx->activeFiber;

	if (ELOX_UNLIKELY(error->raised))
		return;

	bool methodExists = false;
	Value existingMethod;
	ObjInterface *intf = NULL;
	ObjClass *clazz = NULL;

	switch (parent->type) {
		case OBJ_INTERFACE:
			intf = (ObjInterface *)parent;
			methodExists = tableGet(&intf->methods, methodName, &existingMethod);
			break;
		case OBJ_CLASS: {
			clazz = (ObjClass *)parent;
			PropInfo propInfo = propTableGetAny(&clazz->props, methodName);
			if (propInfo.type != ELOX_PROP_NONE) {
				if (ELOX_UNLIKELY(propInfo.type != ELOX_PROP_METHOD))
					ELOX_RAISE_RET(error, RTERR(runCtx, "Method %.*s shadows existing property",
												methodName->string.length, methodName->string.chars));
				methodExists = true;
				existingMethod = clazz->classData.values[propInfo.index];
			}
			break;
		}
		default:
			ELOX_RAISE_RET(error, RTERR(runCtx, "Not a class or interface"));
			break;
	}

	ObjMethodDesc *methodDesc = newMethodDesc(runCtx, arity, hasVarargs);
	ELOX_CHECK_RAISE_RET((methodDesc != NULL), error, OOM(runCtx));

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	PUSH_TEMP(temps, protectedMethod, OBJ_VAL(methodDesc));

	if (methodExists) {
		Obj *existingMethodObj = AS_OBJ(existingMethod);
		ELOX_CHECK_RAISE_RET((existingMethodObj->type == OBJ_METHOD_DESC),
							 error, RTERR(runCtx, "Abstract method cannot override method"));

		if (!prototypeMatches((Obj *)methodDesc, AS_OBJ(existingMethod))) {
			ELOX_RAISE_RET(error, RTERR(runCtx, "Method %.*s overrides incompatible method",
										methodName->string.length, methodName->string.chars));
		}
	} else {
		switch(parent->type) {
			case OBJ_INTERFACE:
				// no need to check for now, we return after this anyway
				tableSet(runCtx, &intf->methods, methodName, OBJ_VAL(methodDesc), error);
				break;
			case OBJ_CLASS: {
				bool pushed = pushClassData(runCtx, clazz, OBJ_VAL(methodDesc));
				ELOX_CHECK_RAISE_RET(pushed, error, OOM(runCtx));
				uint32_t methodIndex = clazz->classData.count - 1;
				// no need to check for now, we return after this anyway
				propTableSet(runCtx, &clazz->props, methodName,
							 (PropInfo){methodIndex, ELOX_PROP_METHOD, ELOX_PROP_METHOD_MASK }, error);
				break;
			}
			default:
				ELOX_UNREACHABLE();
		}
	}

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
	ELOX_CHECK_RAISE_GOTO(nativeObj != NULL, error, OOM(runCtx), cleanup);
	pushTempVal(temps, &protectedNative, OBJ_VAL(nativeObj));
	if (methodName == clazz->name)
		clazz->initializer = OBJ_VAL(nativeObj);
	else {
		ObjMethod *method = newMethod(runCtx, clazz, (Obj *)nativeObj);
		ELOX_CHECK_RAISE_GOTO(method != NULL, error, OOM(runCtx), cleanup);
		pushTempVal(temps, &protectedMethod, OBJ_VAL(method));
		EloxError tableError = ELOX_ERROR_INITIALIZER;
		bool pushed = pushClassData(runCtx, clazz, OBJ_VAL(method));
		if (ELOX_UNLIKELY(!pushed)) {
			ELOX_RAISE(error, OOM(runCtx));
			goto cleanup;
		}
		clazz->tables[ELOX_DT_CLASS] = clazz->classData.values;
		uint32_t methodIndex = clazz->classData.count - 1;

		size_t savedStack = saveStack(fiber);
		propTableSet(runCtx, &clazz->props, methodName,
					 (PropInfo){methodIndex, ELOX_PROP_METHOD, ELOX_PROP_METHOD_MASK }, &tableError);
		if (ELOX_UNLIKELY(tableError.raised)) {
			tableError.discardException(fiber, savedStack);
			ELOX_RAISE(error, OOM(runCtx));
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
	size_t savedStack = saveStack(fiber);
	propTableSet(runCtx, &clazz->props, fieldName,
				 (PropInfo){index, ELOX_PROP_FIELD, ELOX_PROP_FIELD_MASK }, &tableError);
	if (ELOX_UNLIKELY(tableError.raised)) {
		tableError.discardException(fiber, savedStack);
		ELOX_RAISE_RET_VAL(error, OOM(runCtx), -1);
	}
	clazz->numFields++;

	return index;
}

void resolveRef(RunCtx *runCtx, ObjClass *clazz, uint8_t slotType,
				ObjString *propName, uint16_t slot, EloxError *error)
{
	bool super = slotType & 0x1;
	uint8_t propType = (slotType & 0x6) >> 1;

	if (super) {
		ObjClass *superClass = AS_CLASS(clazz->super);
		PropInfo propInfo = propTableGet(&superClass->props, propName, ELOX_PROP_METHOD_MASK);
		ELOX_CHECK_RAISE_RET(propInfo.type != ELOX_PROP_NONE, error,
							 RTERR(runCtx, "Undefined property '%s'",
								   propName->string.chars));
		clazz->refs[slot] = (Ref){
			.tableIndex = ELOX_DT_SUPER,
			.isMethod = true,
			.propIndex = propInfo.index
		};
	} else {
		int32_t propIndex = -1;
		bool isField = false;
		bool isMethod = false;

		if (propType & MEMBER_FIELD) {
			PropInfo info = propTableGet(&clazz->props, propName, ELOX_PROP_FIELD_MASK);
			if (info.type != ELOX_PROP_NONE) {
				propIndex = info.index;
				isField = true;
			}
		}
		if ((propIndex < 0) && (propType & MEMBER_METHOD)) {
			PropInfo propInfo = propTableGet(&clazz->props, propName, ELOX_PROP_METHOD_MASK);
			if (propInfo.type != ELOX_PROP_NONE) {
				propIndex = propInfo.index;
				isMethod = true;
			}
		}

		ELOX_CHECK_RAISE_RET((propIndex >= 0), error, RTERR(runCtx, "Undefined property '%s'",
															propName->string.chars));

		clazz->refs[slot] = (Ref){
			.tableIndex = isField ? ELOX_DT_INST : ELOX_DT_CLASS,
			.isMethod = isMethod,
			.propIndex = propIndex
		};
	}
}

void initOpenClass(OpenClass *oc, RunCtx *runCtx, ObjClass *clazz) {
	oc->runCtx = runCtx;
	oc->clazz = clazz;

	if (oc->fileName != NULL) {
		oc->cCtx.compilerHandle = getCompiler(runCtx);
		if (ELOX_UNLIKELY(oc->cCtx.compilerHandle == NULL)) {
			ELOX_RAISE_RET(oc->error, OOM(runCtx));
		}
		if (ELOX_UNLIKELY(!initCompilerContext(&oc->cCtx, runCtx, oc->fileName, oc->moduleName))) {
			ELOX_RAISE_RET(oc->error, OOM(runCtx));
		}

		CompilerState *compilerState = &oc->cCtx.compilerHandle->compilerState;
		ClassCompiler *classCompiler = &oc->classCompiler;

		initTable(&classCompiler->pendingThisProperties);
		initTable(&classCompiler->pendingSuperProperties);
		classCompiler->enclosing = NULL;
		classCompiler->hasExplicitInitializer = false;
		compilerState->currentClass = classCompiler;
	}
}

void classAddAbstractMethod(OpenClass *oc, ObjString *methodName, uint16_t arity, bool hasVarargs) {
	addAbstractMethod(oc->runCtx, (Obj *)oc->clazz, methodName, arity, hasVarargs, oc->error);
}

ObjMethod *classAddCompiledMethod(OpenClass *oc, uint8_t *src) {
	RunCtx *runCtx = oc->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;
	EloxError *error = oc->error;

	if (ELOX_UNLIKELY(error->raised))
		return NULL;

	ObjClass *clazz = oc->clazz;

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);

	ObjMethod *method = (ObjMethod *)compileFunction(runCtx, &oc->cCtx, oc->clazz, src, error);
	// TODO: check

	ObjString *methodName;
	Obj *callable = method->callable;
	if (callable->type == OBJ_CLOSURE)
		methodName = ((ObjClosure *)callable)->function->name;
	else
		methodName = ((ObjFunction *)callable)->name;

	PUSH_TEMP(temps, protectedMethod, OBJ_VAL(method));
	EloxError tableError = ELOX_ERROR_INITIALIZER;
	bool pushed = pushClassData(runCtx, clazz, OBJ_VAL(method));
	ELOX_CHECK_RAISE_GOTO(pushed, error, OOM(runCtx), cleanup);
	clazz->tables[ELOX_DT_CLASS] = clazz->classData.values;
	uint32_t methodIndex = clazz->classData.count - 1;

	size_t savedStack = saveStack(fiber);
	propTableSet(runCtx, &clazz->props, methodName,
				 (PropInfo){methodIndex, ELOX_PROP_METHOD, ELOX_PROP_METHOD_MASK }, &tableError);
	if (ELOX_UNLIKELY(tableError.raised)) {
		tableError.discardException(fiber, savedStack);
		ELOX_RAISE_GOTO(error, OOM(runCtx), cleanup);
	}
cleanup:
	releaseTemps(&temps);

	return NULL;
}

ObjClass *classClose(OpenClass *oc) {
	RunCtx *runCtx = oc->runCtx;
	EloxError *error = oc->error;

	ObjClass *ret = NULL;

	if (oc->fileName != NULL) {
		Table *pendingThis = &oc->classCompiler.pendingThisProperties;
		Table *pendingSuper = &oc->classCompiler.pendingSuperProperties;

		ObjClass *clazz = oc->clazz;
		ObjClass *super = AS_CLASS(clazz->super);
		uint16_t numSlots = pendingThis->count + pendingSuper->count;

		uint16_t superNumRefs = super->numRefs;
		uint16_t totalNumRefs = superNumRefs + numSlots;
		if (totalNumRefs > 0) {
			clazz->refs = ALLOCATE(runCtx, Ref, totalNumRefs);
			if (ELOX_UNLIKELY(clazz->refs == NULL))
				goto cleanup;
			clazz->numRefs = totalNumRefs;
		}
		if (superNumRefs > 0)
			memcpy(clazz->refs, super->refs, superNumRefs * sizeof(Ref));

		if (pendingThis->count + pendingSuper->count > 0) {
			for (int i = 0; i < pendingThis->capacity; i++) {
				Entry *entry = &pendingThis->entries[i];
				if (entry->key != NULL) {
					uint32_t slot = AS_NUMBER(entry->value);
					ObjString *propName = entry->key;
					uint8_t slotType = getRefSlotType(slot, false);
					resolveRef(runCtx, clazz, slotType, propName, slot, error);
					if (ELOX_UNLIKELY(error->raised))
						goto cleanup;
				}
			}
			for (int i = 0; i < pendingSuper->capacity; i++) {
				Entry *entry = &pendingSuper->entries[i];
				if (entry->key != NULL) {
					uint32_t slot = AS_NUMBER(entry->value);
					ObjString *propName = entry->key;
					uint8_t slotType = getRefSlotType(slot, true);
					resolveRef(runCtx, clazz, slotType, propName, slot, error);
					if (ELOX_UNLIKELY(error->raised))
						goto cleanup;
				}
			}
		}

		bool hasAbstractMethods = false;
		for (int m = 0; m < clazz->props.capacity; m++) {
			PropEntry *entry = &clazz->props.entries[m];
			ObjString *methodName = entry->key;
			if ((methodName != NULL) && (entry->value.type == ELOX_PROP_METHOD)) {
				Obj *method = AS_OBJ(clazz->classData.values[entry->value.index]);
				if (method->type == OBJ_METHOD_DESC) {
					if (!clazz->abstract) {
						ELOX_RAISE_GOTO(error, RTERR(runCtx, "Unimplemented method %.*s",
													 methodName->string.length, methodName->string.chars),
										cleanup);
					} else {
						hasAbstractMethods = true;
						break;
					}
				}
			}
		}
		if (clazz->abstract && (!hasAbstractMethods))
			ELOX_RAISE_GOTO(error, RTERR(runCtx, "Abstract class has no abstract methods"), cleanup);
	}

	ObjClass *clazz = oc->clazz;
	oc->clazz = NULL;
	ret = clazz;

cleanup:
	if (oc->fileName != NULL) {
		if (oc->cCtx.compilerHandle != NULL) {
			freeTable(oc->runCtx, &oc->classCompiler.pendingThisProperties);
			freeTable(oc->runCtx, &oc->classCompiler.pendingSuperProperties);

			eloxReleaseHandle((EloxHandle *)oc->cCtx.compilerHandle);
		}
	}
	return ret;
}

