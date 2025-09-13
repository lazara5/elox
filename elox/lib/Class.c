// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/Class.h>
#include <elox/state.h>

#include <string.h>
#include <assert.h>

ObjInterface *newInterface(RunCtx *runCtx, ObjString *name) {
	ObjInterface *intf = ALLOCATE_OBJ(runCtx, ObjInterface, OBJ_INTERFACE);
	if (ELOX_UNLIKELY(intf == NULL))
		return NULL;
	intf->name = name;
	intf->typeCheckOffset = ELOX_CLASS_DISPLAY_SIZE;
	initTable(&intf->methods);
	intf->openKlass = NULL;
	return intf;
}

ObjClass *newClass(RunCtx *runCtx, ObjString *name, uint8_t flags) {
	VM *vm = runCtx->vmCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

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

	ObjClass *class_ = ALLOCATE_OBJ(runCtx, ObjClass, OBJ_CLASS);
	if (ELOX_UNLIKELY(class_ == NULL))
		return NULL;

	memset(&class_->typeInfo, 0, sizeof(TypeInfo));
	class_->name = className;
	if (name->string.length == 0)
		pop(fiber);
	class_->initializer = NIL_VAL;
	class_->hashCode = NULL;
	class_->equals = NULL;
	class_->super = NULL;
	initPropTable(&class_->props);
	initValueArray(&class_->classData);
	class_->numFields = 0;
	class_->refs = NULL;
	class_->numRefs = 0;
	class_->flags = flags;

	ObjClass *ret = NULL;

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	PUSH_TEMP(temps, protectedClass, OBJ_VAL(class_));

	class_->openKlass = newOpenKlass(runCtx, (ObjKlass *)class_);
	if (ELOX_UNLIKELY(class_->openKlass == NULL))
		goto cleanup;

	ret = class_;

cleanup:
	releaseTemps(&temps);
	return ret;
}

Obj *newInstance(RunCtx *runCtx, ObjClass *class_) {
	VM *vm = runCtx->vmCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

	Obj *ret = NULL;
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	VMTemp protectedInstance = TEMP_INITIALIZER;

	ObjInstance *instance = ALLOCATE_OBJ(runCtx, ObjInstance, OBJ_INSTANCE);
	if (ELOX_UNLIKELY(instance == NULL))
		return NULL;
	pushTempVal(temps, &protectedInstance, OBJ_VAL(instance));
	instance->class_ = class_;
	instance->numFields = 0;
	instance->fields = ALLOCATE(runCtx, Value, class_->numFields);
	if (ELOX_UNLIKELY((class_->numFields > 0) && (instance->fields == NULL)))
		goto cleanup;
	instance->numFields = class_->numFields;
	for (uint16_t i = 0; i < instance->numFields; i++)
		instance->fields[i] = NIL_VAL;

	instance->tables[ELOX_DT_SUPER] = class_->tables[ELOX_DT_SUPER];
	instance->tables[ELOX_DT_CLASS] = class_->tables[ELOX_DT_CLASS];
	instance->tables[ELOX_DT_INST] = instance->fields;

	instance->identityHash = stc64_rand(&vm->prng) & 0xFFFFFFFF;
	instance->flags =
		INST_HAS_HASHCODE * (class_->hashCode != NULL) |
		INST_HAS_EQUALS * (class_->equals != NULL);

	ret = (Obj *)instance;

cleanup:
	releaseTemps(&temps);

	return ret;
}

ObjBoundMethod *newBoundMethod(RunCtx *runCtx,Value receiver, ObjMethod *method) {
	ObjBoundMethod *bound = ALLOCATE_OBJ(runCtx, ObjBoundMethod, OBJ_BOUND_METHOD);
	if (ELOX_UNLIKELY(bound == NULL))
		return NULL;
	bound->receiver = receiver;
	bound->method = method->method.callable;
	return bound;
}

ObjMethod *newMethod(RunCtx *runCtx, ObjKlass *klass, Obj *callable) {
	ObjMethod *method = ALLOCATE_OBJ(runCtx, ObjMethod, OBJ_METHOD);
	if (ELOX_UNLIKELY(method == NULL))
		return NULL;

	method->method.klass = klass;
	method->method.callable = callable;
	method->method.fromDefault = NULL;
	method->isConflicted = false;

	return method;
}

ObjMethod *newPendingMethod(RunCtx *runCtx, ObjDefaultMethod *defaultMethod) {
	ObjMethod *method = ALLOCATE_OBJ(runCtx, ObjMethod, OBJ_PENDING_METHOD);
	if (ELOX_UNLIKELY(method == NULL))
		return NULL;

	method->pending.fromDefault = defaultMethod;
	method->isConflicted = false;

	return method;
}

ObjDefaultMethod *newDefaultMethod(RunCtx *runCtx, ObjFunction *function) {
	ObjDefaultMethod *method = ALLOCATE_OBJ(runCtx, ObjDefaultMethod, OBJ_DEFAULT_METHOD);
	if (ELOX_UNLIKELY(method == NULL))
		return NULL;
	method->function = function;
	method->refs = NULL;
	method->numRefs = 0;
	return method;
}

ObjMethod *newAbstractMethod(RunCtx *runCtx, uint8_t arity, bool hasVarargs) {
	ObjMethod *method = ALLOCATE_OBJ(runCtx, ObjMethod, OBJ_ABSTRACT_METHOD);
	if (ELOX_UNLIKELY(method == NULL))
		return NULL;
	// +1 for this
	method->abstract.proto.arity = arity + 1;
	method->abstract.proto.hasVarargs = hasVarargs;
	return method;
}

void addAbstractMethod(RunCtx *runCtx, Obj* parent, ObjString *methodName,
					   uint16_t arity, bool hasVarargs, EloxError *error) {
	ObjFiber *fiber = runCtx->activeFiber;

	if (ELOX_UNLIKELY(error->raised))
		return;

	bool methodExists = false;
	Value existingMethod;
	ObjInterface *intf = NULL;
	ObjClass *clazz = NULL;

	switch (getObjType(parent)) {
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

	ObjMethod *abstract = newAbstractMethod(runCtx, arity, hasVarargs);
	ELOX_CHECK_RAISE_RET((abstract != NULL), error, OOM(runCtx));

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	PUSH_TEMP(temps, protectedMethod, OBJ_VAL(abstract));

	if (methodExists) {
		Obj *existingMethodObj = AS_OBJ(existingMethod);
		ELOX_CHECK_RAISE_RET((getObjType(existingMethodObj) == OBJ_ABSTRACT_METHOD),
							 error, RTERR(runCtx, "Abstract method cannot override method"));

		if (!prototypeMatches((Obj *)abstract, AS_OBJ(existingMethod))) {
			ELOX_RAISE_RET(error, RTERR(runCtx, "Method %.*s overrides incompatible method",
										methodName->string.length, methodName->string.chars));
		}
	} else {
		switch(getObjType(parent)) {
			case OBJ_INTERFACE:
				// no need to check for now, we return after this anyway
				tableSet(runCtx, &intf->methods, methodName, OBJ_VAL(abstract), error);
				break;
			case OBJ_CLASS: {
				int methodIndex = pushClassData(runCtx, clazz, OBJ_VAL(abstract));
				ELOX_CHECK_RAISE_RET(methodIndex >= 0, error, OOM(runCtx));
				// no need to check for now, we return after this anyway
				propTableSet(runCtx, &clazz->props, methodName,
							 (PropInfo){ methodIndex, ELOX_PROP_METHOD, ELOX_PROP_METHOD_MASK }, error);
				break;
			}
			default:
				ELOX_UNREACHABLE();
		}
	}

	releaseTemps(&temps);
}

ObjNative *addStaticNativeMethod(RunCtx *runCtx, ObjClass *clazz, ObjString *methodName,
								 NativeFn method, uint16_t arity, bool hasVarargs,
								 EloxError *error) {
	ObjFiber *fiber = runCtx->activeFiber;

	if (ELOX_UNLIKELY(error->raised))
		return NULL;

	ObjNative *ret = NULL;
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);

	VMTemp protectedNative = TEMP_INITIALIZER;

	ObjNative *nativeObj = newNative(runCtx, method, arity);
	ELOX_CHECK_RAISE_GOTO(nativeObj != NULL, error, OOM(runCtx), cleanup);
	pushTempVal(temps, &protectedNative, OBJ_VAL(nativeObj));

	EloxError tableError = ELOX_ERROR_INITIALIZER;
	int methodIndex = pushClassData(runCtx, clazz, OBJ_VAL(nativeObj));
	ELOX_CHECK_RAISE_GOTO(methodIndex >= 0, error, OOM(runCtx), cleanup);

	size_t savedStack = saveStack(fiber);
	propTableSet(runCtx, &clazz->props, methodName,
				 (PropInfo){methodIndex, ELOX_PROP_STATIC, ELOX_PROP_STATIC_MASK }, &tableError);
	if (ELOX_UNLIKELY(tableError.raised)) {
		tableError.discardException(fiber, savedStack);
		ELOX_RAISE_GOTO(error, OOM(runCtx), cleanup);
	}

	nativeObj->arity = arity;
	nativeObj->maxArgs = hasVarargs ? ELOX_MAX_ARGS : arity;

	ret = nativeObj;

cleanup:
	releaseTemps(&temps);

	return ret;
}

ObjNative *addNativeMethod(RunCtx *runCtx, ObjClass *clazz, ObjString *methodName,
						   NativeFn method, uint16_t arity, bool hasVarargs,
						   EloxError *error) {
	VM *vm = runCtx->vmCtx->vm;
	ObjFiber *fiber = runCtx->activeFiber;

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
		ObjMethod *method = newMethod(runCtx, (ObjKlass *)clazz, (Obj *)nativeObj);
		ELOX_CHECK_RAISE_GOTO(method != NULL, error, OOM(runCtx), cleanup);
		pushTempVal(temps, &protectedMethod, OBJ_VAL(method));
		EloxError tableError = ELOX_ERROR_INITIALIZER;
		int methodIndex = pushClassData(runCtx, clazz, OBJ_VAL(method));
		ELOX_CHECK_RAISE_GOTO(methodIndex >= 0, error, OOM(runCtx), cleanup);

		size_t savedStack = saveStack(fiber);
		propTableSet(runCtx, &clazz->props, methodName,
					 (PropInfo){methodIndex, ELOX_PROP_METHOD, ELOX_PROP_METHOD_MASK }, &tableError);
		if (ELOX_UNLIKELY(tableError.raised)) {
			tableError.discardException(fiber, savedStack);
			ELOX_RAISE_GOTO(error, OOM(runCtx), cleanup);
		}
		if (methodName == vm->builtins.biObject.strings.hashCode)
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
	ObjFiber *fiber = runCtx->activeFiber;

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

void bindRef(RunCtx *runCtx, ObjClass *clazz, bool isSuper,
			 ObjString *propName, uint32_t slotVal, EloxError *error)
{
	uint32_t slotIndex = slotVal & 0xFFFFFF;
	uint8_t propType = (slotVal >> 24) & 0xFF;

	if (isSuper) {
		ObjClass *superClass = clazz->super;
		PropInfo propInfo = propTableGet(&superClass->props, propName, ELOX_PROP_METHOD_MASK);
		ELOX_CHECK_RAISE_RET(propInfo.type != ELOX_PROP_NONE, error,
							 RTERR(runCtx, "Undefined property '%s'", propName->string.chars));
		clazz->refs[slotIndex] = (Ref){
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

		clazz->refs[slotIndex] = (Ref){
			.tableIndex = isField ? ELOX_DT_INST : ELOX_DT_CLASS,
			.isMethod = isMethod,
			.propIndex = propIndex
		};
#ifdef ELOX_DEBUG_TRACE_EXECUTION
		eloxPrintf(runCtx, ELOX_IO_DEBUG, "[%u](%s)<->%c%c[%d]\n",
				   slotIndex, propName->string.chars, isField ? 'I' : 'C', isMethod ? 'M' : ' ', propIndex);
#endif
	}
}

ObjNative *klassAddNativeMethod(EloxKlassHandle *okh, ObjString *methodName,
								NativeFn method, uint16_t arity, bool hasVarargs) {
	if (ELOX_UNLIKELY(okh == NULL))
		return NULL;
	return addNativeMethod(okh->runCtx, (ObjClass *)okh->klass, methodName, method,
						   arity, hasVarargs, okh->klass->openKlass->error);
}

ObjNative *klassAddStaticNativeMethod(EloxKlassHandle *okh, ObjString *methodName,
									  NativeFn method, uint16_t arity, bool hasVarargs) {
	if (ELOX_UNLIKELY(okh == NULL))
		return NULL;
	return addStaticNativeMethod(okh->runCtx, (ObjClass *)okh->klass, methodName, method,
								 arity, hasVarargs, okh->klass->openKlass->error);
}

void klassAddAbstractMethod(EloxKlassHandle *okh, ObjString *methodName,
							uint16_t arity, bool hasVarargs) {
	if (ELOX_UNLIKELY(okh == NULL))
		return;
	addAbstractMethod(okh->runCtx, (Obj *)okh->klass, methodName,
					  arity, hasVarargs, okh->klass->openKlass->error);
}

int klassAddField(EloxKlassHandle *okh, ObjString *fieldName) {
	if (ELOX_UNLIKELY(okh == NULL))
		return -1;
	return addClassField(okh->runCtx, (ObjClass *)okh->klass, fieldName, okh->klass->openKlass->error);
}

static CCtx *getOpenKlassCCtx(OpenKlass *ok, String *fileName, String *moduleName) {
	RunCtx *runCtx = ok->runCtx;

	if (ok->cCtx.compilerHandle == NULL) {
		ok->cCtx.compilerHandle = getCompiler(runCtx);
		if (ELOX_UNLIKELY(ok->cCtx.compilerHandle == NULL)) {
			ELOX_RAISE_RET_VAL(ok->error, OOM(runCtx), NULL);
		}
		if (ELOX_UNLIKELY(!initCompilerContext(&ok->cCtx, runCtx, fileName, moduleName))) {
			ELOX_RAISE_RET_VAL(ok->error, OOM(runCtx), NULL);
		}

		CompilerState *compilerState = &ok->cCtx.compilerHandle->compilerState;
		KlassCompiler *klassCompiler = &ok->klassCompiler;

		klassCompiler->enclosing = NULL;
		klassCompiler->hasExplicitInitializer = false;
		compilerState->currentKlass = klassCompiler;
	}

	return &ok->cCtx;
}

ObjMethod *klassAddCompiledMethod(EloxKlassHandle *okh, uint8_t *src,
								  String *fileName, String *moduleName) {
	if (ELOX_UNLIKELY(okh == NULL))
		return NULL;

	RunCtx *runCtx = okh->runCtx;
	ObjFiber *fiber = runCtx->activeFiber;

	OpenKlass *ok = okh->klass->openKlass;
	EloxError *error = ok->error;

	if (ELOX_UNLIKELY(error->raised))
		return NULL;

	ObjKlass *klass = ok->klass;

	CCtx *cCtx = getOpenKlassCCtx(ok, fileName, moduleName);
	if (ELOX_UNLIKELY(cCtx == NULL))
		return NULL;

	MethodCompiler methodCompiler;
	Obj *method = compileFunction(runCtx, &ok->cCtx, initMethodCompiler(&methodCompiler),
								  ok->klass, src, error);
	if (ELOX_UNLIKELY(error->raised))
		return NULL;

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	PUSH_TEMP(temps, protectedMethod, OBJ_VAL(method));

	ObjString *methodName;
	switch (getObjType(method)) {
		case OBJ_METHOD: {
			Obj *callable = ((ObjMethod *)method)->method.callable;
			if (getObjType(callable) == OBJ_CLOSURE)
				methodName = ((ObjClosure *)callable)->function->name;
			else
				methodName = ((ObjFunction *)callable)->name;

			ObjClass *clazz = (ObjClass *)klass;
			EloxError tableError = ELOX_ERROR_INITIALIZER;
			int methodIndex = pushClassData(runCtx, clazz, OBJ_VAL(method));
			ELOX_CHECK_RAISE_GOTO(methodIndex >= 0, error, OOM(runCtx), cleanup);
			clazz->tables[ELOX_DT_CLASS] = clazz->classData.values;

			size_t savedStack = saveStack(fiber);
			propTableSet(runCtx, &clazz->props, methodName,
						 (PropInfo){methodIndex, ELOX_PROP_METHOD, ELOX_PROP_METHOD_MASK }, &tableError);
			if (ELOX_UNLIKELY(tableError.raised)) {
				tableError.discardException(fiber, savedStack);
				ELOX_RAISE_GOTO(error, OOM(runCtx), cleanup);
			}

			break;
		}
		case OBJ_DEFAULT_METHOD: {
			ObjDefaultMethod *defaultMethod = (ObjDefaultMethod *)method;
			ObjInterface *intf = (ObjInterface *)klass;
			methodName = defaultMethod->function->name;

			ValueArray *pendingRefs = &methodCompiler.pendingRefs;
			uint32_t numRefs = pendingRefs->count;
			defaultMethod->refs = ALLOCATE(runCtx, RefBindDesc, numRefs);
			ELOX_CHECK_RAISE_GOTO((defaultMethod->refs != NULL), error, OOM(runCtx), cleanup);
			defaultMethod->numRefs = numRefs;
			for (uint32_t i = 0; i < numRefs; i++) {
				uint64_t val = AS_NUMBER(pendingRefs->values[i]);
				uint32_t offset = val & 0xFFFFFF;
				uint8_t memberType = (val & MEMBER_ANY_MASK) >> 30;
				uint16_t nameHandle = (val >> 32) & 0xFFFF;
				uint8_t refType = (val >> 48) & 0xF;
				int8_t slotType = refType | memberType << 1;

				defaultMethod->refs[i] = (RefBindDesc){ offset, slotType, nameHandle };
			}

			EloxError tableError = ELOX_ERROR_INITIALIZER;
			size_t savedStack = saveStack(fiber);
			tableSet(runCtx, &intf->methods, methodName, OBJ_VAL(method), &tableError);
			if (ELOX_UNLIKELY(tableError.raised)) {
				tableError.discardException(fiber, savedStack);
				ELOX_RAISE_GOTO(error, OOM(runCtx), cleanup);
			}

			break;
		}
		default:
			ELOX_UNREACHABLE();
			assert(false);
	}

cleanup:
	freeMethodCompiler(runCtx->vmCtx, &methodCompiler);
	releaseTemps(&temps);

	return NULL;
}

OpenKlass *newOpenKlass(RunCtx *runCtx, ObjKlass *klass) {
	OpenKlass *ok = ALLOCATE(runCtx, OpenKlass, 1);
	if (ELOX_UNLIKELY(ok == NULL))
		return NULL;

	ok->runCtx = runCtx;
	ok->klass = klass;

	ok->numRefs = 0;
	initTable(&ok->pendingThis);
	initTable(&ok->pendingSuper);

	ok->cCtx.compilerHandle = NULL;
	ok->error = NULL;

	return ok;
}

static void cloneDefault(RunCtx *runCtx, ObjString *methodName, ObjMethod *pendingMethod,
						 ObjClass *parentClass, EloxError *error) {
	ObjFiber *fiber = runCtx->activeFiber;

	if (ELOX_UNLIKELY(pendingMethod->isConflicted))
		ELOX_RAISE_RET(error, RTERR(runCtx, "Ambiguous default method %.*s",
									methodName->string.length, methodName->string.chars));

	ObjDefaultMethod *defaultMethod = pendingMethod->pending.fromDefault;

	ObjFunction *defaultFunction = defaultMethod->function;
	Chunk *defaultChunk = &defaultFunction->chunk;
	ObjString *fileName = defaultChunk->fileName;
	ObjClass *super = parentClass->super;

	ObjFunction *function = newFunction(runCtx, fileName);
	ELOX_CHECK_RAISE_RET(function != NULL, error, OOM(runCtx));

	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);

	PUSH_TEMP(temps, protectedFunction, OBJ_VAL(function));

	function->name = defaultFunction->name;
	function->parentClass = parentClass;

	Chunk *chunk = &function->chunk;
	if (defaultFunction->arity > 0) {
		function->defaultArgs = ALLOCATE(runCtx, Value, defaultFunction->arity);
		ELOX_CHECK_RAISE_GOTO(function->defaultArgs != NULL, error, OOM(runCtx), cleanup);
		memcpy(function->defaultArgs, defaultFunction->defaultArgs,
			   defaultFunction->arity * sizeof(Value));
		function->arity = defaultFunction->arity;
	}
	function->maxArgs = defaultFunction->maxArgs;
	function->isMethod = true;

	chunk->code = ALLOCATE(runCtx, uint8_t, defaultChunk->count);
	ELOX_CHECK_RAISE_GOTO(chunk->code != NULL, error, OOM(runCtx), cleanup);
	memcpy(chunk->code, defaultChunk->code, defaultChunk->count);
	chunk->count = chunk->capacity = defaultChunk->count;

	bool cloned = cloneValueArray(runCtx, &chunk->constants, &defaultChunk->constants);
	ELOX_CHECK_RAISE_GOTO(cloned, error, OOM(runCtx), cleanup);

	if (defaultChunk->lines != NULL) {
		chunk->lines = ALLOCATE(runCtx, LineStart, defaultChunk->lineCount);
		ELOX_CHECK_RAISE_GOTO(chunk->lines != NULL, error, OOM(runCtx), cleanup);
		memcpy(chunk->lines, defaultChunk->lines, defaultChunk->lineCount * sizeof(LineStart));
		chunk->lineCount = chunk->lineCapacity = defaultChunk->lineCount;
	}

	function->refOffset = super->numRefs;

	OpenKlass *openKlass = parentClass->openKlass;

	for (int i = 0; i < defaultMethod->numRefs; i++) {
		RefBindDesc *ref = &defaultMethod->refs[i];

		ObjString *refName = AS_STRING(function->chunk.constants.values[ref->nameHandle]);

		bool isSuper = ref->slotType & 0x1;
		uint8_t propType = (ref->slotType & 0x6) >> 1;

		Table *table = isSuper ? &openKlass->pendingSuper : &openKlass->pendingThis;
		int slot = openKlass->numRefs;
		uint64_t actualSlot = AS_NUMBER(tableSetIfMissing(runCtx, table, refName,
														  NUMBER_VAL(slot | propType << 24), error));
		if (ELOX_UNLIKELY(error->raised))
			goto cleanup;
		actualSlot &= 0xFFFFFF;
		if (actualSlot + 1 > openKlass->numRefs)
			openKlass->numRefs = actualSlot + 1;

		chunkPatchUShort(chunk, ref->offset, actualSlot);
	}

	// Convert pending method to actual method
	setObjType(&pendingMethod->obj, OBJ_METHOD);
	pendingMethod->method.klass = (ObjKlass *)parentClass;
	pendingMethod->method.callable = (Obj *)function;
	pendingMethod->method.fromDefault = defaultMethod;
	pendingMethod->isConflicted = false;

cleanup:
	releaseTemps(&temps);
}

void closeOpenKlass(RunCtx *runCtx, ObjKlass *klass, EloxError *error) {
	OpenKlass *ok = klass->openKlass;

	if (ok == NULL)
		return;

	if (getObjType(&klass->obj) == OBJ_CLASS) {
		ObjClass *clazz = (ObjClass *)klass;
		ObjClass *super = clazz->super;

		PropTable *props = &clazz->props;
		Value *classData = clazz->classData.values;
		for (int i = 0; i < props->capacity; i++) {
			PropEntry *entry = &props->entries[i];
			if ((entry->key != NULL) && (entry->value.type == ELOX_PROP_METHOD)) {
				Obj *obj = AS_OBJ(classData[entry->value.index]);
				ObjMethod *method = (ObjMethod *)obj;
				ObjType objType = getObjType(obj);
				if (objType == OBJ_PENDING_METHOD) {
					cloneDefault(runCtx, entry->key, method, clazz, error);
					if (ELOX_UNLIKELY(error->raised))
						goto cleanup;
				} else if ((objType == OBJ_METHOD) && (method->isConflicted)) {
					ELOX_RAISE_GOTO(error, RTERR(runCtx, "Ambiguous default method %.*s",
												 entry->key->string.length, entry->key->string.chars),
									cleanup);
				}
			}
		}

		uint16_t superNumRefs = super == NULL ? 0 : super->numRefs;
		uint16_t totalNumRefs = superNumRefs + ok->numRefs;
		if (totalNumRefs > 0) {
			clazz->refs = ALLOCATE(runCtx, Ref, totalNumRefs);
			ELOX_CHECK_RAISE_GOTO(clazz->refs != NULL, error, OOM(runCtx), cleanup);
			clazz->numRefs = totalNumRefs;
		}
		if (superNumRefs > 0)
			memcpy(clazz->refs, super->refs, superNumRefs * sizeof(Ref));

		Table *pendingThis = &ok->pendingThis;
		Table *pendingSuper = &ok->pendingSuper;

		for (int i = 0; i < pendingThis->capacity; i++) {
			Entry *entry = &pendingThis->entries[i];
			if (entry->key != NULL) {
				bindRef(runCtx, clazz, false, entry->key, AS_NUMBER(entry->value), error);
				if (ELOX_UNLIKELY(error->raised))
					goto cleanup;
			}
		}
		for (int i = 0; i < pendingSuper->capacity; i++) {
			Entry *entry = &pendingSuper->entries[i];
			if (entry->key != NULL) {
				bindRef(runCtx, clazz, true, entry->key, AS_NUMBER(entry->value), error);
				if (ELOX_UNLIKELY(error->raised))
					goto cleanup;
			}
		}
	}

cleanup:
	freeOpenKlass(runCtx->vmCtx, ok);
	klass->openKlass = NULL;
}

void freeOpenKlass(VMCtx *vmCtx, OpenKlass *ok) {
	if (ok == NULL)
		return;

	freeTable(vmCtx, &ok->pendingThis);
	freeTable(vmCtx, &ok->pendingSuper);

	FREE(vmCtx, OpenKlass, ok);
}

void markKlassHandle(EloxHandle *handle) {
	EloxKlassHandle *hnd = (EloxKlassHandle *)handle;

	VMCtx *vmCtx = hnd->base.vmCtx;

	markObject(vmCtx, (Obj *)hnd->klass);
}

