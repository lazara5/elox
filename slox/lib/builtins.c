#include <time.h>
#include <math.h>
#include <inttypes.h>
#include <stdio.h>

#include "slox/builtins.h"

static Value clockNative(VMCtx *vmCtx SLOX_UNUSED,
						int argCount SLOX_UNUSED, Value *args SLOX_UNUSED) {
	return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static Value printNative(VMCtx *vmCtx SLOX_UNUSED, int argCount, Value *args) {
	for (int i = 0; i < argCount; i++) {
		printValue(args[i]);
		printf(" ");
	}
	printf("\n");
	return NIL_VAL;
}

static Value assertNative(VMCtx *vmCtx, int argCount, Value *args) {
	VM *vm = &vmCtx->vm;

	if (argCount > 0) {
		if (isFalsey(args[0])) {
			if (argCount < 2)
				return runtimeError(vmCtx, "Assertion failed");
			else {
				ExecContext execCtx = EXEC_CTX_INITIALIZER(vmCtx);
				Value strVal = toString(&execCtx, args[1]);
				if (SLOX_UNLIKELY(execCtx.error))
					return strVal;
				const char *str = AS_CSTRING(strVal);
				push(vmCtx, strVal);
				Value error = runtimeError(vmCtx, "Assertion failed: %s", str);
				Value exception = pop(vm);
				pop(vm);
				push(vmCtx, exception);
				return error;
			}
		}
	}
	return NIL_VAL;
}

//--- Object --------------------

static Value objectToString(VMCtx *vmCtx, int argCount SLOX_UNUSED, Value *args) {
	HeapCString ret;
	initHeapStringWithSize(vmCtx, &ret, 16);
	ObjInstance *inst = AS_INSTANCE(args[0]);
	addStringFmt(vmCtx, &ret, "%s@%u", inst->clazz->name->chars, inst->identityHash);
	return OBJ_VAL(takeString(vmCtx, ret.chars, ret.length, ret.capacity));
}

static Value objectHashCode(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	ObjInstance *inst = AS_INSTANCE(args[0]);
	return NUMBER_VAL(inst->identityHash);
}

//--- String --------------------

static Value stringToString(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	ObjString *inst = AS_STRING(args[0]);
	return OBJ_VAL(inst);
}

static Value stringHashCode(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	ObjString *inst = AS_STRING(args[0]);
	return NUMBER_VAL(inst->hash);
}

static Value stringLength(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	ObjString *inst = AS_STRING(args[0]);
	return NUMBER_VAL(inst->length);
}

//--- Number --------------------

static Value numberToString(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	double n = AS_NUMBER(args[0]);
	HeapCString ret;
	initHeapString(vmCtx, &ret);
	if (trunc(n) == n)
		addStringFmt(vmCtx, &ret, "%" PRId64, (int64_t)n);
	else
		addStringFmt(vmCtx, &ret, "%g", n);
	return OBJ_VAL(takeString(vmCtx, ret.chars, ret.length, ret.capacity));
}

//--- Exception -----------------

static Value exceptionInit(VMCtx *vmCtx, int argCount SLOX_UNUSED, Value *args) {
	VM *vm = &vmCtx->vm;
	ObjInstance *inst = AS_INSTANCE(args[0]);
	ObjString *msg = AS_STRING(args[1]);
	ObjString *msgName = copyString(vmCtx, STR_AND_LEN("message"));
	push(vmCtx, OBJ_VAL(msgName));
	setInstanceField(inst, msgName, OBJ_VAL(msg));
	pop(vm);
	return OBJ_VAL(inst);
}

//--- Array ---------------------

static Value arrayLength(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	ObjArray *inst = AS_ARRAY(args[0]);
	return NUMBER_VAL(inst->size);
}

static Value arrayIteratorFunc(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED,
							  Value *args SLOX_UNUSED, int numUpvalues SLOX_UNUSED,
							  Value *upvalues) {
	ObjArray *array = AS_ARRAY(upvalues[0]);
	int index = AS_NUMBER(upvalues[1]);
	if (!isValidArrayIndex(array, index))
		return NIL_VAL;
	upvalues[1] = NUMBER_VAL(index + 1);
	return arrayAt(array, index);
}

static Value arrayIterator(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	ObjArray *inst = AS_ARRAY(args[0]);
	ObjNativeClosure *iter = newNativeClosure(vmCtx, arrayIteratorFunc, 2);
	iter->upvalues[0] = OBJ_VAL(inst);
	iter->upvalues[1] = NUMBER_VAL(0);
	return OBJ_VAL(iter);
}

//--- Map -----------------------

static Value mapSize(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	ObjMap *inst = AS_MAP(args[0]);
	return NUMBER_VAL(inst->items.count);
}

static Value mapIteratorFunc(VMCtx *vmCtx, int argCount SLOX_UNUSED,
							 Value *args SLOX_UNUSED, int numUpvalues SLOX_UNUSED,
							 Value *upvalues) {
	VM *vm = &vmCtx->vm;
	ObjMap *map = AS_MAP(upvalues[0]);
	int index = AS_NUMBER(upvalues[1]);
	int modCount = AS_NUMBER(upvalues[2]);

	if (modCount != map->items.modCount)
		return runtimeError(vmCtx, "Map modified during iteration");

	ValueEntry *entry;
	int nextIndex = valueTableGetNext(&map->items, index, &entry);
	if (nextIndex < 0)
		return NIL_VAL;

	upvalues[1] = NUMBER_VAL(nextIndex);

	ObjArray *ret = newArray(vmCtx, 2, OBJ_TUPLE);
	push(vmCtx, OBJ_VAL(ret));
	appendToArray(vmCtx, ret, entry->key);
	appendToArray(vmCtx, ret, entry->value);
	pop(vm);
	return OBJ_VAL(ret);
}

static Value mapIterator(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	ObjMap *inst = AS_MAP(args[0]);
	ObjNativeClosure *iter = newNativeClosure(vmCtx, mapIteratorFunc, 3);
	iter->upvalues[0] = OBJ_VAL(inst);
	iter->upvalues[1] = NUMBER_VAL(0);
	iter->upvalues[2] = NUMBER_VAL(inst->items.modCount);
	return OBJ_VAL(iter);
}

static ObjClass *defineStaticClass(VMCtx *vmCtx, const char *name, ObjClass *super) {
	VM *vm = &vmCtx->vm;
	ObjString *className = copyString(vmCtx, name, strlen(name));
	push(vmCtx, OBJ_VAL(className));
	ObjClass *clazz = newClass(vmCtx, className);
	push(vmCtx, OBJ_VAL(clazz));
	tableSet(vmCtx, &vm->globals, className, OBJ_VAL(clazz));
	popn(vm, 2);
	if (super != NULL) {
		clazz->super = OBJ_VAL(super);
		for (int i = 0; i < super->fields.capacity; i++) {
			Entry *entry = &super->fields.entries[i];
			if (entry->key != NULL)
				tableSet(vmCtx, &clazz->fields, entry->key, entry->value);
		}
		tableAddAll(vmCtx, &super->methods, &clazz->methods);
		clazz->initializer = super->initializer;
	}
	return clazz;
}

void registerBuiltins(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	clearBuiltins(vm);

	vm->iteratorString = copyString(vmCtx, STR_AND_LEN("iterator"));
	vm->hashCodeString = copyString(vmCtx, STR_AND_LEN("hashCode"));
	vm->equalsString = copyString(vmCtx, STR_AND_LEN("equals"));
	vm->toStringString = copyString(vmCtx, STR_AND_LEN("toString"));

	ObjClass *objectClass = defineStaticClass(vmCtx, "Object", NULL);
	addNativeMethod(vmCtx, objectClass, "toString", objectToString);
	addNativeMethod(vmCtx, objectClass, "hashCode", objectHashCode);

	ObjClass *stringClass = defineStaticClass(vmCtx, "String", objectClass);
	addNativeMethod(vmCtx, stringClass, "toString", stringToString);
	addNativeMethod(vmCtx, stringClass, "hashCode", stringHashCode);
	addNativeMethod(vmCtx, stringClass, "length", stringLength);
	vm->stringClass = stringClass;

	ObjClass *numberClass = defineStaticClass(vmCtx, "Number", objectClass);
	addNativeMethod(vmCtx, numberClass, "toString", numberToString);
	vm->numberClass = numberClass;

	ObjClass *exceptionClass = defineStaticClass(vmCtx, "Exception", objectClass);
	addNativeMethod(vmCtx, exceptionClass, "Exception", exceptionInit);
	addClassField(vmCtx, exceptionClass, "message");
	addClassField(vmCtx, exceptionClass, "stacktrace");
	vm->exceptionClass = exceptionClass;

	ObjClass *runtimeExceptionClass = defineStaticClass(vmCtx, "RuntimeException", exceptionClass);
	vm->runtimeExceptionClass = runtimeExceptionClass;

	ObjClass *arrayClass = defineStaticClass(vmCtx, "Array", objectClass);
	addNativeMethod(vmCtx, arrayClass, "length", arrayLength);
	addNativeMethod(vmCtx, arrayClass, "iterator", arrayIterator);
	vm->arrayClass = arrayClass;

	ObjClass *mapClass = defineStaticClass(vmCtx, "Map", objectClass);
	addNativeMethod(vmCtx, mapClass, "size", mapSize);
	addNativeMethod(vmCtx, mapClass, "iterator", mapIterator);
	vm->mapClass = mapClass;

	defineNative(vmCtx, "print", printNative);
	defineNative(vmCtx, "assert", assertNative);
	defineNative(vmCtx, "clock", clockNative);
}

void markBuiltins(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	markObject(vmCtx, (Obj *)vm->iteratorString);
	markObject(vmCtx, (Obj *)vm->hashCodeString);
	markObject(vmCtx, (Obj *)vm->equalsString);
	markObject(vmCtx, (Obj *)vm->toStringString);

	markObject(vmCtx, (Obj *)vm->stringClass);
	markObject(vmCtx, (Obj *)vm->numberClass);
	markObject(vmCtx, (Obj *)vm->exceptionClass);
	markObject(vmCtx, (Obj *)vm->runtimeExceptionClass);
	markObject(vmCtx, (Obj *)vm->arrayClass);
	markObject(vmCtx, (Obj *)vm->mapClass);
}

void clearBuiltins(VM *vm) {
	vm->iteratorString = NULL;
	vm->hashCodeString = NULL;
	vm->equalsString = NULL;
	vm->toStringString = NULL;

	vm->stringClass = NULL;
	vm->numberClass = NULL;
	vm->exceptionClass = NULL;
	vm->runtimeExceptionClass = NULL;
	vm->arrayClass = NULL;
	vm->mapClass = NULL;
}
