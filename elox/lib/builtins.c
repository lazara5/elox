#include <time.h>
#include <math.h>
#include <inttypes.h>
#include <stdio.h>

#include "elox/builtins.h"

static Value clockNative(VMCtx *vmCtx ELOX_UNUSED,
						int argCount ELOX_UNUSED, Args *args ELOX_UNUSED) {
	return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static Value printNative(VMCtx *vmCtx ELOX_UNUSED, int argCount, Args *args) {
	for (int i = 0; i < argCount; i++) {
		printValue(getValueArg(args, i));
		printf(" ");
	}
	printf("\n");
	return NIL_VAL;
}

static Value assertNative(VMCtx *vmCtx, int argCount, Args *args) {
	VM *vm = &vmCtx->vm;

	if (argCount > 0) {
		if (isFalsey(getValueArg(args, 0))) {
			if (argCount < 2)
				return runtimeError(vmCtx, "Assertion failed");
			else {
				ExecContext execCtx = EXEC_CTX_INITIALIZER(vmCtx);
				Value strVal = toString(&execCtx, getValueArg(args, 1));
				if (ELOX_UNLIKELY(execCtx.error))
					return strVal;
				const char *str = AS_CSTRING(strVal);
				push(vm, strVal);
				Value error = runtimeError(vmCtx, "Assertion failed: %s", str);
				Value exception = pop(vm);
				pop(vm);
				push(vm, exception);
				return error;
			}
		}
	}
	return NIL_VAL;
}

//--- Object --------------------

static Value objectToString(VMCtx *vmCtx, int argCount ELOX_UNUSED, Args *args) {
	HeapCString ret;
	initHeapStringWithSize(vmCtx, &ret, 16);
	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	addHeapStringFmt(vmCtx, &ret, "%s@%u", inst->clazz->name->string.chars, inst->identityHash);
	return OBJ_VAL(takeString(vmCtx, ret.chars, ret.length, ret.capacity));
}

static Value objectHashCode(VMCtx *vmCtx ELOX_UNUSED, int argCount ELOX_UNUSED, Args *args) {
	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	return NUMBER_VAL(inst->identityHash);
}

//--- String --------------------

static Value stringToString(VMCtx *vmCtx ELOX_UNUSED, int argCount ELOX_UNUSED, Args *args) {
	ObjString *inst = AS_STRING(getValueArg(args, 0));
	return OBJ_VAL(inst);
}

static Value stringHashCode(VMCtx *vmCtx ELOX_UNUSED, int argCount ELOX_UNUSED, Args *args) {
	ObjString *inst = AS_STRING(getValueArg(args, 0));
	return NUMBER_VAL(inst->hash);
}

static Value stringLength(VMCtx *vmCtx ELOX_UNUSED, int argCount ELOX_UNUSED, Args *args) {
	ObjString *inst = AS_STRING(getValueArg(args, 0));
	return NUMBER_VAL(inst->string.length);
}

//--- Number --------------------

static Value numberToString(VMCtx *vmCtx ELOX_UNUSED, int argCount ELOX_UNUSED, Args *args) {
	double n = AS_NUMBER(getValueArg(args, 0));
	HeapCString ret;
	initHeapString(vmCtx, &ret);
	if (trunc(n) == n)
		addHeapStringFmt(vmCtx, &ret, "%" PRId64, (int64_t)n);
	else
		addHeapStringFmt(vmCtx, &ret, "%g", n);
	return OBJ_VAL(takeString(vmCtx, ret.chars, ret.length, ret.capacity));
}

//--- Exception -----------------

static Value exceptionInit(VMCtx *vmCtx, int argCount ELOX_UNUSED, Args *args) {
	VM *vm = &vmCtx->vm;
	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjString *msg = AS_STRING(getValueArg(args, 1));
	ObjString *msgName = copyString(vmCtx, ELOX_STR_AND_LEN("message"));
	push(vm, OBJ_VAL(msgName));
	setInstanceField(inst, msgName, OBJ_VAL(msg));
	pop(vm);
	return OBJ_VAL(inst);
}

//--- Array ---------------------

static Value arrayLength(VMCtx *vmCtx ELOX_UNUSED, int argCount ELOX_UNUSED, Args *args) {
	ObjArray *inst = AS_ARRAY(getValueArg(args, 0));
	return NUMBER_VAL(inst->size);
}

static Value arrayIteratorFunc(VMCtx *vmCtx ELOX_UNUSED, int argCount ELOX_UNUSED,
							  Args *args ELOX_UNUSED, int numUpvalues ELOX_UNUSED,
							  Value *upvalues) {
	ObjArray *array = AS_ARRAY(upvalues[0]);
	int index = AS_NUMBER(upvalues[1]);
	if (!isValidArrayIndex(array, index))
		return NIL_VAL;
	upvalues[1] = NUMBER_VAL(index + 1);
	return arrayAt(array, index);
}

static Value arrayIterator(VMCtx *vmCtx ELOX_UNUSED, int argCount ELOX_UNUSED, Args *args) {
	ObjArray *inst = AS_ARRAY(getValueArg(args, 0));
	ObjNativeClosure *iter = newNativeClosure(vmCtx, arrayIteratorFunc, 2);
	iter->upvalues[0] = OBJ_VAL(inst);
	iter->upvalues[1] = NUMBER_VAL(0);
	return OBJ_VAL(iter);
}

//--- Map -----------------------

static Value mapSize(VMCtx *vmCtx ELOX_UNUSED, int argCount ELOX_UNUSED, Args *args) {
	ObjMap *inst = AS_MAP(getValueArg(args, 0));
	return NUMBER_VAL(inst->items.count);
}

static Value mapIteratorFunc(VMCtx *vmCtx, int argCount ELOX_UNUSED,
							 Args *args ELOX_UNUSED, int numUpvalues ELOX_UNUSED,
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
	push(vm, OBJ_VAL(ret));
	appendToArray(vmCtx, ret, entry->key);
	appendToArray(vmCtx, ret, entry->value);
	pop(vm);
	return OBJ_VAL(ret);
}

static Value mapIterator(VMCtx *vmCtx ELOX_UNUSED, int argCount ELOX_UNUSED, Args *args) {
	ObjMap *inst = AS_MAP(getValueArg(args, 0));
	ObjNativeClosure *iter = newNativeClosure(vmCtx, mapIteratorFunc, 3);
	iter->upvalues[0] = OBJ_VAL(inst);
	iter->upvalues[1] = NUMBER_VAL(0);
	iter->upvalues[2] = NUMBER_VAL(inst->items.modCount);
	return OBJ_VAL(iter);
}

static ObjClass *registerStaticClass(VMCtx *vmCtx, const String *name, const String *moduleName,
									 ObjClass *super) {
	VM *vm = &vmCtx->vm;
	ObjString *className = copyString(vmCtx, name->chars, name->length);
	push(vm, OBJ_VAL(className));
	ObjClass *clazz = newClass(vmCtx, className);
	push(vm, OBJ_VAL(clazz));
	uint16_t globalIdx = globalIdentifierConstant(vmCtx, name, moduleName);
	vm->globalValues.values[globalIdx] = peek(vm, 0);
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

	if (stringEquals(moduleName, &eloxBuiltinModule)) {
		// already interned and referenced in global table
		ObjString *nameStr = copyString(vmCtx, name->chars, name->length);
		tableSet(vmCtx, &vm->builtinSymbols, nameStr, OBJ_VAL(clazz));
	}

	return clazz;
}

void registerBuiltins(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	clearBuiltins(vm);

	vm->iteratorString = copyString(vmCtx, ELOX_STR_AND_LEN("iterator"));
	vm->hashCodeString = copyString(vmCtx, ELOX_STR_AND_LEN("hashCode"));
	vm->equalsString = copyString(vmCtx, ELOX_STR_AND_LEN("equals"));
	vm->toStringString = copyString(vmCtx, ELOX_STR_AND_LEN("toString"));

	const String objectName = STRING_INITIALIZER("Object");
	ObjClass *objectClass = registerStaticClass(vmCtx, &objectName, &eloxBuiltinModule, NULL);
	DBG_PRINT_STACK("DBG1", vm);
	addNativeMethod(vmCtx, objectClass, "toString", objectToString);
	DBG_PRINT_STACK("DBG2", vm);
	addNativeMethod(vmCtx, objectClass, "hashCode", objectHashCode);
	DBG_PRINT_STACK("DBG3", vm);

	const String stringName = STRING_INITIALIZER("String");
	ObjClass *stringClass = registerStaticClass(vmCtx, &stringName, &eloxBuiltinModule, objectClass);
	addNativeMethod(vmCtx, stringClass, "toString", stringToString);
	addNativeMethod(vmCtx, stringClass, "hashCode", stringHashCode);
	addNativeMethod(vmCtx, stringClass, "length", stringLength);
	addNativeMethod(vmCtx, stringClass, "fmt", stringFmt);
	vm->stringClass = stringClass;

	const String numberName = STRING_INITIALIZER("Number");
	ObjClass *numberClass = registerStaticClass(vmCtx, &numberName, &eloxBuiltinModule, objectClass);
	addNativeMethod(vmCtx, numberClass, "toString", numberToString);
	vm->numberClass = numberClass;

	const String exceptionName = STRING_INITIALIZER("Exception");
	ObjClass *exceptionClass = registerStaticClass(vmCtx, &exceptionName, &eloxBuiltinModule, objectClass);
	addNativeMethod(vmCtx, exceptionClass, "Exception", exceptionInit);
	addClassField(vmCtx, exceptionClass, "message");
	addClassField(vmCtx, exceptionClass, "stacktrace");
	vm->exceptionClass = exceptionClass;

	const String runtimeExceptionName = STRING_INITIALIZER("RuntimeException");
	ObjClass *runtimeExceptionClass = registerStaticClass(vmCtx, &runtimeExceptionName, &eloxBuiltinModule, exceptionClass);
	vm->runtimeExceptionClass = runtimeExceptionClass;

	const String arrayName = STRING_INITIALIZER("Array");
	ObjClass *arrayClass = registerStaticClass(vmCtx, &arrayName, &eloxBuiltinModule, objectClass);
	addNativeMethod(vmCtx, arrayClass, "length", arrayLength);
	addNativeMethod(vmCtx, arrayClass, "iterator", arrayIterator);
	vm->arrayClass = arrayClass;

	const String mapName = STRING_INITIALIZER("Map");
	ObjClass *mapClass = registerStaticClass(vmCtx, &mapName, &eloxBuiltinModule, objectClass);
	addNativeMethod(vmCtx, mapClass, "size", mapSize);
	addNativeMethod(vmCtx, mapClass, "iterator", mapIterator);
	vm->mapClass = mapClass;

	const String printName = STRING_INITIALIZER("print");
	registerNativeFunction(vmCtx, &printName, &eloxBuiltinModule, printNative);

	const String assertName = STRING_INITIALIZER("assert");
	registerNativeFunction(vmCtx, &assertName, &eloxBuiltinModule, assertNative);

	const String clockName = STRING_INITIALIZER("clock");
	registerNativeFunction(vmCtx, &clockName, &eloxBuiltinModule, clockNative);
}

void markBuiltins(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	markTable(vmCtx, &vm->builtinSymbols);

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
