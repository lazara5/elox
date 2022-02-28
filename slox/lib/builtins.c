#include <time.h>

#include "slox/builtins.h"

static Value clockNative(VMCtx *vmCtx SLOX_UNUSED,
						 int argCount SLOX_UNUSED, Value *args SLOX_UNUSED) {
	return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

//--- Object --------------------

static Value objectToString(VMCtx *vmCtx, int argCount SLOX_UNUSED, Value *args) {
	HeapCString ret;
	initHeapStringWithSize(vmCtx, &ret, 16);
	ObjInstance *inst = AS_INSTANCE(args[0]);
	addStringFmt(vmCtx, &ret, "%s@%u", inst->clazz->name->chars, inst->identityHash);
	return(OBJ_VAL(takeString(vmCtx, ret.chars, ret.length, ret.capacity)));
}

static Value objectHashCode(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	ObjInstance *inst = AS_INSTANCE(args[0]);
	return(NUMBER_VAL(inst->identityHash));
}

//--- String --------------------

static Value stringToString(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	ObjString *inst = AS_STRING(args[0]);
	return OBJ_VAL(inst);
}

static Value stringHashCode(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	ObjString *inst = AS_STRING(args[0]);
	return(NUMBER_VAL(inst->hash));
}

static Value stringLength(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	ObjString *inst = AS_STRING(args[0]);
	return NUMBER_VAL(inst->length);
}

//--- Array ---------------------

static Value arrayLength(VMCtx *vmCtx SLOX_UNUSED, int argCount SLOX_UNUSED, Value *args) {
	ObjArray *inst = AS_ARRAY(args[0]);
	return(NUMBER_VAL(inst->size));
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

static ObjClass *defineStaticClass(VMCtx *vmCtx, const char *name, ObjClass *super) {
	VM *vm = &vmCtx->vm;
	ObjString *className = copyString(vmCtx, name, strlen(name));
	push(vm, OBJ_VAL(className));
	ObjClass *clazz = newClass(vmCtx, className);
	push(vm, OBJ_VAL(clazz));
	tableSet(vmCtx, &vm->globals, className, OBJ_VAL(clazz));
	pop(vm);
	pop(vm);
	if (super != NULL) {
		tableAddAll(vmCtx, &super->methods, &clazz->methods);
	}
	return clazz;
}

void registerBuiltins(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	clearBuiltins(vm);

	vm->initString = copyString(vmCtx, STR_AND_LEN("init"));
	vm->iteratorString = copyString(vmCtx, STR_AND_LEN("iterator"));

	ObjClass *objectClass = defineStaticClass(vmCtx, "Object", NULL);
	addNativeMethod(vmCtx, objectClass, "toString", objectToString);
	addNativeMethod(vmCtx, objectClass, "hashCode", objectHashCode);

	ObjClass *stringClass = defineStaticClass(vmCtx, "String", objectClass);
	addNativeMethod(vmCtx, stringClass, "toString", stringToString);
	addNativeMethod(vmCtx, stringClass, "hashCode", stringHashCode);
	addNativeMethod(vmCtx, stringClass, "length", stringLength);
	vm->stringClass = stringClass;

	ObjClass *arrayClass = defineStaticClass(vmCtx, "Array", objectClass);
	addNativeMethod(vmCtx, arrayClass, "length", arrayLength);
	addNativeMethod(vmCtx, arrayClass, "iterator", arrayIterator);
	vm->arrayClass = arrayClass;

	defineNative(vmCtx, "clock", clockNative);
}

void markBuiltins(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	markObject(vmCtx, (Obj *)vm->initString);
	markObject(vmCtx, (Obj *)vm->iteratorString);
	markObject(vmCtx, (Obj *)vm->stringClass);
	markObject(vmCtx, (Obj *)vm->arrayClass);
}

void clearBuiltins(VM *vm) {
	vm->initString = NULL;
	vm->iteratorString = NULL;
	vm->stringClass = NULL;
	vm->arrayClass = NULL;
}
