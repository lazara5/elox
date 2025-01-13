// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <stdlib.h>
#include <stdio.h>

#include "elox/compiler.h"
#include "elox/memory.h"
#include "elox/state.h"

#ifdef ELOX_DEBUG_LOG_GC
#include <elox/debug.h>
#endif

#define GC_HEAP_GROW_FACTOR 2

void *reallocate(RunCtx *runCtx, void *pointer, size_t oldSize, size_t newSize) {
	VM *vm = runCtx->vm;
	VMEnv *env = runCtx->vmEnv;

	vm->bytesAllocated += newSize - oldSize;
	if (newSize > oldSize) {
#ifdef ELOX_DEBUG_STRESS_GC
		collectGarbage(runCtx);
#else
		if (vm->bytesAllocated > vm->nextGC)
			collectGarbage(runCtx);
#endif
	}

	if (newSize == 0) {
		env->free(pointer, env->allocatorUserData);
		return NULL;
	}

	void *result = env->realloc(pointer, newSize, env->allocatorUserData);
	return result;
}

void markObject(RunCtx *runCtx, Obj *object) {
	if (object == NULL)
		return;
	if (object->markers & MARKER_BLACK)
		return;

	VM *vm = runCtx->vm;

#ifdef ELOX_DEBUG_LOG_GC
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%p mark ", (void *)object);
	printValue(vmCtx, ELOX_IO_DEBUG, OBJ_VAL(object));
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "\n");
#endif

	object->markers |= (MARKER_BLACK | MARKER_GRAY);

#ifdef ELOX_DEBUG_FORCE_SLOW_GC
	vm->grayOverflow = true;
#endif

	if (ELOX_UNLIKELY(vm->grayOverflow))
		return;

	VMEnv *env = runCtx->vmEnv;

	if (vm->grayCapacity < vm->grayCount + 1) {
		int newGrayCapacity = GROW_CAPACITY(vm->grayCapacity);
		Obj **oldStack = vm->grayStack;
		vm->grayStack = (Obj **)env->realloc(vm->grayStack,
											 sizeof(Obj *) * newGrayCapacity,
											 env->allocatorUserData);
		if (ELOX_UNLIKELY(vm->grayStack == NULL)) {
			vm->grayStack = oldStack;
			vm->grayOverflow = true;
			return;
		}
		vm->grayCapacity = newGrayCapacity;
	}

	vm->grayStack[vm->grayCount++] = object;
}

void markValue(RunCtx *runCtx, Value value) {
	if (IS_OBJ(value))
		markObject(runCtx, AS_OBJ(value));
}

static void markArray(RunCtx *runCtx, ValueArray *array) {
	for (uint32_t i = 0; i < array->count; i++)
		markValue(runCtx, array->values[i]);
}

static void blackenObject(RunCtx *runCtx, Obj *object) {
#ifdef ELOX_DEBUG_LOG_GC
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%p blacken ", (void *)object);
	printValue(vmCtx, ELOX_IO_DEBUG, OBJ_VAL(object));
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "\n");
#endif
	switch (object->type) {
		case OBJ_HASHMAP: {
			ObjHashMap *map = (ObjHashMap *)object;
			markValueTable(runCtx, &map->items);
			break;
		}
		case OBJ_TUPLE:
		case OBJ_ARRAY: {
			ObjArray *array = (ObjArray *)object;
			for (int i = 0; i < array->size; i++)
				markValue(runCtx, array->items[i]);
			break;
		}
		case OBJ_BOUND_METHOD: {
			ObjBoundMethod *bound = (ObjBoundMethod *)object;
			markValue(runCtx, bound->receiver);
			markObject(runCtx, bound->method);
			break;
		}
		case OBJ_METHOD: {
			ObjMethod *method = (ObjMethod *)object;
			markObject(runCtx, (Obj *)method->clazz);
			markObject(runCtx, method->callable);
			break;
		}
		case OBJ_METHOD_DESC:
			break;
		case OBJ_INTERFACE: {
			ObjKlass *klass = (ObjKlass *)object;
			markObject(runCtx, (Obj *)klass->name);
			ObjInterface *intf = (ObjInterface *)object;
			markTable(runCtx, &intf->methods);
			break;
		}
		case OBJ_CLASS: {
			ObjKlass *klass = (ObjKlass *)object;
			markObject(runCtx, (Obj *)klass->name);
			ObjClass *clazz = (ObjClass *)object;
			markTable(runCtx, &clazz->fields);
			markTable(runCtx, &clazz->methods);
			markTable(runCtx, &clazz->statics);
			markArray(runCtx, &clazz->staticValues);
			markValue(runCtx, clazz->initializer);
			break;
		}
		case OBJ_CLOSURE: {
			ObjClosure *closure = (ObjClosure *)object;
			markObject(runCtx, (Obj *)closure->function);
			for (int i = 0; i < closure->upvalueCount; i++)
				markObject(runCtx, (Obj *)closure->upvalues[i]);
			break;
		}
		case OBJ_NATIVE_CLOSURE: {
			ObjNativeClosure *closure = (ObjNativeClosure *)object;
			for (int i = 0; i < closure->upvalueCount; i++)
				markValue(runCtx, closure->upvalues[i]);
			break;
		}
		case OBJ_FUNCTION: {
			ObjFunction *function = (ObjFunction *)object;
#ifdef ELOX_DEBUG_LOG_GC
	if (function->name) {
		eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%p [marking function %.*s]\n",
					(void *)object, function->name->string.length, function->name->string.chars);
	} else
		eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%p [marking function]\n", (void *)object);
#endif
			markObject(runCtx, (Obj *)function->name);
			markObject(runCtx, (Obj *)function->parentClass);
#ifdef ELOX_DEBUG_LOG_GC
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%p [marking %d constants]\n",
			   (void *)object, function->chunk.constants.count);
#endif
			markArray(runCtx, &function->chunk.constants);
			markObject(runCtx, (Obj *)function->chunk.fileName);
			if (function->defaultArgs != NULL) {
				for (int i = 0; i < function->arity; i++)
					markValue(runCtx, function->defaultArgs[i]);
			}
			break;
		}
		case OBJ_INSTANCE: {
			ObjInstance *instance = (ObjInstance *)object;
			markObject(runCtx, (Obj *)instance->clazz);
			markArray(runCtx, &instance->fields);
			break;
		}
		case OBJ_UPVALUE:
			markValue(runCtx, ((ObjUpvalue *)object)->closed);
			break;
		case OBJ_STRINGPAIR:
			markObject(runCtx, (Obj *)((ObjStringPair *)object)->str1);
			markObject(runCtx, (Obj *)((ObjStringPair *)object)->str2);
			break;
		case OBJ_NATIVE: {
			ObjNative *native = (ObjNative *)object;
			if (native->defaultArgs != NULL) {
				for (int i = 0; i < native->arity; i++)
					markValue(runCtx, native->defaultArgs[i]);
			}
			break;
		}
		case OBJ_STRING:
			break;
	}
}

static void freeObject(RunCtx *runCtx, Obj *object) {
//#if defined(ELOX_DEBUG_LOG_GC) || defined(ELOX_DEBUG_TRACE_EXECUTION)
#ifdef ELOX_DEBUG_LOG_GC
	//printf("%p free type %d (", (void *)object, object->type);
	//printObject(object);
	//printf(")\n");
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%p free type %d\n (", (void *)object, object->type);
#endif

	switch (object->type) {
		case OBJ_HASHMAP: {
			ObjHashMap *map = (ObjHashMap *)object;
			freeValueTable(runCtx, &map->items);
			FREE(runCtx, ObjHashMap, object);
			break;
		}
		case OBJ_TUPLE:
		case OBJ_ARRAY: {
			ObjArray *array = (ObjArray *)object;
			FREE_ARRAY(runCtx, Value *, array->items, array->size);
			FREE(runCtx, ObjArray, object);
			break;
		}
		case OBJ_BOUND_METHOD:
			FREE(runCtx, ObjBoundMethod, object);
			break;
		case OBJ_METHOD:
			FREE(runCtx, ObjMethod, object);
			break;
		case OBJ_METHOD_DESC:
			FREE(runCtx, ObjMethodDesc, object);
			break;
		case OBJ_INTERFACE: {
			ObjInterface *intf = (ObjInterface *)object;
			freeTable(runCtx, &intf->methods);
			FREE(runCtx, ObjInterface, object);
			break;
		}
		case OBJ_CLASS: {
			ObjClass *clazz = (ObjClass *)object;
			freeTable(runCtx, &clazz->fields);
			freeTable(runCtx, &clazz->methods);
			freeTable(runCtx, &clazz->statics);
			freeValueArray(runCtx, &clazz->staticValues);
			FREE_ARRAY(runCtx, MemberRef, clazz->memberRefs, clazz->memberRefCount);
			if (clazz->typeInfo.rssList != NULL)
				FREE_ARRAY(runCtx, Obj *, clazz->typeInfo.rssList, clazz->typeInfo.numRss);
			FREE(runCtx, ObjClass, object);
			break;
		}
		case OBJ_CLOSURE: {
			ObjClosure *closure = (ObjClosure *)object;
			FREE_ARRAY(runCtx, ObjUpvalue *, closure->upvalues, closure->upvalueCount);
			FREE(runCtx, ObjClosure, object);
			break;
		}
		case OBJ_NATIVE_CLOSURE: {
			ObjNativeClosure *closure = (ObjNativeClosure *)object;
			FREE_ARRAY(runCtx, Value, closure->upvalues, closure->upvalueCount);
			FREE(runCtx, ObjNativeClosure, object);
			break;
		}
		case OBJ_FUNCTION: {
			ObjFunction *function = (ObjFunction *)object;
			freeChunk(runCtx, &function->chunk);
			FREE_ARRAY(runCtx, Value, function->defaultArgs, function->arity);
			FREE(runCtx, ObjFunction, object);
			break;
		}
		case OBJ_INSTANCE: {
			ObjInstance *instance = (ObjInstance *)object;
			freeValueArray(runCtx, &instance->fields);
			FREE(runCtx, ObjInstance, object);
			break;
		}
		case OBJ_NATIVE: {
			ObjNative *native = (ObjNative *)object;
			FREE_ARRAY(runCtx, Value, native->defaultArgs, native->arity);
			FREE(runCtx, ObjNative, object);
			break;
		}
		case OBJ_STRING: {
			ObjString *string = (ObjString *)object;
			FREE_ARRAY(runCtx, char, ELOX_UNCONST(string->string.chars), string->string.length + 1);
			FREE(runCtx, ObjString, object);
			break;
		}
		case OBJ_STRINGPAIR:
			FREE(runCtx, ObjStringPair, object);
			break;
		case OBJ_UPVALUE:
			FREE(runCtx, ObjUpvalue, object);
			break;
	}
}

static void markRoots(RunCtx *runCtx) {
	VM *vm = runCtx->vm;

	markFiberCtx(runCtx, vm->initFiber);
	markValueTable(runCtx, &vm->globalNames);
	markArray(runCtx, &vm->globalValues);
	markCompilerRoots(runCtx);

	markHandleSet(&vm->handles);
}

static void slowTraceReferences(RunCtx *runCtx) {
	VM *vm = runCtx->vm;

	bool haveGray;
	do {
		haveGray = false;
		Obj *object = vm->mainHeap.objects;
		while (object != NULL) {
			if (object->markers & MARKER_GRAY) {
				haveGray = true;
				object->markers &= ~MARKER_GRAY;
				blackenObject(runCtx, object);
			}
			object = object->next;
		}
	} while (haveGray);
}

static void traceReferences(RunCtx *runCtx) {
	VM *vm = runCtx->vm;

	if (ELOX_UNLIKELY(vm->grayOverflow)) {
		slowTraceReferences(runCtx);
		goto cleanup;
	}
	while (vm->grayCount > 0) {
		Obj *object = vm->grayStack[--vm->grayCount];
		object->markers &= ~MARKER_GRAY;
		blackenObject(runCtx, object);
		if (ELOX_UNLIKELY(vm->grayOverflow)) {
			slowTraceReferences(runCtx);
			goto cleanup;
		}
	}

cleanup:
	vm->grayCount = 0;
	vm->grayOverflow = 0;
}

static void sweep(RunCtx *runCtx) {
	VM *vm = runCtx->vm;

	Obj *previous = NULL;
	Obj *object = vm->mainHeap.objects;
	while (object != NULL) {
		if (object->markers != 0) {
			object->markers &= ~(MARKER_BLACK | MARKER_GRAY);
			previous = object;
			object = object->next;
		} else {
			Obj *unreached = object;
			object = object->next;
			if (previous != NULL)
				previous->next = object;
			else
				vm->mainHeap.objects = object;

			freeObject(runCtx, unreached);
		}
	}
}

void collectGarbage(RunCtx *runCtx) {
	VM *vm = runCtx->vm;

#ifdef ELOX_DEBUG_LOG_GC
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "-- gc begin\n");
	size_t before = vm->bytesAllocated;
#endif

	markRoots(runCtx);
	traceReferences(runCtx);
	tableRemoveWhite(&vm->strings);
	sweep(runCtx);

	vm->nextGC = vm->bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef ELOX_DEBUG_LOG_GC
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "-- gc end\n");
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "   collected %zu bytes (from %zu to %zu) next at %zu\n",
			   before - vm->bytesAllocated, before, vm->bytesAllocated, vm->nextGC);
#endif
}

void freeObjects(RunCtx *runCtx) {
	VM *vm = runCtx->vm;
	VMEnv *env = runCtx->vmEnv;

	Obj *object = vm->mainHeap.objects;
	while (object != NULL) {
		Obj *next = object->next;
		freeObject(runCtx, object);
		object = next;
	}

	object = vm->permHeap.objects;
	while (object != NULL) {
		Obj *next = object->next;
		freeObject(runCtx, object);
		object = next;
	}

	env->free(vm->grayStack, env->allocatorUserData);
}
