// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <stdlib.h>
#include <stdio.h>

#include "elox/compiler.h"
#include "elox/memory.h"
#include "elox/state.h"
#include <elox/Class.h>

#ifdef ELOX_DEBUG_LOG_GC
#include <elox/debug.h>
#endif

#define GC_HEAP_GROW_FACTOR 2

void *vmRealloc(RunCtx *runCtx, void *pointer, size_t oldSize, size_t newSize) {
	VMCtx *vmCtx = runCtx->vmCtx;
	VM *vm = vmCtx->vm;
	VMEnv *env = vmCtx->vmEnv;

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

void vmFree(VMCtx *vmCtx, void *pointer, size_t oldSize) {
	VM *vm = vmCtx->vm;
	VMEnv *env = vmCtx->vmEnv;

	vm->bytesAllocated -= oldSize;
	env->free(pointer, env->allocatorUserData);
}

void markObject(VMCtx *vmCtx, Obj *object) {
	if (object == NULL)
		return;
	if (getObjMarkers(object) & MARKER_BLACK)
		return;

	VM *vm = vmCtx->vm;

#ifdef ELOX_DEBUG_LOG_GC
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%p mark ", (void *)object);
	printValue(runCtx, ELOX_IO_DEBUG, OBJ_VAL(object));
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "\n");
#endif

	setObjMarkers(object, (getObjMarkers(object) | (MARKER_BLACK | MARKER_GRAY)));

#ifdef ELOX_DEBUG_FORCE_SLOW_GC
	vm->grayOverflow = true;
#endif

	if (ELOX_UNLIKELY(vm->grayOverflow))
		return;

	VMEnv *env = vmCtx->vmEnv;

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

void markValue(VMCtx *vmCtx, Value value) {
	if (IS_OBJ(value))
		markObject(vmCtx, AS_OBJ(value));
}

static void markArray(VMCtx *vmCtx, ValueArray *array) {
	for (uint32_t i = 0; i < array->count; i++)
		markValue(vmCtx, array->values[i]);
}

static void blackenObject(VMCtx *vmCtx, Obj *object) {
#ifdef ELOX_DEBUG_LOG_GC
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%p blacken ", (void *)object);
	printValue(runCtx, ELOX_IO_DEBUG, OBJ_VAL(object));
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "\n");
#endif
	switch (getObjType(object)) {
		case OBJ_HASHMAP: {
			ObjHashMap *map = (ObjHashMap *)object;
			markValueTable(vmCtx, &map->items);
			break;
		}
		case OBJ_TUPLE:
		case OBJ_ARRAY: {
			ObjArray *array = (ObjArray *)object;
			for (int i = 0; i < array->size; i++)
				markValue(vmCtx, array->items[i]);
			break;
		}
		case OBJ_BOUND_METHOD: {
			ObjBoundMethod *bound = (ObjBoundMethod *)object;
			markValue(vmCtx, bound->receiver);
			markObject(vmCtx, bound->method);
			break;
		}
		case OBJ_METHOD: {
			ObjMethod *method = (ObjMethod *)object;
			markObject(vmCtx, (Obj *)method->method.klass);
			markObject(vmCtx, method->method.callable);
			markObject(vmCtx, (Obj *)method->method.fromDefault);
			break;
		}
		case OBJ_PENDING_METHOD: {
			ObjMethod *method = (ObjMethod *)object;
			markObject(vmCtx, (Obj *)method->pending.fromDefault);
			break;
		}
		case OBJ_DEFAULT_METHOD: {
			ObjDefaultMethod *method = (ObjDefaultMethod *)object;
			markObject(vmCtx, (Obj *)method->function);
			break;
		}
		case OBJ_ABSTRACT_METHOD:
			break;
		case OBJ_INTERFACE: {
			ObjKlass *klass = (ObjKlass *)object;
			markObject(vmCtx, (Obj *)klass->name);
			ObjInterface *intf = (ObjInterface *)object;
			markTable(vmCtx, &intf->methods);
			break;
		}
		case OBJ_CLASS: {
			ObjKlass *klass = (ObjKlass *)object;
			markObject(vmCtx, (Obj *)klass->name);
			ObjClass *clazz = (ObjClass *)object;
			markPropTable(vmCtx, &clazz->props);
			markArray(vmCtx, &clazz->classData);
			markValue(vmCtx, clazz->initializer);
			break;
		}
		case OBJ_CLOSURE: {
			ObjClosure *closure = (ObjClosure *)object;
			markObject(vmCtx, (Obj *)closure->function);
			for (int i = 0; i < closure->upvalueCount; i++)
				markObject(vmCtx, (Obj *)closure->upvalues[i]);
			break;
		}
		case OBJ_NATIVE_CLOSURE: {
			ObjNativeClosure *closure = (ObjNativeClosure *)object;
			for (int i = 0; i < closure->upvalueCount; i++)
				markValue(vmCtx, closure->upvalues[i]);
			break;
		}
		case OBJ_FUNCTION: {
			ObjFunction *function = (ObjFunction *)object;
#ifdef ELOX_DEBUG_LOG_GC
	if (function->name) {
		eloxPrintf(runCtx, ELOX_IO_DEBUG, "%p [marking function %.*s]\n",
				   (void *)object, function->name->string.length, function->name->string.chars);
	} else
		eloxPrintf(runCtx, ELOX_IO_DEBUG, "%p [marking function]\n", (void *)object);
#endif
			markObject(vmCtx, (Obj *)function->name);
			markObject(vmCtx, (Obj *)function->parentClass);
#ifdef ELOX_DEBUG_LOG_GC
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%p [marking %d constants]\n",
			   (void *)object, function->chunk.constants.count);
#endif
			markArray(vmCtx, &function->chunk.constants);
			markObject(vmCtx, (Obj *)function->chunk.fileName);
			if (function->defaultArgs != NULL) {
				for (int i = 0; i < function->arity; i++)
					markValue(vmCtx, function->defaultArgs[i]);
			}
			break;
		}
		case OBJ_INSTANCE: {
			ObjInstance *instance = (ObjInstance *)object;
			markObject(vmCtx, (Obj *)instance->class_);
			for (uint16_t i = 0; i < instance->numFields; i++)
				markValue(vmCtx, instance->fields[i]);
			break;
		}
		case OBJ_UPVALUE:
			markValue(vmCtx, ((ObjUpvalue *)object)->closed);
			break;
		case OBJ_STRINGPAIR:
			markObject(vmCtx, (Obj *)((ObjStringPair *)object)->str1);
			markObject(vmCtx, (Obj *)((ObjStringPair *)object)->str2);
			break;
		case OBJ_NATIVE: {
			ObjNative *native = (ObjNative *)object;
			if (native->defaultArgs != NULL) {
				for (int i = 0; i < native->arity; i++)
					markValue(vmCtx, native->defaultArgs[i]);
			}
			break;
		}
		case OBJ_STRING:
		case OBJ_FRAME:
			break;
		case OBJ_FIBER: {
			ObjFiber *fiber = (ObjFiber *)object;
			markObject(vmCtx, (Obj *)fiber->parent);
			markObject(vmCtx, (Obj *)fiber->function);
			markObject(vmCtx, (Obj *)fiber->closure);
			for (Value *slot = fiber->stack; slot < fiber->stackTop; slot++)
				markValue(vmCtx, *slot);

			for (ObjCallFrame *frame = fiber->activeFrame; frame != NULL;
				 frame = (ObjCallFrame *)getObjNext(&frame->obj))
				markObject(vmCtx, (Obj *)frame->function);

			for (ObjUpvalue *upvalue = fiber->openUpvalues; upvalue != NULL; upvalue = upvalue->next)
				markObject(vmCtx, (Obj *)upvalue);

			VMTemp *temp = fiber->temps;
			while (temp != NULL) {
				markValue(vmCtx, temp->val);
				temp = temp->next;
			}
			break;
		}
	}
}

static void freeObject(VMCtx *vmCtx, Obj *object) {
//#if defined(ELOX_DEBUG_LOG_GC) || defined(ELOX_DEBUG_TRACE_EXECUTION)
#ifdef ELOX_DEBUG_LOG_GC
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "%p free type %d\n (", (void *)object, object->type);
	// below is unsafe
	//printObject(runCtx, ELOX_IO_DEBUG, object);
	printf(")\n");
#endif

	switch (getObjType(object)) {
		case OBJ_HASHMAP: {
			ObjHashMap *map = (ObjHashMap *)object;
			freeValueTable(vmCtx, &map->items);
			FREE(vmCtx, ObjHashMap, object);
			break;
		}
		case OBJ_TUPLE:
		case OBJ_ARRAY: {
			ObjArray *array = (ObjArray *)object;
			FREE_ARRAY(vmCtx, Value *, array->items, array->size);
			FREE(vmCtx, ObjArray, object);
			break;
		}
		case OBJ_BOUND_METHOD:
			FREE(vmCtx, ObjBoundMethod, object);
			break;
		case OBJ_METHOD:
		case OBJ_PENDING_METHOD:
		case OBJ_ABSTRACT_METHOD:
			FREE(vmCtx, ObjMethod, object);
			break;
		case OBJ_DEFAULT_METHOD: {
			ObjDefaultMethod *method = (ObjDefaultMethod *)object;
			FREE_ARRAY(vmCtx, RefBindDesc, method->refs, method->numRefs);
			FREE(vmCtx, ObjDefaultMethod, object);
			break;
		}
		case OBJ_INTERFACE: {
			ObjInterface *intf = (ObjInterface *)object;
			freeTable(vmCtx, &intf->methods);
			FREE(vmCtx, ObjInterface, object);
			break;
		}
		case OBJ_CLASS: {
			ObjClass *clazz = (ObjClass *)object;
			freeOpenKlass(vmCtx, clazz->openKlass);
			clazz->openKlass = NULL;
			freePropTable(vmCtx, &clazz->props);
			freeValueArray(vmCtx, &clazz->classData);
			FREE_ARRAY(vmCtx, Ref, clazz->refs, clazz->numRefs);
			if (clazz->typeInfo.rssList != NULL)
				FREE_ARRAY(vmCtx, Obj *, clazz->typeInfo.rssList, clazz->typeInfo.numRss);
			FREE(vmCtx, ObjClass, object);
			break;
		}
		case OBJ_CLOSURE: {
			ObjClosure *closure = (ObjClosure *)object;
			FREE_ARRAY(vmCtx, ObjUpvalue *, closure->upvalues, closure->upvalueCount);
			FREE(vmCtx, ObjClosure, object);
			break;
		}
		case OBJ_NATIVE_CLOSURE: {
			ObjNativeClosure *closure = (ObjNativeClosure *)object;
			FREE_ARRAY(vmCtx, Value, closure->upvalues, closure->upvalueCount);
			FREE(vmCtx, ObjNativeClosure, object);
			break;
		}
		case OBJ_FUNCTION: {
			ObjFunction *function = (ObjFunction *)object;
			freeChunk(vmCtx, &function->chunk);
			FREE_ARRAY(vmCtx, Value, function->defaultArgs, function->arity);
			FREE(vmCtx, ObjFunction, object);
			break;
		}
		case OBJ_INSTANCE: {
			ObjInstance *instance = (ObjInstance *)object;
			FREE_ARRAY(vmCtx, Value, instance->fields, instance->numFields);
			FREE(vmCtx, ObjInstance, object);
			break;
		}
		case OBJ_NATIVE: {
			ObjNative *native = (ObjNative *)object;
			FREE_ARRAY(vmCtx, Value, native->defaultArgs, native->arity);
			FREE(vmCtx, ObjNative, object);
			break;
		}
		case OBJ_STRING: {
			ObjString *string = (ObjString *)object;
			FREE_ARRAY(vmCtx, char, ELOX_UNCONST(string->string.chars), string->string.length + 1);
			FREE(vmCtx, ObjString, object);
			break;
		}
		case OBJ_STRINGPAIR:
			FREE(vmCtx, ObjStringPair, object);
			break;
		case OBJ_UPVALUE:
			FREE(vmCtx, ObjUpvalue, object);
			break;
		case OBJ_FRAME:
			FREE(vmCtx, ObjCallFrame, object);
			break;
		case OBJ_FIBER:
			destroyFiber(vmCtx, (ObjFiber *)object);
			break;
	}
}

static void markRoots(VMCtx *vmCtx) {
	VM *vm = vmCtx->vm;

	markObject(vmCtx, (Obj *)vm->tmpFiber);
	markValueTable(vmCtx, &vm->globalNames);
	markArray(vmCtx, &vm->globalValues);

	markHandleSet(vmCtx, &vm->handles);
}

static void slowTraceReferences(VMCtx *vmCtx) {
	VM *vm = vmCtx->vm;

	bool haveGray;
	do {
		haveGray = false;
		Obj *object = vm->mainHeap.objects;
		while (object != NULL) {
			uint8_t markers = getObjMarkers(object);
			if (markers & MARKER_GRAY) {
				haveGray = true;
				setObjMarkers(object, markers & ~MARKER_GRAY);
				blackenObject(vmCtx, object);
			}
			object = getObjNext(object);
		}
	} while (haveGray);
}

static void traceReferences(VMCtx *vmCtx) {
	VM *vm = vmCtx->vm;

	if (ELOX_UNLIKELY(vm->grayOverflow)) {
		slowTraceReferences(vmCtx);
		goto cleanup;
	}
	while (vm->grayCount > 0) {
		Obj *object = vm->grayStack[--vm->grayCount];
		setObjMarkers(object, (getObjMarkers(object) & ~MARKER_GRAY));
		blackenObject(vmCtx, object);
		if (ELOX_UNLIKELY(vm->grayOverflow)) {
			slowTraceReferences(vmCtx);
			goto cleanup;
		}
	}

cleanup:
	vm->grayCount = 0;
	vm->grayOverflow = 0;
}

static void sweep(VMCtx *vmCtx) {
	VM *vm = vmCtx->vm;

	Obj *previous = NULL;
	Obj *object = vm->mainHeap.objects;
	while (object != NULL) {
		uint8_t markers = getObjMarkers(object);
		if (markers != 0) {
			setObjMarkers(object, markers & ~(MARKER_BLACK | MARKER_GRAY));
			previous = object;
			object = getObjNext(object);
		} else {
			Obj *unreached = object;
			object = getObjNext(object);
			if (previous != NULL)
				setObjNext(previous, object);
			else
				vm->mainHeap.objects = object;

			freeObject(vmCtx, unreached);
		}
	}
}

static bool cleanOrphanFibers(RunCtx *runCtx) {
	VMCtx *vmCtx = runCtx->vmCtx;
	VM *vm = vmCtx->vm;

	if (ELOX_UNLIKELY(vm->suspendedHead == NULL))
		return false;

	bool ret = false;

#ifdef ELOX_DEBUG_TRACE_EXECUTION
	bool debugTrace = false;
#endif

	ObjFiber *fiber = vm->suspendedHead->nextSuspended;
	while (fiber != vm->suspendedHead) {
		ObjFiber *nextFiber = fiber->nextSuspended;

		if (getObjMarkers(&fiber->obj) == 0) {
			// unreacheable suspended fiber

#ifdef ELOX_DEBUG_TRACE_EXECUTION
			if (!debugTrace) {
				ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "[Cleanup idle]\n");
				debugTrace = true;
			}
#endif

			markObject(vmCtx, (Obj *)fiber);

			EloxError error = ELOX_ERROR_INITIALIZER;
			ObjFiber *activeFiber = runCtx->activeFiber;
			resumeThrow(runCtx, fiber, vm->builtins.terminateError, &error);
			fiber->parent = NULL;
			propagateException(runCtx);
			fiber->state = ELOX_FIBER_TERMINATED;
			runCtx->activeFiber = activeFiber;

			ret = true;
		}

		fiber = nextFiber;
	}

#ifdef ELOX_DEBUG_TRACE_EXECUTION
	if (debugTrace)
		ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "[Cleanup idle done]\n");
#endif

	return ret;
}

void collectGarbage(RunCtx *runCtx) {
	VMCtx *vmCtx = runCtx->vmCtx;
	VM *vm = vmCtx->vm;

#ifdef ELOX_DEBUG_LOG_GC
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "-- gc begin\n");
	size_t before = vm->bytesAllocated;
#endif

	markRoots(vmCtx);
	traceReferences(vmCtx);
	if (cleanOrphanFibers(runCtx))
		traceReferences(vmCtx);
	tableRemoveWhite(&vm->strings);
	sweep(vmCtx);

	vm->nextGC = vm->bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef ELOX_DEBUG_LOG_GC
	ELOX_WRITE(runCtx, ELOX_IO_DEBUG, "-- gc end\n");
	eloxPrintf(runCtx, ELOX_IO_DEBUG, "   collected %zu bytes (from %zu to %zu) next at %zu\n",
			   before - vm->bytesAllocated, before, vm->bytesAllocated, vm->nextGC);
#endif
}

void freeObjects(VMCtx *vmCtx) {
	VM *vm = vmCtx->vm;
	VMEnv *env = vmCtx->vmEnv;

	Obj *object = vm->mainHeap.objects;
	while (object != NULL) {
		Obj *next = getObjNext(object);
		freeObject(vmCtx, object);
		object = next;
	}

	object = vm->permHeap.objects;
	while (object != NULL) {
		Obj *next = getObjNext(object);
		freeObject(vmCtx, object);
		object = next;
	}

	env->free(vm->grayStack, env->allocatorUserData);
}
