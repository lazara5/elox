// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <stdlib.h>
#include <stdio.h>

#include "elox/compiler.h"
#include "elox/memory.h"
#include "elox/state.h"
#include "elox/builtins.h"

#ifdef ELOX_DEBUG_LOG_GC
#include <elox/debug.h>
#endif

#define GC_HEAP_GROW_FACTOR 2

void *reallocate(VMCtx *vmCtx, void *pointer, size_t oldSize, size_t newSize) {
	VM *vm = &vmCtx->vm;

	vm->bytesAllocated += newSize - oldSize;
	if (newSize > oldSize) {
#ifdef ELOX_DEBUG_STRESS_GC
		collectGarbage(vmCtx);
#else
		if (vm->bytesAllocated > vm->nextGC)
			collectGarbage(vmCtx);
#endif
	}

	if (newSize == 0) {
		vmCtx->free(pointer, vmCtx->allocatorUserData);
		return NULL;
	}

	void *result = vmCtx->realloc(pointer, newSize, vmCtx->allocatorUserData);
	return result;
}

void markObject(VMCtx *vmCtx, Obj *object) {
	if (object == NULL)
		return;
	if (object->markers & MARKER_BLACK)
		return;

	VM *vm = &vmCtx->vm;

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

	if (vm->grayCapacity < vm->grayCount + 1) {
		int newGrayCapacity = GROW_CAPACITY(vm->grayCapacity);
		Obj **oldStack = vm->grayStack;
		vm->grayStack = (Obj **)vmCtx->realloc(vm->grayStack,
											   sizeof(Obj *) * newGrayCapacity,
											   vmCtx->allocatorUserData);
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
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%p blacken ", (void *)object);
	printValue(vmCtx, ELOX_IO_DEBUG, OBJ_VAL(object));
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "\n");
#endif
	switch (object->type) {
		case OBJ_MAP: {
			ObjMap *map = (ObjMap *)object;
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
			markObject(vmCtx, (Obj *)method->clazz);
			markObject(vmCtx, method->callable);
			break;
		}
		case OBJ_CLASS: {
			ObjClass *clazz = (ObjClass *)object;
			markObject(vmCtx, (Obj *)clazz->name);
			markTable(vmCtx, &clazz->fields);
			markTable(vmCtx, &clazz->methods);
			markTable(vmCtx, &clazz->statics);
			markArray(vmCtx, &clazz->staticValues);
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
		eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%p [marking function %.*s]\n",
					(void *)object, function->name->string.length, function->name->string.chars);
	} else
		eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%p [marking function]\n", (void *)object);
#endif
			markObject(vmCtx, (Obj *)function->name);
			markObject(vmCtx, (Obj *)function->parentClass);
#ifdef ELOX_DEBUG_LOG_GC
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%p [marking %d constants]\n",
			   (void *)object, function->chunk.constants.count);
#endif
			markArray(vmCtx, &function->chunk.constants);
			if (function->defaultArgs != NULL) {
				for (int i = 0; i < function->arity; i++)
					markValue(vmCtx, function->defaultArgs[i]);
			}
			break;
		}
		case OBJ_INSTANCE: {
			ObjInstance *instance = (ObjInstance *)object;
			markObject(vmCtx, (Obj *)instance->clazz);
			markArray(vmCtx, &instance->fields);
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
		}
		case OBJ_STRING:
			break;
	}
}

static void freeObject(VMCtx *vmCtx, Obj *object) {
//#if defined(ELOX_DEBUG_LOG_GC) || defined(ELOX_DEBUG_TRACE_EXECUTION)
#ifdef ELOX_DEBUG_LOG_GC
	//printf("%p free type %d (", (void *)object, object->type);
	//printObject(object);
	//printf(")\n");
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "%p free type %d\n (", (void *)object, object->type);
#endif

	switch (object->type) {
		case OBJ_MAP: {
			ObjMap *map = (ObjMap *)object;
			freeValueTable(vmCtx, &map->items);
			FREE(vmCtx, ObjMap, object);
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
			FREE(vmCtx, ObjMethod, object);
			break;
		case OBJ_CLASS: {
			ObjClass *clazz = (ObjClass *)object;
			freeTable(vmCtx, &clazz->fields);
			freeTable(vmCtx, &clazz->methods);
			freeTable(vmCtx, &clazz->statics);
			freeValueArray(vmCtx, &clazz->staticValues);
			FREE_ARRAY(vmCtx, MemberRef, clazz->memberRefs, clazz->memberRefCount);
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
			freeValueArray(vmCtx, &instance->fields);
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
	}
}

static void markRoots(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	for (Value *slot = vm->stack; slot < vm->stackTop; slot++)
		markValue(vmCtx, *slot);

	for (int i = 0; i < vm->frameCount; i++)
		markObject(vmCtx, vm->frames[i].function);

	for (ObjUpvalue *upvalue = vm->openUpvalues; upvalue != NULL; upvalue = upvalue->next)
		markObject(vmCtx, (Obj *)upvalue);

	VMTemp *temp = vm->temps;
	while (temp != NULL) {
		markValue(vmCtx, temp->val);
		temp = temp->next;
	}

	markValueTable(vmCtx, &vm->globalNames);
	markArray(vmCtx, &vm->globalValues);
	markCompilerRoots(vmCtx);

	markHandleSet(vmCtx, &vm->handles);
}

static void slowTraceReferences(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	bool haveGray;
	do {
		haveGray = false;
		Obj *object = vm->mainHeap.objects;
		while (object != NULL) {
			if (object->markers & MARKER_GRAY) {
				haveGray = true;
				object->markers &= ~MARKER_GRAY;
				blackenObject(vmCtx, object);
			}
			object = object->next;
		}
	} while (haveGray);
}

static void traceReferences(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	if (ELOX_UNLIKELY(vm->grayOverflow)) {
		slowTraceReferences(vmCtx);
		goto cleanup;
	}
	while (vm->grayCount > 0) {
		Obj *object = vm->grayStack[--vm->grayCount];
		object->markers &= ~MARKER_GRAY;
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
	VM *vm = &vmCtx->vm;

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

			freeObject(vmCtx, unreached);
		}
	}
}

void collectGarbage(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	if (ELOX_UNLIKELY(vm->stack == NULL))
		return;

#ifdef ELOX_DEBUG_LOG_GC
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "-- gc begin\n");
	size_t before = vm->bytesAllocated;
#endif

	markRoots(vmCtx);
	traceReferences(vmCtx);
	tableRemoveWhite(&vm->strings);
	sweep(vmCtx);

	vm->nextGC = vm->bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef ELOX_DEBUG_LOG_GC
	ELOX_WRITE(vmCtx, ELOX_IO_DEBUG, "-- gc end\n");
	eloxPrintf(vmCtx, ELOX_IO_DEBUG, "   collected %zu bytes (from %zu to %zu) next at %zu\n",
			   before - vm->bytesAllocated, before, vm->bytesAllocated, vm->nextGC);
#endif
}

void freeObjects(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	Obj *object = vm->mainHeap.objects;
	while (object != NULL) {
		Obj *next = object->next;
		freeObject(vmCtx, object);
		object = next;
	}

	object = vm->permHeap.objects;
	while (object != NULL) {
		Obj *next = object->next;
		freeObject(vmCtx, object);
		object = next;
	}

	vmCtx->free(vm->grayStack, vmCtx->allocatorUserData);
}
