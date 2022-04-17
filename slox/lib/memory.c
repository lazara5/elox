#include <stdlib.h>

#include "slox/compiler.h"
#include "slox/memory.h"
#include "slox/state.h"
#include "slox/builtins.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "slox/debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2

void *reallocate(VMCtx *vmCtx, void *pointer, size_t oldSize, size_t newSize) {
	VM *vm = &vmCtx->vm;
	vm->bytesAllocated += newSize - oldSize;
	if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
		collectGarbage(vmCtx);
#else
		if (vm->bytesAllocated > vm->nextGC) {
			collectGarbage(vmCtx);
		}
#endif
	}

	if (newSize == 0) {
		free(pointer);
		return NULL;
	}

	void *result = realloc(pointer, newSize);
	if (result == NULL)
		exit(1);
	return result;
}

void markObject(VMCtx *vmCtx, Obj *object) {
	if (object == NULL)
		return;
	if (object->isMarked)
		return;

	VM *vm = &vmCtx->vm;

#ifdef DEBUG_LOG_GC
	printf("%p mark ", (void *)object);
	printValue(OBJ_VAL(object));
	printf("\n");
#endif

	object->isMarked = true;

	if (vm->grayCapacity < vm->grayCount + 1) {
		vm->grayCapacity = GROW_CAPACITY(vm->grayCapacity);
		vm->grayStack = (Obj **)vmCtx->realloc(vm->grayStack,
											   sizeof(Obj *) * vm->grayCapacity,
											   vmCtx->allocatorUserdata);
		if (vm->grayStack == NULL)
			exit(1);
	}

	vm->grayStack[vm->grayCount++] = object;
}

void markValue(VMCtx *vmCtx, Value value) {
	if (IS_OBJ(value))
		markObject(vmCtx, AS_OBJ(value));
}

static void markArray(VMCtx *vmCtx, ValueArray *array) {
	for (int i = 0; i < array->count; i++)
		markValue(vmCtx, array->values[i]);
}

static void blackenObject(VMCtx *vmCtx, Obj *object) {
#ifdef DEBUG_LOG_GC
	printf("%p blacken ", (void *)object);
	printValue(OBJ_VAL(object));
	printf("\n");
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
			for (int i = 0; i < array->size; i++) {
				markValue(vmCtx, array->items[i]);
			}
			break;
		}
		case OBJ_BOUND_METHOD: {
			ObjBoundMethod *bound = (ObjBoundMethod *)object;
			markValue(vmCtx, bound->receiver);
			markObject(vmCtx, bound->method);
			break;
		}
		case OBJ_CLASS: {
			ObjClass *clazz = (ObjClass *)object;
			markObject(vmCtx, (Obj *)clazz->name);
			markTable(vmCtx, &clazz->fields);
			markTable(vmCtx, &clazz->methods);
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
			markObject(vmCtx, (Obj *)function->name);
			markObject(vmCtx, (Obj *)function->parentClass);
			markArray(vmCtx, &function->chunk.constants);
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
		case OBJ_NATIVE:
		case OBJ_STRING:
			break;
	}
}

static void freeObject(VMCtx *vmCtx, Obj *object) {
#ifdef DEBUG_LOG_GC
	printf("%p free type %d\n", (void *)object, object->type);
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
		case OBJ_CLASS: {
			ObjClass *clazz = (ObjClass *)object;
			freeTable(vmCtx, &clazz->fields);
			freeTable(vmCtx, &clazz->methods);
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
			FREE(vmCtx, ObjFunction, object);
			break;
		}
		case OBJ_INSTANCE: {
			ObjInstance *instance = (ObjInstance *)object;
			freeValueArray(vmCtx, &instance->fields);
			FREE(vmCtx, ObjInstance, object);
			break;
		}
		case OBJ_NATIVE:
			FREE(vmCtx, ObjNative, object);
			break;
		case OBJ_STRING: {
			ObjString *string = (ObjString *)object;
			FREE_ARRAY(vmCtx, char, string->chars, string->length + 1);
			FREE(vmCtx, ObjString, object);
			break;
		}
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

	markTable(vmCtx, &vm->globals);
	markCompilerRoots(vmCtx);

	markBuiltins(vmCtx);
}

static void traceReferences(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;
	while (vm->grayCount > 0) {
		Obj *object = vm->grayStack[--vm->grayCount];
		blackenObject(vmCtx, object);
	}
}

static void sweep(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	Obj *previous = NULL;
	Obj *object = vm->objects;
	while (object != NULL) {
		if (object->isMarked) {
			object->isMarked = false;
			previous = object;
			object = object->next;
		} else {
			Obj *unreached = object;
			object = object->next;
			if (previous != NULL) {
				previous->next = object;
			} else {
				vm->objects = object;
			}

			freeObject(vmCtx, unreached);
		}
	}
}

void collectGarbage(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	if (SLOX_UNLIKELY(vm->stack == NULL))
		return;

#ifdef DEBUG_LOG_GC
	printf("-- gc begin\n");
	size_t before = vm->bytesAllocated;
#endif

	markRoots(vmCtx);
	traceReferences(vmCtx);
	tableRemoveWhite(&vm->strings);
	sweep(vmCtx);

	vm->nextGC = vm->bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
	printf("-- gc end\n");
	printf("   collected %zu bytes (from %zu to %zu) next at %zu\n",
		   before - vm->bytesAllocated, before, vm->bytesAllocated, vm->nextGC);
#endif
}

void freeObjects(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	Obj *object = vm->objects;
	while (object != NULL) {
		Obj *next = object->next;
		freeObject(vmCtx, object);
		object = next;
	}

	vmCtx->free(vm->grayStack, vmCtx->allocatorUserdata);
}
