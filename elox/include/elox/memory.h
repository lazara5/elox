#ifndef ELOX_MEMORY_H
#define ELOX_MEMORY_H

#include "elox/common.h"
#include "elox/object.h"

typedef struct VMCtx VMCtx;

#define ALLOCATE(vmctx, type, count) \
	(type *)reallocate(vmctx, NULL, 0, sizeof(type) * (size_t)(count))

#define FREE(vmctx, type, pointer) reallocate(vmctx, pointer, sizeof(type), 0)

#define GROW_CAPACITY(capacity) \
	((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(vmctx, type, pointer, oldCount, newCount) \
	(type *)reallocate(vmctx, pointer, sizeof(type) * (size_t)(oldCount), \
		sizeof(type) * ((size_t)newCount))

#define FREE_ARRAY(vmctx, type, pointer, oldCount) \
	reallocate(vmctx, pointer, sizeof(type) * (size_t)(oldCount), 0)

void *reallocate(VMCtx *vmCtx, void *pointer, size_t oldSize, size_t newSize);
void markObject(VMCtx *vmCtx, Obj *object);
void markValue(VMCtx *vmCtx, Value value);
void collectGarbage(VMCtx *vmCtx);
void freeObjects(VMCtx *vmCtx);

typedef void *(*EloxRealloc)(void *oldPtr, size_t newSize, void *userData);
typedef void (*EloxFree)(void *ptr, void *userData);

#endif // ELOX_MEMORY_H
