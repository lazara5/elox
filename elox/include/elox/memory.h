// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_MEMORY_H
#define ELOX_MEMORY_H

#include "elox/value.h"

#define ALLOCATE(RUNCTX, type, count) \
	(type *)vmRealloc(RUNCTX, NULL, 0, sizeof(type) * (size_t)(count))

#define FREE(VMCTX, type, pointer) vmFree(VMCTX, pointer, sizeof(type))
#define GENERIC_FREE(VMCTX, size, pointer) vmFree(VMCTX, pointer, size)

#define GROW_CAPACITY(capacity) \
	((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(RUNCTX, type, pointer, oldCount, newCount) \
	(type *)vmRealloc(RUNCTX, pointer, sizeof(type) * (size_t)(oldCount), \
		sizeof(type) * ((size_t)newCount))

#define FREE_ARRAY(VMCTX, type, pointer, oldCount) \
	vmFree(VMCTX, pointer, sizeof(type) * (size_t)(oldCount))

void *vmRealloc(RunCtx *runCtx, void *pointer, size_t oldSize, size_t newSize);
void vmFree(VMCtx *vmCtx, void *pointer, size_t oldSize);
void markObject(VMCtx *vmCtx, Obj *object);
void markValue(VMCtx *vmCtx, Value value);
void collectGarbage(RunCtx *runCtx);
void freeObjects(VMCtx *vmCtx);

#endif // ELOX_MEMORY_H
