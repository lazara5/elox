// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_MEMORY_H
#define ELOX_MEMORY_H

#include "elox/util.h"
#include "elox/value.h"

typedef struct VMCtx VMCtx;

#define ALLOCATE(vmctx, type, count) \
	(type *)reallocate(vmctx, NULL, 0, sizeof(type) * (size_t)(count))

#define FREE(vmctx, type, pointer) reallocate(vmctx, pointer, sizeof(type), 0)
#define GENERIC_FREE(vmctx, size, pointer) reallocate(vmctx, pointer, size, 0)

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

#endif // ELOX_MEMORY_H
