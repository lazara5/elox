// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_MEMORY_H
#define ELOX_MEMORY_H

#include "elox/value.h"

typedef struct VMCtx VMCtx;

#define ALLOCATE(vmctx, type, count) \
	(type *)reallocate(vmctx, NULL, 0, sizeof(type) * (size_t)(count))

#define FREE(runctx, type, pointer) reallocate(runctx, pointer, sizeof(type), 0)
#define GENERIC_FREE(runctx, size, pointer) reallocate(runctx, pointer, size, 0)

#define GROW_CAPACITY(capacity) \
	((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(runctx, type, pointer, oldCount, newCount) \
	(type *)reallocate(runctx, pointer, sizeof(type) * (size_t)(oldCount), \
		sizeof(type) * ((size_t)newCount))

#define FREE_ARRAY(runctx, type, pointer, oldCount) \
	reallocate(runctx, pointer, sizeof(type) * (size_t)(oldCount), 0)

void *reallocate(RunCtx *runCtx, void *pointer, size_t oldSize, size_t newSize);
void markObject(RunCtx *runCtx, Obj *object);
void markValue(RunCtx *runCtx, Value value);
void collectGarbage(RunCtx *runCtx);
void freeObjects(RunCtx *runCtx);

#endif // ELOX_MEMORY_H
