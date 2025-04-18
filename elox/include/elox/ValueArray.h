// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_VALUE_ARRAY_H
#define ELOX_VALUE_ARRAY_H

#include <elox/value.h>
#include <elox/memory.h>

typedef struct ValueArray {
	uint32_t capacity;
	uint32_t count;
	Value *values;
} ValueArray;

void initValueArray(ValueArray *array);
bool initSizedValueArray(RunCtx *runCtx, ValueArray *array, size_t size);
bool initEmptyValueArray(RunCtx *runCtx, ValueArray *array, size_t size);

bool valueArrayPush(RunCtx *runCtx, ValueArray *array, Value value);
bool valueArraySet(RunCtx *runCtx, ValueArray *array, uint32_t index, Value value);

bool cloneValueArray(RunCtx *runCtx, ValueArray *dst, ValueArray *src);

static inline void valueArrayPop(ValueArray *array) {
	array->count--;
}

static inline void valueArrayPopN(ValueArray *array, uint32_t count) {
	array->count -= count;
}

void freeValueArray(RunCtx *runCtx, ValueArray *array);

#endif // ELOX_VALUE_ARRAY_H
