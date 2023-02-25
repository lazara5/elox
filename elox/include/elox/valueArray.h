// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_VALUE_ARRAY_H
#define ELOX_VALUE_ARRAY_H

#include <elox/value.h>
#include <elox/memory.h>

typedef struct {
	uint32_t capacity;
	uint32_t count;
	Value *values;
} ValueArray;

void initValueArray(ValueArray *array);
void initSizedValueArray(VMCtx *vmCtx, ValueArray *array, size_t size);
void initEmptyValueArray(VMCtx *vmCtx, ValueArray *array, size_t size);

static inline void valueArrayPush(VMCtx *vmCtx, ValueArray *array, Value value) {
	if (ELOX_UNLIKELY(array->capacity < array->count + 1)) {
		int oldCapacity = array->capacity;
		array->capacity = GROW_CAPACITY(oldCapacity);
		array->values = GROW_ARRAY(vmCtx, Value, array->values, oldCapacity, array->capacity);
	}

	array->values[array->count] = value;
	array->count++;
}

static inline void valueArrayPop(ValueArray *array) {
	array->count--;
}

void freeValueArray(VMCtx *vmCtx, ValueArray *array);

#endif // ELOX_VALUE_ARRAY_H
