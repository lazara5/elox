// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/ValueArray.h>

#include <assert.h>

void initValueArray(ValueArray *array) {
	array->values = NULL;
	array->capacity = 0;
	array->count = 0;
}

bool initSizedValueArray(RunCtx *runCtx, ValueArray *array, size_t size) {
	assert (size > 0);

	array->values = NULL;
	array->count = 0;
	array->values = ALLOCATE(runCtx, Value, size);
	if (ELOX_UNLIKELY(array->values == NULL)) {
		array->capacity = 0;
		return false;
	}
	array->capacity = size;

	return true;
}

bool initEmptyValueArray(RunCtx *runCtx, ValueArray *array, size_t size) {
	array->values = NULL;
	array->count = 0;

	if (size == 0) {
		array->capacity = 0;
		return true;
	}

	array->values = ALLOCATE(runCtx, Value, size);
	if (ELOX_UNLIKELY(array->values == NULL)) {
		array->capacity = 0;
		return false;
	}
	array->capacity = array->count = size;
	for (size_t i = 0; i < size; i++)
		array->values[i] = NIL_VAL;

	return true;
}

void freeValueArray(RunCtx *runCtx, ValueArray *array) {
	FREE_ARRAY(runCtx, Value, array->values, array->capacity);
	initValueArray(array);
}

bool valueArrayPush(RunCtx *runCtx, ValueArray *array, Value value) {
	if (ELOX_UNLIKELY(array->capacity < array->count + 1)) {
		int oldCapacity = array->capacity;
		array->capacity = GROW_CAPACITY(oldCapacity);
		Value *oldValues = array->values;
		array->values = GROW_ARRAY(runCtx, Value, array->values, oldCapacity, array->capacity);
		if (ELOX_UNLIKELY(array->values == NULL)) {
			array->values = oldValues;
			return false;
		}
	}

	array->values[array->count] = value;
	array->count++;
	return true;
}
