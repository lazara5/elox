#include <stdio.h>
#include <string.h>
#include <math.h>
#include <inttypes.h>

#include "elox/memory.h"
#include "elox/object.h"
#include "elox/value.h"

void initValueArray(ValueArray *array) {
	array->values = NULL;
	array->capacity = 0;
	array->count = 0;
}

void initSizedValueArray(VMCtx *vmCtx, ValueArray *array, size_t size) {
	array->values = NULL;
	array->count = 0;
	array->values = ALLOCATE(vmCtx, Value, size);
	array->capacity = array->count = size;
	for (size_t i = 0; i < size; i++)
		array->values[i] = NIL_VAL;
}

void writeValueArray(VMCtx *vmCtx, ValueArray *array, Value value) {
	if (array->capacity < array->count + 1) {
		int oldCapacity = array->capacity;
		array->capacity = GROW_CAPACITY(oldCapacity);
		array->values = GROW_ARRAY(vmCtx, Value, array->values, oldCapacity, array->capacity);
	}

	array->values[array->count] = value;
	array->count++;
}

void freeValueArray(VMCtx *vmCtx, ValueArray *array) {
	FREE_ARRAY(vmCtx, Value, array->values, array->capacity);
	initValueArray(array);
}

static void printNumber(double n) {
	if (trunc(n) == n) {
		printf("%" PRId64, (int64_t)n);
	} else {
		printf("%g", n);
	}
}

void printValue(Value value) {
#ifdef ENABLE_NAN_BOXING
	if (IS_BOOL(value)) {
		printf(AS_BOOL(value) ? "true" : "false");
	} else if (IS_NIL(value)) {
		printf("nil");
	} else if (IS_NUMBER(value)) {
		printNumber(AS_NUMBER(value));
	} else if (IS_OBJ(value)) {
		printObject(value);
	}
#else
	switch (value.type) {
		case VAL_BOOL:
			printf(AS_BOOL(value) ? "true" : "false");
			break;
		case VAL_NIL:
			printf("nil");
			break;
		case VAL_NUMBER:
			printNumber(AS_NUMBER(value));
			break;
		case VAL_OBJ:
			printObject(value);
			break;
		case VAL_EXCEPTION:
		case VAL_UNDEFINED:
			// TODO: implement
			break;
	 }
#endif // ENABLE_NAN_BOXING
}

bool valuesEqual(Value a, Value b) {
#ifdef ENABLE_NAN_BOXING
	if (IS_NUMBER(a) && IS_NUMBER(b)) {
		return AS_NUMBER(a) == AS_NUMBER(b);
	}
	return a == b;
#else
	if (a.type != b.type)
		return false;

	switch (a.type) {
		case VAL_BOOL:
			return AS_BOOL(a) == AS_BOOL(b);
		case VAL_NIL:
			return true;
		case VAL_NUMBER:
			return AS_NUMBER(a) == AS_NUMBER(b);
		case VAL_OBJ:
			return AS_OBJ(a) == AS_OBJ(b);
		default:
			return false; // Unreachable.
	}
#endif // ENABLE_NAN_BOXING
}
