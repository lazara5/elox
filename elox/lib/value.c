#include <stdio.h>
#include <string.h>
#include <math.h>
#include <inttypes.h>

#include <elox/object.h>
#include <elox/value.h>
#include <elox/state.h>

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

static void printNumber(VMCtx *vmCtx, EloxIOStream stream, double n) {
	if (trunc(n) == n)
		elox_printf(vmCtx, stream, "%" PRId64, (int64_t)n);
	else
		elox_printf(vmCtx, stream, "%g", n);
}

void printValue(VMCtx *vmCtx, EloxIOStream stream, Value value) {
#ifdef ELOX_ENABLE_NAN_BOXING
	if (IS_BOOL(value))
		printf(AS_BOOL(value) ? "true" : "false");
	else if (IS_NIL(value))
		printf("nil");
	else if (IS_NUMBER(value))
		printNumber(vmCtx, stream, AS_NUMBER(value));
	else if (IS_OBJ(value))
		printValueObject(vmCtx, stream, value);
#else
	switch (value.type) {
		case VAL_BOOL:
			elox_printf(vmCtx, stream, AS_BOOL(value) ? "true" : "false");
			break;
		case VAL_NIL:
			ELOX_WRITE(vmCtx, stream, "nil");
			break;
		case VAL_NUMBER:
			printNumber(vmCtx, stream, AS_NUMBER(value));
			break;
		case VAL_OBJ:
			printValueObject(vmCtx, stream, value);
			break;
		case VAL_EXCEPTION:
			ELOX_WRITE(vmCtx, stream, "exception");
			break;
		case VAL_UNDEFINED:
			ELOX_WRITE(vmCtx, stream, "undefined");
			break;
	 }
#endif // ELOX_ENABLE_NAN_BOXING
}

static uint32_t instanceHash(ObjInstance *instance, Error *error) {
	VMCtx *vmCtx = error->vmCtx;
	VM *vm = &vmCtx->vm;

	if (instance->flags & INST_HAS_HASHCODE) {
		ObjClass *clazz = instance->clazz;
		ObjBoundMethod *boundHashCode = newBoundMethod(vmCtx, OBJ_VAL(instance),
													   AS_OBJ(clazz->hashCode));
		push(vm, OBJ_VAL(boundHashCode));
		Value hash = doCall(vmCtx, 0);
		if (ELOX_UNLIKELY(IS_EXCEPTION(hash))) {
			error->raised = true;
			return 0;
		}

		pop(vm);
		return AS_NUMBER(hash);
	}
	return instance->identityHash;
}

static bool instanceEquals(ObjInstance *ai, ObjInstance *bi, Error *error) {
	VMCtx *vmCtx = error->vmCtx;
	VM *vm = &vmCtx->vm;

	if (ai->flags & INST_HAS_EQUALS) {
		if (ai->clazz != bi->clazz)
			return false;
		ObjBoundMethod *boundEquals = newBoundMethod(vmCtx, OBJ_VAL(ai),
													 AS_OBJ(ai->clazz->equals));
		push(vm, OBJ_VAL(boundEquals));
		push(vm, OBJ_VAL(bi));
		Value equals = doCall(vmCtx, 1);
		if (ELOX_UNLIKELY(IS_EXCEPTION(equals))) {
			error->raised = true;
			return false;
		}

		pop(vm);
		return AS_BOOL(equals);
	}
	return ai == bi;
}

bool valuesEqual(Value a, Value b) {
#ifdef ELOX_ENABLE_NAN_BOXING
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
#endif // ELOX_ENABLE_NAN_BOXING
}

typedef union {
	double d;
	uint32_t u32[2];
} DoubleHash;

#ifdef ELOX_ENABLE_NAN_BOXING

uint32_t hashValue(Value value, Error *error) {
	if (IS_OBJ(value)) {
		Obj *obj = AS_OBJ(value);
		switch (obj->type) {
			case OBJ_STRING:
				return ((ObjString *)obj)->hash;
			case OBJ_STRINGPAIR:
				return ((ObjStringPair *)obj)->hash;
			case OBJ_INSTANCE:
				return instanceHash((ObjInstance *)obj, error);
			default:
				return 0;
		}
	} else if (IS_BOOL(value))
		return AS_BOOL(value);
	else if (IS_NUMBER(value)) {
		DoubleHash dh = { .d = AS_NUMBER(value) };
		if (dh.d == 0)
			return 0;
		return dh.u32[0] ^ dh.u32[1];
	} else
		return 0;
}

bool valuesEquals(const Value a, const Value b, Error *error) {
	if (IS_STRING(a) && IS_STRING(b)) {
		ObjString *as = AS_STRING(a);
		ObjString *bs = AS_STRING(b);
		return as == bs;
	} else if (IS_NUMBER(a) && IS_NUMBER(b))
		return AS_NUMBER(a) == AS_NUMBER(b);
	else if (IS_OBJ(a) && IS_OBJ(b)) {
		Obj *ao = AS_OBJ(a);
		Obj *bo = AS_OBJ(b);
		if (ao->type != bo->type)
			return false;
		switch (ao->type) {
			case OBJ_INSTANCE:
				return instanceEquals((ObjInstance *)ao, (ObjInstance *)bo, error);
			case OBJ_STRINGPAIR: {
				ObjStringPair *pair1 = (ObjStringPair *)ao;
				ObjStringPair *pair2 = (ObjStringPair *)bo;
				return (pair1->str1 == pair2->str1) && (pair1->str2 == pair2->str2);
			}
			default:
				return ao == bo;
		}
	} else
		return false;
}

#else

uint32_t hashValue(Value value, Error *error) {
	switch (value.type) {
		case VAL_OBJ: {
			Obj *obj = AS_OBJ(value);
			switch (obj->type) {
				case OBJ_STRING:
					return ((ObjString *)obj)->hash;
				case OBJ_STRINGPAIR:
					return ((ObjStringPair *)obj)->hash;
				case OBJ_INSTANCE:
					return instanceHash((ObjInstance *)obj, error);
				default:
					return 0;
			}
			break;
		}
		case VAL_BOOL:
			return AS_BOOL(value);
		case VAL_NUMBER: {
			DoubleHash dh = { .d = AS_NUMBER(value) };
			if (dh.d == 0)
				return 0;
			return dh.u32[0] ^ dh.u32[1];
		}
		default:
			return 0;
	}
}

bool valuesEquals(const Value a, const Value b, Error *error) {
	if (a.type != b.type)
		return false;

	switch (a.type) {
		case VAL_BOOL:
			return AS_BOOL(a) == AS_BOOL(b);
		case VAL_NIL:
			return b.type == VAL_NIL;
		case VAL_NUMBER:
			return AS_NUMBER(a) == AS_NUMBER(b);
		case VAL_OBJ: {
			Obj *ao = AS_OBJ(a);
			Obj *bo = AS_OBJ(b);
			if (ao->type != bo->type)
				return false;
			switch (ao->type) {
				case OBJ_INSTANCE:
					return instanceEquals((ObjInstance *)ao, (ObjInstance *)bo, error);
				case OBJ_STRINGPAIR: {
					ObjStringPair *pair1 = (ObjStringPair *)ao;
					ObjStringPair *pair2 = (ObjStringPair *)bo;
					return (pair1->str1 == pair2->str1) && (pair1->str2 == pair2->str2);
				}
				default:
					return ao == bo;
			}
		}
		default:
			return false; // Unreachable.
	}
}
#endif // ELOX_ENABLE_NAN_BOXING
