// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/object.h>
#include <elox/value.h>
#include <elox/state.h>

#include <math.h>
#include <inttypes.h>

static void printNumber(RunCtx *runCtx, EloxIOStream stream, double n) {
	if (trunc(n) == n)
		eloxPrintf(runCtx, stream, "%" PRId64, (int64_t)n);
	else
		eloxPrintf(runCtx, stream, "%g", n);
}

void printValue(RunCtx *runCtx, EloxIOStream stream, Value value) {
#ifdef ELOX_ENABLE_NAN_BOXING
	if (IS_BOOL(value))
		eloxPrintf(runCtx, stream, AS_BOOL(value) ? "true" : "false");
	else if (IS_NIL(value))
		ELOX_WRITE(runCtx, stream, "nil");
	else if (IS_NUMBER(value))
		printNumber(runCtx, stream, AS_NUMBER(value));
	else if (IS_OBJ(value))
		printValueObject(runCtx, stream, value);
#else
	switch (value.type) {
		case VAL_BOOL:
			eloxPrintf(runCtx, stream, AS_BOOL(value) ? "true" : "false");
			break;
		case VAL_NIL:
			ELOX_WRITE(runCtx, stream, "nil");
			break;
		case VAL_NUMBER:
			printNumber(runCtx, stream, AS_NUMBER(value));
			break;
		case VAL_OBJ:
			printValueObject(runCtx, stream, value);
			break;
		case VAL_EXCEPTION:
			ELOX_WRITE(runCtx, stream, "exception");
			break;
		case VAL_UNDEFINED:
			ELOX_WRITE(runCtx, stream, "undefined");
			break;
	 }
#endif // ELOX_ENABLE_NAN_BOXING
}

static uint32_t instanceHash(RunCtx *runCtx, ObjInstance *instance, EloxError *error) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	if (instance->flags & INST_HAS_HASHCODE) {
		ObjClass *clazz = instance->clazz;
		ObjBoundMethod *boundHashCode = newBoundMethod(runCtx, OBJ_VAL(instance),
													   clazz->hashCode);
		if (ELOX_UNLIKELY(boundHashCode == NULL)) {
			push(fiber, OBJ_VAL(vm->builtins.oomError));
			error->raised = true;
			return 0;
		}
		push(fiber, OBJ_VAL(boundHashCode));
		Value hash = runCall(runCtx, 0);
		if (ELOX_UNLIKELY(IS_EXCEPTION(hash))) {
			error->raised = true;
			return 0;
		}

		pop(fiber);
		return AS_NUMBER(hash);
	}
	return instance->identityHash;
}

static bool instanceEquals(RunCtx *runCtx, ObjInstance *ai, ObjInstance *bi, EloxError *error) {
	VM *vm = runCtx->vm;
	FiberCtx *fiber = runCtx->activeFiber;

	if (ai->flags & INST_HAS_EQUALS) {
		if (ai->clazz != bi->clazz)
			return false;
		ObjBoundMethod *boundEquals = newBoundMethod(runCtx, OBJ_VAL(ai),
													 ai->clazz->equals);
		if (ELOX_UNLIKELY(boundEquals == NULL)) {
			push(fiber, OBJ_VAL(vm->builtins.oomError));
			error->raised = true;
			return 0;
		}
		push(fiber, OBJ_VAL(boundEquals));
		push(fiber, OBJ_VAL(bi));
		Value equals = runCall(runCtx, 1);
		if (ELOX_UNLIKELY(IS_EXCEPTION(equals))) {
			error->raised = true;
			return false;
		}

		pop(fiber);
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

uint32_t hashValue(RunCtx *runCtx, Value value, EloxError *error) {
	if (IS_OBJ(value)) {
		Obj *obj = AS_OBJ(value);
		switch (obj->type) {
			case OBJ_STRING:
				return ((ObjString *)obj)->hash;
			case OBJ_STRINGPAIR:
				return ((ObjStringPair *)obj)->hash;
			case OBJ_INSTANCE:
				return instanceHash(runCtx, (ObjInstance *)obj, error);
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

bool valuesEquals(RunCtx *runCtx, const Value a, const Value b, EloxError *error) {
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
				return instanceEquals(runCtx, (ObjInstance *)ao, (ObjInstance *)bo, error);
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

uint32_t hashValue(RunCtx *runCtx, Value value, EloxError *error) {
	switch (value.type) {
		case VAL_OBJ: {
			Obj *obj = AS_OBJ(value);
			switch (obj->type) {
				case OBJ_STRING:
					return ((ObjString *)obj)->hash;
				case OBJ_STRINGPAIR:
					return ((ObjStringPair *)obj)->hash;
				case OBJ_INSTANCE:
					return instanceHash(runCtx, (ObjInstance *)obj, error);
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

bool valuesEquals(RunCtx *runCtx, const Value a, const Value b, EloxError *error) {
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
					return instanceEquals(runCtx, (ObjInstance *)ao, (ObjInstance *)bo, error);
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
