// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/state.h>

#include <string.h>

Value arrayIteratorHasNext(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;

	struct BIArrayIterator *ai = &vm->builtins.biArrayIterator;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjArray *array = AS_ARRAY(inst->fields.values[ai->_array]);
	int32_t cursor = AS_NUMBER(inst->fields.values[ai->_cursor]);

	return BOOL_VAL(cursor != array->size);
}

#define CHECK_MOD_RET(vmCtx, array, modcount) \
{ \
	if (ELOX_UNLIKELY(modCount != array->modCount)) \
		return runtimeError(vmCtx, "Array modified during iteration"); \
}

Value arrayIteratorNext(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;

	struct BIArrayIterator *ai = &vm->builtins.biArrayIterator;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjArray *array = AS_ARRAY(inst->fields.values[ai->_array]);
	int32_t i = AS_NUMBER(inst->fields.values[ai->_cursor]);
	uint32_t modCount = AS_NUMBER(inst->fields.values[ai->_modCount]);

	CHECK_MOD_RET(runCtx, array, modCount);
	if (ELOX_UNLIKELY(i >= array->size))
		return runtimeError(runCtx, "Array index out of bounds");

	inst->fields.values[ai->_cursor] = NUMBER_VAL(i + 1);
	inst->fields.values[ai->_lastRet] = NUMBER_VAL(i);
	return array->items[i];
}

static void removeAt(ObjArray *array, int32_t index) {
	array->modCount++;
	memmove(array->items + index, array->items + index + 1,
			(array->size - index - 1) * sizeof(Value));
	array->size--;
}

Value arrayIteratorRemove(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;

	struct BIArrayIterator *ai = &vm->builtins.biArrayIterator;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjArray *array = AS_ARRAY(inst->fields.values[ai->_array]);
	int32_t lastRet = AS_NUMBER(inst->fields.values[ai->_lastRet]);

	if (ELOX_UNLIKELY(lastRet < 0))
		return runtimeError(runCtx, "Illegal iterator state");

	uint32_t modCount = AS_NUMBER(inst->fields.values[ai->_modCount]);
	CHECK_MOD_RET(runCtx, array, modCount);

	removeAt(array, lastRet);
	inst->fields.values[ai->_cursor] = NUMBER_VAL(lastRet);
	inst->fields.values[ai->_lastRet] = NUMBER_VAL(-1);
	inst->fields.values[ai->_modCount] = NUMBER_VAL(array->modCount);

	return NIL_VAL;
}

Value arrayLength(Args *args) {
	ObjArray *inst = AS_ARRAY(getValueArg(args, 0));
	return NUMBER_VAL(inst->size);
}

Value arrayIterator(Args *args) {
	RunCtx *runCtx = args->runCtx;
	VM *vm = runCtx->vm;
	struct BIArrayIterator *ai = &vm->builtins.biArrayIterator;

	ObjArray *inst = AS_ARRAY(getValueArg(args, 0));

	ObjInstance *iter = newInstance(runCtx, ai->_class);
	if (ELOX_UNLIKELY(iter == NULL))
		return oomError(runCtx);
	iter->fields.values[ai->_array] = OBJ_VAL(inst);
	iter->fields.values[ai->_cursor] = NUMBER_VAL(0);
	iter->fields.values[ai->_lastRet] = NUMBER_VAL(-1);
	iter->fields.values[ai->_modCount] = NUMBER_VAL(inst->modCount);
	return OBJ_VAL(iter);
}

Value arrayAdd(Args *args) {
	RunCtx *runCtx = args->runCtx;

	ObjArray *inst = AS_ARRAY(getValueArg(args, 0));
	Value val = getValueArg(args, 1);
	bool res = appendToArray(runCtx, inst, val);
	if (ELOX_UNLIKELY(!res))
		return oomError(runCtx);

	return NIL_VAL;
}

Value arrayRemoveAt(Args *args) {
	RunCtx *runCtx = args->runCtx;

	ObjArray *inst = AS_ARRAY(getValueArg(args, 0));
	double indexArg;
	ELOX_GET_NUMBER_ARG_ELSE_RET(&indexArg, args, 1);

	int32_t index = indexArg;
	if (ELOX_UNLIKELY((index < 0) || (index >= inst->size)))
		return runtimeError(runCtx, "Array index out of bounds");
	removeAt(inst, index);

	return NIL_VAL;
}

Value arraySlice(RunCtx *runCtx, ObjArray *array, ObjType type, Value start, Value end) {
	int32_t sliceStart;
	int32_t sliceEnd;

	if (ELOX_UNLIKELY(!computeSlice(start, end, array->size, &sliceStart, &sliceEnd)))
		return runtimeError(runCtx, "Slice start and end must be numbers");
	int32_t sliceSize = sliceEnd - sliceStart;

	ObjArray *ret = newArray(runCtx, sliceSize, type);
	if (sliceSize > 0) {
		memcpy(ret->items, array->items + sliceStart, sliceSize * sizeof(Value));
		ret->size = sliceSize;
	}

	return OBJ_VAL(ret);
}

bool arrayContains(RunCtx *runCtx, ObjArray *seq, const Value needle, EloxError *error) {
	for (int i = 0; i < seq->size; i++) {
		if (valuesEquals(runCtx, needle, seq->items[i], error))
			return true;
		if (ELOX_UNLIKELY(error->raised))
			return false;
	}
	return false;
}
