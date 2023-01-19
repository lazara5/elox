#include <elox/state.h>

Value arrayIteratorHasNext(Args *args) {
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;
	struct ArrayIterator *ai = &vm->builtins.arrayIterator;

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
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;
	struct ArrayIterator *ai = &vm->builtins.arrayIterator;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjArray *array = AS_ARRAY(inst->fields.values[ai->_array]);
	int32_t i = AS_NUMBER(inst->fields.values[ai->_cursor]);
	uint32_t modCount = AS_NUMBER(inst->fields.values[ai->_modCount]);

	CHECK_MOD_RET(vmCtx, array, modCount);
	if (ELOX_UNLIKELY(i >= array->size))
		return runtimeError(vmCtx, "Array index out of bounds");

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
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;
	struct ArrayIterator *ai = &vm->builtins.arrayIterator;

	ObjInstance *inst = AS_INSTANCE(getValueArg(args, 0));
	ObjArray *array = AS_ARRAY(inst->fields.values[ai->_array]);
	int32_t lastRet = AS_NUMBER(inst->fields.values[ai->_lastRet]);

	if (ELOX_UNLIKELY(lastRet < 0))
		return runtimeError(vmCtx, "Illegal iterator state");

	uint32_t modCount = AS_NUMBER(inst->fields.values[ai->_modCount]);
	CHECK_MOD_RET(vmCtx, array, modCount);

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
	VMCtx *vmCtx = args->vmCtx;
	VM *vm = &vmCtx->vm;
	struct ArrayIterator *ai = &vm->builtins.arrayIterator;

	ObjArray *inst = AS_ARRAY(getValueArg(args, 0));

	ObjInstance *iter = newInstance(vmCtx, ai->_class);
	iter->fields.values[ai->_array] = OBJ_VAL(inst);
	iter->fields.values[ai->_cursor] = NUMBER_VAL(0);
	iter->fields.values[ai->_lastRet] = NUMBER_VAL(-1);
	iter->fields.values[ai->_modCount] = NUMBER_VAL(inst->modCount);
	return OBJ_VAL(iter);
}

Value arrayAdd(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	ObjArray *inst = AS_ARRAY(getValueArg(args, 0));
	Value val = getValueArg(args, 1);
	appendToArray(vmCtx, inst, val);

	return NIL_VAL;
}

Value arrayRemoveAt(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	ObjArray *inst = AS_ARRAY(getValueArg(args, 0));
	double indexArg;
	ELOX_GET_NUMBER_ARG_ELSE_RET(&indexArg, args, 1);

	int32_t index = indexArg;
	if (ELOX_UNLIKELY((index < 0) || (index >= inst->size)))
		return runtimeError(vmCtx, "Array index out of bounds");
	removeAt(inst, index);

	return NIL_VAL;
}

Value arraySlice(VMCtx *vmCtx, ObjArray *array, Value start, Value end) {
	int32_t sliceStart = AS_NUMBER(start);
	int32_t sliceEnd = AS_NUMBER(end);

	if (sliceStart < 0)
		sliceStart = 0;
	else if (sliceStart > array->size)
		sliceStart = array->size;

	if (sliceEnd < 0)
		sliceEnd = 0;

	if (sliceEnd < sliceStart)
		sliceEnd = sliceStart;
	else if (sliceStart > array->size)
		sliceEnd = array->size;

	int32_t sliceSize = sliceEnd - sliceStart;

	ObjArray *ret = newArray(vmCtx, sliceSize, OBJ_ARRAY);
	if (sliceSize > 0) {
		memcpy(ret->items, array->items + sliceStart, sliceSize * sizeof(Value));
		ret->size = sliceSize;
	}

	return OBJ_VAL(ret);
}
