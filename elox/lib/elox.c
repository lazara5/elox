#include "elox/elox-internal.h"
#include "elox/state.h"

#include <stdio.h>
#include <assert.h>

static void defaultWriteCallback(EloxIOStream stream, const char *data, uint32_t len) {
	FILE *outputStream = (stream == ELOX_IO_OUT ? stdout : stderr);
	fprintf(outputStream, "%.*s", len, data);
}

void eloxInitConfig(EloxConfig *config) {
	config->writeCallback = defaultWriteCallback;
}

void markHandle(VMCtx *vmCtx, EloxHandle *handle) {
	markValue(vmCtx, handle->value);
}

EloxCallableHandle *eloxGetFunction(EloxVM *vmCtx, const char *name, const char *module) {
	VM *vm = &vmCtx->vm;

	if (module == NULL)
		module = eloxMainModuleName;

	String moduleStr = { .chars = (const uint8_t *)module, .length = strlen(module) };
	String nameStr = { .chars = (const uint8_t *)name, .length = strlen(name) };

	uint16_t id = globalIdentifierConstant(vmCtx, &nameStr, &moduleStr);
	Value value = vm->globalValues.values[id];

	if (!IS_OBJ(value))
		return NULL;

	uint16_t fixedArgs = 0;
	uint16_t maxArgs = 0;
	Obj *objVal = AS_OBJ(value);
	switch (objVal->type) {
		case OBJ_FUNCTION: {
			ObjFunction *function = (ObjFunction *)objVal;
			fixedArgs = function->arity;
			maxArgs = function->maxArgs;
			break;
		}
		case OBJ_CLOSURE: {
			ObjClosure *closure = (ObjClosure *)objVal;
			fixedArgs = closure->function->arity;
			maxArgs = closure->function->maxArgs;
			break;
		}
		default:
			return NULL;
	}

	EloxCallableHandle *handle = ALLOCATE(vmCtx, EloxCallableHandle, 1);
	if (handle == NULL)
		return NULL;
	handle->handle.type = CALLABLE_HANDLE;
	handle->handle.value = value;
	handle->fixedArgs = fixedArgs;
	handle->maxArgs = maxArgs;

	handleSetAdd(vmCtx, &vm->handles, (EloxHandle *)handle);

	return handle;
}

EloxCallableInfo eloxPrepareCall(EloxVM *vmCtx, EloxCallableHandle *handle,
								 int16_t numArgs) {
	VM *vm = &vmCtx->vm;

	push(vm, handle->handle.value);
	if (numArgs < 0) {
		pushn(vm, handle->fixedArgs);
		return (EloxCallableInfo){ .vmCtx = vmCtx, .numArgs = handle->fixedArgs, .discardArgs = 0 };
	} else {
		if (numArgs <= handle->fixedArgs) {
			pushn(vm, numArgs);
			int missingArgs = handle->fixedArgs - numArgs;
			for (int i = 0; i < missingArgs; i++)
				push(vm, NIL_VAL);
			return (EloxCallableInfo){ .vmCtx = vmCtx, .numArgs = numArgs, .discardArgs = 0 };
		} else {
			pushn(vm, numArgs);
			uint16_t discardArgs = (numArgs > handle->maxArgs) ?
								   numArgs - handle->maxArgs : 0;
			return (EloxCallableInfo){ .vmCtx = vmCtx,
									   .numArgs = numArgs, .discardArgs = discardArgs };
		}
	}
}

EloxInterpretResult eloxCall(EloxVM *vmCtx, const EloxCallableInfo *callableInfo) {
	VM *vm = &vmCtx->vm;

	popn(vm, callableInfo->discardArgs);
	Value res = doCall(vmCtx, callableInfo->numArgs - callableInfo->discardArgs);
	/*if (ELOX_UNLIKELY(IS_EXCEPTION(res))) {
		popn(vm, 2);
		return ELOX_INTERPRET_RUNTIME_ERROR;
	} else {
		popn(vm, 2);
		return ELOX_INTERPRET_OK;
	}*/
	pop(vm); // discard result
	if (ELOX_UNLIKELY(IS_EXCEPTION(res)))
		return ELOX_INTERPRET_RUNTIME_ERROR;
	else
		return ELOX_INTERPRET_OK;
}

#define VALIDATE_SLOT(CI, SLOT) assert((SLOT) < (CI)->numArgs)

void eloxSetSlotDouble(EloxCallableInfo *callableInfo, uint16_t slot, double val) {
	VM *vm = &callableInfo->vmCtx->vm;

	VALIDATE_SLOT(callableInfo, slot);
	*(vm->stackTop - callableInfo->numArgs + slot) = NUMBER_VAL(val);
}

double eloxGetResultDouble(EloxCallableInfo *callableInfo) {
	VM *vm = &callableInfo->vmCtx->vm;

	// result is just above the top of the stack
	Value *res = vm->stackTop;
	assert(IS_NUMBER(*res));
	return AS_NUMBER(*res);
}

const char *eloxGetResultString(EloxCallableInfo *callableInfo) {
	VM *vm = &callableInfo->vmCtx->vm;

	// result is just above the top of the stack
	Value *res = vm->stackTop;
	assert(IS_STRING(*res));
	return AS_CSTRING(*res);
}
