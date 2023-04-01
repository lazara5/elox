// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/elox-internal.h"
#include "elox/state.h"

#include <stdio.h>
#include <assert.h>
#include <string.h>

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

	handleSetAdd(&vm->handles, (EloxHandle *)handle);

	return handle;
}

EloxCallableInfo eloxPrepareCall(EloxVM *vmCtx, EloxCallableHandle *handle) {
	VM *vm = &vmCtx->vm;

	push(vm, handle->handle.value);
	return (EloxCallableInfo){ .vmCtx = vmCtx, .numArgs = 0, .maxArgs = handle->maxArgs };
}

EloxInterpretResult eloxCall(EloxVM *vmCtx, const EloxCallableInfo *callableInfo) {
	VM *vm = &vmCtx->vm;

	Value res = runCall(vmCtx, callableInfo->numArgs);
	pop(vm); // discard result
	if (ELOX_UNLIKELY(IS_EXCEPTION(res)))
		return ELOX_INTERPRET_RUNTIME_ERROR;
	else
		return ELOX_INTERPRET_OK;
}

void eloxPushDouble(EloxCallableInfo *callableInfo, double val) {
	VM *vm = &callableInfo->vmCtx->vm;

	if (ELOX_LIKELY(callableInfo->numArgs < callableInfo->maxArgs)) {
		push(vm, NUMBER_VAL(val));
		callableInfo->numArgs++;
	}
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
