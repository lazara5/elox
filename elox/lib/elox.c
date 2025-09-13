// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/elox-internal.h"
#include "elox/state.h"
#include "elox/vm.h"

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

static void *defaultRealloc(void *oldPtr, size_t newSize, void *userData ELOX_UNUSED) {
	return realloc(oldPtr, newSize);
}

static void defaultFree(void *ptr, void *userData ELOX_UNUSED) {
	free(ptr);
}

static void defaultWriteCallback(EloxIOStream stream, const char *data, uint32_t len) {
	FILE *outputStream = (stream == ELOX_IO_OUT ? stdout : stderr);
	fprintf(outputStream, "%.*s", len, data);
}

void eloxAPIErrorSet(EloxAPIError *error, const char *fmt, ...) {
	va_list arg_ptr;

	error->raised = true;

	va_start(arg_ptr, fmt);
	vsnprintf(error->msg, sizeof(error->msg), fmt, arg_ptr);
	va_end(arg_ptr);
}

void eloxInitConfig(EloxConfig *config) {
	config->allocator = (EloxAllocator){
		.realloc = defaultRealloc,
		.free = defaultFree,
		.userData = NULL
	};
	config->writeCallback = defaultWriteCallback;
	static EloxModuleLoader defaultLoaders[] = {
		{ .loader = eloxFileModuleLoader },
		{ .loader = eloxNativeModuleLoader },
		{ .loader = eloxBuiltinModuleLoader, .options = ELOX_BML_ENABLE_ALL },
		{ .loader = NULL }
	};
	config->moduleLoaders = defaultLoaders;
}

void eloxReleaseHandle(EloxHandle *handle) {
	if (handle == NULL)
		return;

	VMCtx *vmCtx = handle->vmCtx;
	const EloxHandleDesc *desc = &EloxHandleRegistry[handle->type];

	if (desc->destroy != NULL)
		desc->destroy(handle);

	handleSetRemove(vmCtx, handle);
}

EloxFiberHandle *eloxNewFiber(EloxVMInst *vmInst, EloxAPIError *error) {
	RunCtx localRunCtx = {
		.vmCtx = &vmInst->vmCtx,
		.activeFiber = NULL
	};
	RunCtx *runCtx = &localRunCtx;
	VM *vm = runCtx->vmCtx->vm;

	EloxFiberHandle *handle = ALLOCATE(runCtx, EloxFiberHandle, 1);
	if (ELOX_UNLIKELY(handle == NULL))
		return NULL;

	handle->runCtx.activeFiber = NULL;

	runCtx = &handle->runCtx;

	runCtx->vmCtx = &vmInst->vmCtx;

	EloxMsgError errorMsg = ELOX_ERROR_MSG_INITIALIZER;
	handle->fiber = newFiber(runCtx, NIL_VAL, (EloxError *)&errorMsg);
	if (ELOX_UNLIKELY(errorMsg.raised)) {
		eloxAPIErrorSet(error, errorMsg.msg);
		FREE(localRunCtx.vmCtx, EloxHandle, handle);
		return NULL;
	}

	handle->callDepth = 0;

	handle->base.vmCtx = runCtx->vmCtx;
	handle->base.type = FIBER_HANDLE;

	handleSetAdd(&vm->handles, (EloxHandle *)handle);

	return handle;
}

void markFiberHandle(EloxHandle *handle) {
	EloxFiberHandle *hnd = (EloxFiberHandle *)handle;
	markObject(hnd->base.vmCtx, (Obj *)hnd->fiber);
}

EloxCallableHandle *eloxGetFunction(EloxVMInst *vmInst,
									const char *name, const char *module) {
	VMCtx *vmCtx = &vmInst->vmCtx;
	VM *vm = vmCtx->vm;
	RunCtx runCtx = {
		.vmCtx = vmCtx,
		.activeFiber = vm->tmpFiber
	};

	if (module == NULL)
		module = eloxMainModuleName;

	String moduleStr = { .chars = (const uint8_t *)module, .length = strlen(module) };
	String nameStr = { .chars = (const uint8_t *)name, .length = strlen(name) };

	uint16_t id = globalIdentifierConstant(&runCtx, &nameStr, &moduleStr);
	Value value = vm->globalValues.values[id];

	EloxCallableHandle *handle = NULL;

	if (!IS_OBJ(value))
		goto cleanup;;

	uint16_t fixedArgs = 0;
	uint16_t maxArgs = 0;
	Obj *objVal = AS_OBJ(value);
	switch (getObjType(objVal)) {
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
			goto cleanup;
	}

	handle = ALLOCATE(&runCtx, EloxCallableHandle, 1);
	// TODO: proper error handling
	if (handle == NULL)
		goto cleanup;
	handle->base.vmCtx = vmCtx;
	handle->base.type = CALLABLE_HANDLE;
	handle->callable = value;
	handle->fixedArgs = fixedArgs;
	handle->maxArgs = maxArgs;

	handleSetAdd(&vm->handles, (EloxHandle *)handle);

cleanup:
	resetFiber(vmCtx, vm->tmpFiber);

	return handle;
}

void markCallableHandle(EloxHandle *handle) {
	EloxCallableHandle *hnd = (EloxCallableHandle *)handle;
	markValue(hnd->base.vmCtx, hnd->callable);
}

EloxCallFrame *eloxOpenCall(EloxFiberHandle *fiberHandle, EloxCallableHandle *callableHandle,
							EloxAPIError *error) {
	uint8_t callDepth = fiberHandle->callDepth;
	if (ELOX_UNLIKELY(callDepth > ELOX_MAX_C_CALL_DEPTH)) {
		eloxAPIErrorSet(error, "Max call stack depth exceeded");
		return NULL;
	}

	ObjFiber *fiber = fiberHandle->fiber;
	if (callDepth == 0)
		fiber->stackTop = fiber->stack; // discard any previous results
	EloxCallFrame *callFrame = &fiberHandle->frames[callDepth++];


	push(fiber, callableHandle->callable);
	callFrame->stackOffset = fiber->stackTop - fiber->stack;

	return callFrame;
}

EloxInterpretResult eloxCall(const EloxCallFrame *callFrame) {
	RunCtx *runCtx = &callFrame->fiberHandle->runCtx;
	ObjFiber *fiber = callFrame->fiberHandle->fiber;

	runCtx->activeFiber = fiber;

	Value res = runCall(runCtx, fiber->stackTop - fiber->stack - callFrame->stackOffset);
	pop(fiber); // discard result
	callFrame->fiberHandle->callDepth--;
	if (ELOX_UNLIKELY(IS_EXCEPTION(res)))
		return ELOX_INTERPRET_RUNTIME_ERROR;
	else
		return ELOX_INTERPRET_OK;
}

void eloxPushDouble(EloxCallFrame *callFrame, double val) {
	ObjFiber *fiber = callFrame->fiberHandle->fiber;

	push(fiber, NUMBER_VAL(val));
}

double eloxGetResultDouble(EloxCallFrame *callFrame) {
	ObjFiber *fiber = callFrame->fiberHandle->fiber;

	// result is just above the top of the stack
	Value *res = fiber->stackTop;
	assert(IS_NUMBER(*res));
	return AS_NUMBER(*res);
}

const char *eloxGetResultString(EloxCallFrame *callFrame) {
	ObjFiber *fiber = callFrame->fiberHandle->fiber;

	// result is just above the top of the stack
	Value *res = fiber->stackTop;
	assert(IS_STRING(*res));
	return AS_CSTRING(*res);
}
