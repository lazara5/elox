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

	RunCtx *runCtx = handle->runCtx;
	const EloxHandleDesc *desc = &EloxHandleRegistry[handle->type];

	if (desc->destroy != NULL)
		desc->destroy(handle);

	handleSetRemove(runCtx, handle);
}

EloxRunCtxHandle *eloxNewRunCtx(EloxVMCtx *vmCtx) {
	RunCtx localRunCtx = {
		.vm = &vmCtx->vmInstance,
		.vmEnv = &vmCtx->env
	};
	RunCtx *runCtx = &localRunCtx;
	VM *vm = runCtx->vm;

	EloxRunCtxHandle *handle = ALLOCATE(runCtx, EloxRunCtxHandle, 1);
	if (ELOX_UNLIKELY(handle == NULL))
		return NULL;

	runCtx = &handle->runCtx;

	runCtx->vm = localRunCtx.vm;
	runCtx->vmEnv = localRunCtx.vmEnv;

	runCtx->activeFiber = newFiberCtx(runCtx);
	if (ELOX_UNLIKELY(runCtx->activeFiber == NULL)) {
		FREE(&localRunCtx, EloxHandle, handle);
		return NULL;
	}

	handle->base.runCtx = runCtx;
	handle->base.type = RUN_CTX_HANDLE;

	handleSetAdd(&vm->handles, (EloxHandle *)handle);

	return handle;
}

void markRunCtxHandle(EloxHandle *handle) {
	EloxRunCtxHandle *hnd = (EloxRunCtxHandle *)handle;
	markFiberCtx(hnd->base.runCtx, hnd->runCtx.activeFiber);
}

void destroyRunCtxHandle(EloxHandle *handle) {
	RunCtx *runCtx = handle->runCtx;
	EloxRunCtxHandle *hnd = (EloxRunCtxHandle *)handle;
	FiberCtx *fiber = hnd->runCtx.activeFiber;
	FREE_ARRAY(runCtx, Value, fiber->stack, fiber->stackCapacity);
	FREE(runCtx, FiberCtx,fiber);
}

EloxCallableHandle *eloxGetFunction(EloxRunCtxHandle *runHandle, const char *name, const char *module) {
	RunCtx *runCtx = runHandle->base.runCtx;
	VM *vm = runCtx->vm;

	if (module == NULL)
		module = eloxMainModuleName;

	String moduleStr = { .chars = (const uint8_t *)module, .length = strlen(module) };
	String nameStr = { .chars = (const uint8_t *)name, .length = strlen(name) };

	uint16_t id = globalIdentifierConstant(runCtx, &nameStr, &moduleStr);
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

	EloxCallableHandle *handle = ALLOCATE(runCtx, EloxCallableHandle, 1);
	// TODO: proper error handling
	if (handle == NULL)
		return NULL;
	handle->base.runCtx = runCtx;
	handle->base.type = CALLABLE_HANDLE;
	handle->callable = value;
	handle->fixedArgs = fixedArgs;
	handle->maxArgs = maxArgs;

	handleSetAdd(&vm->handles, (EloxHandle *)handle);

	return handle;
}

void markCallableHandle(EloxHandle *handle) {
	EloxCallableHandle *hnd = (EloxCallableHandle *)handle;
	markValue(hnd->base.runCtx, hnd->callable);
}

EloxCallableInfo eloxPrepareCall(EloxCallableHandle *callableHandle) {
	RunCtx *runCtx = callableHandle->base.runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	push(fiber, callableHandle->callable);
	return (EloxCallableInfo){ .runCtx = runCtx, .numArgs = 0, .maxArgs = callableHandle->maxArgs };
}

EloxInterpretResult eloxCall(const EloxCallableInfo *callableInfo) {
	RunCtx *runCtx = callableInfo->runCtx;
	FiberCtx *fiber = runCtx->activeFiber;

	Value res = runCall(runCtx, callableInfo->numArgs);
	pop(fiber); // discard result
	if (ELOX_UNLIKELY(IS_EXCEPTION(res)))
		return ELOX_INTERPRET_RUNTIME_ERROR;
	else
		return ELOX_INTERPRET_OK;
}

void eloxPushDouble(EloxCallableInfo *callableInfo, double val) {
	FiberCtx *fiber = callableInfo->runCtx->activeFiber;

	if (ELOX_LIKELY(callableInfo->numArgs < callableInfo->maxArgs)) {
		push(fiber, NUMBER_VAL(val));
		callableInfo->numArgs++;
	}
}

double eloxGetResultDouble(EloxCallableInfo *callableInfo) {
	FiberCtx *fiber = callableInfo->runCtx->activeFiber;

	// result is just above the top of the stack
	Value *res = fiber->stackTop;
	assert(IS_NUMBER(*res));
	return AS_NUMBER(*res);
}

const char *eloxGetResultString(EloxCallableInfo *callableInfo) {
	FiberCtx *fiber = callableInfo->runCtx->activeFiber;

	// result is just above the top of the stack
	Value *res = fiber->stackTop;
	assert(IS_STRING(*res));
	return AS_CSTRING(*res);
}
