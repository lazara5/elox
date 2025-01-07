// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/state.h>
#include <elox/builtins.h>

#include <string.h>

static bool initVM(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vmInstance;
	bool ret = false;

	vm->currentCompilerState = NULL;

	vm->handles.head = NULL;

	vm->grayOverflow = false;
	vm->grayCount = 0;
	vm->grayCapacity = 0;
	vm->grayStack = NULL;

	initTable(&vm->strings);

	vm->mainHeap.objects = NULL;
	vm->mainHeap.initialMarkers = 0;
	vm->permHeap.objects = NULL;
	vm->permHeap.initialMarkers = MARKER_BLACK;
	vm->heap = &vm->mainHeap;
	vm->bytesAllocated = 0;
	vm->nextGC = 1024 * 1024;

	vm->initFiber = NULL;
	initValueTable(&vm->globalNames);
	initValueArray(&vm->globalValues);

	RunCtx runCtx = {
		.vm = vm,
		.vmEnv = &vmCtx->env
	};

	vm->initFiber = newFiberCtx(&runCtx);
	if (ELOX_UNLIKELY(vm->initFiber == NULL))
		goto cleanup;
	runCtx.activeFiber = vm->initFiber;

	vm->freeFrames = NULL;
	for (int i = 0; i < ELOX_PREALLOC_CALL_FRAMES; i++) {
		CallFrame *frame = ALLOCATE(&runCtx, CallFrame, 1);
		if (ELOX_UNLIKELY(frame == NULL))
			goto cleanup;
		frame->prev = vm->freeFrames;
		vm->freeFrames = frame;
	}

	vm->handlingException = 0;
	stc64_init(&vm->prng, 64);

	initValueArray(&vm->builtinValues);

	initTable(&vm->modules);
	initTable(&vm->builtinSymbols);

	clearBuiltins(vm);

	vm->heap = &vm->permHeap;
	bool ok = registerBuiltins(&runCtx);
	vm->heap = &vm->mainHeap;
	if (!ok)
		goto cleanup;

	memset(vm->classes, 0, sizeof(vm->classes));
	vm->classes[VTYPE_BOOL] = vm->builtins.biBool._class;
	vm->classes[VTYPE_NUMBER] = vm->builtins.biNumber._class;
	vm->classes[VTYPE_OBJ_STRING] = vm->builtins.biString._class;
	vm->classes[VTYPE_OBJ_CLASS] = vm->builtins.biClass._class;
	vm->classes[VTYPE_OBJ_INSTANCE] = vm->builtins.biInstance._class;
	vm->classes[VTYPE_OBJ_ARRAY] = vm->builtins.biArray._class;
	vm->classes[VTYPE_OBJ_TUPLE] = vm->builtins.biTuple._class;
	vm->classes[VTYPE_OBJ_HASHMAP] = vm->builtins.biHashMap._class;

	ok = initHandleSet(&runCtx, &vm->handles);
	if (!ok)
		goto cleanup;

	ret = true;

cleanup:
	destroyFiberCtx(&runCtx, vm->initFiber);
	vm->initFiber = NULL;

	return ret;
}

EloxVMCtx *eloxNewVMCtx(const EloxConfig *config) {
	VMCtx *vmCtx = config->allocator.realloc(NULL, sizeof(VMCtx), config->allocator.userData);
	if (ELOX_UNLIKELY(vmCtx == NULL))
		return NULL;

	vmCtx->env.realloc = config->allocator.realloc;
	vmCtx->env.free = config->allocator.free;
	vmCtx->env.allocatorUserData = config->allocator.userData;

	vmCtx->env.write = config->writeCallback;
	vmCtx->env.loaders = config->moduleLoaders;

	if (!initVM(vmCtx)) {
		eloxDestroyVMCtx(vmCtx);
		config->allocator.free(vmCtx, config->allocator.userData);
		return NULL;
	}

	return vmCtx;
}

void eloxDestroyVMCtx(EloxVMCtx *vmCtx) {
	if (vmCtx == NULL)
		return;

	VM *vm = &vmCtx->vmInstance;

	RunCtx runCtx = {
		.vm = vm,
		.vmEnv = &vmCtx->env
	};

	freeValueTable(&runCtx, &vm->globalNames);
	freeValueArray(&runCtx, &vm->globalValues);
	freeTable(&runCtx, &vm->builtinSymbols);
	freeTable(&runCtx, &vm->modules);
	freeHandleSet(&runCtx, &vm->handles);
	freeTable(&runCtx, &vm->strings);

	freeValueArray(&runCtx, &vm->builtinValues);

	clearBuiltins(vm);
	freeObjects(&runCtx);

	CallFrame *frame = vm->freeFrames;
	while (frame != NULL) {
		CallFrame *prevFrame = frame->prev;
		FREE(&runCtx, CallFrame, frame);
		frame = prevFrame;
	}
}
