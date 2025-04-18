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

	EloxMsgError errorMsg = ELOX_ERROR_MSG_INITIALIZER;

	vm->suspendedHead = NULL;
	vm->suspendedHead = ALLOCATE(&runCtx, ObjFiber, 1);
	if (ELOX_UNLIKELY(vm->suspendedHead == NULL)) {
		eloxPrintf(&runCtx, ELOX_IO_ERR, "Out of memory");
		return false;
	}
	vm->suspendedHead->nextSuspended = vm->suspendedHead->prevSuspended = vm->suspendedHead;

	vm->initFiber = newFiber(&runCtx, NIL_VAL, (EloxError *)&errorMsg);
	if (ELOX_UNLIKELY(errorMsg.raised)) {
		eloxPrintf(&runCtx, ELOX_IO_ERR, "%s\n", errorMsg.msg);
		goto cleanup;
	}
	runCtx.activeFiber = vm->initFiber;

	vm->freeFrames = NULL;
	for (int i = 0; i < ELOX_PREALLOC_CALL_FRAMES; i++) {
		ObjCallFrame *frame = ALLOCATE(&runCtx, ObjCallFrame, 1);
		if (ELOX_UNLIKELY(frame == NULL))
			goto cleanup;
		frame->obj.type = OBJ_FRAME;
		frame->obj.next = (Obj *)vm->freeFrames;
		vm->freeFrames = frame;
	}

	vm->freeTryBlocks = NULL;

	vm->handlingException = 0;
	stc64_init(&vm->prng, 64);

	initValueArray(&vm->builtinValues);

	initTable(&vm->modules);

	bool ok = initHandleSet(&runCtx, &vm->handles);
	if (!ok)
		goto cleanup;

	initTable(&vm->builtinSymbols);

	clearBuiltins(vm);

	vm->heap = &vm->permHeap;

	ok = registerBuiltins(&runCtx, &errorMsg);
	vm->heap = &vm->mainHeap;
	if (!ok) {
		eloxPrintf(&runCtx, ELOX_IO_ERR, "%s\n", errorMsg.msg);
		goto cleanup;
	}

	memset(vm->classes, 0, sizeof(vm->classes));
	vm->classes[VTYPE_BOOL] = vm->builtins.biBool.class_;
	vm->classes[VTYPE_NUMBER] = vm->builtins.biNumber.class_;
	vm->classes[VTYPE_OBJ_STRING] = vm->builtins.biString.class_;
	vm->classes[VTYPE_OBJ_CLASS] = vm->builtins.biClass.class_;
	vm->classes[VTYPE_OBJ_INSTANCE] = vm->builtins.biInstance.class_;
	vm->classes[VTYPE_OBJ_ARRAY] = vm->builtins.biArray.class_;
	vm->classes[VTYPE_OBJ_TUPLE] = vm->builtins.biTuple.class_;
	vm->classes[VTYPE_OBJ_HASHMAP] = vm->builtins.biHashMap.class_;
	vm->classes[VTYPE_OBJ_FRAME] = vm->builtins.biVarargs.class_;
	vm->classes[VTYPE_OBJ_FIBER] = vm->builtins.biFiber.class_;

	ret = true;

cleanup:
	//destroyFiber(&runCtx, vm->initFiber);
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
		//config->allocator.free(vmCtx, config->allocator.userData);
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

	ObjCallFrame *frame = vm->freeFrames;
	while (frame != NULL) {
		ObjCallFrame *prevFrame = (ObjCallFrame *)frame->obj.next;
		FREE(&runCtx, ObjCallFrame, frame);
		frame = prevFrame;
	}

	TryBlock *block = vm->freeTryBlocks;
	while (block != NULL) {
		TryBlock *prevBlock = block->prev;
		FREE(&runCtx, TryBlock, block);
		block = prevBlock;
	}

	FREE(&runCtx, ObjFiber, vm->suspendedHead);

	vmCtx->env.free(vmCtx, vmCtx->env.allocatorUserData);
}
