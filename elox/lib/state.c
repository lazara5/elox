// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/state.h>
#include <elox/builtins.h>

#include <string.h>

static bool initVM(VMInst *vmInst) {
	VM *vm = &vmInst->instance;

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

	vm->tmpFiber = NULL;
	initValueTable(&vm->globalNames);
	initValueArray(&vm->globalValues);

	VMCtx *vmCtx = &vmInst->vmCtx;
	vmCtx->vm = vm;
	vmCtx->vmEnv = &vmInst->env;
	RunCtx runCtx = {
		.vmCtx = vmCtx
	};

	bool ret = false;
	EloxMsgError errorMsg = ELOX_ERROR_MSG_INITIALIZER;

	vm->suspendedHead = &vm->suspendedHeadMarker;
	vm->suspendedHead->nextSuspended = vm->suspendedHead->prevSuspended = vm->suspendedHead;

	vm->tmpFiber = newFiber(&runCtx, NIL_VAL, (EloxError *)&errorMsg);
	if (ELOX_UNLIKELY(errorMsg.raised)) {
		eloxPrintf(vmCtx, ELOX_IO_ERR, "%s\n", errorMsg.msg);
		return false;
	}
	runCtx.activeFiber = vm->tmpFiber;

	vm->freeFrames = NULL;
	for (int i = 0; i < ELOX_PREALLOC_CALL_FRAMES; i++) {
		ObjCallFrame *frame = ALLOCATE(&runCtx, ObjCallFrame, 1);
		if (ELOX_UNLIKELY(frame == NULL))
			goto cleanup;
		setObjType(&frame->obj, OBJ_FRAME);
		setObjNext(&frame->obj, (Obj *)vm->freeFrames);
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
		eloxPrintf(vmCtx, ELOX_IO_ERR, "%s\n", errorMsg.msg);
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

	resetFiber(vmCtx, vm->tmpFiber);
	return ret;
}

EloxVMInst *eloxNewVMInst(const EloxConfig *config) {
	VMInst *vmInst = config->allocator.realloc(NULL, sizeof(VMInst), config->allocator.userData);
	if (ELOX_UNLIKELY(vmInst == NULL))
		return NULL;

	vmInst->env.realloc = config->allocator.realloc;
	vmInst->env.free = config->allocator.free;
	vmInst->env.allocatorUserData = config->allocator.userData;

	vmInst->env.write = config->writeCallback;
	vmInst->env.loaders = config->moduleLoaders;

	if (!initVM(vmInst)) {
		eloxDestroyVMInst(vmInst);
		//config->allocator.free(vmInst, config->allocator.userData);
		return NULL;
	}

	return vmInst;
}

void eloxDestroyVMInst(EloxVMInst *vmInst) {
	if (vmInst == NULL)
		return;

	VM *vm = &vmInst->instance;
	VMCtx *vmCtx = &vmInst->vmCtx;

	freeValueTable(vmCtx, &vm->globalNames);
	freeValueArray(vmCtx, &vm->globalValues);
	freeTable(vmCtx, &vm->builtinSymbols);
	freeTable(vmCtx, &vm->modules);
	freeHandleSet(vmCtx, &vm->handles);
	freeTable(vmCtx, &vm->strings);

	freeValueArray(vmCtx, &vm->builtinValues);

	clearBuiltins(vm);
	freeObjects(vmCtx);

	ObjCallFrame *frame = vm->freeFrames;
	while (frame != NULL) {
		ObjCallFrame *prevFrame = (ObjCallFrame *)getObjNext(&frame->obj);
		FREE(vmCtx, ObjCallFrame, frame);
		frame = prevFrame;
	}

	TryBlock *block = vm->freeTryBlocks;
	while (block != NULL) {
		TryBlock *prevBlock = block->prev;
		FREE(vmCtx, TryBlock, block);
		block = prevBlock;
	}

	vmInst->env.free(vmInst, vmInst->env.allocatorUserData);
}
