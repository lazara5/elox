// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/state.h>
#include <elox/builtins.h>

#include <string.h>

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
