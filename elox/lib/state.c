// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/state.h"
#include "elox/builtins.h"

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

	vmCtx->env.free(vm->compilerStack, vmCtx->env.allocatorUserData);
}
