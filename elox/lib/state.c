// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/state.h"

bool initVMCtx(VMCtx *vmCtx, const EloxConfig *config) {
	vmCtx->realloc = config->allocator.realloc;
	vmCtx->free = config->allocator.free;
	vmCtx->allocatorUserData = config->allocator.userData;

	vmCtx->write = config->writeCallback;
	vmCtx->loaders = config->moduleLoaders;

	return initVM(vmCtx);
}
