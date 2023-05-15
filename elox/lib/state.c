// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <stdlib.h>

#include "elox/state.h"

static void *defaultRealloc(void *oldPtr, size_t newSize, void *userData ELOX_UNUSED) {
	return realloc(oldPtr, newSize);
}

static void defaultFree(void *ptr, void *userData ELOX_UNUSED) {
	free(ptr);
}

void initVMCtx(VMCtx *vmCtx, const EloxConfig *config) {
	vmCtx->realloc = defaultRealloc;
	vmCtx->free = defaultFree;
	vmCtx->allocatorUserdata = NULL;

	vmCtx->write = config->writeCallback;
	vmCtx->loaders = config->moduleLoaders;

	initVM(vmCtx);
}
