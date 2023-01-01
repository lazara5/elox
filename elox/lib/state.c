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

	initVM(vmCtx);
}
