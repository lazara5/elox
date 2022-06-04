#include <stdlib.h>

#include "slox/state.h"

static void *defaultRealloc(void *oldPtr, size_t newSize, void *userData SLOX_UNUSED) {
	return realloc(oldPtr, newSize);
}

static void defaultFree(void *ptr, void *userData SLOX_UNUSED) {
	free(ptr);
}

void initVMCtx(VMCtx *vmCtx) {
	vmCtx->realloc = defaultRealloc;
	vmCtx->free = defaultFree;
	vmCtx->allocatorUserdata = NULL;

	initVM(vmCtx);
}
