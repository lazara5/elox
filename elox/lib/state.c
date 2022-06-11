#include <stdlib.h>

#include "elox/state.h"

static void *defaultRealloc(void *oldPtr, size_t newSize, void *userData ELOX_UNUSED) {
	return realloc(oldPtr, newSize);
}

static void defaultFree(void *ptr, void *userData ELOX_UNUSED) {
	free(ptr);
}

void initVMCtx(VMCtx *vmCtx) {
	vmCtx->realloc = defaultRealloc;
	vmCtx->free = defaultFree;
	vmCtx->allocatorUserdata = NULL;

	initVM(vmCtx);
}
