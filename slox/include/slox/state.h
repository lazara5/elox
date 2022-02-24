#ifndef SLOX_STATE_H
#define SLOX_STATE_H

#include "slox/vm.h"
#include "slox/memory.h"
#include "slox/scanner.h"
#include "slox/compiler.h"

typedef struct VMCtx {
	VM vm;
	Scanner scanner;
	CompilerState compiler;

	SloxRealloc realloc;
	SloxFree free;
	void *allocatorUserdata;
} VMCtx;

void initVMCtx(VMCtx *vmCtx);

#endif
