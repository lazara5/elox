#ifndef SLOX_STATE_H
#define SLOX_STATE_H

#include "slox/vm.h"
#include "slox/memory.h"
#include "slox/scanner.h"
#include "slox/compiler.h"

typedef struct VMCtx {
	VM vm;

	SloxRealloc realloc;
	SloxFree free;
	void *allocatorUserdata;
} VMCtx;

typedef struct CCtx {
	Scanner scanner;
	CompilerState compilerState;
	VMCtx *vmCtx;
} CCtx;

void initVMCtx(VMCtx *vmCtx);

#endif
