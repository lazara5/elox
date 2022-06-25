#ifndef ELOX_STATE_H
#define ELOX_STATE_H

#include "elox/vm.h"
#include "elox/memory.h"
#include "elox/scanner.h"
#include "elox/compiler.h"

typedef struct VMCtx {
	VM vm;

	EloxRealloc realloc;
	EloxFree free;
	void *allocatorUserdata;
} VMCtx;

typedef struct CCtx {
	Scanner scanner;
	CompilerState compilerState;
	String moduleName;
	int moduleNameLength;
	VMCtx *vmCtx;
} CCtx;

void initVMCtx(VMCtx *vmCtx);

#endif
