// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

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

	EloxIOWrite write;
} VMCtx;

typedef struct CCtx {
	Scanner scanner;
	CompilerState compilerState;
	String moduleName;
	int moduleNameLength;
	VMCtx *vmCtx;
} CCtx;

void initVMCtx(VMCtx *vmCtx, const EloxConfig *config);

static inline void pushTemp(VMCtx *vmCtx, Value value) {
	VM *vm = &vmCtx->vm;

	valueArrayPush(vmCtx, &vm->tmpStack, value);
}

static inline void popTemp(VMCtx *vmCtx) {
	VM *vm = &vmCtx->vm;

	valueArrayPop(&vm->tmpStack);
}


#endif
