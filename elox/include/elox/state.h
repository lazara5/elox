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
	void *allocatorUserData;

	EloxIOWrite write;
	EloxModuleLoader *loaders;
} VMCtx;

typedef struct CCtx {
	Scanner scanner;
	CompilerState compilerState;
	String moduleName;
	int moduleNameLength;
	VMCtx *vmCtx;
} CCtx;

bool initVMCtx(VMCtx *vmCtx, const EloxConfig *config);

typedef uintptr_t PHandle;
#define PHANDLE_INITIALIZER 0

static inline PHandle protectObj(Obj *obj) {
	if (obj) {
		bool protected = obj->markers & MARKER_TEMP;
		obj->markers |= MARKER_TEMP;
		return protected ? 0 : (uintptr_t)obj;
	}
	return 0;
}

static inline PHandle protectVal(Value value) {
	if (IS_OBJ(value)) {
		Obj *obj = AS_OBJ(value);
		bool protected = obj->markers & MARKER_TEMP;
		obj->markers |= MARKER_TEMP;
		return protected ? 0 : (uintptr_t)obj;
	}
	return 0;
}

static inline void unprotectObj(PHandle handle) {
	Obj *obj = (Obj *)handle;
	if (obj)
		obj->markers &= ~MARKER_TEMP;
}

#endif
