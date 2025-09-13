// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_ELOX_INTERNAL_H
#define ELOX_ELOX_INTERNAL_H

#include <elox.h>
#include <elox/value.h>
#include <elox/compiler.h>

typedef struct EloxVMCtx {
	EloxVM *vm;
	EloxVMEnv *vmEnv;
} EloxVMCtx;

typedef struct EloxRunCtx {
	EloxVMCtx *vmCtx;
	EloxFiber *activeFiber;
} EloxRunCtx;

typedef enum {
	CALLABLE_HANDLE,
	FIBER_HANDLE,
	// Internal use
	COMPILER_HANDLE,
	KLASS_HANDLE
} EloxHandleType;

typedef struct EloxHandle {
	struct EloxHandle *next;
	struct EloxHandle *prev;
	EloxVMCtx *vmCtx;
	EloxHandleType type;
} EloxHandle;

typedef struct EloxCallableHandle {
	EloxHandle base;

	Value callable;
	uint16_t fixedArgs;
	uint16_t maxArgs;
} EloxCallableHandle;

typedef struct EloxCallFrame {
	struct EloxFiberHandle *fiberHandle;
	uint32_t stackOffset;
} EloxCallFrame;

typedef struct EloxFiberHandle {
	EloxHandle base;

	EloxFiber *fiber;
	EloxRunCtx runCtx;

	EloxCallFrame frames[ELOX_MAX_C_CALL_DEPTH];
	uint8_t callDepth;
} EloxFiberHandle;

typedef struct EloxCompilerHandle {
	EloxHandle base;

	CompilerState compilerState;
} EloxCompilerHandle;

typedef struct EloxKlassHandle {
	EloxHandle base;

	ObjKlass *klass;
	RunCtx *runCtx;
} EloxKlassHandle;

typedef void (*MarkHandle)(EloxHandle *handle);
typedef void (*HandleDestructor)(EloxHandle *handle);

typedef struct {
	size_t handleSize;
	MarkHandle mark;
	HandleDestructor destroy;
} EloxHandleDesc;

void markCallableHandle(EloxHandle *handle);

void markFiberHandle(EloxHandle *handle);

void markKlassHandle(EloxHandle *handle);

static const EloxHandleDesc EloxHandleRegistry[] = {
	[CALLABLE_HANDLE] = {
		.handleSize = sizeof(EloxCallableHandle),
		.mark = markCallableHandle
	},
	[FIBER_HANDLE] = {
		.handleSize = sizeof(EloxFiberHandle),
		.mark = markFiberHandle,
	},
	[COMPILER_HANDLE] = {
		.handleSize = sizeof(EloxCompilerHandle),
		.mark = markCompilerHandle
	},
	[KLASS_HANDLE] = {
		.handleSize = sizeof(EloxKlassHandle),
		.mark = markKlassHandle
	}
};

typedef EloxString String;

Value defaultModuleLoader(VMInst *vmInst, String *moduleName);

#endif // ELOX_ELOX_INTERNAL_H
