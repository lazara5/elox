// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_ELOX_INTERNAL_H
#define ELOX_ELOX_INTERNAL_H

#include <elox.h>
#include <elox/value.h>
#include <elox/compiler.h>

typedef enum {
	CALLABLE_HANDLE,
	RUN_CTX_HANDLE,
	// Internal use
	COMPILER_HANDLE,
	KLASS_HANDLE
} EloxHandleType;

typedef struct EloxHandle {
	struct EloxHandle *next;
	struct EloxHandle *prev;
	EloxRunCtx *runCtx;
	EloxHandleType type;
} EloxHandle;

typedef struct EloxCallableHandle {
	EloxHandle base;

	Value callable;
	uint16_t fixedArgs;
	uint16_t maxArgs;
} EloxCallableHandle;

typedef struct EloxRunCtxHandle {
	EloxHandle base;

	EloxRunCtx runCtx;
} EloxRunCtxHandle;

typedef struct EloxCompilerHandle {
	EloxHandle base;

	CompilerState compilerState;
} EloxCompilerHandle;

typedef struct EloxKlassHandle {
	EloxHandle base;

	ObjKlass *klass;
} EloxKlassHandle;

typedef void (*MarkHandle)(EloxHandle *handle);
typedef void (*HandleDestructor)(EloxHandle *handle);

typedef struct {
	size_t handleSize;
	MarkHandle mark;
	HandleDestructor destroy;
} EloxHandleDesc;

void markCallableHandle(EloxHandle *handle);

void markRunCtxHandle(EloxHandle *handle);
void destroyRunCtxHandle(EloxHandle *handle);

void markKlassHandle(EloxHandle *handle);

static const EloxHandleDesc EloxHandleRegistry[] = {
	[CALLABLE_HANDLE] = {
		.handleSize = sizeof(EloxCallableHandle),
		.mark = markCallableHandle
	},
	[RUN_CTX_HANDLE] = {
		.handleSize = sizeof(EloxRunCtxHandle),
		.mark = markRunCtxHandle,
		.destroy = destroyRunCtxHandle
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

Value defaultModuleLoader(VMCtx *vmCtx, String *moduleName);

#endif // ELOX_ELOX_INTERNAL_H
