// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_ELOX_INTERNAL_H
#define ELOX_ELOX_INTERNAL_H

#include <elox.h>
#include <elox/value.h>

typedef enum {
	CALLABLE_HANDLE,
	RUN_CTX_HANDLE
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

static const EloxHandleDesc EloxHandleRegistry[] = {
	[CALLABLE_HANDLE] = {
		.handleSize = sizeof(EloxCallableHandle),
		.mark = markCallableHandle
	},
	[RUN_CTX_HANDLE] = {
		.handleSize = sizeof(EloxRunCtxHandle),
		.mark = markRunCtxHandle,
		.destroy = destroyRunCtxHandle
	}
};

typedef EloxString String;

Value defaultModuleLoader(VMCtx *vmCtx, String *moduleName);

#endif // ELOX_ELOX_INTERNAL_H
