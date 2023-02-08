// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_ELOX_INTERNAL_H
#define ELOX_ELOX_INTERNAL_H

#include <elox.h>
#include <elox/value.h>

typedef enum {
	CALLABLE_HANDLE
} EloxHandleType;

typedef struct EloxHandle {
	struct EloxHandle *next;
	struct EloxHandle *prev;
	EloxHandleType type;
	Value value;
} EloxHandle;

typedef struct EloxCallableHandle {
	// value is the callable instance
	EloxHandle handle;
	uint16_t fixedArgs;
	uint16_t maxArgs;
} EloxCallableHandle;

typedef struct {
	size_t handleSize;
} EloxHandleDesc;

static const EloxHandleDesc EloxHandleRegistry[] = {
	[CALLABLE_HANDLE] = { .handleSize = sizeof(EloxCallableHandle) }
};

void markHandle(VMCtx *vmCtx, EloxHandle *handle);

#endif // ELOX_ELOX_INTERNAL_H
