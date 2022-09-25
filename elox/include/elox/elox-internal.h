#ifndef ELOX_ELOX_INTERNAL_H
#define ELOX_ELOX_INTERNAL_H

#include "elox.h"
#include "elox/value.h"

typedef enum {
	CALLABLE_HANDLE
} EloxHandleType;

typedef struct EloxHandle {
	EloxHandleType type;
	Value value;
} EloxHandle;

typedef struct EloxCallableHandle {
	// value is the callable instance
	EloxHandle handle;
	uint16_t fixedArgs;
	uint16_t maxArgs;
} EloxCallableHandle;

void markHandle(VMCtx *vmCtx, EloxHandle *handle);

#endif // ELOX_ELOX_INTERNAL_H
