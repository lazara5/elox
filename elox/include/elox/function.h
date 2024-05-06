// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_FUNCTION_H
#define ELOX_FUNCTION_H

#include <elox/value.h>
#include <elox/elox-config-internal.h>

typedef struct ObjFunction ObjFunction;
typedef struct ObjClosure ObjClosure;

#define MIN_STACK (ELOX_MAX_FRAMES * UINT8_COUNT)

typedef struct {
	uint16_t handlerDataOffset;
	uint16_t stackOffset;
	bool caught;
} TryBlock;

typedef struct CallFrame {
	ObjClosure *closure;
	ObjFunction *function;
	uint8_t *ip;
	Value *slots;
	uint8_t fixedArgs;
	uint8_t varArgs;
	uint8_t argOffset;
	uint8_t handlerCount;
	TryBlock handlerStack[ELOX_MAX_CATCH_HANDLER_FRAMES];
} CallFrame;

typedef struct Args {
	RunCtx *runCtx;
	int count;
	CallFrame *frame;
} Args;

void setValueArg(Args *args, int i, Value val);
Value getValueArg(Args *args, int i);

#endif // ELOX_FUNCTION_H
