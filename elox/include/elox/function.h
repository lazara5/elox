// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_FUNCTION_H
#define ELOX_FUNCTION_H

#include <elox/value.h>

#define FRAMES_MAX 64
#define MIN_STACK (FRAMES_MAX * UINT8_COUNT)
#define MAX_CATCH_HANDLER_FRAMES 16

typedef struct {
	uint16_t handlerDataOffset;
	uint16_t stackOffset;
	uint16_t tmpStackOffset;
	bool caught;
} TryBlock;

typedef struct CallFrame {
	Obj *function;
	uint8_t *ip;
	Value *slots;
	uint8_t fixedArgs;
	uint8_t varArgs;
	uint8_t handlerCount;
	TryBlock handlerStack[MAX_CATCH_HANDLER_FRAMES];
} CallFrame;

typedef struct Args {
	VMCtx *vmCtx;
	int count;
	CallFrame *frame;
} Args;

void setValueArg(Args *args, int i, Value val);
Value getValueArg(Args *args, int i);

#endif // ELOX_FUNCTION_H
