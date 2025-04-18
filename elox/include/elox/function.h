// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_FUNCTION_H
#define ELOX_FUNCTION_H

#include <elox/value.h>
#include <elox/elox-config-internal.h>

typedef struct ObjFunction ObjFunction;
typedef struct ObjClosure ObjClosure;

#define ELOX_STACK_MAX_FRAMES 64
#define MIN_STACK (ELOX_STACK_MAX_FRAMES * UINT8_COUNT)

typedef struct TryBlock {
	struct TryBlock *prev;
	uint16_t handlerDataOffset;
	uint16_t stackOffset;
	bool caught;
} TryBlock;

typedef enum {
	ELOX_FT_INTER = 0,
	ELOX_FT_INTERNAL_CALL_START,
	ELOX_FT_FIBER_START
} ELOX_PACKED FrameType;

/*typedef struct CallFrame {
	struct CallFrame *prev;
	ObjClosure *closure;
	ObjFunction *function;
	uint8_t *ip;
	Value *slots;
	FrameType type : 8;
	uint8_t fixedArgs;
	uint8_t varArgs;
	uint8_t argOffset;
	uint16_t stackArgs; // for native call frames only
	uint8_t tryDepth;
	TryBlock *tryStack;
} CallFrame;*/

typedef struct ObjCallFrame ObjCallFrame;

typedef struct Args {
	RunCtx *runCtx;
	int count;
	ObjCallFrame *frame;
} Args;

typedef struct ValueArray ValueArray;

void setValueArg(Args *args, int i, Value val);
Value getValueArg(Args *args, int i);
ValueArray getArgsFrom(Args *args, int i);

#endif // ELOX_FUNCTION_H
