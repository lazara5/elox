#ifndef ELOX_FUNCTION_H
#define ELOX_FUNCTION_H

#include "elox/value.h"

#define FRAMES_MAX 64
#define MIN_STACK (FRAMES_MAX * UINT8_COUNT)
#define MAX_CATCH_HANDLER_FRAMES 16

typedef struct {
	uint16_t handlerTableOffset;
	uint16_t stackOffset;
} TryBlock;

typedef struct CallFrame {
	Obj *function;
	uint8_t *ip;
	Value *slots;
	uint8_t handlerCount;
	TryBlock handlerStack[MAX_CATCH_HANDLER_FRAMES];
} CallFrame;

typedef struct Args {
	CallFrame *frame;
} Args;

void setValueArg(Args *args, int i, Value val);
Value getValueArg(Args *args, int i);

#endif // ELOX_FUNCTION_H