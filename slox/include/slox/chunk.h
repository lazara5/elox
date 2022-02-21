#ifndef SLOX_CHUNK_H
#define SLOX_CHUNK_H

#include "slox/common.h"
#include "slox/value.h"

typedef struct VMCtx VMCtx;

typedef enum {
	OP_CONSTANT,
	OP_NIL,
	OP_TRUE,
	OP_FALSE,
	OP_POP,
	OP_GET_LOCAL,
	OP_GET_GLOBAL,
	OP_DEFINE_GLOBAL,
	OP_SET_LOCAL,
	OP_SET_GLOBAL,
	OP_GET_UPVALUE,
	OP_SET_UPVALUE,
	OP_GET_PROPERTY,
	OP_SET_PROPERTY,
	OP_GET_SUPER,
	OP_EQUAL,
	OP_GREATER,
	OP_LESS,
	OP_ADD,
	OP_SUBTRACT,
	OP_MULTIPLY,
	OP_DIVIDE,
	OP_MODULO,
	OP_NOT,
	OP_NEGATE,
	OP_PRINT,
	OP_JUMP,
	OP_JUMP_IF_FALSE,
	OP_LOOP,
	OP_CALL,
	OP_INVOKE,
	OP_SUPER_INVOKE,
	OP_CLOSURE,
	OP_CLOSE_UPVALUE,
	OP_RETURN,
	OP_CLASS,
	OP_INHERIT,
	OP_METHOD,
	OP_ARRAY_BUILD,
	OP_ARRAY_INDEX,
	OP_ARRAY_STORE,
	OP_THROW,
	OP_PUSH_EXCEPTION_HANDLER,
	OP_POP_EXCEPTION_HANDLER
} OpCode;

typedef struct {
	int offset;
	int line;
} LineStart;

typedef struct {
	int count;
	int capacity;
	uint8_t *code;
	ValueArray constants;
	int lineCount;
	int lineCapacity;
	LineStart* lines;
} Chunk;

void initChunk(Chunk *chunk);
void freeChunk(VMCtx *vmCtx, Chunk *chunk);
void writeChunk(VMCtx *vmCtx, Chunk *chunk, uint8_t byte, int line);
int addConstant(VMCtx *vmCtx, Chunk *chunk, Value value);
int getLine(Chunk *chunk, int instruction);

#endif // SLOX_CHUNK_H
