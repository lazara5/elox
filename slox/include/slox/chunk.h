#ifndef SLOX_CHUNK_H
#define SLOX_CHUNK_H

#include "slox/common.h"
#include "slox/value.h"

typedef struct VMCtx VMCtx;

typedef enum {
#define OPCODE(name) OP_##name,
#define SLOX_OPCODES_INLINE

#include "slox/opcodes.h"

#undef SLOX_OPCODES_INLINE
#undef OPCODE
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
