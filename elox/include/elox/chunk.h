#ifndef ELOX_CHUNK_H
#define ELOX_CHUNK_H

#include <elox/util.h>
#include <elox/valueArray.h>

typedef struct VMCtx VMCtx;

typedef enum {
#define OPCODE(name) OP_##name,
#define ELOX_OPCODES_INLINE

#include "elox/opcodes.h"

#undef ELOX_OPCODES_INLINE
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
void writeChunk(VMCtx *vmCtx, Chunk *chunk, uint8_t *data, uint8_t len, int line);
int addConstant(VMCtx *vmCtx, Chunk *chunk, Value value);
int getLine(Chunk *chunk, int instruction);

#endif // ELOX_CHUNK_H
