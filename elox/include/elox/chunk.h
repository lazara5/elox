#ifndef ELOX_CHUNK_H
#define ELOX_CHUNK_H

#include <elox/util.h>
#include <elox/ValueArray.h>

typedef struct CCtx CCtx;

typedef enum {
#define OPCODE(name) OP_##name,
#define ELOX_OPCODES_INLINE

#include "elox/opcodes.h"

#undef ELOX_OPCODES_INLINE
#undef OPCODE
	OP_INVALID = 0xff
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
	ObjString *fileName;
	int lineCount;
	int lineCapacity;
	LineStart* lines;
} Chunk;

void initChunk(Chunk *chunk, ObjString *fileName);
void freeChunk(RunCtx *runCtx, Chunk *chunk);
void writeChunk(CCtx *cCtx, Chunk *chunk, uint8_t *data, uint8_t len, int line);
int addConstant(RunCtx *runCtx, Chunk *chunk, Value value);
int getLine(Chunk *chunk, int instruction);

#endif // ELOX_CHUNK_H
