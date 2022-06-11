#ifndef ELOX_DEBUG_H
#define ELOX_DEBUG_H

#include "elox/chunk.h"

void disassembleChunk(Chunk *chunk, const char *name);
int disassembleInstruction(Chunk *chunk, int offset);

#endif // ELOX_DEBUG_H
