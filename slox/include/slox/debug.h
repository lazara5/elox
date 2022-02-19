#ifndef SLOX_DEBUG_H
#define SLOX_DEBUG_H

#include "slox/chunk.h"

void disassembleChunk(Chunk *chunk, const char *name);
int disassembleInstruction(Chunk *chunk, int offset);

#endif // SLOX_DEBUG_H
