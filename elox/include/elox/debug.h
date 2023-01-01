#ifndef ELOX_DEBUG_H
#define ELOX_DEBUG_H

#include "elox/chunk.h"

void disassembleChunk(VMCtx *vmctx, Chunk *chunk, const char *name);
int disassembleInstruction(VMCtx *vmctx, Chunk *chunk, int offset);

#endif // ELOX_DEBUG_H
