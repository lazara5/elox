#ifndef ELOX_DEBUG_H
#define ELOX_DEBUG_H

#include "elox/chunk.h"
#include "elox/scanner.h"

void disassembleChunk(RunCtx *runCtx, Chunk *chunk, const char *name);
int disassembleInstruction(RunCtx *runCtx, Chunk *chunk, int offset);
void printToken(RunCtx *runCtx, Token *token);

#endif // ELOX_DEBUG_H
