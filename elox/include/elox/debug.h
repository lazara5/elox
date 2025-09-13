#ifndef ELOX_DEBUG_H
#define ELOX_DEBUG_H

#include "elox/chunk.h"
#include "elox/scanner.h"

void disassembleChunk(VMCtx *vmCtx, Chunk *chunk, const char *name);
int disassembleInstruction(VMCtx *vmCtx, Chunk *chunk, int offset);
void printToken(VMCtx *vmCtx, Token *token);

#endif // ELOX_DEBUG_H
