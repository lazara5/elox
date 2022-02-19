#include <stdlib.h>

#include "slox/chunk.h"
#include "slox/vm.h"

void initChunk(Chunk *chunk) {
	chunk->count = 0;
	chunk->capacity = 0;
	chunk->code = NULL;
	chunk->lineCount = 0;
	chunk->lineCapacity = 0;
	chunk->lines = NULL;
	initValueArray(&chunk->constants);
}

void freeChunk(VMCtx *vmCtx, Chunk *chunk) {
	FREE_ARRAY(vmCtx, uint8_t, chunk->code, chunk->capacity);
	FREE_ARRAY(vmCtx, LineStart, chunk->lines, chunk->capacity);
	freeValueArray(vmCtx, &chunk->constants);
	initChunk(chunk);
}

void writeChunk(VMCtx *vmCtx, Chunk* chunk, uint8_t byte, int line) {
	if (chunk->capacity < chunk->count + 1) {
		int oldCapacity = chunk->capacity;
		chunk->capacity = GROW_CAPACITY(oldCapacity);
		chunk->code = GROW_ARRAY(vmCtx, uint8_t, chunk->code, oldCapacity, chunk->capacity);
	}

	chunk->code[chunk->count] = byte;
	chunk->count++;

	// See if we're still on the same line
	if ((chunk->lineCount > 0) && (chunk->lines[chunk->lineCount - 1].line == line)) {
		return;
	}

	// Append a new LineStart
	if (chunk->lineCapacity < chunk->lineCount + 1) {
		int oldCapacity = chunk->lineCapacity;
		chunk->lineCapacity = GROW_CAPACITY(oldCapacity);
		chunk->lines = GROW_ARRAY(vmCtx, LineStart, chunk->lines, oldCapacity, chunk->lineCapacity);
	}

	LineStart* lineStart = &chunk->lines[chunk->lineCount++];
	lineStart->offset = chunk->count - 1;
	lineStart->line = line;
}

int addConstant(VMCtx *vmCtx, Chunk *chunk, Value value) {
	VM *vm = &vmCtx->vm;

	push(vm, value);
	writeValueArray(vmCtx, &chunk->constants, value);
	pop(vm);
	return chunk->constants.count - 1;
}

int getLine(Chunk *chunk, int instruction) {
	int start = 0;
	int end = chunk->lineCount - 1;

	for (;;) {
		int mid = (start + end) / 2;
		LineStart* line = &chunk->lines[mid];
		if (instruction < line->offset) {
			end = mid - 1;
		} else if (mid == chunk->lineCount - 1 ||
			instruction < chunk->lines[mid + 1].offset) {
			return line->line;
		} else {
			start = mid + 1;
		}
	}
}
