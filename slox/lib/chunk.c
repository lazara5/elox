#include <stdlib.h>

#include "slox/chunk.h"
#include "slox/memory.h"
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

void freeChunk(Chunk *chunk) {
	FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
	FREE_ARRAY(LineStart, chunk->lines, chunk->capacity);
	freeValueArray(&chunk->constants);
	initChunk(chunk);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
	if (chunk->capacity < chunk->count + 1) {
		int oldCapacity = chunk->capacity;
		chunk->capacity = GROW_CAPACITY(oldCapacity);
		chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
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
		chunk->lines = GROW_ARRAY(LineStart, chunk->lines, oldCapacity, chunk->lineCapacity);
	}

	LineStart* lineStart = &chunk->lines[chunk->lineCount++];
	lineStart->offset = chunk->count - 1;
	lineStart->line = line;
}

int addConstant(Chunk *chunk, Value value) {
	push(value);
	writeValueArray(&chunk->constants, value);
	pop();
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
