// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/chunk.h"
#include "elox/state.h"

#include <stdlib.h>
#include <string.h>

void initChunk(Chunk *chunk) {
	chunk->count = 0;
	chunk->capacity = 0;
	chunk->code = NULL;
	chunk->lineCount = 0;
	chunk->lineCapacity = 0;
	chunk->lines = NULL;
	initValueArray(&chunk->constants);
}

void freeChunk(RunCtx *runCtx, Chunk *chunk) {
	FREE_ARRAY(runCtx, uint8_t, chunk->code, chunk->capacity);
	FREE_ARRAY(runCtx, LineStart, chunk->lines, chunk->capacity);
	freeValueArray(runCtx, &chunk->constants);
	initChunk(chunk);
}

void writeChunk(CCtx *cCtx, Chunk *chunk, uint8_t *data, uint8_t len, int line) {
	RunCtx *runCtx = cCtx->runCtx;

	if (chunk->capacity < chunk->count + len) {
		int oldCapacity = chunk->capacity;
		chunk->capacity = GROW_CAPACITY(oldCapacity);
		uint8_t *oldCode = chunk->code;
		chunk->code = GROW_ARRAY(runCtx, uint8_t, chunk->code, oldCapacity, chunk->capacity);
		if (ELOX_UNLIKELY(chunk->code == NULL)) {
			chunk->code = oldCode;
			compileError(cCtx, "Out of memory");
			return;
		}
	}

	memcpy(chunk->code + chunk->count, data, len);
	chunk->count += len;

	// See if we're still on the same line
	if ((chunk->lineCount > 0) && (chunk->lines[chunk->lineCount - 1].line == line)) {
		return;
	}

	// Append a new LineStart
	if (chunk->lineCapacity < chunk->lineCount + 1) {
		int oldCapacity = chunk->lineCapacity;
		chunk->lineCapacity = GROW_CAPACITY(oldCapacity);
		LineStart *oldLines = chunk->lines;
		chunk->lines = GROW_ARRAY(runCtx, LineStart, chunk->lines, oldCapacity, chunk->lineCapacity);
		if (ELOX_UNLIKELY(chunk->lines == NULL)) {
			chunk->lines = oldLines;
			compileError(cCtx, "Out of memory");
			return;
		}
	}

	LineStart *lineStart = &chunk->lines[chunk->lineCount++];
	lineStart->offset = chunk->count - 1;
	lineStart->line = line;
}

int addConstant(RunCtx *runCtx, Chunk *chunk, Value value) {
	FiberCtx *fiber = runCtx->activeFiber;

	int ret = -1;
	TmpScope temps = TMP_SCOPE_INITIALIZER(fiber);
	PUSH_TEMP(temps, protectedValue, value);
	bool res = valueArrayPush(runCtx, &chunk->constants, value);
	if (ELOX_UNLIKELY(!res))
		goto cleanup;

	ret = chunk->constants.count - 1;

cleanup:
	releaseTemps(&temps);
	return ret;
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
