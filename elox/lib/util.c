// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include <elox/state.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

bool stringEquals(const String *a, const String *b) {
	if (a->length != b->length)
		return false;
	return (memcmp(a->chars, b->chars, a->length) == 0);
}

static uint8_t *readFile(const char *path) {
	FILE *file = fopen(path, "rb");
	if (file == NULL) {
		fprintf(stderr, "Could not open file '%s'\n", path);
		exit(74);
	}

	fseek(file, 0L, SEEK_END);
	size_t fileSize = ftell(file);
	rewind(file);

	uint8_t *buffer = malloc(fileSize + 1);
	if (buffer == NULL) {
		fprintf(stderr, "Not enough memory to read '%s'\n", path);
		exit(74);
	}

	size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
	if (bytesRead < fileSize) {
		fprintf(stderr, "Could not read file '%s'\n", path);
		exit(74);
	}

	buffer[bytesRead] = '\0';

	fclose(file);
	return buffer;
}

EloxInterpretResult eloxRunFile(EloxVM *vmCtx, const char *path) {
	uint8_t *source = readFile(path);
	String main = STRING_INITIALIZER("<main>");
	EloxInterpretResult result = interpret(vmCtx, source, &main);
	free(source);

	return result;

	/*if (result == ELOX_INTERPRET_COMPILE_ERROR)
		exit(65);
	if (result == ELOX_INTERPRET_RUNTIME_ERROR)
		exit(70);*/
}
