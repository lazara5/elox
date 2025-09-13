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

static char *strrpbrk(const char *str, const char *chars) {
	const char *c;
	char *p0 = NULL;
	char *p1 = NULL;

	for (c = chars; c && *c; c++) {
		p1 = strrchr(str, *c);
		if (p1 && p1 > p0)
			p0 = p1;
	}
	return p0;
}

String eloxBasename(const char *path) {
	char *p = strrpbrk(path, "/\\");
	return p ? (String){(uint8_t *)p + 1, strlen(p + 1)} : (String){(uint8_t *)path, strlen(path)};
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

EloxInterpretResult eloxRunFile(EloxFiberHandle *fiberHandle, const char *path) {
	String fileName = eloxBasename(path);
	uint8_t *source = readFile(path);
	String main = ELOX_STRING("<main>");
	fiberHandle->runCtx.activeFiber = fiberHandle->fiber;
	EloxInterpretResult result = interpret(&fiberHandle->runCtx, source, &fileName, &main);
	fiberHandle->runCtx.activeFiber = NULL;
	free(source);

	return result;
}

EloxInterpretResult eloxInterpret(EloxFiberHandle *fiberHandle, uint8_t *source,
								  const String *fileName, const String *moduleName) {
	return interpret(&fiberHandle->runCtx, source, fileName, moduleName);
}
