#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "elox/common.h"
#include "elox/chunk.h"
#include "elox/debug.h"
#include "elox/state.h"

static void repl(VMCtx *vmCtx) {
	char line[1024];
	for (;;) {
		printf("> ");

		if (!fgets(line, sizeof(line), stdin)) {
			printf("\n");
			break;
		}

		String main = STRING_INITIALIZER("<main>");
		interpret(vmCtx, line, &main);
	}
}

static char *readFile(const char *path) {
	FILE *file = fopen(path, "rb");
	if (file == NULL) {
		fprintf(stderr, "Could not open file '%s'\n", path);
		exit(74);
	}

	fseek(file, 0L, SEEK_END);
	size_t fileSize = ftell(file);
	rewind(file);

	char *buffer = (char *)malloc(fileSize + 1);
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

static void runFile(VMCtx *vmCtx, const char *path) {
	char *source = readFile(path);
	String main = STRING_INITIALIZER("<main>");
	InterpretResult result = interpret(vmCtx, source, &main);
	free(source);

	if (result == INTERPRET_COMPILE_ERROR)
		exit(65);
	if (result == INTERPRET_RUNTIME_ERROR)
		exit(70);
}

int main(int argc, char **argv) {
	VMCtx vmCtx;
	initVMCtx(&vmCtx);

	if (argc == 1)
		repl(&vmCtx);
	else if (argc == 2)
		runFile(&vmCtx, argv[1]);
	else {
		fprintf(stderr, "Usage: elox [path]\n");
		exit(64);
	}

	freeVM(&vmCtx);

	return 0;
}
