#include "elox/state.h"
#include "elox/util.h"

#include <stdio.h>
#include <stdlib.h>

static void repl(VMCtx *vmCtx) {
	char line[1024];
	for (;;) {
		printf("> ");

		if (!fgets(line, sizeof(line), stdin)) {
			printf("\n");
			break;
		}

		String main = STRING_INITIALIZER("<main>");
		interpret(vmCtx, (uint8_t *)line, &main);
	}
}

int main(int argc, char **argv) {
	VMCtx vmCtx;
	EloxConfig config;
	eloxInitConfig(&config);
	initVMCtx(&vmCtx, &config);

	if (argc == 1)
		repl(&vmCtx);
	else if (argc == 2) {
		EloxInterpretResult res = eloxRunFile(&vmCtx, argv[1]);
		if (res == ELOX_INTERPRET_COMPILE_ERROR)
			exit(65);
		if (res == ELOX_INTERPRET_RUNTIME_ERROR)
			exit(70);
	} else {
		fprintf(stderr, "Usage: elox [path]\n");
		exit(64);
	}

	destroyVMCtx(&vmCtx);

	return 0;
}
