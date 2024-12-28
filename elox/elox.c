// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/util.h"

#include <stdio.h>
#include <stdlib.h>

static void repl(EloxRunCtxHandle *runHandle) {
	char line[1024];
	for (;;) {
		printf("> ");

		if (!fgets(line, sizeof(line), stdin)) {
			printf("\n");
			break;
		}

		EloxString main = ELOX_STRING("<main>");
		EloxString stdin = ELOX_STRING("<stdin>");
		eloxInterpret(runHandle, (uint8_t *)line, &stdin, &main);
	}
}

int main(int argc, char **argv) {
	EloxConfig config;
	eloxInitConfig(&config);
	EloxVMCtx *vmCtx = eloxNewVMCtx(&config);
	if (vmCtx == NULL)
		exit(60);

	EloxRunCtxHandle *runHandle = eloxNewRunCtx(vmCtx);
	if (runHandle == NULL)
		exit(61);

	if (argc == 1)
		repl(runHandle);
	else if (argc == 2) {
		EloxInterpretResult res = eloxRunFile(runHandle, argv[1]);
		if (res == ELOX_INTERPRET_COMPILE_ERROR)
			exit(65);
		if (res == ELOX_INTERPRET_RUNTIME_ERROR)
			exit(70);
	} else {
		fprintf(stderr, "Usage: elox [path]\n");
		exit(64);
	}

	eloxReleaseHandle((EloxHandle *)runHandle);

	eloxDestroyVMCtx(vmCtx);

	return 0;
}
