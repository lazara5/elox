// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "elox/util.h"

#include <stdio.h>
#include <stdlib.h>

static void repl(EloxFiberHandle *fiberHandle) {
	char line[1024];
	for (;;) {
		printf("> ");

		if (!fgets(line, sizeof(line), stdin)) {
			printf("\n");
			break;
		}

		EloxString main = ELOX_STRING("<main>");
		EloxString stdin = ELOX_STRING("<stdin>");
		eloxInterpret(fiberHandle, (uint8_t *)line, &stdin, &main);
	}
}

int main(int argc, char **argv) {
	EloxConfig config;
	eloxInitConfig(&config);
	EloxVMInst *vmInst = eloxNewVMInst(&config);
	if (vmInst == NULL)
		exit(60);

	EloxAPIError error = ELOX_API_ERROR_INITIALIZER;

	EloxFiberHandle *fiberHandle = eloxNewFiber(vmInst, &error);
	if (fiberHandle == NULL)
		exit(61);

	if (argc == 1)
		repl(fiberHandle);
	else if (argc == 2) {
		EloxInterpretResult res = eloxRunFile(fiberHandle, argv[1]);
		if (res == ELOX_INTERPRET_COMPILE_ERROR)
			exit(65);
		if (res == ELOX_INTERPRET_RUNTIME_ERROR)
			exit(70);
	} else {
		fprintf(stderr, "Usage: elox [path]\n");
		exit(64);
	}

	eloxReleaseHandle((EloxHandle *)fiberHandle);

	eloxDestroyVMInst(vmInst);

	return 0;
}
