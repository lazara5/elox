#include "elox/util.h"
#include <elox.h>

#include <assert.h>
#include <string.h>

int main(int argc ELOX_UNUSED, char **argv) {
	EloxConfig config;
	EloxInterpretResult res = ELOX_INTERPRET_RUNTIME_ERROR;

	eloxInitConfig(&config);
	EloxVMCtx *vmCtx = eloxNewVMCtx(&config);
	if (vmCtx == NULL)
		goto cleanup;

	EloxRunCtxHandle *runHandle = eloxNewRunCtx(vmCtx);
	if (runHandle == NULL)
		goto cleanup;

	eloxRunFile(runHandle, argv[1]);

	EloxCallableHandle *f1Hnd = eloxGetFunction(runHandle, "f1", eloxMainModuleName);
	EloxCallableInfo ci = eloxPrepareCall(f1Hnd);
	eloxPushDouble(&ci, 1);
	res = eloxCall(&ci);
	double dRes = eloxGetResultDouble(&ci);
	assert(dRes == 43);

	EloxCallableHandle *f2Hnd = eloxGetFunction(runHandle, "f2", eloxMainModuleName);
	ci = eloxPrepareCall(f2Hnd);
	eloxPushDouble(&ci, 42);
	eloxPushDouble(&ci, 10);
	eloxPushDouble(&ci, 20);
	res = eloxCall(&ci);
	const char *sRes = eloxGetResultString(&ci);
	assert(strcmp(sRes, "67") == 0);

cleanup:
	eloxDestroyVMCtx(vmCtx);

	return res;
}
