#include "elox/util.h"
#include <elox.h>

#include <assert.h>
#include <string.h>

int main(int argc ELOX_UNUSED, char **argv) {
	EloxConfig config;
	EloxInterpretResult res = ELOX_INTERPRET_RUNTIME_ERROR;

	eloxInitConfig(&config);
	EloxVMInst *vmInst = eloxNewVMInst(&config);
	if (vmInst == NULL)
		goto cleanup;

	EloxAPIError error = ELOX_API_ERROR_INITIALIZER;

	EloxFiberHandle *fiberHandle = eloxNewFiber(vmInst, &error);
	if (fiberHandle == NULL)
		goto cleanup;

	eloxRunFile(fiberHandle, argv[1]);

	EloxCallableHandle *f1Hnd = eloxGetFunction(vmInst, "f1", eloxMainModuleName);
	EloxCallFrame *cf = eloxOpenCall(fiberHandle, f1Hnd, &error);
	eloxPushDouble(cf, 1);
	res = eloxCall(cf);
	double dRes = eloxGetResultDouble(cf);
	assert(dRes == 43);

	EloxCallableHandle *f2Hnd = eloxGetFunction(vmInst, "f2", eloxMainModuleName);
	cf = eloxOpenCall(fiberHandle, f2Hnd, &error);
	eloxPushDouble(cf, 42);
	eloxPushDouble(cf, 10);
	eloxPushDouble(cf, 20);
	res = eloxCall(cf);
	const char *sRes = eloxGetResultString(cf);
	assert(strcmp(sRes, "67") == 0);

cleanup:
	eloxDestroyVMInst(vmInst);

	return res;
}
