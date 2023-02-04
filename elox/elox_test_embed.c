#include "elox/util.h"
#include "elox/state.h"
#include <elox.h>

#include <assert.h>
#include <string.h>

int main(int argc ELOX_UNUSED, char **argv) {
	VMCtx vmCtx;
	EloxConfig config;
	eloxInitConfig(&config);
	initVMCtx(&vmCtx, &config);

	eloxRunFile(&vmCtx, argv[1]);

	EloxCallableHandle *f1Hnd = eloxGetFunction(&vmCtx, "f1", eloxMainModuleName);
	EloxCallableInfo ci = eloxPrepareCall(&vmCtx, f1Hnd);
	eloxPushDouble(&ci, 1);
	EloxInterpretResult res = eloxCall(&vmCtx, &ci);
	double dRes = eloxGetResultDouble(&ci);
	assert(dRes == 43);

	EloxCallableHandle *f2Hnd = eloxGetFunction(&vmCtx, "f2", eloxMainModuleName);
	ci = eloxPrepareCall(&vmCtx, f2Hnd);
	eloxPushDouble(&ci, 42);
	eloxPushDouble(&ci, 10);
	eloxPushDouble(&ci, 20);
	res = eloxCall(&vmCtx, &ci);
	const char *sRes = eloxGetResultString(&ci);
	assert(strcmp(sRes, "67") == 0);

	destroyVMCtx(&vmCtx);

	return res;
}
