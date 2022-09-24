#include "elox/common.h"
#include "elox/state.h"
#include "elox/util.h"
#include "elox.h"

#include <assert.h>

int main(int argc ELOX_UNUSED, char **argv) {
	VMCtx vmCtx;
	initVMCtx(&vmCtx);

	eloxRunFile(&vmCtx, argv[1]);

	EloxCallableHandle *f1Hnd = eloxGetFunction(&vmCtx, "f1", eloxMainModuleName);
	EloxCallableInfo ci = eloxPrepareCall(&vmCtx, f1Hnd, -1);
	eloxSetSlotDouble(&ci, 0, 1);
	EloxInterpretResult res = eloxCall(&vmCtx, &ci);
	double dRes = eloxGetResultDouble(&ci);
	assert(dRes == 43);

	EloxCallableHandle *f2Hnd = eloxGetFunction(&vmCtx, "f2", eloxMainModuleName);
	ci = eloxPrepareCall(&vmCtx, f2Hnd, -1);
	eloxSetSlotDouble(&ci, 0, 42);
	res = eloxCall(&vmCtx, &ci);
	const char *sRes = eloxGetResultString(&ci);
	assert(strcmp(sRes, "42") == 0);

	destroyVMCtx(&vmCtx);

	return res;
}
