#include "elox/common.h"
#include "elox/state.h"
#include "elox/util.h"
#include "elox.h"

int main(int argc ELOX_UNUSED, char **argv) {
	VMCtx vmCtx;
	initVMCtx(&vmCtx);

	eloxRunFile(&vmCtx, argv[1]);

	EloxCallableHandle *f1Hnd = eloxGetFunction(&vmCtx, "f1", eloxMainModuleName);
	EloxCallableInfo ci = eloxPrepareCall(&vmCtx, f1Hnd, -1);
	EloxSetSlotDouble(&ci, 0, 1);
	EloxInterpretResult res = eloxCall(&vmCtx, &ci);

	destroyVMCtx(&vmCtx);

	return 0;
}
