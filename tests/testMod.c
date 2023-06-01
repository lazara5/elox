#include <elox/state.h>
#include <elox.h>

static String testMod = STRING_INITIALIZER("testMod");

static Value printHello(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	ELOX_WRITE(vmCtx, ELOX_IO_OUT, "Hello World!");
	return NIL_VAL;
}

ELOX_EXPORT EloxValue eloxLoadTestMod(Args *args) {
	VMCtx *vmCtx = args->vmCtx;

	const String printName = STRING_INITIALIZER("printHello");
	registerNativeFunction(vmCtx, &printName, &testMod, printHello, 0, false);

	return NIL_VAL;
}
