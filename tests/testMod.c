#include <elox/state.h>
#include <elox.h>

static String testMod = ELOX_STRING("testMod");

static Value printHello(Args *args) {
	RunCtx *runCtx = args->runCtx;

	ELOX_WRITE(runCtx, ELOX_IO_OUT, "Hello World!");
	return NIL_VAL;
}

ELOX_EXPORT EloxValue eloxLoadTestMod(Args *args) {
	RunCtx *runCtx = args->runCtx;

	const String printName = ELOX_STRING("printHello");
	registerNativeFunction(runCtx, &printName, &testMod, printHello, 0, false);

	return NIL_VAL;
}
