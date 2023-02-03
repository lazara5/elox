#include <check.h>
#include <stdlib.h>

#include "elox/util.h"
#include "elox/state.h"
#include <elox.h>

#define FUNCTIONAL_TEST(PATH, NAME) \
	START_TEST(NAME) {\
		VMCtx vmCtx; \
		EloxConfig config; \
		eloxInitConfig(&config); \
		initVMCtx(&vmCtx, &config); \
\
		EloxInterpretResult res = eloxRunFile(&vmCtx, #PATH); \
		ck_assert_msg(res == ELOX_INTERPRET_OK, "FAIL (%d): %s", res, #PATH); \
\
		destroyVMCtx(&vmCtx); \
	} END_TEST

#include "elox-functional-tests.h"
#undef FUNCTIONAL_TEST

int main(int argc ELOX_UNUSED, char **argv ELOX_UNUSED) {
	Suite *s = suite_create("elox");

	TCase *tcFunctional = tcase_create("Functional");

#define FUNCTIONAL_TEST(PATH, NAME) \
	tcase_add_test(tcFunctional, NAME);

#include "elox-functional-tests.h"
#undef FUNCTIONAL_TEST

	suite_add_tcase(s, tcFunctional);

	SRunner *sr = srunner_create(s);

	srunner_run_all(sr, CK_NORMAL);

	int numFailed = srunner_ntests_failed(sr);
	srunner_free(sr);

	return (numFailed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
