#include <check.h>
#include <stdlib.h>

#include "elox/common.h"
#include "elox/state.h"
#include "elox/util.h"
#include "elox.h"

#include "elox-functional-tests.h"

START_TEST(testFunctional) {
	VMCtx vmCtx;

	for (size_t ft = 0; ft < ELOX_ARRAY_SIZE(functionalTests); ft++ ) {
		EloxConfig config;
		eloxInitConfig(&config);
		initVMCtx(&vmCtx, &config);

		EloxInterpretResult res = eloxRunFile(&vmCtx, functionalTests[ft]);
		ck_assert_msg(res == ELOX_INTERPRET_OK, "FAIL (%d): %s", res, functionalTests[ft]);

		destroyVMCtx(&vmCtx);
	}
} END_TEST

int main(int argc ELOX_UNUSED, char **argv ELOX_UNUSED) {
	Suite *s = suite_create("elox");

	TCase *tcFunctional = tcase_create("Functional");
	tcase_add_test(tcFunctional, testFunctional);
	suite_add_tcase(s, tcFunctional);

	SRunner *sr = srunner_create(s);

	srunner_run_all(sr, CK_NORMAL);

	int numFailed = srunner_ntests_failed(sr);
	srunner_free(sr);

	return (numFailed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
