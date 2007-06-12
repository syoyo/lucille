#include <stdio.h>
#include <stdlib.h>

#include <CUnit/Basic.h>
#include <CUnit/TestDB.h>

#include "test_suite.h"

CU_SuiteInfo suites[] = {
	{"array", init_array, cleanup_array, test_array},
	CU_SUITE_INFO_NULL
};


int
main(int argc, char **argv)
{
	CU_BasicRunMode mode = CU_BRM_VERBOSE;

	CU_initialize_registry();

	CU_register_suites(suites);

	CU_basic_set_mode(mode);

	CU_basic_run_tests();

	printf("\n");
	CU_basic_show_failures(CU_get_failure_list());
	printf("\n\n");


	CU_cleanup_registry();

	return 0;
}

