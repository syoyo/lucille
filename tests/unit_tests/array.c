#include <stdio.h>

#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>

void
test1()
{
	CU_ASSERT(1);
}

int
main(int argc, char **argv)
{
#if 0
	CU_pSuite psuite;

	CU_initialize_registry();

	psuite = CU_add_suite("suite1", NULL, NULL);
	
	CU_add_test(psuite, "test1", test1);

	CU_basic_set_mode(CU_BRM_VERBOSE);


	printf("\nTests completed with return value %d.\n", CU_basic_run_tests());

	CU_cleanup_registry();
#endif
}
