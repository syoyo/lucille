#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "util.h"
#include "log.h"
#include "array.h"
#include "memory.h"

static const unsigned int g_primes[] = {
	11,
	19,
	37,
	109,
	163,
	251,
	367,
	557,
	823,
	1237,
	1861,
	2777,
	4177,
	6247,
	9371,
	21089,
	31627,
	47431,
	71143,
	106721,
	160073,
	240101,
	360163,
	540217,
	810343,
	1215497,
	1823231,
	2734867,
	4102283,
	6153409,
	9230113,
	13845163
};

unsigned int
ri_util_closest_prime(unsigned int n)
{
	      int i;	
	const int nprimes = sizeof g_primes / sizeof(g_primes[0]);

	for (i = 0; i < nprimes; i++) {
		if (g_primes[i] > n) return g_primes[i];
	}

	return g_primes[nprimes - 1];
}

unsigned int
ri_util_min_prime()
{
	return g_primes[0];
}

unsigned int
ri_util_max_prime()
{
	const int nprimes = sizeof g_primes / sizeof(g_primes[0]);

	return g_primes[nprimes - 1];
}

/*
 * Function: ri_util_paramlist_build
 *
 *    Parse RIB variable array and allocates memory for it.
 *
 * Parameters:
 *
 *    arg      - variable array
 *    **tokens - list of RtTokens returned.
 *    **values - list of RtPointers returned.
 *
 * Returns:
 *
 *    The number of RIB parameters in arg.
 *
 * SeeAlso:
 *
 *    <ri_util_paramlist_free>
 */
unsigned int
ri_util_paramlist_build(va_list arg, RtToken **tokens, RtPointer **values)
{
	unsigned int  i;
	unsigned int  count;
	RtToken       token;
	RtPointer     value;
	ri_ptr_array_t    *tokenarray;	/* dynamic ptr array for temporal */
	ri_ptr_array_t    *valuearray;	/* dynamic ptr array for temporal */

	count = 0;

	tokenarray = ri_ptr_array_new();
	valuearray = ri_ptr_array_new();

	token = va_arg(arg, RtToken);

	while (token != 0 && token != RI_NULL) {
		value = va_arg(arg, RtPointer);

		if (value == RI_NULL) {
			ri_log(LOG_ERROR, "value == RI_NULL");
			return count;
		}

		ri_ptr_array_insert(tokenarray, count, (void *)token);
		ri_ptr_array_insert(valuearray, count, value);

		count++;
		
		/* next token */
		token = va_arg(arg, RtToken);
	}

	if (count != 0) {
		(*tokens) = (RtToken *)ri_mem_alloc(sizeof(RtToken) * count);
		(*values) = (RtPointer *)ri_mem_alloc(sizeof(RtPointer) *
							 count);

		/* copy array */
		for (i = 0; i < count; i++) {
			(*tokens)[i] = (RtToken)ri_ptr_array_at(tokenarray, i);
			(*values)[i] = (RtPointer)ri_ptr_array_at(valuearray, i);
		}	
	} else {
		/* no arg list */
		(*tokens) = NULL;
		(*values) = NULL;
	}

	ri_ptr_array_free(tokenarray);
	ri_ptr_array_free(valuearray);

	return count;
}

/*
 * Function: ri_util_paramlist_free
 *
 *    Frees RIB variable arrays.
 *
 * Parameters:
 *
 *    *tokens - list of RtTokens to be memory-freed its contents.
 *    *values - list of RtPointers to be memory-freed its contents.
 *
 * Returns:
 *
 *    The number of RIB parameters in arg.
 *
 * SeeAlso:
 *
 *    <ri_util_paramlist_build>
 */
void
ri_util_paramlist_free(RtToken *tokens, RtPointer *values)
{
	ri_mem_free(tokens);
	ri_mem_free(values);
}	

/*
 * Function: ri_util_is_little_endian
 *
 *    Checks endianness of the machine running this program.
 *
 * Parameters:
 *
 *
 * Returns:
 *
 *    1 if the machine is little endian.
 *    0 if the machine is big endian.
 *
 */
int
ri_util_is_little_endian()
{
	/* from C Programming FAQs Q 20.9. */
	int x = 1;
	if (*(char *)x) {
		return 1;
	} else {
		return 0;
	}
}

