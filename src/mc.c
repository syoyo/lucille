#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>

#include "random.h"
#include "mc.h"

static void random_permute(int n, double *x);

/*
 * Function: latin_hyper_cube
 *
 *     Implements Latin hyper cube(lhc) sampling.
 *     It generates n sampling points in stratified n^d voxel space,
 *     which is useful for stratified sampling in higher dimension.
 *
 * Parameters:
 *
 *     *x - d dimensional n random stratified sampling points generated
 *          (d * x doubles).
 *      n - The number of sampling points to generate.
 *      d - The dimension sampling points are generated.
 *
 * Returns:
 *
 *     None.
 *
 * See Also:
 *
 *     <random_permute>
 *
 * Reference:
 *
 *     - None.
 */
void
latin_hyper_cube(double *x, int n, int d)
{
	int i, j;
	double **replications;		/* d replications of n points */
	double r;

	replications = (double **)malloc(sizeof(double) * d);
	if (!replications) exit(-1);
	for (i = 0; i < d; i++) {
		replications[i] = (double *)malloc(sizeof(double) * n);
		if (!replications[i]) exit(-1);

		/* fill with identity mapping (0, 1, ..., n) */
		for (j = 0; j < n; j++) {
			replications[i][j] = j; 
		}

		/* randomly permute array */
		random_permute(n, replications[i]);
	}

	/* Now replications become like this(here for exapmple n = 5, d = 3). 
	 *
	 * replications[0]   (1, 3, 4, 0, 2)
	 * replications[1]   (0, 1, 3, 2, 4)
	 * replications[2]   (4, 2, 0, 1, 3)
	 */

	/* Then, perform stratified random sampling.
	 * Grid space is choosen from each column of replications,
	 */	

	for (i = 0; i < n; i++) {
		for (j = 0; j < d; j++) {
			r = randomMT() / n;		/* [0, 1/n) */
			x[j] = (double)replications[j][i] / n + r;
		}
	}

	for (i = 0; i < d; i++) {
		free(replications[i]); replications[i] = NULL;
	}
	free(replications); replications = NULL;
}

/* --- private functions --- */

/*
 * Function: random_permute
 *
 *     Permutes(shuffles) array randomly.
 *     For all elements, it equally appears on any index with 1/n probability.
 *
 * Parameters:
 *
 *     n  - Number of elements in array.
 *     *x - Pointer to array to be permuted randomly.
 *
 * Returns:
 *
 *     None.
 *
 * Reference:
 *
 *     - A. Nijenhus and H. Wilf,
 *       "Combinatorial Algorithms",
 *       Academic Press, 1978, second edition,
 *       ISBN 0-12-519260-6.
 */
static void
random_permute(int n, double *x)
{
	int i, j;
	double tmp;

	for (i = 1; i < n; i++) {
		j = (int)(randomMT() * (double)(i + 1)); 
		
		assert(j >= 0);
		assert(j < n);

		/* swap index i and j. */	
		tmp = x[j];
		x[j] = x[i];
		x[i] = tmp; 
	} 
}
