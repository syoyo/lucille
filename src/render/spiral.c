/*
 * Spiral bucket ordering from
 *
 *   Ian Stephenson,
 *   "Production Rendering - Design and Implementation",
 *   Springer.
 *
 */

#include <math.h>

#include "spiral.h"

static int g_n = 0;
static int g_nxbuckets, g_nybuckets;

static void NthBucketSpiral(int n, int nxbuckets, int nybuckets,
		            unsigned int *bucketx, unsigned int *buckety);

/* --------------------------------------------------------------------------
 *
 * Public functions
 *
 * ----------------------------------------------------------------------- */

/*
 * Function: spiral_setup
 *
 *     Setups variables required for spiral order scan rendering.
 *
 * Parameters:
 *
 *     wpix        - the number of pixels in image width.
 *     hpix        - the number of pixels in image height.
 *     bucket_size - the size of bucket in pixels.
 *
 * Returns:
 *
 *     None.
 */
void
spiral_setup(unsigned int wpix, unsigned int hpix, unsigned int bucket_size)
{
	int nx, ny;

	nx = (int)ceil(wpix / (double)bucket_size);
	ny = (int)ceil(hpix / (double)bucket_size);
	
	g_nxbuckets = nx;
	g_nybuckets = ny;
}

/*
 * Function: spiral_get_nextlocation
 *
 *     Returns next location in spiral order.
 *
 *
 * Parameters:
 *
 *     *xp - next location of x.
 *     *yp - next location of y.
 *
 * Returns:
 *
 *     0 if traversed all bucket in spiral order, 1 if not.
 */ 
int
spiral_get_nextlocation(unsigned int *xp, unsigned int *yp)
{
	if (g_n >= g_nxbuckets * g_nybuckets) return 0;

	NthBucketSpiral(g_n, g_nxbuckets, g_nybuckets, xp, yp);

	g_n++;

	return 1;
}

/* --------------------------------------------------------------------------
 *
 * Private functions
 *
 * ----------------------------------------------------------------------- */
void
NthBucketSpiral(
	int           n,
	int           nxbuckets,
	int           nybuckets,
	unsigned int *bucketx,
	unsigned int *buckety)
{
	int nx, ny, nxny, minnxny, x, y;
	int minnbuckets;

	int center;

	minnbuckets = (nxbuckets < nybuckets) ? nxbuckets : nybuckets;
	
	center = (minnbuckets - 1) / 2;
	
	nx = nxbuckets;
	ny = nybuckets;

	while (n < nx * ny) { nx = nx - 1; ny = ny -1; }

	nxny = nx * ny;
	minnxny = (nx < ny) ? nx : ny;

	if (minnxny % 2 == 1) {	// odd
		if (n <= (nxny + ny)) {	// down right side
			x = nx - minnxny / 2;
			y = -minnxny / 2 + n - nxny;
		} else {	// back across bottom
			x = nx - minnxny / 2 - (n - (nxny + ny));
			y = ny - minnxny / 2;
		}
	} else {
		if (n <= (nxny + ny)) {	// up right size
			x = -minnxny / 2;
			y = ny - minnxny / 2 - (n - nxny);
		} else {	// back across bottom
			x = -minnxny / 2 + (n - (nxny + ny));
			y = -minnxny / 2;
		}
	}

	*bucketx = x + center;
	*buckety = y + center;
}


#ifdef SPIRAL_TEST
int
main(int argc, char **argv)
{
	int i;
	int nxbuckets = 17;
	int nybuckets = 16;
	int x, y;
	
	for (i = 0; i < nxbuckets * nybuckets; i++) {
		NthBucketSpiral(i, nxbuckets, nybuckets, &x, &y);
		printf("n = %d:  pos (%d, %d)\n", i, x, y);
	} 

	return 0;
}
#endif

