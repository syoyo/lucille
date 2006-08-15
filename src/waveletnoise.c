/*
 * Am implementation of wavelet noise
 *
 * Robert L. Cook and Tony DeRose
 * "Wavelet Noise", SIGGRAPH 2005.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "random.h"
#include "waveletnoise.h"

static float *noiseTileData;
static int    noiseTileSize;

static int Mod(int x, int n) { int m; m = x % n; return (m < 0) ? m + n : m; }

#define ARAD 16

static void Downsample(float *from, float *to, int n, int stride)
{
	int i, k;
	float *a, aCoeffs[2 * ARAD] = {
0.000334,-0.001528, 0.000410, 0.003545,-0.000938,-0.008233, 0.002172, 0.019120,
-0.005040,-0.044412, 0.011655, 0.103311,-0.025936,-0.243780, 0.033979, 0.655340,
0.655340, 0.033979,-0.243780,-0.025936, 0.103311, 0.011655,-0.044412,-0.005040,
0.019120, 0.002172,-0.008233,-0.000938, 0.003546, 0.000410,-0.001528, 0.000334};

	a = &aCoeffs[ARAD];

	for (i = 0; i < n / 2; i++) {
		to[i*stride] = 0;

		for (k = 2 * i - ARAD; k <= 2 * i + ARAD; k++) {
			to[i*stride] += a[k - 2 * i] * from[Mod(k, n) * stride];
		}
	}
}

static void Upsample(float *from, float *to, int n, int stride)
{
	int i, k;
	float *p, pCoeffs[4] = {0.25, 0.75, 0.75, 0.25};
	p = &pCoeffs[2];

	for (i = 0; i < n; i++) {
		to[i*stride] = 0;

		for (k = i/2; k <= i/2 + 1; k++) {
			to[i*stride] += p[i - 2*k] * from[Mod(k,n/2)*stride];
		}
	}
}

static void GenerateNoiseTile(int n, int olap)
{
	int ix, iy, iz, i;
	int sz;
	int offset;
	float *temp1, *temp2, *noise;

	if (n % 2) n++;		/* tile size must be even */
	sz = n * n * n * sizeof(float);

	temp1 = (float *)malloc(sz);	assert(temp1);
	temp2 = (float *)malloc(sz);	assert(temp2);
	noise = (float *)malloc(sz);

	/* Step 1. Fill the tile with random numbers in the range -1 to 1 */
	for (i = 0; i < n * n * n; i++) {
		noise[i] = (float)(2.0 * randomMT() - 1.0);

	/* Step 2 and 3. Downsample and upsample the tile */
	for (iy = 0; iy < n; iy++) {		/* each x row */
		for (iz = 0; iz < n; iz++) {
			i = iy * n + iz * n * n;
			Downsample(&noise[i], &temp1[i], n, 1); 
			Upsample(  &temp1[i], &temp2[i], n, 1);
		}
	}

	for (ix = 0; ix < n; ix++) {		/* each y row */
		for (iz = 0; iz < n; iz++) {
			i = ix * n + iz * n * n;
			Downsample(&temp2[i], &temp1[i], n, n); 
			Upsample(  &temp1[i], &temp2[i], n, n);
		}
	}

	for (ix = 0; ix < n; ix++) {		/* each z row */
		for (iy = 0; iy < n; iy++) {
			i = ix * n + iz * n * n;
			Downsample(&temp2[i], &temp1[i], n, n*n); 
			Upsample(  &temp1[i], &temp2[i], n, n*n);
		}
	}

	/* Step 4. Subtract out the coarse-scale contribution */
	for (i = 0; i < n * n * n; i++) noise[i] -= temp2[i];

	/* Avoid even/odd variance difference by adding odd-offset version of
	 * noise to itself
	 */
	offset = n /2; if (offset % 2 == 0) offset++;

	for (i = 0, ix = 0; ix < n; ix++) {
		for (iy = 0; iy < n; iy++) {
			for (iz = 0; iz < n; iz++) {
				temp1[i++] = noise[Mod(ix+offset, n)         +
						   Mod(iy+offset, n) * n     +
						   Mod(iz+offset, n) * n * n];
			}
		}
	}

	for (i = 0; i < n * n * n; i++) noise[i] += temp1[i];

	noiseTileData = noise; noiseTileSize = n;
	
	free(temp1);
	free(temp2);
}
