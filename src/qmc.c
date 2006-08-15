#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>

#include "qmc.h"

#ifndef M_PI
#define M_PI 3.1415926535
#endif


/* Prime numbers up to 100 dimension. */
static int primes[] = {
	  2,   3,   5,   7,  11,  13,  17,  19,  23,  29, /*  10 */
	 31,  37,  41,  43,  47,  53,  59,  61,  67,  71, /*  20 */
	 73,  79,  83,  89,  97, 101, 103, 107, 109, 113, /*  30 */
	127, 131, 137, 139, 149, 151, 157, 163, 167, 173, /*  40 */ 
	179, 181, 191, 193, 197, 199, 211, 223, 227, 229, /*  50 */
	233, 239, 241, 251, 257, 263, 269, 271, 277, 281, /*  60 */
	283, 293, 307, 311, 313, 317, 331, 337, 347, 349, /*  70 */
	353, 359, 367, 373, 379, 383, 389, 397, 401, 409, /*  80 */
	419, 421, 431, 433, 439, 443, 449, 457, 461, 463, /*  90 */
	467, 479, 487, 491, 499, 503, 509, 521, 523, 541  /* 100 */
};

static int nprimes = sizeof(primes) / sizeof(int);

static int    fibonacci(int k);

double
halton2(int i)
{
	unsigned long h, f;

	h = i & 1; f = 2;
	i >>= 1;

	while (i) {
		h <<= 1;
		h += (i & 1);
		i >>= 1;
		f <<= 1;
		h <<= 1;
		h += (i & 1);
		i >>= 1;
		f <<= 1;
		h <<= 1;
		h += (i & 1);
		i >>= 1;
		f <<= 1;
		h <<= 1;
		h += (i & 1);
		i >>= 1;
		f <<= 1;
	}

	return (double)h / (double)f;
}

/*
 * Generate Hammersley points in [0,1)^2
 *
 */
void
hammersley2(double *out, int n)
{
	double p, t;
	int k, kk;

	for (k = 0; k < n; k++) {
		t = 0;
		for (p = 0.5, kk = k; kk; p *= 0.5, kk >>= 1) {
			if (kk & 1) {		/* kk mod 2 = 1		*/
				t += p;
			}
		}
	
		out[2 * k + 0] = (double)k / (double)n;
		out[2 * k + 1] = t;
	}
}

/*
 * Generate Hammersley points on sphere(p1 = 2)
 *
 * see:
 * "Sampling with Hammersley and Halton Points"
 * Tien-Tsin Wong, Wai-Shing Luk and Pheng-Ann Heng,
 * Journal of Graphics Tools , vol. 2, no. 2, pp 9-24, 1997
 *
 */
void
hammersley2_sphere(double *out, int n)
{
	double p, t, sint, phi;
	int k, kk;

	for (k = 0; k < n; k++) {
		t = 0;
		for (p = 0.5, kk = k; kk; p *= 0.5, kk >>= 1) {
			if (kk & 1) {		/* kk mod 2 = 1		*/
				t += p;
			}
		}
	
		t = 2.0 * t - 1.0;	/* [0,1] -> [-1, 1]	*/
		phi = (k + 0.5) / n;
		phi *= 2.0 * M_PI;	/* [0, 2 Pi)		*/

		sint = sqrt(1.0 - t * t); /* sin(theta)		*/

		out[3 * k + 0] = sint * cos(phi);
		out[3 * k + 1] = sint * sin(phi);
		out[3 * k + 2] = t;
	}
}


/*
 * Function: faure_permutation
 *
 *     Generates the sequence of permutation up to n dimension
 *     using Faure's permutation method.
 *
 * Parameters:
 *
 *      n - Maximum dimension to generate.
 *
 * Returns:
 *
 *      The sequence of permutation.
 *
 * Reference:
 *
 *     - Faure Henri,
 *       Good permutations for extreme discrepancy,
 *       J. Number Theory 42, no. 1, 47--56, 1992
 *
 * Note:
 *
 *     Permutations up to n = 8.
 *
 *     - p2 = (0, 1)
 *     - p3 = (0, 1, 2)
 *     - p4 = (0, 2, 1, 3)
 *     - p5 = (0, 3, 2, 1, 4)
 *     - p6 = (0, 2, 4, 1, 3, 5)
 *     - p7 = (0, 2, 5, 3, 1, 4, 6)
 *     - p8 = (0, 4, 2, 6, 1, 5, 3, 7)
 *
 */
int **
faure_permutation(int n)
{
	int i, j, c;
	int **p;

	if (n < 2) return NULL;

	/* Allocate memory for permutation table. */
	p = (int **)malloc(sizeof(int *) * (n + 1));
	for (i = 1; i < n + 1; i++) {
		p[i] = (int *)malloc(sizeof(int) * (i + 1));
	}

	/* p[0] is not used. */
	p[0] = NULL;

	/* start with identity mapping in p_{2} = (0,1). */
	p[1][0] = 0;
	p[1][1] = 1;

	for (i = 3; i < n + 1; i++) {
		if (i % 2 != 0) {	/* odd */ 
			/* first (i - 1) / 2 index */
			for (j = 0; j < (i - 1) / 2; j++) {
				/*
				 * if p_{i-1}(j) >= (i - 1) / 2
				 *     p_{i}(j) = p_{i-1}(j) + 1
				 * else
				 *     p_{i}(j) = p_{i-1}(j)
				 */
				if (2 * p[i-2][j] >= i - 1) { 
					p[i-1][j] = p[i-2][j] + 1;
				} else {
					p[i-1][j] = p[i-2][j];
				}
			}

			/* insert the value c into the center index */
			c = (int)((i - 1) / 2);
			p[i-1][c] = c;

			/* last (i - 1)/2 + 1 index */
			for (j = (i - 1) / 2 + 1; j < i; j++) {
				/*
				 * if p_{i-1}(j-1) >= (i - 1) / 2
				 *     p_{i}(j) = p_{i-1}(j-1) + 1
				 * else
				 *     p_{i}(j) = p_{i-1}(j-1)
				 */
				if (2 * p[i-2][j-1] >= i - 1) { 
					p[i-1][j] = p[i-2][j-1] + 1;
				} else {
					p[i-1][j] = p[i-2][j-1];
				}
			}
		} else {		/* even */
			/* Generate first i/2 values taking
			 * p_{i}(j) = 2 p_{i/2}(j)
			 */
			for (j = 0; j < i / 2; j++) {
				p[i-1][j] = 2 * p[i/2 - 1][j];
			}

			/* Replicate the sequence above adding
			 * +1 for each elements and append to the last.
			 */
			for (j = i / 2; j < i; j++) {
				p[i-1][j] = p[i - 1][j - i / 2] + 1;
			}
		}
	}

	return p;
}

/*
 * Function: vdC
 *
 *     Returns van der Courput sequence.
 *
 * Parameters:
 *
 *       i    - Index.
 *       base - The base to generate.
 *
 * Returns:
 *
 *      i'th value of van der Courput sequence in base base.
 *
 *
 * Reference:
 *
 *     - Harald Niederreiter,
 *       Random Number Generation and Quasi-Monte Carlo Methods
 *       (Cbms-Nsf Regional Conference Series in Applied Mathematics, No 63),
 *       SIAM, Pennsylvania. ISBN: 0898712955.
 */
double
vdC(int i, int base)
{
	double h=0.0, f, factor;
	int    digit;
	
	f = factor = 1.0/(double)base;

	while (i>0) {
		digit = i % base;
		
		h += (double)digit * factor;
		i /= base;
		factor *= f;
	}
  
	return h;
}

/*
 * Function: generalized_vdC
 *
 *     Generates generalized van der Courput sequence.
 *
 * Parameters:
 *
 *       i    - Index.
 *       base - The base to generate.
 *     **p    - Permutation table.
 *
 * Returns:
 *
 *      i'th value of generalized van der Courput sequence in base base.
 *
 * See Also:
 *
 *      <faure_permutation>
 *
 * Reference:
 *
 *     - Faure Henri,
 *       Good permutations for extreme discrepancy,
 *       J. Number Theory 42, no. 1, 47--56, 1992
 *
 *     - Harald Niederreiter,
 *       Random Number Generation and Quasi-Monte Carlo Methods
 *       (Cbms-Nsf Regional Conference Series in Applied Mathematics, No 63),
 *       SIAM, Pennsylvania. ISBN: 0898712955.
 */
double
generalized_vdC(int i, int base, int **p)
{
	double h=0.0, f, factor;
	int    perm;
	int    digit;
	
	f = factor = 1.0/(double)base;

	while (i>0) {
		digit = i % base;
		
		/* Lookup permutation table. */
		perm = p[base-1][digit];

		h += (double)perm * factor;
		i /= base;
		factor *= f;
	}
  
	return h;
}

/*
 * Function: generalized_scrambled_halton
 *
 *     Returns generalized scrambled Halton sequence.
 *
 * Parameters:
 *
 *       i      - Instance number.
 *       offset - Offset index.
 *       dim    - Dimension.
 *     **p      - Permutation table.
 *
 * Returns:
 *
 *      (i+offset)'th value of generalized scrambled Halton sequence
 *      in dimension dim.
 *
 * See Also:
 *
 *      <generalized_vdC>
 *
 * Reference:
 *
 *     - Alexander Keller,
 *       Strictly Deterministic Sampling Methods in Computer Graphics 
 *       (mental images technical report, 2001)
 *       in "Monte Carlo Ray Tracing", SIGGRAPH'2003 Course #44.
 */
double
generalized_scrambled_halton(int i, int offset, int dim, int **p)
{
	int    prim;
	double val;
	
	if (dim < 1) dim = 1;
	if (dim >= nprimes) {
		fprintf(stderr, "too big dimension: dim = %d", dim);
		dim = nprimes - 1;
	}

	/* dim'th prime number. */
	prim = primes[dim];
	val = generalized_vdC(i + offset, prim, p);

	return val;
}

/*
 * Function: generalized_scrambled_hammersley
 *
 *     Returns generalized scrambled Hammersley point set.
 *
 * Parameters:
 *
 *       i      - Instance number.
 *       offset - Offset index.
 *       n      - number of samples.
 *       dim    - Dimension.
 *     **p      - Permutation table.
 *
 * Returns:
 *
 *      (i+offset)'th value of generalized scrambled Hammersley point set
 *      in dimension dim.
 *
 * See Also:
 *
 *      <generalized_vdC>
 *
 * Reference:
 *
 *     - Alexander Keller,
 *       Strictly Deterministic Sampling Methods in Computer Graphics 
 *       (mental images technical report, 2001)
 *       in "Monte Carlo Ray Tracing", SIGGRAPH'2003 Course #44.
 */
double
generalized_scrambled_hammersley(int i, int offset, int n, int dim, int **p)
{
	int    prim;
	int    j;
	double val;
	
	if (dim < 1) dim = 1;
	if (dim > nprimes) {
		fprintf(stderr, "too big dimension: dim = %d", dim);
		dim = nprimes;
	}

	j = i + offset;
	if (j > n) {
		j = (i + offset) % n;	
	}

	if (dim == 1) {
		val = (double)(i + offset) / (double)n;
	} else {
		/* (dim-1)'th prime number. */
		prim = primes[dim - 1];
		val = generalized_vdC(j, prim, p);
	}

	return val;
}

/*
 * Function: fibonacci_lattice_2D
 *
 *     Generates 2D([0,1)^2) sampling points using the Fibonacci lattices.
 *     Number of points generated are F_{k}. F_{k} is k'th Fibonacci number.
 *     *x must have enough memory capable of F_{k} - 1 points.
 *
 *     The equations is 
 * 
 *     P_{i} = (i / F_{k}, [ i * F_{k-1} / F_{k} ]),
 *
 *     where [] means fractional part.
 *
 *     Here for example k = 7 (F_{k-1} = 8, F_{k} = 13).
 *     Generated 12 points are
 *     
 *     - P_{ 1} = (0.000000, 0.000000)
 *     - P_{ 2} = (0.076923, 0.615385)
 *     - P_{ 3} = (0.153846, 0.230769)
 *     - P_{ 4} = (0.230769, 0.846154)
 *     - P_{ 5} = (0.307692, 0.461539)
 *     - P_{ 6} = (0.384615, 0.076923)
 *     - P_{ 7} = (0.461538, 0.692308)
 *     - P_{ 8] = (0.538462, 0.307693)
 *     - P_{ 9} = (0.615385, 0.923077)
 *     - P_{10} = (0.692308, 0.538462)
 *     - P_{11} = (0.769231, 0.153846)
 *     - P_{12} = (0.846154, 0.769231).
 *
 * Parameters:
 *
 *     *x - 2D sampling points generated.
 *      k - The index of the Fibonacci number.
 *
 * Returns:
 *
 *      None.
 *
 * See Also:
 *
 *      <fibonacci>
 *
 * Reference:
 *
 *     - I. H. Sloan and S. Joe,
 *       Lattice methods for multiple integration,
 *       Oxford University Press, 1994.
 */
void
fibonacci_lattice_2D(double *x, int k)
{
	int i;
	int f1 = fibonacci(k - 1);
	int f2 = fibonacci(k);

	if (k < 3) {
		fprintf(stderr, "k must be >= 3.\n");
		return;
	}

	for (i = 0; i < f2 - 1; i++) {
		x[2 * i + 0] = (double)i / (double)f2;
		x[2 * i + 1] = mod_1((double)(i * f1) / (double)f2);
	}
}

/* x = x mod 1 */
double
mod_1(double x)
{
	double v = x - floor(x);

	assert(v >= 0.0);
	assert(v < 1.0);

	return v;
}

/* --- private functions --- */

/* returns k'th fibonacci number. */
static int 
fibonacci(int k)
{
	if (k < 3) return 1;

	return fibonacci(k - 1) + fibonacci(k - 2);
}

#if 0
int
main(int argc, char **argv)
{
	int i;
	double px, py;

	for (i = 1; i < 1000; i++) {
		px = RI_vdC(i, 0);
		py = RI_vdC(i, 0);
		printf("%d %f %f\n", i, px, py);
	}
}
#endif

