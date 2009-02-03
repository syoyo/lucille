/*
 * Quasi-MonteCarlo routine.
 *
 * $Id: qmc.h,v 1.5 2004/01/28 16:15:44 syoyo Exp $
 */

#ifndef QMC_H
#define QMC_H

#ifdef __cplusplus
extern "C" {
#endif

/* Halton sequence for base 'd' */
extern double   halton(int i, int d);

/* Halton sequence with base 2 */
extern double   halton2(int i);

extern void     hammersley2(double *out, int n);
extern void     hammersley2_sphere(double *out, int n);

/* Generates Faure's permutation. */
extern int    **faure_permutation(int n);

/* van der Corput sequence. */
extern double   vdC(int i, int base);

/* Generalized van der Corput sequence. */
extern double   generalized_vdC(int i, int base, int **p);

/* Generalized scrambled Halton sequence. */
extern double   generalized_scrambled_halton(int i, int offset,
					     int dim, int **p);
/* Generalized scrambled Hammersley point set. */
extern double   generalized_scrambled_hammersley(int i, int offset,
					         int n, int dim, int **p);

/* Generate Fibonacci latice points in [0, 1)^2 */
extern void fibonacci_lattice_2D(double *x, int k);

/* x = x mod 1. */
extern double mod_1(double x);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
