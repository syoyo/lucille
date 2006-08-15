/*
 * MonteCarlo methods.
 *
 * $Id: mc.h,v 1.1 2004/01/26 08:35:00 syoyo Exp $
 */

#ifndef MC_H
#define MC_H

#ifdef __cplusplus
extern "C" {
#endif

/* Latin hyper cube sampling. */
extern void     latin_hyper_cube(double *out, int n, int d);


#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
