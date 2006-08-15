/*
 * uniformly distributed random number generation with
 * twice faster version of Mersenne Twister.
 *
 * $Id: random.h,v 1.2 2004/06/29 13:03:33 syoyo Exp $
 */

#ifndef RANDOM_H
#define RANDOM_H

#include "vector.h"

#ifdef __cplusplus
extern "C" {
#endif

extern void	     seedMT  (unsigned long seed);
extern double    randomMT(void);

/* faster multi-thread version without locking */
extern double    randomMT2(int thread_id);

extern void      random_uniform_vector(ri_vector_t *dst);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
