/*
 * uniformly distributed random number generation with
 * twice faster version of Mersenne Twister.
 *
 * $Id: random.h,v 1.1.1.1 2004/01/06 13:57:17 syoyo Exp $
 */

#ifndef RANDOM_H
#define RANDOM_H

#ifdef __cplusplus
extern "C" {
#endif

extern void	     seedMT  (unsigned long seed);
extern double    randomMT(void);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
