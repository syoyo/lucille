/*
 * multipole expansion of light vector.
 *
 * $Id: multipolelight.h,v 1.1.1.1 2004/01/06 13:57:09 syoyo Exp $
 */

#ifndef MULTIPOLELIGHT_H
#define MULTOPOLELIGHT_H

#include "vector.h"

#ifdef __cplusplus
extern "C" {
#endif

extern void ri_multipolelight_rectangular(ri_vector_t  outirrad[3],
			                  ri_vector_t  inirrad,
			                  ri_vector_t  v,
			                  double       l,
				          double       w);
extern void ri_exact_rectangular(ri_vector_t  outirrad[3],
			         ri_vector_t  inirrad,
			         ri_vector_t  v,
			         double       l,
				 double       w);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

