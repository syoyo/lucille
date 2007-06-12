/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/* File: vector.c
 *
 *   This file contains vector operation routines.
 *   In lucille, vector data is represented by 4 component.
 *   This is for SIMD coding. Although, it is sometimes a waste of
 *   memory when vector operation needs only 3 components(e.g. x, y and z).
 *
 */

/*
 * One big coding restrictions in lucille.
 * You should pass vector data structure as pointer always!!
 *
 * e.g.
 *
 *     void func(ri_vector_t *arg);
 *
 * This is due to some compiler's limitation when using SIMD data type.
 * (stack alignment problem)
 */

/*
 *    To exploid benefits from SIMD and ordinal array based vector storage,
 *    lucille has 2 member in ri_vector_t(lucille's native vector data
 *    structure).
 *
 *    If you turn on WITH_SSE or WITH_ALTIVEC define, you can access
 *    native vector data type(in SSE, it is __m128. in AltiVec, it is vector
 *    float) as member 'v'(e.g. vect->v).
 *
 *    If you want access vector compoennt through ordinally array based style,
 *    you can do it through member 'e'(e.g. vect->e[0]). This is also true when
 *    WITH_SSE or WITH_ALTIVEC is on.
 *
 */
/*
 * $Id: vector.c,v 1.6 2004/07/07 11:06:41 syoyo Exp $
 */


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <math.h>
#include <stdio.h>

#include "vector.h"

void
ri_vector_print(
	const ri_vector_t vec )
{
	printf( "%f, %f, %f, %f\n",
	       vec.f[0], vec.f[1], vec.f[2], vec.f[3] );
}
