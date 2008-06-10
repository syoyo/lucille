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
           vec[0], vec[1], vec[2], vec[3] );
}
