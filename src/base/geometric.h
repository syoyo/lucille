/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */


/*
 * Geometric routine
 */

#include "common.h"
#include "vector.h"

/* Calculates the area of triangle. */
extern ri_float_t ri_area(
                        const ri_vector_t v0,
                        const ri_vector_t v1,
                        const ri_vector_t v2);

extern void ri_normal_of_triangle(
                        ri_vector_t       normal_out,/* [out] */
                        const ri_vector_t v0,
                        const ri_vector_t v1,
                        const ri_vector_t v2);

extern void       ri_lerp_vector(
                        ri_vector_t       n_out,
                        const ri_vector_t n0,
                        const ri_vector_t n1,
                        const ri_vector_t n2,
                        ri_float_t u,
                        ri_float_t v );
