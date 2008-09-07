/*
 *   lucille | Global Illumination render
 *
 *             written by Syoyo.
 *
 */

/*
 *   File: intersection_state.h
 *
 *       Header file for intersection state structure.
 */
#ifndef INTERSECTION_STATE_H
#define INTERSECTION_STATE_H

#ifdef __cplusplus
extern "C"
{
#endif

#include "vector.h"
#include "brdf.h"
#include "geom.h"


/*
 * Struct: ri_intersection_state_t
 *
 *     Structure ri_intersection_state_t stores a information of hit point
 *     on the surface which is found by raytracing.
 *
 */

typedef struct _ri_intersection_state_t {

    ri_vector_t     P;              /* position                     */
    ri_vector_t     Ng;             /* geometry normal              */
    ri_vector_t     Ns;             /* shading normal               */
    ri_vector_t     I;              /* ray's incoming direction     */
    double          t;              /* distance to the hit point
                                     * from ray origin.             */

    ri_brdf_t      *fr;             /* BRDF                         */

    char            inside;         /* the ray hits the surface
                                     * from back face?        */

    ri_geom_t      *geom;
    uint32_t        index;

    ri_vector_t     color;

    ri_vector_t     tangent;        /* tangent vector               */
    ri_vector_t     binormal;       /* binormal vector              */

    ri_vector_t     stqr;           /* interpolated texture coord   */

    ri_float_t      u, v;           /* barycentric coord            */

} ri_intersection_state_t;

extern ri_intersection_state_t *ri_intersection_state_new();

extern void                     ri_intersection_state_free();
extern void                     ri_intersection_state_clear(
    ri_intersection_state_t *state_inout);  /* [inout] */

extern void                     ri_intersection_state_build(
    ri_intersection_state_t *state_inout,   /* [inout] */
    const ri_vector_t        eye,
    const ri_vector_t        dir);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif                                 /* INTERSECTION_STATE_H */
