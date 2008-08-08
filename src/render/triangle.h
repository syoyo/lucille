/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Triangle data structure and routines. Used by BVH and beam/ray tracing.
 *
 * $Id: triangle.h 259 2008-08-01 16:28:06Z syoyo $
 *
 */
#ifndef LUCILLE_TRIANGLE_H
#define LUCILLE_TRIANGLE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "geom.h"

typedef struct _ri_triangle_t {

    ri_vector_t v[3];

    ri_geom_t  *geom;
    uint32_t    index;

} ri_triangle_t;

/*
 * Ray-Triangle intersection test
 */
extern int ri_triangle_isect(       uint32_t      *tid_inout,
                                    ri_float_t    *t_inout,
                                    ri_float_t    *u_inout,
                                    ri_float_t    *v_inout,
                              const ri_triangle_t *triangle,
                                    ri_vector_t    rayorg,
                                    ri_vector_t    raydir,
                                    uint32_t       tid);

/*
 * Calculate distant to the plane defined by the triangle.
 */
extern int ri_triangle_plane_dist(  
                                    ri_float_t    *t_inout,
                              const ri_triangle_t *triangle,
                                    ri_vector_t    rayorg,
                                    ri_vector_t    raydir);

#ifdef __cplusplus
}
#endif

#endif  /* LUCILLE_TRIANGLE_H */
