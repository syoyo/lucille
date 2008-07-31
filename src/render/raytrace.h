/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * ray tracing routine.
 *
 * $Id: raytrace.h,v 1.5 2004/06/13 06:44:51 syoyo Exp $
 */

#ifndef LUCILLE_RAYTRACE_H
#define LUCILLE_RAYTRACE_H

#include "ri.h"
#include "array.h"
#include "vector.h"
#include "render.h"
#include "geom.h"
#include "ray.h"
#include "intersection_state.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Function: ri_raytrace
 *
 *     Traces the ray into the scene and if intersection is found,
 *     store the information of intersection point on the surface into *state*.
 *
 * Parameters:
 *
 *     render - render object
 *     ray    - ray object to be traced into the scene.
 *     state  - the information of hitted point.
 *
 * Returns:
 *
 *     1 if the ray hits a point onto the surface. 0 if the ray hits
 *     environment or no hit point found.
 */
extern int    ri_raytrace(
    ri_render_t             *render,            /* [inout]      */
    ri_ray_t                *ray,               /* [inout]      */
    ri_intersection_state_t *state_out );       /* [out]        */


extern void    ri_raytrace_shutdown();

/*
 * Function: ri_raytrace_statistics
 *
 *     Show ray tracing statistics.
 *
 * Parameters:
 *
 *     None.
 *
 * Returns:
 *
 *     None.
 */
extern void    ri_raytrace_statistics();

#ifdef __cplusplus
}       /* extern "C" */
#endif

#endif  /* LUCILLE_RAYTRACE_H */
