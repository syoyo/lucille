/*
 * shader.
 *
 * $Id: shading.h,v 1.3 2004/02/08 16:38:48 syoyo Exp $
 */

#ifndef SHADING_H
#define SHADING_H

#include "render.h"

#ifdef __cplusplus
extern "C" {
#endif

/* shade a point. */
extern void ri_shade(
    ri_vector_t             *Lo,        /* [out] */
    const ri_vector_t       *eye,
    const ri_ray_t          *ray,
    ri_intersection_state_t *state);

#if 0
extern void ri_shade_indirect(
    ri_vector_t *power,    
    const ri_ray_t *v,
    int nsample,
    float gather_radius,
    int gather_num,
    int fixicache, int forcecalc);
#endif

extern void ri_shade_statistics();

#ifdef __cplusplus
}    /* extern "C" */
#endif

#endif

