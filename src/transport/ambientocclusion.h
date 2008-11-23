/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Ambient Occlusion renderr.
 *
 */

#ifndef LUCILLE_AMBIENTOCCLUSION_H
#define LUCILLE_AMBIENTOCCLUSION_H

#include "render.h"
#include "raytrace.h"

#include "transport.h"

#ifdef __cplusplus
extern "C" {
#endif

extern int  ri_transport_ambientocclusion(
    ri_render_t         *render,
    const ri_ray_t      *ray,
    ri_transport_info_t *result);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif  /* LUCILLE_AMBIENTOCCLUSION_H */
