/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Whitted style classic raytracer.
 *
 */

#ifndef LUCILLE_WHITTED_H
#define LUCILLE_WHITTED_H

#include "render.h"
#include "raytrace.h"

#include "transport.h"

#ifdef __cplusplus
extern "C" {
#endif

extern int  ri_transport_whitted(
    ri_render_t         *render,
    const ri_ray_t      *ray,
    ri_transport_info_t *result);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif  /* LUCILLE_WHITTED_H */
