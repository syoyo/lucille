/*
 *   lucille | Global Illumination render
 *
 *             written by Syoyo.
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "ray.h"
#include "memory.h"

/* TODO: eps should be choosen according to scene scale. */
#define RAY_EPSILON    1.0e-4f

/*
 * Function: ri_ray_copy
 *
 *     Copy ray object. it is equivalent to call memcpy.
 *
 * Parameters:
 *
 *     dst - a ray object to be filled.
 *     src - a ray object to be copied.
 *
 * Return
 *
 *     None.
 */
void
ri_ray_copy(
	ri_ray_t       *dst,
	const ri_ray_t *src )
{
	ri_mem_copy( dst, src, sizeof( ri_ray_t ) );
}

/*
 * Function: ri_ray_perturb
 *
 *     Slightly Move ray's origin towards ray's direction to work around for
 *     numerical err.
 *
 * Parameters:
 *
 *     ray - a ray object. *ray*'s origin is changed.
 *
 * Return
 *
 *     None.
 */
void
ri_ray_perturb(
	ri_ray_t *ray )
{
	ray->org[0] += RAY_EPSILON * ray->dir[0];
	ray->org[1] += RAY_EPSILON * ray->dir[1];
	ray->org[2] += RAY_EPSILON * ray->dir[2];
}
