/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

#ifndef LUCILLE_RAY_H
#define LUCILLE_RAY_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vector.h"

/*
 * Struct: ri_ray_t
 *
 *     Structure for ray object.
 *
 */
typedef struct _ri_ray_t {

	ri_vector_t org;		/* Position			*/
	ri_vector_t dir;		/* Direction			*/
	int         nbound_diffuse;	/* number of diffuse bounds	*/
	int         nbound_specular;	/* number of specular bounds	*/

	float       t;			/* dist from nearest hit pos	*/
	float       max_t;		/* interval of t		*/
	float       min_t;

	float       weight;		/* weight			*/
	char        prev_hit;		/* 'E','S','D',or 'L'		*/

	/*
	 * quasi-Monte Carlo related variables.
	 */
	int         d;			/* current dimension		*/
	int         i;			/* instance number		*/

	/*
	 * variables for multi-threading
	 */
	int         thread_num;		/* thread number.
					 * If multi-threading feature
					 * is turned on this value
					 * should be set		*/

#if 0
	/* Ray differentials for better texture mapping, etc.
	 * TODO: This feature is not yet implemented.
	 */
	ri_vector_t dPdx;		/*  dP / dx	*/
	ri_vector_t dPdy;		/*  dP / dy	*/
	ri_vector_t dDdx;		/*  dD / dx	*/
	ri_vector_t dDdy;		/*  dD / dy	*/
#endif

} ri_ray_t;

/*
 * Function: ri_ray_copy
 *
 *     Copy ray object. it is equivalent to call memcpy.
 *
 * Parameters:
 *
 *     dst - a ray object to be overwritten.
 *     src - a ray object to be copied.
 *
 * Return
 *
 *     None.
 */
extern void	ri_ray_copy(
	ri_ray_t       *dst,
	const ri_ray_t *src );

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
extern void	ri_ray_perturb(
	ri_ray_t *ray );

#ifdef __cplusplus
}       /* extern "C" */
#endif

#endif  /* LUCILLE_RAY_H */
