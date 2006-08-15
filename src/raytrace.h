/*
 * ray tracing routine.
 *
 * $Id: raytrace.h,v 1.5 2004/06/13 06:44:51 syoyo Exp $
 */

#ifndef RAYTRACING_H
#define RAYTRACING_H

#include "ri.h"
#include "array.h"
#include "vector.h"
#include "render.h"
#include "geom.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _ri_ray_t
{
	ri_vector_t org;		/* Position			*/
	ri_vector_t dir;		/* Direction			*/
	int         nbound_diffuse;	/* number of diffuse bounds	*/
	int         nbound_specular;	/* number of specular bounds	*/
	float       isectt;		/* dist from nearest hit pos	*/
	float       weight;		/* weight			*/
	char        prev_hit;		/* 'E','S','D',or 'L'		*/

	/* quasi-Monte Carlo related variables. */
	int         d;			/* current dimension		*/
	int         i;			/* instance number		*/

	/* multi-thread variables */
	int         thread_num;		/* thread number.
					 * If multi-thread is turned on,
					 * this value should be specified */

#if 0
	/* Ray differentials for better texture mapping, etc.
	 * TODO: This feature is not yet implemented.
	 */
	ri_vector_t dPdx;		/* Partial derivative, dP / dx	*/
	ri_vector_t dPdy;		/* Partial derivative, dP / dy	*/
	ri_vector_t dDdx;		/* Partial derivative, dD / dx	*/
	ri_vector_t dDdy;		/* Partial derivative, dD / dy	*/
#endif
} ri_ray_t;

extern void ri_ray_copy(ri_ray_t *dst, const ri_ray_t *src);

extern int ri_raytrace(ri_render_t *render, ri_ray_t *ray,
		       ri_surface_info_t *info);
extern int ri_raytrace_geom(ri_geom_t *geom, ri_ray_t *ray,
		       ri_surface_info_t *info);
extern void ri_raytrace_setup();
extern void ri_raytrace_shutdown();
extern void ri_raytrace_statistics();
extern int g_profile;		/* TODO: remove this */
extern RtInt triangle_intersect(const ri_vector_t *orig,
				const ri_vector_t *dir,
		 		const ri_vector_t *v0,
				const ri_vector_t *v1,
				const ri_vector_t *v2,
		  		RtFloat *t, RtFloat *u, RtFloat *v);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
