/*
 * Light transport routine.
 *
 * $Id: transport.h,v 1.3 2004/05/08 07:01:08 syoyo Exp $
 */

#ifndef TRANSPORT_H
#define TRANSPORT_H

#include "render.h"
#include "raytrace.h"

#ifdef __cplusplus
extern "C" {
#endif

#define TRANSPORT_MCRAYTRACE 0	/* Monte Carlo raytracing	*/
#define TRANSPORT_MLT        1	/* Metropolis Light Transport	*/
#define TRANSPORT_PATHTRACE  2  /* Path tracing			*/


typedef struct _ri_transport_info_t
{
	int                nbound_specular;
	int                nbound_diffuse;
	int                hit;
	ri_surface_info_t  surfinfo;
	ri_vector_t        radiance;
	ri_ray_t           ray;
} ri_transport_info_t;

/* Sample the path of light tranport. */
extern int          ri_transport_sample(ri_render_t *render,
					const ri_ray_t *ray,
					ri_transport_info_t *result);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

