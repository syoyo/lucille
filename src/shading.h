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

/* shade a point ray.org. */
extern void ri_shade(ri_vector_t *radiance,
		     const ri_vector_t *eye,
		     ri_light_t *light,
		     const ri_ray_t *ray,
		     ri_surface_info_t *surfinfo);

extern void ri_shade_indirect(ri_vector_t *power,	
		 	      ri_photonmap_t *photonmap,
		 	      ri_octree_t *irradcachetree,
		  	      const ri_ray_t *v,
		  	      int nsample,
			      float gather_radius,
			      int gather_num,
			      int fixicache, int forcecalc);
extern void ri_shade_statistics();

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

