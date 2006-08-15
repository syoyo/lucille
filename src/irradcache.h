/*
 * irradiance cache routine.
 *
 * $Id: irradcache.h,v 1.4 2004/06/13 06:44:51 syoyo Exp $
 */

#ifndef IRRADCACHE_H
#define IRRADCACHE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "octree.h"
#include "octree_frisken.h"
#include "vector.h"
#include "raytrace.h"

/*
 * If you want to know irradiance cache algorithm,
 * plese refer 
 * "A Ray Tracing Solution for Diffuse Interreflection"
 * Greg Ward, SIGGRAPH 1988
 *
 * "Irradiance Gradients"
 * Greg Ward, Third Eurographics Workshop on Rendering, 1992
 */

#define MAX_HEMISAMPLE 128

typedef struct _ri_irradcache_t
{
	ri_vector_t p;		/* position			*/
	ri_vector_t n;		/* normal			*/
	ri_vector_t e;		/* irradiance			*/
	
	ri_vector_t tg;		/* translational gradients	*/		
	ri_vector_t rg;		/* rotational gradients		*/		
	float	    r;		/* radius			*/
} ri_irradcache_t;

typedef struct _ri_irradcache_octree_t
{
	int dummy;
} ri_irradcache_octree_t;

typedef struct _ri_hemisample_t
{
	double      r;			/* 1 / distance		*/
	ri_vector_t L;			/* incident light	*/
} ri_hemisample_t;

typedef struct _ri_hemisphere_t
{
	unsigned int ntheta, nphi;
	ri_vector_t  basis[3];
	double       rtotal;
	double       rmean;
	ri_hemisample_t samples[MAX_HEMISAMPLE][3 * MAX_HEMISAMPLE];
					/* [theta][phi]	*/
} ri_hemisphere_t;

/*
 * irradiance cache value is stored in octree deta structure
 */

extern int    ri_irradcache_insert(ri_octree_t  *octree, ri_irradcache_t *val);
extern double ri_irradcache_find  (ri_octree_t  *octree,
			           const ri_vector_t *pos,
				   const ri_vector_t *normal,
				   ri_vector_t  *irradiance,
				   unsigned int *nfound);
extern void   ri_irradcache_calc_cache(ri_irradcache_t *cache,
				       const ri_ray_t *ray,
				       const ri_surface_info_t *surfinfo);
extern void   ri_irradcache_calc_gradient(ri_vector_t *tg, ri_vector_t *rg,
					  const ri_hemisphere_t *hemi);
extern void   ri_irradcache_dump  (ri_octree_t  *octree);
extern void   ri_irradcache_load  (ri_octree_t  *octree, const char *cachefile);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
