/*
 * photon map method.
 *
 * $Id: photonmap.h,v 1.2 2004/02/08 16:38:48 syoyo Exp $
 */

#ifndef PHOTONMAP_H
#define PHOTONMAP_H

#ifdef __cplusplus
extern "C" {
#endif

#include "array.h"
#include "vector.h"
#include "light.h"
//#include "render.h"

typedef struct _ri_photonmap_option_t
{
	unsigned int nphotons;			/* number of photons to shoot */
	unsigned int max_gather_photons;    	
	float        max_gather_radius;	
	ri_vector_t  wattage;			/* light source wattage */
	int          precompute_irradiance;
} ri_photonmap_option_t;


typedef struct _ri_photon_t
{
	ri_vector_t    pos;		/* 3D position			*/
	char	       phi, theta;	/* quantized angle		*/
	float	       power[3];	/* todo: real pixelize		*/
	unsigned short plane;		/* for kd-tree			*/

	/* Extra information for precomputing irradiance to speed up
	 * final gathering
	 */
	unsigned char  irrad[4];	/* irradiance in RGBE format	*/ 
	char           nphi, ntheta;	/* surface normal		*/ 
} ri_photon_t;

typedef struct _ri_photonmap_t
{
	ri_array_t    *tmp_array;   /* temporary photon array.
				       if ri_photonmap_balance() is called,
				       this array is freed and not used */
	ri_photon_t   *photons;     /* actual photon array used after
				       ri_photonmap_balance() call */
	unsigned long  nphotons;    /* number of photons	*/
	int	       balanced;    /* is array balanced kd-tree */
	float 	       bbox_min[3]; /* min bounding box		*/
	float 	       bbox_max[3]; /* max bounding box		*/
} ri_photonmap_t;

typedef struct _ri_nearest_photons_t
{
	int            max;
	int            found;	    /* found nearest photons	*/
	int            got_heap;
	ri_vector_t    pos;	    /* center position		*/
	float         *dist2;	    /* squared distance array	*/
	ri_photon_t  **index;
} ri_nearest_photons_t;

extern ri_photon_t *ri_photon_new ();
extern void	    ri_photon_free(ri_photon_t *photon);

extern ri_photonmap_t *ri_photonmap_new       ();
extern void	       ri_photonmap_free      (ri_photonmap_t *map);
extern void	       ri_photonmap_add_photon(ri_photonmap_t *map,
					       ri_photon_t    *photon);
/* create left-balanced kd-tree'ed photon array */
extern void	       ri_photonmap_balance   (ri_photonmap_t *map);

extern void	       ri_photonmap_emit_photons(ri_photonmap_t *cmap,
						 ri_photonmap_t *gmap,
						 ri_light_t     *light);
extern void	       ri_photonmap_estimate_irradiance(ri_photonmap_t *map,
							ri_vector_t *irrad,
							const ri_vector_t *pos,
							const ri_vector_t *normal,
							float        radius,
							int          maxphoton);
extern void	       ri_photonmap_estimate_precomputed_irradiance(
						ri_photonmap_t *map,
						ri_vector_t *irrad,
						const ri_vector_t *pos,
						const ri_vector_t *normal);

extern ri_photonmap_option_t *ri_photonmap_get_option();

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
