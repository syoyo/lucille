/*
 * Image-Based Lighting routine.
 *
 * Copyright Syoyo FUJITA
 *
 * $Id: ibl.h,v 1.5 2004/06/13 06:44:51 syoyo Exp $
 */

#ifndef IBL_H
#define IBL_H

#include "vector.h"
#include "light.h"
#include "raytrace.h"

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_HEMISAMPLE 128

typedef struct _ri_hemisample_t
{
	double      r;			/* 1 / distance		*/
	ri_vector_t L;			/* incident light	*/
} ri_hemisample_t;

typedef struct _ri_hemisphere_t
{
	unsigned int    ntheta;
    unsigned int    nphi;
	ri_vector_t     basis[3];
	double          rtotal;
	double          rmean;
	ri_hemisample_t samples[MAX_HEMISAMPLE][3 * MAX_HEMISAMPLE];
					/* [theta][phi]	*/
} ri_hemisphere_t;

/* cosine weighted sampling(or stratified sampling + ) */
extern void ri_ibl_sample_cosweight(
	ri_vector_t        power,               /* [out] */
	const ri_vector_t  normal,
	int                nsamples,
	const ri_ray_t    *inray,
	const ri_vector_t  pos,
	const ri_vector_t  eye,
	const ri_light_t  *light);

/* importance sampling */
extern void ri_ibl_sample_importance(
	ri_vector_t        power,               /* [out] */
	ri_hemisphere_t   *hemi,
	int                nsamples,
	const ri_vector_t  pos,
	const ri_vector_t  eye,
	const ri_light_t  *light);

/* stratified importance sampling */
extern void ri_ibl_sample_stratified(
	ri_vector_t        power,               /* [out] */
	ri_hemisphere_t   *hemi,
	int                nsamples,
	const ri_vector_t  pos,
	const ri_vector_t  eye,
	const ri_light_t  *light);

/* structured importance sampling */
extern void ri_ibl_sample_structured(
	ri_vector_t        power,               /* [out] */
	ri_hemisphere_t   *hemi,
	int                nsamples,
	const ri_vector_t  pos,
	const ri_vector_t  eye,
	const ri_light_t  *light);

/* EIHDRI(Efficient Illumination by High Dynamic Range Images) sampling */
extern void ri_ibl_sample_eihdri(
	ri_vector_t        power,               /* [out] */
	ri_hemisphere_t   *hemi,
	int                nsamples,
	const ri_vector_t  pos,
	const ri_vector_t  eye,
	const ri_light_t  *light);

/* constant colored domelight lighting */
extern void ri_domelight_sample(
	ri_vector_t        power,               /* [out] */
	ri_hemisphere_t   *hemi,
	int                nsamples,
	const ri_ray_t    *inray,
	const ri_vector_t  pos,
	const ri_vector_t  eye,
	const ri_light_t  *light);

/* exact hemisphere sampling */
extern void ri_ibl_sample_bruteforce(
	ri_vector_t        power,               /* [out] */
	ri_hemisphere_t   *hemi,
	int                nsamples,
	const ri_vector_t  pos,
	const ri_vector_t  eye,
	const ri_light_t  *light);

#ifdef __cplusplus
}
#endif

#endif
