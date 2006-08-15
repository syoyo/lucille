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
#include "irradcache.h"

#ifdef __cplusplus
extern "C" {
#endif

/* cosine weighted sampling(or stratified sampling + ) */
extern void ri_ibl_sample_cosweight(ri_vector_t *power, ri_vector_t *normal,
				    int nsamples, const ri_ray_t *inray,
				    const ri_vector_t *pos,
				    const ri_vector_t *eye,
				    const ri_light_t *light);

/* importance sampling */
extern void ri_ibl_sample_importance(ri_vector_t *power, ri_hemisphere_t *hemi,
				     int nsamples,
				     const ri_vector_t *pos,
				     const ri_vector_t *eye,
				     const ri_light_t *light);

/* stratified importance sampling */
extern void ri_ibl_sample_stratified(ri_vector_t *power, ri_hemisphere_t *hemi,
				     int nsamples,
				     const ri_vector_t *pos,
				     const ri_vector_t *eye,
				     const ri_light_t *light);

/* structured importance sampling */
extern void ri_ibl_sample_structured(ri_vector_t *power, ri_hemisphere_t *hemi,
				     int nsamples,
				     const ri_vector_t *pos,
				     const ri_vector_t *eye,
				     const ri_light_t *light);

/* EIHDRI(Efficient Illumination by High Dynamic Range Images) sampling */
extern void ri_ibl_sample_eihdri(ri_vector_t *power, ri_hemisphere_t *hemi,
				 int nsamples,
				 const ri_vector_t *pos,
				 const ri_vector_t *eye,
				 const ri_light_t *light);

/* constant colored domelight lighting */
extern void ri_domelight_sample(ri_vector_t *power, ri_hemisphere_t *hemi,
				int nsamples, const ri_ray_t *inray,
				const ri_vector_t *pos,
				const ri_vector_t *eye,
				const ri_light_t *light);

/* exact hemisphere sampling */
extern void ri_ibl_sample_bruteforce(ri_vector_t *power, ri_hemisphere_t *hemi,
				    int nsamples,
				    const ri_vector_t *pos,
				    const ri_vector_t *eye,
				    const ri_light_t *light);

#ifdef __cplusplus
}
#endif

#endif
