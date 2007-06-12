#ifndef BRDF_H
#define BRDF_H

#include "vector.h"

#ifdef __cplusplus
extern "C" {
#endif

#define RI_BRDF_LAMBERT				0
#define RI_BRDF_BLINN				1
#define RI_BRDF_PHONG				2
#define RI_BRDF_MODIFIED_PHONG			3
#define RI_BRDF_WARD_ANISOTROPIC		4
#define RI_BRDF_ASHIKHMIN_SHIRLEY		5

typedef struct _ri_brdf_param_field_t
{
	const char *param_name;
	int         param_type;

} ri_brdf_param_field_t;

typedef struct _ri_brdf_t
{
	int brdf_type;			/* brdf type
					 * (lambert, blinn, etc...	*/

} ri_brdf_t;

extern ri_float_t ri_brdf_eval(
	const ri_brdf_t   *fr,
	const ri_vector_t *wo,
	const ri_vector_t *wi,
	const ri_vector_t *n);
				

extern ri_float_t ri_brdf_lambert(
	const ri_vector_t *wo,
	const ri_vector_t *wi,
	const ri_vector_t *n,
	ri_float_t         kd);

extern ri_float_t ri_brdf_blinn(
	const ri_vector_t *wo,
	const ri_vector_t *wi,
	const ri_vector_t *n,
	const ri_float_t   kd,
	const ri_float_t   ks,
	ri_float_t         glossness);

extern ri_float_t ri_brdf_phong(
	const ri_vector_t *wo,
	const ri_vector_t *wi,
	const ri_vector_t *n, 
	const ri_float_t   kd,
	const ri_float_t   ks,
	ri_float_t         glossness);

extern ri_float_t ri_brdf_modified_phong(
	const ri_vector_t *wo,
	const ri_vector_t *wi,
	const ri_vector_t *n,
	const ri_float_t   kd,
	const ri_float_t   ks,
	ri_float_t         glossness);

extern ri_float_t ri_brdf_ward_anisotropic(
	const ri_vector_t *wo,
	const ri_vector_t *wi,
	const ri_vector_t *n,
	const ri_vector_t *u,
	const ri_vector_t *v,
	const ri_float_t   kd,
	const ri_float_t   ks,
	const ri_float_t   ax,
	const ri_float_t   ay);

extern ri_float_t ri_brdf_ashikhmin_shirley(
	const ri_vector_t *wo,
	const ri_vector_t *wi,
	const ri_vector_t *n,
	const ri_vector_t *u,
	const ri_vector_t *v,
	const ri_float_t   kd,
	const ri_float_t   ks,
	const ri_float_t   nu,
	const ri_float_t   nv);

/* PDF functions for importance sampling in MC raytracing */

/* 
 * generate specular ray direction and diffuse ray direction
 * according to Scattering Probability Function(SPF) of Ashikhmin-Shirley
 * BRDF.
 * This is used for Monte Carlo ray tracing.
 */
extern void   ri_brdf_ashikhmin_shirley_spf(
	ri_vector_t       *specularray,
	ri_vector_t       *diffuseray,
	ri_float_t        *specularfactor,
	ri_float_t        *diffusefactor,
	const ri_vector_t *dir,
	const ri_float_t   nu,
	const ri_float_t   nv);

/* importance sampling of modified Phong BRDF. 
 *   output : direction *wo and its PDF *pdf.
 *   input  : input direction *wi, normal *n, random number (u0, u1) and
 *            glossness parameter of modified Phong BRDF.
 */
extern void ri_sample_modified_phong(
	ri_vector_t       *wo,
	ri_float_t        *pdf,
	const ri_vector_t *wi,
	const ri_vector_t *n,
	ri_float_t         u0,
	ri_float_t         u1,
	ri_float_t         glossness);

#ifdef __cplusplus
}
#endif

#endif
