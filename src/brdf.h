#ifndef BRDF_H
#define BRDF_H

#include "vector.h"

#ifdef __cplusplus
extern "C" {
#endif

extern double ri_brdf_lambert(const ri_vector_t *wo,
			      const ri_vector_t *wi,
			      const ri_vector_t *n,
			      double kd);
extern double ri_brdf_blinn(const ri_vector_t *wo,
			    const ri_vector_t *wi,
			    const ri_vector_t *n,
			    double kd, double ks,
			    double glossness);
extern double ri_brdf_phong(const ri_vector_t *wo,
			    const ri_vector_t *wi,
			    const ri_vector_t *n, 
			    double kd, double ks,
			    double glossness);
extern double ri_brdf_modified_phong(const ri_vector_t *wo,
				     const ri_vector_t *wi,
				     const ri_vector_t *n,
				     double kd, double ks,
				     double glossness);
extern double ri_brdf_ward_anisotropic(const ri_vector_t *wo,
				       const ri_vector_t *wi,
				       const ri_vector_t *n,
				       const ri_vector_t *u,
				       const ri_vector_t *v,
				       double kd, double ks,
				       double ax, double ay);
extern double ri_brdf_ashikhmin_shirley(const ri_vector_t *wo,
					const ri_vector_t *wi,
					const ri_vector_t *n,
					const ri_vector_t *u,
					const ri_vector_t *v,
					double kd, double ks,
					double nu, double nv);

/* PDF functions for importance sampling in MC raytracing */

/* generate specular ray direction and diffuse ray direction
 * according to Scattering Probability Function(SPF) of Ashikhmin-Shirley
 * BRDF.
 * This is used for Monte Carlo ray tracing.
 */
extern void   ri_brdf_ashikhmin_shirley_spf(ri_vector_t *specularray,
					    ri_vector_t *diffuseray,
					    double *specularfactor,
					    double *diffusefactor,
					    const ri_vector_t *dir,
					    double nu, double nv);

/* importance sampling of modified Phong BRDF. 
 *   output : direction *wo and its PDF *pdf.
 *   input  : input direction *wi, normal *n, random number (u0, u1) and
 *            glossness parameter of modified Phong BRDF.
 */
extern void ri_sample_modified_phong(ri_vector_t *wo,
				     double *pdf,
				     const ri_vector_t *wi,
				     const ri_vector_t *n,
				     double u0, double u1,
				     double glossness);

#ifdef __cplusplus
}
#endif

#endif
