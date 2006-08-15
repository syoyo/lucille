#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "vector.h"
#include "util.h"
#include "random.h"
#include "log.h"
#include "qmc.h"

#ifndef M_PI
#define M_PI 3.14159265
#endif

static void calc_tangent_and_binormal(ri_vector_t *tangent,
				      ri_vector_t *binormal,
				      const ri_vector_t *normal);

void
ri_reflect(ri_vector_t *reflect, const ri_vector_t *in, const ri_vector_t *n)
{
	float dot;
	ri_vector_t ndot;

	/*
	 * r = in - 2n(in . n)
	 *                 
	 *                n 
	 *                ^
	 *          |\    |    / 
	 *   reflect  \   |   /  in
	 *             \  | |/
	 *   ----------------------
	 */

	dot = ri_vector_dot3(in, n);
	ri_vector_copy(&ndot, n);
	ri_vector_scale(&ndot, 2 * dot);

	ri_vector_sub(reflect, in, &ndot);
}

/*
 * Function: ri_refract
 *
 *     Calculates refraction vector.
 *
 * Parameters:
 *
 *     *refract - Refraction vector, or reflection vector if total internal
 *                reflection occures.
 *     *in      - Incident vector.
 *     *n       - Normal vector.
 *      eta     - Relative refractive index.
 *
 * Returns:
 *
 *     1 if total internal refrection occures.
 */
int
ri_refract(ri_vector_t *refract, const ri_vector_t *in, const ri_vector_t *n,
	   float eta)
{
	float cos1;
	float coeff;
	ri_vector_t nn;

	/*
	 * k = 1 - eta * eta * (1 - (in.n)^2)
	 * if (k < 0) { 
	 *     total internal reflection
	 * } else {
	 *     eta * in - (eta * in.n +  sqrt(k)) * n
	 * }
	 *           ^
	 *           |    / 
	 *         N |   /  IN
	 *           | |/
	 *   ----------------------
	 *         /
	 *       /   refract
	 *    |/
	 */

	cos1 = ri_vector_dot3(in, n);

	coeff = 1.0 - (eta * eta) * (1.0 - cos1 * cos1);
	if (coeff <= 0.0) {
		/* total internal reflection */
		//printf("total internal reflection\n");
		ri_reflect(refract, in, n);
		return 1;
	}

	coeff = eta * cos1 + sqrt(coeff);

	ri_vector_copy(&nn, n);
	ri_vector_scale(&nn, -coeff);
	ri_vector_copy(refract, in);
	ri_vector_scale(refract, eta);
	ri_vector_add(refract, refract, &nn);

	return 0;
}

void
ri_random_vector_cosweight(ri_vector_t *v, const ri_vector_t *n)
{
	double theta, phi;
	ri_vector_t tn, bn, nn;
	ri_vector_t tb;

	calc_tangent_and_binormal(&tn, &bn, n);

	theta = acos(sqrt(1.0 - randomMT()));
	phi   = 2.0 * M_PI * randomMT();

	ri_vector_copy(&nn, n);

	/* D = T*cos(phi)*sin(theta) + B*sin(phi)*sin(theta) + N*cos(theta) */
	ri_vector_scale(&tn, cos(phi) * sin(theta));
	ri_vector_scale(&bn, sin(phi) * sin(theta));
	ri_vector_scale(&nn, cos(theta));

	ri_vector_add(&tb, &tn, &bn);
	ri_vector_add(v, &tb, &nn);
}

void
ri_random_vector_cosNweight(ri_vector_t *v, double *pdf,
                            const ri_vector_t *n,
			    double u0, double u1, double N)
{
	double cos_theta, sin_theta, phi;
	ri_vector_t tn, bn, nn;
	ri_vector_t tb;

	calc_tangent_and_binormal(&tn, &bn, n);

	cos_theta = pow(u0, 1.0/(N+1));
	sin_theta = sqrt(1.0 - cos_theta * cos_theta);

	phi   = 2.0 * M_PI * u1;

	ri_vector_copy(&nn, n);

	/* D = T*cos(phi)*sin(theta) + B*sin(phi)*sin(theta) + N*cos(theta) */
	ri_vector_scale(&tn, cos(phi) * sin_theta);
	ri_vector_scale(&bn, sin(phi) * sin_theta);
	ri_vector_scale(&nn, cos_theta);

	ri_vector_add(&tb, &tn, &bn);
	ri_vector_add(v, &tb, &nn);

	(*pdf) = (N + 1.0) * pow(cos_theta, N) / (2.0 * M_PI);
}

/* Sample a cosine weighted direction on hemisphere using QMC. */
void
ri_random_vector_cosweight_qmc(ri_vector_t *v, const ri_vector_t *n,
			       int d, int i, int **perm)
{
	double u[2];
	double phi, theta;
	ri_vector_t tn, bn, nn;
	ri_vector_t tb;

	calc_tangent_and_binormal(&tn, &bn, n);

	u[0] = generalized_scrambled_halton(i, 0, d, perm);
	u[1] = generalized_scrambled_halton(i, 0, d, perm);

	theta = acos(sqrt(1.0 - u[0]));
	phi   = 2.0 * M_PI * u[1];
	
	/* D = T*cos(phi)*sin(theta) + B*sin(phi)*sin(theta) + N*cos(theta) */
	ri_vector_scale(&tn, cos(phi) * sin(theta));
	ri_vector_scale(&bn, sin(phi) * sin(theta));
	ri_vector_scale(&nn, cos(theta));

	ri_vector_add(&tb, &tn, &bn);
	ri_vector_add(v, &tb, &nn);
}

void
ri_fresnel(ri_vector_t *r,	/* reclected vector	*/
	   ri_vector_t *t,	/* transmitted vector	*/
	   float *kr,		/* the reflection coeff */
	   float *kt,		/* the refraction coeff */
	   const ri_vector_t *in, const ri_vector_t *n,
	   float eta)		/* relative index of refraction */
{
	double R0;
	double dot;
	double in_dot_n;
	double one_minus_dot;
	double one_minus_dot5;
	ri_vector_t negn;

	/*
	 *                 
	 *                n 
	 *                ^
	 *          |\    |    / 
	 *       r    \   |   /  in
	 *             \  | |/
	 *   ----------------------
	 *               -
	 *             --
	 *           --  
	 *        <-     t
	 *
	 */

	in_dot_n = ri_vector_dot3(in, n);

	if (in_dot_n < 0.0f) {	/* imcoming */
		ri_refract(t, in, n, eta);

		if (in_dot_n < -1.0f) in_dot_n = -1.0f;

		dot = -in_dot_n;			/* dot(-d, n) */

		//fprintf(stderr, "imcoming\n");

	} else {		/* outgoing */
		ri_vector_copy(&negn, n);
		ri_vector_neg(&negn);		/* -n */

		ri_refract(t, in, &negn, 1.0f / eta);

		//dot = ri_vector_dot3(t, n);
		dot = in_dot_n;
	}


	/* Schlick's approximation. */

	one_minus_dot = 1.0 - dot;

	/* (1.0 - dot)^5 */
#if 1
	/* Is this a faster version ? */
	one_minus_dot5 = one_minus_dot * one_minus_dot;
	one_minus_dot5 *= one_minus_dot5;
	one_minus_dot5 *= one_minus_dot;
#else
	one_minus_dot5 = pow(one_minus_dot, 5.0f);
#endif

	R0 = (eta - 1.0) / (eta + 1.0);
	R0 *= R0;
	R0 = 0.1;
	(*kr) = R0 + (1.0 - R0) * one_minus_dot5;
	if ((*kr) > 1.0) {
		ri_log(LOG_WARN, "kr > 1.0: kr = %f", (*kr));
		(*kr) = 1.0;
	}

	if ((*kr) < 0.0) {
		//fprintf(stderr, "kr < 0.0 [kr = %f]\n", (*kr));
		(*kr) = 0.0;
	}

	(*kt) = 1.0 - (*kr);

	ri_reflect(r, in, n);

}

void
ri_hvector(ri_vector_t *h, const ri_vector_t *l, const ri_vector_t *v)
{
	ri_vector_add(h, l, v);	
	ri_vector_normalize(h);
}

void
ri_ortho_basis(ri_vector_t *basis, const ri_vector_t *n)
{
	int i;
	ri_vector_copy(&(basis[2]), n);
	ri_vector_zero(&(basis[1]));

	for (i = 0; i < 3; i++) {
		if (basis[2].e[i] < 0.6 && basis[2].e[i] > -0.6)
			break;
	}

	if (i >= 3) i = 0;
	basis[1].e[i] = 1.0;

	ri_vector_cross3(&(basis[0]), &basis[1], &basis[2]);
	ri_vector_normalize(&(basis[0]));
	ri_vector_cross3(&(basis[1]), &basis[2], &basis[0]);
	ri_vector_normalize(&(basis[1]));
}

/* --- private functions --- */

static void
calc_tangent_and_binormal(ri_vector_t *tangent, ri_vector_t *binormal,
			  const ri_vector_t *normal)
{
	/* codes from renderBitch */
	int    i;
	int    index = -1;
	double min   = RI_INFINITY;
	double val;

	/* find the minor axis of the vector */
	for (i = 0; i < 3; i++) {
		val = fabs(normal->e[i]);
		if (val < min) {
			min = val;
			index = i;
		}
	}	

	if (index == 0) {
		tangent->e[0] =  0.0;
		tangent->e[1] = -normal->e[2];
		tangent->e[2] =  normal->e[1];
		ri_vector_normalize(tangent);

		ri_vector_cross3(binormal, tangent, normal);
	} else if (index == 1) {
		tangent->e[0] = -normal->e[2];
		tangent->e[1] =  0.0;
		tangent->e[2] =  normal->e[0];
		ri_vector_normalize(tangent);

		ri_vector_cross3(binormal, tangent, normal);

	} else if (index == 2) {
		tangent->e[0] = -normal->e[1];
		tangent->e[1] =  normal->e[0];
		tangent->e[2] =  0.0;
		ri_vector_normalize(tangent);

		ri_vector_cross3(binormal, tangent, normal);
	} else {
		ri_log(LOG_WARN, "index is not < 3");
	}
}
