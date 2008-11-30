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

static void calc_tangent_and_binormal(
    ri_vector_t       tangent_out,      /* [out] */
    ri_vector_t       binormal_out,     /* [out] */
    const ri_vector_t normal);

void
ri_reflect(
    ri_vector_t       reflect_out,
    const ri_vector_t in,
    const ri_vector_t n)
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

    dot = ri_vector_dot(in, n);
    ri_vector_copy(ndot, n);
    ri_vector_scale(ndot, ndot, 2 * dot);

    ri_vector_sub(reflect_out, in, ndot);
}

/*
 * Function: ri_refract
 *
 *     Calculates refraction vector.
 *
 * Parameters:
 *
 *     *refract - Refraction vector, or reflection vector when total internal
 *                reflection occures.
 *      in      - Incident vector.
 *      n       - Normal vector.
 *      eta     - Relative refractive index.
 *
 * Returns:
 *
 *     1 if total internal refrection occures.
 */
int
ri_refract(
    ri_vector_t       refract_out,
    const ri_vector_t in,
    const ri_vector_t n,
    ri_float_t        eta)
{
    double      cos1;
    double      coeff;
    ri_vector_t N;
    double      e = 1.0 / eta;

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

    cos1 = ri_vector_dot(in, n);
    if (cos1 < 0.0) {
        cos1 = -cos1;
        N[0] = n[0];
        N[1] = n[1];
        N[2] = n[2];
    } else {
        e = eta;
        N[0] = -(n[0]);
        N[1] = -(n[1]);
        N[2] = -(n[2]);
    }

    coeff = 1.0 - (e * e) * (1.0 - cos1 * cos1);
    if (coeff <= 0.0) {
        /* total internal reflection */
        ri_reflect(refract_out, in, n);
        ri_vector_normalize(refract_out);
        return 1;
    }

    coeff = e * cos1 - sqrt(coeff);

    refract_out[0] = coeff * N[0] + e * in[0];
    refract_out[1] = coeff * N[1] + e * in[1];
    refract_out[2] = coeff * N[2] + e * in[2];

    ri_vector_normalize(refract_out);

    return 0;
}

void
ri_random_vector_cosweight(
    ri_vector_t        v_out,
    const ri_vector_t  n)
{
    double      theta, phi;
    ri_vector_t tn, bn, nn;
    ri_vector_t tb;

    calc_tangent_and_binormal(tn, bn, n);

    theta = acos(sqrt(1.0 - randomMT()));
    phi   = 2.0 * M_PI * randomMT();

    ri_vector_copy(nn, n);

    /* D = T*cos(phi)*sin(theta) + B*sin(phi)*sin(theta) + N*cos(theta) */
    ri_vector_scale(tn, tn, cos(phi) * sin(theta));
    ri_vector_scale(bn, bn, sin(phi) * sin(theta));
    ri_vector_scale(nn, nn, cos(theta));

    ri_vector_add(tb, tn, bn);
    ri_vector_add(v_out, tb, nn);
}

void
ri_random_vector_cosNweight(
    ri_vector_t        v_out,
    double            *pdf_out,
    const ri_vector_t  n,
    double             u0,
    double             u1,
    double             N)
{
    double      cos_theta, sin_theta, phi;
    ri_vector_t tn, bn, nn;
    ri_vector_t tb;

    calc_tangent_and_binormal(tn, bn, n);

    cos_theta = pow(u0, 1.0/(N+1));
    sin_theta = sqrt(1.0 - cos_theta * cos_theta);

    phi   = 2.0 * M_PI * u1;

    ri_vector_copy(nn, n);

    /* D = T*cos(phi)*sin(theta) + B*sin(phi)*sin(theta) + N*cos(theta) */
    ri_vector_scale(tn, tn, cos(phi) * sin_theta);
    ri_vector_scale(bn, bn, sin(phi) * sin_theta);
    ri_vector_scale(nn, nn, cos_theta);

    ri_vector_add(tb, tn, bn);
    ri_vector_add(v_out, tb, nn);

    (*pdf_out) = (N + 1.0) * pow(cos_theta, N) / (2.0 * M_PI);
}

/* Sample a cosine weighted direction on hemisphere using QMC. */
void
ri_random_vector_cosweight_qmc(
    ri_vector_t         v_out,
    const ri_vector_t   n,
    int                 d,
    int                 i,
    int               **perm)
{
    double      u[2];
    double      phi, theta;
    ri_vector_t tn, bn, nn;
    ri_vector_t tb;

    calc_tangent_and_binormal(tn, bn, n);

    u[0] = generalized_scrambled_halton(i, 0, d, perm);
    u[1] = generalized_scrambled_halton(i, 0, d, perm);

    theta = acos(sqrt(1.0 - u[0]));
    phi   = 2.0 * M_PI * u[1];
    
    /* D = T*cos(phi)*sin(theta) + B*sin(phi)*sin(theta) + N*cos(theta) */
    ri_vector_scale(tn, tn, cos(phi) * sin(theta));
    ri_vector_scale(bn, bn, sin(phi) * sin(theta));
    ri_vector_scale(nn, nn, cos(theta));

    ri_vector_add(tb, tn, bn);
    ri_vector_add(v_out, tb, nn);
}

/* Return 1 if total internal reflection occurs. */
int
ri_fresnel(
    ri_vector_t        r_out,       /* reclected vector             */
    ri_vector_t        t_out,       /* transmitted vector           */
    float             *kr_out,      /* the reflection coeff         */
    float             *kt_out,      /* the refraction coeff         */
    const ri_vector_t  in,
    const ri_vector_t  n,
    float              eta)         /* relative index of refraction */
{
    double dot;
    double inv_eta;
    double coeff1, coeff2;
    double c, g;

    int tir = 0;    /* Total Internal Refrection */

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

    dot = ri_vector_dot(in, n);

    if (dot < 0.0f) {    /* imcoming */

        tir = ri_refract(t_out, in, n, eta);
            
        dot = -dot;
        if (dot > 1.0) dot = 1.0;

    } else {        /* outgoing */

        ri_vector_copy(negn, n);
        ri_vector_neg(negn);        /* -n */

        tir = ri_refract(t_out, in, n, eta);

    }

    ri_reflect(r_out, in, n);

    if (tir) {
        (*kr_out) = 1.0;
        (*kt_out) = 0.0;
        return 1;
    }

    inv_eta = 1.0 / eta;
    c = dot / inv_eta;
    g = sqrt(1.0 + c * c - inv_eta * inv_eta);

    coeff1 = ((g - c) / (g + c)) * ((g - c) / (g + c));    
    coeff2 = (c * (g + c) - inv_eta * inv_eta) / 
            (c * (g - c) + inv_eta * inv_eta);

    (*kr_out) = 0.5 * coeff1 * (1.0 + coeff2 * coeff2);
    if ((*kr_out) < 0.0) (*kr_out) = 0.0;
    if ((*kr_out) > 1.0) (*kr_out) = 1.0;

    (*kt_out) = 1.0 - (*kr_out);

    return 0;
}

void
ri_hvector(
    ri_vector_t        h_out,
    const ri_vector_t  l,
    const ri_vector_t  v)
{
    ri_vector_add(h_out, l, v);    
    ri_vector_normalize(h_out);
}

void
ri_ortho_basis(
    ri_vector_t       basis[3],
    const ri_vector_t n)
{
    int i;
    ri_vector_copy(basis[2], n);
    ri_vector_setzero(basis[1]);

    for (i = 0; i < 3; i++) {
        if (basis[2][i] < (ri_float_t)0.6 &&
            basis[2][i] > (ri_float_t)-0.6)
            break;
    }

    if (i >= 3) i = 0;
    basis[1][i] = (ri_float_t)1.0;

    ri_vector_cross(basis[0], basis[1], basis[2]);
    ri_vector_normalize(basis[0]);
    ri_vector_cross(basis[1], basis[2], basis[0]);
    ri_vector_normalize(basis[1]);
}

/* --- private functions --- */

static void
calc_tangent_and_binormal(
    ri_vector_t       tangent_out,
    ri_vector_t       binormal_out,
    const ri_vector_t normal)
{
    /* codes from renderBitch */
    int    i;
    int    index = -1;
    double min   = RI_INFINITY;
    double val;

    /* find the minor axis of the vector */
    for (i = 0; i < 3; i++) {
        val = fabs(normal[i]);
        if (val < min) {
            min = val;
            index = i;
        }
    }    

    if (index == 0) {

        tangent_out[0] =  0.0;
        tangent_out[1] = -normal[2];
        tangent_out[2] =  normal[1];
        ri_vector_normalize(tangent_out);

        ri_vector_cross(binormal_out, tangent_out, normal);

    } else if (index == 1) {

        tangent_out[0] = -normal[2];
        tangent_out[1] =  0.0;
        tangent_out[2] =  normal[0];

        ri_vector_normalize(tangent_out);
        ri_vector_cross(binormal_out, tangent_out, normal);

    } else if (index == 2) {

        tangent_out[0] = -normal[1];
        tangent_out[1] =  normal[0];
        tangent_out[2] =  0.0;

        ri_vector_normalize(tangent_out);
        ri_vector_cross(binormal_out, tangent_out, normal);

    } else {

        ri_log(LOG_WARN, "index is not < 3");

    }
}
