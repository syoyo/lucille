#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "brdf.h"
#include "random.h"
#include "reflection.h"

#ifndef M_PI
#define M_PI 3.1415926535
#endif

#define EPSILON 1.0e-16

static ri_float_t fresnel_approx(const ri_vector_t v0, const ri_vector_t v1);

ri_float_t
ri_brdf_lambert(
    const ri_vector_t  wo,
    const ri_vector_t  wi,
    const ri_vector_t  n,
    const ri_float_t   kd)
{
    ri_float_t ldotn;

    (void)wo;
    
    ldotn = ri_vector_dot(wi, n);
    if (ldotn <= 0.0) return 0.0;

    return kd * ldotn;
}

ri_float_t
ri_brdf_blinn(
    const ri_vector_t wo,
    const ri_vector_t wi,
    const ri_vector_t n,
    const ri_float_t  kd,
    const ri_float_t  ks,
    const ri_float_t  glossness)
{
    ri_vector_t half;

    (void)n;

    ri_vector_add(half, wo, wi);
    ri_vector_normalize(half);

    return (kd + ks * pow(half[2], glossness));
}

ri_float_t
ri_brdf_phong(
    const ri_vector_t  wo,
    const ri_vector_t  wi,
    const ri_vector_t  n,
    const ri_float_t   kd,
    const ri_float_t   ks,
    const ri_float_t   glossness)
{
    ri_float_t      rdotl;
    ri_float_t      ndotl;
    ri_vector_t     r;            
    ri_vector_t     v;        /* view vector V    */

    ri_vector_copy(v, wo);

    ri_reflect(r, v, n);        /* R = reflect(V, N)    */

    rdotl = ri_vector_dot(r, wi);    /* (R . L)        */
    ndotl = ri_vector_dot(n, wi);    /* (N . L)        */

    if (rdotl <= 0.0) return 0.0;
    if (ndotl <= 0.0) return 0.0;

    return (kd / M_PI) + ks * (pow(rdotl, glossness) / ndotl);
}

/* Modified Phong BRDF. It is physically correct.
 * see:
 * "Using the Modified Phong brdf for Physically Based Rendering"
 * Eric P. Lafortune and Yves D. Willems,
 * Report CW197, Department of Computer Science, K. U. Leuven. 1994
 */
ri_float_t
ri_brdf_modified_phong(
    const ri_vector_t  wo,
    const ri_vector_t  wi,
    const ri_vector_t  n,
    const ri_float_t   kd,
    const ri_float_t   ks,
    const ri_float_t   glossness)
{
    ri_float_t      rdotl;
    ri_float_t      diffuse, specular;
    
    ri_vector_t r;            
    ri_vector_t v;            /* view vector V    */

    ri_vector_copy(v, wo);

    ri_reflect(r, v, n);        /* R = reflect(V, N)    */

    rdotl = ri_vector_dot(r, wi);    /* (R . L)        */
    if (rdotl <= 0.0) return 0.0;
    if (rdotl >= 1.0) {
        printf("??? rdotl = %f\n", rdotl);
        rdotl = 1.0;
    }
    
    diffuse  = kd / M_PI;
    specular = ks * ((glossness + 2.0) / (2.0 * M_PI)) *
           pow(rdotl, glossness);

    return diffuse + specular;
}

/* Ward's anisotropic BRDF.
 * see:
 * "Measuring and Modeling Anisotropic Reflection"
 * Gregory J. Ward, SIGGRAPH 1992
 */
ri_float_t
ri_brdf_ward_anisotropic(
    const ri_vector_t wo,
    const ri_vector_t wi,
    const ri_vector_t n,
    const ri_vector_t u,
    const ri_vector_t v,
    const ri_float_t  kd,
    const ri_float_t  ks,
    const ri_float_t  ax,
    const ri_float_t  ay)
{
    ri_vector_t     r;
    ri_vector_t     view;                       /* view vector V            */
    ri_vector_t     h;                          /* half(R + L)              */
    ri_vector_t     hp;                         /* H vector projected       */
    ri_float_t      costr;                      /* cos(theta)_{r}           */
    ri_float_t      costi;                      /* cos(theta)_{i}           */
    ri_float_t      cospr;                      /* cos(phi)_{r}             */
    ri_float_t      cospi;                      /* cos(phi)_{i}             */
    ri_float_t      hnorm;                      /* ||h||                    */
    ri_float_t      hdotx, hdoty, hdotn;
    ri_float_t      diffuse, specular;
    ri_float_t      c1, c2, c3;                 /* for temporary            */
    ri_float_t      tand;

    (void)hp; (void)cospr; (void)cospi; (void)tand;
    diffuse = kd / M_PI;

    ri_vector_copy(view, wo);

    ri_reflect(r, view, n);          /* R = reflect(V, N)    */

    costr = ri_vector_dot(r, n);     /* cos(theta)_{r} = r . n    */
    costi = ri_vector_dot(wi, n);    /* cos(theta)_{i} = wi . n    */
    if (costr <= 0.0) {
        /* reflected vector is not on hemisphere avobe normal */
        printf("??? costr <= 0.0\n");
        return diffuse;
    }

    if (costi <= 0.0) {
        /* light vector is not on hemisphere avobe normal */
        return diffuse;
    }

    ri_vector_add(h, r, wi);
    hnorm = ri_vector_length(h);
    ri_vector_normalize(h);
    
    hdotn = ri_vector_dot(h, n);        /* h . n    */
    hdotx = ri_vector_dot(h, u);        /* h . x    */
    hdoty = ri_vector_dot(h, v);        /* h . y    */

    if (costi * costr < 0.0) {
        printf("??? costi * costr < 0.0\n");
        printf("costi = %f, costr = %f\n", costi, costr);
        costi = 0.0;
    } 

    /*
     *                    1.0
     * -------------------------------------
     * sqrt(cos(theta)_{i} x cos(theta)_{r})
     */
    //c1 = 1.0 / sqrt(costi * costr);    
    c1 = 1.0 / sqrt(costi * costr);    

    
    /*
     *              1.0
     * ----------------------------
     * 4 * PI * alpha_{x} * alpha{y}
     */
    c2 = 1.0 / (4.0 * M_PI * ax * ay);

    
    if (hdotn == -1.0) {
        printf("???: hdotn == -1.0\n");
        hdotn = -1.00000001;
    }

    /*
     * ((h . x) / alpha_{x})^2 + ((h . y) / alpha_{y})^2
     * -------------------------------------------------
     *                       1 + (h . n)
     */
    //printf("hdotx = %f\n", hdotx);
    //printf("hdoty = %f\n", hdoty);
    c3  = (hdotx / ax) * (hdotx / ax) + (hdoty / ay) * (hdoty / ay);
    //printf("tmpc3 = %f\n", c3);
    //printf("1.0 + hdotn = %f\n", 1.0 + hdotn);
    c3 /= 1.0 + hdotn;

    specular = ks * c1 * c2 * exp(-2.0 * c3);

    //printf("c1 = %f, c2 = %f, 3 = %f\n", c1, c2, c3);
    //printf("diffuse = %f\n", diffuse);
    //printf("specular = %f\n", specular);


    return diffuse + specular;
    
}

ri_float_t
ri_brdf_ashikhmin_shirley(
    const ri_vector_t  wo,
    const ri_vector_t  wi,
    const ri_vector_t  n,
    const ri_vector_t  u,
    const ri_vector_t  v,
    const ri_float_t   kd, 
    const ri_float_t   ks,
    const ri_float_t   nu,
    const ri_float_t   nv)
{
    ri_float_t      hk;
    ri_float_t      nk1, nk2;
    ri_float_t      fres;
    ri_float_t      c, c1, c2;
    ri_float_t      hn, hu, hv;
    ri_float_t      exponent;
    ri_float_t      rk1, rk2;
    ri_float_t      diffuse;
    ri_float_t      specular;
    ri_float_t      maxdot;

    ri_vector_t view;
    ri_vector_t h;        /* half vector of wi and wo */
    ri_vector_t k1, k2;
    

    ri_vector_copy(k1, wi);
    ri_vector_copy(view, wo);
    ri_vector_neg(view);    /* V = -wo */
    ri_vector_copy(k2, view);
    ri_vector_add(h, k1, k2);
    ri_vector_normalize(h);

    ri_vector_normalize(k1);
    ri_vector_normalize(k2);

    hk  = ri_vector_dot(h, k2);
    nk1 = ri_vector_dot(h, k1);
    nk2 = ri_vector_dot(h, k2);

    //printf("hk = %f, nk1 = %f, nk2 = %f\n", hk, nk1, nk2);

    if (nk2 < 1.0e-6) {
        return 0.0;
    }

    if (nk1 < 1.0e-6) {
        return 0.0;
    }

    /* diffuse part */
    rk1 = 1.0 - pow(1.0 - (nk1 * 0.5), 5);
    rk2 = 1.0 - pow(1.0 - (nk2 * 0.5), 5);

    c = 28.0 / (23.0 * M_PI);
    
    diffuse = kd * c * rk1 * rk2;
    //printf("diffuse = %f\n", diffuse);

    /* specular part */
    c     = sqrt((nu + 1.0) * (nv + 1.0));
    c1    = c / (8.0 * M_PI);
    fres  = fresnel_approx(h, k2);

    hn = ri_vector_dot(h, n);
    hu = ri_vector_dot(h, u);
    hv = ri_vector_dot(h, v);
    exponent = ((nu * hu * hu) + (nv * hv * hv)) / (1.0 - (hn * hn));

    c = pow(hn, exponent);
    maxdot = (nk1 > nk2) ? nk1 : nk2;
    c2 = c / (hk * maxdot);

    specular = ks * c1 * c2 * fres;
    //printf("specular = %f\n", specular);

    return diffuse + specular;
}

void
ri_brdf_ashikhmin_shirley_spf(
    ri_vector_t        specularray,    /* [out] */ 
    ri_vector_t        diffuseray,     /* [out] */
    ri_float_t        *specularfactor, /* [out] */
    ri_float_t        *diffusefactor,  /* [out] */ 
    const ri_vector_t  dir,
    const ri_float_t   nu,
    const ri_float_t   nv)
{
    int         i;
    ri_float_t  r1, r2;
    ri_float_t  k;
    ri_float_t  phi;
    ri_float_t  a;
    ri_float_t  cosp, sinp;
    ri_float_t  cost, sint;
    ri_float_t  exponent;
    ri_float_t  hn, hu, hv, neghn;
    ri_float_t  c1, c2;
    ri_vector_t basis[3];
    ri_vector_t v;
    ri_vector_t half;
    ri_vector_t negdir;
    ri_vector_t k1, k2;
    
    (void)k1; (void)k2;

    /* specular part. */

    r1 = randomMT();
    r2 = randomMT();

    a = sqrt((nu + 1.0) / (nv + 1.0));
    
    if (r1 < 0.25) {            /* [0 - PI/2)        */ 
        k = 4.0 * r1;
        phi = atan(a * tan(M_PI * 0.5 * k));

    } else if (r1 < 0.5) {            /* [PI/2 - PI)        */
        k = 1.0 - 4.0 * (0.5 - r1);
        phi = atan(a * tan(M_PI * 0.5 * k));
        phi = M_PI - phi;

    } else if (r1 < 0.75) {            /* [PI - (3*PI)/2)    */
        k = 4.0 * (r1 - 0.5);
        phi = atan(a * tan(M_PI * 0.5 * k));
        phi += M_PI;

    } else {                /* [(3*PI)/2 - 2*PI)    */    
        k = 1.0 - 4.0 * (1.0 - r1);
        phi = atan(a * tan(M_PI * 0.5 * k));
        phi = 2.0 * M_PI - phi;
    }

    cosp = cos(phi);
    sinp = sin(phi);
    exponent = 1.0 / (cosp * cosp * nu + sinp * sinp * nv);
    cost = pow((1.0 - r2), exponent);
    sint = sqrt(1.0 - cost * cost);

    /* half vector in local coordinate */
    v[0] = (ri_float_t)(cosp * sint);
    v[1] = (ri_float_t)(sinp * sint);
    v[2] = (ri_float_t)cost;

    ri_ortho_basis(basis, dir);

    /* transform local half vector into surface space */
    for (i = 0; i < 3; i++) {
        half[i] = basis[0][i] * v[0] +
                  basis[1][i] * v[1] +
                  basis[2][i] * v[2];
    }

    hn = ri_vector_dot(basis[2], half);
    hu = ri_vector_dot(basis[0], half);
    hv = ri_vector_dot(basis[1], half);

    ri_vector_copy(negdir, dir);
    ri_vector_neg(negdir);
    neghn = ri_vector_dot(negdir, half);

    c1 = sqrt((nu + 1.0) * (nv + 1.0)) / (2.0 * M_PI);
    c2 = pow(hn, (nu * hu * hu + nv * hv * hv));
    (*specularfactor) = (c1 * c2) / (4.0 * neghn);

    ri_reflect(specularray, dir, half);

    /* diffuse part. */

    if ((*specularfactor) >= 1.0) {
        (*diffusefactor) = 0.0;
        ri_vector_setzero(diffuseray);
        return;
    }

    ri_random_vector_cosweight(diffuseray, dir);
    (*diffusefactor) = 1.0 - (*specularfactor);
}


/*
 * Function : ri_sample_modified_phong
 *
 *     Importance sample of modifed Phong BRDF(energy-preserving Phong BRDF).
 *
 * Parameters:
 *
 *     wo         - Importance sampled direction
 *    *pdf        - its PDF(Potensial Distribution Function, i.e. weights)
 *     wi         - incident direction
 *     n          - surface normal
 *     u0         - 1st uniform random number
 *     u1         - 2nd uniform random number
 *     glossness  - glossness parameter of modified Phong BRDF
 */
void
ri_sample_modified_phong(
    ri_vector_t        wo,
    ri_float_t        *pdf,
    const ri_vector_t  wi,
    const ri_vector_t  n,
    const ri_float_t   u0,
    const ri_float_t   u1,
    const ri_float_t   glossness)
{
    ri_vector_t refdir;
    ri_float_t scatterd_power;
    float kd, kg;

    // we assume this BRDF is consisted only glossy part.
    // TODO: consider diffuse and ideal specular part

    kd = 0.0;
    kg = 1.0;

    // scatterd power = diffuse power + specular(glossy) power.
    scatterd_power = kd + kg;
    
    if (scatterd_power < EPSILON) {
        fprintf(stderr, "[ModifedPhongBRDF] scatterd power < 0.0\n");
        exit(-1);
    }

    // compute reflection direction
    ri_reflect(refdir, wi, n);

    // randomly sample direction around the reflection vector. 
    ri_random_vector_cosNweight(wo, pdf, refdir, u0, u1, glossness);
}

static ri_float_t
fresnel_approx(
    const ri_vector_t v0,
    const ri_vector_t v1)
{
    const ri_float_t s = 0.1;
    ri_float_t       p;
    ri_float_t       one_minus_dot;

    one_minus_dot = ri_vector_dot(v0, v1);
    one_minus_dot = 1 - one_minus_dot;

    /* (1 - dot)^5 */
    p = one_minus_dot * one_minus_dot;
    p = p * p;
    p *= one_minus_dot;

    return s + (1.0 - s) * p; 
}


