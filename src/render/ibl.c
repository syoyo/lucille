/*
 * Image-Based Lighting routine.
 *
 * Copyright Syoyo Fujita
 *
 * $Id: ibl.c,v 1.13 2004/08/15 05:19:39 syoyo Exp $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#include "memory.h"
#include "vector.h"
#include "ibl.h"
#include "reflection.h"
#include "random.h"
#include "brdf.h"
#include "qmc.h"

#ifndef M_PI
#define M_PI 3.1415926535
#endif

#ifndef INV_M_PI
#define INV_M_PI 0.31831f    /* 1 / M_PI */
#endif

typedef struct _histgram_t
{
    //int x, y;
    ri_vector_t dir;
    ri_vector_t color;
    ri_float_t sumintensity;
} histgram_t;

static ri_float_t sinc(ri_float_t x);
static void gen_qmc_samples(ri_float_t *samples,
                int nsamples, int instancenum, int **perm);

/*
 * Cosine weighted sampling.
 * Generate sampling direction with PDF(Probability Density Function)
 * cos(theta) / PI on hemisphere.
 * i.e. more samples along normal direction, less samples along perpendicular
 * to normal vector.
 */
void
ri_ibl_sample_cosweight(
    ri_vector_t        power,           /* [out] */
    const ri_vector_t  normal,
    int                nsamples,
    const ri_ray_t    *inray,
    const ri_vector_t  pos,
    const ri_vector_t  eye,
    const ri_light_t  *light)
{
    int i, j, k;
    int hit;
    int count;
    int ntheta, nphi;
    int tid = inray->thread_num;
    ri_float_t dpower[3];
    ri_float_t theta, phi;
    ri_float_t brdf;
    ri_float_t *samples;
    ri_vector_t rad;
    ri_vector_t dir;
    ri_vector_t basis[3];
    ri_ray_t r;
    ri_intersection_state_t state;
    ri_option_t *opt;
    
#if 0
    ri_float_t glossness = 15.0;
    ri_float_t pdf;
#endif

    (void)eye;

    opt = ri_render_get()->context->option;

    dpower[0] = dpower[1] = dpower[2] = 0.0;

    ri_ray_copy(&r, inray);

    ri_vector_copy(r.org, pos);

    /* slightly move the shading point towards the surface normal */
    r.org[0] += normal[0] * 0.0001;
    r.org[1] += normal[1] * 0.0001;
    r.org[2] += normal[2] * 0.0001;

    r.thread_num = inray->thread_num;

    ri_vector_setzero(rad);

    //hemi->rtotal = 0.0;
    count = 0;

    ri_ortho_basis(basis, normal);

    if (opt->use_qmc) {    /* quasi-Monte Carlo sampling */

        samples = (ri_float_t *)ri_mem_alloc(sizeof(ri_float_t) * nsamples * 2);

        gen_qmc_samples(samples,
                nsamples, inray->i, ri_render_get()->perm_table);

        for (i = 0; i < nsamples; i++) {
            theta = sqrt(samples[2 * i + 0]);
            phi = 2.0 * M_PI * samples[2 * i + 1];        
            dir[0] = cos(phi) * theta;
            dir[1] = sin(phi) * theta;
            dir[2] = sqrt(1.0 - theta * theta);

            for (k = 0; k < 3; k++) {
                r.dir[k] = dir[0]*basis[0][k]
                         + dir[1]*basis[1][k]
                         + dir[2]*basis[2][k];
            }

            ri_vector_normalize(r.dir);

            hit = ri_raytrace(ri_render_get(),
                      &r, &state);

            if (!hit) {
                ri_texture_ibl_fetch(
                                rad,
                                light->texture,
                                r.dir);

                /* lambert */
                brdf = INV_M_PI;    /* 1.0 / M_PI; */

                dpower[0] += rad[0] * brdf;
                dpower[1] += rad[1] * brdf;
                dpower[2] += rad[2] * brdf;
            }

            /* radius of irradiance cache value. */ 
            //hemi->rtotal += hemi->samples[i][j].r;
        }

        power[0] = dpower[0] / (ri_float_t)nsamples;
        power[1] = dpower[1] / (ri_float_t)nsamples;
        power[2] = dpower[2] / (ri_float_t)nsamples;

        ri_mem_free(samples);

    } else {    /* Monte Carlo sampling */
        /* theta * phi = total samples.
         * phi = 3 * theta.
         */
        ntheta = nsamples / 3.0;
        ntheta = (int)sqrt((ri_float_t)ntheta);
        if (ntheta < 1) {
            ntheta = 1;
        }
        if (ntheta > MAX_HEMISAMPLE) {
            ntheta = MAX_HEMISAMPLE;
        }
        nphi   = 3 * ntheta;

        for (j = 0; j < (int)nphi; j++) {
            for (i = 0; i < (int)ntheta; i++) {

#if 1
                theta = sqrt(((ri_float_t)i + randomMT2(tid)) /
                    ntheta);
                phi = 2.0 * M_PI * ((ri_float_t)j + randomMT2(tid)) /
                      nphi;        
                dir[0] = cos(phi) * theta;
                dir[1] = sin(phi) * theta;
                dir[2] = sqrt(1.0 - theta * theta);

                for (k = 0; k < 3; k++) {
                    r.dir[k]=dir[0]*basis[0][k]
                          +dir[1]*basis[1][k]
                          +dir[2]*basis[2][k];
                }
#else
                ri_sample_modified_phong(&(r.dir), &pdf,
                    &inray->dir, normal,
                    randomMT(), randomMT(), glossness);
#endif

                ri_vector_normalize(r.dir);

                hit = ri_raytrace(ri_render_get(),
                          &r, &state);

                if (!hit) {
                    ri_texture_ibl_fetch(
                                rad,
                                light->texture,
                                r.dir);

                    /* lambert */
                    brdf = 1.0 / M_PI;
                    
                    /* phong */
                    //brdf = pdf / M_PI;

                    dpower[0] += rad[0] * brdf;
                    dpower[1] += rad[1] * brdf;
                    dpower[2] += rad[2] * brdf;
                }

                /* radius of irradiance cache value. */ 
                //hemi->rtotal += hemi->samples[i][j].r;
            }
        }

        power[0] = M_PI * dpower[0] /
                  (ri_float_t)(ntheta * nphi);
        power[1] = M_PI * dpower[1] /
                  (ri_float_t)(ntheta * nphi);
        power[2] = M_PI * dpower[2] /
                  (ri_float_t)(ntheta * nphi);
    }
}


void
ri_domelight_sample(
    ri_vector_t        power,           /* [out] */
    ri_hemisphere_t   *hemi,
    int                nsamples,
    const ri_ray_t    *inray,
    const ri_vector_t  pos,
    const ri_vector_t  eye,
    const ri_light_t  *light)
{
    int i, j, k;
    int hit;
    //int count;
    ri_float_t dpower[3];
    ri_float_t theta, phi;
    ri_float_t brdf;
    ri_float_t  u, v;
    ri_float_t *samples;
    //ri_float_t *samplepoints;
    ri_vector_t rad;
    ri_vector_t dir;
    ri_ray_t r;
    ri_intersection_state_t state;
    ri_option_t *opt;

    opt = ri_render_get()->context->option;

    (void)eye;

    dpower[0] = dpower[1] = dpower[2] = 0.0;
    ri_ray_copy(&r, inray);
    ri_vector_copy(r.org, pos);
    ri_vector_setzero(rad);

    ri_ortho_basis(hemi->basis, hemi->basis[2]);

    if (opt->use_qmc) {    /* quasi-Monte Carlo sampling */

        samples = (ri_float_t *)ri_mem_alloc(sizeof(ri_float_t) * nsamples * 2);



#if 0 
        u = randomMT();
        v = randomMT();
#endif

        for (i = 0; i < nsamples; i++) {
            samples[2 * i + 0] = generalized_scrambled_hammersley(
                        i, 0, nsamples, 1,
                        ri_render_get()->perm_table);
            samples[2 * i + 1] = generalized_scrambled_hammersley(
                        i, 0, nsamples, 2,
                        ri_render_get()->perm_table);

#if 1
            u = generalized_scrambled_halton(
                        inray->i, 0, inray->d,
                        ri_render_get()->perm_table);
            v = generalized_scrambled_halton(
                        inray->i, 0, inray->d+1,
                        ri_render_get()->perm_table);

#endif

            samples[2 * i + 0] = mod_1(u + samples[2 * i + 0]);
            samples[2 * i + 1] = mod_1(u + samples[2 * i + 1]);
        }

        for (i = 0; i < nsamples; i++) {
            theta = sqrt(samples[2 * i + 0]);
            phi = 2.0 * M_PI * samples[2 * i + 1];        
            dir[0] = cos(phi) * theta;
            dir[1] = sin(phi) * theta;
            dir[2] = sqrt(1.0 - theta * theta);

            for (k = 0; k < 3; k++) {
                r.dir[k]=dir[0]*hemi->basis[0][k]
                      +dir[1]*hemi->basis[1][k]
                      +dir[2]*hemi->basis[2][k];
            }

            ri_vector_normalize(r.dir);

            hit = ri_raytrace(ri_render_get(),
                      &r, &state);

            if (!hit) {
                ri_vector_copy(rad, light->col);
                ri_vector_scale(rad, rad, (ri_float_t)light->intensity);

                /* lambert */
                brdf = (ri_float_t)1.0 / M_PI;

                dpower[0] += rad[0] * brdf;
                dpower[1] += rad[1] * brdf;
                dpower[2] += rad[2] * brdf;
            }
        }

        power[0] = M_PI * dpower[0] / (ri_float_t)nsamples;
        power[1] = M_PI * dpower[1] / (ri_float_t)nsamples;
        power[2] = M_PI * dpower[2] / (ri_float_t)nsamples;

        ri_mem_free(samples);

    } else { /* Monte Carlo sampling, */

        /* theta * phi = total samples.
         * phi = 3 * theta.
         */
        hemi->ntheta = nsamples / 3.0;
        hemi->ntheta = (int)sqrt((ri_float_t)hemi->ntheta);
        if (hemi->ntheta < 1) hemi->ntheta = 1;
        if (hemi->ntheta > MAX_HEMISAMPLE) {
            hemi->ntheta = MAX_HEMISAMPLE;
        }
        hemi->nphi   = 3 * hemi->ntheta;


        for (j = 0; j < (int)hemi->nphi; j++) {
            for (i = 0; i < (int)hemi->ntheta; i++) {
                theta = sqrt(((ri_float_t)i + randomMT()) /
                    hemi->ntheta);
                phi = 2.0 * M_PI * ((ri_float_t)j + randomMT()) /
                      hemi->nphi;        
                dir[0] = cos(phi) * theta;
                dir[1] = sin(phi) * theta;
                dir[2] = sqrt(1.0 - theta * theta);

                for (k = 0; k < 3; k++) {
                    r.dir[k] = dir[0]*hemi->basis[0][k]
                             + dir[1]*hemi->basis[1][k]
                             + dir[2]*hemi->basis[2][k];
                }

                ri_vector_normalize(r.dir);

                hit = ri_raytrace(ri_render_get(),
                          &r, &state);

                if (!hit) {
                    ri_vector_copy(rad, light->col);
                    ri_vector_scale(rad, rad, light->intensity);

                    /* lambert */
                    brdf = 1.0 / M_PI;

                    dpower[0] += rad[0] * brdf;
                    dpower[1] += rad[1] * brdf;
                    dpower[2] += rad[2] * brdf;
                }
            }
        }

        power[0] = M_PI * dpower[0] / (ri_float_t)(nsamples);
        power[1] = M_PI * dpower[1] / (ri_float_t)(nsamples);
        power[2] = M_PI * dpower[2] / (ri_float_t)(nsamples);
    }
}

/*
 * perform exact hemisphere integration.
 */
void
ri_ibl_sample_bruteforce(
    ri_vector_t        power,
    ri_hemisphere_t   *hemi,
    int                nsamples,
    const ri_vector_t  pos,
    const ri_vector_t  eye,
    const ri_light_t  *light)
{
    const ri_float_t scaling = 1.0;
    int i, j;
    int hit;
    int width;
    ri_float_t invdist;
    ri_float_t dpower[3];
    ri_float_t theta, phi;
    ri_float_t brdf;
    ri_float_t u,v,r,x,y,z,domega ;
    ri_float_t dot;
    ri_vector_t rad;
    ri_vector_t dist;
    ri_ray_t ray;
    ri_intersection_state_t state;

    (void)eye;
    (void)nsamples;

    if ((light->type != LIGHTTYPE_IBL) && !light->texture) {
        ri_vector_setzero(power);
        return;
    }

    dpower[0] = dpower[1] = dpower[2] = 0.0;

    ri_vector_copy(ray.org, pos);

    ri_vector_setzero(rad);

    /* we assume input IBL texture is angular map. */
    if (light->texture->width != light->texture->height) {
        printf("texture width != texture height.\n");
        ri_vector_setzero(power);
        return;
    }

    width = light->texture->width;

    /* codes from Ravi Ramamoorthi's prefilter.c */

    ri_ortho_basis(hemi->basis, hemi->basis[2]);

    for (i = 0 ; i < width ; i++) {
        for (j = 0 ; j < width ; j++) {

            /* We now find the cartesian components
             *for the point (i,j) */

            /* v ranges from -1 to 1 */
            v = ((ri_float_t)width/2.0 - (ri_float_t)i)/((ri_float_t)width/2.0);
            /* u ranges from -1 to 1 */
            u = ((ri_float_t)j-(ri_float_t)width/2.0)/((ri_float_t)width/2.0);
            r = sqrt(u*u+v*v) ;               /* The "radius" */
            /* Consider only circle with r<1 */
            if (r > 1.0) continue ;

            /* theta parameter of (i,j) */
            theta = M_PI*r ;
            /* phi parameter */
            phi = atan2(v,u) ;

            /* Cartesian components */
            x = sin(theta)*cos(phi) ;
            y = sin(theta)*sin(phi) ;
            z = cos(theta) ;

            ray.dir[0]  = x;
            ray.dir[1]  = y;
            ray.dir[2]  = z;

            dot = ri_vector_dot(ray.dir, hemi->basis[2]);
            if (dot < 0.0) continue;

                /* Computation of the solid angle.  This follows from
                 * some elementary calculus converting
                 * sin(theta) d theta d phi into coordinates in terms
                 * of r.  This calculation should be redone 
                 * if the form of the input changes
                 */

                domega = (2.0*M_PI/width)*(2.0*M_PI/width)*sinc(theta) ;
                //domega = (2.0*M_PI/width)*(2.0*M_PI/width)*sinc(acos(y)) ;


            hit = ri_raytrace(ri_render_get(), &ray, &state);

            if (!hit) {
                ri_texture_ibl_fetch(
                            rad,
                            light->texture,
                            ray.dir);

                ri_vector_scale(ray.dir, ray.dir, scaling);
                ri_vector_scale(ray.org, ray.dir, scaling);
                ri_vector_sub(dist, ray.dir, ray.org);
                invdist = ri_vector_length(dist);

                if (invdist != 0.0) {
                    invdist = 1.0 / (invdist * invdist);
                }
                //invdist = 1.0;

                /* lambert */
                brdf = 1.0 / M_PI;

                dpower[0] += rad[0] * brdf * dot * domega * invdist;
                dpower[1] += rad[1] * brdf * dot * domega * invdist;
                dpower[2] += rad[2] * brdf * dot * domega * invdist;
            }
        }
    }

    power[0] = dpower[0];
    power[1] = dpower[1];
    power[2] = dpower[2];
}


/* --- private functions --- */


static ri_float_t
sinc(ri_float_t x)
{
    if (fabs(x) < 1.0e-6) return 1.0;
    
    return sin(x) / x;
}

/*
 * For details, see:
 * Alexander Keller,
 * "Strictly deterministic sampling methods in computer graphics"
 * (mental images technical report, 2001)
 *       in "Monte Carlo Ray Tracing", SIGGRAPH'2003 Course #44.
 */
static void
gen_qmc_samples(ri_float_t *samples, int nsamples, int instancenum, int **perm)
{
#if 1
    int    i;
    ri_float_t u[2];

    for (i = 0; i < nsamples; i++) {
        //samples[2 * i + 0] = generalized_scrambled_hammersley(
        //            i, instancenum, nsamples, 1,
        //            perm);
        //samples[2 * i + 1] = generalized_scrambled_hammersley(
        //            i, instancenum, nsamples, 2,
        //            perm);
        samples[2 * i + 0] = generalized_scrambled_halton(
                    i, instancenum, 1,
                    perm);
        samples[2 * i + 1] = generalized_scrambled_halton(
                    i, instancenum, 2,
                    perm);

#if 0
        u[0] = generalized_scrambled_halton(
                    i, instancenum, 3,
                    perm);
        u[1] = generalized_scrambled_halton(
                    i, instancenum, 4,
                    perm);
#endif
        u[0] = 0.0; u[1] = 0.0;

        samples[2 * i + 0] = mod_1(u[0] + samples[2 * i + 0]);
        samples[2 * i + 1] = mod_1(u[1] + samples[2 * i + 1]);
    }

#else
    (void)nsamples;
    (void)instancenum;
    (void)perm;

    fibonacci_lattice_2D(samples, 10);
#endif
}
