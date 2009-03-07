/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Copyright 2003-2203 Syoyo Fujita.  All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the authors nor the names of their contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

/* ---------------------------------------------------------------------------
 *
 * Dirtmap shading based on Daniel Rind's Dirtmap shader for Mental Ray.
 * http://animus.brinkster.net/stuff/plg_dirtmap/plg_dirtmap.html
 *
 * The method is similar to Ambient Occlusion, but intersection test is
 * clampled between near/far clip, which results in local GI effects and
 * low noise.
 *
 * ------------------------------------------------------------------------ */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include "dirtmap.h"

#include "raytrace.h"
#include "reflection.h"
#include "random.h"
#include "texture.h"

/* ---------------------------------------------------------------------------
 *
 * Private functions 
 *
 * ------------------------------------------------------------------------ */

static void
mix_color(vec out, vec c0, vec c1, double t)
{
    double p;

    p = t;
    if (p < 0.0) p = 0.0;
    if (p > 1.0) p = 1.0;

    out[0] = (1.0 - p) * c0[0] - p * c1[0];
    out[1] = (1.0 - p) * c0[1] - p * c1[1];
    out[2] = (1.0 - p) * c0[2] - p * c1[2];

}

static int
calculate_dirt(
    ri_vector_t                    Lo,              /* [out] */
    const ri_ray_t                *inray,
    const ri_intersection_state_t *isect,
    uint32_t                       ntheta_samples,
    uint32_t                       nphi_samples)
{
    int                     hit;
    uint32_t                i, j, k;
    int                     thread_id = 0;

    double                  z0, z1;
    double                  cos_theta, phi;
    double                  eps = 1.0e-5;
    double                  occlusion = 0.0;

    vec                     dir;
    vec                     basis[3];
    vec                     sum_color;
    vec                     col;

    /* TODO: Make these values as parameters. */
    vec                     dirt_color;
    vec                     base_color;
    const double            dirt_gain = 1.0f;
    const double            near_clip = 0.1;   
    const double            far_clip  = 0.5;

    ri_ray_t                ray;
    ri_intersection_state_t state;

    ri_ortho_basis(basis, isect->Ns);

    vzero(sum_color);
    vcpy(ray.org, isect->P);

    /* black */
    dirt_color[0] = 0.0;
    dirt_color[1] = 0.0;
    dirt_color[2] = 0.0;

    /* white */
    base_color[0] = 1.0;
    base_color[1] = 1.0;
    base_color[2] = 1.0;


    /*
     * Slightly move the shading point towards the surface normal.
     * FIXME: Choose eps relative to scene scale, not as an absolute value.
     */
    ray.org[0] += isect->Ns[0] * eps;
    ray.org[1] += isect->Ns[1] * eps;
    ray.org[2] += isect->Ns[2] * eps;

    thread_id = inray->thread_num;
    assert(thread_id >=  0);
    assert(thread_id <  16);

    ray.thread_num = thread_id;

    for (j = 0; j < nphi_samples; j++) {
        for (i = 0; i < ntheta_samples; i++) {

            /*
             * 1. Choose random ray direction over the hemisphere.
             */

            /* Simple stratified sampling */
            z0 = (i + randomMT2(thread_id)) / (double)ntheta_samples;
            z1 = (j + randomMT2(thread_id)) / (double)nphi_samples;

            /* Do importance sampling. the probability function is,
             *
             * p(x) ~ cos(theta) / PI (against with differential solid angle)
             *
             * -> theta = acos(sqrt(z_0))
             *    phi   = 2 PI z_1
             *
             */
            cos_theta = sqrt(z0);
            phi       = 2.0 * M_PI * z1;

            dir[0]    = cos(phi) * cos_theta;
            dir[1]    = sin(phi) * cos_theta;
            dir[2]    = sqrt(1.0 - cos_theta * cos_theta);
    

            /* 'dir' is defined in local coord.
             * Convert it into global coord.
             */
            for (k = 0; k < 3; k++) {
                ray.dir[k] = dir[0]*basis[0][k]
                           + dir[1]*basis[1][k]
                           + dir[2]*basis[2][k];
            }

            /*
             * 2. Do raytracing to check visibility.
             */

            hit = ri_raytrace(ri_render_get(), &ray, &state);

            if (hit) {

                if (state.t <= near_clip) {

                    vadd(sum_color, sum_color, dirt_color);

                } else if (state.t >= far_clip) {

                    vadd(sum_color, sum_color, base_color);

                } else {

                    // mix
                    mix_color(col, base_color, dirt_color,
                              pow(1.0 - ( (state.t - near_clip) / 
                                          (far_clip - near_clip) ),
                                  1.0f / dirt_gain));

                    vadd(sum_color, sum_color, col);

                }

            } else {

                vadd(sum_color, sum_color, base_color);

            }

        }
    }

    double nsamples = ntheta_samples * nphi_samples;
    Lo[0] = sum_color[0] / nsamples;
    Lo[1] = sum_color[1] / nsamples;
    Lo[2] = sum_color[2] / nsamples;

    return 0;   /* OK */
}


/* ---------------------------------------------------------------------------
 *
 * Public functions 
 *
 * ------------------------------------------------------------------------ */

int
ri_transport_dirtmap(
    ri_render_t         *render,
    const ri_ray_t      *ray,
    ri_transport_info_t *result)
{

    ri_ray_t                eyeray;
    ri_intersection_state_t state;
    ri_vector_t             texcol;

    /*
     * Initialize
     */
    {
        ri_vector_setzero(result->radiance);
        result->nbound_diffuse  = 0;
        result->nbound_specular = 0;
        ri_intersection_state_clear( &result->state );

        memcpy(&eyeray, ray, sizeof(ri_ray_t));
    }    


    /*
     * Shoot eye ray.
     */
    int hit;
    int ret;

    hit = ri_raytrace(render, &eyeray, &state);

    if (hit) {

        ret = calculate_dirt(result->radiance,
                             &eyeray,
                             &state,
                             4, 4);

        if (state.geom->material && state.geom->material->texture) {

            ri_texture_fetch(texcol, state.geom->material->texture,
                             state.stqr[0], state.stqr[1]);

            result->radiance[0] *= texcol[0];
            result->radiance[1] *= texcol[1];
            result->radiance[2] *= texcol[2];
        }


    } else {

        vzero(result->radiance);

    }


    return 0;   /* OK */
}

