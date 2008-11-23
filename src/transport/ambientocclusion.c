/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Ambient occlusion computes ratio of occlusion and turns it into the shaded
 * color.
 * For example, the shading point is completely occluded from any direction
 * of the hemisphere over the shading point(imagine inside of the box),
 * occlusion value is 1.0.
 * If the shading point is not blocked by any obscrances from every direction
 * over the hemisphere(e.g. outdoor scene), occlusion value becomes 0.0.
 */

/*
 * TODO: refactor source code.
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#include "ambientocclusion.h"

#include "raytrace.h"
#include "reflection.h"
#include "random.h"

/* ---------------------------------------------------------------------------
 *
 * Private functions 
 *
 * ------------------------------------------------------------------------ */

int
calculate_occlusion(
    ri_vector_t                    Lo,              /* [out] */
    const ri_intersection_state_t *isect,
    uint32_t                       ntheta_samples,
    uint32_t                       nphi_samples)
{
    int                     hit;
    uint32_t                i, j, k;

    double                  z0, z1;
    double                  cos_theta, phi;
    double                  eps = 1.0e-5;
    double                  occlusion = 0.0;

    vec                     dir;
    vec                     basis[3];

    ri_ray_t                ray;
    ri_intersection_state_t state;

    ri_ortho_basis(basis, isect->Ns);

    vcpy(ray.org, isect->P);

    /*
     * Slightly move the shading point towards the surface normal.
     * FIXME: Choose eps relative to scene scale, not as an absolute value.
     */
    ray.org[0] += isect->Ns[0] * eps;
    ray.org[1] += isect->Ns[1] * eps;
    ray.org[2] += isect->Ns[2] * eps;


    for (j = 0; j < nphi_samples; j++) {
        for (i = 0; i < ntheta_samples; i++) {

            /*
             * 1. Choose random ray direction over the hemisphere.
             */

            /* Simple stratified sampling */
            z0 = (i + randomMT()) / (double)ntheta_samples;
            z1 = (j + randomMT()) / (double)nphi_samples;

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
                /* There's an occluder. */
                occlusion += 1.0;
            }

        }
    }

    /*
     * Turn occlusion value into color(radiance)
     *
     *  Lo = m * (N - occlusion) / N
     *
     *    where N = ntheta * nphi
     *          m = pi              if phisically accurate value is required.
     *              1.0             otherwise
     */
    double nsamples = ntheta_samples * nphi_samples;
    double m = 1.0; // (1.0 / M_PI)
    Lo[0] = m * (nsamples - occlusion) / nsamples;
    Lo[1] = m * (nsamples - occlusion) / nsamples;
    Lo[2] = m * (nsamples - occlusion) / nsamples;


    return 0;   /* OK */
}

/* ---------------------------------------------------------------------------
 *
 * Public functions 
 *
 * ------------------------------------------------------------------------ */

int
ri_transport_ambientocclusion(
    ri_render_t         *render,
    const ri_ray_t      *ray,
    ri_transport_info_t *result)
{

    ri_ray_t                eyeray;
    ri_intersection_state_t state;

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

        ret = calculate_occlusion(result->radiance,
                                  &state,
                                  8, 8);

    } else {

        vzero(result->radiance);

    }


    return 0;   /* OK */
}

