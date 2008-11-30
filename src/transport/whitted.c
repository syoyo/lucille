/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Classic Whitted style raytracer.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#include "whitted.h"

#include "raytrace.h"
#include "reflection.h"
#include "random.h"

#define MAX_TRACE_DEPTH 8

/* ---------------------------------------------------------------------------
 *
 * Private functions 
 *
 * ------------------------------------------------------------------------ */

static void trace_whitted(
    ri_render_t             *render,
    ri_intersection_state_t *isect,
    ri_transport_info_t     *result,
    int                      depth)
{
    double                      eps = 1.0e-7; 

    int                         hit;
    ri_intersection_state_t     state;
    ri_ray_t                    Rr;
    vec                         Rd;
    ri_ray_t                    Tr;
    vec                         Td;
    ri_float_t                  eta = 1.33;

    if (depth > MAX_TRACE_DEPTH) {
        return;
    }

    //ri_reflect(Rd, isect->I, isect->Ns);
    ri_refract(Rd, isect->I, isect->Ns, 1.33);
    vcpy(Rr.dir, Rd);

    Rr.org[0] = isect->P[0] + eps * Rd[0];
    Rr.org[1] = isect->P[1] + eps * Rd[1];
    Rr.org[2] = isect->P[2] + eps * Rd[2];

    hit = ri_raytrace(render, &Rr, &state);

    if (hit) {

        //vzero(result->radiance);
        trace_whitted(render, &state, result, depth + 1);

    } else {

        /*
         * If the scene has envmap, add contribution from the envmap.
         */
        if (render->scene->envmap_light) {

            ri_texture_ibl_fetch(result->radiance,
                                 render->scene->envmap_light->texture,
                                 Rd); 

        } else {

            vzero(result->radiance);
        
        }

    }

}


/* ---------------------------------------------------------------------------
 *
 * Public functions 
 *
 * ------------------------------------------------------------------------ */

int
ri_transport_whitted(
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

        trace_whitted(render, &state, result, 1);

    } else {

        /*
         * If the scene has envmap, add contribution from the envmap.
         */
        if (render->scene->envmap_light) {

            ri_texture_ibl_fetch(result->radiance,
                                 render->scene->envmap_light->texture,
                                 eyeray.dir); 

        } else {

            vzero(result->radiance);
        
        }

    }


    return 0;   /* OK */
}

