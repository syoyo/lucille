/*
 * $Id: transport.c,v 1.8 2004/08/15 05:19:39 syoyo Exp $
 *
 * Default light transport routine.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "vector.h"
#include "log.h"
#include "memory.h"
#include "render.h"
#include "raytrace.h"
#include "reflection.h"
#include "random.h"
#include "transport.h"
#include "shading.h"

static void          trace_path(
    ri_render_t         *render,
    ri_ray_t            *ray, 
    ri_transport_info_t *result);

static ri_light_t   *get_light(
    ri_render_t         *render);


/*
 * Function: ri_transport_sample
 *
 *     Triggers light transporter and computes radiance along the ray.
 *
 * Parameters:
 *
 *     *render - The global renderer data.
 *     *ray    - The ray originating camera which throughs pixel plane.
 *     *resut  - Light transport result(including radiance).
 *
 * Returns:
 *
 *     Always 1.
 */
int
ri_transport_sample(
    ri_render_t         *render,
    const ri_ray_t      *ray,
    ri_transport_info_t *result)
{
    ri_ray_t newray;

    memcpy(&newray, ray, sizeof(ri_ray_t));     /* copy */

    /*
     * Initialize
     */
    {
        ri_vector_setzero(result->radiance);
        result->nbound_diffuse  = 0;
        result->nbound_specular = 0;
        ri_intersection_state_clear( &result->state );
    }    



    trace_path(render, &newray, result);

    return 1;
}


/* ---------------------------------------------------------------------------
 *
 * Private functions 
 *
 * ------------------------------------------------------------------------ */

/*
 * Function: get_light
 *
 *     Returns the light in the scene. In current implementation, only one
 *     light is considered.
 *
 * Parameters:
 *
 *     *render - The global renderer data.
 *
 * Returns:
 *
 *     An light information.
 */
static ri_light_t *
get_light(ri_render_t *render)
{
    ri_light_t *light;

    if (!ri_list_first(render->scene->light_list)) {
        ri_log(LOG_WARN, "no light exist.\n");
        return NULL;
    }

    light = (ri_light_t *)ri_list_first(render->scene->light_list)->data;

    return light;
}

/*
 * Function: trace_path
 *
 *     Samples light transport path in recursive manner. In curent
 *     implementation, <trace_path> acts as distribution ray tracing.
 *
 * Parameters:
 *
 *     *render - The global renderer data.
 *     *ray    - The ray to be traced.
 *     *resut  - Light transport result(including radiance).
 *
 * Returns:
 *
 *     None.
 */
static void
trace_path(ri_render_t *render, ri_ray_t *ray, ri_transport_info_t *result)
{
    int max_nbound_specular = 10;

    if (result->nbound_specular > max_nbound_specular) {
        /* Too much reflection, terminate.  */
        return;
    }

    ri_light_t *light = NULL;

    int hit;

    /* hack */
    vec white;
    vec black;

    ri_vector_set1(white,  1.0);
    ri_vector_set1(black,  0.0);

    hit = ri_raytrace(render, ray, &(result->state));

    if (hit) {

        if (result->state.geom->light) {

            light = result->state.geom->light;

            /* Hit light geometry. */
            vcpy(result->radiance, light->col);
            return;

        }

        vcpy(result->radiance, white);

    } else {

        vcpy(result->radiance, black);

    }

    return;

#if 0 // TODO
    int hit, lighthit;
    int hasfresnel;
    ri_vector_t col;
    ri_vector_t transmit_ray;
    ri_vector_t reflect_ray;
    ri_vector_t offset;
    ri_vector_t raydir;
    ri_vector_t rayorg;
    ri_vector_t refrad, trasrad;
    ri_vector_t normal;
    ri_light_t *light;
    ri_vector_t rad;
    ri_material_t *material;
    ri_ray_t       lightray;
    ri_transport_info_t ref_result;        /* for reflection */
    double r, d, s, t;
    float  fresnel_factor = 1.0f;
    float  kr, kt;
    float  eta = 1.0f / 1.4f;
    float  etaval;

    if (result->nbound_specular > 8) {
        //printf("too reflection\n");
        return;
    }

    light = get_light(render);

    ri_vector_copy(&raydir, ray->dir);
    result->state.inside = 0;
    hit = ri_raytrace(render, ray, &(result->state));

    if (hit) {

        if (light->geom) {
            /* Check if a ray also hits light geometry and
             * that is closer than scene geometry or not.
             */
            ri_vector_copy(&lightray.org, ray->org);
            ri_vector_copy(&lightray.dir, ray->dir);

            lighthit = ri_raytrace_geom(
                    light->geom,
                    &lightray,
                    &(result->state));            

            if (lighthit && (lightray.isectt < ray->isectt) ) {

                // light is "seen"
                ri_vector_copy(&result->radiance,
                           light->col);
                result->hit = 1;
                return;
            }
        }

        r = randomMT();

        material = result->state.geom->material;
        if (!material) {
            d = 1.0;
            s = 0.0;
            t = 0.0;
        } else {
            d = ri_vector_ave(&material->kd);
            s = ri_vector_ave(&material->ks);
            t = ri_vector_ave(&material->kt);
        }

        if (s > 0.0) {
            /* specular reflection */

            if (result->state.geom->material &&
                result->state.geom->material->fresnel) {

                /* Fresnel reflection */
            
                ri_fresnel(&ray->dir, &transmit_ray,
                       &kr, &kt,
                       &ray->dir, &result->state.normal,
                       eta);

                fresnel_factor = kr;

            } else {
                ri_reflect(&(ray->dir),
                       &ray->dir,
                       &result->state.normal);

                fresnel_factor = 1.0f;
            }

    
            ri_vector_copy(&(ray->org), result->state.P);

            ri_vector_copy(&col, result->state.color);


            result->nbound_specular++;

            /* push radiance */
            ri_vector_copy(&rad, result->radiance);
            ri_vector_zero(&(result->radiance));

            trace_path(render, ray, result);

            /* pop radiance */
            ri_vector_mul(&(result->radiance),
                      &result->radiance, &material->ks);
            ri_vector_mul(&(result->radiance),
                      &result->radiance, &col);
            ri_vector_scale(&(result->radiance),
                        fresnel_factor);


            ri_vector_add(&(result->radiance),
                      &result->radiance,
                      &rad);
        }

        if (d > 0.0) {
            /* diffuse reflection */
            result->nbound_diffuse++;
            ri_shade(&rad, &ray->dir, ray, &(result->state));

            ri_vector_mul(&rad,
                      &rad, &material->kd);
            ri_vector_add(&(result->radiance), &result->radiance,
                                   &rad);    
        }


        if (t > 0.0) {
            /* specular refraction */

            if (result->state.geom->material &&
                result->state.geom->material->fresnel) {
                hasfresnel = 1;
            } else {
                hasfresnel = 0;
            }


            if (hasfresnel) {
                /* Fresnel effect */
            
                ri_vector_copy(&normal,
                           result->state.normal);

                if (result->state.inside) {
                    printf("inside val = %d\n", result->state.inside);
                    printf("inside\n");
                    /* ray hits inside surface */
                    //ri_vector_neg(&normal);
                    etaval = 1.0 / eta;
                } else {
                    etaval = eta;
                }

                ri_fresnel(&reflect_ray, &transmit_ray,
                       &kr, &kt,
                       &ray->dir, &normal,
                       etaval);

            } else {
                ri_refract(&(ray->dir),
                       &ray->dir,
                       &result->state.normal,
                       eta);

                kr = 0.0; kt = 1.0;
            }

            /* slightly moves the ray towards outgoing direction */

            ri_vector_copy(&rayorg, result->state.P);

            /* ray.org = ray.org + 0.001 * ray.dir */
            ri_vector_copy(&offset, &transmit_ray);
            ri_vector_scale(&offset, 0.001);
            ri_vector_add(&(ray->org), &rayorg, &offset);

            /* ray.dir = refract direction */
            ri_vector_copy(&(ray->dir), &transmit_ray);
 
            ri_vector_copy(&col, &result->state.color);

            result->nbound_specular++;
            ray->prev_hit = 'S';

            /* push radiance */
            ri_vector_copy(&rad, &result->radiance);
            ri_vector_zero(&(result->radiance));

            trace_path(render, ray, result);

            /* pop radiance */
            ri_vector_mul(&(result->radiance),
                      &result->radiance, &material->kt);
            ri_vector_mul(&(result->radiance),
                      &result->radiance, &col);
            ri_vector_scale(&(result->radiance), kt);

            if (hasfresnel) {
                /* add reflection color */

                /* ray.org = ray.org + 0.001 * ray.dir */
                ri_vector_copy(&offset, &reflect_ray);
                ri_vector_scale(&offset, 0.001);
                ri_vector_add(&(ray->org), &rayorg, &offset);

                ri_vector_copy(&(ray->dir), &reflect_ray);
                ri_vector_copy(&col, &result->state.color);

                ray->prev_hit = 'S';

                ri_vector_zero(&ref_result.radiance);
                ref_result.nbound_specular = result->nbound_specular;
                ref_result.nbound_diffuse = result->nbound_diffuse;
                ref_result.state.inside = 0;

                trace_path(render, ray, &ref_result);

                /* pop radiance */
                ri_vector_mul(&(ref_result.radiance),
                          &ref_result.radiance, &col);
                ri_vector_scale(&(ref_result.radiance), kr);
                ri_vector_add(&(result->radiance),
                          &result->radiance, &ref_result.radiance);

            }

            ri_vector_add(&(result->radiance),
                          &result->radiance,
                      &rad);
        }

    //} else if (result->nbound_specular + result->nbound_diffuse < 2) {
    } else {

        /* check if hit light geometry */
        ray->isectt = 0.0f;

        if (light->type == LIGHTTYPE_IBL ||
            light->type == LIGHTTYPE_SUNSKY) {

            ri_texture_ibl_fetch(&(result->radiance),
                         light->texture,
                         &ray->dir);
            result->hit = 1;
            return;    
        } else if (ri_render_get()->background_map) {

            ri_texture_ibl_fetch(&(result->radiance),
                         ri_render_get()->background_map,
                         &ray->dir);
            result->hit = 1;
            return;    

        
        } else {
            if (light->geom) {
                /* area light. */

                lighthit = ri_raytrace_geom(
                        light->geom,
                        ray,
                        &(result->state));            
                if (lighthit) {

                    // light is "seen"
                    result->radiance.e[0] = 1.0;
                    result->radiance.e[1] = 1.0;
                    result->radiance.e[2] = 1.0;
                    result->hit = 1;
                    return;
                }
            } else if (light->type == LIGHTTYPE_DOME) {
                //ri_vector_copy(&(result->radiance),
                //           &(light->col));
                //ri_vector_scale(&(result->radiance),
                //        (float)light->intensity);
            }
        }
    } 
#endif

    return;

}
