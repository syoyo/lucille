#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#include "IBLSampler.h"

#include "ray.h"
#include "reflection.h"
#include "random.h"
#include "raytrace.h"
#include "image_saver.h"
#include "beam.h"
#include "memory.h"

//
// TODO:
//
//  - Use FFT for faster convolution?
//

#define RESOLUTION  (128)
#define SDIV        (2)

#define MIN_DIV_SIZE (M_PI / (RESOLUTION))

// Precalculate sin and cos for faster execution.
double sin_theta_cache[8][SDIV * SDIV];
double cos_theta_cache[8][SDIV * SDIV];
double sin_phi_cache[8][SDIV * SDIV];
double cos_phi_cache[8][SDIV * SDIV];

static vec    dir_cache[8][SDIV * SDIV][4];
static vec   *dirmap;
static int    dirmap_width;
static int    dirmap_height;

static int    dtrace = 0;

//
// Stats
//
uint64_t ncompletely_visible    = 0;
uint64_t npartially_visible     = 0;
uint64_t ncompletely_invisible  = 0;

#if 0
static void
init_sin_cos_cache()
{
    int o, u;
    int sx, sy;

    double theta;
    double phi;
    double theta_step = (0.5 * M_PI) / SDIV;
    double phi_step   = (2.0 * M_PI) / (4.0 * SDIV);

    for (u = 0; u < 2; u++) {               // upper and lower
        for (o = 0; o < 4; o++) {               // half of octants

            for (sy = 0; sy < SDIV; sy++) {        // sub-regions
                for (sx = 0; sx < SDIV; sx++) {

                    theta = u * (0.5 * M_PI) + sy * theta_step;
                    phi   = o * (M_PI * 0.5) + sx * phi_step;

                    sin_theta_cache[u*4+o][sy*SDIV+sx] = sin(theta);
                    cos_theta_cache[u*4+o][sy*SDIV+sx] = cos(theta);
                    sin_phi_cache[u*4+o][sy*SDIV+sx] = sin(phi);
                    cos_phi_cache[u*4+o][sy*SDIV+sx] = cos(phi);
                    
                }
            }
        }
    } 
}
#endif


//
// TODO: Use SAT technique.
//

static void uv2xyz(
    double *x,
    double *y,
    double *z,
    double  u,                  /* [0, pi]      */
    double  v)                  /* [0, 2 pi]    */
{
    //
    // The scene is defined in +Y up coord, so we align to this coord.
    // It differs standard uv2xyz definition which is defined in +Z up coord.)
    //
    //(*x) = cos(v) * sin(u);
    //(*y) = sin(v) * sin(u);
    //(*z) = cos(u);
    (*z) = cos(v) * sin(u);     // x
    (*x) = sin(v) * sin(u);     // y
    (*y) = cos(u);              // z
}

static void
init_dir_map(int width, int height)
{
    int i, j;
    double theta, phi;
    
    dirmap = (vec *)ri_mem_alloc(sizeof(vec) * width * height);

    for (j = 0; j < height; j++) {
        for (i = 0; i < width; i++) {

                theta = j * (M_PI) / (double)width;
                phi   = i * (2.0 * M_PI) / (double)height;

                // Add eps to avoid numerical problem.
                uv2xyz(&(dirmap[j * width + i][0]),
                       &(dirmap[j * width + i][1]),
                       &(dirmap[j * width + i][2]),
                        theta, phi);
        }
    }

    dirmap_width  = width;
    dirmap_height = height;
}

static void
get_dir(vec dir, double theta, double phi)
{
    int u, v;

    u = (int)((phi / (2.0 * M_PI)) * dirmap_width);
    if (u <0) u = 0;
    if (u >= dirmap_width) u = dirmap_width - 1;

    v = (int)((theta / M_PI) * dirmap_height);
    if (v <0) v = 0;
    if (v >= dirmap_height) v = dirmap_height - 1;

    vcpy(dir, dirmap[v * dirmap_width + u]);
}

static void
init_dir_cache()
{
    int o, u;
    int sx, sy;

    double theta;
    double phi;
    double theta_step = (0.5 * M_PI) / SDIV;
    double phi_step   = (2.0 * M_PI) / (4.0 * SDIV);
    double eps        = 0.001;

    vec    *dir;

    for (u = 0; u < 2; u++) {               // upper and lower
        for (o = 0; o < 4; o++) {               // half of octants

            for (sy = 0; sy < SDIV; sy++) {        // sub-regions
                for (sx = 0; sx < SDIV; sx++) {

                    theta = u * (0.5 * M_PI) + sy * theta_step;
                    phi   = o * (M_PI * 0.5) + sx * phi_step;

                    //sin_theta_cache[u*4+o][sy*SDIV+sx] = sin(theta);
                    //cos_theta_cache[u*4+o][sy*SDIV+sx] = cos(theta);
                    //sin_phi_cache[u*4+o][sy*SDIV+sx] = sin(phi);
                    //cos_phi_cache[u*4+o][sy*SDIV+sx] = cos(phi);

                    dir = dir_cache[u*4+o][sy*SDIV+sx];

                    // hack to avoid numerical problem.
                    if (theta < 0.001) theta = 0.001;
                    if (theta > M_PI - 0.001) theta = M_PI - 0.001;

                    // Add eps to avoid numerical problem.
                    uv2xyz(&dir[0][0], &dir[0][1], &dir[0][2],
                           theta + eps, phi + eps);
                    uv2xyz(&dir[1][0], &dir[1][1], &dir[1][2],
                           theta + eps, phi + phi_step - eps);
                    uv2xyz(&dir[2][0], &dir[2][1], &dir[2][2],
                           theta + theta_step - eps, phi + eps);
                    uv2xyz(&dir[3][0], &dir[3][1], &dir[3][2],
                           theta + theta_step - eps, phi + phi_step - eps);
                    
                }
            }
        }
    } 

}

static int
check_visibility(
    ri_bvh_t *bvh,
    vec       org,
    vec       dir[4])
{
    ri_beam_t beam;
    int invalid;
    int vis;

    /*
     * Directions passed to ri_beam_set() is defined in the following order.
     *
     * 0 ---- 3
     * |      |
     * |      |
     * 1 ---- 2
     *
     */

    vec beamdir[4];

    vcpy(beamdir[0], dir[0]);
    vcpy(beamdir[1], dir[2]);
    vcpy(beamdir[2], dir[3]);
    vcpy(beamdir[3], dir[1]);

    invalid = ri_beam_set( &beam, org, beamdir );
    if (invalid) exit(0);
    assert(invalid == 0);

    vis  = ri_bvh_intersect_beam_visibility(
                (void *)bvh, &beam, NULL);

    return vis;

}

static void
contribute_debug(
    float          *dst,
    float          *L,                  /* IBL image in long-lat coord */
    ri_vector_t     n,
    double          theta,
    double          phi,
    double          theta_step,
    double          phi_step,
    int             width,
    int             height)
{

    int u, v;
    int us, ue;
    int vs, ve;
    int idx;

    double u_step, v_step;
    double t, p; 
    double dir[3];
    double cosTheta;


    us = (int)((phi / (2.0 * M_PI)) * width);
    if (us >= width) us = width - 1;
    ue = (int)(((phi + phi_step) / (2.0 * M_PI)) * width);
    if (ue >= width) ue = width - 1;
    u_step = phi_step / (ue - us);

    vs = (int)((theta / M_PI) * height);
    if (vs >= height) vs = height - 1;
    ve = (int)(((theta + theta_step) / M_PI) * height);
    if (ve >= height) ve = height - 1;
    v_step = theta_step / (ve - vs);

    // TODO: employ fast spherical interpolation?

    for (v = vs; v < ve; v++) {

        t = theta + (v - vs) * v_step; 

        for (u = us; u < ue; u++) {

            p = phi + (u - us) * u_step; 

            //uv2xyz(&dir[0], &dir[1], &dir[2], t, p);
            dir[0] = dirmap[v * width + u][0];
            dir[1] = dirmap[v * width + u][1];
            dir[2] = dirmap[v * width + u][2];

            cosTheta = vdot(dir, n);
            if (cosTheta < 0.0) cosTheta = 0.0;

            // L x cosTheta
            idx = 4 * (v * width + u);
            dst[idx + 0] = 1.0;
            dst[idx + 1] = 0.0;
            dst[idx + 2] = 0.0;
        }
    }

}


static void
contribute(
    float          *dst,
    float          *L,                  /* IBL image in long-lat coord */
    ri_vector_t     n,
    double          theta,
    double          phi,
    double          theta_step,
    double          phi_step,
    int             width,
    int             height)
{

    int u, v;
    int us, ue;
    int vs, ve;
    int idx;

    double u_step, v_step;
    double t, p; 
    double dir[3];
    double cosTheta;


    us = (int)((phi / (2.0 * M_PI)) * width);
    if (us >= width) us = width - 1;
    ue = (int)(((phi + phi_step) / (2.0 * M_PI)) * width);
    if (ue >= width) ue = width - 1;
    u_step = phi_step / (ue - us);

    vs = (int)((theta / M_PI) * height);
    if (vs >= height) vs = height - 1;
    ve = (int)(((theta + theta_step) / M_PI) * height);
    if (ve >= height) ve = height - 1;
    v_step = theta_step / (ve - vs);

    // TODO: employ fast spherical interpolation?

    for (v = vs; v < ve; v++) {

        t = theta + (v - vs) * v_step; 

        for (u = us; u < ue; u++) {

            p = phi + (u - us) * u_step; 

            //uv2xyz(&dir[0], &dir[1], &dir[2], t, p);
            dir[0] = dirmap[v * width + u][0];
            dir[1] = dirmap[v * width + u][1];
            dir[2] = dirmap[v * width + u][2];

            cosTheta = vdot(dir, n);
            if (cosTheta < 0.0) cosTheta = 0.0;

            // L x cosTheta
            idx = 4 * (v * width + u);
            dst[idx + 0] = cosTheta * L[idx + 0];
            dst[idx + 1] = cosTheta * L[idx + 1];
            dst[idx + 2] = cosTheta * L[idx + 2];
        }
    }

}

static void
contribute_pixel(
    float          *dst,
    float          *L,                  /* IBL image in long-lat coord */
    ri_vector_t     n,
    double          theta,
    double          phi,
    vec             dir,
    int             width,
    int             height)
{

    int us;
    int vs;
    int idx;

    double cosTheta;


    us = (int)((phi / (2.0 * M_PI)) * width);
    if (us >= width) us = width - 1;

    vs = (int)((theta / M_PI) * height);
    if (vs >= height) vs = height - 1;

    cosTheta = vdot(dir, n);
    if (cosTheta < 0.0) cosTheta = 0.0;

    // L x cosTheta
    idx = 4 * (vs * width + us);
    dst[idx + 0] = cosTheta * L[idx + 0];
    dst[idx + 1] = cosTheta * L[idx + 1];
    dst[idx + 2] = cosTheta * L[idx + 2];
}

static void
mc_sample_subregion(
    ri_bvh_t                        *bvh,
    const ri_texture_t              *Lmap,
    ri_texture_t                    *prodmap,
    const ri_intersection_state_t   *isect,
    ri_vector_t                      n,
    double                           theta,
    double                           phi,
    double                           theta_step,
    double                           phi_step,
    int                              depth)
{
    int i;
    int hit;
    int area_x;
    int pixel_size;
    int nsamples;
    int x, y;
    double u, v;
    
    vec                     raydir;
    ri_ray_t                ray;
    ri_intersection_state_t state;

    area_x = Lmap->width / 8;

    pixel_size = area_x / pow(2, depth);

    if (pixel_size < 1) pixel_size = 1;

    // at least, area covered by this subregion is 1x2 pixel.
    nsamples = pixel_size * 2;

    //printf("depth  = %d, pixel_size = %d, nsamples = %d\n", depth, pixel_size, nsamples);

    /* slightly move the shading point towards the surface normal */
    vcpy(ray.org, isect->P);
    ray.org[0] += isect->Ns[0] * 0.00001;
    ray.org[1] += isect->Ns[1] * 0.00001;
    ray.org[2] += isect->Ns[2] * 0.00001;

    for (y = 0; y < nsamples * 2; y++) {
        for (x = 0; x < nsamples; x++) {

            u = theta + ((y + randomMT()) / (double)(nsamples*2)) * theta_step;
            v = phi + ((x + randomMT()) / (double)(nsamples)) * phi_step;

            uv2xyz(&raydir[0], &raydir[1], &raydir[2], u, v);

            vcpy(ray.dir, raydir);

            hit = ri_bvh_intersect( (void *)bvh, &ray, &state, NULL );

            if (!hit) {

                contribute_pixel(
                    prodmap->data,
                    Lmap->data,
                    n,
                    u,
                    v,
                    raydir,
                    prodmap->width,
                    prodmap->height);

            }
        }
    }
}
    


static void
render_subregion(
    ri_bvh_t                        *bvh,
    const ri_texture_t              *Lmap,
    ri_texture_t                    *prodmap,
    const ri_intersection_state_t   *isect,
    ri_vector_t                      n,
    double                           theta,
    double                           phi,
    double                           theta_step,
    double                           phi_step,
    vec                              dir[4],
    int                              depth)
{
    //vec    dir[4];
    vec    sub_dir[4];
    double dot;
    double sub_theta;
    double sub_phi;
    double sub_theta_step;
    double sub_phi_step;
    //double eps = 1.0e-6;
    //vec    basis[3];
    

    int i, j;


    if ((theta_step < MIN_DIV_SIZE) ||
        (phi_step   < MIN_DIV_SIZE) ||
        (depth      > 2)) {

        if (dtrace) {
            contribute_debug(
                prodmap->data,
                Lmap->data,
                n,
                theta,
                phi,
                theta_step,
                phi_step,
                prodmap->width,
                prodmap->height);
            return;
        }
#if 0
        // FIXME!
        contribute(
            prodmap->data,
            Lmap->data,
            n,
            theta,
            phi,
            theta_step,
            phi_step,
            prodmap->width,
            prodmap->height);
#endif

        /* Switch to monte carlo sampling */
        mc_sample_subregion(
            bvh,
            Lmap,
            prodmap,
            isect,
            n,
            theta,
            phi,
            theta_step,
            phi_step,
            depth);

        return;
    }


    /* Early check of visibility.
     * If all dot product of direction and normal are negative,
     * the beam is under the hemisphere.
     * In this case, there's no need to trace for checking visibility.
     */

    int nunder_hemisphere = 0;
    for (i = 0; i < 4; i++) {
        dot = vdot(dir[i], n);

        //printf("dir[%d]  = %f, %f, %f\n", i, dir[i][0], dir[i][1], dir[i][2]);
        //printf("vdot[%d] = %f\n", i, dot);

        if (dot < 0.0) nunder_hemisphere++;
    }
    
    //printf("nunder_hemisphere = %d\n", nunder_hemisphere);

    if (nunder_hemisphere == 4) {
        /* The beam is completely under the hemisphere . */
        //printf("Invisible: theta = %f, phi = %f, underhemi for normal (%f, %f, %f)\n",
        //    theta, phi, n[0], n[1], n[2]);
        return;
    }

    /* If any dot product is negative, subdivide the beam */
    if (nunder_hemisphere != 0) {
        
        for (j = 0; j < 2; j++) {           // theta
            for (i = 0; i < 2; i++) {       // phi

                /*
                 * 0 ---- 1        ----> phi
                 * |      |        |
                 * |      |        |/
                 * 2 ---- 3        theta
                 */
                sub_theta       = theta + 0.5 * j * theta_step;
                sub_phi         = phi   + 0.5 * i * phi_step;
                sub_theta_step  = 0.5 * theta_step;
                sub_phi_step    = 0.5 * phi_step;

                get_dir(sub_dir[0], sub_theta, sub_phi);
                get_dir(sub_dir[1], sub_theta, sub_phi + sub_phi_step);
                get_dir(sub_dir[2], sub_theta + sub_theta_step, sub_phi);
                get_dir(sub_dir[3], sub_theta + sub_theta_step, sub_phi + sub_phi_step);

                render_subregion(bvh, Lmap, prodmap, isect, n,
                                 sub_theta, sub_phi,
                                 sub_theta_step, sub_phi_step,
                                 sub_dir, depth + 1);
            }
        }

    } else {
        
        //printf("theta = %f, phi = %f, upperhemi for normal (%f, %f, %f)\n",
        //    theta, phi, n[0], n[1], n[2]);

        /*
         * Beam is upper hemisphere defined by normal 'n'.
         * Check visibility.
         */
        vec org;
        int vis;

        org[0] = isect->P[0] + 0.01 * n[0];
        org[1] = isect->P[1] + 0.01 * n[1];
        org[2] = isect->P[2] + 0.01 * n[2];

        vis = check_visibility( bvh, org, dir );

        if (vis == RI_BEAM_HIT_PARTIALLY) {

            npartially_visible++;

            // Needs subdivision.

            for (j = 0; j < 2; j++) {           // theta
                for (i = 0; i < 2; i++) {       // phi

                    /*
                     * 0 ---- 1        ----> phi
                     * |      |        |
                     * |      |        |/
                     * 2 ---- 3        theta
                     */
                    sub_theta       = theta + 0.5 * j * theta_step;
                    sub_phi         = phi   + 0.5 * i * phi_step;
                    sub_theta_step  = 0.5 * theta_step;
                    sub_phi_step    = 0.5 * phi_step;

                    // TODO: calc sub_dir

                    get_dir(sub_dir[0], sub_theta, sub_phi);
                    get_dir(sub_dir[1], sub_theta, sub_phi + sub_phi_step);
                    get_dir(sub_dir[2], sub_theta + sub_theta_step, sub_phi);
                    get_dir(sub_dir[3], sub_theta + sub_theta_step, sub_phi + sub_phi_step);


                    render_subregion(bvh, Lmap, prodmap, isect, n,
                                     sub_theta, sub_phi,
                                     sub_theta_step, sub_phi_step,
                                     sub_dir, depth + 1);
                }
            }

        } else if (vis == RI_BEAM_HIT_COMPLETELY) {

            // Invisible. Do nothing.
            ncompletely_invisible++;

        } else if (vis == RI_BEAM_MISS_COMPLETELY) {

            // Visible all region coverd by the beam.
            ncompletely_visible++;

            contribute(
                prodmap->data,
                Lmap->data,
                n,
                theta,
                phi,
                theta_step,
                phi_step,
                prodmap->width,
                prodmap->height);
        }
    }
}

static void
convolve(
    ri_vector_t   Lo,
    ri_texture_t *prodmap)
{
    /* TODO: Use hierarchical summation technique to speed up convolution. */

    int i;
    double k = 2.0 * M_PI;
    
    vec L;
    vzero(L);

    if (dtrace) {
        ri_image_save_hdr("prod.hdr", prodmap->data, prodmap->width, prodmap->height);
    }
    
    for (i = 0; i < prodmap->width * prodmap->height; i++) {
        L[0] += prodmap->data[4 * i + 0];
        L[1] += prodmap->data[4 * i + 1];
        L[2] += prodmap->data[4 * i + 2];
    }

    Lo[0] = k * L[0] / (prodmap->width * prodmap->height);
    Lo[1] = k * L[1] / (prodmap->width * prodmap->height);
    Lo[2] = k * L[2] / (prodmap->width * prodmap->height);

}


/*
 * Sample IBL with beam.
 *
 * - Divide hemisphere into 16 sub-regions(4 sub-regions for each octant).
 * - Trace beam from the sub-region.
 * - calc visibility map and IBL map.
 * - calc final contribution from IBL with visibiity.
 */
void
sample_ibl_beam(
    ri_vector_t                      Lo,                /* [out]            */
    ri_bvh_t                        *bvh,
    const ri_texture_t              *Lmap,
    ri_texture_t                    *prodmap,           /* [buffer]         */
    const ri_intersection_state_t   *isect,
    int                              debug)
{

    int o, u;
    int sx, sy;

    static int initialized = 0;

    (void)Lmap;
    (void)isect;
    
    double theta_step = (0.5 * M_PI) / SDIV;
    double phi_step   = (2.0 * M_PI) / (4.0 * SDIV);

    double theta[4], phi[4];                // left, top, right, bottom

    vec    dir[4];
    vec    n;

    dtrace = debug;

    if (!initialized) {
        init_dir_cache();
        init_dir_map(Lmap->width, Lmap->height);
        initialized = 1;
    } 

    vcpy(n, isect->Ns);

    for (u = 0; u < 2; u++) {               // upper and lower
        for (o = 0; o < 4; o++) {               // half of octants

            for (sy = 0; sy < SDIV; sy++) {        // sub-regions
                for (sx = 0; sx < SDIV; sx++) {

                    theta[0] = u * (0.5 * M_PI) + (sy + 0) * theta_step;
                    theta[1] = u * (0.5 * M_PI) + (sy + 0) * theta_step;
                    theta[2] = u * (0.5 * M_PI) + (sy + 1) * theta_step;
                    theta[3] = u * (0.5 * M_PI) + (sy + 1) * theta_step;

                    phi[0]   = o * (M_PI * 0.5) + (sx + 0) * phi_step;
                    phi[1]   = o * (M_PI * 0.5) + (sx + 1) * phi_step;
                    phi[2]   = o * (M_PI * 0.5) + (sx + 0) * phi_step;
                    phi[3]   = o * (M_PI * 0.5) + (sx + 1) * phi_step;

                    //for (i = 0; i < 4; i++) {
                    //    printf("[%d] = (%f, %f)\n", i, theta[i], phi[i]);
                    //}

                    // get direction from precomputed table.
                    vcpy(dir[0], dir_cache[u*4+o][sy*SDIV+sx][0]);
                    vcpy(dir[1], dir_cache[u*4+o][sy*SDIV+sx][1]);
                    vcpy(dir[2], dir_cache[u*4+o][sy*SDIV+sx][2]);
                    vcpy(dir[3], dir_cache[u*4+o][sy*SDIV+sx][3]);

                    render_subregion(
                        bvh,
                        Lmap,
                        prodmap,
                        isect,
                        n,
                        theta[0], phi[0], theta_step, phi_step,
                        dir, 0);

                }
            }
        }
    }

    // hack
    //ri_image_save_hdr("integral.hdr", prodmap->data, prodmap->width, prodmap->height);

    convolve(Lo, prodmap);

    //printf("Lo = %f, %f, %f\n", Lo[0], Lo[1], Lo[2]);
    //exit(0);
}

/*
 * Simple MC sampling for reference.
 */
void
sample_ibl_naive(
    ri_vector_t                      Lo,                /* [out]            */
    ri_bvh_t                        *bvh,
    const ri_texture_t              *iblmap,
    const ri_intersection_state_t   *isect,
    uint32_t                         ntheta_samples,
    uint32_t                         nphi_samples)
{
    int                     k;
    int                     hit;
    uint32_t                u, v;
    ri_float_t              z0, z1;

    vec                     radiance;
    vec                     power;
    vec                     basis[3];
    vec                     dir;
    ri_ray_t                ray;
    ri_intersection_state_t state;
    ri_float_t              cosTheta, phi;

    vzero(power);

    ri_ortho_basis(basis, isect->Ns);

    ri_vector_copy(ray.org, isect->P);

    /* slightly move the shading point towards the surface normal */
    ray.org[0] += isect->Ns[0] * 0.00001;
    ray.org[1] += isect->Ns[1] * 0.00001;
    ray.org[2] += isect->Ns[2] * 0.00001;

    
    for (v = 0; v < nphi_samples; v++) {
        for (u = 0; u < ntheta_samples; u++) {

            /*
             * Do importance sampling and stratified sampling for theta and phi.
             * theta is drawn from the following probability.
             *
             *   p(x) ~ cos(theta) / PI  (against differential solid angle).
             *
             */
            z0 = ((ri_float_t)u + randomMT())/ (ri_float_t)ntheta_samples;
            z1 = ((ri_float_t)v + randomMT())/ (ri_float_t)nphi_samples;

            cosTheta = sqrt(z0);
            phi = 2.0 * M_PI * z1;
            dir[0] = cos(phi) * cosTheta;
            dir[1] = sin(phi) * cosTheta;
            dir[2] = sqrt(1.0 - cosTheta * cosTheta);

            for (k = 0; k < 3; k++) {
                ray.dir[k] = dir[0]*basis[0][k]
                           + dir[1]*basis[1][k]
                           + dir[2]*basis[2][k];
            }

            hit = ri_bvh_intersect( (void *)bvh, &ray, &state, NULL );

            if (!hit) {

                /*
                 * Contribution from IBL.
                 */
                ri_texture_ibl_fetch(radiance,
                                     iblmap,
                                     ray.dir);

                vadd(power, power, radiance);
            }
        }
    }

    // Lo = (1/pi) * Lsum / (ntheta * nphi)
    vscale(Lo, power, 1.0 / (M_PI * ntheta_samples * nphi_samples));
}

void
report_ibl_stat()
{
    double total;
    double pct;

    total = ncompletely_visible + npartially_visible + ncompletely_invisible;

    pct   = 100.0 * ncompletely_invisible / total;
    printf("ncompletely_visible   = %d (%f %%)\n",
        (int)ncompletely_visible, pct);

    pct   = 100.0 * npartially_visible / total;
    printf("npartially_visible    = %d (%f %%)\n",
        (int)npartially_visible, pct);

    pct   = 100.0 * ncompletely_visible / total;
    printf("ncompletely_invisible = %d (%f %%)\n",
        (int)ncompletely_invisible, pct);
}

void
clear_ibl_stat()
{
    ncompletely_visible   = 0;
    npartially_visible    = 0;
    ncompletely_invisible = 0;
}
