#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "bvh.h"
#include "beam.h"
#include "memory.h"
#include "image_saver.h"

#include "controller.h"
#include "simplerender.h"

#include "IBLSampler.h"

ri_float_t fov = 45.0;

const int raster_size = 64;

static void
setup_camera(
    vec        corner,        /* [out] */
    vec        du,            /* [out] */
    vec        dv,            /* [out] */
    vec        dw,            /* [out] */
    vec        eye,
    vec        lookat,
    vec        up,
    int        width,
    int        height,
    ri_float_t fov)
{
    ri_float_t flen = 0.5 * width / tan(0.5 * (fov * M_PI / 180.0));

    /* dw = lookat - eye */
    vsub( dw, lookat, eye );

    vcross( du, dw, up );
    vnormalize( du );

    vcross( dv, dw, du );
    vnormalize( dv );

    /* dw = dw/|dw| */
    vnormalize( dw );

    /* corner = flen * dw - (width * du + height * dv) / 2 */

    corner[0] = flen * dw[0] - 0.5 * (width * du[0] + height * dv[0]);
    corner[1] = flen * dw[1] - 0.5 * (width * du[1] + height * dv[1]);
    corner[2] = flen * dw[2] - 0.5 * (width * du[2] + height * dv[2]);

}

static void
get_eyedir(
    vec   raydir,    /* [out] */
    vec   corner,
    vec   du,
    vec   dv,
    float s,
    float t)
{

    raydir[0] = corner[0] + s * du[0] + t * dv[0];
    raydir[1] = corner[1] + s * du[1] + t * dv[1];
    raydir[2] = corner[2] + s * du[2] + t * dv[2];

}

static void
get_beamdir(
    vec   raydir[4],    /* [out] */
    vec   corner,
    vec   du,
    vec   dv,
    float beamwidth,
    float s,
    float t)
{

    int i;

    float offset[4][2];

    offset[0][0] = 0.0;
    offset[0][1] = 0.0;
    offset[1][0] = beamwidth;
    offset[1][1] = 0.0;
    offset[2][0] = beamwidth;
    offset[2][1] = beamwidth;
    offset[3][0] = 0.0;
    offset[3][1] = beamwidth;

    for (i = 0; i < 4; i++) {

        raydir[i][0] = corner[0] + (s + offset[i][0]) * du[0] + (t + offset[i][1]) * dv[0];
        raydir[i][1] = corner[1] + (s + offset[i][0]) * du[1] + (t + offset[i][1]) * dv[1];
        raydir[i][2] = corner[2] + (s + offset[i][0]) * du[2] + (t + offset[i][1]) * dv[2];

    }

}


void
record_sample(
    float *image,
    int    width,
    int    height,
    float  x,
    float  y,
    vec    color)
{
    int s, t;

    s = (int)x;
    t = (int)y;

    if (s < 0 || s >= width) return;
    if (t < 0 || t >= height) return;

    image[3 * ((height - t - 1) * width + s) + 0] = color[0];
    image[3 * ((height - t - 1) * width + s) + 1] = color[1];
    image[3 * ((height - t - 1) * width + s) + 2] = color[2];

    (void)height;

}
    

void
simple_render(
    ri_bvh_t *bvh,
    float    *image,
    int       width,
    int       height,
    vec       eye,
    vec       lookat,
    vec       up)
{
    int      i, j;
    float    s, t;

    vec      corner;
    vec      du; 
    vec      dv; 
    vec      dw; 

    vec      radiance;
    vec      raydir;

    int                     hit;
    ri_ray_t                ray;
    ri_intersection_state_t state;
    ri_bvh_diag_t           diag;

    setup_camera(
        corner, du, dv, dw,
        eye, lookat, up,
        width, height, fov);

    ri_bvh_clear_stat_traversal();

    // MUST CALL
    ri_bvh_invalidate_cache( (void *)bvh );

    for (j = 0; j < height; j++) {
        for (i = 0; i < width; i++) {

            s = (float)i;
            t = (float)j;

            get_eyedir(
                raydir,
                corner, du, dv,
                s, t);
                
            memset( &state, 0, sizeof(ri_ray_t));
            vcpy( ray.org, eye );
            vcpy( ray.dir, raydir );

            memset( &state, 0, sizeof(ri_intersection_state_t));

            hit = ri_bvh_intersect( (void *)bvh, &ray, &state, &diag );

            if (hit) {

                if (gvisualizeMode == VISUALIZE_NUM_TRAVERSALS) {
                    radiance[0] = diag.ninner_node_traversals;
                    radiance[1] = diag.ninner_node_traversals;
                    radiance[2] = diag.ninner_node_traversals;
                } else if (gvisualizeMode == VISUALIZE_NUM_ISECTS) {
                    radiance[0] = diag.ntriangle_isects;
                    radiance[1] = diag.ntriangle_isects;
                    radiance[2] = diag.ntriangle_isects;
                } else if (gvisualizeMode == VISUALIZE_IMAGE) {
                    radiance[0] = state.t;
                    radiance[1] = state.t;
                    radiance[2] = state.t;
                }

            } else {

                radiance[0] = 0.0;
                radiance[1] = 0.0;
                radiance[2] = 0.0;

            }

            record_sample( image, width, height, s, t, radiance );

        }
    }

    ri_bvh_report_stat_traversal();
}

void
simple_render_progressive(
    ri_bvh_t *bvh,
    float    *image,
    int       width,
    int       height,
    vec       eye,
    vec       lookat,
    vec       up,
    int       nsamples)
{
    int      i, j;
    float    s, t;

    vec      corner;
    vec      du; 
    vec      dv; 
    vec      dw; 

    vec      radiance;
    vec      raydir;

    int                     hit;
    ri_ray_t                ray;
    ri_intersection_state_t state;
    ri_bvh_diag_t           diag;

    setup_camera(
        corner, du, dv, dw,
        eye, lookat, up,
        width, height, fov);

    ri_bvh_clear_stat_traversal();

    // MUST CALL
    ri_bvh_invalidate_cache( (void *)bvh );

    int nthetasamples = (int)sqrt(nsamples);
    int nphisamples   = (int)sqrt(nsamples);

    if (nthetasamples < 1) nthetasamples = 1;
    if (nphisamples   < 1) nphisamples   = 1;

    for (j = 0; j < height; j++) {
        for (i = 0; i < width; i++) {

            s = (float)i;
            t = (float)j;

            get_eyedir(
                raydir,
                corner, du, dv,
                s, t);
                
            memset( &state, 0, sizeof(ri_ray_t));
            vcpy( ray.org, eye );
            vcpy( ray.dir, raydir );

            memset( &state, 0, sizeof(ri_intersection_state_t));

            hit = ri_bvh_intersect( (void *)bvh, &ray, &state, &diag );

            if (hit) {

                sample_ibl_naive(radiance, bvh, giblmap, &state, nthetasamples, nphisamples);

            } else {

                radiance[0] = 0.0;
                radiance[1] = 0.0;
                radiance[2] = 0.0;

            }

            record_sample( image, width, height, s, t, radiance );

        }
    }

    //ri_bvh_report_stat_traversal();
}


static ri_texture_t *
make_Lmap(ri_texture_t *longlatmap)
{
    int w, h;
    ri_texture_t *tex;

    w = longlatmap->width;
    h = longlatmap->height;
    
    tex = (ri_texture_t *)ri_mem_alloc(sizeof(ri_texture_t));
    tex->data = (float *)ri_mem_alloc(sizeof(float) * w * h * 4);
    tex->width  = w;
    tex->height = h;

    int i, j;
    int idx;
    double sinTheta;
    
    for (j = 0; j < h; j++) {
        sinTheta = sin(M_PI * j / (double)h);
        //sinTheta = 1.0;
        for (i = 0; i < w; i++) {
            idx = 4 * (j * w + i);
            tex->data[idx + 0] = sinTheta * longlatmap->data[idx + 0];
            tex->data[idx + 1] = sinTheta * longlatmap->data[idx + 1];
            tex->data[idx + 2] = sinTheta * longlatmap->data[idx + 2];
        }
    }

    return tex;
}

void
simple_render_ibl(
    ri_bvh_t *bvh,
    float    *image,
    int       width,
    int       height,
    vec       eye,
    vec       lookat,
    vec       up)
{
    int      i, j;
    float    s, t;
    int      u, v;

    vec      corner;
    vec      du; 
    vec      dv; 
    vec      dw; 

    vec      radiance;
    vec      radsum;
    vec      raydir;

    int                     hit;
    int                     debug;
    ri_ray_t                ray;
    ri_intersection_state_t state;
    ri_bvh_diag_t           diag;

    int nphisamples, nthetasamples;

    nphisamples   = (int)sqrt(giblsamples);
    nthetasamples = (int)sqrt(giblsamples);

    setup_camera(
        corner, du, dv, dw,
        eye, lookat, up,
        width, height, fov);

    ri_bvh_clear_stat_traversal();
    // MUST CALL
    ri_bvh_invalidate_cache( (void *)bvh );

    clear_ibl_stat();

    ri_texture_t            prodmap;        /* L x B x V    */
    ri_texture_t           *Lmap;

    Lmap = make_Lmap(glatlongmap);

    //ri_image_save_hdr("muda.hdr", Lmap->data, Lmap->width, Lmap->height);

    prodmap.width  = Lmap->width;
    prodmap.height = Lmap->height;
    prodmap.data   = (float *)ri_mem_alloc(sizeof(float) * prodmap.width * prodmap.height * 4);

    for (j = 0; j < height; j++) {
        printf("render line %d...\n", j);
        for (i = 0; i < width; i++) {

            debug = 0;
            if (gdebugpixel) {
                if ((gdebugpixel_x == i) && (gdebugpixel_y == j)) {
                    debug = 1;
                }
            }

            vzero(radsum);

            for (v = 0; v < gnsubsamples; v++) {
                for (u = 0; u < gnsubsamples; u++) {

                    s = (float)i + u / (float)(gnsubsamples);
                    t = (float)j + v / (float)(gnsubsamples);

                    get_eyedir(
                        raydir,
                        corner, du, dv,
                        s, t);
                        
                    memset( &state, 0, sizeof(ri_ray_t));
                    vcpy( ray.org, eye );
                    vcpy( ray.dir, raydir );

                    memset( &state, 0, sizeof(ri_intersection_state_t));

                    hit = ri_bvh_intersect( (void *)bvh, &ray, &state, &diag );

                    if (hit) {

                        //sample_ibl_naive(radiance, bvh, giblmap, &state, nthetasamples, nphisamples);
                        memset(prodmap.data, 0, sizeof(float) * 4 * prodmap.width * prodmap.height);
                        sample_ibl_beam(radiance, bvh, Lmap, &prodmap, &state, debug);
                        //exit(0);    // HACK

                    } else {

                        /*
                         * IBL is directly seen.
                         */
                        ri_texture_ibl_fetch(radiance,
                                             giblmap,
                                             ray.dir);

                        // HACK
                        //vscale(radiance, radiance, 00.0);

                    }

                    vadd(radsum, radsum, radiance);
                }
            }

            vscale(radsum, radsum, 1.0 / (double)(gnsubsamples * gnsubsamples));

            record_sample( image, width, height, (float)i, (float)j, radsum );

        }
    }

    ri_bvh_report_stat_traversal();

    report_ibl_stat();

    free(prodmap.data);
    free(Lmap->data);
    free(Lmap);
}

void
mybreak()
{
    printf("dbg\n");
}

void
render_beam_adaptive(
    ri_bvh_t *bvh,
    float    *image,
    int       width,
    int       height,
    int       beamsize,
    vec       eye,
    vec       corner,
    vec       du,
    vec       dv,
    float     s,
    float     t)
{

    int        i;
    int        u, v;

    vec        radiance;
    vec        beamdir[4];

    int                     stat;
    int                     invalid;
    ri_beam_t               beam;
    ri_bvh_diag_t           diag;


    get_beamdir(
        beamdir,
        corner, du, dv,
        (float)beamsize, s, t);

    for (i = 0; i < 4; i++) {
        // printf("beamdir[%d] = %f, %f, %f\n", i, beamdir[i][0], beamdir[i][1], beamdir[i][2]);
    }
                
    invalid = ri_beam_set( &beam, eye, beamdir );

    if (invalid) {
        radiance[0] = 1.0;
        radiance[1] = 0.0;
        radiance[2] = 0.0;
        for (v = 0; v < beamsize; v++) {
            for (u = 0; u < beamsize; u++) {
                record_sample( image, width, height, s + u, t + v, radiance );
            }
        }
        return;
    }            


    stat = ri_bvh_intersect_beam_visibility(
                (void *)bvh, &beam, &diag);

    printf("stat = %d\n", stat);

    if (stat == RI_BEAM_HIT_PARTIALLY) {

        /* subdiv */
        if (beamsize > 8) {

            for (v = 0; v < 2; v++) {
                for (u = 0; u < 2; u++) {

                    render_beam_adaptive(
                        bvh,
                        image,
                        width,
                        height,
                        beamsize / 2,
                        eye,
                        corner,
                        du, dv,
                        s + (beamsize / 2) * u, t + (beamsize / 2) * v);

                }
            }

            return;

        } else {

            radiance[0] = 0.5;
            radiance[1] = 0.5;
            radiance[2] = 0.5;

        }

    } else if (stat == RI_BEAM_HIT_COMPLETELY) {
        radiance[0] = 1.0;
        radiance[1] = 1.0;
        radiance[2] = 1.0;
    } else if (stat == -1) {
        radiance[0] = 0.0;
        radiance[1] = 1.0;
        radiance[2] = 1.0;
    } else { 
        radiance[0] = 0.0;
        radiance[1] = 0.0;
        radiance[2] = 1.0;
    }


    for (v = 0; v < beamsize; v++) {
        for (u = 0; u < beamsize; u++) {

            record_sample( image, width, height, s + u, t + v, radiance );

        }
    }

    // write border
    radiance[0] = 0.0;
    radiance[1] = 1.0;
    radiance[2] = 0.0;

    for (v = 0; v < beamsize; v++) {
        record_sample( image, width, height, s, t + v, radiance );
    }

    for (u = 0; u < beamsize; u++) {
        record_sample( image, width, height, s + u, t, radiance );
    }

}

void
simple_render_beam(
    ri_bvh_t *bvh,
    float    *image,
    int       width,
    int       height,
    int       beamsize,
    vec       eye,
    vec       lookat,
    vec       up)
{
    int        i, j;
    int        u, v;
    float      s, t;

    vec        corner;
    vec        du; 
    vec        dv; 
    vec        dw; 

    vec        radiance;
    vec        beamdir[4];
    vec        camera_frame[3];
    vec        raster_frame[3];
    vec        raster_corner;

    int                     hit;
    int                     stat;
    int                     invalid;
    ri_beam_t               beam;
    ri_raster_plane_t      *rasterplane;
    ri_bvh_diag_t           diag;


    const int               raster_width  = 128;
    const int               raster_height = 128;

    setup_camera(
        corner, du, dv, dw,
        eye, lookat, up,
        width, height, fov);

    rasterplane = ri_raster_plane_new();

    vcpy( camera_frame[0], du );
    vcpy( camera_frame[1], dv );
    vcpy( camera_frame[2], dw );

    ri_bvh_clear_stat_traversal();

    // MUST CALL
    ri_bvh_invalidate_cache( (void *)bvh );

    for (j = 0; j < height; j += beamsize) {
        for (i = 0; i < width; i += beamsize) {

            if (i == 0 && j == 0) {
                mybreak();
            }

            s = (float)i;
            t = (float)j;

            printf("render %d, %d...\n", i, j);

            vcpy(raster_frame[0], du);
            vcpy(raster_frame[1], dv);
            vcpy(raster_frame[2], dw);

            /* Shift corner position */
            vcpy(raster_corner, corner);
            raster_corner[0] = corner[0] + s * du[0] + t * dv[0];
            raster_corner[1] = corner[1] + s * du[1] + t * dv[1];
            raster_corner[2] = corner[2] + s * du[2] + t * dv[2];

            ri_raster_plane_setup( 
                rasterplane,
                raster_width,    
                raster_height,     
                camera_frame,
                raster_corner,
                eye,
                fov);

            render_beam_adaptive(
                bvh,
                image,
                width,
                height,
                beamsize,
                eye,
                corner,
                du, dv,
                s, t);

#if 0
            get_beamdir(
                beamdir,
                corner, du, dv,
                (float)beamsize, s, t);
                
            invalid = ri_beam_set( &beam, eye, beamdir );
            printf("invalid = %d\n", invalid);

            if (invalid) {
                radiance[0] = 1.0;
                radiance[1] = 0.0;
                radiance[2] = 0.0;
                for (v = 0; v < beamsize; v++) {
                    for (u = 0; u < beamsize; u++) {
                        record_sample( image, width, height, s + u, t + v, radiance );
                    }
                }
                continue;
            }            



            stat = ri_bvh_intersect_beam_visibility(
                        (void *)bvh, &beam, &diag);

            printf("stat(%d, %d) = %d\n", i, j, stat);

            if (stat == RI_BEAM_HIT_COMPLETELY) {
                radiance[0] = 1.0;
                radiance[1] = 1.0;
                radiance[2] = 1.0;
            } else if (stat == RI_BEAM_HIT_PARTIALLY) {
                radiance[0] = 0.5;
                radiance[1] = 0.5;
                radiance[2] = 0.5;
            } else {
                radiance[0] = 0.0;
                radiance[1] = 0.0;
                radiance[2] = 1.0;
            }


            for (v = 0; v < beamsize; v++) {
                for (u = 0; u < beamsize; u++) {

                    record_sample( image, width, height, s + u, t + v, radiance );

                }
            }

            // write border
            radiance[0] = 0.0;
            radiance[1] = 1.0;
            radiance[2] = 0.0;
            for (v = 0; v < beamsize; v++) {

                record_sample( image, width, height, s, t + v, radiance );

            }
            for (u = 0; u < beamsize; u++) {

                record_sample( image, width, height, s + u, t, radiance );

            }
#endif

        }
    }

    ri_bvh_report_stat_traversal();

    ri_raster_plane_free(rasterplane); 
}

