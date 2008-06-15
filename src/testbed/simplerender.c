#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bvh.h"

#include "simplerender.h"

static void
setup_camera(
    vec  corner,        /* [out] */
    vec  du,            /* [out] */
    vec  dv,            /* [out] */
    vec  eye,
    vec  lookat,
    vec  up,
    int  width,
    int  height)
{
    vec look;
    float fov = 45.0f;
    float flen = 0.5 * width / tan(0.5 * (fov * M_PI / 180.0));

    /* look = lookat - eye */
    vsub( look, lookat, eye );

    vcross( du, look, up );
    vnormalize( du );

    vcross( dv, look, du );
    vnormalize( dv );

    vnormalize( look );

    /* corner = flen * look - (width * du + height * dv) / 2 */

    corner[0] = flen * look[0] - 0.5 * (width * du[0] + height * dv[0]);
    corner[1] = flen * look[1] - 0.5 * (width * du[1] + height * dv[1]);
    corner[2] = flen * look[2] - 0.5 * (width * du[2] + height * dv[2]);

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
    int   i, j;
    float    s, t;

    vec      corner;
    vec      du; 
    vec      dv; 

    vec      radiance;
    vec      raydir;

    int                     hit;
    ri_ray_t                ray;
    ri_intersection_state_t state;
    ri_bvh_diag_t           diag;

    setup_camera(
        corner, du, dv,
        eye, lookat, up,
        width, height);

    ri_bvh_clear_stat_traversal();

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

                radiance[0] = state.t;
                radiance[1] = state.t;
                radiance[2] = state.t;

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


