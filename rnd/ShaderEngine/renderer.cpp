#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>

#include "renderer.h"

/*
 * Scene
 */
float g_sphere_radius;
vec   g_sphere_center;

float fov = 45.0;

typedef struct _ray_t
{
    vec org;
    vec dir;
} ray_t;

static void
init_scene()
{
    g_sphere_center[0] = 0.0f;
    g_sphere_center[1] = 0.0f;
    g_sphere_center[2] = 20.0f;

    g_sphere_radius = 5.0f;
}


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
    float      fov)
{
    float flen = 0.5 * width / tan(0.5 * (fov * M_PI / 180.0));

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

    printf("corner = %f, %f, %f\n", corner[0], corner[1], corner[2]);

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

    vnormalize(raydir);
    

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

int 
intersect_ray_sphere(
    float *t,
    vec    ray_org,
    vec    ray_dir,         /* dir should be normalized. */
    vec    sphere_center,
    float  sphere_radius)
{

    vec   rs;
    float r2;
    float B;
    float C;
    float D;

    vsub(rs, ray_org, sphere_center);

    r2 = sphere_radius * sphere_radius;
    B  = vdot(rs, ray_dir);
    C  = vdot(rs, rs) - r2;
    D  = B * B - C;

    if (D > 0.0f) {

        (*t) = -B - sqrtf(D);
        return 1;

    } else {

        (*t) = FLT_MAX;
        return 0;

    }    

}
    
    

void
render(
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

    int      hit;
    ray_t    ray;

    float    hitt;

    init_scene();

    setup_camera(
        corner, du, dv, dw,
        eye, lookat, up,
        width, height, fov);

    printf("[render] eye  = %f, %f, %f\n", eye[0], eye[1], eye[2]);
    printf("[render] look = %f, %f, %f\n", lookat[0], lookat[1], lookat[2]);

    for (j = 0; j < height; j++) {
        for (i = 0; i < width; i++) {

            s = (float)i;
            t = (float)j;

            radiance[0] = 0.0f;
            radiance[1] = 0.0f;
            radiance[2] = 0.0f;

            get_eyedir(
                raydir,
                corner, du, dv,
                s, t);
                
            vcpy( ray.org, eye );
            vcpy( ray.dir, raydir );

            hitt = 0.0f;
            hit = intersect_ray_sphere(
                &hitt,
                ray.org,
                ray.dir,
                g_sphere_center,
                g_sphere_radius);

            if (hit) {

                radiance[0] = 0.0f;
                radiance[1] = 0.0f;
                radiance[2] = 1.0f;

            } else {

                radiance[0] = 0.0f;
                radiance[1] = 0.0f;
                radiance[2] = 0.0f;
            }

            record_sample( image, width, height, s, t, radiance );

        }
    }
}


