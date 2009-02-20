#include <stdio.h>
#include <math.h>

#include "render.h"

#define vdot(a, b) ((a).x * (b).x + (a).y * (b).y + (a).z * (b).z)

sphere_t scene_spheres[3];



static void vnormalize(vec *v)
{
    float d = vdot(*v, *v);
    float invd;
    
    if (d > 1.0e-6f) {
        invd = 1.0f / sqrtf(d);
        v->x *= invd;
        v->y *= invd;
        v->z *= invd;
    }
}

int
ray_sphere_intersect(isect_t *isect, const ray_t *ray, const sphere_t *sphere)
{
    int   hit;
    vec   rs;

    rs.x = ray->org.x - sphere->center.x;
    rs.y = ray->org.y - sphere->center.y;
    rs.z = ray->org.z - sphere->center.z;

    float B = vdot(rs, ray->dir);
    float C = vdot(rs, rs) - sphere->radius * sphere->radius;
    float D = B * B - C;
    float t;

    hit = 0;

    if (D > 0.0f) {

        t = -B - sqrtf(D);

        if ( (t > 0.0f) && (t < isect->t) ) {
            hit = 1;
            isect->t = t;

            isect->p.x = ray->org.x + t * ray->dir.x;
            isect->p.y = ray->org.y + t * ray->dir.y;
            isect->p.z = ray->org.z + t * ray->dir.z;

            isect->n.x = isect->p.x - sphere->center.x;
            isect->n.y = isect->p.y - sphere->center.y;
            isect->n.z = isect->p.z - sphere->center.z;

            vnormalize(&isect->n);
        }
 
    }

    return hit;

}

void
init_render_scene()
{
    scene_spheres[0].center.x =  0.0;
    scene_spheres[0].center.y =  0.0;
    scene_spheres[0].center.z = -1.25;
    scene_spheres[0].radius   =  0.75;

    scene_spheres[1].center.x =  0.0;
    scene_spheres[1].center.y = -40-0.75;
    scene_spheres[1].center.z = -2.0;
    scene_spheres[1].radius   =  40.0;

    scene_spheres[2].center.x =  1.0;
    scene_spheres[2].center.y =  0.0;
    scene_spheres[2].center.z = -2.2;
    scene_spheres[2].radius = 0.5;
}
