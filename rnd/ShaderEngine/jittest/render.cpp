#include <stdio.h>
#include <math.h>

#include "render.h"

#define vdot(a, b) (a[0] * b[0] + a[1] * b[1] + a[2] * b[2])

static void vnormalize(float *v)
{
    float d = vdot(v, v);
    float invd;
    
    if (d > 1.0e-6f) {
        invd = 1.0f / sqrt(d);
        v[0] *= invd;
        v[1] *= invd;
        v[2] *= invd;
    }
}

int
sphere_isect(isect_t *isect, const sphere_t *sphere, float *org, float *dir)
{
    int   hit;
    float rs[3];

    rs[0] = org[0] - sphere->center[0];
    rs[1] = org[1] - sphere->center[1];
    rs[2] = org[2] - sphere->center[2];

    float B = vdot(rs, dir);
    float C = vdot(rs, rs) - sphere->radius * sphere->radius;
    float D = B * B - C;
    float t;

    hit = 0;

    if (D > 0.0f) {

        t = -B - sqrt(D);

        if ( (t > 0.0) && (t < isect->t) ) {
            hit = 1;
            isect->t = t;

            isect->p[0] = org[0] + t * dir[0];
            isect->p[1] = org[1] + t * dir[1];
            isect->p[2] = org[2] + t * dir[2];

            isect->n[0] = isect->p[0] - sphere->center[0];
            isect->n[1] = isect->p[1] - sphere->center[1];
            isect->n[2] = isect->p[2] - sphere->center[2];

            vnormalize(isect->n);
        }
 
    }

    return hit;

}
