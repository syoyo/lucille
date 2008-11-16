#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "DirectLighting.h"

void
sample_distant_light(
    ri_vector_t                      Lo,                /* [out]            */
    ri_bvh_t                        *bvh,
    const ri_intersection_state_t   *isect,
    ri_vector_t                      Ldir,              /* normalized       */
    ri_vector_t                      Lcol,
    int                              debug)
{
    (void)debug;

    int                         hit;
    ri_ray_t                    ray;
    ri_intersection_state_t     state;
    ri_float_t                  dot; 

    vcpy(ray.org, isect->P);
    ray.org[0] += isect->Ns[0] * 0.00001;
    ray.org[1] += isect->Ns[1] * 0.00001;
    ray.org[2] += isect->Ns[2] * 0.00001;

    vcpy(ray.dir, Ldir);

    hit = ri_bvh_intersect( (void *)bvh, &ray, &state, NULL );

    if (hit) {
        /* There's obscrances between the shading point and the light */
        vzero(Lo);
    } else {
        /* Lo = L cosTheta */
        dot = vdot(Ldir, isect->Ns);
        if (dot < 0.0) dot = 0.0;

        Lo[0] = Lcol[0] * dot;
        Lo[1] = Lcol[1] * dot;
        Lo[2] = Lcol[2] * dot;
    }

}
