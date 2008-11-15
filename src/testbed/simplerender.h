#ifndef SIMPLERENDER_H
#define SIMPLERENDER_H

#include "bvh.h"

extern void
simple_render(  ri_bvh_t *bvh,
                float *img, int width, int height,
                vec eye, vec lookat, vec up );

extern void
simple_render_ibl(
    ri_bvh_t *bvh,
    float    *image,
    int       width,
    int       height,
    vec       eye,
    vec       lookat,
    vec       up);

extern void
simple_render_beam(
                ri_bvh_t *bvh,
                float *img, int width, int height,
                int beamsize,
                vec eye, vec lookat, vec up );

extern void
simple_render_progressive(
    ri_bvh_t *bvh,
    float *img, int width, int height,
    vec eye, vec lookat, vec up,
    int nsamples );

#endif  /* SIMPLERENDER_H */
