#ifndef SIMPLERENDER_H
#define SIMPLERENDER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "bvh.h"

void
simple_render( ri_bvh_t *bvh,
               float *img, int width, int height,
               vec eye, vec lookat, vec up );

#ifdef __cplusplus
}
#endif

#endif  /* SIMPLERENDER_H */
