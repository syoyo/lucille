#ifndef SIMPLERENDER_H
#define SIMPLERENDER_H

#include "bvh.h"

void
simple_render( ri_bvh_t *bvh,
               float *img, int width, int height,
               vec eye, vec lookat, vec up );

#endif  /* SIMPLERENDER_H */
