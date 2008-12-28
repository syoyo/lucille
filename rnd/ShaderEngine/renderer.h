#ifndef RENDERER_H
#define RENDERER_H

#include "vec.h"

#ifdef __cplusplus
extern "C" {
#endif

extern void
render(
    float    *image,
    int       width,
    int       height,
    vec       eye,
    vec       lookat,
    vec       up);


#ifdef __cplusplus
}
#endif

#endif  /* RENDERER_H */
