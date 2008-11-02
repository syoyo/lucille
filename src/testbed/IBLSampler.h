#ifndef TESTBED_IBL_H
#define TESTBED_IBL_H

#ifdef __cplusplus
extern "C" {
#endif

#include "texture.h"
#include "intersection_state.h"

extern void sample_ibl(
    const ri_texture_t              *iblmap,
    const ri_intersection_state_t   *isect);


#ifdef __cplusplus
}
#endif

#endif  // TESTBED_IBL_H
