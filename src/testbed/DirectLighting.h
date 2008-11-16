#ifndef TESTBED_DIRECTLIGHTING_H
#define TESTBED_DIRECTLIGHTING_H

#ifdef __cplusplus
extern "C" {
#endif

#include "texture.h"
#include "intersection_state.h"
#include "bvh.h"

//
// Stats
//
extern uint64_t ndirect_light_visible;
extern uint64_t ndirect_ligjt_samples;

// Contribution from distant ligh
extern void sample_distant_light(
    ri_vector_t                      Lo,                /* [out]            */
    ri_bvh_t                        *bvh,
    const ri_intersection_state_t   *isect,
    ri_vector_t                      Ldir,              /* Normalized       */
    ri_vector_t                      Lcol,
    int                              debug);

#ifdef __cplusplus
}
#endif

#endif  // TESTBED_DIRECTLIGHTING_H
