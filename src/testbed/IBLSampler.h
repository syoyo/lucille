#ifndef TESTBED_IBL_H
#define TESTBED_IBL_H

#ifdef __cplusplus
extern "C" {
#endif

#include "texture.h"
#include "intersection_state.h"
#include "bvh.h"

//
// Stats
//
extern uint64_t ncompletely_visible  ;
extern uint64_t npartially_visible   ;
extern uint64_t ncompletely_invisible;

extern void sample_ibl_beam(
    ri_vector_t                      Lo,                /* [out]            */
    ri_bvh_t                        *bvh,
    const ri_texture_t              *Lmap,
    ri_texture_t                    *prodmap,           /* [buffer]         */
    const ri_intersection_state_t   *isect);

extern void sample_ibl_naive(
    ri_vector_t                      Lo,                /* [out]            */
    ri_bvh_t                        *bvh,
    const ri_texture_t              *iblmap,
    const ri_intersection_state_t   *isect,
    uint32_t                         ntheta_samples,
    uint32_t                         nphi_samples);

extern void clear_ibl_stat();
extern void report_ibl_stat();

#ifdef __cplusplus
}
#endif

#endif  // TESTBED_IBL_H
