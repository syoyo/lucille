/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

//
// To be SIMD(SSE) optimized Bounding Volume Hierarcies
//

#ifndef LUCILLE_BVH_H
#define LUCILLE_BVH_H

#include "vector.h"
#include "ray.h"
#include "intersection_state.h"


#ifdef __cplusplus
extern "C" {
#endif

/*
 * Struct: ri_qbvh_node_t
 *
 *     Structure for QBVH(Quad BVH) node
 */
typedef struct _ri_qbvh_node_t {
    float                   bbox[4 * 3 * 2];
    struct _ri_qbvh_node_t *child[4];         /* ptr to child node    */
    
    int                     is_leaf;
    
} ri_qbvh_node_t;


/*
 * Struct: ri_bvh_stat_traversal_t
 *
 *     Structure of statistics for BVH traversal phase.
 *
 */
typedef struct _ri_bvh_stat_traversal_t {

    uint64_t ntraversals;
    uint64_t ntested_tris;
    uint64_t nfailed_isects;

} ri_bvh_stat_traversal_t;

/*
 * Struct: ri_bvh_stat_construction_t
 *
 *     Structure of statistics for BVH construction phase.
 *
 */
typedef struct _ri_bvh_stat_construction_t {

    uint64_t ninner_nodes;
    uint64_t nleaf_nodes;
    uint64_t naverage_triangels_per_leaf;

} ri_bvh_stat_construction_t;

/*
 * Struct: ri_bvh_t
 *
 *     Structure for BVH.
 */
typedef struct _ri_bvh_t {

    /*
     * Scene bounding box
     */
    ri_vector_t                 bmin;
    ri_vector_t                 bmax;

    ri_qbvh_node_t              *root;

    /*
     * Statistics
     */ 
    ri_bvh_stat_traversal_t     stat_traversal;
    ri_bvh_stat_construction_t  stat_construction;


    // BVH_PAD(32);

} ri_bvh_t;

extern ri_qbvh_node_t *ri_qbvh_node_new();
extern void            ri_bvh_stat_traversal_clear(
                                       ri_bvh_stat_traversal_t  *stat);

/*
 * Implementation of ri_accel_t interface
 */
extern void          *ri_bvh_build    (const void              *data);
extern void           ri_bvh_free     (void                    *arg);
extern int            ri_bvh_intersect(void                    *accel,
                                       ri_ray_t                *ray,
                                       ri_intersection_state_t *state);

#ifdef __cplusplus
}       /* extern "C" */
#endif

#endif  /* LUCILLE_BVH_H */
