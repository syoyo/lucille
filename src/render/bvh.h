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


#define RI_BVH_ENABLE_DIAGNOSTICS
#define RI_BVH_TRACE_STATISTICS

#define BMIN_X0    (0      )
#define BMIN_X1    (1      )
#define BMIN_X2    (2      )
#define BMIN_X3    (3      )
#define BMIN_Y0    (4      )
#define BMIN_Y1    (5      )
#define BMIN_Y2    (6      )
#define BMIN_Y3    (7      )
#define BMIN_Z0    (8      )
#define BMIN_Z1    (9      )
#define BMIN_Z2    (10     )
#define BMIN_Z3    (11     )
#define BMAX_X0    (12 + 0 )
#define BMAX_X1    (12 + 1 )
#define BMAX_X2    (12 + 2 )
#define BMAX_X3    (12 + 3 )
#define BMAX_Y0    (12 + 4 )
#define BMAX_Y1    (12 + 5 )
#define BMAX_Y2    (12 + 6 )
#define BMAX_Y3    (12 + 7 )
#define BMAX_Z0    (12 + 8 )
#define BMAX_Z1    (12 + 9 )
#define BMAX_Z2    (12 + 10)
#define BMAX_Z3    (12 + 11)


/*
 * Struct: ri_qbvh_node_t
 *
 *     Structure for QBVH(Quad BVH) node
 */
typedef struct _ri_qbvh_node_t {

    /* layout =  bmin:xxxx, yyyy, zzzz, bmax:xxxx, yyyy, zzzz */

    float                   bbox[4 * 3 * 2];
    struct _ri_qbvh_node_t *child[4];         /* ptr to child node    */
    
    int                     axis0, axis1, axis2;
    int                     is_leaf;
    
} ri_qbvh_node_t;

/*
 * Diagnositics for each ri_bvh_intersect query.
 * This is debug purpose. 
 */
typedef struct _ri_bvh_diag_t
{

    uint32_t ninner_node_traversals;
    uint32_t nleaf_node_traversals;
    uint32_t ntriangle_isects;

} ri_bvh_diag_t;


/*
 * Struct: ri_bvh_stat_traversal_t
 *
 *     Structure of statistics for BVH traversal phase.
 *
 */
typedef struct _ri_bvh_stat_traversal_t {

    uint64_t ntraversals;
    uint64_t ntested_triangles;
    uint64_t nactually_hit_triangles;
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
                                       ri_intersection_state_t *state_out,
                                       void                    *user);

#ifdef __cplusplus
}       /* extern "C" */
#endif

#endif  /* LUCILLE_BVH_H */
