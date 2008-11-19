/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Bounding Volume Hierarchy.
 *
 * $Id$
 *
 */

//
// TODO: SIMD(SSE) optimizatio for Bounding Volume Hierarcies construction and
//       traversal.
//

#ifndef LUCILLE_BVH_H
#define LUCILLE_BVH_H

#include "vector.h"
#include "ray.h"
#include "beam.h"
#include "raster.h"
#include "intersection_state.h"


#ifdef __cplusplus
extern "C" {
#endif


/*
 * Flags for (Visual) debugging.
 */
#define RI_BVH_ENABLE_DIAGNOSTICS
#define RI_BVH_TRACE_STATISTICS
#define RI_BVH_TRACE_BEAM_STATISTICS

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
 *   Structure for QBVH(Quad BVH) node
 */
typedef struct _ri_qbvh_node_t {

    /*
     * bbox's memory layout.
     *
     *  bmin                 bmax
     *
     *   0      4      8     12     16     20     24 
     * +------+------+------+------+------+------+
     * | xxxx | yyyy | zzzz | xxxx | yyyy | zzzz |
     * +------+------+------+------+------+------+
     */

    ri_float_t              bbox[4 * 3 * 2];
    struct _ri_qbvh_node_t *child[4];         /* ptr to child node    
                                               * TODO: make this offset
                                               */
    
    int                     axis0, axis1, axis2;
    int                     is_leaf;
    
} ri_qbvh_node_t;   /* 128 bytes. */

/*
 * Struct: ri_bvh_diag_t
 *
 *   Diagnositics for each ri_bvh_intersect query.
 *   This structure is used for visual debugging purpose when
 *   RI_BVH_ENABLE_DIAGNOSTICS is on.
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
 *   Structure of statistics for BVH traversal phase.
 *
 */
typedef struct _ri_bvh_stat_traversal_t {

    uint64_t ninner_node_traversals;
    uint64_t nleaf_node_traversals;
    uint64_t ntested_triangles;
    uint64_t nactually_hit_triangles;
    uint64_t nfailed_isects;
    uint64_t nrays;

    uint64_t nbeams;

} ri_bvh_stat_traversal_t;

/*
 * Struct: ri_bvh_stat_construction_t
 *
 *   Structure of statistics for BVH construction phase.
 *
 */
typedef struct _ri_bvh_stat_construction_t {

    uint64_t ninner_nodes;
    uint64_t nleaf_nodes;
    uint64_t naverage_triangels_per_leaf;

} ri_bvh_stat_construction_t;

/*
 * Struct: ri_bvh_stat_beam_traversal_t
 *
 *   Structure of statistics for beam-BVH traversal phase.
 *
 */
typedef struct _ri_bvh_stat_beam_traversal_t {

    uint64_t noverdraws;
    uint64_t nrasterpixels;

    uint64_t nbeams;

} ri_bvh_stat_beam_traversal_t;

/*
 * Struct: ri_bvh_t
 *
 *   Structure for BVH.
 */
typedef struct _ri_bvh_t {

    /* If the bvh is build for empty scene input, empty flag is set
     * to distinguish null pointer which stands for a bvh is not constructed
     * yet.
     */
    int                         empty;

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


} ri_bvh_t;

/*
 * Implementation of ri_accel_t interface
 */
extern void *ri_bvh_build         (const void                    *data);
extern void  ri_bvh_free          (      void                    *arg);
extern void  ri_bvh_invalidate_cache
                                  (      void                    *data);
extern int   ri_bvh_intersect     (      void                    *accel,
                                         ri_ray_t                *ray,
                                         ri_intersection_state_t *state_out,
                                         void                    *user);

extern int   ri_bvh_intersect_beam(      void                    *accel,
                                         ri_beam_t               *beam,
                                         ri_raster_plane_t       *raster_out,
                                         void                    *user);

/*
 * Just query whether the beam intersects some obstacles in the scene.
 *
 * Return value:
 *
 *  RI_BEAM_MISS_COMPLETELY
 *  RI_BEAM_HIT_COMPLETELY
 *  RI_BEAM_HIT_PARTIALLY
 *  (Values are defined in beam.h)
 */
extern int   ri_bvh_intersect_beam_visibility(
                                         void                    *accel,
                                         ri_beam_t               *beam,
                                         void                    *user);

/*
 * Debug
 */
extern void  ri_bvh_clear_stat_traversal();
extern void  ri_bvh_report_stat_traversal();

#ifdef __cplusplus
}       /* extern "C" */
#endif

#endif  /* LUCILLE_BVH_H */
