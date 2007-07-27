/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

//
// SIMD(SSE) optimized Bounding Volume Hierarcies
//

#ifndef LUCILLE_BVH_H
#define LUCILLE_BVH_H

#include "vector.h"
#include "ray.h"
#include "intersection_state.h"


#ifdef __cplusplus
extern "C" {
#endif

#define BVH_NODE_LEAF 0x3U

#if defined ( __GNUC__ )
#define BVH_DECL_ALIGN( x ) 
#define BVH_ATTRIB_ALIGN( x ) __attribute__((aligned((x))))
#define BVH_PAD( x )          uint8_t pad[0] __attribute__((aligned((x))))
#else
#define BVH_DECL_ALIGN( x )  __declspec(align(x))
#define BVH_ALIGN( x )
#define BVH_PAD( x )
#endif

#define BVH_NODE_FLAG_BITS                   (2)
#define BVH_NODE_FLAG_BIT_MASK               (0x3U)
#define BVH_NODE_FLAG_DATA0_MASK             (~0x3U)
#define BVH_NODE_FLAG_DATA_64_MASK           (~0x3ULL)

/*
 * Struct: ri_bvh_node_t
 *
 *     Structure for BVH node.
 */
typedef struct BVH_DECL_ALIGN(32) _ri_bvh_node_t {

	ri_float_t bmin[3];
	union {
		uintptr_t child_node_ptr; // (for inner node)
		uintptr_t triangle_ptr;   // (for leaf node)
	};

#if defined(__64bit__) && !defined(ENABLE_DOUBLE_PRECISION)
	uint32_t   bmin_pad[3];
#endif 

#if !defined(__64bit__) && defined(ENABLE_DOUBLE_PRECISION)
	uint32_t   bmin_pad[1];
#endif 

	// 16 bytes or 32bytes up to here.

	ri_float_t bmax[3];
	uint16_t   ntriangles;
	uint8_t    axis;
	uint8_t    flag;	// not used for now

#if defined(ENABLE_DOUBLE_PRECISION)
	uint32_t   tmp;
#endif

	// 32 bytes or 64bytes up to here.

	BVH_PAD(32);

} ri_bvh_node_t BVH_ATTRIB_ALIGN ( 32 );

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
typedef struct BVH_DECL_ALIGN(32) _ri_bvh_t {

	//
	// Scene bounding box
	//
	ri_vector_t                 bmin;
	ri_vector_t                 bmax;

	//
	// Ptr to root node
	//
	ri_bvh_node_t              *root;

	//
	// Statistics
	//
	ri_bvh_stat_traversal_t     stat_traversal;
	ri_bvh_stat_construction_t  stat_construction;


	BVH_PAD(32);

} ri_bvh_t BVH_ATTRIB_ALIGN(32);

extern ri_bvh_node_t *ri_bvh_node_new();
extern void           ri_bvh_stat_traversal_clear(
				ri_bvh_stat_traversal_t  *stat);

/*
 * Implementation of ri_accel_t interface
 */
extern void          *ri_bvh_build    ();
extern void           ri_bvh_free     (void                    *arg);
extern int            ri_bvh_intersect(void                    *accel,
                                       ri_ray_t                *ray,
                                       ri_intersection_state_t *state);

#ifdef __cplusplus
}       /* extern "C" */
#endif

#endif  /* LUCILLE_BVH_H */
