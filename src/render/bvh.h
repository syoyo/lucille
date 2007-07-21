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

	/*
	 * 64                 32            2  0
	 *  +------------------+------------+--+
       	 *  |      data1       |   data0    |  |
	 *  +------------------+------------+--+
         *         |                  |       |  
	 *         |                  |       +-> flag(2 bits)
	 *         |                  |
	 *         |                  +-> ptr to triangle data(30 bits).
	 *         |                      the address pointed is aligned to
	 *         |                      16-bytes boundary or more
	 *         |                      (at least, lower 4 bits are zeros),
	 *         |                      so we can safely include flag bits
	 *         |                      into it's lower 2 bits.
         *         |                     
       	 *         +--------------------> used when 64-bit env and
	 *                                sizeof(ri_float) = sizeof(float) = 4.
	 */

	/*
	 * (bmin[0], bmin[1], bmin[2], data0)
	 */
	ri_vector_t bmin_and_data0;

	/*
	 * (bmax[0], bmax[1], bmax[2], data1)
	 */
	ri_vector_t bmax_and_data1;

} ri_bvh_node_t BVH_ATTRIB_ALIGN ( 32 );

static inline uint32_t bvh_node_get_flag(const ri_bvh_node_t *node)
{
	union {
		uint32_t   i;
		ri_float_t f;  
	} d;

	d.f = node->bmin_and_data0.f[3];

	return d.i;
}

static inline void bvh_node_set_flag(ri_bvh_node_t *node, uint32_t flag)
{
	union {
		uint64_t   ui64;
		uint32_t   ui32;
		ri_float_t f;  
	} d;

	d.f = node->bmin_and_data0.f[3];

	d.ui32 = ((d.ui32) & BVH_NODE_FLAG_DATA0_MASK) | flag;

	node->bmin_and_data0.f[3] = d.f;

}

static inline void *bvh_node_get_data(const ri_bvh_node_t *node)
{
	union {
		uint64_t    ui64;
		uint32_t    ui32;
		ri_float_t  f;  
	} d0;

	d0.f = node->bmin_and_data0.f[3];

#if defined(ENABLE_DOULBE_PRECISION)

#if defined(__X86_64__)

	return (void *)(d0.ui64 >> 2);

#else	/* !__X86_64__ */

	return (void *)(d0.ui32 >> BVH_NODE_FLAG_BIT_MASK);

#endif	/* __X86_64__ */

#else	/* !ENABLE_DOULBE_PRECISION */

#if defined(__X86_64__)

	union {
		uint64_t    ui64;
		uint32_t    ui32;
		ri_float_t  f;  
	} d1;

	uint64_t data64;

	d1.f = node->bmin_and_data0.f[3];

	data64 = (d1.ui64 << 32ULL) | d0.ui64;
	data64 = data64 & BVH_NODE_FLAG_DATA_64_MASK;

	return (void *)data64;

#else	/* !__X86_64__ */

	return (void *)d0.ui32;

#endif	/* __X86_64__ */

#endif	/* ENABLE_DOULBE_PRECISION */

}

static inline void bvh_node_set_data(ri_bvh_node_t *node, void *data)
{
	union {
		uint64_t   ui64;
		uint32_t   ui32;
		ri_float_t f;  
	} d0;


	d0.f = node->bmin_and_data0.f[3];

#if defined(ENABLE_DOULBE_PRECISION)

#if defined(__X86_64__)

	
	d0.ui64 |= (uint64_t)data & BVH_NODE_FLAG_DATA_64_MASK;

	node->bmin_and_data0.f[3] = d0.f;

#else	/* !__X86_64__ */

	d0.ui32 |= (uint32_t)data & BVH_NODE_FLAG_DATA0_MASK;

	node->bmin_and_data0.f[3] = d0.f;

#endif	/* __X86_64__ */

#else	/* !ENABLE_DOULBE_PRECISION */

#if defined(__X86_64__)

	union {
		uint64_t    ui64;
		uint32_t    ui32;
		ri_float_t  f;  
	} d1;

	uint32_t low, hi;

	hi = (uint64_t)data >> 32ULL;
	low = (uint32_t)(uintptr_t)data;

	d1.ui32 = hi;
	d0.ui32 |= low & BVH_NODE_FLAG_DATA0_MASK;

	node->bmin_and_data0.f[3] = d0.f;
	node->bmin_and_data0.f[3] = d1.f;

#else	/* !__X86_64__ */

	d0.ui32 |= (uint32_t)data & BVH_NODE_FLAG_DATA0_MASK;

	node->bmin_and_data0.f[3] = d0.f;

#endif	/* __X86_64__ */

#endif	/* ENABLE_DOULBE_PRECISION */

}

/*
 * Struct: ri_bvh_node_t
 *
 *     Structure for BVH tree.
 */
typedef struct _ri_bvh_t {

	/* Scene bounding box */

	/*
	 * (bmin[0], bmin[1], bmin[2], root0) 
	 * (bmax[0], bmax[1], bmax[2], root1) 
	 *
	 * In 64bit-env and ri_float_t == float case,
	 *
	 * root = (root0 << 32) | root
	 */
	ri_vector_t    bmin;
	ri_vector_t    bmax;

	//ri_bvh_node_t *root;
} ri_bvh_t;


static inline ri_bvh_node_t *bvh_get_root(ri_bvh_t *bvh)
{
#if !defined(ENABLE_DOULBE_PRECISION) && defined(__X86_64__)

	union {
		uint32_t ui;
		float    f;  
	} d0, d1;

	uint32_t high, low;

	d0.f = bvh->bmin.f[3];
	d1.f = bvh->bmax.f[3];

	high = d0.ui;
	low  = d1.ui;

	uint64_t data;

	data = ((uint64_t)high << 32ULL) | (uint64_t)low;

	return (void *)data;

#else	/* ENABLE_DOULBE_PRECISION || !__X86_64__ */

	return (ri_bvh_node_t *)&(bvh->bmin.f[3]);

#endif	
	
}


#ifdef __cplusplus
}       /* extern "C" */
#endif

#endif  /* LUCILLE_BVH_H */
