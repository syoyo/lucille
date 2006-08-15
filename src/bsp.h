/*
 * BSP routine.
 *
 * $Id: bsp.h,v 1.6 2004/06/13 06:44:51 syoyo Exp $
 */

#ifndef BSP_H
#define BSP_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vector.h"
#include "array.h"
#include "list.h"
#include "geom.h"
#include "accel.h"

#if 0
typedef struct _bbox_t {
	ri_vector_t min;
	ri_vector_t max;
} bbox_t;
#endif

typedef struct _ri_triinfo_t {
	ri_vector_t    vlist[3];
	unsigned int   index;
	ri_geom_t     *geom;
} ri_triinfo_t;

typedef struct _trilist_t {
	ri_vector_t        *vlist;	/* triangle vertices */
	unsigned int       *indices;	/* array of indices to triangle list
					 * in geoms[i] */
	ri_geom_t          **geoms;	/* array of pointers to geometry
					 * which contains triangle */
		
	int ntris;			/* number of triangles */
	int nalloced;			/* number of allocated memory */

	/* SIMD triangle data */
	ri_simd_tri_info_t *simdtris;	/* SIMD friendly triangle info.  */
} trilist_t;

typedef struct _binnode_t
{
	int depth;
	ri_vector_t min, max;			/* extent of node	*/
	trilist_t trilists;		/* triangle lists this node contains */
	
	struct _binnode_t *child[2];	/* pointers to children nodes	*/
	
	/* distance to the plane which subdivides the children	*/
	float (*dist)(const ri_vector_t *plane, const ri_vector_t *org, const ri_vector_t *dir);

	/* children near/far ordering relative to a input point */
	void (*getchildren)(struct _binnode_t *node,
			    struct _binnode_t **nearnode,
			    struct _binnode_t **farnode,
			    const ri_vector_t *org);
} binnode_t;

typedef struct _ri_bsp_t {
	ri_vector_t min, max;
	trilist_t trilists;

	int    maxdepth;
	int    maxlistlen;
	
	binnode_t *root;		/* root of the tree */
} ri_bsp_t;

extern ri_bsp_t *ri_accel_build_bsp();
extern void	 ri_bsp_free (ri_bsp_t *bsp);
//extern int       ri_bsp_intersect(ri_bsp_t *tree,
//			  const ri_vector_t *org,
//				  const ri_vector_t *dir,
//				  ri_triinfo_t *tri,
//				  float *t, float *u, float *v);
extern void      ri_bsp_statistics();

#ifdef __cplusplus
}	/* extern "C" */
#endif


#endif
