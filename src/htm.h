/*
 * Hierarchical Triangular Mesh routine used in prt.c.
 *
 * see:
 * "Hierarchical Triangular Mesh"
 * http://www.sdss.jhu.edu/htm/
 *
 * $Id: htm.h,v 1.1.1.1 2004/01/06 13:57:08 syoyo Exp $
 */

#ifndef HTM_H
#define HTM_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vector.h"

/* hierarchical triangle mesh node for accelerated point location and
 * visibility test.
 */
typedef struct _ri_htm_node_t
{
	int index;
	int v[3];		/* vertex index				*/
	int w[3];		/* midpoint vertex index		*/
	int child[4];		/* IDs of children node			*/
	int parent;		/* ID of parent node			*/

	int p[8];		/* sample points this node contains	*/
	int npoints;		/* number of sample points this node
				 * contains(shold be less than 8)	*/
	int id;
} ri_htm_node_t;

typedef struct _ri_htm_occlusion_node_t
{
#if 0
	unsigned int  indices[8];	/* triangle index of hitted surface */
#endif
	unsigned char occlusions[8];	/* occlusion(0 or 1) of points
					 * this node contains (shold be less
					 * than 8)	*/
	unsigned char nodeocclusion;	/* occlusion of this node	*/
} ri_htm_occlusion_node_t;

typedef struct _ri_htm_t
{
	ri_htm_node_t *nodes;
	int            nnodes;
	ri_vector_t   *vertices;
	int            nvertices;
	ri_vector_t   *points;
	int            npoints;
	unsigned char *sampled;		/* Is point already sampled?
					 * Becase one sampling point can be
					 * included in multiple nodes. */
} ri_htm_t;

typedef struct _ri_htm_occlusion_t
{
	ri_htm_occlusion_node_t *nodes;
	int                      nnodes;
} ri_htm_occlusion_t;

extern ri_htm_t *ri_htm_build(int level);
extern void      ri_htm_free(ri_htm_t *htm);
extern ri_htm_occlusion_t *ri_htm_occlusion_new();
extern void      ri_htm_occlusion_free(ri_htm_occlusion_t *dst);
extern void      ri_htm_occlusion_copy(ri_htm_occlusion_t *dst,
				       ri_htm_occlusion_t *src);
extern void      ri_htm_occlusion_clear(ri_htm_occlusion_t *dst);
extern void      ri_htm_occlusion_build_hierarchy(ri_htm_occlusion_t *dst,
						  ri_htm_t *htm);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

