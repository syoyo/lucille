/*
 * octree routine.
 *
 * $Id: octree.h,v 1.2 2004/01/30 04:43:57 syoyo Exp $
 */

#ifndef OCTREE_H
#define OCTREE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vector.h"
#include "array.h"

typedef struct _ri_octree_t
{
	struct _ri_octree_t *child[8];

	ri_array_t          *vals;
	void                *data;
	ri_vector_t          center;
	float		     width;	/* from center to boundary	*/
	int                  depth;
	int                  leaf;	/* is this node leaf?		*/
	int                  code;
	
} ri_octree_t;

extern ri_octree_t *ri_octree_new     ();
extern void	    ri_octree_free    (ri_octree_t *octree);
extern void	    ri_octree_traverse(ri_octree_t *octree,
				       int (*func)(ri_octree_t *node, void *data),
				       void *data);

#ifdef __cplusplus
}	/* extern "C" */
#endif


#endif
