/*
 * $Id: octree.c,v 1.1.1.1 2004/01/06 13:57:09 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "memory.h"
#include "log.h"
#include "octree.h"

static void
octree_traverse(ri_octree_t *node,
		int (*func)(ri_octree_t *node, void *data), void *data);

ri_octree_t *
ri_octree_new()
{
	int i;
	ri_octree_t *p = NULL;

	p = (ri_octree_t *)ri_mem_alloc(sizeof(ri_octree_t));

	p->vals = NULL;
	p->data = NULL;
	p->center.e[0] = 0.0f; p->center.e[1] = 0.0f; p->center.e[2] = 0.0f;
	p->width = 1000;
	p->depth = 0;
	p->leaf  = 0;
	p->code  = -1;

	for (i = 0; i < 8; i++) {
		p->child[i] = NULL;
	}

	return p;
}

void
ri_octree_free(ri_octree_t *octree)
{
	int i;

	for (i = 0; i < 8; i++) {
		if (octree->child[i]) ri_octree_free(octree->child[i]);
	}

	ri_array_free(octree->vals);
	ri_mem_free(octree);
}

void
ri_octree_traverse(ri_octree_t *octree,
		   int (*func)(ri_octree_t *node, void *data), void *data)
{
	(void)octree;
	(void)func;
	(void)data;
	
}

/* --- private functions --- */

static void
octree_traverse(ri_octree_t *node,
		int (*func)(ri_octree_t *node, void *data), void *data)
{
	int i;

	if ((*func)(node, data)) return;

	for (i = 0; i < 8; i++) {
		if (node->child[i]) {
			octree_traverse(node->child[i], func, data);
		}
	}	
}
