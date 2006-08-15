/*
 * $Id: bsp.c,v 1.7 2004/06/13 06:44:51 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#include "memory.h"
#include "log.h"
#include "bsp.h"
#include "geom.h"
#include "list.h"
#include "raytrace.h"
#include "log.h"
#include "timer.h"

#define EPSILON 1.0e-6

static void add_tri_to_list(trilist_t *list, const ri_triinfo_t *tri);
#if 0
static void project_tri(const ri_vector_t *dir,
			const ri_vector_t *v0,
			const ri_vector_t *v1,
			const ri_vector_t *v2,
			float             *min,
			float             *max);
static void project_box(const ri_vector_t *dir,
			const ri_vector_t *bmin,
			const ri_vector_t *bmax,
			float             *min,
			float             *max);
#endif
#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
static void convert_simd(trilist_t *dst);
#endif
static void dump_bsp(ri_bsp_t *bsp);
static void dump_bsp_traverse(binnode_t *node, FILE *fp);

static long ncalls = 0;
static long ntravs = 0;
static long ntritests = 0;

/* return the distance between origin and subdivision plane. */

float dist_to_xplane(const ri_vector_t *plane,
		     const ri_vector_t *org, const ri_vector_t *dir)
{
	float div;
	if (dir->e[0] == 0.0) {
		div = 1.0f;
	} else {
		div = dir->e[0];
	}

	return ((plane->e[0] - org->e[0]) / div);
}

float dist_to_yplane(const ri_vector_t *plane,
		     const ri_vector_t *org, const ri_vector_t *dir)
{
	float div;
	if (dir->e[1] == 0.0) {
		div = 1.0f;
	} else {
		div = dir->e[1];
	}

	return ((plane->e[1] - org->e[1]) / div);
}

float dist_to_zplane(const ri_vector_t *plane,
		     const ri_vector_t *org, const ri_vector_t *dir)
{
	float div;
	if (dir->e[2] == 0.0) {
		div = 1.0f;
	} else {
		div = dir->e[2];
	}
	return ((plane->e[2] - org->e[2]) / div);
}


/* determines which of the half space of the two children contains origin,
 * return that child as near, the other as far.
 */
void
get_xchildren(binnode_t *node, binnode_t **nearnode, binnode_t **farnode,
	      const ri_vector_t *org)
{
	if (node->child[0]->max.e[0] >= org->e[0]) {
		*nearnode = node->child[0];
		*farnode  = node->child[1];
	} else {
		*nearnode = node->child[1];
		*farnode  = node->child[0];
	}
}

void
get_ychildren(binnode_t *node, binnode_t **nearnode, binnode_t **farnode,
	      const ri_vector_t *org)
{
	if (node->child[0]->max.e[1] >= org->e[1]) {
		*nearnode = node->child[0];
		*farnode  = node->child[1];
	} else {
		*nearnode = node->child[1];
		*farnode  = node->child[0];
	}
}

void
get_zchildren(binnode_t *node, binnode_t **nearnode, binnode_t **farnode,
	      const ri_vector_t *org)
{
	if (node->child[0]->max.e[2] >= org->e[2]) {
		*nearnode = node->child[0];
		*farnode  = node->child[1];
	} else {
		*nearnode = node->child[1];
		*farnode  = node->child[0];
	}
}

int
tri_in_node(binnode_t *node, ri_vector_t *tri)
{
	int   i;
	float tmin[3], tmax[3];

	/* get triangle's bounding box */
	tmin[0] = tri[0].e[0]; tmin[1] = tri[0].e[1]; tmin[2] = tri[0].e[2];
	tmax[0] = tri[0].e[0]; tmax[1] = tri[0].e[1]; tmax[2] = tri[0].e[2];

	for (i = 1; i < 3; i++) {
		if (tmin[0] > tri[i].e[0]) tmin[0] = tri[i].e[0];		
		if (tmin[1] > tri[i].e[1]) tmin[1] = tri[i].e[1];		
		if (tmin[2] > tri[i].e[2]) tmin[2] = tri[i].e[2];		
		if (tmax[0] < tri[i].e[0]) tmax[0] = tri[i].e[0];		
		if (tmax[1] < tri[i].e[1]) tmax[1] = tri[i].e[1];		
		if (tmax[2] < tri[i].e[2]) tmax[2] = tri[i].e[2];		
	}

	if (node->min.e[0] > tmax[0] || node->max.e[0] < tmin[0]) return 0;
	if (node->min.e[1] > tmax[1] || node->max.e[1] < tmin[1]) return 0;
	if (node->min.e[2] > tmax[2] || node->max.e[2] < tmin[2]) return 0;

	return 1;
}

void
add_tri_to_node(binnode_t *node, trilist_t trilist)
{
	long i;
	ri_triinfo_t tri;
	long count;

	//printf("trilist size = %d\n", trilist.ntris);
	count = 0;
	for (i = 0; i < trilist.ntris; i++) {
		ri_mem_copy(tri.vlist,
			    &(trilist.vlist[3 * i]),
			    sizeof(ri_vector_t) * 3);

		tri.index = trilist.indices[i];
		tri.geom  = trilist.geoms[i];
#if 0

		printf("tri (%f, %f, %f), (%f, %f, %f), (%f, %f, %f)\n",
			tri.vlist[0].e[0],
			tri.vlist[0].e[1],
			tri.vlist[0].e[2],
			tri.vlist[1].e[0],
			tri.vlist[1].e[1],
			tri.vlist[1].e[2],
			tri.vlist[2].e[0],
			tri.vlist[2].e[1],
			tri.vlist[2].e[2]);
		printf("bbox = (%f, %f, %f), (%f, %f, %f)\n",
			node->min.e[0],
			node->min.e[1],
			node->min.e[2],
			node->max.e[0],
			node->max.e[1],
			node->max.e[2]);
#endif
		if (tri_in_node(node, tri.vlist)) {
			//printf("result: in\n");
			add_tri_to_list(&(node->trilists), &tri);
			count++;
		} else {
			//printf("result: out\n");
		}
	}

	//printf("add_tri: count = %ld\n", count);
	//printf("add_tri: ntris = %u\n", node->trilists.ntris);
}

/* Builds the BSP tre by subdividing along the center of x, y, or z bounds,
 * one each time this function is called. This function calls itself
 * recursively until either the tre is deeper than maxdepth or all of the
 * tree leaves contains less than maxlistlen of objects.
 */
void
subdivide(binnode_t *node, int depth, int maxdepth, int maxlistlen)
{
	int        i, nextaxis;
	int        maxwidthdim;
	float      maxwidth;	

	node->child[0] = node->child[1] = NULL;

	if (node->trilists.ntris == 0) return;

	/* find dimension with maximum width */
	maxwidthdim = 1;
	maxwidth    = node->max.e[0] - node->min.e[0];

	if (maxwidth < (node->max.e[1] - node->min.e[1])) {
		maxwidthdim = 2;
		maxwidth    = node->max.e[1] - node->min.e[1];
	}

	if (maxwidth < (node->max.e[2] - node->min.e[2])) {
		maxwidthdim = 3;
		maxwidth    = node->max.e[2] - node->min.e[2];
	}

	//printf("maxwidth dim = %d\n", maxwidthdim);

	if (maxwidthdim == 1) {	/* x */
		node->dist = dist_to_xplane;
		node->getchildren = get_xchildren;
	} else if (maxwidthdim == 2) { /* y */
		node->dist = dist_to_yplane;
		node->getchildren = get_ychildren;
	} else { /* z */
		node->dist = dist_to_zplane;
		node->getchildren = get_zchildren;
	}

	//printf("depth: %d, node: ntris = %d\n", depth, node->trilists.ntris);
	if ((node->trilists.ntris > maxlistlen) && (depth < maxdepth)) {
		for (i = 0; i < 2; i++) {
			node->child[i] = (binnode_t *)
					 ri_mem_alloc(sizeof(binnode_t));	
			node->child[i]->min.e[0] = node->min.e[0];
			node->child[i]->min.e[1] = node->min.e[1];
			node->child[i]->min.e[2] = node->min.e[2];

			node->child[i]->max.e[0] = node->max.e[0];
			node->child[i]->max.e[1] = node->max.e[1];
			node->child[i]->max.e[2] = node->max.e[2];


			if (maxwidthdim == 1) {
				node->child[i]->min.e[0] = (float)(
					node->min.e[0] + 0.5 * i *
					(node->max.e[0] - node->min.e[0]));
				node->child[i]->max.e[0] = (float)(
					node->min.e[0] + 0.5 * (i + 1) *
					(node->max.e[0] - node->min.e[0]));

				nextaxis = 2;

			} else if (maxwidthdim == 2) {
				node->child[i]->min.e[1] = (float)(
					node->min.e[1] + 0.5 * i *
					(node->max.e[1] - node->min.e[1]));
				node->child[i]->max.e[1] = (float)(
					node->min.e[1] + 0.5 * (i + 1) *
					(node->max.e[1] - node->min.e[1]));

				nextaxis = 3;

			} else {
				node->child[i]->min.e[2] = (float)(
					node->min.e[2] + 0.5 * i *
					(node->max.e[2] - node->min.e[2]));
				node->child[i]->max.e[2] = (float)(
					node->min.e[2] + 0.5 * (i + 1) *
					(node->max.e[2] - node->min.e[2]));

				nextaxis = 1;

			}

			node->child[i]->trilists.vlist      = NULL;
			node->child[i]->trilists.indices    = NULL;
			node->child[i]->trilists.geoms      = NULL;
			node->child[i]->trilists.ntris      = 0;
			node->child[i]->trilists.simdtris   = NULL;

			add_tri_to_node(node->child[i], node->trilists);

#if 0
			if (node->child[i]->trilists.ntris == 0) {
				ri_mem_free(node->child[i]->trilists.vlist);
				ri_mem_free(node->child[i]->trilists.indices);
				ri_mem_free(node->child[i]->trilists.geoms);
				ri_mem_free(node->child[i]);
				node->child[i] = NULL;
				continue;
			}
#endif

			node->child[i]->depth = depth + 1;

#if 0
			//printf("BSP: child[%d].ntris = %d\n",
			//	i, node->child[i]->trilists.ntris);
		
			//printf("BSP: subdivide. child = %d, depth = %d\n",
			//	i, depth + 1);
			printf("depth = %d\n", depth);
			printf("child[%d].depth = %d\n", 
				i, node->child[i]->depth);
			printf("child[%d].trilists = %d\n",
				i, node->child[i]->trilists.ntris);
			printf("child[%d].bbox = (%f, %f, %f), (%f, %f, %f)\n",
				i,
				node->child[i]->min.e[0],
				node->child[i]->min.e[1],
				node->child[i]->min.e[2],
				node->child[i]->max.e[0],
				node->child[i]->max.e[1],
				node->child[i]->max.e[2]);
#endif

			subdivide(node->child[i], depth + 1,
				  maxdepth, maxlistlen);
		}

	

	}

#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
	convert_simd(&node->trilists);
#endif
	//printf("leaf()\n");

}


void
ri_bsp_free(ri_bsp_t *bsp)
{
	/* todo: implement */
	//ri_list_free(bsp->vals);
	ri_mem_free(bsp);
}

ri_bsp_t *
ri_accel_build_bsp()
{
	long         i;
	long         id[3];
	long         offset;
	long         ntris;
	ri_list_t   *geomitr;
	ri_geom_t   *geom;
	ri_vector_t *vecp;
	ri_bsp_t    *tree;
	ri_list_t   *geomlist;

	ri_log(LOG_INFO, "--- building BSP tree... ---");
	ri_timer_start(ri_render_get()->context->timer, "BSP-tree building");

	geomlist = ri_render_get()->geomlist;

	tree = (ri_bsp_t *)ri_mem_alloc(sizeof(ri_bsp_t));

	tree->root = (binnode_t *)ri_mem_alloc(sizeof(binnode_t));

	/* first calculate total number of triangles. */
	ntris = 0;
	for (geomitr  = ri_list_first(geomlist);
	     geomitr != NULL;
	     geomitr  = ri_list_next(geomitr)) {
		geom = (ri_geom_t *)geomitr->data;

		ntris += geom->nindices / 3;
	}

	tree->root->trilists.vlist =
		(ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * 3 * ntris);
	tree->root->trilists.indices =
		(unsigned int *)ri_mem_alloc(sizeof(unsigned int) * ntris);
	tree->root->trilists.geoms =
		(ri_geom_t **)ri_mem_alloc(sizeof(ri_geom_t *) * ntris);
	tree->root->trilists.ntris    = ntris;
	tree->root->trilists.simdtris = NULL;

	//printf("BSP: total ntris = %ld\n", ntris);

	ntris = 0;
	for (geomitr  = ri_list_first(geomlist);
	     geomitr != NULL;
	     geomitr  = ri_list_next(geomitr)) {
		geom = (ri_geom_t *)geomitr->data;

		for (i = 0; i < (int)geom->nindices / 3; i++) {
			id[0] = geom->indices[3 * i + 0];	
			id[1] = geom->indices[3 * i + 1];	
			id[2] = geom->indices[3 * i + 2];	

			offset = 3 * (ntris + i);
			vecp = &(tree->root->trilists.vlist[offset + 0]);
			ri_vector_copy(vecp, &(geom->positions[id[0]]));
			vecp = &(tree->root->trilists.vlist[offset + 1]);
			ri_vector_copy(vecp, &(geom->positions[id[1]]));
			vecp = &(tree->root->trilists.vlist[offset + 2]);
			ri_vector_copy(vecp, &(geom->positions[id[2]]));
		
			offset = ntris + i;
			tree->root->trilists.geoms[offset] = geom;
			tree->root->trilists.indices[offset] = i;
		}

		ntris += geom->nindices / 3;
	}

	if (ntris != tree->root->trilists.ntris) {
		//printf("ntris = %d\n", ntris);
	}

	/* calculate the extent of the tree. */
	ri_vector_copy(&(tree->min), &(tree->root->trilists.vlist[0]));
	ri_vector_copy(&(tree->max), &(tree->root->trilists.vlist[0]));
	for (i = 1; i < tree->root->trilists.ntris * 3; i++) {
		if (tree->min.e[0] > tree->root->trilists.vlist[i].e[0]) {
			tree->min.e[0] = tree->root->trilists.vlist[i].e[0];
		}
		if (tree->min.e[1] > tree->root->trilists.vlist[i].e[1]) {
			tree->min.e[1] = tree->root->trilists.vlist[i].e[1];
		}
		if (tree->min.e[2] > tree->root->trilists.vlist[i].e[2]) {
			tree->min.e[2] = tree->root->trilists.vlist[i].e[2];
		}

		if (tree->max.e[0] < tree->root->trilists.vlist[i].e[0]) {
			tree->max.e[0] = tree->root->trilists.vlist[i].e[0];
		}
		if (tree->max.e[1] < tree->root->trilists.vlist[i].e[1]) {
			tree->max.e[1] = tree->root->trilists.vlist[i].e[1];
		}
		if (tree->max.e[2] < tree->root->trilists.vlist[i].e[2]) {
			tree->max.e[2] = tree->root->trilists.vlist[i].e[2];
		}
	}

	/* extend slightly for numerical problem. */
	tree->min.e[0] -= 1.0e-3f;
	tree->min.e[1] -= 1.0e-3f;
	tree->min.e[2] -= 1.0e-3f;
	tree->max.e[0] += 1.0e-3f;
	tree->max.e[1] += 1.0e-3f;
	tree->max.e[2] += 1.0e-3f;
#if 0

	printf("bounding box min = (%f, %f, %f)\n",
		tree->min.e[0], tree->min.e[1], tree->min.e[2]);
	printf("bounding box max = (%f, %f, %f)\n",
		tree->max.e[0], tree->max.e[1], tree->max.e[2]);
#endif

	tree->maxdepth = ri_render_get()->context->option->bsp_tree_depth;
	tree->maxlistlen = 4;

	ri_vector_copy(&(tree->root->min), &(tree->min));
	ri_vector_copy(&(tree->root->max), &(tree->max));

	tree->root->depth = 0;
	subdivide(tree->root, 0, tree->maxdepth, tree->maxlistlen);

	ri_timer_end(ri_render_get()->context->timer, "BSP-tree building");
	ri_log(LOG_INFO, "--- built BSP tree. ---");

	dump_bsp(tree);

	return tree;
}

/* --- private functions --- */

static void
resize_trilist(trilist_t *list, long size)
{
	ri_vector_t  *newvlist;
	unsigned int *newindices;
	ri_geom_t    **newgeoms;

	if (list->ntris >= size) {
		return;
	}

	newvlist   = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
						 3 * size);
	newindices = (unsigned int *)ri_mem_alloc(sizeof(unsigned int) * size);
	newgeoms   = (ri_geom_t **)ri_mem_alloc(sizeof(ri_geom_t *) * size);

	ri_mem_copy(newvlist, list->vlist,
		    sizeof(ri_vector_t) * 3 * list->ntris);
	ri_mem_copy(newindices, list->indices,
		    sizeof(unsigned int) * list->ntris);
	ri_mem_copy(newgeoms, list->geoms,
		    sizeof(ri_geom_t *) * list->ntris);

	ri_mem_free(list->vlist);
	ri_mem_free(list->indices);
	ri_mem_free(list->geoms);

	list->vlist   = newvlist;
	list->indices = newindices;
	list->geoms   = newgeoms;

	list->nalloced = size;
}

static void
add_tri_to_list(trilist_t *list, const ri_triinfo_t *tri)
{
	if (list->ntris == 0) {
		list->vlist   = (ri_vector_t *)
				ri_mem_alloc(sizeof(ri_vector_t) * 3);
		list->indices = (unsigned int *)ri_mem_alloc(
							sizeof(unsigned int));
		list->geoms   = (ri_geom_t **)ri_mem_alloc(sizeof(ri_geom_t *));

		ri_mem_copy(&(list->vlist[0]),
			    tri->vlist,
			    sizeof(ri_vector_t) * 3);

		list->indices[0] = tri->index;
		list->geoms[0]   = tri->geom;

		list->ntris = 1;
		list->nalloced = 1;

	} else {
		if (list->ntris >= list->nalloced) {
			resize_trilist(list, list->nalloced * 2);
		} 

		ri_mem_copy(&(list->vlist[3 * list->ntris]),
			    tri->vlist,
			    sizeof(ri_vector_t) * 3);

		list->indices[list->ntris] = tri->index;
		list->geoms[list->ntris]   = tri->geom;

		list->ntris++;
	}
}

#if 0
static void
project_tri(const ri_vector_t *dir,
	    const ri_vector_t *v0,
	    const ri_vector_t *v1,
	    const ri_vector_t *v2,
	    float             *min,
	    float             *max)
{
	float dot;

	(*min) = ri_vector_dot3(dir, v0);
	(*max) = (*min);

	dot = ri_vector_dot3(dir, v1);
	if (dot < (*min)) {
		(*min) = dot;
	} else if (dot > (*max)) {
		(*max) = dot;
	}

	dot = ri_vector_dot3(dir, v2);
	if (dot < (*min)) {
		(*min) = dot;
	} else if (dot > (*max)) {
		(*max) = dot;
	}
}

static void
project_box(const ri_vector_t *dir,
	    const ri_vector_t *bmin,
	    const ri_vector_t *bmax,
	    float             *min,
	    float             *max)
{
	float dot;
	float r;
	float xdist, ydist, zdist;
	ri_vector_t center;

	ri_vector_add(&center, bmin, bmax);
	ri_vector_scale(&center, 0.5);

	dot = ri_vector_dot3(dir, &center);

	xdist = bmax->e[0] - bmin->e[0];
	ydist = bmax->e[1] - bmin->e[1];
	zdist = bmax->e[2] - bmin->e[2];

	r = (float)(
	    xdist * fabs(dir->e[0]) +
	    ydist * fabs(dir->e[1]) +
	    zdist * fabs(dir->e[2])); 

	(*min) = dot - r;
	(*max) = dot + r;
}
#endif

void
ri_bsp_statistics()
{
	printf("bsp: number of traversals = %lu\n", ntravs);
	printf("bsp: number of ray-triangle test = %lu\n", ntritests);
	printf("bsp: number of bsp calls = %lu\n", ncalls);

	printf("bsp: travs/calls = %f\n", (float)ntravs / (float)ncalls);
	printf("bsp: tritests/calls = %f\n", (float)ntritests / (float)ncalls);

	
}

static void
dump_bsp(ri_bsp_t *bsp)
{
	FILE *fp;

	fp = fopen("bsp.dat", "w");
	if (!fp) exit(-1);

	dump_bsp_traverse(bsp->root, fp);

	fclose(fp);
}

static void
dump_bsp_traverse(binnode_t *node, FILE *fp)
{
	int i;
	ri_vector_t *v;

	if (node->dist == dist_to_xplane) {
		fprintf(fp, "1\n");
	} else if (node->dist == dist_to_yplane) {
		fprintf(fp, "2\n");
	} else {
		fprintf(fp, "3\n");
	}

	fprintf(fp, "%d\n", node->trilists.ntris); 
	for (i = 0; i < node->trilists.ntris * 3; i++) {
		v = &(node->trilists.vlist[i]);
		fprintf(fp, "%f %f %f\n", v->e[0], v->e[1], v->e[2]);
	}

	if (!node->child[0] && !node->child[1]) {
		fprintf(fp, "-1\n");
		return;
	}

	if (node->child[0]) {
		fprintf(fp, "1\n");
		dump_bsp_traverse(node->child[0], fp);
	}

	if (node->child[1]) {
		fprintf(fp, "2\n");
		dump_bsp_traverse(node->child[1], fp);
	}

	if (!node->child[0] || !node->child[1]) {
		fprintf(fp, "-2\n");
	}
}

#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
/* re-arrange the triangle data to be a SIMD friendly data structure. */
static void
convert_simd(trilist_t *dst)
{
	int          i, j;
	int          offset;
	int          nextra;
	ri_vector_t *v0, *v1, *v2;
	ri_vector_t e1, e2;
	unsigned int i0, i1, i2;
	ri_geom_t  *geom;
	unsigned int index;
	ri_geom_t **geoms;
	unsigned int *indices;
	ri_simd_tri_info_t *simdtris;
	int nblocks;

	if (dst->vlist == NULL) return;
	if (dst->ntris < 1) return;

	simdtris = (ri_simd_tri_info_t *)
			ri_mem_alloc(sizeof(ri_simd_tri_info_t));

	/* simd array must be a multiple of 4 */
	nblocks = (int)ceil((double)dst->ntris / (double)4);
	if (nblocks < 1) nblocks = 1;

	simdtris->nblocks = nblocks;

	/* 4 = 4 triangles,
	 * 9 = (xyz 3 components) * (3 vertices) = 1 triangle
	 */
	/* allocate 16-byte aligned memory */
	ri_aligned_float_alloc(&(simdtris->tridata), 4 * 9 * nblocks);

	geoms = (ri_geom_t **)ri_mem_alloc(sizeof(ri_geom_t *) * nblocks * 4);
	indices = (unsigned int *)ri_mem_alloc(
					sizeof(unsigned int) * nblocks * 4);

	for (j = 0; j < nblocks - 1; j++) {
		for (i = 0; i < 4; i++) {
			offset = 4 * j + i;
			geom = dst->geoms[offset];
			index = dst->indices[offset];

			i0 = 3 * offset + 0;
			i1 = 3 * offset + 1;
			i2 = 3 * offset + 2;

			v0 = &(dst->vlist[i0]);
			v1 = &(dst->vlist[i1]);
			v2 = &(dst->vlist[i2]);

			ri_vector_sub(&e1, v1, v0);
			ri_vector_sub(&e2, v2, v0);

			simdtris->tridata.aligned[36 * j +  0 + i] = v0->e[0];
			simdtris->tridata.aligned[36 * j +  4 + i] = v0->e[1];
			simdtris->tridata.aligned[36 * j +  8 + i] = v0->e[2];
			simdtris->tridata.aligned[36 * j + 12 + i] = e1.e[0];
			simdtris->tridata.aligned[36 * j + 16 + i] = e1.e[1];
			simdtris->tridata.aligned[36 * j + 20 + i] = e1.e[2];
			simdtris->tridata.aligned[36 * j + 24 + i] = e2.e[0];
			simdtris->tridata.aligned[36 * j + 28 + i] = e2.e[1];
			simdtris->tridata.aligned[36 * j + 32 + i] = e2.e[2];

			indices[offset] = index;
			geoms[offset]   = geom;
		}
	}

	/* the reminder */
	nextra = dst->ntris - (nblocks - 1) * 4;
	for (i = 0; i < nextra; i++) {
		offset = (nblocks - 1);
		geom = dst->geoms[4 * offset + i];
		index = dst->indices[4 * offset + i];

		i0 = 3 * (4 * offset + i) + 0;
		i1 = 3 * (4 * offset + i) + 1;
		i2 = 3 * (4 * offset + i) + 2;

		v0 = &(dst->vlist[i0]);
		v1 = &(dst->vlist[i1]);
		v2 = &(dst->vlist[i2]);

		ri_vector_sub(&e1, v1, v0);
		ri_vector_sub(&e2, v2, v0);

		simdtris->tridata.aligned[36 * offset +  0 + i] = v0->e[0];
		simdtris->tridata.aligned[36 * offset +  4 + i] = v0->e[1];
		simdtris->tridata.aligned[36 * offset +  8 + i] = v0->e[2];
		simdtris->tridata.aligned[36 * offset + 12 + i] = e1.e[0];
		simdtris->tridata.aligned[36 * offset + 16 + i] = e1.e[1];
		simdtris->tridata.aligned[36 * offset + 20 + i] = e1.e[2];
		simdtris->tridata.aligned[36 * offset + 24 + i] = e2.e[0];
		simdtris->tridata.aligned[36 * offset + 28 + i] = e2.e[1];
		simdtris->tridata.aligned[36 * offset + 32 + i] = e2.e[2];

		indices[4 * offset + i] = index;
		geoms[4 * offset + i]   = geom;
	}

	/* fill the rest with last triangle data */
	if (nextra != 0) {
		for (i = nextra; i < 4; i++) {
			offset = (nblocks - 1);
			geom = dst->geoms[4 * offset + nextra - 1];
			index = dst->indices[4 * offset + nextra - 1];

			i0 = 3 * (4 * offset + nextra - 1) + 0;
			i1 = 3 * (4 * offset + nextra - 1) + 1;
			i2 = 3 * (4 * offset + nextra - 1) + 2;

			v0 = &(dst->vlist[i0]);
			v1 = &(dst->vlist[i1]);
			v2 = &(dst->vlist[i2]);

			ri_vector_sub(&e1, v1, v0);
			ri_vector_sub(&e2, v2, v0);

			simdtris->tridata.aligned[36*offset+ 0+i] = v0->e[0];
			simdtris->tridata.aligned[36*offset+ 4+i] = v0->e[1];
			simdtris->tridata.aligned[36*offset+ 8+i] = v0->e[2];
			simdtris->tridata.aligned[36*offset+12+i] = e1.e[0];
			simdtris->tridata.aligned[36*offset+16+i] = e1.e[1];
			simdtris->tridata.aligned[36*offset+20+i] = e1.e[2];
			simdtris->tridata.aligned[36*offset+24+i] = e2.e[0];
			simdtris->tridata.aligned[36*offset+28+i] = e2.e[1];
			simdtris->tridata.aligned[36*offset+32+i] = e2.e[2];

			indices[4 * offset + i] = index;
			geoms[4 * offset + i]   = geom;
		}
	}

	ri_mem_free(dst->indices); dst->indices = NULL;
	ri_mem_free(dst->geoms);   dst->geoms   = NULL;

	simdtris->indices = indices;
	simdtris->geoms   = geoms;

	/* Now dst->vlist will never be used. */
	ri_mem_free(dst->vlist);
}
#endif
