/*
 * Hierarchical Triangular Mesh routine used in prt.c.
 *
 * For more detail and implementation, see:
 * "Hierarchical Triangular Mesh"
 * http://www.sdss.jhu.edu/htm/
 *
 * $Id: htm.c,v 1.4 2004/04/16 13:46:45 syoyo Exp $
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <assert.h>
#include <math.h>

#include "memory.h"
#include "htm.h"
#include "qmc.h"

/*
 * HTM is a quad tree of spherical triangles.
 * Subdivide from initial octahedron. then every triangle can be decomposited
 * into 4 new triangles by dividing each midpoints of its edges.
 *
 * Each subdivided triangle area is neary equal.
 *
 * The number of triangles generated are as follows.
 * 
 * level:   ntriangles:
 *     1             8
 *     2            32
 *     3           128 
 *     4           512
 *     5          2048
 *     6          8192
 *     7         32768
 *   ...           ...
 */

typedef struct _layert_t
{
	int level;		/* layer level				*/
	int nvertices;		/* number of vertices			*/
	int nnodes;		/* number of nodes			*/
	int nedges;		/* number of edges			*/
	int firstindex;		/* index of first node of this layer	*/
	int firstvertex;	/* index of first vertex of this layer	*/
} layer_t;

typedef struct _edge_t
{
	int start;		/* starting vertex index of edge	*/
	int end;		/* index of end				*/
	int mid;		/* index of center(midpoint)		*/
} edge_t;

#define dot3(v0, v1) ((v0)[0] * (v1)[0] + (v0)[1] * (v1)[1] + (v0)[2] * (v1)[2])

static void cross3(double out[3], double v0[3], double v1[3]);
static void normalize(double v[3]);
static void build(int level, double *vertices, ri_htm_node_t *nodes);
static void calc_size(int level, int *nnodes, int *nvertices);
static int st_inside(double v0[3], double v1[3], double v2[3], double query[3]);
static int newnode(ri_htm_node_t *node, int v0, int v1, int v2,
		   int id, int parent, int *index);
static void newlayer(layer_t *newl, layer_t *oldl, ri_htm_node_t *nodes,
		     int *index);
static int newedge(double *vertices, edge_t *edges, edge_t **ltab,
		   ri_htm_node_t *nodes, int emindex, int index, int k,
		   int *accumindex);
static void subdivide(layer_t *layer, double *vertices, ri_htm_node_t *nodes);
static void makemidpoints(layer_t *layer, double *vertices, edge_t *edges,
			  edge_t **ltab, ri_htm_node_t *nodes, int accumindex);
static edge_t *edgematch(edge_t **ltab, edge_t *em);
static void insertlookup(edge_t **ltab, edge_t *em);
static int getmidpoint(double *vertices, edge_t *em, int *index);
static void sortindex(ri_htm_node_t *nodes, int nnodes);
//static int  id_by_point(double *vertices, ri_htm_node_t *nodes, double v[3]);
static void contain(ri_htm_node_t *nodes, int nnodes,
		    double *vertices, int nvertices,
		    double *points, int npoints);
static unsigned char occlusion_hierarchy_trav(ri_htm_t *htm,
					      ri_htm_occlusion_t *occnode,
					      int parent);

ri_htm_t *
ri_htm_build(int level)
{
	int         i;
	int         nnodes, nvertices;
	int         npoints;
	double     *vertices;
	double     *samplepoints;
	ri_htm_node_t *nodes;
	ri_htm_t   *p;

	if (level < 0) level = 0;

	calc_size(level, &nnodes, &nvertices);

	vertices = (double *)ri_mem_alloc(nvertices * sizeof(double) * 3);

	/* +1 because index starts with 1. */
	nodes = (ri_htm_node_t *)ri_mem_alloc(sizeof(ri_htm_node_t) * (nnodes + 1));

	/* generate sampling points over the sphere */
	npoints = 8 * (int)pow(4, level+1);
	samplepoints = (double *)ri_mem_alloc(sizeof(double) * npoints * 3);
	hammersley2_sphere(samplepoints, npoints);

	p = (ri_htm_t *)ri_mem_alloc(sizeof(ri_htm_t));

	p->vertices = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
						  nvertices);
	p->points = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * npoints);
	p->sampled = (unsigned char *)ri_mem_alloc(sizeof(unsigned char) *
						   npoints);

	build(level, vertices, nodes);

	contain(nodes, nnodes, vertices, nvertices, samplepoints, npoints);

	for (i = 0; i < nvertices; i++) {
		p->vertices[i].e[0] = (float)vertices[3 * i + 0];
		p->vertices[i].e[1] = (float)vertices[3 * i + 1];
		p->vertices[i].e[2] = (float)vertices[3 * i + 2];
	}

	p->nvertices = nvertices;
	p->nodes = nodes;
	for (i = 0; i < npoints; i++) {
		p->points[i].e[0] = (float)samplepoints[3 * i + 0];
		p->points[i].e[1] = (float)samplepoints[3 * i + 1];
		p->points[i].e[2] = (float)samplepoints[3 * i + 2];
	}
	p->nnodes = nnodes;
	p->npoints = npoints;

	for (i = 0; i < npoints; i++) {
		p->sampled[i] = 0;
	}

	ri_mem_free(vertices);
	ri_mem_free(samplepoints);

	return p;
}

void
ri_htm_free(ri_htm_t *htm)
{
	ri_mem_free(htm->nodes);
	ri_mem_free(htm->points);
	ri_mem_free(htm->vertices);
	ri_mem_free(htm->sampled);
	ri_mem_free(htm);
}

ri_htm_occlusion_t *
ri_htm_occlusion_new(int level)
{
	int nnodes, nvertices;
	ri_htm_occlusion_t *p;

	calc_size(level, &nnodes, &nvertices);

	p = (ri_htm_occlusion_t *)ri_mem_alloc(sizeof(ri_htm_occlusion_t));

	p->nodes = (ri_htm_occlusion_node_t *)ri_mem_alloc(
				sizeof(ri_htm_occlusion_node_t) * (nnodes+1));
	p->nnodes = nnodes;

	ri_htm_occlusion_clear(p);

	return p;
}

void
ri_htm_occlusion_free(ri_htm_occlusion_t *dst)
{
	ri_mem_free(dst->nodes);
	ri_mem_free(dst);

}

void
ri_htm_occlusion_copy(ri_htm_occlusion_t *dst, ri_htm_occlusion_t *src)
{
	dst->nnodes = src->nnodes;
	ri_mem_copy(dst->nodes, src->nodes,
		    sizeof(ri_htm_occlusion_node_t) * src->nnodes);
}

void
ri_htm_occlusion_clear(ri_htm_occlusion_t *dst)
{
	int i, j;

	for (i = 0; i < dst->nnodes + 1; i++) {
		for (j = 0; j < 8; j++) {
			dst->nodes[i].occlusions[j] = 0;
		}

		dst->nodes[i].nodeocclusion = 0;	
	}
}

void
ri_htm_occlusion_build_hierarchy(ri_htm_occlusion_t *dst, ri_htm_t *htm)
{
	int i;

	for (i = 1; i <= 8; i++ ) {
		occlusion_hierarchy_trav(htm, dst, i);
	}
}

/* --- private functions --- */

static void
build(int level, double *vertices, ri_htm_node_t *nodes)
{
	int         i;
	int         index;
	int         nnodes, nvertices;
	int         layerlevel;
	layer_t    *layers;

	/* first 6 vertices */
	double v[6][3] = {
		{ 0.0,  0.0,  1.0},	// 0
		{ 1.0,  0.0,  0.0},	// 1
		{ 0.0,  1.0,  0.0},	// 2
		{-1.0,  0.0,  0.0},	// 3
		{ 0.0, -1.0,  0.0},	// 4
		{ 0.0,  0.0, -1.0},	// 5
	};

	if (level < 0) level = 0;

	calc_size(level, &nnodes, &nvertices);

	layers = (layer_t *)malloc(sizeof(layer_t) * (level + 1));
	if (!layers) {
		fprintf(stderr, "muda muda\n");
		exit(-1);
	}

	layers[0].level       = 0;
	layers[0].nvertices   = 6;
	layers[0].nnodes      = 8;
	layers[0].nedges      = 12;
	layers[0].firstindex  = 1;
	layers[0].firstvertex = 0;

	for (i = 0; i < 6; i++) {
		vertices[i * 3 + 0] = v[i][0];
		vertices[i * 3 + 1] = v[i][1];
		vertices[i * 3 + 2] = v[i][2];
	}

	/* create first 8 nodes */
	index = 1;
	newnode(nodes, 1, 5, 2,  8, 0, &index);	/* S0	*/
	newnode(nodes, 2, 5, 3,  9, 0, &index);	/* S1	*/
	newnode(nodes, 3, 5, 4, 10, 0, &index);	/* S2	*/
	newnode(nodes, 4, 5, 1, 11, 0, &index);	/* S3	*/
	newnode(nodes, 1, 0, 4, 12, 0, &index);	/* N0	*/
	newnode(nodes, 4, 0, 3, 13, 0, &index);	/* N1	*/
	newnode(nodes, 3, 0, 2, 14, 0, &index);	/* N2	*/
	newnode(nodes, 2, 0, 1, 15, 0, &index);	/* N3	*/

	layerlevel = 0;
	while (level-- > 0) {
		
		subdivide(&layers[layerlevel], vertices, nodes);
		newlayer(&layers[layerlevel+1], &layers[layerlevel],
			 nodes, &index);

		layerlevel++;
	}

	sortindex(nodes, nnodes + 1);

	free(layers);
}

static void
subdivide(layer_t *layer, double *vertices, ri_htm_node_t *nodes)
{
	int i;
	int index;
	edge_t *edges;
	edge_t **ltab;

	edges = (edge_t *)malloc(sizeof(edge_t) * (layer->nedges + 1));
	if (!edges) {
		fprintf(stderr, "muda muda\n");
		exit(-1);
	}

	ltab = (edge_t **)malloc(sizeof(edge_t *) * (layer->nvertices * 6));
	if (!ltab) {
		fprintf(stderr, "muda muda\n");
		exit(-1);
	}

	for (i = 0; i < layer->nvertices * 6; i++) {
		ltab[i] = NULL;
	}

	index = layer->nvertices;
	
	makemidpoints(layer, vertices, edges, ltab, nodes, index);


	free(edges);
	free(ltab);
}

static void
makemidpoints(layer_t *layer, double *vertices, edge_t *edges, edge_t **ltab,
	      ri_htm_node_t *nodes, int accumindex)
{
	int i;
	int c = 0;
	int index;
	int tmpindex;

	index = layer->firstindex;

	tmpindex = accumindex;

	for (i = 0; i < layer->nnodes; i++, index++) {
		c = newedge(vertices, edges, ltab, nodes, c, index, 0,
			    &tmpindex);
		c = newedge(vertices, edges, ltab, nodes, c, index, 1,
			    &tmpindex);
		c = newedge(vertices, edges, ltab, nodes, c, index, 2,
			    &tmpindex);
	}
}

static int
newedge(double *vertices, edge_t *edges, edge_t **ltab, ri_htm_node_t *nodes,
	int emindex, int index, int k, int *accumindex)
{
	edge_t *en, *em;
	int swap;
	em = &(edges[emindex]);

	switch (k) {
		case 0:
			em->start = nodes[index].v[1];
			em->end   = nodes[index].v[2];
			break;
		case 1:
			em->start = nodes[index].v[0];
			em->end   = nodes[index].v[2];
			break;
		case 2:
			em->start = nodes[index].v[0];
			em->end   = nodes[index].v[1];
			break;
	}

	/* sort the vertices by increasing index */
	if (em->start > em->end) {
		swap      = em->start;
		em->start = em->end;
		em->end   = swap;	
	}

	if ((en = edgematch(ltab, em)) != NULL) {
		nodes[index].w[k] = en->mid;
		return emindex;
	}

	insertlookup(ltab, em);
	nodes[index].w[k] = getmidpoint(vertices, em, accumindex);
	em->mid = nodes[index].w[k];

	return (emindex + 1);
}

static void
insertlookup(edge_t **ltab, edge_t *em)
{
	int i;
	int j = 6 * em->start;

	for (i = 0; i < 6; i++, j++) {
		if (ltab[j] == NULL) {
			ltab[j] = em;
			return;
		}
	}
}

static edge_t *
edgematch(edge_t **ltab, edge_t *em)
{
	int i = 6 * em->start;
	while (ltab[i] != NULL) {
		if (em->end == ltab[i]->end) return ltab[i];
		i++;
	}

	return NULL;
}

static int
getmidpoint(double *vertices, edge_t *em, int *index)
{
	int i;

	for (i = 0; i < 3; i++) {
		vertices[3 * (*index) + i] = vertices[3 * em->start + i] +
					     vertices[3 * em->end   + i];
	}

	normalize(&vertices[3 * (*index)]);

	if ((fabs(vertices[3 * (*index) + 0]) < 0.0001) &&
	    (fabs(vertices[3 * (*index) + 1]) < 0.0001) &&
	    (fabs(vertices[3 * (*index) + 2]) < 0.0001)) {
		printf("???: zero!\n");
	}

	(*index)++;

	return ((*index) - 1);
}

static void
calc_size(int level, int *nnodes, int *nvertices)
{
	/* start from octahedron. */
	int nv = 6;
	int ne = 12;
	int nf = 8;
	int i = level;

	(*nnodes) = nf;
	
	while (i-- > 0) {
		nv += ne;
		nf *= 4;
		ne  = nf + nv - 2;
		(*nnodes) += nf;
	}

	(*nvertices) = nv;

	// stored_leaves = nf;
	// i = maxlevel - buildlevel;
	// while (i-- > 0) {
	//	nf *= 4;
	// leaves = nf;
}

static int
newnode(ri_htm_node_t *node, int v0, int v1, int v2, int id, int parent, int *index)
{
	node[(*index)].v[0] = v0;
	node[(*index)].v[1] = v1;
	node[(*index)].v[2] = v2;

	node[(*index)].w[0] = 0;
	node[(*index)].w[1] = 0;
	node[(*index)].w[2] = 0;

	node[(*index)].child[0] = 0; 
	node[(*index)].child[1] = 0;
	node[(*index)].child[2] = 0;
	node[(*index)].child[3] = 0;

	node[(*index)].id     = id;
	node[(*index)].index  = (*index);
	node[(*index)].parent = parent;
	node[(*index)].npoints = 0;

	(*index)++;

	return ((*index) - 1);
}

static void
cross3(double out[3], double v0[3], double v1[3])
{
	out[0] = v0[1] * v1[2] - v0[2] * v1[1];
	out[1] = v0[2] * v1[0] - v0[0] * v1[2];
	out[2] = v0[0] * v1[1] - v0[1] * v1[0];

	normalize(out);
}

static void
normalize(double v[3])
{
	double len;

	len = sqrt(dot3(v, v));
	if (len != 0.0) len = 1.0 / len;

	v[0] *= len;
	v[1] *= len;
	v[2] *= len;
}

static void
newlayer(layer_t *newl, layer_t *oldl, ri_htm_node_t *nodes, int *index)
{
	int i, id;
	int offset;

	newl->level       = oldl->level + 1;
	newl->nvertices   = oldl->nvertices + oldl->nedges;
	newl->nnodes      = oldl->nnodes * 4;
	newl->nedges      = newl->nnodes + newl->nvertices - 2;
	newl->firstindex  = (*index);
	newl->firstvertex = oldl->firstvertex + oldl->nvertices;

	offset = oldl->firstindex;

	for (i = offset; i < offset + oldl->nnodes; i++) {
		id = nodes[i].id << 2;
		nodes[i].child[0] = newnode(nodes,
					    nodes[i].v[0],
					    nodes[i].w[2],
					    nodes[i].w[1],
					    id++, i, index);
		nodes[i].child[1] = newnode(nodes,
					    nodes[i].v[1],
					    nodes[i].w[0],
					    nodes[i].w[2],
					    id++, i, index);
		nodes[i].child[2] = newnode(nodes,
					    nodes[i].v[2],
					    nodes[i].w[1],
					    nodes[i].w[0],
					    id++, i, index);
		nodes[i].child[3] = newnode(nodes,
					    nodes[i].w[0],
					    nodes[i].w[1],
					    nodes[i].w[2],
					    id, i, index);
	}
}

/* sort the index so that the first node is the invalid node
 * (index 0), the next 8 nodes are the root nodes
 * and then we put all the leaf nodes int the following block
 * int *ascending* id-order.
 */
static void
sortindex(ri_htm_node_t *nodes, int nnodes)
{
	const int offset = 9;
	int i, j;
	int nonleaf;
	int leaf;
	int pid;
	ri_htm_node_t *oldnodes;

	oldnodes = (ri_htm_node_t *)malloc(sizeof(ri_htm_node_t) * nnodes);
	ri_mem_copy(oldnodes, nodes, sizeof(ri_htm_node_t) * nnodes);

	for (i = offset, leaf = offset, nonleaf = nnodes - 1;
	     i < nnodes;
	     i++) {
		if (oldnodes[i].child[0] == 0) {	/* child node */
			ri_mem_copy(&nodes[leaf], &oldnodes[i],
				    sizeof(ri_htm_node_t));

			for (j = 0; j < 4; j++) {
				pid = nodes[leaf].parent;
				if (nodes[pid].child[j] == i) {
					nodes[pid].child[j] = leaf;

					break;
				}	
			}

			leaf++;
		} else {
			ri_mem_copy(&nodes[nonleaf], &oldnodes[i],
				    sizeof(ri_htm_node_t));
			oldnodes[nodes[nonleaf].child[0]].parent = nonleaf;
			oldnodes[nodes[nonleaf].child[1]].parent = nonleaf;
			oldnodes[nodes[nonleaf].child[2]].parent = nonleaf;
			oldnodes[nodes[nonleaf].child[3]].parent = nonleaf;

			for (j = 0; j < 4; j++) {
				pid = nodes[nonleaf].parent;
				if (nodes[pid].child[j] == i) {
					nodes[pid].child[j] = nonleaf;
					break;
				}	

			}

			nonleaf--;

		}
	}
}

#if 0
/* find a leaf node where a vector points to */
static int
id_by_point(double *vertices, ri_htm_node_t *nodes, double v[3])
{
	const double eps = 1.0e-15;

	int index, i;
	int oldindex;
	double *v0, *v1, *v2;
	double cross[3];

	/* start with the 8 root triangles, find the one which v points to */
	for (index = 1; index <= 8; index++) {
		v0 = &(vertices[3 * nodes[index].v[0]]);
		v1 = &(vertices[3 * nodes[index].v[1]]);
		v2 = &(vertices[3 * nodes[index].v[2]]);

		cross3(cross, v0, v1);
		if (dot3(cross, v) < -eps) continue;

		cross3(cross, v1, v2);
		if (dot3(cross, v) < -eps) continue;

		cross3(cross, v2, v0);
		if (dot3(cross, v) < -eps) continue;

		break;
	}
		
	/* loop through matching child until leaves are reached */
	while (nodes[index].child[0] != 0) {
		oldindex = index;
		for (i = 0; i < 4; i++) {
			index = nodes[oldindex].child[i];

			v0 = &(vertices[3 * nodes[index].v[0]]);
			v1 = &(vertices[3 * nodes[index].v[1]]);
			v2 = &(vertices[3 * nodes[index].v[2]]);

			cross3(cross, v0, v1);
			if (dot3(cross, v) < -eps) continue;

			cross3(cross, v1, v2);
			if (dot3(cross, v) < -eps) continue;

			cross3(cross, v2, v0);
			if (dot3(cross, v) < -eps) continue;
			
			break;
		}
	}

	//return nodes[index].id;
	return index;

}
#endif

static int
st_inside(double v0[3], double v1[3], double v2[3], double query[3])
{
	const double eps = 1.0e-15;
	double cross[3];


	cross3(cross, v0, v1);
	if (dot3(cross, query) < -eps) return 0;

	cross3(cross, v1, v2);
	if (dot3(cross, query) < -eps) return 0; 

	cross3(cross, v2, v0);
	if (dot3(cross, query) < -eps) return 0;

	return 1;
}

static void
contain(ri_htm_node_t *nodes, int nnodes, double *vertices, int nvertices, double *points, int npoints)
{
	int i, j;
	double *v0, *v1, *v2, *query;

	(void)nvertices;

	for (i = 1; i < nnodes; i++) {
		if (nodes[i].child[0] != 0) continue;	/* non-leaf */

		if (nodes[i].npoints >= 8) {
			printf("??? npoints >= 8\n");
			continue;
		}

		v0 = &(vertices[3 * nodes[i].v[0]]);
		v1 = &(vertices[3 * nodes[i].v[1]]);
		v2 = &(vertices[3 * nodes[i].v[2]]);

		for (j = 0; j < npoints; j++) {
			query = &(points[3 * j]);
			if (st_inside(v0, v1, v2, query)) {
				nodes[i].p[nodes[i].npoints] = j;
				nodes[i].npoints++;
			}
		}
	}
}

static unsigned char
occlusion_hierarchy_trav(ri_htm_t *htm, ri_htm_occlusion_t *occnode, int parent)
{
	int i;
	unsigned char occlusion;

	if (htm->nodes[parent].child[0] == 0) {	/* parent is leaf node */
		occlusion = 0;
		for (i = 0; i < htm->nodes[parent].npoints; i++) {
			if (occnode->nodes[parent].occlusions[i]) {
				occlusion = 1;
				break;
			}
		}

		occnode->nodes[parent].nodeocclusion = occlusion;

		return occlusion;
	}

	/* non-leaf node */
 
	occlusion = 0;

	/* traverse child node */
	for (i = 0; i < 4; i++) {
		occlusion |= occlusion_hierarchy_trav(
						htm,
						occnode,
						htm->nodes[parent].child[i]);
	}

	occnode->nodes[parent].nodeocclusion = occlusion;

	return occlusion;
}
