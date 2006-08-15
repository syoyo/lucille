/*
 * Subdivision surface(Catmull-Clark) implementation
 *
 * Original C++ by Yusuke Yasui, Converted to C by Syoyo Fujita.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "vector.h"
#include "ri.h"
#include "log.h"
#include "apitable.h"
#include "attribute.h"
#include "geom.h"
#include "render.h"
#include "context.h"

#define MAXSUBDIVLEVEL 4

typedef struct _vertex_t{
	double p[3]; 
	double st[2];
} vertex_t;

typedef struct _face_t {
	int v_id[4];
} face_t;

typedef struct _edge_t {
	int           pair;
	unsigned char boundary;
} edge_t;

typedef struct _hashdata_t
{
	int pair;
	int created;
} hashdata_t;

typedef struct _subd_t
{
	ri_array_t *vertex;
	ri_array_t *face;
	ri_ptr_array_t *edges;
} subd_t;

static subd_t *subd_new();
static void    subd_free(subd_t *subd);

static void read_from_RIB(ri_array_t *vertout, ri_array_t *faceout,
			  int nfaces, int indices[], void *vertices, void *sts);
static void add_face_point(subd_t *subd,
			   const ri_array_t *old_vertex,
			   const face_t *fp);
static void add_edge_point(subd_t *subd,
			   const ri_array_t *old_vertex,
			   int a, int b, int c, int d, int id);
static void refine_vertex_point(subd_t *subd,
				const ri_array_t *old_vertex,
				const ri_array_t *old_face);
static void detect_boundary(ri_ptr_array_t *edgelist, int id1, int id2);
static void subdivide(subd_t *new_subd, const subd_t *old_subd);

static void calc_vertex_normal(ri_vector_t   *normals,	/* output */
			       ri_vector_t   *vertices,
		               int            nvertices,	
			       unsigned int  *indices,
		               int            nindices);
static double calc_area(const ri_vector_t *v0,
			const ri_vector_t *v1,
			const ri_vector_t *v2);
static void init_hash(int num);
static void delete_hash();
static int  check(int a, int b, int c);

static ri_ptr_array_t *hashlist = NULL;


void
ri_api_subdivision_mesh(RtToken scheme,
		        RtInt nfaces, RtInt nvertices[], RtInt vertices[],
		        RtInt ntags, RtToken tags[],
		        RtInt nargs[], RtInt intargs[], RtFloat floatargs[],
		        RtInt n, RtToken tokens[], RtPointer params[])
{
	int        level;
	int        i;

	RtPointer  p_param = NULL;
	RtPointer  st_param = NULL;
	subd_t    *mesh;			/* input mesh */
	subd_t    *subd[MAXSUBDIVLEVEL];	/* Subdivision mesh. */
	face_t    *fp;
	vertex_t  *vp;

	ri_context_t   *ctx;
	ri_attribute_t *attr;
	ri_geom_t      *geom;
	int             rh;
	unsigned int    offset;
	unsigned int    nv;
	unsigned int   *indices;
	unsigned int    nindices;
	ri_matrix_t    *m;
	ri_matrix_t     om;
	ri_matrix_t     orientation;
	int             two_sided;
	ri_vector_t    *vlists;
	ri_vector_t    *nlists;
	ri_vector_t     v;
	unsigned int    npoints;

	(void)ntags;
	(void)tags;
	(void)nargs;
	(void)intargs;
	(void)floatargs;
	
	if (strcmp(scheme, "catmull-clark") != 0) {
		ri_log(LOG_WARN, "Currently supports only Catmull-Clark subdivision scheme");
		return;
	}

	for (i = 0; i < nfaces; i++) {
		if (nvertices[i] != 4) {
			ri_log(LOG_WARN, "Currently supports only quad faces");
			return;
		}
	}

	for (i = 0; i < n; i++) {
		if (strcmp(tokens[i], RI_P) == 0) {
			p_param = params[i];
		} else if (strcmp(tokens[i], RI_ST) == 0) {
			st_param = params[i];
		}
	}

	if (!p_param) {
		ri_log(LOG_WARN, "no RI_P in input");
		return;
	}

	mesh = subd_new();
	read_from_RIB(mesh->vertex, mesh->face, nfaces, vertices,
		      p_param, st_param);

	/* refine! */
	for (i = 0; i < MAXSUBDIVLEVEL; i++) {
		subd[i] = subd_new();
		if (i == 0) {
			subdivide(subd[i], mesh);
			subd_free(mesh);
		} else {
			subdivide(subd[i], subd[i - 1]);
			subd_free(subd[i - 1]);
		}
	}

	level = MAXSUBDIVLEVEL - 1;

	ctx = ri_render_get()->context;
	attr = (ri_attribute_t *)ri_stack_get(ctx->attr_stack);

	if (attr->sides == 2) two_sided = 1;
	else		      two_sided = 0;

	/* TODO: Implement two sided face. */

	/* Get modelview matrix. */
	m = (ri_matrix_t *)ri_stack_get(ctx->trans_stack);

	/* Build orientation matrix. */
	ri_matrix_identity(&orientation);

	if (strcmp(ri_render_get()->context->option->orientation, RI_RH) == 0) {
		rh = 1;
	} else {
		rh = 0;
	}

	if (rh) {
		orientation.e[2][2] = -orientation.e[2][2];
	}

	/* om = orientation . modelview */
	ri_matrix_mul(&om, m, &orientation);

	if (two_sided) {
		nindices = subd[level]->face->nelems * 6 * 2;
	} else {
		nindices = subd[level]->face->nelems * 6;
	}

	indices = (unsigned int *)ri_mem_alloc(sizeof(unsigned int) * nindices);
	
	for (i = 0; i < (int)subd[level]->face->nelems; i++) {
		fp = (face_t *)ri_array_at(subd[level]->face, i);

		if (rh) {
			indices[6 * i + 0] = fp->v_id[2];	
			indices[6 * i + 1] = fp->v_id[1];	
			indices[6 * i + 2] = fp->v_id[0];	
			indices[6 * i + 3] = fp->v_id[3];	
			indices[6 * i + 4] = fp->v_id[2];	
			indices[6 * i + 5] = fp->v_id[0];	

			if (two_sided) {
				offset = subd[level]->face->nelems;
				nv     = subd[level]->vertex->nelems;

				indices[6 * (i+offset) + 0] = fp->v_id[0] + nv;	
				indices[6 * (i+offset) + 1] = fp->v_id[1] + nv;	
				indices[6 * (i+offset) + 2] = fp->v_id[2] + nv;	
				indices[6 * (i+offset) + 3] = fp->v_id[0] + nv;
				indices[6 * (i+offset) + 4] = fp->v_id[2] + nv;	
				indices[6 * (i+offset) + 5] = fp->v_id[3] + nv;	
			}
		} else {
			indices[6 * i + 0] =  fp->v_id[0];	
			indices[6 * i + 1] =  fp->v_id[1];	
			indices[6 * i + 2] =  fp->v_id[2];	
			indices[6 * i + 3] =  fp->v_id[0];	
			indices[6 * i + 4] =  fp->v_id[2];	
			indices[6 * i + 5] =  fp->v_id[3];	

			if (two_sided) {
				offset = subd[level]->face->nelems;
				nv     = subd[level]->vertex->nelems;

				indices[6 * (i+offset) + 0] = fp->v_id[2] + nv;	
				indices[6 * (i+offset) + 1] = fp->v_id[1] + nv;	
				indices[6 * (i+offset) + 2] = fp->v_id[0] + nv;	
				indices[6 * (i+offset) + 3] = fp->v_id[3] + nv;	
				indices[6 * (i+offset) + 4] = fp->v_id[2] + nv;	
				indices[6 * (i+offset) + 5] = fp->v_id[0] + nv;	
			}
		}
	} 

	if (two_sided) {
		npoints = subd[level]->vertex->nelems * 2;
	} else {
		npoints = subd[level]->vertex->nelems;
	}

	vlists = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * npoints);

	for (i = 0; i < (int)subd[level]->vertex->nelems; i++) {
		vp = (vertex_t *)ri_array_at(subd[level]->vertex, i);
		v.e[0] = (float)vp->p[0];
		v.e[1] = (float)vp->p[1];
		v.e[2] = (float)vp->p[2];
		v.e[3] = 1.0;

		/* object space to world space. */
		ri_vector_transform(&(vlists[i]), &v, &om);

		if (two_sided) {
			offset = subd[level]->vertex->nelems;
			ri_vector_transform(&(vlists[i + offset]), &v, &om);
		}
	}

	nlists = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * npoints);

	calc_vertex_normal(nlists, vlists, npoints, indices, nindices);

	geom = ri_geom_new();
	
	ri_geom_add_positions(geom, npoints, vlists);
	ri_geom_add_normals(geom, npoints, nlists);
	ri_geom_add_indices(geom, nindices, indices);

	if (attr->surface) {
		geom->shadername = strdup(attr->surface);
	}

	if (attr->shader) {
		geom->shader = ri_shader_dup(attr->shader);
	}

	if (attr->material) {
		if (!geom->material) geom->material = ri_material_new();
		ri_material_copy(geom->material, attr->material);
	}

	geom->two_side = two_sided;

	ri_render_add_geom(ri_render_get(), geom);

	subd_free(subd[level]);

	ri_mem_free(indices);
	ri_mem_free(vlists);
	ri_mem_free(nlists);
}
		      
static void subdivide(subd_t *new_subd, const subd_t *old_subd)
{
	face_t *CF;
	face_t  f;
	vertex_t v;
	int i, NewID, flag;
	int t0[4], t1[4], t2[4], t3[4];

	v.p[0] = v.p[1] = v.p[2] = 0.0;

	for (i = 0; i < (int)old_subd->vertex->nelems; i++) {
		ri_array_insert(new_subd->vertex, i, &v);
	}

	init_hash(old_subd->vertex->nelems);

	NewID = old_subd->vertex->nelems;

	refine_vertex_point(new_subd, old_subd->vertex, old_subd->face);

	for(i = 0; i < (int)old_subd->face->nelems; i++){
    
		CF = (face_t *)ri_array_at(old_subd->face, i);
    
		t0[0] = CF->v_id[0];
		t1[0] = CF->v_id[1];
		t2[0] = CF->v_id[2];
		t3[0] = CF->v_id[3];
    
		t0[2] = t1[2] = t2[2] = t3[2] = NewID++;

		add_face_point(new_subd, old_subd->vertex, CF);

		flag = check(CF->v_id[0], CF->v_id[1], NewID);
		if(flag == -1){
			t0[1] = t1[3] = NewID++;
			add_edge_point(new_subd, old_subd->vertex,
				       CF->v_id[0], CF->v_id[1],
				       CF->v_id[2], CF->v_id[3], -1);
		}else{
			t0[1] = t1[3] = flag;
			add_edge_point(new_subd, old_subd->vertex,
				       CF->v_id[0], CF->v_id[1],
				       CF->v_id[2], CF->v_id[3], flag);
		}

		flag = check(CF->v_id[1], CF->v_id[2], NewID);
		if(flag == -1){
			t1[1] = t2[3] = NewID++;
			add_edge_point(new_subd, old_subd->vertex,
				       CF->v_id[1], CF->v_id[2],
				       CF->v_id[3], CF->v_id[0], -1);
		}else{
			t1[1] = t2[3] = flag;
			add_edge_point(new_subd, old_subd->vertex,
				       CF->v_id[1], CF->v_id[2],
				       CF->v_id[3], CF->v_id[0], flag);
		}
		  
		flag = check(CF->v_id[2], CF->v_id[3], NewID);
		if(flag == -1){
			t2[1] = t3[3] = NewID++;
			add_edge_point(new_subd, old_subd->vertex,
				       CF->v_id[2], CF->v_id[3],
				       CF->v_id[0], CF->v_id[1], -1);
		}else{
			t2[1] = t3[3] = flag;
			add_edge_point(new_subd, old_subd->vertex,
				       CF->v_id[2], CF->v_id[3],
				       CF->v_id[0], CF->v_id[1], flag);
		}

		flag = check(CF->v_id[3], CF->v_id[0], NewID);
		if(flag == -1){
			t3[1] = t0[3] = NewID++;
			add_edge_point(new_subd, old_subd->vertex,
				       CF->v_id[3], CF->v_id[0],
				       CF->v_id[1], CF->v_id[2], -1);
		}else{
			t3[1] = t0[3] = flag;
			add_edge_point(new_subd, old_subd->vertex,
				       CF->v_id[3], CF->v_id[0],
				       CF->v_id[1], CF->v_id[2], flag);
		}

		f.v_id[0] = t0[0];
		f.v_id[1] = t0[1];
		f.v_id[2] = t0[2];
		f.v_id[3] = t0[3];
		ri_array_insert(new_subd->face, new_subd->face->nelems,
				&f);
		f.v_id[0] = t1[0];
		f.v_id[1] = t1[1];
		f.v_id[2] = t1[2];
		f.v_id[3] = t1[3];
		ri_array_insert(new_subd->face, new_subd->face->nelems,
				&f);
		f.v_id[0] = t2[0];
		f.v_id[1] = t2[1];
		f.v_id[2] = t2[2];
		f.v_id[3] = t2[3];
		ri_array_insert(new_subd->face, new_subd->face->nelems,
				&f);
		f.v_id[0] = t3[0];
		f.v_id[1] = t3[1];
		f.v_id[2] = t3[2];
		f.v_id[3] = t3[3];
		ri_array_insert(new_subd->face, new_subd->face->nelems,
				&f);
	}

	delete_hash();
}

static void add_edge_point(subd_t *subd,
			   const ri_array_t *old_vertex,
			   int a, int b, int c, int d, int ID)
{
	int            i;
	vertex_t       v;
	ri_array_t    *e;
	vertex_t      *va, *vb, *vc, *vd, *vid;
	edge_t        *edge;
	unsigned char  Boundary = 0;
  
	va = (vertex_t *)ri_array_at(old_vertex, a); 
	vb = (vertex_t *)ri_array_at(old_vertex, b); 
	vc = (vertex_t *)ri_array_at(old_vertex, c); 
	vd = (vertex_t *)ri_array_at(old_vertex, d); 

	if(ID == -1){
    
		e = (ri_array_t *)ri_ptr_array_at(subd->edges, a);
		
		for (i = 0; i < (int)e->nelems; i++) {
			edge = (edge_t *)ri_array_at(e, i);
			if(edge->boundary && edge->pair == b){
				Boundary = 1;
				break;
			}
		}


		if(Boundary){

			v.p[0] = (va->p[0] + vb->p[0]) * 0.5;
			v.p[1] = (va->p[1] + vb->p[1]) * 0.5;
			v.p[2] = (va->p[2] + vb->p[2]) * 0.5;
      
		}else{
    
			v.p[0] = (va->p[0] + vb->p[0]) * 0.375
			       + (vc->p[0] + vd->p[0]) * 0.0625;
			v.p[1] = (va->p[1] + vb->p[1]) * 0.375
			       + (vc->p[1] + vd->p[1]) * 0.0625;
			v.p[2] = (va->p[2] + vb->p[2]) * 0.375
			       + (vc->p[2] + vd->p[2]) * 0.0625;
		} 
    
		ri_array_insert(subd->vertex, subd->vertex->nelems, &v);

	} else {
    
		vid = (vertex_t *)ri_array_at(subd->vertex, ID);

		vid->p[0] += (vc->p[0] + vd->p[0]) * 0.0625;
		vid->p[1] += (vc->p[1] + vd->p[1]) * 0.0625;
		vid->p[2] += (vc->p[2] + vd->p[2]) * 0.0625;
	}
}


static void detect_boundary(ri_ptr_array_t *edgelist, int id1, int id2)
{
	int         i, j;
	ri_array_t *edges1;
	ri_array_t *edges2;
	edge_t     *p;
	edge_t      new_edge;

	edges1 = (ri_array_t *)ri_ptr_array_at(edgelist, id1);
	edges2 = (ri_array_t *)ri_ptr_array_at(edgelist, id2);

	for(i = 0; i < (int)edges1->nelems; i++){
		p = (edge_t *)ri_array_at(edges1, i);

		if(p->pair == id2 ){
			p->boundary = 0;

			for(j = 0; j < (int)edges2->nelems; j++){
				p = (edge_t *)ri_array_at(edges2, j);

				if(p->pair == id1 ){
					p->boundary = 0;
					return;
				}
			}
		}
	}
  
	new_edge.boundary = 1;
	new_edge.pair     = id2;
	ri_array_insert(edges1, edges1->nelems, &new_edge);

	new_edge.boundary = 1;
	new_edge.pair     = id1;
	ri_array_insert(edges2, edges2->nelems, &new_edge);
}

static void
refine_vertex_point(subd_t *subd,
		    const ri_array_t *old_vertex, const ri_array_t *old_face)
{
	int i, j;
	face_t *fp;
	edge_t *ep;
	vertex_t *vp, *vi, *vb0, *vb1;

	int valence;
	double beta1, beta2;
	double tempA[3], tempR[3];

	ri_ptr_array_t *adjalist;
	ri_ptr_array_t *ringlist;
	ri_array_t     *edges;
	ri_array_t     *p, *ap, *rp;
	int             BoundaryID[2], base; // for boundary
	int            *app, *rpp;
	vertex_t       *apv, *rpv;
    

	adjalist = ri_ptr_array_new();
	ringlist = ri_ptr_array_new();
	for (i = 0; i < (int)old_vertex->nelems; i++) {
		p = ri_array_new(sizeof(edge_t));
		ri_ptr_array_insert(subd->edges, i, p);
		p = ri_array_new(sizeof(int));
		ri_ptr_array_insert(adjalist, i, p);
		p = ri_array_new(sizeof(int));
		ri_ptr_array_insert(ringlist, i, p);
	}

	for(i = 0; i < (int)old_face->nelems; i++){
		fp = (face_t *)ri_array_at(old_face, i);

		ap = (ri_array_t *)ri_ptr_array_at(adjalist, fp->v_id[0]);
		rp = (ri_array_t *)ri_ptr_array_at(ringlist, fp->v_id[0]);
		ri_array_insert(ap, ap->nelems, &(fp->v_id[1]));
		ri_array_insert(rp, rp->nelems, &(fp->v_id[2]));

		ap = (ri_array_t *)ri_ptr_array_at(adjalist, fp->v_id[1]);
		rp = (ri_array_t *)ri_ptr_array_at(ringlist, fp->v_id[1]);
		ri_array_insert(ap, ap->nelems, &(fp->v_id[2]));
		ri_array_insert(rp, rp->nelems, &(fp->v_id[3]));

		ap = (ri_array_t *)ri_ptr_array_at(adjalist, fp->v_id[2]);
		rp = (ri_array_t *)ri_ptr_array_at(ringlist, fp->v_id[2]);
		ri_array_insert(ap, ap->nelems, &(fp->v_id[3]));
		ri_array_insert(rp, rp->nelems, &(fp->v_id[0]));

		ap = (ri_array_t *)ri_ptr_array_at(adjalist, fp->v_id[3]);
		rp = (ri_array_t *)ri_ptr_array_at(ringlist, fp->v_id[3]);
		ri_array_insert(ap, ap->nelems, &(fp->v_id[0]));
		ri_array_insert(rp, rp->nelems, &(fp->v_id[1]));

		detect_boundary(subd->edges, fp->v_id[0], fp->v_id[1]);
		detect_boundary(subd->edges, fp->v_id[1], fp->v_id[2]);
		detect_boundary(subd->edges, fp->v_id[2], fp->v_id[3]);
		detect_boundary(subd->edges, fp->v_id[3], fp->v_id[0]);
	}
  
	for(i = 0; i < (int)old_vertex->nelems; i++){

		base = 0;

		edges = (ri_array_t *)ri_ptr_array_at(subd->edges, i);
		
		for(j = 0; j < (int)edges->nelems; j++) {
			ep = (edge_t *)ri_array_at(edges, j);
			if(ep->boundary) {
				BoundaryID[base++] =ep->pair;
			}
		}

		if(base){ // Boundary Operation
      
			vp  = (vertex_t *)ri_array_at(subd->vertex, i);
			vi  = (vertex_t *)ri_array_at(old_vertex, i);
			vb0 = (vertex_t *)ri_array_at(old_vertex, BoundaryID[0]);
			vb1 = (vertex_t *)ri_array_at(old_vertex, BoundaryID[1]);

			vp->p[0] = vi->p[0] * 0.75 + vb0->p[0] * 0.125
				 + vb1->p[0] * 0.125;
			vp->p[1] = vi->p[1] * 0.75 + vb0->p[1] * 0.125
				 + vb1->p[1] * 0.125;
			vp->p[2] = vi->p[2] * 0.75 + vb0->p[2] * 0.125
				 + vb1->p[2] * 0.125;
      
		}else{ // Usual Operation

			tempA[0] = tempA[1] = tempA[2] = 0.0;
			tempR[0] = tempR[1] = tempR[2] = 0.0;

			ap = (ri_array_t *)ri_ptr_array_at(adjalist, i);
			rp = (ri_array_t *)ri_ptr_array_at(ringlist, i);
			vp = (vertex_t *)ri_array_at(subd->vertex, i);
			vi = (vertex_t *)ri_array_at(old_vertex, i);

			valence = ap->nelems;
			assert(ap->nelems > 0);
			beta1 = 3.0 / (2.0 * (double)valence);
			beta2 = 1.0 / (4.0 * (double)valence);

			vp->p[0] = vi->p[0] * (1.0 - beta1 - beta2);
			vp->p[1] = vi->p[1] * (1.0 - beta1 - beta2);
			vp->p[2] = vi->p[2] * (1.0 - beta1 - beta2);

			for(j = 0; j < (int)ap->nelems; j++){

				app = (int *)ri_array_at(ap, j);
				rpp = (int *)ri_array_at(rp, j);

				apv = (vertex_t *)ri_array_at(old_vertex, *app);
				rpv = (vertex_t *)ri_array_at(old_vertex, *rpp);

				tempA[0] += apv->p[0];
				tempR[0] += rpv->p[0];
				tempA[1] += apv->p[1];
				tempR[1] += rpv->p[1];
				tempA[2] += apv->p[2];
				tempR[2] += rpv->p[2];
			}

			assert(ap->nelems > 0);
			beta1 /= (double)ap->nelems;
			beta2 /= (double)ap->nelems;

			vp->p[0] += (tempA[0] * beta1 + tempR[0] * beta2);
			vp->p[1] += (tempA[1] * beta1 + tempR[1] * beta2);
			vp->p[2] += (tempA[2] * beta1 + tempR[2] * beta2);
		}
	}

	for (i = 0; i < (int)old_vertex->nelems; i++) {
		p = (ri_array_t *)ri_ptr_array_at(adjalist, i);
		ri_array_free(p);
		p = (ri_array_t *)ri_ptr_array_at(ringlist, i);
		ri_array_free(p);
	}

	ri_ptr_array_free(adjalist);
	ri_ptr_array_free(ringlist);
}

static void
add_face_point(subd_t *subd, const ri_array_t *old_vertex, const face_t *fp)
{
	int       i;
	vertex_t *vp[4];
	vertex_t  v;
	
	for (i = 0; i < 4; i++) {
		vp[i] = (vertex_t *)ri_array_at(old_vertex, fp->v_id[i]);
	}

	for (i = 0; i < 3; i++) {
		v.p[i] = (vp[0]->p[i] + vp[1]->p[i] + vp[2]->p[i] + vp[3]->p[i])
		       * 0.25;
	}

	ri_array_insert(subd->vertex, subd->vertex->nelems, &v);
}

static void
init_hash(int num)
{
	int         i;
	ri_array_t *p;

	hashlist = ri_ptr_array_new();

	for (i = 0; i < num; i++) {
		p = ri_array_new(sizeof(hashdata_t));
		ri_ptr_array_insert(hashlist, i, p);
	}
}

static void
delete_hash()
{
	int         i;
	ri_array_t *p;

	for (i = 0; i < (int)hashlist->nelems; i++) {
		p = (ri_array_t *)ri_ptr_array_at(hashlist, i);
		ri_array_free(p);
	}

	ri_ptr_array_free(hashlist);
	hashlist = NULL;
}

static int
check(int a, int b, int c)
{
	int i, temp;
	ri_array_t *h;
	hashdata_t *hd;
	hashdata_t  newhash;

	h = (ri_array_t *)ri_ptr_array_at(hashlist, a);

	if (h) {
		for (i = 0; i < (int)h->nelems; i++) {
			hd = (hashdata_t *)ri_array_at(h, i);
			if (hd->pair == b) {
				temp = hd->created;
				ri_array_remove_at(h, i);

				return temp;
			}
		}
	}

	newhash.pair    = a;
	newhash.created = c;
	h = (ri_array_t *)ri_ptr_array_at(hashlist, b);
	assert(h != NULL);
	ri_array_insert(h, h->nelems, &newhash);

	return -1;
}

/* --- private functions --- */

static subd_t *
subd_new()
{
	subd_t *p;

	p = ri_mem_alloc(sizeof(subd_t));

	p->vertex = ri_array_new(sizeof(vertex_t));
	p->face   = ri_array_new(sizeof(face_t));
	p->edges  = ri_ptr_array_new();

	return p;
}

static void
subd_free(subd_t *subd)
{
	int i;
	ri_array_t *ap;

	ri_array_free(subd->vertex);
	ri_array_free(subd->face);
	for (i = 0; i < (int)subd->edges->nelems; i++) {
		ap = (ri_array_t *)ri_ptr_array_at(subd->edges, i);
		ri_array_free(ap);
	} 
	ri_ptr_array_free(subd->edges);

	ri_mem_free(subd);
}

static void
read_from_RIB(ri_array_t *vertout, ri_array_t *faceout,
	      int nfaces, int indices[], void *vertices, void *sts )
{
	int      i;
	vertex_t v;
	face_t   f;

	int nvertices = 0;

	/* Assume quad polygon as input. */
	for (i = 0; i < nfaces * 4; i++) {
		if (nvertices < indices[i]) nvertices = indices[i];
	}

	/* because vertex index is zero-base in RIB. */
	nvertices++;

	for (i = 0; i < nvertices; i++) {
		v.p[0] = (double)((float *)vertices)[3 * i + 0];
		v.p[1] = (double)((float *)vertices)[3 * i + 1];
		v.p[2] = (double)((float *)vertices)[3 * i + 2];

		if (sts) {		/* Mesh has texture coords */
			v.st[0] = (double)((float *)sts)[2 * i + 0];
			v.st[1] = (double)((float *)sts)[2 * i + 1];
		}

		ri_array_insert(vertout, vertout->nelems, &v);

	}

	for (i = 0; i < nfaces; i++) {
		f.v_id[0] = indices[4 * i + 0]; 	
		f.v_id[1] = indices[4 * i + 1]; 	
		f.v_id[2] = indices[4 * i + 2]; 	
		f.v_id[3] = indices[4 * i + 3]; 

		ri_array_insert(faceout, faceout->nelems, &f);
	}
}

static void
calc_vertex_normal(ri_vector_t  *normals,
		   ri_vector_t  *vertices,
		   int nvertices,
		   unsigned int *indices,
		   int nindices)
{
	int     i;
	unsigned int i0, i1, i2;  
	ri_vector_t *v0, *v1, *v2;
	ri_vector_t v01, v02;
	ri_vector_t normal;
	double  area;

	/* calculate vertex normal from polygon mesh.
	 * a vertex normal is an aaverage of the face vectors with
	 * area weighting.
 	 */	

	for (i = 0; i < nvertices; i++) {
		ri_vector_zero(&(normals[i]));
	}

	for (i = 0; i < nindices / 3; i++) {
		ri_vector_zero(&normal);

		i0 = indices[3 * i + 0];
		i1 = indices[3 * i + 1];
		i2 = indices[3 * i + 2];

		v0 = &(vertices[i0]);
		v1 = &(vertices[i1]);
		v2 = &(vertices[i2]);

		ri_vector_sub(&v01, v1, v0);
		ri_vector_sub(&v02, v2, v0);
		ri_vector_cross3(&normal, &v01, &v02); 
		ri_vector_normalize(&normal);

		area = calc_area(v0, v1, v2);

		/* weight by the area of the face. */
		//ri_vector_scale(&normal, (float)area);
		ri_vector_add(&(normals[i0]), &normals[i0], &normal);
		ri_vector_add(&(normals[i1]), &normals[i1], &normal);
		ri_vector_add(&(normals[i2]), &normals[i2], &normal);
	}

	for (i = 0; i < nvertices; i++) {
		ri_vector_normalize(&(normals[i]));
	}
}

static double
calc_area(const ri_vector_t *v0, const ri_vector_t *v1, const ri_vector_t *v2)
{
	ri_vector_t v01, v02;
	ri_vector_t cross;
	
	ri_vector_sub(&v01, v1, v0);
	ri_vector_sub(&v02, v2, v0);
	ri_vector_cross3(&cross, &v01, &v02);

	return (ri_vector_length(&cross) * 0.5); 
}
