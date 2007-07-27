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
#include "log.h"

#include "ri.h"

#include "apitable.h"
#include "attribute.h"
#include "context.h"

#include "geom.h"
#include "render.h"
#include "subdivision.h"

typedef struct _hashdata_t
{
	int pair;
	int created;
} hashdata_t;

static void add_face_point(ri_subd_t *subd,
			   const ri_array_t *old_vertex,
			   const ri_subd_face_t *fp);
static void add_edge_point(ri_subd_t *subd,
			   const ri_array_t *old_vertex,
			   int a, int b, int c, int d, int id);
static void refine_vertex_point(ri_subd_t *subd,
				const ri_array_t *old_vertex,
				const ri_array_t *old_face);
static void detect_boundary(ri_ptr_array_t *edgelist, int id1, int id2);

static void init_hash(int num);
static void delete_hash();
static int  check(int a, int b, int c);

static ri_ptr_array_t *hashlist = NULL;


ri_subd_t *
ri_subd_new()
{
	ri_subd_t *p;

	p = (ri_subd_t *)ri_mem_alloc(sizeof(ri_subd_t));

	p->vertex = ri_array_new(sizeof(ri_subd_vertex_t));
	p->face   = ri_array_new(sizeof(ri_subd_face_t));
	p->edges  = ri_ptr_array_new();

	return p;
}

void
ri_subd_delete(ri_subd_t *subd)
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

void
ri_subd_subdivide(ri_subd_t *new_subd, const ri_subd_t *old_subd)
{
	ri_subd_face_t *CF;
	ri_subd_face_t  f;
	ri_subd_vertex_t v;
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
    
		CF = (ri_subd_face_t *)ri_array_at(old_subd->face, i);
    
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


/* ===========================================================================
 *
 * Private functions
 *
 * ========================================================================= */


static void add_edge_point(ri_subd_t *subd,
			   const ri_array_t *old_vertex,
			   int a, int b, int c, int d, int ID)
{
	int            i;
	ri_subd_vertex_t       v;
	ri_array_t    *e;
	ri_subd_vertex_t      *va, *vb, *vc, *vd, *vid;
	ri_subd_edge_t        *edge;
	unsigned char  Boundary = 0;
  
	va = (ri_subd_vertex_t *)ri_array_at(old_vertex, a); 
	vb = (ri_subd_vertex_t *)ri_array_at(old_vertex, b); 
	vc = (ri_subd_vertex_t *)ri_array_at(old_vertex, c); 
	vd = (ri_subd_vertex_t *)ri_array_at(old_vertex, d); 

	if(ID == -1){
    
		e = (ri_array_t *)ri_ptr_array_at(subd->edges, a);
		
		for (i = 0; i < (int)e->nelems; i++) {
			edge = (ri_subd_edge_t *)ri_array_at(e, i);
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
    
		vid = (ri_subd_vertex_t *)ri_array_at(subd->vertex, ID);

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
	ri_subd_edge_t     *p;
	ri_subd_edge_t      new_edge;

	edges1 = (ri_array_t *)ri_ptr_array_at(edgelist, id1);
	edges2 = (ri_array_t *)ri_ptr_array_at(edgelist, id2);

	for(i = 0; i < (int)edges1->nelems; i++){
		p = (ri_subd_edge_t *)ri_array_at(edges1, i);

		if(p->pair == id2 ){
			p->boundary = 0;

			for(j = 0; j < (int)edges2->nelems; j++){
				p = (ri_subd_edge_t *)ri_array_at(edges2, j);

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
refine_vertex_point(ri_subd_t *subd,
		    const ri_array_t *old_vertex, const ri_array_t *old_face)
{
	int i, j;
	ri_subd_face_t *fp;
	ri_subd_edge_t *ep;
	ri_subd_vertex_t *vp, *vi, *vb0, *vb1;

	int valence;
	double beta1, beta2;
	double tempA[3], tempR[3];

	ri_ptr_array_t *adjalist;
	ri_ptr_array_t *ringlist;
	ri_array_t     *edges;
	ri_array_t     *p, *ap, *rp;
	int             BoundaryID[2], base; // for boundary
	int            *app, *rpp;
	ri_subd_vertex_t       *apv, *rpv;
    

	adjalist = ri_ptr_array_new();
	ringlist = ri_ptr_array_new();
	for (i = 0; i < (int)old_vertex->nelems; i++) {
		p = ri_array_new(sizeof(ri_subd_edge_t));
		ri_ptr_array_insert(subd->edges, i, p);
		p = ri_array_new(sizeof(int));
		ri_ptr_array_insert(adjalist, i, p);
		p = ri_array_new(sizeof(int));
		ri_ptr_array_insert(ringlist, i, p);
	}

	for(i = 0; i < (int)old_face->nelems; i++){
		fp = (ri_subd_face_t *)ri_array_at(old_face, i);

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
			ep = (ri_subd_edge_t *)ri_array_at(edges, j);
			if(ep->boundary) {
				BoundaryID[base++] =ep->pair;
			}
		}

		if(base){ // Boundary Operation
      
			vp  = (ri_subd_vertex_t *)ri_array_at(subd->vertex, i);
			vi  = (ri_subd_vertex_t *)ri_array_at(old_vertex, i);
			vb0 = (ri_subd_vertex_t *)ri_array_at(old_vertex, BoundaryID[0]);
			vb1 = (ri_subd_vertex_t *)ri_array_at(old_vertex, BoundaryID[1]);

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
			vp = (ri_subd_vertex_t *)ri_array_at(subd->vertex, i);
			vi = (ri_subd_vertex_t *)ri_array_at(old_vertex, i);

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

				apv = (ri_subd_vertex_t *)ri_array_at(old_vertex, *app);
				rpv = (ri_subd_vertex_t *)ri_array_at(old_vertex, *rpp);

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
add_face_point(ri_subd_t *subd, const ri_array_t *old_vertex, const ri_subd_face_t *fp)
{
	int       i;
	ri_subd_vertex_t *vp[4];
	ri_subd_vertex_t  v;
	
	for (i = 0; i < 4; i++) {
		vp[i] = (ri_subd_vertex_t *)ri_array_at(old_vertex, fp->v_id[i]);
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
