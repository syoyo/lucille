/*
 * Szesci-style KD-tree implementation
 *
 * for details, see
 * "Graphics Programming Methods"
 * ISBN 1-58450-299-1
 *
 * $Id: kdtree.c,v 1.1 2004/11/11 15:54:38 syoyo Exp $
 *
 * almost codes are from section 3-9 of "Graphics Programming Methods".
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "vector.h"
#include "geom.h"
#include "memory.h"
#include "log.h"
#include "raytrace.h"
#include "render.h"

static int g_dbg = 0;

/* change this to fit your machine architecture. */
#define KDTREE_CACHE_LINE_BYTES  64  			 /* G5 = 256 */
#define KDTREE_FLOAT_MAX         3.402823466e+38f
#define KDTREE_EPSILON           1.0e-4
#define KDTREE_BOUNDS_EPSILON    0.0
#define KDTREE_MAX_DEPTH         20

static ri_kdtree_t *kdtree_create(tri_info_t **objptrs, unsigned int nobjs);
static int kdtree_follow_children(ri_kdtree_t *kd,
		                  unsigned int *parent,
		                  unsigned int *lefty, unsigned int *righty);
static void kdtree_build(ri_kdtree_t *kd,
			 unsigned int nodeid, unsigned int *bounds_array,
	    		 unsigned int nobjs, bbox_t *limits,
			 unsigned char axis_mask,
			 unsigned int depth);
static int  kdtree_make_children(ri_kdtree_t *kd,
				 unsigned int parent,
				 unsigned int *lefty, unsigned int *righty);
static void kdtree_delete_leaves(ri_kdtree_t *kd, unsigned int node);
static void kdtree_median_sort(ri_kdtree_t *kd,
			       unsigned int *lo0, unsigned int *hi0,
			       unsigned char axis);
static int kdtree_compare(ri_kdtree_t *kd,
			  unsigned int a_index, unsigned int b_index,
			  unsigned char axis);
static unsigned int *kdtree_find_bound(ri_kdtree_t *kd,
				       unsigned int *bar,
				       unsigned int nbounds, float loc,
				       unsigned char axis);
static void tri_get_bounding_box(bbox_t *bbox, const tri_info_t *tri);
static void kdtree_dump(ri_kdtree_t *kd);
static void kdtree_dump_trav(ri_kdtree_t *kd, FILE *fp, unsigned int nodeid);
static void get_scene_bounding_box(bbox_t *bbox,
				   const tri_info_t **tris, unsigned int ntris);

ri_kdtree_t *
ri_accel_build_kdtree()
{
	unsigned int i;
	/* total number of triangles in the scene */
	unsigned long total_tris = 0;
	unsigned long ntris = 0;	/* triangle counter */
	unsigned long id[3];
	unsigned long offset;

	ri_kdtree_t *kdtree;

	ri_render_t *render = ri_render_get();	
	ri_list_t *geomitr;
	ri_geom_t *geom;
	tri_info_t *tris;
	tri_info_t **triptrs;		/* pointer table for tris */
	ri_vector_t *vecp;

	g_dbg = render->debug_mode;

	ri_log(LOG_INFO, "--- Building Kd-tree... ---");
	ri_timer_start(render->context->timer, "Kd-tree building");

	for (geomitr = ri_list_first(render->geomlist);
	     geomitr != NULL;
	     geomitr = ri_list_next(geomitr)) {
		geom = (ri_geom_t *)geomitr->data;

		total_tris += geom->nindices / 3;
	}

	if (g_dbg) printf("total_tris = %lu\n", total_tris);

	// alloc 1D triangle data
	tris = (tri_info_t *)ri_mem_alloc(sizeof(tri_info_t) * total_tris);

	// alloc pointer table for 1D triangle data
	triptrs = (tri_info_t **)ri_mem_alloc(sizeof(tri_info_t *) *
					      total_tris);

	// flatten geometry data
	ntris = 0;
	for (geomitr = ri_list_first(render->geomlist);
	     geomitr != NULL;
	     geomitr = ri_list_next(geomitr)) {
		geom = (ri_geom_t *)geomitr->data;


		for (i = 0; i < geom->nindices / 3; i++) {
			id[0] = geom->indices[3 * i + 0];
			id[1] = geom->indices[3 * i + 1];
			id[2] = geom->indices[3 * i + 2];

			offset = ntris + i;
			
			/* copy v0 */
			vecp = &(tris[offset].v[0]);
			ri_vector_copy(vecp, &(geom->positions[id[0]]));

			/* copy v1 */
			vecp = &(tris[offset].v[1]);
			ri_vector_copy(vecp, &(geom->positions[id[1]]));

			/* copy v2 */
			vecp = &(tris[offset].v[2]);
			ri_vector_copy(vecp, &(geom->positions[id[2]]));

			tris[offset].geom  = geom;
			tris[offset].index = (unsigned int )i;
		}

		ntris += geom->nindices / 3;
	}

	// assign ptr to 1D triangle data
	for (i = 0; i < total_tris; i++) {
		triptrs[i] = &(tris[i]);
	}


	/* create kd-tree */
	kdtree = kdtree_create(triptrs, total_tris);

	ri_timer_end(render->context->timer, "Kd-tree building");
	ri_log(LOG_INFO, "--- built Kd-tree. ---");

	printf("kdtree-building: elapsed = %f\n",
		ri_timer_elapsed(render->context->timer, "Kd-tree building"));

	kdtree_dump(kdtree);

	//exit(-1);

	return kdtree;
}

int
ri_kdtree_traverse(ri_kdtree_t *kd,
		   const ray_t *ray, hit_t *rec, float mint, float maxt)
{
	static const float eps_tolerance   = 1.0e-4f;
	static const float eps_drop_branch = 1.0e-3f;
	static const float eps_div_by_zero = 1.0e-5f;

	register float t;

	unsigned int  tnode = 0;
	unsigned int  left_child, right_child;
	unsigned int  near_node, far_node;
	float         cut_plane;
	unsigned char axis;
	unsigned int  bitfield;
	float         org_coord;
	float         dir_coord;

	ri_vector_t   invdir;

	int           is_hit;
	float         hit_t, hit_u, hit_v;

	tri_info_t    **leaflist;
	tri_info_t    **cpc;
	unsigned int  leaflength;


	float ray_min = mint;
	float ray_max = maxt;

	int          is_leaf;

	rec->obj = 0;
	rec->is_intersect = 0;
	rec->t = KDTREE_FLOAT_MAX;
	rec->u = 0.0;
	rec->v = 0.0;
	rec->index = 0;
	rec->geom = NULL;

	kd->stack_ptr = 0;

	// handle dirs parallel to axis here in advance
	// will not need to check later for every tnode
	if (ray->dir.e[0] > eps_div_by_zero ||
	    ray->dir.e[0] < -eps_div_by_zero) {
		invdir.e[0] = 1.0f / ray->dir.e[0];
	} else {
		invdir.e[0] = KDTREE_FLOAT_MAX;
	}

	if (ray->dir.e[1] > eps_div_by_zero ||
	    ray->dir.e[1] < -eps_div_by_zero) {
		invdir.e[1] = 1.0f / ray->dir.e[1];
	} else {
		invdir.e[1] = KDTREE_FLOAT_MAX;
	}

	if (ray->dir.e[2] > eps_div_by_zero ||
	    ray->dir.e[2] < -eps_div_by_zero) {
		invdir.e[2] = 1.0f / ray->dir.e[2];
	} else {
		invdir.e[2] = KDTREE_FLOAT_MAX;
	}

	for (;;) {
		is_leaf = !kdtree_follow_children(kd, &tnode,
						  &left_child, &right_child);

		if (is_leaf) { // leaf
			leaflist = *(tri_info_t ***)(&kd->node_table[tnode]);
			if (leaflist) {
				leaflength = (unsigned int)*leaflist;

				for (cpc = (++leaflist);
				     cpc < leaflist + leaflength;
				     cpc++) {
#if 0
					// if this triangle was not tested for this
					// ray before(mailbox test). 
					if (ray.id != (*cpc)->lastTestedRayId) {
						(*cpc)->intersect(ray, depth,
								  raymin, raymax);
					} else {
						depth = (*cpc)->lastTestedRayResult->dept;
					}
#endif

					// intersection test
					is_hit = triangle_intersect(
						&ray->org,
						&ray->dir,
						&(*cpc)->v[0],
						&(*cpc)->v[1],
						&(*cpc)->v[2],
						&hit_t, &hit_u, &hit_v);

					if (is_hit &&
					    hit_t < rec->t) {

						// check that hit point is in
						// the bounding box of current
						// leaf node

						if (hit_t > ray_min - eps_tolerance &&
						    hit_t < ray_max + eps_tolerance) {
							rec->t = hit_t;	
							rec->u = hit_u;	
							rec->v = hit_v;	
							rec->index = (*cpc)->index; 
							rec->geom = (*cpc)->geom;
							rec->is_intersect = 1;	
						}
					}
				}

				if (rec->is_intersect) {
					return 1;
				} else {
					if (kd->stack_ptr >= 0) {
						// have not found anything on this
						// branch, pop the other one from stack
						ray_min = kd->trav_stack[kd->stack_ptr].ray_min;
						ray_max = kd->trav_stack[kd->stack_ptr].ray_max;
						tnode   = kd->trav_stack[kd->stack_ptr].tnode;
						kd->stack_ptr--;
					} else {
						return 0;
					}
				}

			} else {	// leaf is empty
				if (kd->stack_ptr >= 0) {
					ray_min = kd->trav_stack[kd->stack_ptr].ray_min;
					ray_max = kd->trav_stack[kd->stack_ptr].ray_max;
					tnode   = kd->trav_stack[kd->stack_ptr].tnode;
					kd->stack_ptr--;
				} else {
					return 0;
				}
			}

		} else {
			// not a leaf, go deeper

			cut_plane = kd->node_table[tnode];
			axis = *(unsigned int *)(&cut_plane) & 0x00000003;
			bitfield = *(unsigned int*)(&kd->node_table[tnode]) &
							0xfffffffc;

			// extract upper 30 bit as float value
			cut_plane = *(float *)(&bitfield);
		
			org_coord = ray->org.e[axis];
			dir_coord = invdir.e[axis];

			t = (cut_plane - org_coord) * dir_coord;

			// origin on the left or on the plane
			if (org_coord + eps_div_by_zero < cut_plane ||
			    !(cut_plane + eps_div_by_zero < org_coord) &&
			    dir_coord < 0) {

				near_node = left_child;
				far_node  = right_child;

			} else {

				near_node = right_child;
				far_node  = left_child;

			}

			if ((t <= 0.0) || (ray_max + eps_drop_branch < t)) {
				// whole interval on near cell
				tnode = near_node;
			} else {
				if (t < ray_min - eps_drop_branch) {
					// whole interval on far cell
					tnode = far_node;
				} else {
					// intersection.
					// push far branch to stack
					kd->stack_ptr++;
					kd->trav_stack[kd->stack_ptr].ray_min =
						t;
					kd->trav_stack[kd->stack_ptr].ray_max =
						ray_max;
					kd->trav_stack[kd->stack_ptr].tnode =
						far_node;

					tnode = near_node;

					// near branch is next to traverse.
					ray_max = t;
				}
			}
		}
	}

	return 0;	// no hit point found.
}

static int
kdtree_make_children(ri_kdtree_t *kd,
		     unsigned int parent,
		     unsigned int *lefty, unsigned int *righty)
{
	unsigned int sub_pos = parent & (kd->ncachelinenodes);
	unsigned int sup_pos = parent & ~(kd->ncachelinenodes);
	unsigned int sps;

	if (g_dbg) {
		printf("\tmake_children()\n");
	}

	sub_pos = (sub_pos << 1) + 1;

	if (sub_pos < kd->ncachelinenodes) {

		(*lefty)  = (parent & ~(kd->ncachelinenodes)) | sub_pos;
		(*righty) = (parent & ~(kd->ncachelinenodes)) | (sub_pos + 1);

		return 1;

	} else {

		sps = sup_pos + sub_pos	 - kd->ncachelinenodes + 1;
		(*lefty)  = sps * (kd->ncachelinenodes + 1);
		(*righty) = (sps + 1) * (kd->ncachelinenodes + 1);

		if (sps + 1 < kd->ncachelines) {
			return 1;
		} else {
			if (kd->nfreenodes < 2) {
				if (g_dbg) printf("\tkd->nfreenodes < 2");
				return 0;
			}

			kd->nfreenodes -= 2;
			if (g_dbg) {
				printf("\tnfreenodes = %d\n", kd->nfreenodes);
			}

			(*lefty) = kd->freenodes[kd->first_freenode_index];
			kd->first_freenode_index =
				(kd->first_freenode_index + 1) %
				kd->max_freenodes;

			(*righty) = kd->freenodes[kd->first_freenode_index];
			kd->first_freenode_index =
				(kd->first_freenode_index + 1) %
				kd->max_freenodes;
		}
	}

	return 1;
}

int
kdtree_get_freenodes(ri_kdtree_t *kd,
		     unsigned int parent,
		     unsigned int *lefty, unsigned int *righty)
{
	unsigned int sub_pos = parent & (kd->ncachelinenodes);
	unsigned int sup_pos = parent & ~(kd->ncachelinenodes);
	unsigned int grand_sub_pos;
	unsigned int sps;


	if (g_dbg) printf("\tkdtree_get_freenodes()\n");
	sub_pos = (sub_pos << 1) + 1;

	if (sub_pos < kd->ncachelinenodes) {

		grand_sub_pos = ((sub_pos + 1) << 1) + 2;
	
		if (grand_sub_pos >= kd->ncachelinenodes) {
			sps = sup_pos + grand_sub_pos - kd->ncachelinenodes + 1;

			if (sps + 1 >= kd->ncachelines) {
				return 0;
			}
		}

		(*lefty)  = (parent & ~(kd->ncachelinenodes)) | sub_pos;
		(*righty) = (parent & ~(kd->ncachelinenodes)) | (sub_pos + 1);

		return 1;

	} else {

		sps = sup_pos + sub_pos - kd->ncachelinenodes + 1;

		(*lefty)  = sps * (kd->ncachelinenodes + 1);
		(*righty) = (sps + 1) * (kd->ncachelinenodes + 1);

		if (sps + 1 < kd->ncachelines) {
			return 1;
		} else {
			return 0;
		}
	}

	return 1;
}

static unsigned int
kdtree_make_pointer(ri_kdtree_t *kd, unsigned int node)
{
	unsigned int sub_pos = node & (kd->ncachelinenodes);
	unsigned int sup_pos = node & ~(kd->ncachelinenodes);
	unsigned int sps;
	unsigned int cnode;

	if (g_dbg) printf("\tmake_pointer(%d)\n", node);

	sub_pos = (sub_pos << 1) + 1;

	if (sub_pos < kd->ncachelinenodes) {
		return node;
	} else {
		sps = sup_pos + sub_pos  - kd->ncachelinenodes + 1;

		if (sps + 1 < kd->ncachelines) {
			return node;
		} else {
			if (kd->nfreenodes < 1) {
				fprintf(stderr, "???: kdtree_make_pointer\n");
				exit(-1);
			}

			*(unsigned int *)&
				(kd->node_table[node | kd->ncachelinenodes]) &=
				~(0x1 << (node & kd->ncachelinenodes));
		
			cnode = kd->freenodes[kd->first_freenode_index];
			kd->nfreenodes--;

			kd->first_freenode_index =
				(kd->first_freenode_index + 1) %
				kd->max_freenodes;
			kd->node_table[node] = *(float *)(&cnode);

			if (g_dbg) {
				printf("cnode = %d\n", cnode);
			}

			return cnode;
		}
	}
}

static int
kdtree_follow_children(ri_kdtree_t *kd,
		       unsigned int *parent,
		       unsigned int *lefty, unsigned int *righty)
{
	unsigned int sub_pos = (*parent) & (kd->ncachelinenodes);
	unsigned int sup_pos = (*parent) & ~(kd->ncachelinenodes);
	unsigned int leafbit;
	unsigned int sps, wsps;
	unsigned int wsub_pos;
	unsigned int wsup_pos;

	leafbit = ((*(unsigned int *)
			&(kd->node_table[(*parent) | kd->ncachelinenodes])) >>
			sub_pos) & 0x1;

	if (leafbit) {
		return 0;
	}

	sub_pos = (sub_pos << 1) + 1;

	if (sub_pos < kd->ncachelinenodes) {
		(*lefty)  = ((*parent) & ~(kd->ncachelinenodes)) | sub_pos;
		(*righty) = ((*parent) & ~(kd->ncachelinenodes)) | (sub_pos+1);

		return 1;
	} else {
		sps = sup_pos + sub_pos - kd->ncachelinenodes + 1;

		(*lefty)  = sps * (kd->ncachelinenodes + 1);
		(*righty) = (sps + 1) * (kd->ncachelinenodes + 1);

		if (sps + 1 < kd->ncachelines) {
			return 1;
		} else {

			(*parent) = *(int *)(kd->node_table + (*parent));

			wsub_pos = (*parent) & (kd->ncachelines);
			wsup_pos = (*parent) & ~(kd->ncachelines);
			wsub_pos = (wsub_pos << 1) + 1;

			if (wsub_pos < kd->ncachelinenodes) {
				(*lefty)  = wsup_pos | wsub_pos;
				(*righty) = wsup_pos | (wsub_pos + 1);

				return 1;
			} else {
				wsps = wsup_pos + wsub_pos -
				       kd->ncachelinenodes + 1;
				(*lefty) = wsps * (kd->ncachelinenodes + 1);
				(*righty) = (wsps + 1) *
					    (kd->ncachelinenodes + 1);		
				return 1;
			}
		}
	}
}

static void
kdtree_delete_leaves(ri_kdtree_t *kd, unsigned int node)
{
	unsigned int rc, lc;

	if (g_dbg) {
		printf("kdtree_delete_leaves: node = %d\n", node);
	}

	if (!kdtree_follow_children(kd, &node, &lc, &rc)) {
		// intersectable **leaflist = *(intersectable***)&node_table[node];
		// if (leaflist) {
		//	delete leaflist;
		//	return;
		// }
		// return;
	}

	kdtree_delete_leaves(kd, lc);

	if (kd->node_table[node] < KDTREE_FLOAT_MAX - KDTREE_EPSILON) {
		kdtree_delete_leaves(kd, rc);
	}
}

static int
kdtree_isleaf(ri_kdtree_t *kd, unsigned int node)
{
	return ((*(unsigned int *)
			&kd->node_table[node | kd->ncachelinenodes]) >>
			(node & kd->ncachelinenodes)) & 0x1;
}

static float
kdtree_get_bound_value(ri_kdtree_t *kd, unsigned int *bies, unsigned char axis)
{
	// if MSB of bies is 1, take the maximum
	// if MSB of bies is 0, take the minimum
	unsigned int index = (*bies);
	float  ret;
	bbox_t bbox;
	
	if (g_dbg) {
		if (axis > 2) {
			printf("axis = %d\n", axis);
		}
	}

	tri_get_bounding_box(&bbox, kd->objects[index & 0x7fffffff]);

	if (index & 0x80000000) {
		ret = bbox.bmax[axis];
	} else {
		ret = bbox.bmin[axis];
	}
	
	return ret;
}

static void
kdtree_build(ri_kdtree_t *kd,
	     unsigned int nodeid, unsigned int *bounds_array,
	     unsigned int nobjs, bbox_t *limits, unsigned char axis_mask,
	     unsigned int depth)
{
	// separate the axis param to axis and mask
	unsigned char axis = axis_mask & 0x7;
	unsigned char mask = axis_mask & 0x70;

	// calc some values for the cost function
	float cost_base, cost_steep;

	float *limit_min, *limit_max;
	float spatial_median;
	float object_median;
	float min_cut, max_cut;
	float min_cut_cost, max_cut_cost;

	unsigned int *spatial_median_pos;
	unsigned int *object_median_pos;
	
	unsigned int *median_interval_start;
	unsigned int *median_interval_end;

	float min_cost_found;
	float min_cost_cut;
	unsigned int *min_cost_pos;
	int min_intersected = 0;
	int min_left = 0;
	unsigned int *trace;

	unsigned int intersected_count = 0;
	unsigned int left_count = 0;

	float cut;
	float cost_if_cut_here;

	tri_info_t **leaf_list;
	tri_info_t **leaf_loader;
	unsigned int *bubo;

	unsigned int lfree, rfree;

	unsigned int bitfield;
	unsigned int left_child_made, right_child_made;

	bbox_t       diver;
	unsigned int *left_child_bounds, *right_child_bounds;
	unsigned int *copycat_left, *copycat_right;
	unsigned int *snoop;
	unsigned char turned_axis;

	unsigned int  empty;
	unsigned int  ele_left, ele_right;

	bbox_t        bbox;

	
	if (g_dbg) {
		printf("--- kdtree_build() ---\n");
		printf("\tnodeid = %d, nobjs = %d, axis_mask = %d\n",
			nodeid, nobjs, axis_mask);
		printf("\tdepth = %d\n", depth);
		printf("\tlimits = (%f, %f, %f)-(%f, %f, %f)\n",
			limits->bmin[0], limits->bmin[1], limits->bmin[2],
			limits->bmax[0], limits->bmax[1], limits->bmax[2]);
	}

	if (depth >= (unsigned int)kd->maximum_depth) {

		// terminate with here
 
		if (g_dbg) {
			printf("\t*** EXCEEDS MAXIMUM DEPTH. terminate\n");
		}

		leaf_list = (tri_info_t **)ri_mem_alloc(sizeof(tri_info_t *) *
							 (nobjs + 1));
		leaf_loader = leaf_list;

		// first pos of array stores the number of elements in array.
		*(unsigned int *)leaf_loader = nobjs;
		leaf_loader++;

		if (g_dbg) {
			printf("\t=======create leaf: num = %d\n", nobjs);
		}

		for (bubo =  bounds_array;
		     bubo < (bounds_array + (nobjs << 1));
		     bubo++) {
			if (!(*bubo & 0x80000000)) {
				*leaf_loader = *(kd->objects + *bubo);
				leaf_loader++;
			} else {
				//printf("bubo & 0x80000000\n");
			}
		}

		// make the node contain a pointer to the list
		ri_mem_copy(&(kd->node_table[nodeid]), &leaf_list,
			    sizeof(tri_info_t **));
		*(unsigned int *)
			&(kd->node_table[nodeid | kd->ncachelinenodes]) |=
			(0x1 << (nodeid & kd->ncachelinenodes));

		ri_mem_free(bounds_array);

		return;
	}


	switch (axis) {
		case 0:	/* x */
			cost_base  = (limits->bmax[2] - limits->bmin[2])
				   * (limits->bmax[1] - limits->bmin[1]);
			cost_steep = (limits->bmax[2] - limits->bmin[2])
				   + (limits->bmax[1] - limits->bmin[1]);
			break;
		case 1: /* y */
			cost_base  = (limits->bmax[0] - limits->bmin[0])
				   * (limits->bmax[2] - limits->bmin[2]);
			cost_steep = (limits->bmax[0] - limits->bmin[0])
				   + (limits->bmax[2] - limits->bmin[2]);
			break;
		case 2: /* z */
			cost_base  = (limits->bmax[0] - limits->bmin[0])
				   * (limits->bmax[1] - limits->bmin[1]);
			cost_steep = (limits->bmax[0] - limits->bmin[0])
				   + (limits->bmax[1] - limits->bmin[1]);
			break;
		default:
			printf("???\n");
	}

	if (g_dbg) {
		printf("\taxis = %d\n", axis);
		printf("\tcost_base = %f\n", cost_base);
		printf("\tcost_steep = %f\n", cost_steep);
	}

	// some shorthand for this long-long thingie
	limit_min = axis ?
		    ((axis == 1) ? &(limits->bmin[1]) : &(limits->bmin[2])) :
		    &(limits->bmin[0]);
	limit_max = axis ?
		    ((axis == 1) ? &(limits->bmax[1]) : &(limits->bmax[2])) :
		    &(limits->bmax[0]);

	// find the value for tha spatial median.... easy
	spatial_median = ((*limit_max) + (*limit_min)) / 2.0f;

	if (g_dbg) {
		printf("\tlimit_max = %f\n", (*limit_max));
		printf("\tlimit_min = %f\n", (*limit_min));
	}

	// sort the references to the extrema of the patches
	// we will need this to find the best splitting plane.
	kdtree_median_sort(kd, bounds_array,
			   bounds_array + (nobjs << 1) - 1, axis);

	// find the position in the array corresponding to the spatial median
	// value.
	spatial_median_pos = kdtree_find_bound(kd, bounds_array, nobjs << 1,
				 	spatial_median, axis);

	// find object median position(middle of array, surprise-surprise,
	// and value.
	object_median_pos = bounds_array + nobjs;

	object_median = kdtree_get_bound_value(kd, object_median_pos, axis);

	if (g_dbg) {
		printf("\tspacial_median = %f\n", spatial_median);
		printf("\tobject_median = %f\n", object_median);
	}


	tri_get_bounding_box(&bbox, kd->objects[bounds_array[0] & 0x7fffffff]);
	min_cut = bbox.bmin[axis];
	tri_get_bounding_box(&bbox,
		kd->objects[bounds_array[(nobjs << 1) - 1] & 0x7fffffff]);
	max_cut = bbox.bmax[axis];
	
	if (max_cut > (*limit_max)) {
		max_cut = (*limit_max);
	}

	if (min_cut < (*limit_min)) {
		min_cut = (*limit_min);
	}

	if (g_dbg) {
		printf("\tmin_cut = %f\n", min_cut);
		printf("\tmax_cut = %f\n", max_cut);

	}

	// find the estimated cost of ray casting after COES
	min_cut_cost = (((*limit_max) - min_cut) * cost_steep + cost_base) *
		       nobjs;
	max_cut_cost = ((max_cut - (*limit_min)) * cost_steep + cost_base) *
		       nobjs;

	if (min_cut_cost < 0.0f) {
		min_cut_cost = KDTREE_FLOAT_MAX;
	}

	if (max_cut_cost < 0.0f) {
		max_cut_cost = KDTREE_FLOAT_MAX;
	}

	// full range optimum search
	median_interval_start = bounds_array + 1;
	median_interval_end   = bounds_array + (nobjs << 1) - 1;

	if (min_cut_cost < max_cut_cost) {
		min_cost_found  = min_cut_cost;
		min_cost_pos    = bounds_array;
		min_cost_cut    = min_cut;
		min_intersected = 0;
		min_left        = 0;
	} else {
		min_cost_found  = max_cut_cost;
		min_cost_pos    = bounds_array + (nobjs << 1);
		min_cost_cut    = max_cut;
		min_intersected = 0;
		min_left        = nobjs;
	}

	if (g_dbg) {
		printf("\tmin_cost_found = %f\n", min_cost_found);
		printf("\tmin_cost_pos = %d\n", *min_cost_pos);
		printf("\tmin_cost_cut = %f\n", min_cut);
		printf("\tmin_intersected = %d\n", min_intersected);
		printf("\tmin_left = %d\n", min_left);
	}

	// start creeping... count patches
	trace = bounds_array;
	for (; trace < median_interval_start; trace++) {
		if ((*trace) & 0x80000000) {
			intersected_count--;
			left_count++;
		} else {
			intersected_count++;
		}
	}

	if (g_dbg) {
		printf("\tinterescted_count = %d\n", intersected_count);
		printf("\tleft_count = %d\n", left_count);
	}

	// here comes the section where we estimate the cost to find
	// the best cut
	for (; trace < median_interval_end; trace++) {
		cut = kdtree_get_bound_value(kd, trace, axis);
		
		if ((*trace) & 0x80000000) {
			intersected_count--;
			left_count++;
		
			cost_if_cut_here = (left_count + intersected_count) *
					   ((cut - (*limit_min)) * cost_steep +
					    cost_base) + (nobjs - left_count) *
					    (((*limit_max) - cut) * cost_steep +
					    cost_base);

			if (left_count == 0 ||
			    (left_count + intersected_count == nobjs)) {
				cost_if_cut_here = KDTREE_FLOAT_MAX;
			}

			if (cost_if_cut_here < min_cost_found) {
				min_cost_found  = cost_if_cut_here;
				min_cost_pos    = trace + 1;
				min_intersected = intersected_count;
				min_left        = left_count;
				min_cost_cut    = cut;
			}
		} else {
			cost_if_cut_here = (left_count + intersected_count) *
					   ((cut - (*limit_min)) * cost_steep +
					    cost_base) + (nobjs - left_count) *
					    (((*limit_max) - cut) * cost_steep +
					    cost_base);

			if (left_count == 0 ||
			    (left_count + intersected_count == nobjs)) {
				cost_if_cut_here = KDTREE_FLOAT_MAX;
			}

			if (cost_if_cut_here < min_cost_found) {
				min_cost_found  = cost_if_cut_here;
				min_cost_pos    = trace;
				min_intersected = intersected_count;
				min_left        = left_count;
				min_cost_cut    = cut;
			}

			intersected_count++;
		}
	}

	if (g_dbg) {
		printf("\tfound best cut ---\n");
		printf("\tmin_cost_found = %f\n", min_cost_found);
		printf("\tmin_cost_pos = %d\n", *min_cost_pos);
		printf("\tmin_cost_cut = %f\n", min_cost_cut);
		printf("\tmin_intersected = %d\n", min_intersected);
		printf("\tmin_left = %d\n", min_left);
	}

	if (g_dbg) {
		printf("\t--- cost-based test ---\n");
		printf("\tmin_cost_found = %f\n", min_cost_found * 1.0001);
		printf("\test = %f\n", nobjs*(((*limit_max)-(*limit_min))*cost_steep+cost_base));

	}
	
	// termination. cost-based
	if (min_cost_found * 1.0001 >=
		nobjs * (((*limit_max)-(*limit_min))*cost_steep+cost_base)) {
		
		// no split found, mark this axis as failed.
		mask |= (010 << axis);
		if (mask != 070 && (nobjs > 4 || min_cost_found < 10.0)) {
			// if there is still a hopeful axis,
			// do not split here, go on with the other direction
			do {
				axis = (axis + 1) % 3;
			} while (mask & (010 << axis));

			if (g_dbg) {
				printf("\tthere is still a hopeful axis\n");
			}

			kdtree_build(kd, nodeid, bounds_array, nobjs, limits,
				     mask | axis, depth);

			return;
		}

		// if all 3 have abandoned us, stop here, make a leaf
		// save leaf length and patch list
		leaf_list = (tri_info_t **)ri_mem_alloc(sizeof(tri_info_t *) *
							 (nobjs + 1));
		leaf_loader = leaf_list;

		// first pos of array stores the number of elements in array.
		*(unsigned int *)leaf_loader = nobjs;

		if (g_dbg) {
			printf("\t======createleaf: num = %d\n", nobjs);
			printf("\t======createleaf: nodeid = %d\n", nodeid);
			printf("\t======createleaf: leaf_loader = %d\n", *(unsigned int *)leaf_loader);
		}

		leaf_loader++;

		for (bubo =  bounds_array;
		     bubo < (bounds_array + (nobjs << 1));
		     bubo++) {
			if (!(*bubo & 0x80000000)) {
				(*leaf_loader) = *(kd->objects + *bubo);
				leaf_loader++;
			} else {
				//printf("*bubo & 0x80000000\n");
				//printf("*bubo = %d\n", *bubo);
			}
		}

		// make the node contain a pointer to the list
		ri_mem_copy(&(kd->node_table[nodeid]), &leaf_list,
			    sizeof(tri_info_t **));

		if (g_dbg) printf("\tterm:free: %p\n", &bounds_array); 
		ri_mem_free(bounds_array);

		// mark the node as a leaf
		*(unsigned int *)
			&(kd->node_table[nodeid | kd->ncachelinenodes]) |=
				(0x1 << (nodeid & kd->ncachelinenodes));
		

		if (kdtree_get_freenodes(kd, nodeid, &lfree, &rfree)) {
			kd->nfreenodes += 2;
			if (g_dbg) {
				printf("\tnfreenodes = %d\n", kd->nfreenodes);
			}
			kd->freenodes[kd->last_freenode_index] = lfree;
			kd->last_freenode_index =
				(kd->last_freenode_index + 1) %
				kd->max_freenodes;
			kd->freenodes[kd->last_freenode_index] = rfree;
			kd->last_freenode_index =
				(kd->last_freenode_index + 1) %
				kd->max_freenodes;

			if (g_dbg) {
				printf("nfreenodes = %d\n", kd->nfreenodes);
			}
		}

		return;
	}

	// grand another chance to previously failed directions.
	mask = 00;

	// insert a back-pointer if necessary
	nodeid = kdtree_make_pointer(kd, nodeid);

	if (g_dbg) {
		printf("\tmake_pointer:nodeid(backptr) = %d\n", nodeid);
	}

	// store the splitting plane value in the node.
	bitfield = (*(unsigned int *)(&min_cost_cut) & 0xfffffffc) | axis;
	kd->node_table[nodeid] = *(float *)(&bitfield);

	if (g_dbg) {
		printf("\tnode_table[%d] = %p\n", nodeid, &kd->node_table[nodeid]);
	}

	// mark node as a non-leaf node
	*(unsigned int *)&kd->node_table[nodeid | kd->ncachelinenodes] &=
			~(0x1 << (nodeid & kd->ncachelinenodes));

	// allocate child nodes, pass the objects on to them
	if (kdtree_make_children(kd, nodeid,
				 &left_child_made, &right_child_made)) {

		if (g_dbg) {
			printf("\tkdtree_make_children() pass\n");
			printf("\tmin_left = %d\n", min_left);
			printf("\tmin_cost_cut = %f\n", min_cost_cut);
			printf("\tmin_intersected = %d\n", min_intersected);
			printf("\tleft_child_made = %d\n", left_child_made);
			printf("\tright_child_made = %d\n", right_child_made);
		}

		if (min_left + min_intersected) {
			ri_mem_copy(&diver, limits, sizeof(bbox_t));	

			// narrower scene bouding box
			(*limit_max) = min_cost_cut;

			if (g_dbg) {
				printf("\tdiver = (%f, %f, %f)-(%f, %f, %f)\n",
					diver.bmin[0],
					diver.bmin[1],
					diver.bmin[2],
					diver.bmax[0],
					diver.bmax[1],
					diver.bmax[2]);
				printf("\tlimits = (%f, %f, %f)-(%f, %f, %f)\n",
					limits->bmin[0],
					limits->bmin[1],
					limits->bmin[2],
					limits->bmax[0],
					limits->bmax[1],
					limits->bmax[2]);
			}

			if (g_dbg) printf("\t=== left_child_bounds\n");

			left_child_bounds = (unsigned int *)
					ri_mem_alloc(sizeof(unsigned int) *
					((min_left + min_intersected) << 1));

			copycat_left = left_child_bounds;
			snoop        = bounds_array;

			if (g_dbg) {
				printf("min_cost_posNUM = %d\n", min_cost_pos - snoop);
				fflush(stdout);
			}

			for (; snoop < min_cost_pos; snoop++) {
				if (!(*snoop & 0x80000000)) {
					*copycat_left = *snoop;
					copycat_left++;
					*copycat_left = *snoop | 0x80000000;
					copycat_left++;
				}
			}

			turned_axis = axis;

			do {
				turned_axis = (turned_axis + 1) % 3;
			} while (mask & (010 << turned_axis));

			if (g_dbg) {
				printf("\tmake_children: subd_left\n");
			}

			kdtree_build(kd, left_child_made, left_child_bounds,
				     min_left + min_intersected,
				     limits, mask | turned_axis, (++depth));

			ri_mem_copy(limits, &diver, sizeof(bbox_t));	

		} else {

			if (g_dbg) {
				printf("\tleft:empty\n");
			}

			empty = left_child_made;
			//(tri_info_t **)&kd->node_table[empty] = 0x0;
			*(unsigned int *)&(kd->node_table[empty]) = 0x0;
			*(unsigned int *)
				&(kd->node_table[empty | kd->ncachelinenodes])
					|= 0x1 << (empty & kd->ncachelinenodes);

			if (kdtree_get_freenodes(kd, empty,
						 &ele_left, &ele_right)) {
				kd->nfreenodes += 2;
				if (g_dbg) {
					printf("\tnfreenodes = %d\n", kd->nfreenodes);
				}
				kd->freenodes[kd->last_freenode_index] =
					ele_left;
				kd->last_freenode_index =
					(kd->last_freenode_index + 1) %
					kd->max_freenodes;
				kd->freenodes[kd->last_freenode_index] =
					ele_right;
				kd->last_freenode_index =
					(kd->last_freenode_index + 1) %
					kd->max_freenodes;
			}
		}

		if (g_dbg) {
			printf("\tnobjs = %d, min_left = %d\n", nobjs, min_left);
		}

		if (nobjs - min_left) {

			ri_mem_copy(&diver, limits, sizeof(bbox_t));	

			if (g_dbg) printf("\t=== right_child_bounds\n");

			// narrower scene bouding box
			(*limit_min) = min_cost_cut;
			right_child_bounds = (unsigned int *)
					ri_mem_alloc(sizeof(unsigned int) *
					((nobjs - min_left) << 1));

			copycat_right = right_child_bounds;
			snoop        = min_cost_pos;

			for (; snoop < bounds_array + (nobjs << 1); snoop++) {
				if (*snoop & 0x80000000) {
					*copycat_right = *snoop & 0x7fffffff;
					copycat_right++;
					*copycat_right = *snoop;
					copycat_right++;
				}
			}

			turned_axis = axis;

			do {
				turned_axis = (turned_axis + 1) % 3;
			} while (mask & (010 << turned_axis));

			if (g_dbg) {
				printf("\tmake_children: subd_right\n");
			}

			kdtree_build(kd, right_child_made, right_child_bounds,
				     nobjs - min_left,
				     limits, mask | turned_axis, (++depth));

			ri_mem_copy(limits, &diver, sizeof(bbox_t));	

		} else {

			if (g_dbg) {
				printf("\tright:empty\n");
			}

			empty = right_child_made;
			*(unsigned int *)&kd->node_table[empty] = 0x0;
			*(unsigned int *)
				&kd->node_table[empty | kd->ncachelinenodes] |=
					0x1 << (empty & kd->ncachelinenodes);

			if (kdtree_get_freenodes(kd, empty,
						 &ele_left, &ele_right)) {
				kd->nfreenodes += 2;
				if (g_dbg) {
					printf("\tnfreenodes = %d\n", kd->nfreenodes);
				}
				kd->freenodes[kd->last_freenode_index] =
					ele_left;
				kd->last_freenode_index =
					(kd->last_freenode_index + 1) %
					kd->max_freenodes;
				kd->freenodes[kd->last_freenode_index] =
					ele_right;
				kd->last_freenode_index =
					(kd->last_freenode_index + 1) %
					kd->max_freenodes;
			}

		}
	} else { // terminate

		if (g_dbg) {
			printf("\tterminate: overflow\n");
		}

		// node table overflow

		leaf_list = (tri_info_t **)ri_mem_alloc(sizeof(tri_info_t *) *
							 (nobjs + 1));
		leaf_loader = leaf_list;

		// first pos of array stores the number of elements in array.
		*(unsigned int *)leaf_loader = nobjs;
		leaf_loader++;

		if (g_dbg) {
			printf("\t=======create leaf: num = %d\n", nobjs);
		}

		for (bubo =  bounds_array;
		     bubo < (bounds_array + (nobjs << 1));
		     bubo++) {
			if (!(*bubo & 0x80000000)) {
				*leaf_loader = *(kd->objects + *bubo);
				leaf_loader++;
			} else {
				//printf("bubo & 0x80000000\n");
			}
		}

		// make the node contain a pointer to the list
		ri_mem_copy(&(kd->node_table[nodeid]), &leaf_list,
			    sizeof(tri_info_t **));
		*(unsigned int *)
			&(kd->node_table[nodeid | kd->ncachelinenodes]) |=
			(0x1 << (nodeid & kd->ncachelinenodes));

	}

	if (g_dbg) {
		printf("kdtree_build: exit...\n");
	}

	if (g_dbg) printf("\ttexit:free: %p\n", &bounds_array); 
	ri_mem_free(bounds_array);
}

static unsigned int *
kdtree_find_bound(ri_kdtree_t *kd,
		  unsigned int *bar, unsigned int nbounds, float loc,
		  unsigned char axis)
{
	unsigned int wbounds;

	while (nbounds > 1) {
		wbounds = nbounds >> 1;

		if (kdtree_get_bound_value(kd, bar + wbounds, axis) < loc) {
			bar += wbounds;
			nbounds -= wbounds;
		} else {
			nbounds = wbounds;
		}
	}

	return bar;
}

static void
kdtree_median_sort(ri_kdtree_t *kd,
		   unsigned int *lo0, unsigned int *hi0, unsigned char axis)
{
	unsigned int *lo, *hi;
	unsigned int *mid_pos;
	unsigned int  mid_index;
	float         mid;
	unsigned int  tmp;

	if (hi0 > lo0) {
		lo = lo0; hi = hi0;

		// establishing partition element as the midpoint of the array
		mid_pos = lo0 + ( ((unsigned int)hi0 - (unsigned int)lo0) /
			         sizeof(unsigned int) / 2);

		mid_index = (*mid_pos);

		mid = kdtree_get_bound_value(kd, mid_pos, axis);

		// loop through the array until indices cross
		while (lo <= hi) {

			// find the first element that is greater than
			// or equal to the partition element starting from
			// the left index.
			while ((lo < hi0) && 
			       (kdtree_compare(kd, *lo, mid_index, axis))) {
				++lo;
			}

			// find an element that is smaller than or equal to
			// the partition element starting from the right index.
			while ((hi > lo0) &&
			       (kdtree_compare(kd, mid_index, *hi, axis))) {
				--hi;
			}

			// if the indices have not crossed, swap
			if (lo <= hi) {
				tmp = *lo;
				*lo = *hi;
				*hi = tmp;
				++lo;
				--hi;
			}
		}

		// if the right index has not reached the left size of array
		// must now sort the left partition
		if (lo0 < hi) {
			kdtree_median_sort(kd, lo0, hi, axis);
		}

		// if the left index has not reached the right size of array
		// must now sort the right partition
		if (lo < hi0) {
			kdtree_median_sort(kd, lo, hi0, axis);
		}
	}
}

/*
 * this is more complicated than one would predict. rules are:
 * - if values are significantly different, no problem
 * - the minumum of a patch is smaller than its maximum
 * - if a maximum and a minimum are near, the other extrema of
 *   the patches decide
 * - if all four extrema are near, the 2 ends of a patch have to be
 *  simultaneusly smaller or bigger than the other two ends
 */
static int
kdtree_compare(ri_kdtree_t *kd,
	       unsigned int a_index, unsigned int b_index, unsigned char axis)
{
	unsigned int ca_index = a_index & 0x7fffffff;
	float        a_begin, a_end;
	unsigned int cb_index = b_index & 0x7fffffff;
	float        b_begin, b_end;
	bbox_t       ca_bbox, cb_bbox;

	tri_get_bounding_box(&ca_bbox, kd->objects[ca_index]);
	tri_get_bounding_box(&cb_bbox, kd->objects[cb_index]);

	a_begin = ca_bbox.bmin[axis];
	a_end   = ca_bbox.bmax[axis];
	b_begin = cb_bbox.bmin[axis];
	b_end   = cb_bbox.bmax[axis];

	if (a_index & 0x80000000) {
		if (b_index & 0x80000000) {
			if (ca_index == cb_index) {
				return 0;
			}

			if (a_end < b_end - KDTREE_BOUNDS_EPSILON) {
				return 1;
			}

			if (b_end < a_end - KDTREE_BOUNDS_EPSILON) {
				return 0;
			}
		
			if (a_begin < b_begin - KDTREE_BOUNDS_EPSILON) {
				return 1;
			}

			if (b_begin < a_begin - KDTREE_BOUNDS_EPSILON) {
				return 0;
			}

			return (ca_index < cb_index);
		} else {
			if (ca_index == cb_index) {
				return 0;
			}

			if (a_end < b_begin - KDTREE_BOUNDS_EPSILON) {
				return 1;
			}

			if (b_begin < a_end - KDTREE_BOUNDS_EPSILON) {
				return 0;
			}
		
			if (a_begin < b_end - KDTREE_BOUNDS_EPSILON) {
				return 1;
			}

			return (ca_index < cb_index);
		}
	} else {
		if (b_index & 0x80000000) {
			if (ca_index == cb_index) {
				return 1;
			}

			if (b_end < a_begin - KDTREE_BOUNDS_EPSILON) {
				return 0;
			}

			if (a_begin < b_end - KDTREE_BOUNDS_EPSILON) {
				return 1;
			}
		
			if (b_begin < a_end - KDTREE_BOUNDS_EPSILON) {
				return 0;
			}

			return (ca_index < cb_index);
		} else {
			if (ca_index == cb_index) {
				return 0;
			}

			if (a_begin < b_begin - KDTREE_BOUNDS_EPSILON) {
				return 1;
			}

			if (b_begin < a_begin - KDTREE_BOUNDS_EPSILON) {
				return 0;
			}
		
			if (a_end < b_end - KDTREE_BOUNDS_EPSILON) {
				return 1;
			}

			if (b_end < a_end - KDTREE_BOUNDS_EPSILON) {
				return 0;
			}

			return (ca_index < cb_index);
		}
	}
}

static ri_kdtree_t *
kdtree_create(tri_info_t **objptrs, unsigned int nobjs)
{
	unsigned int  tnnodes;
	unsigned int  depth;
	unsigned int *obj_boundaries;
	unsigned int  fill;

	ri_kdtree_t *kdtree = (ri_kdtree_t *)ri_mem_alloc(sizeof(ri_kdtree_t));

	kdtree->maximum_depth =
		ri_render_get()->context->option->kd_tree_depth;
	
	if (kdtree->maximum_depth > KDTREE_MAX_DEPTH) {
		kdtree->maximum_depth = KDTREE_MAX_DEPTH;
	}

	if (g_dbg) {
		printf("kdtree: maximum_depth = %d\n", kdtree->maximum_depth);
	}

	kdtree->ncachelinebytes = KDTREE_CACHE_LINE_BYTES;
	kdtree->ncachelinenodes = kdtree->ncachelinebytes / sizeof(float) - 1;

	// Size is based on experiments and heuristics.
	kdtree->ncachelines     = nobjs * 18 / kdtree->ncachelinenodes + 1;

	// allocate memory structures.
	tnnodes = kdtree->ncachelines * kdtree->ncachelinenodes;

	// depth = floor(log(tnnodes))?
	for (depth = 1; tnnodes; depth++) {
		tnnodes = tnnodes >> 1;
	}

	if (g_dbg) {
		printf("ncachelinenodes = %d\n", kdtree->ncachelinenodes);
		printf("ncachelines = %d\n", kdtree->ncachelines);
		printf("depth = %d\n", depth);
	}


	kdtree->trav_stack = (trav_stack_t *)
				ri_mem_alloc(sizeof(trav_stack_t) *
					     (0x1 << depth));
	kdtree->stack_ptr = -1;

	kdtree->last_freenode_index = 0;
	kdtree->first_freenode_index = 0;

	kdtree->max_freenodes = kdtree->ncachelines *
				(kdtree->ncachelinenodes + 1);

	if (g_dbg) {
		printf("kdtree->max_freenodes = %d\n", kdtree->max_freenodes);
	}

	kdtree->freenodes = (unsigned int *)
				ri_mem_alloc(sizeof(unsigned int) *
					     kdtree->max_freenodes);
	kdtree->nfreenodes = 0;


	// nodes in cachelines + offset correction
	kdtree->enode_table = (float *)
				ri_mem_alloc(sizeof(float) *
					kdtree->ncachelines *
					(kdtree->ncachelinenodes + 1) +
					kdtree->ncachelinenodes + 1);

	kdtree->node_table = (float *)(((unsigned int)kdtree->enode_table /
				  kdtree->ncachelinebytes + 1) *
				  kdtree->ncachelinebytes);

	if (g_dbg) {
		printf("addr(kdtree->node_table) = %p\n", &(kdtree->node_table));
		printf("nodes = %d\n", kdtree->ncachelines * (kdtree->ncachelinenodes + 1) + kdtree->ncachelinenodes + 1);
	}

	// collect the initial object extremes to pass on to the build
	// algorithm.
	obj_boundaries = (unsigned int *)ri_mem_alloc(sizeof(unsigned int) *
						     (nobjs << 1));


	for (fill = 0; fill < nobjs; fill++) {
		obj_boundaries[fill << 1] = fill & 0x7fffffff;
		obj_boundaries[(fill << 1) + 1] = fill | 0x80000000;
	}

	kdtree->objects = objptrs;
	kdtree->nobjs = nobjs;

	// get scene bounding box
	get_scene_bounding_box(&(kdtree->bbox),
			       (const tri_info_t **)kdtree->objects, kdtree->nobjs);
	
	// GO! GO! Yubari!
	kdtree_build(kdtree, 0, obj_boundaries, nobjs, &kdtree->bbox, 0, 0);
				
	ri_mem_free(kdtree->freenodes);

	return kdtree;
}

static void
kdtree_get_bounding_box(bbox_t *bbox, unsigned int *tri_indices,
			ri_vector_t *vlist, unsigned int nobjs)
{
	unsigned int i, j, k;
	unsigned int idx[3];

	if (nobjs < 1) return;

	idx[0] = tri_indices[0];

	bbox->bmin[0] = vlist[idx[0]].e[0];
	bbox->bmin[1] = vlist[idx[0]].e[1];
	bbox->bmin[2] = vlist[idx[0]].e[2];

	bbox->bmax[0] = bbox->bmin[0];
	bbox->bmax[1] = bbox->bmin[1];
	bbox->bmax[2] = bbox->bmin[2];

	for (i = 0; i < nobjs; i += 3) {
		for (j = 0; j < 3; j++) {
			idx[j] = tri_indices[3 * i + j]; 

			for (k = 0; k < 3; k++) {

				if (bbox->bmin[k] > vlist[idx[j]].e[k]) {
					bbox->bmin[k] = vlist[idx[j]].e[k];
				}

				if (bbox->bmax[k] < vlist[idx[j]].e[k]) {
					bbox->bmax[k] = vlist[idx[j]].e[k];
				}
			}
		}
	}
}

static void
tri_get_bounding_box(bbox_t *bbox,	/* result */
		     const tri_info_t *tri)
{
	int i, j;

	bbox->bmin[0] = tri->v[0].e[0];
	bbox->bmin[1] = tri->v[0].e[1];
	bbox->bmin[2] = tri->v[0].e[2];

	bbox->bmax[0] = bbox->bmin[0];
	bbox->bmax[1] = bbox->bmin[1];
	bbox->bmax[2] = bbox->bmin[2];

	for (i = 1; i < 3; i++) {
		for (j = 0; j < 3; j++) {
			if (bbox->bmin[j] > tri->v[i].e[j]) {
				bbox->bmin[j] = tri->v[i].e[j];
			}

			if (bbox->bmax[j] < tri->v[i].e[j]) {
				bbox->bmax[j] = tri->v[i].e[j];
			}
		}
	}
}

static unsigned int maximum_tris = 0;

static void
kdtree_dump(ri_kdtree_t *kd)
{
	unsigned char *filename = "kdtree.kdt";
	FILE *fp;
	unsigned int i;
	tri_info_t *triptr;

	fp = fopen(filename, "wb");
	if (!fp) {
		fprintf(stderr, "coundn't open [ %s ]\n", filename);
		exit(-1);
	}

	printf("total tris in the scene = %d\n", kd->nobjs);

	fwrite(&kd->nobjs, sizeof(unsigned int), 1, fp);
	// output whole scene geometri first
	for (i = 0; i < kd->nobjs; i++) {
		triptr = kd->objects[i];
		fwrite(&(triptr->v[0]), sizeof(float), 3, fp);
		fwrite(&(triptr->v[1]), sizeof(float), 3, fp);
		fwrite(&(triptr->v[2]), sizeof(float), 3, fp);
	}

	// next, output scene bounding box
	fwrite(kd->bbox.bmin, sizeof(float), 3, fp);
	fwrite(kd->bbox.bmax, sizeof(float), 3, fp);

	// then, output kd tree nodes
	kdtree_dump_trav(kd, fp, 0);

	fclose(fp);

	printf("maximum_tris in leaf = %d\n", maximum_tris);

	return;
}

static void
kdtree_dump_trav(ri_kdtree_t *kd, FILE *fp, unsigned int nodeid)
{
	int is_leaf;
	unsigned int left, right;
	unsigned int itoken;
	tri_info_t **leaflist, **end, **iter;
	unsigned int len;
	float       cut_plane;
	unsigned int axis;
	unsigned int bitfield;
	

	is_leaf = !kdtree_follow_children(kd, &nodeid, &left, &right);

	if (g_dbg) {
		printf("--- id = %d ---\n", nodeid);
		printf("\tleft = %d, right = %d\n",  left, right);
	}

	if (is_leaf) {

		itoken = 3;		// leaf node
		fwrite(&itoken, sizeof(unsigned int), 1, fp);

		leaflist = ((tri_info_t ***)kd->node_table)[nodeid];
		if (leaflist) {
			// first 4 bytes of leaflist contains the length.
			len = *(unsigned int *)leaflist;

			if (maximum_tris < len) maximum_tris = len;

			itoken = len;	
			fwrite(&itoken, sizeof(unsigned int), 1, fp);

			end = ++leaflist + len;

			if (g_dbg) {
				printf("\tleaf\n\tnodeid = %d\n",  nodeid);
				printf("\tlen = %d\n",  len);
			}

			for (iter = leaflist;
			     iter < end;
			     iter++) {

				if (g_dbg) {
					printf("\tv0 = (%f, %f, %f)\n",
						(*iter)->v[0].e[0],
						(*iter)->v[0].e[1],
						(*iter)->v[0].e[2]);
					printf("\tv1 = (%f, %f, %f)\n",
						(*iter)->v[1].e[0],
						(*iter)->v[1].e[1],
						(*iter)->v[1].e[2]);
					printf("\tv2 = (%f, %f, %f)\n",
						(*iter)->v[2].e[0],
						(*iter)->v[2].e[1],
						(*iter)->v[2].e[2]);
				}

				fwrite(&((*iter)->v[0]), sizeof(float), 3, fp);
				fwrite(&((*iter)->v[1]), sizeof(float), 3, fp);
				fwrite(&((*iter)->v[2]), sizeof(float), 3, fp);
			}
		} else {
			itoken = 0;		// empty
			fwrite(&itoken, sizeof(unsigned int), 1, fp);
		}
		
	} else {
		// non-leaf node, go deeper

		cut_plane = kd->node_table[nodeid];
		axis = *(unsigned int *)(&cut_plane) & 0x00000003;
		bitfield = *(unsigned int *)(&kd->node_table[nodeid]) &
						0xfffffffc;
		cut_plane = *(float *)(&bitfield);

		fwrite(&axis, sizeof(unsigned int), 1, fp);
		fwrite(&cut_plane, sizeof(float), 1, fp);

		kdtree_dump_trav(kd, fp, left);
		kdtree_dump_trav(kd, fp, right);
	}
}

static void
get_scene_bounding_box(bbox_t *bbox,
		       const tri_info_t **tris, unsigned int ntris)
{
	unsigned int i, j;
	bbox_t bound;

	tri_get_bounding_box(&bound, tris[0]);
	ri_mem_copy(bbox, &bound, sizeof(bbox_t));

	for (i = 1; i < ntris; i++) {
		tri_get_bounding_box(&bound, tris[i]);

		for (j = 0; j < 3; j++) {
			if (bbox->bmin[j] > bound.bmin[j]) {
				bbox->bmin[j] = bound.bmin[j];
			}

			if (bbox->bmax[j] < bound.bmax[j]) {
				bbox->bmax[j] = bound.bmax[j];
			}
		}
	}

	if (g_dbg) {
		printf("kdtree: scene bounding box = \n");
		printf("        (%f, %f, %f)-(%f, %f, %f)\n",
			bbox->bmin[0], bbox->bmin[1], bbox->bmin[2],
			bbox->bmax[0], bbox->bmax[1], bbox->bmax[2]);
	}
}
