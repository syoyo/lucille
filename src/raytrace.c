#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#ifdef WITH_SSE
#include <xmmintrin.h>
#endif

#if defined(__APPLE__) && defined(__MACH__)
//#include <ppc_intrinsics.h>
#endif

#if defined(__APPLE__) && defined(__MACH__)
#define FORCE_INLINE __attribute__((always_inline))
#else
#define FORCE_INLINE
#endif

#include "vector.h"
#include "list.h"
#include "log.h"
#include "raytrace.h"
#include "polygon.h"
#include "util.h"
#include "accel.h"
#include "render.h"
#include "memory.h"
#include "bsp.h"
#include "kdtree.h"
#include "reflection.h"
#include "thread.h"

#define EPS 1.0e-6
#define EPSILON 1.0e-6
#define EPSILON2 1.0e-6
#define DIREPS 1.0e-6

/* use z curve order for addressing volxel memory. */
#define USE_ZORDER 1

#ifdef HAVE_RESTRICT	/* The compiler supports restrict keyword. */
#define RESTRICT restrict
#else
#define RESTRICT
#endif

/* 16-byte aligned thread specific data represented in array form. */
ri_aligned_float_t simd_raydata;
ri_aligned_float_t simd_hitt;
ri_aligned_float_t simd_hitu;
ri_aligned_float_t simd_hitv;

#ifdef WITH_SSE
#define cross_sse(a, b, c, d) (						\
	_mm_sub_ps(_mm_mul_ps((a), (c)), _mm_mul_ps((b), (d))) )

#define dot_sse(ax, ay, az, bx, by, bz) (				\
	_mm_add_ps(_mm_add_ps(_mm_mul_ps((ax), (bx)),			\
			      _mm_mul_ps((ay), (by))),			\
			      _mm_mul_ps((az), (bz))))
#endif

#ifdef WITH_ALTIVEC
#define cross_altivec(a, b, c, d) (					\
	vec_madd((a), (c), vec_nmsub((b), (d), vec_ctf(vec_splat_u32(0), 0))))

#define dot_altivec(ax, ay, az, bx, by, bz) (				\
	vec_madd((ax),							\
		 (bx),							\
		 vec_madd((ay),						\
			  (by),						\
			  vec_madd((az),				\
				   (bz),				\
				   vec_ctf(vec_splat_u32(0), 0)))))

#define vcomp(v, n) (*(((float *)&(v)) + n))
#define vbcomp(v, n) (*(((int *)&(v)) + n))

inline vector float vec_div(vector float v)
{
	vector float reciprocal = vec_re(v);
	return vec_madd(reciprocal,
			vec_nmsub(reciprocal, v, vec_ctf(vec_splat_u32(1), 0)),
			reciprocal);
}

#endif

/* for block'ed memory access */
#define BLKIDX(x, y, z, shift, blksize) ( 				\
		((z >> shift) * blksize * blksize) +			\
		((y >> shift) * blksize) + (x >> shift))

#define SUBIDX(x, y, z, width, mask) ( 					\
		(z & mask) * width * width +				\
		(y & mask) * width + (x & mask))

#define MAP_XYZ(x, y, z, shift, blksize, width, mask) 			\
		BLKIDX((x), (y), (z), (shift), (blksize)) *		\
		(width) * (width) * (width) +				\
		SUBIDX((x), (y), (z), (width), (mask))

#define MAP_Z3D(x, y, z) (						\
		g_z_table[(x)] | (g_z_table[(y)] << 1) |		\
		(g_z_table[(z)] << 2)) 

static unsigned int g_z_table[256];

/* Data structure for a simple stack. this is necessary for implementing an
 * efficient linear BSP tree walking. A stak size of 50 means it is possible
 * to support a BSP tree of up to depth 49 and NO MORE!!! It should be enough
 * to the next decade or so.
 */
#define STACKSIZE 50

typedef struct _bsp_stackelem_t
{
	binnode_t *node;
	float      min, max;
} bsp_stackelem_t;

typedef struct _bsp_stack_t 
{
	int stackptr;
	bsp_stackelem_t stack[STACKSIZE];
} bsp_stack_t;

void
stack_init(bsp_stack_t *stack)
{
	stack->stack[0].node = NULL;
	stack->stackptr = 1;
}

void
stack_push(bsp_stack_t *stack, binnode_t *node, float min, float max)
{
	stack->stack[stack->stackptr].node = node;
	stack->stack[stack->stackptr].min  = min; 
	stack->stack[stack->stackptr].max  = max;
	(stack->stackptr)++;
	if (stack->stackptr == STACKSIZE) {
		printf("??? MAXSTACKSIZE\n");
		exit(-1);
	}
}

void
stack_pop(bsp_stack_t *stack, binnode_t **node, float *min, float *max)
{
	(stack->stackptr)--;
	*node = stack->stack[stack->stackptr].node;
	*min  = stack->stack[stack->stackptr].min;
	*max  = stack->stack[stack->stackptr].max;
}


static void
get_octree_bb(const otCell *cell, float min[3], float max[3])
{
	//float width = 1.0f / (float)(1 << (OT_ROOT_LEVEL - cell->level));
	float width = (float)(1 << (cell->level)) * OT_INV_MAX_VAL;

	min[0] = cell->xLocCode * OT_INV_MAX_VAL;
	min[1] = cell->yLocCode * OT_INV_MAX_VAL;
	min[2] = cell->zLocCode * OT_INV_MAX_VAL;

	max[0] = min[0] + width;
	max[1] = min[1] + width;
	max[2] = min[2] + width;
}

#if defined (__APPLE__) && (__MACH__)
/* This is for Generating a Trace File with Amber. */
void
Start_or_Stop_Trace(long *result)
{
	register long temp;

	// Generate inline assembly to read from the
	// Processor Information Register
	#if defined(__MWERKS__)
	asm { mfspr tmp, 1023}
	#else
	__asm__ volatile ( "mfspr %0, 1023" : "=r" (temp) );
	#endif

	// We take an address as a argument to prevent
	// the compiler from moving the code around
	*result = temp;
}
#endif

static int intersect_with_accel(ri_ugrid_t *ugrid,
				const ri_vector_t *eye, const ri_vector_t *dir,
				ri_surface_info_t *info, float *t);
static int intersect_with_bsp(ri_bsp_t *bsp,
			      const ri_ray_t *ray,
			      ri_surface_info_t *info, float *t);
static int intersect_with_kdtree(ri_kdtree_t *kd,
			         const ri_ray_t *ray,
			         ri_surface_info_t *info, float *t);
static int intersect_with_octree(ri_octree_accel_t *octree,
				 const ri_vector_t *eye, const ri_vector_t *dir,
				 ri_surface_info_t *info, float *t);

static int foreach_polygon_in_geom(ri_geom_t *geom,
				   const ri_vector_t *eye,
				   const ri_vector_t *dir,
				   ri_surface_info_t *info, float *t);
static void calculate_normal(ri_vector_t *n,
			     const ri_vector_t *v0,
			     const ri_vector_t *v1,
			     const ri_vector_t *v2);
static void lerp_normal(ri_vector_t *n,
			const ri_vector_t *n0,
			const ri_vector_t *n1,
			const ri_vector_t *n2,
			float u, float v);
static void lerp_uv(float *newu, float *newv,
		    float *uv0, float *uv1, float *uv2,
		    float u, float v);

static int inside(const ri_vector_t *pos,
		  const float bmin[3], const float bmax[3]);
static int intersect_ray_bbox(const ri_vector_t *eye, const ri_vector_t *dir,
			      const float bmin[3], const float bmax[3],
			      float isectt[2]);
static int intersect_foreach_trilist(ri_tri_list_t *list,
				     const ri_vector_t *eye,
				     const ri_vector_t *dir,
				     ri_surface_info_t *info, float *t);
static int inside_voxel(const ri_vector_t *pos, int x, int y, int z,
			const ri_ugrid_t *grid);
static void build_surfinfo(ri_surface_info_t *info,
			   const ri_vector_t *isectpoint,
			   ri_geom_t *geom, unsigned int index,
			   float u, float v);
static int ray_bbox(const float org[3], const float dir[3],
		    float min[3], float max[3], float t[2]);
static int inside_cell(const ri_vector_t *p, const otCell *cell,
		       const float bmin[3], const float invwidth[3]);
static void min_cell(float p[3], int maxlevel, float min[3], float max[3]);

/* BSP routines. */
static int ray_box_intersect(const ri_ray_t *ray,
			     const ri_vector_t *min, const ri_vector_t *max,
			     float *retmin, float *retmax);
static int ray_trilist_intersect(const ri_ray_t *ray, trilist_t trilist,
				 ri_tri_info_t *tri,
				 float *dist, float *u, float *v);
static int point_in_node(binnode_t *node, const ri_vector_t *point);
static int bsp_is_leaf(binnode_t *node);
static void point_at_distance(const ri_ray_t *ray, float dist, ri_vector_t *point);
static int bsp_intersect(ri_bsp_t *tree,
			 const ri_ray_t *ray,
                         ri_tri_info_t *tri, float *t, float *u, float *v);

/* -- build table for z curve order */
static void build_z_table();

#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
static int intersect_with_accel_simd(const ri_ugrid_t  *ugrid,
			             const ri_ray_t *ray,
			             ri_surface_info_t *info, float *t);

static int intersect_foreach_trilist_simd(
				   const ri_simd_tri_info_t *list,
				   const ri_aligned_float_t *ray,
				   int tnum,		/* thread number */
				   ri_tri_info_t *hittri,
				   float *t, float *u, float *v);

#endif

#ifdef WITH_SSE
static void isect_sse(const float *ray, const float *data, float *t, float *u, float *v);
#endif

#ifdef WITH_ALTIVEC
static void FORCE_INLINE isect_altivec(const float *ray, const float *data, float * RESTRICT t, float * RESTRICT u, float * RESTRICT v);
#endif

void
ri_ray_copy(ri_ray_t *dst, const ri_ray_t *src)
{
	ri_mem_copy(dst, src, sizeof(ri_ray_t));
}

/*
 * codes from
 * "Practical Analysis of Optimized Ray-Triangle Intersection"
 * Tomas Moeller
 * http://www.ce.chalmers.se/staff/tomasm/raytri/
 * 
 * : we use opti1 version(divide at end).
 *   this version seems faster on x86 CPU.
 */
RtInt
triangle_intersect(const ri_vector_t *orig, const ri_vector_t *dir,
		   const ri_vector_t *v0,
		   const ri_vector_t *v1,
		   const ri_vector_t *v2,
		   RtFloat *t, RtFloat *u, RtFloat *v)
{
	/* ray-triangle intersection test with back face culling test */

	ri_vector_t edge1, edge2, tvec, pvec, qvec;
	float det, inv_det;
	
	ri_vector_sub(&edge1, v1, v0);
	ri_vector_sub(&edge2, v2, v0);

	/* begin calculation determinant */
	ri_vector_cross3(&pvec, dir, &edge2);

	/* if determinant is near zero, ray lies in plane of triangle */
	det = ri_vector_dot3(&edge1, &pvec);

	if (det > EPSILON) {

		ri_vector_sub(&tvec, orig, v0);

		(*u) = ri_vector_dot3(&tvec, &pvec);
		if ((*u) < 0.0 || (*u) > det) return 0;

		ri_vector_cross3(&qvec, &tvec, &edge1);

		(*v) = ri_vector_dot3(dir, &qvec);
		if ((*v) < 0.0 || (*u) + (*v) > det) return 0;
	} else {
		/* ray is parallel to the plane of the triangle or
		 * the triangle is back face. */
		return 0;
	}

	inv_det = 1.0 / det;

	(*t) = ri_vector_dot3(&edge2, &qvec);
	(*t) *= inv_det;
	(*u) *= inv_det;
	(*v) *= inv_det;

	return 1;	/* hit! */
}

/* SHOULD BE CALLED only once from out of threaded code.
 * ( called from ri_render_init() )
 */
void
ri_raytrace_setup()
{
	/* allocate 16-byte aligned working area for thread specific data. */

	ri_aligned_float_alloc(&simd_raydata, 24 * RI_MAX_THREADS);
	ri_aligned_float_alloc(&simd_hitt,     4 * RI_MAX_THREADS);
	ri_aligned_float_alloc(&simd_hitu,     4 * RI_MAX_THREADS);
	ri_aligned_float_alloc(&simd_hitv,     4 * RI_MAX_THREADS);

	build_z_table();
}

void
ri_raytrace_shutdown()
{
	ri_aligned_float_free(&simd_raydata);
	ri_aligned_float_free(&simd_hitt);
	ri_aligned_float_free(&simd_hitu);
	ri_aligned_float_free(&simd_hitv);
}

int
ri_raytrace(ri_render_t *render, ri_ray_t *ray, ri_surface_info_t *info)
{
	int   hit = 0;
	int   hashit = 0;
	float t;
	float nearest = (float)RI_INFINITY;
	ri_surface_info_t tmp;
	ri_list_t *itr;
	ri_geom_t *geom;

	ray->isectt = 0.0;
	//g_nrays++;
	ri_render_get()->stat.nrays++;

	tmp.inside = 0;

	if (!render->accel_grid && !render->octree &&
	    !render->accel_kdtree && !render->accel_bsp) {

		for (itr = ri_list_first(render->geomlist);
		     itr != NULL;
		     itr = ri_list_next(itr)) {

			geom = (ri_geom_t *)itr->data;

			hit = ri_raytrace_geom(geom, ray, info);

			if (hit) {
				if (ray->isectt > EPS &&
				    ray->isectt <= nearest) {
					nearest = ray->isectt;
					hashit  = 1;

					/* todo: use our version of memcpy */
					memcpy(info, &tmp,
					       sizeof(ri_surface_info_t));

				}
			}
		}

		if (hashit) ray->isectt = nearest;

	} else {

		if (render->context->option->accel_method == ACCEL_GRID) {
#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
			hit = intersect_with_accel_simd(render->accel_grid,
							ray,
							&tmp, &t);
#else
			hit = intersect_with_accel(render->accel_grid,
						   &ray->org, &ray->dir,
						   &tmp, &t);
#endif
		} else if (render->context->option->accel_method ==
							ACCEL_OCTREE_FRISKEN) {
			hit = intersect_with_octree(render->octree,
						   &ray->org, &ray->dir,
						   &tmp, &t);
		} else if (render->context->option->accel_method ==
							ACCEL_BSP) {
			hit = intersect_with_bsp(render->accel_bsp,
					   ray, &tmp, &t);
		} else if (render->context->option->accel_method ==
							ACCEL_KDTREE) {
			hit = intersect_with_kdtree(render->accel_kdtree,
					   ray, &tmp, &t);
		}

		if (hit) {
			hashit  = 1;


			/* todo: use our version of memcpy */
			memcpy(info, &tmp,
			       sizeof(ri_surface_info_t));

			ray->isectt = t;
		}
	}


	return hashit;
}

int
ri_raytrace_geom(ri_geom_t *geom, ri_ray_t *ray, ri_surface_info_t *info)
{
	//g_nrays++;
	ri_render_get()->stat.nrays++;
	return foreach_polygon_in_geom(geom,
				       &ray->org, &ray->dir,
				       info, &(ray->isectt));
}

void
ri_raytrace_statistics()
{
	unsigned long long ngridtravs;
	unsigned long long ntesttris;
	//unsigned long nmailboxhits;
	unsigned long long nrays;

	double elapsed = 0.0;

	ngridtravs   = ri_render_get()->stat.ngridtravs;
	ntesttris    = ri_render_get()->stat.ntesttris;
	//nmailboxhits = ri_render_get()->stat.nmailboxhits;
	nrays        = ri_render_get()->stat.nrays;

	elapsed = ri_timer_elapsed(ri_render_get()->context->timer,
				   "Render frame");
	
	printf("\n");
	printf("/= Raytracing statistics ======================================================\n");
	printf("| %-48s:  %20llu\n", "Total rays", nrays);
	printf("| %-48s:  %20llu\n", "Total grid cell traversals", ngridtravs);
	printf("| %-48s:  %20llu\n", "Total triangle tests", ntesttris);
	//printf("total mailboxing hits:  %20llu\n", nmailboxhits);
	printf("| %-48s:  %20.6f\n", "The number of tests per ray", (double)ntesttris / (double)nrays);
	printf("| %-48s:  %20.6f\n", "The number of travs per ray",
	       (double)ngridtravs / (double)nrays);
	printf("| %-48s:  %20.6f\n", "Rays/sec", (double)nrays / (double)elapsed);
	printf("\\------------------------------------------------------------------------------\n");
	fflush(stdout);
}

static int
point_in_node(binnode_t *node, const ri_vector_t *point)
{
	if ((point->e[0] >= node->min.e[0]) && (point->e[1] >= node->min.e[1]) &&
	    (point->e[2] >= node->min.e[2]) && (point->e[0] <= node->max.e[0]) &&
	    (point->e[1] <= node->max.e[1]) && (point->e[2] <= node->max.e[2])) {
		return 1;
	}

	return 0;
}

static int
bsp_is_leaf(binnode_t *node)
{
	if (node == NULL) return 1;

	//if (node->child[0] == NULL ||
	//    node->child[1] == NULL) {
	//		return 1;
	//}
	if (node->child[0] == NULL) return 1;
	
	return 0;
}

static void
point_at_distance(const ri_ray_t *ray, float dist, ri_vector_t *point)
{
	point->e[0] = ray->org.e[0] + dist * ray->dir.e[0];
	point->e[1] = ray->org.e[1] + dist * ray->dir.e[1];
	point->e[2] = ray->org.e[2] + dist * ray->dir.e[2];
}


/* Traveres ray through BSP tree and intersects ray width all of the objects
 * along the way. Returns the closest intersection distance and the
 * intersection object if there is one.
 */
static int
bsp_intersect(ri_bsp_t *tree, const ri_ray_t *ray,
	      ri_tri_info_t *tri, float *t, float *u, float *v)
{
	int i;
	int tnum;		/* thread number */
	static bsp_stack_t *stack = NULL;
	binnode_t *currnode, *nearchild, *farchild ;
	float dist, min, max;
	ri_vector_t p;
	ri_ray_t trav_ray;

	//ncalls++;

	/* test if the whole BSP tree is missed by the input ray. */
	if (!ray_box_intersect(ray, &tree->min, &tree->max, &min, &max)) {
		//printf("ray is out of bbox\n");
		return 0;
	}

	if (!stack) {	
		stack = (bsp_stack_t *)ri_mem_alloc(sizeof(bsp_stack_t));
	}

	stack_init(stack);

	tnum = ray->thread_num;
	assert(tnum < RI_MAX_THREADS);

	for (i = 0; i < 4; i++) {
		simd_raydata.aligned[ tnum * 24 +  0 + i] = ray->dir.e[0];
		simd_raydata.aligned[ tnum * 24 +  4 + i] = ray->dir.e[1];
		simd_raydata.aligned[ tnum * 24 +  8 + i] = ray->dir.e[2];
		simd_raydata.aligned[ tnum * 24 + 12 + i] = ray->org.e[0];
		simd_raydata.aligned[ tnum * 24 + 16 + i] = ray->org.e[1];
		simd_raydata.aligned[ tnum * 24 + 20 + i] = ray->org.e[2];
	}

	currnode = tree->root;

	ri_ray_copy(&trav_ray, ray);

	while (currnode != NULL) {
		while (!bsp_is_leaf(currnode)) {
			//ntravs++;
			dist = currnode->dist(&currnode->child[0]->max, &trav_ray.org, &trav_ray.dir);
			currnode->getchildren(currnode, &nearchild, &farchild,
					      &trav_ray.org);

			if ((dist > max) || (dist < 0)) {
				currnode = nearchild;
			} else if (dist < min) {
				currnode = farchild;
			} else {
				stack_push(stack, farchild, dist, max);
				currnode = nearchild;
				max = dist;
			}
		}

#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
		if (currnode->trilists.simdtris &&
		    intersect_foreach_trilist_simd(currnode->trilists.simdtris,
						   &simd_raydata, tnum, tri, t, u, v)) {
#else
		if (ray_trilist_intersect(&trav_ray, currnode->trilists,
					  tri, t, u, v)) {
#endif

			point_at_distance(&trav_ray, *t, &p);

			if (point_in_node(currnode, &p)) {
				return 1;
			}
		}

		stack_pop(stack, &currnode, &min, &max);
	}

	//ri_mem_free(stack);

	return 0;
}

static int
ray_trilist_intersect(const ri_ray_t *ray, trilist_t trilist, ri_tri_info_t *tri,
		      float *dist, float *u, float *v)
{
	/* this routine intersects ray with all of the objects
	 * in the objlist and returns the closest intersection
	 * distance and the interesting object, if there is one.
	 */

	/* todo: implement */

	int i;
	int hit = 0;
	float t, uparam, vparam;
	float nearest = (float)RI_INFINITY;

	//ntritests += trilist.ntris;
	for (i = 0; i < trilist.ntris; i++) {
		if (triangle_intersect(&ray->org, &ray->dir,
				       &trilist.vlist[3 * i + 0],
				       &trilist.vlist[3 * i + 1],
				       &trilist.vlist[3 * i + 2],
				       &t, &uparam, &vparam)) {
			if (t > EPSILON && t < nearest) {
				hit     = 1;
				nearest = t;
				(*dist) = t;
				tri->geom = trilist.geoms[i];
				tri->index = trilist.indices[i];

				//ri_mem_copy(&(tri->vlist[0]),
				//	    &(trilist.vlist[3 * i]),
				//	    sizeof(ri_vector_t) * 3);
				(*u) = uparam;
				(*v) = vparam;
			}
		}
	}

	return hit;
}


/* --- private functions --- */

static void
lerp_uv(float *newu, float *newv,
	float *uv0, float *uv1, float *uv2,
	float u, float v)
{
	/* n = (1 - u - v) n0 + u n1 + v n2 */

	*newu = (1 - u - v) * uv0[0] + u * uv1[0] + v * uv2[0];
	*newv = (1 - u - v) * uv0[1] + u * uv1[1] + v * uv2[1];
}

static int
intersect_with_accel(ri_ugrid_t *ugrid,
		     const ri_vector_t *eye, const ri_vector_t *dir,
		     ri_surface_info_t *info, float *t)
{
	int x, y, z;
	int hit;
	float rayt = (float)RI_INFINITY;
	float maxt = (float)RI_INFINITY;
	float tvals[2];
	float raypos[3];
	double nextx, nexty, nextz;
	double deltax, deltay, deltaz;
	int stepx, stepy, stepz;
	int outx, outy, outz;
	ri_surface_info_t surfinfo;
	ri_vector_t isectpoint;
	float nearest = (float)RI_INFINITY;
	float isectt;
	ri_ugrid_t *grid;
	
	if (dir->e[0] == 0.0 && dir->e[1] == 0.0 && dir->e[2] == 0.0) return 0; 

	//grid = ri_render_get()->accel_grid;
	grid = ugrid;

	if (inside(eye, grid->bboxmin, grid->bboxmax)) {
		rayt = 0.0;
	} else {
		if (!intersect_ray_bbox(eye, dir,
					grid->bboxmin, grid->bboxmax, tvals)) {
			return 0;
		}
		rayt = tvals[0];
		maxt = tvals[1];
	}	

	raypos[0] = eye->e[0] + dir->e[0] * rayt;
	raypos[1] = eye->e[1] + dir->e[1] * rayt;
	raypos[2] = eye->e[2] + dir->e[2] * rayt;

	x = (int)((raypos[0] - grid->bboxmin[0]) * grid->invwidth[0]);
	if (x == grid->voxels[0]) x--;

	if (fabs(dir->e[0]) < DIREPS) {
		nextx  = RI_INFINITY;
		deltax = 0.0;
		stepx  = 0;
		outx   = -1;
	} else if (dir->e[0] > 0) {
		nextx = rayt +
			(((x + 1) * grid->width[0] + grid->bboxmin[0]) -
			 raypos[0]) / dir->e[0];
	 
		deltax = grid->width[0] / dir->e[0];
		stepx = 1;
		outx = grid->voxels[0];
	} else {
		nextx = rayt +
			((x * grid->width[0] + grid->bboxmin[0]) -
			 raypos[0]) / dir->e[0];
	 
		deltax = -grid->width[0] / dir->e[0];
		stepx = -1;
		outx = -1;
	}

	y = (int)((raypos[1] - grid->bboxmin[1]) * grid->invwidth[1]);
	if (y == grid->voxels[1]) y--;

	if (fabs(dir->e[1]) < DIREPS) {
		nexty  = RI_INFINITY;
		deltay = 0.0;
		stepy  = 0;
		outy   = -1;
	} else if (dir->e[1] > 0) {
		nexty = rayt +
			(((y + 1) * grid->width[1] + grid->bboxmin[1]) -
			 raypos[1]) / dir->e[1];
	 
		deltay = grid->width[1] / dir->e[1];
		stepy = 1;
		outy = grid->voxels[1];
	} else {
		nexty = rayt +
			((y * grid->width[1] + grid->bboxmin[1]) -
			 raypos[1]) / dir->e[1];
	 
		deltay = - grid->width[1] / dir->e[1];
		stepy = -1;
		outy = -1;
	}

	z = (int)((raypos[2] - grid->bboxmin[2]) * grid->invwidth[2]);
	if (z == grid->voxels[2]) z--;

	if (fabs(dir->e[2]) < DIREPS) {
		nextz  = RI_INFINITY;
		deltaz = 0.0;
		stepz  = 0;
		outz   = -1;
	} else if (dir->e[2] < 0) {
		nextz = rayt +
			((z * grid->width[2] + grid->bboxmin[2]) -
			 raypos[2]) / dir->e[2];
	 
		deltaz = -grid->width[2] / dir->e[2];
		stepz = -1;
		outz = -1;
	} else {
		nextz = rayt +
			(((z + 1) * grid->width[2] + grid->bboxmin[2]) -
			 raypos[2]) / dir->e[2];
	 
		deltaz = grid->width[2] / dir->e[2];
		stepz = 1;
		outz = grid->voxels[2];
	}

	hit = 0;

	if (x < 0) x = 0;
	if (x > grid->voxels[0] - 1) x = grid->voxels[0] - 1;
	if (y < 0) y = 0;
	if (y > grid->voxels[1] - 1) y = grid->voxels[1] - 1;
	if (z < 0) z = 0;
	if (z > grid->voxels[2] - 1) z = grid->voxels[2] - 1;

	surfinfo.inside = 0;

	while (1) {
		//if (g_profile) g_ngridtravcells++;
		//ri_render_get()->stat.ngridtravs++;

		if (grid->cell[z][y][x]) {
			if (intersect_foreach_trilist(grid->cell[z][y][x],
					              eye, dir,
						      &surfinfo, &isectt)) {
				if (isectt > EPS && isectt <= nearest) {

					hit = 1;

					nearest = isectt;

					ri_mem_copy(info, &surfinfo,
					       sizeof(ri_surface_info_t));

					/* if intersection point is in this
					 * voxel, terminate traversal.
					 */
					ri_vector_copy(&isectpoint, dir);
					ri_vector_scale(&isectpoint, isectt);
					ri_vector_add(&isectpoint,
						      &isectpoint,
						       eye);

					if (inside_voxel(&isectpoint, x, y, z,
							 grid)) {
						break;
					}
				}
			}
		}

		/* 3D DDA */
		if ((nextx < nexty) && (nextx < nextz)) {
			if (maxt < nextx) break;
			x += stepx;
			if (x == outx) break;
			nextx += deltax;
		} else if ((nextz < nexty)) {
			if (maxt < nextz) break;
			z += stepz;
			if (z == outz) break;
			nextz += deltaz;
		} else {
			if (maxt < nexty) break;
			y += stepy;
			if (y == outy) break;
			nexty += deltay;
		}
	}

	(*t) = nearest;
 
	return hit;

}

#if 0
static int
intersect_with_octree(ri_octree_accel_t *octree,
		     ri_vector_t eye, ri_vector_t dir,
		     ri_surface_info_t *info, float *t)
{
	//int                i;
	int                hit = 0;
	int                maxdepth;
	int                deltascale;
	float              rayt = (float)RI_INFINITY;
	float              maxt = (float)RI_INFINITY;
	float              tvals[2];
	float              raypos[3];
	float              min[3], max[3];
	float              mmin[3], mmax[3];
	float              rayorg[3], raydir[3];
	float              p[3];
	float		   deltax, deltay, deltaz;
	float              nextx, nexty, nextz;
	float              travx, travy, travz;
	ri_surface_info_t  surfinfo;
	ri_vector_t        isectpoint;
	float              nearest = (float)RI_INFINITY;
	float              isectt;
	otCell            *cell, *prevcell;
	ri_tri_list_t     *tris;
	
#ifdef DEBUG
	if (dir.e[0] == 0.0 && dir.e[1] == 0.0 && dir.e[2] == 0.0) return 0; 
#endif

	ri_vector_normalize(&dir);

	/* find ray's entry point. */
	if (inside(eye, octree->bmin, octree->bmax)) {
		rayt = 0.0;
	} else {
		if (!intersect_ray_bbox(eye, dir,
					octree->bmin, octree->bmax, tvals)) {
			return 0;
		}
		rayt = tvals[0];
		maxt = tvals[1];
	}	

	raypos[0] = eye.e[0] + dir.e[0] * rayt;
	raypos[1] = eye.e[1] + dir.e[1] * rayt;
	raypos[2] = eye.e[2] + dir.e[2] * rayt;

	raydir[0] = dir.e[0];
	raydir[1] = dir.e[1];
	raydir[2] = dir.e[2];

	/* Normalize ray poistion to [0, 1)^3 */
	rayorg[0] = (raypos[0] - octree->bmin[0]) * octree->invwidth[0];
	rayorg[1] = (raypos[1] - octree->bmin[1]) * octree->invwidth[1];
	rayorg[2] = (raypos[2] - octree->bmin[2]) * octree->invwidth[2];
	if (rayorg[0] <  0.0) rayorg[0] = 0.0;
	if (rayorg[1] <  0.0) rayorg[1] = 0.0;
	if (rayorg[2] <  0.0) rayorg[2] = 0.0;
	if (rayorg[0] >= 1.0) rayorg[0] = 0.999999;
	if (rayorg[1] >= 1.0) rayorg[1] = 0.999999;
	if (rayorg[2] >= 1.0) rayorg[2] = 0.999999;


	/* Locate the cell ray origin contains. */
	cell = otLocateCell(octree->cell, rayorg);
	if (!cell) {
		/* ??? */
		fprintf(stderr, "??? Can't locate cell.\n");
		return 0;
	}

	/* Get octree cell's bounding box. */
	get_octree_bb(cell, min, max);

	/* Get the entory points to find next cell. */
	ray_bbox(rayorg, raydir, min, max, tvals);

#if 0
	printf("cell = (%f, %f, %f)-(%f, %f, %f)\n",
		min[0], min[1], min[2],
		max[0], max[1], max[2]);
	printf("ray = (%f, %f, %f)->(%f, %f, %f)\n",
		rayorg[0], rayorg[1], rayorg[2],
		raydir[0], raydir[1], raydir[2]);
	printf("tvals = %f, %f\n", tvals[0], tvals[1]);
#endif
#if 0
	p[0] = rayorg[0] + (tvals[0] + 0.000001) * raydir[0];
	p[1] = rayorg[1] + (tvals[0] + 0.000001) * raydir[1];
	p[2] = rayorg[2] + (tvals[0] + 0.000001) * raydir[2];
	printf("start 0p = (%f, %f, %f)\n",
		p[0], p[1], p[2]);
#endif
	p[0] = rayorg[0] + (tvals[1] + 0.000001) * raydir[0];
	p[1] = rayorg[1] + (tvals[1] + 0.000001) * raydir[1];
	p[2] = rayorg[2] + (tvals[1] + 0.000001) * raydir[2];
#if 0
	printf("start p = (%f, %f, %f), t = %f\n",
		p[0], p[1], p[2], tvals[1]);
#endif

	maxdepth = octree->maxdepth;

	min_cell(rayorg, maxdepth, mmin, mmax);

#if 0
	printf("mincell = (%f, %f, %f)-(%f, %f, %f)\n",
		mmin[0], mmin[1], mmin[2],
		mmax[0], mmax[1], mmax[2]);
#endif

	if (fabs(raydir[0]) < DIREPS) {
		deltax = 0.0;
		nextx  = RI_INFINITY;
	} else if (dir.e[0] > 0.0) {
		deltax = dir.e[0] / (float)(1 << maxdepth);
		nextx = (max[0] - rayorg[0]) / raydir[0];
		travx = (mmax[0] - rayorg[0]) / raydir[0];
	} else {
		deltax = -dir.e[0] / (float)(1 << maxdepth);
		nextx = -(rayorg[0] - min[0]) / raydir[0];
		travx = -(rayorg[0] - mmin[0]) / raydir[0];
	}

	if (fabs(raydir[1]) < DIREPS) {
		deltay = 0.0;
		nexty  = RI_INFINITY;
	} else if (dir.e[1] > 0.0) {
		deltay = dir.e[1] / (float)(1 << maxdepth);
		nexty = (max[1] - rayorg[1]) / raydir[1];
		travy = (mmax[1] - rayorg[1]) / raydir[1];
	} else {
		deltay = -dir.e[1] / (float)(1 << maxdepth);
		nexty = -(rayorg[1] - min[1]) / raydir[1];
		travy = -(rayorg[1] - mmin[1]) / raydir[1];
	}

	if (fabs(raydir[2]) < DIREPS) {
		deltaz = 0.0;
		nextz  = RI_INFINITY;
	} else if (dir.e[2] > 0.0) {
		deltaz = dir.e[2] / (float)(1 << maxdepth);
		nextz = (max[2] - rayorg[2]) / raydir[2];
		travz = (mmax[2] - rayorg[2]) / raydir[2];
	} else {
		deltaz = -dir.e[2] / (float)(1 << maxdepth);
		nextz = -(rayorg[2] - min[2]) / raydir[2];
		travz = -(rayorg[2] - mmin[2]) / raydir[2];
	}


	if (cell->data) {

		tris = (ri_tri_list_t *)(cell->data);

		if (intersect_foreach_trilist(tris,
					      eye, dir,
					      &surfinfo, &isectt)) {
			//printf("hit t = %f\n", isectt);
			if (isectt > EPS && isectt <= nearest) {

				hit = 1;
				//printf("found hit\n");

				nearest = isectt;

				ri_mem_copy(info, &surfinfo,
				       sizeof(ri_surface_info_t));

				/* if intersection point is in this
				 * voxel, terminate traversal.
				 */
				ri_vector_copy(&isectpoint, dir);
				ri_vector_scale(&isectpoint, isectt);
				ri_vector_add(&isectpoint,
					       isectpoint,
					       eye);

				if (inside_cell(isectpoint,
						cell,
						octree->bmin,
						octree->invwidth)) {
					//printf("inside\n");
					(*t) = nearest;
					return hit; 
					//break;
				}
			}
		}
	}
	
	if ((nextx < nexty) && (nextx < nextz)) {
		//travx = nextx;
		p[0] = rayorg[0] + (nextx + 0.000001) * raydir[0];
		p[1] = rayorg[1] + (nextx + 0.000001) * raydir[1];
		p[2] = rayorg[2] + (nextx + 0.000001) * raydir[2];
		nexty = travy;
		nextz = travz;
	} else if ((nextz < nexty)) {
		//travz = nextz;
		p[0] = rayorg[0] + (nextz + 0.000001) * raydir[0];
		p[1] = rayorg[1] + (nextz + 0.000001) * raydir[1];
		p[2] = rayorg[2] + (nextz + 0.000001) * raydir[2];
		nextx = travx;
		nexty = travy;
	} else {
		//travy = nexty;
		p[0] = rayorg[0] + (nexty + 0.000001) * raydir[0];
		p[1] = rayorg[1] + (nexty + 0.000001) * raydir[1];
		p[2] = rayorg[2] + (nexty + 0.000001) * raydir[2];
		nextx = travx;
		nextz = travz;
	}

#if 1
	if (p[0] < 0 || p[0] >= 1.0) return 0;
	if (p[1] < 0 || p[1] >= 1.0) return 0;
	if (p[2] < 0 || p[2] >= 1.0) return 0;
#endif

	prevcell = cell;

	cell = otLocateCell(octree->cell, p);
	
	if (cell == prevcell) {
		return 0;
	}

	/* traversal loop */

	while (1) {
		if (cell->data) {

			tris = (ri_tri_list_t *)(cell->data);

			if (intersect_foreach_trilist(tris,
						      eye, dir,
						      &surfinfo, &isectt)) {
				if (isectt > EPS && isectt <= nearest) {

					hit = 1;
					//printf("found hit\n");

					nearest = isectt;

					ri_mem_copy(info, &surfinfo,
					       sizeof(ri_surface_info_t));

					/* if intersection point is in this
					 * voxel, terminate traversal.
					 */
					ri_vector_copy(&isectpoint, dir);
					ri_vector_scale(&isectpoint, isectt);
					ri_vector_add(&isectpoint,
						       isectpoint,
						       eye);

					if (inside_cell(isectpoint,
							cell,
							octree->bmin,
							octree->invwidth)) {
						//printf("inside\n");
						break;
					}
				}
			}
		}

		deltascale = (1 << (maxdepth-(OT_ROOT_LEVEL-cell->level))); 

		/* Find next cell to traverse. */
		//if ((nextx < nexty) && (nextx < nextz)) {
		if ((nextx + deltax * (float)deltascale <
		     nexty + deltay * (float)deltascale) &&
		    (nextx + deltax * (float)deltascale <
		     nextz + deltaz * (float)deltascale)) {
			//if (maxt < nextx) break;
			nextx += deltax * (float)deltascale;
			p[0] = rayorg[0] + (nextx + 0.000001) * raydir[0];
			p[1] = rayorg[1] + (nextx + 0.000001) * raydir[1];
			p[2] = rayorg[2] + (nextx + 0.000001) * raydir[2];
		} else if ((nextz + deltaz * (float)deltascale <
			    nexty + deltay * (float)deltascale)) {
			nextz += deltaz * (float)deltascale;
			p[0] = rayorg[0] + (nextz + 0.000001) * raydir[0];
			p[1] = rayorg[1] + (nextz + 0.000001) * raydir[1];
			p[2] = rayorg[2] + (nextz + 0.000001) * raydir[2];
		} else {
			nexty += deltay * (float)deltascale;
			p[0] = rayorg[0] + (nexty + 0.000001) * raydir[0];
			p[1] = rayorg[1] + (nexty + 0.000001) * raydir[1];
			p[2] = rayorg[2] + (nexty + 0.000001) * raydir[2];

			nexty += deltay * (float)deltascale;
		}

#if 1
		if (p[0] < 0 || p[0] >= 1.0) break;
		if (p[1] < 0 || p[1] >= 1.0) break;
		if (p[2] < 0 || p[2] >= 1.0) break;
#endif

		prevcell = cell;

		cell = otLocateCell(octree->cell, p);

		if (cell == prevcell) {
			break;
		}

		
	}

	(*t) = nearest;
 
	return hit;

}
#else
static int
intersect_with_octree(ri_octree_accel_t *octree,
		      const ri_vector_t *eye, const ri_vector_t *dir,
		      ri_surface_info_t *info, float *t)
{
	int                hit = 0;
	float              rayt = (float)RI_INFINITY;
	float              maxt = (float)RI_INFINITY;
	float              tvals[2];
	float              raypos[3];
	float              min[3], max[3];
	float              rayorg[3], raydir[3], invraydir[3];
	float              p[3];
	ri_surface_info_t  surfinfo;
	ri_vector_t        ndir;
	ri_vector_t        isectpoint;
	float              nearest = (float)RI_INFINITY;
	float              isectt;
	otCell            *cell, *prevcell;
	ri_tri_list_t     *tris;
	
#ifdef DEBUG
	if (dir->e[0] == 0.0 && dir->e[1] == 0.0 && dir->e[2] == 0.0) return 0; 
#endif

	ri_vector_copy(&ndir, dir);
	ri_vector_normalize(&ndir);

	/* find ray's entry point. */
	if (inside(eye, octree->bmin, octree->bmax)) {
		rayt = 0.0;
	} else {
		if (!intersect_ray_bbox(eye, &ndir,
					octree->bmin, octree->bmax, tvals)) {
			return 0;
		}
		rayt = tvals[0];
		maxt = tvals[1];
	}	

	raypos[0] = eye->e[0] + ndir.e[0] * rayt;
	raypos[1] = eye->e[1] + ndir.e[1] * rayt;
	raypos[2] = eye->e[2] + ndir.e[2] * rayt;

	raydir[0] = ndir.e[0];
	raydir[1] = ndir.e[1];
	raydir[2] = ndir.e[2];

	if (raydir[0] != 0.0) {
		invraydir[0] = 1.0 / raydir[0];
	} else {
		invraydir[0] = 0.0;
	}

	if (raydir[1] != 0.0) {
		invraydir[1] = 1.0 / raydir[1];
	} else {
		invraydir[1] = 0.0;
	}

	if (raydir[2] != 0.0) {
		invraydir[2] = 1.0 / raydir[2];
	} else {
		invraydir[2] = 0.0;
	}


	/* Normalize ray poistion to [0, 1)^3 */
	rayorg[0] = (raypos[0] - octree->bmin[0]) * octree->invwidth[0];
	rayorg[1] = (raypos[1] - octree->bmin[1]) * octree->invwidth[1];
	rayorg[2] = (raypos[2] - octree->bmin[2]) * octree->invwidth[2];
	if (rayorg[0] <  0.0) rayorg[0] = 0.0;
	if (rayorg[1] <  0.0) rayorg[1] = 0.0;
	if (rayorg[2] <  0.0) rayorg[2] = 0.0;
	if (rayorg[0] >= 1.0) rayorg[0] = 0.999999;
	if (rayorg[1] >= 1.0) rayorg[1] = 0.999999;
	if (rayorg[2] >= 1.0) rayorg[2] = 0.999999;

	/* Locate the cell ray origin contains. */
	cell = otLocateCell(octree->cell, rayorg);
	if (!cell) {
		/* ??? */
		fprintf(stderr, "??? Can't locate cell.\n");
		return 0;
	}

	/* Get octree cell's bounding box. */
	get_octree_bb(cell, min, max);

	/* Get the entory points to find next cell. */
	ray_bbox(rayorg, invraydir, min, max, tvals);

#if 0
	printf("cell = (%f, %f, %f)-(%f, %f, %f)\n",
		min[0], min[1], min[2],
		max[0], max[1], max[2]);
	printf("ray = (%f, %f, %f)->(%f, %f, %f)\n",
		rayorg[0], rayorg[1], rayorg[2],
		raydir[0], raydir[1], raydir[2]);
	printf("tvals = %f, %f\n", tvals[0], tvals[1]);
#endif

	p[0] = rayorg[0] + (tvals[1] + 0.000001) * raydir[0];
	p[1] = rayorg[1] + (tvals[1] + 0.000001) * raydir[1];
	p[2] = rayorg[2] + (tvals[1] + 0.000001) * raydir[2];
#if 0
	printf("start p = (%f, %f, %f), t = %f\n",
		p[0], p[1], p[2], tvals[1]);
#endif

#if 0
	if (p[0] < 0 || p[0] >= 1.0) return 0;
	if (p[1] < 0 || p[1] >= 1.0) return 0;
	if (p[2] < 0 || p[2] >= 1.0) return 0;
#endif


	/* traversal loop */

	prevcell = cell;
	while (1) {
		if (cell->data) {

			tris = (ri_tri_list_t *)(cell->data);

			if (intersect_foreach_trilist(tris,
						      eye, dir,
						      &surfinfo, &isectt)) {
				if (isectt > EPS && isectt <= nearest) {

					hit = 1;
					//printf("found hit\n");

					nearest = isectt;

					ri_mem_copy(info, &surfinfo,
					       sizeof(ri_surface_info_t));

					/* if intersection point is in this
					 * voxel, terminate traversal.
					 */
					ri_vector_copy(&isectpoint, dir);
					ri_vector_scale(&isectpoint, isectt);
					ri_vector_add(&isectpoint,
						      &isectpoint,
						       eye);

					if (inside_cell(&isectpoint,
							cell,
							octree->bmin,
							octree->invwidth)) {
						//printf("inside\n");
						break;
					}
				}
			}
		}

		get_octree_bb(cell, min, max);
		ray_bbox(rayorg, invraydir, min, max, tvals);

#if 0
		printf("cell = (%f, %f, %f)-(%f, %f, %f)\n",
			min[0], min[1], min[2],
			max[0], max[1], max[2]);
		printf("ray = (%f, %f, %f)->(%f, %f, %f)\n",
			rayorg[0], rayorg[1], rayorg[2],
			raydir[0], raydir[1], raydir[2]);
		printf("tvals = %f, %f\n", tvals[0], tvals[1]);
#endif

		p[0] = rayorg[0] + (tvals[1] + 0.000001) * raydir[0];
		p[1] = rayorg[1] + (tvals[1] + 0.000001) * raydir[1];
		p[2] = rayorg[2] + (tvals[1] + 0.000001) * raydir[2];
#if 0
		printf("start p = (%f, %f, %f), t = %f\n",
			p[0], p[1], p[2], tvals[1]);
#endif

#if 1
		if (p[0] < 0 || p[0] >= 1.0) return 0;
		if (p[1] < 0 || p[1] >= 1.0) return 0;
		if (p[2] < 0 || p[2] >= 1.0) return 0;
#endif

		cell = otLocateCell(octree->cell, p);
		
		if (cell == prevcell) {
			return 0;
		}


		prevcell = cell;
	}

	(*t) = nearest;
 
	return hit;

}
#endif

static int 
foreach_polygon_in_geom(ri_geom_t *geom,
			const ri_vector_t *eye, const ri_vector_t *dir,
			ri_surface_info_t *info, float *t)
{
	int          i;
	int          hit = 0;
	int          hashit = 0;
	int          npolys;
	unsigned int i0, i1, i2;
	ri_vector_t  v0, v1, v2;
	ri_vector_t  n0, n1, n2;
	ri_vector_t  c0, c1, c2;
	ri_vector_t  scale;
	ri_vector_t  tmpbasis[3];
	RtFloat      u, v;
	RtColor      defcol = {1.0, 1.0, 1.0};
	RtFloat      defopa = 1.0;
	RtFloat      nearest = RI_INFINITY;


	npolys = geom->nindices / 3;

	if (npolys == 0) return 0;

	for (i = 0; i < npolys; i++) {
		i0 = geom->indices[3 * i + 0];
		i1 = geom->indices[3 * i + 1];
		i2 = geom->indices[3 * i + 2];

		v0 = geom->positions[i0];
		v1 = geom->positions[i1];
		v2 = geom->positions[i2];

		hit = triangle_intersect(eye, dir, &v0, &v1, &v2, t, &u, &v);
		//g_ntesttris++;
		ri_render_get()->stat.ntesttris++;

		if (hit) {
			if ((*t) > EPS && (*t) < nearest) {
				/* update nearest surface information */
				nearest = (*t);
				hashit  = 1;
				ri_vector_copy(&scale, dir);
				ri_vector_scale(&scale, (*t));
				ri_vector_add(&(info->pos), eye, &scale); 
				if (geom->normals) {
					n0 = geom->normals[i0];
					n1 = geom->normals[i1];
					n2 = geom->normals[i2];

					lerp_normal(&(info->normal),
						    &n0, &n1, &n2, u, v);

					if (geom->tangents &&
					    geom->binormals) {
						n0 = geom->tangents[i0];
						n1 = geom->tangents[i1];
						n2 = geom->tangents[i2];

						lerp_normal(&(info->tangent),
							    &n0, &n1, &n2, u, v);

						n0 = geom->binormals[i0];
						n1 = geom->binormals[i1];
						n2 = geom->binormals[i2];

						lerp_normal(&(info->binormal),
							    &n0, &n1, &n2, u, v);
					} else {

						ri_ortho_basis(tmpbasis,
							       &info->normal);
						ri_vector_copy(&(info->tangent),
							       &tmpbasis[0]);
						ri_vector_copy(&(info->binormal),
							       &tmpbasis[1]);
					}
					
				} else {
					calculate_normal(&(info->normal),
							 &v0, &v1, &v2);
					ri_ortho_basis(tmpbasis, &info->normal);
					ri_vector_copy(&(info->tangent),
						       &tmpbasis[0]);
					ri_vector_copy(&(info->binormal),
						       &tmpbasis[1]);
				}

				if (geom->colors) {
					c0 = geom->colors[i0];
					c1 = geom->colors[i1];
					c2 = geom->colors[i2];

					lerp_normal(&(info->color),
						    &c0, &c1, &c2, u, v);
					//info->color = geom->colors[i0];
				} else {
					ri_vector_set(&(info->color), defcol); 
				}

				if (geom->opacities) {
					info->opacity = geom->opacities[i0].e[0];
				} else {
					info->opacity = defopa; 
				}

				if (geom->texcoords) {
					lerp_uv(&info->u, &info->v,
						&geom->texcoords[2 * i0],
						&geom->texcoords[2 * i1],
						&geom->texcoords[2 * i2],
						u, v);
				} else {
					info->u = 0.0; 
					info->v = 0.0; 
				}
				info->geom = geom;
				info->index = 3 * i;

				info->kd = geom->kd;
				info->ks = geom->ks;
			}
		}
	}

	return hashit;
}

static void
calculate_normal(ri_vector_t *n,
		 const ri_vector_t *v0,
		 const ri_vector_t *v1,
		 const ri_vector_t *v2)
{
	ri_vector_t v01, v02;

	ri_vector_sub(&v01, v1, v0);
	ri_vector_sub(&v02, v2, v0);
	ri_vector_cross3(n, &v01, &v02);
	ri_vector_normalize(n);
}

static int
inside(const ri_vector_t *pos, const float bmin[3], const float bmax[3])
{
	if (pos->e[0] < bmin[0]) return 0;
	if (pos->e[1] < bmin[1]) return 0;
	if (pos->e[2] < bmin[2]) return 0;

	if (pos->e[0] > bmax[0]) return 0;
	if (pos->e[1] > bmax[1]) return 0;
	if (pos->e[2] > bmax[2]) return 0;

	return 1;
}

static int
intersect_ray_bbox(const ri_vector_t *eye, const ri_vector_t *dir,
		   const float bmin[3], const float bmax[3], float isectt[2])
{
	float t0, t1;
	float invraydir;
	float neart, fart;
	float tmp;

	t0 = 0.0; t1 = RI_INFINITY;

	if (dir->e[0] == 0.0 && dir->e[1] == 0.0 && dir->e[2] == 0.0) return 0;

	if (dir->e[0] != 0.0) {
		invraydir = 1.0 / dir->e[0];

		neart = (bmin[0] - eye->e[0]) * invraydir;
		fart  = (bmax[0] - eye->e[0]) * invraydir;
		if (neart > fart) {
			tmp = neart; neart = fart; fart = tmp;
		}

		t0 = neart > t0 ? neart : t0;
		t1 = fart  < t1 ? fart  : t1;
		if (t0 > t1) return 0;
	}

	if (dir->e[1] != 0.0) {
		invraydir = 1.0 / dir->e[1];

		neart = (bmin[1] - eye->e[1]) * invraydir;
		fart  = (bmax[1] - eye->e[1]) * invraydir;
		if (neart > fart) {
			tmp = neart; neart = fart; fart = tmp;
		}

		t0 = neart > t0 ? neart : t0;
		t1 = fart  < t1 ? fart  : t1;
		if (t0 > t1) return 0;
	}

	if (dir->e[2] != 0.0) {
		invraydir = 1.0 / dir->e[2];

		neart = (bmin[2] - eye->e[2]) * invraydir;
		fart  = (bmax[2] - eye->e[2]) * invraydir;
		if (neart > fart) {
			tmp = neart; neart = fart; fart = tmp;
		}

		t0 = neart > t0 ? neart : t0;
		t1 = fart  < t1 ? fart  : t1;
		if (t0 > t1) return 0;
	}

	isectt[0] = t0;
	isectt[1] = t1;

	return 1;
}

static int
intersect_foreach_trilist(ri_tri_list_t *list,
			  const ri_vector_t *eye, const ri_vector_t *dir,
			  ri_surface_info_t *info, float *t)
{
	int            i;
	int            hit = 0;
	int	       hashit = 0;
	float          isectt;
	unsigned int   hitindex = 0;
	unsigned int   i0, i1, i2;
	unsigned int   hi0 = 0, hi1 = 0, hi2 = 0;
	ri_vector_t    *v0, *v1, *v2;
	ri_vector_t    *n0, *n1, *n2;
	ri_vector_t    *c0, *c1, *c2;
	ri_vector_t    scale;
	ri_vector_t    tmpbasis[3];
	RtFloat	       u, v;
	RtFloat	       hitu = 0.0f, hitv = 0.0f;
	RtFloat	       nearest = RI_INFINITY;
	RtColor	       defcol = {1.0, 1.0, 1.0};
	RtFloat	       defopa = 1.0;
	ri_geom_t     * RESTRICT geom;
	ri_geom_t     * RESTRICT hitgeom = NULL;
	ri_tri_info_t * RESTRICT triinfo;

	(*t) = -1.0;

	for (i = 0; i < list->ntris; i++) {
		triinfo = &list->tris[i];	

		geom = triinfo->geom;

#if 0
		if (triinfo->index >= geom->nindices) {
			printf("triinfo->index = %d\n", triinfo->index);
			exit(-1);
		}
#endif

		i0 = geom->indices[triinfo->index + 0];
		i1 = geom->indices[triinfo->index + 1];
		i2 = geom->indices[triinfo->index + 2];
		v0 = &geom->positions[i0];
		v1 = &geom->positions[i1];
		v2 = &geom->positions[i2];

		hit = triangle_intersect(eye, dir, v0, v1, v2, &isectt, &u, &v);
		//g_ntesttris++;
		ri_render_get()->stat.ntesttris++;
		if (hit) {
			if (isectt > EPS && isectt < nearest) {
				/* update nearest surface information */
				nearest = isectt;
				hashit  = 1;


				hi0 = i0;
				hi1 = i1;
				hi2 = i2;
				hitu = u; hitv = v;
				hitgeom = geom;
				hitindex = triinfo->index;
			}
		}
	}

	if (hashit) {
		
		(*t) = nearest;

		ri_vector_copy(&scale, dir);
		ri_vector_scale(&scale, nearest);
		ri_vector_add(&(info->pos), eye, &scale); 
		if (hitgeom->normals) {
			n0 = &hitgeom->normals[hi0];
			n1 = &hitgeom->normals[hi1];
			n2 = &hitgeom->normals[hi2];

			lerp_normal(&(info->normal),
				    n0, n1, n2, hitu, hitv);

			if (hitgeom->tangents &&
			    hitgeom->binormals) {
				n0 = &hitgeom->tangents[hi0];
				n1 = &hitgeom->tangents[hi1];
				n2 = &hitgeom->tangents[hi2];

				lerp_normal(&(info->tangent),
					    n0, n1, n2, hitu, hitv);

				n0 = &hitgeom->binormals[hi0];
				n1 = &hitgeom->binormals[hi1];
				n2 = &hitgeom->binormals[hi2];

				lerp_normal(&(info->binormal),
					    n0, n1, n2, hitu, hitv);
			} else {

				ri_ortho_basis(tmpbasis,
					       &info->normal);
				ri_vector_copy(&(info->tangent),
					       &tmpbasis[0]);
				ri_vector_copy(&(info->binormal),
					       &tmpbasis[1]);

			}
		} else {
			v0 = &hitgeom->positions[hi0];
			v1 = &hitgeom->positions[hi1];
			v2 = &hitgeom->positions[hi2];
			calculate_normal(&(info->normal),
					 v0, v1, v2);
			ri_ortho_basis(tmpbasis, &info->normal);
			ri_vector_copy(&(info->tangent),
				       &tmpbasis[0]);
			ri_vector_copy(&(info->binormal),
				       &tmpbasis[1]);
		}

		if (hitgeom->texcoords) {
			lerp_uv(&info->u, &info->v,
				&hitgeom->texcoords[2 * hi0],
				&hitgeom->texcoords[2 * hi1],
				&hitgeom->texcoords[2 * hi2],
				hitu, hitv);
		} else {
			info->u = 0.0; 
			info->v = 0.0; 
		}

		if (hitgeom->colors) {
			c0 = &(hitgeom->colors[hi0]);
			c1 = &(hitgeom->colors[hi1]);
			c2 = &(hitgeom->colors[hi2]);

			lerp_normal(&(info->color),
				    c0, c1, c2, hitu, hitv);
			//info->color = hitgeom->colors[hi0];
		} else {
			ri_vector_set(&(info->color), defcol); 
		}

		if (hitgeom->opacities) {
			info->opacity =
				hitgeom->opacities[hi0].e[0];
		} else {
			info->opacity = defopa; 
		}

		info->geom = hitgeom;
		info->index = hitindex;
		info->kd = hitgeom->kd;
		info->ks = hitgeom->ks;
	}
	
	return hashit;
}

static void
lerp_normal(ri_vector_t *n,
	    const ri_vector_t *n0, const ri_vector_t *n1, const ri_vector_t *n2,
	    float u, float v)
{
	ri_vector_t ns0, ns1, ns2;

	/* n = (1 - u - v) n0 + u n1 + v n2 */

	ri_vector_copy(&ns0, n0);
	ri_vector_scale(&ns0, (1.0 - u - v));
	ri_vector_copy(&ns1, n1);
	ri_vector_scale(&ns1, u);
	ri_vector_copy(&ns2, n2);
	ri_vector_scale(&ns2, v);

	ri_vector_add(n, &ns0, &ns1);
	ri_vector_add(n, n, &ns2);
}


static int
inside_voxel(const ri_vector_t *pos, int x, int y, int z,
	     const ri_ugrid_t *grid)
{
	float bmin[3];
	float bmax[3];

	bmin[0] = x * grid->width[0] + grid->bboxmin[0];
	bmin[1] = y * grid->width[1] + grid->bboxmin[1];
	bmin[2] = z * grid->width[2] + grid->bboxmin[2];

	bmax[0] = (x + 1) * grid->width[0] + grid->bboxmin[0];
	bmax[1] = (y + 1) * grid->width[1] + grid->bboxmin[1];
	bmax[2] = (z + 1) * grid->width[2] + grid->bboxmin[2];

	if (inside(pos, bmin, bmax)) return 1;

	return 0;
}

static int
ray_box_intersect(const ri_ray_t *ray,
		  const ri_vector_t *min, const ri_vector_t *max,
		  float *retmin, float *retmax)
{
	/* todo: implemtent */
	float t0, t1;
	float invraydir;
	float neart, fart;
	float tmp;

	t0 = 0.0f; t1 = (float)RI_INFINITY;

	if (ray->dir.e[0] == 0.0 && ray->dir.e[1] == 0.0 && ray->dir.e[2] == 0.0) {
		return 0;
	}

	if (ray->dir.e[0] != 0.0) {
		invraydir = 1.0f / ray->dir.e[0];

		neart = (min->e[0] - ray->org.e[0]) * invraydir;
		fart  = (max->e[0] - ray->org.e[0]) * invraydir;
		if (neart > fart) {
			tmp = neart; neart = fart; fart = tmp;
		}

		t0 = neart > t0 ? neart : t0;
		t1 = fart  < t1 ? fart  : t1;
		if (t0 > t1) return 0;
	}

	if (ray->dir.e[1] != 0.0) {
		invraydir = 1.0f / ray->dir.e[1];

		neart = (min->e[1] - ray->org.e[1]) * invraydir;
		fart  = (max->e[1] - ray->org.e[1]) * invraydir;
		if (neart > fart) {
			tmp = neart; neart = fart; fart = tmp;
		}

		t0 = neart > t0 ? neart : t0;
		t1 = fart  < t1 ? fart  : t1;
		if (t0 > t1) return 0;
	}

	if (ray->dir.e[2] != 0.0) {
		invraydir = 1.0f / ray->dir.e[2];

		neart = (min->e[2] - ray->org.e[2]) * invraydir;
		fart  = (max->e[2] - ray->org.e[2]) * invraydir;
		if (neart > fart) {
			tmp = neart; neart = fart; fart = tmp;
		}

		t0 = neart > t0 ? neart : t0;
		t1 = fart  < t1 ? fart  : t1;
		if (t0 > t1) return 0;
	}

	(*retmin) = t0;
	(*retmax) = t1;

	return 1;

}



static int
intersect_with_bsp(ri_bsp_t *bsp,
		   const ri_ray_t *ray,
		   ri_surface_info_t *info, float *t)
{
	int           hit;
	long          i0, i1, i2;
	float         u, v;
	ri_tri_info_t tri;
	ri_vector_t   vec;
	ri_vector_t   defcol;
	ri_vector_t   *v0, *v1, *v2;
	ri_vector_t   *n0, *n1, *n2;
	ri_vector_t   *c0, *c1, *c2;
	ri_vector_t   tmpbasis[3];

	hit = bsp_intersect(bsp, ray, &tri, t, &u, &v);

	if (!hit) {
		return 0;
	}

	ri_vector_copy(&vec, &ray->dir);
	ri_vector_scale(&vec, (*t));
	ri_vector_add(&(info->pos), &ray->org, &vec);

	if (!tri.geom) {
		printf("????\n");
		return 0;
	}

	i0 = tri.geom->indices[3 * tri.index + 0];
	i1 = tri.geom->indices[3 * tri.index + 1];
	i2 = tri.geom->indices[3 * tri.index + 2];

	if (tri.geom->normals) {
		n0 = &(tri.geom->normals[i0]);
		n1 = &(tri.geom->normals[i1]);
		n2 = &(tri.geom->normals[i2]);

		lerp_normal(&(info->normal), n0, n1, n2, u, v);

		if (tri.geom->tangents &&
		    tri.geom->binormals) {
			n0 = &(tri.geom->tangents[i0]);
			n1 = &(tri.geom->tangents[i1]);
			n2 = &(tri.geom->tangents[i2]);

			lerp_normal(&(info->tangent), n0, n1, n2, u, v);

			n0 = &(tri.geom->binormals[i0]);
			n1 = &(tri.geom->binormals[i1]);
			n2 = &(tri.geom->binormals[i2]);

			lerp_normal(&(info->binormal), n0, n1, n2, u, v);
		} else {
			ri_ortho_basis(tmpbasis, &info->normal);
			ri_vector_copy(&(info->tangent),
				       &tmpbasis[0]);
			ri_vector_copy(&(info->binormal),
				       &tmpbasis[1]);
		}

	} else {
		v0 = &(tri.geom->positions[i0]);
		v1 = &(tri.geom->positions[i1]);
		v2 = &(tri.geom->positions[i2]);
		calculate_normal(&(info->normal), v0, v1, v2);

		ri_ortho_basis(tmpbasis, &info->normal);
		ri_vector_copy(&(info->tangent),
			       &tmpbasis[0]);
		ri_vector_copy(&(info->binormal),
			       &tmpbasis[1]);
	}

	if (tri.geom->colors) {
		c0 = &(tri.geom->colors[i0]);
		c1 = &(tri.geom->colors[i1]);
		c2 = &(tri.geom->colors[i2]);

		lerp_normal(&(info->color), c0, c1, c2, u, v);
	} else {
		defcol.e[0] = 1.0;
		defcol.e[1] = 1.0;
		defcol.e[2] = 1.0;
		defcol.e[3] = 1.0;
		ri_vector_copy(&(info->color), &defcol);
	}

	if (tri.geom->opacities) {
		info->opacity = tri.geom->opacities[i0].e[0];
	} else {
		info->opacity = 1.0;
	}

	info->geom  = tri.geom;
	info->index = tri.index;
	info->kd    = tri.geom->kd;
	info->ks    = tri.geom->ks;

	return 1;
}

static int
intersect_with_kdtree(ri_kdtree_t *kd,
		   const ri_ray_t *ray,
		   ri_surface_info_t *info, float *t)
{
	int           is_hit;
	long          i0, i1, i2;
	float         u = 0, v = 0;
	ri_tri_info_t tri;
	ri_vector_t   vec;
	ri_vector_t   defcol;
	ri_vector_t   *v0, *v1, *v2;
	ri_vector_t   *n0, *n1, *n2;
	ri_vector_t   *c0, *c1, *c2;
	ri_vector_t   tmpbasis[3];
	
	ray_t         tmpray;
	float         mint = 0.0;
	float         maxt = RI_INFINITY;
	hit_t         hitrec;
	ri_geom_t     *geom;

	ri_vector_copy(&tmpray.org, &ray->org);
	ri_vector_copy(&tmpray.dir, &ray->dir);


	is_hit = ri_kdtree_traverse(kd, &tmpray, &hitrec, mint, maxt);

	if (!is_hit) {
		return 0;
	}

	(*t) = hitrec.t;

	// Hit point = R.org + t * R.dir
	ri_vector_copy(&vec, &ray->dir);
	ri_vector_scale(&vec, hitrec.t);
	ri_vector_add(&(info->pos), &ray->org, &vec);

	geom = hitrec.geom;
	if (!geom) {
		printf("????\n");
		return 0;
	}

	i0 = geom->indices[3 * hitrec.index + 0];
	i1 = geom->indices[3 * hitrec.index + 1];
	i2 = geom->indices[3 * hitrec.index + 2];

	if (geom->normals) {
		n0 = &(geom->normals[i0]);
		n1 = &(geom->normals[i1]);
		n2 = &(geom->normals[i2]);

		lerp_normal(&(info->normal), n0, n1, n2, u, v);

		if (geom->tangents &&
		    geom->binormals) {
			n0 = &(geom->tangents[i0]);
			n1 = &(geom->tangents[i1]);
			n2 = &(geom->tangents[i2]);

			lerp_normal(&(info->tangent), n0, n1, n2, u, v);

			n0 = &(geom->binormals[i0]);
			n1 = &(geom->binormals[i1]);
			n2 = &(geom->binormals[i2]);

			lerp_normal(&(info->binormal), n0, n1, n2, u, v);
		} else {
			ri_ortho_basis(tmpbasis, &info->normal);
			ri_vector_copy(&(info->tangent),
				       &tmpbasis[0]);
			ri_vector_copy(&(info->binormal),
				       &tmpbasis[1]);
		}

	} else {
		v0 = &(geom->positions[i0]);
		v1 = &(geom->positions[i1]);
		v2 = &(geom->positions[i2]);
		calculate_normal(&(info->normal), v0, v1, v2);

		ri_ortho_basis(tmpbasis, &info->normal);
		ri_vector_copy(&(info->tangent),
			       &tmpbasis[0]);
		ri_vector_copy(&(info->binormal),
			       &tmpbasis[1]);
	}

	if (geom->colors) {
		c0 = &(geom->colors[i0]);
		c1 = &(geom->colors[i1]);
		c2 = &(geom->colors[i2]);

		lerp_normal(&(info->color), c0, c1, c2, u, v);
	} else {
		defcol.e[0] = 1.0;
		defcol.e[1] = 1.0;
		defcol.e[2] = 1.0;
		defcol.e[3] = 1.0;
		ri_vector_copy(&(info->color), &defcol);
	}

	if (geom->opacities) {
		info->opacity = geom->opacities[i0].e[0];
	} else {
		info->opacity = 1.0;
	}

	info->geom  = geom;
	info->index = hitrec.index;
	info->kd    = geom->kd;
	info->ks    = geom->ks;

	return 1;
}

#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
static int
intersect_with_accel_simd(const ri_ugrid_t *ugrid,
			  const ri_ray_t *ray,
			  ri_surface_info_t *info, float *t)
{
	int x, y, z;
	int hit;
	int i;
	int index;
	const ri_vector_t *eye, *dir;
	float rayt = (float)RI_INFINITY;
	float maxt = (float)RI_INFINITY;
	float tvals[2];
	float raypos[3];
	double nextx, nexty, nextz;
	double deltax, deltay, deltaz;
	int stepx = 0, stepy = 0, stepz = 0;
	int outx, outy, outz;
	ri_vector_t isectpoint;
	ri_tri_info_t tri;
	float nearest = (float)RI_INFINITY;
	float isectt;
	float u, v;
	const ri_ugrid_t *grid;
	ri_render_t *render;
	int tnum;

	int   hitindex = -1;
	float hitu = 0.0f, hitv = 0.0f;
	ri_geom_t * RESTRICT hitgeom = NULL;

	int   blkwidth  = ugrid->blkwidth;
	int   blksize   = ugrid->blksize;
	int   blkshift  = ugrid->shiftsize;
	int   blkmask   = ugrid->bitmask;

	eye = &ray->org;
	dir = &ray->dir;

	if (dir->e[0] == 0.0 && dir->e[1] == 0.0 && dir->e[2] == 0.0) return 0; 

	grid = ugrid;

	if (inside(eye, grid->bboxmin, grid->bboxmax)) {
		rayt = 0.0;
	} else {
		if (!intersect_ray_bbox(eye, dir,
					grid->bboxmin, grid->bboxmax, tvals)) {
			return 0;
		}
		rayt = tvals[0];
		maxt = tvals[1];
	}	

	raypos[0] = eye->e[0] + dir->e[0] * rayt;
	raypos[1] = eye->e[1] + dir->e[1] * rayt;
	raypos[2] = eye->e[2] + dir->e[2] * rayt;

	tnum = ray->thread_num;
	assert(tnum < RI_MAX_THREADS);

	for (i = 0; i < 4; i++) {
		simd_raydata.aligned[24 * tnum +  0 + i] = dir->e[0];
		simd_raydata.aligned[24 * tnum +  4 + i] = dir->e[1];
		simd_raydata.aligned[24 * tnum +  8 + i] = dir->e[2];
		simd_raydata.aligned[24 * tnum + 12 + i] = eye->e[0];
		simd_raydata.aligned[24 * tnum + 16 + i] = eye->e[1];
		simd_raydata.aligned[24 * tnum + 20 + i] = eye->e[2];
	}

	x = (int)((raypos[0] - grid->bboxmin[0]) * grid->invwidth[0]);
	if (x >= grid->voxels[0]) x = grid->voxels[0] - 1;

	if (fabs(dir->e[0]) < DIREPS) {
		nextx  = RI_INFINITY;
		deltax = 0.0;
		stepx  = 0;
		outx   = -1;
	} else if (dir->e[0] > 0) {
		nextx = rayt +
			(((x + 1) * grid->width[0] + grid->bboxmin[0]) -
			 raypos[0]) / dir->e[0];
	 
		deltax = grid->width[0] / dir->e[0];
		stepx = 1;
		outx = grid->voxels[0];
	} else {
		nextx = rayt +
			((x * grid->width[0] + grid->bboxmin[0]) -
			 raypos[0]) / dir->e[0];
	 
		deltax = -grid->width[0] / dir->e[0];
		stepx = -1;
		outx = -1;
	}

	y = (int)((raypos[1] - grid->bboxmin[1]) * grid->invwidth[1]);
	if (y >= grid->voxels[1]) y = grid->voxels[1] - 1;

	if (fabs(dir->e[1]) < DIREPS) {
		nexty  = RI_INFINITY;
		deltay = 0.0;
		stepy  = 0;
		outy   = -1;
	} else if (dir->e[1] > 0) {
		nexty = rayt +
			(((y + 1) * grid->width[1] + grid->bboxmin[1]) -
			 raypos[1]) / dir->e[1];
	 
		deltay = grid->width[1] / dir->e[1];
		stepy = 1;
		outy = grid->voxels[1];
	} else {
		nexty = rayt +
			((y * grid->width[1] + grid->bboxmin[1]) -
			 raypos[1]) / dir->e[1];
	 
		deltay = - grid->width[1] / dir->e[1];
		stepy = -1;
		outy = -1;
	}

	z = (int)((raypos[2] - grid->bboxmin[2]) * grid->invwidth[2]);
	if (z >= grid->voxels[2]) z = grid->voxels[2] - 1;

	if (fabs(dir->e[2]) < DIREPS) {
		nextz  = RI_INFINITY;
		deltaz = 0.0;
		stepz  = 0;
		outz   = -1;
	} else if (dir->e[2] > 0) {
		nextz = rayt +
			(((z + 1) * grid->width[2] + grid->bboxmin[2]) -
			 raypos[2]) / dir->e[2];
	 
		deltaz = grid->width[2] / dir->e[2];
		stepz = 1;
		outz = grid->voxels[2];
	} else {
		nextz = rayt +
			((z * grid->width[2] + grid->bboxmin[2]) -
			 raypos[2]) / dir->e[2];
	 
		deltaz = -grid->width[2] / dir->e[2];
		stepz = -1;
		outz = -1;
	}

	hit = 0;

	if (x < 0) x = 0;
	if (x > grid->voxels[0] - 1) x = grid->voxels[0] - 1;
	if (y < 0) y = 0;
	if (y > grid->voxels[1] - 1) y = grid->voxels[1] - 1;
	if (z < 0) z = 0;
	if (z > grid->voxels[2] - 1) z = grid->voxels[2] - 1;

	render = ri_render_get();


	while (1) {
		//if (g_profile) g_ngridtravcells++;
		//ri_render_get()->stat.ngridtravs++;
		render->stat.ngridtravs++;

		/* !HOT SPOT! cache miss occurs here with 95% or higher
		 * probability.
		 */
#if USE_ZORDER	/* use z order memory access */
		index = MAP_Z3D(x, y, z);
#else		/* blocked memory access */
		index = MAP_XYZ(x, y, z, blkshift, blksize, blkwidth, blkmask);
#endif
		//printf("index = %d, xyz = %d, %d, %d\n",  index, x, y, z);

		if (grid->cdat[index]) {
		//if (grid->cell[z][y][x]) {
			//printf("isect cell[%d][%d][%d]\n", z, y, x);
			//fflush(stdout);
			if (intersect_foreach_trilist_simd(
					//grid->cell[z][y][x]->simdtris,
					grid->cdat[index]->simdtris,
					&simd_raydata,
					tnum,
					&tri,
					&isectt, &u, &v)) {
				if (isectt > EPS && isectt <= nearest) {

					hit = 1;

					nearest = isectt;
				
					hitindex = tri.index;
					hitgeom  = tri.geom;
					hitu     = u;
					hitv     = v;

					/* if intersection point is in this
					 * voxel, terminate traversal.
					 */
					ri_vector_copy(&isectpoint, dir);
					ri_vector_scale(&isectpoint, isectt);
					ri_vector_add(&isectpoint,
						      &isectpoint,
						       eye);

					if (inside_voxel(&isectpoint, x, y, z,
							 grid)) {
						break;
					}
				}
			}
		}

		/* 3D DDA */
		if ((nextx < nexty) && (nextx < nextz)) {
			if (maxt < nextx) break;
			x += stepx;
			if (x == outx) break;
			nextx += deltax;
		} else if ((nextz < nexty)) {
			if (maxt < nextz) break;
			z += stepz;
			if (z == outz) break;
			nextz += deltaz;
		} else {
			if (maxt < nexty) break;
			y += stepy;
			if (y == outy) break;
			nexty += deltay;
		}
	}

	if (hit) {
		(*t) = nearest;
		build_surfinfo(info, &isectpoint, hitgeom, hitindex, hitu, hitv);
	}
 
#if 0
	ri_aligned_float_free(raydata);
	ri_mem_free(raydata);
#endif




	return hit;

}

static int
intersect_foreach_trilist_simd(const ri_simd_tri_info_t *list,
			       const ri_aligned_float_t *ray,
			       int tnum,
			       ri_tri_info_t *hittri,
			       float *t, float *u, float *v)
{
	int i; //, j;
	int hashit;
	float nearest = RI_INFINITY;

	float * RESTRICT hitt;
	float * RESTRICT hitu;
	float * RESTRICT hitv;

#ifdef WITH_ALTIVEC
	int ret;
	/* 1.0 * 2 ^(-17) = 1.9073e-6 */
	const vector float eps = vec_ctf(vec_splat_u32(1), 19);
	vector float       vt;
#endif

	(*t) = -1.0;
	hashit = 0;

	hitt = &(simd_hitt.aligned[4 * tnum]);
	hitu = &(simd_hitu.aligned[4 * tnum]);
	hitv = &(simd_hitv.aligned[4 * tnum]);

	ri_render_get()->stat.ntesttris++;
	for (i = 0; i < list->nblocks; i++) {
#ifdef WITH_SSE
		isect_sse(&ray->aligned[tnum * 24],
			  //list->tridata.aligned + 36 * i,
			  list->tridataptr + 36 * i,
			  hitt, hitu, hitv);
#else /* WITH_ALTIVEC */
		isect_altivec(&ray->aligned[tnum * 24],
			      //list->tridata.aligned + 36 * i,
			      list->tridataptr + 36 * i,
			      hitt, hitu, hitv);

#endif

#ifdef WITH_ALTIVEC
		/* To avoid expensive VEC -> FP register operation
		 * in subsequent pass,
		 * first ensure any of t's is greater than eps */
		vt = vec_ld(0, hitt);
		ret = vec_any_gt(vt, eps); 

		if (!ret) {
			continue;
		}
#endif
		


		/* unroll loop */
		/* for (j = 0; j < 4; j++) */

		/* (FP1 >= FP2)  ==  !(FP1 < FP2) */
		if (!(hitt[0] < EPS) &&
		    !(hitt[0] > nearest)) {
			(*t) = hitt[0];	
			(*u) = hitu[0];
			(*v) = hitv[0];
			nearest = hitt[0];
			hashit  = 1;

			hittri->index = list->indices[4*i+0];
			hittri->geom  = list->geoms[4*i+0];
		}

		if (!(hitt[1] < EPS) &&
		    !(hitt[1] > nearest)) {
			(*t) = hitt[1];	
			(*u) = hitu[1];
			(*v) = hitv[1];
			nearest = hitt[1];
			hashit  = 1;

			hittri->index = list->indices[4*i+1];
			hittri->geom  = list->geoms[4*i+1];
		}

		if (!(hitt[2] < EPS) &&
		    !(hitt[2] > nearest)) {
			(*t) = hitt[2];	
			(*u) = hitu[2];
			(*v) = hitv[2];
			nearest = hitt[2];
			hashit  = 1;

			hittri->index = list->indices[4*i+2];
			hittri->geom  = list->geoms[4*i+2];
		}

		if (!(hitt[3] < EPS) &&
		    !(hitt[3] > nearest)) {
			(*t) = hitt[3];	
			(*u) = hitu[3];
			(*v) = hitv[3];
			nearest = hitt[3];
			hashit  = 1;

			hittri->index = list->indices[4*i+3];
			hittri->geom  = list->geoms[4*i+3];
		}
	}

	return hashit;
}
#endif

#ifdef WITH_SSE
static void
isect_sse(const float *ray, const float *data, float *t, float *u, float *v)
{
	const __m128 one = _mm_set_ps1(1.0f + EPSILON);

	const __m128 one2 = _mm_set_ps1(1.0f);

	const __m128 zero = _mm_set_ps1(0.0f);

#if 0	/* original code. no buckface culling. */
	const __m128 eps2 = _mm_set_ps1(EPSILON2);
#else	/* do buckface culling. */
	const __m128 eps = _mm_set_ps1(EPSILON);
#endif

	/* loads the vector edge2(v3 - v1) from 4 triangles */
	const __m128 e2x = _mm_load_ps(data + 24);
	const __m128 e2y = _mm_load_ps(data + 28);
	const __m128 e2z = _mm_load_ps(data + 32);

	/* loads ray direction vector. */
	const __m128 rdx = _mm_load_ps(ray);
	const __m128 rdy = _mm_load_ps(ray + 4);
	const __m128 rdz = _mm_load_ps(ray + 8);

	/* loads the vector v1 from 4 triangles */
	const __m128 x1 = _mm_load_ps(data);
	const __m128 y1 = _mm_load_ps(data + 4);
	const __m128 z1 = _mm_load_ps(data + 8);

	/* loads the vector e1(v2 - v1) from 4 triangles. */
	const __m128 e1x = _mm_load_ps(data + 12);
	const __m128 e1y = _mm_load_ps(data + 16);
	const __m128 e1z = _mm_load_ps(data + 20);

	/* cross product p = d x e2 */
	const __m128 px = cross_sse(e2z, e2y, rdy, rdz);
	const __m128 py = cross_sse(e2x, e2z, rdz, rdx);
	const __m128 pz = cross_sse(e2y, e2x, rdx, rdy);

	/* loads the ray origin vector. */
	const __m128 rox = _mm_load_ps(ray + 12);
	const __m128 roy = _mm_load_ps(ray + 16);
	const __m128 roz = _mm_load_ps(ray + 20);

	__m128 a = dot_sse(px, py, pz, e1x, e1y, e1z);

	/* constructs the vector s = (ray origin) - v0. */
	const __m128 sx = _mm_sub_ps(rox, x1);
	const __m128 sy = _mm_sub_ps(roy, y1);
	const __m128 sz = _mm_sub_ps(roz, z1);

	/* calculate (1/a) for all triangles */
	const __m128 rpa = _mm_div_ps(one2, a);

	/* q = s x e1 */
	const __m128 qx = cross_sse(e1z, e1y, sy, sz);
	const __m128 qy = cross_sse(e1x, e1z, sz, sx);
	const __m128 qz = cross_sse(e1y, e1x, sx, sy);

	/* calculate u, v and t for all triangles. */
	const __m128 uu = _mm_mul_ps(dot_sse(sx, sy, sz, px, py, pz), rpa);
	const __m128 vv = _mm_mul_ps(dot_sse(rdx, rdy, rdz, qx, qy, qz), rpa);

	__m128 result;

	/* run all rejection tests. if a triangle passes all test,
	 * it will be marked by all ones in the SSE-register, if it fails
	 * it will be marked by zeros. if triangle 1 and 3 passes 
	 * the resulting register would look like
	 * 	
	 * 0xFFFFFFFF00000000FFFFFFFF00000000
	 *
	 */
	a = _mm_and_ps(
		_mm_and_ps(
#if 0	/* original code. no buck face culling */
			_mm_cmpgt_ps(_mm_mul_ps(a, a), eps2),
#else	/* do back face culling */
			_mm_cmpgt_ps(a, eps),
#endif
			_mm_cmpngt_ps(_mm_add_ps(uu, vv), one)
		),
		_mm_and_ps(
			_mm_cmpnlt_ps(uu, zero),
			_mm_cmpnlt_ps(vv, zero)
		)
	);

	/* the t for all triangles are calculated, only tobe set to zero
	 * if it was rejectted accroding to the rejection tests.
	 */
	result = _mm_mul_ps(dot_sse(e2x, e2y, e2z, qx, qy, qz), rpa);

	/* the final result is moved from SSE-registers into ordinary
	 * memory(16-byte aligned)
	 */
	_mm_store_ps(t, _mm_and_ps(a, result));
	_mm_store_ps(u, uu);
	_mm_store_ps(v, vv);
}
#endif

#ifdef WITH_ALTIVEC
static void
isect_altivec(const float *ray, const float *data, float * RESTRICT t, float * RESTRICT u, float * RESTRICT v)
{

	vector float result;

	/* 1.0 * 2 ^(-17) = 1.9073e-6 */
	const vector float eps = vec_ctf(vec_splat_u32(1), 19);

	/* 1.0 * 1.0 + eps  = 1.0 + eps */
	const vector float one = vec_madd(vec_ctf(vec_splat_u32(1), 0),
					  vec_ctf(vec_splat_u32(1), 0),
					  eps);

	const vector float zero = vec_ctf(vec_splat_u32(0), 0);

#if 0 	/* original code. non buckface culling. */
	/* eps * eps + 0.0 = (eps)^2 */
	const vector float eps2 = vec_madd(eps, eps, zero);
#endif

	/* loads the vector edge2(v3 - v1) from 4 triangles */
	const vector float e2x = vec_ld(24 * 4, data);
	const vector float e2y = vec_ld(28 * 4, data);
	const vector float e2z = vec_ld(32 * 4, data);

	/* loads ray direction vector. */
	const vector float rdx = vec_ld(0 * 4, ray);
	const vector float rdy = vec_ld(4 * 4, ray);
	const vector float rdz = vec_ld(8 * 4, ray);

	/* loads the vector v1 from 4 triangles */
	const vector float x1 = vec_ld(0 * 4, data);
	const vector float y1 = vec_ld(4 * 4, data);
	const vector float z1 = vec_ld(8 * 4, data);

	/* loads the vector e1(v2 - v1) from 4 triangles. */
	const vector float e1x = vec_ld(12 * 4, data);
	const vector float e1y = vec_ld(16 * 4, data);
	const vector float e1z = vec_ld(20 * 4, data);

	/* cross product p = d x e2 */
	const vector float px = cross_altivec(e2z, e2y, rdy, rdz);
	const vector float py = cross_altivec(e2x, e2z, rdz, rdx);
	const vector float pz = cross_altivec(e2y, e2x, rdx, rdy);

	/* loads the ray origin vector. */
	const vector float rox = vec_ld(12 * 4, ray);
	const vector float roy = vec_ld(16 * 4, ray);
	const vector float roz = vec_ld(20 * 4, ray);

	vector float a = dot_altivec(px, py, pz, e1x, e1y, e1z);

	/* constructs the vector s = (ray origin) - v0. */
	const vector float sx = vec_sub(rox, x1);
	const vector float sy = vec_sub(roy, y1);
	const vector float sz = vec_sub(roz, z1);

	/* calculate (1/a) for all triangles */
	const vector float rpa = vec_div(a);

	/* q = s x e1 */
	const vector float qx = cross_altivec(e1z, e1y, sy, sz);
	const vector float qy = cross_altivec(e1x, e1z, sz, sx);
	const vector float qz = cross_altivec(e1y, e1x, sx, sy);

	/* calculate u, v and t for all triangles. */
	const vector float uu = vec_madd(dot_altivec(sx, sy, sz, px, py, pz),
					 rpa,
					 zero);
	const vector float vv = vec_madd(dot_altivec(rdx, rdy, rdz, qx, qy, qz),
					 rpa,
					 zero);


	/* run all rejection tests. if a triangle passes all test,
	 * it will be marked by all ones in the AltiVec-register, if it fails
	 * it will be marked by zeros. if triangle 1 and 3 passes 
	 * the resulting register would look like
	 * 	
	 * 0xFFFFFFFF00000000FFFFFFFF00000000
	 *
	 */
	a = (vector float)(
		vec_and(
#if 0	/* original code */
			vec_and(vec_cmpgt(vec_madd(a, a, zero), eps2),
#else 
			vec_and(vec_cmpgt(a, eps),
#endif
				vec_cmple(vec_add(uu, vv), one)
			),
			vec_and(
				vec_cmpge(uu, zero),
				vec_cmpge(vv, zero)
			)
		)
	);

	/* the t for all triangles are calculated, only tobe set to zero
	 * if it was rejectted accroding to the rejection tests.
	 */
	result = vec_madd(dot_altivec(e2x, e2y, e2z, qx, qy, qz), rpa, zero);

	/* the final result is moved from AltiVec-registers into ordinary
	 * memory(16-byte aligned)
	 */
	vec_st(vec_and(a, result), 0, t);
	vec_st(uu, 0, u);
	vec_st(vv, 0, v);

}
#endif	/* WITH_ALTIVEC */

#ifdef WITH_VU
/* TODO: not supported yet... */
static void
isect_vu(float *ray, float *data, float *t, float *u, float *v)
{
	const ps2_vu0_fvector one = {1.0f + EPSILON,
				     1.0f + EPSILON,
				     1.0f + EPSILON,
				     1.0f + EPSILON};

#if 0

	const __m128 one = _mm_set_ps1(1.0f + EPSILON);

	const __m128 one2 = _mm_set_ps1(1.0f);

	const __m128 zero = _mm_set_ps1(0.0f);

	const __m128 eps2 = _mm_set_ps1(EPSILON2);

	/* loads the vector edge2(v3 - v1) from 4 triangles */
	const __m128 e2x = _mm_load_ps(data + 24);
	const __m128 e2y = _mm_load_ps(data + 28);
	const __m128 e2z = _mm_load_ps(data + 32);

	/* loads ray direction vector. */
	const __m128 rdx = _mm_load_ps(ray);
	const __m128 rdy = _mm_load_ps(ray + 4);
	const __m128 rdz = _mm_load_ps(ray + 8);

	/* loads the vector v1 from 4 triangles */
	const __m128 x1 = _mm_load_ps(data);
	const __m128 y1 = _mm_load_ps(data + 4);
	const __m128 z1 = _mm_load_ps(data + 8);

	/* loads the vector e1(v2 - v1) from 4 triangles. */
	const __m128 e1x = _mm_load_ps(data + 12);
	const __m128 e1y = _mm_load_ps(data + 16);
	const __m128 e1z = _mm_load_ps(data + 20);

	/* cross product p = d x e2 */
	const __m128 px = cross_sse(e2z, e2y, rdy, rdz);
	const __m128 py = cross_sse(e2x, e2z, rdz, rdx);
	const __m128 pz = cross_sse(e2y, e2x, rdx, rdy);

	/* loads the ray origin vector. */
	const __m128 rox = _mm_load_ps(ray + 12);
	const __m128 roy = _mm_load_ps(ray + 16);
	const __m128 roz = _mm_load_ps(ray + 20);

	__m128 a = dot_sse(px, py, pz, e1x, e1y, e1z);

	/* constructs the vector s = (ray origin) - v0. */
	const __m128 sx = _mm_sub_ps(rox, x1);
	const __m128 sy = _mm_sub_ps(roy, y1);
	const __m128 sz = _mm_sub_ps(roz, z1);

	/* calculate (1/a) for all triangles */
	const __m128 rpa = _mm_div_ps(one2, a);

	/* q = s x e1 */
	const __m128 qx = cross_sse(e1z, e1y, sy, sz);
	const __m128 qy = cross_sse(e1x, e1z, sz, sx);
	const __m128 qz = cross_sse(e1y, e1x, sx, sy);

	/* calculate u, v and t for all triangles. */
	const __m128 uu = _mm_mul_ps(dot_sse(sx, sy, sz, px, py, pz), rpa);
	const __m128 vv = _mm_mul_ps(dot_sse(rdx, rdy, rdz, qx, qy, qz), rpa);

	__m128 result;


	/* run all rejection tests. if a triangle passes all test,
	 * it will be marked by all ones in the SSE-register, if it fails
	 * it will be marked by zeros. if triangle 1 and 3 is passes 
	 * the resulting register would look like
	 * 	
	 * 0xFFFFFFFF00000000FFFFFFFF00000000
	 *
	 */
	a = _mm_and_ps(
		_mm_and_ps(
			_mm_cmpgt_ps(_mm_mul_ps(a, a), eps2),
			_mm_cmpngt_ps(_mm_add_ps(uu, vv), one)
		),
		_mm_and_ps(
			_mm_cmpnlt_ps(uu, zero),
			_mm_cmpnlt_ps(vv, zero)
		)
	);

	/* the t for all triangles are calculated, only tobe set to zero
	 * if it was rejectted accroding to the rejection tests.
	 */
	result = _mm_mul_ps(dot_sse(e2x, e2y, e2z, qx, qy, qz), rpa);

	/* the final result is moved from SSE-registers into ordinary
	 * memory(16-byte aligned)
	 */
	_mm_store_ps(t, _mm_and_ps(a, result));
	_mm_store_ps(u, uu);
	_mm_store_ps(v, vv);
#endif

}
#endif

static void
build_surfinfo(ri_surface_info_t *info,
	       const ri_vector_t *isectpoint,
	       ri_geom_t *geom, unsigned int index,
	       float u, float v)
{
	ri_vector_t tmpbasis[3];
	ri_vector_t defcol;
	unsigned int i0, i1, i2;
	ri_vector_t *RESTRICT v0;
	ri_vector_t *RESTRICT v1;
	ri_vector_t *RESTRICT v2;
	ri_vector_t *RESTRICT n0;
	ri_vector_t *RESTRICT n1;
	ri_vector_t *RESTRICT n2;
	ri_vector_t *RESTRICT c0;
	ri_vector_t *RESTRICT c1;
	ri_vector_t *RESTRICT c2;

	ri_vector_copy(&(info->pos), isectpoint);
	
	i0 = geom->indices[index + 0];
	i1 = geom->indices[index + 1];
	i2 = geom->indices[index + 2];

	v0 = &(geom->positions[i0]);
	v1 = &(geom->positions[i1]);
	v2 = &(geom->positions[i2]);

	if (geom->normals) {
		n0 = &(geom->normals[i0]);
		n1 = &(geom->normals[i1]);
		n2 = &(geom->normals[i2]);

		lerp_normal(&(info->normal), n0, n1, n2, u, v);

		if (geom->tangents && geom->binormals) {

			n0 = &(geom->tangents[i0]);
			n1 = &(geom->tangents[i1]);
			n2 = &(geom->tangents[i2]);

			lerp_normal(&(info->tangent),
				    n0, n1, n2, u, v);

			n0 = &(geom->binormals[i0]);
			n1 = &(geom->binormals[i1]);
			n2 = &(geom->binormals[i2]);

			lerp_normal(&(info->binormal),
				    n0, n1, n2, u, v);
		} else {

			ri_ortho_basis(tmpbasis, &info->normal);
			ri_vector_copy(&(info->tangent),
				       &tmpbasis[0]);
			ri_vector_copy(&(info->binormal),
				       &tmpbasis[1]);
		}
	} else {
		calculate_normal(&(info->normal), v0, v1, v2);
		ri_ortho_basis(tmpbasis, &info->normal);
		ri_vector_copy(&(info->tangent),
			       &tmpbasis[0]);
		ri_vector_copy(&(info->binormal),
			       &tmpbasis[1]);
	}

	if (geom->colors) {
		c0 = &(geom->colors[i0]);
		c1 = &(geom->colors[i1]);
		c2 = &(geom->colors[i2]);

		lerp_normal(&(info->color),
			    c0, c1, c2, u, v);
	} else {
		defcol.e[0] = 1.0;
		defcol.e[1] = 1.0;
		defcol.e[2] = 1.0;
		defcol.e[3] = 1.0;
		ri_vector_copy(&(info->color), &defcol);
	}

	if (geom->opacities) {
		info->opacity = geom->opacities[i0].e[0];
	} else {
		info->opacity = 1.0;
	}

	if (geom->texcoords) {
		lerp_uv(&info->u, &info->v,
			&geom->texcoords[2 * i0],
			&geom->texcoords[2 * i1],
			&geom->texcoords[2 * i2],
			u, v);
	} else {
		info->u = 0.0; 
		info->v = 0.0; 
	}

	if (geom->two_side) {
		if (index < geom->nindices / 2) {
			info->inside = 0;
		} else {
			/* surface is inside of geometry */
			info->inside = 1;
		}
	} else {
		info->inside = 0;
	}

	info->geom  = geom;
	info->index = index;
	info->kd    = geom->kd;
	info->ks    = geom->ks;
}

static int
ray_bbox(const float org[3], const float invdir[3],
	 float min[3], float max[3], float t[2])
{
	float tmin, tmax, tymin, tymax, tzmin, tzmax;

	if (invdir[0] >= 0.0) {
		tmin = (min[0] - org[0]) * invdir[0];
		tmax = (max[0] - org[0]) * invdir[0];
	} else {
		tmin = (max[0] - org[0]) * invdir[0];
		tmax = (min[0] - org[0]) * invdir[0];
	}

	if (invdir[1] >= 0.0) {
		tymin = (min[1] - org[1]) * invdir[1];
		tymax = (max[1] - org[1]) * invdir[1];
	} else {
		tymin = (max[1] - org[1]) * invdir[1];
		tymax = (min[1] - org[1]) * invdir[1];
	}

	if ((tmin > tymax) || (tymin > tmax)) return 0;
	if (tymin > tmin) tmin = tymin;
	if (tymax < tmax) tmax = tymax;

	if (invdir[2] >= 0.0) {
		tzmin = (min[2] - org[2]) * invdir[2];
		tzmax = (max[2] - org[2]) * invdir[2];
	} else {
		tzmin = (max[2] - org[2]) * invdir[2];
		tzmax = (min[2] - org[2]) * invdir[2];
	}

	if ((tmin > tzmax) || (tzmin > tmax)) return 0;
	if (tzmin > tmin) tmin = tzmin;
	if (tzmax < tmax) tmax = tzmax;

	if (tmin < tmax) {
		t[0] = tmin; t[1] = tmax;
	} else {
		printf("tmin >= tmax\n");
		t[0] = tmax; t[1] = tmin;
	}	

	return 1;
}

int
inside_cell(const ri_vector_t *p, const otCell *cell,
	    const float bmin[3], const float invwidth[3])
{
	float o[3];
	float min[3], max[3];

	/* map p to [0,1)^3 */
	o[0] = (p->e[0] - bmin[0]) * invwidth[0];
	o[1] = (p->e[1] - bmin[1]) * invwidth[1];
	o[2] = (p->e[2] - bmin[2]) * invwidth[2];

	assert(o[0] >= 0.0);
	assert(o[1] >= 0.0);
	assert(o[2] >= 0.0);
	assert(o[0] <  1.0);
	assert(o[1] <  1.0);
	assert(o[2] <  1.0);

	get_octree_bb(cell, min, max);

	/* TODO: use locational code version. */
	if (min[0] > o[0]) return 0;
	if (min[1] > o[1]) return 0;
	if (min[2] > o[2]) return 0;

	if (max[0] <= o[0]) return 0;
	if (max[1] <= o[1]) return 0;
	if (max[2] <= o[2]) return 0;

	return 1;
}

// find minimum cell bounding box which contains point p.
static void
min_cell(float p[3], int maxlevel, float min[3], float max[3])
{
	unsigned int loccode[3];	
	unsigned int mask; 
	float width = 1.0f / (float)(1 << (maxlevel));

	/* mask = 0x1100000...
	 *            ^
	 *            +- OT_ROOT_LEVEL
	 */
	mask = ~0 << OT_ROOT_LEVEL;

	/* mask = 0x1100000... | (0x000010000... - 1))  
	 *                            ^ ^                           
	 *           OT_ROOT_LEVEL  - + +- (OT_ROOT_LEVEL -maxlevel)
	 *
	 *       =       0x1100011111...
	 *                   ^ ^                           
	 *   OT_ROOT_LEVEL - + +- (OT_ROOT_LEVEL -maxlevel)
	 */
	mask |= (1 << (OT_ROOT_LEVEL - maxlevel)) - 1;

	/* mask =        ~mask  
	 *      =        0x0011100000...
	 *                   ^ ^                           
	 *   OT_ROOT_LEVEL - + +- (OT_ROOT_LEVEL -maxlevel)
	 */
	mask = ~mask;

	loccode[0] = (unsigned int)(p[0] * OT_MAX_VAL);
	loccode[1] = (unsigned int)(p[1] * OT_MAX_VAL);
	loccode[2] = (unsigned int)(p[2] * OT_MAX_VAL);
	
	loccode[0] &= mask;
	loccode[1] &= mask;
	loccode[2] &= mask;


	min[0] = loccode[0] * OT_INV_MAX_VAL;
	min[1] = loccode[1] * OT_INV_MAX_VAL;
	min[2] = loccode[2] * OT_INV_MAX_VAL;

	max[0] = min[0] + width;
	max[1] = min[1] + width;
	max[2] = min[2] + width;
}

static void
build_z_table()
{
	unsigned int   i, j;
	unsigned int   bit;
	unsigned int   v;
	unsigned int   ret;
	unsigned int   mask = 0x1;
	int            shift;

	for (i = 0; i < 256; i++) {
		v = i;
		shift = 0;
		ret = 0;
		for (j = 0; j < 8; j++) {
			/* extract (j+1)'th bit */
			bit = (v >> j) & mask;

			ret += bit << shift;
			shift += 3;
		}

		g_z_table[i] = ret;
	}
}
