/*
 * $Id: accel.c,v 1.10 2004/10/31 01:53:40 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <math.h>

#include "ri.h"
#include "vector.h"
#include "list.h"
#include "memory.h"
#include "log.h"
#include "render.h"
#include "accel.h"
#include "parallel.h"
//#include "hilbert.h"

/* use z curve order for addressing voxel memory. */
#define USE_ZORDER 1

#define MAX_OCTREE_DEPTH 6

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


/* table for z curve order */
static unsigned int g_z_table[256];

static void         calc_bbox(ri_list_t *geom_list,
			      double min[3], double max[3]);
static unsigned int calc_sum_ntriangles(ri_list_t *geom_list);
static void         calc_polybbox(const ri_vector_t *v,
				  double min[3], double max[3]);
static void         conv_cell(ri_tri_list_t *dst[GRIDSIZE][GRIDSIZE][GRIDSIZE],
			      ri_array_t    **src,
	  		      int xvoxels, int yvoxels, int zvoxels);
static void         flatten_cell_data(otCell *cell);
#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
static void         conv_simd(ri_ugrid_t *ugrid,
	  		      int xvoxels, int yvoxels, int zvoxels);
static void         copy_simd(ri_tri_list_t *dst);
static void         free_cell_simd(ri_tri_list_t *cell);
static void         copy_simd_flat(ri_ugrid_t *ugrid,
				   int xvoxels, int yvoxels, int zvoxels);
#endif
static void         free_cell(ri_tri_list_t *cell);
static void         add_poly_to_octree(otCell *cell,
		                       const ri_tri_info_t *triinfo,
				       const ri_vector_t *v);
static int          bb_in_bb(double bmin1[3], double bmax1[3],
			     double bmin2[3], double bmax2[3]);
static void         subdiv_octree(otCell *cell);
static void         unit_coord(ri_vector_t *v,
			       float min[3], float invwidth[3]);
static void         dump_octree(otCell *root);
static void         dump_octree_trav(FILE *fp, otCell *cell);
static void         get_octree_depth(otCell *cell, int *depth);
//static void         calc_hilbert_table(int *table, int cellsize,
//			int shiftsize, int blksize, int width, int mask);

static void         build_z_table();

/*
 * Function: ri_accel_build_uniform_grid
 *
 *     Builds uniform grid data structure for accelerating ray-tracing.
 *     Scene data are read from rendering context.
 *
 * Parameters:
 *
 *     None.
 *
 * Returns:
 *
 *     Built uniform grid data strucure.
 */
ri_ugrid_t *
ri_accel_build_uniform_grid()
{
	unsigned int i, j, k;
	int x, y, z;
	int idx;
	int cuberoot;
	unsigned int maxtrisincell = 0;	/* maximum number of polygons
					 * one cell contains. */
	unsigned int ntriangles;
	ri_tri_info_t triinfo;
	
	double  bmin[3], bmax[3];
	double  pmin[3], pmax[3];
	double  delta;		/* maximum absolute length in bmin & bmax */
	double  dx, dy, dz;
	double  xw, yw, zw;
	double invxw, invyw, invzw;
	int    xvoxels, yvoxels, zvoxels;
	double maxwidth, invmaxwidth;
	int    x0, x1, y0, y1, z0, z1;
	ri_vector_t v[3];
	ri_ugrid_t *ugrid;
	ri_list_t  *geomitr;
	ri_geom_t  *geom;
	ri_array_t  **cell;

	/* Partition grids into 4x4x4 sub blocks */
	int    blkwidth = 4;

	ri_log(LOG_INFO, "Building accel grid ... ");

	ri_timer_start(ri_render_get()->context->timer, "Uniform grid building");
	ugrid = (ri_ugrid_t *)ri_mem_alloc(sizeof(ri_ugrid_t));

	ugrid->blkwidth  = blkwidth;
	ugrid->blksize   = GRIDSIZE / blkwidth;
	ugrid->shiftsize = (int)(log(blkwidth) / log(2)); /* log(2, blkwidth) */
	ugrid->bitmask   = blkwidth - 1;

	cell = (ri_array_t **)ri_mem_alloc(sizeof(ri_array_t *) *
					  GRIDSIZE * GRIDSIZE * GRIDSIZE);

	for (i = 0; i < GRIDSIZE; i++) {
		for (j = 0; j < GRIDSIZE; j++) {
			for (k = 0; k < GRIDSIZE; k++) {
				idx = i*GRIDSIZE*GRIDSIZE+j*GRIDSIZE+k;
				ugrid->cell[i][j][k] = NULL;	
				cell[idx] = NULL;
			}
		}
	}

	ugrid->cdat = (ri_tri_list_t **)
				ri_mem_alloc(sizeof(ri_tri_list_t *) *
					     GRIDSIZE * GRIDSIZE * GRIDSIZE);
	for (i = 0; i < GRIDSIZE * GRIDSIZE * GRIDSIZE; i++) {
		ugrid->cdat[i] = NULL;
	}

	//ugrid->hilbtable = (int *)ri_mem_alloc(sizeof(int) * 
	//				     GRIDSIZE * GRIDSIZE * GRIDSIZE);

	//memset(ugrid->hilbtable, 0,
	//       sizeof(int) * GRIDSIZE * GRIDSIZE * GRIDSIZE);

	calc_bbox(ri_render_get()->geomlist, bmin, bmax);

	/* calc maximun length, delta */
	if (fabs(bmin[0]) > fabs(bmin[1]))   delta = fabs(bmin[0]);
	else				     delta = fabs(bmin[1]);
	if (delta < fabs(bmin[2])) 	     delta = fabs(bmin[2]);

	if (delta < fabs(bmax[0]))	     delta = fabs(bmax[0]);
	if (delta < fabs(bmax[1]))	     delta = fabs(bmax[1]);
	if (delta < fabs(bmax[2]))	     delta = fabs(bmax[2]);

	delta *= 1.0e-4;

	bmin[0] -= delta; bmin[1] -= delta; bmin[2] -= delta;
	bmax[0] += delta; bmax[1] += delta; bmax[2] += delta;

	dx = 1.00001 * (bmax[0] - bmin[0]);
	dy = 1.00001 * (bmax[1] - bmin[1]);
	dz = 1.00001 * (bmax[2] - bmin[2]);

	/* calc inverse of maximum cube width */
	maxwidth = dx > dy ? dx : dy;
	maxwidth = maxwidth > dz ? maxwidth : dz;

	if (maxwidth != 0.0) {
		invmaxwidth = 1.0 / maxwidth;
	} else {
		ri_log(LOG_ERROR, "maxwidth = 0.0");
		return NULL;
	}

	ntriangles = calc_sum_ntriangles(ri_render_get()->geomlist);
	cuberoot = (int)pow(ntriangles, 0.333333);

	xvoxels = 3 * (int)ceil(cuberoot * dx * invmaxwidth);
	yvoxels = 3 * (int)ceil(cuberoot * dy * invmaxwidth);
	zvoxels = 3 * (int)ceil(cuberoot * dz * invmaxwidth);

	if (xvoxels < 1       ) xvoxels = 1;
	if (xvoxels > GRIDSIZE) xvoxels = GRIDSIZE;
	if (yvoxels < 1	      )	yvoxels = 1;
	if (yvoxels > GRIDSIZE) yvoxels = GRIDSIZE;
	if (zvoxels < 1       )	zvoxels = 1;
	if (zvoxels > GRIDSIZE) zvoxels = GRIDSIZE;

	xw = dx / xvoxels;
	yw = dy / yvoxels;
	zw = dz / zvoxels;
	
	invxw = (xw == 0.0) ? 0.0 : 1.0 / xw;
	invyw = (yw == 0.0) ? 0.0 : 1.0 / yw;
	invzw = (zw == 0.0) ? 0.0 : 1.0 / zw;

	for (geomitr  = ri_list_first(ri_render_get()->geomlist);
	     geomitr != NULL;
	     geomitr  = ri_list_next(geomitr)) {
		geom = (ri_geom_t *)geomitr->data;

		for (i = 0; i < geom->nindices / 3; i++) {

			triinfo.index = 3 * i;
			triinfo.geom = geom;
			triinfo.id = -1;

			v[0] = geom->positions[geom->indices[3 * i + 0]];	
			v[1] = geom->positions[geom->indices[3 * i + 1]];	
			v[2] = geom->positions[geom->indices[3 * i + 2]];	

			calc_polybbox(v, pmin, pmax);

			x0 = (int)((pmin[0] - bmin[0]) * invxw);
			if (x0 < 0) x0 = 0;
			if (x0 > xvoxels - 1) x0 = xvoxels - 1;

			x1 = (int)((pmax[0] - bmin[0]) * invxw);
			if (x1 < 0) x1 = 0;
			if (x1 > xvoxels - 1) x1 = xvoxels - 1;

			y0 = (int)((pmin[1] - bmin[1]) * invyw);
			if (y0 < 0) y0 = 0;
			if (y0 > yvoxels - 1) y0 = yvoxels - 1;

			y1 = (int)((pmax[1] - bmin[1]) * invyw);
			if (y1 < 0) y1 = 0;
			if (y1 > yvoxels - 1) y1 = yvoxels - 1;

			z0 = (int)((pmin[2] - bmin[2]) * invzw);
			if (z0 < 0) z0 = 0;
			if (z0 > zvoxels - 1) z0 = zvoxels - 1;

			z1 = (int)((pmax[2] - bmin[2]) * invzw);
			if (z1 < 0) z1 = 0;
			if (z1 > zvoxels - 1) z1 = zvoxels - 1;

			for (z = z0; z <= z1; z++) {
				for (y = y0; y <= y1; y++) {
					for (x = x0; x <= x1; x++) {
						idx = z * GRIDSIZE * GRIDSIZE
						    + y * GRIDSIZE + x;

						if (cell[idx] == NULL) {
							cell[idx] =
							ri_array_new(
							sizeof(ri_tri_info_t));
						}
			
						ri_array_insert(
							cell[idx],
							cell[idx]->nelems,
							(void *)&(triinfo));

						if (maxtrisincell < 
						    cell[idx]->nelems) {
							maxtrisincell = 
							    cell[idx]->nelems;	
						}
					}
				}
			}
		}
	}

	conv_cell(ugrid->cell, cell,
		  xvoxels, yvoxels, zvoxels);

	ri_mem_free(cell);

#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
	//calc_hilbert_table(ugrid->hilbtable, GRIDSIZE, 
	//		   ugrid->shiftsize, ugrid->blksize,
	//		   ugrid->blkwidth, ugrid->bitmask);
	conv_simd(ugrid, xvoxels, yvoxels, zvoxels);
#endif
	
	ugrid->bboxmin[0] = (float)bmin[0];
	ugrid->bboxmin[1] = (float)bmin[1];
	ugrid->bboxmin[2] = (float)bmin[2];

	ugrid->bboxmax[0] = (float)bmax[0];
	ugrid->bboxmax[1] = (float)bmax[1];
	ugrid->bboxmax[2] = (float)bmax[2];

	ri_log(LOG_INFO, "Built accel grid.");

	if (ri_parallel_taskid() == 0 &&
	    ri_log_get_debug() ) {
		printf("--- Uniform grid building statistics ---\n");
		printf("bmin = [ %f, %f, %f ]\n", bmin[0], bmin[1], bmin[2]);
		printf("bmax = [ %f, %f, %f ]\n", bmax[0], bmax[1], bmax[2]);
		printf("voxles = [ %d, %d, %d ]\n", xvoxels, yvoxels, zvoxels);
		printf("ntriangles = [ %u ]\n", ntriangles);
		printf("maximum tris in cell = [ %d ]\n", maxtrisincell);
		printf("width = [ %f, %f, %f ]\n", xw, yw, zw);
		printf("----------------------------------------\n");
	}

	ugrid->voxels[0] = xvoxels;
	ugrid->voxels[1] = yvoxels;
	ugrid->voxels[2] = zvoxels;

	ugrid->width[0] = (float)xw;
	ugrid->width[1] = (float)yw;
	ugrid->width[2] = (float)zw;

	ugrid->invwidth[0] = (float)invxw;
	ugrid->invwidth[1] = (float)invyw;
	ugrid->invwidth[2] = (float)invzw;

	ugrid->curr_rayid  = 0;

	ri_timer_end(ri_render_get()->context->timer, "Uniform grid building");

	return ugrid;
}

/*
 * Function: ri_accel_free
 *
 *     Frees ugrid's memory.
 *
 * Parameters:
 *
 *     *ugrid - Uniform grid data strucure freed.
 *
 * Returns:
 *
 *     None.
 */   
void
ri_accel_free(ri_ugrid_t *ugrid)
{
	unsigned int i, j, k;

	if (ugrid == NULL) return;

	for (i = 0; i < GRIDSIZE; i++) {
		for (j = 0; j < GRIDSIZE; j++) {
			for (k = 0; k < GRIDSIZE; k++) {
				if (ugrid->cell[i][j][k] &&
				    ugrid->cell[i][j][k]->ntris > 0) {
					free_cell(ugrid->cell[i][j][k]);
#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
					free_cell_simd(ugrid->cell[i][j][k]);
#endif
					ri_mem_free(ugrid->cell[i][j][k]);	
				}
			}
		}
	}

	ri_aligned_float_free(&(ugrid->tridata));

	ri_mem_free(ugrid);
}

/*
 * Function: ri_accel_build_octree
 *
 *     Builds octree data structure for accelerating ray-tracing.
 *     Scene data are read from rendering context.
 *
 * Parameters:
 *
 *     None.
 *
 * Returns:
 *
 *     Built octree data strucure.
 */
ri_octree_accel_t *
ri_accel_build_octree()
{
	unsigned int i;
	unsigned int ntriangles;
	ri_tri_info_t triinfo;
	
	double  bmin[3], bmax[3];
	double  width[3];
	double  delta;		/* maximum absolute length in bmin & bmax */
	ri_vector_t v[3];
	ri_octree_accel_t *octree;
	ri_list_t  *geomitr;
	ri_geom_t  *geom;

	printf("octree accel building ---\n");

	ri_timer_start(ri_render_get()->context->timer, "octree_building");
	
	octree = (ri_octree_accel_t *)ri_mem_alloc(sizeof(ri_octree_accel_t));
	octree->cell = (otCell *)ri_mem_alloc(sizeof(otCell));

	octree->cell->level    = OT_ROOT_LEVEL;
	octree->cell->xLocCode = 0;
	octree->cell->yLocCode = 0;
	octree->cell->zLocCode = 0;
	octree->cell->parent   = NULL;
	octree->cell->children = NULL;
	octree->cell->data     = NULL;

	calc_bbox(ri_render_get()->geomlist, bmin, bmax);

	/* calc maximun length, delta */
	if (fabs(bmin[0]) > fabs(bmin[1]))   delta = fabs(bmin[0]);
	else				     delta = fabs(bmin[1]);
	if (delta < fabs(bmin[2])) 	     delta = fabs(bmin[2]);

	if (delta < fabs(bmax[0]))	     delta = fabs(bmax[0]);
	if (delta < fabs(bmax[1]))	     delta = fabs(bmax[1]);
	if (delta < fabs(bmax[2]))	     delta = fabs(bmax[2]);

	delta *= 1.0e-4;

	bmin[0] -= delta; bmin[1] -= delta; bmin[2] -= delta;
	bmax[0] += delta; bmax[1] += delta; bmax[2] += delta;

	width[0] = bmax[0] - bmin[0];
	width[1] = bmax[1] - bmin[1];
	width[2] = bmax[2] - bmin[2];

	/* Find maximun width and store it to width[0]. */

	if (width[0] < width[1]) width[0] = width[1];
	if (width[0] < width[2]) width[0] = width[2];

	octree->bmin[0] = (float)bmin[0];
	octree->bmin[1] = (float)bmin[1];
	octree->bmin[2] = (float)bmin[2];

	octree->bmax[0] = (float)bmin[0] + (float)width[0];
	octree->bmax[1] = (float)bmin[1] + (float)width[0];
	octree->bmax[2] = (float)bmin[2] + (float)width[0];

#if 0
	if (bmax[0] - bmin[0] == 0.0) {
		octree->invwidth[0] = 1.0f;
	} else {
		octree->invwidth[0] = 1.0f / (bmax[0] - bmin[0]);
	}
	if (bmax[1] - bmin[1] == 0.0) {
		octree->invwidth[1] = 1.0f;
	} else {
		octree->invwidth[1] = 1.0f / (bmax[1] - bmin[1]);
	}
	if (bmax[2] - bmin[2] == 0.0) {
		octree->invwidth[2] = 1.0f;
	} else {
		octree->invwidth[2] = 1.0f / (bmax[2] - bmin[2]);
	}
#else
	if (width[0] == 0.0) {
		octree->invwidth[0] = 1.0f;
		octree->invwidth[1] = 1.0f;
		octree->invwidth[2] = 1.0f;
	} else {
		octree->invwidth[0] = 1.0f / (float)width[0];
		octree->invwidth[1] = 1.0f / (float)width[1];
		octree->invwidth[2] = 1.0f / (float)width[2];
	}
#endif

	ntriangles = calc_sum_ntriangles(ri_render_get()->geomlist);

	for (geomitr  = ri_list_first(ri_render_get()->geomlist);
	     geomitr != NULL;
	     geomitr  = ri_list_next(geomitr)) {
		geom = (ri_geom_t *)geomitr->data;

		for (i = 0; i < geom->nindices / 3; i++) {

			triinfo.index = 3 * i;
			triinfo.geom = geom;
			triinfo.id = -1;

			v[0] = geom->positions[geom->indices[3 * i + 0]];	
			v[1] = geom->positions[geom->indices[3 * i + 1]];	
			v[2] = geom->positions[geom->indices[3 * i + 2]];	

			unit_coord(&v[0], octree->bmin, octree->invwidth);
			unit_coord(&v[1], octree->bmin, octree->invwidth);
			unit_coord(&v[2], octree->bmin, octree->invwidth);
			
			add_poly_to_octree(octree->cell, &triinfo, v);

		}
	}

	flatten_cell_data(octree->cell);

	octree->maxdepth = 0;
	get_octree_depth(octree->cell, &(octree->maxdepth));

	printf("max depth = %d\n", octree->maxdepth);
#if 0

#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
	printf("converting cell to simd\n");
	conv_simd(ugrid, xvoxels, yvoxels, zvoxels);
#endif
	

#endif
	printf("uniform grid -----\n");
	printf("bmin = [ %f, %f, %f ]\n", bmin[0], bmin[1], bmin[2]);
	printf("bmax = [ %f, %f, %f ]\n", bmax[0], bmax[1], bmax[2]);
	printf("------------------\n");

	ri_timer_end(ri_render_get()->context->timer, "octree_building");


	dump_octree(octree->cell);

	

	return octree;
}


/* --- private functions --- */
static void
calc_bbox(ri_list_t *geom_list, double min[3], double max[3])
{
	unsigned int i;
	ri_vector_t  v;
	ri_list_t   *itr;
	ri_geom_t   *geom;


	min[0] = min[1] = min[2] =  RI_INFINITY;
	max[0] = max[1] = max[2] = -RI_INFINITY;

	for (itr  = ri_list_first(geom_list);
	     itr != NULL;
	     itr  = ri_list_next(itr)) {

		geom = (ri_geom_t *)itr->data;

		for (i = 0; i < geom->npositions; i++) {
			v = geom->positions[i];

			if (min[0] > v.e[0]) min[0] = v.e[0];	
			if (min[1] > v.e[1]) min[1] = v.e[1];	
			if (min[2] > v.e[2]) min[2] = v.e[2];	

			if (max[0] < v.e[0]) max[0] = v.e[0];	
			if (max[1] < v.e[1]) max[1] = v.e[1];	
			if (max[2] < v.e[2]) max[2] = v.e[2];	
		}
	}
}

static void
calc_polybbox(const ri_vector_t *v, double min[3], double max[3])
{
	min[0] = v[0].e[0] < v[1].e[0] ? v[0].e[0] : v[1].e[0];
	min[0] = min[0] < v[2].e[0] ? min[0] : v[2].e[0];
	min[1] = v[0].e[1] < v[1].e[1] ? v[0].e[1] : v[1].e[1];
	min[1] = min[1] < v[2].e[1] ? min[1] : v[2].e[1];
	min[2] = v[0].e[2] < v[1].e[2] ? v[0].e[2] : v[1].e[2];
	min[2] = min[2] < v[2].e[2] ? min[2] : v[2].e[2];

	max[0] = v[0].e[0] > v[1].e[0] ? v[0].e[0] : v[1].e[0];
	max[0] = max[0] > v[2].e[0] ? max[0] : v[2].e[0];
	max[1] = v[0].e[1] > v[1].e[1] ? v[0].e[1] : v[1].e[1];
	max[1] = max[1] > v[2].e[1] ? max[1] : v[2].e[1];
	max[2] = v[0].e[2] > v[1].e[2] ? v[0].e[2] : v[1].e[2];
	max[2] = max[2] > v[2].e[2] ? max[2] : v[2].e[2];
}

static unsigned int
calc_sum_ntriangles(ri_list_t *geom_list)
{
	unsigned int  ntris;
	ri_list_t    *itr;
	ri_geom_t    *geom;

	ntris = 0;

	for (itr  = ri_list_first(geom_list);
	     itr != NULL;
	     itr  = ri_list_next(itr)) {
		geom = (ri_geom_t *)itr->data;

		ntris += geom->nindices / 3;
	}

	return ntris;
}

static void
conv_cell(ri_tri_list_t *dst[GRIDSIZE][GRIDSIZE][GRIDSIZE], 
	  ri_array_t    **src,
	  int xvoxels, int yvoxels, int zvoxels)
{
	int x, y, z;
	int i;
	int n;
	int pos;
	ri_tri_info_t *tmpinfo;
	//int idx;

	for (z = 0; z < zvoxels; z++) {
		for (y = 0; y < yvoxels; y++) {
			for (x = 0; x < xvoxels; x++) {
				pos = z * GRIDSIZE * GRIDSIZE + 
				      y * GRIDSIZE + x;
				if (!src[pos]) continue;

				n = src[pos]->nelems;
				if (n == 0) continue;

				dst[z][y][x] = (ri_tri_list_t *)ri_mem_alloc(
						sizeof(ri_tri_list_t));

				dst[z][y][x]->tris = (ri_tri_info_t *)
					   ri_mem_alloc(
					   sizeof(ri_tri_info_t) * n);
				dst[z][y][x]->ntris = n;

				for (i = 0; i < n; i++) { 
					tmpinfo = (ri_tri_info_t *)
							ri_array_at(src[pos],
								    i);

					dst[z][y][x]->tris[i].index =
						tmpinfo->index;

					dst[z][y][x]->tris[i].geom =
						tmpinfo->geom;

					dst[z][y][x]->tris[i].id =
						tmpinfo->id;
				}

				ri_array_free(src[pos]);
			}
		}
	}

}

static void
flatten_cell_data(otCell *cell)
{
	int            i;
	int            n;
	ri_array_t    *array;
	ri_tri_info_t *tmpinfo;
	ri_tri_list_t *list;

	if (!cell) return;

	if (cell->data) {

		array = (ri_array_t *)cell->data;
		n = array->nelems;

		if (n == 0) return;

		list = (ri_tri_list_t *)ri_mem_alloc(sizeof(ri_tri_list_t));

		list->tris  = (ri_tri_info_t *)ri_mem_alloc(
						sizeof(ri_tri_info_t) * n);
		list->ntris = n;
		list->simdtris = NULL;

		for (i = 0; i < n; i++) { 
			tmpinfo = (ri_tri_info_t *)ri_array_at(array, i);

			list->tris[i].index = tmpinfo->index;
			list->tris[i].geom  = tmpinfo->geom;
			list->tris[i].id    = tmpinfo->id;
		}

		ri_array_free(cell->data);

		cell->data = (void *)list;
	}

	if (cell->children) {
		for (i = 0; i < 8; i++) {
			flatten_cell_data(&(cell->children[i]));
		}
	}
}

#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
/* construct SIMD friendy data structure */
static void
conv_simd(ri_ugrid_t *ugrid,
	  int xvoxels, int yvoxels, int zvoxels)
{
	int	      x, y, z;
	unsigned int  index;
	unsigned long nblocks = 0;
	ri_tri_list_t *list;

	/* Frist, count number of triangles in the scene.  */
	for (z = 0; z < zvoxels; z++) {
		for (y = 0; y < yvoxels; y++) {
			for (x = 0; x < xvoxels; x++) {
				list = ugrid->cell[z][y][x];

				if (!list) continue;

				/* SIMD array must be a multiple of 4 */
				nblocks += (unsigned long)
						ceil((double)list->ntris /
						     (double)4);
			}
		}
	}

	/* 4 = 4 triangles,
	 * 9 = (xyz 3 components) * (3 vertices) = compose 1 triangle
	 */
	/* allocate 16-byte aligned memory */
	ri_aligned_float_alloc(&(ugrid->tridata), 4 * 9 * nblocks);

	for (z = 0; z < zvoxels; z++) {
		for (y = 0; y < yvoxels; y++) {
			for (x = 0; x < xvoxels; x++) {
				if (!ugrid->cell[z][y][x]) {
					continue;
				}

				copy_simd(ugrid->cell[z][y][x]);

				free_cell(ugrid->cell[z][y][x]);
			}
		}
	}

	//printf("orgazize block cdat\n");
	//printf("voxels = %d, %d, %d\n", zvoxels, yvoxels, xvoxels);

	build_z_table();

	/* Organize block'ed voxel array.  */
	for (z = 0; z < zvoxels; z++) {
		for (y = 0; y < yvoxels; y++) {
			for (x = 0; x < xvoxels; x++) {
				list = ugrid->cell[z][y][x];

#if USE_ZORDER		/* z curve order access */

				index = MAP_Z3D(x, y, z);

#else			/* blocked access */

				index = MAP_XYZ(x, y, z,
						ugrid->shiftsize,
						ugrid->blksize,
						ugrid->blkwidth,
						ugrid->bitmask);
#endif

				if (!list) {
					if (ugrid->cdat[index]) {
						printf("???\n");
					}
					ugrid->cdat[index] = NULL;
					continue;
				}

				ugrid->cdat[index] = list;
			}
		}
	}
		
	copy_simd_flat(ugrid, xvoxels, yvoxels, zvoxels);
}

static void
copy_simd(ri_tri_list_t *dst)
{
	int          i, j;
	int          index;
	int          offset;
	int          nextra;
	ri_vector_t v0, v1, v2;
	ri_vector_t e1, e2;
	unsigned int i0, i1, i2;
	int nblocks;
	ri_geom_t *geom;
	ri_simd_tri_info_t *simdinfo;

	if (!dst->tris) return;
	if (dst->ntris < 1) return;

	dst->simdtris = (ri_simd_tri_info_t *)ri_mem_alloc(
				sizeof(ri_simd_tri_info_t));

	/* simd array must be a multiple of 4 */
	nblocks = (int)ceil((double)dst->ntris / (double)4);
	if (nblocks < 1) nblocks = 1;

	simdinfo = dst->simdtris;

	simdinfo->nblocks = nblocks;

	/* 4 = 4 triangles,
	 * 9 = (xyz 3 components) * (3 vertices) = 1 triangle
	 */
	/* allocate 16-byte aligned memory */
	ri_aligned_float_alloc(&(simdinfo->tridata), 4 * 9 * nblocks);

	simdinfo->geoms = (ri_geom_t **)ri_mem_alloc(
				sizeof(ri_geom_t *) * nblocks * 4);
	simdinfo->indices = (unsigned int *)ri_mem_alloc(
				sizeof(unsigned int) * nblocks * 4);

	for (j = 0; j < nblocks - 1; j++) {
		for (i = 0; i < 4; i++) {
			offset = 4 * j + i;
			geom = dst->tris[offset].geom;
			index = dst->tris[offset].index;
			i0 = geom->indices[index + 0];
			i1 = geom->indices[index + 1];
			i2 = geom->indices[index + 2];

			v0 = geom->positions[i0];
			v1 = geom->positions[i1];
			v2 = geom->positions[i2];

			ri_vector_sub(&e1, &v1, &v0);
			ri_vector_sub(&e2, &v2, &v0);

			simdinfo->tridata.aligned[36 * j +  0 + i] = v0.e[0];
			simdinfo->tridata.aligned[36 * j +  4 + i] = v0.e[1];
			simdinfo->tridata.aligned[36 * j +  8 + i] = v0.e[2];
			simdinfo->tridata.aligned[36 * j + 12 + i] = e1.e[0];
			simdinfo->tridata.aligned[36 * j + 16 + i] = e1.e[1];
			simdinfo->tridata.aligned[36 * j + 20 + i] = e1.e[2];
			simdinfo->tridata.aligned[36 * j + 24 + i] = e2.e[0];
			simdinfo->tridata.aligned[36 * j + 28 + i] = e2.e[1];
			simdinfo->tridata.aligned[36 * j + 32 + i] = e2.e[2];
			simdinfo->indices[offset] = index;
			simdinfo->geoms[offset]   = geom;
		}
	}

	/* the reminder */
	nextra = dst->ntris - (nblocks - 1) * 4;
	for (i = 0; i < nextra; i++) {
		offset = (nblocks - 1);
		geom = dst->tris[4 * offset + i].geom;
		index = dst->tris[4 * offset + i].index;
		i0 = geom->indices[index + 0];
		i1 = geom->indices[index + 1];
		i2 = geom->indices[index + 2];

		v0 = geom->positions[i0];
		v1 = geom->positions[i1];
		v2 = geom->positions[i2];

		ri_vector_sub(&e1, &v1, &v0);
		ri_vector_sub(&e2, &v2, &v0);

		simdinfo->tridata.aligned[36 * offset +  0 + i] = v0.e[0];
		simdinfo->tridata.aligned[36 * offset +  4 + i] = v0.e[1];
		simdinfo->tridata.aligned[36 * offset +  8 + i] = v0.e[2];
		simdinfo->tridata.aligned[36 * offset + 12 + i] = e1.e[0];
		simdinfo->tridata.aligned[36 * offset + 16 + i] = e1.e[1];
		simdinfo->tridata.aligned[36 * offset + 20 + i] = e1.e[2];
		simdinfo->tridata.aligned[36 * offset + 24 + i] = e2.e[0];
		simdinfo->tridata.aligned[36 * offset + 28 + i] = e2.e[1];
		simdinfo->tridata.aligned[36 * offset + 32 + i] = e2.e[2];
		simdinfo->indices[4 * offset + i] = index;
		simdinfo->geoms[4 * offset + i]   = geom;

	}

	/* fill the rest with last triangle data */
	if (nextra != 0) {
		for (i = nextra; i < 4; i++) {
			offset = (nblocks - 1);
			geom = dst->tris[4 * offset + nextra - 1].geom;
			index = dst->tris[4 * offset + nextra - 1].index;
			i0 = geom->indices[index + 0];
			i1 = geom->indices[index + 1];
			i2 = geom->indices[index + 2];

			v0 = geom->positions[i0];
			v1 = geom->positions[i1];
			v2 = geom->positions[i2];

			ri_vector_sub(&e1, &v1, &v0);
			ri_vector_sub(&e2, &v2, &v0);

			simdinfo->tridata.aligned[36*offset+ 0+i] = v0.e[0];
			simdinfo->tridata.aligned[36*offset+ 4+i] = v0.e[1];
			simdinfo->tridata.aligned[36*offset+ 8+i] = v0.e[2];
			simdinfo->tridata.aligned[36*offset+12+i] = e1.e[0];
			simdinfo->tridata.aligned[36*offset+16+i] = e1.e[1];
			simdinfo->tridata.aligned[36*offset+20+i] = e1.e[2];
			simdinfo->tridata.aligned[36*offset+24+i] = e2.e[0];
			simdinfo->tridata.aligned[36*offset+28+i] = e2.e[1];
			simdinfo->tridata.aligned[36*offset+32+i] = e2.e[2];
			simdinfo->indices[4 * offset + i] = index;
			simdinfo->geoms[4 * offset + i]   = geom;
		}
	}
}

static void
free_cell_simd(ri_tri_list_t *cell)
{
	if (!cell->simdtris) return;

	//ri_aligned_float_free(&(cell->simdtris->tridata));
	ri_mem_free(cell->simdtris->geoms);
	ri_mem_free(cell->simdtris->indices);
}

static void
copy_simd_flat(ri_ugrid_t *ugrid, int xvoxels, int yvoxels, int zvoxels)
{
	unsigned int        index;
	int                 x, y, z;
	ri_simd_tri_info_t *simdinfo;
	unsigned long       array_idx = 0;

	for (z = 0; z < zvoxels; z++) {
		for (y = 0; y < yvoxels; y++) {
			for (x = 0; x < xvoxels; x++) {
#if USE_ZORDER 	/* z curve order access */
				index = MAP_Z3D(x, y, z);
#else
				index = MAP_XYZ(x, y, z,
						ugrid->shiftsize,
						ugrid->blksize,
						ugrid->blkwidth,
						ugrid->bitmask);

#endif

				if (!ugrid->cdat[index]) continue;

				simdinfo = ugrid->cdat[index]->simdtris;

				/* one 4-triangle data structure occupies
				 * 4 * 9 floats */

				ri_mem_copy(
					&(ugrid->tridata.aligned[array_idx]),
					simdinfo->tridata.aligned,
					sizeof(float) * simdinfo->nblocks *
					4 * 9); 	

				/* used in raytrace.c */
				simdinfo->tridataptr =
					&(ugrid->tridata.aligned[array_idx]);

				array_idx += simdinfo->nblocks * 4 * 9;

				/* frees unflatten simd data array */
				ri_aligned_float_free(&simdinfo->tridata);
			}
		}
	}
}

#endif

static void
free_cell(ri_tri_list_t *cell)
{
	ri_mem_free(cell->tris);
	cell->tris = NULL;
}


static void
add_poly_to_octree(otCell *cell,
		   const ri_tri_info_t *triinfo, const ri_vector_t *v)
{
	int    i;
	double width;				/* cell's width		  */
	double bmin[3], bmax[3];		/* cell's bounding box    */
	double pmin[3], pmax[3];		/* polygon's bounding box */

	if (!cell) return;

	calc_polybbox(v, pmin, pmax);

	/* calculate cell's bounding box. */
	width = 1.0 / (float)(1 << (OT_ROOT_LEVEL - cell->level));

	bmin[0] = cell->xLocCode * OT_INV_MAX_VAL;
	bmin[1] = cell->yLocCode * OT_INV_MAX_VAL;
	bmin[2] = cell->zLocCode * OT_INV_MAX_VAL;

	bmax[0] = bmin[0] + width;
	bmax[1] = bmin[1] + width;
	bmax[2] = bmin[2] + width;

	if (!bb_in_bb(pmin, pmax, bmin, bmax)) return;

	if (cell->level <= OT_ROOT_LEVEL - MAX_OCTREE_DEPTH ||
	    ((pmax[0] - pmin[0]) >= (width * 0.5) &&
	     (pmax[1] - pmin[1]) >= (width * 0.5) &&
	     (pmax[2] - pmin[2]) >= (width * 0.5))) {

		/* Polygon is too large to fit some of cell's children,
		 * or maximun leaf cell depth reaches.
		 * Add polygon to current cell.
		 */
#if 0
		printf("poly (%f, %f, %f)-(%f, %f, %f) added to ",
			pmin[0], pmin[1], pmin[2],	
			pmax[0], pmax[1], pmax[2]);
		printf(" cell (%f, %f, %f)-(%f, %f, %f)[%d].\n",
			bmin[0], bmin[1], bmin[2],	
			bmax[0], bmax[1], bmax[2], cell->level);
#endif

		if (!cell->data) {
			cell->data = (void *)ri_array_new(
						sizeof(ri_tri_info_t));
		}

		ri_array_insert((ri_array_t *)cell->data,
				((ri_array_t *)cell->data)->nelems,
				triinfo);
	} else {

		if (!cell->children) {

			/* Subdivide current cell. */

			subdiv_octree(cell);
		}

		/* Descend traversing. */

		for (i = 0; i < 8; i++) {
			add_poly_to_octree(&(cell->children[i]),
					   triinfo, v);
		}
	}
}

/* Returns 1 if (bmin1, bmax1) is partially covered by (bmin2, bmax2),
 * 0 if exclusibly not covered. */
static int
bb_in_bb(double bmin1[3], double bmax1[3], double bmin2[3], double bmax2[3])
{
	if ((bmin1[0] > bmax2[0]) ||
	    (bmin1[1] > bmax2[1]) ||
	    (bmin1[2] > bmax2[2]) ||
	    (bmax1[0] < bmin2[0]) ||
	    (bmax1[1] < bmin2[1]) ||
	    (bmax1[2] < bmin2[2])) return 0;

	return 1;
}

static void
subdiv_octree(otCell *cell)
{
	int i;
	int nlevel = cell->level - 1;	/* next level */

	cell->children = (otCell *)malloc(sizeof(otCell) * 8);

	for (i = 0; i < 8; i++) {
		cell->children[i].children = NULL;
		cell->children[i].data     = NULL;
		cell->children[i].parent   = cell;
		cell->children[i].level    = nlevel;
		cell->children[i].data     = NULL;
	}


	cell->children[0].xLocCode = cell->xLocCode;
	cell->children[0].yLocCode = cell->yLocCode;
	cell->children[0].zLocCode = cell->zLocCode;

	cell->children[1].xLocCode = cell->xLocCode | (1 << nlevel);
	cell->children[1].yLocCode = cell->yLocCode;
	cell->children[1].zLocCode = cell->zLocCode;

	cell->children[2].xLocCode = cell->xLocCode;
	cell->children[2].yLocCode = cell->yLocCode | (1 << nlevel);
	cell->children[2].zLocCode = cell->zLocCode;

	cell->children[3].xLocCode = cell->xLocCode | (1 << nlevel);
	cell->children[3].yLocCode = cell->yLocCode | (1 << nlevel);
	cell->children[3].zLocCode = cell->zLocCode;

	cell->children[4].xLocCode = cell->xLocCode;
	cell->children[4].yLocCode = cell->yLocCode;
	cell->children[4].zLocCode = cell->zLocCode | (1 << nlevel);

	cell->children[5].xLocCode = cell->xLocCode | (1 << nlevel);
	cell->children[5].yLocCode = cell->yLocCode;
	cell->children[5].zLocCode = cell->zLocCode | (1 << nlevel);

	cell->children[6].xLocCode = cell->xLocCode;
	cell->children[6].yLocCode = cell->yLocCode | (1 << nlevel);
	cell->children[6].zLocCode = cell->zLocCode | (1 << nlevel);

	cell->children[7].xLocCode = cell->xLocCode | (1 << nlevel);
	cell->children[7].yLocCode = cell->yLocCode | (1 << nlevel);
	cell->children[7].zLocCode = cell->zLocCode | (1 << nlevel);
}

/* convert vertex coordinate into [0, 1) */
static void
unit_coord(ri_vector_t *v, float min[3], float invwidth[3])
{
	v->e[0] = (v->e[0] - min[0]) * invwidth[0];
	v->e[1] = (v->e[1] - min[1]) * invwidth[1];
	v->e[2] = (v->e[2] - min[2]) * invwidth[2];

	assert(v->e[0] >= 0.0);
	assert(v->e[1] >= 0.0);
	assert(v->e[2] >= 0.0);
	assert(v->e[0] < 1.0);
	assert(v->e[1] < 1.0);
	assert(v->e[2] < 1.0);
}

static void
dump_octree(otCell *root)
{
	FILE *fp;

	fp = fopen("octree.dat", "w");
	if (!fp) return;

	dump_octree_trav(fp, root);

	fclose(fp);
}

static void
dump_octree_trav(FILE *fp, otCell *cell)
{
	int   i;
	float org[3];
	float width;

	//if (!cell->data && !cell->children) return;

	width = 0.5f / (float)(1 << (OT_ROOT_LEVEL - cell->level));

	org[0] = cell->xLocCode * (float)OT_INV_MAX_VAL + width;
	org[1] = cell->yLocCode * (float)OT_INV_MAX_VAL + width;
	org[2] = cell->zLocCode * (float)OT_INV_MAX_VAL + width;

	fprintf(fp, "%f %f %f %f %d\n",
			org[0], org[1], org[2], width, cell->level);

	if (cell->children) {
		for (i = 0; i < 8; i++) {
			dump_octree_trav(fp, &(cell->children[i]));
		}
	}
}

static void
get_octree_depth(otCell *cell, int *depth)
{
	int i;

	if (cell->data) {
		if ((unsigned int)(*depth) < OT_ROOT_LEVEL - cell->level) {
			(*depth) = (unsigned int)(OT_ROOT_LEVEL - cell->level);
		}
	}

	if (cell->children) {
		for (i = 0; i < 8; i++) {
			get_octree_depth(&(cell->children[i]), depth);
		}
	}
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

#if 0
static void
calc_hilbert_table(int *table, int cellsize,
		   int shiftsize, int blksize, int width, int mask)
{
	int x, y, z;
	int   index;
	Point p;
	Hcode h;

	for (z = 0; z < cellsize; z++) {
		for (y = 0; y < cellsize; y++) {
			for (x = 0; x < cellsize; x++) {
				p.hcode[0] = x;
				p.hcode[1] = y;
				p.hcode[2] = z;

				/* Get Hilbert index for position (x, y, z) */
				h = H_encode(p);


				/* Blocked memory access */
				index = MAP_XYZ(x, y, z,
						shiftsize, blksize,
						width, mask);

				assert(index < GRIDSIZE * GRIDSIZE * GRIDSIZE);

				table[index] = h.hcode[0];

			}
		}
	}
}
#endif

