/*
 * data structure for accelarating ray-polygon intersection.
 *
 * $Id: accel.h,v 1.3 2004/04/16 13:46:45 syoyo Exp $
 */

#ifndef ACCEL_H
#define ACCEL_H

#ifdef __cplusplus
extern "C" {
#endif

#include "memory.h"
#include "geom.h"
#include "octree.h"
#include "octree_frisken.h"
#include "list.h"

/* grid size for unifrom grid structre */
#define GRIDSIZE 64		/* Should be 2^N and >= 4 */

/* Acceleration methods */
#define ACCEL_GRID           0
#define ACCEL_OCTREE_FRISKEN 1
#define ACCEL_BSP            2
#define ACCEL_KDTREE         3

/* triangle info data structure stored in ri_ugrid_t->cell */
typedef struct _ri_tri_info_t
{
	unsigned int  index;		/* vertex index			*/
	ri_geom_t    *geom;		/* reference for geom		*/
	int           id;		/* ray id for mailboxing test	*/
} ri_tri_info_t;

/* data structure used for SIMD ray-triangle intersection */
typedef struct _ri_simd_tri_info_t
{
	ri_aligned_float_t  tridata;

	ri_geom_t          **geoms;
	unsigned int        *indices;

	int nblocks;

	/* tridataptr points memory address in ri_ugrid_t->tridata */

	float              *tridataptr;	
} ri_simd_tri_info_t;

typedef struct _ri_tri_list_t
{
	ri_tri_info_t      *tris;
	int                 ntris;
	
	ri_simd_tri_info_t *simdtris;
} ri_tri_list_t;

/* uniform grid data structure */
typedef struct _ri_ugrid_t
{
	ri_tri_list_t *cell[GRIDSIZE][GRIDSIZE][GRIDSIZE];
	//ri_list_t *cell[GRIDSIZE][GRIDSIZE][GRIDSIZE];
	float bboxmin[3], bboxmax[3];
	int   voxels[3];
	float width[3];
	float invwidth[3];
	int   curr_rayid;

	ri_tri_list_t **cdat;	/* ptr array */
	/* 1D triangle data array. */ 
	ri_aligned_float_t  tridata;

	//int   *hilbtable;	/* Hilbert mapping table. */

	/* variables for block'ed grid cells */
	int   blkwidth;		/* block width		*/
	int   blksize;		/* block size		*/
	int   shiftsize;	/* bit shift size	*/
	int   bitmask;		/* bit mask		*/
	
} ri_ugrid_t;


/*
 * -- Hilbert orderd memory access illustration. --
 * To increase memory cache, we employ Hilbert curve encoded memory arrangement.
 *
 * ri_ugrid_t->tridata is a large 1D array contans triangle data of
 * all grid cell, which is used for ray-triangle intersection.
 *
 * 3D grid cell is mapped into 1D array using 3D Hilbert space filling curve.
 *
 * ri_simd_tri_info_t->tridataptr points one of tridata positon.
 * ri_simd_tri_info_t->nblocks counts a number of triangle data in cell where
 * ri_simd_tri_info_t->tridataptr points.
 *
 *            +--+--+--+--+
 * nblocks    | 2| 3| 4| 3|
 *            +--+--+--+--+
 *
 *            +--+--+--+--+
 * tridataptr |  |  |  |  |
 *            +|-+|-+|-+|-+
 *             |  |  |  |
 *             |  |  ++ +--------+
 *             |  |   |          |
 *            +|-+|--+|---+ ... +|--+
 * tridata    |* |*  |*   |     |*  |    1D array(Hilbert curve encoded)
 *            +--+---+----+ ... +---+
 *            <-><--><--->      <-->
 *             2   3    4         3
 */

typedef struct _ri_octree_accel_t
{
	float   invwidth[3];
	float   bmin[3];
	float   bmax[3];
	int     maxdepth;
	otCell *cell;
} ri_octree_accel_t;

/* create octree deta structure containing polygon geometry */
//extern ri_octree_t *ri_accel_build_polygon_octree(ri_geom_t *geom);

/* add geometry to be cooked */
//extern void ri_accel_add_geom(ri_geom_t *geom);

/* create uniform grid deta structure from geometry */
extern ri_ugrid_t *ri_accel_build_uniform_grid();

/* create octree deta structure from geomtetry */
extern ri_octree_accel_t *ri_accel_build_octree();

extern void ri_accel_free(ri_ugrid_t *ugrid);
extern void ri_octree_accel_free(ri_octree_accel_t *octree);

#ifdef __cplusplus
}	/* extern "C" */
#endif


#endif
