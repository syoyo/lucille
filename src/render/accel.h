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
#include "list.h"

/* grid size for unifrom grid structre */
#define GRIDSIZE 64		/* Should be 2^N and >= 4 */

/* Acceleration methods */
#define ACCEL_GRID           0

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



/* create uniform grid deta structure from geometry */
extern ri_ugrid_t *ri_accel_build_uniform_grid();

extern void ri_accel_free(ri_ugrid_t *ugrid);

#ifdef __cplusplus
}	/* extern "C" */
#endif


#endif
