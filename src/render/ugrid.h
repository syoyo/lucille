/*
 *   lucille | Global Illumination render
 *
 *             written by Syoyo.
 *
 */

#ifndef LUCILLE_UGRID_H
#define LUCILLE_UGRID_H

/* grid size for unifrom grid structre */
#define GRIDSIZE 64		/* Should be 2^N and >= 4 */

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
 * Implementation of ri_accel_t interface
 */
extern void *ri_ugrid_build    ();
extern void  ri_ugrid_free     (void                    *accel);
extern int   ri_ugrid_intersect(void                    *accel,
                                ri_ray_t                *ray,
                                ri_intersection_state_t *state);

#endif	// LUCILLE_UGRID_H
