/*
 * global rendering manager object.
 *
 * $Id: render.h,v 1.4 2004/06/13 06:44:51 syoyo Exp $
 */

#ifndef RENDERER_H
#define RENDERER_H

#include "context.h"
#include "list.h"
#include "light.h"
#include "hash.h"
#include "geom.h"
#include "display.h"
#include "accel.h"
#include "bsp.h"
#include "kdtree.h"
#include "photonmap.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef MAX_RIBPATH
#define MAX_RIBPATH 1024
#endif

typedef struct _ri_statistic_t
{
	/* raytracing statistics */
	unsigned long long ngridtravs;
	unsigned long long ntesttris;
	unsigned long long nrays;
	unsigned long long nmailboxhits;
} ri_statistic_t;

typedef struct _ri_render_t
{
	ri_hash_t      *geom_drv;		/* geometry driver list */
	ri_hash_t      *display_drv;		/* display driver list	*/
	ri_list_t      *geomlist;
	ri_list_t      *lightlist;
	ri_context_t   *context;		/* rendering context	*/
	ri_ugrid_t     *accel_grid;		/* for accelerate
						   intersection test	*/ 

	ri_octree_accel_t *octree;
#if 1
	ri_bsp_t       *accel_bsp;		/* for accelerate
	
					   intersection test	*/
#endif
	ri_kdtree_t     *accel_kdtree;		/* kdtree acceleration  */

	ri_photonmap_t *global_photonmap;
	ri_photonmap_t *caustics_photonmap;
	ri_octree_t    *irradiance_cache;

	void           (*progress_handler)(void);
	/* \todo multiple context support */

	double          bmin[3];		/* bounding box of scene */
	double          bmax[3];		/* bounding box of scene */

	double          bmaxwidth;		/* maximum width of bounding
						 * box.                  */

	int             dd_use_callback;	/* use callback display
						   driver		 */
	char            ribpath[MAX_RIBPATH];	/* RIB file path	 */

	int             samplexpos;		/* for debug */
	int		sampleypos;		/* for debug */

	/* QMC related stuff */
	int             **perm_table;		/* permutation sequences */

	ri_statistic_t  stat;

	int             debug_mode;		/* debug mode or not */

} ri_render_t;

extern void         ri_render_init();	/* should be called in RiBegin() */
extern ri_render_t *ri_render_get();	/* get global renderer */
extern void         ri_render_free();	/* should be called in RiEnd() */

extern void	    ri_render_frame();

/* register geometry driver */
extern void         ri_render_register_geom_drv(ri_render_t *render,
						const char    *type,
						ri_geom_drv_t *drv);

/* register display driver */
extern void         ri_render_register_display_drv(ri_render_t *render,
						   const char       *type,
						   ri_display_drv_t *drv);

extern void	    ri_render_parse_geom(ri_render_t *render,
					 const char  *type,
					 RtInt        nverts,
					 RtInt	      n,
					 RtToken      tokens[],
					 RtPointer    params[]);

extern void	    ri_render_add_geom(ri_render_t *render, ri_geom_t *geom);
extern RtLightHandle ri_render_add_light(ri_render_t *render,
					 ri_light_t *light);
extern void         ri_render_set_progress_handler(ri_render_t *render,
						   void (*func)(void));

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

