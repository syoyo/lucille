/*
 * $Id: bssrdf.h,v 1.3 2004/02/08 16:38:48 syoyo Exp $
 *
 * BSSRDF code
 */

#ifndef BSSRDF_H
#define BSSRDF_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vector.h"
#include "geom.h"
#include "light.h"
#include "photonmap.h"
#include "display.h"
#include "octree.h"

typedef struct _ri_bssrdf_t
{
	unsigned int  nsamples;
	ri_vector_t  *positions;
	ri_vector_t  *normals;
	ri_vector_t  *irradiances;
	ri_octree_t  *fmmtree;		/* octree structure for FMM	*/
	int           max_treelevel;	/* max level of FMM tree	*/
	double        area;

	/* SIGGRAPH2002 version	*/
	ri_octree_t  *htree;		/* octree structre for tree method */ 
} ri_bssrdf_t;

extern ri_bssrdf_t *ri_bssrdf_new();
extern void         ri_bssrdf_free(ri_bssrdf_t *bssrdf);
extern void         ri_bssrdf_generate_samples(ri_bssrdf_t *bssrdf,
					       ri_geom_t *geom,
					       ri_light_t *light,
					       ri_photonmap_t *photonmap);

extern void         ri_bssrdf_eval_bruteforce(ri_vector_t *rad,
					      ri_bssrdf_t *bssrdf,
					      const ri_vector_t *eye,
					      const ri_vector_t *xo,
					      const ri_vector_t *no);
extern void         ri_bssrdf_eval_sig2002   (ri_vector_t *rad,
					      ri_bssrdf_t *bssrdf,
					      const ri_vector_t *eye,
					      const ri_vector_t *xo,
					      const ri_vector_t *no);
extern void         ri_bssrdf_eval_fmm       (ri_vector_t *rad,
					      ri_bssrdf_t *bssrdf,
					      const ri_vector_t *eye,
					      const ri_vector_t *xo,
					      const ri_vector_t *no);
extern void         ri_bssrdf_test();
extern void         ri_bssrdf_show_points(ri_bssrdf_t *bssrdf, ri_display_drv_t *drv);


#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif


