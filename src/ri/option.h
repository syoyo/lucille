/*
 * The graphics state has various options that must be set bet before rendering
 * a frame. 
 *
 * $Id$
 */

#ifndef OPTION_H
#define OPTION_H

#include <stdio.h>

#include "array.h"
#include "list.h"
#include "display.h"
#include "camera.h"
#include "vector.h"

typedef struct _ri_option_t
{
	ri_camera_t  *camera;

    /* Current display driver */
	//ri_display_t *display;  

    /* RIB can specify multiple displays. */
    ri_list_t    *display_list;

	/* The type of hidden surface algorithm that is performed. */
	RtToken hider;

	/* Number of color components in colors. The default is 3 for RGB. */
	/* Not implemented yet */
	/* RtInt color_samples; */

	/*
	 * A multiplicative factor that can be used to increase or decrease
	 * the effective level of detail used to render an object.
	 */
	RtFloat relative_detail;

	RtToken orientation;
	
	/* Array of search path for texture, shader, object, etc... */
	ri_ptr_array_t *searchpath;

	/* for ray-tracing */
	unsigned int nfinalgather_rays;	/* number of final gather rays */
	unsigned int narealight_rays;	/* number of sampling rays for
					                 * arealight(direct lighting)
					                 */
	unsigned int max_ray_depth;	    /* maximun nuber of tracing depth */
	int          enable_photonmapping;
	int          enable_irradcache; /* do irradiance caching ? 	*/
	double       irradcache_find_tolerance;
	double       irradcache_insert_tolerance;
	double       irradcache_max_radius;
	char         *irradcache_file;
	unsigned int bssrdf_nsamples;
	double       bssrdf_scatter;
	double       bssrdf_absorb;
	double       bssrdf_scale;
	int          bssrdf_tree_level;
	char         *bssrdf_cache_file;
	int          enable_direct_lighting;
	int          enable_indirect_lighting;
	int          enable_caustics_lighting;

	int          accel_method;

	/* precompted radiance transfer options */
	int          compute_prt;
	int          prt_nsamples;
	int          prt_samplinglevel;
	int          prt_is_glossy;
	int          prt_do_interreflection;
	int          prt_do_distscale;
	int          prt_no_shadow;
	float        prt_scale;

	ri_vector_t  bgcolor;			   /* background color */
	ri_vector_t  ambcolor;			   /* ambient color */

	int          nthreads;			   /* for multi-threading */

	int          use_qmc;			   /* Switch to QMC sampling */

	/* rendering algorithm used for the renderer */
	int          render_method;
	//int          use_mlt;			   /* Metropolis Light Transport */

	int          pt_nsamples;		   /* samples per pixel
						                * (for Path Tracing)       */

	int          mlt_nsamples;

    /* Used for ambient occlusion, IBL, etc */
	int          gather_nsamples;

	int          bsp_tree_depth;
	int          kd_tree_depth;

	RtFilterFunc pixel_filter;		    /* pixel filter		        */
	float        pixel_filter_widthx;	/* width of filter in x dir */
	float        pixel_filter_widthy; 	/* width of filter in y dir */

	int          do_adaptive_supersampling;

} ri_option_t;

#ifdef __cplusplus
extern "C" {
#endif

extern ri_option_t *ri_option_new           ();
extern void         ri_option_free          (ri_option_t       *option);
extern void         ri_option_add_searchpath(ri_option_t       *option,
                                             const char        *path);

extern int          ri_option_find_file     (char              *fullpath,
                                             const ri_option_t *option,
                                             const char        *file);

extern ri_display_t *ri_option_get_curr_display(
                                             ri_option_t       *option);

/* For debug    */
extern void         ri_option_show_searchpath(
                                             ri_option_t       *option);

/* implemetation specific option */
extern void	        ri_impl_option_insert   (const char        *name, 
                                             void              *val);
extern void        *ri_impl_option_get      (const char        *name);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
