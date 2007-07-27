#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <assert.h>

#include "vector.h"
#include "log.h"
#include "memory.h"
#include "thread.h"
#include "random.h"
#include "queue.h"

#include "framebufferdrv.h"
#include "hdrdrv.h"
#include "sockdrv.h"
#include "openexrdrv.h"

#include "accel.h"
#include "render.h"
#include "polygon.h"
#include "raytrace.h"
#include "reflection.h"
#include "transport.h"
#include "parallel.h"
#include "shading.h"
#include "qmc.h"
#include "pathtrace.h"
#include "hilbert2d.h"
#include "zorder2d.h"
#include "spiral.h"

#ifndef M_PI
#define M_PI 3.1415926532
#endif

//#define MAXWIDTH 4096
#define MAX_SAMPLES_IN_PIXEL 128

static ri_render_t *grender = NULL;    /* global and unique renderer */


typedef struct _render_thread_t
{
	int              thread_id;

	/*
	 * thread local storage for ri_queue operation.
	 */
	int              var;
	ri_queue_node_t *node;

} render_thread_t;

typedef struct _bucket_t {
        int             x, y;          /* bucket location.             */
        int             w, h;          /* bucket size                  */
        ri_vector_t    *pixels;        /* contents of the bucket       */
        ri_float_t     *depths;        /* contents of Z-buffer         */
        int             rendered;
        int             written;
} bucket_t;

typedef struct _sample_t {
        ri_vector_t     radiance;
        ri_float_t      depth;
        ri_float_t      x, y;          /* sample positon with subpixel accuracy */
} sample_t;

/* for multithreading */
typedef struct _pixelinfo_t {
        ri_vector_t    *radiance;      /* RGB                          */
        //ri_float_t       depth;   /* Z                            */
        //ri_float_t       alpha;   /* A                            */

        /* list of smaples in this pixel */
        sample_t        samples[MAX_SAMPLES_IN_PIXEL];
        int             nsamples;
        int             x, y;          /* pixel position */
} pixelinfo_t;

typedef struct _hammersley_sample_t {
        unsigned int    periodx;       /* 2^(xsamples)         */
        unsigned int    periody;       /* 2^(ysamples)         */
        unsigned int   *sigmax;
        unsigned int   *sigmay;
} hammersley_sample_t;

static hammersley_sample_t hammsample;

static int          initialized = 0;       /* grender is initialized?    */

static unsigned int gqmc_instance;     /* QMC ray instance number */

static void     subsample( pixelinfo_t * pixinfo, int x, int y,
                           int threadid );
static void     init_sigma( int xsamples, int ysamples );
static void     sample_subpixel( unsigned int *i,
                                 ri_float_t jitter[2],
                                 int xs, int ys, int xsamples,
                                 int ysamples );
static void     bucket_scheduling( bucket_t * bucketlist, int *currbucket,
                                   int *assignedbucketlist,
                                   ri_parallel_request_t * requestlist,
                                   int nbuckets, int np,
                                   ri_display_drv_t * drv,
                                   ri_display_t * disp );
static void     bucket_rendering( ri_display_drv_t * drv,
                                  ri_display_t * disp );
static void     bucket_write( bucket_t * bucket,
                              ri_display_drv_t * drv,
                              ri_display_t * disp );

#if 0	// TO BE REMOVED
static void     subsample_threaded( ri_vector_t * pixels,
                                    int sx, int sy,
                                    int width, int height );
static void    *subsample_threadfunc( void *arg, int threadid );
#endif
static void     progress_bar( int progress, ri_float_t eta, ri_float_t elapsed );

static void     create_bucket_list(const ri_render_t *render,
                                   ri_queue_t        *q);

static void     render_frame_controller(ri_render_t *render);

static void     render_frame_cleanup(ri_render_t *render);


void
ri_render_init()
{
        static int      ray_max_depth = 5;

        ri_geom_drv_t  *polygon_drv;

#ifdef HAVE_OPENEXR
        /* OpenEXR is available. */
        ri_display_drv_t *openexr_drv = NULL;
#endif

#if defined(WIN32) || defined(WITH_AQUA) || defined(WITH_X11)
        /* Window system is available */
        ri_display_drv_t *fb_drv = NULL;
#endif

        ri_display_drv_t *hdr_drv  = NULL;
        ri_display_drv_t *sock_drv = NULL;

        if ( initialized ) {
                return;
	}

        grender                   = ( ri_render_t *) ri_mem_alloc(
					sizeof( ri_render_t ) );

        grender->geom_drvs        = ri_hash_new();
        grender->display_drvs     = ri_hash_new();

        grender->curr_display_drv = NULL;

        grender->scene            = ri_scene_new();

        grender->bucket_queue     = ri_queue_new();
        grender->bucket_size      = 32;
        grender->bucket_order     = BUCKET_ORDER_SPIRAL;

        grender->context          = ri_context_new();
        grender->progress_handler = NULL;
        grender->ribpath[0]       = '\0';

        grender->is_debug_mode    = 0;

        grender->background_map   = NULL;

        /* generate permutation sequences up to 100 dimension.
         * I think 100 is far enough for rendering integration
         */
        grender->perm_table = faure_permutation( 100 );

	/*
	 * Initialize statistics
	 */
        grender->stat.ngridtravs   = 0;
        grender->stat.ntesttris    = 0;
        grender->stat.nmailboxhits = 0;
        grender->stat.nrays        = 0;

        polygon_drv =
            ( ri_geom_drv_t * ) ri_mem_alloc( sizeof( ri_geom_drv_t ) );
        polygon_drv->parse = ri_polygon_parse;
        ri_render_register_geom_drv( grender, "polygon", polygon_drv );

#ifdef HAVE_OPENEXR
        openexr_drv           = ( ri_display_drv_t * )
        			ri_mem_alloc( sizeof( ri_display_drv_t ) );
        openexr_drv->open     = openexr_dd_open;
        openexr_drv->write    = openexr_dd_write;
        openexr_drv->close    = openexr_dd_close;
        openexr_drv->progress = openexr_dd_progress;
        openexr_drv->name     = strdup( "openexr" );
        openexr_drv->info     =
            strdup( "Save the image as a OpenEXR format(HDR)" );
        ri_render_register_display_drv( grender, "openexr", openexr_drv );
#endif

#if defined(WIN32) || defined(WITH_AQUA) || defined(WITH_X11)
        fb_drv           = ( ri_display_drv_t * )
				ri_mem_alloc( sizeof( ri_display_drv_t ) );
        fb_drv->open     = fb_dd_open;
        fb_drv->write    = fb_dd_write;
        fb_drv->close    = fb_dd_close;
        fb_drv->progress = fb_dd_progress;
        fb_drv->name     = strdup( "framebuffer" );
        fb_drv->info     = strdup( "Output the image to window(LDR)" );
        ri_render_register_display_drv( grender, RI_FRAMEBUFFER, fb_drv );
#endif

        hdr_drv           = ( ri_display_drv_t * )
        			ri_mem_alloc( sizeof( ri_display_drv_t ) );
        hdr_drv->open     = hdr_dd_open;
        hdr_drv->write    = hdr_dd_write;
        hdr_drv->close    = hdr_dd_close;
        hdr_drv->progress = hdr_dd_progress;
        hdr_drv->name     = strdup( "hdr" );
        hdr_drv->info     =
            strdup( "Save the image as a Radiance .hdr format(HDR)" );
        ri_render_register_display_drv( grender, "hdr", hdr_drv );
        ri_render_register_display_drv( grender, RI_FILE, hdr_drv );

        sock_drv           = ( ri_display_drv_t * )
        			ri_mem_alloc( sizeof( ri_display_drv_t ) );
        sock_drv->open     = sock_dd_open;
        sock_drv->write    = sock_dd_write;
        sock_drv->close    = sock_dd_close;
        sock_drv->progress = sock_dd_progress;
        sock_drv->name     = strdup( "socket" );
        sock_drv->info     = strdup( "Send image data to external program" );
        ri_render_register_display_drv( grender, "socket", sock_drv );


        initialized = 1;

        /* TODO: remove this */
        ri_impl_option_insert( "raytrace_max_depth", &ray_max_depth );

        ri_timer_start( grender->context->timer, "TOTAL rendering time" );

}

ri_render_t *
ri_render_get()
{
        return grender;
}

void
ri_render_free()
{
        if ( !initialized ) {
                return;
	}	

        ri_hash_free( grender->geom_drvs );
        ri_hash_free( grender->display_drvs );

	// ri_scene_free() was already called in render_frame_cleanup(),
	// thus no need to to call it.
	//ri_scene_free( grender->scene );

        ri_context_free( grender->context );
        ri_mem_free( grender );

	grender = NULL;
}

void
ri_render_frame(  )
{
	ri_scene_t       *scene;
        time_t            tm;

	scene = ri_render_get()->scene;

	/*
	 * Setup renderer.
	 */
	ri_render_setup(ri_render_get());


        /*
	 * Setup scene. calculate scene bounding box, etc.
	 */
	ri_scene_setup( scene );

	create_bucket_list( ri_render_get(), ri_render_get()->bucket_queue);

        ri_parallel_barrier();

	{
		char message[1024];

		time( &tm );
		strcpy( message, ctime( &tm ) );
		message[strlen( message ) - 1] = '\0';  // strip last '\n'
		ri_log( LOG_INFO, "Start rendering on %s", message );
	}

	/* ------------------------------------------------------------------
	 *
         * Render pass. render the scene.
	 */
        ri_timer_start( ri_render_get()->context->timer, "Render frame" );

	render_frame_controller(ri_render_get());

        ri_timer_end( ri_render_get()->context->timer, "Render frame" );
	
	/*
	 *
	 * ---------------------------------------------------------------- */

	render_frame_cleanup(ri_render_get());

        ri_parallel_barrier();
}

void
ri_render_register_geom_drv( ri_render_t * render, const char *type,
                             ri_geom_drv_t * drv )
{
        ri_hash_insert( render->geom_drvs, type, drv );
}

void
ri_render_register_display_drv( ri_render_t * render, const char *type,
                                ri_display_drv_t * drv )
{
        ri_hash_insert( render->display_drvs, type, drv );
}


void
ri_render_set_progress_handler( ri_render_t * render,
                                void ( *func ) ( void ) )
{
        render->progress_handler = func;
}

void
ri_render_setup(ri_render_t *render)
{
	int                      nthreads;
	int                      np;
	int                      my_id;
        ri_parallel_request_t   *requestlist;

        int                      w, h;
        int                      xsamples, ysamples;
        ri_display_drv_t        *drv;
        RtToken                  dsp_type;
        char                    *output;
        char                     message[1024];
        ri_light_t              *light;
        ri_option_t             *opt;
        ri_display_t            *disp;

	ri_scene_t              *scene;

	scene = ri_render_get()->scene;

        w = ri_render_get()->context->option->camera->
            horizontal_resolution;
        h = ri_render_get()->context->option->camera->
            vertical_resolution;
        output = ri_render_get()->context->option->display->display_name;
        dsp_type = ri_render_get()->context->option->display->display_type;

        drv = ( ri_display_drv_t * ) ri_hash_lookup(
		ri_render_get()->display_drvs, dsp_type );

        if ( drv == NULL ) {
#ifdef WIN32
                sprintf( message,
                         "not supported display driver [ \"%s\" ]. force use [ \"framebuffer\" ]",
                         dsp_type );
                ri_log( LOG_WARN, message );
                drv = ( ri_display_drv_t * )
                    ri_hash_lookup( ri_render_get(  )->display_drv,
                                    "framebuffer" );
#else
                sprintf( message,
                         "not supported display driver [ \"%s\" ]. force use [ \"file\" ]",
                         dsp_type );
                ri_log( LOG_WARN, message );
                drv = ( ri_display_drv_t * )
                    ri_hash_lookup( ri_render_get(  )->display_drvs,
                                    RI_FILE );
#endif
        }

        ri_log_and_return_if( drv == NULL );

	ri_render_get()->curr_display_drv = drv;

        opt = ri_render_get(  )->context->option;
        disp = ri_render_get(  )->context->option->display;

        xsamples = ( int ) disp->sampling_rates[0];
        ysamples = ( int ) disp->sampling_rates[1];

	/*
	 * Init QMC sample 
	 */
        init_sigma( xsamples, ysamples );

	my_id = ri_parallel_taskid();

        if ( my_id == 0 ) {             /* Master node */
                if ( strcmp( disp->display_format, "float" ) == 0 ||
                     strcmp( dsp_type, "hdr" ) == 0 ||
                     strcmp( dsp_type, "openexr" ) == 0 ||
                     strcmp( dsp_type, RI_FILE ) == 0 ) {
                        if ( !drv->
                             open( output, w, h, 32, RI_RGB, "float" ) ) {
                                ri_log( LOG_WARN,
                                        "Can't open \"%s\" display driver.\n",
                                        dsp_type );
                                return;
                        }
                } else {
                        if ( !drv->open( output, w, h, 8, RI_RGB, "byte" ) ) {

                                /* Try to open file display driver. */
                                ri_log( LOG_WARN,
                                        "Can't open \"%s\" display driver."
                                        " Use \"file\" display driver instead.",
                                        dsp_type );

                                free( disp->display_type );
                                disp->display_type = strdup( RI_FILE );
                                dsp_type = disp->display_type;

                                drv = ( ri_display_drv_t * )
                                    ri_hash_lookup( ri_render_get(  )->
                                                    display_drvs, RI_FILE );
                                if ( !drv )
                                        exit( -1 );


                                if ( !drv->open( output, w, h,
                                                 32, RI_RGB, "float" ) ) {
                                        printf
                                            ( "??? can't open display driver" );
                                        printf( " [ %s ].\n", dsp_type );
                                        return;
                                }

                        }
                }
        }

        if ( !ri_list_first( scene->light_list ) ) {

		/*
		 * No light in the scene.
		 * Create domelight.
		 */

                ri_log( LOG_WARN, "there is no light. create domelight." );

                light = ri_light_new(  );
                light->type = LIGHTTYPE_DOME;

                ri_scene_add_light( scene, light );

        } else {

		// TODO: support for multiple light sources.

                // Currently, only 1 light is supported...
                light =
                    ( ri_light_t * ) ri_list_first( scene->light_list)->data;
        }

        /*
	 * multithreading setup
	 */
	{
		ri_thread_initialize(  );

		nthreads = render->context->option->nthreads;
		if ( nthreads > RI_MAX_THREADS ) {
			nthreads = RI_MAX_THREADS;
		}

		if ( nthreads > 1 ) {
			ri_log(LOG_INFO, "number of threads to use = %d\n", nthreads );
		} else {
			ri_log(LOG_INFO, "Single thread rendering");
		}

		render->nthreads = nthreads;
	}

        /* 
	 * setup for parallel rendering
	 */
	{
		np = ri_parallel_ntasks(  );
		my_id = ri_parallel_taskid(  );


		/*
		 * requestlist[0] is never used.
		 */
		requestlist = ( ri_parallel_request_t * )
		    ri_mem_alloc( sizeof( ri_parallel_request_t ) * np );
	}

}

/* ----------------------------------------------------------------------------
 *
 * private functions
 *
 * ------------------------------------------------------------------------- */

static void
create_bucket_list(
	const ri_render_t *render,
	ri_queue_t *q)			/* [out] */
{

        int              i, j;
        int              idx;

        int              width, height;
        int              screen_width, screen_height;

	int              bucket_size;
        int              nbuckets;
        int              nwidthdiv, nheightdiv;
        int              nxbuckets, nybuckets;
        int              width_reminder, height_reminder;
	
        bucket_t        *bucket_list;
	uint32_t         bucket_id;

	int              tmp_ver;
	ri_queue_node_t *tmp_node;

        uint32_t         xp, yp;

	ri_camera_t     *camera;

        camera = render->context->option->camera;

	bucket_size     = render->bucket_size;

        screen_width    = camera->horizontal_resolution;
        screen_height   = camera->vertical_resolution;

        nwidthdiv       = screen_width / bucket_size;
        nheightdiv      = screen_height / bucket_size;

        width_reminder  = screen_width % bucket_size;
        height_reminder = screen_height % bucket_size;

        if ( width_reminder ) {
                nxbuckets = nwidthdiv + 1;
        } else {
                nxbuckets = nwidthdiv;
	}

        if ( height_reminder ) {
                nybuckets = nheightdiv + 1;
        } else {
                nybuckets = nheightdiv;
	}

        nbuckets = nxbuckets * nybuckets;

        bucket_list = ( bucket_t * ) ri_mem_alloc( sizeof( bucket_t ) *
                                                  nbuckets );

        for ( j = 0; j < nybuckets; j++ ) {
                for ( i = 0; i < nxbuckets; i++ ) {
                        idx = j * nxbuckets + i;

                        bucket_list[idx].x = i * bucket_size;
                        bucket_list[idx].y = j * bucket_size;

                        if ( i == ( nxbuckets - 1 ) && width_reminder ) {
                                width = width_reminder;
                        } else {
                                width = bucket_size;
                        }

                        if ( j == ( nybuckets - 1 ) && height_reminder ) {
                                height = height_reminder;
                        } else {
                                height = bucket_size;
                        }

                        bucket_list[idx].w = width;
                        bucket_list[idx].h = height;

                        bucket_list[idx].rendered = 0;
                        bucket_list[idx].written = 0;
                }
        }

        // Z curve order scan setup
        zorder_setup( screen_width, screen_height, bucket_size );

        // spiral order scan setup
        spiral_setup( screen_width, screen_height, bucket_size );


	while (1) {

		if (render->bucket_order == BUCKET_ORDER_SCANLINE) {

			xp++;
			if (xp >= (uint32_t)nxbuckets) {
				yp++;
				xp = 0;
			}

			if (yp >= (uint32_t)nybuckets) break;
			
		} else if (render->bucket_order == BUCKET_ORDER_SPIRAL) {

			if (!spiral_get_nextlocation( &xp, &yp )) break;

		}

                bucket_id = xp + nxbuckets * yp;


		ri_queue_push(
			q,
			(const void *)&bucket_list[bucket_id],
			sizeof(bucket_t),
			&tmp_ver,		/* not used */
			&tmp_node);		/* not used */
	}

	/*
	 * Now we have a queue of buckets,
	 * whose elements are ordered by user specific rendering order
	 * (scanline, z order, spiral, etc.)
	 */
			
	/*
	 * bucket data are copied into queue element,
	 * so bucket_list is no longer used.
	 */
        ri_mem_free( bucket_list );
}

/*
 * sample subpixel. trace ray from camera through pixel in (x, y)
 */
static void
subsample( pixelinfo_t * pixinfo, int x, int y, int threadid )
{
        int             i;
        int             w, h;
        int             xs, ys;
        int             xsamples, ysamples;
        int             currsample;
        unsigned int    subinstance;
        ri_float_t      inv_nsamples;
        ri_float_t      jitter[2];
        ri_vector_t     dir;
        ri_vector_t     from;
        ri_vector_t     accumrad;
        ri_ray_t        ray;
        ri_display_t   *disp;
        ri_camera_t    *camera;
        ri_transport_info_t result;

        camera = ri_render_get(  )->context->option->camera;

        w = camera->horizontal_resolution;
        h = camera->vertical_resolution;

        disp = ri_render_get(  )->context->option->display;
        xsamples = disp->sampling_rates[0];
        ysamples = disp->sampling_rates[1];

        inv_nsamples = 1.0f / ( ri_float_t) ( ysamples * xsamples );

        pixinfo->nsamples = xsamples * ysamples;
        for ( i = 0; i < pixinfo->nsamples; i++ ) {
                pixinfo->samples[i].depth = 0.0f;
                ri_vector_zero( &( pixinfo->samples[i].radiance ) );
                //pixinfo->alpha = 0.0f;
        }

        currsample = 0;
        ri_vector_zero( &accumrad );
        for ( ys = 0; ys < ysamples; ys++ ) {
                for ( xs = 0; xs < xsamples; xs++ ) {

                        sample_subpixel( &subinstance,
                                         jitter, xs, ys, xsamples,
                                         ysamples );


			ri_camera_get_pos_and_dir(
				&from, &dir,
				camera,
				(ri_float_t)(x + jitter[0]),
				(ri_float_t)(y + jitter[1]));

                        ri_vector_copy( &( ray.org ), &from );
                        ri_vector_sub( &( ray.dir ), &dir, &from );
                        ri_vector_normalize3( &( ray.dir ) );

                        /* dimension 1 for screen x coordinate sample point,
                         * dimension 2 for screen y coordinate sample point.
                         */
                        ray.d = 3;

                        /* Ray's instance number for generalized scrambled
                         * Halton sequence or generalized scrambled
                         * Hammersley point set.
                         * This is used in subsequent QMC sampling.
                         */
                        //ray.i = gqmc_instance * (xsamples * ysamples)
                        //      + subinstance;
                        ray.i = subinstance;
                        gqmc_instance += ( xsamples * ysamples );
                        //printf("instance = %d\n", gqmc_instance);
                        //printf("subinstance = %d\n", subinstance);
                        //printf("subsample:instance = %d\n", ray.i);

                        /* assign threadid to ray's thread number */
                        ray.thread_num = threadid;

                        ri_transport_sample( ri_render_get(  ),
                                             &ray, &result );

                        ri_vector_add( &accumrad,
                                       &accumrad, &result.radiance );

#if 0	// TODO: fixme!
                        if ( result.hit ) {
                                pixinfo->samples[currsample].depth
                                    += result.ray.isectt * inv_nsamples;
                        } else {
                                pixinfo->samples[currsample].depth
                                    += RI_INFINITY * inv_nsamples;
                        }
#endif
                }
        }

        ri_vector_scale( &accumrad,
                         &accumrad, ( (ri_float_t)1.0 / ( xsamples * ysamples ) ) );

        ri_vector_copy( pixinfo->radiance, &accumrad );
}

/* two-dimensional Hammersley points for anti-aliasing.
 * see:
 * "Strictly Deterministic Sampling Methods in Computer Graphics"
 * Alexander Keller, mental images technical report, 2001
 */
static void
sample_subpixel( unsigned int *i, ri_float_t jitter[2],
                 int xs, int ys, int xsamples, int ysamples )
{
        unsigned int    j, k;
        ri_float_t          offsetx, offsety;

        j = xs & ( hammsample.periodx - 1 );
        k = ys & ( hammsample.periodx - 1 );

        /* Instance number. */
        (*i) = j * hammsample.periodx + hammsample.sigmax[k];

        jitter[0] = ( ri_float_t ) xs
            +
            ( ri_float_t ) hammsample.sigmax[k] /
            ( ri_float_t ) hammsample.periodx;
        jitter[1] =
            ( ri_float_t ) ys +
            ( ri_float_t ) hammsample.sigmay[j] /
            ( ri_float_t ) hammsample.periody;

        jitter[0] /= ( ri_float_t ) xsamples;
        jitter[1] /= ( ri_float_t ) ysamples;

        offsetx = 0.5 / ( xsamples * xsamples );
        offsety = 0.5 / ( ysamples * ysamples );

        jitter[0] += offsetx;
        jitter[1] += offsety;

}

/* Initialize permutation for two-dimensional Hammersley points for
 * anti-aliasing.
 *
 * see:
 * "Strictly Deterministic Sampling Methods in Computer Graphics"
 * Alexander Keller, mental images technical report, 2001
 */
void
init_sigma( int xsamples, int ysamples )
{
        unsigned int    i, inverse, digit, bits;

        /* initialize   */
        hammsample.periodx = xsamples;
        hammsample.periody = ysamples;

        hammsample.sigmax =
            ( unsigned int * ) malloc( sizeof( unsigned int ) *
                                       hammsample.periodx );
        hammsample.sigmay =
            ( unsigned int * ) malloc( sizeof( unsigned int ) *
                                       hammsample.periody );

        /* x */
        for ( i = 0; i < hammsample.periodx; i++ ) {
                digit = hammsample.periodx;
                inverse = 0;

                for ( bits = i; bits; bits >>= 1 ) {
                        digit >>= 1;

                        if ( bits & 1 ) {
                                inverse += digit;
                        }
                }

                hammsample.sigmax[i] = inverse;
        }

        /* y */
        for ( i = 0; i < hammsample.periody; i++ ) {
                digit = hammsample.periody;
                inverse = 0;

                for ( bits = i; bits; bits >>= 1 ) {
                        digit >>= 1;

                        if ( bits & 1 ) {
                                inverse += digit;
                        }
                }

                hammsample.sigmay[i] = inverse;
        }
}

#if 0
/*
 * bucket rendering.
 */
static void
bucket_rendering( ri_display_drv_t * drv, ri_display_t * disp )
{
        int             i, j, n;
        int             idx;
        int             np;
        int             myid;
        int             x, y;
        int             sx, sy;
        int             width, height;
        int             screenwidth, screenheight;
        int             bucket_width, bucket_height;
        bucket_t       *bucketlist;    /* all bucket lists     */
        int            *assignedbucketlist;
        int             nbuckets;
        int             currbucket, allocedbucket;
        int             nwidthdiv, nheightdiv;
        int             nxbuckets, nybuckets;
        int             widthreminder, heightreminder;
        int             datasize;
        ri_camera_t    *camera;
        ri_parallel_status_t status;
        ri_parallel_request_t *requestlist;
        ri_float_t          elapsed;       /* elapsed time from rendering starts */
        ri_float_t          etl;           /* estimate time left */
        pixelinfo_t     pixinfo;
        unsigned int    xp, yp;        /* (x, y) location of scanning position */

        int             nthreads;

        /*
	 * multithread setup
	 */
        ri_thread_initialize(  );

        nthreads = ri_render_get(  )->context->option->nthreads;
        if ( nthreads > RI_MAX_THREADS )
                nthreads = RI_MAX_THREADS;

        if ( nthreads > 1 ) {
                ri_log(LOG_INFO, "number of threads to use = %d\n", nthreads );
        } else {
                ri_log(LOG_INFO, "Do single threaded rendering\n");
	}

        // parallel rendering setup
        np = ri_parallel_ntasks(  );
        myid = ri_parallel_taskid(  );

        /* requestlist[0] is never used. */
        requestlist = ( ri_parallel_request_t * )
            ri_mem_alloc( sizeof( ri_parallel_request_t ) * np );

        // setup bucket.
        camera = ri_render_get(  )->context->option->camera;

        screenwidth = camera->horizontal_resolution;
        screenheight = camera->vertical_resolution;

        nwidthdiv = screenwidth / bucketwidth;
        nheightdiv = screenheight / bucketheight;

        widthreminder = screenwidth % bucketwidth;
        heightreminder = screenheight % bucketheight;

        if ( widthreminder )
                nxbuckets = nwidthdiv + 1;
        else
                nxbuckets = nwidthdiv;

        if ( heightreminder )
                nybuckets = nheightdiv + 1;
        else
                nybuckets = nheightdiv;

        nbuckets = nxbuckets * nybuckets;

        bucketlist = ( bucket_t * ) ri_mem_alloc( sizeof( bucket_t ) *
                                                  nbuckets );

        assignedbucketlist =
            ( int * ) ri_mem_alloc( sizeof( int ) * nbuckets );

        for ( j = 0; j < nybuckets; j++ ) {
                for ( i = 0; i < nxbuckets; i++ ) {
                        idx = j * nxbuckets + i;

                        bucketlist[idx].x = i * bucketwidth;
                        bucketlist[idx].y = j * bucketwidth;

                        if ( i == ( nxbuckets - 1 ) && widthreminder ) {
                                width = widthreminder;
                        } else {
                                width = bucketwidth;
                        }

                        if ( j == ( nybuckets - 1 ) && heightreminder ) {
                                height = heightreminder;
                        } else {
                                height = bucketheight;
                        }

                        bucketlist[idx].w = width;
                        bucketlist[idx].h = height;

                        bucketlist[idx].rendered = 0;
                        bucketlist[idx].written = 0;

                        bucketlist[idx].pixels = ( ri_vector_t * )
                            ri_mem_alloc( sizeof( ri_vector_t ) *
                                          width * height );

                        for ( n = 0; n < width * height; n++ ) {
                                ri_vector_zero( &
                                                ( bucketlist[idx].
                                                  pixels[n] ) );
                        }
                }
        }

        // hilbert order scan setup
        hil_setup( screenwidth, screenheight, bucketwidth );

        // Z curve order scan setup
        zorder_setup( screenwidth, screenheight, bucketwidth );

        // spiral order scan setup
        spiral_setup( screenwidth, screenheight, bucketwidth );

        if ( myid == 0 ) {             /* master node  */
                currbucket = 0;
                allocedbucket = currbucket;

                for ( i = 1; i < np; i++ ) {
                        allocedbucket++;
                        assignedbucketlist[i] = allocedbucket;
                        ri_parallel_send( &allocedbucket, sizeof( int ), i,
                                          0 );

                        datasize = sizeof( ri_vector_t ) *
                            bucketlist[allocedbucket].w *
                            bucketlist[allocedbucket].h;

                        /* non-blocking recv */
                        ri_parallel_irecv( bucketlist[allocedbucket].
                                           pixels, datasize, i, 0,
                                           &( requestlist[i] ) );
                }
        } else {                       /* child node   */
                /* get bucket ID */
                ri_parallel_recv( &currbucket, sizeof( int ), 0, 0,
                                  &status );

        }

#if 0
        while ( currbucket < nbuckets ) {
#else
#if 0
        /* Z curve order scan version */
        while ( zorder_get_nextlocation( &xp, &yp ) ) {
#else
        /* spiral order scan version */
        while ( spiral_get_nextlocation( &xp, &yp ) ) {
#endif
                currbucket = xp + nxbuckets * yp;


#endif
                //printf("rank(%d): rendering bucket %d / %d\n", myid, currbucket + 1, nbuckets);
                sx = bucketlist[currbucket].x;
                sy = bucketlist[currbucket].y;
                width = bucketlist[currbucket].w;
                height = bucketlist[currbucket].h;

                if ( nthreads ) {
#if 0
                        /* threaded version. */
                        subsample_threaded( subsample_workpile,
                                            bucketlist[currbucket].pixels,
                                            sx, sy, width, height );
#endif

                        if ( myid == 0 ) {
                                /* For each node(exclude master
                                 * node), check if bucket
                                 * rendering is finished.
                                 * And if so, assign next
                                 * bucket task.
                                 */
                                bucket_scheduling( bucketlist,
                                                   &allocedbucket,
                                                   assignedbucketlist,
                                                   requestlist,
                                                   nbuckets,
                                                   np, drv, disp );
                        }
                } else {
                        /* non threaded version. */
                        /* render bucket region [x, y], [x + w, y + h] */
                        for ( y = 0; y < height; y++ ) {
                                for ( x = 0; x < width; x++ ) {
                                        idx = y * width + x;

                                        pixinfo.radiance =
                                            &( bucketlist[currbucket].
                                               pixels[idx] );

                                        subsample( &pixinfo,
                                                   sx + x, sy + y, 0 );

                                        if ( myid == 0 ) {
                                                /* For each node(exclude master
                                                 * node), check if bucket
                                                 * rendering is finished.
                                                 * And if so, assign next
                                                 * bucket task.
                                                 */
                                                bucket_scheduling
                                                    ( bucketlist,
                                                      &allocedbucket,
                                                      assignedbucketlist,
                                                      requestlist,
                                                      nbuckets, np, drv,
                                                      disp );
                                        }
                                }
                        }
                }

                if ( myid == 0 ) {     /* master node */
                        bucket_write( &( bucketlist[currbucket] ), drv,
                                      disp );
                        allocedbucket++;
                        currbucket = allocedbucket;

                        /* calculate estimate time left. */

                        elapsed =
                            ri_timer_elapsed_current( ri_render_get(  )->
                                                      context->timer,
                                                      "Render frame" );

                        if ( currbucket == 1 ) {
                                etl =
                                    elapsed * ( ri_float_t ) ( ( nbuckets ) +
                                                           1 );

                        } else {
                                etl = ( elapsed / ( ri_float_t ) currbucket );
                                etl *=
                                    ( ri_float_t ) ( nbuckets - currbucket );
                        }

                        /* This output line is parsed by GUI frontend.
                         * Should not change the output format!
                         */
                        //printf("Bucket %d/%d  ", currbucket + 1, nbuckets);
                        printf( "\r" );
                        progress_bar( ( int )
                                      ( ( ( ri_float_t ) currbucket /
                                          nbuckets ) * 100.0 ), etl,
                                      elapsed );
                        //printf(" %3d %%",
                        //      (int)(((ri_float_t)currbucket / nbuckets) * 100.0));
                        //printf("  ETL %d s.", (int)etl);
                        //printf("  Elapsed %d s.", (int)elapsed);
                        //printf("   \r");
                        fflush( stdout );

                } else {               /* child node */
                        /* send rendered pixel of bucket */

                        datasize = sizeof( ri_vector_t ) *
                            bucketlist[currbucket].w *
                            bucketlist[currbucket].h;

                        ri_parallel_send( bucketlist[currbucket].pixels,
                                          datasize, 0, 0 );

                        /* recv next bucket task */
                        ri_parallel_recv( &currbucket,
                                          sizeof( int ), 0, 0, &status );
                }
        }

        if ( myid == 0 ) {
                for ( i = 1; i < np; i++ ) {
                        if ( assignedbucketlist[i] > 0 ) {
                                ri_parallel_wait( &( requestlist[i] ),
                                                  &status );

                                idx = assignedbucketlist[i];
                                bucket_write( &( bucketlist[idx] ), drv,
                                              disp );

                                /* not used message */
                                ri_parallel_send( &nbuckets,
                                                  sizeof( int ), i, 0 );
                        }
                }
        }

        /* wait until all node come here. */
        ri_parallel_barrier(  );

        for ( i = 0; i < nbuckets; i++ ) {
                ri_mem_free( bucketlist[i].pixels );
        }
        ri_mem_free( bucketlist );
        ri_mem_free( requestlist );
        ri_mem_free( assignedbucketlist );

        ri_thread_shutdown(  );
}
#endif


#if 0 /* to be removed */

/*
 * bucket task scheduling.
 */
static void
bucket_scheduling( bucket_t * bucketlist,
                   int *currbucket,
                   int *assignedbucketlist,
                   ri_parallel_request_t * requestlist,
                   int nbuckets,
                   int np, ri_display_drv_t * drv, ri_display_t * disp )
{
        int             i;
        int             finished;
        int             idx;
        int             recvsize;
        ri_parallel_status_t status;
        bucket_t       *bucketp;

        for ( i = 1; i < np; i++ ) {
                /* test if i'th node finished bucket rendering */
                finished =
                    ri_parallel_test( &( requestlist[i] ), &status );

                if ( finished ) {
                        /* free request handle */
                        idx = assignedbucketlist[i];

                        if ( idx >= nbuckets || idx < 0 ) {
                                //printf("??? idx >= nbuckets || idx < 0\n");
                                continue;
                        } else {
                                bucket_write( &( bucketlist[idx] ), drv,
                                              disp );
                        }

                        ( *currbucket )++;
                        if ( ( *currbucket ) >= nbuckets ) {
                                /* all bucket is assigned.
                                 */
                                assignedbucketlist[i] = -1;
                                /* send next bucket index to i'th node. */
                                ri_parallel_send( currbucket,
                                                  sizeof( int ), i, 0 );
                        } else {

                                assignedbucketlist[i] = ( *currbucket );

                                /* send next bucket index to i'th node. */
                                ri_parallel_send( currbucket,
                                                  sizeof( int ), i, 0 );

                                bucketp = &( bucketlist[( *currbucket )] );
                                recvsize = sizeof( ri_vector_t ) *
                                    bucketp->w * bucketp->h;

                                /* non-blocking recv.
                                 * reuse request handle
                                 */
                                ri_parallel_irecv( bucketp->pixels,
                                                   recvsize,
                                                   i, 0,
                                                   &( requestlist[i] ) );



                        }
                }
        }
}
#endif

static void
bucket_write( bucket_t * bucket,
              ri_display_drv_t * drv, ri_display_t * disp )
{
        int             n;
        int             width, height;
        int             screenwidth, screenheight;
        int             x, y;
        int             sx, sy;
        ri_vector_t     rad;
        ri_float_t          floatcol[3];
        unsigned char   col[3];
        ri_camera_t    *camera;

        camera = ri_render_get(  )->context->option->camera;

        screenwidth = camera->horizontal_resolution;
        screenheight = camera->vertical_resolution;

        x = bucket->x;
        y = bucket->y;
        width = bucket->w;
        height = bucket->h;

        for ( sy = 0; sy < height; sy++ ) {
                for ( sx = 0; sx < width; sx++ ) {
                        n = sy * width + sx;
                        ri_vector_copy( &rad, &bucket->pixels[n] );

                        if ( strcmp( disp->display_format,
                                     "float" ) == 0 ||
                             strcmp( disp->display_type, "hdr" ) == 0 ||
                             strcmp( disp->display_type, "openexr" ) == 0
                             || strcmp( disp->display_type, "socket" ) == 0
                             || strcmp( disp->display_type,
                                        RI_FILE ) == 0 ) {
                                floatcol[0] = rad.f[0];
                                floatcol[1] = rad.f[1];
                                floatcol[2] = rad.f[2];

                                drv->write( sx + x,
                                            screenheight - ( sy + y ) - 1,
                                            floatcol );
                        } else {
                                //ri_tonemap_apply( disp, &rad );

                                col[0] =
                                    ( unsigned char ) ( rad.f[0] * 255.0 );
                                col[1] =
                                    ( unsigned char ) ( rad.f[1] * 255.0 );
                                col[2] =
                                    ( unsigned char ) ( rad.f[2] * 255.0 );

                                drv->write( sx + x,
                                            //screenheight - (sy + y) - 1,
                                            ( sy + y ), col );
                        }
                }
        }
}

#if 0
static void
subsample_threaded( workpile_t * wp,
                    ri_vector_t * pixels,
                    int sx, int sy, int width, int height )
{
        int             x, y;
        int             idx;
        pixelinfo_t    *pixinfo;

        pixinfo = ( pixelinfo_t * ) ri_mem_alloc( sizeof( pixelinfo_t ) *
                                                  width * height );

        /* post works(jobs) */
        for ( y = 0; y < height; y++ ) {
                for ( x = 0; x < width; x++ ) {
                        idx = y * width + x;
                        pixinfo[idx].radiance = &( pixels[idx] );
                        pixinfo[idx].x = sx + x;
                        pixinfo[idx].y = sy + y;

                        work_put( wp, ( void * ) &( pixinfo[idx] ) );
                }
        }

        /* wait until all work ends */
        work_wait( wp );

        ri_mem_free( pixinfo );
}

static void    *
subsample_threadfunc( void *arg, int threadid )
{
        pixelinfo_t    *pixinfo;

        pixinfo = ( pixelinfo_t * ) arg;

        subsample( pixinfo, pixinfo->x, pixinfo->y, threadid );

        return NULL;
}
#endif

static void
progress_bar( int progress, ri_float_t eta, ri_float_t elapsed )
{
        int             h, m, s;
        int             i;
        int             len = progress / 3;
        int             tm;
        static int      idx = 0;
        static char     progchar[] = "|\\-/";

        printf( "%c ", progchar[idx] );
        idx++;
        if ( idx >= (int)strlen( progchar ) )
                idx = 0;

        printf( "|" );
        for ( i = 0; i < 33; i++ ) {
                if ( i < len ) {
                        printf( "=" );
                } else {
                        printf( " " );
                }
        }
        printf( "|" );

        printf( " %3d%%", progress );

        tm = ( int ) eta;
        s = tm % 60;
        m = (tm / 60) % 60;
        h = tm / 3600;

        if ( h > 99 ) {
                printf( "  ETA >99:99:99" );

        } else {
                printf( "  ETA %2d:%2d:%2d", h, m, s );
        }

        tm = ( int ) elapsed;
        s = tm % 60;
        m = tm / 60;
        h = tm / 3600;

        if ( h > 99 ) {
                printf( "  Elap >99:99:99" );

        } else {
                printf( "  Elap %2d:%2d:%2d", h, m, s );
        }

}

static void *
render_bucket_thread_func(void *arg)
{
	int              ret;
	bucket_t         bucket;
	uint32_t         data_size;
	render_thread_t *info;

	info = (render_thread_t *)arg;

	while (1) {

		ret = ri_queue_pop(
			ri_render_get()->bucket_queue,
			(void *)&bucket,
			&data_size,
			&info->var,
			&info->node);
		
		if (ret != 0) {
			/* no items in the queue. */
			break;
		}
	}
	

	return NULL;
}

void
render_frame_controller(ri_render_t *render)
{
	int i;
	int ret;
	int nthreads;
	ri_thread_t *threads;

	render_thread_t *thread_tls;

	nthreads   = render->nthreads;

	if (nthreads == 0) nthreads = 1;

	threads    = (ri_thread_t *)ri_mem_alloc(
					sizeof(ri_thread_t) * nthreads);
	thread_tls = (render_thread_t *)ri_mem_alloc(
					sizeof(render_thread_t) * nthreads);

	/*
	 * Invoke threads
	 */
	for (i = 0; i < nthreads; i++) {

		thread_tls[i].thread_id = i;

		ret = ri_thread_create(
			&threads[i],
			render_bucket_thread_func,
			&thread_tls[i]);

	}
	

	/*
	 * Wait threads
	 */
	for (i = 0; i < nthreads; i++) {
		ri_thread_join(&threads[i]);
	}	
}

void
render_frame_cleanup(ri_render_t *render)
{
	char              message[1024];
	ri_option_t      *opt = render->context->option;

	int               my_id;
	time_t            tm;

	my_id = ri_parallel_taskid();
		
	if ( my_id == 0 ) {
		printf( "\n" );
		ri_raytrace_statistics(  );

		ri_shade_statistics(  );
	}

	ri_timer_start( render->context->timer, "Clean up" );

	ri_scene_free( grender->scene );
	grender->scene = NULL;

	ri_timer_end( render->context->timer, "Clean up" );
	ri_timer_end( render->context->timer,
		      "TOTAL rendering time" );

	if ( my_id == 0 ) {
		render->curr_display_drv->close();
		ri_timer_dump( ri_render_get()->context->timer );
	}

	time( &tm );
	strcpy( message, ctime( &tm ) );
	message[strlen( message ) - 1] = '\0';  // strip last '\n'
	ri_log( LOG_INFO, "End rendering on %s", message );
}

