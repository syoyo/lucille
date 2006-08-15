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
#include "accel.h"
#include "log.h"
#include "memory.h"
#include "render.h"
#include "polygon.h"
#include "tiffdrv.h"
#include "framebufferdrv.h"
#include "hdrdrv.h"
#include "sockdrv.h"
#include "openexrdrv.h"
#include "raytrace.h"
#include "reflection.h"
#include "photonmap.h"
#include "prt.h"
#include "irradcache.h"
#include "bssrdf.h"
#include "random.h"
#include "transport.h"
#include "parallel.h"
#include "shading.h"
#include "thread.h"
#include "qmc.h"
#include "metropolis.h"
#include "pathtrace.h"
#include "hilbert2d.h"
#include "zorder2d.h"
#include "spiral.h"

#ifndef M_PI
#define M_PI 3.1415926532
#endif

#define MAXWIDTH 4096
#define MAX_SAMPLES_IN_PIXEL 128

typedef struct _sanline_t
{
	ri_vector_t pixels[MAXWIDTH];		/* RGBA */
	float       depth;			/* Z    */
	
	int row;	
} scanline_t;

typedef struct _bucket_t
{
	int x, y;		/* bucket location.		*/
	int w, h;		/* bucket size			*/
	ri_vector_t *pixels;	/* contents of the bucket	*/
	float       *depths;	/* contents of Z-buffer		*/
	int rendered;
	int written;
} bucket_t;

typedef struct _buckertbuf_t
{
	ri_vector_t *colors;	/* RGB				*/
	float       *depths;	/* Z				*/
	float       *alphas;	/* A				*/
	int          w, h;
} bucketbuf_t;

//static bucketbuf_t gbuckerbuffer[RI_MAX_THREADS]; 

typedef struct _sample_t
{
	ri_vector_t radiance;
	float       depth;
	float       x, y;	/* sample positon with subpixel accuracy */
} sample_t;

/* for multithreading */
typedef struct _pixelinfo_t
{
	ri_vector_t *radiance;	/* RGB				*/
	//float        depth;	/* Z				*/
	//float        alpha;	/* A				*/

	/* list of smaples in this pixel */
	sample_t     samples[MAX_SAMPLES_IN_PIXEL];
	int          nsamples;
	int          x, y;	/* pixel position */
} pixelinfo_t;

/* workpile controller for multi-threaded rendering */
typedef struct _workpile_t
{
	ri_thread_t      *threads[RI_MAX_THREADS];
	ri_mutex_t        lock;
	ri_thread_cond_t *work_wait;
	ri_thread_cond_t *finish_wait;

	void             *(*workfunc)(void *arg, int threadid);
	
	int               nthreads;
	int               threadid;		/* reference for current TID */

	int               maxpile;


	int               nworking;		/* currently running threads */
	int               nwaiting;
	int               npiles;

	int               inp;			/* FIFO input  pointer */
	int               outp;			/* FIFO output pointer */

	void             *pile[1];		/* array of pointers   */
} workpile_t;

typedef struct _hammersley_sample_t
{
	unsigned int  periodx;	/* 2^(xsamples)		*/
	unsigned int  periody;	/* 2^(ysamples)		*/
	unsigned int *sigmax;
	unsigned int *sigmay;
} hammersley_sample_t;

static hammersley_sample_t hammsample;
static workpile_t *subsample_workpile = NULL;

static ri_render_t *grender = NULL;	/* global and unique renderer */
static int         initialized = 0;	/* grender is initialized?    */

static unsigned int gqmc_instance;	/* QMC ray instance number */

static void generate_cache(ri_ray_t *ray, ri_light_t *light);
static void calc_bbox(ri_list_t *geom_list,
		      double min[3], double max[3], double *maxwidth);

static void tonemap(ri_vector_t *result);
static double exposure(double val, double gain, double gamma);

static void calc_irradiance_cache(ri_light_t *light);
static void dof(ri_vector_t *from, ri_vector_t *to,
		double fstop, double flen, double fdist, double imgflen); 
static int rootdeg2(double root[2], double a, double b);
static void subsample(pixelinfo_t *pixinfo, int x, int y, int threadid);
//static void jittering(double jitter[2],
//		      int xs, int ys, int xsamples, int ysamples);
static void init_sigma(int xsamples, int ysamples);
static void sample_subpixel(unsigned int *i,
			    double jitter[2],
		            int xs, int ys, int xsamples, int ysamples);
static void bucket_scheduling(bucket_t *bucketlist,
			      int *currbucket,
			      int *assignedbucketlist,
			      ri_parallel_request_t *requestlist,
			      int nbuckets,
			      int np,
			      ri_display_drv_t *drv,
			      ri_display_t *disp);
static void bucket_rendering(ri_display_drv_t *drv, ri_display_t *disp);
#if 0
static void bucket_write(bucket_t *bucketlist, int nbucket,
			 ri_display_drv_t *drv,
			 ri_display_t *disp);
#endif
static void bucket_write(bucket_t *bucket,
			 ri_display_drv_t *drv,
			 ri_display_t *disp);

static void subsample_threaded(workpile_t *wp,
			       ri_vector_t *pixels,
			       int sx, int sy,
			       int width, int height);
static void *subsample_threadfunc(void *arg, int threadid);
static void progress_bar(int progress);

static void build_glcamera(ri_matrix_t *m, const ri_camera_t *camera);

/* workpile functions */
static workpile_t *work_init(int maxpile,
			     void *(*workfunc)(void *arg, int threadid),
			     int nthreads);
static void work_put(workpile_t *wp, void *ptr);
static void worker(workpile_t *wp);
static void work_wait(workpile_t *wp);
static void work_free(workpile_t *wp);

static ri_bssrdf_t    *bssrdf;
/* map unit square {P = (x,y) | [0,1]^2} to unit disk {P = (x,y)| x^2+y^2 <= 1}
 * preserving area
 */
//static void map_unitdisk(double p[2]);

int use_icachefile = 0;

#ifdef DEBUG
static void dump_irrad(int x, int y, const ri_vector_t *irrad);
#endif

void
ri_render_init()
{
	static int ray_max_depth = 5;

	ri_geom_drv_t    *polygon_drv;

#ifdef HAVE_LIBTIFF
	/* TIFF library is available. */
	ri_display_drv_t *tiff_drv = NULL;
#endif

#ifdef HAVE_OPENEXR
	/* OpenEXR is available. */
	ri_display_drv_t *openexr_drv = NULL;
#endif

#if defined(WIN32) || defined(WITH_AQUA) || defined(WITH_X11)
	/* Window system is available */
	ri_display_drv_t *fb_drv = NULL;
#endif

	ri_display_drv_t *hdr_drv = NULL;
	ri_display_drv_t *sock_drv = NULL;

	if (initialized) return;

	grender = (ri_render_t *)ri_mem_alloc(sizeof(ri_render_t));

	grender->geom_drv    = ri_hash_new();
	grender->display_drv = ri_hash_new();
	grender->geomlist    = ri_list_new();
	grender->lightlist   = ri_list_new();
	grender->context     = ri_context_new();
	grender->accel_grid  = NULL;
	grender->octree      = NULL;
	grender->accel_bsp  = NULL;
	grender->global_photonmap = NULL;
	grender->caustics_photonmap = NULL;
	grender->progress_handler = NULL;
	grender->ribpath[0] = '\0';
	grender->debug_mode = 0;

	/* generate permutation sequences up to 100 dimension.
	 * I think 100 is far enough for rendering integration
	 */
	grender->perm_table = faure_permutation(100);

	grender->stat.ngridtravs   = 0;
	grender->stat.ntesttris    = 0;
	grender->stat.nmailboxhits = 0;
	grender->stat.nrays        = 0;

	polygon_drv = (ri_geom_drv_t *)ri_mem_alloc(sizeof(ri_geom_drv_t));
	polygon_drv->parse = ri_polygon_parse;
	ri_render_register_geom_drv(grender, "polygon", polygon_drv);

#ifdef HAVE_LIBTIFF
	tiff_drv = (ri_display_drv_t *)ri_mem_alloc(sizeof(ri_display_drv_t));
	tiff_drv->open     = tiff_dd_open;
	tiff_drv->write    = tiff_dd_write;
	tiff_drv->close    = tiff_dd_close;
	tiff_drv->progress = tiff_dd_progress;
	tiff_drv->name     = strdup("tiff");
	tiff_drv->info     = strdup("Save the image as a TIFF format(HDR, LDR)");
	ri_render_register_display_drv(grender, "tiff", tiff_drv);
#endif

#ifdef HAVE_OPENEXR
	openexr_drv = (ri_display_drv_t *)
				ri_mem_alloc(sizeof(ri_display_drv_t));
	openexr_drv->open     = openexr_dd_open;
	openexr_drv->write    = openexr_dd_write;
	openexr_drv->close    = openexr_dd_close;
	openexr_drv->progress = openexr_dd_progress;
	openexr_drv->name     = strdup("openexr");
	openexr_drv->info     = strdup("Save the image as a OpenEXR format(HDR)");
	ri_render_register_display_drv(grender, "openexr", openexr_drv);
#endif

#if defined(WIN32) || defined(WITH_AQUA) || defined(WITH_X11)
	fb_drv = (ri_display_drv_t *)ri_mem_alloc(sizeof(ri_display_drv_t));
	fb_drv->open     = fb_dd_open;
	fb_drv->write    = fb_dd_write;
	fb_drv->close    = fb_dd_close;
	fb_drv->progress = fb_dd_progress;
	fb_drv->name     = strdup("framebuffer");
	fb_drv->info     = strdup("Output the image to window(LDR)");
	ri_render_register_display_drv(grender, RI_FRAMEBUFFER, fb_drv);
#endif

	hdr_drv = (ri_display_drv_t *)ri_mem_alloc(sizeof(ri_display_drv_t));
	hdr_drv->open     = hdr_dd_open;
	hdr_drv->write    = hdr_dd_write;
	hdr_drv->close    = hdr_dd_close;
	hdr_drv->progress = hdr_dd_progress;
	hdr_drv->name     = strdup("hdr");
	hdr_drv->info     = strdup("Save the image as a Radiance .hdr format(HDR)");
	ri_render_register_display_drv(grender, "hdr", hdr_drv);
	ri_render_register_display_drv(grender, RI_FILE, hdr_drv);

	sock_drv = (ri_display_drv_t *)ri_mem_alloc(sizeof(ri_display_drv_t));
	sock_drv->open     = sock_dd_open;
	sock_drv->write    = sock_dd_write;
	sock_drv->close    = sock_dd_close;
	sock_drv->progress = sock_dd_progress;
	sock_drv->name     = strdup("socket");
	sock_drv->info     = strdup("Send image data to external program");
	ri_render_register_display_drv(grender, "socket", sock_drv);


	/* setup raytracing data(should be called once)*/
	ri_raytrace_setup();

	initialized = 1;

	/* TODO: remove this */
	ri_impl_option_insert("raytrace_max_depth", &ray_max_depth);

	ri_timer_start(grender->context->timer, "TOTAL rendering time");

}

ri_render_t *
ri_render_get()
{
	return grender;
}

void
ri_render_free()
{
	if (!initialized) return;

	ri_raytrace_shutdown();

	ri_hash_free(grender->geom_drv);
	ri_hash_free(grender->display_drv);
	ri_list_free(grender->geomlist);
	ri_context_free(grender->context);
	ri_mem_free(grender);
}

void
ri_render_frame()
{
	int w, h;
	int xsamples, ysamples;
	ri_display_drv_t *drv;
	RtToken  dsp_type;
	ri_geom_t *geom, *bssrdfgeom;
	ri_list_t *geomitr;
	char *output;
	char message[1024];
	ri_photonmap_t *cphotonmap = NULL, *gphotonmap = NULL;
	ri_light_t     *light;
	ri_option_t    *opt;
	ri_photonmap_option_t *pmapopt;
	ri_display_t   *disp;
	ri_octree_t    *irradcachetree;
	double          area;
	float           maxwattage;

	int             myid;

	time_t		tm;

	myid = ri_parallel_taskid();

	w = ri_render_get()->context->option->camera->horizontal_resolution;
	h = ri_render_get()->context->option->camera->vertical_resolution;
	output = ri_render_get()->context->option->display->display_name;
	dsp_type = ri_render_get()->context->option->display->display_type;

	drv = (ri_display_drv_t *)ri_hash_lookup(
		ri_render_get()->display_drv, dsp_type);

	if (drv == NULL) {
#ifdef WIN32
		sprintf(message, "not supported display driver [ \"%s\" ]. force use [ \"framebuffer\" ]", dsp_type);  
		ri_log(LOG_WARN, message);
		drv = (ri_display_drv_t *)ri_hash_lookup(
			ri_render_get()->display_drv, "framebuffer");
#else
		sprintf(message, "not supported display driver [ \"%s\" ]. force use [ \"file\" ]", dsp_type);  
		ri_log(LOG_WARN, message);
		drv = (ri_display_drv_t *)ri_hash_lookup(
			ri_render_get()->display_drv, RI_FILE);
#endif
	}	

	ri_log_and_return_if(drv == NULL);

	opt = ri_render_get()->context->option;
#ifdef DEBUG
	printf("opt: finalgather_rays = %u\n", opt->nfinalgather_rays);
	printf("opt: arealight_rays = %u\n", opt->narealight_rays);
	printf("opt: max_ray_depth = %u\n", opt->max_ray_depth);
	printf("opt: enable irradcache = %d\n", opt->enable_irradcache);
#endif

	disp = ri_render_get()->context->option->display;
	xsamples = (int)disp->sampling_rates[0];
	ysamples = (int)disp->sampling_rates[1];
#ifdef DEBUG
	printf("disp: gain = %f\n", disp->gain);
	printf("disp: gamma = %f\n", disp->gamma);
	printf("disp: sampling_rate = [%d, %d]\n", xsamples, ysamples);
	fflush(stdout);
#endif

	init_sigma(xsamples, ysamples);

	pmapopt = ri_photonmap_get_option();

	if (opt->compute_prt) {
		ri_prt_sample();
		return;
	}

	if (myid == 0) {	/* Master node */
		if (strcmp(disp->display_format, "float") == 0 ||
		    strcmp(dsp_type, "hdr") == 0 ||
		    strcmp(dsp_type, "openexr") == 0 ||
		    strcmp(dsp_type, RI_FILE) == 0) {
			if (!drv->open(output, w, h, 32, RI_RGB, "float")) {
				ri_log(LOG_WARN,
				       "Can't open \"%s\" display driver.\n",
				       dsp_type);
				return;
			}
		} else {
			if (!drv->open(output, w, h, 8, RI_RGB, "byte")) {

				/* Try to open file display driver. */
				ri_log(LOG_WARN,
				       "Can't open \"%s\" display driver."
				       " Use \"file\" display driver instead.",
				       dsp_type);

				free(disp->display_type);
				disp->display_type = strdup(RI_FILE);
				dsp_type = disp->display_type;

				drv = (ri_display_drv_t *)ri_hash_lookup(
					ri_render_get()->display_drv, RI_FILE);
				if (!drv) exit(-1);


				if (!drv->open(output, w, h,
					       32, RI_RGB, "float")) {
					printf("??? can't open display driver");
					printf(" [ %s ].\n", dsp_type);
					return;
				}

			}
		}
	}

	if (!ri_list_first(ri_render_get()->lightlist)) {
		ri_log(LOG_WARN, "there is no light. create domelight.");

		light = ri_light_new();
		light->domelight = 1;

		ri_list_append(ri_render_get()->lightlist, light);
		
	} else {

		// Currently, only 1 light is supported...
		light = (ri_light_t *)ri_list_first(ri_render_get()->lightlist)->data;
	}

	/* calc scene size */
	calc_bbox(ri_render_get()->geomlist,
		  ri_render_get()->bmin,
		  ri_render_get()->bmax,
		  &(ri_render_get()->bmaxwidth));

	ri_parallel_barrier();

	time(&tm);
	strcpy(message, ctime(&tm));
	message[strlen(message) - 1] = '\0';	// strip last '\n'
	ri_log(LOG_INFO, "Start rendering on %s", message);

	/* 1st pass: shoot photon if photon mapping is on. */
	if (light && light->geom && opt->enable_photonmapping) {
		printf("tracing photon...\n");
		area = ri_geom_area(light->geom);
		if (area < 1.0e-6) area = 1.0;
		maxwattage = pmapopt->wattage.e[0];
		if (maxwattage < pmapopt->wattage.e[1]) {
			maxwattage = pmapopt->wattage.e[1];
		}
		if (maxwattage < pmapopt->wattage.e[2]) {
			maxwattage = pmapopt->wattage.e[2];
		}

		if (maxwattage < 1.0e-6f) maxwattage = 1.0f;

		//ri_vector_copy(&(light->intensity), pmapopt->wattage);
		//ri_vector_scale(&(light->intensity), 1.0f / maxwattage);
		ri_vector_copy(&(light->col), &pmapopt->wattage);
		light->intensity = 1.0f / maxwattage;

		gphotonmap = ri_photonmap_new();
		cphotonmap = ri_photonmap_new();
		ri_photonmap_emit_photons(cphotonmap, gphotonmap, light);

		ri_render_get()->caustics_photonmap = cphotonmap;
		ri_render_get()->global_photonmap   = gphotonmap;
	}

	irradcachetree = ri_octree_new();
	ri_render_get()->irradiance_cache = irradcachetree;

	/* 2nd pass. calc bssrdf samples if BSSRDF is on. */
	bssrdf = ri_bssrdf_new();
	bssrdfgeom = NULL;
	for (geomitr = ri_list_first(ri_render_get()->geomlist);
	     geomitr != NULL;
	     geomitr = ri_list_next(geomitr)) {
		geom = (ri_geom_t *)geomitr->data;
		if (geom->shadername && strcmp(geom->shadername, "bssrdf") == 0) {
			bssrdfgeom = geom;
			break;
		} 
	}

	if (bssrdfgeom) {
		printf("bssrdf sampling...\n");
		ri_bssrdf_generate_samples(bssrdf,
					   bssrdfgeom,
					   light, gphotonmap);

		ri_bssrdf_test();
	}

	/* 3rf pass: precompute irradiace cache if raddiance cache is on. */
	if (opt->enable_indirect_lighting && opt->enable_irradcache) {
		if (opt->irradcache_file) {
			ri_irradcache_load(ri_render_get()->irradiance_cache,
					   opt->irradcache_file);
			use_icachefile = 1;
		} else {
			ri_timer_start(ri_render_get()->context->timer,
				       "Irraidance Cache Genearation Pass");
			calc_irradiance_cache(light);
			ri_timer_end(ri_render_get()->context->timer,
				     "Irraidance Cache Genearation Pass");
		}
	}

	if (opt->enable_irradcache && !use_icachefile && myid == 0) {
		/* save irradiance cache samples */
		ri_irradcache_dump(ri_render_get()->irradiance_cache);
	}

	// Final pass. render the scene.
	ri_timer_start(ri_render_get()->context->timer, "Render frame");

	if (opt->render_method == TRANSPORT_MLT) {
		/* multithread setup */
		ri_thread_initialize();
		ri_transport_mlt(drv);
	} else if (opt->render_method == TRANSPORT_PATHTRACE) {
		/* multithread setup */
		ri_thread_initialize();
		ri_transport_pathtrace(drv);
	} else {		/* TRANSPORT_MCPATHTRACE */
		bucket_rendering(drv, disp);
	}

	ri_timer_end(ri_render_get()->context->timer, "Render frame");

	if (myid == 0) {
		printf("\n");
		ri_raytrace_statistics();

		if (opt->accel_method == ACCEL_BSP) {
			ri_bsp_statistics();
		}

		ri_shade_statistics();
	}

	ri_timer_start(ri_render_get()->context->timer, "Clean up");
	ri_photonmap_free(ri_render_get()->global_photonmap);
	ri_photonmap_free(ri_render_get()->caustics_photonmap);
	if (opt->accel_method == ACCEL_GRID) {
		ri_accel_free(ri_render_get()->accel_grid);
	} else if (opt->accel_method == ACCEL_OCTREE_FRISKEN) {
	}
	ri_timer_end(ri_render_get()->context->timer, "Clean up");
	ri_timer_end(ri_render_get()->context->timer, "TOTAL rendering time");

	if (myid == 0) {
		ri_timer_dump(ri_render_get()->context->timer);
		drv->close();
	}

	time(&tm);
	strcpy(message, ctime(&tm));
	message[strlen(message) - 1] = '\0';	// strip last '\n'
	ri_log(LOG_INFO, "End rendering on %s", message);

	ri_parallel_barrier();
}

static void
tonemap(ri_vector_t *result)
{
	ri_display_t *disp;
	double val;

	disp = ri_render_get()->context->option->display;

	val = exposure(result->e[0], disp->gain, disp->gamma);
	if (val < 0.0) val = 0.0;
	if (val > 1.0) val = 1.0;
	result->e[0] = (RtFloat)val;

	val = exposure(result->e[1], disp->gain, disp->gamma);
	if (val < 0.0) val = 0.0;
	if (val > 1.0) val = 1.0;
	result->e[1] = (RtFloat)val;

	val = exposure(result->e[2], disp->gain, disp->gamma);
	if (val < 0.0) val = 0.0;
	if (val > 1.0) val = 1.0;
	result->e[2] = (RtFloat)val;
}

void
ri_render_register_geom_drv(ri_render_t *render, const char *type,
			    ri_geom_drv_t *drv)
{
	ri_hash_insert(render->geom_drv, type, drv);
}

void
ri_render_register_display_drv(ri_render_t *render, const char *type,
			       ri_display_drv_t *drv)
{
	ri_hash_insert(render->display_drv, type, drv);
}


void
ri_render_parse_geom(ri_render_t *render, const char  *type,
		     RtInt        nverts, RtInt	      n,
		     RtToken      tokens[], RtPointer    params[])
{
	ri_geom_drv_t *drv = NULL;

	drv = (ri_geom_drv_t *)ri_hash_lookup(render->geom_drv, type);

	ri_log_and_return_if(NULL == drv);

	ri_render_add_geom(render, drv->parse(nverts, n, tokens, params));
}

void
ri_render_add_geom(ri_render_t *render, ri_geom_t *geom)
{
	ri_list_append(render->geomlist, (void *)geom);
}

RtLightHandle
ri_render_add_light(ri_render_t *render, ri_light_t *light)
{
	ri_list_append(render->lightlist, (void *)light);

	return (RtLightHandle)light;
}

void
ri_render_set_progress_handler(ri_render_t *render, void (*func)(void))
{
	render->progress_handler = func;
}

#ifdef DEBUG
void
dump_irrad(int x, int y, const ri_vector_t *irrad)
{
	static FILE *fp = NULL;
	int w, h;

	w = ri_render_get()->context->option->camera->horizontal_resolution;
	h = ri_render_get()->context->option->camera->vertical_resolution;

	if (x == 0 && y == 0) {
		fp = fopen("irradval.dbg", "w");
	}

	if (fp) {
		fprintf(fp, "[%d,%d] (%f, %f, %f)\n", x, y, 
						  irrad->e[0],	
						  irrad->e[1],	
						  irrad->e[2]);
	}

	if (x == w - 1 && y == h - 1) {
		fclose(fp);
	} 
}
#endif

/* --- private functions --- */

static double
exposure(double val, double gain, double gamma)
{
#ifdef DEBUG
	if (gamma == 0.0) {
		ri_log(LOG_WARN, "gamma = 0.0");
		return 0.0;
	}
#endif
	if (gamma == 0.0) return 0.0;

	return pow((gain * val), 1.0 / gamma);
}

#if 0
static void
map_unitdisk(double p[2])
{
	const double pi_4 = 3.1415926535 / 4.0;
	double phi, r;
	double a = 2.0 * p[0] - 1.0;
	double b = 2.0 * p[1] - 1.0;

	if (a > -b) {
		if (a > b) {
			r = a;
			phi = pi_4 * (b / a);
		} else {
			r = b;
			phi = pi_4 * (2 - a / b);
		}
	} else {
		if (a < b) {
			r = -a;
			phi = pi_4 * (4 + b / a);
		} else {
			r = -b;
			if (b != 0.0) {
				phi = pi_4 * (6 - a/b);
			} else {
				phi = 0.0;
			}
		}
	}

	p[0] = r * cos(phi);
	p[1] = r * sin(phi);
}
#endif

static void
calc_irradiance_cache(ri_light_t *light)
{
	int x, y;
	int w, h;
	int xsamples, ysamples;
	RtVector v, e;
	RtFloat  fov, flength;
	ri_ray_t ray;
	ri_vector_t tmpvec, eye, dir;
	ri_matrix_t orientation;
	ri_matrix_t c2w;
	ri_matrix_t tmpm;
	ri_display_t *disp;
	ri_camera_t  *camera;

	camera = ri_render_get()->context->option->camera;

	w = camera->horizontal_resolution;
	h = camera->vertical_resolution;

	disp = ri_render_get()->context->option->display;
	xsamples = (int)disp->sampling_rates[0];
	ysamples = (int)disp->sampling_rates[1];


	/* build orientation matrix */
	ri_matrix_identity(&orientation);
	if (strcmp(ri_render_get()->context->option->orientation, RI_RH) == 0) {
		orientation.e[2][2] = -orientation.e[2][2];
	}

	/* compute camera to world matrix */
	if (camera->use_glcamera) {
		build_glcamera(&tmpm, camera);
	} else {
		ri_matrix_copy(&tmpm,
			       &(ri_render_get()->context->world_to_camera));
		ri_matrix_inverse(&tmpm);
	}
	ri_matrix_mul(&c2w, &tmpm, &orientation);

	e[0] = 0.0; e[1] = 0.0; e[2] = 0.0;
	ri_vector_set(&tmpvec, e);
	ri_vector_transform(&eye, &tmpvec, &c2w);

	fov = ri_render_get()->context->option->camera->fov;
	flength = 1.0 / tan((fov * 3.141592 / 180.0) * 0.5);

	for (y = 0; y < h; y++) {
		printf("compute irradiance cache row %d\n", y);
	 	for (x = 0; x < w; x++) {

			v[0] = (2.0 * (RtFloat)(x + 0.5) - w) / w;
			v[1] = (2.0 * (RtFloat)(y + 0.5) - h) / h;
			v[2] = flength;

			ri_vector_set(&tmpvec, v);
			ri_vector_transform(&dir, &tmpvec, &c2w);

			ri_vector_copy(&(ray.org), &eye); 
			ri_vector_sub(&(ray.dir), &dir, &eye);
			ri_vector_normalize(&(ray.dir));
			ray.thread_num = 0;

			generate_cache(&ray, light);

			}
	}				

	(void)light;
}

static void
calc_bbox(ri_list_t *geom_list, double min[3], double max[3], double *maxwidth)
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

	(*maxwidth) = max[0] - min[0];
	if ((*maxwidth) < (max[1] - min[1])) (*maxwidth) = max[1] - min[1];
	if ((*maxwidth) < (max[2] - min[2])) (*maxwidth) = max[2] - min[2];
}

static void
dof(ri_vector_t *from, ri_vector_t *to,
    double fstop, double flen, double fdist, double imgflen)
{
	int    nroots;
	double t;
	double aperture;
	double dtheta, dr;
	double dx, dy;
	double lensdist;
	double root[2];

	aperture = flen / (2.0 * fstop);

	t      = (fdist - from->e[2]) / (to->e[2] - from->e[2]);
	dtheta = randomMT() * 2.0 * M_PI;
	dr     = sqrt(-log(randomMT()) / 5.0) * aperture; 

	to->e[0] = t * (to->e[0] - from->e[0]) + from->e[0];
	to->e[1] = t * (to->e[1] - from->e[1]) + from->e[1];
	to->e[2] = fdist;

	dx = cos(dtheta) * dr;
	dy = sin(dtheta) * dr;

	nroots = rootdeg2(root, -(fdist - imgflen), (fdist - imgflen) * flen);
	if (nroots == 0) {
		printf("fdist = %f\n", fdist);
		printf("imgflen = %f\n", imgflen);
		printf("flen = %f\n", flen);
		//printf("???\n");
	}
	assert(nroots != 0);

	lensdist = (root[0] < root[1]) ? root[0] : root[1];
	from->e[0] += 100.0 * dx;
	from->e[1] += 100.0 * dy;
	from->e[2] = lensdist;
}

static int
rootdeg2(double root[2], double a, double b)
{
	/* solve x^2 + a x + b = 0 */
	double d;

	d = a * a - 4.0 * b;
	if (d >= 0.0) {
		d = sqrt(d);
		root[0] = 0.5 * (-a - d);
		root[1] = 0.5 * (-a + d);
		return 2;
	} else {
		printf("a = %f, b = %f\n", a, b);
		printf("d = %f\n", d);
		return 0;
	} 
}

/*
 * sample subpixel. trace ray from camera through pixel in (x, y)
 */
static void
subsample(pixelinfo_t *pixinfo, int x, int y, int threadid)
{
	int                  i;
	int                  w, h;
	int                  xs, ys;
	int                  xsamples, ysamples;
	int                  currsample;
	int                  ortho;
	unsigned int         subinstance;
	float                inv_nsamples;
	double               fov;
	double               flength;
	double               jitter[2];
	ri_vector_t          v;
	ri_vector_t          dir;
	ri_vector_t          from;
	ri_vector_t          accumrad;
	ri_matrix_t          orientation;
	ri_matrix_t          m; 
	ri_matrix_t          c2w;
	ri_ray_t             ray;
	ri_display_t        *disp;
	ri_camera_t         *camera;
	ri_transport_info_t  result;

	camera = ri_render_get()->context->option->camera;

	w = camera->horizontal_resolution;
	h = camera->vertical_resolution;

	fov = ri_render_get()->context->option->camera->fov;
	flength = 1.0 / tan((fov * 3.141592 / 180.0) * 0.5);

	disp = ri_render_get()->context->option->display;
	xsamples = disp->sampling_rates[0];
	ysamples = disp->sampling_rates[1];

	ortho  = (camera->camera_projection == RI_ORTHOGRAPHIC) ? 1 : 0;

	/* build orientation matrix */
	ri_matrix_identity(&orientation);
	if (strcmp(ri_render_get()->context->option->orientation, RI_RH) == 0) {
		orientation.e[2][2] = -orientation.e[2][2];
	}

	/* compute camera to world matrix */
	if (camera->use_glcamera) {
		build_glcamera(&m, camera);
	} else {
		ri_matrix_copy(&m,
			       &(ri_render_get()->context->world_to_camera));
		ri_matrix_inverse(&m);
	}

	ri_matrix_mul(&c2w, &m, &orientation);

	inv_nsamples = 1.0f / (float)(ysamples * xsamples);

	pixinfo->nsamples = xsamples * ysamples;
	for (i = 0; i < pixinfo->nsamples; i++) {
		pixinfo->samples[i].depth = 0.0f;
		ri_vector_zero(&(pixinfo->samples[i].radiance));
		//pixinfo->alpha = 0.0f;
	}
 
	currsample = 0;
	ri_vector_zero(&accumrad);
	for (ys = 0; ys < ysamples; ys++) {
		for (xs = 0; xs < xsamples; xs++) {

			//jittering(jitter, xs, ys, xsamples, ysamples);
			sample_subpixel(&subinstance,
					jitter, xs, ys, xsamples, ysamples);


#if 0
			// importance sampling of tent filter.
			// see Realistic Ray Tracing p.110.
			if (jitter[0] < 0.5) {
				jitter[0] = sqrt(2.0 * jitter[0]) - 1.0;
			} else {
				jitter[0] = 1.0 - sqrt(2.0 * 2.0 * jitter[0]);
			}

			if (jitter[1] < 0.5) {
				jitter[1] = sqrt(2.0 * jitter[1]) - 1.0;
			} else {
				jitter[1] = 1.0 - sqrt(2.0 * 2.0 * jitter[1]);
			}

			jitter[0] += 0.5; jitter[1] += 0.5;
#endif

			
			//map_unitdisk(jitter);

			v.e[0] = (2.0f * (RtFloat)(x + jitter[0]) - w) / w;
			v.e[1] = (2.0f * (RtFloat)(y + jitter[1]) - h) / h;
			v.e[2] = flength;
			v.e[3] = 1.0;

			// perturberate ray for DOF effect.
			if (camera->focal_length > 0.0) {
				/* TODO: fix this */
				if (ortho) {	/* orthographic camera */
					from.e[0] = v.e[0];
					from.e[1] = v.e[1];
					from.e[2] = 0.0;
					from.e[3] = 1.0;
				} else {	/* perspective camera */
					ri_vector_zero(&from);
					from.e[3] = 1.0;
				}
				dof(&from, &v,
				    camera->fstop,
				    camera->focal_length,
				    camera->focal_distance,
			 	    flength);
				ri_vector_transform(&dir, &v, &c2w);
				ri_vector_copy(&v, &from);
				ri_vector_transform(&from, &v, &c2w);
			} else {
				ri_vector_transform(&dir, &v, &c2w);

				if (ortho) {	/* orthographic camera */
					v.e[0] = v.e[0];
					v.e[1] = v.e[1];
					v.e[2] = 0.0;
					v.e[3] = 1.0;
				} else {	/* perspective camera */
					ri_vector_zero(&v);
					v.e[3] = 1.0;
				}
				ri_vector_transform(&from, &v, &c2w);
			}


			ri_vector_copy(&(ray.org), &from); 
			ri_vector_sub(&(ray.dir), &dir, &from);
			ri_vector_normalize(&(ray.dir));

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
			gqmc_instance += (xsamples * ysamples);
			//printf("instance = %d\n", gqmc_instance);
			//printf("subinstance = %d\n", subinstance);
			//printf("subsample:instance = %d\n", ray.i);

			/* assign threadid to ray's thread number */
			ray.thread_num = threadid;	

			ri_transport_sample(ri_render_get(),
					    &ray,
					    &result);

			ri_vector_add(&accumrad,
				      &accumrad,
				      &result.radiance);

			if (result.hit) {
				pixinfo->samples[currsample].depth
						+= result.ray.isectt *
						   inv_nsamples;
			} else {
				pixinfo->samples[currsample].depth
						+= RI_INFINITY * inv_nsamples;
			}
		}
	}				

	ri_vector_scale(&accumrad, (float)(1.0 / (xsamples * ysamples)));

	ri_vector_copy(pixinfo->radiance, &accumrad);
}

//static void
//jittering(double jitter[2], int xs, int ys, int xsamples, int ysamples)
//{
//	jitter[0] = ((double)xs + randomMT()) / (double)xsamples;
//	jitter[1] = ((double)ys + randomMT()) / (double)ysamples;
//}

static void
generate_cache(ri_ray_t *ray, ri_light_t *light)
{
	int hit;
	ri_surface_info_t surfinfo;
	double r, d;
	ri_vector_t rad;
	ri_ray_t v;
	ri_render_t *render;
	ri_photonmap_option_t *pmapopt;
	ri_option_t *opt;
	int          fixicache = 0;
	int          forcecalc = 0;

	(void)light;

	render = ri_render_get();
	pmapopt = ri_photonmap_get_option();
	opt     = render->context->option;

	ray->isectt = 0.0;
	hit = ri_raytrace(ri_render_get(), ray, &surfinfo);

	if (hit) {

		r = randomMT();

		d = ri_vector_ave(&surfinfo.geom->material->kd);

		/* russian roulette. */

		if (r < d) {
			ri_vector_copy(&v.org, &surfinfo.pos);
			ri_vector_copy(&v.dir, &surfinfo.normal);
			v.thread_num = ray->thread_num;

			ri_shade_indirect(&rad,
					  render->global_photonmap,
					  render->irradiance_cache,
					  &v,
					  opt->nfinalgather_rays,
					  pmapopt->max_gather_radius,
					  pmapopt->max_gather_photons,
					  fixicache,
					  forcecalc);
		}	
	}
}

/* two-dimensional Hammersley points for anti-aliasing.
 * see:
 * "Strictly Deterministic Sampling Methods in Computer Graphics"
 * Alexander Keller, mental images technical report, 2001
 */
static void
sample_subpixel(unsigned int *i, double jitter[2],
	        int xs, int ys, int xsamples, int ysamples)
{
	unsigned int         j, k;
	double               offsetx, offsety;

	j = xs & (hammsample.periodx - 1);
	k = ys & (hammsample.periodx - 1);
	
	/* Instance number. */
	*i = j * hammsample.periodx + hammsample.sigmax[k];
	
	jitter[0] = (double)xs
		  + (double)hammsample.sigmax[k] / (double)hammsample.periodx;
	jitter[1] = (double)ys
		  + (double)hammsample.sigmay[j] / (double)hammsample.periody;

	jitter[0] /= (double)xsamples;
	jitter[1] /= (double)ysamples;

	offsetx = 0.5 / (xsamples * xsamples);
	offsety = 0.5 / (ysamples * ysamples);

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
init_sigma(int xsamples, int ysamples)
{
	unsigned int         i, inverse, digit, bits;

	/* initialize	*/
	hammsample.periodx = xsamples;
	hammsample.periody = ysamples;

	hammsample.sigmax = (unsigned int *)malloc(sizeof(unsigned int) *
					           hammsample.periodx);
	hammsample.sigmay = (unsigned int *)malloc(sizeof(unsigned int) *
					           hammsample.periody);

	/* x */
	for (i = 0; i < hammsample.periodx; i++) {
		digit = hammsample.periodx;
		inverse = 0;

		for (bits = i; bits; bits >>= 1) {
			digit >>= 1;

			if (bits & 1) {
				inverse += digit;
			}
		}

		hammsample.sigmax[i] = inverse;
	}

	/* y */
	for (i = 0; i < hammsample.periody; i++) {
		digit = hammsample.periody;
		inverse = 0;

		for (bits = i; bits; bits >>= 1) {
			digit >>= 1;

			if (bits & 1) {
				inverse += digit;
			}
		}

		hammsample.sigmay[i] = inverse;
	}
}

/*
 * bucket rendering.
 */
static void
bucket_rendering(ri_display_drv_t *drv, ri_display_t *disp)
{
	int i, j, n;
	int idx;
	int np;
	int myid;
	int x, y;
	int sx, sy;
	int width, height;
	int screenwidth, screenheight;
	const int bucketwidth = 32, bucketheight = 32;
	bucket_t *bucketlist;		/* all bucket lists	*/
	int *assignedbucketlist;
	int nbuckets;
	int currbucket, allocedbucket;
	int nwidthdiv, nheightdiv;
	int nxbuckets, nybuckets;
	int widthreminder, heightreminder;
	int datasize;
	ri_camera_t *camera;
	ri_parallel_status_t status;
	ri_parallel_request_t *requestlist;
	double elapsed;	/* elapsed time from rendering starts */
	double etl;	/* estimate time left */
	pixelinfo_t pixinfo;
	unsigned int xp, yp;	/* (x, y) location of scanning position */

	int nthreads;

	/* multithread setup */
	ri_thread_initialize();

	nthreads = ri_render_get()->context->option->nthreads;
	if (nthreads > RI_MAX_THREADS) nthreads = RI_MAX_THREADS;

	if (nthreads > 1) {
		printf("number of threads to use = %d\n", nthreads);
	}

	if (!subsample_workpile) {
		subsample_workpile = work_init(bucketwidth * bucketheight + 1,
					       subsample_threadfunc,
					       nthreads);
	}

	// parallel rendering setup
	np   = ri_parallel_ntasks();
	myid = ri_parallel_taskid();


	/* requestlist[0] is never used. */
	requestlist = (ri_parallel_request_t *)
			ri_mem_alloc(sizeof(ri_parallel_request_t) * np);

	// setup bucket.
	camera = ri_render_get()->context->option->camera;

	screenwidth  = camera->horizontal_resolution;
	screenheight = camera->vertical_resolution;

	nwidthdiv  = screenwidth  / bucketwidth;
	nheightdiv = screenheight / bucketheight;
		
	widthreminder  = screenwidth  % bucketwidth;	
	heightreminder = screenheight % bucketheight;	
		
	if (widthreminder)  nxbuckets = nwidthdiv + 1;
	else 		    nxbuckets = nwidthdiv;

	if (heightreminder) nybuckets = nheightdiv + 1;
	else                nybuckets = nheightdiv;

	nbuckets = nxbuckets * nybuckets;

	bucketlist = (bucket_t *)ri_mem_alloc(sizeof(bucket_t) * 
					      nbuckets);
	
	assignedbucketlist = (int *)ri_mem_alloc(sizeof(int) * nbuckets);

	for (j = 0; j < nybuckets; j++) {
		for (i = 0; i < nxbuckets; i++) {
			idx = j * nxbuckets + i;

			bucketlist[idx].x = i * bucketwidth; 
			bucketlist[idx].y = j * bucketwidth; 

			if (i == (nxbuckets - 1) && widthreminder) {
				width = widthreminder;
			} else {
				width = bucketwidth;
			}

			if (j == (nybuckets - 1) && heightreminder) {
				height = heightreminder;
			} else {
				height = bucketheight;
			}

			bucketlist[idx].w = width;
			bucketlist[idx].h = height;

			bucketlist[idx].rendered = 0;
			bucketlist[idx].written = 0;

			bucketlist[idx].pixels = (ri_vector_t *)
						  ri_mem_alloc(
						  sizeof(ri_vector_t) *
						  width * height);

			for (n = 0; n < width * height; n++) {
				ri_vector_zero(&(bucketlist[idx].pixels[n]));
			}
		}
	}

	// hilbert order scan setup
	hil_setup(screenwidth, screenheight, bucketwidth);

	// Z curve order scan setup
	zorder_setup(screenwidth, screenheight, bucketwidth);

	// spiral order scan setup
	spiral_setup(screenwidth, screenheight, bucketwidth);

	if (myid == 0) {	/* master node	*/
		currbucket = 0;
		allocedbucket = currbucket;
	
		for (i = 1; i < np; i++) {
			allocedbucket++;
			assignedbucketlist[i] = allocedbucket;
			ri_parallel_send(&allocedbucket, sizeof(int), i, 0);

			datasize = sizeof(ri_vector_t) * 
				   bucketlist[allocedbucket].w * 
				   bucketlist[allocedbucket].h;

			/* non-blocking recv */
			ri_parallel_irecv(bucketlist[allocedbucket].pixels,
					  datasize,
					  i, 0,
					  &(requestlist[i]));
		}
	} else {		/* child node	*/
		/* get bucket ID */
		ri_parallel_recv(&currbucket, sizeof(int), 0, 0, &status);
		
	}

#if 0
	while (currbucket < nbuckets) {
#else	
#if 0
	/* Z curve order scan version */
	while (zorder_get_nextlocation(&xp, &yp)) {
#else
	/* spiral order scan version */
	while (spiral_get_nextlocation(&xp, &yp)) {
#endif
		currbucket = xp + nxbuckets * yp;

	
#endif
		//printf("rank(%d): rendering bucket %d / %d\n", myid, currbucket + 1, nbuckets);
		sx     = bucketlist[currbucket].x;
		sy     = bucketlist[currbucket].y;
		width  = bucketlist[currbucket].w;
		height = bucketlist[currbucket].h;

		if (nthreads) {
			/* threaded version. */
			subsample_threaded(subsample_workpile,
					   bucketlist[currbucket].pixels,
					   sx, sy,
					   width, height);

			if (myid == 0) {
				/* For each node(exclude master
				 * node), check if bucket
				 * rendering is finished.
				 * And if so, assign next
				 * bucket task.
				 */
				bucket_scheduling(bucketlist,
						  &allocedbucket,
			 			  assignedbucketlist,
						  requestlist,
						  nbuckets,
						  np, drv, disp);
			}
		} else {
			/* non threaded version. */
			/* render bucket region [x, y], [x + w, y + h] */
			for (y = 0; y < height; y++) {
				for (x = 0; x < width; x++) {
					idx = y * width + x;

					pixinfo.radiance = 
					  &(bucketlist[currbucket].pixels[idx]);

					subsample(
					  &pixinfo,
					  sx + x, sy + y, 0);

					if (myid == 0) {
						/* For each node(exclude master
						 * node), check if bucket
						 * rendering is finished.
						 * And if so, assign next
						 * bucket task.
						 */
						bucket_scheduling(
							bucketlist,
							&allocedbucket,
							assignedbucketlist,
							requestlist,
							nbuckets,
							np, drv, disp);
					}
				}
			}
		}

		if (myid == 0) {	/* master node */
			bucket_write(&(bucketlist[currbucket]), drv, disp);
			allocedbucket++;
			currbucket = allocedbucket;

			/* calculate estimate time left. */

			elapsed = ri_timer_elapsed_current(
					ri_render_get()->context->timer,
					"Render frame");

			if (currbucket == 1) {
				etl = elapsed * (double)((nbuckets) + 1);

			} else {
				etl = (elapsed / (double)currbucket);
				etl *= (double)(nbuckets - currbucket);
			}

			/* This output line is parsed by GUI frontend.
			 * Should not change the output format!
			 */
			printf("Bucket %d/%d  ", currbucket + 1, nbuckets);
			progress_bar((int)(((double)currbucket / nbuckets) * 100.0));
			printf(" %3d %%",
				(int)(((double)currbucket / nbuckets) * 100.0));
			printf("  ETL %d s.", (int)etl);
			printf("  Elapsed %d s.", (int)elapsed);
			printf("   \r");
			fflush(stdout);

		} else {		/* child node */
			/* send rendered pixel of bucket */

			datasize = sizeof(ri_vector_t) * 
				   bucketlist[currbucket].w *
				   bucketlist[currbucket].h;

			ri_parallel_send(bucketlist[currbucket].pixels,
					 datasize, 0, 0); 

			/* recv next bucket task */
			ri_parallel_recv(&currbucket,
					 sizeof(int),
					 0, 0, &status);
		} 
	}

	if (myid == 0) {
		for (i = 1; i < np; i++) {
			if (assignedbucketlist[i] > 0) {
				ri_parallel_wait(&(requestlist[i]),
						 &status);

				idx = assignedbucketlist[i]; 
				bucket_write(&(bucketlist[idx]), drv, disp);

				/* not used message */
				ri_parallel_send(&nbuckets,
						 sizeof(int),
						 i, 0);
			}
		} 
	}

	/* wait until all node come here. */
	ri_parallel_barrier();

	work_free(subsample_workpile);
	subsample_workpile = NULL;

	for (i = 0; i < nbuckets; i++) {
		ri_mem_free(bucketlist[i].pixels);
	}
	ri_mem_free(bucketlist);
	ri_mem_free(requestlist);
	ri_mem_free(assignedbucketlist);

	ri_thread_shutdown();
}

/*
 * bucket task scheduling.
 */
static void
bucket_scheduling(bucket_t *bucketlist,
		  int *currbucket,
		  int *assignedbucketlist,
		  ri_parallel_request_t *requestlist,
		  int nbuckets,
		  int np,
		  ri_display_drv_t *drv,
		  ri_display_t *disp)	
{
	int i;
	int finished;
	int idx;
	int recvsize;
	ri_parallel_status_t status;
	bucket_t *bucketp;

	for (i = 1; i < np; i++) {
		/* test if i'th node finished bucket rendering */ 
		finished = ri_parallel_test(&(requestlist[i]), &status);

		if (finished) {
			/* free request handle */
			idx = assignedbucketlist[i];

			if (idx >= nbuckets || idx < 0) {
				//printf("??? idx >= nbuckets || idx < 0\n");
				continue;
			} else {
				bucket_write(&(bucketlist[idx]), drv, disp);
			}

			(*currbucket)++;
			if ((*currbucket) >= nbuckets) {
				/* all bucket is assigned.
				 */
				assignedbucketlist[i] = -1;
				/* send next bucket index to i'th node. */ 
				ri_parallel_send(currbucket, sizeof(int), i, 0);
			} else {

				assignedbucketlist[i] = (*currbucket);

				/* send next bucket index to i'th node. */ 
				ri_parallel_send(currbucket, sizeof(int), i, 0);

				bucketp = &(bucketlist[(*currbucket)]);
				recvsize = sizeof(ri_vector_t) *
					   bucketp->w * bucketp->h;

				/* non-blocking recv.
				 * reuse request handle
				 */
				ri_parallel_irecv(bucketp->pixels,
						  recvsize,
						  i, 0, &(requestlist[i]));



			}
		}
	}



}

#if 0
static void
bucket_write(bucket_t *bucketlist, int nbucket,
	     ri_display_drv_t *drv, ri_display_t *disp)
{
	int i, n;
	int width, height;
	int screenwidth, screenheight;
	int x, y;
	int sx, sy;
	ri_vector_t   rad;
	float         floatcol[3];
	unsigned char col[3];
	ri_camera_t *camera;
	const char  *dsp_type;        

	camera = ri_render_get()->context->option->camera;

	screenwidth  = camera->horizontal_resolution;
	screenheight = camera->vertical_resolution;

	dsp_type = ri_render_get()->context->option->display->display_type;

	for (i = 0; i < nbucket; i++) {
		x      = bucketlist[i].x;
		y      = bucketlist[i].y;
		width  = bucketlist[i].w;
		height = bucketlist[i].h;

		for (sy = 0; sy < height; sy++) {
			for (sx = 0; sx < width; sx++) {
				n = sy * width + sx;
				ri_vector_copy(&rad,
					       bucketlist[i].pixels[n]);

				if (strcmp(disp->display_format,
					   "float") == 0 ||
				    strcmp(dsp_type, "hdr") == 0 ||
				    strcmp(dsp_type, "openexr") == 0 ||
				    strcmp(dsp_type, "socket") == 0 ||
				    strcmp(dsp_type, RI_FILE) == 0) {
					floatcol[0] = rad.e[0];
					floatcol[1] = rad.e[1];
					floatcol[2] = rad.e[2];

					drv->write(sx + x,
						   screenheight - (sy + y) - 1,
						   floatcol);
				} else {		
					tonemap(&rad);

					col[0] = (unsigned char)
							(rad.e[0] * 255.0); 
					col[1] = (unsigned char)
							(rad.e[1] * 255.0); 
					col[2] = (unsigned char)
							(rad.e[2] * 255.0); 

					drv->write(sx + x,
						   //screenheight - (sy + y) - 1,
						   (sy + y),
						   col);
				}
			}
		}
	}
}
#endif

static void
bucket_write(bucket_t *bucket,
	     ri_display_drv_t *drv, ri_display_t *disp)
{
	int n;
	int width, height;
	int screenwidth, screenheight;
	int x, y;
	int sx, sy;
	ri_vector_t   rad;
	float         floatcol[3];
	unsigned char col[3];
	ri_camera_t *camera;

	camera = ri_render_get()->context->option->camera;

	screenwidth  = camera->horizontal_resolution;
	screenheight = camera->vertical_resolution;

	x      = bucket->x;
	y      = bucket->y;
	width  = bucket->w;
	height = bucket->h;

	for (sy = 0; sy < height; sy++) {
		for (sx = 0; sx < width; sx++) {
			n = sy * width + sx;
			ri_vector_copy(&rad,
				       &bucket->pixels[n]);

			if (strcmp(disp->display_format,
				   "float") == 0 ||
			    strcmp(disp->display_type, "hdr") == 0 ||
			    strcmp(disp->display_type, "openexr") == 0 ||
			    strcmp(disp->display_type, "socket") == 0 ||
			    strcmp(disp->display_type, RI_FILE) == 0) {
				floatcol[0] = rad.e[0];
				floatcol[1] = rad.e[1];
				floatcol[2] = rad.e[2];

				drv->write(sx + x,
					   screenheight - (sy + y) - 1,
					   floatcol);
			} else {		
				tonemap(&rad);

				col[0] = (unsigned char) (rad.e[0] * 255.0); 
				col[1] = (unsigned char) (rad.e[1] * 255.0); 
				col[2] = (unsigned char) (rad.e[2] * 255.0); 

				drv->write(sx + x,
					   //screenheight - (sy + y) - 1,
					   (sy + y),
					   col);
			}
		}
	}
}

static void
subsample_threaded(workpile_t *wp,
		   ri_vector_t *pixels,
		   int sx, int sy,
		   int width, int height)
{
	int x, y;
	int idx;
	pixelinfo_t *pixinfo;

	pixinfo = (pixelinfo_t *)ri_mem_alloc(sizeof(pixelinfo_t) *
					      width * height);

	/* post works(jobs) */
	for (y = 0; y < height; y++) {
		for (x = 0; x < width; x++) {
			idx = y * width + x;
			pixinfo[idx].radiance = &(pixels[idx]);
			pixinfo[idx].x = sx + x;
			pixinfo[idx].y = sy + y;

			work_put(wp, (void *)&(pixinfo[idx]));
		}
	}

	/* wait until all work ends */
	work_wait(wp);

	ri_mem_free(pixinfo);
}

static void *
subsample_threadfunc(void *arg, int threadid)
{
	pixelinfo_t *pixinfo;

	pixinfo = (pixelinfo_t *)arg;

	subsample(pixinfo, pixinfo->x, pixinfo->y, threadid);
	
	return NULL;
}

static workpile_t *
work_init(int maxpile, void *(*workfunc)(void *arg, int threadid), int nthreads)
{
	int         i;
	int         err;
	workpile_t *wp;
	//ri_thread_t thread;		/* dummy */

	/* sizeof(workpile_t) + working area */
	wp = (workpile_t *)ri_mem_alloc(sizeof(workpile_t) + 
					(maxpile * sizeof(void *)));
	
	ri_mutex_init(&(wp->lock));
	wp->work_wait   = ri_thread_cond_new();
	wp->finish_wait = ri_thread_cond_new();

	ri_thread_cond_init(wp->work_wait);
	ri_thread_cond_init(wp->finish_wait);

	wp->maxpile = maxpile;
	wp->nworking = wp->nwaiting = wp->npiles = 0;
	wp->workfunc = workfunc;
	wp->inp = wp->outp = 0;

	if (nthreads > RI_MAX_THREADS) nthreads = RI_MAX_THREADS;
	wp->nthreads = nthreads;

	wp->threadid = 0;
	
	for (i = 0; i < wp->nthreads; i++) {
		wp->threads[i] = (ri_thread_t *)
					ri_mem_alloc(sizeof(ri_thread_t));

		err = ri_thread_create(wp->threads[i],
				       (void *(*)(void *))worker,
				       (void *)wp);
		if (err) {
			fprintf(stderr, "thread err.\n");
			exit(-1);
		}
	}

	return wp;
}

static void
work_put(workpile_t *wp, void *ptr)
{
	ri_mutex_lock(&(wp->lock));
	if (wp->nwaiting) {
		/* wake up idle threads */
		ri_thread_cond_signal(wp->work_wait);
	}

	assert(wp->npiles != wp->maxpile);

	wp->npiles++;
	wp->pile[wp->inp] = ptr;
	wp->inp = (wp->inp + 1) % wp->maxpile;
	
	ri_mutex_unlock(&(wp->lock));
}

static void
worker(workpile_t *wp)
{
	void *ptr;
	int   threadid;

	ri_mutex_lock(&(wp->lock));
	wp->nworking++;

	/* For improving multi-thread performance,
	 * we decided to explicitly allocate thread number
	 * to avoid thread specific data API accessing which may become
	 * costly operation. */
	threadid = (wp->threadid++);

	for (;;) {
		while (wp->npiles == 0) {	/* wait next work */
			if (--wp->nworking == 0) {
				ri_thread_cond_signal(wp->finish_wait);
			}

			wp->nwaiting++;
			ri_thread_cond_wait(wp->work_wait, &(wp->lock));
			wp->nwaiting--;
			wp->nworking++;
		}

		wp->npiles--;
		ptr = wp->pile[wp->outp];
		wp->outp = (wp->outp + 1) % wp->maxpile;
		
		ri_mutex_unlock(&(wp->lock));

		wp->workfunc(ptr, threadid);

		ri_mutex_lock(&(wp->lock));
	}
}


static void
work_wait(workpile_t *wp)
{
	/* wait until all threads stops. */
	
	ri_mutex_lock(&(wp->lock));
	while (wp->npiles != 0 || wp->nworking != 0) {
		ri_thread_cond_wait(wp->finish_wait, &(wp->lock));
	}
	ri_mutex_unlock(&(wp->lock));
}

static void
work_free(workpile_t *wp)
{
	int i;

	for (i = 0; i < wp->nworking; i++) {
		ri_thread_free(wp->threads[i]);
	}

	ri_thread_cond_free(wp->work_wait);
	ri_thread_cond_free(wp->finish_wait);
	ri_mem_free(wp); 
}

static void
progress_bar(int progress)
{
	int i;
	int len = progress / 5;

	printf("|");
	for (i = 0; i < 20; i++) {
		if (i < len) {
			printf("=");
		} else {
			printf(" ");
		}
	}
	printf("|");
	
}

static void
build_glcamera(ri_matrix_t *m, const ri_camera_t *camera)
{
	ri_matrix_t tmp, orientation;
	ri_vector_t dir, up, right;

	ri_vector_sub(&dir, &camera->cam_at, &camera->cam_pos);
	ri_vector_cross3(&right, &dir, &camera->cam_up);
	ri_vector_cross3(&up, &right, &dir);

	ri_vector_normalize(&dir);
	ri_vector_normalize(&right);
	ri_vector_normalize(&up);

	/* build orientation matrix */
	ri_matrix_identity(&orientation);
	orientation.e[2][2] = -orientation.e[2][2];

	ri_matrix_identity(&tmp);
	tmp.e[0][0]  = right.e[0];
	tmp.e[0][1]  = right.e[1];
	tmp.e[0][2]  = right.e[2];
	tmp.e[0][3]  = 0.0;
	tmp.e[1][0]  = up.e[0];
	tmp.e[1][1]  = up.e[1];
	tmp.e[1][2]  = up.e[2];
	tmp.e[1][3]  = 0.0;
	tmp.e[2][0]  = dir.e[0];
	tmp.e[2][1]  = dir.e[1];
	tmp.e[2][2]  = dir.e[2];
	tmp.e[2][3]  = 0.0;
	ri_matrix_mul(m, &tmp, &orientation);
	ri_matrix_translate(m,
			    camera->cam_pos.e[0],
			    camera->cam_pos.e[1],
			    camera->cam_pos.e[2]);
	ri_matrix_inverse(m);


}
